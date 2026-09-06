;;; beads-eldoc.el --- Eldoc support for Beads issue references -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;;; Commentary:

;; Provides eldoc integration for Beads issue references.
;;
;; When the cursor is positioned on a beads issue reference (like
;; bs-lc1lb, gce-hck or bd-a1b2), eldoc will display:
;; - Issue title and status in the echo area
;; - Full issue metadata in the eldoc buffer
;;
;; This works in all major modes: code comments, org files, markdown,
;; etc.
;;
;; Usage:
;;
;;   (beads-eldoc-mode 1)  ; Enable globally
;;
;; Lookups run asynchronously through `beads-command-execute-async';
;; results (and misses) are cached per store and invalidated when
;; issues are modified.  Point-based eldoc works in any text buffer,
;; including shell/comint buffers and `vterm-copy-mode'.

;;; Code:

(require 'beads-util)
(require 'beads-command)
(require 'eldoc)

;; Loaded lazily by `beads-eldoc--spawn': beads-command-show requires
;; this file, so a top-level require would be a cycle.
(declare-function beads-command-show "beads-command-show")

;;; Customization

(defgroup beads-eldoc nil
  "Eldoc support for Beads issue references."
  :group 'beads
  :prefix "beads-eldoc-")

(defcustom beads-eldoc-cache-ttl 300
  "Seconds a fetched issue stays in the eldoc cache."
  :type 'integer
  :group 'beads-eldoc)

(defcustom beads-eldoc-negative-cache-ttl 45
  "Seconds an unknown id stays cached as missing.
Terminal and log text is full of tokens that look like ids
\(\"post-command\", \"gc-agent\"), and each unknown one used to cost a
`bd show' per eldoc tick.  A miss is now remembered for this long, so
an unknown token spawns at most one lookup per interval and scope."
  :type 'integer
  :group 'beads-eldoc)

(defcustom beads-eldoc-issue-pattern nil
  "Regexp overriding `beads-issue-id-regexp' for eldoc, or nil.
Group 1 must be the id.  Nil uses the shared regexp, which knows that
real ids are base-36 (bs-lc1lb, gce-hck), not hexadecimal."
  :type '(choice (const :tag "Shared beads-issue-id-regexp" nil) regexp)
  :group 'beads-eldoc)

(defcustom beads-eldoc-directory nil
  "Store directory eldoc resolves ids in, or nil for `default-directory'.
Either a directory (a TRAMP name is fine; it is localized before
reaching `bd --directory'), or a function called with the issue id
that returns such a directory or nil.  Meant to be set buffer-locally
by callers that know which store owns an id -- gascity.el maps an id
prefix to its rig's store -- so a terminal buffer sitting in one rig
still resolves another rig's ids.  Also part of the cache scope."
  :type '(choice (const :tag "Use default-directory" nil)
                 directory
                 function)
  :local t
  :group 'beads-eldoc)

;;; Variables

(defvar beads-eldoc--cache (make-hash-table :test 'equal)
  "Cache of fetched issues, positive and negative.
Keys are (SCOPE . ISSUE-ID) where SCOPE is the store directory the id
was resolved in (see `beads-eldoc--scope'); values are plists
\(:status ok|missing :timestamp FLOAT :issue ISSUE-or-nil).  Keying by
scope keeps a local bd-1 apart from a remote host's bd-1.")

(defvar beads-eldoc--pending (make-hash-table :test 'equal)
  "Lookups in flight, keyed like `beads-eldoc--cache'.
Values are (STARTED . WAITERS) with WAITERS a list of (BUFFER . CALLBACK)
to run when the result lands.  Deduplicates requests issued while a
spawn is still queued, before `beads-command-execute-async' has a live
process to coalesce on.")

;;; Scope

(defun beads-eldoc--store-directory (issue-id)
  "Return the store directory `beads-eldoc-directory' names for ISSUE-ID, or nil."
  (let ((dir beads-eldoc-directory))
    (cond ((functionp dir) (ignore-errors (funcall dir issue-id)))
          ((stringp dir) dir))))

(defun beads-eldoc--scope (issue-id)
  "Return the cache scope for ISSUE-ID: its store directory, normalized.
Pure string work -- `expand-file-name' on an absolute (or TRAMP) name
does no I/O -- so it is safe from any hook."
  (directory-file-name
   (expand-file-name (or (beads-eldoc--store-directory issue-id)
                         default-directory))))

;;; Cache Management

(defun beads-eldoc--cache-key (issue-id &optional scope)
  "Return the cache key for ISSUE-ID in SCOPE (default: the current scope)."
  (cons (or scope (beads-eldoc--scope issue-id)) issue-id))

(defun beads-eldoc--cache-get (issue-id &optional scope)
  "Return the live cache entry for ISSUE-ID in SCOPE, or nil.
A stale entry (per its status' TTL) is dropped and nil returned."
  (let ((key (beads-eldoc--cache-key issue-id scope)))
    (when-let* ((entry (gethash key beads-eldoc--cache)))
      (let ((ttl (if (eq (plist-get entry :status) 'ok)
                     beads-eldoc-cache-ttl
                   beads-eldoc-negative-cache-ttl)))
        (if (< (- (float-time) (plist-get entry :timestamp)) ttl)
            entry
          (remhash key beads-eldoc--cache)
          nil)))))

(defun beads-eldoc--get-cached-issue (issue-id &optional scope)
  "Return the cached issue for ISSUE-ID in SCOPE, or nil.
Nil for a miss, a stale entry, or a cached negative result."
  (when-let* ((entry (beads-eldoc--cache-get issue-id scope)))
    (and (eq (plist-get entry :status) 'ok)
         (plist-get entry :issue))))

(defun beads-eldoc--cache-issue (issue-id issue &optional scope)
  "Cache ISSUE for ISSUE-ID in SCOPE."
  (puthash (beads-eldoc--cache-key issue-id scope)
           (list :status 'ok :timestamp (float-time) :issue issue)
           beads-eldoc--cache))

(defun beads-eldoc--cache-missing (issue-id &optional scope)
  "Cache that ISSUE-ID does not resolve in SCOPE."
  (puthash (beads-eldoc--cache-key issue-id scope)
           (list :status 'missing :timestamp (float-time) :issue nil)
           beads-eldoc--cache))

(defun beads-eldoc--invalidate-cache (&optional issue-id)
  "Invalidate the eldoc cache.
With ISSUE-ID, drop that id in every scope; otherwise clear everything.
In-flight lookups are left alone: their result is fresh by definition."
  (if issue-id
      (let (stale)
        (maphash (lambda (key _) (when (equal (cdr key) issue-id) (push key stale)))
                 beads-eldoc--cache)
        (dolist (key stale) (remhash key beads-eldoc--cache)))
    (clrhash beads-eldoc--cache)))

;;; Issue Reference Detection

(defun beads-eldoc--issue-id-at-point ()
  "Return the beads issue id at point, or nil.
Honours the buffer's `beads-issue-id-prefixes' allowlist and
`beads-eldoc-issue-pattern'."
  (beads-issue-id-at-point beads-issue-id-prefixes beads-eldoc-issue-pattern))

;;; Issue Fetching (asynchronous)

(defun beads-eldoc--fetch-allowed-p (dir)
  "Return non-nil when a lookup may run in DIR without opening a connection.
Local directories always may; a remote one only while its TRAMP
connection is already established.  Eldoc runs from an idle timer and
must never be the thing that dials a host."
  (let ((remote (file-remote-p dir)))
    (or (null remote)
        (and (file-remote-p dir nil 'connected) t))))

(defun beads-eldoc--pending-fresh-p (pending)
  "Return non-nil when PENDING, a `beads-eldoc--pending' value, is still awaited."
  (< (- (float-time) (car pending))
     (+ beads-command-async-timeout 5)))

(defun beads-eldoc--normalize-issue (result)
  "Return the `beads-issue' in RESULT, or nil.
`beads-command-show' unwraps a single id to one issue; be tolerant of
a list anyway."
  (cond ((cl-typep result 'beads-issue) result)
        ((and (listp result) (cl-typep (car result) 'beads-issue)) (car result))))

(defun beads-eldoc--on-result (key issue-id result)
  "Record RESULT for ISSUE-ID under KEY and run the waiting callbacks.
RESULT nil (or an error) caches a negative entry.  Each callback runs
in the buffer that requested it, if still live; errors are logged and
never propagate into the process sentinel."
  (let* ((issue (beads-eldoc--normalize-issue result))
         (pending (gethash key beads-eldoc--pending))
         (scope (car key)))
    (remhash key beads-eldoc--pending)
    (if issue
        (beads-eldoc--cache-issue issue-id issue scope)
      (beads-eldoc--cache-missing issue-id scope))
    (dolist (waiter (reverse (cdr pending)))
      (let ((buffer (car waiter))
            (callback (cdr waiter)))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (condition-case err
                (funcall callback issue)
              (error
               (beads--log 'verbose "eldoc: callback for %s failed: %s"
                           issue-id (error-message-string err))))))))))

(defun beads-eldoc--spawn (issue-id dir store key)
  "Start an asynchronous `bd show' for ISSUE-ID, delivering via KEY.
DIR is where bd runs (its host, for a TRAMP name); STORE, when non-nil,
is passed as `--directory' so the id is resolved in that store rather
than by cwd-mode lookup.  Never blocks: the command goes through
`beads-command-execute-async' with the shared queue, single-flight
coalescing on KEY and the async timeout."
  (require 'beads-command-show)
  (let ((default-directory dir)
        (non-essential t))
    (condition-case err
        (beads-command-execute-async
         (beads-command-show :issue-ids (list issue-id)
                             :json t
                             :directory store)
         (lambda (result) (beads-eldoc--on-result key issue-id result))
         (lambda (_err) (beads-eldoc--on-result key issue-id nil))
         :queue 'auto
         :cache-key (list 'beads-eldoc key)
         :timeout beads-command-async-timeout)
      (error
       (beads--log 'verbose "eldoc: cannot look up %s: %s"
                   issue-id (error-message-string err))
       (beads-eldoc--on-result key issue-id nil)))))

(defun beads-eldoc--request-issue (issue-id callback)
  "Resolve ISSUE-ID and hand the issue (or nil) to CALLBACK.
Returns `cached' when CALLBACK ran synchronously from the cache
\(positive or negative), `pending' when it will run later from the
lookup now in flight, or nil when nothing will happen because the
store's host is not connected.  CALLBACK runs in the current buffer."
  (let* ((store (beads-eldoc--store-directory issue-id))
         (dir (or store default-directory))
         (scope (directory-file-name (expand-file-name dir)))
         (key (cons scope issue-id))
         (entry (beads-eldoc--cache-get issue-id scope))
         (pending (gethash key beads-eldoc--pending)))
    (cond
     (entry
      (funcall callback (and (eq (plist-get entry :status) 'ok)
                             (plist-get entry :issue)))
      'cached)
     ((and pending (beads-eldoc--pending-fresh-p pending))
      (setcdr pending (cons (cons (current-buffer) callback) (cdr pending)))
      'pending)
     ((not (beads-eldoc--fetch-allowed-p dir))
      (beads--log 'verbose "eldoc: %s not connected, skipping %s" dir issue-id)
      nil)
     (t
      (puthash key (cons (float-time) (list (cons (current-buffer) callback)))
               beads-eldoc--pending)
      (beads-eldoc--spawn issue-id dir store key)
      'pending))))

(defun beads-eldoc--request-current-p (buffer issue-id)
  "Return non-nil when BUFFER is live and ISSUE-ID is still at its point.
The staleness guard for late callbacks: eldoc itself does not drop a
callback whose request is out of date, and `buffer-modified-tick'
would be useless in a terminal buffer that redraws constantly."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (equal (beads-eldoc--issue-id-at-point) issue-id))))

;;; Eldoc Documentation Function

(defun beads-eldoc--format-echo-area (issue)
  "Format ISSUE for display in echo area.
ISSUE is a `beads-issue' object.
Returns a short string with issue title and status."
  (let ((id (oref issue id))
        (title (oref issue title))
        (status (oref issue status)))
    (format "%s [%s]: %s" id status title)))

(defun beads-eldoc--format-doc-buffer (issue)
  "Format ISSUE for display in eldoc documentation buffer.
ISSUE is a `beads-issue' object.
Returns a detailed string with all issue metadata."
  (let ((id (oref issue id))
        (title (oref issue title))
        (description (oref issue description))
        (status (oref issue status))
        (priority (oref issue priority))
        (issue-type (oref issue issue-type))
        (created-at (oref issue created-at))
        (updated-at (oref issue updated-at))
        (assignee (oref issue assignee))
        (notes (oref issue notes)))
    (concat
     (format "Issue: %s\n" id)
     (format "Title: %s\n" title)
     (format "Status: %s\n" status)
     (format "Type: %s  Priority: %s\n" issue-type priority)
     (when assignee (format "Assignee: %s\n" assignee))
     (when created-at (format "Created: %s\n" created-at))
     (when updated-at (format "Updated: %s\n" updated-at))
     (when (and description (not (string-empty-p description)))
       (format "\nDescription:\n%s\n" description))
     (when (and notes (not (string-empty-p notes)))
       (format "\nNotes:\n%s\n" notes)))))

(defun beads-eldoc--deliver (callback issue-id issue)
  "Hand ISSUE for ISSUE-ID to eldoc's CALLBACK."
  (funcall callback
           (beads-eldoc--format-echo-area issue)
           :thing issue-id
           :face 'font-lock-constant-face
           :echo (beads-eldoc--format-echo-area issue)
           :buffer (beads-eldoc--format-doc-buffer issue)))

(defun beads-eldoc-function (callback &rest _)
  "Eldoc documentation function for beads issue references.
For `eldoc-documentation-functions'.  Never blocks: a cached issue is
delivered to CALLBACK at once; otherwise the lookup runs asynchronously
and CALLBACK is invoked when it lands, provided the same id is still at
point in the same buffer (`beads-eldoc--request-current-p').  Returns
non-nil while a result is (or was) forthcoming, nil when there is
nothing to say."
  (when-let* ((issue-id (beads-eldoc--issue-id-at-point)))
    (let* ((buffer (current-buffer))
           (delivered nil)
           (state (beads-eldoc--request-issue
                   issue-id
                   (lambda (issue)
                     (when (and issue
                                (beads-eldoc--request-current-p buffer issue-id))
                       (setq delivered t)
                       (beads-eldoc--deliver callback issue-id issue))))))
      (pcase state
        ('pending t)
        ('cached delivered)
        (_ nil)))))

;;; Minor Mode

;;;###autoload
(define-minor-mode beads-eldoc-mode
  "Global minor mode to enable eldoc support for beads issue references.
When enabled, moving the cursor over a beads issue reference
\(like bs-lc1lb, gce-hck, or bd-a1b2.1) will display issue
information in the echo area and eldoc buffer.  Lookups are
asynchronous and cached (positive and negative), so a slow or remote
store never stalls the UI; in vterm buffers use `vterm-copy-mode' to
put point on an id."
  :global t
  :group 'beads-eldoc
  :lighter nil
  (if beads-eldoc-mode
      (progn
        ;; Add our eldoc function to the documentation functions
        (add-hook 'eldoc-documentation-functions
                  #'beads-eldoc-function nil nil)
        ;; Invalidate cache when issues are modified
        (advice-add 'beads--invalidate-completion-cache
                    :after #'beads-eldoc--invalidate-cache))
    ;; Disable mode
    (remove-hook 'eldoc-documentation-functions
                 #'beads-eldoc-function)
    (advice-remove 'beads--invalidate-completion-cache
                   #'beads-eldoc--invalidate-cache)
    ;; Clear cache
    (beads-eldoc--invalidate-cache)))

;;; Autoload

;;;###autoload
(autoload 'beads-eldoc-mode "beads-eldoc"
  "Toggle eldoc support for beads issue references." t)

(provide 'beads-eldoc)

;;; beads-eldoc.el ends here
