;;; beads-render-guard-test.el --- No file I/O while opening/rendering remote views -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Commentary:

;; dashboard-v3 §8.3 R2, §8.4, §12 B3: opening and rendering a beads.el
;; view on a remote store must not touch the remote host except
;; through its explicit reads (the async bd spawns).  A stat or a git
;; walk over TRAMP at render time is what hung Emacs on 2026-09-06.
;;
;; The guard is a file-name handler for the fake method prefix
;; "/guard:" that answers pure name operations (`file-remote-p',
;; `expand-file-name' on absolute names, `file-name-directory', ...)
;; and signals `beads-render-guard-io' for every operation that could
;; do I/O.  The views are opened with that prefix as their store while
;; the explicit reads (`beads-command-execute-async', the policy probe)
;; are stubbed to deliver canned data.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'beads-command-show)
(require 'beads-dashboard)

(define-error 'beads-render-guard-io "File I/O while rendering a remote view")

(defconst beads-render-guard--prefix "/guard:h:"
  "Remote prefix handled by `beads-render-guard--handler'.")

(defconst beads-render-guard--regexp "\\`/guard:h:"
  "`file-name-handler-alist' key of the guard.")

(defconst beads-render-guard--pure-operations
  '(expand-file-name file-name-directory file-name-nondirectory
    file-name-as-directory directory-file-name file-name-sans-versions
    file-name-case-insensitive-p substitute-in-file-name
    unhandled-file-name-directory file-truename)
  "Operations that only transform names.
They run on the local part and get the prefix back.  `file-truename'
is included because `expand-file-name' callers use it for
normalisation; the guard never resolves links.")

(defun beads-render-guard--local (name)
  "Return NAME without the guard prefix."
  (substring name (length beads-render-guard--prefix)))

(defun beads-render-guard--handler (operation &rest args)
  "Guard handler: pure OPERATIONs pass, anything else signals.
ARGS are OPERATION's arguments."
  (let ((inhibit-file-name-handlers
         (cons #'beads-render-guard--handler inhibit-file-name-handlers))
        (inhibit-file-name-operation operation))
    (pcase operation
      ('file-remote-p
       (let ((identification (nth 1 args)))
         (pcase identification
           ('localname (beads-render-guard--local (car args)))
           ('method "guard")
           ('host "h")
           ('user nil)
           (_ beads-render-guard--prefix))))
      ('file-local-name (beads-render-guard--local (car args)))
      ((guard (memq operation beads-render-guard--pure-operations))
       (let* ((prefixed (lambda (s)
                          (if (and (stringp s)
                                   (string-prefix-p beads-render-guard--prefix s))
                              (beads-render-guard--local s)
                            s)))
              (result (let ((file-name-handler-alist nil)
                            (default-directory
                             (funcall prefixed default-directory)))
                        (apply operation (mapcar prefixed args)))))
         (if (and (stringp result) (file-name-absolute-p result)
                  (not (eq operation 'file-name-nondirectory)))
             (concat beads-render-guard--prefix result)
           result)))
      (_ (signal 'beads-render-guard-io (cons operation args))))))

(defmacro beads-render-guard--with-guard (&rest body)
  "Run BODY with the I/O guard installed for the /guard: prefix."
  (declare (indent 0))
  `(let ((file-name-handler-alist
          (cons (cons beads-render-guard--regexp #'beads-render-guard--handler)
                file-name-handler-alist)))
     ,@body))

(defconst beads-render-guard--issue
  '((id . "bd-1") (title . "Guarded issue") (status . "open") (priority . 1)
    (issue_type . "task") (description . "Body text")
    (created_at . "2025-01-01T00:00:00Z") (updated_at . "2025-01-02T00:00:00Z")
    (dependencies . [((id . "bd-2") (title . "Dep") (status . "open")
                      (dependency_type . "blocks"))]))
  "Canned issue JSON.")

(defun beads-render-guard--canned (command)
  "Return the canned result for COMMAND."
  (cond ((object-of-class-p command 'beads-command-show)
         (beads-issue-from-json beads-render-guard--issue))
        ((or (object-of-class-p command 'beads-command-list)
             (object-of-class-p command 'beads-command-ready)
             (object-of-class-p command 'beads-command-blocked))
         (list (beads-issue-from-json beads-render-guard--issue)))
        (t nil)))

(defmacro beads-render-guard--with-canned-reads (&rest body)
  "Run BODY with the explicit bd reads answering from canned data.
The async callbacks run from a timer, as real sentinels do."
  (declare (indent 0))
  `(cl-letf (((symbol-function 'beads-command-execute-async)
              (lambda (command on-success &optional _on-error &rest _)
                (let ((result (beads-render-guard--canned command)))
                  (run-at-time 0 nil (lambda () (funcall on-success result))))
                'queued))
             ((symbol-function 'beads-command-execute)
              (lambda (command)
                (signal 'beads-render-guard-io
                        (list 'sync-bd (beads-command-line command)))))
             ((symbol-function 'beads-command--policy-probe)
              (lambda (callback) (funcall callback nil))))
     ,@body))

(defun beads-render-guard--drain ()
  "Run pending timers (the canned async callbacks and vui renders)."
  (dotimes (_ 5)
    (let ((timers (copy-sequence timer-list)))
      (dolist (timer timers)
        (when (and (timerp timer) (not (timer--repeat-delay timer))
                   (<= (float-time (timer--time timer)) (+ (float-time) 0.5)))
          (cancel-timer timer)
          ;; Call directly: `timer-event-handler' would demote a guard
          ;; signal from the callback to a message.
          (apply (timer--function timer) (timer--args timer)))))))

(defun beads-render-guard--kill-remote-buffers ()
  "Kill the buffers opened on the guard prefix."
  (dolist (b (buffer-list))
    (when (string-prefix-p beads-render-guard--prefix
                           (or (buffer-local-value 'default-directory b) ""))
      (let ((kill-buffer-hook nil))
        (kill-buffer b)))))

(ert-deftest beads-render-guard-test-handler-self-check ()
  "The guard answers pure name operations and signals on I/O."
  :tags '(:unit)
  (beads-render-guard--with-guard
    (should (equal (file-remote-p "/guard:h:/srv/rig/") "/guard:h:"))
    (should (equal (file-local-name "/guard:h:/srv/rig/") "/srv/rig/"))
    (should (equal (expand-file-name "x" "/guard:h:/srv/rig/")
                   "/guard:h:/srv/rig/x"))
    (should (equal (file-name-nondirectory "/guard:h:/srv/rig") "rig"))
    (should-error (file-exists-p "/guard:h:/srv/rig/") :type 'beads-render-guard-io)
    (should-error (let ((default-directory "/guard:h:/srv/"))
                    (process-file "true"))
                  :type 'beads-render-guard-io)))

(ert-deftest beads-render-guard-test-show-remote-store ()
  "`beads-show' on a remote store opens and renders without file I/O."
  :tags '(:unit)
  (unwind-protect
      (beads-render-guard--with-canned-reads
        (beads-render-guard--with-guard
          (cl-letf (((symbol-function 'beads-buffer-display-detail) #'ignore))
            (let ((default-directory temporary-file-directory))
              (beads-show "bd-1" :directory "/guard:h:/srv/rig"))
            (beads-render-guard--drain)
            (let ((buf (seq-find (lambda (b)
                                   (with-current-buffer b
                                     (and (derived-mode-p 'beads-show-mode)
                                          (equal beads-store-directory
                                                 "/guard:h:/srv/rig/"))))
                                 (buffer-list))))
              (should buf)
              (with-current-buffer buf
                (should (string-match-p "Guarded issue" (buffer-string)))
                ;; Refresh is guarded too.
                (beads-refresh-show)
                (beads-render-guard--drain)
                (should (string-match-p "Guarded issue" (buffer-string)))
                ;; TAB motion and SPC folding stay off the host too.
                (goto-char (point-min))
                (beads-thing-forward)
                (beads-thing-forward -1)
                (goto-char (point-min))
                (re-search-forward "^DESCRIPTION$")
                (beads-thing-toggle)
                (beads-thing-toggle))))))
    (beads-render-guard--kill-remote-buffers)))

(ert-deftest beads-render-guard-test-dashboard-remote-store ()
  "`beads-dashboard' on a remote store opens and renders without file I/O."
  :tags '(:unit)
  (unwind-protect
      (beads-render-guard--with-canned-reads
        (beads-render-guard--with-guard
          (cl-letf (((symbol-function 'pop-to-buffer) #'ignore))
            (let ((default-directory temporary-file-directory))
              (beads-dashboard :directory "/guard:h:/srv/rig"))
            (beads-render-guard--drain)
            (let ((buf (seq-find (lambda (b)
                                   (with-current-buffer b
                                     (and (derived-mode-p 'beads-dashboard-mode)
                                          (equal beads-store-directory
                                                 "/guard:h:/srv/rig/"))))
                                 (buffer-list))))
              (should buf)
              (with-current-buffer buf
                (should (string-match-p "rig" (buffer-string)))
                ;; Sections rendered from the canned reads.
                (should (string-match-p "Guarded issue" (buffer-string)))
                ;; Folding and unfolding (SPC, depth keys) stay off the
                ;; host and never re-read: the data is already loaded
                ;; (§5.4, QA B-1/B-2).
                (let ((reads 0))
                  (cl-letf (((symbol-function 'beads-command-execute-async)
                             (lambda (&rest _) (cl-incf reads) 'queued)))
                    (goto-char (point-min))
                    (re-search-forward "^▾ .*Ready (")
                    (beads-thing-toggle)
                    (beads-render-guard--drain)
                    (goto-char (point-min))
                    (should (re-search-forward "^▸ .*Ready (1)" nil t))
                    (forward-line 0)
                    (beads-thing-toggle)
                    (beads-render-guard--drain)
                    (goto-char (point-min))
                    (should (re-search-forward "^▾ .*Ready (1)" nil t))
                    (should (string-match-p "Guarded issue" (buffer-string)))
                    (beads-dashboard-depth-1)
                    (beads-render-guard--drain)
                    (beads-dashboard-depth-all)
                    (beads-render-guard--drain))
                  (should (= reads 0))))))))
    (beads-render-guard--kill-remote-buffers)))

(ert-deftest beads-render-guard-test-show-async-deadline ()
  "An async show fetch is bounded by `beads-show-async-timeout'; its
failure lands in the buffer, not in a modal error."
  :tags '(:unit)
  (let ((beads-show-async t)
        (beads-show-async-timeout 7)
        kwargs buf)
    (unwind-protect
        (cl-letf (((symbol-function 'beads-command-execute-async)
                   (lambda (_command _on-success &optional on-error &rest kw)
                     (setq kwargs kw)
                     (funcall on-error '(error "bd timed out"))
                     'queued))
                  ((symbol-function 'beads-buffer-display-detail) #'ignore)
                  ((symbol-function 'beads-git-get-branch) (lambda () nil))
                  ((symbol-function 'beads-show--register-with-session) #'ignore))
          (setq buf (let ((default-directory temporary-file-directory))
                      (beads-show "bd-9" :directory temporary-file-directory)
                      (seq-find (lambda (b)
                                  (with-current-buffer b
                                    (and (derived-mode-p 'beads-show-mode)
                                         (equal beads-show--issue-id "bd-9"))))
                                (buffer-list))))
          (should (equal (plist-get kwargs :timeout) 7))
          (with-current-buffer buf
            (should (string-match-p "bd timed out" (buffer-string)))))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(provide 'beads-render-guard-test)
;;; beads-render-guard-test.el ends here
