;;; beads-events-history.el --- Per-issue journal history for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The per-issue history view over the `bd events' journal (design of
;; record: `plans/beads-events-live/design.md' sections 3.2, 3.3, 7.6;
;; WI-LIVE-16).  It is split out of `beads-events.el' because that file
;; grew past the size threshold the design set for the split.
;;
;; `beads-events-history' shows one issue's lifecycle oldest-to-newest
;; but renders it newest-first, `git log' style.  Each mutation carries
;; a field-level diff produced by the pure `beads-event' model
;; (`beads-event-record-diff' / `beads-event-diff-string'): a `create',
;; an `update' with `status open -> in_progress', a `dep_add' as
;; `dependency_added <id>', and a `delete' as a tombstone.  A diff is
;; computed against the running sparse-merged snapshot, so the journal's
;; `omitempty' contract holds: an absent optional field reads as its
;; zero value, and fields outside the wire subset (description,
;; dependencies, comments, counts, revision) are never clobbered.
;;
;; The view is read-only and never writes.  It is intentionally
;; standalone: `beads-live' and `beads-events' are resolved at call time
;; and guarded by `fboundp', so the module loads and renders with only
;; the pure model present.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'beads-faces)
(require 'beads-types)
(require 'beads-thing)
(require 'beads-buffer)
(require 'beads-util)

(declare-function beads-event-record-p "beads-event" (object))
(declare-function beads-event-record-diff "beads-event" (record &optional previous))
(declare-function beads-event-diff-string "beads-event" (change))
(declare-function beads-event-time "beads-event" (record))
(declare-function beads-event-actor-description "beads-event" (record))
(declare-function beads-events--source-records "beads-events" ())
(declare-function beads-events--record-at-point "beads-events" ())
(declare-function beads-events-mode "beads-events" (&optional arg))
(declare-function beads-live-model "beads-live" (stream))
(declare-function beads-live--model-ring "beads-live" (model))
(declare-function beads-show "beads-command-show" (issue-id &rest args))

(defvar beads-store-directory)
(defvar beads-events--stream)
(defvar beads-events-mode-map)

;;; Options

(defcustom beads-events-history-limit 1000
  "Maximum number of records `beads-events-history' renders.
The oldest records beyond the limit are dropped from the view, matching
the bounded ring the history reads from."
  :type 'integer
  :group 'beads)

;;; Buffer-local state

(defvar-local beads-events-history--records nil
  "Oldest-first records the current history buffer renders.")

(defvar-local beads-events-history--issue-id nil
  "The issue id the current history buffer renders.")

(defvar-local beads-events-history--store nil
  "The store directory the current history buffer is scoped to.")

;;; Injection seam

(defvar beads-events-history-records-function nil
  "Optional function (STORE ISSUE-ID) returning history records.
When nil, the view reads the current `beads-events' buffer or the live
model.  A test or an embedding may set this to inject records without a
stream.")

;;; Wire subset and sparse merge

(defconst beads-events-history--wire-fields
  '(id title status priority issue_type owner created_by created_at
    updated_at is_blocked assignee labels started_at lease_expires_at
    heartbeat_at closed_at close_reason)
  "Wire fields a journal `issue' snapshot carries authoritatively.
Mirrors `beads-live--issue-subset'; fields outside it are never in a
snapshot, so a diff must not report their absence as a change.")

(defun beads-events-history--issue->snapshot (issue)
  "Return a wire-shaped alist for beads-issue ISSUE, or nil.
The wire keys are snake_case, matching the journal's JSON."
  (when issue
    (cl-loop for (key . slot) in
             '((id . id) (title . title) (status . status)
               (priority . priority) (issue_type . issue-type)
               (owner . owner) (created_by . created-by)
               (assignee . assignee) (labels . labels)
               (created_at . created-at) (updated_at . updated-at)
               (started_at . started-at) (closed_at . closed-at)
               (close_reason . close-reason)
               (lease_expires_at . lease-expires-at)
               (heartbeat_at . heartbeat-at))
             collect (cons key (and (slot-exists-p issue slot)
                                    (slot-value issue slot))))))

(defun beads-events-history--record-snapshot (record)
  "Return RECORD's raw wire issue snapshot, or nil for a `delete'.
Prefers the raw alist `beads-live' keeps when it is loaded; otherwise
converts the parsed `beads-issue' snapshot."
  (if (fboundp 'beads-live--record-snapshot)
      (beads-live--record-snapshot record)
    (beads-events-history--issue->snapshot (oref record issue))))

(defun beads-events-history--merge (previous snapshot)
  "Return PREVIOUS merged with journal SNAPSHOT under the sparse rule.
PREVIOUS is a wire alist or nil.  Every wire field is authoritative: a
present key is taken from SNAPSHOT and an absent optional key is set to
its zero value; every field outside the wire subset keeps its value."
  (if (null snapshot)
      previous
    (let ((merged (cl-remove-if
                   (lambda (pair)
                     (memq (car pair) beads-events-history--wire-fields))
                   (copy-sequence previous))))
      (dolist (key beads-events-history--wire-fields)
        (when-let* ((pair (assq key snapshot)))
          (push (cons key (cdr pair)) merged)))
      (nreverse merged))))

;;; Record collection

(defun beads-events-history--records-for-issue (records issue-id)
  "Return RECORDS for ISSUE-ID, oldest-first.
Matches the literal `issue-id' and falls back to ordering by timestamp
when a record has no sequence number."
  (let ((kept (seq-filter (lambda (record)
                            (and (beads-event-record-p record)
                                 (equal (oref record issue-id) issue-id)))
                          records)))
    (sort (copy-sequence kept)
          (lambda (a b)
            (let ((sa (or (oref a seq) 0))
                  (sb (or (oref b seq) 0)))
              (if (/= sa sb)
                  (< sa sb)
                (< (beads-event-time a) (beads-event-time b))))))))

(defun beads-events-history--collect (store issue-id)
  "Return STORE's records for ISSUE-ID, oldest-first.
Resolution order: `beads-events-history-records-function', the current
`beads-events' buffer, then the live model ring.  Returns nil when no
source is available."
  (or (when (functionp beads-events-history-records-function)
        (condition-case nil
            (beads-events-history--records-for-issue
             (funcall beads-events-history-records-function store issue-id)
             issue-id)
          (error nil)))
      (when (and (fboundp 'beads-events--source-records)
                 (derived-mode-p 'beads-events-mode))
        (beads-events-history--records-for-issue
         (beads-events--source-records) issue-id))
      (when-let* ((stream (and (boundp 'beads-events--stream)
                               beads-events--stream))
                  ((fboundp 'beads-live-model)))
        (condition-case nil
            (beads-events-history--records-for-issue
             (append (beads-live--model-ring (beads-live-model stream)) nil)
             issue-id)
          (error nil)))))

;;; Rendering

(defun beads-events-history--time-string (record)
  "Return RECORD's timestamp as `YYYY-MM-DD HH:MM:SS', or an em dash."
  (let ((at (beads-event-time record)))
    (if (> at 0) (format-time-string "%Y-%m-%d %H:%M:%S" at) "—")))

(defun beads-events-history--actor-string (record)
  "Return RECORD's actor for display, `system' when absent."
  (if (fboundp 'beads-event-actor-description)
      (beads-event-actor-description record)
    (let ((actor (oref record actor)))
      (if (and (stringp actor) (not (string-empty-p actor))) actor "system"))))

(defun beads-events-history--diff-lines (record previous)
  "Return RECORD's field-diff display lines against PREVIOUS.
Tombstones and dependency payloads render through the pure model's
presentation function."
  (if (fboundp 'beads-event-record-diff)
      (mapcar #'beads-event-diff-string
              (beads-event-record-diff record previous))
    (list (format "%s" (or (oref record op) "")))))

(defun beads-events-history--insert-record (record previous)
  "Insert RECORD's block, diffed against PREVIOUS.
Returns the record so callers can thread it as the next PREVIOUS."
  (let* ((op (or (oref record op) "?"))
         (actor (beads-events-history--actor-string record))
         (seq (or (oref record seq) 0))
         (head (format "%s  %-8s  @%s  #%d"
                       (beads-events-history--time-string record)
                       op actor seq))
         (thing (list :kind 'event :record record :issue-id
                      (oref record issue-id))))
    (insert (beads-thing-propertize head thing) "\n")
    (dolist (line (beads-events-history--diff-lines record previous))
      (insert (propertize (concat "    " line) 'face 'shadow) "\n"))
    (insert "\n")
    record))

(defun beads-events-history--render ()
  "Re-render this history buffer from its records, newest-first."
  (let* ((records beads-events-history--records)
         (issue-id beads-events-history--issue-id)
         (state nil)
         (blocks nil))
    ;; Compute diffs oldest-first against the running sparse snapshot...
    (dolist (record records)
      (push (cons record state) blocks)
      (setq state (beads-events-history--merge
                   state (beads-events-history--record-snapshot record))))
    ;; ...then insert newest-first.
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (propertize
               (format "%s — %d event%s"
                       (or issue-id "?")
                       (length records)
                       (if (= (length records) 1) "" "s"))
               'face 'beads-face-header)
              "\n")
      (insert (make-string 72 ?─) "\n\n")
      (dolist (block blocks)
        (beads-events-history--insert-record (car block) (cdr block))))
    (goto-char (point-min))))

;;; Major mode

(defvar beads-events-history-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (keymap-set map "g" #'beads-events-history-refresh)
    (keymap-set map "q" #'quit-window)
    (keymap-set map "RET" #'beads-events-history-visit)
    map)
  "Keymap for `beads-events-history-mode'.")

(define-derived-mode beads-events-history-mode special-mode "Beads-History"
  "Major mode of the per-issue `bd events' history view.
\\{beads-events-history-mode-map}"
  (setq-local header-line-format
              '(:eval (beads-events-history--header-line)))
  (beads-mode--install-navigation-keys beads-events-history-mode-map))

(defun beads-events-history--header-line ()
  "Return this history view's header line."
  (format " %s history  %d events  store: %s"
          (or beads-events-history--issue-id "?")
          (length beads-events-history--records)
          (or beads-events-history--store "?")))

(defun beads-events-history-refresh ()
  "Re-read and re-render this history view."
  (interactive)
  (unless (derived-mode-p 'beads-events-history-mode)
    (user-error "Not in a beads events history buffer"))
  (setq beads-events-history--records
        (beads-events-history--collect beads-events-history--store
                                       beads-events-history--issue-id))
  (beads-events-history--render))

(defun beads-events-history-visit ()
  "Visit this history's issue in a show buffer."
  (interactive)
  (unless beads-events-history--issue-id
    (user-error "No issue for this history"))
  (require 'beads-command-show)
  (beads-show beads-events-history--issue-id
              :directory beads-events-history--store))

;;; Entry points

(defun beads-events-history--store-name (store)
  "Return a display name for STORE, or nil."
  (when (and store (stringp store) (not (string-empty-p store)))
    (file-name-nondirectory (directory-file-name store))))

(defun beads-events-history--buffer-name (store issue-id)
  "Return the history buffer name for STORE and ISSUE-ID."
  (format "*beads-events-history[%s][%s]*"
          (or (beads-events-history--store-name store) "beads")
          (or issue-id "?")))

(defun beads-events-history--configure (store issue-id records)
  "Set this buffer's history state for STORE, ISSUE-ID and RECORDS."
  (setq beads-events-history--store store
        beads-events-history--issue-id issue-id
        beads-events-history--records records))

;;;###autoload
(defun beads-events-history (issue-id &optional directory records)
  "Show the `bd events' history of ISSUE-ID.
ISSUE-ID is the issue to show.  DIRECTORY scopes the store; RECORDS,
from Lisp, injects the records to render (used by tests).  The view is
read-only and never writes."
  (interactive
   (list (let ((at (and (derived-mode-p 'beads-events-mode)
                        (fboundp 'beads-events--record-at-point)
                        (ignore-errors (beads-events--record-at-point)))))
           (or (and at (oref at issue-id))
               (read-string "Issue id: ")))))
  (let* ((store (or (beads-store-resolve directory) beads-store-directory))
         (records (or records (beads-events-history--collect store issue-id)))
         (records (if (> (length records) beads-events-history-limit)
                      (last records beads-events-history-limit)
                    records))
         (buffer (get-buffer-create
                  (beads-events-history--buffer-name store issue-id))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'beads-events-history-mode)
        (beads-events-history-mode))
      (beads-events-history--configure store issue-id records)
      (beads-events-history--render))
    (pop-to-buffer buffer)))

;;;###autoload
(defun beads-events-history-at-point ()
  "Show the history of the issue of the record at point.
Bound to `H' in `beads-events-mode'."
  (interactive)
  (let ((record (and (fboundp 'beads-events--record-at-point)
                     (beads-events--record-at-point))))
    (unless (and record (oref record issue-id))
      (user-error "No issue at point"))
    (beads-events-history (oref record issue-id))))

(provide 'beads-events-history)
;;; beads-events-history.el ends here
