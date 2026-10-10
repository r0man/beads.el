;;; beads-events-rewind.el --- Read-only journal time travel for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Rewind the beads model to an earlier journal sequence number
;; (design of record: `plans/beads-events-live/design.md' sections 3.3,
;; 4.6, 7.8; WI-LIVE-16).  Split out of `beads-events.el' past the size
;; threshold the design set.
;;
;; `beads-events-rewind' prompts for a seq, or a relative `-N'/`+N',
;; and renders the sparse-merged issue state as of that seq in a
;; read-only buffer with a distinct REWIND header and mode-line.  `g'
;; and `G' step event-by-event so the user can watch the replay; `l'
;; and `r' resume live.  It never writes: no `bd' command, no buffer
;; mutation beyond the rewind buffer's own text, and no store change.
;;
;; The replay is a pure reduce of the wire snapshots over a baseline,
;; newest state last.  Every wire field is authoritative under the
;; journal's `omitempty' contract; fields outside the wire subset keep
;; their baseline value.  When `beads-live' is loaded the replay calls
;; its canonical `beads-live--merge-issue', so the state at seq K is
;; exactly the state the live model would show at K; otherwise a local
;; merge with identical semantics is used.
;;
;; Rewind snapshot materialization is deliberately deferred.  Replay
;; reads the bounded in-memory ring first (design section 13, open
;; decision (a)); only records still retained are replayed, so a rewind
;; below the ring floor reconstructs the retained window, never a
;; fabricated older state.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'beads-faces)
(require 'beads-types)
(require 'beads-thing)
(require 'beads-buffer)
(require 'beads-util)
(require 'beads-events-history)

(declare-function beads-event-record-p "beads-event" (object))
(declare-function beads-event-time "beads-event" (record))
(declare-function beads-live-model "beads-live" (stream))
(declare-function beads-live--model-ring "beads-live" (model))
(declare-function beads-live--merge-issue "beads-live" (model id snapshot))
(declare-function beads-live--model-create "beads-live" ())
(declare-function beads-live--model-issues "beads-live" (model))
(declare-function beads-events--source-records "beads-events" ())
(declare-function beads-events--record-at-point "beads-events" ())
(declare-function beads-events-mode "beads-events" (&optional arg))

(defvar beads-store-directory)
(defvar beads-events--stream)
(defvar beads-events-mode-map)

;;; Injection seam

(defvar beads-events-rewind-records-function nil
  "Optional function (STORE) returning the records to replay.
When nil, the view reads the current `beads-events' buffer or the live
model ring.  A test or an embedding may inject records without a
stream.")

(defvar-local beads-events-rewind--records nil
  "Oldest-first records the current rewind buffer replays.")

(defvar-local beads-events-rewind--baseline nil
  "Alist of (ID . WIRE-SNAPSHOT) state before the first replayed record.")

(defvar-local beads-events-rewind--seq 0
  "The sequence number the current rewind buffer renders.")

(defvar-local beads-events-rewind--head 0
  "The newest sequence number among the replayed records.")

(defvar-local beads-events-rewind--store nil
  "The store directory this rewind buffer is scoped to.")

;;; Pure replay

(defun beads-events-rewind--replay (records k apply-fn)
  "Replay RECORDS with seq <= K oldest-first through APPLY-FN.
APPLY-FN is called with (ID WIRE-SNAPSHOT) for each record."
  (dolist (record (sort (copy-sequence records)
                        (lambda (a b)
                          (< (or (oref a seq) 0) (or (oref b seq) 0)))))
    (when (<= (or (oref record seq) 0) k)
      (funcall apply-fn (oref record issue-id)
               (beads-events-history--record-snapshot record)))))

(defun beads-events-rewind--state-at (records baseline k)
  "Return the issue state as of seq K.
RECORDS is a list of `beads-event-record'; BASELINE is an alist of
\(ID . WIRE-SNAPSHOT) state before the first record; K is the inclusive
sequence bound.  Records are replayed oldest-first and the result is a
hash of id -> wire alist, or `:deleted' for a tombstone.  When
`beads-live' is loaded the replay calls its canonical
`beads-live--merge-issue', so the state matches the live model exactly."
  (if (and (fboundp 'beads-live--merge-issue)
           (fboundp 'beads-live--model-create))
      (let ((model (beads-live--model-create)))
        (dolist (pair baseline)
          (puthash (car pair) (cdr pair) (beads-live--model-issues model)))
        (beads-events-rewind--replay
         records k
         (lambda (id snapshot)
           (beads-live--merge-issue model id snapshot)))
        (beads-live--model-issues model))
    (let ((state (make-hash-table :test 'equal)))
      (dolist (pair baseline)
        (puthash (car pair) (cdr pair) state))
      (beads-events-rewind--replay
       records k
       (lambda (id snapshot)
         (when id
           (if (null snapshot)
               (puthash id :deleted state)
             (puthash id (beads-events-history--merge
                          (gethash id state) snapshot)
                      state)))))
      state)))

(defun beads-events-rewind--head-seq (records)
  "Return the newest sequence number in RECORDS, or 0."
  (let ((head 0))
    (dolist (record records)
      (when (beads-event-record-p record)
        (setq head (max head (or (oref record seq) 0)))))
    head))

(defun beads-events-rewind--parse-target (input head)
  "Return INPUT resolved against HEAD: a seq integer, or `live'.
INPUT may be blank (live), an absolute seq, or `-N' / `+N' relative to
HEAD.  Signals `user-error' on anything else."
  (let ((trimmed (string-trim (or input ""))))
    (cond
     ((string-empty-p trimmed) 'live)
     ((string-match "\\`\\([+-]\\)\\([0-9]+\\)\\'" trimmed)
      (let ((n (string-to-number (match-string 2 trimmed))))
        (if (equal (match-string 1 trimmed) "-")
            (max 0 (- head n))
          (min head (+ head n)))))
     ((string-match "\\`[0-9]+\\'" trimmed)
      (string-to-number trimmed))
     (t (user-error "Invalid rewind target: %s" input)))))

(defun beads-events-rewind--read-target ()
  "Prompt for a rewind target."
  (read-string "Rewind to seq (or -N / +N, blank = live): "))

(defun beads-events-rewind--next-seq (records k)
  "Return the smallest record seq greater than K, or nil."
  (let ((next nil))
    (dolist (record records)
      (let ((seq (or (oref record seq) 0)))
        (when (and (> seq k) (or (null next) (< seq next)))
          (setq next seq))))
    next))

(defun beads-events-rewind--previous-seq (records k)
  "Return the largest record seq less than K, or nil."
  (let ((prev nil))
    (dolist (record records)
      (let ((seq (or (oref record seq) 0)))
        (when (and (< seq k) (or (null prev) (> seq prev)))
          (setq prev seq))))
    prev))

;;; Rendering

(defun beads-events-rewind--status-string (snapshot)
  "Return SNAPSHOT's status, or `unknown'."
  (or (alist-get 'status snapshot) (alist-get 'state snapshot) "unknown"))

(defun beads-events-rewind--issue-line (id snapshot)
  "Return one display line for issue ID in SNAPSHOT.
A `:deleted' snapshot renders a tombstone."
  (if (eq snapshot :deleted)
      (propertize (format "  %-12s  deleted" id) 'face 'shadow)
    (let* ((status (beads-events-rewind--status-string snapshot))
           (face (beads-face-status-face status))
           (title (or (alist-get 'title snapshot) ""))
           (priority (alist-get 'priority snapshot))
           (assignee (alist-get 'assignee snapshot)))
      (concat
       (format "  %-12s  " id)
       (propertize (format "%-11s" status) 'face face)
       (if (numberp priority) (format " P%d" priority) "")
       (if (and title (not (string-empty-p title))) (format "  %s" title) "")
       (if (and assignee (not (string-empty-p assignee)))
           (format "  @%s" assignee)
         "")))))

(defun beads-events-rewind--lines (state)
  "Return the rendered issue lines of STATE, sorted by id."
  (let ((ids (sort (hash-table-keys state) #'string<)))
    (mapcar (lambda (id) (beads-events-rewind--issue-line id (gethash id state)))
            ids)))

(defun beads-events-rewind--header-line ()
  "Return the read-only REWIND header line."
  (propertize
   (format " ⏪ REWIND @%d · +%d events · read-only"
           beads-events-rewind--seq
           (max 0 (- beads-events-rewind--head beads-events-rewind--seq)))
   'face 'beads-events-rewind))

(defun beads-events-rewind--render ()
  "Re-render this rewind buffer from its records at its current seq."
  (let* ((state (beads-events-rewind--state-at
                 beads-events-rewind--records
                 beads-events-rewind--baseline
                 beads-events-rewind--seq))
         (lines (beads-events-rewind--lines state))
         (inhibit-read-only t))
    (erase-buffer)
    (insert (propertize
             (format "State at seq %d  (%d issue%s)"
                     beads-events-rewind--seq
                     (length lines)
                     (if (= (length lines) 1) "" "s"))
             'face 'beads-face-header)
            "\n")
    (insert (make-string 72 ?─) "\n\n")
    (if lines
        (dolist (line lines) (insert line "\n"))
      (insert (propertize "  (no issues in the retained window)\n" 'face 'shadow)))
    (goto-char (point-min))))

(defun beads-events-rewind--set-seq (seq)
  "Render the buffer at SEQ and refresh the mode line."
  (setq beads-events-rewind--seq (max 0 seq))
  (beads-events-rewind--render)
  (force-mode-line-update))

;;; Commands

(defun beads-events-rewind-refresh ()
  "Re-render this rewind buffer at its current seq."
  (interactive)
  (unless (derived-mode-p 'beads-events-rewind-mode)
    (user-error "Not in a beads rewind buffer"))
  (beads-events-rewind--render))

(defun beads-events-rewind-forward ()
  "Step the rewind cursor to the next event."
  (interactive)
  (let ((next (beads-events-rewind--next-seq
               beads-events-rewind--records beads-events-rewind--seq)))
    (if next
        (beads-events-rewind--set-seq next)
      (message "Already at the newest event"))))

(defun beads-events-rewind-backward ()
  "Step the rewind cursor to the previous event."
  (interactive)
  (let ((prev (beads-events-rewind--previous-seq
               beads-events-rewind--records beads-events-rewind--seq)))
    (if prev
        (beads-events-rewind--set-seq prev)
      (message "Already at the oldest retained event"))))

(defun beads-events-rewind-jump ()
  "Prompt for a seq and rewind to it."
  (interactive)
  (beads-events-rewind--set-seq
   (beads-events-rewind--parse-target
    (beads-events-rewind--read-target) beads-events-rewind--head)))

(defun beads-events-rewind-resume ()
  "Resume live and leave the rewind view.
Rewind never writes; resuming simply buries the read-only buffer."
  (interactive)
  (quit-window))

;;; Major mode

(defvar beads-events-rewind-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (keymap-set map "g" #'beads-events-rewind-forward)
    (keymap-set map "G" #'beads-events-rewind-backward)
    (keymap-set map "l" #'beads-events-rewind-resume)
    (keymap-set map "r" #'beads-events-rewind-resume)
    (keymap-set map "J" #'beads-events-rewind-jump)
    (keymap-set map "q" #'quit-window)
    map)
  "Keymap for `beads-events-rewind-mode'.")

(define-derived-mode beads-events-rewind-mode special-mode "Beads-Rewind"
  "Major mode of the read-only `bd events' rewind view.
\\{beads-events-rewind-mode-map}"
  (setq-local header-line-format
              '(:eval (beads-events-rewind--header-line)))
  (setq-local mode-line-process
              '(:eval (propertize " ⏪ REWIND" 'face 'beads-events-rewind)))
  (beads-mode--install-navigation-keys beads-events-rewind-mode-map))

;;; Entry points

(defun beads-events-rewind--buffer-name (store)
  "Return the rewind buffer name for STORE."
  (format "*beads-rewind[%s]*"
          (or (beads-events-history--store-name store) "beads")))

(defun beads-events-rewind--collect (store)
  "Return STORE's records, oldest-first.
Resolution order: `beads-events-rewind-records-function', the current
`beads-events' buffer, then the live model ring."
  (or (when (functionp beads-events-rewind-records-function)
        (condition-case nil
            (copy-sequence (funcall beads-events-rewind-records-function store))
          (error nil)))
      (when (and (fboundp 'beads-events--source-records)
                 (derived-mode-p 'beads-events-mode))
        (copy-sequence (beads-events--source-records)))
      (when-let* ((stream (and (boundp 'beads-events--stream)
                               beads-events--stream))
                  ((fboundp 'beads-live-model)))
        (condition-case nil
            (append (beads-live--model-ring (beads-live-model stream)) nil)
          (error nil)))))

(defun beads-events-rewind--configure (store records baseline)
  "Set this buffer's rewind state for STORE, RECORDS and BASELINE."
  (setq beads-events-rewind--store store
        beads-events-rewind--records records
        beads-events-rewind--baseline baseline
        beads-events-rewind--head (beads-events-rewind--head-seq records)))

;;;###autoload
(defun beads-events-rewind (target &optional directory records baseline)
  "Render the issue state as of TARGET, read-only.
TARGET is a seq integer, or a relative target string such as \"-5\"
or \"+3\", or `live' to resume.  DIRECTORY scopes the store.  RECORDS
and BASELINE, from Lisp, inject the records to replay and the state
before them (used by tests).  Rewind never writes."
  (interactive (list (beads-events-rewind--read-target)))
  (let* ((store (or (beads-store-resolve directory) beads-store-directory))
         (records (or records (beads-events-rewind--collect store)))
         (head (beads-events-rewind--head-seq records))
         (resolved (if (stringp target)
                       (beads-events-rewind--parse-target target head)
                     target)))
    (if (eq resolved 'live)
        (message "beads: live (rewind is a read-only view; nothing to resume)")
      (let ((buffer (get-buffer-create (beads-events-rewind--buffer-name store))))
        (with-current-buffer buffer
          (unless (derived-mode-p 'beads-events-rewind-mode)
            (beads-events-rewind-mode))
          (beads-events-rewind--configure store records baseline)
          (beads-events-rewind--set-seq resolved))
        (pop-to-buffer buffer)))))

(defun beads-events-rewind-at-point ()
  "Rewind to the seq of the record at point.
Bound to `r' in `beads-events-mode'."
  (interactive)
  (let ((record (and (fboundp 'beads-events--record-at-point)
                     (beads-events--record-at-point))))
    (unless record
      (user-error "No event at point"))
    (beads-events-rewind (or (oref record seq) 0))))

;;; beads-events integration (resolved after that module loads)

(with-eval-after-load 'beads-events
  (when (boundp 'beads-events-mode-map)
    (keymap-set beads-events-mode-map "r" #'beads-events-rewind-at-point)))

(provide 'beads-events-rewind)
;;; beads-events-rewind.el ends here
