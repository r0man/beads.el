;;; beads-events.el --- Live journal timeline views for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The Events views over the durable `bd events' journal (design of
;; record: `plans/beads-events-live/design.md' §3.3; WI-LIVE-15).  A
;; `tabulated-list-mode' over `beads-pager', newest-first, with live
;; append, churn folding, op/actor/issue filters, and org export.  It
;; mirrors the `gascity-events' conventions for `gc events'.
;;
;; Two views live here:
;;
;; - `beads-events-timeline' shows one store's records.  It renders
;;   from the stream supervisor's in-memory model when `beads-live' is
;;   loaded, and from the buffer-local record list otherwise, so the
;;   view, its filters and its export are fully unit-testable without a
;;   subprocess.
;; - `beads-events-timeline-city' merges several stores' record sets
;;   by wall clock (`beads-event-time').  Per-store sequence spaces are
;;   never compared: the merge sorts by timestamp and tags each row
;;   with its store, and the optional session tag comes only from the
;;   gascity-free `beads-event-session-resolver' /
;;   `beads-events-session-table' seam (absent by default).
;;
;; Every `beads-live' / `beads-event' entry point is resolved at call
;; time and guarded by `fboundp', so this file loads and renders when
;; the stream supervisor and the pure model are absent.  The pure model
;; is preferred when present; a small local fallback (op -> level,
;; churn fold) keeps the view correct in isolation.
;;
;; History (`beads-events-history') and rewind (`beads-events-rewind')
;; are WI-LIVE-16 and live in their own files once this module grows.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'beads-custom)
(require 'beads-util)
(require 'beads-types)
(require 'beads-faces)
(require 'beads-pager)
(require 'beads-thing)
(require 'beads-buffer)

(declare-function beads-live-attach "beads-live" (&optional buffer &rest args))
(declare-function beads-live-detach "beads-live" (&optional buffer))
(declare-function beads-live-subscribe "beads-live" (fn &optional buffer))
(declare-function beads-live-unsubscribe "beads-live" (handle))
(declare-function beads-live-header-string "beads-live" (&optional dir))
(declare-function beads-live-toggle "beads-live" (&optional dir))
(declare-function beads-live-model "beads-live" (stream))
(declare-function beads-live--model-ring "beads-live" (model))
(declare-function beads-event-level "beads-event" (record &optional diff))
(declare-function beads-event-glyph "beads-event" (thing))
(declare-function beads-event-subject "beads-event" (record))
(declare-function beads-event-actor-description "beads-event" (record))
(declare-function beads-event-time "beads-event" (record))
(declare-function beads-event-window-seconds "beads-event" (window))
(declare-function beads-event-fold "beads-event" (records &optional bucket))
(declare-function beads-show "beads-command-show" (issue-id &rest args))
(declare-function org-mode "org" (&optional arg))

;;; Options

(defcustom beads-events-window "1h"
  "Default time window of the Events views.
A duration such as \"30m\", \"2h\", \"7d\" or \"2w\"; see
`beads-event-window-seconds'."
  :type 'string
  :group 'beads)

(defcustom beads-events-fold-bucket 900
  "Seconds per churn bucket in `beads-events-timeline'.
Non-signal records with the same issue and op inside one bucket fold
into a single `×N' row."
  :type 'integer
  :group 'beads)

(defcustom beads-events-limit 1000
  "Maximum number of records rendered by the Events views."
  :type 'integer
  :group 'beads)

(defvar beads-events-append-delay 0.5
  "Seconds live records are gathered before the view re-renders.")

(defvar beads-events-records-function nil
  "Optional function (STORE) returning that store's records for a view.
When nil the view reads the `beads-live' model if it is loaded, else
the buffer-local record list.  A test or an embedding may set this to
inject records without a stream.")

(defvar beads-events-city-stores-function nil
  "Optional function returning the city timeline's stores.
It returns an alist of (STORE . RECORDS).  When nil,
`beads-events-city-stores' is consulted, then the current buffer's
store.")

(defvar beads-events-city-stores nil
  "Alist of (STORE . RECORDS) for `beads-events-timeline-city'.
Used when `beads-events-city-stores-function' is nil.")

;;; Session attribution seam (optional, gascity-free)

(defvar beads-event-session-resolver nil
  "Abnormal hook (ROOT ACTOR) -> session string or nil.
Optional and gascity-free.  When it yields nothing, the session column
is absent, never a blank placeholder (design.md §3.6).")

(defvar beads-events-session-table nil
  "Alist of (ACTOR . SESSION) for the city timeline's session column.
Consulted after `beads-event-session-resolver'; empty by default, so no
session is ever invented.")

(defun beads-events-session-tag (store actor)
  "Return the session tag for ACTOR in STORE, or nil.
Resolution goes only through the optional, gascity-free
`beads-event-session-resolver' and `beads-events-session-table' seams."
  (or (run-hook-with-args-until-success 'beads-event-session-resolver
                                        store actor)
      (and actor (cdr (assoc actor beads-events-session-table)))))

;;; Buffer-local view state

(defvar-local beads-events--records nil
  "Newest-first list of `beads-event-record' shown by this buffer.
The live source when no stream is attached, and the injection seam for
tests.")

(defvar-local beads-events--store nil
  "The store directory this view is scoped to, or nil in city mode.")

(defvar-local beads-events--city nil
  "The city name when this is a merged city timeline, else nil.")

(defvar-local beads-events--store-name nil
  "Display name of `beads-events--store' or `beads-events--city'.")

(defvar-local beads-events--store-of nil
  "Eq hash mapping a record to its store in city mode, or nil.")

(defvar-local beads-events--stream nil
  "The `beads-live' stream handle this buffer is attached to, or nil.")

(defvar-local beads-events--handle nil
  "The `beads-live-subscribe' handle of this buffer, or nil.")

(defvar-local beads-events--filter nil
  "Plist of active filters: :op :actor :issue :level :window :search.")

(defvar-local beads-events--expanded nil
  "List of churn keys currently unfolded inline.")

(defvar-local beads-events--shown 0
  "Number of records shown by the last render (after filtering).")

(defvar-local beads-events--folded 0
  "Number of records folded into churn rows by the last render.")

(defvar-local beads-events--has-session nil
  "Non-nil when at least one shown record has a session tag.")

(defvar-local beads-events--queue nil
  "Live records waiting for the next debounced append, newest first.")

(defvar-local beads-events--flush-timer nil
  "The debounce timer of pending live appends.")

(defvar-local beads-events--generation 0
  "Render generation, bumped on every refresh.")

(defvar beads-events--live-inhibit noninteractive
  "When non-nil, do not attach a live stream.
Defaults to `noninteractive' so a batch run (including the test suite)
never spawns a subprocess; interactive sessions attach normally.")

;;; Pure model access (prefer `beads-event', fall back locally)

(defun beads-events--level (record)
  "Return RECORD's signal level, or nil.
Uses the pure `beads-event' model when loaded; otherwise maps the seven
journal ops locally.  A `create'/`comment'/`dep_remove' is a plain
event; `update' is `watch'; `close'/`delete'/`dep_add' are `attention'."
  (if (fboundp 'beads-event-level)
      (beads-event-level record)
    (pcase (oref record op)
      ((or "close" "delete" "dep_add") 'attention)
      ("update" 'watch)
      (_ nil))))

(defun beads-events--glyph (record)
  "Return RECORD's signal glyph."
  (if (fboundp 'beads-event-glyph)
      (beads-event-glyph record)
    (pcase (beads-events--level record)
      ('attention "■")
      ('watch "▲")
      (_ " "))))

(defun beads-events--subject (record)
  "Return RECORD's subject text."
  (if (fboundp 'beads-event-subject)
      (beads-event-subject record)
    (let ((id (or (oref record issue-id) ""))
          (op (or (oref record op) "")))
      (if (string-empty-p id) op (format "%s — %s" id op)))))

(defun beads-events--actor (record)
  "Return RECORD's actor for display, or \"system\" when absent."
  (if (fboundp 'beads-event-actor-description)
      (beads-event-actor-description record)
    (let ((actor (oref record actor)))
      (if (and (stringp actor) (not (string-empty-p actor)))
          actor
        "system"))))

(defun beads-events--time (record)
  "Return RECORD's timestamp as a float, or 0 when absent/unparseable."
  (if (fboundp 'beads-event-time)
      (beads-event-time record)
    (let ((ts (oref record ts)))
      (if (and (stringp ts) (not (string-empty-p ts)))
          (condition-case nil (float-time (date-to-time ts)) (error 0))
        0))))

(defun beads-events--window-seconds (window)
  "Return WINDOW as a number of seconds."
  (if (fboundp 'beads-event-window-seconds)
      (beads-event-window-seconds window)
    (cond
     ((numberp window) window)
     ((and (stringp window) (string-match-p "\\`[0-9]+\\'" window))
      (string-to-number window))
     ((and (stringp window)
           (string-match "\\`\\([0-9]+\\)\\([smhdw]\\)\\'" window))
      (* (string-to-number (match-string 1 window))
         (pcase (match-string 2 window)
           ("s" 1) ("m" 60) ("h" 3600) ("d" 86400) ("w" 604800))))
     (t 3600))))

(defun beads-events--fallback-fold (records bucket)
  "Fold RECORDS into (ROWS . FOLDED) newest-first, without `beads-event'.
Mirrors the pure model's churn convention: `attention'/`watch'
records never fold; the rest coalesce per issue and op per BUCKET."
  (let ((buckets (make-hash-table :test 'equal))
        (rows nil)
        (folded 0))
    (dolist (record records)
      (if (beads-events--level record)
          (push (list 'event record) rows)
        (setq folded (1+ folded))
        (let* ((time (beads-events--time record))
               (key (format "%s@%s@%d"
                            (or (oref record issue-id) "")
                            (or (oref record op) "")
                            (floor time bucket)))
               (row (gethash key buckets)))
          (if row
              (progn
                (setf (nth 3 row) (max (nth 3 row) time))
                (push record (nth 4 row)))
            (setq row (list 'churn key
                            (format "%s %s"
                                    (or (oref record issue-id) "")
                                    (or (oref record op) ""))
                            time (list record)))
            (puthash key row buckets)
            (push row rows)))))
    (cons (mapcar #'cdr
                  (sort (mapcar (lambda (row)
                                  (cons (if (eq (car row) 'churn)
                                            (nth 3 row)
                                          (beads-events--time (nth 1 row)))
                                        row))
                                rows)
                        (lambda (a b) (> (car a) (car b)))))
          folded)))

(defun beads-events--fold (records bucket)
  "Fold RECORDS into (ROWS . FOLDED), newest-first.
BUCKET is the churn bucket size in seconds."
  (if (fboundp 'beads-event-fold)
      (beads-event-fold records bucket)
    (beads-events--fallback-fold records bucket)))

;;; Filtering

(defun beads-events--level-rank (level)
  "Return the severity rank of signal LEVEL: 2 attention, 1 watch, 0."
  (pcase level ('attention 2) ('watch 1) (_ 0)))

(defun beads-events--window ()
  "Return the view's effective window."
  (or (plist-get beads-events--filter :window) beads-events-window))

(defun beads-events--match-p (record filter)
  "Return non-nil when RECORD passes the client-side part of FILTER."
  (let ((op (plist-get filter :op))
        (actor (plist-get filter :actor))
        (issue (plist-get filter :issue))
        (level (plist-get filter :level))
        (search (plist-get filter :search)))
    (and (or (null op)
             (let ((case-fold-search t))
               (string-match-p (regexp-quote op) (or (oref record op) ""))))
         (or (null actor)
             (let ((case-fold-search t))
               (string-match-p (regexp-quote actor)
                               (beads-events--actor record))))
         (or (null issue) (equal issue (oref record issue-id)))
         (or (null level)
             (>= (beads-events--level-rank (beads-events--level record))
                 (beads-events--level-rank level)))
         (or (null search)
             (let ((case-fold-search t))
               (string-match-p
                (regexp-quote search)
                (mapconcat (lambda (s) (or s ""))
                           (list (oref record op)
                                 (beads-events--subject record)
                                 (beads-events--actor record))
                           " ")))))))

(defun beads-events--in-window-p (record window)
  "Return non-nil when RECORD falls inside WINDOW."
  (let ((at (beads-events--time record)))
    (or (<= at 0)
        (<= (- (float-time) at) (beads-events--window-seconds window)))))

(defun beads-events--filtered-records (records filter)
  "Return RECORDS passing FILTER, newest-first by wall clock."
  (let ((kept (seq-filter (lambda (record) (beads-events--match-p record filter))
                          records)))
    (when-let* ((window (plist-get filter :window)))
      (setq kept (seq-filter (lambda (record)
                               (beads-events--in-window-p record window))
                             kept)))
    (sort (copy-sequence kept)
          (lambda (a b) (> (beads-events--time a) (beads-events--time b))))))

(defun beads-events--rows (records filter)
  "Return (ROWS SHOWN . FOLDED) for RECORDS under FILTER.
ROWS are `beads-events--fold' rows, newest-first."
  (let* ((kept (beads-events--filtered-records records filter))
         (bucket (or (plist-get filter :bucket) beads-events-fold-bucket))
         (folded (beads-events--fold kept bucket)))
    (list (car folded) (length kept) (cdr folded))))

;;; Record sources

(defun beads-events--model-records (stream)
  "Return STREAM's model ring, newest-first, or nil.
A guarded read: the ring accessor is private to `beads-live'."
  (when (and stream
             (fboundp 'beads-live-model)
             (fboundp 'beads-live--model-ring))
    (condition-case nil
        (when-let* ((model (beads-live-model stream)))
          (append (beads-live--model-ring model) nil))
      (error nil))))

(defun beads-events--read-store-records (store)
  "Return STORE's records via `beads-events-records-function', or nil."
  (when (functionp beads-events-records-function)
    (condition-case nil (funcall beads-events-records-function store)
      (error nil))))

(defun beads-events--source-records ()
  "Return this buffer's unfiltered records, newest-first.
The live model wins when a stream is attached; otherwise the
buffer-local list is the source."
  (or (beads-events--model-records beads-events--stream)
      beads-events--records))

(defun beads-events--merge-stores (stores)
  "Merge STORES, an alist of (STORE . RECORDS), newest-first.
Return (RECORDS . STORE-OF), STORE-OF an eq hash mapping each record to
its store.  Ordering is by wall clock, never by seq, so per-store
sequence spaces are never mixed."
  (let ((store-of (make-hash-table :test 'eq))
        (all nil))
    (dolist (pair stores)
      (let ((store (car pair)))
        (dolist (record (cdr pair))
          (puthash record store store-of)
          (push record all))))
    (cons (sort all (lambda (a b) (> (beads-events--time a)
                                     (beads-events--time b))))
          store-of)))

;;; Rendering

(defun beads-events--time-cell (time now)
  "Return the Time cell of TIME (a float): `HH:MM' today, else with date.
NOW is the current time."
  (let ((ts (format-time-string "%FT%T%z" time)))
    (propertize (if (equal (format-time-string "%F" time)
                           (format-time-string "%F" now))
                    (format-time-string "%H:%M:%S" time)
                  (format-time-string "%b %e %H:%M" time))
                'help-echo ts)))

(defun beads-events--changed-p (record)
  "Return non-nil when RECORD falls inside the live change window."
  (let* ((window (if (boundp 'beads-live-change-window)
                     beads-live-change-window
                   30))
         (seconds (beads-events--window-seconds window))
         (at (beads-events--time record)))
    (and (> at 0) (<= (- (float-time) at) seconds))))

(defun beads-events--issue-cell (record)
  "Return RECORD's Issue cell: its id, plus a dim title when known."
  (let ((id (or (oref record issue-id) ""))
        (issue (oref record issue)))
    (if (and issue (oref issue title))
        (concat (propertize id 'face 'beads-face-id)
                " " (propertize (oref issue title) 'face 'shadow))
      id)))

(defun beads-events--actor-cell (record)
  "Return RECORD's Actor cell."
  (let ((actor (beads-events--actor record)))
    (if (equal actor "system")
        (propertize actor 'face 'shadow)
      (format "@%s" actor))))

(defun beads-events--session-cell (record)
  "Return RECORD's Session cell from the optional resolver seam, or nil."
  (let ((tag (beads-events-session-tag
              (or (and beads-events--store-of
                       (gethash record beads-events--store-of))
                  beads-events--store)
              (beads-events--actor record))))
    (and tag (format "@%s" tag))))

(defun beads-events--record-entry (record now &optional child)
  "Return the tabulated entry of RECORD at NOW.
CHILD indents an unfolded churn member's op."
  (let* ((glyph (beads-events--glyph record))
         (changed (beads-events--changed-p record))
         (columns
          (list (beads-events--time-cell (beads-events--time record) now)
                (if changed
                    (propertize "◈" 'face 'beads-event-changed)
                  glyph)
                (propertize (if child
                                (concat "  " (or (oref record op) ""))
                              (or (oref record op) ""))
                            'face (if child 'shadow
                                    (and (beads-events--level record) 'bold)))
                (beads-events--issue-cell record)
                (beads-events--actor-cell record))))
    (when (and beads-events--city beads-events--store-of)
      (push (or (gethash record beads-events--store-of) "") columns))
    (when (and beads-events--city beads-events--has-session)
      (setq columns (append columns (list (beads-events--session-cell record)))))
    (list record (vconcat columns))))

(defun beads-events--churn-entry (row now)
  "Return the tabulated entry of churn ROW at NOW.
ROW is (churn KEY GROUP TIME RECORDS)."
  (pcase-let ((`(churn ,key ,group ,time ,records) row))
    (let* ((expanded (member key beads-events--expanded))
           (columns
            (list (beads-events--time-cell time now)
                  (if expanded "▾" "▸")
                  (propertize (format "%s ×%d" group (length records))
                              'face 'shadow)
                  (propertize (beads-events--actor-cell (car records))
                              'face 'shadow)
                  "")))
      (when (and beads-events--city beads-events--store-of)
        (push (or (gethash (car records) beads-events--store-of) "") columns))
      (when (and beads-events--city beads-events--has-session)
        (setq columns (append columns
                              (list (beads-events--session-cell (car records))))))
      (list row (vconcat columns)))))

(defun beads-events--entries (rows now)
  "Return the tabulated entries of ROWS at NOW, unfolding expanded churn."
  (mapcan (lambda (row)
            (if (eq (car row) 'churn)
                (cons (beads-events--churn-entry row now)
                      (and (member (nth 1 row) beads-events--expanded)
                           (mapcar (lambda (record)
                                     (beads-events--record-entry record now t))
                                   (nth 4 row))))
              (list (beads-events--record-entry (nth 1 row) now))))
          rows))

(defun beads-events--format ()
  "Return the `tabulated-list-format' for this buffer."
  (let ((base
         (if beads-events--city
             (list (list "Store" 10 nil)
                   (list "Time" 9 nil)
                   (list "Sig" 3 nil)
                   (list "Op" 11 nil)
                   (list "Issue" 24 nil)
                   (list "Actor" 16 nil))
           (list (list "Time" 9 nil)
                 (list "Sig" 3 nil)
                 (list "Op" 11 nil)
                 (list "Issue" 30 nil)
                 (list "Actor" 16 nil)))))
    (when (and beads-events--city beads-events--has-session)
      (setq base (append base (list (list "Session" 16 nil)))))
    (vconcat base)))

(defun beads-events--sync-format ()
  "Recompute `tabulated-list-format' and the header when columns changed."
  (let ((want (beads-events--format)))
    (unless (equal want tabulated-list-format)
      (setq tabulated-list-format want)
      (tabulated-list-init-header))))

(defun beads-events--has-session-p (records)
  "Return non-nil when any of RECORDS has a session tag."
  (and beads-events--city
       beads-events--store-of
       (seq-some (lambda (record)
                   (beads-events-session-tag
                    (gethash record beads-events--store-of)
                    (beads-events--actor record)))
                 records)))

(defun beads-events--render ()
  "Re-render the rows from the records in hand."
  (let* ((source (beads-events--source-records))
         (model (beads-events--rows source beads-events--filter))
         (rows (nth 0 model)))
    (setq beads-events--shown (nth 1 model)
          beads-events--folded (nth 2 model)
          beads-events--has-session
          (beads-events--has-session-p
           (or source beads-events--records)))
    (beads-events--sync-format)
    (beads-pager-set-entries (beads-events--entries rows (float-time)))
    (cl-incf beads-events--generation)
    (force-mode-line-update)))

;;; Header line

(defun beads-events--filter-text ()
  "Return the active filters as `key=value' words, or nil."
  (let ((parts (cl-loop for (key value) on beads-events--filter by #'cddr
                        unless (or (null value) (memq key '(:window :bucket)))
                        collect (format "%s=%s"
                                        (substring (symbol-name key) 1)
                                        value))))
    (and parts (string-join parts " "))))

(defun beads-events--live-string ()
  "Return the live status chip for this buffer's store, or nil."
  (when (fboundp 'beads-live-header-string)
    (condition-case nil
        (beads-live-header-string (or beads-events--store
                                      default-directory))
      (error nil))))

(defun beads-events--header-line ()
  "Return the Events header line: name, window, count, filters, chip.
Pure over buffer-local state, so it is safe in `:eval'."
  (let* ((name (or beads-events--store-name
                   (and beads-events--store
                        (file-name-nondirectory
                         (directory-file-name beads-events--store)))
                   "beads"))
         (kind (if beads-events--city "city events" "events"))
         (filters (beads-events--filter-text))
         (live (beads-events--live-string))
         (right (concat (if (> beads-events--folded 0)
                            (propertize
                             (format "(%d churn folded)" beads-events--folded)
                             'face 'shadow)
                          "")
                        (if live (concat "  " live) ""))))
    (concat " " (propertize name 'face 'beads-face-header)
            " " (propertize kind 'face 'beads-face-header)
            "  " (propertize (format "last %s · " (beads-events--window))
                             'face 'shadow)
            (number-to-string beads-events--shown)
            (propertize
             (format " · signal ≥ %s"
                     (or (plist-get beads-events--filter :level) "event"))
             'face 'shadow)
            (if filters (concat "  " (propertize filters 'face 'transient-value))
              "")
            (propertize " " 'display
                        `(space :align-to (- right ,(1+ (string-width right)))))
            right)))

;;; Live integration

(defun beads-events--live-refresh ()
  "Re-render the current Events buffer after a debounced live batch."
  (when (derived-mode-p 'beads-events-mode)
    (beads-events-refresh)))

(defun beads-events--schedule-flush ()
  "Schedule one debounced re-render for the current buffer."
  (unless (timerp beads-events--flush-timer)
    (setq beads-events--flush-timer
          (run-at-time beads-events-append-delay nil
                       #'beads-events--flush (current-buffer)))))

(defun beads-events--flush (buffer)
  "Fold BUFFER's queued live records into its record list and re-render."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (timerp beads-events--flush-timer)
        (cancel-timer beads-events--flush-timer))
      (setq beads-events--flush-timer nil)
      (dolist (record (nreverse beads-events--queue))
        (unless (seq-some (lambda (other)
                            (and (equal (oref other issue-id)
                                        (oref record issue-id))
                                 (equal (oref other seq) (oref record seq))))
                          beads-events--records)
          (push record beads-events--records)))
      (setq beads-events--queue nil)
      (beads-events-refresh))))

(defun beads-events--live-record (record)
  "Queue one raw journal RECORD for the current Events buffer.
Runs with the Events buffer current (a `beads-live-subscribe' callback)."
  (when (derived-mode-p 'beads-events-mode)
    (push record beads-events--queue)
    (beads-events--schedule-flush)))

(defun beads-events--live-attach ()
  "Attach the current buffer to its store's live journal stream.
A no-op when `beads-events--live-inhibit' is non-nil or the live layer
is not loaded."
  (when (and (not beads-events--live-inhibit)
             (require 'beads-live nil t))
    (setq beads-events--stream
          (beads-live-attach (current-buffer)
                             :refresh #'beads-events--live-refresh
                             :kinds '(events)))
    (when (and (fboundp 'beads-live-subscribe) (null beads-events--handle))
      (setq beads-events--handle
            (beads-live-subscribe #'beads-events--live-record
                                  (current-buffer)))))
  beads-events--stream)

(defun beads-events--teardown ()
  "Detach this buffer's live subscription and stop its debounce timer."
  (when (timerp beads-events--flush-timer)
    (cancel-timer beads-events--flush-timer)
    (setq beads-events--flush-timer nil))
  (when (and beads-events--handle (fboundp 'beads-live-unsubscribe))
    (ignore-errors (beads-live-unsubscribe beads-events--handle)))
  (setq beads-events--handle nil)
  (when (and beads-events--stream (fboundp 'beads-live-detach))
    (ignore-errors (beads-live-detach (current-buffer))))
  (setq beads-events--stream nil))

;;; Commands

(defun beads-events-refresh ()
  "Refresh this Events view from its live source, then re-render.
Interactively (`g'), a live stream is re-read; with no stream the
buffer-local records are re-rendered."
  (interactive)
  (unless (derived-mode-p 'beads-events-mode)
    (user-error "Not in a beads events buffer"))
  (beads-events--render))

(defun beads-events--prompt (label current)
  "Prompt for LABEL, defaulting to CURRENT; empty clears the filter."
  (let ((answer (read-string (format "%s (regexp, empty clears): " label)
                             (and current (format "%s" current)))))
    (unless (string-empty-p answer) answer)))

(defun beads-events-filter (&optional filter)
  "Set this view's filter, then refresh.
Interactively prompts for op, actor and issue; FILTER, from Lisp,
replaces the filter plist entirely (see `beads-events--filter')."
  (interactive)
  (unless (derived-mode-p 'beads-events-mode)
    (user-error "Not in a beads events buffer"))
  (setq beads-events--filter
        (or filter
            (let* ((old beads-events--filter)
                   (op (beads-events--prompt "Op" (plist-get old :op)))
                   (actor (beads-events--prompt "Actor" (plist-get old :actor)))
                   (issue (beads-events--prompt "Issue" (plist-get old :issue)))
                   (level (beads-events--prompt "Signal level"
                                                (plist-get old :level))))
              (let (plist)
                (when op (setq plist (plist-put plist :op op)))
                (when actor (setq plist (plist-put plist :actor actor)))
                (when issue (setq plist (plist-put plist :issue issue)))
                (when level
                  (setq plist (plist-put plist :level (intern level))))
                plist))))
  (beads-events-refresh))

(defun beads-events-clear-filter ()
  "Clear this view's filters and refresh."
  (interactive)
  (setq beads-events--filter nil)
  (beads-events-refresh))

(defun beads-events--toggle-churn ()
  "Fold or unfold the churn row at point."
  (interactive)
  (let ((row (tabulated-list-get-id)))
    (when (and (consp row) (eq (car row) 'churn))
      (let ((key (nth 1 row)))
        (if (member key beads-events--expanded)
            (setq beads-events--expanded (delete key beads-events--expanded))
          (push key beads-events--expanded))
        (beads-events--render)))))

(defun beads-events--record-at-point ()
  "Return the record at point, or nil.
For a folded churn row this is the newest member."
  (let ((id (tabulated-list-get-id)))
    (cond
     ((beads-event-record-p id) id)
     ((and (consp id) (eq (car id) 'churn)) (car (nth 4 id))))))

(defun beads-events-toggle-live ()
  "Toggle the live stream for this view's store (the `W' control).
A friendly error when the `beads-live' layer is not loaded, so the
standalone Events view never signals a void-function."
  (interactive)
  (require 'beads-live nil t)
  (if (fboundp 'beads-live-toggle)
      (beads-live-toggle beads-events--store)
    (user-error "The beads-live layer is not available")))

(defun beads-events-visit ()
  "Visit the issue of the record at point in a show buffer."
  (interactive)
  (let ((record (beads-events--record-at-point)))
    (unless record (user-error "No event at point"))
    (let ((id (oref record issue-id)))
      (unless id (user-error "This event has no issue"))
      (require 'beads-command-show)
      (beads-show id))))

;;; Org export

(defun beads-events--org-string (records &optional title)
  "Return RECORDS (newest-first) as an Org table.
TITLE is the table's `#+TITLE'."
  (concat
   (format "#+TITLE: %s\n\n" (or title "beads events"))
   "| Seq | Time | Op | Issue | Actor |\n"
   "|-----+------+----+-------+-------|\n"
   (mapconcat
    (lambda (record)
      (format "| %s | %s | %s | %s | %s |"
              (or (oref record seq) "")
              (let ((at (beads-events--time record)))
                (if (> at 0) (format-time-string "%F %T" at) ""))
              (or (oref record op) "")
              (or (oref record issue-id) "")
              (beads-events--actor record)))
    records "\n")
   "\n"))

;;;###autoload
(defun beads-events-org-export ()
  "Export this view's filtered records to an Org buffer."
  (interactive)
  (unless (derived-mode-p 'beads-events-mode)
    (user-error "Not in a beads events buffer"))
  (let* ((records (beads-events--filtered-records
                   (beads-events--source-records) beads-events--filter))
         (text (beads-events--org-string
                records (format "%s events" (or beads-events--store-name
                                                "beads"))))
         (buffer (get-buffer-create
                  (format "*beads-events-org[%s]*"
                          (or beads-events--store-name "beads")))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert text)
        (goto-char (point-min))
        (when (require 'org nil t)
          (delay-mode-hooks (org-mode)))))
    (pop-to-buffer buffer)))

;;; Major mode

(defvar beads-events-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (keymap-set map "g" #'beads-events-refresh)
    (keymap-set map "f" #'beads-events-filter)
    (keymap-set map "x" #'beads-events-clear-filter)
    (keymap-set map "e" #'beads-events-org-export)
    (keymap-set map "RET" #'beads-events-visit)
    (keymap-set map "W" #'beads-events-toggle-live)
    (keymap-set map "]" #'beads-pager-next-page)
    (keymap-set map "[" #'beads-pager-prev-page)
    (beads-mode--install-navigation-keys map)
    map)
  "Keymap for `beads-events-mode'.")

(define-derived-mode beads-events-mode tabulated-list-mode "Beads-Events"
  "Major mode of the Events views: the `bd events' journal.
\\{beads-events-mode-map}"
  (setq tabulated-list-format (beads-events--format))
  (setq tabulated-list-padding 1)
  (setq tabulated-list-sort-key nil)
  ;; The header line carries the view summary; the column names are not
  ;; a second header row.
  (setq tabulated-list-use-header-line nil)
  (tabulated-list-init-header)
  (hl-line-mode 1)
  (beads-pager-mode 1)
  (setq header-line-format '(:eval (beads-events--header-line)))
  (add-hook 'beads-thing-toggle-functions #'beads-events--toggle-churn nil t)
  (add-hook 'kill-buffer-hook #'beads-events--teardown nil t))

;;; Entry points

(defun beads-events--store-name (store)
  "Return a display name for STORE."
  (when (and store (stringp store) (not (string-empty-p store)))
    (file-name-nondirectory (directory-file-name store))))

(defun beads-events--buffer-name (store city)
  "Return the buffer name for the Events view of STORE or CITY."
  (if city
      (format "*beads-events-city[%s]*" city)
    (format "*beads-events[%s]*" (or (beads-events--store-name store)
                                     "beads"))))

(defun beads-events--configure (store city)
  "Set this buffer's view state for STORE or CITY."
  (setq beads-events--store store
        beads-events--city city
        beads-events--store-name (if city
                                     city
                                   (beads-events--store-name store))))

;;;###autoload
(defun beads-events-timeline (&optional directory filter)
  "Show the `bd events' journal of the store at DIRECTORY, newest-first.
Reuses one buffer per store.  FILTER, from Lisp, replaces the view's
filter plist.  When the live layer is loaded the view attaches to the
store's stream and follows it; otherwise it renders the injected
records (`beads-events--records')."
  (interactive)
  (let* ((store (or (beads-store-resolve directory) beads-store-directory))
         (buffer (get-buffer-create (beads-events--buffer-name store nil))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'beads-events-mode)
        (beads-events-mode))
      (beads-events--configure store nil)
      (when filter (setq beads-events--filter filter))
      (beads-events--sync-format)
      (beads-events--live-attach)
      (beads-events-refresh))
    (pop-to-buffer buffer)))

;;;###autoload
(defun beads-events-timeline-city (&optional city)
  "Show a merged timeline across every registered beads store.
CITY names the view's buffer and header.  Stores come from
`beads-events-city-stores-function' (an alist of
\(STORE . RECORDS)), else `beads-events-city-stores', else the current
buffer's store.  Records are ordered by wall clock; per-store sequence
spaces are never mixed.  A session tag is shown only when the optional
`beads-event-session-resolver' / `beads-events-session-table' seam
supplies one."
  (interactive)
  (let* ((pairs (cond
                 ((functionp beads-events-city-stores-function)
                  (funcall beads-events-city-stores-function))
                 ((consp beads-events-city-stores) beads-events-city-stores)
                 (beads-events--store
                  (list (cons beads-events--store
                              (beads-events--read-store-records
                               beads-events--store))))
                 (t nil)))
         (merged (beads-events--merge-stores pairs))
         (buffer (get-buffer-create (beads-events--buffer-name nil city))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'beads-events-mode)
        (beads-events-mode))
      (beads-events--configure nil (or city "city"))
      (setq beads-events--records (car merged)
            beads-events--store-of (cdr merged)
            beads-events--filter nil)
      (beads-events--sync-format)
      (beads-events-refresh))
    (pop-to-buffer buffer)))

(provide 'beads-events)
;;; beads-events.el ends here
