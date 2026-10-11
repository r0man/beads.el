;;; beads-event.el --- Pure journal event model for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; What every view that shows `bd events' agrees on, as pure functions
;; over `beads-event-record' and `beads-issue' with no buffers and no
;; processes (plan `plans/beads-events-live/', WI-LIVE-01).  It is safe
;; to require from the views and the mode-line pulse without pulling in
;; the stream.
;;
;; `bd' 1.3 journals exactly seven ops -- `create', `update', `close',
;; `delete', `dep_add', `dep_remove' and `comment'.  Claim, reopen,
;; status, assignee and label mutations all arrive as a single `update',
;; so the finer signal level keys off the *field diff*, never off a
;; distinct op (`beads-event-level', `beads-event-diff').  The audit
;; vocabulary in `beads-types' (`beads-event-created', `-updated', ...)
;; is a different contract and is deliberately not reused here.
;;
;; The `issue' object on a record is a sparse, `omitempty' partial: an
;; absent key means its zero value (the field was cleared), not
;; "unchanged".  `beads-event-diff' therefore reads an absent field as
;; nil and only diffs the fixed wire subset; fields the journal never
;; carries (description, dependencies, comments, counts, revision) are
;; left alone.  Dependency changes come from the `dep' payload, not the
;; snapshot.
;;
;; Actor is the journal's only provenance: a bare, opaque string that
;; may be absent on a derived cascade row.  `beads-event-actor-description'
;; is the single presentation hook; it renders an absent actor as
;; `system' and never invents a session or user.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'beads-custom)
(require 'beads-types)

;;; Journal ops

(defconst beads-event-ops
  '("create" "update" "close" "delete" "dep_add" "dep_remove" "comment")
  "The seven journal ops `bd events' emits, in bd's documented order.
The list is data, not a control path: `beads-event-level' maps ops
through the user-extensible `beads-event-levels'.")

;;; Signal levels

(defvar beads-event--level-memo nil
  "Memo of `beads-event--op-level': (TABLE . HASH).
HASH maps an op string to its level (or the `none' sentinel); it is
valid while TABLE is still the value of `beads-event-levels'.")

(defcustom beads-event-levels
  '(("\\`close\\'" . attention)
    ("\\`delete\\'" . attention)
    ("\\`dep_add\\'" . attention)
    ("\\`update\\'" . watch))
  "Signal level of a journal op: (REGEXP . LEVEL), first match wins.
LEVEL is `attention' or `watch'; an op no entry matches is a plain
event, and a plain `update' whose diff is all bookkeeping drops back
to plain (`beads-event-noise-fields').  A signal event is never folded
into a churn row (`beads-event-fold')."
  :type '(alist :key-type regexp
                :value-type (choice (const attention) (const watch)))
  :group 'beads)

(defcustom beads-event-noise-fields
  '(updated-at heartbeat-at lease-expires-at revision)
  "`update' fields that alone are bookkeeping, not signal.
A diff whose changed fields are all in this list renders as a plain
event; any other field makes the update a `watch'."
  :type '(repeat symbol)
  :group 'beads)

(defcustom beads-event-fold-bucket 900
  "Seconds per churn bucket in `beads-event-fold' (default 15 minutes).
Non-signal records with the same issue and op inside one bucket
coalesce into a single `×N' row."
  :type 'integer
  :group 'beads)

(defconst beads-event-glyphs
  '((attention . "■")
    (watch . "▲"))
  "Glyphs for the journal signal levels.
`attention' renders ■, `watch' ▲; a plain event renders a space.")

(defun beads-event-record-p (object)
  "Return non-nil when OBJECT is a `beads-event-record'."
  (and (fboundp 'cl-typep) (cl-typep object 'beads-event-record)))

(defun beads-event--op-level (op)
  "Return OP's configured signal level through `beads-event-levels', or nil.
Matched once per op (there are seven) and memoized until
`beads-event-levels' changes."
  (let ((op (or op "")))
    (unless (eq (car beads-event--level-memo) beads-event-levels)
      (setq beads-event--level-memo
            (cons beads-event-levels (make-hash-table :test 'equal))))
    (let* ((memo (cdr beads-event--level-memo))
           (level (gethash op memo)))
      (if level
          (and (not (eq level 'none)) level)
        (setq level (cdr (seq-find (lambda (entry)
                                     (string-match-p (car entry) op))
                                   beads-event-levels)))
        (puthash op (or level 'none) memo)
        level))))

(defun beads-event--significant-diff-p (diff)
  "Return non-nil when DIFF touches a field outside `beads-event-noise-fields'."
  (seq-some (lambda (change)
              (let ((field (plist-get change :field)))
                (and field (not (memq field beads-event-noise-fields)))))
            diff))

(defun beads-event-level (record &optional diff)
  "Return RECORD's signal level: `attention', `watch', or nil.
DIFF, when non-nil, is RECORD's field diff (`beads-event-diff'); an
`update' whose changed fields are all bookkeeping
\(`beads-event-noise-fields') drops to a plain event.  The finer level
for claim/reopen/status/label keys off the diff because all of them
arrive as the single `update' op."
  (let ((level (beads-event--op-level (oref record op))))
    (if (and diff
             (equal (oref record op) "update")
             (not (beads-event--significant-diff-p diff)))
        nil
      level)))

(defun beads-event-glyph (thing)
  "Return the glyph of THING: a level symbol or an event record.
`attention' renders ■, `watch' ▲, and a plain event a space."
  (let ((level (if (beads-event-record-p thing)
                   (beads-event-level thing)
                 thing)))
    (or (cdr (assq level beads-event-glyphs)) " ")))

;;; Time

(defun beads-event-time (record)
  "Return RECORD's timestamp as a float, or 0 when absent/unparseable.
`record.ts' mixes zones (RFC3339 with `Z' or an offset), so parse it
to an absolute time; never compare the strings."
  (let ((ts (oref record ts)))
    (if (and (stringp ts) (not (string-empty-p ts)))
        (condition-case nil
            (float-time (date-to-time ts))
          (error 0))
      0)))

(defun beads-event-window-seconds (window)
  "Return WINDOW as a number of seconds.
A duration such as \"30s\", \"15m\", \"2h\", \"7d\" or \"2w\"; a bare
number (or an all-digit string) is already seconds."
  (cond
   ((numberp window) window)
   ((and (stringp window) (string-match-p "\\`[0-9]+\\'" window))
    (string-to-number window))
   ((and (stringp window)
         (string-match "\\`\\([0-9]+\\)\\([smhdw]\\)\\'" window))
    (* (string-to-number (match-string 1 window))
       (pcase (match-string 2 window)
         ("s" 1)
         ("m" 60)
         ("h" 3600)
         ("d" 86400)
         ("w" 604800))))
   (t (user-error "Invalid beads event window: %S" window))))

(defun beads-event-since-arg (window &optional records now)
  "Return a `bd events --since' sequence number for WINDOW.
WINDOW is a duration string (`2h', `7d', `2w') or an explicit seq
\(integer or all-digit string, returned unchanged as an integer; bd's
`--since' is strictly greater than a seq, not a time).

With RECORDS, resolve a duration to the largest seq strictly older
than the window's cutoff, so `--since' yields the records inside the
window; 0 when no record is older.  NOW defaults to the current time;
a duration without RECORDS resolves to 0, which reads from the start."
  (cond
   ((integerp window) window)
   ((and (stringp window) (string-match-p "\\`[0-9]+\\'" window))
    (string-to-number window))
   (t
    (let* ((seconds (beads-event-window-seconds window))
           (cutoff (- (or now (float-time)) seconds))
           (best 0))
      (dolist (record records)
        (let ((seq (oref record seq)))
          (when (and (< (beads-event-time record) cutoff)
                     (> seq best))
            (setq best seq))))
      best))))

;;; Rows

(defun beads-event-actor-description (record)
  "Return RECORD's actor for display, or \"system\" when absent.
The actor is the only provenance the journal carries: an opaque label,
never a session or user id.  A derived (actor-less) row renders as
`system'; this is the single presentation hook views override."
  (let ((actor (oref record actor)))
    (if (and (stringp actor) (not (string-empty-p actor)))
        actor
      "system")))

(defun beads-event-subject (record)
  "Return RECORD's row text: \"issue-id  title — op\".
The title comes from RECORD's issue snapshot; a `delete' (null
snapshot) renders the id with no title, and a missing id falls back to
the op alone."
  (let* ((id (or (oref record issue-id) ""))
         (issue (oref record issue))
         (title (or (and issue (oref issue title)) ""))
         (op (or (oref record op) "")))
    (cond
     ((and (> (length id) 0) (> (length title) 0))
      (format "%s  %s — %s" id title op))
     ((> (length id) 0) (format "%s — %s" id op))
     (t op))))

;;; Field diff

(defconst beads-event-diff-fields
  '(id title status priority issue-type owner created-by created-at
    updated-at is-blocked assignee labels started-at lease-expires-at
    heartbeat-at closed-at close-reason)
  "The wire field subset a journal `issue' snapshot may carry.
Fields outside this set (description, dependencies, comments, counts,
revision) are never in a snapshot, so a diff must not report their
absence as a change.  `is-blocked' is not a `beads-issue' slot today;
it is diffed when the snapshot is a raw JSON alist, which the stream
keeps under the sparse-merge contract.")

(defun beads-event--field-json-key (field)
  "Return FIELD's JSON key (hyphens become underscores)."
  (intern (replace-regexp-in-string "-" "_" (symbol-name field))))

(defun beads-event--issue-value (issue field)
  "Return FIELD's value in ISSUE, a `beads-issue' or a raw JSON alist.
ISSUE may be nil.  An absent key/slot reads as nil, which is the
journal's sparse `omitempty' contract: absent means the field's zero
value (cleared), not unchanged."
  (cond
   ((null issue) nil)
   ((consp issue)
    (cdr (assq (beads-event--field-json-key field) issue)))
   ((and (fboundp 'cl-typep) (cl-typep issue 'beads-issue))
    ;; `slot-name' is dynamic, so use `slot-value' (not the `oref'
    ;; macro, whose slot is literal and would read a slot named
    ;; `field').
    (when (slot-exists-p issue field)
      (slot-value issue field)))
   (t nil)))

(defun beads-event--label-list (value)
  "Return VALUE (a label list or JSON vector) as a plain list of strings."
  (cond
   ((null value) nil)
   ((vectorp value) (append value nil))
   ((listp value) value)
   (t (list value))))

(defun beads-event--label-changes (old new)
  "Return label change entries between label lists OLD and NEW."
  (let* ((old (delete-dups (beads-event--label-list old)))
         (new (delete-dups (beads-event--label-list new)))
         (added (seq-difference new old #'equal))
         (removed (seq-difference old new #'equal))
         changes)
    (dolist (label added)
      (push (list :field 'labels :kind 'label-added :old nil :new label)
            changes))
    (dolist (label removed)
      (push (list :field 'labels :kind 'label-removed :old label :new nil)
            changes))
    (nreverse changes)))

(defun beads-event-diff (old new)
  "Return the field changes between issue snapshots OLD and NEW.
OLD and NEW are `beads-issue' objects or raw JSON alists; either may
be nil, since a `create' has no OLD and a `delete' has no NEW.  Each
change is a plist (:field FIELD :kind KIND :old OLDVALUE :new NEWVALUE)
where KIND is `set' for a scalar, `label-added'/`label-removed' for a
label and `deleted' for a tombstone.  Absent fields read as nil under
the journal's sparse `omitempty' contract.  Dependency fields are not
diffed here, because the journal never carries them; use
`beads-event-record-diff' for the `dep_add'/`dep_remove' payload."
  (cond
   ((and old (null new))
    (list (list :field 'deleted :kind 'deleted
                :old (beads-event--issue-value old 'id) :new nil)))
   ((and (null old) (null new)) nil)
   (t
    (let (changes)
      (dolist (field beads-event-diff-fields)
        (if (eq field 'labels)
            (setq changes
                  (append (beads-event--label-changes
                           (beads-event--issue-value old 'labels)
                           (beads-event--issue-value new 'labels))
                          changes))
          (let ((old-value (beads-event--issue-value old field))
                (new-value (beads-event--issue-value new field)))
            (unless (equal old-value new-value)
              (push (list :field field :kind 'set
                          :old old-value :new new-value)
                    changes)))))
      (nreverse changes)))))

(defun beads-event--dep-target (record)
  "Return RECORD's dependency payload target, or nil."
  (let ((dep (oref record dep)))
    (and (listp dep) (cdr (assq 'target dep)))))

(defun beads-event-record-diff (record &optional previous)
  "Return the changes RECORD carries, against PREVIOUS.
PREVIOUS is the issue snapshot before RECORD.  `dep_add' and
`dep_remove' render their `dep' payload (the journal snapshot never
carries dependencies), `delete' renders a tombstone, and `comment' has
no field change; every other op falls through to `beads-event-diff' of
PREVIOUS and the record's snapshot."
  (pcase (oref record op)
    ((or "dep_add" "dep_remove")
     (let* ((target (beads-event--dep-target record))
            (added (equal (oref record op) "dep_add")))
       (list (list :field 'dependencies
                   :kind (if added 'dependency-added 'dependency-removed)
                   :old (unless added target)
                   :new (when added target)))))
    ("delete"
     (list (list :field 'deleted :kind 'deleted
                 :old (oref record issue-id) :new nil)))
    ("comment" nil)
    ("create"
     (list (list :field 'created :kind 'created :old nil
                 :new (or (oref record issue-id) ""))))
    (_ (beads-event-diff previous (oref record issue)))))

;;; Diff presentation

(defun beads-event-field-label (field)
  "Return FIELD's short human label for a diff row."
  (pcase field
    ('issue-type "type")
    ('is-blocked "blocked")
    ('close-reason "close reason")
    ('lease-expires-at "lease")
    ('heartbeat-at "heartbeat")
    ('started-at "started")
    ('created-at "created")
    ('updated-at "updated")
    ('closed-at "closed")
    ('dependencies "dependency")
    ('created-by "created by")
    ('deleted "deleted")
    ('created "create")
    (_ (replace-regexp-in-string "-" " " (symbol-name field)))))

(defun beads-event-value-string (value)
  "Return VALUE as a short display string; nil renders as an em dash."
  (cond
   ((null value) "—")
   ((eq value t) "true")
   ((eq value :json-false) "false")
   ((numberp value) (number-to-string value))
   ((stringp value) (if (string-empty-p value) "—" value))
   ((vectorp value)
    (mapconcat #'beads-event-value-string (append value nil) ", "))
   (t (format "%s" value))))

(defun beads-event-diff-string (change)
  "Return CHANGE (a `beads-event-diff' entry) as one display line.
Scalars render as \"field old → new\", dependency entries as
\"dependency_added target\", and label entries as \"label \\\"x\\\"\"."
  (let ((field (beads-event-field-label (plist-get change :field)))
        (old (beads-event-value-string (plist-get change :old)))
        (new (beads-event-value-string (plist-get change :new))))
    (pcase (plist-get change :kind)
      ('dependency-added (format "dependency_added %s" new))
      ('dependency-removed (format "dependency_removed %s" old))
      ('deleted (format "deleted %s" old))
      ('created (format "create %s" new))
      ('label-added (format "label \"%s\"" new))
      ('label-removed (format "label \"%s\"" old))
      (_ (format "%s %s → %s" field old new)))))

;;; Churn folding

(defun beads-event-fold (records &optional bucket)
  "Fold RECORDS into rows, newest first.
Non-signal records with the same issue and op inside one BUCKET
\(default `beads-event-fold-bucket') coalesce into a single churn row;
signal records (`beads-event-level') never fold.  Returns (ROWS
. FOLDED): a row is (event RECORD) or
\(churn KEY GROUP TIME RECORDS) with RECORDS newest first, and FOLDED
counts the records that went into churn rows."
  (let ((bucket (or bucket beads-event-fold-bucket))
        (buckets (make-hash-table :test 'equal))
        (rows nil)
        (folded 0))
    (dolist (record records)
      (if (beads-event-level record)
          (push (list 'event record) rows)
        (setq folded (1+ folded))
        (let* ((time (beads-event-time record))
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
                                          (beads-event-time (nth 1 row)))
                                        row))
                                rows)
                        (lambda (a b) (> (car a) (car b)))))
          folded)))

(provide 'beads-event)
;;; beads-event.el ends here
