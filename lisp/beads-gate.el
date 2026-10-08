;;; beads-gate.el --- Gate list, detail, and actions -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Gates are bd's async wait conditions (`bd gate').  A gate is an
;; issue of type `gate' that blocks another issue until it resolves:
;; `human' waits for a manual resolve, `timer' expires after a
;; timeout, `gh:run' waits for a GitHub Actions run, `gh:pr' waits for
;; a pull request merge, and `bead' waits for another bead to close.
;; This module owns the gate porcelain: the tabulated list, the
;; sectioned detail view, and the create/check/resolve/add-waiter/
;; discover actions.
;;
;; List (REQ-SF-030): `beads-gate-list' opens `beads-gate-list-mode',
;; which shows open gates by default and closed gates with `a'
;; (`--all').  `t' filters by type (bd's `gate list' has no --type
;; flag, so the filter is applied to the parsed rows).  Columns follow
;; mockup §7a: Gate, Type, Await, Timeout, Waiters, Blocks, Status.
;; The gate's blocked issue(s) are recovered from the gate description
;; ("Ad-hoc gate blocking ISSUE"), which is the only place bd exposes
;; the edge from a gate row.
;;
;; Detail (REQ-SF-031): `beads-gate-open' renders a sectioned
;; `beads-gate-detail-mode' buffer with type, status, await-id,
;; timeout/expiry, repo, reason, blocked issues, and waiters, using
;; the same section vocabulary as the issue detail view.
;;
;; Actions (REQ-SF-032): `c' create, `C' check, `D' discover, `R'
;; resolve (reason required), and `w' add-waiter are reachable from
;; both the list and the detail buffer.  `beads-gate-create-command'
;; and `beads-gate-check-command' build the underlying command
;; objects so the argument assembly is unit-testable without a live
;; store.
;;
;; Molecule integration (REQ-SF-033): `beads-gate-display' is the
;; cl-defgeneric a molecule view calls to render a gated step, and
;; `beads-gate-step-glyph' returns the `[blocked-gate]' glyph plus the
;; gate id.  `beads-gate-open' is the single entry point a molecule
;; step binds RET to.  After a resolve or a check that closes a gate,
;; `beads-gate-closed-hook' runs with the closed gate id so the
;; molecule view can react to a just-closed gate (`bd ready --gated').

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'tabulated-list)
(require 'time-date)
(require 'beads-command)
(require 'beads-command-gate)
(require 'beads-reader)
(require 'beads-types)
(require 'beads-buffer)
(require 'beads-faces)
(require 'beads-pager)

(declare-function beads-show "beads-command-show" (issue-id))
(declare-function beads-buffer-find-utility-buffers "beads-buffer"
                  (&optional project type))

;;; Customization

(defgroup beads-gate nil
  "Gate (async wait condition) list, detail, and actions."
  :group 'beads)

(defcustom beads-gate-id-width 14
  "Width of the Gate column in the gate list."
  :type 'integer
  :group 'beads-gate)

(defcustom beads-gate-type-width 9
  "Width of the Type column in the gate list."
  :type 'integer
  :group 'beads-gate)

(defcustom beads-gate-await-width 22
  "Width of the Await column in the gate list."
  :type 'integer
  :group 'beads-gate)

(defcustom beads-gate-timeout-width 9
  "Width of the Timeout column in the gate list."
  :type 'integer
  :group 'beads-gate)

(defcustom beads-gate-waiters-width 9
  "Width of the Waiters column in the gate list."
  :type 'integer
  :group 'beads-gate)

(defcustom beads-gate-blocks-width 20
  "Width of the Blocks column in the gate list."
  :type 'integer
  :group 'beads-gate)

(defcustom beads-gate-status-width 10
  "Width of the Status column in the gate list."
  :type 'integer
  :group 'beads-gate)

;;; Faces

(defface beads-gate-type-human
  '((t :inherit beads-face-status-open))
  "Face for the `human' gate type."
  :group 'beads-gate)

(defface beads-gate-type-timer
  '((t :inherit beads-face-status-in-progress))
  "Face for the `timer' gate type."
  :group 'beads-gate)

(defface beads-gate-type-gh-run
  '((t :inherit beads-face-agent-running))
  "Face for the `gh:run' gate type."
  :group 'beads-gate)

(defface beads-gate-type-gh-pr
  '((t :inherit beads-face-agent-idle))
  "Face for the `gh:pr' gate type."
  :group 'beads-gate)

(defface beads-gate-type-bead
  '((t :inherit beads-face-status-blocked))
  "Face for the `bead' gate type."
  :group 'beads-gate)

(defface beads-gate-blocked
  '((t :inherit beads-face-status-blocked))
  "Face for the `[blocked-gate]' molecule step marker."
  :group 'beads-gate)

;;; Buffer-local state

(defvar-local beads-gate-list--show-all nil
  "When non-nil, `bd gate list --all' includes closed gates.")

(defvar-local beads-gate-list--type-filter nil
  "When non-nil, only gates with this `await_type' are listed.")

(defvar-local beads-gate-list--last-error nil
  "Last gate-list error message, or nil after a successful refresh.")

(defvar-local beads-gate-detail--gate-id nil
  "Gate id displayed in the current detail buffer.")

;;; Integration hook

(defvar beads-gate-closed-hook nil
  "Hook run after `beads-gate-resolve' or a closing `beads-gate-check'.
Each function is called with the gate id that closed (a string).
The molecule view (WI-SF-01) uses this to refresh a just-closed
gate; see REQ-SF-033.  `default-directory' is the store the action
ran against.")

;;; Field access

(defun beads-gate--get (gate field)
  "Read FIELD from GATE, a `beads-issue' object or an alist.
The JSON spells the issue type `issue_type' but callers ask for
`issue-type', so both spellings are honored, as are the underscore
aliases `await_type', `await_id', `created_at', and `updated_at'."
  (cond
   ((beads-issue-p gate) (eieio-oref gate field))
   ((listp gate)
    (or (alist-get field gate)
        (pcase field
          ('issue-type (alist-get 'issue_type gate))
          ('await-type (alist-get 'await_type gate))
          ('await-id (alist-get 'await_id gate))
          ('created-at (alist-get 'created_at gate))
          ('updated-at (alist-get 'updated_at gate))
          (_ nil))))
   (t nil)))

(defun beads-gate--description (gate)
  "Return the description string of GATE, or nil."
  (let ((value (beads-gate--get gate 'description)))
    (and (stringp value) (not (string-empty-p value)) value)))

;;; Normalization

(defun beads-gate--coerce (raw)
  "Coerce RAW, an alist or `beads-issue', to a `beads-issue' or nil.
The gate JSON already carries `issue_type', so this is a thin
`beads-from-json' wrapper that drops rows it cannot construct."
  (cond
   ((beads-issue-p raw) raw)
   ((consp raw)
    (condition-case nil
        (beads-from-json 'beads-issue raw)
      (error nil)))
   (t nil)))

(defun beads-gate--normalize (result)
  "Return RESULT as a list of `beads-issue' gate objects.
RESULT is the parsed `bd gate list --json' payload: a vector or
list of gate alists.  Elements that cannot be coerced are dropped."
  (let ((rows (cond
               ((null result) nil)
               ((vectorp result) (append result nil))
               ((listp result) result)
               (t nil))))
    (delq nil (mapcar #'beads-gate--coerce rows))))

(defun beads-gate--filter-type (gates)
  "Return only the GATES whose `await_type' matches the buffer filter.
The filter is `beads-gate-list--type-filter' (nil means no filter)."
  (if (or (null beads-gate-list--type-filter)
          (string-empty-p beads-gate-list--type-filter))
      gates
    (seq-filter (lambda (gate)
                  (equal (beads-gate--get gate 'await-type)
                         beads-gate-list--type-filter))
                gates)))

;;; Display contract (REQ-SF-033)

(cl-defgeneric beads-gate-display (gate)
  "Return the display plist for GATE.
GATE is a `beads-issue' object or a raw gate alist.  The returned
plist has the keys `:id', `:type', `:status', `:await-id',
`:timeout', `:timeout-label', `:waiters', `:blocks', `:reason',
`:repo', `:created', and `:expiry'.  This is the contract the
molecule view uses to render a gated step (REQ-SF-033); a consumer
may add keys with `cl-defmethod' `:after'.")

(defun beads-gate--display-plist (gate)
  "Build the `beads-gate-display' plist for GATE.
Shared by the `beads-issue' and alist specializers."
  (let* ((created (beads-gate--get gate 'created-at))
         (timeout (beads-gate--get gate 'timeout))
         (desc (beads-gate--description gate)))
    (list :id (beads-gate--get gate 'id)
          :type (or (beads-gate--get gate 'await-type) "")
          :status (or (beads-gate--get gate 'status) "")
          :await-id (or (beads-gate--get gate 'await-id) "")
          :timeout timeout
          :timeout-label (beads-gate--format-duration timeout)
          :waiters (or (beads-gate--get gate 'waiters) '())
          :blocks (beads-gate--parse-blocks desc)
          :reason (beads-gate--parse-reason desc)
          :repo (beads-gate--parse-repo gate)
          :created created
          :expiry (beads-gate--expiry created timeout))))

(cl-defmethod beads-gate-display ((gate beads-issue))
  "Return the display plist for GATE, a `beads-issue'."
  (beads-gate--display-plist gate))

(cl-defmethod beads-gate-display ((gate list))
  "Return the display plist for GATE, a raw JSON alist."
  (beads-gate--display-plist gate))

;;; Parsing helpers

(defun beads-gate--parse-blocks (description)
  "Return the blocked issue ids named in DESCRIPTION.
Ad-hoc gates describe themselves as \"Ad-hoc gate blocking ISSUE\";
formula gates often carry the same \"blocking ISSUE\" fragment.  The
ids are returned in order of appearance with duplicates removed, or
nil when DESCRIPTION is nil or has no such fragment."
  (when (and description (stringp description))
    (let (ids)
      (with-temp-buffer
        (insert description)
        (goto-char (point-min))
        (while (re-search-forward
                "blocking[ \t]+\\([A-Za-z0-9][A-Za-z0-9._:-]*\\)" nil t)
          (push (match-string-no-properties 1) ids)))
      (delete-dups (nreverse ids)))))

(defun beads-gate--parse-reason (description)
  "Return the reason named in DESCRIPTION, or nil.
bd renders the reason after a blank line as \"Reason: TEXT\"."
  (when (and description (stringp description))
    (when (string-match "Reason:[ \t]*\\(.*\\)" description)
      (let ((reason (string-trim (match-string-no-properties 1 description))))
        (unless (string-empty-p reason) reason)))))

(defun beads-gate--parse-repo (gate)
  "Return the metadata `repo' value for GATE, or nil.
The value is only present for gates that name another repository."
  (let ((metadata (beads-gate--get gate 'metadata)))
    (when (listp metadata)
      (cdr (assq 'repo metadata)))))

(defun beads-gate--expiry (created timeout)
  "Return the expiry time string for a gate created at CREATED.
TIMEOUT is the gate timeout in nanoseconds (or nil).  Returns nil
when either input is missing or unparseable.  When CREATED already
looks like a preformatted expiry it is returned unchanged."
  (when (and created timeout (numberp timeout) (> timeout 0))
    (condition-case nil
        (format-time-string
         "%Y-%m-%dT%H:%M:%S%z"
         (time-add (date-to-time created) (seconds-to-time (/ timeout 1e9))))
      (error nil))))

;;; Formatting

(defconst beads-gate-type-glyphs
  '(("human" . "⚑") ("timer" . "⏱") ("gh:run" . "⚙")
    ("gh:pr" . "⇄") ("bead" . "⇄"))
  "Glyphs for the bd gate types, keyed by the `await_type' string.")

(defconst beads-gate-type-faces
  '(("human" . beads-gate-type-human)
    ("timer" . beads-gate-type-timer)
    ("gh:run" . beads-gate-type-gh-run)
    ("gh:pr" . beads-gate-type-gh-pr)
    ("bead" . beads-gate-type-bead))
  "Faces for the bd gate types, keyed by the `await_type' string.")

(defun beads-gate-type-glyph (type)
  "Return the canonical glyph for gate TYPE.
Unknown types render an empty string."
  (or (cdr (assoc type beads-gate-type-glyphs)) ""))

(defun beads-gate--type-face (type)
  "Return the face symbol for gate TYPE, or `default'."
  (or (cdr (assoc type beads-gate-type-faces)) 'default))

(defun beads-gate--format-type (type)
  "Return the propertized Type cell for gate TYPE."
  (let ((type (or type "")))
    (propertize (format "%s %s" (beads-gate-type-glyph type) type)
                'face (beads-gate--type-face type)
                'help-echo (format "Gate type: %s" type))))

(defun beads-gate--format-await (_type await-id)
  "Return the Await cell for gate AWAIT-ID.
_TYPE is accepted for symmetry with the other formatters; the cell
is empty for types that carry no await condition."
  (if (or (null await-id) (string-empty-p (format "%s" await-id)))
      ""
    (format "%s" await-id)))

(defun beads-gate--format-duration (ns)
  "Format NS, a duration in nanoseconds, as a compact string.
Accepts a string verbatim so already-formatted inputs pass
through, and returns an empty string for nil."
  (cond
   ((null ns) "")
   ((stringp ns) ns)
   ((not (numberp ns)) "")
   ((<= ns 0) "")
   (t
    (let ((seconds (floor (/ ns 1e9))))
      (cond
       ((< seconds 60) (format "%ds" seconds))
       ((< seconds 3600) (format "%dm" (floor (/ seconds 60))))
       ((< seconds 86400)
        (let ((hours (floor (/ seconds 3600)))
              (mins (floor (/ (mod seconds 3600) 60))))
          (if (> mins 0) (format "%dh%dm" hours mins)
            (format "%dh" hours))))
       (t
        (let ((days (floor (/ seconds 86400)))
              (hours (floor (/ (mod seconds 86400) 3600))))
          (if (> hours 0) (format "%dd%dh" days hours)
            (format "%dd" days)))))))))

(defun beads-gate--format-timeout (gate)
  "Return the Timeout cell for GATE."
  (beads-gate--format-duration (beads-gate--get gate 'timeout)))

(defun beads-gate--format-waiters (gate)
  "Return the Waiters cell (a count) for GATE."
  (let* ((waiters (beads-gate--get gate 'waiters))
         (count (cond ((listp waiters) (length waiters))
                      ((null waiters) 0)
                      (t 1))))
    (if (> count 0)
        (propertize (format "%d" count)
                    'help-echo (format "%d waiter(s)" count))
      "")))

(defun beads-gate--format-blocks (gate)
  "Return the Blocks cell for GATE."
  (let ((blocks (beads-gate--parse-blocks (beads-gate--description gate))))
    (if blocks (mapconcat #'identity blocks ",") "")))

(defun beads-gate--format-status (gate)
  "Return the propertized Status cell for GATE."
  (let* ((status (or (beads-gate--get gate 'status) ""))
         (face (beads-face-status-face status)))
    (propertize (format "%s %s" (beads-face-status-glyph status) status)
                'face face)))

;;; List entries

(defun beads-gate--entry (gate)
  "Return a `tabulated-list-entries' entry for GATE."
  (let ((id (or (beads-gate--get gate 'id) "")))
    (list id
          (vector
           (propertize id 'face 'beads-face-id)
           (beads-gate--format-type (beads-gate--get gate 'await-type))
           (beads-gate--format-await (beads-gate--get gate 'await-type)
                                     (beads-gate--get gate 'await-id))
           (beads-gate--format-timeout gate)
           (beads-gate--format-waiters gate)
           (beads-gate--format-blocks gate)
           (beads-gate--format-status gate)))))

;;; Loading

(defun beads-gate-list--command ()
  "Return a `beads-command-gate-list' for the current buffer settings."
  (beads-command-gate-list
   :all beads-gate-list--show-all
   :json t))

(defun beads-gate-list--populate (result)
  "Populate the current buffer from the parsed gate-list RESULT."
  (setq beads-gate-list--last-error nil)
  (beads-pager-set-entries
   (mapcar #'beads-gate--entry
           (beads-gate--filter-type (beads-gate--normalize result)))))

(defun beads-gate-list--populate-error (err)
  "Record ERR and leave the current buffer empty."
  (setq beads-gate-list--last-error (error-message-string err))
  (beads-pager-set-entries nil)
  (message "bd gate list failed: %s" beads-gate-list--last-error))

(defun beads-gate-list-refresh ()
  "Reload the gate list asynchronously.
Opening and refreshing the view spawns `bd' through
`beads-command-execute-async' so a remote (TRAMP) store is never
blocked on a synchronous round trip."
  (interactive)
  (let ((buffer (current-buffer)))
    (beads-command-execute-async
     (beads-gate-list--command)
     (lambda (result)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer (beads-gate-list--populate result))))
     (lambda (err)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer (beads-gate-list--populate-error err))))
     :cache-key (list 'beads-gate beads-gate-list--show-all
                      beads-gate-list--type-filter))))

;;; Point and view toggles

(defun beads-gate-list--current-id ()
  "Return the gate id at point, or nil."
  (ignore-errors (tabulated-list-get-id)))

(defun beads-gate-list-next ()
  "Move to the next gate."
  (interactive)
  (forward-line 1))

(defun beads-gate-list-previous ()
  "Move to the previous gate."
  (interactive)
  (forward-line -1))

(defun beads-gate-list-quit ()
  "Bury the gate list buffer."
  (interactive)
  (quit-window t))

(defun beads-gate-list-toggle-all ()
  "Toggle whether closed gates are included (`--all')."
  (interactive)
  (setq beads-gate-list--show-all (not beads-gate-list--show-all))
  (beads-gate-list-refresh)
  (message "Gates: %s" (if beads-gate-list--show-all "all" "open only")))

(defun beads-gate-list-set-type ()
  "Set or clear the type filter for the gate list.
bd's `gate list' has no --type flag, so the filter is applied to
the parsed rows."
  (interactive)
  (let* ((choices '("human" "timer" "gh:run" "gh:pr" "bead" ""))
         (value (completing-read "Gate type (blank to clear): "
                                 choices nil nil
                                 beads-gate-list--type-filter)))
    (setq beads-gate-list--type-filter
          (unless (string-empty-p value) value)))
  (beads-gate-list-refresh)
  (message "Gate type filter: %s" (or beads-gate-list--type-filter "any")))

;;; List mode

(defvar beads-gate-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    ;; Navigation
    (keymap-set map "n" #'beads-gate-list-next)
    (keymap-set map "p" #'beads-gate-list-previous)
    (keymap-set map "RET" #'beads-gate-open)
    ;; Actions
    (keymap-set map "c" #'beads-gate-create)
    (keymap-set map "C" #'beads-gate-check)
    (keymap-set map "R" #'beads-gate-resolve)
    (keymap-set map "w" #'beads-gate-add-waiter)
    (keymap-set map "D" #'beads-gate-discover)
    ;; View
    (keymap-set map "a" #'beads-gate-list-toggle-all)
    (keymap-set map "t" #'beads-gate-list-set-type)
    (keymap-set map "g" #'beads-gate-list-refresh)
    (keymap-set map "q" #'beads-gate-list-quit)
    (beads-mode--install-navigation-keys map)
    map)
  "Keymap for `beads-gate-list-mode'.")

(define-derived-mode beads-gate-list-mode tabulated-list-mode "Beads-Gates"
  "Major mode for listing bd gates (async wait conditions).

\\{beads-gate-list-mode-map}"
  (setq tabulated-list-format
        (vector (list "Gate" beads-gate-id-width t)
                (list "Type" beads-gate-type-width t)
                (list "Await" beads-gate-await-width t)
                (list "Timeout" beads-gate-timeout-width t)
                (list "Waiters" beads-gate-waiters-width t)
                (list "Blocks" beads-gate-blocks-width t)
                (list "Status" beads-gate-status-width t)))
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key nil)
  (tabulated-list-init-header)
  (hl-line-mode 1)
  (beads-pager-mode 1))

;;;###autoload
(defun beads-gate-list ()
  "Display bd gates in a tabulated list buffer.
Shows open gates by default; `a' includes closed gates and `t`
filters by type.  RET opens the gate detail; `c' creates, `C'
checks, `R' resolves, `w' adds a waiter, and `D' discovers await
ids."
  (interactive)
  (let ((buffer (get-buffer-create (beads-buffer-utility "gates"))))
    (with-current-buffer buffer
      (beads-gate-list-mode)
      (beads-gate-list-refresh)
      (setq mode-line-format
            '("%e" mode-line-front-space
              mode-line-buffer-identification
              (:eval (let ((count (beads-pager--total-count)))
                       (format "  %d gate%s%s"
                               count
                               (if (= count 1) "" "s")
                               (or (beads-pager--mode-line-fragment) "")))))))
    (pop-to-buffer buffer)))

;;; Create (REQ-SF-032)

(defun beads-gate-create-command (type blocks &rest kwargs)
  "Build a `beads-command-gate-create' for TYPE and BLOCKS.
BLOCKS is the issue the new gate blocks.  KWARGS may carry
`:await-id', `:timeout', `:reason', and `:title'; nil values are
dropped so bd applies its own defaults."
  (apply #'beads-command-gate-create
         :gate-type type
         :blocks blocks
         :json t
         (cl-loop for (key value) on kwargs by #'cddr
                  unless (null value)
                  append (list key value))))

(defun beads-gate--result-id (result)
  "Extract the created gate id from RESULT.
RESULT is the parsed `bd gate create --json' payload: an alist
with an `id', or a bare string when the command ran without JSON."
  (cond
   ((and (listp result) (assq 'id result)) (alist-get 'id result))
   ((stringp result) (string-trim result))
   (t nil)))

(defun beads-gate--read-create ()
  "Prompt for the fields of a new gate and return the built command."
  (let* ((type (completing-read "Gate type: "
                                '("human" "timer" "gh:run" "gh:pr")
                                nil t nil nil "human"))
         (blocks (beads-reader-issue-id "Block issue: "))
         (await (when (member type '("gh:run" "gh:pr"))
                  (read-string "Await id (blank to skip): ")))
         (timeout (when (member type '("timer" "gh:run"))
                    (read-string "Timeout (e.g. 2h, blank to skip): ")))
         (reason (read-string "Reason (blank to skip): ")))
    (beads-gate-create-command
     type blocks
     :await-id (unless (string-empty-p await) await)
     :timeout (unless (string-empty-p timeout) timeout)
     :reason (unless (string-empty-p reason) reason))))

(defun beads-gate--refresh-open-lists ()
  "Refresh every live gate list buffer."
  (dolist (buffer (beads-buffer-find-utility-buffers nil "gates"))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer (beads-gate-list-refresh)))))

;;;###autoload
(defun beads-gate-create ()
  "Interactively create a gate that blocks an issue.
Prompts for the gate type, the blocked issue, and the optional
await-id, timeout, and reason, then executes `bd gate create'."
  (interactive)
  (let* ((cmd (beads-gate--read-create))
         (result (beads-command-execute cmd))
         (gate-id (beads-gate--result-id result)))
    (beads-gate--refresh-open-lists)
    (message "Created gate %s" (or gate-id "?"))
    gate-id))

;;; Check (REQ-SF-032)

(defun beads-gate-check-command (&rest kwargs)
  "Build a `beads-command-gate-check' from KWARGS.
Recognized keys are `:type', `:dry-run', `:escalate', and `:limit'.
Human-readable output is requested (`:json nil') because bd prints
a gate-by-gate report followed by a JSON summary on the same
stream, which the JSON reader cannot parse."
  (apply #'beads-command-gate-check :json nil kwargs))

(defun beads-gate--resolved-ids (output)
  "Return the gate ids reported resolved in check OUTPUT.
Recognizes bd's \"ID: resolved\" report lines.  Returns nil when
OUTPUT is not a string (for example a JSON payload)."
  (when (stringp output)
    (let (ids)
      (with-temp-buffer
        (insert output)
        (goto-char (point-min))
        (while (re-search-forward
                "\\([A-Za-z0-9][A-Za-z0-9._:-]*\\): resolved" nil t)
          (push (match-string-no-properties 1) ids)))
      (delete-dups (nreverse ids)))))

(defun beads-gate--show-output (title output)
  "Display TITLE and OUTPUT in a temporary report buffer."
  (let ((buffer (get-buffer-create (beads-buffer-utility "gate-report"))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert title "\n")
        (insert (make-string (length title) ?=) "\n\n")
        (insert (or output ""))
        (goto-char (point-min))
        (special-mode)))
    (display-buffer buffer)
    buffer))

;;;###autoload
(defun beads-gate-check (&optional dry-run)
  "Evaluate open gates and close the resolved ones.
With prefix argument DRY-RUN non-nil, only show what would happen
without changing anything.  Prompts for a gate type filter.  A
closing check runs `beads-gate-closed-hook' for each resolved gate
and refreshes any open gate list."
  (interactive "P")
  (let* ((type (completing-read
                "Check type (blank = all): "
                '("" "gh" "gh:run" "gh:pr" "timer" "bead" "all")
                nil nil nil))
         (cmd (beads-gate-check-command
               :type (unless (string-empty-p type) type)
               :dry-run (and dry-run t)))
         (output (beads-command-execute cmd)))
    (beads-gate--show-output
     (if dry-run "Gate check (dry-run)" "Gate check applied")
     output)
    (dolist (id (beads-gate--resolved-ids output))
      (run-hook-with-args 'beads-gate-closed-hook id))
    (beads-gate--refresh-open-lists)
    (message "Gate check %s" (if dry-run "previewed" "applied"))
    output))

;;; Resolve (REQ-SF-032)

(defun beads-gate--require-reason (reason)
  "Return REASON trimmed, or signal a `user-error' when blank.
Resolving a gate always records a reason."
  (let ((reason (string-trim (or reason ""))))
    (if (string-empty-p reason)
        (user-error "A reason is required to resolve a gate")
      reason)))

(defun beads-gate--read-gate-id (prompt)
  "Read a gate id with PROMPT, defaulting to the gate at point.
In a detail buffer the displayed gate id is the default."
  (let ((default (or (and (derived-mode-p 'beads-gate-detail-mode)
                          beads-gate-detail--gate-id)
                     (beads-gate-list--current-id))))
    (if (and default (not (string-empty-p default)))
        (read-string (format "%s (default %s): " prompt default)
                     nil nil default)
      (read-string (format "%s: " prompt)))))

;;;###autoload
(defun beads-gate-resolve (&optional gate-id reason)
  "Resolve (close) a gate, recording a required reason.
GATE-ID defaults to the gate at point; REASON is prompted for and
must be non-empty.  Runs `beads-gate-closed-hook' for the gate."
  (interactive)
  (let* ((gate-id (or gate-id (beads-gate--read-gate-id "Resolve gate")))
         (reason (beads-gate--require-reason
                  (or reason (read-string "Reason (required): ")))))
    (beads-command-execute
     (beads-command-gate-resolve
      :gate-id gate-id :reason reason :json nil))
    (run-hook-with-args 'beads-gate-closed-hook gate-id)
    (beads-gate--refresh-open-lists)
    (when (derived-mode-p 'beads-gate-detail-mode)
      (beads-gate-open gate-id))
    (message "Resolved gate %s" gate-id)
    gate-id))

;;; Add waiter (REQ-SF-032)

;;;###autoload
(defun beads-gate-add-waiter (&optional gate-id waiter)
  "Register WAITER as a waiter on GATE-ID.
GATE-ID defaults to the gate at point; WAITER is prompted for."
  (interactive)
  (let* ((gate-id (or gate-id (beads-gate--read-gate-id "Gate")))
         (waiter (or waiter (read-string "Waiter: "))))
    (unless (and (stringp waiter)
                 (not (string-empty-p (string-trim waiter))))
      (user-error "A waiter is required"))
    (beads-command-execute
     (beads-command-gate-add-waiter
      :gate-id gate-id :waiter-id (string-trim waiter) :json nil))
    (beads-gate--refresh-open-lists)
    (message "Added waiter %s to gate %s" (string-trim waiter) gate-id)
    gate-id))

;;; Discover (REQ-SF-032)

;;;###autoload
(defun beads-gate-discover (&optional branch limit max-age dry-run)
  "Discover await ids for `gh:run' gates.
BRANCH, LIMIT, MAX-AGE, and DRY-RUN map to the matching `bd gate
discover' flags.  The output is shown in a report buffer."
  (interactive)
  (let* ((branch (or branch (read-string "Branch (blank = current): ")))
         (limit (or limit (read-string "Limit (blank = default): ")))
         (max-age (or max-age (read-string "Max age (blank = 30m): ")))
         (cmd (beads-command-gate-discover
               :branch (unless (string-empty-p branch) branch)
               :limit (unless (string-empty-p limit) limit)
               :max-age (unless (string-empty-p max-age) max-age)
               :dry-run (and dry-run t)
               :json nil))
         (output (beads-command-execute cmd)))
    (beads-gate--show-output
     (if dry-run "Discover run ids (dry-run)" "Discover run ids applied")
     output)
    (beads-gate--refresh-open-lists)
    output))

;;; Detail view (REQ-SF-031)

(defun beads-gate--insert-header (label value &optional value-face)
  "Insert a LABEL: VALUE line, with VALUE-FACE when supplied."
  (insert (propertize label 'face 'beads-face-key))
  (insert ": ")
  (when value
    (insert (propertize (format "%s" value)
                        'face (or value-face 'beads-face-header))))
  (insert "\n"))

(defun beads-gate--insert-section (title renderer)
  "Insert a section TITLE rendered by RENDERER.
The header is omitted when RENDERER inserts nothing."
  (let ((start (point)))
    (insert "\n")
    (insert (propertize (upcase title) 'face 'beads-face-section))
    (insert "\n")
    (let ((body-start (point)))
      (funcall renderer)
      (when (= (point) body-start)
        (delete-region start (point))))))

(defun beads-gate--insert-issue-list (ids)
  "Insert IDS as clickable issue lines."
  (dolist (id ids)
    (insert "  ")
    (insert-text-button id
                        'face 'beads-face-id
                        'follow-link t
                        'action (lambda (_button) (beads-show id)))
    (insert "\n")))

(defun beads-gate--format-timestamp (value)
  "Format VALUE, an ISO timestamp string, or return it unchanged."
  (if (or (null value) (not (stringp value)) (string-empty-p value))
      value
    (condition-case nil
        (format-time-string "%Y-%m-%d %H:%M:%S" (date-to-time value))
      (error value))))

(defun beads-gate--render (gate)
  "Render GATE (a `beads-issue' or alist) into the current buffer."
  (let* ((display (beads-gate-display gate))
         (id (plist-get display :id))
         (type (plist-get display :type))
         (status (plist-get display :status)))
    (setq header-line-format
          (format "%s — gate%s"
                  id
                  (if (plist-get display :repo)
                      (format " · repo %s" (plist-get display :repo))
                    "")))
    (insert (propertize (format "%s — gate\n" id) 'face 'beads-face-header))
    (insert (make-string 72 ?─) "\n")
    (beads-gate--insert-header "Type" (beads-gate--format-type type))
    (beads-gate--insert-header "Status" status
                               (beads-face-status-face status))
    (beads-gate--insert-header "Await-id" (plist-get display :await-id))
    (beads-gate--insert-header "Repo"
                               (or (plist-get display :repo) "(current)"))
    (beads-gate--insert-header "Created"
                               (beads-gate--format-timestamp
                                (plist-get display :created)))
    (beads-gate--insert-header "Timeout" (plist-get display :timeout-label))
    (beads-gate--insert-header "Expires"
                               (beads-gate--format-timestamp
                                (plist-get display :expiry)))
    (when (plist-get display :reason)
      (beads-gate--insert-header "Reason" (plist-get display :reason)))
    (beads-gate--insert-section
     "Blocks"
     (lambda ()
       (beads-gate--insert-issue-list (plist-get display :blocks))))
    (beads-gate--insert-section
     "Waiters"
     (lambda ()
       (dolist (waiter (plist-get display :waiters))
         (insert (format "  %s\n" waiter)))))
    (goto-char (point-min))))

(defvar beads-gate-detail-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (keymap-set map "g" #'beads-gate-detail-refresh)
    (keymap-set map "q" #'quit-window)
    (keymap-set map "C" #'beads-gate-check)
    (keymap-set map "D" #'beads-gate-discover)
    (keymap-set map "R" #'beads-gate-resolve)
    (keymap-set map "w" #'beads-gate-add-waiter)
    ;; TAB/S-TAB/SPC move and toggle by thing; `?' dispatches; `C-c b'
    ;; is the reserved extension prefix.
    (beads-mode--install-navigation-keys map)
    map)
  "Keymap for `beads-gate-detail-mode'.")

(define-derived-mode beads-gate-detail-mode special-mode "Beads-Gate"
  "Major mode for the sectioned bd gate detail view.

\\{beads-gate-detail-mode-map}"
  (setq truncate-lines nil)
  (setq buffer-read-only t))

(defun beads-gate-detail-refresh ()
  "Reload the gate shown in the current detail buffer."
  (interactive)
  (when (and beads-gate-detail--gate-id
             (derived-mode-p 'beads-gate-detail-mode))
    (beads-gate-open beads-gate-detail--gate-id)))

(defun beads-gate--load (gate-id)
  "Load and normalize the gate GATE-ID.
Returns a `beads-issue' when the store returns one, otherwise the
raw alist.  Signals `beads-command-error' when the store has no
such gate."
  (let ((result (beads-command-execute
                 (beads-command-gate-show :gate-id gate-id :json t))))
    (or (beads-gate--coerce (if (vectorp result)
                                (and (> (length result) 0) (aref result 0))
                              result))
        result)))

;;;###autoload
(defun beads-gate-open (gate-id)
  "Open the detail buffer for GATE-ID.
GATE-ID is a string; with no argument (interactively) the gate at
point is used.  The molecule view calls this for a gated step
\(REQ-SF-033).  Signals a `user-error' when GATE-ID is nil."
  (interactive (list (beads-gate-list--current-id)))
  (unless (and gate-id (not (string-empty-p gate-id)))
    (user-error "No gate at point"))
  (let* ((gate (beads-gate--load gate-id))
         (buffer (get-buffer-create (beads-buffer-utility "gate" gate-id))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'beads-gate-detail-mode)
        (beads-gate-detail-mode))
      (setq beads-gate-detail--gate-id gate-id)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (beads-gate--render gate)))
    (pop-to-buffer buffer)
    buffer))

;;; Molecule integration (REQ-SF-033)

(defun beads-gate-step-glyph (gate)
  "Return the `[blocked-gate]' marker for a gated molecule step.
GATE is a gate id string, a `beads-issue', or a raw alist.  The
returned string starts with the gate glyph and carries GATE's id so
the molecule view can route RET to `beads-gate-open'."
  (let* ((type (cond ((stringp gate) "")
                     (t (or (beads-gate--get gate 'await-type) ""))))
         (id (cond ((stringp gate) gate)
                   (t (or (beads-gate--get gate 'id) "")))))
    (propertize (format "%s [blocked-gate] %s"
                        (if (string-empty-p type)
                            "⊘g"
                          (beads-gate-type-glyph type))
                        id)
                'face 'beads-gate-blocked)))

;; The command classes in `beads-command-gate' auto-generate transient
;; prefixes named `beads-gate-list', `beads-gate-create', etc.  This
;; module owns those entry points as plain commands (design.md §5.3),
;; so clear the stale prefix objects the generator leaves behind and
;; let the parent `beads-gate' menu call the new UI instead.
(dolist (sym '(beads-gate-list beads-gate-create beads-gate-check
               beads-gate-resolve beads-gate-add-waiter
               beads-gate-discover))
  (put sym 'transient--prefix nil))

(provide 'beads-gate)
;;; beads-gate.el ends here
