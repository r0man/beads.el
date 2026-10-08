;;; beads-wisp.el --- Wisp list and lifecycle UI -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Wisps are bd's ephemeral molecule plane: issues stored with
;; `Ephemeral=true' that are local-only and eventually evaporate
;; (burn) or condense (squash).  This module owns the wisp list and
;; its destructive lifecycle actions.
;;
;; The list reads `bd mol wisp list --json', whose payload is an
;; object with a `wisps' array rather than a bare array;
;; `beads-wisp--normalize' unwraps it and yields `beads-issue'
;; objects.  Columns follow mockup §8a: Id, Type, Phase, Status,
;; Started, Updated, Age.
;;
;; Old detection (REQ-SF-040): `beads-wisp-kind' classifies a wisp as
;; `closed' (status closed), `old' (not updated within
;; `beads-wisp-old-seconds', 24h by default), or `new'.  Old rows
;; carry an `old' marker in the Age column.
;;
;; Lifecycle (REQ-SF-041): every destructive path confirms first.
;; Squash offers an optional agent summary and a keep-children
;; toggle; burn runs a dry-run preview and then requires the id to be
;; typed; purge runs a dry-run, echoes its counts, and requires
;; `yes'.  Batch burn acts on marked rows when any are marked.
;;
;; Phase visibility (REQ-SF-042): rows render a phase badge; wisps are
;; ephemeral so they render `◇ vapor', while a promoted issue renders
;; `◆ persistent'.  RET opens the molecule root view through
;; `beads-molecule-open' when `beads-molecule.el' is loaded (WI-SF-01)
;; and otherwise runs `bd mol show' in a terminal buffer.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'tabulated-list)
(require 'time-date)
(require 'beads-command)
(require 'beads-command-mol)
;; `beads-command-purge' lives in beads-command-misc.el.
(require 'beads-command-misc)
(require 'beads-types)
(require 'beads-buffer)
(require 'beads-faces)
(require 'beads-pager)

(declare-function beads-molecule-open "beads-molecule" (mol-id))
(declare-function beads-dispatch "beads" ())

;;; Customization

(defgroup beads-wisp nil
  "Wisp (ephemeral molecule) list and lifecycle."
  :group 'beads)

(defcustom beads-wisp-old-seconds 86400
  "Age in seconds after which a wisp is considered old.
Old wisps have not been updated in this many seconds (24 hours by
default) and are candidates for `bd mol wisp gc'.  See
`beads-wisp-kind'."
  :type 'integer
  :group 'beads-wisp)

(defcustom beads-wisp-id-width 20
  "Width of the Id column in the wisp list."
  :type 'integer
  :group 'beads-wisp)

(defcustom beads-wisp-type-width 10
  "Width of the Type column in the wisp list."
  :type 'integer
  :group 'beads-wisp)

(defcustom beads-wisp-phase-width 12
  "Width of the Phase column in the wisp list."
  :type 'integer
  :group 'beads-wisp)

(defcustom beads-wisp-status-width 12
  "Width of the Status column in the wisp list."
  :type 'integer
  :group 'beads-wisp)

(defcustom beads-wisp-started-width 9
  "Width of the Started column in the wisp list."
  :type 'integer
  :group 'beads-wisp)

(defcustom beads-wisp-updated-width 9
  "Width of the Updated column in the wisp list."
  :type 'integer
  :group 'beads-wisp)

(defcustom beads-wisp-age-width 12
  "Width of the Age column in the wisp list."
  :type 'integer
  :group 'beads-wisp)

;;; Faces

(defface beads-wisp-vapor
  '((t :inherit beads-face-status-in-progress))
  "Face for the ephemeral `vapor' phase badge."
  :group 'beads-wisp)

(defface beads-wisp-persistent
  '((t :inherit beads-face-success))
  "Face for the `persistent' phase badge."
  :group 'beads-wisp)

(defface beads-wisp-old
  '((t :inherit beads-face-warning))
  "Face for the old-wisp marker in the Age column."
  :group 'beads-wisp)

;;; Buffer-local state

(defvar-local beads-wisp--show-all nil
  "When non-nil, `bd mol wisp list --all' includes closed wisps.")

(defvar-local beads-wisp--type-filter nil
  "When non-nil, a string passed to `bd mol wisp list --type'.")

(defvar-local beads-wisp--marked nil
  "List of wisp ids marked for a batch lifecycle action.")

(defvar-local beads-wisp--last-error nil
  "Last wisp-list error message, or nil after a successful refresh.")

;;; Field access

(defun beads-wisp--get (wisp field)
  "Read FIELD from WISP, a `beads-issue' object or an alist.
The JSON wisp payload spells the issue type `type' and the
timestamps with underscores, so those aliases are honored too."
  (cond
   ((beads-issue-p wisp) (eieio-oref wisp field))
   ((listp wisp)
    (or (alist-get field wisp)
        (pcase field
          ('issue-type (alist-get 'type wisp))
          ('created-at (alist-get 'created_at wisp))
          ('updated-at (alist-get 'updated_at wisp)))))
   (t nil)))

(defun beads-wisp--id (wisp)
  "Return the id of WISP."
  (beads-wisp--get wisp 'id))

;;; Normalization

(defun beads-wisp--coerce (raw)
  "Coerce RAW, an alist or `beads-issue', to a `beads-issue' or nil.
The wisp-list JSON names the issue type `type'; map it onto the
`issue_type' slot before delegating to `beads-from-json'.  Wisp-list
rows are ephemeral by definition, so set `ephemeral' when absent."
  (cond
   ((beads-issue-p raw) raw)
   ((consp raw)
    (condition-case nil
        (beads-from-json
         'beads-issue
         (cons (cons 'issue_type
                     (or (alist-get 'type raw)
                         (alist-get 'issue_type raw)))
               (cons '(ephemeral . t) raw)))
      (error nil)))
   (t nil)))

(defun beads-wisp--normalize (result)
  "Return RESULT as a list of `beads-issue' wisp objects.
RESULT is the parsed `bd mol wisp list --json' payload: an alist
with a `wisps' array, or a bare array/list of wisp alists.
Elements that cannot be coerced are dropped."
  (let* ((rows (cond
                ((null result) nil)
                ((and (listp result) (assq 'wisps result))
                 (alist-get 'wisps result))
                ((vectorp result) result)
                ((listp result) result)
                (t nil)))
         (list-rows (cond ((vectorp rows) (append rows nil))
                          ((listp rows) rows)
                          (t nil))))
    (delq nil (mapcar #'beads-wisp--coerce list-rows))))

;;; Classification and formatting

(defun beads-wisp--old-p (wisp)
  "Return non-nil when WISP has not been updated in `beads-wisp-old-seconds'."
  (let ((updated (beads-wisp--get wisp 'updated-at)))
    (when (and updated (stringp updated) (not (string-empty-p updated)))
      (condition-case nil
          (time-less-p (date-to-time updated)
                       (time-subtract (current-time) beads-wisp-old-seconds))
        (error nil)))))

(defun beads-wisp-kind (wisp)
  "Classify WISP as `closed', `old', or `new'.
WISP is a `beads-issue' object or an alist.  A wisp is `closed'
when its status is \"closed\"; otherwise `old' when it has not been
updated within `beads-wisp-old-seconds' (24h by default); otherwise
`new'."
  (cond
   ((equal (beads-wisp--get wisp 'status) "closed") 'closed)
   ((beads-wisp--old-p wisp) 'old)
   (t 'new)))

(defun beads-wisp--format-time (timestamp)
  "Format TIMESTAMP as local HH:MM, or return an empty string.
TIMESTAMP is an RFC3339/ISO string as emitted by bd."
  (if (or (null timestamp) (not (stringp timestamp)) (string-empty-p timestamp))
      ""
    (condition-case nil
        (format-time-string "%H:%M" (date-to-time timestamp))
      (error timestamp))))

(defun beads-wisp--format-age (wisp &optional now)
  "Format how long ago WISP was updated, or an empty string.
NOW defaults to the current time and exists for deterministic
tests."
  (let ((updated (beads-wisp--get wisp 'updated-at)))
    (if (or (null updated) (not (stringp updated)) (string-empty-p updated))
        ""
      (condition-case nil
          (let* ((then (date-to-time updated))
                 (seconds (max 0 (floor (time-to-seconds
                                         (time-subtract (or now (current-time))
                                                        then))))))
            (cond
             ((< seconds 60) (format "%ds" seconds))
             ((< seconds 3600) (format "%dm" (floor (/ seconds 60))))
             ((< seconds 86400)
              (let ((hours (floor (/ seconds 3600)))
                    (mins (floor (/ (mod seconds 3600) 60))))
                (if (> mins 0)
                    (format "%dh%02dm" hours mins)
                  (format "%dh" hours))))
             (t
              (let ((days (floor (/ seconds 86400)))
                    (hours (floor (/ (mod seconds 86400) 3600))))
                (if (> hours 0)
                    (format "%dd%dh" days hours)
                  (format "%dd" days))))))
        (error "")))))

(defun beads-wisp--phase (wisp)
  "Return the phase symbol `vapor' or `persistent' for WISP."
  (if (eq (beads-wisp--get wisp 'ephemeral) t) 'vapor 'persistent))

(defun beads-wisp--format-phase (wisp)
  "Return the propertized phase badge for WISP."
  (let ((phase (beads-wisp--phase wisp)))
    (pcase phase
      ('vapor (propertize "◇ vapor"
                          'face 'beads-wisp-vapor
                          'help-echo "Ephemeral wisp (vapor phase)"))
      (_ (propertize "◆ persistent"
                     'face 'beads-wisp-persistent
                     'help-echo "Promoted to persistent")))))

(defun beads-wisp--format-status (wisp)
  "Return the propertized status cell for WISP."
  (let* ((status (or (beads-wisp--get wisp 'status) ""))
         (face (beads-face-status-face status)))
    (propertize (format "%s %s" (beads-face-status-glyph status) status)
                'face face)))

(defun beads-wisp--format-age-cell (wisp &optional now)
  "Return the Age cell for WISP, marking old and closed wisps.
NOW is passed through to `beads-wisp--format-age' for tests."
  (let ((age (beads-wisp--format-age wisp now)))
    (pcase (beads-wisp-kind wisp)
      ('old (concat age
                    (if (string-empty-p age) "" " ")
                    (propertize "old" 'face 'beads-wisp-old)))
      ('closed (concat age
                       (if (string-empty-p age) "" " ")
                       (propertize "closed" 'face 'beads-face-status-closed)))
      (_ age))))

(defun beads-wisp--entry (wisp)
  "Return a `tabulated-list-entries' entry for WISP."
  (let ((id (or (beads-wisp--id wisp) "")))
    (list id
          (vector
           (propertize id 'face 'beads-face-id)
           (or (beads-wisp--get wisp 'issue-type) "")
           (beads-wisp--format-phase wisp)
           (beads-wisp--format-status wisp)
           (beads-wisp--format-time (beads-wisp--get wisp 'created-at))
           (beads-wisp--format-time (beads-wisp--get wisp 'updated-at))
           (beads-wisp--format-age-cell wisp)))))

;;; Loading

(defun beads-wisp--command ()
  "Return a `beads-command-mol-wisp-list' for the current buffer settings."
  (beads-command-mol-wisp-list
   :show-all beads-wisp--show-all
   :type-filter beads-wisp--type-filter))

(defun beads-wisp--populate (result)
  "Populate the current buffer from the parsed wisp-list RESULT."
  (setq beads-wisp--last-error nil)
  (beads-pager-set-entries
   (mapcar #'beads-wisp--entry (beads-wisp--normalize result))))

(defun beads-wisp--populate-error (err)
  "Record ERR and leave the current buffer empty."
  (setq beads-wisp--last-error (error-message-string err))
  (beads-pager-set-entries nil)
  (message "bd mol wisp list failed: %s" beads-wisp--last-error))

(defun beads-wisp-refresh ()
  "Reload the wisp list asynchronously.
Opening and refreshing the view spawns `bd' through
`beads-command-execute-async' so a remote (TRAMP) store is never
blocked on a synchronous round trip."
  (interactive)
  (let ((buffer (current-buffer)))
    (beads-command-execute-async
     (beads-wisp--command)
     (lambda (result)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (beads-wisp--populate result))))
     (lambda (err)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (beads-wisp--populate-error err))))
     :cache-key (list 'beads-wisp beads-wisp--show-all beads-wisp--type-filter))))

;;; Point and marks

(defun beads-wisp--current-id ()
  "Return the wisp id at point, or nil."
  (tabulated-list-get-id))

(defun beads-wisp--targets ()
  "Return the wisp ids to act on.
Marked ids win; otherwise the single row at point."
  (or (reverse beads-wisp--marked)
      (when-let* ((id (beads-wisp--current-id))) (list id))))

(defun beads-wisp-mark ()
  "Mark the wisp at point for a batch lifecycle action."
  (interactive)
  (when-let* ((id (beads-wisp--current-id)))
    (unless (member id beads-wisp--marked)
      (push id beads-wisp--marked))
    (tabulated-list-put-tag ">" t)))

(defun beads-wisp-unmark ()
  "Unmark the wisp at point."
  (interactive)
  (when-let* ((id (beads-wisp--current-id)))
    (setq beads-wisp--marked (delete id beads-wisp--marked))
    (tabulated-list-put-tag " " t)))

(defun beads-wisp-unmark-all ()
  "Unmark every wisp in the current buffer."
  (interactive)
  (setq beads-wisp--marked nil)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (when (beads-wisp--current-id)
        (tabulated-list-put-tag " "))
      (forward-line 1))))

;;; Navigation and view toggles

(defun beads-wisp-next ()
  "Move to the next wisp."
  (interactive)
  (forward-line 1))

(defun beads-wisp-previous ()
  "Move to the previous wisp."
  (interactive)
  (forward-line -1))

(defun beads-wisp-quit ()
  "Bury the wisp list buffer."
  (interactive)
  (quit-window t))

(defun beads-wisp-toggle-all ()
  "Toggle whether closed wisps are included (`--all')."
  (interactive)
  (setq beads-wisp--show-all (not beads-wisp--show-all))
  (beads-wisp-refresh)
  (message "Wisps: %s" (if beads-wisp--show-all "all" "active only")))

(defun beads-wisp-set-type ()
  "Set or clear the `--type' filter for the wisp list."
  (interactive)
  (setq beads-wisp--type-filter
        (let ((value (read-string "Wisp type (blank to clear): "
                                  beads-wisp--type-filter)))
          (unless (string-empty-p value) value)))
  (beads-wisp-refresh)
  (message "Wisp type filter: %s"
           (or beads-wisp--type-filter "any")))

;;; Open root

(defun beads-wisp-open-root ()
  "Open the molecule root view for the wisp at point.
Uses `beads-molecule-open' when `beads-molecule.el' is available and
otherwise runs `bd mol show' in a terminal buffer."
  (interactive)
  (if-let* ((id (beads-wisp--current-id)))
      (if (fboundp 'beads-molecule-open)
          (beads-molecule-open id)
        (beads-command-execute-interactive
         (beads-command-mol-show :mol-id id)))
    (user-error "No wisp at point")))

;;; Squash

(defun beads-wisp-squash ()
  "Squash the wisp at point into a persistent digest.
Prompts for an optional agent-provided summary and a keep-children
toggle, then confirms before running `bd mol squash'."
  (interactive)
  (if-let* ((id (beads-wisp--current-id)))
      (let* ((summary (read-string "Summary (optional): "))
             (keep (yes-or-no-p "Keep ephemeral children after squash? "))
             (cmd (beads-command-mol-squash
                   :mol-id id
                   :keep-children (and keep t)
                   :summary (unless (string-empty-p summary) summary)
                   :json nil)))
        (when (yes-or-no-p (format "Squash %s into a digest? " id))
          (condition-case err
              (progn
                (beads-command-execute cmd)
                (message "Squashed %s" id))
            (error (message "Squash %s failed: %s" id
                            (error-message-string err))))
          (beads-wisp-refresh)))
    (user-error "No wisp at point")))

;;; Burn

(defun beads-wisp-burn (&optional no-confirm)
  "Burn the wisp(s) at point, or all marked wisps.
A dry-run preview is shown first; unless NO-CONFIRM is non-nil
\(interactively a prefix argument), the wisp id (or `yes' for a
batch) must be typed to proceed.  Burning is permanent and leaves no
digest."
  (interactive "P")
  (let ((ids (beads-wisp--targets)))
    (if (null ids)
        (user-error "No wisp at point")
      (let* ((expected (if (= 1 (length ids)) (car ids) "yes"))
             (preview (mapconcat
                       (lambda (id)
                         (condition-case err
                             (beads-command-execute
                              (beads-command-mol-burn
                               :mol-id id :dry-run t :json nil))
                           (error (error-message-string err))))
                       ids
                       "\n")))
        (when (or no-confirm
                  (beads-wisp--typed-confirm
                   (format "Permanently delete %d wisp(s) and children (no digest):\n%s\nType \"%s\" to confirm: "
                           (length ids) preview expected)
                   expected))
          (dolist (id ids)
            (condition-case err
                (beads-command-execute
                 (beads-command-mol-burn
                  :mol-id id :force t :json nil))
              (error (message "Burn %s failed: %s" id
                              (error-message-string err)))))
          (setq beads-wisp--marked nil)
          (beads-wisp-refresh)
          (message "Burned %d wisp(s)" (length ids)))))))

(defun beads-wisp--typed-confirm (prompt expected)
  "Read PROMPT and return non-nil only when the answer equals EXPECTED."
  (let ((answer (read-string prompt)))
    (if (string= answer expected)
        t
      (message "Aborted (expected %S)" expected)
      nil)))

;;; Purge

(defun beads-wisp--purge-summary (result)
  "Return a human summary string for a purge RESULT payload."
  (cond
   ((stringp result) result)
   ((listp result)
    (or (alist-get 'message result)
        (format "%s bead(s) would be purged"
                (or (alist-get 'purged_count result)
                    (alist-get 'purge_count result)
                    0))))
   (t "purge preview unavailable")))

(defun beads-wisp-purge (&optional no-confirm)
  "Purge closed ephemeral beads.
Prompts for `--older-than' and `--pattern', shows the dry-run result,
and asks for confirmation before applying `--force'.  With a prefix
argument NO-CONFIRM, skip the typed confirmation."
  (interactive "P")
  (let* ((older (read-string "Older than (blank = all closed, e.g. 7d): "))
         (pattern (read-string "ID pattern (blank = any, e.g. *-wisp-*): "))
         (base (append (unless (string-empty-p older) (list :older-than older))
                       (unless (string-empty-p pattern) (list :pattern pattern))
                       (list :json t)))
         (preview (beads-command-execute
                   (apply #'beads-command-purge
                          (append base (list :dry-run t))))))
    (message "Purge preview: %s" (beads-wisp--purge-summary preview))
    (when (or no-confirm
              (beads-wisp--typed-confirm "Type \"yes\" to purge: " "yes"))
      (beads-command-execute
       (apply #'beads-command-purge
              (append base (list :force t))))
      (beads-wisp-refresh)
      (message "Purge complete"))))

;;; Mode

(defvar beads-wisp-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    ;; Navigation
    (keymap-set map "n" #'beads-wisp-next)
    (keymap-set map "p" #'beads-wisp-previous)
    (keymap-set map "RET" #'beads-wisp-open-root)
    ;; Marks
    (keymap-set map "m" #'beads-wisp-mark)
    (keymap-set map "u" #'beads-wisp-unmark)
    (keymap-set map "U" #'beads-wisp-unmark-all)
    ;; Lifecycle
    (keymap-set map "s" #'beads-wisp-squash)
    (keymap-set map "b" #'beads-wisp-burn)
    (keymap-set map "B" #'beads-wisp-burn)
    (keymap-set map "P" #'beads-wisp-purge)
    ;; View
    (keymap-set map "a" #'beads-wisp-toggle-all)
    (keymap-set map "t" #'beads-wisp-set-type)
    (keymap-set map "g" #'beads-wisp-refresh)
    (keymap-set map "q" #'beads-wisp-quit)
    (beads-mode--install-navigation-keys map)
    map)
  "Keymap for `beads-wisp-list-mode'.")

(define-derived-mode beads-wisp-list-mode tabulated-list-mode "Beads-Wisps"
  "Major mode for listing bd wisps (ephemeral molecules).

\\{beads-wisp-list-mode-map}"
  (setq tabulated-list-format
        (vector (list "Id" beads-wisp-id-width t)
                (list "Type" beads-wisp-type-width t)
                (list "Phase" beads-wisp-phase-width t)
                (list "Status" beads-wisp-status-width t)
                (list "Started" beads-wisp-started-width t)
                (list "Updated" beads-wisp-updated-width t)
                (list "Age" beads-wisp-age-width t)))
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key nil)
  (tabulated-list-init-header)
  (hl-line-mode 1)
  (beads-pager-mode 1))

;;;###autoload
(defun beads-wisp-list ()
  "Display wisps (ephemeral molecules) in a tabulated list.
Opens a buffer listing every wisp with its type, phase, status,
start and update times, and age.  RET opens the root molecule view;
`s' squashes, `b' burns, `P' purges."
  (interactive)
  (let ((buffer (get-buffer-create (beads-buffer-utility "wisps"))))
    (with-current-buffer buffer
      (beads-wisp-list-mode)
      (beads-wisp-refresh))
    (pop-to-buffer buffer)))

(provide 'beads-wisp)
;;; beads-wisp.el ends here
