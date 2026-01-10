;;; beads-epic-status.el --- Epic status display for beads.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools, project, issues

;;; Commentary:

;; This module provides epic status display functionality for beads.el.
;; It implements an interactive buffer that shows all epics with their
;; completion progress and allows expanding/collapsing child issues.
;;
;; Key features:
;; - Display all epics with progress information
;; - SPC to expand/collapse child issues inline
;; - TAB to navigate between epics and issues
;; - RET to show epic or issue details
;; - Dynamic fetching of children only when expanded
;; - Visual hierarchy with indentation
;; - Refresh support (g key)
;; - Color-coded progress percentages
;;
;; Usage:
;;   M-x beads-epic
;;
;; Keybindings:
;;   SPC       - Toggle expand/collapse epic at point
;;   TAB       - Next epic or issue
;;   S-TAB     - Previous epic or issue
;;   RET       - Show epic or issue at point using beads-show
;;   g         - Refresh epic status
;;   q         - Quit window

;;; Code:

(require 'beads)
(require 'beads-buffer-name)
(require 'beads-types)
(require 'cl-lib)

;; Forward declarations
(declare-function beads-list-issues "beads-list")

;;; Customization

(defgroup beads-epic-status nil
  "Epic status display settings."
  :group 'beads
  :prefix "beads-epic-status-")

(defface beads-epic-status-bullet-face
  '((t :inherit default))
  "Face for epic bullet (○)."
  :group 'beads-epic-status)

(defface beads-epic-status-id-face
  '((t :inherit font-lock-constant-face :weight bold))
  "Face for epic ID."
  :group 'beads-epic-status)

(defface beads-epic-status-title-face
  '((t :inherit default))
  "Face for epic title."
  :group 'beads-epic-status)

(defface beads-epic-status-progress-low-face
  '((t :inherit error))
  "Face for progress percentage < 33%."
  :group 'beads-epic-status)

(defface beads-epic-status-progress-medium-face
  '((t :inherit warning))
  "Face for progress percentage 33-66%."
  :group 'beads-epic-status)

(defface beads-epic-status-progress-high-face
  '((t :inherit success))
  "Face for progress percentage > 66%."
  :group 'beads-epic-status)

(defface beads-epic-status-child-id-face
  '((t :inherit font-lock-variable-name-face))
  "Face for child issue ID."
  :group 'beads-epic-status)

(defface beads-epic-status-status-open-face
  '((t :inherit default))
  "Face for open status."
  :group 'beads-epic-status)

(defface beads-epic-status-status-closed-face
  '((t :inherit shadow))
  "Face for closed status."
  :group 'beads-epic-status)

(defface beads-epic-status-status-in-progress-face
  '((t :inherit warning))
  "Face for in_progress status."
  :group 'beads-epic-status)

(defface beads-epic-status-status-blocked-face
  '((t :inherit error))
  "Face for blocked status."
  :group 'beads-epic-status)

(defface beads-epic-status-eligible-face
  '((t :inherit success :weight bold))
  "Face for eligible for closure checkmark."
  :group 'beads-epic-status)

;;; Variables

(defvar-local beads-epic-status--epics nil
  "List of beads-epic-status objects for current buffer.")

(defvar-local beads-epic-status--expanded nil
  "Alist mapping epic-id to expanded state and cached children.
Format: ((epic-id . (expanded-p . children)) ...)")

(defvar-local beads-epic-status--point-pos nil
  "Saved point position for refresh.")

;;; Core Functions

;;;###autoload
(defun beads-epic ()
  "Display epic status with progress in interactive buffer."
  (interactive)
  (beads-check-executable)
  (let* ((caller-dir default-directory)
         (epics (beads-command-epic-status!))
         (buf-name (beads-buffer-name-utility "epic-status"))
         (buffer (get-buffer-create buf-name)))
    (with-current-buffer buffer
      (setq default-directory caller-dir)
      (let ((old-expanded beads-epic-status--expanded))
        (beads-epic-status-mode)
        (setq beads-epic-status--epics epics)
        ;; Preserve expanded state on refresh
        (when old-expanded
          (setq beads-epic-status--expanded old-expanded))
        ;; Initialize expanded state for new epics
        (dolist (epic-status epics)
          (let ((epic-id (oref (oref epic-status epic) id)))
            (unless (assoc epic-id beads-epic-status--expanded)
              (push (list epic-id nil) beads-epic-status--expanded))))
        (beads-epic-status--render)))
    (pop-to-buffer buffer)))

(defun beads--parse-epic-statuses (result)
  "Parse RESULT from bd epic status into beads-epic-status objects."
  (mapcar #'beads-epic-status-from-json (append result nil)))

(defun beads-epic-status--render ()
  "Render the epic status buffer."
  (let ((inhibit-read-only t)
        (old-point (point)))
    (erase-buffer)
    (insert (propertize "Beads Epic Status\n\n"
                        'face 'header-line))
    (insert "Press SPC to expand/collapse, TAB/S-TAB to navigate, ")
    (insert "RET to show details, g to refresh, q to quit\n\n")
    (if (null beads-epic-status--epics)
        (insert (propertize "No epics found\n"
                            'face 'shadow))
      (dolist (epic-status beads-epic-status--epics)
        (beads-epic-status--render-epic epic-status)))
    (goto-char (or beads-epic-status--point-pos old-point))
    ;; Move to first epic if point is in header
    (when (< (point) (point-min))
      (goto-char (point-min)))
    (beads-epic-status--move-to-epic-line)))

(defun beads-epic-status--render-epic (epic-status)
  "Render a single EPIC-STATUS with progress and optional sub-issues."
  (let* ((epic (oref epic-status epic))
         (epic-id (oref epic id))
         (title (oref epic title))
         (total (oref epic-status total-children))
         (closed (oref epic-status closed-children))
         (eligible (oref epic-status eligible-for-close))
         (percent (if (> total 0)
                      (/ (* closed 100) total)
                    0))
         (expanded-entry (assoc epic-id beads-epic-status--expanded))
         (expanded-p (and expanded-entry (nth 1 expanded-entry)))
         (children (and expanded-entry (nthcdr 2 expanded-entry)))
         (bullet (if eligible "✓" "○"))
         (bullet-face (if eligible
                          'beads-epic-status-eligible-face
                        'beads-epic-status-bullet-face)))
    ;; Epic line with text property for epic-id
    (let ((start (point)))
      (insert (propertize bullet 'face bullet-face))
      (insert " ")
      (insert (propertize epic-id 'face 'beads-epic-status-id-face))
      (insert " ")
      (insert (propertize title 'face 'beads-epic-status-title-face))
      (insert "\n")
      ;; Add epic-id property to entire line
      (put-text-property start (point) 'epic-id epic-id))
    ;; Progress line
    (insert "   Progress: ")
    (let ((progress-str (format "%d/%d children closed (%d%%)"
                                closed total percent))
          (progress-face (cond
                          ((< percent 33) 'beads-epic-status-progress-low-face)
                          ((< percent 67)
                           'beads-epic-status-progress-medium-face)
                          (t 'beads-epic-status-progress-high-face))))
      (insert (propertize progress-str 'face progress-face)))
    (insert "\n")
    ;; Eligible for closure message
    (when eligible
      (insert (propertize "   Eligible for closure\n"
                          'face 'beads-epic-status-eligible-face)))
    ;; Sub-issues if expanded
    (when (and expanded-p children)
      (dolist (child children)
        (beads-epic-status--render-child child)))
    (insert "\n")))

(defun beads-epic-status--render-child (child)
  "Render a CHILD issue under an epic."
  (let* ((id (oref child id))
         (status (oref child status))
         (priority (oref child priority))
         (type (oref child issue-type))
         (title (oref child title))
         (status-face (pcase status
                        ("open" 'beads-epic-status-status-open-face)
                        ("closed" 'beads-epic-status-status-closed-face)
                        ("in_progress"
                         'beads-epic-status-status-in-progress-face)
                        ("blocked" 'beads-epic-status-status-blocked-face)
                        (_ 'default))))
    (let ((start (point)))
      (insert "     ")
      (insert (propertize id 'face 'beads-epic-status-child-id-face))
      (insert " ")
      (insert (format "[P%d]" priority))
      (insert " ")
      (insert (format "[%s]" (or type "task")))
      (insert " ")
      (insert (propertize status 'face status-face))
      (insert " - ")
      (insert title)
      (insert "\n")
      ;; Add issue-id property to entire child line
      (put-text-property start (point) 'issue-id id))))

;;; Interactive Commands

(defun beads-epic-status-toggle-expand ()
  "Toggle expansion of epic at point."
  (interactive)
  (when-let* ((epic-id (get-text-property (point) 'epic-id)))
    (let ((expanded-entry (assoc epic-id beads-epic-status--expanded)))
      (if (nth 1 expanded-entry)
          ;; Collapse
          (setf (nth 1 expanded-entry) nil)
        ;; Expand - fetch children if not cached
        (unless (nthcdr 2 expanded-entry)
          (message "Loading children for %s..." epic-id)
          (let ((children (beads-epic-status--fetch-children epic-id)))
            (setcdr expanded-entry (cons t children))))
        (setf (nth 1 expanded-entry) t)))
    (setq beads-epic-status--point-pos (point))
    (beads-epic-status--render)))

(defun beads-epic-status--fetch-children (epic-id)
  "Fetch all child issues for EPIC-ID using dependency tree."
  (let* ((tree-data (beads-command-dep-tree! :issue-id epic-id :reverse t)))
    ;; Tree nodes are issues with extra fields (depth, parent_id)
    ;; Filter out the epic itself (depth=0) and keep only children
    (cl-remove-if #'null
                  (mapcar (lambda (node)
                            (let ((depth (or (alist-get 'depth node) 0))
                                  (node-id (alist-get 'id node)))
                              ;; Skip the epic itself, only return children
                              (when (and (> depth 0)
                                         (not (equal node-id epic-id)))
                                (beads-issue-from-json node))))
                          (append tree-data nil)))))

(defun beads-epic-status-show-children ()
  "Show children of epic at point in beads-list buffer."
  (interactive)
  (if-let* ((epic-id (get-text-property (point) 'epic-id)))
      (progn
        (require 'beads-list)
        (beads-list-issues
         (beads-issue-filter
          :deps (format "parent-child:%s" epic-id))))
    (user-error "No epic at point")))

(defun beads-epic-status-next ()
  "Move to next epic."
  (interactive)
  (let ((start (point)))
    (forward-line 1)
    (while (and (not (eobp))
                (not (get-text-property (point) 'epic-id)))
      (forward-line 1))
    (when (eobp)
      (goto-char start)
      (message "No next epic"))))

(defun beads-epic-status-previous ()
  "Move to previous epic."
  (interactive)
  (let ((start (point)))
    (forward-line -1)
    (while (and (not (bobp))
                (not (get-text-property (point) 'epic-id)))
      (forward-line -1))
    (when (and (bobp) (not (get-text-property (point) 'epic-id)))
      (goto-char start)
      (message "No previous epic"))))

(defun beads-epic-status-refresh ()
  "Refresh epic status display."
  (interactive)
  (setq beads-epic-status--point-pos (point))
  (beads-epic))

(defun beads-epic-status-show-at-point ()
  "Show details of epic or issue at point using beads-show."
  (interactive)
  (let ((issue-id (or (get-text-property (point) 'epic-id)
                      (get-text-property (point) 'issue-id))))
    (if issue-id
        (progn
          (require 'beads-show)
          (beads-show issue-id))
      (user-error "No issue at point"))))

(defun beads-epic-status-next-item ()
  "Move to next epic or issue."
  (interactive)
  (let ((start (point)))
    (forward-line 1)
    (while (and (not (eobp))
                (not (or (get-text-property (point) 'epic-id)
                         (get-text-property (point) 'issue-id))))
      (forward-line 1))
    (when (eobp)
      (goto-char start)
      (message "No next item"))))

(defun beads-epic-status-previous-item ()
  "Move to previous epic or issue."
  (interactive)
  (let ((start (point)))
    (forward-line -1)
    (while (and (not (bobp))
                (not (or (get-text-property (point) 'epic-id)
                         (get-text-property (point) 'issue-id))))
      (forward-line -1))
    (when (and (bobp)
               (not (or (get-text-property (point) 'epic-id)
                        (get-text-property (point) 'issue-id))))
      (goto-char start)
      (message "No previous item"))))

(defun beads-epic-status--move-to-epic-line ()
  "Move point to nearest epic line if not already on one."
  (unless (get-text-property (point) 'epic-id)
    (let ((start (point)))
      ;; Try moving forward first
      (while (and (not (eobp))
                  (not (get-text-property (point) 'epic-id)))
        (forward-line 1))
      ;; If we reached end, try moving backward from start
      (when (and (eobp) (not (get-text-property (point) 'epic-id)))
        (goto-char start)
        (while (and (not (bobp))
                    (not (get-text-property (point) 'epic-id)))
          (forward-line -1))))))

;;; Mode Definition

(defvar beads-epic-status-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "SPC") #'beads-epic-status-toggle-expand)
    (define-key map (kbd "TAB") #'beads-epic-status-next-item)
    (define-key map (kbd "<backtab>") #'beads-epic-status-previous-item)
    (define-key map (kbd "S-TAB") #'beads-epic-status-previous-item)
    (define-key map (kbd "RET") #'beads-epic-status-show-at-point)
    (define-key map (kbd "n") #'beads-epic-status-next)
    (define-key map (kbd "p") #'beads-epic-status-previous)
    (define-key map (kbd "g") #'beads-epic-status-refresh)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for beads-epic-status-mode.")

(define-derived-mode beads-epic-status-mode special-mode
  "Beads-Epic-Status"
  "Major mode for displaying Beads epic status.

Key bindings:
\\{beads-epic-status-mode-map}"
  (setq truncate-lines t)
  (setq buffer-read-only t))

(provide 'beads-epic-status)

;;; beads-epic-status.el ends here
