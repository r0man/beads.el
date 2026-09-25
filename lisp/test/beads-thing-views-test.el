;;; beads-thing-views-test.el --- Thing motion in beads.el views -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Commentary:

;; dashboard-v3 §5.4 / §12 B2 adoption: beads.el's own views mark their
;; things and bind TAB/S-TAB/SPC through `beads-thing'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'beads-thing)
(require 'beads-command-epic)
(require 'beads-command-show)
(require 'beads-command-list)
(require 'beads-dashboard)

(defconst beads-thing-views-test--epic
  '((epic . ((id . "bd-e1") (title . "Epic one") (status . "open")
             (priority . 1) (issue_type . "epic")))
    (total_children . 2) (closed_children . 1)
    (eligible_for_close . :json-false))
  "Epic status JSON.")

(defconst beads-thing-views-test--epic-2
  '((epic . ((id . "bd-e2") (title . "Epic two") (status . "open")
             (priority . 1) (issue_type . "epic")))
    (total_children . 1) (closed_children . 0)
    (eligible_for_close . :json-false))
  "Second epic status JSON.")

(ert-deftest beads-thing-views-test-epic-status ()
  "Epic lines are fold things: TAB walks epics, SPC expands one."
  :tags '(:unit)
  (with-temp-buffer
    (beads-epic-status-mode)
    (setq beads-epic-status--epics
          (list (beads-epic-status-from-json beads-thing-views-test--epic)
                (beads-epic-status-from-json beads-thing-views-test--epic-2))
          beads-epic-status--expanded (list (list "bd-e1" nil)
                                            (list "bd-e2" nil)))
    (beads-epic-status--render)
    (goto-char (point-min))
    (beads-thing-forward)
    (should (equal (get-text-property (point) 'epic-id) "bd-e1"))
    (should (eq (beads-thing-kind (beads-thing-at)) 'fold))
    (beads-thing-forward)
    (should (equal (get-text-property (point) 'epic-id) "bd-e2"))
    (beads-thing-backward)
    (cl-letf (((symbol-function 'beads-epic-status--fetch-children)
               (lambda (_id) nil)))
      (beads-thing-toggle))
    (should (nth 1 (assoc "bd-e1" beads-epic-status--expanded)))))

(ert-deftest beads-thing-views-test-show-stamps-headings-and-buttons ()
  "Show buffers mark section headings and buttons as things."
  :tags '(:unit)
  (with-temp-buffer
    (beads-show-mode)
    (let ((inhibit-read-only t))
      (insert "bd-1: Title\n\nDESCRIPTION\n\nSome text\n\nDEPENDS ON\n\n  ")
      (insert-text-button "bd-2" 'action #'ignore)
      (insert "\n"))
    (beads-show--stamp-things)
    (let ((things (mapcar (lambda (p)
                            (buffer-substring-no-properties
                             p (next-single-property-change
                                p 'beads-thing nil (point-max))))
                          (beads-thing-starts))))
      (should (equal things '("DESCRIPTION" "DEPENDS ON" "bd-2"))))
    ;; SPC on a heading folds its section.
    (goto-char (point-min))
    (beads-thing-forward)
    (beads-thing-toggle)
    (should (invisible-p (save-excursion (forward-line 2) (point))))
    (beads-thing-toggle)
    (should-not (invisible-p (save-excursion (forward-line 2) (point))))))

(ert-deftest beads-thing-views-test-keymaps ()
  "Every beads.el view binds the §5.4 keys."
  :tags '(:unit)
  (dolist (map (list beads-show-mode-map beads-list-mode-map
                     beads-epic-status-mode-map beads-dashboard-mode-map
                     beads-section-mode-map))
    (should (eq (keymap-lookup map "TAB") #'beads-thing-forward))
    (should (eq (keymap-lookup map "<tab>") #'beads-thing-forward))
    (should (eq (keymap-lookup map "<backtab>") #'beads-thing-backward))
    (should (eq (keymap-lookup map "S-<tab>") #'beads-thing-backward))
    (should (eq (keymap-lookup map "SPC") #'beads-thing-toggle))
    (should (eq (keymap-lookup map "DEL") #'undefined))))

(ert-deftest beads-thing-views-test-list-spc-toggles-detail ()
  "SPC on a list row runs the list's detail-window handler."
  :tags '(:unit)
  (with-temp-buffer
    (beads-list-mode)
    (setq tabulated-list-entries
          (list (list "bd-1" (vector "bd-1" "task" "open" "P2" "" "One" "" ""))))
    (tabulated-list-print)
    (goto-char (point-min))
    (let (called)
      (cl-letf (((symbol-function 'beads-list--toggle-detail)
                 (lambda (&optional thing) (setq called thing) t)))
        (beads-thing-toggle))
      (should (equal called "bd-1")))))

(ert-deftest beads-thing-views-test-dashboard-header-is-section-thing ()
  "Dashboard section headers render as section things (SPC pushes them)."
  :tags '(:unit)
  (let ((label (vui-vnode-button-label
                (beads-dashboard--section-header
                 "Ready" nil nil 3 #'ignore 'ready))))
    (should (eq (beads-thing-kind (get-text-property 0 'beads-thing label))
                'section))))

(provide 'beads-thing-views-test)
;;; beads-thing-views-test.el ends here
