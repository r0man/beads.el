;;; beads-graph-test.el --- Tests for beads-graph -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; ERT tests for beads-graph.el graphviz integration and dependency
;; graph visualization.

;;; Code:

(require 'ert)
(require 'beads)
(require 'beads-buffer)
(require 'beads-graph)

;;; Test Helpers

(defun beads-graph-test--get-graph-buffer ()
  "Get the graph buffer for the current project."
  (beads-buffer-name-utility "graph"))

(defun beads-graph-test--find-and-kill-graph-buffers ()
  "Kill all graph buffers for cleanup."
  (dolist (buf (beads-buffer-name-find-utility-buffers nil "graph"))
    (when (buffer-live-p buf)
      (kill-buffer buf))))

;;; Test Fixtures

(defvar beads-graph-test--sample-issues
  '(((id . "bd-1")
     (title . "First Issue")
     (status . "open")
     (priority . 1)
     (issue-type . "feature"))
    ((id . "bd-2")
     (title . "Second Issue")
     (status . "in_progress")
     (priority . 2)
     (issue-type . "bug"))
    ((id . "bd-3")
     (title . "Third Issue")
     (status . "closed")
     (priority . 3)
     (issue-type . "task")))
  "Sample issues for graph tests.")

(defvar beads-graph-test--sample-deps
  '((:from "bd-1" :to "bd-2" :type "blocks")
    (:from "bd-2" :to "bd-3" :type "related"))
  "Sample dependencies for graph tests.")

;;; Test Utilities

(defun beads-graph-test--with-filters (status priority type &rest body)
  "Execute BODY with graph filters set."
  (let ((beads-graph--filter-status status)
        (beads-graph--filter-priority priority)
        (beads-graph--filter-type type))
    (eval (cons 'progn body))))

;;; Tests for Color Assignment

(ert-deftest beads-graph-test-issue-color-open ()
  "Test color assignment for open issues."
  (let ((issue '((status . "open"))))
    (should (string= (beads-graph--issue-color issue) "lightblue"))))

(ert-deftest beads-graph-test-issue-color-in-progress ()
  "Test color assignment for in_progress issues."
  (let ((issue '((status . "in_progress"))))
    (should (string= (beads-graph--issue-color issue) "yellow"))))

(ert-deftest beads-graph-test-issue-color-blocked ()
  "Test color assignment for blocked issues."
  (let ((issue '((status . "blocked"))))
    (should (string= (beads-graph--issue-color issue) "red"))))

(ert-deftest beads-graph-test-issue-color-closed ()
  "Test color assignment for closed issues."
  (let ((issue '((status . "closed"))))
    (should (string= (beads-graph--issue-color issue) "lightgray"))))

(ert-deftest beads-graph-test-issue-color-unknown ()
  "Test color assignment for unknown status."
  (let ((issue '((status . "unknown"))))
    (should (string= (beads-graph--issue-color issue) "white"))))

;;; Tests for Shape Assignment

(ert-deftest beads-graph-test-issue-shape-epic ()
  "Test shape assignment for epic issues."
  (let ((issue '((issue-type . "epic"))))
    (should (string= (beads-graph--issue-shape issue) "box3d"))))

(ert-deftest beads-graph-test-issue-shape-feature ()
  "Test shape assignment for feature issues."
  (let ((issue '((issue-type . "feature"))))
    (should (string= (beads-graph--issue-shape issue) "box"))))

(ert-deftest beads-graph-test-issue-shape-bug ()
  "Test shape assignment for bug issues."
  (let ((issue '((issue-type . "bug"))))
    (should (string= (beads-graph--issue-shape issue) "octagon"))))

(ert-deftest beads-graph-test-issue-shape-task ()
  "Test shape assignment for task issues."
  (let ((issue '((issue-type . "task"))))
    (should (string= (beads-graph--issue-shape issue) "ellipse"))))

(ert-deftest beads-graph-test-issue-shape-chore ()
  "Test shape assignment for chore issues."
  (let ((issue '((issue-type . "chore"))))
    (should (string= (beads-graph--issue-shape issue) "note"))))

;;; Tests for Label Generation

(ert-deftest beads-graph-test-issue-label-short-title ()
  "Test label generation with short title."
  (let ((issue '((id . "bd-1")
                 (title . "Short")
                 (status . "open")
                 (priority . 1))))
    (let ((label (beads-graph--issue-label issue)))
      (should (string-match-p "bd-1" label))
      (should (string-match-p "Short" label))
      (should (string-match-p "P1" label))
      (should (string-match-p "open" label)))))

(ert-deftest beads-graph-test-issue-label-long-title ()
  "Test label generation with long title (truncation)."
  (let ((issue `((id . "bd-1")
                 (title . ,(make-string 50 ?x))
                 (status . "open")
                 (priority . 2))))
    (let ((label (beads-graph--issue-label issue)))
      (should (string-match-p "\\.\\.\\." label))
      (should (< (length (car (split-string label "\\\\n"))) 40)))))

;;; Tests for Filtering

(ert-deftest beads-graph-test-filter-issue-no-filters ()
  "Test filtering with no filters applied."
  (let ((beads-graph--filter-status nil)
        (beads-graph--filter-priority nil)
        (beads-graph--filter-type nil)
        (issue '((status . "open")
                 (priority . 1)
                 (issue-type . "bug"))))
    (should (beads-graph--filter-issue issue))))

(ert-deftest beads-graph-test-filter-issue-by-status ()
  "Test filtering by status."
  (let ((beads-graph--filter-status "open")
        (beads-graph--filter-priority nil)
        (beads-graph--filter-type nil))
    (should (beads-graph--filter-issue '((status . "open")
                                         (priority . 1))))
    (should-not (beads-graph--filter-issue '((status . "closed")
                                             (priority . 1))))))

(ert-deftest beads-graph-test-filter-issue-by-priority ()
  "Test filtering by priority."
  (let ((beads-graph--filter-status nil)
        (beads-graph--filter-priority 1)
        (beads-graph--filter-type nil))
    (should (beads-graph--filter-issue '((status . "open")
                                         (priority . 1))))
    (should-not (beads-graph--filter-issue '((status . "open")
                                             (priority . 2))))))

(ert-deftest beads-graph-test-filter-issue-by-type ()
  "Test filtering by issue type."
  (let ((beads-graph--filter-status nil)
        (beads-graph--filter-priority nil)
        (beads-graph--filter-type "bug"))
    (should (beads-graph--filter-issue '((issue-type . "bug"))))
    (should-not (beads-graph--filter-issue '((issue-type . "feature"))))))

(ert-deftest beads-graph-test-filter-issue-multiple-filters ()
  "Test filtering with multiple filters."
  (let ((beads-graph--filter-status "open")
        (beads-graph--filter-priority 1)
        (beads-graph--filter-type "bug"))
    (should (beads-graph--filter-issue '((status . "open")
                                         (priority . 1)
                                         (issue-type . "bug"))))
    (should-not (beads-graph--filter-issue '((status . "closed")
                                             (priority . 1)
                                             (issue-type . "bug"))))))

;;; Tests for DOT Generation

(ert-deftest beads-graph-test-generate-dot-basic ()
  "Test basic DOT generation."
  (let ((beads-graph--filter-status nil)
        (beads-graph--filter-priority nil)
        (beads-graph--filter-type nil))
    (let ((dot (beads-graph--generate-dot
                beads-graph-test--sample-issues
                beads-graph-test--sample-deps)))
      (should (stringp dot))
      (should (string-match-p "digraph beads" dot))
      (should (string-match-p "bd-1" dot))
      (should (string-match-p "bd-2" dot))
      (should (string-match-p "bd-3" dot)))))

(ert-deftest beads-graph-test-generate-dot-with-filters ()
  "Test DOT generation respects filters."
  (let ((beads-graph--filter-status "open")
        (beads-graph--filter-priority nil)
        (beads-graph--filter-type nil))
    (let ((dot (beads-graph--generate-dot
                beads-graph-test--sample-issues
                beads-graph-test--sample-deps)))
      (should (string-match-p "bd-1" dot))
      (should-not (string-match-p "bd-3" dot)))))

(ert-deftest beads-graph-test-generate-dot-includes-edges ()
  "Test DOT generation includes edges."
  (let ((beads-graph--filter-status nil)
        (beads-graph--filter-priority nil)
        (beads-graph--filter-type nil))
    (let ((dot (beads-graph--generate-dot
                beads-graph-test--sample-issues
                beads-graph-test--sample-deps)))
      (should (string-match-p "bd-1.*->.*bd-2" dot))
      (should (string-match-p "blocks" dot)))))

;;; Tests for Check Functions

(ert-deftest beads-graph-test-check-dot-missing ()
  "Test dot executable check fails when missing."
  (let ((beads-graph-dot-executable "nonexistent-dot-executable"))
    (should-error (beads-graph--check-dot) :type 'user-error)))

;;; Tests for Mode

(ert-deftest beads-graph-test-mode-defined ()
  "Test that beads-graph-mode is defined."
  (should (fboundp 'beads-graph-mode)))

(ert-deftest beads-graph-test-mode-keymap ()
  "Test that beads-graph-mode has a keymap."
  (should (keymapp beads-graph-mode-map)))

(ert-deftest beads-graph-test-mode-keybindings ()
  "Test beads-graph-mode key bindings."
  (should (eq (lookup-key beads-graph-mode-map (kbd "g"))
              'beads-graph-refresh))
  (should (eq (lookup-key beads-graph-mode-map (kbd "q"))
              'quit-window))
  (should (eq (lookup-key beads-graph-mode-map (kbd "f"))
              'beads-graph-filter))
  (should (eq (lookup-key beads-graph-mode-map (kbd "e"))
              'beads-graph-export)))

;;; Tests for Commands

(ert-deftest beads-graph-test-commands-defined ()
  "Test that all public commands are defined."
  (should (fboundp 'beads-graph-all))
  (should (fboundp 'beads-graph-issue))
  (should (fboundp 'beads-graph-refresh))
  (should (fboundp 'beads-graph-export))
  (should (fboundp 'beads-graph-filter)))

;;; ============================================================
;;; Integration Tests
;;; ============================================================

(ert-deftest beads-graph-test-graph-all-exists ()
  "Integration test: Verify beads-graph-all command exists."
  :tags '(integration)
  (should (fboundp 'beads-graph-all)))

(ert-deftest beads-graph-test-graph-issue-exists ()
  "Integration test: Verify beads-graph-issue command exists."
  :tags '(integration)
  (should (fboundp 'beads-graph-issue)))

(ert-deftest beads-graph-test-keybinding-g-refresh ()
  "Integration test: Verify g keybinding for refresh."
  :tags '(integration)
  (with-temp-buffer
    (beads-graph-mode)
    (let ((binding (lookup-key beads-graph-mode-map (kbd "g"))))
      (should (eq binding 'beads-graph-refresh)))))

(ert-deftest beads-graph-test-keybinding-f-filter ()
  "Integration test: Verify f keybinding for filter."
  :tags '(integration)
  (with-temp-buffer
    (beads-graph-mode)
    (let ((binding (lookup-key beads-graph-mode-map (kbd "f"))))
      (should (eq binding 'beads-graph-filter)))))

(ert-deftest beads-graph-test-keybinding-e-export ()
  "Integration test: Verify e keybinding for export."
  :tags '(integration)
  (with-temp-buffer
    (beads-graph-mode)
    (let ((binding (lookup-key beads-graph-mode-map (kbd "e"))))
      (should (eq binding 'beads-graph-export)))))

;;; ============================================================
;;; Tests for Render and Display
;;; ============================================================

(ert-deftest beads-graph-test-render-dot-success ()
  "Test successful DOT rendering."
  (skip-unless (executable-find "dot"))
  (let ((dot-string "digraph test { a -> b; }"))
    (let ((image-file (beads-graph--render-dot dot-string "svg")))
      (unwind-protect
          (progn
            (should (stringp image-file))
            (should (file-exists-p image-file))
            (should (string-suffix-p ".svg" image-file)))
        (when (file-exists-p image-file)
          (delete-file image-file))))))

(ert-deftest beads-graph-test-render-dot-png ()
  "Test DOT rendering to PNG format."
  (skip-unless (executable-find "dot"))
  (let ((dot-string "digraph test { a -> b; }"))
    (let ((image-file (beads-graph--render-dot dot-string "png")))
      (unwind-protect
          (progn
            (should (stringp image-file))
            (should (string-suffix-p ".png" image-file)))
        (when (file-exists-p image-file)
          (delete-file image-file))))))

(ert-deftest beads-graph-test-display-image-creates-buffer ()
  "Test that display-image creates the graph buffer."
  (skip-unless (executable-find "dot"))
  ;; Skip if Emacs doesn't have SVG support (e.g., Emacs 28.2 on CI)
  (skip-unless (image-type-available-p 'svg))
  (let ((dot-string "digraph test { a -> b; }")
        (image-file nil))
    (unwind-protect
        (progn
          (setq image-file (beads-graph--render-dot dot-string "svg"))
          (beads-graph--display-image image-file)
          (should (get-buffer (beads-graph-test--get-graph-buffer)))
          (with-current-buffer (beads-graph-test--get-graph-buffer)
            (should (eq major-mode 'beads-graph-mode))))
      (beads-graph-test--find-and-kill-graph-buffers)
      (when (and image-file (file-exists-p image-file))
        (delete-file image-file)))))

;;; ============================================================
;;; Tests for Refresh
;;; ============================================================

(ert-deftest beads-graph-test-refresh-calls-all-without-root ()
  "Test that refresh calls graph-all when no root issue."
  (let ((beads-graph--root-issue nil)
        (graph-all-called nil))
    (cl-letf (((symbol-function 'beads-graph-all)
               (lambda () (setq graph-all-called t))))
      (beads-graph-refresh)
      (should graph-all-called))))

(ert-deftest beads-graph-test-refresh-calls-issue-with-root ()
  "Test that refresh calls graph-issue when root issue is set."
  (let ((beads-graph--root-issue "bd-42")
        (graph-issue-called nil)
        (called-with nil))
    (cl-letf (((symbol-function 'beads-graph-issue)
               (lambda (id)
                 (setq graph-issue-called t)
                 (setq called-with id))))
      (beads-graph-refresh)
      (should graph-issue-called)
      (should (equal called-with "bd-42")))))

;;; ============================================================
;;; Tests for Filter Command
;;; ============================================================

(ert-deftest beads-graph-test-filter-clear-all ()
  "Test clearing all filters."
  (let ((beads-graph--filter-status "open")
        (beads-graph--filter-priority 1)
        (beads-graph--filter-type "bug"))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) "clear-all"))
              ((symbol-function 'beads-graph-refresh) #'ignore))
      (beads-graph-filter)
      (should-not beads-graph--filter-status)
      (should-not beads-graph--filter-priority)
      (should-not beads-graph--filter-type))))

(ert-deftest beads-graph-test-filter-by-status ()
  "Test filtering by status."
  (let ((beads-graph--filter-status nil)
        (call-count 0))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (prompt &rest _)
                 (setq call-count (1+ call-count))
                 (if (= call-count 1) "status" "in_progress")))
              ((symbol-function 'beads-graph-refresh) #'ignore))
      (beads-graph-filter)
      (should (equal beads-graph--filter-status "in_progress")))))

(ert-deftest beads-graph-test-filter-by-priority ()
  "Test filtering by priority."
  (let ((beads-graph--filter-priority nil)
        (call-count 0))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (prompt &rest _)
                 (setq call-count (1+ call-count))
                 (if (= call-count 1) "priority" "2")))
              ((symbol-function 'beads-graph-refresh) #'ignore))
      (beads-graph-filter)
      (should (equal beads-graph--filter-priority 2)))))

(ert-deftest beads-graph-test-filter-by-type ()
  "Test filtering by type."
  (let ((beads-graph--filter-type nil)
        (call-count 0))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (prompt &rest _)
                 (setq call-count (1+ call-count))
                 (if (= call-count 1) "type" "feature")))
              ((symbol-function 'beads-graph-refresh) #'ignore))
      (beads-graph-filter)
      (should (equal beads-graph--filter-type "feature")))))

(ert-deftest beads-graph-test-filter-clear-status ()
  "Test clearing status filter with empty input."
  (let ((beads-graph--filter-status "open")
        (call-count 0))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (prompt &rest _)
                 (setq call-count (1+ call-count))
                 (if (= call-count 1) "status" "")))
              ((symbol-function 'beads-graph-refresh) #'ignore))
      (beads-graph-filter)
      (should-not beads-graph--filter-status))))

;;; ============================================================
;;; Tests for Export
;;; ============================================================

(ert-deftest beads-graph-test-export-dot-format ()
  "Test exporting to DOT format."
  (skip-unless (executable-find "dot"))
  (let ((temp-file (make-temp-file "beads-graph-export-test-" nil ".dot")))
    (unwind-protect
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (&rest _) "dot"))
                  ((symbol-function 'read-file-name)
                   (lambda (&rest _) temp-file))
                  ((symbol-function 'beads-check-executable) #'ignore)
                  ((symbol-function 'beads-command-list!)
                   (lambda () beads-graph-test--sample-issues))
                  ((symbol-function 'beads-graph--get-dependencies)
                   (lambda () beads-graph-test--sample-deps)))
          (let ((beads-graph--filter-status nil)
                (beads-graph--filter-priority nil)
                (beads-graph--filter-type nil))
            (beads-graph-export)
            (should (file-exists-p temp-file))
            (with-temp-buffer
              (insert-file-contents temp-file)
              (should (string-match-p "digraph" (buffer-string))))))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(ert-deftest beads-graph-test-export-svg-format ()
  "Test exporting to SVG format."
  (skip-unless (executable-find "dot"))
  (let ((temp-file (make-temp-file "beads-graph-export-test-" nil ".svg")))
    (unwind-protect
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (&rest _) "svg"))
                  ((symbol-function 'read-file-name)
                   (lambda (&rest _) temp-file))
                  ((symbol-function 'beads-check-executable) #'ignore)
                  ((symbol-function 'beads-command-list!)
                   (lambda () beads-graph-test--sample-issues))
                  ((symbol-function 'beads-graph--get-dependencies)
                   (lambda () beads-graph-test--sample-deps)))
          (let ((beads-graph--filter-status nil)
                (beads-graph--filter-priority nil)
                (beads-graph--filter-type nil))
            (beads-graph-export)
            (should (file-exists-p temp-file))))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

;;; ============================================================
;;; Tests for Graph All and Graph Issue
;;; ============================================================

(ert-deftest beads-graph-test-graph-all-mocked ()
  "Test graph-all with mocked dependencies."
  (skip-unless (executable-find "dot"))
  (let ((beads-graph--root-issue nil))
    (cl-letf (((symbol-function 'beads-check-executable) #'ignore)
              ((symbol-function 'beads-command-list!)
               (lambda () beads-graph-test--sample-issues))
              ((symbol-function 'beads-graph--get-dependencies)
               (lambda () beads-graph-test--sample-deps))
              ((symbol-function 'beads-graph--display-image) #'ignore))
      (let ((beads-graph--filter-status nil)
            (beads-graph--filter-priority nil)
            (beads-graph--filter-type nil))
        (beads-graph-all)
        (should-not beads-graph--root-issue)))))

(ert-deftest beads-graph-test-graph-issue-sets-root ()
  "Test graph-issue sets the root issue."
  (skip-unless (executable-find "dot"))
  (let ((beads-graph--root-issue nil))
    (cl-letf (((symbol-function 'beads-check-executable) #'ignore)
              ((symbol-function 'beads-command-list!)
               (lambda () beads-graph-test--sample-issues))
              ((symbol-function 'beads-graph--get-dependencies)
               (lambda () beads-graph-test--sample-deps))
              ((symbol-function 'beads-graph--display-image) #'ignore))
      (let ((beads-graph--filter-status nil)
            (beads-graph--filter-priority nil)
            (beads-graph--filter-type nil))
        (beads-graph-issue "bd-1")
        (should (equal beads-graph--root-issue "bd-1"))))))

;;; ============================================================
;;; Tests for DOT Edge Styles
;;; ============================================================

(ert-deftest beads-graph-test-dot-edge-style-blocks ()
  "Test that 'blocks' dependency uses correct style."
  (let ((beads-graph--filter-status nil)
        (beads-graph--filter-priority nil)
        (beads-graph--filter-type nil)
        (deps '((:from "bd-1" :to "bd-2" :type "blocks"))))
    (let ((dot (beads-graph--generate-dot beads-graph-test--sample-issues deps)))
      (should (string-match-p "style=solid" dot))
      (should (string-match-p "color=\"red\"" dot)))))

(ert-deftest beads-graph-test-dot-edge-style-related ()
  "Test that 'related' dependency uses correct style."
  (let ((beads-graph--filter-status nil)
        (beads-graph--filter-priority nil)
        (beads-graph--filter-type nil)
        (deps '((:from "bd-1" :to "bd-2" :type "related"))))
    (let ((dot (beads-graph--generate-dot beads-graph-test--sample-issues deps)))
      (should (string-match-p "style=dashed" dot))
      (should (string-match-p "color=\"blue\"" dot)))))

(ert-deftest beads-graph-test-dot-edge-style-parent-child ()
  "Test that 'parent-child' dependency uses correct style."
  (let ((beads-graph--filter-status nil)
        (beads-graph--filter-priority nil)
        (beads-graph--filter-type nil)
        (deps '((:from "bd-1" :to "bd-2" :type "parent-child"))))
    (let ((dot (beads-graph--generate-dot beads-graph-test--sample-issues deps)))
      (should (string-match-p "style=bold" dot))
      (should (string-match-p "color=\"green\"" dot)))))

(ert-deftest beads-graph-test-dot-edge-style-discovered-from ()
  "Test that 'discovered-from' dependency uses correct style."
  (let ((beads-graph--filter-status nil)
        (beads-graph--filter-priority nil)
        (beads-graph--filter-type nil)
        (deps '((:from "bd-1" :to "bd-2" :type "discovered-from"))))
    (let ((dot (beads-graph--generate-dot beads-graph-test--sample-issues deps)))
      (should (string-match-p "style=dotted" dot))
      (should (string-match-p "color=\"gray\"" dot)))))

;;; ============================================================
;;; Customization Tests
;;; ============================================================

(ert-deftest beads-graph-test-customization-dot-executable ()
  "Test that dot executable is customizable."
  (should (boundp 'beads-graph-dot-executable))
  (should (stringp beads-graph-dot-executable)))

(ert-deftest beads-graph-test-customization-default-format ()
  "Test that default format is customizable."
  (should (boundp 'beads-graph-default-format))
  (should (stringp beads-graph-default-format)))

(ert-deftest beads-graph-test-customization-layout ()
  "Test that layout is customizable."
  (should (boundp 'beads-graph-layout))
  (should (stringp beads-graph-layout)))

(ert-deftest beads-graph-test-customization-group ()
  "Test that beads-graph customization group exists."
  (should (get 'beads-graph 'group-documentation)))

;;; ============================================================
;;; Mode Tests
;;; ============================================================

(ert-deftest beads-graph-test-mode-read-only ()
  "Test that beads-graph-mode sets buffer read-only."
  (with-temp-buffer
    (beads-graph-mode)
    (should buffer-read-only)))

(ert-deftest beads-graph-test-mode-inherits-special ()
  "Test that beads-graph-mode inherits from special-mode."
  (with-temp-buffer
    (beads-graph-mode)
    (should (derived-mode-p 'special-mode))))

;;; Dependency Collection Tests

(ert-deftest beads-graph-test-get-dependencies-function-exists ()
  "Test that get-dependencies function exists."
  (should (fboundp 'beads-graph--get-dependencies)))

;;; Public Functions Tests

(ert-deftest beads-graph-test-all-function-exists ()
  "Test that beads-graph-all exists."
  (should (fboundp 'beads-graph-all)))

(ert-deftest beads-graph-test-issue-function-exists ()
  "Test that beads-graph-issue exists."
  (should (fboundp 'beads-graph-issue)))

(ert-deftest beads-graph-test-filter-function-exists ()
  "Test that beads-graph-filter exists."
  (should (fboundp 'beads-graph-filter)))

(ert-deftest beads-graph-test-export-function-exists ()
  "Test that beads-graph-export exists."
  (should (fboundp 'beads-graph-export)))

(ert-deftest beads-graph-test-refresh-function-exists ()
  "Test that beads-graph-refresh exists."
  (should (fboundp 'beads-graph-refresh)))

(provide 'beads-graph-test)
;;; beads-graph-test.el ends here
