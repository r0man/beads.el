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
(require 'beads-graph)

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

(ert-deftest beads-graph-test-integration-mode-defined ()
  "Integration test: Verify beads-graph-mode is defined."
  :tags '(integration)
  (should (fboundp 'beads-graph-mode)))

(ert-deftest beads-graph-test-integration-graph-all-exists ()
  "Integration test: Verify beads-graph-all command exists."
  :tags '(integration)
  (should (fboundp 'beads-graph-all)))

(ert-deftest beads-graph-test-integration-graph-issue-exists ()
  "Integration test: Verify beads-graph-issue command exists."
  :tags '(integration)
  (should (fboundp 'beads-graph-issue)))

(ert-deftest beads-graph-test-integration-keybinding-g-refresh ()
  "Integration test: Verify g keybinding for refresh."
  :tags '(integration)
  (with-temp-buffer
    (beads-graph-mode)
    (let ((binding (lookup-key beads-graph-mode-map (kbd "g"))))
      (should (eq binding 'beads-graph-refresh)))))

(ert-deftest beads-graph-test-integration-keybinding-f-filter ()
  "Integration test: Verify f keybinding for filter."
  :tags '(integration)
  (with-temp-buffer
    (beads-graph-mode)
    (let ((binding (lookup-key beads-graph-mode-map (kbd "f"))))
      (should (eq binding 'beads-graph-filter)))))

(ert-deftest beads-graph-test-integration-keybinding-e-export ()
  "Integration test: Verify e keybinding for export."
  :tags '(integration)
  (with-temp-buffer
    (beads-graph-mode)
    (let ((binding (lookup-key beads-graph-mode-map (kbd "e"))))
      (should (eq binding 'beads-graph-export)))))

(provide 'beads-graph-test)
;;; beads-graph-test.el ends here
