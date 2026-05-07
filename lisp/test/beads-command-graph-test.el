;;; beads-command-graph-test.el --- Tests for beads-command-graph -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for beads-command-graph command classes.

;;; Code:

(require 'ert)
(require 'beads-command-graph)

;;; Unit Tests: beads-command-graph command-line

(ert-deftest beads-command-graph-test-command-line-basic ()
  "Unit test: graph builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-graph))
         (args (beads-command-line cmd)))
    (should (member "graph" args))))

(ert-deftest beads-command-graph-test-command-line-all ()
  "Unit test: graph includes --all option."
  :tags '(:unit)
  (let* ((cmd (beads-command-graph :all t))
         (args (beads-command-line cmd)))
    (should (member "--all" args))))

(ert-deftest beads-command-graph-test-command-line-box ()
  "Unit test: graph includes --box option."
  :tags '(:unit)
  (let* ((cmd (beads-command-graph :box t))
         (args (beads-command-line cmd)))
    (should (member "--box" args))))

(ert-deftest beads-command-graph-test-command-line-compact ()
  "Unit test: graph includes --compact option."
  :tags '(:unit)
  (let* ((cmd (beads-command-graph :compact t))
         (args (beads-command-line cmd)))
    (should (member "--compact" args))))

;;; Graph helper function tests

(ert-deftest beads-command-graph-test-issue-color-open ()
  "Test issue color for open status."
  :tags '(:unit)
  (let ((issue (beads-issue :id "bd-1" :title "Test"
                            :status "open" :priority 1
                            :issue-type "task")))
    (should (equal (beads-graph--issue-color issue) "lightblue"))))

(ert-deftest beads-command-graph-test-issue-color-in-progress ()
  "Test issue color for in_progress status."
  :tags '(:unit)
  (let ((issue (beads-issue :id "bd-1" :title "Test"
                            :status "in_progress" :priority 1
                            :issue-type "task")))
    (should (equal (beads-graph--issue-color issue) "yellow"))))

(ert-deftest beads-command-graph-test-issue-color-blocked ()
  "Test issue color for blocked status."
  :tags '(:unit)
  (let ((issue (beads-issue :id "bd-1" :title "Test"
                            :status "blocked" :priority 1
                            :issue-type "task")))
    (should (equal (beads-graph--issue-color issue) "red"))))

(ert-deftest beads-command-graph-test-issue-color-closed ()
  "Test issue color for closed status."
  :tags '(:unit)
  (let ((issue (beads-issue :id "bd-1" :title "Test"
                            :status "closed" :priority 1
                            :issue-type "task")))
    (should (equal (beads-graph--issue-color issue) "lightgray"))))

(ert-deftest beads-command-graph-test-issue-color-unknown ()
  "Test issue color for unknown status."
  :tags '(:unit)
  (let ((issue (beads-issue :id "bd-1" :title "Test"
                            :status "unknown" :priority 1
                            :issue-type "task")))
    (should (equal (beads-graph--issue-color issue) "white"))))

(ert-deftest beads-command-graph-test-issue-shape-epic ()
  "Test issue shape for epic type."
  :tags '(:unit)
  (let ((issue (beads-issue :id "bd-1" :title "Test"
                            :status "open" :priority 1
                            :issue-type "epic")))
    (should (equal (beads-graph--issue-shape issue) "box3d"))))

(ert-deftest beads-command-graph-test-issue-shape-feature ()
  "Test issue shape for feature type."
  :tags '(:unit)
  (let ((issue (beads-issue :id "bd-1" :title "Test"
                            :status "open" :priority 1
                            :issue-type "feature")))
    (should (equal (beads-graph--issue-shape issue) "box"))))

(ert-deftest beads-command-graph-test-issue-shape-bug ()
  "Test issue shape for bug type."
  :tags '(:unit)
  (let ((issue (beads-issue :id "bd-1" :title "Test"
                            :status "open" :priority 1
                            :issue-type "bug")))
    (should (equal (beads-graph--issue-shape issue) "octagon"))))

(ert-deftest beads-command-graph-test-issue-shape-task ()
  "Test issue shape for task type."
  :tags '(:unit)
  (let ((issue (beads-issue :id "bd-1" :title "Test"
                            :status "open" :priority 1
                            :issue-type "task")))
    (should (equal (beads-graph--issue-shape issue) "ellipse"))))

(ert-deftest beads-command-graph-test-issue-shape-chore ()
  "Test issue shape for chore type."
  :tags '(:unit)
  (let ((issue (beads-issue :id "bd-1" :title "Test"
                            :status "open" :priority 1
                            :issue-type "chore")))
    (should (equal (beads-graph--issue-shape issue) "note"))))

(ert-deftest beads-command-graph-test-issue-label-short-title ()
  "Test issue label with short title."
  :tags '(:unit)
  (let ((issue (beads-issue :id "bd-1" :title "Short"
                            :status "open" :priority 2
                            :issue-type "task")))
    (let ((label (beads-graph--issue-label issue)))
      (should (string-match-p "bd-1" label))
      (should (string-match-p "Short" label))
      (should (string-match-p "P2" label))
      (should (string-match-p "open" label)))))

(ert-deftest beads-command-graph-test-issue-label-long-title ()
  "Test issue label truncates long titles."
  :tags '(:unit)
  (let* ((long-title (make-string 50 ?x))
         (issue (beads-issue :id "bd-1" :title long-title
                             :status "open" :priority 1
                             :issue-type "task")))
    (let ((label (beads-graph--issue-label issue)))
      (should (string-match-p "\\.\\.\\." label)))))

(ert-deftest beads-command-graph-test-filter-issue-no-filters ()
  "Test filter-issue passes everything with no filters."
  :tags '(:unit)
  (let ((issue (beads-issue :id "bd-1" :title "Test"
                            :status "open" :priority 1
                            :issue-type "task"))
        (beads-graph--filter-status nil)
        (beads-graph--filter-priority nil)
        (beads-graph--filter-type nil))
    (should (beads-graph--filter-issue issue))))

(ert-deftest beads-command-graph-test-filter-issue-status-match ()
  "Test filter-issue with matching status filter."
  :tags '(:unit)
  (let ((issue (beads-issue :id "bd-1" :title "Test"
                            :status "open" :priority 1
                            :issue-type "task"))
        (beads-graph--filter-status "open")
        (beads-graph--filter-priority nil)
        (beads-graph--filter-type nil))
    (should (beads-graph--filter-issue issue))))

(ert-deftest beads-command-graph-test-filter-issue-status-no-match ()
  "Test filter-issue rejects non-matching status."
  :tags '(:unit)
  (let ((issue (beads-issue :id "bd-1" :title "Test"
                            :status "open" :priority 1
                            :issue-type "task"))
        (beads-graph--filter-status "closed")
        (beads-graph--filter-priority nil)
        (beads-graph--filter-type nil))
    (should-not (beads-graph--filter-issue issue))))

(ert-deftest beads-command-graph-test-filter-issue-priority-match ()
  "Test filter-issue with matching priority filter."
  :tags '(:unit)
  (let ((issue (beads-issue :id "bd-1" :title "Test"
                            :status "open" :priority 2
                            :issue-type "task"))
        (beads-graph--filter-status nil)
        (beads-graph--filter-priority 2)
        (beads-graph--filter-type nil))
    (should (beads-graph--filter-issue issue))))

(ert-deftest beads-command-graph-test-filter-issue-type-match ()
  "Test filter-issue with matching type filter."
  :tags '(:unit)
  (let ((issue (beads-issue :id "bd-1" :title "Test"
                            :status "open" :priority 1
                            :issue-type "bug"))
        (beads-graph--filter-status nil)
        (beads-graph--filter-priority nil)
        (beads-graph--filter-type "bug"))
    (should (beads-graph--filter-issue issue))))

(ert-deftest beads-command-graph-test-generate-dot-basic ()
  "Test generate-dot produces valid DOT output."
  :tags '(:unit)
  (let ((issues (list (beads-issue :id "bd-1" :title "Issue 1"
                                   :status "open" :priority 1
                                   :issue-type "task")
                      (beads-issue :id "bd-2" :title "Issue 2"
                                   :status "closed" :priority 2
                                   :issue-type "bug")))
        (deps (list (list :from "bd-1" :to "bd-2" :type "blocks")))
        (beads-graph--filter-status nil)
        (beads-graph--filter-priority nil)
        (beads-graph--filter-type nil))
    (let ((dot (beads-graph--generate-dot issues deps)))
      (should (string-match-p "digraph beads" dot))
      (should (string-match-p "bd-1" dot))
      (should (string-match-p "bd-2" dot))
      (should (string-match-p "->" dot)))))

(ert-deftest beads-command-graph-test-generate-dot-empty ()
  "Test generate-dot with no issues."
  :tags '(:unit)
  (let ((beads-graph--filter-status nil)
        (beads-graph--filter-priority nil)
        (beads-graph--filter-type nil))
    (let ((dot (beads-graph--generate-dot nil nil)))
      (should (string-match-p "digraph beads" dot))
      (should (string-match-p "}" dot)))))

(ert-deftest beads-command-graph-test-check-dot-missing ()
  "Test check-dot signals error when dot is not found."
  :tags '(:unit)
  (cl-letf (((symbol-function 'executable-find)
             (lambda (_cmd) nil)))
    (should-error (beads-graph--check-dot) :type 'user-error)))

(ert-deftest beads-command-graph-test-connected-ids-self-only ()
  "An issue with no edges traces to a singleton component."
  :tags '(:unit)
  (let ((deps (list (list :from "bd-9" :to "bd-10" :type "blocks"))))
    (should (equal (beads-graph--connected-ids "bd-1" deps) '("bd-1")))))

(ert-deftest beads-command-graph-test-connected-ids-traverses-both-directions ()
  "Connected component follows edges as undirected (ancestors + descendants)."
  :tags '(:unit)
  ;; bd-1 -> bd-2 -> bd-3, plus an unrelated bd-9 -> bd-10.
  (let* ((deps (list (list :from "bd-1" :to "bd-2" :type "blocks")
                     (list :from "bd-2" :to "bd-3" :type "blocks")
                     (list :from "bd-9" :to "bd-10" :type "blocks")))
         (from-leaf (sort (beads-graph--connected-ids "bd-3" deps) #'string<))
         (from-root (sort (beads-graph--connected-ids "bd-1" deps) #'string<))
         (from-mid (sort (beads-graph--connected-ids "bd-2" deps) #'string<)))
    (should (equal from-leaf '("bd-1" "bd-2" "bd-3")))
    (should (equal from-root '("bd-1" "bd-2" "bd-3")))
    (should (equal from-mid '("bd-1" "bd-2" "bd-3")))
    (should-not (member "bd-9" from-mid))
    (should-not (member "bd-10" from-mid))))

(ert-deftest beads-command-graph-test-mode-defined ()
  "Test graph mode is defined."
  :tags '(:unit)
  (should (fboundp 'beads-graph-mode)))

(ert-deftest beads-command-graph-test-mode-keymap ()
  "Test graph mode has expected keybindings."
  :tags '(:unit)
  (should (keymapp beads-graph-mode-map))
  (should (lookup-key beads-graph-mode-map "g"))
  (should (lookup-key beads-graph-mode-map "q"))
  (should (lookup-key beads-graph-mode-map "f"))
  (should (lookup-key beads-graph-mode-map "e")))

(provide 'beads-command-graph-test)
;;; beads-command-graph-test.el ends here
