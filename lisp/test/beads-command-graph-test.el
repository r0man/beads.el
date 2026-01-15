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

(provide 'beads-command-graph-test)
;;; beads-command-graph-test.el ends here
