;;; beads-command-activity-test.el --- Tests for beads-command-activity -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for beads-command-activity command classes.

;;; Code:

(require 'ert)
(require 'beads-command-activity)

;;; Unit Tests: beads-command-activity command-line

(ert-deftest beads-command-activity-test-command-line-basic ()
  "Unit test: activity builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-activity))
         (args (beads-command-line cmd)))
    (should (member "activity" args))))

(ert-deftest beads-command-activity-test-command-line-follow ()
  "Unit test: activity includes --follow option."
  :tags '(:unit)
  (let* ((cmd (beads-command-activity :follow t))
         (args (beads-command-line cmd)))
    (should (member "--follow" args))))

(ert-deftest beads-command-activity-test-command-line-limit ()
  "Unit test: activity includes --limit option."
  :tags '(:unit)
  (let* ((cmd (beads-command-activity :limit 50))
         (args (beads-command-line cmd)))
    (should (member "--limit" args))
    (should (member "50" args))))

(ert-deftest beads-command-activity-test-command-line-mol ()
  "Unit test: activity includes --mol option."
  :tags '(:unit)
  (let* ((cmd (beads-command-activity :mol "bd-"))
         (args (beads-command-line cmd)))
    (should (member "--mol" args))
    (should (member "bd-" args))))

(ert-deftest beads-command-activity-test-command-line-since ()
  "Unit test: activity includes --since option."
  :tags '(:unit)
  (let* ((cmd (beads-command-activity :since "5m"))
         (args (beads-command-line cmd)))
    (should (member "--since" args))
    (should (member "5m" args))))

(ert-deftest beads-command-activity-test-command-line-type ()
  "Unit test: activity includes --type option."
  :tags '(:unit)
  (let* ((cmd (beads-command-activity :event-type "create"))
         (args (beads-command-line cmd)))
    (should (member "--type" args))
    (should (member "create" args))))

(ert-deftest beads-command-activity-test-command-line-town ()
  "Unit test: activity includes --town option."
  :tags '(:unit)
  (let* ((cmd (beads-command-activity :town t))
         (args (beads-command-line cmd)))
    (should (member "--town" args))))

(ert-deftest beads-command-activity-test-command-line-interval ()
  "Unit test: activity includes --interval option."
  :tags '(:unit)
  (let* ((cmd (beads-command-activity :interval "1s"))
         (args (beads-command-line cmd)))
    (should (member "--interval" args))
    (should (member "1s" args))))

(provide 'beads-command-activity-test)
;;; beads-command-activity-test.el ends here
