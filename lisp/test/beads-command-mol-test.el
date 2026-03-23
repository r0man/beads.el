;;; beads-command-mol-test.el --- Tests for beads-command-mol -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for beads-command-mol command classes.

;;; Code:

(require 'ert)
(require 'beads-command-mol)

;;; Unit Tests: beads-command-mol-show command-line

(ert-deftest beads-command-mol-show-test-command-line-basic ()
  "Unit test: mol show builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-mol-show :mol-id "mol-1"))
         (args (beads-command-line cmd)))
    (should (member "mol" args))
    (should (member "show" args))
    (should (member "mol-1" args))))

;;; Unit Tests: beads-command-mol-pour command-line

(ert-deftest beads-command-mol-pour-test-command-line-basic ()
  "Unit test: mol pour builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-mol-pour :proto-id "proto-1"))
         (args (beads-command-line cmd)))
    (should (member "mol" args))
    (should (member "pour" args))
    (should (member "proto-1" args))))

(ert-deftest beads-command-mol-pour-test-command-line-var ()
  "Unit test: mol pour includes --var option."
  :tags '(:unit)
  (let* ((cmd (beads-command-mol-pour :proto-id "proto-1" :var '("key=value")))
         (args (beads-command-line cmd)))
    (should (member "--var" args))
    (should (member "key=value" args))))

;;; Unit Tests: beads-command-mol-wisp command-line

(ert-deftest beads-command-mol-wisp-test-command-line-basic ()
  "Unit test: mol wisp builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-mol-wisp :proto-id "proto-1"))
         (args (beads-command-line cmd)))
    (should (member "mol" args))
    (should (member "wisp" args))
    (should (member "proto-1" args))))

;;; Unit Tests: beads-command-mol-wisp-create command-line

(ert-deftest beads-command-mol-wisp-create-test-command-line-basic ()
  "Unit test: mol wisp create builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-mol-wisp-create :proto-id "proto-1"))
         (args (beads-command-line cmd)))
    (should (member "mol" args))
    (should (member "wisp" args))
    (should (member "create" args))
    (should (member "proto-1" args))))

(ert-deftest beads-command-mol-wisp-create-test-command-line-dry-run ()
  "Unit test: mol wisp create includes --dry-run flag."
  :tags '(:unit)
  (let* ((cmd (beads-command-mol-wisp-create :proto-id "proto-1"
                                             :dry-run t))
         (args (beads-command-line cmd)))
    (should (member "--dry-run" args))))

(ert-deftest beads-command-mol-wisp-create-test-command-line-root-only ()
  "Unit test: mol wisp create includes --root-only flag."
  :tags '(:unit)
  (let* ((cmd (beads-command-mol-wisp-create :proto-id "proto-1"
                                             :root-only t))
         (args (beads-command-line cmd)))
    (should (member "--root-only" args))))

(ert-deftest beads-command-mol-wisp-create-test-command-line-var ()
  "Unit test: mol wisp create includes --var option."
  :tags '(:unit)
  (let* ((cmd (beads-command-mol-wisp-create :proto-id "proto-1"
                                             :var '("key=value")))
         (args (beads-command-line cmd)))
    (should (member "--var" args))
    (should (member "key=value" args))))

;;; Unit Tests: beads-command-mol-wisp-list command-line

(ert-deftest beads-command-mol-wisp-list-test-command-line-basic ()
  "Unit test: mol wisp list builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-mol-wisp-list))
         (args (beads-command-line cmd)))
    (should (member "mol" args))
    (should (member "wisp" args))
    (should (member "list" args))))

(ert-deftest beads-command-mol-wisp-list-test-command-line-all ()
  "Unit test: mol wisp list includes --all flag."
  :tags '(:unit)
  (let* ((cmd (beads-command-mol-wisp-list :show-all t))
         (args (beads-command-line cmd)))
    (should (member "--all" args))))

(ert-deftest beads-command-mol-wisp-list-test-command-line-type ()
  "Unit test: mol wisp list includes --type option."
  :tags '(:unit)
  (let* ((cmd (beads-command-mol-wisp-list :type-filter "agent"))
         (args (beads-command-line cmd)))
    (should (member "--type" args))
    (should (member "agent" args))))

;;; Unit Tests: beads-command-mol-wisp-gc command-line

(ert-deftest beads-command-mol-wisp-gc-test-command-line-basic ()
  "Unit test: mol wisp gc builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-mol-wisp-gc))
         (args (beads-command-line cmd)))
    (should (member "mol" args))
    (should (member "wisp" args))
    (should (member "gc" args))))

(ert-deftest beads-command-mol-wisp-gc-test-command-line-dry-run ()
  "Unit test: mol wisp gc includes --dry-run flag."
  :tags '(:unit)
  (let* ((cmd (beads-command-mol-wisp-gc :dry-run t))
         (args (beads-command-line cmd)))
    (should (member "--dry-run" args))))

(ert-deftest beads-command-mol-wisp-gc-test-command-line-force ()
  "Unit test: mol wisp gc includes --force flag."
  :tags '(:unit)
  (let* ((cmd (beads-command-mol-wisp-gc :force t))
         (args (beads-command-line cmd)))
    (should (member "--force" args))))

(ert-deftest beads-command-mol-wisp-gc-test-command-line-age ()
  "Unit test: mol wisp gc includes --age option."
  :tags '(:unit)
  (let* ((cmd (beads-command-mol-wisp-gc :age "30m"))
         (args (beads-command-line cmd)))
    (should (member "--age" args))
    (should (member "30m" args))))

(ert-deftest beads-command-mol-wisp-gc-test-command-line-exclude-type ()
  "Unit test: mol wisp gc includes --exclude-type option."
  :tags '(:unit)
  (let* ((cmd (beads-command-mol-wisp-gc :exclude-type '("agent")))
         (args (beads-command-line cmd)))
    (should (member "--exclude-type" args))
    (should (member "agent" args))))

;;; Unit Tests: beads-command-mol-bond command-line

(ert-deftest beads-command-mol-bond-test-command-line-basic ()
  "Unit test: mol bond builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-mol-bond :first-id "mol-1" :second-id "mol-2"))
         (args (beads-command-line cmd)))
    (should (member "mol" args))
    (should (member "bond" args))
    (should (member "mol-1" args))
    (should (member "mol-2" args))))

;;; Unit Tests: beads-command-mol-squash command-line

(ert-deftest beads-command-mol-squash-test-command-line-basic ()
  "Unit test: mol squash builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-mol-squash :mol-id "mol-1"))
         (args (beads-command-line cmd)))
    (should (member "mol" args))
    (should (member "squash" args))
    (should (member "mol-1" args))))

;;; Unit Tests: beads-command-mol-burn command-line

(ert-deftest beads-command-mol-burn-test-command-line-basic ()
  "Unit test: mol burn builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-mol-burn :mol-id "mol-1"))
         (args (beads-command-line cmd)))
    (should (member "mol" args))
    (should (member "burn" args))
    (should (member "mol-1" args))))

;;; Unit Tests: beads-command-mol-distill command-line

(ert-deftest beads-command-mol-distill-test-command-line-basic ()
  "Unit test: mol distill builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-mol-distill :epic-id "epic-1"))
         (args (beads-command-line cmd)))
    (should (member "mol" args))
    (should (member "distill" args))
    (should (member "epic-1" args))))

;;; Unit Tests: beads-command-mol-current command-line

(ert-deftest beads-command-mol-current-test-command-line-basic ()
  "Unit test: mol current builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-mol-current :mol-id "mol-1"))
         (args (beads-command-line cmd)))
    (should (member "mol" args))
    (should (member "current" args))
    (should (member "mol-1" args))))

;;; Unit Tests: beads-command-mol-progress command-line

(ert-deftest beads-command-mol-progress-test-command-line-basic ()
  "Unit test: mol progress builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-mol-progress :mol-id "mol-1"))
         (args (beads-command-line cmd)))
    (should (member "mol" args))
    (should (member "progress" args))
    (should (member "mol-1" args))))

;;; Unit Tests: beads-command-mol-ready command-line

(ert-deftest beads-command-mol-ready-test-command-line-basic ()
  "Unit test: mol ready builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-mol-ready))
         (args (beads-command-line cmd)))
    (should (member "mol" args))
    (should (member "ready" args))))

;;; Unit Tests: beads-command-mol-stale command-line

(ert-deftest beads-command-mol-stale-test-command-line-basic ()
  "Unit test: mol stale builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-mol-stale))
         (args (beads-command-line cmd)))
    (should (member "mol" args))
    (should (member "stale" args))))

(provide 'beads-command-mol-test)
;;; beads-command-mol-test.el ends here
