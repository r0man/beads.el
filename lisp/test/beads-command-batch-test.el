;;; beads-command-batch-test.el --- Tests for beads-command-batch -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for beads-command-batch command class.

;;; Code:

(require 'ert)
(require 'beads-command-batch)

(ert-deftest beads-command-batch-test-class-exists ()
  "Unit test: beads-command-batch class is defined."
  :tags '(:unit)
  (should (cl-find-class 'beads-command-batch)))

(ert-deftest beads-command-batch-test-command-line-basic ()
  "Unit test: batch builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-batch))
         (args (beads-command-line cmd)))
    (should (member "batch" args))))

(ert-deftest beads-command-batch-test-command-line-dry-run ()
  "Unit test: batch --dry-run flag."
  :tags '(:unit)
  (let* ((cmd (beads-command-batch :dry-run t))
         (args (beads-command-line cmd)))
    (should (member "--dry-run" args))))

(ert-deftest beads-command-batch-test-command-line-file ()
  "Unit test: batch --file flag."
  :tags '(:unit)
  (let* ((cmd (beads-command-batch :file "/tmp/cmds.txt"))
         (args (beads-command-line cmd)))
    (should (member "--file" args))
    (should (member "/tmp/cmds.txt" args))))

(ert-deftest beads-command-batch-test-command-line-message ()
  "Unit test: batch --message flag."
  :tags '(:unit)
  (let* ((cmd (beads-command-batch :message "bd: bulk import"))
         (args (beads-command-line cmd)))
    (should (member "--message" args))
    (should (member "bd: bulk import" args))))

(ert-deftest beads-command-batch-test-transient-defined ()
  "Unit test: beads-batch transient is defined."
  :tags '(:unit)
  (should (fboundp 'beads-batch)))

(ert-deftest beads-command-batch-test-result-type-declared ()
  "Unit test: beads-command-batch declares beads-batch-result as :result.
Wiring the result type lets the parser surface bd's structured
status/error/rollback information instead of returning raw JSON."
  :tags '(:unit)
  (should (eq (get 'beads-command-batch 'beads-result)
              'beads-batch-result)))

(ert-deftest beads-command-batch-test-result-parse-success ()
  "Unit test: beads-batch-result parses bd batch --json success shape.
{status:\"ok\", operations:N, results:[...]} -> populated slots."
  :tags '(:unit)
  (let* ((json '((status . "ok")
                 (operations . 1)
                 (results . [((line . 1)
                              (op . "close")
                              (target . "tb-yt0"))])
                 (schema_version . 1)))
         (result (beads-batch-result-from-json json)))
    (should (equal (oref result status) "ok"))
    (should (equal (oref result operations) 1))
    (should (equal (oref result schema-version) 1))
    (should (null (oref result error-message)))
    (should (null (oref result code)))
    (should (beads-batch-result-success-p result))
    (let ((ops (oref result results)))
      (should (= 1 (length ops)))
      (should (equal (oref (car ops) line) 1))
      (should (equal (oref (car ops) op) "close"))
      (should (equal (oref (car ops) target) "tb-yt0")))))

(ert-deftest beads-command-batch-test-result-parse-error ()
  "Unit test: beads-batch-result parses bd batch --json error shape.
{code:\"batch_error\", error:\"...\"} -> success-p nil and populated
error-message; transaction is rolled back."
  :tags '(:unit)
  (let* ((json '((code . "batch_error")
                 (error . "line 1 (close bogus-id done): issue not found: bogus-id")
                 (schema_version . 1)))
         (result (beads-batch-result-from-json json)))
    (should (equal (oref result code) "batch_error"))
    (should (string-match-p "line 1" (oref result error-message)))
    (should (null (oref result status)))
    (should-not (beads-batch-result-success-p result))))

(ert-deftest beads-command-batch-test-result-parse-dry-run ()
  "Unit test: beads-batch-result parses bd batch --json --dry-run shape.
{dry_run:true, operations:N} -> dry-run flag set, treated as success."
  :tags '(:unit)
  (let* ((json '((dry_run . t)
                 (operations . 2)
                 (schema_version . 1)))
         (result (beads-batch-result-from-json json)))
    (should (oref result dry-run))
    (should (equal (oref result operations) 2))
    (should (null (oref result error-message)))
    (should (beads-batch-result-success-p result))))

(provide 'beads-command-batch-test)
;;; beads-command-batch-test.el ends here
