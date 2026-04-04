;;; beads-validation-test.el --- Tests for three-level validation -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; ERT tests for the three-level validation hierarchy (D8):
;; - beads-command-validate-slot: per-slot validation via metadata
;; - beads-command-validate-slots: all-slots loop, collecting errors
;; - beads-command-validate: top-level, delegates to validate-slots

;;; Code:

(require 'ert)
(require 'beads-command)
(require 'beads-command-close)
(require 'beads-meta)

;;; ============================================================
;;; Test Classes
;;; ============================================================

;; A minimal test command with :required and :choices metadata
(beads-defcommand beads-command-test-validate (beads-command-global-options)
  ((title
    :type (or null string)
    :short-option "t"
    :required t)
   (priority
    :type (or null string integer)
    :short-option "p"
    :choices ("0" "1" "2" "3" "4"))
   (status
    :type (or null string)
    :key "s"
    :required t
    :choices ("open" "closed" "in_progress"))
   (labels
    :type (list-of string)
    :key "l"))
  :documentation "Test command for validation."
  :transient nil)

;; A command with no required slots
(beads-defcommand beads-command-test-optional (beads-command-global-options)
  ((filter
    :type (or null string)
    :short-option "f")
   (limit
    :type (or null string integer)
    :short-option "l"))
  :documentation "Test command with only optional slots."
  :transient nil)

;;; ============================================================
;;; Tests for beads-command-validate-slot
;;; ============================================================

(ert-deftest beads-validation-test-slot-required-nil ()
  "Required slot with nil value returns error."
  (let ((cmd (beads-command-test-validate)))
    (should (beads-command-validate-slot cmd 'title nil))))

(ert-deftest beads-validation-test-slot-required-empty-string ()
  "Required slot with empty string returns error."
  (let ((cmd (beads-command-test-validate)))
    (should (beads-command-validate-slot cmd 'title ""))))

(ert-deftest beads-validation-test-slot-required-empty-list ()
  "Required slot with empty list returns error."
  (let ((cmd (beads-command-test-validate)))
    (should (beads-command-validate-slot cmd 'status '()))))

(ert-deftest beads-validation-test-slot-required-valid ()
  "Required slot with non-empty value returns nil."
  (let ((cmd (beads-command-test-validate)))
    (should-not (beads-command-validate-slot cmd 'title "Hello"))))

(ert-deftest beads-validation-test-slot-choices-valid ()
  "Slot with valid choice returns nil."
  (let ((cmd (beads-command-test-validate)))
    (should-not (beads-command-validate-slot cmd 'priority "2"))))

(ert-deftest beads-validation-test-slot-choices-invalid ()
  "Slot with invalid choice returns error."
  (let ((cmd (beads-command-test-validate)))
    (should (beads-command-validate-slot cmd 'priority "5"))))

(ert-deftest beads-validation-test-slot-choices-nil ()
  "Slot with choices and nil value is ok (not required)."
  (let ((cmd (beads-command-test-validate)))
    (should-not (beads-command-validate-slot cmd 'priority nil))))

(ert-deftest beads-validation-test-slot-no-metadata ()
  "Slot without :required or :choices returns nil."
  (let ((cmd (beads-command-test-validate)))
    (should-not (beads-command-validate-slot cmd 'labels nil))))

(ert-deftest beads-validation-test-slot-required-and-choices-nil ()
  "Slot with both :required and :choices, nil value returns required error."
  (let ((cmd (beads-command-test-validate)))
    (let ((err (beads-command-validate-slot cmd 'status nil)))
      (should err)
      (should (string-match-p "required" err)))))

(ert-deftest beads-validation-test-slot-required-and-choices-invalid ()
  "Slot with both :required and :choices, invalid value returns choices error."
  (let ((cmd (beads-command-test-validate)))
    (let ((err (beads-command-validate-slot cmd 'status "bogus")))
      (should err)
      (should (string-match-p "must be one of" err)))))

(ert-deftest beads-validation-test-slot-required-and-choices-valid ()
  "Slot with both :required and :choices, valid value returns nil."
  (let ((cmd (beads-command-test-validate)))
    (should-not (beads-command-validate-slot cmd 'status "open"))))

;;; ============================================================
;;; Tests for beads-command-validate-slots
;;; ============================================================

(ert-deftest beads-validation-test-slots-all-missing ()
  "Validate-slots returns errors for all missing required slots."
  (let ((cmd (beads-command-test-validate)))
    (let ((errors (beads-command-validate-slots cmd)))
      (should (listp errors))
      ;; title and status are required, both nil
      (should (>= (length errors) 2)))))

(ert-deftest beads-validation-test-slots-partial ()
  "Validate-slots returns errors only for missing required slots."
  (let ((cmd (beads-command-test-validate :title "Hello")))
    (let ((errors (beads-command-validate-slots cmd)))
      (should (listp errors))
      ;; Only status is still missing
      (should (= (length errors) 1))
      (should (string-match-p "status" (car errors))))))

(ert-deftest beads-validation-test-slots-all-valid ()
  "Validate-slots returns nil when all required slots are set."
  (let ((cmd (beads-command-test-validate :title "Hello" :status "open")))
    (should-not (beads-command-validate-slots cmd))))

(ert-deftest beads-validation-test-slots-no-required ()
  "Validate-slots returns nil for command with no required slots."
  (let ((cmd (beads-command-test-optional)))
    (should-not (beads-command-validate-slots cmd))))

(ert-deftest beads-validation-test-slots-invalid-choice ()
  "Validate-slots catches invalid choices."
  (let ((cmd (beads-command-test-validate
              :title "Hello" :status "open" :priority "99")))
    (let ((errors (beads-command-validate-slots cmd)))
      (should (= (length errors) 1))
      (should (string-match-p "priority" (car errors))))))

;;; ============================================================
;;; Tests for beads-command-validate (top-level)
;;; ============================================================

(ert-deftest beads-validation-test-validate-delegates ()
  "Base validate delegates to validate-slots."
  (let ((cmd (beads-command-test-validate)))
    (let ((result (beads-command-validate cmd)))
      ;; Should return errors (non-nil) for missing required slots
      (should result)
      (should (listp result)))))

(ert-deftest beads-validation-test-validate-valid ()
  "Base validate returns nil when all valid."
  (let ((cmd (beads-command-test-validate :title "Hello" :status "open")))
    (should-not (beads-command-validate cmd))))

;;; ============================================================
;;; Tests for per-slot override via eql specializer
;;; ============================================================

;; Override validate-slot for title on our test command with custom rule
(cl-defmethod beads-command-validate-slot
    ((_cmd beads-command-test-validate) (_slot (eql title)) value)
  "Title must be 200 characters or fewer."
  (cond
   ((or (null value) (and (stringp value) (string-empty-p value)))
    "title is required")
   ((and (stringp value) (> (length value) 200))
    "title must be 200 characters or fewer")))

(ert-deftest beads-validation-test-slot-override-long-title ()
  "Per-slot override catches custom constraint."
  (let ((cmd (beads-command-test-validate
              :title (make-string 201 ?x) :status "open")))
    (let ((err (beads-command-validate-slot cmd 'title (oref cmd title))))
      (should err)
      (should (string-match-p "200 characters" err)))))

(ert-deftest beads-validation-test-slot-override-valid-title ()
  "Per-slot override returns nil for valid title."
  (let ((cmd (beads-command-test-validate :title "Good" :status "open")))
    (should-not (beads-command-validate-slot cmd 'title (oref cmd title)))))

;;; ============================================================
;;; Tests for execute integration
;;; ============================================================

(ert-deftest beads-validation-test-execute-signals-on-validation-error ()
  "beads-command-execute signals beads-validation-error for invalid commands."
  (let ((cmd (beads-command-test-validate)))
    (should-error (beads-command-execute cmd)
                  :type 'beads-validation-error)))

;;; ============================================================
;;; Tests against real command: beads-command-close
;;; ============================================================

(ert-deftest beads-validation-test-close-validate-slot-issue-ids ()
  "validate-slot catches missing required issue-ids on close command."
  (let ((cmd (beads-command-close)))
    (should (beads-command-validate-slot cmd 'issue-ids nil))))

(ert-deftest beads-validation-test-close-validate-slot-reason ()
  "validate-slot catches missing required reason on close command."
  (let ((cmd (beads-command-close)))
    (should (beads-command-validate-slot cmd 'reason nil))))

(ert-deftest beads-validation-test-close-validate-slot-valid ()
  "validate-slot passes for valid close command slots."
  (let ((cmd (beads-command-close :issue-ids '("bd-1") :reason "done")))
    (should-not (beads-command-validate-slot cmd 'issue-ids '("bd-1")))
    (should-not (beads-command-validate-slot cmd 'reason "done"))))

(provide 'beads-validation-test)

;;; beads-validation-test.el ends here
