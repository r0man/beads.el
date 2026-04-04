;;; beads-from-json-test.el --- Tests for beads-from-json generics -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; Tests for `beads-from-json' and `beads-coerce-json-value' generics
;; (bde-bhaw: Phase 3 of defcommand redesign).

;;; Code:

(require 'ert)
(require 'json)
(require 'beads-meta)
(require 'beads-types)
(require 'beads-command)

;;; ========================================
;;; beads-coerce-json-value tests
;;; ========================================

(ert-deftest beads-from-json-test-coerce-string ()
  "Coerce a string value for string type."
  (should (equal (beads-coerce-json-value "hello" 'string) "hello")))

(ert-deftest beads-from-json-test-coerce-string-nil ()
  "Coerce nil value for string type."
  (should (eq (beads-coerce-json-value nil 'string) nil)))

(ert-deftest beads-from-json-test-coerce-integer ()
  "Coerce an integer value for integer type."
  (should (equal (beads-coerce-json-value 42 'integer) 42)))

(ert-deftest beads-from-json-test-coerce-integer-from-string ()
  "Coerce a string value to integer type."
  (should (equal (beads-coerce-json-value "42" 'integer) 42)))

(ert-deftest beads-from-json-test-coerce-boolean-true ()
  "Coerce t to boolean."
  (should (equal (beads-coerce-json-value t 'boolean) t)))

(ert-deftest beads-from-json-test-coerce-boolean-false ()
  "Coerce :json-false to boolean."
  (should (equal (beads-coerce-json-value :json-false 'boolean) nil)))

(ert-deftest beads-from-json-test-coerce-nullable-string ()
  "Coerce value through nullable (or null string) type."
  (should (equal (beads-coerce-json-value "hi" '(or null string)) "hi")))

(ert-deftest beads-from-json-test-coerce-nullable-nil ()
  "Coerce nil through nullable type passes through."
  (should (eq (beads-coerce-json-value nil '(or null string)) nil)))

(ert-deftest beads-from-json-test-coerce-nullable-integer ()
  "Coerce integer through nullable (or null integer) type."
  (should (equal (beads-coerce-json-value 5 '(or null integer)) 5)))

(ert-deftest beads-from-json-test-coerce-list-of-string ()
  "Coerce a vector of strings to list-of string."
  (should (equal (beads-coerce-json-value ["a" "b" "c"] '(list-of string))
                 '("a" "b" "c"))))

(ert-deftest beads-from-json-test-coerce-list-of-empty ()
  "Coerce an empty vector to list-of."
  (should (equal (beads-coerce-json-value [] '(list-of string)) nil)))

(ert-deftest beads-from-json-test-coerce-list-of-nil ()
  "Coerce nil to list-of returns nil."
  (should (eq (beads-coerce-json-value nil '(list-of string)) nil)))

(ert-deftest beads-from-json-test-coerce-eieio-class ()
  "Coerce an alist to an EIEIO class type (beads-label)."
  (let ((result (beads-coerce-json-value
                 '((issue_id . "bd-42") (label . "urgent"))
                 'beads-label)))
    (should (beads-label-p result))
    (should (equal (oref result issue-id) "bd-42"))
    (should (equal (oref result label) "urgent"))))

(ert-deftest beads-from-json-test-coerce-list-of-eieio ()
  "Coerce a vector of alists to list-of EIEIO class."
  (let ((result (beads-coerce-json-value
                 [((issue_id . "bd-1") (label . "bug"))
                  ((issue_id . "bd-2") (label . "feature"))]
                 '(list-of beads-label))))
    (should (= (length result) 2))
    (should (beads-label-p (car result)))
    (should (equal (oref (car result) label) "bug"))))

(ert-deftest beads-from-json-test-coerce-unknown-type ()
  "Coerce value with unknown type passes through unchanged."
  (should (equal (beads-coerce-json-value "raw" t) "raw")))

;;; ========================================
;;; beads-from-json tests
;;; ========================================

(ert-deftest beads-from-json-test-simple-label ()
  "Parse a beads-label from JSON alist."
  (let ((result (beads-from-json 'beads-label
                                 '((issue_id . "bd-42")
                                   (label . "urgent")))))
    (should (beads-label-p result))
    (should (equal (oref result issue-id) "bd-42"))
    (should (equal (oref result label) "urgent"))))

(ert-deftest beads-from-json-test-comment ()
  "Parse a beads-comment from JSON alist."
  (let ((result (beads-from-json 'beads-comment
                                 '((id . "c1")
                                   (issue_id . "bd-42")
                                   (author . "alice")
                                   (text . "Hello")
                                   (created_at . "2026-01-01")))))
    (should (beads-comment-p result))
    (should (equal (oref result id) "c1"))
    (should (equal (oref result author) "alice"))
    (should (equal (oref result text) "Hello"))))

(ert-deftest beads-from-json-test-absent-vs-null ()
  "Absent fields use initform, explicit null gets nil."
  ;; Only provide 'id', everything else absent
  (let ((result (beads-from-json 'beads-issue
                                 '((id . "bd-42")))))
    (should (beads-issue-p result))
    (should (equal (oref result id) "bd-42"))
    ;; Absent fields get initform default (nil for these slots)
    (should (eq (oref result title) nil))))

(ert-deftest beads-from-json-test-explicit-null ()
  "Explicit JSON null (nil) is passed through."
  (let ((result (beads-from-json 'beads-issue
                                 '((id . "bd-42")
                                   (title . nil)))))
    (should (beads-issue-p result))
    (should (eq (oref result title) nil))))

(ert-deftest beads-from-json-test-unknown-keys-dropped ()
  "Unknown JSON keys are silently dropped."
  (let ((result (beads-from-json 'beads-label
                                 '((issue_id . "bd-42")
                                   (label . "bug")
                                   (unknown_field . "ignored")))))
    (should (beads-label-p result))
    (should (equal (oref result issue-id) "bd-42"))))

(ert-deftest beads-from-json-test-nested-objects ()
  "Parse an issue with nested dependency objects."
  (let ((result (beads-from-json
                 'beads-issue
                 '((id . "bd-42")
                   (title . "Test issue")
                   (dependencies . [((issue_id . "bd-42")
                                     (depends_on_id . "bd-1")
                                     (type . "blocks"))])))))
    (should (beads-issue-p result))
    (should (= (length (oref result dependencies)) 1))
    (should (beads-dependency-p (car (oref result dependencies))))
    (should (equal (oref (car (oref result dependencies)) type) "blocks"))))

(ert-deftest beads-from-json-test-nullable-integer ()
  "Parse an issue with nullable integer priority."
  (let ((result (beads-from-json 'beads-issue
                                 '((id . "bd-42")
                                   (priority . 2)))))
    (should (equal (oref result priority) 2))))

(ert-deftest beads-from-json-test-list-of-strings ()
  "Parse an issue with a list-of string labels."
  (let ((result (beads-from-json 'beads-issue
                                 '((id . "bd-42")
                                   (labels . ["bug" "urgent"])))))
    (should (equal (oref result labels) '("bug" "urgent")))))

;;; ========================================
;;; beads-command-parse :result integration
;;; ========================================

;;; Test command class for command-parse integration tests.
;;; Uses a bare class with no parse override.
(defclass beads-from-json-test--command (beads-command-global-options)
  ()
  :documentation "Test command for from-json integration tests.")

(ert-deftest beads-from-json-test-command-parse-with-result ()
  "Base beads-command-parse uses :result for auto-parsing."
  (let ((orig (get 'beads-from-json-test--command 'beads-result)))
    (unwind-protect
        (progn
          (put 'beads-from-json-test--command 'beads-result
               '(list-of beads-issue))
          (let* ((cmd (beads-from-json-test--command :json t))
                 (json-str "[{\"id\":\"bd-42\",\"title\":\"Test\",\"status\":\"closed\"}]")
                 (result (beads-command-parse cmd json-str)))
            (should (listp result))
            (should (= (length result) 1))
            (should (beads-issue-p (car result)))
            (should (equal (oref (car result) id) "bd-42"))))
      (put 'beads-from-json-test--command 'beads-result orig))))

(ert-deftest beads-from-json-test-command-parse-no-result ()
  "Base beads-command-parse without :result returns raw JSON."
  (let ((orig (get 'beads-from-json-test--command 'beads-result)))
    (unwind-protect
        (progn
          (put 'beads-from-json-test--command 'beads-result nil)
          (let* ((cmd (beads-from-json-test--command :json t))
                 (json-str "{\"id\":\"bd-42\"}")
                 (result (beads-command-parse cmd json-str)))
            (should (consp result))
            (should (equal (alist-get 'id result) "bd-42"))))
      (put 'beads-from-json-test--command 'beads-result orig))))

(ert-deftest beads-from-json-test-command-parse-single-object ()
  "beads-command-parse with :result CLASS parses single object."
  (let ((orig (get 'beads-from-json-test--command 'beads-result)))
    (unwind-protect
        (progn
          (put 'beads-from-json-test--command 'beads-result 'beads-issue)
          (let* ((cmd (beads-from-json-test--command :json t))
                 (json-str "{\"id\":\"bd-99\",\"title\":\"Solo\"}")
                 (result (beads-command-parse cmd json-str)))
            (should (beads-issue-p result))
            (should (equal (oref result id) "bd-99"))))
      (put 'beads-from-json-test--command 'beads-result orig))))

(provide 'beads-from-json-test)
;;; beads-from-json-test.el ends here
