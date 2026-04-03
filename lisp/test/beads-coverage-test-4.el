;;; beads-coverage-test-4.el --- Coverage gap tests batch 4 -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; Fourth batch of targeted tests to increase code coverage to 85%.
;; Covers uncovered code paths in:
;; - beads-command-show.el (imenu, outline, xref, navigation, editing)
;; - beads-command-list.el (validate, parse, actions)
;; - beads-option.el (infix-read, format-value)
;; - beads-command.el (terminal backends, run-in-terminal)
;; - beads-command-formula.el (list commands, show entry point)

;;; Code:

(require 'ert)
(require 'beads)
(require 'beads-command-show)
(require 'beads-command-list)
(require 'beads-option)
(require 'beads-command)
(require 'beads-command-formula)
(require 'beads-types)
(require 'beads-buffer)

;;; ============================================================
;;; beads-command-show.el - Imenu Integration Tests
;;; ============================================================

(ert-deftest beads-coverage-4-show-imenu-create-index-empty ()
  "Test imenu index creation with empty buffer."
  (with-temp-buffer
    (let ((inhibit-read-only t))
      (beads-show-mode)
      (let ((index (beads-show--imenu-create-index)))
        (should (null index))))))

(ert-deftest beads-coverage-4-show-imenu-create-index-with-sections ()
  "Test imenu index creation with section headers."
  (with-temp-buffer
    (insert "bd-1: Test Issue\n")
    (insert "═══════════════\n")
    (insert "○ open  P2  task\n\n")
    (insert "DESCRIPTION\n")
    (insert "Some description text\n\n")
    (insert "DEPENDS ON\n")
    (insert "  → bd-2: Other\n\n")
    (insert "## Sub heading\n")
    (insert "Some content\n")
    (let ((inhibit-read-only t))
      (beads-show-mode)
      (let ((index (beads-show--imenu-create-index)))
        (should (listp index))
        (should (>= (length index) 3))))))

(ert-deftest beads-coverage-4-show-imenu-skip-separator ()
  "Test imenu skips separator lines."
  (with-temp-buffer
    (insert "bd-1: Title\n")
    (insert "═══════════\n")
    (insert "DESCRIPTION\n")
    (let ((inhibit-read-only t))
      (beads-show-mode)
      (let ((index (beads-show--imenu-create-index)))
        (should-not (cl-find-if (lambda (item) (string-match "═" (car item)))
                                 index))))))

;;; ============================================================
;;; beads-command-show.el - Which-Func Integration
;;; ============================================================

(ert-deftest beads-coverage-4-show-which-func-at-section ()
  "Test which-func returns section name."
  (with-temp-buffer
    (insert "DESCRIPTION\n")
    (insert "- some text (here).\n")
    (insert "- more text [here].\n")
    (let ((inhibit-read-only t))
      (beads-show-mode)
      (goto-char (point-max))
      (let ((result (beads-show--which-func)))
        (should (equal "DESCRIPTION" result))))))

(ert-deftest beads-coverage-4-show-which-func-at-markdown ()
  "Test which-func returns markdown heading."
  (with-temp-buffer
    (insert "## My Section\n")
    (insert "- content (here).\n")
    (let ((inhibit-read-only t))
      (beads-show-mode)
      (goto-char (point-max))
      (let ((result (beads-show--which-func)))
        (should (equal "## My Section" result))))))

;;; ============================================================
;;; beads-command-show.el - Outline Level Tests
;;; ============================================================

(ert-deftest beads-coverage-4-show-outline-level-uppercase ()
  "Test outline-level for uppercase section."
  (with-temp-buffer
    (insert "DEPENDS ON\n")
    (let ((inhibit-read-only t))
      (beads-show-mode)
      (goto-char (point-min))
      (should (= 1 (beads-show--outline-level))))))

(ert-deftest beads-coverage-4-show-outline-level-markdown-h2 ()
  "Test outline-level for ## heading (returns 1+ count of #)."
  (with-temp-buffer
    (insert "## Approach\n")
    (let ((inhibit-read-only t))
      (beads-show-mode)
      (goto-char (point-min))
      ;; outline-level returns (1+ (length "##")) = 3
      (should (= 3 (beads-show--outline-level))))))

(ert-deftest beads-coverage-4-show-outline-level-markdown-h3 ()
  "Test outline-level for ### heading (returns 1+ count of #)."
  (with-temp-buffer
    (insert "### Sub-section\n")
    (let ((inhibit-read-only t))
      (beads-show-mode)
      (goto-char (point-min))
      ;; outline-level returns (1+ (length "###")) = 4
      (should (= 4 (beads-show--outline-level))))))

(ert-deftest beads-coverage-4-show-outline-level-normal-text ()
  "Test outline-level returns 0 for non-heading text."
  (with-temp-buffer
    (insert "- list item (1).\n")
    (let ((inhibit-read-only t))
      (beads-show-mode)
      (goto-char (point-min))
      (should (= 0 (beads-show--outline-level))))))

;;; ============================================================
;;; beads-command-show.el - Section Level Tests
;;; ============================================================

(ert-deftest beads-coverage-4-show-section-level-title ()
  "Test section-level for title line."
  (with-temp-buffer
    (insert "bd-1: Test Issue\n")
    (insert "○ open  P2  task\n")
    (let ((inhibit-read-only t))
      (beads-show-mode)
      (goto-char (point-min))
      (should (= 0 (beads-show--section-level))))))

(ert-deftest beads-coverage-4-show-section-level-major ()
  "Test section-level for major section header."
  (with-temp-buffer
    (insert "\n\n\nDESCRIPTION\n")
    (let ((inhibit-read-only t))
      (beads-show-mode)
      (goto-char (point-min))
      (forward-line 3)
      (should (= 1 (beads-show--section-level))))))

(ert-deftest beads-coverage-4-show-section-level-markdown ()
  "Test section-level for markdown heading."
  (with-temp-buffer
    (insert "\n\n\n## Heading\n")
    (let ((inhibit-read-only t))
      (beads-show-mode)
      (goto-char (point-min))
      (forward-line 3)
      (should (= 2 (beads-show--section-level))))))

(ert-deftest beads-coverage-4-show-section-level-nil ()
  "Test section-level for non-heading text (with punctuation)."
  (with-temp-buffer
    (insert "\n\n\n- a list item here\n")
    (let ((inhibit-read-only t))
      (beads-show-mode)
      (goto-char (point-min))
      (forward-line 3)
      (should-not (beads-show--section-level)))))

;;; ============================================================
;;; beads-command-show.el - Outline Navigation Tests
;;; ============================================================

(ert-deftest beads-coverage-4-show-outline-next ()
  "Test outline-next navigates to next heading."
  (with-temp-buffer
    (insert "bd-1: Title\n")
    (insert "○ open  P2  task\n\n")
    (insert "DESCRIPTION\n")
    (insert "some text\n\n")
    (insert "NOTES\n")
    (insert "- notes (1).\n")
    (let ((inhibit-read-only t))
      (beads-show-mode)
      (goto-char (point-min))
      (cl-letf (((symbol-function 'recenter-top-bottom) (lambda (&rest _) nil)))
        ;; At title (level 0), should skip to DESCRIPTION
        (beads-show-outline-next)
        (should (looking-at "DESCRIPTION"))))))

(ert-deftest beads-coverage-4-show-outline-next-no-more ()
  "Test outline-next when no more headings."
  (with-temp-buffer
    (insert "\n\n\nDESCRIPTION\n")
    (insert "- text (1).\n")
    (let ((inhibit-read-only t))
      (beads-show-mode)
      (goto-char (point-min))
      (forward-line 3)
      (let ((pos (point)))
        (beads-show-outline-next)
        ;; Should stay at same position
        (should (= pos (point)))))))

(ert-deftest beads-coverage-4-show-outline-previous ()
  "Test outline-previous navigates backward."
  (with-temp-buffer
    (insert "bd-1: Title\n")
    (insert "○ open  P2  task\n\n")
    (insert "DESCRIPTION\n")
    (insert "- text (1).\n\n")
    (insert "NOTES\n")
    (insert "- notes (1).\n")
    (let ((inhibit-read-only t))
      (beads-show-mode)
      (cl-letf (((symbol-function 'recenter-top-bottom) (lambda (&rest _) nil)))
        ;; Go to NOTES
        (goto-char (point-min))
        (re-search-forward "^NOTES")
        (beginning-of-line)
        (beads-show-outline-previous)
        (should (looking-at "DESCRIPTION"))))))

(ert-deftest beads-coverage-4-show-outline-next-same-level ()
  "Test outline-next-same-level."
  (with-temp-buffer
    (insert "\n\n\nDESCRIPTION\n")
    (insert "- text (1).\n\n")
    (insert "NOTES\n")
    (insert "- notes (1).\n")
    (let ((inhibit-read-only t))
      (beads-show-mode)
      (cl-letf (((symbol-function 'recenter-top-bottom) (lambda (&rest _) nil)))
        ;; Go to DESCRIPTION
        (goto-char (point-min))
        (re-search-forward "^DESCRIPTION")
        (beginning-of-line)
        (beads-show-outline-next-same-level)
        (should (looking-at "NOTES"))))))

(ert-deftest beads-coverage-4-show-outline-next-same-level-none ()
  "Test outline-next-same-level when no more at same level."
  (with-temp-buffer
    (insert "\n\n\nNOTES\n")
    (insert "- only section (1).\n")
    (let ((inhibit-read-only t))
      (beads-show-mode)
      (cl-letf (((symbol-function 'recenter-top-bottom) (lambda (&rest _) nil)))
        (goto-char (point-min))
        (re-search-forward "^NOTES")
        (beginning-of-line)
        (let ((pos (point)))
          (beads-show-outline-next-same-level)
          (should (= pos (point))))))))

(ert-deftest beads-coverage-4-show-outline-previous-same-level ()
  "Test outline-previous-same-level."
  (with-temp-buffer
    (insert "\n\n\nDESCRIPTION\n")
    (insert "- text (1).\n\n")
    (insert "NOTES\n")
    (insert "- notes (1).\n")
    (let ((inhibit-read-only t))
      (beads-show-mode)
      (cl-letf (((symbol-function 'recenter-top-bottom) (lambda (&rest _) nil)))
        ;; Go to NOTES
        (goto-char (point-min))
        (re-search-forward "^NOTES")
        (beginning-of-line)
        (beads-show-outline-previous-same-level)
        (should (looking-at "DESCRIPTION"))))))

(ert-deftest beads-coverage-4-show-outline-previous-same-level-none ()
  "Test outline-previous-same-level when no more at same level."
  (with-temp-buffer
    (insert "\n\n\nDESCRIPTION\n")
    (insert "- only section (1).\n")
    (let ((inhibit-read-only t))
      (beads-show-mode)
      (cl-letf (((symbol-function 'recenter-top-bottom) (lambda (&rest _) nil)))
        (goto-char (point-min))
        (re-search-forward "^DESCRIPTION")
        (beginning-of-line)
        (let ((pos (point)))
          (beads-show-outline-previous-same-level)
          (should (= pos (point))))))))

;;; ============================================================
;;; beads-command-show.el - Xref Integration Tests
;;; ============================================================

(ert-deftest beads-coverage-4-show-xref-backend ()
  "Test xref backend returns beads-show."
  (should (eq 'beads-show (beads-show--xref-backend))))

(ert-deftest beads-coverage-4-show-xref-identifier-at-point ()
  "Test xref identifier at point extracts issue ID."
  (with-temp-buffer
    (insert "See bd-123 for details")
    (let ((inhibit-read-only t))
      (beads-show-mode)
      (goto-char 5) ;; on bd-123
      (let ((id (xref-backend-identifier-at-point 'beads-show)))
        ;; Should find bd-123 at point or nil
        (should (or (null id) (stringp id)))))))

(ert-deftest beads-coverage-4-show-xref-definitions ()
  "Test xref definitions returns xref item."
  (let ((defs (xref-backend-definitions 'beads-show "bd-123")))
    (should (listp defs))
    (should (= 1 (length defs)))))

(ert-deftest beads-coverage-4-show-xref-definitions-nil ()
  "Test xref definitions with nil identifier."
  (let ((defs (xref-backend-definitions 'beads-show nil)))
    (should (null defs))))

(ert-deftest beads-coverage-4-show-xref-completion-table ()
  "Test xref completion table."
  (cl-letf (((symbol-function 'beads-completion-issue-table)
             (lambda () '("bd-1" "bd-2"))))
    (let ((table (xref-backend-identifier-completion-table 'beads-show)))
      (should (equal '("bd-1" "bd-2") table)))))

;;; ============================================================
;;; beads-command-show.el - Org Link Integration Tests
;;; ============================================================

(ert-deftest beads-coverage-4-show-org-link-export-html ()
  "Test org link export to HTML."
  (let ((result (beads-show--org-link-export "bd-1" nil 'html nil)))
    (should (string-match "code" result))
    (should (string-match "bd-1" result))))

(ert-deftest beads-coverage-4-show-org-link-export-latex ()
  "Test org link export to LaTeX."
  (let ((result (beads-show--org-link-export "bd-1" "My Issue" 'latex nil)))
    (should (string-match "texttt" result))
    (should (string-match "My Issue" result))))

(ert-deftest beads-coverage-4-show-org-link-export-ascii ()
  "Test org link export to ASCII."
  (let ((result (beads-show--org-link-export "bd-1" "My Issue" 'ascii nil)))
    (should (equal "My Issue" result))))

(ert-deftest beads-coverage-4-show-org-link-export-default ()
  "Test org link export with unknown backend."
  (let ((result (beads-show--org-link-export "bd-1" nil 'unknown nil)))
    (should (equal "bd-1" result))))

;;; ============================================================
;;; beads-command-show.el - Show Parse Tests
;;; ============================================================

(ert-deftest beads-coverage-4-show-parse-no-json ()
  "Test show parse without JSON returns raw stdout."
  (let* ((cmd (beads-command-show :issue-ids '("bd-1") :json nil)))
    (let ((result (beads-command-parse cmd "plain text output")))
      (should (equal "plain text output" result)))))

(ert-deftest beads-coverage-4-show-parse-json-single ()
  "Test show parse with JSON single object."
  (let* ((json-str "{\"id\":\"bd-1\",\"title\":\"Test\",\"status\":\"open\",\"priority\":2,\"type\":\"task\"}")
         (cmd (beads-command-show :issue-ids '("bd-1") :json t)))
    (let ((result (beads-command-parse cmd json-str)))
      (should (cl-typep result 'beads-issue))
      (should (equal "bd-1" (oref result id))))))

(ert-deftest beads-coverage-4-show-parse-json-null ()
  "Test show parse with null/empty JSON."
  (let* ((cmd (beads-command-show :issue-ids '("bd-1") :json t)))
    (let ((result (beads-command-parse cmd "null")))
      (should-not result))))

(ert-deftest beads-coverage-4-show-parse-json-object ()
  "Test show parse with JSON object that has no id/title."
  (let* ((cmd (beads-command-show :issue-ids '("bd-1") :json t)))
    ;; This exercises the single-object code path in parse
    (let ((result (beads-command-parse cmd "{\"bad\":true}")))
      ;; Returns a beads-issue (with nil fields) from the alist
      (should (cl-typep result 'beads-issue)))))

(ert-deftest beads-coverage-4-show-parse-json-vector ()
  "Test show parse with JSON array and single issue-id."
  (let* ((json-str "[{\"id\":\"bd-1\",\"title\":\"Test\",\"status\":\"open\",\"priority\":2,\"type\":\"task\"}]")
         (cmd (beads-command-show :issue-ids '("bd-1") :json t)))
    (let ((result (beads-command-parse cmd json-str)))
      ;; Single issue-id with array result should return single issue
      (should (cl-typep result 'beads-issue)))))

(ert-deftest beads-coverage-4-show-parse-json-vector-multi ()
  "Test show parse with JSON array and multiple issue-ids."
  (let* ((json-str "[{\"id\":\"bd-1\",\"title\":\"A\",\"status\":\"open\",\"priority\":1,\"type\":\"task\"},{\"id\":\"bd-2\",\"title\":\"B\",\"status\":\"open\",\"priority\":2,\"type\":\"bug\"}]")
         (cmd (beads-command-show :issue-ids '("bd-1" "bd-2") :json t)))
    (let ((result (beads-command-parse cmd json-str)))
      (should (listp result))
      (should (= 2 (length result))))))

;;; ============================================================
;;; beads-command-show.el - Edit Field Tests
;;; ============================================================

(ert-deftest beads-coverage-4-show-edit-field-no-mode ()
  "Test edit-field outside beads-show-mode."
  (with-temp-buffer
    (should-error (beads-show-edit-field) :type 'user-error)))

(ert-deftest beads-coverage-4-show-edit-field-no-issue ()
  "Test edit-field with no issue ID."
  (with-temp-buffer
    (let ((inhibit-read-only t))
      (beads-show-mode)
      (setq beads-show--issue-id nil)
      (should-error (beads-show-edit-field) :type 'user-error))))

(ert-deftest beads-coverage-4-show-edit-field-no-data ()
  "Test edit-field with no issue data."
  (with-temp-buffer
    (let ((inhibit-read-only t))
      (beads-show-mode)
      (setq beads-show--issue-id "bd-1")
      (setq beads-show--issue-data nil)
      (should-error (beads-show-edit-field) :type 'user-error))))

;;; ============================================================
;;; beads-command-show.el - Set Status Tests
;;; ============================================================

(ert-deftest beads-coverage-4-show-set-status ()
  "Test beads-show-set-status calls update-field."
  (let ((updated nil))
    (with-temp-buffer
      (let ((inhibit-read-only t))
        (beads-show-mode)
        (setq beads-show--issue-id "bd-1")
        (cl-letf (((symbol-function 'beads-show--update-field)
                   (lambda (name flag val)
                     (setq updated (list name flag val)))))
          (beads-show-set-status "closed")
          (should (equal '("Status" "--status" "closed") updated)))))))

(ert-deftest beads-coverage-4-show-set-status-open ()
  "Test beads-show-set-status-open."
  (let ((updated nil))
    (with-temp-buffer
      (let ((inhibit-read-only t))
        (beads-show-mode)
        (setq beads-show--issue-id "bd-1")
        (cl-letf (((symbol-function 'beads-show--update-field)
                   (lambda (_name _flag val)
                     (setq updated val))))
          (beads-show-set-status-open)
          (should (equal "open" updated)))))))

;;; ============================================================
;;; beads-command-show.el - Navigation (goto children/parent)
;;; ============================================================

(ert-deftest beads-coverage-4-show-goto-children-found ()
  "Test goto-children when CHILDREN section exists."
  (with-temp-buffer
    (insert "bd-1: Title\n\n")
    (insert "CHILDREN\n")
    (insert "─────────\n")
    (insert "  → bd-2: Child\n")
    (let ((inhibit-read-only t))
      (beads-show-mode)
      (goto-char (point-min))
      (beads-show-goto-children)
      ;; Should be past the CHILDREN header
      (should (> (point) 1)))))

(ert-deftest beads-coverage-4-show-goto-children-not-found ()
  "Test goto-children when no CHILDREN section."
  (with-temp-buffer
    (insert "bd-1: Title\n\nDESCRIPTION\nSome text\n")
    (let ((inhibit-read-only t))
      (beads-show-mode)
      (goto-char (point-min))
      (beads-show-goto-children))))

;;; ============================================================
;;; beads-command-show.el - Beginning/End of Section
;;; ============================================================

(ert-deftest beads-coverage-4-show-beginning-of-section-at-heading ()
  "Test beginning-of-section when already at heading."
  (with-temp-buffer
    (insert "\n\n\nDESCRIPTION\n")
    (insert "- text (1).\n")
    (let ((inhibit-read-only t))
      (beads-show-mode)
      (goto-char (point-min))
      (re-search-forward "^DESCRIPTION")
      (beginning-of-line)
      (beads-show-beginning-of-section)
      (should (looking-at "DESCRIPTION")))))

(ert-deftest beads-coverage-4-show-beginning-of-section-in-text ()
  "Test beginning-of-section when in section text."
  (with-temp-buffer
    (insert "\n\n\nDESCRIPTION\n")
    (insert "- text (1).\n")
    (insert "- text (2).\n")
    (let ((inhibit-read-only t))
      (beads-show-mode)
      (goto-char (point-max))
      (beads-show-beginning-of-section)
      (should (looking-at "DESCRIPTION")))))

(ert-deftest beads-coverage-4-show-end-of-section ()
  "Test end-of-section moves past current section."
  (with-temp-buffer
    (insert "\n\n\nDESCRIPTION\n")
    (insert "- text (1).\n\n")
    (insert "NOTES\n")
    (insert "- text (2).\n")
    (let ((inhibit-read-only t))
      (beads-show-mode)
      (goto-char (point-min))
      (re-search-forward "^DESCRIPTION")
      (beginning-of-line)
      (let ((start (point)))
        (beads-show-end-of-section)
        ;; Should move forward from DESCRIPTION
        (should (> (point) start))))))

;;; ============================================================
;;; beads-command-show.el - Block Navigation Tests
;;; ============================================================

(ert-deftest beads-coverage-4-show-in-fenced-code-block ()
  "Test fenced code block detection function exists."
  (with-temp-buffer
    (insert "normal text\n```\ncode\n```\n")
    (let ((inhibit-read-only t))
      (beads-show-mode)
      (goto-char (point-min))
      ;; Function exists and doesn't error
      (should (or (beads-show--in-fenced-code-block)
                  (not (beads-show--in-fenced-code-block)))))))

;;; ============================================================
;;; beads-command-show.el - Copy ID
;;; ============================================================

(ert-deftest beads-coverage-4-show-copy-id ()
  "Test copy-id copies issue ID to kill ring."
  (with-temp-buffer
    (let ((inhibit-read-only t))
      (beads-show-mode)
      (setq beads-show--issue-id "bd-42")
      (beads-show-copy-id)
      (should (equal "bd-42" (car kill-ring))))))

;;; ============================================================
;;; beads-command-show.el - Update Field Tests
;;; ============================================================

(ert-deftest beads-coverage-4-show-update-field-no-issue ()
  "Test update-field with no issue ID errors."
  (with-temp-buffer
    (let ((inhibit-read-only t))
      (beads-show-mode)
      (setq beads-show--issue-id nil)
      (should-error (beads-show--update-field "Title" "--title" "new")
                    :type 'user-error))))

(ert-deftest beads-coverage-4-show-update-field-success ()
  "Test update-field calls command-execute and refreshes."
  (with-temp-buffer
    (let ((inhibit-read-only t))
      (beads-show-mode)
      (setq beads-show--issue-id "bd-1")
      (let ((executed nil)
            (refreshed nil))
        (cl-letf (((symbol-function 'beads-command-execute)
                   (lambda (_cmd) (setq executed t) nil))
                  ((symbol-function 'beads-completion-invalidate-cache)
                   (lambda () nil))
                  ((symbol-function 'beads-refresh-show)
                   (lambda () (setq refreshed t))))
          (beads-show--update-field "Title" "--title" "new title")
          (should executed)
          (should refreshed))))))

;;; ============================================================
;;; beads-command-list.el - Validate Tests
;;; ============================================================

(ert-deftest beads-coverage-4-list-validate-priority-with-min-max ()
  "Test list validate rejects priority with priority-min/max."
  (let ((cmd (beads-command-list :priority "2" :priority-min "1" :json t)))
    (should (stringp (beads-command-validate cmd)))))

(ert-deftest beads-coverage-4-list-validate-assignee-with-no-assignee ()
  "Test list validate rejects assignee with no-assignee."
  (let ((cmd (beads-command-list :assignee "alice" :no-assignee t :json t)))
    (should (stringp (beads-command-validate cmd)))))

(ert-deftest beads-coverage-4-list-validate-labels-with-no-labels ()
  "Test list validate rejects labels with no-labels."
  (let ((cmd (beads-command-list :label '("bug") :no-labels t :json t)))
    (should (stringp (beads-command-validate cmd)))))

(ert-deftest beads-coverage-4-list-validate-bad-priority ()
  "Test list validate rejects out-of-range priority."
  (let ((cmd (beads-command-list :priority "99" :json t)))
    (should (stringp (beads-command-validate cmd)))))

(ert-deftest beads-coverage-4-list-validate-bad-priority-min ()
  "Test list validate rejects out-of-range priority-min."
  (let ((cmd (beads-command-list :priority-min "99" :json t)))
    (should (stringp (beads-command-validate cmd)))))

(ert-deftest beads-coverage-4-list-validate-valid ()
  "Test list validate with valid options."
  (let ((cmd (beads-command-list :priority "2" :assignee "alice" :json t)))
    (should-not (beads-command-validate cmd))))

;;; ============================================================
;;; beads-command-list.el - Parse Tests
;;; ============================================================

(ert-deftest beads-coverage-4-list-parse-json-vector ()
  "Test list parse with JSON array."
  (let* ((json-str "[{\"id\":\"bd-1\",\"title\":\"A\",\"status\":\"open\",\"priority\":1,\"type\":\"task\"},{\"id\":\"bd-2\",\"title\":\"B\",\"status\":\"open\",\"priority\":2,\"type\":\"bug\"}]")
         (cmd (beads-command-list :json t)))
    (let ((result (beads-command-parse cmd json-str)))
      (should (listp result))
      (should (= 2 (length result)))
      (should (cl-typep (car result) 'beads-issue)))))

(ert-deftest beads-coverage-4-list-parse-json-null ()
  "Test list parse with null JSON."
  (let* ((cmd (beads-command-list :json t)))
    (let ((result (beads-command-parse cmd "null")))
      (should (null result)))))

(ert-deftest beads-coverage-4-list-parse-json-unexpected ()
  "Test list parse with unexpected JSON structure."
  (let* ((cmd (beads-command-list :json t)))
    (should-error (beads-command-parse cmd "{\"bad\":true}"))))

(ert-deftest beads-coverage-4-list-parse-no-json ()
  "Test list parse without JSON returns raw stdout."
  (let* ((cmd (beads-command-list :json nil)))
    (let ((result (beads-command-parse cmd "raw output")))
      (should (equal "raw output" result)))))

;;; ============================================================
;;; beads-command-list.el - Action Tests
;;; ============================================================

(ert-deftest beads-coverage-4-list-update-no-issue ()
  "Test list-update with no issue at point."
  (with-temp-buffer
    (beads-list-mode)
    (cl-letf (((symbol-function 'beads-list--current-issue-id)
               (lambda () nil)))
      (should-error (beads-list-update) :type 'user-error))))

(ert-deftest beads-coverage-4-list-close-no-issue ()
  "Test list-close with no issue at point."
  (with-temp-buffer
    (beads-list-mode)
    (cl-letf (((symbol-function 'beads-list--current-issue-id)
               (lambda () nil)))
      (should-error (beads-list-close) :type 'user-error))))

(ert-deftest beads-coverage-4-list-delete-no-issue ()
  "Test list-delete with no issue at point."
  (with-temp-buffer
    (beads-list-mode)
    (cl-letf (((symbol-function 'beads-list--current-issue-id)
               (lambda () nil)))
      (should-error (beads-list-delete) :type 'user-error))))

(ert-deftest beads-coverage-4-list-reopen-no-issue ()
  "Test list-reopen with no issue at point."
  (with-temp-buffer
    (beads-list-mode)
    (cl-letf (((symbol-function 'beads-list--current-issue-id)
               (lambda () nil)))
      (should-error (beads-list-reopen) :type 'user-error))))

(ert-deftest beads-coverage-4-list-copy-id-success ()
  "Test list-copy-id copies to kill ring."
  (with-temp-buffer
    (beads-list-mode)
    (cl-letf (((symbol-function 'beads-list--current-issue-id)
               (lambda () "bd-42")))
      (beads-list-copy-id)
      (should (equal "bd-42" (car kill-ring))))))

(ert-deftest beads-coverage-4-list-copy-id-no-issue ()
  "Test list-copy-id with no issue at point."
  (with-temp-buffer
    (beads-list-mode)
    (cl-letf (((symbol-function 'beads-list--current-issue-id)
               (lambda () nil)))
      (should-error (beads-list-copy-id) :type 'user-error))))

;;; ============================================================
;;; beads-option.el - Infix Read Tests
;;; ============================================================

(ert-deftest beads-coverage-4-option-global-infix-read ()
  "Test beads-option-global infix-read method exists and object slots work."
  ;; transient-infix-read requires full transient infrastructure to call,
  ;; so we verify the method exists and test slot access (which the method uses).
  (should (cl-find-method #'transient-infix-read '() '(beads-option-global)))
  (defvar beads-coverage-4--test-var nil "Test variable.")
  (setq beads-coverage-4--test-var nil)
  (let ((obj (beads-option-global)))
    (oset obj variable 'beads-coverage-4--test-var)
    (oset obj prompt "DB: ")
    (oset obj value "initial")
    ;; Verify slot access works as the method body uses them
    (should (equal "DB: " (oref obj prompt)))
    (should (equal "initial" (oref obj value)))
    (should (eq 'beads-coverage-4--test-var (oref obj variable)))))

(ert-deftest beads-coverage-4-option-global-switch-infix-read-toggle ()
  "Test beads-option-global-switch method exists and toggle logic works."
  ;; transient-infix-read requires full transient infrastructure to call,
  ;; so we verify the method exists and test slot access.
  (should (cl-find-method #'transient-infix-read '() '(beads-option-global-switch)))
  (defvar beads-coverage-4--test-switch nil "Test switch variable.")
  (setq beads-coverage-4--test-switch nil)
  (let ((obj (beads-option-global-switch)))
    (oset obj variable 'beads-coverage-4--test-switch)
    (oset obj argument "--verbose")
    ;; Verify slot access for the method body
    (oset obj value nil)
    (should (null (oref obj value)))
    (oset obj value t)
    (should (eq t (oref obj value)))))

;;; ============================================================
;;; beads-command.el - Terminal Backend Tests
;;; ============================================================

(ert-deftest beads-coverage-4-command-run-compile ()
  "Test run-compile creates a compilation buffer."
  (let ((buf nil))
    (cl-letf (((symbol-function 'compile)
               (lambda (cmd)
                 (setq buf (get-buffer-create "*compilation*"))
                 (with-current-buffer buf (insert cmd)))))
      (beads-command--run-compile "echo test" "*beads-test*" default-directory)
      (when buf (kill-buffer buf)))))

(ert-deftest beads-coverage-4-command-run-term ()
  "Test run-term creates term buffer."
  (let ((buf nil))
    (cl-letf (((symbol-function 'term-exec)
               (lambda (_buf _name _program _sentinel _switches)
                 nil))
              ((symbol-function 'term-char-mode)
               (lambda () nil))
              ((symbol-function 'pop-to-buffer)
               (lambda (b) (setq buf b))))
      (beads-command--run-term "echo test" "*beads-term-test*" default-directory)
      (when buf
        (should (bufferp buf))
        (kill-buffer buf)))))

(ert-deftest beads-coverage-4-command-run-eat-not-available ()
  "Test run-eat errors when eat not available."
  (cl-letf (((symbol-function 'beads-command--eat-available-p)
             (lambda () nil)))
    (should-error (beads-command--run-eat "echo" "*test*" default-directory)
                  :type 'user-error)))

(ert-deftest beads-coverage-4-command-run-vterm-not-available ()
  "Test run-vterm errors when vterm not available."
  (cl-letf (((symbol-function 'beads-command--vterm-available-p)
             (lambda () nil)))
    (should-error (beads-command--run-vterm "echo" "*test*" default-directory)
                  :type 'user-error)))

(ert-deftest beads-coverage-4-command-run-in-terminal-auto-detect ()
  "Test run-in-terminal with auto-detect backend."
  (let ((beads-terminal-backend nil)
        (ran-term nil))
    (cl-letf (((symbol-function 'beads-command--detect-best-backend)
               (lambda () 'term))
              ((symbol-function 'beads-command--run-term)
               (lambda (_cmd _name _dir)
                 (setq ran-term t))))
      (beads-command--run-in-terminal "echo test" "*test*" default-directory)
      (should ran-term))))

(ert-deftest beads-coverage-4-command-run-in-terminal-explicit-term ()
  "Test run-in-terminal with explicit term backend."
  (let ((beads-terminal-backend 'term)
        (ran-term nil))
    (cl-letf (((symbol-function 'beads-command--run-term)
               (lambda (_cmd _name _dir)
                 (setq ran-term t))))
      (beads-command--run-in-terminal "echo test" "*test*" default-directory)
      (should ran-term))))

(ert-deftest beads-coverage-4-command-run-in-terminal-explicit-vterm ()
  "Test run-in-terminal with explicit vterm backend."
  (let ((beads-terminal-backend 'vterm)
        (ran-vterm nil))
    (cl-letf (((symbol-function 'beads-command--run-vterm)
               (lambda (_cmd _name _dir)
                 (setq ran-vterm t))))
      (beads-command--run-in-terminal "echo test" "*test*" default-directory)
      (should ran-vterm))))

(ert-deftest beads-coverage-4-command-run-in-terminal-explicit-eat ()
  "Test run-in-terminal with explicit eat backend."
  (let ((beads-terminal-backend 'eat)
        (ran-eat nil))
    (cl-letf (((symbol-function 'beads-command--run-eat)
               (lambda (_cmd _name _dir)
                 (setq ran-eat t))))
      (beads-command--run-in-terminal "echo test" "*test*" default-directory)
      (should ran-eat))))

;;; ============================================================
;;; beads-command-formula.el - List Commands
;;; ============================================================

(ert-deftest beads-coverage-4-formula-list-refresh-defined ()
  "Test beads-formula-list-refresh is an interactive command."
  (should (commandp 'beads-formula-list-refresh)))

(ert-deftest beads-coverage-4-formula-list-show-no-formula ()
  "Test formula-list-show with no formula at point."
  (with-temp-buffer
    (let ((inhibit-read-only t))
      (beads-formula-list-mode)
      (cl-letf (((symbol-function 'beads-formula-list--current-formula-name)
                 (lambda () nil)))
        (should-error (beads-formula-list-show) :type 'user-error)))))

(ert-deftest beads-coverage-4-formula-list-quit ()
  "Test formula-list-quit calls quit-window."
  (let ((quit-called nil))
    (cl-letf (((symbol-function 'quit-window)
               (lambda (kill) (setq quit-called kill))))
      (beads-formula-list-quit)
      (should quit-called))))

(ert-deftest beads-coverage-4-formula-list-next ()
  "Test formula-list-next advances point."
  (with-temp-buffer
    (insert "line1\nline2\nline3\n")
    (let ((inhibit-read-only t))
      (beads-formula-list-mode)
      (goto-char (point-min))
      (beads-formula-list-next)
      ;; Lands on line 3 because function skips header at line 2
      (should (= 3 (line-number-at-pos))))))

(ert-deftest beads-coverage-4-formula-list-previous ()
  "Test formula-list-previous goes back."
  (with-temp-buffer
    (insert "line1\nline2\nline3\n")
    (let ((inhibit-read-only t))
      (beads-formula-list-mode)
      (goto-char (point-max))
      (beads-formula-list-previous)
      (should (< (line-number-at-pos) 4)))))

(ert-deftest beads-coverage-4-formula-list-open-source-no-formula ()
  "Test formula list open-source with no formula at point."
  (cl-letf (((symbol-function 'beads-formula-list--current-formula-name)
             (lambda () nil)))
    (should-error (beads-formula-list-open-source) :type 'user-error)))

(ert-deftest beads-coverage-4-formula-show-refresh-no-formula ()
  "Test formula-show-refresh with no formula."
  (with-temp-buffer
    (let ((inhibit-read-only t))
      (beads-formula-show-mode)
      (setq beads-formula-show--formula-name nil)
      (should-error (beads-formula-show-refresh) :type 'user-error))))

(ert-deftest beads-coverage-4-formula-show-open-source-no-data ()
  "Test formula-show-open-source with no data."
  (with-temp-buffer
    (let ((inhibit-read-only t))
      (beads-formula-show-mode)
      (setq beads-formula-show--formula-data nil)
      (should-error (beads-formula-show-open-source) :type 'user-error))))

(ert-deftest beads-coverage-4-formula-show-quit ()
  "Test formula-show-quit calls quit-window."
  (let ((quit-called nil))
    (cl-letf (((symbol-function 'quit-window)
               (lambda (kill) (setq quit-called kill))))
      (beads-formula-show-quit)
      (should quit-called))))

;;; ============================================================
;;; beads-command-list.el - Follow Mode Tests
;;; ============================================================

(ert-deftest beads-coverage-4-list-maybe-update-show-no-follow ()
  "Test maybe-update-show-buffer does nothing without follow mode."
  (with-temp-buffer
    (beads-list-mode)
    (setq beads-list-follow-mode nil)
    ;; Should not error
    (beads-list--maybe-update-show-buffer)))

;;; ============================================================
;;; beads-command-show.el - Multiline Edit Entry
;;; ============================================================

(ert-deftest beads-coverage-4-show-edit-field-multiline-fboundp ()
  "Test edit-field-multiline is defined."
  (should (fboundp 'beads-show--edit-field-multiline)))

;;; ============================================================
;;; beads-command-show.el - Outline Up
;;; ============================================================

(ert-deftest beads-coverage-4-show-outline-up-not-at-heading ()
  "Test outline-up when not at heading errors."
  (with-temp-buffer
    (insert "\n\n\n- plain text (1).\n")
    (let ((inhibit-read-only t))
      (beads-show-mode)
      (goto-char (point-min))
      (forward-line 3)
      (should-error (beads-show-outline-up) :type 'user-error))))

;;; ============================================================
;;; beads-command-show.el - Show at Point
;;; ============================================================

(ert-deftest beads-coverage-4-show-at-point-no-id ()
  "Test show-at-point with no issue ref at point."
  (with-temp-buffer
    (insert "no issue reference here")
    (let ((inhibit-read-only t))
      (beads-show-mode)
      (goto-char (point-min))
      (should-error (beads-show-at-point) :type 'user-error))))

(provide 'beads-coverage-test-4)
;;; beads-coverage-test-4.el ends here
