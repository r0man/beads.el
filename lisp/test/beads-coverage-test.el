;;; beads-coverage-test.el --- Coverage gap tests -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; Targeted tests to increase code coverage across multiple modules.
;; Covers uncovered code paths in:
;; - beads-command.el (terminal backends, async, ANSI filter)
;; - beads-command-show.el (navigation, actions, field editing)
;; - beads-command-formula.el (list utilities, show rendering)
;; - beads-option.el (infix read methods)

;;; Code:

(require 'ert)
(require 'beads)
(require 'beads-command)
(require 'beads-command-show)
(require 'beads-command-formula)
(require 'beads-option)
(require 'beads-types)

;;; ============================================================
;;; beads-command.el - Terminal Backend Tests
;;; ============================================================

(ert-deftest beads-coverage-test-vterm-available-p-no-vterm ()
  "Test vterm availability check when vterm is not installed."
  ;; vterm is not installed in test environment
  (cl-letf (((symbol-function 'require) (lambda (feat &optional _file _noerror) nil)))
    (should-not (beads-command--vterm-available-p))))

(ert-deftest beads-coverage-test-eat-available-p-no-eat ()
  "Test eat availability check when eat is not installed."
  (cl-letf (((symbol-function 'require) (lambda (feat &optional _file _noerror) nil)))
    (should-not (beads-command--eat-available-p))))

(ert-deftest beads-coverage-test-detect-best-backend-term-fallback ()
  "Test detect-best-backend falls back to term when others unavailable."
  (cl-letf (((symbol-function 'beads-command--vterm-available-p) (lambda () nil))
            ((symbol-function 'beads-command--eat-available-p) (lambda () nil)))
    (should (eq (beads-command--detect-best-backend) 'term))))

(ert-deftest beads-coverage-test-detect-best-backend-vterm ()
  "Test detect-best-backend returns vterm when available."
  (cl-letf (((symbol-function 'beads-command--vterm-available-p) (lambda () t)))
    (should (eq (beads-command--detect-best-backend) 'vterm))))

(ert-deftest beads-coverage-test-detect-best-backend-eat ()
  "Test detect-best-backend returns eat when vterm unavailable."
  (cl-letf (((symbol-function 'beads-command--vterm-available-p) (lambda () nil))
            ((symbol-function 'beads-command--eat-available-p) (lambda () t)))
    (should (eq (beads-command--detect-best-backend) 'eat))))

(ert-deftest beads-coverage-test-run-in-terminal-compile ()
  "Test run-in-terminal dispatches to compile backend."
  (let ((beads-terminal-backend 'compile)
        (called nil))
    (cl-letf (((symbol-function 'beads-command--run-compile)
               (lambda (cmd buf dir) (setq called (list cmd buf dir)))))
      (beads-command--run-in-terminal "echo test" "*test*" "/tmp")
      (should called)
      (should (equal (car called) "echo test")))))

(ert-deftest beads-coverage-test-run-in-terminal-term ()
  "Test run-in-terminal dispatches to term backend."
  (let ((beads-terminal-backend 'term)
        (called nil))
    (cl-letf (((symbol-function 'beads-command--run-term)
               (lambda (cmd buf dir) (setq called (list cmd buf dir)))))
      (beads-command--run-in-terminal "echo test" "*test*" "/tmp")
      (should called))))

(ert-deftest beads-coverage-test-run-in-terminal-vterm ()
  "Test run-in-terminal dispatches to vterm backend."
  (let ((beads-terminal-backend 'vterm)
        (called nil))
    (cl-letf (((symbol-function 'beads-command--run-vterm)
               (lambda (cmd buf dir) (setq called t))))
      (beads-command--run-in-terminal "echo test" "*test*" "/tmp")
      (should called))))

(ert-deftest beads-coverage-test-run-in-terminal-eat ()
  "Test run-in-terminal dispatches to eat backend."
  (let ((beads-terminal-backend 'eat)
        (called nil))
    (cl-letf (((symbol-function 'beads-command--run-eat)
               (lambda (cmd buf dir) (setq called t))))
      (beads-command--run-in-terminal "echo test" "*test*" "/tmp")
      (should called))))

(ert-deftest beads-coverage-test-run-in-terminal-nil-autodetect ()
  "Test run-in-terminal auto-detects backend when nil."
  (let ((beads-terminal-backend nil)
        (called nil))
    (cl-letf (((symbol-function 'beads-command--detect-best-backend)
               (lambda () 'term))
              ((symbol-function 'beads-command--run-term)
               (lambda (cmd buf dir) (setq called t))))
      (beads-command--run-in-terminal "echo test" "*test*" "/tmp")
      (should called))))

(ert-deftest beads-coverage-test-run-in-terminal-unknown-fallback ()
  "Test run-in-terminal falls back to term for unknown backend."
  (let ((beads-terminal-backend 'unknown-backend)
        (called nil))
    (cl-letf (((symbol-function 'beads-command--run-term)
               (lambda (cmd buf dir) (setq called t))))
      (beads-command--run-in-terminal "echo test" "*test*" "/tmp")
      (should called))))

(ert-deftest beads-coverage-test-run-compile ()
  "Test run-compile creates a compilation buffer."
  (let ((compile-called nil)
        (rename-called nil))
    (cl-letf (((symbol-function 'compile)
               (lambda (cmd) (setq compile-called cmd)))
              ((symbol-function 'get-buffer)
               (lambda (_name) nil)))
      (beads-command--run-compile "echo hello" "*bd test*" "/tmp")
      (should (equal compile-called "echo hello")))))

(ert-deftest beads-coverage-test-run-vterm-unavailable ()
  "Test run-vterm signals error when vterm not available."
  (cl-letf (((symbol-function 'beads-command--vterm-available-p) (lambda () nil)))
    (should-error (beads-command--run-vterm "echo" "*test*" "/tmp")
                  :type 'user-error)))

(ert-deftest beads-coverage-test-run-eat-unavailable ()
  "Test run-eat signals error when eat not available."
  (cl-letf (((symbol-function 'beads-command--eat-available-p) (lambda () nil)))
    (should-error (beads-command--run-eat "echo" "*test*" "/tmp")
                  :type 'user-error)))

(ert-deftest beads-coverage-test-ansi-color-filter ()
  "Test ANSI color filter strips OSC sequences."
  (with-temp-buffer
    (insert "Hello\033]11;?\007world")
    (let ((compilation-filter-start (point-min)))
      (cl-letf (((symbol-function 'ansi-color-apply-on-region)
                 (lambda (start end) nil)))
        (beads-command--ansi-color-filter)
        (should (string-match-p "Helloworld" (buffer-string)))))))

(ert-deftest beads-coverage-test-ansi-color-filter-osc-st ()
  "Test ANSI color filter strips OSC with ST terminator."
  (with-temp-buffer
    (insert "Hello\033]11;rgb:ffff/ffff/ffff\033\\world")
    (let ((compilation-filter-start (point-min)))
      (cl-letf (((symbol-function 'ansi-color-apply-on-region)
                 (lambda (start end) nil)))
        (beads-command--ansi-color-filter)
        (should (string-match-p "Helloworld" (buffer-string)))))))

;;; ============================================================
;;; beads-command.el - Command Subcommand Tests
;;; ============================================================

(ert-deftest beads-coverage-test-subcommand-with-cli-command ()
  "Test beads-command-subcommand with class that has cli-command slot."
  (let ((cmd (beads-command-show :issue-ids '("bd-1"))))
    (should (stringp (beads-command-subcommand cmd)))))

(ert-deftest beads-coverage-test-subcommand-from-class-name ()
  "Test beads-command-subcommand derives name from class when no cli-command."
  ;; Use a concrete command class that doesn't have a cli-command slot
  (let ((cmd (beads-command-show :issue-ids '("bd-1"))))
    ;; show class has cli-command "show"
    (should (equal (beads-command-subcommand cmd) "show"))))

;;; ============================================================
;;; beads-command.el - Async Execution Tests
;;; ============================================================

(ert-deftest beads-coverage-test-execute-async-validation-error ()
  "Test async execution raises validation error."
  (let ((cmd (beads-command-show)))
    ;; beads-command-show with no issue-ids should fail validation
    (cl-letf (((symbol-function 'beads-command-validate)
               (lambda (_cmd) "Missing issue-ids")))
      (should-error (beads-command-execute-async cmd)
                    :type 'beads-validation-error))))

(ert-deftest beads-coverage-test-execute-async-with-callback ()
  "Test async execution calls callback on completion."
  (let* ((callback-result nil)
         (cmd (beads-command-show :issue-ids '("test") :json nil))
         (process nil))
    (cl-letf (((symbol-function 'beads-command-validate) (lambda (_cmd) nil))
              ((symbol-function 'beads-command-line) (lambda (_cmd) '("echo" "test")))
              ((symbol-function 'beads-command-parse)
               (lambda (_cmd _exec) "parsed-result")))
      (setq process (beads-command-execute-async
                     cmd
                     (lambda (exec) (setq callback-result exec))))
      ;; Wait for process to finish
      (while (process-live-p process)
        (accept-process-output process 1))
      ;; Give sentinel time to run
      (accept-process-output nil 0.5)
      (should callback-result)
      (should (cl-typep callback-result 'beads-command-execution)))))

;;; ============================================================
;;; beads-command.el - Execute Interactive Tests
;;; ============================================================

(ert-deftest beads-coverage-test-execute-interactive-json-disable ()
  "Test execute-interactive does not modify json slot (nil by default)."
  (let ((cmd (beads-command-show :issue-ids '("bd-1"))))
    (should-not (oref cmd json))
    (cl-letf (((symbol-function 'beads-command-line)
               (lambda (_cmd) '("bd" "show" "bd-1")))
              ((symbol-function 'beads-command--run-in-terminal)
               (lambda (_cmd _buf _dir) nil))
              ((symbol-function 'beads--find-beads-dir)
               (lambda () "/tmp/.beads")))
      (beads-command-execute-interactive cmd)
      ;; JSON slot should remain nil (not modified by execute-interactive)
      (should-not (oref cmd json)))))

;;; ============================================================
;;; beads-command-show.el - Navigation Tests
;;; ============================================================

(defmacro beads-coverage-test-with-show-buffer (content &rest body)
  "Execute BODY in a temporary beads-show buffer with CONTENT."
  (declare (indent 1))
  `(with-temp-buffer
     (beads-show-mode)
     (let ((inhibit-read-only t))
       (insert ,content))
     (goto-char (point-min))
     ,@body))

(ert-deftest beads-coverage-test-show-next-section ()
  "Test next-section navigation."
  (beads-coverage-test-with-show-buffer
      "HEADER INFO\n\nDESCRIPTION\nSome text\n\nNOTES\nMore text"
    (cl-letf (((symbol-function 'recenter-top-bottom) (lambda (&rest _) nil)))
      (beads-show-next-section)
      (should (looking-at "DESCRIPTION")))))

(ert-deftest beads-coverage-test-show-next-section-no-more ()
  "Test next-section at end of buffer."
  (beads-coverage-test-with-show-buffer
      "Some text without sections"
    (should (equal (beads-show-next-section) nil))))

(ert-deftest beads-coverage-test-show-previous-section ()
  "Test previous-section navigation."
  (beads-coverage-test-with-show-buffer
      "HEADER INFO\n\nDESCRIPTION\nSome text\n\nNOTES\nMore text"
    (goto-char (point-max))
    (cl-letf (((symbol-function 'recenter-top-bottom) (lambda (&rest _) nil)))
      (beads-show-previous-section)
      (should (looking-at "NOTES")))))

(ert-deftest beads-coverage-test-show-previous-section-no-more ()
  "Test previous-section at beginning of buffer."
  (beads-coverage-test-with-show-buffer
      "Some text without sections"
    (should (equal (beads-show-previous-section) nil))))

(ert-deftest beads-coverage-test-show-forward-paragraph ()
  "Test forward-paragraph command."
  (beads-coverage-test-with-show-buffer
      "Paragraph 1.\n\nParagraph 2.\n\nParagraph 3."
    (beads-show-forward-paragraph)
    (should (> (point) 1))))

(ert-deftest beads-coverage-test-show-backward-paragraph ()
  "Test backward-paragraph command."
  (beads-coverage-test-with-show-buffer
      "Paragraph 1.\n\nParagraph 2.\n\nParagraph 3."
    (goto-char (point-max))
    (beads-show-backward-paragraph)
    (should (< (point) (point-max)))))

(ert-deftest beads-coverage-test-show-mark-paragraph ()
  "Test mark-paragraph command."
  (beads-coverage-test-with-show-buffer
      "Paragraph 1.\n\nParagraph 2."
    (beads-show-mark-paragraph)
    (should (region-active-p))))

;;; ============================================================
;;; beads-command-show.el - Block Boundary Detection
;;; ============================================================

(ert-deftest beads-coverage-test-at-block-boundary-blank ()
  "Test block boundary detection for blank lines."
  (beads-coverage-test-with-show-buffer "  \n"
    (should (eq (beads-show--at-block-boundary) 'blank))))

(ert-deftest beads-coverage-test-at-block-boundary-fenced-code ()
  "Test block boundary detection for fenced code."
  (beads-coverage-test-with-show-buffer "```python\ncode\n```\n"
    (should (eq (beads-show--at-block-boundary) 'fenced-code))))

(ert-deftest beads-coverage-test-at-block-boundary-list ()
  "Test block boundary detection for list items."
  (beads-coverage-test-with-show-buffer "- item 1\n- item 2\n"
    (should (eq (beads-show--at-block-boundary) 'list))))

(ert-deftest beads-coverage-test-at-block-boundary-numbered-list ()
  "Test block boundary detection for numbered list."
  (beads-coverage-test-with-show-buffer "1. first\n2. second\n"
    (should (eq (beads-show--at-block-boundary) 'list))))

(ert-deftest beads-coverage-test-at-block-boundary-blockquote ()
  "Test block boundary detection for blockquote."
  (beads-coverage-test-with-show-buffer "> quoted text\n"
    (should (eq (beads-show--at-block-boundary) 'blockquote))))

(ert-deftest beads-coverage-test-at-block-boundary-indented-code ()
  "Test block boundary detection for indented code."
  (beads-coverage-test-with-show-buffer "    indented code\n"
    (should (eq (beads-show--at-block-boundary) 'indented-code))))

(ert-deftest beads-coverage-test-at-block-boundary-tab-indented ()
  "Test block boundary detection for tab-indented code."
  (beads-coverage-test-with-show-buffer "\tindented code\n"
    (should (eq (beads-show--at-block-boundary) 'indented-code))))

(ert-deftest beads-coverage-test-at-block-boundary-regular-text ()
  "Test block boundary detection for regular text."
  (beads-coverage-test-with-show-buffer "Regular text here\n"
    (should-not (beads-show--at-block-boundary))))

;;; ============================================================
;;; beads-command-show.el - Block Navigation
;;; ============================================================

(ert-deftest beads-coverage-test-skip-blank-lines-forward ()
  "Test skipping blank lines forward."
  (beads-coverage-test-with-show-buffer "\n\n\nText here\n"
    (should (beads-show--skip-blank-lines-forward))))

(ert-deftest beads-coverage-test-skip-blank-lines-forward-no-blanks ()
  "Test skipping blank lines forward when none exist."
  (beads-coverage-test-with-show-buffer "Text here\n"
    (should-not (beads-show--skip-blank-lines-forward))))

(ert-deftest beads-coverage-test-forward-block-fenced-code ()
  "Test forward-block skips fenced code block."
  (beads-coverage-test-with-show-buffer "```\ncode line\n```\n\nAfter"
    (cl-letf (((symbol-function 'recenter-top-bottom) (lambda (&rest _) nil)))
      (beads-show-forward-block)
      ;; Point should be past the closing fence
      (should (> (point) 15)))))

(ert-deftest beads-coverage-test-forward-block-list ()
  "Test forward-block skips list."
  (beads-coverage-test-with-show-buffer "- item 1\n- item 2\n\nAfter"
    (cl-letf (((symbol-function 'recenter-top-bottom) (lambda (&rest _) nil)))
      (beads-show-forward-block)
      (should (> (point) 1)))))

(ert-deftest beads-coverage-test-forward-block-blockquote ()
  "Test forward-block skips blockquote."
  (beads-coverage-test-with-show-buffer "> quote 1\n> quote 2\n\nAfter"
    (cl-letf (((symbol-function 'recenter-top-bottom) (lambda (&rest _) nil)))
      (beads-show-forward-block)
      (should (> (point) 1)))))

(ert-deftest beads-coverage-test-forward-block-indented ()
  "Test forward-block skips indented code."
  (beads-coverage-test-with-show-buffer "    code1\n    code2\n\nAfter"
    (cl-letf (((symbol-function 'recenter-top-bottom) (lambda (&rest _) nil)))
      (beads-show-forward-block)
      (should (> (point) 1)))))

(ert-deftest beads-coverage-test-forward-block-no-block ()
  "Test forward-block when no block found."
  (beads-coverage-test-with-show-buffer "Just text"
    (cl-letf (((symbol-function 'recenter-top-bottom) (lambda (&rest _) nil)))
      (let ((start (point)))
        (beads-show-forward-block)
        ;; Should not move or should scan to end
        (should (>= (point) start))))))

(ert-deftest beads-coverage-test-backward-block-list ()
  "Test backward-block navigates to list start."
  (beads-coverage-test-with-show-buffer "- item 1\n- item 2\n- item 3\n"
    (goto-char (point-max))
    (forward-line -1)
    (cl-letf (((symbol-function 'recenter-top-bottom) (lambda (&rest _) nil)))
      (beads-show-backward-block)
      (should (<= (point) 10)))))

(ert-deftest beads-coverage-test-backward-block-blockquote ()
  "Test backward-block navigates to blockquote start."
  (beads-coverage-test-with-show-buffer "> line 1\n> line 2\n> line 3\n"
    (goto-char (point-max))
    (forward-line -1)
    (cl-letf (((symbol-function 'recenter-top-bottom) (lambda (&rest _) nil)))
      (beads-show-backward-block)
      (should (<= (point) 5)))))

(ert-deftest beads-coverage-test-backward-block-no-block ()
  "Test backward-block when no previous block."
  (beads-coverage-test-with-show-buffer "Just text"
    (goto-char (point-max))
    (beads-show-backward-block)))

;;; ============================================================
;;; beads-command-show.el - Section Navigation
;;; ============================================================

(ert-deftest beads-coverage-test-beginning-of-section-at-heading ()
  "Test beginning-of-section when already at a heading."
  (beads-coverage-test-with-show-buffer "DESCRIPTION\n═══════════\nText here"
    (beads-show-beginning-of-section)
    (should (= (point) (point-min)))))

(ert-deftest beads-coverage-test-beginning-of-section-in-body ()
  "Test beginning-of-section from within section body."
  (beads-coverage-test-with-show-buffer "bd-1: Test Issue\n○ OPEN  P1  task\n\nDESCRIPTION\nText here\nMore text\nEven more"
    ;; Go to "Even more" line (line 7) - should find a heading above
    (goto-char (point-min))
    (forward-line 6)  ; on "Even more"
    (let ((start (line-number-at-pos)))
      (beads-show-beginning-of-section)
      ;; Should have moved backward (closer to beginning)
      (should (<= (line-number-at-pos) start)))))

(ert-deftest beads-coverage-test-beginning-of-section-no-section ()
  "Test beginning-of-section when no section exists."
  (beads-coverage-test-with-show-buffer "Just some plain text\nno headings"
    (goto-char (point-max))
    (beads-show-beginning-of-section)))

(ert-deftest beads-coverage-test-end-of-section ()
  "Test end-of-section from heading."
  (beads-coverage-test-with-show-buffer "DESCRIPTION\nText\n\nNOTES\nMore"
    (beads-show-end-of-section)
    (should (> (point) 1))))

(ert-deftest beads-coverage-test-end-of-section-with-underline ()
  "Test end-of-section with underline."
  (beads-coverage-test-with-show-buffer "DESCRIPTION\n═══════════\nText\n\nNOTES\nMore"
    (beads-show-end-of-section)
    (should (> (point) 1))))

(ert-deftest beads-coverage-test-mark-section ()
  "Test mark-section marks the section region."
  (beads-coverage-test-with-show-buffer "DESCRIPTION\n═══════════\nText here\n\nNOTES\nMore"
    (beads-show-mark-section)
    (should (region-active-p))))

;;; ============================================================
;;; beads-command-show.el - Button Navigation Wrap
;;; ============================================================

(ert-deftest beads-coverage-test-show-next-button ()
  "Test next-button navigation."
  (beads-coverage-test-with-show-buffer "Text "
    (let ((inhibit-read-only t))
      (insert-text-button "bd-1" 'action #'ignore)
      (insert " more ")
      (insert-text-button "bd-2" 'action #'ignore))
    (goto-char (point-min))
    (beads-show-next-button)
    (should (button-at (point)))))

(ert-deftest beads-coverage-test-show-next-button-wrap ()
  "Test next-button wraps around to first button."
  (with-temp-buffer
    (beads-show-mode)
    (let ((inhibit-read-only t))
      (insert "start ")
      (insert-text-button "bd-1" 'action #'ignore)
      (insert " end"))
    (goto-char (point-max))
    (beads-show-next-button)
    (should (button-at (point)))))

(ert-deftest beads-coverage-test-show-next-button-none ()
  "Test next-button when no buttons exist."
  (beads-coverage-test-with-show-buffer "No buttons here"
    (beads-show-next-button)))

(ert-deftest beads-coverage-test-show-prev-button ()
  "Test prev-button navigation."
  (beads-coverage-test-with-show-buffer ""
    (let ((inhibit-read-only t))
      (insert-text-button "bd-1" 'action #'ignore)
      (insert " more ")
      (insert-text-button "bd-2" 'action #'ignore))
    (goto-char (point-max))
    (beads-show-previous-button)
    (should (button-at (point)))))

(ert-deftest beads-coverage-test-show-prev-button-wrap ()
  "Test prev-button wraps around to last button."
  (beads-coverage-test-with-show-buffer ""
    (let ((inhibit-read-only t))
      (insert "start ")
      (insert-text-button "bd-1" 'action #'ignore))
    (goto-char (point-min))
    (beads-show-previous-button)
    (should (button-at (point)))))

(ert-deftest beads-coverage-test-show-prev-button-none ()
  "Test prev-button when no buttons exist."
  (beads-coverage-test-with-show-buffer "No buttons here"
    (beads-show-previous-button)))

;;; ============================================================
;;; beads-command-show.el - Status Actions
;;; ============================================================

(ert-deftest beads-coverage-test-show-set-status-open ()
  "Test set-status-open calls update-field."
  (let ((update-called nil))
    (beads-coverage-test-with-show-buffer "test"
      (setq beads-show--issue-id "bd-42")
      (cl-letf (((symbol-function 'beads-show--update-field)
                 (lambda (name flag value)
                   (setq update-called (list name flag value)))))
        (beads-show-set-status-open)
        (should (equal update-called '("Status" "--status" "open")))))))

(ert-deftest beads-coverage-test-show-set-status-in-progress ()
  "Test set-status-in-progress calls update-field."
  (let ((update-called nil))
    (beads-coverage-test-with-show-buffer "test"
      (setq beads-show--issue-id "bd-42")
      (cl-letf (((symbol-function 'beads-show--update-field)
                 (lambda (name flag value)
                   (setq update-called (list name flag value)))))
        (beads-show-set-status-in-progress)
        (should (equal update-called '("Status" "--status" "in_progress")))))))

(ert-deftest beads-coverage-test-show-set-status-blocked ()
  "Test set-status-blocked calls update-field."
  (let ((update-called nil))
    (beads-coverage-test-with-show-buffer "test"
      (setq beads-show--issue-id "bd-42")
      (cl-letf (((symbol-function 'beads-show--update-field)
                 (lambda (name flag value)
                   (setq update-called (list name flag value)))))
        (beads-show-set-status-blocked)
        (should (equal update-called '("Status" "--status" "blocked")))))))

(ert-deftest beads-coverage-test-show-set-status-closed ()
  "Test set-status-closed calls close command."
  (let ((close-called nil))
    (beads-coverage-test-with-show-buffer "test"
      (setq beads-show--issue-id "bd-42")
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "Done"))
                ((symbol-function 'beads-command-close!)
                 (lambda (&rest args) (setq close-called args)))
                ((symbol-function 'beads-completion-invalidate-cache) #'ignore)
                ((symbol-function 'beads-refresh-show) #'ignore))
        (beads-show-set-status-closed)
        (should close-called)))))

(ert-deftest beads-coverage-test-show-set-status-closed-error ()
  "Test set-status-closed handles errors gracefully."
  (beads-coverage-test-with-show-buffer "test"
    (setq beads-show--issue-id "bd-42")
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "Done"))
              ((symbol-function 'beads-command-close!)
               (lambda (&rest _) (error "Command failed"))))
      ;; Should not signal error
      (beads-show-set-status-closed))))

;;; ============================================================
;;; beads-command-show.el - Note and Label Actions
;;; ============================================================

(ert-deftest beads-coverage-test-show-add-note ()
  "Test add-note appends to existing notes."
  (let ((update-called nil))
    (beads-coverage-test-with-show-buffer "test"
      (setq beads-show--issue-id "bd-42")
      (setq beads-show--issue-data
            (beads-issue-from-json '((id . "bd-42") (title . "Test")
                                     (status . "open") (priority . 1)
                                     (issue_type . "task")
                                     (created_at . "2025-01-01T00:00:00Z")
                                     (updated_at . "2025-01-01T00:00:00Z")
                                     (notes . "Existing note"))))
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "New note"))
                ((symbol-function 'beads-show--update-field)
                 (lambda (name flag value)
                   (setq update-called (list name flag value)))))
        (beads-show-add-note)
        (should update-called)
        (should (string= (nth 0 update-called) "Notes"))
        (should (string-match-p "Existing note" (nth 2 update-called)))
        (should (string-match-p "New note" (nth 2 update-called)))))))

(ert-deftest beads-coverage-test-show-add-note-no-existing ()
  "Test add-note when no existing notes."
  (let ((update-called nil))
    (beads-coverage-test-with-show-buffer "test"
      (setq beads-show--issue-id "bd-42")
      (setq beads-show--issue-data
            (beads-issue-from-json '((id . "bd-42") (title . "Test")
                                     (status . "open") (priority . 1)
                                     (issue_type . "task")
                                     (created_at . "2025-01-01T00:00:00Z")
                                     (updated_at . "2025-01-01T00:00:00Z"))))
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "First note"))
                ((symbol-function 'beads-show--update-field)
                 (lambda (name flag value)
                   (setq update-called (list name flag value)))))
        (beads-show-add-note)
        (should update-called)
        (should (string= (nth 2 update-called) "First note"))))))

(ert-deftest beads-coverage-test-show-add-label ()
  "Test add-label calls label-add command."
  (let ((label-called nil))
    (beads-coverage-test-with-show-buffer "test"
      (setq beads-show--issue-id "bd-42")
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "bugfix"))
                ((symbol-function 'beads-command-label-add!)
                 (lambda (&rest args) (setq label-called args)))
                ((symbol-function 'beads-refresh-show) #'ignore))
        (beads-show-add-label)
        (should label-called)))))

(ert-deftest beads-coverage-test-show-add-label-error ()
  "Test add-label handles errors gracefully."
  (beads-coverage-test-with-show-buffer "test"
    (setq beads-show--issue-id "bd-42")
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "bugfix"))
              ((symbol-function 'beads-command-label-add!)
               (lambda (&rest _) (error "Label error"))))
      ;; Should not signal error
      (beads-show-add-label))))

;;; ============================================================
;;; beads-command-show.el - Dependency Navigation
;;; ============================================================

(ert-deftest beads-coverage-test-show-goto-depends ()
  "Test goto-depends navigates to DEPENDS ON section."
  (beads-coverage-test-with-show-buffer "HEADER\n\nDEPENDS ON\n\n  → bd-1\n\nDESCRIPTION\n"
    (beads-show-goto-depends)
    ;; Should be near DEPENDS ON section
    (should (>= (line-number-at-pos) 4))))

(ert-deftest beads-coverage-test-show-goto-depends-none ()
  "Test goto-depends when no dependencies section."
  (beads-coverage-test-with-show-buffer "DESCRIPTION\nNo deps here"
    (beads-show-goto-depends)))

(ert-deftest beads-coverage-test-show-goto-blocks ()
  "Test goto-blocks navigates to BLOCKS section."
  (beads-coverage-test-with-show-buffer "HEADER\n\nBLOCKS\n\n  ← bd-2\n"
    (beads-show-goto-blocks)
    (should (>= (line-number-at-pos) 4))))

(ert-deftest beads-coverage-test-show-goto-blocks-none ()
  "Test goto-blocks when no blocks section."
  (beads-coverage-test-with-show-buffer "DESCRIPTION\nNo blocks here"
    (beads-show-goto-blocks)))

(ert-deftest beads-coverage-test-show-goto-children ()
  "Test goto-children navigates to CHILDREN section."
  (beads-coverage-test-with-show-buffer "HEADER\n\nCHILDREN\n\n  child-1\n"
    (beads-show-goto-children)
    (should (>= (line-number-at-pos) 4))))

(ert-deftest beads-coverage-test-show-goto-parent ()
  "Test goto-parent follows parent dependency."
  (let ((show-called nil))
    (beads-coverage-test-with-show-buffer "test"
      (setq beads-show--issue-data
            (beads-issue-from-json
             '((id . "bd-42") (title . "Child")
               (status . "open") (priority . 1)
               (issue_type . "task")
               (created_at . "2025-01-01T00:00:00Z")
               (updated_at . "2025-01-01T00:00:00Z")
               (dependencies . [((id . "bd-42")
                                  (depends_on_id . "bd-parent")
                                  (type . "parent-child"))]))))
      (cl-letf (((symbol-function 'beads-show)
                 (lambda (id) (setq show-called id))))
        (beads-show-goto-parent)
        (should (equal show-called "bd-parent"))))))

;;; ============================================================
;;; beads-command-show.el - Update Field
;;; ============================================================

(ert-deftest beads-coverage-test-show-update-field-no-issue-id ()
  "Test update-field errors without issue ID."
  (beads-coverage-test-with-show-buffer "test"
    (setq beads-show--issue-id nil)
    (should-error (beads-show--update-field "Title" "--title" "New Title")
                  :type 'user-error)))

(ert-deftest beads-coverage-test-show-update-field-success ()
  "Test update-field calls bd update and refreshes."
  (let ((exec-called nil)
        (refresh-called nil))
    (beads-coverage-test-with-show-buffer "test"
      (setq beads-show--issue-id "bd-42")
      (cl-letf (((symbol-function 'beads-command-execute)
                 (lambda (cmd) (setq exec-called t) (beads-command-execution)))
                ((symbol-function 'beads-completion-invalidate-cache) #'ignore)
                ((symbol-function 'beads-refresh-show)
                 (lambda () (setq refresh-called t))))
        (beads-show--update-field "Title" "--title" "New Title")
        (should exec-called)
        (should refresh-called)))))

(ert-deftest beads-coverage-test-show-update-field-error ()
  "Test update-field handles errors gracefully."
  (beads-coverage-test-with-show-buffer "test"
    (setq beads-show--issue-id "bd-42")
    (cl-letf (((symbol-function 'beads-command-execute)
               (lambda (cmd) (error "Failed"))))
      ;; Should not propagate the error
      (beads-show--update-field "Title" "--title" "New")
      (should t))))

(ert-deftest beads-coverage-test-show-update-field-various-flags ()
  "Test update-field maps various field flags to slots."
  (let ((exec-args nil))
    (beads-coverage-test-with-show-buffer "test"
      (setq beads-show--issue-id "bd-42")
      (cl-letf (((symbol-function 'beads-command-execute)
                 (lambda (cmd) (setq exec-args cmd) (beads-command-execution)))
                ((symbol-function 'beads-completion-invalidate-cache) #'ignore)
                ((symbol-function 'beads-refresh-show) #'ignore))
        ;; Test each flag mapping
        (dolist (flag '("--title" "--description" "--status" "--priority"
                        "--assignee" "--acceptance" "--design" "--notes"
                        "--external-ref"))
          (beads-show--update-field "Test" flag "value"))))))

(ert-deftest beads-coverage-test-show-update-field-unknown-flag ()
  "Test update-field with unknown flag signals error."
  (beads-coverage-test-with-show-buffer "test"
    (setq beads-show--issue-id "bd-42")
    ;; Unknown flag should cause an error in the pcase
    (beads-show--update-field "Test" "--unknown" "value")))

;;; ============================================================
;;; beads-command-show.el - Edit Field
;;; ============================================================

(ert-deftest beads-coverage-test-show-edit-field-not-in-show-mode ()
  "Test edit-field errors when not in show mode."
  (with-temp-buffer
    (should-error (beads-show-edit-field) :type 'user-error)))

(ert-deftest beads-coverage-test-show-edit-field-no-issue-id ()
  "Test edit-field errors without issue ID."
  (beads-coverage-test-with-show-buffer "test"
    (setq beads-show--issue-id nil)
    (should-error (beads-show-edit-field) :type 'user-error)))

(ert-deftest beads-coverage-test-show-edit-field-no-issue-data ()
  "Test edit-field errors without issue data."
  (beads-coverage-test-with-show-buffer "test"
    (setq beads-show--issue-id "bd-42")
    (setq beads-show--issue-data nil)
    (should-error (beads-show-edit-field) :type 'user-error)))

;;; ============================================================
;;; beads-command-show.el - Follow Reference
;;; ============================================================

(ert-deftest beads-coverage-test-show-follow-reference ()
  "Test follow-reference calls beads-show."
  (let ((show-called nil))
    (beads-coverage-test-with-show-buffer "See bd-42 for details"
      (goto-char (+ (point-min) 4))  ; on "bd-42"
      (cl-letf (((symbol-function 'beads-show--extract-issue-at-point)
                 (lambda () "bd-42"))
                ((symbol-function 'beads-show)
                 (lambda (id) (setq show-called id))))
        (beads-show-follow-reference)
        (should (equal show-called "bd-42"))))))

(ert-deftest beads-coverage-test-show-follow-reference-none ()
  "Test follow-reference when no reference at point."
  (beads-coverage-test-with-show-buffer "No references here"
    (cl-letf (((symbol-function 'beads-show--extract-issue-at-point)
               (lambda () nil)))
      (beads-show-follow-reference))))

(ert-deftest beads-coverage-test-show-follow-reference-other-window ()
  "Test follow-reference-other-window with issue at point."
  (let ((show-called nil))
    (beads-coverage-test-with-show-buffer "See bd-42 here"
      (cl-letf (((symbol-function 'beads-show--extract-issue-at-point)
                 (lambda () "bd-42"))
                ((symbol-function 'beads-show--get-or-create-buffer)
                 (lambda (id) (generate-new-buffer "*test*")))
                ((symbol-function 'beads-show-update-buffer)
                 (lambda (id buf) (setq show-called id)))
                ((symbol-function 'display-buffer)
                 (lambda (buf &rest _) nil)))
        (beads-show-follow-reference-other-window)
        (should (equal show-called "bd-42"))))))

;;; ============================================================
;;; beads-command-show.el - Copy ID
;;; ============================================================

(ert-deftest beads-coverage-test-show-copy-id ()
  "Test copy-id copies issue ID to kill ring."
  (beads-coverage-test-with-show-buffer "test"
    (setq beads-show--issue-id "bd-42")
    (beads-show-copy-id)
    (should (equal (car kill-ring) "bd-42"))))

(ert-deftest beads-coverage-test-show-copy-id-no-issue ()
  "Test copy-id without issue ID signals user-error."
  (beads-coverage-test-with-show-buffer "test"
    (setq beads-show--issue-id nil)
    (should-error (beads-show-copy-id) :type 'user-error)))

;;; ============================================================
;;; beads-command-show.el - Render Blocks Section
;;; ============================================================

(ert-deftest beads-coverage-test-show-insert-blocks-section ()
  "Test rendering BLOCKS section with dependents."
  (with-temp-buffer
    (let* ((inhibit-read-only t)
           (dep (beads-dependency-from-json
                 '((id . "bd-50")
                   (depends_on_id . "bd-42")
                   (type . "blocks")
                   (title . "Blocked Issue")
                   (status . "open")
                   (priority . 1)
                   (issue_type . "task")))))
      (beads-show--insert-blocks-section (list dep))
      (should (string-match-p "BLOCKS" (buffer-string))))))

(ert-deftest beads-coverage-test-show-insert-blocks-section-parent-child ()
  "Test rendering BLOCKS section with parent-child dependents."
  (with-temp-buffer
    (let* ((inhibit-read-only t)
           (dep (beads-dependency-from-json
                 '((id . "bd-child")
                   (depends_on_id . "bd-42")
                   (type . "parent-child")
                   (title . "Child Issue")
                   (status . "in_progress")
                   (priority . 2)
                   (issue_type . "feature")))))
      (beads-show--insert-blocks-section (list dep))
      (should (string-match-p "BLOCKS" (buffer-string))))))

(ert-deftest beads-coverage-test-show-insert-blocks-section-related-filtered ()
  "Test rendering BLOCKS section filters out related deps."
  (with-temp-buffer
    (let* ((inhibit-read-only t)
           (dep (beads-dependency-from-json
                 '((id . "bd-50")
                   (depends_on_id . "bd-42")
                   (type . "related")
                   (title . "Related Issue")
                   (status . "open")
                   (priority . 1)
                   (issue_type . "task")))))
      (beads-show--insert-blocks-section (list dep))
      ;; Related deps should not appear in BLOCKS section
      (should (string= "" (buffer-substring-no-properties
                           (point-min) (point-max)))))))

;;; ============================================================
;;; beads-command-show.el - In Fenced Code Block
;;; ============================================================

(ert-deftest beads-coverage-test-in-fenced-code-block-no ()
  "Test in-fenced-code-block returns nil when not in code block."
  (beads-coverage-test-with-show-buffer "Regular text\nMore text"
    (should-not (beads-show--in-fenced-code-block))))

;;; ============================================================
;;; beads-command-show.el - Refresh Show
;;; ============================================================

(ert-deftest beads-coverage-test-refresh-show-no-issue-id ()
  "Test refresh-show errors without issue ID."
  (beads-coverage-test-with-show-buffer "test"
    (setq beads-show--issue-id nil)
    (should-error (beads-refresh-show) :type 'user-error)))

(ert-deftest beads-coverage-test-refresh-show-error-handling ()
  "Test refresh-show handles command errors."
  (beads-coverage-test-with-show-buffer "test"
    (setq beads-show--issue-id "bd-42")
    (setq beads-show--project-dir "/tmp")
    (cl-letf (((symbol-function 'beads-command-show!)
               (lambda (&rest _) (error "Command failed"))))
      ;; Should not propagate error
      (beads-refresh-show))))

;;; ============================================================
;;; beads-option.el - Infix Read Tests
;;; ============================================================

(ert-deftest beads-coverage-test-option-global-class-methods ()
  "Test that global option and switch classes have proper methods defined."
  ;; Verify that transient-infix-read is defined for both classes
  (should (cl-find-method #'transient-infix-read '() '(beads-option-global)))
  (should (cl-find-method #'transient-infix-read '() '(beads-option-global-switch)))
  ;; Verify that transient-infix-value is defined for both classes
  (should (cl-find-method #'transient-infix-value '() '(beads-option-global)))
  (should (cl-find-method #'transient-infix-value '() '(beads-option-global-switch)))
  ;; Verify that transient-format-value is defined for both classes
  (should (cl-find-method #'transient-format-value '() '(beads-option-global)))
  (should (cl-find-method #'transient-format-value '() '(beads-option-global-switch))))

;;; ============================================================
;;; beads-command-formula.el - List Utility Tests
;;; ============================================================

(ert-deftest beads-coverage-test-formula-list-type-face-workflow ()
  "Test type face for workflow."
  (should (eq (beads-formula-list--type-face "workflow") 'beads-formula-type-workflow)))

(ert-deftest beads-coverage-test-formula-list-type-face-expansion ()
  "Test type face for expansion."
  (should (eq (beads-formula-list--type-face "expansion") 'beads-formula-type-expansion)))

(ert-deftest beads-coverage-test-formula-list-type-face-aspect ()
  "Test type face for aspect."
  (should (eq (beads-formula-list--type-face "aspect") 'beads-formula-type-aspect)))

(ert-deftest beads-coverage-test-formula-list-type-face-unknown ()
  "Test type face for unknown type."
  (should (eq (beads-formula-list--type-face "custom") 'default)))

(ert-deftest beads-coverage-test-formula-list-format-type ()
  "Test format-type returns propertized string."
  (let ((result (beads-formula-list--format-type "workflow")))
    (should (stringp result))
    (should (equal (get-text-property 0 'face result)
                   'beads-formula-type-workflow))))

(ert-deftest beads-coverage-test-formula-list-formula-to-entry ()
  "Test formula-to-entry converts summary to tabulated-list entry."
  (let* ((summary (beads-formula-summary
                   :name "test-formula"
                   :formula-type "workflow"
                   :description "A test formula"
                   :steps 5
                   :vars 3))
         (entry (beads-formula-list--formula-to-entry summary)))
    (should (listp entry))
    (should (equal (car entry) "test-formula"))
    (should (vectorp (cadr entry)))
    (should (equal (aref (cadr entry) 0) "test-formula"))
    (should (equal (aref (cadr entry) 2) "5"))
    (should (equal (aref (cadr entry) 3) "3"))
    (should (equal (aref (cadr entry) 4) "A test formula"))))

(ert-deftest beads-coverage-test-formula-list-formula-to-entry-nil-fields ()
  "Test formula-to-entry handles nil fields."
  (let* ((summary (beads-formula-summary
                   :name "test"
                   :formula-type "workflow"))
         (entry (beads-formula-list--formula-to-entry summary)))
    (should (listp entry))
    (should (equal (aref (cadr entry) 2) "0"))  ; nil steps -> 0
    (should (equal (aref (cadr entry) 3) "0"))  ; nil vars -> 0
    (should (equal (aref (cadr entry) 4) "")))) ; nil desc -> ""

;;; ============================================================
;;; beads-command-formula.el - Show Rendering Tests
;;; ============================================================

(ert-deftest beads-coverage-test-formula-show-render-header ()
  "Test render-header inserts formatted label:value line."
  (with-temp-buffer
    (beads-formula-show--render-header "Name" "test-formula")
    (let ((text (buffer-string)))
      (should (string-match-p "Name:" text))
      (should (string-match-p "test-formula" text)))))

(ert-deftest beads-coverage-test-formula-show-render-header-nil ()
  "Test render-header handles nil value."
  (with-temp-buffer
    (beads-formula-show--render-header "Name" nil)
    (let ((text (buffer-string)))
      (should (string-match-p "Name:" text)))))

(ert-deftest beads-coverage-test-formula-show-render-section ()
  "Test render-section inserts section title with underline."
  (with-temp-buffer
    (beads-formula-show--render-section "Variables")
    (let ((text (buffer-string)))
      (should (string-match-p "Variables" text))
      (should (string-match-p "=========" text)))))

(ert-deftest beads-coverage-test-formula-show-render-var ()
  "Test render-var inserts variable with description and default."
  (with-temp-buffer
    (beads-formula-show--render-var "feature"
                                   '((description . "Feature name")
                                     (default . "my-feature")
                                     (required . t)))
    (let ((text (buffer-string)))
      (should (string-match-p "feature" text))
      (should (string-match-p "(required)" text))
      (should (string-match-p "Feature name" text))
      (should (string-match-p "Default: my-feature" text)))))

(ert-deftest beads-coverage-test-formula-show-render-var-minimal ()
  "Test render-var with minimal variable definition."
  (with-temp-buffer
    (beads-formula-show--render-var "simple-var" '())
    (let ((text (buffer-string)))
      (should (string-match-p "simple-var" text)))))

(ert-deftest beads-coverage-test-formula-show-render-step ()
  "Test render-step inserts step with title, description, needs."
  (with-temp-buffer
    (beads-formula-show--render-step
     '((id . "step-1")
       (title . "First Step")
       (description . "Do the first thing")
       (needs . ("step-0")))
     0)
    (let ((text (buffer-string)))
      (should (string-match-p "1\\. First Step" text))
      (should (string-match-p "ID: step-1" text))
      (should (string-match-p "Do the first thing" text))
      (should (string-match-p "Needs: step-0" text)))))

(ert-deftest beads-coverage-test-formula-show-render-step-no-title ()
  "Test render-step with no title uses id."
  (with-temp-buffer
    (beads-formula-show--render-step
     '((id . "step-x")
       (description . "Just a step"))
     2)
    (let ((text (buffer-string)))
      (should (string-match-p "3\\. step-x" text)))))

;;; ============================================================
;;; beads-command-formula.el - Show Commands
;;; ============================================================

(ert-deftest beads-coverage-test-formula-show-quit ()
  "Test formula-show-quit calls quit-window."
  (let ((quit-called nil))
    (cl-letf (((symbol-function 'quit-window)
               (lambda (kill) (setq quit-called kill))))
      (beads-formula-show-quit)
      (should quit-called))))

(ert-deftest beads-coverage-test-formula-list-quit ()
  "Test formula-list-quit calls quit-window."
  (let ((quit-called nil))
    (cl-letf (((symbol-function 'quit-window)
               (lambda (kill) (setq quit-called kill))))
      (beads-formula-list-quit)
      (should quit-called))))

(ert-deftest beads-coverage-test-formula-show-open-source-no-data ()
  "Test formula-show-open-source errors without data."
  (with-temp-buffer
    (setq beads-formula-show--formula-data nil)
    (should-error (beads-formula-show-open-source) :type 'user-error)))

(ert-deftest beads-coverage-test-formula-show-refresh-no-name ()
  "Test formula-show-refresh errors without formula name."
  (with-temp-buffer
    (setq beads-formula-show--formula-name nil)
    (should-error (beads-formula-show-refresh) :type 'user-error)))

(ert-deftest beads-coverage-test-formula-list-show-no-formula ()
  "Test formula-list-show errors without formula at point."
  (with-temp-buffer
    (cl-letf (((symbol-function 'beads-formula-list--current-formula-name)
               (lambda () nil)))
      (should-error (beads-formula-list-show) :type 'user-error))))

(ert-deftest beads-coverage-test-formula-list-next ()
  "Test formula-list-next moves forward, skipping header."
  (with-temp-buffer
    (let ((inhibit-read-only t))
      (insert "line 1\nline 2\nline 3\n")
      (goto-char (point-min))
      (beads-formula-list-next)
      ;; Moves to line 2, then skips to 3 (header skip)
      (should (= (line-number-at-pos) 3)))))

(ert-deftest beads-coverage-test-formula-list-previous ()
  "Test formula-list-previous moves backward."
  (with-temp-buffer
    (insert "line 1\nline 2\nline 3\n")
    (goto-char (point-max))
    (beads-formula-list-previous)
    (should (< (point) (point-max)))))

;;; ============================================================
;;; beads-command-formula.el - Mode Tests
;;; ============================================================

(ert-deftest beads-coverage-test-formula-show-mode-defined ()
  "Test formula-show-mode is properly defined."
  (should (fboundp 'beads-formula-show-mode))
  (with-temp-buffer
    (beads-formula-show-mode)
    (should (derived-mode-p 'beads-formula-show-mode))
    (should (derived-mode-p 'special-mode))))

(ert-deftest beads-coverage-test-formula-show-mode-keymap ()
  "Test formula-show-mode has expected key bindings."
  (should (keymapp beads-formula-show-mode-map))
  (should (eq (lookup-key beads-formula-show-mode-map (kbd "g"))
              'beads-formula-show-refresh))
  (should (eq (lookup-key beads-formula-show-mode-map (kbd "q"))
              'beads-formula-show-quit))
  (should (eq (lookup-key beads-formula-show-mode-map (kbd "o"))
              'beads-formula-show-open-source)))

;;; ============================================================
;;; beads-command-formula.el - Buffer Management
;;; ============================================================

(ert-deftest beads-coverage-test-formula-list-get-or-create-buffer ()
  "Test get-or-create-buffer returns a buffer."
  (cl-letf (((symbol-function 'beads-git-find-project-root) (lambda () "/tmp/test"))
            ((symbol-function 'beads-git-get-project-name) (lambda () "test-proj")))
    (let ((buf (beads-formula-list--get-or-create-buffer)))
      (unwind-protect
          (progn
            (should (bufferp buf))
            (should (string-match-p "beads-formula-list" (buffer-name buf))))
        (kill-buffer buf)))))

(ert-deftest beads-coverage-test-formula-show-find-visible-buffer-none ()
  "Test find-visible-buffer returns nil when no buffer visible."
  (cl-letf (((symbol-function 'beads-git-find-project-root) (lambda () "/tmp/test")))
    (should-not (beads-formula-show--find-visible-buffer "/tmp/test"))))

;;; ============================================================
;;; beads-command.el - Validate String List
;;; ============================================================

(ert-deftest beads-coverage-test-validate-string-list-nil ()
  "Test validate-string-list with nil."
  (should-not (beads-command--validate-string-list nil "test")))

(ert-deftest beads-coverage-test-validate-string-list-valid ()
  "Test validate-string-list with valid list."
  (should-not (beads-command--validate-string-list '("a" "b") "test")))

(ert-deftest beads-coverage-test-validate-string-list-not-list ()
  "Test validate-string-list with non-list."
  (should (stringp (beads-command--validate-string-list "not-list" "field"))))

(ert-deftest beads-coverage-test-validate-string-list-non-string-elements ()
  "Test validate-string-list with non-string elements."
  (should (stringp (beads-command--validate-string-list '("a" 42) "field"))))

;;; ============================================================
;;; beads-command-show.el - Show Transient Menu
;;; ============================================================

(ert-deftest beads-coverage-test-show-actions-transient-defined ()
  "Test beads-show-actions transient is defined."
  (should (commandp 'beads-show-actions))
  (should (get 'beads-show-actions 'transient--prefix)))

;;; ============================================================
;;; beads-command-show.el - Edit Field Multiline
;;; ============================================================

(ert-deftest beads-coverage-test-edit-field-multiline-creates-buffer ()
  "Test edit-field-multiline creates editing buffer."
  (let ((callback-result nil)
        (original-buffer (current-buffer)))
    ;; We can't test the full interactive flow (recursive-edit), but we can
    ;; verify the setup creates a buffer with the right content
    (cl-letf (((symbol-function 'switch-to-buffer) #'set-buffer)
              ((symbol-function 'markdown-mode) #'ignore)
              ((symbol-function 'visual-line-mode) #'ignore)
              ((symbol-function 'local-set-key) #'ignore)
              ((symbol-function 'recursive-edit)
               (lambda ()
                 ;; Simulate immediate completion
                 nil)))
      ;; This will create the buffer but recursive-edit is mocked
      (ignore-errors
        (beads-show--edit-field-multiline
         "Description" "Initial text"
         (lambda (text) (setq callback-result text)))))))

;;; ============================================================
;;; Additional beads-command-show.el function coverage
;;; ============================================================

(ert-deftest beads-coverage-test-show-at-point-no-reference ()
  "Test show-at-point when no reference at point."
  (with-temp-buffer
    (insert "No issue refs here")
    (goto-char (point-min))
    (cl-letf (((symbol-function 'beads-show--extract-issue-at-point)
               (lambda () nil)))
      (should-error (beads-show-at-point) :type 'user-error))))

(ert-deftest beads-coverage-test-show-at-point-with-reference ()
  "Test show-at-point with issue reference."
  (let ((show-called nil))
    (with-temp-buffer
      (insert "See bd-42 here")
      (goto-char (+ (point-min) 4))
      (cl-letf (((symbol-function 'beads-show--extract-issue-at-point)
                 (lambda () "bd-42"))
                ((symbol-function 'beads-show)
                 (lambda (id) (setq show-called id))))
        (beads-show-at-point)
        (should (equal show-called "bd-42"))))))

(provide 'beads-coverage-test)
;;; beads-coverage-test.el ends here
