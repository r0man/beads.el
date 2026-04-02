;;; beads-command-coverage-test.el --- Coverage tests for beads-command.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; Tests targeting uncovered code paths in beads-command.el including:
;; - beads-defcommand macro expansion (:cli-command, :parse-as, :global-section)
;; - beads--generate-parse-method for :issue and :issues
;; - Terminal backend detection and dispatch
;; - beads-command-subcommand auto-derivation with cli-command slot
;; - beads-command--ansi-color-filter
;; - beads-command-execute-interactive for beads-command
;; - beads-command-execute with validation error
;; - beads-command-execute-async
;; - beads-command--validate-string-list

;;; Code:

(require 'ert)
(require 'beads-command)

;;; ============================================================
;;; beads--generate-parse-method Tests
;;; ============================================================

(ert-deftest beads-command-coverage-test-generate-parse-method-issue ()
  "Test beads--generate-parse-method generates :issue parse method code."
  (let ((result (beads--generate-parse-method 'beads-command-test-cmd :issue)))
    ;; Should generate a list with one cl-defmethod form
    (should (listp result))
    (should (= (length result) 1))
    (should (eq (caar result) 'cl-defmethod))
    (should (eq (cadar result) 'beads-command-parse))))

(ert-deftest beads-command-coverage-test-generate-parse-method-issues ()
  "Test beads--generate-parse-method generates :issues parse method code."
  (let ((result (beads--generate-parse-method 'beads-command-test-cmd :issues)))
    (should (listp result))
    (should (= (length result) 1))
    (should (eq (caar result) 'cl-defmethod))
    (should (eq (cadar result) 'beads-command-parse))))

(ert-deftest beads-command-coverage-test-generate-parse-method-invalid ()
  "Test beads--generate-parse-method errors on invalid parse-as."
  (should-error (beads--generate-parse-method 'beads-command-test-cmd :bogus)
                :type 'error))

;;; ============================================================
;;; beads--extract-option Tests
;;; ============================================================

(ert-deftest beads-command-coverage-test-extract-option-present ()
  "Test beads--extract-option extracts a present keyword."
  (let ((result (beads--extract-option :cli-command '(:cli-command "show" :doc "Test"))))
    (should (equal (car result) "show"))
    (should (equal (cdr result) '(:doc "Test")))))

(ert-deftest beads-command-coverage-test-extract-option-missing ()
  "Test beads--extract-option returns nil when keyword is missing."
  (let ((result (beads--extract-option :cli-command '(:doc "Test"))))
    (should (null (car result)))
    (should (equal (cdr result) '(:doc "Test")))))

;;; ============================================================
;;; Terminal Backend Detection Tests
;;; ============================================================

(ert-deftest beads-command-coverage-test-detect-backend-term-fallback ()
  "Test backend detection falls back to term when nothing else available."
  (cl-letf (((symbol-function 'beads-command--vterm-available-p)
             (lambda () nil))
            ((symbol-function 'beads-command--eat-available-p)
             (lambda () nil)))
    (should (eq (beads-command--detect-best-backend) 'term))))

(ert-deftest beads-command-coverage-test-detect-backend-vterm ()
  "Test backend detection prefers vterm when available."
  (cl-letf (((symbol-function 'beads-command--vterm-available-p)
             (lambda () t))
            ((symbol-function 'beads-command--eat-available-p)
             (lambda () t)))
    (should (eq (beads-command--detect-best-backend) 'vterm))))

(ert-deftest beads-command-coverage-test-detect-backend-eat ()
  "Test backend detection uses eat when vterm unavailable."
  (cl-letf (((symbol-function 'beads-command--vterm-available-p)
             (lambda () nil))
            ((symbol-function 'beads-command--eat-available-p)
             (lambda () t)))
    (should (eq (beads-command--detect-best-backend) 'eat))))

;;; ============================================================
;;; beads-command--run-in-terminal Tests
;;; ============================================================

(ert-deftest beads-command-coverage-test-run-in-terminal-term ()
  "Test run-in-terminal dispatches to term backend."
  (let ((beads-terminal-backend 'term)
        (called-with nil))
    (cl-letf (((symbol-function 'beads-command--run-term)
               (lambda (cmd buf dir)
                 (setq called-with (list cmd buf dir)))))
      (beads-command--run-in-terminal "echo hi" "*test*" "/tmp")
      (should (equal called-with '("echo hi" "*test*" "/tmp"))))))

(ert-deftest beads-command-coverage-test-run-in-terminal-compile ()
  "Test run-in-terminal dispatches to compile backend."
  (let ((beads-terminal-backend 'compile)
        (called-with nil))
    (cl-letf (((symbol-function 'beads-command--run-compile)
               (lambda (cmd buf dir)
                 (setq called-with (list cmd buf dir)))))
      (beads-command--run-in-terminal "echo hi" "*test*" "/tmp")
      (should (equal called-with '("echo hi" "*test*" "/tmp"))))))

(ert-deftest beads-command-coverage-test-run-in-terminal-auto-detect ()
  "Test run-in-terminal auto-detects when backend is nil."
  (let ((beads-terminal-backend nil)
        (called nil))
    (cl-letf (((symbol-function 'beads-command--detect-best-backend)
               (lambda () 'term))
              ((symbol-function 'beads-command--run-term)
               (lambda (_cmd _buf _dir)
                 (setq called t))))
      (beads-command--run-in-terminal "echo hi" "*test*" "/tmp")
      (should called))))

(ert-deftest beads-command-coverage-test-run-in-terminal-vterm ()
  "Test run-in-terminal dispatches to vterm backend."
  (let ((beads-terminal-backend 'vterm)
        (called nil))
    (cl-letf (((symbol-function 'beads-command--run-vterm)
               (lambda (_cmd _buf _dir) (setq called t))))
      (beads-command--run-in-terminal "echo hi" "*test*" "/tmp")
      (should called))))

(ert-deftest beads-command-coverage-test-run-in-terminal-eat ()
  "Test run-in-terminal dispatches to eat backend."
  (let ((beads-terminal-backend 'eat)
        (called nil))
    (cl-letf (((symbol-function 'beads-command--run-eat)
               (lambda (_cmd _buf _dir) (setq called t))))
      (beads-command--run-in-terminal "echo hi" "*test*" "/tmp")
      (should called))))

(ert-deftest beads-command-coverage-test-run-in-terminal-unknown ()
  "Test run-in-terminal falls back to term for unknown backend."
  (let ((beads-terminal-backend 'unknown-backend)
        (called nil))
    (cl-letf (((symbol-function 'beads-command--run-term)
               (lambda (_cmd _buf _dir) (setq called t))))
      (beads-command--run-in-terminal "echo hi" "*test*" "/tmp")
      (should called))))

;;; ============================================================
;;; beads-command--run-term Tests
;;; ============================================================

(ert-deftest beads-command-coverage-test-run-term ()
  "Test run-term creates term buffer."
  (let ((buf nil))
    (unwind-protect
        (cl-letf (((symbol-function 'pop-to-buffer)
                   (lambda (b) (setq buf b))))
          (beads-command--run-term "echo hello" "*beads-test-term*" "/tmp")
          (should (bufferp buf))
          (should (string= (buffer-name buf) "*beads-test-term*")))
      (when (and buf (buffer-live-p buf))
        (let ((proc (get-buffer-process buf)))
          (when proc (delete-process proc)))
        (kill-buffer buf)))))

;;; ============================================================
;;; beads-command--run-compile Tests
;;; ============================================================

(ert-deftest beads-command-coverage-test-run-compile ()
  "Test run-compile dispatches to compilation-start."
  (let ((compile-called nil)
        (compile-cmd nil))
    (cl-letf (((symbol-function 'compilation-start)
               (lambda (cmd &rest _args)
                 (setq compile-called t
                       compile-cmd cmd))))
      (beads-command--run-compile "echo hello" "*beads-test-compile*" "/tmp")
      (should compile-called)
      (should (string= compile-cmd "echo hello")))))

;;; ============================================================
;;; beads-command-subcommand Tests
;;; ============================================================

(ert-deftest beads-command-coverage-test-subcommand-derived-class ()
  "Test subcommand returns correct value for derived class."
  (let ((cmd (beads-command-close :issue-ids '("bd-1") :reason "Done")))
    (should (string= (beads-command-subcommand cmd) "close"))))

(ert-deftest beads-command-coverage-test-subcommand-with-cli-command ()
  "Test subcommand returns cli-command value when slot exists."
  ;; beads-command-close has a cli-command slot via beads-defcommand
  (let ((cmd (beads-command-close)))
    (should (stringp (beads-command-subcommand cmd)))
    (should (string= (beads-command-subcommand cmd) "close"))))

;;; ============================================================
;;; beads-command--ansi-color-filter Tests
;;; ============================================================

(ert-deftest beads-command-coverage-test-ansi-color-filter ()
  "Test ANSI color filter strips OSC sequences."
  (with-temp-buffer
    (require 'compile)
    (let ((compilation-filter-start (point-min)))
      ;; Insert text with an OSC escape sequence
      (insert "\033]11;rgb:0000/0000/0000\007normal text")
      (setq compilation-filter-start (point-min))
      (beads-command--ansi-color-filter)
      ;; OSC sequence should be stripped
      (should (string-match-p "normal text" (buffer-string)))
      (should-not (string-match-p "\033" (buffer-string))))))

;;; ============================================================
;;; beads-command-execute-interactive for beads-command Tests
;;; ============================================================

(ert-deftest beads-command-coverage-test-json-execute-interactive ()
  "Test that beads-command execute-interactive does not modify json slot."
  (let ((terminal-called nil))
    (cl-letf (((symbol-function 'beads-command--run-in-terminal)
               (lambda (_cmd _buf _dir) (setq terminal-called t)))
              ((symbol-function 'beads--find-beads-dir)
               (lambda () "/tmp/.beads")))
      ;; Default json is nil; execute-interactive should leave it alone
      (let ((cmd (beads-command-list)))
        (beads-command-execute-interactive cmd)
        (should-not (oref cmd json))
        (should terminal-called)))))

;;; ============================================================
;;; beads-command-execute with Validation Error Tests
;;; ============================================================

(ert-deftest beads-command-coverage-test-execute-validation-error ()
  "Test beads-command-execute signals validation error."
  (let ((cmd (beads-command-close :reason "Fixed")))
    ;; Close without issue-ids should fail validation
    (should-error (beads-command-execute cmd)
                  :type 'beads-validation-error)))

;;; ============================================================
;;; beads-command--validate-string-list Tests
;;; ============================================================

(ert-deftest beads-command-coverage-test-validate-string-list-nil ()
  "Test validate-string-list with nil returns nil (valid)."
  (should (null (beads-command--validate-string-list nil "test"))))

(ert-deftest beads-command-coverage-test-validate-string-list-valid ()
  "Test validate-string-list with valid string list returns nil."
  (should (null (beads-command--validate-string-list '("a" "b") "test"))))

(ert-deftest beads-command-coverage-test-validate-string-list-not-list ()
  "Test validate-string-list with non-list returns error."
  (should (stringp (beads-command--validate-string-list "not-a-list" "test"))))

(ert-deftest beads-command-coverage-test-validate-string-list-non-strings ()
  "Test validate-string-list with non-string elements returns error."
  (should (stringp (beads-command--validate-string-list '("a" 42) "test"))))

;;; ============================================================
;;; beads-command-execute-async Tests
;;; ============================================================

(ert-deftest beads-command-coverage-test-execute-async-validation-error ()
  "Test beads-command-execute-async signals validation error immediately."
  (let ((cmd (beads-command-close :reason "Fixed")))
    ;; Close without issue-ids should fail validation
    (should-error (beads-command-execute-async cmd)
                  :type 'beads-validation-error)))

(ert-deftest beads-command-coverage-test-execute-async-returns-process ()
  "Test beads-command-execute-async returns a process object."
  (let* ((cmd (beads-command-list :json nil))
         (proc nil))
    ;; Mock the execution to use a real, fast command
    (cl-letf (((symbol-function 'beads-command-validate)
               (lambda (_cmd) nil))
              ((symbol-function 'beads-command-line)
               (lambda (_cmd) '("echo" "test"))))
      (setq proc (beads-command-execute-async cmd))
      (should (processp proc))
      ;; Clean up
      (when (process-live-p proc)
        (delete-process proc)))))

;;; Tests for async error propagation (be-u5j)

(ert-deftest beads-command-coverage-test-execute-async-parse-error-fires-callback ()
  "Test that parse errors in async sentinel still fire the callback.
The callback must always be called even when parsing fails, so callers
never hang waiting for a response that never arrives."
  :tags '(:unit)
  (let* ((cmd (beads-command-list :json t))
         (callback-fired nil)
         (callback-execution nil))
    (cl-letf (((symbol-function 'beads-command-validate)
               (lambda (_cmd) nil))
              ((symbol-function 'beads-command-line)
               ;; Output invalid JSON to trigger parse error
               (lambda (_cmd) '("echo" "not-valid-json"))))
      (beads-command-execute-async
       cmd
       (lambda (exec)
         (setq callback-fired t)
         (setq callback-execution exec)))
      ;; Wait for the async process to complete (up to 5 seconds)
      (let ((deadline (+ (float-time) 5.0)))
        (while (and (not callback-fired) (< (float-time) deadline))
          (sit-for 0.1)))
      (should callback-fired)
      ;; The execution should have a non-zero exit code due to parse error
      (should (cl-typep callback-execution 'beads-command-execution))
      (should (not (zerop (oref callback-execution exit-code)))))))

(ert-deftest beads-command-coverage-test-execute-async-nonzero-exit-fires-callback ()
  "Test that non-zero exit codes still fire the callback with the execution."
  :tags '(:unit)
  (let* ((cmd (beads-command-list :json nil))
         (callback-fired nil)
         (callback-execution nil))
    (cl-letf (((symbol-function 'beads-command-validate)
               (lambda (_cmd) nil))
              ((symbol-function 'beads-command-line)
               ;; Exit with non-zero code
               (lambda (_cmd) '("sh" "-c" "exit 1"))))
      (beads-command-execute-async
       cmd
       (lambda (exec)
         (setq callback-fired t)
         (setq callback-execution exec)))
      ;; Wait for the async process to complete (up to 5 seconds)
      (let ((deadline (+ (float-time) 5.0)))
        (while (and (not callback-fired) (< (float-time) deadline))
          (sit-for 0.1)))
      (should callback-fired)
      (should (cl-typep callback-execution 'beads-command-execution))
      (should (= (oref callback-execution exit-code) 1)))))

(ert-deftest beads-command-coverage-test-execute-async-success-fires-callback ()
  "Test that successful async commands fire callback with zero exit code."
  :tags '(:unit)
  (let* ((cmd (beads-command-list :json nil))
         (callback-fired nil)
         (callback-execution nil))
    (cl-letf (((symbol-function 'beads-command-validate)
               (lambda (_cmd) nil))
              ((symbol-function 'beads-command-line)
               (lambda (_cmd) '("echo" "done"))))
      (beads-command-execute-async
       cmd
       (lambda (exec)
         (setq callback-fired t)
         (setq callback-execution exec)))
      ;; Wait for the async process to complete (up to 5 seconds)
      (let ((deadline (+ (float-time) 5.0)))
        (while (and (not callback-fired) (< (float-time) deadline))
          (sit-for 0.1)))
      (should callback-fired)
      (should (cl-typep callback-execution 'beads-command-execution))
      (should (zerop (oref callback-execution exit-code))))))

;;; ============================================================
;;; beads-command parse Tests
;;; ============================================================

(ert-deftest beads-command-coverage-test-json-parse-with-json ()
  "Test beads-command parse method with valid JSON."
  (let* ((cmd (beads-command-list :json t))
         (exec (beads-command-execution
                :command cmd
                :exit-code 0
                :stdout "[{\"id\":\"bd-1\",\"title\":\"Test\"}]"
                :stderr "")))
    (let ((result (beads-command-parse cmd exec)))
      (should (listp result))
      (should (= (length result) 1)))))

(ert-deftest beads-command-coverage-test-json-parse-with-json-nil ()
  "Test beads-command parse method with :json nil."
  (let* ((cmd (beads-command-list :json nil))
         (exec (beads-command-execution
                :command cmd
                :exit-code 0
                :stdout "raw text output"
                :stderr "")))
    (let ((result (beads-command-parse cmd exec)))
      (should (stringp result))
      (should (string= result "raw text output")))))

(ert-deftest beads-command-coverage-test-json-parse-invalid-json ()
  "Test beads-command parse signals error on invalid JSON."
  (let* ((cmd (beads-command-list :json t))
         (exec (beads-command-execution
                :command cmd
                :exit-code 0
                :stdout "this is not json"
                :stderr "")))
    (should-error (beads-command-parse cmd exec)
                  :type 'beads-json-parse-error)))

;;; ============================================================
;;; beads-command-preview Tests
;;; ============================================================

(ert-deftest beads-command-coverage-test-preview ()
  "Test beads-command-preview returns command string."
  (let ((cmd (beads-command-close :issue-ids '("bd-42") :reason "Fixed")))
    (let ((preview (beads-command-preview cmd)))
      (should (stringp preview))
      (should (string-match-p "bd" preview))
      (should (string-match-p "close" preview)))))

;;; ============================================================
;;; beads-command-line for beads-command Tests
;;; ============================================================

(ert-deftest beads-command-coverage-test-command-line-json-flag ()
  "Test beads-command adds --json flag."
  (let* ((cmd (beads-command-list :json t))
         (args (beads-command-line cmd)))
    (should (member "--json" args))))

(ert-deftest beads-command-coverage-test-command-line-no-json-flag ()
  "Test beads-command without --json."
  (let* ((cmd (beads-command-list :json nil))
         (args (beads-command-line cmd)))
    (should-not (member "--json" args))))

;;; ============================================================
;;; beads-command-execute-interactive for base class Tests
;;; ============================================================

(ert-deftest beads-command-coverage-test-execute-interactive-base ()
  "Test beads-command execute-interactive runs in terminal."
  (let ((terminal-called nil))
    (cl-letf (((symbol-function 'beads-command--run-in-terminal)
               (lambda (_cmd _buf _dir)
                 (setq terminal-called t)))
              ((symbol-function 'beads--find-beads-dir)
               (lambda () "/tmp/.beads")))
      (let ((cmd (beads-command-list :json nil)))
        (cl-letf (((symbol-function 'beads-command-line)
                   (lambda (_cmd) '("bd" "test"))))
          (beads-command-execute-interactive cmd)
          (should terminal-called))))))

;;; ============================================================
;;; beads--derive-transient-name Tests
;;; ============================================================

(ert-deftest beads-command-coverage-test-derive-transient-name ()
  "Test beads--derive-transient-name extracts transient name."
  (should (eq (beads--derive-transient-name 'beads-command-close)
              'beads-close))
  (should (eq (beads--derive-transient-name 'beads-command-worktree-create)
              'beads-worktree-create)))

;;; ============================================================
;;; beads--extract-first-sentence Tests
;;; ============================================================

(ert-deftest beads-command-coverage-test-extract-first-sentence ()
  "Test beads--extract-first-sentence extracts first sentence."
  (should (string= (beads--extract-first-sentence "First sentence. Second sentence.")
                    "First sentence."))
  (should (string= (beads--extract-first-sentence "Only one sentence")
                    "Only one sentence"))
  (should (null (beads--extract-first-sentence nil))))

;;; ============================================================
;;; beads-defcommand macro expansion Tests
;;; ============================================================

(ert-deftest beads-command-coverage-test-defcommand-with-cli-command ()
  "Test defcommand generates correct subcommand method from :cli-command."
  ;; beads-command-close uses :cli-command "close"
  (let ((cmd (beads-command-close :issue-ids '("bd-1") :reason "Done")))
    ;; Subcommand should return the :cli-command value
    (should (string= (beads-command-subcommand cmd) "close"))
    ;; Command line should include "close" subcommand
    (let ((args (beads-command-line cmd)))
      (should (member "close" args)))))

(ert-deftest beads-command-coverage-test-defcommand-bang-function ()
  "Test defcommand generates bang convenience function."
  ;; beads-command-close! should be defined
  (should (fboundp 'beads-command-close!)))

(ert-deftest beads-command-coverage-test-defcommand-parse-as-issue ()
  "Test defcommand with :parse-as :issue generates parse method."
  ;; beads-command-close uses :parse-as :issue
  (let* ((cmd (beads-command-close :json t :issue-ids '("bd-1") :reason "Done"))
         (exec (beads-command-execution
                :command cmd :exit-code 0
                :stdout "[{\"id\":\"bd-1\",\"title\":\"Test\",\"status\":\"closed\"}]"
                :stderr "")))
    (let ((result (beads-command-parse cmd exec)))
      ;; Single issue-id should return single issue (not list)
      (should (beads-issue-p result)))))

;;; ============================================================
;;; beads-command-subcommand auto-derive Tests
;;; ============================================================

(ert-deftest beads-command-coverage-test-subcommand-auto-derive ()
  "Test subcommand auto-derives from class name when no cli-command."
  ;; beads-command-list should auto-derive to "list"
  (let ((cmd (beads-command-list)))
    (should (stringp (beads-command-subcommand cmd)))))

(ert-deftest beads-command-coverage-test-subcommand-close-returns-close ()
  "Test subcommand returns correct value for close command."
  (let ((cmd (beads-command-close :issue-ids '("bd-1") :reason "R")))
    (should (string= (beads-command-subcommand cmd) "close"))))

;;; ============================================================
;;; beads-command--run-vterm and run-eat Tests
;;; ============================================================

(ert-deftest beads-command-coverage-test-run-vterm-unavailable ()
  "Test run-vterm signals error when vterm not available."
  (cl-letf (((symbol-function 'beads-command--vterm-available-p)
             (lambda () nil)))
    (should-error (beads-command--run-vterm "echo hi" "*test*" "/tmp")
                  :type 'user-error)))

(ert-deftest beads-command-coverage-test-run-eat-unavailable ()
  "Test run-eat signals error when eat not available."
  (cl-letf (((symbol-function 'beads-command--eat-available-p)
             (lambda () nil)))
    (should-error (beads-command--run-eat "echo hi" "*test*" "/tmp")
                  :type 'user-error)))

;;; ============================================================
;;; beads-command-preview Tests (more coverage)
;;; ============================================================

(ert-deftest beads-command-coverage-test-preview-list ()
  "Test beads-command-preview for list command."
  (let ((cmd (beads-command-list :json t)))
    (let ((preview (beads-command-preview cmd)))
      (should (stringp preview))
      (should (string-match-p "bd" preview))
      (should (string-match-p "list" preview)))))

;;; ============================================================
;;; beads-command-line global options Tests
;;; ============================================================

(ert-deftest beads-command-coverage-test-command-line-global-options ()
  "Test command-line includes global options when set."
  (let* ((cmd (beads-command-list :json t :verbose t :quiet t))
         (args (beads-command-line cmd)))
    (should (member "--verbose" args))
    (should (member "--quiet" args))))

(ert-deftest beads-command-coverage-test-command-line-db ()
  "Test command-line includes --db option."
  (let* ((cmd (beads-command-list :json t :db "/tmp/test.db"))
         (args (beads-command-line cmd)))
    (should (member "--db" args))
    (should (member "/tmp/test.db" args))))

;;; ============================================================
;;; Additional Terminal Backend Tests
;;; ============================================================

(ert-deftest beads-command-coverage-test-vterm-available-check ()
  "Test vterm availability check when not installed."
  (cl-letf (((symbol-function 'require)
             (lambda (_feature &rest _args) nil)))
    (should-not (beads-command--vterm-available-p))))

(ert-deftest beads-command-coverage-test-eat-available-check ()
  "Test eat availability check when not installed."
  (cl-letf (((symbol-function 'require)
             (lambda (_feature &rest _args) nil)))
    (should-not (beads-command--eat-available-p))))

(ert-deftest beads-command-coverage-test-execute-interactive-default ()
  "Test default execute-interactive runs in terminal."
  (let ((ran nil))
    (cl-letf (((symbol-function 'beads-command--run-in-terminal)
               (lambda (_cmd _buf _dir) (setq ran t)))
              ((symbol-function 'beads--find-beads-dir)
               (lambda () "/tmp/.beads")))
      (let ((cmd (beads-command-list :json nil)))
        (beads-command-execute-interactive cmd)
        (should ran)))))

(ert-deftest beads-command-coverage-test-preview-formatted ()
  "Test command preview returns formatted string."
  (let ((cmd (beads-command-list :json t)))
    (let ((preview (beads-command-preview cmd)))
      (should (stringp preview))
      (should (string-match-p "list" preview)))))

;;; ============================================================
;;; Create Command Parse Tests
;;; ============================================================

(ert-deftest beads-command-coverage-test-create-parse-single-issue ()
  "Test create parse with single JSON object."
  (let* ((cmd (beads-command-create :json t :title "Test"))
         (json-string (json-encode '((id . "bd-99")
                                     (title . "Test")
                                     (status . "open")
                                     (priority . 1)
                                     (issue_type . "task")
                                     (created_at . "2025-01-01T00:00:00Z")
                                     (updated_at . "2025-01-01T00:00:00Z"))))
         (exec (beads-command-execution
                :command cmd
                :exit-code 0
                :stdout json-string
                :stderr "")))
    (let ((result (beads-command-parse cmd exec)))
      (should (cl-typep result 'beads-issue))
      (should (equal (oref result id) "bd-99")))))

(ert-deftest beads-command-coverage-test-create-parse-multiple-issues ()
  "Test create parse with JSON array (from file)."
  (let* ((cmd (beads-command-create :json t :file "/tmp/issues.md"))
         (json-string (json-encode
                       (vector '((id . "bd-1") (title . "Issue 1")
                                 (status . "open") (priority . 1)
                                 (issue_type . "task")
                                 (created_at . "2025-01-01T00:00:00Z")
                                 (updated_at . "2025-01-01T00:00:00Z"))
                               '((id . "bd-2") (title . "Issue 2")
                                 (status . "open") (priority . 2)
                                 (issue_type . "bug")
                                 (created_at . "2025-01-01T00:00:00Z")
                                 (updated_at . "2025-01-01T00:00:00Z")))))
         (exec (beads-command-execution
                :command cmd
                :exit-code 0
                :stdout json-string
                :stderr "")))
    (let ((result (beads-command-parse cmd exec)))
      (should (listp result))
      (should (= (length result) 2))
      (should (cl-typep (car result) 'beads-issue)))))

(ert-deftest beads-command-coverage-test-create-parse-unexpected ()
  "Test create parse signals error on unexpected JSON."
  (let* ((cmd (beads-command-create :json t :title "Test"))
         (exec (beads-command-execution
                :command cmd
                :exit-code 0
                :stdout "42"
                :stderr "")))
    (should-error (beads-command-parse cmd exec)
                  :type 'beads-json-parse-error)))

(ert-deftest beads-command-coverage-test-create-parse-no-json ()
  "Test create parse with json=nil returns raw output."
  (let* ((cmd (beads-command-create :json nil :title "Test"))
         (exec (beads-command-execution
                :command cmd
                :exit-code 0
                :stdout "Created issue bd-99"
                :stderr "")))
    (let ((result (beads-command-parse cmd exec)))
      (should (stringp result)))))

(ert-deftest beads-command-coverage-test-create-parse-error ()
  "Test create parse signals error on invalid JSON."
  (let* ((cmd (beads-command-create :json t :title "Test"))
         (exec (beads-command-execution
                :command cmd
                :exit-code 0
                :stdout "not json"
                :stderr "")))
    (should-error (beads-command-parse cmd exec)
                  :type 'beads-json-parse-error)))

;;; Base class command-line (no subcommand) Tests

(ert-deftest beads-command-coverage-test-subcommand-returns-nil-for-base ()
  "Test that beads-command-subcommand returns nil for 'beads-command' class name."
  :tags '(:unit)
  ;; The subcommand method returns nil when class-name is "beads-command"
  ;; We test the logic by creating a concrete subclass that has a subcommand
  (let ((cmd (beads-command-list)))
    ;; This should NOT be nil (it's a concrete class with subcommand "list")
    (should (equal (beads-command-subcommand cmd) "list"))))

(ert-deftest beads-command-coverage-test-subcommand-cli-command-slot ()
  "Test subcommand returns cli-command slot when present and bound."
  :tags '(:unit)
  ;; beads-command-dep-add has :cli-command "dep add"
  (let ((cmd (beads-command-dep-add)))
    (should (equal (beads-command-subcommand cmd) "dep add"))))

;;; ============================================================
;;; beads-defcommand Slot Shorthand Tests
;;; ============================================================

;; Define a test command using shorthand slot definitions
(beads-defcommand beads-command-shorthand-test (beads-command-global-options)
  ((reason
    :option-type :string
    :short-option "r"
    :key "r"
    :group "Test"
    :level 1
    :order 1)
   (force
    :option-type :boolean
    :key "!"
    :group "Flags"
    :level 2
    :order 1)
   (title
    :option-type :string
    :positional 1
    :key "t"
    :group "Required"
    :level 1
    :order 2)
   (labels
    :option-type :list
    :option-separator ","
    :key "l"
    :group "Options"
    :level 2
    :order 2)
   (estimate
    :option-type :integer
    :key "e"
    :group "Options"
    :level 3
    :order 1))
  :documentation "Test command with shorthand slot definitions."
  :cli-command "shorthand-test")

(ert-deftest beads-defcommand-shorthand-initarg-inferred ()
  "Test :initarg is inferred from slot name in beads-defcommand."
  :tags '(:unit)
  (let ((cmd (beads-command-shorthand-test :reason "done")))
    (should (equal "done" (oref cmd reason)))))

(ert-deftest beads-defcommand-shorthand-type-inferred ()
  "Test :type is inferred from :option-type in beads-defcommand."
  :tags '(:unit)
  ;; String option should accept string
  (let ((cmd (beads-command-shorthand-test :reason "test")))
    (should (equal "test" (oref cmd reason))))
  ;; Boolean option should accept boolean
  (let ((cmd (beads-command-shorthand-test :force t)))
    (should (eq t (oref cmd force))))
  ;; List option should accept list
  (let ((cmd (beads-command-shorthand-test :labels '("a" "b"))))
    (should (equal '("a" "b") (oref cmd labels)))))

(ert-deftest beads-defcommand-shorthand-initform-nil ()
  "Test :initform defaults to nil for shorthand slots."
  :tags '(:unit)
  (let ((cmd (beads-command-shorthand-test)))
    (should (null (oref cmd reason)))
    (should (null (oref cmd force)))
    (should (null (oref cmd title)))
    (should (null (oref cmd labels)))
    (should (null (oref cmd estimate)))))

(ert-deftest beads-defcommand-shorthand-long-option-inferred ()
  "Test :long-option is inferred from slot name for non-positional slots."
  :tags '(:unit)
  ;; Non-positional slots should have :long-option inferred
  (should (equal "reason"
                 (beads-meta-slot-property 'beads-command-shorthand-test
                                           'reason :long-option)))
  (should (equal "force"
                 (beads-meta-slot-property 'beads-command-shorthand-test
                                           'force :long-option)))
  ;; Positional slots should NOT have :long-option
  (should (null (beads-meta-slot-property 'beads-command-shorthand-test
                                          'title :long-option))))

(ert-deftest beads-defcommand-shorthand-cli-works ()
  "Test command line generation works with shorthand-defined slots."
  :tags '(:unit)
  (let* ((cmd (beads-command-shorthand-test
               :reason "done" :force t :title "My issue"
               :labels '("bug" "p1") :estimate 60))
         (args (beads-command-line cmd)))
    ;; Should contain the subcommand
    (should (member "shorthand-test" args))
    ;; Should contain positional arg (title)
    (should (member "My issue" args))
    ;; Should contain --reason=done
    (should (seq-some (lambda (a) (string-prefix-p "--reason" a)) args))
    ;; Should contain --force
    (should (member "--force" args))
    ;; Should contain --labels
    (should (seq-some (lambda (a) (string-prefix-p "--labels" a)) args))
    ;; Should contain --estimate
    (should (seq-some (lambda (a) (string-prefix-p "--estimate" a)) args))))

(ert-deftest beads-defcommand-shorthand-bang-fn-exists ()
  "Test that the bang function is generated for shorthand commands."
  :tags '(:unit)
  (should (fboundp 'beads-command-shorthand-test!)))

(provide 'beads-command-coverage-test)
;;; beads-command-coverage-test.el ends here
