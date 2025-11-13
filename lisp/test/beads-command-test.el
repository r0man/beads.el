;;; beads-command-test.el --- Tests for beads-command.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; Tests for EIEIO command classes in beads-command.el.

;;; Code:

(require 'ert)
(require 'beads-command)

;; Define beads-executable for testing (it's defined in beads.el normally)
(defvar beads-executable "bd"
  "Path to the bd executable for testing.")

;;; Base Command Tests

(ert-deftest beads-command-base-class-is-abstract ()
  "Verify beads-command cannot be instantiated directly.
Since EIEIO doesn't provide eieio-class-abstract-p, we verify
the class has :abstract t in its definition."
  ;; The class definition has :abstract t, so we verify the slot exists
  ;; by checking we can't instantiate it (should error)
  (should-error (beads-command)
                :type 'error))

(ert-deftest beads-command-to-args-empty ()
  "Test beads-command-init-to-args with no flags set returns just init."
  (let* ((cmd (beads-command-init))
         (args (beads-command-to-args cmd)))
    (should (equal args '("init")))))

(ert-deftest beads-command-to-args-with-actor ()
  "Test beads-command-to-args with --actor flag."
  (let* ((cmd (beads-command-init :actor "testuser"))
         (args (beads-command-to-args cmd)))
    (should (member "--actor" args))
    (should (member "testuser" args))))

(ert-deftest beads-command-to-args-with-db ()
  "Test beads-command-to-args with --db flag."
  (let* ((cmd (beads-command-init :db "/path/to/db"))
         (args (beads-command-to-args cmd)))
    (should (member "--db" args))
    (should (member "/path/to/db" args))))

(ert-deftest beads-command-to-args-with-boolean-flags ()
  "Test beads-command-to-args with boolean flags."
  (let* ((cmd (beads-command-init
               :no-auto-flush t
               :no-daemon t
               :sandbox t))
         (args (beads-command-to-args cmd)))
    (should (member "--no-auto-flush" args))
    (should (member "--no-daemon" args))
    (should (member "--sandbox" args))))

(ert-deftest beads-command-to-args-all-global-flags ()
  "Test beads-command-to-args with all global flags set."
  (let* ((cmd (beads-command-init
               :actor "alice"
               :db "/tmp/test.db"
               :no-auto-flush t
               :no-auto-import t
               :no-daemon t
               :no-db t
               :sandbox t))
         (args (beads-command-to-args cmd)))
    (should (member "--actor" args))
    (should (member "alice" args))
    (should (member "--db" args))
    (should (member "/tmp/test.db" args))
    (should (member "--no-auto-flush" args))
    (should (member "--no-auto-import" args))
    (should (member "--no-daemon" args))
    (should (member "--no-db" args))
    (should (member "--sandbox" args))))

;;; JSON Command Tests

;; Define a concrete test command class that inherits from beads-command-json
(defclass beads-test-json-command (beads-command-json)
  ()
  :documentation "Test command class for testing beads-command-json.")

(cl-defmethod beads-command-to-args ((command beads-test-json-command))
  "Build test command arguments."
  (let ((args (list "test")))
    ;; Get parent args (global + json)
    (setq args (append args (cl-call-next-method)))
    args))

;; No need to override beads-command-execute - will use parent implementation

(ert-deftest beads-json-command-class-is-abstract ()
  "Verify beads-command-json cannot be instantiated directly."
  (should-error (beads-command-json)
                :type 'error))

(ert-deftest beads-json-command-inherits-from-beads-command ()
  "Verify beads-command-json inherits from beads-command."
  (let ((cmd (beads-test-json-command)))
    (should (object-of-class-p cmd 'beads-command-json))
    (should (object-of-class-p cmd 'beads-command))))

(ert-deftest beads-json-command-to-args-without-json ()
  "Test beads-command-json-to-args with json=nil."
  (let* ((cmd (beads-test-json-command :json nil))
         (args (beads-command-to-args cmd)))
    (should (equal (car args) "test"))
    (should-not (member "--json" args))))

(ert-deftest beads-json-command-to-args-with-json ()
  "Test beads-command-json-to-args with json=t."
  (let* ((cmd (beads-test-json-command :json t))
         (args (beads-command-to-args cmd)))
    (should (equal (car args) "test"))
    (should (member "--json" args))))

(ert-deftest beads-json-command-to-args-combined ()
  "Test beads-command-json-to-args with json and global flags."
  (let* ((cmd (beads-test-json-command
               :actor "alice"
               :json t
               :no-daemon t))
         (args (beads-command-to-args cmd)))
    (should (equal (car args) "test"))
    (should (member "--json" args))
    (should (member "--actor" args))
    (should (member "alice" args))
    (should (member "--no-daemon" args))))

(ert-deftest beads-json-command-execute-with-json ()
  "Test beads-command-json-execute with JSON enabled returns parsed JSON."
  (let ((cmd (beads-test-json-command :json t)))
    ;; Mock process-file to return JSON
    (cl-letf (((symbol-function 'process-file)
               (lambda (_program &optional _infile buffer _display &rest _args)
                 (when (listp buffer)
                   ;; Write to stdout buffer (first element)
                   (with-current-buffer (car buffer)
                     (insert "{\"id\":\"bd-42\",\"status\":\"open\"}"))
                   ;; Handle stderr (second element - could be buffer or file)
                   (when (nth 1 buffer)
                     (if (stringp (nth 1 buffer))
                         ;; It's a file path, write to file
                         (with-temp-buffer
                           (insert "")
                           (write-region (point-min) (point-max) (nth 1 buffer)))
                       ;; It's a buffer, write to buffer
                       (with-current-buffer (nth 1 buffer)
                         (insert "")))))
                 0)))
      (let ((result (beads-command-execute cmd)))
        ;; Should return (exit-code parsed-json stderr)
        (should (listp result))
        (should (= (length result) 3))
        (should (= (nth 0 result) 0))
        ;; Second element is parsed JSON
        (should (listp (nth 1 result)))
        (should (equal (alist-get 'id (nth 1 result)) "bd-42"))
        (should (equal (alist-get 'status (nth 1 result)) "open"))
        ;; Third element is stderr
        (should (stringp (nth 2 result)))))))

(ert-deftest beads-json-command-execute-without-json ()
  "Test beads-command-json-execute with JSON disabled returns (exit-code stdout stderr)."
  (let ((cmd (beads-test-json-command :json nil)))
    ;; Mock process-file to return plain text
    (cl-letf (((symbol-function 'process-file)
               (lambda (_program &optional _infile buffer _display &rest _args)
                 (when (listp buffer)
                   ;; Write to stdout buffer (first element)
                   (with-current-buffer (car buffer)
                     (insert "Success\n"))
                   ;; Handle stderr (second element - could be buffer or file)
                   (when (nth 1 buffer)
                     (if (stringp (nth 1 buffer))
                         ;; It's a file path, write to file
                         (with-temp-buffer
                           (insert "")
                           (write-region (point-min) (point-max) (nth 1 buffer)))
                       ;; It's a buffer, write to buffer
                       (with-current-buffer (nth 1 buffer)
                         (insert "")))))
                 0)))
      (let ((result (beads-command-execute cmd)))
        ;; Should return list of (exit-code stdout stderr)
        (should (listp result))
        (should (= (length result) 3))
        (should (= (nth 0 result) 0))
        (should (stringp (nth 1 result)))
        (should (string-match-p "Success" (nth 1 result)))
        (should (stringp (nth 2 result)))))))

(ert-deftest beads-json-command-execute-parse-error ()
  "Test beads-command-json-execute handles JSON parse errors."
  (let ((cmd (beads-test-json-command :json t)))
    ;; Mock process-file to return invalid JSON
    (cl-letf (((symbol-function 'process-file)
               (lambda (_program &optional _infile buffer _display &rest _args)
                 (when (listp buffer)
                   ;; Write to stdout buffer (first element)
                   (with-current-buffer (car buffer)
                     (insert "not valid json"))
                   ;; Handle stderr (second element - could be buffer or file)
                   (when (nth 1 buffer)
                     (if (stringp (nth 1 buffer))
                         ;; It's a file path, write to file
                         (with-temp-buffer
                           (insert "")
                           (write-region (point-min) (point-max) (nth 1 buffer)))
                       ;; It's a buffer, write to buffer
                       (with-current-buffer (nth 1 buffer)
                         (insert "")))))
                 0)))
      (should-error (beads-command-execute cmd)
                    :type 'beads-json-parse-error))))

(ert-deftest beads-json-command-execute-command-failure ()
  "Test beads-command-json-execute handles command failure."
  (let ((cmd (beads-test-json-command :json t)))
    ;; Mock process-file to return non-zero exit code
    (cl-letf (((symbol-function 'process-file)
               (lambda (_program &optional _infile buffer _display &rest _args)
                 (when (listp buffer)
                   ;; Write to stdout buffer (first element)
                   (with-current-buffer (car buffer)
                     (insert ""))
                   ;; Handle stderr (second element - could be buffer or file)
                   (when (nth 1 buffer)
                     (if (stringp (nth 1 buffer))
                         ;; It's a file path, write to file
                         (with-temp-buffer
                           (insert "Error: command failed")
                           (write-region (point-min) (point-max) (nth 1 buffer)))
                       ;; It's a buffer, write to buffer
                       (with-current-buffer (nth 1 buffer)
                         (insert "Error: command failed")))))
                 1)))
      (should-error (beads-command-execute cmd)
                    :type 'beads-command-error))))

;;; Init Command Tests

(ert-deftest beads-init-command-create-minimal ()
  "Test creating beads-command-init with no arguments."
  (let ((cmd (beads-command-init)))
    (should (beads-command-init-p cmd))
    (should (object-of-class-p cmd 'beads-command))))

(ert-deftest beads-init-command-create-with-slots ()
  "Test creating beads-command-init with all slots."
  (let ((cmd (beads-command-init
              :branch "develop"
              :contributor t
              :prefix "proj"
              :quiet t
              :skip-merge-driver t
              :team nil)))
    (should (string= (oref cmd branch) "develop"))
    (should (eq (oref cmd contributor) t))
    (should (string= (oref cmd prefix) "proj"))
    (should (eq (oref cmd quiet) t))
    (should (eq (oref cmd skip-merge-driver) t))
    (should (null (oref cmd team)))))

(ert-deftest beads-init-command-to-args-minimal ()
  "Test beads-command-init-to-args with no flags."
  (let* ((cmd (beads-command-init))
         (args (beads-command-to-args cmd)))
    (should (equal args '("init")))))

(ert-deftest beads-init-command-to-args-with-prefix ()
  "Test beads-command-init-to-args with --prefix."
  (let* ((cmd (beads-command-init :prefix "myproj"))
         (args (beads-command-to-args cmd)))
    (should (equal args '("init" "--prefix" "myproj")))))

(ert-deftest beads-init-command-to-args-with-branch ()
  "Test beads-command-init-to-args with --branch."
  (let* ((cmd (beads-command-init :branch "main"))
         (args (beads-command-to-args cmd)))
    (should (equal args '("init" "--branch" "main")))))

(ert-deftest beads-init-command-to-args-with-quiet ()
  "Test beads-command-init-to-args with --quiet."
  (let* ((cmd (beads-command-init :quiet t))
         (args (beads-command-to-args cmd)))
    (should (equal args '("init" "--quiet")))))

(ert-deftest beads-init-command-to-args-with-contributor ()
  "Test beads-command-init-to-args with --contributor."
  (let* ((cmd (beads-command-init :contributor t))
         (args (beads-command-to-args cmd)))
    (should (equal args '("init" "--contributor")))))

(ert-deftest beads-init-command-to-args-with-team ()
  "Test beads-command-init-to-args with --team."
  (let* ((cmd (beads-command-init :team t))
         (args (beads-command-to-args cmd)))
    (should (equal args '("init" "--team")))))

(ert-deftest beads-init-command-to-args-with-skip-merge-driver ()
  "Test beads-command-init-to-args with --skip-merge-driver."
  (let* ((cmd (beads-command-init :skip-merge-driver t))
         (args (beads-command-to-args cmd)))
    (should (equal args '("init" "--skip-merge-driver")))))

(ert-deftest beads-init-command-to-args-all-init-flags ()
  "Test beads-command-init-to-args with all init-specific flags."
  (let* ((cmd (beads-command-init
               :branch "develop"
               :prefix "test"
               :quiet t
               :skip-merge-driver t))
         (args (beads-command-to-args cmd)))
    (should (member "--branch" args))
    (should (member "develop" args))
    (should (member "--prefix" args))
    (should (member "test" args))
    (should (member "--quiet" args))
    (should (member "--skip-merge-driver" args))))

(ert-deftest beads-init-command-to-args-combined ()
  "Test beads-command-init-to-args with global and init flags."
  (let* ((cmd (beads-command-init
               :actor "bob"
               :prefix "proj"
               :quiet t))
         (args (beads-command-to-args cmd)))
    ;; Should have init command
    (should (equal (car args) "init"))
    ;; Should have global flags
    (should (member "--actor" args))
    (should (member "bob" args))
    ;; Should have init flags
    (should (member "--prefix" args))
    (should (member "proj" args))
    (should (member "--quiet" args))))

;;; Validation Tests

(ert-deftest beads-init-command-validate-success ()
  "Test beads-command-init-validate with valid command."
  (let ((cmd (beads-command-init :prefix "test")))
    (should (null (beads-command-validate cmd)))))

(ert-deftest beads-init-command-validate-contributor-only ()
  "Test beads-command-init-validate with --contributor only."
  (let ((cmd (beads-command-init :contributor t)))
    (should (null (beads-command-validate cmd)))))

(ert-deftest beads-init-command-validate-team-only ()
  "Test beads-command-init-validate with --team only."
  (let ((cmd (beads-command-init :team t)))
    (should (null (beads-command-validate cmd)))))

(ert-deftest beads-init-command-validate-conflict ()
  "Test beads-command-init-validate with --contributor and --team."
  (let ((cmd (beads-command-init :contributor t :team t)))
    (should (stringp (beads-command-validate cmd)))
    (should (string-match-p "both" (beads-command-validate cmd)))))

;;; Execution Tests (mocked)

(ert-deftest beads-init-command-execute-validates ()
  "Test beads-command-execute calls validation."
  (let ((cmd (beads-command-init :contributor t :team t)))
    (should-error (beads-command-execute cmd)
                  :type 'beads-validation-error)))

(ert-deftest beads-init-command-execute-builds-args ()
  "Test beads-command-execute builds correct arguments and returns output."
  (let ((cmd (beads-command-init :prefix "test" :quiet t))
        (called-args nil))
    ;; Mock process-file to capture arguments
    (cl-letf (((symbol-function 'process-file)
               (lambda (program &optional _infile buffer _display &rest args)
                 (setq called-args (cons program args))
                 ;; Write mock output to buffers
                 (when (listp buffer)
                   ;; Write to stdout buffer (first element)
                   (with-current-buffer (car buffer)
                     (insert "Initialized beads in .beads/\n"))
                   ;; Handle stderr (second element - could be buffer or file)
                   (when (nth 1 buffer)
                     (if (stringp (nth 1 buffer))
                         ;; It's a file path, write to file
                         (with-temp-buffer
                           (insert "")
                           (write-region (point-min) (point-max) (nth 1 buffer)))
                       ;; It's a buffer, write to buffer
                       (with-current-buffer (nth 1 buffer)
                         (insert "")))))
                 0))) ; return success
      (let ((result (beads-command-execute cmd)))
        ;; Should return list of (exit-code stdout stderr)
        (should (listp result))
        (should (= (length result) 3))
        (should (numberp (nth 0 result)))
        (should (= (nth 0 result) 0))
        (should (stringp (nth 1 result)))
        (should (stringp (nth 2 result)))
        ;; Verify correct command was built
        (should (equal (car called-args) "bd"))
        (should (member "--prefix" (cdr called-args)))
        (should (member "test" (cdr called-args)))
        (should (member "--quiet" (cdr called-args)))))))

;;; Utility Function Tests

(ert-deftest beads-init-command-from-options ()
  "Test beads-command-init-from-options creates command from plist."
  (let ((cmd (beads-command-init-from-options
              '(:prefix "test"
                        :branch "main"
                        :quiet t
                        :actor "alice"))))
    (should (beads-command-init-p cmd))
    (should (string= (oref cmd prefix) "test"))
    (should (string= (oref cmd branch) "main"))
    (should (eq (oref cmd quiet) t))
    (should (string= (oref cmd actor) "alice"))))

(ert-deftest beads-command-init-execute ()
  "Test beads-command-init execution in a temporary directory.
This test requires bd to be installed and available."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (let* ((temp-dir (make-temp-file "beads-test-" t))
         (default-directory temp-dir))
    (unwind-protect
        (let ((result (beads-command-execute (beads-command-init))))
          ;; Should return (exit-code stdout stderr)
          (should (listp result))
          (should (= (length result) 3))
          ;; Exit code should be 0
          (should (= (nth 0 result) 0))
          ;; Stdout should be a string
          (should (stringp (nth 1 result)))
          ;; Stderr should be a string
          (should (stringp (nth 2 result)))
          ;; Should create .beads directory
          (should (file-directory-p (expand-file-name ".beads" temp-dir))))
      ;; Cleanup: remove temp directory
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

;;; List Command Tests

(ert-deftest beads-list-command-create-minimal ()
  "Test creating beads-command-list with no arguments."
  (let ((cmd (beads-command-list)))
    (should (beads-command-list-p cmd))
    (should (object-of-class-p cmd 'beads-command-json))
    (should (object-of-class-p cmd 'beads-command))))

(ert-deftest beads-list-command-to-args-minimal ()
  "Test beads-command-list-to-args with no flags."
  (let* ((cmd (beads-command-list))
         (args (beads-command-to-args cmd)))
    (should (equal args '("list")))))

(ert-deftest beads-list-command-to-args-with-json ()
  "Test beads-command-list-to-args with --json."
  (let* ((cmd (beads-command-list :json t))
         (args (beads-command-to-args cmd)))
    (should (member "list" args))
    (should (member "--json" args))))

(ert-deftest beads-list-command-to-args-with-status ()
  "Test beads-command-list-to-args with --status."
  (let* ((cmd (beads-command-list :status "open"))
         (args (beads-command-to-args cmd)))
    (should (member "--status" args))
    (should (member "open" args))))

(ert-deftest beads-list-command-to-args-with-priority ()
  "Test beads-command-list-to-args with --priority."
  (let* ((cmd (beads-command-list :priority 1))
         (args (beads-command-to-args cmd)))
    (should (member "--priority" args))
    (should (member "1" args))))

(ert-deftest beads-list-command-to-args-with-assignee ()
  "Test beads-command-list-to-args with --assignee."
  (let* ((cmd (beads-command-list :assignee "alice"))
         (args (beads-command-to-args cmd)))
    (should (member "--assignee" args))
    (should (member "alice" args))))

(ert-deftest beads-list-command-to-args-with-labels ()
  "Test beads-command-list-to-args with multiple --label flags."
  (let* ((cmd (beads-command-list :label '("bug" "urgent")))
         (args (beads-command-to-args cmd)))
    (should (member "--label" args))
    (should (member "bug" args))
    (should (member "urgent" args))))

(ert-deftest beads-list-command-to-args-with-label-any ()
  "Test beads-command-list-to-args with multiple --label-any flags."
  (let* ((cmd (beads-command-list :label-any '("feature" "enhancement")))
         (args (beads-command-to-args cmd)))
    (should (member "--label-any" args))
    (should (member "feature" args))
    (should (member "enhancement" args))))

(ert-deftest beads-list-command-to-args-with-limit ()
  "Test beads-command-list-to-args with --limit."
  (let* ((cmd (beads-command-list :limit 10))
         (args (beads-command-to-args cmd)))
    (should (member "--limit" args))
    (should (member "10" args))))

(ert-deftest beads-list-command-to-args-with-dates ()
  "Test beads-command-list-to-args with date filters."
  (let* ((cmd (beads-command-list
               :created-after "2025-01-01"
               :updated-before "2025-12-31"))
         (args (beads-command-to-args cmd)))
    (should (member "--created-after" args))
    (should (member "2025-01-01" args))
    (should (member "--updated-before" args))
    (should (member "2025-12-31" args))))

(ert-deftest beads-list-command-to-args-with-boolean-filters ()
  "Test beads-command-list-to-args with boolean filters."
  (let* ((cmd (beads-command-list
               :no-assignee t
               :no-labels t
               :empty-description t))
         (args (beads-command-to-args cmd)))
    (should (member "--no-assignee" args))
    (should (member "--no-labels" args))
    (should (member "--empty-description" args))))

(ert-deftest beads-list-command-to-args-combined ()
  "Test beads-command-list-to-args with multiple filters."
  (let* ((cmd (beads-command-list
               :json t
               :status "open"
               :priority 1
               :assignee "bob"
               :limit 5))
         (args (beads-command-to-args cmd)))
    (should (equal (car args) "list"))
    (should (member "--json" args))
    (should (member "--status" args))
    (should (member "open" args))
    (should (member "--priority" args))
    (should (member "1" args))
    (should (member "--assignee" args))
    (should (member "bob" args))
    (should (member "--limit" args))
    (should (member "5" args))))

(ert-deftest beads-list-command-validate-success ()
  "Test beads-command-list-validate with valid command."
  (let ((cmd (beads-command-list :status "open")))
    (should (null (beads-command-validate cmd)))))

(ert-deftest beads-list-command-validate-priority-conflict ()
  "Test beads-command-list-validate with priority conflict."
  (let ((cmd (beads-command-list :priority 1 :priority-min 0)))
    (should (stringp (beads-command-validate cmd)))
    (should (string-match-p "priority" (beads-command-validate cmd)))))

(ert-deftest beads-list-command-validate-assignee-conflict ()
  "Test beads-command-list-validate with assignee conflict."
  (let ((cmd (beads-command-list :assignee "alice" :no-assignee t)))
    (should (stringp (beads-command-validate cmd)))
    (should (string-match-p "assignee" (beads-command-validate cmd)))))

(ert-deftest beads-list-command-validate-labels-conflict ()
  "Test beads-command-list-validate with labels conflict."
  (let ((cmd (beads-command-list :label '("bug") :no-labels t)))
    (should (stringp (beads-command-validate cmd)))
    (should (string-match-p "label" (beads-command-validate cmd)))))

(ert-deftest beads-list-command-validate-priority-range ()
  "Test beads-command-list-validate with invalid priority range."
  (let ((cmd1 (beads-command-list :priority 5))
        (cmd2 (beads-command-list :priority-min -1))
        (cmd3 (beads-command-list :priority-max 10)))
    (should (stringp (beads-command-validate cmd1)))
    (should (stringp (beads-command-validate cmd2)))
    (should (stringp (beads-command-validate cmd3)))))

(ert-deftest beads-list-command-execute-returns-issue-list ()
  "Test beads-command-list-execute returns list of beads-issue instances."
  (require 'beads-types)
  (let ((cmd (beads-command-list :json t)))
    ;; Mock process-file to return JSON array of issues
    (cl-letf (((symbol-function 'process-file)
               (lambda (_program &optional _infile buffer _display &rest _args)
                 (when (listp buffer)
                   ;; Write to stdout buffer (first element)
                   (with-current-buffer (car buffer)
                     (insert "[{\"id\":\"bd-1\",\"title\":\"Test\",\"status\":\"open\",\"priority\":2}]"))
                   ;; Handle stderr
                   (when (nth 1 buffer)
                     (if (stringp (nth 1 buffer))
                         (with-temp-buffer
                           (insert "")
                           (write-region (point-min) (point-max) (nth 1 buffer)))
                       (with-current-buffer (nth 1 buffer)
                         (insert "")))))
                 0)))
      (let ((result (beads-command-execute cmd)))
        ;; Should return list of beads-issue instances
        (should (listp result))
        (should (> (length result) 0))
        (should (beads-issue-p (car result)))
        (should (string= (oref (car result) id) "bd-1"))
        (should (string= (oref (car result) title) "Test"))
        (should (string= (oref (car result) status) "open"))))))

(ert-deftest beads-list-command-execute-empty-result ()
  "Test beads-command-list-execute with empty result."
  (require 'beads-types)
  (let ((cmd (beads-command-list :json t)))
    ;; Mock process-file to return empty JSON array
    (cl-letf (((symbol-function 'process-file)
               (lambda (_program &optional _infile buffer _display &rest _args)
                 (when (listp buffer)
                   (with-current-buffer (car buffer)
                     (insert "[]"))
                   (when (nth 1 buffer)
                     (if (stringp (nth 1 buffer))
                         (with-temp-buffer
                           (insert "")
                           (write-region (point-min) (point-max) (nth 1 buffer)))
                       (with-current-buffer (nth 1 buffer)
                         (insert "")))))
                 0)))
      (let ((result (beads-command-execute cmd)))
        ;; Should return empty list
        (should (listp result))
        (should (= (length result) 0))))))

;;; Create Command Tests

(ert-deftest beads-create-command-create-minimal ()
  "Test creating beads-command-create with minimal arguments."
  (let ((cmd (beads-command-create :title "Test issue")))
    (should (beads-command-create-p cmd))
    (should (object-of-class-p cmd 'beads-command-json))
    (should (object-of-class-p cmd 'beads-command))
    (should (string= (oref cmd title) "Test issue"))))

(ert-deftest beads-create-command-to-args-minimal ()
  "Test beads-command-create-to-args with just title."
  (let* ((cmd (beads-command-create :title "Test issue"))
         (args (beads-command-to-args cmd)))
    (should (equal (car args) "create"))
    (should (member "Test issue" args))))

(ert-deftest beads-create-command-to-args-with-json ()
  "Test beads-command-create-to-args with --json."
  (let* ((cmd (beads-command-create :title "Test" :json t))
         (args (beads-command-to-args cmd)))
    (should (member "create" args))
    (should (member "Test" args))
    (should (member "--json" args))))

(ert-deftest beads-create-command-to-args-with-type ()
  "Test beads-command-create-to-args with --type."
  (let* ((cmd (beads-command-create
               :title "Bug fix"
               :issue-type "bug"))
         (args (beads-command-to-args cmd)))
    (should (member "--type" args))
    (should (member "bug" args))))

(ert-deftest beads-create-command-to-args-with-priority ()
  "Test beads-command-create-to-args with --priority."
  (let* ((cmd (beads-command-create
               :title "Critical issue"
               :priority "0"))
         (args (beads-command-to-args cmd)))
    (should (member "--priority" args))
    (should (member "0" args))))

(ert-deftest beads-create-command-to-args-with-description ()
  "Test beads-command-create-to-args with --description."
  (let* ((cmd (beads-command-create
               :title "Feature"
               :description "Add new feature"))
         (args (beads-command-to-args cmd)))
    (should (member "--description" args))
    (should (member "Add new feature" args))))

(ert-deftest beads-create-command-to-args-with-assignee ()
  "Test beads-command-create-to-args with --assignee."
  (let* ((cmd (beads-command-create
               :title "Task"
               :assignee "alice"))
         (args (beads-command-to-args cmd)))
    (should (member "--assignee" args))
    (should (member "alice" args))))

(ert-deftest beads-create-command-to-args-with-labels ()
  "Test beads-command-create-to-args with --labels."
  (let* ((cmd (beads-command-create
               :title "Task"
               :labels '("bug" "urgent")))
         (args (beads-command-to-args cmd)))
    (should (member "--labels" args))
    (should (member "bug,urgent" args))))

(ert-deftest beads-create-command-to-args-with-deps ()
  "Test beads-command-create-to-args with --deps."
  (let* ((cmd (beads-command-create
               :title "Task"
               :deps '("discovered-from:bd-20" "blocks:bd-15")))
         (args (beads-command-to-args cmd)))
    (should (member "--deps" args))
    (should (member "discovered-from:bd-20,blocks:bd-15" args))))

(ert-deftest beads-create-command-to-args-with-parent ()
  "Test beads-command-create-to-args with --parent."
  (let* ((cmd (beads-command-create
               :title "Subtask"
               :parent "bd-42"))
         (args (beads-command-to-args cmd)))
    (should (member "--parent" args))
    (should (member "bd-42" args))))

(ert-deftest beads-create-command-to-args-with-external-ref ()
  "Test beads-command-create-to-args with --external-ref."
  (let* ((cmd (beads-command-create
               :title "Task"
               :external-ref "gh-123"))
         (args (beads-command-to-args cmd)))
    (should (member "--external-ref" args))
    (should (member "gh-123" args))))

(ert-deftest beads-create-command-to-args-with-file ()
  "Test beads-command-create-to-args with --file."
  (let* ((cmd (beads-command-create
               :file "/path/to/issues.md"))
         (args (beads-command-to-args cmd)))
    (should (member "--file" args))
    (should (member "/path/to/issues.md" args))))

(ert-deftest beads-create-command-to-args-combined ()
  "Test beads-command-create-to-args with multiple options."
  (let* ((cmd (beads-command-create
               :json t
               :title "New feature"
               :issue-type "feature"
               :priority "1"
               :description "Implement feature"
               :assignee "bob"
               :labels '("enhancement")
               :deps '("blocks:bd-10")))
         (args (beads-command-to-args cmd)))
    (should (equal (car args) "create"))
    (should (member "--json" args))
    (should (member "New feature" args))
    (should (member "--type" args))
    (should (member "feature" args))
    (should (member "--priority" args))
    (should (member "1" args))
    (should (member "--description" args))
    (should (member "Implement feature" args))
    (should (member "--assignee" args))
    (should (member "bob" args))
    (should (member "--labels" args))
    (should (member "enhancement" args))
    (should (member "--deps" args))
    (should (member "blocks:bd-10" args))))

(ert-deftest beads-create-command-validate-success-with-title ()
  "Test beads-command-create-validate with title."
  (let ((cmd (beads-command-create :title "Test")))
    (should (null (beads-command-validate cmd)))))

(ert-deftest beads-create-command-validate-success-with-file ()
  "Test beads-command-create-validate with file."
  (let ((cmd (beads-command-create :file "/path/to/file.md")))
    (should (null (beads-command-validate cmd)))))

(ert-deftest beads-create-command-validate-no-title-no-file ()
  "Test beads-command-create-validate without title or file."
  (let ((cmd (beads-command-create)))
    (should (stringp (beads-command-validate cmd)))
    (should (string-match-p "title\\|file" (beads-command-validate cmd)))))

(ert-deftest beads-create-command-validate-both-title-and-file ()
  "Test beads-command-create-validate with both title and file."
  (let ((cmd (beads-command-create
              :title "Test"
              :file "/path/to/file.md")))
    (should (stringp (beads-command-validate cmd)))
    (should (string-match-p "both" (beads-command-validate cmd)))))

(ert-deftest beads-create-command-execute-returns-issue ()
  "Test beads-command-create-execute returns beads-issue instance."
  (require 'beads-types)
  (let ((cmd (beads-command-create :title "Test" :json t)))
    ;; Mock process-file to return JSON object
    (cl-letf (((symbol-function 'process-file)
               (lambda (_program &optional _infile buffer _display &rest _args)
                 (when (listp buffer)
                   ;; Write to stdout buffer
                   (with-current-buffer (car buffer)
                     (insert "{\"id\":\"bd-42\",\"title\":\"Test\",\"status\":\"open\",\"priority\":2}"))
                   ;; Handle stderr
                   (when (nth 1 buffer)
                     (if (stringp (nth 1 buffer))
                         (with-temp-buffer
                           (insert "")
                           (write-region (point-min) (point-max) (nth 1 buffer)))
                       (with-current-buffer (nth 1 buffer)
                         (insert "")))))
                 0)))
      (let ((result (beads-command-execute cmd)))
        ;; Should return beads-issue instance
        (should (beads-issue-p result))
        (should (string= (oref result id) "bd-42"))
        (should (string= (oref result title) "Test"))
        (should (string= (oref result status) "open"))))))

(ert-deftest beads-create-command-execute-returns-issue-list-from-file ()
  "Test beads-command-create-execute returns list from file."
  (require 'beads-types)
  (let ((cmd (beads-command-create :file "/tmp/issues.md" :json t)))
    ;; Mock process-file to return JSON array
    (cl-letf (((symbol-function 'process-file)
               (lambda (_program &optional _infile buffer _display &rest _args)
                 (when (listp buffer)
                   (with-current-buffer (car buffer)
                     (insert "[{\"id\":\"bd-1\",\"title\":\"A\",\"status\":\"open\",\"priority\":2},{\"id\":\"bd-2\",\"title\":\"B\",\"status\":\"open\",\"priority\":2}]"))
                   (when (nth 1 buffer)
                     (if (stringp (nth 1 buffer))
                         (with-temp-buffer
                           (insert "")
                           (write-region (point-min) (point-max) (nth 1 buffer)))
                       (with-current-buffer (nth 1 buffer)
                         (insert "")))))
                 0)))
      (let ((result (beads-command-execute cmd)))
        ;; Should return list of beads-issue instances
        (should (listp result))
        (should (= (length result) 2))
        (should (beads-issue-p (car result)))
        (should (string= (oref (car result) id) "bd-1"))
        (should (string= (oref (cadr result) id) "bd-2"))))))

(provide 'beads-command-test)
;;; beads-command-test.el ends here
