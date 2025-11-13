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

(provide 'beads-command-test)
;;; beads-command-test.el ends here
