;;; beads-command-config-test.el --- Tests for beads-command-config -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for beads-command-config command classes.

;;; Code:

(require 'ert)
(require 'beads-command-config)

;;; Unit Tests: beads-command-config-get command-line

(ert-deftest beads-command-config-get-test-command-line-basic ()
  "Unit test: config get builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-config-get :key "sync.branch"))
         (args (beads-command-line cmd)))
    (should (member "config" args))
    (should (member "get" args))
    (should (member "sync.branch" args))))

(ert-deftest beads-command-config-get-test-validation-missing-key ()
  "Unit test: config get validation fails without key."
  :tags '(:unit)
  (let ((cmd (beads-command-config-get)))
    (should (beads-command-validate cmd))))

(ert-deftest beads-command-config-get-test-validation-success ()
  "Unit test: config get validation succeeds with key."
  :tags '(:unit)
  (let ((cmd (beads-command-config-get :key "test.key")))
    (should (null (beads-command-validate cmd)))))

;;; Unit Tests: beads-command-config-set command-line

(ert-deftest beads-command-config-set-test-command-line-basic ()
  "Unit test: config set builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-config-set :key "sync.branch" :value "main"))
         (args (beads-command-line cmd)))
    (should (member "config" args))
    (should (member "set" args))
    (should (member "sync.branch" args))
    (should (member "main" args))))

(ert-deftest beads-command-config-set-test-validation-missing-key ()
  "Unit test: config set validation fails without key."
  :tags '(:unit)
  (let ((cmd (beads-command-config-set :value "test")))
    (should (beads-command-validate cmd))))

(ert-deftest beads-command-config-set-test-validation-missing-value ()
  "Unit test: config set validation fails without value."
  :tags '(:unit)
  (let ((cmd (beads-command-config-set :key "test.key")))
    (should (beads-command-validate cmd))))

(ert-deftest beads-command-config-set-test-validation-success ()
  "Unit test: config set validation succeeds with key and value."
  :tags '(:unit)
  (let ((cmd (beads-command-config-set :key "test.key" :value "test")))
    (should (null (beads-command-validate cmd)))))

;;; Unit Tests: beads-command-config-list command-line

(ert-deftest beads-command-config-list-test-command-line-basic ()
  "Unit test: config list builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-config-list))
         (args (beads-command-line cmd)))
    (should (member "config" args))
    (should (member "list" args))))

;;; Unit Tests: beads-command-config-unset command-line

(ert-deftest beads-command-config-unset-test-command-line-basic ()
  "Unit test: config unset builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-config-unset :key "test.key"))
         (args (beads-command-line cmd)))
    (should (member "config" args))
    (should (member "unset" args))
    (should (member "test.key" args))))

(ert-deftest beads-command-config-unset-test-validation-missing-key ()
  "Unit test: config unset validation fails without key."
  :tags '(:unit)
  (let ((cmd (beads-command-config-unset)))
    (should (beads-command-validate cmd))))

(ert-deftest beads-command-config-unset-test-validation-success ()
  "Unit test: config unset validation succeeds with key."
  :tags '(:unit)
  (let ((cmd (beads-command-config-unset :key "test.key")))
    (should (null (beads-command-validate cmd)))))

;;; Unit Tests: beads-command-config-validate command-line

(ert-deftest beads-command-config-validate-test-command-class-exists ()
  "Unit test: config validate command class is defined."
  :tags '(:unit)
  (should (fboundp 'beads-command-config-validate)))

(ert-deftest beads-command-config-validate-test-command-line-basic ()
  "Unit test: config validate builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-config-validate))
         (args (beads-command-line cmd)))
    (should (member "config" args))
    (should (member "validate" args))))

(ert-deftest beads-command-config-validate-test-validation-always-valid ()
  "Unit test: config validate has no required fields."
  :tags '(:unit)
  (let ((cmd (beads-command-config-validate)))
    (should (null (beads-command-validate cmd)))))

(ert-deftest beads-command-config-validate-test-transient-defined ()
  "Unit test: beads-config-validate transient is defined."
  :tags '(:unit)
  (should (fboundp 'beads-config-validate)))

(ert-deftest beads-command-config-validate-test-transient-is-prefix ()
  "Unit test: beads-config-validate is a transient prefix."
  :tags '(:unit)
  (should (get 'beads-config-validate 'transient--prefix)))

(ert-deftest beads-command-config-validate-test-config-menu-has-validate ()
  "Unit test: beads-config menu includes validate command."
  :tags '(:unit)
  (let* ((layout (get 'beads-config 'transient--layout))
         (layout-str (format "%s" layout)))
    (should (string-match-p "beads-config-validate" layout-str))))

;;; Unit Tests: beads-command-config-set-many command-line

(ert-deftest beads-command-config-set-many-test-class-exists ()
  "Unit test: beads-command-config-set-many class is defined."
  :tags '(:unit)
  (should (cl-find-class 'beads-command-config-set-many)))

(ert-deftest beads-command-config-set-many-test-subcommand ()
  "Unit test: config set-many subcommand is 'config set-many'."
  :tags '(:unit)
  (let ((cmd (beads-command-config-set-many)))
    (should (equal (beads-command-subcommand cmd) "config set-many"))))

(ert-deftest beads-command-config-set-many-test-command-line-basic ()
  "Unit test: config set-many builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-config-set-many
               :pairs '("key1=val1" "key2=val2")))
         (args (beads-command-line cmd)))
    (should (member "config" args))
    (should (member "set-many" args))
    (should (member "key1=val1" args))
    (should (member "key2=val2" args))))

(ert-deftest beads-command-config-set-many-test-validation-missing-pairs ()
  "Unit test: config set-many validation fails without pairs."
  :tags '(:unit)
  (let ((cmd (beads-command-config-set-many)))
    (should (beads-command-validate cmd))))

(ert-deftest beads-command-config-set-many-test-validation-success ()
  "Unit test: config set-many validation succeeds with pairs."
  :tags '(:unit)
  (let ((cmd (beads-command-config-set-many :pairs '("k=v"))))
    (should (null (beads-command-validate cmd)))))

;;; Unit Tests: beads-command-config-show command-line

(ert-deftest beads-command-config-show-test-command-line-basic ()
  "Unit test: config show builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-config-show))
         (args (beads-command-line cmd)))
    (should (member "config" args))
    (should (member "show" args))))

(ert-deftest beads-command-config-show-test-command-line-source ()
  "Unit test: config show --source filter."
  :tags '(:unit)
  (let* ((cmd (beads-command-config-show :source "config.yaml"))
         (args (beads-command-line cmd)))
    (should (member "--source" args))
    (should (member "config.yaml" args))))

;;; Unit Tests: beads-command-config-apply command-line

(ert-deftest beads-command-config-apply-test-command-line-basic ()
  "Unit test: config apply builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-config-apply))
         (args (beads-command-line cmd)))
    (should (member "config" args))
    (should (member "apply" args))))

(ert-deftest beads-command-config-apply-test-command-line-dry-run ()
  "Unit test: config apply --dry-run flag."
  :tags '(:unit)
  (let* ((cmd (beads-command-config-apply :dry-run t))
         (args (beads-command-line cmd)))
    (should (member "--dry-run" args))))

;;; Unit Tests: beads-command-config-drift command-line

(ert-deftest beads-command-config-drift-test-command-line-basic ()
  "Unit test: config drift builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-config-drift))
         (args (beads-command-line cmd)))
    (should (member "config" args))
    (should (member "drift" args))))

;;; Unit Tests: transient definitions for new config subcommands

(ert-deftest beads-command-config-show-test-transient-defined ()
  "Unit test: beads-config-show transient is defined."
  :tags '(:unit)
  (should (fboundp 'beads-config-show)))

(ert-deftest beads-command-config-show-test-transient-is-prefix ()
  "Unit test: beads-config-show is a transient prefix."
  :tags '(:unit)
  (should (get 'beads-config-show 'transient--prefix)))

(ert-deftest beads-command-config-apply-test-transient-defined ()
  "Unit test: beads-config-apply transient is defined."
  :tags '(:unit)
  (should (fboundp 'beads-config-apply)))

(ert-deftest beads-command-config-apply-test-transient-is-prefix ()
  "Unit test: beads-config-apply is a transient prefix."
  :tags '(:unit)
  (should (get 'beads-config-apply 'transient--prefix)))

(ert-deftest beads-command-config-drift-test-transient-defined ()
  "Unit test: beads-config-drift transient is defined."
  :tags '(:unit)
  (should (fboundp 'beads-config-drift)))

(ert-deftest beads-command-config-drift-test-transient-is-prefix ()
  "Unit test: beads-config-drift is a transient prefix."
  :tags '(:unit)
  (should (get 'beads-config-drift 'transient--prefix)))

(provide 'beads-command-config-test)
;;; beads-command-config-test.el ends here
