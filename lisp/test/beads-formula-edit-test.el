;;; beads-formula-edit-test.el --- Tests for beads-formula-edit -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; Tests for the formula authoring surfaces in `beads-formula-edit.el'
;; (WI-SF-09, REQ-SF-060 … REQ-SF-062): scaffolding and search-path
;; resolution, opening an existing source, schema completion and
;; validation (required fields + type mismatches, inline and results
;; buffer), and the JSON->TOML convert flow.  `beads-command-execute'
;; is mocked in the unit tests, so no `bd' is required.

;;; Code:

(require 'ert)
(require 'beads-formula-edit)
(require 'beads-command-formula)
(require 'beads-types)
(require 'beads-test)
(require 'beads-integration-test)

;;; ========================================
;;; Fixtures
;;; ========================================

(defun beads-formula-edit-test--schema ()
  "Return a minimal schema list for validation/completion tests."
  (list
   (beads-formula-schema-struct
    :name "Formula"
    :fields
    (list
     (beads-formula-schema-field :name "Name" :json-name "formula"
                                 :type "string" :required t)
     (beads-formula-schema-field :name "Version" :json-name "version"
                                 :type "int" :required t)
     (beads-formula-schema-field :name "Type" :json-name "type"
                                 :type "FormulaType" :required t)
     (beads-formula-schema-field :name "Description" :json-name "description"
                                 :type "string")
     (beads-formula-schema-field :name "Steps" :json-name "steps"
                                 :type "[]*Step")))
   (beads-formula-schema-struct
    :name "VarDef"
    :fields
    (list
     (beads-formula-schema-field :name "Description" :json-name "description"
                                 :type "string")
     (beads-formula-schema-field :name "Required" :json-name "required"
                                 :type "bool")))))

;;; ========================================
;;; Scaffold and paths (REQ-SF-060)
;;; ========================================

(ert-deftest beads-formula-edit-test-scaffold ()
  "The scaffold carries the formula name, type and a starter step."
  :tags '(:unit)
  (let ((text (beads-formula-edit-scaffold "my-flow" "expansion")))
    (should (string-match-p "^formula = \"my-flow\"$" text))
    (should (string-match-p "^type = \"expansion\"$" text))
    (should (string-match-p "^version = 1$" text))
    (should (string-match-p "^\\[\\[steps\\]\\]$" text))
    (should (string-match-p "my-flow-step" text))))

(ert-deftest beads-formula-edit-test-path-project-and-user ()
  "Project paths resolve under the store root; user paths under HOME."
  :tags '(:unit)
  (let ((beads-store-directory "/tmp/store/"))
    (should (equal (beads-formula-edit--path "x" "project")
                   (expand-file-name ".beads/formulas/x.formula.toml"
                                     "/tmp/store/"))))
  (should (equal (beads-formula-edit--path "x" "user")
                 (expand-file-name "~/.beads/formulas/x.formula.toml"))))

(ert-deftest beads-formula-edit-test-new-creates-and-associates ()
  "`beads-formula-new' writes the scaffold and associates the buffer."
  :tags '(:unit)
  (let* ((dir (make-temp-file "beads-formula-edit-" t))
         (beads-store-directory dir)
         (beads-formula-edit--schema-cache nil)
         (opened nil))
    (unwind-protect
        (cl-letf (((symbol-function 'pop-to-buffer)
                   (lambda (buffer &rest _) (setq opened buffer) buffer)))
          (beads-formula-new "demo" "workflow" "project")
          (let ((file (expand-file-name ".beads/formulas/demo.formula.toml" dir)))
            (should (file-exists-p file))
            (should (string-match-p "formula = \"demo\""
                                    (with-temp-buffer
                                      (insert-file-contents file)
                                      (buffer-string))))
            (with-current-buffer opened
              (should (equal beads-formula-edit--formula-name "demo"))
              (should beads-formula-edit-minor-mode))))
      (ignore-errors (delete-directory dir t)))))

(ert-deftest beads-formula-edit-test-new-refuses-existing ()
  "Creating an existing formula signals a `user-error'."
  :tags '(:unit)
  (let* ((dir (make-temp-file "beads-formula-edit-" t))
         (beads-store-directory dir))
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".beads/formulas" dir) t)
          (write-region "" nil
                        (expand-file-name ".beads/formulas/demo.formula.toml" dir))
          (should-error (beads-formula-new "demo" "workflow" "project")
                        :type 'user-error))
      (ignore-errors (delete-directory dir t)))))

(ert-deftest beads-formula-edit-test-new-rejects-path-separator ()
  "A name with a path separator is rejected."
  :tags '(:unit)
  (should-error (beads-formula-new "a/b" "workflow" "project")
                :type 'user-error))

;;; ========================================
;;; Open source (REQ-SF-060)
;;; ========================================

(ert-deftest beads-formula-edit-test-source-resolves-and-opens ()
  "`beads-formula-edit-source' opens the resolved source path."
  :tags '(:unit)
  (let ((opened nil))
    (cl-letf (((symbol-function 'beads-command-execute)
               (lambda (&rest _) (beads-formula :source "/tmp/x.formula.toml")))
              ((symbol-function 'beads-formula-edit--visit)
               (lambda (path name) (setq opened (cons path name)))))
      (beads-formula-edit-source "x")
      (should (equal opened '("/tmp/x.formula.toml" . "x"))))))

(ert-deftest beads-formula-edit-test-source-missing ()
  "A formula without a source signals a `user-error'."
  :tags '(:unit)
  (cl-letf (((symbol-function 'beads-command-execute)
             (lambda (&rest _) (beads-formula :source nil))))
    (should-error (beads-formula-edit-source "x") :type 'user-error)))

;;; ========================================
;;; Schema completion (REQ-SF-061)
;;; ========================================

(ert-deftest beads-formula-edit-test-completions-top-level ()
  "Top-level completions are the Formula struct field names."
  :tags '(:unit)
  (let ((cands (beads-formula--schema-completions
                (beads-formula-edit-test--schema) "" "")))
    (should (member "formula" cands))
    (should (member "version" cands))
    (should-not (member "required" cands))))

(ert-deftest beads-formula-edit-test-completions-section ()
  "A `[vars.x]' section completes VarDef fields."
  :tags '(:unit)
  (let ((cands (beads-formula--schema-completions
                (beads-formula-edit-test--schema) "" "vars.feature")))
    (should (member "description" cands))
    (should (member "required" cands))
    (should-not (member "version" cands))))

(ert-deftest beads-formula-edit-test-completions-prefix ()
  "Prefix filtering narrows the candidate list."
  :tags '(:unit)
  (let ((cands (beads-formula--schema-completions
                (beads-formula-edit-test--schema) "vers" "")))
    (should (equal cands '("version")))))

(ert-deftest beads-formula-edit-test-completion-carries-field ()
  "Completion candidates carry their schema field for annotation."
  :tags '(:unit)
  (let* ((cands (beads-formula--schema-completions
                 (beads-formula-edit-test--schema) "required" "vars.x"))
         (field (get-text-property 0 'beads-formula-schema-field (car cands))))
    (should (beads-formula-schema-field-p field))
    (should (equal (oref field type) "bool"))))

;;; ========================================
;;; Validation (REQ-SF-061)
;;; ========================================

(ert-deftest beads-formula-edit-test-validate-text-required ()
  "Missing required top-level fields are reported."
  :tags '(:unit)
  (let ((diagnostics (beads-formula-edit--validate-text
                      "description = \"x\"\n" (beads-formula-edit-test--schema))))
    (should (cl-find "formula" diagnostics
                     :key (lambda (d) (plist-get d :message))
                     :test (lambda (a b) (string-match-p a b))))
    (should (cl-find "version" diagnostics
                     :key (lambda (d) (plist-get d :message))
                     :test (lambda (a b) (string-match-p a b))))))

(ert-deftest beads-formula-edit-test-validate-text-type-mismatch ()
  "A wrong top-level value type is reported with its line."
  :tags '(:unit)
  (let* ((text (concat "formula = \"x\"\n"
                       "version = \"nope\"\n"
                       "type = \"workflow\"\n"))
         (diagnostics (beads-formula-edit--validate-text
                       text (beads-formula-edit-test--schema)))
         (mismatch (cl-find "version" diagnostics
                            :key (lambda (d) (plist-get d :message))
                            :test (lambda (a b) (string-match-p a b)))))
    (should mismatch)
    (should (equal (plist-get mismatch :line) 2))))

(ert-deftest beads-formula-edit-test-validate-text-clean ()
  "A well-typed formula produces no diagnostics."
  :tags '(:unit)
  (should (null
           (beads-formula-edit--validate-text
            "formula = \"x\"\nversion = 1\ntype = \"workflow\"\ndescription = \"d\"\n"
            (beads-formula-edit-test--schema)))))

(ert-deftest beads-formula-edit-test-parse-bd-error ()
  "A `bd' TOML parse message becomes a diagnostic with its line."
  :tags '(:unit)
  (let ((diagnostics
         (beads-formula-edit--parse-bd-error
          "Error: parse /tmp/bad.formula.toml: toml: toml: line 3 (last key \"version\"): incompatible types: TOML value has type string; destination has type integer\n\nSearch paths:\n")))
    (should (= (length diagnostics) 1))
    (should (equal (plist-get (car diagnostics) :line) 3))
    (should (string-match-p "incompatible types"
                            (plist-get (car diagnostics) :message)))))

(ert-deftest beads-formula-edit-test-validate-renders-inline-and-results ()
  "Validation renders inline overlays and a compile-style buffer."
  :tags '(:unit)
  (let ((source (generate-new-buffer " *beads-test-formula*")))
    (unwind-protect
        (with-current-buffer source
          (insert "version = \"nope\"\n")
          (setq-local beads-formula-edit--formula-name nil)
          (cl-letf (((symbol-function 'beads-formula-edit--schema)
                     (lambda () (beads-formula-edit-test--schema)))
                    ((symbol-function 'beads-formula-edit--bd-diagnostics)
                     (lambda (_name) nil))
                    ((symbol-function 'display-buffer)
                     (lambda (&rest _) nil)))
            (let ((diagnostics (beads-formula-edit-validate source)))
              (should diagnostics)
              ;; Inline overlay on the offending line.
              (should (cl-some (lambda (ov)
                                 (overlay-get ov 'beads-formula-edit-diagnostic))
                               (overlays-in (point-min) (point-max))))
              ;; Compile-style results buffer.
              (with-current-buffer beads-formula-edit-validate-buffer-name
                (should (derived-mode-p 'beads-formula-edit-validate-mode))
                (should (text-property-any
                         (point-min) (point-max)
                         'beads-formula-edit-source source))))))
      (when (buffer-live-p source) (kill-buffer source))
      (when (get-buffer beads-formula-edit-validate-buffer-name)
        (kill-buffer beads-formula-edit-validate-buffer-name)))))

(ert-deftest beads-formula-edit-test-validate-jump ()
  "`RET' in the results buffer jumps to the diagnostic line."
  :tags '(:unit)
  (let ((source (generate-new-buffer " *beads-test-formula*")))
    (unwind-protect
        (with-current-buffer source
          (insert "one\nversion = \"nope\"\n")
          (let ((results (beads-formula-edit--show-results
                          source (list (list :line 2 :severity 'error
                                             :message "bad")))))
            (with-current-buffer results
              (goto-char (or (text-property-any
                              (point-min) (point-max)
                              'beads-formula-edit-source source)
                             (point-min)))
              (beads-formula-edit-validate-jump)
              (should (eq (current-buffer) source))
              (should (equal (line-number-at-pos) 2)))))
      (when (buffer-live-p source) (kill-buffer source))
      (when (get-buffer beads-formula-edit-validate-buffer-name)
        (kill-buffer beads-formula-edit-validate-buffer-name)))))

;;; ========================================
;;; Convert JSON -> TOML (REQ-SF-062)
;;; ========================================

(ert-deftest beads-formula-edit-test-convert-command-line ()
  "The convert command carries the source and the option flags."
  :tags '(:unit)
  (let* ((state (list :source "/tmp/x.formula.json"
                      :stdout t :delete t))
         (cmd (beads-formula-edit-convert--command state))
         (args (beads-command-line cmd)))
    (should (member "convert" args))
    (should (member "/tmp/x.formula.json" args))
    (should (member "--stdout" args))
    (should (member "--delete" args))
    (should-not (member "--json" args))))

(ert-deftest beads-formula-edit-test-convert-command-line-plain ()
  "Without toggles the convert command writes a TOML file."
  :tags '(:unit)
  (let* ((state (list :source "/tmp/x.formula.json"
                      :stdout nil :delete nil))
         (args (beads-command-line (beads-formula-edit-convert--command state))))
    (should-not (member "--stdout" args))
    (should-not (member "--delete" args))))

(ert-deftest beads-formula-edit-test-convert-stdout-buffer ()
  "`--stdout' shows the TOML in the conversion output buffer."
  :tags '(:unit)
  (let ((state (list :source "/tmp/x.formula.json" :stdout t :delete nil)))
    (unwind-protect
        (cl-letf (((symbol-function 'beads-command-execute)
                   (lambda (&rest _) "formula = \"x\"\n"))
                  ((symbol-function 'display-buffer)
                   (lambda (&rest _) nil)))
          (beads-formula-edit-convert--apply state)
          (with-current-buffer beads-formula-edit-convert-buffer-name
            (should (string-match-p "formula = \"x\"" (buffer-string)))))
      (when (get-buffer beads-formula-edit-convert-buffer-name)
        (kill-buffer beads-formula-edit-convert-buffer-name)))))

(ert-deftest beads-formula-edit-test-convert-entry-point ()
  "The entry point seeds the state and opens the convert transient."
  :tags '(:unit)
  (with-temp-buffer
    (setq buffer-file-name "/tmp/x.formula.json")
    (let ((setup-prefix nil))
      (cl-letf (((symbol-function 'transient-setup)
                 (lambda (prefix &rest _) (setq setup-prefix prefix))))
        (beads-formula-convert-at-point)
        (should (eq setup-prefix 'beads-formula-edit-convert--transient))
        (should (equal (beads-formula-edit-convert--get :source)
                       "/tmp/x.formula.json"))))))

(ert-deftest beads-formula-edit-test-convert-entry-point-rejects-non-json ()
  "The entry point rejects a buffer that is not a JSON formula."
  :tags '(:unit)
  (with-temp-buffer
    (setq buffer-file-name "/tmp/x.formula.toml")
    (should-error (beads-formula-convert-at-point) :type 'user-error)))

;;; ========================================
;;; Minor mode
;;; ========================================

(ert-deftest beads-formula-edit-test-minor-mode-keys ()
  "The authoring minor mode binds the validate/save/convert keys."
  :tags '(:unit)
  (with-temp-buffer
    (beads-formula-edit-minor-mode 1)
    (should (eq (key-binding (kbd "C-c C-c")) #'beads-formula-edit-save))
    (should (eq (key-binding (kbd "C-c C-v")) #'beads-formula-edit-validate))
    (should (eq (key-binding (kbd "C-c C-x")) #'beads-formula-convert-at-point))
    (should (memq #'beads-formula-edit--after-save after-save-hook))
    (should (memq #'beads-formula-edit-completion-at-point
                  completion-at-point-functions))))

;;; ========================================
;;; Integration: scaffold and validate with bd
;;; ========================================

(ert-deftest beads-formula-edit-test-validate-integration ()
  "A scaffolded formula validates clean; a wrong type is caught by `bd'."
  :tags '(:integration :slow)
  (beads-test-skip-unless-bd)
  (beads-formula-edit-forget-schema)
  (beads-test-with-temp-repo (:init-beads t)
    (let ((buffer (beads-formula-new "valid-flow" "workflow" "project")))
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (should (null (beads-formula-edit-validate buffer))))
            ;; A wrong `version' type on disk is reported by the `bd' TOML parser.
            (let ((bad (expand-file-name "bad-flow.formula.toml"
                                         (file-name-directory
                                          (buffer-file-name buffer)))))
              (with-temp-file bad
                (insert "formula = \"bad-flow\"\nversion = \"nope\"\ntype = \"workflow\"\n"))
              (with-current-buffer buffer
                (setq-local beads-formula-edit--formula-name "bad-flow")
                (let ((diagnostics (beads-formula-edit-validate buffer)))
                  (should diagnostics)
                  (should (cl-some
                           (lambda (d)
                             (and (equal (plist-get d :line) 2)
                                  (string-match-p
                                   "incompatible types"
                                   (plist-get d :message))))
                           diagnostics))))))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(provide 'beads-formula-edit-test)
;;; beads-formula-edit-test.el ends here
