;;; beads-command-formula-coverage-test.el --- Extra coverage for formula UI -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; Additional `:unit' coverage for `beads-command-formula.el': the
;; schema parser/command, the convert/list/show entry points (store
;; mocked), buffer selection, and the show renderer's remaining
;; branches (gates, bond points, composition, source).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'beads-types)
(require 'beads-command-formula)

;;; Fixtures

(defconst beads-formula-cov--summary
  (beads-formula-summary :name "cov-formula"
                         :formula-type "workflow"
                         :description "A coverage formula"
                         :steps 2 :vars 1
                         :source "/tmp/cov-formula.toml")
  "A summary object for the list/show entry points.")

(defconst beads-formula-cov--formula
  (beads-formula
   :name "cov-formula"
   :formula-type "workflow"
   :version 2
   :description "A coverage formula"
   :phase "persistent"
   :extends '("base")
   :aspects '("security")
   :expansions '("fancy")
   :source "/tmp/cov-formula.toml"
   :vars (list (beads-formula-var :name "feature"
                                  :description "Feature desc"
                                  :var-type "string"
                                  :required t
                                  :enum '("a" "b")
                                  :pattern "^[a-z]+$"
                                  :default "a"))
   :steps (list (beads-formula-step :id "s1" :title "Step one"
                                    :step-type "task"
                                    :depends-on '("s0")
                                    :waits-for "gate-1"
                                    :gate (beads-formula-gate :type "timer"
                                                              :id "g1"
                                                              :timeout "10m"
                                                              :repo "r/x")
                                    :description "First"))
   :bond-points (list (beads-formula-bond-point :id "bp1"
                                                :before-step "s1"
                                                :parallel t
                                                :description "attach here")
                      (beads-formula-bond-point :id "bp2"
                                                :after-step "s1")))
  "A fully-populated formula for renderer coverage.")

;;; Schema parser and commands

(ert-deftest beads-formula-cov-schema-parse-test ()
  "Unit test: the schema parser decodes the struct array."
  :tags '(:unit)
  (let* ((json "[{\"name\":\"LoopSpec\",\"doc\":\"loop\",\"fields\":[]}]")
         (parsed (beads-command-parse (beads-command-formula-schema) json)))
    (should (= 1 (length parsed)))
    (should (beads-formula-schema-struct-p (car parsed)))
    (should (equal (oref (car parsed) name) "LoopSpec")))
  ;; Empty/nil output yields nil.
  (should-not (beads-command-parse (beads-command-formula-schema) ""))
  (should-not (beads-command-parse (beads-command-formula-schema) nil)))

(ert-deftest beads-formula-cov-schema-command-test ()
  "Unit test: `beads-formula-schema' forwards a primitive when given."
  :tags '(:unit)
  (let ((captured nil))
    (cl-letf (((symbol-function 'beads-check-executable) #'ignore)
              ((symbol-function 'beads-command-execute-interactive)
               (lambda (cmd) (setq captured cmd))))
      ;; Full index: no primitive.
      (beads-formula-schema "")
      (should-not (oref captured primitive))
      ;; Primitive: forwarded verbatim.
      (beads-formula-schema "LoopSpec")
      (should (equal (oref captured primitive) "LoopSpec")))))

(ert-deftest beads-formula-cov-convert-command-test ()
  "Unit test: `beads-formula-convert' executes the convert command."
  :tags '(:unit)
  (let ((captured nil))
    (cl-letf (((symbol-function 'beads-check-executable) #'ignore)
              ((symbol-function 'beads-command-execute)
               (lambda (cmd) (setq captured cmd))))
      (beads-formula-convert "cov-formula")
      (should (beads-command-formula-convert-p captured))
      (should (equal (oref captured formula-name) "cov-formula")))))

;;; Buffer management

(ert-deftest beads-formula-cov-get-or-create-buffer-test ()
  "Unit test: a fresh project gets a new formula-list buffer."
  :tags '(:unit)
  (cl-letf (((symbol-function 'beads--project-root) (lambda () "/tmp/cov-proj"))
            ((symbol-function 'beads--project-name-for-root) (lambda (_) "cov-proj"))
            ((symbol-function 'beads-formula-list--find-buffer-for-project)
             (lambda (_) nil)))
    (let ((buf (beads-formula-list--get-or-create-buffer)))
      (unwind-protect
          (should (equal (buffer-name buf) "*beads-formula-list[cov-proj]*"))
        (kill-buffer buf)))))

(ert-deftest beads-formula-cov-current-formula-name-test ()
  "Unit test: the formula at point requires a string id."
  :tags '(:unit)
  (cl-letf (((symbol-function 'tabulated-list-get-id) (lambda () "cov-formula")))
    (should (equal (beads-formula-list--current-formula-name) "cov-formula")))
  (cl-letf (((symbol-function 'tabulated-list-get-id) (lambda () '(header))))
    (should-not (beads-formula-list--current-formula-name))))

(ert-deftest beads-formula-cov-list-show-test ()
  "Unit test: `beads-formula-list-show' opens the row's formula."
  :tags '(:unit)
  (let ((shown nil))
    (cl-letf (((symbol-function 'beads-formula-list--current-formula-name)
               (lambda () "cov-formula"))
              ((symbol-function 'beads-formula-show)
               (lambda (name) (setq shown name))))
      (beads-formula-list-show)
      (should (equal shown "cov-formula")))))

(ert-deftest beads-formula-cov-list-nav-test ()
  "Unit test: list navigation skips the tabulated header line."
  :tags '(:unit)
  ;; next: from line 1 the header is skipped to the first data line.
  (with-temp-buffer
    (let ((inhibit-read-only t))
      (beads-formula-list-mode)
      (insert "Name\nformula-1\n")
      (goto-char (point-min))
      (beads-formula-list-next)
      ;; Either moved to line 2 or skipped to line 3 when line 2 has no id.
      (should (>= (line-number-at-pos) 2))))
  ;; previous: clamp to line 1 and then forward to line 1.
  (with-temp-buffer
    (let ((inhibit-read-only t))
      (beads-formula-list-mode)
      (insert "Name\nformula-1\n")
      (goto-char (point-min))
      (forward-line 1)
      (beads-formula-list-previous)
      ;; Clamps to the first data line (skips the header).
      (should (= (line-number-at-pos) 2)))))

(ert-deftest beads-formula-cov-list-open-source-test ()
  "Unit test: opening the source finds the row's file."
  :tags '(:unit)
  (let ((opened nil))
    (cl-letf (((symbol-function 'beads-formula-list--current-formula-name)
               (lambda () "cov-formula"))
              ((symbol-function 'beads-formula-list--get-formula-by-name)
               (lambda (_) beads-formula-cov--summary))
              ((symbol-function 'find-file) (lambda (f) (setq opened f))))
      (beads-formula-list-open-source)
      (should (equal opened "/tmp/cov-formula.toml")))))

;;; Show buffer plumbing

(ert-deftest beads-formula-cov-find-visible-buffer-test ()
  "Unit test: a visible show buffer for the project is found."
  :tags '(:unit)
  (let ((buf (generate-new-buffer " *cov-formula-show*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (beads-formula-show-mode)
            (setq beads-formula-show--project-dir "/tmp/cov-proj"))
          (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) buf)))
            (should (eq (beads-formula-show--find-visible-buffer "/tmp/cov-proj") buf))
            (should-not (beads-formula-show--find-visible-buffer "/tmp/other"))))
      (kill-buffer buf))))

(ert-deftest beads-formula-cov-show-refresh-test ()
  "Unit test: the show refresh requires and reloads the formula."
  :tags '(:unit)
  (with-temp-buffer
    (beads-formula-show-mode)
    (setq beads-formula-show--formula-name nil)
    (should-error (beads-formula-show-refresh) :type 'user-error))
  (with-temp-buffer
    (beads-formula-show-mode)
    (setq beads-formula-show--formula-name "cov-formula")
    (cl-letf (((symbol-function 'beads-command-execute)
               (lambda (&rest _) beads-formula-cov--formula)))
      (beads-formula-show-refresh)
      (should (beads-formula-p beads-formula-show--formula-data))
      (should (string-match-p "Formula: cov-formula" (buffer-string))))))

(ert-deftest beads-formula-cov-show-open-source-test ()
  "Unit test: opening the show source handles nil and success."
  :tags '(:unit)
  (with-temp-buffer
    (beads-formula-show-mode)
    (setq beads-formula-show--formula-data nil)
    (should-error (beads-formula-show-open-source) :type 'user-error))
  (with-temp-buffer
    (beads-formula-show-mode)
    (setq beads-formula-show--formula-data beads-formula-cov--formula)
    (let ((opened nil))
      (cl-letf (((symbol-function 'find-file) (lambda (f) (setq opened f))))
        (beads-formula-show-open-source)
        (should (equal opened "/tmp/cov-formula.toml"))))))

;;; Renderer branches

(ert-deftest beads-formula-cov-render-var-rich-test ()
  "Unit test: all render-var fields are emitted."
  :tags '(:unit)
  (with-temp-buffer
    (beads-formula-show--render-var
     "feature"
     (beads-formula-var :name "feature" :var-type "string"
                        :required t :description "desc"
                        :enum '("a" "b") :pattern "^x$" :default "a"))
    (let ((text (buffer-string)))
      (should (string-match-p "type string" text))
      (should (string-match-p "required" text))
      (should (string-match-p "Enum: a, b" text))
      (should (string-match-p "Pattern: \\^x\\$" text))
      (should (string-match-p "Default: a" text)))))

(ert-deftest beads-formula-cov-render-gate-test ()
  "Unit test: gates render their non-nil parts."
  :tags '(:unit)
  (should-not (beads-formula-show--render-gate (beads-formula-gate)))
  (should (equal (beads-formula-show--render-gate
                  (beads-formula-gate :type "timer" :id "g1"
                                      :timeout "10m" :repo "r/x"))
                 "timer g1 timeout 10m repo r/x")))

(ert-deftest beads-formula-cov-render-step-rich-test ()
  "Unit test: step rendering covers type, deps, waits-for, and gate."
  :tags '(:unit)
  (with-temp-buffer
    (beads-formula-show--render-step
     (beads-formula-step :id "s1" :title "One"
                         :step-type "task"
                         :depends-on '("s0")
                         :waits-for "gate-1"
                         :gate (beads-formula-gate :type "timer" :id "g1")
                         :description "line1\nline2")
     0)
    (let ((text (buffer-string)))
      (should (string-match-p "Type: task" text))
      (should (string-match-p "Depends on: s0" text))
      (should (string-match-p "Waits for: gate-1" text))
      (should (string-match-p "Gate: timer g1" text))
      (should (string-match-p "line2" text)))))

(ert-deftest beads-formula-cov-render-bond-point-test ()
  "Unit test: bond points render before/after and parallel."
  :tags '(:unit)
  (with-temp-buffer
    (beads-formula-show--render-bond-point
     (beads-formula-bond-point :id "bp1" :before-step "s1"
                               :parallel t :description "attach"))
    (let ((text (buffer-string)))
      (should (string-match-p "before s1" text))
      (should (string-match-p "parallel" text))
      (should (string-match-p "attach" text))))
  (with-temp-buffer
    (beads-formula-show--render-bond-point
     (beads-formula-bond-point :id "bp2" :after-step "s1"))
    (should (string-match-p "after s1" (buffer-string)))))

(ert-deftest beads-formula-cov-render-composition-test ()
  "Unit test: composition renders extends/aspects/expansions."
  :tags '(:unit)
  (with-temp-buffer
    (beads-formula-show--render-composition beads-formula-cov--formula)
    (let ((text (buffer-string)))
      (should (string-match-p "COMPOSITION" text))
      (should (string-match-p "extends: base" text))
      (should (string-match-p "aspects: security" text))
      (should (string-match-p "expansions: fancy" text))))
  ;; Nothing to render: no section, no output.
  (with-temp-buffer
    (beads-formula-show--render-composition (beads-formula :name "empty"))
    (should (equal (buffer-string) ""))))

(ert-deftest beads-formula-cov-render-full-test ()
  "Unit test: the full renderer covers bond points, composition, source."
  :tags '(:unit)
  (with-temp-buffer
    (beads-formula-show-mode)
    (beads-formula-show--render beads-formula-cov--formula)
    (let ((text (buffer-string)))
      (should (string-match-p "Bond points (2)" text))
      (should (string-match-p "COMPOSITION" text))
      (should (string-match-p "Source" text))
      (should (string-match-p "cov-formula.toml" text))
      (should (string-match-p "Steps (1)" text)))))

;;; Public entry points

(ert-deftest beads-formula-cov-list-entry-point-test ()
  "Unit test: `beads-formula-list' populates or reports an empty store."
  :tags '(:unit)
  (let ((buf (generate-new-buffer " *cov-formula-list*")))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'beads-check-executable) #'ignore)
                    ((symbol-function 'beads--project-root) (lambda () "/tmp/cov-proj"))
                    ((symbol-function 'beads-formula-list--get-or-create-buffer)
                     (lambda () buf))
                    ((symbol-function 'beads-command-execute)
                     (lambda (&rest _) (list beads-formula-cov--summary)))
                    ((symbol-function 'beads-buffer-display-same-or-reuse)
                     (lambda (b) b)))
            (should (eq (beads-formula-list) buf))
            (with-current-buffer buf
              (should (derived-mode-p 'beads-formula-list-mode))
              (should (= 1 (beads-pager--total-count)))))
          ;; Empty result: prints the empty list and reports.
          (cl-letf (((symbol-function 'beads-check-executable) #'ignore)
                    ((symbol-function 'beads--project-root) (lambda () "/tmp/cov-proj"))
                    ((symbol-function 'beads-formula-list--get-or-create-buffer)
                     (lambda () buf))
                    ((symbol-function 'beads-command-execute) (lambda (&rest _) nil))
                    ((symbol-function 'beads-buffer-display-same-or-reuse)
                     (lambda (b) b)))
            (should (eq (beads-formula-list) buf))))
      (kill-buffer buf))))

(ert-deftest beads-formula-cov-show-entry-point-test ()
  "Unit test: `beads-formula-show' renders into a project-qualified buffer."
  :tags '(:unit)
  (cl-letf (((symbol-function 'beads-check-executable) #'ignore)
            ((symbol-function 'beads--project-root) (lambda () "/tmp/cov-proj"))
            ((symbol-function 'beads--project-name-for-root) (lambda (_) "cov-proj"))
            ((symbol-function 'beads-command-execute)
             (lambda (&rest _) beads-formula-cov--formula))
            ((symbol-function 'beads-buffer-display-same-or-reuse)
             (lambda (b) b)))
    (let ((buf (beads-formula-show "cov-formula")))
      (unwind-protect
          (progn
            (should (equal (buffer-name buf) "*beads-formula[cov-proj]/cov-formula*"))
            (with-current-buffer buf
              (should (derived-mode-p 'beads-formula-show-mode))
              (should (equal beads-formula-show--formula-name "cov-formula"))
              (should (string-match-p "Formula: cov-formula" (buffer-string)))))
        (kill-buffer buf)))))

(ert-deftest beads-formula-cov-menu-list-test ()
  "Unit test: the menu suffix forwards the --type filter to the browser."
  :tags '(:unit)
  (let ((captured 'unset))
    (cl-letf (((symbol-function 'transient-args)
               (lambda (&rest _) '("--type=aspect")))
              ((symbol-function 'transient-arg-value)
               (lambda (_key _args) "aspect"))
              ((symbol-function 'beads-formula-browse)
               (lambda (type) (setq captured type))))
      (beads-formula-menu--list)
      (should (equal captured "aspect")))))

(provide 'beads-command-formula-coverage-test)
;;; beads-command-formula-coverage-test.el ends here
