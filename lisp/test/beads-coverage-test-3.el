;;; beads-coverage-test-3.el --- Coverage gap tests batch 3 -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; Third batch of targeted tests to increase code coverage to 85%.
;; Covers uncovered code paths in:
;; - beads-command-label.el (validate, execute-interactive, list)
;; - beads-command-create.el (parse, execute-interactive)
;; - beads-command-delete.el (parse, preview, delete workflow)
;; - beads-command-reopen.el (execute-interactive)
;; - beads-command-ready.el (command-line, validate, parse)
;; - beads-agent-list.el (format, populate, refresh, list)
;; - beads-reader.el (various reader functions)
;; - beads-command-graph.el (get-dependencies)
;; - beads-agent-claudemacs.el (setup helpers)
;; - beads-command-edit.el (parse-transient-args, validate)
;; - beads-agent-claude-code.el (backend start/stop)
;; - beads-agent-claude-code-ide.el (backend start/stop)

;;; Code:

(require 'ert)
(require 'beads)
(require 'beads-agent)
(require 'beads-command-label)
(require 'beads-command-create)
(require 'beads-command-delete)
(require 'beads-command-reopen)
(require 'beads-command-ready)
(require 'beads-agent-list)
(require 'beads-reader)
(require 'beads-command-graph)
(require 'beads-command-edit)
(require 'beads-command-update)
(require 'beads-command-worktree)
(require 'beads-types)
(require 'beads-agent-backend)
(require 'beads-agent-claudemacs)
(require 'beads-agent-claude-code)
(require 'beads-agent-claude-code-ide)
(require 'beads-buffer)

;;; ============================================================
;;; beads-command-label.el - Validate and Execute Tests
;;; ============================================================

(ert-deftest beads-coverage-3-label-remove-validate-no-issue ()
  "Test label remove validation with no issue IDs."
  (let ((cmd (beads-command-label-remove :issue-ids nil :label "bug")))
    (should (stringp (beads-command-validate cmd)))))

(ert-deftest beads-coverage-3-label-remove-validate-no-label ()
  "Test label remove validation with no label."
  (let ((cmd (beads-command-label-remove :issue-ids '("bd-1") :label "")))
    (should (stringp (beads-command-validate cmd)))))

(ert-deftest beads-coverage-3-label-remove-validate-valid ()
  "Test label remove validation with valid args."
  (let ((cmd (beads-command-label-remove :issue-ids '("bd-1")
                                          :label "bug")))
    (should-not (beads-command-validate cmd))))

(ert-deftest beads-coverage-3-label-list-validate-no-issue ()
  "Test label list validation with no issue ID."
  (let ((cmd (beads-command-label-list :issue-id nil)))
    (should (beads-command-validate cmd))))

(ert-deftest beads-coverage-3-label-list-validate-empty ()
  "Test label list validation with empty issue ID."
  (let ((cmd (beads-command-label-list :issue-id "")))
    (should (beads-command-validate cmd))))

(ert-deftest beads-coverage-3-label-list-validate-valid ()
  "Test label list validation with valid issue ID."
  (let ((cmd (beads-command-label-list :issue-id "bd-1")))
    (should-not (beads-command-validate cmd))))

(ert-deftest beads-coverage-3-label-list-all-validate ()
  "Test label list-all validation (always valid)."
  (let ((cmd (beads-command-label-list-all)))
    (should-not (beads-command-validate cmd))))

(ert-deftest beads-coverage-3-label-add-execute-interactive ()
  "Test label add execute-interactive."
  (let* ((cmd (beads-command-label-add :issue-ids '("bd-1")
                                        :label "bug" :json t)))
    (cl-letf (((symbol-function 'beads-command-execute)
               (lambda (_cmd) nil))
              ((symbol-function 'beads--invalidate-completion-cache)
               (lambda () nil)))
      (beads-command-execute-interactive cmd))))

(ert-deftest beads-coverage-3-label-add-execute-interactive-multi ()
  "Test label add execute-interactive with multiple issues."
  (let* ((cmd (beads-command-label-add :issue-ids '("bd-1" "bd-2")
                                        :label "urgent" :json t)))
    (cl-letf (((symbol-function 'beads-command-execute)
               (lambda (_cmd) nil))
              ((symbol-function 'beads--invalidate-completion-cache)
               (lambda () nil)))
      (beads-command-execute-interactive cmd))))

(ert-deftest beads-coverage-3-label-remove-execute-interactive ()
  "Test label remove execute-interactive."
  (let* ((cmd (beads-command-label-remove :issue-ids '("bd-1")
                                           :label "bug" :json t)))
    (cl-letf (((symbol-function 'beads-command-execute)
               (lambda (_cmd) nil))
              ((symbol-function 'beads--invalidate-completion-cache)
               (lambda () nil)))
      (beads-command-execute-interactive cmd))))

(ert-deftest beads-coverage-3-label-list-interactive-with-labels ()
  "Test beads-label-list-interactive with labels returned."
  (cl-letf (((symbol-function 'beads-label-list)
             (lambda (_id) '("bug" "urgent"))))
    (beads-label-list-interactive "bd-1")))

(ert-deftest beads-coverage-3-label-list-interactive-no-labels ()
  "Test beads-label-list-interactive with no labels."
  (cl-letf (((symbol-function 'beads-label-list)
             (lambda (_id) nil)))
    (beads-label-list-interactive "bd-1")))

;;; ============================================================
;;; beads-command-create.el - Parse and Execute Tests
;;; ============================================================

(ert-deftest beads-coverage-3-create-parse-single-issue ()
  "Test create parse with single issue JSON."
  (let* ((json-str "{\"id\":\"bd-99\",\"title\":\"New\",\"status\":\"open\",\"priority\":2,\"type\":\"task\"}")
         (cmd (beads-command-create :json t)))
    (let ((result (beads-command-parse cmd json-str)))
      (should (cl-typep result 'beads-issue))
      (should (equal "bd-99" (oref result id))))))

(ert-deftest beads-coverage-3-create-parse-vector ()
  "Test create parse with array of issues."
  (let* ((json-str "[{\"id\":\"bd-1\",\"title\":\"A\",\"status\":\"open\",\"priority\":1,\"type\":\"task\"},{\"id\":\"bd-2\",\"title\":\"B\",\"status\":\"open\",\"priority\":2,\"type\":\"bug\"}]")
         (cmd (beads-command-create :json t)))
    (let ((result (beads-command-parse cmd json-str)))
      (should (listp result))
      (should (= 2 (length result)))
      (should (cl-typep (car result) 'beads-issue)))))

(ert-deftest beads-coverage-3-create-execute-interactive-single ()
  "Test create execute-interactive with single issue."
  (let* ((issue (beads-issue :id "bd-99" :title "New Issue"
                              :status "open" :priority 2
                              :issue-type "task"))
         (cmd (beads-command-create :json t)))
    (cl-letf (((symbol-function 'beads-command-execute)
               (lambda (_cmd) issue))
              ((symbol-function 'beads--invalidate-completion-cache)
               (lambda () nil))
              ((symbol-function 'y-or-n-p)
               (lambda (_prompt) nil)))
      (beads-command-execute-interactive cmd))))

(ert-deftest beads-coverage-3-create-execute-interactive-nil ()
  "Test create execute-interactive with nil result."
  (let* ((cmd (beads-command-create :json t)))
    (cl-letf (((symbol-function 'beads-command-execute)
               (lambda (_cmd) nil))
              ((symbol-function 'beads--invalidate-completion-cache)
               (lambda () nil)))
      (beads-command-execute-interactive cmd))))

(ert-deftest beads-coverage-3-create-execute-interactive-multi ()
  "Test create execute-interactive with multiple issues."
  (let* ((issues (list (beads-issue :id "bd-1" :title "A"
                                     :status "open" :priority 1
                                     :issue-type "task")
                       (beads-issue :id "bd-2" :title "B"
                                     :status "open" :priority 2
                                     :issue-type "bug")))
         (cmd (beads-command-create :json t)))
    (cl-letf (((symbol-function 'beads-command-execute)
               (lambda (_cmd) issues))
              ((symbol-function 'beads--invalidate-completion-cache)
               (lambda () nil)))
      (beads-command-execute-interactive cmd))))

;;; ============================================================
;;; beads-command-delete.el - Parse and Delete Tests
;;; ============================================================

(ert-deftest beads-coverage-3-delete-parse-no-json ()
  "Test delete parse without json flag."
  (let* ((cmd (beads-command-delete :issue-ids '("bd-1") :json nil)))
    (let ((result (beads-command-parse cmd "preview text")))
      (should (equal "preview text" result)))))

(ert-deftest beads-coverage-3-delete-parse-preview-mode ()
  "Test delete parse without force (preview mode)."
  (let* ((cmd (beads-command-delete :issue-ids '("bd-1")
                                     :json t :force nil)))
    (let ((result (beads-command-parse cmd "Would delete bd-1")))
      (should (equal "Would delete bd-1" result)))))

(ert-deftest beads-coverage-3-delete-parse-force-single ()
  "Test delete parse with force and single deletion JSON."
  (let* ((json-str "{\"deleted\":\"bd-1\",\"dependencies_removed\":2}")
         (cmd (beads-command-delete :issue-ids '("bd-1")
                                     :json t :force t)))
    (let ((result (beads-command-parse cmd json-str)))
      (should (consp result))
      (should (equal "bd-1" (alist-get 'deleted result))))))

(ert-deftest beads-coverage-3-delete-parse-force-daemon ()
  "Test delete parse with daemon RPC format."
  (let* ((json-str "{\"deleted_count\":3,\"total_count\":3}")
         (cmd (beads-command-delete :issue-ids '("bd-1")
                                     :json t :force t)))
    (let ((result (beads-command-parse cmd json-str)))
      (should (consp result))
      (should (equal 3 (alist-get 'deleted_count result))))))

(ert-deftest beads-coverage-3-delete-parse-force-vector ()
  "Test delete parse with force and vector result."
  (let* ((json-str "[{\"id\":\"bd-1\"},{\"id\":\"bd-2\"}]")
         (cmd (beads-command-delete :issue-ids '("bd-1" "bd-2")
                                     :json t :force t)))
    (let ((result (beads-command-parse cmd json-str)))
      (should (listp result))
      (should (= 2 (length result))))))

(ert-deftest beads-coverage-3-delete-parse-force-null-json ()
  "Test delete parse with force and null JSON."
  (let* ((cmd (beads-command-delete :issue-ids '("bd-1")
                                     :json t :force t)))
    (let ((result (beads-command-parse cmd "null")))
      (should-not result))))

(ert-deftest beads-coverage-3-delete-parse-force-legacy ()
  "Test delete parse with legacy format (id field)."
  (let* ((json-str "{\"id\":\"bd-1\",\"status\":\"deleted\"}")
         (cmd (beads-command-delete :issue-ids '("bd-1")
                                     :json t :force t)))
    (let ((result (beads-command-parse cmd json-str)))
      (should (consp result))
      (should (equal "bd-1" (alist-get 'id result))))))

(ert-deftest beads-coverage-3-delete-show-preview ()
  "Test beads-delete--show-preview creates a buffer."
  (let ((buf (beads-delete--show-preview "bd-1" "Would delete bd-1")))
    (unwind-protect
        (progn
          (should (bufferp buf))
          (with-current-buffer buf
            (should (string-match "bd-1" (buffer-string)))))
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

(ert-deftest beads-coverage-3-delete-validate-no-args ()
  "Test delete validate with no issue-ids or from-file."
  (let ((cmd (beads-command-delete :issue-ids nil :from-file nil)))
    (should (stringp (beads-command-validate cmd)))))

(ert-deftest beads-coverage-3-delete-validate-valid ()
  "Test delete validate with valid issue-ids."
  (let ((cmd (beads-command-delete :issue-ids '("bd-1"))))
    (should-not (beads-command-validate cmd))))

;;; ============================================================
;;; beads-command-reopen.el - Execute Interactive Tests
;;; ============================================================

(ert-deftest beads-coverage-3-reopen-execute-interactive-single ()
  "Test reopen execute-interactive with single issue."
  (let* ((issue (beads-issue :id "bd-1" :title "Reopened"
                              :status "open" :priority 1
                              :issue-type "task"))
         (cmd (beads-command-reopen :issue-ids '("bd-1") :json t)))
    (cl-letf (((symbol-function 'beads-command-execute)
               (lambda (_cmd) issue))
              ((symbol-function 'beads--invalidate-completion-cache)
               (lambda () nil)))
      (beads-command-execute-interactive cmd))))

(ert-deftest beads-coverage-3-reopen-execute-interactive-multi ()
  "Test reopen execute-interactive with multiple issues."
  (let* ((issues (list (beads-issue :id "bd-1" :title "A"
                                     :status "open" :priority 1
                                     :issue-type "task")
                       (beads-issue :id "bd-2" :title "B"
                                     :status "open" :priority 2
                                     :issue-type "bug")))
         (cmd (beads-command-reopen :issue-ids '("bd-1" "bd-2") :json t)))
    (cl-letf (((symbol-function 'beads-command-execute)
               (lambda (_cmd) issues))
              ((symbol-function 'beads--invalidate-completion-cache)
               (lambda () nil)))
      (beads-command-execute-interactive cmd))))

(ert-deftest beads-coverage-3-reopen-execute-interactive-nil ()
  "Test reopen execute-interactive with nil result."
  (let* ((cmd (beads-command-reopen :issue-ids '("bd-1") :json t)))
    (cl-letf (((symbol-function 'beads-command-execute)
               (lambda (_cmd) nil))
              ((symbol-function 'beads--invalidate-completion-cache)
               (lambda () nil)))
      (beads-command-execute-interactive cmd))))

;;; ============================================================
;;; beads-command-ready.el - Command Line and Validate Tests
;;; ============================================================

(ert-deftest beads-coverage-3-ready-command-line-basic ()
  "Test ready command-line with basic args."
  (let ((cmd (beads-command-ready :json t)))
    (let ((args (beads-command-line cmd)))
      (should (member "--json" args)))))

(ert-deftest beads-coverage-3-ready-command-line-flags ()
  "Test ready command-line with boolean flags."
  (let ((cmd (beads-command-ready :json t
                                   :include-deferred t
                                   :unassigned t)))
    (let ((args (beads-command-line cmd)))
      (should (member "--include-deferred" args))
      (should (member "--unassigned" args)))))

(ert-deftest beads-coverage-3-ready-command-line-string-opts ()
  "Test ready command-line with string options."
  (let ((cmd (beads-command-ready :json t
                                   :assignee "alice"
                                   :issue-type "bug"
                                   :sort "priority")))
    (let ((args (beads-command-line cmd)))
      (should (member "--assignee" args))
      (should (member "alice" args))
      (should (member "--type" args))
      (should (member "--sort" args)))))

(ert-deftest beads-coverage-3-ready-command-line-integer-opts ()
  "Test ready command-line with integer options."
  (let ((cmd (beads-command-ready :json t
                                   :limit 10
                                   :priority 2)))
    (let ((args (beads-command-line cmd)))
      (should (member "--limit" args))
      (should (member "10" args))
      (should (member "--priority" args))
      (should (member "2" args)))))

(ert-deftest beads-coverage-3-ready-command-line-labels ()
  "Test ready command-line with label lists."
  (let ((cmd (beads-command-ready :json t
                                   :label '("bug" "urgent")
                                   :label-any '("feature"))))
    (let ((args (beads-command-line cmd)))
      ;; Labels appear in command line (may be duplicated by global-args)
      (should (member "--label" args))
      (should (member "bug" args))
      (should (member "urgent" args))
      (should (member "--label-any" args))
      (should (member "feature" args)))))

(ert-deftest beads-coverage-3-ready-command-line-mol ()
  "Test ready command-line with mol options."
  (let ((cmd (beads-command-ready :json t
                                   :mol "my-mol"
                                   :mol-type "swarm"
                                   :parent "bd-100")))
    (let ((args (beads-command-line cmd)))
      (should (member "--mol" args))
      (should (member "my-mol" args))
      (should (member "--mol-type" args))
      (should (member "swarm" args))
      (should (member "--parent" args)))))

(ert-deftest beads-coverage-3-ready-validate-valid ()
  "Test ready validate with valid args."
  (let ((cmd (beads-command-ready :sort "priority" :priority 2)))
    (should-not (beads-command-validate cmd))))

(ert-deftest beads-coverage-3-ready-validate-bad-sort ()
  "Test ready validate with invalid sort."
  (let ((cmd (beads-command-ready :sort "invalid")))
    (should (stringp (beads-command-validate cmd)))))

(ert-deftest beads-coverage-3-ready-validate-bad-priority ()
  "Test ready validate with out-of-range priority."
  (let ((cmd (beads-command-ready :priority 99)))
    (should (stringp (beads-command-validate cmd)))))

(ert-deftest beads-coverage-3-ready-validate-bad-mol-type ()
  "Test ready validate with invalid mol-type."
  (let ((cmd (beads-command-ready :mol-type "invalid")))
    (should (stringp (beads-command-validate cmd)))))

(ert-deftest beads-coverage-3-ready-parse-success ()
  "Test ready parse with JSON array."
  (let* ((json-str "[{\"id\":\"bd-1\",\"title\":\"Ready\",\"status\":\"open\",\"priority\":1,\"type\":\"task\"}]")
         (cmd (beads-command-ready :json t)))
    (let ((result (beads-command-parse cmd json-str)))
      (should (listp result))
      (should (= 1 (length result)))
      (should (cl-typep (car result) 'beads-issue)))))

;;; ============================================================
;;; beads-agent-list.el - Format and Display Tests
;;; ============================================================

(ert-deftest beads-coverage-3-agent-list-format-duration-empty ()
  "Test format-duration with empty string."
  (should (equal "" (beads-agent-list--format-duration ""))))

(ert-deftest beads-coverage-3-agent-list-format-duration-nil ()
  "Test format-duration with nil."
  (should (equal "" (beads-agent-list--format-duration nil))))

(ert-deftest beads-coverage-3-agent-list-format-duration-recent ()
  "Test format-duration with a recent timestamp."
  ;; Use a timestamp 30 seconds ago
  (let* ((now (current-time))
         (past (time-subtract now (seconds-to-time 30)))
         (ts (format-time-string "%Y-%m-%dT%H:%M:%S%z" past)))
    (let ((result (beads-agent-list--format-duration ts)))
      (should (stringp result))
      (should (string-match "s$" result)))))

(ert-deftest beads-coverage-3-agent-list-format-duration-minutes ()
  "Test format-duration with minutes-old timestamp."
  (let* ((now (current-time))
         (past (time-subtract now (seconds-to-time 300)))
         (ts (format-time-string "%Y-%m-%dT%H:%M:%S%z" past)))
    (let ((result (beads-agent-list--format-duration ts)))
      (should (stringp result))
      (should (string-match "m$" result)))))

(ert-deftest beads-coverage-3-agent-list-format-duration-hours ()
  "Test format-duration with hours-old timestamp."
  (let* ((now (current-time))
         (past (time-subtract now (seconds-to-time 7200)))
         (ts (format-time-string "%Y-%m-%dT%H:%M:%S%z" past)))
    (let ((result (beads-agent-list--format-duration ts)))
      (should (stringp result))
      (should (string-match "h" result)))))

(ert-deftest beads-coverage-3-agent-list-format-duration-days ()
  "Test format-duration with days-old timestamp."
  (let* ((now (current-time))
         (past (time-subtract now (seconds-to-time 172800)))
         (ts (format-time-string "%Y-%m-%dT%H:%M:%S%z" past)))
    (let ((result (beads-agent-list--format-duration ts)))
      (should (stringp result))
      (should (string-match "d" result)))))

(ert-deftest beads-coverage-3-agent-list-status-face-running ()
  "Test status face for running."
  (should (beads-agent-list--status-face 'running)))

(ert-deftest beads-coverage-3-agent-list-status-face-stale ()
  "Test status face for stale."
  (should (beads-agent-list--status-face 'stale)))

(ert-deftest beads-coverage-3-agent-list-mode-defined ()
  "Test that beads-agent-list-mode is defined."
  (should (fboundp 'beads-agent-list-mode)))

(ert-deftest beads-coverage-3-agent-list-refresh-empty ()
  "Test agent-list-refresh with no sessions."
  (let ((buf (get-buffer-create "*test-agent-list*")))
    (unwind-protect
        (with-current-buffer buf
          (beads-agent-list-mode)
          (cl-letf (((symbol-function 'beads-agent--get-all-sessions)
                     (lambda () nil)))
            (beads-agent-list--populate-buffer)
            (should (= 0 (length tabulated-list-entries)))))
      (kill-buffer buf))))

(ert-deftest beads-coverage-3-agent-list-fetch-titles-empty ()
  "Test fetch-titles with no issue IDs."
  (let ((cache (beads-agent-list--fetch-titles nil)))
    (should (hash-table-p cache))
    (should (= 0 (hash-table-count cache)))))

(ert-deftest beads-coverage-3-agent-list-entry-point ()
  "Test that beads-agent-list is an interactive command."
  (should (commandp 'beads-agent-list)))

;;; ============================================================
;;; beads-reader.el - Reader Function Tests
;;; ============================================================

(ert-deftest beads-coverage-3-reader-create-parent ()
  "Test beads-reader-create-parent calls completion."
  (cl-letf (((symbol-function 'beads-completion-read-issue)
             (lambda (_prompt &rest _) "bd-10")))
    (should (equal "bd-10"
                   (beads-reader-create-parent nil nil nil)))))

(ert-deftest beads-coverage-3-reader-create-repo ()
  "Test beads-reader-create-repo reads a string."
  (cl-letf (((symbol-function 'read-string)
             (lambda (_prompt &rest _) "my-repo")))
    (should (equal "my-repo"
                   (beads-reader-create-repo nil nil nil)))))

(ert-deftest beads-coverage-3-reader-create-from-template ()
  "Test beads-reader-create-from-template."
  (cl-letf (((symbol-function 'completing-read)
             (lambda (_prompt _coll &rest _) "epic")))
    (should (equal "epic"
                   (beads-reader-create-from-template nil nil nil)))))

(ert-deftest beads-coverage-3-reader-update-status ()
  "Test beads-reader-update-status."
  (let ((beads-update--status nil))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt _coll &rest _) "in_progress")))
      (should (equal "in_progress"
                     (beads-reader-update-status nil nil nil))))))

(ert-deftest beads-coverage-3-reader-update-type ()
  "Test beads-reader-update-type."
  (let ((beads-update--type nil))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt _coll &rest _) "bug")))
      (should (equal "bug"
                     (beads-reader-update-type nil nil nil))))))

(ert-deftest beads-coverage-3-reader-update-title ()
  "Test beads-reader-update-title."
  (let ((beads-update--title "old title"))
    (cl-letf (((symbol-function 'read-string)
               (lambda (_prompt &rest _) "new title")))
      (should (equal "new title"
                     (beads-reader-update-title nil nil nil))))))

(ert-deftest beads-coverage-3-reader-update-assignee ()
  "Test beads-reader-update-assignee."
  (let ((beads-update--assignee "alice"))
    (cl-letf (((symbol-function 'read-string)
               (lambda (_prompt &rest _) "bob")))
      (should (equal "bob"
                     (beads-reader-update-assignee nil nil nil))))))

(ert-deftest beads-coverage-3-reader-update-external-ref ()
  "Test beads-reader-update-external-ref."
  (let ((beads-update--external-ref nil))
    (cl-letf (((symbol-function 'read-string)
               (lambda (_prompt &rest _) "gh-42")))
      (should (equal "gh-42"
                     (beads-reader-update-external-ref nil nil nil))))))

(ert-deftest beads-coverage-3-reader-close-issue-id ()
  "Test beads-reader-close-issue-id."
  (let ((beads-close--issue-id nil))
    (cl-letf (((symbol-function 'beads-completion-read-issue)
               (lambda (_prompt &rest _) "bd-5")))
      (should (equal "bd-5"
                     (beads-reader-close-issue-id nil nil nil))))))

(ert-deftest beads-coverage-3-reader-reopen-issue-id ()
  "Test beads-reader-reopen-issue-id."
  (let ((beads-reopen--issue-id nil))
    (cl-letf (((symbol-function 'beads-completion-read-issue)
               (lambda (_prompt &rest _) "bd-3")))
      (should (equal "bd-3"
                     (beads-reader-reopen-issue-id nil nil nil))))))

(ert-deftest beads-coverage-3-reader-edit-issue-id ()
  "Test beads-reader-edit-issue-id."
  (let ((beads-edit--issue-id nil))
    (cl-letf (((symbol-function 'beads-completion-read-issue)
               (lambda (_prompt &rest _) "bd-7")))
      (should (equal "bd-7"
                     (beads-reader-edit-issue-id nil nil nil))))))



(ert-deftest beads-coverage-3-reader-dep-add-issue-id ()
  "Test dep add issue ID reader."
  (cl-letf (((symbol-function 'beads-completion-read-issue)
             (lambda (_prompt &rest _) "bd-1")))
    (should (equal "bd-1"
                   (beads-reader-dep-add-issue-id "Issue: " nil nil)))))

(ert-deftest beads-coverage-3-reader-dep-add-depends-on ()
  "Test dep add depends-on ID reader."
  (cl-letf (((symbol-function 'beads-completion-read-issue)
             (lambda (_prompt &rest _) "bd-2")))
    (should (equal "bd-2"
                   (beads-reader-dep-add-depends-on-id "Dep: " nil nil)))))

(ert-deftest beads-coverage-3-reader-dep-add-type ()
  "Test dep add type reader."
  (cl-letf (((symbol-function 'completing-read)
             (lambda (_prompt _coll &rest _) "blocks")))
    (should (equal "blocks"
                   (beads-reader-dep-add-type "Type: " nil nil)))))

(ert-deftest beads-coverage-3-reader-dep-remove-issue-id ()
  "Test dep remove issue ID reader."
  (cl-letf (((symbol-function 'beads-completion-read-issue)
             (lambda (_prompt &rest _) "bd-3")))
    (should (equal "bd-3"
                   (beads-reader-dep-remove-issue-id "Issue: " nil nil)))))

(ert-deftest beads-coverage-3-reader-dep-remove-depends-on ()
  "Test dep remove depends-on ID reader."
  (cl-letf (((symbol-function 'beads-completion-read-issue)
             (lambda (_prompt &rest _) "bd-4")))
    (should (equal "bd-4"
                   (beads-reader-dep-remove-depends-on-id "Dep: " nil nil)))))

(ert-deftest beads-coverage-3-reader-create-file ()
  "Test beads-reader-create-file reads a file."
  (cl-letf (((symbol-function 'read-file-name)
             (lambda (_prompt &rest _) "/tmp/issues.md")))
    (should (equal "/tmp/issues.md"
                   (beads-reader-create-file nil nil nil)))))

(ert-deftest beads-coverage-3-reader-agent-backend-no-backends ()
  "Test agent backend reader with no backends."
  (cl-letf (((symbol-function 'beads-agent--get-all-backends)
             (lambda () nil)))
    (should-error (beads-reader-agent-backend nil nil nil)
                  :type 'user-error)))

;;; ============================================================
;;; beads-command-graph.el - Dependency Graph Tests
;;; ============================================================

(ert-deftest beads-coverage-3-graph-get-deps-empty ()
  "Test get-dependencies with no issues."
  (cl-letf (((symbol-function 'beads-command-execute)
             (lambda (_cmd) nil)))
    (should (null (beads-graph--get-dependencies)))))

(ert-deftest beads-coverage-3-graph-get-deps-with-issues ()
  "Test get-dependencies with issues and deps."
  (let* ((issue1 (beads-issue :id "bd-1" :title "A"
                               :status "open" :priority 1
                               :issue-type "task"))
         (dep1 (beads-dependency :issue-id "bd-1"
                                  :depends-on-id "bd-2"
                                  :type "blocks")))
    (cl-letf (((symbol-function 'beads-command-execute)
               (lambda (cmd)
                 (if (cl-typep cmd 'beads-command-list)
                     (list issue1)
                   (list dep1)))))
      (let ((deps (beads-graph--get-dependencies)))
        (should (= 1 (length deps)))
        (should (equal "bd-1" (plist-get (car deps) :from)))
        (should (equal "bd-2" (plist-get (car deps) :to)))))))

;;; ============================================================
;;; beads-agent-claudemacs.el - Setup Helper Tests
;;; ============================================================

(ert-deftest beads-coverage-3-claudemacs-setup-bell-no-buffer ()
  "Test bell handler setup when no claudemacs buffer."
  (cl-letf (((symbol-function 'claudemacs--get-buffer)
             (lambda () nil)))
    ;; Should not error when no buffer exists
    (beads-agent-claudemacs--setup-bell-handler-fixed)))

(ert-deftest beads-coverage-3-claudemacs-ensure-eat-no-eat ()
  "Test ensure-eat-gv-setter when eat not available."
  (cl-letf (((symbol-function 'featurep)
             (lambda (feat &rest _)
               (not (memq feat '(eat claudemacs)))))
            ((symbol-function 'require)
             (lambda (_feat &optional _noerr _nosuffix) nil)))
    ;; Should not error when eat is not available
    (beads-agent-claudemacs--ensure-eat-gv-setter)))

(ert-deftest beads-coverage-3-claudemacs-install-advice ()
  "Test bell handler advice installation."
  (let ((beads-agent-claudemacs--bell-handler-advice-installed t))
    ;; Already installed, should be a no-op
    (beads-agent-claudemacs--install-bell-handler-advice)))

;;; ============================================================
;;; beads-command-edit.el - Parse and Validate Tests
;;; ============================================================

(ert-deftest beads-coverage-3-edit-parse-transient-args ()
  "Test beads-edit--parse-transient-args."
  (let ((args '("--id=bd-1" "--title")))
    (let ((cmd (beads-edit--parse-transient-args args)))
      (should (cl-typep cmd 'beads-command-edit))
      (should (equal "bd-1" (oref cmd issue-id))))))

(ert-deftest beads-coverage-3-edit-validate-no-issue ()
  "Test edit validate with no issue ID."
  (should (beads-edit--validate-all
           (beads-command-edit :issue-id nil))))

(ert-deftest beads-coverage-3-edit-validate-with-issue ()
  "Test edit validate with valid issue ID."
  (should-not (beads-edit--validate-all
               (beads-command-edit :issue-id "bd-1"))))

(ert-deftest beads-coverage-3-edit-defined ()
  "Test beads-edit is an interactive command."
  (should (commandp 'beads-edit)))

;;; ============================================================
;;; beads-agent-claude-code.el - Backend Tests
;;; ============================================================

(ert-deftest beads-coverage-3-claude-code-backend-class ()
  "Test claude-code backend class exists."
  (should (find-class 'beads-agent-backend-claude-code)))

;;; ============================================================
;;; beads-agent-claude-code-ide.el - Backend Tests
;;; ============================================================

(ert-deftest beads-coverage-3-claude-code-ide-backend-class ()
  "Test claude-code-ide backend class exists."
  (should (find-class 'beads-agent-backend-claude-code-ide)))

;;; ============================================================
;;; beads-command-dep.el - Additional Validate Tests
;;; ============================================================

(ert-deftest beads-coverage-3-dep-list-command-line ()
  "Test dep list command-line construction."
  (let ((cmd (beads-command-dep-list :issue-id "bd-1" :json t)))
    (let ((args (beads-command-line cmd)))
      (should (member "bd-1" args)))))

;;; ============================================================
;;; beads-command-update.el - Additional Tests
;;; ============================================================

(ert-deftest beads-coverage-3-update-validate-missing-fields ()
  "Test update validate with missing required fields."
  (let ((cmd (beads-command-update :issue-ids nil :json t)))
    (should (stringp (beads-command-validate cmd)))))

;;; ============================================================
;;; beads-agent.el - Additional Mode-Line Tests
;;; ============================================================

(ert-deftest beads-coverage-3-agent-mode-line-format-full ()
  "Test mode-line full format with all context."
  (let ((beads-agent-mode-line-faces nil)
        (ctx (list :project-name "beads.el" :branch "main"
                   :in-worktree t :agent-type "task"
                   :agent-instance 1 :agent-session nil)))
    (let ((result (beads-agent--mode-line-format-full ctx)))
      (should (stringp result))
      (should (string-match "beads.el" result))
      (should (string-match "worktree" result)))))

(ert-deftest beads-coverage-3-agent-mode-line-format-full-nil ()
  "Test mode-line full format with nil project."
  (let ((ctx '(:project-name nil)))
    (should-not (beads-agent--mode-line-format-full ctx))))

(ert-deftest beads-coverage-3-agent-mode-line-compact-no-agent ()
  "Test mode-line compact format without agent."
  (let ((ctx (list :project-name "proj" :branch "main"
                   :in-worktree nil :agent-type nil)))
    (let ((result (beads-agent--mode-line-format-compact ctx)))
      (should (stringp result))
      (should (string-match "\\[p" result)))))

(ert-deftest beads-coverage-3-agent-mode-line-compact-nil-proj ()
  "Test mode-line compact format with nil project."
  (let ((ctx '(:project-name nil)))
    (should-not (beads-agent--mode-line-format-compact ctx))))

(ert-deftest beads-coverage-3-agent-mode-line-indicator-default ()
  "Test mode-line indicator with default format."
  (let ((beads-agent-mode-line-format 'default)
        (beads-agent-mode-line-faces nil))
    (cl-letf (((symbol-function 'beads-agent--mode-line-context)
               (lambda () '(:project-name "test" :branch "main"))))
      (should (stringp (beads-agent--mode-line-indicator))))))

(ert-deftest beads-coverage-3-agent-mode-line-indicator-compact ()
  "Test mode-line indicator with compact format."
  (let ((beads-agent-mode-line-format 'compact))
    (cl-letf (((symbol-function 'beads-agent--mode-line-context)
               (lambda () '(:project-name "test" :branch "main"))))
      (should (stringp (beads-agent--mode-line-indicator))))))

(ert-deftest beads-coverage-3-agent-mode-line-indicator-full ()
  "Test mode-line indicator with full format."
  (let ((beads-agent-mode-line-format 'full)
        (beads-agent-mode-line-faces nil))
    (cl-letf (((symbol-function 'beads-agent--mode-line-context)
               (lambda () '(:project-name "test" :branch "main"))))
      (should (stringp (beads-agent--mode-line-indicator))))))

(ert-deftest beads-coverage-3-agent-mode-line-indicator-custom ()
  "Test mode-line indicator with custom format function."
  (let ((beads-agent-mode-line-format (lambda () "[custom]")))
    (cl-letf (((symbol-function 'beads-agent--mode-line-context)
               (lambda () '(:project-name "test"))))
      (should (equal "[custom]" (beads-agent--mode-line-indicator))))))

(ert-deftest beads-coverage-3-agent-mode-line-cache-valid-p ()
  "Test mode-line cache validity check."
  (let ((beads-agent--mode-line-cache nil))
    (should-not (beads-agent--mode-line-cache-valid-p))))

(ert-deftest beads-coverage-3-agent-mode-line-mode-defined ()
  "Test beads-agent-mode-line-mode is defined."
  (should (fboundp 'beads-agent-mode-line-mode)))

;;; ============================================================
;;; beads-command-worktree.el - Additional Tests
;;; ============================================================

(ert-deftest beads-coverage-3-worktree-list-refresh-defined ()
  "Test worktree list refresh is defined."
  (should (fboundp 'beads-worktree-list-refresh)))

(ert-deftest beads-coverage-3-worktree-list-info-defined ()
  "Test worktree list info is defined."
  (should (fboundp 'beads-worktree-list-info)))

(ert-deftest beads-coverage-3-worktree-list-remove-defined ()
  "Test worktree list remove is defined."
  (should (fboundp 'beads-worktree-list-remove)))

(provide 'beads-coverage-test-3)
;;; beads-coverage-test-3.el ends here
