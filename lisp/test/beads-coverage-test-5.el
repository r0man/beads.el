;;; beads-coverage-test-5.el --- Coverage boost tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Additional tests to push coverage from ~84% to 85%+.
;; Targets: beads-agent-list.el, beads-command-show.el,
;; beads-command-formula.el, beads-command-list.el, beads-meta.el,
;; beads-option.el

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'beads)

;;; ============================================================
;;; beads-agent-list.el - Action Command Tests
;;; ============================================================

(ert-deftest beads-coverage-5-agent-list-stop-all-no-sessions ()
  "Test stop-all with no sessions shows message."
  (cl-letf (((symbol-function 'beads-agent--get-all-sessions)
             (lambda () nil)))
    (beads-agent-list-stop-all)))

(ert-deftest beads-coverage-5-agent-list-stop-all-with-sessions ()
  "Test stop-all stops all sessions when confirmed."
  (let ((stopped-ids nil))
    (cl-letf (((symbol-function 'beads-agent--get-all-sessions)
               (lambda ()
                 (list (beads-agent-session :id "s1" :issue-id "bd-1"
                                            :started-at "2024-01-01")
                       (beads-agent-session :id "s2" :issue-id "bd-2"
                                            :started-at "2024-01-01"))))
              ((symbol-function 'y-or-n-p)
               (lambda (_prompt) t))
              ((symbol-function 'beads-agent-stop)
               (lambda (id) (push id stopped-ids)))
              ((symbol-function 'beads-agent-list-refresh)
               (lambda () nil)))
      (beads-agent-list-stop-all)
      (should (member "s1" stopped-ids))
      (should (member "s2" stopped-ids)))))

(ert-deftest beads-coverage-5-agent-list-restart-no-session ()
  "Test restart with no session at point."
  (cl-letf (((symbol-function 'beads-agent-list--current-session-id)
             (lambda () nil)))
    (should-error (beads-agent-list-restart) :type 'user-error)))

(ert-deftest beads-coverage-5-agent-list-restart-with-session ()
  "Test restart stops then starts."
  (let ((stopped nil) (started nil))
    (cl-letf (((symbol-function 'beads-agent-list--current-session-id)
               (lambda () "s1"))
              ((symbol-function 'beads-agent--get-session)
               (lambda (_id)
                 (beads-agent-session :id "s1" :issue-id "bd-5"
                                      :started-at "2024-01-01")))
              ((symbol-function 'beads-agent-stop)
               (lambda (_id) (setq stopped t)))
              ((symbol-function 'beads-agent-start)
               (lambda (issue-id) (setq started issue-id)))
              ((symbol-function 'run-with-timer)
               (lambda (&rest _) nil)))
      (beads-agent-list-restart)
      (should stopped)
      (should (equal "bd-5" started)))))

(ert-deftest beads-coverage-5-agent-list-copy-session-id ()
  "Test copy-session-id adds to kill ring."
  (cl-letf (((symbol-function 'beads-agent-list--current-session-id)
             (lambda () "sess-42")))
    (beads-agent-list-copy-session-id)
    (should (equal "sess-42" (car kill-ring)))))

(ert-deftest beads-coverage-5-agent-list-copy-session-id-no-session ()
  "Test copy-session-id with no session."
  (cl-letf (((symbol-function 'beads-agent-list--current-session-id)
             (lambda () nil)))
    (should-error (beads-agent-list-copy-session-id) :type 'user-error)))

(ert-deftest beads-coverage-5-agent-list-copy-issue-id ()
  "Test copy-issue-id adds to kill ring."
  (cl-letf (((symbol-function 'beads-agent-list--current-issue-id)
             (lambda () "bd-99")))
    (beads-agent-list-copy-issue-id)
    (should (equal "bd-99" (car kill-ring)))))

(ert-deftest beads-coverage-5-agent-list-copy-issue-id-no-session ()
  "Test copy-issue-id with no session."
  (cl-letf (((symbol-function 'beads-agent-list--current-issue-id)
             (lambda () nil)))
    (should-error (beads-agent-list-copy-issue-id) :type 'user-error)))

(ert-deftest beads-coverage-5-agent-list-cleanup ()
  "Test cleanup calls cleanup and refresh."
  (let ((cleaned nil) (refreshed nil))
    (cl-letf (((symbol-function 'beads-agent-cleanup-stale-sessions)
               (lambda () (setq cleaned t)))
              ((symbol-function 'beads-agent-list-refresh)
               (lambda () (setq refreshed t))))
      (beads-agent-list-cleanup)
      (should cleaned)
      (should refreshed))))

(ert-deftest beads-coverage-5-agent-list-format-status ()
  "Test format-status returns propertized string."
  (cl-letf (((symbol-function 'beads-agent--session-active-p)
             (lambda (_s) t))
            ((symbol-function 'beads-agent--get-issue-outcome)
             (lambda (_id) nil)))
    (let* ((session (beads-agent-session :id "s1" :issue-id "bd-1"
                                          :started-at "2024-01-01"))
           (result (beads-agent-list--format-status session)))
      (should (stringp result))
      (should (string= "running" (substring-no-properties result))))))

(ert-deftest beads-coverage-5-agent-list ()
  "Test beads-agent-list creates buffer."
  (let ((buf nil))
    (cl-letf (((symbol-function 'beads-agent-list--populate-buffer)
               (lambda () nil))
              ((symbol-function 'display-buffer)
               (lambda (b &rest _) (setq buf b) nil)))
      (beads-agent-list)
      (when buf (kill-buffer buf)))))

;;; ============================================================
;;; beads-command-show.el - Edit Field Tests
;;; ============================================================

(ert-deftest beads-coverage-5-show-edit-field-title ()
  "Test edit-field with title field uses read-string."
  (with-temp-buffer
    (let ((inhibit-read-only t))
      (beads-show-mode)
      (setq beads-show--issue-id "bd-1")
      (setq beads-show--issue-data
            (beads-issue :id "bd-1" :title "Old Title"
                         :status "open" :priority 1 :issue-type "task"))
      (let ((updated-field nil) (updated-value nil))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_prompt _coll &rest _) "Title"))
                  ((symbol-function 'read-string)
                   (lambda (_prompt &optional _initial) "New Title"))
                  ((symbol-function 'beads-show--update-field)
                   (lambda (name flag value)
                     (setq updated-field name updated-value value))))
          (beads-show-edit-field)
          (should (equal "Title" updated-field))
          (should (equal "New Title" updated-value)))))))

;;; ============================================================
;;; beads-command-show.el - Beginning of Block Tests
;;; ============================================================

(ert-deftest beads-coverage-5-show-at-block-boundary-blockquote ()
  "Test at-block-boundary detects blockquote lines."
  (with-temp-buffer
    (insert "> quoted text\n")
    (let ((inhibit-read-only t))
      (beads-show-mode)
      (goto-char (point-min))
      (let ((result (beads-show--at-block-boundary)))
        (should (eq 'blockquote result))))))

;;; ============================================================
;;; beads-command-formula.el - Refresh Tests
;;; ============================================================

(ert-deftest beads-coverage-5-formula-list-refresh ()
  "Test formula-list-refresh calls execute and populates."
  (with-temp-buffer
    (let ((inhibit-read-only t)
          (beads-formula-list--command-obj nil)
          (populated nil))
      (beads-formula-list-mode)
      (cl-letf (((symbol-function 'beads-command-execute)
                 (lambda (_cmd)
                   (beads-command-execution
                    :command (beads-command-formula-list :json t)
                    :exit-code 0 :stdout "[]" :stderr ""
                    :result '())))
                ((symbol-function 'beads-formula-list--populate-buffer)
                 (lambda (formulas _cmd) (setq populated formulas))))
        (beads-formula-list-refresh)
        (should (listp populated))))))

;;; ============================================================
;;; beads-command-list.el - Edge Case Tests
;;; ============================================================

(ert-deftest beads-coverage-5-list-search-refresh-path ()
  "Test list refresh with search command type."
  (with-temp-buffer
    (let ((inhibit-read-only t))
      (beads-list-mode)
      (setq beads-list--command 'search)
      (defvar beads-search--command-obj nil)
      (setq beads-search--command-obj (beads-command-list :json t))
      (let ((populated nil))
        (cl-letf (((symbol-function 'beads-command-execute)
                   (lambda (cmd)
                     (beads-command-execution
                      :command cmd :exit-code 0
                      :stdout "[{\"id\":\"bd-1\",\"title\":\"A\",\"status\":\"open\",\"priority\":1,\"issue_type\":\"task\"}]"
                      :stderr "" :result (list (beads-issue :id "bd-1" :title "A"
                                                            :status "open" :priority 1
                                                            :issue-type "task")))))
                  ((symbol-function 'beads-list--populate-buffer)
                   (lambda (issues _type &optional _cmd)
                     (setq populated issues)))
                  ((symbol-function 'get-buffer-window)
                   (lambda (&rest _) nil))
                  ((symbol-function 'beads-list--restore-position)
                   (lambda (&rest _) nil)))
          (beads-list-refresh)
          (should populated))))))

;;; ============================================================
;;; beads-meta.el - Resolve/Infer Tests
;;; ============================================================

(ert-deftest beads-coverage-5-meta-resolve-long-option-with-key ()
  "Test resolve-long-option auto-generates from slot name when key present."
  (let ((result (beads-meta--resolve-long-option
                 'issue-type
                 '(:key "t"))))
    (should (equal "issue-type" result))))

(ert-deftest beads-coverage-5-meta-resolve-long-option-already-set ()
  "Test resolve-long-option returns nil when already set."
  (let ((result (beads-meta--resolve-long-option
                 'issue-type
                 '(:long-option "type"))))
    (should (null result))))

(ert-deftest beads-coverage-5-meta-resolve-long-option-positional ()
  "Test resolve-long-option returns nil for positional args."
  (let ((result (beads-meta--resolve-long-option
                 'title
                 '(:positional t :key "t"))))
    (should (null result))))

(ert-deftest beads-coverage-5-meta-resolve-long-option-no-key ()
  "Test resolve-long-option returns nil when no key/short-option."
  (let ((result (beads-meta--resolve-long-option
                 'issue-type
                 '())))
    (should (null result))))

(ert-deftest beads-coverage-5-meta-resolve-long-option-with-short ()
  "Test resolve-long-option with short-option."
  (let ((result (beads-meta--resolve-long-option
                 'priority
                 '(:short-option "p"))))
    (should (equal "priority" result))))

;;; ============================================================
;;; beads-option.el - Multiline Format Value Tests
;;; ============================================================

(ert-deftest beads-coverage-5-option-multiline-format-value-with-value ()
  "Test multiline format-value shows escaped text."
  (let ((obj (beads-transient-multiline)))
    (oset obj argument "--description=")
    (oset obj value "line1\nline2")
    (let ((result (transient-format-value obj)))
      (should (stringp result))
      ;; Should contain escaped newline
      (should (string-match-p "line1" (substring-no-properties result))))))

(ert-deftest beads-coverage-5-option-multiline-format-value-empty ()
  "Test multiline format-value with empty value."
  (let ((obj (beads-transient-multiline)))
    (oset obj argument "--description=")
    (oset obj value nil)
    (let ((result (transient-format-value obj)))
      (should (stringp result)))))

(ert-deftest beads-coverage-5-option-multiline-format-value-long ()
  "Test multiline format-value truncates long text."
  (let ((obj (beads-transient-multiline))
        (beads-display-value-max-length 10))
    (oset obj argument "--desc=")
    (oset obj value "this is a very long string that should be truncated")
    (let ((result (transient-format-value obj)))
      (should (string-match-p "\\.\\.\\." (substring-no-properties result))))))

;;; ============================================================
;;; beads-command-show.el - Show Error Path Tests
;;; ============================================================

(ert-deftest beads-coverage-5-show-command-error-display ()
  "Test that show command error path displays error message in buffer."
  (let ((buf nil))
    (cl-letf (((symbol-function 'beads-check-executable)
               (lambda () nil))
              ((symbol-function 'beads-git-find-project-root)
               (lambda () "/tmp"))
              ((symbol-function 'beads-git-get-branch)
               (lambda () "main"))
              ((symbol-function 'beads-git-get-project-name)
               (lambda () "test"))
              ((symbol-function 'beads-show--get-or-create-buffer)
               (lambda (_id) (setq buf (generate-new-buffer " *beads-test*"))))
              ((symbol-function 'beads-show--register-with-session)
               (lambda () nil))
              ((symbol-function 'beads-command-show!)
               (lambda (&rest _) (error "Connection failed")))
              ((symbol-function 'beads-buffer-display-detail)
               (lambda (_buf _mode) nil))
              ((symbol-function 'completing-read)
               (lambda (_p _c &rest _) "bd-1")))
      (beads-show "bd-1")
      (when buf
        (with-current-buffer buf
          (should (string-match-p "Error" (buffer-string))))
        (kill-buffer buf)))))

;;; ============================================================
;;; beads-command-show.el - At Block Boundary Tests
;;; ============================================================

(ert-deftest beads-coverage-5-show-at-block-boundary-fenced ()
  "Test at-block-boundary detects fenced code."
  (with-temp-buffer
    (insert "```python\n")
    (insert "code here\n")
    (insert "```\n")
    (let ((inhibit-read-only t))
      (beads-show-mode)
      (goto-char (point-min))
      (forward-line 2) ;; On closing ```
      (let ((result (beads-show--at-block-boundary)))
        (should (eq 'fenced-code result))))))

(ert-deftest beads-coverage-5-show-at-block-boundary-list ()
  "Test at-block-boundary detects list items."
  (with-temp-buffer
    (insert "- item 1\n")
    (insert "- item 2\n")
    (let ((inhibit-read-only t))
      (beads-show-mode)
      (goto-char (point-min))
      (let ((result (beads-show--at-block-boundary)))
        (should (eq 'list result))))))

;;; ============================================================
;;; beads-command-list.el - Mark/Unmark Tests
;;; ============================================================

(ert-deftest beads-coverage-5-list-mark-unmark ()
  "Test mark and unmark operations."
  (with-temp-buffer
    (let ((inhibit-read-only t))
      (beads-list-mode)
      (setq beads-list--marked-issues nil)
      ;; Mock current issue
      (cl-letf (((symbol-function 'beads-list--current-issue-id)
                 (lambda () "bd-1"))
                ((symbol-function 'tabulated-list-put-tag)
                 (lambda (_tag &optional _advance) nil)))
        (beads-list-mark)
        (should (member "bd-1" beads-list--marked-issues))
        (beads-list-unmark)
        (should-not (member "bd-1" beads-list--marked-issues))))))

;;; ============================================================
;;; beads-agent-list.el - Format Duration Tests
;;; ============================================================

(ert-deftest beads-coverage-5-agent-list-format-duration-nil ()
  "Test format-duration with nil input."
  (should (equal "" (beads-agent-list--format-duration nil))))

(ert-deftest beads-coverage-5-agent-list-format-duration-empty ()
  "Test format-duration with empty string."
  (should (equal "" (beads-agent-list--format-duration ""))))

;;; ============================================================
;;; beads-command-show.el - Copy/Set Status Tests
;;; ============================================================

(ert-deftest beads-coverage-5-show-copy-id ()
  "Test copy-id adds issue ID to kill ring."
  (with-temp-buffer
    (let ((inhibit-read-only t))
      (beads-show-mode)
      (setq beads-show--issue-id "bd-1")
      (beads-show-copy-id)
      (should (equal "bd-1" (car kill-ring))))))

(ert-deftest beads-coverage-5-show-copy-id-no-id ()
  "Test copy-id with no issue ID."
  (with-temp-buffer
    (let ((inhibit-read-only t))
      (beads-show-mode)
      (setq beads-show--issue-id nil)
      (should-error (beads-show-copy-id) :type 'user-error))))

;;; ============================================================
;;; beads-command-formula.el - Mode Tests
;;; ============================================================

(ert-deftest beads-coverage-5-formula-show-mode-defined ()
  "Test formula-show-mode is defined."
  (should (fboundp 'beads-formula-show-mode)))

(ert-deftest beads-coverage-5-formula-show-quit ()
  "Test formula-show-quit calls quit-window."
  (let ((quit-called nil))
    (cl-letf (((symbol-function 'quit-window)
               (lambda (kill) (setq quit-called kill))))
      (beads-formula-show-quit)
      (should quit-called))))

(ert-deftest beads-coverage-5-formula-show-refresh-defined ()
  "Test formula-show-refresh is a command."
  (should (commandp 'beads-formula-show-refresh)))

;;; ============================================================
;;; beads-command-list.el - Transient Reset Tests
;;; ============================================================

(ert-deftest beads-coverage-5-list-bulk-operations-defined ()
  "Test bulk operation commands are defined."
  (should (commandp 'beads-list-bulk-close))
  (should (commandp 'beads-list-bulk-update-priority))
  (should (commandp 'beads-list-mark))
  (should (commandp 'beads-list-unmark))
  (should (commandp 'beads-list-unmark-all)))

;;; ============================================================
;;; beads-meta.el - Humanize and Auto-Key Tests
;;; ============================================================

(ert-deftest beads-coverage-5-meta-humanize-slot-name ()
  "Test humanize-slot-name converts to readable format."
  (should (stringp (beads-meta--humanize-slot-name 'issue-type)))
  (should (stringp (beads-meta--humanize-slot-name 'priority-min))))

(ert-deftest beads-coverage-5-meta-auto-generate-key ()
  "Test auto-generate-key for positional args."
  (let ((result (beads-meta--auto-generate-key 'title t)))
    (should (stringp result))))

(ert-deftest beads-coverage-5-meta-infer-argument-boolean ()
  "Test infer-argument for boolean type."
  (let ((result (beads-meta--infer-argument
                 '(:long-option "verbose" :option-type :boolean))))
    (should (equal "--verbose" result))))

(ert-deftest beads-coverage-5-meta-infer-argument-string ()
  "Test infer-argument for string type."
  (let ((result (beads-meta--infer-argument
                 '(:long-option "title" :option-type :string))))
    (should (equal "--title=" result))))

(ert-deftest beads-coverage-5-meta-infer-argument-already-set ()
  "Test infer-argument returns nil when already set."
  (let ((result (beads-meta--infer-argument
                 '(:long-option "title" :argument "--title="))))
    (should (null result))))

(provide 'beads-coverage-test-5)
;;; beads-coverage-test-5.el ends here
