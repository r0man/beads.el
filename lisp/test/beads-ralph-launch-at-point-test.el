;;; beads-ralph-launch-at-point-test.el --- Tests for launch-at-point + transient wiring -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; ERT coverage for bde-deqx.7:
;;
;;   - `beads-detect-issue-id' detection clauses (list mode, show
;;     mode, Ralph epic browser mode, buffer-name parse, no context).
;;   - `beads-ralph-launch-at-point' kind resolution (epic vs issue).
;;   - `beads-ralph-launch-at-point' error paths (empty id, missing
;;     issue).
;;   - Top-level `beads' transient suffix presence smoke test for the
;;     two new entries (`R' → cockpit, `M-e' → epic browser).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'transient)

(require 'beads)
(require 'beads-agent)
(require 'beads-ralph-launcher)
(require 'beads-types)

;;; Detector — `beads-detect-issue-id'

(ert-deftest beads-ralph-launch-at-point-test-detect-from-list-mode ()
  "List-mode context returns the row's id."
  (with-temp-buffer
    (cl-letf (((symbol-function 'derived-mode-p)
               (lambda (mode) (eq mode 'beads-list-mode)))
              ((symbol-function 'beads-list--current-issue-id)
               (lambda () "bd-42")))
      (should (equal (beads-detect-issue-id) "bd-42")))))

(ert-deftest beads-ralph-launch-at-point-test-detect-from-show-mode ()
  "Show-mode context returns the buffer-local id."
  (with-temp-buffer
    (let ((beads-show--issue-id "bd-7"))
      (cl-letf (((symbol-function 'derived-mode-p)
                 (lambda (mode) (eq mode 'beads-show-mode))))
        (should (equal (beads-detect-issue-id) "bd-7"))))))

(ert-deftest beads-ralph-launch-at-point-test-detect-from-epic-browser ()
  "Ralph epic browser context returns the epic id at point."
  (with-temp-buffer
    (cl-letf (((symbol-function 'derived-mode-p)
               (lambda (mode)
                 (eq mode 'beads-ralph-epic-browser-mode)))
              ((symbol-function
                'beads-ralph-epic-browser--epic-id-at-point)
               (lambda () "bde-99")))
      (should (equal (beads-detect-issue-id) "bde-99")))))

(ert-deftest beads-ralph-launch-at-point-test-detect-from-buffer-name ()
  "Falls back to parsing the `*beads-show[PROJ]/ID*' buffer name."
  (with-temp-buffer
    (rename-buffer
     (generate-new-buffer-name "*beads-show[proj]/bd-456*"))
    (cl-letf (((symbol-function 'derived-mode-p)
               (lambda (_mode) nil)))
      (should (equal (beads-detect-issue-id) "bd-456")))))

(ert-deftest beads-ralph-launch-at-point-test-detect-no-context ()
  "Returns nil when no detection clause matches."
  (with-temp-buffer
    (rename-buffer
     (generate-new-buffer-name "*not-a-beads-buffer*"))
    (cl-letf (((symbol-function 'derived-mode-p)
               (lambda (_mode) nil)))
      (should-not (beads-detect-issue-id)))))

(ert-deftest beads-ralph-launch-at-point-test-detect-epic-browser-guarded ()
  "When the epic-browser symbol is unbound, the detector skips its
clause cleanly instead of raising `void-function'."
  (let* ((sym 'beads-ralph-epic-browser--epic-id-at-point)
         (had-defn (fboundp sym))
         (orig-fn (and had-defn (symbol-function sym))))
    (unwind-protect
        (progn
          (when had-defn (fmakunbound sym))
          (with-temp-buffer
            (cl-letf (((symbol-function 'derived-mode-p)
                       (lambda (_mode) nil)))
              ;; No mode matches and the epic-browser fn is unbound;
              ;; the detector must return nil rather than crash.
              (should-not (beads-detect-issue-id)))))
      (when had-defn
        (fset sym orig-fn)))))

;;; Reader — `beads-read-issue-id'

(ert-deftest beads-ralph-launch-at-point-test-read-prefers-context ()
  "When detection succeeds, no prompt is issued."
  (cl-letf (((symbol-function 'beads-detect-issue-id)
             (lambda () "bd-from-ctx"))
            ((symbol-function 'beads-completion-read-issue)
             (lambda (&rest _) (error "should not be called"))))
    (should (equal (beads-read-issue-id) "bd-from-ctx"))))

(ert-deftest beads-ralph-launch-at-point-test-read-falls-back-to-prompt ()
  "When detection returns nil, the completion prompt fires."
  (cl-letf (((symbol-function 'beads-detect-issue-id)
             (lambda () nil))
            ((symbol-function 'beads-completion-read-issue)
             (lambda (&rest _) "bd-from-prompt")))
    (should (equal (beads-read-issue-id) "bd-from-prompt"))))

;;; Private aliases preserved

(ert-deftest beads-ralph-launch-at-point-test-private-aliases ()
  "Legacy private callers keep working via `defalias'."
  (should (eq (symbol-function 'beads-agent--detect-issue-id)
              'beads-detect-issue-id))
  (should (eq (symbol-function 'beads-agent--read-issue-id)
              'beads-read-issue-id)))

;;; Kind resolution

(defun beads-ralph-launch-at-point-test--fake-issue (type)
  "Build a `beads-issue' carrying TYPE as `issue-type'."
  (make-instance 'beads-issue :id "test-1" :issue-type type))

(ert-deftest beads-ralph-launch-at-point-test-kind-epic ()
  "issue-type \"epic\" routes to `:kind 'epic'."
  (let (captured)
    (cl-letf (((symbol-function 'beads-read-issue-id)
               (lambda () "bde-1"))
              ((symbol-function 'beads-issue-read)
               (lambda (_id)
                 (beads-ralph-launch-at-point-test--fake-issue "epic")))
              ((symbol-function 'beads-ralph-launcher)
               (lambda (id &rest args)
                 (setq captured (list :id id :args args))
                 nil)))
      (beads-ralph-launch-at-point))
    (should (equal "bde-1" (plist-get captured :id)))
    (should (eq 'epic (plist-get (plist-get captured :args) :kind)))))

(ert-deftest beads-ralph-launch-at-point-test-kind-issue ()
  "Non-epic `issue-type' routes to `:kind 'issue'."
  (let (captured)
    (cl-letf (((symbol-function 'beads-read-issue-id)
               (lambda () "bd-1"))
              ((symbol-function 'beads-issue-read)
               (lambda (_id)
                 (beads-ralph-launch-at-point-test--fake-issue "task")))
              ((symbol-function 'beads-ralph-launcher)
               (lambda (id &rest args)
                 (setq captured (list :id id :args args))
                 nil)))
      (beads-ralph-launch-at-point))
    (should (eq 'issue (plist-get (plist-get captured :args) :kind)))))

(ert-deftest beads-ralph-launch-at-point-test-kind-nil-type ()
  "A nil `issue-type' defaults to `:kind 'issue', no crash."
  (let (captured)
    (cl-letf (((symbol-function 'beads-read-issue-id)
               (lambda () "bd-1"))
              ((symbol-function 'beads-issue-read)
               (lambda (_id)
                 (beads-ralph-launch-at-point-test--fake-issue nil)))
              ((symbol-function 'beads-ralph-launcher)
               (lambda (id &rest args)
                 (setq captured (list :id id :args args))
                 nil)))
      (beads-ralph-launch-at-point))
    (should (eq 'issue (plist-get (plist-get captured :args) :kind)))))

(ert-deftest beads-ralph-launch-at-point-test-forwards-issue ()
  "The fetched `beads-issue' is forwarded via `:issue' to the launcher."
  (let* ((issue (beads-ralph-launch-at-point-test--fake-issue "task"))
         captured)
    (cl-letf (((symbol-function 'beads-read-issue-id)
               (lambda () "bd-1"))
              ((symbol-function 'beads-issue-read)
               (lambda (_id) issue))
              ((symbol-function 'beads-ralph-launcher)
               (lambda (id &rest args)
                 (setq captured (list :id id :args args))
                 nil)))
      (beads-ralph-launch-at-point))
    (should (eq issue (plist-get (plist-get captured :args) :issue)))))

;;; Error paths

(ert-deftest beads-ralph-launch-at-point-test-rejects-empty-id ()
  "Empty id from the reader surfaces as `user-error'."
  (cl-letf (((symbol-function 'beads-read-issue-id)
             (lambda () ""))
            ((symbol-function 'beads-issue-read)
             (lambda (_id) (error "should not be called")))
            ((symbol-function 'beads-ralph-launcher)
             (lambda (&rest _) (error "should not be called"))))
    (should-error (beads-ralph-launch-at-point) :type 'user-error)))

(ert-deftest beads-ralph-launch-at-point-test-rejects-nil-id ()
  "Nil id from the reader surfaces as `user-error'."
  (cl-letf (((symbol-function 'beads-read-issue-id)
             (lambda () nil))
            ((symbol-function 'beads-issue-read)
             (lambda (_id) (error "should not be called")))
            ((symbol-function 'beads-ralph-launcher)
             (lambda (&rest _) (error "should not be called"))))
    (should-error (beads-ralph-launch-at-point) :type 'user-error)))

(ert-deftest beads-ralph-launch-at-point-test-rejects-missing-issue ()
  "`beads-issue-read' returning nil surfaces a clear `user-error'."
  (cl-letf (((symbol-function 'beads-read-issue-id)
             (lambda () "ghost-1"))
            ((symbol-function 'beads-issue-read)
             (lambda (_id) nil))
            ((symbol-function 'beads-ralph-launcher)
             (lambda (&rest _) (error "should not be called"))))
    (should-error (beads-ralph-launch-at-point) :type 'user-error)))

;;; Transient suffix wiring

(ert-deftest beads-ralph-launch-at-point-test-transient-has-cockpit ()
  "The top-level `beads' transient binds `R' to `beads-ralph-cockpit'."
  ;; `beads.el' wires the suffix via `with-eval-after-load 'transient',
  ;; which has already fired by the time tests load.  This is a smoke
  ;; check that the binding survived the wiring.  Old-style suffix
  ;; spec is returned as a list whose car is the class symbol and
  ;; whose tail is a plist of slot/value pairs.
  (let ((suffix (ignore-errors
                  (transient-get-suffix 'beads "R"))))
    (should suffix)
    (should (eq 'beads-ralph-cockpit
                (plist-get (cdr suffix) :command)))))

(ert-deftest beads-ralph-launch-at-point-test-transient-has-epic-browser ()
  "The top-level `beads' transient binds `M-e' to `beads-ralph-epic-browser'."
  (let ((suffix (ignore-errors
                  (transient-get-suffix 'beads "M-e"))))
    (should suffix)
    (should (eq 'beads-ralph-epic-browser
                (plist-get (cdr suffix) :command)))))

;;; Buffer-local L bindings

(ert-deftest beads-ralph-launch-at-point-test-list-mode-binds-L ()
  "`beads-list-mode-map' binds `L' to `beads-ralph-launch-at-point'."
  (require 'beads-command-list)
  (should (eq 'beads-ralph-launch-at-point
              (lookup-key beads-list-mode-map (kbd "L")))))

(ert-deftest beads-ralph-launch-at-point-test-show-mode-binds-L ()
  "`beads-show-mode-map' binds `L' to `beads-ralph-launch-at-point'."
  (require 'beads-command-show)
  (should (eq 'beads-ralph-launch-at-point
              (lookup-key beads-show-mode-map (kbd "L")))))

(ert-deftest beads-ralph-launch-at-point-test-epic-browser-binds-L ()
  "`beads-ralph-epic-browser-mode-map' binds `L'."
  (require 'beads-ralph-epic-browser)
  (should (eq 'beads-ralph-launch-at-point
              (lookup-key beads-ralph-epic-browser-mode-map (kbd "L")))))

(provide 'beads-ralph-launch-at-point-test)

;;; beads-ralph-launch-at-point-test.el ends here
