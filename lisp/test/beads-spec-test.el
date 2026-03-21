;;; beads-spec-test.el --- Tests for beads-spec.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; ERT tests for beads-spec.el: filter spec objects for beads list views.
;; Tests cover:
;; - beads-issue-spec class definition and slots
;; - beads-issue-spec--to-args conversion
;; - beads-list-default-spec defcustom
;; - beads-list--spec buffer-local variable
;; - beads-list--refresh with mocked command execution

;;; Code:

(require 'ert)
(require 'eieio)
(require 'beads-spec)
(require 'beads-types)
(require 'beads-command-list)

;;; Helpers

(defun beads-spec-test--make-spec (&rest args)
  "Return a beads-issue-spec with ARGS merged into defaults."
  (apply #'beads-issue-spec args))

(defun beads-spec-test--make-issue (&rest args)
  "Return a beads-issue with ARGS merged into defaults."
  (apply #'beads-issue
         (append '(:id "bd-001"
                   :title "Test issue"
                   :status "open"
                   :priority 2
                   :issue-type "task")
                 args)))

(defun beads-spec-test--mock-execute (issues)
  "Return a mock execution result containing ISSUES."
  (let ((exec (beads-command-execution)))
    (oset exec result issues)
    (oset exec exit-code 0)
    (oset exec stdout "")
    (oset exec stderr "")
    exec))

;;; beads-issue-spec Class Tests

(ert-deftest beads-spec-test-class-defined ()
  "Verify beads-issue-spec is defined as an EIEIO class."
  (should (class-p 'beads-issue-spec)))

(ert-deftest beads-spec-test-class-slots-exist ()
  "Verify beads-issue-spec has all required slots."
  (let ((spec (beads-issue-spec)))
    (should (slot-exists-p spec 'status))
    (should (slot-exists-p spec 'type))
    (should (slot-exists-p spec 'priority))
    (should (slot-exists-p spec 'assignee))
    (should (slot-exists-p spec 'label))
    (should (slot-exists-p spec 'order))
    (should (slot-exists-p spec 'limit))
    (should (slot-exists-p spec 'ready-only))))

(ert-deftest beads-spec-test-default-values ()
  "Verify beads-issue-spec has correct default slot values."
  (let ((spec (beads-issue-spec)))
    (should (null (oref spec status)))
    (should (null (oref spec type)))
    (should (null (oref spec priority)))
    (should (null (oref spec assignee)))
    (should (null (oref spec label)))
    (should (eq (oref spec order) 'newest))
    (should (= (oref spec limit) 50))
    (should (null (oref spec ready-only)))))

(ert-deftest beads-spec-test-slot-values-set ()
  "Verify beads-issue-spec slots can be set via initargs."
  (let ((spec (beads-issue-spec
               :status "open"
               :type "bug"
               :priority 1
               :assignee "alice"
               :label "urgent"
               :order 'priority
               :limit 25
               :ready-only t)))
    (should (equal (oref spec status) "open"))
    (should (equal (oref spec type) "bug"))
    (should (= (oref spec priority) 1))
    (should (equal (oref spec assignee) "alice"))
    (should (equal (oref spec label) "urgent"))
    (should (eq (oref spec order) 'priority))
    (should (= (oref spec limit) 25))
    (should (oref spec ready-only))))

;;; beads-issue-spec--to-args Tests

(ert-deftest beads-spec-test-to-args-empty-spec ()
  "Verify to-args with all-nil spec produces only limit arg."
  (let* ((spec (beads-issue-spec :order 'newest :limit 50))
         (args (beads-issue-spec--to-args spec)))
    ;; Should include sort and limit at minimum
    (should (member "--limit=50" args))
    ;; newest order → --sort=created --reverse
    (should (member "--sort=created" args))
    (should (member "--reverse" args))))

(ert-deftest beads-spec-test-to-args-status ()
  "Verify to-args includes --status when set."
  (let* ((spec (beads-issue-spec :status "open"))
         (args (beads-issue-spec--to-args spec)))
    (should (member "--status=open" args))))

(ert-deftest beads-spec-test-to-args-no-status ()
  "Verify to-args omits --status when nil."
  (let* ((spec (beads-issue-spec :status nil))
         (args (beads-issue-spec--to-args spec)))
    (should-not (seq-find (lambda (a) (string-prefix-p "--status=" a)) args))))

(ert-deftest beads-spec-test-to-args-type ()
  "Verify to-args includes --type when set."
  (let* ((spec (beads-issue-spec :type "bug"))
         (args (beads-issue-spec--to-args spec)))
    (should (member "--type=bug" args))))

(ert-deftest beads-spec-test-to-args-priority ()
  "Verify to-args includes --priority when set."
  (let* ((spec (beads-issue-spec :priority 1))
         (args (beads-issue-spec--to-args spec)))
    (should (member "--priority=1" args))))

(ert-deftest beads-spec-test-to-args-assignee ()
  "Verify to-args includes --assignee when set."
  (let* ((spec (beads-issue-spec :assignee "alice"))
         (args (beads-issue-spec--to-args spec)))
    (should (member "--assignee=alice" args))))

(ert-deftest beads-spec-test-to-args-label ()
  "Verify to-args includes --label when set."
  (let* ((spec (beads-issue-spec :label "urgent"))
         (args (beads-issue-spec--to-args spec)))
    (should (member "--label=urgent" args))))

(ert-deftest beads-spec-test-to-args-order-newest ()
  "Verify order=newest maps to --sort=created --reverse."
  (let* ((spec (beads-issue-spec :order 'newest))
         (args (beads-issue-spec--to-args spec)))
    (should (member "--sort=created" args))
    (should (member "--reverse" args))))

(ert-deftest beads-spec-test-to-args-order-oldest ()
  "Verify order=oldest maps to --sort=created without --reverse."
  (let* ((spec (beads-issue-spec :order 'oldest))
         (args (beads-issue-spec--to-args spec)))
    (should (member "--sort=created" args))
    (should-not (member "--reverse" args))))

(ert-deftest beads-spec-test-to-args-order-priority ()
  "Verify order=priority maps to --sort=priority."
  (let* ((spec (beads-issue-spec :order 'priority))
         (args (beads-issue-spec--to-args spec)))
    (should (member "--sort=priority" args))
    (should-not (member "--reverse" args))))

(ert-deftest beads-spec-test-to-args-order-updated ()
  "Verify order=updated maps to --sort=updated."
  (let* ((spec (beads-issue-spec :order 'updated))
         (args (beads-issue-spec--to-args spec)))
    (should (member "--sort=updated" args))))

(ert-deftest beads-spec-test-to-args-limit ()
  "Verify to-args includes --limit with correct value."
  (let* ((spec (beads-issue-spec :limit 25))
         (args (beads-issue-spec--to-args spec)))
    (should (member "--limit=25" args))))

(ert-deftest beads-spec-test-to-args-ready-only ()
  "Verify to-args includes --ready when ready-only is t."
  (let* ((spec (beads-issue-spec :ready-only t))
         (args (beads-issue-spec--to-args spec)))
    (should (member "--ready" args))))

(ert-deftest beads-spec-test-to-args-no-ready-only ()
  "Verify to-args omits --ready when ready-only is nil."
  (let* ((spec (beads-issue-spec :ready-only nil))
         (args (beads-issue-spec--to-args spec)))
    (should-not (member "--ready" args))))

(ert-deftest beads-spec-test-to-args-returns-list ()
  "Verify beads-issue-spec--to-args returns a list of strings."
  (let* ((spec (beads-issue-spec))
         (args (beads-issue-spec--to-args spec)))
    (should (listp args))
    (should (seq-every-p #'stringp args))))

;;; beads-list-default-spec Tests

(ert-deftest beads-spec-test-default-spec-is-defcustom ()
  "Verify beads-list-default-spec is a custom variable."
  (should (custom-variable-p 'beads-list-default-spec)))

(ert-deftest beads-spec-test-default-spec-is-spec ()
  "Verify beads-list-default-spec is a beads-issue-spec instance."
  (should (object-of-class-p beads-list-default-spec 'beads-issue-spec)))

(ert-deftest beads-spec-test-default-spec-status-open ()
  "Verify beads-list-default-spec defaults to status=open."
  (should (equal (oref beads-list-default-spec status) "open")))

(ert-deftest beads-spec-test-default-spec-order-newest ()
  "Verify beads-list-default-spec defaults to order=newest."
  (should (eq (oref beads-list-default-spec order) 'newest)))

(ert-deftest beads-spec-test-default-spec-limit-50 ()
  "Verify beads-list-default-spec defaults to limit=50."
  (should (= (oref beads-list-default-spec limit) 50)))

;;; beads-list--spec Buffer-local Variable Tests

(ert-deftest beads-spec-test-spec-var-is-buffer-local ()
  "Verify beads-list--spec is a buffer-local variable."
  (with-temp-buffer
    (should (local-variable-if-set-p 'beads-list--spec))))

(ert-deftest beads-spec-test-spec-var-nil-default ()
  "Verify beads-list--spec is nil by default."
  (with-temp-buffer
    (should (null beads-list--spec))))

(ert-deftest beads-spec-test-spec-var-independent-per-buffer ()
  "Verify beads-list--spec is independent across buffers."
  (let ((spec (beads-issue-spec :status "closed")))
    (with-temp-buffer
      (setq-local beads-list--spec spec)
      (with-temp-buffer
        (should (null beads-list--spec))))))

;;; beads-list--refresh Tests

(ert-deftest beads-spec-test-refresh-uses-spec ()
  "Verify beads-list--refresh calls beads-command-execute with a command."
  (with-temp-buffer
    (beads-list-mode)
    (let ((called-cmd nil)
          (issues (list (beads-spec-test--make-issue))))
      (cl-letf (((symbol-function 'beads-command-execute)
                 (lambda (cmd)
                   (setq called-cmd cmd)
                   (beads-spec-test--mock-execute issues)))
                ((symbol-function 'beads-list--populate-buffer)
                 (lambda (_issues _command &optional _cmd-obj) nil)))
        (beads-list--refresh (beads-issue-spec :status "open")))
      (should called-cmd)
      (should (object-of-class-p called-cmd 'beads-command-list)))))

(ert-deftest beads-spec-test-refresh-populates-buffer ()
  "Verify beads-list--refresh calls beads-list--populate-buffer with issues."
  (with-temp-buffer
    (beads-list-mode)
    (let ((issues (list (beads-spec-test--make-issue)))
          (populated-issues nil))
      (cl-letf (((symbol-function 'beads-command-execute)
                 (lambda (_cmd)
                   (beads-spec-test--mock-execute issues)))
                ((symbol-function 'beads-list--populate-buffer)
                 (lambda (is _command &optional _cmd-obj)
                   (setq populated-issues is))))
        (beads-list--refresh (beads-issue-spec :status "open")))
      (should (equal populated-issues issues)))))

(ert-deftest beads-spec-test-refresh-stores-spec ()
  "Verify beads-list--refresh sets beads-list--spec in current buffer."
  (with-temp-buffer
    (beads-list-mode)
    (let ((spec (beads-issue-spec :status "closed")))
      (cl-letf (((symbol-function 'beads-command-execute)
                 (lambda (_cmd)
                   (beads-spec-test--mock-execute nil)))
                ((symbol-function 'beads-list--populate-buffer)
                 (lambda (_is _command &optional _cmd-obj) nil)))
        (beads-list--refresh spec))
      (should (eq beads-list--spec spec)))))

(ert-deftest beads-spec-test-refresh-falls-back-to-buffer-spec ()
  "Verify beads-list--refresh uses beads-list--spec when no spec given."
  (with-temp-buffer
    (beads-list-mode)
    (let ((spec (beads-issue-spec :status "blocked"))
          (used-cmd nil))
      (setq-local beads-list--spec spec)
      (cl-letf (((symbol-function 'beads-command-execute)
                 (lambda (cmd)
                   (setq used-cmd cmd)
                   (beads-spec-test--mock-execute nil)))
                ((symbol-function 'beads-list--populate-buffer)
                 (lambda (_is _command &optional _cmd-obj) nil)))
        (beads-list--refresh))
      ;; Used a command (demonstrating buffer spec was used)
      (should used-cmd))))

(ert-deftest beads-spec-test-refresh-falls-back-to-default-spec ()
  "Verify beads-list--refresh uses default spec when no spec anywhere."
  (with-temp-buffer
    (beads-list-mode)
    (let ((used-cmd nil))
      (setq-local beads-list--spec nil)
      (cl-letf (((symbol-function 'beads-command-execute)
                 (lambda (cmd)
                   (setq used-cmd cmd)
                   (beads-spec-test--mock-execute nil)))
                ((symbol-function 'beads-list--populate-buffer)
                 (lambda (_is _command &optional _cmd-obj) nil)))
        (beads-list--refresh))
      (should used-cmd)
      ;; The default spec has status=open, so the command should have status=open
      (should (equal (oref used-cmd status) "open")))))

;;; beads-list-filter-menu Tests

(ert-deftest beads-spec-test-spec-to-args-status ()
  "Verify spec-to-args encodes status as --filter-status=."
  (let* ((spec (beads-issue-spec :status "open"))
         (args (beads-list-filter--spec-to-args spec)))
    (should (member "--filter-status=open" args))))

(ert-deftest beads-spec-test-spec-to-args-no-status ()
  "Verify spec-to-args omits --filter-status= when nil."
  (let* ((spec (beads-issue-spec :status nil))
         (args (beads-list-filter--spec-to-args spec)))
    (should-not (seq-find (lambda (a)
                            (string-prefix-p "--filter-status=" a))
                          args))))

(ert-deftest beads-spec-test-spec-to-args-type ()
  "Verify spec-to-args encodes type."
  (let* ((spec (beads-issue-spec :type "bug"))
         (args (beads-list-filter--spec-to-args spec)))
    (should (member "--filter-type=bug" args))))

(ert-deftest beads-spec-test-spec-to-args-priority ()
  "Verify spec-to-args encodes priority."
  (let* ((spec (beads-issue-spec :priority 2))
         (args (beads-list-filter--spec-to-args spec)))
    (should (member "--filter-priority=2" args))))

(ert-deftest beads-spec-test-spec-to-args-assignee ()
  "Verify spec-to-args encodes assignee."
  (let* ((spec (beads-issue-spec :assignee "alice"))
         (args (beads-list-filter--spec-to-args spec)))
    (should (member "--filter-assignee=alice" args))))

(ert-deftest beads-spec-test-spec-to-args-label ()
  "Verify spec-to-args encodes label."
  (let* ((spec (beads-issue-spec :label "urgent"))
         (args (beads-list-filter--spec-to-args spec)))
    (should (member "--filter-label=urgent" args))))

(ert-deftest beads-spec-test-spec-to-args-order ()
  "Verify spec-to-args encodes non-default order."
  (let* ((spec (beads-issue-spec :order 'priority))
         (args (beads-list-filter--spec-to-args spec)))
    (should (member "--filter-order=priority" args))))

(ert-deftest beads-spec-test-spec-to-args-order-newest-omitted ()
  "Verify spec-to-args omits order when newest (default)."
  (let* ((spec (beads-issue-spec :order 'newest))
         (args (beads-list-filter--spec-to-args spec)))
    (should-not (seq-find (lambda (a)
                            (string-prefix-p "--filter-order=" a))
                          args))))

(ert-deftest beads-spec-test-spec-to-args-limit ()
  "Verify spec-to-args encodes limit."
  (let* ((spec (beads-issue-spec :limit 25))
         (args (beads-list-filter--spec-to-args spec)))
    (should (member "--filter-limit=25" args))))

(ert-deftest beads-spec-test-spec-to-args-ready-only ()
  "Verify spec-to-args encodes ready-only switch."
  (let* ((spec (beads-issue-spec :ready-only t))
         (args (beads-list-filter--spec-to-args spec)))
    (should (member "--filter-ready-only" args))))

(ert-deftest beads-spec-test-spec-to-args-no-ready-only ()
  "Verify spec-to-args omits ready-only when nil."
  (let* ((spec (beads-issue-spec :ready-only nil))
         (args (beads-list-filter--spec-to-args spec)))
    (should-not (member "--filter-ready-only" args))))

(ert-deftest beads-spec-test-spec-to-args-returns-strings ()
  "Verify spec-to-args returns a list of strings."
  (let* ((spec (beads-issue-spec :status "open" :type "bug" :limit 10))
         (args (beads-list-filter--spec-to-args spec)))
    (should (listp args))
    (should (seq-every-p #'stringp args))))

(ert-deftest beads-spec-test-args-to-spec-defaults ()
  "Verify args-to-spec returns spec with defaults from empty args."
  (let ((spec (beads-list-filter--args-to-spec '())))
    (should (null (oref spec status)))
    (should (null (oref spec type)))
    (should (null (oref spec priority)))
    (should (null (oref spec assignee)))
    (should (null (oref spec label)))
    (should (eq (oref spec order) 'newest))
    (should (= (oref spec limit) 50))
    (should (null (oref spec ready-only)))))

(ert-deftest beads-spec-test-args-to-spec-status ()
  "Verify args-to-spec parses --filter-status=."
  (let ((spec (beads-list-filter--args-to-spec '("--filter-status=closed"))))
    (should (equal (oref spec status) "closed"))))

(ert-deftest beads-spec-test-args-to-spec-type ()
  "Verify args-to-spec parses --filter-type=."
  (let ((spec (beads-list-filter--args-to-spec '("--filter-type=feature"))))
    (should (equal (oref spec type) "feature"))))

(ert-deftest beads-spec-test-args-to-spec-priority ()
  "Verify args-to-spec parses --filter-priority=."
  (let ((spec (beads-list-filter--args-to-spec '("--filter-priority=1"))))
    (should (= (oref spec priority) 1))))

(ert-deftest beads-spec-test-args-to-spec-assignee ()
  "Verify args-to-spec parses --filter-assignee=."
  (let ((spec (beads-list-filter--args-to-spec '("--filter-assignee=bob"))))
    (should (equal (oref spec assignee) "bob"))))

(ert-deftest beads-spec-test-args-to-spec-label ()
  "Verify args-to-spec parses --filter-label=."
  (let ((spec (beads-list-filter--args-to-spec '("--filter-label=bug"))))
    (should (equal (oref spec label) "bug"))))

(ert-deftest beads-spec-test-args-to-spec-order ()
  "Verify args-to-spec parses --filter-order= as symbol."
  (let ((spec (beads-list-filter--args-to-spec '("--filter-order=updated"))))
    (should (eq (oref spec order) 'updated))))

(ert-deftest beads-spec-test-args-to-spec-limit ()
  "Verify args-to-spec parses --filter-limit=."
  (let ((spec (beads-list-filter--args-to-spec '("--filter-limit=20"))))
    (should (= (oref spec limit) 20))))

(ert-deftest beads-spec-test-args-to-spec-ready-only ()
  "Verify args-to-spec parses --filter-ready-only switch."
  (let ((spec (beads-list-filter--args-to-spec '("--filter-ready-only"))))
    (should (oref spec ready-only))))

(ert-deftest beads-spec-test-round-trip-full-spec ()
  "Verify spec → args → spec round-trip preserves all fields."
  (let* ((orig (beads-issue-spec
                :status "closed"
                :type "feature"
                :priority 1
                :assignee "carol"
                :label "polish"
                :order 'priority
                :limit 30
                :ready-only t))
         (args (beads-list-filter--spec-to-args orig))
         (spec (beads-list-filter--args-to-spec args)))
    (should (equal (oref spec status) "closed"))
    (should (equal (oref spec type) "feature"))
    (should (= (oref spec priority) 1))
    (should (equal (oref spec assignee) "carol"))
    (should (equal (oref spec label) "polish"))
    (should (eq (oref spec order) 'priority))
    (should (= (oref spec limit) 30))
    (should (oref spec ready-only))))

(ert-deftest beads-spec-test-filter-menu-is-defined ()
  "Verify beads-list-filter-menu is defined as a command."
  (should (commandp 'beads-list-filter-menu)))

(ert-deftest beads-spec-test-filter-apply-calls-refresh ()
  "Verify beads-list-filter--apply-with-spec calls beads-list--refresh."
  (with-temp-buffer
    (beads-list-mode)
    (let ((refreshed-spec nil))
      (cl-letf (((symbol-function 'beads-list--refresh)
                 (lambda (spec) (setq refreshed-spec spec)))
                ((symbol-function 'beads-list--populate-buffer)
                 (lambda (_issues _command &optional _cmd-obj) nil)))
        (let ((spec (beads-issue-spec :status "closed")))
          (beads-list-filter--apply-with-spec (current-buffer) spec)))
      (should refreshed-spec)
      (should (equal (oref refreshed-spec status) "closed")))))

(ert-deftest beads-spec-test-filter-reset-calls-refresh-with-default ()
  "Verify beads-list-filter--reset-buffer calls refresh with default spec."
  (with-temp-buffer
    (beads-list-mode)
    (let ((refreshed-spec nil))
      (cl-letf (((symbol-function 'beads-list--refresh)
                 (lambda (spec) (setq refreshed-spec spec)))
                ((symbol-function 'beads-list--populate-buffer)
                 (lambda (_issues _command &optional _cmd-obj) nil)))
        (beads-list-filter--reset-buffer (current-buffer)))
      (should refreshed-spec)
      (should (object-of-class-p refreshed-spec 'beads-issue-spec)))))

;;; beads--transient-args-to-plist Tests

(ert-deftest beads-spec-test-args-to-plist-empty ()
  "Verify empty args returns empty plist."
  (should (null (beads--transient-args-to-plist '()))))

(ert-deftest beads-spec-test-args-to-plist-nil ()
  "Verify nil args returns empty plist."
  (should (null (beads--transient-args-to-plist nil))))

(ert-deftest beads-spec-test-args-to-plist-single-option ()
  "Verify single --key=value arg converts to plist."
  (let ((plist (beads--transient-args-to-plist '("--status=open"))))
    (should (equal (plist-get plist :status) "open"))))

(ert-deftest beads-spec-test-args-to-plist-multiple-options ()
  "Verify multiple --key=value args convert to plist."
  (let ((plist (beads--transient-args-to-plist
                '("--status=open" "--type=bug" "--priority=1"))))
    (should (equal (plist-get plist :status) "open"))
    (should (equal (plist-get plist :type) "bug"))
    (should (equal (plist-get plist :priority) "1"))))

(ert-deftest beads-spec-test-args-to-plist-switch ()
  "Verify switch (no =) converts to t."
  (let ((plist (beads--transient-args-to-plist '("--reverse"))))
    (should (eq (plist-get plist :reverse) t))))

(ert-deftest beads-spec-test-args-to-plist-mixed ()
  "Verify mix of options and switches."
  (let ((plist (beads--transient-args-to-plist
                '("--status=open" "--reverse" "--type=bug"))))
    (should (equal (plist-get plist :status) "open"))
    (should (eq (plist-get plist :reverse) t))
    (should (equal (plist-get plist :type) "bug"))))

(ert-deftest beads-spec-test-args-to-plist-hyphenated-key ()
  "Verify hyphenated keys convert to hyphenated keywords."
  (let ((plist (beads--transient-args-to-plist
                '("--created-after=2025-01-01"))))
    (should (equal (plist-get plist :created-after) "2025-01-01"))))

(ert-deftest beads-spec-test-args-to-plist-empty-value ()
  "Verify --key= with empty value converts to empty string."
  (let ((plist (beads--transient-args-to-plist '("--status="))))
    (should (equal (plist-get plist :status) ""))))

(ert-deftest beads-spec-test-args-to-plist-returns-list ()
  "Verify return value is a proper plist."
  (let ((plist (beads--transient-args-to-plist
                '("--status=open" "--type=bug"))))
    (should (listp plist))
    (should (= (length plist) 4))))

;;; beads--transient-args-to-spec Tests

(ert-deftest beads-spec-test-transient-args-to-spec-empty ()
  "Verify empty args returns spec with defaults."
  (let ((spec (beads--transient-args-to-spec '())))
    (should (beads-issue-spec-p spec))
    (should (null (oref spec status)))
    (should (null (oref spec type)))
    (should (eq (oref spec order) 'newest))
    (should (= (oref spec limit) 50))))

(ert-deftest beads-spec-test-transient-args-to-spec-status ()
  "Verify --status= maps to spec status."
  (let ((spec (beads--transient-args-to-spec '("--status=open"))))
    (should (equal (oref spec status) "open"))))

(ert-deftest beads-spec-test-transient-args-to-spec-type ()
  "Verify --type= maps to spec type."
  (let ((spec (beads--transient-args-to-spec '("--type=bug"))))
    (should (equal (oref spec type) "bug"))))

(ert-deftest beads-spec-test-transient-args-to-spec-priority ()
  "Verify --priority= maps to spec priority as integer."
  (let ((spec (beads--transient-args-to-spec '("--priority=1"))))
    (should (= (oref spec priority) 1))))

(ert-deftest beads-spec-test-transient-args-to-spec-assignee ()
  "Verify --assignee= maps to spec assignee."
  (let ((spec (beads--transient-args-to-spec '("--assignee=alice"))))
    (should (equal (oref spec assignee) "alice"))))

(ert-deftest beads-spec-test-transient-args-to-spec-label ()
  "Verify --label= maps to spec label."
  (let ((spec (beads--transient-args-to-spec '("--label=urgent"))))
    (should (equal (oref spec label) "urgent"))))

(ert-deftest beads-spec-test-transient-args-to-spec-order ()
  "Verify --order= maps to spec order as symbol."
  (let ((spec (beads--transient-args-to-spec '("--order=priority"))))
    (should (eq (oref spec order) 'priority))))

(ert-deftest beads-spec-test-transient-args-to-spec-limit ()
  "Verify --limit= maps to spec limit as integer."
  (let ((spec (beads--transient-args-to-spec '("--limit=25"))))
    (should (= (oref spec limit) 25))))

(ert-deftest beads-spec-test-transient-args-to-spec-ready ()
  "Verify --ready switch maps to spec ready-only."
  (let ((spec (beads--transient-args-to-spec '("--ready"))))
    (should (oref spec ready-only))))

(ert-deftest beads-spec-test-transient-args-to-spec-full ()
  "Verify full set of args maps correctly."
  (let ((spec (beads--transient-args-to-spec
               '("--status=closed" "--type=feature" "--priority=0"
                 "--assignee=bob" "--label=v2" "--order=updated"
                 "--limit=10" "--ready"))))
    (should (equal (oref spec status) "closed"))
    (should (equal (oref spec type) "feature"))
    (should (= (oref spec priority) 0))
    (should (equal (oref spec assignee) "bob"))
    (should (equal (oref spec label) "v2"))
    (should (eq (oref spec order) 'updated))
    (should (= (oref spec limit) 10))
    (should (oref spec ready-only))))

(provide 'beads-spec-test)
;;; beads-spec-test.el ends here
