;;; beads-swarm-test.el --- Tests for beads-swarm -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; Unit and integration tests for the swarm porcelain (WI-SF-16):
;; the fleet list, the status board and the worker lanes.  Unit tests
;; mock `beads-command-execute-async' and `beads-command-execute'; the
;; integration test drives the real `bd swarm' commands against a
;; temporary embedded-Dolt repo.

;;; Code:

(require 'ert)
(require 'cl-lib)

(require 'beads-swarm)
(require 'beads-command-swarm)
(require 'beads-command-create)
(require 'beads-command-list)
(require 'beads-types)
(require 'beads-integration-test)

;;; Fixtures

(defconst beads-swarm-test--list-result
  '((schema_version . 1)
    (swarms . [((id . "sw-1") (title . "Swarm: Epic One")
                (epic_id . "ep-1") (epic_title . "Epic One")
                (coordinator . "alice") (status . "open")
                (progress_percent . 33) (total_issues . 3)
                (completed_issues . 1) (active_issues . 1))
               ((id . "sw-2") (title . "Swarm: Epic Two")
                (epic_id . "ep-2") (epic_title . "Epic Two")
                (coordinator . "") (status . "closed")
                (progress_percent . 100) (total_issues . 1)
                (completed_issues . 1) (active_issues . 0))]))
  "Fixture for a `bd swarm list --json' result.")

(defconst beads-swarm-test--status-result
  '((schema_version . 1)
    (epic_id . "ep-1") (epic_title . "Epic One")
    (progress_percent . 33) (total_issues . 3)
    (active_count . 1) (ready_count . 1) (blocked_count . 1)
    (completed . [((id . "ep-1.0") (title . "Done task"))])
    (active . [((id . "ep-1.1") (title . "Active task") (assignee . "alice"))])
    (ready . [((id . "ep-1.2") (title . "Ready task"))])
    (blocked . [((id . "ep-1.3") (title . "Blocked task")
                 (blocked_by . ["ep-1.2"]))]))
  "Fixture for a `bd swarm status --json' result.")

(defconst beads-swarm-test--validate-result
  '((schema_version . 1) (swarmable . t)
    (max_parallelism . 2) (estimated_sessions . 3)
    (ready_fronts . [((wave . 0) (issues . ["ep-1.1"]) (titles . ["A"]))]))
  "Fixture for a `bd swarm validate --json' result.")

(defun beads-swarm-test--mock-async (list-result status-result
                                                 validate-result worker-result)
  "Return a `beads-command-execute-async' replacement.
LIST-RESULT, STATUS-RESULT and VALIDATE-RESULT are returned for the
matching swarm classes; WORKER-RESULT is returned for
`beads-command-list' (the worker lanes).  Every callback fires
synchronously so the views render immediately."
  (lambda (cmd on-success &optional on-error &rest _kwargs)
    (ignore on-error)
    (pcase (eieio-object-class cmd)
      ('beads-command-swarm-list (funcall on-success list-result))
      ('beads-command-swarm-status (funcall on-success status-result))
      ('beads-command-swarm-validate (funcall on-success validate-result))
      ('beads-command-list (funcall on-success worker-result))
      (_ (funcall on-success nil)))))

(defmacro beads-swarm-test--with-buffer (mode &rest body)
  "Create a temporary buffer in MODE and evaluate BODY there.
The buffer is killed afterwards."
  (declare (indent 1) (debug (form body)))
  `(let ((buf (generate-new-buffer " *beads-swarm-test*")))
     (unwind-protect
         (with-current-buffer buf
           (funcall ,mode)
           ,@body)
       (kill-buffer buf))))

;;; Pure data tests

(ert-deftest beads-swarm-test-field-alist-symbol-and-string ()
  "Test reading symbol and string alist keys."
  :tags '(:unit)
  (let ((alist '((epic_id . "ep-1") ("title" . "T"))))
    (should (equal "ep-1" (beads-swarm--field alist 'epic_id)))
    (should (equal "T" (beads-swarm--field alist 'title)))
    (should (null (beads-swarm--field alist 'missing)))
    (should (equal "d" (beads-swarm--field alist 'missing "d")))))

(ert-deftest beads-swarm-test-field-plist ()
  "Test reading a plist."
  :tags '(:unit)
  (should (equal "v" (beads-swarm--field (list :id "v" :x 1) :id))))

(ert-deftest beads-swarm-test-field-eieio-object ()
  "Test reading an EIEIO slot, the future typed-result shape."
  :tags '(:unit)
  (let ((issue (beads-issue :id "ep-1.1" :title "Active")))
    (should (equal "ep-1.1" (beads-swarm--field issue 'id)))
    (should (equal "Active" (beads-swarm--field issue 'title)))))

(ert-deftest beads-swarm-test-domain-error-p ()
  "Test domain-error detection."
  :tags '(:unit)
  (should (equal "swarm already exists"
                 (beads-swarm-domain-error-p
                  '((error . "swarm already exists")
                    (existing_id . "sw-1")))))
  (should (null (beads-swarm-domain-error-p '((swarmable . t)))))
  (should (null (beads-swarm-domain-error-p nil))))

(ert-deftest beads-swarm-test-list-items ()
  "Test extracting swarm items and tolerating an error payload."
  :tags '(:unit)
  (let ((items (beads-swarm--list-items beads-swarm-test--list-result)))
    (should (= 2 (length items)))
    (should (equal "sw-1" (beads-swarm--field (car items) 'id))))
  (should (null (beads-swarm--list-items '((error . "nope"))))))

(ert-deftest beads-swarm-test-display-normalizes ()
  "Test that `beads-swarm-display' normalizes JSON keys."
  :tags '(:unit)
  (let* ((items (beads-swarm--list-items beads-swarm-test--list-result))
         (display (beads-swarm-display (car items))))
    (should (equal "sw-1" (plist-get display :id)))
    (should (equal "ep-1" (plist-get display :epic-id)))
    (should (equal "alice" (plist-get display :coordinator)))
    (should (= 3 (plist-get display :total-issues)))))

(ert-deftest beads-swarm-test-lane-state ()
  "Test worker lane classification."
  :tags '(:unit)
  (should (eq 'active
              (beads-swarm--lane-state '(:ids ("a")) '("a"))))
  (should (eq 'idle-slot
              (beads-swarm--lane-state '(:ids ("x")) '("a"))))
  (should (eq 'over-committed
              (beads-swarm--lane-state '(:ids ("a" "b")) '("a" "b"))))
  (should (eq 'idle
              (beads-swarm--lane-state '(:ids nil) '("a")))))

(ert-deftest beads-swarm-test-headroom ()
  "Test headroom arithmetic and saturation."
  :tags '(:unit)
  (should (equal '(2 . nil) (beads-swarm--headroom 3 1)))
  (should (equal '(0 . t) (beads-swarm--headroom 2 2)))
  (should (equal '(0 . t) (beads-swarm--headroom 1 3)))
  (should (equal '(1 . nil) (beads-swarm--headroom nil -1))))

(ert-deftest beads-swarm-test-worker-plist ()
  "Test building a lane plist from parsed issues."
  :tags '(:unit)
  (let ((worker (beads-swarm--worker-plist
                 "alice"
                 (vector (beads-issue :id "ep-1.1")
                         (beads-issue :id "ep-1.2")))))
    (should (equal "alice" (plist-get worker :assignee)))
    (should (= 2 (plist-get worker :active)))
    (should (equal '("ep-1.1" "ep-1.2") (plist-get worker :ids)))))

(ert-deftest beads-swarm-test-progress-bar ()
  "Test progress-bar width and composition."
  :tags '(:unit)
  (let ((bar (substring-no-properties (beads-swarm--progress-bar 50 10))))
    (should (= 10 (length bar)))
    (should (string-prefix-p "█████" bar))))

;;; Fleet list tests

(ert-deftest beads-swarm-test-list-visible-active-vs-all ()
  "Test the active/all toggle and text filter."
  :tags '(:unit)
  (let* ((items (mapcar #'beads-swarm-display
                        (beads-swarm--list-items
                         beads-swarm-test--list-result)))
         (beads-swarm-list--items items)
         (beads-swarm-list--all nil)
         (beads-swarm-list--filter nil))
    (should (= 1 (length (beads-swarm-list--visible items))))
    (setq beads-swarm-list--all t)
    (should (= 2 (length (beads-swarm-list--visible items))))
    (setq beads-swarm-list--filter "Two")
    (should (= 1 (length (beads-swarm-list--visible items))))
    (setq beads-swarm-list--filter "matches-nothing")
    (should (= 0 (length (beads-swarm-list--visible items))))))

(ert-deftest beads-swarm-test-list-load-async ()
  "Test that the fleet list loads and renders asynchronously."
  :tags '(:unit)
  (beads-swarm-test--with-buffer #'beads-swarm-list-mode
    (cl-letf (((symbol-function 'beads-command-execute-async)
               (beads-swarm-test--mock-async
                beads-swarm-test--list-result nil nil nil)))
      (beads-swarm-list--load (current-buffer)))
    (should (= 2 (length beads-swarm-list--items)))
    (should (equal "sw-1" (plist-get (car beads-swarm-list--items) :id)))
    (should (null beads-swarm-list--error))
    (should (string-match-p "Epic One"
                            (buffer-substring-no-properties
                             (point-min) (point-max))))
    ;; Active-only by default: the closed swarm is not rendered.
    (should-not (string-match-p "Epic Two"
                                (buffer-substring-no-properties
                                 (point-min) (point-max))))))

(ert-deftest beads-swarm-test-list-error ()
  "Test that a domain error leaves the fleet list empty with a message."
  :tags '(:unit)
  (beads-swarm-test--with-buffer #'beads-swarm-list-mode
    (cl-letf (((symbol-function 'beads-command-execute-async)
               (beads-swarm-test--mock-async
                '((error . "swarm list unavailable")) nil nil nil)))
      (beads-swarm-list--load (current-buffer)))
    (should (null beads-swarm-list--items))
    (should (equal "swarm list unavailable" beads-swarm-list--error))))

;;; Status board tests

(ert-deftest beads-swarm-test-status-render-groups ()
  "Test that the status board renders every group and its annotations."
  :tags '(:unit)
  (beads-swarm-test--with-buffer #'beads-swarm-status-mode
    (setq beads-swarm--id "ep-1"
          beads-swarm--status beads-swarm-test--status-result)
    (beads-swarm--render)
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (should (string-match-p "Completed" text))
      (should (string-match-p "Active" text))
      (should (string-match-p "Ready" text))
      (should (string-match-p "Blocked" text))
      (should (string-match-p "ep-1.1" text))
      (should (string-match-p "\\[alice\\]" text))
      (should (string-match-p "blocked by ep-1.2" text)))))

(ert-deftest beads-swarm-test-status-load-async ()
  "Test the async status load with validate and lanes."
  :tags '(:unit)
  (beads-swarm-test--with-buffer #'beads-swarm-status-mode
    (setq beads-swarm--id "ep-1"
          beads-swarm--show-lanes t)
    (cl-letf (((symbol-function 'beads-command-execute-async)
               (beads-swarm-test--mock-async
                nil beads-swarm-test--status-result
                beads-swarm-test--validate-result
                (vector (beads-issue :id "ep-1.1")))))
      (beads-swarm-status--load (current-buffer)))
    (should (equal "ep-1" beads-swarm--epic-id))
    (should (= 2 (beads-swarm--field beads-swarm--validate 'max_parallelism)))
    (should (gethash "alice" beads-swarm--lanes))
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (should (string-match-p "alice" text))
      (should (string-match-p "headroom: 1" text)))))

(ert-deftest beads-swarm-test-status-lane-saturated ()
  "Test that a saturated lane renders its marker and zero headroom."
  :tags '(:unit)
  (beads-swarm-test--with-buffer #'beads-swarm-status-mode
    (setq beads-swarm--id "ep-1"
          beads-swarm--status
          '((epic_id . "ep-1") (epic_title . "Epic")
            (active_count . 2) (progress_percent . 0) (total_issues . 2)
            (active . [((id . "ep-1.1") (title . "A") (assignee . "alice"))
                       ((id . "ep-1.2") (title . "B") (assignee . "bob"))]))
          beads-swarm--validate '((max_parallelism . 2))
          beads-swarm--lanes (let ((h (make-hash-table :test #'equal)))
                               (puthash "alice" '(:ids ("ep-1.1")) h)
                               (puthash "bob" '(:ids ("ep-1.2")) h)
                               h)
          beads-swarm--show-lanes t)
    (beads-swarm--render)
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (should (string-match-p "saturated" text))
      (should (string-match-p "headroom: 0" text)))))

(ert-deftest beads-swarm-test-status-window-clips ()
  "Test that a large group clips to the window and offers to widen."
  :tags '(:unit)
  (beads-swarm-test--with-buffer #'beads-swarm-status-mode
    (let ((items (mapcar (lambda (i) (list :id (format "ep-1.%d" i)
                                           :title "x"))
                         (number-sequence 1 10))))
      (setq beads-swarm--id "ep-1"
            beads-swarm--status (list (cons 'ready (vconcat items))
                                      (cons 'total_issues 10)
                                      (cons 'progress_percent 0))
            beads-swarm--window 3)
      (beads-swarm--render)
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "7 more" text)))
      (let ((beads-swarm-window-step 4))
        (beads-swarm-status-widen))
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "3 more" text))
        (should-not (string-match-p "7 more" text)))))
  )

(ert-deftest beads-swarm-test-status-domain-error ()
  "Test that a status domain error renders the error line."
  :tags '(:unit)
  (beads-swarm-test--with-buffer #'beads-swarm-status-mode
    (setq beads-swarm--id "ep-1")
    (cl-letf (((symbol-function 'beads-command-execute-async)
               (beads-swarm-test--mock-async
                nil '((error . "no swarm")) nil nil)))
      (beads-swarm-status--load (current-buffer)))
    (should (equal "no swarm" beads-swarm--status-error))
    (should (string-match-p "no swarm"
                            (buffer-substring-no-properties
                             (point-min) (point-max))))))

(ert-deftest beads-swarm-test-status-toggle-group ()
  "Test collapsing a status group."
  :tags '(:unit)
  (beads-swarm-test--with-buffer #'beads-swarm-status-mode
    (setq beads-swarm--id "ep-1"
          beads-swarm--status beads-swarm-test--status-result)
    (beads-swarm--render)
    (should (string-match-p "Active task"
                            (buffer-substring-no-properties
                             (point-min) (point-max))))
    (beads-swarm-status-toggle-group 'active)
    (should-not (string-match-p "Active task"
                                (buffer-substring-no-properties
                                 (point-min) (point-max))))))

(ert-deftest beads-swarm-test-error-string-shapes ()
  "Test normalizing the async error shapes."
  :tags '(:unit)
  (should (equal "boom" (beads-swarm--error-string "boom")))
  (should (equal "Command failed with exit code 1"
                 (beads-swarm--error-string
                  '("Command failed with exit code 1" :command "bd"))))
  (should (equal "condition"
                 (beads-swarm--error-string '(error "condition")))))

(ert-deftest beads-swarm-test-status-command-error ()
  "Test that a non-zero-exit async error renders instead of crashing."
  :tags '(:unit)
  (beads-swarm-test--with-buffer #'beads-swarm-status-mode
    (setq beads-swarm--id "ep-1")
    (cl-letf (((symbol-function 'beads-command-execute-async)
               (lambda (_cmd _success &optional on-error &rest _)
                 (funcall on-error '("Command failed with exit code 1")))))
      (beads-swarm-status--load (current-buffer)))
    (should (equal "Command failed with exit code 1"
                   beads-swarm--status-error))
    (should (string-match-p "Command failed"
                            (buffer-substring-no-properties
                             (point-min) (point-max))))))

;;; Integration test

(ert-deftest beads-swarm-test-integration-roundtrip ()
  "Integration: validate -> create -> list -> status -> claim -> close."
  :tags '(:integration :slow)
  (skip-unless (executable-find (if (boundp 'beads-executable)
                                    beads-executable "bd")))
  (skip-unless (beads-test-bd-has-subcommand-p "swarm"))
  (beads-test-with-temp-repo (:init-beads t)
    (let* ((epic (beads-execute 'beads-command-create
                                :title "Swarm epic" :issue-type "epic"))
           (epic-id (oref epic id))
           (a (beads-execute 'beads-command-create
                             :title "Task A" :issue-type "task"
                             :parent epic-id))
           (_b (beads-execute 'beads-command-create
                              :title "Task B" :issue-type "task"
                              :parent epic-id
                              :deps (list (concat "blocked-by:" (oref a id))))))
      ;; validate (computed)
      (let ((validate (beads-execute 'beads-command-swarm-validate
                                     :epic-id epic-id)))
        (should (beads-swarm--field validate 'swarmable))
        (should (beads-swarm--field validate 'ready_fronts)))
      ;; create + list (standalone embedded Dolt)
      (let ((created (beads-execute 'beads-command-swarm-create
                                    :epic-id epic-id)))
        (should (beads-swarm--field created 'swarm_id)))
      (let* ((result (beads-execute 'beads-command-swarm-list))
             (items (beads-swarm--list-items result)))
        (should items)
        (should (plist-get (beads-swarm-display (car items)) :epic-id)))
      ;; status: ready has Task A, blocked has Task B with blocked_by
      (let ((status (beads-execute 'beads-command-swarm-status
                                   :swarm-id epic-id)))
        (should (beads-swarm--field status 'ready))
        (should (beads-swarm--field status 'blocked)))
      ;; claim A, then the worker source sees it
      (let* ((updated (beads-execute 'beads-command-update
                                     :issue-ids (list (oref a id))
                                     :claim t))
             (claimed (cond ((listp updated) (car updated))
                            ((vectorp updated) (aref updated 0))
                            (t updated)))
             (assignee (beads-swarm--field claimed 'assignee))
             (lane (beads-swarm-worker-source assignee)))
        (should (listp lane))
        (should (listp (plist-get lane :ids))))
      (beads-execute 'beads-command-close :issue-ids (list (oref a id))
                     :reason "done"))))

(provide 'beads-swarm-test)
;;; beads-swarm-test.el ends here
