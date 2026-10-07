;;; beads-command-swarm-test.el --- Tests for beads-command-swarm -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for beads-command-swarm command classes.

;;; Code:

(require 'ert)
(require 'beads-command-swarm)

;;; Unit Tests: beads-command-swarm-create command-line

(ert-deftest beads-command-swarm-create-test-command-line-basic ()
  "Unit test: swarm create builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-swarm-create :epic-id "epic-1"))
         (args (beads-command-line cmd)))
    (should (member "swarm" args))
    (should (member "create" args))
    (should (member "epic-1" args))))

(ert-deftest beads-command-swarm-create-test-validation-missing-epic-id ()
  "Unit test: swarm create validation fails without epic-id."
  :tags '(:unit)
  (let ((cmd (beads-command-swarm-create)))
    (should (beads-command-validate cmd))))

(ert-deftest beads-command-swarm-create-test-validation-success ()
  "Unit test: swarm create validation succeeds with epic-id."
  :tags '(:unit)
  (let ((cmd (beads-command-swarm-create :epic-id "epic-1")))
    (should (null (beads-command-validate cmd)))))

;;; Unit Tests: beads-command-swarm-list command-line

(ert-deftest beads-command-swarm-list-test-command-line-basic ()
  "Unit test: swarm list builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-swarm-list))
         (args (beads-command-line cmd)))
    (should (member "swarm" args))
    (should (member "list" args))))

;;; Unit Tests: beads-command-swarm-status command-line

(ert-deftest beads-command-swarm-status-test-command-line-basic ()
  "Unit test: swarm status builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-swarm-status :swarm-id "swarm-1"))
         (args (beads-command-line cmd)))
    (should (member "swarm" args))
    (should (member "status" args))
    (should (member "swarm-1" args))))

;;; Unit Tests: beads-command-swarm-validate command-line

(ert-deftest beads-command-swarm-validate-test-command-line-basic ()
  "Unit test: swarm validate builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-swarm-validate :epic-id "epic-1"))
         (args (beads-command-line cmd)))
    (should (member "swarm" args))
    (should (member "validate" args))
    (should (member "epic-1" args))))

(ert-deftest beads-command-swarm-validate-test-validation-missing-epic-id ()
  "Unit test: swarm validate validation fails without epic-id."
  :tags '(:unit)
  (let ((cmd (beads-command-swarm-validate)))
    (should (beads-command-validate cmd))))

(ert-deftest beads-command-swarm-validate-test-validation-success ()
  "Unit test: swarm validate validation succeeds with epic-id."
  :tags '(:unit)
  (let ((cmd (beads-command-swarm-validate :epic-id "epic-1")))
    (should (null (beads-command-validate cmd)))))

;;; ============================================================
;;; Fixtures
;;; ============================================================

(defconst beads-command-swarm-test--list-json
  "{\"schema_version\":1,\"swarms\":[{\"id\":\"be-sw1\",\"title\":\"Swarm: Epic\",\"status\":\"open\",\"epic_id\":\"be-ep1\",\"epic_title\":\"Epic\",\"coordinator\":\"rig/obs\",\"total_issues\":3,\"completed_issues\":1,\"active_issues\":1,\"progress_percent\":33.33}]}"
  "JSON fixture for `bd swarm list --json'.")

(defconst beads-command-swarm-test--status-json
  "{\"schema_version\":1,\"epic_id\":\"be-ep1\",\"epic_title\":\"Epic\",\"total_issues\":3,\"progress_percent\":33.33,\"active_count\":1,\"ready_count\":0,\"blocked_count\":1,\"completed\":[{\"id\":\"be.1\",\"title\":\"A\",\"closed_at\":\"2026-10-07 16:32\"}],\"active\":[{\"id\":\"be.3\",\"title\":\"C\",\"assignee\":\"test\"}],\"ready\":[],\"blocked\":[{\"id\":\"be.2\",\"title\":\"B\",\"blocked_by\":[\"be.3\"]}]}"
  "JSON fixture for `bd swarm status --json'.")

(defconst beads-command-swarm-test--validate-json
  "{\"schema_version\":1,\"epic_id\":\"be-ep1\",\"epic_title\":\"Epic\",\"swarmable\":true,\"total_issues\":3,\"closed_issues\":1,\"estimated_sessions\":2,\"max_parallelism\":2,\"warnings\":null,\"errors\":null,\"ready_fronts\":[{\"wave\":0,\"issues\":[\"be.1\",\"be.3\"],\"titles\":[\"A\",\"C\"]},{\"wave\":1,\"issues\":[\"be.2\"],\"titles\":[\"B\"]}],\"issues\":{\"be.1\":{\"id\":\"be.1\",\"title\":\"A\",\"status\":\"open\",\"priority\":2,\"wave\":0,\"depends_on\":[],\"depended_on_by\":[\"be.2\"]},\"be.2\":{\"id\":\"be.2\",\"title\":\"B\",\"status\":\"open\",\"priority\":2,\"wave\":1,\"depends_on\":[\"be.1\"],\"depended_on_by\":[]}}}"
  "JSON fixture for `bd swarm validate --verbose --json'.")

(defconst beads-command-swarm-test--create-json
  (concat
   "{\"schema_version\":1,\"swarm_id\":\"be-sw1\",\"epic_id\":\"be-ep1\","
   "\"coordinator\":\"rig/obs\",\"analysis\":"
   "{\"epic_id\":\"be-ep1\",\"epic_title\":\"Epic\",\"swarmable\":true,"
   "\"total_issues\":3,\"closed_issues\":0,\"estimated_sessions\":2,"
   "\"max_parallelism\":2,\"ready_fronts\":[{\"wave\":0,\"issues\":[\"be.1\"],"
   "\"titles\":[\"A\"]}]}}")
  "JSON fixture for `bd swarm create --json' success.")

(defconst beads-command-swarm-test--create-exists-json
  "{\"schema_version\":1,\"error\":\"swarm already exists\",\"existing_id\":\"be-sw1\",\"existing_title\":\"Swarm: Epic\"}"
  "JSON fixture for the `swarm already exists' domain error.")

(defconst beads-command-swarm-test--create-not-swarmable-json
  "{\"schema_version\":1,\"error\":\"epic is not swarmable\",\"analysis\":{\"epic_id\":\"be-ep1\",\"swarmable\":false,\"errors\":[\"cycle: a -> b -> a\"]}}"
  "JSON fixture for the `epic is not swarmable' domain error.")

(defconst beads-command-swarm-test--validate-not-swarmable-json
  "{\"schema_version\":1,\"epic_id\":\"be-ep1\",\"epic_title\":\"Epic\",\"swarmable\":false,\"errors\":[\"cycle: a -> b -> a\"]}"
  "JSON fixture for a non-swarmable validate domain state.")

(defun beads-command-swarm-test--read-alist (json)
  "Read JSON string into an alist with symbol keys."
  (let ((json-null nil)
        (json-object-type 'alist)
        (json-array-type 'list)
        (json-key-type 'symbol))
    (json-read-from-string json)))

;;; ============================================================
;;; :result declarations
;;; ============================================================

(ert-deftest beads-command-swarm-test-result-declarations ()
  "All four swarm command classes declare a result type."
  :tags '(:unit)
  (should (eq (get 'beads-command-swarm-create 'beads-result)
              'beads-swarm-create-result))
  (should (equal (get 'beads-command-swarm-list 'beads-result)
                 '(list-of beads-swarm-list-item)))
  (should (eq (get 'beads-command-swarm-status 'beads-result)
              'beads-swarm-status))
  (should (eq (get 'beads-command-swarm-validate 'beads-result)
              'beads-swarm-analysis)))

;;; ============================================================
;;; Parsing
;;; ============================================================

(ert-deftest beads-command-swarm-list-test-parse-unwraps-swarms ()
  "List parse unwraps `swarms' into typed items."
  :tags '(:unit)
  (let* ((cmd (beads-command-swarm-list :json t))
         (items (beads-command-parse cmd beads-command-swarm-test--list-json)))
    (should (= 1 (length items)))
    (let ((item (car items)))
      (should (beads-swarm-list-item-p item))
      (should (equal "be-sw1" (oref item id)))
      (should (equal "Epic" (oref item epic-title)))
      (should (equal "rig/obs" (oref item coordinator)))
      (should (= 3 (oref item total-issues)))
      (should (= 1 (oref item completed-issues)))
      (should (= 1 (oref item active-issues)))
      (should (= 33.33 (oref item progress-percent))))))

(ert-deftest beads-command-swarm-list-test-parse-empty ()
  "An empty swarm list parses to nil."
  :tags '(:unit)
  (let ((cmd (beads-command-swarm-list :json t)))
    (should (null (beads-command-parse cmd "{\"schema_version\":1,\"swarms\":[]}")))))

(ert-deftest beads-command-swarm-status-test-parse-groups ()
  "Status parse populates the four groups and their counts."
  :tags '(:unit)
  (let* ((cmd (beads-command-swarm-status :swarm-id "be-sw1" :json t))
         (status (beads-command-parse cmd beads-command-swarm-test--status-json)))
    (should (beads-swarm-status-p status))
    (should (equal "be-ep1" (oref status epic-id)))
    (should (= 3 (oref status total-issues)))
    (should (= 1 (oref status active-count)))
    (should (= 0 (oref status ready-count)))
    (should (= 1 (oref status blocked-count)))
    (let ((completed (car (oref status completed))))
      (should (beads-swarm-status-issue-p completed))
      (should (equal "be.1" (oref completed id)))
      (should (equal "2026-10-07 16:32" (oref completed closed-at))))
    (should (equal "test" (oref (car (oref status active)) assignee)))
    (should (equal '("be.3") (oref (car (oref status blocked)) blocked-by)))
    (should (null (oref status ready)))))

(ert-deftest beads-command-swarm-validate-test-parse-waves-and-graph ()
  "Validate parse builds ready fronts, wave math and the issue graph."
  :tags '(:unit)
  (let* ((cmd (beads-command-swarm-validate :epic-id "be-ep1" :json t))
         (analysis (beads-command-parse cmd beads-command-swarm-test--validate-json))
         (fronts (oref analysis ready-fronts))
         (nodes (oref analysis issues)))
    (should (beads-swarm-analysis-p analysis))
    (should (eq t (oref analysis swarmable)))
    (should (= 3 (oref analysis total-issues)))
    (should (= 1 (oref analysis closed-issues)))
    (should (= 2 (oref analysis estimated-sessions)))
    (should (= 2 (oref analysis max-parallelism)))
    (should (= 2 (length fronts)))
    (should (= 0 (oref (car fronts) wave)))
    (should (equal '("be.1" "be.3") (oref (car fronts) issues)))
    (should (= 1 (oref (cadr fronts) wave)))
    (should (equal '("be.2") (oref (cadr fronts) issues)))
    (should (= 2 (length nodes)))
    (let ((node (seq-find (lambda (n) (equal "be.2" (oref n id))) nodes)))
      (should (beads-swarm-issue-node-p node))
      (should (equal "B" (oref node title)))
      (should (= 1 (oref node wave)))
      (should (equal '("be.1") (oref node depends-on)))
      (should (null (oref node depended-on-by))))))

(ert-deftest beads-command-swarm-create-test-parse-success ()
  "Create success parses the envelope and nested analysis."
  :tags '(:unit)
  (let* ((cmd (beads-command-swarm-create :epic-id "be-ep1" :json t))
         (result (beads-command-parse cmd beads-command-swarm-test--create-json)))
    (should (beads-swarm-create-result-p result))
    (should (equal "be-sw1" (oref result swarm-id)))
    (should (equal "be-ep1" (oref result epic-id)))
    (should (equal "rig/obs" (oref result coordinator)))
    (should (null (oref result error)))
    (should (beads-swarm-analysis-p (oref result analysis)))
    (should (eq t (oref (oref result analysis) swarmable)))
    (should (equal '("be.1") (oref (car (oref (oref result analysis) ready-fronts))
                                    issues)))))

(ert-deftest beads-command-swarm-create-test-parse-already-exists ()
  "Create already-exists preserves the domain error and existing swarm."
  :tags '(:unit)
  (let* ((cmd (beads-command-swarm-create :epic-id "be-ep1" :json t))
         (result (beads-command-parse cmd
                                      beads-command-swarm-test--create-exists-json)))
    (should (equal "swarm already exists" (oref result error)))
    (should (equal "be-sw1" (oref result existing-id)))
    (should (equal "Swarm: Epic" (oref result existing-title)))
    (should (beads-swarm-domain-error-p result))))

(ert-deftest beads-command-swarm-create-test-parse-not-swarmable ()
  "Create not-swarmable preserves the error and embedded analysis."
  :tags '(:unit)
  (let* ((cmd (beads-command-swarm-create :epic-id "be-ep1" :json t))
         (result (beads-command-parse cmd
                                      beads-command-swarm-test--create-not-swarmable-json)))
    (should (equal "epic is not swarmable" (oref result error)))
    (should (beads-swarm-analysis-p (oref result analysis)))
    (should (null (oref (oref result analysis) swarmable)))
    (should (equal '("cycle: a -> b -> a") (oref (oref result analysis) errors)))
    (should (beads-swarm-domain-error-p result))))

;;; ============================================================
;;; Domain-error detector
;;; ============================================================

(ert-deftest beads-command-swarm-domain-error-p-alist-already-exists ()
  "Detector recognises the raw `already exists' payload."
  :tags '(:unit)
  (let ((parsed (beads-command-swarm-test--read-alist
                 beads-command-swarm-test--create-exists-json)))
    (should (equal "swarm already exists"
                   (beads-swarm-domain-error-p parsed)))))

(ert-deftest beads-command-swarm-domain-error-p-alist-not-swarmable ()
  "Detector recognises the raw `not swarmable' payload."
  :tags '(:unit)
  (let ((parsed (beads-command-swarm-test--read-alist
                 beads-command-swarm-test--create-not-swarmable-json)))
    (should (equal "epic is not swarmable"
                   (beads-swarm-domain-error-p parsed)))))

(ert-deftest beads-command-swarm-domain-error-p-validate-false ()
  "Detector recognises a swarmable=false validate payload."
  :tags '(:unit)
  (let* ((cmd (beads-command-swarm-validate :epic-id "be-ep1" :json t))
         (parsed (beads-command-parse
                  cmd beads-command-swarm-test--validate-not-swarmable-json)))
    (should (equal "swarmable=false" (beads-swarm-domain-error-p parsed)))))

(ert-deftest beads-command-swarm-domain-error-p-validate-false-alist ()
  "Detector recognises a raw swarmable=false validate payload."
  :tags '(:unit)
  (let ((parsed (beads-command-swarm-test--read-alist
                 beads-command-swarm-test--validate-not-swarmable-json)))
    (should (equal "swarmable=false"
                   (beads-swarm-domain-error-p parsed)))))

(ert-deftest beads-command-swarm-domain-error-p-happy-path ()
  "A successful validate result is not a domain error."
  :tags '(:unit)
  (let* ((cmd (beads-command-swarm-validate :epic-id "be-ep1" :json t))
         (parsed (beads-command-parse cmd beads-command-swarm-test--validate-json)))
    (should (null (beads-swarm-domain-error-p parsed)))
    (should (null (beads-swarm-domain-error-p
                   (beads-command-swarm-test--read-alist
                    "{\"epic_id\":\"be-ep1\",\"swarmable\":true}"))))))

(ert-deftest beads-command-swarm-domain-error-p-unrelated ()
  "An unrelated error string is not a swarm domain error."
  :tags '(:unit)
  (should (null (beads-swarm-domain-error-p
                 (beads-command-swarm-test--read-alist
                  "{\"error\":\"database is locked\"}"))))
  (should (null (beads-swarm-domain-error-p nil))))

(provide 'beads-command-swarm-test)
;;; beads-command-swarm-test.el ends here
