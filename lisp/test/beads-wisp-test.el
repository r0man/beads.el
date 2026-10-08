;;; beads-wisp-test.el --- Tests for beads-wisp -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Unit and integration tests for the wisp list and lifecycle UI:
;; `beads-wisp-kind', the `bd mol wisp list --json' unwrapping in
;; `beads-wisp--normalize', the tabulated entries and the
;; squash/burn/purge commands.

;;; Code:

(require 'ert)
(require 'beads-integration-test)
(require 'beads-wisp)
(require 'beads-command-mol)
(require 'beads-command-misc)

;;; Fixtures

(defun beads-wisp-test--timestamp (seconds-ago)
  "Return an RFC3339 UTC timestamp SECONDS-AGO seconds before now."
  (format-time-string "%Y-%m-%dT%H:%M:%SZ"
                      (time-subtract (current-time) seconds-ago)
                      t))

(defun beads-wisp-test--issue (id &optional status updated type ephemeral)
  "Build a `beads-issue' wisp for tests.
ID is the issue id; STATUS defaults to \"open\", UPDATED to now, TYPE
to \"task\"; EPHEMERAL defaults to t."
  (beads-from-json
   'beads-issue
   `((id . ,id)
     (title . ,(format "Wisp %s" id))
     (status . ,(or status "open"))
     (issue_type . ,(or type "task"))
     (created_at . ,(beads-wisp-test--timestamp 3600))
     (updated_at . ,(or updated (beads-wisp-test--timestamp 0)))
     (ephemeral . ,(if (null ephemeral) t ephemeral)))))

(defun beads-wisp-test--wire (id title status updated type)
  "Build one wisp alist exactly as `bd mol wisp list --json' emits it."
  `((id . ,id)
    (title . ,title)
    (status . ,status)
    (type . ,type)
    (created_at . ,(beads-wisp-test--timestamp 7200))
    (updated_at . ,updated)))

(defun beads-wisp-test--payload ()
  "Return a two-row wisp-list payload with one old row."
  `((count . 2)
    (schema_version . 1)
    (wisps . ,(vector
               (beads-wisp-test--wire "w-new" "Fresh" "open"
                                      (beads-wisp-test--timestamp 60) "task")
               (beads-wisp-test--wire "w-old" "Stale" "open"
                                      (beads-wisp-test--timestamp 172800)
                                      "molecule")))))

;;; beads-wisp-kind

(ert-deftest beads-wisp-test-kind-new ()
  "A wisp updated just now is new."
  :tags '(:unit)
  (should (eq 'new (beads-wisp-kind
                    (beads-wisp-test--issue "w-1" "open"
                                            (beads-wisp-test--timestamp 60))))))

(ert-deftest beads-wisp-test-kind-old ()
  "A wisp not updated in 24h is old."
  :tags '(:unit)
  (should (eq 'old (beads-wisp-kind
                    (beads-wisp-test--issue "w-1" "open"
                                            (beads-wisp-test--timestamp 172800))))))

(ert-deftest beads-wisp-test-kind-old-boundary ()
  "A wisp updated just under 24h ago is still new.
`beads-wisp-old-seconds' is honored rather than a hardcoded value."
  :tags '(:unit)
  (let ((beads-wisp-old-seconds 3600))
    (should (eq 'new (beads-wisp-kind
                      (beads-wisp-test--issue "w-1" "open"
                                              (beads-wisp-test--timestamp 60)))))
    (should (eq 'old (beads-wisp-kind
                      (beads-wisp-test--issue "w-2" "open"
                                              (beads-wisp-test--timestamp 7200)))))))

(ert-deftest beads-wisp-test-kind-closed ()
  "A closed wisp is closed even when recent."
  :tags '(:unit)
  (should (eq 'closed (beads-wisp-kind
                       (beads-wisp-test--issue "w-1" "closed"
                                               (beads-wisp-test--timestamp 60))))))

;;; beads-wisp--normalize

(ert-deftest beads-wisp-test-normalize-unwraps-wisps-object ()
  "Normalization unwraps the `wisps' array and maps `type' to issue-type."
  :tags '(:unit)
  (let ((wisps (beads-wisp--normalize (beads-wisp-test--payload))))
    (should (= 2 (length wisps)))
    (should (equal "w-new" (oref (car wisps) id)))
    (should (equal "task" (oref (car wisps) issue-type)))
    (should (eq t (oref (car wisps) ephemeral)))
    (should (equal "w-old" (oref (cadr wisps) id)))
    (should (equal "molecule" (oref (cadr wisps) issue-type)))))

(ert-deftest beads-wisp-test-normalize-bare-vector ()
  "Normalization accepts a bare vector of wisp alists."
  :tags '(:unit)
  (let ((wisps (beads-wisp--normalize
                (vector (beads-wisp-test--wire
                         "w-1" "One" "open"
                         (beads-wisp-test--timestamp 0) "task")))))
    (should (= 1 (length wisps)))
    (should (equal "w-1" (oref (car wisps) id)))))

(ert-deftest beads-wisp-test-normalize-empty ()
  "An empty `wisps' array normalizes to nil."
  :tags '(:unit)
  (should-not (beads-wisp--normalize '((count . 0) (wisps . [])))))

;;; Entry formatting

(ert-deftest beads-wisp-test-format-age ()
  "Age formatting covers seconds, minutes, hours and days."
  :tags '(:unit)
  (let ((wisp (beads-wisp-test--issue "w-1" "open"
                                      (beads-wisp-test--timestamp 1800))))
    (should (string-match-p "m\\'" (beads-wisp--format-age wisp)))
    (let ((wisp (beads-wisp-test--issue "w-1" "open"
                                        (beads-wisp-test--timestamp 7200))))
      (should (string-match-p "h" (beads-wisp--format-age wisp))))
    (let ((wisp (beads-wisp-test--issue "w-1" "open"
                                        (beads-wisp-test--timestamp 172800))))
      (should (string-match-p "d" (beads-wisp--format-age wisp))))))

(ert-deftest beads-wisp-test-entry-columns ()
  "An entry has the mockup columns and marks an old wisp."
  :tags '(:unit)
  (let* ((wisp (beads-wisp-test--issue "w-old" "open"
                                       (beads-wisp-test--timestamp 172800)
                                       "molecule"))
         (entry (beads-wisp--entry wisp))
         (cols (cadr entry)))
    (should (equal "w-old" (car entry)))
    (should (= 7 (length cols)))
    (should (equal "w-old" (aref cols 0)))
    (should (equal "molecule" (aref cols 1)))
    (should (string-match-p "vapor" (aref cols 2)))
    (should (string-match-p "open" (aref cols 3)))
    (should (string-match-p "old" (aref cols 6)))))

(ert-deftest beads-wisp-test-entry-persistent-phase ()
  "A non-ephemeral issue renders the persistent phase badge."
  :tags '(:unit)
  (let* ((issue (beads-from-json
                 'beads-issue
                 '((id . "m-1")
                   (title . "Persistent")
                   (status . "open")
                   (issue_type . "molecule")
                   (ephemeral . nil))))
         (cols (cadr (beads-wisp--entry issue))))
    (should (string-match-p "persistent" (aref cols 2)))))

;;; Buffer refresh

(ert-deftest beads-wisp-test-refresh-populates ()
  "Refresh populates the buffer from an async result."
  :tags '(:unit)
  (cl-letf (((symbol-function 'beads-command-execute-async)
             (lambda (_cmd on-success &optional _on-error &rest _)
               (funcall on-success (beads-wisp-test--payload)))))
    (with-temp-buffer
      (beads-wisp-list-mode)
      (beads-wisp-refresh)
      (should (= 2 (length tabulated-list-entries)))
      (should (equal "w-new" (caar tabulated-list-entries))))))

(ert-deftest beads-wisp-test-refresh-command-flags ()
  "Refresh passes `--all' and `--type' from the buffer-local state."
  :tags '(:unit)
  (let ((captured nil))
    (cl-letf (((symbol-function 'beads-command-execute-async)
               (lambda (cmd on-success &optional _on-error &rest _)
                 (setq captured cmd)
                 (funcall on-success '((wisps . []))))))
      (with-temp-buffer
        (beads-wisp-list-mode)
        (setq beads-wisp--show-all t
              beads-wisp--type-filter "patrol")
        (beads-wisp-refresh)
        (should captured)
        (should (oref captured show-all))
        (should (equal "patrol" (oref captured type-filter)))))))

(ert-deftest beads-wisp-test-refresh-error ()
  "An async error is recorded and leaves the buffer empty."
  :tags '(:unit)
  (cl-letf (((symbol-function 'beads-command-execute-async)
             (lambda (_cmd _on-success &optional on-error &rest _)
               (funcall on-error '(error "boom"))))
            ((symbol-function 'beads-pager-set-entries)
             (lambda (entries) (setq tabulated-list-entries entries))))
    (with-temp-buffer
      (beads-wisp-list-mode)
      (beads-wisp-refresh)
      (should beads-wisp--last-error)
      (should-not tabulated-list-entries))))

;;; Squash

(ert-deftest beads-wisp-test-squash-command ()
  "Squash builds a `bd mol squash' with summary and keep-children."
  :tags '(:unit)
  (let* ((captured nil))
    (cl-letf (((symbol-function 'beads-wisp--current-id)
               (lambda () "w-1"))
              ((symbol-function 'read-string)
               (lambda (&rest _) "agent summary"))
              ((symbol-function 'yes-or-no-p)
               (lambda (&rest _) t))
              ((symbol-function 'beads-command-execute)
               (lambda (cmd) (setq captured cmd) nil))
              ((symbol-function 'beads-wisp-refresh)
               (lambda () nil)))
      (beads-wisp-squash)
      (should captured)
      (should (cl-typep captured 'beads-command-mol-squash))
      (should (equal "w-1" (oref captured mol-id)))
      (should (equal "agent summary" (oref captured summary)))
      (should (oref captured keep-children))
      (should-not (oref captured json)))))

(ert-deftest beads-wisp-test-squash-abort ()
  "Squash does nothing when the confirmation is declined."
  :tags '(:unit)
  (let ((executed nil))
    (cl-letf (((symbol-function 'beads-wisp--current-id)
               (lambda () "w-1"))
              ((symbol-function 'read-string)
               (lambda (&rest _) ""))
              ((symbol-function 'yes-or-no-p)
               (lambda (&rest _) nil))
              ((symbol-function 'beads-command-execute)
               (lambda (_cmd) (setq executed t) nil))
              ((symbol-function 'beads-wisp-refresh)
               (lambda () nil)))
      (beads-wisp-squash)
      (should-not executed))))

;;; Burn

(ert-deftest beads-wisp-test-burn-confirm ()
  "Burn dry-runs, then burns with --force after typed confirmation."
  :tags '(:unit)
  (let ((commands nil))
    (cl-letf (((symbol-function 'beads-wisp--targets)
               (lambda () '("w-1")))
              ((symbol-function 'read-string)
               (lambda (&rest _) "w-1"))
              ((symbol-function 'beads-command-execute)
               (lambda (cmd) (push cmd commands) ""))
              ((symbol-function 'beads-wisp-refresh)
               (lambda () nil)))
      (beads-wisp-burn)
      (setq commands (nreverse commands))
      (should (= 2 (length commands)))
      (should (cl-typep (car commands) 'beads-command-mol-burn))
      (should (oref (car commands) dry-run))
      (should-not (oref (car commands) force))
      (should (oref (cadr commands) force))
      (should-not (oref (cadr commands) dry-run))
      (should (equal "w-1" (oref (cadr commands) mol-id))))))

(ert-deftest beads-wisp-test-burn-abort ()
  "Burn does nothing when the typed confirmation does not match."
  :tags '(:unit)
  (let ((commands nil))
    (cl-letf (((symbol-function 'beads-wisp--targets)
               (lambda () '("w-1")))
              ((symbol-function 'read-string)
               (lambda (&rest _) "wrong"))
              ((symbol-function 'beads-command-execute)
               (lambda (cmd) (push cmd commands) ""))
              ((symbol-function 'beads-wisp-refresh)
               (lambda () nil)))
      (beads-wisp-burn)
      ;; Only the dry-run preview ran.
      (should (= 1 (length commands)))
      (should (oref (car commands) dry-run)))))

(ert-deftest beads-wisp-test-burn-batch ()
  "Burn acts on every marked wisp and asks for `yes'."
  :tags '(:unit)
  (let ((burns nil)
        (prompt nil))
    (cl-letf (((symbol-function 'beads-wisp--targets)
               (lambda () '("w-1" "w-2")))
              ((symbol-function 'read-string)
               (lambda (p &rest _) (setq prompt p) "yes"))
              ((symbol-function 'beads-command-execute)
               (lambda (cmd)
                 (when (and (cl-typep cmd 'beads-command-mol-burn)
                            (oref cmd force))
                   (push (oref cmd mol-id) burns))
                 ""))
              ((symbol-function 'beads-wisp-refresh)
               (lambda () nil)))
      (beads-wisp-burn)
      (should (equal '("w-1" "w-2") (nreverse burns)))
      (should (string-match-p "yes" prompt)))))

;;; Purge

(ert-deftest beads-wisp-test-purge ()
  "Purge dry-runs, echoes the preview, then forces after confirmation."
  :tags '(:unit)
  (let ((commands nil)
        (answers '("7d" "*-wisp-*" "yes")))
    (cl-letf (((symbol-function 'read-string)
               (lambda (&rest _) (pop answers)))
              ((symbol-function 'beads-command-execute)
               (lambda (cmd)
                 (push cmd commands)
                 '((message . "12 beads would be purged")
                   (purged_count . 12))))
              ((symbol-function 'beads-wisp-refresh)
               (lambda () nil)))
      (beads-wisp-purge)
      (setq commands (nreverse commands))
      (should (= 2 (length commands)))
      (should (cl-typep (car commands) 'beads-command-purge))
      (should (oref (car commands) dry-run))
      (should (equal "7d" (oref (car commands) older-than)))
      (should (equal "*-wisp-*" (oref (car commands) pattern)))
      (should (oref (car commands) json))
      (should (oref (cadr commands) force))
      (should-not (oref (cadr commands) dry-run))
      (should (oref (cadr commands) json)))))

;;; View toggles and open

(ert-deftest beads-wisp-test-toggle-all ()
  "Toggle-all flips the `--all' buffer state and refreshes."
  :tags '(:unit)
  (let ((refreshed nil))
    (cl-letf (((symbol-function 'beads-wisp-refresh)
               (lambda () (setq refreshed t))))
      (with-temp-buffer
        (setq beads-wisp--show-all nil)
        (beads-wisp-toggle-all)
        (should beads-wisp--show-all)
        (should refreshed)
        (beads-wisp-toggle-all)
        (should-not beads-wisp--show-all)))))

(ert-deftest beads-wisp-test-open-root-molecule ()
  "Open-root prefers `beads-molecule-open' when it is available."
  :tags '(:unit)
  (let ((called nil))
    (cl-letf (((symbol-function 'beads-wisp--current-id)
               (lambda () "w-9"))
              ((symbol-function 'beads-molecule-open)
               (lambda (id) (setq called (cons :molecule id)))))
      (beads-wisp-open-root)
      (should (equal '(:molecule . "w-9") called)))))

(ert-deftest beads-wisp-test-open-root-fallback ()
  "Open-root runs `bd mol show' without the molecule module."
  :tags '(:unit)
  (let ((called nil)
        (had-molecule (fboundp 'beads-molecule-open))
        (old-molecule (and (fboundp 'beads-molecule-open)
                           (symbol-function 'beads-molecule-open))))
    (unwind-protect
        (progn
          (when had-molecule (fmakunbound 'beads-molecule-open))
          (cl-letf (((symbol-function 'beads-wisp--current-id)
                     (lambda () "w-9"))
                    ((symbol-function 'beads-command-execute-interactive)
                     (lambda (cmd) (setq called cmd))))
            (beads-wisp-open-root)
            (should (cl-typep called 'beads-command-mol-show))
            (should (equal "w-9" (oref called mol-id)))))
      (when had-molecule (fset 'beads-molecule-open old-molecule)))))

;;; Integration

(defconst beads-wisp-test--formula "wisp-flow"
  "Local formula the wisp integration tests create and use.")

(defun beads-wisp-test--write-formula (name)
  "Write a minimal NAME formula into the current temp repo.
The tests must not depend on a formula shipped by the host `bd'
(`mol-do-work' exists in some builds only); a project formula works on
every `bd' version."
  (let ((path (expand-file-name (format ".beads/formulas/%s.formula.toml" name))))
    (make-directory (file-name-directory path) t)
    (with-temp-file path
      (insert (format (concat "formula = \"%s\"\n"
                              "description = \"wisp integration test\"\n"
                              "type = \"workflow\"\nversion = 1\n\n"
                              "[[steps]]\nid = \"s1\"\ntitle = \"%s step\"\ntype = \"task\"\n")
                      name name)))
    path))

(defun beads-wisp-test--create-wisp ()
  "Create a wisp from the local test formula.
Returns the new root molecule id."
  (beads-wisp-test--write-formula beads-wisp-test--formula)
  (let ((result (beads-command-execute
                 (beads-command-mol-wisp-create
                  :proto-id beads-wisp-test--formula :json t))))
    (alist-get 'new_epic_id result)))

(defun beads-wisp-test--live-wisps ()
  "Return the live wisp list as `beads-issue' objects."
  (beads-wisp--normalize
   (beads-command-execute
    (beads-command-mol-wisp-list :show-all t :json t))))

(defun beads-wisp-test--ids (wisps)
  "Return the ids of WISPS."
  (mapcar (lambda (wisp) (oref wisp id)) wisps))

(ert-deftest beads-wisp-test-integration-list-squash ()
  "Cook a wisp, list it, then squash it into a persistent digest."
  :tags '(:integration :slow)
  (beads-test-skip-unless-bd)
  (beads-test-with-temp-repo (:init-beads t)
    (let* ((root (beads-wisp-test--create-wisp))
           (before (beads-wisp-test--live-wisps)))
      (should root)
      (should (> (length before) 0))
      (should (member root (beads-wisp-test--ids before)))
      ;; Everything created through `mol wisp create' is ephemeral/vapor.
      (should (equal 'vapor
                     (beads-wisp--phase
                      (seq-find (lambda (w) (equal root (oref w id))) before))))
      ;; Squash promotes the ephemeral children into a digest.
      (beads-command-execute
       (beads-command-mol-squash
        :mol-id root :summary "integration squash" :json nil))
      (let ((after (beads-wisp-test--live-wisps)))
        (should-not (member root (beads-wisp-test--ids after)))))))

(ert-deftest beads-wisp-test-integration-kind-old ()
  "The old classifier works against live `bd mol wisp list' rows."
  :tags '(:integration :slow)
  (beads-test-skip-unless-bd)
  (beads-test-with-temp-repo (:init-beads t)
    (let* ((root (beads-wisp-test--create-wisp))
           (wisps (beads-wisp-test--live-wisps))
           (root-wisp (seq-find (lambda (w) (equal root (oref w id))) wisps)))
      (should root-wisp)
      (should (memq (beads-wisp-kind root-wisp) '(new old)))
      (should-not (eq 'closed (beads-wisp-kind root-wisp))))))

(ert-deftest beads-wisp-test-integration-burn ()
  "A wisp can be burned (with --force) after listing it."
  :tags '(:integration :slow)
  (beads-test-skip-unless-bd)
  (beads-test-with-temp-repo (:init-beads t)
    (let* ((root (beads-wisp-test--create-wisp)))
      (should root)
      (beads-command-execute
       (beads-command-mol-burn :mol-id root :force t :json nil))
      (should-not (member root (beads-wisp-test--ids
                                (beads-wisp-test--live-wisps)))))))

(provide 'beads-wisp-test)
;;; beads-wisp-test.el ends here
