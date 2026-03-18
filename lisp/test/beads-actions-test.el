;;; beads-actions-test.el --- Tests for context-aware actions -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026

;;; Commentary:

;; Tests for Pattern 1 context-aware action commands:
;; beads-actions-close, beads-actions-claim, beads-actions-reopen,
;; beads-actions-set-status, beads-actions-set-priority.

;;; Code:

(require 'ert)
(require 'beads-actions)
(require 'beads-command-list)
(require 'beads-command-show)

;;; beads-actions--target-ids

(ert-deftest beads-actions-test-target-ids-from-marks ()
  "Target IDs should come from marked issues when marks exist."
  :tags '(:unit)
  (with-temp-buffer
    (setq-local beads-list--marked-issues '("bd-1" "bd-2" "bd-3"))
    (should (equal '("bd-1" "bd-2" "bd-3")
                   (beads-actions--target-ids)))))

(ert-deftest beads-actions-test-target-ids-from-point ()
  "Target IDs should fall back to issue at point."
  :tags '(:unit)
  (with-temp-buffer
    (setq-local beads-list--marked-issues nil)
    (let* ((vnew (lambda () "bd-42"))
           (old (symbol-function 'beads-issue-at-point)))
      (unwind-protect
          (progn
            (fset 'beads-issue-at-point vnew)
            (should (equal '("bd-42") (beads-actions--target-ids))))
        (fset 'beads-issue-at-point old)))))

(ert-deftest beads-actions-test-target-ids-none ()
  "Target IDs should signal error when no issue found."
  :tags '(:unit)
  (with-temp-buffer
    (setq-local beads-list--marked-issues nil)
    (let* ((vnew (lambda () nil))
           (old (symbol-function 'beads-issue-at-point)))
      (unwind-protect
          (progn
            (fset 'beads-issue-at-point vnew)
            (should-error (beads-actions--target-ids)
                          :type 'user-error))
        (fset 'beads-issue-at-point old)))))

;;; beads-actions-close

(ert-deftest beads-actions-test-close-single-issue ()
  "Close should prompt for reason and call close! for single issue."
  :tags '(:unit)
  (with-temp-buffer
    (setq-local beads-list--marked-issues nil)
    (let* ((closed-ids nil)
           (closed-reason nil)
           (vnew-at-point (lambda () "bd-42"))
           (vnew-close (lambda (&rest args)
                         (setq closed-ids (plist-get args :issue-ids))
                         (setq closed-reason (plist-get args :reason))
                         nil))
           (vnew-read (lambda (&rest _) "Done"))
           (vnew-after (lambda () nil))
           (old-at-point (symbol-function 'beads-issue-at-point))
           (old-close (symbol-function 'beads-command-close!))
           (old-read (symbol-function 'read-string))
           (old-after (symbol-function 'beads-actions--after-mutation)))
      (unwind-protect
          (progn
            (fset 'beads-issue-at-point vnew-at-point)
            (fset 'beads-command-close! vnew-close)
            (fset 'read-string vnew-read)
            (fset 'beads-actions--after-mutation vnew-after)
            (beads-actions-close)
            (should (equal '("bd-42") closed-ids))
            (should (equal "Done" closed-reason)))
        (fset 'beads-issue-at-point old-at-point)
        (fset 'beads-command-close! old-close)
        (fset 'read-string old-read)
        (fset 'beads-actions--after-mutation old-after)))))

(ert-deftest beads-actions-test-close-bulk ()
  "Close should iterate over marked issues."
  :tags '(:unit)
  (with-temp-buffer
    (setq-local beads-list--marked-issues '("bd-1" "bd-2"))
    (let* ((closed-ids nil)
           (vnew-close (lambda (&rest args)
                         (push (car (plist-get args :issue-ids)) closed-ids)
                         nil))
           (vnew-read (lambda (&rest _) "Bulk close"))
           (vnew-after (lambda () nil))
           (old-close (symbol-function 'beads-command-close!))
           (old-read (symbol-function 'read-string))
           (old-after (symbol-function 'beads-actions--after-mutation)))
      (unwind-protect
          (progn
            (fset 'beads-command-close! vnew-close)
            (fset 'read-string vnew-read)
            (fset 'beads-actions--after-mutation vnew-after)
            (beads-actions-close)
            (should (= 2 (length closed-ids)))
            (should (member "bd-1" closed-ids))
            (should (member "bd-2" closed-ids)))
        (fset 'beads-command-close! old-close)
        (fset 'read-string old-read)
        (fset 'beads-actions--after-mutation old-after)))))

;;; beads-actions-claim

(ert-deftest beads-actions-test-claim-single-issue ()
  "Claim should execute immediately without prompt."
  :tags '(:unit)
  (with-temp-buffer
    (setq-local beads-list--marked-issues nil)
    (let* ((claimed-ids nil)
           (vnew-at-point (lambda () "bd-42"))
           (vnew-update (lambda (&rest args)
                          (setq claimed-ids (plist-get args :issue-ids))
                          nil))
           (vnew-after (lambda () nil))
           (old-at-point (symbol-function 'beads-issue-at-point))
           (old-update (symbol-function 'beads-command-update!))
           (old-after (symbol-function 'beads-actions--after-mutation)))
      (unwind-protect
          (progn
            (fset 'beads-issue-at-point vnew-at-point)
            (fset 'beads-command-update! vnew-update)
            (fset 'beads-actions--after-mutation vnew-after)
            (beads-actions-claim)
            (should (equal '("bd-42") claimed-ids)))
        (fset 'beads-issue-at-point old-at-point)
        (fset 'beads-command-update! old-update)
        (fset 'beads-actions--after-mutation old-after)))))

;;; beads-actions-set-status

(ert-deftest beads-actions-test-set-status-single ()
  "Set-status should prompt and call update! with status."
  :tags '(:unit)
  (with-temp-buffer
    (setq-local beads-list--marked-issues nil)
    (let* ((updated-status nil)
           (vnew-at-point (lambda () "bd-42"))
           (vnew-update (lambda (&rest args)
                          (setq updated-status (plist-get args :status))
                          nil))
           (vnew-read (lambda (&rest _) "in_progress"))
           (vnew-after (lambda () nil))
           (old-at-point (symbol-function 'beads-issue-at-point))
           (old-update (symbol-function 'beads-command-update!))
           (old-read (symbol-function 'completing-read))
           (old-after (symbol-function 'beads-actions--after-mutation)))
      (unwind-protect
          (progn
            (fset 'beads-issue-at-point vnew-at-point)
            (fset 'beads-command-update! vnew-update)
            (fset 'completing-read vnew-read)
            (fset 'beads-actions--after-mutation vnew-after)
            (beads-actions-set-status)
            (should (equal "in_progress" updated-status)))
        (fset 'beads-issue-at-point old-at-point)
        (fset 'beads-command-update! old-update)
        (fset 'completing-read old-read)
        (fset 'beads-actions--after-mutation old-after)))))

;;; beads-actions-set-priority

(ert-deftest beads-actions-test-set-priority-single ()
  "Set-priority should prompt and call update! with priority."
  :tags '(:unit)
  (with-temp-buffer
    (setq-local beads-list--marked-issues nil)
    (let* ((updated-priority nil)
           (vnew-at-point (lambda () "bd-42"))
           (vnew-update (lambda (&rest args)
                          (setq updated-priority (plist-get args :priority))
                          nil))
           (vnew-read (lambda (&rest _) "1 - High"))
           (vnew-after (lambda () nil))
           (old-at-point (symbol-function 'beads-issue-at-point))
           (old-update (symbol-function 'beads-command-update!))
           (old-read (symbol-function 'completing-read))
           (old-after (symbol-function 'beads-actions--after-mutation)))
      (unwind-protect
          (progn
            (fset 'beads-issue-at-point vnew-at-point)
            (fset 'beads-command-update! vnew-update)
            (fset 'completing-read vnew-read)
            (fset 'beads-actions--after-mutation vnew-after)
            (beads-actions-set-priority)
            (should (equal 1 updated-priority)))
        (fset 'beads-issue-at-point old-at-point)
        (fset 'beads-command-update! old-update)
        (fset 'completing-read old-read)
        (fset 'beads-actions--after-mutation old-after)))))

;;; beads-actions-reopen

(ert-deftest beads-actions-test-reopen-single ()
  "Reopen should prompt for reason and call reopen!."
  :tags '(:unit)
  (with-temp-buffer
    (setq-local beads-list--marked-issues nil)
    (let* ((reopened-ids nil)
           (vnew-at-point (lambda () "bd-42"))
           (vnew-reopen (lambda (&rest args)
                          (setq reopened-ids (plist-get args :issue-ids))
                          nil))
           (vnew-read (lambda (&rest _) "Needs more work"))
           (vnew-after (lambda () nil))
           (old-at-point (symbol-function 'beads-issue-at-point))
           (old-reopen (symbol-function 'beads-command-reopen!))
           (old-read (symbol-function 'read-string))
           (old-after (symbol-function 'beads-actions--after-mutation)))
      (unwind-protect
          (progn
            (fset 'beads-issue-at-point vnew-at-point)
            (fset 'beads-command-reopen! vnew-reopen)
            (fset 'read-string vnew-read)
            (fset 'beads-actions--after-mutation vnew-after)
            (beads-actions-reopen)
            (should (equal '("bd-42") reopened-ids)))
        (fset 'beads-issue-at-point old-at-point)
        (fset 'beads-command-reopen! old-reopen)
        (fset 'read-string old-read)
        (fset 'beads-actions--after-mutation old-after)))))

;;; beads-actions--refresh

(ert-deftest beads-actions-test-refresh-calls-list-refresh ()
  "Refresh should call beads-list-refresh in list-mode buffers."
  :tags '(:unit)
  (with-temp-buffer
    (setq major-mode 'beads-list-mode)
    (let* ((refreshed nil)
           (vnew (lambda () (setq refreshed t)))
           (old (symbol-function 'beads-list-refresh)))
      (unwind-protect
          (progn
            (fset 'beads-list-refresh vnew)
            (beads-actions--refresh)
            (should refreshed))
        (fset 'beads-list-refresh old)))))

(ert-deftest beads-actions-test-refresh-calls-show-refresh ()
  "Refresh should call beads-refresh-show in show-mode buffers."
  :tags '(:unit)
  (with-temp-buffer
    (let* ((refreshed nil)
           (vnew (lambda () (setq refreshed t)))
           (old (symbol-function 'beads-refresh-show)))
      (unwind-protect
          (progn
            (setq major-mode 'beads-show-mode)
            (fset 'beads-refresh-show vnew)
            (beads-actions--refresh)
            (should refreshed))
        (fset 'beads-refresh-show old)))))

;;; Keybindings

(ert-deftest beads-actions-test-list-mode-d-binding ()
  "d should be bound to beads-actions-close in list-mode."
  :tags '(:unit)
  (should (eq 'beads-actions-close
              (lookup-key beads-list-mode-map (kbd "d")))))

(ert-deftest beads-actions-test-list-mode-C-binding ()
  "C should be bound to beads-actions-claim in list-mode."
  :tags '(:unit)
  (should (eq 'beads-actions-claim
              (lookup-key beads-list-mode-map (kbd "C")))))

(ert-deftest beads-actions-test-list-mode-s-binding ()
  "s should be bound to beads-actions-set-status in list-mode."
  :tags '(:unit)
  (should (eq 'beads-actions-set-status
              (lookup-key beads-list-mode-map (kbd "s")))))

(ert-deftest beads-actions-test-list-mode-hash-binding ()
  "# should be bound to beads-actions-set-priority in list-mode."
  :tags '(:unit)
  (should (eq 'beads-actions-set-priority
              (lookup-key beads-list-mode-map (kbd "#")))))

(ert-deftest beads-actions-test-show-mode-d-binding ()
  "d should be bound to beads-actions-close in show-mode."
  :tags '(:unit)
  (should (eq 'beads-actions-close
              (lookup-key beads-show-mode-map (kbd "d")))))

(ert-deftest beads-actions-test-show-mode-C-binding ()
  "C should be bound to beads-actions-claim in show-mode."
  :tags '(:unit)
  (should (eq 'beads-actions-claim
              (lookup-key beads-show-mode-map (kbd "C")))))

(ert-deftest beads-actions-test-show-mode-s-binding ()
  "s should be bound to beads-actions-set-status in show-mode."
  :tags '(:unit)
  (should (eq 'beads-actions-set-status
              (lookup-key beads-show-mode-map (kbd "s")))))

(ert-deftest beads-actions-test-show-mode-actions-binding ()
  "? should be bound to beads-show-actions in show-mode."
  :tags '(:unit)
  (should (eq 'beads-show-actions
              (lookup-key beads-show-mode-map (kbd "?")))))

(provide 'beads-actions-test)

;;; beads-actions-test.el ends here
