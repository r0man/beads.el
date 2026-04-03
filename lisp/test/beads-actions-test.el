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
    (let* ((close-called nil)
           (close-cmd nil))
      (cl-letf (((symbol-function 'beads-issue-at-point)
                 (lambda () "bd-42"))
                ((symbol-function 'beads-command-execute)
                 (lambda (cmd)
                   (when (cl-typep cmd 'beads-command-close)
                     (setq close-called t)
                     (setq close-cmd cmd))
                   nil))
                ((symbol-function 'read-string)
                 (lambda (&rest _) "Done"))
                ((symbol-function 'beads-actions--after-mutation)
                 (lambda () nil)))
        (beads-actions-close)
        (should close-called)
        (should (equal '("bd-42") (oref close-cmd issue-ids)))
        (should (equal "Done" (oref close-cmd reason)))))))

(ert-deftest beads-actions-test-close-bulk ()
  "Close should iterate over marked issues."
  :tags '(:unit)
  (with-temp-buffer
    (setq-local beads-list--marked-issues '("bd-1" "bd-2"))
    (let* ((closed-ids nil))
      (cl-letf (((symbol-function 'beads-command-execute)
                 (lambda (cmd)
                   (when (cl-typep cmd 'beads-command-close)
                     (push (car (oref cmd issue-ids)) closed-ids))
                   nil))
                ((symbol-function 'read-string)
                 (lambda (&rest _) "Bulk close"))
                ((symbol-function 'beads-actions--after-mutation)
                 (lambda () nil)))
        (beads-actions-close)
        (should (= 2 (length closed-ids)))
        (should (member "bd-1" closed-ids))
        (should (member "bd-2" closed-ids))))))

;;; beads-actions-claim

(ert-deftest beads-actions-test-claim-single-issue ()
  "Claim should execute immediately without prompt."
  :tags '(:unit)
  (with-temp-buffer
    (setq-local beads-list--marked-issues nil)
    (let* ((claim-cmd nil))
      (cl-letf (((symbol-function 'beads-issue-at-point)
                 (lambda () "bd-42"))
                ((symbol-function 'beads-command-execute)
                 (lambda (cmd)
                   (when (cl-typep cmd 'beads-command-update)
                     (setq claim-cmd cmd))
                   nil))
                ((symbol-function 'beads-actions--after-mutation)
                 (lambda () nil)))
        (beads-actions-claim)
        (should claim-cmd)
        (should (equal '("bd-42") (oref claim-cmd issue-ids)))))))

;;; beads-actions-set-status

(ert-deftest beads-actions-test-set-status-single ()
  "Set-status should prompt and call update! with status."
  :tags '(:unit)
  (with-temp-buffer
    (setq-local beads-list--marked-issues nil)
    (let* ((update-cmd nil))
      (cl-letf (((symbol-function 'beads-issue-at-point)
                 (lambda () "bd-42"))
                ((symbol-function 'beads-command-execute)
                 (lambda (cmd)
                   (when (cl-typep cmd 'beads-command-update)
                     (setq update-cmd cmd))
                   nil))
                ((symbol-function 'completing-read)
                 (lambda (&rest _) "in_progress"))
                ((symbol-function 'beads-actions--after-mutation)
                 (lambda () nil)))
        (beads-actions-set-status)
        (should update-cmd)
        (should (equal "in_progress" (oref update-cmd status)))))))

;;; beads-actions-set-priority

(ert-deftest beads-actions-test-set-priority-single ()
  "Set-priority should prompt and call update! with priority."
  :tags '(:unit)
  (with-temp-buffer
    (setq-local beads-list--marked-issues nil)
    (let* ((update-cmd nil))
      (cl-letf (((symbol-function 'beads-issue-at-point)
                 (lambda () "bd-42"))
                ((symbol-function 'beads-command-execute)
                 (lambda (cmd)
                   (when (cl-typep cmd 'beads-command-update)
                     (setq update-cmd cmd))
                   nil))
                ((symbol-function 'completing-read)
                 (lambda (&rest _) "1 - High"))
                ((symbol-function 'beads-actions--after-mutation)
                 (lambda () nil)))
        (beads-actions-set-priority)
        (should update-cmd)
        (should (equal 1 (oref update-cmd priority)))))))

;;; beads-actions-reopen

(ert-deftest beads-actions-test-reopen-single ()
  "Reopen should prompt for reason and call reopen!."
  :tags '(:unit)
  (with-temp-buffer
    (setq-local beads-list--marked-issues nil)
    (let* ((reopen-cmd nil))
      (cl-letf (((symbol-function 'beads-issue-at-point)
                 (lambda () "bd-42"))
                ((symbol-function 'beads-command-execute)
                 (lambda (cmd)
                   (when (cl-typep cmd 'beads-command-reopen)
                     (setq reopen-cmd cmd))
                   nil))
                ((symbol-function 'read-string)
                 (lambda (&rest _) "Needs more work"))
                ((symbol-function 'beads-actions--after-mutation)
                 (lambda () nil)))
        (beads-actions-reopen)
        (should reopen-cmd)
        (should (equal '("bd-42") (oref reopen-cmd issue-ids)))))))

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
