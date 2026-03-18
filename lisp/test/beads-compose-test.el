;;; beads-compose-test.el --- Tests for beads-compose.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; ERT tests for beads-compose.el: buffer-based editing for issue
;; creation, editing, and commenting (Pattern 3).

;;; Code:

(require 'ert)
(require 'beads-compose)

;;; Mode Tests

(ert-deftest beads-compose-test-mode-defined ()
  "Verify beads-compose-mode is defined."
  (should (fboundp 'beads-compose-mode)))

(ert-deftest beads-compose-test-mode-activates ()
  "Verify beads-compose-mode activates in a buffer."
  (with-temp-buffer
    (beads-compose-mode)
    (should (eq major-mode 'beads-compose-mode))))

(ert-deftest beads-compose-test-mode-keybindings ()
  "Verify C-c C-c and C-c C-k are bound."
  (with-temp-buffer
    (beads-compose-mode)
    (should (commandp (lookup-key beads-compose-mode-map (kbd "C-c C-c"))))
    (should (commandp (lookup-key beads-compose-mode-map (kbd "C-c C-k"))))))

(ert-deftest beads-compose-test-mode-metadata-key ()
  "Verify C-c C-a is bound to metadata transient."
  (with-temp-buffer
    (beads-compose-mode)
    (should (commandp
             (lookup-key beads-compose-mode-map (kbd "C-c C-a"))))))

;;; Title/Body Extraction Tests

(ert-deftest beads-compose-test-extract-title-from-first-line ()
  "Verify title is extracted from first line."
  (with-temp-buffer
    (insert "My Issue Title\n\nSome description here.")
    (should (equal (beads-compose--extract-title) "My Issue Title"))))

(ert-deftest beads-compose-test-extract-title-with-hash ()
  "Verify title strips leading '# ' marker."
  (with-temp-buffer
    (insert "# My Issue Title\n\nDescription.")
    (should (equal (beads-compose--extract-title) "My Issue Title"))))

(ert-deftest beads-compose-test-extract-title-trims-whitespace ()
  "Verify title is trimmed."
  (with-temp-buffer
    (insert "  My Title  \n\nBody.")
    (should (equal (beads-compose--extract-title) "My Title"))))

(ert-deftest beads-compose-test-extract-title-empty ()
  "Verify empty buffer returns nil title."
  (with-temp-buffer
    (should (null (beads-compose--extract-title)))))

(ert-deftest beads-compose-test-extract-body ()
  "Verify body is everything after first line."
  (with-temp-buffer
    (insert "Title\n\nFirst paragraph.\n\nSecond paragraph.")
    (should (equal (beads-compose--extract-body)
                   "First paragraph.\n\nSecond paragraph."))))

(ert-deftest beads-compose-test-extract-body-no-blank-line ()
  "Verify body with no blank line after title."
  (with-temp-buffer
    (insert "Title\nBody starts here.")
    (should (equal (beads-compose--extract-body)
                   "Body starts here."))))

(ert-deftest beads-compose-test-extract-body-empty ()
  "Verify empty body returns nil."
  (with-temp-buffer
    (insert "Title only")
    (should (null (beads-compose--extract-body)))))

(ert-deftest beads-compose-test-extract-body-title-only-with-newline ()
  "Verify body is nil when only title with trailing newline."
  (with-temp-buffer
    (insert "Title\n")
    (should (null (beads-compose--extract-body)))))

;;; Buffer-Local Metadata Tests

(ert-deftest beads-compose-test-metadata-defaults ()
  "Verify metadata defaults are set in compose buffers."
  (with-temp-buffer
    (beads-compose-mode)
    (should (equal beads-compose--type "task"))
    (should (= beads-compose--priority 2))
    (should (null beads-compose--assignee))
    (should (null beads-compose--labels))
    (should (null beads-compose--parent))))

(ert-deftest beads-compose-test-metadata-independent ()
  "Verify metadata is buffer-local."
  (let (buf1 buf2)
    (setq buf1 (generate-new-buffer " *test-compose-1*"))
    (setq buf2 (generate-new-buffer " *test-compose-2*"))
    (unwind-protect
        (progn
          (with-current-buffer buf1
            (beads-compose-mode)
            (setq beads-compose--type "bug"))
          (with-current-buffer buf2
            (beads-compose-mode)
            (should (equal beads-compose--type "task"))))
      (kill-buffer buf1)
      (kill-buffer buf2))))

;;; Create Buffer Tests

(ert-deftest beads-compose-test-create-opens-buffer ()
  "Verify beads-compose-create opens a compose buffer."
  (cl-letf (((symbol-function 'beads-git-get-project-name)
             (lambda () "testproj"))
            ((symbol-function 'beads-check-executable)
             (lambda () t)))
    (let ((buf (beads-compose-create)))
      (unwind-protect
          (progn
            (should (buffer-live-p buf))
            (should (string-match-p "\\*beads-create\\[testproj\\]\\*"
                                    (buffer-name buf)))
            (with-current-buffer buf
              (should (eq major-mode 'beads-compose-mode))
              (should (eq beads-compose--action 'create))))
        (kill-buffer buf)))))

(ert-deftest beads-compose-test-create-reuses-buffer ()
  "Verify beads-compose-create reuses existing buffer."
  (cl-letf (((symbol-function 'beads-git-get-project-name)
             (lambda () "testproj"))
            ((symbol-function 'beads-check-executable)
             (lambda () t)))
    (let ((buf1 (beads-compose-create))
          buf2)
      (unwind-protect
          (progn
            (setq buf2 (beads-compose-create))
            (should (eq buf1 buf2)))
        (when (buffer-live-p buf1) (kill-buffer buf1))
        (when (and buf2 (buffer-live-p buf2)
                   (not (eq buf1 buf2)))
          (kill-buffer buf2))))))

;;; Submit Tests

(ert-deftest beads-compose-test-submit-create ()
  "Verify C-c C-c in create buffer calls beads-command-create!."
  (let ((created-args nil))
    (cl-letf (((symbol-function 'beads-command-create!)
               (lambda (&rest args)
                 (setq created-args args)
                 (beads-issue :id "bd-99" :title "Test"
                              :status "open" :priority 2)))
              ((symbol-function 'beads-git-get-project-name)
               (lambda () "testproj"))
              ((symbol-function 'beads-check-executable)
               (lambda () t))
              ((symbol-function 'beads--invalidate-completion-cache)
               #'ignore)
              ((symbol-function 'beads-list-refresh-all)
               #'ignore))
      (let ((buf (beads-compose-create)))
        (unwind-protect
            (with-current-buffer buf
              (erase-buffer)
              (insert "Test Issue\n\nThis is the description.")
              (setq beads-compose--type "bug")
              (setq beads-compose--priority 1)
              (beads-compose-submit)
              (should created-args)
              (should (equal (plist-get created-args :title)
                             "Test Issue"))
              (should (equal (plist-get created-args :description)
                             "This is the description."))
              (should (equal (plist-get created-args :issue-type)
                             "bug"))
              (should (equal (plist-get created-args :priority) "1")))
          (when (buffer-live-p buf) (kill-buffer buf)))))))

(ert-deftest beads-compose-test-submit-requires-title ()
  "Verify submit signals error when title is empty."
  (cl-letf (((symbol-function 'beads-git-get-project-name)
             (lambda () "testproj"))
            ((symbol-function 'beads-check-executable)
             (lambda () t)))
    (let ((buf (beads-compose-create)))
      (unwind-protect
          (with-current-buffer buf
            (erase-buffer)
            (insert "\n\nNo title here.")
            (should-error (beads-compose-submit) :type 'user-error))
        (when (buffer-live-p buf) (kill-buffer buf))))))

;;; Edit Buffer Tests

(ert-deftest beads-compose-test-edit-opens-buffer ()
  "Verify beads-compose-edit opens a compose buffer for editing."
  (cl-letf (((symbol-function 'beads-git-get-project-name)
             (lambda () "testproj"))
            ((symbol-function 'beads-check-executable)
             (lambda () t)))
    (let ((buf (beads-compose-edit "bd-42" "Old description")))
      (unwind-protect
          (progn
            (should (buffer-live-p buf))
            (should (string-match-p "beads-edit.*bd-42"
                                    (buffer-name buf)))
            (with-current-buffer buf
              (should (eq beads-compose--action 'edit))
              (should (equal beads-compose--issue-id "bd-42"))
              ;; Buffer should contain the old description
              (should (string-match-p "Old description"
                                      (buffer-string)))))
        (kill-buffer buf)))))

(ert-deftest beads-compose-test-submit-edit ()
  "Verify C-c C-c in edit buffer calls beads-command-update!."
  (let ((update-args nil))
    (cl-letf (((symbol-function 'beads-command-execute)
               (lambda (cmd)
                 (setq update-args cmd)
                 (let ((exec (beads-command-execution)))
                   (oset exec exit-code 0)
                   (oset exec result nil)
                   exec)))
              ((symbol-function 'beads-git-get-project-name)
               (lambda () "testproj"))
              ((symbol-function 'beads-check-executable)
               (lambda () t))
              ((symbol-function 'beads--invalidate-completion-cache)
               #'ignore)
              ((symbol-function 'beads-list-refresh-all)
               #'ignore))
      (let ((buf (beads-compose-edit "bd-42" "Old text")))
        (unwind-protect
            (with-current-buffer buf
              (erase-buffer)
              (insert "New description text")
              (beads-compose-submit)
              (should update-args)
              (should (object-of-class-p update-args
                                         'beads-command-update)))
          (when (buffer-live-p buf) (kill-buffer buf)))))))

;;; Comment Buffer Tests

(ert-deftest beads-compose-test-comment-opens-buffer ()
  "Verify beads-compose-comment opens a buffer for commenting."
  (cl-letf (((symbol-function 'beads-git-get-project-name)
             (lambda () "testproj"))
            ((symbol-function 'beads-check-executable)
             (lambda () t)))
    (let ((buf (beads-compose-comment "bd-42")))
      (unwind-protect
          (progn
            (should (buffer-live-p buf))
            (should (string-match-p "beads-comment.*bd-42"
                                    (buffer-name buf)))
            (with-current-buffer buf
              (should (eq beads-compose--action 'comment))
              (should (equal beads-compose--issue-id "bd-42"))))
        (kill-buffer buf)))))

;;; Cancel Tests

(ert-deftest beads-compose-test-cancel-kills-buffer ()
  "Verify C-c C-k kills the compose buffer."
  (cl-letf (((symbol-function 'beads-git-get-project-name)
             (lambda () "testproj"))
            ((symbol-function 'beads-check-executable)
             (lambda () t))
            ((symbol-function 'y-or-n-p)
             (lambda (_prompt) t)))
    (let ((buf (beads-compose-create)))
      (with-current-buffer buf
        (beads-compose-cancel))
      (should-not (buffer-live-p buf)))))

(provide 'beads-compose-test)
;;; beads-compose-test.el ends here
