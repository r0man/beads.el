;;; beads-log-test.el --- Tests for beads-log -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is part of beads.el.

;;; Commentary:

;; Unit tests for the durable command log: append, bounding/truncation,
;; rendering, and the async hook.

;;; Code:

(require 'ert)
(require 'beads-log)
(require 'beads-command)
(require 'beads-command-list)

;;; Recording

(ert-deftest beads-log-test-record-appends-entry ()
  "A recorded entry carries every field an async failure needs."
  :tags '(:unit)
  (let ((beads-log--entries nil)
        (beads-log-buffer-name "*beads-log-test-append*"))
    (unwind-protect
        (progn
          (beads-log-record :status 'failed
                            :store "/tmp/proj"
                            :argv '("bd" "list" "--status" "open")
                            :exit-code 1
                            :duration 0.25
                            :stderr "boom\n"
                            :error "Command failed with exit code 1")
          (should (= 1 (length beads-log--entries)))
          (let ((entry (car beads-log--entries)))
            (should (eq (plist-get entry :status) 'failed))
            (should (equal (plist-get entry :store) "/tmp/proj"))
            (should (equal (plist-get entry :argv)
                           "bd list --status open"))
            (should (= (plist-get entry :exit-code) 1))
            (should (= (plist-get entry :duration) 0.25))
            (should (equal (plist-get entry :stderr) "boom\n"))
            (should (equal (plist-get entry :error)
                           "Command failed with exit code 1"))))
      (when-let* ((buf (get-buffer beads-log-buffer-name)))
        (kill-buffer buf)))))

(ert-deftest beads-log-test-record-defaults-status-to-ok ()
  "A record without :status is treated as success."
  :tags '(:unit)
  (let ((beads-log--entries nil)
        (beads-log-buffer-name "*beads-log-test-default*"))
    (unwind-protect
        (progn
          (beads-log-record :argv "bd version")
          (should (eq (plist-get (car beads-log--entries) :status) 'ok))
          (should (listp (plist-get (car beads-log--entries) :timestamp))))
      (when-let* ((buf (get-buffer beads-log-buffer-name)))
        (kill-buffer buf)))))

(ert-deftest beads-log-test-record-truncates-to-max-entries ()
  "The log never grows past `beads-log-max-entries'."
  :tags '(:unit)
  (let ((beads-log--entries nil)
        (beads-log-max-entries 3)
        (beads-log-buffer-name "*beads-log-test-max*"))
    (unwind-protect
        (progn
          (dotimes (i 5)
            (beads-log-record :argv (format "cmd-%d" i)))
          (should (= 3 (length beads-log--entries)))
          ;; Newest first: cmd-4, cmd-3, cmd-2.
          (should (equal (plist-get (car beads-log--entries) :argv) "cmd-4"))
          (should (equal (plist-get (car (last beads-log--entries)) :argv)
                         "cmd-2")))
      (when-let* ((buf (get-buffer beads-log-buffer-name)))
        (kill-buffer buf)))))

(ert-deftest beads-log-test-record-truncates-long-stderr ()
  "Long stderr is truncated with a marker."
  :tags '(:unit)
  (let ((beads-log--entries nil)
        (beads-log-max-stderr-chars 10)
        (beads-log-buffer-name "*beads-log-test-stderr*"))
    (unwind-protect
        (progn
          (beads-log-record :status 'failed :argv "bd x"
                            :stderr (make-string 50 ?x))
          (let ((stderr (plist-get (car beads-log--entries) :stderr)))
            (should (string-match-p "truncated" stderr))
            (should (< (length stderr) 50))))
      (when-let* ((buf (get-buffer beads-log-buffer-name)))
        (kill-buffer buf)))))

(ert-deftest beads-log-test-clear-entries ()
  "`beads-log-clear-entries' discards everything."
  :tags '(:unit)
  (let ((beads-log--entries nil)
        (beads-log-buffer-name "*beads-log-test-clear*"))
    (unwind-protect
        (progn
          (beads-log-record :argv "bd x")
          (should beads-log--entries)
          (beads-log-clear-entries)
          (should-not (beads-log-entries)))
      (when-let* ((buf (get-buffer beads-log-buffer-name)))
        (kill-buffer buf)))))

;;; Rendering

(ert-deftest beads-log-test-refresh-renders-fields ()
  "The buffer shows argv, store, exit code, and stderr."
  :tags '(:unit)
  (let ((beads-log--entries nil)
        (beads-log-buffer-name "*beads-log-test-render*"))
    (unwind-protect
        (progn
          (beads-log-record :status 'failed
                            :store "/tmp/proj"
                            :argv '("bd" "list")
                            :exit-code 1
                            :stderr "boom")
          (with-temp-buffer
            (beads-log-mode)
            (beads-log--refresh)
            (let ((text (buffer-string)))
              (should (string-match-p "bd list" text))
              (should (string-match-p "/tmp/proj" text))
              (should (string-match-p "exit 1" text))
              (should (string-match-p "boom" text))
              (should (string-match-p "FAILED" text)))))
      (when-let* ((buf (get-buffer beads-log-buffer-name)))
        (kill-buffer buf)))))

(ert-deftest beads-log-test-refresh-renders-empty-state ()
  "An empty log renders a placeholder, not an error."
  :tags '(:unit)
  (let ((beads-log--entries nil)
        (beads-log-buffer-name "*beads-log-test-empty*"))
    (unwind-protect
        (progn
          (with-temp-buffer
            (beads-log-mode)
            (beads-log--refresh)
            (should (string-match-p "No async bd invocations" (buffer-string)))))
      (when-let* ((buf (get-buffer beads-log-buffer-name)))
        (kill-buffer buf)))))

(ert-deftest beads-log-test-record-refreshes-live-buffer ()
  "Recording into a live log buffer updates it in place."
  :tags '(:unit)
  (let ((beads-log--entries nil)
        (beads-log-buffer-name "*beads-log-test-live*"))
    (unwind-protect
        (progn
          (with-current-buffer (get-buffer-create beads-log-buffer-name)
            (beads-log-mode)
            (beads-log--refresh))
          (beads-log-record :argv "bd version")
          (with-current-buffer beads-log-buffer-name
            (should (string-match-p "bd version" (buffer-string)))))
      (when-let* ((buf (get-buffer beads-log-buffer-name)))
        (kill-buffer buf)))))

(ert-deftest beads-log-test-command-opens-buffer ()
  "`beads-log' opens a buffer in `beads-log-mode'."
  :tags '(:unit)
  (let ((beads-log--entries nil)
        (beads-log-buffer-name "*beads-log-test-open*"))
    (unwind-protect
        (progn
          (beads-log)
          (with-current-buffer beads-log-buffer-name
            (should (derived-mode-p 'beads-log-mode))))
      (when-let* ((buf (get-buffer beads-log-buffer-name)))
        (kill-buffer buf)))))

;;; Async hook

(ert-deftest beads-log-test-async-failure-recorded ()
  "A failing async bd invocation leaves a log entry."
  :tags '(:unit)
  (let ((beads-log--entries nil)
        (beads-log-buffer-name "*beads-log-test-async*")
        (cmd (beads-command-list :json nil)))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'beads-command-validate)
                     (lambda (_cmd) nil))
                    ((symbol-function 'beads-command-line)
                     (lambda (_cmd) '("sh" "-c" "echo boom >&2; exit 3"))))
            (beads-command-execute-async cmd #'ignore #'ignore)
            (let ((deadline (+ (float-time) 5.0)))
              (while (and (null beads-log--entries) (< (float-time) deadline))
                (sit-for 0.1))))
          (should beads-log--entries)
          (let ((entry (car beads-log--entries)))
            (should (eq (plist-get entry :status) 'failed))
            (should (= (plist-get entry :exit-code) 3))
            (should (string-match-p "boom" (plist-get entry :stderr)))
            (should (string-match-p "exit code 3" (plist-get entry :error)))))
      (when-let* ((buf (get-buffer beads-log-buffer-name)))
        (kill-buffer buf)))))

(provide 'beads-log-test)
;;; beads-log-test.el ends here
