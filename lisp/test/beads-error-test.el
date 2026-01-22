;;; beads-error-test.el --- Tests for beads-error.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; This file is part of beads.el.

;;; Commentary:

;; Tests for the error extraction utilities in beads-error.el.

;;; Code:

(require 'ert)
(require 'beads-error)

;;; beads-error-extract-message tests

(ert-deftest beads-error-test-extract-message-with-stderr ()
  "Test extracting message from error with stderr."
  :tags '(:unit)
  (let ((err '(beads-command-error
               "Command failed with exit code 1"
               :command "bd dep add bd-1 bd-2"
               :exit-code 1
               :stdout ""
               :stderr "Error: Adding dependency would create a cycle")))
    (should (string= (beads-error-extract-message err)
                     "Error: Adding dependency would create a cycle"))))

(ert-deftest beads-error-test-extract-message-stderr-with-whitespace ()
  "Test that stderr is trimmed of whitespace."
  :tags '(:unit)
  (let ((err '(beads-command-error
               "Command failed"
               :command "bd test"
               :exit-code 1
               :stdout ""
               :stderr "  Error message with spaces  \n")))
    (should (string= (beads-error-extract-message err)
                     "Error message with spaces"))))

(ert-deftest beads-error-test-extract-message-fallback-to-stdout ()
  "Test falling back to stdout when stderr is empty."
  :tags '(:unit)
  (let ((err '(beads-command-error
               "Command failed"
               :command "bd test"
               :exit-code 1
               :stdout "Output from command"
               :stderr "")))
    (should (string= (beads-error-extract-message err)
                     "Output from command"))))

(ert-deftest beads-error-test-extract-message-fallback-to-message ()
  "Test falling back to message when both stdout and stderr are empty."
  :tags '(:unit)
  (let ((err '(beads-command-error
               "Command failed with exit code 1"
               :command "bd test"
               :exit-code 1
               :stdout ""
               :stderr "")))
    (should (string= (beads-error-extract-message err)
                     "Command failed with exit code 1"))))

(ert-deftest beads-error-test-extract-message-prefers-stderr ()
  "Test that stderr is preferred over stdout."
  :tags '(:unit)
  (let ((err '(beads-command-error
               "Command failed"
               :command "bd test"
               :exit-code 1
               :stdout "Some stdout output"
               :stderr "Actual error message")))
    (should (string= (beads-error-extract-message err)
                     "Actual error message"))))

(ert-deftest beads-error-test-extract-message-whitespace-only-stderr ()
  "Test that whitespace-only stderr falls back to stdout."
  :tags '(:unit)
  (let ((err '(beads-command-error
               "Command failed"
               :command "bd test"
               :exit-code 1
               :stdout "Fallback output"
               :stderr "   \n  ")))
    (should (string= (beads-error-extract-message err)
                     "Fallback output"))))

(ert-deftest beads-error-test-extract-message-nil-stderr ()
  "Test handling nil stderr."
  :tags '(:unit)
  (let ((err '(beads-command-error
               "Command failed"
               :command "bd test"
               :exit-code 1
               :stdout "Output"
               :stderr nil)))
    (should (string= (beads-error-extract-message err)
                     "Output"))))

(ert-deftest beads-error-test-extract-message-both-nil ()
  "Test handling both stdout and stderr nil."
  :tags '(:unit)
  (let ((err '(beads-command-error
               "Fallback message"
               :command "bd test"
               :exit-code 1
               :stdout nil
               :stderr nil)))
    (should (string= (beads-error-extract-message err)
                     "Fallback message"))))

(provide 'beads-error-test)
;;; beads-error-test.el ends here
