#!/usr/bin/env -S emacs --script
;; Test script to debug beads-update issue

(add-to-list 'load-path (expand-file-name "lisp" default-directory))
(require 'package)
(package-initialize)

(require 'transient)
(require 'beads)
(require 'beads-update)

;; Enable debug on error
(setq debug-on-error t)

;; Test the function calls
(message "Testing beads-update--fetch-issue...")
(let ((issue-id "beads.el-1du"))
  (message "Issue ID: %s" issue-id)
  (condition-case err
      (let ((issue (beads-update--fetch-issue issue-id)))
        (message "Success! Issue: %S" issue)
        (message "Issue title: %s" (oref issue title)))
    (error
     (message "ERROR: %S" err))))
