;;; beads-status-test.el --- Tests for the beads status buffer -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; ERT tests for the hand-built beads status buffer (REQ-001, REQ-002):
;;   - the `beads-status' entry point and the `beads' front door
;;   - `beads-status-mode' derivation and the navigation contract
;;   - section registration and the async loader contract
;;   - the four section render states (loading/empty/error/populated)

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'vui)
(require 'beads-status)
(require 'beads-section)
(require 'beads-types)

;;; Entry Point

(ert-deftest beads-status-test-entry-point-defined ()
  "`beads-status' is a callable interactive command."
  :tags '(:unit)
  (should (fboundp 'beads-status))
  (should (commandp 'beads-status)))

(ert-deftest beads-status-test-beads-front-door-defined ()
  "`beads' is a callable interactive command (REQ-001)."
  :tags '(:unit)
  (should (fboundp 'beads))
  (should (commandp 'beads)))

(ert-deftest beads-status-test-beads-opens-status ()
  "`M-x beads' forwards to `beads-status'."
  :tags '(:unit)
  (let ((called nil))
    (cl-letf (((symbol-function 'beads-status)
               (lambda (&rest _) (setq called t))))
      (beads))
    (should called)))

;;; Mode and Navigation Contract

(ert-deftest beads-status-test-mode-derived-from-section ()
  "`beads-status-mode' derives from `beads-section-mode'."
  :tags '(:unit)
  (with-temp-buffer
    (beads-status-mode)
    (should (derived-mode-p 'vui-mode))
    (should (derived-mode-p 'beads-section-mode))))

(defmacro beads-status-test--key (key)
  "Return the binding of KEY in `beads-status-mode-map'."
  `(lookup-key beads-status-mode-map (kbd ,key)))

(ert-deftest beads-status-test-navigation-contract ()
  "The REQ-002 keys resolve in `beads-status-mode-map'."
  :tags '(:unit)
  (should (eq (beads-status-test--key "RET") #'beads-section-visit-issue))
  (should (eq (beads-status-test--key "TAB") #'beads-thing-forward))
  (should (eq (beads-status-test--key "<backtab>") #'beads-thing-backward))
  (should (eq (beads-status-test--key "SPC") #'beads-thing-toggle))
  (should (eq (beads-status-test--key "q") #'quit-window))
  (should (eq (beads-status-test--key "g") #'beads-status-refresh))
  (should (eq (beads-status-test--key "?") #'beads-dispatch)))

;;; Section Registration

(ert-deftest beads-status-test-builtin-sections ()
  "The status buffer ships its four built-in async sections."
  :tags '(:unit)
  (should (equal (mapcar (lambda (s) (plist-get s :key))
                         beads-status--sections)
                 '(in-flight ready blocked closed))))

(ert-deftest beads-status-test-builtin-sections-have-loaders-and-renderers ()
  "Every built-in section carries a loader and a renderer."
  :tags '(:unit)
  (dolist (section beads-status--sections)
    (should (functionp (plist-get section :loader)))
    (should (functionp (plist-get section :renderer)))))

(ert-deftest beads-status-test-extension-registration ()
  "A section registered via `beads-section-register' joins the board."
  :tags '(:unit)
  (unwind-protect
      (progn
        (beads-section-register 'be-status-ext "Ext"
                                (lambda () "data")
                                (lambda (data) data))
        (should (object-of-class-p
                 (beads-section-spec-for 'be-status-ext)
                 'beads-section-spec))
        (should (memq 'be-status-ext
                      (mapcar (lambda (s) (oref s key))
                              (beads-status--extension-specs)))))
    (remhash 'be-status-ext beads-section--registry))
  (should-not (beads-section-spec-for 'be-status-ext)))

(ert-deftest beads-status-test-sections-hook-preserved ()
  "`beads-status-sections-hook' remains a public extension point."
  :tags '(:unit)
  (should (boundp 'beads-status-sections-hook)))

;;; Async Loader Contract

(ert-deftest beads-status-test-loaders-are-async ()
  "Built-in section loaders fetch through `beads-command-execute-async'."
  :tags '(:unit)
  (let ((async-called 0)
        (sync-called nil))
    (cl-letf (((symbol-function 'beads-command-execute-async)
               (lambda (&rest _) (cl-incf async-called) 'queued))
              ((symbol-function 'beads-command-execute)
               (lambda (&rest _) (setq sync-called t) nil)))
      (dolist (section beads-status--sections)
        (when-let* ((loader (plist-get section :loader))
                    (thunk (funcall loader nil)))
          (funcall thunk #'ignore #'ignore)))
      (should (> async-called 0))
      (should-not sync-called))))

(ert-deftest beads-status-test-buffer-name-local ()
  "Local buffers are named after the project basename."
  :tags '(:unit)
  (should (equal (beads-status--buffer-name-for "/home/me/proj/")
                 "*beads-status<proj>*"))
  (should (equal (beads-status--buffer-name-for nil) "*beads-status*")))

;;; Four Section States

(ert-deftest beads-status-test-section-loading-state ()
  "Pending sections render the loading skeleton."
  :tags '(:unit)
  (should (vui-vnode-p (beads-dashboard--loading-line 'ready))))

(ert-deftest beads-status-test-section-empty-state ()
  "Empty sections render a placeholder line."
  :tags '(:unit)
  (should (vui-vnode-p (beads-dashboard--empty-line "Nothing ready." 'ready))))

(ert-deftest beads-status-test-section-error-state ()
  "Failed sections render an error line without signalling."
  :tags '(:unit)
  (should (vui-vnode-p (beads-dashboard--error-line "boom" 'ready))))

(ert-deftest beads-status-test-section-populated-state ()
  "A populated section renders real issue rows."
  :tags '(:unit)
  (let* ((issue (beads-issue :id "bd-1" :title "Guarded issue"
                             :status "open" :priority 1
                             :issue-type "task"))
         (vnode (beads-dashboard-render-ready (list issue) 'ready)))
    (should (vui-vnode-p vnode))))

(provide 'beads-status-test)
;;; beads-status-test.el ends here