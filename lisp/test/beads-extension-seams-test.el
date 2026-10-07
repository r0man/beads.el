;;; beads-extension-seams-test.el --- Tests for the WI-4 extension seams -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Commentary:

;; Unit tests for the foundation extension seams (design.md §4, WI-4):
;; store resolvers/prefix functions/descriptor, section registry,
;; dashboard section providers, action providers and the after-action
;; hook, menu providers, sling targets and dispatch, the reserved
;; extension keymap and the canonical faces.  Every hook/provider is
;; exercised through the function that consults it, and every seam is
;; checked for the standalone no-op (empty hook) guarantee (REQ-021).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'beads-util)
(require 'beads-section)
(require 'beads-dashboard-sections)
(require 'beads-actions)
(require 'beads-menu)
(require 'beads-sling)
(require 'beads-buffer)
(require 'beads-faces)
(require 'beads-command-list)

;;; Store seams

(ert-deftest beads-extension-seams-store-resolvers ()
  "`beads-store-resolvers' is consulted; empty hook is the no-op."
  :tags '(:unit)
  (should (equal (beads-store-resolve "/srv/rig") "/srv/rig/"))
  (let ((beads-store-resolvers
         (list (lambda (dir) (when (equal dir "/srv/rig/") "/mapped/")))))
    (should (equal (beads-store-resolve "/srv/rig") "/mapped/")))
  (let ((beads-store-resolvers (list (lambda (_dir) nil))))
    (should (equal (beads-store-resolve "/srv/rig") "/srv/rig/"))))

(ert-deftest beads-extension-seams-store-prefix-functions ()
  "`beads-store-prefix-functions' is consulted; empty hook is nil."
  :tags '(:unit)
  (should-not (beads-store-for-prefix "be"))
  (should-not (beads-store-for-prefix nil))
  (let ((beads-store-prefix-functions
         (list (lambda (prefix) (when (equal prefix "be") "/srv/rig/")))))
    (should (equal (beads-store-for-prefix "be") "/srv/rig/"))
    (should-not (beads-store-for-prefix "gc"))))

(ert-deftest beads-extension-seams-store-descriptor ()
  "`beads-store-descriptor' exposes the scoping slots."
  :tags '(:unit)
  (let ((d (beads-store-descriptor-for "/srv/rig"
                                       :label "rig"
                                       :database "/srv/rig/bd.db"
                                       :prefixes '("be"))))
    (should (object-of-class-p d 'beads-store-descriptor))
    (should (equal (oref d root) "/srv/rig/"))
    (should (equal (oref d label) "rig"))
    (should (equal (oref d database) "/srv/rig/bd.db"))
    (should (equal (oref d prefixes) '("be")))))

;;; Section registry

(ert-deftest beads-extension-seams-section-register ()
  "`beads-section-register' stores a spec and returns its key."
  :tags '(:unit)
  (unwind-protect
      (progn
        (should-not (beads-section-spec-for 'wi4-demo))
        (should (eq (beads-section-register 'wi4-demo "Demo"
                                            (lambda () "data")
                                            (lambda (data) data))
                    'wi4-demo))
        (let ((spec (beads-section-spec-for 'wi4-demo)))
          (should (object-of-class-p spec 'beads-section-spec))
          (should (equal (oref spec title) "Demo"))
          (should (equal (funcall (oref spec loader)) "data"))
          (should (equal (funcall (oref spec renderer) "x") "x")))
        (should (memq 'wi4-demo
                      (mapcar (lambda (s) (oref s key))
                              (beads-section-registered)))))
    (remhash 'wi4-demo beads-section--registry)))

(ert-deftest beads-extension-seams-section-registered-vnodes ()
  "Registered sections render; an empty registry is a no-op."
  :tags '(:unit)
  (unwind-protect
      (progn
        (clrhash beads-section--registry)
        (should-not (beads-section-registered-vnodes))
        (beads-section-register 'wi4-a "A" (lambda () "payload")
                                (lambda (data) (format "V:%s" data)))
        (should (equal (beads-section-registered-vnodes) '("V:payload"))))
    (clrhash beads-section--registry)))

(ert-deftest beads-extension-seams-section-build-vnode ()
  "`beads-section-build-vnode' consults the status hook and registry."
  :tags '(:unit)
  (unwind-protect
      (progn
        (clrhash beads-section--registry)
        (let ((beads-status-sections-hook nil))
          (should (beads-section-build-vnode)))
        (let ((beads-status-sections-hook (list (lambda () "from-hook"))))
          (should (beads-section-build-vnode))))
    (clrhash beads-section--registry)))

;;; Dashboard section providers

(ert-deftest beads-extension-seams-dashboard-providers ()
  "Provider specs are collected, filtered and deduped; empty is a no-op."
  :tags '(:unit)
  (should-not (beads-dashboard--provider-specs))
  (let* ((a (beads-section-spec :key 'a :title "A"
                                 :loader (lambda () nil) :renderer #'identity))
         (b (beads-section-spec :key 'a :title "A2"
                                :loader (lambda () nil) :renderer #'identity))
         (c "not-a-spec")
         (beads-dashboard-section-providers
          (list (lambda () (list a c b)))))
    (let ((specs (beads-dashboard--provider-specs)))
      (should (= 1 (length specs)))
      (should (eq (oref (car specs) key) 'a))
      (should (equal (oref (car specs) title) "A")))))

;;; Action seams

(ert-deftest beads-extension-seams-action-providers ()
  "`beads-action-providers' is consulted; empty hook is nil."
  :tags '(:unit)
  (should-not (beads-actions-provider-actions :list))
  (let ((beads-action-providers
         (list (lambda (context)
                 (when (eq context :list) '(("x" . ignore)))))))
    (should (equal (beads-actions-provider-actions :list)
                   '(("x" . ignore))))
    (should-not (beads-actions-provider-actions :show))))

(ert-deftest beads-extension-seams-after-action-hook ()
  "`beads-after-action-functions' runs with action and issues."
  :tags '(:unit)
  (let ((seen nil))
    (let ((beads-after-action-functions
           (list (lambda (action issues) (setq seen (cons action issues))))))
      (beads-actions--after-action 'close '("be-1" "be-2")))
    (should (equal seen '(close "be-1" "be-2")))))

(ert-deftest beads-extension-seams-actions-context ()
  "`beads-actions-context' classifies the major mode."
  :tags '(:unit)
  (with-temp-buffer
    (setq major-mode 'beads-list-mode)
    (should (eq (car (beads-actions-context)) :list))
    (should-not (cdr (beads-actions-context))))
  (with-temp-buffer
    (setq major-mode 'beads-section-mode)
    (should (eq (car (beads-actions-context)) :status))))

;;; Menu providers

(ert-deftest beads-extension-seams-menu-providers ()
  "`beads-menu-providers' is collected; empty hook is nil."
  :tags '(:unit)
  (should-not (beads-menu-provider-groups))
  (let ((beads-menu-providers
         (list (lambda () '(("g" "Group" ignore)))
               (lambda () '(("h" "Other" ignore))))))
    (should (equal (beads-menu-provider-groups)
                   '(("g" "Group" ignore) ("h" "Other" ignore))))))

;;; Sling

(ert-deftest beads-extension-seams-sling-targets ()
  "`beads-sling-target-functions' is consulted and deduped; empty is nil."
  :tags '(:unit)
  (let ((beads-sling-target-functions nil))
    (should-not (beads-sling-targets)))
  (let* ((t1 (beads-sling-target :name "one" :kind 'agent))
         (t2 (beads-sling-target :name "one" :kind 'city))
         (t3 (beads-sling-target :name "two" :kind 'agent))
         (beads-sling-target-functions
          (list (lambda () (list t1))
                (lambda () (list t2 t3 "junk")))))
    (let ((targets (beads-sling-targets)))
      (should (= 2 (length targets)))
      (should (equal (mapcar (lambda (x) (oref x name)) targets)
                     '("one" "two"))))))

(ert-deftest beads-extension-seams-sling-dispatch-default ()
  "The default `beads-sling-dispatch' starts a local agent."
  :tags '(:unit)
  (let ((target (beads-sling-target :name "local" :kind 'agent))
        (captured nil))
    (cl-letf (((symbol-function 'beads-agent-start)
               (lambda (&rest args) (setq captured args) 'session)))
      (should (eq (beads-sling-dispatch target "be-1" "do it") 'session))
      (should (equal captured '("be-1" nil "do it" nil)))))
  (let ((target (beads-sling-target :name "named" :kind 'agent
                                    :backend "claude-code"))
        (captured nil))
    (cl-letf (((symbol-function 'beads-agent-start)
               (lambda (&rest args) (setq captured args) 'session)))
      (beads-sling-dispatch target "be-2" nil)
      (should (equal captured '("be-2" "claude-code" nil nil))))))

;;; Extension keymap

(ert-deftest beads-extension-seams-extension-map ()
  "`beads-mode--install-extension-map' binds the reserved `C-c b' prefix."
  :tags '(:unit)
  (should (keymapp beads-mode-extension-map))
  (let ((map (make-sparse-keymap)))
    (should (eq (beads-mode--install-extension-map map) map))
    (should (eq (lookup-key map (kbd "C-c b")) beads-mode-extension-map))))

;;; Faces

(ert-deftest beads-extension-seams-canonical-faces ()
  "The canonical face set exists."
  :tags '(:unit)
  (dolist (face '(beads-face-header beads-face-section beads-face-issue-line
                  beads-face-id beads-face-key
                  beads-face-status-open beads-face-status-in-progress
                  beads-face-status-blocked beads-face-status-closed
                  beads-face-priority-critical beads-face-priority-high
                  beads-face-priority-medium beads-face-priority-low
                  beads-face-agent-running beads-face-agent-idle
                  beads-face-agent-failed
                  beads-face-success beads-face-warning beads-face-error))
    (should (facep face))))

(provide 'beads-extension-seams-test)
;;; beads-extension-seams-test.el ends here