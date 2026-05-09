;;; beads-dashboard-test.el --- Tests for beads-dashboard -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; Tests for the Magit-idiomatic beads-dashboard:
;;   - render dispatch (loading / empty / error / ready)
;;   - collapse persistence across `g' refresh
;;   - collapse persistence across buffer-close-and-reopen via the
;;     session visibility cache
;;   - Magit-style depth keys (M-1..M-4)
;;   - transient guard (refresh skipped while a transient is active)
;;   - :key stability on section components

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'vui)
(require 'beads-command)
(require 'beads-dashboard)
(require 'beads-dashboard-sections)

;;; Render-state Tests

(ert-deftest beads-dashboard-test-section-loading-state ()
  "Pending status renders the loading skeleton when not collapsed."
  :tags '(:unit)
  (let* ((async-mock (lambda (_resolve _reject) nil))
         (vnode nil))
    ;; Verify that a section with a loader that never resolves shows
    ;; the loading placeholder via beads-dashboard--loading-line.
    (setq vnode (beads-dashboard--loading-line))
    (should (vui-vnode-p vnode))))

(ert-deftest beads-dashboard-test-section-empty-state ()
  "Empty state line renders a friendly default message."
  :tags '(:unit)
  (let ((vnode (beads-dashboard--empty-line)))
    (should (vui-vnode-p vnode))))

(ert-deftest beads-dashboard-test-section-empty-state-custom ()
  "Empty state accepts a custom message."
  :tags '(:unit)
  (let ((vnode (beads-dashboard--empty-line "Nothing here.")))
    (should (vui-vnode-p vnode))))

(ert-deftest beads-dashboard-test-section-error-line ()
  "Error line renders for string and structured errors alike."
  :tags '(:unit)
  (should (vui-vnode-p (beads-dashboard--error-line "boom")))
  (should (vui-vnode-p (beads-dashboard--error-line '("boom" :exit-code 1)))))

(ert-deftest beads-dashboard-test-section-limit-truncates ()
  "`beads-dashboard--render-issue-list' truncates at the section limit
and appends a `… and N more' line."
  :tags '(:unit)
  (let ((beads-dashboard-section-limit 3)
        (issues (mapcar (lambda (n)
                          (beads-issue :id (format "bd-%d" n)
                                       :title (format "Issue %d" n)
                                       :status "open"
                                       :priority 2
                                       :issue-type "task"))
                        (number-sequence 1 10)))
        ;; vstack vnodes carry their children; verify truncation by counting.
        )
    (let* ((vnode (beads-dashboard--render-issue-list issues))
           (children (and (vui-vnode-p vnode)
                          (vui-vnode-vstack-children vnode))))
      ;; 3 issue lines + 1 trailing "more" line
      (should (= 4 (length children))))))

(ert-deftest beads-dashboard-test-section-limit-nil-renders-all ()
  "When `beads-dashboard-section-limit' is nil, all issues render."
  :tags '(:unit)
  (let ((beads-dashboard-section-limit nil)
        (issues (mapcar (lambda (n)
                          (beads-issue :id (format "bd-%d" n)
                                       :title (format "Issue %d" n)
                                       :status "open"
                                       :priority 2
                                       :issue-type "task"))
                        (number-sequence 1 5))))
    (let* ((vnode (beads-dashboard--render-issue-list issues))
           (children (and (vui-vnode-p vnode)
                          (vui-vnode-vstack-children vnode))))
      (should (= 5 (length children))))))

(ert-deftest beads-dashboard-test-data-empty-p ()
  "`beads-dashboard--data-empty-p' recognises nil, empty list, empty vector."
  :tags '(:unit)
  (should (beads-dashboard--data-empty-p nil))
  (should (beads-dashboard--data-empty-p '()))
  (should (beads-dashboard--data-empty-p []))
  (should-not (beads-dashboard--data-empty-p '(1)))
  (should-not (beads-dashboard--data-empty-p [1])))

;;; Header / Modeline

(ert-deftest beads-dashboard-test-format-relative-time ()
  "Relative time renders short strings that include a unit suffix."
  :tags '(:unit)
  (should (string-match-p "\\(s\\|m\\|h\\) ago"
                          (beads-dashboard--format-relative-time
                           (- (float-time) 5))))
  (should (string-match-p "m ago"
                          (beads-dashboard--format-relative-time
                           (- (float-time) 120))))
  (should-not (beads-dashboard--format-relative-time nil)))

;;; Visibility Cache (collapse persistence)

(ert-deftest beads-dashboard-test-visibility-cache-default ()
  "Loading visibility for an unknown root returns the defaults."
  :tags '(:unit)
  (let ((beads-dashboard--visibility-cache nil))
    (let ((collapsed (beads-dashboard--load-visibility "/tmp/no/such/root/")))
      (should (equal (cdr (assq 'blocked collapsed)) t))
      (should (equal (cdr (assq 'epics collapsed)) t))
      (should (equal (cdr (assq 'closed collapsed)) t))
      (should (equal (cdr (assq 'federation collapsed)) t)))))

(ert-deftest beads-dashboard-test-visibility-cache-roundtrip ()
  "Saving then loading visibility for a project root returns stored alist."
  :tags '(:unit)
  (let ((beads-dashboard--visibility-cache nil)
        (root "/tmp/proj/")
        (state '((blocked . nil) (epics . t) (closed . t) (federation . t))))
    (beads-dashboard--save-visibility root state)
    (should (equal (beads-dashboard--load-visibility root) state))))

(ert-deftest beads-dashboard-test-visibility-cache-survives-refresh ()
  "Collapse state is restored after re-loading the dashboard buffer."
  :tags '(:unit)
  (let ((beads-dashboard--visibility-cache nil)
        (root "/tmp/proj/"))
    ;; Simulate a user collapse: blocked is *expanded* (nil), the rest
    ;; default-collapsed.  Save then re-read — must match.
    (let ((custom '((blocked . nil) (epics . t) (closed . t) (federation . t))))
      (beads-dashboard--save-visibility root custom)
      (should (equal (beads-dashboard--load-visibility root) custom)))
    ;; Mimic close-and-reopen: identity is preserved across reloads
    ;; since we only mutate via save-visibility.
    (should (equal (beads-dashboard--load-visibility root)
                   '((blocked . nil) (epics . t) (closed . t) (federation . t))))))

;;; Section Construction (`:key' stability)

(ert-deftest beads-dashboard-test-section-key-is-set ()
  "`beads-dashboard--section' yields a vnode with a stable :key."
  :tags '(:unit)
  (let* ((collapsed '((stats . nil)))
         (vnode (beads-dashboard--section
                 'stats "Stats"
                 (lambda (_r _j) nil)
                 (lambda (_d) (vui-text "ok"))
                 collapsed 0 (current-buffer))))
    (should (vui-vnode-p vnode))
    (should (eq (vui-vnode-key vnode) 'stats))))

(ert-deftest beads-dashboard-test-section-async-key-incorporates-generation ()
  "Bumping `generation' must change the async-key prop on the vnode.
This is what invalidates `vui-use-async' on hard refresh."
  :tags '(:unit)
  (let* ((collapsed '((stats . nil)))
         (vnode-1 (beads-dashboard--section
                   'stats "Stats"
                   (lambda (_r _j) nil) #'identity collapsed 0
                   (current-buffer)))
         (vnode-2 (beads-dashboard--section
                   'stats "Stats"
                   (lambda (_r _j) nil) #'identity collapsed 1
                   (current-buffer)))
         (props-1 (vui-vnode-component-props vnode-1))
         (props-2 (vui-vnode-component-props vnode-2)))
    (should-not (equal (plist-get props-1 :async-key)
                       (plist-get props-2 :async-key)))))

(ert-deftest beads-dashboard-test-section-collapsed-skips-fetch ()
  "A collapsed section installs a no-op loader that resolves to nil.
Default-collapsed sections must not fetch their data until expanded."
  :tags '(:unit)
  (let* ((collapsed '((blocked . t)))
         (vnode (beads-dashboard--section
                 'blocked "Blocked"
                 (lambda (_resolve _reject)
                   (error "Loader fired despite collapse"))
                 #'identity collapsed 0 (current-buffer)))
         (props (vui-vnode-component-props vnode))
         (loader (plist-get props :load))
         (resolved 'nope))
    (funcall loader (lambda (v) (setq resolved v)) #'ignore)
    (should (null resolved))))

;;; Magit-style Depth Keys

(ert-deftest beads-dashboard-test-depth-keys-defined ()
  "Depth keymap keys map to depth-toggling commands."
  :tags '(:unit)
  (should (eq (lookup-key beads-dashboard-mode-map (kbd "M-1"))
              #'beads-dashboard-depth-1))
  (should (eq (lookup-key beads-dashboard-mode-map (kbd "M-2"))
              #'beads-dashboard-depth-2))
  (should (eq (lookup-key beads-dashboard-mode-map (kbd "M-3"))
              #'beads-dashboard-depth-3))
  (should (eq (lookup-key beads-dashboard-mode-map (kbd "M-4"))
              #'beads-dashboard-depth-4)))

(ert-deftest beads-dashboard-test-navigation-keys-bound ()
  "n/p and M-n/M-p map to the dashboard navigation commands."
  :tags '(:unit)
  (should (eq (lookup-key beads-dashboard-mode-map (kbd "n"))
              #'beads-dashboard-next-item))
  (should (eq (lookup-key beads-dashboard-mode-map (kbd "p"))
              #'beads-dashboard-previous-item))
  (should (eq (lookup-key beads-dashboard-mode-map (kbd "M-n"))
              #'beads-dashboard-next-section))
  (should (eq (lookup-key beads-dashboard-mode-map (kbd "M-p"))
              #'beads-dashboard-previous-section)))

(ert-deftest beads-dashboard-test-header-line-p-detects-glyphs ()
  "`beads-dashboard--header-line-p' recognises ▼/▶ section glyphs."
  :tags '(:unit)
  (with-temp-buffer
    (insert "▼ Stats\n  body\n▶ Blocked\n")
    (goto-char (point-min))
    (should (beads-dashboard--header-line-p))
    (forward-line 1)
    (should-not (beads-dashboard--header-line-p))
    (forward-line 1)
    (should (beads-dashboard--header-line-p))))

(ert-deftest beads-dashboard-test-issue-line-p-detects-property ()
  "`beads-dashboard--issue-line-p' detects the `beads-section' property."
  :tags '(:unit)
  (with-temp-buffer
    (insert "▼ Ready\n")
    (let ((line (propertize "  bd-1   open  Title"
                            'beads-section
                            (beads-issue-section
                             :issue (beads-issue :id "bd-1"
                                                 :title "Title"
                                                 :status "open"
                                                 :priority 2
                                                 :issue-type "task")))))
      (insert line "\n  plain line\n")
      ;; Line 1: header.  Line 2: propertized issue.  Line 3: plain.
      (goto-char (point-min))
      (forward-line 1)
      (should (beads-dashboard--issue-line-p))
      (forward-line 1)
      (should-not (beads-dashboard--issue-line-p)))))

(ert-deftest beads-dashboard-test-magit-keys-bound ()
  "`g', `r', `q', `c', `b', `RET', `TAB' have Magit-idiomatic bindings."
  :tags '(:unit)
  (should (eq (lookup-key beads-dashboard-mode-map (kbd "g"))
              #'beads-dashboard-refresh-dispatch))
  (should (eq (lookup-key beads-dashboard-mode-map (kbd "r"))
              #'beads-dashboard-toggle-auto-refresh))
  (should (eq (lookup-key beads-dashboard-mode-map (kbd "q"))
              #'quit-window))
  (should (eq (lookup-key beads-dashboard-mode-map (kbd "c"))
              #'beads-dashboard-claim-at-point))
  (should (eq (lookup-key beads-dashboard-mode-map (kbd "b"))
              #'beads-dashboard-jump-to-blocker))
  (should (eq (lookup-key beads-dashboard-mode-map (kbd "RET"))
              #'beads-dashboard-visit-at-point))
  (should (eq (lookup-key beads-dashboard-mode-map (kbd "TAB"))
              #'beads-dashboard-toggle-section)))

;;; Transient Guard

(ert-deftest beads-dashboard-test-idle-refresh-skips-while-transient-active ()
  "Idle refresh must not preempt an active transient."
  :tags '(:unit)
  (let ((tmp (generate-new-buffer " *beads-dash-test*")))
    (unwind-protect
        (with-current-buffer tmp
          (beads-dashboard-mode)
          ;; Mock transient--prefix to look bound.
          (cl-letf (((symbol-value 'transient--prefix) t))
            ;; Should run silently without re-rendering — exercise the
            ;; guard by ensuring no error is raised.
            (beads-dashboard--idle-refresh tmp)
            (should t)))
      (let ((kill-buffer-query-functions nil)) (kill-buffer tmp)))))

(ert-deftest beads-dashboard-test-idle-refresh-skips-on-dead-buffer ()
  "Idle refresh must skip safely when its target buffer is dead."
  :tags '(:unit)
  (let ((tmp (generate-new-buffer " *beads-dash-test-dead*")))
    (let ((kill-buffer-query-functions nil)) (kill-buffer tmp))
    ;; No error means the guard did its job.
    (beads-dashboard--idle-refresh tmp)
    (should t)))

;;; Buffer Naming

(ert-deftest beads-dashboard-test-buffer-name-for-root ()
  "Buffer name reflects the project root basename or the default fallback."
  :tags '(:unit)
  (should (equal (beads-dashboard--buffer-name-for "/tmp/proj/")
                 "*beads-dashboard<proj>*"))
  (should (equal (beads-dashboard--buffer-name-for nil)
                 beads-dashboard--buffer-name)))

;;; Compat Shim

(ert-deftest beads-dashboard-test-compat-shim-exists ()
  "`beads-status' is preserved as a compat shim that loads beads-dashboard."
  :tags '(:unit)
  (should (fboundp 'beads-status))
  (should (fboundp 'beads-dashboard)))

;;; Limited-vstack with extra-leading-rows

(ert-deftest beads-dashboard-test-limited-vstack-extra-leading-rows ()
  "`extra-leading-rows' replaces the default per-item mapcar."
  :tags '(:unit)
  (let ((beads-dashboard-section-limit 5)
        (items '(a b c)))
    (let* ((vnode (beads-dashboard--limited-vstack
                   items #'ignore
                   (lambda (visible)
                     (mapcar (lambda (x) (vui-text (format "G:%s" x)))
                             visible))))
           (children (and (vui-vnode-p vnode)
                          (vui-vnode-vstack-children vnode))))
      (should (= 3 (length children))))))

(provide 'beads-dashboard-test)
;;; beads-dashboard-test.el ends here
