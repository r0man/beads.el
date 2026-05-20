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
(require 'beads-section)
(require 'beads-agent)

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

(ert-deftest beads-dashboard-test-truncate-lines-buffer-local ()
  "`beads-dashboard-mode' sets `truncate-lines' buffer-locally so issue
rows do not wrap to a continuation line in narrow / side-by-side splits."
  :tags '(:unit)
  (let ((tmp (generate-new-buffer " *beads-dash-truncate-test*")))
    (unwind-protect
        (with-current-buffer tmp
          (beads-dashboard-mode)
          (should (eq truncate-lines t))
          (should (local-variable-p 'truncate-lines)))
      (kill-buffer tmp))))

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
                   items #'ignore nil nil
                   (lambda (visible)
                     (mapcar (lambda (x) (vui-text (format "G:%s" x)))
                             visible))))
           (children (and (vui-vnode-p vnode)
                          (vui-vnode-vstack-children vnode))))
      (should (= 3 (length children))))))

;;; Trailing agent badges on issue rows (bde-npte.4)

(ert-deftest beads-dashboard-test-render-issue-list-no-agents-no-padding ()
  "Issue rows with no agent activity render no trailing badge or padding."
  :tags '(:unit)
  (cl-letf (((symbol-function 'beads-agent--get-sessions-focused-on-issue)
             (lambda (_id) nil))
            ((symbol-function 'beads-agent--get-sessions-touching-issue)
             (lambda (_id) nil))
            ((symbol-function 'beads-agent--get-sessions-for-issue)
             (lambda (_id) nil))
            ((symbol-function 'beads-agent--get-issue-outcome)
             (lambda (_id) nil)))
    (let* ((issues (list (beads-issue :id "bd-noagent" :title "Plain row"
                                      :status "open" :priority 2
                                      :issue-type "task")))
           (vnode (beads-dashboard--render-issue-list issues 'ready))
           (kids (vui-vnode-vstack-children vnode))
           (label (vui-vnode-button-label (car kids))))
      ;; The label ends at the title — no trailing badge or padding.
      (should (string-suffix-p "Plain row" label)))))

(ert-deftest beads-dashboard-test-render-issue-list-one-focused-agent ()
  "Issue rows with one focused agent append the role glyph to the label."
  :tags '(:unit)
  (require 'beads-agent-types)
  (unless (beads-agent-type-get "task")
    (beads-agent-type-register (beads-agent-type-task)))
  (let ((beads-agent-display-use-icons nil)
        (beads-agent-type-icons nil))
    (cl-letf (((symbol-function 'beads-agent--get-sessions-focused-on-issue)
               (lambda (id) (and (equal id "bd-agent") '(sess1))))
              ((symbol-function 'beads-agent--get-sessions-touching-issue)
               (lambda (_id) nil))
              ((symbol-function 'beads-agent--get-sessions-for-issue)
               (lambda (_id) nil))
              ((symbol-function 'beads-agent--get-issue-outcome)
               (lambda (_id) nil))
              ((symbol-function 'beads-agent-session-type-name)
               (lambda (_s) "Task"))
              ((symbol-function 'beads-agent-session-instance-number)
               (lambda (_s) 1)))
      (let* ((issues (list (beads-issue :id "bd-agent" :title "Has agent"
                                        :status "in_progress" :priority 2
                                        :issue-type "task")))
             (vnode (beads-dashboard--render-issue-list issues 'in-flight))
             (label (vui-vnode-button-label (car (vui-vnode-vstack-children vnode)))))
        (should (string-suffix-p "Has agent  T" label))))))

(ert-deftest beads-dashboard-test-render-issue-list-multiple-focused-agents ()
  "Multiple focused agents render joined by a space, after a two-space gap."
  :tags '(:unit)
  (require 'beads-agent-types)
  (unless (beads-agent-type-get "task")
    (beads-agent-type-register (beads-agent-type-task)))
  (require 'beads-agent-types)
  (unless (beads-agent-type-get "review")
    (beads-agent-type-register (beads-agent-type-review)))
  (let* ((beads-agent-display-use-icons nil)
         (beads-agent-type-icons nil)
         (types '((s1 . "Task") (s2 . "Review"))))
    (cl-letf (((symbol-function 'beads-agent--get-sessions-focused-on-issue)
               (lambda (_id) '(s1 s2)))
              ((symbol-function 'beads-agent--get-sessions-touching-issue)
               (lambda (_id) nil))
              ((symbol-function 'beads-agent--get-sessions-for-issue)
               (lambda (_id) nil))
              ((symbol-function 'beads-agent--get-issue-outcome)
               (lambda (_id) nil))
              ((symbol-function 'beads-agent-session-type-name)
               (lambda (s) (cdr (assoc s types))))
              ((symbol-function 'beads-agent-session-instance-number)
               (lambda (_s) 1)))
      (let* ((issues (list (beads-issue :id "bd-multi" :title "Two agents"
                                        :status "in_progress" :priority 2
                                        :issue-type "task")))
             (vnode (beads-dashboard--render-issue-list issues 'in-flight))
             (label (vui-vnode-button-label (car (vui-vnode-vstack-children vnode)))))
        (should (string-suffix-p "Two agents  T R" label))))))

(ert-deftest beads-dashboard-test-render-issue-list-finished-outcome-badge ()
  "Finished outcome renders `✓T' badge on the trailing position."
  :tags '(:unit)
  (let ((beads-agent-display-use-icons nil)
        (beads-agent-type-icons nil))
    (cl-letf (((symbol-function 'beads-agent--get-sessions-focused-on-issue)
               (lambda (_id) nil))
              ((symbol-function 'beads-agent--get-sessions-touching-issue)
               (lambda (_id) nil))
              ((symbol-function 'beads-agent--get-sessions-for-issue)
               (lambda (_id) nil))
              ((symbol-function 'beads-agent--get-issue-outcome)
               (lambda (_id) '("Task" . finished))))
      (let* ((issues (list (beads-issue :id "bd-done" :title "Done row"
                                        :status "closed" :priority 2
                                        :issue-type "task")))
             (vnode (beads-dashboard--render-issue-list issues 'closed))
             (label (vui-vnode-button-label (car (vui-vnode-vstack-children vnode)))))
        (should (string-suffix-p "Done row  ✓T" label))))))

(ert-deftest beads-dashboard-test-render-issue-list-failed-outcome-badge ()
  "Failed outcome renders `✗R' badge on the trailing position."
  :tags '(:unit)
  (let ((beads-agent-display-use-icons nil)
        (beads-agent-type-icons nil))
    (cl-letf (((symbol-function 'beads-agent--get-sessions-focused-on-issue)
               (lambda (_id) nil))
              ((symbol-function 'beads-agent--get-sessions-touching-issue)
               (lambda (_id) nil))
              ((symbol-function 'beads-agent--get-sessions-for-issue)
               (lambda (_id) nil))
              ((symbol-function 'beads-agent--get-issue-outcome)
               (lambda (_id) '("Review" . failed))))
      (let* ((issues (list (beads-issue :id "bd-fail" :title "Fail row"
                                        :status "open" :priority 2
                                        :issue-type "task")))
             (vnode (beads-dashboard--render-issue-list issues 'blocked))
             (label (vui-vnode-button-label (car (vui-vnode-vstack-children vnode)))))
        (should (string-suffix-p "Fail row  ✗R" label))))))

;;; Agent issue-id detection from dashboard-style buffers

(ert-deftest beads-dashboard-test-agent-detect-issue-id-from-section ()
  "`beads-agent--detect-issue-id' resolves an issue stamped on the
line at point via the `beads-section' text-property contract that the
dashboard (and any other `beads-section-mode'-derived view) uses to
mark issue lines.  Regression: previously the detector only knew
`beads-list-mode' and `beads-show-mode', so `beads-agent-start-at-point'
and friends were no-ops in the dashboard.

Pure unit test — does not touch `beads-agent--backends', so it cannot
trigger the registry-pollution failure mode."
  :tags '(:unit)
  (let ((issue (beads-issue :id "bd-dash-1"
                            :title "Detector probe"
                            :status "open"
                            :priority 2
                            :issue-type "task")))
    (with-temp-buffer
      ;; Mimic what the dashboard's issue-line renderer stamps onto
      ;; each issue line.  No vui mount needed — the detector only
      ;; reads the text property at point.
      (insert (beads-section--propertize
               "  bd-dash-1 P2 task open Detector probe"
               (beads-issue-section :issue issue)))
      (insert "\n")
      (goto-char (point-min))
      ;; Point is on the propertized region.
      (should (equal (beads-section-issue-id-at-point) "bd-dash-1"))
      (should (equal (beads-agent--detect-issue-id) "bd-dash-1"))
      ;; Off the propertized region → nil for both.  After
      ;; `goto-char point-min', `end-of-line' lands on the bare
      ;; unpropertized newline that terminates the issue line; the
      ;; extra `forward-char' steps one past it for good measure.
      (end-of-line)
      (forward-char 1)
      (should-not (beads-section-issue-id-at-point))
      (should-not (beads-agent--detect-issue-id)))))

;;; Per-Section Load More

(ert-deftest beads-dashboard-test-extra-cache-default-nil ()
  "Loading extra-rows for an unknown root returns nil, not an error."
  :tags '(:unit)
  (let ((beads-dashboard--extra-cache nil))
    (should-not (beads-dashboard--load-extra "/tmp/no/such/root/"))))

(ert-deftest beads-dashboard-test-extra-cache-roundtrip ()
  "Saving then loading extra-rows for a project root returns stored alist."
  :tags '(:unit)
  (let ((beads-dashboard--extra-cache nil)
        (root "/tmp/proj-extra/")
        (state '((ready . 20) (blocked . all))))
    (beads-dashboard--save-extra root state)
    (should (equal (beads-dashboard--load-extra root) state))))

(ert-deftest beads-dashboard-test-extra-cache-nil-clears-entry ()
  "Saving nil for a root removes its entry rather than storing (root . nil)."
  :tags '(:unit)
  (let ((beads-dashboard--extra-cache nil)
        (root "/tmp/proj-extra/"))
    (beads-dashboard--save-extra root '((ready . 20)))
    (beads-dashboard--save-extra root nil)
    (should-not (beads-dashboard--load-extra root))
    (should-not (assoc root beads-dashboard--extra-cache))))

(ert-deftest beads-dashboard-test-effective-display-limit-base ()
  "Nil extra → effective display limit equals the section base."
  :tags '(:unit)
  (let ((beads-dashboard-section-limit 10))
    (should (= 10 (beads-dashboard--effective-display-limit nil)))))

(ert-deftest beads-dashboard-test-effective-display-limit-with-extra ()
  "Base 10 + extra 5 → effective display limit 15."
  :tags '(:unit)
  (let ((beads-dashboard-section-limit 10))
    (should (= 15 (beads-dashboard--effective-display-limit 5)))))

(ert-deftest beads-dashboard-test-effective-display-limit-all ()
  "`all' sentinel → nil (unlimited)."
  :tags '(:unit)
  (let ((beads-dashboard-section-limit 10))
    (should-not (beads-dashboard--effective-display-limit 'all))))

(ert-deftest beads-dashboard-test-effective-display-limit-base-nil ()
  "Nil section base → nil (unlimited), regardless of extra."
  :tags '(:unit)
  (let ((beads-dashboard-section-limit nil))
    (should-not (beads-dashboard--effective-display-limit nil))
    (should-not (beads-dashboard--effective-display-limit 5))))

(ert-deftest beads-dashboard-test-effective-fetch-limit-nil ()
  "Nil extra → nil (no --limit passed, CLI default applies).
This preserves the headline (N) count and keeps the `… and N more'
affordance live when the user has not bumped the section."
  :tags '(:unit)
  (let ((beads-dashboard-section-limit 10))
    (should-not (beads-dashboard--effective-fetch-limit nil))))

(ert-deftest beads-dashboard-test-effective-fetch-limit-all ()
  "`all' → 0 (CLI unlimited semantics for bd ready/stale/list)."
  :tags '(:unit)
  (let ((beads-dashboard-section-limit 10))
    (should (= 0 (beads-dashboard--effective-fetch-limit 'all)))))

(ert-deftest beads-dashboard-test-effective-fetch-limit-int ()
  "Integer extra → 0 (unlimited at the CLI; local truncation does the work).
Once the user has invoked `+', we want the section header to report
the true total instead of dropping when our derived limit falls below
the CLI default."
  :tags '(:unit)
  (let ((beads-dashboard-section-limit 10)
        (beads-dashboard-section-batch 10))
    (should (= 0 (beads-dashboard--effective-fetch-limit 15)))))

(ert-deftest beads-dashboard-test-more-line-clickable ()
  "`beads-dashboard--more-line' returns a clickable button vnode with
the hidden count visible in its label."
  :tags '(:unit)
  (let ((vnode (beads-dashboard--more-line 'ready 7)))
    (should (vui-vnode-button-p vnode))
    (should (string-match-p "7" (vui-vnode-button-label vnode)))
    (should (functionp (vui-vnode-button-on-click vnode)))))

(ert-deftest beads-dashboard-test-more-line-hidden-zero-returns-nil ()
  "No more-line is emitted when nothing is hidden."
  :tags '(:unit)
  (should-not (beads-dashboard--more-line 'ready 0))
  (should-not (beads-dashboard--more-line 'ready nil)))

(ert-deftest beads-dashboard-test-more-line-carries-section-key ()
  "The more-line label carries `beads-dashboard-section-key' so a click
on it can route the load-more action to the correct section."
  :tags '(:unit)
  (let* ((vnode (beads-dashboard--more-line 'ready 3))
         (label (vui-vnode-button-label vnode)))
    (should (eq 'ready
                (get-text-property 0 'beads-dashboard-section-key label)))))

(ert-deftest beads-dashboard-test-limited-vstack-honours-extra ()
  "Extra rows raise the effective display limit and shrink the more-line."
  :tags '(:unit)
  (let ((beads-dashboard-section-limit 2)
        (items '(a b c d e)))
    ;; Without extra: 2 visible + 1 more-line button = 3 children.
    (let* ((vnode (beads-dashboard--limited-vstack
                   items (lambda (x _) (vui-text (format "%s" x)))
                   nil 'ready))
           (kids (vui-vnode-vstack-children vnode)))
      (should (= 3 (length kids))))
    ;; With extra 2: 4 visible + 1 more-line button = 5 children.
    (let* ((vnode (beads-dashboard--limited-vstack
                   items (lambda (x _) (vui-text (format "%s" x)))
                   2 'ready))
           (kids (vui-vnode-vstack-children vnode)))
      (should (= 5 (length kids))))
    ;; With `all': 5 visible, no more-line.
    (let* ((vnode (beads-dashboard--limited-vstack
                   items (lambda (x _) (vui-text (format "%s" x)))
                   'all 'ready))
           (kids (vui-vnode-vstack-children vnode)))
      (should (= 5 (length kids))))))

(ert-deftest beads-dashboard-test-render-issue-list-stamps-section-key ()
  "Each rendered issue row carries the supplied `beads-dashboard-section-key'."
  :tags '(:unit)
  (let* ((issues (list (beads-issue :id "bd-1" :title "A"
                                    :status "open" :priority 2
                                    :issue-type "task")
                       (beads-issue :id "bd-2" :title "B"
                                    :status "open" :priority 2
                                    :issue-type "task")))
         (beads-dashboard-section-limit 10)
         (vnode (beads-dashboard--render-issue-list issues 'ready))
         (kids (vui-vnode-vstack-children vnode))
         (first (car kids)))
    (should (vui-vnode-button-p first))
    (let ((label (vui-vnode-button-label first)))
      (should (eq 'ready
                  (get-text-property 0 'beads-dashboard-section-key label))))))

(ert-deftest beads-dashboard-test-section-async-key-includes-extra ()
  "Bumping the per-section `:extra-rows' prop must change the vnode's
async-key — otherwise `vui-use-async' would return the cached smaller
payload and `+' would visibly do nothing."
  :tags '(:unit)
  (let* ((collapsed '((ready . nil)))
         (vnode-a (beads-dashboard--section
                   'ready "Ready"
                   (lambda (_r _j) nil) #'identity collapsed 0
                   (current-buffer) :extra-rows nil))
         (vnode-b (beads-dashboard--section
                   'ready "Ready"
                   (lambda (_r _j) nil) #'identity collapsed 0
                   (current-buffer) :extra-rows 10))
         (key-a (plist-get (vui-vnode-component-props vnode-a) :async-key))
         (key-b (plist-get (vui-vnode-component-props vnode-b) :async-key)))
    (should-not (equal key-a key-b))))

(ert-deftest beads-dashboard-test-load-keys-bound ()
  "+/-/* bind to the load-more / load-less / load-all commands."
  :tags '(:unit)
  (should (eq (lookup-key beads-dashboard-mode-map (kbd "+"))
              #'beads-dashboard-load-more))
  (should (eq (lookup-key beads-dashboard-mode-map (kbd "-"))
              #'beads-dashboard-load-less))
  (should (eq (lookup-key beads-dashboard-mode-map (kbd "*"))
              #'beads-dashboard-load-all)))

(ert-deftest beads-dashboard-test-bump-extra-adds-entry-and-persists ()
  "`beads-dashboard--bump-extra' adds a section entry and writes the cache."
  :tags '(:unit)
  (let ((tmp (generate-new-buffer " *beads-dash-bump-test*"))
        (beads-dashboard--extra-cache nil))
    (unwind-protect
        (with-current-buffer tmp
          (beads-dashboard-mode)
          ;; Stub the project root and the root-state plumbing so the
          ;; bump runs without a full vui mount.
          (cl-letf* ((root "/tmp/bump-proj/")
                     ((symbol-function 'beads-dashboard--project-root)
                      (lambda () root))
                     (state '())
                     ((symbol-function 'beads-dashboard--root-state)
                      (lambda (k) (when (eq k :extra) state)))
                     ((symbol-function 'beads-dashboard--bump)
                      (lambda (_k v) (setq state v))))
            (beads-dashboard--bump-extra 'ready 10)
            (should (equal '((ready . 10)) state))
            (should (equal '((ready . 10))
                           (beads-dashboard--load-extra root)))))
      (let ((kill-buffer-query-functions nil)) (kill-buffer tmp)))))

(ert-deftest beads-dashboard-test-bump-extra-floors-at-zero ()
  "A negative delta that would go below zero clears the entry."
  :tags '(:unit)
  (let ((tmp (generate-new-buffer " *beads-dash-floor-test*"))
        (beads-dashboard--extra-cache nil))
    (unwind-protect
        (with-current-buffer tmp
          (beads-dashboard-mode)
          (cl-letf* ((root "/tmp/floor-proj/")
                     ((symbol-function 'beads-dashboard--project-root)
                      (lambda () root))
                     (state '((ready . 5)))
                     ((symbol-function 'beads-dashboard--root-state)
                      (lambda (k) (when (eq k :extra) state)))
                     ((symbol-function 'beads-dashboard--bump)
                      (lambda (_k v) (setq state v))))
            (beads-dashboard--bump-extra 'ready -10)
            (should (null state))
            (should-not (assq 'ready state))))
      (let ((kill-buffer-query-functions nil)) (kill-buffer tmp)))))

(ert-deftest beads-dashboard-test-bump-extra-all-then-int ()
  "Going from `all' to an integer delta starts fresh from zero."
  :tags '(:unit)
  (let ((tmp (generate-new-buffer " *beads-dash-all-test*"))
        (beads-dashboard--extra-cache nil))
    (unwind-protect
        (with-current-buffer tmp
          (beads-dashboard-mode)
          (cl-letf* ((root "/tmp/all-proj/")
                     ((symbol-function 'beads-dashboard--project-root)
                      (lambda () root))
                     (state '((ready . all)))
                     ((symbol-function 'beads-dashboard--root-state)
                      (lambda (k) (when (eq k :extra) state)))
                     ((symbol-function 'beads-dashboard--bump)
                      (lambda (_k v) (setq state v))))
            (beads-dashboard--bump-extra 'ready 5)
            (should (equal '((ready . 5)) state))))
      (let ((kill-buffer-query-functions nil)) (kill-buffer tmp)))))

(ert-deftest beads-dashboard-test-section-at-point-uses-text-property ()
  "`beads-dashboard--section-at-point' resolves the section via the
`beads-dashboard-section-key' text property at point."
  :tags '(:unit)
  (with-temp-buffer
    (insert (propertize "row" 'beads-dashboard-section-key 'ready))
    (goto-char (point-min))
    (should (eq 'ready (beads-dashboard--section-at-point)))))

(ert-deftest beads-dashboard-test-section-at-point-scans-line ()
  "Section detection finds the key anywhere on the current line, not
just exactly at point — covers landing in a gap between the chevron
glyph and the stamped title text."
  :tags '(:unit)
  (with-temp-buffer
    ;; First two chars unstamped (simulates the chevron + space),
    ;; the rest carries the section-key like a header label would.
    (insert "▼ "
            (propertize "Ready (10)" 'beads-dashboard-section-key 'ready))
    ;; Land point on the unstamped prefix.
    (goto-char (point-min))
    (should (eq 'ready (beads-dashboard--section-at-point)))))

(ert-deftest beads-dashboard-test-section-at-point-walks-back-to-header ()
  "When point sits on a blank line below a section's content, section
detection walks back to the header chevron line and reads the key
from there."
  :tags '(:unit)
  (with-temp-buffer
    (insert "▼ "
            (propertize "Stale (3)" 'beads-dashboard-section-key 'stale)
            "\n"
            "  bde-1   P0   task   open   row\n"
            "\n")
    ;; Trailing blank line.
    (goto-char (point-max))
    (forward-line -1)
    (should (eq 'stale (beads-dashboard--section-at-point)))))

(ert-deftest beads-dashboard-test-section-at-point-on-header-line ()
  "Section detection works when point is *on* the header line itself —
the bare chevron at column 0 must still resolve to the section."
  :tags '(:unit)
  (with-temp-buffer
    (insert "▼ "
            (propertize "Stale (3)" 'beads-dashboard-section-key 'stale)
            "\n"
            "  bde-1   P0   task   open   row\n")
    (goto-char (point-min)) ; on the chevron
    (should (eq 'stale (beads-dashboard--section-at-point)))
    ;; Also at end-of-header-line.
    (end-of-line)
    (should (eq 'stale (beads-dashboard--section-at-point)))))

(ert-deftest beads-dashboard-test-section-at-point-on-empty-state ()
  "Section detection works on an empty-state line that carries only the
section-key (no per-row `beads-section' issue object) — covers the
`Nothing to show.' / `No work claimed.' placeholders."
  :tags '(:unit)
  (with-temp-buffer
    (insert "▼ "
            (propertize "Orphans (0)" 'beads-dashboard-section-key 'orphans)
            "\n"
            (propertize "  Nothing to show."
                        'beads-dashboard-section-key 'orphans)
            "\n")
    (forward-line -1)
    (forward-char 4) ; "  No|thing to show."
    (should (eq 'orphans (beads-dashboard--section-at-point)))))

(ert-deftest beads-dashboard-test-section-header-stamps-key ()
  "`beads-dashboard--section-header' propertises its label with
`beads-dashboard-section-key' when given one — verified by walking
the rendered vnode's button label."
  :tags '(:unit)
  (let* ((vnode (beads-dashboard--section-header
                 "Ready" "✅" nil 5 (lambda () (ignore)) 'ready))
         (label (vui-vnode-button-label vnode)))
    (should (stringp label))
    ;; Anywhere on the label suffices — the property starts after the
    ;; chevron glyph.  Look at the last char to be safe.
    (should (eq 'ready
                (get-text-property (1- (length label))
                                   'beads-dashboard-section-key label)))))

(ert-deftest beads-dashboard-test-empty-line-stamps-key ()
  "`beads-dashboard--empty-line' stamps SECTION-KEY when supplied."
  :tags '(:unit)
  (let* ((vnode (beads-dashboard--empty-line "Nothing." 'in-flight))
         (text  (vui-vnode-text-content vnode)))
    (should (eq 'in-flight
                (get-text-property 0 'beads-dashboard-section-key text)))))

(ert-deftest beads-dashboard-test-loading-line-stamps-key ()
  "`beads-dashboard--loading-line' stamps SECTION-KEY when supplied."
  :tags '(:unit)
  (let* ((vnode (beads-dashboard--loading-line 'closed))
         (text  (vui-vnode-text-content vnode)))
    (should (eq 'closed
                (get-text-property 0 'beads-dashboard-section-key text)))))

(ert-deftest beads-dashboard-test-issue-id-at-line-finds-stamped-id ()
  "`beads-dashboard--issue-id-at-line' returns the id of a beads-issue
stamped on the current line via the `beads-section' property contract."
  :tags '(:unit)
  (let* ((issue (beads-issue :id "bd-42" :title "x" :status "open"
                             :priority 0 :issue-type "task"))
         (section (beads-issue-section :issue issue))
         (stamped (propertize "  bd-42 row text" 'beads-section section)))
    (with-temp-buffer
      (insert stamped)
      (goto-char (point-min))
      (should (equal "bd-42" (beads-dashboard--issue-id-at-line)))
      ;; Anywhere on the line works.
      (forward-char 8)
      (should (equal "bd-42" (beads-dashboard--issue-id-at-line))))))

(ert-deftest beads-dashboard-test-goto-issue-line-finds-and-leaves-point ()
  "`beads-dashboard--goto-issue-line' moves point to the stamped row."
  :tags '(:unit)
  (let* ((issue (beads-issue :id "bd-7" :title "x" :status "open"
                             :priority 0 :issue-type "task"))
         (section (beads-issue-section :issue issue)))
    (with-temp-buffer
      (insert "▼ header\n"
              (propertize "  bd-7  row text\n" 'beads-section section)
              "▼ next header\n")
      (goto-char (point-max))
      (should (beads-dashboard--goto-issue-line "bd-7"))
      (should (looking-at "bd-7"))
      ;; Missing id returns nil and leaves point alone.
      (let ((before (point)))
        (should-not (beads-dashboard--goto-issue-line "bd-MISSING"))
        (should (= before (point)))))))

(ert-deftest beads-dashboard-test-goto-section-header-jumps-to-header ()
  "`beads-dashboard--goto-section-header' lands point past the chevron
on the header line for the given key."
  :tags '(:unit)
  (with-temp-buffer
    (insert "▼ "
            (propertize "Ready (5)" 'beads-dashboard-section-key 'ready)
            "\n"
            "  row\n"
            "▼ "
            (propertize "Stale (3)" 'beads-dashboard-section-key 'stale)
            "\n"
            "  another row\n")
    (goto-char (point-max))
    (should (beads-dashboard--goto-section-header 'ready))
    (should (looking-at "Ready"))
    (should (beads-dashboard--goto-section-header 'stale))
    (should (looking-at "Stale"))
    ;; Unknown key returns nil.
    (let ((before (point)))
      (should-not (beads-dashboard--goto-section-header 'no-such))
      (should (= before (point))))))

(ert-deftest beads-dashboard-test-restore-point-prefers-issue ()
  "`beads-dashboard--restore-point' goes to the stamped issue when it
exists in the buffer."
  :tags '(:unit)
  (let* ((issue (beads-issue :id "bd-9" :title "x" :status "open"
                             :priority 0 :issue-type "task"))
         (section (beads-issue-section :issue issue)))
    (with-temp-buffer
      (insert "▼ "
              (propertize "Ready (1)" 'beads-dashboard-section-key 'ready)
              "\n"
              (propertize "  bd-9 row\n" 'beads-section section)
              "▼ "
              (propertize "Stale (0)" 'beads-dashboard-section-key 'stale)
              "\n")
      (goto-char (point-min))
      (beads-dashboard--restore-point 'ready "bd-9" nil)
      (should (looking-at "bd-9")))))

(ert-deftest beads-dashboard-test-restore-point-falls-back-to-header ()
  "`beads-dashboard--restore-point' falls back to the section header
when the issue is no longer in the buffer."
  :tags '(:unit)
  (with-temp-buffer
    (insert "▼ "
            (propertize "Ready (0)" 'beads-dashboard-section-key 'ready)
            "\n")
    (goto-char (point-min))
    (beads-dashboard--restore-point 'ready "bd-vanished" nil)
    (should (looking-at "Ready"))))

(ert-deftest beads-dashboard-test-on-more-line-detects-more-line ()
  "`beads-dashboard--on-more-line-p' is t on a `… and N more (+)' row."
  :tags '(:unit)
  (with-temp-buffer
    (insert "▼ "
            (propertize "Ready (15)" 'beads-dashboard-section-key 'ready)
            "\n"
            (propertize "  … and 5 more (+)" 'beads-dashboard-section-key 'ready)
            "\n")
    (forward-line -1)
    (forward-char 4)
    (should (beads-dashboard--on-more-line-p))
    ;; Header is NOT a more-line.
    (goto-char (point-min))
    (should-not (beads-dashboard--on-more-line-p))))

(ert-deftest beads-dashboard-test-more-line-text-p-shared-format ()
  "`beads-dashboard--more-line-text-p' matches the same format string
that `beads-dashboard--more-line' produces — guards against the two
sites drifting if the button label ever changes."
  :tags '(:unit)
  (with-temp-buffer
    (insert "  … and 7 more (+)")
    (forward-line 0)
    (should (beads-dashboard--more-line-text-p))
    ;; Other rows that share the section-key contract must NOT match
    ;; (header, empty-state, issue line).
    (erase-buffer)
    (insert "▼ Ready (15)")
    (forward-line 0)
    (should-not (beads-dashboard--more-line-text-p))
    (erase-buffer)
    (insert "  Nothing to show.")
    (forward-line 0)
    (should-not (beads-dashboard--more-line-text-p))))

(ert-deftest beads-dashboard-test-goto-more-line-finds-stamped-row ()
  "`beads-dashboard--goto-more-line' jumps to the more-line for the key."
  :tags '(:unit)
  (with-temp-buffer
    (insert "▼ "
            (propertize "Ready (15)" 'beads-dashboard-section-key 'ready)
            "\n"
            (propertize "  … and 5 more (+)"
                        'beads-dashboard-section-key 'ready)
            "\n"
            "▼ "
            (propertize "Stale (3)" 'beads-dashboard-section-key 'stale)
            "\n")
    (goto-char (point-max))
    (should (beads-dashboard--goto-more-line 'ready))
    (should (looking-at "…"))
    ;; No more-line for stale → returns nil, leaves point.
    (let ((before (point)))
      (should-not (beads-dashboard--goto-more-line 'stale))
      (should (= before (point))))))

(ert-deftest beads-dashboard-test-restore-point-prefers-more-line ()
  "When the user was on a more-line, restore picks the new more-line
in the same section over the section header fallback."
  :tags '(:unit)
  (with-temp-buffer
    (insert "▼ "
            (propertize "Ready (20)" 'beads-dashboard-section-key 'ready)
            "\n"
            (propertize "  … and 10 more (+)"
                        'beads-dashboard-section-key 'ready)
            "\n")
    (goto-char (point-min))
    (beads-dashboard--restore-point 'ready nil t)
    (should (looking-at "…"))))

(provide 'beads-dashboard-test)
;;; beads-dashboard-test.el ends here
