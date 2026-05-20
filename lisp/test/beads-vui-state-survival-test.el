;;; beads-vui-state-survival-test.el --- vui :state survival spike -*- lexical-binding: t; -*-

;;; Commentary:

;; Spike for bde-deqx.1 — "Verify vui-set-state survives across remount
;; in beads.el".
;;
;; The Ralph launcher (bde-deqx.5) needs intra-component reactivity for
;; editable parameter rows that update a live prompt+argv preview.  Every
;; existing vui surface in beads.el uses the remount-on-external-event
;; pattern (call `vui-mount' afresh in response to a hook) — see
;; `beads-agent-ralph-dashboard.el:687-705' (`beads-agent-ralph-dashboard-render')
;; which rebuilds the instance tree on every controller update.  The
;; `beads-dashboard.el:211' docstring states that `:state' "does not
;; survive `vui-mount' reliably" and that comment is the reason this
;; spike exists.
;;
;; What this spike actually verifies:
;;
;;   1. Within a single mount cycle, `vui-set-state' updates the
;;      component instance's state plist (vui.el:1211) and triggers a
;;      reconciling re-render via `vui--rerender-instance' (vui.el:1880)
;;      that preserves the existing instance tree.  State survives.
;;
;;   2. Calling `vui-mount' again with the same component vnode creates
;;      a NEW root instance via `vui--create-instance' (vui.el:3036,
;;      called at vui.el:2250) which re-initialises `:state' from the
;;      component's `initial-state-fn' (vui.el:2259-2262).  Any prior
;;      state from the previous mount is gone — by design, not by bug.
;;
;; Decision (recorded on bde-deqx.1):
;;
;;   The Ralph launcher's parameter editing happens within a single
;;   mount, so option (A) — pure-vui intra-component state via
;;   `:state' + `vui-set-state' — is the right call.  Persistence
;;   across an external remount (if ever needed) belongs in
;;   buffer-local vars threaded as props, mirroring how
;;   `beads-dashboard--root' lifts `collapsed' to a serialised
;;   visibility cache (beads-dashboard.el:212-216).

;;; Code:

(require 'ert)
(require 'vui)

;; Spike component.  Counter held in `:state', mutated by clicking [+]
;; / [-] which call `vui-set-state'.  Re-render is reactive.

(vui-defcomponent beads-vui-state-survival--counter ()
  "Throwaway counter for the bde-deqx.1 spike."
  :state ((count 0))
  :render
  (vui-vstack
    (vui-text (format "Count: %d" count))
    (vui-button "+" :on-click (lambda () (vui-set-state :count (1+ count))))
    (vui-button "-" :on-click (lambda () (vui-set-state :count (1- count))))))

(defun beads-vui-state-survival--press-button (label)
  "Search the current buffer for a `push-button' with tag LABEL and press it.
Signals an error if no such button is found."
  (goto-char (point-min))
  (let (found)
    (while (and (not found) (not (eobp)))
      (let ((w (widget-at (point))))
        (when (and w
                   (eq (widget-type w) 'push-button)
                   (equal (widget-get w :tag) label))
          (setq found w)))
      (unless found (forward-char 1)))
    (unless found
      (error "No push-button with tag %S found in buffer %s"
             label (buffer-name)))
    (widget-apply found :action)))

(ert-deftest beads-vui-state-survival-test-state-survives-within-mount ()
  "Within one mount, clicking [+] increments `count' via `vui-set-state'.
Reconciliation preserves the root instance, so `:state' persists
across the re-render."
  :tags '(:unit :spike)
  (let ((buf (generate-new-buffer " *beads-vui-spike*")))
    (unwind-protect
        (let ((instance
               (vui-mount (vui-component 'beads-vui-state-survival--counter)
                          (buffer-name buf))))
          (with-current-buffer buf
            ;; Initial render: state starts at 0.
            (should (= 0 (plist-get (vui-instance-state instance) :count)))
            (should (string-match-p "Count: 0" (buffer-string)))
            ;; Click [+] twice, [-] once.
            (beads-vui-state-survival--press-button "+")
            (vui-flush-sync)
            (beads-vui-state-survival--press-button "+")
            (vui-flush-sync)
            (beads-vui-state-survival--press-button "-")
            (vui-flush-sync)
            ;; The instance object is the same; its `state' slot was
            ;; mutated in place by `vui-set-state'.
            (should (= 1 (plist-get (vui-instance-state instance) :count)))
            (should (string-match-p "Count: 1" (buffer-string)))))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest beads-vui-state-survival-test-state-resets-across-vui-mount ()
  "Calling `vui-mount' again creates a fresh root instance whose
`:state' is reinitialised from the component definition.  Any state
mutated in the previous mount is gone."
  :tags '(:unit :spike)
  (let ((buf (generate-new-buffer " *beads-vui-spike*")))
    (unwind-protect
        (let ((first
               (vui-mount (vui-component 'beads-vui-state-survival--counter)
                          (buffer-name buf))))
          (with-current-buffer buf
            (beads-vui-state-survival--press-button "+")
            (vui-flush-sync)
            (should (= 1 (plist-get (vui-instance-state first) :count))))
          ;; Remount.  `vui-mount' calls `vui--create-instance' which
          ;; allocates a new struct and re-runs `initial-state-fn'.
          (let ((second
                 (vui-mount (vui-component 'beads-vui-state-survival--counter)
                            (buffer-name buf))))
            (should-not (eq first second))
            (should (= 0 (plist-get (vui-instance-state second) :count)))
            (with-current-buffer buf
              (should (string-match-p "Count: 0" (buffer-string))))))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(provide 'beads-vui-state-survival-test)

;;; beads-vui-state-survival-test.el ends here
