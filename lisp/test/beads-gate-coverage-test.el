;;; beads-gate-coverage-test.el --- Extra coverage for beads-gate -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; Additional `:unit' coverage for the gate porcelain in
;; `beads-gate.el': field access, normalization, display formatting,
;; list loading/population, view toggles, the create/check/resolve/
;; add-waiter/discover actions (with the store mocked), the report and
;; detail renderers, and the molecule glyph seam.  These complement the
;; REQ-SF-030 .. REQ-SF-033 tests in `beads-gate-test.el'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'beads-gate)
(require 'beads-command-gate)

(defconst beads-gate-cov--alist
  '((id . "gate-1")
    (issue_type . "gate")
    (status . "open")
    (title . "Gate: timer")
    (await_type . "timer")
    (await_id . "run-7")
    (timeout . 7200000000000)
    (waiters . ("beads.el/task" "beads.el/other"))
    (metadata . ((repo . "r0man/beads.el")))
    (created_at . "2026-10-07T10:00:00Z")
    (updated_at . "2026-10-07T11:00:00Z")
    (description . "Ad-hoc gate blocking bd-abc\n\nReason: Wait for CI"))
  "A rich raw gate alist used by the coverage tests.")

;;; Field access

(ert-deftest beads-gate-cov--get-test ()
  "Unit test: field access handles issues, alists, aliases, and junk."
  :tags '(:unit)
  (let ((issue (beads-gate--coerce beads-gate-cov--alist)))
    (should (equal (beads-gate--get issue 'id) "gate-1"))
    (should (equal (beads-gate--get issue 'await-type) "timer")))
  (let ((alist beads-gate-cov--alist))
    (should (equal (beads-gate--get alist 'id) "gate-1"))
    (should (equal (beads-gate--get alist 'issue-type) "gate"))
    (should (equal (beads-gate--get alist 'await-type) "timer"))
    (should (equal (beads-gate--get alist 'await-id) "run-7"))
    (should (equal (beads-gate--get alist 'created-at) "2026-10-07T10:00:00Z"))
    (should (equal (beads-gate--get alist 'updated-at) "2026-10-07T11:00:00Z"))
    (should-not (beads-gate--get alist 'missing-field)))
  (should-not (beads-gate--get 42 'id)))

(ert-deftest beads-gate-cov--description-test ()
  "Unit test: descriptions ignore blanks and non-strings."
  :tags '(:unit)
  (should (equal (beads-gate--description beads-gate-cov--alist)
                 "Ad-hoc gate blocking bd-abc\n\nReason: Wait for CI"))
  (should-not (beads-gate--description '((id . "g") (description . ""))))
  (should-not (beads-gate--description '((id . "g") (description . 7))))
  (should-not (beads-gate--description '((id . "g")))))

(ert-deftest beads-gate-cov--coerce-and-normalize-test ()
  "Unit test: normalization coerces vectors/lists and drops junk."
  :tags '(:unit)
  (let ((issue (beads-gate--coerce (beads-issue :id "x" :await-type "human"))))
    (should (beads-issue-p issue))
    (should (equal (oref issue id) "x")))
  (should (beads-issue-p (beads-gate--coerce beads-gate-cov--alist)))
  (should-not (beads-gate--coerce 42))
  (should-not (beads-gate--coerce "not a gate"))
  (should-not (beads-gate--normalize nil))
  (should-not (beads-gate--normalize 42))
  (should (= 1 (length (beads-gate--normalize (vector beads-gate-cov--alist)))))
  (should (= 1 (length (beads-gate--normalize (list beads-gate-cov--alist)))))
  (should (= 1 (length (beads-gate--normalize (list beads-gate-cov--alist 42))))))

(ert-deftest beads-gate-cov-display-issue-method-test ()
  "Unit test: the `beads-issue' display method delegates to the plist."
  :tags '(:unit)
  (let ((display (beads-gate-display (beads-gate--coerce beads-gate-cov--alist))))
    (should (equal (plist-get display :id) "gate-1"))
    (should (equal (plist-get display :await-id) "run-7"))
    (should (equal (plist-get display :repo) "r0man/beads.el"))))

(ert-deftest beads-gate-cov--parse-repo-and-expiry-test ()
  "Unit test: repo metadata parsing and expiry edge cases."
  :tags '(:unit)
  (should (equal (beads-gate--parse-repo beads-gate-cov--alist)
                 "r0man/beads.el"))
  (should-not (beads-gate--parse-repo '((id . "g"))))
  (should-not (beads-gate--parse-repo '((id . "g") (metadata . "nope"))))
  (should-not (beads-gate--expiry "2026-10-07T10:00:00Z" nil))
  (should-not (beads-gate--expiry nil 1000))
  (should-not (beads-gate--expiry "2026-10-07T10:00:00Z" 0))
  (should (beads-gate--expiry "2026-10-07T10:00:00Z" 3600000000000))
  (should-not (beads-gate--expiry "not-a-date" 1000)))

;;; Formatting

(ert-deftest beads-gate-cov-format-helpers-test ()
  "Unit test: the remaining cell formatters."
  :tags '(:unit)
  (should (eq (beads-gate--type-face "timer") 'beads-gate-type-timer))
  (should (eq (beads-gate--type-face "unknown") 'default))
  (should (string-match-p "timer" (beads-gate--format-type "timer")))
  (should (equal (beads-gate--format-type nil) " "))
  (should (equal (beads-gate--format-await "timer" "run-1") "run-1"))
  (should (equal (beads-gate--format-await "human" "") ""))
  (should (equal (beads-gate--format-await "human" nil) ""))
  (should (equal (beads-gate--format-timeout beads-gate-cov--alist) "2h"))
  (should (equal (beads-gate--format-blocks beads-gate-cov--alist) "bd-abc"))
  (should (equal (beads-gate--format-blocks '((id . "g"))) ""))
  (should (string-match-p "open" (beads-gate--format-status beads-gate-cov--alist))))

;;; List loading and population

(ert-deftest beads-gate-cov-list--command-test ()
  "Unit test: the list command mirrors the buffer's --all toggle."
  :tags '(:unit)
  (let ((beads-gate-list--show-all t))
    (let ((cmd (beads-gate-list--command)))
      (should (oref cmd all))
      (should (oref cmd json)))))

(ert-deftest beads-gate-cov-list--populate-test ()
  "Unit test: populating builds one pager entry per normalized gate."
  :tags '(:unit)
  (with-temp-buffer
    (beads-pager-mode 1)
    (beads-gate-list--populate (vector beads-gate-cov--alist))
    (should-not beads-gate-list--last-error)
    (should (= 1 (beads-pager--total-count))))
  (with-temp-buffer
    (beads-pager-mode 1)
    (setq beads-gate-list--type-filter "human")
    (beads-gate-list--populate (vector beads-gate-cov--alist))
    (should (= 0 (beads-pager--total-count)))))

(ert-deftest beads-gate-cov-list--populate-error-test ()
  "Unit test: a load error is recorded and the buffer stays empty."
  :tags '(:unit)
  (with-temp-buffer
    (beads-pager-mode 1)
    (beads-gate-list--populate-error (list 'error "boom"))
    (should (string-match-p "boom" beads-gate-list--last-error))
    (should (= 0 (beads-pager--total-count)))))

(ert-deftest beads-gate-cov-list-refresh-test ()
  "Unit test: refresh spawns an async gate-list command."
  :tags '(:unit)
  (let ((captured nil))
    (cl-letf (((symbol-function 'beads-command-execute-async)
               (lambda (cmd _ok _err &rest _) (setq captured cmd))))
      (with-temp-buffer
        (beads-gate-list-refresh)))
    (should (beads-command-gate-list-p captured))))

;;; Point and view toggles

(ert-deftest beads-gate-cov-list-toggles-test ()
  "Unit test: navigation, quit, show-all, and type-filter toggles."
  :tags '(:unit)
  (cl-letf (((symbol-function 'beads-gate-list-refresh) #'ignore)
            ((symbol-function 'quit-window) (lambda (&rest _) 'quit)))
    (with-temp-buffer
      (insert "one\ntwo\n")
      (goto-char (point-min))
      (beads-gate-list-next)
      (should (= (line-number-at-pos) 2))
      (beads-gate-list-previous)
      (should (= (line-number-at-pos) 1))
      (should-not (beads-gate-list--current-id))
      (should (eq (beads-gate-list-quit) 'quit)))
    (with-temp-buffer
      (let ((beads-gate-list--show-all nil))
        (beads-gate-list-toggle-all)
        (should beads-gate-list--show-all)
        (beads-gate-list-toggle-all)
        (should-not beads-gate-list--show-all)))
    (with-temp-buffer
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) "timer")))
        (beads-gate-list-set-type)
        (should (equal beads-gate-list--type-filter "timer")))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) "")))
        (beads-gate-list-set-type)
        (should-not beads-gate-list--type-filter)))))

;;; Entry point

(ert-deftest beads-gate-cov-list-entry-test ()
  "Unit test: `beads-gate-list' creates and displays the utility buffer."
  :tags '(:unit)
  (cl-letf (((symbol-function 'beads-command-execute-async) #'ignore)
            ((symbol-function 'pop-to-buffer) (lambda (buf &rest _) buf)))
    (let ((buf (beads-gate-list)))
      (should (buffer-live-p buf))
      (with-current-buffer buf
        (should (derived-mode-p 'beads-gate-list-mode))
        (should tabulated-list-format))
      (kill-buffer buf))))

;;; Create

(ert-deftest beads-gate-cov--result-id-test ()
  "Unit test: created gate ids come from alists or plain strings."
  :tags '(:unit)
  (should (equal (beads-gate--result-id '((id . "g1"))) "g1"))
  (should (equal (beads-gate--result-id "  g2\n") "g2"))
  (should-not (beads-gate--result-id '((status . "open"))))
  (should-not (beads-gate--result-id 42)))

(ert-deftest beads-gate-cov--read-create-test ()
  "Unit test: the create prompt assembles the documented flags."
  :tags '(:unit)
  (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "gh:run"))
            ((symbol-function 'beads-reader-issue-id) (lambda (&rest _) "bd-1"))
            ((symbol-function 'read-string)
             (lambda (prompt &rest _)
               (cond ((string-match-p "Await" prompt) "release.yml")
                     ((string-match-p "Timeout" prompt) "30m")
                     ((string-match-p "Reason" prompt) "CI")))))
    (let ((cmd (beads-gate--read-create)))
      (should (equal (oref cmd gate-type) "gh:run"))
      (should (equal (oref cmd blocks) "bd-1"))
      (should (equal (oref cmd await-id) "release.yml"))
      (should (equal (oref cmd timeout) "30m"))
      (should (equal (oref cmd reason) "CI"))))
  ;; Blank optional fields are dropped.
  (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "human"))
            ((symbol-function 'beads-reader-issue-id) (lambda (&rest _) "bd-2"))
            ((symbol-function 'read-string) (lambda (&rest _) "")))
    (let ((cmd (beads-gate--read-create)))
      (should-not (oref cmd await-id))
      (should-not (oref cmd timeout))
      (should-not (oref cmd reason)))))

(ert-deftest beads-gate-cov--refresh-open-lists-test ()
  "Unit test: open gate lists are refreshed in place."
  :tags '(:unit)
  (let ((buf (generate-new-buffer " *gate-list-test*"))
        (refreshed 0))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'beads-buffer-find-utility-buffers)
                     (lambda (&rest _) (list buf)))
                    ((symbol-function 'beads-gate-list-refresh)
                     (lambda () (setq refreshed (1+ refreshed)))))
            (beads-gate--refresh-open-lists))
          (should (= refreshed 1)))
      (kill-buffer buf))))

(ert-deftest beads-gate-cov-create-action-test ()
  "Unit test: `beads-gate-create' executes and returns the new id."
  :tags '(:unit)
  (cl-letf (((symbol-function 'beads-gate--read-create)
             (lambda () (beads-gate-create-command "human" "bd-1")))
            ((symbol-function 'beads-command-execute)
             (lambda (&rest _) '((id . "gate-new"))))
            ((symbol-function 'beads-gate--refresh-open-lists) #'ignore))
    (should (equal (beads-gate-create) "gate-new"))))

;;; Check

(ert-deftest beads-gate-cov--show-output-test ()
  "Unit test: the report buffer carries the title and body."
  :tags '(:unit)
  (cl-letf (((symbol-function 'display-buffer) (lambda (buf &rest _) buf)))
    (let ((buf (beads-gate--show-output "Gate check" "all good")))
      (unwind-protect
          (with-current-buffer buf
            (should (derived-mode-p 'special-mode))
            (should (string-match-p "Gate check" (buffer-string)))
            (should (string-match-p "all good" (buffer-string)))
            ;; nil output is rendered as an empty body.
            (should (beads-gate--show-output "T" nil)))
        (kill-buffer buf)))))

(ert-deftest beads-gate-cov-check-action-test ()
  "Unit test: a check runs the hook for resolved gates and reports."
  :tags '(:unit)
  (let ((seen nil))
    (let ((collect (lambda (id) (push id seen))))
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "timer"))
                ((symbol-function 'beads-command-execute)
                 (lambda (&rest _) "be-ig05: resolved - timer expired"))
                ((symbol-function 'beads-gate--show-output)
                 (lambda (&rest _) 'buffer))
                ((symbol-function 'beads-gate--refresh-open-lists) #'ignore))
        (add-hook 'beads-gate-closed-hook collect)
        (unwind-protect
            (should (equal (beads-gate-check)
                           "be-ig05: resolved - timer expired"))
          (remove-hook 'beads-gate-closed-hook collect)))
      (should (equal seen '("be-ig05"))))))

(ert-deftest beads-gate-cov-check-dry-run-test ()
  "Unit test: a dry-run check still reports and never closes gates."
  :tags '(:unit)
  (let ((captured nil))
    (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) ""))
              ((symbol-function 'beads-command-execute)
               (lambda (cmd) (setq captured cmd) "nothing resolved"))
              ((symbol-function 'beads-gate--show-output)
               (lambda (&rest _) 'buffer))
              ((symbol-function 'beads-gate--refresh-open-lists) #'ignore))
      (beads-gate-check t)
      (should (oref captured dry-run)))))

;;; Resolve, waiter, discover

(ert-deftest beads-gate-cov--read-gate-id-test ()
  "Unit test: the gate id reader defaults to point/detail identity."
  :tags '(:unit)
  (with-temp-buffer
    (beads-gate-detail-mode)
    (let ((beads-gate-detail--gate-id "gate-detail"))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (_prompt &optional _initial _history default)
                   default)))
        (should (equal (beads-gate--read-gate-id "Gate") "gate-detail")))))
  (with-temp-buffer
    (cl-letf (((symbol-function 'read-string)
               (lambda (prompt &rest _) (concat "typed:" prompt))))
      (should (string-match-p "typed:" (beads-gate--read-gate-id "Gate"))))))

(ert-deftest beads-gate-cov-resolve-action-test ()
  "Unit test: resolve executes with the trimmed reason and runs the hook."
  :tags '(:unit)
  (let ((captured nil) (seen nil))
    (let ((collect (lambda (id) (push id seen))))
      (cl-letf (((symbol-function 'beads-command-execute)
                 (lambda (cmd) (setq captured cmd)))
                ((symbol-function 'beads-gate--refresh-open-lists) #'ignore))
        (add-hook 'beads-gate-closed-hook collect)
        (unwind-protect
            (should (equal (beads-gate-resolve "gate-1" "  done  ") "gate-1"))
          (remove-hook 'beads-gate-closed-hook collect)))
      (should (equal (oref captured gate-id) "gate-1"))
      (should (equal (oref captured reason) "done"))
      (should (equal seen '("gate-1"))))))

(ert-deftest beads-gate-cov-add-waiter-action-test ()
  "Unit test: adding a waiter trims and requires a non-empty waiter."
  :tags '(:unit)
  (let ((captured nil))
    (cl-letf (((symbol-function 'beads-command-execute)
               (lambda (cmd) (setq captured cmd)))
              ((symbol-function 'beads-gate--refresh-open-lists) #'ignore))
      (should (equal (beads-gate-add-waiter "gate-1" " beads.el/agent ")
                     "gate-1"))
      (should (equal (oref captured waiter-id) "beads.el/agent"))
      (should-error (beads-gate-add-waiter "gate-1" "   ") :type 'user-error))))

(ert-deftest beads-gate-cov-discover-action-test ()
  "Unit test: discover forwards its flags and reports the output."
  :tags '(:unit)
  (let ((captured nil))
    (cl-letf (((symbol-function 'beads-command-execute)
               (lambda (cmd) (setq captured cmd) "discovered"))
              ((symbol-function 'beads-gate--show-output)
               (lambda (&rest _) 'buffer))
              ((symbol-function 'beads-gate--refresh-open-lists) #'ignore))
      (should (equal (beads-gate-discover "main" "5" "1h" t) "discovered"))
      (should (equal (oref captured branch) "main"))
      (should (equal (oref captured limit) "5"))
      (should (equal (oref captured max-age) "1h"))
      (should (oref captured dry-run)))
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) ""))
              ((symbol-function 'beads-command-execute)
               (lambda (cmd) (setq captured cmd) "out"))
              ((symbol-function 'beads-gate--show-output)
               (lambda (&rest _) 'buffer))
              ((symbol-function 'beads-gate--refresh-open-lists) #'ignore))
      (beads-gate-discover)
      (should-not (oref captured branch))
      (should-not (oref captured limit))
      (should-not (oref captured max-age)))))

;;; Detail rendering

(ert-deftest beads-gate-cov-insert-helpers-test ()
  "Unit test: the section/header/list insertion helpers."
  :tags '(:unit)
  (with-temp-buffer
    (beads-gate--insert-header "Type" "timer" 'bold)
    (should (string-match-p "Type: timer" (buffer-string))))
  (with-temp-buffer
    (beads-gate--insert-header "Empty" nil)
    (should (string-match-p "Empty: " (buffer-string))))
  (with-temp-buffer
    (beads-gate--insert-section "Blocks" (lambda () (insert "  bd-1\n")))
    (should (string-match-p "BLOCKS" (buffer-string)))
    (should (string-match-p "bd-1" (buffer-string))))
  ;; A renderer that inserts nothing removes the section header.
  (with-temp-buffer
    (beads-gate--insert-section "Waiters" #'ignore)
    (should (equal (buffer-string) "")))
  (with-temp-buffer
    (beads-gate--insert-issue-list '("bd-1" "bd-2"))
    (should (string-match-p "bd-1" (buffer-string)))
    (should (string-match-p "bd-2" (buffer-string))))
  (let ((value "2026-10-07T10:00:00Z"))
    (should (equal (beads-gate--format-timestamp value)
                   (format-time-string "%Y-%m-%d %H:%M:%S" (date-to-time value)))))
  ;; Non-string and unparseable values pass through unchanged.
  (should (equal (beads-gate--format-timestamp "junk") "junk"))
  (should-not (beads-gate--format-timestamp nil))
  (should (= (beads-gate--format-timestamp 7) 7)))

(ert-deftest beads-gate-cov--render-test ()
  "Unit test: the detail renderer emits the sectioned gate view."
  :tags '(:unit)
  (let ((gate (beads-gate--coerce beads-gate-cov--alist)))
    (with-temp-buffer
      (beads-gate-detail-mode)
      (let ((inhibit-read-only t))
        (beads-gate--render gate))
      (should (string-match-p "gate-1 — gate" (buffer-string)))
      (should (string-match-p "Await-id" (buffer-string)))
      (should (string-match-p "r0man/beads.el" (buffer-string)))
      (should (string-match-p "Reason" (buffer-string)))
      (should (string-match-p "bd-abc" (buffer-string)))
      (should (string-match-p "beads.el/task" (buffer-string)))
      (should (string-match-p "repo r0man/beads.el" header-line-format))))
  ;; A gate without a repo uses the "(current)" placeholder.
  (with-temp-buffer
    (beads-gate-detail-mode)
    (let ((inhibit-read-only t))
      (beads-gate--render '((id . "g2") (await_type . "human")
                            (status . "open"))))
    (should (string-match-p "(current)" (buffer-string)))))

(ert-deftest beads-gate-cov--load-test ()
  "Unit test: loading normalizes a single gate from a vector or alist."
  :tags '(:unit)
  (cl-letf (((symbol-function 'beads-command-execute)
             (lambda (&rest _) (vector beads-gate-cov--alist))))
    (should (beads-issue-p (beads-gate--load "gate-1"))))
  (cl-letf (((symbol-function 'beads-command-execute)
             (lambda (&rest _) beads-gate-cov--alist)))
    (should (equal (beads-gate--get (beads-gate--load "gate-1") 'id) "gate-1"))))

(ert-deftest beads-gate-cov-detail-refresh-test ()
  "Unit test: the detail refresh only reloads for a known gate id."
  :tags '(:unit)
  (with-temp-buffer
    (beads-gate-detail-mode)
    (let ((opened nil))
      (cl-letf (((symbol-function 'beads-gate-open)
                 (lambda (id) (setq opened id))))
        (setq beads-gate-detail--gate-id "gate-1")
        (beads-gate-detail-refresh)
        (should (equal opened "gate-1"))
        (setq beads-gate-detail--gate-id nil)
        (beads-gate-detail-refresh)
        (should (equal opened "gate-1"))))))

(ert-deftest beads-gate-cov-open-test ()
  "Unit test: opening builds the detail buffer and rejects an empty id."
  :tags '(:unit)
  (cl-letf (((symbol-function 'beads-command-execute)
             (lambda (&rest _) beads-gate-cov--alist))
            ((symbol-function 'pop-to-buffer) (lambda (buf &rest _) buf)))
    (let ((buf (beads-gate-open "gate-1")))
      (unwind-protect
          (with-current-buffer buf
            (should (derived-mode-p 'beads-gate-detail-mode))
            (should (equal beads-gate-detail--gate-id "gate-1"))
            (should (string-match-p "gate-1" (buffer-string))))
        (kill-buffer buf))))
  (should-error (beads-gate-open nil) :type 'user-error)
  (should-error (beads-gate-open "") :type 'user-error))

(provide 'beads-gate-coverage-test)
;;; beads-gate-coverage-test.el ends here
