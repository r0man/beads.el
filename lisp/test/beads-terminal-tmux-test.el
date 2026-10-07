;;; beads-terminal-tmux-test.el --- Tests for beads-terminal-tmux -*- lexical-binding: t -*-

;; Copyright (C) 2026

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; Acceptance tests for the tmux attach/status/mouse/scroll subsystem
;; ported from gascity.el's gascity-terminal.el (WI-14).  No test here
;; drives a real terminal backend or a real tmux server; the attach
;; pre-step and every host command run through `beads-terminal-tmux--run-async',
;; which these tests answer in-process.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'beads-terminal-tmux)
(require 'beads-terminal)
(require 'beads-test-helpers)

;;; The host stub: every attach/status tmux call is one async host
;;; script (`beads-terminal-tmux--run-async'); tests answer it.

(defvar beads-terminal-tmux-test--host-scripts nil
  "(DIR SCRIPT) of every host script the terminal ran, newest first.")

(defun beads-terminal-tmux-test--answer (script)
  "Default answer of a healthy host to SCRIPT: (EXIT . STDOUT)."
  (cond
   ((string-match-p "has-session" script)
    (cons 0 (concat "beads-tmux:/opt/bin/tmux\n"
                    (if (string-match-p "\\[ -d " script) "beads-dir-ok\n" "")
                    (if (string-match-p "infocmp" script) "beads-term-ok\n" "")
                    "beads-ok\n")))
   ((string-match-p "list-windows" script)
    (cons 0 "1\t1:claude*\n0\t0:bash\nbeads-status-left\ngastown.mayor\n"))
   (t (cons 0 ""))))

(defmacro beads-terminal-tmux-test--with-host (answer &rest body)
  "Run BODY with the terminal's host scripts answered by ANSWER.
ANSWER is a function of the script returning (EXIT . STDOUT), or nil
for `beads-terminal-tmux-test--answer'.  The callback runs at once."
  (declare (indent 1))
  `(let ((beads-terminal-tmux-test--host-scripts nil))
     (cl-letf (((symbol-function 'beads-terminal-tmux--run-async)
                (lambda (dir script callback)
                  (push (list dir script) beads-terminal-tmux-test--host-scripts)
                  (funcall callback (funcall (or ,answer #'beads-terminal-tmux-test--answer)
                                             script))
                  nil))
               ;; Nothing here may run a synchronous process.
               ((symbol-function 'process-file)
                (lambda (&rest a) (error "Synchronous process-file %S" a)))
               ((symbol-function 'call-process)
                (lambda (&rest a) (error "Synchronous call-process %S" a))))
       ,@body)))

(defun beads-terminal-tmux-test--host-script-p (regexp)
  "Return non-nil when a recorded host script matches REGEXP."
  (seq-some (lambda (e) (string-match-p regexp (nth 1 e)))
            beads-terminal-tmux-test--host-scripts))

;;; TRAMP mock method (offline remote coverage)

(require 'tramp)

(defconst beads-terminal-tmux-test--mock-directory
  (format "/mock::%s" temporary-file-directory)
  "TRAMP name of the local temp directory behind the mock method.")

(defun beads-terminal-tmux-test--ensure-mock-method ()
  "Register the tramp-tests.el \"mock\" method (idempotent)."
  (unless (assoc "mock" tramp-methods)
    (add-to-list 'tramp-methods
                 '("mock"
                   (tramp-login-program "sh")
                   (tramp-login-args (("-i")))
                   (tramp-direct-async ("-c"))
                   (tramp-remote-shell "/bin/sh")
                   (tramp-remote-shell-args ("-c"))
                   (tramp-connection-timeout 10)))
    (add-to-list 'tramp-default-host-alist
                 `("\\`mock\\'" nil ,(system-name)))))

(defmacro beads-terminal-tmux-test--with-mock-remote (&rest body)
  "Run BODY with a remote `default-directory' via the TRAMP mock method.
Skips the calling test when the mock connection cannot be established."
  (declare (indent 0) (debug t))
  `(progn
     (beads-terminal-tmux-test--ensure-mock-method)
     (let ((tramp-verbose 0)
           (default-directory beads-terminal-tmux-test--mock-directory))
       (skip-unless (ignore-errors (file-directory-p default-directory)))
       ,@body)))

(ert-deftest beads-terminal-tmux-test-socket-args ()
  "A real socket yields -L NAME; nil/empty/\"default\" yield nothing."
  (should (equal (beads-terminal-tmux--socket-args "bright-lights")
                 '("-L" "bright-lights")))
  (should (null (beads-terminal-tmux--socket-args "default")))
  (should (null (beads-terminal-tmux--socket-args "")))
  (should (null (beads-terminal-tmux--socket-args nil))))

(ert-deftest beads-terminal-tmux-test-mouse-ensure-script ()
  "`beads-terminal-tmux--mouse-ensure-script' turns the session's tmux mouse
option on and installs the wheel-to-bottom copy-mode binding
(DESIGN-agent-scrolling.md D2/D3)."
  (let ((script (beads-terminal-tmux--mouse-ensure-script "sess" "sock")))
    (should (string-search "tmux -L sock set-option -t sess mouse on" script))
    (should (string-search
             "tmux -L sock bind -T copy-mode WheelDownPane" script))
    ;; The compound command must reach tmux as ONE argv word in the
    ;; shape of `beads-terminal-tmux--mouse-wheel-command' (verified live,
    ;; tmux 3.7c): a word ending in plain ";" is tmux's command
    ;; separator - the `if' then runs at bind time ("not in a mode",
    ;; exit 1) and the key is bound to bare select-pane; an embedded
    ;; "\;" is a LITERAL `;' once the binding fires ("too many
    ;; arguments").  Don't string-match the quoting style - assert the
    ;; bound VALUE through a real tmux on a scratch socket, and fire
    ;; the binding in copy mode.  WheelDownPane itself needs a real
    ;; mouse, so the functional pass binds the same command to C-o,
    ;; which `send-keys' dispatches through the same key table.
    (skip-unless (executable-find "tmux"))
    (let* ((socket (concat "gc-mouse-ensure-" (number-to-string (emacs-pid))))
           (tmux (concat (executable-find "tmux") " -L " socket))
           (kill (concat tmux " kill-server >/dev/null 2>&1"))
           ;; The live pass runs the ensure script for its OWN scratch
           ;; socket, so the tmux invocations and the bound script agree.
           (script (beads-terminal-tmux--mouse-ensure-script "probe" socket))
           (out (generate-new-buffer " *gc-mouse-ensure-probe*"))
           pane read)
      (unwind-protect
          (progn
            (call-process "sh" nil nil nil "-c"
                          (concat tmux " new-session -d -s probe"))
            (call-process "sh" nil nil nil "-c"
                          (concat tmux " send-keys -t probe 'seq 1 200' Enter"))
            (call-process "sh" nil nil nil "-c" script)
            ;; The binding is installed and its value is the compound:
            ;; if-shell with the cancel-then-scroll brace group - not
            ;; bare select-pane, and not a literal-`;'-argument.
            (with-current-buffer out
              (erase-buffer)
              (call-process "sh" nil t nil "-c"
                            (concat tmux " list-keys -T copy-mode"))
              (should (string-match-p
                       (concat "WheelDownPane[ \\t]+if-shell -F"
                               "[ \\t]+\\\"#{==:#{scroll_position},0}\\\""
                               "[ \\t]+\\\"send -X cancel\\\""
                               "[ \\t]+{ select-pane ; send-keys -X -N 5"
                               " scroll-down }")
                       (buffer-substring (point-min) (point-max)))))
            ;; Fire the same command through a sendable key: scrolling
            ;; keeps copy mode, and at the bottom it leaves for the
            ;; live tail.
            (call-process "sh" nil nil nil "-c"
                          (beads-terminal-tmux--tmux-sh
                           socket "bind" "-T" "copy-mode" "C-o"
                           beads-terminal-tmux--mouse-wheel-command))
            (with-current-buffer out
              (erase-buffer)
              (call-process "sh" nil t nil "-c"
                            (concat tmux " list-panes -t probe"
                                    " -F '#{pane_id}'"))
              (setq pane (string-trim
                          (buffer-substring (point-min) (point-max)))))
            (should (string-prefix-p "%" pane))
            (setq read (lambda (fmt)
                         (with-current-buffer out
                           (erase-buffer)
                           (call-process "sh" nil t nil "-c"
                                         (concat tmux " display-message -p -t "
                                                 pane " '" fmt "'"))
                           (string-trim (buffer-substring
                                         (point-min) (point-max))))))
            ;; Wait (bounded) for the shell to have produced the
            ;; scrollback before entering copy mode.
            (let ((deadline (+ (float-time) 10)))
              (while (and (< (float-time) deadline)
                          (< (string-to-number
                              (funcall read "#{history_size}"))
                             50))
                (sit-for 0.1)))
            (call-process "sh" nil nil nil "-c"
                          (concat tmux " copy-mode -t " pane))
            (call-process "sh" nil nil nil "-c"
                          (concat tmux " send -X -t " pane " -N 10 scroll-up"))
            (call-process "sh" nil nil nil "-c"
                          (concat tmux " send-keys -t " pane " C-o"))
            (should (equal (funcall read "#{scroll_position}") "5"))
            (should (equal (funcall read "#{pane_in_mode}") "1"))
            ;; Wheel to the bottom: the position clamps at 0, and the
            ;; next wheel leaves copy mode (E8).
            (call-process "sh" nil nil nil "-c"
                          (concat tmux " send -X -t " pane
                                  " -N 20 scroll-down"))
            (call-process "sh" nil nil nil "-c"
                          (concat tmux " send-keys -t " pane " C-o"))
            (should (equal (funcall read "#{pane_in_mode}") "0")))
        (call-process "sh" nil nil nil "-c" kill)
        (kill-buffer out)))))

(ert-deftest beads-terminal-tmux-test-attach-script-mouse ()
  "The attach pre-step ensures tmux mouse only under
`beads-terminal-tmux-ensure-mouse'; with the option nil the script is
unchanged."
  (let (plain)
    (cl-letf ((beads-terminal-tmux-ensure-mouse nil))
      (setq plain (beads-terminal-tmux--attach-script "sess" "sock"
                                                   :status-off t)))
    (should (string-search "set-option -t sess status off" plain))
    (should-not (string-search "mouse" plain))
    (should-not (string-search "WheelDownPane" plain))
    ;; With the option at its default (t) the script is exactly the plain
    ;; one plus the mouse fragment: nothing else changes.
    (let ((script (beads-terminal-tmux--attach-script "sess" "sock"
                                                   :status-off t)))
      (should (equal plain
                     (string-replace
                      (beads-terminal-tmux--mouse-ensure-script "sess" "sock")
                      "" script))))))

(ert-deftest beads-terminal-tmux-test-status-teardown-restores-mouse ()
  "The teardown restores what the attach installed: the `status' override
\(when the mirror was installed) and, under
`beads-terminal-tmux-ensure-mouse', the session's `mouse' option and the
copy-mode `WheelDownPane' binding.  With the option nil only the status
override is reverted."
  (let ((buf (generate-new-buffer "*gc-agent-teardown-mouse*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (setq beads-terminal-tmux--status-session "sess"
                  beads-terminal-tmux--status-socket "sock"
                  beads-terminal-tmux--status-mirrored t))
          (let (script)
            (cl-letf (((symbol-function 'beads-terminal-tmux--run-async)
                       (lambda (_dir s _cb) (setq script s) nil)))
              (with-current-buffer buf (beads-terminal-tmux--status-teardown))
              (should (string-search
                       "tmux -L sock set-option -t sess -u status" script))
              (should (string-search
                       "tmux -L sock set-option -t sess -u mouse" script))
              (should (string-search
                       "tmux -L sock unbind -T copy-mode WheelDownPane"
                       script))))
          ;; Option nil: the mouse overrides are left alone.
          (let (script)
            (cl-letf (((symbol-function 'beads-terminal-tmux--run-async)
                       (lambda (_dir s _cb) (setq script s) nil))
                      (beads-terminal-tmux-ensure-mouse nil))
              (with-current-buffer buf (beads-terminal-tmux--status-teardown))
              (should (string-search
                       "tmux -L sock set-option -t sess -u status" script))
              (should-not (string-search "mouse" script))
              (should-not (string-search "unbind" script))))
          ;; No session recorded: no host round trip at all.
          (with-current-buffer buf
            (setq beads-terminal-tmux--status-session nil))
          (let (called)
            (cl-letf (((symbol-function 'beads-terminal-tmux--run-async)
                       (lambda (&rest _) (setq called t) nil)))
              (with-current-buffer buf (beads-terminal-tmux--status-teardown))
              (should-not called))))
      (kill-buffer buf))))

(ert-deftest beads-terminal-tmux-test-status-install-no-mirror-installs-teardown ()
  "With `beads-terminal-tmux-mode-line-status' nil the mirror is off, but the
buffer still gets the session locals and the kill-buffer teardown, so the
mouse ensure is restored even without the mirror (REQ-013)."
  (let ((buf (generate-new-buffer "*gc-agent-nomirror*")))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'beads-terminal-tmux--run-async)
                     (lambda (&rest _) nil))
                    (beads-terminal-tmux-mode-line-status nil))
            (with-current-buffer buf
              (beads-terminal-tmux--status-install buf "sess" "sock" nil t))
            (with-current-buffer buf
              (should (equal beads-terminal-tmux--status-session "sess"))
              (should-not beads-terminal-tmux--status-mirrored)
              (should (memq #'beads-terminal-tmux--status-teardown
                            kill-buffer-hook))
              (should-not beads-terminal-tmux--status-timer))))
      (kill-buffer buf))))

(ert-deftest beads-terminal-tmux-test-scroll-sequence ()
  "`beads-terminal-tmux--scroll-sequence' is the pure D1 translation table:
C-p/C-n become C-Up/C-Down bytes (E7), the paging keys become
PPage/NPage, M-</M-> ride tmux's own history-top/bottom bindings
(locked empirically), q/Esc leave copy mode — any other event
translates to nothing (and keeps the backend's behaviour)."
  (should (equal (beads-terminal-tmux--scroll-sequence ?\C-p) "\e[1;5A"))
  (should (equal (beads-terminal-tmux--scroll-sequence ?\C-n) "\e[1;5B"))
  (should (equal (beads-terminal-tmux--scroll-sequence ?\C-v) "\e[6~"))
  (should (equal (beads-terminal-tmux--scroll-sequence ?\M-v) "\e[5~"))
  (should (equal (beads-terminal-tmux--scroll-sequence 'next) "\e[6~"))
  (should (equal (beads-terminal-tmux--scroll-sequence 'prior) "\e[5~"))
  ;; Top/bottom jumps ride tmux's own history-top/history-bottom
  ;; bindings — single modified-key bytes, locked empirically (the
  ;; goto-prompt burst left a stuck prompt in the live pass).
  (should (equal (beads-terminal-tmux--scroll-sequence ?\M-<) "\e<"))
  (should (equal (beads-terminal-tmux--scroll-sequence ?\M->) "\e>"))
  (should (equal (beads-terminal-tmux--scroll-sequence ?q) "q"))
  (should (equal (beads-terminal-tmux--scroll-sequence 'escape) "\e"))
  ;; Not table keys: nil, so the backend keeps them.
  (should (null (beads-terminal-tmux--scroll-sequence ?a)))
  (should (null (beads-terminal-tmux--scroll-sequence 'f13))))

(ert-deftest beads-terminal-tmux-test-scroll-adapter-dispatch ()
  "`beads-terminal-tmux--send-raw' picks the backend's raw-key API from the
buffer's major mode (E6) and returns non-nil when it sent: vterm →
`vterm-send-string', term/ansi-term → `term-send-raw-string', eat →
`eat-self-input' one character event per byte, ghostel →
`ghostel-send-string' for escape sequences and `ghostel-send-key' for
control bytes (E6's semi-char strictness)."
  (let ((buf (generate-new-buffer "*gc-scroll-adapter*")))
    (unwind-protect
        (progn
          ;; vterm → vterm-send-string.
          (with-current-buffer buf (setq major-mode 'vterm-mode))
          (let (sent)
            (cl-letf (((symbol-function 'vterm-send-string)
                       (lambda (s &optional _p) (setq sent s))))
              (should (beads-terminal-tmux--send-raw buf "\e[1;5A"))
              (should (equal sent "\e[1;5A"))))
          ;; term/ansi-term → term-send-raw-string.
          (dolist (mode '(term-mode ansi-term-mode))
            (with-current-buffer buf (setq major-mode mode))
            (let (sent)
              (cl-letf (((symbol-function 'term-send-raw-string)
                         (lambda (s) (setq sent s))))
                (should (beads-terminal-tmux--send-raw buf "\e[6~"))
                (should (equal sent "\e[6~")))))
          ;; eat → eat-self-input, one character event per byte.
          (with-current-buffer buf (setq major-mode 'eat-mode))
          (let (events)
            (cl-letf (((symbol-function 'eat-self-input)
                       (lambda (_n e) (setq events (append events (list e))))))
              (should (beads-terminal-tmux--send-raw buf "q\r"))
              (should (equal events '(?q ?\r)))))
          ;; ghostel: the control/escape split.
          (with-current-buffer buf (setq major-mode 'ghostel-mode))
          (let (strings keys)
            (cl-letf (((symbol-function 'ghostel-send-string)
                       (lambda (s) (setq strings (append strings (list s)))))
                      ((symbol-function 'ghostel-send-key)
                       (lambda (k &optional m)
                         (setq keys (append keys (list (cons k m)))))))
              ;; An escape sequence passes as one string (E6).
              (should (beads-terminal-tmux--send-raw buf "\e[1;5A"))
              (should (equal strings '("\e[1;5A")))
              (should-not keys)
              ;; A control byte needs ghostel-send-key: C-b → ("b" "ctrl"),
              ;; and the printable tail goes as its own string.
              (setq strings nil keys nil)
              (should (beads-terminal-tmux--send-raw buf "\C-b["))
              (should (equal keys '(("b" . "ctrl"))))
              (should (equal strings '("[")))
              ;; Mixed: "g0" as a string, RET as the `return' key.
              (setq strings nil keys nil)
              (should (beads-terminal-tmux--send-raw buf "g0\r"))
              (should (equal strings '("g0")))
              (should (equal keys '(("return")))))))
      (kill-buffer buf))))

(ert-deftest beads-terminal-tmux-test-scroll-unknown-backend ()
  "A backend without a raw-key adapter never errors (REQ-007):
`beads-terminal-tmux--send-raw' returns nil and deactivates the mode with
a message, and the toggle reports without arming anything."
  (let ((buf (generate-new-buffer "*gc-scroll-unknown*")))
    (unwind-protect
        (with-current-buffer buf
          ;; fundamental-mode: no adapter.
          (should-not (beads-terminal-tmux--send-raw buf "\e[1;5A"))
          (should-not beads-terminal-tmux-scroll-mode)
          ;; With the mode armed by hand, a send deactivates it.
          (beads-terminal-tmux-scroll-mode 1)
          (should-not (beads-terminal-tmux--send-raw buf "q"))
          (should-not beads-terminal-tmux-scroll-mode)
          ;; The toggle reports and stays off.
          (beads-terminal-tmux-scroll-toggle)
          (should-not beads-terminal-tmux-scroll-mode))
      (kill-buffer buf))))

(ert-deftest beads-terminal-tmux-test-scroll-toggle ()
  "Toggle semantics (REQ-008): activation sends the copy-mode entry
bytes `C-b [' and arms the map; a re-toggle sends `q' first
(self-healing after an out-of-band copy-mode exit) then re-enters;
`q' and Esc leave copy mode and deactivate the mode (REQ-009)."
  (let ((buf (generate-new-buffer "*gc-scroll-toggle*")))
    (unwind-protect
        (with-current-buffer buf
          (setq major-mode 'vterm-mode)
          (let (sent)
            (cl-letf (((symbol-function 'vterm-send-string)
                       (lambda (s &optional _p) (push s sent))))
              ;; Activation: exactly the entry bytes.
              (beads-terminal-tmux-scroll-toggle)
              (should beads-terminal-tmux-scroll-mode)
              (should (equal (nreverse sent) '("\C-b[")))
              ;; Re-toggle: q first, then re-entry; the mode stays on.
              (setq sent nil)
              (beads-terminal-tmux-scroll-toggle)
              (should beads-terminal-tmux-scroll-mode)
              (should (equal (nreverse sent) '("q" "\C-b[")))
              ;; q: translated, and the mode deactivates.
              (setq sent nil last-command-event ?q)
              (beads-terminal-tmux-scroll-key)
              (should (equal (nreverse sent) '("q")))
              (should-not beads-terminal-tmux-scroll-mode)
              ;; Esc likewise.
              (beads-terminal-tmux-scroll-toggle)
              (should beads-terminal-tmux-scroll-mode)
              (setq sent nil last-command-event 'escape)
              (beads-terminal-tmux-scroll-key)
              (should (equal (nreverse sent) '("\e")))
              (should-not beads-terminal-tmux-scroll-mode)
              ;; A key with no table entry sends nothing, keeps the mode.
              (beads-terminal-tmux-scroll-mode 1)
              (setq sent nil last-command-event ?a)
              (beads-terminal-tmux-scroll-key)
              (should-not sent)
              (should beads-terminal-tmux-scroll-mode))))
      (kill-buffer buf))))

(ert-deftest beads-terminal-tmux-test-scroll-status-marker ()
  "The status mirror's segment carries the `[scroll]' marker exactly
while `beads-terminal-tmux-scroll-mode' is active in the buffer (REQ-010):
same segment, nothing appended by tmux."
  (let ((buf (generate-new-buffer "*gc-scroll-marker*")))
    (unwind-protect
        (with-current-buffer buf
          (setq beads-terminal-tmux--status-string "mayor  1:claude*")
          (should (equal (beads-terminal-tmux--status-segment)
                         " mayor  1:claude*"))
          (beads-terminal-tmux-scroll-mode 1)
          (should (equal (beads-terminal-tmux--status-segment)
                         " mayor  1:claude* [scroll]"))
          (beads-terminal-tmux-scroll-mode -1)
          (should (equal (beads-terminal-tmux--status-segment)
                         " mayor  1:claude*")))
      (kill-buffer buf))))

(ert-deftest beads-terminal-tmux-test-scroll-bindings ()
  "The attach map binds `C-c s' to the toggle (no §10 collision — the
attach map owns only `C-c' keys), and the scroll-mode map owns the D1
keys through the one translator command."
  (should (eq (lookup-key beads-terminal-tmux-attach-map (kbd "C-c s"))
              #'beads-terminal-tmux-scroll-toggle))
  (dolist (key '("C-p" "C-n" "C-v" "M-v" "<next>" "<prior>" "M-<" "M->"
                 "q" "<escape>"))
    (should (eq (lookup-key beads-terminal-tmux-scroll-mode-map (kbd key))
                #'beads-terminal-tmux-scroll-key))))

(ert-deftest beads-terminal-tmux-test-scroll-backend-reports-mouse ()
  "`beads-terminal-tmux--backend-reports-mouse-p' is the pure D2 table:
ghostel and eat report the mouse natively (E4/E5); vterm, term and an
unknown backend do not."
  (should (beads-terminal-tmux--backend-reports-mouse-p 'ghostel))
  (should (beads-terminal-tmux--backend-reports-mouse-p 'eat))
  (should-not (beads-terminal-tmux--backend-reports-mouse-p 'vterm))
  (should-not (beads-terminal-tmux--backend-reports-mouse-p 'term))
  (should-not (beads-terminal-tmux--backend-reports-mouse-p nil)))

(ert-deftest beads-terminal-tmux-test-scroll-wheel-map-selection ()
  "The scroll mode's effective map carries the wheel bindings only on
backends that do not report the mouse (REQ-004): ghostel/eat keep the
base map — no wheel bindings to interfere with the native tmux
passthrough — while vterm gets the wheel extension."
  ;; The base map never carries wheel bindings.
  (should (null (lookup-key beads-terminal-tmux-scroll-mode-map
                            (kbd "<mouse-4>"))))
  (should (null (lookup-key beads-terminal-tmux-scroll-mode-map
                            (kbd "<wheel-down>"))))
  (let ((buf (generate-new-buffer "*gc-scroll-wheel-map*")))
    (unwind-protect
        (with-current-buffer buf
          ;; A reporting backend: enabling the mode installs no wheel map.
          (setq major-mode 'ghostel-mode)
          (beads-terminal-tmux-scroll-mode 1)
          (should beads-terminal-tmux-scroll-mode)
          (should-not (assq 'beads-terminal-tmux-scroll-mode
                            minor-mode-overriding-map-alist))
          (beads-terminal-tmux-scroll-mode -1)
          (should-not (assq 'beads-terminal-tmux-scroll-mode
                            minor-mode-overriding-map-alist))
          ;; A non-reporting backend: the effective map is the wheel map.
          (setq major-mode 'vterm-mode)
          (beads-terminal-tmux-scroll-mode 1)
          (should (eq (cdr (assq 'beads-terminal-tmux-scroll-mode
                                 minor-mode-overriding-map-alist))
                      beads-terminal-tmux-scroll-wheel-map))
          ;; Deactivating removes the buffer-local entry again.
          (beads-terminal-tmux-scroll-mode -1)
          (should-not (assq 'beads-terminal-tmux-scroll-mode
                            minor-mode-overriding-map-alist)))
      (kill-buffer buf))))

(ert-deftest beads-terminal-tmux-test-scroll-wheel-notch ()
  "A wheel notch sends a run of `beads-terminal-tmux--scroll-wheel-notch'
C-Ups (wheel up) or C-Downs (wheel down); the first notch after
(re-)entry re-sends the copy-mode entry bytes before the run (REQ-011),
later notches the run only.  A reporting backend is never driven (REQ-004)."
  (let ((buf (generate-new-buffer "*gc-scroll-wheel*")))
    (unwind-protect
        (with-current-buffer buf
          (setq major-mode 'vterm-mode)
          (let (sent)
            (cl-letf (((symbol-function 'vterm-send-string)
                       (lambda (s &optional _p) (push s sent))))
              ;; Toggle arms the first-notch state (and sends the entry).
              (beads-terminal-tmux-scroll-toggle)
              (setq sent nil)
              (setq last-command-event 'mouse-4)
              (beads-terminal-tmux-scroll-wheel)
              (should (equal (nreverse sent)
                             (list (concat beads-terminal-tmux--copy-mode-entry
                                           "\e[1;5A\e[1;5A\e[1;5A"))))
              ;; Later notches: the run only.
              (setq sent nil)
              (beads-terminal-tmux-scroll-wheel)
              (should (equal (nreverse sent) '("\e[1;5A\e[1;5A\e[1;5A")))
              ;; Wheel down is the C-Down mirror; the notch constant is
              ;; the adjustable knob of requirements Open Question 2.
              (setq beads-terminal-tmux--scroll-wheel-first t sent nil)
              (setq last-command-event 'mouse-5)
              (beads-terminal-tmux-scroll-wheel)
              (should (equal (nreverse sent)
                             (list (concat beads-terminal-tmux--copy-mode-entry
                                           "\e[1;5B\e[1;5B\e[1;5B")))))))
      (kill-buffer buf))))

(ert-deftest beads-terminal-tmux-test-ghostel-available-probe ()
  "`beads-terminal-tmux--ghostel-available-p' probes by loading ghostel,
not merely `featurep' — the load-order dependency the bug was about."
  (cl-letf (((symbol-function 'require) (lambda (&rest _) t))
            ((symbol-function 'beads-terminal-available-p)
             (lambda (_t) t)))
    (should (beads-terminal-tmux--ghostel-available-p)))
  ;; Not installed (or broken): the load attempt answers nil.
  (cl-letf (((symbol-function 'require) (lambda (&rest _) nil)))
    (should-not (beads-terminal-tmux--ghostel-available-p))))

(ert-deftest beads-terminal-tmux-test-backend-class-deterministic ()
  "Unset backend resolves to ghostel when it is available, else to the
beads.el auto walk; an explicit choice is never overridden (ga-eqpxs)."
  (let ((beads-terminal-tmux-backend nil))
    (cl-letf (((symbol-function 'beads-terminal-tmux--ghostel-available-p)
               (lambda () t)))
      (should (eq (beads-terminal-tmux--backend-class) 'beads-terminal-ghostel)))
    (cl-letf (((symbol-function 'beads-terminal-tmux--ghostel-available-p)
               (lambda () nil)))
      (should (eq (beads-terminal-tmux--backend-class) 'beads-terminal-auto))))
  (let ((beads-terminal-tmux-backend 'vterm))
    (should (eq (beads-terminal-tmux--backend-class) 'beads-terminal-vterm)))
  (let ((beads-terminal-tmux-backend 'eat))
    (should (eq (beads-terminal-tmux--backend-class) 'beads-terminal-eat)))
  (let ((beads-terminal-tmux-backend 'term))
    (should (eq (beads-terminal-tmux--backend-class) 'beads-terminal-term))))

(ert-deftest beads-terminal-tmux-test-wheel-mouse-sequence ()
  "The injected wheel event is SGR button 64 (up) / 65 (down) at the
pane's top-left — exactly what a reporting terminal sends tmux."
  (should (equal (beads-terminal-tmux--wheel-mouse-sequence t) "\e[<64;1;1M"))
  (should (equal (beads-terminal-tmux--wheel-mouse-sequence nil) "\e[<65;1;1M")))

(ert-deftest beads-terminal-tmux-test-wheel-armed-by-default ()
  "`beads-terminal-tmux--arm-wheel' enables the wheel mode on vterm/term,
leaves ghostel/eat to their native passthrough, and honours
`beads-terminal-tmux-ensure-mouse' (ga-eqpxs)."
  (let ((buf (generate-new-buffer "*gc-wheel-arm*")))
    (unwind-protect
        (with-current-buffer buf
          (setq major-mode 'vterm-mode)
          (beads-terminal-tmux--arm-wheel buf)
          (should beads-terminal-tmux-wheel-mode)
          (should (eq (cdr (assq 'beads-terminal-tmux-wheel-mode
                                 minor-mode-overriding-map-alist))
                      beads-terminal-tmux-wheel-map))
          ;; Reporting backends: never armed (no double-driving).
          (dolist (mode '(ghostel-mode eat-mode))
            (beads-terminal-tmux-wheel-mode -1)
            (setq major-mode mode)
            (beads-terminal-tmux--arm-wheel buf)
            (should-not beads-terminal-tmux-wheel-mode))
          ;; ensure-mouse nil: the injected event would be meaningless.
          (setq major-mode 'vterm-mode)
          (beads-terminal-tmux-wheel-mode -1)
          (let ((beads-terminal-tmux-ensure-mouse nil))
            (beads-terminal-tmux--arm-wheel buf)
            (should-not beads-terminal-tmux-wheel-mode)))
      (kill-buffer buf))))

(ert-deftest beads-terminal-tmux-test-install-keys-arms-wheel ()
  "`beads-terminal-tmux--install-keys' arms the wheel mode on a
non-reporting backend, so an attach buffer scrolls with no toggle."
  (with-temp-buffer
    (setq major-mode 'vterm-mode)
    (beads-terminal-tmux--install-keys (current-buffer))
    (should (eq (key-binding (kbd "<wheel-up>"))
                #'beads-terminal-tmux-scroll-wheel))
    (should beads-terminal-tmux-wheel-mode)))

(ert-deftest beads-terminal-tmux-test-wheel-default-injects-mouse ()
  "With no scroll mode active, a wheel notch on vterm/term injects tmux's
own SGR mouse event (64 up / 65 down) instead of copy-mode keys, so
tmux's own enter/scroll/leave-at-bottom handling runs (ga-eqpxs).  A
reporting backend is never driven."
  (let ((buf (generate-new-buffer "*gc-wheel-default*")))
    (unwind-protect
        (with-current-buffer buf
          (setq major-mode 'vterm-mode)
          (let (sent)
            (cl-letf (((symbol-function 'vterm-send-string)
                       (lambda (s &optional _p) (push s sent))))
              (setq last-command-event 'wheel-up)
              (beads-terminal-tmux-scroll-wheel)
              (should (equal (nreverse sent) '("\e[<64;1;1M")))
              (setq sent nil last-command-event 'mouse-5)
              (beads-terminal-tmux-scroll-wheel)
              (should (equal (nreverse sent) '("\e[<65;1;1M")))))
          ;; A reporting backend keeps its passthrough: nothing is sent.
          (setq major-mode 'ghostel-mode)
          (let (sent)
            (cl-letf (((symbol-function 'beads-terminal-tmux--send-raw)
                       (lambda (&rest _) (setq sent t))))
              (beads-terminal-tmux-scroll-wheel)
              (should-not sent))))
      (kill-buffer buf))))

(ert-deftest beads-terminal-tmux-test-pane-cwd ()
  "The pane-cwd query returns the trimmed path, passing the socket; nil on miss."
  (should (null (beads-terminal-tmux-pane-cwd "")))
  (should (null (beads-terminal-tmux-pane-cwd nil)))
  ;; Success: trimmed stdout is the path, with `-L SOCKET' in the argv.
  (let (seen-args)
    (cl-letf (((symbol-function 'call-process)
               (lambda (_prog _in buf _disp &rest args)
                 (setq seen-args args)
                 (when (eq buf t) (insert "/live/wd\n"))
                 0)))
      (should (equal (beads-terminal-tmux-pane-cwd "tm" "sock") "/live/wd"))
      (should (equal seen-args
                     '("-L" "sock" "display-message" "-t" "tm"
                       "-p" "#{pane_current_path}")))))
  ;; A missing session (non-zero exit) yields nil.
  (cl-letf (((symbol-function 'call-process) (lambda (&rest _) 1)))
    (should (null (beads-terminal-tmux-pane-cwd "gone"))))
  ;; An empty pane path yields nil.
  (cl-letf (((symbol-function 'call-process)
             (lambda (_prog _in buf _disp &rest _)
               (when (eq buf t) (insert "   \n"))
               0)))
    (should (null (beads-terminal-tmux-pane-cwd "tm")))))

(ert-deftest beads-terminal-tmux-test-live-buffer ()
  "`beads-terminal-tmux--live-buffer' returns the buffer only when it hosts a
live process: nil for a missing buffer, a process-less buffer, or a dead
process; the buffer itself while the process runs."
  ;; Missing buffer.
  (should (null (beads-terminal-tmux--live-buffer "*gc-agent-absent-xyz*")))
  ;; Buffer with no process.
  (let ((buf (generate-new-buffer "*gc-agent-noproc*")))
    (unwind-protect
        (should (null (beads-terminal-tmux--live-buffer (buffer-name buf))))
      (kill-buffer buf)))
  ;; A live process makes the buffer reusable; once it dies, it does not.
  (let* ((buf (generate-new-buffer "*gc-agent-live*"))
         (proc (make-pipe-process :name "gc-test-live" :buffer buf :noquery t)))
    (unwind-protect
        (progn
          (should (eq (beads-terminal-tmux--live-buffer (buffer-name buf)) buf))
          (delete-process proc)
          (should (null (beads-terminal-tmux--live-buffer (buffer-name buf)))))
      (when (process-live-p proc) (delete-process proc))
      (kill-buffer buf))))

(ert-deftest beads-terminal-tmux-test-run-reuses-live-buffer ()
  "`beads-terminal-tmux-run' reuses a buffer that already hosts a live process:
it pops to that buffer and does NOT spawn a second terminal (the bug where
pressing `t' on an already-open agent terminal errored)."
  (let* ((buf (generate-new-buffer "*gc-agent-reuse*"))
         (name (buffer-name buf))
         (proc (make-pipe-process :name "gc-test-reuse" :buffer buf :noquery t))
         spawned popped)
    (unwind-protect
        (cl-letf (((symbol-function 'beads-terminal-spawn)
                   (lambda (&rest _) (setq spawned t) (error "must not re-spawn")))
                  ((symbol-function 'pop-to-buffer)
                   (lambda (b &rest _) (setq popped b) b)))
          (let ((ret (beads-terminal-tmux-run '("env") name)))
            (should (eq ret buf))
            (should (eq popped buf))
            (should-not spawned)))
      (when (process-live-p proc) (delete-process proc))
      (kill-buffer buf))))

(ert-deftest beads-terminal-tmux-test-run-spawns-when-not-live ()
  "With no existing live-process buffer, `beads-terminal-tmux-run' spawns a
fresh terminal via `beads-terminal-spawn' and pops to the new buffer."
  (let ((name "*gc-agent-fresh*")
        spawn-args popped)
    (unwind-protect
        (cl-letf (((symbol-function 'beads-terminal-spawn)
                   (lambda (_term buffer-name argv dir _env)
                     (setq spawn-args (list buffer-name argv dir))
                     (get-buffer-create buffer-name)))
                  ((symbol-function 'pop-to-buffer)
                   (lambda (b &rest _) (setq popped b) b)))
          (let ((ret (beads-terminal-tmux-run '("echo" "hi") name "/tmp")))
            (should (bufferp ret))
            (should (equal (nth 0 spawn-args) name))
            (should (equal (nth 1 spawn-args) '("echo" "hi")))
            (should (string-prefix-p "/tmp" (nth 2 spawn-args)))
            (should (eq popped ret))))
      (when (get-buffer name) (kill-buffer name)))))

(ert-deftest beads-terminal-tmux-test-unshadow-keys ()
  "`beads-terminal-tmux--unshadow-keys' gives each configured minor mode an empty
keymap in the buffer, is idempotent, never clobbers an entry another package
made, and does nothing when the option is nil (gce-43c)."
  (let ((buf (generate-new-buffer "*gc-unshadow*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (let ((beads-terminal-tmux-unshadow-minor-modes
                   '(pixel-scroll-precision-mode)))
              (beads-terminal-tmux--unshadow-keys buf)
              (let ((entry (assq 'pixel-scroll-precision-mode
                                 minor-mode-overriding-map-alist)))
                (should entry)
                (should (keymapp (cdr entry)))
                ;; An empty keymap: the mode binds nothing in this buffer.
                (should (null (lookup-key (cdr entry) (kbd "<prior>")))))
              ;; Idempotent — a second call adds no duplicate.
              (beads-terminal-tmux--unshadow-keys buf)
              (should (= 1 (length (seq-filter
                                    (lambda (c)
                                      (eq (car c) 'pixel-scroll-precision-mode))
                                    minor-mode-overriding-map-alist))))))
          ;; An existing entry wins: beads leaves it exactly as it found it.
          (with-current-buffer buf
            (let ((mine (make-sparse-keymap))
                  (beads-terminal-tmux-unshadow-minor-modes '(some-other-mode)))
              (define-key mine (kbd "<prior>") #'ignore)
              (setq-local minor-mode-overriding-map-alist
                          (cons (cons 'some-other-mode mine)
                                minor-mode-overriding-map-alist))
              (beads-terminal-tmux--unshadow-keys buf)
              (should (eq (cdr (assq 'some-other-mode
                                     minor-mode-overriding-map-alist))
                          mine))))
          ;; nil option: no keymaps touched at all.
          (with-current-buffer buf
            (kill-local-variable 'minor-mode-overriding-map-alist)
            (let ((beads-terminal-tmux-unshadow-minor-modes nil))
              (beads-terminal-tmux--unshadow-keys buf)
              (should (null minor-mode-overriding-map-alist)))))
      (kill-buffer buf))))

(ert-deftest beads-terminal-tmux-test-run-unshadows-both-paths ()
  "`beads-terminal-tmux-run' unshadows the terminal buffer whether it spawned it
or reused a live one (gce-43c)."
  (let ((beads-terminal-tmux-unshadow-minor-modes '(pixel-scroll-precision-mode))
        (name "*gc-agent-unshadow*"))
    (unwind-protect
        (cl-letf (((symbol-function 'beads-terminal-spawn)
                   (lambda (_term buffer-name &rest _)
                     (get-buffer-create buffer-name)))
                  ((symbol-function 'pop-to-buffer) (lambda (b &rest _) b)))
          ;; Spawn path.
          (let ((buf (beads-terminal-tmux-run '("echo" "hi") name)))
            (with-current-buffer buf
              (should (assq 'pixel-scroll-precision-mode
                            minor-mode-overriding-map-alist))
              (kill-local-variable 'minor-mode-overriding-map-alist))
            ;; Reuse path: a live process makes `beads-terminal-tmux-run' reuse
            ;; this very buffer, and it must be unshadowed there too.
            (let ((proc (make-pipe-process :name "gc-test-unshadow"
                                           :buffer buf :noquery t)))
              (unwind-protect
                  (progn
                    (should (eq (beads-terminal-tmux-run '("echo" "hi") name) buf))
                    (with-current-buffer buf
                      (should (assq 'pixel-scroll-precision-mode
                                    minor-mode-overriding-map-alist))))
                (when (process-live-p proc) (delete-process proc))))))
      (when (get-buffer name) (kill-buffer name)))))

(ert-deftest beads-terminal-tmux-test-window-list ()
  "`beads-terminal-tmux--window-list' parses tmux output into :active/:label plists."
  (cl-letf (((symbol-function 'call-process)
             (lambda (_prog _in buf _disp &rest args)
               (should (equal (car args) "list-windows"))
               (when (eq buf t) (insert "0\t0:bash-\n1\t1:claude*\n"))
               0)))
    (let ((ws (beads-terminal-tmux--window-list "sess" nil)))
      (should (= (length ws) 2))
      (should (equal (plist-get (nth 0 ws) :label) "0:bash-"))
      (should (null (plist-get (nth 0 ws) :active)))
      (should (equal (plist-get (nth 1 ws) :label) "1:claude*"))
      (should (eq (plist-get (nth 1 ws) :active) t))))
  ;; A missing session (tmux exit non-zero) yields nil — this is the
  ;; session-existence probe.
  (cl-letf (((symbol-function 'call-process) (lambda (&rest _) 1)))
    (should (null (beads-terminal-tmux--window-list "gone" nil)))))

(ert-deftest beads-terminal-tmux-test-status-string ()
  "`beads-terminal-tmux--status-string' shows the `status-left' name and the
window list with the current window emphasised; nil when the session is gone."
  (cl-letf (((symbol-function 'call-process)
             (lambda (_prog _in buf _disp &rest args)
               (when (eq buf t)
                 (cond
                  ((equal (car args) "list-windows")
                   (insert "0\t0:bash\n1\t1:claude*\n"))
                  ((equal (car args) "display-message")
                   ;; `status-left' value, with trailing space to trim.
                   (insert "gastown.mayor \n"))))
               0)))
    (let ((s (beads-terminal-tmux--status-string "sess" nil)))
      (should (stringp s))
      ;; Friendly name (trimmed) faced as the session identity.
      (should (string-match "gastown.mayor" s))
      (should (eq (get-text-property (string-match "gastown.mayor" s) 'face s)
                  'beads-terminal-tmux-session-face))
      ;; Active window emphasised; inactive window not.
      (should (string-match "1:claude" s))
      (should (eq (get-text-property (string-match "1:claude" s) 'face s)
                  'beads-terminal-tmux-active-window-face))
      (should (string-match "0:bash" s))
      (should (eq (get-text-property (string-match "0:bash" s) 'face s)
                  'default))))
  (cl-letf (((symbol-function 'call-process) (lambda (&rest _) 1)))
    (should (null (beads-terminal-tmux--status-string "gone" nil)))))

(ert-deftest beads-terminal-tmux-test-status-install-teardown ()
  "Install turns the session's tmux status bar off, adds a mode-line segment
and a refresh timer; teardown (via `kill-buffer-hook') cancels the timer and
reverts the override with `set-option -u'.  All tmux ops are session-scoped
and asynchronous (one host script each), never a synchronous process."
  (let ((buf (generate-new-buffer "*gc-agent-install-test*"))
        (beads-terminal-tmux-status-interval 3600)) ; far enough to never fire
    (unwind-protect
        (beads-terminal-tmux-test--with-host nil
          (beads-terminal-tmux--status-install buf "sess" "sock")
          (with-current-buffer buf
            (should (beads-terminal-tmux-test--host-script-p
                     "tmux -L sock set-option -t sess status off"))
            ;; Segment present AND before the trailing fill, or it renders
            ;; off-screen (gce-hjj regression: appending after
            ;; `mode-line-end-spaces' hid it).
            (let ((seg (member beads-terminal-tmux--status-mode-line-segment
                               mode-line-format))
                  (end (member 'mode-line-end-spaces mode-line-format)))
              (should seg)
              (should end)
              (should (> (length seg) (length end))))
            (should (timerp beads-terminal-tmux--status-timer))
            (should (equal beads-terminal-tmux--status-session "sess"))
            (should (equal (substring-no-properties beads-terminal-tmux--status-string)
                           "gastown.mayor  1:claude* 0:bash")))
          ;; Killing the buffer must revert the override, scoped to the session.
          (setq beads-terminal-tmux-test--host-scripts nil)
          (kill-buffer buf)
          (should (beads-terminal-tmux-test--host-script-p
                   "tmux -L sock set-option -t sess -u status")))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest beads-terminal-tmux-test-status-refresh-async ()
  "The status refresh runs one host script, skips a tick while one is in
flight, keeps the last string on a failure and stops once the session is
gone (exit 3)."
  (let ((buf (generate-new-buffer "*gc-agent-refresh-test*"))
        (answers nil) (pending nil))
    (unwind-protect
        (cl-letf (((symbol-function 'beads-terminal-tmux--run-async)
                   (lambda (_dir script callback)
                     (push script answers)
                     (setq pending callback)
                     ;; A live process stands for the query in flight.
                     (start-process "beads-terminal-tmux-test-inflight" nil "sleep" "5")))
                  ((symbol-function 'process-file)
                   (lambda (&rest a) (error "Synchronous process-file %S" a))))
          (with-current-buffer buf
            (setq beads-terminal-tmux--status-session "sess"
                  beads-terminal-tmux--status-timer (run-with-timer 3600 nil #'ignore))
            (beads-terminal-tmux--status-refresh)
            (should (= (length answers) 1))
            ;; In flight: the next tick starts nothing.
            (beads-terminal-tmux--status-tick buf)
            (should (= (length answers) 1))
            (delete-process beads-terminal-tmux--status-process)
            (funcall pending (cons 0 "1\t1:claude*\nbeads-status-left\nmayor\n"))
            (should (equal (substring-no-properties beads-terminal-tmux--status-string)
                           "mayor  1:claude*"))
            ;; A timeout keeps the last good string.
            (beads-terminal-tmux--status-refresh)
            (delete-process beads-terminal-tmux--status-process)
            (funcall pending (cons nil ""))
            (should (equal (substring-no-properties beads-terminal-tmux--status-string)
                           "mayor  1:claude*"))
            ;; The session is gone: cleared, timer stopped.
            (beads-terminal-tmux--status-refresh)
            (delete-process beads-terminal-tmux--status-process)
            (funcall pending (cons 3 ""))
            (should (null beads-terminal-tmux--status-string))
            (should (null beads-terminal-tmux--status-timer))))
      (kill-buffer buf))))

(ert-deftest beads-terminal-tmux-test-run-async-remote-is-local-ssh ()
  "A remote host script runs as a LOCAL ssh pipe from a local directory,
with no file handler: starting it does no TRAMP I/O."
  (beads-terminal-tmux-test--ensure-mock-method)
  (let (spawned)
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest plist) (setq spawned plist) nil))
              ((symbol-function 'beads-remote-ssh-pipe-argv)
               (lambda (dir argv &rest keys) (list 'ssh dir argv keys))))
      (let ((tramp-verbose 0))
        (beads-terminal-tmux--run-async "/ssh:u@h:/city/"
                                        "tmux list-sessions" #'ignore)))
    (let ((command (plist-get spawned :command)))
      (should (eq (car command) 'ssh))
      (should (equal (nth 1 command) "/ssh:u@h:/city/"))
      (should (equal (nth 2 command) '("sh" "-c" "tmux list-sessions"))))
    (should (null (plist-get spawned :file-handler)))
    (should (eq (plist-get spawned :connection-type) 'pipe))))

(ert-deftest beads-terminal-tmux-test-attach-honours-status-toggle ()
  "`beads-terminal-tmux-attach' turns the session's tmux status bar off
in the pre-step only when `beads-terminal-tmux-mode-line-status' is non-nil;
the status-install call happens either way (the teardown must restore the
mouse ensure regardless of the mirror)."
  (let ((buf (generate-new-buffer "*gc-agent-toggle*")) installed)
    (unwind-protect
        (beads-terminal-tmux-test--with-host nil
          (cl-letf (((symbol-function 'beads-terminal-tmux-run)
                     (lambda (&rest _) buf))
                    ((symbol-function 'beads-terminal-tmux--status-install)
                     (lambda (&rest _) (setq installed t))))
            (let ((beads-terminal-tmux-mode-line-status t))
              (setq installed nil)
              (beads-terminal-tmux-attach "sess" "sock" nil)
              (should installed)
              ;; The status bar went off in the pre-step's one round trip.
              (should (beads-terminal-tmux-test--host-script-p "set-option -t sess status off")))
            (let ((beads-terminal-tmux-mode-line-status nil))
              (setq installed nil beads-terminal-tmux-test--host-scripts nil)
              (beads-terminal-tmux-attach "sess" "sock" nil)
              ;; Still installed: the teardown (mouse restore) is needed
              ;; even without the mirror.
              (should installed)
              ;; But the tmux status bar is left alone.
              (should-not (beads-terminal-tmux-test--host-script-p "status off")))))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest beads-terminal-tmux-test-attach-async-missing-session ()
  "A missing session is echoed from the pre-step's answer; nothing is
spawned and nothing signals (the callback runs from a timer)."
  (let (spawned said)
    (beads-terminal-tmux-test--with-host (lambda (_s) (cons 0 "beads-no-session\n"))
      (cl-letf (((symbol-function 'beads-terminal-tmux-run)
                 (lambda (&rest _) (setq spawned t)))
                ((symbol-function 'message)
                 (lambda (fmt &rest args) (setq said (apply #'format fmt args)))))
        (should (null (beads-terminal-tmux-attach "gone" nil nil)))))
    (should-not spawned)
    (should (string-match-p "Can't find tmux session: gone" said))))

(ert-deftest beads-terminal-tmux-test-attach-live-buffer-no-probe ()
  "Re-attaching to a live terminal raises it at once, with no host script."
  (let ((raised nil))
    (beads-terminal-tmux-test--with-host (lambda (s) (error "Probed: %s" s))
      (cl-letf (((symbol-function 'beads-terminal-tmux--live-buffer)
                 (lambda (&rest _) (current-buffer)))
                ((symbol-function 'beads-terminal-tmux-run)
                 (lambda (argv name &rest _) (setq raised (list argv name)) 'buf)))
        (should (eq (beads-terminal-tmux-attach "sess" nil nil) 'buf))))
    (should (equal (car raised) nil))
    (should (null beads-terminal-tmux-test--host-scripts))))

(ert-deftest beads-terminal-tmux-test-attach-term-probe ()
  "A remote attach asks the host for the backend TERM's terminfo in its one
pre-step: missing forces the fallback, found is cached (no probe next time)."
  (let ((default-directory "/ssh:u@h:/city/")
        (beads-terminal-tmux-remote-term "xterm-256color")
        (beads-remote--cache (make-hash-table :test 'equal))
        argvs)
    (cl-letf (((symbol-function 'beads-terminal-tmux--client-term)
               (lambda () "xterm-ghostty"))
              ((symbol-function 'beads-terminal-tmux--status-install) #'ignore)
              ((symbol-function 'beads-terminal-tmux-run)
               (lambda (argv &rest _) (push argv argvs) nil)))
      ;; Missing on the host: the fallback is forced host-side.
      (beads-terminal-tmux-test--with-host
          (lambda (script)
            (should (string-match-p "infocmp xterm-ghostty" script))
            (cons 0 "beads-tmux:/opt/bin/tmux\nbeads-term-missing\nbeads-ok\n"))
        (beads-terminal-tmux-attach "sess" nil nil))
      (should (member (shell-quote-argument "TERM=xterm-256color") (car argvs)))
      ;; Found: nothing forced, and remembered.
      (beads-terminal-tmux-test--with-host nil
        (beads-terminal-tmux-attach "sess" nil nil))
      (should-not (seq-find (lambda (a) (string-prefix-p "TERM" a)) (car argvs)))
      (beads-terminal-tmux-test--with-host
          (lambda (script)
            (should-not (string-match-p "infocmp" script))
            (beads-terminal-tmux-test--answer script))
        (beads-terminal-tmux-attach "sess" nil nil)))))

(ert-deftest beads-terminal-tmux-test-preload-backend-once ()
  "The first view schedules a one-shot idle preload of the terminal
backend; later views and a loaded backend schedule nothing; batch never."
  (let ((beads-terminal-tmux--preload-state nil)
        (scheduled 0) (loaded 0) (noninteractive nil)
        (beads-terminal-tmux-backend 'vterm))
    (cl-letf (((symbol-function 'run-with-idle-timer)
               (lambda (_secs _repeat fn &rest _) (cl-incf scheduled) (funcall fn)))
              ((symbol-function 'beads-terminal-tmux--client-term)
               (lambda () (should-not (file-remote-p default-directory))
                 (cl-incf loaded) "xterm-256color"))
              ((symbol-function 'featurep)
               (lambda (f &rest _) (and (eq f 'vterm) (> loaded 0)))))
      (let ((default-directory "/ssh:u@h:/city/"))
        (beads-terminal-tmux--schedule-preload)
        (beads-terminal-tmux--schedule-preload))
      (should (= scheduled 1))
      (should (= loaded 1))
      (should (eq beads-terminal-tmux--preload-state 'done))))
  ;; Already loaded: nothing scheduled.
  (let ((beads-terminal-tmux--preload-state nil) (noninteractive nil) (scheduled 0)
        (beads-terminal-tmux-backend 'term))
    (cl-letf (((symbol-function 'run-with-idle-timer)
               (lambda (&rest _) (cl-incf scheduled)))
              ((symbol-function 'featurep) (lambda (f &rest _) (eq f 'term))))
      (beads-terminal-tmux--schedule-preload)
      (should (= scheduled 0))))
  ;; Batch: never.
  (let ((beads-terminal-tmux--preload-state nil) (scheduled 0))
    (cl-letf (((symbol-function 'run-with-idle-timer)
               (lambda (&rest _) (cl-incf scheduled))))
      (beads-terminal-tmux--schedule-preload)
      (should (= scheduled 0)))))

(ert-deftest beads-terminal-tmux-test-preload-waits-for-idle ()
  "The preload arms after `beads-terminal-tmux-preload-idle' idle seconds,
re-arms instead of loading while input is pending, and is off with nil."
  (let ((beads-terminal-tmux--preload-state nil) (noninteractive nil)
        (beads-terminal-tmux-backend 'vterm)
        (beads-terminal-tmux-preload-idle 10)
        (armed nil) (loaded 0) (pending t))
    (cl-letf (((symbol-function 'run-with-idle-timer)
               (lambda (secs _repeat fn &rest _) (push (cons secs fn) armed)))
              ((symbol-function 'input-pending-p) (lambda (&rest _) pending))
              ((symbol-function 'beads-terminal-tmux--client-term)
               (lambda () (cl-incf loaded) "xterm-256color"))
              ((symbol-function 'featurep) (lambda (&rest _) (> loaded 0))))
      (beads-terminal-tmux--schedule-preload)
      (should (equal (mapcar #'car armed) '(10)))
      ;; Typing: not loaded, re-armed.
      (funcall (cdr (pop armed)))
      (should (= loaded 0))
      (should (= (length armed) 1))
      ;; Genuine idle: loaded once.
      (setq pending nil)
      (funcall (cdr (pop armed)))
      (should (= loaded 1))
      (should (null armed))))
  (let ((beads-terminal-tmux--preload-state nil) (noninteractive nil)
        (beads-terminal-tmux-preload-idle nil) (armed 0))
    (cl-letf (((symbol-function 'run-with-idle-timer)
               (lambda (&rest _) (cl-incf armed)))
              ((symbol-function 'featurep) (lambda (&rest _) nil)))
      (beads-terminal-tmux--schedule-preload)
      (should (= armed 0)))))

(ert-deftest beads-terminal-tmux-test-install-keys ()
  "The attach map is active in the buffer whatever the local map does.
vterm's copy mode swaps the local map; an emulation map survives that."
  (with-temp-buffer
    (let ((backend (make-sparse-keymap)))
      (define-key backend (kbd "C-c C-t") #'ignore)
      (use-local-map backend)
      (should-not (eq (key-binding (kbd "C-c b")) #'beads-show-at-point))
      (beads-terminal-tmux--install-keys (current-buffer))
      (should (eq (key-binding (kbd "C-c b")) #'beads-show-at-point))
      (should (eq (key-binding (kbd "C-c C-t")) #'ignore))
      ;; A swapped local map (copy mode) keeps beads' key.
      (use-local-map (make-sparse-keymap))
      (should (eq (key-binding (kbd "C-c b")) #'beads-show-at-point))
      ;; Other buffers are untouched.
      (with-temp-buffer
        (should-not (eq (key-binding (kbd "C-c b")) #'beads-show-at-point))))))

(ert-deftest beads-terminal-tmux-test-attach-argv ()
  "Attach argv: direct tmux locally; wrapped in local ssh for a remote city."
  (should (equal (beads-terminal-tmux--attach-argv "sess" "sock")
                 '("env" "-u" "TMUX" "tmux" "-L" "sock"
                   "attach-session" "-t" "sess")))
  (should (equal (beads-terminal-tmux--attach-argv "sess" nil "/ssh:u@h:/city/")
                 '("ssh" "-t" "-l" "u" "h"
                   "env" "-u" "TMUX" "tmux" "attach-session" "-t" "sess")))
  ;; A resolved PROGRAM (a host path from `beads-remote-find-executable')
  ;; replaces the bare tmux in the remote command.
  (should (equal (beads-terminal-tmux--attach-argv
                  "sess" nil "/ssh:u@h:/city/"
                  "/home/user/.guix-home/profile/bin/tmux")
                 '("ssh" "-t" "-l" "u" "h"
                   "env" "-u" "TMUX" "/home/user/.guix-home/profile/bin/tmux"
                   "attach-session" "-t" "sess")))
  ;; A forced TERM lands in the env prefix, before the tmux program —
  ;; host-side, overriding what ssh forwarded (gce-25q).
  (should (equal (beads-terminal-tmux--attach-argv
                  "sess" "sock" nil nil "xterm-256color")
                 '("env" "-u" "TMUX" "TERM=xterm-256color" "tmux"
                   "-L" "sock" "attach-session" "-t" "sess")))
  ;; Remote tokens pass through `shell-quote-argument' (the `=' gets a
  ;; backslash the remote shell strips again).
  (should (equal (beads-terminal-tmux--attach-argv
                  "sess" nil "/ssh:u@h:/city/" "/opt/tmux" "xterm-256color")
                 `("ssh" "-t" "-l" "u" "h"
                   "env" "-u" "TMUX"
                   ,(shell-quote-argument "TERM=xterm-256color") "/opt/tmux"
                   "attach-session" "-t" "sess"))))

(ert-deftest beads-terminal-tmux-test-remote-term ()
  "TERM fallback decision: forced only when the feature is on, the
client's TERM differs from the fallback, and the host lacks (or beads
cannot name) that TERM's terminfo."
  (let ((client "xterm-ghostty") (host-has nil) (probes 0))
    (cl-letf (((symbol-function 'beads-terminal-tmux--client-term)
               (lambda () client))
              ((symbol-function 'beads-remote-terminfo-p)
               (lambda (_term _dir) (cl-incf probes) host-has)))
      (let ((beads-terminal-tmux-remote-term "xterm-256color"))
        ;; Host lacks the client's terminfo: force the fallback.
        (should (equal (beads-terminal-tmux--remote-term "/ssh:u@h:/c/")
                       "xterm-256color"))
        ;; Host has it: keep the native TERM.
        (setq host-has t)
        (should-not (beads-terminal-tmux--remote-term "/ssh:u@h:/c/"))
        ;; Client TERM equals the fallback: nothing to change, no probe.
        (setq client "xterm-256color" probes 0)
        (should-not (beads-terminal-tmux--remote-term "/ssh:u@h:/c/"))
        (should (= probes 0))
        ;; Unknown client TERM: force the fallback, again without probing.
        (setq client nil)
        (should (equal (beads-terminal-tmux--remote-term "/ssh:u@h:/c/")
                       "xterm-256color"))
        (should (= probes 0)))
      ;; Feature off: never force.
      (let ((beads-terminal-tmux-remote-term nil))
        (setq client "xterm-ghostty")
        (should-not (beads-terminal-tmux--remote-term "/ssh:u@h:/c/"))))))

(ert-deftest beads-terminal-tmux-test-tmux-probe-bounded-remote ()
  "`beads-terminal-tmux--tmux' answers nil — uniformly with the other
failure modes — when a remote probe hits the sync-timeout bound
instead of hanging on a wedged channel (ga-yam7)."
  (beads-terminal-tmux-test--with-mock-remote
    (cl-letf (((symbol-function 'beads-remote-find-executable)
               (lambda (&rest _) "/bin/tmux"))
              ((symbol-function 'process-file)
               (lambda (&rest _)
                 ;; A wedged channel: yield forever, like a dead ssh.
                 (sit-for 5)
                 nil)))
      (let ((beads-remote-sync-timeout 0.2))
        (should-not (beads-terminal-tmux--tmux nil "list-sessions"))
        ;; And the plain probe runner degrades identically.
        (should-not
         (beads-terminal-tmux-session-exists-p "sess"))))))

(ert-deftest beads-terminal-tmux-test-beads-integrate ()
  "The attach buffer gets beads eldoc's buffer-local contract: STORE as
`beads-eldoc-directory' when known, and it is a no-op, not an error,
when beads.el lacks the variable."
  (require 'beads-eldoc nil t)
  (let ((saved (and (boundp 'beads-eldoc-directory)
                    (default-value 'beads-eldoc-directory))))
    (unwind-protect
        (progn
          (set-default 'beads-eldoc-directory nil)
          (with-temp-buffer
            (beads-terminal-tmux--beads-integrate (current-buffer) "/p/store/")
            (should (equal (buffer-local-value 'beads-eldoc-directory
                                               (current-buffer))
                           "/p/store/")))
          ;; Unknown store: the local variable is left at its default.
          (with-temp-buffer
            (beads-terminal-tmux--beads-integrate (current-buffer) nil)
            (should-not (local-variable-p 'beads-eldoc-directory
                                          (current-buffer)))))
      (when (boundp 'beads-eldoc-directory)
        (set-default 'beads-eldoc-directory saved)))))

(provide 'beads-terminal-tmux-test)
;;; beads-terminal-tmux-test.el ends here
