;;; beads-thing-test.el --- Tests for beads-thing -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; ERT tests for the thing-motion primitive (beads-thing.el):
;; forward/backward with wrap, toggle dispatch, tabulated rows,
;; and the key installer.

;;; Code:

(require 'ert)
(require 'beads-thing)
(require 'tabulated-list)
(require 'vui)

(defmacro beads-thing-test--with-buffer (&rest body)
  "Run BODY in a temp buffer holding three things and decoration.
Layout: a summary line (no thing), a section header, blank line, two
rows, a help line."
  (declare (indent 0))
  `(with-temp-buffer
     (insert "Summary line\n"
             (beads-thing-propertize "Header" '(:kind section)) "\n"
             "\n"
             "  " (beads-thing-propertize "row one" "r1") "\n"
             "  " (beads-thing-propertize "row two" "r2") "\n"
             "help text\n")
     (goto-char (point-min))
     ,@body))

(defun beads-thing-test--line ()
  "Return the text of the current line, without properties."
  (buffer-substring-no-properties (line-beginning-position)
                                  (line-end-position)))

(ert-deftest beads-thing-test-forward-visits-things-in-order ()
  "TAB visits exactly the things, in order, skipping decoration."
  :tags '(:unit)
  (beads-thing-test--with-buffer
    (let (seen)
      (dotimes (_ 3)
        (beads-thing-forward)
        (push (buffer-substring-no-properties
               (point) (next-single-property-change (point) 'beads-thing))
              seen))
      (should (equal (nreverse seen) '("Header" "row one" "row two"))))))

(ert-deftest beads-thing-test-forward-wraps-with-message ()
  "Forward at the last thing wraps to the first and echoes Wrapped."
  :tags '(:unit)
  (beads-thing-test--with-buffer
    (search-forward "row two")
    (let (msg)
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
        (beads-thing-forward))
      (should (equal msg "Wrapped"))
      (should (equal (beads-thing-test--line) "Header")))))

(ert-deftest beads-thing-test-backward-wraps-and-starts ()
  "Backward goes to the current thing's start first, then wraps."
  :tags '(:unit)
  (beads-thing-test--with-buffer
    (search-forward "row o")
    (beads-thing-backward)
    (should (equal (buffer-substring-no-properties (point) (+ (point) 7))
                   "row one"))
    (beads-thing-backward)
    (should (equal (beads-thing-test--line) "Header"))
    (let (msg)
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
        (beads-thing-backward))
      (should (equal msg "Wrapped"))
      (should (equal (beads-thing-test--line) "  row two")))))

(ert-deftest beads-thing-test-backward-from-before-first-wraps-to-last ()
  "Backward from the decoration above the first thing lands on the last."
  :tags '(:unit)
  (beads-thing-test--with-buffer
    (beads-thing-backward)
    (should (equal (beads-thing-test--line) "  row two"))))

(ert-deftest beads-thing-test-prefix-argument ()
  "A numeric argument moves several things."
  :tags '(:unit)
  (beads-thing-test--with-buffer
    (beads-thing-forward 2)
    (should (equal (beads-thing-test--line) "  row one"))
    (beads-thing-forward -1)
    (should (equal (beads-thing-test--line) "Header"))))

(ert-deftest beads-thing-test-adjacent-distinct-things ()
  "Two adjacent runs with different values are two things."
  :tags '(:unit)
  (with-temp-buffer
    (insert (beads-thing-propertize "aa" 'a) (beads-thing-propertize "bb" 'b))
    (goto-char (point-min))
    (should (equal (beads-thing-starts) '(1 3)))))

(ert-deftest beads-thing-test-no-things-errors ()
  "Motion in a buffer without things is a user error."
  :tags '(:unit)
  (with-temp-buffer
    (insert "nothing\n")
    (should-error (beads-thing-forward) :type 'user-error)))

(ert-deftest beads-thing-test-toggle-dispatch ()
  "Toggle calls the thing's function, its :toggle, then the hook."
  :tags '(:unit)
  (let (calls)
    (with-temp-buffer
      (insert (beads-thing-propertize "fn" (lambda () (push 'fn calls))) "\n"
              (beads-thing-propertize
               "pl" (list :kind 'row :toggle (lambda () (push 'plist calls))))
              "\n"
              (beads-thing-propertize "obj" "id-1") "\n"
              "deco\n")
      (setq-local beads-thing-toggle-functions
                  (list (lambda (thing) (push (list 'hook thing) calls) t)))
      (goto-char (point-min))
      (beads-thing-toggle)
      (forward-line 1)
      (beads-thing-toggle)
      (forward-line 1)
      (beads-thing-toggle)
      (should (equal (nreverse calls) '(fn plist (hook "id-1"))))
      (forward-line 1)
      (let (msg)
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (setq msg (apply #'format fmt args)))))
          (beads-thing-toggle))
        (should (equal msg "Nothing to toggle here"))))))

(ert-deftest beads-thing-test-toggle-section-pushes-button ()
  "A section thing without :toggle activates the button at point."
  :tags '(:unit)
  (let (pushed)
    (with-temp-buffer
      (insert-text-button "Header" 'action (lambda (_) (setq pushed t))
                          'beads-thing '(:kind section))
      (goto-char (point-min))
      (beads-thing-toggle)
      (should pushed))))

(ert-deftest beads-thing-test-toggle-unhandled-thing ()
  "A thing no handler toggles echoes Nothing to toggle here."
  :tags '(:unit)
  (with-temp-buffer
    (insert (beads-thing-propertize "obj" "id-1"))
    (goto-char (point-min))
    (let (msg)
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
        (beads-thing-toggle))
      (should (equal msg "Nothing to toggle here")))))

(ert-deftest beads-thing-test-tabulated-rows-are-things ()
  "In a tabulated-list buffer every row is a thing of kind row."
  :tags '(:unit)
  (with-temp-buffer
    (tabulated-list-mode)
    (setq tabulated-list-format [("Name" 10 t)])
    (setq tabulated-list-entries '((a ["alpha"]) (b ["beta"]) (c ["gamma"])))
    (tabulated-list-init-header)
    (tabulated-list-print)
    (goto-char (point-min))
    (should (eq (beads-thing-at) 'a))
    (should (eq (beads-thing-kind (beads-thing-at)) 'row))
    (beads-thing-forward)
    (should (eq (tabulated-list-get-id) 'b))
    (beads-thing-forward)
    (should (eq (tabulated-list-get-id) 'c))
    (beads-thing-forward)
    (should (eq (tabulated-list-get-id) 'a))
    (let (got)
      (setq-local beads-thing-toggle-functions
                  (list (lambda (thing) (setq got thing))))
      (beads-thing-toggle)
      (should (eq got 'a)))))

(ert-deftest beads-thing-test-define-keys ()
  "The key installer binds the §5.4 keys and remaps vui navigation."
  :tags '(:unit)
  (let ((map (beads-thing-define-keys (make-sparse-keymap))))
    (dolist (key '("TAB" "<tab>"))
      (should (eq (keymap-lookup map key) #'beads-thing-forward)))
    (dolist (key '("<backtab>" "S-TAB" "S-<tab>"))
      (should (eq (keymap-lookup map key) #'beads-thing-backward)))
    (should (eq (keymap-lookup map "SPC") #'beads-thing-toggle))
    (should (eq (keymap-lookup map "DEL") #'undefined))
    (should (eq (keymap-lookup map "S-SPC") #'undefined))
    (should (eq (lookup-key map [remap vui-forward]) #'beads-thing-forward))
    (should (eq (lookup-key map [remap vui-backward]) #'beads-thing-backward))))

(ert-deftest beads-thing-test-tab-on-vui-button-moves-by-thing ()
  "TAB on a vui button (own keymap binds TAB) still moves by thing."
  :tags '(:unit)
  (let ((map (beads-thing-define-keys (make-sparse-keymap))))
    (set-keymap-parent map vui-mode-map)
    (with-temp-buffer
      (use-local-map map)
      (insert (propertize "btn" 'keymap vui--button-keymap
                          'beads-thing 'x)
              "\n")
      (goto-char (point-min))
      (let ((cmd (key-binding (kbd "TAB") nil nil (point))))
        (should (eq (or (command-remapping cmd (point)) cmd)
                    #'beads-thing-forward))))))

(provide 'beads-thing-test)
;;; beads-thing-test.el ends here
