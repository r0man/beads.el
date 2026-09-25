;;; beads-prefix-test.el --- Tests for beads-prefix.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; ERT tests for beads-prefix.el: transient menus that remember the
;; directory they were opened for.  The menu tests drive real transient
;; menus with `execute-kbd-macro', opening them the way
;; `project-switch-project' does: with `project-current-directory-override'
;; bound only while the prefix command runs.

;;; Code:

(require 'ert)
(require 'project)
(require 'beads-prefix)

;;; Fixtures

(defvar beads-prefix-test--seen nil
  "Directories recorded by the test suffixes, most recent first.")

(defun beads-prefix-test--record ()
  "Record `default-directory'."
  (interactive)
  (push default-directory beads-prefix-test--seen))

(defun beads-prefix-test--record-arg (dir)
  "Record DIR, read by the interactive spec."
  (interactive (list default-directory))
  (push dir beads-prefix-test--seen))

(beads-define-group beads-prefix-test--group
  ["Included"
   ("i" "Record" beads-prefix-test--record)])

(beads-define-prefix beads-prefix-test--child ()
  "Child menu."
  ["Child"
   ("c" "Record" beads-prefix-test--record)])

(beads-define-prefix beads-prefix-test--menu ()
  "Parent menu."
  [["Column"
    ("a" "Record" beads-prefix-test--record)
    ("s" "Record and stay" beads-prefix-test--record :transient t)
    ("r" "Record argument" beads-prefix-test--record-arg)]
   ["Nested"
    ("n" "Child menu" beads-prefix-test--child)]]
  beads-prefix-test--group)

(defmacro beads-prefix-test--with-dirs (&rest body)
  "Run BODY with directories `home' and `project' bound."
  (declare (indent 0) (debug t))
  `(let ((home (file-name-as-directory (make-temp-file "beads-home" t)))
         (project (file-name-as-directory (make-temp-file "beads-proj" t))))
     (unwind-protect (progn ,@body)
       (delete-directory home t)
       (delete-directory project t))))

(defun beads-prefix-test--switch-and-type (home project keys)
  "Open the test menu for PROJECT from a buffer in HOME, then type KEYS.
Return the recorded directories, oldest first."
  (setq beads-prefix-test--seen nil)
  (let ((map (make-sparse-keymap)))
    (keymap-set map "<f12>" #'beads-prefix-test--menu)
    (with-current-buffer (window-buffer)
      (let ((default-directory home)
            (overriding-local-map map))
        (setq-local default-directory home)
        (let ((project-current-directory-override project))
          (execute-kbd-macro (kbd "<f12>")))
        (execute-kbd-macro (kbd keys)))))
  (reverse beads-prefix-test--seen))

;;; Macro expansion

(ert-deftest beads-prefix-test-define-prefix-sets-class ()
  "The prefix gets `beads-prefix' as class unless it names one."
  :tags '(:unit)
  (should (equal (macroexpand-1 '(beads-define-prefix m () "Doc."))
                 '(transient-define-prefix m () "Doc." :class beads-prefix)))
  (should (equal (macroexpand-1
                  '(beads-define-prefix m () "Doc." :class my-class))
                 '(transient-define-prefix m () "Doc." :class my-class))))

(ert-deftest beads-prefix-test-define-prefix-advises-groups ()
  "Every group vector, nested or not, gets the directory advice."
  :tags '(:unit)
  (should (equal (macroexpand-1
                  '(beads-define-prefix m ()
                     :value '("--x")
                     [["A" ("a" "A" a)]
                      [1 "B" :if p ("b" "B" b)]]
                     included))
                 '(transient-define-prefix m ()
                    :class beads-prefix
                    :value '("--x")
                    [:advice* beads-prefix-call-in-directory
                     ["A" :advice* beads-prefix-call-in-directory
                      ("a" "A" a)]
                     [1 "B" :if p :advice* beads-prefix-call-in-directory
                        ("b" "B" b)]]
                    included))))

(ert-deftest beads-prefix-test-define-prefix-keeps-own-advice ()
  "A group with its own advice is left alone."
  :tags '(:unit)
  (should (equal (macroexpand-1
                  '(beads-define-prefix m () ["A" :advice f ("a" "A" a)]))
                 '(transient-define-prefix m ()
                    :class beads-prefix
                    ["A" :advice f ("a" "A" a)]))))

(ert-deftest beads-prefix-test-define-group-advises-groups ()
  "`beads-define-group' advises its groups."
  :tags '(:unit)
  (should (equal (macroexpand-1 '(beads-define-group g ["A" ("a" "A" a)]))
                 '(transient-define-group g
                    ["A" :advice* beads-prefix-call-in-directory
                     ("a" "A" a)]))))

;;; Scope

(ert-deftest beads-prefix-test-init-scope-records-directory ()
  "The scope records the directory, preferring project.el's override."
  :tags '(:unit)
  (let ((default-directory "/tmp/home/"))
    (let ((obj (beads-prefix)))
      (transient-init-scope obj)
      (should (equal (oref obj scope) '(:directory "/tmp/home/"))))
    (let ((obj (beads-prefix))
          (project-current-directory-override "/tmp/project"))
      (transient-init-scope obj)
      (should (equal (oref obj scope) '(:directory "/tmp/project/"))))))

(ert-deftest beads-prefix-test-init-scope-keeps-existing-scope ()
  "Scope keys set by the caller survive; a recorded directory is kept."
  :tags '(:unit)
  (let ((default-directory "/tmp/home/"))
    (let ((obj (beads-prefix :scope '(:city "/c/"))))
      (transient-init-scope obj)
      (should (equal (oref obj scope) '(:directory "/tmp/home/" :city "/c/"))))
    (let ((obj (beads-prefix :scope '(:directory "/tmp/project/"))))
      (transient-init-scope obj)
      (should (equal (oref obj scope) '(:directory "/tmp/project/"))))
    (let ((obj (beads-prefix :scope "not a plist")))
      (transient-init-scope obj)
      (should (equal (oref obj scope) "not a plist")))))

;;; Menus

(ert-deftest beads-prefix-test-suffix-runs-in-menu-directory ()
  "A suffix runs in the directory the menu was opened for."
  :tags '(:unit :transient)
  (beads-prefix-test--with-dirs
    (should (equal (beads-prefix-test--switch-and-type home project "a")
                   (list project)))))

(ert-deftest beads-prefix-test-interactive-spec-runs-in-menu-directory ()
  "A suffix's interactive spec runs in the menu's directory too."
  :tags '(:unit :transient)
  (beads-prefix-test--with-dirs
    (should (equal (beads-prefix-test--switch-and-type home project "r")
                   (list project)))))

(ert-deftest beads-prefix-test-included-group-runs-in-menu-directory ()
  "Suffixes of a group defined with `beads-define-group' are advised."
  :tags '(:unit :transient)
  (beads-prefix-test--with-dirs
    (should (equal (beads-prefix-test--switch-and-type home project "i")
                   (list project)))))

(ert-deftest beads-prefix-test-staying-suffix-keeps-directory ()
  "A suffix that keeps the menu open runs in its directory every time."
  :tags '(:unit :transient)
  (beads-prefix-test--with-dirs
    (should (equal (beads-prefix-test--switch-and-type home project "s s C-g")
                   (list project project)))))

(ert-deftest beads-prefix-test-child-menu-inherits-directory ()
  "A menu opened from a suffix records the parent's directory."
  :tags '(:unit :transient)
  (beads-prefix-test--with-dirs
    (should (equal (beads-prefix-test--switch-and-type home project "n c")
                   (list project)))))

(ert-deftest beads-prefix-test-without-override-uses-current-directory ()
  "Opened without project.el, the menu acts on `default-directory'."
  :tags '(:unit :transient)
  (beads-prefix-test--with-dirs
    (should (equal (beads-prefix-test--switch-and-type home home "a")
                   (list home)))))

(provide 'beads-prefix-test)
;;; beads-prefix-test.el ends here
