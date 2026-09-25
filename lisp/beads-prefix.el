;;; beads-prefix.el --- Directory-scoped transient prefixes -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools, project, issues

;;; Commentary:

;; Transient menus remember the directory they were opened for.
;;
;; A transient prefix command returns as soon as its menu is shown;
;; the suffix the user picks runs later, from the original buffer.
;; Anything that only held for the prefix invocation is gone by then:
;; the `default-directory' binding of `project-any-command', and the
;; buffer-local `project-current-directory-override' that
;; `project-switch-project' sets up and kills again once its dispatched
;; command returns.  Without help, a beads menu opened for project B
;; from a buffer in project A runs its suffixes against A.
;;
;; `beads-define-prefix' and `beads-define-group' are drop-in
;; replacements for `transient-define-prefix' and
;; `transient-define-group' that fix this:
;;
;; - The prefix gets the class `beads-prefix', whose
;;   `transient-init-scope' method records the invocation directory
;;   (`beads-prefix-invocation-directory') under `:directory' in the
;;   prefix's scope plist.  Transient carries the scope along when a
;;   child menu returns to its parent, and callers that pass their own
;;   `:scope' plist keep their keys.
;;
;; - Every group gets `:advice* beads-prefix-call-in-directory', which
;;   runs the suffix's body and interactive spec with
;;   `default-directory' bound to that directory.  Transient only looks
;;   at the advice of a suffix's own group, so the macros add it to
;;   every group, nested ones included, and groups that set their own
;;   `:advice'/`:advice*' are left alone.
;;
;; A suffix that opens another beads menu runs with the directory
;; already bound, so the child menu records the same directory.

;;; Code:

(require 'cl-lib)
(require 'transient)

(defvar project-current-directory-override)

(defclass beads-prefix (transient-prefix) ()
  "Transient prefix that remembers the directory it was opened for.
The directory is stored under `:directory' in the prefix's scope
plist; see `beads-prefix-directory'.")

(defun beads-prefix-invocation-directory ()
  "Return the directory a command invoked right now should act on.
This is the directory `project-switch-project' is dispatching to,
when called from its menu, and `default-directory' otherwise."
  (file-name-as-directory
   (expand-file-name
    (or (bound-and-true-p project-current-directory-override)
        default-directory))))

(cl-defmethod transient-init-scope ((obj beads-prefix))
  "Record the invocation directory in the scope plist of OBJ.
Keep a directory that is already there, which is the case when
Transient restores a parent menu, and leave a scope that is not a
plist alone."
  (let ((scope (oref obj scope)))
    (when (and (plistp scope) (not (plist-get scope :directory)))
      (oset obj scope (append (list :directory
                                    (beads-prefix-invocation-directory))
                              scope)))))

(defun beads-prefix-directory ()
  "Return the directory of the active beads menu, or nil.
That is the menu the current suffix was invoked from, or the one
being set up."
  (let ((scope (transient-scope nil 'beads-prefix)))
    (and (plistp scope) (plist-get scope :directory))))

(defun beads-prefix-call-in-directory (fn &rest args)
  "Apply FN to ARGS in the directory of the active beads menu.
Used as the `:advice*' of every group defined by
`beads-define-prefix' and `beads-define-group'."
  (let ((default-directory (or (beads-prefix-directory) default-directory)))
    (apply fn args)))

(defun beads-prefix--advise-group (group)
  "Return GROUP, a group vector, with `:advice*' added.
Subgroups are advised as well."
  (let ((elts (append group nil))
        head keywords)
    (when (integerp (car elts))
      (push (pop elts) head))
    (when (stringp (car elts))
      (push (pop elts) head))
    (while (keywordp (car elts))
      (push (pop elts) keywords)
      (push (pop elts) keywords))
    (setq keywords (nreverse keywords))
    (unless (or (plist-member keywords :advice)
                (plist-member keywords :advice*))
      (setq keywords (append keywords
                             (list :advice* 'beads-prefix-call-in-directory))))
    (vconcat (nreverse head)
             keywords
             (mapcar (lambda (elt)
                       (if (vectorp elt) (beads-prefix--advise-group elt) elt))
                     elts))))

(defun beads-prefix--advise-groups (args)
  "Advise the group vectors in ARGS, the tail of a transient definition.
Values of keyword arguments are left untouched."
  (let (result)
    (while args
      (let ((arg (pop args)))
        (cond ((keywordp arg)
               (push arg result)
               (when args (push (pop args) result)))
              ((vectorp arg)
               (push (beads-prefix--advise-group arg) result))
              (t (push arg result)))))
    (nreverse result)))

(defmacro beads-define-prefix (name arglist &rest args)
  "Define NAME as a transient prefix that remembers its directory.
Like `transient-define-prefix', which see for ARGLIST and ARGS, but
the prefix defaults to the class `beads-prefix' and every group runs
its suffixes in the directory the menu was opened for.  A `:class'
given in ARGS should derive from `beads-prefix'."
  (declare (debug transient-define-prefix)
           (indent defun)
           (doc-string 3))
  (let ((docstring (and (stringp (car args)) (list (pop args))))
        keywords)
    (while (keywordp (car args))
      (push (pop args) keywords)
      (push (pop args) keywords))
    (setq keywords (nreverse keywords))
    (unless (plist-member keywords :class)
      (setq keywords (append (list :class 'beads-prefix) keywords)))
    `(transient-define-prefix ,name ,arglist
       ,@docstring
       ,@keywords
       ,@(beads-prefix--advise-groups args))))

(defmacro beads-define-group (name &rest groups)
  "Define NAME as transient GROUPS whose suffixes remember the directory.
Like `transient-define-group', for groups included in a menu defined
by `beads-define-prefix'."
  (declare (debug transient-define-group)
           (indent defun))
  `(transient-define-group ,name
     ,@(beads-prefix--advise-groups groups)))

(provide 'beads-prefix)
;;; beads-prefix.el ends here
