;;; beads-cross-repo-ownership-test.el --- Guard for the cross-repo seam list -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; Guard tests for WI-SF-19 / REQ-SF-100 (docs/cross-repo-ownership.md):
;; beads.el owns the generic machinery gascity.el consumes, the dependency
;; is one-way, and the machine-readable seam list in the document matches
;; the symbols beads.el actually provides.  Unit only; no `bd'.

;;; Code:

(require 'ert)
(require 'beads-formula)
(require 'beads-command-formula)
(require 'beads-sling)
(require 'beads-terminal)

(defconst beads-cross-repo-ownership--seams
  '(beads-formula-var-reader
    beads-formula-var-kind
    beads-formula-var-choices
    beads-formula-methodology
    beads-formula-validate-vars
    beads-formula-missing-required-vars
    beads-formula-launch
    beads-formula-launch-context
    beads-formula
    beads-formula-var
    beads-command-formula-show
    beads-command-formula-list
    beads-sling-targets
    beads-sling-dispatch
    beads-terminal-spawn
    beads-terminal-attach)
  "The beads.el seam list owned by `docs/cross-repo-ownership.md'.")

(defun beads-cross-repo-ownership--resolvable-p (symbol)
  "Return non-nil when SYMBOL resolves as a function, variable or class."
  (or (fboundp symbol)
      (boundp symbol)
      (and (find-class symbol nil) t)))

(ert-deftest beads-cross-repo-ownership-seams-resolve ()
  "Every seam the ownership document promises resolves in beads.el."
  :tags '(:unit)
  (dolist (seam beads-cross-repo-ownership--seams)
    (should (beads-cross-repo-ownership--resolvable-p seam))))

(ert-deftest beads-cross-repo-ownership-doc-names-seams ()
  "The ownership document names every seam in its seam-list table."
  :tags '(:unit)
  (let* ((lisp-dir (file-name-directory (locate-library "beads-formula")))
         (doc (expand-file-name "../docs/cross-repo-ownership.md" lisp-dir)))
    (should (file-exists-p doc))
    (let ((text (with-temp-buffer
                  (insert-file-contents doc)
                  (buffer-string))))
      (dolist (seam beads-cross-repo-ownership--seams)
        (should (string-match-p (regexp-quote (format "| `%s` |" seam)) text))))))

(ert-deftest beads-cross-repo-ownership-one-way-dependency ()
  "No beads.el source requires gascity.el (the dependency is one way)."
  :tags '(:unit)
  (let* ((lisp-dir (file-name-directory (locate-library "beads-formula")))
         (offenders nil))
    (dolist (file (directory-files lisp-dir t "\\.el\\'"))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (re-search-forward "(require 'gascity" nil t)
          (push file offenders))))
    (should-not offenders)))

(provide 'beads-cross-repo-ownership-test)
;;; beads-cross-repo-ownership-test.el ends here
