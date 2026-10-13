;;; beads-dispatch-doc.el --- Beads dispatch reference generation -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Beads Contributors
;; Keywords: tools, docs, transient

;;; Commentary:

;; Beads-specific wiring for `beads-transient-doc'.  It names the
;; hand-written prefixes that form the documented dispatch contract,
;; renders the checked-in reference snapshot, and verifies the generated
;; blocks embedded in README.md and the Org manual.
;;
;; The heavy lifting (walking the transient layout, rendering Markdown and
;; Org) lives in `beads-transient-doc', which is package-agnostic.  A
;; downstream package can reuse that module with its own prefix list.
;;
;; `scripts/dispatch-docs' is the command-line front end:
;;
;;   scripts/dispatch-docs --check   # fail on drift (what CI runs)
;;   scripts/dispatch-docs --write   # regenerate the snapshot and regions

;;; Code:

(require 'cl-lib)
(require 'beads-menu)
(require 'beads-transient-doc)

(defconst beads-dispatch-doc-menus
  '((beads-dispatch . "Dispatch menu (`?`)")
    (beads-maintenance . "Maintenance menu (`!`)")
    (beads-integrations-menu . "Integrations menu")
    (beads-setup-menu . "Setup menu"))
  "Hand-written transient prefixes that make up the dispatch contract.
Each cell is (PREFIX . TITLE).  `beads-transient-doc' turns the list
into the generated dispatch reference.")

(defconst beads-dispatch-doc-reference-file "docs/dispatch-reference.md"
  "Project-relative path of the generated dispatch reference snapshot.")

(defconst beads-dispatch-doc-readme-file "README.md"
  "Project-relative path of the README carrying the dispatch table.")

(defconst beads-dispatch-doc-readme-begin
  "<!-- BEGIN GENERATED: dispatch menu -->"
  "Marker that opens the generated README dispatch block.")

(defconst beads-dispatch-doc-readme-end
  "<!-- END GENERATED: dispatch menu -->"
  "Marker that closes the generated README dispatch block.")

(defconst beads-dispatch-doc-manual-file "docs/manual/orchestration.org"
  "Project-relative path of the manual page carrying the maintenance table.")

(defconst beads-dispatch-doc-manual-begin
  "# BEGIN GENERATED: maintenance menu"
  "Marker that opens the generated manual maintenance block.")

(defconst beads-dispatch-doc-manual-end
  "# END GENERATED: maintenance menu"
  "Marker that closes the generated manual maintenance block.")

(defun beads-dispatch-doc--abs (root file)
  "Return FILE resolved against project ROOT."
  (expand-file-name file root))

(defun beads-dispatch-doc-reference ()
  "Return the generated dispatch reference document as a string."
  (beads-transient-doc-markdown beads-dispatch-doc-menus))

(defun beads-dispatch-doc-readme-region ()
  "Return the exact text for the generated README dispatch region.
The region wraps the dispatch menu block in the blank lines that
surround the HTML comment markers."
  (concat "\n"
          (string-trim-right
           (beads-transient-doc-menu-markdown
            'beads-dispatch "Dispatch menu (`?`)"))
          "\n"))

(defun beads-dispatch-doc-manual-region ()
  "Return the exact text for the generated manual maintenance region.
The region wraps the maintenance menu table in the blank lines that
surround the Org comment markers."
  (concat "\n"
          (string-trim-right
           (beads-transient-doc-menu-org
            'beads-maintenance "Maintenance reference"))
          "\n"))

(defun beads-dispatch-doc-check (&optional root)
  "Signal an error when any generated dispatch doc is out of date.
ROOT defaults to `beads-transient-doc-project-root'.  Checks the
reference snapshot and the marked README and manual regions.  Returns t
when everything is current."
  (let ((root (or root (beads-transient-doc-project-root))))
    (beads-transient-doc-file-check
     (beads-dispatch-doc--abs root beads-dispatch-doc-reference-file)
     (beads-dispatch-doc-reference))
    (beads-transient-doc-region-check
     (beads-dispatch-doc--abs root beads-dispatch-doc-readme-file)
     beads-dispatch-doc-readme-begin
     beads-dispatch-doc-readme-end
     (beads-dispatch-doc-readme-region))
    (beads-transient-doc-region-check
     (beads-dispatch-doc--abs root beads-dispatch-doc-manual-file)
     beads-dispatch-doc-manual-begin
     beads-dispatch-doc-manual-end
     (beads-dispatch-doc-manual-region))
    t))

(defun beads-dispatch-doc-write (&optional root)
  "Regenerate every generated dispatch doc under ROOT.
ROOT defaults to `beads-transient-doc-project-root'.  Writes the
reference snapshot and updates the marked README and manual regions.
Returns t."
  (let ((root (or root (beads-transient-doc-project-root))))
    (beads-transient-doc-file-write
     (beads-dispatch-doc--abs root beads-dispatch-doc-reference-file)
     (beads-dispatch-doc-reference))
    (beads-transient-doc-region-write
     (beads-dispatch-doc--abs root beads-dispatch-doc-readme-file)
     beads-dispatch-doc-readme-begin
     beads-dispatch-doc-readme-end
     (beads-dispatch-doc-readme-region))
    (beads-transient-doc-region-write
     (beads-dispatch-doc--abs root beads-dispatch-doc-manual-file)
     beads-dispatch-doc-manual-begin
     beads-dispatch-doc-manual-end
     (beads-dispatch-doc-manual-region))
    t))

(provide 'beads-dispatch-doc)

;;; beads-dispatch-doc.el ends here
