;;; beads-transient-doc-test.el --- Tests for generated dispatch docs -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Commentary:

;; ERT tests for `beads-transient-doc' (the generic transient-layout
;; introspection and rendering) and `beads-dispatch-doc' (the beads
;; wiring).  The central test asserts that the checked-in dispatch
;; reference, the README dispatch table, and the Texinfo manual dispatch
;; and maintenance tables all agree with the transient menu code, so a
;; documented key can never silently drift (plan F2, large item 12).

;;; Code:

(require 'ert)
(require 'beads-dispatch-doc)
(require 'beads-transient-doc)

;;; A synthetic menu for the generic engine

(transient-define-prefix beads-transient-doc-test--menu ()
  "Synthetic transient used to exercise the generic doc engine."
  ["Alpha"
   ("a" "First" ignore)
   ("b" "Second" ignore)]
  ["Beta"
   ("c" "Third" ignore)])

;;; Generic engine

(ert-deftest beads-transient-doc-test-suffixes ()
  "`beads-transient-doc-suffixes' flattens groups to keyed entries."
  :tags '(:unit)
  (let ((entries
         (beads-transient-doc-suffixes 'beads-transient-doc-test--menu)))
    (should (equal (mapcar (lambda (e) (plist-get e :key)) entries)
                   '("a" "b" "c")))
    (should (equal (mapcar (lambda (e) (plist-get e :description)) entries)
                   '("First" "Second" "Third")))
    (should (equal (mapcar (lambda (e) (plist-get e :group)) entries)
                   '("Alpha" "Alpha" "Beta")))))

(ert-deftest beads-transient-doc-test-menu-markdown ()
  "The Markdown renderer emits a table row per keyed suffix."
  :tags '(:unit)
  (let ((md (beads-transient-doc-menu-markdown
             'beads-transient-doc-test--menu "Synthetic")))
    (should (string-match-p "#### Synthetic" md))
    (should (string-match-p "| `a` | `ignore` | First |" md))
    (should (string-match-p "| `c` | `ignore` | Third |" md))))

(ert-deftest beads-transient-doc-test-menu-org ()
  "The Org renderer emits an Org table with group and level columns."
  :tags '(:unit)
  (let ((org (beads-transient-doc-menu-org
              'beads-transient-doc-test--menu "Synthetic")))
    (should (string-match-p "^\\*\\* Synthetic" org))
    (should (string-match-p
             "| Alpha | 1 | =a= | =ignore= | First |" org))))

(ert-deftest beads-transient-doc-test-menu-texinfo ()
  "The Texinfo renderer emits a @multitable with group and key columns."
  :tags '(:unit)
  (let ((texi (beads-transient-doc-menu-texinfo
               'beads-transient-doc-test--menu "Synthetic")))
    (should (string-match-p "^@multitable" texi))
    (should (string-match-p
             "@item Alpha @tab @kbd{a} @tab @code{ignore} @tab First"
             texi))
    (should (string-match-p
             "@item Beta @tab @kbd{c} @tab @code{ignore} @tab Third"
             texi))
    (should (string-match-p "@end multitable" texi))))

(ert-deftest beads-transient-doc-test-missing-prefix-errors ()
  "Introspecting a non-prefix signals a clear error."
  :tags '(:unit)
  (should-error
   (beads-transient-doc-suffixes 'beads-transient-doc-no-such-prefix)))

;;; The beads dispatch contract

(ert-deftest beads-transient-doc-test-dispatch-check ()
  "Every generated dispatch doc matches the transient menus."
  :tags '(:unit)
  (should (beads-dispatch-doc-check)))

(ert-deftest beads-transient-doc-test-check-fails-on-unregenerated-menu ()
  "A menu edited without regeneration fails the drift check.
This encodes the WI-10 verification bar that a hand-edited menu which
was not regenerated makes the checked-in docs fail, not only a
hand-edited checked-in artifact."
  :tags '(:unit)
  (let ((beads-dispatch-doc-menus
         (cons '(beads-dispatch . "Hand-edited title that was not regenerated")
               beads-dispatch-doc-menus)))
    (should-error (beads-dispatch-doc-check))))

(defun beads-transient-doc-test--table-keys (region)
  "Return the first-column keys of the Markdown table in REGION."
  (with-temp-buffer
    (insert region)
    (goto-char (point-min))
    (let (result)
      (while (re-search-forward "^| `\\([^`]+\\)` |" nil t)
        (push (match-string-no-properties 1) result))
      (nreverse result))))

(defun beads-transient-doc-test--texinfo-keys (region)
  "Return the @kbd keys of the Texinfo multitable in REGION."
  (with-temp-buffer
    (insert region)
    (goto-char (point-min))
    (let (result)
      (while (re-search-forward "@kbd{\\([^}]+\\)}" nil t)
        (push (match-string-no-properties 1) result))
      (nreverse result))))

(ert-deftest beads-transient-doc-test-readme-lists-dispatch-keys ()
  "The README dispatch table documents every keyed dispatch suffix.
This parses the generated region independently of the generator, so a
key added to the menu without regenerating the docs fails twice."
  :tags '(:unit)
  (let* ((root (beads-transient-doc-project-root))
         (region (beads-transient-doc-region-read
                  (expand-file-name beads-dispatch-doc-readme-file root)
                  beads-dispatch-doc-readme-begin
                  beads-dispatch-doc-readme-end))
         (doc-keys (and region (beads-transient-doc-test--table-keys region)))
         (menu-keys (mapcar (lambda (e) (plist-get e :key))
                            (beads-transient-doc-suffixes 'beads-dispatch))))
    (should region)
    (should (equal doc-keys menu-keys))))

(ert-deftest beads-transient-doc-test-manual-lists-dispatch-keys ()
  "The Texinfo manual dispatch table documents every dispatch suffix."
  :tags '(:unit)
  (let* ((root (beads-transient-doc-project-root))
         (region (beads-transient-doc-region-read
                  (expand-file-name beads-dispatch-doc-manual-file root)
                  beads-dispatch-doc-dispatch-begin
                  beads-dispatch-doc-dispatch-end))
         (doc-keys (and region
                         (beads-transient-doc-test--texinfo-keys region)))
         (menu-keys (mapcar (lambda (e) (plist-get e :key))
                            (beads-transient-doc-suffixes 'beads-dispatch))))
    (should region)
    (should (equal doc-keys menu-keys))))

(ert-deftest beads-transient-doc-test-manual-lists-maintenance-keys ()
  "The Texinfo manual maintenance table documents every suffix."
  :tags '(:unit)
  (let* ((root (beads-transient-doc-project-root))
         (region (beads-transient-doc-region-read
                  (expand-file-name beads-dispatch-doc-manual-file root)
                  beads-dispatch-doc-manual-begin
                  beads-dispatch-doc-manual-end))
         (doc-keys (and region
                         (beads-transient-doc-test--texinfo-keys region)))
         (menu-keys (mapcar (lambda (e) (plist-get e :key))
                            (beads-transient-doc-suffixes 'beads-maintenance))))
    (should region)
    (should (equal doc-keys menu-keys))))

(ert-deftest beads-transient-doc-test-detects-region-drift ()
  "A stale generated region signals an error instead of passing."
  :tags '(:unit)
  (let ((root (beads-transient-doc-project-root)))
    (should-error
     (beads-transient-doc-region-check
      (expand-file-name beads-dispatch-doc-readme-file root)
      beads-dispatch-doc-readme-begin
      beads-dispatch-doc-readme-end
      "stale content that cannot match"))))

(ert-deftest beads-transient-doc-test-detects-file-drift ()
  "A stale generated snapshot signals an error instead of passing."
  :tags '(:unit)
  (let ((root (beads-transient-doc-project-root)))
    (should-error
     (beads-transient-doc-file-check
      (expand-file-name beads-dispatch-doc-reference-file root)
      "stale snapshot that cannot match"))))

(provide 'beads-transient-doc-test)
;;; beads-transient-doc-test.el ends here
