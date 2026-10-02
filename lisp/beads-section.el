;;; beads-section.el --- vui-based rendering for beads issue sections -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; vui.el-based UI primitives for beads.el.  Provides EIEIO data
;; containers (`beads-issues-section', `beads-issue-section',
;; `beads-blocked-section', `beads-ready-section'), a section
;; text-property contract for context detection at point, and the
;; `beads-section-mode' major mode.  Issue context is stored as a
;; `beads-section' text property on each rendered line and read back
;; via `beads-section-issue-id-at-point'.
;;
;; Mode-inheritance idiom (the blessed extension pattern):
;;
;;   `beads-section-mode' derives from `vui-mode' and parents its keymap
;;   to `vui-mode-map'.  A consumer library should derive its own
;;   section major mode from `beads-section-mode' and parent its keymap
;;   to `beads-section-mode-map', forming the chain
;;
;;       vui-mode -> beads-section-mode -> consumer-mode
;;
;;   so the consumer inherits the vui machinery and beads.el's section
;;   navigation bindings and only adds its own.  For example:
;;
;;       (defvar-keymap my-section-mode-map
;;         :parent beads-section-mode-map
;;         "RET" #'my-visit-thing-at-point)
;;
;;       (define-derived-mode my-section-mode beads-section-mode "My"
;;         "Major mode for my-tool section buffers."
;;         :interactive nil)
;;
;; See the manual node "Extending beads.el" for the full extension API.

;;; Code:

(require 'eieio)
(require 'vui)
(require 'beads-command)
(require 'beads-command-blocked)
(require 'beads-command-list)
(require 'beads-command-ready)
(require 'beads-types)
(require 'beads-thing)
(require 'beads-buffer)
(require 'beads-faces)

;;; Forward Declarations

(declare-function beads-show "beads-command-show")

;;; Glyphs

(defconst beads-section-glyph-expanded "▾"
  "Glyph rendered on expanded section headers.")

(defconst beads-section-glyph-collapsed "▸"
  "Glyph rendered on collapsed section headers.")

;;; Faces

(defface beads-issue-line
  '((t :inherit default))
  "Face for clickable issue lines in section and dashboard buffers."
  :group 'beads)

;;; Context Detection

(defun beads-section--propertize (str section &optional extra-props)
  "Return STR with SECTION stored as the `beads-section' text property.
When EXTRA-PROPS is non-nil, it is a plist of additional text properties
merged into the result (callers stamp surface-specific keys like
`beads-dashboard-section-key' without polluting this module)."
  ;; Every section row is also a thing for TAB/S-TAB motion.
  (apply #'propertize str 'beads-section section 'beads-thing section
         extra-props))

(defun beads-section-issue-id-at-point ()
  "Return the issue ID at point via text property, or nil.
Reads the `beads-section' text property and extracts the issue ID
from a `beads-issue-section' data object."
  (when-let* ((sec (get-text-property (point) 'beads-section))
              (_ (object-of-class-p sec 'beads-issue-section))
              (issue (oref sec issue)))
    (oref issue id)))

;;; vui Components

(defun beads-section--plain-button (label on-click)
  "Return a non-decorated, plain-faced `vui-button' carrying LABEL and ON-CLICK.
Used for buttons that should read as plain text rather than as
hyperlinks (no underline, no tooltip)."
  (vui-button label
    :no-decoration t
    :face 'beads-issue-line
    :help-echo nil
    :on-click on-click))

(defun beads-section--issue-line-vnode (issue &optional extra-props agents)
  "Return a vui button vnode for a single ISSUE.
The button displays the issue id, priority, type, status, and title.
Its label carries a `beads-section' text property for context
detection at point via `beads-section-issue-id-at-point'.

EXTRA-PROPS, when non-nil, is a plist of additional text properties
merged into the label (e.g. `beads-dashboard-section-key' stamped by
the dashboard so commands like `beads-dashboard-load-more' can resolve
the enclosing section in O(1)).

AGENTS, when a non-empty string, is appended to the line as a trailing
badge group (two-space separator).  The string is expected to already
carry its own faces and `help-echo' — typically the return value of
`beads-agent-display-format-issue-agents'.  When nil or empty, no
separator is added so rows with no agent activity show no padding
artifacts."
  (let* ((id       (or (oref issue id) ""))
         (title    (or (oref issue title) ""))
         (priority (oref issue priority))
         (type     (or (oref issue issue-type) ""))
         (status   (or (oref issue status) ""))
         (prio-str (if priority (format "P%d" priority) "--"))
         (core     (format "  %-14s %-4s %-10s %-14s %s"
                           id prio-str type status title))
         (label    (beads-section--propertize core
                                              (beads-issue-section :issue issue)
                                              extra-props))
         (full-label (if (and (stringp agents) (not (string-empty-p agents)))
                         (concat label "  " agents)
                       label)))
    (beads-section--plain-button
     full-label
     (let ((issue-id id))
       (lambda () (beads-show issue-id))))))

(vui-defcomponent beads-section--issue-group (title issues)
  "Collapsible vui component showing a group of ISSUES under TITLE.
Clicking the header toggles expanded/collapsed state.
When ISSUES is nil this component renders nothing."
  :state ((expanded t))
  :render
  (when issues
    (vui-vstack
     (vui-button
      (beads-thing-propertize
       (format "%s %s (%d)"
               (if expanded
                   beads-section-glyph-expanded
                 beads-section-glyph-collapsed)
               title
               (length issues))
       '(:kind section))
      :no-decoration t
      :face 'bold
      :help-echo (if expanded
                     (format "Collapse %s" title)
                   (format "Expand %s" title))
      :on-click (lambda () (vui-set-state :expanded (not expanded))))
     (when expanded
       (apply #'vui-vstack
              (mapcar #'beads-section--issue-line-vnode issues))))))

;;; Mode

(defvar-keymap beads-section-mode-map
  :parent vui-mode-map
  "RET" #'beads-section-visit-issue)

;; TAB/S-TAB move by thing, SPC toggles, ? dispatches, C-c b is
;; reserved for extensions (dashboard-v3 §5.4, design.md §3.3).
(beads-mode--install-navigation-keys beads-section-mode-map)

(define-derived-mode beads-section-mode vui-mode "Beads"
  "Major mode for browsing beads issues using vui.el.

Provides collapsible section groups for issue categories with
keyboard navigation.  Sections are rendered by collecting vnodes
from `beads-status-sections-hook' and mounting them via vui.

Key bindings:
  TAB     — Move to the next thing (section header or issue line)
  S-TAB   — Move to the previous thing
  SPC     — Toggle the thing at point (fold a section)
  RET     — Visit issue at point (on issue lines)"
  :interactive nil)

;;; Section Spec Registry

(defclass beads-section-spec ()
  ((key
    :initarg :key
    :type symbol
    :documentation "Symbolic section key used for identity and lookup.")
   (title
    :initarg :title
    :type string
    :documentation "Human-readable section title.")
   (loader
    :initarg :loader
    :type function
    :documentation "Function of no arguments returning the section's data.")
   (renderer
    :initarg :renderer
    :type function
    :documentation "Function of one argument (the loader's data) returning a vnode.")
   (keys
    :initarg :keys
    :initform nil
    :type list
    :documentation "Optional list of extra keybindings the section wants.")
   (order
    :initarg :order
    :initform 0
    :type number
    :documentation "Sort order among registered sections; lower comes first."))
  "Descriptor for a named, renderable beads section.
A consumer registers one of these via `beads-section-register' so that
other views (status, dashboard, formula) can render it without knowing
where it came from.  Both `loader' and `renderer' are ordinary
functions: the loader runs with no arguments and returns the data, and
the renderer receives that data and returns a vui vnode.")

(defvar beads-section--registry (make-hash-table :test #'eq)
  "Registry of named sections, keyed by their symbolic `key'.
Populated by `beads-section-register'; consumed by
`beads-section-registered' and `beads-section-registered-vnodes'.")

(defun beads-section-register (key title loader renderer &optional keys)
  "Register a named section and return KEY.
KEY is a symbol identifying the section.  TITLE is its display title.
LOADER is a function of no arguments returning the section data.
RENDERER is a function of one argument (the loader's data) returning a
vui vnode.  KEYS, when non-nil, is a list of extra keybindings for the
section.  Registering the same KEY twice replaces the previous spec."
  (puthash key (beads-section-spec :key key :title title
                                    :loader loader :renderer renderer
                                    :keys keys)
           beads-section--registry)
  key)

(defun beads-section-spec-for (key)
  "Return the `beads-section-spec' registered for KEY, or nil.
KEY is a symbol; returns nil when no section is registered.  Named
`-for' because `beads-section-spec' is the EIEIO constructor."
  (gethash key beads-section--registry))

(defun beads-section-registered ()
  "Return all registered section specs, sorted by order then key.
A stable order lets the status and dashboard builders concatenate
registered sections deterministically."
  (let (specs)
    (maphash (lambda (_ spec) (push spec specs)) beads-section--registry)
    (sort specs
          (lambda (a b)
            (let ((ao (oref a order)) (bo (oref b order)))
              (if (= ao bo)
                  (string< (symbol-name (oref a key))
                           (symbol-name (oref b key)))
                (< ao bo)))))))

(defun beads-section-registered-vnodes ()
  "Return vnodes for every registered section, in registry order.
Calls each spec's loader and, when it returns non-nil, its renderer.
The empty registry returns nil (the standalone no-op)."
  (delq nil
        (mapcar (lambda (spec)
                  (when-let* ((data (funcall (oref spec loader))))
                    (funcall (oref spec renderer) data)))
                (beads-section-registered))))

;;; Status Sections Hook

(defcustom beads-status-sections-hook
  '(beads-insert-open-issues
    beads-insert-blocked-issues
    beads-insert-ready-work)
  "Hook listing functions that return vui vnodes for beads status buffers.

Each function takes no arguments and returns a vui vnode or nil.
The return values are collected and assembled into the status buffer.

Default functions:
- `beads-insert-open-issues'    — collapsible group of open issues
- `beads-insert-blocked-issues' — collapsible group of blocked issues
- `beads-insert-ready-work'     — collapsible group of ready work"
  :group 'beads
  :type 'hook)

;;; Insert Functions (return vui vnodes)

(defun beads-insert-open-issues ()
  "Return a collapsible vui vnode for open issues, or nil when none.
Fetches issues via `bd list --status open --json' and sorts by
priority (lowest number first)."
  (let* ((cmd (beads-command-list :status "open" :json t))
         (issues (beads-command-execute cmd)))
    (when issues
      (vui-component 'beads-section--issue-group
        :title "Open Issues"
        :issues (seq-sort-by (lambda (i) (or (oref i priority) 99))
                             #'< issues)))))

(defun beads-insert-blocked-issues ()
  "Return a collapsible vui vnode for blocked issues, or nil when none.
Fetches issues via `bd blocked --json'.
Issues with status \"hooked\" or \"in_progress\" are excluded because
they represent actively running work, not stalled work."
  (let* ((cmd (beads-command-blocked :json t))
         (all-issues (beads-command-execute cmd))
         (issues (seq-remove
                  (lambda (issue)
                    (member (oref issue status)
                            '("hooked" "in_progress")))
                  all-issues)))
    (when issues
      (vui-component 'beads-section--issue-group
        :title "Blocked Issues"
        :issues issues))))

(defun beads-insert-ready-work ()
  "Return a collapsible vui vnode for ready work, or nil when none.
Fetches issues via `bd ready --json'."
  (let* ((cmd (beads-command-ready :json t))
         (issues (beads-command-execute cmd)))
    (when issues
      (vui-component 'beads-section--issue-group
        :title "Ready Work"
        :issues issues))))

;;; Section Tree Builder

(defun beads-section-build-vnode ()
  "Build the complete section vnode tree from `beads-status-sections-hook'.
Calls each hook function, collects non-nil results, appends the vnodes
of every section in `beads-section--registry', and assembles them into a
`vui-vstack' with spacing between sections.  With no hook entries and an
empty registry this returns an empty vstack (the standalone no-op)."
  (let ((vnodes (append (delq nil (mapcar #'funcall beads-status-sections-hook))
                        (beads-section-registered-vnodes))))
    (apply #'vui-vstack :spacing 1 vnodes)))

;;; Commands

;;;###autoload
(defun beads-section-visit-issue ()
  "Visit the beads issue at point.

Reads the issue id from the `beads-section' text property at point
and calls `beads-show'.  Does nothing when point is not on an issue
line with a `beads-issue-section' context."
  (interactive)
  (when-let* ((id (beads-section-issue-id-at-point)))
    (beads-show id)))

(provide 'beads-section)
;;; beads-section.el ends here
