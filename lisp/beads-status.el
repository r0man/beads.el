;;; beads-status.el --- Hand-built status buffer for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; `beads-status' is the primary entry point of beads.el (REQ-001): a
;; hand-built, vui-sectioned status buffer that summarises the store
;; and offers dispatch, opening with `M-x beads'.  It reuses the
;; asynchronous board loaders defined for `beads-dashboard' (which
;; remains the full board) so every built-in section goes through
;; `beads-command-execute-async' and no synchronous `bd' call runs at
;; render time.
;;
;; Built-in sections live in `beads-status--sections'.  Downstream
;; packages extend the board by registering a `beads-section-spec' via
;; `beads-section-register' (WI-4 seam) or by contributing to
;; `beads-dashboard-section-providers'; those sections render through
;; the same error-bounded section shell.
;;
;; The former `beads' transient prefix now lives in `beads-menu.el' as
;; `beads-dispatch', bound to `?' (REQ-002).  `beads-dashboard' stays
;; the full-board alias.

;;; Code:

(require 'eieio)
(require 'cl-lib)
(require 'vui)
(require 'beads-section)
(require 'beads-command)
(require 'beads-dashboard)
(require 'beads-dashboard-sections)
(require 'beads-util)
;; Load the `bd status' command class first so its auto-generated
;; `beads-status' transient runs before our `defun' claims the symbol
;; slot (same dance as the legacy compat shim).
(require 'beads-command-status)

(declare-function beads-dashboard--section "beads-dashboard")
(declare-function beads-dashboard--load-visibility "beads-dashboard")
(declare-function beads-dashboard--save-visibility "beads-dashboard")
(declare-function beads-dashboard--root-state "beads-dashboard")
(declare-function beads-dashboard--bump "beads-dashboard")
(declare-function beads-dashboard--no-project-vnode "beads-dashboard")
(declare-function beads-dashboard--provider-specs "beads-dashboard-sections")
(declare-function beads-dashboard--in-flight-loader "beads-dashboard-sections")
(declare-function beads-dashboard--ready-loader "beads-dashboard-sections")
(declare-function beads-dashboard--blocked-loader "beads-dashboard-sections")
(declare-function beads-dashboard--closed-loader "beads-dashboard-sections")
(declare-function beads-dashboard-render-in-flight "beads-dashboard-sections")
(declare-function beads-dashboard-render-ready "beads-dashboard-sections")
(declare-function beads-dashboard-render-blocked "beads-dashboard-sections")
(declare-function beads-dashboard-render-closed "beads-dashboard-sections")
(declare-function beads--project-root "beads-util")
(declare-function beads--get-database-path "beads-util")
(declare-function beads-store-resolve "beads-util")
(declare-function beads-prefix-invocation-directory "beads-prefix")

;;; Buffer Identity

(defvar-local beads-status--root nil
  "The project root this status buffer was opened for, resolved at open.")

(defun beads-status--buffer-name-for (root)
  "Return the status buffer name for project ROOT.
A remote ROOT is qualified with its TRAMP prefix so a local and a
remote project with the same basename get distinct buffers."
  (if root
      (let ((remote (file-remote-p root))
            (name (file-name-nondirectory (directory-file-name root))))
        (if remote
            (format "*beads-status<%s|%s>*" remote name)
          (format "*beads-status<%s>*" name)))
    "*beads-status*"))

;;; Built-in Sections
;;
;; A curated subset of the full dashboard: the actionable buckets
;; first, then recent forward motion.  Each entry reuses the matching
;; dashboard asynchronous loader/renderer pair, so the status board
;; inherits the loading/empty/error/ready contract.

(defconst beads-status--sections
  '((:key in-flight :title "In progress" :icon "🚧"
          :loader beads-dashboard--in-flight-loader
          :renderer beads-dashboard-render-in-flight)
    (:key ready :title "Ready" :icon "✅"
          :loader beads-dashboard--ready-loader
          :renderer beads-dashboard-render-ready)
    (:key blocked :title "Blocked" :icon "🔒"
          :loader beads-dashboard--blocked-loader
          :renderer beads-dashboard-render-blocked)
    (:key closed :title "Recent activity" :icon "📦"
          :loader beads-dashboard--closed-loader
          :renderer beads-dashboard-render-closed))
  "Built-in status-buffer sections.
Each plist carries :key, :title, :icon, an asynchronous :loader
\(function of the bd store path returning a `vui-use-async' thunk) and
a :renderer (function of the loaded data returning a vnode).")

;;; Header / Footer

(defun beads-status--header-vnode (root db)
  "Return the status-buffer header vnode for project ROOT and DB path."
  (let ((project-name (if root
                          (file-name-nondirectory (directory-file-name root))
                        "unknown")))
    (vui-hstack :spacing 0
      (vui-text "Beads" :face 'bold)
      (vui-text " — " :face 'shadow)
      (vui-text project-name :face 'font-lock-constant-face)
      (when db
        (vui-text (format " (%s)" db) :face 'shadow)))))

(defun beads-status--footer-vnode ()
  "Return the status-buffer footer vnode (key hints)."
  (vui-text
   "KEYS: TAB/S-TAB next/prev · SPC fold · RET visit · g refresh · ? dispatch · q bury"
   :face 'shadow))

;;; Extension Sections

(defun beads-status--extension-specs ()
  "Return downstream `beads-section-spec' objects for the status board.
Combines the WI-4 registry (`beads-section-registered') with the
dashboard provider hook, deduped by key; the registry wins on a
collision.  The empty set is the standalone no-op."
  (let ((seen (make-hash-table :test #'eq))
        (specs nil))
    (dolist (spec (append (beads-section-registered)
                          (beads-dashboard--provider-specs)))
      (when (eieio-object-p spec)
        (let ((key (oref spec key)))
          (unless (gethash key seen)
            (puthash key t seen)
            (push spec specs)))))
    (nreverse specs)))

(defun beads-status--section-vnode (section collapsed generation buffer db-path)
  "Return the vnode for a built-in SECTION plist.
COLLAPSED, GENERATION and BUFFER drive the shared
`beads-dashboard--section' dispatch and its toggle callback; DB-PATH
scopes the asynchronous loader."
  (let* ((key (plist-get section :key))
         (loader-fn (plist-get section :loader))
         (renderer (or (plist-get section :renderer)
                       (lambda (data) (vui-text (format "  %S" data)
                                                :face 'shadow)))))
    (beads-dashboard--section
     key (plist-get section :title)
     (and loader-fn (funcall loader-fn db-path))
     renderer
     collapsed generation buffer
     :icon (plist-get section :icon)
     :hide-count (plist-get section :hide-count)
     :force-render (plist-get section :force-render)
     :db-path db-path)))

(defun beads-status--extension-vnodes (collapsed generation buffer db-path)
  "Return vnodes for downstream extension sections.
Each spec's synchronous loader runs inside the same async section
shell the built-in sections use, so a failing extension shows a
section-local error rather than blanking the board.  COLLAPSED,
GENERATION and BUFFER are threaded to each section exactly as the
built-in sections receive them; DB-PATH scopes the render.  The
empty set returns nil."
  (mapcar (lambda (spec)
            (beads-dashboard--section
             (oref spec key) (oref spec title)
             (lambda (resolve reject)
               (condition-case err
                   (funcall resolve (funcall (oref spec loader)))
                 (error (funcall reject err))))
             (oref spec renderer)
             collapsed generation buffer
             :db-path db-path))
          (beads-status--extension-specs)))

;;; Root Component

(vui-defcomponent beads-status--root (project-root db-path)
  "Root vui component for the beads status buffer.
PROJECT-ROOT keys the visibility cache and labels the header.
DB-PATH is resolved once at mount and threaded through so the header
does not stat the filesystem on every reconcile.  Built-in sections
and downstream extension sections both render through the shared
dashboard section component, so they inherit the
async/loading/empty/error contract."
  :state ((collapsed (beads-dashboard--load-visibility project-root))
          (generation 0))
  :render
  (let ((buffer (current-buffer)))
    (if (null project-root)
        (vui-vstack
         :spacing 1
         (beads-status--header-vnode project-root db-path)
         (beads-dashboard--no-project-vnode)
         (beads-status--footer-vnode))
      (vui-vstack
       :spacing 1
       (beads-status--header-vnode project-root db-path)
       (apply #'vui-vstack
              :spacing 1
              (append
               (mapcar (lambda (section)
                         (beads-status--section-vnode
                          section collapsed generation buffer db-path))
                       beads-status--sections)
               (beads-status--extension-vnodes
                collapsed generation buffer db-path)))
       (beads-status--footer-vnode)))))

;;; Refresh

(defun beads-status-refresh ()
  "Refresh stale status sections by bumping the generation counter."
  (interactive)
  (when (derived-mode-p 'beads-status-mode)
    (beads-dashboard--bump
     :generation
     (1+ (or (beads-dashboard--root-state :generation) 0)))
    (message "Beads status refreshed.")))

(defun beads-status--revert (_ignore _noconfirm)
  "Revert the status buffer (no-prompt revert hook)."
  (beads-status-refresh))

;;; Mode

(defvar-keymap beads-status-mode-map
  :parent beads-section-mode-map
  "g"   #'beads-status-refresh
  "q"   #'quit-window
  "?"   #'beads-dispatch
  ;; Bind both `RET' (TTY/C-m) and `<return>' (GUI) so visit fires in
  ;; both terminal and windowed Emacs.
  "RET"      #'beads-section-visit-issue
  "<return>" #'beads-section-visit-issue)

(define-derived-mode beads-status-mode beads-section-mode "Beads-Status"
  "Major mode for the hand-built beads status buffer.

Derived from `beads-section-mode' so the `beads-section' text-property
contract (RET to visit, eldoc, thing motion) is preserved while `q',
`g' and `?' obey the REQ-002 navigation contract.

\\{beads-status-mode-map}"
  :interactive nil
  (setq-local truncate-lines t)
  (setq-local revert-buffer-function #'beads-status--revert))

;;; Entry Point

;;;###autoload
(cl-defun beads-status (&key directory)
  "Open or refresh the hand-built beads status buffer for this project.

With DIRECTORY non-nil, scope the board to the bead store at DIRECTORY
instead of resolving from `default-directory'.  Without it, a call
from the menu of `project-switch-project' opens the board of the
chosen project.

Each section loads asynchronously; no synchronous `bd' call runs at
render time."
  (interactive)
  (let* ((store (beads-store-resolve directory))
         (default-directory (or store
                                (beads-prefix-invocation-directory)))
         ;; A remote explicit store is its own root: no VC/marker walk,
         ;; which is synchronous TRAMP I/O.
         (root (if (and store (file-remote-p store))
                   store
                 (beads--project-root)))
         (buf-name (beads-status--buffer-name-for root))
         (buf (get-buffer-create buf-name))
         (db (unless (file-remote-p default-directory)
               (ignore-errors (beads--get-database-path)))))
    (with-current-buffer buf
      (unless (eq major-mode 'beads-status-mode)
        (beads-status-mode))
      (setq-local beads-store-directory
                  (or store (and root (file-remote-p root) root)))
      (setq-local beads-status--root root)
      ;; Share the dashboard's buffer-local root so the inherited
      ;; visibility/toggle helpers key on the same project.
      (setq-local beads-dashboard--root root))
    (unless beads-command--policy
      (beads-command--policy-probe (lambda (_p) (ignore))))
    (with-current-buffer buf
      (vui-mount
       (vui-component 'beads-status--root
                      :project-root root
                      :db-path db)
       (buffer-name)))
    (pop-to-buffer buf)))

;;; Legacy Entry Point

;; The auto-generated `bd status' transient clears the symbol slot but
;; leaves an `interactive-only' property behind; our `beads-status' is
;; a genuine callable command, so drop it before `beads' calls it.
;; `eval-and-compile' so the compiler does not warn on the call below.
(eval-and-compile (put 'beads-status 'interactive-only nil))

;;;###autoload
(defun beads ()
  "Open the beads status buffer.

The primary entry point of beads.el (REQ-001).  Press `?' inside the
buffer for the `beads-dispatch' menu."
  (interactive)
  (funcall #'beads-status))

(provide 'beads-status)
;;; beads-status.el ends here