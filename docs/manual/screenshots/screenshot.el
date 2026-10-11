;;; screenshot.el --- Reusable Emacs GUI screenshot engine -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Commentary:

;; A small, package-agnostic engine for capturing high-resolution PNG
;; screenshots of Emacs UI buffers from a batchy, scripted GUI session.
;; It is the reusable half of the gascity.el documentation screenshot
;; pipeline; a sibling package (e.g. beads.el) reuses it verbatim by
;; providing its own load path, theme, and shot list (see the README in
;; this directory).
;;
;; The engine deliberately knows nothing about gascity: a caller sets the
;; configuration variables below, calls `screenshot-init' once to load a
;; theme and strip the frame chrome, then `screenshot-run' with a list of
;; shots.  Each shot opens a view and the engine fits the frame to the
;; rendered content and exports it with `x-export-frames'.
;;
;; Determinism: many porcelain views fetch their data asynchronously, so
;; `screenshot-settle' pumps the event loop until no live process matches
;; `screenshot-async-process-regexp' (or a fixed budget elapses).  The
;; caller sets that regexp to its async process name (gascity uses
;; "gascity-gc").
;;
;; This file must run in a *graphical* Emacs (not --batch): `x-export-frames'
;; needs a real frame.  The frame may live on a private headless Wayland
;; compositor so nothing pops onto the user's desktop (see capture.sh).

;;; Code:

(require 'cl-lib)

;;; Configuration (set by the caller before `screenshot-init')

(defvar screenshot-output-dir default-directory
  "Directory into which `screenshot-export' writes PNG files.")

(defvar screenshot-theme nil
  "Theme symbol to enable, or nil to keep the default faces.")

(defvar screenshot-theme-library nil
  "Library name whose directory holds THEME's `*-theme.el' files.
When non-nil, that directory is added to `custom-theme-load-path' so
`load-theme' can find themes installed outside the default search path
\(e.g. ef-themes from a Guix profile).  Example: \"ef-themes\".")

(defvar screenshot-font "DejaVu Sans Mono 12"
  "Frame font spec passed to `set-frame-font' (best effort).")

(defvar screenshot-async-process-regexp nil
  "Regexp matching the names of a view's async data processes.
`screenshot-settle' waits until no live process name matches.  nil
means do not wait on processes (settle a fixed budget only).")

(defvar screenshot-settle-seconds 15
  "Maximum seconds `screenshot-settle' waits for async data.")

(defvar screenshot-max-cols 230
  "Upper bound on the auto-fitted frame width, in columns.")
(defvar screenshot-min-cols 48
  "Lower bound on the auto-fitted frame width, in columns.")
(defvar screenshot-max-rows 92
  "Upper bound on the auto-fitted frame height, in rows.")
(defvar screenshot-baseline-cols 220
  "Frame width each shot starts at, before fitting to content.
Reset before every shot so a view renders at full size first; this is
what keeps paginated `tabulated-list-mode' views from opening into the
tiny frame the previous shot was fitted to.")
(defvar screenshot-baseline-rows 88
  "Frame height each shot starts at, before fitting to content.")
(defvar screenshot-min-rows 8
  "Lower bound on the auto-fitted frame height, in rows.")
(defvar screenshot-col-pad 2
  "Columns of breathing room added beyond the measured content width.")
(defvar screenshot-row-pad 1
  "Rows of breathing room added beyond the measured content height.")

(defvar screenshot-log-function #'ignore
  "Function called with a format string and args to report progress.")

(defun screenshot--log (fmt &rest args)
  "Report progress via `screenshot-log-function'."
  (apply screenshot-log-function fmt args))

;;; Setup

(defun screenshot-init ()
  "Strip frame chrome, hide the cursor, load the theme and font.
Reads the `screenshot-*' configuration variables.  Call once, after a
graphical frame exists and the configuration is set."
  (setq inhibit-startup-screen t
        ring-bell-function #'ignore
        make-backup-files nil
        auto-save-default nil
        use-dialog-box nil
        use-file-dialog nil
        frame-resize-pixelwise t
        truncate-lines t
        indicate-empty-lines nil
        indicate-buffer-boundaries nil
        scroll-bar-mode nil)
  ;; Clean documentation frames: no toolbar/menubar/scrollbars, slim
  ;; fringes, and no cursor box (the headless frame never has input
  ;; focus, so a focused cursor would render as a distracting hollow box).
  (when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
  (when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
  (when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
  (when (fboundp 'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode -1))
  (when (fboundp 'set-fringe-mode) (set-fringe-mode 8))
  (when (fboundp 'blink-cursor-mode) (blink-cursor-mode -1))
  (setq-default cursor-type nil
                cursor-in-non-selected-windows nil)
  ;; Make the theme discoverable, then load it.
  (when screenshot-theme-library
    (let ((dir (ignore-errors
                 (file-name-directory (locate-library screenshot-theme-library)))))
      (when dir (add-to-list 'custom-theme-load-path dir))))
  (when screenshot-theme
    (load-theme screenshot-theme t))
  (ignore-errors (set-frame-font screenshot-font nil t))
  ;; A neutral mode line height and no overlay arrows.
  (setq overlay-arrow-string ""))

;;; Settling async data

(defun screenshot-settle (&optional max)
  "Pump the event loop until async data has loaded.
Waits up to MAX seconds (default `screenshot-settle-seconds') for every
process whose name matches `screenshot-async-process-regexp' to exit,
then runs a few extra redisplay beats so the reconciler can repaint."
  (let ((deadline (+ (float-time) (or max screenshot-settle-seconds))))
    (when screenshot-async-process-regexp
      (while (and (< (float-time) deadline)
                  (cl-some
                   (lambda (p)
                     (string-match-p screenshot-async-process-regexp
                                     (process-name p)))
                   (process-list)))
        (accept-process-output nil 0.1)
        (redisplay t)))
    (dotimes (_ 6)
      (accept-process-output nil 0.08)
      (redisplay t))))

;;; Fitting the frame to content

(defun screenshot--content-cols (buffer)
  "Return the width in columns of the widest line in BUFFER.
Includes the header line (e.g. a `tabulated-list-mode' column header),
which lives outside the buffer text."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (let ((width 0))
        (while (not (eobp))
          (end-of-line)
          (setq width (max width (current-column)))
          (forward-line 1))
        (when header-line-format
          (setq width (max width
                           (string-width
                            (format-mode-line header-line-format)))))
        width))))

(defun screenshot--content-rows (buffer)
  "Return the number of screen lines of content in BUFFER."
  (with-current-buffer buffer
    (let ((lines (count-lines (point-min) (point-max))))
      ;; count-lines misses a final line lacking a trailing newline.
      (when (and (> (point-max) (point-min))
                 (/= (char-before (point-max)) ?\n))
        (setq lines (1+ lines)))
      lines)))

(defun screenshot--chrome-rows (buffer)
  "Return the frame rows BUFFER's chrome takes beyond its text area.
`set-frame-size' counts every screen line of the frame, not just the
selected window's text: the mode line, the header line when the buffer
has one, and the minibuffer window each take a row.  Leaving them out
makes the fitted frame short by up to three rows, which clips the tail
of the very content it was fitted to."
  (+ 1                                  ; mode line
     (if (buffer-local-value 'header-line-format buffer) 1 0)
     1))                                ; minibuffer window

(defun screenshot-fit-frame (buffer &optional fixed-rows)
  "Resize the selected frame to fit BUFFER's content.
Width always fits the content (clamped to the col bounds).  Height fits
the content plus its chrome (`screenshot--chrome-rows'), unless
FIXED-ROWS is non-nil, in which case the frame keeps that many rows
\(used for paginated `tabulated-list-mode' views whose page size tracks
the window height)."
  (let* ((cols (min screenshot-max-cols
                    (max screenshot-min-cols
                         (+ (screenshot--content-cols buffer)
                            screenshot-col-pad))))
         (rows (or fixed-rows
                   (min screenshot-max-rows
                        (max screenshot-min-rows
                             (+ (screenshot--content-rows buffer)
                                (screenshot--chrome-rows buffer)
                                screenshot-row-pad))))))
    (set-frame-size (selected-frame) cols rows)
    (redisplay t)))

;;; Exporting

(defun screenshot-export (buffer name &optional fixed-rows)
  "Display BUFFER full-frame, fit the frame, and write NAME.png.
Returns the output path.  FIXED-ROWS is passed to `screenshot-fit-frame'."
  (let ((buf (get-buffer buffer))
        (path (expand-file-name (concat name ".png") screenshot-output-dir)))
    (switch-to-buffer buf)
    (delete-other-windows)
    (goto-char (point-min))
    (set-window-start (selected-window) (point-min))
    (screenshot-fit-frame buf fixed-rows)
    (redisplay t)
    (let ((data (x-export-frames (selected-frame) 'png))
          (coding-system-for-write 'binary))
      (with-temp-file path
        (set-buffer-multibyte nil)
        (insert data)))
    (screenshot--log "exported %-22s %3dx%-3d %6d bytes" name
                     (frame-width) (frame-height)
                     (or (nth 7 (file-attributes path)) -1))
    path))

(defun screenshot-export-frame (name)
  "Write the whole current frame to NAME.png as-is (no buffer fitting).
Used for overlay UIs such as a transient popup that span several windows."
  (let ((path (expand-file-name (concat name ".png") screenshot-output-dir)))
    (redisplay t)
    (let ((data (x-export-frames (selected-frame) 'png))
          (coding-system-for-write 'binary))
      (with-temp-file path
        (set-buffer-multibyte nil)
        (insert data)))
    (screenshot--log "exported %-22s (frame) %6d bytes" name
                     (or (nth 7 (file-attributes path)) -1))
    path))

;;; Running a shot list

(defun screenshot-run (shots)
  "Capture each shot in SHOTS.
Each shot is a plist-tailed list: (NAME THUNK . PROPS).  THUNK opens a
view and returns the buffer (object or name) to capture.  PROPS may
include:

  :settle SECONDS   wait budget for this shot's async data
  :fixed-rows N     keep the frame N rows tall (paginated lists)
  :row-pad N        extra rows past the content (default `screenshot-row-pad')
  :col-pad N        extra columns past the content (default `screenshot-col-pad')
  :whole-frame t    export the entire frame via THUNK's own arrangement
                    (THUNK's return value is ignored)

A failing shot is logged and skipped so one broken view never aborts the
run."
  (dolist (shot shots)
    (let* ((name (nth 0 shot))
           (thunk (nth 1 shot))
           (props (cddr shot)))
      (condition-case err
          (let ((ret (progn
                       ;; Start every shot from a large frame so the view
                       ;; renders at full size before we fit it down.
                       (set-frame-size (selected-frame)
                                       screenshot-baseline-cols
                                       screenshot-baseline-rows)
                       (redisplay t)
                       (funcall thunk)))
                (screenshot-row-pad (or (plist-get props :row-pad)
                                        screenshot-row-pad))
                (screenshot-col-pad (or (plist-get props :col-pad)
                                        screenshot-col-pad)))
            (screenshot-settle (plist-get props :settle))
            (if (plist-get props :whole-frame)
                (screenshot-export-frame name)
              (screenshot-export ret name (plist-get props :fixed-rows))))
        (error (screenshot--log "SHOT %-22s FAILED: %S" name err))))))

(provide 'screenshot)
;;; screenshot.el ends here
