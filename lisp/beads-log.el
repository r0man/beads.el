;;; beads-log.el --- Durable command/error log for async bd invocations -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is part of beads.el.

;;; Commentary:

;; A durable, always-on record of asynchronous `bd' invocations.  The
;; debug buffer (`*beads-debug*', see `beads-enable-debug') is a
;; developer sink gated behind a defcustom; this module is the
;; user-facing counterpart gascity.el already ships as `*gascity-log*'.
;;
;; Every async invocation that reaches `beads-command--spawn-async'
;; records one entry here on completion (success, failure, timeout, or
;; spawn error): timestamp, resolved store, the `bd' argv, the exit
;; code, the duration, and the captured stderr or error message.  The
;; entries live in memory (`beads-log--entries') so they survive killing
;; the buffer, and are bounded by `beads-log-max-entries' so the log can
;; never grow without limit.
;;
;; This is an additive sink: the per-view error display and the debug
;; buffer are unchanged.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'button)
(require 'beads-custom)

(defgroup beads-log nil
  "Durable command log for beads.el."
  :group 'beads)

(defcustom beads-log-buffer-name "*beads-log*"
  "Name of the buffer that displays the beads command log."
  :type 'string
  :group 'beads-log)

(defcustom beads-log-max-entries 200
  "Maximum number of async command entries retained.
Once the log grows past this bound the oldest entries are dropped, so
the in-memory log and its buffer cannot grow without limit."
  :type 'integer
  :group 'beads-log)

(defcustom beads-log-max-stderr-chars 4000
  "Maximum number of stderr characters retained per entry.
Longer stderr is truncated with a marker so one noisy command cannot
dominate the log."
  :type 'integer
  :group 'beads-log)

(defvar beads-log--entries nil
  "Newest-first list of log entry plists.
Each entry has :timestamp, :status, :store, :argv, :exit-code,
:duration, :stderr, and :error.  Kept in memory so the log survives
killing the log buffer.")

;;; Recording

(defun beads-log--truncate (string max)
  "Return STRING truncated to MAX characters, or STRING unchanged.
A truncation marker is appended when STRING is longer than MAX."
  (if (and (stringp string) (> (length string) max))
      (concat (substring string 0 max) "\n...[truncated]")
    string))

(defun beads-log--argv-string (argv)
  "Return ARGV as a display string.
ARGV may already be a string (the usual case) or a list of arguments."
  (cond
   ((null argv) nil)
   ((stringp argv) argv)
   ((listp argv) (mapconcat #'shell-quote-argument argv " "))
   (t (format "%s" argv))))

(defun beads-log-record (&rest plist)
  "Append one entry describing an async bd invocation.
PLIST keys:

  :timestamp  time value (defaults to `current-time')
  :status     symbol: `ok', `failed', `timeout', `parse-error', or
              `spawn-error'
  :store      directory the command ran in
  :argv       command line as a string or list of arguments
  :exit-code  integer exit status, when a process ran
  :duration   elapsed time in seconds as a float
  :stderr     captured stderr string
  :error      human-readable error message

Returns the recorded entry.  The oldest entries past
`beads-log-max-entries' are discarded, and any live log buffer is
refreshed."
  (let ((entry (list :timestamp (or (plist-get plist :timestamp)
                                    (current-time))
                     :status (or (plist-get plist :status) 'ok)
                     :store (plist-get plist :store)
                     :argv (beads-log--argv-string (plist-get plist :argv))
                     :exit-code (plist-get plist :exit-code)
                     :duration (plist-get plist :duration)
                     :stderr (beads-log--truncate (plist-get plist :stderr)
                                                  beads-log-max-stderr-chars)
                     :error (plist-get plist :error))))
    (push entry beads-log--entries)
    (when (and (integerp beads-log-max-entries)
               (> (length beads-log--entries) beads-log-max-entries))
      (setq beads-log--entries (seq-take beads-log--entries
                                         beads-log-max-entries)))
    (beads-log--refresh-live-buffers)
    entry))

(defun beads-log-entries ()
  "Return the recorded log entries, newest first."
  beads-log--entries)

(defun beads-log-clear-entries ()
  "Discard every recorded log entry."
  (setq beads-log--entries nil))

;;; Rendering

(defun beads-log--status-label (status)
  "Return a short display label for STATUS."
  (pcase status
    ('ok "OK")
    ('failed "FAILED")
    ('timeout "TIMEOUT")
    ('parse-error "PARSE-ERROR")
    ('spawn-error "SPAWN-ERROR")
    (_ (upcase (format "%s" status)))))

(defun beads-log--status-face (status)
  "Return a face used to display an entry with STATUS."
  (pcase status
    ('ok 'success)
    ('failed 'error)
    ('timeout 'warning)
    ('parse-error 'error)
    ('spawn-error 'error)
    (_ 'default)))

(defun beads-log--format-duration (duration)
  "Return DURATION (seconds) formatted for display, or \"-\"."
  (if (numberp duration)
      (format "%.3fs" duration)
    "-"))

(defun beads-log-copy-argv (button)
  "Copy the argv recorded on BUTTON to the kill ring."
  (let ((argv (button-get button 'beads-argv)))
    (when argv
      (kill-new argv)
      (message "Copied command line: %s" argv))))

(define-button-type 'beads-log-argv
  'follow-link t
  'action #'beads-log-copy-argv
  'mouse-face 'highlight
  'help-echo "Click to copy the command line"
  :supertype 'button)

(defun beads-log--insert-entry (entry)
  "Insert a rendered ENTRY at point."
  (let ((status (plist-get entry :status))
        (store (plist-get entry :store))
        (argv (plist-get entry :argv))
        (exit (plist-get entry :exit-code))
        (err (plist-get entry :error))
        (stderr (plist-get entry :stderr)))
    (insert (propertize
             (format-time-string "%Y-%m-%d %H:%M:%S" (plist-get entry :timestamp))
             'face 'font-lock-comment-face))
    (insert "  ")
    (insert (propertize (format "%-11s" (beads-log--status-label status))
                        'face (beads-log--status-face status)))
    (insert (format " exit %-4s"
                    (if (numberp exit) (number-to-string exit) "-")))
    (insert (format " %8s\n" (beads-log--format-duration
                              (plist-get entry :duration))))
    (when (and (stringp store) (not (string-empty-p store)))
      (insert "  store:  " store "\n"))
    (when (and (stringp argv) (not (string-empty-p argv)))
      (insert "  argv:   ")
      (insert-text-button argv :type 'beads-log-argv 'beads-argv argv)
      (insert "\n"))
    (when (and (stringp err) (not (string-empty-p err)))
      (insert "  error:  " err "\n"))
    (when (and (stringp stderr) (not (string-empty-p (string-trim stderr))))
      (insert "  stderr: ")
      (insert (replace-regexp-in-string "\n" "\n          "
                                        (string-trim-right stderr)))
      (insert "\n"))
    (insert "\n")))

(defun beads-log--refresh ()
  "Re-render the current buffer from `beads-log--entries'."
  (let ((inhibit-read-only t)
        (entries beads-log--entries))
    (erase-buffer)
    (if (null entries)
        (insert "No async bd invocations recorded yet.\n")
      (dolist (entry entries)
        (beads-log--insert-entry entry)))
    (goto-char (point-min))))

(defun beads-log--refresh-live-buffers ()
  "Refresh any live log buffer from `beads-log--entries'."
  (when-let* ((buf (get-buffer beads-log-buffer-name)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (derived-mode-p 'beads-log-mode)
          (beads-log--refresh))))))

;;; Commands and Mode

(defvar beads-log-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "g") #'beads-log-refresh)
    (define-key map (kbd "c") #'beads-log-clear)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `beads-log-mode'.")

(define-derived-mode beads-log-mode special-mode "Beads-Log"
  "Major mode for the durable Beads command log.
\\{beads-log-mode-map}"
  (setq truncate-lines nil)
  (setq buffer-read-only t))

;;;###autoload
(defun beads-log-refresh ()
  "Refresh the log buffer from the recorded entries."
  (interactive)
  (beads-log--refresh)
  (message "Beads log refreshed"))

;;;###autoload
(defun beads-log-clear ()
  "Discard every recorded entry and refresh the log buffer."
  (interactive)
  (beads-log-clear-entries)
  (beads-log--refresh)
  (message "Beads log cleared"))

;;;###autoload
(defun beads-log ()
  "Open the durable Beads command log.

Every async bd invocation is recorded here with its store, argv, exit
status, duration, and stderr.  Unlike the debug buffer
\(`beads-show-debug-buffer') this is always on and is not gated by
`beads-enable-debug'."
  (interactive)
  (let ((buf (get-buffer-create beads-log-buffer-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'beads-log-mode)
        (beads-log-mode))
      (beads-log--refresh))
    (pop-to-buffer buf)))

(provide 'beads-log)
;;; beads-log.el ends here
