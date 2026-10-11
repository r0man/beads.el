;;; beads-pulse.el --- Global mode-line pulse for live bead stores -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Beads Contributors
;; Keywords: tools

;; This file is part of beads.el.

;;; Commentary:

;; The cross-store mode-line lighter for the live events journal
;; (WI-LIVE-17, US-4/AC-7; design.md §3.4, §7.10/§7.12).
;;
;; `beads-pulse-mode' is an opt-in global minor mode.  Its mode-line
;; construct is a plain string variable rebuilt only when the in-memory
;; state changes, so redisplay evaluates nothing: it never runs `bd',
;; never touches TRAMP and never does file I/O.  Every figure comes from
;; state the views and the event model already hold in memory:
;;
;; - `beads-pulse-publish' lets a view (the dashboard, a list, show, the
;;   events timeline) report its store's open/in-flight/blocked counts
;;   and sequence for the lighter.
;; - Live stream entries are included even with no open view, derived
;;   from `beads-live' status and the pure event model counts.
;; - `beads-pulse-record' keeps a small per-store ring of samples that
;;   `beads-pulse-sparkline' draws as `▁▂▃…'.
;;
;; The lighter is deliberately independent of `beads-live': every call
;; into it is resolved at runtime and guarded, so `beads-pulse-mode'
;; loads and works (over published figures) when the stream supervisor
;; is absent.  The lighter is distinct from
;; `beads-dashboard--update-mode-line', which owns `mode-line-misc-info'
;; for a dashboard buffer; the pulse is the global cross-store strip.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'beads-custom)

(defvar beads-live--streams)
(defvar beads-live--models)
(defvar beads-live-state-functions)
(defvar beads-event-hooks)

(declare-function beads-live-canonical-root "beads-live" (dir))
(declare-function beads-live-status "beads-live" (&optional dir))
(declare-function beads-live--model-issues "beads-live" (model))

(defcustom beads-pulse-samples 40
  "How many samples `beads-pulse-record' keeps per store.
This is also the width of the `beads-pulse-sparkline'."
  :type 'natnum
  :group 'beads)

;;; City keys

(defun beads-pulse-city-key (dir)
  "Return the pulse key of the store rooted at DIR.
The canonical store root when `beads-live' is available, so every
spelling of one store maps to one lighter segment (a remote name stays
host-qualified and distinct from a local store of the same name)."
  (cond
   ((null dir) nil)
   ((fboundp 'beads-live-canonical-root)
    (beads-live-canonical-root dir))
   (t (file-name-as-directory (expand-file-name dir)))))

(defun beads-pulse-abbrev (name)
  "Return the lighter abbreviation of store NAME: `beads.el' → `be'.
The initials of its `-'/`_'/`.'-separated words, or the first two
letters of a one-word name."
  (let ((words (split-string (or name "?") "[-_. ]+" t)))
    (if (cdr words)
        (mapconcat (lambda (w) (substring w 0 1)) words "")
      (let ((w (or (car words) "?")))
        (substring w 0 (min 2 (length w)))))))

;;; Published figures

(defvar beads-pulse--cities (make-hash-table :test 'equal)
  "City key → plist (:name :buffer :counts :seq :at).
Filled by `beads-pulse-publish'; an entry whose buffer died is dropped
by `beads-pulse--forget'.")

(defun beads-pulse-publish (dir &optional buffer name &rest counts)
  "Record the live figures of the store rooted at DIR.
BUFFER is the view reporting them, NAME the store display name; COUNTS
is a plist with any of `:open', `:inflight', `:blocked', `:ready' and
`:seq'.  Rebuilds the lighter string only when a figure changed, so a
re-render with the same data costs nothing."
  (when dir
    (let* ((key (beads-pulse-city-key dir))
           (old (gethash key beads-pulse--cities))
           (new (list :name name :buffer buffer :counts counts
                      :seq (plist-get counts :seq) :at (float-time))))
      (unless (and old
                   (eq (plist-get old :buffer) buffer)
                   (equal (plist-get old :name) name)
                   (equal (plist-get old :counts) counts))
        (puthash key new beads-pulse--cities)
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (add-hook 'kill-buffer-hook #'beads-pulse--forget nil t)))
        (beads-pulse--update))
      ;; Keep the timestamp fresh without a rebuild.
      (when old
        (plist-put (gethash key beads-pulse--cities) :at (float-time))))))

(defun beads-pulse--forget ()
  "Drop the pulse entries of the view buffer being killed."
  (let ((buffer (current-buffer)) (dead nil))
    (maphash (lambda (key entry)
               (when (eq (plist-get entry :buffer) buffer) (push key dead)))
             beads-pulse--cities)
    (when dead
      (dolist (key dead) (remhash key beads-pulse--cities))
      (beads-pulse--update))))

(defun beads-pulse-city (dir)
  "Return the published plist of the store rooted at DIR, or nil.
Only entries whose reporting buffer is still live count."
  (let ((entry (gethash (beads-pulse-city-key dir) beads-pulse--cities)))
    (and entry (buffer-live-p (plist-get entry :buffer)) entry)))

(defun beads-pulse-cities ()
  "Return the live published entries as (KEY . PLIST), sorted by name.
Pure: reads the in-memory table only, so it is safe at redisplay."
  (let (out)
    (maphash (lambda (key entry)
               (when (buffer-live-p (plist-get entry :buffer))
                 (push (cons key entry) out)))
             beads-pulse--cities)
    (sort out (lambda (a b)
                (or (string< (or (plist-get (cdr a) :name) "")
                             (or (plist-get (cdr b) :name) ""))
                    (and (equal (plist-get (cdr a) :name)
                                (plist-get (cdr b) :name))
                         (string< (car a) (car b))))))))

;;; Live-stream figures (no published view)

(defun beads-pulse--model-counts (root)
  "Return the in-memory status counts of ROOT's event model, or nil.
Keys `:open', `:inflight' and `:blocked'; pure, no `bd' call."
  (when (and (boundp 'beads-live--models)
             (fboundp 'beads-live--model-issues))
    (when-let* ((model (gethash root beads-live--models)))
      (let ((open 0) (inflight 0) (blocked 0))
        (maphash (lambda (_id issue)
                   (when (consp issue)
                     (let ((status (alist-get 'status issue)))
                       (cond ((or (null status) (equal status "open"))
                              (setq open (1+ open)))
                             ((equal status "in_progress")
                              (setq inflight (1+ inflight)))
                             ((equal status "blocked")
                              (setq blocked (1+ blocked)))))))
                 (beads-live--model-issues model))
        (list :open open :inflight inflight :blocked blocked)))))

(defun beads-pulse--stream-entries ()
  "Return live stream entries as (KEY . PLIST).
A stream is only an entry when it has no published view, so one store
never appears twice.  Pure: reads the stream and model tables only."
  (when (and (boundp 'beads-live--streams)
             (fboundp 'beads-live-status))
    (let (out)
      (maphash
       (lambda (root stream)
         (ignore stream)
         (let ((key (beads-pulse-city-key root)))
           (unless (gethash key beads-pulse--cities)
             (when-let* ((status (beads-live-status root)))
               (push (cons key
                           (list :name (or (plist-get status :name)
                                           "store")
                                 :counts (or (beads-pulse--model-counts root)
                                             '(:open 0 :inflight 0 :blocked 0))
                                 :seq (plist-get status :seq)
                                 :state (plist-get status :state)
                                 :rate (plist-get status :rate)
                                 :at (float-time)))
                     out)))))
       beads-live--streams)
      out)))

;;; Samples and sparkline

(defvar beads-pulse--samples (make-hash-table :test 'equal)
  "City key → (LAST . VALUES), VALUES newest first.")

(defun beads-pulse-record (dir &optional value)
  "Sample VALUE for the store rooted at DIR, for the sparkline.
VALUE may be a number or a plist (`:seq', `:rate'); nil samples the
current stream sequence when `beads-live' is available.  A repeated
sample of the same plist is skipped, so one read yields one sample
however often its view re-renders.  Keeps at most
`beads-pulse-samples' values.  Returns the new sample list, oldest
first."
  (when dir
    (let* ((key (beads-pulse-city-key dir))
           (cell (gethash key beads-pulse--samples))
           (n (cond ((numberp value) value)
                    ((and (consp value) (numberp (plist-get value :seq)))
                     (plist-get value :seq))
                    ((and (consp value) (numberp (plist-get value :rate)))
                     (plist-get value :rate))
                    (t (plist-get (and (fboundp 'beads-live-status)
                                       (beads-live-status dir))
                                  :seq)))))
      (when (numberp n)
        (unless (eq (car cell) value)
          (puthash key
                   (cons value (seq-take (cons n (cdr cell))
                                         beads-pulse-samples))
                   beads-pulse--samples)
          (beads-pulse--update))))
    (reverse (cdr (gethash (beads-pulse-city-key dir) beads-pulse--samples)))))

(defconst beads-pulse--spark "▁▂▃▄▅▆▇█"
  "The sparkline levels, lowest first.")

(defun beads-pulse-sparkline (values)
  "Return VALUES (numbers) as a `▁▂▃…' sparkline string.
The range min..max maps onto the eight levels; a flat series is all
`▁'.  Nil VALUES yields the empty string.  Pure."
  (if (null values)
      ""
    (let* ((lo (apply #'min values))
           (hi (apply #'max values))
           (span (- hi lo))
           (top (1- (length beads-pulse--spark))))
      (mapconcat (lambda (v)
                   (let ((i (if (zerop span) 0
                              (min top (floor (* (/ (float (- v lo)) span)
                                                 (+ top 0.999)))))))
                     (string (aref beads-pulse--spark i))))
                 values ""))))

;;; Lighter text

(defvar beads-pulse--string ""
  "The lighter's current text; the mode line shows this variable as-is.")
(put 'beads-pulse--string 'risky-local-variable t)

(defun beads-pulse--state-glyph (state)
  "Return the one-character glyph for stream STATE.  Pure."
  (pcase state
    ('live "●")
    ('poll "↻")
    ('partial "◐")
    ((or 'connecting 'reconnecting) "○")
    ('offline "○")
    ('off "○")
    (_ "·")))

(defun beads-pulse--count-string (counts)
  "Return COUNTS as `o·i·b', or nil when there is nothing to show.
COUNTS is a plist of `:open', `:inflight' and `:blocked'.  Pure."
  (let ((open (or (plist-get counts :open) 0))
        (inflight (or (plist-get counts :inflight) 0))
        (blocked (or (plist-get counts :blocked) 0)))
    (when (or (> open 0) (> inflight 0) (> blocked 0))
      (format "%d·%d·%d" open inflight blocked))))

(defun beads-pulse--counts-face (counts)
  "Return the mode-line face for COUNTS: attention when blocked.  Pure."
  (if (> (or (plist-get counts :blocked) 0) 0) 'warning 'mode-line))

(defun beads-pulse--visit (buffer)
  "Return a mouse command that shows BUFFER."
  (lambda (event)
    (interactive "e")
    (ignore event)
    (if (buffer-live-p buffer)
        (pop-to-buffer buffer)
      (message "That beads view is gone"))))

(defun beads-pulse--segment (key entry)
  "Return the lighter segment of store KEY with ENTRY.
ENTRY is a plist of `:name', `:buffer', `:counts', `:seq', `:state',
`:rate'.  Pure: only propertizing in-memory values."
  (let* ((name (or (plist-get entry :name) "store"))
         (abbrev (beads-pulse-abbrev name))
         (state (plist-get entry :state))
         (counts (beads-pulse--count-string (plist-get entry :counts)))
         (text (concat abbrev
                       " " (beads-pulse--state-glyph state)
                       (if counts (concat " " counts) "")))
         (buffer (plist-get entry :buffer))
         (map (and (buffer-live-p buffer) (make-sparse-keymap))))
    (when map
      (define-key map [mode-line mouse-1] (beads-pulse--visit buffer)))
    (propertize text
                'face (beads-pulse--counts-face (plist-get entry :counts))
                'mouse-face (and map 'mode-line-highlight)
                'local-map map
                'help-echo (format "%s: %s%s%s — mouse-1: show"
                                   name
                                   (or state 'published)
                                   (if (integerp (plist-get entry :seq))
                                       (format " seq %d" (plist-get entry :seq))
                                     "")
                                   (if-let* ((spark (beads-pulse-sparkline
                                                     (beads-pulse-store-samples key)))
                                             ((not (string-empty-p spark))))
                                       (concat " " spark)
                                     "")))))

(defun beads-pulse-store-samples (key)
  "Return the recorded samples of store KEY, oldest first.  Pure."
  (reverse (cdr (gethash (beads-pulse-city-key key) beads-pulse--samples))))

(defun beads-pulse-mode-line-string ()
  "Return the lighter text for every known store.
Published views and live streams each contribute one segment
`beads[ab ● o·i·b]'; the string is empty when there is nothing to
show.  Pure: reads only the in-memory pulse, stream and model tables,
so no `bd' or file operation runs at redisplay."
  (let* ((published (beads-pulse-cities))
         (streams (beads-pulse--stream-entries))
         (segments (append
                    (mapcar (lambda (c) (beads-pulse--segment (car c) (cdr c)))
                            published)
                    (mapcar (lambda (s) (beads-pulse--segment (car s) (cdr s)))
                            streams))))
    (if (null segments)
        ""
      (concat " beads[" (mapconcat #'identity segments " · ") "]"))))

(defvar beads-pulse-mode)

(defun beads-pulse--update ()
  "Rebuild the lighter text (when the mode is on) and redisplay mode lines."
  (when (bound-and-true-p beads-pulse-mode)
    (setq beads-pulse--string (beads-pulse-mode-line-string))
    (force-mode-line-update t)))

(defun beads-pulse--stream-updated (&rest _)
  "Refresh the lighter after a live stream changed state or delivered."
  (beads-pulse--update))

(add-hook 'beads-live-state-functions #'beads-pulse--stream-updated)
(add-hook 'beads-event-hooks #'beads-pulse--stream-updated)

;;;###autoload
(define-minor-mode beads-pulse-mode
  "Show the live bead stores' figures in the mode line (WI-LIVE-17).
One segment per store: `beads[be ● 3·5·12]', `○' when the stream is
off or reconnecting.  The figures come from what the views and the
event model already hold in memory; the lighter never runs `bd' or
touches a remote host at redisplay."
  :global t
  :group 'beads
  (let ((rest (delq 'beads-pulse--string
                    (cond ((null global-mode-string) nil)
                          ((listp global-mode-string)
                           (copy-sequence global-mode-string))
                          (t (list global-mode-string))))))
    (setq global-mode-string
          (cond (beads-pulse-mode
                 (append (if (stringp (car rest)) rest (cons "" rest))
                         '(beads-pulse--string)))
                ((equal rest '("")) nil)
                (t rest))))
  (if beads-pulse-mode
      (beads-pulse--update)
    (setq beads-pulse--string "")
    (force-mode-line-update t)))

(provide 'beads-pulse)
;;; beads-pulse.el ends here
