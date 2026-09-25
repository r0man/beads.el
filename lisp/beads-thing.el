;;; beads-thing.el --- Thing-at-point motion and toggling -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; One movement scheme for every beads.el (and gascity.el) view, vui
;; and tabulated alike:
;;
;;   TAB, <tab>             `beads-thing-forward'   next thing, wraps
;;   <backtab>, S-<tab>     `beads-thing-backward'  previous thing, wraps
;;   SPC                    `beads-thing-toggle'    toggle the thing at point
;;   DEL, S-SPC             unbound (no scroll-back without SPC scroll)
;;
;; A "thing" is a maximal run of text whose `beads-thing' text property
;; holds the same (`eq') non-nil value: a section header, an object
;; row, a fold row.  Decoration (blank lines, summaries, help text)
;; simply carries no property and is skipped.  In `tabulated-list-mode'
;; buffers every row is a thing: when a position has no `beads-thing'
;; property, its `tabulated-list-id' stands in.
;;
;; The property value describes the thing.  It is either
;;
;; - a function: called with no arguments by `beads-thing-toggle';
;; - a plist (:kind KIND :toggle FN ...): FN, when present, is called
;;   with no arguments; KIND is a symbol such as `section', `fold',
;;   `row' that toggle handlers dispatch on (see `beads-thing-kind');
;;   any other keys are free for the consumer;
;; - any other non-nil value (a domain object, an id string, t): a
;;   thing you can move to; toggling it is up to the handlers.
;;
;; `beads-thing-toggle' resolves the thing at point in this order: its
;; own function or :toggle, then the abnormal hook
;; `beads-thing-toggle-functions' (buffer-local handlers, e.g. an
;; inline detail drawer or a side window for tabulated rows), then,
;; for kinds `section' and `fold', the button at point (vui section
;; headers toggle on click).  Otherwise it echoes "Nothing to toggle
;; here".
;;
;; Using it from a mode:
;;
;;   (defvar-keymap my-mode-map :parent vui-mode-map)
;;   (beads-thing-define-keys my-mode-map)
;;
;;   ;; render: stamp the header and the rows
;;   (propertize "Agents  3 running" 'beads-thing
;;               (list :kind 'section :toggle toggle-fn))
;;   (beads-thing-propertize row-string agent)
;;
;; In vui buffers, a :toggle closure that calls `vui-set-state' must
;; capture the component context (`vui-with-async-context'), since
;; `beads-thing-toggle' runs outside any render.

;;; Code:

(require 'button)
(require 'seq)

(defvar tabulated-list-mode-map)
(declare-function push-button "button")

(defvar-local beads-thing-toggle-functions nil
  "Abnormal hook run by `beads-thing-toggle' for things it cannot toggle itself.
Each function is called with the thing value (the `beads-thing'
property, or the `tabulated-list-id' of a tabulated row) with point
on the thing.  The first function returning non-nil ends the search;
that function is considered to have toggled the thing.  Consumers add
handlers buffer-locally, e.g. an inline drawer or a detail window:

  (add-hook \\='beads-thing-toggle-functions #\\='my-toggle-drawer nil t)")

(defun beads-thing-at (&optional pos)
  "Return the thing value at POS (default point), or nil.
That is the `beads-thing' text property; in a `tabulated-list-mode'
buffer a row without it falls back to its `tabulated-list-id'."
  (let ((pos (or pos (point))))
    (or (get-text-property pos 'beads-thing)
        (and (derived-mode-p 'tabulated-list-mode)
             (get-text-property pos 'tabulated-list-id)))))

(defun beads-thing-kind (thing)
  "Return the kind of THING, a symbol, or nil.
For a plist THING that is its :kind; a tabulated row id counts as
kind `row' when THING came from a `tabulated-list-id'."
  (cond ((and (consp thing) (keywordp (car thing)))
         (plist-get thing :kind))
        ((and thing (derived-mode-p 'tabulated-list-mode)
              (not (get-text-property (point) 'beads-thing))
              (eq thing (get-text-property (point) 'tabulated-list-id)))
         'row)))

(defun beads-thing-propertize (string thing)
  "Return a copy of STRING with THING as its `beads-thing' property.
See the commentary of beads-thing.el for the shape of THING."
  (propertize string 'beads-thing thing))

(defun beads-thing--start-p (pos)
  "Return non-nil when a thing starts at POS."
  (let ((thing (beads-thing-at pos)))
    (and thing
         (or (= pos (point-min))
             (not (eq thing (beads-thing-at (1- pos)))))
         (not (invisible-p pos)))))

(defun beads-thing-starts ()
  "Return the sorted start positions of all things in the buffer."
  (let ((pos (point-min))
        (end (point-max))
        starts)
    (while (< pos end)
      (when (beads-thing--start-p pos)
        (push pos starts))
      ;; Jump to the next change of either property; a thing can only
      ;; start there.
      (setq pos (min (next-single-property-change pos 'beads-thing nil end)
                     (next-single-property-change pos 'tabulated-list-id
                                                  nil end))))
    (nreverse starts)))

(defun beads-thing-beginning (&optional pos)
  "Return the start of the thing at POS (default point), or nil."
  (let* ((pos (or pos (point)))
         (thing (beads-thing-at pos)))
    (when thing
      (while (and (> pos (point-min))
                  (eq thing (beads-thing-at (1- pos))))
        (setq pos (1- pos)))
      pos)))

(defun beads-thing--move (n)
  "Move point N things forward (backward when N is negative), wrapping.
Echo \"Wrapped\" when the motion wrapped around the buffer."
  (let ((starts (beads-thing-starts)))
    (unless starts
      (user-error "No things in this buffer"))
    (let* ((len (length starts))
           (pt (point))
           ;; Forward counts from the last start at or before point;
           ;; backward from the first start at or after it, so moving
           ;; back from inside a thing lands on its own start first, as
           ;; `backward-button' does.
           (base (if (> n 0)
                     (1- (seq-count (lambda (p) (<= p pt)) starts))
                   (seq-count (lambda (p) (< p pt)) starts)))
           (target (+ base n))
           (idx (mod target len)))
      (goto-char (nth idx starts))
      (when (or (< target 0) (>= target len))
        (message "Wrapped")))))

(defun beads-thing-forward (&optional n)
  "Move to the start of the Nth next thing; wrap and echo \"Wrapped\".
A thing is a run of text with one `beads-thing' value (every row in a
`tabulated-list-mode' buffer).  Interactively N is the prefix
argument; a negative N moves backward."
  (interactive "p")
  (beads-thing--move (or n 1)))

(defun beads-thing-backward (&optional n)
  "Move to the start of the Nth previous thing; wrap and echo \"Wrapped\".
Inside a thing, the first step goes to its own start.  Interactively
N is the prefix argument; a negative N moves forward."
  (interactive "p")
  (beads-thing--move (- (or n 1))))

(defun beads-thing-toggle ()
  "Toggle the thing at point: fold a section, expand a row, open a drawer.
Tries, in order: the thing's own function (a function value or a
plist :toggle), `beads-thing-toggle-functions', and for kinds
`section' and `fold' the button at point.  Otherwise echo
\"Nothing to toggle here\"."
  (interactive)
  (let* ((thing (beads-thing-at))
         (fn (cond ((functionp thing) thing)
                   ((and (consp thing) (keywordp (car thing)))
                    (plist-get thing :toggle)))))
    (cond ((null thing) (message "Nothing to toggle here"))
          (fn (funcall fn))
          ((run-hook-with-args-until-success
            'beads-thing-toggle-functions thing))
          ((and (memq (beads-thing-kind thing) '(section fold))
                (button-at (point)))
           (push-button))
          (t (message "Nothing to toggle here")))))

(defun beads-thing-define-keys (map)
  "Install the thing-motion keys in keymap MAP and return MAP.
Binds TAB and <tab> to `beads-thing-forward', <backtab>, S-TAB and
S-<tab> to `beads-thing-backward', SPC to `beads-thing-toggle', and
leaves DEL and S-SPC undefined (they would scroll back while SPC no
longer scrolls forward).  Both TAB and <tab> are bound because vui
binds <tab> in a parent map, which would win over TAB in GUI frames.

The navigation commands of vui, widget.el and button.el are remapped
too: vui buttons carry their own keymap (a text property, which takes
precedence over MAP) binding TAB to `vui-forward', and a remap in the
major mode map still applies to it."
  (dolist (key '("TAB" "<tab>"))
    (keymap-set map key #'beads-thing-forward))
  (dolist (key '("<backtab>" "S-TAB" "S-<tab>"))
    (keymap-set map key #'beads-thing-backward))
  (keymap-set map "SPC" #'beads-thing-toggle)
  (dolist (key '("DEL" "S-SPC"))
    (keymap-set map key #'undefined))
  (dolist (cmd '(vui-forward widget-forward forward-button))
    (define-key map (vector 'remap cmd) #'beads-thing-forward))
  (dolist (cmd '(vui-backward widget-backward backward-button))
    (define-key map (vector 'remap cmd) #'beads-thing-backward))
  map)

(provide 'beads-thing)
;;; beads-thing.el ends here
