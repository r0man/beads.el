;;; beads-store.el --- Store backend capabilities -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module answers one question about a bead store: can `bd list'
;; page with `--offset'?  Only the proxied-server Dolt route supports
;; that flag; the embedded/direct route rejects it, so the list UI has
;; to know which route a store uses before it decides how to paginate.
;;
;; Detection is cached per store and never runs on every refresh:
;;
;;   1. `bd context --json' is the preferred, stable capability surface
;;      (`dolt_mode' is `proxied-server' on the paging route).
;;   2. `.beads/metadata.json' is read only when `bd context' is
;;      unavailable or too old to answer.
;;   3. A one-row `bd list --limit 1 --offset 1 --json' probe is the last
;;      resort, for stores whose `bd' answers neither of the above.  The
;;      offset must be non-zero: the embedded route accepts `--offset 0'
;;      and only rejects a non-zero offset, so probing zero would call an
;;      embedded store server-capable.
;;
;; Failures are cached as "not capable" rather than signaled: an old or
;; missing `bd' is a fallback, not an error.  Call
;; `beads-store-forget-cache' after changing a store's backend (for
;; example after `bd migrate') to let the next refresh re-probe it.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'beads-command)
(require 'beads-util)

;;; Cache

(defconst beads-store--unprobed 'beads-store--unprobed
  "Sentinel marking a store whose backend mode has not been probed.
Distinguishes \"not probed yet\" from a probed store whose mode is nil.")

(defvar beads-store--backend-cache (make-hash-table :test #'equal)
  "Canonical store directory -> backend mode string, or nil.
A missing key means the store has not been probed.  Probing a store
that answers neither `bd context' nor `metadata.json' caches nil, so
the capability check never pays for the same failed probe twice.")

(defun beads-store--store-directory (&optional directory)
  "Return the canonical store directory for DIRECTORY, or the current one.
DIRECTORY defaults to `beads-store-directory', then the current
project root, then `default-directory'.  The result is absolute and
ends in a slash."
  (file-name-as-directory
   (expand-file-name
    (or directory
        (and (boundp 'beads-store-directory) beads-store-directory)
        (ignore-errors (beads--project-root))
        default-directory))))

(defun beads-store-forget-cache (&optional directory)
  "Forget the cached backend mode for DIRECTORY.
With DIRECTORY nil, clear the whole cache.  Call this after changing a
store's backend, for example after `bd migrate'."
  (if directory
      (remhash (beads-store--store-directory directory)
               beads-store--backend-cache)
    (clrhash beads-store--backend-cache)))

;;; Mode parsing

(defun beads-store--normalize-mode (mode)
  "Return MODE as a normalized lowercase string, or nil.
Accept a string or a symbol."
  (cond
   ((stringp mode) (downcase (string-trim mode)))
   ((symbolp mode) (downcase (symbol-name mode)))
   (t nil)))

(defun beads-store--mode-from-alist (data)
  "Return the backend mode carried by JSON alist DATA, or nil.
Looks for `dolt_mode' / `dolt-mode' first (what `bd context' and
`metadata.json' report), then a plain `mode'.  Returns a lowercase
string such as \"proxied-server\" or nil when DATA has no mode."
  (when (listp data)
    (beads-store--normalize-mode
     (or (alist-get 'dolt_mode data)
         (alist-get 'dolt-mode data)
         (alist-get 'doltMode data)
         (alist-get 'mode data)))))

;;; Probes

(defun beads-store--context-mode (store)
  "Return STORE's backend mode from `bd context --json', or nil.
The command runs with `default-directory' pinned to STORE and without
a buffer-local `beads-store-directory', so cwd discovery resolves the
same store the cache key names.  Signals nothing: an unavailable or
old `bd' simply yields nil and the caller falls back."
  (condition-case err
      (progn
        (require 'beads-command-misc)
        (let ((default-directory (file-name-as-directory store))
              (beads-store-directory nil))
          (ignore beads-store-directory)
          (beads-store--mode-from-alist
           (beads-execute 'beads-command-context :json t))))
    (error
     (when (fboundp 'beads--log)
       (beads--log 'info
                   "beads-store: bd context probe failed for %s: %s"
                   store (error-message-string err)))
     nil)))

(defun beads-store--metadata-mode (store)
  "Return STORE's backend mode from `.beads/metadata.json', or nil.
This is the fallback when `bd context --json' is unavailable.  Read
errors, TRAMP failures and malformed JSON are treated as an absent
answer; probing a view must never escape with an I/O error."
  (condition-case nil
      (let ((file (expand-file-name ".beads/metadata.json" store)))
        (when (file-exists-p file)
          (with-temp-buffer
            (insert-file-contents file)
            (beads-store--mode-from-alist
             (json-read-from-string (buffer-string))))))
    (error nil)))

(defun beads-store--probe-mode (store)
  "Probe STORE with a one-row `bd list --offset 1', returning a mode or nil.
Only used when neither `bd context' nor `metadata.json' answered.  A
successful probe means the route honours a non-zero `--offset', so the
store is reported as \"proxied-server\"; any error (including the
embedded route's \"--offset is only supported under --proxied-server\")
yields nil.  The offset is deliberately non-zero: embedded `bd' accepts
`--offset 0' and only rejects a non-zero offset, so a zero-offset probe
would misclassify an embedded store."
  (condition-case err
      (progn
        (require 'beads-command-list)
        (let ((default-directory (file-name-as-directory store))
              (beads-store-directory nil))
          (ignore beads-store-directory)
          (beads-execute 'beads-command-list :limit 1 :offset 1)
          "proxied-server"))
    (error
     (when (fboundp 'beads--log)
       (beads--log 'info
                   "beads-store: offset probe failed for %s: %s"
                   store (error-message-string err)))
     nil)))

(defun beads-store--detect-backend (store)
  "Return STORE's backend mode, or nil when it cannot be determined.
Tries `bd context --json', then `.beads/metadata.json', then a
one-row non-zero offset probe."
  (or (beads-store--context-mode store)
      (beads-store--metadata-mode store)
      (beads-store--probe-mode store)))

;;; Public capability predicate

(defun beads-store--backend-mode (&optional directory)
  "Return the cached backend mode for DIRECTORY, probing it once.
DIRECTORY is resolved by `beads-store--store-directory'."
  (let* ((store (beads-store--store-directory directory))
         (cached (gethash store beads-store--backend-cache
                          beads-store--unprobed)))
    (if (not (eq cached beads-store--unprobed))
        cached
      (let ((mode (beads-store--detect-backend store)))
        (puthash store mode beads-store--backend-cache)
        mode))))

(defun beads-store-offset-capable-p (&optional directory)
  "Return non-nil when DIRECTORY's store accepts `bd list --offset'.
DIRECTORY defaults to the current buffer's store.  The answer is
probed once per store and cached; use `beads-store-forget-cache' to
invalidate it.  Only the proxied-server route is capable; embedded
and direct stores fall back to the client-side pager."
  (equal (beads-store--backend-mode directory) "proxied-server"))

(provide 'beads-store)
;;; beads-store.el ends here
