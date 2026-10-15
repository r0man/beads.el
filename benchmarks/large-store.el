;;; large-store.el --- Time beads.el hot paths on a large store -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Timing harness for the `large-store' benchmark.  It is driven by
;; `benchmarks/large-store.sh', which builds a throwaway store seeded
;; with N issues and then runs this file under `emacs --batch'.
;;
;; It measures the pure-Emacs hot paths that make a large store feel
;; slow, independent of the terminal/vui layer so it is reproducible in
;; CI and on a headless box:
;;
;;   * `bd list' end to end (subprocess + JSON parse + object coercion)
;;   * raw `json-read-from-string' baseline (parse without coercion)
;;   * `beads-command-parse' coercion baseline (parse into objects)
;;   * issue-list population (flat and status-grouped)
;;   * dashboard section renderers (ready / blocked)
;;
;; Every measurement is repeated and the best observed time is
;; reported, so a cold first run (font-lock / byte-code warm-up) does
;; not distort the result.  The number of `bd' reads issued during the
;; measurement is counted through `beads-command-execute' and
;; `beads-command-execute-async'.
;;
;; Environment (set by the shell driver):
;;   BEADS_BENCH_DIR      absolute path of the seeded store (required)
;;   BEADS_BENCH_N        issue count, only used in the report header
;;   BEADS_BENCH_REPEAT   repetitions per measurement (default 3)
;;   BEADS_BENCH_LABEL    optional label printed in the report header

;;; Code:

(require 'cl-lib)
(require 'beads)
(require 'beads-command-list)
(require 'beads-dashboard)
(require 'beads-dashboard-sections)
(require 'beads-types)

(defvar beads-bench--dir (getenv "BEADS_BENCH_DIR")
  "Store directory under test.")

(defvar beads-bench--n (string-to-number (or (getenv "BEADS_BENCH_N") "0"))
  "Number of issues seeded, for the report header.")

(defvar beads-bench--repeat
  (max 1 (string-to-number (or (getenv "BEADS_BENCH_REPEAT") "3")))
  "Repetitions per measurement; the best time is reported.")

(defvar beads-bench--bd-reads 0
  "Number of `bd' reads issued since the last reset.")

(defun beads-bench--count-read (&rest _)
  "Advice: count one `bd' read."
  (cl-incf beads-bench--bd-reads))

(advice-add 'beads-command-execute :before #'beads-bench--count-read)
(advice-add 'beads-command-execute-async :before #'beads-bench--count-read)

(defun beads-bench--measure (label thunk)
  "Run THUNK `beads-bench--repeat' times and report the best time.
Returns a cons (BEST . READS) where READS is the `bd'-read count of the
fastest run.  LABEL names the measured operation."
  (let ((best most-positive-fixnum)
        (best-reads 0)
        (results nil))
    (dotimes (_ beads-bench--repeat)
      (setq beads-bench--bd-reads 0)
      (let* ((t0 (float-time))
             (val (funcall thunk))
             (elapsed (- (float-time) t0)))
        (ignore val)
        (push elapsed results)
        (when (< elapsed best)
          (setq best elapsed
                best-reads beads-bench--bd-reads))))
    (message "%-48s %8.3f s   %4d bd-read%s"
             label best best-reads (if (= best-reads 1) "" "s"))
    (cons best best-reads)))

(defun beads-bench--main ()
  "Run the benchmark and print the report."
  (unless (and beads-bench--dir (file-directory-p beads-bench--dir))
    (error "BEADS_BENCH_DIR is not a directory: %S" beads-bench--dir))
  (setq default-directory (file-name-as-directory beads-bench--dir))
  (when (boundp 'beads-store-directory)
    (setq beads-store-directory beads-bench--dir))
  ;; Keep the store's live journal out of the way: the benchmark drives
  ;; the read paths directly and must not attach a stream.
  (setq beads-dashboard-live-auto-attach nil)

  (message "beads.el large-store benchmark")
  (message "  label   : %s" (or (getenv "BEADS_BENCH_LABEL") "(none)"))
  (message "  store   : %s" beads-bench--dir)
  (message "  issues  : %s" beads-bench--n)
  (message "  repeat  : %d (best of)" beads-bench--repeat)
  (message "  emacs   : %s" emacs-version)
  (message "")
  (message "%-48s %8s   %s" "measurement" "best" "cost")
  (message "%s" (make-string 72 ?-))

  ;; 1. Full `bd list' round trip (subprocess + parse + coerce).
  (beads-bench--measure "bd list --json (process+parse+coerce)"
    (lambda ()
      (length (beads-command-execute
               (beads-command-list :json t :limit 0)))))

  (let* ((raw (beads-bench--capture-list-json))
         (issues (beads-command-parse
                  (beads-command-list :json t :limit 0) raw)))
    (message "")
    (message "parsed %d issues; pure-Emacs breakdown:" (length issues))

    ;; 2. Raw JSON parse baseline.
    (beads-bench--measure "  json-read-from-string (baseline)"
      (lambda () (length (json-read-from-string raw))))

    ;; 3. String -> objects coercion.
    (beads-bench--measure "  beads-command-parse (coercion)"
      (lambda ()
        (length (beads-command-parse
                 (beads-command-list :json t :limit 0) raw))))

    ;; 4. Issue-list population.
    (beads-bench--measure "  beads-list--populate flat"
      (lambda ()
        (with-temp-buffer
          (let ((beads-list-group-by-status nil))
            (beads-list-mode)
            (beads-list--populate-buffer issues 'list))
          0)))
    (beads-bench--measure "  beads-list--populate grouped"
      (lambda ()
        (with-temp-buffer
          (let ((beads-list-group-by-status t))
            (beads-list-mode)
            (beads-list--populate-buffer issues 'list))
          0)))

    ;; 5. Dashboard section renderers.
    (beads-bench--measure "  dashboard render-ready"
      (lambda () (beads-dashboard-render-ready issues 'ready nil) 0))
    (beads-bench--measure "  dashboard render-blocked"
      (lambda () (beads-dashboard-render-blocked issues 'blocked nil) 0)))

  ;; 6. Dashboard open fan-out: every built-in section loader runs
  ;; concurrently through `beads-command-execute-async', exactly as
  ;; `vui-use-async' drives them on mount.  This reports the wall clock
  ;; and the number of `bd' reads a board pays on open.
  (beads-bench--measure "dashboard open fan-out (8 sections, async)"
    (lambda ()
      (beads-bench--run-loaders
       (list (beads-dashboard--stats-loader nil)
             (beads-dashboard--closed-loader nil)
             (beads-dashboard--in-flight-loader nil)
             (beads-dashboard--ready-loader nil)
             (beads-dashboard--blocked-loader nil)
             (beads-dashboard--epic-loader nil)
             (beads-dashboard--stale-loader nil)
             (beads-dashboard--orphans-loader nil)))
      0))

  (message "")
  (message "note: bd-read counts exclude coalesced async requests and")
  (message "      the `bd list' subprocess cost is included in row 1 only."))

(defun beads-bench--capture-list-json ()
  "Return the raw JSON for the full `bd list' output.
Runs the CLI once through `process-file' so the string can be re-parsed
by the pure-Emacs measurements without re-spawning the subprocess."
  (let ((cmd (let ((beads-command--ssh-pipe nil))
               (beads-command-line (beads-command-list :json t :limit 0)))))
    (with-temp-buffer
      (apply #'process-file (car cmd) nil (list t nil) nil (cdr cmd))
      (buffer-string))))

(defun beads-bench--run-loaders (thunks)
  "Run THUNKS (\\(resolve reject) async loaders) concurrently to completion.
Pumps the event loop with `accept-process-output' until every resolve or
reject has fired, mirroring how `vui-use-async' drives the dashboard on
mount.  Signals when a loader rejects or the deadline passes."
  (let ((pending (length thunks))
        errors)
    (dolist (thunk thunks)
      (funcall thunk
               (lambda (_value) (cl-decf pending))
               (lambda (err) (cl-decf pending) (push err errors))))
    (let ((deadline (+ (float-time) 60)))
      (while (and (> pending 0) (< (float-time) deadline))
        (accept-process-output nil 0.05)))
    (when errors
      (error "dashboard loader error: %S" (car errors)))
    (when (> pending 0)
      (error "dashboard loaders timed out: %d still pending" pending))))

(beads-bench--main)

;;; large-store.el ends here
