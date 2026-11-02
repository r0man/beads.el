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
;;   * the list view's open/refresh path, filters, and marks
;;   * dashboard open/refresh, per-section loaders, and M-0..M-4 depth
;;   * resident-set growth from parsing and rendering the whole store
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
;;   BEADS_BENCH_PROXIED  1 when the fixture uses --proxied-server
;;   BEADS_BENCH_PROFILE  when non-nil, also write a CPU profile report
;;   BEADS_BENCH_PROFILE_OUT  path for that report (default: store dir)

;;; Code:

(require 'cl-lib)
(require 'beads)
(require 'beads-command-list)
(require 'beads-dashboard)
(require 'beads-dashboard-sections)
(require 'beads-spec)
(require 'beads-types)
(require 'profiler)

(defvar beads-bench--dir (getenv "BEADS_BENCH_DIR")
  "Store directory under test.")

(defvar beads-bench--n (string-to-number (or (getenv "BEADS_BENCH_N") "0"))
  "Number of issues seeded, for the report header.")

(defvar beads-bench--repeat
  (max 1 (string-to-number (or (getenv "BEADS_BENCH_REPEAT") "3")))
  "Repetitions per measurement; the best time is reported.")

(defvar beads-bench--bd-reads 0
  "Number of `bd' reads issued since the last reset.")

(defun beads-bench--rss-kb ()
  "Return this process's resident set size in KiB, or nil if unavailable."
  (when (file-readable-p "/proc/self/status")
    (with-temp-buffer
      (insert-file-contents "/proc/self/status")
      (goto-char (point-min))
      (when (re-search-forward "^VmRSS:\\s-*\\([0-9]+\\) kB" nil t)
        (string-to-number (match-string 1))))))

(defun beads-bench--mb (kb)
  "Format KB as megabytes, or `n/a'."
  (if kb (format "%.1f MB" (/ kb 1024.0)) "n/a"))

(defun beads-bench--list-buffer ()
  "Return (creating if needed) the scratch list buffer used by the benchmark."
  (let ((buf (get-buffer-create "*beads-bench-list*")))
    (with-current-buffer buf
      ;; These must be set before `beads-list-mode' initializes the
      ;; column format: it reads `beads-list-group-by-status' to decide
      ;; whether the columns are sortable, and a stale grouped-mode
      ;; initialization leaves a sort key the flat printer cannot call.
      (setq-local default-directory
                  (file-name-as-directory beads-bench--dir))
      (setq-local beads-store-directory beads-bench--dir)
      (setq-local beads-list-group-by-status nil)
      ;; Marks and row iteration are per displayed page.  Use one page so
      ;; the mark-all measurement covers every fetched row.
      (setq-local beads-pager--page-size 1000000)
      (unless (derived-mode-p 'beads-list-mode)
        (beads-list-mode)))
    buf))

(defun beads-bench--open-list (spec)
  "Open/refresh the benchmark list buffer with SPEC and return its line count."
  (let ((buf (beads-bench--list-buffer)))
    (with-current-buffer buf
      (beads-list--refresh spec))
    (with-current-buffer buf
      (count-lines (point-min) (point-max)))))

(defun beads-bench--open-list-all ()
  "Open the list with a full fetch (spec limit 0) and return row count."
  (beads-bench--open-list
   (beads-issue-spec :order 'newest :limit 0)))

(defun beads-bench--dashboard-buffer ()
  "Return the live benchmark dashboard buffer, if any."
  (cl-find-if (lambda (buf)
                (with-current-buffer buf
                  (derived-mode-p 'beads-dashboard-mode)))
              (buffer-list)))

(defun beads-bench--dashboard-settled-p ()
  "Return non-nil when the dashboard has no pending section or async read.
Stale-while-revalidate keeps the previous rows on screen during a soft
refresh, so the absence of `Loading…' alone is not enough; also require
the global async queue to be empty."
  (when-let* ((buf (beads-bench--dashboard-buffer)))
    (and (with-current-buffer buf
           (not (string-match-p "Loading…" (buffer-string))))
         (zerop beads-command--in-flight)
         (null beads-command--queue))))

(defun beads-bench--pump-until (predicate timeout)
  "Pump the event loop until PREDICATE or TIMEOUT seconds pass.
Returns PREDICATE's final value."
  (let ((deadline (+ (float-time) timeout)))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.02))
    (funcall predicate)))

(defun beads-bench--open-dashboard ()
  "Open the benchmark dashboard and wait for every section to settle.
Returns the number of buffer lines."
  (beads-dashboard :directory beads-bench--dir)
  (unless (beads-bench--pump-until #'beads-bench--dashboard-settled-p 120)
    (error "dashboard sections did not settle within 120s"))
  (accept-process-output nil 0.05)
  (with-current-buffer (beads-bench--dashboard-buffer)
    (count-lines (point-min) (point-max))))

(defun beads-bench--refresh-dashboard ()
  "Soft-refresh the benchmark dashboard and wait for it to settle."
  (with-current-buffer (beads-bench--dashboard-buffer)
    (beads-dashboard-refresh))
  (unless (beads-bench--pump-until #'beads-bench--dashboard-settled-p 120)
    (error "dashboard refresh did not settle within 120s"))
  (accept-process-output nil 0.05))

(defun beads-bench--depth (n)
  "Apply dashboard depth N (0 = all) and return the resulting line count."
  (with-current-buffer (beads-bench--dashboard-buffer)
    (beads-dashboard--set-depth (if (= n 0) most-positive-fixnum n))
    (count-lines (point-min) (point-max))))

(defun beads-bench--loaders ()
  "Return (NAME . LOADER) pairs for every built-in dashboard section."
  (list (cons "stats"   (beads-dashboard--stats-loader nil))
        (cons "closed"  (beads-dashboard--closed-loader nil))
        (cons "in-flight" (beads-dashboard--in-flight-loader nil))
        (cons "ready"   (beads-dashboard--ready-loader nil))
        (cons "blocked" (beads-dashboard--blocked-loader nil))
        (cons "epics"   (beads-dashboard--epic-loader nil))
        (cons "stale"   (beads-dashboard--stale-loader nil))
        (cons "orphans" (beads-dashboard--orphans-loader nil))))

(defun beads-bench--profile-run (path)
  "Profile list-all and dashboard open/refresh, writing a report to PATH."
  (profiler-start 'cpu)
  (beads-bench--open-list-all)
  (beads-bench--open-dashboard)
  (beads-bench--refresh-dashboard)
  (profiler-stop)
  (profiler-report)
  (let ((report (cl-find-if
                 (lambda (buf)
                   (string-match-p "Profiler-Report" (buffer-name buf)))
                 (buffer-list))))
    (if report
        (with-current-buffer report
          (goto-char (point-min))
          ;; With a prefix argument this expands the whole call tree, so
          ;; the saved report names the actual hotspots instead of a
          ;; single collapsed root row.
          (profiler-report-expand-entry t)
          (write-region (point-min) (point-max) path nil 'silent)
          (message "CPU profile written to %s" path))
      (message "warning: no profiler report buffer was produced"))))

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

  ;; 6. List view open/refresh, filters, marks.
  (message "")
  (message "list view (the interactive `l'/filter path):")
  (beads-bench--measure "  open default spec (open, limit 50)"
    (lambda () (beads-bench--open-list beads-list-default-spec) 0))
  (beads-bench--measure "  refresh default spec"
    (lambda () (with-current-buffer (beads-bench--list-buffer)
                 (beads-list--refresh beads-list-default-spec))
             0))
  (beads-bench--measure "  open all (limit 0)"
    (lambda () (beads-bench--open-list-all) 0))
  (beads-bench--measure "  refresh all (limit 0)"
    (lambda () (with-current-buffer (beads-bench--list-buffer)
                 (beads-list--refresh
                  (beads-issue-spec :order 'newest :limit 0)))
             0))
  (dolist (case (list (cons "status=open" (beads-issue-spec :status "open" :limit 0))
                      (cons "type=bug" (beads-issue-spec :type "bug" :limit 0))
                      (cons "priority=2" (beads-issue-spec :priority 2 :limit 0))
                      (cons "label=performance" (beads-issue-spec :label "performance" :limit 0))
                      (cons "ready-only" (beads-issue-spec :ready-only t :limit 0))))
    (let ((label (car case))
          (spec (cdr case)))
      (beads-bench--measure (format "  filter %s" label)
        (lambda () (with-current-buffer (beads-bench--list-buffer)
                     (beads-list--refresh spec))
                   0))))
  (beads-bench--open-list-all)
  (beads-bench--measure "  mark all rows"
    (lambda () (with-current-buffer (beads-bench--list-buffer)
                 (beads-list-mark-all)
                 (length beads-list--marked-issues))))
  (beads-bench--measure "  unmark all rows"
    (lambda () (with-current-buffer (beads-bench--list-buffer)
                 (beads-list-unmark-all))
             0))

  (garbage-collect)
  (let ((before (beads-bench--rss-kb)))
    (beads-bench--open-list-all)
    (let ((after (beads-bench--rss-kb)))
      (message "  RSS after list-all open: %s (delta %s)"
               (beads-bench--mb after)
               (if (and before after)
                   (format "%+.1f MB" (/ (- after before) 1024.0))
                 "n/a"))))

  ;; 7. Dashboard open, refresh, per-section fetch counts, and depth keys.
  (message "")
  (message "dashboard (vui board):")
  (beads-bench--measure "  open all sections"
    (lambda () (beads-bench--open-dashboard) 0))
  (beads-bench--measure "  refresh all sections"
    (lambda () (beads-bench--refresh-dashboard) 0))
  (dolist (n '(1 2 3 4 0))
    (beads-bench--measure (format "  depth M-%d re-render" n)
      (lambda () (beads-bench--depth n) 0)))

  (message "")
  (message "per-section loader fetch (one async read each):")
  (dolist (pair (beads-bench--loaders))
    (beads-bench--measure (format "  %s" (car pair))
      (lambda ()
        (beads-bench--run-loaders (list (cdr pair)))
        0)))

  (garbage-collect)
  (let ((before (beads-bench--rss-kb)))
    (beads-bench--open-dashboard)
    (let ((after (beads-bench--rss-kb)))
      (message "  RSS after dashboard open: %s (delta %s)"
               (beads-bench--mb after)
               (if (and before after)
                   (format "%+.1f MB" (/ (- after before) 1024.0))
                 "n/a"))))

  ;; 8. Dashboard open fan-out: every built-in section loader runs
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

  (when-let* ((profile (getenv "BEADS_BENCH_PROFILE"))
              ((not (string-empty-p profile))))
    (message "")
    (beads-bench--profile-run
     (or (getenv "BEADS_BENCH_PROFILE_OUT")
         (expand-file-name "large-store-profile.txt" beads-bench--dir))))

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
