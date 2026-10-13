;;; beads-check.el --- Run the beads.el ERT suite under plain Emacs  -*- lexical-binding: t; -*-
;;
;; Usage (driven by `make check', which sets BEADS_CHECK_TOOLS):
;;
;;   BEADS_CHECK_TOOLS="bd=yes dolt=yes graphviz=yes" \
;;     emacs --batch -Q -L lisp -L lisp/test -l build-aux/beads-check.el
;;
;; This is the no-Eldev equivalent of `eldev test'.  It reproduces the
;; two Eldev behaviours the suite depends on:
;;
;;   F2 (be-47jo): delete stale `lisp/*.elc' and `lisp/test/*.elc'
;;   immediately before ERT.  A leftover `.elc' is preferred by `load'
;;   and silently changes `beads-defcommand' slot metadata, making the
;;   `bd show' flag-parity and transient-key tests order dependent.
;;
;;   F3: `ert-write-junit-test-report' ignores `EMACS_TEST_JUNIT_REPORT'
;;   when choosing its output filename (it uses `ert-load-file-name' or
;;   the first test's `symbol-file').  Wrap it so the report lands at the
;;   requested path, which is what the coverage job uploads.
;;
;; The suite runs in full (all tests, including `:integration') only when
;; `bd', `dolt', and `graphviz' are all present; otherwise it runs the
;; `:unit' subset and names the missing tools.  `configure --with-bd=no'
;; (and the `dolt'/`graphviz' siblings) reaches this helper as a `no'.

;;; Code:

(require 'ert)
(require 'seq)
(require 'subr-x)

(defconst beads-check--root
  (let* ((helper (or load-file-name buffer-file-name))
         (build-aux (file-name-directory (expand-file-name helper))))
    ;; <srcdir>/build-aux/beads-check.el -> <srcdir>
    (file-name-directory (directory-file-name build-aux)))
  "Repository root the runner was loaded from.")

(defun beads-check--parse-tools (spec)
  "Parse SPEC (\"bd=yes dolt=no ...\") into an alist of (NAME . BOOLEAN)."
  (let (tools)
    (dolist (word (split-string (or spec "") " " t))
      (let ((eq (string-match "=" word)))
        (when eq
          (push (cons (substring word 0 eq)
                      (string= "yes" (substring word (1+ eq))))
                tools))))
    (nreverse tools)))

(defun beads-check--drop-stale-byte-code ()
  "Delete project byte-code so ERT loads the checkout's `.el' sources (F2)."
  (dolist (pattern '("lisp/*.elc" "lisp/test/*.elc"))
    (dolist (elc (file-expand-wildcards
                  (expand-file-name pattern beads-check--root)))
      (ignore-errors (delete-file elc)))))

(defun beads-check--fix-junit-report-path (orig stats)
  "Write the JUnit report to `EMACS_TEST_JUNIT_REPORT' (F3).
ORIG is `ert-write-junit-test-report', STATS the test statistics."
  (if-let* ((report-path (getenv "EMACS_TEST_JUNIT_REPORT")))
      (progn
        (when (file-name-directory report-path)
          (make-directory (file-name-directory report-path) t))
        (let ((ert-load-file-name report-path))
          (funcall orig stats)
          ;; ERT always writes a `.xml'-suffixed file; rename to the exact
          ;; requested path when the caller asked for a different extension.
          (let ((xml-path (file-name-with-extension report-path "xml")))
            (unless (string= xml-path report-path)
              (when (file-exists-p xml-path)
                (rename-file xml-path report-path t))))))
    (funcall orig stats)))

(defun beads-check--silence-message (orig &rest args)
  "Display the message ORIG would, but return nil.

Eldev's test harness routes `message' through a nil-returning wrapper;
reproduce that so assertions about interactive suffixes returning nil
(the close/reopen `--execute' tests) behave as they did under Eldev."
  (apply orig args)
  nil)

(defun beads-check--loaded-p (file)
  "Return non-nil when FILE is already present in `load-history'.

Checking live `load-history' (rather than a snapshot) also skips files
that another test file pulled in with `require' while the loop ran."
  (let ((truename (file-truename file)))
    (seq-some (lambda (entry)
                (and (stringp (car entry))
                     (string= (file-truename (car entry)) truename)))
              load-history)))

(defun beads-check--load-tests ()
  "Load every ERT test file under `lisp/test/' in a stable order.

A test file that another file already `require'd is skipped: ERT signals
when a test name is defined twice in batch mode, and `beads-test.el' is
the shared support module as well as a test file."
  (let ((test-dir (expand-file-name "lisp/test" beads-check--root)))
    (dolist (file (sort (directory-files test-dir t "\\.el\\'") #'string<))
      (unless (beads-check--loaded-p file)
        (load file nil t)))))

(defun beads-check--run ()
  "Entry point: drop byte-code, load tests, and run the ERT batch."
  (let* ((tools (beads-check--parse-tools (getenv "BEADS_CHECK_TOOLS")))
         (missing (mapcar #'car (seq-filter (lambda (cell) (not (cdr cell))) tools)))
         (full (null missing)))
    (beads-check--drop-stale-byte-code)
    (advice-add 'ert-write-junit-test-report
                :around #'beads-check--fix-junit-report-path)
    (advice-add 'message :around #'beads-check--silence-message)
    (beads-check--load-tests)
    (if full
        (message "beads-check: bd/dolt/graphviz present; running the full suite")
      (message "beads-check: missing %s; running only :unit tests"
               (string-join missing ", ")))
    (ert-run-tests-batch-and-exit
     (if full t '(not (tag :integration))))))

(beads-check--run)

;;; beads-check.el ends here