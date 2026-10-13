;;; beads-coverage.el --- Run the beads.el ERT suite under undercover  -*- lexical-binding: t; -*-
;;
;; Usage (driven by `scripts/coverage', which `make coverage' and the
;; Coverage workflow call):
;;
;;   BEADS_CHECK_NO_RUN=1 EMACS_TEST_JUNIT_REPORT=coverage/junit.xml \
;;     emacs --batch -Q -L lisp -L lisp/test -l build-aux/beads-coverage.el
;;
;; This is the no-Eldev equivalent of the old
;; `eldev -s -dtT test -U coverage/codecov.json'.  It reproduces the
;; three pieces the Eldev file configured for coverage:
;;
;;   1. `beads-undercover-fileset' -> undercover is given "lisp/*.el"
;;      with (:exclude "lisp/test/*.el").
;;   2. codecov format, not sent automatically -> (:report-format
;;      'codecov) (:send-report nil); the report path is $COVERAGE_REPORT
;;      (default coverage/codecov.json).
;;   3. the transient edebug-spec fix -> undercover instruments via
;;      edebug, and transient's spec for `transient-define-prefix' only
;;      accepts vectors, while beads.el includes reusable groups by
;;      symbol.  The spec is widened before instrumentation runs.
;;
;; The suite is the `:unit' subset: the `:integration' tests exercise the
;; bd/dolt CLI, not Emacs Lisp, so they add no coverage and are skipped.
;; `:performance' tests are skipped too: they assert wall-clock bounds
;; (e.g. 1000 validations under 15s) that undercover's edebug
;; instrumentation makes flaky, and they cover no behavior the other
;; `:unit' tests do not.  The stale-byte-code drop and the JUnit-path
;; advice come from build-aux/beads-check.el, which is loaded without
;; running the suite.

;;; Code:

;; Force undercover on before it is loaded: `undercover-force-coverage'
;; is initialised from this environment variable at load time, and it is
;; what lets the codecov report be written on a machine where undercover
;; cannot detect a specific CI service (a local run, or CI=true without
;; GITHUB_ACTIONS).
(setenv "UNDERCOVER_FORCE" "1")

(require 'undercover)
(setq undercover-force-coverage t)

;; Widen transient's edebug specs so undercover can instrument the
;; generated prefixes.  `transient' is required first so the spec is
;; installed over the real definition, not a placeholder.
(require 'transient nil t)

(defun beads-coverage--fix-transient-edebug-specs ()
  "Override transient edebug specs to allow symbol group references.
Undercover instruments with edebug; `transient-define-prefix''s stock
spec only accepts vectors, but beads.el includes groups defined by
`transient-define-group' as symbols."
  (def-edebug-spec transient-define-prefix
    (&define name lambda-list
             [&optional lambda-doc]
             [&rest keywordp sexp]
             [&rest [&or symbolp vectorp]]
             [&optional ("interactive" interactive) def-body]))
  (def-edebug-spec transient-define-group
    (&define name [&rest [&or symbolp vectorp]])))

(when (featurep 'transient)
  (beads-coverage--fix-transient-edebug-specs))

;; Helpers from `make check': stale-byte-code drop (F2), JUnit-path
;; advice (F3), the nil-returning `message' wrapper, and the test loader.
;; BEADS_CHECK_NO_RUN is set by scripts/coverage so loading it does not
;; immediately run the suite.
(load (expand-file-name "beads-check.el" (file-name-directory
                                          (or load-file-name buffer-file-name)))
      nil t)

(defvar beads-coverage--report
  (or (getenv "COVERAGE_REPORT") "coverage/codecov.json")
  "Path undercover writes the codecov JSON report to.")

(undercover "lisp/*.el"
            (:exclude "lisp/test/*.el")
            (:report-format 'codecov)
            (:report-file beads-coverage--report)
            (:send-report nil))

(beads-check-run-tests '(not (or (tag :integration) (tag :performance))))

;;; beads-coverage.el ends here
