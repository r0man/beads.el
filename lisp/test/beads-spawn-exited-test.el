;;; beads-spawn-exited-test.el --- A bd process that exits at once -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Commentary:

;; `beads-command--spawn-async' used to report a spawn failure when the
;; process had already exited by the time it checked `process-live-p':
;; any fast bd call (an early error, a tiny output) could lose its
;; result.  An exited process did spawn; its sentinel delivers the
;; result.  The test makes the race deterministic: the spawn returns
;; only after the child has exited (its status is recorded by the
;; SIGCHLD handler; the sentinel has not run yet).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'beads-command)
(require 'beads-command-list)

(defmacro beads-spawn-exited-test--dead-on-return (&rest body)
  "Run BODY with every `make-process' returning an already-exited process."
  (declare (indent 0))
  `(let ((real (symbol-function 'make-process)))
     (cl-letf (((symbol-function 'make-process)
                (lambda (&rest args)
                  (let ((p (apply real args))
                        (deadline (+ (float-time) 5)))
                    ;; Busy-wait: no `accept-process-output', so the
                    ;; sentinel cannot run before the caller checks.
                    (while (and (process-live-p p) (< (float-time) deadline))
                      (ignore))
                    p))))
       ,@body)))

(defun beads-spawn-exited-test--wait (pred)
  "Pump the event loop until PRED or 5 s."
  (let ((deadline (+ (float-time) 5)))
    (while (and (not (funcall pred)) (< (float-time) deadline))
      (accept-process-output nil 0.02))
    (funcall pred)))

(ert-deftest beads-spawn-exited-test-success-delivered ()
  "A process that exits before the liveness check still delivers its
parsed result, and the spawn returns that process."
  :tags '(:unit)
  (let (result err proc)
    (cl-letf (((symbol-function 'beads-command-line)
               (lambda (_cmd) '("printf" "ok")))
              ((symbol-function 'beads-command-parse)
               (lambda (_cmd out) (list :parsed out))))
      (beads-spawn-exited-test--dead-on-return
        (setq proc (beads-command-execute-async
                    (beads-command-list) (lambda (r) (setq result r))
                    (lambda (e) (setq err e)))))
      (should (processp proc))
      (should-not (process-live-p proc))
      (should (beads-spawn-exited-test--wait (lambda () (or result err))))
      (should-not err)
      (should (equal result '(:parsed "ok"))))))

(ert-deftest beads-spawn-exited-test-failure-delivered ()
  "A fast failing process reports its exit code, not a spawn failure."
  :tags '(:unit)
  (let (result err)
    (cl-letf (((symbol-function 'beads-command-line)
               (lambda (_cmd) '("sh" "-c" "echo boom >&2; exit 3"))))
      (beads-spawn-exited-test--dead-on-return
        (beads-command-execute-async
         (beads-command-list) (lambda (r) (setq result r))
         (lambda (e) (setq err e))))
      (should (beads-spawn-exited-test--wait (lambda () (or result err))))
      (should-not result)
      (should (eql (plist-get (cdr err) :exit-code) 3))
      (should-not (plist-get (cdr err) :spawn-error)))))

(provide 'beads-spawn-exited-test)
;;; beads-spawn-exited-test.el ends here
