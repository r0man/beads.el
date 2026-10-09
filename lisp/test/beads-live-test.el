;;; beads-live-test.el --- Tests for the live event stream -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is part of beads.el.

;;; Commentary:

;; Offline (`:unit', no `bd', no TRAMP connection) coverage for the
;; `beads-live' stream supervisor:
;;
;; - WI-LIVE-03: canonical store root and `(root, journal kind)' stream
;;   key; the cached events-journal capability probe.
;; - WI-LIVE-04: `bd events tail --follow' argv (no `--json'), local and
;;   local-ssh-pipe transport, and the `beads-live--spawn' seam.
;; - WI-LIVE-05: JSONL chunking (partial/non-JSON), the sparse-merge
;;   rule, the bounded ring/log, subscriber delivery, and the debounced
;;   batch.
;; - WI-LIVE-06: identity-keyed checkpoints, start/resume, and the
;;   pruned-`--since' / reconcile re-baseline.
;; - WI-LIVE-07: exit classification and the `(2 5 15 60)' backoff.
;; - WI-LIVE-08: poll-vs-stream selection and the poll delivery path.
;; - WI-LIVE-09: attach/detach refcount and last-detach stop, raw
;;   subscribe/unsubscribe, the public `beads-event-hooks' per-record
;;   hook (advisory A1), status/header rendering, the public
;;   `beads-live-invalidate' seam, and the toggle/reconnect/stop-all
;;   controls.
;;
;; Every seam is a function variable or a buffer-local, so the suite is
;; offline.  Named functions (not inline closures) are used for the
;; dynamically bound seams because ERT evaluates test bodies in dynamic
;; scope and a named function cannot capture lexical test state.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'tramp)
(require 'seq)
(require 'beads-custom)
(require 'beads-remote)
(require 'beads-live)

;; `beads-store-directory' is a buffer-local `defvar-local' in
;; `beads-meta.el'; declared so view buffers can pin a store.
(defvar beads-store-directory)
;; The seams declared in `beads-live.el' are special.
(defvar beads-live-state-directory)
(defvar beads-live-baseline-function)
(defvar beads-live-head-function)
(defvar beads-live-spawn-function)
(defvar beads-live-branch-function)
(defvar beads-live-replica-function)
(defvar beads-live-capability-function)
(defvar beads-live-invalidate-functions)
(defvar beads-event-hooks)
(defvar beads-live-state-functions)

;; Keep any stray debounce flush out of the user's real state directory.
(setq beads-live-state-directory (make-temp-file "beads-live-test-state-" t))

;;; Shared helpers

(defmacro beads-live-test--with-stream (binding &rest body)
  "Bind BINDING to a stream for BODY, then kill its stderr buffer.
BINDING is (VAR . PLIST); PLIST is passed to
`beads-live--stream-create'."
  (declare (indent 1) (debug t))
  `(let ((,(car binding)
          (apply #'beads-live--stream-create (list ,@(cdr binding)))))
     (unwind-protect
         (progn ,@body)
       (when-let* ((buf (beads-live--stream-stderr-buffer ,(car binding))))
         (when (buffer-live-p buf)
           (let ((kill-buffer-query-functions nil))
             (kill-buffer buf)))))))

(defmacro beads-live-test--with-fresh-live (&rest body)
  "Run BODY with fresh model and raw-issue tables.
BODY is responsible for its own stream."
  (declare (indent 0) (debug t))
  `(let ((beads-live--models (make-hash-table :test 'equal))
         (beads-live--raw-issues (make-hash-table :test #'eq))
         (beads-live-invalidate-functions nil)
         (beads-event-hooks nil))
     ,@body))

(defun beads-live-test--stream ()
  "Return a stream for a temp store named `store'."
  (beads-live--stream-create :root "/tmp/beads-live-test-store" :name "store"))

(defun beads-live-test--json (seq op issue-json &optional actor)
  "Return a journal JSONL line with SEQ, OP, ISSUE-JSON and ACTOR."
  (format "{\"seq\":%d,\"op\":\"%s\",\"issue_id\":\"be-1\"%s,\"issue\":%s}"
          seq op
          (if actor (format ",\"actor\":\"%s\"" actor) "")
          issue-json))

(defun beads-live-test--parse-one (seq op issue-json &optional actor)
  "Parse one journal line and return the `beads-event-record'."
  (car (car (beads-live-parse-chunk
             "" (concat (beads-live-test--json seq op issue-json actor)
                        "\n")))))

(defmacro beads-live-test--with-state-dir (&rest body)
  "Run BODY with a fresh `beads-live-state-directory', then delete it."
  (declare (indent 0) (debug t))
  `(let* ((dir (make-temp-file "beads-live-test-" t))
          (beads-live-state-directory dir))
     (unwind-protect
         (progn ,@body)
       (ignore-errors (delete-directory dir t)))))

(defmacro beads-live-test--with-store (binding &rest body)
  "Bind BINDING to a stream over a fresh temp store for BODY."
  (declare (indent 1) (debug t))
  `(let* ((root (make-temp-file "beads-live-store-" t))
          (,(car binding)
           (apply #'beads-live--stream-create :root root :name "store"
                  (list ,@(cdr binding)))))
     (unwind-protect
         (progn ,@body)
       (ignore-errors (delete-directory root t))
       (ignore-errors (beads-live--forget-stream root)))))

(defmacro beads-live-test--capture (calls &rest body)
  "Run BODY with the sibling collaborators stubbed.
CALLS is bound to a plist recording the last `:run-at-time', `:start',
`:rebaseline' and `:start-poll' invocation.  `run-at-time' returns the
symbol `fake-timer' and `cancel-timer' is a no-op, so no real timer is
scheduled."
  (declare (indent 1) (debug t))
  `(let ((,calls nil))
     (cl-letf (((symbol-function 'run-at-time)
                (lambda (delay repeat fn &rest args)
                  (setq ,calls (plist-put ,calls :run-at-time
                                          (list delay repeat fn args)))
                  'fake-timer))
               ((symbol-function 'cancel-timer) #'ignore)
               ((symbol-function 'beads-live--start)
                (lambda (stream &optional refresh)
                  (setq ,calls (plist-put ,calls :start (list stream refresh)))))
               ((symbol-function 'beads-live--rebaseline)
                (lambda (stream &optional reason)
                  (setq ,calls (plist-put ,calls :rebaseline (list stream reason)))))
               ((symbol-function 'beads-live--start-poll)
                (lambda (stream)
                  (setq ,calls (plist-put ,calls :start-poll (list stream))))))
       ,@body)))

(defun beads-live-test--stage-stderr (stream lines)
  "Write LINES to STREAM's stderr buffer and return the pre-write mark."
  (let ((buf (beads-live--stderr-buffer stream))
        mark)
    (with-current-buffer buf
      (setq mark (point-max))
      (insert (mapconcat #'identity lines "\n") "\n"))
    mark))

(defmacro beads-live-test--with-registry (&rest body)
  "Run BODY with fresh stream/model tables and a temp state directory."
  (declare (indent 0) (debug t))
  `(let ((beads-live--streams (make-hash-table :test 'equal))
         (beads-live--models (make-hash-table :test 'equal))
         (beads-live--raw-issues (make-hash-table :test #'eq))
         (beads-live-state-directory (make-temp-file "beads-live-test-state-" t))
         (beads-live-invalidate-functions nil)
         (beads-event-hooks nil)
         (beads-live-state-functions nil))
     ,@body))

(defun beads-live-test--registry-stream (&optional root)
  "Register and return (ROOT . STREAM) with a live state for tests."
  (let* ((root (or root (make-temp-file "beads-live-store-" t)))
         (stream (beads-live--stream-for root)))
    (setf (beads-live--stream-state stream) 'live
          (beads-live--stream-seq stream) 1047
          (beads-live--stream-activity stream) (list (float-time)))
    (cons root stream)))

(defun beads-live-test--view-buffer (root)
  "Return a fresh buffer pinned to store ROOT."
  (let ((buf (generate-new-buffer " *beads-live-view*")))
    (with-current-buffer buf
      (setq-local default-directory (file-name-as-directory root))
      (setq-local beads-store-directory (file-name-as-directory root)))
    buf))

(defun beads-live-test--kill (buf)
  "Kill BUF without prompting."
  (let ((kill-buffer-query-functions nil))
    (when (buffer-live-p buf) (kill-buffer buf))))

;;; WI-LIVE-03 — canonical store root

(ert-deftest beads-live-test-canonical-root-local-absolute ()
  "A local directory is expanded and given a trailing slash."
  :tags '(:unit)
  (should (equal (beads-live-canonical-root "/p") "/p/"))
  (should (equal (beads-live-canonical-root "/p/") "/p/"))
  (should (equal (beads-live-canonical-root ".")
                 (file-name-as-directory
                  (expand-file-name default-directory)))))

(ert-deftest beads-live-test-canonical-root-nil-and-empty ()
  "Nil and empty directories canonicalize to nil, not to a directory."
  :tags '(:unit)
  (should-not (beads-live-canonical-root nil))
  (should-not (beads-live-canonical-root ""))
  (should-not (beads-live-canonical-root 42)))

(ert-deftest beads-live-test-canonical-root-remote-idempotent ()
  "Every spelling of one remote store canonicalizes to one value."
  :tags '(:unit)
  (let ((canonical (beads-live-canonical-root
                    "/ssh:user@example.com:/home/user/p")))
    (should (equal canonical "/ssh:user@example.com:/home/user/p/"))
    (should (equal (beads-live-canonical-root
                    "/ssh:user@example.com:/home/user/p/")
                   canonical))
    (should (equal (beads-live-canonical-root
                    "/ssh:user@example.com:/home/user/p")
                   canonical))))

(ert-deftest beads-live-test-canonical-root-fills-default-host ()
  "TRAMP's filled-in default host does not fork a second key."
  :tags '(:unit)
  (let* ((vec (tramp-dissect-file-name "/ssh::/p"))
         (explicit (file-name-as-directory (tramp-make-tramp-file-name vec))))
    (should (equal (beads-live-canonical-root "/ssh::/p") explicit))))

(ert-deftest beads-live-test-canonical-root-remote-not-local ()
  "A remote store and a local store with the same path stay distinct."
  :tags '(:unit)
  (should-not (equal (beads-live-canonical-root "/ssh:user@example.com:/p")
                     (beads-live-canonical-root "/p"))))

;;; WI-LIVE-03 — stream key

(ert-deftest beads-live-test-stream-key-same-store ()
  "Every spelling of one store yields one stream key."
  :tags '(:unit)
  (should (equal (beads-live-stream-key "/p")
                 (beads-live-stream-key "/p/")))
  (should (equal (beads-live-stream-key "/ssh:user@example.com:/p")
                 (beads-live-stream-key "/ssh:user@example.com:/p/")))
  (should (equal (beads-live-stream-key "/p" "bd-events")
                 (beads-live-stream-key "/p" nil))))

(ert-deftest beads-live-test-stream-key-kind-distinct ()
  "The journal kind is part of the key: `gc' and `bd' never conflate."
  :tags '(:unit)
  (should-not (equal (beads-live-stream-key "/p" beads-live-journal-kind-bd)
                     (beads-live-stream-key "/p" beads-live-journal-kind-gc)))
  (should-not (equal (beads-live-stream-key "/p" "bd-events")
                     (beads-live-stream-key "/p" "gc-events"))))

(ert-deftest beads-live-test-stream-key-store-distinct ()
  "Distinct stores do not share a stream key."
  :tags '(:unit)
  (should-not (equal (beads-live-stream-key "/p")
                     (beads-live-stream-key "/q")))
  (should-not (equal (beads-live-stream-key "/p")
                     (beads-live-stream-key "/ssh:user@example.com:/p"))))

(ert-deftest beads-live-test-stream-key-nil ()
  "A nil or empty store yields no key."
  :tags '(:unit)
  (should-not (beads-live-stream-key nil))
  (should-not (beads-live-stream-key "")))

(ert-deftest beads-live-test-key-alias ()
  "`beads-live-key' is the gascity-mirroring alias of the stream key."
  :tags '(:unit)
  (should (equal (beads-live-key "/p")
                 (beads-live-stream-key "/p")))
  (should (equal (beads-live-key "/p" "gc-events")
                 (beads-live-stream-key "/p" "gc-events"))))

;;; WI-LIVE-03 — capability probe

(defvar beads-live-test--calls 0
  "Number of mock `beads-command-execute' invocations in a test.")

(defun beads-live-test--mock-execute (value)
  "Return a mock for `beads-command-execute' returning VALUE.
Increments `beads-live-test--calls' on every call."
  (lambda (_command)
    (setq beads-live-test--calls (1+ beads-live-test--calls))
    value))

(ert-deftest beads-live-test-capability-journal-on ()
  "A `value' of \"true\" reports stream mode."
  :tags '(:unit)
  (let ((beads-live-test--calls 0))
    (cl-letf (((symbol-function 'beads-command-execute)
               (beads-live-test--mock-execute '((value . "true")))))
      (beads-live-forget-capability)
      (should (beads-live-events-journal-enabled-p "/p"))
      (should (eq (beads-live-capability "/p") 'stream)))))

(ert-deftest beads-live-test-capability-journal-off ()
  "A `value' of \"false\" reports poll mode."
  :tags '(:unit)
  (let ((beads-live-test--calls 0))
    (cl-letf (((symbol-function 'beads-command-execute)
               (beads-live-test--mock-execute '((value . "false")))))
      (beads-live-forget-capability)
      (should-not (beads-live-events-journal-enabled-p "/p"))
      (should (eq (beads-live-capability "/p") 'poll)))))

(ert-deftest beads-live-test-capability-probes-once-per-store ()
  "The capability is read once per canonical store and then cached."
  :tags '(:unit)
  (let ((beads-live-test--calls 0))
    (cl-letf (((symbol-function 'beads-command-execute)
               (beads-live-test--mock-execute '((value . "true")))))
      (beads-live-forget-capability)
      (should (beads-live-events-journal-enabled-p "/p"))
      (should (beads-live-events-journal-enabled-p "/p/"))
      (should (eq (beads-live-capability "/p") 'stream))
      (should (= beads-live-test--calls 1))
      (should (beads-live-events-journal-enabled-p "/q"))
      (should (= beads-live-test--calls 2)))))

(ert-deftest beads-live-test-capability-refresh ()
  "REFRESH re-probes even a cached store."
  :tags '(:unit)
  (let ((beads-live-test--calls 0)
        (beads-live-test--value '((value . "true"))))
    (cl-letf (((symbol-function 'beads-command-execute)
               (lambda (_command)
                 (setq beads-live-test--calls (1+ beads-live-test--calls))
                 beads-live-test--value)))
      (beads-live-forget-capability)
      (should (beads-live-events-journal-enabled-p "/p"))
      (setq beads-live-test--value '((value . "false")))
      (should (beads-live-events-journal-enabled-p "/p"))
      (should (= beads-live-test--calls 1))
      (should-not (beads-live-events-journal-enabled-p "/p" 'refresh))
      (should (= beads-live-test--calls 2)))))

(ert-deftest beads-live-test-capability-probe-failure-is-poll ()
  "A failing probe is poll mode, never a signaled error."
  :tags '(:unit)
  (let ((beads-live-test--calls 0))
    (cl-letf (((symbol-function 'beads-command-execute)
               (lambda (_command)
                 (setq beads-live-test--calls (1+ beads-live-test--calls))
                 (signal 'beads-command-error '("bd: unknown config key")))))
      (beads-live-forget-capability)
      (should-not (beads-live-events-journal-enabled-p "/p"))
      (should (eq (beads-live-capability "/p") 'poll))
      (should (= beads-live-test--calls 1)))))

(ert-deftest beads-live-test-capability-nil-store ()
  "A nil/empty store is poll without probing or signaling."
  :tags '(:unit)
  (let ((beads-live-test--calls 0))
    (cl-letf (((symbol-function 'beads-command-execute)
               (beads-live-test--mock-execute '((value . "true")))))
      (beads-live-forget-capability)
      (should-not (beads-live-events-journal-enabled-p nil))
      (should-not (beads-live-events-journal-enabled-p ""))
      (should (eq (beads-live-capability nil) 'poll))
      (should (= beads-live-test--calls 0)))))

(ert-deftest beads-live-test-capability-forget ()
  "Forgetting one store invalidates only that store's cache."
  :tags '(:unit)
  (let ((beads-live-test--calls 0))
    (cl-letf (((symbol-function 'beads-command-execute)
               (beads-live-test--mock-execute '((value . "true")))))
      (beads-live-forget-capability)
      (should (beads-live-events-journal-enabled-p "/p"))
      (should (beads-live-events-journal-enabled-p "/q"))
      (should (= beads-live-test--calls 2))
      (beads-live-forget-capability "/p")
      (should (beads-live-events-journal-enabled-p "/p"))
      (should (= beads-live-test--calls 3))
      (beads-live-forget-capability)
      (should (beads-live-events-journal-enabled-p "/q"))
      (should (= beads-live-test--calls 4)))))

(ert-deftest beads-live-test-capability-probe-command ()
  "The probe runs `bd config get events-journal' scoped to the store."
  :tags '(:unit)
  (let ((executed nil))
    (cl-letf (((symbol-function 'beads-command-execute)
               (lambda (command)
                 (setq executed command)
                 '((value . "true")))))
      (beads-live-forget-capability)
      (beads-live-events-journal-enabled-p "/p")
      (should executed)
      (should (equal (oref executed key) beads-live-events-journal-key))
      (should (equal (oref executed directory) "/p"))
      (should (member "config" (beads-command-line executed)))
      (should (member "events-journal" (beads-command-line executed))))))

(ert-deftest beads-live-test-journal-value-enabled-p ()
  "Truthy spellings enable; everything else (including nil) is off."
  :tags '(:unit)
  (should (beads-live--journal-value-enabled-p "true"))
  (should (beads-live--journal-value-enabled-p "TRUE"))
  (should (beads-live--journal-value-enabled-p " true "))
  (should (beads-live--journal-value-enabled-p "1"))
  (should-not (beads-live--journal-value-enabled-p "false"))
  (should-not (beads-live--journal-value-enabled-p nil))
  (should-not (beads-live--journal-value-enabled-p "maybe")))

(ert-deftest beads-live-test-events-journal-value ()
  "The value is extracted from the several result shapes."
  :tags '(:unit)
  (should (equal (beads-live--events-journal-value '((value . "true")))
                 "true"))
  (should (equal (beads-live--events-journal-value "false") "false"))
  (should (equal (beads-live--events-journal-value t) "true"))
  (should-not (beads-live--events-journal-value nil))
  (should-not (beads-live--events-journal-value '((other . "x")))))

;;; WI-LIVE-04 — argv

(ert-deftest beads-live-test-args-local ()
  "The stream argv is `events tail --follow' scoped with `--directory'."
  :tags '(:unit)
  (beads-live-test--with-stream (stream :root "/tmp/beads-store" :name "store")
    (should (equal (beads-live--args stream)
                   '("events" "tail" "--follow"
                     "--directory" "/tmp/beads-store")))))

(ert-deftest beads-live-test-args-resume ()
  "A known seq resumes gap-free with `--since SEQ'."
  :tags '(:unit)
  (beads-live-test--with-stream (stream :root "/tmp/beads-store" :name "store"
                                        :seq 1047)
    (should (equal (beads-live--args stream)
                   '("events" "tail" "--follow"
                     "--since" "1047"
                     "--directory" "/tmp/beads-store")))))

(ert-deftest beads-live-test-args-remote-directory-is-host-local ()
  "A remote store scopes `--directory' with the host-local path."
  :tags '(:unit)
  (beads-live-test--with-stream
      (stream :root "/ssh:user@example.com:/srv/beads" :name "store")
    (should (equal (beads-live--args stream)
                   '("events" "tail" "--follow"
                     "--directory" "/srv/beads")))))

(ert-deftest beads-live-test-args-no-json ()
  "`--json' must never appear on the stream argv."
  :tags '(:unit)
  (beads-live-test--with-stream (stream :root "/tmp/beads-store" :name "store"
                                        :seq 5)
    (should-not (member "--json" (beads-live--args stream)))))

;;; WI-LIVE-04 — transport selection

(ert-deftest beads-live-test-ssh-p-local ()
  "A local store is not ssh, so it spawns bd directly."
  :tags '(:unit)
  (should-not (beads-live--ssh-p "/tmp/beads-store")))

(ert-deftest beads-live-test-ssh-p-single-hop ()
  "A single-hop ssh-family name uses the local ssh pipe."
  :tags '(:unit)
  (should (beads-live--ssh-p "/ssh:user@example.com:/srv/beads"))
  (should (beads-live--ssh-p "/scp:user@example.com:/srv/beads")))

(ert-deftest beads-live-test-ssh-p-other-methods ()
  "Non-ssh and multi-hop names are poll-only, never a stream."
  :tags '(:unit)
  (should-not (beads-live--ssh-p "/docker:root@example.com:/srv/beads"))
  (should-not (beads-live--ssh-p "/sudo:root@example.com:/srv/beads")))

;;; WI-LIVE-04 — command

(ert-deftest beads-live-test-command-local ()
  "The local command is the bd program followed by the stream argv."
  :tags '(:unit)
  (beads-live-test--with-stream (stream :root "/tmp/beads-store" :name "store"
                                        :seq 7)
    (should (equal (beads-live-command stream)
                   (cons beads-executable
                         (beads-live--args stream))))))

(ert-deftest beads-live-test-command-ssh ()
  "The ssh command is a local no-pty pipe that cds to the store."
  :tags '(:unit)
  (beads-live-test--with-stream
      (stream :root "/ssh:user@example.com:/srv/beads" :name "store")
    (let ((argv (beads-live-command stream)))
      (should (equal (car argv) "ssh"))
      (should (member "-T" argv))
      (should (member "--" argv))
      (let ((command (car (last argv))))
        (should (string-match-p "\\`cd /srv/beads && " command))
        (should (string-match-p "exec " command))
        (should (string-match-p "events" command))
        (should (string-match-p "tail" command))
        (should (string-match-p "--follow" command))
        (should (string-match-p "--directory" command))))))

(ert-deftest beads-live-test-stderr-buffer-naming ()
  "The stderr buffer is named `*beads-live: STORE*' and is local."
  :tags '(:unit)
  (beads-live-test--with-stream
      (stream :root "/ssh:user@example.com:/srv/beads" :name "beads.el")
    (let ((buf (beads-live--stderr-buffer stream)))
      (should (equal (buffer-name buf) "*beads-live: beads.el*"))
      (with-current-buffer buf
        (should (equal default-directory temporary-file-directory))))))

;;; WI-LIVE-04 — spawn (scripted emitter)

(ert-deftest beads-live-test-spawn-builds-pipe-process ()
  "Spawn runs the built argv as a local pipe process with a stderr pipe."
  :tags '(:unit)
  (beads-live-test--with-stream (stream :root "/tmp/beads-store" :name "store"
                                        :seq 3)
    (let ((captured nil)
          (fake-stderr 'fake-stderr-pipe)
          (fake-proc 'fake-proc))
      (cl-letf (((symbol-function 'make-pipe-process)
                 (lambda (&rest _) fake-stderr))
                ((symbol-function 'make-process)
                 (lambda (&rest args)
                   (setq captured args)
                   fake-proc)))
        (should (eq (beads-live--spawn stream) fake-proc))
        (should (eq (beads-live--stream-process stream) fake-proc))
        (should (eq (beads-live--stream-mode stream) 'stream))
        (should (equal (plist-get captured :command)
                       (beads-live-command stream)))
        (should (eq (plist-get captured :connection-type) 'pipe))
        (should-not (plist-get captured :file-handler))
        (should (eq (plist-get captured :stderr) fake-stderr))
        (should (functionp (plist-get captured :filter)))
        (should (functionp (plist-get captured :sentinel)))))))

(ert-deftest beads-live-test-spawn-failure-records-reason ()
  "A make-process error leaves the stream reconnecting with a reason."
  :tags '(:unit)
  (beads-live-test--with-stream (stream :root "/tmp/beads-store" :name "store")
    (let ((retried nil))
      (cl-letf (((symbol-function 'make-pipe-process)
                 (lambda (&rest _) 'fake-stderr-pipe))
                ((symbol-function 'delete-process)
                 (lambda (&rest _) nil))
                ((symbol-function 'make-process)
                 (lambda (&rest _) (error "cannot spawn")))
                ((symbol-function 'beads-live--schedule-retry)
                 (lambda (&rest _) (setq retried t))))
        (should-not (beads-live--spawn stream))
        (should (eq (beads-live--stream-state stream) 'reconnecting))
        (should (equal (beads-live--stream-reason stream) "cannot spawn"))
        (should retried)
        (should (string-match-p
                 "cannot spawn"
                 (with-current-buffer (beads-live--stderr-buffer stream)
                   (buffer-string))))))))

;;; WI-LIVE-05 — parse

(ert-deftest beads-live-test-parse-chunk-complete-lines ()
  "Two newline-terminated JSONL lines parse in order."
  :tags '(:unit)
  (beads-live-test--with-fresh-live
    (let* ((json (concat
                  (beads-live-test--json
                   1 "create" "{\"id\":\"be-1\",\"title\":\"t\",\"status\":\"open\"}"
                   "alice")
                  "\n"
                  (beads-live-test--json
                   2 "update" "{\"id\":\"be-1\",\"status\":\"in_progress\"}"
                   "bob")
                  "\n"))
           (parsed (beads-live-parse-chunk "" json)))
      (should (equal "" (cdr parsed)))
      (should (= 2 (length (car parsed))))
      (should (= 1 (oref (car (car parsed)) seq)))
      (should (equal "create" (oref (car (car parsed)) op)))
      (should (= 2 (oref (cadr (car parsed)) seq)))
      (should (equal "bob" (oref (cadr (car parsed)) actor))))))

(ert-deftest beads-live-test-parse-chunk-buffers-partial ()
  "A trailing partial line is kept until its newline arrives."
  :tags '(:unit)
  (beads-live-test--with-fresh-live
    (let* ((line (beads-live-test--json
                  7 "close" "{\"id\":\"be-1\",\"status\":\"closed\"}" "alice"))
           (first (beads-live-parse-chunk "" (substring line 0 20))))
      (should (null (car first)))
      (should (equal (substring line 0 20) (cdr first)))
      (let ((second (beads-live-parse-chunk (cdr first) (concat (substring line 20) "\n"))))
        (should (= 1 (length (car second))))
        (should (= 7 (oref (car (car second)) seq)))
        (should (equal "" (cdr second)))))))

(ert-deftest beads-live-test-parse-chunk-skips-non-json ()
  "Banners, blanks and malformed lines are skipped without signalling."
  :tags '(:unit)
  (beads-live-test--with-fresh-live
    (let* ((json (concat
                  "note: the events journal is disabled\n"
                  "{\"seq\":4,\"op\":\"update\",\"issue_id\":\"be-1\",\"issue\":{\"id\":\"be-1\"}}\n"
                  "not json at all\n"
                  "{ this is not valid json }\n"
                  "\n"))
           (parsed (beads-live-parse-chunk "" json)))
      (should (equal "" (cdr parsed)))
      (should (= 1 (length (car parsed))))
      (should (= 4 (oref (car (car parsed)) seq))))))

(ert-deftest beads-live-test-parse-chunk-keeps-raw-issue ()
  "The raw wire issue alist is remembered, including `is_blocked'."
  :tags '(:unit)
  (beads-live-test--with-fresh-live
    (let ((record (beads-live-test--parse-one
                   3 "dep_add"
                   "{\"id\":\"be-1\",\"is_blocked\":true,\"created_by\":\"carol\"}"
                   "alice")))
      (should (eq t (alist-get 'is_blocked
                               (gethash record beads-live--raw-issues))))
      (should (equal "carol" (alist-get 'created_by
                                         (gethash record beads-live--raw-issues))))
      (should (beads-issue-p (oref record issue))))))

;;; WI-LIVE-05 — deliver

(ert-deftest beads-live-test-deliver-advances-seq-and-dedupes ()
  "The stream seq advances to the max record seq; a replay is ignored."
  :tags '(:unit)
  (beads-live-test--with-fresh-live
    (let* ((stream (beads-live-test--stream))
           (model (beads-live-model stream))
           (snap "{\"id\":\"be-1\",\"title\":\"t\",\"status\":\"open\"}"))
      (beads-live--deliver stream (list (beads-live-test--parse-one 3 "create" snap)))
      (should (= 3 (beads-live--stream-seq stream)))
      (should (= 1 (length (beads-live--model-ring model))))
      (beads-live--deliver stream (list (beads-live-test--parse-one 3 "create" snap)))
      (should (= 1 (length (beads-live--model-ring model))))
      (beads-live--deliver stream (list (beads-live-test--parse-one 5 "update" snap)))
      (should (= 5 (beads-live--stream-seq stream)))
      (should (= 2 (length (beads-live--model-ring model)))))))

(ert-deftest beads-live-test-deliver-sparse-merge ()
  "The snapshot is authoritative for subset fields; outside fields keep
their baseline value.  An absent optional field is its zero value."
  :tags '(:unit)
  (beads-live-test--with-fresh-live
    (let* ((stream (beads-live-test--stream))
           (model (beads-live-model stream))
           (issues (beads-live--model-issues model)))
      (puthash "be-1"
               '((id . "be-1") (title . "old") (status . "open")
                 (description . "keep me") (dependency_count . 3)
                 (labels . ("x")) (is_blocked . t))
               issues)
      (beads-live--deliver
       stream
       (list (beads-live-test--parse-one
              9 "update"
              "{\"id\":\"be-1\",\"title\":\"new\",\"status\":\"in_progress\",\"labels\":[\"y\"]}")))
      (let ((merged (gethash "be-1" issues)))
        (should (equal "in_progress" (alist-get 'status merged)))
        (should (equal "new" (alist-get 'title merged)))
        (should (equal '("y") (alist-get 'labels merged)))
        (should (equal "keep me" (alist-get 'description merged)))
        (should (equal 3 (alist-get 'dependency_count merged)))
        (should (null (assq 'is_blocked merged)))))))

(ert-deftest beads-live-test-deliver-present-is-blocked-wins ()
  "A present `is_blocked' overrides the baseline."
  :tags '(:unit)
  (beads-live-test--with-fresh-live
    (let* ((stream (beads-live-test--stream))
           (model (beads-live-model stream))
           (issues (beads-live--model-issues model)))
      (puthash "be-1" '((id . "be-1") (is_blocked . t)) issues)
      (beads-live--deliver
       stream
       (list (beads-live-test--parse-one
              4 "update" "{\"id\":\"be-1\",\"is_blocked\":false}")))
      (should (null (alist-get 'is_blocked (gethash "be-1" issues)))))))

(ert-deftest beads-live-test-deliver-delete-tombstone ()
  "A `delete' with a null snapshot leaves a tombstone."
  :tags '(:unit)
  (beads-live-test--with-fresh-live
    (let* ((stream (beads-live-test--stream))
           (model (beads-live-model stream)))
      (beads-live--deliver
       stream (list (beads-live-test--parse-one 1 "create" "{\"id\":\"be-1\"}")))
      (beads-live--deliver
       stream (list (beads-live-test--parse-one 2 "delete" "null")))
      (should (eq :deleted (gethash "be-1" (beads-live--model-issues model)))))))

(ert-deftest beads-live-test-deliver-label-add-two-rows ()
  "A label add of two labels arrives as two update rows; both append."
  :tags '(:unit)
  (beads-live-test--with-fresh-live
    (let* ((stream (beads-live-test--stream))
           (model (beads-live-model stream))
           (records (list (beads-live-test--parse-one
                           11 "update" "{\"id\":\"be-1\",\"labels\":[\"bar\"]}")
                          (beads-live-test--parse-one
                           12 "update" "{\"id\":\"be-1\",\"labels\":[\"bar\",\"foo\"]}"))))
      (beads-live--deliver stream records)
      (should (= 2 (length (gethash "be-1" (beads-live--model-logs model)))))
      (should (equal '("bar" "foo")
                     (alist-get 'labels (gethash "be-1"
                                                (beads-live--model-issues model))))))))

(ert-deftest beads-live-test-deliver-actorless-derived-row ()
  "A derived cascade row carries no actor and still delivers."
  :tags '(:unit)
  (beads-live-test--with-fresh-live
    (let* ((stream (beads-live-test--stream))
           (model (beads-live-model stream))
           (record (beads-live-test--parse-one
                    7 "update" "{\"id\":\"be-1\",\"is_blocked\":false}")))
      (should (null (oref record actor)))
      (beads-live--deliver stream (list record))
      (should (= 1 (length (beads-live--model-ring model)))))))

(ert-deftest beads-live-test-deliver-ring-is-bounded ()
  "The ring keeps only the newest records."
  :tags '(:unit)
  (beads-live-test--with-fresh-live
    (let* ((stream (beads-live-test--stream))
           (model (beads-live-model stream)))
      (setf (beads-live--model-ring-size model) 3)
      (dotimes (i 5)
        (beads-live--deliver
         stream (list (beads-live-test--parse-one
                       (1+ i) "update" "{\"id\":\"be-1\",\"status\":\"open\"}"))))
      (should (= 3 (length (beads-live--model-ring model))))
      (should (= 5 (oref (car (beads-live--model-ring model)) seq))))))

(ert-deftest beads-live-test-queue-flush-coalesces ()
  "A burst flushes once: one invalidation hook run and one checkpoint."
  :tags '(:unit)
  (beads-live-test--with-fresh-live
    (let* ((stream (beads-live-test--stream))
           (runs nil)
           (checkpoints 0)
           (beads-live-invalidate-functions
            (list (lambda (root kinds ops)
                    (push (list root kinds ops) runs)))))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (&rest _) 'fake-timer))
                ((symbol-function 'beads-live--checkpoint-write)
                 (lambda (_stream) (setq checkpoints (1+ checkpoints)))))
        (beads-live--deliver
         stream
         (list (beads-live-test--parse-one 1 "create" "{\"id\":\"be-1\"}")
               (beads-live-test--parse-one 2 "update" "{\"id\":\"be-1\"}")
               (beads-live-test--parse-one 3 "close" "{\"id\":\"be-1\"}")))
        (should (= 3 (length (beads-live--stream-pending stream))))
        (beads-live--flush stream)
        (should (null (beads-live--stream-pending stream)))
        (should (null (beads-live--stream-debounce-timer stream)))
        (should (= 1 (length runs)))
        (should (equal '("create" "update" "close") (nth 2 (car runs))))
        (should (= 1 checkpoints))))))

(ert-deftest beads-live-test-filter-partial-then-deliver ()
  "The filter buffers a partial line and delivers the next one whole."
  :tags '(:unit)
  (beads-live-test--with-fresh-live
    (let* ((stream (beads-live-test--stream))
           (model (beads-live-model stream))
           (line (beads-live-test--json
                  21 "create" "{\"id\":\"be-1\",\"title\":\"t\"}" "alice")))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (&rest _) 'fake-timer)))
        (beads-live--filter stream (substring line 0 15))
        (should (= 0 (length (beads-live--model-ring model))))
        (beads-live--filter stream (concat (substring line 15) "\n")))
      (should (= 1 (length (beads-live--model-ring model))))
      (should (= 21 (beads-live--stream-seq stream))))))

;;; WI-LIVE-06 — checkpoint files

;;; Named seams (dynamic scope)

(defun beads-live-test--branch (_root) "main")
(defun beads-live-test--branch-dev (_root) "dev")
(defun beads-live-test--replica (_root) "r1")
(defun beads-live-test--baseline (_stream) '(baselined))
(defun beads-live-test--head-5 (_stream) 5)
(defun beads-live-test--head-11 (_stream) 11)
(defun beads-live-test--head-99 (_stream) 99)
(defun beads-live-test--head-100 (_stream) 100)
(defun beads-live-test--poll (_root _refresh) 'poll)
(defun beads-live-test--spawn (stream)
  "Fake spawn: mark STREAM live so tests can observe the call."
  (beads-live--set-state stream 'live nil))

(ert-deftest beads-live-test-checkpoint-file-is-identity-keyed ()
  "The checkpoint path is deterministic and unique per identity."
  :tags '(:unit)
  (beads-live-test--with-state-dir
    (let ((main (beads-live-checkpoint-file "/s" "main" "r1")))
      (should (string-prefix-p
               (file-name-as-directory beads-live-state-directory) main))
      (should (equal main (beads-live-checkpoint-file "/s" "main" "r1")))
      (should-not (equal main (beads-live-checkpoint-file "/s" "dev" "r1")))
      (should-not (equal main (beads-live-checkpoint-file "/s" "main" "r2")))
      (should-not (equal main (beads-live-checkpoint-file "/t" "main" "r1"))))))

(ert-deftest beads-live-test-checkpoint-round-trip ()
  "A written checkpoint reads back with its seq and identity."
  :tags '(:unit)
  (beads-live-test--with-state-dir
    (beads-live-checkpoint-write "/s" "main" "r1" 42)
    (let ((checkpoint (beads-live-checkpoint-read "/s" "main" "r1")))
      (should (equal (plist-get checkpoint :seq) 42))
      (should (equal (plist-get checkpoint :branch) "main"))
      (should (equal (plist-get checkpoint :replica) "r1")))))

(ert-deftest beads-live-test-checkpoint-missing-is-nil ()
  "No checkpoint file means no resume."
  :tags '(:unit)
  (beads-live-test--with-state-dir
    (should-not (beads-live-checkpoint-read "/s" "main" "r1"))))

(ert-deftest beads-live-test-checkpoint-rejects-branch-mismatch ()
  "A copied checkpoint for another branch is discarded, not carried."
  :tags '(:unit)
  (beads-live-test--with-state-dir
    (beads-live-checkpoint-write "/s" "main" "r1" 7)
    (rename-file (beads-live-checkpoint-file "/s" "main" "r1")
                 (beads-live-checkpoint-file "/s" "dev" "r1") t)
    (should-not (beads-live-checkpoint-read "/s" "dev" "r1"))))

(ert-deftest beads-live-test-checkpoint-rejects-replica-mismatch ()
  "A copied checkpoint for another replica is discarded, not carried."
  :tags '(:unit)
  (beads-live-test--with-state-dir
    (beads-live-checkpoint-write "/s" "main" "r1" 7)
    (rename-file (beads-live-checkpoint-file "/s" "main" "r1")
                 (beads-live-checkpoint-file "/s" "main" "r2") t)
    (should-not (beads-live-checkpoint-read "/s" "main" "r2"))))

(ert-deftest beads-live-test-checkpoint-delete ()
  "Deleting removes the file and is idempotent."
  :tags '(:unit)
  (beads-live-test--with-state-dir
    (beads-live-checkpoint-write "/s" "main" "r1" 1)
    (should (beads-live-checkpoint-delete "/s" "main" "r1"))
    (should-not (beads-live-checkpoint-delete "/s" "main" "r1"))
    (should-not (beads-live-checkpoint-read "/s" "main" "r1"))))

;;; WI-LIVE-06 — start / resume

(ert-deftest beads-live-test-start-resumes-from-checkpoint ()
  "A valid checkpoint resumes with its seq and skips the baseline."
  :tags '(:unit)
  (beads-live-test--with-state-dir
    (beads-live-test--with-store (stream)
      (let ((beads-live-branch-function #'beads-live-test--branch)
            (beads-live-replica-function #'beads-live-test--replica)
            (beads-live-baseline-function #'beads-live-test--baseline)
            (beads-live-head-function #'beads-live-test--head-99)
            (beads-live-spawn-function #'beads-live-test--spawn))
        (beads-live-checkpoint-write (beads-live--stream-root stream)
                                     "main" "r1" 12)
        (beads-live--start-stream stream)
        (should-not (beads-live--stream-baseline stream))
        (should (eq (beads-live--stream-state stream) 'live))
        (should (equal (beads-live--stream-seq stream) 12))
        (should (equal (beads-live--stream-branch stream) "main"))
        (should (equal (beads-live--stream-replica stream) "r1"))))))

(ert-deftest beads-live-test-start-baselines-without-checkpoint ()
  "Without a checkpoint the store is baselined and the head persisted."
  :tags '(:unit)
  (beads-live-test--with-state-dir
    (beads-live-test--with-store (stream)
      (let ((beads-live-branch-function #'beads-live-test--branch)
            (beads-live-replica-function #'beads-live-test--replica)
            (beads-live-baseline-function #'beads-live-test--baseline)
            (beads-live-head-function #'beads-live-test--head-99)
            (beads-live-spawn-function #'beads-live-test--spawn))
        (beads-live--start-stream stream)
        (should (equal (beads-live--stream-baseline stream) '(baselined)))
        (should (eq (beads-live--stream-state stream) 'live))
        (should (equal (beads-live--stream-seq stream) 99))
        (should (equal (plist-get (beads-live-checkpoint stream) :seq) 99))))))

(ert-deftest beads-live-test-start-discards-foreign-identity ()
  "A checkpoint from another branch is discarded and re-baselined."
  :tags '(:unit)
  (beads-live-test--with-state-dir
    (beads-live-test--with-store (stream)
      (let ((beads-live-branch-function #'beads-live-test--branch-dev)
            (beads-live-replica-function #'beads-live-test--replica)
            (beads-live-baseline-function #'beads-live-test--baseline)
            (beads-live-head-function #'beads-live-test--head-5)
            (beads-live-spawn-function #'beads-live-test--spawn))
        (beads-live-checkpoint-write (beads-live--stream-root stream)
                                     "main" "r1" 400)
        (beads-live--start-stream stream)
        (should (equal (beads-live--stream-baseline stream) '(baselined)))
        (should (equal (beads-live--stream-branch stream) "dev"))
        (should (equal (beads-live--stream-seq stream) 5))))))

(ert-deftest beads-live-test-start-poll-mode-starts-no-follower ()
  "A journal-off store is `poll' and spawns no stream process."
  :tags '(:unit)
  (beads-live-test--with-store (stream)
    (let ((beads-live-capability-function #'beads-live-test--poll)
          (beads-live-spawn-function #'beads-live-test--spawn))
      (cl-letf (((symbol-function 'beads-live--start-poll)
                 (lambda (_stream) nil)))
        (let ((started (beads-live-start (beads-live--stream-root stream))))
          (should (eq (beads-live--stream-mode started) 'poll))
          (should-not (eq (beads-live--stream-state started) 'live)))))))

;;; WI-LIVE-06 — re-baseline

(ert-deftest beads-live-test-truncation-detection ()
  "The pruned-checkpoint diagnostic is recognized and parsed."
  :tags '(:unit)
  (let ((text (concat "Error: events journal truncated: checkpoint 7 is "
                      "below the retained window [12..99]; records were pruned")))
    (should (beads-live-truncation-p text))
    (should (equal (beads-live-truncation-info text)
                   '(:checkpoint 7 :floor 12 :head 99)))
    (should-not (beads-live-truncation-p "some other error"))
    (should-not (beads-live-truncation-info "some other error"))))

(ert-deftest beads-live-test-handle-truncation-rebaselines ()
  "A pruned `--since' re-baselines and resets the checkpoint to head."
  :tags '(:unit)
  (beads-live-test--with-state-dir
    (beads-live-test--with-store (stream)
      (let ((beads-live-baseline-function #'beads-live-test--baseline)
            (beads-live-head-function #'beads-live-test--head-100))
        (setf (beads-live--stream-branch stream) "main"
              (beads-live--stream-replica stream) "r1")
        (beads-live-checkpoint-write (beads-live--stream-root stream)
                                     "main" "r1" 3)
        (should (equal
                 (beads-live-handle-truncation
                  stream
                  (concat "events journal truncated: checkpoint 3 is below "
                          "the retained window [10..100]"))
                 100))
        (should (equal (beads-live--stream-baseline stream) '(baselined)))
        (should (eq (beads-live--stream-state stream) 'partial))
        (should (equal (beads-live--stream-seq stream) 100))
        (should (equal (plist-get (beads-live-checkpoint stream) :seq) 100))))))

(ert-deftest beads-live-test-handle-truncation-ignores-other-errors ()
  "A non-truncation stderr tail does not re-baseline."
  :tags '(:unit)
  (beads-live-test--with-store (stream)
    (let ((beads-live-baseline-function #'beads-live-test--baseline))
      (should-not (beads-live-handle-truncation stream "connection reset"))
      (should-not (beads-live--stream-baseline stream)))))

(ert-deftest beads-live-test-reconcile-rebaselines ()
  "The periodic reconcile re-baselines with its reason recorded."
  :tags '(:unit)
  (beads-live-test--with-state-dir
    (beads-live-test--with-store (stream)
      (let ((beads-live-baseline-function #'beads-live-test--baseline)
            (beads-live-head-function #'beads-live-test--head-11))
        (setf (beads-live--stream-branch stream) "main"
              (beads-live--stream-replica stream) "r1")
        (beads-live-reconcile stream)
        (should (equal (beads-live--stream-baseline stream) '(baselined)))
        (should (eq (beads-live--stream-state stream) 'partial))
        (should (equal (beads-live--stream-reason stream) "periodic reconcile"))
        (should (equal (beads-live--stream-seq stream) 11))))))

;;; WI-LIVE-07 — classification

(ert-deftest beads-live-test-classify-journal-disabled ()
  "The journal-off note classifies as `poll' (stop retrying)."
  :tags '(:unit)
  (should (eq (beads-live--classify
               '("note: the events journal is disabled for this workspace"
                 "new mutations are not being recorded"))
              'poll)))

(ert-deftest beads-live-test-classify-unknown-subcommand ()
  "An unknown/unsupported `bd events' invocation classifies as `poll'."
  :tags '(:unit)
  (should (eq (beads-live--classify
               '("Error: unknown command \"events\" for \"bd\""))
              'poll))
  (should (eq (beads-live--classify
               '("Error: unrecognized subcommand \"tail\""))
              'poll))
  (should (eq (beads-live--classify
               '("Error: operation not supported by this backend"))
              'poll)))

(ert-deftest beads-live-test-classify-truncated ()
  "A pruned checkpoint classifies as `partial' (re-baseline)."
  :tags '(:unit)
  (should (eq (beads-live--classify
               '("Error: events journal truncated: checkpoint 0 is below the retained window [5..10]; records 1..4 were pruned"
                 "Hint: resume with --since 4 to continue"))
              'partial)))

(ert-deftest beads-live-test-classify-connection ()
  "A connection or host failure classifies as `offline'."
  :tags '(:unit)
  (dolist (line '("ssh: connect to host example.com port 22: Connection refused"
                  "dial tcp 10.0.0.1:3306: connect: connection refused"
                  "Error: connection reset by peer"
                  "ssh: Could not resolve hostname example.com"
                  "ssh: connect to host example.com port 22: No route to host"))
    (should (eq (beads-live--classify (list line)) 'offline))))

(ert-deftest beads-live-test-classify-other-is-reconnecting ()
  "Anything else falls back to `reconnecting'."
  :tags '(:unit)
  (should (eq (beads-live--classify '("Error: something unexpected")) 'reconnecting))
  (should (eq (beads-live--classify nil) 'reconnecting))
  (should (eq (beads-live--classify "") 'reconnecting)))

(ert-deftest beads-live-test-classify-accepts-string ()
  "A raw stderr string classifies the same as a list of its lines."
  :tags '(:unit)
  (should (eq (beads-live--classify
               "Error: events journal truncated: checkpoint 0 is below the retained window")
              'partial)))

(ert-deftest beads-live-test-classify-precedence ()
  "The documented order wins when fragments co-occur."
  :tags '(:unit)
  (should (eq (beads-live--classify
               '("the events journal is disabled"
                 "Error: connection refused"))
              'poll))
  (should (eq (beads-live--classify
               '("Error: events journal truncated: floor 5"
                 "connection reset"))
              'partial)))

;;; WI-LIVE-07 — backoff

(ert-deftest beads-live-test-backoff-schedule ()
  "The default backoff schedule is exactly `(2 5 15 60)'."
  :tags '(:unit)
  (should (equal (mapcar #'beads-live--backoff-delay '(0 1 2 3))
                 '(2 5 15 60))))

(ert-deftest beads-live-test-backoff-repeats ()
  "The schedule repeats once attempts run past its end."
  :tags '(:unit)
  (should (equal (mapcar #'beads-live--backoff-delay '(4 5 6 7 8))
                 '(2 5 15 60 2))))

(ert-deftest beads-live-test-backoff-honours-option ()
  "The schedule honours a customized `beads-live-backoff'."
  :tags '(:unit)
  (let ((beads-live-backoff '(1 3)))
    (should (equal (mapcar #'beads-live--backoff-delay '(0 1 2 3))
                   '(1 3 1 3)))))

(ert-deftest beads-live-test-stream-stable-reset ()
  "A stream older than `beads-live-stable-after' is stable."
  :tags '(:unit)
  (beads-live-test--with-stream
      (stream :started (- (float-time) (1+ beads-live-stable-after)))
    (should (beads-live--stream-stable-p stream)))
  (beads-live-test--with-stream (stream :started (float-time))
    (should-not (beads-live--stream-stable-p stream))))

;;; WI-LIVE-07 — retry scheduling

(ert-deftest beads-live-test-schedule-retry-uses-delay ()
  "`beads-live--schedule-retry' uses the current attempt's delay and advances."
  :tags '(:unit)
  (beads-live-test--capture calls
    (beads-live-test--with-stream (stream :attempt 1)
      (beads-live--schedule-retry stream)
      (should (= (beads-live--stream-attempt stream) 2))
      (should (eq (beads-live--stream-retry-timer stream) 'fake-timer))
      (should (numberp (beads-live--stream-retry-at stream)))
      (should (= (nth 0 (plist-get calls :run-at-time)) 5))
      (should (eq (nth 2 (plist-get calls :run-at-time)) #'beads-live--retry))
      (should (eq (car (nth 3 (plist-get calls :run-at-time))) stream)))))

(ert-deftest beads-live-test-retry-starts-when-enabled ()
  "`beads-live--retry' starts the stream when enabled and not stopping."
  :tags '(:unit)
  (beads-live-test--capture calls
    (beads-live-test--with-stream (stream :retry-timer 'fake-timer)
      (beads-live--retry stream)
      (should-not (beads-live--stream-retry-timer stream))
      (should (eq (car (plist-get calls :start)) stream)))))

(ert-deftest beads-live-test-retry-skips-when-stopping ()
  "`beads-live--retry' does nothing for a deliberately stopped stream."
  :tags '(:unit)
  (beads-live-test--capture calls
    (beads-live-test--with-stream (stream :stopping t)
      (beads-live--retry stream)
      (should-not (plist-get calls :start)))))

;;; WI-LIVE-07 — exit handling

(ert-deftest beads-live-test-exited-partial-rebaselines ()
  "A truncated checkpoint becomes `partial', re-baselines and retries."
  :tags '(:unit)
  (beads-live-test--capture calls
    (beads-live-test--with-stream (stream :name "store" :started (float-time))
      (let ((mark (beads-live-test--stage-stderr
                   stream
                   '("Error: events journal truncated: checkpoint 0 is below the retained window"))))
        (should (eq (beads-live--exited stream nil mark) 'partial))
        (should (eq (beads-live--stream-state stream) 'partial))
        (should (eq (car (plist-get calls :rebaseline)) stream))
        (should (numberp (beads-live--stream-retry-at stream)))))))

(ert-deftest beads-live-test-exited-journal-off-polls ()
  "A disabled journal becomes `poll', starts polling and never retries."
  :tags '(:unit)
  (beads-live-test--capture calls
    (beads-live-test--with-stream (stream :name "store" :started (float-time))
      (let ((mark (beads-live-test--stage-stderr
                   stream
                   '("note: the events journal is disabled for this workspace"))))
        (should (eq (beads-live--exited stream nil mark) 'poll))
        (should (eq (beads-live--stream-state stream) 'poll))
        (should (eq (beads-live--stream-mode stream) 'poll))
        (should (eq (car (plist-get calls :start-poll)) stream))
        (should-not (beads-live--stream-retry-at stream))
        (should-not (plist-get calls :run-at-time))))))

(ert-deftest beads-live-test-exited-connection-offline ()
  "A connection failure becomes `offline' and schedules a retry."
  :tags '(:unit)
  (beads-live-test--capture calls
    (beads-live-test--with-stream (stream :name "store" :started (float-time))
      (let ((mark (beads-live-test--stage-stderr
                   stream
                   '("ssh: connect to host example.com port 22: Connection refused"))))
        (should (eq (beads-live--exited stream nil mark) 'offline))
        (should (eq (beads-live--stream-state stream) 'offline))
        (should (numberp (beads-live--stream-retry-at stream)))
        (should (plist-get calls :run-at-time))))))

(ert-deftest beads-live-test-exited-unknown-reconnecting ()
  "An unclassified exit becomes `reconnecting' and schedules a retry."
  :tags '(:unit)
  (beads-live-test--capture calls
    (beads-live-test--with-stream (stream :name "store" :started (float-time))
      (let ((mark (beads-live-test--stage-stderr stream '("Error: something odd"))))
        (should (eq (beads-live--exited stream nil mark) 'reconnecting))
        (should (eq (beads-live--stream-state stream) 'reconnecting))
        (should (numberp (beads-live--stream-retry-at stream)))
        (should (plist-get calls :run-at-time))))))

(ert-deftest beads-live-test-exited-stable-resets-attempt ()
  "A stable stream resets its backoff attempt before the next failure."
  :tags '(:unit)
  (beads-live-test--capture calls
    (beads-live-test--with-stream
        (stream :name "store" :attempt 3
                :started (- (float-time) (1+ beads-live-stable-after)))
      (let ((mark (beads-live-test--stage-stderr stream '("connection refused"))))
        (beads-live--exited stream nil mark)
        (should (= (beads-live--stream-attempt stream) 1))
        (should (= (nth 0 (plist-get calls :run-at-time)) 2))))))

(ert-deftest beads-live-test-exited-stopping-does-not-retry ()
  "A deliberately stopped stream transitions to `off' with no timer."
  :tags '(:unit)
  (beads-live-test--capture calls
    (beads-live-test--with-stream (stream :name "store" :stopping t :started (float-time))
      (let ((mark (beads-live-test--stage-stderr stream '("connection refused"))))
        (beads-live--exited stream nil mark)
        (should (eq (beads-live--stream-state stream) 'off))
        (should-not (beads-live--stream-retry-at stream))
        (should-not (plist-get calls :run-at-time))))))

;;; WI-LIVE-07 — reconnect

(ert-deftest beads-live-test-reconnect-immediate-full-refresh ()
  "`beads-live-reconnect' resets, marks a full refresh and starts at once."
  :tags '(:unit)
  (beads-live-test--capture calls
    (beads-live-test--with-stream (stream :attempt 3 :stopping t :mode 'poll)
      (beads-live-reconnect stream)
      (should (= (beads-live--stream-attempt stream) 0))
      (should (eq (beads-live--stream-resume stream) :all))
      (should-not (beads-live--stream-stopping stream))
      (should-not (beads-live--stream-retry-at stream))
      (should (eq (car (plist-get calls :start)) stream))
      (should (eq (beads-live--stream-state stream) 'connecting)))))

;;; WI-LIVE-08 — poll fallback

(ert-deftest beads-live-test-start-poll-arms-timer ()
  "`beads-live--start-poll' arms a repeating poll timer and sets poll mode."
  :tags '(:unit)
  (beads-live-test--with-stream (stream :name "store")
    (let ((calls nil))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (delay repeat fn &rest args)
                   (setq calls (list delay repeat fn args))
                   'fake-poll-timer))
                ((symbol-function 'cancel-timer) #'ignore))
        (beads-live--start-poll stream)
        (should (eq (beads-live--stream-mode stream) 'poll))
        (should (eq (beads-live--stream-poll-timer stream) 'fake-poll-timer))
        (should (= (nth 0 calls) beads-live-poll-interval))
        (should (= (nth 1 calls) beads-live-poll-interval))
        (should (eq (nth 2 calls) #'beads-live--poll))
        (beads-live--stop-poll stream)
        (should-not (beads-live--stream-poll-timer stream))))))

(ert-deftest beads-live-test-poll-delivers-new-records ()
  "A poll result is delivered through the sparse-merge path and marks poll."
  :tags '(:unit)
  (beads-live-test--with-registry
    (let* ((root (make-temp-file "beads-live-store-" t))
           (stream (beads-live--stream-for root))
           (records (list (beads-live-test--parse-one
                           4 "update" "{\"id\":\"be-1\",\"status\":\"open\"}"))))
      (setf (beads-live--stream-seq stream) 3)
      (cl-letf (((symbol-function 'beads-command-execute-async)
                 (lambda (_command on-success &optional _on-error &rest _kw)
                   (funcall on-success records)))
                ((symbol-function 'run-at-time)
                 (lambda (&rest _) 'fake-timer)))
        (beads-live--poll stream)
        (should (= 4 (beads-live--stream-seq stream)))
        (should (= 1 (length (beads-live--model-ring (beads-live-model stream)))))
        (should (eq (beads-live--stream-state stream) 'poll))
        (should-not (beads-live--stream-poll-busy stream))))))

(ert-deftest beads-live-test-poll-error-rebaselines-on-truncation ()
  "A pruned checkpoint over the poll path triggers a re-baseline."
  :tags '(:unit)
  (beads-live-test--with-registry
    (let* ((root (make-temp-file "beads-live-store-" t))
           (stream (beads-live--stream-for root))
           (rebased nil))
      (cl-letf (((symbol-function 'beads-command-execute-async)
                 (lambda (_command _on-success on-error &rest _kw)
                   (funcall on-error
                            (list 'beads-command-error
                                  "bd: events journal truncated: checkpoint 3 is below the retained window [10..100]"))))
                ((symbol-function 'beads-live--rebaseline)
                 (lambda (_stream &optional reason) (setq rebased reason)))
                ((symbol-function 'run-at-time)
                 (lambda (&rest _) 'fake-timer)))
        (beads-live--poll stream)
        (should rebased)
        (should (string-match-p "truncated" rebased))
        (should-not (beads-live--stream-poll-busy stream))))))

(ert-deftest beads-live-test-poll-error-offline ()
  "A non-truncation poll failure marks the stream offline."
  :tags '(:unit)
  (beads-live-test--with-registry
    (let* ((root (make-temp-file "beads-live-store-" t))
           (stream (beads-live--stream-for root)))
      (cl-letf (((symbol-function 'beads-command-execute-async)
                 (lambda (_command _on-success on-error &rest _kw)
                   (funcall on-error (list 'beads-command-error
                                           "bd: connection refused"))))
                ((symbol-function 'run-at-time)
                 (lambda (&rest _) 'fake-timer)))
        (beads-live--poll stream)
        (should (eq (beads-live--stream-state stream) 'offline))
        (should-not (beads-live--stream-poll-busy stream))))))

;;; WI-LIVE-09 — attach / detach

(ert-deftest beads-live-test-attach-detach-refcount ()
  "The first attach starts; the last detach stops and forgets."
  :tags '(:unit)
  (beads-live-test--with-registry
    (let* ((root (make-temp-file "beads-live-store-" t))
           (buf1 (beads-live-test--view-buffer root))
           (buf2 (beads-live-test--view-buffer root))
           (started 0)
           (stopped 0)
           (gone nil)
           (beads-live-state-functions
            (list (lambda (r state _reason)
                    (when (eq state 'gone) (setq gone r))))))
      (cl-letf (((symbol-function 'beads-live--start)
                 (lambda (_stream) (setq started (1+ started))))
                ((symbol-function 'beads-live--running-p)
                 (lambda (_stream) (> started 0)))
                ((symbol-function 'beads-live--stop)
                 (lambda (stream)
                   (setq stopped (1+ stopped))
                   (beads-live--set-state stream 'off nil))))
        (let ((stream (beads-live-attach buf1)))
          (should stream)
          (should (= 1 started))
          (should (equal (list buf1) (beads-live--stream-views stream)))
          ;; The second view joins the same stream; no second stream/start.
          (should (eq (beads-live-attach buf2) stream))
          (should (= 1 started))
          (should (= 2 (length (beads-live--stream-views stream))))
          ;; First detach leaves the refcount at one: no stop.
          (beads-live-detach buf1)
          (should (= 0 stopped))
          (should (equal (list buf2) (beads-live--stream-views stream)))
          ;; Last detach stops, forgets and announces `gone'.
          (beads-live-detach buf2)
          (should (= 1 stopped))
          (should gone)
          (should-not (gethash (beads-live--stream-key root)
                               beads-live--streams))))
      (beads-live-test--kill buf1)
      (beads-live-test--kill buf2))))

(ert-deftest beads-live-test-attach-sets-view-locals ()
  "Attach records the refresh function and kinds buffer-locally."
  :tags '(:unit)
  (beads-live-test--with-registry
    (let* ((root (make-temp-file "beads-live-store-" t))
           (buf (beads-live-test--view-buffer root))
           (refresh (lambda () nil)))
      (cl-letf (((symbol-function 'beads-live--start) (lambda (_stream) nil))
                ((symbol-function 'beads-live--running-p) (lambda (_stream) t)))
        (beads-live-attach buf :refresh refresh :kinds '(show))
        (with-current-buffer buf
          (should (eq beads-live--refresh refresh))
          (should (equal beads-live--kinds '(show)))
          (should (equal beads-live--root (beads-live--store-root root)))))
      (beads-live-test--kill buf))))

;;; WI-LIVE-09 — subscribe

(ert-deftest beads-live-test-subscribe-unsubscribe ()
  "A subscriber receives raw records and stops after unsubscribing."
  :tags '(:unit)
  (beads-live-test--with-registry
    (let* ((root (make-temp-file "beads-live-store-" t))
           (buf (beads-live-test--view-buffer root))
           (got nil))
      (cl-letf (((symbol-function 'beads-live--start) (lambda (_stream) nil))
                ((symbol-function 'beads-live--running-p) (lambda (_stream) t))
                ((symbol-function 'run-at-time) (lambda (&rest _) 'fake-timer)))
        (beads-live-attach buf)
        (let* ((stream (gethash (beads-live--stream-key root) beads-live--streams))
               (handle (beads-live-subscribe
                        (lambda (record) (push record got)) buf)))
          (should handle)
          (beads-live--deliver
           stream (list (beads-live-test--parse-one 1 "create" "{\"id\":\"be-1\"}")))
          (should (= 1 (length got)))
          (beads-live-unsubscribe handle)
          (beads-live--deliver
           stream (list (beads-live-test--parse-one 2 "update" "{\"id\":\"be-1\"}")))
          (should (= 1 (length got)))))
      (beads-live-test--kill buf))))

(ert-deftest beads-live-test-subscribe-runs-in-buffer ()
  "A subscriber runs with its own buffer current."
  :tags '(:unit)
  (beads-live-test--with-registry
    (let* ((root (make-temp-file "beads-live-store-" t))
           (buf (beads-live-test--view-buffer root))
           (seen nil))
      (cl-letf (((symbol-function 'beads-live--start) (lambda (_stream) nil))
                ((symbol-function 'beads-live--running-p) (lambda (_stream) t))
                ((symbol-function 'run-at-time) (lambda (&rest _) 'fake-timer)))
        (beads-live-attach buf)
        (let* ((stream (gethash (beads-live--stream-key root) beads-live--streams))
               (handle (beads-live-subscribe
                        (lambda (_record) (setq seen (current-buffer))) buf)))
          (beads-live--deliver
           stream (list (beads-live-test--parse-one 1 "create" "{\"id\":\"be-1\"}")))
          (should (eq seen buf))
          (beads-live-unsubscribe handle)))
      (beads-live-test--kill buf))))

;;; WI-LIVE-09 — public per-record hook (advisory A1)

(ert-deftest beads-live-test-event-hooks-once-per-applied-record ()
  "`beads-event-hooks' fires once per applied record, not per echo."
  :tags '(:unit)
  (beads-live-test--with-fresh-live
    (let* ((stream (beads-live-test--stream))
           (seen nil)
           (beads-event-hooks
            (list (lambda (record root) (push (list record root) seen)))))
      (beads-live--deliver
       stream
       (list (beads-live-test--parse-one 1 "create" "{\"id\":\"be-1\"}")
             (beads-live-test--parse-one 2 "update" "{\"id\":\"be-1\"}")
             (beads-live-test--parse-one 3 "close" "{\"id\":\"be-1\"}")))
      (should (= 3 (length seen)))
      (should (equal (beads-live--stream-root stream) (nth 1 (car seen))))
      ;; A replayed seq is an echo and must not run the hook again.
      (beads-live--deliver
       stream (list (beads-live-test--parse-one 1 "create" "{\"id\":\"be-1\"}")))
      (should (= 3 (length seen))))))

(ert-deftest beads-live-test-event-hooks-error-is-isolated ()
  "A hook error does not stop the record from being applied."
  :tags '(:unit)
  (beads-live-test--with-fresh-live
    (let* ((stream (beads-live-test--stream))
           (model (beads-live-model stream))
           (beads-event-hooks (list (lambda (&rest _) (error "boom")))))
      (beads-live--deliver
       stream (list (beads-live-test--parse-one 1 "create" "{\"id\":\"be-1\"}")))
      (should (= 1 (length (beads-live--model-ring model)))))))

;;; WI-LIVE-09 — status and header

(ert-deftest beads-live-test-status-plist ()
  "Status returns the documented keys from in-memory state."
  :tags '(:unit)
  (beads-live-test--with-registry
    (let* ((root (make-temp-file "beads-live-store-" t))
           (stream (beads-live--stream-for root)))
      (setf (beads-live--stream-state stream) 'live
            (beads-live--stream-seq stream) 1047
            (beads-live--stream-activity stream) (list (float-time)))
      (let ((status (beads-live-status root)))
        (should (eq (plist-get status :state) 'live))
        (should (= (plist-get status :seq) 1047))
        (should (>= (plist-get status :rate) 1))
        (should (equal (plist-get status :mode) 'stream))
        (should (equal (beads-live--stream-name stream) (plist-get status :name))))
      (should-not (beads-live-status "/no/such/store/anywhere")))))

(ert-deftest beads-live-test-header-string-live ()
  "The live chip carries the rate and seq."
  :tags '(:unit)
  (beads-live-test--with-registry
    (let* ((root (make-temp-file "beads-live-store-" t))
           (stream (beads-live--stream-for root)))
      (setf (beads-live--stream-state stream) 'live
            (beads-live--stream-seq stream) 1047
            (beads-live--stream-activity stream) (list (float-time)))
      (let ((header (beads-live-header-string root)))
        (should (string-match-p "live" header))
        (should (string-match-p "1047" header))
        (should (string-match-p "∿" header))))))

(ert-deftest beads-live-test-header-string-states ()
  "The chip degrades to poll/partial/offline/reconnecting."
  :tags '(:unit)
  (beads-live-test--with-registry
    (let* ((root (make-temp-file "beads-live-store-" t))
           (stream (beads-live--stream-for root)))
      (setf (beads-live--stream-state stream) 'poll)
      (should (string-match-p "poll" (beads-live-header-string root)))
      (setf (beads-live--stream-state stream) 'partial
            (beads-live--stream-reason stream) "floor")
      (should (string-match-p "partial" (beads-live-header-string root)))
      (setf (beads-live--stream-state stream) 'offline)
      (should (string-match-p "offline" (beads-live-header-string root)))
      (setf (beads-live--stream-state stream) 'reconnecting
            (beads-live--stream-retry-at stream) (+ (float-time) 5))
      (should (string-match-p "reconnecting" (beads-live-header-string root)))
      (should-not (beads-live-header-string "/no/such/store/anywhere")))))

;;; WI-LIVE-09 — active-p and invalidation

(ert-deftest beads-live-test-active-p ()
  "Only a `live' or `poll' stream is active."
  :tags '(:unit)
  (beads-live-test--with-registry
    (let* ((root (make-temp-file "beads-live-store-" t))
           (stream (beads-live--stream-for root)))
      (setf (beads-live--stream-state stream) 'live)
      (should (beads-live-active-p root))
      (setf (beads-live--stream-state stream) 'poll)
      (should (beads-live-active-p root))
      (setf (beads-live--stream-state stream) 'connecting)
      (should-not (beads-live-active-p root))
      (setf (beads-live--stream-state stream) 'live
            (beads-live--stream-enabled stream) nil)
      (should-not (beads-live-active-p root))
      (should-not (beads-live-active-p "/no/such/store/anywhere")))))

(ert-deftest beads-live-test-invalidate-runs-hook-and-view ()
  "Invalidate runs the batch hook and the view refresh for matching kinds."
  :tags '(:unit)
  (beads-live-test--with-registry
    (let* ((root (make-temp-file "beads-live-store-" t))
           (buf (beads-live-test--view-buffer root))
           (refreshed 0)
           (hook (lambda (_root _kinds _ops) nil))
           (beads-live-invalidate-functions nil))
      (cl-letf (((symbol-function 'beads-live--start) (lambda (_stream) nil))
                ((symbol-function 'beads-live--running-p) (lambda (_stream) t)))
        (beads-live-attach buf
                           :refresh (lambda () (setq refreshed (1+ refreshed)))
                           :kinds '(show)))
      (setq beads-live-invalidate-functions (list hook))
      (beads-live-invalidate root '(show) '("update"))
      (should (= 1 refreshed))
      ;; A kind the view does not depend on does not refresh it.
      (beads-live-invalidate root '(dashboard) '("update"))
      (should (= 1 refreshed))
      ;; An `all' batch always refreshes.
      (beads-live-invalidate root 'all '("close"))
      (should (= 2 refreshed))
      (beads-live-test--kill buf))))

(ert-deftest beads-live-test-invalidate-no-stream-is-noop ()
  "Invalidate on a store with no stream does nothing and signals nothing."
  :tags '(:unit)
  (beads-live-test--with-registry
    (should-not (beads-live-invalidate "/no/such/store/anywhere" 'all '("close")))))

;;; WI-LIVE-09 — controls

(ert-deftest beads-live-test-toggle-control ()
  "Toggle disables and stops, then re-enables and starts."
  :tags '(:unit)
  (beads-live-test--with-registry
    (let* ((root (make-temp-file "beads-live-store-" t))
           (stream (beads-live--stream-for root))
           (stopped 0)
           (started 0))
      (cl-letf (((symbol-function 'beads-live--stop)
                 (lambda (s) (setq stopped (1+ stopped))
                   (beads-live--set-state s 'off nil)))
                ((symbol-function 'beads-live--start)
                 (lambda (_s) (setq started (1+ started)))))
        (beads-live-toggle root)
        (should-not (beads-live--stream-enabled stream))
        (should (= 1 stopped))
        (beads-live-toggle root)
        (should (beads-live--stream-enabled stream))
        (should (= 1 started))))))

(ert-deftest beads-live-test-reconnect-by-dir ()
  "The interactive reconnect resolves the covering stream."
  :tags '(:unit)
  (beads-live-test--with-registry
    (let* ((root (make-temp-file "beads-live-store-" t))
           (stream (beads-live--stream-for root))
           (started 0))
      (setf (beads-live--stream-state stream) 'offline
            (beads-live--stream-attempt stream) 3)
      (cl-letf (((symbol-function 'beads-live--start)
                 (lambda (_s) (setq started (1+ started))))
                ((symbol-function 'beads-live--stop-poll) (lambda (_s) nil)))
        (beads-live-reconnect root)
        (should (= 1 started))
        (should (= 0 (beads-live--stream-attempt stream)))
        (should (eq (beads-live--stream-state stream) 'connecting))))))

(ert-deftest beads-live-test-stop-all ()
  "Stop-all stops every registered stream and empties the table."
  :tags '(:unit)
  (beads-live-test--with-registry
    (let* ((root1 (make-temp-file "beads-live-store-" t))
           (root2 (make-temp-file "beads-live-store-" t))
           (_ (beads-live--stream-for root1))
           (_ (beads-live--stream-for root2))
           (stopped 0))
      (cl-letf (((symbol-function 'beads-live--stop)
                 (lambda (_s) (setq stopped (1+ stopped)))))
        (beads-live-stop-all)
        (should (= 2 stopped))
        (should (= 0 (hash-table-count beads-live--streams)))))))

(provide 'beads-live-test)
;;; beads-live-test.el ends here
