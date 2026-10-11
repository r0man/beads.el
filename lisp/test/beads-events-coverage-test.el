;;; beads-events-coverage-test.el --- Extra coverage for the events views -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; Additional `:unit' coverage for `beads-events.el': the pure-model
;; fallbacks (with the `beads-event' accessors unbound), the client-side
;; filter matcher, city/session columns, live attach/teardown, the
;; interactive filter/visit/toggle commands, Org export, and the two
;; timeline entry points.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'beads-event)
(require 'beads-types)
(require 'beads-events)

;;; Helpers

(defun beads-events-cov--issue (&rest args)
  "Build a `beads-issue' for tests from ARGS."
  (apply #'beads-issue :id "be-1" :title "A title" :status "open" args))

(defun beads-events-cov--record (seq op issue-id actor &rest args)
  "Build a `beads-event-record' at SEQ with OP, ISSUE-ID, ACTOR, ARGS."
  (apply #'beads-event-record
         :seq seq
         :ts (format-time-string "%Y-%m-%dT%H:%M:%SZ" (+ seq 1600000000) t)
         :op op
         :issue-id issue-id
         :actor actor
         args))

;;; Pure-model fallbacks

(defmacro beads-events-cov--without-model (&rest body)
  "Run BODY with the pure `beads-event' accessors unbound."
  (declare (indent 0))
  `(cl-letf (((symbol-function 'beads-event-level) nil)
             ((symbol-function 'beads-event-glyph) nil)
             ((symbol-function 'beads-event-subject) nil)
             ((symbol-function 'beads-event-actor-description) nil)
             ((symbol-function 'beads-event-time) nil)
             ((symbol-function 'beads-event-window-seconds) nil)
             ((symbol-function 'beads-event-fold) nil))
     ,@body))

(ert-deftest beads-events-cov-fallback-accessors-test ()
  "Unit test: every fallback accessor maps a raw record locally."
  :tags '(:unit)
  (beads-events-cov--without-model
   (let ((close (beads-events-cov--record 1 "close" "be-1" nil))
         (update (beads-events-cov--record 2 "update" "be-1" "alice"))
         (comment (beads-events-cov--record 3 "comment" "" nil)))
     (should (eq (beads-events--level close) 'attention))
     (should (eq (beads-events--level update) 'watch))
     (should-not (beads-events--level comment))
     (should (equal (beads-events--glyph close) "■"))
     (should (equal (beads-events--glyph update) "▲"))
     (should (equal (beads-events--glyph comment) " "))
     (should (equal (beads-events--subject close) "be-1 — close"))
     (should (equal (beads-events--subject comment) "comment"))
     (should (equal (beads-events--actor update) "alice"))
     (should (equal (beads-events--actor close) "system"))
     (should (> (beads-events--time close) 0))
     (should (= (beads-events--time
                 (beads-event-record :seq 1 :op "x" :ts nil))
                0))
     (should (= (beads-events--window-seconds 12) 12))
     (should (= (beads-events--window-seconds "12") 12))
     (should (= (beads-events--window-seconds "5m") 300))
     (should (= (beads-events--window-seconds "2h") 7200))
     (should (= (beads-events--window-seconds "1d") 86400))
     (should (= (beads-events--window-seconds "1w") 604800))
     (should (= (beads-events--window-seconds "bogus") 3600)))))

(ert-deftest beads-events-cov-fallback-fold-test ()
  "Unit test: the fallback fold coalesces plain ops and keeps signals."
  :tags '(:unit)
  (beads-events-cov--without-model
   (let* ((records (list (beads-events-cov--record 1 "comment" "be-1" "a")
                         (beads-events-cov--record 2 "comment" "be-1" "b")
                         (beads-events-cov--record 3 "close" "be-2" "c")))
          (folded (beads-events--fallback-fold records 900)))
     ;; One churn row for the two comments plus the signal row.
     (should (= 2 (length (car folded))))
     (should (= 2 (cdr folded)))
     (should (= 1 (length (cl-remove-if-not (lambda (row) (eq (car row) 'churn))
                                            (car folded))))))))

;;; Filtering

(ert-deftest beads-events-cov-match-p-test ()
  "Unit test: each client-side filter clause is honored."
  :tags '(:unit)
  (let ((record (beads-events-cov--record 1 "close" "be-1" "alice"
                                          :issue (beads-events-cov--issue
                                                  :id "be-1" :title "Fix it"))))
    (should (beads-events--match-p record nil))
    (should (beads-events--match-p record '(:op "clo")))
    (should-not (beads-events--match-p record '(:op "comment")))
    (should (beads-events--match-p record '(:actor "ali")))
    (should-not (beads-events--match-p record '(:actor "bob")))
    (should (beads-events--match-p record '(:issue "be-1")))
    (should-not (beads-events--match-p record '(:issue "be-2")))
    (should (beads-events--match-p record '(:level "watch")))
    (should (beads-events--match-p record '(:level "attention")))
    (should (beads-events--match-p record '(:search "fix")))
    (should-not (beads-events--match-p record '(:search "nope")))))

(ert-deftest beads-events-cov-model-records-test ()
  "Unit test: the guarded model-ring read returns nil or the ring."
  :tags '(:unit)
  ;; No beads-live loaded: nil.
  (should-not (beads-events--model-records nil))
  (cl-letf (((symbol-function 'beads-live-model) (lambda (_) 'model))
            ((symbol-function 'beads-live--model-ring) (lambda (_) '(a b))))
    (should (equal (beads-events--model-records 'stream) '(a b))))
  ;; A signaling ring accessor is swallowed.
  (cl-letf (((symbol-function 'beads-live-model) (lambda (_) 'model))
            ((symbol-function 'beads-live--model-ring)
             (lambda (_) (error "nope"))))
    (should-not (beads-events--model-records 'stream))))

(ert-deftest beads-events-cov-read-store-records-test ()
  "Unit test: store records come from the injected reader."
  :tags '(:unit)
  (let ((beads-events-records-function (lambda (_) '(r1 r2))))
    (should (equal (beads-events--read-store-records "/tmp/s") '(r1 r2))))
  (let ((beads-events-records-function (lambda (_) (error "boom"))))
    (should-not (beads-events--read-store-records "/tmp/s")))
  (let ((beads-events-records-function nil))
    (should-not (beads-events--read-store-records "/tmp/s"))))

;;; Columns

(ert-deftest beads-events-cov-columns-with-city-and-session-test ()
  "Unit test: city and session columns extend the row."
  :tags '(:unit)
  (let* ((record (beads-events-cov--record 1 "close" "be-1" "alice"))
         (store-of (make-hash-table :test 'eq))
         (beads-events--city "cov-city")
         (beads-events--store-of store-of)
         (beads-events--has-session t)
         (beads-events-session-table '(("alice" . "sess-1"))))
    (puthash record "/tmp/store" store-of)
    (let ((entry (beads-events--record-entry record (float-time))))
      ;; Store, Time, Sig, Op, Issue, Actor, Session
      (should (= 7 (length (cadr entry)))))
    (let ((cols (cadr (beads-events--record-entry record (float-time) t))))
      (should (string-match-p "close" (aref cols 3))))
    (let* ((row (list 'churn "k" "be-1 close" (float-time) (list record)))
           (entry (beads-events--churn-entry row (float-time))))
      (should (= 7 (length (cadr entry)))))
    (should (beads-events--session-cell record))))

(ert-deftest beads-events-cov-header-line-branches-test ()
  "Unit test: the header line shows folded churn and the live chip."
  :tags '(:unit)
  (with-temp-buffer
    (beads-events-mode)
    (setq beads-events--store-name "mystore"
          beads-events--shown 3
          beads-events--folded 2
          beads-events--filter '(:op "close"))
    (cl-letf (((symbol-function 'beads-live-header-string)
               (lambda (_) "LIVE")))
      (let ((line (beads-events--header-line)))
        (should (string-match-p "mystore" line))
        (should (string-match-p "churn folded" line))
        (should (string-match-p "LIVE" line))
        (should (string-match-p "op=close" line))))))

;;; Live integration

(ert-deftest beads-events-cov-live-attach-teardown-test ()
  "Unit test: attach wires the stream/handle and teardown clears them."
  :tags '(:unit)
  (with-temp-buffer
    (beads-events-mode)
    (require 'beads-live)
    (let ((beads-events--live-inhibit nil))
      (cl-letf (((symbol-function 'beads-live-attach) (lambda (&rest _) 'stream))
                ((symbol-function 'beads-live-subscribe) (lambda (&rest _) 'handle)))
        (should (eq (beads-events--live-attach) 'stream))
        (should (eq beads-events--stream 'stream))
        (should (eq beads-events--handle 'handle))))
    (let ((detached nil) (unsubscribed nil))
      (cl-letf (((symbol-function 'beads-live-detach)
                 (lambda (_) (setq detached t)))
                ((symbol-function 'beads-live-unsubscribe)
                 (lambda (_) (setq unsubscribed t))))
        (beads-events--teardown)
        (should detached)
        (should unsubscribed)
        (should-not beads-events--stream)
        (should-not beads-events--handle)))))

(ert-deftest beads-events-cov-live-refresh-test ()
  "Unit test: the debounced live refresh re-renders the view."
  :tags '(:unit)
  (with-temp-buffer
    (beads-events-mode)
    (let ((refreshed 0))
      (cl-letf (((symbol-function 'beads-events-refresh)
                 (lambda () (setq refreshed (1+ refreshed)))))
        (beads-events--live-refresh))
      (should (= refreshed 1)))
    (with-temp-buffer
      (beads-events--live-refresh))))

;;; Commands

(ert-deftest beads-events-cov-refresh-guard-test ()
  "Unit test: refresh requires an Events buffer."
  :tags '(:unit)
  (with-temp-buffer
    (should-error (beads-events-refresh) :type 'user-error))
  (with-temp-buffer
    (beads-events-mode)
    (beads-events-refresh)))

(ert-deftest beads-events-cov-prompt-and-filter-test ()
  "Unit test: filter prompting builds a plist and empty answers clear."
  :tags '(:unit)
  (with-temp-buffer
    (beads-events-mode)
    (setq beads-events--records (list (beads-events-cov--record
                                       1 "close" "be-1" "alice")))
    (cl-letf (((symbol-function 'beads-events-refresh) #'ignore))
      ;; Interactive: prompts for each field.
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) "close")))
        (setq beads-events--filter '(:op "create"))
        (beads-events-filter)
        (should (equal (plist-get beads-events--filter :op) "close")))
      ;; Empty answers clear the filter.
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "")))
        (setq beads-events--filter '(:op "close"))
        (beads-events-filter)
        (should-not beads-events--filter))
      ;; From Lisp: the plist replaces the filter.
      (beads-events-filter '(:op "comment"))
      (should (equal (plist-get beads-events--filter :op) "comment"))
      (beads-events-clear-filter)
      (should-not beads-events--filter)))
  (with-temp-buffer
    (should-error (beads-events-filter) :type 'user-error)))

(ert-deftest beads-events-cov-toggle-churn-test ()
  "Unit test: toggling a churn row expands and collapses it."
  :tags '(:unit)
  (with-temp-buffer
    (beads-events-mode)
    (let ((row (list 'churn "key" "group" (float-time) nil)))
      (cl-letf (((symbol-function 'tabulated-list-get-id) (lambda () row))
                ((symbol-function 'beads-events--render) #'ignore))
        (setq beads-events--expanded nil)
        (beads-events--toggle-churn)
        (should (member "key" beads-events--expanded))
        (beads-events--toggle-churn)
        (should-not (member "key" beads-events--expanded))))))

(ert-deftest beads-events-cov-record-at-point-test ()
  "Unit test: the record at point handles records and churn rows."
  :tags '(:unit)
  (with-temp-buffer
    (beads-events-mode)
    (let ((record (beads-events-cov--record 1 "close" "be-1" "a")))
      (cl-letf (((symbol-function 'tabulated-list-get-id) (lambda () record)))
        (should (eq (beads-events--record-at-point) record)))
      (cl-letf (((symbol-function 'tabulated-list-get-id)
                 (lambda () (list 'churn "k" "g" 0 (list record)))))
        (should (eq (beads-events--record-at-point) record))))))

(ert-deftest beads-events-cov-toggle-live-test ()
  "Unit test: toggling live delegates or signals when unavailable."
  :tags '(:unit)
  (with-temp-buffer
    (beads-events-mode)
    (cl-letf (((symbol-function 'beads-live-toggle) (lambda (_store) 'toggled)))
      (should (eq (beads-events-toggle-live) 'toggled))))
  (with-temp-buffer
    (beads-events-mode)
    (cl-letf (((symbol-function 'beads-live-toggle) nil))
      (should-error (beads-events-toggle-live) :type 'user-error))))

(ert-deftest beads-events-cov-visit-test ()
  "Unit test: visiting an event opens its issue or signals."
  :tags '(:unit)
  (with-temp-buffer
    (beads-events-mode)
    (cl-letf (((symbol-function 'tabulated-list-get-id) (lambda () nil)))
      (should-error (beads-events-visit) :type 'user-error))
    (cl-letf (((symbol-function 'tabulated-list-get-id)
               (lambda () (beads-events-cov--record 1 "close" nil "a"))))
      (should-error (beads-events-visit) :type 'user-error))
    (let ((shown nil))
      (require 'beads-command-show)
      (cl-letf (((symbol-function 'tabulated-list-get-id)
                 (lambda () (beads-events-cov--record 1 "close" "be-1" "a")))
                ((symbol-function 'beads-show) (lambda (id) (setq shown id))))
        (beads-events-visit)
        (should (equal shown "be-1"))))))

;;; Org export

(ert-deftest beads-events-cov-org-export-test ()
  "Unit test: Org export writes a table buffer for the filtered view."
  :tags '(:unit)
  (with-temp-buffer
    (beads-events-mode)
    (setq beads-events--records (list (beads-events-cov--record
                                       1 "close" "be-1" "alice"))
          beads-events--store-name "covstore"
          beads-events--filter nil)
    (cl-letf (((symbol-function 'pop-to-buffer) (lambda (buf &rest _) buf)))
      (let ((buf (beads-events-org-export)))
        (unwind-protect
            (with-current-buffer buf
              (should (string-match-p "#\\+TITLE: covstore events" (buffer-string)))
              (should (string-match-p "| Seq |" (buffer-string))))
          (kill-buffer buf)))))
  (with-temp-buffer
    (should-error (beads-events-org-export) :type 'user-error)))

;;; Entry points

(ert-deftest beads-events-cov-configure-and-names-test ()
  "Unit test: buffer/config naming helpers."
  :tags '(:unit)
  (should (equal (beads-events--store-name "/tmp/city") "city"))
  (should-not (beads-events--store-name nil))
  (should-not (beads-events--store-name ""))
  (should (equal (beads-events--buffer-name "/tmp/city" nil)
                 "*beads-events[city]*"))
  (should (equal (beads-events--buffer-name nil nil) "*beads-events[beads]*"))
  (should (equal (beads-events--buffer-name nil "mycity")
                 "*beads-events-city[mycity]*"))
  (with-temp-buffer
    (beads-events--configure "/tmp/city" nil)
    (should (equal beads-events--store "/tmp/city"))
    (should (equal beads-events--store-name "city"))
    (beads-events--configure nil "mycity")
    (should (equal beads-events--city "mycity"))
    (should (equal beads-events--store-name "mycity"))))

(ert-deftest beads-events-cov-timeline-test ()
  "Unit test: the timeline entry point reuses a store buffer."
  :tags '(:unit)
  (cl-letf (((symbol-function 'beads-store-resolve)
             (lambda (_) "/tmp/covstore"))
            ((symbol-function 'pop-to-buffer) (lambda (buf &rest _) buf)))
    (let ((buf (beads-events-timeline)))
      (unwind-protect
          (with-current-buffer buf
            (should (derived-mode-p 'beads-events-mode))
            (should (equal beads-events--store "/tmp/covstore")))
        (kill-buffer buf)))))

(ert-deftest beads-events-cov-timeline-city-test ()
  "Unit test: the city timeline merges stores and sets session state."
  :tags '(:unit)
  (let ((beads-events-city-stores
         (list (cons "/tmp/s1" (list (beads-events-cov--record 1 "close" "be-1" "a")))
               (cons "/tmp/s2" (list (beads-events-cov--record 2 "comment" "be-2" "b"))))))
    (cl-letf (((symbol-function 'pop-to-buffer) (lambda (buf &rest _) buf)))
      (let ((buf (beads-events-timeline-city "covcity")))
        (unwind-protect
            (with-current-buffer buf
              (should (derived-mode-p 'beads-events-mode))
              (should (equal beads-events--city "covcity"))
              (should (= (length beads-events--records) 2))
              (should beads-events--store-of))
          (kill-buffer buf))))))

;;; Notifications

(ert-deftest beads-events-cov-notify-accessors-test ()
  "Unit test: notification accessors tolerate alists and junk."
  :tags '(:unit)
  (let ((record (beads-events-cov--record
                 1 "close" "be-1" "a"
                 :issue (beads-events-cov--issue :id "be-1" :title "Fix it"))))
    (should (equal (beads-events-notify--op record) "close"))
    (should (equal (beads-events-notify--issue-id record) "be-1"))
    (should (equal (beads-events-notify--title record) "be-1 Fix it")))
  (let ((alist '((op . "create") (issue-id . "be-2"))))
    (should (equal (beads-events-notify--op alist) "create"))
    (should (equal (beads-events-notify--issue-id alist) "be-2"))
    (should (equal (beads-events-notify--title alist) "be-2")))
  (should-not (beads-events-notify--op 42))
  (should-not (beads-events-notify--issue-id 42))
  (should (equal (beads-events-notify--title 42) "bead")))

(ert-deftest beads-events-cov-notify-display-test ()
  "Unit test: desktop notifications fall back to `message' in batch."
  :tags '(:unit)
  (let ((messages nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
      (beads-events-notify--display "Title" "Body")
      (should (member "Title: Body" messages)))))

(provide 'beads-events-coverage-test)
;;; beads-events-coverage-test.el ends here
