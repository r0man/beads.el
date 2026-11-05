;;; beads-pager-server-test.el --- Server-side paging tests -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; Unit tests for optional server-side list pagination
;; (`beads-store-offset-capable-p' and the `beads-pager' server mode).
;; bd is faked at `beads-command-execute', so no store is touched.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'beads-command-list)
(require 'beads-pager)
(require 'beads-spec)
(require 'beads-store)

;;; Fixtures

(defun beads-pager-server-test--issue (n)
  "Return a beads-issue object numbered N."
  (beads-issue
   :id (format "bd-%03d" n)
   :title (format "Issue %d" n)
   :status "open"
   :priority 2
   :issue-type "task"
   :created-at "2025-01-01T00:00:00Z"
   :updated-at "2025-01-01T00:00:00Z"))

(defun beads-pager-server-test--issues (from to)
  "Return issues numbered FROM through TO inclusive."
  (cl-loop for n from from to to
           collect (beads-pager-server-test--issue n)))

(defmacro beads-pager-server-test--with-list (&rest body)
  "Run BODY in a flat beads-list buffer with a fixed page size of 3."
  (declare (indent 0))
  `(let ((beads-list-group-by-status nil)
         (beads-store--backend-cache (make-hash-table :test #'equal)))
     (with-temp-buffer
       (beads-list-mode)
       (setq beads-pager--page-size 3)
       ,@body)))

(defun beads-pager-server-test--calls-in-order (calls)
  "Return CALLS oldest first."
  (nreverse (copy-sequence calls)))

(defmacro beads-pager-server-test--with-fake-bd (data &rest body)
  "Run BODY with bd faked over DATA, a list of beads-issue objects.
The fake honours the command's offset and limit, records each request
in the lexical variable `calls', and reports the store as offset
capable."
  (declare (indent 1))
  `(let ((calls nil)
         (dataset ,data))
     (cl-letf (((symbol-function 'beads-store-offset-capable-p)
                (lambda (&optional _directory) t))
               ((symbol-function 'beads-command-execute)
                (lambda (command)
                  (let* ((offset (or (oref command offset) 0))
                         (requested (oref command limit))
                         (limit (if (and (integerp requested)
                                         (> requested 0))
                                    requested
                                  (length dataset))))
                    (push (list :offset offset
                                :limit requested
                                :status (oref command status)
                                :type (oref command issue-type)
                                :priority (oref command priority)
                                :label (oref command label)
                                :all (oref command all))
                          calls)
                    (cl-subseq dataset
                               (min offset (length dataset))
                               (min (+ offset limit) (length dataset)))))))
       ,@body)))

(defun beads-pager-server-test--visible-ids ()
  "Return the issue ids displayed in the current list buffer."
  (mapcar #'car tabulated-list-entries))

;;; Capability detection and cache

(ert-deftest beads-pager-server-test-capability-cached-per-store ()
  "A store is probed once; later checks read the cached answer."
  :tags '(:unit)
  (let ((calls 0)
        (beads-store--backend-cache (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'beads-store--context-mode)
               (lambda (_store)
                 (cl-incf calls)
                 "proxied-server")))
      (should (beads-store-offset-capable-p "/tmp/store-a/"))
      (should (beads-store-offset-capable-p "/tmp/store-a/"))
      (should (= calls 1)))))

(ert-deftest beads-pager-server-test-capability-negative-cached ()
  "A non-proxied store is also cached, so it is not re-probed."
  :tags '(:unit)
  (let ((calls 0)
        (beads-store--backend-cache (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'beads-store--context-mode)
               (lambda (_store)
                 (cl-incf calls)
                 "embedded")))
      (should-not (beads-store-offset-capable-p "/tmp/store-b/"))
      (should-not (beads-store-offset-capable-p "/tmp/store-b/"))
      (should (= calls 1)))))

(ert-deftest beads-pager-server-test-metadata-fallback ()
  "metadata.json answers when `bd context' is unavailable."
  :tags '(:unit)
  (let ((dir (make-temp-file "beads-store-mode-" t))
        (beads-store--backend-cache (make-hash-table :test #'equal)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".beads" dir) t)
          (with-temp-file (expand-file-name ".beads/metadata.json" dir)
            (insert "{\"database\":\"dolt\",\"dolt_mode\":\"proxied-server\"}"))
          (cl-letf (((symbol-function 'beads-store--context-mode)
                     (lambda (_store) nil))
                    ((symbol-function 'beads-store--probe-mode)
                     (lambda (_store)
                       (ert-fail "probe must not run when metadata answers"))))
            (should (beads-store-offset-capable-p dir))))
      (delete-directory dir t))))

(ert-deftest beads-pager-server-test-probe-is-last-resort-and-cached ()
  "Without `bd context' or metadata, one probe decides and is cached."
  :tags '(:unit)
  (let ((dir (make-temp-file "beads-store-mode-" t))
        (probes 0)
        (beads-store--backend-cache (make-hash-table :test #'equal)))
    (unwind-protect
        (cl-letf (((symbol-function 'beads-store--context-mode)
                   (lambda (_store) nil))
                  ((symbol-function 'beads-store--probe-mode)
                   (lambda (_store)
                     (cl-incf probes)
                     "proxied-server")))
          (should (beads-store-offset-capable-p dir))
          (should (beads-store-offset-capable-p dir))
          (should (= probes 1)))
      (delete-directory dir t))))

(ert-deftest beads-pager-server-test-probe-rejects-embedded-store ()
  "The last-resort probe must not call an embedded store proxied.
Embedded `bd' accepts `--offset 0' and rejects every non-zero offset,
so a zero-offset probe would misclassify the store as server-capable."
  :tags '(:unit)
  (let ((dir (make-temp-file "beads-store-mode-" t))
        (probed-offsets nil)
        (beads-store--backend-cache (make-hash-table :test #'equal)))
    (unwind-protect
        (cl-letf (((symbol-function 'beads-store--context-mode)
                   (lambda (_store) nil))
                  ((symbol-function 'beads-store--metadata-mode)
                   (lambda (_store) nil))
                  ((symbol-function 'beads-command-execute)
                   (lambda (command)
                     (let ((offset (oref command offset)))
                       (push offset probed-offsets)
                       (if (and (integerp offset) (> offset 0))
                           (error "--offset is only supported under --proxied-server")
                         (list (beads-pager-server-test--issue 1)))))))
          (should-not (beads-store-offset-capable-p dir))
          (should (equal probed-offsets '(1)))
          ;; The negative answer is cached; no second probe.
          (should-not (beads-store-offset-capable-p dir))
          (should (equal probed-offsets '(1))))
      (delete-directory dir t))))

(ert-deftest beads-pager-server-test-probe-accepts-proxied-store ()
  "The last-resort probe reports a route that honours a non-zero offset."
  :tags '(:unit)
  (let ((dir (make-temp-file "beads-store-mode-" t))
        (probed-offsets nil)
        (beads-store--backend-cache (make-hash-table :test #'equal)))
    (unwind-protect
        (cl-letf (((symbol-function 'beads-store--context-mode)
                   (lambda (_store) nil))
                  ((symbol-function 'beads-store--metadata-mode)
                   (lambda (_store) nil))
                  ((symbol-function 'beads-command-execute)
                   (lambda (command)
                     (push (oref command offset) probed-offsets)
                     (list (beads-pager-server-test--issue 2)))))
          (should (beads-store-offset-capable-p dir))
          (should (equal probed-offsets '(1))))
      (delete-directory dir t))))

(ert-deftest beads-pager-server-test-forget-cache-reprobes ()
  "`beads-store-forget-cache' drops one store's cached answer."
  :tags '(:unit)
  (let ((calls 0)
        (mode "proxied-server")
        (beads-store--backend-cache (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'beads-store--context-mode)
               (lambda (_store)
                 (cl-incf calls)
                 mode)))
      (should (beads-store-offset-capable-p "/tmp/store-c/"))
      (should (= calls 1))
      (beads-store-forget-cache "/tmp/store-c/")
      (setq mode "embedded")
      (should-not (beads-store-offset-capable-p "/tmp/store-c/"))
      (should (= calls 2)))))

(ert-deftest beads-pager-server-test-context-wins-over-metadata ()
  "A definitive `bd context' answer stops the fallback chain."
  :tags '(:unit)
  (let ((dir (make-temp-file "beads-store-mode-" t))
        (beads-store--backend-cache (make-hash-table :test #'equal))
        (metadata-called nil))
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".beads" dir) t)
          (with-temp-file (expand-file-name ".beads/metadata.json" dir)
            (insert "{\"dolt_mode\":\"proxied-server\"}"))
          (cl-letf (((symbol-function 'beads-store--context-mode)
                     (lambda (_store) "embedded"))
                    ((symbol-function 'beads-store--metadata-mode)
                     (lambda (_store)
                       (setq metadata-called t)
                       "proxied-server")))
            (should-not (beads-store-offset-capable-p dir))
            (should-not metadata-called)))
      (delete-directory dir t))))

;;; List integration: server-side paging

(ert-deftest beads-pager-server-test-first-page-uses-offset ()
  "A capable store fetches page 1 with --limit PAGE --offset 0."
  :tags '(:unit)
  (beads-pager-server-test--with-fake-bd
      (beads-pager-server-test--issues 1 7)
    (beads-pager-server-test--with-list
      (beads-list--refresh (beads-issue-spec :limit 0))
      (should (beads-list--server-paging-p))
      (should (equal (beads-pager-server-test--visible-ids)
                     '("bd-001" "bd-002" "bd-003")))
      (let ((call (car (beads-pager-server-test--calls-in-order calls))))
        (should (= (plist-get call :offset) 0))
        (should (= (plist-get call :limit) 3))))))

(ert-deftest beads-pager-server-test-next-prev-and-short-last-page ()
  "Next/prev fetch adjacent pages; a short page ends the sequence."
  :tags '(:unit)
  (beads-pager-server-test--with-fake-bd
      (beads-pager-server-test--issues 1 7)
    (beads-pager-server-test--with-list
      (beads-list--refresh (beads-issue-spec :limit 0))
      (should beads-pager--server-has-next)
      ;; Page 2.
      (beads-pager-next-page)
      (should (= beads-pager--page 2))
      (should (equal (beads-pager-server-test--visible-ids)
                     '("bd-004" "bd-005" "bd-006")))
      (should beads-pager--server-has-next)
      ;; Page 3 is short: one row, no next.
      (beads-pager-next-page)
      (should (= beads-pager--page 3))
      (should (equal (beads-pager-server-test--visible-ids) '("bd-007")))
      (should-not beads-pager--server-has-next)
      ;; At the end, next does not fetch.
      (let ((before (length calls)))
        (beads-pager-next-page)
        (should (= beads-pager--page 3))
        (should (= (length calls) before)))
      ;; Back to page 2, then page 1.
      (beads-pager-prev-page)
      (should (= beads-pager--page 2))
      (should (equal (beads-pager-server-test--visible-ids)
                     '("bd-004" "bd-005" "bd-006")))
      (beads-pager-prev-page)
      (should (= beads-pager--page 1))
      (should (equal (beads-pager-server-test--visible-ids)
                     '("bd-001" "bd-002" "bd-003"))))))

(ert-deftest beads-pager-server-test-filter-preserved-with-offset ()
  "Filters stay on the command for every server page."
  :tags '(:unit)
  (beads-pager-server-test--with-fake-bd
      (beads-pager-server-test--issues 1 7)
    (beads-pager-server-test--with-list
      (beads-list--refresh
       (beads-issue-spec :status "open" :type "bug" :priority 1
                         :label "perf" :all t :limit 0))
      (beads-pager-next-page)
      (let ((call (car (last (beads-pager-server-test--calls-in-order
                              calls)))))
        (should (= (plist-get call :offset) 3))
        (should (= (plist-get call :limit) 3))
        (should (equal (plist-get call :status) "open"))
        (should (equal (plist-get call :type) "bug"))
        (should (equal (plist-get call :priority) "1"))
        (should (equal (plist-get call :label) '("perf")))
        (should (plist-get call :all))))))

(ert-deftest beads-pager-server-test-overall-limit-caps-last-page ()
  "The spec limit is an overall cap, not a per-page size."
  :tags '(:unit)
  (beads-pager-server-test--with-fake-bd
      (beads-pager-server-test--issues 1 7)
    (beads-pager-server-test--with-list
      (beads-list--refresh (beads-issue-spec :limit 5))
      ;; Page 1: 3 rows, 2 remaining under the cap.
      (should beads-pager--server-has-next)
      (beads-pager-next-page)
      (should (= beads-pager--page 2))
      (should (equal (beads-pager-server-test--visible-ids)
                     '("bd-004" "bd-005")))
      (should-not beads-pager--server-has-next)
      (let ((calls* (beads-pager-server-test--calls-in-order calls)))
        (should (= (plist-get (nth 1 calls*) :offset) 3))
        (should (= (plist-get (nth 1 calls*) :limit) 2))))))

(ert-deftest beads-pager-server-test-refresh-resets-page ()
  "An explicit refresh reloads page 1; a silent one keeps the page."
  :tags '(:unit)
  (beads-pager-server-test--with-fake-bd
      (beads-pager-server-test--issues 1 7)
    (beads-pager-server-test--with-list
      (beads-list--refresh (beads-issue-spec :limit 0))
      (beads-pager-next-page)
      (should (= beads-pager--page 2))
      ;; Silent refresh (live/hook) keeps the current page.
      (beads-list-refresh t)
      (should (= beads-pager--page 2))
      (should (= (plist-get (car (last (beads-pager-server-test--calls-in-order
                                        calls)))
                            :offset)
                 3))
      ;; Explicit refresh resets to page 1.
      (beads-list-refresh)
      (should (= beads-pager--page 1))
      (should (= (plist-get (car (last (beads-pager-server-test--calls-in-order
                                        calls)))
                            :offset)
                 0)))))

(ert-deftest beads-pager-server-test-dedupes-shifted-row ()
  "A row repeated by an offset shift is dropped from the later page."
  :tags '(:unit)
  (beads-pager-server-test--with-list
    (let ((calls nil)
          (pages '((0 . (1 2 3))
                   (3 . (3 4 5))
                   (6 . (6 7)))))
      (cl-letf (((symbol-function 'beads-store-offset-capable-p)
                 (lambda (&optional _directory) t))
                ((symbol-function 'beads-command-execute)
                 (lambda (command)
                   (push (oref command offset) calls)
                   (mapcar #'beads-pager-server-test--issue
                           (cdr (assq (oref command offset) pages))))))
        (beads-list--refresh (beads-issue-spec :limit 0))
        (should (equal (beads-pager-server-test--visible-ids)
                       '("bd-001" "bd-002" "bd-003")))
        ;; Page 2 repeats bd-003 because an insert shifted the window.
        (beads-pager-next-page)
        (should (equal (beads-pager-server-test--visible-ids)
                       '("bd-004" "bd-005")))
        ;; Page 3 completes the set without a duplicate.
        (beads-pager-next-page)
        (should (equal (beads-pager-server-test--visible-ids)
                       '("bd-006" "bd-007")))
        (should (= (length calls) 3))))))

;;; Direct backend keeps the client-side pager

(ert-deftest beads-pager-server-test-direct-backend-unchanged ()
  "A non-capable store fetches everything and slices client-side."
  :tags '(:unit)
  (let ((calls nil)
        (dataset (beads-pager-server-test--issues 1 7)))
    (cl-letf (((symbol-function 'beads-store-offset-capable-p)
               (lambda (&optional _directory) nil))
              ((symbol-function 'beads-command-execute)
               (lambda (command)
                 (push (list :offset (oref command offset)
                             :limit (oref command limit))
                       calls)
                 dataset)))
      (beads-pager-server-test--with-list
        (beads-list--refresh (beads-issue-spec :limit 0))
        (should-not (beads-list--server-paging-p))
        (should (= (length beads-pager--all-entries) 7))
        (should (= (length tabulated-list-entries) 3))
        (should (equal (beads-pager-server-test--visible-ids)
                       '("bd-001" "bd-002" "bd-003")))
        (should (= (length calls) 1))
        (should-not (plist-get (car calls) :offset))))))

;;; `--sort id' never uses offset

(ert-deftest beads-pager-server-test-sort-id-excluded ()
  "Offset is never combined with `--sort id'."
  :tags '(:unit)
  (beads-pager-server-test--with-list
    (cl-letf (((symbol-function 'beads-store-offset-capable-p)
               (lambda (&optional _directory) t)))
      (should (beads-list--server-paging-supported-p
               (beads-command-list :sort "created")))
      (should-not (beads-list--server-paging-supported-p
                   (beads-command-list :sort "id"))))))

;;; Pager indicators

(ert-deftest beads-pager-server-test-mode-line-indicators ()
  "Server pages show a page/range indicator and a `more' marker."
  :tags '(:unit)
  (beads-pager-server-test--with-fake-bd
      (beads-pager-server-test--issues 1 7)
    (beads-pager-server-test--with-list
      (beads-list--refresh (beads-issue-spec :limit 0))
      (let ((frag (beads-pager--mode-line-fragment)))
        (should (string-match-p "Page 1" frag))
        (should (string-match-p "items 1-3" frag))
        (should (string-match-p "(more)" frag)))
      (beads-pager-next-page)
      (beads-pager-next-page)
      (let ((frag (beads-pager--mode-line-fragment)))
        (should (string-match-p "Page 3" frag))
        (should (string-match-p "items 7-7" frag))
        (should-not (string-match-p "(more)" frag))))))

;;; Window shrink and dedupe

(ert-deftest beads-pager-server-test-shrink-refetch-keeps-page-rows ()
  "A page-size shrink refetches without dropping every overlapping row.
Opening a transient shrinks the window, so the resize hook refetches the
current page at the minimum page size.  Rows that the smaller window
overlaps from an earlier page must not be deduped away, leaving the page
empty."
  :tags '(:unit)
  (beads-pager-server-test--with-fake-bd
      (beads-pager-server-test--issues 1 40)
    (beads-pager-server-test--with-list
      (setq beads-pager--page-size 12)
      (beads-list--refresh (beads-issue-spec :limit 0))
      (beads-pager-next-page)
      (should (equal (beads-pager-server-test--visible-ids)
                     (mapcar (lambda (n) (format "bd-%03d" n))
                             (number-sequence 13 24))))
      ;; The window shrinks to the minimum page size.
      (setq beads-pager--page-size 5)
      (beads-pager--apply)
      (should (equal (beads-pager-server-test--visible-ids)
                     (mapcar (lambda (n) (format "bd-%03d" n))
                             (number-sequence 6 10))))
      (let ((frag (beads-pager--mode-line-fragment)))
        (should-not (string-match-p "no items" frag))
        (should (string-match-p "items 6-10" frag))))))

(ert-deftest beads-pager-server-test-range-follows-kept-rows ()
  "The mode-line range spans the raw rows kept, not offset plus count.
An insert shift can repeat an earlier page's row mid-page; dropping that
duplicate must not pull the reported end inward."
  :tags '(:unit)
  (beads-pager-server-test--with-list
    (let ((pages '((0 . (1 2 3))
                   (3 . (4 3 5))
                   (6 . (6 7)))))
      (cl-letf (((symbol-function 'beads-store-offset-capable-p)
                 (lambda (&optional _directory) t))
                ((symbol-function 'beads-command-execute)
                 (lambda (command)
                   (mapcar #'beads-pager-server-test--issue
                           (cdr (assq (oref command offset) pages))))))
        (beads-list--refresh (beads-issue-spec :limit 0))
        (beads-pager-next-page)
        (should (equal (beads-pager-server-test--visible-ids)
                       '("bd-004" "bd-005")))
        ;; bd-003 was dropped at raw item 5; the kept rows are items 4 and 6.
        (let ((frag (beads-pager--mode-line-fragment)))
          (should (string-match-p "items 4-6" frag)))))))

(provide 'beads-pager-server-test)
;;; beads-pager-server-test.el ends here
