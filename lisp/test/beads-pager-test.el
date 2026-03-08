;;; beads-pager-test.el --- Tests for beads-pager.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; Tests for beads-pager-mode pagination minor mode.

;;; Code:

(require 'beads-pager)
(require 'ert)

;;; Helpers

(defmacro beads-pager-test--with-buffer (&rest body)
  "Create a temp buffer with tabulated-list-mode and beads-pager-mode, run BODY."
  (declare (indent 0))
  `(with-temp-buffer
     (tabulated-list-mode)
     (beads-pager-mode 1)
     ,@body))

(defun beads-pager-test--make-entries (n)
  "Return a list of N dummy tabulated-list entries."
  (mapcar (lambda (i)
            (list (number-to-string i)
                  (vector (number-to-string i) "test")))
          (number-sequence 1 n)))

;;; Mode Activation

(ert-deftest beads-pager-test-mode-activates ()
  "Verify beads-pager-mode enables without error."
  (beads-pager-test--with-buffer
    (should (bound-and-true-p beads-pager-mode))
    (should (eq beads-pager--page 1))
    (should (null beads-pager--all-entries))))

;;; beads-pager-set-entries

(ert-deftest beads-pager-test-set-entries-single-page ()
  "All entries shown on one page when count fits in window."
  (beads-pager-test--with-buffer
    ;; Override page size to 10 for determinism
    (setq beads-pager--page-size 10)
    (let ((entries (beads-pager-test--make-entries 5)))
      (beads-pager-set-entries entries)
      (should (eq beads-pager--page 1))
      (should (eq (length beads-pager--all-entries) 5))
      (should (eq (length tabulated-list-entries) 5)))))

(ert-deftest beads-pager-test-set-entries-paginates ()
  "Page 1 slice shown when entry count exceeds page size."
  (beads-pager-test--with-buffer
    (setq beads-pager--page-size 5)
    (let ((entries (beads-pager-test--make-entries 12)))
      (beads-pager-set-entries entries)
      (should (eq beads-pager--page 1))
      (should (eq (length beads-pager--all-entries) 12))
      (should (eq (length tabulated-list-entries) 5)))))

(ert-deftest beads-pager-test-set-entries-resets-page ()
  "Calling set-entries again resets page to 1."
  (beads-pager-test--with-buffer
    (setq beads-pager--page-size 5)
    (beads-pager-set-entries (beads-pager-test--make-entries 12))
    (setq beads-pager--page 3)
    ;; Set new entries — page should reset to 1
    (beads-pager-set-entries (beads-pager-test--make-entries 12))
    (should (eq beads-pager--page 1))))

;;; Total Pages

(ert-deftest beads-pager-test-total-pages ()
  "Total pages computed correctly."
  (beads-pager-test--with-buffer
    (setq beads-pager--page-size 5)
    (beads-pager-set-entries (beads-pager-test--make-entries 12))
    (should (eq (beads-pager--total-pages) 3))))

(ert-deftest beads-pager-test-total-pages-exact-fit ()
  "Exact multiple of page size gives correct total pages."
  (beads-pager-test--with-buffer
    (setq beads-pager--page-size 5)
    (beads-pager-set-entries (beads-pager-test--make-entries 10))
    (should (eq (beads-pager--total-pages) 2))))

(ert-deftest beads-pager-test-total-pages-empty ()
  "Empty entry list gives 1 total page."
  (beads-pager-test--with-buffer
    (setq beads-pager--page-size 5)
    (beads-pager-set-entries '())
    (should (eq (beads-pager--total-pages) 1))))

;;; Navigation: Next Page

(ert-deftest beads-pager-test-next-page ()
  "Next page advances page number and shows correct slice."
  (beads-pager-test--with-buffer
    (setq beads-pager--page-size 5)
    (beads-pager-set-entries (beads-pager-test--make-entries 12))
    (beads-pager-next-page)
    (should (eq beads-pager--page 2))
    (should (eq (length tabulated-list-entries) 5))
    ;; Entries 6-10 (1-based IDs)
    (should (equal (caar tabulated-list-entries) "6"))))

(ert-deftest beads-pager-test-next-page-last-partial ()
  "Last page shows remaining entries even if count < page size."
  (beads-pager-test--with-buffer
    (setq beads-pager--page-size 5)
    (beads-pager-set-entries (beads-pager-test--make-entries 12))
    (beads-pager-next-page)  ; page 2
    (beads-pager-next-page)  ; page 3 (2 entries: 11, 12)
    (should (eq beads-pager--page 3))
    (should (eq (length tabulated-list-entries) 2))))

(ert-deftest beads-pager-test-next-page-at-last-stays ()
  "Next page at last page keeps page number unchanged."
  (beads-pager-test--with-buffer
    (setq beads-pager--page-size 5)
    (beads-pager-set-entries (beads-pager-test--make-entries 8))
    (beads-pager-next-page)  ; page 2 (last)
    (let ((msgs '()))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (push (apply #'format fmt args) msgs))))
        (beads-pager-next-page))  ; attempt page 3
      (should (eq beads-pager--page 2))
      (should (string-match "last page" (car msgs))))))

;;; Navigation: Prev Page

(ert-deftest beads-pager-test-prev-page ()
  "Prev page decrements page and shows correct slice."
  (beads-pager-test--with-buffer
    (setq beads-pager--page-size 5)
    (beads-pager-set-entries (beads-pager-test--make-entries 12))
    (beads-pager-next-page)  ; page 2
    (beads-pager-prev-page)  ; back to 1
    (should (eq beads-pager--page 1))
    (should (equal (caar tabulated-list-entries) "1"))))

(ert-deftest beads-pager-test-prev-page-at-first-stays ()
  "Prev page at first page keeps page number unchanged."
  (beads-pager-test--with-buffer
    (setq beads-pager--page-size 5)
    (beads-pager-set-entries (beads-pager-test--make-entries 12))
    (let ((msgs '()))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (push (apply #'format fmt args) msgs))))
        (beads-pager-prev-page))
      (should (eq beads-pager--page 1))
      (should (string-match "first page" (car msgs))))))

;;; Navigation: Goto Page

(ert-deftest beads-pager-test-goto-page ()
  "Goto page jumps to the specified page."
  (beads-pager-test--with-buffer
    (setq beads-pager--page-size 5)
    (beads-pager-set-entries (beads-pager-test--make-entries 15))
    (beads-pager-goto-page 3)
    (should (eq beads-pager--page 3))
    (should (equal (caar tabulated-list-entries) "11"))))

(ert-deftest beads-pager-test-goto-page-clamps-high ()
  "Goto page clamps to total pages when N is too large."
  (beads-pager-test--with-buffer
    (setq beads-pager--page-size 5)
    (beads-pager-set-entries (beads-pager-test--make-entries 10))
    (beads-pager-goto-page 99)
    (should (eq beads-pager--page 2))))

(ert-deftest beads-pager-test-goto-page-clamps-low ()
  "Goto page clamps to 1 when N <= 0."
  (beads-pager-test--with-buffer
    (setq beads-pager--page-size 5)
    (beads-pager-set-entries (beads-pager-test--make-entries 10))
    (beads-pager-goto-page 0)
    (should (eq beads-pager--page 1))))

;;; Total Count

(ert-deftest beads-pager-test-total-count-with-pager ()
  "beads-pager--total-count returns total (not page) count when pager active."
  (beads-pager-test--with-buffer
    (setq beads-pager--page-size 5)
    (beads-pager-set-entries (beads-pager-test--make-entries 12))
    ;; tabulated-list-entries has 5 (page 1), total-count should be 12
    (should (eq (beads-pager--total-count) 12))))

(ert-deftest beads-pager-test-total-count-without-pager ()
  "beads-pager--total-count falls back to tabulated-list-entries length."
  (with-temp-buffer
    (tabulated-list-mode)
    ;; pager mode NOT enabled
    (setq tabulated-list-entries (beads-pager-test--make-entries 7))
    (should (eq (beads-pager--total-count) 7))))

;;; Mode-Line Fragment

(ert-deftest beads-pager-test-mode-line-single-page ()
  "Mode-line fragment is nil when there is only one page."
  (beads-pager-test--with-buffer
    (setq beads-pager--page-size 20)
    (beads-pager-set-entries (beads-pager-test--make-entries 5))
    (should (null (beads-pager--mode-line-fragment)))))

(ert-deftest beads-pager-test-mode-line-multi-page ()
  "Mode-line fragment shows page info when there are multiple pages."
  (beads-pager-test--with-buffer
    (setq beads-pager--page-size 5)
    (beads-pager-set-entries (beads-pager-test--make-entries 12))
    (let ((frag (beads-pager--mode-line-fragment)))
      (should (stringp frag))
      (should (string-match "Page 1/3" frag))
      (should (string-match "of 12" frag)))))

(ert-deftest beads-pager-test-mode-line-page-2 ()
  "Mode-line fragment reflects current page after navigation."
  (beads-pager-test--with-buffer
    (setq beads-pager--page-size 5)
    (beads-pager-set-entries (beads-pager-test--make-entries 12))
    (beads-pager-next-page)
    (let ((frag (beads-pager--mode-line-fragment)))
      (should (string-match "Page 2/3" frag))
      (should (string-match "items 6-10" frag)))))

;;; Mode Disable

(ert-deftest beads-pager-test-disable-restores-entries ()
  "Disabling beads-pager-mode restores all entries."
  (beads-pager-test--with-buffer
    (setq beads-pager--page-size 5)
    (beads-pager-set-entries (beads-pager-test--make-entries 12))
    (should (eq (length tabulated-list-entries) 5))
    (beads-pager-mode -1)
    (should (eq (length tabulated-list-entries) 12))))

;;; Fallback Without Mode

(ert-deftest beads-pager-test-set-entries-fallback ()
  "beads-pager-set-entries works without pager mode (sets entries directly)."
  (with-temp-buffer
    (tabulated-list-mode)
    ;; pager mode NOT enabled
    (let ((entries (beads-pager-test--make-entries 5)))
      (beads-pager-set-entries entries)
      (should (eq (length tabulated-list-entries) 5)))))

(provide 'beads-pager-test)
;;; beads-pager-test.el ends here
