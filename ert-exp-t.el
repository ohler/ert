;;; ert-exp-t.el --- Tests for ert-exp.el

;; Copyright (C) 2008 Phil Hagelberg

;; Author: Phil Hagelberg

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see `http://www.gnu.org/licenses/'.

;;; Commentary:

;; This file is part of ERT, the Emacs Lisp Regression Testing tool.
;; See ert.el or the texinfo manual for more details.

;;; Code:

(require 'ert)
(require 'ert-exp)
(eval-when-compile
  (require 'cl))

;;; Predicates

(ert-deftest ert-buffer-changes-p ()
  (with-temp-buffer
    (should (buffer-changes-p
             (insert "hello")))
    (should-not (buffer-changes-p
                 (message "hello")))))

(ert-deftest ert-buffer-contains-p ()
  (with-temp-buffer
    (insert "hello world")
    (should (buffer-contains-p "hello"))
    (should-not (buffer-contains-p "goodbye"))))

(ert-deftest ert-correctly-indented-p ()
  (let ((well-indented (concat "(hello (world\n"
                               "        'elisp)\n"))
        (badly-indented (concat "(hello\n"
                                "       world)")))
    (with-temp-buffer
      (insert well-indented)
      (emacs-lisp-mode)
      (should (correctly-indented-p)))
    (with-temp-buffer
      (insert badly-indented)
      (emacs-lisp-mode)
      (should-not (correctly-indented-p)))))


;;; Utilities

(ert-deftest ert-with-test-buffer ()
  (let ((contents "Foo bar\n  baz\n\tbip"))
    (with-test-buffer contents
      (should (string-equal (buffer-string) contents)))))

(ert-deftest ert-with-test-buffer-inserting ()
  (let ((contents "Foo bar\n  baz\n\tbip"))
    (with-test-buffer contents
      (insert "Hello\n")
      (should (string-equal (buffer-string) (concat "Hello\n" contents))))))

(ert-deftest ert-with-test-buffer-mark ()
  (with-test-buffer "Foo<mark> bar baz"
    (should (string-equal (buffer-substring (point) (mark)) "Foo"))
    (should (string-equal (buffer-string) "Foo bar baz"))))

(ert-deftest ert-with-test-buffer-fake-mark ()
  (with-test-buffer "Foo\\<mark> bar baz"
    (should (string-equal (buffer-string) "Foo<mark> bar baz"))))

(ert-deftest ert-with-test-buffer-point ()
  (with-test-buffer "Foo bar<point> baz"
    (insert "bell")
    (should (string-equal (buffer-string) "Foo barbell baz"))))

(ert-deftest ert-with-test-buffer-mark-and-point ()
  (with-test-buffer "Foo <mark>bar<point> baz"
    (upcase-region (mark) (point))
    (should (string-equal (buffer-string) "Foo BAR baz"))))


(ert-deftest ert-filter-string ()
  (should (equal (ert-filter-string "foo bar baz" "quux")
                 "foo bar baz"))
  (should (equal (ert-filter-string "foo bar baz" "bar")
                 "foo  baz")))

(ert-deftest ert-propertized-string ()
  (should (ert-equal-including-properties
           (ert-propertized-string "a" '(a b) "b" '(c t) "cd")
           #("abcd" 1 2 (a b) 2 4 (c t))))
  (should (ert-equal-including-properties
           (ert-propertized-string "foo " '(face italic) "bar" " baz" nil
                                   " quux")
           #("foo bar baz quux" 4 11 (face italic)))))


;;; Tests for ERT itself that require test features from ert-exp.el.

(ert-deftest ert-test-x-run-tests-interactively ()
  :tags '(:causes-redisplay)
  (let ((passing-test (make-ert-test :name 'passing-test
                                     :body (lambda () (ert-pass))))
        (failing-test (make-ert-test :name 'failing-test
                                     :body (lambda () (ert-fail
                                                       "failure message")))))
    (let ((ert-debug-on-error nil))
      (let* ((buffer-name (generate-new-buffer-name "*ert-test-run-tests*"))
             (messages nil)
             (mock-message-fn
              (lambda (format-string &rest args)
                (push (apply #'format format-string args) messages))))
        (flet ((expected-string (with-font-lock-p)
                 (ert-propertized-string
                  "Selector: (member <passing-test> <failing-test>)\n"
                  "Passed: 1\n"
                  "Failed: 1 (1 unexpected)\n"
                  "Error:  0\n"
                  "Total:  2/2\n\n"
                  "Started at:\n"
                  "Finished.\n"
                  "Finished at:\n\n"
                  `(category ,(button-category-symbol
                               'ert--results-progress-bar-button)
                             button (t)
                             face ,(if with-font-lock-p
                                       'ert-test-result-unexpected
                                     'button))
                  ".F"
                  nil
                  "\n\n"
                  `(category ,(button-category-symbol
                               'ert--results-expand-collapse-button)
                             button (t)
                             face ,(if with-font-lock-p
                                       'ert-test-result-unexpected
                                     'button))
                  "F"
                  nil
                  " "
                  `(category ,(button-category-symbol
                               'ert--test-name-button)
                             button (t)
                             ert-test-name failing-test)
                  "failing-test"
                  nil
                  "\n    (ert-test-failed \"failure message\")\n\n\n"
                  )))
        (save-window-excursion
          (unwind-protect
              (let ((case-fold-search nil))
                (ert-run-tests-interactively
                 `(member ,passing-test ,failing-test) buffer-name
                 mock-message-fn)
                (should (equal messages `(,(concat
                                            "Ran 2 tests, 1 results were "
                                            "as expected, 1 unexpected"))))
                (with-current-buffer buffer-name
                  (font-lock-mode 0)
                  (should (ert-equal-including-properties
                           (ert-filter-string (buffer-string)
                                              '("Started at:\\(.*\\)$" 1)
                                              '("Finished at:\\(.*\\)$" 1))
                           (expected-string nil)))
                  ;; `font-lock-mode' only works if interactive, so
                  ;; pretend we are.
                  (let ((noninteractive nil))
                    (font-lock-mode 1))
                  (should (ert-equal-including-properties
                           (ert-filter-string (buffer-string)
                                              '("Started at:\\(.*\\)$" 1)
                                              '("Finished at:\\(.*\\)$" 1))
                           (expected-string t)))))
            (when (get-buffer buffer-name)
              (kill-buffer buffer-name)))))))))


(ert-deftest ert-test-messages-on-log-truncation ()
  :tags '(:causes-redisplay)
  (let ((test (make-ert-test
               :body (lambda ()
                       ;; Emacs would combine messages if we
                       ;; generate the same message multiple
                       ;; times.
                       (message "a")
                       (message "b")
                       (message "c")
                       (message "d")))))
    (let (result)
      (ert-with-buffer-renamed ("*Messages*")
        (let ((message-log-max 2))
          (setq result (ert-run-test test)))
        (should (equal (with-current-buffer "*Messages*"
                         (buffer-string))
                       "c\nd\n")))
      (should (equal (ert-test-result-messages result) "a\nb\nc\nd\n")))))

(ert-deftest ert-test-describe-test ()
  "Tests `ert-describe-test'."
  (save-window-excursion
    (ert-with-buffer-renamed ("*Help*")
      (if (< emacs-major-version 24)
          (should-error (ert-describe-test 'ert-describe-test)
                        :test (lambda (condition)
                                (should (equal condition
                                               '(error "Requires Emacs 24")))))
        (ert-describe-test 'ert-test-describe-test)
        (with-current-buffer "*Help*"
          (let ((case-fold-search nil))
            (should (string-match (concat
                                   "\\`ert-test-describe-test is a test"
                                   " defined in `ert-tests.elc?'\\.\n\n"
                                   "Tests `ert-describe-test'\\.\n\\'")
                                  (buffer-string)))))))))

(ert-deftest ert-test-builtin-message-log-flushing ()
  "This test attempts to demonstrate that there is no way to
force immediate truncation of the *Messages* buffer from Lisp
\(and hence justifies the existence of
`ert--force-message-log-buffer-truncation'\): The only way that
came to my mind was \(message \"\"\), which doesn't have the
desired effect."
  :tags '(:causes-redisplay)
  (ert-with-buffer-renamed ("*Messages*")
    (with-current-buffer "*Messages*"
      (should (equal (buffer-string) ""))
      ;; We used to get sporadic failures in this test that involved
      ;; a spurious newline at the beginning of the buffer, before
      ;; the first message.  Below, we print a message and erase the
      ;; buffer since this seems to eliminate the sporadic failures.
      (message "foo")
      (erase-buffer)
      (should (equal (buffer-string) ""))
      (let ((message-log-max 2))
        (let ((message-log-max t))
          (loop for i below 4 do
                (message "%s" i))
          (should (equal (buffer-string) "0\n1\n2\n3\n")))
        (should (equal (buffer-string) "0\n1\n2\n3\n"))
        (message "")
        (should (equal (buffer-string) "0\n1\n2\n3\n"))
        (message "Test message")
        (should (equal (buffer-string) "3\nTest message\n"))))))

(ert-deftest ert-test-force-message-log-buffer-truncation ()
  :tags '(:causes-redisplay)
  (labels ((body ()
             (loop for i below 3 do
                   (message "%s" i)))
           ;; Uses the implicit messages buffer truncation implemented
           ;; in Emacs' C core.
           (c (x)
             (ert-with-buffer-renamed ("*Messages*")
               (let ((message-log-max x))
                 (body))
               (with-current-buffer "*Messages*"
                 (buffer-string))))
           ;; Uses our lisp reimplementation.
           (lisp (x)
             (ert-with-buffer-renamed ("*Messages*")
               (let ((message-log-max t))
                 (body))
               (let ((message-log-max x))
                 (ert--force-message-log-buffer-truncation))
               (with-current-buffer "*Messages*"
                 (buffer-string)))))
    (loop for x in '(0 1 2 3 4 t) do
          (should (equal (c x) (lisp x))))))


(provide 'ert-exp-t)

;;; ert-exp-t.el ends here
