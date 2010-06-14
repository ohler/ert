;;; ert-experimental-tests.el --- Tests for ert-experimental.el

;; Copyright (C) 2008 Phil Hagelberg

;; Author: Phil Hagelberg
;; Version: 0.2
;; Keywords: lisp, tools

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

;;; Code:

(require 'ert)
(require 'ert-experimental)

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


(provide 'ert-experimental-tests)

;;; ert-experimental-tests.el ends here
