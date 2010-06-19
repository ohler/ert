;;; ert-tests.el --- ERT's self-tests

;; Copyright (C) 2007, 2008, 2010 Christian M. Ohler

;; Author: Christian M. Ohler

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

(eval-when-compile
  (require 'cl))
;; Test definitions should normally depend just on ert, not ert-ui.
;; However, this file includes tests for ert-ui, so it has to depend
;; on it.
(require 'ert-ui)


;;; Self-test that doesn't rely on ERT, for bootstrapping.

;; This is used to test that bodies actually run.
(defvar ert-test-body-was-run)
(ert-deftest ert-test-body-runs ()
  (setq ert-test-body-was-run t))

(defun ert-self-test ()
  "Runs tests and make sure they actually ran."
  (let ((window-configuration (current-window-configuration)))
    (let ((ert-test-body-was-run nil))
      ;; The buffer name chosen here should not compete with the default
      ;; results buffer name for completion in `switch-to-buffer'.
      (let ((stats (ert-run-tests-interactively "^ert-" " *ert self-tests*")))
        (assert ert-test-body-was-run)
        (if (zerop (ert-stats-completed-unexpected stats))
            ;; Hide results window only when everything went well.
            (set-window-configuration window-configuration)
          (error "ERT self-test failed"))))))

(defun ert-self-test-and-exit ()
  (unwind-protect
      (progn
        (ert-self-test)
        (kill-emacs 0))
    (unwind-protect
        (progn
          (message "Error running tests")
          (backtrace))
      (kill-emacs 1))))


;;; Further tests are defined using ERT.

(ert-deftest ert-nested-test-body-runs ()
  "Test that nested test bodies run."
  (lexical-let ((was-run nil))
    (let ((test (make-ert-test :body (lambda ()
                                       (setq was-run t)))))
      (assert (not was-run))
      (ert-run-test test)
      (assert was-run))))


;;; Test that pass/fail works.
(ert-deftest ert-test-pass ()
  (let ((test (make-ert-test :body (lambda ()))))
    (let ((result (ert-run-test test)))
      (assert (typep result 'ert-test-passed)))))

(ert-deftest ert-test-fail ()
  (let ((test (make-ert-test :body (lambda () (ert-fail "failure message")))))
    (let ((result (let ((ert-debug-on-error nil))
                    (ert-run-test test))))
      (assert (typep result 'ert-test-failed) t)
      (assert (equal (ert-test-result-with-condition-condition result)
                     '(ert-test-failed "failure message"))
              t))))

(ert-deftest ert-test-fail-debug-with-condition-case ()
  (let ((test (make-ert-test :body (lambda () (ert-fail "failure message")))))
    (condition-case condition
        (progn
          (let ((ert-debug-on-error t))
            (ert-run-test test))
          (assert nil))
      ((error)
       (assert (equal condition '(ert-test-failed "failure message")) t)))))

(ert-deftest ert-test-fail-debug-with-debugger-1 ()
  (let ((test (make-ert-test :body (lambda () (ert-fail "failure message")))))
    (let ((debugger (lambda (&rest debugger-args)
                      (assert nil))))
      (let ((ert-debug-on-error nil))
        (ert-run-test test)))))

(ert-deftest ert-test-fail-debug-with-debugger-2 ()
  (let ((test (make-ert-test :body (lambda () (ert-fail "failure message")))))
    (block nil
      (let ((debugger (lambda (&rest debugger-args)
                        (return-from nil nil))))
        (let ((ert-debug-on-error t))
          (ert-run-test test))
        (assert nil)))))

(ert-deftest ert-test-fail-debug-nested-with-debugger ()
  (let ((test (make-ert-test :body (lambda ()
                                     (let ((ert-debug-on-error t))
                                       (ert-fail "failure message"))))))
    (let ((debugger (lambda (&rest debugger-args)
                      (assert nil nil "Assertion a"))))
      (let ((ert-debug-on-error nil))
        (ert-run-test test))))
  (let ((test (make-ert-test :body (lambda ()
                                     (let ((ert-debug-on-error nil))
                                       (ert-fail "failure message"))))))
    (block nil
      (let ((debugger (lambda (&rest debugger-args)
                        (return-from nil nil))))
        (let ((ert-debug-on-error t))
          (ert-run-test test))
        (assert nil nil "Assertion b")))))

(ert-deftest ert-test-error ()
  (let ((test (make-ert-test :body (lambda () (error "error message")))))
    (let ((result (let ((ert-debug-on-error nil))
                    (ert-run-test test))))
      (assert (typep result 'ert-test-error) t)
      (assert (equal (ert-test-result-with-condition-condition result)
                     '(error "error message"))
              t))))

(ert-deftest ert-test-error-debug ()
  (let ((test (make-ert-test :body (lambda () (error "error message")))))
    (condition-case condition
        (progn
          (let ((ert-debug-on-error t))
            (ert-run-test test))
          (assert nil))
      ((error)
       (assert (equal condition '(error "error message")) t)))))


;;; Test that `should' works.
(ert-deftest ert-test-should ()
  (let ((test (make-ert-test :body (lambda () (should nil)))))
    (let ((result (let ((ert-debug-on-error nil))
                    (ert-run-test test))))
      (assert (typep result 'ert-test-failed) t)
      (assert (equal (ert-test-result-with-condition-condition result)
                     '(ert-test-failed ((should nil) :form nil :value nil)))
              t)))
  (let ((test (make-ert-test :body (lambda () (should t)))))
    (let ((result (ert-run-test test)))
      (assert (typep result 'ert-test-passed) t))))

(ert-deftest ert-test-should-value ()
  (should (eql (should 'foo) 'foo))
  (should (eql (should 'bar) 'bar)))

(ert-deftest ert-test-should-not ()
  (let ((test (make-ert-test :body (lambda () (should-not t)))))
    (let ((result (let ((ert-debug-on-error nil))
                    (ert-run-test test))))
      (assert (typep result 'ert-test-failed) t)
      (assert (equal (ert-test-result-with-condition-condition result)
                     '(ert-test-failed ((should-not t) :form t :value t)))
              t)))
  (let ((test (make-ert-test :body (lambda () (should-not nil)))))
    (let ((result (ert-run-test test)))
      (assert (typep result 'ert-test-passed)))))

(ert-deftest ert-test-should-with-macrolet ()
  (let ((test (make-ert-test :body (lambda ()
                                     (macrolet ((foo () `(progn t nil)))
                                       (should (foo)))))))
    (let ((result (let ((ert-debug-on-error nil))
                    (ert-run-test test))))
      (should (typep result 'ert-test-failed))
      (should (equal
               (ert-test-result-with-condition-condition result)
               '(ert-test-failed ((should (foo))
                                  :form (progn t nil)
                                  :value nil)))))))

(ert-deftest ert-test-should-error ()
  ;; No error.
  (let ((test (make-ert-test :body (lambda () (should-error (progn))))))
    (let ((result (let ((ert-debug-on-error nil))
                    (ert-run-test test))))
      (should (typep result 'ert-test-failed))
      (should (equal (ert-test-result-with-condition-condition result)
                     '(ert-test-failed
                       ((should-error (progn))
                        :form (progn)
                        :value nil
                        :fail-reason "did not signal an error"))))))
  ;; A simple error.
  (let ((test (make-ert-test :body (lambda () (should-error (error "foo"))))))
    (let ((result (ert-run-test test)))
      (should (typep result 'ert-test-passed))))
  ;; Error of unexpected type, no test.
  (let ((test (make-ert-test :body (lambda ()
                                     (should-error (error "foo")
                                                   :type 'singularity-error)))))
    (let ((result (ert-run-test test)))
      (should (typep result 'ert-test-failed))
      (should (equal
               (ert-test-result-with-condition-condition result)
               '(ert-test-failed
                 ((should-error (error "foo") :type 'singularity-error)
                  :form (error "foo")
                  :condition (error "foo")
                  :fail-reason
                  "the error signalled did not have the expected type"))))))
  ;; Error of the expected type, no test.
  (let ((test (make-ert-test :body (lambda ()
                                     (should-error (signal 'singularity-error
                                                           nil)
                                                   :type 'singularity-error)))))
    (let ((result (ert-run-test test)))
      (should (typep result 'ert-test-passed))))
  ;; Error that fails the test, no type.
  (let ((test (make-ert-test :body (lambda ()
                                     (should-error
                                      (error "foo")
                                      :test (lambda (error) nil))))))
    (let ((result (ert-run-test test)))
      (should (typep result 'ert-test-failed))
      (should (equal (ert-test-result-with-condition-condition result)
                     '(ert-test-failed
                       ((should-error (error "foo") :test (lambda (error) nil))
                        :form (error "foo")
                        :condition (error "foo")
                        :fail-reason
                        "the error signalled did not pass the test"))))))
  ;; Error that passes the test, no type.
  (let ((test (make-ert-test :body (lambda ()
                                     (should-error (error "foo")
                                                   :test (lambda (error) t))))))
    (let ((result (ert-run-test test)))
      (should (typep result 'ert-test-passed))))
  ;; Error that has the expected type but fails the test.
  (let ((test (make-ert-test :body (lambda ()
                                     (should-error
                                      (signal 'singularity-error nil)
                                      :type 'singularity-error
                                      :test (lambda (error) nil))))))
    (let ((result (ert-run-test test)))
      (should (typep result 'ert-test-failed))
      (should (equal (ert-test-result-with-condition-condition result)
                     '(ert-test-failed
                       ((should-error (signal 'singularity-error nil)
                                      :type 'singularity-error
                                      :test (lambda (error) nil))
                        :form (signal singularity-error nil)
                        :condition (singularity-error)
                        :fail-reason
                        "the error signalled did not pass the test"))))))
  ;; Error that has the expected type and passes the test.
  (let ((test (make-ert-test :body (lambda ()
                                     (should-error
                                      (signal 'singularity-error nil)
                                      :type 'singularity-error
                                      :test (lambda (error) t))))))
    (let ((result (ert-run-test test)))
      (should (typep result 'ert-test-passed)))))

(ert-deftest ert-test-should-error-subtypes ()
  (let ((test (make-ert-test
               :body (lambda ()
                       (should-error (signal 'singularity-error nil)
                                     :type 'singularity-error
                                     :exclude-subtypes t)))))
    (let ((result (ert-run-test test)))
      (should (typep result 'ert-test-passed))))
  (let ((test (make-ert-test
               :body (lambda ()
                       (should-error (signal 'arith-error nil)
                                     :type 'singularity-error)))))
    (let ((result (ert-run-test test)))
      (should (typep result 'ert-test-failed))
      (should (equal
               (ert-test-result-with-condition-condition result)
               '(ert-test-failed
                 ((should-error (signal 'arith-error nil)
                                :type 'singularity-error)
                  :form (signal arith-error nil)
                  :condition (arith-error)
                  :fail-reason
                  "the error signalled did not have the expected type"))))))
  (let ((test (make-ert-test
               :body (lambda ()
                       (should-error (signal 'arith-error nil)
                                     :type 'singularity-error
                                     :exclude-subtypes t)))))
    (let ((result (ert-run-test test)))
      (should (typep result 'ert-test-failed))
      (should (equal
               (ert-test-result-with-condition-condition result)
               '(ert-test-failed
                 ((should-error (signal 'arith-error nil)
                                :type 'singularity-error
                                :exclude-subtypes t)
                  :form (signal arith-error nil)
                  :condition (arith-error)
                  :fail-reason
                  "the error signalled did not have the expected type"))))))
  (let ((test (make-ert-test
               :body (lambda ()
                       (should-error (signal 'singularity-error nil)
                                     :type 'arith-error
                                     :exclude-subtypes t)))))
    (let ((result (ert-run-test test)))
      (should (typep result 'ert-test-failed))
      (should (equal
               (ert-test-result-with-condition-condition result)
               '(ert-test-failed
                 ((should-error (signal 'singularity-error nil)
                                :type 'arith-error
                                :exclude-subtypes t)
                  :form (signal singularity-error nil)
                  :condition (singularity-error)
                  :fail-reason
                  "the error signalled was a subtype of the expected type")))))
    ))

(defmacro ert-test-my-list (&rest args)
  `(list ,@args))

(ert-deftest ert-test-should-failure-debugging ()
  "Test that `should' errors contain the information we expect them to."
  (loop for (body expected-condition) in
        `((,(lambda () (let ((x nil)) (should x)))
           (ert-test-failed ((should x) :form x :value nil)))
          (,(lambda () (let ((x t)) (should-not x)))
           (ert-test-failed ((should-not x) :form x :value t)))
          (,(lambda () (let ((x t)) (should (not x))))
           (ert-test-failed ((should (not x)) :form (not t) :value nil)))
          (,(lambda () (let ((x nil)) (should-not (not x))))
           (ert-test-failed ((should-not (not x)) :form (not nil) :value t)))
          (,(lambda () (let ((x t) (y nil)) (should-not
                                             (ert-test-my-list x y))))
           (ert-test-failed
            ((should-not (ert-test-my-list x y))
             :form (list t nil)
             :value (t nil))))
          (,(lambda () (let ((x t)) (should (error "foo"))))
           (error "foo")))
        do
        (let ((test (make-ert-test :body body)))
          (condition-case actual-condition
              (progn
                (let ((ert-debug-on-error t))
                  (ert-run-test test))
                (assert nil))
            ((error)
             (should (equal actual-condition expected-condition)))))))

(ert-deftest ert-test-deftest ()
  (should (equal (macroexpand '(ert-deftest abc () "foo" :tags '(bar)))
                 '(progn
                    (ert-set-test 'abc
                                  (make-ert-test :name 'abc
                                                 :documentation "foo"
                                                 :tags '(bar)
                                                 :body (lambda ())))
                    (push '(ert-deftest . abc) current-load-list)
                    'abc)))
  (should (equal (macroexpand '(ert-deftest def ()
                                 :expected-result ':passed))
                 '(progn
                    (ert-set-test 'def
                                  (make-ert-test :name 'def
                                                 :expected-result-type ':passed
                                                 :body (lambda ())))
                    (push '(ert-deftest . def) current-load-list)
                    'def)))
  ;; :documentation keyword is forbidden
  (should-error (macroexpand '(ert-deftest ghi ()
                                :documentation "foo"))))

(ert-deftest ert-test-record-backtrace ()
  (let ((test (make-ert-test :body (lambda () (ert-fail "foo")))))
    (let ((result (ert-run-test test)))
      (should (ert-test-failed-p result))
      (with-temp-buffer
        (ert-print-backtrace (ert-test-failed-backtrace result))
        (goto-char (point-min))
        (end-of-line)
        (let ((first-line (buffer-substring-no-properties (point-min) (point))))
          (should (equal first-line "  signal(ert-test-failed (\"foo\"))")))))))

(ert-deftest ert-test-messages ()
  :tags '(:causes-redisplay)
  (let* ((message-string "Test message")
         (messages-buffer (get-buffer-create "*Messages*"))
         (test (make-ert-test :body (lambda () (message "%s" message-string)))))
    (with-current-buffer messages-buffer
      (let ((result (ert-run-test test)))
        (should (equal (concat message-string "\n")
                       (ert-test-result-messages result)))))))

(defun ert-call-with-temporary-messages-buffer (thunk)
  (lexical-let ((new-buffer-name (generate-new-buffer-name
                                  "*Messages* orig buffer")))
    (unwind-protect
        (progn
          (with-current-buffer (get-buffer-create "*Messages*")
            (rename-buffer new-buffer-name))
          (get-buffer-create "*Messages*")
          (funcall thunk))
      (kill-buffer "*Messages*")
      (with-current-buffer new-buffer-name
        (rename-buffer "*Messages*")))))

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
      (ert-call-with-temporary-messages-buffer
       (lambda ()
         (let ((message-log-max 2))
           (setq result (ert-run-test test)))
         (should (equal (with-current-buffer "*Messages*"
                          (buffer-string))
                        "c\nd\n"))))
      (should (equal (ert-test-result-messages result) "a\nb\nc\nd\n")))))

(ert-deftest ert-test-running-tests ()
  (let ((outer-test (ert-get-test 'ert-test-running-tests)))
    (should (equal (ert-running-test) outer-test))
    (let (test1 test2 test3)
      (setq test1 (make-ert-test
                   :name "1"
                   :body (lambda ()
                           (should (equal (ert-running-test) outer-test))
                           (should (equal ert-running-tests
                                          (list test1 test2 test3
                                                outer-test)))))
            test2 (make-ert-test
                   :name "2"
                   :body (lambda ()
                           (should (equal (ert-running-test) outer-test))
                           (should (equal ert-running-tests
                                          (list test3 test2 outer-test)))
                           (ert-run-test test1)))
            test3 (make-ert-test
                   :name "3"
                   :body (lambda ()
                           (should (equal (ert-running-test) outer-test))
                           (should (equal ert-running-tests
                                          (list test3 outer-test)))
                           (ert-run-test test2))))
      (should (ert-test-passed-p (ert-run-test test3))))))

(ert-deftest ert-test-test-result-expected-p ()
  "Test `ert-test-result-expected-p' and (implicitly) `ert-test-result-type-p'."
  ;; passing test
  (let ((test (make-ert-test :body (lambda ()))))
    (should (ert-test-result-expected-p test (ert-run-test test))))
  ;; unexpected failure
  (let ((test (make-ert-test :body (lambda () (ert-fail "failed")))))
    (should-not (ert-test-result-expected-p test (ert-run-test test))))
  ;; expected failure
  (let ((test (make-ert-test :body (lambda () (ert-fail "failed"))
                             :expected-result-type ':failed)))
    (should (ert-test-result-expected-p test (ert-run-test test))))
  ;; `not' expected type
  (let ((test (make-ert-test :body (lambda ())
                             :expected-result-type '(not :failed))))
    (should (ert-test-result-expected-p test (ert-run-test test))))
  (let ((test (make-ert-test :body (lambda ())
                             :expected-result-type '(not :passed))))
    (should-not (ert-test-result-expected-p test (ert-run-test test))))
  ;; `and' expected type
  (let ((test (make-ert-test :body (lambda ())
                             :expected-result-type '(and :passed :failed))))
    (should-not (ert-test-result-expected-p test (ert-run-test test))))
  (let ((test (make-ert-test :body (lambda ())
                             :expected-result-type '(and :passed
                                                         (not :failed)))))
    (should (ert-test-result-expected-p test (ert-run-test test))))
  ;; `or' expected type
  (let ((test (make-ert-test :body (lambda ())
                             :expected-result-type '(or (and :passed :failed)
                                                        :passed))))
    (should (ert-test-result-expected-p test (ert-run-test test))))
  (let ((test (make-ert-test :body (lambda ())
                             :expected-result-type '(or (and :passed :failed)
                                                   nil (not t)))))
    (should-not (ert-test-result-expected-p test (ert-run-test test)))))

;;; Test `ert-select-tests'.
(ert-deftest ert-test-select-regexp ()
  (should (equal (ert-select-tests "^ert-test-select-regexp$" t)
                 (list (ert-get-test 'ert-test-select-regexp)))))

(ert-deftest ert-test-test-boundp ()
  (should (ert-test-boundp 'ert-test-test-boundp))
  (should-not (ert-test-boundp (make-symbol "ert-not-a-test"))))

(ert-deftest ert-test-select-member ()
  (should (equal (ert-select-tests '(member ert-test-select-member) t)
                 (list (ert-get-test 'ert-test-select-member)))))

(ert-deftest ert-test-select-test ()
  (should (equal (ert-select-tests (ert-get-test 'ert-test-select-test) t)
                 (list (ert-get-test 'ert-test-select-test)))))

(ert-deftest ert-test-select-symbol ()
  (should (equal (ert-select-tests 'ert-test-select-symbol t)
                 (list (ert-get-test 'ert-test-select-symbol)))))

(ert-deftest ert-test-select-and ()
  (let ((test (make-ert-test
               :name nil
               :body nil
               :most-recent-result (make-ert-test-failed
                                    :condition nil
                                    :backtrace nil))))
    (should (equal (ert-select-tests `(and (member ,test) :failed) t)
                   (list test)))))

(ert-deftest ert-test-select-tag ()
  (let ((test (make-ert-test
               :name nil
               :body nil
               :tags '(a b))))
    (should (equal (ert-select-tests `(tag a) (list test)) (list test)))
    (should (equal (ert-select-tests `(tag b) (list test)) (list test)))
    (should (equal (ert-select-tests `(tag c) (list test)) '()))))


;;; Tests for utility functions.
(ert-deftest ert-proper-list-p ()
  (should (ert-proper-list-p '()))
  (should (ert-proper-list-p '(1)))
  (should (ert-proper-list-p '(1 2)))
  (should (ert-proper-list-p '(1 2 3)))
  (should (ert-proper-list-p '(1 2 3 4)))
  (should (not (ert-proper-list-p 'a)))
  (should (not (ert-proper-list-p '(1 . a))))
  (should (not (ert-proper-list-p '(1 2 . a))))
  (should (not (ert-proper-list-p '(1 2 3 . a))))
  (should (not (ert-proper-list-p '(1 2 3 4 . a))))
  (let ((a (list 1)))
    (setf (cdr (last a)) a)
    (should (not (ert-proper-list-p a))))
  (let ((a (list 1 2)))
    (setf (cdr (last a)) a)
    (should (not (ert-proper-list-p a))))
  (let ((a (list 1 2 3)))
    (setf (cdr (last a)) a)
    (should (not (ert-proper-list-p a))))
  (let ((a (list 1 2 3 4)))
    (setf (cdr (last a)) a)
    (should (not (ert-proper-list-p a))))
  (let ((a (list 1 2)))
    (setf (cdr (last a)) (cdr a))
    (should (not (ert-proper-list-p a))))
  (let ((a (list 1 2 3)))
    (setf (cdr (last a)) (cdr a))
    (should (not (ert-proper-list-p a))))
  (let ((a (list 1 2 3 4)))
    (setf (cdr (last a)) (cdr a))
    (should (not (ert-proper-list-p a))))
  (let ((a (list 1 2 3)))
    (setf (cdr (last a)) (cddr a))
    (should (not (ert-proper-list-p a))))
  (let ((a (list 1 2 3 4)))
    (setf (cdr (last a)) (cddr a))
    (should (not (ert-proper-list-p a))))
  (let ((a (list 1 2 3 4)))
    (setf (cdr (last a)) (cdddr a))
    (should (not (ert-proper-list-p a)))))

(ert-deftest ert-parse-keys-and-body ()
  (should (equal (ert-parse-keys-and-body '(foo)) '(nil (foo))))
  (should (equal (ert-parse-keys-and-body '(:bar foo)) '((:bar foo) nil)))
  (should (equal (ert-parse-keys-and-body '(:bar foo a (b)))
                 '((:bar foo) (a (b)))))
  (should (equal (ert-parse-keys-and-body '(:bar foo :a (b)))
                 '((:bar foo :a (b)) nil)))
  (should (equal (ert-parse-keys-and-body '(bar foo :a (b)))
                 '(nil (bar foo :a (b)))))
  (should-error (ert-parse-keys-and-body '(:bar foo :a))))


(ert-deftest ert-test-run-tests-interactively ()
  :tags '(:causes-redisplay)
  (let ((passing-test (make-ert-test :name 'passing-test
                                     :body (lambda () (ert-pass))))
        (failing-test (make-ert-test :name 'failing-test
                                     :body (lambda () (ert-fail
                                                       "failure message"))))
        )
    (let ((ert-debug-on-error nil))
      (let* ((buffer-name (generate-new-buffer-name " *ert-test-run-tests*"))
             (messages nil)
             (mock-message-fn
              (lambda (format-string &rest args)
                (push (apply #'format format-string args) messages))))
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
                  (goto-char (point-min))
                  (should (equal
                           (buffer-substring (point-min)
                                             (save-excursion
                                               (forward-line 5)
                                               (point)))
                           (concat
                            "Selector: (member <passing-test> <failing-test>)\n"
                            "Passed: 1\n"
                            "Failed: 1 (1 unexpected)\n"
                            "Error:  0\n"
                            "Total:  2/2\n")))))
            (when (get-buffer buffer-name)
              (kill-buffer buffer-name))))))))

(ert-deftest ert-test-special-operator-p ()
  (should (ert-special-operator-p 'if))
  (should-not (ert-special-operator-p 'car))
  (should-not (ert-special-operator-p 'ert-special-operator-p))
  (let ((b (ert-gensym)))
    (should-not (ert-special-operator-p b))
    (fset b 'if)
    (should (ert-special-operator-p b))))

(ert-deftest ert-test-builtin-message-log-flushing ()
  "This test attempts to demonstrate that there is no way to
force immediate truncation of the *Messages* buffer from Lisp
\(and hence justifies the existence of
`ert-force-message-log-buffer-truncation'\): The only way that
came to my mind was \(message ""\), which doesn't have the
desired effect."
  :tags '(:causes-redisplay)
  (ert-call-with-temporary-messages-buffer
   (lambda ()
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
         (should (equal (buffer-string) "3\nTest message\n")))))))

(ert-deftest ert-test-force-message-log-buffer-truncation ()
  :tags '(:causes-redisplay)
  (labels ((body ()
             (loop for i below 3 do
                   (message "%s" i)))
           ;; Uses the implicit messages buffer truncation implemented
           ;; in Emacs' C core.
           (c (x)
             (ert-call-with-temporary-messages-buffer
              (lambda ()
                (let ((message-log-max x))
                  (body))
                (with-current-buffer "*Messages*"
                  (buffer-string)))))
           ;; Uses our lisp reimplementation.
           (lisp (x)
             (ert-call-with-temporary-messages-buffer
              (lambda ()
                (let ((message-log-max t))
                  (body))
                (let ((message-log-max x))
                  (ert-force-message-log-buffer-truncation))
                (with-current-buffer "*Messages*"
                  (buffer-string))))))
    (loop for x in '(0 1 2 3 4 t) do
          (should (equal (c x) (lisp x))))))

(ert-deftest ert-test-list-of-should-forms ()
  (let ((test (make-ert-test :body (lambda ()
                                     (should t)
                                     (should (null '()))
                                     (should nil)
                                     (should t)))))
    (let ((result (let ((ert-debug-on-error nil))
                    (ert-run-test test))))
      (should (equal (ert-test-result-should-forms result)
                     '(((should t) :form t :value t)
                       ((should (null '())) :form (null nil) :value t)
                       ((should nil) :form nil :value nil)))))))

(ert-deftest ert-test-list-of-should-forms-observers-should-not-stack ()
  (let ((test (make-ert-test
               :body (lambda ()
                       (let ((test2 (make-ert-test
                                     :body (lambda ()
                                             (should t)))))
                         (let ((result (ert-run-test test2)))
                           (should (typep result 'ert-test-passed))))))))
    (let ((result (let ((ert-debug-on-error nil))
                    (ert-run-test test))))
      (should (typep result 'ert-test-passed))
      (should (eql (length (ert-test-result-should-forms result))
                   1)))))

(ert-deftest ert-test-list-of-should-forms-no-deep-copy ()
  (let ((test (make-ert-test :body (lambda ()
                                     (let ((obj (list 'a)))
                                       (should (equal obj '(a)))
                                       (setf (car obj) 'b)
                                       (should (equal obj '(b))))))))
    (let ((result (let ((ert-debug-on-error nil))
                    (ert-run-test test))))
      (should (typep result 'ert-test-passed))
      (should (equal (ert-test-result-should-forms result)
                     '(((should (equal obj '(a))) :form (equal (b) (a)) :value t
                        :explanation nil)
                       ((should (equal obj '(b))) :form (equal (b) (b)) :value t
                        :explanation nil)
                       ))))))

(ert-deftest ert-test-remprop ()
  (let ((x (ert-gensym)))
    (should (equal (symbol-plist x) '()))
    ;; Remove nonexistent property on empty plist.
    (ert-remprop x 'b)
    (should (equal (symbol-plist x) '()))
    (put x 'a 1)
    (should (equal (symbol-plist x) '(a 1)))
    ;; Remove nonexistent property on nonempty plist.
    (ert-remprop x 'b)
    (should (equal (symbol-plist x) '(a 1)))
    (put x 'b 2)
    (put x 'c 3)
    (put x 'd 4)
    (should (equal (symbol-plist x) '(a 1 b 2 c 3 d 4)))
    ;; Remove property that is neither first nor last.
    (ert-remprop x 'c)
    (should (equal (symbol-plist x) '(a 1 b 2 d 4)))
    ;; Remove last property from a plist of length >1.
    (ert-remprop x 'd)
    (should (equal (symbol-plist x) '(a 1 b 2)))
    ;; Remove first property from a plist of length >1.
    (ert-remprop x 'a)
    (should (equal (symbol-plist x) '(b 2)))
    ;; Remove property when there is only one.
    (ert-remprop x 'b)
    (should (equal (symbol-plist x) '()))))

(ert-deftest ert-test-remove-if-not ()
  (let ((list (list 'a 'b 'c 'd))
        (i 0))
    (let ((result (ert-remove-if-not (lambda (x)
                                       (should (eql x (nth i list)))
                                       (incf i)
                                       (member i '(2 3)))
                                     list)))
      (should (equal i 4))
      (should (equal result '(b c)))
      (should (equal list '(a b c d)))))
  (should (equal '()
                 (ert-remove-if-not (lambda (x) (should nil)) '()))))

(ert-deftest ert-test-remove* ()
  (let ((list (list 'a 'b 'c 'd))
        (key-index 0)
        (test-index 0))
    (let ((result
           (ert-remove* 'foo list
                        :key (lambda (x)
                               (should (eql x (nth key-index list)))
                               (prog1
                                   (list key-index x)
                                 (incf key-index)))
                        :test
                        (lambda (a b)
                          (should (eql a 'foo))
                          (should (equal b (list test-index
                                                 (nth test-index list))))
                          (incf test-index)
                          (member test-index '(2 3))))))
      (should (equal key-index 4))
      (should (equal test-index 4))
      (should (equal result '(a d)))
      (should (equal list '(a b c d)))))
  (let ((x (cons nil nil))
        (y (cons nil nil)))
    (should (equal (ert-remove* x (list x y))
                   ;; or (list x), since we use `equal' -- the
                   ;; important thing is that only one element got
                   ;; removed, this proves that the default test is
                   ;; `eql', not `equal'
                   (list y)))))


(defun ert-data-for-set-tests ())

(ert-deftest ert-test-set-functions ()
  (let ((c1 (cons nil nil))
        (c2 (cons nil nil))
        (sym (make-symbol "a")))
    (let ((e '())
          (a (list 'a 'b sym nil "" "x" c1 c2))
          (b (list c1 'y 'b sym 'x)))
      (should (equal (ert-set-difference e e) e))
      (should (equal (ert-set-difference a e) a))
      (should (equal (ert-set-difference e a) e))
      (should (equal (ert-set-difference a a) e))
      (should (equal (ert-set-difference b e) b))
      (should (equal (ert-set-difference e b) e))
      (should (equal (ert-set-difference b b) e))
      (should (equal (ert-set-difference a b) (list 'a nil "" "x" c2)))
      (should (equal (ert-set-difference b a) (list 'y 'x)))

      (should (equal (ert-union e e) e))
      (should (equal (ert-union a e) a))
      (should (equal (ert-union e a) a))
      (should (equal (ert-union a a) a))
      (should (equal (ert-union b e) b))
      (should (equal (ert-union e b) b))
      (should (equal (ert-union b b) b))
      (should (equal (ert-union a b) (list 'a 'b sym nil "" "x" c1 c2 'y 'x)))
      (should (equal (ert-union b a) (list c1 'y 'b sym 'x 'a nil "" "x" c2)))

      (should (equal (ert-intersection e e) e))
      (should (equal (ert-intersection a e) e))
      (should (equal (ert-intersection e a) e))
      (should (equal (ert-intersection a a) a))
      (should (equal (ert-intersection b e) e))
      (should (equal (ert-intersection e b) e))
      (should (equal (ert-intersection b b) b))
      (should (equal (ert-intersection a b) (list 'b sym c1)))
      (should (equal (ert-intersection b a) (list c1 'b sym))))))

(ert-deftest ert-test-gensym ()
  ;; Since the expansion of `should' calls `ert-gensym' and thus has a
  ;; side-effect on `ert-gensym-counter', we have to make sure all
  ;; macros in our test body are expanded before we rebind
  ;; `ert-gensym-counter' and run the body.  Otherwise, the test would
  ;; fail if run interpreted.
  (let ((body (byte-compile
               '(lambda ()
                  (should (equal (symbol-name (ert-gensym)) "G0"))
                  (should (equal (symbol-name (ert-gensym)) "G1"))
                  (should (equal (symbol-name (ert-gensym)) "G2"))
                  (should (equal (symbol-name (ert-gensym "foo")) "foo3"))
                  (should (equal (symbol-name (ert-gensym "bar")) "bar4"))
                  (should (equal ert-gensym-counter 5))))))
    (let ((ert-gensym-counter 0))
      (funcall body))))

(ert-deftest ert-test-coerce-to-vector ()
  (let* ((a (vector))
         (b (vector 1 a 3))
         (c (list))
         (d (list b a)))
    (should (eql (ert-coerce-to-vector a) a))
    (should (eql (ert-coerce-to-vector b) b))
    (should (equal (ert-coerce-to-vector c) (vector)))
    (should (equal (ert-coerce-to-vector d) (vector b a)))))

(ert-deftest ert-test-explain-not-equal ()
  (should (equal (ert-explain-not-equal nil 'foo)
                 '(different-atoms nil foo)))
  (should (equal (ert-explain-not-equal '(a a) '(a b))
                 '(list-elt 1 (different-atoms a b))))
  (should (equal (ert-explain-not-equal '(1 48) '(1 49))
                 '(list-elt 1 (different-atoms (48 "#x30" "?0")
                                               (49 "#x31" "?1")))))
  (should (equal (ert-explain-not-equal 'nil '(a))
                 '(different-types nil (a))))
  (should (equal (ert-explain-not-equal '(a b c) '(a b c d))
                 '(proper-lists-of-different-length 3 4 (a b c) (a b c d)))))

(provide 'ert-tests)

;;; ert-tests.el ends here
