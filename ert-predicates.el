;;; ert-predicates.el --- Extra Predicates for Emacs Lisp Regression Testing

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

;;; Commentary:

;; This file includes some extra higher-level predicates to use while
;; writing automated tests with ert.el.

;; Since it is not meant to be loaded during normal use, this file
;; includes functions that are not prefixed.

;;; Code:

(defmacro buffer-changes-p (&rest body)
  "Return t if the body changes the buffer contents."
  `(let ((buffer-changed-init-value (buffer-string)))
     (unwind-protect (progn ,@body)
       (string= buffer-changed-init-value
                (buffer-string)))))

(defmacro error-p (&rest body)
  "Return t if the body signals an error."
  `(condition-case err
       (progn
         ,@body
         nil
     (error t))))

(defun buffer-contains-p (regexp &optional buffer)
  "Return t if contents of buffer (defaults to current) matches regexp."
  (save-excursion
    (if buffer (switch-to-buffer buffer))
    (not (not (search-forward-regexp regexp nil t)))))

(defun correctly-indented-p (filename)
  "Returns t if the buffer is already indented the way Emacs would indent it."
  (save-excursion
    (find-file filename)
    (let ((buffer-original-indentation (buffer-string))
          (kill-buffer-query-functions nil))
      (indent-region (point-min) (point-max))
      (let ((buffer-new-indentation (buffer-string)))
        (revert-buffer nil t)
        (kill-buffer nil)
        (string= buffer-original-indentation buffer-new-indentation)))))

(provide 'ert-predicates)
;;; ert-predicates.el ends here