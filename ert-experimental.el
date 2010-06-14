;;; ert-experimental.el --- Staging area for proposed extensions to ERT

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

;; This file includes some extra helper functions to use while writing
;; automated tests with ERT.  These have been proposed as extensions
;; to ERT but are not mature yet and likely to change.
;;
;; Since it is not meant to be loaded during normal use, this file
;; includes functions that are not prefixed for readability's sake.

;;; Code:

(defmacro buffer-changes-p (&rest body)
  "Execute BODY and return true if it changed the buffer content."
  (let ((original-buffer-content (make-symbol "original-buffer-content")))
    `(let ((,original-buffer-content (buffer-string)))
       (progn ,@body
              (not (string= ,original-buffer-content
                            (buffer-string)))))))

(defun buffer-contains-p (regexp &optional buffer)
  "Return true if regexp is found anywhere in BUFFER (default current buffer)."
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (goto-char (point-min))
      (not (not (search-forward-regexp regexp nil t))))))
  
;; TODO(ohler): modify this to allow better error reporting in the
;; case of failure, e.g., by changing it to
;; `should-be-correctly-indented' or
;; `buffer-contents-after-reindentation', or add an explainer.
(defun correctly-indented-p (&optional buffer)
  "Returns true if BUFFER is indented the way Emacs would indent it.

BUFFER defaults to current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (with-current-buffer
        (let ((buffer-file-name nil)) ; otherwise clone-buffer would refuse
          (clone-buffer))
      (unwind-protect
          (let ((buffer-original-indentation (buffer-string)))
            (indent-region (point-min) (point-max))
            (let ((buffer-new-indentation (buffer-string)))
              (string= buffer-original-indentation buffer-new-indentation)))
        (let ((kill-buffer-query-functions nil))
          (kill-buffer nil))))))

(defun ert-test-buffer-substitute (string fn)
  "Removes the all occurrences of STRING in the buffer
and runs FN with at that point each one is removed.

Backslash-escaped STRINGs are unescaped and ignored."
  (let ((len (length string)))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward string nil t)
        (save-excursion
          (backward-char len)
          (if (eq (char-before (point)) ?\\) (delete-char -1)
            (delete-char len)
            (funcall fn)))))))

(defmacro with-test-buffer (contents &rest body)
  "Runs BODY in a buffer containing CONTENTS.

The mark may be set in the buffer using the string \"<mark>\".
This can be escaped with a backslash to unclude it literally."
  `(with-temp-buffer
     (insert ,contents)
     (goto-char (point-min))

     (let ((new-mark))
       (ert-test-buffer-substitute "<mark>" (lambda () (setq new-mark (point))))
       (set-mark new-mark))

     (let ((new-point (point)))
       (ert-test-buffer-substitute "<point>"
                                   (lambda () (setq new-point (point))))
       (goto-char new-point))
     ,@body))
(put 'with-test-buffer 'lisp-indent-function 1)


(provide 'ert-experimental)

;;; ert-experimental.el ends here
