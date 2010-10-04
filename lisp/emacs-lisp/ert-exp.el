;;; ert-exp.el --- Staging area for experimental extensions to ERT

;; Copyright (C) 2008 Phil Hagelberg

;; Author: Phil Hagelberg
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
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

;;; Commentary:

;; This file includes some extra helper functions to use while writing
;; automated tests with ERT.  These have been proposed as extensions
;; to ERT but are not mature yet and likely to change.
;;
;; Since it is not meant to be loaded during normal use, this file
;; includes functions that are not prefixed for readability's sake.

;;; Code:

(eval-when-compile
  (require 'cl))
(require 'ert-ui)

(defmacro buffer-changes-p (&rest body)
  "Execute BODY and return true if it changed the buffer content."
  (let ((original-buffer-content (make-symbol "original-buffer-content")))
    `(let ((,original-buffer-content (buffer-string)))
       (progn ,@body
              (not (string= ,original-buffer-content
                            (buffer-string)))))))

(defun buffer-contains-p (regexp &optional buffer)
  "Return true if REGEXP is found anywhere in BUFFER (default current buffer)."
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (goto-char (point-min))
      (not (not (search-forward-regexp regexp nil t))))))

;; TODO(ohler): modify this to allow better error reporting in the
;; case of failure, e.g., by changing it to
;; `should-be-correctly-indented' or
;; `buffer-contents-after-reindentation', or add an explainer.
(defun correctly-indented-p (&optional buffer)
  "Return true if BUFFER is indented the way Emacs would indent it.

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


;;; Test buffers.

(defun ert--text-button (string &rest properties)
  "Return a string containing STRING as a text button with PROPERTIES.

See `make-text-button'."
  (with-temp-buffer
    (insert string)
    (apply #'make-text-button (point-min) (point-max) properties)
    (buffer-string)))

(defun ert--format-test-buffer-name (base-name)
  (format "*Test buffer (%s)%s*"
	  (or (and (ert-running-test)
		   (ert-test-name (ert-running-test)))
	      "<anonymous test>")
	  (if base-name
	      (format ": %s" base-name)
	    "")))

(defvar ert--test-buffers (make-hash-table :weakness t)
  "Table of all test buffers.  Keys are the buffer objects, values are t.

The main use of this table is for `ert-kill-all-test-buffers'.
Not all buffers in this table are necessarily live, but all live
test buffers are in this table.")

(define-button-type 'ert--test-buffer-button
  'action #'ert--test-buffer-button-action
  'help-echo "mouse-2, RET: Pop to test buffer")

(defun ert--test-buffer-button-action (button)
  (pop-to-buffer (button-get button 'ert--test-buffer)))

(defun ert--run-with-test-buffer (ert--base-name ert--thunk)
  "Helper function for `ert-with-test-buffer'."
  (let* ((ert--buffer (generate-new-buffer
                       (ert--format-test-buffer-name ert--base-name)))
         (ert--button (ert--text-button (buffer-name ert--buffer)
                                        :type 'ert--test-buffer-button
                                        'ert--test-buffer ert--buffer)))
    (puthash ert--buffer 't ert--test-buffers)
    ;; We don't use `unwind-protect' here since we want to kill the
    ;; buffer only on success.
    (prog1 (with-current-buffer ert--buffer
             (ert-info (ert--button :prefix "Buffer: ")
               (funcall ert--thunk)))
      (kill-buffer ert--buffer)
      (remhash ert--buffer ert--test-buffers))))

(defmacro* ert-with-test-buffer ((&key ((:name name-form)))
				 &body body)
  "Create a test buffer and run BODY in that buffer.

To be used in ERT tests.  If BODY finishes successfully, the test
buffer is killed; if there is an error, the test buffer is kept
around on error for further inspection.  Its name is derived from
the name of the test and the result of NAME-FORM."
  (declare (debug ((form) body))
           (indent 1))
  `(ert--run-with-test-buffer ,name-form (lambda () ,@body)))

;; We use these `put' forms in addition to the (declare (indent)) in
;; the defmacro form since the `declare' alone does not lead to
;; correct indentation before the .el/.elc file is loaded.
;; Autoloading these `put' forms solves this.
;;;###autoload
(progn
  ;; TODO(ohler): Figure out what these mean and make sure they are correct.
  (put 'ert-with-test-buffer 'lisp-indent-function 1))

(defun ert-kill-all-test-buffers ()
  "Kill all test buffers that are still live."
  (interactive)
  (let ((count 0))
    (maphash (lambda (buffer dummy)
	       (when (or (not (buffer-live-p buffer))
			 (kill-buffer buffer))
		 (incf count)))
	     ert--test-buffers)
    (message "%s out of %s test buffers killed"
	     count (hash-table-count ert--test-buffers)))
  ;; It could be that some test buffers were actually kept alive
  ;; (e.g., due to `kill-buffer-query-functions').  I'm not sure what
  ;; to do about this.  For now, let's just forget them.
  (clrhash ert--test-buffers)
  nil)


;;; Simulate commands.

(defvar ert-simulate-command-delay nil)

(defvar ert-simulate-command-post-hook nil
  "Normal hook to be run at end of `ert-simulate-command'.")

;; Avoid warning in old Emacsen.
(defvar last-repeatable-command)

;; Fix-me: use this in all tests where applicable.
(defun ert-simulate-command (command run-idle-timers)
  ;; Fix-me: run-idle-timers - use seconds
  ;; Fix-me: add unread-events
  "Simulate calling command COMMAND as in Emacs command loop.
If RUN-IDLE-TIMERS is non-nil then run the idle timers after
calling everything involved with the command.

COMMAND should be a list where the car is the command symbol and
the rest are arguments to the command.

NOTE: Since the command is not called by `call-interactively'
test for `called-interactively' in the command will fail.

Return the value of calling the command, ie

  (apply (car COMMAND) (cdr COMMAND)).

Run the hook `ert-simulate-command-post-hook' at the very end."

  (message "command=%s" command)
  (should (listp command))
  (should (commandp (car command)))
  (should (not unread-command-events))
  (let (return-value
        (font-lock-mode t))
    ;; For the order of things here see command_loop_1 in keyboard.c
    ;;
    ;; The command loop will reset the command related variables so
    ;; there is no reason to let bind them. They are set here however
    ;; to be able to test several commands in a row and how they
    ;; affect each other.
    (setq deactivate-mark nil)
    (setq this-original-command (car command))
    ;; remap through active keymaps
    (setq this-command (or (command-remapping this-original-command)
                           this-original-command))
    (run-hooks 'pre-command-hook)
    (setq return-value (apply (car command) (cdr command))) ;; <-----
    (message "post-command-hook=%s" post-command-hook)
    (run-hooks 'post-command-hook)
    (when deferred-action-list
      (run-hooks 'deferred_action_function))
    (setq real-last-command (car command))
    (setq last-repeatable-command real-last-command)
    (setq last-command this-command)
    (when (and deactivate-mark transient-mark-mode) (deactivate-mark))
    ;;(message "ert-simulate-command.before idle-timers, point=%s" (point))
    (when run-idle-timers
      ;;(dolist (timer (copy-list timer-idle-list))
      (dolist (timer (copy-sequence timer-idle-list))
        (timer-event-handler timer)
        ;;(message "   after timer=%s, point=%s" timer (point))
        )
      (redisplay t))
    ;;(message "ert-simulate-command.after  idle-timers, point=%s" (point))
    (when ert-simulate-command-delay
      ;; Show user
      ;;(message "After M-x %s" command)
      (let ((old-buffer-name (buffer-name)))
        (rename-buffer (propertize (format "After M-x %s" (car command))
                                   'face 'highlight)
                       t)
        (sit-for ert-simulate-command-delay)
        (rename-buffer old-buffer-name)))
    (should (not unread-command-events))
    (run-hooks 'ert-simulate-command-post-hook)
    return-value))


;;; More general utilities.

(defun ert-filter-string (s &rest regexps)
  "Return a copy of S with all matches of REGEXPS removed.

Elements of REGEXPS may also be two-element lists \(REGEXP
SUBEXP\), where SUBEXP is the number of a subexpression in
REGEXP.  In that case, only that subexpression will be removed
rather than the entire match."
  ;; Use a temporary buffer since replace-match copies strings, which
  ;; would lead to N^2 runtime.
  (with-temp-buffer
    (insert s)
    (dolist (x regexps)
      (destructuring-bind (regexp subexp) (if (listp x) x `(,x nil))
        (goto-char (point-min))
        (while (re-search-forward regexp nil t)
          (replace-match "" t t nil subexp))))
    (buffer-string)))


(defun ert-propertized-string (&rest args)
  "Return a string with properties as specified by ARGS.

ARGS is a list of strings and plists.  The strings in ARGS are
concatenated to produce an output string.  In the output string,
each string from ARGS will be have the preceding plist as its
property list, or no properties if there is no plist before it.

As a simple example,

\(ert-propertized-string \"foo \" '(face italic) \"bar\" \" baz\" nil \" quux\"\)

would return the string \"foo bar baz quux\" where the substring
\"bar baz\" has a `face' property with the value `italic'.

None of the ARGS are modified, but the return value may share
structure with the plists in ARGS."
  (with-temp-buffer
    (loop with current-plist = nil
          for x in args do
          (etypecase x
            (string (let ((begin (point)))
                      (insert x)
                      (set-text-properties begin (point) current-plist)))
            (list (unless (zerop (mod (length x) 2))
                    (error "Odd number of args in plist: %S" x))
                  (setq current-plist x))))
    (buffer-string)))


(defun ert-call-with-buffer-renamed (buffer-name thunk)
  "Protect the buffer named BUFFER-NAME from side-effects and run THUNK.

Renames the buffer BUFFER-NAME to a new temporary name, creates a
new buffer named BUFFER-NAME, executes THUNK, kills the new
buffer, and renames the original buffer back to BUFFER-NAME.

This is useful if THUNK has undesirable side-effects on an Emacs
buffer with a fixed name such as *Messages*."
  (lexical-let ((new-buffer-name (generate-new-buffer-name
                                  (format "%s orig buffer" buffer-name))))
    (with-current-buffer (get-buffer-create buffer-name)
      (rename-buffer new-buffer-name))
    (unwind-protect
        (progn
          (get-buffer-create buffer-name)
          (funcall thunk))
      (when (get-buffer buffer-name)
        (kill-buffer buffer-name))
      (with-current-buffer new-buffer-name
        (rename-buffer buffer-name)))))

(defmacro* ert-with-buffer-renamed ((buffer-name-form) &body body)
  "Protect the buffer named BUFFER-NAME from side-effects and run BODY.

See `ert-call-with-buffer-renamed' for details."
  (declare (indent 1))
  `(ert-call-with-buffer-renamed ,buffer-name-form (lambda () ,@body)))


(provide 'ert-exp)

;;; ert-exp.el ends here
