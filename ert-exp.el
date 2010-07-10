;;; ert-exp.el --- Staging area for experimental extensions to ERT

;; Copyright (C) 2008 Phil Hagelberg

;; Author: Phil Hagelberg
;; Author: Lennart Borgman (lennart O borgman A gmail O com)

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

(defun ert-test-buffer-substitute (string fn)
  "Remove all occurrences of STRING in current buffer and call FN for each.

Removes the all occurrences of STRING in the buffer and runs FN
at each point where one is removed.

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
  "In a temporary buffer containing CONTENTS, run BODY.

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


;;; Test buffers

(defvar ert-temp-test-buffer-test nil)
(make-variable-buffer-local 'ert-temp-test-buffer-test)
(put 'ert-temp-test-buffer-test 'permanent-local t)

(defvar ert-temp-test-buffer-file nil)
(make-variable-buffer-local 'ert-temp-test-buffer-file)
(put 'ert-temp-test-buffer-file 'permanent-local t)

(defvar ert-failed-tests-temp-buffers nil)

(defvar ert-list-failed-buffers-name "*Ert Failed Test Buffers*")

(defun ert-kill-temp-test-buffers ()
  "Delete test buffers from unsuccessful tests."
  (interactive)
  (let ((failed (get-buffer ert-list-failed-buffers-name)))
    (when failed (kill-buffer failed)))
  (dolist (buf ert-failed-tests-temp-buffers)
    (when (buffer-live-p buf)
      (kill-buffer buf)))
  (setq ert-failed-tests-temp-buffers nil))

(defun ert-list-temp-test-buffers ()
  "List test buffers from unsuccessful tests."
  (interactive)
  (setq ert-failed-tests-temp-buffers
        (delq nil
              (mapcar (lambda (buf)
                        (when (buffer-live-p buf)
                          buf))
                      ert-failed-tests-temp-buffers)))
  (let ((ert-buffer (get-buffer "*ert*"))
        (buffers ert-failed-tests-temp-buffers))
    (when ert-buffer (setq buffers (cons ert-buffer buffers)))
    (switch-to-buffer
     (let ((Buffer-menu-buffer+size-width 40))
       (list-buffers-noselect nil buffers)))
    (rename-buffer ert-list-failed-buffers-name t))
  (unless ert-failed-tests-temp-buffers
    (message "No test buffers from unsuccessful tests")))

(defvar ert-temp-test-buffer-minor-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Add menu bar entries for test buffer and test function
    (define-key map [(control ?c) ?? ?t] 'ert-temp-test-buffer-go-test)
    (define-key map [(control ?c) ?? ?f] 'ert-temp-test-buffer-go-file)
    map))
(defun ert-temp-test-buffer-go-test ()
  (interactive)
  (ert-find-test-other-window ert-temp-test-buffer-test))
(defun ert-temp-test-buffer-go-file ()
  (interactive)
  (find-file-other-window ert-temp-test-buffer-file))

(define-minor-mode ert-temp-test-buffer-minor-mode
  "Helpers for those buffers ..."
  )
(put 'ert-temp-test-buffer-minor-mode 'permanent-local t)


(defmacro* ert-with-temp-buffer-include-file (file-name-form &body body)
  "Insert FILE-NAME-FORM in a temporary buffer and eval BODY.
If success then delete the temporary buffer, otherwise keep it.

To access these temporary test buffers use
- `ert-list-temp-test-buffers': list them
- `ert-kill-temp-test-buffers': delete them"
  (declare (indent 1) (debug t))
  (let ((file-name (make-symbol "file-name-")))
    `(let* ((,file-name (ert-get-test-file-name ,file-name-form))
            (mode-line-buffer-identification
             (list (propertize "%b" 'face 'highlight)))
            ;; Give the buffer a name that allows us to switch to it
            ;; quickly when debugging a failure.
            (temp-buf
             (generate-new-buffer
              (format "%s" (ert-running-test)))))
       (unless (file-readable-p ,file-name)
         (if (file-exists-p ,file-name)
             (error "Can't read %s" ,file-name)
           (error "Can't find %s" ,file-name)))
       (message "Testing with file %s" ,file-name)
       (push temp-buf ert-failed-tests-temp-buffers)
       (with-current-buffer temp-buf
         (ert-temp-test-buffer-minor-mode 1)
         (setq ert-temp-test-buffer-file ,file-name)
         (setq ert-temp-test-buffer-test (ert-running-test))
         ;; Avoid global font lock
         (let ((font-lock-global-modes nil))
           ;; Turn off font lock in buffer
           (font-lock-mode -1)
           (when (> emacs-major-version 22)
             (assert (not font-lock-mode) t
                     "%s %s" "in ert-with-temp-buffer-include-file"))
           (insert-file-contents ,file-name)
           (save-window-excursion
             ;; Switch to buffer so it will show immediately when
             ;; debugging a failure.
             (switch-to-buffer-other-window (current-buffer))
             ,@body)
           ;; Fix-me: move to success list?
           (kill-buffer temp-buf))))))


;;; Simulate commands

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

ARGS is a list of strings and plists.  It is processed from left
to right to produce the output string.  Each string in ARGS is
inserted with properties that are determined by the preceding
plists in ARGS: It is inserted with properties that are the
combination of all plists that preceded it, where the rightmost
plist's value wins if multiple elements specify a value for the
same key, and plist keys with nil values are dropped.

As a simple example,

\(ert-propertized-string \"foo \" '(face italic) \"bar\" '(face nil) \" baz\"\)

would return the string \"foo bar baz\" where the substring
\"bar\" has the property face=italic.

None of the ARGS are modified."
  (with-temp-buffer
    (loop with plist-holder =
          ;; hacky but we don't have cl
          (make-symbol "plist-holder")
          for x in args do
          (etypecase x
            (string (let ((begin (point)))
                      (insert x)
                      (set-text-properties begin (point)
                                           (copy-sequence
                                            (symbol-plist plist-holder)))))
            (list (unless (zerop (mod (length x) 2))
                    (error "Odd number of args in plist: %S" x))
                  (loop for (key value . rest) on x by #'cddr do
                        (ert-remprop plist-holder key)
                        (unless (null value)
                          (put plist-holder key value))))))
    (buffer-string)))

(provide 'ert-exp)

;;; ert-exp.el ends here
