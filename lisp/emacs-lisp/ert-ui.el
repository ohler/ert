;;; ert-ui.el --- ERT's interactive UI

;; Copyright (C) 2007, 2008, 2010 Free Software Foundation, Inc.

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

;; This file is part of ERT, the Emacs Lisp Regression Testing tool.
;; See ert.el or the texinfo manual for more details.

;;; Code:

(eval-when-compile
  (require 'cl))
(require 'ert)
(require 'ert-run)
(require 'easymenu)
(require 'ewoc)
(require 'help)
(require 'button)


;;; UI customization options.

(defgroup ert ()
  "ERT, the Emacs Lisp regression testing tool."
  :prefix "ert-"
  :group 'lisp)

(defface ert-test-result-expected '((((class color) (background light))
                                     :background "green1")
                                    (((class color) (background dark))
                                     :background "green3"))
  "Face used for expected results in the ERT results buffer."
  :group 'ert)

(defface ert-test-result-unexpected '((((class color) (background light))
                                       :background "red1")
                                      (((class color) (background dark))
                                       :background "red3"))
  "Face used for unexpected results in the ERT results buffer."
  :group 'ert)


;;; Some basic interactive functions.

(defun ert-read-test-name (prompt &optional default history
                                  add-default-to-prompt)
  "Read the name of a test and return it as a symbol.

Prompt with PROMPT.  If DEFAULT is a valid test name, use it as a
default.  HISTORY is the history to use; see `completing-read'.
If ADD-DEFAULT-TO-PROMPT is non-nil, PROMPT will be modified to
include the default, if any.

Signals an error if no test name was read."
  (etypecase default
    (string (let ((symbol (intern-soft default)))
              (unless (and symbol (ert-test-boundp symbol))
                (setq default nil))))
    (symbol (setq default
                  (if (ert-test-boundp default)
                      (symbol-name default)
                    nil)))
    (ert-test (setq default (ert-test-name default))))
  (when add-default-to-prompt
    (setq prompt (if (null default)
                     (format "%s: " prompt)
                   (format "%s (default %s): " prompt default))))
  (let ((input (completing-read prompt obarray #'ert-test-boundp
                                t nil history default nil)))
    ;; completing-read returns an empty string if default was nil and
    ;; the user just hit enter.
    (let ((sym (intern-soft input)))
      (if (ert-test-boundp sym)
          sym
        (error "Input does not name a test")))))

(defun ert-read-test-name-at-point (prompt)
  "Read the name of a test and return it as a symbol.
As a default, use the symbol at point, or the test at point if in
the ERT results buffer.  Prompt with PROMPT, augmented with the
default (if any)."
  (ert-read-test-name prompt (ert-test-at-point) nil t))

(defun ert-find-test-other-window (test-name)
  "Find, in another window, the definition of TEST-NAME."
  (interactive (list (ert-read-test-name-at-point "Find test definition: ")))
  (find-function-do-it test-name 'ert-deftest 'switch-to-buffer-other-window))

(defun ert-delete-test (test-name)
  "Make the test TEST-NAME unbound.

Nothing more than an interactive interface to `ert-make-test-unbound'."
  (interactive (list (ert-read-test-name-at-point "Delete test")))
  (ert-make-test-unbound test-name))

(defun ert-delete-all-tests ()
  "Make all symbols in `obarray' name no test."
  (interactive)
  (when (interactive-p)
    (unless (y-or-n-p "Delete all tests? ")
      (error "Aborted")))
  ;; We can't use `ert-select-tests' here since that gives us only
  ;; test objects, and going from them back to the test name symbols
  ;; can fail if the `ert-test' defstruct has been redefined.
  (mapc #'ert-make-test-unbound (apropos-internal "" #'ert-test-boundp))
  t)


;;; Display of test progress and results.

;; An entry in the results buffer ewoc.  There is one entry per test.
(defstruct ert--ewoc-entry
  (test (assert nil))
  ;; If the result of this test was expected, its ewoc entry is hidden
  ;; initially.
  (hidden-p (assert nil))
  ;; An ewoc entry may be collapsed to hide details such as the error
  ;; condition.
  ;;
  ;; I'm not sure the ability to expand and collapse entries is still
  ;; a useful feature.
  (expanded-p t)
  ;; By default, the ewoc entry presents the error condition with
  ;; certain limits on how much to print (`print-level',
  ;; `print-length').  The user can interactively switch to a set of
  ;; higher limits.
  (extended-printer-limits-p nil))

;; Variables local to the results buffer.

;; The ewoc.
(defvar ert--results-ewoc)
;; The stats object.
(defvar ert--results-stats)
;; A string with one character per test.  Each character represents
;; the result of the corresponding test.  The string is displayed near
;; the top of the buffer and serves as a progress bar.
(defvar ert--results-progress-bar-string)
;; The position where the progress bar button begins.
(defvar ert--results-progress-bar-button-begin)
;; The test result listener that updates the buffer when tests are run.
(defvar ert--results-listener)

(defun ert-insert-test-name-button (test-name)
  "Insert a button that links to TEST-NAME."
  (insert-text-button (format "%S" test-name)
                      :type 'ert--test-name-button
                      'ert-test-name test-name))

(defun ert--results-format-expected-unexpected (expected unexpected)
  "Return a string indicating EXPECTED expected results, UNEXPECTED unexpected."
  (if (zerop unexpected)
      (format "%s" expected)
    (format "%s (%s unexpected)" (+ expected unexpected) unexpected)))

(defun ert--results-update-ewoc-hf (ewoc stats)
  "Update the header and footer of EWOC to show certain information from STATS.

Also sets `ert--results-progress-bar-button-begin'."
  (let ((run-count (ert-stats-completed stats))
        (results-buffer (current-buffer))
        ;; Need to save buffer-local value.
        (font-lock font-lock-mode))
    (ewoc-set-hf
     ewoc
     ;; header
     (with-temp-buffer
       (insert "Selector: ")
       (ert--insert-human-readable-selector (ert--stats-selector stats))
       (insert "\n")
       (insert
        (format (concat "Passed: %s\n"
                        "Failed: %s\n"
                        "Total:  %s/%s\n\n")
                (ert--results-format-expected-unexpected
                 (ert--stats-passed-expected stats)
                 (ert--stats-passed-unexpected stats))
                (ert--results-format-expected-unexpected
                 (ert--stats-failed-expected stats)
                 (ert--stats-failed-unexpected stats))
                run-count
                (ert-stats-total stats)))
       (insert
        (format "Started at:   %s\n"
                (ert--format-time-iso8601 (ert--stats-start-time stats))))
       ;; FIXME: This is ugly.  Need to properly define invariants of
       ;; the `stats' data structure.
       (let ((state (cond ((ert--stats-aborted-p stats) 'aborted)
                          ((ert--stats-current-test stats) 'running)
                          ((ert--stats-end-time stats) 'finished)
                          (t 'preparing))))
         (ecase state
           (preparing
            (insert ""))
           (aborted
            (cond ((ert--stats-current-test stats)
                   (insert "Aborted during test: ")
                   (ert-insert-test-name-button
                    (ert-test-name (ert--stats-current-test stats))))
                  (t
                   (insert "Aborted."))))
           (running
            (assert (ert--stats-current-test stats))
            (insert "Running test: ")
            (ert-insert-test-name-button (ert-test-name
                                          (ert--stats-current-test stats))))
           (finished
            (assert (not (ert--stats-current-test stats)))
            (insert "Finished.")))
         (insert "\n")
         (if (ert--stats-end-time stats)
             (insert
              (format "%s%s\n"
                      (if (ert--stats-aborted-p stats)
                          "Aborted at:   "
                        "Finished at:  ")
                      (ert--format-time-iso8601 (ert--stats-end-time stats))))
           (insert "\n"))
         (insert "\n"))
       (let ((progress-bar-string (with-current-buffer results-buffer
                                    ert--results-progress-bar-string)))
         (let ((progress-bar-button-begin
                (insert-text-button progress-bar-string
                                    :type 'ert--results-progress-bar-button
                                    'face (or (and font-lock
                                                   (ert-face-for-stats stats))
                                              'button))))
           ;; The header gets copied verbatim to the results buffer,
           ;; and all positions remain the same, so
           ;; `progress-bar-button-begin' will be the right position
           ;; even in the results buffer.
           (with-current-buffer results-buffer
             (set (make-local-variable 'ert--results-progress-bar-button-begin)
                  progress-bar-button-begin))))
       (insert "\n\n")
       (buffer-string))
     ;; footer
     ;;
     ;; We actually want an empty footer, but that would trigger a bug
     ;; in ewoc, sometimes clearing the entire buffer.  (It's possible
     ;; that this bug has been fixed since this has been tested; we
     ;; should test it again.)
     "\n")))


(defvar ert-test-run-redisplay-interval-secs .1
  "How many seconds ERT should wait between redisplays while running tests.

While running tests, ERT shows the current progress, and this variable
determines how frequently the progress display is updated.")

(defun ert--results-update-stats-display (ewoc stats)
  "Update EWOC and the mode line to show data from STATS."
  ;; TODO(ohler): investigate using `make-progress-reporter'.
  (ert--results-update-ewoc-hf ewoc stats)
  (force-mode-line-update)
  (redisplay t)
  (setf (ert--stats-next-redisplay stats)
        (+ (float-time) ert-test-run-redisplay-interval-secs)))

(defun ert--results-update-stats-display-maybe (ewoc stats)
  "Call `ert--results-update-stats-display' if not called recently.

EWOC and STATS are arguments for `ert--results-update-stats-display'."
  (when (>= (float-time) (ert--stats-next-redisplay stats))
    (ert--results-update-stats-display ewoc stats)))

(defun ert--tests-running-mode-line-indicator ()
  "Return a string for the mode line that shows the test run progress."
  (let* ((stats ert--current-run-stats)
         (tests-total (ert-stats-total stats))
         (tests-completed (ert-stats-completed stats)))
    (if (>= tests-completed tests-total)
        (format " ERT(%s/%s,finished)" tests-completed tests-total)
      (format " ERT(%s/%s):%s"
              (1+ tests-completed)
              tests-total
              (if (null (ert--stats-current-test stats))
                  "?"
                (format "%S"
                        (ert-test-name (ert--stats-current-test stats))))))))

(defun ert--make-xrefs-region (begin end)
  "Attach cross-references to function names between BEGIN and END.

BEGIN and END specify a region in the current buffer."
  (save-excursion
    (save-restriction
      (narrow-to-region begin (point))
      ;; Inhibit optimization in `debugger-make-xrefs' that would
      ;; sometimes insert unrelated backtrace info into our buffer.
      (let ((debugger-previous-backtrace nil))
        (debugger-make-xrefs)))))

(defun ert--string-first-line (s)
  "Return the first line of S, or S if it contains no newlines.

The return value does not include the line terminator."
  (substring s 0 (ert--string-position ?\n s)))

(defun ert-face-for-test-result (expectedp)
  "Return a face that shows whether a test result was expected or unexpected.

If EXPECTEDP is nil, returns the face for unexpected results; if
non-nil, returns the face for expected results.."
  (if expectedp 'ert-test-result-expected 'ert-test-result-unexpected))

(defun ert-face-for-stats (stats)
  "Return a face that represents STATS."
  (cond ((ert--stats-aborted-p stats) 'nil)
        ((plusp (ert-stats-completed-unexpected stats))
         (ert-face-for-test-result nil))
        ((eql (ert-stats-completed-expected stats) (ert-stats-total stats))
         (ert-face-for-test-result t))
        (t 'nil)))

(defun ert--print-test-for-ewoc (entry)
  "The ewoc print function for ewoc test entries.  ENTRY is the entry to print."
  (let* ((test (ert--ewoc-entry-test entry))
         (stats ert--results-stats)
         (result (let ((pos (ert--stats-test-pos stats test)))
                   (assert pos)
                   (aref (ert--stats-test-results stats) pos)))
         (hiddenp (ert--ewoc-entry-hidden-p entry))
         (expandedp (ert--ewoc-entry-expanded-p entry))
         (extended-printer-limits-p (ert--ewoc-entry-extended-printer-limits-p
                                     entry)))
    (cond (hiddenp)
          (t
           (let ((expectedp (ert-test-result-expected-p test result)))
             (insert-text-button (format "%c" (ert-char-for-test-result
                                               result expectedp))
                                 :type 'ert--results-expand-collapse-button
                                 'face (or (and font-lock-mode
                                                (ert-face-for-test-result
                                                 expectedp))
                                           'button)))
           (insert " ")
           (ert-insert-test-name-button (ert-test-name test))
           (insert "\n")
           (when (and expandedp (not (eql result 'nil)))
             (when (ert-test-documentation test)
               (insert "    "
                       (propertize
                        (ert--string-first-line (ert-test-documentation test))
                        'font-lock-face 'font-lock-doc-face)
                       "\n"))
             (etypecase result
               (ert-test-passed
                (if (ert-test-result-expected-p test result)
                    (insert "    passed\n")
                  (insert "    passed unexpectedly\n"))
                (insert ""))
               (ert-test-result-with-condition
                (ert--insert-infos result)
                (let ((print-escape-newlines t)
                      (print-level (if extended-printer-limits-p 12 6))
                      (print-length (if extended-printer-limits-p 100 10)))
                  (insert "    ")
                  (let ((begin (point)))
                    (ert--pp-with-indentation-and-newline
                     (ert-test-result-with-condition-condition result))
                    (ert--make-xrefs-region begin (point)))))
               (ert-test-aborted-with-non-local-exit
                (insert "    aborted\n")))
             (insert "\n")))))
  nil)

(defun ert--results-font-lock-function (enabledp)
  "Redraw the ERT results buffer after font-lock-mode was switched on or off.

ENABLEDP is true if font-lock-mode is switched on, false
otherwise."
  (ert--results-update-ewoc-hf ert--results-ewoc ert--results-stats)
  (ewoc-refresh ert--results-ewoc)
  (font-lock-default-function enabledp))

(defun ert--setup-results-buffer (stats listener buffer-name)
  "Set up a test results buffer.

STATS is the stats object; LISTENER is the results listener;
BUFFER-NAME, if non-nil, is the buffer name to use."
  (unless buffer-name (setq buffer-name "*ert*"))
  (let ((buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (setq buffer-read-only t)
      (let ((inhibit-read-only t))
        (buffer-disable-undo)
        (erase-buffer)
        (ert-results-mode)
        ;; Erase buffer again in case switching out of the previous
        ;; mode inserted anything.  (This happens e.g. when switching
        ;; from ert-results-mode to ert-results-mode when
        ;; font-lock-mode turns itself off in change-major-mode-hook.)
        (erase-buffer)
        (set (make-local-variable 'font-lock-function)
             'ert--results-font-lock-function)
        (let ((ewoc (ewoc-create 'ert--print-test-for-ewoc nil nil t)))
          (set (make-local-variable 'ert--results-ewoc) ewoc)
          (set (make-local-variable 'ert--results-stats) stats)
          (set (make-local-variable 'ert--results-progress-bar-string)
               (make-string (ert-stats-total stats)
                            (ert-char-for-test-result nil t)))
          (set (make-local-variable 'ert--results-listener) listener)
          (loop for test across (ert--stats-tests stats) do
                (ewoc-enter-last ewoc
                                 (make-ert--ewoc-entry :test test :hidden-p t)))
          (ert--results-update-ewoc-hf ert--results-ewoc ert--results-stats)
          (goto-char (1- (point-max)))
          buffer)))))


(defvar ert--selector-history nil
  "List of recent test selectors read from terminal.")

;; Should OUTPUT-BUFFER-NAME and MESSAGE-FN really be arguments here?
;; They are needed only for our automated self-tests at the moment.
;; Or should there be some other mechanism?
;;;###autoload
(defun ert-run-tests-interactively (selector
                                    &optional output-buffer-name message-fn)
  "Run the tests specified by SELECTOR and display the results in a buffer.

SELECTOR works as described in `ert-select-tests'.
OUTPUT-BUFFER-NAME and MESSAGE-FN should normally be nil; they
are used for automated self-tests and specify which buffer to use
and how to display message."
  (interactive
   (list (let ((default (if ert--selector-history
                            (first ert--selector-history)
                          "t")))
           (read-from-minibuffer (if (null default)
                                     "Run tests: "
                                   (format "Run tests (default %s): " default))
                                 nil nil t 'ert--selector-history
                                 default nil))
         nil))
  (unless message-fn (setq message-fn 'message))
  (lexical-let ((output-buffer-name output-buffer-name)
                buffer
                listener
                (message-fn message-fn))
    (setq listener
          (lambda (event-type &rest event-args)
            (ecase event-type
              (run-started
               (destructuring-bind (stats) event-args
                 (setq buffer (ert--setup-results-buffer stats
                                                         listener
                                                         output-buffer-name))
                 (pop-to-buffer buffer)))
              (run-ended
               (destructuring-bind (stats abortedp) event-args
                 (funcall message-fn
                          "%sRan %s tests, %s results were as expected%s"
                          (if (not abortedp)
                              ""
                            "Aborted: ")
                          (ert-stats-total stats)
                          (ert-stats-completed-expected stats)
                          (let ((unexpected
                                 (ert-stats-completed-unexpected stats)))
                            (if (zerop unexpected)
                                ""
                              (format ", %s unexpected" unexpected))))
                 (ert--results-update-stats-display (with-current-buffer buffer
                                                      ert--results-ewoc)
                                                    stats)))
              (test-started
               (destructuring-bind (stats test) event-args
                 (with-current-buffer buffer
                   (let* ((ewoc ert--results-ewoc)
                          (pos (ert--stats-test-pos stats test))
                          (node (ewoc-nth ewoc pos)))
                     (assert node)
                     (setf (ert--ewoc-entry-test (ewoc-data node)) test)
                     (aset ert--results-progress-bar-string pos
                           (ert-char-for-test-result nil t))
                     (ert--results-update-stats-display-maybe ewoc stats)
                     (ewoc-invalidate ewoc node)))))
              (test-ended
               (destructuring-bind (stats test result) event-args
                 (with-current-buffer buffer
                   (let* ((ewoc ert--results-ewoc)
                          (pos (ert--stats-test-pos stats test))
                          (node (ewoc-nth ewoc pos)))
                     (when (ert--ewoc-entry-hidden-p (ewoc-data node))
                       (setf (ert--ewoc-entry-hidden-p (ewoc-data node))
                             (ert-test-result-expected-p test result)))
                     (aset ert--results-progress-bar-string pos
                           (ert-char-for-test-result result
                                                     (ert-test-result-expected-p
                                                      test result)))
                     (ert--results-update-stats-display-maybe ewoc stats)
                     (ewoc-invalidate ewoc node))))))))
    (ert-run-tests
     selector
     listener)))
;;;###autoload
(defalias 'ert 'ert-run-tests-interactively)


;;; Simple view mode for auxiliary information like stack traces or
;;; messages.  Mainly binds "q" for quit.

(define-derived-mode ert-simple-view-mode fundamental-mode "ERT-View"
  "Major mode for viewing auxiliary information in ERT.")

(loop for (key binding) in
      '(("q" quit-window)
        )
      do
      (define-key ert-simple-view-mode-map key binding))


;;; Commands and button actions for the results buffer.

(define-derived-mode ert-results-mode fundamental-mode "ERT-Results"
  "Major mode for viewing results of ERT test runs.")

(loop for (key binding) in
      '(;; Stuff that's not in the menu.
        ("\t" forward-button)
        ([backtab] backward-button)
        ("j" ert-results-jump-between-summary-and-result)
        ("q" quit-window)
        ("L" ert-results-toggle-printer-limits-for-test-at-point)
        ("n" ert-results-next-test)
        ("p" ert-results-previous-test)
        ;; Stuff that is in the menu.
        ("R" ert-results-rerun-all-tests)
        ("r" ert-results-rerun-test-at-point)
        ("d" ert-results-rerun-test-at-point-debugging-errors)
        ("." ert-results-find-test-at-point-other-window)
        ("b" ert-results-pop-to-backtrace-for-test-at-point)
        ("m" ert-results-pop-to-messages-for-test-at-point)
        ("l" ert-results-pop-to-should-forms-for-test-at-point)
        ("h" ert-results-describe-test-at-point)
        ("D" ert-delete-test)
        ("T" ert-results-pop-to-timings)
        )
      do
      (define-key ert-results-mode-map key binding))

(easy-menu-define ert-results-mode-menu ert-results-mode-map
  "Menu for `ert-results-mode'."
  '("ERT Results"
    ["Re-run all tests" ert-results-rerun-all-tests]
    "--"
    ["Re-run test" ert-results-rerun-test-at-point]
    ["Debug test" ert-results-rerun-test-at-point-debugging-errors]
    ["Show test definition" ert-results-find-test-at-point-other-window]
    "--"
    ["Show backtrace" ert-results-pop-to-backtrace-for-test-at-point]
    ["Show messages" ert-results-pop-to-messages-for-test-at-point]
    ["Show `should' forms" ert-results-pop-to-should-forms-for-test-at-point]
    ["Describe test" ert-results-describe-test-at-point]
    "--"
    ["Delete test" ert-delete-test]
    "--"
    ["Show execution time of each test" ert-results-pop-to-timings]
    ))

(define-button-type 'ert--results-progress-bar-button
  'action #'ert--results-progress-bar-button-action
  'help-echo "mouse-2, RET: Reveal test result")

(define-button-type 'ert--test-name-button
  'action #'ert--test-name-button-action
  'help-echo "mouse-2, RET: Find test definition")

(define-button-type 'ert--results-expand-collapse-button
  'action #'ert--results-expand-collapse-button-action
  'help-echo "mouse-2, RET: Expand/collapse test result")

(defun ert--results-test-node-or-null-at-point ()
  "If point is on a valid ewoc node, return it; return nil otherwise.

To be used in the ERT results buffer."
  (let* ((ewoc ert--results-ewoc)
         (node (ewoc-locate ewoc)))
    ;; `ewoc-locate' will return an arbitrary node when point is on
    ;; header or footer, or when all nodes are invisible.  So we need
    ;; to validate its return value here.
    ;;
    ;; Update: I'm seeing nil being returned in some cases now,
    ;; perhaps this has been changed?
    (if (and node
             (>= (point) (ewoc-location node))
             (not (ert--ewoc-entry-hidden-p (ewoc-data node))))
        node
      nil)))

(defun ert--results-test-node-at-point ()
  "If point is on a valid ewoc node, return it; signal an error otherwise.

To be used in the ERT results buffer."
  (or (ert--results-test-node-or-null-at-point)
      (error "No test at point")))

(defun ert-results-next-test ()
  "Move point to the next test.

To be used in the ERT results buffer."
  (interactive)
  (ert--results-move (ewoc-locate ert--results-ewoc) 'ewoc-next
                     "No tests below"))

(defun ert-results-previous-test ()
  "Move point to the previous test.

To be used in the ERT results buffer."
  (interactive)
  (ert--results-move (ewoc-locate ert--results-ewoc) 'ewoc-prev
                     "No tests above"))

(defun ert--results-move (node ewoc-fn error-message)
  "Move point from NODE to the previous or next node.

EWOC-FN specifies the direction and should be either `ewoc-prev'
or `ewoc-next'.  If there are no more nodes in that direction, an
error is signalled with the message ERROR-MESSAGE."
  (loop
   (setq node (funcall ewoc-fn ert--results-ewoc node))
   (when (null node)
     (error "%s" error-message))
   (unless (ert--ewoc-entry-hidden-p (ewoc-data node))
     (goto-char (ewoc-location node))
     (return))))

(defun ert--results-expand-collapse-button-action (button)
  "Expand or collapse the test node BUTTON belongs to."
  (let* ((ewoc ert--results-ewoc)
         (node (save-excursion
                 (goto-char (ert--button-action-position))
                 (ert--results-test-node-at-point)))
         (entry (ewoc-data node)))
    (setf (ert--ewoc-entry-expanded-p entry)
          (not (ert--ewoc-entry-expanded-p entry)))
    (ewoc-invalidate ewoc node)))

(defun ert-results-find-test-at-point-other-window ()
  "Find the definition of the test at point in another window.

To be used in the ERT results buffer."
  (interactive)
  (let ((name (ert-test-at-point)))
    (unless name
      (error "No test at point"))
    (ert-find-test-other-window name)))

(defun ert--test-name-button-action (button)
  "Find the definition of the test BUTTON belongs to, in another window."
  (let ((name (button-get button 'ert-test-name)))
    (ert-find-test-other-window name)))

(defun ert--ewoc-position (ewoc node)
  ;; checkdoc-order: nil
  "Return the position of NODE in EWOC, or nil if NODE is not in EWOC."
  (loop for i from 0
        for node-here = (ewoc-nth ewoc 0) then (ewoc-next ewoc node-here)
        do (when (eql node node-here)
             (return i))
        finally (return nil)))

(defun ert-results-jump-between-summary-and-result ()
  "Jump back and forth between the test run summary and individual test results.

From an ewoc node, jumps to the character that represents the
same test in the progress bar, and vice versa.

To be used in the ERT results buffer."
  ;; Maybe this command isn't actually needed much, but if it is, it
  ;; seems like an indication that the UI design is not optimal.  If
  ;; jumping back and forth between a summary at the top of the buffer
  ;; and the error log in the remainder of the buffer is useful, then
  ;; the summary apparently needs to be easily accessible from the
  ;; error log, and perhaps it would be better to have it in a
  ;; separate buffer to keep it visible.
  (interactive)
  (let ((ewoc ert--results-ewoc)
        (progress-bar-begin ert--results-progress-bar-button-begin))
    (cond ((ert--results-test-node-or-null-at-point)
           (let* ((node (ert--results-test-node-at-point))
                  (pos (ert--ewoc-position ewoc node)))
             (goto-char (+ progress-bar-begin pos))))
          ((and (<= progress-bar-begin (point))
                (< (point) (button-end (button-at progress-bar-begin))))
           (let* ((node (ewoc-nth ewoc (- (point) progress-bar-begin)))
                  (entry (ewoc-data node)))
             (when (ert--ewoc-entry-hidden-p entry)
               (setf (ert--ewoc-entry-hidden-p entry) nil)
               (ewoc-invalidate ewoc node))
             (ewoc-goto-node ewoc node)))
          (t
           (goto-char progress-bar-begin)))))

(defun ert-test-at-point ()
  "Return the name of the test at point as a symbol, or nil if none."
  (or (and (eql major-mode 'ert-results-mode)
           (let ((test (ert--results-test-at-point-no-redefinition)))
             (and test (ert-test-name test))))
      (let* ((thing (thing-at-point 'symbol))
             (sym (intern-soft thing)))
        (and (ert-test-boundp sym)
             sym))))

(defun ert--results-test-at-point-no-redefinition ()
  "Return the test at point, or nil.

To be used in the ERT results buffer."
  (assert (eql major-mode 'ert-results-mode))
  (if (ert--results-test-node-or-null-at-point)
      (let* ((node (ert--results-test-node-at-point))
             (test (ert--ewoc-entry-test (ewoc-data node))))
        test)
    (let ((progress-bar-begin ert--results-progress-bar-button-begin))
      (when (and (<= progress-bar-begin (point))
                 (< (point) (button-end (button-at progress-bar-begin))))
        (let* ((test-index (- (point) progress-bar-begin))
               (test (aref (ert--stats-tests ert--results-stats)
                           test-index)))
          test)))))

(defun ert--results-test-at-point-allow-redefinition ()
  "Look up the test at point, and check whether it has been redefined.

To be used in the ERT results buffer.

Returns a list of two elements: the test (or nil) and a symbol
specifying whether the test has been redefined.

If a new test has been defined with the same name as the test at
point, replaces the test at point with the new test, and returns
the new test and the symbol `redefined'.

If the test has been deleted, returns the old test and the symbol
`deleted'.

If the test is still current, returns the test and the symbol nil.

If there is no test at point, returns a list with two nils."
  (let ((test (ert--results-test-at-point-no-redefinition)))
    (cond ((null test)
           `(nil nil))
          ((null (ert-test-name test))
           `(,test nil))
          (t
           (let* ((name (ert-test-name test))
                  (new-test (and (ert-test-boundp name)
                                 (ert-get-test name))))
             (cond ((eql test new-test)
                    `(,test nil))
                   ((null new-test)
                    `(,test deleted))
                   (t
                    (ert--results-update-after-test-redefinition
                     (ert--stats-test-pos ert--results-stats test)
                     new-test)
                    `(,new-test redefined))))))))

(defun ert--results-update-after-test-redefinition (pos new-test)
  "Update results buffer after the test at pos POS has been redefined.

Also updates the stats object.  NEW-TEST is the new test
definition."
  (let* ((stats ert--results-stats)
         (ewoc ert--results-ewoc)
         (node (ewoc-nth ewoc pos))
         (entry (ewoc-data node)))
    (ert--stats-set-test-and-result stats pos new-test nil)
    (setf (ert--ewoc-entry-test entry) new-test
          (aref ert--results-progress-bar-string pos) (ert-char-for-test-result
                                                       nil t))
    (ewoc-invalidate ewoc node))
  nil)

(defun ert--button-action-position ()
  "The buffer position where the last button action was triggered."
  (cond ((integerp last-command-event)
         (point))
        ((eventp last-command-event)
         (posn-point (event-start last-command-event)))
        (t (assert nil))))

(defun ert--results-progress-bar-button-action (button)
  "Jump to details for the test represented by the character clicked in BUTTON."
  (goto-char (ert--button-action-position))
  (ert-results-jump-between-summary-and-result))

(defun ert-results-rerun-all-tests ()
  "Re-run all tests, using the same selector.

To be used in the ERT results buffer."
  (interactive)
  (assert (eql major-mode 'ert-results-mode))
  (let ((selector (ert--stats-selector ert--results-stats)))
    (ert-run-tests-interactively selector (buffer-name))))

(defun ert-results-rerun-test-at-point ()
  "Re-run the test at point.

To be used in the ERT results buffer."
  (interactive)
  (destructuring-bind (test redefinition-state)
      (ert--results-test-at-point-allow-redefinition)
    (when (null test)
      (error "No test at point"))
    (let* ((stats ert--results-stats)
           (progress-message (format "Running %stest %S"
                                     (ecase redefinition-state
                                       ((nil) "")
                                       (redefined "new definition of ")
                                       (deleted "deleted "))
                                     (ert-test-name test))))
      ;; Need to save and restore point manually here: When point is on
      ;; the first visible ewoc entry while the header is updated, point
      ;; moves to the top of the buffer.  This is undesirable, and a
      ;; simple `save-excursion' doesn't prevent it.
      (let ((point (point)))
        (unwind-protect
            (unwind-protect
                (progn
                  (message "%s..." progress-message)
                  (ert-run-or-rerun-test stats test
                                         ert--results-listener))
              (ert--results-update-stats-display ert--results-ewoc stats)
              (message "%s...%s"
                       progress-message
                       (let ((result (ert-test-most-recent-result test)))
                         (ert-string-for-test-result
                          result (ert-test-result-expected-p test result)))))
          (goto-char point))))))

(defun ert-results-rerun-test-at-point-debugging-errors ()
  "Re-run the test at point with `ert-debug-on-error' bound to t.

To be used in the ERT results buffer."
  (interactive)
  (let ((ert-debug-on-error t))
    (ert-results-rerun-test-at-point)))

(defun ert-results-pop-to-backtrace-for-test-at-point ()
  "Display the backtrace for the test at point.

To be used in the ERT results buffer."
  (interactive)
  (let* ((test (ert--results-test-at-point-no-redefinition))
         (stats ert--results-stats)
         (pos (ert--stats-test-pos stats test))
         (result (aref (ert--stats-test-results stats) pos)))
    (etypecase result
      (ert-test-passed (error "Test passed, no backtrace available"))
      (ert-test-result-with-condition
       (let ((backtrace (ert-test-result-with-condition-backtrace result))
             (buffer (get-buffer-create "*ERT Backtrace*")))
         (pop-to-buffer buffer)
         (setq buffer-read-only t)
         (let ((inhibit-read-only t))
           (buffer-disable-undo)
           (erase-buffer)
           (ert-simple-view-mode)
           ;; Use unibyte because `debugger-setup-buffer' also does so.
           (set-buffer-multibyte nil)
           (setq truncate-lines t)
           (ert--print-backtrace backtrace)
           (debugger-make-xrefs)
           (goto-char (point-min))
           (insert "Backtrace for test `")
           (ert-insert-test-name-button (ert-test-name test))
           (insert "':\n")))))))

(defun ert-results-pop-to-messages-for-test-at-point ()
  "Display the part of the *Messages* buffer generated during the test at point.

To be used in the ERT results buffer."
  (interactive)
  (let* ((test (ert--results-test-at-point-no-redefinition))
         (stats ert--results-stats)
         (pos (ert--stats-test-pos stats test))
         (result (aref (ert--stats-test-results stats) pos)))
    (let ((buffer (get-buffer-create "*ERT Messages*")))
      (pop-to-buffer buffer)
      (setq buffer-read-only t)
      (let ((inhibit-read-only t))
        (buffer-disable-undo)
        (erase-buffer)
        (ert-simple-view-mode)
        (insert (ert-test-result-messages result))
        (goto-char (point-min))
        (insert "Messages for test `")
        (ert-insert-test-name-button (ert-test-name test))
        (insert "':\n")))))

(defun ert-results-pop-to-should-forms-for-test-at-point ()
  "Display the list of `should' forms executed during the test at point.

To be used in the ERT results buffer."
  (interactive)
  (let* ((test (ert--results-test-at-point-no-redefinition))
         (stats ert--results-stats)
         (pos (ert--stats-test-pos stats test))
         (result (aref (ert--stats-test-results stats) pos)))
    (let ((buffer (get-buffer-create "*ERT list of should forms*")))
      (pop-to-buffer buffer)
      (setq buffer-read-only t)
      (let ((inhibit-read-only t))
        (buffer-disable-undo)
        (erase-buffer)
        (ert-simple-view-mode)
        (if (null (ert-test-result-should-forms result))
            (insert "\n(No should forms during this test.)\n")
          (loop for form-description in (ert-test-result-should-forms result)
                for i from 1 do
                (insert "\n")
                (insert (format "%s: " i))
                (let ((begin (point)))
                  (ert--pp-with-indentation-and-newline form-description)
                  (ert--make-xrefs-region begin (point)))))
        (goto-char (point-min))
        (insert "`should' forms executed during test `")
        (ert-insert-test-name-button (ert-test-name test))
        (insert "':\n")
        (insert "\n")
        (insert (concat "(Values are shallow copies and may have "
                        "looked different during the test if they\n"
                        "have been modified destructively.)\n"))
        (forward-line 1)))))

(defun ert-results-toggle-printer-limits-for-test-at-point ()
  "Toggle how much of the condition to print for the test at point.

To be used in the ERT results buffer."
  (interactive)
  (let* ((ewoc ert--results-ewoc)
         (node (ert--results-test-node-at-point))
         (entry (ewoc-data node)))
    (setf (ert--ewoc-entry-extended-printer-limits-p entry)
          (not (ert--ewoc-entry-extended-printer-limits-p entry)))
    (ewoc-invalidate ewoc node)))

(defun ert-results-pop-to-timings ()
  "Display test timings for the last run.

To be used in the ERT results buffer."
  (interactive)
  (let* ((stats ert--results-stats)
         (start-times (ert--stats-test-start-times stats))
         (end-times (ert--stats-test-end-times stats))
         (buffer (get-buffer-create "*ERT timings*"))
         (data (loop for test across (ert--stats-tests stats)
                     for start-time across (ert--stats-test-start-times stats)
                     for end-time across (ert--stats-test-end-times stats)
                     collect (list test
                                   (float-time (subtract-time end-time
                                                              start-time))))))
    (setq data (sort data (lambda (a b)
                            (> (second a) (second b)))))
    (pop-to-buffer buffer)
    (setq buffer-read-only t)
    (let ((inhibit-read-only t))
      (buffer-disable-undo)
      (erase-buffer)
      (ert-simple-view-mode)
      (if (null data)
          (insert "(No data)\n")
        (insert (format "%-3s  %8s %8s\n" "" "time" "cumul"))
        (loop for (test time) in data
              for cumul-time = time then (+ cumul-time time)
              for i from 1 do
              (let ((begin (point)))
                (insert (format "%3s: %8.3f %8.3f " i time cumul-time))
                (ert-insert-test-name-button (ert-test-name test))
                (insert "\n"))))
      (goto-char (point-min))
      (insert "Tests by run time (seconds):\n\n")
      (forward-line 1))))

;;;###autoload
(defun ert-describe-test (test-or-test-name)
  "Display the documentation for TEST-OR-TEST-NAME (a symbol or ert-test)."
  (interactive (list (ert-read-test-name-at-point "Describe test")))
  (when (< emacs-major-version 24)
    (error "Requires Emacs 24"))
  (let (test-name
        test-definition)
    (etypecase test-or-test-name
      (symbol (setq test-name test-or-test-name
                    test-definition (ert-get-test test-or-test-name)))
      (ert-test (setq test-name (ert-test-name test-or-test-name)
                      test-definition test-or-test-name)))
    (help-setup-xref (list #'ert-describe-test test-or-test-name)
                     (called-interactively-p 'interactive))
    (save-excursion
      (with-help-window (help-buffer)
        (with-current-buffer (help-buffer)
          (insert (if test-name (format "%S" test-name) "<anonymous test>"))
          (insert " is a test")
          (let ((file-name (and test-name
                                (symbol-file test-name 'ert-deftest))))
            (when file-name
              (insert " defined in `" (file-name-nondirectory file-name) "'")
              (save-excursion
                (re-search-backward "`\\([^`']+\\)'" nil t)
                (help-xref-button 1 'help-function-def test-name file-name)))
            (insert ".")
            (fill-region-as-paragraph (point-min) (point))
            (insert "\n\n")
            (unless (and (ert-test-boundp test-name)
                         (eql (ert-get-test test-name) test-definition))
              (let ((begin (point)))
                (insert "Note: This test has been redefined or deleted, "
                        "this documentation refers to an old definition.")
                (fill-region-as-paragraph begin (point)))
              (insert "\n\n"))
            (insert (or (ert-test-documentation test-definition)
                        "It is not documented.")
                    "\n")))))))

(defun ert-results-describe-test-at-point ()
  "Display the documentation of the test at point.

To be used in the ERT results buffer."
  (interactive)
  (ert-describe-test (ert--results-test-at-point-no-redefinition)))

(provide 'ert-ui)

;;; ert-ui.el ends here
