;;; ert-ui.el --- ERT's interactive UI

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
(require 'ert)
(require 'ert-run)
(require 'ewoc)


;;; Some basic interactive functions.

(defun ert-read-test-name (prompt &optional default-value history)
  "Read the name of a test and return it as a symbol.
Prompt with PROMPT.  By default, return DEFAULT-VALUE."
  (when (symbolp default-value)
    (setq default-value (symbol-name default-value)))
  (intern (completing-read prompt obarray #'ert-test-boundp
                           t nil history default-value nil)))

(defun ert-find-test-other-window (test-name)
  "Find, in another window, the definition of TEST-NAME."
  (interactive (list (ert-read-test-name "Find test definition: ")))
  (find-function-do-it test-name 'ert-deftest 'switch-to-buffer-other-window))

(defun ert-delete-test (test-name)
  "An interactive interface to `ert-make-test-unbound'."
  (interactive (list (let ((default (thing-at-point 'symbol)))
                       (when default
                         (set-text-properties 0 (length default) nil default)
                         (when (or (string= default "nil")
                                   (intern-soft default))
                           (setq default (intern default)))
                         (unless (ert-test-boundp default)
                           (setq default nil)))
                       (completing-read (if (null default)
                                            "Delete test: "
                                          (format "Delete test (default %s): "
                                                  default))
                                        obarray #'ert-test-boundp
                                        'really-require-match
                                        nil nil default nil))))
  (ert-make-test-unbound test-name))

(defun ert-delete-all-tests ()
  "Make all symbols in `obarray' name no test."
  (interactive)
  (when (interactive-p)
    (unless (y-or-n-p "Delete all tests? ")
      (error "Aborted")))
  (mapc #'ert-delete-test (mapcar #'ert-test-name (ert-select-tests t t)))
  t)


;;; Display of test progress and results.

;; An entry in the results buffer ewoc.  There is one entry per test.
(defstruct ert-ewoc-entry
  (test (assert nil))
  (result nil)
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
(defvar ert-results-ewoc)
;; The stats object.
(defvar ert-results-stats)
;; A string with one character per test.  Each character represents
;; the result of the corresponding test.  The string is displayed near
;; the top of the buffer and serves as a progress bar.
(defvar ert-results-progress-bar-string)
;; The position where the progress bar button begins.
(defvar ert-results-progress-bar-button-begin)
;; The test result listener that updates the buffer when tests are run.
(defvar ert-results-listener)

;; The same as `ert-results-stats', but dynamically bound.  Used for
;; the mode line progress indicator.
(defvar ert-current-run-stats nil)

(defun ert-insert-test-name-button (test-name)
  (insert-text-button (format "%S" test-name)
                      :type 'ert-test-name-button
                      'ert-test-name test-name))

(defun ert-results-format-expected-unexpected (expected unexpected)
  (if (zerop unexpected)
      (format "%s" expected)
    (format "%s (%s unexpected)" (+ expected unexpected) unexpected)))

(defun ert-results-update-ewoc-hf (ewoc stats)
  "Update the header and footer of EWOC to show certain information from STATS.

Also sets `ert-results-progress-bar-button-begin'."
  (let ((run-count (ert-stats-completed stats))
        (results-buffer (current-buffer)))
    (ewoc-set-hf
     ewoc
     ;; header
     (with-temp-buffer
       (insert "Selector: ")
       (ert-insert-human-readable-selector (ert-stats-selector stats))
       (insert "\n")
       (insert
        (format (concat "Passed: %s\n"
                        "Failed: %s\n"
                        "Error:  %s\n"
                        "Total:  %s/%s\n\n")
                (ert-results-format-expected-unexpected
                 (ert-stats-passed-expected stats)
                 (ert-stats-passed-unexpected stats))
                (ert-results-format-expected-unexpected
                 (ert-stats-failed-expected stats)
                 (ert-stats-failed-unexpected stats))
                (ert-results-format-expected-unexpected
                 (ert-stats-error-expected stats)
                 (ert-stats-error-unexpected stats))
                run-count
                (ert-stats-total stats)))
       (insert
        (format "Started at:   %s\n"
                (ert-format-time-iso8601 (ert-stats-start-time stats))))
       ;; FIXME: This is ugly.  Need to properly define invariants of
       ;; the `stats' data structure.
       (let ((state (cond ((ert-stats-aborted-p stats)
                           'aborted)
                          ((ert-stats-current-test stats)
                           'running)
                          ((ert-stats-end-time stats)
                           'finished)
                          (t
                           'preparing))))
         (ecase state
           (preparing
            (insert ""))
           (aborted
            (cond ((ert-stats-current-test stats)
                   (insert "Aborted during test: ")
                   (ert-insert-test-name-button
                    (ert-test-name (ert-stats-current-test stats))))
                  (t
                   (insert "Aborted."))))
           (running
            (assert (ert-stats-current-test stats))
            (insert "Running test: ")
            (ert-insert-test-name-button (ert-test-name
                                          (ert-stats-current-test stats))))
           (finished
            (assert (not (ert-stats-current-test stats)))
            (insert "Finished.")))
         (insert "\n")
         (if (ert-stats-end-time stats)
             (insert
              (format "%s%s\n"
                      (if (ert-stats-aborted-p stats)
                          "Aborted at:   "
                        "Finished at:  ")
                      (ert-format-time-iso8601 (ert-stats-end-time stats))))
           (insert "\n"))
         (insert "\n"))
       (let ((progress-bar-string (with-current-buffer results-buffer
                                    ert-results-progress-bar-string)))
         (let ((progress-bar-button-begin
                (insert-text-button progress-bar-string
                                    :type 'ert-results-progress-bar-button)))
           ;; The header gets copied verbatim to the results buffer,
           ;; and all positions remain the same, so
           ;; `progress-bar-button-begin' will be the right position
           ;; even in the results buffer.
           (with-current-buffer results-buffer
             (set (make-local-variable 'ert-results-progress-bar-button-begin)
                  progress-bar-button-begin))))
       (insert "\n\n")
       (buffer-string))
     ;; footer
     ;;
     ;; We actually want an empty footer, but that would trigger a bug
     ;; in ewoc, sometimes clearing the entire buffer.
     "\n")))


(defvar ert-test-run-redisplay-interval-secs .1
  "How many seconds ERT should wait between redisplays while running tests.

While running tests, ERT shows the current progress, and this variable
determines how frequently the progress display is updated.")

(defun ert-results-update-stats-display (ewoc stats)
  "Update EWOC and the mode line to show data from STATS."
  ;; TODO(ohler): investigate using `make-progress-reporter'.
  (ert-results-update-ewoc-hf ewoc stats)
  (when (>= (float-time) (ert-stats-next-redisplay stats))
    (force-mode-line-update)
    (redisplay t)
    (setf (ert-stats-next-redisplay stats)
          (+ (float-time) ert-test-run-redisplay-interval-secs))))

(defun ert-tests-running-mode-line-indicator ()
  (let* ((stats ert-current-run-stats)
         (tests-total (ert-stats-total stats))
         (tests-completed (ert-stats-completed stats)))
    (if (>= tests-completed tests-total)
        (format " ERT(%s/%s,finished)" tests-completed tests-total)
      (format " ERT(%s/%s):%s"
              (1+ tests-completed)
              tests-total
              (if (null (ert-stats-current-test stats))
                  "?"
                (format "%S"
                        (ert-test-name (ert-stats-current-test stats))))))))

(defun ert-make-xrefs-region (begin end)
  (save-restriction
    (narrow-to-region begin (point))
    ;; Inhibit optimization in `debugger-make-xrefs' that would
    ;; sometimes insert unrelated backtrace info into our buffer.
    (let ((debugger-previous-backtrace nil))
      (debugger-make-xrefs))))

(defun ert-print-test-for-ewoc (entry)
  "The ewoc print function for ewoc test entries."
  (let* ((test (ert-ewoc-entry-test entry))
         (result (ert-ewoc-entry-result entry))
         (hiddenp (ert-ewoc-entry-hidden-p entry))
         (expandedp (ert-ewoc-entry-expanded-p entry))
         (extended-printer-limits-p (ert-ewoc-entry-extended-printer-limits-p
                                     entry)))
    (cond (hiddenp)
          (t
           (insert-text-button (format "%c"
                                       (ert-char-for-test-result
                                        result
                                        (ert-test-result-expected-p test
                                                                    result)))
                               :type 'ert-results-expand-collapse-button)
           (insert " ")
           (ert-insert-test-name-button (ert-test-name test))
           (insert "\n")
           (when (and expandedp (not (eql result 'nil)))
             (etypecase result
               (ert-test-passed
                (insert "    passed\n")
                (insert ""))
               (ert-test-result-with-condition
                (insert "    ")
                (let ((print-escape-newlines t)
                      (print-level (if extended-printer-limits-p 12 6))
                      (print-length (if extended-printer-limits-p 100 10)))
                  (let ((begin (point)))
                    (ert-pp-with-indentation-and-newline
                     (ert-test-result-with-condition-condition result))
                    (ert-make-xrefs-region begin (point)))))
               (ert-test-aborted-with-non-local-exit
                (insert "    aborted\n")))
             (insert "\n")))))
  nil)

(defun ert-setup-results-buffer (stats listener buffer-name)
  "Set up a test results buffer."
  (unless buffer-name (setq buffer-name "*ert*"))
  (let ((buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (setq buffer-read-only t)
      (let ((inhibit-read-only t))
        (buffer-disable-undo)
        (erase-buffer)
        (ert-results-mode)
        (let ((ewoc (ewoc-create 'ert-print-test-for-ewoc nil nil t)))
          (set (make-local-variable 'ert-results-ewoc) ewoc)
          (set (make-local-variable 'ert-results-stats) stats)
          (set (make-local-variable 'ert-results-progress-bar-string)
               (make-string (ert-stats-total stats)
                            (ert-char-for-test-result nil t)))
          (set (make-local-variable 'ert-results-listener) listener)
          (loop for test across (ert-stats-tests stats) do
                (ewoc-enter-last ewoc
                                 (make-ert-ewoc-entry :test test :hidden-p t)))
          (ert-results-update-ewoc-hf ert-results-ewoc ert-results-stats)
          (goto-char (1- (point-max)))
          buffer)))))


(defvar ert-selector-history nil
  "List of recent test selectors read from terminal.")

;; Should OUTPUT-BUFFER-NAME and MESSAGE-FN really be arguments here?
;; They are needed only for our automated self-tests at the moment.
;; Or should there be some other mechanism?
;;;###autoload
(defun ert-run-tests-interactively (selector
                                    &optional output-buffer-name message-fn)
  "Run the tests specified by SELECTOR and display the results in a buffer.

SELECTOR works as described in `ert-select-tests'."
  (interactive
   (list (let ((default (if ert-selector-history
                            (first ert-selector-history)
                          "t")))
           (read-from-minibuffer (if (null default)
                                     "Run tests: "
                                   (format "Run tests (default %s): " default))
                                 nil nil t 'ert-selector-history
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
                 (setq buffer (ert-setup-results-buffer stats
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
                 (ert-results-update-stats-display (with-current-buffer buffer
                                                     ert-results-ewoc)
                                                   stats)))
              (test-started
               (destructuring-bind (stats test) event-args
                 (with-current-buffer buffer
                   (let* ((ewoc ert-results-ewoc)
                          (pos (ert-stats-test-index stats test))
                          (node (ewoc-nth ewoc pos)))
                     (assert node)
                     (setf (ert-ewoc-entry-test (ewoc-data node)) test)
                     (setf (ert-ewoc-entry-result (ewoc-data node)) nil)
                     (aset ert-results-progress-bar-string pos
                           (ert-char-for-test-result nil t))
                     (ert-results-update-stats-display ewoc stats)
                     (ewoc-invalidate ewoc node)))))
              (test-ended
               (destructuring-bind (stats test result) event-args
                 (with-current-buffer buffer
                   (let* ((ewoc ert-results-ewoc)
                          (pos (ert-stats-test-index stats test))
                          (node (ewoc-nth ewoc pos)))
                     (setf (ert-ewoc-entry-result (ewoc-data node)) result)
                     (when (ert-ewoc-entry-hidden-p (ewoc-data node))
                       (setf (ert-ewoc-entry-hidden-p (ewoc-data node))
                             (ert-test-result-expected-p test result)))
                     (aset ert-results-progress-bar-string pos
                           (ert-char-for-test-result result
                                                     (ert-test-result-expected-p
                                                      test result)))
                     (ert-results-update-stats-display ewoc stats)
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
      '(("j" ert-results-jump-between-summary-and-result)
        ("." ert-results-find-test-at-point-other-window)
        ("r" ert-results-rerun-test-at-point)
        ("d" ert-results-rerun-test-at-point-debugging-errors)
        ("b" ert-results-pop-to-backtrace-for-test-at-point)
        ("m" ert-results-pop-to-messages-for-test-at-point)
        ;; TODO(ohler): Make n and p navigate up and down.
        ("p" ert-results-toggle-printer-limits-for-test-at-point)
        ("D" ert-delete-test)
        ("l" ert-results-pop-to-should-forms-for-test-at-point)
        ("T" ert-results-pop-to-timings)
        ("q" quit-window)
        ([tab] forward-button)
        ([backtab] backward-button)
        )
      do
      (define-key ert-results-mode-map key binding))

(define-button-type 'ert-results-progress-bar-button
  'action #'ert-results-progress-bar-button-action
  'help-echo "mouse-2, RET: Reveal test result")

(define-button-type 'ert-test-name-button
  'action #'ert-test-name-button-action
  'help-echo "mouse-2, RET: Find test definition")

(define-button-type 'ert-results-expand-collapse-button
  'action #'ert-results-expand-collapse-button-action
  'help-echo "mouse-2, RET: Expand/collapse test result")

(defun ert-results-test-node-or-null-at-point ()
  "If point is on a valid ewoc node, return it; return nil otherwise.

To be used in the ERT results buffer."
  (let* ((ewoc ert-results-ewoc)
         (node (ewoc-locate ewoc)))
    ;; `ewoc-locate' will return an arbitrary node when point is on
    ;; header or footer, or when all nodes are invisible.  So we need
    ;; to validate its return value here.
    ;;
    ;; Update: I'm seeing nil being returned in some cases now,
    ;; perhaps this has been changed?
    (if (and node
             (>= (point) (ewoc-location node))
             (not (ert-ewoc-entry-hidden-p (ewoc-data node))))
        node
      nil)))

(defun ert-results-test-node-at-point ()
  "If point is on a valid ewoc node, return it; signal an error otherwise.

To be used in the ERT results buffer."
  (or (ert-results-test-node-or-null-at-point)
      (error "No test at point")))

(defun ert-results-expand-collapse-button-action (button)
  "Expand or collapse the test node BUTTON belongs to."
  (let* ((ewoc ert-results-ewoc)
         (node (save-excursion
                 (goto-char (ert-button-action-position))
                 (ert-results-test-node-at-point)))
         (entry (ewoc-data node)))
    (setf (ert-ewoc-entry-expanded-p entry)
          (not (ert-ewoc-entry-expanded-p entry)))
    (ewoc-invalidate ewoc node)))

(defun ert-results-find-test-at-point-other-window ()
  "Find the definition of the test at point in another window.

To be used in the ERT results buffer."
  (interactive)
  (let* ((node (ert-results-test-node-at-point))
         (entry (ewoc-data node))
         (test (ert-ewoc-entry-test entry))
         (name (ert-test-name test)))
    (ert-find-test-other-window name)))

(defun ert-test-name-button-action (button)
  "Find the definition of the test BUTTON belongs to, in another window."
  (let ((name (button-get button 'ert-test-name)))
    (ert-find-test-other-window name)))

(defun ert-ewoc-position (ewoc node)
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
  (let ((ewoc ert-results-ewoc)
        (progress-bar-begin ert-results-progress-bar-button-begin))
    (cond ((ert-results-test-node-or-null-at-point)
           (let* ((node (ert-results-test-node-at-point))
                  (pos (ert-ewoc-position ewoc node)))
             (goto-char (+ progress-bar-begin pos))))
          ((and (<= progress-bar-begin (point))
                (< (point) (button-end (button-at progress-bar-begin))))
           (let* ((node (ewoc-nth ewoc (- (point) progress-bar-begin)))
                  (entry (ewoc-data node)))
             (when (ert-ewoc-entry-hidden-p entry)
               (setf (ert-ewoc-entry-hidden-p entry) nil)
               (ewoc-invalidate ewoc node))
             (ewoc-goto-node ewoc node)))
          (t
           (goto-char progress-bar-begin)))))

(defun ert-button-action-position ()
  "The buffer position where the last button action was triggered."
  (cond ((integerp last-command-event)
         (point))
        ((eventp last-command-event)
         (posn-point (event-start last-command-event)))
        (t (assert nil))))

(defun ert-results-progress-bar-button-action (button)
  "Find the ewoc node that represents the same test as the character clicked."
  (goto-char (ert-button-action-position))
  (ert-results-jump-between-summary-and-result))

(defun ert-results-rerun-test-at-point ()
  "Re-run the test at point.

To be used in the ERT results buffer."
  (interactive)
  (let* ((ewoc ert-results-ewoc)
         (node (ert-results-test-node-at-point))
         (entry (ewoc-data node))
         (old-test (ert-ewoc-entry-test entry))
         (test-name (ert-test-name old-test))
         ;; FIXME: Write a test for this lookup.
         (test (if test-name
                   (if (ert-test-boundp test-name)
                       (ert-get-test test-name)
                     (error "No such test: %S" test-name))
                 old-test))
         (stats ert-results-stats)
         (pos (gethash test (ert-stats-test-map stats)))
         (progress-message (format "Running test %S" (ert-test-name test))))
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
                                       ert-results-listener))
            (ert-results-update-stats-display ewoc stats)
            (message "%s...%s"
                     progress-message
                     (let ((result (ert-test-most-recent-result test)))
                       (ert-string-for-test-result
                        result (ert-test-result-expected-p test result)))))
        (goto-char point)))))

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
  (let* ((node (ert-results-test-node-at-point))
         (entry (ewoc-data node))
         (test (ert-ewoc-entry-test entry))
         (result (ert-ewoc-entry-result entry)))
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
           (ert-print-backtrace backtrace)
           (debugger-make-xrefs)
           (goto-char (point-min))
           (insert "Backtrace for test `")
           (ert-insert-test-name-button (ert-test-name test))
           (insert "':\n")))))))

(defun ert-results-pop-to-messages-for-test-at-point ()
  "Display the part of the *Messages* buffer generated during the test at point.

To be used in the ERT results buffer."
  (interactive)
  (let* ((node (ert-results-test-node-at-point))
         (entry (ewoc-data node))
         (test (ert-ewoc-entry-test entry))
         (result (ert-ewoc-entry-result entry)))
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
  (let* ((node (ert-results-test-node-at-point))
         (entry (ewoc-data node))
         (test (ert-ewoc-entry-test entry))
         (result (ert-ewoc-entry-result entry)))
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
                  (ert-pp-with-indentation-and-newline form-description)
                  (ert-make-xrefs-region begin (point)))))
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
  (let* ((ewoc ert-results-ewoc)
         (node (ert-results-test-node-at-point))
         (entry (ewoc-data node)))
    (setf (ert-ewoc-entry-extended-printer-limits-p entry)
          (not (ert-ewoc-entry-extended-printer-limits-p entry)))
    (ewoc-invalidate ewoc node)))

(defun ert-results-pop-to-timings ()
  "Display test timings for the last run.

To be used in the ERT results buffer."
  (interactive)
  (let* ((stats ert-results-stats)
         (start-times (ert-stats-test-start-times stats))
         (end-times (ert-stats-test-end-times stats))
         (buffer (get-buffer-create "*ERT timings*"))
         (data (loop for test across (ert-stats-tests stats)
                     for start-time across (ert-stats-test-start-times stats)
                     for end-time across (ert-stats-test-end-times stats)
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

(provide 'ert-ui)

;;; ert-ui.el ends here
