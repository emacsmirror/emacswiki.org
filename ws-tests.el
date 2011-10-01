;;; ws-tests.el --- automated tests for win-switch

;; Copyright (C) 2011, Christopher R. Genovese, all rights reserved.

;; Author: Christopher Genovese <genovese@cmu.edu>
;; Maintainer: Christopher R. Genovese <genovese@cmu.edu>
;; URL: http://www.stat.cmu.edu/~genovese/emacs/

;; Version: 1.0
;; Update#: 2
;; Created:      Wed 04 Aug 2011 at 20:25 EDT
;; Last-Updated: Fri 12 Aug 2011 at 17:39 EDT
;; By: Christopher R. Genovese

;; Keywords: window, switch, key bindings, ergonomic, efficient
;; Package-Requires: ((win-switch . 1.0))
;; Compatibility: Tested with GNU Emacs 22, 23, 24 on Mac OS X 10.5
;;                Testing and feedback on other platforms appreciated.


;;; Commentary:
;;
;;  Simple tests for win-switch under a variety of configurations
;;  and window arrangements. Produces a results summary in a separate
;;  frame or window.
;;
;;  To use, either load or require this file (depending on where it is installed),
;;  e.g.,
;;
;;      (load-file "ws-tests.el") ; use appropriate path
;;
;;  and then execute M-x ws-test-run-all
;;
;;  New tests and test suites can be easily defined using
;;  `ws-test' and `def-ws-test-suite'. Fixtures (i.e.,
;;  win-switch parameter settings) can be inherited and combined
;;  relatively simply as well. The examples at the bottom of this file
;;  are a good place to start and illustrate the api and main options.
;;
;;  Admittedly, all these features might be a bit of overkill.
;;  (Do we need tests for the tests now?) But as the examples show,
;;  it does provide what it is meant to: an extensible framework
;;  for testing win-switch mode under various configurations.
;;
;;  Known Issues:
;;
;;  On Carbon Emacs 22, the Vertical Resizing 1 test can fail at random
;;  when the main frame is small at start up. Enlarging the frame by
;;  a couple lines fixes the problem. I'm looking into this.
;; 
;;  Code Contents (by emacs pages)
;;    1. (@> "Utility Functions")
;;    2. (@> "Window and buffer setup and cleanup")
;;    3. (@> "Support routines and variables for test suite definition")
;;    4. (@> "Main interface for test writing and execution")
;;    5. (@> "Actual Test Suite Definitions")
;;  


;;; Code:

(require 'win-switch)


;; (@* "Utility functions")

(defmacro ws-test-pstring (string &rest face-props)
  "STRING with its face properties set to FACE-PROPS.
If FACE-PROPS is empty, return the unpropertized string."
  (declare (indent 1))
  (if (null face-props)
      `,string
  `(propertize ,string 'face '(,@face-props))))


;; (@* "Window and buffer setup and cleanup")

(defvar ws-test-count 0)
(defvar ws-test-lines 16)
(defvar ws-test-num-windows 0)
(defvar ws-test-buffer-root "**ws-test-%d**")
(defvar ws-test-buffer-tmp  "save-")
(defvar ws-test-results-buf "*ws-test-results*")

(defun ws-test-buffer-setup (num-windows)
  "Prepare buffers for testing and switch to first window.
NUM-WINDOWS is the number of windows (and thus buffers) to be
used in the test. This function overwrites the contents of
register z."
  (window-configuration-to-register ?z)
  (setq ws-test-count 0)
  (setq ws-test-num-windows num-windows)
  (let ((i 0)
        (buf nil)
        (buf-name nil))
    (while (< i num-windows)
      (setq buf-name (format ws-test-buffer-root i))
      (setq buf      (get-buffer buf-name))
      (when buf                    ; if buffer exists (unlikely) save it
        (with-current-buffer buf   ; but one level of checking only
          (rename-buffer (concat ws-test-buffer-tmp buf-name))))
      (setq buf (get-buffer-create buf-name))
      (with-current-buffer buf
        (let ((j 0))
          (while (< j ws-test-lines)
            (insert (format "Line %d\n" ws-test-count))
            (setq ws-test-count (1+ ws-test-count))
            (setq j (1+ j))))
        (goto-char (point-min)))
      (setq i (1+ i))))
  (setq buf-name (format ws-test-buffer-root 0))
  (switch-to-buffer buf-name)
  (delete-other-windows))

(defun ws-test-window-setup (split-tree)
  "Split windows as specified by SPLIT-TREE.
SPLIT-TREE is a list, each element of which is itself a list
describing one level of splits. The situation begins with one
window, and the first element splits that or does nothing. If the
first window is split, the second element is a list of two split
specifications. Each successive element is a list of as many
split specifications as there are windows. A split specification
is either a nil, meaning no split, 'horiz (or 'h or 'horizontal)
for a horizontal split in half, 'vert (or 'v or 'vertical) for a
vertical split in half, or (size horiz) where size is an integer
and horiz is a boolean indicating a split into size lines and
horizontal or not. ATTN: clarify this."
  (let ((curlist split-tree)
        (windows (make-vector ws-test-num-windows nil))
        (index   0))
    (aset windows index (selected-window))
    (setq index (1+ index))
    (while curlist
      (let* ((splits (first curlist)) ; (size . horizontal?)
             (i 0)
             (split nil)
             (w nil)
             (numwins index))
        (while (and splits (< i numwins))
          (setq split (first splits))
          (select-window (aref windows i))
          (when split
            (let ((size (if (listp split) (first split) nil))
                  (horz (if (listp split) (second split) (if (memq split '(h horiz horizontal)) t nil))))
              (setq w (split-window (selected-window) size horz)))
            (select-window w)
            (switch-to-buffer (format ws-test-buffer-root index))
            (aset windows index w)
            (setq index (1+ index)))
          (setq splits (rest splits))
          (setq i (1+ i)))
        (setq curlist (rest curlist))))
    (select-window (aref windows 0))))
  
(defun ws-test-results-setup ()
  "Setup a buffer to summarize results."
  (let ((buf (get-buffer-create ws-test-results-buf))
        (mesg (concat
               (ws-test-pstring "WinSwitch Test Results"
                 :foreground "midnight blue" :weight bold :height 1.44)
               "\n    To dismiss, do M-x ws-test-dismiss or push button below.\n")))
    (with-current-buffer buf
      (when (= (point) (point-min))
        (fundamental-mode)
        (insert mesg))
      (insert "\n"))
    (bury-buffer buf)))

(defun ws-test-results-update (name result)
  "Add outcome of test NAME with RESULT."
  (with-current-buffer ws-test-results-buf
    (insert (format "%-24s... %s\n" name result))))
                                                      ; Adjust indentation
(put 'ws-test-results-update 'lisp-indent-function 1) ; RESULT usually body-like forms

(defun ws-test-cleanup ()
  "Cleanup test buffers."
  (let ((i 0)
        (buf nil)
        (buf-name nil))
    (while (< i ws-test-num-windows)
      (setq buf-name (format ws-test-buffer-root i))
      (kill-buffer buf-name)
      (setq buf (get-buffer (concat ws-test-buffer-tmp buf-name)))
      (when buf                    ; if buffer exists restore
        (with-current-buffer buf
          (rename-buffer buf-name)))
      (setq i (1+ i))))
  (setq ws-test-num-windows 0)
  (jump-to-register ?z))

(defvar ws-test-results-frame nil
  "If non-nil, the (pop-up) frame displaying the results buffer.")

(defun ws-test-done ()
  "Display test results, in separate frame if possible."
  (when window-system
    (unless (and ws-test-results-frame (framep ws-test-results-frame))
      (setq ws-test-results-frame
            (make-frame '((title . "WinSwitch Test Results")
                          (top . 1) (left . -1)
                          (width . 96) (height . 32)
                          (vertical-scroll-bars . right)
                          (buffer-list . (list (get-buffer-create ws-test-results-buf)))
                          (unsplittable . t)))))
    (select-frame-set-input-focus ws-test-results-frame))
  (pop-to-buffer ws-test-results-buf)
  (delete-other-windows))

(defun ws-test-display-tallies (num-suites num-tests num-passed num-unexpected)
  "Display tallies of successful and failed tests in results buffer."
  (when ws-test-results-buf
    (with-current-buffer ws-test-results-buf
      (insert "\n\n"
              (format "Ran %d test suites with %d total tests, "
                      num-suites num-tests)
              "yielding "
              (let ((passes (format "%d passes" num-passed)))
                (if (> num-passed 0)
                    (ws-test-pstring passes :underline "green")
                  passes))
              " and "
              (let ((failures (format "%d failures" (- num-tests num-passed))))
                (if (< num-passed num-tests)
                    (ws-test-pstring failures :underline "red")
                  failures))
              ".\n"
              (if (> num-unexpected 0)
                (concat "There were "
                        (ws-test-pstring
                            (format "%d error conditions" num-unexpected)
                          :foreground "red" :weight bold)
                        " detected.\n")
                "")
              (if (or (< num-passed num-tests) (> num-unexpected 0))
                  (concat
                   "\nPlease send a copy of this buffer, "
                   "details of your platform and emacs version,\n"
                   "and any other relevant context or information "
                   "to genovese@cmu.edu.\n")
                "")
              "\n"))))

(defun ws-test-display-dismiss-button ()
  "Display button that dismisses the results buffer/frame."
  (when ws-test-results-buf
    (with-current-buffer ws-test-results-buf
      (insert-button
       (if (facep 'custom-button)
           (ws-test-pstring "Dismiss" 'custom-button)
           "[Dismiss]")
       'action (lambda (button) (ws-test-dismiss)))
      (insert "\n"))))

(defun ws-test-window-sizes (&optional select-size)
  "Make vector of window sizes for all test buffers.
By default, sizes are lists as returned by `window-edges'.
If provided, SELECT-SIZE should be a function that operates
on such a list. For example, using
     (lambda (x) (and x
                      (list (- (third x)  (first x))
                            (- (fourth x) (second x)))))
give a vector of width, height pairs."
  (let ((size-f (if (functionp select-size) select-size #'identity))
        (wsizes (make-vector ws-test-num-windows nil))
        (w 0))
    (while (< w ws-test-num-windows)
      (let* ((buf (get-buffer (format ws-test-buffer-root w)))
             (win (get-buffer-window buf)))
        (aset wsizes w
              (if win
                  (funcall size-f (window-edges win))
                nil)))
      (setq w (1+ w)))
    wsizes))


;; (@* "Support routines and variables for test suite definition")
;;
;; Suites are associated with a symbol name that is expanded into
;; a fully realized global symbol ws-test-suite-<name>
;; for use as a function. Suite's have associated settings
;; lists mapped to the global-symbol's 'win-switch-settings
;; property. Various methods for manipulating the settings
;; list are given below.
;;

(defvar ws-test-defined-suites nil
  "List of defined test suites, in reverse order.")

(defvar ws-test-total-tests 0
  "Total number of tests in last run.")

(defvar ws-test-successful-tests 0
  "Number of tests passed in last run.")

(defvar ws-test-unexpected-error-tests 0
  "Number of tests produing an unexpected error in last run.")

(defvar ws-test-saved-settings nil
  "Stack of most recently saved win-switch settings.")

(defun ws-test-sym->suite (name)
  "Convert symbol NAME to a test suite symbol."
  (intern (concat "ws-test-suite-" (symbol-name name))))

(defun ws-test-safe-value (value &optional function?)
  "Produce valid representation for setting VALUE in lisp code.
The resulting form can be used as the right-hand-side binding in
a set or setq, for instance. If FUNCTION? is non-nil, interpret
any symbol as a function."
  (cond
   ((null value)
    nil)
   ((eq value t)
    t)
   ((symbolp value)
    (if function? `#',value `',value))
   ((atom value)
    value)
   ((eq (first value) 'lambda)
    `,value)
   (t
    `',value)))

(defun ws-test-default-settings ()
  "Produce form specifying default win-switch settings."
  '(idle-time                  0.75
    window-threshold           2
    other-window-first         t
    provide-visual-feedback    t
    feedback-background-color  "red"
    feedback-foreground-color  "white"
    on-feedback-function       nil
    off-feedback-function      nil
    other-window-function      nil
    (win-switch-set-wrap-around 1)
    (win-switch-set-keys '("i")          'up)
    (win-switch-set-keys '("k")          'down)
    (win-switch-set-keys '("j")          'left)
    (win-switch-set-keys '("l")          'right)
    (win-switch-set-keys '("o")          'next-window)
    (win-switch-set-keys '("p")          'previous-window)
    (win-switch-set-keys '("I")          'enlarge-vertically)
    (win-switch-set-keys '("K")          'shrink-vertically)
    (win-switch-set-keys '("J")          'shrink-horizontally)
    (win-switch-set-keys '("L")          'enlarge-horizontally)
    (win-switch-set-keys '(" ")          'other-frame)
    (win-switch-set-keys '("u" [return]) 'exit)
    (win-switch-set-keys '(";")          'split-horizontally)
    (win-switch-set-keys '("h")          'split-vertically)
    (win-switch-set-keys '("0")          'delete-window)
    (win-switch-set-keys '("\M-\C-g")    'emergency-exit)
    (win-switch-set-once-keys '("u")     'once-double-next)
    (win-switch-set-once-keys '("y")     'once-double-prev)
    (global-set-key "\C-xo" 'win-switch-dispatch)))

(defun ws-test-current-settings ()
  "Produce form specifying current win-switch settings."
  `(idle-time                      ,(ws-test-safe-value win-switch-idle-time)
    window-threshold               ,(ws-test-safe-value win-switch-window-threshold)
    other-window-first             ,(ws-test-safe-value win-switch-other-window-first)
    provide-visual-feedback        ,(ws-test-safe-value win-switch-provide-visual-feedback)
    feedback-background-color      ,(ws-test-safe-value win-switch-feedback-background-color)
    feedback-foreground-color      ,(ws-test-safe-value win-switch-feedback-foreground-color)
    on-feedback-function           ,(ws-test-safe-value win-switch-on-feedback-function)
    off-feedback-function          ,(ws-test-safe-value win-switch-off-feedback-function)
    other-window-function          ,(ws-test-safe-value win-switch-other-window-function)
    (win-switch-set-wrap-around    ,(if win-switch-wrap-around 1 -1))
    (win-switch-set-keys ,(ws-test-safe-value win-switch-up-keys)                    'up)
    (win-switch-set-keys ,(ws-test-safe-value win-switch-down-keys)                  'down)
    (win-switch-set-keys ,(ws-test-safe-value win-switch-left-keys)                  'left)
    (win-switch-set-keys ,(ws-test-safe-value win-switch-right-keys)                 'right)
    (win-switch-set-keys ,(ws-test-safe-value win-switch-next-window-keys)           'next-window)
    (win-switch-set-keys ,(ws-test-safe-value win-switch-previous-window-keys)       'previous-window)
    (win-switch-set-keys ,(ws-test-safe-value win-switch-enlarge-vertically-keys)    'enlarge-vertically)
    (win-switch-set-keys ,(ws-test-safe-value win-switch-shrink-vertically-keys)     'shrink-vertically)
    (win-switch-set-keys ,(ws-test-safe-value win-switch-shrink-horizontally-keys)   'shrink-horizontally)
    (win-switch-set-keys ,(ws-test-safe-value win-switch-enlarge-horizontally-keys)  'enlarge-horizontally)
    (win-switch-set-keys ,(ws-test-safe-value win-switch-other-frame-keys)           'other-frame)
    (win-switch-set-keys ,(ws-test-safe-value win-switch-exit-keys)                  'exit)
    (win-switch-set-keys ,(ws-test-safe-value win-switch-split-horizontally-keys)    'split-horizontally)
    (win-switch-set-keys ,(ws-test-safe-value win-switch-split-vertically-keys)      'split-vertically)
    (win-switch-set-keys ,(ws-test-safe-value win-switch-delete-window-keys)         'delete-window)
    (win-switch-set-keys ,(ws-test-safe-value win-switch-emergency-exit-keys)        'emergency-exit)
    (win-switch-set-once-keys ,(ws-test-safe-value win-switch-once-double-next-keys) 'once-double-next)
    (win-switch-set-once-keys ,(ws-test-safe-value win-switch-once-double-prev-keys) 'once-double-prev)
    (global-set-key "\C-xo" ',(lookup-key (current-global-map) "\C-xo"))))

(defun ws-test-use-settings (settings)
  "Set win-switch parameters prior to the test.
SETTINGS is a list whose elements are either symbol and value
pairs, space separated with no parentheses, or a sexp.
If the former, the pair S V, is converted to (setq win-switch-S V).
If the latter, the sexp is evaluated as is."
  (let ((items settings))
    (while items
      (let ((item (first items))
            (item2 (second items)))
        (cond
         ((symbolp item)
          (set (intern (concat "win-switch-" (symbol-name item)))
               item2)
          (setq items (rest (rest items))))
         ((consp item)
          (eval item)
          (setq items (rest items)))
         (t
          (error "Improperly formed ws-test-use-settings body (%s %s...)" item item2)))))
    nil))

(defun ws-test-save-settings ()
  "Save current win-switch settings."
  (let ((settings (ws-test-current-settings)))
    (setq ws-test-saved-settings
          (cons settings ws-test-saved-settings))))

(defun ws-test-restore-settings ()
  "Restore previously saved win-switch settings."
  (ws-test-use-settings
   (prog1
       (first ws-test-saved-settings)
     (setq ws-test-saved-settings (rest ws-test-saved-settings)))))
  
(defmacro with-win-switch-settings (settings &rest forms)
  "With win-switch SETTINGS as temporary context, execute body FORMS.
Previous settings are restored upon completion of body forms."
  (declare (indent 1))
  `(progn
     (ws-test-save-settings)
     (ws-test-use-settings ,settings)
     ,@forms
     (ws-test-restore-settings)))

(defun ws-test-suite-get-settings (name)
  "Retrieve settings list associated with suite NAME."
  (get (ws-test-sym->suite name) 'win-switch-settings))

(defsubst ws-test-suite-put-settings-x (suite-sym settings)
  "Set win-switch settings for symbol SUITE-SYM to be the list SETTINGS."
  (put suite-sym 'win-switch-settings settings))

(defun ws-test-suite-put-settings (name settings)
  "Set win-switch settings for suite NAME to be the list SETTINGS."
  (ws-test-suite-put-settings-x (ws-test-sym->suite name) settings))

(defsubst ws-test-suite-add-settings-x (suite-sym settings)
  "Append to win-switch-settings for symbol SUITE-SYM, adding SETTINGS.
Note that SUITE-SYM is the full symbol representing the suite, i.e.
ws-test-suite-<name> rather than just <name>."
  (put suite-sym 'win-switch-settings
       (append (get suite-sym 'win-switch-settings)
               settings
               nil)))

(defun ws-test-suite-add-settings (name settings)
  "Add to settings list associated with suite NAME, appending list SETTINGS."
  (let ((suite-sym (ws-test-sym->suite name)))
    (ws-test-suite-add-settings-x suite-sym settings)))

;; Clearer indentation highlighting name and allowing complex settings list
(put 'ws-test-suite-add-settings-x 'lisp-indent-function 1) 
(put 'ws-test-suite-put-settings-x 'lisp-indent-function 1)
(put 'ws-test-suite-add-settings   'lisp-indent-function 1)
(put 'ws-test-suite-put-settings   'lisp-indent-function 1)


;; (@* "Main interface for test writing and execution")

(defun ws-test-interactive-cmds (cmd-string)
  "Simulate a string of interactive commands.
CMD-STRING is a keyboard macro string."
  (execute-kbd-macro (read-kbd-macro cmd-string)))

(defsubst ws-test-result-mesg (passed?)
  "Produce message associated with test result.
If PASSED? is non-nil, produce success message;
otherwise, produce failure message. PASSED? is
evaluated only once, so complex forms can be
used without penalty."
  (if passed?
      (ws-test-pstring "PASSED" :foreground "green" :weight bold)
    (ws-test-pstring "FAILED" :foreground "red" :weight bold)))

(defmacro ws-test-should (&rest forms)
  "Execute FORMS and return string describing outcome."
  `(not (null (progn ,@forms))))

(defmacro ws-test-should-error (&rest forms)
  "Execute FORMS and return string describing outcome."
  `(condition-case nil
        (progn ,@forms nil)
      (error t)))

(defmacro ws-test (name num-windows split-spec save &rest body)
  "Run a specified test, reporting results and keeping tally.
NAME is the name of the test. Begins with NUM-WINDOWS windows
split according to SPLIT-SPEC (see `ws-test-windows-setup').
If SAVE is non-nil, do not eliminate test buffers after completion.
Then do forms in BODY, which should give a symbolic value of either
passed or failed. This adjusts the test tallies of passes and failures;
callers of test suites are responsible for initializing the tallies
as desired."
  (declare (indent 4))
  `(progn
     (ws-test-buffer-setup ,num-windows)
     (ws-test-window-setup ,split-spec)
     (let ((result
            (condition-case test-err
                (progn ,@body)
              (error (list 'error test-err)))))
       (setq ws-test-total-tests (1+ ws-test-total-tests))
       (cond
        ((null result)
         (ws-test-results-update ,name (ws-test-result-mesg nil)))
        ((and (consp result) (eq (first result) 'error))
         (setq ws-test-unexpected-error-tests (1+ ws-test-unexpected-error-tests))
         (ws-test-results-update ,name
           (concat (ws-test-result-mesg nil)
                   (format "  %s" (second result)))))
        (t
         (setq ws-test-successful-tests (1+ ws-test-successful-tests))
         (ws-test-results-update ,name (ws-test-result-mesg result)))))
     (unless ,save
       (ws-test-cleanup))))

(defmacro def-ws-test-suite (name mesg &rest body)
  "Define a test suite.
NAME is the name of the suite, which should be a
symbol (excluding the prefix ws-test-suite-). The
function ws-test-suite-NAME is defined to run the suite,
but instead of calling it directly, use
`ws-test-run-suite' with NAME. MESG is a string to put in
the results buffer before the results. (No trailing newline is
needed.) BODY is a set of forms, typically including test
definitions. The first elements of BODY can contain parameter
settings for configuring win-switch. These can be specified
either by :inherits SUITE or by :settings SETTINGS-LIST, where
SUITE is a suite name (as with the one passed to this function)
and SETTINGS-LIST is an explicit settings list as allowed by
`ws-test-suite-settings'. The former sets all the settings
to those of SUITE, while the latter changes only the given
settings. Thus, using :inherits followed by :settings gives
an easy way to modify the settings of an other suite. With
neither :inherits or :settings, the settings are set to
those given by `ws-test-default-settings'."
  (if (memq name ws-test-defined-suites)
      (message "Redefining win-switch test suite %s" name)
    (setq ws-test-defined-suites (cons name ws-test-defined-suites)))
  (let ((suite-symbol (ws-test-sym->suite name))
        (keyword nil))
    ;; Start with the default settings to ensure that all 
    ;; suites are parameterized set consistently.
    (ws-test-suite-put-settings-x suite-symbol
      (ws-test-default-settings))
    ;; Read optional settings information at beginning of BODY
    ;; Look for :inherits NAME-SYM  or  :settings SETTINGS-LIST
    ;; Mo sense using more than one :inherits, but it does no harm
    (while (setq keyword
                 (let ((fst (first body)))
                   (and (keywordp fst)
                        (or (eq fst :inherits) (eq fst :settings)))))
      (if (eq keyword :inherits)
          (ws-test-suite-put-settings-x suite-symbol
            (ws-test-get-settings (second body)))
        (ws-test-suite-add-settings-x suite-symbol
          (second body)))                         
      (setq body (nthcdr 2 body)))
  `(defun ,suite-symbol (&optional suppress-results)
     ,(concat "Test suite for win-switch. "
              mesg
              "\nIf SUPPRESS-RESULTS is non-nil, do not show results window/frame after completion.")
     (ws-test-results-setup)
     ,(when mesg
        `(with-current-buffer ws-test-results-buf
           (insert ,(ws-test-pstring mesg :weight bold) "\n")))
     (with-win-switch-settings
         (ws-test-suite-get-settings ',name)
       (condition-case suite-err
           (progn ,@body)
         (error (when suite-err
                  (with-current-buffer ws-test-results-buf
                    (insert
                     "\n"
                     (ws-test-pstring
                         (format
                          "Unexpected error raised while executing test suite %s: %s."
                          ',name suite-err)
                       :foreground "red" :weight bold)
                     "\n"))))))
     (unless suppress-results
       (ws-test-done)))))

(defun ws-test-run-suite (suite &optional suppress)
  "Execute the tests in SUITE passing SUPPRESS.
SUITE should be a symbol as given to `def-ws-test-suite'.
An error is raised if test suite SUITE has not been defined."
  (interactive "S")
  (let ((the-suite
         (intern (concat "ws-test-suite-" (symbol-name suite)))))
    (unless (fboundp the-suite)
      (error "Win-switch test suite %s has not been defined" suite))
    (message "Running suite %s..." suite)
    (funcall the-suite suppress)
    (message "Done")))

(defun ws-test-dismiss ()
  "Eliminate results display and return to initial configuration."
  (interactive)
  (when (and window-system ws-test-results-frame)
    (delete-frame ws-test-results-frame t)
    (setq ws-test-results-frame nil))
  (kill-buffer ws-test-results-buf)
  (jump-to-register ?z))
                   

;; Execution of defined test suites

(defun ws-test-run-all ()
  "Run all defined test suites for win-switch."
  (interactive)
  (setq ws-test-total-tests            0
        ws-test-successful-tests       0
        ws-test-unexpected-error-tests 0)
  (let ((suites (reverse ws-test-defined-suites)))
    (while suites
      (ws-test-run-suite (first suites) t)
      (setq suites (rest suites)))
    (ws-test-done)
    (ws-test-display-tallies (length ws-test-defined-suites)
                             ws-test-total-tests
                             ws-test-successful-tests
                             ws-test-unexpected-error-tests)
    (ws-test-display-dismiss-button)))

(defun ws-test-run-matching (predicate)
  "Run defined test suites for win-switch matching PREDICATE.
PREDICATE should be a boolean function that takes a single
symbol argument."
  (interactive "xPredicate: ")
  (setq ws-test-total-tests            0
        ws-test-successful-tests       0
        ws-test-unexpected-error-tests 0)
  (let ((suites (reverse ws-test-defined-suites))
        (num-suites 0))
    (while suites
      (when (funcall predicate (first suites))
        (ws-test-run-suite (first suites) t)
        (setq num-suites (1+ num-suites)))
      (setq suites (rest suites)))
    (ws-test-done)
    (ws-test-display-tallies num-suites
                             ws-test-total-tests
                             ws-test-successful-tests
                             ws-test-unexpected-error-tests)
    (ws-test-display-dismiss-button)))

(defun ws-test-run-matching-name (regex)
  "Run defined test suites for win-switch whose name matches REGEX.
REGEX is a string containing a regular expression to match."
  (interactive "sPattern: ")
  (ws-test-run-matching (lambda (sym) (string-match regex (symbol-name sym)))))


;; (@* "Actual Test Suite Definitions")

(def-ws-test-suite default4
  "Four equal windows, default configuration."
  (let ((num-win 4)
        (win-spl '((vert) (horiz horiz)))
        (save    nil))
    (ws-test "Directional Keys 1" num-win win-spl save
      (ws-test-interactive-cmds "C-x o p i l k j u")
      (next-line)
      (ws-test-should (looking-at "^Line 1")))
    (ws-test "Directional Keys 2" num-win win-spl save
      (ws-test-interactive-cmds "C-x o k j i u")
      (next-line)
      (ws-test-should (looking-at "^Line 1")))
    (ws-test "Directional Keys 3" num-win win-spl save
      (ws-test-interactive-cmds "C-x o k j i l l u")
      (next-line)
      (ws-test-should (looking-at "^Line 1")))
    (ws-test "Other Window Key" num-win win-spl save
      (ws-test-interactive-cmds "C-x o o o o u")
      (next-line)
      (ws-test-should (looking-at "^Line 1")))
    (ws-test "Previous Window Key" num-win win-spl save
      (ws-test-interactive-cmds "C-x o p p p p p u")
      (next-line)
      (ws-test-should (looking-at "^Line 1")))
    (ws-test "Mixture 1" num-win win-spl save
      (ws-test-interactive-cmds "C-x o l i o o u")
      (next-line)
      (ws-test-should (looking-at "^Line 1")))
    (ws-test "Mixture 2" num-win win-spl save
      (ws-test-interactive-cmds "C-x o o i u")
      (next-line)
      (ws-test-should (looking-at "^Line 1")))))

(def-ws-test-suite default4-nowrap-noother
  "Four equal windows, default config except no wrap, no other-window."
  :settings (other-window-first nil
             (win-switch-set-wrap-around -1))
  (let ((num-win 4)
        (win-spl '((vert) (horiz horiz)))
        (save    nil))
    (ws-test "Directional Keys 1" num-win win-spl save
      (ws-test-should-error
       (ws-test-interactive-cmds "C-x o i l k j u")
       (next-line)
       (looking-at "^Line 1")))
    (ws-test "Directional Keys 2" num-win win-spl save
      (ws-test-interactive-cmds "C-x o l k j i u")
      (next-line)
      (ws-test-should (looking-at "^Line 1")))
    (ws-test "Directional Keys 3" num-win win-spl save
      (ws-test-interactive-cmds "C-x o k l i j u")
      (next-line)
      (ws-test-should (looking-at "^Line 1")))
    (ws-test "Other Window Key" num-win win-spl save
      (ws-test-interactive-cmds "C-x o o o o o u")
      (next-line)
      (ws-test-should (looking-at "^Line 1")))
    (ws-test "Previous Window Key" num-win win-spl save
      (ws-test-interactive-cmds "C-x o p p p p u")
      (next-line)
      (ws-test-should (looking-at "^Line 1")))
    (ws-test "Mixture 1" num-win win-spl save
      (ws-test-interactive-cmds "C-x o o k j o o u")
      (next-line)
      (ws-test-should (looking-at "^Line 1")))
    (ws-test "Mixture 2" num-win win-spl save
      (ws-test-interactive-cmds "C-x o o o i u")
      (next-line)
      (ws-test-should (looking-at "^Line 1")))))

(def-ws-test-suite default4-arrows-C
  "Four equal windows, default parameters, arrow-keys with dispatch."
  :settings ((win-switch-setup-keys-arrows 'control))
  (let ((num-win 4)
        (win-spl '((vert) (horiz horiz)))
        (save    nil))
    (ws-test "Directional Keys 1" num-win win-spl save
      (ws-test-interactive-cmds "<C-up> <right> <up> <left> <RET>")
      (next-line)
      (ws-test-should (looking-at "^Line 1")))
    (ws-test "Directional Keys 2" num-win win-spl save
      (ws-test-interactive-cmds "<C-right> <down> <left> <up> <RET>")
      (next-line)
      (ws-test-should (looking-at "^Line 1")))
    (ws-test "Directional Keys 3" num-win win-spl save
      (ws-test-interactive-cmds "<C-down> <right> <up> <up> <left> <up> <RET>")
      (next-line)
      (ws-test-should (looking-at "^Line 1")))
    (ws-test "Other Window Key" num-win win-spl save
      (ws-test-interactive-cmds "<C-left> / / / <RET>")
      (next-line)
      (ws-test-should (looking-at "^Line 1")))
    (ws-test "Previous Window Key" num-win win-spl save
      (ws-test-interactive-cmds "<C-up> . . <RET>")
      (next-line)
      (ws-test-should (looking-at "^Line 1")))
    (ws-test "Mixture 1" num-win win-spl save
      (ws-test-interactive-cmds "<C-right> <down> <left> / . <up> <RET>")
      (next-line)
      (ws-test-should (looking-at "^Line 1")))
    (ws-test "Mixture 2" num-win win-spl save
      (ws-test-interactive-cmds "<C-down> . . <up> <right> <down> <left> <RET>")
      (next-line)
      (ws-test-should (looking-at "^Line 1")))))

(def-ws-test-suite default4-arrows-C-sizes
  "Resizing four equal windows, default parameters, arrow-keys with dispatch."
  :settings ((win-switch-setup-keys-arrows 'control))
  (let ((num-win 4)
        (win-spl '((vert) (horiz horiz)))
        (save    nil)
        (width-height (lambda (x)
                        (and x
                             (list (- (third x)  (first x))
                                   (- (fourth x) (second x)))))))
    ;; Each test checks that the windows have changed size
    ;; (either height or width or both) by the correct amount.
    (ws-test "Vertical Resizing 1" num-win win-spl save
      (let ((sizes (ws-test-window-sizes width-height))
            (newsizes nil)
            (use-dialog-box nil))
        (ws-test-interactive-cmds "<C-down> <S-up> <S-up> <S-up> <RET>")
        (setq newsizes (ws-test-window-sizes width-height))
        (ws-test-should (and (= 3 (- (second (aref newsizes 1)) (second (aref sizes 1))))
                             (= 3 (- (second (aref sizes 0))    (second (aref newsizes 0))))))))
    (ws-test "Vertical Resizing 2" num-win win-spl save
      (let ((sizes (ws-test-window-sizes width-height))
            (newsizes nil))
        (ws-test-interactive-cmds "<C-down> <right> <S-up> <S-down> <S-up> <S-up> <RET>")
        (setq newsizes (ws-test-window-sizes width-height))
        (ws-test-should (and (= 2 (- (second (aref newsizes 3)) (second (aref sizes 3))))
                             (= 2 (- (second (aref sizes 2))    (second (aref newsizes 2))))))))
    (ws-test "Horizontal Resizing 1" num-win win-spl save
      (let ((sizes (ws-test-window-sizes width-height))
            (newsizes nil))
        (ws-test-interactive-cmds "<C-down> <S-left> <S-left> <S-left> <RET>")
        (setq newsizes (ws-test-window-sizes width-height))
        (ws-test-should (and (= 3 (- (first (aref newsizes 3)) (first (aref sizes 3))))
                             (= 3 (- (first (aref sizes 1))    (first (aref newsizes 1))))))))
    (ws-test "Horizontal Resizing 2" num-win win-spl save
      (let ((sizes (ws-test-window-sizes width-height))
            (newsizes nil))
        (ws-test-interactive-cmds "<C-down> <S-right> <S-right> <S-left> <S-left> <S-right> <RET>")
        (setq newsizes (ws-test-window-sizes width-height))
        (ws-test-should (and (= 1 (- (first (aref newsizes 1)) (first (aref sizes 1))))
                             (= 1 (- (first (aref sizes 3))    (first (aref newsizes 3))))))))
    (ws-test "Mixed Resizing 1" num-win win-spl save
      (let ((sizes (ws-test-window-sizes width-height))
            (newsizes nil))
        (ws-test-interactive-cmds "<C-down> <S-right> <S-right> <S-up> <S-up> <S-up> <RET>")
        (setq newsizes (ws-test-window-sizes width-height))
        (ws-test-should (and (= 3 (- (second (aref newsizes 1)) (second (aref sizes 1))))
                             (= 2 (- (first (aref newsizes 1)) (first (aref sizes 1))))))))))

(def-ws-test-suite uneven4-ijkl
  "Four windows, ijkl configuration, no other."
  :settings (other-window-first nil)
  (let ((num-win 4)
        (win-spl '((v) (nil h) (nil nil (5 nil))))
        (save    nil)
        (use-dialog-box nil))
    (ws-test "Directional Keys 1" num-win win-spl save
      (ws-test-interactive-cmds "C-x o k l k j i u")
      (next-line)
      (ws-test-should (looking-at "^Line 1")))
    (ws-test "Directional Keys 2" num-win win-spl save
      (ws-test-interactive-cmds "C-x o k u")
      (goto-char (point-max))
      (redisplay t) ; ATTN seems to allow windmove to calculate correctly
      (ws-test-interactive-cmds "C-x o l i i u")
      (next-line)
      (ws-test-should (looking-at "^Line 1")))
    (ws-test "Directional Keys 3" num-win win-spl save
      (ws-test-interactive-cmds "C-x o k l i i u")
      (ws-test-should (looking-at "^Line 1")))
    (ws-test "Other Window Key" num-win win-spl save
      (ws-test-interactive-cmds "C-x o o o o o u")
      (next-line)
      (ws-test-should (looking-at "^Line 1")))
    (ws-test "Previous Window Key" num-win win-spl save
      (ws-test-interactive-cmds "C-x o p p p p u")
      (next-line)
      (ws-test-should (looking-at "^Line 1")))
    (ws-test "Mixture 1" num-win win-spl save
      (ws-test-interactive-cmds "C-x o l i o o j i u")
      (next-line)
      (ws-test-should (looking-at "^Line 1")))
    (ws-test "Mixture 2" num-win win-spl save
      (ws-test-interactive-cmds "C-x o p l i u")
      (next-line)
      (ws-test-should (looking-at "^Line 1")))))

(def-ws-test-suite default3
  "Three windows, default configuration."
  (let ((num-win 3)
        (win-spl '((horiz) (nil vert)))
        (save    nil))
    (ws-test "Directional Keys 1" num-win win-spl save
      (ws-test-interactive-cmds "C-x o p i l k j u")
      (next-line)
      (ws-test-should (looking-at "^Line 1")))
    (ws-test "Directional Keys 2" num-win win-spl save
      (ws-test-interactive-cmds "C-x o k j i i u")
      (next-line)
      (ws-test-should (looking-at "^Line 1")))
    (ws-test "Directional Keys 3" num-win win-spl save
      (ws-test-interactive-cmds "C-x o k j i l l i u")
      (next-line)
      (ws-test-should (looking-at "^Line 1")))
    (ws-test "Other Window Key" num-win win-spl save
      (ws-test-interactive-cmds "C-x o o o u")
      (next-line)
      (ws-test-should (looking-at "^Line 1")))
    (ws-test "Previous Window Key" num-win win-spl save
      (ws-test-interactive-cmds "C-x o p p p p u")
      (next-line)
      (ws-test-should (looking-at "^Line 1")))
    (ws-test "Mixture 1" num-win win-spl save
      (ws-test-interactive-cmds "C-x o l i o o o u")
      (next-line)
      (ws-test-should (looking-at "^Line 1")))
    (ws-test "Mixture 2" num-win win-spl save
      (ws-test-interactive-cmds "C-x o o i u")
      (next-line)
      (ws-test-should (looking-at "^Line 1")))))

(def-ws-test-suite basic6
  "Six windows, default configuration except no other-window."
  :settings (other-window-first nil)
  (let ((num-win 6)
        (win-spl '((horiz) (vert vert) (nil nil vert vert)))
        (save    nil))
    (ws-test "Directional Keys 1" num-win win-spl save
      (ws-test-interactive-cmds "C-x o k l i j k k l u")
      (ws-test-should (looking-at "^Line 80")))
    (ws-test "Directional Keys 2" num-win win-spl save
      (ws-test-interactive-cmds "C-x o l k j k l i u")
      (ws-test-should (looking-at "^Line 48")))
    (ws-test "Directional Keys 3" num-win win-spl save
      (ws-test-interactive-cmds "C-x o j i l i l i j k u")
      (ws-test-should (looking-at "^Line 32")))
    (ws-test "Other Window Key" num-win win-spl save
      (ws-test-interactive-cmds "C-x o o o o o o o u")
      (next-line)
      (ws-test-should (looking-at "^Line 1")))
    (ws-test "Previous Window Key" num-win win-spl save
      (ws-test-interactive-cmds "C-x o p p p p p p u")
      (next-line)
      (ws-test-should (looking-at "^Line 1")))
    (ws-test "Mixture 1" num-win win-spl save
      (let ((result t))
        (ws-test-interactive-cmds "C-x o k l u")
        (setq result (and result (looking-at "^Line 48")))
        (ws-test-interactive-cmds "C-x o i j o p u")
        (next-line)
        (setq result (and result (looking-at "^Line 1")))
        (ws-test-interactive-cmds "C-x o k k u")
        (setq result (and result (looking-at "^Line 64")))
        (ws-test-interactive-cmds "C-x o o u")
        (next-line 4)
        (setq result (and result (looking-at "^Line 20")))
        (ws-test-should result)))
    (ws-test "Mixture 2" num-win win-spl save
      (ws-test-interactive-cmds "C-x o o l p j u")
      (next-line)
      (ws-test-should (looking-at "^Line 1")))))

(def-ws-test-suite exit5
  "Exit conditions, five windows, no-other configuration."
  :settings (other-window-first nil)
  (let ((num-win 5)
        (win-spl '((horiz) (vert vert) (nil nil nil vert)))
        (save    nil)
        (where   (lambda (cmd)
                   (key-description (where-is-internal cmd '(keymap) t)))))
    (ws-test "Exit on Nav Ctrl" num-win win-spl save
      (let ((C-n (funcall where 'next-line)))
        (ws-test-interactive-cmds
         (concat "C-x o o o o o o " C-n))
        (ws-test-should (looking-at "^Line 1"))))
    (ws-test "Exit on Nav Meta" num-win win-spl save
      (let ((M-e (funcall where 'forward-sentence)))
        (ws-test-interactive-cmds
         (concat "C-x o o o o o o " M-e))
        (forward-line 0)
        (ws-test-should (looking-at "^Line 15"))))
    (ws-test "Exit on Help" num-win win-spl save
      (kill-buffer (get-buffer-create "*Help*"))
      (let ((result nil)
            (help-window-select 'other) ; Emacs 23+
            (C-h (key-description (vector help-char))))
        (ws-test-interactive-cmds
         (concat "C-x o o o o o o " C-h " l"))
        (ws-test-should (and (get-buffer "*Help*")
                             (window-live-p (get-buffer-window "*Help*"))))))))


(provide 'ws-tests)

;; Local Variables:
;; mode: emacs-lisp
;; mode: linkd
;; End:

;;; ws-tests.el ends here
