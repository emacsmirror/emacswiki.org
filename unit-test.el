;;; unit-test.el --- Run unit tests from Emacs with visual feedback
;;
;; Author: Mark Triggs <mark@dishevelled.net>
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;;; Commentary:
;;
;; This code displays the results of unit tests with a small image in the
;; mode line.  To use this code, bind the functions `run-unit-tests',
;; `open-unit-test-file' and `set-unit-test-command' to convenient keys
;; with something like:
;;
;;   (define-key global-map (kbd "C-c t") 'run-unit-tests)
;;   (define-key global-map (kbd "C-c s") 'set-unit-test-command)
;;   (define-key global-map (kbd "C-c o") 'open-unit-test-file)
;;
;; You will need to define a value for `unit-test-command' in any buffer you
;; are interested in running tests from.  This should be a function that does
;; whatever is necessary to run your unit tests, and returns:
;;
;;   * nil if some tests failed.
;;   * `handled' if the unit-test-command will update the mode line itself
;;     (using `show-test-status' as described below) at some later time.
;;   * any other non-nil value if all tests passed.
;;
;; The `handled' case is intended to allow you to use asynchronous commands to
;; run your unit tests.  To signal the test results, the handler should
;; evaluate either (show-test-status 'passed) or (show-test-status 'failed).
;;
;; In the simplest case a test function might look like:
;;
;;   (setq unit-test-command (lambda () (zerop (shell-command "make test"))))
;;
;;
;; To set a single command for a whole directory tree, you might use something
;; like:
;;
;;   (add-hook 'find-file-hook 'apply-project-settings)
;;
;;   (defun apply-project-settings ()
;;     (let ((dir (expand-file-name default-directory)))
;;       (cond ((string-match (expand-file-name "~/projects/some-project") dir)
;;              (setq unit-test-command
;;                    (lambda ()
;;                      (let ((status (shell-command
;;                                     "cd ~/projects/some-project; make")))
;;                        (cond ((equal status 0)
;;                               (message "All tests passed")
;;                               t)
;;                              (t (message "Some tests failed")
;;                                 nil))))))
;;             ...)))
;;
;; where running "make" is assumed to run all unit tests and return an error
;; status indicating success or failure.

;;; Code:

(defvar unit-test-command nil
  "A function that runs the unit tests for this project.
This should have no required arguments and return nil if tests failed,
`handled' if the test indicator will be updated later, or non-nil otherwise.

Examples:

  (setq unit-test-command 'my-defun)
  (setq unit-test-command (lambda () ...))
  (setq unit-test-command (lambda (&optional arg)
                            (interactive \"P\")
                            ...))
")

(make-variable-buffer-local 'unit-test-command)

(defvar unit-test-file-fn nil
  "A function that takes an absolute path to a file and returns an absolute
path to the corresponding file of unit tests, or nil if this file has no
unit tests.")
(make-variable-buffer-local 'unit-test-file-fn)


(defvar unit-test-colours '(("orange"      . "#FF9900")
                            ("dark-orange" . "#E86400")
                            ("green"       . "#00FF00")
                            ("dark-green"  . "#00C400")
                            ("red"         . "#FF0000")
                            ("dark-red"    . "#C40000")))

(defun unit-test-dot (colour)
  "Return an XPM string representing a dot whose colour is COLOUR."
  (format "/* XPM */
static char * test_pass_xpm[] = {
\"18 13 4 1\",
\" 	c None\",
\".	c #000000\",
\"+	c %s\",
\"c	c %s\",
\"                  \",
\"       .....      \",
\"      .ccccc.     \",
\"     .cc+++cc.    \",
\"    .cc+++++cc.   \",
\"    .c+++++++c.   \",
\"    .c+++++++c.   \",
\"    .c+++++++c.   \",
\"    .cc+++++cc.   \",
\"     .cc+++cc.    \",
\"      .ccccc.     \",
\"       .....      \",
\"                  \"};"
          (cdr (assoc colour unit-test-colours))
          (cdr (assoc (concat "dark-"colour) unit-test-colours))))


(defvar unit-test-passed-xpm (unit-test-dot "green")
"An XPM image displayed in the mode-line when all unit tests pass.")

(defvar unit-test-failed-xpm (unit-test-dot "red")
"An XPM image displayed in the mode-line when some unit tests fail.")

(defvar unit-test-running-xpm (unit-test-dot "orange")
"An XPM image displayed in the mode-line while tests are running.")

(defvar unit-test-passed-string ":o)"
"A string displayed in the mode-line when all unit tests pass.")

(defvar unit-test-failed-string ":o("
"A string displayed in the mode-line when some unit tests fail.")

(defvar unit-test-running-string ":o0"
"A string displayed in the mode-line while tests are running.")


(defvar unit-tests-passed-hook '())
(defvar unit-tests-failed-hook '())

(defun show-test-status (status)
  (with-current-buffer (or last-unit-test-buffer
                           (current-buffer))
    (let ((map (make-sparse-keymap)))
      (define-key map [mode-line mouse-1] 'show-test-none)
      (setq mode-line-buffer-identification
            (if (and window-system
                     (member 'xpm image-types))
                `(,(propertize " %b"
                               'help-echo (case status
                                            (passed
                                             "Tests passed")
                                            (failed
                                             "Some tests failed")
                                            (running
                                             "Tests running"))
                               'keymap map
                               'display
                               `(image :type xpm
                                       :data ,(case status
                                                (passed unit-test-passed-xpm)
                                                (failed unit-test-failed-xpm)
                                                (running
                                                 unit-test-running-xpm))
                                       :ascent center)))
              `(,(format " [%s] %%b"
                         (case status
                           (passed unit-test-passed-string)
                           (failed unit-test-failed-string)
                           (running unit-test-running-string))))))
      (ignore-errors
        (force-mode-line-update)
        (redraw-modeline)))))

(defun show-test-none ()
  (interactive)
  (setq mode-line-buffer-identification '(#("%12b ")))
  (when (fboundp 'redraw-modeline) (redraw-modeline)))

(defvar last-unit-test-buffer nil)

(defun run-unit-tests ()
  (interactive)
  (unless unit-test-command
    (set-unit-test-command))
  (setq last-unit-test-buffer (current-buffer))
  (show-test-status 'running)
  (sit-for 0)
  (let ((result (if (commandp unit-test-command)
                    (call-interactively unit-test-command)
                  (funcall unit-test-command))))
    (cond ((eq result 'handled) nil)
          (result
           (run-hooks 'unit-tests-passed-hook)
           (show-test-status 'passed))
          (t (run-hooks 'unit-tests-failed-hook)
             (show-test-status 'failed)))))

(defun set-unit-test-command ()
  (interactive)
  (setq unit-test-command
        (read-from-minibuffer "Function to run unit tests: "
                              (format "%S" unit-test-command)
                              read-expression-map t
                              'read-expression-history)))

(defun open-unit-test-file ()
  "Open the file of unit tests for the current buffer"
  (interactive)
  (if (and (boundp 'unit-test-window-configuration))
      (set-window-configuration unit-test-window-configuration)
    (let ((window-configuration (current-window-configuration))
          (file (buffer-file-name (current-buffer))))
      (if (and file unit-test-file-fn)
          (let ((unit-tests (funcall unit-test-file-fn file)))
            (when unit-tests
              (pop-to-buffer (or (find-buffer-visiting unit-tests)
                                 (find-file-noselect unit-tests)))
              (set (make-local-variable 'unit-test-window-configuration)
                   window-configuration)))
        (message "No unit test file known for this buffer.")))))


(provide 'unit-test)
;;; unit-test.el ends here
