;;; todostack.el --- keep a to-do list as a simple stack

;; Copyright (C) 2011, Evans Winner <ego111@gmail.com>

;; Author: Evans Winner <ego111@gmail.com>
;; Time-stamp: <2011-05-14 03:24:15 thorne>
;; Keywords: calendar
   (defvar todostack-version 1)
;; License: GPL3

;; See the end of this file for license information.
;; This file is not part of GNU Emacs

 
;;; Commentary:

;; GENERAL USE

;; Todostack is a trivial (and experimental -- use at your own risk)
;; todo list manager for a particular way of doing things that may or
;; may not appeal to you.  It uses a stack as a todo-list system.  The
;; idea is to reduce the effect of distractions.  When you have to
;; switch tasks, push what you are currently doing onto the stack with
;; command `todostack-push', then when you return, inspect the stack
;; with `todostack'.  When you have finished the task currently on the
;; top of the stack, pop it off with `todostack-pop'

;; Command `todostack-list' will list the whole current stack in order
;; from top to bottom.  It uses the value of `todostack-list-function'
;; to decide how to do it, so you can easily write your own list
;; function which should be a function of zero arguments.  Three such
;; functions are pre-defined: First, `todostack-list-buffer' and
;; `todostack-list-echo-area' -- the first creates a buffer and lists
;; the stack there; the second does what it sounds like.  The default,
;; however, is `todostack-list-dwim' which tries to decide if the list
;; will fit in the echo area and if so displays it there using the
;; first of those functions, and if not displays it in a buffer using
;; the second of those functions.

;; You can also use command `todostack-procrastinate' to push back the
;; top item on the list by one space (or farther with a prefix arg)
;; and you can use `todostack-rotate' to send the top item all the way
;; to the back.  You can use `todostack-queue' to add something to the
;; bottom of the stack instead of the top, which lets you treat the
;; stack more like a queue.

;; Commands `todostack-save' and `todostack-load' will save and load
;; the todostack to a file specified in the variable
;; `todostack-save-file'.  The default is
;; "~/.emacs.d/todostacksave.el".  Also, `todostack-backup' will make
;; a backup copy of the current stack in a file in the same directory
;; and same name with an extention named by variable
;; `todostack-backup-extension'.  This is a very good function to add
;; to `todostack-before-save-hook' -- so good, in fact, that I have
;; made it the default and set the extention to ".bak".  See also
;; `todostack-rescue'.  Finally, if you want to hack on it, see
;; `todostack-test'.

;; LOADING

;; You can use the customize interface if you like it. I use use the
;; following in my .emacs.  The hooks allow my stack to persist across
;; Emacs sessions and in case of a crash, by aggressively saving the
;; stack whenever I modify it.  I strongly recommend their use:

;;    (load-file "/PATH/TO/todostack.el")
;;    (load-file todostack-save-file)
;;    (add-hook 'kill-emacs-hook 'todostack-save)
;;    (add-hook 'emacs-startup-hook 'todostack-load)
;;    (add-hook 'todostack-post-op-hook 'todostack-save)

;; I also bind some of the most-used functions to keys of my choice.

;; OTHER FUNCTIONS

;; You can have multiple todo stacks, but can operate on only one at a
;; time -- whichever is named by the variable
;; `todostack-current-stack'.  The default is the aptly-named
;; `todostack-default-stack'.  A stack is simply a Lisp list, so you
;; need only initialize a symbol to nil and then set
;; `todostack-current-stack' to point to it.  But REMEMBER TO ALSO
;; CHANGE `todostack-save-file', or else your old saved todostack will
;; get clobbered the first time you try to save the new stack.  For
;; example:

;;    (defvar mystack '())
;;    (setq todostack-current-stack 'mystack)
;;    (setq todostack-save-file "~/mystacksave.el")

;; ORG-MODE INTEGRATION

;; Org-mode users may find it useful to use command
;; `todostack-org-snarf' to convert a buffer with org TODO items into
;; a stack.  See the doc string for `todostack-org-snarf' for more
;; information.  You will probably want to set or customize the
;; following:

;;    (setq todostack-list-function 'todostack-list-buffer)
;;    (setq todostack-list-line-prefix "* TODO ")
;;    (add-hook 'todostack-post-list-buffer-hook 'org-mode)

;; CHANGELOG

;; 2011/5/14: Initial release. EW

;;; Code:

;; REQUIRES

(require 'lisp-mnt nil t)  ;we use it only if it's available

;; VAIRIABLES

(defvar todostack-default-stack '()
  "The default todostack.")

(defvar todostack-current-stack 'todostack-default-stack
  "The stack to operate currently.")

(defgroup todostack nil
  "Customization group for todostack."
  :group 'applications
  :prefix 'todostack)

;; These hooks are usually actually run after the stack is modified,
;; but before the final call to `todostack' to show the top of the
;; stack.  This is because there is no way that I know of to disable
;; the message to the echo area that the file was saved.
(defcustom todostack-post-push-hook nil
  "Hooks run after push of the stack.
Generally use `todostack-post-op-hook' instead."
  :type 'hook :group 'todostack)

(defcustom todostack-post-pop-hook nil
  "Hooks run after pop of the stack.
Generally use `todostack-post-op-hook' instead."
  :type 'hook :group 'todostack)

(defcustom todostack-post-queue-hook nil
  "Hooks run after a job is queued.
Generally use `todostack-post-op-hook' instead."
  :type 'hook :group 'todostack)

(defcustom todostack-post-op-hook nil
  "Hooks run after any stack-modifying function is run.
This is generally the only hook you should need."
  :type 'hook
  :options  '(todostack-save)
  :group 'todostack)

(defcustom todostack-post-list-buffer-hook nil
  "Hooks run after the stack is listed in a buffer.
Turn on a mode, etc."
  :type 'hook
  :options  '(org-mode)
  :group 'todostack)

(defcustom todostack-before-save-hook '(todostack-backup)
  "Hooks run before `todostack-save' does its work."
  :type 'hook
  :group 'todostack)

(defcustom todostack-list-function 'todostack-list-dwim
  "Function to run when requesting a list of the current stack.
Should be a lambda or function of no arguments."
  :type 'function :group 'todostack)


(defcustom todostack-list-line-prefix ""
  "A string to append before each line in a a buffer listing of
the stack.  Org-mode users might try \'* TODO \""
  :type 'string :group 'todostack
  :options '(todotack-list-line-prefix-org))

(defcustom todostack-save-file
  (expand-file-name
   "~/.emacs.d/todostacksave.el")
  "The name of the file in which to save the todostack."
  :type 'file :group 'todostack)

(defcustom todostack-backup-extension ".bak"
  "File extension to tack on to `todostack-save-file' when making
automatic backups of the stack."
  :type 'string :group 'todostack)

(defcustom todostack-list-buffer-name "*todostack*"
  "Name to use for the `todostack-list' buffer.
This should apply if `todostack-list-function' points to a
function like `todostack-list-buffer'.  Irrelevant if it points
to something that does not create a new buffer."
  :type 'string :group 'todostack)

;; This got a bit ridiculous.  The file name munging is about making
;; it work when todostack.el is byte-compiled.  The idea here is to
;; use the file headers for the doc string for function `todostack'.
(defvar todostack-documentation
  (if (and (fboundp 'lm-commentary)
	   (file-exists-p
	    (concat (file-name-sans-extension load-file-name)
		    ".el")))
      (substring		      ;get rid of ";;; Commentary:\n"
       (replace-regexp-in-string      ;get rid of comment chars
	"^;; " ""
	(lm-commentary
	 (concat
	  (file-name-sans-extension load-file-name) ".el")))
       (length ";;; Commentary:\n"))
    "Examine but do not modify the top of the stack.") ;fallback
  "Try to use the library headers as the doc string for function
`todostack'.")

 
;; FUNCTIONS

;; Basic functions

(defun todostack-get-current-stack ()
  "Return the current todo stack."
  (eval todostack-current-stack))

;;;###autoload
(defun todostack-push (job)
  "Add a to-do item to the top of the todo stack."
  (interactive "MPush on to top of stack: ")
  (set todostack-current-stack
       (cons job (todostack-get-current-stack)))
  (run-hooks 'todostack-post-push-hook)
  (run-hooks 'todostack-post-op-hook)
  (todostack))

;;;###autoload
(defun todostack-pop ()
  "Remove the top-most entry from the todostack."
  (interactive)
  (let ((old (car (todostack-get-current-stack))))
    (if (todostack-get-current-stack)
	(progn
	  (set todostack-current-stack
	       (cdr (todostack-get-current-stack)))
	  (run-hooks 'todostack-post-pop-hook)
	  (run-hooks 'todostack-post-op-hook)
	  (message
	   (concat old " popped.\n"
		   (if (todostack-get-current-stack)
		       (concat "New top: "
			       (car (todostack-get-current-stack)))
		     "Todostack empty"))))
      (message "Todostack empty"))))

;; The definition of funtion `todostack' is a hack to allow us to use
;; the library headers as the doc string because I hate writing the
;; same thing twice.  First we defun it so it can be autoloaded (not
;; sure if this works, as I never use autoloads).
;;;###autoload
(defun todostack ()
  nil)
;; Then we redefine it with a back-quote to allow use of a variable
;; contents as doc-string.
(fset 'todostack
      `(lambda ()
	 ,todostack-documentation
	 (interactive)
	 (if (todostack-get-current-stack)
	     (message (car (todostack-get-current-stack)))
	   (message "Todostack empty"))))

 
;; Listing funtions.  `todostack-list' calls whichever one is named in
;; variable `todostack-list-function'.
(defun todostack-list-buffer ()
  "Create a buffer full of what you have to do."
  (if (todostack-get-current-stack)
      (progn
	(switch-to-buffer
	 (get-buffer-create todostack-list-buffer-name))
	(kill-region (point-min) (point-max))
	(dolist (job (todostack-get-current-stack))
	  (insert (concat todostack-list-line-prefix job "\n")))
	(message "Things to do")
	(run-hooks 'todostack-post-list-buffer-hook))
    (message "Todostack empty")))

(defun todostack-list-echo-area ()
  "List what you have to do in the echo area.
If your todostack might be too large to fit in the echo area, a
better function to use might be `todostack-list-dwim'."
  (if (todostack-get-current-stack)
      (let* ((string "")
	     (string
	      (dolist (job (todostack-get-current-stack) string)
		(setq string (concat string job "\n")))))
	;; Get rid of the last newline
	(message (substring string 0 (1- (length string)))))
    (message "Todostack empty")))

(defun todostack-list-dwim ()
  "List the todo stack in the echo area if it will fit.
Otherwise create a temporary buffer to list it."
  (if (> (length (todostack-get-current-stack))
	 (if (integerp max-mini-window-height)
	     max-mini-window-height
	   (floor (* (frame-height) max-mini-window-height))))
      (todostack-list-buffer)
    (todostack-list-echo-area)))

;;;###autoload
(defun todostack-list ()
  "List the contents of the todo stack.
Simply calls the function named by the variable
`todostack-list-function'."
  (interactive)
  (funcall todostack-list-function))

 
;; Convenience functions
;;;###autoload
(defun todostack-procrastinate (n)
  "Push the top item of the todostack N positions back.
Default is 1.  If N is larger than the length of the todostack,
leave the top item at the back of the todostack.
Then show the new top item in the echo area."
  (interactive "p")
  (if (> n (1- (length (todostack-get-current-stack))))
      (todostack-rotate 1)
    (let* ((n (1+ n))
	   (old (todostack-get-current-stack))
	   (procrastinand (car old))
	   (new-tail (cons procrastinand (nthcdr n old)))
	   (new-head (cdr (butlast old (- (length old) n)))))
      (set todostack-current-stack (append new-head new-tail))))
  (run-hooks 'todostack-post-op-hook)
  (todostack))

;;;###autoload
(defun todostack-rotate (n)
  "Push the first item in the todo stack back to the last position.
With prefix arg, do this N consecutive times.  Then show the new
top item in the echo area."
  (interactive "p")
  (dotimes (foo n (todostack-get-current-stack))
      (set todostack-current-stack
	(append (cdr (todostack-get-current-stack))
		(list (car (todostack-get-current-stack))))))
  (run-hooks 'todostack-post-op-hook)
  (todostack))

;;;###autoload
(defun todostack-queue (job)
  "Add a new item directly to the bottom of the stack.
This is a way to use the stack more like a queue."
  (interactive "MAdd to bottom of stack: ")
  (let ((todostack-post-push-hook nil)
	(todostack-post-op-hook nil))
    (todostack-push job))
  (todostack-rotate 1)
  (run-hooks 'todostack-post-queue-hook)
  (run-hooks 'todostack-post-op-hook))

;;;###autoload
(defun todostack-org-snarf (&optional p)
  "Add TODO items in an org-mode style list to the todo stack.
With prefix argument replace the current todo stack with the
contents of the buffer.

Items are pushed to the top of the stack from the bottom-most
TODO item in the buffer to the top-most, leaving the todo stack
looking like the buffer, thus:

** TODO A
*** TODO B
* TODO C

Will end up creating a stack with \"A\" at the top and \"C\" at
the bottom.  If this is not good, one possibility is to snarf
your TODO list, then use `todostack-list-buffer' to create a new
list, re-organize it and then re-snarf that with the prefix arg.

Items added will be anything on a line consisting of any number
of asterisk characters followed by the string TODO and a space.
Lines that do not match are ignored."
  (interactive "P")
  ;; Build the whole list first --
  (let ((temp '()))
    (save-excursion
      (goto-char (point-max))
      (while
	  (re-search-backward "^\** TODO " nil t)
	(let ((beginning (search-forward "TODO ")))
	  (end-of-line)
	  (setq temp
	   (cons (buffer-substring-no-properties beginning (point))
	  temp)))
      (beginning-of-line)))
  ;; -- then either append to the current stack or replace the stack,
  ;; depending on prefix arg.
    (if p (set todostack-current-stack temp)
      (set todostack-current-stack
	   (append temp (todostack-get-current-stack)))))
  (run-hooks 'todostack-post-op-hook)
  (todostack))

(defun todostack-help ()
  "Display some help for todostack."
  (interactive)
  (describe-function 'todostack))

 
;; Save and load.
(defun todostack-save ()
  "Save the todostack to a file named by `todostack-save-file'."
  (interactive)
  (run-hooks 'todostack-before-save-hook)
  (with-temp-buffer
    (insert
     (concat
      ";; This file was created programatically by todostack.el."
      "\n;; Edit by hand at your own risk.\n"
      "\n\(setq "
      (prin1-to-string todostack-current-stack)
      " '"
      (prin1-to-string (todostack-get-current-stack)) "\)"))
    (write-file (expand-file-name todostack-save-file))))

(defun todostack-load ()
  "Load the todostack from the file named by
`todostack-save-file'."
  (interactive)
  (load-file (expand-file-name todostack-save-file)))

(defun todostack-backup (&optional tofile)
  "Make a backup copy of the current todostack."
  (unless (file-exists-p todostack-save-file) ;first time it is run
    (with-temp-buffer (write-file todostack-save-file)))
  (let ((file (if tofile tofile todostack-save-file)))
    (copy-file todostack-save-file
	       (concat (expand-file-name file)
		       todostack-backup-extension) t)))

(defun todostack-rescue (&optional file)
  "Load the todostack backup file but do not save it."
  (interactive)
  (let ((file (if file file
		(concat (expand-file-name todostack-save-file)
			todostack-backup-extension))))
    (if (file-exists-p file)
	(progn (load-file file)
	       (message "Backup loaded. You must save it manually\
 with todostack-save."))
      (error "Backup file by that name does not exist"))))

(defun todostack-test ()
  "Quick and dirty check to make sure basic things are working.
Note that this will produce a save file for the test."
  (let* ((stack '())
	(todostack-default-stack stack)
	(todostack-save-file "~/.emacs.d/todostacktest.el")
	(todostack-list-function 'todostack-list-buffer)
	(todostack-list-line-prefix ""))
    (todostack-push "A")                ;A
    (todostack-push "B")                ;BA
    (todostack-push "C")                ;CBA
    (todostack-queue "D")               ;CBAD
    (todostack-rotate 1)                ;BADC
    (todostack-rotate 2)                ;DCBA
    (todostack-pop)                     ;CBA
    (todostack-procrastinate 1)         ;BCA
    (todostack-push "D")                ;DBCA
    (todostack-procrastinate 200)       ;BCAD
    (todostack-procrastinate 2)         ;CABD
    (todostack-procrastinate 2)         ;ABCD
    (todostack-list)
    (message "Should say: A B C D.")))

(provide 'todostack)

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.

;;; todostack.el ends here
