;;; screencast.el --- demonstrate the capabilities of Emacs

;; Copyright (C) 2009 ESBEN Andreasen <esbenandreasen@gmail.com>

;; Authors: esbenandreasen <esbenandreasen@gmail.com>(new)

;; Keywords: demo screencast

;; This file is not an official part of Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This file allows you to create video-like sessions, which
;; demonstrates the capabilities of Emacs.

;;; Usage:

;; Install this file to an appropriate directory in your load-path,
;; and add these expressions to your ~/.emacs

;; (auto-load 'screencast "screencast")

;; Try it out by evaluating (screencast-screencast-producer) and (screencast-screencast-user)

;; Your own screencast files should have a (require 'screencast)

;;; Conventions:

;; PRODUCER : creates a screencast

;; USER     : sees a screencast

;; producer sections in this document contains variables which the producer
;; should modify as need be, and functions to be called during the creation of a
;; screencast

;;; Code:

(defconst screencast-message-buffer-name "*Screencast Messages*"
  "The name of the buffer to put messages from the screencast in")
(defconst screencast-version 1 "The version number of the screencast-mode")

;;;; BEGIN PRODUCER VARIABLES
;; these variables should be changed as needed by the producer
(defvar screencast-pause-length 1 "The length of a pause ('p) in the screencast")
(defvar screencast-pause-char-length 0.15 
  "The time between each typed character in the function `screencast-insert-with-delay'")
(defvar screencast-pause-command-length 2
  "The time between the announcement of the function call, and the call itself.")
(defvar screencast-dont-print-list '(progn i n) 
  "A list of lists of function names which aren't printed as
  being evaluated in the messages, this includes all producer
  functions by default")
;;;; END PRODUCER VARIABLES

;;;; BEGIN RUNTIME VARIABLES
;; variables which can be read during runtime to obtain information about the
;; current screencast
(defvar screencast-nopause-global nil
  "Variable to be used for user functions if they are using
 pauses, they should deactivate the pause if this is true.")
;;;; END RUNTIME VARIABLES

;;;; BEGIN MODE
(defvar screencast-mode-map 
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'screencast-goto-step)
    map)
  "Keymap for `screencast-mode'."
  )
(define-derived-mode screencast-mode nil "screencast"
  "Major mode for viewing screencasts."
  (auto-fill-mode 1)
  )
;;;; END MODE

;;;; BEGIN PRODUCER FUNCTIONS
(defun i (string)
  "Screencast producer function. _i_nserts STRING with a delay between each character.
See `screencast-insert-with-delay' for more details."
                                        ;  (print screencast-nopause-global)
  (screencast-insert-with-delay string screencast-nopause-global))
(defun n (list command-buffer)
  "Screencast producer function. Creates an _n_ew screencast with COMMAND-BUFFER as the command-buffer.
The message-buffer remains the same.  Once the inner screencast
ends, the original comman-buffer regains its status.


IMPORTANT: 

You are responsible for killing the `COMMAND-BUFFER'
before the outermost screencast ends, otherwise you'll receive
the modified buffer the next time you run the outermost
screencast."
  ;; we want to start an 'inner screencast', but the current buffer is the
  ;; command-buffer, and the expected starting buffer is the
  ;; screencast-message-buffer
  (pop-to-buffer (get-buffer screencast-message-buffer-name))
  (screencast-internal list 
                       (get-buffer command-buffer)
                       0
                       screencast-nopause-global)
  )
;;;; END PRODUCER FUNCTIONS
;;;; BEGIN CORE

(defun screencast-fontify-step-region ()
  "Fontifies regions with step-references.
To be called immediatly after functions which put step-numbers
in the message-buffer. Will fontify from the beginning of the
line with the step number to the end of the buffer."
  (save-excursion
    (goto-char (point-max))
    (let ((beg (search-backward-regexp "^Step [[:digit:]]+:" (point-min))))
      (screencast-put-shadow-and-make-clickable beg (point-max))
      )))

(defun screencast-put-shadow-and-make-clickable (beg end)
  "The region between BEG and END becomes shadowed and clickable.
`screencast-goto-step' is evalled when clicked"
  (add-text-properties beg (- end 0)
                       (list
                        'face 'shadow
                        'mouse-face 'highlight 
                        'help-echo "mouse-1: continue from this step"
                        ))
  (make-region-clickable beg (- end 0) 'screencast-goto-step))

(defun screencast-get-step ()
  "Returns the step-number of a step-reference region.
If not in step-reference region, returns nil" 

  (if
      ;; check if we are at a step-reference region
      (save-excursion
        (goto-char (line-end-position))
        (or
         ;; first line
         (search-backward-regexp "^Step [[:digit:]]+:" (line-beginning-position) t)
         ;; second line
         (search-backward-regexp "^  Callable with:" (line-beginning-position) t)))
      ;; get the step number
      (save-excursion
        (search-backward-regexp "^Step \\([[:digit:]]+\\):")
        (let ((beg (match-beginning 1))
              (end (match-end 1)))
          (string-to-number
           (buffer-substring-no-properties  beg end))))
    ;; not in step-reference region
    nil))

(defun screencast-goto-step (&optional arg)
  "Restarts the screencast at the chosen ARG step. Default is the first step."
  (interactive "p")
  (let ((step (if (not (= 1 arg))
                  arg
                (screencast-get-step)))
        ;; bug? using (point), standing at point max gives nil values! 
        (list (get-text-property (point-min) 'screencast-list))
        (name (get-text-property (point-min) 'screencast-command-buffer-name)))
    ;;     (print step)
    ;;     (print list)
    ;;     (print name)
    (screencast list name 
                -1 ; we just ran the screencast, so version should be no problem      
                (if step
                    (- step 1)          ; the command just before!
      
                  0))
    ))

(defun screencast-insert-with-delay (string &optional nopause)
  "Inserts STRING with a delay between each character.
If NOPAUSE is non-nil, the delay will be 0.

The pause between each character is given by `screencast-pause-char-length'."
  (let ((l (string-to-list string)))
    (dolist (c l)
      (insert c)
      ;; simple filling. If the char position equals fill-column. The
      ;; whole word is moved to the next line.
      (when (and (= (- (line-end-position) (line-beginning-position)) fill-column))
        (search-backward " ") 
        (insert "\n ") ; two space indentation as the previous space is moved too
        (end-of-line)  ; ?
        )
      (screencast-pause-maybe nopause screencast-pause-char-length))))

(defun screencast-newline-only-once ()
  "Inserts a newline at point if, and only if the current line is nonempty."
  (unless (= (line-beginning-position) (line-end-position))
    (newline))
  )

(defun screencast-make-region-clickable (beg end action &optional key)
  "Makes the chosen region clickable, executing chosen action.
Default key is [mouse-1]."
  (let ((map (make-sparse-keymap))
        (keyc (if key
                  key
                [mouse-1]))
        )
    (define-key map keyc action)
    (put-text-property 
     beg 
     end
     'keymap map))
  )

(defun screencast-show-command (com step command-buffer)
  "Inserts the STEP number and key-binding for a command, COM."
  (screencast-newline-only-once)
  (insert "Step " (number-to-string step) ": `" (symbol-name com) "'")
  (newline) 
  (insert "  Callable with: ") 
  (insert (where-is-return com command-buffer))
  (screencast-fontify-step-region)
  (newline)
  )

(defun screencast-line (&optional length)
  (let ((l (if length
               length
             25)))
    (screencast-newline-only-once)
    (insert (repeat-string "-" l))
    (center-line)
    (newline)
    ))

(defun screencast-header ()
  (screencast-newline-only-once)
  (newline)
  (screencast-line 50)
  (newline))

(defun repeat-string (s n)
  (apply 'concat (make-list n s)))

(defun screencast-make-break (nopause)
  (screencast-newline-only-once)
  (newline)
  (screencast-line)
  (newline)
  (screencast-pause-maybe nopause)
  (screencast-pause-maybe nopause)
  )

(defun screencast-pause-maybe (nopause &optional length)
  "Pauses the program, unless NOPAUSE is non-nil.
    If length is nil, a default pause LENGTH is used."
  (unless nopause
    (let ((l (if length
                 length
               screencast-pause-length)))
      (sit-for l))))

(defun n-first (n list)
  "The n first elements of a list."
  (loop for x in list repeat n collect x))

(defun buffer-recreate (buffer-name)
  "Kills the buffer with BUFFER-NAME, and recreates it."
  (let ((buffer (get-buffer buffer-name)))
    (when buffer
      (when (buffer-file-name buffer)
        (save-excursion
          (set-buffer buffer)
          (save-buffer)))
    (kill-buffer buffer-name)))
(get-buffer-create buffer-name))

(defun screencast (list command-buffer-name version &optional beginat init)
"Prints and evaluates a list, LIST, of strings and functions in a tempo humans can follow.
The strings in LIST is printed to the screencast-message-buffer.
Functions are evaluated in the buffer named COMMAND-BUFFER-NAME.
VERSION is the version of screencast-mode the screencast is
written for, older versions of screencast-mode might not support
everything in newer screencasts.  
The first BEGINAT elements of the list will be done without
delays.  
INIT is a list of functions to be evaluated in the message-buffer
prior to the first message"
(when (> version screencast-version)
  (error 
   "The version of the screencast is newer than the version of
the screencast-mode itself.  You might still be able to run the
screencast succesfully though, just change the screencasts
version number to try it out."))
;; preparations:
(let* ((message-buffer (buffer-recreate screencast-message-buffer-name))
       (command-buffer (if (string= command-buffer-name 
                                    screencast-message-buffer-name)
                           message-buffer
                         (buffer-recreate command-buffer-name)))
       (screencast-step-number 0))
  (delete-other-windows)
  (split-window-horizontally)
  (switch-to-buffer message-buffer)
  (pop-to-buffer message-buffer)
  (display-buffer command-buffer)
  (screencast-mode)
  (toggle-read-only 0)
  ;; evaluate all the functions of init
  (dolist (f init)
    (eval f))

  ;; show
  (screencast-internal list command-buffer beginat)
  ;; save the arguments in the buffer
  (add-text-properties (point-min) (point-max)
                       (list 'screencast-list list
                             'screencast-command-buffer-name command-buffer-name))
  (toggle-read-only 1)
  )
)

(defun screencast-internal (list command-buffer &optional beginat nopause)
"The internal version of screencast, refer to the documentation string
    there.
    If NOPAUSE is non-nil no delays are used."
(let ((list (if (and beginat 
                     (> beginat 0))   ; if begin at is positive , split the list
                (progn
                  (screencast-internal (n-first beginat list) command-buffer
                                       0 t) ; do the n first steps instantly
                  (nthcdr beginat list)     ; do the rest of the steps
                  )
              list)                     ; else: let the list be as it is
            )
      )
  (setq screencast-nopause-global nopause) ; store the pause globally
  ;; make sure we are visiting the file in case it is needed (e.g. compile!)
  (save-excursion
    (set-buffer command-buffer)
    (unless (buffer-file-name)
      (set-visited-file-name (buffer-name))
      ))
  (dolist (c list)
    (setq screencast-step-number (+ 1 screencast-step-number)) ; inc the step number
    (cond
     ((symbolp c)
      ;; special symbols
      (cond
       ((eq 's c)                       ; step
        (screencast-newline-only-once)
        (insert "Step " (number-to-string screencast-step-number) ":")
        (screencast-fontify-step-region)
        )
       ((eq 'l c)                       ; line
        (screencast-line))
       ((eq 'n c)                       ; newline
        (newline))
       ((eq 'p c)                       ; pause 
        (screencast-pause-maybe nopause))
       ((eq 'b c)                       ; break
        (screencast-make-break nopause)
        )
       (t
        (error (concat "Unknown symbol:" (symbol-name c)))))
      )
     ((listp c)
      ;; function
      (progn
        (unless (member (car c) screencast-dont-print-list) ; these need no print
          (screencast-show-command (car c) screencast-step-number command-buffer)
          )
        (unless nopause     
          (screencast-pause-maybe nopause screencast-pause-command-length) ; pause
          )
        ;;( save excursion style which allows for inner screencasts
        (pop-to-buffer command-buffer)
        (eval c)                        ; evaluate
        (pop-to-buffer screencast-message-buffer-name)
        ;;)
            
        (pop-to-buffer screencast-message-buffer-name) ; needed to regain real focus!
        ))
     ((stringp c)
      ;; it's a string - instert it.
      (screencast-insert-with-delay c nopause))
     (t
      (error (concat "I don't know what to do with element:" c)))
     )
    )
  )
)

(defun where-is-return (definition buffer)
"A modification of where-is, which returns the message-string instead of printing it.
  Also skips the removes name from the output.
  BUFFER is the buffer to call where-is in."
(save-excursion
  (set-buffer buffer)
  (let ((func (indirect-function definition))
        (defs nil)
        (return-string ""))
    ;; In DEFS, find all symbols that are aliases for DEFINITION.
    (mapatoms (lambda (symbol)
                (and (fboundp symbol)
                     (not (eq symbol definition))
                     (eq func (condition-case ()
                                  (indirect-function symbol)
                                (error symbol)))
                     (push symbol defs))))
    ;; Look at all the symbols--first DEFINITION,
    ;; then its aliases.
    (dolist (symbol (cons definition defs))
      (let* ((remapped (command-remapping symbol))
             (keys (where-is-internal
                    symbol overriding-local-map nil nil remapped))
             (keys (mapconcat 'key-description keys ", "))
             string)
        (setq string
              (if t
                  (if (> (length keys) 0)
                      (if remapped
                          (format "%s (%s) (remapped from %s)"
                                  keys remapped symbol)
                        (format "%s" keys))
                    (format "M-x %s RET" symbol))
                (if (> (length keys) 0)
                    (if remapped
                        (format "%s is remapped to %s which is on %s"
                                symbol remapped keys)
                      (format "%s is on %s" symbol keys))
                  ;; If this is the command the user asked about,
                  ;; and it is not on any key, say so.
                  ;; For other symbols, its aliases, say nothing
                  ;; about them unless they are on keys.
                  (if (eq symbol definition)
                      (format "%s is not on any key" symbol)))))
        (when string
          (unless (eq symbol definition)
            (setq return-string (concat return-string ";\n its alias "))) ;
          (setq return-string (concat return-string string)))))
    return-string)))
;;;; END CORE
;;;; BEGIN DOCUMENTATION
(defconst screencast-screencast-text-producer
'(
  "Hello, this is the screencast for creating your own screencasts."
  n
  "If you create a list (first argument) of strings, each string will be typed to the message buffer (this buffer), at a human-readable pace."
  n
  "If you put a 'p in the list, a pause will be inserted. "  p
  p p p p "See?" p p
  l
  "(The above line was inserted instantly with the symbol 'l)"
  n
  "(Blank lines can be inserted using the 'n symbol, removing the need for \\n in strings)"  n
  n
  n
  "All of the above is combined in the symbol 'b, which creates a break in the screencast. This could be used between two different sections for instance."
  b
  "You can also put functions in the list, these will be evaluated in the command-buffer (second argument)."
  n
  "The function is written as a list, with the function name first, and the arguments after that, e.g. '(backward-char 2)."
  n
  n p
  "Each time a function is evaluated, a message is displayed in the message buffer, using the where-is function."
  n
  "In addition to this a step-number is displayed, this step-number corresponds to the functions position in the list."
  n
  "Let's try out some functions:" 
  p
  (insert "THIS IS AN INSERTION\n")
  "The last "
  (i
   "this is also an insertion, but it is done at typing speed")
  "Hmm.." p p "let's delete the line we just typed in the command buffer" 
  (kill-whole-line 1)
  "Notice the keybindings which are displayed."
  b
  "Multiple command-buffers and handled as well:"
  (n 
   '((i "I'm a new command-buffer"))
   "new-command-buffer")
  b
  "If you don't want to document everything you do, for instance moving the cursor, you can put the functions you want to \"hide\" inside a progn."
  n
  "I'll delete the first line now, by using '(progn (previous-line) (kill-whole-line 1))"
  (progn (previous-line) (kill-whole-line 1))
  b
  "The fourth (optional) argument given to the screencast function is the step-number to start using pauses, and output to the message buffer at, e.g. it is a fast-forward."
  n
  "These step-numbers can also be printed separately in the message-buffer using the 's symbol in the list." 
  n
  s
  n 
  "See?"
  b
  "Once you have finished a screencast and want it published, you can record it as a video (.ogv) using `screencast-record'."n
  "As a part of the recording - the font-size (`screencast-record-font') is changed, as well as the fill-column variable (`screencast-record-fill-column') for improved readability on a video."n
  "As a consequence, you should _never_ use fill-paragraph and the like, to get a nicely formatted source-file."n
  "But the Emacs community will benefit the most if you publish the screencast file itself - so please do!"n
  "You can publish it at http://www.emacswiki.org/emacs/ScreencastSources"
  b
  "This screencast should cover most of the options for creating a screencast, and can be seen in the constant \"screencast-screencast-text-producer\". For further customization, you can look at screencast.el in the PRODUCER sections."
  b
  "Happy screencasting!"
  (progn (kill-buffer "new-command-buffer"))
  )
"The text the screencast-screencast-producer is based upon")

(defconst screencast-screencast-text-user 
'(
  "Hello, welcome to the screencast for viewing screencasts in screencast mode."n
  "Screencasts are like movies, they type some explanatory text (like this), and executes functions in order to show you the capabilities of different tools in Emacs."n
  "Once a screencast has finished, you can move the cursor to an executed function and press RET or MOUSE-1 to review the screencast from that step."n
  "Alternatively you can use the numeric prefix argument to pinpoint the step to begin at."n
  "If no prefix argument is given, and point isn't at an executed function, the screencast is restarted from the first step."
  ))

(defun screencast-screencast-producer(&optional arg)
"Displays the screencast for creating screencasts."
(interactive "P")
(apply (if arg
           'screencast-record
         'screencast)
       screencast-screencast-text-producer "screencast-screencast-producer" 1 ()))

(defun screencast-screencast-user(&optional arg)
"Displays the screencast for using screencasts."
(interactive "P")
(apply (if arg
           'screencast-record
         'screencast)
       screencast-screencast-text-user "screencast-screencast-user" 1 ()))
;;;; END DOCUMENTATION
(provide 'screencast)

