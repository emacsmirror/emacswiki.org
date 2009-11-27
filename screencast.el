;;; screencast.el --- demonstrate the capabilities of Emacs

;; Copyright (C) 2009 ESBEN Andreasen <esbenandreasen@gmail.com>

;; Authors: Esben Andreasen <esbenandreasen@gmail.com>

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

;;; Change-log:

;; 1.0: core functionality

;; 1.1: PRODUCER ADDITIONS:
;;      public variables which contain information about the current screencast
;;      ability to change last-command and last-command-char
;;      ability to use let and flet, while still outputting command descriptions
;;      ability to create blinking sections to move the attention towards those
;;	ability to show the region as if transient-mark-mode was on
;;
;;      split the screencasts for the producer into a basic and an advanced
;;	voice synthesizing of typed text. Requires festival to be installed.
;;	global speed control
;;      typed strings in screencasts can not contain tabs and newlines
;;; Code:

(defconst screencast-message-buffer-name "*Screencast Messages*"
  "The name of the buffer to put messages from the screencast in")

(defconst screencast-version 1.1 "The version number of the screencast-mode")

(defconst screencast-speed-relation-speech-type 18.0
  "When this is correctly adjusted, speech and typing should end
at the same time. Lower values means faster speech.")
;;;; BEGIN USER VARIABLES
(defvar screencast-pause-length 2 "The length of a pause ('p) in the screencast")

(defvar screencast-pause-char-length 0.12
  "The time between each typed character in the function `screencast-insert-with-delay'")

(defvar screencast-pause-command-length 3
  "The time between the announcement of the function call, and the call itself.")

(defvar screencast-speech nil "If non-nil, slowly typed strings are read aloud")

(defvar screencast-speed 1.0 "How fast the screencast should be. Higher values equals higher speed. This can not be changed _during_ the screencast.") 
;;;; BEGIN PRODUCER VARIABLES
;; these variables should be changed as needed by the producer

(defvar screencast-dont-print-list '(
                                     progn 
                                      let
                                      flet
                                      save-excursion
                                      save-window-excursion
                                      i
                                      screencast-producer-insert-with-delay
                                      screencast-producer-set-last-command
                                      screencast-producer-set-last-char
                                      screencast-producer-new-buffer
                                      screencast-producer-show-region
                                      screencast-producer-blink-regions)
  "A list of lists of function names which aren't printed as
  being evaluated in the messages, this includes all producer
  functions by default")

(defvar screencast-producer-blink-time 0.5
  "The time a blink lasts.")

;; variables which can be read during run-time to obtain information about the
;; current screencast
(defvar screencast-producer-nopause nil
  "Variable to be used for producer functions if they are using
 pauses, they should deactivate the pause if this variable is non-nil.")

(defvar screencast-producer-command-buffer nil
  "Variable to be used for producer functions if they are using
 need the current command-buffer.")

(defvar screencast-producer-step-number 0
  "Variable to be used for producer functions if they need to
know the current step number. 
This is a _COPY_ of the value the screencast uses!")

(defvar screencast-producer-beginat 0
  "Variable to be used for producer functions if they need to
  know where the screencast is supposed to be using pauses at")
;;;; END PRODUCER VARIABLES


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
(defun screencast-producer-insert-with-delay (string)
  "Screencast producer function. _i_nserts STRING with a delay between each character.
See `screencast-insert-with-delay' for more details."
  (let ((screencast-speech nil))
    (screencast-insert-with-delay string screencast-producer-nopause)))

(defalias 'i 'screencast-producer-insert-with-delay
  "Short name for `screencast-producer-insert-with-delay'.
This is chosen as it improves readability a lot in the screencast-source.")

(defun screencast-producer-set-last-command (f last) 
  "Sets the last-command to LAST before evaluating F.
Also prints the info about F, like it would have done normally." 
  (screencast-producer-show-command (car f))
  (eval-with-last f last)
  )

(defun eval-with-last (f last)
  (eval (list 'progn
              ;; wtf? that's the only way it works (lines can be permuted!)
              '(setq last-command last)
              '(setq this-command last)
              f)))

(defun screencast-producer-set-last-char (char f) 
  "Sets the last-command to CHAR before evaluating F.
Also prints the info about F, like it would have done normally."
  (screencast-producer-show-command (car f))
  (eval (list 'progn 
              '(setq last-command-char (string-to-char char))
              f)))

(defun screencast-producer-show-command (command)
  "Shows the COMMAND, and how it can be called in the message-buffer."
  (pop-to-buffer (get-buffer screencast-message-buffer-name))
  (screencast-show-command command
                           screencast-producer-step-number
                           screencast-producer-command-buffer)
  (pop-to-buffer (get-buffer screencast-producer-command-buffer)))

(defun screencast-producer-new-buffer (list command-buffer-name)
  "Screencast producer function. Creates an new screencast with
 COMMAND-BUFFER-NAME as the command-buffer.  The message-buffer
 remains the same.  Once the inner screencast ends, the original
 command-buffer regains its status.


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
                       (get-buffer command-buffer-name)
                       screencast-producer-beginat)
  )

(defun screencast-producer-show-region (beg end)
  "Marks the currently active region as if transient mark mode was on."
  (unless screencast-producer-nopause
    (let ((overlay (make-overlay beg end)))
      (overlay-put overlay 'face (cons beg end))
      ;; unless there's a LOT of regions, the blinks will be synchronous
      (run-with-timer screencast-pause-length nil 'delete-overlay overlay)
      )
    (sit-for screencast-pause-length)
    )
  )

(defun screencast-producer-blink-regions (regions)
  "The REGIONS will blink.
A region is a pair: (beg . end)."
  (unless screencast-producer-nopause
    (dotimes (n 5)
      (dolist (region regions)
        (let ((overlay (make-overlay (car region) (cdr region))))
          (overlay-put overlay 'face 'region)
          ;; unless there's a LOT of regions, the blinks will be synchronous
          (run-with-timer screencast-producer-blink-time nil 'delete-overlay overlay))
        )
      (sit-for (* 2 screencast-producer-blink-time)) 
      )))

;;;; END PRODUCER FUNCTIONS
;;;; BEGIN CORE
(defun make-region-clickable (beg end action &optional key)
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

(defun screencast-fontify-step-region ()
  "Fontifies regions with step-references.
To be called immediately after functions which put step-numbers
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
          (unless (let ((start (substring-no-properties buffer-name 0 1)))
                    (or (string= start " ") (string= start "*")))
            (save-buffer)))
        (kill-buffer buffer-name)))
    (get-buffer-create buffer-name))
  )
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
                    (- step 1)    ; the command just before!
                  
                  0))
    ))

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

(defun screencast-speech-start (string nopause)
  "Starts the speech-synthesizer with STRING, unless NOPAUSE is nonnil.
Also requires `screencast-speech' to be non-nil.
The speech speed depends on the typing speed (`screencast-speed-relation-speech-type')."
  (when (and (not nopause) screencast-speech)
    (let* ((duration (concat 
                      "-b \"(Parameter.set 'Duration_Stretch " 
                      (number-to-string (* screencast-pause-char-length
                                           screencast-speed-relation-speech-type)) ")\""))
           (tosay (replace-regexp-in-string "'" "'\"'\"'" string))
           (say (concat "-b '(SayText \"" tosay "\")'"))
           )
      (save-window-excursion
        (shell-command
         (concat "festival " duration " " say "&"))
        ))))

(defun screencast-speech-wait-for (nopause)
  "Blocks until the speech synthesizer is done speaking."
  (when (and (not nopause) screencast-speech)
    (shell-command "while [ `pgrep festival` ] ; do sleep 0.1; done;")
    (sit-for 0.1))                      ; needed
  )

(defun screencast-insert-with-delay (string &optional nopause)
  "Inserts STRING with a delay between each character.
If NOPAUSE is non-nil, the delay will be 0.

The pause between each character is given by `screencast-pause-char-length'."
  (let ((string (screencast-strip-newlines-and-normalize-whitespace string)))
    (screencast-speech-start string nopause)
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
        (screencast-pause-maybe nopause screencast-pause-char-length)))
    (screencast-speech-wait-for nopause)
    )
  )

(defun screencast-strip-newlines-and-normalize-whitespace (string)
  "Replaces all newlines and tabs in STRING by a single
whitespace, also collapses multiple whitespaces."
  (replace-regexp-in-string "[ ]+" " " (replace-regexp-in-string "\n" " " string)))

(defalias 'screencast 'screencast-producer-screencast "Renaming for simplicity")

(defun screencast-producer-screencast (list command-buffer-name
                                            version &optional beginat init)
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
     (concat "The version of the screencast (" (number-to-string
                                                version) ") is newer than the version of the screencast-mode
itself (" (number-to-string screencast-version) "). You might still be able
to run the screencast successfully though, just change the
screencasts version number to try it out.")))


  ;; preparations:
  (let* (
         ;; speed adjustments
         (screencast-pause-length (/ screencast-pause-length
                                     screencast-speed))
         (screencast-pause-char-length (/
                                        screencast-pause-char-length
                                        screencast-speed))
         (screencast-pause-command-length (/
                                           screencast-pause-command-length
                                           screencast-speed))
         ;; buffers
         (message-buffer (buffer-recreate screencast-message-buffer-name))
         (command-buffer (if (string= command-buffer-name 
                                      screencast-message-buffer-name)
                             message-buffer
                           (buffer-recreate command-buffer-name)))
         ;; numbers
         (screencast-step-number 0)
         (beginat (if beginat
                      beginat
                    0)))
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

(defun screencast-internal (list command-buffer beginat)
  "The internal version of screencast, refer to the documentation string
    there."
  ;; producer variables
  (setq screencast-producer-command-buffer command-buffer)
  (setq screencast-producer-beginat beginat)
  ;; make sure we are visiting the file in case it is needed (e.g. compile!)
  (save-excursion
    (set-buffer command-buffer)
    (unless (buffer-file-name)
      (set-visited-file-name (buffer-name))
      ))
  ;; for each element in the list
  (dolist (c list)
    (let ((nopause
           (if (>= screencast-step-number beginat)
               nil
             t)))
      ;; producer variables        
      (setq screencast-producer-nopause nopause) 
      (setq screencast-producer-step-number screencast-step-number) 
      (cond
       ((symbolp c)
        ;; special symbols
        (cond
         ((eq 's c)                     ; step
          (screencast-newline-only-once)
          (insert "Step " (number-to-string screencast-step-number) ":")
          (screencast-fontify-step-region)
          )
         ((eq 'l c)                     ; line
          (screencast-line))
         ((eq 'n c)                     ; newline
          (newline))
         ((eq 'p c)                     ; pause 
          (screencast-pause-maybe nopause))
         ((eq 'b c)                     ; break
          (screencast-make-break nopause)
          )
         (t
          (error (concat "Screencast-internal encountered an error: Unknown symbol: " (symbol-name c)))))
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
          (if (member (car c) '(let flet)) 
              (progn
                ;; we want the environment - but also to print the commands! 
                (eval (list (car c)     ; the members above
                            (cadr c)    ;the lets of flets
                            '(screencast-internal (cddr c) command-buffer beginat))) ;the rest
                )
            ;; evaluate standard
            (progn 
              ;; save excursion style which allows for inner screencasts
              (pop-to-buffer command-buffer)
              (eval c)
              (pop-to-buffer screencast-message-buffer-name)))
          (pop-to-buffer screencast-message-buffer-name) ; needed to regain real focus!
          ))
       ((stringp c)
        ;; it's a string - instert it.
        (screencast-insert-with-delay c nopause))
       (t
        (error (concat "I don't know what to do with element:" c)))
       )
      (setq screencast-step-number (+ 1 screencast-step-number)) ; inc the step number
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
    "Hello, this is the screencast for creating your own
    screencasts."  n
    "If you create a list (first argument) of strings, each
    string will be typed to the message buffer (this buffer), at
    a human-readable pace."  n
    "If you put a 'p in the list, a pause will be inserted. "  p
    p p p p "See?" p p
    l
    "(The above line was inserted instantly with the symbol 'l)"
    n
    "(Blank lines can be inserted using the 'n symbol, newlines
    in strings are removed)" n n n
    "All of the above is combined in the symbol 'b, which creates
    a break in the screencast. This could be used between two
    different sections for instance."  b
    "You can also put functions in the list, these will be
    evaluated in the command-buffer (second argument)."  n
    "The function is written as a list, with the function name
    first, and the arguments after that, e.g. '(backward-char
    2)."  n n p
    "Each time a function is evaluated, a message is displayed in
    the message buffer, using the where-is function."  n
    "In addition to this a step-number is displayed, this
    step-number corresponds to the functions position in the
    list."  n
    "Let's try out some functions:" n
    "((insert \"THIS IS AN INSERTION\n\") will be evaluated)" p
    (insert "THIS IS AN INSERTION\n")
    "You can call the special function `screencast-producer-insert-with-delay', aliased to `i' to insert with delay in the command-buffer."n
    (i
     "this is also an insertion, but it is done at typing speed")
    "Hmm.." p p "let's delete the line we just typed in the
    command buffer [[(kill-whole-line 1)]]"
    (kill-whole-line 1)
    "Notice the keybindings which are displayed."  b
    "The fourth (optional) argument given to the screencast
    function is the step-number to start using pauses, and output
    to the message buffer at, e.g. it is a fast-forward. Which is
    _very_ nice when producing a screencast."  n
    "These step-numbers can also be printed separately in the
    message-buffer using the 's symbol in the list."  n s n
    "See?"  b
    "Once you have finished a screencast and want it published,
    you can record it as a video (.ogv) using
    `screencast-record'."n
    "As a part of the recording - the
    font-size (`screencast-record-font') is changed, as well as
    the fill-column variable (`screencast-record-fill-column')
    for improved readability on a video."n
    "As a consequence, you should _never_ use fill-paragraph and
    the like, to get a nicely formatted source-file."n
    "But the Emacs community will benefit the most if you publish
    the screencast file itself - so please do!"n
    "You can publish it at
    http://www.emacswiki.org/emacs/ScreencastSources" b
    "This screencast should cover the basic options for creating
    a screencast, and can be seen in the constant
    `screencast-screencast-text-producer'."n
    "A screencast covering the more advanced functions of
    screencast is available in the function
    `screencast-screencast-producer-advanced'."  b
    "Happy screencasting!"  )
  "The text the screencast-screencast-producer is based upon")

(defconst screencast-screencast-text-user '(
                                            "Hello, welcome to the screencast for viewing screencasts in
    screencast mode."n
    "Screencasts are like movies, they type some explanatory
    text (like this), and executes functions in order to show you
    the capabilities of different tools in Emacs."n
    "Once a screencast has finished, you can move the cursor to
    an executed function and press RET or MOUSE-1 to review the
    screencast from that step."n
    "Alternatively you can use the numeric prefix argument to
    pinpoint the step to begin at."n
    "If no prefix argument is given, and point isn't at an
    executed function, the screencast is restarted from the first
    step."  ))

(defconst screencast-screencast-text-producer-advanced 
  '(
    "This screencast covers the advanced functions of screencast-mode."n
    "Please read the documentation for the functions as well."n
    "Regarding the functions and variables in this file:"n
    "You, as a producer, are supposed to be using the functions starting with `screencast-producer-' (and `screencast' itself ofcourse), they are tailored for ease of use. The others are for internal use - and there's no guarantee they are stable throughout versions."
    b
    "It is possible to use multiple command-buffers:"
    (screencast-producer-new-buffer 
     '((i "I'm a new command-buffer"))
     "new-command-buffer")
    (progn (kill-buffer "new-command-buffer"))
    "It is done via the function `screencast-producer-new-buffer' which takes a list and a buffer - almost like the screencast function itself. "
    b
    "If you don't want to document everything you do, for instance moving the cursor, you can put the functions you want to \"hide\" inside a `progn'."
    b
    "If you need temporary variables or functions (for instance when you need to override a function which uses the mini-buffer), you can just put in a `let' or `flet'"
    b
    "If you need to modify the last-command-char (for self-insert-commands) or the last-command (for continued killing) there's also support for that:"n
    "Use `screencast-producer-set-last-char'  or `screencast-producer-set-last-command'"
    "The text the screencast-screencast-producer-advanced is based upon"))

(defun screencast-screencast-producer-advanced(&optional arg)
  "Displays the screencast for creating advanced screencasts."
  (interactive "P")
  (apply (if arg
             'screencast-record
           'screencast)
         screencast-screencast-text-producer-advanced "screencast-screencast-producer" 1.1 ()))

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

