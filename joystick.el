;;; joystick.el --- Lisp part of joystick interface for Emacs

;; Copyright (C) 2007  John C. G. Sturdy

;; Author: John C. G. Sturdy <john@cb1.com>
;; Keywords: hardware

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This listens (and occasionally talks) to a companion process which
;; is implemented by joylisp.c.

;; It makes the joystick events appear like extra function keys.  A
;; basic set of bindings is included (search this file for "Bindings") .

;; A help function attempts to draw a typical gamepad and label it
;; with the bindings.  On a typical gamepad, this might be the button
;; labelled "10" or "BaseBtn4".

;; More detail:

;; The companion process can be started and stopped by
;; `joystick-start' and `joystick-stop'.  `joystick-start' has the
;; rudiments of multiple joystick support (it lets you select the
;; device, and give it a prefix for the stuff it sends back to Emacs)
;; but doesn't as yet follow through on that -- for example, there is
;; just a plain variable for the joystick process.  But I do intend to
;; do that sometime, unless someone else does it first.

;; The joystick communication process sends s-exps to Emacs, which
;; `eval's them in a process filter.  Mostly, they run a function `jse'
;; (standing for JoyStick Event), which sticks its argument (the
;; event) into the unprocessed event queue.  This makes them look a
;; bit like function keys, and fortunately Emacs doesn't seem to be
;; limited to just things which are defined as function keys; any
;; symbol in the event queue seems to get used that way.  You define
;; them in the same way as function keys.  There's a starter set at
;; the end of this file, designed for use with a "Gamepad"-style
;; controller.

;; Buttons are sent as "-down" and "-up" events, but they also act as
;; modifiers to each other.  If a button has been used as a modifier
;; (i.e. another one was pressed while it was down), it sends a
;; "-release" event instead of "-up".  This way, you can use the same
;; buttons as modifiers and commands, according to whether or not they
;; have actually modified anything by the time they were released.

;; Modifier names are abbreviated versions of the button names
;; (uppercase letters, lowercase letters immediately following
;; uppercase letters, digits are all kept, the rest are dropped).  The
;; modifier names come before the button name, in an attempt to align
;; with the Emacs practice of putting "C-" and "M-C-" etc at the start
;; of key names.

;; For example, you can get event sequences like these:

;;       Trigger-down
;;       Trigger-up

;;       Trigger-down
;;       Tr-TopBtn-down
;;       Tr-TopBtn-up
;;       Trigger-release

;; Note that if you're using "-up" codes for chording, it makes a
;; difference which button you take your finger off last.  This makes
;; for a potentially extremely subtle chording keyboard!

;; Axes are sent as "-next" and "-previous" events, and the rate at
;; which they are sent depends on how far the joystick has been pushed
;; in that direction.  Modifiers also apply to them.

;; There are a few other s-exps that the joystick communication
;; process sends, but these are mostly for information and some are
;; used "internally" by this package.  For example, as it initializes,
;; the joystick communication process reports back what buttons and
;; axes it has; the Lisp code collects those up for use as completion
;; tables.

;; To see for yourself what it sends, run "joylisp" directly from a
;; shell, and watch its output as you press things.  Type "quit" at it
;; to make it go away.

;; I've observed that occasionally my joystick "disappears" from the
;; system, and comes back as "/dev/js1" instead of "/dev/js0".
;; There's some code at the top of `joystick-start' to try to handle
;; things like this.

;; You can also send commands to control the joystick communication
;; process, using `joystick-command'.  This lets you configure the
;; sensitivity of each channel, and various things like that.

;; See the comment at the top of joylisp.c for what the commands are.

;; Some of the Lisp commands in this package (`joystick-set-rate',
;; etc) are packaged versions of `joystick-command'.

;; Things which it might be good to do to this:

;; (1) Complete the multiple joystick support

;; (2) Allow the connection to the joystick communication process to
;;     work over the network, so people can put their joystick on their
;;     X-server rather than having to have it locally on the host on which
;;     their Emacs is running

;;; History:

;; John Sturdy was already interested in non-keyboard Emacs input,
;; particularly for use with the high-level editing package Versor
;; <http://emacs-versor.sourceforge.net/>.

;; Shae Erisson asked about such a thing, on EmacsWiki.

;; John eventually got a joystick (gamepad) and wrote this.

;; The general idea (of receiving s-exps from the process, and sending
;; simple line commands to it) came from Barry Jaspan's vr.el, which
;; listens to a glue program using Dragon's voice input API, and the
;; code started off from fragments of that.  The C code grew from a
;; fragment of Vojtech Pavlik's jstest.c.

;; Initially written 2007-08-29.

;;; Code:

(defgroup joystick nil
  "Parameters for the joystick reader."
  :group 'hardware)

(defcustom joystick-default-device "/dev/js0"
  "The device to use as the joystick."
  :group 'joystick
  :type '(file :must-match t))

(defcustom joystick-log nil
  "*Whether to log the joystick events to the message buffer."
  :group 'joystick
  :type 'boolean)

;;;; Choosing a joystick

(defun joystick-list-joysticks ()
  "Create a list of all joystick devices."
  (mapcar (lambda (j)
	    (cons (file-name-nondirectory j)
		  j))
	  (directory-files "/dev" t "js[0-9]+")))

(defvar joystick-all-joystick-devices
  (joystick-list-joysticks)
  "All devices that appear to be joysticks.
As an alist, for completing.")

;;;; Joystick process handling

(defvar joystick-reading-string nil
  "Storage for partially-read commands from the joylisp subprocess.")

(defun joystick-output-filter (p s)
  "Act on output of joystick interface process P that has sent S."
  ;; based on vr-mode.el
  (unless (stringp s)
    (message "Non-string %S given to joystick-output-filter" s))

  (setq joystick-reading-string (concat joystick-reading-string s))

  ;;(condition-case err
  (while (> (length joystick-reading-string) 0)
    (let* ((parsed (read-from-string joystick-reading-string))
	   (joystick-expr (car parsed))
	   (idx (cdr parsed)))
      (setq joystick-reading-string (substring joystick-reading-string (1+ idx)))
      (when joystick-log
	(message "from joystick: %S" joystick-expr))
      (if (and (consp joystick-expr)
	       (fboundp (car joystick-expr)))
	  (eval joystick-expr)
	(message "Undefined joystick command %S" joystick-expr))))

  ;; sexp isn't complete yet.  Since all commands are currently
  ;; issued in a single write(), this "never" happens.  So for now,
  ;; re-throw the error; if this ever changes, we'll notice. :-)
  ;;('error (error err)))
  )

(defun joystick-sentinel (p s)
  "Sentinel function for joystick process P, getting status S."
  (if (equal s "finished\n")
      (progn
	(setq joystick-process nil))
    (error "Joystick process exited with status \"%s\"" s)))

(defvar joystick-process nil
  ;; Perhaps we should allow many of these, as an alist of (device
  ;; . process)?  Multiple controllers might be useful for pair
  ;; programming, etc.  Maybe do that in the multiple-terminal version
  ;; of Emacs?
  "Process communicating with the joystick.")

(defun joystick-find-working-joystick ()
  "Make sure that `joystick-default-device' is valid."
  (setq joystick-all-joystick-devices (joystick-list-joysticks))
  (let ((all joystick-all-joystick-devices))
    (while (and all
		(not (file-exists-p joystick-default-device)))
      (setq joystick-default-device (cdar all)
	    all (cdr all)))))

(defun joystick-start (&optional device event-identifier other-identifier)
  "Start the joystick process on optional DEVICE.
Optionally give the device an EVENT-IDENTIFIER and
OTHER-IDENTIFIER, in case you have several joystick-type
devices."
  ;; todo: keep an alist of device names to processes, for better
  ;; handling of multiple joysticks
  ;; todo: make this work over network sockets?
  (interactive
   (progn
     ;; first, scan to see which joysticks exist, as they have been
     ;; known to disappear and re-appear under other names
     (joystick-find-working-joystick)
     (if current-prefix-arg
	 (list (completing-read "Joystick: "
				joystick-all-joystick-devices
				nil t)
	       (read-from-minibuffer "Joystick event function prefix: " "jse '")
	       (read-from-minibuffer "Joystick non-event function prefix: " "joystick-"))
       nil)))
  (or device
      (progn
	(joystick-find-working-joystick)
	(setq device joystick-default-device)))
  ;; set the connection type to use a pipe, because otherwise commands
  ;; we send back to the joystick get buffered up until there are huge
  ;; quantities of them

  ;; todo: could someone make a socket not buffer them up, so that we
  ;; can make this work over a network, and then people can attach the
  ;; joystick to their X-server rather than the machine on which
  ;; they're running Emacs?
  (let ((process-connection-type nil))
    (setq joystick-reading-string ""	; get rid of old stuff
	  joystick-process (start-process "joylisp"
					  nil
					  "joylisp"
					  device
					  (or event-identifier
					      "")
					  (or other-identifier
					      ""))))
  ;; I keep suspecting there's a timing hazard here (in Emacs process
  ;; setup generally: what if Emacs gets delayed a bit and the process
  ;; starts producing useful output between `start-process' and
  ;; `set-process-filter'?)  If really worried about it, I'd make
  ;; joylisp.c do nothing until it receives a "start" command, which
  ;; I'd send after I've got the process filter set up.  However, I've
  ;; not seen it give any trouble yet.
  (set-process-filter joystick-process 'joystick-output-filter)
  (set-process-sentinel joystick-process 'joystick-sentinel)
  (set-process-query-on-exit-flag joystick-process nil))

(defun joystick-stop ()
  "Stop the joystick process."
  ;; todo: use the hypothetical alist of joysticks mentioned in the
  ;; todo comment for joystick-start
  (interactive)
  (condition-case evar
      (when (processp joystick-process)
	(joystick-command "quit")
	(sit-for 1)
	;; just in case:
	(kill-process joystick-process))
    (error nil)))

(defun joystick-command (command)
  "Send COMMAND to the joystick interface program."
  ;; todo: use the hypothetical alist of joysticks mentioned in the
  ;; todo comment for joystick-start; this also applies to
  ;; joystick-set-rate, joystick-set-sensitivity, joystick-hat-speed,
  ;; joystick-rumble, joystick-shock and anything else like that.
  (interactive "sJoystick command: ")
  (if (processp joystick-process)
      ;; todo: these don't seem to be getting through, unless they
      ;; fill the buffer -- how can we flush them?
      (process-send-string joystick-process command)
    (error "No joystick process")))

;;;; Specific commands to the joystick

(defvar joystick-latest-speed nil
  "The last known speed of the joystick.")

(defun joystick-set-rate (rate)
  "Set the joystick rate to RATE ticks per second."
  (interactive "nRate: ")
  (joystick-command (format "tickrate %f\n" rate))
  (setq joystick-latest-speed rate))

(defun joystick-get-speed ()
  "Get the current speed of the joystick."
  (when (null joystick-latest-speed)
    (joystick-command "tickrate")
    (while (null joystick-latest-speed)
      ;; The joystick communication process should now send back a
      ;; joystick-current-tick-rate s-exp; wait for it to come
      ;; through.  It is executed as an expression, so we don't have
      ;; to actually do anything here.
      (sit-for 1))
    joystick-latest-speed))

(defun joystick-current-tick-rate (rate)
  "Set the latest known speed to RATE.
This is a command sent back by the joystick communication
process, in response to a \"tickrate\" command with no argument."
  (setq joystick-latest-speed rate))

(defun joystick-faster ()
  "Make the joystick go a bit faster."
  (joystick-set-rate (* 1.25 (joystick-get-speed))))

(defun joystick-slower ()
  "Make the joystick go a bit slower."
  (joystick-set-rate (* 0.8 (joystick-get-speed))))

(defun joystick-set-sensitivity (sensitivity &optional axis)
  "Set the joystick SENSITIVITY, either for all axes, or for optional AXIS."
  (interactive
   (list (joystick-read-axis-name "Axis: " t)))
  (if (and axis (not (string= axis "all")))
      (joystick-command (format "sensitivity %s %d" axis sensitivity))
    (joystick-command (format "sensitivity %d" sensitivity))))

(defun joystick-hat-speed (hat-speed &optional axis)
  "Set the joystick HAT-SPEED, either for all hat axes, or for optional AXIS."
  (interactive
   (list (joystick-read-axis-name "Axis: " t)))
  (if (and axis (not (string= axis "all")))
      (joystick-command (format "sensitivity %s %d" axis hat-speed))
    (joystick-command (format "sensitivity %d" hat-speed))))

(defun joystick-rumble ()
  "Make the joystick rumble, if it supports it."
  (interactive)
  (joystick-command "rumble\n"))

(defun joystick-shock ()
  "Make the joystick shock, if it supports it."
  (interactive)
  (joystick-command "shock\n"))

;; todo: the rest of the joylisp commands

;;;; Getting data back from the joystick, including setup

(defvar joystick-buttons nil
  "A-list of buttons names.
Filled in as the joystick communication process starts.")

(defvar joystick-button-regexps nil
  "List of regexps matching joystick buttons.
Filled in as the joystick communication process starts.")

(defvar joystick-axes nil
  "A-list of axis names.
Filled in as the joystick communication process starts.")

(defvar joystick-axis-regexps nil
  "List of regexps matching joystick axes.
Filled in as the joystick communication process starts.")

(defun joystick-declare-version (n)
  "Accept a declaration that the version is N.")

(defun joystick-declare-buttons (n)
  "Accept that the button count is N.")

(defun joystick-declare-axes (n)
  "Accept that the axis count is N.")

(defun joystick-declare-name (n)
  "Accept a declaration that the name is N.")

(defun joystick-declare-button (n name)
  "Declare that button N is called NAME.
This is a command sent back to Emacs from the joystick communication process."
  ;; todo: use the hypothetical alist of joysticks mentioned in the todo comment for joystick-start
  (push (cons name n)
	joystick-buttons)
  ;; Store a regexp for modifying the help diagram
  (push (cons (format "\\<%s *:" name)
	      (vector (intern (concat (symbol-name name) "-up"))))
	joystick-button-regexps))

(defun joystick-declare-axis (n name)
  "Declare that axis N is called NAME.
This is a command sent back to Emacs from the joystick communication process."
  ;; todo: use the hypothetical alist of joysticks mentioned in the todo comment for joystick-start
  (push (cons name n)
	joystick-axes)
  ;; Store a couple of regexps for modifying the help diagram
  (let ((name-string (symbol-name name)))
    (push (cons (format "\\<%s-p *:" name-string)
		(vector (intern (concat (symbol-name name) "-previous"))))
	  joystick-axis-regexps)
    (push (cons (format "\\<%s-n *:" name-string)
		(vector (intern (concat (symbol-name name) "-next"))))
	  joystick-axis-regexps)))

(defun joystick-read-button-name (prompt &optional allow-all)
  "Read a button name, using PROMPT.
Optionally with ALLOW-ALL, allow a completion \"all\"."
  ;; todo: use the hypothetical alist of joysticks mentioned in the todo comment for joystick-start
  (completing-read prompt
		   (if allow-all
		       (cons (cons "all" nil)
			     joystick-buttons)
		        joystick-buttons)
		   nil
		   t))

(defun joystick-read-axis-name (prompt &optional allow-all)
  "Read an axis name, using PROMPT.
Optionally with ALLOW-ALL, allow a completion \"all\"."
  ;; todo: use the hypothetical alist of joysticks mentioned in the todo comment for joystick-start
  (completing-read prompt
		   (if allow-all
		       (cons (cons "all" nil)
			     joystick-axes)
		     joystick-axes)
		   nil
		   t))

;;;; Process the joystick events

(defun jse (event-type &rest event-args)
  "Process a joystick event, with EVENT-TYPE and perhaps some EVENT-ARGS."
  ;; todo: pass args if given
  (if (key-binding (vector event-type))
      (setq unread-command-events
	    (nreverse (cons event-type
			    (nreverse unread-command-events))))
    (when joystick-log
      (message "%S not bound" event-type))))

;;;; Help on button assignments

(defun joystick-diagram-replace-label (sub-match key-sequence binding)
  "Replace SUB-MATCH of match data with help for KEY-SEQUENCE to BINDING.

The help can be a property of the symbol -- the major mode name
is tried -- should be safe as these symbols are probably not
greatly used other than for joystick -- or 'joystick-help.
Otherwise the binding symbol name is used."
  (let* ((available-length (- (match-end sub-match)
			      (match-beginning sub-match)))
	 (symbol (aref key-sequence 0))
	 (new-label (or (get symbol major-mode)
			(get symbol 'joystick-help)
			(get binding 'joystick-help)
			(symbol-name binding)))
	 (new-length (length new-label))
	 (trimmed-label (if (< new-length available-length)
			    (concat new-label
				    (make-string (- available-length new-length)
						 ?\ ))
			  (substring new-label 0 available-length))))
    (replace-match trimmed-label t t nil sub-match)))

(defun joystick-help ()
  "Display a diagram of the joystick button assignments.

If setting up your own joystick bindings, to get a command
labelled with something short enough to fit, give the command
symbol a 'joystick-help property of the string you want."
  (interactive)
  (let ((joystick-buffer "*Joystick*"))
    (save-window-excursion
      (save-excursion
	(condition-case evar
	    (progn
	      (with-output-to-temp-buffer joystick-buffer
		;; If re-drawing the diagram, note that the text
		;; replaced by a label runs from the button name up to
		;; its trailing ":" character.
		(princ "
     /----------------------\\           /----------------------\\   front
    /  PinkBtn        :      \\---------/   BaseBtn2         :   \\   of
    |  TopBtn2        :                     BaseBtn         :   |   pad
    +-----------------------------------------------------------+
    |    Hat0Y-p   :                              ThumbBtn   :  |   top
    | Hat0X-p : Hat0X-n : BaseBtn3 : BaseBtn : Trigger : TopBtn:|   of
    |    Hat0Y-n:+-----------+         +-----------+ ThumbBtn2: |   pad
    +------------|    Y-p  : |---------|    Z-p  : |------------+
       /         | X-p : X-n:|         | Rz-p:Rz-n:|      \\
      /          |    Y-n   :|         |    Z-n   :|       \\
     /           +-----------+         +-----------+        \\
    /             /                           \\              \\
   /_____________/                             \\______________\\
"))
	      (with-current-buffer joystick-buffer
		(let ((buffer-read-only nil))
		  (dolist (label-list (list joystick-button-regexps
					    joystick-axis-regexps))
		    (dolist (button label-list)
		      (goto-char (point-min))
		      (let ((binding (key-binding (cdr button))))
			(when (and binding
				   (re-search-forward (car button) (point-max) t))
			  (joystick-diagram-replace-label 0 (cdr button) binding)))))))
	      (read-event))
	  (error
	   (message "help error: %S" evar)
	   nil))))))



;;;; Bindings

;; first, ignore the initialization events

;; character/line movements
(global-set-key [ Hat0X-previous ] 'backward-char)
(global-set-key [ Hat0X-next ] 'forward-char)
(global-set-key [ Hat0Y-previous ] 'previous-line)
(global-set-key [ Hat0Y-next ] 'next-line)

(global-set-key [ X-previous ] 'backward-char)
(global-set-key [ X-next ] 'forward-char)
(global-set-key [ Y-previous ] 'previous-line)
(global-set-key [ Y-next ] 'next-line)

(put 'backward-char 'joystick-help "char--")
(put 'forward-char 'joystick-help "char++")
(put 'previous-line 'joystick-help "line--")
(put 'next-line 'joystick-help "line++")

;; some simple editing

;;     /----------------------\           /----------------------\   front
;;    /  next-buffer/bufmenu   \---------/             sexps      \   of
;;    |  other-window                               dabbrev/words |   pad
;;    +-----------------------------------------------------------+
;;    |       up                                        kill      |   top
;;    |  left    right    speed                     save    yank  |   of
;;    |      down  +-----------+         +-----------+  find      |   pad
;;    +------------|           |---------|           |------------+
;;       /         |   move    |	 |	     |	    \
;;      /	   |	       |	 |	     |	     \
;;     /	   +-----------+  	 +-----------+	      \
;;    /		    /				\ 	       \
;;   /_____________/				 \______________\

;; The buttons marked "sexps" and "words" modify the movements made by
;; the hat switch.  "words", when released without having been used as
;; a modifier, does dabbrev expansion.

;; The buttons marked "save" and "yank" also do `exit-minibuffer' and
;; `minibuffer-complete', respectively, when in the minibuffer.

(global-set-key [ Trigger-up ] 'kill-ring-save)
(global-set-key [ ThumbBtn-up ] 'kill-region)
(global-set-key [ TopBtn-up ] 'yank)
(global-set-key [ ThumbBtn2-up ] 'find-tag)
(define-key emacs-lisp-mode-map [ ThumbBtn2-up ] 'find-function)
(define-key lisp-interaction-mode-map [ ThumbBtn2-up ] 'find-function)
(global-set-key [ BaseBtn-up ] 'dabbrev-expand)
(global-set-key [ TopBtn-upBtn2-up ] 'other-window)

(put 'kill-ring-save 'joystick-help "copy")
(put 'kill-region 'joystick-help "kill")
(put 'find-tag 'joystick-help "find")
(put 'find-function 'joystick-help "find")
(put 'dabbrev-expand 'joystick-help "dabbrev")
(put 'other-window 'joystick-help "otherwin")

;; Use PinkieBtn as a modifier or a command, for buffer selection:
(require 'ebuff-menu)
;; Note: the menu appears when you press PinkieBtn AND Hat-Downwards,
;; not immediately on PinkieBtn
(global-set-key [ PiBt-Hat0Y-next ] 'electric-buffer-list)
(define-key electric-buffer-menu-mode-map [ PiBt-Hat0Y-next ] 'next-line)
(define-key electric-buffer-menu-mode-map [ PiBt-Hat0Y-previous ] 'previous-line)
(define-key electric-buffer-menu-mode-map [ ToBt2-PinkieBtn-up ] 'Buffer-menu-mark)
(define-key electric-buffer-menu-mode-map [ PinkieBtn-release ] 'Electric-buffer-menu-select)
;; PinkieBtn by itself (without Hat) just goes to the next buffer
(global-set-key [ PinkieBtn-up ] 'next-buffer)

;; use the upper right button for "words/sentences"
(global-set-key [ BaBt-Hat0X-previous ] 'backward-word)
(global-set-key [ BaBt-Hat0X-next ] 'forward-word)
(global-set-key [ BaBt-Hat0Y-previous ] 'backward-sentence)
(global-set-key [ BaBt-Hat0Y-next ] 'forward-sentence)
(global-set-key [ BaBt-ThumbBtn2-up ] 'isearch-yank-word)
(global-set-key [ BaBt-ThumbBtn-up ] 'kill-word)

;; use the lower right button for expressions
(global-set-key [ BaBt2-Hat0X-previous ] 'backward-sexp)
(global-set-key [ BaBt2-Hat0X-next ] 'forward-sexp)
(global-set-key [ BaBt2-Hat0Y-previous ] 'backward-up-list)
(global-set-key [ BaBt2-Hat0Y-next ] 'down-list)
(global-set-key [ BaBt2-ThumbBtn-up ] 'kill-sexp)

(mapcar (lambda (map)
	  (define-key map [ Trigger-up ] 'exit-minibuffer)
	  (define-key map [ TopBtn-up ] 'minibuffer-complete)
	  )
	(list minibuffer-local-map
	      minibuffer-local-must-match-filename-map
	      minibuffer-local-filename-completion-map
	      minibuffer-local-completion-map
	      minibuffer-local-must-match-map
	      minibuffer-local-ns-map))

(global-set-key [ BaBt3-Hat0Y-previous ] 'joystick-faster)
(global-set-key [ BaBt3-Hat0Y-next ] 'joystick-slower)

(global-set-key [ BaseBtn4-down ] 'joystick-help)
(global-set-key [ BaseBtn4-up ] 'recenter)

(provide 'joystick)

;;; joystick.el ends here
