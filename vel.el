;;; vel.el --- keyboard/mouse-based "self-scroller"
;; Time-stamp: <2003-02-26 09:19:41 deego>
;; Copyright (C) 2002 D. Goel
;; Emacs Lisp Archive entry
;; Filename: vel.el
;; Package: vel
;; Author: D. Goel <deego@glue.umd.edu>
;; Version: 0.9dev
;; Author's homepage: http://deego.gnufans.org/~deego
;; For latest version: 

(defvar vel-home-page
  "http://deego.gnufans.org/~deego/pub/emacspub/lisp-mine/vel/")


 
;; This file is NOT (yet) part of GNU Emacs.
 
;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
 
;; This is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
 

;; See also:
;; Based on ideas obtained from: accel.el 

;; Quick start:
(defvar vel-quick-start
  "Drop this in load-path.  Add to .emacs:
\(require 'vel)
\(setq-default vel-mode t)
 [or (add-hook 'foo-mode-hook 'vel-mode-enable)]
Now just right click (mouse3) whenever you want to scroll (or use
keys like ESC up (M-up) to start, up/down for speed, left-arrow to stop). 

If you do want a general 2-dimensional scrolling including horizontal
scrolling too, do an M-x toggle-truncate-lines and turn off
automatic-hscrolling in your .emacs before loading vel.el. 

That's it...  now customize away if not happy with anything.. and let
me know too.")

(defun vel-quick-start ()
  "Provides electric help regarding variable `vel-quick-start'."
  (interactive)
  (with-electric-help
   '(lambda () (insert vel-quick-start) nil) "*doc*"))

;;; Introduction:
;; Stuff that gets posted to gnu.emacs.sources
;; as introduction
(defvar vel-introduction
  " In this scroll-style, you control the velocity, not scroll-amount.
Press right mouse and see emacs scroll as long as the mouse is
pressed..  Release the mouse when done.  Alt., rightclick to enter the
scroll-phase, and right/left click when done.  Also, keys like ESC-up
and ESC-down (or M-down or M-pgdn) land you into that special
temporary vel-croll phase. Wherein the screen keeps scrolling by
itself and you control the velocity by gently moving the mouse or by
using up/down arrows or pgup/down.  Anything else gets you out of that
mode.  Type M-x vel-quick-start.

I know there are still some weird bugs in the mouse-based vel.el
scrolling, they have stopped showing up for me on my setup.  If you
encounter any, I would really like to know---that is the main
motivation for this release! :) ")

;;;###autoload
(defun vel-introduction ()
  "Provides electric help regarding variable `vel-introduction'."
  (interactive)
  (with-electric-help
   '(lambda () (insert vel-introduction) nil) "*doc*"))

;;; Commentary:
(defvar vel-commentary
  "
There's always atmost one timer and The timer is always temporary.  

Version 0.6.1
=============
Following Doug Gerecht suggestions, introduce variables vel-ver-factor
and vel-hor-factor.  Tweak the defaults a bit. 

NEW IN VERSION 0.6
==================
Mouse-based scrolling---this is much more convenient..

No use of advices any more.

Much cleaner implementation, Smoother scroll, more customizability. 


New in v. 0.3
=============
 Following Christopher Conrad's suggestions, scrolling, by default,
 now happens as if done by hand---viz.  it calls certain pre-command-
 and post-command- hooks after scrolling.  This should enable stuff
 like highlighting to work.
 
 A revamped vel-quick-start and keybindings. 

 The preferred method of working in the minor mode vel-mode is now to
 advise next/previous-line and scroll functions. This enables turning
 on the mode globally without spoiling the arrow keys functionality in
 such modes as minibuffer-modes.  The earlier non-advice method is now
 called vel-vanilla-mode---and it is not wise to turn that minor mode
 on globally.  If you don't like the up/down arrows, please do
 complain to the author.  A few bugfixes: The timer certainly gets
 stopped now once we reach an end/beginning-of-buffer condition.
 Slightly better doc for vel-moment.

 Many thanks to Christoph Conrad and Doug Gerecht for useful
 suggestions.  Thanks to Alex Shroeder and Mario Lang on #emacs for
 help with keybindings.  Suggestions/patches welcome.
 Thanks to J&#65533;r&#65533;me.Bouat@wanadoo.fr <jerome.bouat@wanadoo.fr>@wanadoo.fr
 for suggesting fractional movements, and thanks to Kim F. Storm for
 help in accomplishing it. ")


(defun vel-commentary ()
  "Provides electric help regarding variable `vel-commentary'."
  (interactive)
  (with-electric-help
   '(lambda () (insert vel-commentary) nil) "*doc*"))

;;; History:

;;; Bugs:
;; vertical scrolling is sometimes funky/erratic when horizontal lines
;; are long and folded...  This happens at small vel-velocities in the
;; #emacs channel for me..  I have ONLY observed this bug in erc-mode.
;; Confirmations of the same, and reports for other modes are
;; welcome. 



;;; New features:
(defvar vel-new-features
  " 
New in Version 0.7:
* Several bugfixes.  
* Once emacs is scrolling, right-click exits it too.  Several ways to
  scroll via right-mouse now..") 


(defun vel-new-features ()
  "Provides electric help regarding variable `vel-new-features'."
  (interactive)
  (with-electric-help
   '(lambda () (insert vel-new-features) nil) "*doc*"))

;;; TO DO:
(defvar vel-todo
  "
[1] trace the weird scroll-bug..
[2] when scroll ends due to end of buffer, make sure it ends when we
actually reach the end... for larse scrolls,. that doesn't happen..
"
)

(defun vel-todo ()
  "Provides electric help regarding variable `vel-todo'."
  (interactive)
  (with-electric-help
   '(lambda () (insert vel-todo) nil) "*doc*"))

(defvar vel-version "0.9dev")

;;==========================================
;;; Code:

(eval-when-compile (require 'cl))

(defvar vel-debug-p nil "internal")
(defgroup vel nil 
  "The group vel"
   :group 'applications)
(defcustom vel-before-load-hooks nil "" :group 'vel)
(defcustom vel-after-load-hooks nil "" :group 'vel)
(run-hooks 'vel-before-load-hooks)

(defcustom vel-loops-max 100000
  "Max runs before exiting the scroll-phase.
Meant to prevent emacs from hanging. "
  :type 'number
  :group 'vel)

(defvar vel-loops-current 0)

(defcustom vel-echo-status-p t
  ""
  :type 'boolean
  :group 'vel)
(defcustom vel-verbosity 0
  "suggested: Anywhere from -100 to 100

The design is such that a value of 0 should be optimum. 
viz.: Once you are experienced with this library, you might prefer a value
of 0 for this variable if this is > 0 right now."
  :group 'vel
)

(defcustom vel-pre-scroll-hooks nil 
  "Additional Hooks to run before doing the actual scroll.  " 
  :group 'vel)
(defcustom vel-post-scroll-hooks nil 
  "Additional Hooks to run before doing the actual scroll.  " 
  :group 'vel)
(defcustom vel-call-command-hooks-p nil
  "When non-nil scrolling will be treated as a command.  
Viz: the regular command-hooks will be called before and after each
scroll.  Useful for highlightinf stuff etc." :group 'vel)

(defcustom vel-moment-initial 0.01
  "First scroll occurs after this much time. "
  :group 'vel)



(defcustom vel-key-ver-factor 7
  "Each up/down key is equivalent to these many mouse movements.."
  :type 'integer
  :group 'vel)

(defcustom vel-key-hor-factor 7
  "Each up/down key is equivalent to these many mouse movements.."
  :type 'integer
  :group 'vel)

(defcustom vel-mouse-position-function 
  'mouse-pixel-position
  ""
  :group 'vel)

(defcustom vel-moment 0.05
  "Every scroll occurs this many seconds.  
Changing this variable will NOT affect the speed.  This variable is
just served as a least-count for vel.el. "
  :group 'vel)

(defvar vel-ver-moves 0)
(defvar vel-err-var nil)

(defcustom vel-key-horizontal-p nil
  "Whether left/right arrow keys try horizontal scrolling.  You seldom
want that.  Moreover, leaving it nil means that these keys are (in a
sense) unbound---which means pressing them exits the scroll---very
convenient...")

(defcustom vel-ver-bound '(50 5000)
  "A list of (Upper bound on vel-ver-moves.  Scrolled lines per sec
for that bound).  No need to change this.")

(defcustom vel-hor-bound '(50 5000)
  "A list of (Upper bound on vel-hor-moves.  Scrolled lines per sec for that
bound).  No need to change this.")

(defcustom vel-ver-expt 1.1
  "see vel-coeff-add."
  :group 'vel)

(defcustom vel-ver-mult  2.5
  "See vel-coeff-add."
  :group 'vel)

(defvar vel-ver-enter nil "The position of the mouse when vel entered
this mode.." )

(defcustom vel-ver-factor 0.2
  "See vel-ver-add. "
  :group 'vel)

(defcustom vel-ver-add -3.6
  " The actual scroll-value (downwards) per second, is given by:
 F*(E^N + M*N + A), where N = vel-ver-moves (the number of up or down
pixels that the mouse has moved. 
E = vel-ver-expt, M = vel-ver-mult A = vel-ver-add
F = vel-ver-factor. 

vel-ver-value stores the scroll-value per vel-moment. "
:group 'vel)
 
(defvar vel-ver-value nil)

(defvar vel-ver-accumulation 0)

(defvar vel-hor-moves 0)
(defvar vel-hor-enter nil)
(defcustom vel-hor-expt 2
  "see vel-coeff-add."
  :group 'vel)

(defcustom vel-hor-mult  1
  "See vel-coeff-add."
  :group 'vel)


(defcustom vel-hor-factor 0.2
  "See vel-hor-add"
  :group 'vel)

(defcustom vel-hor-add -2
  "
The actual scroll-value (rightwards) per second is given by:
 F*( E^N + M*N + A),
where N = vel-hor-moves (the number of up or down arrows you have
                        pressed). 
E = vel-hor-expt,
M = vel-hor-mult
A = vel-hor-add.   
F = vel-hor-factor
ver-hor-value stores the scroll-value per
vel-moment. "
:group 'vel)
 
(defvar vel-hor-value nil)
(defvar vel-hor-accumulation 0)

  
(defcustom vel-calculate-scroll-speeds-function 'vel-calculate-scroll-speeds
  "You probably don't need to change this. 
The default should be flexible enough for you, esp. since you can
customize vel-scroll-increment and vel-scroll-multiplier. 
If you still don't like the default, define a function and bind to this
variable. "
  :group 'vel)


(defvar vel-timer nil
  "when non-nil, contains the timer that might get activated when
idle. ") 


(defvar vel-mx-last 0 "internal")
(defvar vel-my-last 0 "internal")

(defmacro vel-ignore-errors (&rest body)
  "Like ignore-errors, but tells the error..
Improved for me by Kalle on 7/3/01:
 * used backquote: something i was too lazy to convert my macro to..
 * removed the progn: condition-case automatically has one..
 * made sure that the return is nil.. just as it is in ignore-errors. 
 Else, returns t. 
"
    `(condition-case vel-err-var (progn ,@body t)
       (error
	;(ding t)
	;(ding t)
	;(ding t)
	(message "%s" (error-message-string vel-err-var))
					;(sit-for 1)
	nil)))


(defcustom vel-scroll-fractional-p nil
  "Whether to attempt fractional scrools for smoother behavior. 
Fractional scrolling does NOT work right now. 
"
  :group 'vel)

(defun vel-scroll-fractional-p-toggle ()
  (interactive)
  (setq vel-scroll-fractional-p (not vel-scroll-fractional-p))
  (message "Symbol %S set to %S" 'vel-scroll-fractional-p 
	   vel-scroll-fractional-p))

(defvar vel-buffer nil "buffer whose Window to scroll. ")
  
;;; ;;;###autoload
;;; (defun vel-utils-current-window ()
;;;   (get-buffer-window (current-buffer)))

(defun vel-buffer-set ()
  (setq vel-buffer (current-buffer)))


(defun vel-scroll-up-fractional (acc)
					;(if (< acc 0)
					;(error "Impossible!"))
  (when (< acc 0)
    (error "Accumulation is negative! not allowed"))
  (progn
    ;; all set-window-scrolls should be preceded by this!
    (sit-for 0)
    (set-window-vscroll 
     (get-buffer-window vel-buffer)
     acc)))

(defvar vel-screenpos 0
  "internal. This shall always record the current screen position"  )

;(defvar vel-screenpos-max 1
;  "should normally be 1.  Please don'T customize this. 
;hmm how do i explain this variable? ")

(defun vel-debug-screenpos-initialize ()
  (interactive)
  (setq vel-screenpos 0))

(defun vel-scroll-up (amount accumulation)
  " accumulation just CANNOT be negative!"
  
  ;;(error "foo")
  (cond
   ((not vel-scroll-fractional-p)
    
    (scroll-up amount)
    (sit-for 0)
    (when vel-scroll-fractional-p
      (vel-scroll-up-fractional
       accumulation)))
   (t 
    (setq vel-screenpos
	  (+ vel-screenpos amount accumulation))
    (when 
	(< vel-screenpos 0)
      (let ((scr (floor vel-screenpos)))
	(scroll-up scr)
	(next-line 1)
	(setq vel-screenpos (- vel-screenpos scr))))
 
    (when
	(> vel-screenpos 1)
      ;; the funny thing is that this scroll-up will not move the
      ;; cursor, much as we expect it to.. because as we go from 0.99
      ;; to 1.01, emacs likes to keep the cursor where it was..
      ;; so we have to explicitly forward-line. 
      (let ((scr (floor vel-screenpos)))
	(scroll-up scr)
	(next-line  1)
	(setq vel-screenpos (- vel-screenpos scr))))

    (vel-scroll-up-fractional vel-screenpos))))
    

      
;; (defvar vel-min-height nil
;;   "should always be a float please."
;;   )
;; (defun vel-min-set ()
;;   (setq vel-min-height (/ 1.0 (frame-char-height))))


;; (defun vel-min-round (num)
;;   "rounds NUM to the best multiple of vel-min-height.. vel-min-height
;; be <= 1 "
;;   (setq num (float num))
;;   (let* ((basic (round num))
;; 	 (rem (- num basic)))
;;     (+ basic
;;        (* (round (/ rem vel-min-height)) vel-min-height))))


(defun vel-calculate-scroll-speeds ()
  "

If the mouse is out of the frame, leaves vel-ver-moves and
vel-hor-moves at their previous values..."

  (let* ((mxy (cdr (funcall vel-mouse-position-function)))
	 (mx (car mxy))
	 (my (cdr mxy)))
    (unless mx (setq mx vel-mx-last))
    (unless my (setq my vel-my-last))
    (setq vel-mx-last mx)
    (setq vel-my-last my)
    
    ;; this should never be possible...
    ;;(unless (numberp vel-ver-enter) (setq vel-ver-enter my))
    ;;(unless (numberp vel-hor-enter) (setq vel-hor-enter mx))
    (setq vel-ver-moves (- my vel-ver-enter))
    (setq vel-hor-moves (- mx vel-hor-enter)))
  (let* ((verabs (abs vel-ver-moves))
	 (sign (vel-sign vel-ver-moves)))
    (setq vel-ver-value
	  (* vel-moment sign
	     (cond
	      ((zerop verabs) 0)
	      ((> verabs (first vel-ver-bound))
	       (vel-message 10 "Thresold exceeded")
	       (second vel-ver-bound))
	      (t 
		  (* vel-ver-factor 
		     (+
		      (expt vel-ver-expt verabs)
		      (* vel-ver-mult verabs)
		      vel-ver-add)))))))
  
  
  ;;(unless (numberp vel-hor-moves) (setq vel-hor-moves 0))
  (let* ((horabs (abs vel-hor-moves))
	 (sign (vel-sign vel-hor-moves)))
    (setq vel-hor-value
	  (* vel-moment sign
	     (cond 
	      ((zerop horabs) 0)
	      ((> horabs (first vel-hor-bound))
	       (vel-message 10 "Threshold exceeded")
	       (second vel-hor-bound))
	      (t (vel-message 10 nil)
		 (* vel-hor-factor
		    (+ (expt vel-hor-expt horabs)
		       (* vel-hor-mult horabs)
		       vel-hor-add)))))))
  )





(defun vel-do-once ()
  (interactive)
  (if (or 
       (and vel-doscroll-mode
	    (<= vel-loops-current vel-loops-max))
       vel-debug-p)
      
      ;; wanna do only if input is not pending.. else just do nothing
      ;; at all...
      (when (or (not (input-pending-p)) vel-debug-p)
	(setq vel-loops-current (+ vel-loops-current 1))
	(vel-message -90 "VEL-DO-ONCE...calculating speeds..")
	(funcall vel-calculate-scroll-speeds-function)
	(vel-message -90 "VEL-DO-ONCE...calculated speeds..")
	(let* ((neth (+ vel-hor-value vel-hor-accumulation))
	       (thish (round neth))
	       (netv (+ vel-ver-value vel-ver-accumulation))
	       (thisv 
		
		;;(if 
		;;vel-scroll-fractional-p
		;;(floor netv)
		(round netv)
		  
		))
		  
		
	  (when vel-echo-status-p
	    (vel-message 10 
			 (format "%s %s"
				 (cond
				  ((plusp vel-ver-moves)  "DOWN: ")
				  ((minusp vel-ver-moves) "  UP: ")
				  (t                      "      "))
				 (if (zerop vel-ver-moves) ""
				   (abs vel-ver-moves)))))
	  (vel-message 
	   -90 
	   "V: %S %S, %S %S %S.\n H: %S %S, %S %S %S.." 
	  	       vel-ver-moves thisv vel-ver-value vel-ver-accumulation
	         netv 
	       vel-hor-moves
	       thish vel-hor-value vel-hor-accumulation neth)
	  (setq vel-hor-accumulation 
		(if (zerop thish) 
		    (- neth thish)
		  0))
	  ;;(when vel-debug-p (read-char))
	  (sit-for 0)
	  ;; if has stopped scrolling, stop attempting..
	  (unless (vel-ignore-errors (scroll-left thish))
	    (vel-message -90 "Resetting hor. scroll.")
	    (setq vel-hor-enter vel-mx-last))
	  (setq vel-ver-accumulation 
		(if (zerop thisv) (- netv thisv) 
		  0))
	  ;; if has stopped scrolling, stop attempting..
	  (unless (vel-ignore-errors 
		   (vel-scroll-up thisv vel-ver-accumulation))
	    (vel-message -80 "Exited at %S" vel-loops-current)
	    (setq vel-ver-enter vel-my-last))
	  (vel-message -90 "V: %S, %S %S %S .\n H: %S %S %S %S..done" 
	  	       thisv vel-ver-moves 
	  	       vel-ver-accumulation
	  	       netv 
	  	       thish
	  	       vel-hor-moves
	  	       vel-hor-accumulation neth)
	  )
	)

    



    ;; primary condition not met..
    
    (progn
      (if (and vel-doscroll-mode 
	       (> vel-loops-current vel-loops-max))
	  (vel-message 10 "Max loops exceeded, exiting vel.."))

      (vel-message -90 "DO-ONCE doscroll unfound...Cancelling vel-timer ")
      (vel-doscroll-exit)
      (vel-message 
       -90 
       "DO-ONCE doscroll unfound...Cancelling vel-timer...done")))
  (vel-message -20 "%S " vel-loops-current)
  )








(defvar vel-with-buffers-var nil)


(defmacro vel-with-buffers (&rest body)
  `(mapcar 
    (lambda (vel-with-buffers-var)
      (with-current-buffer vel-with-buffers-var
	,@body))
    (buffer-list)))


(defun vel-cancel-timer ()
  (when (timerp vel-timer)
    (cancel-timer vel-timer)))

(defun vel-sign (number)
  (cond
   ((minusp number) -1)
   ((plusp number) +1)
   (t 0)))

(defun vel-message (points &rest args)
  (unless (minusp (+ points vel-verbosity))
    (apply #'message args)))


(defun vel-doscroll-enter-mouse3 ()
  (interactive)
  (if vel-doscroll-mode
      (progn
	(vel-message -90 "requesting Initial doscroll-exit...")
	(vel-doscroll-exit))

    (vel-doscroll-enter)))

(defun vel-doscroll-enter ()
  (interactive)
  (setq vel-screenpos 0)
  (vel-buffer-set)
  (vel-cancel-timer)
  (setq vel-hor-accumulation 0)
  (setq vel-ver-accumulation 0)
  (setq vel-loops-current 0)
  (let* ((mxy (cdr (funcall vel-mouse-position-function)))
	 (mx (car mxy))
	 (my (cdr mxy)))
    (setq vel-ver-enter my)
    (setq vel-hor-enter mx))
  (unless (numberp vel-ver-enter)
    (setq vel-ver-enter 0))
  (unless (numberp vel-hor-enter)
    (setq vel-hor-enter 0))
  (setq vel-my-last vel-ver-enter)
  (setq vel-mx-last vel-hor-enter)
  (setq vel-timer 
	(run-at-time vel-moment-initial vel-moment 'vel-do-once))
  (vel-doscroll-mode 1)
  (vel-message -90 "vel-doscroll-mode: %S" vel-doscroll-mode)
  )


(defun vel-doscroll-exit ()
  (interactive)
  (vel-message -90 "VEL-DOSCROLL-EXIT.. cancelling timer, mode..")
  (discard-input)
  (vel-cancel-timer) 
  (setq vel-loops-current 0)
  (vel-doscroll-mode -1)
  (vel-with-buffers (vel-doscroll-mode -1))
  (discard-input)
  (vel-message -90 "VEL-DOSCROLL-EXIT.. cancelling timer, mode..done"))

(defun vel-doscroll-noop ()
  (interactive) 
  nil)


(defun vel-doscroll-enter-and-key-up ()
  (interactive)
  (vel-doscroll-enter)
  (vel-doscroll-key-up))

(defun vel-doscroll-enter-and-key-down ()
  (interactive)
  (vel-doscroll-enter)
  (vel-doscroll-key-down))

(defun vel-doscroll-key-up ()
  (interactive)
  (when (numberp vel-ver-enter)
    (setq vel-ver-enter (+ vel-ver-enter vel-key-ver-factor))
    ;;(vel-message -80 "ver-enter: %S my-last: %S" vel-ver-enter vel-my-last)
    ;;(when (< (abs (- vel-ver-enter vel-my-last)) vel-key-ver-factor)
    ;; (vel-message -80 "setting vel-ver-enter ===vel-key-ver-factor")
    ;;(setq vel-ver-enter vel-my-last))))
    ))

(defun vel-doscroll-key-down  ()
  (interactive)
  (when (numberp vel-ver-enter)
    (setq vel-ver-enter (- vel-ver-enter vel-key-ver-factor))
    ;;(when (< (abs (- vel-ver-enter vel-my-last)) vel-key-ver-factor)
     ;; (setq vel-ver-enter vel-my-last))))
    ))

(defun vel-doscroll-key-right ()
  (interactive)
  (if vel-key-horizontal-p
      (when (numberp vel-hor-enter)
	(setq vel-hor-enter (- vel-hor-enter vel-key-hor-factor))
	;;(when (< (abs (- vel-hor-enter vel-mx-last)) vel-key-hor-factor)
	;;(setq vel-hor-enter vel-mx-last)))
	)
    (vel-message -90 "keyright doscroll-exit")
    (vel-doscroll-exit)))

(defun vel-doscroll-key-left ()
  (interactive)
  (if vel-key-horizontal-p
      (when (numberp vel-hor-enter)
	(setq vel-hor-enter (+ vel-hor-enter vel-key-hor-factor))
	;;(when (< (abs (- vel-hor-enter vel-mx-last)) vel-key-hor-factor)
	;;(setq vel-hor-enter vel-mx-last)))
	)
    (vel-message -90 "keyleft doscroll-exit")

    (vel-doscroll-exit)))

;;;====================================================
;; now: the (visible) minor-mode stuff..
;;;====================================================

;;(defcustom vel-mode-string " v" nil :group 'vel)
(defcustom vel-mode-string "" nil :group 'vel)

(defvar vel-mode-map-default
  '(keymap))

(defcustom vel-mode-map vel-mode-map-default
  "Change this to what yoyu like inn your .emacs"
  :group 'vel)  

(define-key vel-mode-map-default
  (kbd "<down-mouse-3>") 'vel-doscroll-enter-mouse3)
(define-key vel-mode-map-default
  (kbd "<M-down>") 'vel-doscroll-enter-and-key-down)
(define-key vel-mode-map-default
  (kbd "<M-up>") 'vel-doscroll-enter-and-key-up)
(define-key vel-mode-map-default
  (kbd "ESC <down>") 'vel-doscroll-enter-and-key-down)
(define-key vel-mode-map-default
  (kbd "ESC <up>") 'vel-doscroll-enter-and-key-up)
(define-key vel-mode-map-default
  (kbd "ESC <next>") 'vel-doscroll-enter-and-key-down)
(define-key vel-mode-map-default
  (kbd "ESC <prior>") 'vel-doscroll-enter-and-key-up)


(easy-mmode-define-minor-mode
 vel-mode
 "The mode to inherit minibuffer keybindings"
 nil
 vel-mode-string
 ;; 3 means C-c
 ;; 16 means C-p
 'vel-mode-map)

;;;###autoload
(defun vel-mode-disable ()
  (interactive) 
  (vel-mode -1))

;;;###autoload
(defun vel-mode-enable ()
  (interactive) 
  (vel-mode 1))


;;;====================================================
;; now: the (INVISIBLE) doscroll-mode stuff..
;;;==================================================== 
(defcustom vel-doscroll-mode-string " VEL" nil :group 'vel)

(defvar vel-doscroll-mode-map-default
  '(keymap (t  . vel-doscroll-exit-from-key)))

(defcustom vel-doscroll-mode-map vel-doscroll-mode-map-default
  "Change this to what yoyu like in your .emacs"
  :group 'vel)  


(easy-mmode-define-minor-mode
 vel-doscroll-mode
 "The mode to inherit minibuffer keybindings"
 nil
 vel-doscroll-mode-string
 'vel-doscroll-mode-map)







(define-key vel-doscroll-mode-map-default
  (kbd "<mouse-3>") 'vel-doscroll-noop)

(define-key vel-mode-map-default
  (kbd "<mouse-3>") 'vel-doscroll-noop)


(define-key vel-doscroll-mode-map-default
  (kbd "<down-mouse-3>") 'vel-doscroll-exit-from-key)

(define-key vel-doscroll-mode-map-default
  (kbd "<prior>") 'vel-doscroll-key-up)

(define-key vel-doscroll-mode-map-default
  (kbd "ESC <prior>") 'vel-doscroll-key-up)

(define-key vel-doscroll-mode-map-default
  (kbd "<next>") 'vel-doscroll-key-up)

(define-key vel-doscroll-mode-map-default
  (kbd "ESC <next>") 'vel-doscroll-key-up)

(define-key vel-doscroll-mode-map-default
  (kbd "<up>") 'vel-doscroll-key-up)

(define-key vel-doscroll-mode-map-default
  (kbd "ESC O A") 'vel-doscroll-key-up)

(define-key vel-doscroll-mode-map-default 
  [27 79 t] 'vel-doscroll-exit-from-key)
(define-key vel-doscroll-mode-map-default 
  [27 t] 'vel-doscroll-exit-from-key)

(define-key vel-doscroll-mode-map-default
  (kbd "M-<up>") 'vel-doscroll-key-up)

(define-key vel-doscroll-mode-map-default
  (kbd "ESC <up>") 'vel-doscroll-key-up)

(define-key vel-doscroll-mode-map-default
  (kbd "<down>") 'vel-doscroll-key-down)

(define-key vel-doscroll-mode-map-default
  (kbd "ESC O B") 'vel-doscroll-key-down)

(define-key vel-doscroll-mode-map-default
  (kbd "M-<down>") 'vel-doscroll-key-down)

(define-key vel-doscroll-mode-map-default
  (kbd "ESC <down>") 'vel-doscroll-key-down)

(define-key vel-doscroll-mode-map-default
  (kbd "<right>") 'vel-doscroll-key-right)

(define-key vel-doscroll-mode-map-default
  (kbd "<left>") 'vel-doscroll-key-left)

(defun vel-doscroll-exit-from-key ()
  (interactive)
  (vel-message -90 "Key pressed.. doscroll-exiting")
  (vel-doscroll-exit))





(provide 'vel)
(run-hooks 'vel-after-load-hooks)



;;; vel.el ends here
