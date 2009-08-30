;;; xt-mouse-xmas.el --- Xemacs port of xt-mouse.el - Support the mouse when emacs run in an xterm.

;; Copyright (C) 1994 Free Software Foundation

;; Author: Per Abrahamsen <abraham@dina.kvl.dk>
;; Keywords: mouse, terminals

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Comments:

;; Enable mouse support when running inside an xterm or Linux console.

;; This is actually useful when you are running X11 locally, but is
;; working on remote machine over a modem line or through a gateway.

;; It works by translating xterm escape codes into generic emacs mouse
;; events so it should work with any package that uses the mouse.

;; The xterm mouse escape codes are supposedly also supported by the
;; Linux console, but I have not been able to verify this.

;; You don't have to turn off xterm mode to use the normal xterm mouse
;; functionality, it is still available by holding down the SHIFT key
;; when you press the mouse button.

;;; Todo:

;; Support multi-click -- somehow.

;; Clicking on the mode-line does not work, although it should.


;;; Xemacs comments:

;;  File is slightly updated Bill Perry xt-mouse port from
;;  http://list-archive.xemacs.org/xemacs-beta/199911/msg00195.html
;;  Added optional mouse-wheel, modeline drag and control modifier support. 
;;  Checked on xterm/gnome-terminal with xemacs-21.4.16
;;  To switch on use:
;;  (xterm-mouse-mode 1)

;;  Problems: 
;;  First problem of this implementation is that in xemacs
;;  you cannot(have no idea why) add motion event to unread-command-events.
;;  As far as i inderstand there is no such limitation on gnu emacs.
;;  Second problem is that some xemacs built-in functions used 
;;  to implement mouse/selection return nil on tty. E.g. 
;;  event-over-*, console-on-window-system-p, selection-owner-p.
;;  
;;  Todo:
;;  Fix multi-click, switching to minibuffer, find way to drag
;;  vertical divider ...


;;; Code: 

(defvar xt-drag-modeline t
  "Use drag modeline hack")

(defvar xt-mwheel t
  "Use mwheel hack")

(define-key function-key-map "\e[M" 'xterm-mouse-translate)

(defvar xterm-mouse-last)

(defun xterm-mouse-translate (event)
  ;; Read a click and release event from XTerm.
  (save-excursion
    (save-window-excursion
      ;;(deactivate-mark) ; Emacs-ism
      (let* ((xterm-mouse-last)
             (down (xterm-mouse-event))
	     (down-where (cons (event-x down) (event-y down)))
             (down-binding (key-binding down))
	     (is-click (eq (event-type down) 'button-release)))
	(unless is-click
	  (or (and (eq (read-char) ?\e)
		   (eq (read-char) ?\[)
		   (eq (read-char) ?M))
	      (error "Unexpected escape sequence from XTerm")))
        (let* ((click (if is-click down (xterm-mouse-event)))
               (click-where (cons (event-x click) (event-y click))))
          (if (memq down-binding '(nil ignore))
              (if (and (symbolp click-where)
                       (not (eq 'menu-bar click-where)))
                  (error "Not sure what to do here") ;            (vector (list click-where click-data) click)
                click)
            (setq unread-command-events
                  (if unread-command-events
                      (append unread-command-events click)
                    (list click)))))
        (vector down)))))

(defvar xterm-mouse-x 0
  "Position of last xterm mouse event relative to the frame.")

(defvar xterm-mouse-y 0
  "Position of last xterm mouse event relative to the frame.")

;; Indicator for the xterm-mouse mode.
(defvar xterm-mouse-mode nil)

(defadvice mouse-position (around xterm-mouse activate)
  "Use last key from xterm-mouse-mode if available."
  (let ((answer ad-do-it))
    (setq ad-return-value
          (if xterm-mouse-mode
              (cons (car answer) (cons xterm-mouse-x xterm-mouse-y))
            answer))))

(if (not (fboundp 'window-at))
    (defun window-at (x y &optional frame)
      (let ((windows (window-list frame t))
            (edges nil)
            (found nil))
        (while windows
          (setq edges (window-pixel-edges (car windows)))
          (if (and (>= x (nth 0 edges))
                   (<= x (nth 2 edges))
                   (>= y (nth 1 edges))
                   (<= y (nth 3 edges)))
              (setq found (car windows)
                    windows nil))
          (setq windows (cdr windows)))
        found)))


(if (not (fboundp 'coordinates-in-window-p))
    (defun coordinates-in-window-p (coordinates window)
      (let ((edges (window-pixel-edges window))
            (x (car coordinates))
            (y (cdr coordinates)))
        (if (and (>= x (nth 0 edges))
                 (<= x (nth 2 edges))
                 (>= y (nth 1 edges))
                 (<= y (nth 3 edges)))
            (cons (- x (nth 0 edges))
                  (- y (nth 1 edges)))))))


;; read xterm sequences above ascii 127 (#x7f)
(defun xterm-mouse-event-read ()
  (let ((c (read-char)))
    (if (< c 0)
        (+ c #x8000000 128)
      c)))

(defun xterm-mouse-event ()
  ;; Convert XTerm mouse event to Emacs mouse event.
  (let* (
	 ;;(type (- (xterm-mouse-event-read) #o40))
	 ;;(x (- (xterm-mouse-event-read) #o40 1))
	 ;;(y (- (xterm-mouse-event-read) #o40 1))
	 (type (- (read-char) ? ))
         (x (- (read-char) ?  1))
         (y (- (read-char) ?  1))
         (point (cons x y))
         (window (window-at x y))
         (where (if window 
                    (coordinates-in-window-p point window)
                  'menu-bar))
         (pos (if (consp where)
                  (progn
		    (select-window window)
		    (goto-char (window-start window))
		    (move-to-window-line  (cdr where))
		    (move-to-column (+ (car where) (current-column)
				       (max 0 (1- (window-hscroll)))))
		    (point))
                where))
	 (modifiers 
	  (if (and (>= type 16)
		   (<= type 19))
	      t nil))
         (mouse-button 
	  (progn 
	    ;;(message "type %d" type)
	    (cond ((>= type 64)
		   (setq xterm-mouse-last (- type 61))
		   (- type  60))
		  ((or (eq type 16)
		       (eq type 17)
		       (eq type 18))
		   (setq xterm-mouse-last (- type 16))
		   (- type  15))
		  ((or (eq type 3)
		       (eq type 19))
		   (+ 1 xterm-mouse-last))
		  (t 
		   (setq xterm-mouse-last type)
		   (+ 1 type)))))
	 (event-type (if (or (eq type 3) (eq type 19))
			 'button-release
                       'button-press)))
    (setq xterm-mouse-x x
          xterm-mouse-y y)
    (if modifiers
	(allocate-event event-type (list 'channel (window-frame window)
                                     'button mouse-button
				     'modifiers (list 'control)
                                     'x x
				     'timestamp (current-event-timestamp)
                                     'y (1+ y)))
      (allocate-event event-type (list 'channel (window-frame window)
				       'button mouse-button
				       'x x
				       'timestamp (current-event-timestamp)
				       'y (1+ y))))))
    
;;; ???
;;(defun console-on-window-system-p (&optional CONSOLE)
;;  t)

;;;###autoload
(defun xterm-mouse-mode (arg)
  "Toggle XTerm mouse mode.
With prefix arg, turn XTerm mouse mode on iff arg is positive.

Turn it on to use emacs mouse commands, and off to use xterm mouse commands."
  (interactive "P")
  (if (or (and (null arg) xterm-mouse-mode)
          (<= (prefix-numeric-value arg) 0))
      ;; Turn it off
      (if xterm-mouse-mode
          (progn
	    (when xt-drag-modeline
	      (fset 'mouse-drag-modeline 'orig-mouse-drag-modeline))
            (turn-off-xterm-mouse-tracking)
            (setq xterm-mouse-mode nil)
            (set-buffer-modified-p (buffer-modified-p))))
    ;;Turn it on
    (if xterm-mouse-mode
        ()
      (when xt-drag-modeline
	(fset 'mouse-drag-modeline 'xt-mouse-drag-modeline))
      (when xt-mwheel
	(global-set-key 'button5 'xt-mousewheel-scroll-up)
	(global-set-key 'button4 'xt-mousewheel-scroll-down))
      (setq xterm-mouse-mode t)
      (turn-on-xterm-mouse-tracking)
      (set-buffer-modified-p (buffer-modified-p)))))

(defun turn-on-xterm-mouse-tracking ()
  ;; Enable emacs mouse tracking in xterm.
  (if xterm-mouse-mode
      (send-string-to-terminal "\e[?1000h" nil (selected-device))))

(defun turn-off-xterm-mouse-tracking ()
  ;; Disable disable emacs mouse tracking in xterm.
  (if xterm-mouse-mode
      (send-string-to-terminal "\e[?1000l" nil (selected-device))))

;; Restore normal mouse behaviour outside Emacs.
(add-hook 'suspend-hook 'turn-off-xterm-mouse-tracking)
(add-hook 'suspend-resume-hook 'turn-on-xterm-mouse-tracking)
(add-hook 'kill-emacs-hook 'turn-off-xterm-mouse-tracking)

(add-minor-mode 'xterm-mouse-mode " Mouse"  nil nil 'xterm-mouse-mode)


;; primitive mouse-wheel support 

(defun xt-mousewheel-scroll-up (event)
  "Scroll window under mouse up by five lines."
  (interactive "e")
  (let ((current-window (selected-window)))
    (unwind-protect
	(progn 
	  (select-window (event-window event))
	  (scroll-up 5))
      (select-window current-window))))

(defun xt-mousewheel-scroll-down (event)
  "Scroll window under mouse down by five lines."
  (interactive "e")
  (let ((current-window (selected-window)))
    (unwind-protect
	(progn 
	  (select-window (event-window event))
	  (scroll-down 5))
      (select-window current-window))))



;; drag modeline hack

(defun xt-mouse-drag-modeline (event)
  "Resize a window by dragging its modeline.
This command should be bound to a button-press event in modeline-map.
Holding down a mouse button and moving the mouse up and down will
make the clicked-on window taller or shorter.

See also the variable `modeline-scrolling-method'."
  (interactive "e")
  (or (button-press-event-p event)
      (error "%s must be invoked by a mouse-press" this-command))
  ;;(or (event-over-modeline-p event)
  ;;    (error "not over a modeline"))
  ;; Give the modeline a "pressed" look.  --hniksic
  ;;(let-specifier ((modeline-shadow-thickness
;;		   (- (specifier-instance modeline-shadow-thickness
;;					  (event-window event)))
;;		   (event-window event)))
  (let ((done nil)
	(depress-line (event-y event))
	(start-event-frame (event-frame event))
	(start-event-window (event-window event))
	(start-nwindows (count-windows t))
	(hscroll-delta (face-width 'modeline))
	(start-hscroll (modeline-hscroll (event-window event)))
	(start-x-pixel (event-x-pixel event))
	(start-y-pixel (event-y-pixel event))
	(last-timestamp 0)
	default-line-height
	modeline-height
	should-enlarge-minibuffer
	event min-height minibuffer y top bot edges wconfig growth)
    (setq minibuffer (minibuffer-window start-event-frame)
	  default-line-height (face-height 'default start-event-window)
	  min-height (+ (* window-min-height default-line-height)
			;; Don't let the window shrink by a
			;; non-multiple of the default line
			;; height.  (enlarge-window -1) will do
			;; this if the difference between the
			;; current window height and the minimum
			;; window height is less than the height
			;; of the default font.  These extra
			;; lost pixels of height don't come back
			;; if you grow the window again.  This
			;; can make it impossible to drag back
			;; to the exact original size, which is
			;; disconcerting.
			(% (window-pixel-height start-event-window)
			   default-line-height))
	  modeline-height
	  (if (specifier-instance has-modeline-p start-event-window)
	      (+ (face-height 'modeline start-event-window)
		 (* 2 (specifier-instance modeline-shadow-thickness
					  start-event-window)))
	    (* 2 (specifier-instance modeline-shadow-thickness
				     start-event-window))))
    (if (not (eq (window-frame minibuffer) start-event-frame))
	(setq minibuffer nil))
    (if (and (null minibuffer) (one-window-p t))
	(error "Attempt to resize sole window"))
    ;; if this is the bottommost ordinary window, then to
    ;; move its modeline the minibuffer must be enlarged.
    (setq should-enlarge-minibuffer
	  (and minibuffer (window-lowest-p start-event-window)))
    ;; loop reading events
    (while (not done)
      (setq event (next-event event))
      ;; requeue event and quit if this is a misc-user, eval or
      ;;   keypress event.
      ;; quit if this is a button press or release event, or if the event
      ;;   occurred in some other frame.
      ;; drag if this is a mouse motion event and the time
      ;;   between this event and the last event is greater than
      ;;   drag-divider-event-lag.
      ;; do nothing if this is any other kind of event.
      (cond ((or (misc-user-event-p event)
		 (key-press-event-p event))
	     (setq unread-command-events (nconc unread-command-events
						(list event))
		   done t))
	      ;;;((button-release-event-p event)
	      ;;; (setq done t)
	      ;;; ;; Consider we have a mouse click neither X pos (modeline
	      ;;; ;; scroll) nore Y pos (modeline drag) have changed.
	      ;;; (and modeline-click-swaps-buffers
	      ;;;      (= depress-line (event-y event))
	      ;;;      (or (not modeline-scrolling-method)
	      ;;;  	(= start-hscroll
	      ;;;  	   (modeline-hscroll start-event-window)))
	      ;;;      (modeline-swap-buffers event)))
	      ;;;((button-event-p event)
	      ;;; (setq done t))
	      ;;;((not (motion-event-p event))
	      ;;; (dispatch-event event))
	      ;;;((not (eq start-event-frame (event-frame event)))
	      ;;; (setq done t))
	      ;;;((< (abs (- (event-timestamp event) last-timestamp))
	      ;;;    drag-divider-event-lag)
	      ;;; nil)
	      ;;;(t
	    ((button-release-event-p event)
	     (when modeline-scrolling-method
	       (let ((delta (/ (- (event-x-pixel event) start-x-pixel)
			       hscroll-delta)))
		 (set-modeline-hscroll start-event-window
				       (if (eq modeline-scrolling-method t)
					   (- start-hscroll delta)
					 (+ start-hscroll delta)))
		 ))
	       
	     (setq last-timestamp (event-timestamp event)
		   y (event-y-pixel event)
		   edges (window-pixel-edges start-event-window)
		   top (nth 1 edges)
		   bot (nth 3 edges))
	     ;; scale back a move that would make the
	     ;; window too short.
	     (when (not (eq start-y-pixel y)) 
	       (cond ((< (- y top (- modeline-height)) min-height)
		      (setq y (+ top min-height (- modeline-height)))))
	       ;; compute size change needed
	       (setq growth (- y bot (/ (- modeline-height) 2))
		     wconfig (current-window-configuration))
	       ;; grow/shrink minibuffer?
	       (if should-enlarge-minibuffer
		   (progn
		     ;; yes.  scale back shrinkage if it
		     ;; would make the minibuffer less than 1
		     ;; line tall.
		     ;;
		     ;; also flip the sign of the computed growth,
		     ;; since if we want to grow the window with the
		     ;; modeline we need to shrink the minibuffer
		     ;; and vice versa.
		     (if (and (> growth 0)
			      (< (- (window-pixel-height minibuffer)
				    growth)
				 default-line-height))
			 (setq growth
			       (- (window-pixel-height minibuffer)
				  default-line-height)))
		     (setq growth (- growth))))
	       ;; window grow and shrink by lines not pixels, so
	       ;; divide the pixel height by the height of the
	       ;; default face.
	       (setq growth (/ growth default-line-height))
	       ;; grow/shrink the window
	       (enlarge-window growth nil (if should-enlarge-minibuffer
					      minibuffer
					    start-event-window))
	       ;; if this window's growth caused another
	       ;; window to be deleted because it was too
	       ;; short, rescind the change.
	       ;;
	       ;; if size change caused space to be stolen
	       ;; from a window above this one, rescind the
	       ;; change, but only if we didn't grow/shrink
	       ;; the minibuffer.  minibuffer size changes
	       ;; can cause all windows to shrink... no way
	       ;; around it.
	       (if (or (/= start-nwindows (count-windows t))
		       (and (not should-enlarge-minibuffer)
			    (/= top (nth 1 (window-pixel-edges
					    start-event-window)))))
		   (set-window-configuration wconfig))))))))
  
  
(defun orig-mouse-drag-modeline (event)
  "Resize a window by dragging its modeline.
This command should be bound to a button-press event in modeline-map.
Holding down a mouse button and moving the mouse up and down will
make the clicked-on window taller or shorter.

See also the variable `modeline-scrolling-method'."
  (interactive "e")
  (or (button-press-event-p event)
      (error "%s must be invoked by a mouse-press" this-command))
  (or (event-over-modeline-p event)
      (error "not over a modeline"))
  ;; Give the modeline a "pressed" look.  --hniksic
  (let-specifier ((modeline-shadow-thickness
		   (- (specifier-instance modeline-shadow-thickness
					  (event-window event)))
		   (event-window event)))
    (let ((done nil)
	  (depress-line (event-y event))
	  (start-event-frame (event-frame event))
	  (start-event-window (event-window event))
	  (start-nwindows (count-windows t))
	  (hscroll-delta (face-width 'modeline))
	  (start-hscroll (modeline-hscroll (event-window event)))
	  (start-x-pixel (event-x-pixel event))
	  (last-timestamp 0)
	  default-line-height
	  modeline-height
	  should-enlarge-minibuffer
	  event min-height minibuffer y top bot edges wconfig growth)
      (setq minibuffer (minibuffer-window start-event-frame)
	    default-line-height (face-height 'default start-event-window)
	    min-height (+ (* window-min-height default-line-height)
			  ;; Don't let the window shrink by a
			  ;; non-multiple of the default line
			  ;; height.  (enlarge-window -1) will do
			  ;; this if the difference between the
			  ;; current window height and the minimum
			  ;; window height is less than the height
			  ;; of the default font.  These extra
			  ;; lost pixels of height don't come back
			  ;; if you grow the window again.  This
			  ;; can make it impossible to drag back
			  ;; to the exact original size, which is
			  ;; disconcerting.
			  (% (window-pixel-height start-event-window)
			     default-line-height))
	    modeline-height
	    (if (specifier-instance has-modeline-p start-event-window)
		(+ (face-height 'modeline start-event-window)
		   (* 2 (specifier-instance modeline-shadow-thickness
					    start-event-window)))
	      (* 2 (specifier-instance modeline-shadow-thickness
				       start-event-window))))
      (if (not (eq (window-frame minibuffer) start-event-frame))
	  (setq minibuffer nil))
      (if (and (null minibuffer) (one-window-p t))
	  (error "Attempt to resize sole window"))
      ;; if this is the bottommost ordinary window, then to
      ;; move its modeline the minibuffer must be enlarged.
      (setq should-enlarge-minibuffer
	    (and minibuffer (window-lowest-p start-event-window)))
      ;; loop reading events
      (while (not done)
	(setq event (next-event event))
	;; requeue event and quit if this is a misc-user, eval or
	;;   keypress event.
	;; quit if this is a button press or release event, or if the event
	;;   occurred in some other frame.
	;; drag if this is a mouse motion event and the time
	;;   between this event and the last event is greater than
	;;   drag-divider-event-lag.
	;; do nothing if this is any other kind of event.
	(cond ((or (misc-user-event-p event)
		   (key-press-event-p event))
	       (setq unread-command-events (nconc unread-command-events
						  (list event))
		     done t))
	      ((button-release-event-p event)
	       (setq done t)
	       ;; Consider we have a mouse click neither X pos (modeline
	       ;; scroll) nore Y pos (modeline drag) have changed.
	       (and modeline-click-swaps-buffers
		    (= depress-line (event-y event))
		    (or (not modeline-scrolling-method)
			(= start-hscroll
			   (modeline-hscroll start-event-window)))
		    (modeline-swap-buffers event)))
	      ((button-event-p event)
	       (setq done t))
	      ((not (motion-event-p event))
	       (dispatch-event event))
	      ((not (eq start-event-frame (event-frame event)))
	       (setq done t))
	      ((< (abs (- (event-timestamp event) last-timestamp))
		  drag-divider-event-lag)
	       nil)
	      (t
	       (when modeline-scrolling-method
		 (let ((delta (/ (- (event-x-pixel event) start-x-pixel)
				 hscroll-delta)))
		   (set-modeline-hscroll start-event-window
					 (if (eq modeline-scrolling-method t)
					     (- start-hscroll delta)
					   (+ start-hscroll delta)))
		   ))
	       (setq last-timestamp (event-timestamp event)
		     y (event-y-pixel event)
		     edges (window-pixel-edges start-event-window)
		     top (nth 1 edges)
		     bot (nth 3 edges))
	       ;; scale back a move that would make the
	       ;; window too short.
	       (cond ((< (- y top (- modeline-height)) min-height)
		      (setq y (+ top min-height (- modeline-height)))))
	       ;; compute size change needed
	       (setq growth (- y bot (/ (- modeline-height) 2))
		     wconfig (current-window-configuration))
	       ;; grow/shrink minibuffer?
	       (if should-enlarge-minibuffer
		   (progn
		     ;; yes.  scale back shrinkage if it
		     ;; would make the minibuffer less than 1
		     ;; line tall.
		     ;;
		     ;; also flip the sign of the computed growth,
		     ;; since if we want to grow the window with the
		     ;; modeline we need to shrink the minibuffer
		     ;; and vice versa.
		     (if (and (> growth 0)
			      (< (- (window-pixel-height minibuffer)
				    growth)
				 default-line-height))
			 (setq growth
			       (- (window-pixel-height minibuffer)
				  default-line-height)))
		     (setq growth (- growth))))
	       ;; window grow and shrink by lines not pixels, so
	       ;; divide the pixel height by the height of the
	       ;; default face.
	       (setq growth (/ growth default-line-height))
	       ;; grow/shrink the window
	       (enlarge-window growth nil (if should-enlarge-minibuffer
					      minibuffer
					    start-event-window))
	       ;; if this window's growth caused another
	       ;; window to be deleted because it was too
	       ;; short, rescind the change.
	       ;;
	       ;; if size change caused space to be stolen
	       ;; from a window above this one, rescind the
	       ;; change, but only if we didn't grow/shrink
	       ;; the minibuffer.  minibuffer size changes
	       ;; can cause all windows to shrink... no way
	       ;; around it.
	       (if (or (/= start-nwindows (count-windows t))
		       (and (not should-enlarge-minibuffer)
			    (/= top (nth 1 (window-pixel-edges
					    start-event-window)))))
		   (set-window-configuration wconfig))))))))


(provide 'xt-mouse-xmas)

;;; xt-mouse-xmas.el ends here
