;;; code-definition-window.el
;-------------------------------------------
; Code Definition Window
;
; inspired by visual studio feature;
; toggle windows to show definition of symbol under cursor (0.5s delay)
;
; binds:
;	C-x w	- (enable-code-def-window)-create 2nd window &enable;
;	C-x p	- (popup-code-def-window) - new frame with code-def window
;			(toggle-code-def-window) -  toggle current win as a code-def
;   code-def-windows		- variable, list of all active code-def windows 
;
; extended to have any number of code-def windows
; replacement algorithm tries to keep useful selection of files open
; definition panes also revert to usual edit behaviour when you enter them
;
; TODO: improve replacement algorithm, show declarations as well as definitions
;			option for new frames , visual indication
;
; TAGS: code definition window ; auto jump to tag ; auto navigation

(message "loading code definition window")
(setf code-def-windows nil)

(defvar code-def-win-offset 4)
(defvar cdw-popup-width 60)
(defvar cdw-popup-height 20)
(defvar cdw-popup-mode nil)
(defvar cdw-delay 0.25)
(defvar cdw-colors [
	"#f8fffc" "#f8fcff" "#fffcf8" "#f8fffc" "#fff8f8" "#f8fffa" "#fffff8"
	"#fcfff8" "#f0f8ff" "#fff8fc" "#fcfff8" "#fffff8" "#faf8ff" "#faffff"
])
(defvar cdw-next-col 0)

(defun toggle-code-def-window()
	"toggles the current window as a Code-Definition Window by adding or removing from cde-def-windows (active list)"
	(interactive)
	(let ((win (selected-window)))
		(if (code-def-window? win)
			(progn
				(setq code-def-windows
					(remove-if (lambda(x)(eq win x)) code-def-windows))
				(message "Disabled code-def-window, remaining=%d"
										(length code-def-windows)))
		:else
			(push win code-def-windows)
			(message (concat "Enable code-def-window, num=" (int-to-string(length code-def-windows)))))))

(defun set-code-def-window(win)
	"set current win as a code-def window - add to the active list if not already there"
	(if (not(code-def-window? win))
		(push win code-def-windows)))

(defun in-minibuffer?()
	(minibufferp (window-buffer(selected-window))))

(defun in? (x ls)
	(if (null ls)
		nil
	;else
		(if (eq x (car ls))
			t
			(in? x (cdr ls)))))

(defun code-def-window?(win)
	; todo: there will be multiple code-def panes allowed..
	(in? win code-def-windows)
)

(defun rotate-right (ls)
	(let ((x (car ls)))
		(append (cdr ls) (list x))))			


(defun cdw-get-buffer-color (bf)
	(let (	(i0 (length	(buffer-name bf)))
			(i1 (aref	(buffer-name bf) 0)))
		(aref cdw-colors (mod (+ i0 i1) (length cdw-colors)))
	)
)

(defun next-code-def-window()
	(if (null code-def-windows)
		nil
	;else
		(car
			(setq code-def-windows 
				(rotate-right code-def-windows)))))

(defun next-code-def-window-prefering-same-frame-as (win)
	(setq code-def-windows
		(append
			(code-def-windows-same-frame-as win)
			(code-def-windows-not-in-same-frame-as win)))
	(next-code-def-window)
)

(defun next-code-def-window-except(win)
	(let ((r (next-code-def-window)))
		(if (not (eq r win))
			r
			(next-code-def-window-except win))))

(defun get-win-of-buf-except (ls buf exwin)
	(if (or(not ls)(null buf))
		nil
		(let ((x (car ls)))
			(if (not x)
				nil
				(if (and
						(eq buf (window-buffer x))
						(not(eq x exwin)))
					x
					(get-win-of-buf-except (cdr ls) buf exwin))))))


(defun window-name(win)(if (not win)"nil" (buffer-name(window-buffer win))))

(defun overlap-of-range (s0 d0 s1 d1)
	(let ((e0 (+ s0 d0))(e1 (+ s1 d1)))
		(let ((s (max s0 s1))(e (min e0 e1)))
			(max 0 (- e s))
		)
	)
)
		
(defun overlap-at (f0 f1 x0 y0)
	(* (overlap-of-range (frame-x f1) (frame-pixel-width f1) x0 (frame-pixel-width f0))
	   (overlap-of-range (frame-y f1) (frame-pixel-height f1) y0 (frame-pixel-height f0))))

(defun measure-weighted-overlap (f x y)
	"TODO: find overlap of a frame if it was placed here, weighted for window priorities.. for new frame placement algorith "
	(reduce (lambda(a b)(overlap-at a b x y))
		(frames-on-display-list )0)
)

(defun first-not-eq-to (ls x)
	(if (not ls) nil
		(if (eq (first ls) x)
			(first-not-eq-to (cdr ls) x)
			(first ls))))
		

(setq show-def-async nil)
(defun show-definition()
	"main callback for the code-def window, triggers everything"
	(interactive)
	(if show-def-async
		(cancel-timer show-def-async))
	(setq show-def-async
		(run-with-idle-timer cdw-delay nil 'show-def)))


(defun show-def()
	"code-def window outer function: remove windows that have been deleted"
	;cleanup list: remove closed windows
	(when (or cdw-popup-mode (not(null code-def-windows)))
		(setq code-def-windows
			(remove-if (lambda(w)(not(window-live-p w))) code-def-windows))
			(when (not(null code-def-windows))
				(show-definition-sub))))

(defun show-definition-sub()
	"code def window main behaviour. Searches thee code-def window list to find the most suitable one to replace with the latest definition- tries to keep a selection of source files open by prefering to show definitions from the same file in the same window"
;	(if (selected-window)
;		(if (window-buffer (selected-window))
;			(if (<= 0 (buffer-size (window-buffer(selected-window))))
;				(return))))
	(message "find def for %s" (thing-at-point 'symbol))
	(unless (or
;				(<= 0 (buffer-size (window-buffer (selected-window)))) 
				(in-minibuffer?)
;				(and (in? (selected-window) code-def-windows)
;					(not cdw-popup-mode)
;					(eq (length code-def-windows) 1))
				(null (thing-at-point 'symbol))
;				(and (not cdw-popup-mode)
;					(code-def-window? 
;						(selected-window)))
			)
		(message "ok..")
		; find window to replace.
		; tries to find a window thats' already on the right buffer,
		; to try and show selection of useful buffers.
		; if not available, just pick next round-robin

;		(find-tag-noselect tag) ;find first tag
		(setq code-def-windows (cdw-sort-windows-by-priority code-def-windows (selected-window)))
		(point-to-register 'curpos)
		(let*(	(tag		(thing-at-point 'symbol))
				(cur-win	(selected-window))
				(win1		(next-code-def-window))
				(def-buf	(ignore-errors (find-tag-noselect tag)))
				(win2a		(get-win-of-buf-except (code-def-windows-in-same-frame-as cur-win) def-buf cur-win))
				(win2b		(get-win-of-buf-except code-def-windows def-buf cur-win))
				(win2		(if win2a win2a win2b))
;				(win		(if (null win2) win1 win2))
				(win4 (first-not-eq-to (code-def-windows-in-same-frame-as cur-win) cur-win))
				; replacement policy:-
				(win (cond
						(win2a win2a) ;1st: same frame, already same buffer
						(win4 win4) ;2nd: next in same frame.
						(t win1); 3rd - next in cyclical order
					)
				)
						
			)
			(message "find def for %s in %s" (thing-at-point 'symbol) (buffer-name def-buf))
			(when (and win def-buf)
			;put current window to back of replace list
				(setq code-def-windows 
					(append 
						(list win)
						(remove-if
							(lambda(x)(eq win x))
							code-def-windows)))
			;	(if (not(eq (window-buffer cur-win) next-buf )) ? dont browse from current, messes loc?
				(ignore-errors
					(select-window win)
					(find-tag tag)
					(buffer-face-set :background (cdw-get-buffer-color def-buf))
					;(set-window-buffer win3 def-buf) ?doesn't seem to always work, if refinding in same buf
					(recenter (min code-def-win-offset (/(- (window-height win) 2) 2)))
					(raise-frame (window-frame win))
					(select-window cur-win)
					(raise-frame (window-frame cur-win))
				)
			)
		)
		(register-to-point 'curpos)
	)
)

(defun broken-popup-mode-do-not-call()
			(when cdw-popup-mode 
				(message "popup-mode:-")
				(when def-buf
					; popup-mode -always make new windows for definitions we find
					(message "popup mode:tag %s in %s" tag (buffer-name def-buf))
					(let ((win4ls (get-buffer-window-list def-buf)) (win4 nil))
						(print win4ls)
						(when win4ls (message "found window for %s" (buffer-name def-buf))
							(setf win4 (car win4ls))
						)
						(unless win4ls
							(message "try to create:")
							(setf win4 (popup-code-def-window))
							(print "created:-")
							(print win4)
							(message "created new popup , buf=%s" (buffer-name(window-buffer win4)))
							(set-window-buffer win4 def-buf)
						)
						(setf win3 win4)
						(set-window-buffer win3 def-buf)
				;		(setf cdw-popup-mode nil)
					)
				)
				(unless def-buf
					(message "popup-mode  - no def found")
					(setf win3 nil)
				)
			)
)

(defun enable-code-def-window ()
	"Creates a second window if only one pane is visible, and toggles the current pane as a code-definition window. Convinient 1-press key for a common useable setup"
	(interactive)
	(cond
		(	(> (length (window-list)) 1)
			(toggle-code-def-window)
		)
		(	t
			(split-window-horizontally)
			(other-window 1)
			(toggle-code-def-window)
			(other-window -1)
		)
	)
)

(defun window-in-same-frame? (win1 win2)
	(eq (window-frame win1)(window-frame win2)))

(defun cdw-sort-windows-by-priority (wins curr)
	"sorts the list of code-def windows such that anything on the same frame comes first, anything on the same display comes second, anything on other displays is last."
	(append
		(remove-if-not (lambda(x) (window-in-same-frame? x curr)) wins)
		(remove-if (lambda(x) (window-in-same-frame? x curr)) wins)))

(defun code-def-windows-in-same-frame-as (curr)
	(remove-if-not (lambda(x) (window-in-same-frame? x curr)) code-def-windows))
(defun code-def-windows-not-in-same-frame-as (curr)
	(remove-if (lambda(x) (window-in-same-frame? x curr)) code-def-windows))

(defun popup-code-def-window()
	"Show a popup code def window, enable popup floating-frame mode;"
	(interactive)
	(let ((nf (new-frame)))
;		(setf cdw-popup-mode t) popup mode is completely broken
		(setf cdw-delay 0.25)
		(other-frame 1)
		(set-frame-size nf cdw-popup-width cdw-popup-height)
		(set-code-def-window (frame-first-window nf))
		; popup-mode doesn't need to maintain a code-def window list - all are active.
		(other-frame -1)
		(print (frame-first-window nf))
		(return (frame-first-window nf))
	)
)

(add-hook 'post-command-hook  'show-definition)
(global-set-key (kbd "C-x w") 'enable-code-def-window)
(global-set-key (kbd "C-x p") 'popup-code-def-window)

(provide 'show-definition)
(provide 'toggle-code-def-window)
(provide 'enable-code-def-window)
(provide 'code-def-windows)
(provide 'code-def-win-offset)
(provide 'cdw-colors)
