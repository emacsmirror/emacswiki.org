;;; code-definition-window.el
;-------------------------------------------
; emacs Code Definition Window
;
; inspired by visual studio feature;
; automatic code navigation assistance -
; toggle windows to show definition of symbol under cursor (0.5s delay)
;
; binds default hotkey C-x w  - toggle a window as a code-def pane
;	(toggle-code-def-window) -  toggle current window as a code-def
;   code-def-windows		- variable, list of all active code-def windows 
;
; extended to have any number of code-def windows
; replacement algorithm tries to keep useful selection of files open
; definition panes also revert to usual edit behaviour when you enter them
;
; TODO: improve replacement algorithm, show declarations as well as definitions
;			option for new frames , visual indication

(message "loading code definition window")
(setf code-def-windows nil)
(defun toggle-code-def-window()
	(interactive)
	(let ((win (selected-window)))
		(if (code-def-window? win)
			(setq code-def-windows
				(remove-if (lambda(x)(eq win x)) code-def-windows)
				(message (concat "Disabled code-def-window; remaining:" (int-to-string (length code-def-windows))))
			)
		;else
			(push win code-def-windows)
			(message (concat "Enable code-def-window, num=" (int-to-string(length code-def-windows)))))))

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


(defun next-code-def-window()
	(if (null code-def-windows)
		nil
	;else
		(car
			(setq code-def-windows 
				(rotate-right code-def-windows)))))

(defun next-code-def-window-except(win)
	(let ((r (next-code-def-window)))
		(if (not (eq r win))
			r
			(next-code-def-window-except win)
		)
	)
)
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

(setq show-def-async nil)
(defun show-definition()
	(interactive)
	(if show-def-async
		(cancel-timer show-def-async))
	(setq show-def-async
		(run-with-idle-timer 0.25 nil 'show-def)))

(defun show-def()
	;cleanup list: remove closed windows
	(when (not(null code-def-windows))
		(setq code-def-windows
			(remove-if (lambda(w)(not(window-live-p w))) code-def-windows))
			(when (not(null code-def-windows))
				(show-definition-sub ))))

(defun show-definition-sub()
	(unless (or 
				(in-minibuffer?)
				(and (in? (selected-window) code-def-windows)
					(eq (length code-def-windows) 1))
				(null (thing-at-point 'symbol))
			)
		; find window to replace.
		; tries to find a window thats' already on the right buffer,
		; to try and show selection of useful buffers.
		; if not available, just pick next round-robin
		(let*(	(tag		(thing-at-point 'symbol))
				(cur-win	(selected-window))
				(win1		(next-code-def-window-except cur-win))
				(next-buf	(ignore-errors (find-tag-noselect tag)))
				(win2		(get-win-of-buf-except code-def-windows next-buf cur-win))
				(win3		(if (null win2) win1 win2))
			)
			(when win3
			;put current window to back of replace list
				(setq code-def-windows 
					(append 
						(list win3)
						(remove-if
							(lambda(x)(eq win3 x))
							code-def-windows)
					))
				;Dont browse to definitions in the same source file, it messes mark 
				; main purpose is to find other useful files
				(if (not(eq (window-buffer cur-win) next-buf )) 
					(ignore-errors
						(set-window-buffer win3 next-buf)							))
			)
		)
	)
)

(add-hook 'post-command-hook  'show-definition)
(global-set-key (kbd "C-x w") 'toggle-code-def-window)

(provide 'show-definition)
(provide 'toggle-code-def-window)
(provide 'code-def-windows)

