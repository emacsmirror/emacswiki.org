;;; frame-restore.el --- save/restore frame size&position at shutdown/startup

;; Copyright (C) 2002 by Free Software Foundation, Inc.

;; Author: Patrick Anderson 
;; Version: 1.3

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.	If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; ChangeLog
;; 1.3: simplified install (now just copy one line to .emacs, and eval the next)
;; 1.3: added checks for running in terminal
;; 1.3: added checks for running on non-w32
;; 1.2: added font save/restore
;; 1.1: added more descriptive, correct installation docs

;;;installation:
;1. put this file in your load path and add to your .emacs file (as the last thing) (without the semicolon):
;(require 'frame-restore)

;2. now evaluate the next line (don't uncomment it) [by putting the cursor at the end and pressing C-xC-e]
;(progn (require 'desktop) (customize-set-variable 'desktop-enable t) (desktop-save "~/") (require 'frame-restore))

;3. now change your font using S-down-mouse-1, adjust your frame size, then shutdown/restart emacs to test.

;;once installed, i never have problems, but before that, it seems possible to get into strange states.	if that happens try:
	;1. shutdown emacs
	;2. delete .emacs.desktop
	;3. restart
	;4. follow normal install

;since the font is stored here, don't also store it through a customization of the 'default' face.  you may customize that face, just make sure the "Font Family" attribute box is unchecked.

;;;Code:
(require 'cl)

;this must be global - as that is how desktop-globals-to-save works 
;(defvar final-frame-params '((frame-parameter (selected-frame) 'font) 50 50 150 50 nil)) ;font, left, top, width, height, maximized
(defvar final-frame-params '("-adobe-courier-medium-r-normal--*-120-*-*-m-*-iso8859-1" 50 50 150 50 nil)) ;font, left, top, width, height, maximized

(if window-system
	(add-hook 'after-init-hook
			  '(lambda()
				 "this is executed as emacs is coming up - _after_ final-frame-params have been read from `.emacs.desktop'."
				 (when desktop-enable
				   (desktop-load-default)
				   (desktop-read)
				   ;;now size and position frame according to the  values read from disk
				   (set-default-font (first final-frame-params)) ;do font first - as it will goof with the frame size
				   (set-frame-size (selected-frame) (fourth final-frame-params) (fifth final-frame-params))
				   (set-frame-position (selected-frame) (max (eval (second final-frame-params)) 0)	(max (eval (third final-frame-params)) 0))
				   (if (sixth final-frame-params)
					   (if (eq window-system 'w32)
						   (w32-send-sys-command ?\xf030)
										;else, do X something
						 ))))))

(if window-system
	(add-hook 'desktop-save-hook
			  '(lambda()
				 (let ((maximized (listp (frame-parameter (selected-frame) 'left))))
				   "this hook sets the fram size/pos vars before `desktop.el' writes them out to disk"
				   (if (eq window-system 'w32)
					   (w32-send-sys-command ?\xf120)	;restore the frame (so we can save the 'restored' size/pos)
										;else, do X something
					 )
				   ;;prepend our vars to the save list so `desktop.el' will save them out to disk
				   (setq desktop-globals-to-save (cons 'final-frame-params
													   desktop-globals-to-save))
				   
				   (setq final-frame-params 
						 (list 
						  (frame-parameter (selected-frame) 'font)
						  (frame-parameter (selected-frame) 'left) ;x
						  (frame-parameter (selected-frame) 'top) ;y
						  (frame-width)				;width
						  (frame-height)				;height
						  maximized)))))	;if this frame param is a list, we're probably maximized (not guaranteed)
  )

(provide 'frame-restore)
;;; frame-restore.el ends here
