;;; virtual-desktops.el --- allows you to save/restore a frame configuration: windows and buffers.
;;
;; Filename: virtual-desktops.el
;; Description: allows you to save/retore a frame configuration: windows and buffers.
;; Author: Cédric Chépied <cedric.chepied@gmail.com>
;; Maintainer: Cédric Chépied
;; Copyright (C) 2012, Cédric Chépied
;; Last updated: Wed Jul  4 17:47:07 UTC
;;     By Cédric Chépied
;;     Update 1
;; Keywords: virtual, desktop
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;allows you to save/retore a frame configuration: windows and buffers.
;;
;;
;; Keys and interactive functions:
;; virtual-desktops-add:             save current configuration in a new virtual desktop and select it (C-c C-d a)
;; virtual-desktops-delete:          delete current desktop and select the nil desktop (C-c C-d d)
;; virtual-desktops-delete-specific: delete a specific desktop and select the nil desktop if you choose current desktop (C-c C-d D)
;; virtual-desktops-goto:            restore a specific desktop (C-c C-d g)
;; virtual-desktops-next:            go to next desktop (C->)
;; virtual-desktops-previous:        go to previous desktop (C-<)
;; virtual-desktops-list:            list all desktops (C-c C-d l)
;; virtual-desktops-update:          save current configuration in current desktop
;;
;;
;; Variables:
;; virtual-desktops-auto-update: if non nil, current desktop will be updated before execution of virtual-desktops-next, virtual-desktops-prev, virtual-desktops-goto
;;
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;; Copyright Cédric Chépied 2012
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;;
;;
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; TODO
;;
;; list buffer must be interactive
;; make current desktop frame specific
;; test very complicated window configuration
;;
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; desktop format
;;
;; desktop list : [desktop 1] [desktop 2]
;;    				|
;; 	    			---> [Window 1] [Window 2]
;;							 |
;;							 ---> [Window Edges] [buffer name]
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; Code:

(provide 'virtual-desktops)

;;constants
(defconst virtual-desktops-list-buffer-name "##virtual-desktops##")

;;global variables
(defvar virtual-desktops-list (list nil))
(defvar virtual-desktops-current 0)
(defvar virtual-desktops-mode-line-string nil)


;;group
(defgroup virtual-desktop nil "Customization of virtual-desktop variables."
  :tag "virtual-desktop"
  :group 'emacs)


;;customizable variables
(defcustom virtual-desktops-auto-update "desktop auto update" "If non nil, current desktop will be updated when calling any virtual-desktops function."
  :type 'boolean
  :group 'virtual-desktop)


;; Custom Minor Mode
(define-minor-mode virtual-desktops-mode
  "Enable desktops creation which save or restore windows and buffers of the frame."
  ;; The initial value - Set to 1 to enable by default
  nil
  ;; The indicator for the mode line.
  ""
  ;; The minor mode keymap
  `(
    (,(kbd "C->") . virtual-desktops-next)
    (,(kbd "C-<") . virtual-desktops-prev)
	(,(kbd "C-c C-d a") . virtual-desktops-add)
  	(,(kbd "C-c C-d d") . virtual-desktops-del)
  	(,(kbd "C-c C-d D") . virtual-desktops-del-specific)
  	(,(kbd "C-c C-d g") . virtual-desktops-goto)
  	(,(kbd "C-c C-d l") . virtual-desktops-list)
   )
   ;; Make mode global rather than buffer local
   :global 1

   ;;initialize variables
   (progn
	 (setq virtual-desktops-list (list nil))
	 (setq virtual-desktops-current 0)
	 (if virtual-desktops-mode
		 (setq virtual-desktops-mode-line-string "-Desk: nil-")
	     (setq virtual-desktops-mode-line-string ""))
	 (or global-mode-string
		 (setq global-mode-string '("")))
	 (or (memq 'virtual-desktops-mode-line-string global-mode-string)
		 (setq global-mode-string
			   (append global-mode-string '(virtual-desktops-mode-line-string)))))
)




(defun virtual-desktops-get-window-xmin (window)
  (nth 0 (nth 0 window))
)

(defun virtual-desktops-get-window-xmax (window)
  (nth 2 (nth 0 window))
)

(defun virtual-desktops-get-window-ymin (window)
  (nth 1 (nth 0 window))
)

(defun virtual-desktops-get-window-ymax (window)
  (nth 3 (nth 0 window))
)

(defun virtual-desktops-get-window-buffer (window)
  (nth 1 window)
)

(defun virtual-desktops-get-window-width (window)
  (- (virtual-desktops-get-window-xmax window) (virtual-desktops-get-window-xmin window))
)

(defun virtual-desktops-get-window-height (window)
  (- (virtual-desktops-get-window-ymax window) (virtual-desktops-get-window-ymin window))
)

(defun virtual-desktops-get-window (x y window-list)
  (let (window)
	(setq window nil)
	(catch 'break;
	  (dolist (w window-list)
		(progn (if (and (>= x (virtual-desktops-get-window-xmin w))
						(<= x (virtual-desktops-get-window-xmax w))
						(>= y (virtual-desktops-get-window-ymin w))
						(<= y (virtual-desktops-get-window-ymax w)))
				   (progn (setq window w)
						  (throw 'break t))))))
	window)
)


;; returns (block-xmin block-ymin block-xmax block-ymax block-width block-height)
(defun virtual-desktops-get-block-dimensions (block)
  (let (block-xmin block-ymin block-xmax block-ymax block-width block-height)
	(dolist (window block)
	  (progn (if (or (equal nil block-xmin)
					 (< (virtual-desktops-get-window-xmin window) block-xmin))
				 (setq block-xmin (virtual-desktops-get-window-xmin window)))
			 (if (or (equal nil block-xmax)
					 (> (virtual-desktops-get-window-xmax window) block-xmax))
				 (setq block-xmax (virtual-desktops-get-window-xmax window)))
			 (if (or (equal nil block-ymin)
					 (< (virtual-desktops-get-window-ymin window) block-ymin))
				 (setq block-ymin (virtual-desktops-get-window-ymin window)))
			 (if (or (equal nil block-ymax)
					 (> (virtual-desktops-get-window-ymax window) block-ymax))
				 (setq block-ymax (virtual-desktops-get-window-ymax window)))))
	(setq block-width (- block-xmax block-xmin))
	(setq block-height (- block-ymax block-ymin))

	(list block-xmin block-ymin block-xmax block-ymax block-width block-height))
)

;; splits vertically and return the 2 new blocks
;; return nil if can't split
(defun virtual-desktops-split-vertically (window-list total-frame-width total-frame-height)
  (let (dimensions block-xmin block-xmax block-ymin block-ymax block-width block-height window windows-width split-y)
	(setq windows-width 0)
	(setq split-y nil)

	;;get size
	(setq dimensions (virtual-desktops-get-block-dimensions window-list))
	(setq block-xmin (nth 0 dimensions))
	(setq block-ymin (nth 1 dimensions))
	(setq block-xmax (nth 2 dimensions))
	(setq block-ymax (nth 3 dimensions))
	(setq block-width (nth 4 dimensions))
	(setq block-height (nth 5 dimensions))

	;;select window
	(let (tempx tempy framey)
	  (setq framey (1- (frame-height))) ;;remove minibuffer
	  (setq tempx (1- (/ (* block-xmax (frame-width)) total-frame-width)))
	  (setq tempy (1- (/ (* block-ymax framey) total-frame-height)))
	  (select-window (window-at tempx tempy)))


	(catch 'break
	  (let (y window)
		(setq y block-ymin)
		(while (< y block-ymax) ;;while there are windows left
		  (setq window (virtual-desktops-get-window block-xmin y window-list)) ;; get window in (0,y)
		  (setq windows-width (virtual-desktops-get-window-width window)) ;;windows-width is window width
		  (if (>= windows-width block-width) ;;if there is only one window in all width we can split
			  (progn (setq split-y (virtual-desktops-get-window-height window))
					 (throw 'break t)))
		  (progn (let (wtemp)
				   (dolist (wtemp window-list)
					 (progn (if (not (equal wtemp window)) ;;if wtemp is not the same window
								(if (= (virtual-desktops-get-window-ymax wtemp) (virtual-desktops-get-window-ymax window)) ;;if wtemp ymax = window ymax they are forming a line
									(progn (setq windows-width (+ windows-width (virtual-desktops-get-window-width wtemp))) ;;add window width to total width
										   (if (>= windows-width block-width) ;;if all selected window width is the block width we can split
											   (progn (setq split-y (- (virtual-desktops-get-window-ymax window) block-ymin))
													  (throw 'break t))))))))))
		  (setq y (+ y (virtual-desktops-get-window-height window)))))) ;;next window


	(if (and split-y
			 (< split-y block-height))
		(progn (let (temp)
				 (setq temp (/ (* split-y (window-height)) block-height))
				 (split-window-vertically temp))
			   ;;window is splitted we need to create the two blocks
			   (let (block-1 block-2)
				 (setq block-1 'nil)
				 (setq block-2 'nil)
				 (dolist (window window-list)
				   (progn (if (< (virtual-desktops-get-window-ymin window) (+ block-ymin split-y))
							  (setq block-1 (cons window block-1))
							  (setq block-2 (cons window block-2)))))
				 (list block-1 block-2)))
	    nil))
)


;; splits horizontally and return the 2 new blocks
;; return nil if can't split
(defun virtual-desktops-split-horizontally (window-list total-frame-width total-frame-height)
  (let (block-xmin block-xmax block-ymin block-ymax block-width block-height window windows-height split-x dimensions)
	(setq windows-height 0)
	(setq split-x nil)

	;;get size
	(setq dimensions (virtual-desktops-get-block-dimensions window-list))
	(setq block-xmin (nth 0 dimensions))
	(setq block-ymin (nth 1 dimensions))
	(setq block-xmax (nth 2 dimensions))
	(setq block-ymax (nth 3 dimensions))
	(setq block-width (nth 4 dimensions))
	(setq block-height (nth 5 dimensions))

	;;select window
	(let (tempx tempy framey)
	  (setq framey (1- (frame-height))) ;;remove minibuffer
	  (setq tempx (1- (/ (* block-xmax (frame-width)) total-frame-width)))
	  (setq tempy (1- (/ (* block-ymax framey) total-frame-height)))
	  (select-window (window-at tempx tempy)))

	(catch 'break
	  (let (x window)
		(setq x block-xmin)
		(while (< x block-xmax) ;;while there are windows left
		  (setq window (virtual-desktops-get-window x block-ymin window-list)) ;; get window in (0,y)
		  (setq windows-height (virtual-desktops-get-window-height window)) ;;windows-width is window width
		  (if (>= windows-height block-height) ;;if there is only one window in all height we can split
			  (progn (setq split-x (virtual-desktops-get-window-width window))
					 (throw 'break t)))
		  (progn (let (wtemp)
				   (dolist (wtemp window-list)
					 (progn (if (not (equal wtemp window)) ;;if wtemp is not the same window
								(if (= (virtual-desktops-get-window-xmax wtemp) (virtual-desktops-get-window-xmax window)) ;;if wtemp xmax = window xmax they are forming a line
									(progn  (setq windows-height (+ windows-height (virtual-desktops-get-window-height wtemp))) ;;add window height to total height
											(if (>= windows-height block-height) ;;if all selected window height is the block height we can split
												(progn (setq split-x (- (virtual-desktops-get-window-xmax window) block-xmin))
													   (throw 'break t))))))))))
		  (setq x (+ x (virtual-desktops-get-window-width window)))))) ;;next window



	(if (and split-x
			 (< split-x block-width))
		(progn (let (temp)
				 (setq temp (1+ (/ (* split-x (window-width)) block-width)))
				 (split-window-horizontally temp))
			   ;;window is splitted we need to create the two blocks
			   (let (block-1 block-2)
				 (setq block-1 'nil)
				 (setq block-2 'nil)
				 (dolist (window window-list)
				   (progn (if (< (virtual-desktops-get-window-xmin window) (+ block-xmin split-x))
							  (setq block-1 (cons window block-1))
							  (setq block-2 (cons window block-2)))))
				 (list block-1 block-2)))
	    nil))
)


;;splits the block until there is only one window in each block
(defun virtual-desktops-split-block (window-list total-frame-width total-frame-height)
  (if (> (safe-length window-list) 1)
	  (progn (let (block-1 block-2 result)
			   ;;search for a split
			   (setq result (virtual-desktops-split-vertically window-list total-frame-width total-frame-height))
			   (if (equal result nil)
				   (setq result (virtual-desktops-split-horizontally window-list total-frame-width total-frame-height)))
			   (if (not (equal result nil))
				   (progn (setq block-1 (nth 0 result))
						  (setq block-2 (nth 1 result))
						  (virtual-desktops-split-block block-1 total-frame-width total-frame-height)
						  (virtual-desktops-split-block block-2 total-frame-width total-frame-height))
				 (print "Error: No split Found!"))))
	  (progn (if (not (equal nil (buffer-name (virtual-desktops-get-window-buffer (nth 0 window-list)))))
				 (set-window-buffer (window-at (- (/ (* (virtual-desktops-get-window-xmax (nth 0 window-list)) (frame-width)) total-frame-width) 2)
											   (- (/ (* (virtual-desktops-get-window-ymax (nth 0 window-list)) (frame-height)) total-frame-height) 2))
									(virtual-desktops-get-window-buffer (nth 0 window-list))))))
)


;;restore the desired desktop
(defun virtual-desktops-restore (number)
  (let (desktop)
 	(setq desktop (nth number virtual-desktops-list))
	(if (not (equal nil desktop))
		(progn (let (frame-width frame-height temp-list)
				 (delete-other-windows)
				 (setq temp-list (virtual-desktops-get-block-dimensions desktop))
				 (setq frame-width (nth 4 temp-list))
				 (setq frame-height (nth 5 temp-list))
				 (virtual-desktops-split-block desktop frame-width frame-height)))))
)


;;save desktop and return it
(defun virtual-desktops-create-desktop ()
  (let (window-list user-window top-left-window)
	;;save user window
	(setq user-window (selected-window))

	;;go to the top left window
	(select-window (window-at 0 0))
	(setq top-left-window (selected-window))

	;;create list with the first windo
	(setq window-list (list (list (window-edges) (window-buffer))))
	(select-window (next-window))

	(while (not (equal top-left-window (selected-window)))
	  (setq window-list (cons (list (window-edges) (window-buffer)) window-list))
	  (select-window (next-window)))
			 			 
	(select-window user-window)
	window-list)
)


;;delete a desktop if it is not the nil desktop
(defun virtual-desktops-delete (number)
  (if (and (< number (safe-length virtual-desktops-list))
		   (> number 0))
	  (setq virtual-desktops-list (delq (nth number virtual-desktops-list) virtual-desktops-list))
	  (print (concat "Cant delete this desktop :" (number-to-string number))))
)


(defun virtual-desktops-update-mode-line ()
  (if (= virtual-desktops-current 0)
	  (setq virtual-desktops-mode-line-string "-Desk: nil")
	  (setq virtual-desktops-mode-line-string (concat "-Desk: " (number-to-string virtual-desktops-current))))
  (force-mode-line-update)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;								Interactive functions									;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun virtual-desktops-add ()
  (interactive)
  (if virtual-desktops-mode
	  (progn (setq virtual-desktops-list (append virtual-desktops-list (list (virtual-desktops-create-desktop))))
			 (setq virtual-desktops-current (1- (safe-length virtual-desktops-list)))
			 (virtual-desktops-update-mode-line))
	  (print "virtual-desktops-mode must be enabled"))
)


(defun virtual-desktops-update ()
  (interactive)
  (if virtual-desktops-mode
	  (progn (if (not (= virtual-desktops-current 0))
				 (let (desktop)
				   (setq desktop (virtual-desktops-create-desktop))
				   (setcar (nthcdr virtual-desktops-current virtual-desktops-list) desktop))))
  	  (print "virtual-desktops-mode must be enabled"))
)

(defun virtual-desktops-del ()
  (interactive)
    (if virtual-desktops-mode
		(progn (virtual-desktops-delete virtual-desktops-current)
			   (setq virtual-desktops-current 0)
			   (virtual-desktops-update-mode-line))
	    (print "virtual-desktops-mode must be enabled"))
)

(defun virtual-desktops-del-specific ()
  (interactive)
  (if virtual-desktops-mode
	  (progn (let (desktop)
			   (setq desktop (read-from-minibuffer "desktop to delete? "))
			   (virtual-desktops-delete (string-to-number desktop))
			   (if (= virtual-desktops-current (string-to-number desktop))
				   (setq virtual-desktops-current 0)))
			 (virtual-desktops-update-mode-line))
	  (print "virtual-desktops-mode must be enabled"))
)

(defun virtual-desktops-next ()
  (interactive)
  (if virtual-desktops-mode
	  (progn (if (not (equal nil virtual-desktops-auto-update))
				 (virtual-desktops-update))
			 (setq virtual-desktops-current (1+ virtual-desktops-current))
			 (if (>= virtual-desktops-current (safe-length virtual-desktops-list))
				 (setq virtual-desktops-current 0))
			 (virtual-desktops-restore virtual-desktops-current)
			 (virtual-desktops-update-mode-line))
	  (print "virtual-desktops-mode must be enabled"))
)

(defun virtual-desktops-prev ()
  (interactive)
  (if virtual-desktops-mode
	  (progn (if (not (equal nil virtual-desktops-auto-update))
				 (virtual-desktops-update))
			 (setq virtual-desktops-current (1- virtual-desktops-current))
			 (if (< virtual-desktops-current 0)
				 (setq virtual-desktops-current (1- (safe-length virtual-desktops-list))))
			 (virtual-desktops-restore virtual-desktops-current)
			 (virtual-desktops-update-mode-line))
	  (print "virtual-desktops-mode must be enabled"))
)

(defun virtual-desktops-goto ()
  (interactive)
  (if virtual-desktops-mode
	  (progn (if (not (equal nil virtual-desktops-auto-update))
				 (virtual-desktops-update))
			 (let (desktop number)
			   (setq desktop (read-from-minibuffer "desktop to display? "))
			   (if (equal "nil" desktop)
				   (setq number 0)
				   (setq number (string-to-number desktop)))
			   (setq virtual-desktops-current number))
			 (virtual-desktops-restore virtual-desktops-current)
			 (virtual-desktops-update-mode-line))
	  (print "virtual-desktops-mode must be enabled"))
)

(defun virtual-desktops-list ()
  (interactive)
  (if virtual-desktops-mode
	  (progn (let (buffer i)
			   ;;killing buffer if it exists
			   (if (not (equal nil (get-buffer virtual-desktops-list-buffer-name)))
				   (kill-buffer virtual-desktops-list-buffer-name))
	
			   ;;creating buffer
			   (setq buffer (get-buffer-create virtual-desktops-list-buffer-name))
			   (switch-to-buffer buffer)
	
			   ;;insert desktop list
			   (insert "This is desktop list\nYou can set point on the desired one and press RET to switch to this desktop\n\n")
			   (setq i 0)
			   (while (< i (safe-length virtual-desktops-list))
				 (insert (propertize (number-to-string i) 'font-lock-face '(:foreground "red")))
				 (insert "\t")
				 (let (window window-list)
				   (setq window-list (nth i virtual-desktops-list))
				   (if (equal window-list nil)
					   (insert "nil")
					 (dolist (window window-list)
					   (insert "<")
					   (if (equal nil (buffer-name (virtual-desktops-get-window-buffer window)))
						   (insert "Deleted buffer")
						 (insert (buffer-name (virtual-desktops-get-window-buffer window))))
					   (insert "> "))))
				 (insert "\n\n")
				 (setq i (1+ i)))
	
			   ;;setting buffer read only
			   (toggle-read-only)))
			 (print "virtual-desktops-mode must be enabled"))
)

;;virtual-desktops.el ends here
