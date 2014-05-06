;;; virtual-desktops.el --- allows you to save/restore a frame configuration:
;;  windows and buffers.
;;
;; Filename: virtual-desktops.el
;; Description: allows you to save/retore a frame configuration: windows and buffers.
;; Author: Cédric Chépied <cedric.chepied@gmail.com>
;; Maintainer: Cédric Chépied
;; Copyright (C) 2012 - 2014, Cédric Chépied
;; Last updated: Tue May 6th 16:45:46 CEST 2014
;;     By Cédric Chépied
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
;; virtual-desktops-add:             save current configuration in a new virtual
;;                                   desktop and select it (C-c C-d a). Use
;;                                   prefix argument to create several desktops.
;; virtual-desktops-delete:          delete current desktop and select the nil
;;                                   desktop (C-c C-d d)
;; virtual-desktops-delete-specific: delete a specific desktop and select the
;;                                   nil desktop if you choose current
;;                                   desktop (C-c C-d D)
;; virtual-desktops-goto:            restore a specific desktop (C-c C-d g)
;; virtual-desktops-next:            go to next desktop (C->)
;; virtual-desktops-previous:        go to previous desktop (C-<)
;; virtual-desktops-list:            list all desktops (C-c C-d l)
;; virtual-desktops-update:          save current configuration in current
;;                                   desktop (C-c C-d u)
;;
;;
;; Variables:
;; virtual-desktops-auto-update: if non nil, current desktop will be updated
;; before execution of virtual-desktops-next, virtual-desktops-prev and
;; virtual-desktops-goto
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
;; Copyright Cédric Chépied 2012 - 2014
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
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; desktop:
;; (frame-size window-list selected-window)

;; frame-size:
;; (w h)

;; window-list:
;; (window0 window1 ...)
;; window0 = minibuffer

;; window
;; (buffer window-edges)

;; selected window = int


(provide 'virtual-desktops)

;;constants
(defconst virtual-desktops-list-buffer-name "##virtual-desktops##")


;;global variables
(defvar virtual-desktops-list (list nil))
(defvar virtual-desktops-current 0)
(defvar virtual-desktops-mode-line-string nil)
(defvar virtual-desktops-last-frame nil)
(defvar virtual-desktops-list-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") (lambda () (interactive)
                                  (virtual-desktops-goto (tabulated-list-get-id) t)))

    (define-key map (kbd "D") (lambda () (interactive)
                                (virtual-desktops-del-specific (tabulated-list-get-id))))

    map)
  "Keymap for `virtual-desktops-list-mode'.")


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
  	(,(kbd "C-c C-d u") . virtual-desktops-update)
   )
   ;; Make mode global rather than buffer local
   :global 1

   ;;initialize variables
   (progn
	 (setq virtual-desktops-list (list nil))
	 (setq virtual-desktops-current 0)
	 (if virtual-desktops-mode
		 (setq virtual-desktops-mode-line-string " (D nil) ")
	     (setq virtual-desktops-mode-line-string ""))
	 (or global-mode-string
		 (setq global-mode-string '("")))
	 (or (memq 'virtual-desktops-mode-line-string global-mode-string)
		 (setq global-mode-string
			   (append global-mode-string '(virtual-desktops-mode-line-string)))))
)

;; Major mode for desktop list buffer
(define-derived-mode virtual-desktops-list-mode
  tabulated-list-mode
  "Desktop list" "Mode for listing virtual desktops."
  (setq tabulated-list-format [("Id" 5 t)
                               ("Buffers" 0 nil)])
  (setq tabulated-list-entries 'virtual-desktops-list-entries)
  (tabulated-list-init-header)
  (use-local-map (append tabulated-list-mode-map
                         virtual-desktops-list-keymap)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;								  Internal functions									;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun virtual-desktops-get-window-xmin (window)
  "Return the x coordinate of top left corner of the window."
  (nth 0 (nth 1 window)))

(defun virtual-desktops-get-window-xmax (window)
  "Return the x coordinate of bottom right corner of the window."
  (nth 2 (nth 1 window)))

(defun virtual-desktops-get-window-ymin (window)
  "Return the y coordinate of top left corner of the window."
  (nth 1 (nth 1 window)))

(defun virtual-desktops-get-window-ymax (window)
  "Return the y coordinate of bottom right corner of the window."
  (nth 3 (nth 1 window)))

(defun virtual-desktops-get-window-buffer (window)
  "Return the buffer of a window."
  (car window))

(defun virtual-desktops-get-window-width (window)
  "Return the width of a window."
  (- (virtual-desktops-get-window-xmax window) (virtual-desktops-get-window-xmin window))
)

(defun virtual-desktops-get-window-height (window)
  "Return the height of a window."
  (- (virtual-desktops-get-window-ymax window) (virtual-desktops-get-window-ymin window))
)

;; returns (block-xmin block-ymin block-xmax block-ymax block-width block-height)
(defun virtual-desktops-get-block-dimensions (block)
  "Return the dimensions of a block of windows.
return format: (block-xmin block-ymin block-xmax block-ymax block-width block-height)."
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

(defun virtual-desktops-get-window (x y window-list)
  "Return the window located at (x, y) in window-list."
  (let ((window nil))
	(catch 'break;
	  (dolist (w window-list)
		(if (and (>= x (virtual-desktops-get-window-xmin w))
                 (<= x (virtual-desktops-get-window-xmax w))
                 (>= y (virtual-desktops-get-window-ymin w))
                 (<= y (virtual-desktops-get-window-ymax w)))
            (progn (setq window w)
                   (throw 'break t)))))
	window))


(defun virtual-desktops-split-block-vertically (block)
  "Try to split vertically the block.
In case of success, the return value is a list of two new blocks.
If no split was found, return nil."
  (let* ((dimensions (virtual-desktops-get-block-dimensions block))
         (block-xmin (nth 0 dimensions))
         (block-ymin (nth 1 dimensions))
         (block-xmax (nth 2 dimensions))
         (block-ymax (nth 3 dimensions))
         (block-width (nth 4 dimensions))
         (block-height (nth 5 dimensions)))

    ;;select window to split
    (select-window (window-at block-xmin block-ymin))

    (let ((y (1+ block-ymin)))
      (catch 'break
        (while (< y block-ymax)
          (let* ((w-current (virtual-desktops-get-window block-xmin y block))
                 (total-width (virtual-desktops-get-window-width w-current)))
            (dolist (w-temp block) ;;check all windows
              (unless (equal w-temp w-current) ;;if x-temp is not current windows
                (when (= (virtual-desktops-get-window-ymax w-temp)
                         (virtual-desktops-get-window-ymax w-current)) ;; windows have same bottom
                  (setq total-width (+ total-width
                                       (virtual-desktops-get-window-width w-temp)))))) ;;add window width to total
            (when (>= total-width block-width) ;;we can split
              (setq y (+ y (virtual-desktops-get-window-height w-current))) ;; we split here
              (throw 'break t))
            (setq y (+ y (virtual-desktops-get-window-height w-current)))))) ;;next window

      ;;if we found a split, we split and return 2 blocks
      (if (< y block-ymax)
          (let (block-1 block-2)
            (split-window-vertically (1- (- y block-ymin)))
            (dolist (w block)
              (if (< (virtual-desktops-get-window-ymax w) y)
                  (setq block-1 (cons w block-1))
                (setq block-2 (cons w block-2))))
            (list block-1 block-2))
	    nil))))

(defun virtual-desktops-split-block-horizontally (block)
  "Try to split horizontally the block.
In case of success, the return value is a list of two new blocks.
If no split was found, return nil."
  (let* ((dimensions (virtual-desktops-get-block-dimensions block))
         (block-xmin (nth 0 dimensions))
         (block-ymin (nth 1 dimensions))
         (block-xmax (nth 2 dimensions))
         (block-ymax (nth 3 dimensions))
         (block-width (nth 4 dimensions))
         (block-height (nth 5 dimensions)))

    ;;select window to split
    (select-window (window-at block-xmin block-ymin))

    (let ((x (1+ block-xmin)))
      (catch 'break
        (while (< x block-xmax)
          (let* ((w-current (virtual-desktops-get-window x block-ymin block))
                 (total-height (virtual-desktops-get-window-height w-current)))
            (dolist (w-temp block) ;;check all windows
              (unless (equal w-temp w-current) ;;if x-temp is not current windows
                (when (= (virtual-desktops-get-window-xmax w-temp)
                         (virtual-desktops-get-window-xmax w-current)) ;; windows have same right side
                  (setq total-height (+ total-height
                                       (virtual-desktops-get-window-height w-temp)))))) ;;add window height to total
            (when (>= total-height block-height) ;;we can split
              (setq x (+ x (virtual-desktops-get-window-width w-current))) ;; we split here
              (throw 'break t))
            (setq x (+ x (virtual-desktops-get-window-width w-current)))))) ;;next window

      ;;if we found a split, we split and return 2 blocks
      (if (< x block-xmax)
          (let (block-1 block-2)
            (split-window-horizontally (1- (- x block-xmin)))
            (dolist (w block)
              (if (< (virtual-desktops-get-window-xmax w) x)
                  (setq block-1 (cons w block-1))
                (setq block-2 (cons w block-2))))
            (list block-1 block-2))
	    nil))))

(defun virtual-desktops-split-block (block)
  "Split the block until all blocks are composed of only one window.
Window buffers are set."
  (if (> (safe-length block) 1)
      (let ((result (virtual-desktops-split-block-vertically block)))
        (unless result
          (setq result (virtual-desktops-split-block-horizontally block)))
        (if result
            (progn (virtual-desktops-split-block (car result))
                   (virtual-desktops-split-block (nth 1 result)))
          (error "No split found")))
    (let* ((w (car block)) ;;only one window in list, setting buffer.
           (edges (nth 1 w)))
      (set-window-buffer (window-at (nth 0 edges) (nth 1 edges))
                         (car w)))))



(defun virtual-desktops-create-desktop ()
  "Save windows and buffer configuration and add the new desktop to the list."
  (let ((frame nil)
        (window-listv nil))
    (setq frame (list (frame-width) (frame-height)))

    ;;for each window, starting with minibuffer
    (dolist (window (window-list (selected-frame) t (minibuffer-window)))
      (setq window-listv (cons (list (window-buffer window) (window-edges window)) window-listv)))

  (list frame                              ;;frame
        (reverse window-listv)             ;;windows
        (list (nth 0 (window-edges))       ;;selected window x
              (nth 1 (window-edges))))))   ;;selected window y

;;restore the desired desktop
(defun virtual-desktops-restore (number)
  "Restore the desired desktop."
  (let ((desktop (nth number virtual-desktops-list)))
    (when desktop
      (let ((frame (car desktop))
            (window-listv (nth 1 desktop))
            (select (nth 2 desktop)))
        ;;resize frame
        (set-frame-size (selected-frame) (car frame) (nth 1 frame))

        ;;delete all windows
        (delete-other-windows)

        ;;resize minibuffer
        (let ((mini (minibuffer-window))
              (mini-edges (nth 1 (car window-listv))))
          (window-resize mini (- (- (nth 3 mini-edges) (nth 1 mini-edges))
                                 (window-height mini))))

        ;;let's split windows
        (virtual-desktops-split-block (cdr window-listv))
        (select-window (window-at (car select) (nth 1 select))))
      (setq virtual-desktops-last-frame (selected-frame)))))


;;delete a desktop if it is not the nil desktop
(defun virtual-desktops-delete (number)
  "Delete a desktop if it is not the nil desktop."
  (if (and (< number (safe-length virtual-desktops-list))
		   (> number 0))
	  (setq virtual-desktops-list (delq (nth number virtual-desktops-list) virtual-desktops-list))
	  (message (concat "Cant delete this desktop :" (number-to-string number))))
)


(defun virtual-desktops-update-mode-line ()
  "Update mode line."
  (if (= virtual-desktops-current 0)
	  (setq virtual-desktops-mode-line-string " (D nil) ")
	  (setq virtual-desktops-mode-line-string (concat " (D " (number-to-string virtual-desktops-current) ") ")))
  (force-mode-line-update)
)

(defun virtual-desktops-update-if-needed ()
  "Test if the desktop must be updated. It must be updated if
virtual-desktops-auto-update is set and if we are not in a new frame."
  (when (and virtual-desktops-auto-update
             (or (equal nil virtual-desktops-last-frame)
                 (equal (selected-frame) virtual-desktops-last-frame)))
    (virtual-desktops-update)))

(defun virtual-desktops-insert-desktop-entry (desktop list-entries number)
  "Insert a new entry for list buffer.
The new list is returned."
  (let ((buffer-list "| "))
    (dolist (w (cdr (nth 1 desktop)))
        (setq buffer-list (concat buffer-list
                                  (buffer-name (car w))
                                  " | ")))
    (append list-entries
            (list (list number (vector (number-to-string number) buffer-list))))))

(defun virtual-desktops-list-entries ()
  "Return the list of desktops for list-mode."
  (let ((list-entries (list (list 0 ["0" "nil"])))
        (num 1))
    (dolist (d (cdr virtual-desktops-list))
      (setq list-entries (virtual-desktops-insert-desktop-entry d list-entries num))
      (setq num (1+ num)))
    list-entries))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;								Interactive functions									;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun virtual-desktops-add (nb-desktops)
  "Create a new desktop with current window an buffer configuration."
  (interactive "P")
  (if virtual-desktops-mode
	  (progn (setq virtual-desktops-list (append virtual-desktops-list (list (virtual-desktops-create-desktop))))
			 (setq virtual-desktops-current (1- (safe-length virtual-desktops-list)))
             (setq virtual-desktops-last-frame (selected-frame))
			 (virtual-desktops-update-mode-line)
             (when (and (integerp nb-desktops)
                        (> nb-desktops 1))
               (virtual-desktops-add (1- nb-desktops))))
	  (message "virtual-desktops-mode must be enabled"))
)


(defun virtual-desktops-update ()
  "Update current desktop with current window an buffer configuration."
  (interactive)
  (if virtual-desktops-mode
	  (progn (if (not (= virtual-desktops-current 0))
				 (let ((desktop (virtual-desktops-create-desktop)))
                   (setq virtual-desktops-last-frame (selected-frame))
				   (setcar (nthcdr virtual-desktops-current virtual-desktops-list) desktop))))
  	  (message "virtual-desktops-mode must be enabled")))

(defun virtual-desktops-del ()
  "Delete current desktop."
  (interactive)
    (if virtual-desktops-mode
		(progn (virtual-desktops-delete virtual-desktops-current)
			   (setq virtual-desktops-current 0)
               (setq virtual-desktops-last-frame (selected-frame))
			   (virtual-desktops-update-mode-line))
	    (message "virtual-desktops-mode must be enabled")))

(defun virtual-desktops-del-specific (desktop)
  "Delete a specific desktop."
  (interactive "Ndesktop to delete: ")
  (if virtual-desktops-mode
      (when (and (< desktop (safe-length virtual-desktops-list))
                 (yes-or-no-p (concat "Really delete desktop "
                                      (number-to-string desktop)
                                      "? ")))
        (virtual-desktops-delete desktop)
        (if (= virtual-desktops-current desktop)
            (setq virtual-desktops-current 0)
          (when (> virtual-desktops-current desktop)
            (setq virtual-desktops-current (1- virtual-desktops-current))))
        (setq virtual-desktops-last-frame (selected-frame))
        (virtual-desktops-update-mode-line))
    (message "virtual-desktops-mode must be enabled")))

(defun virtual-desktops-next ()
  "Go to next desktop."
  (interactive)
  (if virtual-desktops-mode
	  (if (not (active-minibuffer-window))
		  (progn (virtual-desktops-update-if-needed)
				 (setq virtual-desktops-current (1+ virtual-desktops-current))
				 (if (>= virtual-desktops-current (safe-length virtual-desktops-list))
					 (setq virtual-desktops-current 0))
				 (virtual-desktops-restore virtual-desktops-current)
				 (virtual-desktops-update-mode-line))
		  (message "pb minibuffer"))
	  (message "virtual-desktops-mode must be enabled"))
)

(defun virtual-desktops-prev ()
  "Go to previous desktop."
  (interactive)
  (if virtual-desktops-mode
	  (if (not (active-minibuffer-window))
		  (progn (virtual-desktops-update-if-needed)
				 (setq virtual-desktops-current (1- virtual-desktops-current))
				 (if (< virtual-desktops-current 0)
					 (setq virtual-desktops-current (1- (safe-length virtual-desktops-list))))
				 (virtual-desktops-restore virtual-desktops-current)
				 (virtual-desktops-update-mode-line))
		  (message "pb minibuffer"))
	  (message "virtual-desktops-mode must be enabled"))
)

(defun virtual-desktops-goto (number &optional dont-update)
  "Goto specific desktop.
If dont-update is not nil, current desktop won't be updated even if
virtual-desktops-auto-update is set."
  (interactive "Ndesktop to display: ")
  (if virtual-desktops-mode
	  (if (not (active-minibuffer-window))
		  (progn (unless dont-update
                   (virtual-desktops-update-if-needed))
                 (setq virtual-desktops-current number)
				 (virtual-desktops-restore virtual-desktops-current)
				 (virtual-desktops-update-mode-line)))
	  (message "virtual-desktops-mode must be enabled")))

(defun virtual-desktops-list ()
  "List all desktops."
  (interactive)
  (if virtual-desktops-mode
	  (let ((buffer (get-buffer-create virtual-desktops-list-buffer-name)))
        (switch-to-buffer buffer)
        (virtual-desktops-list-mode)
        (tabulated-list-print t))
    (message "virtual-desktops-mode must be enabled")))

;;virtual-desktops.el ends here
