;;; pmenu.el --- Easily customizable paged menu system

;; contact me at aiisdead@gmail.com
;; Written 2009

(defconst pmenu "$Revision: 1.0 $"
  "`pmenu' version number.")

;; This software is provided as-is, without express or implied
;; warranty.  Permission to use, copy, modify, distribute or sell this
;; software, without fee, for any purpose and by any individual or
;; organization, is hereby granted, provided that the above copyright
;; notice and this paragraph appear in all copies.

;;; Commentary:

;; This is a simple means of associating often used functionality with
;; keystrokes in a visual manner.

;; It combines the global-set-key idea and the menu-bar idea and is
;; specifically intended where functionality is used rarely enough
;; that one might forget it but often enough to make a keystroke
;; worthwhile. Also it is specifically intended to be easy to add and
;; categorise menu items with a nice long description if needed.
;;
;; Note that it does not use or interfere with any of the existing
;; menu functionality, it is completely stand-alone.

;; This package consists of two file, this one pmenu.el which defines
;; the base functionality, and pmenu-config.el where the menus are
;; defined, which includes the main menu which is stored in a variable
;; pmenu-items.

;; The menu is displayed in a new buffer called *pmenu* which is
;; buried after use.

;; All menu items are selected via keystrokes, which can be multiple
;; characters, and are associated as keymaps on the buffer Menus can
;; beget other menus, which simply replace the existing menu.

;; Menu functionality is defined as lambdas. Just before executing the
;; function, the previous state is restored so that the lambda is
;; executed on the original buffer in the state it was in.

;; Customization: 

;; None, except the ability to define menus. See the example
;; pmenu-config.el for examples of creating menus.

;; Special menu keys:

;; q closes the menu.

;; INSTALLATION:

;; Just copy the files into site-lisp and load them. There is no 'require needed.
;; For example:
;;(load-file "site-lisp/pmenu.el")
;;(load-file "site-lisp/pmenu-config.el")

;; To launch the menu use the command pmenu-go, which should probably be
;; bound to some easy to reach key, for example: 
;; (global-set-key (kbd "C-,") 'pmenu-go)

;; If you change pmenu-config.el, which it is expected will happen
;; often, simply re-executing (load-file "site-lisp/pmenu-config.el")
;; or (eval-buffer) from inside it will refresh the menus.


;;; Code:

(defvar pmenu-buffer-name "*pmenu*"
  "pmenu buffer name.")

(defun pmenu-create-menu-separator (title)
  (list "" (concat "\n" title "\n--------\n") '(print "separator")))

(defun pmenu-create-menu-item (key title action)
  (list key (concat key " : " title) action))

(defvar pmenu-keymap nil)

(defun pmenu-print-menu-for-item (item)
  (let ((point-before (point-max)))
    (insert (car (cdr item)))
    (insert "\n")))

(defun pmenu-go ()
  (interactive)
  (pmenu-prepare-buffer pmenu-items))

(defun pmenu-prepare-buffer (menu-items)
  (window-configuration-to-register 'z)
  (get-buffer-create pmenu-buffer-name)
  (set-buffer pmenu-buffer-name)
  (setq buffer-read-only nil)
  (delete-other-windows)
  (erase-buffer)
  (kill-all-local-variables)

  (mapcar 'pmenu-print-menu-for-item menu-items)
  (widget-insert "\n ? ")

  (switch-to-buffer pmenu-buffer-name)
  (kill-all-local-variables)
  (setq major-mode 'pmenu-mode)
  (setq mode-name "pmenu")
  (set-syntax-table text-mode-syntax-table)
  (pmenu-create-modemap menu-items)
  (use-local-map pmenu-keymap)
  (setq overlay-arrow-position nil))

(defun pmenu-quit ()
  "Quit pmenu buffer."
  (interactive)
  (kill-buffer nil)
  (bury-buffer)
  (jump-to-register 'z))

(defun pmenu-create-keymap-lambda (item)
  `(lambda (&rest ignore) (interactive) 
     (bury-buffer)
     (jump-to-register 'z)
     (let ((command ,(car (cdr (cdr item)))))
       (eval command))))

(defun pmenu-create-keymap-for-item (item map)
  (define-key map (car item) (pmenu-create-keymap-lambda item)))

(defun pmenu-create-modemap (menu-items)
  (let ((map (make-keymap)))
    (define-key map "q" 'pmenu-quit)
    (mapcar '(lambda (item) (pmenu-create-keymap-for-item item map)) menu-items)
    (setq pmenu-keymap map)))




