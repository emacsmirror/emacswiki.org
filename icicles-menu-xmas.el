;;; icicles-menu-xmas.el --- xemacs port (beta) of icicles-menu.el

;;; icicles-menu.el --- Execute menu items as commands, with completion.
;;
;; Filename: icicles-menu.el
;; Description: Execute menu items as commands, with completion.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2005, Drew Adams, all rights reserved.
;; Created: Fri Aug 12 17:18:02 2005
;; Version: 22.0
;; Last-Updated: Fri Nov 18 11:05:53 2005 (-28800 Pacific Standard Time)
;;           By: dradams
;;     Update #: 263
;; Keywords: menu-bar, menu, command, help, abbrev, minibuffer, keys,
;;           completion, matching, local, internal, extensions,
;; Compatibility: GNU Emacs 20.x, GNU Emacs 21.x, GNU Emacs 22.x
;;
;; Features that might be required by this library:
;;
;;   `subr-21'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  Execute menu items as commands, with completion.
;;
;;  Type a menu item.  Completion is available.  The corresponding
;;  command is executed.
;;
;;  Put this in your init file (~/.emacs):
;;
;;    (require 'icicles-menu)
;;
;;  Suggested binding:
;;
;;    (global-set-key [?\e ?\M-x] 'icicle-execute-menu-command)
;;
;;  Consider also replacing `tmm-menu':
;;
;;    (global-set-key [?\M-`] 'icicle-execute-menu-command)
;;
;;  For a powerful and easy-to-use extension to ordinary minibuffer
;;  completion, try library `icicles.el'.  It enhances the
;;  functionality of `icicles-menu.el' in several ways, but it is not
;;  required, to be able to use `icicles-menu.el'.
;;
;;  Note: If you use MS Windows keyboard accelerators, consider using
;;        `icicle-remove-w32-keybd-accelerators' as the value of
;;        `icicle-convert-menu-item-function'.  It removes any
;;        unescaped `&' characters (indicating an accelerator) from
;;        the menu items.  A library that adds keyboard accelerators
;;        to your menu items is `emacsw32-menu.el', by Lennart Borgman
;;        (<lennart.borgman.073@student.lu.se>).
;;
;;
;;  To Do?
;;  ------
;;
;;  1. Provide an option to sort by menu-bar order, instead of
;;     alphabetically.
;;  2. Echo key bindings for each completed menu item.
;;
;;  3. Maybe use tmm-get-bind?
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;; 2005/11/08 dadams
;;     icicle-execute-menu-command: 
;;       Reset icicle-menu-items-alist in unwind-protect.
;;       Fix for dynamic menus Select and Paste, Buffers, and Frames: 
;;         Treat special cases of last-command-event.
;;     icicle-get-overall-menu-item-alist: setq result of sort.
;; 2005/11/05 dadams
;;     Replaced icicle-menu-items with icicle-menu-items-alist (no need for both).
;;     icicle-execute-menu-command: Set, don't bind icicle-menu-items-alist.
;; 2005/08/23 dadams
;;     icicle-execute-menu-command: renamed alist to icicle-menu-items-alist, so can
;;       refer to it unambiguously in icicle-help-on-candidate (in icicles.el).
;; 2005/08/19 dadams
;;     Added: icicle-convert-menu-item-function, icicle-remove-w32-keybd-accelerators,
;;            icicle-escape-w32-accel.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;;; User Options -------------------------------------------

(defgroup icicles nil
    "Minibuffer input completion and cycling of completion candidates."
      :prefix "icicle-"
      :group 'completion :group 'convenience)

(defvar icicle-menu-items nil
  "Alist of menu-items and their corresponding commands.")



(defun icicle-execute-menu-command ()
  "Execute a menu-bar menu command.
Type a menu item.  Completion is available."
  (interactive)
  (let* (;; `icicle-menu-items-alist' is used also in `icicle-help-on-candidate',
         ;; defined in `icicles.el'.
         (icicle-menu-items-alist (icicle-get-a-menu-item-alist current-menubar))
         (menu-item (completing-read "Menu command: " icicle-menu-items-alist))
         (cmd (cdr (assoc menu-item icicle-menu-items-alist))))
    (cond ((listp cmd) (eval cmd))
	  ((commandp cmd) (command-execute cmd))
	  ((symbolp cmd) (funcall cmd))
	  (t (error "No such menu command")))))
    

(defun icicle-get-a-menu-item-alist (menu)
  "Alist of pairs (MENU-ITEM . COMMAND) defined by KEYMAP.
KEYMAP is any keymap that has menu items.
MENU-ITEM is a menu item, with ancestor-menu prefixes.
  Example: `(\"Files > Insert File...\" . insert-file)'.
COMMAND is the command  bound to the menu item."
  (setq icicle-menu-items nil)
  (icicle-get-a-menu-item-alist-1 menu)
  (nreverse icicle-menu-items))

(defun icicle-menu-get-keyword (menu-item keyword &optional defret)
  "From MENU-ITEM get KEYWORD value.
If KEYWORD not found DEFRET returned."
  (let* ((mi-list (if (listp menu-item) menu-item (append menu-item nil)))
         (ckword (car mi-list))
         (kw-retval defret))
    (while mi-list
      (if (eq ckword keyword)
          (progn
            (setq kw-retval (cadr mi-list))
            (setq mi-list nil)))        ; break
      (setq mi-list (cdr mi-list))
      (setq ckword (car mi-list)))
    kw-retval))


(defun icicle-menu-remove-keyword (menu-item keyword)
  "From MENU-ITEM get KEYWORD value.
If KEYWORD not found DEFRET returned."
  (let* ((mi-list (if (listp menu-item) menu-item (append menu-item nil)))
         (ckword (car mi-list))
         (kw-retval nil))
    (while mi-list
      (if (not (eq ckword keyword))
          (progn
            (setq kw-retval (append kw-retval (list ckword)))
	    (setq mi-list (cdr mi-list))
	    (setq ckword (car mi-list)))
	(setq mi-list (cddr mi-list))
	(setq ckword (car mi-list))))
    kw-retval))

(defun icicle-get-a-menu-item-alist-1 (menu &optional root)
  "Helper function for `icicle-get-a-menu-item-alist'.
This calls itself recursively, to process submenus."
  (let ((scan menu))
    (setq root (or root))               ; nil, for top level.
    (while (consp scan)
      (let ((defn (car scan))
	    composite-name)
	;; Get REAL-BINDING for the menu item.
	;;(debug)
	(when (and (listp defn) (icicle-menu-get-keyword defn :filter))
	  (when (stringp (car defn)) 
	    (setq composite-name (concat root (and root " > ") (car defn)))
	    (setq composite-name (replace-in-string composite-name "%_" "" composite-name)))
	  (setq defn (funcall (icicle-menu-get-keyword defn :filter) defn)))
	(cond
	 ((vectorp defn)
	  ;;(debug)
	  (let* ((included (eval (icicle-menu-get-keyword defn :included t)))
		 (active (if included (eval (icicle-menu-get-keyword defn :active t)) nil))
		 (name (if (not (equal "" (elt defn 0))) 
			   (elt defn 0)
			 (elt defn (1- (length defn))))))
	    
	    (if active
		(progn
		  (setq composite-name (concat root (and root " > ") name))
		  (setq composite-name (replace-in-string composite-name "%_" "" composite-name))
		  (setq defn (elt defn 1)))))))
	
	(when (listp defn)
	  (if (stringp (car defn))
	      (progn
		(setq composite-name (concat root (and root " > ") (car defn)))
		(setq composite-name (replace-in-string composite-name "%_" "" composite-name))
		(if (not (string-match "(Customize)" composite-name))
		    (icicle-get-a-menu-item-alist-1 (cdr defn) composite-name)))
	    (icicle-get-a-menu-item-alist-1 defn composite-name)))

	  
	
	;; Add menu item and command pair to `icicle-menu-items' alist.
	;; (`name' is bound in `icicle-get-a-menu-item-alist'.)
	(when (and root 
		   (or (and (listp defn)
			    (not (or (stringp (car defn))
				     (vectorp (car defn)))))
		       (commandp defn)
		       (symbolp defn))
		   composite-name)
	  (setq icicle-menu-items
		(cons
		 (cons composite-name
		       defn)
		 icicle-menu-items))))
      (when (consp scan) (setq scan (cdr scan))))
    icicle-menu-items)) 


(provide 'icicles-menu-xmas)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; icicles-menu-xmas.el ends here
