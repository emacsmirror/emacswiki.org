;;; bookmark-add.el - creates the buffer for work with bookmarks.
;;
;; $Id$
;;
;; Time-stamp: <28-10-2004 00:25:00>
;;
;; Copyright (C) 2004 Eugene V. Markov

;; Author: Eugene V. Markov
;; Version: $Revision$
;; Keywords: bookmarks, placeholders, annotations

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; `bookmark-open-in-symply-buffer' will generate buffer named
;; *Bookmark list*, which shows bookmarks from .emacs.bmk.
;; It has possibilities for deleting bookmarks from list,
;; saveing bookmarks list to file and loading  bookmarks list
;; from file.
;;
;; Install.
;;
;; (load "bookmark-add")
;; (global-set-key [?\C-c ?b ?l] 'bookmark-open-in-simply-buffer)
;; (global-set-key [?\C-c ?b ?m] 'bookmark-set-add)
;; (global-set-key [?\C-.] 'bookmark-jump-next-cyclic)
;; (global-set-key [?\C-,] 'bookmark-jump-prev-cyclic)
;; (global-set-key [?\C-'] 'bookmark-jump-backwards)
;;
;; More commentary:
;;
;; `bookmark-open-in-simply-buffer' - switch to buffer named
;; *Bookmark list*. If to hit key Enter on bookmark region will
;; pass on a corresponding file. If to hit key Delete (or Ctrl-d) on
;; bookmark region remove this bookmark from bookmarks list.
;; If to hit key 'q' the buffer will be closed.
;; `bookmark-set-add' - add this bookmark to bookmarks list.
;; To use history, it is necessary to press buttons Up and Down.
;; The first pressing Up inserts expression which is near to a point.
;; `bookmark-jump-next-cyclic' - cyclic moving on bookmarks forward.
;; `bookmark-jump-prev-cyclic' - cyclic moving on bookmarks backward.
;; `bookmark-jump-backwards' - will move to last cursor position right
;;  after uses of commands `bookmark-jump-next-cyclic' and
;; `bookmark-jump-prev-cyclic'.

(require 'bookmark)
(require 'wid-edit)

(defvar bookmark-add-version "$Revision$"
  "The version of bookmark-add currently loaded")

(defgroup bookmark-add nil
  "Setting, annotation and jumping to bookmarks."
  :group 'bookmark)

(defcustom bookmark-add-default-file-name
  ".emacs.bmk"
  ""
  :type 'file
  :group 'bookmark-add)


(defface bookmark-simply-buffer-face
  '((t (:background "light grey" :foreground "red" :bold t)))
  ""
  :group 'bookmark-add)

(defvar evm-current-bookmark-path nil
  "Path to current bookmark file."
  )


(defun bookmark-open-in-simply-buffer ()
  "Allow to open bookmark list in simply buffer."
  (interactive)
  (if (bookmark-alist-exists-p)
      (if (and (get-buffer "*Bookmark list*")
               (get-buffer-window (get-buffer "*Bookmark list*")))
          (select-window (get-buffer-window (get-buffer "*Bookmark list*")))
	  (setq bookmark-windows-configure (current-window-configuration))
	  (split-window-vertically)
	  (other-window 1)
	  (with-current-buffer (get-buffer-create "*Bookmark list*")
	    (switch-to-buffer (current-buffer))
	    (kill-all-local-variables)
	    (setq truncate-lines t)
	    (let ((inhibit-read-only t))
		(erase-buffer))
	    (let ((all (overlay-lists)))
		;; Delete all the overlays.
		(mapcar 'delete-overlay (car all))
		(mapcar 'delete-overlay (cdr all)))
	    ;; Insert the dialog header
	    (widget-insert "Click on a text to open it or on Cancel to quit.\n\n")
	    ;; Insert the list of texts as buttons
	    (setq bookmark-max-fnl (let ((l bookmark-alist )(nl 0)(str))
					     (while
						   (setq str (cdr (assoc 'front-context-string
										 (cadr (car l)))))
						 (if (> (length str) nl)
						     (setq nl (length str)))
						 (setq l (cdr l))
						 )
					     (symbol-value 'nl)))
	    (setq bookmark-max-fnl (+ bookmark-max-fnl 2))
	    (mapcar '(lambda (menu-element)
			   (let ((menu-item1 (cdr (assoc 'front-context-string (cadr menu-element))))
				   (menu-item2 (cdr (assoc 'rear-context-string (cadr menu-element))))
				   (file-path (copy-sequence
						   (cdr (assoc 'filename (cadr menu-element)))))
				   (name (copy-sequence (car menu-element)))
                           bp ep ovl)
                       (setq bp (point))
			     (widget-insert " Bookmark ")
                       (let (bp ep ovl)
                         (setq bp (point))
                         (widget-insert name)
                         (setq ep (point))
                         (setq ovl (make-overlay bp ep))
                         (overlay-put ovl 'face font-lock-function-name-face))
			     (widget-insert " from file\n")
			     (widget-insert " ")
                       (let (bp ep ovl)
                         (setq bp (point))
                         (widget-insert file-path)
                         (setq ep (point))
                         (setq ovl (make-overlay bp ep))
                         (overlay-put ovl 'face font-lock-keyword-face))
			     (widget-insert "\n")
			     (widget-create 'push-button
						  :button-face 'bookmark-simply-buffer-face
						  :tag (concat " " menu-item2 menu-item1 "\n")
						  :help-echo (concat "Open bookmark " name)
						  :format "%[%t%]"
						  :notify 'bookmark-open-in-simply-buffer-action
						  menu-element)
			     (widget-insert "\n")
                       (setq ep (point))
                       (setq ovl (make-overlay bp ep))
                       (overlay-put ovl 'bookmark-add-item-region t)
			     ))
			bookmark-alist)
	    (widget-insert "\n")
	    ;; Insert the Cancel button
	    (widget-create 'push-button
				 :notify 'bookmark-open-in-simply-buffer-cancel
				 :help-echo "Quit from this buffer."
				 "Cancel")
	    (widget-insert " ")
	    ;; Insert the Save button
	    (widget-create 'push-button
				 :notify 'bookmark-save-wrapper
				 :help-echo "Save bookmark list to file."
				 "Save")
	    (widget-insert " ")
	    ;; Insert the Load button
	    (widget-create 'push-button
				 :notify 'bookmark-load-wrapper
				 :help-echo "Load bookmark list from file."
				 "Load")
	    (widget-insert " ")
	    ;; Insert the Bookmark path button
	    (widget-create 'push-button
				 :notify 'evm-bookmark-path-to-current
				 :help-echo "Path to bookmark file."
				 "Bookmark path")
	    (let ((map (copy-keymap widget-keymap)))
; 		(define-key map [up] 'widget-backward)
; 		(define-key map [down] 'widget-forward)
		(define-key map [delete] 'bookmark-delete-wrapper)
		(define-key map [?\C-d] 'bookmark-delete-wrapper)
		(define-key map [?q] 'bookmark-open-in-simply-buffer-cancel)
		(define-key map "\e\e" 'bookmark-open-in-simply-buffer-cancel)
		(use-local-map map))
	    (widget-setup)
	    (goto-char (point-min))))))


(defun bookmark-delete-wrapper ()
  (interactive)
  (let ((ovls (overlays-at (point)))
        (inhibit-read-only t))
    (while ovls
      (if (and (overlay-get (car ovls) 'bookmark-add-item-region)
               (widget-tabable-at))
          (progn
            (bookmark-delete (car (widget-value (widget-tabable-at))))
            (delete-region (overlay-start (car ovls))
                           (overlay-end (car ovls)))
            (setq ovls nil)))
      (setq ovls (cdr ovls)))))


(defun evm-bookmark-path-to-current  (&rest ignore)
  (interactive)
  (message "%s" evm-current-bookmark-path))


(defun bookmark-open-in-simply-buffer-action (widget &rest ignore)
   (kill-buffer (current-buffer))
   (set-window-configuration bookmark-windows-configure)
   (bookmark-delete (car (widget-value widget)))
   (setq bookmark-alist (cons (widget-value widget) bookmark-alist))
   (bookmark-jump (car (widget-value widget)))
  )

(defun bookmark-open-in-simply-buffer-cancel (&rest ignore)
  (interactive)
  (kill-buffer (current-buffer))
  (set-window-configuration bookmark-windows-configure)
  (message "Command canceled."))


(defun bookmark-alist-exists-p ()
  ;; &#1101;&#1090;&#1086; &#1076;&#1083;&#1103; &#1079;&#1072;&#1075;&#1088;&#1091;&#1079;&#1082;&#1080; &#1089; &#1087;&#1088;&#1086;&#1074;&#1077;&#1088;&#1082;&#1086;&#1081;, &#1072; &#1085;&#1077; &#1076;&#1083;&#1103; &#1087;&#1088;&#1086;&#1074;&#1077;&#1088;&#1082;&#1080;.
  (if (and (boundp 'bookmark-alist)
           (not evm-current-bookmark-path))
	(if (and (boundp 'desktop-dirname)
		   desktop-dirname
		   (file-exists-p (concat desktop-dirname ".emacs.bmk")))
	    (progn
		(bookmark-load (concat desktop-dirname ".emacs.bmk") t)
            (setq evm-current-bookmark-path
                  (concat desktop-dirname ".emacs.bmk"))
		t)
	  (if (file-exists-p (expand-file-name "~/.emacs.bmk"))
		(progn
		  (bookmark-load (expand-file-name "~/.emacs.bmk") t)
              (setq evm-current-bookmark-path
                    (expand-file-name "~/.emacs.bmk"))
		  t)
	    nil))
    t))


(defun bookmark-set-add ()
  (interactive)
  (if (bookmark-alist-exists-p)
	(let ((l bookmark-alist) (s) (sxp))
	  (while l
	    (setq s (nconc s (list (caar l))))
	    (setq l (cdr l)))
	  (setq l minibuffer-history)
	  (if (setq sxp (thing-at-point 'sexp))
		(setq s (nconc (list sxp) s)))
	  (setq minibuffer-history s)
	  (unwind-protect
		(bookmark-set)
	    (setq minibuffer-history l))
	  )))

(defun bookmark-jump-next-cyclic()
  (interactive)
  (if (bookmark-alist-exists-p)
	(let* ((keys (recent-keys))
		 (len (length keys))
		 (key1 (if (> len 0) (elt keys (- len 1)) nil))
		 (key2 (if (> len 1) (elt keys (- len 2)) nil))
		 n)
	  (if (eq key1 key2)
		(progn
		  (setq n (length bookmark-alist))
		  (nconc bookmark-alist bookmark-alist)
		  (setq bookmark-alist (cdr bookmark-alist))
		  (setcdr (nthcdr (- n 1) bookmark-alist) nil)
		  (bookmark-jump (car (car bookmark-alist)))
		  )
	    (progn
		(if (not (equal key2
				    (elt (car (where-is-internal 'bookmark-jump-prev-cyclic)) 0)))
		    (progn
			(setq bookmark-windows-configure
				(cons (current-window-configuration) (point)))))
		(bookmark-jump (car (car bookmark-alist))))))))


(defun bookmark-jump-prev-cyclic()
  (interactive)
  (if (bookmark-alist-exists-p)
	(let* ((keys (recent-keys))
		 (len (length keys))
		 (key1 (if (> len 0) (elt keys (- len 1)) nil))
		 (key2 (if (> len 1) (elt keys (- len 2)) nil))
		 n)
	  (if (eq key1 key2)
		(progn
		  (setq n (length bookmark-alist))
		  (setq bookmark-alist (reverse bookmark-alist))
		  (nconc bookmark-alist bookmark-alist)
		  (setq bookmark-alist (cdr bookmark-alist))
		  (setcdr (nthcdr (- n 1) bookmark-alist) nil)
		  (bookmark-jump (car (car bookmark-alist)))
		  (setq bookmark-alist (reverse bookmark-alist))
		  )
	    (progn
		(if (not (equal key2
				    (elt (car (where-is-internal 'bookmark-jump-next-cyclic)) 0)))
		    (progn
			(setq bookmark-windows-configure
				(cons (current-window-configuration) (point)))))
		(bookmark-jump (car (car (reverse bookmark-alist)))))))))


(defun bookmark-jump-backwards()
  (interactive)
  (let* ((keys (recent-keys))
	   (key (if (> (length keys) 1) (elt keys (- (length keys) 2)) nil)))
    (if (or (eq key
		    (elt (car (where-is-internal 'bookmark-jump-prev-cyclic)) 0))
		(eq key
		    (elt (car (where-is-internal 'bookmark-jump-next-cyclic)) 0)))
	  (progn
	    (set-window-configuration (car bookmark-windows-configure))
	    (goto-char (cdr bookmark-windows-configure))))))


(defadvice bookmark-jump (after bookmark-jump-folding-mode activate)
  (if (and (boundp 'folding-mode) folding-mode)
	(progn
	  (let ((p (point)))
		  (folding-show-current-entry nil t)
		  (goto-char p)))))



(defun bookmark-save-wrapper  (&rest ignore)
  ""
  (interactive)
  (cond
   ((featurep 'evm-dired-ch)
    (evm-pcompleting-minibuffer-file-dired
       "Path to bookmarks file (Save): "
       'bookmark-save-wrapper-fn
       evm-current-bookmark-path))
   ((featurep 'evm-pcomplete)
    (bookmark-save-wrapper-fn
     (evm-pcompleting-minibuffer-dir "Path to bookmarks file (Save): "
                                     evm-current-bookmark-path)))
   (t
    (bookmark-add-default-file-name
     (file (read-file-name "Path to bookmarks file (Save): "
                           evm-current-bookmark-path nil nil
                           ))))))


(defun bookmark-save-wrapper-fn  (path)
  ""
  (when (listp path)
    (setq path (car path)))
  ;;
  (if (and path (not (= (length path) 0)))
      (progn
        (setq path (expand-file-name path))
        (bookmark-save nil path))))


(defun bookmark-load-wrapper  (&rest ignore)
  ""
  (interactive)
  (cond
   ((featurep 'evm-dired-ch)
    (evm-pcompleting-minibuffer-file-dired*
       "Path to bookmarks file (Load): "
       'bookmark-load-wrapper-fn
       evm-current-bookmark-path))
   ((featurep 'evm-pcomplete)
    (bookmark-load-wrapper-fn
     (evm-pcompleting-minibuffer-dir "Path to bookmarks file (Load): "
                                     evm-current-bookmark-path)))
   (t
    (bookmark-add-default-file-name
     (file (read-file-name "Path to bookmarks file (Load): "
                           evm-current-bookmark-path nil nil
                           ))))))


(defun bookmark-load-wrapper-fn  (path)
  ""
  (when (listp path)
    (setq path (car path)))
  ;;
  (if (and path (not (= (length path) 0)))
      (progn
        (setq path (expand-file-name path))
        (bookmark-load path t)
        (setq evm-bookmark-path-to-current path)
        (kill-buffer (current-buffer))
        (set-window-configuration bookmark-windows-configure)
        (bookmark-open-in-simply-buffer))))


(provide 'bookmark-add)

;;; bookmark-add.el ends here.
