;;; mule-menu.el --- Add multiple language support for menubar

;; Copyright 2006 Ye Wenbin
;;
;; Author: wenbinye@163.com
;; Time-stamp: <Ye Wenbin 2006-12-25 22:35:59>
;; Version: $Id: mule-menu.el,v 1.2 2006/12/26 09:32:19 ywb Exp $
;; Keywords: 
;; X-URL: not distributed yet

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'mule-menu)

;;; Todo:
;; 1. Support `tmm-menubar'

;;; FIXME:

;;; How to create a new locale menu:
;; 1. emacs -Q to start up, then load this library, mule-menu.
;;    M-x mule-menu-extract-keymap RET RET. Copy the list in new
;;    displayed buffer to a new file, Add following line to the file:
;;    (setq mule-menu-menu
;;          '((t
;;             (file "File" ...)
;;             (edit "Edit" ...))))
;;
;;    Translate all english string to your language. Load the file.
;;    That's all.
;;
;; 2. The new menu from other library can add after the library loaded
;;    or when the mode turn on. For minor mode, just add following to
;;    `mule-menu-menu':
;;    (add-to-list 'mule-menu-menu
;;                 '(hs-minor-mode
;;                   (Hide/Show ...)))
;;
;;    For major mode, set the new list car part to (eq major-mode 'xx-mode).
;;    for example:
;;    (add-to-list 'mule-menu-menu
;;                 '((eq major-mode 'outline-mode)
;;                   (headings ...)
;;                   (show ...)))
;;    For a library add new menu to exists menu, take template as an
;;    example:
;;    (add-to-list 'mule-menu-menu
;;                 '((feature template)
;;                   (file "File"
;;                         (Open\.\.\.recently\ changed "blahblah")
;;                         ...)))
;;
;; See also chinese-menu.el.

;;; Code:

(provide 'mule-menu)
(eval-when-compile
  (require 'cl))
(require 'tmm)

(defvar mule-menu-menu nil
  "Store the locale menu. The list looks like:
 ((CONDITION MENUS) ...)

When CONDITION can be:
 o permanent
   Which will install when menu updated, and never remove from list
 o (feature CERTAIN-FEATURE)
   Which will install when CERTAIN-FEATURE loaded
 o Form
   If the form eval result is non-nil, the menu will install.
   then the MENUS will remove from mule-menu-menu to avoid install
   repeatly.

The menu labels will change according to the MENUS. 
")

(defvar mule-menu-menu-map (make-sparse-keymap "Set Menu Language"))

(define-key-after mule-menu-keymap [separator-mule-menu]
  '("--") 'mule-diag)
(define-key-after mule-menu-keymap [set-menu-language]
  (cons "Set Menu Language" mule-menu-menu-map)
  'separator-mule-menu)

;; Is this the right coding system
(defvar mule-menu-coding-system (and window-system locale-coding-system)
  "The coding system for encode string displayed in menu")

(defvar mule-menu-languages-list nil)

(defsubst mule-menu-string (string)
  (encode-coding-string string mule-menu-coding-system))

(defun mule-menu-add-language (language)
  "Add new language to menu. LANGUAGE looks like: (LABEL LIBRARY). "
  (define-key mule-menu-menu-map (vector (intern (car language)))
    (list 'menu-item (mule-menu-string (car language))
          `(lambda () (interactive)
             (load ,(cadr language))
             (message nil)))))
(mapc 'mule-menu-add-language mule-menu-languages-list)

(defsubst mule-menu-menu-item (key keymap)
  (cdr (assoc key keymap)))

(defun mule-menu-install-menu ()
  (when mule-menu-menu
    (let ((menu-bar (tmm-get-keybind [menu-bar])))
      (dolist (menu mule-menu-menu)
        (cond ((eq (car menu) 'permanent)
               (mule-menu-install-menu-1 (cdr menu) menu-bar))
              ((and (listp (car menu))
                    (eq (caar menu) 'feature))
               (let ((lib (car (cdar menu))))
                 (if (featurep lib)
                     (mule-menu-install-menu-1 (cdr menu) menu-bar)
                   (eval-after-load (symbol-name lib)
                     `(progn
                        (mule-menu-install-menu-1 (quote ,(cdr menu))
                                                  (tmm-get-keybind [menu-bar]))))))
               (setq mule-menu-menu (delq menu mule-menu-menu)))
              ((ignore-errors (eval (car menu)))
               (mule-menu-install-menu-1 (cdr menu) menu-bar)
               (setq mule-menu-menu (delq menu mule-menu-menu))))))))

(defun mule-menu-install-menu-1 (menu keymap)
  (let (menu-item map str)
    (if (and (symbolp keymap) (boundp keymap))
        (setq keymap (symbol-value keymap)))
    (dolist (item menu)
      (setq menu-item (mule-menu-menu-item (car item) keymap))
      (when (and menu-item (listp menu-item))
        (setq str (cadr item))
        (cond ((stringp str)
               (setq str (mule-menu-string str)))
              ;; if it is a eval form, change to (mule-menu-string EVAL-FORM)
              ((listp str)             
               (setq str `(mule-menu-string ,str))))
        (cond
         ;; (ITEM-STRING . READ-BINDING)
         ((and (stringp (car menu-item)))
          (setcar menu-item str)
          (setq map (cdr menu-item))
          (and (keymapp map) (nthcdr 2 item)
               (mule-menu-install-menu-1 (nthcdr 2 item) map)))
         ;; (MENU-ITEM ITEM-NAME REAL-BINDING . ITEM-PROPERTY-LIST)
         ((eq (car menu-item) 'menu-item)
          (setcar (cdr menu-item) str)
          ;; install help echo
          (let ((menu-help (member :help menu-item))
                help)
            (and menu-help
                 (setq help (cadr (member :help item)))
                 (setcar (cdr menu-help) help)))
          (setq map (nth 2 menu-item))
          (and (keymapp map) (nthcdr 2 item)
               (mule-menu-install-menu-1 (nthcdr 2 item) map))))))))

(add-hook 'menu-bar-update-hook 'mule-menu-install-menu t)

;;;###autoload
(defun mule-menu-extract-keymap (&optional keymap)
  "Get menus from menu bar. The result can add to `mule-menu-menu'
with a little modify."
  (interactive
   (list
    (let ((menu
           (completing-read "Which menu: "
                            (let ((menus '("")))
                              (map-keymap
                               (lambda (env binding)
                                 (if (listp binding)
                                     (setq menus (cons (symbol-name env) menus))))
                               (tmm-get-keybind [menu-bar]))
                              menus) nil t)))
      (if (> (length menu) 0) (intern menu) nil))))
  (require 'pp)
  (let (menu)
    (if keymap
        (setq menu (mule-menu-menu-item keymap (tmm-get-keybind [menu-bar]))
              menu (append (list keymap)
                           (if (eq (car menu) 'menu-item)
                               (cons (cadr  menu)
                                     (mule-menu-extract-keymap-1 (nth
                                                                  2
                                                                  menu)))
                             (cons (car menu)
                                   (mule-menu-extract-keymap-1 (cdr menu))))))
      (setq menu
            (mule-menu-extract-keymap-1 (tmm-get-keybind [menu-bar]))))
    (with-current-buffer (get-buffer-create "*menu*")
      (emacs-lisp-mode)
      (erase-buffer)
      (let ((print-length nil)
            (print-level nil))
        (pp menu (current-buffer)))
      (display-buffer (current-buffer)))))
;;;###autoload
(defun mule-menu-extract-keymap-1 (keymap)
  (let (menu str map)
    (if (and (symbolp keymap) (boundp keymap))
        (setq keymap (symbol-value keymap)))
    (when (and (listp keymap) (keymapp keymap) (cdr-safe keymap))
      (dolist (item (cdr keymap))
        (when (and (listp item) (cdr-safe item)
                   (listp (cdr item)))
          (setq item
                (cond
                 ;; (ITEM-STRING . READ-BINDING)
                 ((and (stringp (cadr item))
                       (not (string-match "^-+$" (cadr item))))
                  (append (list (car item) (cadr item))
                          (progn
                            (setq map (nthcdr 2 item))
                            (and (keymapp map)
                                 (mule-menu-extract-keymap-1 map)))))
                 ;; (MENU-ITEM ITEM-NAME REAL-BINDING . ITEM-PROPERTY-LIST)
                 ((and (eq (cadr item) 'menu-item)
                       (if (stringp (nth 2 item))
                           (not (string-match "^-+$" (nth 2 item)))
                         t))
                  (append (list (car item) (nth 2 item))
                          (let* ((plist (cdr-safe (nthcdr 4 item)))
                                 (help (and plist (listp plist)
                                            (plist-get plist :help))))
                            (and help (list :help help)))
                          (progn (setq map (nth 3 item))
                                 (and (keymapp map)
                                      (mule-menu-extract-keymap-1 map)))))))
          (and item (setq menu (cons item menu)))))
      (nreverse menu))))

;;; mule-menu.el ends here
