;;; a-menu.el --- create a menu from a specified directory

;; Copyright (C) 2001-2006  by Seiji Zenitani

;; Author: Seiji Zenitani <zenitani@mac.com>
;; $Id: a-menu.el 33 2006-10-24 21:30:35Z zenitani $
;; Keywords: tools, unix
;; Created: 2001-08-01
;; Compatibility: Emacs 21, 20
;; URL(en): http://homepage.mac.com/zenitani/comp-e.html
;; URL(jp): http://homepage.mac.com/zenitani/elisp-j.html#a-menu

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This package provides `a-menu' function.
;; It enables you to build your own menu in the menu bar,
;; based on the structure of the specified directory.
;;
;; Let us assume the following files and directories:
;;
;;     ~/directory/
;;     ~/directory/Test
;;     ~/directory/Test/Item_1.el
;;     ~/directory/Test/Item_2.el.
;;
;; The code
;;
;;     (a-menu "~/directory/Test")
;;
;; will create "Test" menu in the menu bar, and the names of
;; the *.el files are shown in it. Choosing one of menu items loads
;; the relevant *.el file, and so emacs executes lisp codes in it.
;; To create a hierarchical structure in the menu, just make subdirectories.


;;; Code:

(defun a-menu (dir &optional updir)
  "Enable you to build your own menu in the menu bar,
based on the directory name: DIR.

The second argument UPDIR is internally used. Do not use it."
  (interactive "D")

  ;; remove trailing slash
  (if (string-match "/$" dir)
      (setq dir (substring dir 0 -1)))

  ;; check arg
  (if (equal "" dir) (error (format "invalid argument")))
  (setq dir (expand-file-name dir))
  (or (file-directory-p dir)(error (format "%s: not directory" dir)))

  (let ((nondir (file-name-nondirectory dir))
        (menu (concat "menu-bar-" dir "-menu")))

    ;; keymap
    (eval
     (read
      (concat
       "(progn "
       "(setq " menu " (make-sparse-keymap))"
       "(define-key " menu " [a-menu-" dir "-refresh] "
       " '(\"Refresh This Menu\" . (lambda() (interactive)"
       "(a-menu \"" dir "\" " (if updir (format "%s" updir) "nil") "))))"
       "(define-key " menu " [a-menu-" dir "-last-separator] '(\"---\"))"
       ")\n"
       )))
    
    (let ((el (reverse (directory-files dir t))))
      (while el
        (let ((item (car el)))
;          (message (format "item: %s" item))
          (if (file-readable-p item)
              (cond
               ((string-match "^\\." (file-name-nondirectory item))
;                (message (format "skipped: %s" item))
                t)
               ((file-directory-p item)
                (a-menu item dir)
;                (message (format "dir: %s" item))
                )
               ((string-match "\\.el$" item)
                (let* ((item-name (file-name-sans-extension
                                   (file-name-nondirectory item)))
                       (menu-name item-name))
                  (while (string-match "_" menu-name)
                    (setq menu-name (replace-match " " t nil menu-name)))
                  (eval
                   (read
                    (format "(define-key %s [a-menu-%s]
 '(\"%s\" .(lambda()(interactive)(load-file \"%s\"))))\n"
                            menu item-name menu-name item)
                    ))
                  ))
               )))
        (setq el (cdr el))
        ))
    (let ((menu-name nondir))
      (while (string-match "_" menu-name)
        (setq menu-name (replace-match " " t nil menu-name)))
      (if updir
          (eval
           (read
            (concat
             "(define-key menu-bar-" updir "-menu [a-menu-" nondir "] "
             "(list 'menu-item \"" menu-name "\" menu-bar-" dir "-menu" "))"
             )))
        (eval
         (read
          (concat
           "(progn"
           "(global-set-key [menu-bar a-menu-" dir "] (cons \""
           menu-name "\" " menu "))\n"
           "(setq menu-bar-final-items (cons 'a-menu-" dir
           " menu-bar-final-items))\n)"
           )))
        ))
    
    ))

(provide 'a-menu)

;;; a-menu.el ends here.
