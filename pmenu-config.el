;;; pmenu-config.el --- Customisation file. Pairs with pmenu.el

;; contact me at aiisdead@gmail.com
;; Written 2009

;; This software is provided as-is, without express or implied
;; warranty.  Permission to use, copy, modify, distribute or sell this
;; software, without fee, for any purpose and by any individual or
;; organization, is hereby granted, provided that the above copyright
;; notice and this paragraph appear in all copies.

;; This is a sample configuration file. The only thing it has to
;; contain is a definition for pmenu-items. However it will typically
;; contain a number of menu definitions and possibly some helper
;; functions depending on personal requirements.

;; After changing this file, reload it (for example M-x eval-buffer) to
;; refresh the menus.


;; Menus are defined as lists, which are built up using one of 2 functions: 

;;    (pmenu-create-menu-separator "some name")
;; Defines an arbitrary string, useful for spacing or menu headings.

;;    (pmenu-create-menu-item "abc" "menu item name" '(my-function))
;; Defines a menu item. The first argument is the keystroke, the
;; second is the item description (the keystroke is also automatically
;; displayed), the third is a lambda to run for this item.

;; To create a sub-menu, set the lambda to use 
;;  '(pmenu-prepare-buffer pmenu-my-submenu)
;; where pmenu-my-submenu is a variable defined in
;; exactly the same way as the main menu.

;; Note that a menu item 'q' is built-in and quits the menu.

;; The examples shown below are pretty simple and serve only to
;; demonstrate some ideas. Generally any function which is used fairly
;; frequently but not enough to have its own key-binding is a useful
;; candidate.

;; Menu items can have multiple keystrokes, and can launch new
;; menus. So for example if you wanted to run a build in two ways, you
;; could either have 2 menu items with keystrokes "bu1" and "bu2", or
;; you could have a menu item with keystroke "bu" which launched a new
;; menu with 2 items "1" and "2". The final function would still be
;; accessed by the typing "bu1" or "bu2", but the second variant would
;; show an additional menu. This is purely a matter of personal
;; preference.

;;;;;;;;;;;;;;;;;;;;;
;;;; Main Menu definition : Note not all these items will actually
;;;; work because of path dependencies etc, customise to suit your
;;;; environment.

(setq pmenu-items
      (list
       (pmenu-create-menu-separator "Build main code")
       (pmenu-create-menu-item "b" "Ant build default" '(jde-ant-build "/home/me/dev/build.xml" "clean-build compile-test" nil))

       (pmenu-create-menu-separator "Stuff")
       (pmenu-create-menu-item "e" "Goto my site-list dir" '(dired "/home/me/site-lisp"))
       (pmenu-create-menu-item "s" "A submenu" '(pmenu-prepare-buffer pmenu-my-submenu))
       ))

;;;; Submenu definitions

(setq pmenu-my-submenu
      (list
       (pmenu-create-menu-separator "dev")
       (pmenu-create-menu-item "j" "dev java file" '(find-name-dired "/home/me/dev/" (concat (read-from-minibuffer "File name? ") "*java")))
       (pmenu-create-menu-item "db" "database" '(splodge-open-some-db "user" "password" "localhost" "db_name"))
       ))

;;;;;;;;
;; Helper functions, again just examples.

(defun splodge-open-some-db (user password host dbname)
  (setq sql-user user)
  (setq sql-password password)
  (setq sql-database dbname)
  (setq sql-server host)
  (sql-mysql))

