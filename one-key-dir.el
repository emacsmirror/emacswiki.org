;;; one-key-dir.el --- Functions for quickly navigating directory trees with one-key menus

;; Filename: one-key-dir.el
;; Description: Functions for quickly navigating directory trees with one-key menus
;; Author: Joe Bloggs <vapniks@yahoo.com>
;; Maintainer: Joe Bloggs <vapniks@yahoo.com>
;; Copyright (C) 2010, Joe Bloggs, all rights reserved.
;; Created: 2010-09-21 17:23:00
;; Version: 1.0
;; Last-Updated: 2012-08-07 17:23:00
;;           By: Joe Bloggs
;; URL: http://www.emacswiki.org/emacs/download/one-key-dir.el
;; Keywords: one-key, directories
;; Compatibility: GNU Emacs 24.0.50.1
;;
;; Features that might be required by this library:
;;
;; `hexrgb' `one-key'
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
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

;;; Commentary: 
;; 
;; This library defines a `one-key' menu type for navigating directories, and visiting files.
;; See the documentation for `one-key' for how to add a new menu to a menu-set.
;; From the *One-Key* buffer you can add a menu of type "directory" which will prompt you for a given directory.
;; You can also add a menu of type "dir:<PATH>" to a menu set from the customization buffer for
;; `one-key-set-of-menus-alist', where <PATH> is the path to an existing directory.

;; The one-key-dir menu will contain items for files and directories in the associated directory,
;; and also the items "." which will take you to a dired buffer for the directory, and ".." which will
;; open a menu for the parent directory. You can change the behaviour of the "." item by customizing
;; `one-key-dir-default-dir-func'.
;; Note that no ".." item will be added when the directory is `one-key-dir-topdir',
;; and it is not possible to navigate above this directory.
;; If there are a large number of items they may not all fit in one menu, in which case several menus may be created
;; (with the same name suffixed by a number in brackets).
;; You can switch between the different menus by using the left/right arrow keys (or whatever the appropriate
;; keys are in `one-key-special-keybindings').

;; By default hidden files and directories (with names beginning with ".") are excluded from the menu.
;; This can be changed by customizing `one-key-dir-default-exclude-regex'.

;;; Installation:
;;
;; Put one-key-dir.el in a directory in your load-path, e.g. ~/.emacs.d/
;; You can add a directory to your load-path with the following line in ~/.emacs
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;; where ~/elisp is the directory you want to add 
;; (you don't need to do this for ~/.emacs.d - it's added by default).
;;
;; Add the following to your ~/.emacs startup file.
;;
;; (require 'one-key-dir)

;;; Customize:

;; `one-key-dir-default-exclude-regex' : A regular expression matching files/dirs to be excluded by default from 
;;                                       one-key-dir menus.
;; `one-key-dir-default-file-func' : Default function to use for opening files from within a `one-key-dir' menu.
;; `one-key-dir-default-dir-func' : Default function to use for opening the current directory from within a `one-key-dir' 
;;                                  menu.
;; `one-key-dir-parentdir-key' : Key that will be used to return to the parent directory.
;; `one-key-dir-current-directory-key' : Key that will be used to open the current directory.
;; `one-key-dir-topdir' : The default top level dir that `one-key-dir-visit' can explore the subdirs of, but can't go above 
;;                        this dir.
;; `one-key-dir-max-items-per-page' : The maximum number of menu items to display on each page.
;; `one-key-dir-special-keybindings' : List of special keys to be used for one-key-dir menus (see 
;;                                     `one-key-default-special-keybindings' for more info).

;;; TODO:
;;
;; Handle large directories by splitting items between several alists and passing these to `one-key-menu' as a list
;; of alists/symbols (once that functionality is added to `one-key-menu').
;; Auto-highlight menu items based on filetype using dired colour scheme.
;; Add "sort by filetype" to sort list.
;; Put sort type info in mode-line instead of top of buffer.

;;; Change log:
;;	
;; 2010/09/21
;;      * First released.
;; 

;;; Require
(require 'one-key)

;;; Code:
(defgroup one-key-dir nil
  "Filesystem navigation using `one-key'."
  :group 'one-key)

(defface one-key-dir-file-name 
  `((default (:background ,(if (featurep 'dired+)
                               (face-foreground 'diredp-file-name nil t)
                             (face-foreground 'dired-ignored nil t)))))
  "*Face used for file names."
  :group 'one-key-dir)

(defface one-key-dir-directory
  `((default (:background ,(if (featurep 'dired+)
                               (face-foreground 'diredp-dir-priv nil t)
                             (face-foreground 'dired-directory nil t)))))
  "*Face used for directories."
  :group 'one-key-dir)

(defface one-key-dir-symlink
  `((default (:background ,(if (featurep 'dired+)
                               (face-foreground 'diredp-symlink nil t)
                             (face-foreground 'dired-symlink nil t)))))
  "*Face used for symlinks."
  :group 'one-key-dir)

(defvar one-key-dir-current-dir nil
  "Current directory which is visited by one-key.")

(defcustom one-key-dir-sort-method-alist '(("name" . (lambda (itema itemb)
                                                       (let* ((a (one-key-dir-extract-file-from-item itema))
                                                              (b (one-key-dir-extract-file-from-item itemb)))
                                                         (string< a b))))
                                           ("extension" . (lambda (itema itemb)
                                                            (let* ((a (one-key-dir-extract-file-from-item itema))
                                                                   (b (one-key-dir-extract-file-from-item itemb))
                                                                   (filea (file-name-nondirectory a))
                                                                   (fileb (file-name-nondirectory b)))
                                                              (flet ((get-ext (file-name) ; function to get file extension
                                                                              (car (cdr (split-string file-name "\\.")))))
                                                                (cond ((file-directory-p a) t)
                                                                      ((file-directory-p b) nil)
                                                                      (t (string< (get-ext filea) (get-ext fileb))))))))
                                           ("size" . (lambda (itema itemb)
                                                       (let* ((a (one-key-dir-extract-file-from-item itema))
                                                              (b (one-key-dir-extract-file-from-item itemb))
                                                              (attriba (file-attributes a))
                                                              (attribb (file-attributes b)))
                                                         (> (nth 7 attriba) (nth 7 attribb)))))
                                           ("time accessed" . (lambda (itema itemb)
                                                                (let* ((a (one-key-dir-extract-file-from-item itema))
                                                                       (b (one-key-dir-extract-file-from-item itemb))
                                                                       (attriba (file-attributes a))
                                                                       (attribb (file-attributes b))
                                                                       (x (nth 4 attriba))
                                                                       (y (nth 4 attribb)))
                                                                  (or (> (car x) (car y))
                                                                      (and (= (car x) (car y))
                                                                           (> (cadr x) (cadr y)))))))
                                           ("time modified" . (lambda (itema itemb)
                                                                (let* ((a (one-key-dir-extract-file-from-item itema))
                                                                       (b (one-key-dir-extract-file-from-item itemb))
                                                                       (attriba (file-attributes a))
                                                                       (attribb (file-attributes b))
                                                                       (x (nth 5 attriba))
                                                                       (y (nth 5 attribb)))
                                                                  (or (> (car x) (car y))
                                                                      (and (= (car x) (car y))
                                                                           (> (cadr x) (cadr y)))))))
                                           ("time changed" . (lambda (itema itemb)
                                                               (let* ((a (one-key-dir-extract-file-from-item itema))
                                                                      (b (one-key-dir-extract-file-from-item itemb))
                                                                      (attriba (file-attributes a))
                                                                      (attribb (file-attributes b))
                                                                      (x (nth 6 attriba))
                                                                      (y (nth 6 attribb)))
                                                                 (or (> (car x) (car y))
                                                                     (and (= (car x) (car y))
                                                                          (> (cadr x) (cadr y))))))))
  "An alist of sort predicates to use for sorting directory listings.
Each element is a cons cell in the form (NAME . PREDICATE) where NAME is a symbol naming the predicate and PREDICATE
is a function which takes two items as arguments and returns non-nil if the first item should come before the second
in the menu."
  :type '(alist :key-type (string :help-echo "Name of sort method")
                :value-type (function :help-echo "Predicate that returns non-nil if 1st item comes before 2nd"))  
  :group 'one-key-dir)

(defcustom one-key-dir-default-sort-method (caar one-key-dir-sort-method-alist)
  "The default sort method to use when building new one-key-dir menus."
  :type 'string
  :group 'one-key-dir)

(defcustom one-key-dir-default-exclude-regex "^\\.\\|~$"
  "A regular expression matching files/dirs to be excluded by default from one-key-dir menus.
This is the default value for the exclude-regex argument to `one-key-dir-build-menu-alist' and `one-key-dir-visit'."
  :type 'regexp
  :group 'one-key-dir)

(defcustom one-key-dir-default-file-func 'find-file
  "Default function to use for opening files from within a `one-key-dir' menu.
The function should take a single argument - the name of the file to be opened/operated on."
  :group 'one-key-dir
  :type 'function)

(defcustom one-key-dir-default-dir-func 'find-file
  "Default function to use for opening the current directory from within a `one-key-dir' menu.
The function should take a single argument - the name of the file to be opened/operated on.
Note: this is only used when opening the current directory. Child or parent directories are opened recursively
with `one-key-dir-visit'."
  :group 'one-key-dir
  :type 'function)

(defcustom one-key-dir-parentdir-key ?^
  "Key that will be used to return to the parent directory.
This should not be a letter or number key."
  :group 'one-key-dir
  :type '(character :validate (lambda (w) (let ((val (widget-value w)))
                                            (if (memq val (append one-key-default-menu-keys
                                                                  (list one-key-dir-current-directory-key)))
                                                (progn (widget-put w :error "That key is already used! Try another")
                                                       w))))))

(defcustom one-key-dir-current-directory-key ?.
  "Key that will be used to open the current directory.
This should not be a letter or number key."
  :group 'one-key-dir
  :type '(character :validate (lambda (w) (let ((val (widget-value w)))
                                            (if (memq val (append one-key-default-menu-keys
                                                                  (list one-key-dir-parentdir-key)))
                                                (progn (widget-put w :error "That key is already used! Try another")
                                                       w))))))

(defcustom one-key-dir-topdir "/"
  "The default top level dir that can't be navigated above.
This is the default value for the topdir arg to `one-key-dir-build-menu-alist' and `one-key-dir-visit'."
  :group 'one-key-dir
  :type 'directory)

(defcustom one-key-dir-max-items-per-page 60
  "The maximum number of menu items to display on each page."
  :group 'one-key-dir
  :type '(number :match (lambda (w val)
                          (> val 0))))

;; Add new special keys for `one-key-dir' menus
(customize-set-variable 'one-key-special-keybindings
                        (one-key-add-elements-to-alist
                         'one-key-special-keybindings
                         '((dir-documentation documentation "Show one-key-dir documentation"
                                              (lambda nil (finder-commentary (locate-library "one-key-dir"))
                                                (setq one-key-menu-window-configuration nil)
                                                nil))) t))

(defcustom one-key-dir-special-keybindings
  (one-key-add-elements-to-list
   'one-key-general-special-keybindings
   '(dir-documentation limit-items highlight-items add-menu remove-menu move-item donate report-bug))
  "List of special keys to be used for one-key-dir menus (see `one-key-default-special-keybindings' for more info)."  
  :group 'one-key-dir
  :type '(repeat (symbol :tag "Name" :help-echo "The name/symbol corresponding to the keybinding.")))

(defun one-key-dir-extract-file-from-item (item)
  "Given a one-key-dir menu item ITEM, return corresponding file/directory (as a string).
Alternatively, if ITEM is a string it will be returned as is (this is so that we can also use the sort functions
for sorting files directly)."
  (if (stringp item) (if (file-exists-p item)
                         (if (file-directory-p item)
                             (concat (file-truename item) "/")
                           (file-truename item))
                       (error "Non-existing file!"))
    (let* ((part (fourth (cdr item)))
           (isfile (eq (car part) 'funcall)))
      (if isfile (third part)
        (second (cadar (second part)))))))

(defun* one-key-dir-visit (dir)
  "Visit DIR using one-key.
For each sub-dir of DIR, the function will be called recursively replacing DIR with the sub-dir.
For each file under DIR, the associated command will be `(funcall filefunc)' (so filefunc should not require any arguments).
In FILEFUNC, `one-key-dir-current-filename' can be used to do operations on the current file.
The optional FILENAME-MAP-FUNC specifies a function to be called on each file name, it has one argument (string),
the original file name, and returns a string, the new file name which will be displayed in the one-key menu.
DIR should either be `one-key-dir-topdir' or a directory under `one-key-dir-topdir' in the directory tree.
This prevents the user from navigating above `one-key-dir-topdir'.
Any files/dirs that match the EXCLUDE-REGEX argument will be omitted from the menu.

Normally items for the current directory and parent directory are included, and will be labelled \".\" and \"..\" with corresponding keys `one-key-dir-current-directory-key' and `one-key-dir-parentdir-key'.
However if the directory is TOPDIR or a parent of TOPDIR the \"..\" item will not be included.
Also if VISITABLE is nil then the \".\" item will not be included for any directories.
Default values for TOPDIR and VISITABLE are `one-key-dir-topdir' and t respectively."
  (interactive (list (if (featurep 'ido)
                         (ido-read-directory-name "one-key-dir: " default-directory)
                       (read-directory-name "one-key-dir: " default-directory))))
  (unless (file-directory-p dir)
    (error "one-key-dir-visit called with a non-directory"))
  (unless (one-key-dir-descendant-p dir t)
    (error "one-key-dir-visit called with an illegal directory."))
  (setq one-key-dir-current-dir dir)
  (unwind-protect
      (let* ((dir-alists (one-key-dir-build-menu-alist dir))
             (nummenus (length dir-alists))
             (dirname (file-name-as-directory (file-truename dir)))
             (menunames (one-key-append-numbers-to-menu-name dirname nummenus)))
        (one-key-menu menunames dir-alists))))

(defun* one-key-dir-build-menu-alist (dir &key
                                          (filefunc one-key-dir-default-file-func)                                          
                                          (dirfunc one-key-dir-default-dir-func)
                                          filename-map-func
                                          (exclude-regex one-key-dir-default-exclude-regex)
                                          (initial-sort-method one-key-dir-default-sort-method)
                                          (keyfunc 'one-key-generate-key)
                                          (topdir one-key-dir-topdir)
                                          (visitable t))
  "Build `one-key-menu' items lists for directory DIR.
Each element of the returned list has the following form: ((KEY . NAME) . FUNCTION).
Where FUNCTION is a function that may call FILEFUNC or DIRFUNC depending on whether the item corresponds to a file
or directory. FILEFUNC and DIRFUNC are functions of one argument that take a file/directory name respectively.
If FILENAME-MAP-FUNC is non-nil it should be a function that takes a single file or directory name
as argument and returns a label for the menu item. Otherwise the name of the file/directory will be used for the label.
EXCLUDE-REGEX should be a regular expression which will be matched against each items name (before being put through
FILENAME-MAP-FUNC). Any matching items will be omitted from the results.
By default EXCLUDE-REGEX is set to `one-key-dir-default-exclude-regex'.
The returned menu alist will initially be sorted by the method indicated by the INITIAL-SORT-METHOD arg which should be
the car of one of the items in `one-key-dir-sort-method-alist' (a symbol). By default INITIAL-SORT-METHOD is set to
`one-key-dir-default-sort-method'.
The KEYFUNC arg should be a function for generating keys for the menu items.
This function should take two args, an item description and a list of used keys, and return a key for the item.
By default KEYFUNC is set to `one-key-generate-key' and you will probably not need to change it.

Normally items for the current directory and parent directory are included, and will be labelled \".\" and \"..\"
with corresponding keys `one-key-dir-current-directory-key' and `one-key-dir-parentdir-key'.
However if the directory is TOPDIR or a parent of TOPDIR the \"..\" item will not be included.
Also if VISITABLE is nil then the \".\" item will not be included for any directories.
Default values for TOPDIR and VISITABLE are `one-key-dir-topdir' and t respectively.

Only the first `one-key-dir-max-items-per-page' items (excluding \"..\" and \".\") will be placed in each list.
If there are no more than `one-key-dir-max-items-per-page' items, then a single list will be returned, otherwise several
lists will be returned (as list of lists). These lists can be navigated from the `one-key' menu using the arrow keys."
  (unless (file-directory-p dir)
    (error "one-key-dir-build-key-name-list called with a non-directory."))
  (flet ( ;; temp function to indicate whether to exclude an item or not
         (exclude (str) (or (equal (file-name-nondirectory str) "..")
                            (equal (file-name-nondirectory str) ".")
                            (and exclude-regex
                                 (string-match exclude-regex
                                               (file-name-nondirectory str)))))
         (cmdfunc (item) (if (file-directory-p item)
                             `(lambda nil ; command for directories
                                (interactive)
                                (let* ((dir-alists (one-key-dir-build-menu-alist
                                                    ,item
                                                    :filefunc ',filefunc
                                                    :dirfunc ',dirfunc
                                                    :filename-map-func ',filename-map-func
                                                    :exclude-regex ,exclude-regex
                                                    :topdir ,topdir
                                                    :visitable ,visitable))
                                       (nummenus (length dir-alists))
                                       (dirname (concat "dir:" ,(file-name-as-directory (file-truename item))))
                                       (menunames (one-key-append-numbers-to-menu-name dirname nummenus)))
                                  (one-key-open-submenu menunames dir-alists)))
                           `(lambda nil ; command for files
                              (interactive)
                              (funcall ',filefunc ,(file-truename item)))))
         (descfunc (item) (let ((name (if filename-map-func (funcall filename-map-func item)
                                        (file-name-nondirectory item))))
                            (if (file-directory-p item)
                                (propertize (concat name "/") 'face 'one-key-dir-directory)
                              (if (file-symlink-p item) (propertize name 'face 'one-key-dir-symlink)
                                (propertize name 'face 'one-key-dir-file-name))))))
    (let* ((dirname (file-name-as-directory (file-truename dir)))
           (sortfunc (cdr (assoc initial-sort-method one-key-dir-sort-method-alist)))
           (items (sort (remove-if 'exclude (directory-files dirname t)) sortfunc))
           (commands (mapcar 'cmdfunc items))
           (descriptions (mapcar 'descfunc items))
           (menus (one-key-create-menu-lists commands descriptions nil nil
                                             :maxsize one-key-dir-max-items-per-page
                                             :keyfunc keyfunc))
           (thisdircmd `(lambda nil (interactive) (funcall ',dirfunc ,dirname)))
           (updircmd (cmdfunc (file-name-directory (file-truename (if (equal (substring dir -1) "/")
                                                                      (substring dir 0 -1)
                                                                    dir))))))
      (loop for menu in-ref menus do
            (if visitable
                (push (cons (cons (single-key-description one-key-dir-current-directory-key)
                                  ".") thisdircmd) menu))
            (if (one-key-dir-descendant-p dir nil topdir)
                (push (cons (cons (single-key-description one-key-dir-parentdir-key)
                                  "..") updircmd) menu)))
      menus)))

(defun* one-key-dir-descendant-p (dir &optional allow-equal (topdir one-key-dir-topdir))
  "Return t if DIR is a descendant of TOPDIR (default `one-key-dir-topdir').
If ALLOW-EQUAL is non-nil also return t if DIR is the same dir as TOPDIR."
  (let ((topdir-name (file-name-as-directory (file-truename topdir)))
	(dir-name (file-name-as-directory (file-truename dir))))
    (if (string= topdir-name dir-name) (if allow-equal t)
      (if (= (- (abs (compare-strings topdir-name 0 nil dir-name 0 nil)) 1) (length topdir-name)) t))))

;; Set menu-alist, title string and special keybindings for new `one-key-dir' menus, prompting the user for the directory
(one-key-add-to-alist 'one-key-types-of-menu
                      (list "directory"
                            (lambda (name) ;; this type accepts the path to any existing directory
                              (or (string= name "directory")
                                  (and (string-match "^dir:\\(.*?\\)\\( ([0-9]+)\\)?$" name)
                                       (file-directory-p (match-string 1 name)))))
                            (lambda (name)
                              (let* ((match (and (string-match "dir:\\(.*?\\)\\( ([0-9]*)\\)?$" name)
                                                 (match-string 1 name)))
                                     (match2 (and match (match-string 2 name)))
                                     (dir (if (and match (file-directory-p match))
                                              (if (or (not match2)
                                                      (string= match2 " (1)"))
                                                  match)
                                            (if (featurep 'ido)
                                                (ido-read-directory-name "Directory to use: " default-directory)
                                              (read-directory-name "Directory to use: " default-directory))))
                                     (menulists (if dir (one-key-dir-build-menu-alist dir)))
                                     (nummenus (if dir (length menulists)))
                                     (names (if dir (one-key-append-numbers-to-menu-name
                                                     (concat "dir:" dir) nummenus))))
                                (if dir (cons names menulists))))
                            nil
                            'one-key-dir-special-keybindings
                            'one-key-dir-sort-method-alist) t)

(provide 'one-key-dir)
;;; one-key-dir.el ends here
