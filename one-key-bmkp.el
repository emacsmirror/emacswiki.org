;;; one-key-bmkp.el --- one-key menus for bookmarks

;; Filename: one-key-bmkp.el
;; Description: one-key menus for bookmarks
;; Author: Joe Bloggs <vapniks@yahoo.com>
;; Maintainer: Joe Bloggs <vapniks@yahoo.com>
;; Copyleft (Ↄ) 2012, Joe Bloggs, all rites reversed.
;; Created: 2012-08-10 22:16:19
;; Version: 0.1
;; Last-Updated: 2012-08-10 22:16:19
;;           By: Joe Bloggs
;; URL: http://www.emacswiki.org/emacs/download/one-key-bmkp.el
;; Keywords: convenience, files, tools
;; Compatibility: GNU Emacs 24.1.1
;;
;; Features that might be required by this library:
;;
;; one-key.el, cl.el, ido.el, hexrgb.el, bookmark+.el
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
;; along with this program; see the file COPYING.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary: 
;; 
;; This library defines a `one-key' menu type for bookmarks.
;; See the documentation for `one-key' for how to add a new menu to a menu-set.

;; From the *One-Key* buffer you can add a menu of type "bookmarks+" which will prompt you for a logical expression
;; defining a filter for selecting which bookmarks to display. The logical expression is created by selecting filters
;; from the one-key menu which are then OR/AND'ed together depending on the depth of bracketing. You can open/close
;; brackets in the logical expression using the bracket keys or SPACE/RET keys, and you can negate items by pressing !
;; followed by the item. If you make a mistake you can delete the previous item by pressing <backspace>.
;; The logical expression is displayed above the filter items.
;; To complete the filter press RET or ) at the top level of the logical expression. You will then be prompted to save
;; the filter and name it. If you save the filter, e.g. under the name "filter1", you can then add it to a menu set
;; by customizing `one-key-sets-of-menus-alist' and adding "bmkp:filter1" to the menu set.
;; Any saved filters may also be used for new filters, and made a part of the logical expression defining the filter.

;; After defining a filter, or on loading a saved filter, the one-key menu will contain all bookmarks matching the
;; corresponding filter. Pressing a menu item key visits the associated bookmark.
;; Press <F1> to see the special keys associated with the menu which allow you to add/remove tags, and edit bookmarks,
;; aswell as visit the *Bookmark List* buffer.

;;; Installation:
;;
;; Make sure you've already installed one-key (http://www.emacswiki.org/emacs/OneKey),
;; and bookmarks+ (http://www.emacswiki.org/emacs/BookmarkPlus).
;; Put one-key-bmkp.el in a directory in your load-path, e.g. ~/.emacs.d/
;; You can add a directory to your load-path with the following line in ~/.emacs
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;; where ~/elisp is the directory you want to add 
;; (you don't need to do this for ~/.emacs.d - it's added by default).
;;
;; Add the following to your ~/.emacs startup file.
;;
;; (require 'one-key-bmkp)

;;; Customize:
;;
;; `one-key-bmkp-filter-alist' : Alist of filters for one-key bookmark menus.
;; `one-key-bmkp-special-keybindings' : List of special keys to be used for one-key-bmkp menus (see 
;;                                      `one-key-default-special-keybindings' for more info).

;;
;; All of the above can customized by:
;;      M-x customize-group RET one-key-bmkp RET
;;

;;; Change log:
;;	
;; 2012/08/10
;;      * First released.
;; 

;;; Acknowledgements:
;;
;; 
;;

;;; TODO
;;
;; Update documentation
;;

;;; Require
(require 'one-key)
(require 'bookmark+)

;;; Code:

(defgroup one-key-bmkp nil
  "Bookmark menus using `one-key'."
  :group 'one-key-bmkp)

(defcustom one-key-bmkp-filter-alist
  '(("no bookmarks" .
     (or nil (not (string-match ".*" (car bmk)))))
    ("all bookmarks" .
     (or (string-match ".*" (car bmk)))))
  "Alist of filters for one-key bookmark menus.
The car of each element is a name for the filter, and the cdr is a lisp form which returns non-nil when a bookmark bmk should
be included in the menu. The variable bmk will be set to the bookmark being tested, and bmk-tags will be set to the list of tags
of that bookmark."
  :type '(alist :key-type (string :tag "Filter name") :value-type (function :tag "Filter function"))
  :group 'one-key-bmkp)

(defvar one-key-bmkp-sort-method-alist
  '((key . (lambda (itema itemb) (string< (caar itema) (caar itemb))))
    (length . (lambda (itema itemb) (> (length (cdar itema)) (length (cdar itemb)))))
    (name . (lambda (itema itemb) (string< (cdar itema) (cdar itemb))))
    (type . (lambda (itema itemb)
              (let* ((bmka (bookmark-get-bookmark (one-key-bmkp-extract-bookmark-from-item itema)))
                     (bmkb (bookmark-get-bookmark (one-key-bmkp-extract-bookmark-from-item itemb)))
                     (typea (one-key-bmkp-get-bookmark-type bmka))
                     (typeb (one-key-bmkp-get-bookmark-type bmkb)))
                (string< (car typea) (car typeb))))))
  "An alist of sort predicates to use for sorting bookmarks.
See `one-key-default-sort-method-alist' for the format of this variable.")

(defvar one-key-bmkp-current-sort-method 'name
  "The current method used to sort the items in the bookmarks menu.
Should contain the car of one of the items in `one-key-bmkp-sort-method-alist'.")

(customize-set-variable 'one-key-special-keybindings
                        (one-key-add-elements-to-alist
                         'one-key-special-keybindings
                         `((bmk-documentation documentation "Show one-key-bmkp documentation"
                                              (lambda nil (finder-commentary (locate-library "one-key-bmkp"))
                                                (setq one-key-menu-window-configuration nil)
                                                nil))
                           (bmk-sort-next sort-next "Sort items by next method"
                                          ,(apply-partially 'one-key-sort-items-by-next-method
                                                            nil t
                                                            'one-key-bmkp-current-sort-method
                                                            'one-key-bmkp-sort-method-alist))
                           (bmk-sort-prev sort-prev "Sort items by previous method"
                                          ,(apply-partially 'one-key-sort-items-by-next-method
                                                            t t
                                                            'one-key-bmkp-current-sort-method
                                                            'one-key-bmkp-sort-method-alist))
                           (bmk-new-filter highlight-items "Create new bookmarks filter"
                                           (lambda nil
                                             (let* ((pair (one-key-bmkp-create-menu "new bookmarks filter"))
                                                    (menunames (car pair))
                                                    (menulists (cdr pair)))
                                               (setq one-key-menu-call-first-time nil)
                                               (one-key-add-menus menunames menulists)
                                               (string-match "^\\(.*?\\)\\( ([0-9]+)\\)?$" okm-this-name)
                                               (one-key-delete-menus
                                                (regexp-opt (list (match-string 1 okm-this-name))))) t))
                           (bmk-customize customize-menusets "Customize one-key-bmkp"
                                          (lambda nil
                                            (setq one-key-menu-window-configuration nil)
                                            (with-selected-window (previous-window)
                                              (customize-group 'one-key-bmkp)) nil))
                           (bmk-load-filter "C-l" "Open saved filter"
                                            (lambda nil
                                              (let* ((pair (one-key-bmkp-prompt-for-filter))
                                                     (menunames (car pair))
                                                     (menulists (cdr pair)))
                                                (setq one-key-menu-call-first-time nil)
                                                (one-key-add-menus menunames menulists)
                                                (string-match "^\\(.*?\\)\\( ([0-9]+)\\)?$" okm-this-name)
                                                (one-key-delete-menus
                                                 (regexp-opt (list (match-string 1 okm-this-name))))) t))
                           (bmk-goto-bookmark-list "M-l" "Goto bookmark list buffer"
                                                   (lambda nil
                                                     (let ((bookmark-alist (one-key-bmkp-extract-bookmarks-from-menus
                                                                            (cdr (one-key-get-menus)))))
                                                       (setq bmkp-latest-bookmark-alist  bookmark-alist)
                                                       (bookmark-bmenu-list)
                                                       (one-key-menu-window-close)
                                                       (switch-to-buffer "*Bookmark List*")) nil))
                           (bmk-edit-bookmark edit-item "Edit a bookmark"
                                              (lambda nil
                                                (let* ((key (read-event "Press the key of the item you want to edit"))
                                                       (item (one-key-get-menu-item key okm-full-list))
                                                       (bookmark (one-key-bmkp-extract-bookmark-from-item item)))
                                                  (bmkp-edit-bookmark-record bookmark)
                                                  (setq one-key-menu-window-configuration nil))
                                                nil))
                           (bmk-add-tags kill-items "Add tags to displayed bookmarks"
                                         (lambda nil
                                           (let ((bookmarks (one-key-bmkp-extract-bookmarks-from-menus
                                                             (list okm-filtered-list)))
                                                 (tags (bmkp-read-tags-completing)))
                                             (dolist (bmk bookmarks)
                                               (bmkp-add-tags bmk tags))
                                             ;; update tags list and save bookmarks
                                             (bmkp-tags-list)
                                             (bookmark-bmenu-save nil)) t))
                           (bmk-remove-tags swap-keys "Remove tags from displayed bookmarks"
                                            (lambda nil
                                              (let ((bookmarks (one-key-bmkp-extract-bookmarks-from-menus
                                                                (list okm-filtered-list)))
                                                    (tags (bmkp-read-tags-completing)))
                                                (unless (not (y-or-n-p
                                                              (concat
                                                               "The following tags will be removed from the displayed bookmarks: "
                                                               (mapconcat 'identity tags ",") "\nAre you sure")))
                                                  (dolist (bmk bookmarks)
                                                    (bmkp-remove-tags bmk tags))
                                                  ;; update tags list and save bookmarks
                                                  (bmkp-tags-list)
                                                  (bookmark-bmenu-save nil))) t)))
                         t))

(defcustom one-key-bmkp-special-keybindings
  '(quit-close quit-open toggle-persistence toggle-display next-menu prev-menu up down scroll-down scroll-up
               bmk-customize limit-items bmk-new-filter bmk-load-filter bmk-goto-bookmark-list bmk-save-state toggle-help
               bmk-documentation toggle-row/column-order bmk-sort-next bmk-sort-prev reverse-order bmk-edit-bookmark
               bmk-add-tags bmk-remove-tags add-menu remove-menu move-item donate report-bug)
  "List of special keys to be used for one-key-bmkp menus (see `one-key-default-special-keybindings' for more info)."  
  :group 'one-key-bmkp
  :type '(repeat (symbol :tag "Name" :help-echo "The name/symbol corresponding to the keybinding.")))

(defun one-key-bmkp-convert-item-to-condition (item)
  (string-match "^\\([^=]+\\):\\(.*\\)" item)
  (let ((pre (match-string 1 (substring-no-properties item)))
        (post (match-string 2 (substring-no-properties item))))
    (cond ((equal pre "tag") `(member ,post bmk-tags))
          ((equal pre "type") `(,(intern-soft (concat "bmkp-" post "-bookmark-p")) bmk))
          ((equal pre "regexp-name") `(string-match ,post (car bmk)))
          ((equal pre "regexp-all-tags") `(bmkp-every (lambda (tag) (string-match ,post tag))
                                                bmk-tags))
          ((equal pre "regexp-some-tags") `(bmkp-some (lambda (tag) (string-match ,post tag))
                                                bmk-tags))
          ((equal pre "filter") (cdr (assoc post one-key-bmkp-filter-alist)))
          ((equal pre "TRUE") t))))

(defun one-key-bmkp-get-filter (&optional filtername)
  "Prompt the user for a logical formula defining a predicate function for filtering bookmarks.
Return a cons cell whose car is a description of the filter, and whose cdr is the predicate function.
If FILTERNAME is supplied then just return the filter function associated with that name in `one-key-bmkp-filter-alist'."
  (if filtername (cdr (assoc filtername one-key-bmkp-filter-alist))
    (let* ((alltags (mapcar
                     (lambda (tag) (one-key-colourize-string "red" (concat "tag:" tag)))
                     (bmkp-tags-list t)))
           (pair (one-key-read-logical-formula
                  "Define bookmark filter"
                  (append '("TRUE:" "existing filter" "match name with regexp" "match all tags with regexp"
                            "match some tags with regexp" "match type") alltags)
                  (lambda (i d m)
                    (let ((i2 (cond ((equal i "match type")
                                     (concat "type:"
                                             (ido-completing-read
                                              "Type: "
                                              (loop for sym being the symbols
                                                    when (fboundp sym)
                                                    when (string-match
                                                          "^bmkp-\\(.*\\)-bookmark-p$"
                                                          (symbol-name sym))
                                                    collect (match-string 1 (symbol-name sym))))))
                                    ((equal i "match name with regexp")
                                     (concat "regexp-name:"
                                             (read-regexp "Regular expression")))
                                    ((equal i "match all tags with regexp")
                                     (concat "regexp-all-tags:"
                                             (read-regexp "Regular expression")))
                                    ((equal i "match some tags with regexp")
                                     (concat "regexp-some-tags:"
                                             (read-regexp "Regular expression")))
                                    ((equal i "existing filter")
                                     (concat "filter:" (let ((filterlist
                                                             (mapcar 'car one-key-bmkp-filter-alist)))
                                                        (if (featurep 'ido)
                                                            (ido-completing-read "Filter name: " filterlist)
                                                          (completing-read "Filter name: " filterlist)))))
                                    (t i))))
                      (if (stringp i2) (one-key-bmkp-convert-item-to-condition i2) i2))) t)))
      (cons (car pair) (cdr pair)))))

(defun one-key-bmkp-extract-bookmarks-from-menus (menulists)
  "Given a list MENULISTS of one-key bookmark menu alists, return a list of all bookmarks in the menus."
  (loop for menu in menulists
        append (loop for item in menu
                     collect (one-key-bmkp-extract-bookmark-from-item item))))

(defun one-key-bmkp-extract-bookmark-from-item (item)
  "Given an item ITEM from a bookmark menu alist, return the associated bookmark."
  (let ((rest (cdr item)))
    (cadr (second (fourth (if (one-key-list-longer-than-1-p rest)
                              (car rest) rest))))))

(defun one-key-bmkp-get-bookmark-type (bmk)
  "Return a cons cell whose car is string indicating what type of bookmark BMK is, and whose cdr is a face for the bookmark."
  (let* ((bookmark (bookmark-get-bookmark bmk))
         (bookmark-name   (bmkp-bookmark-name-from-record bookmark))
         (buffp           (bmkp-get-buffer-name bookmark))
         (filep           (bookmark-get-filename bookmark))
         (sudop           (and filep  (boundp 'tramp-file-name-regexp)
                               (string-match tramp-file-name-regexp filep)
                               (string-match bmkp-su-or-sudo-regexp filep))))
    (cond ((bookmark-prop-get bookmark 'file-handler) ; `file-handler' bookmark
            (cons "file handler" (bmkp-face-prop 'bmkp-file-handler)))
           ((bmkp-sequence-bookmark-p bookmark) ; Sequence bookmark
            (cons "sequence" (bmkp-face-prop 'bmkp-sequence)))
           ((bmkp-function-bookmark-p bookmark) ; Function bookmark
            (cons "function" (bmkp-face-prop 'bmkp-function)))
           ((bmkp-variable-list-bookmark-p bookmark) ; Variable-list bookmark
            (cons "variable list" (bmkp-face-prop 'bmkp-variable-list)))
           ((bmkp-bookmark-list-bookmark-p bookmark) ; Bookmark-list bookmark
            (cons "bookmark list" (bmkp-face-prop 'bmkp-bookmark-list)))
           ((bmkp-desktop-bookmark-p bookmark) ; Desktop bookmark
            (cons "desktop" (bmkp-face-prop 'bmkp-desktop)))
           ((bmkp-bookmark-file-bookmark-p bookmark) ; Bookmark-file bookmark
            (cons "bookmark file" (bmkp-face-prop 'bmkp-bookmark-file)))
           ((bmkp-info-bookmark-p bookmark) ; Info bookmark
            (cons "info page" (bmkp-face-prop 'bmkp-info)))
           ((bmkp-man-bookmark-p bookmark) ; Man bookmark
            (cons "man page" (bmkp-face-prop 'bmkp-man)))
           ((bmkp-gnus-bookmark-p bookmark) ; Gnus bookmark
            (cons "gnus" (bmkp-face-prop 'bmkp-gnus)))
           ((bmkp-url-bookmark-p bookmark) ; URL bookmark
            (cons "URL" (bmkp-face-prop 'bmkp-url)))
           ((and sudop  (not (bmkp-root-or-sudo-logged-p))) ; Root/sudo not logged in
            (cons "sudo" (bmkp-face-prop 'bmkp-su-or-sudo)))
           ;; Test for remoteness before any other tests of the file itself
           ;; (e.g. `file-exists-p'), so Tramp does not prompt for a password etc.
           ((and filep  (bmkp-file-remote-p filep)  (not sudop)) ; Remote file (ssh, ftp)
            (cons "remote file" (bmkp-face-prop 'bmkp-remote-file)))
           ((and filep ; Local directory or local Dired buffer (could be wildcards)
                 (or (file-directory-p filep)  (bmkp-dired-bookmark-p bookmark)))
            (cons "local dir" (bmkp-face-prop 'bmkp-local-directory)))
           ((and filep  (file-exists-p filep) ; Local file with region
                 (bmkp-region-bookmark-p bookmark))
            (cons "local file with region" (bmkp-face-prop 'bmkp-local-file-with-region)))
           ((and filep  (file-exists-p filep)) ; Local file without region
            (cons "local file without region" (bmkp-face-prop 'bmkp-local-file-without-region)))
                                        ; Existing buffer, including for a file bookmark if the file buffer has not yet been saved.
           ((and buffp  (get-buffer buffp))
            (cons "existing buffer" (bmkp-face-prop 'bmkp-buffer)))
           ((or (not filep)  (equal filep bmkp-non-file-filename)) ; Non-file, and no existing buffer.
            (cons "non-existing buffer" (bmkp-face-prop 'bmkp-non-file)))
           ((and filep  (not (file-exists-p filep))) ; Local-file bookmark, but no such file exists.
            (cons "non-existent local file" (bmkp-face-prop 'bmkp-no-local)))
           (t (cons "other" (bmkp-face-prop 'bmkp-bad-bookmark))))))

(defun one-key-bmkp-propertize-item (bmk)
  "Apply appropriate properties to string BMK (a bookmark name) as would be applied in the bookmark list.
This function is based on `bmkp-bmenu-propertize-item'"
  (let* ((bookmark (bookmark-get-bookmark bmk))
         (bookmark-name   (bmkp-bookmark-name-from-record bookmark))
         (buffp           (bmkp-get-buffer-name bookmark))
         (filep           (bookmark-get-filename bookmark))
         (sudop           (and filep  (boundp 'tramp-file-name-regexp)
                               (string-match tramp-file-name-regexp filep)
                               (string-match bmkp-su-or-sudo-regexp filep)))
         (type (one-key-bmkp-get-bookmark-type bmk)))
    (add-text-properties
     0  (length bmk)
     (cdr (one-key-bmkp-get-bookmark-type bmk))
     bmk)
    bmk))

(defun one-key-bmkp-create-menu (name)
  "Given the name of a bookmark menu, create and return the corresponding menus and menu titles.
If NAME does not correspond to a saved bookmarks filter in `one-key-bmkp-filter-alist' then the user will be prompted to
create a new filter. This function is used in the \"bookmarks\" item in `one-key-types-of-menu'."
  (setq one-key-menu-call-first-time t)
  (let* ((filtername (and name
                          (string-match "^bmk:\\(.*\\)" name)
                          (match-string 1 name)))
         (one-key-menu-window-close t)
         (pair (one-key-bmkp-get-filter filtername))
         (predfunc `(lambda (bmk) (let ((bmk-tags (mapcar 'bmkp-tag-name (bmkp-get-tags bmk))))
                                    ,(if filtername pair (cdr pair)))))
         (title (or filtername
                    (if (not (y-or-n-p "Save filter?"))
                        (car pair)
                      (let (newname)
                        (while (and (setq newname (read-string "Filter name: "))
                                    (assoc newname one-key-bmkp-filter-alist)
                                    (not (y-or-n-p "Filter with that name already exists, overwrite?"))))
                        (one-key-add-to-alist 'one-key-bmkp-filter-alist (cons newname predfunc))
                        newname))))
         (bookmarks (if predfunc (bmkp-remove-if-not predfunc bookmark-alist)
                      (message "Invalid filter name!")))
         (bmknames (mapcar (lambda (bmk) (one-key-bmkp-propertize-item (car bmk))) bookmarks))
         (commands (mapcar (lambda (bmk) `(lambda nil (interactive) (bookmark-jump ',bmk))) bookmarks))
         (menuitems (one-key-create-menu-lists commands bmknames))
         (nummenus (length menuitems))
         (menuname (concat "bmkp:" title))
         (menunames (if (> nummenus 1)
                        (one-key-append-numbers-to-menu-name menuname nummenus)
                      (list menuname))))
    (cons menunames (or menuitems (list nil)))))

(defun one-key-bmkp-prompt-for-filter nil
  "Prompt the user for an existing bookmarks filter, and return a cons cell containing the menu name and menu list respectively.
If there are no existing filters, print a message saying so and return nil."
  (let* ((filternames (mapcar 'car one-key-bmkp-filter-alist))
         (chosenfilter (and filternames
                            (if (featurep 'ido)
                                (ido-completing-read "Filter name: " filternames)
                              (completing-read "Filter name: " filternames)))))
    (if chosenfilter (one-key-bmkp-create-menu (concat "bmkp:" chosenfilter))
      (message "No existing filters available!") nil)))

;; Add new menu types to `one-key-types-of-menu'
(one-key-add-to-alist 'one-key-types-of-menu
                      (list "bookmarks+"
                            (lambda (name) (or (and name (string-match "^bmk:\\(.*\\)" name))
                                               (equal name "bookmarks+")))
                            'one-key-bmkp-create-menu
                            nil
                            'one-key-bmkp-special-keybindings) t)

(provide 'one-key-bmkp)

;;; one-key-bmkp.el ends here
