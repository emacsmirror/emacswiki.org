;;; one-key-yas.el --- functions for using one-key menus to access yasnippets

;; Filename: one-key-yas.el
;; Description: functions for using one-key menus to access yasnippets
;; Author:  Joe Bloggs <vapniks@yahoo.com>
;; Maintainer: Joe Bloggs <vapniks@yahoo.com>
;; Copyright (C) 2010, , all rights reserved.
;; Created: 2010-09-21 14:31:52
;; Version: 1.0
;; Last-Updated: 2012-08-07 18:30:52
;;           By: Joe Bloggs
;; URL: http://www.emacswiki.org/emacs/download/one-key-yas.el
;; Keywords: yasnippet one-key snippet
;; Compatibility: GNU Emacs 24.1.1
;;
;; Features that might be required by this library:
;;
;; one-key.el (tested on version 1.0) yasnippet.el (tested on version 0.6.1b)
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
;; This library defines a `one-key' menu type for accessing yasnippets.
;; See the documentation for `one-key' for how to add a new menu to a menu-set.
;; You can add a menu of type "yasnippet" which will contain snippets for the major-mode of the current buffer,
;; or a menu of type "yas:<MODE>" where <MODE> can be the name of any major-mode with snippets defined.
;; The former type of menu can be added from within the *One-Key* buffer, whereas the latter must be added
;; to a menu set from the customization buffer for `one-key-set-of-menus-alist'.
;;
 
;;; Installation:
;;
;; Put one-key-yas.el in a directory in your load-path, e.g. ~/.emacs.d/
;; You can add a directory to your load-path with the following line in ~/.emacs:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;; where ~/elisp is the directory you want to add.
;; (you don't need to do this for ~/.emacs.d - it's added by default).
;;
;; Add the following to your ~/.emacs startup file.
;;
;; (require 'one-key-yas)

;;; Customize: 
;;
;; You may need to change the value of `yas/root-directory'
;; so that it matches the location of your snippets files.
;; This can be done by:
;;      M-x customize-variable RET yas/root-directory RET
;;

;;; Change log:
;;
;; See the git log.
;;	
;; 2010/09/21
;;      * First released.
;; 

;;; TODO
;;

;;; Require
(require 'one-key)
(require 'one-key-dir)
(require 'yasnippet)

;;; Code:

;;; Unlike the original version in Yasnippet `yas/get-snippet-tables'
;;; this function only uses mode-symbol to index the loaded tables
(defun one-key-yas/get-snippet-tables (mode-symbol)
  "Get snippet tables for mode MODE-SYMBOL.
Return a list of 'yas/snippet-table' objects indexed by mode."
  (let ((mode-tables (list (gethash mode-symbol yas/snippet-tables)))
        all-tables)
    (dolist (table (remove nil mode-tables))
      (push table all-tables)
      (nconc all-tables (yas/snippet-table-get-all-parents table)))
    (remove-duplicates all-tables)))

(defun one-key-yas-get-mode-dir (mode)
  "Given major-mode symbol MODE, return the directory containing snippets for that mode.
If there is no snippets directory associated with that mode return `yas/root-directory'."
  (let* ((templates (yas/all-templates (one-key-yas/get-snippet-tables mode)))
         (full-file-names (mapcar #'(lambda (template) (yas/template-file template)) templates))
         (regex (regexp-opt (list (concat "/" (symbol-name mode) "/")))))
    (or (dolist (file-name full-file-names)
          (when (string-match regex file-name)
            (return (substring file-name 0 (match-end 0)))))
        yas/root-directory)))

(defun one-key-yas-filefunc (file)
  "Expand the snippet stored in snippet file FILE.
This function is used in calls to `one-key-dir-build-menu-alist'."
  (let (snippet)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (setq snippet
            (buffer-substring-no-properties 
             (re-search-forward "# --.*\n") 
             (point-max))))
    (yas/expand-snippet snippet)))

(defun one-key-yas-filename-map-func (file)
  "Return name for menu item corresponding to file or dir FILE in yasnippet one-key menus."
  (if (file-directory-p file) (file-name-nondirectory file)
    (with-temp-buffer
      (let ((full-file (file-truename file)))
        (if (file-readable-p full-file)
            (progn (insert-file-contents full-file nil nil nil t)
                   (third (yas/parse-template full-file)))
          file)))))

(defun one-key-yas-get-menu (name)
  "Return cons cell in form (menu-name . menu-alist) for yasnippet one-key menus with name NAME."
  (let* ((modename (if (string= name "yasnippet") 
                       (symbol-name (with-selected-window (previous-window) major-mode))
                     (substring name 4)))
         (mode (intern-soft modename))
         (modedir (one-key-yas-get-mode-dir mode))
         (menuname (concat "yas:" modename)))
    (cons menuname (car (one-key-dir-build-menu-alist
                         modedir
                         :filefunc 'one-key-yas-filefunc
                         :filename-map-func 'one-key-yas-filename-map-func
                         :exclude-regex "^\\.\\|~$"
                         :topdir yas/root-directory)))))

;; Set the menu-alist, title string format and special keybindings for `yasnippet' menus
(one-key-add-to-alist 'one-key-types-of-menu
                      (list "yasnippet"
                            (lambda (name) (string-match "^yas\\(:\|nippet\\)" name))
                            'one-key-yas-get-menu
                            one-key-default-title-func
                            'one-key-default-special-keybindings) t)

(provide 'one-key-yas)
;;; one-key-yas.el ends here





