;;; jb-misc-functions.el --- Miscellaneous functions for keymaps & other things

;; Filename: jb-misc-functions.el
;; Description: Miscellaneous functions for use with my other libraries
;; Author: Joe Bloggs <vapniks@yahoo.com>
;; Maintainer: Joe Bloggs <vapniks@yahoo.com>
;; Copyleft (Ↄ) 2015, Joe Bloggs, all rites reversed.
;; Created: 2015-11-18 23:42:15
;; Version: 20151124.2128
;; Last-Updated: Thu Dec  3 02:19:31 2015
;;           By: Joe Bloggs
;;     Update #: 21
;; URL: https://github.com/vapniks/jb-misc-functions
;; Keywords: internal
;; Compatibility: GNU Emacs 24.5.1
;; Package-Requires:  ((dash "20151021.113") (keymap-utils "20151030.326"))
;;
;; Features that might be required by this library:
;;
;; cl-lib keymap-utils dash
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Commentary: 


;; 
;; This library contains various functions that are used in some of my other libraries.
;; So far it contains mostly functions for doing things with keys and keymaps.
;; 

;;;;;;;;

;;; Installation:
;;
;; Put jb-misc-functions.el in a directory in your load-path, e.g. ~/.emacs.d/
;; You can add a directory to your load-path with the following line in ~/.emacs
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;; where ~/elisp is the directory you want to add 
;; (you don't need to do this for ~/.emacs.d - it's added by default).
;;
;; Add the following to your ~/.emacs startup file.
;;
;; (require 'jb-misc-functions)

;;; History:
;; 24-Nov-2015    Joe Bloggs  
;;    Last-Updated: Tue Nov 24 21:27:57 2015 #5 (Joe Bloggs)
;;    Add remove-unreadable


;;; Require
(require 'cl-lib)
(require 'keymap-utils)
(require 'dash)

;;; Code:

;;;###autoload
(defun jb-eval-keymap (keymap)
  "Return the keymap pointed to by KEYMAP, or KEYMAP itself if it is a keymap."
  (cond ((kmu-keymap-variable-p keymap) (eval keymap))
        ((keymapp keymap) keymap)))

;;;###autoload
(defun jb-keymaps-in-file (file &optional eval)
  "Return a list of keymaps and variables pointing to keymaps that are used in FILE.
If EVAL is non-nil eval any variables in the returned list."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((sexps
           (cl-loop with it
                    while (setq it (condition-case v
                                       (read (current-buffer)) (error nil)))
                    collect it)))
      (cl-remove-if-not
       (lambda (x) (or (keymapp x)
                       (kmu-keymap-variable-p x)))
       (cl-remove-duplicates (-flatten sexps))))))

;;;###autoload
(defun jb-command-key-description (cmd &optional keymaps sep)
  "Return description of key-sequence for CMD.
The KEYMAPS can be a single keymap/variable or list of keymaps/variables to search for CMD,
otherwise `overriding-local-map' is searched.
By default the first keybinding will be returned, but if SEP is supplied it will be used
to seperate the descriptions of all key-sequences bound to CMD.
If there is no key-sequence for command then a string in the form \"M-x CMD\" will be returned."
  (let* ((keymaps2 (if keymaps
		       (if (listp keymaps)
			   ;; Note: don't bother trying to put this function in
			   ;; a cl-flet form, or you won't be able to do the mapcar
			   (mapcar 'eval-keymap keymaps)
			 (eval-keymap keymaps))))
	 (key (where-is-internal cmd (or keymaps2 overriding-local-map) (unless sep t))))
    (if key
        (if sep
            (mapconcat 'key-description key sep)
          (key-description key))
      (format "M-x %s" cmd))))

;;;###autoload
(defun jb-remove-unreadable (tree)
  "Remove unreadable objects from TREE.
Return value has the same structure as TREE but with all unreadable objects removed."
  (cl-subst-if nil (lambda (x)
		     (condition-case err
			 (and (atom x)
			      (read (format "%S" x))
			      nil)
		       (error t)))
	       tree))

(provide 'jb-misc-functions)

;; (org-readme-sync)
;; (magit-push)

;;; jb-misc-functions.el ends here
