;;; auto-complete+.el --- Auto complete plus

;; Copyright (C) 2009 ahei

;; Author: ahei <ahei0802@126.com>
;; Keywords: auto complete plus regexp

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; This library auto-complete+ extend library auto-complete with let users to
;; define regexp to ignore files when they expand use
;; ac-source-files-in-current-dir and ac-source-filename, and filter invalid
;; symbol when they expand use ac-source-symbols, because there so many garbage
;; in obarray.

;;; Installation:
;;
;; Copy auto-complete+.el to your load-path and add to your .emacs:
;;
;; (require 'auto-complete+)

;;; History:
;; 
;; 2009-11-2
;;      * initial version 1.0.

;;; Code:

(require 'auto-complete)

(defgroup auto-complete+ nil
  "Auto completion plus."
  :group 'convenience
  :prefix "ac+-")

(defcustom ac+-filename-ignore-regexp "^#.*#$\\|.*~$\\|^\\./?$\\|^\\.\\./?$\\|^.svn"
  "Regexp of filename to ignore when use AC complete."
  :type 'regexp
  :group 'auto-complete+)

(defcustom ac+-valid-symbol-fun 'ac+-valid-symbolp
  "Function to judge a symbol is a valid symbol or not."
  :type 'function
  :group 'auto-complete+)

(defun ac+-filename-candidate ()
  "Get all candidates for filename."
  (let ((dir (file-name-directory ac-prefix)))
    (ignore-errors
      (delq nil
            (mapcar
             (lambda (file)
               (unless (string-match ac+-filename-ignore-regexp file) (concat dir file)))
             (file-name-all-completions (file-name-nondirectory ac-prefix) dir))))))

(defun ac+-files-candidate ()
  "Get all candidates for files in current directory."
  (all-completions
   ac-prefix
   (delq nil
         (mapcar
          (lambda (file)
            (unless (string-match ac+-filename-ignore-regexp file) file))
          (directory-files default-directory)))))

(defun ac+-valid-symbolp (symbol)
  "Judge symbol SYMBOL is a valid symbol or not."
  (or (fboundp symbol)
      (boundp symbol)
      (generic-p symbol)
      (facep symbol)))
  
(defun ac+-symbol-candidate ()
  "Get all candidates for Emacs Lisp symbols."
  (all-completions ac-prefix obarray ac+-valid-symbol-fun))

(setq ac-source-filename '((candidates . ac+-filename-candidate)))
(setq ac-source-files-in-current-dir '((candidates . ac+-files-candidate)))
(setq ac-source-symbols '((candidates . ac+-symbol-candidate)))

(provide 'auto-complete+)

;;; auto-complete+.el ends here
