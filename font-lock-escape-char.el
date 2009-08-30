;;; font-lock-escape-char.el --- 

;; Copyright 2009 Yen-Chin,Lee
;;
;; Author: Yen-Chin,Lee
;; Version: $Id: font-lock-escape-char.el,v 0.1 2009/07/24 07:22:16 coldnew Exp $
;; Keywords: font-lock escape vim
;; X-URL: http://www.emacswiki.org/emacs/font-lock-escape-char.el

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
;;

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'font-lock-escape-char)
;;   (setq font-lock-escape-char t)  
;;
;;; Code:

(provide 'font-lock-escape-char)
(eval-when-compile
  (require 'cl))
 
;;;;##########################################################################
;;;;  User Options, Variables
;;;;##########################################################################


(defface font-lock-escape-char-face
  '((((class color)) (:foreground "seagreen2")))
  "highlight c escapes char like vim"
  :group 'font-lock-faces)

(defvar font-lock-escape-char nil
  "Non-nil means font-lock mode highlights escape char ")

(if font-lock-escape-char
    (dolist (mode '(c-mode c++-mode))
      (font-lock-add-keywords 
       mode '(("\\\\\\(?:[abfnrtv'\"?\\0]\\|x[a-fA-F]\\{2\\}\\|[0-7]\\{3\\}\\)"
               0 'font-lock-escape-char-face prepend)))))

;;; font-lock-escape-char.el ends here
