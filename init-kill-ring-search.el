;;; init-kill-ring-search.el --- Init for kill ring search

;; Filename: init-kill-ring-search.el
;; Description: Init for kill ring search
;; Author: Andy Stewart <andy@freedom>
;; Maintainer: Andy Stewart <andy@freedom>
;; Copyright (C) 2013, Andy Stewart, all rights reserved.
;; Created: 2013-12-30 09:48:49
;; Version: 0.1
;; Last-Updated: 2013-12-30 09:48:49
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/init-kill-ring-search.el
;; Keywords: 
;; Compatibility: GNU Emacs 24.3.50.1
;;
;; Features that might be required by this library:
;;
;; 
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
;; Init for kill ring search
;; 

;;; Installation:
;;
;; Put init-kill-ring-search.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-kill-ring-search)
;;
;; No need more.

;;; Customize:
;;
;; 
;;
;; All of the above can customize by:
;;      M-x customize-group RET init-kill-ring-search RET
;;

;;; Change log:
;;	
;; 2013/12/30
;;      * First released.
;; 

;;; Acknowledgements:
;;
;; 
;;

;;; TODO
;;
;; 
;;

;;; Require

(require 'kill-ring-search)

;;; Code:

(lazy-set-mode-autoload-key
 '(
   ("C-s" . kill-ring-search-prev))     ;下一个匹配
 kill-ring-search-keymap nil "kill-ring-search"
 )

(provide 'init-kill-ring-search)

;;; init-kill-ring-search.el ends here
