;;; smart-dnd.el --- user-configurable drag-n-drop feature

;; Copyright (C) 2003-2008, 2012  by Seiji Zenitani

;; Author: Seiji Zenitani <zenitani@mac.com>
;; $Id: smart-dnd.el 756 2012-06-13 14:11:21Z zenitani $
;; Keywords: tools
;; Created: 2003-04-27
;; Compatibility: Emacs 22
;; URL(en): http://www.emacswiki.org/emacs/smart-dnd.el
;; URL(jp): http://th.nao.ac.jp/MEMBER/zenitani/elisp-j.html#smart-dnd

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

;;; Commentary

;; This package provides user-configurable drag-n-drop feature to Emacs 22.
;;
;; Usage:
;;
;; First, evaluate `smart-dnd-setup' function with an alist in the buffer.
;; The code modifies drag-n-drop behaviour in the local buffer and then
;; a string "image file: file.png" will be inserted when *.png file is dropped.
;;
;; (require 'smart-dnd)
;; (smart-dnd-setup
;;  '(
;;    ("\\.png\\'" . "image file: %f\n")
;;    ("\\.jpg\\'" . "image file: %f\n")
;;    (".exe\\'"   . (message (concat "executable: " f)))
;;    (".*"        . "any filename: %f\n")
;;    ))
;;
;; String elements will be formatted by `smart-dnd-string'.
;; You can also put elisp expression into the alist.
;; In the case of ".exe" in the above list, a local variable 'f'
;; will be replaced by the dropped filename in the expression.

(provide 'smart-dnd)

;;; smart-dnd.el ends here
