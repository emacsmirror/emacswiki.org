;;; text-translator-load.el --- Text Translator

;; Copyright (C) 2007-2009  khiker

;; Author: khiker <khiker.mail+elisp@gmail.com>
;;         plus   <MLB33828@nifty.com>

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

;; autoload for text-translator

;;; Code:

(autoload 'text-translator "text-translator" nil t)
(autoload 'text-translator-translate-last-string "text-translator" nil t)
(autoload 'text-translator-translate-by-auto-selection "text-translator" nil t)
(autoload 'text-translator-toggle-leave-string "text-translator" nil t)
(autoload 'text-translator-translate-recent-type "text-translator" nil t)
(autoload 'text-translator-translate-default "text-translator" nil t)

(provide 'text-translator-load)
;;; text-translator-load.el ends here

;; Local Variables:
;; Coding: iso-2022-7bit
;; End:
