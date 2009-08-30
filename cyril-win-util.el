;;; cyril-win-util.el ---  utilities for Cyrillic-CP1251 scripts. 

;; Copyright (C) 2004 Eugene Markov.
;; Licensed to the Free Software Foundation.

;; Keywords: mule, multilingual, Cyrillic

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(require 'cyril-util)

;;;###autoload
(defun setup-cyrillic-cp1251-environment ()
  "Setup multilingual environment (MULE) for Cyrillic CP1251 users."
  (interactive)
  (set-language-environment "Cyrillic-CP1251"))

;;;###autoload
(defun cyrillic-encode-cp1251-char (char)
  "Return CP1251 external character code of CHAR if appropriate."
  (aref (char-table-extra-slot cp1251-nonascii-translation-table 0)
	char))

(provide 'cyril-win-util)

;;; cyril-win-util.el ends here
