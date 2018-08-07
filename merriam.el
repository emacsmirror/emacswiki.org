;;; merriam.el --- Look up a word in WWW Merriam-Webster dictionary

;; Author: padik@live.com
;; Website: padik.t15.org

;; Version: 1.0
;; Keywords: hypermedia
;; URL: http://www.emacswiki.org/elisp/merriam.el

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

;;; Commentary:

;; Look up a word in WWW Merriam-Webster dictionary. This requires w3m browser.
;; Use M-x merriam RET word RET

;;; Acknowledgement:

;; This code is derived from webster-www.el by Tomasz J. Cholewo <t.cholewo@ieee.org>.

;;; Code:

(require 'w3m)

(defvar webster-url "http://www.m-w.com/cgi-bin/dictionary?book=Dictionary&va=")

(defun merriam (word)
        (interactive "sLook up a word: ")
                (let ((start (point)))
                        (w3m-goto-url (concat webster-url word))))

(provide 'merriam)

;;; merriam.el ends here
