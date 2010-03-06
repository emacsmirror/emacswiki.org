;;;; open-junk-file.el --- Open a junk (memo) file to try-and-error
;; $Id: open-junk-file.el,v 1.1 2010/03/05 22:17:32 rubikitch Exp $

;; Copyright (C) 2010  rubikitch

;; Author: rubikitch <rubikitch@ruby-lang.org>
;; Keywords: convenience, tools
;; URL: http://www.emacswiki.org/cgi-bin/wiki/download/open-junk-file.el

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
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; M-x `open-junk-file' opens a new file whose filename is derived from
;; current time. You can write short program in it. It helps to
;; try-and-error programs.
;;
;; For example, in Emacs Lisp programming, use M-x `open-junk-file'
;; instead of *scratch* buffer. The junk code is SEARCHABLE.
;;
;; In Ruby programming, use M-x `open-junk-file' and
;; write a script with xmpfilter annotations. It is the FASTEST
;; methodology to try Ruby methods, Irb is not needed anymore.
;; Xmpfilter is available at http://eigenclass.org/hiki/rcodetools

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `open-junk-file'
;;    Open a new file whose filename is derived from current time.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;
;;  `open-junk-file-directory'
;;    *Directory to put junk files.
;;    default = "~/junk/%Y/%m/%d-%H%M%S."
;;  `open-junk-file-find-file-function'
;;    *Function to open junk files.
;;    default = (quote find-file-other-window)

;;; Installation:
;;
;; Put open-junk-file.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'open-junk-file)
;;
;; No need more.

;;; Customize:
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET open-junk-file RET
;;


;;; History:

;; $Log: open-junk-file.el,v $
;; Revision 1.1  2010/03/05 22:17:32  rubikitch
;; Initial revision
;;

;;; Code:

(defvar open-junk-file-version "$Id: open-junk-file.el,v 1.1 2010/03/05 22:17:32 rubikitch Exp $")
(eval-when-compile (require 'cl))
(defgroup open-junk-file nil
  "open-junk-file"
  :group 'emacs)
(defcustom open-junk-file-directory "~/junk/%Y/%m/%d-%H%M%S."
  "*Directory to put junk files.
It can include `format-time-string' format specifications."
  :type 'string  
  :group 'open-junk-file)
(defcustom open-junk-file-find-file-function 'find-file-other-window
  "*Function to open junk files."
  :type 'function  
  :group 'open-junk-file)

(defun open-junk-file ()
  "Open a new file whose filename is derived from current time.
 You can write short program in it. It helps to try-and-error programs.

For example, in Emacs Lisp programming, use M-x `open-junk-file'
instead of *scratch* buffer. The junk code is SEARCHABLE."
  (interactive)
  (let* ((file (format-time-string open-junk-file-directory (current-time)))
         (dir (file-name-directory file)))
    (make-directory dir t)
    (funcall open-junk-file-find-file-function (read-string "Junk Code (Enter extension): " file))))

(provide 'open-junk-file)

;; How to save (DO NOT REMOVE!!)
;; (emacswiki-post "open-junk-file.el")
;;; open-junk-file.el ends here
