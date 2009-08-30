;;; hexcolour.el --- Highlight HTML-style colour specifications

;; Copyright (C) 2003  Free Software Foundation, Inc.

;; Author: Oliver Scholz <epameinondas@gmx.de>
;; Keywords: 

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

;; This package provides a minor mode for highlighting of HTML-style
;; colour specifications, a là #324f3a. "Fine," you say, "what's so
;; special about this?" Well, when this mode is turned on, Emacs
;; highlights the colour spec _in the specified_ colour (as
;; background).

;; To activate it, put it into a directory in you `load-path' and put
;; this into your .emacs:

;; (autoload 'hexcolour-mode "hexcolour" nil t nil)

;; (add-hook <your favourite major mode hook>
;; 	  (lambda ()
;; 	    (hexcolour-mode 1)))

;; This package was implemented as a minor mode in order to work
;; around some quirks in the implementation of font-lock in the
;; current (as of summer 2003) released version of GNU Emacs. If you
;; use a recent CVS version of Emacs (the one that will probably (!)
;; become 21.5), then you shouldn't use it. You can simply something
;; like this into your .emacs:

;; (defvar hexcolour-keywords
;;   '(("#[abcdef[:digit:]]\\{6\\}"
;;      (0 (put-text-property (match-beginning 0)
;; 			   (match-end 0)
;; 			   'face (list :background 
;; 				       (match-string-no-properties 0)))))))

;; (defun hexcolour-add-to-font-lock ()
;;   (font-lock-add-keywords nil hexcolour-keywords))

;; (add-hook <your favourite major mode hook> 'hexcoulour-add-to-font-lock)

;;; Code:

(defconst hexcolour-chunk-size 500)

(defconst hexcolour-regexp "#[abcdef[:digit:]]\\{6\\}")

(defvar hexcolour-old-fontify-region-function nil)
(make-variable-buffer-local 'hexcolour-old-fontify-region-function)

(defun hexcolour-fontification-function (beg end)
  (save-restriction
    (widen)
    (while (re-search-forward hexcolour-regexp end t)
      (put-text-property (match-beginning 0)
			 (match-end 0)
			 'face (list :background
				     (match-string-no-properties 0))))))

(defun hexcolour-fontify-region-function (beg end verbose)
  (funcall hexcolour-old-fontify-region-function beg end verbose)
  (hexcolour-fontification-function beg end))

(define-minor-mode hexcolour-mode
  "Toggle `hexcolour-mode'.

With positive numeric prefix argument, turn mode on. With negative
numeric prefix arg, turn it off.

If `hexcolour-mode' is turned on, hexadecimal colour specifications
like #3253ff are displayed with the specified colour as background."
  nil "hc" nil
  (cond
   (hexcolour-mode
    ;; Turn mode on.
    (setq hexcolour-old-fontify-region-function
	  font-lock-fontify-region-function)
    (setq font-lock-fontify-region-function
	  'hexcolour-fontify-region-function)
    (font-lock-fontify-buffer))
   (t
    ;; Turn mode off.
    (setq font-lock-fontify-region-function
	  hexcolour-old-fontify-region-function)
    (setq hexcolour-old-fontify-region-function
	  nil)
    (font-lock-fontify-buffer))))

(provide 'hexcolour)
;;; hexcolour.el ends here
