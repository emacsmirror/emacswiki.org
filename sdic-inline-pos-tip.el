;;; sdic-inline-pos-tip.el -- Extension for sdic-inline-mode using pos-tip.el

;; Copyright (C) 2010 S. Irie

;; Author: S. Irie
;; Maintainer: S. Irie
;; Keywords: Tooltip, Dictionary

(defconst sdic-inline-pos-tip-version "0.0.3")

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.

;; It is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:

;; This program was written as a sample of pos-tip.el library, and
;; provides the tooltip showing word meanings at cursor position
;; like `rikaichan' Firefox extension, using sdic-inline.el library
;; which was written by khiker.

;;
;; Installation:
;;
;; First, save this file as pos-tip.el and byte-compile in
;; a directory that is listed in load-path.
;;
;; Put the following in your .emacs file:
;;
;;   (require 'sdic-inline-pos-tip)
;;   (setq sdic-inline-display-func 'sdic-inline-pos-tip-show)
;;   (define-key sdic-inline-map "\C-c\C-p" 'sdic-inline-pos-tip-show)
;;
;; and start emacs, then system is enabled.
;;

;;; History:
;; 2010-03-09  S. Irie
;;         * Changed to use `pos-tip-split-string'
;;              (*Require pos-tip.el ver. 0.1.0 or higher*)
;;         * Version 0.0.3
;;
;; 2010-03-08  S. Irie
;;         * Added options to use substitutive functions in non-X frame
;;         * Version 0.0.2
;;
;; 2010-03-07  S. Irie
;;         * First release
;;         * Version 0.0.1

;; ToDo:

;;         * Word wrap

;;; Code:

(require 'sdic-inline)
(require 'pos-tip)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar sdic-inline-pos-tip-timeout-auto 5
  "Timeout of tooltip for automatic popup (in seconds).
See `pos-tip-show' for details.")

(defvar sdic-inline-pos-tip-timeout-man 0
  "Timeout of tooltip for manual popup (in seconds).
See `pos-tip-show' for details.")

(defvar sdic-inline-pos-tip-max-width 80
  "Maximum width of tooltip. nil means use display width.")

(defface sdic-inline-pos-tip
  '((t
     :foreground "white"
     :background "RoyalBlue4"))
  "Face for description in sdic-inline-pos-tip's tooltip.")

(defface sdic-inline-pos-tip-entry
  '((t
     :foreground "cyan"
     :bold t
     :inherit sdic-inline-pos-tip))
  "Face for entry in sdic-inline-pos-tip's tooltip.")

(defvar sdic-inline-pos-tip-subst-func-auto
  'sdic-inline-display-minibuffer
  "Function used as substitute for auto-popup in non-X frame.")

(defvar sdic-inline-pos-tip-subst-func-man
  'sdic-inline-display-popup
  "Function used as substitute for manual-popup in non-X frame.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sdic-inline-pos-tip-show (&optional entry)
  "Show tooltip which describes the word meanings at current point."
  (interactive)
  (cond
   ((eq window-system 'x)
    (if (interactive-p)
	(setq entry sdic-inline-last-entry))
    (when entry
      ;; Use the same font as selected frame in tooltip
      (set-face-font 'sdic-inline-pos-tip (frame-parameter nil 'font))
      (let (width-list)
	;; Main part
	(pos-tip-show-no-propertize
	 ;; Arange string
	 (mapconcat
	  (lambda (item)
	    (let ((head (sdicf-entry-headword item))
		  (desc (pos-tip-split-string (sdicf-entry-text item)
					      sdic-inline-pos-tip-max-width 1)))
	      ;; Record all row width in order to calculate tooltip width
	      (setq width-list (cons (string-width head)
				     (nconc (mapcar 'string-width desc)
					    width-list)))
	      ;; Propertize entry string by appropriate faces
	      (concat (propertize head 'face 'sdic-inline-pos-tip-entry)
		      "\n"
		      (mapconcat
		       (lambda (row)
			 (propertize row 'face 'sdic-inline-pos-tip))
		       desc "\n"))))
	  entry "\n")
	 ;; Face which specifies tooltip's background color
	 'sdic-inline-pos-tip
	 ;; Display current point, then omit POS and WINDOW
	 nil nil
	 ;; Timeout
	 (if (interactive-p)
	     sdic-inline-pos-tip-timeout-man
	   sdic-inline-pos-tip-timeout-auto)
	 ;; Calculate tooltip's pixel size
	 (pos-tip-tooltip-width (apply 'max width-list) (frame-char-width))
	 (pos-tip-tooltip-height (length width-list) (frame-char-height))))))
   ;; If non-X frame, use substitutive function
   ((interactive-p)
    (if (commandp sdic-inline-pos-tip-subst-func-man)
	(call-interactively sdic-inline-pos-tip-subst-func-man)))
   ((functionp sdic-inline-pos-tip-subst-func-auto )
    (funcall sdic-inline-pos-tip-subst-func-auto entry))))


(provide 'sdic-inline-pos-tip)

;;;
;;; sdic-inline-pos-tip.el ends here
