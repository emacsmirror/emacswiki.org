;;; ac-R.el --- Autocompletion routines for R
;;
;; Filename: ac-R.el
;; Description: Autocompletion for R.
;; Author: Matthew L. Fidler
;; Maintainer: Matthew L. Fidler
;; Created: Mon Aug 23 15:11:28 2010 (-0500)
;; Version: 0.2
;; Last-Updated:
;;           By:
;;     Update #: 46
;; URL:
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  NOTE:  ESS SVN has out-of-the-box integration with AC from v12.02
;;  See http://www.emacswiki.org/emacs/ESSAuto-complete for details.
;;
;;  Please don't use this file if you use new
;;  version of ESS, or it might have unpredictable consequences. 
;;  Send a mail to ess-help for support.
;;
;;Put this file into load-path'ed directory, and byte compile it if
;; desired.  And put the following expression into your ~/.emacs.
;;
;; (require 'ac-R)
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;; 17-Jan-2012 Vitalie Spinu
;;    Removed caching,  it was meaningless in this context.
;;    Replaced the completion mechanism with the function ess-R-get-rcompletions.
;;    Replaced regexp search by a function to recognize dots, brackets etc.
;;    Replaced lambdas in hooks.
;; 7-July-2011    Zigler Zhang
;;    Quote local variable to avoid "Symbol nil may not be buffer-local"
;;    when start process of ESS
;; 23-Mar-2011    Timothy C. Harper
;;    Hook up R documentation in completion popup.
;; 22-Oct-2010    Matthew L. Fidler
;;    Added caching mechanism for 3 characters or less
;; 23-Aug-2010    Matthew L. Fidler
;;    Initial Version
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(defun ess-get-help-text (sym)
  (interactive)
  (require 'ess-help)
  (with-temp-buffer
    (ess-command (format inferior-ess-help-command sym) (current-buffer))
    (ess-help-underline)
    (buffer-substring (point-min) (point-max))))

(defun ess-get-ac-start ()
  (let ((chars "]A-Za-z0-9.$@_:["))
    (when (string-match (format "[%s]" chars) (char-to-string (char-before)))
      (save-excursion
	(re-search-backward (format "[^%s]" chars) nil t)
	(1+ (point))) ;; else nil
      )))

(setq ac-source-R
      '((prefix     . ess-get-ac-start)
        (requires   . 1)
        (candidates . ess-R-get-rcompletions)
        (document   . ess-get-help-text)))


(defun ess-R-get-rcompletions ()
  "Calls R internal complation utilities for posible completions"
  (let ((beg-of-line (save-excursion (comint-bol nil) (point)))
	(line-buffer ))
    (ess-get-words-from-vector (format
				"{utils:::.assignLinebuffer('%s')
			          utils:::.assignEnd(%d)
			          utils:::.guessTokenFromLine()
			          utils:::.completeToken()
			          utils:::.retrieveCompletions()}\n"
				(buffer-substring beg-of-line (point-at-eol))
				(- (point) beg-of-line))
			       )))


(add-to-list 'ac-modes 'ess-mode)

(defun ess-ac-init ()
  (setq ac-sources '(ac-source-R ac-source-filename))
  (make-local-variable 'ac-ignore-case)
  (setq ac-ignore-case nil))

(add-hook 'ess-mode-hook 'ess-ac-init)
(add-hook 'inferior-ess-mode-hook 'ess-ac-init)

(provide 'ac-R)
