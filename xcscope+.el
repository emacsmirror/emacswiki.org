;;; xscope+.el -- Providing an extension to xcscope.
;;
;; Author: Cedric Lallain <kandjar76@hotmail.com>
;; Version:  1.0
;; Keywords: xcscope cscope
;; Description: Provide an extension to xcscope.el
;; Tested with: GNU Emacs 21.x and GNU Emacs 22.x
;;
;; This file is *NOT* part of GNU Emacs.
;;
;;    This program is free software; you can redistribute it and/or modify
;;    it under the terms of the GNU General Public License as published by
;;    the Free Software Foundation; either version 2 of the License, or
;;    (at your option) any later version.
;;
;;    This program is distributed in the hope that it will be useful,
;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;    GNU General Public License for more details.
;;
;;    You should have received a copy of the GNU General Public License
;;    along with this program; if not, write to the Free Software
;;    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; CScope is a developer module for browsing source code, finding symbols...
;;  can be found here: http://cscope.sourceforge.net/
;;
;; This file is an extension of xcscope.el
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; By default CScope will do an update of the database for each research. However
;; if you're working on a big project, an update can take times...
;; On the other hand you don't want to disable the update all the times.
;;
;; This files provide function for easy mapping. Those function will temporary
;; disable the update for the current research.
;;
;; Here is a sample of key binding:
;;  (define-key global-map [(f10)]               'cscope-find-this-symbol-no-prompting-no-updates)
;;  (define-key global-map [(f12)]               'cscope-find-global-definition-no-prompting-no-updates)
;;  (define-key global-map [(control f9)]        'cscope-find-this-text-string-no-updates)
;;  (define-key global-map [(control f10)]       'cscope-find-this-symbol-no-updates)
;;  (define-key global-map [(control f11)]       'cscope-find-functions-calling-this-function-no-updates)
;;  (define-key global-map [(control f12)]       'cscope-find-global-definition-no-updates)
;;  (define-key global-map [(control shift f9)]  'cscope-find-this-text-string)
;;  (define-key global-map [(control shift f10)] 'cscope-find-this-symbol)
;;  (define-key global-map [(control shift f11)] 'cscope-find-functions-calling-this-function)
;;  (define-key global-map [(control shift f12)] 'cscope-find-global-definition)
;;  (define-key global-map [(control ?*)]        'cscope-pop-mark)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; F.Y.I:
;;
;; In order to enable cscope, you first need to build his database, for that
;; you have to start by creating a file containing a list of source files
;; Then you need to run cscope on this file.
;;
;; Here is a sample of a script which could do the trick:
;;
;; make ~/.cscope
;; find ~/work -name \*.cpp -or -name \*.h -or -name \*.c > ~/work/cscope.files
;; cscope -b -q -k
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(require 'xcscope)

(defun cscope-find-this-symbol-no-updates(symbol)
  "Locate a symbol in source code [no database update performed]."
  (interactive (list (cscope-prompt-for-symbol "Find this symbol [without updating the database]: " nil)))
  (let ((cscope-do-not-update-database t)
	(cscope-adjust t))
    (setq cscope-symbol symbol)
    (cscope-call (format "Finding symbol: %s" symbol)
		 (list "-0" symbol) nil 'cscope-process-filter
		 'cscope-process-sentinel)))

(defun cscope-find-global-definition-no-updates(symbol)
  "Find a symbol's global definition [no database update performed]."
  (interactive (list (cscope-prompt-for-symbol "Find this global definition [without updating the database]: " nil)))
  (let ((cscope-do-not-update-database t)
	(cscope-adjust t))
    (setq cscope-symbol symbol)
    (cscope-call (format "Finding global definition: %s" symbol)
		 (list "-1" symbol) nil 'cscope-process-filter
		 'cscope-process-sentinel)))

(defun cscope-find-this-text-string-no-updates(symbol)
  "Locate where a text string occurs [no database update performed]."
  (interactive (list (cscope-prompt-for-symbol "Find this text string [without updating the database]: " nil)))
  (let ((cscope-do-not-update-database t)
	(cscope-adjust t))
    (setq cscope-symbol symbol)
    (cscope-call (format "Finding text string: %s" symbol)
		 (list "-4" symbol) nil 'cscope-process-filter
		 'cscope-process-sentinel)))

(defun cscope-find-functions-calling-this-function-no-updates (symbol)
  "Display functions calling a function."
  (interactive (list (cscope-prompt-for-symbol "Find functions calling this function [without updating the database]: " nil)))
  (let ((cscope-do-not-update-database t)
	(cscope-adjust t))
    (setq cscope-symbol symbol)
    (cscope-call (format "Finding functions calling: %s" symbol)
		 (list "-3" symbol) nil 'cscope-process-filter
		 'cscope-process-sentinel)))

(defun cscope-find-global-definition-no-prompting-no-updates ()
  "Find a symbol's global definition without prompting."
  (interactive)
  (let ((symbol (cscope-extract-symbol-at-cursor nil))
	(cscope-do-not-update-database t)
	(cscope-adjust t))	 ;; Use fuzzy matching.
    (setq cscope-symbol symbol)
    (cscope-call (format "Finding global definition: %s" symbol)
		 (list "-1" symbol) nil 'cscope-process-filter
		 'cscope-process-sentinel)))

(defun cscope-find-this-symbol-no-prompting-no-updates()
  "Locate a symbol in source code [no database update performed -- no user prompting]."
  (interactive)
  (let ((symbol (cscope-extract-symbol-at-cursor nil))
	(cscope-do-not-update-database t)
	(cscope-adjust t))
    (setq cscope-symbol symbol)
    (cscope-call (format "Finding symbol: %s" symbol)
		 (list "-0" symbol) nil 'cscope-process-filter
		 'cscope-process-sentinel)))


(provide 'xcscope+)
