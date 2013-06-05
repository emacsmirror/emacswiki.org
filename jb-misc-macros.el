;;; jb-misc-macros.el --- Miscellaneous macros

;; Filename: jb-misc-macros.el
;; Description: Miscellaneous macros
;; Author: Joe Bloggs <vapniks@yahoo.com>
;; Maintainer: Joe Bloggs <vapniks@yahoo.com>
;; Copyleft (Ↄ) 2013, Joe Bloggs, all rites reversed.
;; Created: 2013-06-05 01:38:24
;; Version: 0.1
;; Last-Updated: 2013-06-05 01:38:24
;;           By: Joe Bloggs
;; URL: https://github.com/vapniks/jb-misc-macros
;; Keywords: lisp
;; Compatibility: GNU Emacs 24.3.1
;; Package-Requires: ((macro-utils "1.0"))
;;
;; Features that might be required by this library:
;;
;; macro-utils.el
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
;; along with this program; see the file COPYING.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary: 
;;
;; Bitcoin donations gratefully accepted: 1AoGev8FTwVVspNZxHuu8LMztAwxdzRndZ
;;
;; This library contains miscellaneous macros that I use in my projects, or that
;; I thought might be useful.

;;; Examples:
;;
;; Prompt the user for a free keybinding:
;; (untilnext (read-key-sequence "Enter a key: ")
;;            (read-key-sequence "That key is already bound to a command. Try again: ")
;;            (lambda (x) (not (key-binding x))))

;; Keep a track of the number of prompts
;; (untilnext (read-number "What is 1+1? Attempt 1: ")
;;            (prog1 (read-number  (concat "Wrong! Try again. Attempt " (number-to-string num) ": "))
;;              (setq num (1+ num)))
;;            (lambda (x) (= x 2))
;;            (num 1))


;;; Installation:
;;
;; Put jb-misc-macros.el in a directory in your load-path, e.g. ~/.emacs.d/
;; You can add a directory to your load-path with the following line in ~/.emacs
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;; where ~/elisp is the directory you want to add 
;; (you don't need to do this for ~/.emacs.d - it's added by default).
;;
;; Add the following to your ~/.emacs startup file.
;;
;; (require 'jb-misc-macros)

;;; Change log:
;;	
;; 2013/06/05
;;      * First released.
;; 

;;; Acknowledgements:
;;
;; Lars Brinkhoof for macro-utils.el, Paul Graham for his book "On Lisp"
;;

;;; TODO
;;
;; More macros.
;;

;;; Require
(require 'macro-utils)

;;; Code:

(defmacro untilnext (initform nextform &optional testfunc &rest bindings)
  "Evaluate INITFORM followed by NEXTFORM repeatedly. Stop when one of them returns non-nil, and returning that value.
If TESTFUNC is supplied it should be a function that takes a single argument (the results of evaluating INITFORM or NEXTFORM),
and will be used as the stopping criterion. In this case evaluation will stop when TESTFUNC returns non-nil, but the
return value of the macro will still be the return value of INITFORM or NEXTFORM.
If BINDINGS are supplied then these will be placed in a let form wrapping the code, thus allowing for some persistence of state
between successive evaluations of NEXTFORM."
  (once-only (initform)
             (let ((retval (gensym)))
               `(let* (,@bindings ,retval)
                  (or (and ,testfunc
                           (or (and (funcall ,testfunc ,initform) ,initform)
                               (while (not (funcall ,testfunc (setq ,retval ,nextform))))
                               ,retval))
                      ,initform
                      (and 
                       (while (not (setq ,retval ,nextform)))
                       ,retval))))))

;; This might be better as an inline function.
(defmacro list-subset (indices list)
  "Return elements of LIST corresponding to INDICES."
  `(mapcar (lambda (i) (nth i ,list)) ,indices))

(provide 'jb-misc-macros)

;; (magit-push)
;; (yaoddmuse-post "EmacsWiki" "jb-misc-macros.el" (buffer-name) (buffer-string) "update")

;;; jb-misc-macros.el ends here
