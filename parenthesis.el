;;; parenthesis.el --- Insert pair of parenthesis

;; Copyright (C) 2007-2008  khiker

;; Author: khiker <khiker.mail+elisp@gmail.com>
;;         plus   <MLB33828@nifty.com>
;; Keywords: lisp

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

;; Insert pair of parenthesis.
;; If region is active and transient-mark-mode is not nil, insert
;; enclosing strings at region boundaries.
;; If the letter before the point is a backslash this packages's
;; functions do not work.
;; (This behavior is to make it easy that you write regular expression.)
;; In that case, packages functions work as the normal self-insert-command.

;; Functions:

;; style of {}   : parenthesis-insert-braces
;; style of []   : parenthesis-insert-brackets
;; style of ()   : parenthesis-insert-parens
;; style of ''   : parenthesis-insert-single-quotation
;; style of ""   : parenthesis-insert-double-quotation
;; style of <>   : parenthesis-insert-angle

;; style of `'   : parenthesis-insert-grave-and-quotation

;; style of {  } : parenthesis-insert-braces2
;; style of [  ] : parenthesis-insert-brackets2
;; style of (  ) : parenthesis-insert-parens2
;; style of '  ' : parenthesis-insert-single-quotation2
;; style of "  " : parenthesis-insert-double-quotation2
;; style of <  > : parenthesis-insert-angle2

;; Variables:

;; parenthesis-func-alist :
;;
;;   list of functions.
;;
;; parenthesis-insert-ignore-backslash :
;;
;;   If non-nil, insert pair of parensis even a backslash
;;   exists bofore point.
;;
;; parenthesis-push-mark :
;;
;;   Non-nil means to push mark to end point of close paren
;;   when region has been selected.
;;   Default value is t.

;; Prefix Arg Example:

;; C- - C-4 M-x parenthesis-insert-parens => ((((
;; C-0      M-x parenthesis-insert-parens => (
;; C-4      M-x parenthesis-insert-parens => (((())))

;; Example of Setting:

;; This package provide functions only.
;; So, it is necessary to do key setting by yourself.
;; If you do key setting as a whole, you can use a
;; parenthesis-register-keys function.
;; Usage example of this function is as follows.
;;
;; (add-hook
;;  'scheme-mode-hook
;;  (lambda()
;;    (parenthesis-register-keys "(\"[" scheme-mode-map)))
;;
;; This do key settings for scheme-mode.
;; In this, it insert a group of (, " and [ in a mass.
;; And, if c-mode, it will be as follows.
;;
;; (add-hook
;;  'c-mode-hook
;;  (lambda()
;;    (parenthesis-register-keys "{('\"[" c-mode-map)))
;;
;; In this, it insert a group of {, (, ', " and [ in a mass.
;;
;; And, if you want to insert a <div></div> in a mass,
;; (and the point is just after <div>.)
;; add the following lines in your ~/.emacs.
;;
;; (add-to-list 'parenthesis-func-alist
;;              '(parenthesis-insert-divs "<div>" "</div>"))
;; (parenthesis-init)
;;
;; The point position after inserting can be set.
;; The following setting is the point is not after the linefeed.
;; The point is after the '>'.
;;
;; (add-to-list 'parenthesis-func-alist
;;              '(parenthesis-insert-divs "<div>\n" "</div>" 5))
;; (parenthesis-init)

;; Difference of insert-pair:

;; Main Difference is follows.
;;
;;   - insert-pair is char.
;;   - parenthesis is string.
;;   - 1st argument of insert-pair enclose following arg sexps in
;;     a pair of open and close characters.
;;   - 1st argument of parenthesis repeats arg time.
;;   - insert-pair is Emacs built-in function.
;;   - parenthesis is external elisp package.


;;; Code:

;;; Variables:

(defconst parenthesis-version 0.5 "parenthesis's version")


(defgroup parenthesis nil
  "parenthesis"
  :tag "parenthesis"
  :group 'parenthesis)

(defcustom parenthesis-func-alist
  '((parenthesis-insert-braces "{" "}")
    (parenthesis-insert-braces2 "{ " " }")
    (parenthesis-insert-brackets "[" "]")
    (parenthesis-insert-brackets2 "[ " " ]")
    (parenthesis-insert-parens "(" ")")
    (parenthesis-insert-parens2 "( " " )")
    (parenthesis-insert-single-quotation "'" "'")
    (parenthesis-insert-single-quotation2 "' " " '")
    (parenthesis-insert-double-quotation "\"" "\"")
    (parenthesis-insert-double-quotation2 "\" " " \"")
    (parenthesis-insert-angle "<" ">")
    (parenthesis-insert-angle2 "< " " >")
    (parenthesis-insert-grave-and-quotation "`" "'"))
  "*list of functions for parenthesis."
  :type  '(repeat
           (list :tag "List of functions"
                 (symbol :tag "Name of function.")
                 (string :tag "open")
                 (string :tag "close")
                 (choice :tag "optional argument"
                         (const :tag "end point of open" nil)
                         (number :tag "numeric value of point"))))
  :group 'parenthesis)

(defcustom parenthesis-insert-ignore-backslash nil
  "*If non-nil, insert pair of parensis even a backslash exists bofore point."
  :type  'symbol
  :group 'parenthesis)

(defcustom parenthesis-push-mark t
  "*Non-nil means to push mark to end point of close paren
when region has been selected.
Default value is t."
  :type  'symbol
  :group 'parenthesis)


;;; Functions:

(defun parenthesis-init ()
  "Define the functions that insert pair of parenthesis."
  (interactive)
  (mapc
   (lambda (x)
     (fset (car x)
           `(lambda (rep)
              (interactive "p")
              (parenthesis-insert-internal
               rep ,(nth 1 x) ,(nth 2 x) ,(nth 3 x)))))
   parenthesis-func-alist))

(defun parenthesis-insert-internal (rep open close &optional pos)
  (if (or parenthesis-insert-ignore-backslash
          (bobp)
          (/= (char-before) ?\\))
      (cond
       ((> rep 0)
        (save-excursion
          (save-restriction
            (if (and transient-mark-mode mark-active)
                (narrow-to-region (region-beginning) (region-end))
              (narrow-to-region (point) (point)))
            (goto-char (point-min))
            (insert (parenthesis-repeat-string rep open))
            (goto-char (point-max))
            (insert (parenthesis-repeat-string rep close))))
        (when (and mark-active transient-mark-mode)
          (let ((p (region-beginning)))
            (when parenthesis-push-mark
              ;; push mark to end point of close paren.
              (push-mark (+ (region-end) (* (length close) rep))))
            (goto-char p)))
        ;; pos is non-nil: pos + length_of_open * (rep - 1)
        ;; pos is nil: lenght_of_open * rep
        (forward-char (+ (if (numberp pos) pos 0)
                         (* (length open) (if (numberp pos) (1- rep) rep)))))
       (t
        (insert (parenthesis-repeat-string (if (= rep 0) 1 (- rep)) open))))
    (self-insert-command rep)))

(defun parenthesis-repeat-string (rep str)
  "Return a string which STR times REP is repeated."
  (mapconcat 'identity (make-vector rep str) ""))

(defun parenthesis-register-keys (str map &optional spc)
  "Register keys to local-map."
  (mapc
   (lambda (x)
     (mapc
      (lambda (y)
        (when (string-equal (nth 1 x) (concat y (and spc " ")))
          (define-key map y (car x))))
      (split-string str "" t)))
   parenthesis-func-alist))


(make-variable-buffer-local 'parenthesis-insert-ignore-backslash)
(parenthesis-init)


(provide 'parenthesis)

;;; parenthesis.el ends here
