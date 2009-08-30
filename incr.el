;;; incr.el --- Increase everything as you want -*- coding: utf-8 -*-

;; Copyright 2006 Ye Wenbin
;;
;; Author: wenbinye@163.com
;; Time-stamp: <Ye Wenbin 2008-03-20 01:32:34>
;; Version: $Id: incr.el,v 1.1.1.1 2007-03-13 13:16:10 ywb Exp $
;; Keywords: convenience
;; X-URL: not distributed yet

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Quick start:
;; Type number 0 in any place, then put cursor on the number, M-x
;; incr-dwim. See the number turn to be 1. Now try with a prefix
;; argument 4: C-u 4 C-x z. The number change to 5. The number can
;; also be a hex, such as 0x1, 0x01.
;; 
;; If This is not interesting, then type character I in any place,
;; then M-x incr-dwim RET C-x z z z z z. You can try it until tired.
;; the number is increase in roman.
;;
;; Now try play with weekdays. Put cursor on the word "monday", and do
;; with the same way.
;;
;; The command also can do with a rectangle region, the feature
;; require transient-mark-mode is ON. For example, when select a
;; rectangle like this:
;;   7
;;   7
;;   7
;; then use M-x incr-dwim, the region will change to:
;;   7
;;   8
;;   9
;; When select a rectangle region, If with a numeric prefix argument,
;; the region will increase or decrease by the argument, but if the
;; prefix argument is not numberic which invoke by C-u M-x incr-dwim,
;; you will be ask several question about some paramters, such as
;; increment, justify, fill character and so on.

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'incr)

;;; Code:

(provide 'incr)
(eval-when-compile
  (require 'cl))
(require 'rect)
(require 'calculator)

(defvar incr-version "1.3")

(defgroup incr nil
  "Increase or decrease things smartly"
  :group 'convenience)

(defcustom incr-rotate-text
  '(("zero" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine")
    ("mon" "tue" "wed" "thu" "fri" "sat" "sun")
    ("monday" "tuesday" "wednesday" "thursday" "friday" "saturday" "sunday")
    ("jan" "feb" "mar" "apr" "may" "jun" "jul" "aug" "sep" "oct" "nov" "dec")
    ("january" "february" "march" "april" "may" "june" "july" "august" "september" "october" "november" "december")
    (("星期一二三四五六日") "星期一" "星期二" "星期三" "星期四" "星期五" "星期六" "星期日")
    )
  "A list of rotated text. The first element can be a list which
hold the character contain in the text. The default chars list is 
`incr-extend-chars'."
  :type 'sexp
  :group 'incr)

(defcustom incr-extend-chars "a-zA-Z0-9"
  "The rectangle region will extend when possible"
  :type 'string
  :group 'incr)


(defcustom incr-enable-feature
  '(number rotate roman date)
  "Things can handle with `incr-dwim'"
  :type (cons 'set (mapcar (lambda (type)
                             (list 'const type))
                           '(number rotate roman date han-number)))
  :set (lambda (symbol value)
         (set symbol value)
         ;; FIXME: how to avoid recursive load
         ;; (if (intern-soft "incr-try-alist")
         ;;   (load (symbol-file 'incr-enable-feature 'defvar) t))
         (message "Please reload incr again by M-x load-libary RET incr RET")
         value)
  :group 'incr)

(defvar incr-try-alist nil
  "Element: (method validator function skip-chars)
validator can be a regexp or a function")
(setq incr-try-alist
      (delete-dups
       (append
        incr-try-alist
        (if (member 'roman incr-enable-feature)
            '((roman ((incr-type . roman)
                      "^[mcdlxvi]+$")
                     incr-roman)))
        (if (member 'rotate incr-enable-feature)
            `((rotate (incr-rotate-list
                       incr-rotate-find-list)
                      incr-rotate
                      ,(concat "a-zA-Z0-9"
                               (mapconcat (lambda (text) (if (listp (car text)) (caar text)))
                                          incr-rotate-text "")))))
        (if (member 'date incr-enable-feature)
            '((date (incr-date-type
                     "^\\([0-9]+[:/\-]\\)\\{2\\}[0-9]+$")
                    incr-date
                    "0-9:/\\-")))
        (if (member 'number incr-enable-feature)
            '((digit ((incr-base . 10)
                      "^-?[0-9]+$") incr-digit "0-9a-fA-Fx+\-")
              (oct ((incr-base . 8)
                    "^0[0-7]+$") incr-oct "0-9a-fA-Fx+\-")
              (hex ((incr-base . 16)
                    "^\\(0x\\)?[0-9a-fA-F]+$") incr-hex "0-9a-fA-Fx+\-")
              (bin ((incr-base . 2)
                    "^[01]+$") incr-bin "0-9a-fA-Fx+\-")))
        (if (member 'han-number incr-enable-feature)
            '((han-gb ((incr-han-type . gb)
                       "^[零一二三四五六七八九十百千万亿点负]+$")
                      incr-han-gb "零一二三四五六七八九十百千万亿点负")
              (han-gb-currency ((incr-han-type . gb-currency)
                                "^[零壹贰参肆伍陆柒捌玖拾佰仟点负圆整万亿]+$")
                               incr-han-gb-currency
                               "零壹贰参肆伍陆柒捌玖拾佰仟点负圆整万亿")
              (han-big5 ((incr-han-type . big5)
                         "^[零一二三四五六七八九十百千萬億點]+$")
                        incr-han-big5 "零一二三四五六七八九十百千萬億點")
              (han-big5-currency ((incr-han-type . big5-currency)
                                  "^[零壹貳參肆伍陸柒捌玖拾佰仟萬億點負圓整]+$")
                                 incr-han-big5-currency
                                 "零壹貳參肆伍陸柒捌玖拾佰仟萬億點負圓整"))))))

(defvar incr-decr-p nil)

(defsubst incr-strip (str)
  "Remove blank at the front or rear of the STR"
  (replace-regexp-in-string
   "^\\s-*" ""
   (replace-regexp-in-string "\\s-*$" "" str)))

(defsubst incr-string-at (&optional pos)
  "Use `incr-extend-chars' to extract string at POS"
  (save-excursion
    (and pos (goto-char pos))
    (buffer-substring (progn (skip-chars-backward incr-extend-chars)
                             (point))
                      (progn (skip-chars-forward incr-extend-chars)
                             (point)))))

(defsubst incr-string-region (pos beg end)
  "Get string in the position POS from the column of posistion
BEG to the column of end"
  (save-excursion
    (let ((startcol (progn (goto-char beg)
                           (current-column)))
          (endcol (progn (goto-char end)
                         (current-column))))
      (goto-char pos)
      (incr-strip
       (buffer-substring
        (progn (move-to-column startcol) (point))
        (progn (move-to-column endcol)
               (skip-chars-forward incr-extend-chars)
               (point)))))))

(defun incr-get-text-preporties-any (pos prop string)
  "Get text property PROP from POS in STRING"
  (or (get-text-property pos prop string)
      (let ((pos (next-single-property-change pos prop string)))
        (if pos (get-text-property pos prop string)))))

(defun incr-get-incr-properties (pos &optional object)
  "Get text property which name start with \"incr-\" at position POS"
  (let ((props (text-properties-at pos object))
        incr-props)
    (while props
      (if (string-match "^incr-" (symbol-name (car props)))
          (setq incr-props (append incr-props (list (car props)
                                                    (cadr props)))))
      (setq props (cddr props)))
    incr-props))

(defun incr-apply-on-rectangle (incr-generator incr-formater beg end)
  "GENERATOR call with two parameters: one is text in the line,
the second is current line number. FORMATER call with one
argument: the total list in the region. This function should set
the list to be insert. Note the the list is reverse, so if need
to change the list, you should pay attention to that."
  (let ((incr-line 0)
        incr-lists)
    (apply-on-rectangle
     (lambda (scol ecol)
       (when (= (move-to-column scol t) scol)
         (setq incr-lists
               (cons 
                (funcall incr-generator
                         (buffer-substring
                          (point)
                          (progn
                            (move-to-column ecol t)
                            (skip-chars-forward incr-extend-chars)
                            (point)))
                         incr-line)
                incr-lists)
               incr-line (1+ incr-line))))
     beg end)
    (setq incr-lists (funcall (or incr-formater 'nreverse)
                              incr-lists))
    (apply-on-rectangle 
     (lambda (startcol endcol)
       (when (= (move-to-column startcol t) startcol)
         (delete-region
          (point)
          (progn (move-to-column endcol t)
                 (skip-chars-forward incr-extend-chars)
                 (point)))
         (insert (car incr-lists))
         (setq incr-lists (cdr incr-lists))))
     beg end)))

(defun incr-format-on-rectangle (arg format-generator beg end)
  "FORMAT-GENERATOR call with three arguments: first is the text
in the line, second is the line number, the third is the
increment step. This is function of FORMAT-GENERATOR is similar
to the GENERATOR function in `incr-apply-on-rectangle'."
  (let ((incr-maxlen 0) incr-arg
        param fill-char formater new-generator)
    (setq incr-arg (incr-prefix-numeric-value arg))
    (if (numberp incr-arg)
        (setq new-generator
              (lambda (text line)
                (funcall format-generator text line incr-arg))
              formater nil)
      (setq param (incr-read-parameter)
            incr-arg (car param)
            fill-char (if (string= (nth 2 param) "") 32
                        (aref (nth 2 param) 0))
            new-generator
            (lambda (text line)
              (setq text
                    (funcall format-generator text line incr-arg)
                    incr-maxlen (max incr-maxlen (length text)))
              text)
            formater
            (lambda (lists)
              (let (filled)
                (dolist (text lists)
                  (setq filled (cons
                                (if (eq (cadr param) 'right)
                                    (concat (make-string (- incr-maxlen (length text)) fill-char)
                                            text)
                                  (concat text
                                          (make-string (- incr-maxlen (length text)) ? )))
                                filled)))
                filled))))
    (incr-apply-on-rectangle new-generator formater beg end)))

(defun incr-at-point (incr-func increment &rest incr-args)
  "Call INCR-FUNC with two argument, the origin text and INCREMENT"
  (let ((pos (point))
        (beg (progn (skip-chars-backward incr-extend-chars)
                    (point)))
        (end (progn (skip-chars-forward incr-extend-chars)
                    (point))))
    (incr-apply-on-rectangle (lambda (text line)
                               (apply incr-func (append (list text increment) incr-args)))
                             'identity
                             beg end)
    (goto-char pos)))

(defun incr-read-parameter (&optional inc justify fill-char)
  (setq inc (incr-read-number inc)
        justify (completing-read "Justify(default left): "
                                 '("right" "left") justify t)
        fill-char (read-from-minibuffer (concat "Fill with(default "
                                                (if (null fill-char) "blank" fill-char)
                                                "): ") fill-char)
        justify (if (string= justify "right")
                    'right 'left))
  (list inc justify fill-char))

(defun incr-read-number (&optional default)
  "If `incr-decr-p' is non-nil, the number will be negative to
input number."
  (let ((inc (read-number (if incr-decr-p "Decrement: " "Increment: ")
                          (or default 1))))
    (if incr-decr-p (- inc) inc)))

(defun incr-prefix-numeric-value (arg &optional force)
  "like `prefix-numeric-value', except when the ARG is not a
number or null, the ARG will not change to numberic value"
  (if (or force
          (or (null arg) (numberp arg)))
      (* (if incr-decr-p -1 1) (prefix-numeric-value arg))
    arg))

(defun incr-propertize (str &rest properties)
  (apply 'propertize  (append (list str 'rear-nonsticky t)
                              properties)))

(defun incr-clear-text-property ()
  (interactive)
  (let ((props (incr-get-incr-properties (point))))
    (when props
      (remove-text-properties (previous-property-change (point))
                              (next-property-change (point))
                              props))))

;;;###autoload 
(defun incr-dwim (arg)
  "Use `incr-try-alist' to find most possible method to increase
thing at point or region"
  (interactive "P")
  (let ((methods incr-try-alist)
        (region (and mark-active transient-mark-mode))
        (chars incr-extend-chars)
        incr-extend-chars method thing beg end
        validator valid prop props)
    (while methods
      (setq method (car methods))
      (setq incr-extend-chars
            (if (and (nth 3 method) (string< "" (nth 3 method)))
                (nth 3 method)
              chars))
      (if region 
          (setq beg (region-beginning)
                end (region-end)
                thing (incr-string-region beg beg end))
        (setq thing (incr-string-at)))
      (setq validator (cadr method)
            prop (car validator)
            props (incr-get-incr-properties  0 thing)
            valid 
            (if props
                (if (listp prop)
                    (equal (cdr prop) (cadr (member (car prop) props)))
                  (member prop props))
              (if (stringp (cadr validator))
                  (string-match (cadr validator) thing)
                (funcall (cadr validator) thing))))
      (if valid
          (progn
            (apply (nth 2 method) (list arg))
            (setq methods nil))
        (setq methods (cdr methods)
              method nil)))
    (or method
        (message "Don't know how to %s this!"
                 (if incr-decr-p "decrease" "increase")))))

;;;###autoload 
(defun decr-dwim (arg)
  (interactive "P")
  (let ((incr-decr-p t))
    (incr-dwim arg)))

(defun incr-add-to-alist (method &optional before)
  (setq before (assoc before incr-try-alist))
  (let ((rest (member before incr-try-alist)))
    (setq incr-try-alist
          (delete-dups
           (append
            (butlast incr-try-alist (length rest))
            (list method)
            rest)))))

;;; number
(when (member 'number incr-enable-feature)
(defun incr-number-detect-base (str)
  (cond ((or (string-match "^0x" str)
             (string-match "[a-zA-Z]" str)) 16)
        ((and (string-match "^0[0-9]" str)
              (not (string-match "[89]" str))) 8)
        (t 10)))

(defun incr-number-detect-format (from &optional base to)
  (if (and base (= base 2))
      "%b"
    (or base (setq base (incr-number-detect-base from)))
    (let ((suffix (assoc-default base
                                 '((10 . "d") (16 . "x") (8 . "o"))))
          (width (length from))
          (prefix "%"))
      (if (let (case-fold-search) (string-match "[A-F]" from))
          (setq suffix "X"))
      (setq prefix
            (concat prefix
                    (if (string-match "^0" from)
                        (cond ((= base 10) "0")
                              ((= base 16) (concat
                                            (if (string-match "^0x" from)
                                                (progn (if (string-match "^0x0" from)
                                                           "#0" (setq width nil) "#"))
                                              "0")))
                              ((= base 8) (if (string-match "^00" from)
                                              "#0"
                                            (setq width nil) "#")))
                      (setq width nil))))
      (if width
          (progn
            (if to
                (setq to (format (format "%s%d%s" prefix width suffix) to))
              (setq to from))
            (format "%s%d%s" prefix (max (length to) width) suffix))
        (concat prefix suffix)))))

(defun incr-string-to-number (string &optional base)
  (cond ((or (null base)
             (= base 10)
             (= base 8))
         (string-to-number string base))
        ((= base 16)
         (string-to-number (replace-regexp-in-string "^0x" "" string) base))
        ((= base 2)
         (require 'calculator)
         (let ((calculator-input-radix 'bin))
           (calculator-string-to-number string)))
        (t (error "Sorry, only 2, 8, 10, 16 radix support"))))

(defun incr-number-to-string (number &optional format)
  (if (string= format "%b")
      (progn
        (require 'calculator)
        (let ((calculator-output-radix 'bin)
              (calculator-radix-grouping-mode nil))
          (calculator-number-to-string number)))
    (format format number)))

(defun incr-number (num inc &optional base format)
  (or base (setq base (or (incr-get-text-preporties-any 0 'incr-base num)
                          (incr-number-detect-base num))))
  (or format (setq format (or (if (= base 2) "%b")
                              (incr-get-text-preporties-any 0 'incr-format num)
                              (incr-number-detect-format num base))))
  (incr-propertize
   (incr-number-to-string   
    (+ (incr-string-to-number num base) inc) format)
   'incr-base base 'incr-format format))

(defun incr-number-region (arg beg end base &optional inplace)
  (let ((origin (incr-string-region beg beg end))
        (lines (count-lines beg end))
        (suffix
         (cdr (assoc base '((16 . "x") (10 . "d")
                            (8 . "o")))))
        format from)
    (setq from (incr-string-to-number origin base)
          arg (incr-prefix-numeric-value arg))
    (if (numberp arg)
        (setq format
              (incr-number-detect-format origin base
                                         (+ from (* (1- lines) arg))))
      (setq arg (incr-read-number))
      (if (= base 2)
          (setq format "%b")
        (setq format (concat "%" (read-from-minibuffer "Format(such as 03): ")
                             suffix))))
    (unless (string-match "^%[\- #0]*[0-9]*" format)
      (error "Wrong format! Please C-h f format for help"))
    (incr-apply-on-rectangle
     (if inplace
         (lambda (text line)
           (incr-number text arg base format))
       (lambda (text line)
         (incr-propertize
          (incr-number-to-string (+ from (* line arg)) format)
          'incr-base base 'incr-format format)))
     'nil beg end)))

(defmacro define-incr-number (name base chars)
  `(progn
     (defun ,(intern (concat "incr-" name)) (arg)
       (interactive "P")
       (let ((incr-extend-chars ,chars))
         (if (and mark-active transient-mark-mode)
             (incr-number-region arg (region-beginning) (region-end) ,base)
           (incr-at-point 'incr-number (incr-prefix-numeric-value arg t) ,base))))
     (defun ,(intern (concat "incr-" name "-in-region")) (arg beg end)
       (interactive "p\nr")
       (let ((incr-extend-chars ,chars))
         (incr-number-region arg beg end ,base t)))))

(define-incr-number "digit" 10 "0-9\\-")
(define-incr-number "hex" 16 "xa-fA-F0-9")
(define-incr-number "oct" 8 "0-7")
(define-incr-number "bin" 2 "01")
)

;;; rotate text
(when (member 'rotate incr-enable-feature)
(defun incr-rotate-list-at (&optional pos beg end)
  (let (list text)
    (if (setq list (get-text-property (or pos (point))
                                      'incr-rotate-list))
        (let ((incr-extend-chars incr-extend-chars))
          (if (listp (car list))
              (setq incr-extend-chars (caar list)))
          (cons (incr-string-at pos) list))
      (let ((texts incr-rotate-text)
            (chars incr-extend-chars)
            (find-function (if beg
                               'incr-string-region
                             (lambda (pos &rest ignore)
                               (incr-string-at pos))))
            incr-extend-chars thing list)
        (while texts
          (save-excursion
            (and pos (goto-char pos))
            (setq text (car texts))
            (if (listp (car text))
                (setq incr-extend-chars (caar text))
              (setq incr-extend-chars chars))
            (setq thing (funcall find-function pos beg end))
            (if (member (downcase thing) text)
                (setq texts nil)
              (setq texts (cdr texts)
                    thing nil))))
        (if thing
            (cons thing text))))))

(defun incr-rotate-region (arg beg end &optional inplace)
  (let* ((list (incr-rotate-list-at beg beg end))
         origin from len pos)
    (if (null list)
        (message "Don't known how to incease it")
      (setq origin (car list)
            list (cdr list))
      (incr-format-on-rectangle
       arg
       (if inplace
           (lambda (text line inc)
             (incr-rotate-text text (* line inc) list))
         (if (listp (car list)) (setq list (cdr list)))
         (setq from (downcase origin)
               len (length list)
               pos (- len (length (member from list))))
         (lambda (text line inc)
           (replace-regexp-in-string
            text
            (nth (mod (+ pos (* line inc)) len) list)
            text))) beg end))))

(defun incr-rotate-in-region (arg beg end)
  (interactive "p\nr")
  (incr-rotate-region arg beg end t))

(defun incr-rotate (arg)
  (interactive "P")
  (if (and mark-active transient-mark-mode)
      (incr-rotate-region arg (region-beginning) (region-end))
    (let ((list (cdr (incr-rotate-list-at)))
          (incr-extend-chars incr-extend-chars))
      (if (null list)
          (message "Don't known how to incease it")
        (if (listp (car list))
            (setq incr-extend-chars (caar list)))
        (incr-at-point 'incr-rotate-text (incr-prefix-numeric-value arg t) list)))))

(defun incr-rotate-text (arg increment &optional list)
  (let ((elm (downcase arg))
        oldlist rest len result)
    (unless list
      (setq list (incr-rotate-find-list elm)))
    (setq oldlist list)
    (if (listp (car list))
        (setq list (cdr list)))
    (if (setq rest (member elm list))
        (progn (setq len (length list))
               (incr-propertize
                (replace-regexp-in-string
                 arg
                 (nth (mod (+ (- len (length rest)) increment)
                           len) list)
                 arg)
                'incr-rotate-list oldlist))
      (error "Can't find %S in list %S" arg list))))

(defun incr-rotate-find-list (elm)
  (let ((lists incr-rotate-text)
        list)
    (setq elm (downcase elm))
    (while lists
      (if (member elm (car lists))
          (setq list (car lists)
                lists nil)
        (setq lists (cdr lists))))
    list))
)

;;; roman
(when (member 'roman incr-enable-feature)
(defvar incr-int2roman
  [(1000 . "M")
   (900 . "CM")
   (500 . "D")
   (400 . "CD")
   (100 . "C")
   (90 . "XC")
   (50 . "L")
   (40 . "XL")
   (10 . "X")
   (9 . "IX")
   (5 . "V")
   (4 . "IV")
   (1 . "I")]
  "Steal from visincr.vim. See
 http://vim.sourceforge.net/scripts/script.php?script_id=670")

(defun incr-int2roman (int)
  (if (< int 1)
      "I"
    (let ((roman "")
          (i 0))
      (while (> int 0)
        (while (>= int (car (aref incr-int2roman i)))
          (setq int (- int (car (aref incr-int2roman i)))
                roman (concat roman (cdr (aref incr-int2roman i)))))
        (setq i (1+ i)))
      roman)))

(defun incr-roman2int (roman)
  (setq roman (upcase roman))
  (let ((int 0)
        (len (length incr-int2roman))
        (idx 0))
    (while (and (string< "" roman)
                (progn
                  (while (not (string-match
                               (concat "^"
                                       (cdr (aref incr-int2roman idx)))
                               roman))
                    (setq idx (1+ idx)))
                  (< idx len)))
      (setq int (+ int (car (aref incr-int2roman idx)))
            roman (substring roman (length (cdr (aref incr-int2roman idx))))))
    int))

(defun incr-roman-1 (arg increment)
  (incr-propertize
   (incr-int2roman (+ (incr-roman2int arg) increment))
   'incr-type 'roman))

(defun incr-roman-region (arg beg end &optional inplace)
  (let ((from (incr-roman2int (incr-string-region beg beg end))))
    (incr-format-on-rectangle
     arg
     (if inplace
         (lambda (text line inc)
           (incr-roman-1 text inc))
       (lambda (text line inc)
         (incr-propertize
          (incr-int2roman (+ from (* line inc)))
          'incr-type 'roman)))
     beg end)))

(defun incr-roman (arg)
  (interactive "P")
  (if (and mark-active transient-mark-mode)
      (incr-roman-region arg (region-beginning) (region-end))
    (let ((incr-extend-chars "mcdlxviMCDLXVI"))
      (incr-at-point 'incr-roman-1 (incr-prefix-numeric-value arg t)))))
(defun incr-roman-in-region (arg beg end)
  (interactive "p\nr")
  (incr-roman-region arg beg end 'inplace))
)

;;; chinese number
(when (member 'han-number incr-enable-feature)
(require 'hannum)
(defvar incr-han-chars
  '((gb . "零一二三四五六七八九十百千万亿点负")
    (big5 . "零一二三四五六七八九十百千萬億點")
    (gb-currency . "零壹贰参肆伍陆柒捌玖拾佰仟点负圆整万亿")
    (big5-currency . "零壹貳參肆伍陸柒捌玖拾佰仟萬億點負圓整")))

(defvar incr-han-convertor
  '((gb hannum-gb-to-number hannum-number-to-gb)
    (big5 hannum-big5-to-number hannum-number-to-big5)
    (gb-currency hannum-gb-to-currency hannum-currency-to-gb)
    (big5-currency hannum-big5-to-currency hannum-currency-to-big5))
  "")

(defun incr-han-detect-type (str)
  (let ((types incr-han-chars)
        type)
    (while types
      (if (string-match (concat "^[" (cdar types) "]+$") str)
          (setq type (car types)
                types nil)
        (setq types (cdr types))))
    type))
  
(defun incr-han-number (num increment &optional type)
  (or type (setq type (car (incr-han-detect-type num))))
  (let ((convert (cdr (assoc type incr-han-convertor))))
    (incr-propertize
     (funcall (cadr convert) (+ (funcall (car convert) num) increment))
     'incr-han-type type)))

(defun incr-han-number-region (arg beg end &optional type inplace)
  (let ((incr-extend-chars (cdr (assoc type incr-han-chars)))
        convert from)
    (setq from (incr-string-region beg beg end))
    (or type (setq type (car (incr-han-detect-type from))))
    (setq convert (cdr (assoc type incr-han-convertor))
          from (funcall (car convert) from))
    (incr-format-on-rectangle
     arg
     (if inplace
         (lambda (text line inc)
           (incr-han-number text inc type))
       (lambda (text line inc)
         (funcall (cadr convert) (+ from (* line inc)))))
     beg end)))

(defmacro define-incr-han-number (type)
  `(progn
     (defun ,(intern (concat "incr-han-" (symbol-name type))) (arg)
       (interactive "P")
       (if (and mark-active transient-mark-mode)
           (incr-han-number-region arg (region-beginning) (region-end) ',type)
         (let ((incr-extend-chars (cdr (assoc ',type incr-han-chars))))
           (incr-at-point 'incr-han-number (incr-prefix-numeric-value arg t)
                          ',type))))
     (defun ,(intern (concat "incr-han-" (symbol-name type) "-in-region")) (arg beg end)
       (interactive "p\nr")
       (incr-han-number-region arg beg end ',type t))))

(define-incr-han-number gb-currency)
(define-incr-han-number big5-currency)

(define-incr-han-number gb)
(define-incr-han-number big5)

)

;;; date
(when (member 'date incr-enable-feature)
(defvar incr-date-format
  '((second . "%S")
    (min . "%M")
    (hour . "%H")
    (day . "%d")
    (month . "%m")
    (year . "%y")))

(defvar incr-date-type
  '((ymd 86400 year month day)
    (dmy 86400 day month year)
    (mdy 86400 month day year)
    (hms 1 hour min second)))

(defun incr-parse-time-string (str)
  (if (string-match "[\-/:,]" str)
      (let ((token (match-string 0 str)))
        (cons token
              (mapcar 'string-to-number (split-string str token))))))

(defun incr-date-parse (date type)
  (let (from token type-format format
             second min hour year month day)
    (if (setq from (incr-parse-time-string date))
        (progn
          (setq type-format (cdr (assoc type incr-date-type))
                type-format (cdr type-format)
                token (car from)
                from (cdr from))
          (map 'list (lambda (sym val)
                       (set sym val))
               '(second min hour day month year)
               (butlast (decode-time) 3))
          (dolist (i type-format)
            (set i (car from))
            (push
             (if (eq i 'year)
                 (if (> (car from) 99)
                     "%Y"
                   (set i (+ (car from)
                             (if (< (car from) 37) 2000 1900)))
                   "%y")
               (cdr (assoc i incr-date-format))) format)
            (setq from (cdr from)))
          (cons
           (float-time
            (encode-time second min hour day month year))
           (mapconcat 'identity (nreverse format) token)))
      (error "Not support date format!"))))

(defun incr-date-1 (date increment &optional type)
  (let (from type-format)
    (if (setq from (incr-parse-time-string date))
        (progn
          (or type
              (progn
                (setq type (incr-get-text-preporties-any 0 'incr-date-type date))
                (unless (assoc type incr-date-type)
                  (setq type
                        (intern (completing-read "Which type of date: "
                                                 '("ymd" "mdy" "dmy" "hms") nil t))))))
          (setq type-format (cdr (assoc type incr-date-type))
                increment (* (car type-format) increment)
                date (incr-date-parse date type))
          (incr-propertize
           (format-time-string
            (cdr date)
            (seconds-to-time (+ (car date) increment)))
           'incr-date-type type))
      (error "Not support date format!"))))

(defun incr-date-in-region (arg beg end)
  (interactive "p\nr")
  (incr-date-region arg beg end 'inplace))

(defun incr-date-region (arg beg end &optional inplace)
  (let ((incr-extend-chars "0-9\-/:")
        (from (incr-string-region beg beg end))
        increment type)
    (setq type
          (or
           (incr-get-text-preporties-any 0 'incr-date-type from)
           (unless (assoc type incr-date-type)
             (intern (completing-read "Which type of date: "
                                      '("ymd" "mdy" "dmy" "hms") nil t)))))
    (setq from (incr-date-parse from type))
    (incr-apply-on-rectangle
     (if inplace
         (lambda (text line)
           (incr-date-1 text arg type))
       (setq increment (cadr (assoc type incr-date-type)))
       (lambda (text line)
         (incr-propertize
          (format-time-string
           (cdr from)
           (seconds-to-time (+ (car from) (* line increment))))
          'incr-date-type type)))
     nil beg end)))

(defun incr-date (arg)
  (interactive "p")
  (if (and mark-active transient-mark-mode)
      (incr-date-region arg (region-beginning) (region-end))
    (let ((incr-extend-chars "0-9\-/:"))
      (incr-at-point 'incr-date-1 (incr-prefix-numeric-value arg t)))))
)

;;; incr.el ends here
