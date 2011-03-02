;;;; joseph-autopair.el  Another autopair or skeleton. 

;; Filename: joseph-autopair.el
;; Description:   Another autopair or skeleton. 
;; Author: Joseph <jixiuf@gmail.com>
;; Maintainer: Joseph <jixiuf@gmail.com>
;; Copyright (C) 2011~, Joseph, all rights reserved.
;; Created: 2011-03-02
;; Version: 0.1.0
;; URL: http://www.emacswiki.org/joseph-autopair.el
;; Keywords: autopair parentheses skeleton
;; Compatibility: (Test on GNU Emacs 23.2.1).
;;
;;
;;; This file is NOT part of GNU Emacs
;;
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
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;;  this package doesn't use skeleton or anything like this.
;;  it use `after-change-functions' hook.
;;  when your buffer changed(insert or delete) ,then
;;  `after-change-functions' is activated.
;;  So I can judge  whether need to insert some "tail string".
;;  for example:  for "(" the "tail string" is ")".
;;  you needn't binding keys on parentheses or any paired character.
;;  just typing. so it is more sensitive.
;;
;;
;;  I also remaped:
;;  `delete-backward-char'
;;  `backward-delete-char-untabify'
;;  so that when you press `Backspace' between "(" and ")"
;;  both "(" and ")" is deleted.
;;
;;  This is only enabled when the "tail" is `stringp'
;;  what is that means?
;;  because "tail" can be a `string' or a lisp sentence
;;  when it is a `string' , it will be inserted directly.
;;  when it is a lisp sentence ,it will be eval.
;;  see the default value of `joseph-autopair-alist'.
;;
;;      `joseph-autopair-origin-backward-delete-char-untabify'
;;      `joseph-autopair-origin-delete-backward-char'
;;  are the original
;;  `backward-delete-char-untabify' `delete-backward-char'.
;;
;;
;;  Actually: 
;;  a pair like this in `joseph-autopair-alist':
;;                    ("[" "]")
;;  is equals to:
;;                    ("[" (save-excursion (insert "]")))
;;
;;  but a litter difference exists :
;;  when the "tail" is `stringp' then I can get the length of tail
;;  eazyly, so that I can delete or skip it depending
;;  on the length of "tail".
;;  that means only string type "tail" can be skipped
;   and auto deleted .
;;  
;;
;;
;;; Install:
;;
;; Just put joseph-autopair.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'joseph-autopair)
;; and custom joseph-autopair-alist if you want ,
;; (joseph-autopair-toggle-autopair) ;;enable joseph-autopair.
;; 

;;; Commands:
;;
;; Below are complete command list:
;;
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;
;;  `joseph-autopair-alist'
;;    doc.
;;    default = (quote ((emacs-lisp-mode ... ... ... ...) (lisp-interaction-mode ... ... ... ...) (c-mode ... ... ... ... ...) (java-mode ... ... ... ... ...) (sh-mode ... ...)))

(defgroup joseph-autopair nil
  " Autoinsert parentheses or other
things you defined in pairs."
  :group 'convenience
  :prefix "joseph-autopair-"
  )

(defcustom joseph-autopair-alist
      '( (emacs-lisp-mode . (
                             ("\"" "\"")
                             ("`" "'")
                             ("(" ")")
                             ("[" "]")
                             ))
         (lisp-interaction-mode . (
                                   ("\"" "\"")
                                   ("`" "'")
                                   ("(" ")")
                                   ("[" "]")
                                   ))
         ( c-mode . (
                     ("\"" "\"" )
                     ("'" "'")
                     ("(" ")" )
                     ("[" "]" )
                     ("{" (joseph-autopair-newline-indent-insert "}")) 
                     ))
         (java-mode . (
                       ("\"" "\"")
                       ("'" "'")
                       ("(" ")")
                       ("[" "]")
                       ("{" (joseph-autopair-newline-indent-insert "}"))
                       ))
         (sh-mode . ( ;;just a example
                     ("if" (joseph-autopair-newline-indent-insert "fi"))
                     ("begin" (progn
                              (insert " end")
                              (end-of-line)
                              ))
                     )))
      "doc."
      :group 'joseph-autopair
      )

(defun joseph-autopair-newline-indent-insert(str)
  "insert new line and insert str ,put point on the
new line and indent the region."
  (let((begin (point-at-bol))
       end)
    (newline)
    (newline)
    (insert str)
    (setq end (point))
    (forward-line -1)
    (indent-region begin end)
    )
  )

(defun joseph-autopair-editing-find-head(string-list)
  "using `looking-back' find which string in
 `string-list' is before point,and return it."
  (let (found-string head)
    (while (and (not found-string) string-list)
      (setq head (car-safe string-list))
      (when (and  (stringp head)
                  (looking-back (regexp-quote head)))
        (setq found-string head))
      (setq string-list (cdr-safe string-list))
      )
    found-string
    ))

(defun joseph-autopair-delete-backward-char
  ( N &optional KILLP)
  (interactive "*p\nP")
  (when (and (boundp 'major-mode)
             (member major-mode (mapcar 'car joseph-autopair-alist)))
    (let* ((mode-pair (cdr (assoc major-mode joseph-autopair-alist)))
           (heads (mapcar 'car mode-pair))
           (head   (joseph-autopair-editing-find-head heads))
           )
      (when head
        (let ((tail (nth 1 (assoc head mode-pair))))
          (print tail)
          (when (stringp tail)
            (delete-char (length  tail)
                         ))))
      (joseph-autopair-origin-delete-backward-char  N KILLP)
      ))  
  )

(defun joseph-autopair-backward-delete-char-untabify
  (  ARG &optional KILLP)
  (interactive "*p\nP")
  (when (and (boundp 'major-mode)
             (member major-mode (mapcar 'car joseph-autopair-alist)))
    (let* ((mode-pair (cdr (assoc major-mode joseph-autopair-alist)))
           (heads (mapcar 'car mode-pair))
           (head   (joseph-autopair-editing-find-head heads))
           )
      (when head
        (let ((tail (nth 1 (assoc head mode-pair))))
          (when (stringp tail)
            (delete-char (length  tail)
                         ))))
      (joseph-autopair-origin-backward-delete-char-untabify ARG  KILLP)
      )))

(defalias 'joseph-autopair-origin-backward-delete-char-untabify (symbol-function 'backward-delete-char-untabify))
(defalias 'joseph-autopair-origin-delete-backward-char (symbol-function 'backward-delete-char))


(defun joseph-autopair-after-change-function (first last len)
  (when (and (= len 0)
             (boundp 'major-mode)
             (member major-mode (mapcar 'car joseph-autopair-alist)))
    (let* ( (mode-pair (cdr (assoc major-mode joseph-autopair-alist)))
            (heads (mapcar 'car mode-pair))
            (head   (joseph-autopair-editing-find-head heads))
            tail 
            )
      (if (and head
               (not
                (and (stringp (setq tail (nth 1 (assoc head mode-pair)))) 
                     (string-equal head tail)
                     (looking-at (regexp-quote head)))
                )
           )
          (joseph-autopair-insert-or-eval-tail (assoc head mode-pair));;insert tail
        (joseph-autopair-skip-tail first last mode-pair heads);; skip tail
        ))))

(defun joseph-autopair-skip-tail (first last mode-pair heads)
  "skip `tail'."
  (let ((new-inserted (buffer-substring first last))
        head
        tail)
    (joseph-autopair-origin-backward-delete-char-untabify (length new-inserted))
    (setq head   (joseph-autopair-editing-find-head heads))
    (if head 
        (progn
          (setq tail (nth 1 (assoc head mode-pair)))
          (if (and (stringp tail)
                   (looking-at (regexp-quote new-inserted))
                   (looking-at (regexp-quote tail)))
              (forward-char (length tail))
            (insert new-inserted)
            )
          )
      (insert new-inserted)
      )))


(defun joseph-autopair-insert-or-eval-tail(pair)
  " if param `pair' is string insert it
if not ,eval it."
  (let* ((tail (nth 1 pair)))
    (if (stringp tail)
        (save-excursion (insert tail))
      (eval tail)
      )
    ))

(defvar joseph-autopair-activated-p nil)

(defun joseph-autopair-toggle-autopair()
  (interactive)
  (if joseph-autopair-activated-p
      (progn
        (defalias 'backward-delete-char-untabify  (symbol-function 'joseph-autopair-origin-backward-delete-char-untabify))
        (defalias 'backward-delete-char  (symbol-function 'joseph-autopair-origin-delete-backward-char))
        (remove-hook 'after-change-functions 'joseph-autopair-after-change-function)
        (setq joseph-autopair-activated-p nil)
        (message "joseph-autopair is deactivated now!")
        )
    (defalias 'backward-delete-char-untabify  (symbol-function 'joseph-autopair-backward-delete-char-untabify))
    (defalias 'backward-delete-char  (symbol-function 'joseph-autopair-delete-backward-char))
    (add-hook 'after-change-functions 'joseph-autopair-after-change-function)
    (setq joseph-autopair-activated-p t)
    (message "joseph-autopair is activated now!")
    )
  )

(provide 'joseph-autopair)
;;joseph-autopair.el ends here.
