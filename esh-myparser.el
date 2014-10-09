;;;; esh-myparser.el --- Define original Eshell cmdline parser by bypassing original parser
;; Time-stamp: <2014-10-09 21:49:36 rubikitch>

;; Copyright (C) 2011,2012  rubikitch

;; Author: rubikitch <rubikitch@ruby-lang.org>
;; Keywords: eshell, languages
;; URL: http://www.emacswiki.org/cgi-bin/wiki/download/esh-myparser.el

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
;;
;; Pass entire content of eshell command line to other program, such
;; as, zsh or ruby.
;;
;; Example of ruby:
;;
;; $ rb 1+3
;; 4
;; $ rb "hogehoge".length
;; 8
;; $ rb ["foo", "bar", "baz"].sort
;; ["bar", "baz", "foo"]
;; $ rb Math.cos(Math::PI)
;; -1.0
;; $ rb include Math; cos(PI)
;; -1.0
;;
;; Example of zsh:
;; $ z for i (1 2 3) echo $i
;; 1
;; 2
;; 3


;;; Commands:
;;
;; Below are complete command list:
;;
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;

;;; Installation:
;;
;; Put esh-myparser.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'esh-myparser)
;;
;; No need more.

;;; Customize:
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET esh-myparser RET
;;


;;; History:

;; See http://www.rubyist.net/~rubikitch/gitlog/eshell-dx.txt

;;; Code:

(eval-when-compile (require 'eshell))
(defgroup esh-myparser nil
  "esh-myparser"
  :group 'eshell)

;;; [2014-10-09 Thu] new implementation
(defvar eshell-myparsers-regexp-cache nil)
(defun eshell-myparsers-regexp ()
  (setq eshell-myparsers-regexp-cache
        (or eshell-myparsers-regexp-cache
            (regexp-opt (mapcar (lambda (cmd) (substring (symbol-name cmd) 14))
                                (apropos-internal "^eshell-parser/" 'fboundp))
                        t))))
(defun eshell-update-myparsers ()
  (interactive)
  (setq eshell-myparsers-regexp-cache nil)
  (eshell-myparsers-regexp))
(add-hook 'eshell-mode-hook 'eshell-update-myparsers)

;; (defun eshell-parser/xxx (str) str)
;; (fmakunbound 'eshell-parser/xxx)

(defun eshell-parse-argument-hook--myparser ()
  ;; ex. "rb[3+3]"
  (when (looking-at (concat (eshell-myparsers-regexp) "\\[\\(.+\\)\\]"))
    (goto-char (1- (match-beginning 2))) ;point is at [
    (let ((s (1+ (point))))
      (goto-char (1+ (eshell-find-delimiter ?\[ ?\]))) ;after ]
      (eshell-finish-arg `(eshell--myparser ,(match-string 1) ,(buffer-substring s (1- (point)))))))
  ;; ex. "rb 3+3"
  (when (looking-at (concat (eshell-myparsers-regexp) " "))
    (goto-char (match-end 0))
    (let ((s (point)))
      (end-of-line)
      (eshell-finish-arg `(eshell--myparser ,(match-string 1) ,(buffer-substring s (point)))))))

(defun eshell--myparser (name arg)
  (funcall (intern (format "eshell-parser/%s" name)) arg))
(defun eshell-rewrite-command-hook--myparser (terms)
  (if (and (listp (car terms))
	   (eq (caar terms) 'eshell--myparser))
      (cl-destructuring-bind (cmd &rest args) (eval (car terms))
        `(eshell-named-command ,cmd
                               (list ,@(mapcar (lambda (x) `(eshell-escape-arg ,x)) args))))))
(add-hook 'eshell-parse-argument-hook 'eshell-parse-argument-hook--myparser)
(add-hook 'eshell-rewrite-command-hook 'eshell-rewrite-command-hook--myparser)


;;; Sample parser: Ruby
;;;
;;; Usage: $ rb ANY_RUBY_SCRIPT
(defun eshell-parser/rb (str &optional ruby)
  (list (or ruby "ruby") "-rpp" "-e"
   (format "PP.pp(eval(ARGV[0]),$>,%d)" (window-width))
   "--"
   str))

;;; Sample parser: bash
;;;
;;; Usage: $ b ANY_BASH_SCRIPT
(defun eshell-parser/b (str &optional sh)
  (list (or sh "bash") "-c" str))

;;; Sample parser: zsh
;;;
;;; Usage: $ b ANY_ZSH_SCRIPT
(defun eshell-parser/z (str &optional sh)
  (list (or sh "zsh") "-c" str))

(provide 'esh-myparser)

;; How to save (DO NOT REMOVE!!)
;; (progn (git-log-upload) (emacswiki-post "esh-myparser.el"))
;;; esh-myparser.el ends here
