;;;; esh-myparser.el --- Define original Eshell cmdline parser by bypassing original parser
;; Time-stamp: <2011-03-20 17:03:21 rubikitch>

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

(defvar eshell-bypassing-parser nil)
(defadvice eshell-parse-command-input (around eshell-myparser activate)
  "If your own eshell parser is available, use it."
  (setq eshell-bypassing-parser nil)
  (let ((parser-cmd (eshell-get-parser-command (ad-get-arg 0) (ad-get-arg 1))))
    (if (not parser-cmd)
        ad-do-it
      (setq eshell-bypassing-parser t)
      (setq ad-return-value parser-cmd))))
;; (progn (ad-disable-advice 'eshell-parse-command-input 'around 'eshell-myparser) (ad-update 'eshell-parse-command-input))

(defadvice eshell-hist-parse-arguments (around eshell-myparser activate)
  "To avoid error in `eshell-parse-arguments'.

For example `<' char is interpreted input redirection."
  (ignore-errors ad-do-it))
;; (progn (ad-disable-advice 'eshell-hist-parse-arguments 'around 'eshell-my) (ad-update 'eshell-hist-parse-arguments))



(defadvice eshell-invoke-directly (around eshell-myparser activate)
  "Return t if your own eshell parser is usable."
  (if eshell-bypassing-parser
      (setq ad-return-value nil)
    ad-do-it))
;; (progn (ad-disable-advice 'eshell-invoke-directly 'around 'eshell-myparser) (ad-update 'eshell-invoke-directly))

(defun eshell-get-parser-command (s e)
  "Use your own eshell parser,
if the first word in command line is XX and
`eshell-parser/XX' function is defined,

Given command line region between S and E."
  (when (save-excursion (goto-char s)
                        (re-search-forward "\\(\\S +\\) " nil t))
    (let ((func (intern-soft (format "eshell-parser/%s" (match-string 1)))))
      (when (fboundp func)
        `(eshell-commands
          (progn
            (run-hooks 'eshell-pre-command-hook)
            (eshell-update-markers eshell-last-output-end)
            (let ((ret (,func ,(buffer-substring (match-end 0) e))))
              (eshell-named-command (car ret) (mapcar 'eshell-escape-arg (cdr ret))))
            (run-hooks 'eshell-post-command-hook)))))))

(defadvice eshell-complete-commands-list (after eshell-myparser activate)
  "Add eshell-parser/* to completion."
  (setq ad-return-value
        (append (mapcar (lambda (name) (substring name 14))
                        (all-completions (concat "eshell-parser/" pcomplete-stub)
                                         obarray 'functionp))
                ad-return-value)))
;; (progn (ad-disable-advice 'eshell-complete-commands-list 'after 'eshell-myparser) (ad-update 'eshell-complete-commands-list))

(defadvice eshell-expand-history-references (around eshell-myparser activate)
  "Prevent cmdline from parsing."
  (if eshell-bypassing-parser
      (setq ad-return-value nil)
    ad-do-it))
;; (progn (ad-disable-advice 'eshell-expand-history-references 'around 'eshell-myparser) (ad-update 'eshell-expand-history-references))

;;; Utility function
(defun eshell-get-process-output (program &rest args)
  "Get output string from PROGRAM with ARGS."
  (let ((tempbuf (generate-new-buffer " *eshell-get-process-output*")))
    ;; FIXME use start-process
    (apply 'call-process program nil tempbuf nil args)
    (prog1 (with-current-buffer tempbuf (buffer-string))
      (kill-buffer tempbuf))))

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
