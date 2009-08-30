;;; perlcritic.el --- Call perlcritic in Emacs

;; Copyright (C) 2007 Free Software Foundation, Inc.
;;
;; Author: Ye Wenbin <wenbinye@gmail.com>
;; Maintainer: Ye Wenbin <wenbinye@gmail.com>
;; Created: 31 Dec 2007
;; Version: 0.01
;; Keywords: tools, convenience

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

;; 

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'perlcritic)

;;; Code:

(eval-when-compile
  (require 'cl))
(require 'compile)

(defgroup perlcritic nil
  "Call perlcritic"
  :group 'tools)

(defcustom perlcritic-program "perlcritic"
  "The perlcritic program used by `perlcritic'."
  :type 'string
  :group 'perlcritic)

(defcustom perlcritic-profile t
  "Specify an alternate .perlcriticrc file.
If value is nil, use -noprofile,
If value is t, use the default profile .perlcriticrc,
If non-nil, and the profile exists, use the profile."
  :type 'string
  :group 'perlcritic)

(defcustom perlcritic-severity nil
  "Severity level for perlcritic.
Directs perlcritic to only report violations of Policies with a
severity greater than N. Severity values are integers ranging
from 1 (least severe) to 5 (most severe). The default is 5. For a
given -profile, decreasing the -severity will usually produce
more violations. Users can redefine the severity for any Policy
in their .perlcriticrc file."
  :type '(radio
          (const :tag "default (gentle)" nil)
          (const :tag "stern"  4)
          (const :tag "harsh"  3)
          (const :tag "cruel"  2)
          (const :tag "brutal" 1))
  :group 'perlcritic)

(defcustom perlcritic-top nil
  "Report only the top N Policy violations in each file.
If the -severity option is not explicitly given, the -top option
implies that the minimum severity level is 1. Users can redefine
the severity for any Policy in their .perlcriticrc file."
  :type 'integer
  :group 'perlcritic)

(defcustom perlcritic-include nil
  "Directs \"perlcritic\" to apply additional Policies that match the regex \"/PATTERN/imx\"."
  :type 'string
  :group 'perlcritic)

(defcustom perlcritic-exclude nil
  "Directs \"perlcritic\" to not apply any Policy that matches the regex
\"/PATTERN/imx\"."
  :type 'string
  :group 'perlcritic)

(defcustom perlcritic-force nil
  "Directs \"perlcritic\" to ignore the magical \"## no critic\"
pseudo-pragmas in the source code. You can set the default value for this
option in your .perlcriticrc file."
  :type 'boolean
  :group 'perlcritic)

(defcustom perlcritic-verbose nil
  "Sets the numeric verbosity level or format for reporting violations.
Please do use the verbose level give by perlcritic, otherwise you
should setup compilation error regexp by yourself."
  :type 'integer
  :group 'perlcritic)

(defvar perlcritic-used-verbose
  [nil perlcritic-v1-2 perlcritic-v1-2 perlcritic-v3
       perlcritic-v4-8-10 perlcritic-v5 perlcritic-v6-9-11
       perlcritic-v7 perlcritic-v4-8-10 perlcritic-v6-9-11
       perlcritic-v4-8-10 perlcritic-v6-9-11]
  "A list of verbose tag that used.
Set the verbose you are not used to nil to avoid add them to
`compilation-error-regexp-alist-alist'.")

(defvar perlcritic-error-regexp-alist
  (let* ((file "\\(\\S-+\\)")
         (line "\\([0-9]+\\)")
         (col line)
         (other "[^\n]+")
         (severity "(Severity: \\(?:\\(5\\)\\|\\(4\\)\\|\\([0-9]\\)\\))"))
    `((perlcritic-v1-2 ,(concat "^" file ":\\(?: (\\)?" line ":" col)
                       1 2 3)
      (perlcritic-v3 ,(concat "at " file " line " line) 1 2)
      (perlcritic-v4-8-10 ,(concat "^\\(?:\\sw+[^:]\\|\\[\\S-+\\)\\s-+"
                                   other "at \\(line\\) " line
                                   ", column " col other
                                   "\\(?:\n[^\n]+\\)?" severity)
                          1 2 3 (5 . 6))
      (perlcritic-v6-9-11 ,(concat "^\\(?:\\sw+[^:]\\|\\[\\S-+\\)\\s-+"
                                   other "at \\(line\\) " line
                                   ", near " other
                                   "\\(?:\n[^\n]+\\)?" severity)
                          1 2 nil (4 . 5))
      (perlcritic-v5 ,(concat "^" file ": " other "at line " line
                              ", column " col other severity)
                     1 2 3 (5 . 6))
      (perlcritic-v7 ,(concat "^" file ": " other "at line " line
                              " near " other severity)
                     1 2 nil (4 . 5))))
  "Alist for perlcritic error regexp")
    
(mapc (lambda (tag)
        (when tag
          (add-to-list 'compilation-error-regexp-alist-alist
                       (assq tag perlcritic-error-regexp-alist))
          (add-to-list 'compilation-error-regexp-alist tag)))
      (delete-dups (append perlcritic-used-verbose nil)))

(defvar perlcritic-regexp nil)
(defvar perlcritic-buffer nil)
(defvar perlcritic-line-offset nil)

(defun perlcritic-file-name (name)
  "Get the right error file."
  (if (file-exists-p name)
      name
    (buffer-file-name perlcritic-buffer)))

(defun perlcritic-filter (proc string)
  "Remove colors and adjust line number."
  (setq string
        (replace-regexp-in-string "\033\\[\\([0-9;]*\\)m" "" string))
  (with-current-buffer (process-buffer proc)
    (when (local-variable-if-set-p 'perlcritic-line-offset)
      (let ((re (car perlcritic-regexp))
            (match (nth 2 perlcritic-regexp)))
        (setq string (replace-regexp-in-string
                      re 
                      (lambda (text)
                        (number-to-string (+ (string-to-number (match-string match text))
                                             perlcritic-line-offset -1)))
                      string nil nil match)))))
  (compilation-filter proc string))

(defun perlcritic-command ()
  (or (get 'perlcritic-program 'has-perlcritic)
      (if (executable-find perlcritic-program)
          (put 'perlcritic-program 'has-perlcritic t)
        (error "Seem perlcritic is not installed")))
  (mapconcat
   'identity
   (append
    (list perlcritic-program)
    (if (and (stringp perlcritic-profile)
             (file-exists-p perlcritic-profile))
        (list "-profile" perlcritic-profile))
    (if (null perlcritic-profile)
        (list "-noprofile"))
    (if perlcritic-severity
        (list (format "-%d" perlcritic-severity)))
    (if perlcritic-top
        (list "-top" (number-to-string perlcritic-top)))
    (if perlcritic-include
        (list "-include" perlcritic-include))
    (if perlcritic-exclude
        (list "-exclude" perlcritic-exclude))
    (if perlcritic-force
        (list "-force"))
    (if perlcritic-verbose
        (list "-verbose" (number-to-string perlcritic-verbose))))
   " "))
          
(defun perlcritic-setup (command &optional region)
  "Add handler for find-file and adjust line number."
  (with-current-buffer next-error-last-buffer
    (set (make-local-variable 'perlcritic-regexp)
         (if (string-match "-verbose\\s-+\\([0-9]+\\)" command)
             (cdr (assq (aref (string-to-number (match-string 1 command))
                              perlcritic-used-verbose)
                        perlcritic-error-regexp-alist))
           '("at line \\([0-9]+\\)" nil 1)))
    (set (make-local-variable 'perlcritic-buffer) buf)
    (set (make-local-variable 'compilation-parse-errors-filename-function)
         'perlcritic-file-name)
    (setq perlcritic-line-offset nil))
  (set-process-filter (get-buffer-process next-error-last-buffer) 'perlcritic-filter))

;;;###autoload 
(defun perlcritic ()
  "Call perlcritic.
If region selected, call perlcritic on the region, otherwise call
perlcritic use the command given."
  (interactive)
  (let ((buf (current-buffer)))
    (setq compile-command (concat (perlcritic-command) " "
                                  (file-name-nondirectory buffer-file-name)))
    (unless (string= compile-command (car compile-history))
      (setq compile-history (cons compile-command compile-history)))
    (call-interactively 'compile)
    (perlcritic-setup compile-command)))

;;;###autoload 
(defun perlcritic-region (beg end)
  (interactive "r")
  (let ((buf (current-buffer))
        (line (line-number-at-pos beg)))
    ;; exclude strict, because most region does't has strict pragam
    (setq compile-command (concat (perlcritic-command) " -exclude strict "))
    (unless (string= compile-command (car compile-history))
      (setq compile-history (cons compile-command compile-history)))
    (call-interactively 'compile)
    (perlcritic-setup compile-command)
    (with-current-buffer next-error-last-buffer
      (set (make-local-variable 'perlcritic-line-offset) line))
    ;; if select region, send the region
    (let ((proc (get-buffer-process next-error-last-buffer)))
      (send-region proc beg end)
      (process-send-eof proc))))

(provide 'perlcritic)
;;; perlcritic.el ends here
