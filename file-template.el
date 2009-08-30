;;; file-template.el --- File templates

;; Copyright (C) 2007  Scott Frazer

;; Author: Scott Frazer <frazer.scott@gmail.com>
;; Maintainer: Scott Frazer <frazer.scott@gmail.com>
;; Created: 29 Nov 2007
;; Version: 1.0
;; Keywords: template

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
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

;; Insert template into buffer, performing tag expansions.  Inspired by
;; auto-insert and auto-insert-tkld.
;;
;; See `file-template-tag-alist' for predefined tags.
;;
;; Add this to your .emacs if you want to insert templates explicitly:
;;
;; (autoload 'file-template-auto-insert "file-template" nil t)
;;
;; You can also have templates inserted into new files automatically
;; by setting `file-template-insert-automatically' appropriately and
;; adding this to your .emacs:
;;
;; (autoload 'file-template-find-file-not-found-hook "file-template" nil t)
;; (add-hook 'find-file-not-found-hooks 'file-template-find-file-not-found-hook 'append)
;;
;;; Change log:
;;
;; 29 Nov 2007 -- v1.0
;;                Initial creation

;;; Code:

(require 'custom)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Custom variables

(defgroup file-template nil
  "*File templates."
  :group 'file-template)

(defcustom file-template-insert-automatically nil
  "*Insert file-template automatically.
Can be one of the following values:

nil - do not insert automatically.
t   - always insert automatically.
ask - ask whether to insert or not."
  :group 'file-template
  :type '(choice (const :tag "No"  nil)
                 (const :tag "Yes" t)
                 (const :tag "Ask" 'ask)))

(defcustom file-template-login-name (user-login-name)
  "*User's login name."
  :group 'file-template
  :type 'string)

(defcustom file-template-full-name (user-full-name)
  "*User's full name."
  :group 'file-template
  :type 'string)

(defcustom file-template-num-prefix "0"
  "*String used as prefix for numerical days and months.
Suggested values are \" \", \"0\" and \"\"."
  :group 'file-template
  :type 'string)

(defcustom file-template-paths '("~/insert/" "/usr/share/emacs/insert/")
  "*List of directories where templates are."
  :group 'file-template
  :type '(repeat string))

(defcustom file-template-search-current-dir t
  "*Search current directory of buffer for templates before `file-template-paths'."
  :group 'file-template
  :type 'boolean)

(defcustom file-template-mapping-alist
  '(("\\.el$" . "template.el")
    ("\\.c$" . "template.c")
    ("\\.\\(cc\\|cpp\\|C\\)$" . "template.cc")
    ("\\.h\\(pp\\)?$" . "template.h")
    ("[Mm]akefile" . "template.mk")
    ("\\.sh$" . "template.sh")
    ("\\.csh$" . "template.csh")
    ("\\.pl$" . "template.pl")
    ("\\.py$" . "template.py"))
  "*Alist mapping filename or extension to a template."
  :group 'file-template
  :type 'alist)

(defcustom file-template-insert-hook nil
  "*List of functions to call after inserting a template."
  :group 'file-template
  :type 'hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Internal variables

(defvar file-template-history nil
  "Minibuffer history list for templates.")

(defvar file-template-prompt-start-point nil
  "Starting point for prompt string.")

(defvar file-template-prompted-strings nil
  "Strings prompted for (to fill in %1-%9 later in template).")

(defvar file-template-eval-start-point nil
  "Starting point for eval string.")

(defvar file-template-final-point nil
  "Where point should go after the template is done being inserted.")

(defvar file-template-tag-alist
  '(("u" . file-template-login-name)
    ("U" . file-template-full-name)
    ("a" . user-mail-address)
    ("f" . (buffer-file-name))
    ("b" . (file-name-nondirectory (buffer-file-name)))
    ("n" . (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))
    ("N" . (upcase (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))))
    ("e" . (file-name-extension (buffer-file-name)))
    ("E" . (upcase (or (file-name-extension (buffer-file-name)) "")))
    ("p" . (file-name-directory (buffer-file-name)))
    ("d" . (file-template-pad-num-string (file-template-get-date-item 'day)))
    ("m" . (file-template-get-month-num))
    ("M" . (file-template-get-date-item 'month))
    ("y" . (substring (file-template-get-date-item 'year) -2))
    ("Y" . (file-template-get-date-item 'year))
    ("q" . (fill-paragraph nil))
    ("[" . (file-template-start-prompt))
    ("]" . (file-template-finish-prompt))
    ("1" . (file-template-get-nth-prompted 1))
    ("2" . (file-template-get-nth-prompted 2))
    ("3" . (file-template-get-nth-prompted 3))
    ("4" . (file-template-get-nth-prompted 4))
    ("5" . (file-template-get-nth-prompted 5))
    ("6" . (file-template-get-nth-prompted 6))
    ("7" . (file-template-get-nth-prompted 7))
    ("8" . (file-template-get-nth-prompted 8))
    ("9" . (file-template-get-nth-prompted 9))
    ("(" . (file-template-start-eval))
    (")" . (file-template-finish-eval))
    ("%" . "%")
    ("@" . (setq file-template-final-point (point))))
  "Lookup table mapping % tags to variable/function.  Return a string
to be inserted into the buffer; non-strings are ignored.  Predefined
tags are:

 %u       user's login name
 %U       user's full name
 %a       user's mail address (from the variable `user-mail-address')
 %f       file name with path
 %b       file name without path
 %n       file name without path and extension
 %N       file name without path and extension, capitalized
 %e       file extension
 %E       file extension capitalized
 %p       file directory
 %d       day
 %m       month
 %M       abbreviated month name
 %y       last two digits of year
 %Y       year
 %q       `fill-paragraph'
 %[ %]    prompt user for a string
 %1-%9    refer to the nth strings prompted for with %[ %]
 %( %)    elisp form to be evaluated
 %%       inserts %
 %@       sets the final position of `point'")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions

(defun file-template-get-date-item (item)
  "item can be 'day, 'month, or 'year."
  (let ((time-string (current-time-string)))
    (when (string-match "^\\w+\\s-+\\(\\w+\\)\\s-+\\([0-9]+\\)\\s-+[0-9]+:[0-9]+:[0-9]+\\s-+\\([0-9]+\\)" time-string)
      (cond
       ((equal item 'day)
        (match-string 2 time-string))
       ((equal item 'month)
        (match-string 1 time-string))
       ((equal item 'year)
        (match-string 3 time-string))))))

(defun file-template-get-month-num ()
  "Get month as a number."
  (let ((month (file-template-get-date-item 'month))
        (month-names '("Dec" "Nov" "Oct" "Sep" "Aug" "Jul" "Jun" "May" "Apr" "Mar" "Feb" "Jan")))
    (catch 'found
      (while month-names
        (if (string= month (car month-names))
            (throw 'found (file-template-pad-num-string (int-to-string (length month-names))))
          (setq month-names (cdr month-names)))))))

(defun file-template-pad-num-string (num-string)
  "Pad out a number string."
  (let ((result (concat file-template-num-prefix num-string)))
    (if (> (length result) 2)
        (substring result -2)
      result)))

(defun file-template-get-nth-prompted (n)
  "Get nth prompted string."
  (if (> n (length file-template-prompted-strings))
      (error "There are only %d prompted strings so far, and you tried to expand %%%d"
             (length file-template-prompted-strings) n)
    (nth (1- n) file-template-prompted-strings)))

(defun file-template-start-prompt ()
  "Start prompting for input."
  (if file-template-prompt-start-point
      (error "Nested prompts are not allowed")
    (setq file-template-prompt-start-point (point))))

(defun file-template-finish-prompt ()
  "Finish prompting for input."
  (if (not file-template-prompt-start-point)
      (error "No matching %%[")
    (let ((prompt (buffer-substring file-template-prompt-start-point (point))) answer)
      (delete-region file-template-prompt-start-point (point))
      (setq answer (read-string prompt))
      (setq file-template-prompted-strings (append file-template-prompted-strings (list answer)))
      (setq file-template-prompt-start-point nil)
      answer)))

(defun file-template-start-eval ()
  "Start eval of elisp."
  (if file-template-eval-start-point
      (error "Nested evals are not allowed")
    (setq file-template-eval-start-point (point))))

(defun file-template-finish-eval ()
  "Finish eval of elisp."
  (if (not file-template-eval-start-point)
      (error "No matching %%(")
    (let ((form (buffer-substring file-template-eval-start-point (point))))
      (delete-region file-template-eval-start-point (point))
      (save-excursion
        (save-restriction
          (setq file-template-eval-start-point nil)
          (eval (car (read-from-string form))))))))

;;;###autoload
(defun file-template-insert (template)
  "Insert template into buffer, performing tag expansions.
See `file-template-tag-alist' for list of predefined tags.

Use this function when you don't want to insert the default template
associated with the file type in `file-template-mapping-alist'.
Otherwise, use `file-template-auto-insert'."
  (interactive "fTemplate to insert? ")
  (setq file-template-prompt-start-point nil)
  (setq file-template-prompted-strings '())
  (setq file-template-eval-start-point nil)
  (setq file-template-final-point nil)
  (save-restriction
    (narrow-to-region (point) (point))
    (insert-file-contents template)
    (let (char result)
      (while (search-forward "%" nil t)
        (delete-char -1)
        (setq char (char-after))
        (delete-char 1)
        (setq result (assoc (char-to-string char) file-template-tag-alist))
        (if (not result)
            (error "Unknown tag %%%c" char)
          (setq result (eval (cdr result)))
          (when (stringp result)
            (insert result))))
      (when file-template-final-point
        (goto-char file-template-final-point)))
    (run-hooks 'file-template-insert-hook)))

;;;###autoload
(defun file-template-auto-insert ()
  "Insert default template into buffer."
  (interactive)
  (let ((mapping-alist file-template-mapping-alist)
        template-name template)
    (setq template-name
          (catch 'found
            (while mapping-alist
              (if (string-match (caar mapping-alist) (buffer-name))
                  (throw 'found (cdar mapping-alist))
                (setq mapping-alist (cdr mapping-alist))))))
    (if (not template-name)
        (message (format "No template defined for file type \"%s\"" (buffer-name)))
      (setq template (locate-file template-name (if file-template-search-current-dir
                                                    (cons "." file-template-paths)
                                                  file-template-paths)))
      (if (not template)
          (message (format "Couldn't find template \"%s\"" template-name))
        (file-template-insert template)))))

;;;###autoload
(defun file-template-find-file-not-found-hook ()
  "Hook to (optionally) insert the default template when a new file is created."
  (when (or (equal file-template-insert-automatically t)
            (and (equal file-template-insert-automatically 'ask)
                 (y-or-n-p "Insert default template? ")))
    (file-template-auto-insert)))

(provide 'file-template)
;;; file-template.el ends here
