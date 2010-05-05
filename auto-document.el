;;; auto-document.el --- Automatic document generator of Emacs Lisp
;; $Id: auto-document.el,v 1.16 2010/05/04 09:00:52 rubikitch Exp $

;; Copyright (C) 2009  rubikitch

;; Author: rubikitch <rubikitch@ruby-lang.org>
;; Keywords: docs, convenience, lisp
;; URL: http://www.emacswiki.org/cgi-bin/wiki/download/auto-document.el

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
;;
;; This package generates auto-generatable documents in the header in
;; Emacs Lisp file. Currently it generates command list: command name
;; and first line of docstring. Document is inserted after empty line
;; below Commentary section. If auto document is found, it is
;; updated. So you can move it to any position.
;; 
;; M-x auto-document inserts/updates auto document of current buffer.
;;
;; DON'T REMOVE THE EMPTY LINE JUST AFTER GENERATED DOCUMENT! Or
;; `auto-document' eats hand-written documents.

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `auto-document'
;;    Insert or update command list of current buffer.
;;  `auto-document-test'
;;    Display generated document of FILENAME.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;
;;  `adoc-command-list-header-message'
;;    *The first line of `Commands' section.
;;    default = "Below are complete command list"
;;  `adoc-command-name-format'
;;    *Format string of listing command name.
;;    default = ";;  `%s'\n"
;;  `adoc-command-doc-format'
;;    *Format string of docstring (1st line only).
;;    default = ";;    %s\n"
;;  `adoc-option-list-header-message'
;;    *The first line of `Customizable Options' section.
;;    default = "Below are customizable option list"
;;  `adoc-option-name-format'
;;    *Format string of listing option name.
;;    default = ";;  `%s'\n"
;;  `adoc-option-doc-format'
;;    *Format string of docstring (1st line only).
;;    default = ";;    %s\n"
;;  `adoc-option-default-format'
;;    *Format string of default value.
;;    default = ";;    default = %s\n"
;;  `adoc-document-insert-position'
;;    *Auto document is inserted after the occurrence of this string.
;;    default = "\n;;; Commentary"
;;  `adoc-print-length'
;;    *Maximum length of list to print before abbreviating.
;;    default = 5
;;  `adoc-print-level'
;;    *Maximum depth of list nesting to print before abbreviating.
;;    default = 3
;;  `adoc-define-command-functions'
;;    *Define command functions.
;;    default = (quote (define-derived-mode define-compilation-mode easy-mmode-define-minor-mode define-minor-mode easy-mmode-define-global-mode ...))
;;  `adoc-exclude-file-regexp'
;;    *Regexp of files not to apply `auto-document'.
;;    default = nil

;;; Installation:
;;
;; Put auto-document.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'auto-document)
;;
;; If you want to update auto document before save, add the following.
;;
;; (add-to-list 'before-save-hook 'auto-document-maybe)


;;; Customize:
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET auto-document RET
;;

;;; TODO:
;;
;; * support sections of command
;; * support other kind of document

;;; Bug Report:
;;
;; If you have problem, send a bug report via M-x adoc-send-bug-report.
;; The step is:
;;  0) Setup mail in Emacs, the easiest way is:
;;       (setq user-mail-address "your@mail.address")
;;       (setq user-full-name "Your Full Name")
;;       (setq smtpmail-smtp-server "your.smtp.server.jp")
;;       (setq mail-user-agent 'message-user-agent)
;;       (setq message-send-mail-function 'message-smtpmail-send-it)
;;  1) Be sure to use the LATEST version of auto-document.el.
;;  2) Enable debugger. M-x toggle-debug-on-error or (setq debug-on-error t)
;;  3) Use Lisp version instead of compiled one: (load "auto-document.el")
;;  4) Do it!
;;  5) If you got an error, please do not close *Backtrace* buffer.
;;  6) M-x adoc-send-bug-report and M-x insert-buffer *Backtrace*
;;  7) Describe the bug using a precise recipe.
;;  8) Type C-c C-c to send.
;;  # If you are a Japanese, please write in Japanese:-)

;;; History:

;; $Log: auto-document.el,v $
;; Revision 1.16  2010/05/04 09:00:52  rubikitch
;; Added bug report command
;;
;; Revision 1.15  2010/04/24 00:48:28  rubikitch
;; New option: `adoc-exclude-file-regexp'
;;
;; Revision 1.14  2009/05/25 17:57:16  rubikitch
;; Bug fix
;;
;; Revision 1.13  2009/03/27 01:34:23  rubikitch
;; New option: `adoc-define-command-functions'
;; Recognize command-definition functions:
;;   define-derived-mode define-compilation-mode
;;   easy-mmode-define-minor-mode  define-minor-mode easy-mmode-define-global-mode
;;   define-global-minor-mode define-globalized-minor-mode define-generic-mode
;;
;; Revision 1.12  2009/03/27 01:18:07  rubikitch
;; silence byte-compiler / `adoc-prin1-to-string' bug fix
;;
;; Revision 1.11  2009/03/27 01:17:41  rubikitch
;; Refactor `adoc-construct' and add unit test.
;;
;; Revision 1.10  2009/03/16 19:24:50  rubikitch
;; `defun*' support
;;
;; Revision 1.9  2009/03/08 08:02:24  rubikitch
;; refactoring
;;
;; Revision 1.8  2009/03/08 07:58:54  rubikitch
;; Output default value of customizable options.
;;
;; Revision 1.7  2009/03/08 07:40:29  rubikitch
;; refactoring
;;
;; Revision 1.6  2009/03/06 03:08:41  rubikitch
;; Fix bug
;;
;; Revision 1.5  2009/03/06 01:35:10  rubikitch
;; Fix bug of duplicating Customizable Options.
;;
;; Revision 1.4  2009/03/06 01:28:37  rubikitch
;; * Renamed option `auto-document-insert-position' => `adoc-insert-position'
;; * Option List
;;
;; Revision 1.3  2009/03/06 01:08:39  rubikitch
;; * New option `auto-document-insert-position'
;; * Fix doc
;;
;; Revision 1.2  2009/03/04 15:07:57  rubikitch
;; Oops, let binding.
;;
;; Revision 1.1  2009/03/04 14:48:56  rubikitch
;; Initial revision
;;

;;; Code:

(defvar auto-document-version "$Id: auto-document.el,v 1.16 2010/05/04 09:00:52 rubikitch Exp $")
(eval-when-compile (require 'cl))
(defgroup auto-document nil
  "auto-document"
  :group 'lisp)

(defcustom adoc-command-list-header-message "Below are complete command list"
  "*The first line of `Commands' section."
  :type 'string  
  :group 'auto-document)
(defcustom adoc-command-name-format ";;  `%s'\n"
  "*Format string of listing command name."
  :type 'string
  :group 'auto-document)
(defcustom adoc-command-doc-format ";;    %s\n"
  "*Format string of docstring (1st line only)."
  :type 'string  
  :group 'auto-document)
(defcustom adoc-option-list-header-message "Below are customizable option list"
  "*The first line of `Customizable Options' section."
  :type 'string  
  :group 'auto-document)
(defcustom adoc-option-name-format ";;  `%s'\n"
  "*Format string of listing option name."
  :type 'string
  :group 'auto-document)
(defcustom adoc-option-doc-format ";;    %s\n"
  "*Format string of docstring (1st line only)."
  :type 'string  
  :group 'auto-document)
(defcustom adoc-option-default-format ";;    default = %s\n"
  "*Format string of default value."
  :type 'string  
  :group 'auto-document)
(defcustom adoc-document-insert-position "\n;;; Commentary"
  "*Auto document is inserted after the occurrence of this string."
  :type 'string  
  :group 'auto-document)
(defcustom adoc-print-length 5
  "*Maximum length of list to print before abbreviating.
See also `print-length'."
  :type 'integer  
  :group 'auto-document)
(defcustom adoc-print-level 3
  "*Maximum depth of list nesting to print before abbreviating.
See also `print-level'."
  :type 'integer  
  :group 'auto-document)
(defcustom adoc-define-command-functions
  '( define-derived-mode define-compilation-mode
     easy-mmode-define-minor-mode  define-minor-mode easy-mmode-define-global-mode
     define-global-minor-mode define-globalized-minor-mode define-generic-mode)
  "*Define command functions."
  :type 'list
  :group 'auto-document)

(defcustom adoc-exclude-file-regexp nil
  "*Regexp of files not to apply `auto-document'."
  :type 'string  
  :group 'auto-document)

(defun adoc-construct (buf)
  "Scan for command definitions in BUF and return data structure."
  (save-excursion
    (set-buffer buf)
    (goto-char (point-min))
    (adoc-construct-from-sexps
     (loop with it
           while (setq it (condition-case v (read (current-buffer)) (error nil)))
           collect it))))

(defun adoc-construct-from-sexps (sexps)
  (loop with doc
        for sexp in sexps
        for func = (car sexp)
        for doc = (ignore-errors (nth (get func 'doc-string-elt) sexp))
        when (and (stringp doc)
                  (or (and (memq func '(defun* defun))
                           (eq (car-safe (nth 4 sexp)) 'interactive))
                      (memq func adoc-define-command-functions)))
        collect (cons (nth 1 sexp) doc) into commands
        when (and (stringp doc)
                  (eq func 'defcustom))
        collect (list (nth 1 sexp) doc (nth 2 sexp)) into options
        finally (return (list commands options))))

(defun adoc-output (buf)
  "Scan for command definitions in BUF and generate command list."
  (destructuring-bind (commands options)
      (adoc-construct buf)
    (adoc-output-commands commands)
    (adoc-output-separator)
    (adoc-output-customizable-options options)))

(defun adoc-output-section (section pairs header-msg name-fmt doc-fmt)
  (adoc-output-section-header section header-msg)
  (loop for (name . doc) in pairs do
        (princ (format name-fmt name))
        (princ (format doc-fmt (substring doc 0 (string-match "$" doc))))))

(defun adoc-output-commands (pairs)
  (adoc-output-section-header "Commands" adoc-command-list-header-message)
  (loop for (name . doc) in pairs do
        (princ (format adoc-command-name-format name))
        (princ (format adoc-command-doc-format (adoc-first-line doc)))))

(defun adoc-output-customizable-options (pairs)
  (adoc-output-section-header "Customizable Options" adoc-option-list-header-message)
  (loop for (name doc default) in pairs do
        (princ (format adoc-option-name-format name))
        (princ (format adoc-option-doc-format (adoc-first-line doc)))
        (princ (format adoc-option-default-format (adoc-prin1-to-string default)))))

(defun adoc-prin1-to-string (object)
  (let ((print-escape-newlines t)
        (print-escape-nonascii t)
        (print-length adoc-print-length)
        (print-level adoc-print-level))
    (prin1-to-string object)))

(defun adoc-first-line (str)
  "Get first line of STR."
  (substring str 0 (string-match "$" str)))

(defun adoc-output-section-header (section header-msg)
  (princ (concat ";;; " section ":\n"))
  (princ ";;\n")
  (princ (format ";; %s:\n" header-msg))
  (princ ";;\n"))
                 
(defun adoc-output-separator ()
  (princ ";;\n"))

(defun adoc-prepare (buf)
  "Prepare to insert command list."
  (progn
    (set-buffer buf)
    (goto-char (point-min))
    (if (not (search-forward adoc-document-insert-position nil t))
        (error "Cannot find Commentary section")
      (cond ((search-forward "\n;;; Commands:\n" nil t)
             ;; delete old document
             (let ((s (match-beginning 0)) e) 
              (when (search-forward "\n\n" nil t)
                (setq e (1- (point)))
                (delete-region s e))))
            ((and (search-forward ";" nil t) (search-forward "\n\n" nil t))
             t)
            (t
             (error "Cannot prepare to insert command summary"))))))

(defun auto-document-maybe ()
  "Insert or update command list of current buffer if the major-mode is `emacs-lisp-mode'."
  (when (and (eq major-mode 'emacs-lisp-mode)
             (not (and adoc-exclude-file-regexp
                       (string-match adoc-exclude-file-regexp
                                     (or buffer-file-name "")))))
    (ignore-errors (auto-document))))

(defun auto-document ()
  "Insert or update command list of current buffer."
  (interactive)
  (save-excursion
    (adoc-prepare (current-buffer))
    (insert (with-output-to-string (adoc-output (current-buffer))) "\n")))

(defun auto-document-test (filename)
  "Display generated document of FILENAME."
  (interactive "fDocument for Elisp: ")
  (with-output-to-temp-buffer "*Document Generator*"
    (adoc-output (find-file-noselect filename))))
;; (auto-document-test buffer-file-name)
;; (auto-document-test "~/src/anything-config/anything-config.el")

;;;; Bug report
(defvar adoc-maintainer-mail-address
  (concat "rubiki" "tch@ru" "by-lang.org"))
(defvar adoc-bug-report-salutation
  "Describe bug below, using a precise recipe.

When I executed M-x ...

How to send a bug report:
  1) Be sure to use the LATEST version of auto-document.el.
  2) Enable debugger. M-x toggle-debug-on-error or (setq debug-on-error t)
  3) Use Lisp version instead of compiled one: (load \"auto-document.el\")
  4) If you got an error, please paste *Backtrace* buffer.
  5) Type C-c C-c to send.
# If you are a Japanese, please write in Japanese:-)")
(defun adoc-send-bug-report ()
  (interactive)
  (reporter-submit-bug-report
   adoc-maintainer-mail-address
   "auto-document.el"
   (apropos-internal "^adoc-" 'boundp)
   nil nil
   adoc-bug-report-salutation))


;;;; unit test
;; (install-elisp "http://www.emacswiki.org/cgi-bin/wiki/download/el-expectations.el")
;; (install-elisp "http://www.emacswiki.org/cgi-bin/wiki/download/el-mock.el")
(dont-compile
  (when (fboundp 'expectations)
    (expectations
      (desc "defun")
      (expect '(((foo . "foodoc")) nil)
        (adoc-construct-from-sexps
         '((defun foo ()
             "foodoc"
             (interactive)))))
      (expect '(((foo2 . "foo2doc")) nil)
        (adoc-construct-from-sexps
         '((defun* foo2 ()
             "foo2doc"
             (interactive)))))
      (expect '(nil nil)
        (adoc-construct-from-sexps
         '((defun* foo3 ()
             "foo3doc"))))
      (expect '(nil nil)
        (adoc-construct-from-sexps
         '((defun* foo4 ()
             (interactive)))))
      (expect '(nil nil)
        (adoc-construct-from-sexps
         '((defun func (arg)
             "doc"
             "string"
             ))))
      (desc "define-minor-mode")
      (expect '(((foo-mode . "foo minor mode")) nil)
        (adoc-construct-from-sexps
         '((define-minor-mode foo-mode
             "foo minor mode"))))
      (desc "defcustom")
      (expect '(nil ((custom-var "vardoc" 22)))
        (adoc-construct-from-sexps
         '((defcustom custom-var 22
             "vardoc"))))
      (desc "combination")
      (expect '(((foo . "foodoc")) ((custom-var "vardoc" 22)))
        (adoc-construct-from-sexps
         '((defcustom custom-var 22
             "vardoc")
           (defun foo ()
             "foodoc"
             (interactive)))))
      )))


(provide 'auto-document)

;; How to save (DO NOT REMOVE!!)
;; (emacswiki-post "auto-document.el")
;;; auto-document.el ends here
