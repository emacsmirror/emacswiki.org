;;; usage-memo.el --- integration of Emacs help system and memo

;; $Id: usage-memo.el,v 1.16 2010/05/09 21:36:21 rubikitch Exp $

;; Copyright (C) 2007, 2010  rubikitch

;; Author: rubikitch <rubikitch@ruby-lang.org>
;; Keywords: convenience, languages, lisp, help, tools, docs
;; URL:http://www.emacswiki.org/cgi-bin/wiki/download/usage-memo.el

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

;; This program enables you to write annotation in *Help* and
;; third-party help systems. When we do programming, we often use
;; Emacs help system (ie. describe-function). Do you want to take a
;; note in the *Help* buffer and want Emacs to show your note later?
;; In other words, integration of Emacs help and your memo!


;;; Bug Report:
;;
;; If you have problem, send a bug report via M-x umemo-send-bug-report.
;; The step is:
;;  0) Setup mail in Emacs, the easiest way is:
;;       (setq user-mail-address "your@mail.address")
;;       (setq user-full-name "Your Full Name")
;;       (setq smtpmail-smtp-server "your.smtp.server.jp")
;;       (setq mail-user-agent 'message-user-agent)
;;       (setq message-send-mail-function 'message-smtpmail-send-it)
;;  1) Be sure to use the LATEST version of usage-memo.el.
;;  2) Enable debugger. M-x toggle-debug-on-error or (setq debug-on-error t)
;;  3) Use Lisp version instead of compiled one: (load "usage-memo.el")
;;  4) Do it!
;;  5) If you got an error, please do not close *Backtrace* buffer.
;;  6) M-x umemo-send-bug-report and M-x insert-buffer *Backtrace*
;;  7) Describe the bug using a precise recipe.
;;  8) Type C-c C-c to send.
;;  # If you are a Japanese, please write in Japanese:-)

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `umemo-save'
;;    Save current usage memo(annotation) into file.
;;  `usage-memo-mode'
;;    Automatically enabled minor mode to add usage-memo feature by `define-usage-memo'.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;

;; Annotation files are stored below ~/memo/umemo by default. Its
;; subdirectories are categories. And their subdirectories are entry
;; annotation files. Because annotation files are read in each case,
;; you can generate annotation files automatically.

;;; Supported help systems:

;; describe-function
;; describe-variable
;; slime-documentation (slime.el)
;; slime-describe-symbol (slime.el)
;; slime-describe-function (slime.el)
;; ri (ri-ruby.el)
;; lh-refe (langhelp / ReFe)
;;
;; usage-memo is general-purpose: it is easy to support other help systems.

;;; Installation:

;; PREPEND to .emacs:
;;   (require 'usage-memo)
;;   (umemo-initialize)

;; Usage-memo must be loaded earlier than other minor
;; modes. Otherwise, `usage-memo-mode-map' takes precedence over them:
;; you cannot use other minor modes with usage-memo.
;; [EVAL IT] (describe-variable 'minor-mode-map-alist)

;; If you support other help systems, add `define-usage-memo' sexps in
;; .emacs or redefine `umemo-initialize'.

;;; Usage:

;; * Switch to *Help*.
;; * Write your annotation below `===*===*===*===*===*===*===*===*===*===*===' line.
;; * Press C-x C-s to save annotation.
;; * Even if *Help* is killed or Emacs is restarted,
;;   your annotation is shown when you look up the same entry!!

;;; Further information:

;; [EVAL IT] (describe-function 'define-usage-memo)
;; [EVAL IT] (describe-function 'usage-memo-mode)
;; [EVAL IT] (describe-variable 'umemo-base-directory)
;; [EVAL IT] (describe-function 'umemo-pathname)
;; [EVAL IT] (describe-variable 'umemo-category)
;; [EVAL IT] (describe-variable 'umemo-entry-name)
;; [EVAL IT] (describe-variable 'umemo-current-entry-name)

;;; History:

;; $Log: usage-memo.el,v $
;; Revision 1.16  2010/05/09 21:36:21  rubikitch
;; * unbind letter chars in `usage-memo-mode-map'
;; * Store major mode when `usage-memo-mode' is turned on.
;;
;; Revision 1.15  2010/05/09 21:27:55  rubikitch
;; Automatical change major-mode in memo area.
;;
;; Revision 1.14  2010/05/09 04:14:26  rubikitch
;; `umemo-send-bug-report': fix variable regexp
;;
;; Revision 1.13  2010/05/04 08:43:01  rubikitch
;; Added bug report command
;;
;; Revision 1.12  2010/03/23 08:26:53  rubikitch
;; `describe-mode' support
;;
;; Revision 1.11  2007/08/30 19:00:46  rubikitch
;; *** empty log message ***
;;
;; Revision 1.10  2007/07/20 13:02:52  rubikitch
;; SLIME: SBCL, CMUCL, and CLISP support.
;;
;; Revision 1.9  2007/07/18 19:32:24  rubikitch
;; set header-line
;;
;; Revision 1.8  2007/07/18 17:49:41  rubikitch
;; In SLIME, entry name is full name, ie PACKAGE:SYMBOL.
;;
;; Revision 1.7  2007/07/12 06:56:36  rubikitch
;; Use emacswiki-post at `How to save'.
;;
;; Revision 1.6  2007/07/12 06:28:44  rubikitch
;; usage-memo-mode-map: bind SPC .. ~ to self-insert-command
;;
;; Revision 1.5  2007/07/06 18:41:51  rubikitch
;; added available URL.
;;
;; Revision 1.4  2007/07/06 18:20:21  rubikitch
;; define-usage-memo: additional optional argument `name-converter-function'
;;
;; Use downcase filename for SLIME.
;; If you already use upcase filename, rename all the upcase filename to downcase.
;; `wdired-change-to-wdired-mode' is convenient to do this.
;;
;; Revision 1.3  2007/07/01 20:05:53  rubikitch
;; Support slime-describe-function.
;; Update docs.
;; First public release!
;;
;; Revision 1.2  2007/07/01 18:40:47  rubikitch
;; write docs.
;;
;; Revision 1.1  2007/07/01 10:07:15  rubikitch
;; Initial revision
;;

;;; Code:
(require 'cl)
(require 'font-lock)
(defvar umemo-base-directory "~/memo/umemo"
  "Directory where memo files are placed.
You need not create directory when it does not exist.
usage-memo creates it automatically.")
(defvar umemo-separator "\n===*===*===*===*===*===*===*===*===*===*===\n"
  "Separator between documentation and memo.")
(defvar umemo-category nil
  "Category of usage-memo. It is defined by `define-usage-memo'.")
(defvar umemo-entry-name nil
  "Entry name that help buffer(per buffer) is showing. It is used as filename of annotation.")
(defvar umemo-orig-major-mode nil
  "Major mode when `usage-memo-mode' turn on.")
(defvar umemo-current-entry-name nil
  "Entry name that help buffer(globally) is showing. See also `umemo-entry-name'.

It is needed because some help system (eg. SLIME) kills help
buffer and re-create it.  In this case, the buffer-local
`umemo-entry-name' is cleared: I have no choice but to use global
variable. But it is probaby rare case.")
(make-variable-buffer-local 'umemo-category)
(make-variable-buffer-local 'umemo-entry-name)
(make-variable-buffer-local 'umemo-orig-major-mode)

(defvar umemo-auto-change-major-mode-flag t
  "If non-nil, change major mode to `umemo-second-mode' when point is below the separator.")
(defvar umemo-second-mode 'org-mode
  "Major mode to write a usage memo.")
(defvar umemo-change-major-mode-flag nil)

;;;; low-level utils
(defun umemo-pathname (category entry-name)
  "Filename of annotation. `umemo-base-directory'/CATEGORY/ENTRY-NAME."
  (format "%s/%s/%s" umemo-base-directory category entry-name))

(defun umemo-fall-back-to-global-key-binding-in-memo-area (key)
  (let* ((lb (local-key-binding key))
         (gb (global-key-binding key)))
    (call-interactively
     (cond ((umemo-point-is-in-memo-area-p (point))  gb)
           (lb                                       lb)
           (t                                        gb)))))

(defun umemo-point-is-in-memo-area-p (point)
  (save-excursion
    (goto-char point)
    (search-backward umemo-separator nil t)))

(defmacro umemo-with-append-to-buffer (buffer &rest body)
  "Execute BODY at the end of BUFFER."
  `(save-excursion
     (set-buffer buffer)
     (goto-char (point-max))
     ,@body))

(defmacro umemo-if-buffer-has-separator (&rest body)
  `(save-excursion
     (goto-char (point-min))
     (when (search-forward umemo-separator nil t)
       ,@body)))

(defun umemo-has-entry-p ()
  (save-excursion
    (goto-char (point-min))
    (and (search-forward umemo-separator nil t)
         (/= (point) (point-max)))))

;;;; mid-level utils
(defun umemo-insert-memo (category entry-name buffer)
  (let ((path (umemo-pathname category entry-name)))
    (umemo-with-append-to-buffer buffer
     (setq buffer-read-only nil)
     (unless (umemo-has-entry-p)
       (insert umemo-separator)
       (and (file-exists-p path)  (insert-file-contents path))))))

(defun umemo-setup-variables (category entry-name buffer)
  (setq umemo-current-entry-name entry-name)
  (with-current-buffer buffer
    (setq umemo-category category
          umemo-entry-name entry-name
          umemo-orig-major-mode major-mode)))
(put 'umemo-category 'permanent-local t)
(put 'umemo-entry-name 'permanent-local t)
(put 'umemo-orig-major-mode 'permanent-local t)

;;;; automatical major-mode change
(defun umemo-change-mode (mode)
  (when (and (fboundp mode)
             (not (eq major-mode mode)))
    (funcall mode)
    (usage-memo-mode 1)
    (umemo-auto-change-major-mode-setup)
    (if (eq font-lock-mode t)
        (font-lock-fontify-buffer)))) 
(defun umemo-update-mode ()
  (when (and umemo-auto-change-major-mode-flag
             umemo-change-major-mode-flag)
    (if (save-excursion (search-forward umemo-separator nil t)) ; help [pt] separator
        (umemo-change-mode umemo-orig-major-mode)
      (umemo-change-mode umemo-second-mode))))
(add-hook 'post-command-hook 'umemo-update-mode)
(defun umemo-auto-change-major-mode-setup ()
  (set (make-local-variable 'umemo-change-major-mode-flag) t))

;;;; initializer

(defun umemo-setup (category entry-name buffer)
  (when (get-buffer buffer)
    (with-current-buffer buffer
      (umemo-setup-variables category entry-name buffer)
      (umemo-insert-memo category entry-name buffer)
      (usage-memo-mode 1))))

;;;; commands
(defun umemo-save ()
  "Save current usage memo(annotation) into file."
  (interactive)
  (umemo-if-buffer-has-separator
   (let* ((path (umemo-pathname umemo-category umemo-entry-name))
          (dir  (file-name-directory path)))
     (unless (file-directory-p dir) (make-directory dir t))
     (write-region (point) (point-max) path)
     (set-buffer-modified-p nil)
     (message "Wrote %s" path))))

(defun umemo-electric-return ()
  (interactive)
  (umemo-fall-back-to-global-key-binding-in-memo-area "\r"))

;;;; define API
(defvar buffer-fmt)
(defmacro define-usage-memo (command category nth-arg buffer-fmt &optional name-converter-function)
  "Add usage-memo feature to COMMAND.

Define `usage-memo' around-advice for COMMAND.
CATEGORY is usage-memo category to use, typically language name.
NTH-ARG is COMMAND's argument position (0-origin) representing entry name,
NTH-ARG is passed to `ad-get-arg' macro.
BUFFER-FMT is a `format' string, which %s is replaced with entry name,
representing document display buffer.
Optional NAME-CONVERTER-FUNCTION is 1-argument function
to convert COMMAND's argument (indicated by NTH-ARG) to entry name. 
Of course, the default is `identity'.

NAME-CONVERTER-FUNCTION can be used when the entry name is determined by document-displaying buffer contents.
BUFFER-FMT is bound when NAME-CONVERTER-FUNCTION is called.

For example: To support `describe-function' (already supported):

  (define-usage-memo describe-function \"elisp\" 0 \"*Help*\")

Because `define-usage-memo' is a macro, COMMAND is not quoted.
The CATEGORY is \"elisp\".  The NTH-ARG is 0 because
`describe-function' takes a symbol to lookup at the first
argument. Because NTH-ARG is 0-origin, 0 means the first
argument.  BUFFER-FMT is same as *Help* buffer: it is only
coincidental.


Another example: Support RI lookup (Ruby's document-lookup tool)

`ri' function is defined in ri-ruby.el. The usage is:

  (ri ENTRY_NAME)

Then `ri' creates

  ri `ENTRY_NAME'

buffer. Instead of reusing a buffer, it creates a buffer per query.
That is why BUFFER-FMT uses `format' string!

  (define-usage-memo ri \"ruby\" 0 \"ri `%s'\")


SLIME example: Downcase Lisp's symbol

  (define-usage-memo slime-show-description \"cl\" 0 \"*SLIME Description*\" umemo-make-entry-name:slime)

See also `umemo-initialize' definition.

"
  (let ((converter (or name-converter-function 'identity)))
    `(defadvice ,command (around usage-memo activate)
       ad-do-it
       (let* ((buffer-fmt ,buffer-fmt)
              (entry-name (funcall ',converter (format "%s" (ad-get-arg ,nth-arg))))
              (buf (with-no-warnings (format ,buffer-fmt entry-name))))
         (umemo-setup ,category entry-name buf)))))

;; (umemo-initialize)
;; (ri "Array#length")
;; (lh-refe "Array#length")
;; (describe-function 'princ)
;; (slime-describe-symbol "princ")
;; (slime-describe-symbol "cl:princ")
;; (slime-describe-symbol "cl:princ")
;; (slime-describe-symbol "CL-UTILITIES:WITH-GENSYMS")
;; (slime-describe-symbol "KMRCL:WITH-GENSYMS")
;; (with-current-buffer "*SLIME Description*" umemo-entry-name)

;;;; sample definition
(defun umemo-initialize ()
  "A bunch of `define-usage-memo' definitions. Feel free to redefine!"
  (define-usage-memo ri "ruby" 0 "ri `%s'")
  (define-usage-memo lh-refe "ruby" 0 "refe \"%s\"")
  ;; slime-describe-symbol, slime-describe-function, and slime-documentation
  ;; calls slime-show-description.
  (define-usage-memo slime-show-description "cl" 0 "*SLIME Description*" umemo-make-entry-name:slime)
  (define-usage-memo describe-function "elisp" 0 "*Help*")
  (define-usage-memo describe-variable "elisp" 0 "*Help*")
  (define-usage-memo describe-mode "elisp" 0 "*Help*"
    (lambda (arg) major-mode)))

;; (ad-delete 'slime-describe-symbol 'around 'usage-memo)

;;;; minor mode definition
(defvar usage-memo-mode-map (make-sparse-keymap))
(define-key usage-memo-mode-map "\C-x\C-s" 'umemo-save)
(define-key usage-memo-mode-map "\r" 'umemo-electric-return)
;; (loop for c from ?  to ?~ do
;;      (define-key usage-memo-mode-map (char-to-string c) 'self-insert-command))
      
(define-minor-mode usage-memo-mode
  "Automatically enabled minor mode to add usage-memo feature by `define-usage-memo'.

Write your annotation below `===*===*===*===*===*===*===*===*===*===*===' line.
\\<usage-memo-mode-map>\\[umemo-save]: Save your annotation to file indicated by `umemo-pathname'.

Of course, your annotation is revived even if Emacs is restarted!
"
  nil "<UMemo>" usage-memo-mode-map
  :global nil
  (setq buffer-read-only nil)
  (when view-mode (view-mode -1))
  (setq header-line-format (format "[%s/%s]" umemo-category umemo-entry-name))
  (umemo-auto-change-major-mode-setup)
  (buffer-enable-undo))

;;;; SLIME hack
(defun umemo-make-entry-name:slime (symbol-name)
  ;; SYMBOL-NAME is ignored.
  ;; But it is needed to follow `define-usage-memo' API.
  ;; BUFFER-FMT is constant.
  (downcase (umemo-slime-get-full-symbol-name buffer-fmt)))

(defun umemo-slime-get-full-symbol-name (buf)
  "Get common lisp full symbol name describing in BUF. Currently it supports SBCL, CMUCL and CLISP.

If you want to adjust to other CL implementations, redefine this function."
  (flet ((srch (re num) (and (re-search-forward re nil t) (match-string num)))
         (srcheq (re num str) (equal (srch re num) str)))
    (save-excursion
      (set-buffer buf)
      (goto-char (point-min))
      (apply
       #'concat
       (reverse
        (list (srch "^\\(.+:+\\)?\\([^ ]+\\)" 2)
              (if (or (srcheq "an \\(internal\\|external\\) symbol" 1 "internal")
                      (save-excursion (srcheq "SYMBOL-PLIST[^:]+\\(:+\\)" 1 "::")))
                  "::" ":")
              (or (srch "#<PACKAGE \"\\(.+\\)\">" 1)
                  (srch "in the \\(.+\\) package" 1)
                  (srch "#<PACKAGE \\(.+\\)>" 1))))))))

;;;; Bug report
(defvar umemo-maintainer-mail-address
  (concat "rubiki" "tch@ru" "by-lang.org"))
(defvar umemo-bug-report-salutation
  "Describe bug below, using a precise recipe.

When I executed M-x ...

How to send a bug report:
  1) Be sure to use the LATEST version of usage-memo.el.
  2) Enable debugger. M-x toggle-debug-on-error or (setq debug-on-error t)
  3) Use Lisp version instead of compiled one: (load \"usage-memo.el\")
  4) If you got an error, please paste *Backtrace* buffer.
  5) Type C-c C-c to send.
# If you are a Japanese, please write in Japanese:-)")
(defun umemo-send-bug-report ()
  (interactive)
  (reporter-submit-bug-report
   umemo-maintainer-mail-address
   "usage-memo.el"
   (apropos-internal "^u\\(sage-\\)?memo-" 'boundp)
   nil nil
   umemo-bug-report-salutation))

(provide 'usage-memo)

;; How to save (DO NOT REMOVE!!)
;; (emacswiki-post "usage-memo.el")
;;; usage-memo.el ends here
