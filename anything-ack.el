;;; anything-ack.el --- search refinement of ack result with anything
;; $Id: anything-ack.el 113 2010-12-20 04:07:46Z ikk $

;; Copyright (C) 2010  APP Design, Inc.

;; Author: Ivan K, x12 at users.sourceforge.net
;; Maintainer: Ivan K, x12 at users.sourceforge.net
;; Version: 1.0
;; Keywords: convenience, unix
;; URL: http://www.emacswiki.org/cgi-bin/wiki/download/anything-ack.el

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

;; Do ack in anything buffer. When we search information with ack,
;; we often narrow the candidates. Let's use `anything' to do it.
;; 
;; This file is a 'creative' merge of anything-grep.el by
;; rubikitch <rubikitch@ruby-lang.org>, http://www.emacswiki.org/cgi-bin/wiki/download/anything-grep.el
;; and
;; ack.el by Philip Jackson <phil@shellarchive.co.uk>, https://github.com/markhepburn/dotemacs/blob/master/ack.el

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `anything-ack'
;;    Run ack in `anything' buffer to narrow results.

;;; Code:

(defvar anything-ack-version "$Id: anything-ack.el 113 2010-12-20 04:07:46Z ikk $")
(require 'anything)
(require 'grep)

;;; Customizable variables
(defgroup anything-ack nil
  "Ack anything mode."
  :group 'anything)

(defcustom anything-ack-type-map
  '(
    ((LaTeX-mode) . "tex")
    ((TeX-mode) . "tex")
    ((asm-mode) . "asm")
    ((batch-file-mode) . "batch")
    ((c++-mode) . "cpp")
    ((c++-mode) . "cpp")
    ((c-mode) . "cc")
    ((cfmx-mode) . "cfmx")
    ((cperl-mode) . "perl")
    ((csharp-mode) . "csharp")
    ((css-mode) . "css")
    ((emacs-lisp-mode) . "elisp")
    ((erlang-mode) . "erlang")
    ((espresso-mode) . "js")
    ((fortran-mode) . "fortran")
    ((haskell-mode) . "haskell")
    ((hexl-mode) . "binary")
    ((html-mode) . "html")
    ((java-mode) . "java")
    ((javascript-mode) . "js")
    ((jde-mode) . "java")
    ((js2-mode) . "js")
    ((jsp-mode) . "jsp")
    ((latex-mode) . "tex")
    ((lisp-mode) . "lisp")
    ((lua-mode) . "lua")
    ((makefile-mode) . "make")
    ((mason-mode) . "mason")
    ((nxml-mode) . "xml")
    ((objc-mode) . "objc")
    ((ocaml-mode) . "ocaml")
    ((parrot-mode) . "parrot")
    ((perl-mode cperl-mode) . "perl")
    ((php-mode) . "php")
    ((plone-mode) . "plone")
    ((python-mode) . "python")
    ((ruby-mode) . "ruby")
    ((scheme-mode) . "scheme")
    ((shell-script-mode) . "shell")
    ((skipped-mode) . "skipped")
    ((smalltalk-mode) . "smalltalk")
    ((sql-mode) . "sql")
    ((tcl-mode) . "tcl")
    ((tex-mode) . "tex")
    ((text-mode) . "text")
    ((tt-mode) . "tt")
    ((vb-mode) . "vb")
    ((vim-mode) . "vim")
    ((xml-mode nxml-mode) . "xml")
    ((yaml-mode) . "yaml")
  )
  "alist describing how to fill in the '--type=' argument to ack"
  :group 'anything-ack
  :type '(repeat sexp))

(defcustom anything-ack-guess-type t
  "Setting this value to `t' will have `ack' do its best to fill
in the --type argument to the ack command"
  :group 'anything-ack
  :type 'boolean)

(defcustom anything-ack-command "c:/tools/trunk/ActivePerl-811/bin/perl.exe c:/home/bin/ack --nocolor --nogroup "
  "The command to be run by the ack function."
  :group 'anything-ack
  :type 'string)

(defvar anything-ack-save-buffers-before-grep nil
  "Do `save-some-buffers' before performing `anything-ack'.")

(defvar anything-ack-goto-hook nil
  "List of functions to be called after `anything-ack-goto' opens file.")

(defvar anything-ack-find-file-function 'find-file
  "Function to visit a file with.
It takes one argument, a file name to visit.")

(defvar anything-ack-multiline t
  "If non-nil, use multi-line display. It is prettier.
Use anything.el v1.147 or newer.")

(defvar anything-ack-fontify-file-name t
  "If non-nil, fontify file name and line number of matches.")

(defvar anything-ack-filter-command nil
  "If non-nil, filter the result of ack command.
For example, normalizing many Japanese encodings to EUC-JP,
set this variable to \"ruby -rkconv -pe '$_.replace $_.toeuc'\".
The command is converting standard input to EUC-JP line by line. ")


;; (@* "core")

(defun anything-ack-find-type-for-mode ()
  (catch 'found
    (dolist (mode-type anything-ack-type-map)
      (when (member major-mode (car mode-type))
        (throw 'found (cdr mode-type))))))

(defun anything-ack-build-command ()
  (let ((type (anything-ack-find-type-for-mode)))
    (concat anything-ack-command
            (when (and anything-ack-guess-type type)
              (concat " --type=" type)) " ")))

(defvar anything-ack-sources nil
  "`anything-sources' for last invoked `anything-ack'.")

(defvar anything-ack-buffer-name nil)

(defun anything-ack-base (sources &optional bufname)
  "Invoke `anything' for `anything-ack'."
  (setq anything-ack-sources sources)
  (setq anything-ack-buffer-name (or bufname "*anything ack*"))
  (let ((anything-quit-if-no-candidate t)
        (anything-compile-source-functions
         (cons 'anything-compile-source--anything-ack-init anything-compile-source-functions)))
    (anything sources nil nil nil nil bufname)))

(defun anything-ack-source (command pwd)
  "Anything Source of `anything-ack'."
  `((command . ,command)
    (pwd . ,pwd)
    (name . ,(format "%s [%s]" command pwd))
    (action . anything-ack-goto)
    (anything-ack)
    (candidate-number-limit . 9999)
    (migemo)
    ;; to inherit faces
    (candidates-in-buffer)
    (get-line . buffer-substring)
    ,@(when anything-ack-multiline
        '((multiline)
          (real-to-display . anything-ack-real-to-display)))))

(defun anything-compile-source--anything-ack-init (source)
  (if (assq 'anything-ack source)
      (append '((init . anything-ack-init)
                (candidates)) source)
    source))

(defun anything-ack-init ()
  (anything-ack-create-buffer (anything-attr 'command)  (anything-attr 'pwd)))

(defun anything-ack-real-to-display (file-line-content)
  (if (string-match ":\\([0-9]+\\):" file-line-content)
      (format "%s:%s\n %s"
              (substring file-line-content 0 (match-beginning 0))
              (match-string 1 file-line-content)
              (substring file-line-content (match-end 0)))
    file-line-content))

(defvar anything-ack-source-local nil)
(defvar anything-ack-waiting-source nil
  "`anything' sources to get together in `anything-ack-sentinel'.")
(defun anything-ack-do-ack (command pwd)
  "Insert result of COMMAND. The current directory is PWD."
  (let ((process-environment process-environment))
    (set (make-local-variable 'anything-ack-source-local) (anything-get-current-source))
    (add-to-list 'anything-ack-waiting-source anything-ack-source-local)
    (set-process-sentinel
     (let ((default-directory pwd))
       (start-process-shell-command "anything-ack" (current-buffer) command))
     'anything-ack-sentinel)))

(defvar anything-ack-do-after-minibuffer-exit nil)
(defun anything-ack-minibuffer-exit-hook ()
  (when anything-ack-do-after-minibuffer-exit
    (run-at-time 1 nil anything-ack-do-after-minibuffer-exit)
    (setq anything-ack-do-after-minibuffer-exit nil)))
(add-hook 'minibuffer-exit-hook 'anything-ack-minibuffer-exit-hook)

(defun anything-ack-show (func)
  (if (active-minibuffer-window)
      (setq anything-ack-do-after-minibuffer-exit func)
    (funcall func)))

(defun anything-ack-sentinel (proc stat)
  (with-current-buffer (process-buffer proc)
    (setq anything-ack-waiting-source (delete anything-ack-source-local anything-ack-waiting-source))
    (anything-ack-fontify))
  (unless anything-ack-waiting-source
    ;; call anything
    (anything-ack-show
     (lambda ()
       (let ((anything-quit-if-no-candidate (lambda () (message "No matches"))))
         (anything anything-ack-sources nil nil nil nil anything-ack-buffer-name))))))

(defun anything-ack-fontify ()
  "Fontify the result of `anything-ack-do-ack'."
  ;; Color matches.
  (goto-char 1)
  (while (re-search-forward "\\(\033\\[01;31m\\)\\(.*?\\)\\(\033\\[[0-9]*m\\)" nil t)
    (put-text-property (match-beginning 2) (match-end 2) 'face  grep-match-face)
    (replace-match "" t t nil 1)
    (replace-match "" t t nil 3))
  ;; Delete other escape sequences.
  (goto-char 1)
  (while (re-search-forward "\\(\033\\[[0-9;]*[mK]\\)" nil t)
    (replace-match "" t t nil 0))
  (when anything-ack-fontify-file-name
    (goto-char 1)
    (while (re-search-forward ":\\([0-9]+\\):" nil t)
      (put-text-property (point-at-bol) (match-beginning 0) 'face compilation-info-face)
      (put-text-property (match-beginning 1) (match-end 1) 'face compilation-line-face)
      (forward-line 1))))

(defun anything-ack-create-buffer (command pwd)
  "Create candidate buffer for `anything-ack'.
Its contents is fontified ack result."
  (with-current-buffer (anything-candidate-buffer 'global)
    (setq default-directory pwd)
    (anything-ack-do-ack command pwd)
    (current-buffer)))

(defun anything-ack-goto  (file-line-content)
  "Visit the source for the ack result at point."
  (string-match ":\\([0-9]+\\):" file-line-content)
  (save-match-data
    (funcall anything-ack-find-file-function
             (expand-file-name (substring file-line-content
                                          0 (match-beginning 0))
                               (anything-attr 'pwd))))
  (goto-line (string-to-number (match-string 1 file-line-content)))
  (run-hooks 'anything-ack-goto-hook))

;;;###autoload
(defun anything-ack (command pwd)
  "Run ack in `anything' buffer to narrow results.
It asks COMMAND for ack command line and PWD for current directory."
  (interactive
   (progn
     (let ((default (anything-ack-build-command)))
       (list (read-from-minibuffer "Run ack like this: "
				   (if current-prefix-arg
				       default (anything-ack-build-command))
				   nil nil 'grep-history
				   (if current-prefix-arg nil default))
             (read-directory-name "Directory: " default-directory default-directory t)))))
  (anything-ack-base (list (anything-ack-source (anything-ack-preprocess-command command) pwd))
                      (format "*anything ack:%s [%s]*" command (abbreviate-file-name pwd))))

(defun anything-ack-preprocess-command (command)
  (with-temp-buffer
    (insert command)
    ;;(message command)
    (goto-char 1)
    (when (search-forward "$buffers" nil t)
      (delete-region (match-beginning 0) (match-end 0))
      (insert (mapconcat 'shell-quote-argument
                         (delq nil (mapcar 'buffer-file-name (buffer-list))) " ")))
    (when anything-ack-filter-command
      (goto-char (point-max))
      (insert "|" anything-ack-filter-command))
    (buffer-string)))

(provide 'anything-ack)

;; How to save (DO NOT REMOVE!!)
;; (progn (magit-push) (emacswiki-post "anything-ack.el"))
;;; anything-ack.el ends here
