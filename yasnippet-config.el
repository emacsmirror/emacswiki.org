;;;; yasnippet-config.el --- Configuration of yasnippet.el
;; $Id: yasnippet-config.el,v 1.28 2012/07/27 06:33:30 rubikitch Exp $

;; Copyright (C) 2009, 2010, 2012  rubikitch

;; Author: rubikitch <rubikitch@ruby-lang.org>
;; Keywords: abbrev, languages, lisp, convenience
;; URL: http://www.emacswiki.org/cgi-bin/wiki/download/yasnippet-config.el

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
;; Configuration of yasnippet package. This file includes some hacks.

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `yas/setup'
;;    Obsolete. DO NOT USE.
;;  `yas/oneshot-snippet'
;;    Register/expand oneshot snippet.
;;  `yas/new-snippet-with-content'
;;    Create snippet from region to speed-up snippet development.
;;  `yas/make-placeholder'
;;    Make yasnippet placeholder from region.
;;  `yas/expand-sync'
;;    Execute `yas/expand'. This function exits after expanding snippet.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;
;;  `yas/init-snippet-template'
;;    *Initial snippet for `yas/new-snippet-with-content'.
;;    default = "# -*- mode: snippet -*-\n# name: $1\n# key: ${2:${1:$(replace-regexp-in-string \"\\\\\\\\(\\\\\\\\w+\\\\\\\\).*\" \"\\\\\\\\1\" yas/text)}}\n# --\n"

;;; Installation:

;;
;; Extension and configuration of yasnippet.
;;
;; (require 'yasnippet-config)
;;
;;
;; If you use yasnippet from `auto-complete', add
;; (yas/set-ac-modes)
;; (yas/enable-emacs-lisp-paren-hack)
;; before `auto-complete' settings.
;; 


;;; History:

;; $Log: yasnippet-config.el,v $
;; Revision 1.28  2012/07/27 06:33:30  rubikitch
;; yas/new-snippet-with-content: fix escape problem
;;
;; Revision 1.27  2012/07/27 02:41:32  rubikitch
;; `yas/delete-current-link-maybe': fix regexp
;;
;; Revision 1.26  2012/07/27 02:27:29  rubikitch
;; yas/expand-link, yas/expand-snippet-link: Do not delete link if it is in comment line.
;;
;; Revision 1.25  2012/07/26 14:44:06  rubikitch
;; Remove advice for yas/delete-all: because of undefined function
;;
;; Revision 1.24  2012/07/25 17:01:28  rubikitch
;; New function: `yas/define-auto-insert'
;;
;; Revision 1.23  2012/07/25 03:22:04  rubikitch
;; New function: `yas/delete-keys'
;;
;; Revision 1.22  2012/07/25 03:07:01  rubikitch
;; New function: `yas/expand-link', `yas/expand-snippet-link', `yas/expand-link-choice'
;;
;; Revision 1.21  2012/07/24 04:46:07  rubikitch
;; Silence warning about `paredit-raise-sexp'
;;
;; Revision 1.20  2012/07/24 04:28:02  rubikitch
;; New command: yas/expand-sync / New function: yas/expand-snippet-sync
;;
;; Revision 1.19  2012/07/22 10:22:07  rubikitch
;; paredit-mode boundp check
;;
;; Revision 1.18  2012/07/22 09:38:39  rubikitch
;; new hack: `yas/enable-emacs-lisp-paren-hack'
;;
;; Revision 1.17  2012/07/22 09:36:21  rubikitch
;; `auto-mode-alist' for `snippet-mode'
;;
;; Revision 1.16  2012/07/22 03:33:24  rubikitch
;; New command: yas/make-placeholder  (C-c C-n in snippet-mode)
;;
;; Revision 1.15  2012/07/22 03:30:54  rubikitch
;; Update copyright
;;
;; Revision 1.14  2012/07/22 03:30:10  rubikitch
;; New command: yas/new-snippet-with-content
;; Add some document.
;;
;; Revision 1.13  2012/07/22 03:15:36  rubikitch
;; Add auto-complete settting
;;
;; Revision 1.12  2012/07/22 03:08:34  rubikitch
;; remove buffer local condition workaround
;;
;; Revision 1.11  2012/07/20 09:01:40  rubikitch
;; skk hack: use yas/minor-mode only
;;
;; Revision 1.10  2012/07/20 09:01:00  rubikitch
;; `yas/setup' makes error now.
;;
;; Revision 1.9  2012/07/20 08:43:43  rubikitch
;; remove view-mode hack
;;
;; Revision 1.8  2012/07/18 17:19:12  rubikitch
;; cleanup: Remove `yas/setup' and backward compatibility code / Rewrite Installation
;;
;; Revision 1.7  2012/07/14 14:11:29  rubikitch
;; bugfix about yas/snippet-table-parent
;;
;; Revision 1.6  2010/04/09 04:56:00  rubikitch
;; New command: `yas/oneshot-snippet'
;;
;; Revision 1.5  2009/09/16 09:40:08  rubikitch
;; Adjust to yasnippet 0.6.1
;;
;; Revision 1.4  2009/09/15 02:06:08  rubikitch
;; * backward compatibility for anything-c-yasnippet.el and auto-complete-yasnippet.el
;; * sample configuration of dropdown-list
;; * oneshot snippet
;;
;; Revision 1.3  2009/07/21 17:14:26  rubikitch
;; `yas/snippet-file-p': ignore substring error
;;
;; Revision 1.2  2009/07/20 20:15:29  rubikitch
;; typo
;;
;; Revision 1.1  2009/07/20 19:47:27  rubikitch
;; Initial revision
;;

;;; Code:

(defvar yasnippet-config-version "$Id: yasnippet-config.el,v 1.28 2012/07/27 06:33:30 rubikitch Exp $")
(eval-when-compile (require 'cl))

(require 'yasnippet) ;; not yasnippet-bundle

(defun yas/setup (&rest ignore)
  "Obsolete. DO NOT USE."
  (interactive)
  (error "Do not use `yas/setup'. `yas/global-mode' is enough now."))

;;;; auto-mode-alist
(add-to-list 'auto-mode-alist '("emacs.+/snippets/" . snippet-mode))
(add-to-list 'auto-mode-alist '("emacs.+/snippets/.+\\.el$" . emacs-lisp-mode))


;;;; With `skk-mode'
(defadvice skk-j-mode-on (after yasnippet activate)
  (yas/minor-mode -1))
(defadvice skk-mode-exit (after yasnippet activate)
  (yas/minor-mode 1))
(defadvice skk-latin-mode-on (after yasnippet activate)
  (yas/minor-mode 1))
(defun yas/disable-when-skk-is-enabled ()
  (when (and (boundp 'skk-mode) skk-mode)
    (yas/minor-mode -1)))
(add-hook 'after-change-major-mode-hook 'yas/disable-when-skk-is-enabled t)

;;;; Disable flymake during expansion
(defvar flymake-is-active-flag nil)

(defadvice yas/expand-snippet
  (before inhibit-flymake-syntax-checking-while-expanding-snippet activate)
  (setq flymake-is-active-flag
        (or flymake-is-active-flag
            (assoc-default 'flymake-mode (buffer-local-variables))))
  (when flymake-is-active-flag
    (flymake-mode-off)))

(add-hook 'yas/after-exit-snippet-hook
          '(lambda ()
             (when flymake-is-active-flag
               (flymake-mode-on)
               (setq flymake-is-active-flag nil))))

;;;; oneshot snippet
(defvar yas/oneshot-snippet nil)
(defun yas/register-oneshot-snippet (s e)
  (interactive "r")
  (setq yas/oneshot-snippet (buffer-substring-no-properties s e))
  (delete-region s e)
  (yas/expand-oneshot-snippet)
  (message "%s" (substitute-command-keys "Press \\[yas/expand-oneshot-snippet] to expand.")))

(defun yas/expand-oneshot-snippet ()
  (interactive)
  (if (string< "0.6" yas/version)
      (yas/expand-snippet yas/oneshot-snippet)
    (yas/expand-snippet (point) (point) yas/oneshot-snippet)))

(defun yas/oneshot-snippet ()
  "Register/expand oneshot snippet.

If `transient-mark-mode' is enabled and region is selected,
register the region as oneshot snippet, Otherwise expand it."
  (interactive)
  (if (region-active-p)
      (yas/register-oneshot-snippet (region-beginning) (region-end))
    (yas/expand-oneshot-snippet)))

;;;; auto-complete
(defun yas/set-ac-modes ()
  "Add modes in `yas/snippet-dirs' to `ac-modes'.

Call (yas/set-ac-modes) BEFORE (global-auto-complete-mode 1) or (ac-config-default)."
  (eval-after-load "auto-complete"
    '(setq ac-modes
          (append
           (apply 'append (mapcar (lambda (dir) (mapcar 'intern (directory-files dir nil "-mode$")))
                                  (yas/snippet-dirs)))
           ac-modes))))

(provide 'yasnippet-config)

;;;; yas/new-snippet-with-content
(defcustom yas/init-snippet-template "\
# -*- mode: snippet -*-
# name: $1
# key: ${2:${1:$(replace-regexp-in-string \"\\\\\\\\(\\\\\\\\w+\\\\\\\\).*\" \"\\\\\\\\1\" yas/text)}}
# --
"
  "*Initial snippet for `yas/new-snippet-with-content'."
  :type 'string  
  :group 'yasnippet)

(defun yas/new-snippet-with-content (s e)
  "Create snippet from region to speed-up snippet development."
  (interactive "r")
  (let ((initial-text (buffer-substring s e))
        (default-directory (file-name-as-directory (car (yas/snippet-dirs)))))
    (yas/new-snippet t)
    (save-excursion
      (when initial-text
        (insert initial-text)
        (goto-char (point-min))
        (while (re-search-forward "[\\$]" nil t)
          (replace-match "\\\\\\&"))))
    (yas/expand-snippet yas/init-snippet-template)))

;;;; make placeholder
(defun yas/make-placeholder (s e)
  "Make yasnippet placeholder from region."
  (interactive "r")
  (let ((text (buffer-substring s e)))
    (yas/expand-snippet "\\${$1:`text`}" s e)))
(define-key snippet-mode-map "\C-c\C-n" 'yas/make-placeholder)


;;;; parentheses hack in `emacs-lisp-mode'
(declare-function paredit-raise-sexp "ext:paredit")
(declare-function yas/reload-all "yasnippet")
(defun yas/before-expand-snippet-hook--emacs-lisp-remove-parenthsis (content)
  (when (and (boundp 'paredit-mode) paredit-mode
             (memq major-mode '(emacs-lisp-mode lisp-interaction-mode ielm-mode))
             (ignore-errors (fboundp (car (read content)))))
    (let ((m (point)) m2)
      (forward-sexp -1)
      (setq m2 (point))
      (when (eq (char-before) ?\()
        (paredit-raise-sexp)
        ;; Remove original abbrev 1- by `paredit-raise-sexp'
        (delete-region (1- m2) (1- m))))))
(defadvice yas/expand-snippet (before emacs-lisp-remove-parenthesis (content &optional start end expand-env))
  "Remove parentheses when yasnippet abbrev is expanded in function position."
  (setq start (and start (move-marker (make-marker) start)))
  (setq end (and start (move-marker (make-marker) end)))
  (and start end (yas/before-expand-snippet-hook--emacs-lisp-remove-parenthsis content)))

(defun yas/enable-emacs-lisp-paren-hack ()
  "Enable advice in `yas/expand-snippet' emacs-lisp-remove-parenthesis."
  (ad-enable-advice 'yas/expand-snippet 'before 'emacs-lisp-remove-parenthesis)
  (ad-update 'yas/expand-snippet))

;;;; Expand snippet synchronously
(defvar yas/recursive-edit-flag nil)
(defun yas/expand-sync ()
  "Execute `yas/expand'. This function exits after expanding snippet."
  (interactive)
  (let ((yas/recursive-edit-flag t))
    (call-interactively 'yas/expand)
    (recursive-edit)))
(defun yas/expand-snippet-sync (content &optional start end expand-env)
  "Execute `yas/expand-snippet'. This function exits after expanding snippet."
  (let ((yas/recursive-edit-flag t))
    (yas/expand-snippet content start end expand-env)
    (recursive-edit)))
(defun yas/after-exit-snippet-hook--recursive-edit ()
  (when yas/recursive-edit-flag
    (throw 'exit nil)))
(add-hook 'yas/after-exit-snippet-hook 'yas/after-exit-snippet-hook--recursive-edit)

;;;; Snippet hyperlink
(defun yas/delete-current-link-maybe ()
  (ignore-errors
    (if (save-excursion
            (beginning-of-line)
            (looking-at (concat "^[ \t]*" (if comment-start (regexp-quote comment-start) "#"))))
        (beginning-of-line)
      (delete-region (point) (progn (forward-sexp -1) (point))))))

(defun yas/expand-link (key)
  "Hyperlink function for yasnippet expansion."
  (yas/delete-current-link-maybe)
  (insert key)
  (yas/expand))

(defun yas/expand-snippet-link (content)
  (yas/delete-current-link-maybe)
  (yas/expand-snippet content))
;; (yas/expand-snippet-link "$1 = 2\n")

(defun yas/expand-link-choice (&rest keys)
  "Hyperlink to select yasnippet template."
  (yas/expand-link (yas/choose-value keys)))
;; (yas/expand-link-choice "defgp" "defcm")

;;;; Disable some standard snippets
(defun yas/delete-keys (mode &rest keys)
  "Delete snippets expanded by KEYS in MODE."
  (let ((hash (yas/table-hash (yas/table-get-create mode))))
    (dolist (k keys) (remhash k hash))))

;;;; auto-insert with yasnippet
(defun yas/define-auto-insert (condition snippet-key &optional after)
  "Set `auto-insert-alist' to expand SNIPPET-KEY at file creation.

CONDITION may be a regexp that must match the new file's name, or it may be
a symbol that must match the major mode for this element to apply.

Associate CONDITION with SNIPPET-KEY in `auto-insert-alist'.
Optional AFTER means to insert snippet after all existing snippets for CONDITION."
  (add-to-list 'auto-insert-alist `(,condition . (lambda () (yas/expand-link ,snippet-key))) after))


;; How to save (DO NOT REMOVE!!)
;; (emacswiki-post "yasnippet-config.el")
;;; yasnippet-config.el ends here
