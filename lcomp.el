;;; lcomp.el --- list completion hacks!

;; Copyright (C) 2002, 2004, 2010 by Taiki SUGAWARA

;; Author: Taiki SUGAWARA <buzz.taiki@gmail.com>
;; Keywords: tools, convenience
;; Version: 0.03
;; Time-stamp: <2010-02-08 17:16:51 UTC taiki>
;; URL: http://www.emacswiki.org/cgi-bin/wiki/lcomp.el
;; URL: http://bitbucket.org/buzztaiki/elisp/src/tip/lcomp.el

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides following features:
;;
;;  - `lcomp-mode': make the completions buffer window disappear after
;;	use
;;
;;   - `lcomp-keys-mode': add keybindings to the completions buffer
;;
;; You want to use this package. put these lines into your ~/.emacs.
;;
;;     (require 'lcomp)
;;     (lcomp-mode 1)
;;     (lcomp-keys-mode 1)
;;
;; The completions buffer is usually only dismissed after completion
;; when it is created from minibuffer completion, but `lcomp-mode'
;; makes it get dismissed correctly from any buffer (e.g. shell, or by
;; calling `comint-dynamic-complete-filename').

;;; Key Bindings:

;; `lcomp-mode' adds global keybindings if enabled:
;;    "M-v"	  -> lcomp-select-completion-window-or-original
;;     
;; `lcomp-keys-mode' adds keybindings to the completions buffer if enabled:
;;    "C-i"	  -> next-completion
;;    "M-C-i"	  -> previous-completion
;;    "f"	  -> next-completion
;;    "b"	  -> previous-completion
;;    "n"	  -> next-line
;;    "p"	  -> previous-line
;;    " "	  -> scroll-up
;;    [del]	  -> scroll-down
;;    [backspace] -> scroll-down
;;    "q"	  -> delete-completion-window

;;; Code:
(require 'easy-mmode)

;; lcomp
(defvar lcomp-before-completion-winconf nil
  "This variable hold before completion window configulation.")
(defvar lcomp-completion-halfway-p nil
  "If non-nil, completion is halfway now.")
(defvar lcomp-display-completion-buffer-p nil
  "If non-nil completion buffer is displayed.")
(defvar lcomp-completion-buffer nil
  "This variable hold completion buffer.")

(defvar lcomp-mode-map
  (let ((map (or (and (boundp 'lcomp-mode-map)
		      (keymapp (symbol-value 'lcomp-mode-map))
		      (symbol-value 'lcomp-mode-map))
		 (make-sparse-keymap))))
    (define-key map "\M-v" 'lcomp-select-completion-window-or-original)
    map))


(defadvice try-completion (after lcomp-ad disable)
  (setq lcomp-completion-halfway-p (stringp ad-return-value)))

(defadvice choose-completion (after lcomp-ad disable)
  (when lcomp-before-completion-winconf
    (lcomp-resume-before-completion-winconf-1)))

(defadvice delete-completion-window (around lcomp-ad disable)
  (if lcomp-before-completion-winconf
      (let ((buf completion-reference-buffer))
	(when (buffer-live-p buf)
	  (switch-to-buffer buf))
	(lcomp-resume-before-completion-winconf))
    ad-do-it))

(defun lcomp-setup-completion ()
  (when (and (not lcomp-before-completion-winconf)
	     (not (window-minibuffer-p)))
    (setq lcomp-display-completion-buffer-p t)
    (setq lcomp-completion-buffer standard-output)
    (setq lcomp-before-completion-winconf (current-window-configuration))))

(defun lcomp-resume-before-completion-winconf-1 ()
  (condition-case err
      (set-window-configuration lcomp-before-completion-winconf)
    (error 
     (message "%s occured. bat ignore." (error-message-string err))))
  (setq lcomp-before-completion-winconf nil)
  (setq lcomp-completion-buffer nil))

(defun lcomp-resume-before-completion-winconf ()
  (when (and lcomp-before-completion-winconf
	     (not (or (and (eq this-command 'self-insert-command)
			   (string-match "\\(\\sw\\|\\s_\\)"
					 (this-command-keys)))
		      (eq (current-buffer) lcomp-completion-buffer)
		      (window-minibuffer-p)
		      lcomp-display-completion-buffer-p
		      lcomp-completion-halfway-p)))
    (let ((buf (current-buffer)))
      (lcomp-resume-before-completion-winconf-1)
      (when (and (not (eq buf (current-buffer)))
		 (buffer-live-p buf))
	(switch-to-buffer buf))))
  (setq lcomp-display-completion-buffer-p nil)
  (setq lcomp-completion-halfway-p nil))

(defun lcomp-select-completion-window ()
  (interactive)
  (when (and lcomp-completion-buffer
	     (get-buffer-window lcomp-completion-buffer))
    (select-window (get-buffer-window lcomp-completion-buffer))))

(defun lcomp-select-completion-window-or-original ()
  (interactive)
  (or (lcomp-select-completion-window)
      (let ((minor-mode-overriding-map-alist
	     '((lcomp-mode . nil))))
	(call-interactively (or (key-binding (this-command-keys-vector))
				'ignore)))))

(defun lcomp--install ()
  (add-hook 'completion-setup-hook 'lcomp-setup-completion)
  (add-hook 'post-command-hook 'lcomp-resume-before-completion-winconf)

  (ad-enable-regexp "^lcomp-ad$")
  (ad-activate-regexp "^lcomp-ad$" t))


(defun lcomp--uninstall ()
  (remove-hook 'completion-setup-hook 'lcomp-setup-completion)
  (remove-hook 'post-command-hook 'lcomp-resume-before-completion-winconf)

  (ad-disable-regexp "^lcomp-ad$")
  (ad-activate-regexp "^lcomp-ad$" t))

(define-minor-mode lcomp-mode
  "Auto close completion window mode."
  :group 'lcomp
  :global t
  (if lcomp-mode
      (lcomp--install)
    (lcomp--uninstall)))

;; lcomp backward compatibility
(make-obsolete 'lcomp-install 'lcomp-mode)
(make-obsolete 'lcomp-uinstall 'lcomp-mode)
(make-obsolete 'lcomp-activate-advices 'lcomp-mode)

(defun lcomp-install ()
  "Install lcomp.
This adds some hooks, advices, key definitions."
  (interactive)
  (lcomp-mode 1))

(defun lcomp-uninstall ()
  "Uninstall lcomp.
This removes some hooks, advices, key definitions."
  (interactive)
  (lcomp-mode -1))

(defun lcomp-activate-advices (on)
  "Activate lcomp advices if ON is non-nil, disable otherwise."
  (if on
      (lcomp-mode 1)
    (lcomp-mode -1)))

;; lcomp-keys
(defvar lcomp-keys-override-map
  (let ((map (or (and (boundp 'lcomp-keys-override-map)
		      (keymapp (symbol-value 'lcomp-keys-override-map))
		      (symbol-value 'lcomp-keys-override-map))
		 (make-sparse-keymap))))
    (define-key map "\C-i" 'next-completion)
    (define-key map "\M-\C-i" 'previous-completion)
    (define-key map "f" 'next-completion)
    (define-key map "b" 'previous-completion)
    (define-key map "n" 'next-line)
    (define-key map "p" 'previous-line)
    (define-key map " " 'scroll-up)
    (define-key map [del] 'scroll-down)
    (define-key map [backspace] 'scroll-down)
    (define-key map "q" 'delete-completion-window)
    map))

(defun lcomp-keys-override ()
  (push (cons 'lcomp-keys-mode
	      lcomp-keys-override-map)
	minor-mode-overriding-map-alist))

(define-minor-mode lcomp-keys-mode
  "Add keybindings to the completions buffer.

\\{lcomp-keys-override-map}"
  :global t
  (if lcomp-keys-mode
      (add-hook 'completion-list-mode-hook 'lcomp-keys-override)
    (remove-hook 'completion-list-mode-hook 'lcomp-keys-override)))

(provide 'lcomp)

;;; lcomp.el ends here
