;;; startup.el
;; startup.el is free software
; erik bourget
; GNU emacs 21

(setq load-path (append
		 '("~/emacs")
		 load-path))

(require 'backup-dir "~/emacs/backup-dir.el")
(require 'show-temp-buffer "~/emacs/show-temp-buffer.el")
(require 'whitespace "~/emacs/whitespace.el")

; desktop
(load "desktop")
(desktop-load-default)
(desktop-read)

; skeletons
(load "skeletons")

; autoloads
(autoload 'cperl-mode "cperl-mode" "" t)
(add-to-list 'auto-mode-alist '("\\.pl\\'" . cperl-mode))

; non-special keybinds
(global-set-key "\M-z" 'repeat)
(global-set-key "\M-p" 'next-error)
(global-set-key "\M-u" 'undo)
(global-set-key "\C-x\C-b" 'bs-show)
(global-set-key "\M-s" 'shell)
(global-set-key "\M-g" 'goto-line
(global-set-key [(alt c)] 'clone-indirect-buffer)
(global-set-key [mouse-4] '(lambda () (interactive) (scroll-down 5)))
(global-set-key [mouse-5] '(lambda () (interactive) (scroll-up 5)))
(global-set-key [M-up] 'enlarge-window)
(global-set-key [M-down] 'shrink-window)
(global-set-key [M-right] 'other-window)

; functions shown below
(global-set-key "\C-cg" 'insert-gpl-v2)
(global-set-key [(alt right)] 'forward-buffer)
(global-set-key [(alt left)] 'backward-buffer)

; display modes
(global-font-lock-mode 1)
(auto-insert-mode 1)
(show-paren-mode)
(display-time)

; minibuffer autocomplete
(icomplete-mode 1)
(iswitchb-mode 1)

; minibuffer resize
(resize-minibuffer-mode 1)

; allow f-b to expand to foo-bar
(partial-completion-mode)

; ghetto-vision
(set-background-color "black")
(set-foreground-color "light gray")
(transient-mark-mode 1)

; army of setq
(custom-set-variables
 '(message-log-max 100)
 '(font-lock-maximum-decoration t)
 '(line-number-mode t)
 '(column-number-mode t)
 '(mouse-yank-at-point t)
 '(c-default-style "k&r")
 '(debian-changelog-full-name "Erik Bourget")
 '(debian-changelog-mailing-address "ebourg@po-box.mcgill.ca")
 '(fill-column 79)
 '(scroll-conservatively 100)
 '(frame-title-format "%b - emacs")
 '(kill-whole-line t)
 '(inhibit-startup-message t)
; backup into ~/.backups
 '(backup-by-copying t)
 '(backup-directory-alist '(("." . "~/.backups")))
 '(delete-old-versions t)
 '(kept-new-versions 6)
 '(kept-old-versions 2)
 '(version-control t)
 '(indent-tabs-mode nil)
 '(semantic-load-turn-everything-on t)
 '(compilation-window-height 8)
 '(c-basic-offset 4)
 '(require-final-newline t)
 '(whitespace-auto-cleanup t)
 '(whitespace-global-mode 1)
 '(cperl-electric-keywords t)
 '(c-font-lock-extra-types '("FILE" "\\sw+_t" "Pango\\w+" "Gdk\\w+" "Gtk\\w+" "AppletWidget" "Gnome\\w+" "G\\(A\\(llocator\\|rray\\)\\|ByteArray\\|C\\(ache\\|o\\(mpletion\\|nd\\)\\)\\|D\\(at\\(e\\(Day\\|Year\\)\\|[ae]\\)\\|e\\(bugKey\\|stroyNotify\\)\\)\\|EqualFunc\\|H\\(ash\\(Func\\|Table\\)\\|ook\\(List\\)?\\)\\|IO\\(Channel\\|Funcs\\)\\|List\\|M\\(ainLoop\\|emChunk\\|utex\\)\\|Node\\|Object\\(Class\\)?\\|P\\(aramSpec\\|ollFD\\|rivatrArray\\)\\|Quark\\|Relation\\|S\\(List\\|canner\\(Config\\)?\\|ourceFuncs\\|t\\(atic\\(Mutex\\|Private\\)\\|ring\\(Chunk\\)?\\)\\)\\|T\\(hreadFunctions\\|ime\\(Val\\|r\\)?\\|okenValue\\|ree\\|uples\\)\\|Value\\)\\|g\\(boolean\\|c\\(har\\|onstpointer\\)\\|double\\|float\\|int\\(16\\|32\\|64\\|8\\)?\\|l\\(double\\|ong\\)\\|pointer\\|s\\(hort\\|ize\\|size\\)\\|u\\(char\\|int\\(16\\|32\\|64\\|8\\)?\\|long\\|short\\)\\)")))

; regular autocomplete
(abbrev-mode 1)
(if (file-exists-p abbrev-file-name)
    (quietly-read-abbrev-file))

; reclaim screen space
(if (and (>= emacs-major-version 21)
	 (not (eq window-system nil)))
    (progn
      (menu-bar-mode -1) ; kill menubar
      (tool-bar-mode -1) ; kill toolbar
      (scroll-bar-mode -1) ; kill scrollbar
      (blink-cursor-mode -1)
      (set-face-font 'default
		     "-dec-terminal-medium-r-normal-*-*-100-*-*-c-*-iso8859-1"
		     )))

; make all yes/no prompts y/n instead
(fset 'yes-or-no-p 'y-or-n-p)

(defun save-and-compile ()
  "Save current buffer and make."
  (interactive "")
  (save-some-buffers 0)
  (compile "make -k"))

(add-hook 'c-mode-common-hook
	  (lambda () (local-set-key "\C-c\C-c" 'save-and-compile)))

(add-hook 'c-mode-common-hook
	  (lambda () (hs-minor-mode 1)))

(add-hook 'hs-minor-mode-hook
	  (lambda () (local-set-key "\C-cs" 'hs-show-block)))
(add-hook 'hs-minor-mode-hook
	  (lambda () (local-set-key "\C-ch" 'hs-hide-block)))
(add-hook 'hs-minor-mode-hook
	  (lambda () (local-set-key "\C-cS" 'hs-show-all)))
(add-hook 'hs-minor-mode-hook
	  (lambda () (local-set-key "\C-cH" 'hs-hide-all)))

; indent on ENTER
(add-hook 'c-mode-common-hook
	  (lambda () (local-set-key "\C-m" 'newline-and-indent)))
(add-hook 'c-mode-common-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'c-mode-common-hook
	  '(lambda () (c-toggle-auto-state 1)))
(add-hook 'java-mode-hook
	  '(lambda () (c-set-style "gnu")))

(defun insert-gpl-v2 ()
  "Insert standard GPL header."
  (interactive "*")
  (insert-string "/*
 * program - the description
 * Copyright (C) 2002 Erik Bourget
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 */
"))

;==============================================================================
; tab completion
(defun indent-or-complete ()
  "Complete if point is at end of a word, otherwise indent line."
  (interactive)
  (if (looking-at "\\>")
      (dabbrev-expand nil)
    (indent-for-tab-command)
    ))

(add-hook 'c-mode-common-hook
	  (function (lambda ()
		      (local-set-key (kbd "<tab>") 'indent-or-complete)
		      )))
;==============================================================================
; buffer cycling
(defun backward-buffer () (interactive)
  "Switch to previously selected buffer."
  (let* ((list (cdr (buffer-list)))
	 (buffer (car list)))
    (while (and (cdr list) (string-match "\\*" (buffer-name buffer)))
      (progn
	(setq list (cdr list))
	(setq buffer (car list))))
    (bury-buffer)
    (switch-to-buffer buffer)))

(defun forward-buffer () (interactive)
  "Opposite of backward-buffer."
  (let* ((list (reverse (buffer-list)))
	 (buffer (car list)))
    (while (and (cdr list) (string-match "\\*" (buffer-name buffer)))
      (progn
	(setq list (cdr list))
	(setq buffer (car list))))
    (switch-to-buffer buffer)))

;;; startup.el ends here
