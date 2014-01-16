;;; bracketed-paste.el --- bracketed paste mode support within emacs -nw -*- lexical-binding: t -*-

;; Copyright (C) 2014 Takeshi Banse <takebi@laafc.net>

;; Author: Takeshi Banse <takebi@laafc.net>
;; Version: 0.1.0
;; Package-Requires: ((emacs "24"))
;; Keywords: terminals

;; This program is free software; you can redistribute it and/or modify
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

;; This package provides bracketed paste mode support within emacs -nw.
;; - http://invisible-island.net/xterm/ctlseqs/ctlseqs.html (DECSET/DECRST)
;; - http://invisible-island.net/xterm/ctlseqs/ctlseqs.html#Bracketed Paste Mode

;; I've adapted these Vim's settings to Emacs.
;;
;; - http://slashdot.jp/journal/506765/Bracketed-Paste-Mode (Japanese)
;; - http://bugs.debian.org/cgi-bin/bugreport.cgi?bug=504244
;; - http://stackoverflow.com/a/7053522
;;
;; Thank you very much for Josh Triplett, IWAMOTO Kouichi, Chris Page and
;; Rainer Müller to open these settings to the public!

;; Please put the following to your ~/.emacs file then restart Emacs:
;;
;;   (require 'bracketed-paste)
;;   (bracketed-paste-enable)

;;; Note:

;; Because `bracketed-paste-enable' only calls `add-hook', it is mandatory
;; to restart Emacs, and the hook function gets called eventually.
;; If you want to get an effect in an interactive fashion, please call
;; `bracketed-paste-setup', too.
;;
;; To accumulate the "cut-buffer", the "Pasting Mode"
;; `bracketed-paste--pasting-mode' will be active, though the keymap
;; `bracketed-paste--pasting-mode-map' is kind of brute force. It is *not*
;; possible to do even `M-x' in the `bracketed-paste--pasting-mode'.
;; If you accidentaly activate/switch-to some "Pasting Mode", please recover
;; using `emacsclient` on the other terminals.

;;; Code:

(eval-when-compile (require 'cl-macs))

(defalias 'bracketed-paste--tty-state-enter
  (apply-partially 'send-string-to-terminal "\e[?2004h"))

(defalias 'bracketed-paste--tty-state-exit
  (apply-partially 'send-string-to-terminal "\e[?2004l"))

(defvar bracketed-paste--pasting-mode-map
  (let ((map `(,@(make-sparse-keymap) (t . self-insert-command))))
    (define-key map (kbd "RET") 'newline)
    (define-key map (kbd "\e[201~") 'bracketed-paste--pasting-exit)
    map))

(define-derived-mode bracketed-paste--pasting-mode  nil "Pasting"
  "Major mode used for pasting some texts within bracketed paste mode."
  (buffer-disable-undo)
  (setq-local inhibit-read-only t)
  (setq-local inhibit-modification-hooks t)
  (setq-local deactivate-mark nil)
  (setq-local indent-line-function 'ignore)
  (setq-local inhibit-redisplay t))

(defvar bracketed-paste--pasting-buffer-name " *bracketed-paste*")

(defun bracketed-paste--generate-pasting-buffer ()
  (generate-new-buffer
   (generate-new-buffer-name bracketed-paste--pasting-buffer-name)))

(defvar bracketed-paste--current-buffer nil)
(put 'bracketed-paste--current-buffer 'permanent-local t)

(defun bracketed-paste--pasting-enter ()
  (interactive)
  (let ((buffer (current-buffer)))
    (switch-to-buffer (bracketed-paste--generate-pasting-buffer))
    (setq-local bracketed-paste--current-buffer buffer)
    (bracketed-paste--pasting-mode)))

(defvar bracketed-paste-paste-command 'yank
  "*Command to `yank' the \"cut-buffer\".")

(defun bracketed-paste--pasting-exit ()
  (interactive)
  (unless (eq major-mode 'bracketed-paste--pasting-mode)
    (error "Expected `bracketed-paste--pasting-mode': %s" major-mode))
  (bracketed-paste--pasting-exit-aux bracketed-paste-paste-command
                                     bracketed-paste--current-buffer))

(defun bracketed-paste--pasting-exit-aux (paste-command paste-buffer)
  (kill-region (point-min) (point-max))
  (kill-buffer)
  (switch-to-buffer paste-buffer)
  (call-interactively paste-command)
  (setq this-command 'yank))

(defvar bracketed-paste--setup-entry-hooks
  '(suspend-resume-hook resume-tty-functions))

(defvar bracketed-paste--setup-exit-hooks
  '(suspend-hook suspend-tty-functions kill-emacs-hook delete-frame-functions))

(defun bracketed-paste--safe-tty-state-call (terminalish send-tty)
  (cond ((null terminalish) (funcall send-tty))
        ((and (eq (terminal-live-p terminalish) t) ; borrowed from xt-mouse.el
              (not (string= (terminal-name terminalish) "initial_terminal")))
         (funcall send-tty terminalish))))

(defun bracketed-paste--tty-state-enter-hook (&optional arg)
  (bracketed-paste--safe-tty-state-call arg 'bracketed-paste--tty-state-enter))

(defun bracketed-paste--tty-state-exit-hook (&optional arg)
  (bracketed-paste--safe-tty-state-call arg 'bracketed-paste--tty-state-exit))

(defun bracketed-paste--setup ()
  "Setup hooks and keybindings for bracketed paste mode on the terminal."
  (cl-labels ((flip* (fn) #'(lambda (&rest args) (apply fn (reverse args))))
              (hookf (fn) (apply-partially (flip* 'add-hook) fn)))
    (mapc (hookf 'bracketed-paste--tty-state-enter-hook)
          bracketed-paste--setup-entry-hooks)
    (mapc (hookf 'bracketed-paste--tty-state-exit-hook)
          bracketed-paste--setup-exit-hooks))
  (global-set-key (kbd "\e[200~") 'bracketed-paste--pasting-enter)
  (global-set-key (kbd "\e[201~") 'bracketed-paste--pasting-exit)
  (define-key minibuffer-local-map (kbd "\e[200~") 'ignore)
  (define-key minibuffer-local-map (kbd "\e[201~") 'ignore))

;; Evil
(defvar evil-normal-state-map)
(defvar evil-insert-state-map)
(defvar evil-visual-state-map)
(defvar evil-replace-state-map)
(declare-function evil-insert-state-p "evil-states")
(declare-function evil-visual-state-p "evil-states")
(declare-function evil-replace-state-p "evil-states")

(defun bracketed-paste--evil-setup ()
  (add-hook 'bracketed-paste--pasting-mode-hook 'turn-off-evil-mode)
  (setq bracketed-paste-paste-command
        ;; TODO: make evilish
        ;; XXX: pasting in replace-state behavior differs inside/outside
        ;; bracketed paste mode. GVim's behaviour here for an ease.
        (defun bracketed-paste--evil-paste ()
          (interactive)
          (call-interactively (if (evil-visual-state-p)
                                  'evil-paste-after
                                'evil-paste-before))
          (when (or (evil-insert-state-p)
                    (evil-replace-state-p))
            (forward-char))))
  (cl-flet ((map-paste-key (&rest maps)
              (mapc #'(lambda (map)
                        (define-key map (kbd "\e[200~")
                          'bracketed-paste--pasting-enter))
                    maps)))
    (map-paste-key evil-normal-state-map
                   evil-insert-state-map
                   evil-visual-state-map
                   evil-replace-state-map)))

;;;###autoload
(defun bracketed-paste-enable ()
  "Enable bracketed paste mode support on the terminal."
  (add-hook 'tty-setup-hook
            (let (setupp)
              (defun bracketed-paste-setup ()
                "Setup bracketed paste mode support."
                (bracketed-paste--tty-state-enter)
                (unless setupp
                  (bracketed-paste--setup)
                  (when (featurep 'evil)
                    (with-eval-after-load 'evil
                      (bracketed-paste--evil-setup)))
                  (setq setupp t))))))

(provide 'bracketed-paste)
;;; bracketed-paste ends here
