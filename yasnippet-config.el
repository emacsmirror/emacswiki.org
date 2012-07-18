;;;; yasnippet-config.el --- Configuration of yasnippet.el
;; $Id: yasnippet-config.el,v 1.8 2012/07/18 17:19:12 rubikitch Exp $

;; Copyright (C) 2009  rubikitch

;; Author: rubikitch <rubikitch@ruby-lang.org>
;; Keywords: abbrev, languages, lisp, convenience
;; URL: http://www.emacswiki.org/cgi-bin/wiki/download/yasnippet-config.el

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
;; Configuration of yasnippet package. This file includes some hacks.

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `yas/oneshot-snippet'
;;    If `transient-mark-mode' is enabled and region is selected,
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;

;;; Installation:

;;
;; Extension and configuration of yasnippet.
;;
;; (require 'yasnippet-config)
;;
;; No need more.

;;; History:

;; $Log: yasnippet-config.el,v $
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

(defvar yasnippet-config-version "$Id: yasnippet-config.el,v 1.8 2012/07/18 17:19:12 rubikitch Exp $")
(eval-when-compile (require 'cl))

(require 'yasnippet) ;; not yasnippet-bundle

;;; dropdown-list
;; (require 'dropdown-list)
;; (setq yas/prompt-functions '(yas/dropdown-prompt))

;;; With `skk-mode'
(defadvice skk-j-mode-on (after yasnippet activate)
  (yas/minor-mode-off))
(defadvice skk-mode-exit (after yasnippet activate)
  (yas/minor-mode-on))
(defadvice skk-latin-mode-on (after yasnippet activate)
  (yas/minor-mode-on))
(defun yas/disable-when-skk-is-enabled ()
  (when (and (boundp 'skk-mode) skk-mode)
    (yas/minor-mode-off)))
(add-hook 'after-change-major-mode-hook 'yas/disable-when-skk-is-enabled t)

;;; With `view-mdoe'
;; Mysteriously after exiting view-mode, yas/minor-mode is nil.
(defadvice view-mode-exit (after yasnippet activate)
  (yas/minor-mode-on))
;; (progn (ad-disable-advice 'view-mode-exit 'after 'yasnippet) (ad-update 'view-mode-exit)) 
(defadvice view-mode-disable (after yasnippet activate)
  (yas/minor-mode-on))
;; (progn (ad-disable-advice 'view-mode-disable 'after 'yasnippet) (ad-update 'view-mode-disable)) 
(defadvice view-mode-enable (after yasnippet activate)
  (yas/minor-mode-off))
;; (progn (ad-disable-advice 'view-mode-enable 'after 'yasnippet) (ad-update 'view-mode-enable)) 

;;; Disable flymake during expansion
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

;;; `yas/buffer-local-condition' workaround
(setq yas/buffer-local-condition
      '(or (not (memq (get-text-property (point) 'face)
                      '(font-lock-comment-face font-lock-doc-face font-lock-string-face)))
           '(require-snippet-condition . force-in-comment)))

;;; oneshot snippet
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
  "If `transient-mark-mode' is enabled and region is selected,
register the region as oneshot snippet, Otherwise expand it."
  (interactive)
  (if (region-active-p)
      (yas/register-oneshot-snippet (region-beginning) (region-end))
    (yas/expand-oneshot-snippet)))

(provide 'yasnippet-config)

;; How to save (DO NOT REMOVE!!)
;; (emacswiki-post "yasnippet-config.el")
;;; yasnippet-config.el ends here
