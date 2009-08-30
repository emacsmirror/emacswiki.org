;;;; yasnippet-config.el --- Configuration of yasnippet.el
;; $Id: yasnippet-config.el,v 1.3 2009/07/21 17:14:26 rubikitch Exp rubikitch $

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
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;

;;; Installation:
;;
;; For full install of the normal archive, just download and unpack
;; the latest yasnippet-x.y.z.tar.bz2. You'll get a directory named
;; `yasnippet, put it in ~/.emacs.d/plugins/
;;
;; http://code.google.com/p/yasnippet/
;;
;; Put yasnippet-config.el to your load-path.  The load-path is
;; usually ~/.emacs.d/.  It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/.emacs.d"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'yasnippet-config)
;; (yas/setup "~/.emacs.d/plugins/yasnippet")
;;
;; No need more.

;;; History:

;; $Log: yasnippet-config.el,v $
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

(defvar yasnippet-config-version "$Id: yasnippet-config.el,v 1.3 2009/07/21 17:14:26 rubikitch Exp rubikitch $")
(eval-when-compile (require 'cl))

;;; Setup
(defun yas/setup (package-directory)
  ;; Ensure to end with /
  (setq package-directory (file-name-as-directory package-directory))
  (add-to-list 'load-path package-directory)
  (require 'yasnippet) ;; not yasnippet-bundle
  (yas/initialize)
  (yas/load-directory (concat package-directory "snippets")))

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


;;; Automatic reload after snippet modification
(defun yas/snippet-file-p (filename)
  "Return non-nil if FILENAME is yasnippet snippet file."
  (when filename
    (setq filename (expand-file-name filename))
    (loop for dir in yas/root-directory
          for edir = (expand-file-name dir)
          for len = (length edir)
          thereis (equal edir (ignore-errors (substring filename 0 len))))))
(defun yas/after-save-hook ()
  (when (yas/snippet-file-p buffer-file-name)
    (yas/reload-all)))
(add-hook 'after-save-hook 'yas/after-save-hook)

(provide 'yasnippet-config)

;; How to save (DO NOT REMOVE!!)
;; (emacswiki-post "yasnippet-config.el")
;;; yasnippet-config.el ends here
