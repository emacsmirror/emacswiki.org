;;; init-irc.el --- Init irc

;; Filename: init-irc.el
;; Description: Init irc
;; Author: Andy Stewart <andy@freedom>
;; Maintainer: Andy Stewart <andy@freedom>
;; Copyright (C) 2013, Andy Stewart, all rights reserved.
;; Created: 2013-12-30 14:51:52
;; Version: 0.1
;; Last-Updated: 2013-12-30 14:51:52
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/init-irc.el
;; Keywords: 
;; Compatibility: GNU Emacs 24.3.50.1
;;
;; Features that might be required by this library:
;;
;; 
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary: 
;; 
;; Init irc
;; 

;;; Installation:
;;
;; Put init-irc.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-irc)
;;
;; No need more.

;;; Customize:
;;
;; 
;;
;; All of the above can customize by:
;;      M-x customize-group RET init-irc RET
;;

;;; Change log:
;;	
;; 2013/12/30
;;      * First released.
;; 

;;; Acknowledgements:
;;
;; 
;;

;;; TODO
;;
;; 
;;

;;; Require

(require 'one-key)

;;; Code:

(defvar one-key-menu-irc-channel-alist nil
  "The `one-key' menu alist for IRC-CHANNEL.")

(setq one-key-menu-irc-channel-alist
      '(
        (("e" . "#emacs") . (lambda () (interactive) (try-to-switch-buffer "#emacs")))
        (("h" . "#haskell") . (lambda () (interactive) (try-to-switch-buffer "#haskell")))
        (("x" . "#xmonad") . (lambda () (interactive) (try-to-switch-buffer "#xmonad")))
        (("l" . "#lisp") . (lambda () (interactive) (try-to-switch-buffer "#lisp")))
        (("d" . "#debian") . (lambda () (interactive) (try-to-switch-buffer "#debian")))
        (("z" . "#zsh") . (lambda () (interactive) (try-to-switch-buffer "#zsh")))
        (("m" . "#mijamedia") . (lambda () (interactive) (try-to-switch-buffer "##manatee")))
        (("y" . "#yi") . (lambda () (interactive) (try-to-switch-buffer "#yi")))
        (("a" . "#android-dev") . (lambda () (interactive) (try-to-switch-buffer "#android-dev")))
        (("u" . "#ubuntu") . (lambda () (interactive) (try-to-switch-buffer "#ubuntu")))
        (("s" . "##English") . (lambda () (interactive) (try-to-switch-buffer "##English")))
        (("p" . "#python") . (lambda () (interactive) (try-to-switch-buffer "#python")))
        (("n" . "#mandarin") . (lambda () (interactive) (try-to-switch-buffer "#mandarin")))))

(defun one-key-menu-irc-channel ()
  "The `one-key' menu for IRC-CHANNEL."
  (interactive)
  (one-key-menu "IRC-CHANNEL" one-key-menu-irc-channel-alist t))

(provide 'init-irc)

;;; init-irc.el ends here
