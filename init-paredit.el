;;; init-paredit.el --- Paredit configuration

;; Filename: init-paredit.el
;; Description: Paredit configuration
;; Author: Andy Stewart lazycat.manatee@gmail.com
;; Maintainer: Andy Stewart lazycat.manatee@gmail.com
;; Copyright (C) 2008, 2009, Andy Stewart, all rights reserved.
;; Created: 2008-10-20 09:41:55
;; Version: 0.1
;; Last-Updated: 2008-10-20 09:41:55
;;           By: Andy Stewart
;; URL:
;; Keywords: paredit
;; Compatibility: GNU Emacs 23.0.60.1
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
;; Paredit configuration
;;

;;; Installation:
;;
;; Put init-paredit.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-paredit)
;;
;; No need more.

;;; Change log:
;;
;; 2008/10/20
;;      First released.
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


;;; Code:

(dolist (hook (list
               'c-mode-hook
               ;; paredit-kill (C-k) behaves badly on // comment lines in c++-mode and java-mode (at least in Emacs 24.4, paredit 24)
               ;;'c++-mode-hook
               ;;'java-mode-hook
               'haskell-mode-hook
               'emacs-lisp-mode-hook
               'lisp-interaction-mode-hook
               'lisp-mode-hook
               'maxima-mode-hook
               'ielm-mode-hook
               'sh-mode-hook
               'makefile-gmake-mode-hook
               'python-mode-hook
               'js-mode-hook
               'go-mode-hook
               'qml-mode-hook
               'web-mode-hook
               ))
  (add-hook hook '(lambda () (paredit-mode 1))))

(defvar one-key-menu-paredit-alist nil
  "The `one-key' menu alist for PAREDIT.")

(setq one-key-menu-paredit-alist
      '(
        ;; Wrap.
        (("h" . "Wrap Left Object") . paredit-backward-slurp-sexp)
        (("l" . "Wrap Right Object") . paredit-forward-slurp-sexp)
        ;; Free.
        (("," . "Free Left Object") . paredit-backward-barf-sexp)
        (("." . "Free Right Object") . paredit-forward-barf-sexp)
        ;; Remove.
        (("<" . "Remove Paren And Left Object") . paredit-splice-sexp-killing-backward)
        ((">" . "Remove Paren And Right Object") . paredit-splice-sexp-killing-forward)
        ;; Join or Split.
        (("j" . "Join Sexps") . paredit-join-sexps)
        (("k" . "Split Sexps") . paredit-split-sexp)
        (("n" . "Join next list") . paredit-join-with-next-list)
        (("p" . "Join previous list") . paredit-join-with-previous-list)
        (("N" . "Add next list") . paredit-add-to-next-list)
        (("P" . "Add previous list") . paredit-add-to-previous-list)
        ;; Move.
        (("u" . "Move list forward") . paredit-move-list-forward)
        (("i" . "Move list backward") . paredit-move-list-backward)
        ))

(defun one-key-menu-paredit ()
  "The `one-key' menu for PAREDIT."
  (interactive)
  (one-key-menu "PAREDIT" one-key-menu-paredit-alist t t nil nil t))

(provide 'init-paredit)

;;; init-paredit.el ends here
