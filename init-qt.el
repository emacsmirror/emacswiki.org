;;; init-qt.el --- Setup for qt mode

;; Filename: init-qt.el
;; Description: Setup for qt mode
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2017, Andy Stewart, all rights reserved.
;; Created: 2017-02-06 21:29:16
;; Version: 0.1
;; Last-Updated: 2017-02-06 21:29:16
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/init-qt.el
;; Keywords:
;; Compatibility: GNU Emacs 25.0.50.1
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
;; Setup for qt mode
;;

;;; Installation:
;;
;; Put init-qt.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-qt)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET init-qt RET
;;

;;; Change log:
;;
;; 2017/02/06
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


;;; Code:
(defun c-mode-style-setup ()
  (interactive)
  "Set up c-mode and related modes.

 Includes support for Qt code (signal, slots and alikes)."
  ;; eldoc.
  (require 'c-eldoc)
  (c-turn-on-eldoc-mode)

  ;; base-style
  (c-set-style "stroustrup")

  ;; qt keywords and stuff ...
  ;; set up indenting correctly for new qt kewords
  (setq c-protection-key (concat "\\<\\(public\\|public slot\\|protected"
                                 "\\|protected slot\\|private\\|private slot"
                                 "\\)\\>")
        c-C++-access-key (concat "\\<\\(signals\\|public\\|protected\\|private"
                                 "\\|public slots\\|protected slots\\|private slots"
                                 "\\)\\>[ \t]*:"))
  (progn
    ;; modify the colour of slots to match public, private, etc ...
    (font-lock-add-keywords 'c++-mode
                            '(("\\<\\(slots\\|signals\\)\\>" . font-lock-type-face)))
    ;; make new font for rest of qt keywords
    (make-face 'qt-keywords-face)
    (set-face-foreground 'qt-keywords-face "DeepSkyBlue1")
    ;; qt keywords
    (font-lock-add-keywords 'c++-mode
                            '(("\\<Q_OBJECT\\>" . 'qt-keywords-face)))
    (font-lock-add-keywords 'c++-mode
                            '(("\\<SIGNAL\\|SLOT\\>" . 'qt-keywords-face)))
    (font-lock-add-keywords 'c++-mode
                            '(("\\<Q[A-Z][A-Za-z]*" . 'qt-keywords-face)))
    ))

(dolist (hook (list
               'c-mode-hook
               'c++-mode-hook
               'c-mode-common-hook
               ))
  (add-hook hook '(lambda () (c-mode-style-setup))))

(dolist (elt-cons '(
                    ("\\.h$" . c++-mode)
                    ))
  (add-to-alist 'auto-mode-alist elt-cons))

(provide 'init-qt)

;;; init-qt.el ends here
