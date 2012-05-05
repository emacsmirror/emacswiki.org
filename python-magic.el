;;; python-magic.el --- outline mode extension for python mode

;; Copyright (C) 2010 Naveen Garg

;; Maintainer: Naveen Garg
;; Version: 0.1
;; Keywords: outlines, languages

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; require outline-magic.el by CarstenDominik found here: 
;; http://www.astro.uva.nl/~dominik/Tools/outline-magic.el
;; code taken from:
;; http://stackoverflow.com/a/4093889
;; modified code here by Nikwin slightly found here: 
;; http://stackoverflow.com/a/1085551

;;; Code:

(add-hook 'outline-minor-mode-hook 
           (lambda () 
             (require 'outline-magic)
))
(add-hook 'python-mode-hook 'my-python-outline-hook)

(defun py-outline-level ()
  (let (buffer-invisibility-spec)
    (save-excursion
      (skip-chars-forward "    ")
      (current-column))))

(defun my-python-outline-hook ()
  (setq outline-regexp "[ \t]*# \\|[ \t]+\\(class\\|def\\|if\\|elif\\|else\\|while\\|for\\|try\\|except\\|with\\) ")
  (setq outline-level 'py-outline-level)

  (outline-minor-mode t)
  (hide-body)
  (show-paren-mode 1)
  (define-key python-mode-map [tab] 'outline-cycle)
  (define-key outline-minor-mode-map [S-tab] 'indent-for-tab-command)
  (define-key outline-minor-mode-map [M-down] 'outline-move-subtree-down)
  (define-key outline-minor-mode-map [M-up] 'outline-move-subtree-up)
)
(provide 'python-magic)
;;; python-magic.el ends here
