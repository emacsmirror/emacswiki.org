;;; indent-file.el ---

;; Copyright 2010 Yen-Chin,Lee
;;
;; Author: Yen-Chin,Lee <coldnew.tw@gmail.com>
;; Version: $Id: indent-file.el,v 0.0 2010/08/08 16:26:59 coldnew Exp $
;; Keywords: indent
;; X-URL: http://www.emacswiki.org/emacs/indent-file.el

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'indent-file)
;;   (add-hook 'emacs-lisp-mode-hook 'indent-file-when-save)
;;   (add-hook 'emacs-lisp-mode-hook 'indent-file-when-visit)

;;; Code:

(provide 'indent-file)
(eval-when-compile
    (require 'cl))



;;;;##########################################################################
;;;;  User Options, Variables
;;;;##########################################################################

(defun indent-whole-buffer ()
    "indent whole buffer and untabify it"
    (interactive)
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max) nil)
    (untabify (point-min) (point-max)))

(defun indent-file-when-save ()
    "indent file when save."
    (make-local-variable 'after-save-hook)
    (add-hook 'after-save-hook
        (lambda ()
            (if (buffer-file-name)
                (indent-whole-buffer))
            (save-buffer))))

(defun indent-file-when-visit ()
    "indent file when visit."
    (make-local-variable 'find-file-hook)
    (add-hook 'find-file-hook
        (lambda ()
            (if (buffer-file-name)
                (indent-whole-buffer))
            (save-buffer))))




;;; indent-file.el ends here
