;;; haskell-extension.el --- Some extension for haskell mode.

;; Author: Andy Stewart lazycat.manatee@gmail.com
;; Maintainer: Andy Stewart lazycat.manatee@gmail.com
;; Copyright (C) 2008, 2009, Andy Stewart, all rights reserved.
;; Created: 2008-10-15 08:43:29
;; Version: 0.1
;; Last-Updated: 2008-10-15 08:43:33
;; URL:
;; Keywords: haskell
;; Compatibility: GNU Emacs 23.0.60.1

;; This file is not part of GNU Emacs

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

;;; Features that might be required by this library:
;;
;; `haskell-mode'
;;

;;; Overview:
;;
;; This package support some useful function for extension haskell-mode.
;;

;;; Commentary:
;;
;; Comment
;;

;;; Installation:
;;
;; Put haskell-extension.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'haskell-extension)
;;
;; No need more

;;; Configuration:
;;
;;
;;

;;; Change log:
;;
;; 2008/10/15
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
(require 'haskell-mode)

;;; Code:

(defun comment-dwim-with-haskell-style ()
  "Call `comment-dwim' with haskell indent style."
  (interactive)
  (if (blank-line-p)
      (progn
        (call-interactively 'haskell-indent-cycle-)
        (insert comment-start))
    (if (only-comment-p)
        (progn
          (back-to-indentation)
          (forward-char (length comment-start))
          (call-interactively 'haskell-indent-cycle-))
      (call-interactively 'comment-dwim))))

(defun haskell-indent-cycle- ()
  "Comment the current line with Haskell style.
But don't insert text like `haskell-indent-cycle'."
  (interactive)
  (let ((marker (if (> (current-column) (haskell-indent-current-indentation))
                    (point-marker))))
    (haskell-indent-back-to-indentation)
    (unless (and (eq last-command this-command)
                 (eq (line-beginning-position) (car haskell-indent-last-info)))
      (save-excursion
        (setq haskell-indent-last-info
              (list (line-beginning-position) (haskell-indent-indentation-info) 0 0))))

    (let* ((il (nth 1 haskell-indent-last-info))
           (index (nth 2 haskell-indent-last-info))
           (indent-info (nth index il)))
      (if (cdr indent-info)
          (setq index (1+ index)))
      (haskell-indent-line-to (car indent-info)) ; insert indentation
      (setq haskell-indent-last-info
            (list (line-beginning-position) il (% (1+ index) (length il)) 0))
      (if marker (goto-char (marker-position marker))))))

(defun haskell-indent-insert-comment ()
  "Insert `{- | -}' comment."
  (interactive "*")
  (insert "{- | -}")
  (backward-char +2))

(provide 'haskell-extension)

;;; haskell-extension.el ends here

;;; LocalWords:  haskell dwim il
