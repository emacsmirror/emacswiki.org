;;; savekill.el --- Save kill ring to disk
;; Version: 20140418.2152

;; Time-stamp: <2014-04-18 11:29:02 rubikitch>

;; Copyright (C) 2011  rubikitch

;; Author: rubikitch <rubikitch@ruby-lang.org>
;; Keywords: tools
;; URL: http://www.emacswiki.org/cgi-bin/wiki/download/savekill.el

;; This file is free software; you can redistribute it and/or modify
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
;;
;; Save kill ring to disk every time `kill-ring' is updated.
;; It is a risk hedge of Emacs sudden death.

;;; Commands:
;;
;; Below are complete command list:
;;
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;
;;  `save-kill-file-name'
;;    *Saved `kill-ring' filename.
;;    default = "~/.emacs.d/kill-ring-saved.el"

;;; Installation:
;;
;; Put savekill.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'savekill)
;;
;; No need more.

;;; Customize:
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET savekill RET
;;


;;; History:

;; See http://www.rubyist.net/~rubikitch/gitlog/savekill.txt

;;; Code:

(eval-when-compile (require 'cl))
(defgroup savekill nil
  "savekill"
  :group 'killing)

(defcustom save-kill-file-name "~/.emacs.d/kill-ring-saved.el"
  "*Saved `kill-ring' filename."
  :type 'string
  :group 'savekill)

(defcustom savekill-max-saved-items 300
  "Maximum number of items of the kill ring list that will be saved.
A nil value means to save the whole list.
See the command `save-kill-internal'."
  :group 'savekill
  :type 'integer)

(defsubst savekill-trunc-list (l n)
  "Return from L the list of its first N elements."
  (if n
      (let (nl)
        (while (and l (> n 0))
          (setq nl (cons (car l) nl)
                n  (1- n)
                l  (cdr l)))
        (nreverse nl))
    l))

(defvar save-kill-coding-system 'utf-8)

(defun save-kill-internal ()
  (let ((coding-system-for-write save-kill-coding-system))
    (write-region
    (concat "(setq kill-ring '"
            (prin1-to-string (savekill-trunc-list
                              (mapcar 'substring-no-properties kill-ring)
                              savekill-max-saved-items))
            ")\n")
    nil save-kill-file-name nil 'silent)))

(defadvice kill-new (after savekill activate)
  "Save kill ring to `save-kill-file-name' everytime kill ring is updated."
  (save-kill-internal))
;; (progn (ad-disable-advice 'kill-new 'after 'savekill) (ad-update 'kill-new))

(add-hook 'after-init-hook (lambda () (load save-kill-file-name t)))
(provide 'savekill)

;; How to save (DO NOT REMOVE!!)
;; (progn (git-log-upload) (emacswiki-post "savekill.el"))
;;; savekill.el ends here
