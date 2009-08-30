;;; planner-favoris.el -- add some bookmarks in a planner day page
;;
;;
;; Copyright 2008 Thierry Volpiatto

;; This file is not part of planner.

;; planner is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; planner is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with planner; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

;; Title:planner-favoris.el
;; Author:ThierryVolpiatto
;; Contact: thierry dot volpiatto from gmail dot com

;	$Id: planner-favoris.el,v 1.8 2008/02/20 07:36:47 thierry Exp $	

;;; Usage:
;; 1) add to your planner-day-page-template:
;; * Favoris\n\n
;; it should look some things like that:
;; (setq planner-day-page-template
;;      "* Schedule\n\n\n* Tasks\n\n\n* Diary\n\n\n* Favoris\n\n* Notes\n")
;; 2) Add to .emacs in your planner settings:
;; (require 'planner-favoris)
;; (add-hook 'planner-goto-hook 'planner-favoris-insert)
;; 3) Touch ~/.emacs.d/.planner-favoris 
;; (or the path you have set in planner-favoris-file)
;; add path to file or lisp command or anyting you want
;; to your .planner-favoris (see muse manual)
;; 4) You can add or delete favoris manually or within
;; your planner page with the functions
;; planner-favoris-append-favori
;; and
;; planner-favoris-delete-favori

;; Example of .planner-favoris file:
;;[[lisp:planner-notes-index-weeks][Voir Semaine(n)]]
;;[[~/finance/ledger.dat][Voir Compta]]
;;[[info:///usr/share/info/elisp#Top][Emacs lisp Reference]]
;;[[lisp:/elisp-index-search][Index Elisp Manual]]
;;[[lisp:/(emms-lastfm-radio-global-tag "rock")][Lastfm Rock Music]]

;;; Code:


(require 'planner)

;;; Customize

(defgroup planner-favoris nil
  "Provide a way to have some bookmarks in planner"
  :prefix "planner-favoris"
  :group 'planner)

(defcustom planner-favoris-regex 
  "^\* Favoris *$"
  "regex to match in a planner file"
  :type 'string
  :group 'planner)

(defcustom planner-favoris-file
  "~/.emacs.d/.planner-favoris"
  "The default file to store favoris"
  :type 'string
  :group 'planner)

(defun tv-read-and-insert-favoris (regex file)
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward regex nil t)
      (progn
	(newline 1)
	(insert-file-contents file nil 0 (nth 7 (file-attributes file)))))))

(defun tv-clear-favoris (reg-start reg-end)
  (save-excursion
  (goto-char (point-min))
  (when (re-search-forward reg-start nil t)
    (progn
      (forward-line)
      (delete-region (point)
		     (if (re-search-forward reg-end nil t)
			 (line-beginning-position)
		       (point-max)))))))

(defun planner-favoris-insert ()
  (interactive)
  (tv-clear-favoris planner-favoris-regex "^\\*")
  (tv-read-and-insert-favoris planner-favoris-regex planner-favoris-file))

(defun planner-favoris-append-favori (favori)
  (interactive "sNewFavori: ")
  (save-excursion
    (find-file planner-favoris-file)
    (goto-char (point-max))
    (if (looking-back "\]")
	(forward-line))
    (insert favori)
    (save-buffer)
    (planner-goto-today)))

(defun planner-favoris-delete-favori ()
  (interactive)
  (kill-line)
  (delete-blank-lines)
  (save-excursion
    (let (end
	  beg)
	  (when (re-search-forward "^\\*")
		 (forward-line -1)
		 (end-of-line)
		 (setq end (point)))
      (when (re-search-backward planner-favoris-regex)
	(forward-line 1)
	(beginning-of-line)
	(setq beg (line-beginning-position)))
      (copy-region-as-kill beg end)
      (find-file planner-favoris-file)
      (goto-char (point-min))
      (delete-region (point-min) (point-max))
      (yank)
      (save-buffer)
      (planner-goto-today))))


(provide 'planner-favoris)

;;; planner-favoris.el ends here
