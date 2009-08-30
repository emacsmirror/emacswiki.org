;;; help-url.el --- Un module pour se connecter aux aides en lignes de php ou de coldfusion

;; Copyright (C) 2003  Free Software Foundation, Inc.

;; Author: Norbert Caudéran <address@bogus.example.com>
;; Keywords:

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
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;

;;; Code:
(defvar help-url-cfm-base-help-url
  '("file:c:\\Program Files\\Allaire\\HomeSite
4.5\\Extensions\\Docs\\CFMLTags" "file:c:\\Program Files\\Allaire\\HomeSite
4.5\\Extensions\\Docs\\CFMLFunctions")
  "Les répertoires dans lesquels sont stockés les fichiers d'aide CFM, ou
l'unique URL")

(defvar help-url-php-base-help-url
  '("http://www.php.net/";)
  "Les répertoires dans lesquels sont stockés les fichiers d'aide PHP, ou
l'unique URL")


(defun word-2-url-cfm (base word)
  "Crée l'url cfm"
  (concat base "\\" word ".htm")
)


(defun word-2-url-php (base word)
  "Crée l'url php"
  (concat base word)
)

(fset 'word-2-url 'word-2-url-cfm)
(setq help-url-base-help-url help-url-cfm-base-help-url)

(defun help-url-find-page (word)
  "Crée une liste d'url possibles à partir du mot clé"
  (setq nberr 0)
  (dolist (url (mapcar
  (lambda (base) (word-2-url  base word))
  help-url-base-help-url))
    (condition-case err
 (browse-url url)
      (error
       (setq nberr (+ nberr 1)))))
  (if (eq nberr (length help-url-cfml-base-help-url))
      (concat "Pas d'aide disponible pour " word)
    "Affichage du fichier d'aide"))

(defun help-url-at-word ()
  "Affiche la page d'aide du mot courant"
  (interactive)
  (message (help-url-find-page (current-word t))))

(global-set-key [C-f4] 'help-url-at-word)

(provide 'help-url)
;;; help-url.el ends here
