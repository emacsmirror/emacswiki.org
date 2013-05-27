;;; apt-utils-ido.el --- Ido commands for apt-utils

;; Filename: apt-utils-ido.el
;; Description: Ido functions for apt-utils
;; Author: Joe Bloggs <vapniks@yahoo.com>
;; Maintainer: Joe Bloggs <vapniks@yahoo.com>
;; Copyleft (Ↄ) 2013, Joe Bloggs, all rites reversed.
;; Created: 2008
;; Version: 0.2
;; Last-Updated: 2013-05-14 03:59:52
;;           By: Joe Bloggs
;; URL: https://github.com/vapniks/apt-utils-ido
;; Keywords: convenience
;; Compatibility: GNU Emacs 24.3.1
;; Package-Requires: ((apt-utils "1.212"))
;;
;; Features that might be required by this library:
;;
;; apt-utils ido
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
;; along with this program; see the file COPYING.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary: 
;;
;; Bitcoin donations gratefully accepted: 14zv8HBJVqHRFRnXXaiLbUippnnYvg3iYv
;;
;; This library provides commands for accessing apt-utils using ido.
;; This means you don't need to remember so many keybindings.
;;
;; Press "S" to select a type of package search
;; Press "b" to select webpage to browse
;; Press "v" to select type of file to view

;;;;

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `apt-utils-ido-select-search'
;;    Use ido to select type of package search to do in apt-utils mode
;;  `apt-utils-ido-select-view'
;;    Use ido to select type of file to view in apt-utils mode
;;  `apt-utils-ido-select-browse'
;;    Use ido to select type of webpage to browse in apt-utils mode
;;

;;; Installation:
;;
;; Put apt-utils-ido.el in a directory in your load-path, e.g. ~/.emacs.d/
;; You can add a directory to your load-path with the following line in ~/.emacs
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;; where ~/elisp is the directory you want to add 
;; (you don't need to do this for ~/.emacs.d - it's added by default).
;;
;; Add the following to your ~/.emacs startup file.
;;
;; (require 'apt-utils-ido)

;;; Change log:
;;	
;; 2013/05/14
;;      * First released.
;; 

;;; Acknowledgements:
;;
;; Matthew P. Hodges <MPHodges@member.fsf.org> - Author of apt-utils
;;

;;; TODO
;;
;; 
;;

;;; Require
(require 'apt-utils)
(require 'ido)

;;; Code:

;;;###autoload
(defun apt-utils-ido-select-search (type)
  "Use ido to select type of package search to do in apt-utils mode"
  (interactive (list (ido-completing-read "Search: " 
					  (list "names & descriptions" "names only" "filenames" "selected fields"))))
  (let ((functionmap '(("names & descriptions" . apt-utils-search)
		       ("names only" . apt-utils-search-names-only)
		       ("filenames" . apt-utils-search-file-names)
		       ("selected fields" . apt-utils-search-grep-dctrl))))
    (call-interactively (cdr (assoc type functionmap)))))

;;;###autoload
(defun apt-utils-ido-select-view (type)
  "Use ido to select type of file to view in apt-utils mode"
  (interactive (list (ido-completing-read "View: " 
					  (list "files" "version" "Changelog" "Debian Changelog" "README" "Debian README" "NEWS" "Debian NEWS" "licence" "man page"))))
  (let ((functionmap '(("files" . apt-utils-list-package-files)
		       ("version" . apt-utils-view-version)
		       ("Changelog" . apt-utils-view-changelog)
		       ("Debian Changelog" . apt-utils-view-debian-changelog)
		       ("README" . apt-utils-view-readme)
		       ("Debian README" . apt-utils-view-debian-readme)
		       ("NEWS" . apt-utils-view-news)
		       ("Debian NEWS" . apt-utils-view-debian-news)
		       ("licence" . apt-utils-view-copyright)
		       ("man page" . apt-utils-view-man-page))))
    (call-interactively (cdr (assoc type functionmap)))))

;;;###autoload
(defun apt-utils-ido-select-browse (type)
  "Use ido to select type of webpage to browse in apt-utils mode"
  (interactive (list (ido-completing-read "Webpage: " 
					  (list "Debian Changelog" "bug report" "license" "package versions"))))
  (let ((functionmap '(("Debian Changelog" . apt-utils-web-browse-debian-changelog)
		       ("bug report" . apt-utils-web-browse-bug-reports)
		       ("license" . apt-utils-web-browse-copyright)
		       ("package versions" . apt-utils-web-browse-versions))))
    (call-interactively (cdr (assoc type functionmap)))))

;; Redefine keys
(define-key apt-utils-mode-map (kbd "S s") nil)
(define-key apt-utils-mode-map (kbd "S f") nil)
(define-key apt-utils-mode-map (kbd "S g") nil)
(define-key apt-utils-mode-map (kbd "S n") nil)
(define-key apt-utils-mode-map (kbd "S") 'apt-utils-ido-select-search)
(define-key apt-utils-mode-map (kbd "b C") nil)
(define-key apt-utils-mode-map (kbd "b b") nil)
(define-key apt-utils-mode-map (kbd "b l") nil)
(define-key apt-utils-mode-map (kbd "b v") nil)
(define-key apt-utils-mode-map (kbd "b") 'apt-utils-ido-select-browse)
(define-key apt-utils-mode-map (kbd "v C") nil)
(define-key apt-utils-mode-map (kbd "v R") nil)
(define-key apt-utils-mode-map (kbd "v N") nil)
(define-key apt-utils-mode-map (kbd "v c") nil)
(define-key apt-utils-mode-map (kbd "v e") nil)
(define-key apt-utils-mode-map (kbd "v f") nil)
(define-key apt-utils-mode-map (kbd "v l") nil)
(define-key apt-utils-mode-map (kbd "v m") nil)
(define-key apt-utils-mode-map (kbd "v n") nil)
(define-key apt-utils-mode-map (kbd "v r") nil)
(define-key apt-utils-mode-map (kbd "v v") nil)
(define-key apt-utils-mode-map (kbd "v") 'apt-utils-ido-select-view)

(provide 'apt-utils-ido)

;; (magit-push)
;; (yaoddmuse-post "EmacsWiki" "apt-utils-ido.el" (buffer-name) (buffer-string) "update")

;;; apt-utils-ido.el ends here


