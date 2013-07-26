;;; ido-gnus.el --- Access gnus groups or servers using ido

;; Filename: ido-gnus.el
;; Description: Access gnus groups or servers using ido
;; Author: Joe Bloggs <vapniks@yahoo.com>
;; Maintainer: Joe Bloggs <vapniks@yahoo.com>
;; Copyleft (Ↄ) 2013, Joe Bloggs, all rites reversed.
;; Created: 2013-06-21 12:26:18
;; Version: 0.4
;; Last-Updated: 2013-07-26 16:30:00
;;           By: Joe Bloggs
;; URL: https://github.com/vapniks/ido-gnus
;; Keywords: comm
;; Compatibility: GNU Emacs 24.3.1
;; Package-Requires: ((gnus "5.13"))
;;
;; Features that might be required by this library:
;;
;; gnus ido
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
;; Bitcoin donations gratefully accepted: 16mhw12vvtzCwQyvvLAqCSBvf8i41CfKhK
;;

;;; Commands:
;;
;; Below is a complete command list:
;;
;;  `ido-gnus-select-group'
;;    Select a gnus group to visit using ido.
;;  `ido-gnus-select-server'
;;    Select a gnus server to visit using ido.
;;  `ido-gnus-select'
;;    Select a gnus group/server or existing gnus buffer using ido.
;;; Customizable Options:
;;
;; Below is a customizable option list:
;;
;;  `ido-gnus-show-article'
;;    If non-nil then an article will be auto-selected on group entry.
;;    default = nil
;;  `ido-gnus-num-articles'
;;    The number of articles to display when a group is visited.
;;    default = t
;;
;; All of the above can customized by:
;;      M-x customize-group RET ido-gnus RET
;;


;;; Installation:
;;
;; Put ido-gnus.el in a directory in your load-path, e.g. ~/.emacs.d/
;; You can add a directory to your load-path with the following line in ~/.emacs
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;; where ~/elisp is the directory you want to add 
;; (you don't need to do this for ~/.emacs.d - it's added by default).
;;
;; Add the following to your ~/.emacs startup file.
;;
;; (require 'ido-gnus)



;;; Change log:
;;	
;; 2013/06/21
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
(require 'gnus)
(require 'ido)

;;; Code:
(defgroup ido-gnus nil
  "Access to gnus groups using ido."
  :group 'gnus
  :group 'ido)

(defcustom ido-gnus-show-article nil
  "If non-nil then an article will be auto-selected on group entry."
  :group 'ido-gnus
  :type 'boolean)

(defcustom ido-gnus-num-articles t
  "The number of articles to display when a group is visited.
If this is a positive number, fetch this number of the latest
articles in the group.  If it is a negative number, fetch this
number of the earliest articles in the group.
If it is not a number then prompt the user for the number of articles."
  :group 'ido-gnus
  :type '(choice (const :tag "Prompt for number of articles." t)
                 (integer :tag "Number of articles")))

;;;###autoload
(defun ido-gnus-select-group (prefix)
  "Select a gnus group to visit using ido.
If a prefix arg is used then the sense of `ido-gnus-num-articles' will be reversed:
  if it is a number then the number of articles to display will be prompted for,
otherwise `gnus-large-newsgroup' articles will be displayed.

gnus will be started if it is not already running."
  (interactive "P")
  (unless gnus-newsrc-alist (gnus))
  (let* ((groups (mapcar 'car (cdr gnus-newsrc-alist)))
         (group (ido-completing-read "Group: " groups nil t)))
    (if (member group groups)
        (gnus-group-read-group (if prefix
                                   (if (numberp ido-gnus-num-articles) t
                                     gnus-large-newsgroup)
                                 ido-gnus-num-articles)
                               (not ido-gnus-show-article) group))))

;;;###autoload
(defun ido-gnus-select-server nil
  "Select a gnus server to visit using ido.

gnus will be started if it is not already running."
  (interactive)
  (unless gnus-newsrc-alist (gnus))
  (unless (get-buffer gnus-server-buffer) (gnus-enter-server-buffer))
  (let* ((srvs (mapcar (lambda (x) (concat (symbol-name (caar x)) ":" (cadar x)))
                       gnus-opened-servers))
         (srv (ido-completing-read "Server: " srvs nil t)))
    (if (member srv srvs)
        (with-current-buffer gnus-server-buffer
          (gnus-server-read-server srv)))))

;;;###autoload
(defun ido-gnus-select (prefix)
  "Select a gnus group/server or existing gnus buffer using ido."
  (interactive "P")
  (unless gnus-newsrc-alist (gnus))
  (let* ((gnusbuffers (loop for buf in (buffer-list)
                            for mode = (with-current-buffer buf major-mode)
                            if (memq mode (list 'gnus-article-mode 'gnus-summary-mode))
                            collect (buffer-name buf)))
         (items (append '("Group buffer" "Other groups" "Server buffer" "Other servers") gnusbuffers))
         (item (ido-completing-read "Open: " items nil t)))
    (cond ((equal item "Other groups") (ido-gnus-select-group prefix))
          ((equal item "Group buffer") (switch-to-buffer gnus-group-buffer))
          ((equal item "Other servers") (ido-gnus-select-server))
          ((equal item "Server buffer")
           (unless (get-buffer gnus-server-buffer) (gnus-enter-server-buffer))
           (switch-to-buffer gnus-server-buffer))
          (t (switch-to-buffer item)))))

(provide 'ido-gnus)

;; (magit-push)
;; (yaoddmuse-post "EmacsWiki" "ido-gnus.el" (buffer-name) (buffer-string) "update")

;;; ido-gnus.el ends here
