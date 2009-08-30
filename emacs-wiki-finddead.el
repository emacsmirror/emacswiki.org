;;; emacs-wiki-finddead.el --- Find dead files in your EmacsWiki

;; Copyright (C) 2004  Jorgen Schaefer

;; Version: 1.0
;; Author: Jorgen Schaefer
;; URL: http://www.emacswiki.org/elisp/emacs-wiki-finddead.el

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Commentary:

;; This code will find and list all files in the current directory
;; that are not reachable from the current emacs-wiki page.

;;; Code:

(defun emacs-wiki-finddead ()
  "Show a buffer with all pages in the current directory that are not
reachable from the current page."
  (interactive)
  (let ((all-pages (finddead-all-pages))
        (current (file-name-nondirectory (buffer-file-name))))
    (finddead-display
     (finddead-main (delete current all-pages)
                    (list current)
                    (finddead-page-links current)))))

(defun finddead-all-pages ()
  "Return a list of all pages in the current directory."
  (let ((files '()))
    (mapc (lambda (file)
            (when (not (file-directory-p file))
              (setq files (cons file files))))
          (directory-files "."))
    files))

(defun finddead-display (lis)
  "Display the names in LIS in a separate buffer."
  (pop-to-buffer (get-buffer-create "*EmacsWikiFinddead*"))
  (let ((inhibit-read-only t))
    (delete-region (point-min) (point-max))
    (mapc (lambda (name)
            (insert name "\n"))
          ;; This sorts case-insensitive and is efficient...
          (mapcar #'car
                  (sort (mapcar (lambda (name)
                                  (cons name
                                        (downcase name)))
                                lis)
                        (lambda (a b)
                          (string< (cdr a) (cdr b))))))
    (goto-char (point-min)))
  (view-mode 1))

(defun finddead-main (all done todo)
  "Remove all pages reachable from TODO and move them to DONE."
  (while todo
    (if (member (car todo) done)
        (setq todo (cdr todo))
      (setq all (delete (car todo) all)
            done (cons (car todo) done)
            todo (append (cdr todo)
                         (finddead-page-links (car todo))))))
  ;; Whatever is left now in ALL was never referenced...
  all)

(defun finddead-page-links (pagename)
  "Return a list of pages that are linked to from PAGENAME in DIR."
  (when (file-exists-p pagename)
    (with-temp-buffer
      (insert-file-contents pagename)
      (finddead-buffer-links))))

(defun finddead-buffer-links ()
  "Return a list of pages that are linked to from the current buffer."
  (let ((links '())
        (case-fold-search nil))
    (while (re-search-forward emacs-wiki-name-regexp nil t)
      (let ((name (replace-regexp-in-string "#.*"
                                            ""
                                            (or (match-string 1)
                                                (match-string 0)))))
        (setq links (cons name links))))
    (reverse links)))

(provide 'emacs-wiki-finddead)
;;; emacs-wiki-finddead.el ends here
