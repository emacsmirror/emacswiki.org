;;; jump.el --- interactive function that jump directly to the thing at point

;; Copyright (C) 2002  by Free Software Foundation, Inc.

;; Author: Pierre Gaston <pgas@intracom.gr>
;; Maintainer: 
;; Version: 1.0
;; Keywords: Key
;; Description:  interactive function that jump directly to the thing at point
;; URL: http://www.emacswiki.org/elisp/jump.el
;; URL: http://www.emacswiki.org/cgi-bin/wiki.pl?JumpKey

;; This is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

; This file is not part of GNU Emacs.

;; Commentary:
;;
;; provide a jump function that opens a buffer corresponding to the thing at point
;; add (require 'jump) (global-set-key [f4] 'jump)
;;
;; customize jump-functions-list to add or remove functions
;; These functions are called sequentially are responsible
;; to open the thing they found at point.
;; If they found nothing they return nil
;;
;; currently defined functions:
;; jump-to-emacs-wiki,  jump-to-ffap, jump-to-simple-wiki, jump-to-interwiki

;; TODO
;; add more jump functions
;; may be we can do this only by modifying ffap




(require 'ffap)


;;more things at point for 'wiki-name and 'interwiki-name


(defcustom  thing-at-point-wiki-name-regexp "\\<[A-Z\xc0-\xde]+[a-z\xdf-\xff]+\\([A-Z\xc0-\xde]+[a-z\xdf-\xff]*\\)+\\>"
  "Wiki Name regexp used for finding")

(defcustom  thing-at-point-interwiki-name-regexp "\\<[a-zA-Z\xc0-\xde\xdf-\xff]+:[A-Z\xc0-\xde]+[a-z\xdf-\xff]+\\([A-Z\xc0-\xde]+[a-z\xdf-\xff]*\\)+\\>"
  "InterWiki Name regexp used for finding interwiki name.")

(defcustom intermap-wiki-list 
  '(
    ("EmacsWiki" . (lambda (page) (swc-browse "ew" page)))
    ("LocalWiki" . emacs-wiki-find-file)
    ("TWiki"     . (lambda (page) (browse-url (concat "http://twiki.sourceforge.net/cgi-bin/view/TWiki" page))))
  )
  "\
alist associating a prefix and the function to use when a interwiki name is found.
The function take a page name as argument and open the page"
  :type '(alist :key-type  string  )
  :initialize 'custom-initialize-default
  :group' jump
)

(defcustom jump-functions-list '(
   jump-to-ffap)
 "\List of functions used to by jump. 
These functions open a buffer containing the thing at point,
if there is nothing at point they return nil"
)



(defun thing-at-point-bounds-of-case-sensitive-regexp-at-point (my-regex)
  (let* ((prev-case-fold-search case-fold-search) (case-fold-search nil))
    (if (thing-at-point-looking-at my-regex)
	(let ((beginning (match-beginning 0))
	      (end (match-end 0)))
	  (setq case-fold-search prev-case-fold-search)
	  (cons beginning end))
      (setq case-fold-search prev-case-fold-search)
      nil
      )
    )
)

(defun thing-at-point-bounds-of-wiki-name-at-point ()
  (thing-at-point-bounds-of-case-sensitive-regexp-at-point thing-at-point-wiki-name-regexp))

(put 'wiki-name 'bounds-of-thing-at-point 'thing-at-point-bounds-of-wiki-name-at-point )

(defun thing-at-point-bounds-of-interwiki-name-at-point ()
  (thing-at-point-bounds-of-case-sensitive-regexp-at-point thing-at-point-interwiki-name-regexp))


(put 'interwiki-name 'bounds-of-thing-at-point 'thing-at-point-bounds-of-interwiki-name-at-point )

(defun jump-to-emacs-wiki ()
  "jump to a local emacswiki (see http://repose.cx/emacs/wiki) "
  (let ((thing (thing-at-point 'wiki-name)))
    (if  thing
    (emacs-wiki-find-file thing)    
    )
  )
)

(defun jump-to-simple-wiki () 
  "\
jump using simple-wiki-completion using simple-wiki-edit mode 
see  http://www.emacswiki.org/cgi-bin/wiki.pl?SimpleWikiEditMode"
  (let ((thing (thing-at-point 'wiki-name)))
    (if thing
       (if swc-nick-current
	   (swc-follow)
	 )
      )
    )
  )
   
(defun jump-to-interwiki ()
  "jump using intermap-wiki-list\
Inter wiki name are like that Prefix:WikiName
you can customize intermap-wiki-list"
  (let ((thing (thing-at-point 'interwiki-name)))
    (when thing
      (string-match "\\(.*\\):\\(.*\\)" thing )
      (let ((wiki-func (cdr (assoc (match-string 1 thing ) intermap-wiki-list))))
	(when wiki-func
	  (funcall wiki-func (match-string 2 thing))
	  t
	  )
       )
      )
    )
)

(defun jump-to-ffap ()
  "jump using ffap"
  (let ((thing (ffap-guesser)))
  (when  thing 
      (ffap thing)
    t
    )
  )      
  )



(defun jump ()
  "Jump to the thing at point.
If a filename, open it."
  (interactive)
  (let ((jump-list jump-functions-list) found)
    (while (and (not found) jump-list)
      (setq found (funcall (car jump-list)))
      (setq jump-list (cdr jump-list))
      )
    (if found
	found
      (error "nothing to jump"))
  )
)

(provide 'jump)
 
;;; jump.el ends here


