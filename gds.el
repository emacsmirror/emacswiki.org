;;; gds.el --- Emacs interface to Google Desktop Search
;;
;; Copyright (C) 2006 Mathias Dahl
;;
;; Version: 0.2
;; Keywords: search, convenience, files, gds, google
;; Author: Mathias Dahl <mathias.rem0veth1s.dahl@gmail.com>

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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; gds.el provides a couple of commands to interface Google Desktop
;; Search (GDS) from Emacs.
;;
;; gds.el should work on all operating systems where GDS exist.
;; Currently it seems only to exist officially in Microsoft Windows
;; and Mac OSX but there is supposedly some sort of beta for GNU/Linux
;; as well.
;;
;; Basically all it does is to build a URL with the search, send it to
;; GDS and do different fun things with the result.  The commands
;; currently implemented are `gds', `gds-iswitchb-open-file' and
;; `gds-dired'.
;;
;; It is easy to use, just customize `gds-search-url' and get going! I
;; recommend starting with the `gds' command.
;;
;;; Todo:
;;
;;; History:
;;
;; Version 0.1, 2006-02-08
;;
;; * First release.
;;
;; Version 0.1.1, 2006-02-09
;;
;; * Finished first version of `gds-dired'.
;;
;; Version 0.2, 2007-07-06
;;
;; * Added the `gds' command.
;; * Made some refactoring.
;; * Added some helper functions.
;;

;;; Code:

(require 'url)
(require 'xml)
(require 'cl)

(defcustom gds-search-url ""
  "Search URL for the GDS installation.
You must specify this value.  It can be found in the Windows
registry, under
HKEY_CURRENT_USER\\Software\\Google\\Google Desktop\\AP\I\search_url"
  :type 'string
  :group 'gds)

(defun gds-search-url ()
  "Get GDS search url.  Raise error if empty or nil.
Maybe this is ugly, I don't know, but it sure should make the
user configure this..."
  (if (or (string= "" gds-search-url)
          (not gds-search-url))
      (error "Option gds-search-url is empty or nil")
    gds-search-url))

(defun gds-get-xml-response (search &optional num)
  "Do a Google Desktop Search for SEARCH, return result as xml.
Optional argument NUM is used to set the number of hits to get."
  (with-temp-buffer
    (url-insert-file-contents
     (format "%s%s&format=xml%s"
             (gds-search-url)
             search
             (if num
                 (format "&num=%d" num)
               "")))
    (goto-char (point-min))
    (if (not (search-forward-regexp "^<\\?xml" nil t))
        (message "No XML data from GDS search")
      (xml-parse-region (match-beginning 0) (point-max)))))

(defcustom gds-ignored-file-names-regexp ""
  "Regexp of file names to remove from the hit list."
  :type 'string
  :group 'gds)

(defun gds-get-matching-files (search &optional num)
  "Search GDS for files containing SEARCH.
Returns a list of file names.

Optional argument NUM sets the number of search results.  Default
is 10 hits.

Only hits of category \"file\" are returned. File names matching
`gds-ignored-file-names-regexp' will be removed."
  (let ((xml (xml-get-children
              (car (gds-get-xml-response search num)) 'result))
        result results)
    (while xml
      (setq result (car xml))
      (let ((category  (car (xml-node-children (assoc 'category result))))
            (file-name (car (xml-node-children (assoc 'url result)))))
        (if (and (string= "file" category)
                 (not (string-match gds-ignored-file-names-regexp file-name)))
            (setq results (append
                           results
                           (list file-name)))))
      (setq xml (cdr xml)))
    results))

(defvar gds-ignored-categories nil
  "List of strings of ignored categories.
Example of categories are file, web and email.  The whole list can
be found here: http://desktop.google.com/dev/queryapi.html.  For
some reason the category can sometimes be nil, so you can add
this as an item in the list as well.")

(defun gds-get-matching-items (search &optional num)
  "Search GDS for items containing SEARCH.
Returns a list of items names.

Optional argument NUM sets the number of search results.  Default
is 10 hits.

Hits with a category in `gds-ignored-categories' will not be
returned.

File names matching `gds-ignored-file-names-regexp' will be
removed."
  (let ((xml (xml-get-children
              (car (gds-get-xml-response search num)) 'result))
        result results)
    (while xml
      (setq result (car xml))
      (let ((category  (car (xml-node-children (assoc 'category result))))
            (file-name (car (xml-node-children (assoc 'url result))))
            (title (car (xml-node-children (assoc 'title result))))
            (cache-url (car (xml-node-children (assoc 'cache_url result)))))
        (if (and (not (member category gds-ignored-categories))
                 (not (and (string= category "file")
                           (string-match gds-ignored-file-names-regexp file-name))))
            (setq results (append
                           results
                           (list (list category title file-name cache-url))))))
      (setq xml (cdr xml)))
    results))

(defun gds-browse-url ()
  "Browse to URL given by the title's URL property."
  (interactive)
  (browse-url (get-text-property (point) 'url)))

(defun gds-mouse-browse-url (start-event)
  "Browse to URL given by the title's URL property.
Argument START-EVENT is not documented."
  (interactive "e")
  (gds-browse-url))

(defvar gds-title-map nil
  "Keymap for the title in the result list.")

(progn
  (setq gds-title-map (make-sparse-keymap))
  (define-key gds-title-map (kbd "RET") 'gds-browse-url)
  (define-key gds-title-map (kbd "<mouse-1>") 'gds-mouse-browse-url))

(define-derived-mode gds-display-mode fundamental-mode "GDS Display"
  "Major mode for browsing hits from GDS")

(progn
  (setq gds-display-mode-map (make-sparse-keymap))
  (define-key gds-display-mode-map (kbd "q") 'bury-buffer)
  (define-key gds-display-mode-map (kbd "TAB") 'gds-find-next-title))

(defface gds-title-face '((t (:foreground "Red" :weight bold)))
  "*Face used to highlight the title of search results.."
  :group 'gds :group 'faces)

(defface gds-url-face '((t (:foreground "Gray")))
  "*Face used to highlight the URL of search results.."
  :group 'gds :group 'faces)

(defcustom gds-max-results 40
  "Maximum number of results to search for.
Note that this is the number of hits to get from GDS *before*
filtering non-file hits and file names matching
`gds-ignored-file-names-regexp'.  This means that you probably
want to make this number higher that the number of files you want
to choose from at the prompt."
  :type 'integer
  :group 'gds)

(defun gds (search-string)
  "Make a Google Desktop search and display the results.
Argument SEARCH-STRING is the string to search for."
  (interactive "MSearch string: ")
  (let ((results (gds-get-matching-items search-string gds-max-results))
        (buf (get-buffer-create "*gds-search*")))
    (switch-to-buffer buf)
    (gds-display-mode)
    (toggle-truncate-lines 1)
    (toggle-read-only 0)
    (erase-buffer)
    (insert "\n*** Click mouse-1 or type `RET' on an item title
    to find the item in GDS. Type `TAB' to jump to the next
    search result or use the normal movement keys to move
    around. Type `q' to quit.\n\n")
    (mapc
     (lambda (x)
       (insert (format "%-5s %-60s (%s)\n" (if (car x)
                                               (capitalize (car x))
                                             "Other")
                       (propertize (cadr x)
                                    'face 'gds-title-face
                                    'mouse-face 'highlight
                                    'url (cadddr x)
                                    'keymap gds-title-map)
                        (propertize (cadddr x)
                                    'face 'gds-url-face))))
     results))
  (goto-char (point-min))
  (set-buffer-modified-p nil)
  (toggle-read-only 1)
  (gds-find-next-title))

(defun gds-next-url-property-change ()
  "Return position of next change in the url property."
  (next-single-property-change (point) 'url nil (point-max)))

(defun gds-find-next-title ()
  "Go to next search result.
If at the bottom, wrap around to the top."
  (interactive)
  (goto-char (gds-next-url-property-change))
  (let (p)
    (when (not (get-text-property (point) 'url))
      (setq p (gds-next-url-property-change))
      (if (eq p (point-max))
          (progn
            (goto-char (point-min))
            (goto-char (gds-next-url-property-change)))
        (goto-char p)))))

(defcustom gds-iswitchb-open-file-number-of-hits 20
  "Number of hits to fetch from GDS when using `gds-iswitchb-open-file'.
Note that this is the number of hits to get from GDS *before*
filtering non-file hits and file names matching
`gds-ignored-file-names-regexp'.  This means that you probably
want to make this number higher that the number of files you want
to choose from at the prompt."
  :type 'integer
  :group 'gds)

(defun gds-iswitchb-open-file (search)
  "Use iswitchb to open file matching SEARCH in GDS."
  (interactive "sSearch for: ")
  (let ((iswitchb-make-buflist-hook
	 (lambda ()
	   (setq iswitchb-temp-buflist (gds-get-matching-files search 20)))))
    (find-file (iswitchb-read-buffer "Find file: "))))

(defcustom gds-dired-number-of-hits 100
  "Number of hits to fetch from GDS when using `gds-dired'.
Note that this is the number of hits to get from GDS *before*
filtering non-file hits and file names matching
`gds-ignored-file-names-regexp'.  This means that you probably
want to make this number higher that the number of files you want
to choose from at the prompt."
  :type 'integer
  :group 'gds)

(defun gds-dired ()
  "Use Google Desktop Search to find files and list them in dired.
It generates a result like `find-dired' does, but uses Google
Desktop Search to find matching files."
  (interactive)
  (let ((dir (read-directory-name "Set current directory: "))
        (search (read-string "Search string: "))
        (buf (get-buffer-create "*gds-dired*")))
    (switch-to-buffer buf)
    (kill-all-local-variables)
    (setq buffer-read-only nil)
    (erase-buffer)
    (setq default-directory dir)
    (dired-mode dir)
    (use-local-map (append (make-sparse-keymap) (current-local-map)))
    (define-key (current-local-map) "g" 'undefined)
     ;; Set subdir-alist so that Tree Dired will work:
     (if (fboundp 'dired-simple-subdir-alist)
         ;; will work even with nested dired format (dired-nstd.el,v
         ;; 1.15 and later)
         (dired-simple-subdir-alist)
       ;; else we have an ancient tree dired (or classic dired, where
       ;; this does no harm)
       (set (make-local-variable 'dired-subdir-alist)
            (list (cons default-directory (point-min-marker)))))
    (setq buffer-read-only nil)
    (insert "  " dir ":\n")
    ;; Make second line a ``dir'' line in analogy to the ``total'' or
    ;; ``wildcard'' line.
    (insert "  GDS search results for \"" search "\"\n")
    (let ((buffer-read-only nil)
          (saved-pos nil))
      (goto-char (point-max))
      (setq mode-line-process (concat ": GDS search"))
      (mapc
       (lambda (x)
         (when (file-exists-p x)
           (insert "  ")
           (insert-directory (expand-file-name x) "")))
       (gds-get-matching-files search gds-dired-number-of-hits))

      (insert " at " (substring (current-time-string) 0 19))
      (force-mode-line-update))
    (goto-char (point-min))
    (forward-line 1)
    (dired-next-line 1)))

;; Experimental
(defun gds-dired-2 ()
  "Use Google Desktop Search to find files and list them in dired.
It generates a result like `find-dired' does, but uses Google
Desktop Search to find matching files."
  (interactive)
  (let ((dir (read-directory-name "Set current directory: "))
        (search (read-string "Search string: ")))
       (dired (cons dir (gds-get-matching-files
                         search gds-dired-number-of-hits)))))

(provide 'gds)

;;; gds.el ends here
