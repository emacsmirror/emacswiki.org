;;; yas-ido.el --- Access yas snippets using ido

;; Filename: yas-ido.el
;; Description: Access yas snippets using ido
;; Author: Joe Bloggs <vapniks@yahoo.com>
;; Maintainer: Joe Bloggs <vapniks@yahoo.com>
;; Copyleft (Ↄ) 2015, Joe Bloggs, all rites reversed.
;; Created: 2015-08-29 15:00:00
;; Version: 0.1
;; Last-Updated: 2015-08-29 15:00:00
;;           By: Joe Bloggs
;; URL: https://github.com/vapniks/yas-ido
;; Keywords: convenience
;; Compatibility: GNU Emacs 24.5.1
;; Package-Requires: ((yasnippet "0.8.0beta"))
;;
;; Features that might be required by this library:
;;
;; yasnippet

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
;; This package contains a couple of commands for using yasnippets with `ido'.
;;
;; Bitcoin donations gratefully accepted: 1ArFina3Mi8UDghjarGqATeBgXRDWrsmzo
;;
;;;;

;;; Commands:
;;
;; Below are complete command list:
;;
;; `yas-ido-expand-or-edit-snippet'
;;   With prefix ARG call `yas-ido-edit-snippet' else call `yas-ido-expand-snippet'.
;; `yas-ido-expand-snippet'
;;   Select snippet using ido and expand it. 
;; `yas-ido-edit-snippet'
;;   Select snippet using ido and edit it.
;;;;

;;; Customize: 
;;
;; You may need to change the value of `yas-ido-snippet-header-file' (which see)
;; This can be done by:
;;      M-x customize-variable RET yas-ido-snippet-header-file RET
;;


;;; Installation:
;;
;; Put yas-ido.el in a directory in your load-path, e.g. ~/.emacs.d/
;; You can add a directory to your load-path with the following line in ~/.emacs
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;; where ~/elisp is the directory you want to add 
;; (you don't need to do this for ~/.emacs.d - it's added by default).
;;
;; Add the following to your ~/.emacs startup file.
;;
;; (require 'yas-ido)

;;; Require
(require 'ido)

;;; Code:

;;;###autoload
(defun yas-ido-root-directory nil
  "Return the users root snippets directory."
  (if (stringp yas-snippet-dirs) yas-snippet-dirs
    (if (listp yas-snippet-dirs) (car yas-snippet-dirs)
      (error "Invalid `yas-snippet-dirs'"))))

;;;###autoload
(defun yas-ido-get-mode-dir (mode)
  "Given major-mode symbol MODE, return the directory containing snippets for that mode.
If there is no snippets directory associated with that mode return `(yas-ido-root-directory)'."
  (let* ((templates (yas--all-templates (remove-duplicates (list (gethash mode yas--tables)))))
         (full-file-names (mapcar #'(lambda (template) (yas--template-file template)) templates))
         (regex (regexp-opt (list (concat "/" (symbol-name mode) "/")))))
    (or (dolist (file-name full-file-names)
          (when (string-match regex file-name)
            (return (substring file-name 0 (match-end 0)))))
        (yas-ido-root-directory))))

;;;###autoload
(defun yas-ido-expand-snippet (location)
  "Select yasnippet using ido and expand it.
When called non-interactively expand snippet in file at LOCATION."
  (interactive (list (ido-read-file-name
		      "Snippet file: "
		      (yas-ido-get-mode-dir major-mode) nil t)))
  (let ((snippet (with-temp-buffer
		   (insert-file-contents location)
		   (goto-char (point-min))
		   (buffer-substring-no-properties
		    (re-search-forward "# --.*\n")
		    (point-max)))))
    (yas-expand-snippet snippet)))

(defcustom yas-ido-snippet-header-file nil
  "File containing header for snippet files.
This header will be inserted at the beginning of any snippets created 
with `yas-ido-edit-snippet'."
  :group 'yasnippet
  :type 'file)

;;;###autoload
(defun yas-ido-edit-snippet (location content)
  "Select snippet using ido and edit it.
When called non-interactively make/edit a snippet in file at LOCATION placing CONTENT at the end"
  (interactive (list (ido-read-file-name "Snippet file: " (yas-ido-get-mode-dir major-mode))
		     (if (and mark-active (y-or-n-p "Insert currently selected region (y/n)? "))
			 (if mark-active
			     (buffer-substring-no-properties
			      (region-beginning) (region-end))
			   nil) "")))
  (if (file-exists-p location)
      (progn
	(find-file location)
	(goto-char (point-max))
	(insert content))
    (switch-to-buffer (get-buffer-create location))
    (set-visited-file-name location)
    (insert content)
    (snippet-mode)
    (if (and (stringp yas-ido-snippet-header-file)
	     (file-exists-p yas-ido-snippet-header-file))
	(let ((snippettemplate
	       (with-temp-buffer
		 (goto-char (point-min))
		 (insert-file-contents yas-ido-snippet-header-file)
		 (goto-char (point-min))
		 (buffer-substring-no-properties
		  (re-search-forward "# --.*\n")
		  (point-max)))))
	  (goto-char (point-min))
	  (yas-expand-snippet snippettemplate)))))

;;;###autoload
(defun yas-ido-expand-or-edit-snippet (arg)
  "With prefix ARG call `yas-ido-edit-snippet' else call `yas-ido-expand-snippet'."
  (interactive "P")
  (call-interactively
   (if arg 'yas-ido-edit-snippet
     'yas-ido-expand-snippet)))

(provide 'yas-ido)

;; (magit-push)
;; (yaoddmuse-post "EmacsWiki" "yas-ido.el" (buffer-name) (buffer-string) "update")

;;; yas-ido.el ends here
