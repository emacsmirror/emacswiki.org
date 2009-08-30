;;; no-word.el --- use antiword program to view word documents in emacs

;; Copyright (C) 2002  by Free Software Foundation, Inc.

;; Author: Pierre Gaston <pgas@intracom.gr>
;; Maintainer: 
;; Version: 1.0
;; Keywords: Word, word processors 
;; Description: use antiword program to view word documents in emacs
;; URL: http://www.emacswiki.org/elisp/no-word.el
;; based on code by Alex Schroeder


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

;; this package requires the antiword program is in your path
;; Antiword can be found here : http://www.winfield.demon.nl/ or http://antiword.cjb.net/ 
;; it has been tested only under linux with antiword 0.33 and emacs 21.2.1
;;
;; Installation
;; Put this autoloads in ~/.emacs:
;;  (autoload 'no-word "no-word" "word to txt")
;;  (add-to-list 'auto-mode-alist '("\\.doc\\'" . no-word))
;; antiword will be run on every doc file you open
;;
;; You'll probably need to check the options M-x customize-group RET no-word RET
;; mapping have only been added for iso-8859-1 and iso-8859-7 
;; if you have the file utility installed setting no-word-check-word to true
;; allows to check if the .doc file is a M$ document


;; Alternatively, use
;; (autoload 'no-word-find-file "no-word" "open word document" t)
;; and use M-x no-word-find-file RET to open a word document
;;
;; (autoload 'no-word-gv-find-file "no-word" "view word document in gv" t)
;; and use M-x no-word-gv-find-file RET to view a word document in gv
;; gv (ghost view must be in your path)
;;
;; TODO
;; -fixing and testing :-)
;; -perhaps use a temp file rather than a pipe for ghostview


(defgroup no-word nil
  "Options controlling the behaviour of no-word mode."
  :group 'external
  :group 'wp)

(defcustom no-word-ask-coding t
  "Non nil if no word asks for a coding"
  :type 'boolean
  :initialize 'custom-initialize-default
  :group 'no-word)


(defcustom no-word-check-word nil
  "if non-nil no word uses the file utility to test if the .doc file is actually a M$ document"
  :type 'boolean
  :initialize 'custom-initialize-default
  :group 'no-word)


(defcustom no-word-rename-buffer t
  "if non-nil no word will open the X.doc in a buffer *X.doc* otherwise the buffer will be X.doc"
  :type 'boolean
  :initialize 'custom-initialize-default
  :group 'no-word)


(defcustom no-word-default-coding "iso-8859-1"
  "default coding used. A mapping to an antiword map file shoud exist. See no-word-coding-systems."
  :type 'string
  :initialize 'custom-initialize-default
  :group 'no-word)


(defcustom no-word-coding-systems '(( "greek-iso-8bit"  "8859-7.txt")
				    ("iso-8859-7"  "8859-7.txt")
				    ("iso-8859-1" "8859-1.txt"))
  "Alist mapping coding system to antiword map file. Key must be an emacs coding value the name of an antiword map file used by the -m option of antiword"
  :type '(alist :key-type  string  :value-type (group string))
  :initialize 'custom-initialize-default
  :group 'no-word )

(defcustom no-word-text-width 78
  "Width of the text outpout in characters"
  :type 'integer
  :initialize 'custom-initialize-default
  :group 'no-word)

(defcustom no-word-show-hidden nil
  "Non nil if no word asks for a coding"
  :type 'boolean
  :initialize 'custom-initialize-default
  :group 'no-word)

(defgroup no-word-ps nil
  "Options controlling the ps output."
  :group 'no-word)

(defcustom no-word-ps-papersize "a4"
  "paper size 10x14, a3, a4, a5, b4, b5, executive, folio,  legal, letter, note, quarto, statement or tabloid"
  :type 'string
  :initialize 'custom-initialize-default
  :group 'no-word-ps)

(defcustom no-word-ps-landscape nil
  "if non-nil the use landscape mode"
  :type 'boolean
  :initialize 'custom-initialize-default
  :group 'no-word-ps)

(defcustom no-word-ps-landscape nil
  "if non-nil the use landscape mode"
  :type 'boolean
  :initialize 'custom-initialize-default
  :group 'no-word-ps)

(defcustom no-word-ps-image-level 2
  "image level see antiword documentation"
  :type 'integer
  :initialize 'custom-initialize-default
  :group 'no-word-ps)




(defun no-word-command (map-file &optional file) 
  "return the command line to be used"
  (concat 
   (format "antiword -m %s -w %d "  map-file no-word-text-width)
   (if no-word-show-hidden  "-s ")
   (if file file "-")
   )
  )

(defun no-word-is-word () 
  "test if the current buffer is a word document"
  (string-match "Microsoft "
		(shell-command-to-string 
		 (concat "file " 
			 ;; windows'people like spaces in filenames
			 (replace-regexp-in-string " " "\\ " buffer-file-name t t)
			 )
		 )
		)
  )

(defun no-word (&optional file)
  "Run antiword on the entire buffer."
  (when (or (not no-word-check-word) 
	    (and no-word-check-word  (no-word-is-word))
	    )
    (let* (
	   (no-word-coding (if no-word-ask-coding
			       (completing-read
				(format "Select coding: (default %s): " no-word-default-coding)
				no-word-coding-systems
				nil t nil nil no-word-default-coding)
			     no-word-default-coding
			     ))
	   (map-file (cadr (assoc no-word-coding no-word-coding-systems)))
	   (doc-name (buffer-name))
	   (coding-system-for-read (intern no-word-coding))
	   )

      (save-window-excursion
	(shell-command-on-region 
	 (point-min) 
	 (point-max) 
	 ;;(no-word-command  map-file) 
	 (concat 
	  (format "antiword -m %s -w %d "  map-file no-word-text-width)
	  (if no-word-show-hidden  "-s ")
	  (if file (replace-regexp-in-string " " "\\ " file t t) "-")
	  )
	 "*no-word-temp-name*"
	 ) 	
	(kill-buffer (current-buffer))
	)
      (switch-to-buffer "*no-word-temp-name*")
      (rename-buffer (if no-word-rename-buffer (concat "*" doc-name "*") doc-name))
      )
    )
  )

(defun no-word-find-file (file) 
  "Interactive function used to open emacs document in an emacs buffer using antiword"
  (interactive "fNo word Find file: ")
  (no-word file)
  )

(defun no-word-gv-find-file (file)
  "Interactive function that use antiword and open ghostview"
  (interactive "fView Word file: ")
  (when (or (not no-word-check-word) 
	    (and no-word-check-word  (no-word-is-word))
	    )
    (let* (
	   (no-word-coding (if no-word-ask-coding
			       (completing-read
				(format "Select coding: (default %s): " no-word-default-coding)
				no-word-coding-systems
				nil t nil nil no-word-default-coding)
			     no-word-default-coding
			     ))
	   (map-file (cadr (assoc no-word-coding no-word-coding-systems)))
	   )
      (shell-command 
       (concat 
	(format "antiword -p %s -m %s -i %d  " no-word-ps-papersize map-file no-word-ps-image-level)
	(if no-word-show-hidden  "-s ")
	(if no-word-ps-landscape  "-L ")
	(replace-regexp-in-string " " "\\ " file t t)
	" | gv -eof  - &")
       )
      )
    )
  )


(provide 'no-word)
;;; no-word.el ends here
