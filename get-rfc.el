;;; get-rfc.el --- Getting and viewing RFCs

;; This file is NOT part of Emacs.

;; Copyright (C) 2002, 2003, 2011 Lawrence Mitchell <wence@gmx.li>
;; Filename: get-rfc.el
;; Version: 1.14
;; Author: Lawrence Mitchell <wence@gmx.li>
;; Created: 2002-04-16
;; Keywords: convenience RFCs

;; COPYRIGHT NOTICE

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
;; for more details. http://www.gnu.org/copyleft/gpl.html
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs. If you did not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave., Cambridge, MA 02139, USA.

;;; Commentary:
;; This is a tiny little package to alleviate the pain of having to
;; switch out of Emacs if you want to view an RFC.
;; It prompts you for a RFC number and pops up a new buffer with said RFC.
;; You can specify whether you want to find RFCs locally or on the
;; internet, in either case the RFC is still opened in Emacs.
;;
;; Note, you need a working wget type program to get a remote RFC and
;; view it in Emacs.  If you don't have one, set the variable
;; `get-rfc-no-wget' to t (in this case, a browser will be opened up
;; to view the RFC.)
;;
;; If you do view RFCs in Emacs, rfcview.el is a useful package that
;; formats them nicely, available at
;; <URL:http://www.neilvandyke.org/rfcview/rfcview.el>
;;
;; Two commands are provided:
;; `get-rfc-view-rfc' -- Prompts for an RFC number and then displays
;;                       it in a new frame.
;; `get-rfc-view-rfc-at-point' -- Displays the RFC number at point in
;;                                a new frame.
;; `get-rfc-grep-rfc-index' -- Greps for the occurrence of a string in
;;                             the file rfc-index.txt.
;;
;; To use this file, place it somewhere in your load path, and then
;; add the following to your .emacs
;; (autoload 'get-rfc-view-rfc "get-rfc" "Get and view an RFC" t nil)
;; (autoload 'get-rfc-view-rfc-at-point "get-rfc" "View the RFC at point" t nil)
;; (autoload 'get-rfc-grep-rfc-index "get-rfc" "Grep rfc-index.txt" t nil)
;; You can then bind these functions to a key.


;;; History:
;;

;;; Code:

;;;
;;; User variables
;;;

(defgroup get-rfc nil
  "Getting RFCs from within Emacs."
  :group 'convenience)

(defcustom get-rfc-rfcs-local-flag t
  "*Non-nil means RFCs are available locally.

If this variable is t you will need to set
`get-rfc-local-rfc-directory' appropriately."
  :group 'get-rfc
  :type 'boolean)

(defcustom get-rfc-remote-rfc-directory "http://www.ietf.org/rfc/"
  "*Where to find RFCs on the WWW.

This *must* end in a trailing slash."
  :group 'get-rfc
  :type 'string)

(defcustom get-rfc-remote-rfc-index
  "http://www.isi.edu/in-notes/rfc-index.txt"
  "*Where to find the file \"rfc-index.txt\" which lists all currently
available RFCS.

You probably want to change this to point to a site nearer you."
  :group 'get-rfc
  :type 'string)

(defcustom get-rfc-wget-program "wget"
  "*The wget program `get-rfc' should use to fetch an RFC from the WWW."
  :group 'get-rfc
  :type 'string)

(defcustom get-rfc-no-wget nil
  "Set this to non-nil if you don't have a working wget.

If this variable is non-nil, getting a remote RFC will call your
favourite browser (via `browse-url')."
  :group 'get-rfc
  :type 'boolean)

(defcustom get-rfc-wget-output-flag "-O"
  "*Flag to pass to `get-rfc-wget-program' to output a downloaded file
to a specified filename."
  :group 'get-rfc
  :type 'string)
  
(defcustom get-rfc-local-rfc-directory "/usr/local/rfcs/"
  "*Directory in which RFCs are available locally.

This *must* end in a trailing slash."
  :group 'get-rfc
  :type 'string)

(defcustom get-rfc-view-rfc-mode nil
  "*Mode for viewing RFCs.

Set this to the name of your favourite mode for viewing RFCs."
  :group 'get-rfc
  :type 'symbol)

(defcustom get-rfc-open-in-new-frame t
  "*Whether or not get-rfc should open a new frame to view an RFC."
  :group 'get-rfc
  :type 'boolean)

(defcustom get-rfc-save-new-rfcs-locally t
  "*Whether or not get-rfc should save newly downloaded RFCs.

Files are saved in `get-rfc-local-rfc-directory' (q.v.)."
  :group 'get-rfc
  :type 'boolean)

;;;
;;; Internal variables
;;;

(defvar get-rfc-rfc-index "rfc-index.txt"
  "*The file name of the index of RFCs.")

(defvar get-rfc-grep-command "grep"
  "*The grep command to use.")

(defvar get-rfc-grep-flags "-n -e"
  "*Flags to pass to grep.")

(defconst get-rfc-version
  "1.14"
  "get-rfc.el's version number.")

;;;
;;; Internal functions
;;;

(defun get-rfc (rfc &optional fullpath)
  "Get RFC from `get-rfc-remote-rfc-directory'.

If FULLPATH is non-nil, then assume that RFC is an absolute location.
Return the file it was saved in, so we can do
\(find-file (get-rfc \"foo\"))."
  (let ((rfc-full (concat (if (not fullpath)
                              get-rfc-remote-rfc-directory)
                          rfc))
	(tmp-file (make-temp-file "get-rfc")))
    (if get-rfc-no-wget
        (browse-url rfc-full)
      (call-process get-rfc-wget-program nil nil nil
                    rfc-full (concat get-rfc-wget-output-flag tmp-file))
      (when get-rfc-save-new-rfcs-locally
        (let ((file (concat get-rfc-local-rfc-directory rfc)))
          (copy-file tmp-file file)
          (setq tmp-file file)))
      tmp-file)))

;;;
;;; User functions
;;;

(defun get-rfc-version (&optional arg)
  "Print Get RFCs version number in the minibuffer.

If optional ARG is non-nil, insert in current buffer."
  (interactive "*P")
  (if arg
      (insert "\n" get-rfc-version "\n")
    (message get-rfc-version)))

;;;###autoload
(defun get-rfc-view-rfc (number)
  "View RFC NUMBER.

You can specify whether RFCs are available locally by setting
`get-rfc-rfcs-local-flag' to t.  If you do so you should also set
`get-rfc-local-rfc-directory' to point to the relevant directory.
You may also specify where on the web to find RFCs by setting
`get-rfc-remote-rfc-directory' appropriately."
  (interactive "sWhich RFC number: ")
  (let* ((rfc (concat "rfc"number".txt"))
	 (rfc-abs (concat (if get-rfc-rfcs-local-flag
			      get-rfc-local-rfc-directory
			    get-rfc-remote-rfc-directory)
			  rfc))
         (find-file-command (if get-rfc-open-in-new-frame
                                'find-file-other-frame
                              'find-file)))
    (if get-rfc-rfcs-local-flag
	(if (file-exists-p rfc-abs)
	    (funcall find-file-command rfc-abs)
	  (funcall find-file-command (get-rfc rfc)))
      (funcall find-file-command (get-rfc rfc))))
  (if get-rfc-view-rfc-mode
      (funcall get-rfc-view-rfc-mode)))

;;;###autoload
(defun get-rfc-view-rfc-at-point ()
  "View the RFC whose number is at point."
  (interactive)
  (condition-case err ; in case there is no word at point
      (let ((rfc (thing-at-point 'word)))
        (and (string-match "[^0-9]+" rfc)
             (setq rfc (replace-match "" nil t rfc)))
	(if (string= "" rfc)
	    (message "There's no RFC here!")
	(get-rfc-view-rfc rfc)))
    (wrong-type-argument
     (message "There's no RFC here!"))))

;;;###autoload
(defun get-rfc-grep-rfc-index (string)
  "Grep for STRING in rfc-index.txt."
  (interactive "sSearch for: ")
  (let ((grep-args (concat get-rfc-grep-command
                           " -i " get-rfc-grep-flags " \""
                           string "\" "
                           (if get-rfc-rfcs-local-flag
                               (concat get-rfc-local-rfc-directory
                                       get-rfc-rfc-index)
                             (get-rfc
                              get-rfc-remote-rfc-directory t)))))
  (grep grep-args)))

(provide 'get-rfc)

;;; get-rfc.el ends here
