;;; mail-signature.el -- Add context sensitive signature
;;; Copyright (C) 1997 Kevin Davidson
;;; Copyright (C) 1985, 1986, 1992 Free Software Foundation, Inc
 
;;; Maintainer: tkld
;;; Keywords: mail

;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; A copy of the GNU General Public License can be obtained from this
;;; program's author (send electronic mail to <tkld@cogsci.ed.ac.uk>)
;;; or from the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA 02111-1307, USA.

;;; LCD Archive Entry:
;;; mail-signature|Kevin Davidson|<tkld@cogsci.ed.ac.uk>
;;; |Add context sensitive signature
;;; |$Date: 1997/04/18 09:14:51 $|$Revision: 1.7 $|~/packages/mail-signature.el

;;; Commentary:

;;; This is a reworking of the function mail-signature in sendmail.el
;;; (part of the Emacs distribution) to insert a context sensitive signature.
;;; Using regular expressions, appropriate signatures can be inserted 
;;; for different audiences.
;;; Repeated calls removes the current signature from the message and cycles
;;; through all applicable signatures.
;;; Use with something like this in .emacs:
;;; (eval-after-load "sendmail"
;;;   (progn
;;;     (load "mail-signature")
;;;     (setq mail-signature-alist 
;;;           (append '(("To" "friend" "-friendly")) mail-signature-alist))))
;;; And create a file called ~/.signature-friendly that has a
;;; signature appropriate for the user `friend' to receive.
;;; If using message-mode (included with Emacs 19.34/GNUS 5.3 or later)
;;; (setq message-signature 'mail-signature)

;;; Change log:
;; $Log: mail-signature.el,v $
; Revision 1.7  1997/04/18  09:14:51  tkld
; Add change log. Update GPL version and FSF address. Cycle through all
; possible signatures, not just toggle between two.
;

;;; Code:

(defconst mail-signature-version (substring "$Revision: 1.7 $" 11 -2)
  "$Id: mail-signature.el,v 1.7 1997/04/18 09:14:51 tkld Exp $

Report bugs to: Kevin Davidson <tkld@cogsci.ed.ac.uk>")


(defvar mail-signature-last-signature 0
 "Record index of last signature used for repeated calls of mail-signature
Buffer local")
(make-variable-buffer-local 'mail-signature-last-signature)

(defvar mail-signature-base "~/.signature"
  "*The base part of signature filename. 
Entries from mail-signature-alist will be added to this.")

(defvar mail-signature-alist
  '(("" "" ""))
  "*List of extensions to add to mail-signature-base to form name of sig file.
Format is: (HEADER REGEX EXTENSION), where REGEX is a regular expression
that should match the contents of the mail or news header HEADER.
The first to match is used. In REGEX, ^ and $ mark the beginning and end
of just the text in the header, not the whole line.")

(defun mail-signature (&optional atpoint)
  "Sign letter with context sensitive signature, based on mail-signature-alist.
Argument ATPOINT says whether to insert signature at point, or at end of
buffer."
  (interactive "P")
  (save-excursion
    (or atpoint
 (goto-char (point-max)))
    ;; First search for previous signature to delete
    ;; or delete trailing whitespace
    (if (null (search-backward "\n\n-- \n" (point-min) t))
 (progn
   (skip-chars-backward " \t\n")
   (end-of-line)))
    (or atpoint
        (delete-region (point) (point-max)))
    (insert "\n\n-- \n")
    (let ((sig-file (expand-file-name (mail-find-signature))))
      (if (file-exists-p sig-file)
   (insert-file-contents sig-file)
 (error "Signature file %s does not exist. Check mail-signature-alist."
        sig-file)))))

(defun mail-find-signature ()
  "Find an appropriate signature file."
  (let* ((elist mail-signature-alist)
  (found nil)
  (sind 0)
  (entry (car elist))
  (header (car entry))
  (regex (car (cdr entry)))
  (file (car (cdr (cdr entry)))))
    (save-excursion
      (if (>= mail-signature-last-signature (length mail-signature-alist))
   (setq mail-signature-last-signature 0))
      (while (and (not found) elist)
 (if (and (mail-position-on-field header 'soft)
   (re-search-backward (concat header ":[ \t]*\\(.*\\)$")
         (point-min) t)
   (string-match regex (buffer-substring (match-beginning 1)
             (match-end 1))))
     (if (<= sind mail-signature-last-signature)
  (setq elist (cdr elist)
        entry (car elist)
        header (car entry)
        sind (1+ sind)
        regex (car (cdr entry))
        file (car (cdr (cdr entry))))
       (setq found t))
   (setq elist (cdr elist)
  entry (car elist)
  header (car entry)
  sind (1+ sind)
  regex (car (cdr entry))
  file (car (cdr (cdr entry)))))))
    (setq mail-signature-last-signature sind)
    (concat mail-signature-base file)))

(provide 'mail-signature)

;;; mail-signature.el ends here
