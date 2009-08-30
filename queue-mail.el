;;; queue-mail.el -- Queue draft e-mail in a directory
;; Copyright (C) 1996 John Wiegley
 
;; Author: John Wiegley <johnw@borland.com>
;; Keywords: mail,queue
;; $Revision:   1.0  $
;; $Date:   31 May 1996 16:42:12  $
;; $Source: $
 
;; LCD Archive Entry:
;; queue-mail|John Wiegley|johnw@borland.com|
;; Queue draft e-mail in a directory|
;; $Date:   31 May 1996 16:42:12  $|$Revision:   1.0  $|
;; |
 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
 
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
  
;; When done editing your mail, hit M-x queue-mail instead of C-c C-c,
;; and it will be queued in a 'queue-directory'.  From there, you can
;; load it, change the mode to *mail-mode*, and send it.
 
(defvar queue-directory "~/Mail/"
  "Where to store e-mail that is in-process.  Must end with a backslash!")
 
(defun queue-mail ()
  "Queue outgoing mail in a directory before sending it."
  (interactive)
  (let ((doit (if (string= (buffer-name (current-buffer)) "*mail*")
                  t
                (y-or-n-p "Queue the current buffer as e-mail? "))))
    (if doit
        (progn
          (if (not (file-exists-p queue-directory))
              (make-directory queue-directory))
          (let ((seqf (get-next-sequence (concat queue-directory "seqf"))))
            (write-region (point-min) (point-max)
                          (concat queue-directory (number-to-string seqf))))
          (bury-buffer)))))
 
(defun get-next-sequence (file)
  "Treat the given file as a sequence file, and return the next number
in the sequence.  The purpose of the file is to maintain state across
separate Emacs executions."
  (let (seqf (buffer (get-buffer-create " *seqf*")))
    (unwind-protect
        (save-excursion
          (set-buffer buffer)
          (if (not (file-exists-p file))
              (insert "0")
            (insert-file-contents file))
          (setq seqf (1+ (string-to-number
                          (buffer-substring (point-min) (point-max)))))
          (delete-region (point-min) (point-max))
          (insert (number-to-string seqf))
          (write-file file))
      (kill-buffer buffer))
    seqf))
 
(provide 'queue-mail)
;;; queue-mail.el ends here
