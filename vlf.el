;;; vlf.el --- View Large Files

;; Copyright (C) 2006  Mathias Dahl

;; Version: 0.1.2
;; Keywords: files, helpers, utilities
;; Author: Mathias Dahl <mathias.rem0veth1s.dahl@gmail.com>
;; Maintainer: Mathias Dahl
;; URL: http://www.emacswiki.org/cgi-bin/wiki/VLF

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;; 
;; After reading the Nth post on Gnu Emacs Help about Viewing Large
;; Files in Emacs, it itched so much that I decided to make a try.  It
;; helped quite a lot when Kevin Rodgers posted a snippet on how to
;; use `insert-file-contents' to extract part of a file.  At first I
;; made a try using head and tail and that worked too, but using
;; internal Emacs commands is nicer.  Here is the code to extract data
;; using head and tail in case someone wanna try that out in the
;; future:

;; (defun vlf-extract-part-of-file (file from to)
;;   "Returns bytes in FILE from FROM to TO."
;;   (let ((size (vlf-file-size file)))
;;     (if (or (> from size)
;;             (> to size))
;;         (error "From or to is larger that the file size"))
;;     (with-temp-buffer
;;       (shell-command
;;        (format "head --bytes %d %s | tail --bytes %d"
;; 	       to file (+ (- to from) 1)) t)
;;       (buffer-substring (point-min) (point-max)))))

;;; History:
;;
;; - Wed Jan 10 00:13:45 2007
;;
;;    First version created and released into the wild.
;;
;; - Wed Jan 10 18:58:47 2007
;;
;;    0.1.2
;;
;;    Added option to use external tools (head and tail) for
;;    extracting the data from the file.
;;
;;    Refactored buffer name format code into a new function.
;;
;;    Started to fiddle with float/integer conversions.
;;

;;; Bugs
;;
;; Probably some. Feel free to fix them :)

;;; Code:

(defgroup vlf nil
  "Browse large files in Emacs"
  :prefix "vlf-"
  :group 'files)

(defcustom vlf-batch-size 1000
  "Defines how large each batch of file data is."
  :type 'integer
  :group 'vlf)

(defcustom vlf-external-extraction nil
  "How to extract the data from a file.
`nil' means to use internal extraction, using
`insert-file-contents'. `t' means to use external `head' and
`tail' tools."
  :type 'boolean
  :group 'vlf)

(defvar vlf-current-start-pos 1
  "Keeps track of file position.")

(defvar vlf-current-batch-size nil
  "Keeps track of current batch size.")

(defvar vlf-current-file nil
  "File that is currently viewed.")

(defvar vlf-current-file-size 0
  "Size of current file.")

(defvar vlf-mode-map (make-sparse-keymap)
  "Keymap for `vlf-mode'.")

(defun vlf-define-keymap ()
  "Define keymap for `vlf-mode'."
  (define-key vlf-mode-map [next] 'vlf-next)
  (define-key vlf-mode-map [prior] 'vlf-prev)
  (define-key vlf-mode-map "q" 'vlf-quit))

(define-derived-mode vlf-mode
  fundamental-mode "vlf-mode"
  "Mode to browse large files in.
See `vlf' for details."
  (vlf-define-keymap)
  (toggle-read-only 1)
  (message "vlf-mode enabled"))

(defun vlf-file-size (file)
  "Get size of FILE."
  (nth 7 (file-attributes file)))

(defun vlf-quit ()
  "Quit vlf."
  (interactive)
  (kill-buffer (current-buffer)))

(defun vlf-extract-with-head-and-tail (file from to)
  "Returns bytes in FILE from FROM to TO."
  (let ((size (vlf-file-size file)))
    (if (or (> from size)
            (> to size))
        (error "From or to is larger that the file size"))
    (with-temp-buffer
      (shell-command
       (format "head --bytes %.0f \"%s\" | tail --bytes %.0f"
	       (float to) (expand-file-name file)
               (float (+ (- to from) 1))) t)
      (buffer-substring (point-min) (point-max)))))

(defun vlf-insert-batch ()
  "Insert current batch of data."
  (let* ((beg (1- vlf-current-start-pos))
        (end (+ beg vlf-current-batch-size)))
    (if vlf-external-extraction
        (insert
         (vlf-extract-with-head-and-tail
          vlf-current-file (1+ beg) end))
      (insert-file-contents
       vlf-current-file nil
       (floor beg) (floor end)))))

(defun vlf-format-buffer-name ()
  "Return format for vlf buffer name."
  (format "%s[%.0f,%.0f(%.0f)]"
          (file-name-nondirectory vlf-current-file)
          vlf-current-start-pos
          (1- (+ vlf-current-start-pos
                 vlf-current-batch-size))
          vlf-current-file-size))

(defun vlf-next ()
  "Display the next batch of file data."
  (interactive)
  (let ((inhibit-read-only t)
        left next-start-pos
        (size (vlf-file-size vlf-current-file)))
    (setq next-start-pos (float (+ vlf-current-start-pos
                                   vlf-batch-size)))
    (if (> next-start-pos size)
        (message "End of file")
      (setq vlf-current-batch-size
            vlf-batch-size
            vlf-current-start-pos next-start-pos
            left (1+ (- size vlf-current-start-pos)))     
      (if (< left vlf-current-batch-size)
          (setq vlf-current-batch-size left))
      (erase-buffer)
      (vlf-insert-batch)
      (rename-buffer
       (vlf-format-buffer-name)))))

(defun vlf-prev ()
  "Display the previous batch of file data."
  (interactive)
  (if (= 1 vlf-current-start-pos)
      (message "At beginning of file")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (setq vlf-current-start-pos (- vlf-current-start-pos
                                     vlf-batch-size)
            vlf-current-batch-size vlf-batch-size)
      (vlf-insert-batch)
      (rename-buffer
       (vlf-format-buffer-name)))))

(defun vlf (file)
  "View a large file in Emacs FILE is the file to open.
Batches of the file data from FILE will be displayed in a
read-only buffer.  You can customize the amount of bytes to
display by customizing `vlf-batch-size'."
  (interactive "fFile to open: ")
  (setq vlf-current-file file
        vlf-current-start-pos 1
        vlf-current-file-size (vlf-file-size file)
        vlf-current-batch-size
        (1- (+ vlf-current-start-pos
               vlf-batch-size)))
  (switch-to-buffer
   (generate-new-buffer (vlf-format-buffer-name)))
  (erase-buffer)
  (vlf-insert-batch)
  (vlf-mode))

(provide 'vlf)

;;; vlf.el ends here
