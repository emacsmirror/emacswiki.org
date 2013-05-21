;;; ediff-trees.el --- Recursively ediff two directory trees 
;;;----------------------------------------------------------------------
;; Author: Joao Cachopo <joao.cachopo@inesc-id.pt>
;; Created on: Wed May 10 17:30:49 2006
;; Keywords: ediff, comparing
;; Version: 20071126.1
;;
;; Copyright (C) 2006 Joao Cachopo

;; This program is not part of GNU Emacs

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, 675 Massachusettes Ave, Cambridge, MA
;; 02139, USA.

;;; Commentary:

;; The ediff-trees package is a simple frontend to the emacs' ediff
;; package to allow a simpler comparison of two similar directory
;; trees.

;; I wrote this package because I often need to compare two different
;; versions of the same directory tree and ediff-directories is not
;; very helpful in this case.  Specially when the directory trees to
;; compare are deep and only a few files have changed.
;; Typically, that occurs when I create a copy of some project
;; directory tree either to make some experiments myself or to send to
;; someone else that will return a modified directory tree to me
;; later.  (Yes, I heard of version control systems, and I use them
;; regularly.  Yet, for several reasons, sometimes that is not an
;; option.)

;; Later, when I want to integrate the modified directory tree with
;; the original tree, I want to see the differences to the original
;; version, so that I may decide whether to accept the changes or not.
;; This is where this package kicks in...

;; To use it, just call `ediff-trees', which will ask for two
;; directories to compare.  Usually, I give the original directory as
;; the first one and the modified directory as the second one.

;; ediff-trees recursively descends both directories, collecting the
;; pairs of files that are worth "comparing": either files that
;; changed, or that appear in one of the two directory trees but not
;; in the other.  Then, it shows the first "change" using ediff.

;; In fact, ediff-trees either uses ediff to compare a file with its
;; changed version, or simply opens a file that occurs in only one of
;; the trees.

;; The user can then navigate backward and forward in the set of
;; changes by using `ediff-trees-examine-next' and
;; `ediff-trees-examine-previous', respectively.  These functions move
;; from one change (quiting the current ediff session or killing the
;; current file buffer) to another.  Therefore, by repeatedly using
;; these functions we can go through all the changes.  I usually use
;; some global bindings for these functions.  Something like this:
;;
;;   (global-set-key (kbd "s-SPC") 'ediff-trees-examine-next)
;;   (global-set-key (kbd "S-s-SPC") 'ediff-trees-examine-previous)
;;   (global-set-key (kbd "C-s-SPC") 'ediff-trees-examine-next-regexp)
;;   (global-set-key (kbd "C-S-s-SPC") 'ediff-trees-examine-previous-regexp))

;; The `ediff-trees-examine-next-regexp' and
;; `ediff-trees-examine-previous-regexp' skip over the list of changes
;; to a file with a filename that matches a given regexp.

;; This package allows for some customization.  Please, see the
;; ediff-trees group under customize.

;; Finally, to deal with small changes in the white space I often find
;; it useful to configure ediff like this:
;;
;;   (setq ediff-diff-options "-w")
;;   (setq-default ediff-ignore-similar-regions t)

;;; Code:


(require 'ediff)

(defgroup ediff-trees nil
  "Extend ediff to allow comparing two trees recursively."
  :tag "Ediff Trees"
  :group 'ediff)


(defface ediff-trees-deleted-original-face
  '((((class color))
     (:background "Pink"))
    (t (:inverse-video t)))
  "Face for highlighting the buffer when it was deleted from the original tree."
  :group 'ediff-trees)

(defcustom ediff-trees-file-ignore-regexp
  "\\`\\(\\.?#.*\\|.*,v\\|.*~\\|CVS\\|_darcs\\)\\'"
  "A regexp matching either files or directories to be ignored
when comparing two trees.  If a directory matches the regexp,
then its contents is not scanned by `ediff-trees'."
  :type 'regexp
  :group 'ediff-trees)


(defcustom ediff-trees-sort-order-regexps nil
  "*Specifies a list of regexps that determine the order in which
files will be presented during the ediff-trees session.  Files
with filenames matching former regexps appear earlier in the
session.  If a filename matches more than one regexp, the first
one wins."
  :type '(repeat regexp)
  :group 'ediff-trees)


;;;###autoload
(defun ediff-trees (root1 root2)
  "Starts a new ediff session that recursively compares two
trees."
  (interactive
   (let ((dir-A (ediff-get-default-directory-name))
         f)
     (list (setq f (ediff-read-file-name "Directory A to compare:" dir-A nil))
	   (ediff-read-file-name "Directory B to compare:"
				 (if ediff-use-last-dir
				     ediff-last-dir-B
				   (ediff-strip-last-dir f))
				 nil))))
  (ediff-trees-internal root1 root2))


;;; Internal variables, used during an ediff-trees session
(defvar ediff-trees-current-file nil)
(defvar ediff-trees-remaining-files (list))
(defvar ediff-trees-examined-files (list))


(defun ediff-trees-internal (root1 root2)
  (let ((files-changed (ediff-trees-collect-files root1 root2)))
    (if (not (null files-changed))
        (progn
          (setq ediff-trees-remaining-files files-changed)
          (setq ediff-trees-examined-files (list))
          (ediff-trees-examine-next 1))
      (message "There are no changes between the trees!"))))

(defun ediff-trees-collect-files (root1 root2)
  (ediff-trees-sort-files
   (nconc (ediff-trees-collect-changed-files root1 root2)
          (mapcar (lambda (el) (cons el nil))
                  (ediff-trees-collect-new-files root1 root2))
          (mapcar (lambda (el) (cons nil el))
                  (ediff-trees-collect-new-files root2 root1)))))


(defun ediff-trees-sort-files (files)
  (let ((tagged-files (mapcar (lambda (pair)
                                (cons (ediff-trees-get-sort-order (or (car pair) (cdr pair)))
                                      pair))
                              files)))
    (mapcar #'cdr 
            (sort tagged-files
                  (lambda (tf1 tf2)
                    (let ((order1 (car tf1))
                          (order2 (car tf2)))
                      (or (< order1 order2)
                          (and (= order1 order2)
                               (let ((el1 (or (cadr tf1) (cddr tf1)))
                                     (el2 (or (cadr tf2) (cddr tf2))))
                                 (string< el1 el2))))))))))


(defun ediff-trees-get-sort-order (pathname)
  (let ((order 0)
        (sorting-regexps ediff-trees-sort-order-regexps))
    (while (and (not (null sorting-regexps))
                (not (string-match (pop sorting-regexps) pathname)))
      (setq order (+ order 1)))
    order))
      


(defun ediff-trees-collect-changed-files (root1 root2)
  (let ((changed (list)))
    (dolist (filename (directory-files root1))
      (unless (ediff-trees-skip-file-p filename)
        (let ((file1 (expand-file-name filename root1))
              (file2 (expand-file-name filename root2)))
          (when (file-exists-p file2)
            (if (eql (file-directory-p file1)
                     (file-directory-p file2))
                (cond ((file-directory-p file1)
                       (setq changed (nconc changed (ediff-trees-collect-changed-files file1 file2))))
                      ((not (ediff-same-file-contents file1 file2))
                       (push (cons file1 file2) changed)))
              (let ((msg (format "I cannot compare a directory, '%s', with a file.  Continue? "
                                 (if (file-directory-p file1) file1 file2))))
                (if (not (y-or-n-p msg))
                    (error "Aborting ediff-trees"))))))))
    changed))


(defun ediff-trees-collect-new-files (root1 root2)
  "Collect files from root1 that do not appear at root2."
  (let ((new-files (list)))
    (dolist (filename (directory-files root1))
      (unless (ediff-trees-skip-file-p filename)
        (let ((file1 (expand-file-name filename root1))
              (file2 (and root2 (expand-file-name filename root2))))
          (cond ((file-directory-p file1)
                 (setq new-files
                       (nconc new-files
                              (ediff-trees-collect-new-files file1
                                                             (and (stringp file2)
                                                                  (file-directory-p file2)
                                                                  file2)))))
                ((or (null file2) (not (file-exists-p file2)))
                 (push file1 new-files))))))
    new-files))

(defun ediff-trees-skip-file-p (filename)
  ;; always ignore . and ..
  (or (string= filename ".")
      (string= filename "..")
      (string-match ediff-trees-file-ignore-regexp filename)))


(defun ediff-trees-examine-next (num)
  (interactive "p")
  (if (< num 0)
    (ediff-trees-examine-previous (- num))
    (ediff-trees-examine-file
     (lambda (file) (zerop (setq num (- num 1))))
     (lambda (file) (push file ediff-trees-examined-files))
     (lambda () (pop ediff-trees-remaining-files)))))


(defun ediff-trees-examine-previous (num)
  (interactive "p")
  (if (< num 0)
    (ediff-trees-examine-next (- num))
    (ediff-trees-examine-file 
     (lambda (file) (zerop (setq num (- num 1))))
     (lambda (file) (push file ediff-trees-remaining-files))
     (lambda () (pop ediff-trees-examined-files)))))


(defun ediff-trees-examine-next-regexp (regexp)
  (interactive "sSearch for (regexp): ")
  (ediff-trees-examine-file
   (lambda (file) (string-match regexp (or (car file) (cdr file))))
   (lambda (file) (push file ediff-trees-examined-files))
   (lambda () (pop ediff-trees-remaining-files))))


(defun ediff-trees-examine-previous-regexp (regexp)
  (interactive "sSearch for (regexp): ")
  (ediff-trees-examine-file
   (lambda (file) (string-match regexp (or (car file) (cdr file))))
   (lambda (file) (push file ediff-trees-remaining-files))
   (lambda () (pop ediff-trees-examined-files))))


(defun ediff-trees-examine-file (pred save-current-file-fn get-next-file-fn)
  (when (eq (current-buffer) ediff-control-buffer)
    (ediff-really-quit nil))
  (unless (null ediff-trees-current-file)
    (funcall save-current-file-fn ediff-trees-current-file)
    (when (car ediff-trees-current-file)
      (kill-buffer (find-buffer-visiting (car ediff-trees-current-file))))
    (when (cdr ediff-trees-current-file)
      (kill-buffer (find-buffer-visiting (cdr ediff-trees-current-file))))
    (setq ediff-trees-current-file nil))
  (let ((next-file (ediff-trees-get-next-file pred save-current-file-fn get-next-file-fn)))
    (if (null next-file)
        (message "No more files.")
      (progn
        (setq ediff-trees-current-file next-file)
        (if (and (car next-file) (cdr next-file))
            (ediff-files (car next-file) (cdr next-file))
          (progn
            (delete-other-windows)
            (find-file-read-only (or (car next-file) (cdr next-file)))
            (when (null (cdr next-file))
              (let ((overlay (make-overlay 0 (point-max))))
                (overlay-put overlay 'face 'ediff-trees-deleted-original-face)))))))))


(defun ediff-trees-get-next-file (pred save-current-file-fn get-next-file-fn)
  (let ((return-value 'not-found))
    (while (eq return-value 'not-found)
      (let ((next-file (funcall get-next-file-fn)))
        (cond ((null next-file)
               (setq return-value nil))
              ((funcall pred next-file)
               (setq return-value next-file))
              (t
               (funcall save-current-file-fn next-file)))))
    return-value))


(provide 'ediff-trees)
