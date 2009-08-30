;;;----------------------------------------------------------------------
;; cm-path.el
;; Primary Author: Mike Mattie
;; Copyright (C) 2009 Mike Mattie
;; License: LGPL-v3
;;;----------------------------------------------------------------------

(require 'cm-string)

;; this function was created because file-readable-p is strangely
;; akward in that it returns t instead of the path it was given which
;; neccesitates this silly wrapper. Consider sending this upstream as
;; a patch or add-on to file-readable-p"

(defun grail-sanitize-path ( path )
  "grail-sanitize-path PATH

   sanitize a load-path reducing redundant file separators to single
   separators. The sanitized PATH is returned.

   This situation: foo/bar/baz//  has bombed (load file) for me.
  "
  (replace-regexp-in-string "/+" "/" path))

(defun file-path-if-readable ( file )
  "return the path if the file is readable, otherwise nil"
  (if (file-readable-p file)
    file))

(defun delete-trailing-path-separators ( path )
  "delete-trailing-path-separators

   Delete any trailing separators from the path, returning the modified path.
  "
  (let
    ((i (- (length path) 1)))

    (while (and (> i 0) (char-equal ?/ (elt path i)))
      (setq i (- i 1)))

    (substring path 0 (+ i 1))))

(defun make-path-relative-to ( parent child )
  (let
    ((p    (reverse (split-string parent "/" t)))
     (c    (reverse (split-string child "/" t)))
     (path nil))

    (while (not (equal (car c) (car p)))
      (setq path (cons (car c) path))
      (setq c (cdr c))
      )
    (concat (string-join "/" path) "/")))

(provide 'cm-path)
