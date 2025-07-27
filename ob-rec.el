;;; ob-rec.el --- org-babel functions for recutils evaluation

;; Copyright (C) 2011-2019 Free Software Foundation

;; Author: Jose E. Marchesi
;; Keywords: literate programming, reproducible research
;; Homepage: http://orgmode.org
;; Version: 7.7

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;;; Commentary:

;; Org-Babel support for evaluating recsel queries and substituing the
;; contained template.  See http://www.gnu.org/software/recutils/

;;; Code:
(require 'ob)

(defvar org-babel-default-header-args:rec
  '((:exports . "results")))

(defun org-babel-execute:rec (body params)
  "Execute a block containing a recsel query.
This function is called by `org-babel-execute-src-block'."
  (let* ((in-file ((lambda (el)
		     (or el
			 (error
                          "rec code block requires :data header argument")))
		   (cdr (assoc :data params))))
         (result-params (cdr (assq :result-params params)))
	 (cmdline (cdr (assoc :cmdline params)))
	 (rec-type (cdr (assoc :type params)))
	 (fields (cdr (assoc :fields params)))
         (join (cdr (assoc :join params)))
         (sort (cdr (assoc :sort params)))
         (groupby (cdr (assoc :groupby params)))
	 (cmd (concat "recsel"
		      (when rec-type (concat " -t " rec-type " "))
		      " " (expand-file-name in-file)
		      (when (> (length (org-babel-trim body)) 0)
                        (concat " -e " "\""
                                (replace-regexp-in-string "\"" "\\\\\"" body)
                                "\""))
                      (when join (concat " -j " join " "))
                      (when sort (concat " -S " sort " "))
                      (when groupby (concat " -G " groupby " "))
		      (when fields (concat " -p " fields " "))))
         (do-raw (or (member "scalar" result-params)
                     (member "html" result-params)
                     (member "code" result-params)
                     (member "verbatim" result-params)
                     (equal (point-min) (point-max)))))
    (unless do-raw
      ;; Get the csv representation, that will be used by
      ;; org-table-convert-region below.
      (setq cmd (concat cmd " | rec2csv")))
    (with-temp-buffer
      (shell-command cmd (current-buffer))
      (if do-raw
          (buffer-string)
	(org-table-convert-region (point-min) (point-max) '(4))
        (let ((table (org-table-to-lisp)))
          ;; The first row always contains the table header.
          (cons (car table) (cons 'hline (cdr table))))))))

(provide 'ob-rec)

;; ob-rec.el ends here
