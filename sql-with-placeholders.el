;;; sql-with-placeholders.el --- handle SQL statements with placeholders gently

;; Copyright (C) 2007 Andrey Balaguta

;; Author: Andrey Balaguta <andrey.balaguta@gmail.com>
;; Maintainer: Andrey Balaguta <andrey.balaguta@gmail.com>
;; Version 0.1
;; Keywords: sql

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;; This set of functions are used to process queries with
;;; placeholders like ':var'. Statement is parsed for those, then user
;;; asked for values and placeholders are replaced with values quoted
;;; with apostrophes. Not the smartest way to do it, but something.

;;; You can replace ordinary sql-send-* function with my
;;; *-with-placeholders variants:

;;; (eval-after-load "sql"
;;;  '(load-library "sql-with-placeholders"))
;;;
;;; (add-hook 'sql-mode-hook (lambda nil
;;;                            (local-set-key [(control c) (control b)] 'sql-send-buffer-with-placeholders)
;;;                            (local-set-key [(control c) (control c)] 'sql-send-paragraph-with-placeholders)
;;;                            (local-set-key [(control c) (control r)] 'sql-send-region-with-placeholders)))


(require 'sql)

(make-variable-buffer-local 'sql-with-placeholders-history)

(defun sql-send-region-with-placeholders (start end)
  (interactive)
  (let ((reg (buffer-substring-no-properties start end)))
    ;; reset match data
    (string-match "^" reg)
    (while (string-match ":\\(\\(\\sw\\|_\\)+\\)" reg (match-end 0))
      (let* ((name (upcase (match-string 1 reg)))
             (prevval (cdr (assoc name (buffer-local-value 'sql-with-placeholders-history
                                                      (current-buffer)))))
             (curval (read-string (format (if prevval
                                              "Enter value for %s [%s]: "
                                            "Enter value for %s: ")
                                          (upcase (match-string 1 reg))
                                          prevval)
                                  nil nil prevval t)))
        ;; replace with value read
        (setq reg (replace-match (concat "'" curval "'") t t reg))
        ;; save as default value for future
        (add-to-list 'sql-with-placeholders-history (cons name curval))))
    ;; feed region to sql
    (sql-send-string reg)))

(defun sql-send-buffer-with-placeholders ()
  "Send the buffer contents to the SQL process."
  (interactive)
  (sql-send-region-with-placeholders (point-min) (point-max)))

(defun sql-send-paragraph-with-placeholders ()
  "Send the current paragraph to the SQL process."
  (interactive)
  (let ((start (save-excursion
		 (backward-paragraph)
		 (point)))
	(end (save-excursion
	       (forward-paragraph)
	       (point))))
    (sql-send-region-with-placeholders start end)))


(provide 'sql-with-placeholders)
