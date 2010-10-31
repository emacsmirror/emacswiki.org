;;; couchdb-document-text.el --- couchdb document as text

;; Copyright (C) 2010 Changyuan Yu

;; Author: Changyuan Yu <rei.vzy@gmail.com>
;; Created: 2010-10-30
;; Keywords: file, couchdb, json, org

;; This file is *NOT* part of GNU Emacs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; A copy of the GNU General Public License can be obtained from this
;; program's author (send electronic mail to andyetitmoves@gmail.com)
;; or from the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;;; Commentary:

;; Example:

;; (require 'couchdb-document-text)
;; (find-file "/couchdb:/db1/a.txt")
;; (find-file "/couchdb:/db1/b.org")
;; (find-file "/couchdb:/db1/c.org.gpg")

;;; Code:

(require 'couchdb-document)

(defgroup couchdb-document-text nil
  "Edit couchdb document as text."
  :group 'couchdb-document)

(defcustom couchdb-document-text/file-name-regexp
  "\\.txt$\\|\\.org$\\|\\.org\\.gpg$\\|\\.txt\\.gpg$"
  "File name match regexp for new document as text file."
  :group 'couchdb-document-text
  :type 'regexp)

(defun couchdb-document-text/select (ht)
  "Select text encode & decode method."
  (let ((doc (gethash 'doc ht))
        (fn  (gethash 'filename ht)))
    (unless (or (gethash 'encode ht)
                (gethash 'decode ht))
      (when (or (and (assoc 'type doc)
                     (equal (cdr (assoc 'type doc)) "text")
                     (assoc 'text doc)
                     (stringp (cdr (assoc 'text doc))))
                (and (null doc) ;; empty file
                     (or (string-match
                          couchdb-document-text/file-name-regexp fn))))
        (puthash 'encode 'couchdb-document-text/encode ht)
        (puthash 'decode 'couchdb-document-text/decode ht)))))

(defun couchdb-document-text/encode (ht)
  "Encode text to json object.
{
  \"type\" : \"text\",
  \"text\" : (buffer-substring-no-properties (point-min) (point-max)),
  \"major-mode\" : major-mode
}"
  `((type . "text")
    (major-mode . ,major-mode)
    (text . ,(buffer-substring-no-properties (point-min)
                                             (point-max)))))

(defun couchdb-document-text/decode (ht)
  "Decode text from json object. Just return value of \"text\" in json object."
  (let* ((doc  (gethash 'doc ht))
         (text (cdr (assoc 'text doc))))
    (if (stringp text) text "")))

;; add to hook
(add-hook 'couchdb-document-open-hook 'couchdb-document-text/select)

(provide 'couchdb-document-text)

;;; couchdb-document-text.el ends here
