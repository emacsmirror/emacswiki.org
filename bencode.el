;;; bencode.el handles serialization of integers, strings, lists, and hash-tables
;; Copyright 2007  Alex Schroeder <alex@gnu.org>

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Bencoding is a way to encode integers, strings, lists, and hash-tables
;; as strings (serialization), and bdecoding does the reverse operation.
;; It is part of the torrent metafile specification at 
;; <http://bittorrent.org/protocol.html>.

;;; Code:

(defun bencode (obj)
  "Encode an elisp object using bencode as specified for torrent metainfo files."
  (cond ((integerp obj)
	 (concat "i" (number-to-string obj) "e"))
	((stringp obj)
	 (concat (number-to-string (length obj)) ":" obj))
	((and (listp obj) (eq (car obj) 'dict))
	 (concat "d" (mapconcat (lambda (i)
				  (concat (bencode (car i))
					  (bencode (cdr i))))
				(cdr obj) "") "e"))
	((listp obj)
	 (concat "l" (mapconcat 'bencode obj "") "e"))
	(t
	 (error "Cannot encode object" obj))))

(defun bdecode (str)
  "Decode the bencoded string STR."
  (with-temp-buffer
    (save-excursion
      (insert str))
    (prog1
	(bdecode-buffer)
      (unless (eobp)
	(error "Junk at the end of string" str)))))

(defun bdecode-buffer ()
  "Decode a bencoded string in the current buffer starting at point."
  (cond ((looking-at "i\\([0-9]+\\)e")
	 (goto-char (match-end 0))
	 (string-to-number (match-string 1)))
	((looking-at "\\([0-9]+\\):")
	 (goto-char (match-end 0))
	 (let ((start (point))
	       (end (+ (point) (string-to-number (match-string 1)))))
	   (goto-char end)
	   (buffer-substring-no-properties start end)))
	((looking-at "l")
	 (goto-char (match-end 0))
	 (let (result item)
	   (while (setq item (bdecode-buffer))
	     (setq result (cons item result)))
	   (nreverse result)))
	((looking-at "d")
	 (goto-char (match-end 0))
	 (let (dict key)
	   (while (setq item (bdecode-buffer))
	     (if key
		 (setq dict (cons (cons key item) dict)
		       key nil)
	       (unless (stringp item)
		 (error "Dictionary keys have to be strings" item))
	       (setq key item)))
	   (cons 'dict (nreverse dict))))
	((looking-at "e")
	 (goto-char (match-end 0))
	 nil)
	(t
	 (error "Cannot decode object" (point)))))

(let* ((str (concat "l" "i43e"
			"4:spam"
			"l" "4:spam"
			    "4:eggs" "e"
			"d" "3:cow" "3:moo"
			    "4:spam" "4:eggs" "e"
			"d" "4:spam" "l" "1:a"
					 "1:b" "e" "e" "e"))
       (obj (with-temp-buffer
	      (save-excursion
		(insert str))
	      (bdecode-buffer))))
  (assert (equal obj
		 '(43 "spam" ("spam" "eggs")
		      (dict ("cow" . "moo") ("spam" . "eggs"))
		      (dict ("spam" "a" "b")))))
  (assert (string= (bencode obj) str)))
