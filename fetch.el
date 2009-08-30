;;; fetch.el --- Fetch a file from a java hierarchy

;; Copyright (C) 2006 Jason Meade

;; Author: Jason Meade <jemeade@gmail.com>
;; Adapted-by: Jonas Bernoulli <jonas@bernoulli.cc> [*]
;; Last Update: April 17, 2006
;; Version: 1.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, write to:
;; The Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;; Fetch a file from a java hierarchy. Simply highlight the class name,
;; and type M-x fetch. Fetch will walk your current package looking for the
;; class, and if it finds it will open the file in a new buffer.

;; Another library named `fetch.el' exists which does something completely different.
;; It used to be available at http://gnufans.net/~deego/emacspub/lisp-mine/fetch/.

;; [*] Added above not about other `fetch.el' and fixed header.

;;; Code:

(defconst *fetch-step* 5000)

;; Bump up the max-specpdl-size and max-lisp-eval-depth vars if they 
;; are too low. (Called interactively for debugging purposes)
(defun fetch-bump-it-up-a-notch ()
  (interactive)
  (setq max-specpdl-size (+ max-specpdl-size *fetch-step*))
  (setq max-lisp-eval-depth (+ max-lisp-eval-depth *fetch-step*))
  (message "Bumped it up a notch"))

;; Isolate and return the currently marked text.
(defun fetch-marked-text (start end)
  (let ((text (buffer-substring-no-properties start end)))
    text))

;; Fetch all the text, up to the initial public|private|protected
;; declaration, from this buffer without adornments.
(defun fetch-all-text ()
  (save-excursion
    (set-window-point (get-buffer-window (buffer-name)) 1)
    (let* ((end (re-search-forward "^.*\\(public\\|private\\|protected\\)"))
	   (text (buffer-substring-no-properties (point-min) end)))
      text)))

;; Break the current buffer into a list of lines
(defun fetch-all-lines ()
  (let ((lines (split-string (fetch-all-text) "[\r\n]+")))
    lines))

;; Extract only lines of text that match a regexp
(defun fetch-some-lines (regexp lines)
  (if (null lines)
      (quote ()))
  (let ((res '()))
    (do ((lines lines (cdr lines)))
	((null lines) res)
      (let ((line (car lines)))
	(if (string-match regexp line)
	    (setq res (append res (list line))))))))

;; Build a path upwards n directories
(defun fetch-upwards-path (n)
  (if (zerop n)
      nil
    (concat "../" (fetch-upwards-path (- n 1)))))

;; Find the document root for the working classpath
(defun fetch-document-root ()
  (let ((package (fetch-some-lines "^package" (fetch-all-lines))))
    (if (null package)
	"./"
      (let ((stack (length (split-string (car (cdr (split-string
		 (car (fetch-some-lines "^package" (fetch-all-lines)))
		 ))) "\\."))))
	(fetch-upwards-path stack)))))

;; Given a class name, produce a java sourcefile name
(defun fetch-class-to-source (class)
  (concat class ".java"))

;; Find a file in the local directory
(defun fetch-file-here (class)
  (let ((file (fetch-class-to-source class)))
    (if (directory-files "." nil (concat "^" file "$"))
	(progn 
	  (find-file-other-window (fetch-class-to-source class))
	  t)
      nil)))

;; Clean up an import statement by eliminating all but the actual path
(defun fetch-cleanup-import (path)
  (let ((str path))
    (setq str (replace-regexp-in-string "^import " "" str))
    (setq str (replace-regexp-in-string ";" "" str))
    (setq str (replace-regexp-in-string "\\." "/" str))
    (setq str (replace-regexp-in-string "[^/]*$" "" str))
    str))

;; Find a file given a class name, a document root, and a list of import tags
(defun fetch-file-there (class document-root import-list)
  (if (null import-list)
      (message (concat "Could not find source code for Class '" class "'"))
    (let* ((import (fetch-cleanup-import (car import-list)))
	   (file (fetch-class-to-source class))
	   (full-path (concat document-root import)))
      (if (and (file-directory-p full-path)
	       (directory-files full-path nil (concat "^" file "$")))
	  (find-file-other-window (concat full-path file))
	(fetch-file-there class document-root (cdr import-list))))))
  
;; Fetch a file from the classpath by name
(defun fetch (start end)
  (interactive "r")
  (let ((class (fetch-marked-text start end))
	(root (fetch-document-root))
	(import (fetch-some-lines "^import" (fetch-all-lines))))
    (if (not (fetch-file-here class))
	(fetch-file-there class root import))))

(provide 'fetch)
;;; fetch.el ends here
