;;; java-class-doc.el -- Open the documentation for a Java class
;;
;; Copyright (C) 2006 Mathias Dahl
;;
;; Version: 0.1
;; Keywords: java, documentation, api
;; Author: Mathias Dahl <mathias.rem0veth1s.dahl@gmail.com>
;; Maintainer: Mathias Dahl
;; URL: http://www.emacswiki.org/cgi-bin/wiki/JavaClassDoc
;;
;; This file is not (yet) part of GNU Emacs.
;;
;; This is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;;; Commentary:
;;
;;  This is a simple package to quickly open the Java API
;;  documentation for a specific class.  After it is installed, just do
;;  M-x java-class-doc RET.
;;
;;  It is quite ugly (but it works) and works by scanning the
;;  allclasses-noframe.html file for all Java classes and the path to
;;  their documentation page.  Then it build a cache from this, stored
;;  as an alist.  This cache can be saved to list to avoid the time
;;  consuming scan each time you start Emacs.
;;
;;; Installation
;;
;;  - This is the most important step and what needs manual work.  Get
;;    the `allclasses-noframe.html' file, either from a local
;;    installation of the Java API documentation or from the internet
;;    (http://java.sun.com/j2se/1.4.2/docs/api/allclasses-noframe.html)
;;
;;  - Evaluate this file (M-x eval-buffer RET)
;;
;;  - Execute `java-class-doc-generate-cache' (M-x
;;    java-class-doc-generate-cache RET)
;;
;;    This will generate a cache of class names / paths for the
;;    documentation.  Save this to a file using
;;    `java-class-doc-save-cache-to-file' (M-x
;;    java-class-doc-save-cache-to-file RET)
;;
;;  - Put java-class-doc.el somewhere in your load-path.
;;
;;  - Put the following in your .emacs:
;;
;;    (autoload 'java-class-doc "java-class-doc" "Open documentation for class in Java API docs" t)
;;
;;  - Try out M-x java-class-doc RET CLASSNAME RET (completion is available)
;;
;;  - Enjoy!
;;
;;; Todo
;;
;;  - Maybe add http-get support, or use wget/curl, to get the
;;  allclasses-noframe.html source directly from internet.
;;
;;  - Improve speed and stability in
;;  `java-class-doc-generate-cache'.  It is not very robust if the
;;  format of the allclasses-noframe.html file changes.
;;
;;  Any volunteers? :)
;;
;;; Bugs
;;
;;  - No known bugs.
;;
;;; History:
;;
;;  * 2006-01-10, Mathias Dahl
;;
;;    - First release
;; 
;;; Code:


(defvar java-class-doc-cache nil
  "Class - path cache for Java documentation.
An alist of class name - path pairs for finding on which page a
specific Java class is documented.")

(defun java-class-doc-generate-cache ()
  "Generate cache.

If you do not have the Java documentation locally, save the web
page
http://java.sun.com/j2se/1.4.2/docs/api/allclasses-noframe.html
to a local file and use that file when answering the
prompt.

After the scan is complete, you probably want to call
`java-class-doc-save-cache-to-file' to save the cache."
  (interactive)
  (setq java-class-doc-cache nil)
  (let ((file (read-file-name "Find your allclasses-noframe.html file: "))
        class pos path)
    (find-file file)
    (goto-char (point-min))
    (message "Scanning, please wait...")
    (while (search-forward-regexp "A HREF=\"[^\"]*\"" nil t)
      (setq path
            (buffer-substring-no-properties
             (+ (match-beginning 0) 8)
             (- (match-end 0) 1)))
      (setq class path)
      (setq pos (string-match "\\([^/]*\\)\\.html" class))
      (setq class (substring class pos (match-end 1)))
      (setq java-class-doc-cache (append java-class-doc-cache (list (cons class path)))))
    (kill-buffer (current-buffer))
    (message "%d classes added to class cache. Save it with java-class-doc-save-cache-to-file."
             (length java-class-doc-cache))))

(defvar java-class-doc-cache-file "~/.java-class-doc-cache"
  "File to store cache.")

(defun java-class-doc-save-cache-to-file ()
  "Save cache to file."
  (interactive)
  (let ((buf (get-buffer-create "*java-class-doc*")))
    (save-excursion
      (set-buffer buf)
      (erase-buffer)
      (insert
       (with-output-to-string
         (print java-class-doc-cache)))
      (write-region
       (point-min) (point-max)
       (expand-file-name java-class-doc-cache-file)))))

(defun java-class-doc-read-cache-from-file ()
  "Read cache from file."
  (interactive)
  (setq java-class-doc-cache nil)
  (let ((buf (get-buffer-create "*java-class-doc*")))
    (save-excursion
      (set-buffer buf)
      (erase-buffer)
      (insert-file-contents java-class-doc-cache-file)
      (setq java-class-doc-cache
            (car (read-from-string
                  (buffer-substring (point-min) (point-max))))))))

(defvar java-class-doc-base-url "http://java.sun.com/j2se/1.4.2/docs/api"
  "Base URL for looking up Java documentation.
Can be a local file or a web page.  For a local file, use
file:///c:\\java\\docs\\api.  Directly from sun.com, use
http://java.sun.com/j2se/1.4.2/docs/api.  The path should not end
in a slash.")

(defun java-class-doc ()
  "Open Java documentation for a specific class."
  (interactive)
  ;; If cache is empty, first try to load from file
  (if (not java-class-doc-cache)
      (java-class-doc-read-cache-from-file))
  ;; Still empty? Eeek!
  (if (not java-class-doc-cache)
      (error "Generate the java-class-doc-cache by calling `java-class-doc-generate-cache'")
    (let ((class (completing-read "Class: " java-class-doc-cache nil t))
          path)
      (setq path (cdr (assoc class java-class-doc-cache)))
      (message "Class: %s, Path: %s" class path)
      (browse-url (format "%s/%s" java-class-doc-base-url path)))))

(provide 'java-class-doc)

(provide 'java-class-doc)

;;; java-class-doc.el ends here
