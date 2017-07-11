;;; dbk.el --- convert docbook to muse

;; Copyright (C) 2006 Elena Pomohaci (epomulet)

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation.

;;; Code:

(provide 'dbk)

(require 'xml)

(defvar dbk-prefix "dbk-")


(defvar dbk-para-indent "\n\n"
  "Para elements indentation (0, less than 6 spaces, more than 6 spaces)")

(defun dbk-reset-para-indent ()
  (setq dbk-para-indent "\n\n"))


(defun dbk (src dest)
  "Main entry point for dbk"
  (interactive  "fSource: \nBDestination: ")
  (let ((xml-list (xml-parse-file src)))
    (switch-to-buffer dest)
    (muse-mode)
    (dbk-parse-tree xml-list)))


(defun dbk-parse-tree (lst)
  "Parse an xml tree list"
  (mapc #'dbk-parse-node lst))

(defun dbk-parse-node (node)
  "Parse a xml tree node"
  (if (stringp node)
      (if (string-match "[^ \n\t]." node)
	  (insert node))
    (let ((fname (intern-soft (concat dbk-prefix
				      (symbol-name (xml-node-name node))))))
	(if (functionp fname) (funcall fname node)
	  (dbk-node node)))))


(defun dbk-node (node)
  "Default node function"
  (dbk-parse-tree (xml-node-children node)))

(defun dbk-get-title (node)
  (let ((tit (car (xml-get-children node 'title))))
    (insert (caddr tit) ?\n)
    (dbk-parse-tree (xml-node-children (remove tit node)))))
  

(defun dbk-chapter (node)
  "Chapter conversion function"
  (insert ?\n "#title ")
  (dbk-get-title node))


(defalias 'dbk-appendix 'dbk-chapter)

(defalias 'dbk-appendixinfo 'dbk-chapterinfo)

(defalias 'dbk-articleinfo 'dbk-chapter)

(defun dbk-sect1 (node)
  "Section 1 conversion function"
  (insert ?\n "* ")
  (dbk-get-title node))


(defun dbk-graphic (node)
  "Graphic conversion function. Image format is forced to PNG"
  (let ((name (xml-get-attribute node 'fileref)))
  (insert "\n[[img/" name ".png][" name "]]")))

(defun dbk-para (node)
  (insert dbk-para-indent)
  (dbk-node node))


(defun dbk-emphasis (node)
  (insert "*")
  (dbk-node node)
  (insert "*"))
  
(defun dbk-quote (node)
  (insert "\"")
  (dbk-node node)
  (insert "\""))
  
(defun dbk-blockquote (node)
  (setq dbk-para-indent "\n\n  ")
  (dbk-node node)
  (dbk-reset-para-indent))

(defun dbk-member (node)
  (insert "\n> ")
  (dbk-node node))

(defun dbk-bridgehead (node)
  (insert "\n* ")
  (dbk-node node))

(provide 'dbk)
;;; dbk.el ends here


