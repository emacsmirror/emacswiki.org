;;; rss.el --- Create and update RSS(version 2.0)

;; Copyright (C) 2008 Free Software Foundation, Inc.
;;
;; Author: Ye Wenbin <wenbinye@gmail.com>
;; Maintainer: Ye Wenbin <wenbinye@gmail.com>
;; Created: 13 Apr 2008
;; Version: 0.01
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; This library provides basic functions to manipulate RSS file.
;; Currently support only version 2.0 RSS. Maybe other version
;; of RSS will support in future. I think these API will not
;; change:
;;  (rss-read)
;;  (rss-read-file file)
;;  (rss-write rss &optional no-newlines)
;;  (rss-write-file rss file &optional no-newlines)
;;  (make-rss &optional version)
;;  (rss-channel rss node ...)
;;  (rss-add-item rss title link description)
;;  (rss-add-item rss '(attr node ...))
;;  (rss-image rss title url link)
;;  (rss-image rss '(attr node ...))
;;  (rss-textInput rss title name link)
;;  (rss-textInput rss '(attr node ...))
;; 
;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'rss)

;;; Code:

(require 'cl)
(require 'xml)

;;{{{ xml extension
(defsubst xml-set-node-name (node name)
  "Set node name"
  (setcar node name))

(defsubst xml-set-node-attributes (node attrs)
  "Set node attributes."
  (setcar (cdr node) attrs))

(defsubst xml-set-node-children (node childrens)
  "Set node children."
  (setcdr (cdr node) childrens))

(defun xml-find-by-name (node name)
  "Find the first node the with the NAME.
Return the a list begin with the first node match the NAME. If
you want find all node with the NAME, use `xml-get-children'
instead."
  (or (consp (car node))
      (setq node (xml-node-children node)))
  (member-if (lambda (c) (eq (car c) name)) node))

(defun xml-replace-node-children (node &rest children)
  "Change children of node with the same tag name in CHILDREN.
The NODE can be a xml node or the children of the node so you can
select a offset to replace.
The replace is taken in the order as the xml node."
  (let (val)
    (or (consp (car node))
        (setq node (xml-node-children node)))
    (while (and node children)
      (setq val (assoc (caar node) children))
      (if val
          (setcar node val))
      (setq node (cdr node)
            children (delq val children)))))

(defun xml-write (xml-list &optional header add-newlines)
  "Insert the xml to current buffer."
  (insert (or header "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"))
  (dolist (node xml-list)
    (xml-write-node node 0 add-newlines)))

(defun xml-write-node (node depth &optional add-newlines)
  (if (stringp node)
      (progn (insert
              (if (get-text-property 0 'xml-no-escape node)
                  node
                (xml-escape-string node)))
             nil)
    (let ((tag (symbol-name (xml-node-name node)))
          (attrs (xml-node-attributes node))
          (children (xml-node-children node))
          (add-nl t))
      (and depth (bolp) (insert (make-string (* depth 2) 32)))
      (insert "<" tag)
      (when attrs
        (insert " "
                (mapconcat (lambda (attr)
                             (concat (symbol-name (car attr))
                                     "=\""
                                     (xml-escape-string (cdr attr))
                                     "\""))
                           attrs " ")))
      (if (or (null children) (equal children '(nil)))
          (insert " />")
        (insert ">")
        (dolist (child children)
          (and add-newlines add-nl
               (not (stringp child))
               (insert "\n"))
          (setq add-nl 
                (xml-write-node child (1+ depth) add-newlines)))
        (when add-nl
          (and add-newlines (insert "\n"))
          (and depth (insert (make-string (* depth 2) 32))))
        (insert "</" tag ">"))
      t)))

(defun xml-cleanup (xml-list)
  "Remove empty string or whitespace string node from the XML-LIST."
  (mapcar 'xml-cleanup-node xml-list))

(defun xml-cleanup-node (node)
  (apply 'list
         (xml-node-name node)
         (xml-node-attributes node)
         (let (new)
           (dolist (child (xml-node-children node))
             (if (stringp child)
                 (or (string-match "\\`[ \t\n]+\\'" child)
                     (push child new))
               (push (xml-cleanup-node child) new)))
           (nreverse new))))
;;}}}

(defvar rss-header "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
  "Header for rss output")

(defvar rss-template nil
  "Template for rss.
A valid template is a xml node with name \"channel\", for example:
  (channel nil (title nil \"Title of my website\")
           (link nil \"http://my.site.com\")
           (description nil \"blah blah\"))

See xml.el for xml node format.")

;;{{{  Fields
(defvar rss-fields
  `((channel nil
             (title nil "")
             (link nil "")
             (description nil "")
             (image)
             (language)
             (copyright)
             (managingEditor)
             (webMaster)
             (pubDate)
             (lastBuildDate)
             (category)
             (generator)
             (docs)
             (cloud)
             (skipDays)
             (skipHours)
             (ttl)
             (item nil nil)
             (textInput))
    (image nil
           (title nil "")
           (url nil "")
           (link nil "")
           (width)
           (height)
           (description))
    (item nil
          (title nil "")
          (link nil "")
          (description nil "")
          (guid)
          (author)
          (category)
          (comments)
          (enclosure)
          (pubDate)
          (source))
    (enclosure ((url . "")
                (length . "")
                (type . "")))
    (textInput nil
               (title nil "")
               (description nil "")
               (name nil "")
               (link nil "")))
  "Default valid fields and display order of the fields.")
;;}}}

(defun rss-fill-fields (fields template)
  "Fill the FIELD with the TEMPLATE.
The field can be a node "
  (let ((fields (copy-tree fields))
        (nodes (xml-node-children template))
        (attrs (xml-node-attributes template))
        children rest val name)
    (dolist (attr (xml-node-attributes fields))
      (if (setq val (assoc (car attr) attrs))
          (setcdr attr (cdr val))))
    (dolist (node (xml-node-children fields))
      (setq name (xml-node-name node)
            rest (xml-find-by-name nodes name))
      (if rest
          (progn
            (setq val rest)
            (while (eq (caar rest) name)
              (push (car rest) children)
              (setq rest (cdr rest)))
            (setcdr val rest)
            (setq nodes (delq (car val) nodes)))
        (push node children)))
    (xml-set-node-children fields (append (nreverse children) nodes))
    fields))

(defun make-rss (&optional version)
  "Create a rss lisp object."
  (rss-fill-fields (assoc 'channel rss-fields) rss-template))

(defun rss-add-item (rss &rest args)
  "Add item to rss.
Call the funtion as:
 (rss-add-item RSS title link description)
Or:
 (rss-add-item RSS
               (attrs
                (title nil TITLE)
                (link nil LINK)
                (description nil DESCRIPTION)
                ...))

RSS can be a rss lisp object or children of the rss object.
To add a batches of items to rss, for effiency, it's recommend
do as following:
 (let ((tmp rss))
   (setq tmp (rss-add-item tmp title link description))
   (setq tmp (rss-add-item tmp title link description)))"
  (or (consp (car rss))
      (setq rss (xml-node-children rss)))
  (let ((new (rss-replace-node 'item '(title link description) args))
        prev)
    (setq rss (xml-find-by-name rss 'item))
    (if (equal (car rss) '(item nil nil))
        (progn
          (setcar rss new)
          rss)
      (while (and rss (eq (xml-node-name (car rss)) 'item))
        (setq prev rss
              rss (cdr rss)))
      (when prev
        (setcdr prev (cons new rss))))))

(defalias 'rss-channel 'xml-replace-node-children
  "Change channel information.
Call like :
  (rss-channel rss node ...).")

(defun rss-replace-node (name fields args)
  (let (node)
    (if (= (length args) 1)
        (setq node (cons name (car args)))
                                  
      (setq node (apply 'list name nil
                        (mapcar (lambda (f)
                                  (list f nil (prog1
                                                  (car args)
                                                (setq args (cdr args)))))
                                fields))))
    (rss-fill-fields (assoc name rss-fields) node)))

(defsubst rss-image (rss &rest args)
  "Add image element to RSS.
Call the funtion as:
  (rss-image RSS TITLE URL LINK)
Or:
  (rss-image RSS '(attr
               (title nil TITLE)
               (url nil URL)
               (link nil LINK)
               (width nil WIDTH))"
  (xml-replace-node-children
   rss (rss-replace-node 'image '(title url link) args)))

(defsubst rss-textInput (rss &rest args)
  "Add textInput element to RSS.
Call the funtion as:
  (rss-textInput RSS TITLE NAME LINK)
Or:
  (rss-textInput RSS '(attr
               (title nil TITLE)
               (name nil NAME)
               (link nil LINK)
               (description nil DESCRIPTION))"
  (xml-replace-node-children
   rss (rss-replace-node 'textInput '(title name link) args)))

(defun rss-cleanup-node (rss)
  "Remove the empty node."
  (apply 'list
         (xml-node-name rss)
         (xml-node-attributes rss)
         (let (new)
           (dolist (child (xml-node-children rss))
             (if (consp child)
                 (if (null (cdr child))
                     ()
                   (push (rss-cleanup-node child) new))
               (push child new)))
           (nreverse new))))

(defsubst rss-write (rss &optional no-newlines)
  "Insert stringfied rss to current buffer.
ADD-NEWLINES will make the output indent more pretty."
  (xml-write 
   (list (list 'rss '((version . "2.0")) (rss-cleanup-node rss)))
   rss-header (not no-newlines)))

(defun rss-write-file (rss file &optional no-newlines)
  (with-temp-buffer
    (rss-write rss no-newlines)
    (write-region (point-min) (point-max) file)))

(defun rss-read ()
  "Read rss file into a list rss object."
  (let ((rss (xml-parse-region (point-min) (point-max))))
    (setq rss (car (xml-node-children (car (xml-cleanup rss)))))
    (rss-fill-fields (assoc 'channel rss-fields) rss)))

(defun rss-read-file (file)
  (with-temp-buffer
    (insert-file-contents file)
    (rss-read)))

(provide 'rss)
;;; rss.el ends here

;;{{{ Examples
;; Create a rss file:
;; (require 'rss nil t)
;; (let ((rss (make-rss)))
;;   (rss-channel rss
;;                '(title nil  "freshmeat.net")
;;                '(link           nil "http://freshmeat.net")
;;                '(language       nil "en")
;;                '(description    nil "the one-stop-shop for all your Linux software needs")
;;                '(rating         nil "(PICS-1.1 \"http://www.classify.org/safesurf/\" 1 r (SS~~000 1))")
;;                '(copyright      nil "Copyright 1999) Freshmeat.net")
;;                '(pubDate        nil "Thu) 23 Aug 1999 07:00:00 GMT")
;;                '(lastBuildDate  nil "Thu) 23 Aug 1999 16:20:26 GMT")
;;                '(docs           nil "http://www.blahblah.org/fm.cdf")
;;                '(managingEditor nil "scoop@freshmeat.net")
;;                '(webMaster      nil "scoop@freshmeat.net"))
;;   (rss-image rss
;;              '(nil
;;                (title nil "freshmeat.net")
;;                (url nil "http://freshmeat.net/images/fm.mini.jpg")
;;                (link nil "http://freshmeat.net")
;;                (width        nil "88")
;;                (height       nil "31")
;;                (description  nil "This is the Freshmeat image stupid")))
;;   (rss-add-item rss
;;                 '((permaLink . "http://freshmeat.net/news/1999/06/21/930003829.html")
;;                   (title nil "GTKeyboard 0.85")
;;                   (link nil "http://freshmeat.net/news/1999/06/21/930003829.html")
;;                   (description nil "blah blah")
;;                   (enclosure ((url . "http://freshmeat.net/")
;;                               (type ."application/x-bittorrent")))))
;;   (rss-add-item rss "GTKeyboard 0.86"
;;                 "http://freshmeat.net/news/1999/06/21/930003830.html"
;;                 "blah blah")
;;   (rss-textInput rss
;;                  '(nil
;;                    (title nil "quick finder")
;;                    (name nil "query")
;;                    (link nil "http://core.freshmeat.net/search.php3")
;;                    (description nil "Use the text input below to search freshmeat")))
;;   (rss-write-file rss "feed.rss"))

;; Update feed.rss
;; (let ((rss (rss-read-file "feed.rss"))
;;       (system-time-locale "C"))
;;   (rss-channel rss (list 'pubDate nil
;;                          (format-time-string "%a, %d %b %Y %H:%M:%S %Z")))
;;   (rss-add-item rss 
;;                 "GTKeyboard 0.87"
;;                 "http://freshmeat.net/news/1999/06/21/930003831.html"
;;                 "blah blah")
;;   (rss-write-file rss "feed.rss"))
;;}}}
