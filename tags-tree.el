;;; tags-tree.el --- 

;; Copyright 2007 Ye Wenbin
;;
;; Author: wenbinye@163.com
;; Version: $Id: tags-tree.el,v 1.2 2007/02/17 06:40:33 ywb Exp ywb $
;; Keywords: 
;; X-URL: not distributed yet

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

;; 

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'tags-tree)

;;; Code:

(provide 'tags-tree)
(eval-when-compile
  (require 'cl))
(require 'etags)
(require 'tree-mode)

;;{{{  customization and variables definition
(defgroup tags-tree nil
  "Display TAGS using tree-widget"
  :group 'convenience)

(defcustom tags-tree-create-buffer-function nil
  "A function to create buffer for insert tags tree"
  :group 'tags-tree
  :type 'function)

(defcustom tags-tree-group-file-function 'tags-tree-group-file-by-name
  "A function to group files. Accept a list of files, return
a tree like:

   (\"Root name\"
    (\"Group1\"
     (\"file1\" . \"full name of file1\"))
    (\"Group2\"
     (\"subgroup\"
      (\"file2\" . \"full name of file2\"))))
"
  :group 'tags-tree
  :type 'function)

(defcustom tags-tree-group-tags-function 'tags-tree-group-tags
  "A function to group tags. Accept a list of tags info, return
a tags tree like:

  (((\"group1\")
    ((\"subgroup1\")
     (\"tag1\" . tag-info))
    (\"tag2\" . tag-info))
   ((\"group2\")
    (\"tag3\" . tag-info)))

The group name set in a list to distinct with tags name.
"
  :group 'tags-tree
  :type 'function)

(defvar tags-tree-list nil "Alist for (FILE . TREE)")

(defvar tags-tree-icons
  '(("Variables" . "other")))

(defcustom tags-tree-window-width 40
  "Windows width of tags tree buffer"
  :group 'tags-tree
  :type 'integer)

(defcustom tags-tree-window-fuction
  (lambda (trbuf buf)
    (delete-other-windows)
    (let* ((w1 (selected-window))
           (w2 (split-window w1 tags-tree-window-width t)))
      (set-window-buffer w1 trbuf)
      (set-window-buffer w2 buf)
      (other-window 1)))
  "Function to set the window buffer display"
  :group 'tags-tree
  :type 'function)

(defvar tags-tree-group-tags-expression
  (list
   (cons "\\.el$"  (append lisp-imenu-generic-expression
                           '((nil "^(\\sw+\\s-+[']?\\(\\(\\sw\\|\\s_\\)+\\)" 1))))
   (cons (regexp-opt '(".cpp" ".h" ".c"))
         `((nil ,(concat
                  "^\\s-*"
                  "\\([" "[:alpha:]" "_][" "[:alnum:]" "_:<>~]*\\)"
                  "[ \t]*("))
           (nil ,(concat
                  "^\\s-*\\<"
                  "[^()]*"
                  "[^" "[:alnum:]" "_:<>~]"
                  "\\([" "[:alpha:]" "_][" "[:alnum:]" "_:<>~]*\\)"
                  "\\([ \t\n]\\|\\\\\n\\)*(") 1)))))

(defvar tags-tree-tags-update-program "myetags")

;;}}}

(define-derived-mode tags-tree-mode tree-mode "Tags-Tree"
  "A mode to display tree of TAGS"
  (tree-widget-set-theme "imenu")
  (add-hook 'tree-mode-delete-tree-hook 'tree-mode-kill-buffer)
  (add-hook 'kill-buffer-hook 'tags-tree-kill-buffer nil t))

;;{{{  Group file
(defun tags-tree-common-string (s1 s2 &optional icase)
  (let ((i 0)
        (len (min (length s1) (length s2))))
    (if icase
        (setq s1 (downcase s1)
              s2 (downcase s2)))
    (while (and (< i len)
                (= (aref s1 i) (aref s2 i)))
      (setq i (1+ i)))
    (substring s1 0 i)))

(defun tags-tree-common-path (f1 f2 &optional icase)
  (when (and f1 f2)
    (let ((str (tags-tree-common-string f1 f2 icase)))
      (file-name-directory str))))

(defun tags-tree-parse-file (file)
  "If file contain directory, return a list (DIRECTORY FILENAME),
otherwise return the file"
  (let ((dir (file-name-directory file)))
    (if dir
        (list dir (file-name-nondirectory file))
      file)))

(defun tags-tree-merge-file (tree file &optional icase)
  (if (null tree)
      (list (file-name-directory file) (file-name-nondirectory file))
    (let ((common (tags-tree-common-path (car tree) file icase))
          re1 re2)
      (setq re1 (substring (car tree) (length common))
            re2 (substring file (length common)))
      (if (string= re1 "")
          (let ((child (cadr tree)))
            (if (listp child)
                (cons common (if (tags-tree-common-path (car child) re2 icase)
                                 (cons (tags-tree-merge-file child re2)
                                       (cddr tree))
                               (cons (tags-tree-parse-file re2)
                                     (cdr tree))))
              (append (list common (tags-tree-parse-file re2))
                      (cdr tree))))
        (cons common
              (list (tags-tree-parse-file re2)
                    (cons re1 (cdr tree))))))))

(defun tags-tree-mark-file-name (tree prefix)
  (setq prefix (concat prefix (car tree)))
  (cons (car tree)
        (mapcar (lambda (child)
                  (if (listp child)
                      (tags-tree-mark-file-name child prefix)
                    (cons child (concat prefix child))))
                (sort (cdr tree)
                      (lambda (c1 c2)
                        (if (and (listp c1) (listp c2))
                            (string< (car c1) (car c2))
                          (or (listp c1)
                              (string< c1 c2))))))))

(defun tags-tree-group-file-by-name (files)
  (setq files (sort (copy-sequence files) 'string<))
  (let (tree)
    (dolist (file (mapcar (lambda (f)
                            (concat "./" f)) files))
      (setq tree (tags-tree-merge-file tree file)))
    (setcar tree (substring (car tree) 2))
    (setq tree (tags-tree-mark-file-name tree ""))
    (if (string= (car tree) "")
        (setcar tree default-directory))
    tree))
;;}}}

;;{{{  Group tags
(defun tags-tree-group-tags-default (tags)
  (mapcar (lambda (tag)
            (cons 
             (concat (car tag)
                     (if (= (aref (car tag) 0) ?\() " ...)"))
             tag))
          tags))
 
(defun tags-tree-group-add-tags (alist key val)
  (let ((alist-val (symbol-value alist))
        old)
    (if (setq old (assoc key alist-val))
        (progn
          (setcdr old (cons val (cdr old)))
          alist-val)
      (set alist (cons (list key val) alist-val)))))

(defun tags-tree-group-tags (tags file)
  (let ((exp-list tags-tree-group-tags-expression)
        exp tag name found-pat pats group found)
    (while (and (not found) exp-list)
      (setq exp (car exp-list)
            exp-list (cdr exp-list))
      (when (string-match (car exp) file)
        (setq found t
              exp (cdr exp))
        (while tags
          (setq tag (car tags)
                tags (cdr tags)
                name (car tag)
                found-pat nil
                pats exp)
          (while (and (not found-pat) pats)
            (setq pat (car pats)
                  pats (cdr pats))
            (when (string-match (cadr pat) name)
              (tags-tree-group-add-tags 'group (car pat)
                                        (cons (match-string (nth 2 pat) name) tag))
              (setq found-pat t)))
          (if (not found-pat)
              (tags-tree-group-add-tags 'group nil (cons name tag))))))
    (if found
        (let ((other (assoc nil group))
              res)
          (append (mapcar (lambda (g)
                            (cons (list (car g))
                                  (nreverse (cdr g))))
                          (delq other group))
                  (nreverse (cdr other))))
      (tags-tree-group-tags-default tags))))
;;}}}

(defun tags-tree-list-tags (widget)
  (or (widget-get widget :args)
      (let ((file (widget-get widget :file))
            (tags-file-name (widget-get (tree-mode-widget-root widget)
                                        :tags-file))
            tags)
        (save-excursion
          (or (visit-tags-table-buffer tags-file-name)
              (signal 'file-error (list "Visiting tags table"
                                        "file does not exist"
                                        tags-file-name)))
          (goto-char (point-min))
          (when (re-search-forward (concat "\f\n" "\\(" file "\\)" ",") nil t)
            (let ((path (save-excursion (forward-line 1) (file-of-tag)))
                  (goto-func goto-tag-location-function))
              (forward-line 1)
              (while (not (or (eobp) (looking-at "\f")))
                (push (save-excursion (funcall snarf-tag-function)) tags)
                (forward-line 1))
              (setq tags (funcall tags-tree-group-tags-function (nreverse tags) path))
              (mapcar (lambda (group)
                        (tags-tree-make-nodes group path goto-func))
                      tags)))))))

(defun tags-tree-make-nodes (tags path goto-func &optional icon)
  (if (listp (car tags))
      `(tree-widget
        :node (push-button
               :tag ,(caar tags)
               :format "%[%t%]\n"
               :button-icon "bucket"
               :notify tree-mode-reflesh-parent)
        ,@(mapcar (lambda (group)
                    (tags-tree-make-nodes group path goto-func (caar tags)))
                  (cdr tags)))
    `(push-button
      :tag ,(car tags)
      :format "%[%t%]\n"
      :tag-info ,(cdr tags)
      :file-path ,path
      :button-icon ,(or (assoc-default icon tags-tree-icons)
                        "function")
      :notify tags-tree-select)))

(defun tags-tree-select (wid &rest ignore)
  (let ((buf (tag-find-file-of-tag-noselect (widget-get wid :file-path))))
    (select-window (display-buffer buf))
    (etags-goto-tag-location (widget-get wid :tag-info))))

(defun tags-tree-expand (tree)
  (or (widget-get tree :args)
      (with-current-buffer (find-file-noselect (widget-get tree :tags-file))
        (when tags-table-files
          (let ((files (funcall tags-tree-group-file-function tags-table-files)))
            (if (car files)
                (widget-put (tree-widget-node tree) :tag (car files)))
            (mapcar 'tags-tree-widget (cdr files)))))))

(defun tags-tree-widget (tree)
  (if (listp (cdr tree))
      `(tree-widget
        :node (push-button
               :tag ,(car tree)
               :format "%[%t%]\n"
               :notify tree-mode-reflesh-parent)
        ,@(mapcar 'tags-tree-widget (cdr tree)))
    `(tree-widget
      :node (push-button
             :tag ,(car tree)
             :format "%[%t%]\n"
             :button-icon "leaf"
             :file-path ,(expand-file-name (cdr tree) (file-truename default-directory))
             :notify tree-mode-reflesh-parent)
      :file ,(cdr tree)
      :dynargs tags-tree-list-tags)))

(defun tags-tree (arg)
  (interactive "P")
  (let ((oldbuf (current-buffer)))
    (visit-tags-table-buffer)
    (add-hook 'kill-buffer-hook 'tags-tree-kill-tree nil t)
    (let ((tags-file buffer-file-name)
          tree)
      (unless (setq tree (assoc-default tags-file tags-tree-list))
        (with-current-buffer
            (if (functionp tags-tree-create-buffer-function)
                (funcall tags-tree-create-buffer-function tags-file)
              (get-buffer-create "*tags-tree*"))
          (unless (eq major-mode 'tags-tree-mode)
            (tags-tree-mode))
          (setq tree (tree-mode-insert `(tree-widget
                                         :node (push-button
                                                :tag ,tags-file
                                                :format "%[%t%]\n"
                                                :notify tree-mode-reflesh-parent)
                                         :dynargs tags-tree-expand
                                         :has-children t
                                         :tags-file ,tags-file
                                         :open t)))
          (add-to-list 'tags-tree-list (cons tags-file tree))))
      (let* ((marker (widget-get tree :from))
             (buf (marker-buffer marker)))
        (unless (get-buffer-window buf)
          (switch-to-buffer buf)
          (funcall tags-tree-window-fuction buf oldbuf))
        (let ((win (get-buffer-window buf)))
          (with-selected-window win
            (unless (widget-get tree :open)
              (widget-apply-action tree))
            (setq tree (tags-tree-find-file tree (buffer-file-name oldbuf)))
            (if tree
                (goto-char (widget-get tree :from))
              (message "No assoc file found in the tree, please use M-x visit-tags-table to change TAGS file!"))
            (recenter 1)))
        (if arg
            (select-window (get-buffer-window buf)))))))

(defun tags-tree-kill-buffer ()
  (mapcar (lambda (tree)
            (setq tags-tree-list
                  (remove (assoc (widget-get tree :tags-file) tags-tree-list)
                          tags-tree-list)))
          tree-mode-list))

(defun tags-tree-kill-tree ()
  (let ((tree (assoc buffer-file-name tags-tree-list)))
    (when tree
      (setq tags-tree-list (remove tree tags-tree-list)
            tree (cdr tree))
      (with-current-buffer (tree-mode-tree-buffer tree)
        (tree-mode-delete tree)))))

(defun tags-tree-display ()
  (interactive)
  (let ((widget (widget-at (1- (line-end-position))))
        file)
    (if (setq file (widget-get widget :file-path))
        (with-selected-window (display-buffer (find-file-noselect file))
          (if (widget-get widget :tag-info)
              (etags-goto-tag-location (widget-get widget :tag-info)))))))

(defun tags-tree-find-file (tree file)
  (let ((path (tags-tree-find-file-1 tree file nil))
        children child found)
    (when path
      (setq path (cdr (nreverse path)))
      (while path
        (setq found nil)
        (unless (widget-get tree :open)
          (widget-apply-action tree))
        (setq children (widget-get tree :children))
        (while (and (not found) children)
          (setq child (car children)
                children (cdr children))
          (if (string= (widget-get (tree-widget-node child) :tag) (car path))
              (setq path (cdr path)
                    tree child
                    found t))))
      tree)))

(defun tags-tree-find-file-1 (tree file path)
  (let ((children (widget-get tree :args))
        node found)
    (if (null children)
        (if (string= file (widget-get (setq node (tree-widget-node tree)) :file-path))
            (cons (widget-get node :tag) path))
      (setq node (widget-get (tree-widget-node tree) :tag))
      (while (and (not found) children)
        (setq child (car children)
              children (cdr children))
        (setq found (tags-tree-find-file-1 child file (cons node path))))
      found)))

(defun tags-tree-update-file ()
  (interactive)
  (let ((tree (widget-get (tree-mode-button-current-line) :parent))
        file)
    (if (setq file (widget-get tree :file))
        (let ((default-directory (file-name-directory
                                  (widget-get (tree-mode-widget-root tree) :tags-file))))
          (if (zerop (call-process tags-tree-tags-update-program
                                   nil nil nil
                                   "-u" "-f" file))
              (let ((tags-revert-without-query t))
                (tree-mode-reflesh-tree tree))
            (message "Error when update file"))))))

(let ((map tags-tree-mode-map))
  (define-key map "\C-o" 'tags-tree-display)
  (define-key map "G" 'tags-tree-update-file))

;;; tags-tree.el ends here
