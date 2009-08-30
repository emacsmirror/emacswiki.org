;;; bibus.el --- 

;; Copyright 2006 Ye Wenbin
;;
;; Author: wenbinye@163.com
;; Version: $Id: bibus.el,v 1.2 2006/08/27 10:54:51 ywb Exp ywb $
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

;;;_* Commentary:
;; 
;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'bibus)
;; To start bibus, just M-x bibus

;;;_. Code:

(provide 'bibus)
(eval-when-compile
  (require 'cl))
(require 'mysql)
(require 'tree-widget)

(defvar bibus-database "biblio"
  "Database name of bibus")
(defvar bibus-tables  '((ref . "bibref")
                        (query . "bibquery")
                        (refkey . "bibrefKey")
                        (reflink . "bibrefLink"))
  "Name of tables in database.")
(defvar bibus-sql-process nil "Process for mysql query")
(defvar bibus-ref-header nil "References table fields")
(defvar bibus-refkey nil "Key tree. A tree-widget.")
(defvar bibus-buffer-names '((ref "*bibus-ref*" bibus-ref-mode)
                             (key "*bibus-key*" bibus-key-mode)
                             (entry "*bibus-entry*" bibus-entry-mode))
  "Buffer name and mode")

(defsubst bibus-table (table)
  (assoc-default table bibus-tables))

(defsubst bibus-buffer (name)
  (let ((bufname (cadr (assoc name bibus-buffer-names))))
    (or (get-buffer bufname)
        bufname)))

(defun bibus-ensure-buffer (name)
  "Return the buffer according to NAME. If buffer not exists, create
the buffer, and set mode assoc it.

See `bibus-buffer-names'."
  (let ((buf (bibus-buffer name)))
    (if (bufferp buf)
        buf
      (with-current-buffer (get-buffer-create buf)
        (funcall (nth 2 (assoc name bibus-buffer-names)))
        (current-buffer)))))

(defsubst bibus-clear-buffer (name)
  (with-current-buffer (bibus-ensure-buffer name)
    (let ((inhibit-read-only t))
      (erase-buffer))))

(defun bibus-ref-field (ref field)
  "Return the content of reference whose field name is FIELD."
  (let ((mysql-header bibus-ref-header))
    (if (listp field)
        (mapcar (lambda (f) (mysql-cell ref f)) field)
      (mysql-cell ref field))))

;;;_. reference display
(defvar bibus-elide-string ">"
  "The string to show when the field exceed")
(defvar bibus-formats
  '(("Identifier" 15) " " ("Author" 20) " " ("Year" 5) " " ("Title" 40))
  "Format to show the reference. Each has format as:
 (FIELD COLUMN JUSTFIED)
JUSTFIED which can be right or omit which means left.")

(defvar bibus-compiled-formats nil
  "Compiled `bibus-formats'. Each slot contain a function to return a
string for this column.")
(defvar bibus-field-column nil
  "A vector to restore position information of `bibus-formats'.
If the slot is a positive number, it indicate the index is column
start position, and its value is the index of the `bibus-formats'.
If the slot is a nagtive number, it indicate the shift to the column
start index.
If the slot is zero, it indicates that is a blank")

(defun bibus-format-string (str col frt)
  "If STR longer than col, strip the str and append the
`bibus-elide-string'. Otherwise, format STR by FRT."
  (if (> (length str) col)
      (concat (substring str 0 (- col (length bibus-elide-string)))
              bibus-elide-string)
    (format frt str)))

(defsubst bibus-header-line ()
  (let ((header (mapcar 'car bibus-ref-header)))
    (concat (make-string (if scroll-bar-mode 4 2) 32)
            (mapconcat
             (lambda (frt)
               (funcall frt header))
             bibus-compiled-formats ""))))

(defun bibus-compile-format ()
  (interactive)
  (let ((i 0) len (col 0))
    (setq bibus-field-column (make-vector
                              (dolist (frt bibus-formats i)
                                (setq i (+ i
                                           (if (stringp frt)
                                               (length frt)
                                             (cadr frt)))))
                              0))
    (setq i 0)
    (dolist (frt bibus-formats)
      (setq col (1+ col))
      (if (stringp frt)
          (progn
            (setq len (length frt))
            (aset bibus-field-column i 0))
        (setq len (cadr frt))
        (aset bibus-field-column i col))
      (dotimes (v (1- len))
        (aset bibus-field-column (+ i v 1) (- -1 v)))
      (setq i (+ i len))))
  (setq bibus-compiled-formats
        (mapcar
         (lambda (frt)
           (if (listp frt)
               (if (eq (nth 2 frt) 'right)
                   `(lambda (ref)
                      (bibus-format-string (bibus-ref-field ref ,(car frt))
                                           ,(cadr frt)
                                           (format "%%%ds" ,(cadr frt))))
                 `(lambda (ref)
                    (bibus-format-string (bibus-ref-field ref ,(car frt))
                                         ,(cadr frt)
                                         (format "%%-%ds" ,(cadr frt)))))
             `(lambda (ref) ,frt)))
         bibus-formats))
  (with-current-buffer (bibus-ensure-buffer 'ref)
    (setq header-line-format (bibus-header-line))))

;;;_. refkey display
(define-widget 'bibus-key-widget 'push-button
  "bibus key widget"
  :format "%[%t%]%d"
  :notify 'bibus-display-key-refs
  :doc ""
  :keep '(:doc))
(defvar bibus-key-alist nil)

(defsubst bibus-ref-keys ()
  "Return the keys of References."
  (cdr (mapcar
        (lambda (key)
          (widget-get key :tag))
        (widget-get (bibus-key-widget "References") :children))))

(defun bibus-key-widget (tag &optional parent)
  "Get the widget which tag is TAG. It only find children of
`bibus-refkey'."
  (let (pwid children child found)
    (setq pwid (if parent (bibus-key-widget parent) bibus-refkey)
          children (widget-get pwid :children))
    (while (and (not found) children)
      (setq child (car children)
            children (cdr children))
      (when (string= (widget-get child :tag) tag)
        (setq found t)))
    (if found child)))

(defun bibus-update-key-alist ()
  (setq bibus-key-alist
        (cdr
         (mysql-query
          (format "select key_Id, key_name from %s.%s"
                  bibus-database (bibus-table 'refkey))
          bibus-sql-process))))

(defsubst bibus-key-id (key)
  "Return the key_Id according to KEY"
  (car (rassoc (list key) bibus-key-alist)))

(defsubst bibus-key-name (keyid)
  "Return the key_name according to KEYID"
  (cadr (assoc keyid bibus-key-alist)))

;; (defsubst bibus-key-id (key)
;;           (caadr (mysql-query
;;           (format
;;            "select key_Id from %s.%s where key_name=%s"
;;            bibus-database (bibus-table 'refkey) (mysql-quote key))
;;           bibus-sql-process)))

(defun bibus-reference-keys (widget)
  "Create References widgets"
  (let ((refkid (bibus-key-id "References")))
    (mapcar
     (lambda (row)
       `(bibus-key-widget
         :tag ,(cadr row)
         :sql ,(string-to-number (car row))
         :keyid ,(car row)))
     (cdr
      (mysql-query
       (format "select key_Id, key_name from %s.%s where parent=%s"
               bibus-database (bibus-table 'refkey) refkid)
       bibus-sql-process)))))

(defun bibus-query-keys (widget)
  "Create Query widgets"
  (let ((queryid (bibus-key-id "Query")))
    (mapcar
     (lambda (row)
       `(bibus-key-widget
         :tag ,(car row)
         :sql ,(replace-regexp-in-string "\\\\n" "" (cadr row))))
     (remove-if (lambda (row)
                  (string= (car row) "*last-query*"))
                (cdr
                 (mysql-query
                  (format "select name, query from %s.%s"
                          bibus-database (bibus-table 'query))
                  bibus-sql-process))))))

;;;_. select reference from database
(defvar bibus-query-cache 40
  "Reference number for every query")
(defvar bibus-query-offset 0
  "Current query offset")
(defvar bibus-query-total 0
  "Total reference of `bibus-query-sql'")
(defvar bibus-query-sql nil
  "Current sql to fetch reference")

(defun bibus-fetch-sql (where &optional other-tables)
  "Format sql to fetch reference"
  (let ((sql (format " from %s.%s as ref"
                     bibus-database (bibus-table 'ref))))
    (and other-tables
         (setq sql (concat sql ", "
                           (mapconcat
                            (lambda (tbl)
                              (format "%s.%s as %s"
                                      bibus-database
                                      (bibus-table tbl)
                                      (symbol-name tbl)))
                            other-tables ", "))))
    (setq sql (format "%s where %s" sql where))))

(defun bibus-fetch-reference (sql)
  "Fetch reference by the sql"
  (setq sql (format "select ref.* %s limit %d offset %d"
                    sql bibus-query-cache bibus-query-offset))
  (setq bibus-query-offset (+ bibus-query-offset bibus-query-cache))
  (cdr (mysql-query sql bibus-sql-process)))

(defun bibus-selected-key-widget ()
  "Current selected"
  (let (widget)
    (with-current-buffer (bibus-ensure-buffer 'key)
      (save-excursion
        (goto-char (point-min))
        (while (and (re-search-forward "[*]$" nil t)
                    (progn
                      (widget-backward 1)
                      (setq widget (widget-at))
                      (not (eq (car widget) 'bibus-key-widget))
                      (forward-line 1))))
        (if (and widget
                 (eq (widget-type widget) 'bibus-key-widget))
            widget)))))

(defun bibus-display-key-refs (widget &rest ignore)
  (let ((prev (bibus-selected-key-widget)))
    (when prev 
      (widget-put prev :doc "")
      (widget-value-set prev (widget-value prev)))
    (widget-put widget :doc "*")
    (widget-value-set widget (widget-value widget))
    (setq bibus-entry-edit-mode nil)
    (bibus-display-key-refs-1 (widget-get widget :sql))))
    
(defun bibus-display-key-refs-1 (where)
  "Display the reference by the where_definition WHERE"
  (setq bibus-query-sql
        (if (stringp where)
            (bibus-fetch-sql where)
          (bibus-fetch-sql (format "reflink.key_Id=%d and ref.Id=reflink.ref_Id" where)
                           '(reflink))))
  (bibus-clear-buffer 'ref)
  (setq bibus-query-total
        (string-to-number
         (caadr
          (mysql-query
           (concat "select count(*)" bibus-query-sql)
           bibus-sql-process))))
  (setq bibus-query-offset 0)
  (if (> bibus-query-total 0)
      (with-current-buffer (bibus-buffer 'ref)
        (bibus-insert-ref (bibus-fetch-reference bibus-query-sql))
        (goto-char (point-min))
        (display-buffer (current-buffer)))
    (message "No match reference!")))

(defun bibus-insert-ref (refs)
  "Insert reference the current buffer"
  (if (and refs
           (= (length (car refs)) (length bibus-ref-header)))
      (let ((inhibit-read-only t))
        (dolist (ref refs)
          (insert (propertize
                   (concat
                    " "
                    (mapconcat (lambda (frt) (funcall frt ref))
                               bibus-compiled-formats ""))
                   'reference ref) "\n")))))
;;;_. window setup
(defvar bibus-window-configuration
  '(key (horizontal 0.35 (ref (vertical 0.6 (entry)))))
  "Configuration of the bibus windows. The size can be a integer of a
portion to divide the current window. It is recommended to use a
portion which may less possible to raise an error.

 (key (horizontal 0.35 (ref (vertical 0.8 (entry)))))
will set window like this:
         +-----+----------+
         | key |  ref     |
         |     |          |
         |     |          |
         |     |          |
         |     |          |
         |     +----------+
         |     |  entry   |
         +-----+----------+
              
 (key (vertical 0.8 (entry))
      (horizontal 0.35 (ref)))
will set window like this:
         +-----+-----------+
         | key | ref       |
         |     |           |
         |     |           |
         |     |           |
         |     |           |
         +-----+-----------+
         | entry           |
         +-----------------+")

(defvar bibus-current-window-configuration nil)

(defun bibus-set-window-configuration (conf)
  "Set up window by `bibus-window-configuration'"
  (let ((buf (bibus-buffer (car conf)))
        full size min splitfunc window oldwin)
    (if buf (set-window-buffer (selected-window) buf))
    (dolist (other (cdr conf))
      (when other
        (setq oldwin (selected-window))
        (if (eq (car other) 'horizontal)
            (setq splitfunc 'split-window-horizontally
                  full (window-width)
                  min window-min-width)
          (setq splitfunc 'split-window-vertically
                full (window-height)
                min window-min-height))
        (setq size (if (integerp (cadr other))
                       (cadr other)
                     (floor (* (cadr other) full))))
        (if (and (> size min)
                 (> (- full size) min))
            (progn
              (setq window (funcall splitfunc size))
              (when (nth 2 other)
                (select-window window)
                (bibus-set-window-configuration (nth 2 other)))
              (select-window oldwin))
          (error "window size is too small or too large for %S" (car conf)))))))

(defun bibus-erase-window-config ()
  (interactive)
  (setq bibus-current-window-configuration nil))

;;;_. buffer navigate
(defun bibus-switch-buffer (name)
  (let ((buf (bibus-ensure-buffer name)))
    (if (get-buffer-window buf)
        (select-window (get-buffer-window buf))
      (select-window (display-buffer buf)))))

(defun bibus-switch-to-key ()
  (interactive)
  (bibus-switch-buffer 'key))

(defun bibus-switch-to-entry ()
  (interactive)
  (bibus-switch-buffer 'entry))

(defun bibus-switch-to-ref ()
  (interactive)
  (bibus-switch-buffer 'ref))

;;;_. mode define
;;;_ , keymaps
(defvar bibus-common-map nil)
(defvar bibus-ref-mode-map nil)
(defvar bibus-key-mode-map nil)
(defvar bibus-entry-mode-map nil)

(or bibus-common-map
    (setq bibus-common-map
          (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-r") 'bibus-switch-to-ref)
            (define-key map (kbd "C-c C-k") 'bibus-switch-to-key)
            (define-key map (kbd "C-c C-e") 'bibus-switch-to-entry)
            (define-key map (kbd "C-c C-s") 'bibus-query)
            (define-key map (kbd "C-c C-n") 'bibus-name-last-query)
            map)))

(or bibus-key-mode-map
    (setq bibus-key-mode-map
          (let ((map (copy-keymap widget-keymap)))
            (set-keymap-parent map bibus-common-map)
            (define-key map "d" 'bibus-delete-key)
            (define-key map "a" 'bibus-add-key)
            (define-key map "r" 'bibus-rename-key)
            (define-key map "e" 'bibus-edit-query)
            (define-key map "A" 'bibus-add-query)
            (define-key map "R" 'bibus-name-last-query)
            (define-key map "s" 'bibus-show-key)
            map)))

(or bibus-ref-mode-map
    (setq bibus-ref-mode-map
          (let ((map (make-sparse-keymap)))
            (set-keymap-parent map bibus-common-map)
            (define-key map (kbd "C-v") 'bibus-scroll-up)
            (define-key map (kbd "M-v") 'bibus-scroll-down)
            (define-key map (kbd "M->") 'bibus-end-of-buffer)
            (define-key map "s" 'bibus-show-field-at-point)
            (define-key map "\t" 'bibus-next-field)
            (define-key map (kbd "<backtab>") 'bibus-prev-field)
            (define-key map "m" 'bibus-mark-ref-forward)
            (define-key map "u" 'bibus-unmark-ref-forward)
            (define-key map "U" 'bibus-unmark-ref-all)
            (define-key map "M" 'bibus-mark-ref-all)
            (define-key map "w" 'bibus-copy-field-at-point)
            (define-key map "t" 'bibus-set-ref-key)
            (define-key map (kbd "C-t") 'bibus-set-current-ref-key)
            (define-key map "T" 'bibus-tag-ref)
            (define-key map "d" 'bibus-delete-key-ref)
            (define-key map "D" 'bibus-delete-ref)
            (define-key map ">" 'bibus-widen-column)
            (define-key map "<" 'bibus-narrow-column)
            (define-key map "j" 'next-line)
            (define-key map "k" 'previous-line)
            (define-key map (kbd "C-c C-o") 'bibus-browse-ref-url)
            (define-key map (kbd "C-c C-f") 'bibus-open-ref-file)
            (define-key map "R" 'bibus-relocate-ref-file)
            (define-key map "S" 'bibus-sort-ref)
            (define-key map "K" 'bibus-ref-assoc-keys)
            map)))

(or bibus-entry-mode-map
    (setq bibus-entry-mode-map
          (let ((map (copy-keymap widget-keymap)))
            (set-keymap-parent map bibus-common-map)
            (define-key map "E" 'bibus-entry-edit)
            (define-key map (kbd "C-c C-c") 'bibus-entry-edit-submit)
            map)))

;;;_ , ref mode
(defvar bibus-file-field-name "Custom5"
  "The field name of file in reference table")
(defvar bibus-file-app '(("pdf" . "xpdf \"%s\""))
  "How to open file")
(defvar bibus-default-file-directory "~/papers/"
  "Default directory of reference file")

(define-derived-mode bibus-ref-mode nil "Ref"
  "Major mode for display and manage reference.
 \\{bibus-ref-mode-map}"
  (setq buffer-read-only nil)
  (erase-buffer)
  (setq buffer-read-only t)
  (setq truncate-lines t)
  (font-lock-add-keywords
   nil '(("^[*].+" 0 font-lock-warning-face prepend)))
  (unless header-line-format
    (setq header-line-format (bibus-header-line))))
;;;_  . scroll
(defun bibus-scroll-up (arg)
  "ARG is how many reference to append to this buffer"
  (interactive "P")
  (let ((col (current-column)))
    (if (> bibus-query-total bibus-query-offset)
        (save-excursion
          (or arg (setq arg bibus-query-cache))
          (let ((bibus-query-cache arg))
            (goto-char (point-max))
            (bibus-insert-ref (bibus-fetch-reference
                               bibus-query-sql)))))
    (call-interactively 'scroll-up)
    (move-to-column col)))

(defun bibus-end-of-buffer ()
  "Fetch all reference of current `bibus-query-sql' in the database."
  (interactive)
  (if (> bibus-query-total bibus-query-offset)
      (bibus-scroll-up (- bibus-query-total bibus-query-offset))
    (end-of-buffer)))

(defun bibus-scroll-down (arg)
  "Scroll down without change current column"
  (interactive "P")
  (let ((col (current-column)))
    (scroll-down arg)
    (move-to-column col)))

;;;_  . view
(defsubst bibus-current-column ()
  (let ((col (1- (current-column))))
    (if (< col 0) 0 col)))

(defun bibus-field-at-point ()
  "Field name in the `bibus-formats' according to current column"
  (let ((col (bibus-current-column))
        field)
    (when (< col (length bibus-field-column))
      (setq field (aref bibus-field-column col))
      (if (< field 0)
          (setq field (aref bibus-field-column (+ col field))))
      (when (> field 0)
        (car (nth (1- field) bibus-formats))))))

(defun bibus-show-field-at-point ()
  "Echo the field content."
  (interactive)
  (let ((field (bibus-field-at-point)))
    (when field
      (message "%s: %s" field
               (bibus-ref-field (bibus-ref-at-point)
                                field)))))

(defun bibus-copy-field-at-point ()
  "Add the content of this column to kill ring"
  (interactive)
  (let ((field (bibus-field-at-point)))
    (when field
      (setq field (bibus-ref-field (bibus-ref-at-point) field))
      (if (eq last-command 'kill-region)
          (kill-append field nil)
        (kill-new field))
      (message field))))

(defun bibus-next-field (arg)
  "Jump the next ARG fields"
  (interactive "P")
  (or arg (setq arg 1))
  (if (< arg 0)
      (bibus-prev-field (- arg))
    (let ((col (bibus-current-column))
          (len (1- (length bibus-field-column)))
          field)
      (setq field (aref bibus-field-column col))
      (if (< field 0)
          (setq col (+ col field)))
      (while (> arg 0)
        (if (< col len)
            (setq col (1+ col))
          (forward-line)
          (setq col 0))
        (if (> (aref bibus-field-column col) 0)
            (setq arg (1- arg))))
      (move-to-column (1+ col)))))

(defun bibus-prev-field (arg)
  "Jump the previous ARG fields"
  (interactive "P")
  (or arg (setq arg 1))
  (if (< arg 0)
      (bibus-next-field (- arg))
    (let ((col (bibus-current-column))
          (len (1- (length bibus-field-column)))
          field)
      (setq field (aref bibus-field-column col))
      (if (< field 0)
          (setq col (+ col field)))
      (while (> arg 0)
        (if (> col 0)
            (setq col (1- col))
          (forward-line -1)
          (setq col len))
        (if (> (aref bibus-field-column col) 0)
            (setq arg (1- arg))))
      (move-to-column (1+ col)))))

(defun bibus-widen-column (arg)
  "Widen the current column by ARG."
  (interactive "p")
  (let ((col (bibus-current-column))
        field)
    (when (< col (length bibus-field-column))
      (setq field (aref bibus-field-column col))
      (if (< field 0)
          (setq field (aref bibus-field-column (+ col field))))
      (when (> field 0)
        (setq field (nth (1- field) bibus-formats))
        (setcdr field (list (+ arg (cadr field))))
        (bibus-compile-format)
        (bibus-redisplay-ref)))))

(defun bibus-narrow-column (arg)
  "Narrow the current column by ARG."
  (interactive "p")
  (bibus-widen-column (- arg)))

(defun bibus-redisplay-ref ()
  "Redisplay the reference without query from database."
  (let ((inhibit-read-only t)
        (line (line-number-at-pos))
        (col (bibus-current-column))
        refs)
    (goto-char (point-min))
    (while (not (eobp))
      (setq refs (append refs
                         (list (bibus-ref-at-point))))
      (forward-line))
    (bibus-clear-buffer 'ref)
    (bibus-insert-ref refs)
    (goto-line line)
    (move-to-column (1+ col))))

(defun bibus-sort-ref (arg)
  "Sort the reference by current column. With prefix, sort by descend."
  (interactive "P")
  (let ((field (bibus-field-at-point))
        (inhibit-read-only t)
        (col (current-column))
        pos)
    (if (setq pos (string-match "\\s-*order by ref\." bibus-query-sql))
        (setq bibus-query-sql (substring bibus-query-sql 0 pos)))
    (setq bibus-query-sql (format "%s order by ref.%s %s"
                                  bibus-query-sql
                                  field (if arg "desc" "asc")))
    (setq bibus-query-offset 0)
    (erase-buffer)
    (bibus-insert-ref (bibus-fetch-reference bibus-query-sql))
    (goto-char (point-min))
    (move-to-column col)))

;;;_  . reference manage
(defvar bibus-mark-char ?*)
(defsubst bibus-ref-at-point ()
  (get-text-property (point) 'reference))

(defun bibus-ref-id ()
  "If mark some reference, return all reference id of marked
reference, otherwise return current reference id."
  (let (ids)
    (if (bibus-has-marked-refp)
        (bibus-do-with-marked-ref
         (lambda ()
           (setq ids
                 (append ids
                         (list (bibus-ref-field
                                (bibus-ref-at-point) "Id"))))))
      (setq ids (list (bibus-ref-field (bibus-ref-at-point) "Id"))))    
    ids))

(defun bibus-mark-ref (&optional arg)
  (interactive "P")
  (or arg (setq arg 1))
  (let ((inhibit-read-only t))
    (save-excursion
      (beginning-of-line)
      (while (and (not (eobp))
                  (> arg 0))
        (delete-char 1)
        (insert (propertize (string bibus-mark-char)
                            'reference (bibus-ref-at-point)))
        (forward-line 1)
        (setq arg (1- arg))))))

(defun bibus-mark-ref-forward (arg)
  (interactive "P")
  (bibus-mark-ref arg)
  (next-line arg))

(defun bibus-mark-ref-all ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (bibus-mark-ref (count-lines (point-min) (point-max)))))

(defun bibus-unmark-ref-forward (arg)
  (interactive "P")
  (let ((inhibit-read-only t)
        (bibus-mark-char 32))
    (bibus-mark-ref-forward arg)))

(defun bibus-unmark-ref-all ()
  (interactive)
  (let ((bibus-mark-char 32))
    (bibus-do-with-marked-ref 'bibus-mark-ref)))

(defsubst bibus-has-marked-refp ()
  (save-excursion
    (goto-char (point-min))
    (not (null (re-search-forward "^[*]" nil t)))))

(defun bibus-do-with-marked-ref (func)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^[*]" nil t)
      (funcall func))))

(defun bibus-set-current-ref-key (key)
  "Set marked or current reference association key"
  (interactive (list (completing-read
                      "Set this ref to key: "
                      (append '("Tagged") (bibus-ref-keys)))))
  (let ((keyid (bibus-key-id key))
        (refid (bibus-ref-field (bibus-ref-at-point) "Id")))
    (when (and keyid refid)
      (mysql-query
       (format "insert ignore %s.%s values (%s, %s)"
               bibus-database (bibus-table 'reflink)
               keyid refid)
       bibus-sql-process))))

(defun bibus-set-ref-key (key)
  "Set marked or current reference association key"
  (interactive (list (completing-read
                      (if (bibus-has-marked-refp)
                          "Set MARKED ref to key: "
                        "Set THIS ref to key: ")
                      (append '("Tagged") (bibus-ref-keys)))))
  (let ((keyid (bibus-key-id key))
        (refid (bibus-ref-id)))
    (when (and keyid refid)
      (mysql-query
       (format "insert ignore %s.%s values %s"
               bibus-database (bibus-table 'reflink)
               (mapconcat
                (lambda (id)
                  (format "(%s, %s)" keyid id))
                refid ","))
       bibus-sql-process))))

(defun bibus-tag-ref ()
  "Set marked or current reference with key \"Tagged\""
  (interactive)
  (if (yes-or-no-p (if (bibus-has-marked-refp)
                       "Tag MARKED ref "
                     "Tag this ref"))
      (bibus-set-ref-key "Tagged")))

(defun bibus-delete-key-ref ()
  "Delete marked or current reference from current key"
  (interactive)
  (let* ((select (bibus-selected-key-widget))
         (keytag (widget-get select :tag))
         keyid refid)
    (if (string= keytag "All")
        (message "Nothing to do in key \"All\".")
      (setq keyid (bibus-key-id keytag)
            refid (bibus-ref-id))
      (when (and keyid refid
                 (yes-or-no-p (if (bibus-has-marked-refp)
                                  "Delete MARKED references "
                                "Delete THIS reference ")))
        (mysql-query
         (format "delete from %s.%s where key_Id=%s and ref_Id in (%s)"
                 bibus-database (bibus-table 'reflink) keyid
                 (mapconcat 'identity refid ","))
         bibus-sql-process)
        (let ((inhibit-read-only t))
          (if (bibus-has-marked-refp)
              (save-excursion
                (goto-char (point-min))
                (flush-lines "^[*]"))
            (delete-region (line-beginning-position)
                           (progn (forward-line) (point)))))))))

(defun bibus-delete-ref ()
  "Delete marked or current reference from database. This only can't
preform when in key \"All\"."
  (interactive)
  (let* ((select (bibus-selected-key-widget))
         (keytag (widget-get select :tag))
         keyid refid)
    (if (not (string= keytag "All"))
        (message "Delete the reference from key \"All\".")
      (setq keyid (bibus-key-id keytag)
            refid (bibus-ref-id))
      (when (and keyid refid
                 (yes-or-no-p (if (bibus-has-marked-refp)
                                  "Really delete MARKED references from database "
                                "Really delete THIS reference from database ")))
        (setq refid (mapconcat 'identity refid ","))
        (mysql-query
         (concat 
          (format "delete from %s.%s where ref_Id in (%s)"
                  bibus-database (bibus-table 'reflink) refid)
          ";"
          (format "delete from %s.%s where Id in (%s)"
                  bibus-database (bibus-table 'ref) refid))
         bibus-sql-process)
        (let ((inhibit-read-only t))
          (if (bibus-has-marked-refp)
              (save-excursion
                (goto-char (point-min))
                (flush-lines "^[*]"))
            (delete-region (line-beginning-position)
                           (progn (forward-line) (point)))))))))

(defun bibus-browse-ref-url ()
  (interactive)
  (let ((url (bibus-ref-field (bibus-ref-at-point) "URL")))
    (browse-url url)
    (message "Browser url: %s" url)))

(defun bibus-open-ref-file ()
  "Open file according to `bibus-file-app'."
  (interactive)
  (let* ((reffile (expand-file-name
                   (bibus-ref-field (bibus-ref-at-point)
                                    bibus-file-field-name)))
         (ext (file-name-extension reffile)))
    (when (and reffile (assoc ext bibus-file-app))
      (message "Open file: %s" reffile)
      (start-process-shell-command "bibus-run" nil
                                   (format (assoc-default ext
                                                          bibus-file-app)
                                           reffile)))))

(defun bibus-update-ref (ref fields)
  "Update the reference in database according to FIELDS.
FIELDS is a list as ((field content)...).
"
  (mysql-query
   (format
    "update %s.%s set %s where Id=%s"
    bibus-database (bibus-table 'ref)
    (mapconcat (lambda (field)
                 (format "%s=%s"
                         (car field)
                         (mysql-quote (cadr field))))
               fields ", ")
    (bibus-ref-field ref "Id"))
   bibus-sql-process)
  (bibus-ref-update-line))

(defun bibus-ref-update-line ()
  "Reset 'reference property"
  (with-current-buffer (bibus-ensure-buffer 'ref)
    (let ((ref (bibus-ref-at-point))
          (inhibit-read-only t))
      (setq ref
            (cadr
             (mysql-query (format "select * from %s.%s where Id=%s"
                                  bibus-database (bibus-table 'ref) (bibus-ref-field ref "Id"))
                          bibus-sql-process)))
      (if ref
          (put-text-property (line-beginning-position) (line-end-position)
                             'reference ref)
        (error "The ref in line %d is not exists in database!" (line-number-at-pos))))))

(defun bibus-relocate-ref-file (file)
  (interactive (list (read-file-name "Set assoc file: " bibus-default-file-directory)))
  (when file
    (bibus-update-ref (bibus-ref-at-point)
                      (list (list bibus-file-field-name file)))))

(defun bibus-ref-assoc-keys ()
  (interactive)
  (let ((keys (cdr
               (mysql-query
                (format (concat "SELECT refkey.key_name FROM %s.%s as refkey, %s.%s as reflink "
                                "where reflink.key_Id=refkey.key_Id and reflink.ref_Id=%s")
                        bibus-database (bibus-table 'refkey)
                        bibus-database (bibus-table 'reflink)
                        (bibus-ref-field (bibus-ref-at-point) "Id"))
                bibus-sql-process))))
    (if keys (message "Keys: %s" (mapconcat 'car keys ", ") ""))))

;;;_ , key mode
(defvar bibus-expert-query t
  "When non-nil, write query sql directly. Otherwise, use widget form
to set up query sql(Not implement).")
(defvar bibus-minibuffer-local-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map "\t" 'bibus-complete-field)
    map))

(defun bibus-complete-field ()
  (interactive)
  (let ((system-type 'cygwin)           ; ignore case
        (comint-completion-addsuffix nil))
    (comint-dynamic-simple-complete (current-word)
                                    (mapcar 'car bibus-ref-header))
    (message nil)))                     ; we are in minibuffer

(define-derived-mode bibus-key-mode nil "Key"
  "Major mode for display and manage reference keys.
 \\{bibus-key-mode-map}"
  (toggle-truncate-lines 1)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (bibus-update-key-alist)
  (setq bibus-refkey
        (widget-create
         (append '(tree-widget :open t :tag "Bibus")
                 (mapcar
                  (lambda (row)
                    (cond ((string= (cadr row) "References")
                           `(tree-widget
                             :open t
                             :tag "References"
                             :keyid ,(car row)
                             :expander bibus-reference-keys))
                          ((string= (cadr row) "Query")
                           `(tree-widget
                             :open nil
                             :keyid ,(car row)
                             :tag "Query"
                             :expander bibus-query-keys))
                          ((string= (cadr row) "All")
                           `(bibus-key-widget
                             :tag "All"
                             :keyid ,(car row)
                             :sql "Id > 0"))
                          (t `(bibus-key-widget
                               :tag ,(cadr row)
                               :sql ,(string-to-number (car row))
                               :keyid ,(car row)))))
                  (cdr
                   (mysql-query
                    (format "select key_Id, key_name from %s.%s where parent=1"
                            bibus-database (bibus-table 'refkey))
                    bibus-sql-process))))))
  (widget-setup)
  (goto-char (point-min)))

(defun bibus-update-keys (name)
  "Redraw tree of References or Query"
  (interactive (list (completing-read "References or Query? "
                                      '("References" "Query"))))
  (when (member name '("References" "Query"))
    (with-current-buffer (bibus-ensure-buffer 'key)
      (let ((refkey (bibus-key-widget name)))
        (widget-put refkey :args nil)
        (when (widget-get refkey :open)
          (widget-apply refkey :action)
          (widget-apply refkey :action))))
    (bibus-update-key-alist)))

(defun bibus-add-key (name)
  (interactive "sNew KEY name: ")
  (let* ((refid (bibus-key-id "References"))
         (keys (mysql-col
                (mysql-query
                 (format
                  "select key_name from %s.%s where parent=%s"
                  bibus-database (bibus-table 'refkey) refid)
                 bibus-sql-process) 0)))
    (unless (or (string= name "")
                (member name keys))
      (mysql-query (format
                    "insert %s.%s (user, parent, key_name) values (\"root\", %s, \"%s\")"
                    bibus-database (bibus-table 'refkey) refid name)
                   bibus-sql-process)
      (bibus-update-keys "References")
      (bibus-display-key-refs (bibus-key-widget name "References")))))

(defun bibus-key-at-point ()
  "Return the key widget at point"
  (let (key)
    (save-excursion
      (end-of-line)
      (while (progn
               (backward-char)
               (and (null (setq key (widget-at)))
                    (> (point) (line-beginning-position))))))
    (if (and key (eq (widget-type key) 'bibus-key-widget))
        key)))

(defun bibus-show-key ()
  "Show keyid or query sql of current key"
  (interactive)
  (let* ((key (bibus-key-at-point))
         (sql (widget-get key :sql)))
    (cond ((numberp sql) (message "Key id: %d" sql))
          ((stringp sql) (message "Query sql: %s" sql)))))

(defun bibus-delete-key ()
  (interactive)
  (let ((key (bibus-key-at-point))
        parent)
    (when key
      (setq parent (widget-get (widget-get key :parent) :tag))
      (setq key (widget-get key :tag))
      (cond ((string=  parent "References")
             (when (yes-or-no-p (format "Delete KEY %s " key))
               (let ((keyid (bibus-key-id key)))
                 (when keyid
                   (mysql-query
                    (format
                     "delete from %s.%s where key_Id=%s; delete from %s.%s where key_id=%s"
                     bibus-database (bibus-table 'refkey) keyid
                     bibus-database (bibus-table 'reflink) keyid)
                    bibus-sql-process)))
               (bibus-update-keys "References")))
            ((string= parent "Query")
             (when (yes-or-no-p (format "Delete QUERY %s" key))
               (mysql-query
                (format "delete from %s.%s where name=%s"
                        bibus-database (bibus-table 'query) (mysql-quote key))
                bibus-sql-process)
               (bibus-update-keys "Query")))))))

(defun bibus-rename-key ()
  (interactive)
  (let ((key (bibus-key-at-point))
        keyname parent name)
    (when key
      (setq keyname (widget-get key :tag))
      (setq name (read-from-minibuffer "Change name to: " keyname))
      (when (not (string= name ""))
        (setq parent (widget-get (widget-get key :parent) :tag))
        (cond ((string=  parent "References")
               (let ((keyid (bibus-key-id keyname)))
                 (when keyid
                   (mysql-query
                    (format
                     "update %s.%s set key_name=%s where key_Id=%s"
                     bibus-database (bibus-table 'refkey) (mysql-quote name) keyid)
                    bibus-sql-process)
                   (bibus-update-keys "References")
                   (bibus-display-key-refs (bibus-key-widget name "References")))))
              ((string= parent "Query")
               (progn
                 (mysql-query
                  (format "update ignore %s.%s set name=%s where name=%s"
                          bibus-database (bibus-table 'query)
                          (mysql-quote name) (mysql-quote keyname))
                  bibus-sql-process)
                 (bibus-update-keys "Query")
                 (bibus-display-key-refs (bibus-key-widget name "Query")))))))))

(defun bibus-query ()
  "Query some reference."
  (interactive)
  (let (sql)
    (if bibus-expert-query
        (setq sql (read-from-minibuffer "Query: " nil bibus-minibuffer-local-map))
      )
    (when (and sql (not (string= sql "")))
      (bibus-add-query-1 "*last-query*" sql))))

(defun bibus-edit-query ()
  "Edit sql of query key at point"
  (interactive)
  (let ((key (bibus-key-at-point))
        sql parent)
    (when key
      (setq parent (widget-get (widget-get key :parent) :tag))
      (when (string= parent "Query")
        (setq sql (read-from-minibuffer "Query: " (widget-get key :sql)
                                        bibus-minibuffer-local-map))
        (widget-put key :sql sql)
        (mysql-query
         (format "update %s.%s set query=%s where name=%s"
                 bibus-database (bibus-table 'query)
                 (mysql-quote sql) (mysql-quote (widget-get key :tag)))
         bibus-sql-process)
        (bibus-display-key-refs key)))))

(defun bibus-name-last-query (name)
  "Give a name to last query"
  (interactive "sName last query to: ")
  (bibus-add-query-1
   name
   (caadr
    (mysql-query
     (format "select query from %s.%s where name='*last-query*'"
             bibus-database (bibus-table 'query))
     bibus-sql-process))))

(defun bibus-add-query ()
  "Add a new query"
  (interactive)
  (let (name sql)
    (setq name (read-from-minibuffer "New QUERY name: ")
          sql (read-from-minibuffer "Query: " nil bibus-minibuffer-local-map))
    (bibus-add-query-1 name sql)))

(defun bibus-add-query-1 (name sql)
  (mysql-query
   (format "replace %s.%s (user, name, query) values (\"root\", %s, %s)"
           bibus-database (bibus-table 'query) (mysql-quote name) (mysql-quote sql))
   bibus-sql-process)
  (if (string= name "*last-query*")
      (bibus-display-key-refs-1 sql)
    (bibus-update-keys "Query")
    (bibus-display-key-refs (bibus-key-widget name "Query"))))

;;;_ , entry mode
(defvar bibus-current-ref nil
  "Current reference to display in entry buffer")
(defvar bibus-entry-timer nil
  "Timer to update entry display")
(defvar bibus-entry-edit-mode nil
  "When non-nil, means we are edit certain reference.")
(defvar bibus-entry-header-width 15
  "Header width of field in `bibus-entry-list-fields'")
(defvar bibus-entry-not-fill-fields '("URL" "File")
  "All field when display in entry mode is filled. The field in this
list will not be filled")

(defvar bibus-entry-list-fields
  '(
    ;; I don't know why add this, sometimes emacs will lose control.
    ;; Is it possible query too frequence to mysql?
    ;; But i set mysql-finished to test whether last query is finished.
    ;; I don't know how to debug it.
    ;; ("Keys" (lambda ()
    ;;           (let ((keys (cdr
    ;;                        (mysql-query
    ;;                         (format (concat "SELECT refkey.key_name FROM %s.%s as refkey, %s.%s as reflink "
    ;;                                         "where reflink.key_Id=refkey.key_Id and reflink.ref_Id=%s")
    ;;                                 bibus-database (bibus-table 'refkey)
    ;;                                 bibus-database (bibus-table 'reflink)
    ;;                                 (bibus-ref-field bibus-current-ref "Id"))
    ;;                         bibus-sql-process))))
    ;;             (if keys (mapconcat 'car keys ", ") ""))))
    "Author"
    "Title"
    ("Journal" (lambda ()
                 (apply 'format
                        (append '("%s. Vol %s: %s")
                                (bibus-ref-field bibus-current-ref
                                                 '("Journal"
                                                   "Volume"
                                                   "Pages"))))))
    ("Pub Date" (lambda ()
                  (mapconcat 'identity
                             (bibus-ref-field bibus-current-ref
                                              '("Year"
                                                "Month")) " ")))
    "URL"
    ("File" (lambda ()
              (bibus-ref-field bibus-current-ref bibus-file-field-name)))
    "Abstract")
  "Field to display in entry buffer. Can be a field name in reference
table or a customized field with content given by a function.")

(defvar bibus-entry-edit-fields
  '("BibliographicType" "Annote" "Author" "Journal" "Month" "Note"
    "Number" "Pages" "Title" "Report_Type" "Volume" "Year" "URL"
    "Custom1" "Custom2" "Custom3" "Custom4" "Custom5" "Abstract")
  "A list of field to edit")

(defvar bibus-entry-display-intrival 0.3
  "Second to update entry buffer")

(define-derived-mode bibus-entry-mode nil "Entry"
  "Major mode for display reference entry.
 \\{bibus-entry-mode-map}"
  (make-local-variable 'bibus-current-ref)
  (if bibus-entry-timer
      (cancel-timer bibus-entry-timer))
  (setq bibus-entry-timer
        (run-with-idle-timer bibus-entry-display-intrival t 'bibus-entry-display))
  (setq truncate-lines t)
  (setq buffer-read-only t))

(defun bibus-entry-display ()
  "Display reference. This is called by `bibus-entry-timer'."
  (when (not bibus-entry-edit-mode)
    (save-excursion
      (let ((prev (buffer-local-value 'bibus-current-ref
                                      (bibus-ensure-buffer 'entry)))
            (fill-prefix (make-string (+ bibus-entry-header-width 1) 32))
            new)
        (set-buffer (bibus-ensure-buffer 'ref))
        (setq new (bibus-ref-at-point))
        (when (and new (not (eq prev new)))
          (set-buffer (bibus-buffer 'entry))
          (setq bibus-current-ref new)
          (let ((inhibit-read-only t))
            (erase-buffer)
            (dolist (field bibus-entry-list-fields)
              (let ((beg (point))
                    (frt (format "%%-%ds %%s\n"
                                 bibus-entry-header-width)))
                (insert
                 (if (stringp field)
                     (format frt (propertize field
                                             'face 'bold)
                             (bibus-ref-field new field))
                   (format frt (propertize (car field)
                                           'face 'bold)
                           (funcall (cadr field)))))
                (unless (member (if (stringp field)
                                    field
                                  (car field))
                                  bibus-entry-not-fill-fields)
                  (fill-region beg (point) nil t))))))))))

(defsubst bibus-kill-wid-variables ()
  (mapc 'kill-local-variable
        '(widget-field-new
          widget-field-list
          before-change-functions
          after-change-functions
          help-button-cache)))

(defun bibus-entry-edit ()
  (interactive)
  (setq bibus-entry-edit-mode t)
  (setq truncate-lines nil)
  (switch-to-buffer (bibus-ensure-buffer 'entry))
  (let ((ref bibus-current-ref))
    (delete-other-windows)
    (bibus-kill-wid-variables)
    (let ((inhibit-read-only t))
      (erase-buffer))
    (remove-overlays)
    (setq buffer-read-only nil)
    (let ((frt (format "%%-%ds " bibus-entry-header-width)))
      (dolist (field bibus-entry-edit-fields)
        (widget-insert (format frt field))
        (widget-create 'editable-field
                       :header field
                       :origin (bibus-ref-field ref field)
                       (bibus-ref-field ref field))
        (widget-insert "\n"))
      (widget-create 'push-button
                     :notify (lambda (&rest ignore)
                               (bibus-entry-edit-submit nil))
                     "Submit")
      (widget-create 'push-button
                     :notify (lambda (&rest ignore)
                               (bibus-entry-edit))
                     "Reset")
      (widget-create 'push-button
                     :notify (lambda (&rest ignore)
                               (bibus-entry-edit-submit t))
                     "Quit")
      (widget-setup)
      (goto-char (point-min)))))

(defun bibus-entry-edit-restore ()
  "Restore the value of current field."
  (interactive)
  (let ((wid (widget-at)))
    (when (eq (widget-type wid) 'editable-field)
      (widget-value-set (widget-get wid :origin))
      (widget-setup))))

(defun bibus-entry-edit-submit (quit)
  "With prefix quit without make change in database. Otherwise, set
the reference according to current buffer"
  (interactive "P")
  (when bibus-entry-edit-mode
    (when (or quit
              (when (yes-or-no-p "Set reference according to this buffer? ")
                (let (wid submit field)
                  (dolist (wid widget-field-list)
                    (when (setq field (widget-get wid :header))
                      (setq submit
                            (append submit (list (list field
                                                       (widget-value
                                                        wid)))))))
                  (bibus-update-ref bibus-current-ref submit)
                  t)))
      (setq bibus-entry-edit-mode nil)
      (bibus-kill-wid-variables)
      (remove-overlays)
      (setq buffer-read-only t)
      (setq bibus-current-ref nil)      ; force redisplay
      (bibus-entry-display)
      (bibus)
      (message "Finished"))))

;;;_. bibus start
(defvar bibus-initialized nil)
(defun bibus ()
  (interactive)
  (unless (mysql-check-process bibus-sql-process)
    (if (and (processp bibus-sql-process)
             (not (eq (process-status bibus-sql-process) 'exit)))
        (mysql-disconnect bibus-sql-process))
    (setq bibus-sql-process (mysql-connect)))
  (unless bibus-initialized
    (let ((refs (mysql-shell-query
                 (format "select * from %s.%s limit 1"
                         bibus-database
                         (bibus-table 'ref)))))
      (setq bibus-ref-header (mysql-table-header refs)))
    (bibus-compile-format)
    (setq bibus-initialized t))
  (dolist (buf bibus-buffer-names)
    (unless (bufferp (bibus-buffer (car buf)))
      (bibus-ensure-buffer (car buf))
      (setq bibus-current-window-configuration nil)))
  (if bibus-current-window-configuration
      (set-window-configuration bibus-current-window-configuration)
    (delete-other-windows)
    (bibus-set-window-configuration bibus-window-configuration)
    (setq bibus-current-window-configuration
          (current-window-configuration))))

(defun bibus-reconnect ()
  "If you encounter some error that can draw back, this command is to rescue."
  (interactive)
  (if (and (processp bibus-sql-process)
           (not (eq (process-status bibus-sql-process) 'exit)))
      (mysql-disconnect bibus-sql-process))
  (setq bibus-sql-process nil)
  (mapc (lambda (buf) (if (buffer-live-p (bibus-buffer (car buf)))
                          (kill-buffer (bibus-buffer (car buf)))))
        bibus-buffer-names)
  (bibus))
;;; bibus.el ends here

;;;_. LocalVariable
;;; Local Variables: ***
;;; allout-layout:(1 : -1 -1 0) ***
;;; End: ***

