;;; file-props.el --- Add file properties to your files
;;
;; Copyright (C) 2006, 2007 Mathias Dahl
;;
;; Version: 0.1.3
;; Keywords: search, convenience, files
;; Author: Mathias Dahl <mathias.rem0veth1s.dahl@gmail.com>

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; file-props.el provides a way to "tag" or "label" your files.
;; Actually, the functionality is generic and supports adding any type
;; of property to your files.  Right now "tags" and a "comment" can be
;; added.
;;
;; After having added this meta data you can use it to find files in
;; different ways.  Provided is a command to mark all files having a
;; certain tag in a dired buffer.
;;
;;; Installation:
;;
;; Place this file in your `load-path'.
;;
;; Put this in your .emacs file:
;;
;;   (require 'file-props)
;;
;; If you want to activate display of comment and tags for Dired, put
;; this in your .emacs as well:
;;
;;   (file-props-dired-activate-display)
;;
;; If you want to disable the display temporarily, do M-x
;; file-props-dired-deactivate-display
;;
;; To setup convenient Dired keybindings, put this:
;;
;;  (file-props-dired-setup-keybindings)
;;
;;; Usage:
;;
;; - Adding tags:
;;
;; In dired, mark a couple of files and type M-x
;; file-props-dired-add-tags RET.
;;
;; There are three versions of tag input, see option
;; `file-props-read-tag-multi-method' for more information.
;;
;; - Finding files:
;;
;; Type M-x file-props-find-tags-dired RET.
;;
;; Enter a tag and type RET.
;;
;; You will be presented with a dired buffer containing the files
;; having the tag you searched for.
;;
;; - Marking files:
;;
;; You can use the commad `file-props-dired-mark-from-tag' in a Dired
;; buffer to mark files that have a certain file tag.  It is
;; equivalent to how some of the other `%-commands' in dired works.
;;
;; - Edit tags and comments:
;;
;; Mark some file in Dired, either manually or using
;; `file-props-dired-mark-from-tag', then execute `file-props-edit'.
;; This will open up a new buffer where you can edit tags can comments
;; easily.
;;
;; - Other uses:
;;
;; Currently, the only other command is `file-props-dired-add-comment'
;; which will add a comment property to the file.  The idea is that
;; while tags are used to categorize or label your files, a comment is
;; more specific and can act as the description of the file.
;;
;; As explained above, tags and comments are just examples of meta
;; data you might want to add to your files, other types of data
;; should be easy to add if you want.  Look at
;; `file-props-dired-add-tags' and `file-props-dired-add-comment' for
;; examples.
;;
;;
;;; Wish-list:
;;
;; - When searching for files with tags the user should be able to
;;   specify multiple tags, not just one, when further tags are
;;   specified (with completion) only those tags are offered which
;;   have an intersection with the previous ones, (see del.icio.us)
;;
;; - Renaming and deleting tags.
;;
;;
;;; History:
;;
;;
;; Version 0.1, 2006-06-27
;;
;; First release.
;;
;; Version 0.1.1, 2006-06-28
;;
;; Removed warning when loading properties.  It wasn't very
;; useful.
;;
;; Removed question about current directory from
;; `file-props-find-tags-dired'.  I don't think this was needed
;; either.
;;
;; Replaced `read-string' with `completing-read' when reading tags to
;; search for.  I know Drew will like this because it should enabled
;; `icicles' to assimilate this functionality... :)
;;
;; Added command `file-props-dired-mark-from-tag'.
;;
;; Added command `file-props-dired-edit'.
;;
;; Fixed problems with whitespace when splitting tags string entered
;; by the user.  Two new functions was added, `file-props-trim-spaces'
;; and `file-props-split-and-trim'.
;;
;; Added a new way to enter multiple tags, using completion and made
;; this the default.  The option
;; `file-props-read-tags-comma-separated' was added if the user wants
;; to use the old method, using a comma separated string.
;;
;; Version 0.1.2, 2006-11-02
;;
;; Add `file-props-add-tags'. Change
;; `file-props-add-tags-to-current-file', `file-props-dired-add-tags',
;; `file-props-save-edits'.
;;
;; Version 0.1.3, 2007-07-20
;;
;; Add function `file-props-dired-display-info' and customize option
;; `file-props-dired-display-info-format'. Add advice for
;; `dired-next-line' and `dired-previous-line'.
;;
;; Add commands `file-props-dired-activate-display',
;; `file-props-dired-deactivate-display' and
;; `file-props-dired-setup-keybindings'.
;;
;; Now file properties are loaded when this file is loaded.
;;
;;; Code:


(require 'widget)
(require 'crm)
(require 'format-spec)

(eval-when-compile
  (require 'wid-edit))

(defgroup file-props nil
  "File properties lets you add different kinds of properties or
meta-data to files and have these properties saved to a central
data file.  This information can, for example, be used to mark
file having a certain meta-data in Dired."
  :group 'Convenience)


(defcustom file-props-data-file "~/.emacs.d/file-props"
  "File in which the file properties are saved."
  :type 'file
  :group 'file-props)


(defvar file-props-list nil
  "List containing the file properties.")


(defvar file-props-tag-history nil
  "Keeps tag history when doing `completing-read'.")


(defvar file-props-widget-list nil
  "List to keep track of meta data in edit buffer.")


(defun file-props-add-property (file property value)
  "For FILE, set PROPERTY to VALUE.
If the property does not exist, it will be created.  If it
exists, the value will be overwritten."
  (unless (assoc file file-props-list)
    (setq file-props-list (append file-props-list (list (list file nil)))))
  (setcdr (assoc file file-props-list)
          (plist-put (cdr (assoc file file-props-list)) property value)))


(defun file-props-save-properties ()
  "Save file properties.
Save file properties to file `file-props-data-file'."
  (with-temp-file (expand-file-name file-props-data-file)
    (prin1 file-props-list (current-buffer))))


(defun file-props-load-properties ()
  "Load all file properties.
Load all file properties from file `file-props-data-file'.  If
the files does not exist, no harm is done; it will be created
when file properties are added to files."
  (let ((file (expand-file-name file-props-data-file))
        buf)
    (when (file-exists-p file)
      (setq buf (find-file-noselect
                 file))
      (setq file-props-list (read buf))
      (kill-buffer buf))))


(defcustom file-props-read-tag-multi-method 'comma
  "Control how to read multiple tags."
  :type '(choice :tag "How to read tags"
                 (const :tag "With completion, using RET between each tag" ret-method)
                 (const :tag "Completing read multiple in one minibuffer call" crm-method)
                 (const :tag "Comma separated string, no completion" comma))
  :group 'file-props)

(defun file-props-dired-add-tags ()
  "Add file tags to current or marked files."
  (interactive)
  (let ((tags (file-props-read-tag-multi)))
    (mapc
     (lambda (x)
       (file-props-add-tags x tags))
     (dired-get-marked-files))))

(defun file-props-add-tags-to-current-file ()
  "Add file tags to currently open file."
  (interactive)
  (let ((file (buffer-file-name)))
    (if file
        (file-props-add-tags file (file-props-read-tag-multi))
      (message "This buffer has no associated file"))))


(defun file-props-add-tags (file tags)
  "Add file tags to currently file.
Add to FILE TAGS.  TAGS is a list of strings."
  (let ((new (file-props-get-tags file)))
    (mapc
     (lambda (x)
       (add-to-list 'new x))
     tags)
    (file-props-add-property file 'tags new)
    (when file-props-list
      (file-props-save-properties))))


(defun file-props-dired-add-comment ()
  "Add file comment to current or marked files."
  (interactive)
  (let ((comment (read-string "Enter comment: ")))
    (mapc
     (lambda (x)
       (file-props-add-property x 'comment comment))
     (dired-get-marked-files)))
    (when file-props-list
      (file-props-save-properties)))


(defun file-props-list-all-tags ()
  "Return all unique tags for all files."
  (unless file-props-list
    (file-props-load-properties))
  (let (all-tags)
    (mapc
     (lambda (x)
       (let ((tags (plist-get (cdr x) 'tags)))
         (mapc
          (lambda (y)
            (unless (member y all-tags)
              (setq all-tags (append all-tags (list y)))))
          tags)))
     file-props-list)
    all-tags))


(defun file-props-get-property (file property)
  "Return from FILE property PROPERTY's value."
  (plist-get (cdr (assoc file file-props-list)) property))


(defun file-props-get-tags (file)
  "Return list of tags for FILE."
  (file-props-get-property file 'tags))


(defun file-props-get-comment (file)
  "Return comment property for FILE."
  (file-props-get-property file 'comment))


(defun file-props-find-files-from-tag (tag)
  "Return a list of all files having file tag TAG."
  (let (files)
    (mapc
     (lambda (x)
       (when (member tag (plist-get (cdr x) 'tags))
         (setq files (append files (list (car x))))))
     file-props-list)
    files))

(defun file-props-read-tag-multi ()
  "Read multiple tags with completion."
  (let (tags tag)
    (cond ((eq file-props-read-tag-multi-method 'ret-method)
           (while (not (string= "" (setq tag (file-props-read-tag
                                              "Input one or more tags, \
typing RET in between. An empty value ends input: "))))
             (setq tags (append tags (list tag))))
           tags)
          ((eq file-props-read-tag-multi-method 'crm-method)
           (completing-read-multiple "Input one or more tags, \
separating them with a comma. Completion is available: "
                                     (file-props-list-all-tags) nil nil))
          ((eq file-props-read-tag-multi-method 'comma)
           (file-props-split-and-trim
            (read-string "Input one or more tags. \
Separate multiple tags by a comma: ") ",")))))


(defun file-props-read-tag (prompt)
  "Display PROMPT and read tag, completing from available tags."
  (completing-read
   prompt (file-props-list-all-tags) nil nil nil
   'file-props-tag-history))

(defun file-props-find-tags-dired ()
  "Search for file tag TAG to find files and list them in dired.
It generates a result like `find-dired' does."
  (interactive)
  (let* ((tag (file-props-read-tag "Tag to search for: "))
         (files (file-props-find-files-from-tag tag)))
    (if files
        (dired (cons default-directory files))
      (message "No files with tag `%s'" tag))))


(defun file-props-dired-mark-from-tag ()
  "Mark all files having a certain file tag.
In Dired, find all files that have a certain file tag and mark
them if they exist in the current directory."
  (interactive)
  (let* ((tag (file-props-read-tag "Tag to search for: "))
         (files (file-props-find-files-from-tag tag))
         (count 0))
    (when files
      (save-excursion
        (mapc
         (lambda (x)
           (goto-char (point-min))
           (when (and (string= (file-name-directory x)
                               (expand-file-name default-directory))
                      (search-forward-regexp
                       (format "%s$" (file-name-nondirectory x))
                       nil t))
             (setq count (1+ count))
             (dired-mark 1)))
         files)))
    (message "%d files marked" count)))


(defun file-props-dired-edit ()
  "Edit comment and tags of current or marked files.
Edit comment and tags for all marked files in an easy-to-use
form."
  (interactive)
  (setq file-props-widget-list nil)
  ;; Setup buffer.
  (let ((files (dired-get-marked-files)))
    (switch-to-buffer "*File Props Edit*")
    (kill-all-local-variables)
    (make-local-variable 'widget-example-repeat)
    (let ((inhibit-read-only t))
      (erase-buffer))
    (remove-overlays)
    ;; Some help for the user.
    (widget-insert
"\nEdit comments and tags for each file.  Separate multiple tags
with a comma.  Move forward between fields using TAB or RET.
Move to the previous field using backtab (S-TAB).  Save by
activating the Save button at the bottom of the form or cancel
the opration by activating the Cancel button.\n\n")
    ;; Here comes all file names and a comment and tag field for each
    ;; file.
    (mapc
     (lambda (file)
       (let (comment-widget tag-widget)
         (widget-insert file)
         (widget-insert "\n\nComment: ")
         (setq comment-widget
               (widget-create 'editable-field
                              :size 40
                              :format "%v "
                              :value (or (file-props-get-comment file) "")))
         (widget-insert "\nTags:    ")
         (setq tag-widget
               (widget-create 'editable-field
                              :size 40
                              :format "%v "
                              :value (or (mapconcat
                                          (lambda (tag)
                                            tag)
                                          (file-props-get-tags file)
                                          ",") "")))
         ;; Save information in all widgets so that we can use it when
         ;; the user saves the form.
         (setq file-props-widget-list
               (append file-props-widget-list
                       (list (list file comment-widget tag-widget))))
         (widget-insert "\n\n")))
     files))
  ;; Footer with Save and Cancel button.
  (widget-insert "\n")
  (widget-create 'push-button
                 :notify
                 (lambda (&rest ignore)
                   (file-props-save-edits)
                   (bury-buffer)
                   (message "Done."))
                 "Save")
  (widget-insert " ")
  (widget-create 'push-button
                 :notify
                 (lambda (&rest ignore)
                   (bury-buffer)
                   (message "Operation canceled."))
                 "Cancel")
  (widget-insert "\n")
  (use-local-map widget-keymap)
  (widget-setup)
  ;; Jump to the first widget.
  (widget-forward 1))

(defun file-props-save-edits ()
  "Save information found in `file-props-widget-list'.
Use the information in `file-props-widget-list' to save comments
and tags for their respective file.  Internal function used by
`file-props-dired-edit'."
  (mapc
   (lambda (x)
     (let ((file (car x))
           (comment (widget-value (cadr x)))
           (tags (widget-value (caddr x))))
       (file-props-add-property file 'comment comment)
       (file-props-add-tags file (file-props-split-and-trim tags ","))))
   file-props-widget-list)
  (file-props-save-properties))

(defun file-props-trim-spaces (str)
  "Strip STR of any leading (if BEFOREP) and/or trailing (if AFTERP) space."
  (string-match "\\`\\s-*\\(.*?\\)\\s-*\n?\\'" str)
  (match-string 1 str))


(defun file-props-split-and-trim (str split-str)
  "Call `split-string' and trim leading and trailing spaces.
Split string STR using SPLIT-STR and trim leading and trailing
spaces from the resulting list items by calling
`file-props-trim-spaces'."
  (mapcar
   (lambda (x)
     (file-props-trim-spaces x))
   (split-string str split-str)))

(defcustom file-props-dired-display-info-format "Comment: %c Tags: %t"
  "Format for displaying file properties.
%c - the file comment
%t - the list of tags"
  :type 'string
  :group 'file-props)

(defun file-props-dired-display-info ()
  "Display comment and tags for the current file.
Only display it if there are at least one tag or a comment."
  (let* ((file (dired-get-filename))
         (comment (file-props-get-comment file))
         (tags (file-props-get-tags file)))
    (when (or (and comment (not (string= comment "")))
              (and tags (not (string= (car tags) ""))))
      (message
       (format-spec
        file-props-dired-display-info-format
        (list
         (cons ?c (or comment ""))
         (cons ?t (if tags (mapconcat
                            'princ
                            tags
                            ", ")
                    ""))))))))

(defadvice dired-next-line (after file-props-dired-next-line-advice disable)
  "Advice to display file properties for `dired-next-line'."
  (file-props-dired-display-info))

(defadvice dired-previous-line (after file-props-dired-previous-line-advice disable)
  "Advice to display file properties for `dired-previous-line'."
  (file-props-dired-display-info))

(defun file-props-dired-activate-display ()
  "Activate display of comment and tags in Dired."
  (interactive)
  (ad-enable-regexp "file-props-dired")
  (ad-activate-regexp "file-props-dired"))

(defun file-props-dired-deactivate-display ()
  "Deactivate display of comment and tags in Dired."
  (interactive)
  (ad-disable-regexp "file-props-dired")
  (ad-deactivate-regexp "file-props-dired"))

(file-props-load-properties)

(defun file-props-dired-setup-keybindings ()
  "Setup Dired keybindings for file-props."
  (interactive)
  (require 'dired)
  (define-key dired-mode-map (kbd "C-c f c") 'file-props-dired-add-comment)
  (define-key dired-mode-map (kbd "C-c f t") 'file-props-dired-add-tags)
  (define-key dired-mode-map (kbd "C-c f e") 'file-props-dired-edit)
  (define-key dired-mode-map (kbd "C-c f m") 'file-props-dired-mark-from-tag))

(provide 'file-props)

;;; file-props.el ends here
