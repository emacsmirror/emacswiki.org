;;; tagger.el --- Tagged information handler

;; Copyright (C) 2007  Tamas Patrovics

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; When one finds out about something it's useful if the information
;; is recorded somewhere. I usually use some file to dump everything
;; in it and use incremental search to find stuff. This is not
;; efficient, it is more useful to limit the search to a particular
;; context, so the the information can be retrieved easier. One can
;; keep separate note files, of course, but these tend to proliferate.
;;
;; This package provides a very simple information manager mode where
;; information snippets can be tagged and retrieved using tags.
;;
;; Each information snippet has a title and several tags. The title
;; begins with a * and tags are in the title after a / character:
;;
;;   * some title /tag1 tag2 ...
;;
;; Tags are separated with whitespaces.
;;
;;
;; tagger-mode activates the mode in a buffer.
;;
;; tagger-show shows only the snippets having all the given tags or
;; everything if an empty string is given.
;;
;;
;; Note there is tab completion for tags both in the snippet title and
;; in the minibuffer.
;;
;;
;; Tested on Emacs 22.
;;

;;; Code:

(require 'cl)

(defvar tagger-title-regexp "^\\*")

(defvar tagger-tag-marker "/")

(defvar tagger-title-face 'highlight
   "Face for titles.")

(defvar tagger-tag-face 'lazy-highlight
   "Face for tags.")

;;----------------------------------------------------------------------

(define-derived-mode tagger-mode text-mode "Tagger"
   (set (make-local-variable 'font-lock-defaults) '(tagger-font-lock-keywords))
   (setq header-line-format " No filtering. Everything is shown.")
   (modify-syntax-entry ?/ "."))


(define-key tagger-mode-map (kbd "<tab>") 'tagger-complete-tag-in-title)


(defvar tagger-font-lock-keywords `((,(concat tagger-title-regexp
                                               ".*\n") . tagger-title-face)
                                     (,(concat tagger-title-regexp
                                               ".*\\("
                                               tagger-tag-marker
                                               ".*\\)\n") .
                                               (1 tagger-tag-face t)))
   "Font lock rules.")


(defvar tagger-current-tags nil
   "The list of current tags used to filter the buffer.")

(defvar tagger-buffer nil
   "The current buffer with tagger-mode.")



(defun tagger-show ()
   "Read tags and show only stuff with those tags. If the read
string is empty then show all tags."
   (interactive)
   (setq tagger-buffer (current-buffer))

   (let ((icicle (and (boundp 'icicle-mode)
                      icicle-mode)))
     (if icicle
         (icicle-mode -1))
     
     (unwind-protect
         (let ((minibuffer-local-map (copy-keymap minibuffer-local-map)))
           (define-key minibuffer-local-map
             (kbd "<tab>") 'tagger-complete-tag-in-minibuffer)

           (setq tagger-current-tags
                 (read-string "Tags (use whitespace for multiple tags): "
                              (mapconcat 'identity tagger-current-tags " "))))

       (if icicle
           (icicle-mode 1))))

   (let ((inhibit-read-only t)
         (modified (buffer-modified-p))
         (buffer-undo-list t))

     (setq tagger-current-tags 
           (delete-dups (tagger-split-tag-string tagger-current-tags)))

     (unwind-protect
         (progn
           ;; show all first
           (remove-list-of-text-properties (point-min) (point-max) '(read-only
                                                                     invisible))
           (if tagger-current-tags
               (let ((tag-info (tagger-collect-tag-info)))
                 (while tag-info
                   (let* ((info (pop tag-info))
                          (title-tags (cdr info))
                          (next (car tag-info)))
                     (unless (and title-tags
                                  (every (lambda (tag)
                                           (member tag title-tags))
                                         tagger-current-tags))
                       (add-text-properties (car info) 
                                            (if next
                                                (car next)
                                              (point-max))
                                            '(invisible t read-only t)))))

                 (setq header-line-format 
                       (concat " Filtered by tags: " 
                               (mapconcat 'identity tagger-current-tags " ")))
                 (recenter))

             (setq header-line-format " No filtering. Everything is shown.")))

       (unless modified
         (restore-buffer-modified-p nil)))))


(defun tagger-complete-tag-in-title ()
   "Complete tag in a title line."
   (interactive)
   (if (not (save-excursion
              (beginning-of-line)
              (looking-at "^\\*")))
       (message "Not on a title line.")

     (if (not (save-excursion
                (beginning-of-line)
                (looking-at "^\\*.*/\\(.*\\)")))
         (message "Not tag section in the title.")

       (let ((tags-beginning (match-beginning 1)))
         (if (< (point) tags-beginning)
             (message "Not in the tag section of the title.")

           (tagger-complete-tag (tagger-get-all-tags)
                                tags-beginning
                                (line-end-position)))))))


(defun tagger-complete-tag-in-minibuffer ()
   "Complete tag in the minibuffer."
   (interactive)
   (let ((enable-recursive-minibuffers t))
     (tagger-complete-tag (with-current-buffer tagger-buffer
                            (tagger-get-all-tags))
                          (minibuffer-prompt-end)
                          (point-max)
                          'tagger-get-intersection-tags)))


(defun tagger-complete-tag (tags region-start region-end &optional tag-filter)
   "Complete current tag at point from TAGS and insert the result
   into the buffer. REGION-START and REGION-END are the limits of
   the region where this function operates.

TAG filter is function called with the current set of local tags
and the list of completion candidates. It should return a
filtered set of completion candidates."
   (let* ((end (save-excursion
                 (skip-syntax-forward "w_")
                 (point)))
          (begin (save-excursion
                   (skip-syntax-backward "w_")
                   (point)))
          (input (buffer-substring-no-properties
                  begin end))
          (try (let ((local-tags (append (tagger-split-tag-string 
                                          (buffer-substring-no-properties
                                           region-start begin))
                                         (tagger-split-tag-string 
                                          (buffer-substring-no-properties
                                           end region-end)))))
                 (when tag-filter
                   (setq tags (funcall tag-filter local-tags))
                   (unless tags
                     (error "No available completions in this context")))
                         
                 ;; remove the current input and tags in the local
                 ;; context from the list of possible completions
                 (dolist (tag (cons input local-tags))
                   (setq tags (remove tag tags)))
                 (try-completion input tags))))
     (cond ((eq try t)
            (message "Sole completion."))

           ((eq try nil)
            (message "No completions."))
              
           (t
            (let ((tag (if (equal try input)
                           (progn
                             (push ?\t unread-command-events)
                             (completing-read "Tag: " tags nil t input))
                         try)))
              (delete-region begin end)
              (insert tag))))))


(defun tagger-collect-tag-info ()
   "Get the start position and tags of snippets."
   (let (info)
     (save-excursion
       (goto-char (point-min))
       (while (re-search-forward tagger-title-regexp nil t)
         (push (cons (line-beginning-position)
                     (if (looking-at ".*/\\(.*\\)")
                         (tagger-split-tag-string (match-string-no-properties 1))))
               info)))
     (reverse info)))


(defun tagger-get-all-tags ()
   "Return all unique tags from the buffer."
   (let ((infos (tagger-collect-tag-info))
         tags)
     (dolist (info infos)
       (dolist (tag (cdr info))
         (unless (member tag tags)
           (push tag tags))))
     tags))


(defun tagger-split-tag-string (str)
  "Split STR into a list of tags."
  (split-string str "[ \t]" t))


(defun tagger-get-intersection-tags (local-tags)
  "Return tags for titles which have an intersection with LOCAL-TAGS."
   (let ((infos (with-current-buffer tagger-buffer
                  (tagger-collect-tag-info)))
         tags)
     (dolist (info infos)
       (if (every (lambda (tag)
                    (member tag (cdr info)))
                  local-tags)
           (dolist (tag (cdr info))
             (unless (member tag tags)
               (push tag tags)))))
     tags))


(provide 'tagger)
;;; tagger.el ends here
