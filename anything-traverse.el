;;; anything-traverse.el --- Use traverselisp within anything.

;; Copyright (C) 2008, 2009 Thierry Volpiatto, all rights reserved

;; Author: thierry volpiatto
;; Maintainer: thierry volpiatto

;; Created: lun jan 12 11:23:02 2009 (+0100)

;; X-URL: http://mercurial.intuxication.org/hg/traverselisp
;; Keywords: data, regexp

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;  ==========
;;  This is the sources and functions to use traverselisp.el
;;  with anything. http://www.emacswiki.org/cgi-bin/wiki/Anything.
;;
;;  You will be able to incremental search any regexp in current buffer
;;  or in all files of current dired buffer.
;;
;;  Three sources are available:
;;  `anything-c-source-traverse-occur'
;;       Search for regexp in all files or marked files of a dired buffer
;;       or for regexp in current buffer.
;;  `anything-c-source-files-in-current-tree'
;;       Display all files of current directory and his subdirectories.
;;  `anything-traverse-c-source-record-positions'
;;       Retrieve recorded position in current buffer.
;;
;;  NOTE: You don't need this file to use traverselisp.el if you don't use
;;  Anything.

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `anything-files-in-current-tree'
;;    Show files in current tree.
;;  `anything-traverse-next-or-prec-file'
;;    Go to next or precedent file in anything buffer.
;;  `anything-traverse'
;;    Launch anything with traverse separately.
;;  `anything-traverse-positions-ring'
;;    Preconfigured anything to retrieve positions in current-buffer.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;

;;; Install:
;;  =======
;; You have first to install anything and traverselisp.
;; Then put this file somewhere in your load-path and:
;; (require 'anything-traverse)

;;; Usage:
;;  =====
;; `anything-traverse':
;;
;; M-x anything-traverse
;;
;; If you add `anything-c-source-traverse-occur' to `anything-sources'
;; you will be able to use traverse from the main anything, but the
;; variable `anything-traverse-check-only' will not be available:
;; When searching in a dired buffer the search will be performed on ALL files
;; unless you mark files in this buffer.
;; You can mark only one file and the search will be performed in this file only.
;; If instead you use `anything-traverse' , a prefix arg will be available:
;; C-u M-x anything-traverse will give you a prompt for the .ext/or regexp matching only files.
;; You can give as many .ext file you want at this prompt separated with a space.
;; You can add also regexp to this list or plain_word:
;; Exemple: SearchOnly:.el .sh$ TAGS .\.py~$
;; Will search only in .el .sh TAGS and .py~ files.
;; You can also use `anything-traverse' as `re-builder', when regexp in anything-pattern
;; satisfy you, you can copy it to kill-ring (select in actions).
;;
;; `anything-files-in-current-tree':
;;
;; M-x anything-files-in-current-tree
;; If you use  a prefix arg, the database will be updated.
;; If you have `anything-c-files-in-current-tree-allow-tagging' to a non--nil value
;; (the default) the anything tag file (`anything-c-files-in-current-tree-tag-file-name')
;; will be rewrited.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:

(eval-when-compile (require 'cl))
(require 'traverselisp)

;;; User variables
(defvar anything-c-traverse-func 'traverse-buffer-process-ext
  "*See `traverse-buffer-process-ext' in traverselisp.el.")

(defvar anything-c-traverse-length-line 80
  "*Length of the line displayed in anything buffer.")

(defvar anything-c-files-in-current-tree-ignore-files traverse-ignore-files
  "*See `traverse-ignore-files' in traverselisp.el.")

(defvar anything-c-traverse-ignore-files traverse-ignore-files
  "*See `traverse-ignore-files' in traverselisp.el.")

(defvar anything-c-traverse-fontify-buffer nil
  "*Fontify buffer before starting a search in a buffer.
This have no effect on searching in files from dired.
This can SLOW down search when non--nil.")

(defvar anything-c-files-in-current-tree-allow-tagging t
  "*Allow create and use anything tags files for persistent data.")

(defvar anything-c-files-in-current-tree-tag-file-name "ANYTHING-TAG-FILE"
  "*The name your anything tags files will have.")

(defvar anything-c-traverse-browse-regexp-lisp
  "\(def\\(un\\|subst\\|macro\\|ine\\|face\\|alias\\|advice\\|struct\\|type\\|theme\\|var\\|group\\|custom\\|const\\)"
  "*Regexp used to parse lisp buffer when browsing code.")

(defvar anything-c-traverse-browse-regexp-python
  "\\<def\\>\\|\\<class\\>"
  "*Regexp used to parse python buffer when browsing code.")

;;; Internals variables
(defvar anything-traverse-current-buffer)
(defvar anything-c-traverse-overlay-face nil)
(defvar anything-traverse-occur-overlay nil)
(defvar anything-c-traverse-diredp-flag nil)
(defvar anything-traverse-check-only nil)
(defvar anything-traverse-killed-pattern nil)
(defvar anything-c-files-in-current-tree-table (make-hash-table :test 'eq))
(defvar anything-traverse-buffer-positions-ring nil)
(make-variable-buffer-local 'anything-traverse-buffer-positions-ring)
(defvar anything-c-traverse-browse-regexp nil)

;;; Types
(defface anything-traverse-overlay-face '((t (:background "IndianRed4" :underline t)))
  "Face for source header in the anything buffer." :group 'traverse-faces)
(setq anything-c-traverse-overlay-face 'anything-traverse-overlay-face)

;;; Hooks
(add-hook 'anything-cleanup-hook #'(lambda ()
                                     (when anything-traverse-occur-overlay
                                       (delete-overlay anything-traverse-occur-overlay)
                                       (setq anything-traverse-occur-overlay nil))
                                     (setq anything-traverse-check-only nil)))
                                     
(add-hook 'anything-after-persistent-action-hook #'(lambda ()
                                                     (when anything-traverse-occur-overlay
                                                       (delete-overlay anything-traverse-occur-overlay)
                                                       (anything-traverse-occur-color-current-line))))

;;; Keymap
(define-key anything-map (kbd "M-<down>") #'anything-traverse-next-or-prec-file)
(define-key anything-map (kbd "M-<up>") #'(lambda ()
                                         (interactive)
                                         (anything-traverse-next-or-prec-file -1)))

;;; Shared Functions
(defun anything-traverse-occur-color-current-line ()
  "Highlight and underline current position"
  (if (not anything-traverse-occur-overlay)
      (setq anything-traverse-occur-overlay
            (make-overlay
             (line-beginning-position) (1+ (line-end-position))))
      (move-overlay anything-traverse-occur-overlay
                    (line-beginning-position) (1+ (line-end-position))))
  (overlay-put anything-traverse-occur-overlay
               'face anything-c-traverse-overlay-face))

(defun anything-c-traverse-buffer-action (elm)
  (let (pos-elm)
    (when (string-match "[0-9]+" elm 0)
      (setq pos-elm (string-to-number (match-string 0 elm))))
    (with-current-buffer anything-traverse-current-buffer
      (goto-line pos-elm))))

(defun anything-c-traverse-dir-action (elm)
  (let* ((elm-split   (split-string elm " "))
         (fname       (nth 0 elm-split))
         (line-number (first (split-string (nth 1 elm-split)
                                           ":"))))
    (find-file fname)
    (goto-line (string-to-number line-number))))

(defun anything-c-traverse-default-action (elm)
  (if anything-c-traverse-diredp-flag
      (anything-c-traverse-dir-action elm)
      (anything-c-traverse-buffer-action elm)))

;;; Anything files in current tree
(defun anything-c-files-in-current-tree-create-db (&optional tree)
  (let* ((cur-dir (or tree (expand-file-name default-directory)))
         (files-list (gethash (intern cur-dir)
                              anything-c-files-in-current-tree-table)))
    (unless files-list
      (message "Please Wait, creating database ...")
      (setq files-list
            (puthash (intern cur-dir)
                     (traverse-list-files-in-tree
                      cur-dir
                      anything-c-files-in-current-tree-ignore-files)
                     anything-c-files-in-current-tree-table)))
    files-list))

(defun anything-files-in-current-tree-tag-tree (&optional update)
  (let ((cur-tree  (expand-file-name default-directory))
        (tag-fname (expand-file-name anything-c-files-in-current-tree-tag-file-name
                                     default-directory)))
    (when update
      (remhash (intern cur-tree)
               anything-c-files-in-current-tree-table))
    (let ((files-list (anything-c-files-in-current-tree-create-db)))
      (if (file-writable-p tag-fname)
          (with-current-buffer (find-file-noselect tag-fname)
            (erase-buffer)
            (goto-char (point-min))
            (dolist (i files-list)
              (insert (concat i "\n")))
            (save-buffer) (kill-buffer (current-buffer))
            (message "%d Files have been indexed in %s." (length files-list) cur-tree)
            (sit-for 2))
          (message "Unable to create file <%s>: permission denied" tag-fname)
          (sit-for 2)))))
        
                
(defun anything-c-files-in-current-tree-init ()
  (with-current-buffer (anything-candidate-buffer 'local)
    (let ((tag-file (expand-file-name anything-c-files-in-current-tree-tag-file-name
                                      default-directory))
          files-list)
      (if (and anything-c-files-in-current-tree-allow-tagging
               (file-exists-p tag-file))
          (insert-file-contents tag-file)
          (setq files-list (anything-c-files-in-current-tree-create-db))
          (dolist (i files-list)
            (insert (concat i "\n")))))))

(defun anything-files-in-current-tree ()
  "Show files in current tree.
with prefix arg refresh data base."
  (interactive)
  (let* ((cur-tree (expand-file-name default-directory))
         (tag-file (expand-file-name anything-c-files-in-current-tree-tag-file-name
                                     cur-tree)))
    (if current-prefix-arg
        (progn
          (if anything-c-files-in-current-tree-allow-tagging
              (anything-files-in-current-tree-tag-tree 'update)
              (remhash (intern cur-tree) anything-c-files-in-current-tree-table))
          (anything 'anything-c-source-files-in-current-tree))
        (if (and anything-c-files-in-current-tree-allow-tagging
                 (not (file-exists-p tag-file)))
            (progn
              (anything-files-in-current-tree-tag-tree)
              (anything 'anything-c-source-files-in-current-tree))
            (anything 'anything-c-source-files-in-current-tree)))))

;;; Anything positions
(defun anything-traverse-record-pos ()
  (interactive)
  (let* ((str-at-pos    (buffer-substring (point-at-bol) (point-at-eol)))
         (line-number   (propertize (int-to-string (line-number-at-pos))
                                    'face 'traverse-match-face))
         (line-to-store (concat line-number ":" str-at-pos "\n")))
    (if (not (member line-to-store anything-traverse-buffer-positions-ring))
        (progn
          (push line-to-store anything-traverse-buffer-positions-ring)
          (message "Record new marked position at line %s" line-number))
        (message "Position at line %s is already recorded" line-number))))


(defun anything-traverse-position-relocate-maybe (elm)
  (let* ((elm-mod     (concat elm "\n"))
         (pos-in-list (position elm-mod anything-traverse-buffer-positions-ring :test 'equal))
         (new-pos     nil)
         (dry-elm     (replace-regexp-in-string "\\(^[0-9]+:\\)" "" elm)))
    ;; Goto line position
    (anything-c-traverse-buffer-action elm)
    ;; If line have changed, try to relocate it.
    (unless (string= dry-elm (buffer-substring (point-at-bol) (point-at-eol)))
      (when (or (search-backward dry-elm (point-min) t)
                (search-forward dry-elm (point-max) t))
        (setq new-pos (point))
        ;; If success, remove old elm.
        (setq anything-traverse-buffer-positions-ring
              (remove elm-mod anything-traverse-buffer-positions-ring)))
      ;; Record new position.
      (when new-pos
        (goto-char new-pos)
        (forward-line 0)
        (anything-traverse-record-pos)))
    ;; put this candidate at top of stack in ring.
    (unless new-pos
      (push (pop (nthcdr pos-in-list anything-traverse-buffer-positions-ring))
            anything-traverse-buffer-positions-ring))))

          
(defun anything-traverse-positions-ring ()
  "Preconfigured anything to retrieve positions in current-buffer."
  (interactive)
  (anything 'anything-traverse-c-source-record-positions))

;;; Anything traverse
(defun* anything-traverse-next-or-prec-file (&optional (n 1))
  "Go to next or precedent file in anything buffer.
When search is performed in dired buffer on all files
this allow to switch from one file to the other.
If we are in another source just go to next/prec line."
  (interactive)
  (with-anything-window
      (if anything-c-traverse-diredp-flag
          (progn
            (let* ((current-line-list  (split-string
                                        (buffer-substring
                                         (point-at-bol)
                                         (point-at-eol))))  
                   (current-fname      (nth 0 current-line-list))
                   ;; Don't use file names like "somename+.el"
                   (current-fname-less (replace-regexp-in-string "\+"
                                                                 ""
                                                                 (file-name-sans-extension
                                                                  current-fname)))
                   (fn-b-o-f           (if (eq n 1) 'eobp 'bobp))) ; func back or forward
              (catch 'break
                (while (not (funcall fn-b-o-f))
                  (forward-line n)
                  (beginning-of-line)
                  (when (not (or (re-search-forward current-fname
                                                    (point-at-eol) t)
                                 (when (string-match "\+" current-fname)
                                   (re-search-forward current-fname-less
                                                      (point-at-eol) t))))
                    (anything-mark-current-line)
                    (throw 'break nil))))
              (if (eq n 1)
                  (when (eobp)
                    (re-search-backward ".")
                    (beginning-of-line)
                    (anything-mark-current-line))
                  (when (bobp)
                    (forward-line)
                    (beginning-of-line)
                    (anything-mark-current-line)))))
          (if (eq n 1)
              (anything-next-line)
              (anything-previous-line)))))

(defun anything-traverse ()
  "Launch anything with traverse separately."
  (interactive)
  (if current-prefix-arg
      (progn
        (if (car (rassq (current-buffer) dired-buffers))
            (progn
              (setq anything-traverse-check-only
                    (split-string
                     (read-string
                      (propertize "SearchOnly: "
                                  'help-echo "You can use here .ext, regexp, or plain_name separated by spaces"))))
              (anything 'anything-c-source-traverse-occur))
            (anything-traverse-at-point)))
      (setq anything-traverse-check-only nil)
      (anything 'anything-c-source-traverse-occur)))

(defun anything-traverse-at-point ()
  "Launch anything-traverse with `thing-at-point' as input."
  (let ((input (thing-at-point 'symbol)))
    (anything 'anything-c-source-traverse-occur input)))

;; (find-fline "~/labo/traverse-qpatch/traverselisp.el" "traverse-dired-get-marked-files")
;; (find-fline "~/labo/traverse-qpatch/traverselisp.el" "defun* traverse-buffer-process-ext")
;; (find-fline "~/labo/traverse-qpatch/traverselisp.el" "defun traverse-file-process-ext")
(defun anything-traverse-init-search ()
  "Main function that proceed search in current-buffer.
If current-buffer is a dired buffer search is performed on all files."
  (setq anything-traverse-killed-pattern anything-pattern)
  (let ((anything-traverse-buffer (get-buffer-create "*Anything traverse*"))
        (dired-buffer-name        (find (rassoc anything-traverse-current-buffer
                                                dired-buffers)
                                        dired-buffers)))
    (with-current-buffer anything-traverse-buffer
      (erase-buffer)
      (goto-char (point-min))
      (if anything-c-traverse-diredp-flag
          ;; search is performed on files from this dired buffer
          (let* ((marked-list (with-current-buffer anything-traverse-current-buffer
                                (traverse-dired-get-marked-files t)))
                 (dir-list    (or marked-list
                                  (traverse-list-directory (car dired-buffer-name) t))))
            ;; walk files of current dir
            (dolist (f dir-list)
              (if (and anything-traverse-check-only
                       (not (file-directory-p f)))
                  (when (traverse-check-only-lists f anything-traverse-check-only)
                    (traverse-file-process-ext
                     anything-pattern
                     f))
                  (unless (or (file-directory-p f)
                              (traverse-check-only-lists f anything-c-traverse-ignore-files)
                              (file-compressed-p f)
                              (file-symlink-p f)
                              (not (file-regular-p f)))
                    (traverse-file-process-ext
                     anything-pattern
                     f)))))
          ;; search in current-buffer
          ;; fontify buffer
          (when anything-c-traverse-fontify-buffer
            (with-current-buffer anything-traverse-current-buffer
              (jit-lock-fontify-now)))
          (traverse-buffer-process-ext
           anything-pattern
           anything-traverse-current-buffer
           :lline anything-c-traverse-length-line))
      (split-string (buffer-string) "\n"))))

;;; Sources
(defvar anything-c-source-traverse-occur
  '((name . "Traverse Occur")
    (init . (lambda ()
              (setq anything-traverse-current-buffer
                    (current-buffer))
              (let ((dired-buffer-name (find (rassoc anything-traverse-current-buffer
                                                     dired-buffers)
                                                     dired-buffers)))
                (if dired-buffer-name
                    (setq anything-c-traverse-diredp-flag t)
                    (setq anything-c-traverse-diredp-flag nil)))))
    (candidates . anything-traverse-init-search)
    (action . (("Go to Line" . (lambda (elm)
                                 (anything-c-traverse-default-action elm)))
               ("Copy regexp" . (lambda (elm)
                                  (kill-new anything-traverse-killed-pattern)))))
    (persistent-action . (lambda (elm)
                           (anything-c-traverse-default-action elm)
                           (anything-traverse-occur-color-current-line)))
    (requires-pattern . 2)
    (get-line . buffer-substring)
    (volatile)
    (delayed)))

;; (anything 'anything-c-source-traverse-occur)

;; (find-epp anything-type-attributes)
(defvar anything-c-source-files-in-current-tree
  '((name . "Files from Current Tree")
    (init . anything-c-files-in-current-tree-init)
    (candidates-in-buffer)
    (type . file)))

;; (anything 'anything-c-source-files-in-current-tree)

(defvar anything-traverse-c-source-record-positions
  '((name . "Traverse Mark Pos")
    (init . (lambda ()
              (setq anything-traverse-current-buffer
                    (current-buffer))
              (let ((cand-list anything-traverse-buffer-positions-ring))
                (with-current-buffer (anything-candidate-buffer 'local)
                  (dolist (i cand-list)
                    (if (with-current-buffer anything-current-buffer
                          (goto-char (point-min))
                          (search-forward (replace-regexp-in-string "\\(^[0-9]+:\\)" "" i) (point-max) t))
                        (insert i)
                        (insert
                         (propertize i 'face '((:foreground "red"))
                                     'help-echo "WARNING:Line has been modified, you should relocate it or remove it"))))))))
    (candidates-in-buffer)
    (action . (("Go to Line" . (lambda (elm)
                                 (anything-traverse-position-relocate-maybe elm)))
               ("Refresh Ring" . (lambda (elm)
                                   (with-current-buffer anything-current-buffer
                                     (goto-char (point-min))
                                     (dolist (i anything-traverse-buffer-positions-ring)
                                       (let ((regex-only (replace-regexp-in-string "[0-9]*:" "" i)))
                                         (when (not (search-forward regex-only (point-max) t))
                                           (setq anything-traverse-buffer-positions-ring
                                                 (remove i anything-traverse-buffer-positions-ring))))))))
               ("Remove Entry" . (lambda (elm)
                                   (let ((elm-mod (concat elm "\n")))
                                     (with-current-buffer anything-current-buffer
                                       (setq anything-traverse-buffer-positions-ring
                                             (remove elm-mod anything-traverse-buffer-positions-ring))))))
               ("Clear Ring" . (lambda (elm)
                                 (with-current-buffer anything-current-buffer
                                   (setq anything-traverse-buffer-positions-ring nil))))))
    (get-line . buffer-substring)
    (persistent-action . (lambda (elm)
                           (anything-traverse-position-relocate-maybe elm)
                           (anything-traverse-occur-color-current-line)))))

;; (anything 'anything-traverse-c-source-record-positions)

(defvar anything-c-source-traverse-browse-code 
  '((name . "Browse code")
    (init . (lambda ()
              (when anything-c-traverse-fontify-buffer
                (with-current-buffer anything-current-buffer
                  (jit-lock-fontify-now)))
              (cond ((eq major-mode 'emacs-lisp-mode)
                     (setq anything-c-traverse-browse-regexp
                           anything-c-traverse-browse-regexp-lisp))
                    ((eq major-mode 'python-mode)
                     (setq anything-c-traverse-browse-regexp
                           anything-c-traverse-browse-regexp-python)))
              (let ((lis (traverse-find-readlines anything-current-buffer
                                                  anything-c-traverse-browse-regexp
                                                  :insert-fn 'buffer)))
                (with-current-buffer (anything-candidate-buffer 'local)
                  (dolist (f lis)
                    (let ((pos (number-to-string (car f))))
                      (insert (concat  pos ": " (cadr f) "\n"))))))))
    (candidates-in-buffer)
    (action . (("GotoLine" . (lambda (elm)
                               (let* ((lis-elm (split-string elm ":"))
                                      (num (string-to-number (car lis-elm))))
                                 (goto-line (1+ num)))))))
    (get-line . buffer-substring)
    (persistent-action . (lambda (elm)
                           (let* ((lis-elm (split-string elm ":"))
                                  (num (string-to-number (car lis-elm))))
                             (goto-line (1+ num)))
                           (anything-traverse-occur-color-current-line)))))
                  

;; (anything 'anything-c-source-traverse-browse-code)

(defun anything-traverse-browse-code ()
  (interactive)
  (anything 'anything-c-source-traverse-browse-code))

;;; Provide
(provide 'anything-traverse)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; anything-traverse.el ends here
