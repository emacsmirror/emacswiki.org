;;; traverselisp.el --- walk through directories and perform actions on files.

;; Copyright (C) 2008, 2009 Thierry Volpiatto
;; Author:     Thierry Volpiatto 
;; Maintainer: Thierry Volpiatto
;; Keywords:   data

;; X-URL: http://mercurial.intuxication.org/hg/traverselisp

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;; Developped and tested on:
;; GNU Emacs 23.0.91.1 (i686-pc-linux-gnu, GTK+ Version 2.14.7)
 
;; Install:
;; =======
;; Put this file in your load-path
;; And Byte-compile it.(If you don't do that you will have error)
;; Add to your .emacs:
;;
;; (require 'traverselisp)
;;
;; Set up your prefered keys for dired and globals as usual
;;
;; Here is my config with version-1.16:
;; ===================================
;; (require 'traverselisp)
;; (setq traverse-use-avfs t)
;; (global-set-key (kbd "<f5> f") 'traverse-deep-rfind)
;; (global-set-key (kbd "<f5> u") 'traverse-build-tags-in-project)
;; (global-set-key (kbd "C-c C-o") 'traverse-occur-current-buffer)
;; (define-key dired-mode-map (kbd "A") 'traverse-dired-search-regexp-in-anything-at-point)
;; (define-key dired-mode-map (kbd "C-c C-z") 'traverse-dired-browse-archive)
;; (define-key dired-mode-map (kbd "C-c t") 'traverse-dired-find-in-all-files)
;; (add-to-list 'traverse-ignore-files ".ledger-cache")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; * Commands defined here:
;; [EVAL] (traverse-auto-document-lisp-buffer :type 'command)
;; `traverselisp-version'
;; `traverse-quit'
;; `traverse-find-in-file'
;; `traverse-occur-current-buffer'
;; `traverse-deep-rfind'
;; `traverse-search-in-dired-dir-at-point'
;; `traverse-search-in-dired-file-at-point'
;; `traverse-dired-browse-archive'
;; `traverse-dired-search-in-archive'
;; `traverse-dired-find-in-marked-files'
;; `traverse-dired-find-in-all-files'
;; `traverse-dired-search-regexp-in-anything-at-point'
;; `traverse-go-forward'
;; `traverse-go-backward'
;; `traverse-scroll-down-other-window'
;; `traverse-scroll-up-other-window'
;; `traverse-search-and-replace'
;; `traverse-search-and-replace-all'
;; `traverse-cp-or-mv-extfiles-in-dir'
;; `traverse-build-tags-in-project'
;; `traverse-toggle-split-window-h-v'
;; `traverse-count-files-in-dir'
;; `traverse-pprint-tree'

;;========LIMIT=========(DONT REMOVE!)
;;
;;;  * Non--interactive functions defined here:
;; [EVAL] (traverse-auto-document-lisp-buffer :type 'function :prefix "traverse")
;; `traverse-list-directory'
;; `traverse-walk-directory'
;; `traverse-comp-str-to-list'
;; `traverse-check-only-lists'
;; `traverse-find-readlines'
;; `traverse-file-process'
;; `traverse-file-process-ext'
;; `traverse-buffer-process-ext'
;; `traverse-occur-color-current-line'
;; `traverse-button-func'
;; `traverse-prepare-buffer'
;; `traverse-read-regexp'
;; `traverse-dired-get-marked-files'
;; `traverse-dired-has-marked-files'
;; `traverse-go-forward-or-backward'
;; `traverse-window-split-h-or-t'
;; `traverse-list-directories-in-tree'
;; `traverse-list-files-in-tree'
;; `traverse-apply-func-on-files'
;; `traverse-apply-func-on-dirs'

;;========LIMIT=========(DONT REMOVE!)
;;
;;;  * Internal variables defined here:
;; [EVAL] (traverse-auto-document-lisp-buffer :type 'internal-variable :prefix "traverse")
;; `traversedir-mode-map'
;; `traverse-match-overlay-face'
;; `traverse-show-regexp-delay'
;; `traverse-keep-indent'
;; `traverse-occur-use-miniwindow'
;; `traverse-miniwindow-width'
;; `traverse-count-occurences'
;; `traverse-occur-overlay'
;; `traverse-last-regexp'
;; `traverse-replace-auth'

;;========LIMIT=========(DONT REMOVE!)
;;
;;;  * Faces defined here:
;; [EVAL] (traverse-auto-document-lisp-buffer :type 'faces :prefix "traverse")
;; `traverse-match-face'
;; `traverse-regex-face'
;; `traverse-path-face'
;; `traverse-overlay-face'

;;========LIMIT=========(DONT REMOVE!)
;;
;;;  * User variables defined here:
;; [EVAL] (traverse-auto-document-lisp-buffer :type 'user-variable :prefix "^traverse")
;; `traverse-ignore-files'
;; `traverse-ignore-dirs'
;; `traverse-length-line'
;; `traverse-file-function'
;; `traverse-use-avfs'
;; `traverse-avfs-default-directory'

;;========LIMIT=========(DONT REMOVE!)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Usage:
;; =====
;; M-x `traverse-deep-rfind'
;; When searching is done and you are in traverse buffer
;; some interactive actions are provided for navigate and for replacing regexps
;; Use "C-h m" for more info while in traverse-buffer.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Special commands:
;; key             binding
;; ---             -------

;; ESC             Prefix Command
;; N               traverse-go-forward
;; P               traverse-go-backward
;; R               traverse-search-and-replace-all ==> [interactive menu]
;; S               traverse-search-and-replace
;; q               traverse-quit
;; <S-down>        traverse-scroll-down-other-window
;; <S-up>          traverse-scroll-up-other-window
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; You can also use traverse from DIRED:
;; M-x `traverse-dired-search-regexp-in-anything-at-point'
;; This function work on directory, files, (1)compressed files (AVFS)
;; and marked files:
;; If you have marked files search will be performed on these files.
;; If no files are marked, traverse will search in element at point
;; with appropriate function.
;; However, you can use specialized functions, check this file to see all
;; the interactives functions.
;;
;; M-x `traverse-dired-find-in-all-files'
;; Search in all regular files in the current dired buffer
;;
;; M-x `traverse-occur-current-buffer'
;; Just like occur but you can navigate and replace regexp.
;;
;; M-x `traverse-dired-browse-archive'
;; This function use (1)AVFS to browse archive tar.gz, bz2 etc..
;; Other functions are provided:
;; `traverse-cp-or-mv-extfiles-in-dir'
;; `traverse-build-tags-in-project'
;;
;; (1)NOTE: You have to install AVFS and enable fuse in your kernel if
;; you want to browse and search in archives.
;; Please see the doc of your distrib.
;; and the doc of AVFS
;; http://sourceforge.net/projects/avf
;; If you don't want to use AVFS in traverse, set `traverse-use-avfs'
;; to nil (or do nothing because it's the default)
;; 
;; You can also use traverselisp.el in anything.el with the appropriate sources:
;; http://www.emacswiki.org/emacs/AnythingSources
;;
;; Contact:
;; =======
;; thierry dot volpiatto hat gmail dot com
;; You can get the developpement version of the file here with hg:
;; hg clone http://mercurial.intuxication.org/hg/traverselisp
;; For the current developpement branch:
;; hg update -C 1.1.0
;; or with DVC ==> C-u C-u M-x xhg-update RET 1.1.0 (or last rev number)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;; http://freehg.org/u/thiedlecques/traverselisp/ 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Version:
(defconst traverse-version "1.1.12")

;;; Code:

(require 'derived)
(eval-when-compile (require 'cl))


(defvar traversedir-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?q] 'traverse-quit)
    (define-key map [?S] 'traverse-search-and-replace)
    (define-key map [?R] 'traverse-search-and-replace-all)
    (define-key map [?N] 'traverse-go-forward)
    (define-key map [?P] 'traverse-go-backward)
    (define-key map [(shift down)] 'traverse-scroll-down-other-window)
    (define-key map [(shift up)] 'traverse-scroll-up-other-window)
    (define-key map [?|] 'traverse-toggle-split-window-h-v)
    map)
  "Keymap used for traversedir commands.")

(define-derived-mode traversedir-mode text-mode "traversedir"
                     "Major mode to recurse in a tree and perform diverses actions on files.

Special commands:
\\{traversedir-mode-map}")

(defgroup traversedir nil
  "Mode that allow walking through directories and perform diverses actions on files."
  :prefix "traversedir-"
  :group 'text)

(defcustom traverse-ignore-files
  '(".elc$" ".pyc$"
    ".orig$" ".bz2$"
    ".gz$" ".zip$"
    ".vdi$" ".doc$"
    ".jpg$" ".avi$"
    ".jpeg$" ".png$"
    ".xpm$" ".jar$"
    ".pbm$" ".gif$"
    ".xls$" ".ppt$"
    ".mdb$" ".adp$"
    "\\<\\(TAGS\\)\\>"
    ".tiff$" ".img$"
    ".pdf$" ".dvi$"
    ".xbm$" ".gpg$"
    ".svg$" ".rej$")
  "Files we want to ignore.
Are allowed:(examples)
- extensions file ==> .ext
- Plain name ==> TAGS ; note regexps take precedence on plain names.
- Regexp ==> \".*\\(.py\\)$\""
  :group 'traversedir
  :type '(repeat string))

(defcustom traverse-ignore-dirs
  '(".hg" ".svn"
    "RCS" ".bzr"
    ".git" ".VirtualBox"
    ".arch-ids" "CVS"
    "{arch}" "knits")
  "Directories we don't want to search in."
  :group 'traversedir
  :type '(repeat string))

(defcustom traverse-length-line
  45
  "Length of the line displayed in traverse buffers."
  :group 'traversedir
  :type 'integer)

(defcustom traverse-file-function
  'traverse-file-process
  "Default function to use to process files."
  :group 'traversedir
  :type 'symbol)

(defcustom traverse-use-avfs
  nil
  "Enable support for avfs."
  :group 'traversedir
  :type 'boolean)

(defcustom traverse-avfs-default-directory
  "~/.avfs"
  "Default directory for avfs."
  :group 'traversedir
  :type 'string)

;;; Faces for traverse
(defgroup traverse-faces nil
  "Faces for TRAVERSEDIR."
  :group 'traversedir)

(defface traverse-match-face '((t (:foreground "red")))
  "TRAVERSEDIR face."
  :group 'traverse-faces)
(defface traverse-regex-face '((t (:foreground "yellow")))
  "TRAVERSEDIR face."
  :group 'traverse-faces)
(defface traverse-path-face '((t (:foreground "green")))
  "TRAVERSEDIR face."
  :group 'traverse-faces)
(defface traverse-overlay-face '((t (:background "MediumAquamarine" :underline t)))
  "Face for highlight line in matched buffer."
  :group 'traverse-faces)


;;; User's variable (you can set these variables)
(defvar traverse-match-overlay-face 'traverse-overlay-face
  "Use the default traverse face for overlay.")
(defvar traverse-show-regexp-delay 1
  "Delay in seconds where regexp found is highlighted.")
(defvar traverse-keep-indent nil
  "Keep indentation in traverse buffer if non nil.")
(defvar traverse-occur-use-miniwindow nil
  "Use a side miniwindow to display results.")
(defvar traverse-miniwindow-width 30
  "If nil split window equally.")

;;; Internal use only (DON'T modify)
(defvar traverse-count-occurences 0
  "Simple variable to store the number of occurence found.")
(defvar traverse-occur-overlay nil)
(defvar traverse-last-regexp nil
  "Used in `traverse-search-and-replace'.
remember the regexp used in last search.")
(defvar traverse-replace-auth nil
  "Used in `traverse-search-and-replace'.
Allow traverse to continue replacing operation.")

(defun traverselisp-version ()
  "Give version number of traverselisp."
  (interactive)
  (message "traverse-lisp-version-%s" traverse-version))

;;; Main backend functions

(defun traverse-quit ()
  "Quit and kill traverse buffer."
  (interactive)
  (when traverse-occur-overlay
    (delete-overlay traverse-occur-overlay))
  (quit-window t)
  (other-window 1)
  (delete-other-windows))

(defsubst traverse-list-directory (dirname &optional abs)
  "Use directory-files without these \".\" \"..\".
If abs is non-nil use absolute path."
  (directory-files dirname abs "[^\\.]"))

(defsubst* traverse-walk-directory (dirname &key file-fn dir-fn exclude-files exclude-dirs)
  "Walk through dirname and use file-fn and/or dir-fn function on each file found.
`dirname' ==> we start in this directory

Use keys to set args:

You must specify at list one of these 2 functions:
`:file-fn' ==> function to apply to FILES
`:dir-fn' ==> function to apply on DIRECTORIES

Files or directories in these lists will be skipped:
`:excludes-files' ==> list of .ext or files to ignore.  
`:exclude-dirs' ==> list of directory to ignore.
Look at `traverse-ignore-files' and `traverse-ignore-dirs'
"
  (labels
      ((walk (name)
         (cond ((and (file-directory-p name) ;; DIR PROCESSING
                     (not (file-symlink-p name))) ;; don't follow symlinks
                (when dir-fn
                  (funcall dir-fn name))
                (if exclude-dirs
                    (dolist (x (traverse-list-directory name t))
                      (when (stringp x) ;; be sure x is a string and not nil
                        (unless (member (file-name-nondirectory x) exclude-dirs)
                          (walk x)))) ;; Return to TOP and take the good cond
                    (dolist (x (traverse-list-directory name t))
                      (when (stringp x)
                        (walk x))))) ;; Return to TOP and take the good cond
               ((and (file-regular-p name) ;; FILE PROCESSING
                     (not (file-symlink-p name))) ;; don't follow symlinks
                (when file-fn
                  (if exclude-files
                      (unless (traverse-check-only-lists name exclude-files)
                        (funcall file-fn name))
                      (funcall file-fn name)))))))
    (if (or file-fn dir-fn)
        (walk (expand-file-name dirname))
        (error "Error:you must specify at list one function"))))


(defun traverse-comp-str-to-list (str lis)
  "Compare `str' with all elements of list `lis'.
elements of list `lis' are regexps."
  (catch 'break
    (dolist (i lis)
      (when (string-match i str)
        (throw 'break t)))))

(defun traverse-check-only-lists (str lis)
  "Check if `str' match one element of `lis'."
  (if (or (member (file-name-extension str t) lis)
          (traverse-comp-str-to-list str lis))
      t
      nil))

(defsubst* traverse-find-readlines (bfile regexp &key (insert-fn 'file) (stop-at-first nil))
  "Return all the lines of a file or buffer matching `regexp'.
with the number of line in a list where each element is a list of the form:
\\(\"number_of_line\" \"line\")"
  (let* ((matched-elm)
         (fn (cond ((eq insert-fn 'file)
                    'insert-file-contents)
                   ((eq insert-fn 'buffer)
                    'insert-buffer-substring))))
    (with-temp-buffer
      (funcall fn bfile) ; call insert function
      (goto-char (point-min))
      (let ((lines-list (split-string (buffer-string) "\n")))
        (dolist (i lines-list)
          (when (string-match regexp i)
            (if stop-at-first
                (return-from traverse-find-readlines (cons (position i lines-list)
                                                           bfile))
                (push (list (position i lines-list)
                            (replace-regexp-in-string "\n" "" i))
                      matched-elm))))))
    (nreverse matched-elm)))


(defun traverse-file-process (regex fname &optional full-path insert-fn)
  "Default function to process files and insert matched lines in *traverse-lisp* buffer."
  (let ((matched-lines (traverse-find-readlines fname regex :insert-fn (or insert-fn 'file))))
    (when matched-lines 
      (dolist (i matched-lines) ;; each element is of the form '(key value)
        (let ((line-to-print (if traverse-keep-indent
                                 (second i)
                                 (replace-regexp-in-string "\\(^ *\\)" "" (second i)))))
          (and (cond ((eq insert-fn 'file)
                      (insert-button (format "[%s]" (if full-path
                                                        fname
                                                        (file-relative-name fname
                                                                            default-directory)))
                                     'action 'traverse-button-func
                                     'face "hi-green"))
                     ((eq insert-fn 'buffer)
                      (insert-button (format "[%s]" (buffer-name fname))
                                     'action 'traverse-button-func
                                     'face "hi-green")))
               (insert (concat " "
                               (int-to-string (+ (first i) 1))
                               ":"
                               (if (> (length line-to-print)
                                      traverse-length-line)
                                   (substring line-to-print
                                              0
                                              traverse-length-line)
                                   line-to-print)
                               "\n"))))))
    (setq traverse-count-occurences (+ traverse-count-occurences
                                       (length matched-lines)))))

(defun traverse-file-process-ext (regex fname &optional insert-fn)
  "Function to process files in external program like anything."
  (let ((matched-lines (traverse-find-readlines fname regex :insert-fn (or insert-fn 'file))))
    (when matched-lines
      (dolist (i matched-lines) ;; each element is of the form '(key value)
        (let ((line-to-print (if traverse-keep-indent
                                 (second i)
                                 (replace-regexp-in-string "\\(^ *\\)" "" (second i)))))
          (insert (concat (propertize (file-name-nondirectory fname)
                                      'face 'traverse-path-face
                                      'help-echo line-to-print)
                          " "
                          (propertize (int-to-string (+ (first i) 1))
                                      'face 'traverse-match-face)
                          ":"
                          (if (> (length line-to-print)
                                 traverse-length-line)
                              (substring line-to-print
                                         0
                                         traverse-length-line)
                              line-to-print)
                          "\n")))))))

(defun* traverse-buffer-process-ext (regex buffer &key (lline traverse-length-line))
  "Function to process buffer in external program like anything."
  (let ((matched-lines (traverse-find-readlines buffer regex :insert-fn 'buffer)))
    (when matched-lines 
      (dolist (i matched-lines) ;; each element is of the form '(key value)
        (let ((line-to-print (if traverse-keep-indent
                                 (second i)
                                 (replace-regexp-in-string (if (string-match "^\t" (second i))
                                                               "\\(^\t*\\)"
                                                               "\\(^ *\\)")
                                                           "" (second i)))))
          (insert (concat " "
                          (propertize (int-to-string (+ (first i) 1))
                                      'face 'traverse-match-face
                                      'help-echo line-to-print)
                          ":"
                          (if (> (length line-to-print)
                                 lline)
                              (substring line-to-print
                                         0
                                         lline)
                              line-to-print)
                          "\n")))))))

;;;###autoload
(defun traverse-find-in-file (fname regexp &optional full-path)
  "Traverse search regex in a single file."
  (interactive (list (read-file-name "FileName: ")
                     (traverse-read-regexp "Regexp: ")))
  (traverse-prepare-buffer)
  (let ((prefarg (not (null current-prefix-arg))))
    (if (and (not (bufferp fname))
             (file-regular-p fname)
             (not (file-symlink-p fname)))
        (traverse-file-process regexp fname prefarg 'file)
        (traverse-file-process regexp fname prefarg 'buffer))
    (goto-char (point-min))
    (when (re-search-forward "^Wait")
      (beginning-of-line)
      (delete-region (point) (line-end-position))
      (insert (format "Found %s occurences for %s:\n"
                      traverse-count-occurences
                      regexp))
      (message "%s Occurences found for %s"
               (propertize (int-to-string traverse-count-occurences)
                           'face 'traverse-match-face)
               (propertize regexp
                           'face 'traverse-regex-face))
      (highlight-regexp regexp) 
      (setq traverse-count-occurences 0)))
  (switch-to-buffer-other-window "*traverse-lisp*"))

(defun traverse-occur-color-current-line ()
  "Highlight and underline current position."
  (if (not traverse-occur-overlay)
      (setq traverse-occur-overlay
            (make-overlay
             (line-beginning-position) (1+ (line-end-position))))
      (move-overlay traverse-occur-overlay
                    (line-beginning-position) (1+ (line-end-position))))
  (overlay-put traverse-occur-overlay
               'face traverse-match-overlay-face))

(defun traverse-button-func (button)
  "The function called by buttons in traverse buffers."
  (let* ((list-line (split-string (thing-at-point 'line)))
         (nline (nth 1 list-line))
         (regex)
         (fname (button-label (button-at (point)))))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "for ")
        (setq regex
              (buffer-substring (point)
                                (- (line-end-position) 1)))))
    (save-excursion
      (setq fname (replace-regexp-in-string "\\[" "" fname))
      (setq fname (replace-regexp-in-string "\\]" "" fname))
      (if (bufferp (get-buffer fname))
          (switch-to-buffer-other-window (get-buffer fname))
          (find-file-other-window fname))
      (goto-line (string-to-number nline))
      (setq case-fold-search t)
      (beginning-of-line)
      (when (re-search-forward regex nil nil)
        (goto-char (- (point) (length regex)))
        (traverse-occur-color-current-line)))))


(defun traverse-prepare-buffer ()
  "Prepare a traverse buffer."
  (set-buffer (get-buffer-create "*traverse-lisp*"))
  (erase-buffer)
  (hi-lock-mode 1)
  (goto-char (point-min))
  (traversedir-mode)
  (insert " *Traverse-lisp-output*\n\n\n")
  (highlight-regexp " \\*Traverse-lisp-output\\*$" "hi-pink")
  (display-buffer "*traverse-lisp*")
  (insert  "Wait Lisp searching...\n\n")
  (sit-for 1))


(defun traverse-read-regexp (&rest args)
  "For compatibility with emacs-22.
Use `read-string' in emacs-22 instead of using `read-regexp'.
Use the same args as `read-string' or `read-regexp'
depending of what emacs version you use.
NOTE:When using `read-string' some regexp (complex)
may not be displayed correctly to traverselisp"
  (apply #'funcall (if (fboundp 'read-regexp)
                       'read-regexp
                       'read-string)
         args))

;;;###autoload
(defun traverse-occur-current-buffer (regexp)
  "Search regexp in current buffer."
  (interactive (list
                (traverse-read-regexp "Regexp: ")))
  (let ((buf-fname (buffer-file-name (current-buffer))))
    (if traverse-occur-use-miniwindow
        (progn
          (delete-other-windows)
          (split-window-horizontally traverse-miniwindow-width))
        (delete-other-windows)
        (split-window-vertically))
    (other-window 1)
    (if buf-fname
        (traverse-find-in-file buf-fname regexp)
        (traverse-find-in-file (current-buffer) regexp))))

;;;###autoload
(defun traverse-deep-rfind (tree regexp &optional only)
  "Search for regexp in all files of dirs and subdirs of current tree.
Main function that call walk, if only is omitted it
will be set as nil and search will be proceeded on all files
except on files that are in `traverse-ignore-files'
Called with prefix-argument (C-u) absolute path is displayed"
  (interactive
   (list (read-directory-name "Tree: ")
         (traverse-read-regexp "Regexp: ")
         (read-string "CheckOnly: ")))
  (traverse-prepare-buffer)
  (let ((init-time (cadr (current-time)))
        (only-list (split-string only)))
    (unwind-protect
         (traverse-walk-directory
          tree
          :file-fn #'(lambda (y)
                       (let ((prefarg (not (null current-prefix-arg))))
                         (if only-list
                             (when (traverse-check-only-lists y only-list)
                               (funcall traverse-file-function regexp y prefarg 'file))
                             (funcall traverse-file-function regexp y prefarg 'file)))
                       (message "%s [Matches] for %s in [%s]"
                                (if (>= traverse-count-occurences 1)
                                    (propertize (int-to-string traverse-count-occurences)
                                                'face 'traverse-match-face)
                                    0)
                                (propertize regexp
                                            'face 'traverse-regex-face)
                                (propertize y
                                            'face 'traverse-path-face)))
          :exclude-files (unless only-list
                           traverse-ignore-files)
          :exclude-dirs traverse-ignore-dirs)
      (setq traverse-count-occurences (if (< traverse-count-occurences 0)
                                          0
                                          traverse-count-occurences))
      (if (eq traverse-count-occurences 0)
          (progn
            (goto-char (point-min))
            (when (re-search-forward "^Wait")
              (beginning-of-line)
              (delete-region (point) (line-end-position))
              (insert "Oh!No! Nothing found!")))
          (goto-char (point-min))
          (when (re-search-forward "^Wait")
            (beginning-of-line)
            (delete-region (point) (line-end-position))
            (insert (format "Search performed in %s seconds\n\n"
                            (- (cadr (current-time)) init-time)))
            (insert (format "Found %s occurences for %s:\n"
                            traverse-count-occurences
                            regexp))))
      (message "%s Occurences found for %s in %s seconds"
               (propertize (int-to-string traverse-count-occurences)
                           'face 'traverse-match-face)
               (propertize regexp
                           'face 'traverse-regex-face)
               (- (cadr (current-time)) init-time))
      (highlight-regexp regexp) 
      (setq traverse-count-occurences 0)))
  (switch-to-buffer-other-window "*traverse-lisp*"))


;;; Dired functions
;;;###autoload
(defun traverse-search-in-dired-dir-at-point (regex &optional only)
  "Search for regexp in all files of directory at point in a dired buffer."
  (interactive (list (traverse-read-regexp "Regexp: ")
                     (read-string "CheckOnly: ")))
  (if (eq major-mode 'dired-mode)
      (let ((tree (dired-get-filename)))
        (if (file-directory-p tree)
            (traverse-deep-rfind tree regex only)
            (message "Sorry! %s is not a Directory" tree)))
      (message "Hoops! We are not in Dired!")))

;;;###autoload
(defun traverse-search-in-dired-file-at-point (regex)
  "Search for regexp in file at point in a dired buffer."
  (interactive (list (traverse-read-regexp "Regexp: ")))
  (if (eq major-mode 'dired-mode)
      (let ((fname (dired-get-filename)))
        (if (file-regular-p fname)
            (traverse-find-in-file fname regex)
            (message "Sorry! %s is not a regular file" fname)))
      (message "Hoops! We are not in Dired!")))

;;;###autoload
(defun traverse-dired-browse-archive ()
  "Open compressed archive at point in a dired buffer.
This function use AVFS and FUSE, so be sure
to have these programs and modules installed on your system."
  (interactive)
  (when traverse-use-avfs
    (let ((file-at-point (dired-get-filename)))
      (if (file-compressed-p file-at-point)
          (progn
            (when (not (cddr (directory-files traverse-avfs-default-directory)))
              (shell-command "mountavfs"))
            (find-file (concat traverse-avfs-default-directory file-at-point "#")))
          (find-file file-at-point)))))

;;;###autoload
(defun traverse-dired-search-in-archive (regexp &optional only)
  "Search for regexp in compressed archive at point in a dired buffer.
This function use AVFS and FUSE, so be sure
to have these programs installed on your system and FUSE module
enabled in your kernel.
This function is disabled by default, enable it setting
traverse-use-avfs to non--nil"
  (interactive (list (traverse-read-regexp "Regexp: ")
                     (read-string "CheckOnly: ")))
  (when traverse-use-avfs
    (let ((file-at-point (dired-get-filename)))
      (if (file-compressed-p file-at-point)          
          (progn
            (when (not (cddr (directory-files traverse-avfs-default-directory)))
              (shell-command "mountavfs"))
            (traverse-deep-rfind (concat traverse-avfs-default-directory file-at-point "#")
                                 regexp
                                 only))
          (message "That's not a compressed file")))))

;;;###autoload
(defun traverse-dired-find-in-marked-files (regexp &optional full-path)
  "Search for regexp in all marked files of a dired buffer.
if some of the marked files are directories ignore them
if no marked files use file at point."
  (interactive (list (traverse-read-regexp "Regexp: ")))
  (let ((prefarg (not (null current-prefix-arg)))
        (fname-list (traverse-dired-get-marked-files)))
    (traverse-prepare-buffer)
    (dolist (i fname-list)
      (traverse-file-process regexp i prefarg 'file))
    (goto-char (point-min))
    (when (re-search-forward "^Wait")
      (beginning-of-line)
      (delete-region (point) (line-end-position))
      (insert (format "Found %s occurences for %s:\n"
                      traverse-count-occurences
                      regexp))
      (message "%s Occurences found for %s"
               (propertize (int-to-string traverse-count-occurences)
                           'face 'traverse-match-face)
               (propertize regexp
                           'face 'traverse-regex-face))
      (highlight-regexp regexp) 
      (setq traverse-count-occurences 0)))
  (switch-to-buffer-other-window "*traverse-lisp*"))

(defun traverse-dired-find-in-all-files (regexp only &optional full-path)
  "Search for regexp in all files of current dired buffer.
except compressed files and symlinks"
  (interactive (list (traverse-read-regexp "Regexp: ")
                     (read-string "CheckOnly: ")))
  (let ((prefarg (not (null current-prefix-arg)))
        (all-files (traverse-list-directory (dired-current-directory)))
        (only-list (split-string only)))
    (traverse-prepare-buffer)
    (dolist (i all-files)
      (when (and (file-regular-p i)
                 (not (file-symlink-p i))
                 (not (file-compressed-p i))
                 (if only-list
                     (traverse-check-only-lists i only-list)
                     (not (traverse-check-only-lists i traverse-ignore-files))))
        (traverse-file-process regexp i prefarg 'file)))
    (goto-char (point-min))
    (when (re-search-forward "^Wait")
      (beginning-of-line)
      (delete-region (point) (line-end-position))
      (insert (format "Found %s occurences for %s:\n"
                      traverse-count-occurences
                      regexp))
      (message "%s Occurences found for %s"
               (propertize (int-to-string traverse-count-occurences)
                           'face 'traverse-match-face)
               (propertize regexp
                           'face 'traverse-regex-face))
      (highlight-regexp regexp) 
      (setq traverse-count-occurences 0)))
  (switch-to-buffer-other-window "*traverse-lisp*"))

(defun traverse-dired-get-marked-files (&optional strict)
  "Get a list of all marked files in a dired buffer for traverse."
  (let* ((fname-list nil)
         (all-marked (dired-get-marked-files nil nil nil t))
         (dir-marked-list (if strict
                              (if (symbolp (car all-marked))
                                  (cdr all-marked)
                                  (when (> (length all-marked) 1)
                                    all-marked))
                              (if (symbolp (car all-marked))
                                  (cdr all-marked)
                                  all-marked))))
    (dolist (i dir-marked-list)
      (when (and (not (file-directory-p i))
                 (not (file-compressed-p i)))
        (push i fname-list)))
    (nreverse fname-list)))

(defun traverse-dired-has-marked-files ()
  "Check if dired has marked files for traverse.
remove compressed files and directories from the list."
  (let ((fm-list (traverse-dired-get-marked-files)))
    (if fm-list
        t
        nil)))

;;;###autoload
(defun traverse-dired-search-regexp-in-anything-at-point (regexp &optional only)
  "Use the right function in dired depending on context.
Search in:
file at point
or
marked files
or
directory at point (recursion)
or
in compressed archive at point if traverse-use-avfs is non--nil."
  (interactive
   (let ((f-or-d-name (dired-get-filename)))
     (cond ((traverse-dired-has-marked-files)
            (list (traverse-read-regexp "Regexp: ")))
           ((or (file-directory-p f-or-d-name)
                (and (file-regular-p f-or-d-name)
                     (file-compressed-p f-or-d-name)))
            (list (traverse-read-regexp "Regexp: ")
                  (read-string "CheckOnly: "))))))
  (let ((fname (dired-get-filename)))
    (cond ((traverse-dired-has-marked-files)
           (traverse-dired-find-in-marked-files regexp))
          ((file-directory-p fname)
           (traverse-search-in-dired-dir-at-point regexp only))
          ((and (file-regular-p fname)
                (file-compressed-p fname))
           (traverse-dired-search-in-archive regexp only)))))

;;;; Navigate in traverse
(defun traverse-go-forward-or-backward (num)
  "Go to next or precedent occurence in a traverse buffer."
  (other-window -1)
  (when (buffer-file-name (current-buffer))
    (save-buffer)
    (kill-buffer (current-buffer)))
  (other-window -1)
  (forward-button num)
  (push-button)
  (other-window -1))

(defun traverse-go-forward (&optional num)
  "Go to next occurence and open file with point at the right place in other window."
  (interactive "p")
  (traverse-go-forward-or-backward (or num 1)))

(defun traverse-go-backward (&optional num)
  "Go to next occurence and open file with point at the right place in other window."
  (interactive "p")
  (traverse-go-forward-or-backward (- (or num 1))))

(defun traverse-scroll-down-other-window ()
  "Scroll other window down from a traverse buffer."
  (interactive)
  (when (equal (current-buffer)
               (get-buffer "*traverse-lisp*"))
    (scroll-other-window 1)))

(defun traverse-scroll-up-other-window ()
  "Scroll other window up from a traverse buffer."
  (interactive)
  (when (equal (current-buffer)
               (get-buffer "*traverse-lisp*"))
    (scroll-other-window -1)))

;;;; Replace functions
;;;###autoload
(defun traverse-search-and-replace (str &optional regex)
  "Replace current regexp with `str', replacement is performed only on current line."
  (interactive "sNewstring: ")
  (if (eq (current-buffer) (get-buffer "*traverse-lisp*"))
      (progn
        (let ((pos (point)))
          (goto-char (point-min))
          (when (not regex)
            (when (re-search-forward "for ")
              (setq regex
                    (buffer-substring (point)
                                      (- (line-end-position) 1)))))
          (goto-char pos)
          (if (button-at (point))
              (progn
                (save-window-excursion
                  (let ((fname (button-label (button-at (point))))
                        (flag-w nil))
                    (setq fname (replace-regexp-in-string "\\[" "" fname))
                    (setq fname (replace-regexp-in-string "\\]" "" fname))
                    (setq fname (expand-file-name fname))
                    (push-button)
                    ;; We are now in the file buffer
                    (with-current-buffer (find-buffer-visiting fname) 
                      (if (and (file-writable-p fname)
                               (not (backup-file-name-p fname)))
                          (when (re-search-forward regex)
                            (setq traverse-last-regexp (match-string 0))
                            (if (not traverse-replace-auth)
                                (if (y-or-n-p (format "Replace all occurences of [%s] with [%s]? "
                                                      (propertize traverse-last-regexp
                                                                  'face 'traverse-match-face)
                                                      (propertize str
                                                                  'face 'traverse-match-face)))
                                    (setq traverse-replace-auth t)
                                    (throw 'break
                                      (error "Operation Aborted"))))
                            
                            (let ((line (thing-at-point 'line))
                                  (new-line))
                              (delete-region (line-beginning-position)
                                             (line-end-position))
                              (setq new-line (replace-regexp-in-string regex str line))
                              (insert new-line)
                              (delete-blank-lines)
                              (save-buffer)
                              (highlight-regexp str 'hi-pink)
                              (sit-for traverse-show-regexp-delay)
                              (setq flag-w t))
                            (kill-buffer (current-buffer)))))
                    ;; We are back in traverse-buffer
                    (beginning-of-line)
                    (delete-region (point) (line-end-position))
                    (delete-blank-lines)
                    (forward-line 1)
                    (if flag-w
                        (message "<%s> Replaced by <%s> in [%s]"
                                 (propertize regex
                                             'face 'traverse-regex-face)
                                 (propertize str
                                             'face 'traverse-match-face)
                                 fname)
                        (message "Skipping: File not writable or under vc")))))
              (message "We are not on a button!"))))
      (error "You are not in a traverse-buffer, run first traverse-deep-rfind")))


;;;###autoload
(defun traverse-search-and-replace-all (str)
  "Launch search and replace interactively on all occurences found in current traverse buffer.
commands provided here are: (n)ext (a)ll (s)kip (x)stop"
  (interactive "sNewstring: ")
  (if (eq (current-buffer) (get-buffer "*traverse-lisp*"))
      (progn
        (goto-char (point-min))
        (let ((mem-srd traverse-show-regexp-delay)
              (action "")
              (count 0)
              (regex (when (re-search-forward "for ")
                       (buffer-substring (point)
                                         (- (line-end-position) 1)))))
          (unwind-protect
               (progn
                 (setq traverse-show-regexp-delay 0)
                 (when (not (button-at (point)))
                   (goto-char (point-min))
                   (forward-button 1))
                 (catch 'break
                   (while (button-at (point))
                     (catch 'continue
                       (if (eq action '?a)
                           ;; replace all without asking
                           (progn
                             (traverse-search-and-replace str regex)
                             (incf count))
                           ;; ask for next action and set it
                           (setq action (read-event (concat (propertize "Next("
                                                                        'face 'traverse-match-face)
                                                            (propertize "n "
                                                                        'face 'traverse-path-face)
                                                            (propertize ") All("
                                                                        'face 'traverse-match-face)
                                                            (propertize "a"
                                                                        'face 'traverse-path-face)
                                                            (propertize ") Skip("
                                                                        'face 'traverse-match-face)
                                                            (propertize "s"
                                                                        'face 'traverse-path-face)
                                                            (propertize ") Stop("
                                                                        'face 'traverse-match-face)
                                                            (propertize "x"
                                                                        'face 'traverse-path-face)
                                                            (propertize ") :"
                                                                        'face 'traverse-match-face))))
                           (case action
                             ('?n (progn
                                    (setq traverse-show-regexp-delay 1)
                                    (traverse-search-and-replace str regex)
                                    (incf count)
                                    (throw 'continue nil)))
                             ('?a (progn
                                    (message "Replacing all, you can %s at any time with %s"
                                             (propertize "STOP"
                                                         'face 'traverse-match-face)
                                             (propertize "<C-g>"
                                                         'face 'traverse-match-face))
                                    (setq traverse-show-regexp-delay 0.1)
                                    (sit-for 3)
                                    (traverse-search-and-replace str regex)
                                    (incf count)
                                    (throw 'continue nil)))
                             ('?s (progn
                                    (delete-region (point) (line-end-position))
                                    (delete-blank-lines)
                                    (forward-button 1)
                                    (throw 'continue nil)))
                             ('?x (progn
                                    (throw 'break nil)))
                             (t (progn
                                  (error "Unknow command, operation Aborted")
                                  (throw 'break nil)))))))))
            (if (eq action '?x)
                ;; action is stopped
                (progn
                  (setq traverse-show-regexp-delay mem-srd)
                  (message "[%s] Occurences of %s replaced by <%s>"
                           (propertize (int-to-string count)
                                       'face 'traverse-match-face)
                           (propertize regex
                                       'face 'traverse-regex-face)
                           (propertize str
                                       'face 'traverse-path-face)))
                ;; action is finish
                (setq traverse-replace-auth nil)
                (setq traverse-show-regexp-delay mem-srd)
                (when (re-search-backward "^Found")
                  (beginning-of-line)
                  (delete-region (point) (line-end-position))
                  (highlight-regexp str "hi-pink")
                  (highlight-regexp "^\\[.\\]" "hi-green")
                  (insert (format "[%s] Occurences of <%s> replaced by <%s>"
                                  count
                                  regex
                                  str))))
            (setq traverse-replace-auth nil))))
      (error "You are not in a traverse-buffer, run first traverse-deep-rfind")))

;;;; Utils

(defun file-compressed-p (fname)
  "Return t if fname is a compressed file."
  (let ((ext (file-name-extension fname)))
    (cond ((equal ext "gz")
           t)
          ((equal ext "bz2")
           t)
          ((equal ext "zip")
           t)
          (t nil))))

;;;###autoload
(defun* traverse-cp-or-mv-extfiles-in-dir (tree ext dir &optional (fn 'copy-file))
  "Recurse in `tree' and copy/move all files with `ext' in `dir'.
Default is copying, called with prefix-arg (C-u) Move files with `ext' in `Dir'.
`func' is a symbol when called non-interactively.

Note: `dir' will be use as target and NO search inside it will be performed.
If you want to move/copy files that are nested in subdir(s) of `dir'
It will fail silently.==> So use another dir target

If `dir' exists and is not empty, it will be synch with the newest files
found in `tree'"
  (interactive "DTree: \nsExt(with dot): \nGTargetDirectory: ")
  (let ((igndir (file-name-nondirectory dir)))
    (unless (file-directory-p dir)
      (make-directory dir t))
    (when current-prefix-arg
      (setq fn 'rename-file))
    (traverse-walk-directory
     tree
     :file-fn #'(lambda (x)
                  (when (equal (file-name-extension x t) ext)
                    ;; should i recurse in dir at this point ?
                    ;; would implement synch completely.
                    (if (file-exists-p (concat dir (file-name-nondirectory x)))
                        (when (file-newer-than-file-p (expand-file-name x)
                                                      (concat dir
                                                              (file-name-nondirectory x)))
                          (funcall fn (expand-file-name x) dir 'overwrite))
                        (funcall fn (expand-file-name x) dir 'overwrite))))
     :exclude-dirs `(,igndir))))


;; Experimental ==> Huge projects not supported (Tags files become to big)
;;;###autoload
(defun traverse-build-tags-in-project (dir ext &optional new-file)
  "Build an etags file at root of current project.
If `new-file' is non-nil (do it with C-u) build a new file
instead of appending to the current one.
Many file extensions can be enter at `ext' prompt.
Tag file will be build in `dir'"
  (interactive "Ddir: \nsExt: ")
  (let ((ext-list (split-string ext))
        (count 0))
    (when current-prefix-arg
      (setq new-file t))
    (when new-file
      (delete-file (expand-file-name "TAGS" dir)))
    (dolist (i ext-list)
      (traverse-walk-directory
       dir
       :file-fn #'(lambda (x)
                    (when (equal (file-name-extension x t) i)
                      (message "Tagging [%s]" (propertize x
                                                          'face 'traverse-path-face))
                      (incf count)
                      (call-process-shell-command (format "etags %s -a %s"
                                                          x
                                                          (expand-file-name "TAGS" dir)))))
       :exclude-dirs traverse-ignore-dirs))
    (message "%s Files tagged" (propertize (int-to-string count)
                                           'face 'traverse-match-face))))


(defun traverse-window-split-h-or-t ()
  "Give current split window state under symbol form.
Possible value are 'hor or 'ver"
  (cdr (assoc 'dir (bw-get-tree))))

;;;###autoload
(defun traverse-toggle-split-window-h-v ()
  "From traverse buffer toggle split window horizontally or vertically ala ediff."
  (interactive)
  (when (eq (count-windows) 2)
    (balance-windows)
    (let ((buffA (current-buffer))
          (buffB)
          (split-pos (traverse-window-split-h-or-t)))
      (save-excursion
        (other-window 1)
        (setq buffB (current-buffer))
        (delete-window))
      (if (eq split-pos 'hor)
          (split-window-vertically)
          (split-window-horizontally))
      (set-buffer (get-buffer buffB))
      (display-buffer (current-buffer)))))

;;;###autoload
(defun traverse-count-files-in-dir (tree &optional quiet)
  "Count files in `tree'.
and return a message and the number of files.
If `quiet' is non-nil don't send message."
  (interactive "DDirectory: ")
  (let ((count-files 0))
    (traverse-walk-directory
     tree
     :file-fn #'(lambda (n)
                  (when n
                    (incf count-files))))
    (unless quiet
      (message "[%s] contain <%s> files"
               (propertize tree
                           'face 'traverse-path-face)
               (propertize (number-to-string count-files)
                           'face 'traverse-match-face)))
    count-files))


(defun traverse-list-directories-in-tree (tree &optional ignore-dirs)
  "Return all directories and subdirectories of `tree'.
`ignore-dirs' is a list of directories to ignore."
  (let (list-dirs)
    (traverse-walk-directory
     tree
     :dir-fn #'(lambda (x)
                 (push x list-dirs))
     :exclude-dirs (if ignore-dirs
                       ignore-dirs))
    (nreverse list-dirs)))

(defun* traverse-list-files-in-tree (tree &optional ignore-files (ignore-dirs traverse-ignore-dirs) only-ext)
  "Return all files in `tree' without directories.
`ignore-files' is a list of files(and/or).ext to ignore.
`only-ext' will match only files with .ext or matching regexp that are in this list.
NOTE: if both `ignore-files' and `only-ext' are set, `only-ext'
will take precedence on `ignore-files'."
  (let (list-files)
    (traverse-walk-directory
     tree
     :file-fn #'(lambda (x)
                  (if only-ext
                      (if (traverse-check-only-lists x only-ext)
                          (push x list-files))
                      (push x list-files)))
     :exclude-files (unless only-ext
                      (if ignore-files
                          ignore-files))
     :exclude-dirs ignore-dirs)
    (nreverse list-files)))

(defun traverse-apply-func-on-files (tree fn &optional ext)
  "Exec `function' on all files of `tree'.
If `ext' apply func only on files with .`ext'."
  (let ((files-list (traverse-list-files-in-tree tree)))
    (dolist (i files-list)
      (if ext
          (when (equal (file-name-extension i t) ext)
            (funcall fn i))
          (funcall fn i)))))

(defun traverse-apply-func-on-dirs (tree fn &optional ignore-dirs)
  "Exec `function' on all directories of `tree'.
`ignore-dirs' is a list of directories to ignore."
  (let ((dirs-list (traverse-list-directories-in-tree tree ignore-dirs)))
    (dolist (i dirs-list)
      (funcall fn i))))


(defmacro* traverse-auto-document-lisp-buffer (&key type prefix)
  "Auto document tool for lisp code."
  `(let* ((boundary-regexp "^;;=*LIMIT.*")
          (regexp          (case ,type
                             ('nested "^ +\(def\\(un\\|subst\\|advice\\)")
                             ('command "\(def\\(un\\|subst\\)")
                             ('internal-variable "\(defvar")
                             ('user-variable "\(defcustom")
                             ('faces "\(defface")
                             ('function "\(def\\(un\\|subst\\|advice\\)")
                             (t (error "Unknow type"))))
          (fn-list         (traverse-find-readlines
                            (current-buffer)
                            regexp
                            :insert-fn 'buffer))
          beg end)
     (insert "\n") (setq beg (point))
     (save-excursion (when (re-search-forward boundary-regexp)
                       (forward-line -1) (setq end (point))))
     (delete-region beg end)
     (dolist (i fn-list)
       (let* ((elm     (cadr i))
              (elm1    (replace-regexp-in-string "\*" "" elm))
              (elm-mod (replace-regexp-in-string regexp "" elm1))
              (elm-fin (replace-regexp-in-string "\(\\|\)" ""(car (split-string elm-mod)))))
         (cond ((eq ,type 'command)
                (when (commandp (intern elm-fin))
                  (if ,prefix
                      (when (string-match ,prefix elm-fin)
                        (insert (concat ";; \`" elm-fin "\'\n")))
                      (insert (concat ";; \`" elm-fin "\'\n")))))
               ((eq ,type 'function)
                (when (not (commandp (intern elm-fin)))
                  (if ,prefix
                      (when (string-match ,prefix elm-fin)
                        (insert (concat ";; \`" elm-fin "\'\n")))
                      (insert (concat ";; \`" elm-fin "\'\n")))))
               ((eq ,type 'internal-variable)
                (if ,prefix
                    (when (string-match ,prefix elm-fin)
                      (insert (concat ";; \`" elm-fin "\'\n")))
                    (insert (concat ";; \`" elm-fin "\'\n"))))
               ((eq ,type 'user-variable)
                (if ,prefix
                    (when (string-match ,prefix elm-fin)
                      (insert (concat ";; \`" elm-fin "\'\n")))
                    (insert (concat ";; \`" elm-fin "\'\n"))))
               ((eq ,type 'faces)
                (if ,prefix
                    (when (string-match ,prefix elm-fin)
                      (insert (concat ";; \`" elm-fin "\'\n")))
                    (insert (concat ";; \`" elm-fin "\'\n"))))
               ((eq ,type 'nested)
                (if ,prefix
                    (when (string-match ,prefix elm-fin)
                      (insert (concat ";; \`" elm-fin "\'\n")))
                    (insert (concat ";; \`" elm-fin "\'\n"))))
               (t
                (insert (concat ";; \`" elm-fin "\'\n"))))))))


;; TODO use align-regexp here that is now part of emacs.
;;;###autoload
(defun traverse-pprint-tree (tree)
  "PPrint the content of `tree'.
NOTE: for a better output, be sure to have
http://www.emacswiki.org/emacs/align.el
installed on your emacs even if that work without."
  (interactive "DTree: ")
  (switch-to-buffer "*traverse-ls*")
  (erase-buffer)
  (let ((dirs-list (traverse-list-directories-in-tree tree))
        (beg)
        (end))
    (loop for i in dirs-list
       do
         (insert (propertize (format "%s\n" i)
                             'face 'traverse-match-face))
         (setq beg (point))
         (loop for x in (traverse-list-directory i t) 
            do
              (let* ((attr (file-attributes x))
                     (time-stamp (format-time-string "%Y-%m-%d %H:%M" (nth 6 attr)))
                     (size (int-to-string (nth 7 attr)))
                     (mode (nth 8 attr)))
                (if (file-directory-p x)
                    (insert (propertize (format "    %s %s %s %s\n"
                                                mode
                                                size
                                                time-stamp
                                                (file-name-nondirectory x))
                                        'face 'traverse-regex-face))
                    (insert (propertize (format "    %s %s %s %s\n"
                                                mode
                                                size
                                                time-stamp
                                                (file-name-nondirectory x))
                                        'face 'traverse-path-face)))))
         (setq end (point))
         (unless (eq beg end)
           (if (fboundp 'align-cols)
               (align-cols beg end 4)))))
  (goto-char (point-min))
  (traversedir-mode))

(provide 'traverselisp)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (emacswiki-post "traverselisp.el")
;;; traverselisp.el ends here


