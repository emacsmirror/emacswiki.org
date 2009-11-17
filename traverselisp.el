;;; traverselisp.el --- walk through directories and perform actions on files.

;; Copyright (C) 2008, 2009 Thierry Volpiatto
;; Author:     Thierry Volpiatto 
;; Maintainer: Thierry Volpiatto
;; Keywords:   data, regexp

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
;;  ==========

;; Developped and tested on:
;; GNU Emacs 23.1.50.1 (i686-pc-linux-gnu, GTK+ Version 2.18.3) of 2009-11-07 on tux

;; Compatibility: Emacs23.*
;; =============

;; Install:
;; =======
;;
;; Put this file in your load-path And Byte-compile it.
;; (If you don't do that you will have error as traverse
;; use code that work only at compile time.)
;;
;; Add to your .emacs:
;;
;; (require 'traverselisp)
;;
;; Set up your prefered keys for dired and globals as usual.
;;
;; Here is my config with version-1.1.31:
;; =====================================
;;
;; (require 'traverselisp)
;; (setq traverse-use-avfs t)
;; (global-set-key (kbd "<f5> f") 'traverse-deep-rfind)
;; (global-set-key (kbd "C-c o") 'traverse-incremental-occur)
;; (define-key dired-mode-map (kbd "A") 'traverse-dired-search-regexp-in-anything-at-point)
;; (define-key dired-mode-map (kbd "C-c C-z") 'traverse-dired-browse-archive)
;; (define-key dired-mode-map (kbd "C-c t") 'traverse-dired-find-in-all-files)
;; (mapc #'(lambda (x)
;;           (add-to-list 'traverse-ignore-files x))
;;       '(".ledger-cache"  "ANYTHING-TAG-FILE"))
;; (add-to-list 'traverse-ignore-dirs "emacs_backup")
;; (global-set-key (kbd "C-c C-f") 'anything-traverse)
;; (global-set-key (kbd "C-M-|") 'traverse-toggle-split-window-h-v)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Traverse auto documentation
;;  ---------------------------
;;
;;  [UPDATE ALL EVAL] (traverse-auto-update-documentation)
;;
;;  * Commands defined here:
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
;; `traverse-toggle-split-window-h-v'
;; `traverse-count-files-in-dir'
;; `traverse-auto-update-documentation'
;; `traverse-auto-documentation-insert-header'
;; `traverse-incremental-next-line'
;; `traverse-incremental-precedent-line'
;; `traverse-incremental-jump-and-quit'
;; `traverse-incremental-scroll-down'
;; `traverse-incremental-scroll-up'
;; `traverse-incremental-occur'

;;  * Non--interactive functions defined here:
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
;; `traverse-goto-line'
;; `traverse-incremental-forward-line'
;; `traverse-incremental-jump'
;; `traverse-incremental-scroll'
;; `traverse-read-char-or-event'
;; `traverse-incremental-read-search-input'
;; `traverse-incremental-filter-alist-by-regexp'
;; `traverse-incremental-start-timer'
;; `traverse-incremental-cancel-search'
;; `traverse-incremental-occur-color-current-line'

;;  * Macros defined here:
;; [EVAL] (traverse-auto-document-lisp-buffer :type 'macro :prefix "traverse")
;; `traverse-collect-files-in-tree-if'
;; `traverse-collect-files-in-tree-if-not'
;; `traverse-auto-document-lisp-buffer'

;;  * Internal variables defined here:
;; [EVAL] (traverse-auto-document-lisp-buffer :type 'internal-variable :prefix "traverse")
;; `traversedir-mode-map'
;; `traverse-match-overlay-face'
;; `traverse-show-regexp-delay'
;; `traverse-keep-indent'
;; `traverse-occur-use-miniwindow'
;; `traverse-miniwindow-width'
;; `traverse-count-occurences'
;; `traverse-occur-overlay'
;; `traverse-incremental-mode-map'
;; `traverse-incremental-search-pattern'
;; `traverse-incremental-search-timer'
;; `traverse-incremental-quit-flag'
;; `traverse-incremental-current-buffer'
;; `traverse-incremental-occur-overlay'
;; `traverse-incremental-read-fn'
;; `traverse-incremental-face'

;;  * Faces defined here:
;; [EVAL] (traverse-auto-document-lisp-buffer :type 'faces :prefix "traverse")
;; `traverse-match-face'
;; `traverse-regex-face'
;; `traverse-path-face'
;; `traverse-overlay-face'
;; `traverse-incremental-overlay-face'

;;  * User variables defined here:
;; [EVAL] (traverse-auto-document-lisp-buffer :type 'user-variable :prefix "^traverse")
;; `traverse-ignore-files'
;; `traverse-ignore-dirs'
;; `traverse-length-line'
;; `traverse-file-function'
;; `traverse-use-avfs'
;; `traverse-avfs-default-directory'
;; `traverse-incremental-search-delay'
;; `traverse-incremental-search-prompt'
;; `traverse-incremental-length-line'

;;  *** END auto-documentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Usage:
;; =====
;;
;; M-x `traverse-deep-rfind'
;; That is the interactive recursive function of traverse.
;; It will search for a regexp in all files of a directory
;; tree and his subdirectories.
;;
;; When searching is done and you are in traverse buffer
;; some interactive actions are provided for navigate
;; Use "C-h m" for more info while in traverse-buffer.
;;
;; You can also use traverse from DIRED:
;; M-x `traverse-dired-search-regexp-in-anything-at-point'
;;
;; This function work on directory, files, (1)compressed files (AVFS)
;; and marked files:
;;
;; If you have marked files search will be performed only on these files.
;; (No recursion will be performed on marked directories, so don't mark directories.)
;;
;; If no files are marked, traverse will search in element at point
;; with appropriate function.
;;
;; However, you can use specialized functions, check this file to see all
;; the interactives functions.
;;
;; M-x `traverse-dired-find-in-all-files'
;; Search in all regular files in the current dired buffer (no recursion).
;;
;; M-x `traverse-occur-current-buffer'
;; Just like occur but with traverse engine.
;;
;; M-x `traverse-incremental-occur'
;; occur current buffer incrementally.(C-u to have thing-at-point as default prompt).
;;
;; M-x `traverse-dired-browse-archive'
;; This function use (1)AVFS to browse archive tar.gz, bz2 etc..
;;
;; (1)NOTE: You have to install AVFS and enable fuse in your kernel if
;; you want to browse and search in archives.
;; Please see the doc of your DISTRIB and the doc of AVFS here:
;; http://sourceforge.net/projects/avf
;;
;; If you want to use AVFS in traverse, set `traverse-use-avfs' to non--nil.
;;
;; Traverse provide also diverses functions to use in your programs.
;; (especially for recursion like `traverse-walk-directory'.)
;;
;; You will find also some functions to auto document list of functions,
;; macros, commands, etc..., see headers above.
;;
;; Contact: thierry dot volpiatto hat gmail dot com
;; =======
;;
;; You can get the developpement version of the file here with hg:
;;
;; hg clone http://mercurial.intuxication.org/hg/traverselisp
;; For the current developpement branch:
;; hg update -C 1.1.0
;; or with DVC ==> C-u C-u M-x xhg-update RET 1.1.0 (or last rev number)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;;  http://mercurial.intuxication.org/hg/traverselisp
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Version:
(defconst traverse-version "1.1.43")

;;; Code:

(require 'derived)
(eval-when-compile (require 'cl))


(defvar traversedir-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?q] 'traverse-quit)
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
(defface traverse-overlay-face '((t (:background "Indianred4" :underline t)))
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
  "Walk through DIRNAME and use FILE-FN and/or DIR-FN function on each file/dir found.

DIRNAME ==> we start in this directory

Use keys to set args:

You must specify at list one of these 2 functions:
:FILE-FN ==> function to apply to FILES
:DIR-FN ==> function to apply on DIRECTORIES

Files or/and directories in these lists will be skipped:
:EXCLUDES-FILES ==> list of .ext or files to ignore.  
:EXCLUDE-DIRS ==> list of directory to ignore.

Example of use:

 (traverse-walk-directory \"~/foo\" :file-fn #'(lambda (x) (princ x) (terpri)))

See `traverse-ignore-files' and `traverse-ignore-dirs'."
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
  "Compare STR with all elements of list LIS.
All the elements of list LIS are regexps issued from prompt.
Each element of LIS is compared with the filename STR."
  (catch 'break
    (dolist (i lis)
      (when (string-match i str)
        (throw 'break t)))))

(defun traverse-check-only-lists (str lis)
  "Check if STR match one element of LIS."
  (or (member (file-name-extension str t) lis)
      (traverse-comp-str-to-list str lis)))

(defsubst* traverse-find-readlines (bfile regexp &key (insert-fn 'file))
  "Return an alist of all the (num-line line) of a file or buffer BFILE matching REGEXP."
  (let ((count 0)
        (fn    (case insert-fn
                 ('file 'insert-file-contents)
                 ('buffer 'insert-buffer-substring))))
    (with-temp-buffer
      (funcall fn bfile) ; call insert function
      (goto-char (point-min))
      (loop
         with lines-list = (split-string (buffer-string) "\n")
         for i in lines-list when (string-match regexp i)
         collect (list count (replace-regexp-in-string "\n" "" i)) into lis
         do (incf count)
         finally return lis))))


(defun traverse-file-process (regex fname &optional full-path insert-fn)
  "Default function to process files and insert matched lines in *traverse-lisp* buffer."
  (let ((matched-lines (traverse-find-readlines fname regex :insert-fn (or insert-fn 'file))))
    (when matched-lines 
      (dolist (i matched-lines) ;; each element is of the form '(key value)
        (let ((line-to-print (if traverse-keep-indent
                                 (second i)
                                 (replace-regexp-in-string "\\(^ *\\)" "" (second i)))))
          (and (cond ((eq insert-fn 'file)
                      (insert-button (format "[%s]"
                                             (if full-path
                                                 fname
                                                 (file-relative-name fname default-directory)))
                                     'action 'traverse-button-func
                                     'face "hi-green"))
                     ((eq insert-fn 'buffer)
                      (insert-button (format "[%s]" (buffer-name fname))
                                     'action 'traverse-button-func
                                     'face "hi-green")))
               (insert (concat " " (int-to-string (+ (first i) 1))
                               ":"
                               (if (> (length line-to-print) traverse-length-line)
                                   (substring line-to-print 0 traverse-length-line)
                                   line-to-print)
                               "\n"))))))
    (setq traverse-count-occurences (+ traverse-count-occurences (length matched-lines)))))

(defun* traverse-file-process-ext (regex fname &key (lline traverse-length-line))
  "Function to process files in external program like anything."
  (let ((matched-lines (traverse-find-readlines fname regex :insert-fn 'file)))
    (when matched-lines
      (dolist (i matched-lines) ;; each element is of the form '(key value)
        (let* ((ltp           (second i))
               (replace-reg   (if (string-match "^\t" ltp) "\\(^\t*\\)" "\\(^ *\\)"))
               (new-ltp       (replace-regexp-in-string replace-reg "" ltp))
               (line-to-print (if traverse-keep-indent ltp new-ltp)))
          (insert (concat (propertize (file-name-nondirectory fname)
                                      'face 'traverse-path-face
                                      'help-echo line-to-print)
                          " "
                          (propertize (int-to-string (+ (first i) 1))
                                      'face 'traverse-match-face)
                          ":"
                          (if (> (length line-to-print) lline)
                              (substring line-to-print 0 lline)
                              line-to-print)
                          "\n")))))))

(defun* traverse-buffer-process-ext (regex buffer &key (lline traverse-length-line))
  "Function to process buffer in external program like anything."
  (let ((matched-lines (traverse-find-readlines buffer regex :insert-fn 'buffer)))
    (when matched-lines 
      (dolist (i matched-lines) ;; each element is of the form '(key value)
        (let* ((ltp           (second i))
               (replace-reg   (if (string-match "^\t" ltp) "\\(^\t*\\)" "\\(^ *\\)"))
               (new-ltp       (replace-regexp-in-string replace-reg "" ltp)) 
               (line-to-print (if traverse-keep-indent ltp new-ltp)))
          (insert (concat " " (propertize (int-to-string (+ (first i) 1))
                                          'face 'traverse-match-face
                                          'help-echo line-to-print)
                          ":"
                          (if (> (length line-to-print) lline)
                              (substring line-to-print 0 lline)
                              line-to-print)
                          "\n")))))))

;;;###autoload
(defun traverse-find-in-file (fname regexp &optional full-path)
  "Traverse search regex in a single file."
  (interactive (list (read-file-name "FileName: ")
                     (traverse-read-regexp (if (fboundp 'read-regexp) "Regexp" "Regexp: "))))
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
         (nline     (nth 1 list-line))
         regex
         (fname     (button-label (button-at (point)))))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "for ")
        (setq regex
              (buffer-substring (point) (- (line-end-position) 1)))))
    (save-excursion
      (setq fname (replace-regexp-in-string "\\[" "" fname))
      (setq fname (replace-regexp-in-string "\\]" "" fname))
      (if (bufferp (get-buffer fname))
          (switch-to-buffer-other-window (get-buffer fname))
          (find-file-other-window fname))
      (traverse-goto-line (string-to-number nline))
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
  (apply #'funcall (if (fboundp 'read-regexp) 'read-regexp 'read-string)
         args))

;;;###autoload
(defun traverse-occur-current-buffer (regexp)
  "Search regexp in current buffer."
  (interactive
   (list (traverse-read-regexp (if (fboundp 'read-regexp) "Regexp" "Regexp: "))))
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
         (traverse-read-regexp (if (fboundp 'read-regexp) "Regexp" "Regexp: "))
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
  (interactive (list (traverse-read-regexp (if (fboundp 'read-regexp) "Regexp" "Regexp: "))
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
  (interactive (list (traverse-read-regexp (if (fboundp 'read-regexp) "Regexp" "Regexp: "))))
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
  (interactive (list (traverse-read-regexp (if (fboundp 'read-regexp) "Regexp" "Regexp: "))
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
  (interactive (list (traverse-read-regexp (if (fboundp 'read-regexp) "Regexp" "Regexp: "))))
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
  (interactive (list (traverse-read-regexp (if (fboundp 'read-regexp) "Regexp" "Regexp: "))
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
            (list (traverse-read-regexp (if (fboundp 'read-regexp) "Regexp" "Regexp: "))))
           ((or (file-directory-p f-or-d-name)
                (and (file-regular-p f-or-d-name)
                     (file-compressed-p f-or-d-name)))
            (list (traverse-read-regexp (if (fboundp 'read-regexp) "Regexp" "Regexp: "))
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


;;;; Utils

(defun file-compressed-p (fname)
  "Return t if FNAME is a compressed file."
  (let ((ext (file-name-extension fname)))
    (case ext
      ("gz"  t)
      ("bz2" t)
      ("zip" t)
      (t     nil))))


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
  "Count files in TREE.
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
  "Return all directories and subdirectories of TREE.
IGNORE-DIRS is a list of directories to ignore."
  (let (list-dirs)
    (traverse-walk-directory
     tree
     :dir-fn #'(lambda (x)
                 (push x list-dirs))
     :exclude-dirs (if ignore-dirs
                       ignore-dirs))
    (nreverse list-dirs)))

(defun* traverse-list-files-in-tree (tree
                                     &optional
                                     (ignore-files traverse-ignore-files)
                                     (ignore-dirs traverse-ignore-dirs)
                                     only-ext)
  "Return all files in TREE without directories.
IGNORE-FILES is a list of files(and/or).ext to ignore.
ONLY-EXT will match only files with .ext or matching regexp that are in this list.
NOTE: if both IGNORE-FILES and ONLY-EXT' are set, ONLY-EXT will take precedence on IGNORE-FILES."
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


(defmacro traverse-collect-files-in-tree-if (tree pred)
  "Return a list of files matching PRED in TREE.
PRED is a function that take one arg."
  `(let ((flist ()))
     (traverse-walk-directory
      ,tree
      :file-fn #'(lambda (x) (when (funcall ,pred x) (push x flist))))
     flist))

(defmacro traverse-collect-files-in-tree-if-not (tree pred)
  "Return a list of files not matching PRED in TREE.
PRED is a function that take one arg."
  `(let ((flist ()))
     (traverse-walk-directory
      ,tree
      :file-fn #'(lambda (x) (unless (funcall ,pred x) (push x flist))))
     flist))

(defmacro* traverse-auto-document-lisp-buffer (&key type prefix)
  "Auto document tool for lisp code.
TYPE can be one of:
    - command           
    - nested-command    
    - function          
    - nested-function   
    - macro             
    - internal-variable 
    - nested-variable   
    - user-variable     
    - faces             
    - anything-source
PREFIX let you define a special name (e.g match only function with PREFIX \"^traverse-\")

Example: (traverse-auto-document-lisp-buffer :type 'function :prefix \"traverse\").

It's better to use `traverse-auto-documentation-insert-header' to setup your headers.
Don't forget to add this line at the end of your traverse-auto-documentation:

    ;;  *** END auto-documentation

See headers of traverselisp.el for example."
  `(let* ((boundary-regexp "^;; +\\*+ .*")
         (regexp          (case ,type
                            ('command           "^\(def\\(un\\|subst\\)")
                            ('nested-command    "^ +\(def\\(un\\|subst\\)")
                            ('function          "^\(def\\(un\\|subst\\|advice\\)")
                            ('nested-function   "^ +\(def\\(un\\|subst\\|advice\\)")
                            ('macro             "^\(defmacro")
                            ('internal-variable "^\(defvar")
                            ('nested-variable   "^ +\(defvar")
                            ('user-variable     "\(defcustom")
                            ('faces             "\(defface")
                            ('anything-source   "^\(defvar anything-c-source")
                            (t (error           "Unknow type"))))
         (fn-list         (traverse-find-readlines
                           (current-buffer)
                           regexp
                           :insert-fn 'buffer))
         beg end)
    (flet ((maybe-insert-with-prefix (name)
             (if ,prefix
                 (when (string-match ,prefix name)
                   (insert (concat ";; \`" name "\'\n")))
                 (insert (concat ";; \`" name "\'\n")))))
      (insert "\n") (setq beg (point))
      (save-excursion (when (re-search-forward boundary-regexp)
                        (forward-line -1) (setq end (point))))
      (delete-region beg end)
      (when (eq ,type 'anything-source) (setq regexp "\(defvar"))
      (dolist (i fn-list)
        (let* ((elm     (cadr i))
               (elm1    (replace-regexp-in-string "\*" "" elm))
               (elm-mod (replace-regexp-in-string regexp "" elm1))
               (elm-fin (replace-regexp-in-string "\(\\|\)" ""(car (split-string elm-mod)))))
          (case ,type
            ('command
             (when (commandp (intern elm-fin))
               (maybe-insert-with-prefix elm-fin)))
            ('nested-command
             (when (commandp (intern elm-fin))
               (maybe-insert-with-prefix elm-fin)))
            ('function
             (when (not (commandp (intern elm-fin)))
               (maybe-insert-with-prefix elm-fin)))
            ('nested-function
             (when (not (commandp (intern elm-fin)))
               (maybe-insert-with-prefix elm-fin)))
            ('internal-variable
             (unless (string-match "anything-c-source" elm-fin)
               (maybe-insert-with-prefix elm-fin)))
            (t
             (maybe-insert-with-prefix elm-fin))))))))

;;;###autoload
(defun traverse-auto-update-documentation ()
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward "^;; +\\[EVAL\\]" nil t)
    (end-of-line) (eval-last-sexp t)
    (while (not (bolp)) (delete-char -1))))

;;;###autoload
(defun traverse-auto-documentation-insert-header (title &optional nstar)
  (interactive "sTitle: \np")
  (let ((ttype (completing-read "Type: " '("command " "nested-command "
                                           "function " "nested-function "
                                           "macro " "internal-variable "
                                           "nested-variable " "faces "
                                           "anything-source ") nil t)))
    (insert (concat ";;  " (make-string nstar ?*) " " title "\n"
                    ";; [EVAL] (traverse-auto-document-lisp-buffer :type \'" ttype ":prefix \"\")"))))


;;; Incremental occur

(defvar traverse-incremental-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'traverse-quit)
    (define-key map (kbd "RET") 'traverse-incremental-jump-and-quit)
    (define-key map (kbd "<S-down>") 'traverse-incremental-scroll-down)
    (define-key map (kbd "<S-up>") 'traverse-incremental-scroll-up)
    (define-key map (kbd "<down>") 'traverse-incremental-next-line)
    (define-key map (kbd "<up>") 'traverse-incremental-precedent-line)
    (define-key map (kbd "C-n") 'traverse-incremental-next-line)
    (define-key map (kbd "C-p") 'traverse-incremental-precedent-line)
    map)
  "Keymap used for traversedir commands.")

(define-derived-mode traverse-incremental-mode text-mode "traverse-incremental"
                     "Major mode to search occurences of regexp in current buffer.

Special commands:
\\{traverse-incremental-mode-map}")


(defcustom traverse-incremental-search-delay 0.2
  "*During incremental searching display is updated all `traverse-incremental-search-delay' seconds."
  :group 'traversedir
  :type  'integer)

(defcustom traverse-incremental-search-prompt "Pattern: "
  "*Prompt used for `traverse-incremental-occur'."
  :group 'traversedir
  :type  'string)

(defcustom traverse-incremental-length-line 80
  "*Length of the line dispalyed in traverse incremental buffer."
  :group 'traversedir
  :type 'integer)

;;; Internal variables
(defvar traverse-incremental-search-pattern "")
(defvar traverse-incremental-search-timer nil)
(defvar traverse-incremental-quit-flag nil)
(defvar traverse-incremental-current-buffer nil)
(defvar traverse-incremental-occur-overlay nil)
(defvar traverse-incremental-read-fn
  (if (fboundp 'read-key) 'read-key 'traverse-read-char-or-event))
(defvar traverse-incremental-exit-and-quit-p nil)

(defun traverse-goto-line (numline)
  "Non--interactive version of `goto-line.'"
  (goto-char (point-min)) (forward-line (1- numline)))

(defun traverse-incremental-forward-line (n)
  "Forward line only if it is not an empty line."
  (let (pos)
    (save-excursion
      (forward-line n) (forward-line 0)
      (when (looking-at "^ [0-9]+") (forward-line 0) (setq pos (point)))) 
  (when pos (goto-char pos) (traverse-incremental-occur-color-current-line))))

;;;###autoload
(defun traverse-incremental-next-line ()
  "Goto next line if it is not an empty line."
  (interactive)
  (traverse-incremental-forward-line 1))

;;;###autoload
(defun traverse-incremental-precedent-line ()
  "Goto precedent line if it is not an empty line."
  (interactive)
  (traverse-incremental-forward-line -1))

(defun traverse-incremental-jump ()
  "Jump to line in other buffer and put an overlay on it."
  (let ((line (buffer-substring (point-at-bol) (point-at-eol)))
        pos)
    (unless (or (string= line "")
                (string= line "Traverse Incremental occur"))
      (when (string-match "[0-9]+" line)
        (setq pos (string-to-number (match-string 0 line))))
      (pop-to-buffer traverse-incremental-current-buffer)
      (traverse-goto-line pos) (traverse-occur-color-current-line))))
      

;;;###autoload
(defun traverse-incremental-jump-and-quit ()
  "Jump to line in other buffer and quit search buffer."
  (interactive)
  (when (traverse-incremental-jump)
    (delete-other-windows)
    (sit-for 0.3)
    (when traverse-occur-overlay
      (delete-overlay traverse-occur-overlay))))
    
(defun traverse-incremental-scroll (n)
  "Scroll other buffer and move overlay accordingly."
  (traverse-incremental-forward-line n)
  (traverse-incremental-occur-color-current-line)
  (when (traverse-incremental-jump)
    (other-window 1)))

;;;###autoload
(defun traverse-incremental-scroll-down ()
  "Scroll other buffer down."
  (interactive)
  (traverse-incremental-scroll 1))

;;;###autoload
(defun traverse-incremental-scroll-up ()
  "Scroll other buffer up."
  (interactive)
  (traverse-incremental-scroll -1))

(defun traverse-read-char-or-event (prompt)
  "Use `read-char' to read keyboard input, if input is not a char use `read-event' instead."
  (let* ((chr (condition-case nil (read-char prompt) (error nil)))
         (evt (unless chr (read-event))))
    (or chr evt)))

(defun traverse-incremental-read-search-input (initial-input)
  "Read each keyboard input and add it to `traverse-incremental-search-pattern'."
  (let* ((prompt       (propertize traverse-incremental-search-prompt 'face '((:foreground "cyan"))))
         (doc          "     [RET:exit, C-g:quit, C-z:Jump, C-j:Jump&quit, C-n/p:next/prec-line]")
         (inhibit-quit t)
         (tmp-list     ())
         char)
    (unless (string= initial-input "")
      (loop for char across initial-input
         do (push (text-char-description char) tmp-list)))
    (setq traverse-incremental-search-pattern initial-input)
    (catch 'break
      (while 1
        (catch 'continue
          (setq char (funcall traverse-incremental-read-fn
                      (concat prompt traverse-incremental-search-pattern doc)))
          (case char
            ((or down ?\C-n) ; Next line
             (when traverse-incremental-search-timer
               (traverse-incremental-cancel-search))
             (traverse-incremental-next-line)
             (traverse-incremental-occur-color-current-line)
             (throw 'continue nil)) ; Fix me: Is it needed?
            ((or up ?\C-p) ; precedent line
             (when traverse-incremental-search-timer
               (traverse-incremental-cancel-search))
             (traverse-incremental-precedent-line)
             (traverse-incremental-occur-color-current-line)
             (throw 'continue nil)) ; Fix me: Is it needed?
            ((or ?\e ?\r) ; RET or ESC break and exit code.
             (throw 'break (message "Incremental Search completed")))    
            (?\d ; Delete last char of `traverse-incremental-search-pattern' with DEL.
             (unless traverse-incremental-search-timer
               (traverse-incremental-start-timer))
             (pop tmp-list)         
             (setq traverse-incremental-search-pattern (mapconcat 'identity (reverse tmp-list) ""))
             (throw 'continue nil))
            (?\C-g ; Quit and restore buffers.
             (setq traverse-incremental-quit-flag t) (throw 'break (message "Quit")))
            ((or right ?\C-z) ; persistent action
             (traverse-incremental-jump) (other-window 1))
            ((or left ?\C-j)
             (setq traverse-incremental-exit-and-quit-p t)
             (throw 'break (message "Incremental Search completed")))
            (?\C-v ; Scroll down
             (scroll-other-window 1))
            (?\M-v ; Scroll up
             (scroll-other-window -1))
            (t
             (unless traverse-incremental-search-timer
               (traverse-incremental-start-timer))
             (condition-case nil ; If keyboard input is an event not listed above we exit.
                 (progn
                   (push (text-char-description char) tmp-list)
                   (setq traverse-incremental-search-pattern (mapconcat 'identity (reverse tmp-list) ""))
                   (throw 'continue nil))
               (error (throw 'break nil))))))))))


(defun traverse-incremental-filter-alist-by-regexp (regexp buffer-name)
  "Print all lines matching REGEXP in current buffer to buffer BUFFER-NAME."
  (let ((title (propertize "Traverse Incremental occur" 'face '((:background "Dodgerblue4")))))
    (if (string= regexp "")
        (progn (erase-buffer) (insert (concat title "\n\n")))
        (erase-buffer) (insert (concat title "\n\n"))
        (traverse-buffer-process-ext regexp buffer-name :lline traverse-incremental-length-line)
        (goto-char (point-min)) (forward-line 2)
        (traverse-incremental-occur-color-current-line))))
        

(defun traverse-incremental-start-timer ()
  "Start traverse incremental timer and set it to `traverse-incremental-search-timer'."
  (setq traverse-incremental-search-timer
        (run-with-idle-timer
         traverse-incremental-search-delay 'repeat
         #'(lambda ()
             (traverse-incremental-filter-alist-by-regexp
              traverse-incremental-search-pattern
              traverse-incremental-current-buffer)))))

;;;###autoload
(defun traverse-incremental-occur (&optional initial-input)
  "Incremental search of lines in current buffer matching `traverse-incremental-search-pattern'.
With a prefix arg search symbol at point.
While you are incremental searching, commands provided are:
C-n or <down>:  next line.
C-p or <up>:    precedent line.
C-v and M-v:    scroll up and down.
C-z or <right>: jump without quitting loop.
C-j or <left>:  jump and exit search buffer.
RET or ESC:     exit but don't quit search buffer.
DEL:            remove last character entered.
C-g:            quit and restore buffer.
When you quit incremental search with RET or ESC, see `traverse-incremental-mode'
for commands provided in the search buffer."
  (interactive "P")
  (setq traverse-incremental-exit-and-quit-p nil)
  (setq traverse-incremental-current-buffer (buffer-name (current-buffer)))
  (with-current-buffer traverse-incremental-current-buffer
    (jit-lock-fontify-now))
  (let* ((init-str (if initial-input (thing-at-point 'symbol) ""))
         (len      (length init-str))
         (curpos      (point))
         str-no-prop)
    (set-text-properties 0 len nil init-str)
    (setq str-no-prop init-str)
    (pop-to-buffer (get-buffer-create "*traverse search*"))
    (traverse-incremental-mode)
    (unwind-protect
         (progn
           (traverse-incremental-start-timer)
           (traverse-incremental-read-search-input str-no-prop))
      (progn
        (traverse-incremental-cancel-search)
        (when (equal (buffer-substring (point-at-bol) (point-at-eol)) "")
          (setq traverse-incremental-quit-flag t))
        (if traverse-incremental-quit-flag
            (progn
              (kill-buffer "*traverse search*")
              (switch-to-buffer traverse-incremental-current-buffer)
              (when traverse-occur-overlay
                (delete-overlay traverse-occur-overlay))
              (delete-other-windows) (goto-char curpos))
            (if traverse-incremental-exit-and-quit-p
                (traverse-incremental-jump-and-quit)
                (traverse-incremental-jump) (other-window 1)))
        (setq traverse-incremental-quit-flag nil)))))

(defun traverse-incremental-cancel-search ()
  "Cancel timer used for traverse incremental searching."
  (when traverse-incremental-search-timer
    (cancel-timer traverse-incremental-search-timer)
    (setq traverse-incremental-search-timer nil)))

(defface traverse-incremental-overlay-face '((t (:background "Green4" :underline t)))
  "Face for highlight line in matched buffer."
  :group 'traverse-faces)

(defvar traverse-incremental-face 'traverse-incremental-overlay-face)

;; TODO Make one generic overlay function for all traverse.
(defun traverse-incremental-occur-color-current-line ()
  "Highlight and underline current position."
  (if (not traverse-incremental-occur-overlay)
      (setq traverse-incremental-occur-overlay
            (make-overlay
             (line-beginning-position) (1+ (line-end-position))))
      (move-overlay traverse-incremental-occur-overlay
                    (line-beginning-position) (1+ (line-end-position))))
  (overlay-put traverse-incremental-occur-overlay
               'face traverse-incremental-face))

;; Provide
(provide 'traverselisp)

;;; traverselisp.el ends here


