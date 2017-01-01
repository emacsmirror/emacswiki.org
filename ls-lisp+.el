;;; ls-lisp+.el --- Enhancements of standard library `ls-lisp.el'.
;;
;; Filename: ls-lisp+.el
;; Description: Enhancements of standard library `ls-lisp.el'.
;; Author: Drew Adams
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 2008-2017, Drew Adams, all rights reserved.
;; Created: Fri Feb 29 10:54:37 2008 (Pacific Standard Time)
;; Version: 0
;; Package-Requires: ((files+ "0"))
;; Last-Updated: Sun Jan  1 10:43:26 2017 (-0800)
;;           By: dradams
;;     Update #: 242
;; URL: http://www.emacswiki.org/ls-lisp+.el
;; Doc URL: http://emacswiki.org/LsLisp
;; Keywords: internal, extensions, local, files, dired
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x, 24.x, 25.x
;;
;; Features that might be required by this library:
;;
;;   `files+', `misc-fns', `strings', `thingatpt', `thingatpt+'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Enhancements of standard library `ls-lisp.el'.
;;
;;  If you use MS Windows, MS-DOS, or MacOS, then you will likely want
;;  to use library `ls-lisp.el' plus this library, to use an Emacs
;;  Lisp only definition of `insert-directory'.
;;
;;  `ls-lisp+.el' loads libraries `ls-lisp.el' and `files+.el'.  Both
;;  `files+.el' and `ls-lisp+.el' redefine `insert-directory' so that
;;  the second header line includes the number of files and
;;  directories in the directory.  Files `.' and `..' are excluded
;;  from the count, but all other directories listed are included.
;;
;;  The second header line thus becomes this, in Emacs 22:
;;
;;    files 276 space used  27359 available 56238272
;;
;;  or this, in Emacs 20 and 21:
;;
;;    files 276 total 27359
;;
;;  This library also lets you use wildcards in the file names in an
;;  explicit cons arg to `dired'.  It thus provides a bug fix for
;;  Emacs bug #7027.
;;
;;  This library also provides a fix for bug #2801 for Emacs 21 and 22
;;  for the case where switches `F' and `R' are both provided.  This
;;  is fixed in vanilla Emacs 23.
;;
;;
;;  ***** NOTE: The following functions defined in `ls-lisp.el' have
;;              been REDEFINED HERE:
;;
;;  `ls-lisp--insert-directory' - Different 2nd header line.
;;  `insert-directory' - If wildcard, set FILE to `default-directory'
;;                       if no dir component.  Include number of files
;;                       in 2nd header line.
;;  `ls-lisp-insert-directory' - If FILE nil, use `default-directory'.
;;                               Do nothing if FILE is "".
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2013/11/03 dadams
;;     Added: redefinition of ls-lisp--insert-directory, for Emacs 24.4.
;;     insert-directory: If wildcard, set FILE to `default-directory' if FILE has no dir component.
;;                       (+ Miscellaneous changes made a while ago.)
;;     ls-lisp-insert-directory: FILE can be null for Emacs 23+.
;; 2010/11/23 dadams
;;     ls-lisp-insert-directory: Finish last fix - second call to ls-lisp-format.
;; 2010/11/16 dadams
;;     ls-lisp-insert-directory for Emacs 24: ls-lisp-format no longer takes NOW arg.
;;     For Emacs 20: Soft-load ls-lisp-20.el.
;; 2010/09/29 dadams
;;     insert-directory: Updated per Emacs 24 code: delete SPC (also) from switches.
;; 2010/09/26 dadams
;;     Added: ls-lisp-insert-directory (Emacs 23+), to let you use wildcards in
;;            file names of a cons arg to dired.  Corrected bug in it for ""FILE.
;; 2009/10/23 dadams
;;     insert-directory: DTRT if count-dired-files returns 0.
;; 2009/04/26 dadams
;;     insert-directory: Bind inhibit-field-text-motion to t, just to be sure.
;; 2008/03/27 dadams
;;     Added: ls-lisp-insert-directory for Emacs 21, 22 (bug fix).
;; 2008/03/04 dadams
;;     insert-directory: Use two separate tooltips.
;; 2008/03/02 dadams
;;     insert-directory: Add total files in dir: shown/total.
;;       Added tooltip, mouse-face.  Bind RET, mouse-2 locally to
;;       dired(-mouse)-describe-listed-directory.
;; 2008/02/29 dadams
;;     Created.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
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
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(or (and (= emacs-major-version 20)
         (load "ls-lisp-20" t nil nil 'MUST-SUFFIX))
    (require 'ls-lisp))

(require 'files+) ;; count-dired-files, dired-describe-listed-directory,
                  ;; dired-mouse-describe-listed-directory

;; Quiet the byte-compiler
(defvar original-insert-directory)      ; Defined in `ls-lisp.el'.
(defvar ls-lisp-uid-d-fmt)      ; Defined in `ls-lisp.el' (Emacs 23+).
(defvar ls-lisp-uid-s-fmt)      ; Defined in `ls-lisp.el' (Emacs 23+).
(defvar ls-lisp-gid-d-fmt)      ; Defined in `ls-lisp.el' (Emacs 23+).
(defvar ls-lisp-gid-s-fmt)      ; Defined in `ls-lisp.el' (Emacs 23+).
(defvar ls-lisp-filesize-d-fmt) ; Defined in `ls-lisp.el' (Emacs 23+).
(defvar ls-lisp-filesize-f-fmt) ; Defined in `ls-lisp.el' (Emacs 23+).

;;;;;;;;;;;;;;;;;;;;;;;;;;




;; REPLACE ORIGINAL in `ls-lisp.el'
;;
;; 1. If wildcard, set FILE to `default-directory' if FILE has no dir component.
;; 2. In second header line: include the number of files and subdirs in the directory.
;;
(when (fboundp 'ls-lisp--insert-directory) ; Emacs 24.4+
  (defun ls-lisp--insert-directory (orig-fun file switches &optional wildcard full-directory-p)
    "Insert directory listing for FILE, formatted according to SWITCHES.
Leaves point after the inserted text.
SWITCHES may be a string of options, or a list of strings.
Optional third arg WILDCARD means treat FILE as shell wildcard.
Optional fourth arg FULL-DIRECTORY-P means file is a directory and
switches do not contain `d', so that a full listing is expected.

This version of the function comes from `ls-lisp.el'.
If the value of `ls-lisp-use-insert-directory-program' is non-nil then
this advice just delegates the work to ORIG-FUN (the normal `insert-directory'
function from `files.el').
But if the value of `ls-lisp-use-insert-directory-program' is nil
then it runs a Lisp emulation.

The Lisp emulation does not run any external programs or shells.  It
supports ordinary shell wildcards if `ls-lisp-support-shell-wildcards'
is non-nil; otherwise, it interprets wildcards as regular expressions
to match file names.  It does not support all `ls' switches -- those
that work are: A a B C c F G g h i n R r S s t U u X.  The l switch
is assumed to be always present and cannot be turned off."
    (if ls-lisp-use-insert-directory-program
        (funcall orig-fun
                 file switches wildcard full-directory-p)
      ;; We need the directory in order to find the right handler.
      (let ((handler (find-file-name-handler (expand-file-name file)
                                             'insert-directory))
            (orig-file file)
            wildcard-regexp)
        (if handler
            (funcall handler 'insert-directory file switches
                     wildcard full-directory-p)
          ;; Remove --dired switch
          (if (string-match "--dired " switches)
              (setq switches (replace-match "" nil nil switches)))
          ;; Convert SWITCHES to a list of characters.
          (setq switches (delete ?\  (delete ?- (append switches nil))))
          ;; Sometimes we get ".../foo*/" as FILE.  While the shell and
          ;; `ls' don't mind, we certainly do, because it makes us think
          ;; there is no wildcard, only a directory name.
          (if (and ls-lisp-support-shell-wildcards
                   (string-match "[[?*]" file)
                   ;; Prefer an existing file to wildcards, like
                   ;; dired-noselect does.
                   (not (file-exists-p file)))
              (progn
                (or (not (eq (aref file (1- (length file))) ?/))
                    (setq file (substring file 0 (1- (length file)))))
                (setq wildcard t)))
          (if wildcard

              (setq wildcard-regexp
                    (if ls-lisp-support-shell-wildcards
                        (wildcard-to-regexp (file-name-nondirectory file))
                      (file-name-nondirectory file))
                    file             (or (file-name-directory file)  default-directory))
            (if (memq ?B switches) (setq wildcard-regexp "[^~]\\'")))
          (condition-case err
              (ls-lisp-insert-directory
               file switches (ls-lisp-time-index switches)
               wildcard-regexp full-directory-p)
            (invalid-regexp
             ;; Maybe they wanted a literal file that just happens to
             ;; use characters special to shell wildcards.
             (if (equal (cadr err) "Unmatched [ or [^")
                 (progn
                   (setq wildcard-regexp  (if (memq ?B switches) "[^~]\\'")
                         file             (file-relative-name orig-file))
                   (ls-lisp-insert-directory
                    file switches (ls-lisp-time-index switches)
                    nil full-directory-p))
               (signal (car err) (cdr err)))))
          ;; Try to insert the amount of free space.
          (save-excursion
            (goto-char (point-min))
            (while (re-search-forward "^total" nil t)
              (beginning-of-line)
              (let ((counted  (save-match-data (count-dired-files))))
                (if (zerop counted)
                    (insert "files 0/0 ")
                  (insert "files " (number-to-string counted)
                          "/" (number-to-string
                               (- (length (directory-files default-directory
                                                           nil nil t)) 2))
                          " ")))
              (goto-char (point-min))
              (re-search-forward "^files [0-9]+/[0-9]+ \\(total\\)" nil t)
              (replace-match "space used" nil nil nil 1)
              (let ((available (and (fboundp 'get-free-disk-space)
                                    (get-free-disk-space ".")))
                    (map (make-sparse-keymap))
                    (inhibit-field-text-motion t)) ; Just to be sure, for eol.
                (define-key map [mouse-2]
                  'dired-mouse-describe-listed-directory)
                (define-key map "\r" 'dired-describe-listed-directory)
                (when available (end-of-line) (insert " available " available))
                (add-text-properties
                 (save-excursion (beginning-of-line) (line-beginning-position))
                 (1- (match-beginning 1))
                 `(mouse-face highlight keymap ,map
                   help-echo "Files shown / total files in directory \
\[RET, mouse-2: more info]"))
                (add-text-properties (match-beginning 1) (line-end-position)
                                     `(mouse-face highlight keymap ,map
                                       help-echo "Kbytes used in directory, Kbytes \
available on disk [RET, mouse-2: more info]"))))))))))



;; REPLACE ORIGINAL in `ls-lisp.el'
;;
;; 1. If wildcard, set FILE to `default-directory' if FILE has no dir component.
;; 2. In second header line: include the number of files and subdirs in the directory.
;;
(unless (fboundp 'ls-lisp--insert-directory) ; Emacs < 24.4.
  (defun insert-directory (file switches &optional wildcard full-directory-p)
    "Insert directory listing for FILE, formatted according to SWITCHES.
Leaves point after the inserted text.
SWITCHES may be a string of options, or a list of strings.
Optional third arg WILDCARD means treat FILE as shell wildcard.
Optional fourth arg FULL-DIRECTORY-P means file is a directory and
switches do not contain `d', so that a full listing is expected.

This version of the function comes from `ls-lisp.el'.
If the value of `ls-lisp-use-insert-directory-program' is non-nil then
it works exactly like the version from `files.el' and runs a directory
listing program whose name is in the variable
`insert-directory-program'; if also WILDCARD is non-nil then it runs
the shell specified by `shell-file-name'.  If the value of
`ls-lisp-use-insert-directory-program' is nil then it runs a Lisp
emulation.

The Lisp emulation does not run any external programs or shells.  It
supports ordinary shell wildcards if `ls-lisp-support-shell-wildcards'
is non-nil; otherwise, it interprets wildcards as regular expressions
to match file names.  It does not support all `ls' switches -- those
that work are: A a c i r S s t u U X g G B C R n and F partly."
    (if ls-lisp-use-insert-directory-program
        (funcall original-insert-directory ; Free here.  Defined in `ls-lisp.el'.
                 file switches wildcard full-directory-p)
      ;; We need the directory in order to find the right handler.
      (let ((handler (find-file-name-handler (expand-file-name file)
                                             'insert-directory))
            (orig-file file)
            wildcard-regexp)
        (if handler
            (funcall handler 'insert-directory file switches
                     wildcard full-directory-p)
          ;; Remove --dired switch
          (if (string-match "--dired " switches)
              (setq switches (replace-match "" nil nil switches)))
          ;; Convert SWITCHES to a list of characters.
          (setq switches (delete ?\  (delete ?- (append switches nil))))
          ;; Sometimes we get ".../foo*/" as FILE.  While the shell and
          ;; `ls' don't mind, we certainly do, because it makes us think
          ;; there is no wildcard, only a directory name.
          (if (and ls-lisp-support-shell-wildcards
                   (string-match "[[?*]" file)
                   ;; Prefer an existing file to wildcards, like
                   ;; dired-noselect does.
                   (not (file-exists-p file)))
              (progn
                (or (not (eq (aref file (1- (length file))) ?/))
                    (setq file (substring file 0 (1- (length file)))))
                (setq wildcard t)))
          (if wildcard
              (setq wildcard-regexp  (if ls-lisp-support-shell-wildcards
                                         (wildcard-to-regexp (file-name-nondirectory file))
                                       (file-name-nondirectory file))
                    file             (or (file-name-directory file) default-directory))
            (if (memq ?B switches) (setq wildcard-regexp "[^~]\\'")))
          (condition-case err
              (if (< emacs-major-version 21)
                  (condition-case err2
                      ;; First try for Francis Wright's version of `ls-lisp.el'.
                      ;; Its signature is the same for Emacs 20 as for Emacs > 20 and is the same as
                      ;; for vanilla Emacs > Emacs 20.
                      (ls-lisp-insert-directory file switches (ls-lisp-time-index switches)
                                                wildcard-regexp full-directory-p)
                    (wrong-number-of-arguments ; Vanilla `ls-lisp.el' for Emacs 20.
                     (ls-lisp-insert-directory file switches wildcard-regexp full-directory-p)))
                (ls-lisp-insert-directory
                 file switches (ls-lisp-time-index switches)
                 wildcard-regexp full-directory-p))
            (invalid-regexp
             ;; Maybe they wanted a literal file that just happens to
             ;; use characters special to shell wildcards.
             (if (equal (cadr err) "Unmatched [ or [^")
                 (progn
                   (setq wildcard-regexp  (if (memq ?B switches) "[^~]\\'")
                         file             (file-relative-name orig-file))
                   (ls-lisp-insert-directory
                    file switches (ls-lisp-time-index switches)
                    nil full-directory-p))
               (signal (car err) (cdr err)))))
          (save-excursion
            (goto-char (point-min))
            (while (re-search-forward "^total" nil t)
              (beginning-of-line)
              (let ((counted  (save-match-data (count-dired-files))))
                (if (zerop counted)
                    (insert "files 0/0 ")
                  (insert "files " (number-to-string counted)
                          "/" (number-to-string
                               (- (length (directory-files default-directory
                                                           nil nil t)) 2))
                          " ")))
              (goto-char (point-min))
              (re-search-forward "^files [0-9]+/[0-9]+ \\(total\\)" nil t)
              (replace-match "space used" nil nil nil 1)
              (let ((available (and (fboundp 'get-free-disk-space)
                                    (get-free-disk-space ".")))
                    (map (make-sparse-keymap))
                    (inhibit-field-text-motion t)) ; Just to be sure, for eol.
                (define-key map [mouse-2]
                  'dired-mouse-describe-listed-directory)
                (define-key map "\r" 'dired-describe-listed-directory)
                (when available (end-of-line) (insert " available " available))
                (add-text-properties
                 (save-excursion (beginning-of-line) (line-beginning-position))
                 (1- (match-beginning 1))
                 `(mouse-face highlight keymap ,map
                   help-echo "Files shown / total files in directory \
\[RET, mouse-2: more info]"))
                (add-text-properties (match-beginning 1) (line-end-position)
                                     `(mouse-face highlight keymap ,map
                                       help-echo "Kbytes used in directory, Kbytes \
available on disk [RET, mouse-2: more info]"))))))))))


;; REPLACE ORIGINAL in `ls-lisp.el'.
;;
;; BUG fix for Emacs 21 and 22.  Emacs 23 already has the bug (#2801) fix.
;;
(cond ((= emacs-major-version 21)
       (defun ls-lisp-insert-directory
           (file switches time-index wildcard full-directory-p)
         "Insert directory listing for FILE, formatted according to SWITCHES.
Leaves point after the inserted text.  This is an internal function
optionally called by the `ls-lisp.el' version of `insert-directory'.
It is called recursively if the -R switch is used.
SWITCHES is a *list* of characters.  TIME-INDEX is the time index into
file-attributes according to SWITCHES.  WILDCARD is nil or an *Emacs
regexp*.  FULL-DIRECTORY-P means file is a directory and SWITCHES does
not contain `d', so that a full listing is expected."
         ;; Sometimes we get ".../foo*/" as FILE.  While the shell and
         ;; `ls' don't mind, we certainly do, because it makes us think
         ;; there is no wildcard, only a directory name.
         (if (and ls-lisp-support-shell-wildcards
                  (string-match "[[?*]" file))
             (progn
               (or (not (eq (aref file (1- (length file))) ?/))
                   (setq file (substring file 0 (1- (length file)))))
               (setq wildcard t)))
         (if (or wildcard full-directory-p)
             (let* ((dir (file-name-as-directory file))
                    (default-directory dir) ; so that file-attributes works
                    (file-alist
                     (directory-files-and-attributes dir nil wildcard t))
                    (now (current-time))
                    (sum 0)
                    ;; do all bindings here for speed
                    total-line files elt short file-size fil attr)
               (cond ((memq ?A switches)
                      (setq file-alist
                            (ls-lisp-delete-matching "^\\.\\.?$" file-alist)))
                     ((not (memq ?a switches))
                      ;; if neither -A  nor -a, flush . files
                      (setq file-alist
                            (ls-lisp-delete-matching "^\\." file-alist))))
               (setq file-alist
                     (ls-lisp-handle-switches file-alist switches))
               (if (memq ?C switches)   ; column (-C) format
                   (ls-lisp-column-format file-alist)
                 (setq total-line (cons (point) (car-safe file-alist)))
                 (setq files file-alist)
                 (while files           ; long (-l) format
                   (setq elt (car files)
                         files (cdr files)
                         short (car elt)
                         attr (cdr elt)
                         file-size (nth 7 attr))
                   (and attr
                        (setq sum (+ file-size
                                     ;; Even if neither SUM nor file's size
                                     ;; overflow, their sum could.
                                     (if (or (< sum (- 134217727 file-size))
                                             (floatp sum)
                                             (floatp file-size))
                                         sum
                                       (float sum))))
                        (insert (ls-lisp-format short attr file-size
                                                switches time-index now))))
                 ;; Insert total size of all files:
                 (save-excursion
                   (goto-char (car total-line))
                   (or (cdr total-line)
                       ;; Shell says ``No match'' if no files match
                       ;; the wildcard; let's say something similar.
                       (insert "(No match)\n"))
                   (insert (format "total %.0f\n" (fceiling (/ sum 1024.0))))))
               (if (memq ?R switches)
                   ;; List the contents of all directories recursively.
                   ;; cadr of each element of `file-alist' is t for
                   ;; directory, string (name linked to) for symbolic
                   ;; link, or nil.
                   (while file-alist
                     (setq elt (car file-alist)
                           file-alist (cdr file-alist))
                     (when (and (eq (cadr elt) t) ; directory
                                ;; Under -F, we have already decorated all
                                ;; directories, including "." and "..", with
                                ;; a /, so allow for that as well.
                                (not (string-match "\\`\\.\\.?/?\\'" (car elt))))
                       (setq elt (expand-file-name (car elt) dir))
                       (insert "\n" elt ":\n")
                       (ls-lisp-insert-directory
                        elt switches time-index wildcard full-directory-p)))))
           ;; If not full-directory-p, FILE *must not* end in /, as
           ;; file-attributes will not recognize a symlink to a directory,
           ;; so must make it a relative filename as ls does:
           (if (eq (aref file (1- (length file))) ?/)
               (setq file (substring file 0 -1)))
           (let ((fattr (file-attributes file)))
             (if fattr
                 (insert (ls-lisp-format file fattr (nth 7 fattr)
                                         switches time-index (current-time)))
               (message "%s: doesn't exist or is inaccessible" file)
               (ding) (sit-for 2))))))

      ((= emacs-major-version 22)
       (defun ls-lisp-insert-directory
           (file switches time-index wildcard-regexp full-directory-p)
         "Insert directory listing for FILE, formatted according to SWITCHES.
Leaves point after the inserted text.  This is an internal function
optionally called by the `ls-lisp.el' version of `insert-directory'.
It is called recursively if the -R switch is used.
SWITCHES is a *list* of characters.  TIME-INDEX is the time index into
file-attributes according to SWITCHES.  WILDCARD-REGEXP is nil or an *Emacs
regexp*.  FULL-DIRECTORY-P means file is a directory and SWITCHES does
not contain `d', so that a full listing is expected."
         (if (or wildcard-regexp full-directory-p)
             (let* ((dir (file-name-as-directory file))
                    (default-directory dir) ; so that file-attributes works
                    (file-alist
                     (directory-files-and-attributes
                      dir nil wildcard-regexp t 'string))
                    (now (current-time))
                    (sum 0)
                    ;; do all bindings here for speed
                    total-line files elt short file-size fil attr)
               (cond ((memq ?A switches)
                      (setq file-alist
                            (ls-lisp-delete-matching "^\\.\\.?$" file-alist)))
                     ((not (memq ?a switches))
                      ;; if neither -A  nor -a, flush . files
                      (setq file-alist
                            (ls-lisp-delete-matching "^\\." file-alist))))
               (setq file-alist
                     (ls-lisp-handle-switches file-alist switches))
               (if (memq ?C switches)   ; column (-C) format
                   (ls-lisp-column-format file-alist)
                 (setq total-line (cons (point) (car-safe file-alist)))
                 (setq files file-alist)
                 (while files           ; long (-l) format
                   (setq elt (car files)
                         files (cdr files)
                         short (car elt)
                         attr (cdr elt)
                         file-size (nth 7 attr))
                   (and attr
                        (setq sum (+ file-size
                                     ;; Even if neither SUM nor file's size
                                     ;; overflow, their sum could.
                                     (if (or (< sum (- 134217727 file-size))
                                             (floatp sum)
                                             (floatp file-size))
                                         sum
                                       (float sum))))
                        (insert (ls-lisp-format short attr file-size
                                                switches time-index now))))
                 ;; Insert total size of all files:
                 (save-excursion
                   (goto-char (car total-line))
                   (or (cdr total-line)
                       ;; Shell says ``No match'' if no files match
                       ;; the wildcard; let's say something similar.
                       (insert "(No match)\n"))
                   (insert (format "total %.0f\n" (fceiling (/ sum 1024.0))))))
               (if (memq ?R switches)
                   ;; List the contents of all directories recursively.
                   ;; cadr of each element of `file-alist' is t for
                   ;; directory, string (name linked to) for symbolic
                   ;; link, or nil.
                   (while file-alist
                     (setq elt (car file-alist)
                           file-alist (cdr file-alist))
                     (when (and (eq (cadr elt) t) ; directory
                                ;; Under -F, we have already decorated all
                                ;; directories, including "." and "..", with
                                ;; a /, so allow for that as well.
                                (not (string-match "\\`\\.\\.?/?\\'" (car elt))))
                       (setq elt (expand-file-name (car elt) dir))
                       (insert "\n" elt ":\n")
                       (ls-lisp-insert-directory
                        elt switches time-index wildcard-regexp
                        full-directory-p)))))
           ;; If not full-directory-p, FILE *must not* end in /, as
           ;; file-attributes will not recognize a symlink to a directory,
           ;; so must make it a relative filename as ls does:
           (if (eq (aref file (1- (length file))) ?/)
               (setq file (substring file 0 -1)))
           (let ((fattr (file-attributes file 'string)))
             (if fattr
                 (insert (ls-lisp-format file fattr (nth 7 fattr)
                                         switches time-index (current-time)))
               (message "%s: doesn't exist or is inaccessible" file)
               (ding) (sit-for 2)))))))



;; REPLACE ORIGINAL in `ls-lisp.el'.
;;
;; 1. If FILE is nil, then just use `default-directory'.
;; 2. Do nothing if FILE is "".  This is a fix for Emacs bug #7112.
;;
(when (> emacs-major-version 22)
  (defun ls-lisp-insert-directory
      (file switches time-index wildcard-regexp full-directory-p)
    "Insert directory listing for FILE, formatted according to SWITCHES.
Leaves point after the inserted text.  This is an internal function
optionally called by the `ls-lisp.el' version of `insert-directory'.
It is called recursively if the -R switch is used.
SWITCHES is a *list* of characters.  TIME-INDEX is the time index into
file-attributes according to SWITCHES.  WILDCARD-REGEXP is nil or an *Emacs
regexp*.  FULL-DIRECTORY-P means file is a directory and SWITCHES does
not contain `d', so that a full listing is expected."
    (when (or (null file) (> (length file) 0)) ; Do nothing for empty FILE, "".
      (if (or wildcard-regexp full-directory-p)

          (let* ((dir               (if file
                                        (file-name-as-directory file)
                                      default-directory))
                 (default-directory  dir) ; so that file-attributes works
                 (file-alist
                  (directory-files-and-attributes dir nil wildcard-regexp t
                                                  (if (memq ?n switches)
                                                      'integer
                                                    'string)))
                 (now (current-time))
                 (sum 0)
                 (max-uid-len 0)
                 (max-gid-len 0)
                 (max-file-size 0)
                 ;; do all bindings here for speed
                 total-line files elt short file-size fil attr
                 fuid fgid uid-len gid-len)
            (cond ((memq ?A switches)
                   (setq file-alist
                         (ls-lisp-delete-matching "^\\.\\.?$" file-alist)))
                  ((not (memq ?a switches))
                   ;; if neither -A  nor -a, flush . files
                   (setq file-alist
                         (ls-lisp-delete-matching "^\\." file-alist))))
            (setq file-alist
                  (ls-lisp-handle-switches file-alist switches))
            (if (memq ?C switches)      ; column (-C) format
                (ls-lisp-column-format file-alist)
              (setq total-line (cons (point) (car-safe file-alist)))
              ;; Find the appropriate format for displaying uid, gid, and
              ;; file size, by finding the longest strings among all the
              ;; files we are about to display.
              (dolist (elt file-alist)
                (setq attr (cdr elt)
                      fuid (nth 2 attr)
                      uid-len (if (stringp fuid) (string-width fuid)
                                (length (format "%d" fuid)))
                      fgid (nth 3 attr)
                      gid-len (if (stringp fgid) (string-width fgid)
                                (length (format "%d" fgid)))
                      file-size (nth 7 attr))
                (if (> uid-len max-uid-len)
                    (setq max-uid-len uid-len))
                (if (> gid-len max-gid-len)
                    (setq max-gid-len gid-len))
                (if (> file-size max-file-size)
                    (setq max-file-size file-size)))
              (setq ls-lisp-uid-d-fmt (format " %%-%dd" max-uid-len))
              (setq ls-lisp-uid-s-fmt (format " %%-%ds" max-uid-len))
              (setq ls-lisp-gid-d-fmt (format " %%-%dd" max-gid-len))
              (setq ls-lisp-gid-s-fmt (format " %%-%ds" max-gid-len))
              (setq ls-lisp-filesize-d-fmt
                    (format " %%%dd"
                            (if (memq ?s switches)
                                (length (format "%.0f"
                                                (fceiling (/ max-file-size 1024.0))))
                              (length (format "%.0f" max-file-size)))))
              (setq ls-lisp-filesize-f-fmt
                    (format " %%%d.0f"
                            (if (memq ?s switches)
                                (length (format "%.0f"
                                                (fceiling (/ max-file-size 1024.0))))
                              (length (format "%.0f" max-file-size)))))
              (setq files file-alist)
              (while files              ; long (-l) format
                (setq elt (car files)
                      files (cdr files)
                      short (car elt)
                      attr (cdr elt)
                      file-size (nth 7 attr))
                (and attr
                     (setq sum (+ file-size
                                  ;; Even if neither SUM nor file's size
                                  ;; overflow, their sum could.
                                  (if (or (< sum (- 134217727 file-size))
                                          (floatp sum)
                                          (floatp file-size))
                                      sum
                                    (float sum))))
                     (insert
                      (if (> emacs-major-version 23)
                          (ls-lisp-format short attr file-size switches time-index)
                        (ls-lisp-format short attr file-size
                                        switches time-index now)))))
              ;; Insert total size of all files:
              (save-excursion
                (goto-char (car total-line))
                (or (cdr total-line)
                    ;; Shell says ``No match'' if no files match
                    ;; the wildcard; let's say something similar.
                    (insert "(No match)\n"))
                (insert (format "total %.0f\n" (fceiling (/ sum 1024.0))))))
            (if (memq ?R switches)
                ;; List the contents of all directories recursively.
                ;; cadr of each element of `file-alist' is t for
                ;; directory, string (name linked to) for symbolic
                ;; link, or nil.
                (while file-alist
                  (setq elt (car file-alist)
                        file-alist (cdr file-alist))
                  (when (and (eq (cadr elt) t) ; directory
                             ;; Under -F, we have already decorated all
                             ;; directories, including "." and "..", with
                             ;; a /, so allow for that as well.
                             (not (string-match "\\`\\.\\.?/?\\'" (car elt))))
                    (setq elt (expand-file-name (car elt) dir))
                    (insert "\n" elt ":\n")
                    (ls-lisp-insert-directory
                     elt switches time-index wildcard-regexp full-directory-p)))))
        ;; If not full-directory-p, FILE *must not* end in /, as
        ;; file-attributes will not recognize a symlink to a directory,
        ;; so must make it a relative filename as ls does:
        (if (file-name-absolute-p file) (setq file (expand-file-name file)))
        (if (eq (aref file (1- (length file))) ?/)
            (setq file (substring file 0 -1)))
        (let ((fattr (file-attributes file 'string)))
          (if fattr
              (insert
               (if (> emacs-major-version 23)
                   (ls-lisp-format (if (memq ?F switches)
                                       (ls-lisp-classify-file file fattr)
                                     file)
                                   fattr (nth 7 fattr) switches time-index)
                 (ls-lisp-format file fattr (nth 7 fattr) switches time-index (current-time))))
            (message "%s: doesn't exist or is inaccessible" file)
            (ding) (sit-for 2)))))))    ; to show user the message!

;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'ls-lisp+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ls-lisp+.el ends here
