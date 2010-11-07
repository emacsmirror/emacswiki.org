;;; mon-dir-utils.el --- functions and commands for directories and files
;; -*- mode: EMACS-LISP; -*-
;;; ================================================================
;; Copyright © 2009, 2010 MON KEY. All rights reserved.
;;; ================================================================

;; FILENAME: mon-dir-utils.el
;; AUTHOR: MON KEY
;; MAINTAINER: MON KEY
;; CREATED: 2009-05-11T11:13:47-05:00Z
;; VERSION: 1.0.0
;; COMPATIBILITY: Emacs23.*
;; KEYWORDS: dired, files, environment, convenience

;;; ================================================================

;;; COMMENTARY: 

;; =================================================================
;; DESCRIPTION:
;; mon-dir-utils.el Provides a collection of functions and commands
;; commands for working with directories and files.
;;
;; FUNCTIONS:►►►
;; `mon-dired-srt-alph',`mon-dired-srt-chrn',`mon-dired-srt-type',
;; `mon-dired-srt-type-alph', `mon-dired-srt-type-chrn',
;; `mon-dired-up-directory-this-buffer', `mon-dired-insert-dirs-recursive',
;; `mon-explorer-naf-artist', `mon-explorer-naf-brand', `mon-dired-naf-artist-letter',
;; `mon-dired-naf-brand-letter', `mon-dir-save-current',
;; `mon-dir-save-current-to-file', `mon-add-subdirs-to-list',
;; `mon-insert-subdirs-in-buffer', `mon-rename-file-serial',
;; `mon-path', `mon-copy-file-path', `mon-insert-path',
;; `mon-get-buffers-directories', `mon-proc-buffers-directories',
;; `mon-get-proc-buffers-directories', `mon-get-buffer-parent-dir',
;; `mon-buffer-written-p', `mon-string-split-buffer-name',
;; `mon-string-split-buffer-parent-dir', 
;; `mon-ebay-image-directory-not-ok', `mon-ebay-image-directory-ok-p',
;; `mon-truncate-path-for-prompt', `mon-dir-common-paths',
;; `mon-string-split-dir-recurse', `mon-dired-nef-dir', `mon-dir-nef-big', 
;; `mon-dir-nef-converge', `mon-dir-nef-keep-3',`mon-dir-nef-name-to-head',
;; `mon-dir-nef-conc-ranges', `mon-dir-nef-ranges', `mon-dir-nef-conc-dups',
;; `mon-dir-nef-find-dups', `mon-dir-nef-remove-if-empty', `mon--local-url-for-bug'
;; `mon-get-local-url-for-bug', `mon-toggle-dired-dwim-target',
;; `mon-get-relative-w-absolute', `mon-copy-file-dired-as-list',
;; `mon-copy-file-dired-as-string', `mon-get-ps2ascii', `mon-get-pdftotext',
;; `mon-get-new-buffer-w-stamp',
;; `mon-bind-nefs-photos-at-loadtime', `mon-dired-other-window',
;; `mon-get-dir-subdir-default', `mon-file-map-elisp-fileset',
;; `mon-get-dir-size', `mon-dired-uninsert-subdir',
;; `mon-dired-uninsert-subdir-all', `mon-dired-unmark-elc',
;; `mon-file-dir-attributes->plist', `mon-file-ensure-extension-is-el'
;; `mon-frame-live-visible-graphic-p', `mon-find-buffer-visiting-other-live-frame',
;; `mon-dired-find-file-other-frame', `mon-file-truename-p',
;; `%mon-dir-get-subdirs-filter-no-full', `%mon-dir-get-subdirs-filter-full',
;; `mon-dir-get-subdirs',
;; FUNCTIONS:◄◄◄
;;
;; MACROS:
;;
;; METHODS:
;;
;; CLASSES:
;;
;; CONSTANTS: 
;;
;; VARIABLES:
;; `*mon-img-hash*' and (:TESTING version `*temp-hash*')
;; `*mon-nef-img-hash*', `*mon-jpg-img-hash*', `*mon-bmp-img-hash*',
;; `*mon-pdfinfo-exec-path*', `*mon-pdftotext-exec-path*',
;; `*mon-add-subdirs-to-list-ignorables*',
;; `*regexp-add-subdirs-to-list-filter-ignorables*', 
;;
;; :GROUPS
;; `mon-dir-utils'
;;
;; ALIASED/ADVISED/SUBST'D:
;; `dired-up-here'                            -> `mon-dired-up-directory-this-buffer' 
;; `mon-make-path'                            -> `mon-build-path'
;; `mon-get-w3m-dired-file'                   -> `mon-w3m-dired-file'
;; `mon-dired-kill-files-to-list'             -> `mon-copy-file-dired-as-list'
;; `mon-dired-copy-files-to-list'             -> `mon-copy-file-dired-as-list'
;; `mon-dired-kill-files-to-strings'          -> `mon-copy-file-dired-as-string'
;; `mon-dired-copy-files-to-strings'          -> `mon-copy-file-dired-as-string'
;; `mon-move-file'                            -> `rename-file'
;; `mon-mv-file'                              -> `rename-file'
;; `mon-rename-file'                          -> `rename-file'
;; `mon-rnm-file'                             -> `rename-file'
;; `mon-file-move'                            -> `rename-file'
;; `mon-file-mv'                              -> `rename-file'
;; `mon-file-rename'                          -> `rename-file'
;; `mon-file-rnm'                             -> `rename-file'
;; `mon-file-copy-path'                       -> `mon-copy-file-path'
;; `mon-file-copy-multiple'                   -> `mon-copy-file-multiple'    
;; `mon-file-copy-in-sub-dirs'                -> `mon-copy-files-in-sub-dirs'
;; `mon-file-rename-serial'                   -> `mon-rename-file-serial'
;; `mon-buffer-string-split-name'             -> `mon-string-split-buffer-name'            
;; `mon-buffer-string-split-parent-dir'       -> `mon-string-split-buffer-parent-dir'      
;; `mon-buffer-string-split-parent-dir-quick' -> `mon-string-split-buffer-parent-dir-quick'
;; `mon-buffer-string-split-dir-recurse'      -> `mon-string-split-dir-recurse'
;; `mon-buffer-get-new-w-stamp'               -> `mon-get-new-buffer-w-stamp'
;; `mon-buffer-get-parent-dir'                -> `mon-get-buffer-parent-dir'
;; `mon-buffer-get-directories'               -> `mon-get-buffers-directories'
;; `mon-buffer-get-and-proc-dirs'             -> `mon-get-proc-buffers-directories'
;; `mon-buffer-file-copy-path'                -> `mon-copy-file-path'
;; `mon-buffer-subdirs-insert'                -> `mon-insert-subdirs-in-buffer'
;; `mon-dir-recurse-string-split'             -> `mon-string-split-dir-recurse'
;; `mon-dir-get-subdir'                       -> `mon-get-dir-subdir-default'
;; `mon-get-dir-common-path'                  -> `mon-dir-common-paths'
;; `mon-dir-name-absolute'                    -> `mon-get-dir-name-absolute'
;; `mon-dir-name-relative-w-absolute'         -> `mon-get-relative-w-absolute'
;; `dired-uninsert-subdir'                    -> `mon-dired-uninsert-subdir'
;; `dired-subdir-uninsert'                    -> `mon-dired-uninsert-subdir'
;; `dired-uninsert-subdir-all'                -> `mon-dired-uninsert-subdir-all'
;; `dired-subdir-uninsert-all'                -> `mon-dired-uninsert-subdir-all'
;; `naf-dired-artist-letter'                  -> `mon-dired-naf-artist-letter'
;; `naf-drive-dired-artist-letter'            -> `mon-dired-naf-artist-letter'
;; `naf-dired-brand-letter'                   -> `mon-dired-naf-brand-letter'
;; `naf-drive-dired-brand-letter'             -> `mon-dired-naf-brand-letter'
;; `dired-find-file-other-frame'              -> `mon-dired-find-file-other-frame'
;; `frame-live-visible-graphic-p'             -> `mon-frame-live-visible-graphic-p'
;; `find-buffer-visiting-other-live-frame'    -> `mon-find-buffer-visiting-other-live-frame'
;; `mon-directory-attributes-plist'           -> `mon-file-dir-attributes->plist'
;; `mon-file-attributes-plist'                -> `mon-file-dir-attributes->plist'
;; `mon-dired-toggle-dwim-target'             -> `mon-toggle-dired-dwim-target'
;; `file-truename-p'                          -> `mon-file-truename-p' 
;; `directory-attributes-plist'               -> `mon-file-dir-attributes->plist'
;; `file-attributes-plist'                    -> `mon-file-dir-attributes->plist'
;; `mon-directory-get-size'                   -> `mon-get-dir-size'
;; `mon-dir-name-truncate-for-prompt'         -> `mon-truncate-path-for-prompt'
;;
;; DEPRECATED:
;;
;; RENAMED:
;; `mon-split-string-buffer-name'             -> `mon-string-split-buffer-name'
;; `mon-split-string-buffer-parent-dir'       -> `mon-string-split-buffer-parent-dir'
;; `mon-split-string-buffer-parent-dir-quick' -> `mon-string-split-buffer-parent-dir-quick'
;; `*img-hash*'                               -> `*mon-img-hash*'    
;; `mon-map-elisp-fileset'                    -> `mon-file-map-elisp-fileset'
;; `mon-dir-nef-rmv-empt'                     -> `mon-dir-nef-remove-if-empty'
;; `mon-dir-nef-alist'                        -> `mon-dir-nef-name-to-head'
;;
;; MOVED:
;; `*mon-artist-naf-path*'                    -> mon-dir-locals-alist.el
;; `*mon-brand-naf-path*'                     -> mon-dir-locals-alist.el
;; `mon-comp-times-flt-pnt'                   -> mon-time-utils.el
;; `mon-conv-time-flt-pnt'                    -> mon-time-utils.el
;; `mon-get-file-mod-times'                   -> mon-time-utils.el
;; `mon-file-older-than-file-p'               -> mon-time-utils.el
;; `mon-dired-srt-alph'                       <- mon-dir-utils-switches.el
;; `mon-dired-srt-chrn'                       <- mon-dir-utils-switches.el
;; `mon-dired-srt-type'                       <- mon-dir-utils-switches.el
;; `mon-dired-srt-type-alph'                  <- mon-dir-utils-switches.el
;; `mon-dired-srt-type-chrn'                  <- mon-dir-utils-switches.el
;; `mon-dired-up-directory-this-buffer'       <- mon-dir-utils-switches.el
;; `mon-w3m-dired-file'                       -> mon-url-utils.el
;; `mon-w3m-kill-url-at-point'                -> mon-url-utils.el
;; `mon-map-elisp-fileset'                    <- mon-elisp-fileset.el
;;
;; REQUIRES:
;; 
;; :FILE cl.el
;; `mon-read-multiple', `mon-file-reduce-name', `mon-dired-naf-image-dir'
;; 
;;                                                   
;; :FILE mon-replacement-utils.el
;; :SEE (URL `http://www.emacswiki.org/emacs/mon-replacement-utils.el')
;;
;; :FILE mon-css-color.el 
;; :SEE (URL `http://www.emacswiki.org/emacs/mon-css-color.el')
;; :NOTE _before_ mon-rename-image-utils
;;
;; :FILE mon-rename-image-utils.el
;; :SEE (URL `http://www.emacswiki.org/emacs/mon-css-color.el')
;;
;; :FILE mon-time-utils.el
;; `mon-get-new-buffer-w-stamp' -> `mon-file-stamp-vrfy-put-eof', `mon-file-stamp'
;; `mon-file-map-elisp-fileset' -> `mon-file-stamp'
;; :SEE (URL `http://www.emacswiki.org/emacs/mon-time-utils.el')
;;
;; TODO:
;;
;; NOTES:
;;
;; SNIPPETS:
;;
;; THIRD PARTY CODE:
;; 
;; URL: http://www.emacswiki.org/emacs/mon-dir-utils.el
;; FILE-PUBLISHED: <Timestamp: #{2009-09-02} - by MON KEY>
;; 
;; FILE-CREATED:
;; <Timestamp: #{2009-05-11T11:13:47-05:00Z} - by MON KEY>
;; 
;; ================================================================

;;; LICENSE:

;; =================================================================
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
;; =================================================================
;; Permission is granted to copy, distribute and/or modify this
;; document under the terms of the GNU Free Documentation License,
;; Version 1.3 or any later version published by the Free Software
;; Foundation; with no Invariant Sections, no Front-Cover Texts,
;; and no Back-Cover Texts. A copy of the license is included in
;; the section entitled ``GNU Free Documentation License''.
;; 
;; A copy of the license is also available from the Free Software
;; Foundation Web site at:
;; (URL `http://www.gnu.org/licenses/fdl-1.3.txt').
;;; ==============================
;; Copyright © 2009, 2010 MON KEY 
;;; ==============================

;;; CODE:

;; :REQUIRED-BY `mon-read-multiple', `mon-file-reduce-name'
(eval-when-compile (require 'cl))

;;; ==============================
;; :WAS (require 'mon-dir-locals-alist)
(load "mon-dir-locals-alist")
(require 'mon-replacement-utils)
(require 'mon-css-color) ;; :NOTE _before_ mon-rename-image-utils
(require 'mon-rename-image-utils)
;;; ==============================

;;; ==============================
;;; :CHANGESET 2261
;;; :CREATED <Timestamp: #{2010-11-03T19:11:39-04:00Z}#{10443} - by MON KEY>
(defgroup mon-dir-utils nil
  "Defaults for variables accessed by various `mon-*' functions.\n
:SEE-ALSO .\n►►►"
  :link '(url-link 
          :tag ":EMACSWIKI-FILE" "http://www.emacswiki.org/emacs/mon-dir-utils.el")
  :link '(emacs-library-link "mon-dir-utils.el")
  :group 'mon-base)

;;; ==============================
;;; :CHANGESET 2261
;;; :CREATED <Timestamp: #{2010-11-03T19:08:24-04:00Z}#{10443} - by MON KEY>
(defcustom *mon-add-subdirs-to-list-ignorables*
  '("." ".." "RCS" "CVS" "rcs" "cvs" "lost+found" ".git" ".bzr" ".hg" ".svn" "_darcs")
  "A list of directory namestring components to filter.\n
Elements of list will not be appear in return values of:\n
 `mon-add-subdirs-to-list' `mon-dir-get-subdirs'\n
These are not full paths but only the directory name component of one, e.g.:
 \(file-name-nondirectory \(expand-file-name \".bzr\" default-directory\)\)
Where the return value \".bzr\" is the name of a directory \".bzr\".\n
:SEE-ALSO `%mon-dir-get-subdirs-filter-full', `%mon-dir-get-subdirs-filter-no-full',
`*regexp-add-subdirs-to-list-filter-ignorables*',
`*mon-add-subdirs-to-list-ignorables*'.\n►►►"
  :type '(repeat string)
  :group 'mon-dir-utils)

;;; ==============================
;;; :CHANGESET 2261
;;; :CREATED <Timestamp: #{2010-11-04T15:21:23-04:00Z}#{10444} - by MON KEY>
(defvar *regexp-add-subdirs-to-list-filter-ignorables* 
  (concat "^.*" (regexp-opt *mon-add-subdirs-to-list-ignorables*) "$")
  "Regexp which filters which subdirectories ignorable directory names.\n
Generated from value of variable `*mon-add-subdirs-to-list-ignorables*'.\n
Directory namestrings matched by this regexp are filted from return values of:\n
 `mon-add-subdirs-to-list' `mon-dir-get-subdirs'\n
:EXAMPLE\n\n\(let \(\(fltr-if *regexp-add-subdirs-to-list-filter-ignorables*\)\)
  \(setq fltr-if ; Re-using the local var
        \(mapcar #'\(lambda \(dirs\)
                    \(and \(not \(or \(string-or-null-p \(cadr dirs\)\)
                                  \(string-match-p fltr-if \(car dirs\)\)\)\)
                         \(file-name-as-directory \(car dirs\)\)\)\)
                \(directory-files-and-attributes default-directory t\)\)\)
  \(append \(list default-directory\) \(delq nil fltr-if\)\)\)\n
:NOTE For use with functions which evaluate `directory-files' or
`directory-files-and-attributes' with optional FULL arg non-nil.
:SEE-ALSO `%mon-dir-get-subdirs-filter-full', `%mon-dir-get-subdirs-filter-no-full',
`*regexp-add-subdirs-to-list-filter-ignorables*',
`*mon-add-subdirs-to-list-ignorables*'.\n►►►")

;;; ==============================
;;; :PREFIX "mc-"
;;; :COURTESY Thierry Volpiatto :HIS tv-utils.el :WAS `cat'
;;; :MODIFICATIONS <Timestamp: #{2009-10-26T11:19:18-04:00Z}#{09441} - by MON>
;;; :ADDED `make-symbol', `buffer-substring-no-properties', and docstring.
(defmacro mon-cat (file &optional from-psn to-psn)
  "Return FILE contents as string - like `cat'.\n
When optional args FROM-PSN and TO-PSN are non-nil these specify what portion
of the file to return. Signal an error if this range is not accessible.\n
:EXAMPLE\n\n\(mon-cat \"~/.emacs\"\)\n
\(mon-cat \"~/.emacs\" 10 253\)\n
:SEE \(man \"cat\"\)
:SEE info node `(coreutils)cat invocation'\n
:SEE-ALSO `insert-file-contents'.\n►►►"
  (let ((mc-fc (make-symbol "mc-fc")))
    `(let ((mc-fc (with-temp-buffer
                    (insert-file-contents ,file nil ,from-psn ,to-psn)
                    ;; :WAS (buffer-substring-no-properties (buffer-end 0) (buffer-end 1)) )))
                    (mon-buffer-sub-no-prop) )))
       ;; ??? ,mc-fc))) 
       mc-fc)))

;;; ==============================
;;; :CHANGESET 2261
;;; :CREATED <Timestamp: #{2010-11-03T20:02:34-04:00Z}#{10443} - by MON KEY>
(defun mon-file-truename-p (w-putative-file-truename &optional w-boolean-list-return)
  "Return non-nil if W-PUTATIVE-FILE-TRUENAME is an absolutely true filename.\n
This requires that arg W-PUTATIVE-FILE-TRUENAME satisfy the following constraints:\n
 - that it is `mon-string-not-null-nor-zerop';
 - that it is `file-name-absolute-p';
 - that it is not `file-symlink-p';
 - that it is existent per `file-exists-p';
 - that it is the string equivalent (as per `equal') of its `file-truename';\n
When all constraints are satisfied return W-PUTATIVE-FILE-TRUENAME, else nil.\n
When optional arg W-BOOLEAN-LIST-RETURN is non-nil return value is a list, such
that when all constraints are satisfied, car is `t' and cadr is the string
W-PUTATIVE-FILE-TRUENAME. In which case it has the format:\n
 \(t <W-PUTATIVE-FILE-TRUENAME>\)\n
and evaluating the cadr of return value should return non-nil.\n
When a constraint failed, car is `nil' and cadr is a cons. It has the format:\n
 (nil (<FAILED-CONSTRAINT> <W-PUTATIVE-FILE-TRUENAME>))\n
and evaluating the cadr of return value should return `nil'.\n
:EXAMPLE\n
\(file-truename default-directory\)
\(mon-file-truename-p default-directory\)
\(mon-file-truename-p default-directory t\)
\(eval \(cadr \(mon-file-truename-p default-directory t\)\)\)

\(mon-file-truename-p \"\"\)
\(mon-file-truename-p \"\" t\)
\(nth 0 \(cadr \(mon-file-truename-p \"\" t\)\)\)
\(file-truename \(nth 1 \(cadr \(mon-file-truename-p \"\" t\)\)\)\)
\(eval  \(cadr \(mon-file-truename-p \"\" t\)\)\)

\(mon-file-truename-p \"./\"\)
\(mon-file-truename-p \"./\" t\)
\(nth 0 \(cadr \(mon-file-truename-p \"./\" t\)\)\)
\(file-truename \(nth 1 \(cadr \(mon-file-truename-p \"./\" t\)\)\)\)
\(eval  \(cadr \(mon-file-truename-p \"./\" t\)\)\)

\(mon-file-truename-p \"../\"\)
\(mon-file-truename-p \"../\" t\)
\(nth 0 \(cadr \(mon-file-truename-p \"../\" t\)\)\)
\(file-truename \(nth 1 \(cadr \(mon-file-truename-p \"../\" t\)\)\)\)
\(eval  \(cadr \(mon-file-truename-p \"../\" t\)\)\)

\(file-truename \"~/\"\)
\(mon-file-truename-p \"~/\"\)
\(mon-file-truename-p \"~/\" t\)
\(nth 0 \(cadr \(mon-file-truename-p \"~/\" t\)\)\)
\(file-truename \(nth 1 \(cadr \(mon-file-truename-p \"~/\" t\)\)\)\)
\(eval \(cadr \(mon-file-truename-p \"~/\" t\)\)\)

\(setq tt--non-existent
      \(expand-file-name 
       \(format \"./no-friggin-way-i-exist-%d\" \(random\)\) 
       default-directory\)\)

\(mon-file-truename-p tt--non-existent\)
\(mon-file-truename-p tt--non-existent t\)
\(nth 0 \(cadr \(mon-file-truename-p tt--non-existent t\)\)\)
\(file-truename \(nth 1 \(cadr \(mon-file-truename-p tt--non-existent t\)\)\)\)
\(eval \(cadr \(mon-file-truename-p tt--non-existent t\)\)\)

\(unintern \"tt--non-existent\" obarray\)

:NOTE For various reasons Emacs \"pathmaiming\" functions may return unexpected
or non-intuitive values.  Individually this may not be a problem but when one or
more of Emacs' file/path related functions must interact things become slippery:
 \(equal \(file-truename \"\"\) default-directory\)\n
 \(equal \(file-truename \"\"\) \(directory-file-name default-directory\)\)\n
 \(let* \(\(empty-nm \"\"\)
        \(true-nm \(file-truename empty-nm\)\)\)
   \(format \(mapconcat #'identity
                      '\(\"\"
                        \"`empty-nm` string-value: %S\"
                        \"`true-nm`  string-value: %S\"
                        \"`empty-nm` `length': %d\"
                        \"`true-nm`  `lengty': %S\"
                        \"`empty-nm` and `true-nm`s `equal': %S\"
                        \"`empty-nm`  `file-directory-p': %S\"
                        \"`true-nm`   `file-directory-p': %S\"
                        \"`empty-nm`s `file-absolute-p': %S\"
                        \"`empty-nm`s `file-absolute-p': %S\"
                        \"`empty-nm`s `file-truename': %S\"
                        \"`true-nm`s  `file-truename': %S\"\)
                      \"n-- \"\)
           empty-nm true-nm 
           \(length empty-nm\) \(length true-nm\)
           \(equal empty-nm true-nm\)
           \(file-directory-p empty-nm\) \(file-directory-p true-nm\)
           \(file-name-absolute-p empty-nm\) \(file-name-absolute-p true-nm\)
           \(file-truename empty-nm\) \(file-truename true-nm\)\)\)\n
:ALIASED-BY `file-truename-p'
:SEE-ALSO `mon-file-dir-attributes->plist', `mon-file-reduce-name',
`mon-copy-files-in-sub-dirs', `mon-add-subdirs-to-list', `mon-build-path',
`*mon-add-subdirs-to-list-ignorables*',
`*regexp-add-subdirs-to-list-filter-ignorables*'.\n►►►"
  (let (mftp-rtn)
    (and 
     (or (mon-string-not-null-nor-zerop w-putative-file-truename)
         (and (setq mftp-rtn `(mon-string-not-null-nor-zerop ,w-putative-file-truename)) nil))
     (or (file-name-absolute-p w-putative-file-truename)
         (and (setq mftp-rtn `(file-name-absolute-p ,w-putative-file-truename)) nil))
     (or (not (file-symlink-p w-putative-file-truename))
         (and (setq mftp-rtn `(file-symlink-p ,w-putative-file-truename)) nil))
     (or (file-exists-p w-putative-file-truename)
         (and (setq mftp-rtn `(file-exists-p ,w-putative-file-truename)) nil))
     (or (equal (file-truename w-putative-file-truename) w-putative-file-truename)
         (and (setq mftp-rtn 
                    ;; `(equal ,w-putative-file-truename (file-truename ,(file-truename w-putative-file-truename))))
                    `(equal ,w-putative-file-truename (file-truename ,w-putative-file-truename)))
              nil))
     (setq mftp-rtn 'is-truename))
    (or (and w-boolean-list-return
             (cond ((eq mftp-rtn 'is-truename) `(t ,w-putative-file-truename))
                   (t  `(nil ,mftp-rtn))))
        (and (eq mftp-rtn 'is-truename)
             w-putative-file-truename))))

;;; ==============================
;;; :TODO return prop `:permissions' in octal. dired fncns should have this.
;;; `file-modes-symbolic-to-number', `file-modes'
;;; `sb-posix::define-stat-call' 
;;; sys/stat.h
;;; stat, lstat, fstat
;;; s_isreg, s_isdir, s_ischr, s_isblk, s_isfifo,
;;; s_islnk, s_issock,
;;; :CREATED <Timestamp: #{2010-06-11T18:42:42-04:00Z}#{10235} - by MON>
(defun mon-file-dir-attributes->plist  (file-dir-name)
  "Return a plist of as if by `file-attributes' for `file-dir-name'.\n
Properties returned are as follows:\n
 :stat-dir-p | :stat-syml-p\n :stat-gid-chg-p\n :stat-nlink
 :stat-mode\n :stat-size\n :stat-uid\n :stat-gid \n :stat-ino
 :stat-dev\n :stat-atime\n :stat-mtime\n :stat-ctime\n
Values returned by the time properties `:stat-*time' are in long-iso form e.g.:\n
 yyyy-mm-ddThh:mm:ss-zzzzZ
 \(format-time-string \"%Y-%m-%dT%T%zZ\"\)\n
:EXAMPLE\n\n\(file-attributes  user-emacs-directory\)\n
\(mon-file-dir-attributes->plist user-emacs-directory)\n
\(plist-get \(mon-file-dir-attributes->plist user-emacs-directory\) :stat-ctime\)\n
\(plist-get \(mon-file-dir-attributes->plist (getenv \"HOME\")\) :stat-dir-p\)\n
:NOTE May be funky on w32, not adapted for `w32-get-true-file-attributes'.\n
:NOTE Loosely modeled after SBCL's sb-posix:stat function.\n
:SEE :FILE sbcl/contrib/sb-posix/interface.lisp
:SEE info node `(libc)Attribute Meanings'.
:SEE info node `(coreutils)stat invocation'.
:SEE \(woman \"stat\"\)\n
:ALIASED-BY `mon-directory-attributes-plist'
:ALIASED-BY `mon-file-attributes-plist'
:ALIASED-BY `directory-attributes-plist'
:ALIASED-BY `file-attributes-plist'\n
:SEE-ALSO `mon-file-ensure-extension-is-el', `file-attributes-lessp'.\n►►►"
  (unless (or (file-directory-p file-dir-name)
              (file-exists-p file-dir-name))
    (error (concat ":FUNCTION `mon-file-dir-attributes->plist' "
                   "-- arg FILE-DIR-NAME non-existent, got: %S")
           file-dir-name))
  (multiple-value-bind  
      (stat-dir-p stat-nlink stat-uid stat-gid stat-atime stat-mtime stat-ctime
       stat-size stat-mode stat-gid-chg-p stat-ino stat-dev)
      (file-attributes file-dir-name)
    `(,@(if (stringp stat-dir-p)
            `(,:stat-syml-p ,stat-dir-p)
          `(,:stat-dir-p ,stat-dir-p))
      ;;:stat-dir-p ,stat-dir-p
      ,@(and `(,:stat-nlink ,stat-nlink))
      :stat-uid ,stat-uid
      :stat-gid ,stat-gid
      ,@`(:stat-atime ,(if stat-atime
                            (format-time-string "%Y-%m-%dT%T%zZ" stat-atime)))
      ,@`(:stat-mtime ,(if stat-mtime
                            (format-time-string "%Y-%m-%dT%T%zZ" stat-mtime)))
      ,@`(:stat-ctime ,(if stat-ctime
                            (format-time-string "%Y-%m-%dT%T%zZ" stat-ctime)))
      :stat-size ,stat-size
      :stat-mode ,stat-mode
      ;;: (file-modes user-emacs-directory)
      :stat-gid-chg-p ,stat-gid-chg-p
      :stat-dev ,stat-dev)))
;;
;;; :TEST-ME (file-attributes  user-emacs-directory)
;;; :TEST-ME (mon-file-dir-attributes->plist user-emacs-directory)
;;; :TEST-ME (mon-file-dir-attributes->plist (getenv "HOME"))
;;; :TEST-ME (plist-get (mon-file-dir-attributes->plist user-emacs-directory) :stat-ctime)

;;; ==============================
;;; :NOTE There is already the fncn`find-library-suffixes' which strips the
;;; ".elc" and ".elc.gz" suffixe from return value of
;;; `get-load-suffixes'. However, as a matter of principle, I find this equally
;;; problematic as it still leaves the ".el.gz" which is IMHO unclean and leads
;;; to any number of Emacs lisp related kludges where one has to recursively
;;; remove the second "extension" around any explicit checks for a non
;;; compressed file with an ".el" exstension (which be gzip'd or have other than
;;; a ".gz" extension). e.g. where ada-mode lives in
;;; lisp/progmodes/ada-mode.el.gz and we we want only "ada-mode.el" and don't
;;; know before hand if the file name returned is byte compiled with ".elc"
;;; extensions, regular with ".el" extension", or compressed with ".el.gz" and
;;; ".elc.gz"
;;;
;;; :CREATED <Timestamp: #{2010-07-10T12:36:32-04:00Z}#{10276} - by MON>
(defun mon-file-ensure-extension-is-el (w-ensured-file-name)
  "Locate library W-ENSURED-FILE-NAME only if it has an \".el\" extension.\n
Find the library as if by `locate-library'.\n
Return W-ENSURED-FILE-NAME's full path to library, when library exists and its
name does not end with one of the following extensions:\n
 \".elc\" \"elc.gz\" \".el.gz\"\n
:EXAMPLE\n\n(mon-file-ensure-extension-is-el \"ada-mode\"\)\n
:NOTE For additional discussion w/re the rationale for this function:
:SEE (URL `http://lists.gnu.org/archive/html/emacs-devel/2010-01/msg01060.html')\n
:SEE-ALSO `mon-file-truename-p', `mon-file-map-elisp-fileset',
`get-load-suffixes', `load-suffixes', `file-name-sans-extension',
`file-name-extension', `mon-help-file-dir-functions',
`mon-help-file-dir-functions-usage'.\n►►►"
  (let* ((fnd-lbr-nm (and (stringp w-ensured-file-name)
                          (file-name-sans-extension w-ensured-file-name)))
         (fnd-lbr (locate-library fnd-lbr-nm)))
    ;; We found W-ENSURED-FILE-NAME.* the extension is anything in (get-load-suffixes)
    (unless (null fnd-lbr)
      ;; Is it an .elc? If so, look again.
      (when (string-equal (file-name-extension fnd-lbr) "elc")
        (setq fnd-lbr (locate-library (concat fnd-lbr-nm ".el")))
        ;; File is either an ".el.gz" or ".elc.gz" so we need to:
        ;;  a) strip the .gz 
        ;;  b) test if the resulting file ada-mode.elc actually exists
        (unless (null fnd-lbr)
          (when (string-equal (file-name-extension fnd-lbr) "gz")
            (setq fnd-lbr (if (file-exists-p (file-name-sans-extension fnd-lbr))
                              (file-name-sans-extension fnd-lbr)
                            fnd-lbr))))
        (cond ((and (string-equal (file-name-extension fnd-lbr) "el")
                    (file-exists-p fnd-lbr)) 
               (file-truename fnd-lbr))
              ((or (null fnd-lbr)
                   (string-equal (file-name-extension fnd-lbr) "elc")
                   (string-equal (file-name-extension fnd-lbr) "gz"))
               (error (concat ":FUNCTION `mon-file-ensure-extension-is-el' "
                              "-- arg W-ENSURED-FILE-NAME does not exist as '%s.el'%s")
                      fnd-lbr-nm                      
                       (if (null fnd-lbr)
                           ""
                         (format "\n%38cdid locate a similarly named file:\n%38c'%s'"
                                 32 32 fnd-lbr)))))))))

;;; ==============================
;;; :MODIFICATIONS <Timestamp: #{2009-10-26T12:58:23-04:00Z}#{09441} - by MON KEY>
(defun mon-toggle-dired-dwim-target (&optional intrp)
  "Toggle `dired-dwim-target'.\n
:ALIASED-BY `mon-dired-toggle-dwim-target'\n
:SEE-ALSO `dired-dwim-target-directory', `mon-toggle-case-query-user',
`mon-toggle-case-regexp', `mon-toggle-case-regexp-region',
`mon-toggle-eval-length', `mon-toggle-menu-bar',
`mon-toggle-read-only-point-motion', `mon-toggle-restore-llm',
`mon-toggle-show-point-mode', `mon-toggle-truncate-line'.\n►►►"
  (interactive "p")
  (let ((toggle-dwim-target
         (if (bound-and-true-p dired-dwim-target)
             (progn
               (setq dired-dwim-target nil)
               (when intrp (message (concat "Variable `dired-dwim-target' turned off\n"
                                            "-- Call `mon-toggle-dired-dwim-target' to toggle on"))))
             (progn
               (setq dired-dwim-target t)
               (when intrp (message (concat "Variable `dired-dwim-target' turned on\n"
                                            "-- Call `mon-toggle-dired-dwim-target' to toggle off")))))))
    toggle-dwim-target))
;;
;;; :TEST-ME (mon-toggle-dired-dwim-target)
;;; :TEST-ME (mon-toggle-dired-dwim-target)

;;; ==============================
;;; :NOTE when using `ido-mode' with 'everywhere there is a `command-remapping'
;;; for `find-file-other-frame' -> `ido-find-file-other-frame' to bind "\C-x5f"
;;; you may need to put this on the `dired-mode-hook' first:
;;;
;;; (set (make-local-variable 'minor-mode-overriding-map-alist)
;;;        `(ido-mode keymap 
;;;                   (remap ,@(remove '(find-file-other-frame . ido-find-file-other-frame)
;;;                                    (cdaddr ido-minor-mode-map-entry)))))
;;; :CREATED <Timestamp: #{2010-08-26T13:13:53-04:00Z}#{10344} - by MON>
(defun mon-dired-find-file-other-frame ()
  "In dired-mode buffer visit file-at point in another frame.\n
:ALIASED-BY `dired-find-file-other-frame'\n
:SEE-ALSO `mon-find-buffer-visiting-other-live-frame',
`mon-frame-live-visible-graphic-p' `dired-get-file-for-visit',
`dired-find-file-other-window', `find-file-other-frame', `visible-frame-list',
`find-file', `select-frame-set-input-focus'
`window--display-buffer-1', `window--display-buffer-2'.\n►►►"
  (interactive)
  (let ((other-frame-file (dired-get-file-for-visit)))
    (unless (mon-find-buffer-visiting-other-live-frame other-frame-file)
      (let ((fnd-bv (find-buffer-visiting other-frame-file))
            (vfl (visible-frame-list)))
        (cond ((and fnd-bv (> (length vfl) 1))
               (with-selected-frame (car (remove (selected-frame) (visible-frame-list)))
                 (select-frame-set-input-focus (selected-frame))
                 (with-current-buffer fnd-bv
                   (switch-to-buffer (current-buffer)))))
              ((> (length vfl) 1) 
               (with-selected-frame (car (remove (selected-frame) (visible-frame-list)))
                 (select-frame-set-input-focus (selected-frame))
                 (find-file other-frame-file)))
              (t (find-file-other-frame other-frame-file)))))))

;;; ==============================
;;; :PREFIX "mflvgp-"
;;; :CHANGESET 2091
;;; :CREATED <Timestamp: #{2010-08-26T14:46:20-04:00Z}#{10344} - by MON KEY>
(defun mon-frame-live-visible-graphic-p (test-frame)
  "Whether TEST-FRAME is live, visible, not iconified, and not invisible.
Return cons (TEST-FRAME . { t | nil } when TEST-FRAME satisfies the predicate
`frame-live-p' where a live frame has the type either `x` or `w32` and the
predicate `frame-visible-p' returns non-nil with a visible frame not of type
`icon`.\n
:EXAMPLE\n\n\(mon-frame-live-visible-graphic-p \(selected-frame\)\)\n
\(mapcar  #'mon-frame-live-visible-graphic-p \(frame-list\)\)\n
\(filtered-frame-list #'mon-frame-live-visible-graphic-p\)\n
:ALIASED-BY `frame-live-visible-graphic-p'\n
:SEE-ALSO `mon-find-buffer-visiting-other-live-frame',
`dired-find-file-other-window', `filtered-frame-list', `make-frame-names-alist',
`visible-frame-list', `mon-help-frame-functions'.\n►►►"
  (let ((mflvgp-flp (frame-live-p test-frame))
        (mflvgp-fvp (frame-visible-p test-frame)))
    (if (and mflvgp-flp (memq mflvgp-flp '(x w32)) 
             (not (null mflvgp-fvp))
             (not (eq mflvgp-fvp 'icon)))
        (cons test-frame  t)
      (list test-frame))))
;;
;; :TEST-ME (mon-frame-live-visible-graphic-p (selected-frame))
;; :TEST-ME (mapcar  #'mon-frame-live-visible-graphic-p (frame-list)) 
;; :TEST-ME (filtered-frame-list #'mon-frame-live-visible-graphic-p)

;;; ==============================
;;; :CHANGESET 2091
;;; :CREATED <Timestamp: #{2010-08-30T16:02:36-04:00Z}#{10351} - by MON KEY>
(defun mon-find-buffer-visiting-other-live-frame (file-to-find)
  "Find buffer visiting FILE-TO-FIND on some other live frame and visit it now.\n
When there is not a buffer visiting `file-to-find' or variable `multiple-frames'
is null return nil.\n
:ALIASED-BY `find-buffer-visiting-other-live-frame'
:ALIASED-BY `mon-get-buffer-visiting-other-live-frame'
:ALIASED-BY `get-buffer-visiting-other-live-frame'\n
:SEE-ALSO `mon-frame-live-visible-graphic-p'.\n►►►"
  (let ((fbv (find-buffer-visiting file-to-find
                                   #'(lambda (bfr) 
                                       (window-live-p
                                        (get-buffer-window bfr 'visible))))))
    (unless (or (not multiple-frames)
                (eq (selected-frame) (window-frame (get-buffer-window fbv 'visible))))
      (select-frame-set-input-focus (window-frame (get-buffer-window fbv 'visible)))
      t)))

;;; ==============================
;;; :CREATED <Timestamp: #{2009-10-11T10:18:36-04:00Z}#{09417} - by MON>
(defun mon-copy-file-dired-as-list (&optional localp intrp)
  "Copy dired file\(s\) to kill-ring as a list of strings.
When no file-names are marked copy file-name at point. 
If one or more file-names are marked copy these to kill-ring.
When LOCALP is non-nil or called-interactively with prefix arg
do not copy full path of files to kill-ring.\n
:ALIASED-BY `mon-dired-kill-files-to-list'\n
:ALIASED-BY `mon-dired-copy-files-to-list'\n
:SEE-ALSO `mon-copy-file-dired-as-string', `mon-copy-file-path',
`mon-w3m-dired-file', `mon-dired-unmark-elc', `mon-file-ensure-extension-is-el'
`mon-get-text-properties-region-to-kill-ring'.\n►►►"
  (interactive "P\np")
  (let ((dgmf (dired-get-marked-files localp)))
    (if intrp 
        (kill-new (pp dgmf))
        dgmf)))

;;; ==============================
;;; :PREFIX "mcfdas-"
;;; :CREATED <Timestamp: #{2009-10-11T10:18:40-04:00Z}#{09417} - by MON>
(defun mon-copy-file-dired-as-string (&optional localp qt-strings intrp)
  "Copy dired file\(s\) to kill-ring.\n
When no file-names are marked copy file-name at point. 
If one or more file-names are marked copy these to kill-ring.
When LOCALP is non-nil do not copy full path of files to kill-ring.
When QT-STRINGS is non-nil or called-interactively with prefix arg
copy file-names such that when yanked they are inserted as quoted strings.\n
:ALIASED-BY `mon-dired-kill-files-to-strings'
:ALIASED-BY `mon-dired-copy-files-to-strings'\n
:SEE-ALSO `mon-copy-file-dired-as-list', `mon-copy-file-path',
`mon-dired-unmark-elc', `mon-w3m-dired-file',
`mon-get-text-properties-region-to-kill-ring',
`mon-file-dir-attributes->plist', `mon-file-ensure-extension-is-el'.\n►►►"
  (interactive "i\nP\np")
  (let ((mcfdas-dir-lst (mon-copy-file-dired-as-list localp)))
    (setq mcfdas-dir-lst
          (if qt-strings
              (mapconcat #'(lambda (mcfdas-L-1) (format "%S" mcfdas-L-1)) mcfdas-dir-lst "\n")
            (mapconcat #'identity mcfdas-dir-lst "\n")))
    (if intrp
        (progn    
          (kill-new mcfdas-dir-lst)
          (message mcfdas-dir-lst))
        mcfdas-dir-lst)))

;;; ==============================
;;; :TODO fix command remapping in docstriing
;;; :CREATED <Timestamp: #{2010-03-30T13:54:10-04:00Z}#{10132} - by MON KEY>
(defun mon-dired-other-window ()
  "Invoke dired with `default-directory' in other-window.\n
:NOTE globaly bound to C-xd M-o in :FILE mon-keybindings.el.\n
:SEE-ALSO `mon-dired-uninsert-subdir', `mon-dired-uninsert-subdir-all',
`ido-dired', `mon-dired-copy-files-to-list', `mon-dired-copy-files-to-strings',
`mon-dired-kill-files-to-list', `mon-dired-kill-files-to-strings',
`mon-dired-nef-dir', `mon-dired-srt-alph', `mon-dired-srt-chrn',
`mon-dired-srt-type', `mon-dired-srt-type-alph', `mon-dired-srt-type-chrn',
`mon-dired-unmark-elc'.\n►►►"
  (interactive)
  (dired-other-window default-directory))

;;; ==============================
;;; :PREFIX "mgds-"
;;; :CREATED <Timestamp: #{2010-04-25T13:09:48-04:00Z}#{10167} - by MON KEY>g
(defun mon-get-dir-size (get-dir-size &optional insrtp intrp)
  "Get the size of directory DIR as if by `du -h --max-depth=1'.\n
When called-interactively or optional arg INSRTP non-nil insert DIR size in current-buffer.
Does not move point.\n
:ALIASED-BY`mon-directory-get-size'\n
:EXAMPLE\n\n(mon-get-dir-size default-directory)\n
:SEE-ALSO `mon-file-dir-attributes->plist', `mon-help-du-incantation',
`mon-help-unix-commands'./n►►►"
  (interactive "DGet Directory Size: ")
  (if (not (executable-find "du"))
      (error (concat ":FUNCTION `mon-get-dir-size' "
                     "-- `du' executable not in path"))
    (let* ((mgds-dir (directory-file-name (file-truename get-dir-size))) ;; (file-directory-p "~/bubba")
           (mgds-size 
            (if (file-directory-p mgds-dir)
                (shell-command-to-string (format "du -h --max-depth=1 %s" mgds-dir))
              (error (concat ":FUNCTION `mon-get-dir-size' " 
                             "-- arg GET-DIR-SIZE does not name exististing directory")))))
      (if (or insrtp intrp)
          (save-excursion 
            (newline) 
            (princ mgds-size (current-buffer)))
        mgds-size))))
;;
;;; :TEST-ME (mon-get-dir-size (file-truename "~/mon-scripts/"))
;;; :TEST-ME (mon-get-dir-size "~/mon-scripts" t)

;;; ==============================
(defun mon-dired-srt-alph ()
  "Set ls switch to sort Dired direcotry alphebetically.\n
:SEE-ALSO `mon-dired-uninsert-subdir', `mon-dired-uninsert-subdir-all',
`dired-sort-toggle-or-edit', `mon-dired-srt-chrn',
`mon-dired-srt-type', `mon-dired-srt-type', `mon-dired-srt-type-alph',
`mon-dired-srt-type-chrn', `mon-dired-other-window', `mon-dired-unmark-elc',
`mon-dired-insert-dirs-recursive', `mon-dired-up-directory-this-buffer'.\n►►►"
  (interactive)
  (dired-sort-other "-la"))

;;; ==============================
(defun mon-dired-srt-chrn ()
  "Set ls switch to sort Dired direcotry chronologically.\n
:SEE-ALSO `mon-dired-uninsert-subdir', `mon-dired-uninsert-subdir-all',
`dired-sort-toggle-or-edit', `mon-dired-srt-alph',
`mon-dired-srt-type', `mon-dired-srt-type', `mon-dired-srt-type-alph',
`mon-dired-srt-type-chrn', `mon-dired-other-window', `mon-dired-unmark-elc',
`mon-dired-insert-dirs-recursive', `mon-dired-up-directory-this-buffer'.\n►►►"
  (interactive)
  (dired-sort-other "-lt")) ;;mon-dired-srt-alph

;;; ==============================
(defun mon-dired-srt-type ()
  "Set ls switch to sort Dired direcotry by type.\n
:SEE-ALSO `mon-dired-uninsert-subdir', `mon-dired-uninsert-subdir-all',
`dired-sort-toggle-or-edit', `mon-dired-srt-alph', `mon-dired-srt-chrn',
`mon-dired-srt-type', `mon-dired-srt-type-alph', `mon-dired-srt-type-chrn',
`mon-dired-other-window', `mon-dired-insert-dirs-recursive', 
`mon-dired-up-directory-this-buffer', `mon-dired-unmark-elc'.\n►►►"
  (interactive)
  (dired-sort-other "-lX"))

;;; ==============================
(defun mon-dired-srt-type-alph ()
  "Set ls switch to sort Dired direcotry by type -> alphabetically.\n
:SEE-ALSO `mon-dired-uninsert-subdir', `mon-dired-uninsert-subdir-all',
`dired-sort-toggle-or-edit', `mon-dired-srt-alph', `mon-dired-srt-chrn',
`mon-dired-srt-type', `mon-dired-srt-type', `mon-dired-srt-type-chrn',
`mon-dired-other-window', `mon-dired-insert-dirs-recursive',
`mon-dired-up-directory-this-buffer', `mon-dired-unmark-elc',\n►►►"
  (interactive)
  (dired-sort-other "-lXa"))

;;; ==============================
(defun mon-dired-srt-type-chrn ()
  "Set ls switch to sort Dired direcotry by type -> chronologically.\n
:SEE-ALSO `mon-dired-uninsert-subdir', `mon-dired-uninsert-subdir-all',
`dired-sort-toggle-or-edit', `mon-dired-srt-alph',
`mon-dired-srt-chrn', `mon-dired-srt-type', `mon-dired-srt-type',
`mon-dired-srt-type-alph', `mon-dired-other-window',
`mon-dired-insert-dirs-recursive', `mon-dired-up-directory-this-buffer'.\n►►►"
  (interactive)
  (dired-sort-other "-lXt"))

;;; ==============================
;;; :PREFIX "mdudtb-"
;;; :COURTESY Stefan Reichor :HIS xsteve-functions.el :VERSION 2001-03-28
;;; :WAS `mon-dired-up-directory-this-buffer'
;;; :NOTE (define-key dired-mode-map "\177" 'mon-dired-up-directory-this-buffer)
(defun mon-dired-up-directory-this-buffer ()
  "Move up directory tree i.e. `../' to a new dired buffer killing current one.\n
:ALIASED-BY `dired-up-here'\n
:SEE-ALSO `dired-up-directory', `mon-dired-uninsert-subdir',
`mon-dired-uninsert-subdir-all' `mon-dired-srt-alph', `mon-dired-srt-type-alph',
`mon-dired-srt-chrn', `mon-dired-srt-type', `mon-dired-srt-type',
`mon-dired-srt-type-chrn', `mon-dired-other-window',
`mon-dired-insert-dirs-recursive'.\n►►►"
  (interactive)
  ;;  (dired-current-directory)
  (let ((mdudtb-bfr))
    (setq mdudtb-bfr (current-buffer))
    (dired-up-directory)
    (kill-buffer mdudtb-bfr)))

;;; ==============================
;;; :CHANGESET 1742
;;; :CREATED <Timestamp: #{2010-05-21T18:04:31-04:00Z}#{10205} - by MON KEY>
(defun mon-dired-uninsert-subdir ()
  "Don't show the inserted subdir in dired any longer.
Dired's API for doing this:
`dired-do-kill-lines' <- Most fucking moronic fncn name ever IMHO.
Also, it doesn't DTRT when point is at a file inside subdir. I still wan't the inserted gone!\n
:ALIASED-BY `dired-uninsert-subdir'
:ALIASED-BY `dired-subdir-uninsert'\n
:SEE-ALSO `dired-sort-toggle-or-edit', `mon-dired-srt-alph',
`mon-dired-srt-chrn', `mon-dired-srt-type', `mon-dired-srt-type',
`mon-dired-srt-type-alph', `mon-dired-other-window', `mon-dired-unmark-elc',
`mon-dired-insert-dirs-recursive', `mon-dired-up-directory-this-buffer'.\n►►►"
  (interactive)
  (goto-char (marker-position 
              (cdr (assoc-string (dired-current-directory) dired-subdir-alist))))
  (if (dired-subdir-hidden-p (dired-current-directory))
      (dired-unhide-subdir)
    (dired-do-kill-lines t)))

;;; ==============================
;;; :CHANGESET 1742
;;; :CREATED <Timestamp: #{2010-05-21T19:52:27-04:00Z}#{10205} - by MON KEY>
(defun mon-dired-uninsert-subdir-all ()
  "Uninsert all inserted subdirs in current dired buffer.\n 
Like `mon-dired-uninsert-subdir'.\n
:ALIASED-BY `dired-uninsert-subdir-all'
:ALIASED-BY `dired-subdir-uninsert-all'
:SEE-ALSO `dired-sort-toggle-or-edit', `mon-dired-srt-alph',
`mon-dired-srt-chrn', `mon-dired-srt-type', `mon-dired-srt-type',
`mon-dired-srt-type-alph', `mon-dired-other-window', `mon-dired-unmark-elc',
`mon-dired-insert-dirs-recursive', `mon-dired-up-directory-this-buffer'.\n►►►"
  (interactive)
  (let (chk-tp-mrk)
    (while (unless (or (eq (setq chk-tp-mrk (marker-position (cdar dired-subdir-alist)))
                           (mon-g2be -1 t))
                       (equal (caar dired-subdir-alist)
                              (directory-file-name (file-truename default-directory))))
             chk-tp-mrk)
      (save-excursion 
        (goto-char chk-tp-mrk)
        (mon-dired-uninsert-subdir)))))

;;; ==============================
;;; :COURTESY Stefan Reichor, :HIS xsteve-functions.el :VERSION 2001-03-28
;;; :WAS `mon-dired-insert-dirs-recursive'
;;; :NOTE (define-key dired-mode-map [(meta i)] 'mon-dired-insert-dirs-recursive)
(defun mon-dired-insert-dirs-recursive (dirname)
  "In dired recursively inserts the subdirs of DIRNAME at point.\n
:SEE-ALSO `mon-dired-srt-alph', `mon-dired-srt-type-alph', `mon-dired-srt-chrn',
`mon-dired-srt-type', `mon-dired-srt-type', `mon-dired-srt-type-chrn',
`mon-dired-other-window', `mon-dired-up-directory-this-buffer', 
`mon-dired-unmark-elc'.\n►►►"
  (interactive (list (dired-get-filename)))
  (dired-maybe-insert-subdir dirname "-laR"))

;;; ==============================
;;; :CREATED <Timestamp: #{2010-05-27T14:17:28-04:00Z}#{10214} - by MON KEY>
(defun mon-dired-unmark-elc  (&optional unmark-regexp intrp)
  "Unmark the .elc files matched by `dired-mark-files-regexp'.\n
When optional arg UNMARK-REGEXP is non-nil it is a regexp to unmark.\n
When called-interactively with prefix arg or read regexp.\n
:SEE-ALSO `dired-flag-extension', `mon-dired-copy-files-to-list',
`mon-dired-copy-files-to-strings', `mon-dired-insert-dirs-recursive',
`mon-dired-kill-files-to-list', `mon-dired-kill-files-to-strings',
`mon-dired-naf-artist-letter', `mon-dired-naf-brand-letter',
`mon-dired-naf-image-dir', `mon-dired-nef-dir', `mon-dired-other-window',
`mon-dired-srt-alph', `mon-dired-srt-chrn', `mon-dired-srt-type',
`mon-dired-srt-type-alph', `mon-dired-srt-type-chrn',
`mon-dired-uninsert-subdir', `mon-dired-uninsert-subdir-all',
`mon-dired-up-directory-this-buffer'.\n►►►"
  (interactive "P\np")
  (when current-prefix-arg 
    (setq unmark-regexp ;; (read-regexp "unmark files matching regexp: ")))
          (read-regexp (concat ":FUNCTION `mon-dired-unmark-elc' "
                               "-- unmark files matching regexp: "))))
  (dired-mark-files-regexp (or unmark-regexp "\.elc$") ?\040))

;; (dired-mark-extension "el" 69) ;; (string-to-char "E") 69
;; (dired-do-search (read-regexp "-- regexp to search: ")
;; (dired-do-search
;; ( (dired-subdir-alist (dired-current-directory) dired-directory

;; (dired-mark-extension "el") extension dired-del-marker)
;; (char-to-string dired-del-marker)

;;; ==============================
;;; :RENAMED `mon-new-buffer-w-stamp' -> `mon-get-new-buffer-w-stamp'
;;; :REQUIRES `mon-file-stamp-vrfy-put-eof' -> :FILE mon-time-utils.el
;;; :REQUIRES `mon-file-stamp'              -> :FILE mon-time-utils.el
;;; :SEE (URL `http://www.emacswiki.org/emacs/mon-time-utils.el')
;;; :CREATED <Timestamp: #{2009-12-18T21:51:32-05:00Z}#{09515} - by MON>
(defun mon-get-new-buffer-w-stamp (new-buffer-w-name &optional auto-save intrp)
  "Create and display NEW-BUFFER-W-NAME pre-populated with `mon-file-stamp'.\n
The pre-filled line `:FILE' line is formatted as:\n
\x3B;; :FILE default-directory/NEW-BUFFER-W-NAME\n
:NOTE The file is not written yet. Save it yourself if that is what you want.
When called-interactively prompts for NEW-BUFFER-W-NAME.\n
When AUTO-SAVE is non-nil or called-interactively with prefix-arg 
do not prompt before saving NEW-BUFFER-W-NAME.\n
If NEW-BUFFER-W-NAME is an existing file in default-directory signal an error.\n
:EXAMPLE\n(call-interactively 'mon-get-new-buffer-w-stamp)\n
:ALIASED-BY `mon-buffer-get-new-w-stamp'\n
:SEE-ALSO `mon-file-stamp-buffer-filename', `mon-file-stamp',
`mon-file-stamp-minibuffer', `mon-insert-file-template', `mon-lisp-stamp',
`mon-stamp-in-context', `mon-timestamp', `mon-stamp', `mon-accessed-stamp',
`mon-file-dir-attributes->plist'.\n►►►"
  (interactive "i\nP\np")
  (let ((nbwn (make-symbol "--nbwn--"))
        nb-name)
    (setq nb-name (if intrp 
                      (read-string  (concat ":FUNCTION `mon-get-new-buffer-w-stamp' "
                                            "-- new buffer-file-name: "))
                    new-buffer-w-name))
    (if (file-exists-p (concat default-directory nb-name))
        (error (concat ":FUNCTION `mon-get-new-buffer-w-stamp' "
                       "-- pre-existing file %s in %s") 
               nb-name (pwd))
      (setq nb-name (concat (expand-file-name 
                             (directory-file-name 
                              (file-name-directory default-directory)))
                            "/" nb-name)))
    (with-current-buffer (get-buffer-create (format "%s" nbwn))
      (set-visited-file-name nb-name)
      (mon-file-stamp t))
    (switch-to-buffer (get-buffer (file-name-nondirectory nb-name)))
    (if auto-save
        (progn (write-file (buffer-file-name) t)
               (buffer-file-name))
      (when (yes-or-no-p 
             (format (concat  ":FUNCTION `mon-get-new-buffer-w-stamp' "
                              "-- buffer visiting the unwritten file `%s'.\n"
                              "Save now (Y) or proceed without saving (N)?: ")
                     (file-relative-name (buffer-file-name) "../")))
        (write-file (buffer-file-name) t)
        (buffer-file-name)))))
;;
;;; :TEST-ME (mon-get-new-buffer-w-stamp "testing")
;;; :TEST-ME (call-interactively 'mon-get-new-buffer-w-stamp)

;;; ==============================
;;; :PREFIX "mgdna-"
;;; :CREATED <Timestamp: #{2009-10-27T15:50:08-04:00Z}#{09442} - by MON>
(defun mon-get-dir-name-absolute (dir-name)
  "Return absolute directory file-name of DIR-NAME.\n
:EXAMPLE\n(mon-get-dir-name-absolute \(getenv \"HOME\"\)\)\n
:ALIASED-BY `mon-dir-name-absolute'\n
:SEE-ALSO `mon-copy-file-multiple', `mon-copy-files-in-sub-dirs',
`mon-get-relative-w-absolute' `mon-get-dir-name-absolute',
`mon-file-reduce-name', `mon-build-path', `mon-string-split-buffer-name',
`mon-get-buffer-parent-dir' `mon-string-split-buffer-parent-dir',
`mon-string-split-dir-recurse', `mon-dir-common-paths',
`mon-get-buffer-parent-dir', `mon-get-proc-buffers-directories',
`mon-get-buffers-directories', `mon-file-dir-attributes->plist',
`mon-string-split-buffer-parent-dir-quick',
`mon-string-split-buffer-parent-dir', `mon-file-ensure-extension-is-el',
`file-truename' `expand-file-name', `directory-file-name',
`file-name-directory', `file-relative-name'.\n►►►"
  (let ((mgdna-trnm (file-truename dir-name)))
    (if (file-directory-p mgdna-trnm)
        (directory-file-name mgdna-trnm)
      (directory-file-name (file-name-directory mgdna-trnm)))))
;;
;;; (file-truename "")
;;; :TEST-ME (mon-get-dir-name-absolute "../")
;;; :TEST-ME (mon-get-dir-name-absolute "./")
;;; :TEST-ME (mon-get-dir-name-absolute "~/")
;;; :TEST-ME (mon-get-dir-name-absolute default-directory)
;;; :TEST-ME (mon-get-dir-name-absolute (buffer-file-name))
;;; :TEST-ME (mon-get-dir-name-absolute (concat (getenv "HOME")"\\"))
;;; :TEST-ME (mon-get-dir-name-absolute (getenv "HOME"))

;;; ==============================
;;; :PREFIX "mgrwa-"
;;; :CREATED <Timestamp: #{2009-12-30T18:26:28-05:00Z}#{09533} - by MON KEY>
(defun mon-get-relative-w-absolute (match-pattern file-or-dir)
  "Return FILE-OR-DIR as a two element list of strings split on MATCH-PATTERN.\n
Useful for comparing a path difference by diffing the elets of list1 with list2.\n
:EXAMPLE\n
\(mon-get-relative-w-absolute \(file-truename \(getenv \"HOME\"\)\) default-directory\)\n
:ALIASED-BY `mon-dir-name-relative-w-absolute'\n
:SEE-ALSO `mon-copy-file-multiple', `mon-copy-files-in-sub-dirs',
`mon-get-relative-w-absolute' `mon-get-dir-name-absolute',
`mon-file-reduce-name', `mon-build-path', `mon-string-split-buffer-name',
`mon-get-buffer-parent-dir', `mon-file-dir-attributes->plist'
`mon-string-split-dir-recurse', `mon-dir-common-paths',
`mon-get-buffer-parent-dir', `mon-get-proc-buffers-directories',
`mon-get-buffers-directories', `mon-string-split-buffer-parent-dir-quick',
`mon-string-split-buffer-parent-dir', `file-relative-name'.\n►►►"
  (unwind-protect  
      (let ((mgrwa-fod (if (equal (string-match-p "[A-z]:\\\\?" file-or-dir) 0)
                           (file-truename file-or-dir)
                         file-or-dir)))
        (cond ((or (string= (mon-get-dir-name-absolute mgrwa-fod)
                            (if (and (not (file-directory-p mgrwa-fod))
                                     (file-exists-p mgrwa-fod))
                                (mon-get-dir-name-absolute mgrwa-fod)))
                   (string= "~/" match-pattern)
                   (string= "~/" (substring mgrwa-fod 0 2)))
               (let ((mgrwa-abslt (mon-get-dir-name-absolute mgrwa-fod)))
                 (save-match-data
                   `(,(split-string mgrwa-abslt "/" t) ,(split-string mgrwa-abslt "/" t)))))
              ((string-match match-pattern mgrwa-fod)
               `(,(save-match-data
                    (split-string (substring mgrwa-fod (match-end 0)) "/" t)) 
                 ,(save-match-data
                    (split-string (mon-get-dir-name-absolute
                                   (substring mgrwa-fod (match-beginning 0)(match-end 0)))
                                  "/" t))))))
    (set-match-data nil)))
;;
;;; :TEST-ME (mon-get-relative-w-absolute "" (getenv "HOME"))
;;; :TEST-ME (mon-get-relative-w-absolute "~/"  (buffer-file-name))
;;; :TEST-ME (mon-get-relative-w-absolute ""  (buffer-file-name))
;;; :TEST-ME (mon-get-relative-w-absolute "c:/" default-directory)
;;; :TEST-ME (mon-get-relative-w-absolute "" default-directory)
;;; :TEST-ME (mon-get-relative-w-absolute "" 
;;;            (file-relative-name (buffer-file-name) (getenv "HOME") ))
;;; :TEST-ME (mon-get-relative-w-absolute 
;;;           (file-truename (getenv "HOME")) default-directory)

;;; ==============================
;;; :PREFIX "mcfisd-"
;;; :MODIFICATIONS <Timestamp: #{2010-02-04T15:31:47-05:00Z}#{10054} - by MON KEY>
;;; :CHANGED `mapcar's -> `mapc's :ADDED local var WHAT-HPPD for return value.
;;; :CREATED <Timestamp: Monday June 22, 2009 @ 02:54.20 PM - by MON>
(defun mon-copy-files-in-sub-dirs (gather-dir destination-dir)
  "For sub-directories \(SD\) of GATHR-DIR, copy files of SD to DESTINATION-DIR.\n
GATHER-DIR and DESTINATION-DIR are full paths.\n
Copy _only_ files not directories.\n
Does not recursively descend SD sub-directories.
Does not copy sub-directories of SD to DESTINATION-DIR.\n
Return a list of strings informing what happened, elts of list have the form:\n
  \"Copied file: <FILE> to Directory: <DIRECTORY>\"\n
:ALIASED-BY `mon-file-copy-in-sub-dirs'\n
:SEE-ALSO `mon-copy-file-multiple', `mon-copy-files-in-sub-dirs',
`mon-get-buffer-parent-dir', `mon-get-proc-buffers-directories',
`mon-get-buffers-directories', `mon-string-split-buffer-name',
`mon-string-split-buffer-parent-dir-quick', `mon-string-split-buffer-parent-dir',
`mon-file-dir-attributes->plist', `mon-file-ensure-extension-is-el'.\n►►►"
  ;; First, gather sub-dirs.
  (let* ((mcfisd-gthr-dir (if (and 
                               (file-exists-p gather-dir) 
                               (file-directory-p gather-dir)
                               ;; :ADDED <Timestamp: #{2010-11-03T19:27:17-04:00Z}#{10443} - by MON KEY>
                               (not (file-symlink-p gather-dir)))
                       (directory-file-name gather-dir)
                     (error (concat 
                             ":FUNCTION `mon-copy-files-in-sub-dirs' "
                             "-- arg GATHER-DIR non-existent or names a file: \n%s") 
                            gather-dir)))
         (mcfisd-to-dir (if (and (file-exists-p destination-dir) (file-directory-p destination-dir))
                     (directory-file-name destination-dir)
                   (error (concat 
                           ":FUNCTION `mon-copy-files-in-sub-dirs' "
                           "-- arg DESTINATION-DIR non-existent or names a file: \n%s")
                          destination-dir)))
         ;; (mcfisd-to-dir-fname (car (last (split-string mcfisd-to-dir "/" t))))
         (mcfisd-gthr-in mcfisd-gthr-dir)
         (mcfisd-wlk-dir (directory-files mcfisd-gthr-in t))
         mcfisd-gthrd 
         mcfisd-what-hppd)
    (setq mcfisd-gthrd ())
    (mapc #'(lambda (mcfisd-L-1)
              (let ((in-here (if (and (file-directory-p mcfisd-L-1)
                                      (not (or (string= (concat mcfisd-gthr-in "/.") mcfisd-L-1)
                                           (string= (concat mcfisd-gthr-in "/..") mcfisd-L-1)
                                           (string= mcfisd-to-dir mcfisd-L-1))))
                                 mcfisd-L-1)))
                (when in-here  (setq mcfisd-gthrd (cons in-here mcfisd-gthrd))))) mcfisd-wlk-dir)
    ;; Now copy files per subdir to dest-dir.
    (mapc #'(lambda (mcfisd-L-2) 
              (let* ((mcfisd-L-in-dir mcfisd-L-2)
                     (mcfisd-L-wlk-sub (directory-files mcfisd-L-in-dir t)))
                ;; :WAS (mapcar #'(lambda (mcfisd-L-3)
                (mapc #'(lambda (mcfisd-L-3)
                          (when (and (not (file-directory-p mcfisd-L-3)) 
                                     (not (or (string= (concat mcfisd-L-in-dir "/" ".") mcfisd-L-3) 
                                              (string= (concat mcfisd-L-in-dir "/" "..") mcfisd-L-3))))
                            (push (format "Copied file: %s to Directory: %s" mcfisd-L-3 mcfisd-to-dir) 
                                  mcfisd-what-hppd)
                            (copy-file mcfisd-L-3 mcfisd-to-dir t)))
                      mcfisd-L-wlk-sub)))
          mcfisd-gthrd)
    (setq mcfisd-what-hppd (nreverse mcfisd-what-hppd))))

;;; ==============================
;;; :COURTESY Thierry Volpiatto :HIS tv-utils.el :WAS `lmcp'
(defun mon-copy-file-multiple (file &optional list-of-dir)
  "Copy `file' in multiple directories.\n
At each prompt for a directory add + to input another directory name.
When '+' is not added to directory name input is finished and function returns.\n
:ALIASED-BY `mon-file-copy-multiple'\n
:SEE-ALSO `mon-copy-file-multiple', `mon-copy-files-in-sub-dirs',
`mon-get-relative-w-absolute' `mon-get-dir-name-absolute',
`mon-file-reduce-name', `mon-build-path', `mon-dir-common-paths',
`mon-file-dir-attributes->plist' `mon-get-buffer-parent-dir',
`mon-string-split-buffer-name', `mon-get-buffer-parent-dir',
`mon-get-proc-buffers-directories', `mon-get-buffers-directories',
`mon-string-split-dir-recurse', `mon-string-split-buffer-parent-dir-quick',
`mon-string-split-buffer-parent-dir', `mon-file-ensure-extension-is-el',
`file-relative-name'.\n►►►"
  (interactive "f:FUNCTION `mon-copy-file-multiple' -- file: ")
  (let* ((dest-list nil)
         (final-list
          (if list-of-dir
              list-of-dir
              (mon-read-multiple 'read-directory-name))))
    (loop for i 
          in final-list
	  do (copy-file file i t))))

;;; ==============================
;;; :PREFIX "mmrn-"
;;; :COURTESY Thierry Volpiatto :HIS tv-utils.el :WAS `multi-read-name'
;;; :MODIFICATIONS <Timestamp: #{2010-03-30T14:02:31-04:00Z}#{10132} - by MON KEY>
(defun* mon-read-multiple (&optional (multi-fun 'read-string))
  "Prompt indefinely while a is `+' suffixed to read value.\n
Return a list of all inputs in local var `mmrn-var`.\n
MULTI-FUN is an alternate input function to use when reading. 
Default is `read-string'.\n
:EXAMPLE\n\n(mon-read-multiple)\n
:SEE-ALSO `read-string', `read-directory-name'.\n►►►"
  (let ((mmrn-var (make-symbol "mmrn-var")))
    (labels ((multiread ()
               (let ((mmrn-str 
                      (funcall multi-fun 
                               (cond ((eq multi-fun 'read-string)
                                      (concat ":FUNCTION `mon-read-multiple' "
                                              "-- string (add + to repeat): "))
                                     ((eq multi-fun 'read-directory-name)
                                      (concat ":FUNCTION `mon-read-multiple' "
                                              "-- directory (add + to repeat): "))
                                     (t (concat ":FUNCTION `mon-read-multiple' "
                                                "-- file (add + to repeat): ")))))
                     (mmrn-stock))
                 (push (replace-regexp-in-string "\+" "" mmrn-str) mmrn-stock)
                 (cond (;; use `string-match-p' instead?
                        (string-match "\+" mmrn-str)
                        (push (car mmrn-stock) mmrn-var) ;; var)
                        (multiread))
                       (t (push (car mmrn-stock) mmrn-var) ;var)
                          (nreverse mmrn-var)))))) ;; var))))))
      (let (mmrn-var)
        (multiread)))))

(declare-function edmacro-subseq "edmacro")
;;; ==============================
;;; :PREFIX "mfrn-"
;;; :COURTESY Thierry Volpiatto :HIS tv-utils.el :WAS `tv-reduce-file-name'
;;; :MODIFICATIONS <Timestamp: #{2009-09-01T20:39:35-04:00Z}#{09363} - by MON>
;;; :CHANGESET 1889 <Timestamp: #{2010-06-17T12:00:24-04:00Z}#{10244} - by MON KEY>
(defun* mon-file-reduce-name (fname level &key unix-close expand)
  "Reduce file-name by LEVEL (an integer) depending on LEVEL's value.\n
If LEVEL is positive reduce by end else by beginning.
UNIX-CLOSE \(a boolean\) non-nil close filename with '/'.
EXPAND \(a boolean\) when non-nil `expand-file-name' of FNAME.\n
:EXAMPLE\n\(mon-file-reduce-name data-directory 3\)\n
:SEE-ALSO `mon-copy-file-multiple', `mon-copy-files-in-sub-dirs',
`mon-get-relative-w-absolute' `mon-get-dir-name-absolute',
`mon-file-reduce-name', `mon-build-path', `mon-file-dir-attributes->plist',
`mon-get-buffer-parent-dir', `mon-string-split-dir-recurse',
`mon-dir-common-paths', `mon-get-buffer-parent-dir',
`mon-get-proc-buffers-directories', `mon-get-buffers-directories',
`mon-string-split-buffer-name', `mon-string-split-buffer-parent-dir-quick',
`mon-string-split-buffer-parent-dir', `mon-file-ensure-extension-is-el',
`file-relative-name', `expand-file-name', `file-expand-wildcards',
`wildcard-to-regexp'.\n►►►"
  (let* ((mfrn-exp-f-nm (expand-file-name fname))
         (mfrn-f-nm-lst ;; :WAS (split-string (if expand mfrn-exp-f-nm fname) "/" t))
          (save-match-data (split-string (if expand mfrn-exp-f-nm fname) "/" t)))
         (mfrn-len (length mfrn-f-nm-lst))
         (mfrn-pop-lst (if (< level 0)
                       (edmacro-subseq mfrn-f-nm-lst (* level -1))
                     (edmacro-subseq mfrn-f-nm-lst 0 (- mfrn-len level))))
         (mfrn-rslt (mapconcat #'(lambda (mfrn-L-1) mfrn-L-1) mfrn-pop-lst "/")))
    (if unix-close
        (if expand
            (if (< level 0)
                (concat "../" mfrn-rslt "/")
                (concat "/" mfrn-rslt "/"))
            (if (string-match-p "~/" mfrn-rslt)
                (concat mfrn-rslt "/")
                (if (< level 0)
                    (concat "../" mfrn-rslt "/")
                    (concat "/" mfrn-rslt "/"))))
        (if expand
            (if (< level 0)
                (concat "../" mfrn-rslt "/")
                (concat "/" mfrn-rslt "/"))
            (if (string-match-p "~/" mfrn-rslt)
                (concat mfrn-rslt "/")
                (if (< level 0)
                    (concat "../" mfrn-rslt "/")
                    (concat "/" mfrn-rslt "/")))))))
;;
;;; :TEST-ME (mon-file-reduce-name data-directory 3)

;;; ==============================
;;; :CREATED <Timestamp: Saturday May 30, 2009 @ 02:42.31 PM - by MON>
(defun mon-build-path (expand-path suffix-path &rest more-paths)
  "Return a path with EXPAND-PATH concatenated to SUFFIX-PATH.\n
When MORE-PATHS is non-nil each additional string is appended to path.
Signal an error if any of the args aren't in the path.\n
:EXAMPLE
\(mon-build-path *mon-artist-naf-path* \"C-Artists names\" \"Cappiello \(Leonetto\)\"\)
\(mon-build-path *mon-artist-naf-path* \"C-Artists names\"\)\n
:EXAMPLE {:CALLED-PROGRAMATICALLY}
\(apply 'mon-build-path *mon-artist-naf-path* \"C-Artists names\"
\(split-string \"Cappiello \(Leonetto\)/Aux Trois Quartier/mmm\" \"/\" t\)\)\n
:SEE-ALSO `mon-get-relative-w-absolute' `mon-get-dir-name-absolute',
`mon-file-reduce-name', `mon-build-path', `mon-file-dir-attributes->plist',
`mon-get-buffer-parent-dir', `mon-string-split-dir-recurse',
`mon-dir-common-paths', `mon-get-buffer-parent-dir',
`mon-get-proc-buffers-directories', `mon-get-buffers-directories',
`mon-string-split-buffer-name', `mon-string-split-buffer-parent-dir-quick',
`mon-string-split-buffer-parent-dir', `mon-file-ensure-extension-is-el',
`file-relative-name', `expand-file-name', `file-expand-wildcards',
`wildcard-to-regexp'.\n►►►"
  (let (tst-pth stack-pth f-pth)
    (setq tst-pth '(lambda (tst &optional in-sub) 
                     (if (and (not (file-symlink-p tst))
                              (or (file-exists-p tst) (file-directory-p tst)))
                         t
                       (cond (in-sub 
                              (error 
                               (concat ":FUNCTION `mon-build-path' "
                                       "--  dir/file named `%s'\nIsn't in the path `%s/'" )
                               (file-name-nondirectory tst) in-sub))
                             ((not in-sub)
                              (error 
                               (concat ":FUNCTION `mon-build-path' "
                                       "-- dir/file named `%s' isn't in the path" )
                               tst in-sub))))))
    (setq stack-pth  '(lambda (in-pth sub-pth)
                       (let* ((t-sub sub-pth)
                              (t-whole (concat (directory-file-name in-pth) "/" sub-pth)))
                         (when (funcall tst-pth t-whole in-pth)
                           (setq f-pth t-whole)))))
    (funcall stack-pth (directory-file-name expand-path) nil)
    (funcall stack-pth f-pth suffix-path)
    (when more-paths
      (let ((mr-pth more-paths))
	(while mr-pth
	  (let* ((nxt-sub (car mr-pth))
		 (parent f-pth)
		 (wlk-rst (funcall stack-pth parent nxt-sub)))
            (if wlk-rst
                (progn
                  (setq f-pth wlk-rst)
                  (setq mr-pth (cdr mr-pth)))
                (setq mr-pth nil))))))
    f-pth))
;;
;;; :TEST-ME (mon-build-path *mon-artist-naf-path* "C-Artists names" "Cappiello (Leonetto)")
;;; :TEST-ME (mon-build-path *mon-artist-naf-path* "C-Artists names")
;;; :TEST-ME (apply 'mon-build-path *mon-artist-naf-path* "C-Artists names"
;;;          (split-string "Cappiello (Leonetto)/Aux Trois Quartier/mmm" "/" t))

;;; ==============================
;;; :PREFIX "mbwp-"
;;; :MODIFICATIONS <Timestamp: 2009-08-01-W31-6T11:38:50-0400Z - by MON>
;;; :REMOVED Best I can see the (and * t) is totally pointless; removed it.
;;; :CREATED <Timestamp: Saturday May 23, 2009 @ 11:38.18 AM - by MON>
(defun mon-buffer-written-p (&optional insrtp intrp)
  "Non-nil current buffer has been written to a file or created with `find-file'
 and _can_ be written in current directory - whether it has been or not).\n
:SEE-ALSO `mon-buffer-exists-p', `mon-buffer-written-p', `mon-with-file-buffer',
`mon-buffer-name->kill-ring', `mon-get-buffer-parent-dir',
`mon-get-proc-buffers-directories', `mon-get-buffers-directories',
`mon-string-split-buffer-name', `mon-string-split-buffer-parent-dir',
`mon-file-dir-attributes->plist', `mon-file-ensure-extension-is-el',
`with-current-buffer', `with-temp-file', `with-temp-buffer'.\n►►►"
  (interactive "P\np")
  (let* (;; and w/ t is not needed! Why was this here?         
         ;; :WAS (and (buffer-file-name) t)) 
         (mbwp-written-p (buffer-file-name))
	 (mbwp-has-or-not (if mbwp-written-p "has or can be"  "_hasn't or can't_ be")))
    (when intrp
      (message "buffer `%s' %s written to file" (buffer-name) mbwp-has-or-not))
    (when insrtp 
      (insert (format "#Buffer `%s' %s written to file" (buffer-name) mbwp-has-or-not)))
    mbwp-written-p))
;;
;;; :TEST-ME (mon-buffer-written-p)
;;; :TEST-ME (mon-buffer-written-p)
;;; :TEST-ME (mon-buffer-written-p t)
;;; :TEST-ME (call-interactively 'mon-buffer-written-p) 

;;; ==============================
;;; :PREFIX "mssbn-"
;;; :RENAMED `mon-split-string-buffer-name' -> `mon-string-split-buffer-name'
;;; :MODIFICATIONS <Timestamp: 2009-08-01-W31-6T11:48:58-0400Z - by MON>
;;; :ADDED optional args insrtp intrp
;;; :CREATED <Timestamp: Saturday May 23, 2009 @ 11:50.56 AM - by MON>
(defun mon-string-split-buffer-name (&optional insrtp intrp)
  "Return current `buffer-name' as a list with split-string.\n
When INSRTP is non-nil or called-interactively with prefix arg insert the list
of split strings at point.\n
:ALIASED-BY `mon-buffer-string-split-name'\n
:SEE-ALSO `mon-buffer-exists-p', `mon-buffer-written-p', `mon-with-file-buffer',
`mon-buffer-name->kill-ring', `mon-get-buffer-parent-dir',
`mon-get-proc-buffers-directories', `mon-get-buffers-directories',
`mon-string-split-buffer-parent-dir',
`mon-string-split-buffer-parent-dir-quick', `with-current-buffer',
`with-temp-file', `with-temp-buffer'.\n►►►"
  (interactive "P\np")
  (let ((mssbn-bfr-nm
         (save-match-data
           (if (mon-buffer-written-p)
               (split-string (buffer-file-name) "/" t)
             (split-string default-directory "/" t)))))
    (when intrp (message "%S" mssbn-bfr-nm))
    (when insrtp (insert (format "%S" mssbn-bfr-nm)))
    mssbn-bfr-nm))
;;
;;; :TEST-ME (mon-string-split-buffer-name)
;;; :TEST-ME (mon-string-split-buffer-name t)
;;; :TEST-ME (call-interactively 'mon-string-split-buffer-name)

;;; ==============================
;;; :PREFIX "mss-"
;;; :RENAMED `mon-split-string-buffer-parent-dir-quick' -> `mon-string-split-buffer-parent-dir-quick'
;;; :MODIFICATIONS <Timestamp: 2009-08-01-W31-6T11:48:58-0400Z - by MON>
;;; :ADDED optional args insrtp
;;; :CREATED <Timestamp: Saturday May 23, 2009 @ 08:17.10 PM - by MON>
(defun mon-string-split-buffer-parent-dir-quick (&optional insrtp)
  "Like `mon-string-split-buffer-parent-dir' but with less checks.\n
When INSRTP is non nil or called-interactively with prefix arg
insert the split in buffer. Moves point.\n
:ALIASED-BY `mon-buffer-string-split-parent-dir'\n
:SEE-ALSO `mon-buffer-exists-p', `mon-buffer-written-p', `mon-with-file-buffer',
`mon-buffer-name->kill-ring', `mon-get-buffer-parent-dir',
`mon-file-dir-attributes->plist' `mon-get-proc-buffers-directories',
`mon-get-buffers-directories', `mon-string-split-buffer-name',
`mon-string-split-buffer-parent-dir', `with-current-buffer', `with-temp-file',
`with-temp-buffer'.\n►►►"
  (interactive "P") 
  (let ((mss-bpdq (save-match-data
                    (split-string (directory-file-name (expand-file-name "./"))"/" t ))))
    (when insrtp (insert (format "%S" mss-bpdq)))
    mss-bpdq))
;;
;;; :TEST-ME (mon-string-split-buffer-parent-dir-quick)
;;; :TEST-ME (mon-string-split-buffer-parent-dir-quick t)
;;; :TEST-ME (call-interactively 'mon-string-split-buffer-parent-dir-quick)

;;; ==============================
;;; :PREFIX "mssbpd-"
;;; :MODIFICATIONS <Timestamp: 2009-08-01-W31-6T11:48:58-0400Z - by MON>
;;; :CREATED <Timestamp: Saturday May 23, 2009 @ 12:31.43 PM - by MON>
(defun mon-string-split-buffer-parent-dir (&optional insrtp intrp)
  "Return buffers parent sans buffer's file name as a split-string list.\n
When `buffer-file-name' is nil return parents of buffers `default-directory'
\(inclusive=) as list of strings.\n
Like `mon-string-split-buffer-name' but does not strip tail of buffer's 
`default-directory' when `mon-buffer-written-p' is nil.
Unlike =(file-name-nondirectory buffer-file-name\) which does not check if buffer 
has a file name - throws an error instead.\n
:EXAMPLE\n\n(mon-string-split-buffer-parent-dir\)\n
:NOTE Could also accomplish with `mon-string-split-buffer-parent-dir-quick'.\n
e.g. \n\(split-string \(directory-file-name \(expand-file-name \"./\"\)\)\"/\" t \)\n
:ALIASED-BY `mon-buffer-string-split-parent-dir'\n
:SEE-ALSO `mon-buffer-exists-p', `mon-buffer-written-p', `mon-with-file-buffer',
`mon-buffer-name->kill-ring', `mon-get-buffer-parent-dir',
`mon-file-dir-attributes->plist', `mon-get-proc-buffers-directories',
`mon-get-buffers-directories', `with-current-buffer', `with-temp-file',
`with-temp-buffer'.\n►►►"
  (interactive "P\np")
  (let* ((mssbpd-wrtn-p (mon-buffer-written-p))
	 (mssbpd-mod (save-match-data 
                       (if mssbpd-wrtn-p
                           (split-string (buffer-file-name) "/" t)
                         (split-string default-directory "/" t))))
	 (mssbpd-last  (if (and 
		       ;; File exists in dir.
		       mssbpd-wrtn-p  
		       ;; Don't delete top level dir.
		       (not (< (length mssbpd-mod) 1)))
		      ;; Get count for deleting last string in path.
		      (nth (- `,(length mssbpd-mod) 1) mssbpd-mod)
		    ;; Don't remove objects eq nil below.
		    '()))		
	 mssbpd-rmvd)
    (setq mssbpd-rmvd (remq mssbpd-last mssbpd-mod))
    (when insrtp (insert (format "%S" mssbpd-rmvd)))
    ;; (message "%S" mssbpd-rmvd) ;;message call is redundant
    mssbpd-rmvd))
;;
;;; :TEST-ME (mon-string-split-buffer-parent-dir)
;;; :TEST-ME (mon-string-split-buffer-parent-dir t)
;;; :TEST-ME (call-interactively 'mon-string-split-buffer-parent-dir)

;;; ==============================
;;; :PREFIX "mgbpd-"
;;; :MODIFICATIONS <Timestamp: #{2009-10-27T16:24:19-04:00Z}#{09442} - by MON KEY>
;;; :MODIFICATIONS <Timestamp: 2009-08-01-W31-6T11:26:49-0400Z - by MON>
;;; :ADDED optional insrtp, intrp args 
;;; :CREATED <Timestamp: Saturday May 23, 2009 @ 11:28.41 AM - by MON>
(defun mon-get-buffer-parent-dir (&optional w-full-path insrtp intrp)
  "Return buffers' parent directory as a string.\n
By default returns buffer's parent directory _only_.\n
When W-FULL-PATH is non-nil return W-FULL-PATH path of buffers parent directory as string.
If we are in a buffer which has been written to a file or _can be_ return files
parent, else return parent of buffers `default-directory'.\n
When called-intereactively or INSRTP is non-nil insert buffers parent directory.\n
:EXAMPLE\n\n\(mon-get-buffer-parent-dir\)\n
\(mon-get-buffer-parent-dir t\)\n
:NOTE Could also accomplish with:\n
 \(car \(last \(split-string 
               \(directory-file-name (expand-file-name \"./\"\)\) \"/\" t\)\)\)\n
But, not without the checks or a facility for sanity checks in programmatic
situations where `default-directory' of a non-written buffer may not evaluate to
what is expected. This is esp. the case where a calling function(s) has or might
`cd' to some alien path to do some stuff. We don't neccesarily want to blindly
write a buffer assuming that it will wind up in 'the' current directory.
It might not.\n
:ALIASED-BY `mon-buffer-get-parent-dir'\n
:SEE-ALSO `mon-buffer-exists-p', `mon-buffer-written-p', `mon-with-file-buffer',
`mon-buffer-name->kill-ring', `mon-get-proc-buffers-directories',
`mon-get-buffers-directories', `mon-string-split-buffer-name',
`mon-file-dir-attributes->plist' `mon-string-split-buffer-parent-dir'
`mon-string-split-buffer-parent-dir-quick', `with-current-buffer',
`with-temp-file', `with-temp-buffer'.\n►►►"
  (interactive "i\ni\np")
  (let* ((mgbpd-is-written (mon-buffer-written-p))
	 (mgbpd-rtn-bfr-dir 
          (if mgbpd-is-written
              (if w-full-path
                  (directory-file-name 
                   (file-name-directory (buffer-file-name)))
                (file-name-nondirectory 
                 (directory-file-name 
                  (file-name-directory (buffer-file-name)))))
            (if w-full-path
                (directory-file-name default-directory)
              (file-name-nondirectory 
               (directory-file-name default-directory))))))
    (if mgbpd-is-written
	;; (progn 
        ;;   (when (or insrtp intrp)
        ;;     (message "buffer: `%s' parent dir is `%s'."  (buffer-name) mgbpd-rtn-bfr-dir))
          (if (or insrtp intrp)
              (prog1 
                  (when (or insrtp intrp)
                    (message (concat ":FUNCTION `mon-get-buffer-parent-dir' "
                                    "-- :BUFFER `%s' parent dir is `%s'.")
                            (buffer-name) mgbpd-rtn-bfr-dir))
                (insert mgbpd-rtn-bfr-dir))
              mgbpd-rtn-bfr-dir)
        ;; (prog1
        ;;   (when (or insrtp intrp)
        ;;     (message "Buffer: `%s' not written yet, parent of buffer's default-directory is `%s'." 
        ;;              (buffer-name) mgbpd-rtn-bfr-dir)
          (if (or insrtp intrp)
              (prog1
                  (message  
                   (concat ":FUNCTION `mon-get-buffer-parent-dir' "
                           "-- :BUFFER `%s' not written yet, parent of buffer's default-directory is `%s'." )
                   (buffer-name) mgbpd-rtn-bfr-dir)
                (insert mgbpd-rtn-bfr-dir))
              mgbpd-rtn-bfr-dir))))
;;
;;; :TEST-ME (mon-get-buffer-parent-dir)
;;; :TEST-ME (mon-get-buffer-parent-dir t)
;;; :TEST-ME (mon-get-buffer-parent-dir t t)
;;; :TEST-ME (mon-get-buffer-parent-dir nil t)
;;; :TEST-ME (call-interactively 'mon-get-buffer-parent-dir)

;;; ==============================
;;; :PREFIX "mtpfp-"
;;; :CREATED <Timestamp: Friday May 29, 2009 @ 07:26.02 PM - by MON>
(defun mon-truncate-path-for-prompt (&optional intrp)
  "Return a truncated path string of current buffers path.\n
Useful for passing around to helper functions that prompt.\n
:EXAMPLE\n\n(mon-truncate-path-for-prompt)\n
:ALIASED-BY `mon-dir-name-truncate-for-prompt'\n
:SEE-ALSO `mon-file-reduce-name'.\n►►►"
  (interactive "p")
  (let* ((mtpfp-pth (directory-file-name (expand-file-name "./")))
	 (mtpfp-splt (save-match-data (split-string mtpfp-pth "/")))
	 (mtpfp-len (length mtpfp-splt))
	 mtpfp-gthr)
    (setq mtpfp-gthr (cond ((>= mtpfp-len 3)(last mtpfp-splt 3))
                        ((>= mtpfp-len 2)(last mtpfp-splt 2))
                        ((>= mtpfp-len 1)(last mtpfp-splt))))
    (setq mtpfp-gthr (mapconcat #'identity mtpfp-gthr "/"))
    (if intrp (message (concat ":FUNCTION `mon-truncate-path-for-prompt' "
                               "-- truncated path: %s")
                       mtpfp-gthr) mtpfp-gthr)))
;;
;;; :TEST-ME (mon-truncate-path-for-prompt)

;;; ==============================
;;; :PREFIX "mssdr-"
;;; :CREATED <Timestamp: Saturday May 23, 2009 @ 08:37.50 PM - by MON>
(defun mon-string-split-dir-recurse (&optional alt-path reverse-path)
  "Return default-directory as list of recursively split strings.\n
When ALT-PATH (a directory name string) is non-nil use it instead.
Signal an error if ALT-PATH doesn't exist.\n
When REVERSE-PATH is non-nil return the result in reversed format.
The REVERSE-PATH arg is useful for faster comparison of two trees.
Default is to split buffer's current directory.\n
Use to walk up the directory or buffers path.
:EXAMPLE\n\n\(mon-string-split-dir-recurse\)\n
\(mon-string-split-dir-recurse nil t\)
\(mon-string-split-dir-recurse user-emacs-directory\)\n
\(mon-string-split-dir-recurse user-emacs-directory t\)\n
\(mon-string-split-dir-recurse \(expand-file-name \"~/\"\)\)\n
\(mon-string-split-dir-recurse \(expand-file-name \"~/\"\) t\)\n
:ALIASED-BY `mon-dir-recurse-string-split'
:ALIASED-BY `mon-buffer-string-split-dir-recurse'\n
:SEE-ALSO `mon-dir-common-paths', `expand-file-name', `file-expand-wildcards'.\n►►►"
  (interactive)
  (let* ((mssdr-alt-pth (when alt-path
                          (if (file-exists-p (directory-file-name alt-path))
                              (directory-file-name alt-path)
                            (error (concat ":FUNCTION `mon-string-split-dir-recurse' "
                                           "-- path name provided non-existent")))))
	 (mssdr-wlk-bfr-dirs (if mssdr-alt-pth 
                                 (save-match-data (split-string mssdr-alt-pth "/" t))
                               (mon-string-split-buffer-parent-dir)))
	 mssdr-gthr)
    (while mssdr-wlk-bfr-dirs
      (push (mapconcat #'identity mssdr-wlk-bfr-dirs "/") mssdr-gthr)
      (setq mssdr-wlk-bfr-dirs (nreverse mssdr-wlk-bfr-dirs))
      (pop mssdr-wlk-bfr-dirs)
      (setq mssdr-wlk-bfr-dirs (nreverse mssdr-wlk-bfr-dirs)))
    (if reverse-path (nreverse mssdr-gthr) mssdr-gthr)))
;;
;;; :TEST-ME (mon-string-split-dir-recurse) 
;;; :TEST-ME (mon-string-split-dir-recurse nil t)
;;; :TEST-ME (mon-string-split-dir-recurse (expand-file-name "~/"))
;;; :TEST-ME (mon-string-split-dir-recurse (expand-file-name "~/") t)

;;; ==============================
;;; :PREFIX "mdcp-"
;;; :CREATED <Timestamp: Wednesday May 27, 2009 @ 04:28.57 PM - by MON>
(defun mon-dir-common-paths (path-is path-in)
  "Given two paths return an ascending list of the most common parents of two.\n
Comparison made as a test if PATH-IS (the unknown path) has a common parent
directory in PATH-IN (the target path).\n
The car retrun value is the the first deepest directory.\n
The last elt or return value is least deepest directory.\n
:EXAMPLE\n\n\(mon-dir-common-paths *mon-ebay-images-bmp-path* *mon-ebay-images-path*\)\n
\(mon-dir-common-paths user-emacs-directory \(getenv \"HOME\"\)\)\n
:ALIASED-BY `mon-get-dir-common-path'\n
:SEE-ALSO `mon-string-split-dir-recurse', `mon-get-buffer-parent-dir',
`mon-add-subdirs-to-list', `mon-insert-subdirs-in-buffer',
`mon-get-dir-subdir-default'.\n►►►"
  (let ((mdcp-pth-A (mon-string-split-dir-recurse path-is t)) ;; Longer - reversed.
	(mdcp-pth-B (mon-string-split-dir-recurse path-in t)) ;; Check if is-in - reversed.
	mdcp-got)
    (while (and mdcp-pth-A (not mdcp-got))
      (let* ((mdcp-pth-A-hd (car mdcp-pth-A))
	     (mdcp-look (member mdcp-pth-A-hd mdcp-pth-B)))
	(when mdcp-look (setq mdcp-got mdcp-look)))
      (setq mdcp-pth-A (cdr mdcp-pth-A)))
    ;; Why are we messaging here?
    (when (not mdcp-got)
      (message (concat ":FUNCTION `mon-dir-common-paths' "
                       "-- no common paths for:\n%s and\n %s") 
               path-is path-in))
    mdcp-got))
;;
;;; :TEST-ME (mon-dir-common-paths *mon-ebay-images-bmp-path* *mon-ebay-images-path*)

;;; ==============================
;;; :CHANGESET 2261
;;; :CREATED <Timestamp: #{2010-11-04T16:06:28-04:00Z}#{10444} - by MON KEY>
(defun mon-dir-get-subdirs (w-dir &optional no-full no-check)
  "Return W-DIR and its immedate sub-directories as a list of directory names.\n
When NO-FULL is non-nil only W-DIR directory name is a full pathname
subdirs beneath W-DIR are unqualified path namestrings.\n
W-DIR must satisfy `mon-file-truename-p', signal an error if not.\n
Unlike `mon-add-subdirs-to-list' does not descend below current directory.\n
:EXAMPLE\n\n\(mon-dir-get-subdirs default-directory\)\n
:NOTE When optional arg NO-CHECK is non-nil, do not check that W-DIR exists and
do not return W-DIR as an element of list -- this option is _only_ provided for
use with `mon-add-subdirs-to-list' which checks directory existence itself.
In most other situations this argument should normally be ommitted.\n
:SEE-ALSO `directory-files-and-attributes', `directory-files',
`%mon-dir-get-subdirs-filter-full', `%mon-dir-get-subdirs-filter-no-full',
`*regexp-add-subdirs-to-list-filter-ignorables*',
`*mon-add-subdirs-to-list-ignorables*'.\n►►►"
  (let ((mdgs-dirp (or (and no-check (list t w-dir))
                       (and (file-directory-p w-dir)
                            ;; Make sure we have a directory name ending in "/"
                            (or (= (aref w-dir (1- (length w-dir))) 47)
                                (setq w-dir (concat w-dir "/")))
                            (mon-file-truename-p w-dir t)))))
    (setq mdgs-dirp
          (nconc 
           (unless no-check
             (list (cadr mdgs-dirp)))
           (delq nil 
                 (mapcar (or (and no-full #'%mon-dir-get-subdirs-filter-no-full)
                             #'%mon-dir-get-subdirs-filter-full)
                         (directory-files-and-attributes 
                          (or (and (car mdgs-dirp) (cadr mdgs-dirp))
                              (error (concat ":FUNCTION `mon-dir-get-subdirs' "
                                             "-- arg W-DIR not `mon-file-truename-p', "
                                             "failed with: %S") (cadr mdgs-dirp)))
                          (not no-full))))))))

;;; ==============================
;;; :CHANGESET 2261
;;; :CREATED <Timestamp: #{2010-11-04T19:14:49-04:00Z}#{10444} - by MON KEY>
(defun %mon-dir-get-subdirs-filter-full (path-attribs)
  "Helper function for `mon-dir-get-subdirs' and `mon-add-subdirs-to-list'.\n
PATH-ATTRIBS is an elt of list returned by `directory-files-and-attributes'.\n
Return a fully qaulified directory namestring when either are true:\n
 - cadr of PATH-ATTRIBS is not `string-or-null-p'.\n
 - car of PATH-ATTRIBS matches `*regexp-add-subdirs-to-list-filter-ignorables*'\n
:SEE-ALSO `%mon-dir-get-subdirs-filter-no-full',
`*mon-add-subdirs-to-list-ignorables*'.\n►►►"
  (and (not (or (string-or-null-p (cadr path-attribs))
                (string-match-p 
                 *regexp-add-subdirs-to-list-filter-ignorables*
                 (car path-attribs))))
       (file-name-as-directory (car path-attribs))))

;;; ==============================
;;; :CHANGESET 2261
;;; :CREATED <Timestamp: #{2010-11-04T19:11:18-04:00Z}#{10444} - by MON KEY>
(defun %mon-dir-get-subdirs-filter-no-full (path-attribs)
  "Helper function for `mon-dir-get-subdirs' and `mon-add-subdirs-to-list'.\n
PATH-ATTRIBS is an elt of list returned by `directory-files-and-attributes'.\n
Return an unqaulified directory namestring component when following are true:\n
 - car of PATH-ATTRIBS does not match `*mon-add-subdirs-to-list-ignorables*'\n
 - cadr of PATH-ATTRIBS is not `string-or-null-p'.\n
:SEE-ALSO `%mon-dir-get-subdirs-filter-full',
`*regexp-add-subdirs-to-list-filter-ignorables*'.\n►►►"
  (and (not (or ;; 2nd elt of attributes list `d-f-a-a'
             (string-or-null-p (cadr path-attribs))
             (member (car path-attribs) 
                     *mon-add-subdirs-to-list-ignorables*)))
       (car path-attribs)))

(declare-function untranslated-canonical-name "dos-w32" t t)
;; Make sure `untranslated-canonical-name' is available for w32.
(eval-when (compile load) 
  (when (memq system-type '(ms-dos windows-nt cygwin)) 
    (require 'dos-w32 nil t)))

;;; =======================
;;; :TODO Consider adding optional arg W-FULL which would toggle the optional
;;; FULL arg to `directory-files-and-attributes'. See comments below.
;;; :COURTESY :FILE lisp/startup.el :WAS `normal-top-level-add-subdirs-to-load-path'
;;; :NOTE Significantly modified.
;;; :CHANGESET 2261 <Timestamp: #{2010-11-03T21:01:23-04:00Z}#{10443} - by MON KEY>
;;; :CHANGESET 1779 <Timestamp: #{2010-05-27T15:32:43-04:00Z}#{10214} - by MON KEY>
(defun mon-add-subdirs-to-list (w-directory &optional w-add-to-list w-no-symlinks)
  "Add all immediate subdirectories of `w-directory' to `w-add-to-list'.\n
Does a breadth-first tree walk on DIR's subtree, gathering the subdirectories
whose names start with letters or digits; it excludes any subdirectory named in
the variable `*mon-add-subdirs-to-list-ignorables*', and any sub-directory which
contains a file named `.nosearch'.\n
When optional arg W-ADD-TO-LIST is non-nil destrucitively add elts of return
value to W-ADD-TO-LIST as if by `add-to-list'.\n
When optional arg W-NO-SYMLINKS is non-nil and W-DIRECTORY is `file-symlink-p'
signal an error.\n
:EXAMPLE\n\n(mon-add-subdirs-to-list default-directory\)\n
\(let \(gets-set\)
  \(mon-add-subdirs-to-list default-directory 'gets-set\)
  gets-set\)\n
:SEE-ALSO `mon-copy-files-in-sub-dirs', `mon-insert-subdirs-in-buffer',
`mon-path', `mon-copy-file-path', `mon-insert-path',
`mon-file-dir-attributes->plist', `mon-get-buffers-directories',
`mon-proc-buffers-directories', `mon-get-proc-buffers-directories'.\n►►►" 
  (let (;; :WAS (mastl-pending (list (file-name-directory w-directory)))
        (mastl-pending 
         (list (or (and (file-directory-p w-directory)
                        (or (and w-no-symlinks 
                                 (file-symlink-p w-directory)
                                 (error 
                                  (concat ":FUNCTION `mon-add-subdirs-to-list' " 
                                          "-- arg W-DIRECTORY is `file-symlink-p', "
                                          " refusing to descend")))
                            ;; Make sure we have a directory name ending in "/"
                            (and (or (= (aref w-directory (1- (length w-directory))) 47)
                                     (setq w-directory (concat w-directory "/")))
                                 w-directory)))
                   (error 
                    (concat ":FUNCTION `mon-add-subdirs-to-list' " 
                            "-- arg W-DIRECTORY does not satisfy `file-directory-p'")))))
         mastl-ntlasil ;; `normal-top-level-add-subdirs-inode-list'
         mastl-dirs 
         mastl-attrs)
    ;; This loop does a breadth-first tree walk on DIR's subtree,
    ;; putting each subdir into MASTL-DIRS as its contents are examined.
    (while mastl-pending
      (push (pop mastl-pending) mastl-dirs)
      (let* ((mastl-this-dir (car mastl-dirs))
             (mastl-contents (mon-dir-get-subdirs mastl-this-dir nil t))
	     (default-directory mastl-this-dir)
	     (mastl-canonicalized (if (fboundp 'untranslated-canonical-name)
                                      (untranslated-canonical-name mastl-this-dir))))
	;; The Windows version doesn't report meaningful inode numbers,
	;; so use the canonicalized absolute file name of the
	;; directory instead. -- (or so says :FILE startup.el) -MON
        ;; :NOTE nth 10 is the inode number
        (setq mastl-attrs (or mastl-canonicalized 
                              (nthcdr 10 (file-attributes mastl-this-dir))))
	(unless (member mastl-attrs mastl-ntlasil)
	  (push mastl-attrs mastl-ntlasil)
	  (dolist (mastl-D-1-file mastl-contents)
            ;; ----
            ;; :WAS (when (and (string-match-p "\\`[[:alnum:]]" mastl-D-1-file))
            ;;   (let ((mastl-expanded 
            ;;          (file-name-as-directory 
            ;;           (expand-file-name mastl-D-1-file mastl-this-dir))))
            ;;     (unless (file-exists-p (expand-file-name ".nosearch" mastl-expanded))
            ;;       (setq mastl-pending (nconc mastl-pending (list mastl-expanded)))))) ))))
            ;;----
            ;; The original only included files matching regexp: "\\`[[:alnum:]]"
            ;; It isn't clear if this is what is wanted for a more generalized version.
            ;; Bring it back if that is what is wanted....
            ;; (when (and (string-match-p "\\`[[:alnum:]]" mastl-D-1-file))
            (unless (file-exists-p (expand-file-name ".nosearch" mastl-D-1-file))
              (setq mastl-pending (nconc mastl-pending (list mastl-D-1-file)))) )))) ; )
    (or (and w-add-to-list
             (dolist (mastl-D-2 mastl-dirs w-add-to-list)
               (add-to-list w-add-to-list mastl-D-2)))
        (nreverse mastl-dirs))))
;;
;;; :TEST-ME (mon-add-subdirs-to-list default-directory)
;;; :TEST-ME (let (gets-set) (mon-add-subdirs-to-list default-directory 'gets-set) gets-set)

;;; ================================================================
;;; :PREFIX "misib-"
;;; :SEE (ULR `http://www.emacswiki.org/emacs/SubdirsToList') - no-author.
;;; :MODIFICATIONS <Timestamp: Tuesday February 17, 2009 @ 05:50.26 PM - by MON>
(defun mon-insert-subdirs-in-buffer (&optional pth-to-l)
  "Insert at point the top-level subdirs found in PTH-TO-L.\n
PTH-TO-L is nil or called-interactively prompt for a path name.\n
Ignore directories with a \".nosearch\" namestring.\n
:CALLED-BY `mon-add-subdirs-to-list'\n
:ALIASED-BY `mon-buffer-subdirs-insert'\n
:SEE-ALSO `mon-get-dir-subdir-default', `mon-copy-files-in-sub-dirs',
`mon-dir-common-paths', `mon-get-buffers-directories',
`mon-file-dir-attributes->plist', `mon-get-proc-buffers-directories',
`mon-proc-buffers-directories', `insert-directory'.\n►►►"
  (interactive "D:FUNCTION `mon-insert-subdirs-in-buffer' -- give a directory path: ")
  (save-excursion
    (let* ((misib-pth-lst (if pth-to-l ;;(and file-directory-p pth-to-l)
                              pth-to-l
                            (read-directory-name 
                             (concat ":FUNCTION `mon-insert-subdirs-in-buffer' "
                                     "-- give a directory path: "))))           
	   ;; :WAS 
           ;; (misib-sub-dir-lst ())
           ;; (get-list (mon-add-subdirs-to-list misib-pth-lst 'misib-sub-dir-lst))
           ;; (misib-print-lst get-list))	   
           (misib-sub-dir-lst (mon-add-subdirs-to-list misib-pth-lst)))
      (while misib-sub-dir-lst ;; misib-print-lst
	(progn
	  (newline)
          (princ (car misib-sub-dir-lst) (current-buffer)))
	(setq misib-sub-dir-lst (cdr misib-sub-dir-lst))))))
;;
;; (unless (and (intern-soft "" obarray) (fboundp ')) (defalias ' 'mon-insert-subdirs-in-buffer))

;;; ==============================
;;; :PREFIX "mgdsd-"
;;; :CREATED <Timestamp: #{2010-04-05T16:11:37-04:00Z}#{10141} - by MON>
(defun mon-get-dir-subdir-default (&optional w-dir-to-list insrtp intrp)
  "Return the top-level subdirs of default-directory ommitting \"/.\" and \"/..\".\n
When W-DIR-TO-LIST is non-nil it is a directory name string.\n
When INSRTP is non-nil or called-interactively insert return value at point.
Does not move point.\n
:EXAMPLE\n\n\(mon-get-dir-subdir-default\)\n
\(mon-get-dir-subdir-default user-emacs-directory\)\n
\(with-current-buffer \(get-buffer-create \"*MON-GET-DIR-SUBDIR-DEFAULT-EXAMPLE*\"\)
     \(mon-get-dir-subdir-default user-emacs-directory t\)
     \(display-buffer \(current-buffer\)\) \(sit-for 2\) 
     \(when \(eq \(current-buffer\) \(get-buffer \"*MON-GET-DIR-SUBDIR-DEFAULT-EXAMPLE*\"\)\)
       \(kill-buffer \(current-buffer\)\)\)\)\n
:ALIASED-BY `mon-dir-get-subdir'\n
:SEE-ALSO `mon-copy-files-in-sub-dirs', `mon-insert-subdirs-in-buffer',
`mon-get-buffers-directories', `mon-get-proc-buffers-directories',
`mon-proc-buffers-directories', `directory-files', `expand-file-name',
`mon-file-dir-attributes->plist', `file-expand-wildcards'.\n►►►"
  (interactive "i\ni\np")
  (let ((mgdsd-pth-lst 
         (directory-files 
          (cond (intrp default-directory)
                ((and w-dir-to-list (file-directory-p w-dir-to-list)) w-dir-to-list)
                ;; A fall through case.
                ((or (not w-dir-to-list) (not (file-directory-p w-dir-to-list)))
                 ;; (error ":FUNCTION `mon-insert-subdirs-in-buffer' -- arg W-DIR-TO-LIST is non-existent"))
                 default-directory)) t))
        mgdsd-gthr)
    (dolist (mgdsd-D-1 mgdsd-pth-lst (setq mgdsd-gthr (nreverse mgdsd-gthr)))
      (unless (or (null (car (file-attributes mgdsd-D-1)))
                  (string-match-p "/\.\.?$" mgdsd-D-1))
        (push mgdsd-D-1 mgdsd-gthr)))
    (if (or insrtp intrp)
        (save-excursion
          (setq mgdsd-gthr (mapconcat #'identity mgdsd-gthr "\n"))
          (newline)
          (princ mgdsd-gthr (current-buffer)))
        mgdsd-gthr)))
;;
(unless (and (intern-soft "mon-dir-get-subdir" obarray) 
             (fboundp 'mon-dir-get-subdir))
(defalias 'mon-dir-get-subdir 'mon-get-dir-subdir-default))
;;
;;; :TEST-ME (mon-get-dir-subdir-default)
;;; :TEST-ME (mon-get-dir-subdir-default user-emacs-directory)
;;; :TEST-ME (mon-get-dir-subdir-default user-emacs-directory t)
;;; :TEST-ME (with-current-buffer (get-buffer-create "*MON-GET-DIR-SUBDIR-DEFAULT-EXAMPLE*")
;;;               (mon-get-dir-subdir-default user-emacs-directory t)
;;;               (display-buffer (current-buffer)) (sit-for 2) 
;;;               (when (eq (current-buffer) (get-buffer "*MON-GET-DIR-SUBDIR-DEFAULT-EXAMPLE*"))
;;;                 (kill-buffer (current-buffer))))

;; ==============================
;;; :PREFIX "mrfs-"
;;; :COURTESY Thierry Volpiatto :HIS thumb-page.el :WAS `tv-serial-rename'
;;; :SEE (URL `http://www.emacswiki.org/emacs/SerialRename')
;;; :CREATED <Timestamp: Sunday April 05, 2009 @ 01:14.27 PM - by MON>
(defun mon-rename-file-serial (w-dir w-ext w-name start-num)
  "Rename sequentially a set of file(s).\n
Rename W-NAME and W-EXT in W-DIR from START-NUM number.\n
:ALIASED-BY `mon-file-rename-serial'\n
:SEE-ALSO `mon-rename-file', `mon-rename-imgs-in-dir'.\n►►►"
  (interactive "f:FUNCTION `mon-rename-file-serial' -- W-Dir: \ns-- W-EXT (no dot): \ns-- W-NAME: \nn-- START-NUM: ")
  (find-file w-dir)
  (let (mrfs-ls-dir mrfs-new-dir  mrfs-iter mrfs-cnt)
    (setq mrfs-ls-dir (file-expand-wildcards (format "*.%s" w-ext) t))
    (setq mrfs-new-dir nil)
    (setq mrfs-iter 0)
    (while (< mrfs-iter (length mrfs-ls-dir))
      (if (< start-num 10)
	  (push (concat w-dir w-name (format "0%s" start-num) "." w-ext) mrfs-new-dir)
	(push (concat w-dir w-name (format "%s" start-num) "." w-ext) mrfs-new-dir))
      (setq start-num (+ start-num 1))
      (setq mrfs-iter (+ mrfs-iter 1)))
    (setq mrfs-ls-dir (reverse mrfs-ls-dir))
    (setq mrfs-cnt 0)
    (dolist (mrfs-D-1 mrfs-ls-dir)
      (rename-file mrfs-D-1 (nth mrfs-cnt mrfs-new-dir))
      (setq mrfs-cnt (+ mrfs-cnt 1)))))
;;
(unless (and (intern-soft "mon-file-rename-serial" obarray) 
             (fboundp 'mon-file-rename-serial))
(defalias 'mon-file-rename-serial  'mon-rename-file-serial))
;;
;;; :TEST-ME (mon-rename-file-serial)

;;; ==============================
;;; :DEPRECATED use `mon-copy-file-path'
(defun mon-path (&optional intrp)
  ":DEPRECATED use `mon-copy-file-path'.\n
Message user with the the current file's path.\n
Unlike `mon-copy-file-path' path doesn't copy to file's path kill ring.\n
:SEE-ALSO `mon-copy-file-path', `mon-insert-path', `mon-add-subdirs-to-list'.\n►►►" 
  (interactive "p")
  (if intrp
      (message "%s" buffer-file-name)
    (buffer-file-name)))
;;
(make-obsolete 'mon-path 'mon-copy-file-path "2010-03-30")
;;
;;; :TEST-ME (mon-path)
;;; :TEST-ME (mon-path t)
;;; :TEST-ME (call-interactively 'mon-path)

;;; ==============================
;;; :MODIFICATIONS <Timestamp: Tuesday July 21, 2009 @ 05:19.11 PM - by MON>
(defun mon-copy-file-path (&optional insrtp intrp)
  "Copy current buffer's file path to kill-ring. Return path value as message.\n
When INSRTP is non-nil or called with prefix arg insert path at point.\n
:ALIASED-BY `mon-file-copy-path'
:ALIASED-BY `mon-buffer-file-copy-path'\n
:SEE-ALSO `mon-insert-path', `mon-path', `mon-add-subdirs-to-list',
`mon-copy-files-in-sub-dirs'.\n►►►"
  (interactive "i\np") ;; "P\np")
  (let (scfp)
    (if (and (mon-buffer-written-p) (file-readable-p (buffer-file-name)))
        (progn
          (setq scfp (buffer-file-name))
          (kill-new (buffer-file-name))
          (when (or intrp (not insrtp))
            (progn
              (message (concat ":FUNCTION `mon-copy-file-path' " 
                               "-- the path is: %s") scfp)
              (sit-for 1))))
      (progn
        (setq scfp (buffer-name))
        (kill-new (format 
                   "#P/not-written/not-readable/not-exisistent/buffer-name-is/%s" 
                   (buffer-name)))
        (cond ((or intrp (not insrtp))
               (message (concat ":FUNCTION `mon-copy-file-path' "
                                "-- path non-existent buffer-name is: %s") (buffer-name))
               (sit-for 1))
              ((and insrtp (not intrp))
               (setq scfp 
                     (format "#P/not-written/not-readable/not-exisistent/buffer-name-is/%s"
                             (buffer-name)))))))
    (if insrtp (insert scfp))))
;;
(unless (and (intern-soft "mon-file-copy-path" obarray) 
             (fboundp 'mon-file-copy-path))
(defalias 'mon-file-copy-path        'mon-copy-file-path))
;;
(unless (and (intern-soft "mon-buffer-file-copy-path" obarray) 
             (fboundp 'mon-buffer-file-copy-path))
(defalias 'mon-buffer-file-copy-path 'mon-copy-file-path))
;;
;;; :TEST-ME (mon-copy-file-path)
;;; :TEST-ME (mon-copy-file-path t )
;;; :TEST-ME (mon-copy-file-path t t)
;;; :TEST-ME (call-interactively 'mon-copy-file-path)

;;; ==============================
;;; :MODIFICATIONS: <Timestamp: Tuesday July 21, 2009 @ 05:21.05 PM - by MON>
 (defun mon-insert-path ()
   ":DEPRECATED use `mon-copy-file-path'.\nInsert current file's path at point.\n
:SEE-ALSO `mon-copy-file-path', `mon-path', `mon-add-subdirs-to-list',
`mon-copy-files-in-sub-dirs'.\n►►►"
   (interactive)
   (mon-copy-file-path t))
;;
(make-obsolete 'mon-insert-path 'mon-copy-file-path "2010-03-30")
;;
;;; :TEST-ME (mon-insert-file-path)



;;; ==============================
;;; :PREFIX "mpbd-"
;;; :CREATED <Timestamp: Thursday May 07, 2009 @ 08:24.16 PM - by MON>
(defun mon-proc-buffers-directories (&optional opt-dir) 
  "Return directory list of buffers' directories sub-directories.\n
:CALLED-BY `mon-get-buffers-directories', `mon-get-proc-buffers-directories',
`mon-cln-blank-lines'.\n
:NOTE For alternative implementation of same:
:SEE `mon-insert-subdirs-in-buffer', `mon-add-subdirs-to-list'.\n
:SEE-ALSO `mon-copy-files-in-sub-dirs', `directory-files'.\n►►►"
  (let ((mpbd-to-proc (mon-get-buffers-directories opt-dir))
        mpbd-proc-rslt)
    ;; :WAS (setq mpbd-to-proc (mon-get-buffers-directories opt-dir)) 
    (setq mpbd-proc-rslt
	  (with-temp-buffer
	    (insert mpbd-to-proc)
	    ;; :WAS (let* ((mrkr-start) (mrkr-end)(mrkr-start (make-marker))(mrkr-end   (make-marker))
            (let ((mrkr-start (make-marker))
                  (mrkr-end   (make-marker)))
	      ;; Need to check here if there are actually any directories in this path.
	      ;; "^drwxrwxrwx  1 Everyone Everyone        0 05-15 16:14 "\\([^\.+]\\)
              (mon-g2be -1)
	      (set-marker mrkr-start (progn (search-forward "total used in directory") (point-at-bol)))
	      (set-marker mrkr-end  (mon-g2be 1 t))
	      (let* ((start-point (marker-position  mrkr-start)) ;(start-point (point))
		     (end-point (marker-position  mrkr-end))
		     (dirs '("^\\(-.*\\)$" "^\\(total .*\\)$" "^\\(d.*\\)$" ":$"))
		     (dir-swp))
		(while dirs
		  (goto-char start-point)
		  (setq dir-swp (car dirs))
		  (while (search-forward-regexp dir-swp nil t)
		    (replace-match ""))
		  (setq dirs (cdr dirs)))))
            (mon-g2be -1)
	    (while (search-forward-regexp "^$" nil t)
	      (replace-match "#"))
            (mon-g2be -1)
	    (while (search-forward-regexp "^#$" (mon-g2be 1 t) t)
	      (cond ((and (eolp) (not (bobp))
			  (not (not (char-before)))
			  (not (not (char-before (1- (point))))))
		     (if (and (= (char-before) 35)
			      (= (line-beginning-position) (1- (point)))
			      (= (char-before (1- (point))) 10)
			      (not (= (char-before (- (point) 2)) 10)))
			 (delete-char -2)))))
            (mon-g2be -1)
	    (progn
	      (search-forward-regexp "^#$" nil t)
	      (forward-char 1)
	      (delete-char -2))
	    (buffer-string)))
    mpbd-proc-rslt)) ;;(insert mpbd-proc-rslt)))
;;
;;;(progn (fmakunbound 'mon-proc-buffers-directories)
;;;       (unintern "mon-proc-buffers-directories" obarray) )

;;; ==============================
;;; :PREFIX "mgpbd-"
;;; :NOTE Can `subst-char-in-string' be used here instead?
;;;       Did this function get screwed up at :CHANGESET 533:5c6419be867a?
;;;       I think it is fixed, but...
;;; :CREATED <Timestamp: Thursday May 07, 2009 @ 09:40.22 PM - by MON>
(defun mon-get-proc-buffers-directories (&optional intrp opt-dir) 
  "Return dir list of buffer files directories.\n
Directory list acquired with `mon-get-buffers-directories' and
`mon-proc-buffers-directories' and cleaned with `mon-cln-blank-lines'.\n
:ALIASED-BY `mon-buffer-get-and-proc-dirs'\n
:NOTE For alternative implementation of same:
:SEE `mon-insert-subdirs-in-buffer', `mon-add-subdirs-to-list'.\n
:SEE-ALSO `mon-copy-files-in-sub-dirs', `mon-buffer-name->kill-ring',
`mon-buffer-exists-p', `mon-buffer-written-p', `mon-with-file-buffer'.\n►►►"
  (interactive "p")
  ;; mgpbd-
  (let ((mgpd-proc (mon-proc-buffers-directories opt-dir))
        mgpd-rslt)
    ;; :WAS (setq mgpd-proc (mon-proc-buffers-directories opt-dir))
    (setq mgpd-rslt (with-temp-buffer
		   (insert mgpd-proc)
                   (mon-cln-blank-lines (mon-g2be -1 t) (mon-g2be 1 t))
		   (let* ((file-n (bounds-of-thing-at-point 'filename))
			  (bound-s (car file-n))
			  (bound-e (cdr file-n))
			  (file-ns (buffer-substring bound-s bound-e))
			  (file-list) (f-name))
                     ;; (save-excursion
                     (mon-g2be 1)
		     (while (mon-spacep) 
		       (delete-char -1))
                     (mon-g2be -1)
		     (while (mon-spacep nil t)
		       (delete-char 1))
                     (mon-g2be -1)
		     (mon-cln-spc-tab-at-eol-in-region (mon-g2be -1 t) (mon-g2be 1 t))
                     (mon-g2be -1)
                     ;; :TODO Seperate to dedicated-function.
		     (while (search-forward-regexp " " (mon-g2be 1 t) t) 
		       (if  (not (= (line-end-position)(car (cddddr (mon-line-test-content 'whitespace t)))))
                            (progn    
                              (beginning-of-line)
                              (search-forward-regexp 
                               "\\(\\(\\([[:alnum:]]\\)\\([[:space:]]\\)\\([[:alnum:]]\\)\\)\\{1,1\\}\\)" nil t)
                              (replace-match "\\3_#_~_#_\\5"))))
                     (mon-g2be -1)
		     (setq file-list ())
		     (while (not (eobp))
		       (setq f-name (thing-at-point 'filename))
		       (cond ((or (mon-line-bol-is-eol) (mon-spacep-is-bol))
			      (forward-thing 'line))
			     ((string-match
                               "\\(\\(\\([[:alnum:]]\\)\\(_#_~_#_\\)\\([[:alnum:]]\\)\\)\\{1,1\\}\\)" f-name)
			      (setq f-name 
                                    (replace-regexp-in-string 
                                     "\\(\\(\\([[:alnum:]]\\)\\(_#_~_#_\\)\\([[:alnum:]]\\)\\)\\{1,1\\}\\)" 
                                     "\\3 \\5"  f-name)))
			     (t f-name))
		       (setq file-list (cons f-name file-list))	;; (thing-at-point 'filename) file-list)))
		       (forward-thing 'line))
		     (setq file-list (nreverse file-list)) ;; (newline) ;;(prin1 file-list (current-buffer)))
		     file-list)))
    (if intrp 
	(progn 
	  (newline)
	  (prin1 mgpd-rslt (current-buffer)))
        mgpd-rslt)))
;;
(unless (and (intern-soft "mon-buffer-get-and-proc-dirs" obarray) 
             (fboundp 'mon-buffer-get-and-proc-dirs))
(defalias 'mon-buffer-get-and-proc-dirs 'mon-get-proc-buffers-directories))
;;
;;; :TEST-ME (progn (newline)(prin1 (mon-get-proc-buffers-directories) (current-buffer)))

;;; ==============================
;;; :PREFIX "mdbl-"
;;; :TODO Should probably be refactored or callers. 
;;; Could use `mon-add-subdirs-to-list' but that descends.
;;; Better to just map over return value of `directory-files-and-attributes'
;;; looking for directories at the cadr of each sublist, e.g.:
;;; (mapcar #'(lambda (dirp) 
;;;             (and (mon-string-not-null-nor-zerop (cadr dirp))
;;;                  (car dirp)))
;;;         (directory-files-and-attributes default-directory t))
;;; :THIS Said 
;;; :CREATED <Timestamp: Thursday May 21, 2009 @ 08:06.42 PM - by MON>
(defun mon-dir-build-list (w-dir &optional not-concat-path)
  "Return a _list_ of directories in W-DIR.\n
When non-nil NOT-CONCAT-PATH returns a list _without_ the leading path.\n
:EXAMPLE\n\n\(mon-dir-build-list default-directory t\)\n
\(mon-dir-build-list \"./\" t\)
:NOTE May return non-sensical results when W-DIR is `default-directory' and
NOT-CONCAT-PATH is non-nil.\n
:NOTE This funcion was written \(and originally tested\) on w32 Emacsen without
significant consideration given the value of variables
`list-directory-brief-switches' and `list-directory-verbose-switches'.\n
:SEE-ALSO `mon-add-subdirs-to-list', `mon-dir-try-comp',
`mon-dir-hashed-complete', `mon-dir-hash-images', `directory-files',
`mon-file-dir-attributes->plist'.\n►►►"
  (save-excursion
    (save-window-excursion
      (let ((mdbl-cur-bfr (get-buffer (current-buffer)))
	    (mdbl-in-dir w-dir)
            mdbl-tmp-str 
            mdbl-rtn-dir)
	(setq mdbl-tmp-str    
	      (with-temp-buffer
                ;; :PREFIX "mdbl-lcl-"
		(let (mdbl-lcl1-this-bfr mdbl-lcl1-that-bfr mdbl-lcl1-bfr-ss)
		  (setq mdbl-lcl1-this-bfr (get-buffer (current-buffer)))
		  (list-directory w-dir t)
		  (setq mdbl-lcl1-that-bfr (get-buffer "*Directory*"))
		  (set-buffer mdbl-lcl1-that-bfr)
		  ;; :WAS (setq mdbl-lcl1-bfr-ss (buffer-substring-no-properties (point-min) (point-max)) )
                  (setq mdbl-lcl1-bfr-ss (mon-buffer-sub-no-prop))
		  (set-buffer mdbl-lcl1-this-bfr)
		  ;; :WAS (kill-buffer mdbl-lcl1-that-bfr)
                  (when (get-buffer mdbl-lcl1-that-bfr) (kill-buffer mdbl-lcl1-that-bfr))
		  (insert mdbl-lcl1-bfr-ss)
                  (mon-g2be -1)
		  (keep-lines "^d.*[0-9][0-9]:[0-9][0-9] .*$")
                  (mon-g2be -1)
		  (while (search-forward-regexp 
                          "\\(\\(^d.*[0-9][0-9]:[0-9][0-9][[:space:]]\\)\\(.*$\\)\\)" nil t)
		    (replace-match "\\3" ))
		  (mon-cln-trail-whitespace)
                  (mon-g2be -1)
		  (while (search-forward-regexp "^\\(.*\\)$" nil t)
		    (if (and (mon-line-bol-is-eol) (not (eobp)))
			(delete-char 1)
		      (replace-match "\\1|")))
		  (while (search-backward-regexp "^\|$" nil t)
		    (if (= (char-after) 124)
                        (delete-char 1)))
                  (mon-g2be -1)
		  (mon-delete-back-up-list (mon-g2be -1 t) (mon-g2be 1 t))
                  (mon-buffer-sub-no-prop))))
	(set-buffer mdbl-cur-bfr)
	(setq mdbl-rtn-dir (save-match-data (split-string mdbl-tmp-str "| ")))
	(setq mdbl-rtn-dir (delete "" mdbl-rtn-dir))
	(if (not not-concat-path)
	    (setq mdbl-rtn-dir
		  ;; :PREFIX "mdbl-lcl2-"
                  (let ((mdbl-lcl2-map-dir mdbl-rtn-dir)
			(mdbl-lcl2-conc-dir (concat mdbl-in-dir "/")))
		    (mapcar #'(lambda (mdbl-L-1) 
                                (concat mdbl-lcl2-conc-dir mdbl-L-1))
                            mdbl-lcl2-map-dir)))
	  mdbl-rtn-dir)
	;; (prin1 mdbl-rtn-dir (current-buffer))
	mdbl-rtn-dir))))
;;
;;; :TEST-ME (mon-dir-build-list *mon-emacs-root*)
;;; :TEST-ME (mon-dir-build-list *mon-emacs-root* t)


;;; ==============================
(provide 'mon-dir-utils)
;;; ==============================

 
;; Local Variables:
;; mode: EMACS-LISP
;; generated-autoload-file: "./mon-loaddefs.el"
;; End:

;;; ================================================================
;;; mon-dir-utils.el ends here
;;; EOF

