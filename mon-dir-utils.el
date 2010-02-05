;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*- mode: EMACS-LISP; -*_
;;; This is mon-dir-utils.el
;;; ================================================================
;;; DESCRIPTION:
;;; mon-dir-utils.el Provides a collection of handy functions and interactive
;;; commands for working with directories and files.
;;;
;;; FUNCTIONS:►►►
;;; `mon-dired-srt-alph',`mon-dired-srt-chrn',`mon-dired-srt-type'
;;; `mon-dired-srt-type-alph', `mon-dired-srt-type-chrn'
;;; `dired-up-directory-this-buffer', `dired-insert-dirs-recursive',
;;; `naf-explorer-artist', `naf-explorer-brand', `naf-dired-artist-letter',
;;; `naf-dired-brand-letter', `mon-save-current-directory',
;;; `mon-save-current-directory-to-file', `mon-add-subdirs-to-list',
;;; `mon-insert-subdirs-in-buffer', `mon-serial-rename', `mon-map-file-lines',
;;; `mon-path', `mon-copy-file-path', `mon-insert-path',
;;; `mon-get-buffers-directories', `mon-proc-buffers-directories',
;;; `mon-get-proc-buffers-directories', `mon-get-buffers-parent-dir'
;;; `mon-buffer-written-p', `mon-split-string-buffer-name',
;;; `mon-split-string-buffer-parent-dir', `mon-check-image-type'
;;; `mon-ebay-image-directory-not-ok', `mon-ebay-image-directory-ok-p'
;;; `mon-truncate-path-for-prompt', `mon-get-most-common-path'
;;; `mon-walk-buff-or-dir-path', `mon-dired-nef-dir', `mon-nef-dir-big', 
;;; `mon-nef-dir-converge', `mon-nef-dir-keep-3',`mon-nef-dir-fldr',
;;; `mon-nef-dir-conc-ranges', `mon-nef-dir-ranges',`mon-nef-dir-conc-dups',
;;; `mon-nef-dir-find-dups', `mon-nef-dir-rmv-empt', 
;;; `mon--local-url-for-bug' `mon-local-url-for-bug'
;;; `mon-toggle-dired-dwim-target', `mon-get-relative-w-absolute'
;;; `mon-copy-file-dired-as-list', `mon-copy-file-dired-as-string'
;;; `mon-get-ps2ascii', `mon-get-pdftotext', `mon-get-pdfinfo'
;;; `mon-w3m-dired-file', `mon-new-buffer-w-stamp'
;;; `mon-bind-nefs-photos-at-loadtime'
;;; FUNCTIONS:◄◄◄
;;;
;;; MACROS:
;;;
;;; METHODS:
;;;
;;; CLASSES:
;;;
;;; CONSTANTS: 
;;;
;;; VARIABLES:
;;; `*img-hash*' and (testing version `*temp-hash*')
;;; `*nef-img-hash*', `*jpg-img-hash*', `*bmp-img-hash*'
;;; `*mon-pdfinfo-exec-path*', `*mon-pdftotext-exec-path*'
;;;
;;; ALIASED/ADVISED/SUBST'D:
;;; `dired-up-here'                   -> `dired-up-directory-this-buffer' 
;;; `mon-make-path'                   -> `mon-build-path'
;;; `mon-get-w3m-dired-file'          -> `mon-w3m-dired-file'
;;; `mon-dired-kill-files-to-list'    -> `mon-copy-file-dired-as-list'
;;; `mon-dired-copy-files-to-list'    -> `mon-copy-file-dired-as-list'
;;; `mon-dired-kill-files-to-strings' -> `mon-copy-file-dired-as-string'
;;; `mon-dired-copy-files-to-strings' -> `mon-copy-file-dired-as-string'
;;;
;;; 
;;; DEPRECATED:
;;;
;;; RENAMED:
;;;
;;; MOVED:
;;; `*artist-naf-path*'                -> `mon-dir-locals-alist.el'
;;; `*brand-naf-path*'                 -> `mon-dir-locals-alist.el'
;;; `mon-comp-times-flt-pnt'           -> `mon-time-utils.el'
;;; `mon-conv-time-flt-pnt'            -> `mon-time-utils.el'
;;; `mon-get-file-mod-times'           -> `mon-time-utils.el'
;;; `mon-file-older-than-file-p'       -> `mon-time-utils.el'
;;; `mon-dired-srt-alph'               <- `mon-dir-utils-switches.el'
;;; `mon-dired-srt-chrn'               <- `mon-dir-utils-switches.el'
;;; `mon-dired-srt-type'               <- `mon-dir-utils-switches.el'
;;; `mon-dired-srt-type-alph'          <- `mon-dir-utils-switches.el'
;;; `mon-dired-srt-type-chrn'          <- `mon-dir-utils-switches.el'
;;; `dired-up-directory-this-buffer'   <- `mon-dir-utils-switches.el'
;;;
;;; REQUIRES:
;;; 
;;; :FILE cl.el
;;; `mon-multi-read-name' `mon-reduce-file-name' `naf-dired-image-dir'
;;; 
;;; :FILE mon-dir-locals-alist.el
;;;  |-> `*nefs_photos_nefs-alist*'`*nef-scan-nefs-path*'`*ebay-images-bmp-path*'
;;; :SEE (URL `http://www.emacswiki.org/emacs/mon-dir-locals-alist.el')
;;;
;;; :FILE mon-hash-utils.el
;;;  |-> `mon-hash-img-dir', `mon-complete-hashed-dir' -> `mon-hash-all-keys'
;;; :SEE (URL `http://www.emacswiki.org/emacs/mon-hash-utils.el')
;;;                                                   
;;; :FILE mon-replacement-utils.el
;;; :SEE (URL `http://www.emacswiki.org/emacs/mon-replacement-utils.el')
;;;
;;; :FILE mon-css-color.el 
;;; :SEE (URL `http://www.emacswiki.org/emacs/mon-css-color.el')
;;; :NOTE _before_ mon-rename-image-utils
;;;
;;; :FILE mon-rename-image-utils.el
;;; :SEE (URL `http://www.emacswiki.org/emacs/mon-css-color.el')
;;;
;;; :FILE mon-time-utils.el
;;; `mon-new-buffer-w-stamp' -> `mon-file-stamp-vrfy-put-eof', `mon-file-stamp'
;;; :SEE (URL `http://www.emacswiki.org/emacs/mon-time-utils.el')
;;;
;;; TODO:
;;;
;;; NOTES:
;;;
;;; SNIPPETS:
;;;
;;; THIRD PARTY CODE:
;;;
;;; AUTHOR: MON KEY
;;; MAINTAINER: MON KEY
;;; 
;;; PUBLIC-LINK: (URL `http://www.emacswiki.org/emacs/mon-dir-utils.el')
;;; FILE-PUBLISHED: <Timestamp: #{2009-09-02} - by MON KEY>
;;; 
;;; FILE-CREATED:
;;; <Timestamp: Monday May 11, 2009 @ 11:13.47 AM - by MON KEY>
;;; ================================================================
;;; This file is not part of GNU Emacs.
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 3, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; see the file COPYING.  If not, write to
;;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;;; Floor, Boston, MA 02110-1301, USA.
;;; ================================================================
;;; Permission is granted to copy, distribute and/or modify this
;;; document under the terms of the GNU Free Documentation License,
;;; Version 1.3 or any later version published by the Free Software
;;; Foundation; with no Invariant Sections, no Front-Cover Texts,
;;; and no Back-Cover Texts. A copy of the license is included in
;;; the section entitled "GNU Free Documentation License".
;;; A copy of the license is also available from the Free Software
;;; Foundation Web site at:
;;; (URL `http://www.gnu.org/licenses/fdl-1.3.txt').
;;; ================================================================
;;; Copyright © 2009 MON KEY 
;;; ==============================
;;; CODE:

;; :REQUIRED-BY `mon-multi-read-name', `mon-reduce-file-name'
(eval-when-compile (require 'cl)) 
(eval-when-compile (require 'mon-cl-compat))

;;; ==============================
;; :WAS (require 'mon-dir-locals-alist)
(load "mon-dir-locals-alist")
(require 'mon-hash-utils)
(require 'mon-replacement-utils)
(require 'mon-css-color) ;; :NOTE _before_ mon-rename-image-utils
(require 'mon-rename-image-utils)
;;; ==============================

;;; ==============================
(defvar *img-hash* nil
  "Hash-table for holding images in image directories.\n
:CALLED-BY `mon-hash-img-dir', `mon-try-comp-dir', `mon-complete-hashed-dir'.\n
Image directories defined in global vars: 
`*nef-scan-path*', `*nef-scan-nefs-path*',`*nef-scan-nef2-path*',
`*ebay-images-path*',`*ebay-images-bmp-path*',`*ebay-images-jpg-path*',
`*ebay-images-temp-path*'.")
;;
(when (not (bound-and-true-p *img-hash*))
  (setq *img-hash* (make-hash-table :test 'equal)))
;;
;;; :TEST-ME (boundp '*img-hash*)
;;; :TEST-ME *img-hash*
;;
;;;(progn (makunbound '*img-hash*) (unintern '*img-hash*) )

;;; ==============================
(defvar *nef-img-hash* nil
  "Hash-table for holding images in image directories.\n
:CALLED-BY `mon-hash-img-dir', `mon-try-comp-dir', `mon-complete-hashed-dir'.\n
Image directories defined in global vars `*nefs_photos_nefs-alist*'
`*nef-scan-path*', `*nef-scan-nefs-path*',`*nef-scan-nef2-path*',
`*ebay-images-path*',`*ebay-images-bmp-path*',`*ebay-images-jpg-path*',
`*ebay-images-temp-path*'.\n►►►")
;;
(when (not (bound-and-true-p *nef-img-hash*))
  (setq *nef-img-hash* (make-hash-table :test 'equal)))
;;
;;; :TEST-ME (assoc ".nef1" *ebay-images-lookup-path*)
;;; :TEST-ME (assoc ".nef2" *ebay-images-lookup-path*)
;;; :TEST-ME (boundp '*nef-img-hash*)
;;
;;;(progn (makunbound '*nef-img-hash*) (unintern '*nef-img-hash*) )

;;; ==============================
(defvar *bmp-img-hash* nil
  "Hash-table for holding images in image `*ebay-images-bmp-path*'.\n
:CALLED-BY `mon-hash-img-dir', `mon-try-comp-dir', `mon-complete-hashed-dir'.\n
Image directories defined in global vars: 
`*nef-scan-path*', `*nef-scan-nefs-path*',`*nef-scan-nef2-path*',
`*ebay-images-path*',`*ebay-images-bmp-path*',`*ebay-images-jpg-path*',
`*ebay-images-temp-path*'.\n►►►")
;;
(when (not (bound-and-true-p *bmp-img-hash*))
  (setq *bmp-img-hash* (make-hash-table :test 'equal)))
;;
;;; :TEST-ME (assoc ".bmp" *ebay-images-lookup-path*)
;;; :TEST-ME (boundp '*bmp-img-hash*)
;;
;;;(progn (makunbound '*bmp-img-hash*) (unintern '*bmp-img-hash*) )

;;; ==============================
(defvar *jpg-img-hash* 'nil
  "Hash-table for holding images in image `*ebay-images-bmp-path*'.
:CALLED-BY `mon-hash-img-dir', `mon-try-comp-dir', `mon-complete-hashed-dir'.
Image directories defined in global vars: 
`*nef-scan-path*', `*nef-scan-nefs-path*',`*nef-scan-nef2-path*',
`*ebay-images-path*',`*ebay-images-bmp-path*',`*ebay-images-jpg-path*',
`*ebay-images-temp-path*'.\n►►►")
;;
(when (not (bound-and-true-p *jpg-img-hash*))
  (setq *jpg-img-hash* (make-hash-table :test 'equal)))
;;
;;; :TEST-ME *jpg-img-hash*
;;; :TEST-ME (boundp '*jpg-img-hash*)
;;
;;;(progn (makunbound '*jpg-img-hash*) (unintern '*jpg-img-hash*) )

;;; ==============================
;;; :DIRECTORY-AND-DIRED-RELATED
;;; ==============================

;;; ==============================
;;; :MODIFICATIONS <Timestamp: #{2009-10-26T12:58:23-04:00Z}#{09441} - by MON KEY>
(defun mon-toggle-dired-dwim-target (&optional intrp)
  "Toggle `dired-dwim-target'.\n
:SEE-ALSO `dired-dwim-target-directory'.\n►►►"
  (interactive "p")
  (let ((toggle-dwim-target
         (if (bound-and-true-p dired-dwim-target)
             (progn
               (setq dired-dwim-target nil)
               (when intrp (message (concat "Variable `dired-dwim-target' turned off.\n"
                                            "Call `mon-toggle-dired-dwim-target' to toggle on"))))
             (progn
               (setq dired-dwim-target t)
               (when intrp (message (concat "Variable `dired-dwim-target' turned on.\n"
                                            "Call `mon-toggle-dired-dwim-target' to toggle off")))))))
    toggle-dwim-target))
;;
;;; :TEST-ME (mon-toggle-dired-dwim-target)
;;; :TEST-ME (mon-toggle-dired-dwim-target)

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
`mon-w3m-dired-file', `mon-kill-ring-save-w-props'.\n►►►"
  (interactive "P\np")
  (let ((dgmf (dired-get-marked-files localp)))
    (if intrp 
        (kill-new (pp dgmf))
        dgmf)))
;;
(defalias 'mon-dired-kill-files-to-list 'mon-copy-file-dired-as-list)
(defalias 'mon-dired-copy-files-to-list 'mon-copy-file-dired-as-list)

;;; ==============================
;;; :CREATED <Timestamp: #{2009-10-11T10:18:40-04:00Z}#{09417} - by MON>
(defun mon-copy-file-dired-as-string (&optional localp qt-strings intrp)
  "Copy dired file\(s\) to kill-ring.\n
When no file-names are marked copy file-name at point. 
If one or more file-names are marked copy these to kill-ring.
When LOCALP is non-nil do not copy full path of files to kill-ring.
When QT-STRINGS is non-nil or called-interactively with prefix arg
copy file-names such that when yanked they are inserted as quoted strings.\n
:ALIASED-BY `mon-dired-kill-files-to-strings'\n
:ALIASED-BY `mon-dired-copy-files-to-strings'\n
:SEE-ALSO `mon-copy-file-dired-as-list', `mon-copy-file-path',
`mon-w3m-dired-file', `mon-kill-ring-save-w-props'.\n►►►"
  (interactive "i\nP\np")
  (let ((dgmfk (mon-copy-file-dired-as-list localp)))
    (setq dgmfk
          (if qt-strings
              (mapconcat (lambda (x) (format "%S" x)) dgmfk "\n")
              (mapconcat 'identity dgmfk "\n")))
    (if intrp
        (progn    
          (kill-new dgmfk)
          (message dgmfk))
        dgmfk)))
;;
(defalias 'mon-dired-kill-files-to-strings 'mon-copy-file-dired-as-string)
(defalias 'mon-dired-copy-files-to-strings 'mon-copy-file-dired-as-string)

;;; ==============================
;;; :CREATED <Timestamp: #{2009-12-16T23:00:32-05:00Z}#{09513} - by MON KEY>
(if (and (fboundp 'w3m-find-file) (executable-find "w3m"))
(defun mon-w3m-dired-file (w3m-find-file)
  "Browse dired file at point with w3m.\n
:SEE-ALSO `mon-get-w3m-url-at-point-maybe', `mon-get-w3m-url-at-point',
`mon-w3m-read-gnu-lists-nxt-prv', `mon-copy-file-dired-as-list',
`mon-copy-file-dired-as-string', `dired-get-marked-files'.\n►►►"
  (interactive)
  (w3m-find-file (car (dired-get-marked-files))))
;;
(message "Can not find the w3m executable"))
;;
(when (fboundp 'mon-w3m-dired-file)
(defalias 'mon-get-w3m-dired-file  'mon-w3m-dired-file))
;;

;;; ==============================
;;; :CREATED <Timestamp: Tuesday July 21, 2009 @ 05:36.07 PM - by MON>
(defun mon-file-path-for-bug (&optional file-name-path insrtp yankp intrp)
  "Provide portable file-name-path for BUGd systems.\n
FILE-NAME-PATH \(a string\) should be a full pathname string and be located 
beneath `mon-emacs-root'.
When *bug-HG-path* is local return a path suitable for a remote machine.
When *bug-HG-path* is remote return a path suitable for the local machine.\n
:SEE-ALSO .\n►►►"
  (interactive "i\ni\nP\np")
  (let* ((fnp-tst (cond ((not file-name-path) "Path not under ")
                        ((and (not intrp) file-name-path)
                         (if (and (file-exists-p file-name-path)
                                  (string-match mon-emacs-root file-name-path))
                             file-name-path
                             "Path not under "))))
         (fnp (cond ((not (string-match "Path not" fnp-tst)) fnp-tst)
                    ((or intrp (string-match "Path not" fnp-tst))
                     (expand-file-name
                      (read-file-name 
                       (if (string-match "Path not" fnp-tst)
                           (concat fnp-tst mon-emacs-root " :")
                           "Which path shall we build? :")
                       (expand-file-name default-directory)
                       (if (mon-buffer-written-p)
                           buffer-file-name
                           (expand-file-name default-directory))
                       t
                       (if (mon-buffer-written-p)
                           (file-name-nondirectory buffer-file-name)))))))
         (fnp-rel (concat "/" (file-relative-name fnp mon-emacs-root))))
    (cond (insrtp
           (if yankp
               (progn
                 (kill-new (concat *bug-HG-path* fnp-rel))
                 (insert (format "(find-file \"%s\")" (concat *bug-HG-path* fnp-rel))))
               (insert (format "(find-file \"%s\")" (concat *bug-HG-path* fnp-rel)))))
          ((or intrp yankp)
           (progn
             (kill-new (concat *bug-HG-path* fnp-rel))
             (when intrp
               (message "Yank path to insert: %s "(concat *bug-HG-path* fnp-rel)))))
          (t (format "%s%s"*bug-HG-path* fnp-rel)))))
;;          
;;; :TEST-ME (mon-file-path-for-bug)
;;; :TEST-ME (mon-file-path-for-bug nil t t)
;;; :TEST-ME (mon-file-path-for-bug "bubba" nil t)
;;; :TEST-ME (mon-file-path-for-bug (concat mon-naf-mode-notes "/latin-lorum-ipsum.dbc") t t)
;;; :TEST-ME (mon-file-path-for-bug (concat mon-naf-mode-notes "/latin-lorum-ipsum.dbc" t) 

;;; ==============================
;;; :CREATED <Timestamp: Wednesday July 15, 2009 @ 02:19.43 PM - by MON>
(defun mon--local-url-for-bug (is-url file-string)
  "Helper function for interactive `mon-local-url-for-bug'.\n
:SEE-ALSO `mon-local-url-for-bug' and `*bug-HG-path*'.\n►►►"
  (concat "file:///" *bug-HG-path* "/" file-string))
;;
(defun mon-local-url-for-bug (file-string)
  "Prompt for a url to concat onto the common path shared between the MON KEY 
machine and the BUG'd system.\n
:SEE-ALSO `mon--local-url-for-bug',`*bug-HG-path*'.\n►►►"
  (interactive (list (read-string (format "Url beneath file: %s" 
                                  (concat (nth 6 (assoc 3 *mon-emacsd*)) "/")))))
   ;; (prin1 (mon--local-url-for-bug file-string) (current-buffer))
  (prin1 (mon--local-url-for-bug nil file-string)  (current-buffer)))
;;
;;; :TEST-ME (mon--local-url-for-bug "bubba")
;;; :TEST-ME (call-interactively 'mon-local-url-for-bug)

;;; ==============================
(defun mon-dired-srt-alph ()
  "Set ls switch to sort Dired direcotry alphebetically.\n
:SEE-ALSO `mon-dired-srt-chrn', `mon-dired-srt-type', `mon-dired-srt-type',
`mon-dired-srt-type-alph', `mon-dired-srt-type-chrn'.\n►►►"
  (interactive)
  (dired-sort-other "-la"))

;;; ==============================
(defun mon-dired-srt-chrn ()
  "Set ls switch to sort Dired direcotry chronologically.\n
:SEE-ALSO `mon-dired-srt-alph', `mon-dired-srt-type', `mon-dired-srt-type',
`mon-dired-srt-type-alph', `mon-dired-srt-type-chrn'.\n►►►"
  (interactive)
  (dired-sort-other "-lt")) ;;mon-dired-srt-alph

;;; ==============================
(defun mon-dired-srt-type ()
  "Set ls switch to sort Dired direcotry by type.\n
:SEE-ALSO `mon-dired-srt-alph', `mon-dired-srt-chrn', `mon-dired-srt-type',
`mon-dired-srt-type-alph', `mon-dired-srt-type-chrn'.\n►►►"
  (interactive)
  (dired-sort-other "-lX"))

;;; ==============================
(defun mon-dired-srt-type-alph ()
  "Set ls switch to sort Dired direcotry by type -> alphabetically.\n
:SEE-ALSO `mon-dired-srt-alph', `mon-dired-srt-chrn', `mon-dired-srt-type', 
`mon-dired-srt-type', `mon-dired-srt-type-chrn'.\n►►►"
  (interactive)
  (dired-sort-other "-lXa"))

;;; ==============================
(defun mon-dired-srt-type-chrn ()
  "Set ls switch to sort Dired direcotry by type -> chronologically.\n
:SEE-ALSO `mon-dired-srt-alph', `mon-dired-srt-chrn', `mon-dired-srt-type', 
`mon-dired-srt-type', `mon-dired-srt-type-alph'.\n►►►"
  (interactive)
  (dired-sort-other "-lXt"))


;;; ==============================
;;; :COURTESY Stefan Reichor :HIS xsteve-functions.el :VERSION 2001-03-28
;;; :WAS `dired-up-directory-this-buffer'
;;; :NOTE (define-key dired-mode-map "\177" 'dired-up-directory-this-buffer)
(defun dired-up-directory-this-buffer ()
  "Move up directory tree i.e. `../' to a new dired buffer killing current one.\n
:ALIASED-BY `dired-up-here'
:SEE-ALSO .\n►►►"
  (interactive)
  (let ((buffer))
    (setq buffer (current-buffer))
    (dired-up-directory)
    (kill-buffer buffer)))
;;
(defalias 'dired-up-here 'dired-up-directory-this-buffer)

;;; ==============================
;;; :COURTESY Stefan Reichor, :HIS xsteve-functions.el :VERSION 2001-03-28
;;; :WAS `dired-insert-dirs-recursive'
;;; :NOTE (define-key dired-mode-map [(meta i)] 'dired-insert-dirs-recursive)
(defun dired-insert-dirs-recursive (dirname)
  "In dired recursively inserts the subdirs of dir at point.\n
:SEE-ALSO `dired-up-directory-this-buffer'.\n►►►"
  (interactive (list (dired-get-filename)))
  (dired-maybe-insert-subdir dirname "-laR"))

;;; ==============================
;;; :REQUIRES `mon-file-stamp-vrfy-put-eof' -> :FILE mon-time-utils.el
;;; :REQUIRES `mon-file-stamp'              -> :FILE mon-time-utils.el
;;; :SEE (URL `http://www.emacswiki.org/emacs/mon-time-utils.el')
;;; :CREATED <Timestamp: #{2009-12-18T21:51:32-05:00Z}#{09515} - by MON>
(defun mon-new-buffer-w-stamp (new-buffer-w-name &optional auto-save intrp)
  "Create and display NEW-BUFFER-W-NAME pre-populated with `mon-file-stamp'.\n
The pre-filled line `:FILE' line is formatted as:\n
\x3B;; :FILE default-directory/NEW-BUFFER-W-NAME\n
:NOTE The file is not written yet. Save it yourself if that is what you want.
When called-interactively prompts for NEW-BUFFER-W-NAME.
When AUTO-SAVE is non-nil or called-interactively with prefix-arg 
do not prompt before saving NEW-BUFFER-W-NAME.
If NEW-BUFFER-W-NAME is an existing file in default-directory signal an error.
:EXAMPLE\n(call-interactively 'mon-new-buffer-w-stamp)\n
:SEE-ALSO `mon-file-stamp', `mon-insert-file-template',`mon-lisp-stamp',
`mon-timestamp', `mon-stamp', `mon-accessed-stamp'.\n►►►"
  (interactive "i\nP\np")
  (let ((nbwn (make-symbol "--nbwn--"))
        (nb-name))
    (setq nb-name (if intrp 
                      (read-string "New buffer-file-name: ")
                      new-buffer-w-name))
    (if (file-exists-p (concat default-directory nb-name))
        (error "Pre-existing file %s in %s" nb-name (pwd))
        (setq nb-name (concat 
                       (expand-file-name 
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
               (format "Buffer visiting the unwritten file `%s'. Save now (Y) or proceed without saving (N)? " 
                       (file-relative-name (buffer-file-name) "../")))
          (write-file (buffer-file-name) t)
          (buffer-file-name)))))
;;
;;; :TEST-ME (mon-new-buffer-w-stamp "testing")
;;; :TEST-ME (call-interactively 'mon-new-buffer-w-stamp)

 ;;; ==============================
;;; :CREATED <Timestamp: #{2009-10-27T15:50:08-04:00Z}#{09442} - by MON>
(defun mon-get-dir-name-absolute (dir-name)
  "Return absolute directory file-name of DIR-NAME.\n
:EXAMPLE\n(mon-get-dir-name-absolute \(getenv \"HOME\"\)\)\n
:SEE-ALSO `file-truename' `expand-file-name', `directory-file-name',
`file-name-directory'.  `mon-multi-copy-file', `mon-copy-files-in-sub-dirs',
`mon-get-relative-w-absolute' `mon-get-dir-name-absolute',
`mon-reduce-file-name', `mon-build-path', `mon-split-string-buffer-name',
`mon-get-buffers-parent-dir' `mon-split-string-buffer-parent-dir',
`mon-walk-buff-or-dir-path', `mon-get-most-common-path',
`mon-get-buffers-parent-dir', `mon-get-proc-buffers-directories',
`mon-get-buffers-directories', `mon-split-string-buffer-name',
`mon-split-string-buffer-parent-dir-quick',
`mon-split-string-buffer-parent-dir', `file-relative-name'.\n►►►"
(let ((ft (file-truename dir-name)))
  (if (file-directory-p ft)
      (directory-file-name ft)
      (directory-file-name (file-name-directory ft)))))
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
;;; :CREATED <Timestamp: #{2009-12-30T18:26:28-05:00Z}#{09533} - by MON KEY>
(defun mon-get-relative-w-absolute (match-pattern file-or-dir)
  "Return FILE-OR-DIR as a two element list of strings split on MATCH-PATTERN.\n
Useful for comparing a path difference by diffing the elets of list1 with list2.\n
:EXAMPLE
\(mon-get-relative-w-absolute \(file-truename \(getenv \"HOME\"\)\) default-directory\)\n
:SEE-ALSO `mon-multi-copy-file', `mon-copy-files-in-sub-dirs',
`mon-get-relative-w-absolute' `mon-get-dir-name-absolute',
`mon-reduce-file-name', `mon-build-path', `mon-split-string-buffer-name',
`mon-get-buffers-parent-dir' `mon-split-string-buffer-parent-dir',
`mon-walk-buff-or-dir-path', `mon-get-most-common-path',
`mon-get-buffers-parent-dir', `mon-get-proc-buffers-directories',
`mon-get-buffers-directories', `mon-split-string-buffer-name',
`mon-split-string-buffer-parent-dir-quick',
`mon-split-string-buffer-parent-dir', `file-relative-name'.\n►►►"
  (unwind-protect  
       (let ((fod
              (if (equal (string-match-p "[A-z]:\\\\?" file-or-dir) 0)
                  (file-truename file-or-dir)
                  file-or-dir)))
         (cond ((or (string= (mon-get-dir-name-absolute fod)
                             (if (and (not (file-directory-p fod))
                                      (file-exists-p fod))
                                 (mon-get-dir-name-absolute fod)))
                    (string= "~/" match-pattern)
                    (string= "~/" (substring fod 0 2)))
                (let ((i (mon-get-dir-name-absolute fod)))
                  (save-match-data
                    `(,(split-string i "/" t) ,(split-string i "/" t)))))
               ((string-match match-pattern fod)
                `(,(save-match-data
                    (split-string 
                     (substring fod (match-end 0)) "/" t)) 
                   ,(save-match-data
                     (split-string 
                      (mon-get-dir-name-absolute
                       (substring fod (match-beginning 0)(match-end 0))) "/" t))))))
    (set-match-data nil)))
;;
;;; :TEST-ME (mon-get-relative-w-absolute "" (getenv "HOME"))
;;; :TEST-ME (mon-get-relative-w-absolute "~/"  (buffer-file-name))
;;; :TEST-ME (mon-get-relative-w-absolute ""  (buffer-file-name))
;;; :TEST-ME (mon-get-relative-w-absolute "c:/" default-directory)
;;; (mon-get-relative-w-absolute "c:/" default-directory)
;;; (mon-get-relative-w-absolute "" default-directory)
;;; :TEST-ME (mon-get-relative-w-absolute "" default-directory)
;;; :TEST-ME (mon-get-relative-w-absolute "" 
;;;            (file-relative-name (buffer-file-name) (getenv "HOME") ))
;;; :TEST-ME (mon-get-relative-w-absolute 
;;;           (file-truename (getenv "HOME")) default-directory)

;;; ==============================
;;; :MODIFICATIONS <Timestamp: #{2010-02-04T15:31:47-05:00Z}#{10054} - by MON KEY>
;;; :CHANGED `mapcar's -> `mapc's :ADDED local var WHAT-HPPD for return value.
;;; :CREATED <Timestamp: Monday June 22, 2009 @ 02:54.20 PM - by MON>
(defun mon-copy-files-in-sub-dirs (gather-dir destination-dir)
  "For sub-directories 'sd' of GATHR-DIR copy files of 'sd' to DESTINATION-DIR.\n
GATHER-DIR and DESTINATION-DIR are full paths.
Copy _only_ files not directories. 
Does not recursively descend 'sd' subdirectories.
Does not copy sub-directories of 'sd' to DESTINATION-DIR.\n
Return a list of strings informing what happened:
Elements of list have the form:
  \"Copied file: <FILE> to Directory: <DIRECTORY>\"\n
:SEE-ALSO `mon-multi-copy-file', `mon-copy-files-in-sub-dirs',
`mon-get-buffers-parent-dir', `mon-get-proc-buffers-directories',
`mon-get-buffers-directories', `mon-split-string-buffer-name',
`mon-split-string-buffer-parent-dir-quick', `mon-split-string-buffer-parent-dir'.\n►►►"
  ;; First, gather sub-dirs.
  (let* ((gthr-dir (if (and (file-exists-p gather-dir) (file-directory-p gather-dir))
                       (directory-file-name gather-dir)
                       (error 
                        "Directory does not exist or names a file: \n%s" gather-dir)))
         (to-dir (if (and (file-exists-p destination-dir) (file-directory-p destination-dir))
                     (directory-file-name destination-dir)
                     (error 
                      "Directory does not exist or names a file: \n%s" destination-dir)))
         ;; (to-dir-fname (car (last (split-string to-dir "/" t))))
         (gthr-in gthr-dir)
         (walk-dir (directory-files gthr-in t))
         (gthrd)
         (what-hppd))
    (setq gthrd ())
    ;; :TODO use `mapc' or `dolist'
    ;; :WAS (mapcar '(lambda (x)
    (mapc #'(lambda (x)
              (let ((in-here (if (and (file-directory-p x)
                                      (not (or (string= (concat gthr-in "/.") x)
                                           (string= (concat gthr-in "/..") x)
                                           (string= to-dir x))))
                                 x)))
                (when in-here  (setq gthrd (cons in-here gthrd))))) walk-dir)
    ;; :TODO use `mapc' or `dolist'
    ;; :WAS (mapcar (lambda (x) 
    ;; Now copy files per subdir to dest-dir.
    (mapc #'(lambda (x) 
              (let* ((in-dir x)
                     (walk-sub (directory-files in-dir t)))
                ;; :WAS (mapcar #'(lambda (y)
                (mapc #'(lambda (y)
                          (when (and (not (file-directory-p y)) 
                                     (not (or (string= (concat in-dir "/" ".") y) 
                                              (string= (concat in-dir "/" "..") y))))
                            (push (format "Copied file: %s to Directory: %s" y to-dir) what-hppd)
                            (copy-file y to-dir t)))
                      walk-sub)))
          gthrd)
    (setq what-hppd (nreverse what-hppd))))

;;; ==============================
;;; :COURTESY Thierry Volpiatto :HIS tv-utils.el :WAS `lmcp'
(defun mon-multi-copy-file (file &optional list-of-dir)
  "Copy `file' in multiple directories.\n
At each prompt for a directory add + to input another directory name.
When '+' is not added to directory name input is finished and function returns.\n
:SEE-ALSO `mon-multi-copy-file', `mon-copy-files-in-sub-dirs',
`mon-get-relative-w-absolute' `mon-get-dir-name-absolute',
`mon-reduce-file-name', `mon-build-path', `mon-split-string-buffer-name',
`mon-get-buffers-parent-dir' `mon-split-string-buffer-parent-dir',
`mon-walk-buff-or-dir-path', `mon-get-most-common-path',
`mon-get-buffers-parent-dir', `mon-get-proc-buffers-directories',
`mon-get-buffers-directories', `mon-split-string-buffer-name',
`mon-split-string-buffer-parent-dir-quick',
`mon-split-string-buffer-parent-dir', `file-relative-name'.\n►►►"
  (interactive "fFile: ")
  (let* ((dest-list nil)
         (final-list
          (if list-of-dir
              list-of-dir
              (mon-multi-read-name 'read-directory-name))))
    (loop for i 
          in final-list
	  do (copy-file file i t))))

;;; ==============================
;;; :COURTESY Thierry Volpiatto :HIS tv-utils.el :WAS `multi-read-name'
(defun* mon-multi-read-name (&optional (fn 'read-string))
  "Prompt indefinely while a is `+' suffixed to read value.\n
Return a list of all inputs in `var'.
Accepts specification of an alternate input function to use.\n
:SEE-ALSO .\n►►►"
  (labels ((multiread ()
             (let ((stock)
                   (str (funcall fn (cond ((eq fn 'read-string)
                                           "String(add + to repeat): ")
                                          ((eq fn 'read-directory-name)
                                           "Directory(add + to repeat): ")
                                          (t
                                           "File(add + to repeat): ")))))
               (push (replace-regexp-in-string "\+" "" str) stock)
               (cond ((string-match "\+" str)
                      (push (car stock) var)
                      (multiread))
                     (t
                      (push (car stock) var)
                      (nreverse var))))))
    (let (var)
      (multiread))))

;;; ==============================
;;; :COURTESY Thierry Volpiatto :HIS tv-utils.el :WAS `cat'
;;; :MODIFICATIONS <Timestamp: #{2009-10-26T11:19:18-04:00Z}#{09441} - by MON>
;;; :ADDED `make-symbol', `buffer-substring-no-properties', and docstring.
(defmacro mon-cat (file)
  "Return FILE contents as string - like `cat'.\n
:SEE-ALSO .\n►►►"
  (let ((fc (make-symbol "fc")))
    `(let ((fc (with-temp-buffer
                 (insert-file-contents ,file)
                 (buffer-substring-no-properties (buffer-end 0) (buffer-end 1)))))
         
       fc)))

;;; ==============================
;;; :COURTESY Thierry Volpiatto :HIS tv-utils.el :WAS `tv-reduce-file-name'
;;; :MODIFICATIONS <Timestamp: #{2009-09-01T20:39:35-04:00Z}#{09363} - by MON>
(defun* mon-reduce-file-name (fname level &key unix-close expand)
  "Reduce file-name by LEVEL (an integer) depending on LEVEL's value.\n
If LEVEL is positive reduce by end else by beginning.
UNIX-CLOSE (a boolean) non-nil close filename with '/'.
EXPAND (a boolean) when non-nil `expand-file-name' of FNAME.\n
:EXAMPLE\n\(mon-reduce-file-name data-directory 3\)\n
:SEE-ALSO `mon-multi-copy-file', `mon-copy-files-in-sub-dirs',
`mon-get-relative-w-absolute' `mon-get-dir-name-absolute',
`mon-reduce-file-name', `mon-build-path', `mon-split-string-buffer-name',
`mon-get-buffers-parent-dir' `mon-split-string-buffer-parent-dir',
`mon-walk-buff-or-dir-path', `mon-get-most-common-path',
`mon-get-buffers-parent-dir', `mon-get-proc-buffers-directories',
`mon-get-buffers-directories', `mon-split-string-buffer-name',
`mon-split-string-buffer-parent-dir-quick',
`mon-split-string-buffer-parent-dir', `file-relative-name'.\n►►►"
  (let* ((exp-fname (expand-file-name fname))
         (fname-list ;; :WAS (split-string (if expand exp-fname fname) "/" t))
          (save-match-data (split-string (if expand exp-fname fname) "/" t)))
         (len (length fname-list))
         (pop-list (if (< level 0)
                       (cl::subseq fname-list (* level -1))
                       (cl::subseq fname-list 0 (- len level))))
         (result (mapconcat #'(lambda (x) x) pop-list "/")))
    (if unix-close
        (if expand
            (if (< level 0)
                (concat "../" result "/")
                (concat "/" result "/"))
            (if (string-match "~/" result)
                (concat result "/")
                (if (< level 0)
                    (concat "../" result "/")
                    (concat "/" result "/"))))
        (if expand
            (if (< level 0)
                (concat "../" result "/")
                (concat "/" result "/"))
            (if (string-match "~/" result)
                (concat result "/")
                (if (< level 0)
                    (concat "../" result "/")
                    (concat "/" result "/")))))))
;;
;;; :TEST-ME (mon-reduce-file-name data-directory 3)

;;; ==============================
;;; :CREATED <Timestamp: Saturday May 30, 2009 @ 02:42.31 PM - by MON>
(defun mon-build-path (expand-path suffix-path &rest more-paths)
  "Return a path with EXPAND-PATH concatenated to SUFFIX-PATH.\n
When MORE-PATHS is non-nil each additional string is appended to path.
Signal an error if any of the args aren't in the path.\n
:EXAMPLE
\(mon-build-path *artist-naf-path* \"C-Artists names\" \"Cappiello \(Leonetto\)\"\)
\(mon-build-path *artist-naf-path* \"C-Artists names\"\)\n
:EXAMPLE {:CALLED-PROGRAMATICALLY}
\(apply 'mon-build-path *artist-naf-path* \"C-Artists names\"
\(split-string \"Cappiello \(Leonetto\)/Aux Trois Quartier/mmm\" \"/\" t\)\)\n
:SEE-ALSO .\n►►►"
  (let (tst-pth stack-pth f-pth)
    (setq tst-pth '(lambda (tst &optional in-sub) 
                    (if (and (not (file-symlink-p tst))
                             (or (file-exists-p tst) (file-directory-p tst)))
                        t
                        (cond (in-sub
                               (error "The dir/file named `%s'\nIsn't in the path `%s/'" 
                                      (file-name-nondirectory tst) in-sub))
                              ((not in-sub)
                               (error "The dir/file named `%s' isn't in the path" 
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
(defalias 'mon-make-path 'mon-build-path)
;;
;;; :TEST-ME (mon-build-path *artist-naf-path* "C-Artists names" "Cappiello (Leonetto)")
;;; :TEST-ME (mon-build-path *artist-naf-path* "C-Artists names")
;;; :TEST-ME (apply 'mon-build-path *artist-naf-path* "C-Artists names"
;;;          (split-string "Cappiello (Leonetto)/Aux Trois Quartier/mmm" "/" t))
       
;;; ==============================
;;; :TODO Make a global var for this and default to it. 
;;; :TODO Add post save hook to record the directory for new or modified .naf's.
;;; :MODIFICATIONS <Timestamp: Monday February 09, 2009 @ 09:34.29 PM - by MON>
;;; :COURTESY Stefan Reichor, stefan@xsteve.at :HIS xsteve-functions.el :VERSION 2001-03-28
(defun mon-save-current-directory ()
  "Save the current directory to a file.\n
:SEE-ALSO `mon-save-current-directory-to-file'.\n►►►"
  (interactive)
  (let* ((current-sys-type (concat mon-emacs-root "/current-directory"))
	 (dir default-directory)
         (file-name (shell-quote-argument (expand-file-name dir))))
    (with-current-buffer (find-file-noselect current-sys-type)
      ;; Overwrites the file. As commented appends:
      ;; (delete-region (point-min) (point-max)) 
      (when (equal system-type 'windows-nt)
        (setq file-name (replace-regexp-in-string "/" "\\\\" file-name)))
      (goto-char (point-max))
      (princ file-name (current-buffer)) ;;  (insert file-name)
      (newline)
      (save-buffer)
      (message "Saved directory '%s' to %s" file-name current-sys-type)
      (kill-buffer (current-buffer)))))

;;; ==============================
;;; :MODIFICATIONS <Timestamp: #{2009-08-11T18:12:53-04:00Z}#{09332} - by MON>
(defun mon-save-current-directory-to-file (&optional intrp)
  "Save the current files directory path to a file.\n
Default file is held by global var `*mon-record-current-directory*'.\n
:SEE-ALSO `mon-save-current-directory', `mon-append-to-register',
`append-to-buffer', `append-output-to-file'.\n►►►"
  (interactive "p")
  (let* ((you-rang intrp)
	 (caller-wants (when (and you-rang)
			 (read-directory-name "Gimme a directory path:")))
	 (alt-file  (when (and you-rang)
		      ;; (read-file-name
		      (read-string
		       "Gimme a filename to hold this weight:"
		       ;; caller-wants
		       nil
		       (concat "naf-directory-list-" (format-time-string "%m-%d-%Y") ".dbc"))))
	 (build-file (when (and you-rang)
		       (concat caller-wants alt-file)))
	 (current-sys-type (if (and you-rang)
			       build-file
                             ;; (concat mon-emacs-root "/current-directory")))
                               *mon-record-current-directory*))
	 (dir default-directory)
	 (file-name (shell-quote-argument (expand-file-name dir))))
    (with-current-buffer (find-file-noselect current-sys-type)
      ;; Overwrites the file. As commented appends:
      ;; (delete-region (point-min) (point-max)) 
      (when (equal system-type 'windows-nt)
	(setq file-name (replace-regexp-in-string "/" "\\\\" file-name)))
      (goto-char (point-max))
      (princ file-name (current-buffer)) ;; (insert file-name)
      (newline)
      (save-buffer)
      (message "Saved directory '%s' to %s" file-name current-sys-type)
      (kill-buffer (current-buffer)))))

;;; ==============================
;;; :MODIFICATIONS <Timestamp: 2009-08-01-W31-6T11:38:50-0400Z - by MON>
;;; :REMOVED Best I can see the (and * t) is totally pointless; removed it.
;;; :CREATED <Timestamp: Saturday May 23, 2009 @ 11:38.18 AM - by MON>
(defun mon-buffer-written-p (&optional insrtp intrp)
  "Non-nil current buffer has been written to a file or created with `find-file'
  and _can_ be written in current directory - whether it has been or not).\n
:SEE-ALSO `mon-buffer-exists-p', `mon-buffer-written-p', `mon-with-file-buffer',
`mon-buffer-name->kill-ring', `mon-get-buffers-parent-dir',
`mon-get-proc-buffers-directories', `mon-get-buffers-directories',
`mon-split-string-buffer-name', `mon-split-string-buffer-parent-dir'
`with-current-buffer', `with-temp-file', `with-temp-buffer'.\n►►►"
  (interactive "P\np")
  (let* ((written-p (buffer-file-name))
         ;; :WAS (and (buffer-file-name) t)) 
         ;; and w/ t is not needed! Why was this here?
	 (has-or-not (if written-p "has or can be"  "_hasn't or can't_ be")))
    (when intrp
      (message "buffer `%s' %s written to file." (buffer-name) has-or-not))
    (when insrtp 
      (insert (format "#Buffer `%s' %s written to file." (buffer-name) has-or-not)))
    written-p))
;;
;;; :TEST-ME (mon-buffer-written-p)
;;; :TEST-ME (mon-buffer-written-p)
;;; :TEST-ME (mon-buffer-written-p t)
;;; :TEST-ME (call-interactively 'mon-buffer-written-p) 

;;; ==============================
;;; :MODIFICATIONS <Timestamp: 2009-08-01-W31-6T11:48:58-0400Z - by MON>
;;; :ADDED optional args insrtp intrp
;;; :CREATED <Timestamp: Saturday May 23, 2009 @ 11:50.56 AM - by MON>
(defun mon-split-string-buffer-name (&optional insrtp intrp)
  "Return current `buffer-name' as a list with split-string.
When INSRTP is non-nil or called-interactively with prefix arg insert the list
of split strings at point.\n
:SEE-ALSO `mon-buffer-exists-p', `mon-buffer-written-p', `mon-with-file-buffer',
`mon-buffer-name->kill-ring', `mon-get-buffers-parent-dir',
`mon-get-proc-buffers-directories', `mon-get-buffers-directories',
`mon-split-string-buffer-name', `mon-split-string-buffer-parent-dir'
`mon-split-string-buffer-parent-dir-quick', `with-current-buffer',
`with-temp-file', `with-temp-buffer'.\n►►►"
  (interactive "P\np")
(let ((buf-split
       (save-match-data
         (if (mon-buffer-written-p)
             (split-string (buffer-file-name) "/" t)
             (split-string default-directory "/" t)))))
    (when intrp (message "%S" buf-split))
    (when insrtp (insert (format "%S" buf-split)))
    buf-split))
;;
;;; :TEST-ME (mon-split-string-buffer-name)
;;; :TEST-ME (mon-split-string-buffer-name t)
;;; :TEST-ME (call-interactively 'mon-split-string-buffer-name)

;;; ==============================
;;; :MODIFICATIONS <Timestamp: 2009-08-01-W31-6T11:48:58-0400Z - by MON>
;;; :ADDED optional args insrtp
;;; :CREATED <Timestamp: Saturday May 23, 2009 @ 08:17.10 PM - by MON>
(defun mon-split-string-buffer-parent-dir-quick (&optional insrtp)
  "Like `mon-split-string-buffer-parent-dir' but with less checks.\n
When INSERTP is non nil or called-interactively with prefix arg
insert the split in buffer. Moves point.\n
:SEE-ALSO `mon-buffer-exists-p', `mon-buffer-written-p', `mon-with-file-buffer',
`mon-buffer-name->kill-ring', `mon-get-buffers-parent-dir',
`mon-get-proc-buffers-directories', `mon-get-buffers-directories',
`mon-split-string-buffer-name', `mon-split-string-buffer-parent-dir'
`mon-split-string-buffer-parent-dir-quick', `with-current-buffer',
`with-temp-file', `with-temp-buffer'.\n►►►"
  (interactive "P") 
  (let ((sss-bpdq ;; WAS (split-string (directory-file-name (expand-file-name "./"))"/" t )
         (save-match-data
           (split-string (directory-file-name (expand-file-name "./"))"/" t ))))
    (when insrtp (insert (format "%S" sss-bpdq)))
    sss-bpdq))
;;
;;; :TEST-ME (mon-split-string-buffer-parent-dir-quick)
;;; :TEST-ME (mon-split-string-buffer-parent-dir-quick t)
;;; :TEST-ME (call-interactively 'mon-split-string-buffer-parent-dir-quick)

;;; ==============================
;;; :MODIFICATIONS <Timestamp: 2009-08-01-W31-6T11:48:58-0400Z - by MON>
;;; :ADDED optional args insertp intrp
;;; :REMOVED (message "%S" rmvd) ;;message call is redundant
;;; :CREATED <Timestamp: Saturday May 23, 2009 @ 12:31.43 PM - by MON>
(defun mon-split-string-buffer-parent-dir (&optional insrtp intrp)
  "Return buffers parent sans buffer's file name as a split-string list.
When `buffer-file-name' is nil return parents of buffers `default-directory'
\(inclusive=) as list of strings.\n
Like `mon-split-string-buffer-name' but does not strip tail of buffer's 
`default-directory' when `mon-buffer-written-p' is nil.
Unlike =(file-name-nondirectory buffer-file-name\) which does not check if buffer 
has a file name - throws an error instead.\n
:NOTE Could also accomplish with `mon-split-string-buffer-parent-dir-quick'.\n
e.g. \n\(split-string \(directory-file-name \(expand-file-name \"./\"\)\)\"/\" t \)\n
:SEE-ALSO `mon-buffer-exists-p', `mon-buffer-written-p', `mon-with-file-buffer',
`mon-buffer-name->kill-ring', `mon-get-buffers-parent-dir',
`mon-get-proc-buffers-directories', `mon-get-buffers-directories',
`with-current-buffer', `with-temp-file', `with-temp-buffer'.\n►►►"
(interactive "P\np")
  (let* ((is-written (mon-buffer-written-p))
	 (l-mod (if is-written ;; :WAS (split-string (buffer-file-name) "/" t))
		    (save-match-data (split-string (buffer-file-name) "/" t))
                      (split-string default-directory "/" t)))
	 (l-last  (if (and 
		       ;; File exists in dir.
		       is-written  
		       ;; Don't delete top level dir.
		       (not (< (length l-mod) 1)))
		      ;; Get count for deleting last string in path.
		      (nth (- `,(length l-mod) 1) l-mod)
		    ;; Don't remove objects eq nil below.
		    '()))		
	 (rmvd))
    (setq rmvd (remq l-last l-mod))
    (when insrtp (insert (format "%S" rmvd)))
    ;; (message "%S" rmvd) ;;message call is redundant
    rmvd))
;;
;;; :TEST-ME (mon-split-string-buffer-parent-dir)
;;; :TEST-ME (mon-split-string-buffer-parent-dir t)
;;; :TEST-ME (call-interactively 'mon-split-string-buffer-parent-dir)

;;; ==============================
;;; :MODIFICATIONS <Timestamp: #{2009-10-27T16:24:19-04:00Z}#{09442} - by MON KEY>
;;; :MODIFICATIONS <Timestamp: 2009-08-01-W31-6T11:26:49-0400Z - by MON>
;;; :ADDED optional insrtp, intrp args 
;;; :CREATED <Timestamp: Saturday May 23, 2009 @ 11:28.41 AM - by MON>
(defun mon-get-buffers-parent-dir (&optional full insrtp intrp)
  "Return buffers' parent directory as a string.
By default returns buffer's parent directory _only_.
When FULL is non-nil return full path of buffers parent directory as string.
If we are in a buffer which has been written to a file or _can be_ return files
parent, else return parent of buffers `default-directory'.\n
When called-intereactively or INSRTP is non-nil insert buffers parent directory.
:NOTE Could also accomplish with:\n
 \(car \(last \(split-string 
               \(directory-file-name (expand-file-name \"./\"\)\) \"/\" t\)\)\)\n
But, not without the checks or a facility for sanity checks in programmatic
situations where `default-directory' of a non-written buffer may not evaluate to
what is expected. This is esp. the case where a calling function(s) has or might
`cd' to some alien path to do some stuff. We don't neccesarily want to blindly
write a buffer assuming that it will wind up in 'the' current directory.
It might not.\n
:SEE-ALSO `mon-buffer-exists-p', `mon-buffer-written-p', `mon-with-file-buffer',
`mon-buffer-name->kill-ring', `mon-get-proc-buffers-directories',
`mon-get-buffers-directories', `mon-split-string-buffer-name',
`mon-split-string-buffer-parent-dir' `mon-split-string-buffer-parent-dir-quick',
`with-current-buffer', `with-temp-file', `with-temp-buffer'.\n►►►"
  (interactive "i\ni\np")
  (let* ((is-written (mon-buffer-written-p))
	 (ret-buf-dir 
          (if is-written
              (if full
                  (directory-file-name 
                   (file-name-directory (buffer-file-name)))
                  (file-name-nondirectory 
                   (directory-file-name 
                    (file-name-directory (buffer-file-name)))))
              (if full
                  (directory-file-name default-directory)
                  (file-name-nondirectory 
                   (directory-file-name default-directory))))))
    (if is-written
	;; (progn 
        ;;   (when (or insrtp intrp)
        ;;     (message "buffer: `%s' parent dir is `%s'."  (buffer-name) ret-buf-dir))
          (if (or insrtp intrp)
              (prog1 
                  (when (or insrtp intrp)
                    (message "buffer: `%s' parent dir is `%s'."  (buffer-name) ret-buf-dir))
                (insert ret-buf-dir))
              ret-buf-dir)
        ;; (prog1
        ;;   (when (or insertp intrp)
        ;;     (message "Buffer: `%s' not written yet, parent of buffer's default-directory is `%s'." 
        ;;              (buffer-name) ret-buf-dir)
          (if (or insrtp intrp)
              (prog1
                  (message 
                   "Buffer: `%s' not written yet, parent of buffer's default-directory is `%s'." 
                   (buffer-name) ret-buf-dir)
                (insert ret-buf-dir))
              ret-buf-dir))))
;;
;;; :TEST-ME (mon-get-buffers-parent-dir)
;;; :TEST-ME (mon-get-buffers-parent-dir t)
;;; :TEST-ME (mon-get-buffers-parent-dir t t)
;;; :TEST-ME (mon-get-buffers-parent-dir nil t)
;;; :TEST-ME (call-interactively 'mon-get-buffers-parent-dir)

;;; ==============================
;;; :CREATED <Timestamp: Friday May 29, 2009 @ 07:26.02 PM - by MON>
(defun mon-truncate-path-for-prompt (&optional intrp)
  "Return a truncated path string of current buffers path.\n
Useful for passing around to helper functions that prompt.\n
:EXAMPLE\n(mon-truncate-path-for-prompt)\n
:SEE-ALSO `mon-reduce-file-name'.\n►►►"
(interactive "p")
  (let* ((trunc-pth (directory-file-name (expand-file-name "./")))
	 (trunc-s ;; :WAS (split-string trunc-pth "/"))
          (save-match-data (split-string trunc-pth "/")))
	 (trunc-l (length trunc-s))
	 (bld-lst))
    (setq bld-lst (cond ((>= trunc-l 3)(last trunc-s 3))
                        ((>= trunc-l 2)(last trunc-s 2))
                        ((>= trunc-l 1)(last trunc-s))))
    (setq bld-lst (mapconcat 'identity bld-lst "/"))
    (if intrp (message "Truncated path: %s" bld-lst) bld-lst)))
;;
;;; :TEST-ME (mon-truncate-path-for-prompt)

;;; ==============================
;;; :CREATED <Timestamp: Saturday May 23, 2009 @ 08:37.50 PM - by MON>
(defun mon-walk-buff-or-dir-path (&optional alt-path rev)
  "Return a rercusively split list of directory paths as strings.\n
When non-nil ALT-PATH is a directory name supplied as a string.
Throws an error if ALT-PATH doesn't exist.
When non-nil REV returns the result in reversed format - 
The REV arg is useful for faster comparison of two trees.
Use to walk up the directory or buffers path.
Default is to split buffer's current directory.\n
:EXAMPLE\n\(mon-walk-buff-or-dir-path\)\n
:SEE-ALSO `mon-get-most-common-path'.\n►►►"
  (interactive)
  (let* ((alt-p (if alt-path
		  (if (file-exists-p (directory-file-name alt-path))
		      (directory-file-name alt-path)
		    (error "path name provided doesn't exist"))
		  nil))
	 (walk-buf-dirs (if alt-p
                            ;; :WAS (split-string alt-p "/" t))
			    (save-match-data (split-string alt-p "/" t))
                            (mon-split-string-buffer-parent-dir)))
	 (walking))
    (setq walking '())
    (while walk-buf-dirs
      (push (mapconcat 'identity walk-buf-dirs "/") 
	    walking)
      (setq walk-buf-dirs (nreverse walk-buf-dirs))
      (pop walk-buf-dirs)
      (setq walk-buf-dirs (nreverse walk-buf-dirs)))
    (if rev (nreverse walking) walking)))
;;
;;; :TEST-ME (mon-walk-buff-or-dir-path) 
;;; :TEST-ME (mon-walk-buff-or-dir-path nil t)
;;; :TEST-ME (mon-walk-buff-or-dir-path (expand-file-name "~/"))
;;; :TEST-ME (mon-walk-buff-or-dir-path (expand-file-name "~/") t)

;;; ==============================
;;; :CREATED <Timestamp: Wednesday May 27, 2009 @ 04:28.57 PM - by MON>
(defun mon-get-most-common-path (path-is path-in)
  "Given two paths return an acending list of the most common parents of two.
Comparison made as a test if PATH-IS (the unknown) has a common parent directory in
PATH-IN (target). 
Take the car of the list returned for the first deepest directory.
Take the last elt of the list returned for least deepest directory.\n
:EXAMPLE\n\(mon-get-most-common-path *ebay-images-bmp-path* *ebay-images-path*\)\n
:SEE-ALSO `mon-walk-buff-or-dir-path'.\n►►►"
  (let ((path-a (mon-walk-buff-or-dir-path path-is t)) ;; Longer - reversed.
	(path-b (mon-walk-buff-or-dir-path path-in t)) ;; Check if is-in - reversed.
	(caught))
    (while (and path-a (not caught))
      (let* ((path-a-hd (car path-a))
	     (look (member path-a-hd path-b)))
	(when look (setq caught look)))
      (setq path-a (cdr path-a)))
    ;; Why are we messaging here?
    (when (not caught)
      (message "No common paths found for:\n%s and\n %s" path-is path-in))
    caught))
;;
;;; :TEST-ME (mon-get-most-common-path *ebay-images-bmp-path* *ebay-images-path*)

;;; =======================
;;; :NOTE This is a derivation of `normal-top-level-add-subdirs-to-load-path'
;;;       there is still some crud left from it. :SEE startup.el
;;; :SEE (URL `http://www.emacswiki.org/emacs/SubdirsToList')
(defun mon-add-subdirs-to-list (my-directory my-list)
  "Add all immediate subdirectories of `my-directory' to `my-list'.
More precisely, this uses only the subdirectories whose names start with
letters or digits; it excludes any subdirectory named `RCS' or `CVS', and any
subdirectory that contains a file named `.nosearch'.\n
:SEE-ALSO `mon-copy-files-in-sub-dirs', `mon-insert-subdirs-in-buffer'
`mon-path', `mon-copy-file-path', `mon-insert-path', 
`mon-get-buffers-directories', `mon-proc-buffers-directories',
`mon-get-proc-buffers-directories'.\n►►►"
  (let ((dirs)
    	(attrs)
    	(pending (list my-directory))
    	(subdirs-inode-list)
	(my-add-subdirs-inode-list))
    (let* ((this-dir (car pending))
    	   (contents (directory-files this-dir))
    	   (default-directory this-dir)
    	   (canonicalized (and (eq system-type 'windows-nt)
    			       (untranslated-canonical-name this-dir))))
      ;; The Windows version doesn't report meaningful inode numbers, so use the
      ;; canonicalized absolute file name of the directory instead.
      (setq attrs (or canonicalized
		      (nthcdr 10 (file-attributes this-dir))))
      (unless (member attrs subdirs-inode-list)
	(setq my-add-subdirs-inode-list
	      (cons attrs subdirs-inode-list))
	(while contents
	  ;; The lower-case variants of RCS and CVS are for DOS/Windows.
	  (unless (member (car contents) '("." ".." "RCS" "CVS" "rcs" "cvs"))
	    (when (and (string-match "\\`[[:alnum:]]" (car contents))
		       ;; Avoid doing a `stat' when it isn't necessary
		       ;; because that can cause trouble when an NFS server
		       ;; is down.
		       (not (string-match "\\.elc?\\'" (car contents)))
		       (file-directory-p (car contents)))
	      (let ((expanded (expand-file-name (car contents))))
		(unless (file-exists-p (expand-file-name ".nosearch"
							 expanded))
		  (setq dirs (nconc dirs (list expanded)))))))
	  (setq contents (cdr contents)))))
    (list my-list)
    (dolist (my-dir dirs)
      (add-to-list my-list my-dir)))
  (eval my-list)) 

;;; ================================================================
;;; :SEE (ULR `http://www.emacswiki.org/emacs/SubdirsToList') - no-author.
;;; :MODIFICATIONS <Timestamp: Tuesday February 17, 2009 @ 05:50.26 PM - by MON>
(defun mon-insert-subdirs-in-buffer (&optional pth-to-l)
  "Insert at point the top-level subdirs found in PTH-TO-L.\n
PTH-TO-L is nil or called-interactively prompt for a path name.
Ignore dirs with .nosearch.\n
:CALLED-BY `mon-add-subdirs-to-list'.\n
:SEE alternative implementation using; `mon-get-buffers-directories', 
`mon-get-proc-buffers-directories', `mon-proc-buffers-directories',
`mon-cln-blank-lines'.\n
:SEE-ALSO `mon-copy-files-in-sub-dirs'.\n►►►"
  (interactive)
  (save-excursion
    (let* ((path-list (if (and pth-to-l)
			  pth-to-l
			(read-directory-name "Gimme a directory path :")))
	   (hold-list ())
	   (get-list (mon-add-subdirs-to-list path-list 'hold-list))
	   (to-print get-list))
      (while to-print
	(progn
	  (newline)
	  (princ (car to-print) (current-buffer)))
	(setq to-print (cdr to-print))))))

;; ==============================
;;; :COURTESY Thierry Volpiatto :HIS thumb-page.el :WAS `tv-serial-rename'
;;; :SEE (URL `http://www.emacswiki.org/emacs/SerialRename')
;;; :CREATED <Timestamp: Sunday April 05, 2009 @ 01:14.27 PM - by MON>
(defun mon-serial-rename (dir ext w-name start)
  "Rename sequentially a set of file(s).\n
Rename W-NAME and EXT in DIR from START number.\n
:SEE-ALSO .\n►►►"
  (interactive "fDir: \nsExt(no dot): \nsName: \nnStart: ")
  (find-file dir)
  (let (ls-dir new-ls-dir n c)
    (setq ls-dir (file-expand-wildcards (format "*.%s" ext) t))
    (setq new-ls-dir nil)
    (setq n 0)
    (while (< n (length ls-dir))
      (if (< start 10)
	  (push (concat dir w-name (format "0%s" start) "." ext) new-ls-dir)
	(push (concat dir w-name (format "%s" start) "." ext) new-ls-dir))
      (setq start (+ start 1))
      (setq n (+ n 1)))
    (setq ls-dir (reverse ls-dir))
    (setq c 0)
    (dolist (i ls-dir)
      (rename-file i (nth c new-ls-dir))
      (setq c (+ c 1)))))
;;
;;; :TEST-ME (mon-serial-rename)

;;; ;;; ==============================
;;; ;;; :COURTESY Ted Zlatanov <tzz@lifelogs.com> :HIS `map-file-lines'
;;; ;;; :SEE Message-ID: <86wsc87o3c.fsf@lifelogs.com>
;;; ;;;      emacs-devel Date: Mon, 02 Feb 2009 11:20:07 -0600
;;; ;;; :NOTE: (getenv "TEMP")
;;; ;;;    (map-file-lines f "/tmp/test" (lambda (line num) (message "%d: %s" line)))
;;; (defun mon-map-file-lines (file func &optional startline count bufsize)
;;;   "Iterate over all the lines of ARG file with ARG func.\n
;;; Read a block of data in one shot processing it linewise.
;;; Intended for use with _BIG_ files.\n
;;; :EXAMPLE\n
;;; \(mon-map-file-lines \"/tmp/test\" 
;;;  \(lambda \(line num\) \(message \"%d: %s\" line\)\)\)\n
;;; :SEE-ALSO .\n►►►"
;;;   (let ((filepos 0)
;;;         (linenum 0)
;;;         (bufsize (or bufsize (* 128 1024))))
;;;     (with-temp-buffer
;;;       (while
;;;           (let* ((inserted (insert-file-contents 
;;;                             file nil
;;;                             filepos (+ filepos bufsize)
;;;                             t))
;;;                  (numlines (count-lines (point-min) (point-max)))
;;;                  (read (nth 1 inserted))
;;;                  (done (< 1 read))
;;;                  (result) 
;;;                  (line-end))
;;;             (dotimes (n (count-lines (point-min) (point-max)))
;;; 	      (goto-char (point-min))
;;; 	      (setq line-end (line-end-position))
;;;               (setq result 
;;;                     (if (and startline (< linenum startline))
;;;                         ()
;;;                         (if (and count (>= (- linenum startline) count))
;;;                             (return)
;;;                             (funcall func 
;;;                                      (buffer-substring 
;;;                                       (line-beginning-position)
;;;                                       line-end)
;;;                                      linenum))))
;;;               (set done (and done result))
;;; 	      (incf filepos line-end)
;;; 	      (forward-line)
;;; 	      (incf linenum))
;;;             done)))
;;;     linenum))


;;; (mon-map-file-lines 
;;;  "c:/home/sp/bin/Emacs/emacs-23-1-src/src/lread.c" 
;;;  (lambda (line num) (message "%d: %s" num line)))

;;; :TEST-ME (mon-map-file-lines "/tmp/test" (lambda (line num) (message "%d: %s" num line)))
;;; :TEST-ME (mon-map-file-lines "/tmp/test" (lambda (line num) (message "%d: %s" num line)) 100)
;;; :TEST-ME (mon-map-file-lines "/tmp/test"(lambda (line num) (message "%d: %s" num line)) 100 10)

;;; ==============================
;;; :CREATED <Timestamp: Monday February 09, 2009 @ 09:35.31 PM - by MON>
(defun naf-explorer-artist (prefix)
  "Open an w32 explorer window in alphabetic NAF drive Artists Folder.\n
PREFIX is a one letter string A-Z.
When called-interactively, prompt for an alphabetic directory PREFIX.
Default path held by global var: `*artist-naf-path*'.\n
:SEE-ALSO `naf-explorer-brand',`naf-dired-artist-letter',
`naf-dired-brand-letter', `mon-open-explorer'.\nUsed in `naf-mode'.\n►►►"
  (interactive "p")
  (when (and win32p)
    (let ((dl (format "%s-Artists names"
                       (if (numberp prefix)
		       (upcase (read-string "Alphabetic Artists Directory:"))
                       (upcase prefix))))
	   (naf-alph-path))             ;; w32 explorer needs this backslashed.
      (setq naf-alph-path (concat *artist-naf-path* "/" dl))
      (setq naf-alph-path (subst-char-in-string 47 92 naf-alph-path))
      (w32-shell-execute  "open" "explorer" (concat "/e, " naf-alph-path)))))
;;
;;; :TEST-ME (naf-explorer-artist "b")

;;; ==============================
;;; :CREATED <Timestamp: Monday February 09, 2009 @ 09:35.31 PM - by MON>
(defun naf-explorer-brand (prefix)
 "Open a w32 explorer window in alphabetic NAF drive Brand Folder.\n
PREFIX is a one letter string A-Z.
When called-interactively, prompt for an alphabetic naf brand directory.
Default path held in var: `*brand-naf-path*'.\n
:SEE-ALSO `naf-explorer-artist',`naf-dired-artist-letter',
`naf-dired-brand-letter', `naf-dired-image-dir'`mon-open-explorer'.\n
:USED-IN `naf-mode'.\n►►►"
  (interactive "p")
  (when (and win32p)
    (let* ((dl (format "%s-Brand-NAFs"
                       (if (numberp prefix)
                           (upcase (read-string "Alphabetic Artists Directory:"))
                         (upcase prefix))))
	   (naf-alph-path))             ;;win32 explorer needs this backslashed
      (setq naf-alph-path (concat *brand-naf-path* "/" dl))
      (setq naf-alph-path (subst-char-in-string 47 92 naf-alph-path))
      (w32-shell-execute  "open" "explorer" (concat "/e, " naf-alph-path)))))
;;
;;; :TEST-ME (naf-explorer-brand "b")

;;; ==============================
;;; :TODO Figure out how to take a second argument that heuristically
;;;       puts point at an appropriate location in the returned dired buffer.
;;; :CREATED <Timestamp: Monday February 09, 2009 @ 09:35.31 PM - by MON>
(defun naf-dired-artist-letter (prefix)
  "Dired visit the alphabetic Artist naf-directory by letter PREFIX. 
PREFIX is a one letter string A-Z. 
When Called interactively prompts for:\"Alphabetic Brand Directory :\" 
concatatenating \"LETTER-Artist names/\" to the default naf path.
Default naf path held by the var: `*artist-naf-path*'.\n
:EXAMPLE\n(naf-dired-artist-letter \"b\")\n
:SEE-ALSO `naf-explorer-artist',`naf-explorer-brand', `naf-dired-brand-letter',
`naf-dired-image-dir', `mon-open-explorer'.\nUsed in `naf-mode'.\n►►►"
  (interactive "p")
  (let* ((dl (format "/%s-Artists names/"
                       (if (numberp prefix)
                           (upcase (read-string "Alphabetic Artists Directory :"))
                         (upcase prefix))))
	 (naf-alph-path (concat *artist-naf-path* dl))
	 (default-directory naf-alph-path))
    (dired-other-window naf-alph-path)))
;;
;;; :TEST-ME (naf-dired-artist-letter "b")

;;; ==============================
;;; :CREATED <Timestamp: Monday February 09, 2009 @ 09:35.31 PM - by MON>
(defun naf-dired-brand-letter (prefix); &optional intrp)
  "Dired the alphabetic Brand naf-directory by letter PREFIX. 
PREFIX is a one letter string A-Z.
When Called interactively prompts for:\"Alphabetic Brand Directory :\" 
concatatenating \"LETTER-Brand-NAFS/\" to the default naf path.
Default naf path held by the var: `*brand-naf-path*'.\n
:EXAMPLE\n(naf-dired-brand-letter \"b\")\n
:SEE-ALSO `naf-dired-artist-letter', `naf-explorer-artist',
`naf-explorer-brand', `naf-dired-image-dir', `mon-open-explorer'.\n
:USED-IN `naf-mode'.\n►►►"
  (interactive "p")
  (let* ((dl (format "/%s-Brand-NAFs/"
                       (if (numberp prefix)
                           (upcase (read-string "Alphabetic Brand Directory :"))
                         (upcase prefix))))
	 (naf-alph-path (concat *brand-naf-path* dl))
	 (default-directory naf-alph-path))
    (dired-other-window naf-alph-path)))
;;
;;; :TEST-ME (naf-dired-brand-letter  "b")

;;; ==============================
;;; :CREATED <Timestamp: Thursday June 25, 2009 @ 06:03.54 PM - by MON>
(defun naf-dired-image-dir (pth-nm &optional intrp)
  "Dired to an image directory.
PTH-NM is an image directory udner one of the following strings:
\"nefs-archived\", \"nefs-working\", \"ebay-bmp\", \"ebay-jpg\".
These are bound as alist keys in the vars: `*nef-scan-nefs-path*', 
`*nef-scan-nef2-path*',`*ebay-images-bmp-path*', `*ebay-images-jpg-path*'
When called-interactively complete the key to dired to the directory val.\n
:EXAMPLE\n(naf-dired-image-dir \"ebay-bmp\")\n
:SEE-ALSO `naf-dired-artist-letter', `naf-explorer-artist', 
`naf-explorer-brand', `mon-open-explorer', `mon-dired-nef-dir',
`mon-nef-dir-big', `*nefs_photos_nefs-alist*'.\n►►►"
(interactive "p\nP")
(eval-when-compile (require 'mon-cl-compat))
(let* ((img-pths (cl::pairlis  
                  '("nefs-archived" "nefs-working" "ebay-bmp"  "ebay-jpg")               
                  `(,*nef-scan-nefs-path* ,*nef-scan-nef2-path* 
                                          ,*ebay-images-bmp-path* ,*ebay-images-jpg-path*)))
       (in-d (cond (intrp (completing-read "which path :" img-pths nil t))
                   ((assoc pth-nm img-pths) pth-nm)
                   ((not (assoc pth-nm img-pths))
                    (completing-read "which path :" img-pths nil t))))
      (to-d (cdr (assoc in-d img-pths))))
  ;; Complete on each directories of nefs-archived with a cached alist.
  (if (string= in-d "nefs-archived")
      (let ((nef-alist (mon-nef-dir-big *nefs_photos_nefs-alist*))
            (get-nef-dir))
        (setq get-nef-dir
              (cadr (assoc-string 
                     (completing-read "Which nef directory (tab-completes):" nef-alist) 
                     nef-alist)))
        (dired (concat *nef-scan-nefs-path* "/" get-nef-dir)))
    (dired (cdr (assoc in-d  img-pths))))))
;;
;;; :TEST-ME (naf-dired-image-dir "nefs-archived")

;;; ==============================
;;; :DEPRECATE use `mon-copy-file-path'
(defun mon-path (&optional intrp)
  ":DEPRECATE use `mon-copy-file-path'.\n
Returns the current file path as a message.
Unlike `mon-copy-file-path' path doesn't copy to file's path kill ring.\n
:SEE-ALSO `mon-copy-file-path', `mon-insert-path', `mon-add-subdirs-to-list'.\n►►►" 
  (interactive "p")
(if intrp
  (message "%s" buffer-file-name)
  (buffer-file-name)))
;;
(make-obsolete 'mon-path 'mon-copy-file-path)
;;
;;; :TEST-ME (mon-path)
;;; :TEST-ME (mon-path t)
;;; :TEST-ME (call-interactively 'mon-path)

;;; ==============================
;;; :MODIFICATIONS <Timestamp: Tuesday July 21, 2009 @ 05:19.11 PM - by MON>
(defun mon-copy-file-path (&optional insertp intrp)
  "Copy current buffer's file path to kill-ring. Return path value as message.\n
When INSERTP is non-nil or called with prefix arg insert path at point.\n
:SEE-ALSO `mon-insert-path', `mon-path', `mon-add-subdirs-to-list',
`mon-copy-files-in-sub-dirs'.\n►►►"
  (interactive "P\np")
  (let (scfp)
    (if (and (mon-buffer-written-p)
              (file-readable-p (buffer-file-name)))
        (progn
          (setq scfp (buffer-file-name))
          (kill-new (buffer-file-name))
          (when (or intrp (not insertp))
            (progn
              (message "The-path-is: %s" scfp)
              (sit-for 1))))
        (progn
          (setq scfp (buffer-name))
          (kill-new (format 
                     "#P/not-written/not-readable/not-exisistent/buffer-name-is/%s" 
                     (buffer-name)))
          (cond ((or intrp (not insertp))
                 (message "The path doesn't exist buffer-name is: %s" (buffer-name))
                 (sit-for 1))
                ((and insertp (not intrp))
                 (setq scfp 
                       (format 
                        "#P/not-written/not-readable/not-exisistent/buffer-name-is/%s" 
                        (buffer-name)))))))
    (if insertp (insert scfp))))
;;
;;; :TEST-ME (mon-copy-file-path)
;;; :TEST-ME (mon-copy-file-path t )
;;; :TEST-ME (mon-copy-file-path t t)
;;; :TEST-ME (call-interactively 'mon-copy-file-path)

;;; ==============================
;;; :MODIFICATIONS: <Timestamp: Tuesday July 21, 2009 @ 05:21.05 PM - by MON>
 (defun mon-insert-path ()
   ":DEPRECATE use `mon-copy-file-path'.\nInsert current file's path at point.\n
:SEE-ALSO `mon-copy-file-path', `mon-path', `mon-add-subdirs-to-list',
`mon-copy-files-in-sub-dirs'.\n►►►"
   (interactive)
   (mon-copy-file-path t))
;;
(make-obsolete 'mon-insert-path 'mon-copy-file-path)
;;
;;; :TEST-ME (mon-insert-file-path)

;;; ==============================
;;; :CREATED <Timestamp: Friday May 08, 2009 @ 12:12.46 PM - by MON>
(defun mon-get-buffers-directories (&optional opt-dir)
  "Return buffer list for buffers' directories sub-dirs.\n
:CALLED-BY `mon-proc-buffers-directories', `mon-get-proc-buffers-directories',
`mon-cln-blank-lines'.\nCALLS-VARIABLE: `*nef-scan-path*'.\n
:NOTE For alternative implementation of same,
:SEE `mon-insert-subdirs-in-buffer', `mon-add-subdirs-to-list'.\n
:SEE-ALSO .\n►►►"
  (let ((get-dirs)
	(df-dir (file-name-as-directory (file-name-directory default-directory)))
	(this-dir))
    (cond ((and (not (buffer-file-name)) (not opt-dir))
	   (if (yes-or-no-p "Supply an alternate directory path: ")
	       (setq this-dir (file-name-directory 
			       (file-name-as-directory
				(read-directory-name "List subdirs of dir :" *nef-scan-path* df-dir nil t))))
	    (setq this-dir df-dir)))
	   ;; (error (format 
           ;;         "The `%s' not associated with a directory or called from unsaved buffer"
           ;;         (buffer-name))))
	  (opt-dir 
	   (if (file-exists-p (file-name-directory (file-name-as-directory opt-dir)))
	       (setq this-dir (file-name-directory  (file-name-as-directory opt-dir)))
	     (error "Directory doesn't exist, is not readable, or is a file")))
	  ((and (not opt-dir) (buffer-file-name))
	   (setq this-dir (file-name-as-directory (file-name-directory buffer-file-name)))))
    (setq get-dirs
          (with-temp-buffer
	    (insert-directory this-dir "-gloRBd --dired a" nil t)
	    (buffer-string)))
    get-dirs))

;;; ==============================
;;; :CREATED <Timestamp: Thursday May 07, 2009 @ 08:24.16 PM - by MON>
(defun mon-proc-buffers-directories (&optional opt-dir) 
  "Return directory list of buffers' directories sub-directories.
:CALLED-BY `mon-get-buffers-directories', `mon-get-proc-buffers-directories',
`mon-cln-blank-lines'.\n
:NOTE for alternative implementation of same,
:SEE `mon-insert-subdirs-in-buffer', `mon-add-subdirs-to-list'.\n
:SEE-ALSO `mon-copy-files-in-sub-dirs'.\n►►►"
  (let ((proc-result)
	(to-proc))
    (setq to-proc (mon-get-buffers-directories opt-dir)) 
    (setq proc-result
	  (with-temp-buffer
	    (insert to-proc)
	    (let* ((mrkr-start)
		   (mrkr-end)
		   (mrkr-start (make-marker))
		   (mrkr-end (make-marker)))
	      ;; Need to check here if there are actually any directories in this path.
	      ;; "^drwxrwxrwx  1 Everyone Everyone        0 05-15 16:14 "\\([^\.+]\\)
	      (goto-char (point-min))
	      (set-marker mrkr-start (progn (search-forward "total used in directory") (point-at-bol)))
	      (set-marker mrkr-end (point-max))
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
	    (goto-char (point-min))
	    (while (search-forward-regexp "^$" nil t)
	      (replace-match "#"))
	    (goto-char (point-min))
	    (while (search-forward-regexp "^#$" (point-max) t)
	      (cond ((and (eolp) (not (bobp))
			  (not (not (char-before)))
			  (not (not (char-before (1- (point))))))
		     (if (and (= (char-before) 35)
			      (= (line-beginning-position) (1- (point)))
			      (= (char-before (1- (point))) 10)
			      (not (= (char-before (- (point) 2)) 10)))
			 (delete-char -2)))))
	    (goto-char (point-min))
	    (progn
	      (search-forward-regexp "^#$" nil t)
	      (forward-char 1)
	      (delete-char -2))
	    (buffer-string)))
    proc-result)) ;;(insert proc-result)))
;;
;;;(progn (fmakunbound 'mon-proc-buffers-directories)
;;;       (unintern 'mon-proc-buffers-directories) )

;;; ==============================
;;; :NOTE Can `subst-char-in-string' be used here instead?
;;;       Did this function get screwed up at :CHANGESET 533:5c6419be867a?
;;;       I think it is fixed, but...
;;; :CREATED <Timestamp: Thursday May 07, 2009 @ 09:40.22 PM - by MON>
(defun mon-get-proc-buffers-directories (&optional intrp opt-dir) 
  "Return dir list of buffer files directories.\n
Directory list acquired with: `mon-get-buffers-directories' and
`mon-proc-buffers-directories'. List cleaned with `mon-cln-blank-lines'.\n
:NOTE For alternative implementation of same,
:SEE `mon-insert-subdirs-in-buffer', `mon-add-subdirs-to-list'.\n
:SEE-ALSO `mon-copy-files-in-sub-dirs', `mon-buffer-name->kill-ring'
`mon-buffer-exists-p', `mon-buffer-written-p', `mon-with-file-buffer'.\n►►►"
  (interactive "p")
  (let ((result) (proc))
    (setq proc (mon-proc-buffers-directories opt-dir))
    (setq result (with-temp-buffer
		   (insert proc)
		   (mon-cln-blank-lines (point-min) (point-max))
		   (let* ((file-n (bounds-of-thing-at-point 'filename))
			  (bound-s (car file-n))
			  (bound-e (cdr file-n))
			  (file-ns (buffer-substring bound-s bound-e))
			  (file-list) (f-name))
                     ;; (save-excursion
		     (goto-char (point-max))
		     (while (mon-spacep) 
		       (delete-char -1))
		     (goto-char (point-min))
		     (while (mon-spacep nil t)
		       (delete-char 1))
		     (goto-char (point-min))
		     (mon-cln-spc-tab-at-eol-in-region (point-min) (point-max))
		     (goto-char (point-min))
                     ;; :TODO Seperate to dedicated-function.
		     (while (search-forward-regexp " " (point-max) t) 
		       (if  (not (= (line-end-position)(car (cddddr (mon-line-test-content 'whitespace t)))))
                            (progn    
                              (beginning-of-line)
                              (search-forward-regexp 
                               "\\(\\(\\([[:alnum:]]\\)\\([[:space:]]\\)\\([[:alnum:]]\\)\\)\\{1,1\\}\\)" nil t)
                              (replace-match "\\3_#_~_#_\\5"))))
		     (goto-char (point-min))
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
	  (prin1 result (current-buffer)))
        result)))
;;
;;; :TEST-ME (progn (newline)(prin1 (mon-get-proc-buffers-directories) (current-buffer)))

;;; ==============================
;;; :CREATED <Timestamp: Thursday May 21, 2009 @ 08:06.42 PM - by MON>
(defun mon-build-dir-list (dir &optional not-concat-path)
  "Return a _list_ of directories in DIR.\n
When non-nil NOT-CONCAT-PATH returns a list _without_ the leading path.\n
:SEE-ALSO `mon-try-comp-dir',`mon-complete-hashed-dir',`mon-hash-img-dir'.\n►►►"
(save-excursion
    (save-window-excursion
      (let ((temp-string)
	    (curr-buff (get-buffer (current-buffer)))
	    (in-dir dir)
	    (rtn-dir))
	(setq temp-string    
	      (with-temp-buffer
		(let ((this-buff)
		      (that-buff)
		      (ss))
		  (setq this-buff (get-buffer (current-buffer)))
		  (list-directory dir t)
		  (setq that-buff (get-buffer "*Directory*"))
		  (set-buffer that-buff)
		  (setq ss (buffer-substring-no-properties (point-min) (point-max)))
		  (set-buffer this-buff)
		  (kill-buffer that-buff)
		  (insert ss)
		  (goto-char (point-min))
		  (keep-lines "^d.*[0-9][0-9]:[0-9][0-9] .*$")
		  (goto-char (point-min))
		  (while (search-forward-regexp 
                          "\\(\\(^d.*[0-9][0-9]:[0-9][0-9][[:space:]]\\)\\(.*$\\)\\)" nil t)
		    (replace-match "\\3" ))
		  (mon-cln-trail-whitespace)
		  (goto-char (point-min))
		  (while (search-forward-regexp "^\\(.*\\)$" nil t)
		    (if (and (mon-line-bol-is-eol) (not (eobp)))
			(delete-char 1)
		      (replace-match "\\1|")))
		  (while (search-backward-regexp "^\|$" nil t)
		    (if (= (char-after) 124)
		      (delete-char 1)))
		  (goto-char (point-min))
		  (mon-delete-back-up-list (point-min) (point-max))
		  (buffer-substring-no-properties (point-min) (point-max)))))
	(set-buffer curr-buff)
	(setq rtn-dir
	      ;; :WAS (split-string temp-string "| "))
              (save-match-data (split-string temp-string "| ")))
	(setq rtn-dir (delete "" rtn-dir))
	(if (not not-concat-path)
	    (setq rtn-dir
		  (let ((map-dir rtn-dir)
			(conc-dir (concat in-dir "/")))
		    (mapcar '(lambda (x) (concat conc-dir x)) map-dir)))
	  rtn-dir)
	;; (prin1 rtn-dir (current-buffer))
	rtn-dir))))
;;
;;; :TEST-ME (mon-build-dir-list mon-emacs-root)
;;; :TEST-ME (mon-build-dir-list mon-emacs-root t)

;;; ==============================
;;; :CREATED <Timestamp: Saturday June 27, 2009 @ 04:01.56 PM - by MON>
(defun mon-update-nef-photos-alist ()
  "Refresh the alist contents of variable `*nefs_photos_nefs-alist*'.\n
Assumes directories of `*nef-scan-nefs-path*' formatted as one of following:\n
NNN_Name-of-folder_(NNNN-NNNN)\n1...2*.............3..........\n
NNN_(NNNN-NNNN)\n1...3..........\n
NNN_Name-of-folder_(NNNN-NNNN, NNNN,NNNN,NNNN)
1...2*.............3.........................\n
NNN_(NNNN-NNNN, NNNN)\n1...3................\n
1) [Image-directory-number
    :Integer {3}
    :Underscore: \"_\" char: 95]\n
2) [Folder-name-string                       *Optional
    :Namestring ::= [A-z0-9,]
    :Namestring-delimiter ::= \"-\" char: 45]\n
3) [Image-Range
    :Underscore \"_\" char: 95
    :Opening-Paren ::= char: 40
    :Integer ::= {1,5}
    :Range-delimter ::= delimit with \"-\" char: 45
    :Multi-range ::= delimit second with \", \"  char: 44 char 32
    :Subsequent-multi-ranges ::=  \",=\" char: 44
    :Integer ::= {2,5}]\n
:EXAMPLE If directory Contains the two folders:
\"001_Lesson-Humming-Brds_\(1-21\)\"\n\"242_\(12390-12400\)\"\n
Return an alist as:
\(\(\"001_Lesson-Humming-Birds_\(1-21\)\" \"001\" \"Lesson-Humming-Birds\" \"\(1-21\)\"\)
\(\"242_\(12390-12400\)\" \"242\" \"\(12390-12400\)\)\"\)\)\n
:CALLED-BY `mon-dired-nef-dir', `mon-nef-dir-big'.
:NOTE Evaluated at loadtime with `mon-bind-nefs-photos-at-loadtime'.
:SEE-ALSO .\n►►►"
  (let (cf-efct)
    (setq cf-efct
          (mapcar #'(lambda (x) 
                      (let (split split-name)
                        (setq split ;; :WAS (split-string x "_" ))
                              (save-match-data (split-string x "_" )))
                        (setq split-name
                              (cond ((= (length split) 3) 
                                     `(,x
                                       ,(car split)
                                       ,(subst-char-in-string 45 32 (cadr split))
                                       ,(caddr split)))
                                    ((= (length split) 2)
                                     `(,x ,(car split) ,(cadr split))) 
                                    ;; We should never see this.
                                    (t `(,x ,split))))
                        split-name))
                  (directory-files *nef-scan-nefs-path* nil "^\\([0-9]\\{2,3\\}_.*\\)")))))
;; (let ((new-alist (mon-update-nef-photos-alist)))
;;   (setq *nefs_photos_nefs-alist* new-alist))
;;
;;;(progn (makunbound '*nefs_photos_nefs-alist*) 
;;;       (unintern '*nefs_photos_nefs-alist*) )

;;; ==============================
;;; :CREATED <Timestamp: #{2010-02-04T16:33:33-05:00Z}#{10054} - by MON KEY>
(defun mon-bind-nefs-photos-at-loadtime ()
  "Build `*nefs_photos_nefs-alist*' with `mon-update-nef-photos-alist' at loadtime.\n
:SEE-ALSO `mon-bind-cifs-vars-at-loadtime', `mon-set-register-tags-loadtime',
`mon-bind-iptables-vars-at-loadtime', `mon-CL-cln-colon-swap'.\n►►►"
  ;; :NOTE __DON'T SNARF IF ON REMOTE MACHINES!!__
  (when (or IS-MON-P-W32 IS-BUG-P)
    (unless (bound-and-true-p *nefs_photos_nefs-alist*))
    (setq *nefs_photos_nefs-alist* (mon-update-nef-photos-alist))))

;;; ==============================
;;; :CREATED <Timestamp: Saturday June 27, 2009 @ 04:27.24 PM - by MON>
(defun mon-nef-dir-ranges (folder-alist)
  "Return FOLDER-ALIST folder ranges as two string alist ranges in head position.\n
Parens are stripped from ranges.\n
:EXAMPLE\n\n\(mon-nef-dir-ranges  *nefs_photos_nefs-alist*\)\n
:SEE-ALSO `mon-dired-nef-dir', `mon-nef-dir-big', `mon-nef-dir-converge',
`mon-nef-dir-keep-3',`mon-nef-dir-fldr',`mon-nef-dir-conc-ranges',
`mon-nef-dir-conc-dups',`mon-nef-dir-find-dups',`mon-nef-dir-rmv-empt',
`*nefs_photos_nefs-alist*', `*nef-scan-nefs-path*'.\n►►►"
  (let (range>)
    (setq range> (mapcar #'(lambda (x) 
                             (let* ((this-x (assoc (car x) folder-alist))
                                    (len (length x))
                                    (pth (car this-x))
                                    (range (cond ((= len 3)(caddr this-x))
                                                 ((= len 4)(cadddr this-x)))))
                               (setq range (replace-regexp-in-string "(" "" range)
                                     range (replace-regexp-in-string ")" "" range))
                               `(,range ,pth)))
                         folder-alist))
    range>))
;;
;;; :TEST-ME (mon-nef-dir-ranges  *nefs_photos_nefs-alist*)

;;; ==============================
;;; :CREATED <Timestamp: Saturday June 27, 2009 @ 04:27.24 PM - by MON>
(defun mon-nef-dir-fldr (folder-alist)
  "Return FOLDER-ALIST as two string alist. 
Return with the directory name in head position.\n
:EXAMPLE\n\(mon-nef-dir-fldr *nefs_photos_nefs-alist*\)\n
:SEE-ALSO `mon-dired-nef-dir', `mon-nef-dir-big', `mon-nef-dir-converge',
`mon-nef-dir-keep-3',`mon-nef-dir-conc-ranges',`mon-nef-dir-ranges',
`mon-nef-dir-conc-dups',`mon-nef-dir-find-dups', `mon-nef-dir-rmv-empt', 
`*nefs_photos_nefs-alist*', `*nef-scan-nefs-path*'.\n►►►"
  (let (folder>)
    (setq folder> 
          (mapcar #'(lambda (x) 
                      (let* ((this-x (assoc (car x) folder-alist))
                             (pth (car this-x))
                             (fld-num (cadr this-x)))
                        `(,fld-num ,pth)))
                  folder-alist))
    folder>))
;;    
;;; :TEST-ME (mon-nef-dir-fldr *nefs_photos_nefs-alist*)

;;; ==============================
;;; :CREATED <Timestamp: Saturday June 27, 2009 @ 04:27.24 PM - by MON>
(defun mon-nef-dir-rmv-empt (folder-alist)
  "Return FOLDER-ALIST folder names as string as two string alist.\n
Return with empty `nil' folders removed, e.g. folders formatted as:\n
NNN_(NNNN-NNNN)\n1...3..........\n
This is required before we can filter out duplicate folders named with
`mon-nef-dir-find-dups' otherwise nil entries will appear as well.\n
:SEE-ALSO `mon-dired-nef-dir', `mon-nef-dir-big', `mon-nef-dir-converge',
`mon-nef-dir-keep-3',`mon-nef-dir-fldr',`mon-nef-dir-conc-ranges',
`mon-nef-dir-ranges',`mon-nef-dir-conc-dups', `*nefs_photos_nefs-alist*',
`*nef-scan-nefs-path*'.\n►►►"
  (let (freename>1)
    (setq freename>1
          (mapcar #'(lambda (x) 
                      (let* ((this-x (assoc (car x) folder-alist))
                             (len (length x))
                             (pth (car this-x))
                             (free (third this-x)))
                        (when (= len 4)
                          `(,free ,pth))))
                  folder-alist)) 
    (setq freename>1 (delq '() freename>1))))
;;
;;; :TEST-ME (mon-nef-dir-rmv-empt *nefs_photos_nefs-alist*)

;;; ==============================
;;; :CREATED <Timestamp: Saturday June 27, 2009 @ 04:27.24 PM - by MON>
(defun mon-nef-dir-find-dups (folder-alist)
  "Find duplicated folder names in the alist generated with 
`mon-nef-dir-rmv-empt' and rotate tail-position to head-position.\n
:EXAMPLE\n\(mon-nef-dir-find-dups *nefs_photos_nefs-alist*\)\n
:SEE-ALSO `mon-dired-nef-dir', `mon-nef-dir-big', `mon-nef-dir-converge',
`mon-nef-dir-keep-3',`mon-nef-dir-fldr',`mon-nef-dir-conc-ranges',
`mon-nef-dir-ranges',`mon-nef-dir-conc-dups',`*nefs_photos_nefs-alist*',
 `*nef-scan-nefs-path*'.\n►►►"
  (let (comp freename>2) 
    (setq comp (mon-nef-dir-rmv-empt folder-alist))
    ;; :TODO fix the `mapcar's called for effect
    ;; :WAS 
    (mapc #'(lambda (free)
              (let* ((free-hd (car free))
                     (free-tl (cadr free))
                     (match (assoc free-hd comp)))
                (when (and match (not (string= free-tl (cadr match))))
                  (when (not (member match freename>2))
                    (setq freename>2 (cons match freename>2)))
                  (when (not (member free freename>2))
                    (setq freename>2 (cons free freename>2))
                    (setq comp (delq match comp))))))
          comp)
    ;; :WAS (setq freename>2 (mapcar #'(lambda (x) (nreverse x))  freename>2))))
    (setq comp nil)
    (mapc #'(lambda (x) (push (nreverse x) comp)) freename>2)
    (setq comp (nreverse comp))))
;;
;;; :TEST-ME (mon-nef-dir-find-dups *nefs_photos_nefs-alist*)

;;; ==============================
;;; :CREATED <Timestamp: Saturday June 27, 2009 @ 04:27.24 PM - by MON>
(defun mon-nef-dir-conc-dups (folder-alist) ;*nefs_photos_nefs-alist*
  "Concat directory name identifiers (integer) onto duplicate directory names in
alist generated with `mon-nef-dir-find-dups'. Each folder with a namestring 
matching an existing namestring needs a unique name so build it.
:EXAMPLE\n\(mon-nef-dir-conc-dups *nefs_photos_nefs-alist*\)\n
:SEE-ALSO `mon-dired-nef-dir', `mon-nef-dir-big', `mon-nef-dir-converge',
`mon-nef-dir-keep-3',`mon-nef-dir-fldr',`mon-nef-dir-conc-ranges',
`mon-nef-dir-ranges',`mon-nef-dir-rmv-empt',`*nefs_photos_nefs-alist*',
`*nef-scan-nefs-path*'.\n►►►"
  (let ((freename>4 (mon-nef-dir-find-dups folder-alist))
        (fldrname> ;; Bind mon-nef-dir-fldr in wrapping defun.
         (mapcar #'(lambda (x) (nreverse x)) (mon-nef-dir-fldr folder-alist)))
        freename>3)
    ;; :TODO fix the `mapcar's called for effect
    ;; :WAS (mapcar #'(lambda (x) 
    (mapc #'(lambda (x) 
              (let ((mk-new (assoc (car x) fldrname>))
                    (new))
                (when mk-new 
                  (setq new  `(,(car mk-new) 
                                ,(concat (cadr x) " |In Folder-> " (cadr mk-new))))
                  (delq (assoc (car x) freename>3) freename>3)
                  (setq freename>3 (cons new freename>3)))))
          freename>4)
    freename>3))
;;
;;; :TEST-ME (mon-nef-dir-conc-dups *nefs_photos_nefs-alist*)

;;; ==============================
;;; :CREATED <Timestamp: Saturday June 27, 2009 @ 04:27.24 PM - by MON>
(defun mon-nef-dir-converge (folder-alist) 
  "Return FOLDER-ALIST with renamed unambiguated duplicate directory names.\n
Directory names of FOLDER-ALIST are folded back into the surronding alist.\n
:SEE-ALSO `mon-dired-nef-dir', `mon-nef-dir-big', `mon-nef-dir-keep-3',
`mon-nef-dir-fldr',`mon-nef-dir-conc-ranges', `mon-nef-dir-ranges',
`mon-nef-dir-conc-dups',`mon-nef-dir-find-dups', `mon-nef-dir-rmv-empt',
`*nefs_photos_nefs-alist*', `*nef-scan-nefs-path*'.\n►►►"
  (let ((big-dupd (mapcar #'(lambda (x) 
                              (nreverse x))
                          (mon-nef-dir-rmv-empt folder-alist)))
        (dups-only (mon-nef-dir-conc-dups folder-alist))
        (no-dups))
    ;; :TODO fix the `mapcar's called for effect
    ;; :WAS (mapcar #'(lambda (x)
    (mapc #'(lambda (x)
              (let* ((is-dup (car x))
                     (old-dup (assoc is-dup big-dupd)))
                (when old-dup 
                  (setq big-dupd (delq old-dup big-dupd))
                  (setq big-dupd (cons x big-dupd)))))
            dups-only)
    (setq no-dups (sort big-dupd #'(lambda (x y) (string< (car x) (car y)))))
    (setq no-dups 
          (sort 
           (setq no-dups 
                 (mapcar #'(lambda (x) (nreverse x))  no-dups))
           #'(lambda (x y) (string-lessp (car x) (car y)))))))

;;
;;; :TEST-ME (mon-nef-dir-converge *nefs_photos_nefs-alist*)

;;; ==============================
;;; :CREATED <Timestamp: Saturday June 27, 2009 @ 04:27.24 PM - by MON>
(defun mon-nef-dir-conc-ranges (folder-alist) ;*nefs_photos_nefs-alist*
  "Return FOLDER-ALIST as two string alist with the directory name in head position.
followed by its range.\n
:SEE-ALSO; `mon-dired-nef-dir', `mon-nef-dir-big', `mon-nef-dir-converge',
`mon-nef-dir-keep-3',`mon-nef-dir-fldr', `mon-nef-dir-ranges',
`mon-nef-dir-conc-dups',`mon-nef-dir-find-dups', `mon-nef-dir-rmv-empt',
`*nefs_photos_nefs-alist*', `*nef-scan-nefs-path*'.\n►►►"
  (let ((not-empt (mapcar #'(lambda (x) (nreverse x)) (mon-nef-dir-rmv-empt folder-alist)))
        (ranges (mapcar #'(lambda (x) (nreverse x)) (mon-nef-dir-ranges  folder-alist)))
        (rangename>))
    ;; :FIXME use mapc, dolist etc.
    ;; :WAS (mapcar #'(lambda (x)
    (mapc #'(lambda (x)
               (let ((mk-rngnm (assoc (car x) not-empt))
                     (new))
                 (when mk-rngnm 
                   (setq new `(,(concat  
                                 (cadr mk-rngnm) 
                                 " |In-Range-> " 
                                 (cadr x)) 
                               ,(car mk-rngnm)))
                   (setq rangename> (cons new rangename>)))))
             ranges)
    rangename>))
;;
;;; :TEST-ME (mon-nef-dir-conc-ranges *nefs_photos_nefs-alist*)

;;; ==============================
;;; :CREATED <Timestamp: Saturday June 27, 2009 @ 04:27.24 PM - by MON>
(defun mon-nef-dir-keep-3 (folder-alist)
  "Return FOLDER-ALIST folder names as string as two string alist.\n
Return FOLDER-ALIST names identified as two elt 'empty' `nil' directories,
e.g. those formatted as:\nNNN_(NNNN-NNNN)\n1...3..........\n
These were removed from surrounding alist by `mon-nef-dir-rmv-empt' for use by
`mon-nef-dir-find-dups'. We need them back, so get them.\n
:EXAMPLE\n\(mon-nef-dir-keep-3 *nefs_photos_nefs-alist*\)\n
:SEE-ALSO `mon-dired-nef-dir', `mon-nef-dir-big', `mon-nef-dir-converge',
`mon-nef-dir-fldr',`mon-nef-dir-conc-ranges',`mon-nef-dir-ranges',
`mon-nef-dir-conc-dups',`mon-nef-dir-find-dups',`mon-nef-dir-rmv-empt',
`*nefs_photos_nefs-alist*', `*nef-scan-nefs-path*'.\n►►►"
  (when (locate-library "mon-cl-compat")
    (eval-when-compile (require 'mon-cl-compat))) ;; <- cl::set-difference
  (let* ((l1 (mapcar #'(lambda (x) (cadr x)) (mon-nef-dir-rmv-empt folder-alist))) 
         (l2 (mapcar #'(lambda (x) (nreverse x)) (mon-nef-dir-ranges folder-alist)))
         (l3 (mapcar #'(lambda (x) (car x)) l2))
         (l4 (if (fboundp 'cl::set-difference)
                 (cl::set-difference l3 l1)
                 (cl::set-difference 13 11)))
         (l5 (mapcar #'(lambda (x) (assoc x l2)) l4))
         (l6 (mapcar #'(lambda (x) (nreverse x)) (mon-nef-dir-fldr folder-alist)))
         (l7))
    ;; :WAS (mapcar #'(lambda (x)
    (mapc #'(lambda (x)
              (let ((range-match (cadr (assoc x l5)))
                    (folder-match (cadr (assoc x l6))))
                (when (and folder-match range-match)
                  (setq l7 (cons `(,(concat range-match " |In Folder-> " folder-match) ,x) l7)))))
            l4) l7))
;;
;;; :TEST-ME (mon-nef-dir-keep-3 *nefs_photos_nefs-alist*)

;;; ==============================
;;; :CREATED <Timestamp: Saturday June 27, 2009 @ 04:27.24 PM - by MON>
(defun mon-nef-dir-big (folder-alist)
  "Return FOLDER-ALIST as one 'BIG' combined alist. 
Return value includes those formatted with:
`mon-nef-dir-converge', `mon-nef-dir-conc-ranges', and `mon-nef-dir-keep-3'.\n
:CALLED-BY `mon-dired-nef-dir',`naf-dired-image-dir' completing-read prompts.\n
:SEE-ALSO `mon-nef-dir-keep-3',`mon-nef-dir-fldr',`mon-nef-dir-ranges',
`mon-nef-dir-conc-dups',`mon-nef-dir-find-dups',`mon-nef-dir-rmv-empt',
`*nefs_photos_nefs-alist*', `*nef-scan-nefs-path*'.\n►►►"
  (let (newp)
    (setq newp (nconc 
                (mon-nef-dir-converge folder-alist)
                (mon-nef-dir-conc-ranges folder-alist)
                (mon-nef-dir-keep-3 folder-alist)
                newp))
    newp))
;;
;;; :TEST-ME (mon-nef-dir-big *nefs_photos_nefs-alist*)

;;; ==============================
;;; :CREATED <Timestamp: Saturday June 27, 2009 @ 04:27.24 PM - by MON>
(defun mon-dired-nef-dir ()
  "Dired to one of the files in `*nef-scan-nefs-path*'.
Prompts for a directory using completions generated from
`*nefs_photos_nefs-alist*'.\n
:NOTE Use `naf-dired-image-dir' for extended dir options.\n
:SEE-ALSO `mon-dired-nef-dir', `mon-nef-dir-big', `mon-nef-dir-converge',
`mon-nef-dir-keep-3',`mon-nef-dir-fldr',`mon-nef-dir-conc-ranges',
`mon-nef-dir-ranges',`mon-nef-dir-conc-dups',`mon-nef-dir-find-dups',
`mon-nef-dir-rmv-empt'.\n►►►"
  (interactive)
  (let ((nef-alist (mon-nef-dir-big *nefs_photos_nefs-alist*))
        (get-nef-dir))
    (setq get-nef-dir
          (cadr (assoc-string 
                 (completing-read "Which nef directory (tab-completes) :" nef-alist)
                 nef-alist)))
    (dired (concat *nef-scan-nefs-path* "/" get-nef-dir))))
;;
;;; :TEST-ME (mon-dired-nef-dir)
;;; :TEST-ME (call-interactively 'mon-dired-nef-dir)

;;; ==============================
;;; :TODO incorporate: (file-directory-p filename)
;;; :WORKING :NOT-FINISHED-AS-OF
;;; :CREATED <Timestamp: Friday May 15, 2009 @ 04:49.17 PM - by MON>
(defun mon-hash-img-dir (&optional dir-to-hash)
  "Hash the img dirs in path.\n
When non-nil DIR-TO-HASH supplies an alternate img directory to hash.
Defaults to value of global variable `*img-hash*'.\n
:SEE-ALSO `mon-try-comp-dir', `mon-complete-hashed-dir', `mon-build-dir-list',
`mon-hash-img-dir'.\n►►►"
  (let* ((dir-list (mon-get-proc-buffers-directories nil dir-to-hash))
         ;; For use with for the keyword arg to make-hash-table: `:size h-size'.
	 (h-size (length dir-list)) 
	 (img-hash)
	 (with-bmp)
	 ;;(let (
	 (last-hashed "last-hashed")
	 (hashed-on (current-time))     ;-> (HIGH LOW MICRO)
	 ;; )(unless (gethash "last-hashed" *temp-hash*)
         ;;          (puthash last-hashed hashed-on *temp-hash*)))
	 )
    (setq img-hash *img-hash*) ;;(setq img-hash *temp-hash*) 
    (while dir-list
      (let ((in-dir (car dir-list)))
	(puthash in-dir '() img-hash)
	(setq dir-list (cdr dir-list))))
    (setq dir-list (mon-hash-all-keys *img-hash*)) ;*img-hash* ;*temp-hash*
    (while dir-list
      (let* ((in-dir (car dir-list))
             ;; Get partial-path of the .bmps in directory.
     	     (has-imgs (mon-get-ebay-bmps-in-dir t in-dir)) 
	     ;; (file-expand-wildcards (concat (file-name-as-directory in-dir "*.bmp"))
	     ;; (gethash 'last-hashed *img-hash*) ;*temp-hash*
	     ;; Check the file for modification since last.
	     ;; (file-attributes file-to-examine
	     ;;		 (> mod-times now-time)
	     ;; (decode-time (cadr 
             ;;              (gethash (concat *ebay-images-bmp-path* "/e1140") *temp-hash*))) 
	     ;; (decode-time (cadr (gethash SOME-HASH-ELT *temp-hash*))) 
	     )
	(if has-imgs
	    (puthash in-dir `(,has-imgs ,(current-time)) img-hash)
            (puthash in-dir `(,() ,(current-time)) img-hash))
	(setq dir-list (cdr dir-list))))
    ;; `unless' is _NOT_ what we want here - unless (> curr-time some-heuristic)
    (unless (gethash "last-hashed" img-hash) ;*img-hash*) ;*temp-hash* 
      (puthash last-hashed hashed-on img-hash)) 
    img-hash))

;;; ==============================
;;; :WORKING NOT-FINISHED-AS-OF:
;;; :CREATED <Timestamp: Tuesday May 19, 2009 @ 05:55.58 PM - by MON>
(defun mon-complete-hashed-dir (comp-hsh dir-string common-string)
  "Return a buffer displaying possible completions for DIR-STRING in COMP-HSH.
COMP-HSH \(a hash-table\) should contain a common substring COMMON-STRING.
:SEE-ALSO `mon-hash-img-dir', `mon-try-comp-dir', `mon-build-dir-list'.\n►►►"
  (let ((compl (mon-hash-all-keys comp-hsh)) ; *temp-hash*))
	(dir-st dir-string)                  ; *ebay-images-path*
	(comm-st common-string))             ; "e1")) 
    ;; (save-excursion
    (with-output-to-temp-buffer "*MON-COMPLETIONS*"
      ;; (set-buffer "*MON-COMPLETIONS*")
      ;; (display-buffer "*MON-COMPLETIONS*" t)
      (display-completion-list
       (all-completions ;; display-completion-list' completions.
        dir-st          ;; all-completions' string.
        compl)          ;; all-completions' collection.
       comm-st))        ;; display-completion-list' common-substring.
    ;; (sit-for 3)
    ;; (kill-buffer  "*MON-COMPLETIONS*"))
    ))			      
;;
;;; :TEST-ME (mon-complete-hashed-dir *img-hash* (concat *ebay-images-path* "/") "e1")

;;; ==============================
;;; :TODO Better heuristics on PTH and COLLECTION.
;;;       COLLECTION should build an a-list rather than defaulting to the hash.
;;; :TESTING-AS-OF
;;; :CREATED <Timestamp: Thursday May 21, 2009 @ 02:28.56 PM - by MON>
(defun mon-try-comp-dir (str &optional pth collection)
  "Best completion of string STR in directory PTH using COLLECTION.
When non-nil PTH is a path name default is `*ebay-images-bmp-path*'.
When non-nil COLLECTION is a list of direotories in PTH.
Default is `*img-hash*'.
List value built with `mon-build-dir-list' per completion specs.\n
:SEE-ALSO `mon-complete-hashed-dir',`mon-hash-img-dir'.\n►►►"
  (let* ((comp-str str)
	 (path (if pth 
		   (directory-file-name pth)
		 (directory-file-name *ebay-images-bmp-path*)))
	 (combo (concat path "/" str))
	 (in-coll (if collection (mon-build-dir-list pth) *img-hash*))) ; *temp-hash*
    (try-completion combo in-coll)))
;;
;;; :TEST-ME (mon-try-comp-dir "e1" *ebay-images-jpg-path* t)
;;
;;;(progn (fmakunbound 'mon-try-comp-dir) (unintern 'mon-try-comp-dir) )

;;; ==============================
(provide 'mon-dir-utils)
;;; ==============================

(eval-after-load "mon-dir-utils" '(mon-bind-nefs-photos-at-loadtime))

;;; ==============================
;;; mon-dir-utils.el ends here
;;; EOF
;;;
;;; (mon-nef-dir-ranges      *nefs_photos_nefs-alist*)
;;; (mon-nef-dir-fldr        *nefs_photos_nefs-alist*)
;;; (mon-nef-dir-rmv-empt    *nefs_photos_nefs-alist*)
;;; (mon-nef-dir-find-dups   *nefs_photos_nefs-alist*)
;;; (mon-nef-dir-conc-dups   *nefs_photos_nefs-alist*)
;;; (mon-nef-dir-converge    *nefs_photos_nefs-alist*)
;;; (mon-nef-dir-conc-ranges *nefs_photos_nefs-alist*)
;;; (mon-nef-dir-keep-3      *nefs_photos_nefs-alist*)
