;;; mon-dir-locals-alist.el --- MON global vars bound to commonly used local-site paths
;; -*- mode: EMACS-LISP; -*-

;;; ================================================================
;; Copyright © 2009, 2010 MON KEY. All rights reserved.
;;; ================================================================

;; FILENAME: mon-dir-locals-alist.el
;; AUTHOR: MON KEY
;; MAINTAINER: MON KEY
;; CREATED: 2009-05-28T16:43:15-04:00Z
;; VERSION: 1.0.0
;; COMPATIBILITY: Emacs23.*
;; KEYWORDS: environment, local, installation, dired

;;; ================================================================

;;; COMMENTARY: 

;; =================================================================
;; DESCRIPTION:
;; mon-dir-locals-alist provides global vars bound to commonly used paths
;; with user/site conditionals to path. Additionaly some subr's for quickload
;; from alists of some directory contents.
;;
;; FUNCTIONS:►►►
;; 
;; FUNCTIONS:◄◄◄
;;
;; MACROS:
;;
;; CONSTANTS:
;;
;; VARIABLES:
;; `*mon-artist-naf-path*', `*mon-brand-naf-path*', `*mon-nef-scan-path*', `*mon-nef-scan-nefs-path*',
;; `*mon-nef-scan-nef2-path*', `*mon-ebay-images-path*', `*mon-ebay-images-bmp-path*',
;; `*mon-ebay-images-jpg-path*', `*mon-ebay-images-temp-path*', `*emacs2html-temp*',
;; `*bug-HG-path*', `*mon-CL-scratch-path*',
;; `*mon-buffer-mode-defaults*', `*mon-HG-root-path*', `*mon-smith-poster-HG-path*', 
;; `*mon-record-current-directory*', `*mon-dir-locals-alist-xrefs*',
;; `*mon-nef-scan-drive*', `*mon-nef-scan-base-path*',
;;
;; ALIASES/ADVISED/SUBST'D:
;; `*mon-emacs2html-temp*' -> `*emacs2html-temp*'
;;
;; DEPRECATED:
;;
;; RENAMED:
;;
;; MOVED:
;; <Timestamp: Tuesday June 16, 2009 @ 04:21.18 PM - by MON KEY>
;; :DIRECTORY-PATH-VARS
;; `*mon-nef-scan-path*', `*mon-nef-scan-nefs-path*', `*mon-nef-scan-nef2-path*', 
;; `*mon-ebay-images-path*', `*mon-ebay-images-bmp-path*', `*mon-ebay-images-jpg-path*', 
;; `*mon-ebay-images-temp-path*'
;;
;; :NAF-MODE-RELATED-PATH-VARS 
;; `*mon-artist-naf-path*'    <- ebay-template-tools.el
;; `*mon-brand-naf-path*'     <- ebay-template-tools.el
;; 
;; `*mon-timestamp-cond*' -> mon-time-utils.el
;;
;; REQUIRES:
;;
;; REQUIRED-BY: 
;;  :FILE mon-dir-utils.el
;;  :FILE ebay-template-mode.el
;;  :FILE mon-rename-image-utils.el
;;  :FILE smith-poster-utils.el 
;;
;; TODO:
;; All of the variables contained of this file should be defcustom'd.
;;
;; NOTES:
;; Many of the variables provided here perform lookups on key values of
;; `*mon-misc-path-alist*' to derive a system local path. For additional details:
;; :SEE :FILE mon-site-local-defaults.el
;; :SEE (URL `http://www.emacswiki.org/emacs/mon-site-local-defaults.el')
;;
;; SNIPPETS:
;;
;; THIRD PARTY CODE:
;;
;; URL: http://www.emacswiki.org/emacs/mon-dir-locals-alist.el
;; FILE-PUBLISHED: <Timestamp: #{2009-09-20} - by MON KEY>
;; 
;; EMACSWIKI: { URL of an EmacsWiki describing mon-dir-locals-alist. }
;;
;; FILE-CREATED:
;; <Timestamp: #{2009-05-28T16:43:15-04:00Z} - by MON KEY>
;;
;; =================================================================

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

(eval-when-compile (require 'cl))


;;; ==============================
;;; :NOTE `IS-MON-SYSTEM-P' may be called already use a tempory symbol and
;;;       unintern it at BOF.
;;; :CHANGESET 1790
;;; :CREATED <Timestamp: #{2010-05-28T17:35:26-04:00Z}#{10215} - by MON KEY>
(defvar mon-bind-dir-locals-alist)
;;
(eval-when (compile load eval)
  (when (or (intern-soft "IS-MON-SYSTEM-P")
            (bound-and-true-p IS-MON-SYSTEM-P))
    (setq mon-bind-dir-locals-alist t)))

;;; ==============================
;;; :CREATED <Timestamp: #{2009-09-28T16:02:00-04:00Z}#{09401} - by MON>
(defvar *mon-dir-locals-alist-xrefs* nil
  "*Xrefing list of variables defined in file mon-dir-locals-alist.el\n
:EXAMPLE\n\(symbol-value '*mon-dir-locals-alist-xrefs*\)
\(symbol-value \(nth 3 *mon-dir-locals-alist-xrefs*\)\)\n
:SEE-ALSO `*naf-mode-xref-of-xrefs*'.\n►►►")
;;
(unless (bound-and-true-p *mon-dir-locals-alist-xrefs*)
  (when mon-bind-dir-locals-alist
    (setq *mon-dir-locals-alist-xrefs* 
          '(*mon-artist-naf-path*
            *mon-brand-naf-path*
            *mon-ebay-images-bmp-path*
            *mon-ebay-images-jpg-path*
            *mon-ebay-images-lookup-path*
            *mon-ebay-images-path*
            *mon-ebay-images-temp-path*
            *emacs2html-temp*
            *mon-html-fontify-file-name-template*
            *mon-nef-scan-base-path*
            *mon-nef-scan-drive*
            *mon-nef-scan-nef2-path*
            *mon-nef-scan-nefs-path*
            *mon-nef-scan-path*
            *mon-nefs_photos_nefs-alist*
            *mon-smith-poster-HG-path*
            *mon-CL-scratch-path*
            *mon-record-current-directory*
            *bug-HG-path*
            *mon-HG-root-path*
            *mon-buffer-mode-defaults*
            *mon-dir-locals-alist-xrefs*))))
;;
;;; :TEST-ME  *mon-dir-locals-alist-xrefs*
;;; :TEST-ME (symbol-value (nth 3 *mon-dir-locals-alist-xrefs*))
;;
;;;(progn (makunbound '*mon-dir-locals-alist-xrefs*) 
;;;        (unintern '*mon-dir-locals-alist-xrefs*) )

;;; ==============================
;;; :CREATED <Timestamp: #{2009-08-11T13:42:20-04:00Z}#{09332} - by MON KEY>
(defvar *mon-HG-root-path*  nil
  "Path to the root path for HG repos.\n
:EXAMPLE\n\n(cons *mon-HG-root-path* \(file-directory-p *mon-HG-root-path*\)\)\n
:SEE-ALSO `*mon-emacs-root*', `*mon-emacsd*'.\n►►►")
;;
(unless (and (bound-and-true-p *mon-HG-root-path*)
             (not mon-bind-dir-locals-alist)) 
  (let ((mhrp (nth 5 (assoc (cond (IS-MON-P-W32     1)
                                  (IS-BUG-P-REMOTE  4)
                                  (IS-BUG-P         3)
                                  (IS-MON-P-GNU     2))
                            *mon-emacsd*))))
    (setq *mon-HG-root-path* mhrp)))
;;         
;;; :TEST-ME (file-directory-p *mon-HG-root-path*)
;;
;;;(progn (makunbound '*mon-HG-root-path*) (unintern '*mon-HG-root-path*) )

;;; ==============================
;;; CREATED: <Timestamp: #{2009-08-11T13:42:35-04:00Z}#{09332} - by MON KEY>
(defvar *mon-smith-poster-HG-path* nil
  "Path to the local Smith HG docs folder.\n
:EXAMPLE\n\n(cons *mon-smith-poster-HG-path*
  \(file-directory-p *mon-smith-poster-HG-path*\)\)\n
:SUBDIR-OF `*mon-HG-root-path*'\n
:SEE-ALSO `*mon-emacsd*'.\n►►►")
;;
(unless (and (bound-and-true-p *mon-smith-poster-HG-path*)
             (not mon-bind-dir-locals-alist))
  (setq *mon-smith-poster-HG-path* 
        (when IS-MON-SYSTEM-P (concat *mon-HG-root-path* *smith-poster-docs*))))
;;
;;; :TEST-ME (file-directory-p *mon-smith-poster-HG-path*)
;;
;;;(progn (makunbound '*mon-smith-poster-HG-path*) (unintern '*mon-smith-poster-HG-path*) )
              
;;; ==============================
;;; :CREATED <Timestamp: Monday July 20, 2009 @ 02:59.11 PM - by MON KEY>
(defvar *mon-CL-scratch-path* nil
  "Path for string CL-scratch files. Subdir of `*mon-emacs-root*'. 
This is used to keep files transferable across machines with Mercurial.\n
:EXAMPLE\n\n\(cons *mon-CL-scratch-path*
      \(file-directory-p *mon-CL-scratch-path*\)\)\n
:SEE-ALSO `common-lisp-hyperspec-root'.\n►►►")
;;
(unless (and (bound-and-true-p *mon-CL-scratch-path*)
             (not mon-bind-dir-locals-alist))
  (setq *mon-CL-scratch-path*
        (when IS-MON-P
          (concat *mon-HG-root-path* 
                    (cadr (assoc 'the-CL-path *mon-misc-path-alist*))
                    "/CL-NOTES"))))
;;
;;; :TEST-ME (file-directory-p *mon-CL-scratch-path*)
;;
;;;(progn (makunbound '*mon-CL-scratch-path*) (unintern '*mon-CL-scratch-path*) )

;;; ==============================
;;; :CREATED <Timestamp: Wednesday July 15, 2009 @ 01:36.16 PM - by MON KEY>
(defvar *bug-HG-path* nil
  "Path var used to pass Bug paths to file in the HG repo.
Used primarily over ERC to exchange paths w32 network share paths.\n
:EXAMPLE\n\n\(cons *bug-HG-path*
      \(file-directory-p *bug-HG-path*\)\)\n
:SEE-ALSO `mon--local-url-for-bug', `mon-get-local-url-for-bug',
`*mon-HG-root-path*'.\n►►►")
;;
(unless (and (bound-and-true-p *bug-HG-path*)
             (not mon-bind-dir-locals-alist))
  (setq *bug-HG-path* 
        (cond (IS-MON-P-W32 (nth 6 (assoc 3 *mon-emacsd* ))) ;; Get path from BUG alist.
	      ((or IS-BUG-P-REMOTE IS-BUG-P IS-MON-P-GNU) *mon-emacs-root*))))
;;
;;; :TEST-ME (file-directory-p *bug-HG-path*)
;;
;;;(progn (makunbound '*bug-HG-path*) (unintern '*bug-HG-path*) )

;;; ==============================
;;; :CREATED <Timestamp: #{2009-08-11T18:12:47-04:00Z}#{09332} - by MON KEY>
(defvar *mon-record-current-directory* nil
  "Default filename to record buffer's current-directory.\n
EXAMPLE:\n\n\(cons *mon-record-current-directory* 
      \(file-exists-p *mon-record-current-directory*\)\)\n
:SUBDIR-OF `*mon-emacs-root*'
:CALLED-BY `mon-dir-save-current-to-file'.\n
:SEE-ALSO .\n►►►")
;;
(unless (and (bound-and-true-p *mon-record-current-directory*)
             (not mon-bind-dir-locals-alist))
  (setq *mon-record-current-directory* 
        (concat *mon-emacs-root* "/current-directory")))
;;
;;; :TEST-ME (file-exists-p *mon-record-current-directory*)
;; 
;;;(progn (makunbound '*mon-record-current-directory*) 
;;;       (unintern '*mon-record-current-directory*) )

;;; ==============================
;;; :MODIFICATIONS <Timestamp: #{2010-04-02T12:11:11-04:00Z}#{10135} - by MON KEY>
(defvar *emacs2html-temp* nil
  "*Path to hold temp files transformed with `htmlfontify-buffer'.\n
:EXAMPLE\n\n(cons *emacs2html-temp* (file-directory-p *emacs2html-temp*))\n
:ALIASED-BY `*mon-emacs2html-temp*'\n
:SEE-ALSO `mon-htmlfontify-region-to-firefox', `mon-htmlfontify-buffer-to-firefox', 
`mon-htmlfontify-dir-purge-on-quit',  `mon-html-fontify-generate-file-name'
`*mon-html-fontify-file-name-template*', `*emacs2html-temp*'.\n►►►")
;;
;;; :NOTE `*emacs2html-temp*' is provided in :FILE html-fontify.el and _may_
;;;        already be `bound-and-true-p'. So, it is better to bind it directly
;;;        with setq.
;;; :WAS (unless (bound-and-true-p *emacs2html-temp*)
(unless (not mon-bind-dir-locals-alist)
  (let ((e2ht (cond ((bound-and-true-p IS-MON-SYSTEM-P)
                     (concat *mon-local-emacs-temp-dir* "/emacs2html-temp"))
                    (t (concat 
                        (if (string-match-p "/$" user-emacs-directory)
                            ;; :NOTE Knock of the trailing / e.g. when:
                            ;;       user-emacs-directory => "~/.emacs.d/"
                            (replace-regexp-in-string "/$" "" user-emacs-directory t)
                          user-emacs-directory)
                        "/emacs2html-temp")))))
    (if (file-exists-p e2ht)
        (setq *emacs2html-temp* e2ht)
      ;; Shouldn't write to file system when `IS-NOT-A-MON-SYSTEM'
      (if (bound-and-true-p IS-MON-SYSTEM-P) 
          (progn 
            (mkdir e2ht)
            (setq *emacs2html-temp* e2ht))
        (progn
          (setq *emacs2html-temp* e2ht)
          (warn (concat
                 "The :VARIABLE `*emacs2html-temp*' -- was bound to non-existent directory:\n"
                 "\n %18c%s\n\n"
                 "%17c This path is required by functions:\n\n"
                 "%17c `mon-htmlfontify-dir-purge-on-quit'\n"              
                 "%17c `mon-htmlfontify-buffer-to-firefox'\n"
                 "%17c `mon-htmlfontify-region-to-firefox'\n\n"
                 "%17c Before using these functions verify path and/or \(re\)bind accordingly")
                32 *emacs2html-temp* 32 32 32 32 32))))))
;;
(when (or (bound-and-true-p *emacs2html-temp*)
          mon-bind-dir-locals-alist)
(defvaralias '*mon-emacs2html-temp* '*emacs2html-temp*))
;;
;;; :TEST-ME (file-directory-p *emacs2html-temp*)
;;
;;;(progn (makunbound '*emacs2html-temp*) (unintern '*emacs2html-temp*) )

;;; ==============================
;;; :CREATED <Timestamp: #{2010-04-02T12:37:47-04:00Z}#{10135} - by MON KEY>
(defvar *mon-html-fontify-file-name-template* nil
  "*Format string for `mon-html-fontify-generate-file-name'\n
Used to generate temporary file-names for `mon-htmlfontify-*' procedures.\n
:SEE-ALSO `mon-htmlfontify-region-to-firefox', `mon-htmlfontify-buffer-to-firefox', 
`mon-htmlfontify-dir-purge-on-quit',  `mon-html-fontify-generate-file-name'
`*mon-html-fontify-file-name-template*', `*emacs2html-temp*'.\n►►►")
;;
(unless (and (bound-and-true-p *mon-html-fontify-file-name-template*)
             (not mon-bind-dir-locals-alist))
  (setq *mon-html-fontify-file-name-template* "%s/emacs2firefox-%d.html"))
;;
;;;(progn (makunbound '*mon-html-fontify-file-name-template*)
;;;       (unintern   '*mon-html-fontify-file-name-template*) )  

;;; ==============================
;;; :CREATED <Timestamp: Monday May 25, 2009 @ 03:18.01 PM - by MON KEY>
(defvar *mon-artist-naf-path* nil
  "*Path to Brand NAF folders on local systems.\n
:EXAMPLE\n\n\(cons *mon-artist-naf-path*
      \(file-directory-p *mon-artist-naf-path*\)\)\n
:NOTE Binding only relevant if specifed in `*mon-emacsd*'. When `*mon-emacsd*' is non-nil it
is an alist of integer based keys which map user/system names to local path
names. :SEE :FILE mon-site-local-defaults.el for exampls and additional discussion.\n
:CALLED-BY `mon-dired-naf-artist-letter', `mon-explorer-naf-brand', `mon-dired-naf-image-dir'.\n
:SEE-ALSO `*mon-brand-naf-path*'.\n►►►")
;; 
(unless (and (bound-and-true-p *mon-artist-naf-path*)
             (not mon-bind-dir-locals-alist)
             (bound-and-true-p *mon-emacsd*))
  (let ((anp (nth 7 (assoc (cond (IS-MON-P-W32      1)
                                   (IS-BUG-P-REMOTE 4) 
                                   (IS-BUG-P        3)
                                   (IS-MON-P-GNU    2)
                                   (t nil))
                             *mon-emacsd*))))
      (setq *mon-artist-naf-path* (concat anp "/ARTISTS"))))
;;
;;; :TEST-ME (file-directory-p *mon-artist-naf-path*)
;;
;;;(progn (makunbound '*mon-artist-naf-path*) (unintern '*mon-artist-naf-path*) )

;;; ==============================
;;; :NOTE Update these when if a path becomes available.
;;;  
;;;  (IS-BUG-P-REMOTE  )
;;; :CREATED <Timestamp: Monday May 25, 2009 @ 03:17.53 PM - by MON KEY>
(defvar *mon-brand-naf-path* nil
  "*Path to Brand NAF folders on local w32 systems.\n
Path relative to the `*mon-artist-naf-path*' directory.\n
:EXAMPLE\n\n\(cons *mon-brand-naf-path*
      \(file-directory-p *mon-brand-naf-path*\)\)\n
:CALLED-BY `mon-dired-naf-brand-letter', `mon-explorer-naf-brand', `mon-dired-naf-image-dir'
:SEE-ALSO `*mon-artist-naf-path*'.\n►►►")
;;
(unless (and (bound-and-true-p *mon-brand-naf-path*)
             (not mon-bind-dir-locals-alist))
  (setq *mon-brand-naf-path*
        (when (intern-soft "IS-MON-SYSTEM-P")    
          (cond ((or IS-MON-P-W32 IS-BUG-P)  
                 (concat *mon-artist-naf-path* "/BRANDS") )
                (IS-MON-P-GNU 
                 (concat (directory-file-name 
                          (file-name-directory *mon-artist-naf-path*)) "/BRANDS"))
                ;; These aren't Available on these machines.
                (IS-BUG-P-REMOTE nil)))))
;;
;;; :TEST-ME (file-directory-p *mon-brand-naf-path*)
;;
;;;(progn (makunbound '*mon-brand-naf-path*) (unintern '*mon-brand-naf-path*) ) 

;;; ==============================
;;; :CREATED <Timestamp: #{2009-09-28T15:34:06-04:00Z}#{09401} - by MON>
(defvar *mon-nef-scan-drive* nil
  "*Base drive or device level path for computing local path variables.\n
Path relative to the alist key ``the-nef-drv'' in `*mon-misc-path-alist*'.\n
:SEE-ALSO `*mon-nef-scan-base-path*',`*mon-nef-scan-path*',
`*mon-nef-scan-nefs-path*',`*mon-nef-scan-nef2-path*',
`*mon-ebay-images-path*',`*mon-ebay-images-bmp-path*',
`*mon-ebay-images-jpg-path*',`*mon-ebay-images-temp-path*'.\n►►►")
;;
(unless (and (bound-and-true-p *mon-nef-scan-drive*)
             (not mon-bind-dir-locals-alist))
  (setq *mon-nef-scan-drive*
        (when (intern-soft "IS-MON-SYSTEM-P")
          (cadr (assoc 'the-nef-drv *mon-misc-path-alist*)))))

;;; ==============================
;;; :CREATED <Timestamp: #{2009-09-28T15:34:02-04:00Z}#{09401} - by MON>
(defvar *mon-nef-scan-base-path* nil
  "*Base path sans drive or device for computing *mon-{...}-path* variables.\n
:EXAMPLE\n\n\(cons *mon-nef-scan-base-path*
      \(file-directory-p *mon-nef-scan-nefs-path*\)\)\n
:SUBDIR-OF `*mon-nef-scan-drive*'
:SEE-ALSO `*nef-scan-base-drive*'`*mon-nef-scan-path*',
`*mon-nef-scan-nefs-path*',`*mon-nef-scan-nef2-path*',
`*mon-ebay-images-path*',`*mon-ebay-images-bmp-path*',
`*mon-ebay-images-jpg-path*',`*mon-ebay-images-temp-path*'.\n►►►")
;;
(unless (and (bound-and-true-p *mon-nef-scan-base-path*)
             (not mon-bind-dir-locals-alist))
  (setq *mon-nef-scan-base-path*
        (when (intern-soft "IS-MON-SYSTEM-P")
          (concat *mon-nef-scan-drive*
                  ;; :NOTE Filesystems, dirs, mounts etc.  change periodically
                  ;;       so we must do this silliness.
                  (cond (IS-W32-P "/")
                        ;; :WAS (IS-MON-P-GNU (concat (nth 3 (assoc 2 *mon-emacsd*)) "/"))))
                        (IS-MON-P-GNU "-"))))))
;;
;;; :TEST-ME (file-directory-p (concat *mon-nef-scan-base-path* "\x41"))
;;
;;; (progn (makunbound '*mon-nef-scan-base-path*) (unintern '*mon-nef-scan-base-path*) )
        
;;; ==============================
;;; :NOTE Following Global Variables
;;; :CREATED <Timestamp: Wednesday May 06, 2009 @ 07:49.11 PM - by MON KEY>
;;; ==============================
;;; :MODIFICATIONS <Timestamp: #{2009-09-29T13:04:26-04:00Z}#{09402} - by MON KEY>
(defvar *mon-nef-scan-path* nil
  "*User conditional path to the NEF archived photos drive.\n
:SUBDIR-OF `*mon-nef-scan-base-path*'\n
:SEE-ALSO `*mon-nef-scan-drive*', `*mon-nef-scan-base-path*'
`*mon-nef-scan-nefs-path*', `*mon-nef-scan-nef2-path*', `*mon-ebay-images-path*',
`*mon-ebay-images-bmp-path*', `*mon-ebay-images-jpg-path*', `*mon-ebay-images-temp-path*',
`*mon-ebay-images-lookup-path*'.\n►►►")
;;
(unless (and (bound-and-true-p *mon-nef-scan-path*)
             (not mon-bind-dir-locals-alist))
  (setq *mon-nef-scan-path* 
        (cond (IS-W32-P (concat *mon-nef-scan-base-path* "NEFS_PHOTOS"))
              (IS-MON-P-GNU (concat *mon-nef-scan-base-path* "\x42")))))
;;
;;; :TEST-ME (file-directory-p *mon-nef-scan-path*)
;;
;;; (progn (makunbound '*mon-nef-scan-path*) (unintern '*mon-nef-scan-path*) )

;;; ==============================
(defvar *mon-nef-scan-nefs-path* nil
  "*User conditional path to NEF Photos working drive.\n
:EXAMPLE\n\n\(cons *mon-nef-scan-nefs-path* 
      \(file-directory-p *mon-nef-scan-nefs-path*\)\)\n
:NOTE The var `*mon-nefs_photos_nefs-alist*' contains an association list of this
directory's paths as the directory doesnt' change that much.\n
:SEE-ALSO `*mon-nef-scan-drive*', `*mon-nef-scan-base-path*',
`*mon-nef-scan-path*', `*mon-nef-scan-nef2-path*', `*mon-ebay-images-path*',
`*mon-ebay-images-bmp-path*', `*mon-ebay-images-jpg-path*', 
`*mon-ebay-images-temp-path*' `*mon-ebay-images-lookup-path*'.\n►►►")
;;
(unless (and (bound-and-true-p *mon-nef-scan-nefs-path*)
             (not mon-bind-dir-locals-alist))
  (setq *mon-nef-scan-nefs-path* 
        (cond (IS-W32-P (concat *mon-nef-scan-base-path* "NEF_Drive2"))
              (IS-MON-P-GNU (concat *mon-nef-scan-base-path* "\x42")))))
;;
;;; :TEST-ME (file-directory-p *mon-nef-scan-nefs-path*)
;;
;;; (progn (makunbound '*mon-nef-scan-path*) (unintern '*mon-nef-scan-path*) )

;;; ==============================
;;; :TODO This needs to be a hash-table.
;;; :NOTE Var loaded from `mon-dir-utils.el' at loadtime.
;;; :MODIFICATIONS <Timestamp: Thursday May 28, 2009 @ 04:43.15 PM - by MON KEY>
(defvar *mon-nefs_photos_nefs-alist* nil
  "*Directory contents as an alist for path `*mon-nef-scan-nefs-path*'.
A list generated with `mon-dir-nef-update-photos-alist' and bound at loadtime
with `mon-bind-nefs-photos-at-loadtime'.\n
:NOTE using the ugly underscores to help distinguish in completion lists.\n
:SEE-ALSO `*mon-nef-scan-drive*', `*mon-nef-scan-base-path*',`*mon-nef-scan-path*',
`*mon-nef-scan-nefs-path*', `*mon-ebay-images-path*', `*mon-ebay-images-bmp-path*',
`*mon-ebay-images-jpg-path*', `*mon-ebay-images-temp-path*',
`*mon-ebay-images-lookup-path*'.\n►►►")

;;; ==============================
(defvar *mon-nef-scan-nef2-path* nil
  "*User conditional path to NEF2 drive.\n
:EXAMPLE\n\n\(cons *mon-nef-scan-nef2-path* 
      \(file-directory-p *mon-nef-scan-nef2-path*\)\)\n
:SUBDIR-OF `*mon-nef-scan-nefs-path*'
:SEE-ALSO `*mon-nef-scan-drive*', `*mon-nef-scan-base-path*',`*mon-nef-scan-path*',
`*mon-nef-scan-nefs-path*', `*mon-ebay-images-path*', `*mon-ebay-images-bmp-path*',
`*mon-ebay-images-jpg-path*', `*mon-ebay-images-temp-path*',
`*mon-ebay-images-lookup-path*'.\n►►►")
;;
(unless (and (bound-and-true-p *mon-nef-scan-nef2-path*)
             (not mon-bind-dir-locals-alist))
  ;; :NOTE This needs to happen this way because of occasional filesystem shifts.
  (setq *mon-nef-scan-nef2-path*
        (if IS-MON-P-GNU 
            (concat (substring *mon-nef-scan-nefs-path* 0 -1) "\x41")
          *mon-nef-scan-nefs-path*)))
;;
;;; :TEST-ME (file-directory-p *mon-nef-scan-nef2-path*)
;;
;;; (progn (makunbound '*mon-nef-scan-nef2-path*) (unintern '*mon-nef-scan-nef2-path*) )

;;; ==============================
(defvar *mon-ebay-images-path* nil
  "*User conditional path to eBay image scans.\n
:EXAMPLE\n\n\(cons *mon-ebay-images-path*
      \(file-directory-p *mon-ebay-images-path*\)\)\n
:SUBDIR-OF `*mon-nef-scan-nef2-path*'
:SEE-ALSO `*mon-nef-scan-drive*', `*mon-nef-scan-base-path*',`*mon-nef-scan-path*',
`*mon-nef-scan-nefs-path*', `*mon-nef-scan-nef2-path*',`*mon-ebay-images-bmp-path*',
`*mon-ebay-images-jpg-path*', `*mon-ebay-images-temp-path*',
`*mon-ebay-images-lookup-path*'.\n►►►")
;;
(unless (and (bound-and-true-p *mon-ebay-images-path*)
             (not mon-bind-dir-locals-alist))
  (setq *mon-ebay-images-path*
	(concat *mon-nef-scan-nef2-path* "/EBAY")))
;;
;;; :TEST-ME (file-directory-p *mon-ebay-images-path*)
;;
;;; (progn (makunbound '*mon-ebay-images-path*) (unintern '*mon-ebay-images-path*) )

;;; ==============================
(defvar *mon-ebay-images-bmp-path* nil
  "*User conditional path to ebay .bmp scans.\n
:EXAMPLE\n\n\(cons *mon-ebay-images-bmp-path*
      \(file-directory-p *mon-ebay-images-bmp-path*\)\)\n
:SUBDIR-OF `*mon-ebay-images-path*'
:CALLED-BY `mon-dir-try-comp', `mon-dired-naf-image-dir'.\n
:SEE-ALSO `*mon-nef-scan-drive*', `*mon-nef-scan-base-path*',`*mon-nef-scan-path*',
`*mon-nef-scan-nefs-path*', `*mon-nef-scan-nef2-path*', `*mon-ebay-images-path*',
`*mon-ebay-images-jpg-path*', `*mon-ebay-images-temp-path*',
`*mon-ebay-images-lookup-path*'.\n►►►")
;;
(unless (and (bound-and-true-p *mon-ebay-images-bmp-path*)
             (not mon-bind-dir-locals-alist))
  (setq *mon-ebay-images-bmp-path*
        (concat *mon-ebay-images-path* "/BMP-Scans")))
;;
;;; :TEST-ME (file-directory-p *mon-ebay-images-bmp-path*)
;;
;;; (progn (makunbound '*mon-ebay-images-bmp-path*) (unintern '*mon-ebay-images-bmp-path*) )

;;; ==============================
(defvar *mon-ebay-images-jpg-path* nil
  "*User conditional path to ebay scans converted to .jpg.\n
:EXAMPLE\n\n\(cons *mon-ebay-images-jpg-path*
      \(file-directory-p *mon-ebay-images-jpg-path*\)\)\n
:SUBDIR-OF `*mon-ebay-images-path*'.\n
:CALLED-BY `mon-dired-naf-image-dir'.\n
:SEE-ALSO `*mon-nef-scan-drive*', `*mon-nef-scan-base-path*',`*mon-nef-scan-path*',
`*mon-nef-scan-nefs-path*', `*mon-nef-scan-nef2-path*', `*mon-ebay-images-path*',
`*mon-ebay-images-bmp-path*', `*mon-ebay-images-temp-path*',
`*mon-ebay-images-lookup-path*'.\n►►►")
;;
(unless (and (bound-and-true-p *mon-ebay-images-jpg-path*)
             (not mon-bind-dir-locals-alist))
  (setq *mon-ebay-images-jpg-path*
 	(concat *mon-ebay-images-path* "/BIG-cropped-jpg")))
;;
;;; :TEST-ME (file-directory-p *mon-ebay-images-jpg-path*)
;;
;;; (progn (makunbound '*mon-ebay-images-jpg-path*) (unintern '*mon-ebay-images-jpg-path*) )

;;; ==============================
(defvar *mon-ebay-images-temp-path* nil
  "*User conditional path to ebay temp files.\n
:EXAMPLE\n\n\(cons *mon-ebay-images-temp-path*
      \(file-directory-p *mon-ebay-images-temp-path*\)\)\n
:SUBDIR-OF `*mon-ebay-images-path*'\n
:CALLED-BY `mon-insert-ebay-dirs' and `mon-make-ebay-dir-list'.\n
:SEE-ALSO `*mon-nef-scan-drive*', `*mon-nef-scan-base-path*', `*mon-nef-scan-path*',
`*mon-nef-scan-nefs-path*', `*mon-nef-scan-nef2-path*', `*mon-ebay-images-path*',
`*mon-ebay-images-bmp-path*', `*mon-ebay-images-jpg-path*', 
`*mon-ebay-images-lookup-path*'.\n►►►")
;;
(unless (and (bound-and-true-p *mon-ebay-images-temp-path*)
             (not mon-bind-dir-locals-alist))
  (setq *mon-ebay-images-temp-path*
	(concat *mon-ebay-images-path* "/temp-batch")))
;;
;;; :TEST-ME (file-directory-p *mon-ebay-images-temp-path*)
;;
;;;(progn (makunbound '*mon-ebay-images-temp-path*) (unintern '*mon-ebay-images-temp-path*) )

;;; ==============================
;;; :NOTE If/when these get hashed set above path vars to:
;;;       `*mon-nef-img-hash*' `*mon-bmp-img-hash*' `*mon-jpg-img-hash*'
(defvar *mon-ebay-images-lookup-path* nil
  "*A list of default paths to examine when functions need to look for images.\n
Car of list is keys image-type extension as a string: \".nef\", \".jpg\", or \".bmp\".
For these purposes we don't want to be in the NEFS folder and assume a .nef 
source image is in the `*mon-ebay-images-bmp-path*'.\n
:EXAMPLE\n\n\(file-directory-p 
 \(symbol-value \(cadr \(assoc \".nef\" *mon-ebay-images-lookup-path*\)\)\)\)\n
:SEE-ALSO `mon-image-verify-type', `*mon-nef-scan-drive*',
`*mon-nef-scan-base-path*', `*mon-nef-scan-path*', `*mon-nef-scan-nefs-path*',
`*mon-nef-scan-nef2-path*', `*mon-ebay-images-path*',
`*mon-ebay-images-jpg-path*'.\n►►►")
;;
(unless (and (bound-and-true-p *mon-ebay-images-lookup-path*)
             (not mon-bind-dir-locals-alist))
  (setq *mon-ebay-images-lookup-path*
	  '((".nef" *mon-ebay-images-bmp-path* "BMP-Scans")       ;; *mon-nef-img-hash*)
	    (".jpg" *mon-ebay-images-jpg-path* "BIG-cropped-jpg") ;; *mon-jpg-img-hash*)
	    (".bmp" *mon-ebay-images-bmp-path* "BMP-Scans"))))   ;; *mon-bmp-img-hash*))))
;;
;;; :TEST-ME (file-directory-p (symbol-value (cadr (assoc ".nef" *mon-ebay-images-lookup-path*))))
;;
;;;(progn (makunbound '*mon-ebay-images-lookup-path*)(unintern 'ebay-images-lookup-path*) )

;;; ==============================
(defvar *mon-buffer-mode-defaults* nil
  "*A list of defuault directory and file extensions.
Used by mon-* functions to help to determine best path with conditional on
buffers' current mode and/or extension.\n
:EXAMPLE\n\n\(assoc-string \"NAF-mode\" *mon-buffer-mode-defaults*\)\n
:SEE-ALSO `*mon-ebay-images-bmp-path*', `*mon-artist-naf-path*',
`*mon-CL-scratch-path*', `*mon-emacs-root*' .\n►►►")
;;
(unless (and (bound-and-true-p *mon-buffer-mode-defaults*)
             (not mon-bind-dir-locals-alist))
  (setq *mon-buffer-mode-defaults*
        `(("eBay-Template"  ,*mon-ebay-images-bmp-path* ".dbc")
          ("NAF-mode" ,*mon-artist-naf-path* ".naf")
          ("Emacs-Lisp" ,*mon-naf-mode-root* ".el")
          ("Lisp" ,*mon-CL-scratch-path* ".lisp"))))
;;
;;; :NESETED-SYTLE-ALIST
;;; `(("eBay-Template"  (,*mon-ebay-images-bmp-path* ".dbc"))
;;;  ("NAF-mode"  (,*mon-artist-naf-path* ".naf"))
;;;  ("Emacs-Lisp" (,(concat *mon-emacs-root* "/naf-mode") ".el"))
;;;  ("Lisp" (,*mon-CL-scratch-path* ".lisp"))))
;;
;;; :TEST-ME (assoc-string "eBay-Template" *mon-buffer-mode-defaults*)
;;; :TEST-ME (assoc-string "Lisp" *mon-buffer-mode-defaults*)
;;; :TEST-ME (assoc-string "Emacs-Lisp" *mon-buffer-mode-defaults*)
;;; :TEST-ME (assoc-string "NAF-mode" *mon-buffer-mode-defaults*)
;;
;;;(progn (makunbound '*mon-buffer-mode-defaults*) (unintern '*mon-buffer-mode-defaults*) )

;;; ==============================
;;; :CREATED <Timestamp: Monday April 06, 2009 @ 10:56.24 AM - by MON KEY>
;;; :NOTE Trying to set a delete to trash dir. variable in naf sub-dirs.
;;;       I think maybe w/ version 23.1 dir-locals Suck!
;;; (dir-locals-set-class-variables 
;;;  'naf-dir-no-clobber
;;;  '((nil . ((delete-by-moving-to-trash . t)))))
;;; ;;
;;; (dir-locals-set-directory-class 
;;;  (cond 
;;;   (IS-MON-P-W32 
;;;    (expand-file-name *mon-artist-naf-path*))
;;;   (IS-BUG-P  
;;;   (expand-file-name *mon-artist-naf-path*))) 'naf-dir-no-clobber)
;;; ==============================

;;; ==============================
;;; :CHANGESET 1789
;;; :CREATED <Timestamp: #{2010-05-28T17:32:45-04:00Z}#{10215} - by MON KEY>
;; Remove the symbol `mon-bind-dir-locals-alist' if it isn't needed.
(eval-when (compile load eval)
  (when (and (intern-soft "mon-bind-dir-locals-alist")
             (null mon-bind-dir-locals-alist))
    (progn (makunbound 'mon-bind-dir-locals-alist)
           (unintern 'mon-bind-dir-locals-alist))))

;;; ==============================
(provide 'mon-dir-locals-alist)
;;; ==============================

;;; ================================================================
;;; mon-dir-locals-alist.el ends here
;;; EOF

;;; ==============================
;;; :TEST-ME (file-directory-p *mon-nef-scan-path*)
;;; :TEST-ME (file-directory-p *mon-nef-scan-nefs-path*)
;;; :TEST-ME (file-directory-p *mon-nef-scan-nefs-path*)
;;; :TEST-ME (file-directory-p *mon-nef-scan-nef2-path*)
;;; :TEST-ME (file-directory-p *mon-ebay-images-path*)
;;; :TEST-ME (file-directory-p *mon-ebay-images-bmp-path*)
;;; :TEST-ME (file-directory-p *mon-ebay-images-jpg-path*)
;;; :TEST-ME (file-directory-p *mon-ebay-images-temp-path*)
;;; :TEST-ME (file-directory-p *bug-HG-path*)
;;; :TEST-ME (file-directory-p *mon-CL-scratch-path*)
;;; :TEST-ME (file-directory-p *mon-HG-root-path*)
;;; :TEST-ME (file-directory-p *mon-smith-poster-HG-path*)
;;; :TEST-ME (file-directory-p (cadr (assoc-string "Emacs-Lisp" *mon-buffer-mode-defaults*)))
;;; :TEST-ME (file-directory-p (cadr (assoc-string "eBay-Template" *mon-buffer-mode-defaults*)))
;;; :TEST-ME (file-directory-p (cadr (assoc-string "NAF-mode" *mon-buffer-mode-defaults*)))
;;; :TEST-ME (file-directory-p (cadr (assoc-string "Lisp" *mon-buffer-mode-defaults*)))

;;; ==============================
;;; :UNBIND-EM
;;; (mapc '(lambda (x) 
;;;  (progn (makunbound x) (unintern x))) *mon-dir-locals-alist-xrefs*)

