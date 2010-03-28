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
;; `*artist-naf-path*', `*brand-naf-path*', `*nef-scan-path*', `*nef-scan-nefs-path*',
;; `*nef-scan-nef2-path*', `*ebay-images-path*', `*ebay-images-bmp-path*',
;; `*ebay-images-jpg-path*', `*ebay-images-temp-path*', `*emacs2html-temp*'
;; `*bug-HG-path*', `*CL-scratch-path*'
;; `*buffer-mode-defaults*', `*mon-HG-root-path*', `*smith-poster-HG-path*' 
;; `*mon-record-current-directory*', `*mon-dir-locals-alist-xrefs*'
;; `*nef-scan-drive*', `*nef-scan-base-path*'
;;
;; ALIASES/ADVISED/SUBST'D:
;;
;; DEPRECATED:
;;
;; RENAMED:
;;
;; MOVED:
;; <Timestamp: Tuesday June 16, 2009 @ 04:21.18 PM - by MON KEY>
;; :DIRECTORY-PATH-VARS
;; `*nef-scan-path*', `*nef-scan-nefs-path*', `*nef-scan-nef2-path*', 
;; `*ebay-images-path*', `*ebay-images-bmp-path*', `*ebay-images-jpg-path*', 
;; `*ebay-images-temp-path*'
;;
;; :NAF-MODE-RELATED-PATH-VARS 
;; `*artist-naf-path*'    <- ebay-template-tools.el
;; `*brand-naf-path*'     <- ebay-template-tools.el
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
;;
;; NOTES:
;;
;; SNIPPETS:
;;
;; THIRD PARTY CODE:
;;
;; AUTHOR: MON KEY
;; MAINTAINER: MON KEY 
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
;;; :CREATED <Timestamp: #{2009-09-28T16:02:00-04:00Z}#{09401} - by MON>
(defvar *mon-dir-locals-alist-xrefs* nil
  "*Xrefing list of variables defined in file mon-dir-locals-alist.el\n
:EXAMPLE\n\(symbol-value '*mon-dir-locals-alist-xrefs*\)
\(symbol-value \(nth 3 *mon-dir-locals-alist-xrefs*\)\)\n
:SEE-ALSO `*naf-mode-xref-of-xrefs*'\n►►►.")
;;
(unless (bound-and-true-p *mon-dir-locals-alist-xrefs*)
  (setq *mon-dir-locals-alist-xrefs* 
        '(*artist-naf-path*
          *brand-naf-path*
          *ebay-images-bmp-path*
          *ebay-images-jpg-path*
          *ebay-images-lookup-path*
          *ebay-images-path*
          *ebay-images-temp-path*
          *emacs2html-temp*
          *nef-scan-base-path*
          *nef-scan-drive*
          *nef-scan-nef2-path*
          *nef-scan-nefs-path*
          *nef-scan-path*
          *nefs_photos_nefs-alist*
          *smith-poster-HG-path*
          *CL-scratch-path*
          *mon-record-current-directory*
          *bug-HG-path*
          *mon-HG-root-path*
          *buffer-mode-defaults*
          *mon-dir-locals-alist-xrefs*)))
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
:EXAMPLE\n(symbol-value '*mon-HG-root-path*)\n
:SEE-ALSO `mon-emacs-root', `*mon-emacsd*'.\n►►►")
;;
(unless (bound-and-true-p *mon-HG-root-path*)
  (let ((mhrp (nth 5 (assoc (cond (IS-MON-P-W32     1)
                                  (IS-BUG-P-REMOTE  4)
                                  (IS-BUG-P         3)
                                  (IS-MON-P-GNU     2))
                            *mon-emacsd*))))
    (setq *mon-HG-root-path* mhrp)))
;;         
;;; :TEST-ME  *mon-HG-root-path*
;;;(progn (makunbound '*mon-HG-root-path*) (unintern '*mon-HG-root-path*) )

;;; ==============================
;;; CREATED: <Timestamp: #{2009-08-11T13:42:35-04:00Z}#{09332} - by MON KEY>
(defvar *smith-poster-HG-path* nil
  "Path to the local Smith HG docs folder.\n
EXAMPLE:\n(symbol-value '*smith-poster-HG-path*)\n
:SEE-ALSO `*mon-HG-root-path*'.\n►►►")
;;
(unless (bound-and-true-p *smith-poster-HG-path* )
  (setq *smith-poster-HG-path* 
        (cond ((or IS-MON-P-W32 IS-BUG-P IS-BUG-P-REMOTE)
               (concat *mon-HG-root-path* *smith-poster-docs*))
              (IS-MON-P-GNU nil))))
;;
;;; :TEST-ME *smith-poster-HG-path*
;;;(progn (makunbound '*smith-poster-HG-path*) (unintern '*smith-poster-HG-path*) )
              
;;; ==============================
;;; :CREATED <Timestamp: Monday July 20, 2009 @ 02:59.11 PM - by MON KEY>
(defvar *CL-scratch-path* nil
  "Path for string CL-scratch files. Subdir of `mon-emacs-root'. 
This is used to keep files transferable across machines with Mercurial.\n
EXAMPLE:\n(symbol-value '*CL-scratch-path*)\n
:SEE-ALSO `common-lisp-hyperspec-root'.\n►►►")
;;
(unless (bound-and-true-p *CL-scratch-path*)
  (setq *CL-scratch-path*
        (if (or IS-MON-P-W32 IS-MON-P-GNU)
            (concat *mon-HG-root-path* 
                    (cadr (assoc 'the-CL-scratch-path *mon-misc-path-alist*))))))
;;
;;; :TEST-ME  *CL-scratch-path*
;;;(progn (makunbound '*CL-scratch-path*) (unintern '*CL-scratch-path*) )

;;; ==============================
;;; :CREATED <Timestamp: Wednesday July 15, 2009 @ 01:36.16 PM - by MON KEY>
(defvar *bug-HG-path* nil
  "Path var used to pass Bug paths to file in the HG repo.
Used primarily over ERC to exchange paths w32 network share paths.\n
:EXAMPLE\n(symbol-value '*bug-HG-path*)\n
:SEE-ALSO `mon--local-url-for-bug', `mon-local-url-for-bug',
`*mon-HG-root-path*'.\n►►►")
;;
(unless (bound-and-true-p *bug-HG-path*)
  (setq *bug-HG-path* (cond 
                       (IS-MON-P-W32 (nth 6 (assoc 3 *mon-emacsd* ))) ;; Get path from BUG alist.
                       ((or IS-BUG-P-REMOTE IS-BUG-P IS-MON-P-GNU) mon-emacs-root))))
;;
;;; :TEST-ME *bug-HG-path*
;;;(progn (makunbound '*bug-HG-path*) (unintern '*bug-HG-path*) )

;;; ==============================
;;; :CREATED <Timestamp: #{2009-08-11T18:12:47-04:00Z}#{09332} - by MON KEY>
(defvar *mon-record-current-directory* nil
  "Default filename to record buffer's current-directory.\n
EXAMPLE:\n(symbol-value '*mon-record-current-directory*)\n
:CALLED-BY `mon-save-current-directory-to-file'.\n
:SEE-ALSO .\n►►►")
;;
(when (not (bound-and-true-p *mon-record-current-directory*))
  (setq *mon-record-current-directory* (concat mon-emacs-root "/current-directory")))
;;
;;; :TEST-ME *mon-record-current-directory* 
;; 
;;;(progn (makunbound '*mon-record-current-directory*) 
;;;  (unintern '*mon-record-current-directory*) )

;;; ==============================
(defvar *emacs2html-temp* nil
  "Path to hold temp files transformed with `htmlfontify-buffer'.\n
:EXAMPLE\n(symbol-value '*emacs2html-temp*)\n
:CALLED-BY `mon-htmlfontify-region-to-firefox', 
`mon-htmlfontify-buffer-to-firefox'.
:SEE-ALSO `*mon-misc-path-alist*'.\n►►►")
;;
(when (not (bound-and-true-p *emacs2html-temp*))
  (let ((e2ht (concat mon-emacs-root 
                      (cadr  (assoc 'the-emacs2html-temp *mon-misc-path-alist*)))))
    (if (file-exists-p e2ht) (setq *emacs2html-temp* e2ht))))
;;
;;; :TEST-ME *emacs2html-temp*
;;;(progn (makunbound '*emacs2html-temp*) (unintern '*emacs2html-temp*) )

;;; ==============================
;;; :CREATED <Timestamp: Monday May 25, 2009 @ 03:18.01 PM - by MON KEY>
(defvar *artist-naf-path* nil
  "Path to Brand NAF folders on local systems.
Only relevant if specifed in `*mon-emacsd*'.\n
:EXAMPLE\n(symbol-value '*artist-naf-path*)\n
:CALLED-BY `naf-dired-artist-letter', `naf-explorer-brand', `naf-dired-image-dir'.\n
:SEE-ALSO .\n►►►")
;;
(when (not (bound-and-true-p *artist-naf-path*))
  (let ((anp (nth 7 (assoc (cond (IS-MON-P-W32    1)
                                 (IS-BUG-P-REMOTE 4) 
                                 (IS-BUG-P        3)
                                 (IS-MON-P-GNU    2)
                                 (t nil))          
                           *mon-emacsd*))))
    (setq *artist-naf-path* anp)))
;;
;;; :TEST-ME *artist-naf-path*
;;;(progn (makunbound '*artist-naf-path*) (unintern '*artist-naf-path*) )

;;; ==============================
;;; :NOTE Update these when if a path becomes available.
;;;  (IS-MON-P-GNU     )
;;;  (IS-BUG-P-REMOTE  )
;;; :CREATED <Timestamp: Monday May 25, 2009 @ 03:17.53 PM - by MON KEY>
(defvar *brand-naf-path* nil
  "Path to Brand NAF folders on local w32 systems.
Only relevant if specifed, else throws an error.\n
:EXAMPLE\n(symbol-value '*brand-naf-path*)\n
:CALLED-BY `naf-dired-brand-letter', `naf-explorer-brand', `naf-dired-image-dir'
:SEE-ALSO .\n►►►")
;;
(when (not (bound-and-true-p *brand-naf-path*))
  (setq *brand-naf-path*
	(cond ((or IS-MON-P-W32 IS-BUG-P)  
               (concat *artist-naf-path* "/BRANDS") ) 
              ;; These aren't Available on these machines.               
              ((or IS-BUG-P-REMOTE IS-MON-P-GNU) nil) ))) 
;;
;;; :TEST-ME *brand-naf-path*
;;;(progn (makunbound '*brand-naf-path*) (unintern '*brand-naf-path*) ) 

;;; ==============================
;;; :CREATED <Timestamp: #{2009-09-28T15:34:06-04:00Z}#{09401} - by MON>
(defvar *nef-scan-drive* nil
  "*Base drive or device level path for computing `*nef-scan-???* variables.
:EXAMPLE\n(symbol-value '*nef-scan-drive*)\n
:SEE-ALSO `*nef-scan-base-path*',`*nef-scan-path*',
`*nef-scan-nefs-path*',`*nef-scan-nef2-path*',
`*ebay-images-path*',`*ebay-images-bmp-path*',
`*ebay-images-jpg-path*',`*ebay-images-temp-path*'.\n►►►")
;;
(unless (bound-and-true-p  *nef-scan-drive*)
  (setq *nef-scan-drive*
        (let ((the-drv (cond (IS-MON-P-GNU "//")
                             (IS-MON-P-W32 (cadr (assoc 'the-nef-drv *mon-misc-path-alist*)))
                             (IS-BUG-P (cadr (assoc 'the-shr-prfx *mon-misc-path-alist*))))))
          (substring the-drv 0 -1))))

;;; ==============================
;;; :CREATED <Timestamp: #{2009-09-28T15:34:02-04:00Z}#{09401} - by MON>
(defvar *nef-scan-base-path* nil
  "*Base path sans drive or device for computing `*nef-scan-???* variables.
:EXAMPLE:\n(symbol-value '*nef-scan-base-path*)\n
:SEE-ALSO `*nef-scan-base-drive*'`*nef-scan-path*',
`*nef-scan-nefs-path*',`*nef-scan-nef2-path*',
`*ebay-images-path*',`*ebay-images-bmp-path*',
`*ebay-images-jpg-path*',`*ebay-images-temp-path*'.\n►►►")
;;
(unless *nef-scan-base-path*
  (setq *nef-scan-base-path*
        (concat *nef-scan-drive*
                (cond ((or IS-MON-P-W32 IS-BUG-P) "/")
                      (IS-MON-P-GNU (concat (nth 3 (assoc 2 *mon-emacsd*)) "/"))))))

;;; ==============================
;;; :NOTE Following Global Variables
;;; :CREATED <Timestamp: Wednesday May 06, 2009 @ 07:49.11 PM - by MON KEY>
;;; ==============================
;;; :MODIFICATIONS <Timestamp: #{2009-09-29T13:04:26-04:00Z}#{09402} - by MON KEY>
(defvar *nef-scan-path* nil
  "*User conditional path to ebay nef photo drive.\n
:EXAMPLE\n(symbol-value '*nef-scan-path*)\n
:CALLED-BY `mon-get-buffers-directories'.\n
:SEE-ALSO `*nef-scan-drive*', `*nef-scan-base-path*'
`*nef-scan-nefs-path*', `*nef-scan-nef2-path*', `*ebay-images-path*',
`*ebay-images-bmp-path*', `*ebay-images-jpg-path*', `*ebay-images-temp-path*',
`*ebay-images-lookup-path*'.\n►►►")
;;
(unless (bound-and-true-p *nef-scan-path*)
  (setq *nef-scan-path* (concat *nef-scan-base-path* "NEFS_PHOTOS")))

;;; ==============================
(defvar *nef-scan-nefs-path* nil
  "*User conditional path to ebay NEFS drive.\n
:EXAMPLE\n(symbol-value '*nef-scan-nefs-path*)\n
:NOTE The var `*nefs_photos_nefs-alist*' contains an association list of this
directory's paths as the directory doesnt' change that much.\n
:SEE-ALSO `*nef-scan-drive*', `*nef-scan-base-path*',
`*nef-scan-path*', `*nef-scan-nef2-path*', `*ebay-images-path*',
`*ebay-images-bmp-path*', `*ebay-images-jpg-path*', 
`*ebay-images-temp-path*' `*ebay-images-lookup-path*'.\n►►►")
;;
(when (not (bound-and-true-p *nef-scan-nefs-path*))
  (setq *nef-scan-nefs-path* 
	(concat *nef-scan-path* "/NEFS")))

;;; ==============================
;;; :TODO This needs to be a hash-table.
;;; :NOTE Var loaded from `mon-dir-utils.el' at loadtime.
;;; :MODIFICATIONS <Timestamp: Thursday May 28, 2009 @ 04:43.15 PM - by MON KEY>
(defvar *nefs_photos_nefs-alist* nil
  "*Directory contents as an alist for path `*nef-scan-nefs-path*'.
Alist generated with `mon-update-nef-photos-alist' at startup.
:NOTE using the ugly underscores to help distinguish in completion lists.\n
:EXAMPLE\n(symbol-velue '*nefs_photos_nefs-alist*)\n
:NOTE Bound loadtime with `mon-bind-nefs-photos-at-loadtime'.
:SEE-ALSO.\n►►►")

;;; ==============================
(defvar *nef-scan-nef2-path* nil
  "*User conditional path to ebay nef photo drive.\n
:EXAMPLE\n(symbol-value '*nef-scan-nef2-path*)\n
:SEE-ALSO `*nef-scan-drive*', `*nef-scan-base-path*',`*nef-scan-path*',
`*nef-scan-nefs-path*', `*ebay-images-path*', `*ebay-images-bmp-path*',
`*ebay-images-jpg-path*', `*ebay-images-temp-path*',
`*ebay-images-lookup-path*'.\n►►►")
;;
(when (not (bound-and-true-p *nef-scan-nef2-path*))
  (setq *nef-scan-nef2-path* 
	(concat *nef-scan-path* "/Nef_Drive2")))

;;; ==============================
(defvar *ebay-images-path* nil
  "*User conditional path to eBay image scans.\n
:EXAMPLE\n(symbol-value '*ebay-images-path*)\n
:SEE-ALSO `*nef-scan-drive*', `*nef-scan-base-path*',`*nef-scan-path*',
`*nef-scan-nefs-path*', `*nef-scan-nef2-path*',`*ebay-images-bmp-path*',
`*ebay-images-jpg-path*', `*ebay-images-temp-path*'
`*ebay-images-lookup-path*'.\n►►►")
;;
(when (not (bound-and-true-p *ebay-images-path*))
  (setq *ebay-images-path*
	(concat *nef-scan-nef2-path* "/EBAY")))

;;; ==============================
(defvar *ebay-images-bmp-path* nil
  "*User conditional path to ebay .bmp scans.\n
:EXAMPLE\n(symbol-value '*ebay-images-bmp-path*)\n
:CALLED-BY `mon-try-comp-dir', `naf-dired-image-dir'.\n
:SEE-ALSO `*nef-scan-drive*', `*nef-scan-base-path*',`*nef-scan-path*',
`*nef-scan-nefs-path*', `*nef-scan-nef2-path*', `*ebay-images-path*',
`*ebay-images-jpg-path*', `*ebay-images-temp-path*',
`*ebay-images-lookup-path*'.\n►►►")
;;
(when (not (bound-and-true-p *ebay-images-bmp-path*))
  (setq *ebay-images-bmp-path*
	(concat *ebay-images-path* "/BMP-Scans")))

;;; ==============================
(defvar *ebay-images-jpg-path* nil
  "*User conditional path to ebay scans converted to .jpg.\n
:EXAMPLE\n(symbol-value '*ebay-images-jpg-path*)\n
:CALLED-BY `naf-dired-image-dir'.\n
:SEE-ALSO `*nef-scan-drive*', `*nef-scan-base-path*',`*nef-scan-path*',
`*nef-scan-nefs-path*', `*nef-scan-nef2-path*', `*ebay-images-path*',
`*ebay-images-bmp-path*', `*ebay-images-temp-path*',
`*ebay-images-lookup-path*'.\n►►►")
;;
(when (not (bound-and-true-p *ebay-images-jpg-path*)) 
  (setq *ebay-images-jpg-path*
 	(concat *ebay-images-path* "/BIG-cropped-jpg")))

;;; ==============================
(defvar *ebay-images-temp-path* nil
  "*User conditional path to ebay temp files.\n
:EXAMPLE *ebay-images-temp-path*\n
Primarily used with: `mon-insert-ebay-dirs' and `mon-make-ebay-dir-list'.\n
:SEE-ALSO `*nef-scan-drive*', `*nef-scan-base-path*', `*nef-scan-path*',
`*nef-scan-nefs-path*', `*nef-scan-nef2-path*', `*ebay-images-path*',
`*ebay-images-bmp-path*', `*ebay-images-jpg-path*', 
`*ebay-images-lookup-path*'.\n►►►")
;;
(when (not (bound-and-true-p *ebay-images-temp-path*)) 
  (setq *ebay-images-temp-path*
	(concat *ebay-images-path* "/temp-batch")))
;;
;;; :TEST-ME *EBAY-IMAGES-TEMP-PATH*
;;; :TEST-ME (DIRED *EBAY-IMAGES-TEMP-PATH*)
;;; :TEST-ME (boundp '*ebay-images-temp-path*)
;;;(progn (makunbound '*ebay-images-temp-path*) (unintern '*ebay-images-temp-path*) )

;;; ==============================
;;; :NOTE If/when these get hashed set above path vars to:
;;;       `*nef-img-hash*' `*bmp-img-hash*' `*jpg-img-hash*'
(defvar *ebay-images-lookup-path* nil
  "*An alist of paths to examine when functions need to look for images.
alist keys are of the image-type as a string: \".nef\", \".jpg\", or \".bmp\".
For these purposes we don't want to be in the NEFS folder and assume a .nef 
source image is in the `*ebay-images-bmp-path*'.\n
:EXAMPLE\n(symbol-value '*ebay-images-lookup-path*)\n
:SEE-ALSO `*nef-scan-drive*', `*nef-scan-base-path*', `*nef-scan-path*',
`*nef-scan-nefs-path*', `*nef-scan-nef2-path*',
`*ebay-images-path*', `*ebay-images-jpg-path*'.\n►►►")
;;
(when (not (bound-and-true-p *ebay-images-lookup-path*)) 
  (setq *ebay-images-lookup-path*
	  '((".nef" *ebay-images-bmp-path* "BMP-Scans"); *nef-img-hash*)
	    (".jpg" *ebay-images-jpg-path* "BIG-cropped-jpg"); *jpg-img-hash*)
	    (".bmp" *ebay-images-bmp-path* "BMP-Scans")))); *bmp-img-hash*))))
;;
;;; :TEST-ME (boundp '*ebay-images-lookup-path*)
;;;(progn (makunbound '*ebay-images-lookup-path*)(unintern 'ebay-images-lookup-path*) )

;;; ==============================
(defvar *buffer-mode-defaults* nil
  "*Alist of defuault dir and file extensions to help to determine best path 
with conditional on buffers' current mode and/or extension.\n
:EXAMPLE\n*buffer-mode-defaults*\n\n►►►")
;;
(when (not (bound-and-true-p *buffer-mode-defaults*))
  (setq *buffer-mode-defaults*
        `(("eBay-Template"  ,*ebay-images-bmp-path* ".dbc")
          ("NAF-mode" ,*artist-naf-path* ".naf")
          ("Emacs-Lisp" ,(concat mon-emacs-root "/naf-mode") ".el")
          ("Lisp" ,*CL-scratch-path* ".lisp"))))
;;
;;; :NESETED-SYTLE-ALIST
;;; `(("eBay-Template"  (,*ebay-images-bmp-path* ".dbc"))
;;;  ("NAF-mode"  (,*artist-naf-path* ".naf"))
;;;  ("Emacs-Lisp" (,(concat mon-emacs-root "/naf-mode") ".el"))
;;;  ("Lisp" (,*CL-scratch-path* ".lisp"))))
;;
;;; :TEST-ME (ASSOC "EBAY-TEMPLATE" *BUFFER-MODE-DEFAULTS*)
;;; :TEST-ME (ASSOC "LISP" *BUFFER-MODE-DEFAULTS*)
;;; :TEST-ME (ASSOC "EMACS-LISP" *BUFFER-MODE-DEFAULTS*)
;;; :TEST-ME (assoc "NAF-mode" *buffer-mode-defaults*)
;;
;;;(progn (makunbound '*buffer-mode-defaults*) (unintern '*buffer-mode-defaults*) )

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
;;;    (expand-file-name *artist-naf-path*))
;;;   (IS-BUG-P  
;;;   (expand-file-name *artist-naf-path*))) 'naf-dir-no-clobber)
;;; ==============================

;;; ==============================
;;; :TEST-ME (dired *nef-scan-drive*)
;;; :TEST-ME (dired *nef-scan-base-path*)
;;; :TEST-ME (dired *nef-scan-path*)
;;; :TEST-ME (dired *nef-scan-nefs-path*)
;;; :TEST-ME (dired *nef-scan-nefs-path*)
;;; :TEST-ME (dired *nef-scan-nef2-path*)
;;; :TEST-ME (dired *ebay-images-path*)
;;; :TEST-ME (dired *ebay-images-bmp-path*)
;;; :TEST-ME (dired *ebay-images-jpg-path*)
;;; :TEST-ME (dired *ebay-images-temp-path*)
;;; :TEST-ME (dired *bug-HG-path*)
;;; :TEST-ME (dired *CL-scratch-path*)
;;; :TEST-ME (dired *mon-HG-root-path*)
;;; :TEST-ME (dired *smith-poster-HG-path*)
;;; :TEST-ME (dired (cadr (assoc "Emacs-Lisp" *buffer-mode-defaults*)))
;;; :TEST-ME (dired (cadr (assoc "eBay-Template" *buffer-mode-defaults*)))
;;; :TEST-ME (dired (cadr (assoc "NAF-mode" *buffer-mode-defaults*)))
;;; :TEST-ME (dired (cadr (assoc "Lisp" *buffer-mode-defaults*)))

;;; ==============================
;;; :UNBIND-EM
;;; (mapc '(lambda (x) 
;;;  (progn (makunbound x) (unintern x))) *mon-dir-locals-alist-xrefs*)

;;; ==============================
(provide 'mon-dir-locals-alist)
;;; ==============================

;;; ================================================================
;;; mon-dir-locals-alist.el ends here
;;; EOF
