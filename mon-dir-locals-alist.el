;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*- mode: EMACS-LISP; -*-
;;; this is mon-dir-locals-alist.el
;;; ================================================================
;;; DESCRIPTION:
;;; mon-dir-locals-alist provides global vars bound to commonly used paths
;;; with user/site conditionals to path. Additionally some subr's for quick load
;;; from alists of some directory contents.
;;;
;;; FUNCTIONS:►►►
;;;
;;; FUNCTIONS:◄◄◄
;;;
;;; CONSTANTS or VARIABLES:
;;; `*artist-naf-path*', `*brand-naf-path*', `*nef-scan-path*', `*nef-scan-nefs-path*',
;;; `*nef-scan-nef2-path*', `*ebay-images-path*', `*ebay-images-bmp-path*',
;;; `*ebay-images-jpg-path*', `*ebay-images-temp-path*', `*emacs2html-temp*'
;;; `*bug-HG-path*', `*mon-timestamp-cond-alist*', `*CL-scratch-path*'
;;; `*buffer-mode-defaults*', `*mon-HG-root-path*', `*smith-poster-HG-path*' 
;;; `*mon-record-current-directory*'
;;;
;;; MACROS:
;;;
;;; SUBST or ALIASES:
;;;
;;; DEPRECATED, OR RENAMED:
;;;
;;; MOVED:
;;; <Timestamp: Tuesday June 16, 2009 @ 04:21.18 PM - by MON KEY>
;;; Directory-Path-Vars path-vars moved here from:
;;; `*nef-scan-path*', `*nef-scan-nefs-path*', `*nef-scan-nef2-path*', 
;;; `*ebay-images-path*', `*ebay-images-bmp-path*', `*ebay-images-jpg-path*', 
;;; `*ebay-images-temp-path*'
;;;
;;; Naf-mode related Dired path-vars: 
;;; `*artist-naf-path*'   <- ./ebay-template-tools.el
;;; `*brand-naf-path*'    <- ./ebay-template-tools.el
;;; 
;;; `*mon-timestamp-cond-alist*' -> ./mon-time-utils.el
;;;
;;; REQUIRES:
;;;
;;; REQUIRED-BY: 
;;; "./mon-dir-utils.el"
;;; "./ebay-template-mode.el"
;;; "./mon-rename-image-utils.el"
;;; "./smith-poster-utils.el" 
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
;;; FILE CREATED:
;;; <Timestamp: Thursday May 28, 2009 @ 04:43.15 PM - by MON-KEY>
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
;;; Copyright (C) 2009 MON KEY 
;;; ===========================
;;; CODE:
;;;

;;; ==============================
;;; CREATED: <Timestamp: #{2009-08-11T13:42:20-04:00Z}#{09332} - by MON KEY>
(defvar *mon-HG-root-path*  nil
"Path to the root path for HG repos.\n
EXAMPLE:\n*mon-HG-root-path*\n
See also; `mon-emacs-root', `*mon-emacsd*'.")
;;
(when (not (bound-and-true-p *mon-HG-root-path*))
  (let ((mhrp (nth 5 (assoc (cond (IS-MON-P-W32     1)
                                  (IS-BUG-P-REMOTE  4)
                                  (IS-BUG-P         3)
                                  (IS-MON-P-GNU     2))
                            *mon-emacsd*))))
    (setq *mon-HG-root-path* mhrp)))
         
;;;test-me; *mon-HG-root-path*
;;;(progn (makunbound '*mon-HG-root-path*) (unintern '*mon-HG-root-path*))

;;; ==============================
;;; CREATED: <Timestamp: #{2009-08-11T13:42:35-04:00Z}#{09332} - by MON KEY>
(defvar *smith-poster-HG-path* nil
  "Path to the local Smith HG docs folder.\n
EXAMPLE:\n*smith-poster-HG-path*\n
See also; `*mon-HG-root-path*'.")
;;
(when (not (bound-and-true-p *smith-poster-HG-path* ))
  (setq *smith-poster-HG-path* 
        (cond ((or IS-MON-P-W32 IS-BUG-P IS-BUG-P-REMOTE)
               (concat *mon-HG-root-path* *smith-poster-docs*))
              (IS-MON-P-GNU nil))))

;;;test-me; *smith-poster-HG-path*
;;;(progn (makunbound '*smith-poster-HG-path*) (unintern '*smith-poster-HG-path*))
              
;;; ==============================
;;; CREATED: <Timestamp: Monday July 20, 2009 @ 02:59.11 PM - by MON KEY>
(defvar *CL-scratch-path* nil
  "Path for string CL-scratch files. Subdir of `mon-emacs-root'. 
This is used to keep files transferable across machines with Mercurial.\n
EXAMPLE:\n*CL-scratch-path* .")
;;
(when (not (bound-and-true-p *CL-scratch-path*))
  (setq *CL-scratch-path*
        (if (or IS-MON-P-W32 IS-MON-P-GNU)
             (concat mon-emacs-root (cadr (assoc 'the-CL-scratch-path *mon-misc-path-alist*))))))

;;;test-me; *CL-scratch-path*
;;;(progn (makunbound '*CL-scratch-path*) (unintern '*CL-scratch-path*))

;;; ==============================
;;; CREATED: <Timestamp: Wednesday July 15, 2009 @ 01:36.16 PM - by MON KEY>
(defvar *bug-HG-path* nil
  "Path var used to pass Bug paths to file in the HG repo.
Used primarily over ERC to exchange paths w32 network share paths.\n
EXAMPLE:\n*bug-HG-path*\n
See also; `mon--local-url-for-bug', `mon-local-url-for-bug', `*mon-HG-root-path*'.")
;;
(when (not (bound-and-true-p *bug-HG-path*))
  (setq *bug-HG-path* (cond 
                       (IS-MON-P-W32 (nth 6 (assoc 3 *mon-emacsd* ))) ;get path from BUG alist
                       ((or IS-BUG-P-REMOTE IS-BUG-P IS-MON-P-GNU) mon-emacs-root))))

;;;test-me; *bug-HG-path*
;;;(progn (makunbound '*bug-HG-path*) (unintern '*bug-HG-path*))

;;; ==============================
;;; CREATED: <Timestamp: #{2009-08-11T18:12:47-04:00Z}#{09332} - by MON KEY>
(defvar *mon-record-current-directory* nil
  "Default filename to record buffer's current-directory.
CALLED-BY: `mon-save-current-directory-to-file'.")
;;
(when (not (bound-and-true-p *mon-record-current-directory*))
  (setq *mon-record-current-directory* (concat mon-emacs-root "/current-directory")))
;;;test-me; *mon-record-current-directory* 
 
;;;(progn (makunbound '*mon-record-current-directory*) 
;;;  (unintern '*mon-record-current-directory*))

;;; ==============================
(defvar *emacs2html-temp* nil
  "Path to hold temp files transformed with `htmlfontify-buffer'.
CALLED-BY: `mon-htmlfontify-region-to-firefox', 
`mon-htmlfontify-buffer-to-firefox'.")
;;
(when (not (bound-and-true-p *emacs2html-temp*))
  (let ((e2ht (concat mon-emacs-root 
                      (cadr  (assoc 'the-emacs2html-temp *mon-misc-path-alist*)))))
    (if (file-exists-p e2ht) (setq *emacs2html-temp* e2ht))))

;;;test-me; *emacs2html-temp*
;;;(progn (makunbound '*emacs2html-temp*) (unintern '*emacs2html-temp*))

;;; ==============================
;;; CREATED: <Timestamp: Monday May 25, 2009 @ 03:18.01 PM - by MON KEY>
(defvar *artist-naf-path* nil
  "Path to Brand NAF folders on local w32 systems.
Only relevant if specifed, else throws an error.\n
EXAMPLE:\n*artist-naf-path*\n
CALLED-BY: `naf-dired-artist-letter', `naf-explorer-brand',`naf-dired-image-dir'.")
;;
(when (not (bound-and-true-p *artist-naf-path*))
  (let ((anp (nth 7 (assoc (cond (IS-MON-P-W32    1)
                                 (IS-BUG-P-REMOTE 4) 
                                 (IS-BUG-P        3)
                                 (IS-MON-P-GNU    2)
                                 (t nil))          
                           *mon-emacsd*))))
    (setq *artist-naf-path* anp)))

;;;test-me; *artist-naf-path*
;;;(progn (makunbound '*artist-naf-path*) (unintern '*artist-naf-path*))

;;; ==============================
;;; NOTE: Update these when if a path becomes available.
;;;  (IS-MON-P-GNU     )
;;;  (IS-BUG-P-REMOTE  )
;;; CREATED: <Timestamp: Monday May 25, 2009 @ 03:17.53 PM - by MON KEY>
(defvar *brand-naf-path* nil
  "Path to Brand NAF folders on local w32 systems.
Only relevant if specifed, else throws an error.\n
EXAMPLE:\n*brand-naf-path*\n
CALLED-BY: `naf-dired-brand-letter', `naf-explorer-brand',`naf-dired-image-dir'.")
;;
(when (not (bound-and-true-p *brand-naf-path*))
  (setq *brand-naf-path*
	(cond ((or IS-MON-P-W32 IS-BUG-P)  
               (concat *artist-naf-path* "/BRANDS") ) 
              ;; These aren't Available on these machines.               
              ((or IS-BUG-P-REMOTE IS-MON-P-GNU) nil) ))) 

;;;test-me; *brand-naf-path*
;;;(progn (makunbound '*brand-naf-path*) (unintern '*brand-naf-path*)) 

;;; ==============================
;;; Following Global Variables:
;;; CREATED: <Timestamp: Wednesday May 06, 2009 @ 07:49.11 PM - by MON KEY>
;;; ==============================
(defvar *nef-scan-path* nil
  "User conditional path to ebay nef photo drive.\n
EXAMPLE:\n*nef-scan-path*\n
CALLED-BY: `mon-get-buffers-directories'.\n
See also; `*nef-scan-nefs-path*', `*nef-scan-nef2-path*', `*ebay-images-path*',
`*ebay-images-bmp-path*', `*ebay-images-jpg-path*', `*ebay-images-temp-path*',
`*ebay-images-lookup-path*'.")
;;
(when (not (bound-and-true-p *nef-scan-path*))
  (let ((nsp (concat (cadr (assoc (cond (IS-MON-P-W32 'the-nef-drv) 
                                        (IS-BUG-P 'the-shr-prfx)) 
                                  *mon-misc-path-alist*))
                     "NEFS_PHOTOS")))
    (setq *nef-scan-path* nsp)))

;;;test-me; *nef-scan-path*
;;;(progn (makunbound '*nef-scan-path*)(unintern '*nef-scan-path*))

;;; ==============================
(defvar *nef-scan-nefs-path* nil
  "User conditional path to ebay NEFS drive.\n
EXAMPLE:\n*nef-scan-nefs-path*\n
NOTE: The var `*nefs_photos_nefs-alist*' contains an association list of this
directory's paths as the directory doesnt' change that much.
See also; `*nef-scan-path*', `*nef-scan-nef2-path*', `*ebay-images-path*',
`*ebay-images-bmp-path*', `*ebay-images-jpg-path*', `*ebay-images-temp-path*'
`*ebay-images-lookup-path*'.")
;;
(when (not (bound-and-true-p *nef-scan-nefs-path*))
  (setq *nef-scan-nefs-path* 
	(concat *nef-scan-path* "/NEFS")))

;;; ==============================
;;; TODO: This needs to be a hash-table.
;;; LAST-UPDATED: <Timestamp: Thursday May 28, 2009 @ 04:43.15 PM - by MON KEY>
(defvar *nefs_photos_nefs-alist* nil
  "Directory contents as an alist for path `*nef-scan-nefs-path*'.
Alist generated with `mon-update-nef-photos-alist' at startup.
NOTE: using the ugly underscores to help distinguish in completion lists.\n
EXAMPLE:\n*nefs_photos_nefs-alist* .")

;;; Var loaded from `mon-dir-utils.el'.

;;; ==============================
(defvar *nef-scan-nef2-path* nil
  "User conditional path to ebay nef photo drive.\n
EXAMPLE:\n*nef-scan-nef2-path*\n
See also; `*nef-scan-path*', `*nef-scan-nefs-path*', `*ebay-images-path*',
`*ebay-images-bmp-path*', `*ebay-images-jpg-path*', `*ebay-images-temp-path*',
`*ebay-images-lookup-path*'.")
;;
(when (not (bound-and-true-p *nef-scan-nef2-path*))
  (setq *nef-scan-nef2-path* 
	(concat *nef-scan-path* "/Nef_Drive2")))

;;; ==============================
(defvar *ebay-images-path* nil
  "User conditional path to eBay image scans.\n
EXAMPLE:\n*ebay-images-path*\n
See also; `*nef-scan-path*', `*nef-scan-nefs-path*', `*nef-scan-nef2-path*',
`*ebay-images-bmp-path*', `*ebay-images-jpg-path*', `*ebay-images-temp-path*'
`*ebay-images-lookup-path*'.")
;;
(when (not (bound-and-true-p *ebay-images-path*))
  (setq *ebay-images-path*
	(concat *nef-scan-nef2-path* "/EBAY")))

;;; ==============================
(defvar *ebay-images-bmp-path* nil
  "User conditional path to ebay .bmp scans.\n
EXAMPLE:\n*ebay-images-bmp-path*\n
CALLED-BY: `mon-try-comp-dir', `naf-dired-image-dir'.\n
See also; `*nef-scan-path*', `*nef-scan-nefs-path*', `*nef-scan-nef2-path*',
`*ebay-images-path*', `*ebay-images-jpg-path*', `*ebay-images-temp-path*',
`*ebay-images-lookup-path*'.")
;;
(when (not (bound-and-true-p *ebay-images-bmp-path*))
  (setq *ebay-images-bmp-path*
	(concat *ebay-images-path* "/BMP-Scans")))

;;; ==============================
(defvar *ebay-images-jpg-path* nil
  "User conditional path to ebay scans converted to .jpg.\n
EXAMPLE:\n*ebay-images-jpg-path*\n
CALLED-BY: `naf-dired-image-dir'.\n
See also; `*nef-scan-path*', `*nef-scan-nefs-path*', `*nef-scan-nef2-path*',
`*ebay-images-path*', `*ebay-images-bmp-path*', `*ebay-images-temp-path*',
`*ebay-images-lookup-path*'.")
;;
(when (not (bound-and-true-p *ebay-images-jpg-path*)) 
  (setq *ebay-images-jpg-path*
 	(concat *ebay-images-path* "/BIG-cropped-jpg")))

;;; ==============================
(defvar *ebay-images-temp-path* nil
  "User conditional path to ebay temp files.\n
EXAMPLE:*ebay-images-temp-path*\n
Primarily used with: `mon-insert-ebay-dirs' and `mon-make-ebay-dir-list'.\n
See also; `*nef-scan-path*', `*nef-scan-nefs-path*', `*nef-scan-nef2-path*',
`*ebay-images-path*', `*ebay-images-bmp-path*', `*ebay-images-jpg-path*', 
`*ebay-images-lookup-path*'.")
;;
(when (not (bound-and-true-p *ebay-images-temp-path*)) 
  (setq *ebay-images-temp-path*
	(concat *ebay-images-path* "/temp-batch")))

;;;test-me; *ebay-images-temp-path*
;;;test-me;(dired *ebay-images-temp-path*)
;;;test-me;(boundp *ebay-images-temp-path*)
;;;(progn (makunbound '*ebay-images-temp-path*) (uninter '*ebay-images-temp-path*))

;;; ==============================
(defvar *ebay-images-lookup-path* nil
  "alist of paths to examine when functions need to look for images.
alist keys are of the image-type as a string: \".nef\", \".jpg\", or \".bmp\".
For these purposes we don't want to be in the NEFS folder and assume a .nef 
source image is in the `*ebay-images-bmp-path*'.\n
EXAMPLE:\n*ebay-images-lookup-path*\n
See also; `*nef-scan-path*', `*nef-scan-nefs-path*', `*nef-scan-nef2-path*',
`*ebay-images-path*', `*ebay-images-jpg-path*'.")
;;
(when (not (bound-and-true-p *ebay-images-lookup-path*)) 
  (setq *ebay-images-lookup-path*
	  '((".nef" *ebay-images-bmp-path* "BMP-Scans"); *nef-img-hash*)
	    (".jpg" *ebay-images-jpg-path* "BIG-cropped-jpg"); *jpg-img-hash*)
	    (".bmp" *ebay-images-bmp-path* "BMP-Scans")))); *bmp-img-hash*))))

;;; If/when these get hashed set above path vars to:
;;; *nef-img-hash* *bmp-img-hash* *jpg-img-hash*

;;;test-me;(boundp '*ebay-images-lookup-path*)
;;;(progn (makunbound '*ebay-images-lookup-path*)(unintern 'ebay-images-lookup-path*))

;;; ==============================
(defvar *buffer-mode-defaults* nil
  "Alist of defuault dir and file extensions to help to determine best path 
with conditional on buffers' current mode and/or extension.\n
EXAMPLE:\n*buffer-mode-defaults* .")
;;
(when (not (bound-and-true-p *buffer-mode-defaults*))
  (setq *buffer-mode-defaults*
        `(("eBay-Template"  ,*ebay-images-bmp-path* ".dbc")
          ("NAF-mode" ,*artist-naf-path* ".naf")
          ("Emacs-Lisp" ,(concat mon-emacs-root "/naf-mode") ".el")
          ("Lisp" ,*CL-scratch-path* ".lisp"))))

;;; NESETED SYTLE ALIST:
;;; `(("eBay-Template"  (,*ebay-images-bmp-path* ".dbc"))
;;;  ("NAF-mode"  (,*artist-naf-path* ".naf"))
;;;  ("Emacs-Lisp" (,(concat mon-emacs-root "/naf-mode") ".el"))
;;;  ("Lisp" (,*CL-scratch-path* ".lisp"))))

;;;test-me;(assoc "eBay-Template" *buffer-mode-defaults*)
;;;test-me;(assoc "Lisp" *buffer-mode-defaults*)
;;;test-me;(assoc "Emacs-Lisp" *buffer-mode-defaults*)
;;;test-me;(assoc "NAF-mode" *buffer-mode-defaults*)

;;;(progn (makunbound '*buffer-mode-defaults*) (unintern '*buffer-mode-defaults*))

;;; ==============================

;;; ==============================
;;; CREATED: <Timestamp: Monday April 06, 2009 @ 10:56.24 AM - by MON KEY>
;;; Trying to set a delete to trash dir. variable in naf sub-dirs.
;;; Still-testing. I think maybe w/ version 23.1 dir-locals Suck!
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
;;;test-me;(dired *nef-scan-nefs-path*)
;;;test-me;(dired *nef-scan-path*)
;;;test-me;(dired *nef-scan-nefs-path*)
;;;test-me;(dired *nef-scan-nef2-path*)
;;;test-me;(dired *ebay-images-path*)
;;;test-me;(dired *ebay-images-bmp-path*)
;;;test-me;(dired *ebay-images-jpg-path*)
;;;test-me;(dired *ebay-images-temp-path*)
;;;test-me;(dired *bug-HG-path*)
;;;test-me;(dired *CL-scratch-path*)
;;;test-me;(dired *mon-HG-root-path*)
;;;test-me;(dired *smith-poster-HG-path*)
;;;test-me;(dired (cadr (assoc "Emacs-Lisp" *buffer-mode-defaults*)))
;;;test-me;(dired (cadr (assoc "eBay-Template" *buffer-mode-defaults*)))
;;;test-me;(dired (cadr (assoc "NAF-mode" *buffer-mode-defaults*)))
;;;test-me;(dired (cadr (assoc "Lisp" *buffer-mode-defaults*)))

;;; ==============================
;;; Unbind 'em:
;; (mapc 
;;  '(lambda (x) (progn (makunbound x) (unintern x)))
;;  '(*mon-HG-root-path*
;;    *nef-scan-path*
;;    *nef-scan-nefs-path*
;;    *nef-scan-nef2-path*
;;    *ebay-images-path*
;;    *ebay-images-bmp-path*
;;    *ebay-images-jpg-path*
;;    *ebay-images-temp-path*
;;    *nef-scan-path*
;;    *buffer-mode-defaults*
;;    *CL-scratch-path*
;;    *bug-HG-path*
;;    *mon-HG-root-path* 
;;    *smith-poster-HG-path*
;;    ))

;;; ==============================

;;; ==============================
(provide 'mon-dir-locals-alist)
;;; ==============================

;;; ================================================================
;;; mon-dir-locals-alist.el ends here
;;; EOF
