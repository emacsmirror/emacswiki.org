;;; mon-dir-locals-alist.el --- MON global vars bound to commonly used local-site paths
;; -*- mode: EMACS-LISP; -*-

;;; ================================================================
;; Copyright © 2009-2011 MON KEY. All rights reserved.
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
;; `*mon-artist-naf-path*', `*mon-brand-naf-path*', `*mon-nef-scan-path*',
;; `*mon-nef-scan-nefs-path*', `*mon-nef-scan-nef2-path*',
;; `*mon-ebay-images-path*', `*mon-ebay-images-bmp-path*',
;; `*mon-ebay-images-jpg-path*', `*mon-ebay-images-temp-path*',
;; `*emacs2html-temp*', `*bug-HG-path*', `*mon-CL-scratch-path*',
;; `*mon-buffer-mode-defaults*', `*mon-HG-root-path*',
;; `*mon-smith-poster-HG-path*', `*mon-record-current-directory*',
;; `*mon-dir-locals-alist-xrefs*', `*mon-nef-scan-drive*',
;; `*mon-nef-scan-base-path*',
;;
;; GROUPS:
;; `mon-dir-locals'
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
;; `*mon-nef-scan-path*', `*mon-nef-scan-nefs-path*',
;; `*mon-nef-scan-nef2-path*', `*mon-ebay-images-path*',
;; `*mon-ebay-images-bmp-path*', `*mon-ebay-images-jpg-path*',
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
;; Copyright © 2009-2011 MON KEY 
;;; ==============================

;;; CODE:

 
(eval-when-compile (require 'cl))
;;
(unless (and (intern-soft "*IS-MON-OBARRAY*")
             (bound-and-true-p *IS-MON-OBARRAY*))
(setq *IS-MON-OBARRAY* (make-vector 17 nil)))

;;; ==============================
;;; :CHANGESET 2178
;;; :CREATED <Timestamp: #{2010-10-02T18:49:38-04:00Z}#{10396} - by MON KEY>
(defgroup mon-dir-locals nil
  "Site local directories needed in various `mon-*' functions.\n
:SEE-ALSO .\n►►►"
  :link '(url-link 
          :tag "\n:EMACSWIKI-FILE (URL `http://www.emacswiki.org/emacs/mon-dir-locals-alist.el')" 
          "http://www.emacswiki.org/emacs/mon-dir-locals-alist.el")
  :link '(emacs-library-link 
          :tag "\n:FILE mon-dir-locals-alist.el"
          "mon-dir-locals-alist.el")
  :group 'mon-base)

;;; ==============================
;;; :NOTE `IS-MON-SYSTEM-P' may be called already use a temporary symbol and
;;;       unintern it at EOF.
;;; :CHANGESET 1790
;;; :CREATED <Timestamp: #{2010-05-28T17:35:26-04:00Z}#{10215} - by MON KEY>
(defcustom *mon-bind-dir-locals-alist* nil 
  "Whether to bind variables defined in :FILE mon-dir-locals-alist.el.\n
When non-nil bind the provided directory/file variables.\n
Bound at loadtime when `IS-MON-SYSTEM-P'.\n
:SEE-ALSO .\n►►►"
  :type  'boolean
  :group 'mon-dir-locals)

;;; ==============================
;;; :CREATED <Timestamp: #{2009-09-28T16:02:00-04:00Z}#{09401} - by MON>
(defcustom *mon-dir-locals-alist-xrefs*
  '(*mon-artist-naf-path* *mon-brand-naf-path* *mon-ebay-images-bmp-path*
    *mon-ebay-images-jpg-path* *mon-ebay-images-lookup-path*
    *mon-ebay-images-path* *mon-ebay-images-temp-path* *emacs2html-temp*
    *mon-html-fontify-file-name-template* *mon-nef-scan-base-path*
    *mon-nef-scan-drive* *mon-nef-scan-nef2-path* *mon-nef-scan-nefs-path*
    *mon-nef-scan-path* *mon-nefs_photos_nefs-alist* *mon-smith-poster-HG-path*
    *mon-CL-scratch-path* *mon-record-current-directory* *bug-HG-path*
    *mon-HG-root-path* *mon-buffer-mode-defaults* *mon-dir-locals-alist-xrefs*)
  "Xrefing list of variables defined in file mon-dir-locals-alist.el\n
:EXAMPLE\n\(symbol-value '*mon-dir-locals-alist-xrefs*\)
\(symbol-value \(nth 3 *mon-dir-locals-alist-xrefs*\)\)\n
:SEE-ALSO `*mon-regexp-symbols-xrefs*', `*mon-default-loads-xrefs*',
`*mon-default-start-loads-xrefs*', `*mon-dir-locals-alist-xrefs*',
`*mon-testme-utils-xrefs*', `*mon-button-utils-xrefs*',
`*naf-mode-xref-of-xrefs*'.\n►►►"
  :type '(repeat symbol)
  :group 'mon-xrefs
  :group 'mon-dir-locals)
;;
;;; :TEST-ME  *mon-dir-locals-alist-xrefs*
;;; :TEST-ME (symbol-value (nth 3 *mon-dir-locals-alist-xrefs*))
;;
;;;(progn (makunbound '*mon-dir-locals-alist-xrefs*) 
;;;        (unintern "*mon-dir-locals-alist-xrefs*" obarray) )

;;; ==============================
;;; :CREATED <Timestamp: #{2009-08-11T13:42:20-04:00Z}#{09332} - by MON KEY>
(defcustom *mon-HG-root-path* nil
  "Path to the root path for HG repos.\n
:EXAMPLE\n\n(cons *mon-HG-root-path* \(file-directory-p *mon-HG-root-path*\)\)\n
:SEE-ALSO `*mon-emacs-root*', `*mon-emacsd*'.\n►►►"
  :type 'directory
  :group 'mon-dir-locals)
;;         
;;; :TEST-ME (file-directory-p *mon-HG-root-path*)
;;
;;;(progn (makunbound '*mon-HG-root-path*) (unintern "*mon-HG-root-path*" obarray) )

;;; ==============================
;;; CREATED: <Timestamp: #{2009-08-11T13:42:35-04:00Z}#{09332} - by MON KEY>
(defcustom *mon-smith-poster-HG-path* nil
  "Path to the local Smith HG docs folder.\n
:EXAMPLE\n\n(cons *mon-smith-poster-HG-path*
  \(file-directory-p *mon-smith-poster-HG-path*\)\)\n
When `IS-MON-SYSTEM-P' path is bound to the value returned by the lambda form held
in the cadr of the return value for the key 'the-smith-poster-docs-pth in
`*mon-misc-path-alist*', e.g:\n
 \(cadr \(assoc 'the-smith-poster-docs-pth *mon-misc-path-alist*\)\)\n
:SEE :FILE mon-site-local-defaults.el
:SUBDIR-OF `*mon-HG-root-path*'\n
:SEE-ALSO `*mon-emacsd*'.\n►►►"
  :type 'directory
  :group 'mon-dir-locals)
;;
;;
;;; :TEST-ME (file-directory-p *mon-smith-poster-HG-path*)
;;
;;;(progn (makunbound '*mon-smith-poster-HG-path*) (unintern "*mon-smith-poster-HG-path*" obarray) )

;;; ==============================
;;; :CREATED <Timestamp: Monday July 20, 2009 @ 02:59.11 PM - by MON KEY>
(defcustom *mon-CL-scratch-path* nil
  "Path for string CL-scratch files.\n
Subdir of `*mon-emacs-root*'.\n
This is used to keep files transferable across machines with Mercurial.\n
:EXAMPLE\n\n\(cons *mon-CL-scratch-path*
      \(file-directory-p *mon-CL-scratch-path*\)\)\n
:SEE-ALSO `common-lisp-hyperspec-root'.\n►►►"
  :type 'directory
  :group 'mon-dir-locals)
;;
;;; :TEST-ME (file-directory-p *mon-CL-scratch-path*)
;;
;;;(progn (makunbound '*mon-CL-scratch-path*) (unintern "*mon-CL-scratch-path*" obarray) )

;;; ==============================
;;; :CREATED <Timestamp: Wednesday July 15, 2009 @ 01:36.16 PM - by MON KEY>
(defcustom *bug-HG-path* nil
  "Path var used to pass Bug paths to file in the HG repo.\n
Used primarily over ERC to exchange paths w32 network share paths.\n
:EXAMPLE\n\n\(cons *bug-HG-path*
      \(file-directory-p *bug-HG-path*\)\)\n
:SEE-ALSO `mon--local-url-for-bug', `mon-get-local-url-for-bug',
`*mon-HG-root-path*'.\n►►►"
  :type 'directory
  :group 'mon-dir-locals)
;;
;;; :TEST-ME (file-directory-p *bug-HG-path*)
;;
;;;(progn (makunbound '*bug-HG-path*) (unintern "*bug-HG-path*" obarray) )

;;; ==============================
;;; :CREATED <Timestamp: #{2009-08-11T18:12:47-04:00Z}#{09332} - by MON KEY>
(defcustom *mon-record-current-directory* nil
  "Default filename to record buffer's current-directory.\n
EXAMPLE:\n\n\(cons *mon-record-current-directory* 
      \(file-exists-p *mon-record-current-directory*\)\)\n
:SUBDIR-OF `*mon-emacs-root*'\n
:CALLED-BY `mon-dir-save-current-to-file'.\n
:SEE-ALSO .\n►►►"
  :type 'file
  :group 'mon-dir-locals)
;;
;;; :TEST-ME (file-exists-p *mon-record-current-directory*)
;; 
;;;(progn (makunbound '*mon-record-current-directory*) 
;;;       (unintern "*mon-record-current-directory*" obarray) )

;;; ==============================
;;; :MODIFICATIONS <Timestamp: #{2010-04-02T12:11:11-04:00Z}#{10135} - by MON KEY>
(defcustom *emacs2html-temp* nil
  "*Path to hold temp files transformed with `htmlfontify-buffer'.\n
:EXAMPLE\n\n(cons *emacs2html-temp* (file-directory-p *emacs2html-temp*))\n
:ALIASED-BY `*mon-emacs2html-temp*'\n
:SEE-ALSO `mon-htmlfontify-region-to-firefox', `mon-htmlfontify-buffer-to-firefox', 
`mon-htmlfontify-dir-purge-on-quit',  `mon-html-fontify-generate-file-name'
`*mon-html-fontify-file-name-template*', `*emacs2html-temp*'.\n►►►"
  :type 'directory
  :group 'mon-dir-locals)
;;
;;; :TEST-ME (file-directory-p *emacs2html-temp*)
;;
;;;(progn (makunbound '*emacs2html-temp*) (unintern "*emacs2html-temp*" obarray) )

;;; ==============================
;;; :CREATED <Timestamp: #{2010-04-02T12:37:47-04:00Z}#{10135} - by MON KEY>
(defcustom *mon-html-fontify-file-name-template* nil
  "*Format string for `mon-html-fontify-generate-file-name'\n
Used to generate temporary file-names for `mon-htmlfontify-*' procedures.\n
:EXAMPLE\n\n\"%s/emacs2firefox-%d.html\"\n
:SEE-ALSO `mon-htmlfontify-region-to-firefox', `mon-htmlfontify-buffer-to-firefox', 
`mon-htmlfontify-dir-purge-on-quit',  `mon-html-fontify-generate-file-name'
`*mon-html-fontify-file-name-template*', `*emacs2html-temp*'.\n►►►"
  :type  'string
  :group 'mon-dir-locals)  
;;
;;;(progn (makunbound '*mon-html-fontify-file-name-template*)
;;;       (unintern   "*mon-html-fontify-file-name-template*" obarray) )  

;;; ==============================
;;; :CREATED <Timestamp: Monday May 25, 2009 @ 03:18.01 PM - by MON KEY>
(defcustom *mon-artist-naf-path* nil
  "*Path to Brand NAF folders on local systems.\n
:EXAMPLE\n\n\(cons *mon-artist-naf-path*
      \(file-directory-p *mon-artist-naf-path*\)\)\n
\(concat \"[c:]?/mnt/point\" \"/ARTISTS\"\)\n
:NOTE Binding only relevant if specifed in `*mon-emacsd*'.\n
When `*mon-emacsd*' is non-nil it is an alist of integer based keys which map
user/system names to local path names.\n
:SEE :FILE mon-site-local-defaults.el\n
:CALLED-BY `mon-dired-naf-artist-letter'
:CALLED-BY `mon-explorer-naf-brand'
:CALLED-BY `mon-dired-naf-image-dir'\n
:SEE-ALSO `*mon-brand-naf-path*'.\n►►►"
  :type  'directory
  :group 'mon-dir-locals)
;;
;;; :TEST-ME (file-directory-p *mon-artist-naf-path*)
;;
;;;(progn (makunbound '*mon-artist-naf-path*) (unintern "*mon-artist-naf-path*" obarray) )

;;; ==============================
;;; :NOTE Update these when if a path becomes available.
;;;  
;;;  (IS-BUG-P-REMOTE  )
;;; :CREATED <Timestamp: Monday May 25, 2009 @ 03:17.53 PM - by MON KEY>
(defcustom *mon-brand-naf-path* nil
  "*Path to Brand NAF folders on local systems.\n
Path relative to the `*mon-artist-naf-path*' directory.\n
:EXAMPLE\n\n\(cons *mon-brand-naf-path*
      \(file-directory-p *mon-brand-naf-path*\)\)\n
\(concat *mon-artist-naf-path* \"/BRANDS\"\)\n
:CALLED-BY `mon-dired-naf-brand-letter'
:CALLED-BY `mon-explorer-naf-brand'
:CALLED-BY `mon-dired-naf-image-dir'
:SEE-ALSO `*mon-artist-naf-path*'.\n►►►"
  :type  'directory
  :group 'mon-dir-locals)
;;
;;; :TEST-ME (file-directory-p *mon-brand-naf-path*)
;;
;;;(progn (makunbound '*mon-brand-naf-path*) (unintern "*mon-brand-naf-path*" obarray) ) 

;;; ==============================
;;; :CREATED <Timestamp: #{2009-09-28T15:34:06-04:00Z}#{09401} - by MON>
(defcustom *mon-nef-scan-drive* nil
  "*Base drive or device level path for computing local path variables.\n
Path relative to the alist key ``the-nef-drv'' in `*mon-misc-path-alist*'.\n
:SEE-ALSO `*mon-nef-scan-base-path*', `*mon-nef-scan-path*',
`*mon-nef-scan-nefs-path*', `*mon-nef-scan-nef2-path*',
`*mon-ebay-images-path*', `*mon-ebay-images-bmp-path*',
`*mon-ebay-images-jpg-path*', `*mon-ebay-images-temp-path*'.\n►►►"
  :type  'directory
  :group 'mon-dir-locals)

;;; ==============================
;;; :CREATED <Timestamp: #{2009-09-28T15:34:02-04:00Z}#{09401} - by MON>
(defcustom *mon-nef-scan-base-path* nil
  "*Base path sans drive or device for computing *mon-{...}-path* variables.\n
:EXAMPLE\n\n\(cons *mon-nef-scan-base-path*
      \(file-directory-p *mon-nef-scan-nefs-path*\)\)\n
:SUBDIR-OF `*mon-nef-scan-drive*'\n
:SEE-ALSO `*nef-scan-base-drive*'`*mon-nef-scan-path*',
`*mon-nef-scan-nefs-path*', `*mon-nef-scan-nef2-path*',
`*mon-ebay-images-path*', `*mon-ebay-images-bmp-path*',
`*mon-ebay-images-jpg-path*', `*mon-ebay-images-temp-path*'.\n►►►"
  :type  'directory
  :group 'mon-dir-locals)
;;
;;; :TEST-ME (file-directory-p (concat *mon-nef-scan-base-path* "\x41"))
;;
;;; (progn (makunbound '*mon-nef-scan-base-path*) (unintern "*mon-nef-scan-base-path*" obarray) )

;;; ==============================
;;; :NOTE Following Global Variables
;;; :CREATED <Timestamp: Wednesday May 06, 2009 @ 07:49.11 PM - by MON KEY>
;;; ==============================
;;; :MODIFICATIONS <Timestamp: #{2009-09-29T13:04:26-04:00Z}#{09402} - by MON KEY>
(defcustom *mon-nef-scan-path* nil
  "User conditional path to a local NEF archived photos drive/path.\n
:SUBDIR-OF `*mon-nef-scan-base-path*'\n
:SEE-ALSO `*mon-nef-scan-drive*', `*mon-nef-scan-base-path*',
`*mon-nef-scan-nefs-path*', `*mon-nef-scan-nef2-path*',
`*mon-ebay-images-path*', `*mon-ebay-images-bmp-path*',
`*mon-ebay-images-jpg-path*', `*mon-ebay-images-temp-path*',
`*mon-ebay-images-lookup-path*'.\n►►►"
  :type  'directory
  :group 'mon-dir-locals)
;;
;;; :TEST-ME (file-directory-p *mon-nef-scan-path*)
;;
;;; (progn (makunbound '*mon-nef-scan-path*) (unintern "*mon-nef-scan-path*" obarray) )

;;; ==============================
(defcustom *mon-nef-scan-nefs-path* nil
  "User conditional path to a local NEF Photos working drive/path.\n
:EXAMPLE\n\n\(cons *mon-nef-scan-nefs-path* 
      \(file-directory-p *mon-nef-scan-nefs-path*\)\)\n
:NOTE The var `*mon-nefs_photos_nefs-alist*' contains an association list of this
directory's paths as the directory doesnt' change that much.\n
:SEE-ALSO `*mon-nef-scan-drive*', `*mon-nef-scan-base-path*',
`*mon-nef-scan-path*', `*mon-nef-scan-nef2-path*', `*mon-ebay-images-path*',
`*mon-ebay-images-bmp-path*', `*mon-ebay-images-jpg-path*',
`*mon-ebay-images-temp-path*' `*mon-ebay-images-lookup-path*'.\n►►►"
  :type  'directory
  :group 'mon-dir-locals)
;;
;;; :TEST-ME (file-directory-p *mon-nef-scan-nefs-path*)
;;
;;; (progn (makunbound '*mon-nef-scan-path*) (unintern "*mon-nef-scan-path*" obarray) )

;;; ==============================
;;; :TODO This needs to be a hash-table.
;;; :NOTE Var loaded from `mon-dir-utils.el' at loadtime.
;;; :MODIFICATIONS <Timestamp: Thursday May 28, 2009 @ 04:43.15 PM - by MON KEY>
(defvar *mon-nefs_photos_nefs-alist* nil
  "*Directory contents as an alist for path `*mon-nef-scan-nefs-path*'.
A list generated with `mon-dir-nef-update-photos-alist' and bound at loadtime
with `mon-bind-nefs-photos-at-loadtime'.\n
:NOTE Ugly underscores in symbol-name to help distinguish in completion lists.\n
:SEE-ALSO `*mon-nef-scan-drive*', `*mon-nef-scan-base-path*',
`*mon-nef-scan-path*', `*mon-nef-scan-nefs-path*', `*mon-ebay-images-path*',
`*mon-ebay-images-bmp-path*', `*mon-ebay-images-jpg-path*',
`*mon-ebay-images-temp-path*', `*mon-ebay-images-lookup-path*'.\n►►►")

;;; ==============================
(defcustom *mon-nef-scan-nef2-path* nil
  "User conditional path to a local NEF2 drive/path.\n
:EXAMPLE\n\n\(cons *mon-nef-scan-nef2-path* 
      \(file-directory-p *mon-nef-scan-nef2-path*\)\)\n
:SUBDIR-OF `*mon-nef-scan-nefs-path*'\n
:SEE-ALSO `*mon-nef-scan-drive*', `*mon-nef-scan-base-path*',
`*mon-nef-scan-path*', `*mon-nef-scan-nefs-path*', `*mon-ebay-images-path*',
`*mon-ebay-images-bmp-path*', `*mon-ebay-images-jpg-path*',
`*mon-ebay-images-temp-path*', `*mon-ebay-images-lookup-path*'.\n►►►"
  :type  'directory
  :group 'mon-dir-locals)
;;
;;; :TEST-ME (file-directory-p *mon-nef-scan-nef2-path*)
;;
;;; (progn (makunbound '*mon-nef-scan-nef2-path*) (unintern "*mon-nef-scan-nef2-path*" obarray) )

;;; ==============================
(defcustom *mon-ebay-images-path* nil
  "User conditional path to a local eBay image scans path.\n
:EXAMPLE\n\n\(cons *mon-ebay-images-path*
      \(file-directory-p *mon-ebay-images-path*\)\)\n
\(concat *mon-nef-scan-nef2-path* \"/EBAY\"\)\n
:SUBDIR-OF `*mon-nef-scan-nef2-path*'\n
:SEE-ALSO `*mon-nef-scan-drive*', `*mon-nef-scan-base-path*',
`*mon-nef-scan-path*', `*mon-nef-scan-nefs-path*', `*mon-nef-scan-nef2-path*',
`*mon-ebay-images-bmp-path*', `*mon-ebay-images-jpg-path*',
`*mon-ebay-images-temp-path*', `*mon-ebay-images-lookup-path*'.\n►►►"
  :type  'directory
  :group 'mon-dir-locals)
;;
;;; :TEST-ME (file-directory-p *mon-ebay-images-path*)
;;
;;; (progn (makunbound '*mon-ebay-images-path*) (unintern "*mon-ebay-images-path*" obarray) )

;;; ==============================
(defcustom *mon-ebay-images-bmp-path* nil
  "User conditional path to a ebay .bmp scans path.\n
:EXAMPLE\n\n\(cons *mon-ebay-images-bmp-path*
      \(file-directory-p *mon-ebay-images-bmp-path*\)\)\n
\(concat *mon-ebay-images-path* \"/BMP-Scans\"\)\n
:SUBDIR-OF `*mon-ebay-images-path*'\n
:CALLED-BY `mon-dir-try-comp', `mon-dired-naf-image-dir'.\n
:SEE-ALSO `*mon-nef-scan-drive*', `*mon-nef-scan-base-path*',
`*mon-nef-scan-path*', `*mon-nef-scan-nefs-path*', `*mon-nef-scan-nef2-path*',
`*mon-ebay-images-path*', `*mon-ebay-images-jpg-path*',
`*mon-ebay-images-temp-path*', `*mon-ebay-images-lookup-path*'.\n►►►"
  :type  'directory
  :group 'mon-dir-locals)
;;
;;; :TEST-ME (file-directory-p *mon-ebay-images-bmp-path*)
;;
;;; (progn (makunbound '*mon-ebay-images-bmp-path*) (unintern "*mon-ebay-images-bmp-path*" obarray) )

;;; ==============================
(defcustom *mon-ebay-images-jpg-path* nil
  "User conditional path to ebay scans converted to files with a .jpg extension.\n
:EXAMPLE\n\n\(cons *mon-ebay-images-jpg-path*
      \(file-directory-p *mon-ebay-images-jpg-path*\)\)\n
\(concat *mon-ebay-images-path* \"/BIG-cropped-jpg\"\)\n
:SUBDIR-OF `*mon-ebay-images-path*'.\n
:CALLED-BY `mon-dired-naf-image-dir'.\n
:SEE-ALSO `*mon-nef-scan-drive*', `*mon-nef-scan-base-path*',
`*mon-nef-scan-path*', `*mon-nef-scan-nefs-path*', `*mon-nef-scan-nef2-path*',
`*mon-ebay-images-path*', `*mon-ebay-images-bmp-path*',
`*mon-ebay-images-temp-path*', `*mon-ebay-images-lookup-path*'.\n►►►"
  :type  'directory
  :group 'mon-dir-locals)
;;
;;; :TEST-ME (file-directory-p *mon-ebay-images-jpg-path*)
;;
;;; (progn (makunbound '*mon-ebay-images-jpg-path*) (unintern "*mon-ebay-images-jpg-path*" obarray) )

;;; ==============================
(defcustom *mon-ebay-images-temp-path* nil
  "User conditional path to local ebay temp files path.\n
:EXAMPLE\n\n\(cons *mon-ebay-images-temp-path*
      \(file-directory-p *mon-ebay-images-temp-path*\)\)\n
\(concat *mon-ebay-images-path* \"/temp-batch\"\)\n
:SUBDIR-OF `*mon-ebay-images-path*'\n
:CALLED-BY `mon-insert-ebay-dirs' and `mon-make-ebay-dir-list'.\n
:SEE-ALSO `*mon-nef-scan-drive*', `*mon-nef-scan-base-path*',
`*mon-nef-scan-path*', `*mon-nef-scan-nefs-path*', `*mon-nef-scan-nef2-path*',
`*mon-ebay-images-path*', `*mon-ebay-images-bmp-path*',
`*mon-ebay-images-jpg-path*', `*mon-ebay-images-lookup-path*'.\n►►►"
  :type  'directory
  :group 'mon-dir-locals)
;;
;;; :TEST-ME (file-directory-p *mon-ebay-images-temp-path*)
;;
;;;(progn (makunbound '*mon-ebay-images-temp-path*) (unintern "*mon-ebay-images-temp-path*" obarray) )

;;; ==============================
;;; :NOTE If/when these get hashed set above path vars to:
;;;       `*mon-nef-img-hash*' `*mon-bmp-img-hash*' `*mon-jpg-img-hash*'
(defcustom *mon-ebay-images-lookup-path* nil
  "A list of default pathnames to examine where `mon-*' look for images.\n
Elements of list have the format:
 ( <IMG-EXTENSION> <BASE-PATH> <SUBDIR> )
<IMG-EXTENSION> is a string naming an image extension (include leading \".\"), e.g:\n
 \".nef\", \".jpg\", or \".bmp\"\n
<BASE-PATH> is a string naming an existing `directory-file-name'.\n
<SUBDIR> is a string naming an existing sub directory file-name of <BASE-PATH>.\n
:EXAMPLE\n\n\(file-directory-p 
 \(symbol-value \(cadr \(assoc-string \".nef\" *mon-ebay-images-lookup-path*\)\)\)\)\n
;=> \(\(\".nef\" *mon-ebay-images-bmp-path* \"BMP-Scans\"\)
     \(\".jpg\" *mon-ebay-images-jpg-path* \"BIG-cropped-jpg\"\)
     \(\".jpeg\" *mon-ebay-images-jpg-path* \"BIG-cropped-jpg\"\)
     \(\".bmp\" *mon-ebay-images-bmp-path* \"BMP-Scans\"\)\)\n
:NOTE For our purposes we don't want to be in the NEFS folder and assume a \".nef\" 
source image is in the `*mon-ebay-images-bmp-path*'.\n
For use when populating variables holding hashtables, e.g:\n
 `*mon-bmp-img-hash*', `*mon-jpg-img-hash*', `*mon-nef-img-hash*'\n
:SEE-ALSO `mon-image-verify-type', `*mon-nef-scan-drive*',
`*mon-nef-scan-base-path*', `*mon-nef-scan-path*', `*mon-nef-scan-nefs-path*',
`*mon-nef-scan-nef2-path*', `*mon-ebay-images-path*',
`*mon-ebay-images-jpg-path*'.\n►►►"
  :type  '(repeat (list string symbol string))
  :group 'mon-dir-locals)

;;
;;; :TEST-ME (file-directory-p (symbol-value (cadr (assoc ".nef" *mon-ebay-images-lookup-path*))))
;;
;;;(progn (makunbound '*mon-ebay-images-lookup-path*)(unintern "*ebay-images-lookup-path*" obarray) )

;;; ==============================
(defcustom *mon-buffer-mode-defaults* nil
  "A list of defuault directory and file extensions.\n
Used by `mon-*' functions to help to determine best path with conditional on
buffers' current mode and/or extension.\n
Elements of list have the format:\n
 \(<MODE-NAME> <DIRECTORY> <EXTENSION>\)\n
<MODE-NAME> is a string naming a `major-mode'\n
<DIRECTORY> is a string naming a directory and satisfying `file-directory-p'\n
<EXTENSION> is a string naming a file extension including the `.'\n
For example the following backqute template would expand to valid valus:\n
 `\(\(\"eBay-Template\"  ,*mon-ebay-images-bmp-path* \".dbc\"\)
   \(\"NAF-mode\"       ,*mon-artist-naf-path*      \".naf\"\)
   \(\"Emacs-Lisp\"     ,*mon-naf-mode-root*        \".el\"\)
   \(\"Lisp\"           ,*mon-CL-scratch-path*      \".lisp\"\)\)\n
:EXAMPLE\n\n\(assoc-string \"NAF-mode\" *mon-buffer-mode-defaults*\)\n
:SEE-ALSO `*mon-ebay-images-bmp-path*', `*mon-artist-naf-path*',
`*mon-CL-scratch-path*', `*mon-emacs-root*'.\n►►►"
  :type  '(repeat (list 
                   string    ;; :tag "mode-name" 
                   directory 
                   string )) ;; :tag "extension"
  :group 'mon-dir-locals)
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
;;;(progn (makunbound '*mon-buffer-mode-defaults*) (unintern "*mon-buffer-mode-defaults*" obarray) )

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
;;; :NOTE We `*mon-bind-dir-locals-alist*' is now a defcustom so we no longer
;;; remove the symbol `*mon-bind-dir-locals-alist*' if it isn't needed.
;;; :CHANGESET 2180 <Timestamp: #{2010-10-04T13:17:13-04:00Z}#{10401} - by MON KEY>
;;; :CHANGESET 1789
;;; :CREATED <Timestamp: #{2010-05-28T17:32:45-04:00Z}#{10215} - by MON KEY>
;;; (eval-when (compile load eval)
;;;   (when (and (intern-soft "*mon-bind-dir-locals-alist*")
;;;              (null *mon-bind-dir-locals-alist*))
;;;     (progn (makunbound '*mon-bind-dir-locals-alist*)
;;;            (unintern "*mon-bind-dir-locals-alist*" obarray))))


;;; ==============================
;;; :NOTE Everything below sets up mon-local requirements per the custom forms above:
(eval-when (compile load eval)
  (when (and (intern-soft "IS-MON-SYSTEM-P" obarray)  ;; *IS-MON-OBARRAY*
             (bound-and-true-p IS-MON-SYSTEM-P))
    (unless (and (intern-soft "*mon-bind-dir-locals-alist*" obarray) ;; *IS-MON-OBARRAY*
                 (bound-and-true-p *mon-bind-dir-locals-alist*))
      (setq *mon-bind-dir-locals-alist* t))))
;;
(unless (and (and (intern-soft "*mon-HG-root-path*" obarray) ;; *IS-MON-OBARRAY*
                  (bound-and-true-p *mon-HG-root-path*))
             (not (and (intern-soft "*mon-bind-dir-locals-alist*" obarray) ;; *IS-MON-OBARRAY*
                       (bound-and-true-p *mon-bind-dir-locals-alist*))))
  (when (and (intern-soft "IS-MON-SYSTEM-P" obarray) ;; *IS-MON-OBARRAY*
             (bound-and-true-p IS-MON-SYSTEM-P))  
    (let ((mhrp (nth 5 (assoc (cond (IS-MON-P-W32     1)
                                    (IS-BUG-P-REMOTE  4)
                                    (IS-BUG-P         3)
                                    (IS-MON-P-GNU     2))
                              *mon-emacsd*))))
      (setq *mon-HG-root-path* mhrp))))
;;
(unless (and (and (intern-soft "*mon-smith-poster-HG-path*" obarray) ;; *IS-MON-OBARRAY*
                  (bound-and-true-p *mon-smith-poster-HG-path*))
             (not (and (intern-soft "*mon-bind-dir-locals-alist*" obarray)  ;; *IS-MON-OBARRAY*
                       (bound-and-true-p *mon-bind-dir-locals-alist*))))
  (when (and (intern-soft "IS-MON-SYSTEM-P" obarray) ;; *IS-MON-OBARRAY*
             (bound-and-true-p IS-MON-SYSTEM-P))
    (setq *mon-smith-poster-HG-path* 
          (funcall 
           (cadr (assoc 'the-smith-poster-docs-pth *mon-misc-path-alist*))))))
;;
(unless (and (and (intern-soft "*mon-CL-scratch-path*" obarray)
                  (bound-and-true-p *mon-CL-scratch-path*))
             (not (and (intern-soft "*mon-bind-dir-locals-alist*" obarray) 
                       (bound-and-true-p *mon-bind-dir-locals-alist*))))
  (when (intern-soft "IS-MON-SYSTEM-P" obarray)
    (setq *mon-CL-scratch-path*
          (when IS-MON-P
            (let ((clscrtch (concat *mon-HG-root-path* 
                                    (cadr (assoc 'the-CL-path 
                                                 *mon-misc-path-alist*)))))
              (expand-file-name "CL-NOTES" clscrtch))))))
;;
(when (intern-soft "IS-MON-SYSTEM-P" obarray)              ;; *IS-MON-OBARRAY*
  (unless (and (and (intern-soft "*bug-HG-path*" obarray) ;; *IS-MON-OBARRAY*
                    (bound-and-true-p *bug-HG-path*))
               (not (and (intern-soft "*mon-bind-dir-locals-alist*" obarray)  ;; *IS-MON-OBARRAY*
                         (bound-and-true-p *mon-bind-dir-locals-alist*))))
    (setq *bug-HG-path* 
          (cond (IS-MON-P-W32 (nth 6 (assoc 3 *mon-emacsd* ))) ;; Get path from BUG alist.
                ((or IS-BUG-P-REMOTE IS-BUG-P IS-MON-P-GNU) *mon-emacs-root*)))))
;;
(unless (and (and (intern-soft "*mon-record-current-directory*" obarray) ;; *IS-MON-OBARRAY*
                  (bound-and-true-p *mon-record-current-directory*))
             (not (and (intern-soft "*mon-bind-dir-locals-alist*" obarray) 
                       (bound-and-true-p *mon-bind-dir-locals-alist*))))
  (when (intern-soft "IS-MON-SYSTEM-P" obarray)   ;; *IS-MON-OBARRAY*
    (setq *mon-record-current-directory* 
          (concat *mon-emacs-root* "/current-directory"))))
;;
;;; :NOTE `*emacs2html-temp*' is provided in :FILE htmlfontify.el and _may_
;;;        already be `bound-and-true-p'. So, it is better to bind it directly
;;;        with setq.
;;; :WAS (unless (bound-and-true-p *emacs2html-temp*)
(unless  (and (and (intern-soft "*emacs2html-temp*" obarray) ;; *IS-MON-OBARRAY*
                   (bound-and-true-p *emacs2html-temp*))
              (not (and (intern-soft "*mon-bind-dir-locals-alist*" obarray) ;; *IS-MON-OBARRAY*
                        (bound-and-true-p *mon-bind-dir-locals-alist*))))
  (let ((e2ht (cond ((and (intern-soft "IS-MON-SYSTEM-P" obarray) ;; *IS-MON-OBARRAY*
                          (bound-and-true-p IS-MON-SYSTEM-P))
                     (concat *mon-local-emacs-temp-dir* "/emacs2html-temp"))
                    (t (concat 
                        (if (string-match-p "/$" user-emacs-directory)
                            ;; :NOTE Knock of the trailing / e.g. when:
                            ;;       `user-emacs-directory' => "~/.emacs.d/"
                            (replace-regexp-in-string "/$" "" user-emacs-directory t)
                          user-emacs-directory)
                        "/emacs2html-temp")))))
    (if (file-exists-p e2ht)
        (setq *emacs2html-temp* e2ht)
      ;; Shouldn't write to file system when `IS-NOT-A-MON-SYSTEM'
      (if (and (intern-soft "IS-MON-SYSTEM-P" obarray) ;; *IS-MON-OBARRAY*
               (bound-and-true-p IS-MON-SYSTEM-P))
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
(unless (and (and (intern-soft "*mon-html-fontify-file-name-template*" obarray) ;; *IS-MON-OBARRAY*
                  (bound-and-true-p *mon-html-fontify-file-name-template*))
             (not (and (intern-soft "*mon-bind-dir-locals-alist*" obarray)
                       (bound-and-true-p *mon-bind-dir-locals-alist*))))
  (when (and (intern-soft "IS-MON-SYSTEM-P" obarray) 
             (bound-and-true-p IS-MON-SYSTEM-P))
    (setq *mon-html-fontify-file-name-template* "%s/emacs2firefox-%d.html")))
;;
(unless (and (and (intern-soft "*mon-artist-naf-path*" obarray) ;; *IS-MON-OBARRAY*
                  (bound-and-true-p *mon-artist-naf-path*))
             (and (intern-soft "*mon-emacsd*" obarray) ;; *IS-MON-OBARRAY*
                  (bound-and-true-p *mon-emacsd*))
             (not (and (intern-soft "*mon-bind-dir-locals-alist*" obarray)  ;; *IS-MON-OBARRAY*
                       (bound-and-true-p *mon-bind-dir-locals-alist*))))
  (when (and (intern-soft "IS-MON-SYSTEM-P" obarray) ;; *IS-MON-OBARRAY*
             (bound-and-true-p IS-MON-SYSTEM-P))
    (let ((anp (nth 7 (assoc 
                       ;; :NOTE Conditionals are fall through cases, order is important.
                       (cond ((and (intern-soft "IS-MON-P-W32" obarray) ;; *IS-MON-OBARRAY*
                                   (bound-and-true-p IS-MON-P-W32))    1)
                             ((and (intern-soft "IS-BUG-P-REMOTE" obarray) ;; *IS-MON-OBARRAY*
                                   (bound-and-true-p IS-BUG-P-REMOTE)) 4) 
                             ((and (intern-soft "IS-BUG-P" obarray)     ;; *IS-MON-OBARRAY*
                                   (bound-and-true-p IS-BUG-P))        3)
                             ((and (intern-soft "IS-MON-P-GNU" obarray) ;; *IS-MON-OBARRAY* 
                                   (bound-and-true-p IS-MON-P-GNU))    2)
                             (t nil))
                       *mon-emacsd*))))
      (setq *mon-artist-naf-path* (concat anp "/ARTISTS")))))
;;
(unless (and (and (intern-soft "*mon-brand-naf-path*" obarray)  ;; *IS-MON-OBARRAY*
                  (bound-and-true-p *mon-brand-naf-path*))    
             (not (and (intern-soft "*mon-bind-dir-locals-alist*" obarray)  ;; *IS-MON-OBARRAY*
                       (bound-and-true-p *mon-bind-dir-locals-alist*))))
  (when (and (intern-soft "IS-MON-SYSTEM-P" obarray)  ;; *IS-MON-OBARRAY*
             (bound-and-true-p IS-MON-SYSTEM-P))
    (setq *mon-brand-naf-path*
          (cond ((or (and (intern-soft "IS-MON-P-W32" obarray) ;; *IS-MON-OBARRAY*
                          (bound-and-true-p IS-MON-P-W32 ))
                     (and (intern-soft "IS-BUG-P" obarray) ;; *IS-MON-OBARRAY*
                          (bound-and-true-p IS-BUG-P)))
                 (concat *mon-artist-naf-path* "/BRANDS"))
                ((and (intern-soft "IS-MON-P-GNU" obarray) ;; *IS-MON-OBARRAY*
                      (bound-and-true-p IS-MON-P-GNU))
                 (concat (directory-file-name 
                          (file-name-directory *mon-artist-naf-path*)) "/BRANDS"))
                ;; Not available on following machine kept here for completeness:
                ((and (intern-soft "IS-BUG-P-REMOTE" obarray)         ;; *IS-MON-OBARRAY*
                      (bound-and-true-p IS-BUG-P-REMOTE)) nil)))))
;;
(unless (and (and (intern-soft "*mon-nef-scan-drive*" obarray) ;; *IS-MON-OBARRAY*
                  (bound-and-true-p *mon-nef-scan-drive*))
             (not (and (intern-soft "*mon-bind-dir-locals-alist*" obarray) ;; *IS-MON-OBARRAY*
                       (bound-and-true-p *mon-bind-dir-locals-alist*))))
  (when (and (intern-soft "IS-MON-SYSTEM-P" obarray) ;; *IS-MON-OBARRAY*
             (bound-and-true-p IS-MON-SYSTEM-P))
    (setq *mon-nef-scan-drive* (cadr (assoc 'the-nef-drv *mon-misc-path-alist*)))))
;;
(unless (and (and (intern-soft "*mon-nef-scan-base-path*" obarray) ;; *IS-MON-OBARRAY*
                  (bound-and-true-p *mon-nef-scan-base-path*))
             (not (and (intern-soft "*mon-bind-dir-locals-alist*" obarray)  ;; *IS-MON-OBARRAY*
                       (bound-and-true-p *mon-bind-dir-locals-alist*))))
  (when (and (intern-soft "IS-MON-SYSTEM-P" obarray)  ;; *IS-MON-OBARRAY*
             (bound-and-true-p IS-MON-SYSTEM-P))
    ;; :NOTE Filesystems, dirs, mounts etc. change periodically across
    ;;  OS's so we must do this silliness.
    (setq *mon-nef-scan-base-path*  
          (concat *mon-nef-scan-drive*
                  (cond ((and (intern-soft "IS-W32-P" obarray) ;; *IS-MON-OBARRAY*
                              (bound-and-true-p IS-W32-P)) "/")
                        ( ;; :WAS (IS-MON-P-GNU (concat (nth 3 (assoc 2 *mon-emacsd*)) "/"))))
                         (and (intern-soft "IS-MON-P-GNU" obarray)  ;; *IS-MON-OBARRAY*
                              (bound-and-true-p IS-MON-P-GNU)) "-"))))))
;;
(unless (and (and (intern-soft "*mon-nef-scan-path*" obarray) ;; *IS-MON-OBARRAY*
                  (bound-and-true-p *mon-nef-scan-path*))
             (not (and (intern-soft "*mon-bind-dir-locals-alist*" obarray) ;; *IS-MON-OBARRAY*
                       (bound-and-true-p *mon-bind-dir-locals-alist*))))
  (when (intern-soft "IS-MON-SYSTEM-P" obarray) ;; *IS-MON-OBARRAY*
    (setq *mon-nef-scan-path* 
          (cond (IS-W32-P (concat *mon-nef-scan-base-path* "NEFS_PHOTOS"))
                (IS-MON-P-GNU (concat *mon-nef-scan-base-path* "\x42"))))))
;;
(unless (and (and (intern-soft "*mon-nef-scan-nefs-path*" obarray)
                  (bound-and-true-p *mon-nef-scan-nefs-path*))
             (not (and (intern-soft "*mon-bind-dir-locals-alist*" obarray) 
                       (bound-and-true-p *mon-bind-dir-locals-alist*))))
  (when (intern-soft "IS-MON-SYSTEM-P" obarray)
    (setq *mon-nef-scan-nefs-path* 
          (cond (IS-W32-P (concat *mon-nef-scan-base-path* "NEF_Drive2"))
                (IS-MON-P-GNU (concat *mon-nef-scan-base-path* "\x42"))))))
;;
(unless (and (and (intern-soft "*mon-nef-scan-nef2-path*" obarray)
                  (bound-and-true-p *mon-nef-scan-nef2-path*))
             (not (and (intern-soft "*mon-bind-dir-locals-alist*" obarray) 
                       (bound-and-true-p *mon-bind-dir-locals-alist*))))
  (when (and (intern-soft "IS-MON-SYSTEM-P" obarray) ;; *IS-MON-OBARRAY*
             (bound-and-true-p IS-MON-SYSTEM-P))
    ;; :NOTE This needs to happen this way because of occasional filesystem shifts.
    (setq *mon-nef-scan-nef2-path*
          (if IS-MON-P-GNU 
              (concat (substring *mon-nef-scan-nefs-path* 0 -1) "\x41")
            *mon-nef-scan-nefs-path*))))
;;
(unless (and (and (intern-soft "*mon-ebay-images-path*")
                  (bound-and-true-p *mon-ebay-images-path*))
             (not (and (intern-soft "*mon-bind-dir-locals-alist*") 
                       (bound-and-true-p *mon-bind-dir-locals-alist*))))
  (when (and (intern-soft "IS-MON-SYSTEM-P")     ;; *IS-MON-OBARRAY*
             (bound-and-true-p IS-MON-SYSTEM-P))
    (setq *mon-ebay-images-path* (concat *mon-nef-scan-nef2-path* "/EBAY"))))
;;
(unless (and (and (intern-soft "*mon-ebay-images-bmp-path*" obarray)
                  (bound-and-true-p *mon-ebay-images-bmp-path*))
             (not (and (intern-soft "*mon-bind-dir-locals-alist*" obarray) 
                       (bound-and-true-p *mon-bind-dir-locals-alist*))))
  (when (and (intern-soft "IS-MON-SYSTEM-P" obarray) 
             (bound-and-true-p IS-MON-SYSTEM-P))  ;; *IS-MON-OBARRAY*
    (setq *mon-ebay-images-bmp-path* (concat *mon-ebay-images-path* "/BMP-Scans"))))
;;
(unless (and (and (intern-soft "*mon-ebay-images-jpg-path*" obarray)
                  (bound-and-true-p *mon-ebay-images-jpg-path*))
             (not (and (intern-soft "*mon-bind-dir-locals-alist*" obarray) 
                       (bound-and-true-p *mon-bind-dir-locals-alist*))))
  (when (and (intern-soft "IS-MON-SYSTEM-P" obarray) ;; *IS-MON-OBARRAY*
             (bound-and-true-p IS-MON-SYSTEM-P))
    (setq *mon-ebay-images-jpg-path* (concat *mon-ebay-images-path* "/BIG-cropped-jpg"))))
;;
(unless (and (and (intern-soft "*mon-ebay-images-temp-path*" obarray)
                  (bound-and-true-p *mon-ebay-images-temp-path*))
             (not (and (intern-soft "*mon-bind-dir-locals-alist*" obarray) 
                       (bound-and-true-p *mon-bind-dir-locals-alist*))))
  (when (and (intern-soft "IS-MON-SYSTEM-P" obarray) ;; *IS-MON-OBARRAY*
             (bound-and-true-p IS-MON-SYSTEM-P))
    (setq *mon-ebay-images-temp-path* (concat *mon-ebay-images-path* "/temp-batch"))))
;;
(unless (and (and (intern-soft "*mon-ebay-images-lookup-path*" obarray)
                  (bound-and-true-p *mon-ebay-images-lookup-path*))
             (not (and (intern-soft "*mon-bind-dir-locals-alist*" obarray) 
                       (bound-and-true-p *mon-bind-dir-locals-alist*))))
  (when (and (intern-soft "IS-MON-SYSTEM-P" obarray) ;; *IS-MON-OBARRAY*
             (bound-and-true-p IS-MON-SYSTEM-P))
    (setq *mon-ebay-images-lookup-path*
	  '((".nef" *mon-ebay-images-bmp-path*  "BMP-Scans") ;; *mon-nef-img-hash*)
            (".bmp" *mon-ebay-images-bmp-path*  "BMP-Scans")
            (".tiff" *mon-ebay-images-bmp-path* "BMP-Scans")
            (".pnm" *mon-ebay-images-bmp-path*  "BMP-Scans")
            (".ppm" *mon-ebay-images-bmp-path*  "BMP-Scans")
            (".pgm" *mon-ebay-images-bmp-path*  "BMP-Scans")
            (".pbm" *mon-ebay-images-bmp-path*  "BMP-Scans")
            (".png" *mon-ebay-images-jpg-path*  "BIG-cropped-jpg")
	    (".jpg" *mon-ebay-images-jpg-path*  "BIG-cropped-jpg") ;; *mon-jpg-img-hash*)
	    (".jpeg" *mon-ebay-images-jpg-path* "BIG-cropped-jpg") ;; *mon-jpg-img-hash*)
	    )))) ;; *mon-bmp-img-hash*))))
;;
(unless (and (and (intern-soft "*mon-buffer-mode-defaults*" obarray)
                  (bound-and-true-p *mon-buffer-mode-defaults*))
             (not (and (intern-soft "*mon-bind-dir-locals-alist*" obarray) 
                       (bound-and-true-p *mon-bind-dir-locals-alist*))))
  (when (and (intern-soft "IS-MON-SYSTEM-P" obarray) ;; *IS-MON-OBARRAY*
             (bound-and-true-p IS-MON-SYSTEM-P))
    (setq *mon-buffer-mode-defaults*
          `(("eBay-Template"  ,*mon-ebay-images-bmp-path* ".dbc")
            ("NAF-mode"       ,*mon-artist-naf-path*      ".naf")
            ("Emacs-Lisp"     ,*mon-naf-mode-root*        ".el")
            ("Lisp"           ,*mon-CL-scratch-path*      ".lisp")))))
;;
;; Inform custom that her kittens await.
(dolist (in-bnd '("*mon-ebay-images-lookup-path*"
                  "*mon-ebay-images-temp-path*"
                  "*mon-ebay-images-jpg-path*"
                  "*mon-ebay-images-bmp-path*"
                  "*mon-ebay-images-path*"
                  "*mon-nef-scan-nef2-path*"
                  "*mon-nefs_photos_nefs-alist*"
                  "*mon-nef-scan-nefs-path*"
                  "*mon-nef-scan-path*"
                  "*mon-nef-scan-base-path*"
                  "*mon-nef-scan-drive*"
                  "*mon-brand-naf-path*"
                  "*mon-artist-naf-path*"
                  "*mon-html-fontify-file-name-template*"
                  "*mon-record-current-directory*"
                  "*bug-HG-path*"
                  "*mon-CL-scratch-path*"
                  "*mon-smith-poster-HG-path*"
                  "*emacs2html-temp*        "
                  "*mon-buffer-mode-defaults*"
                  "*mon-HG-root-path*"))
  (let* ((is-int (intern-soft in-bnd)))
    (when (and is-int (funcall #'boundp (quote is-int)) is-int)
      (funcall #'custom-note-var-changed is-int)))) ;;(quote is-int) )))
               

;;; ==============================
;;
;;,---- :UNCOMMENT-BELOW-TO-TEST
;;| (file-directory-p *mon-nef-scan-path*)
;;| (file-directory-p *mon-nef-scan-nefs-path*)
;;| (file-directory-p *mon-nef-scan-nefs-path*)
;;| (file-directory-p *mon-nef-scan-nef2-path*)
;;| (file-directory-p *mon-ebay-images-path*)
;;| (file-directory-p *mon-ebay-images-bmp-path*)
;;| (file-directory-p *mon-ebay-images-jpg-path*)
;;| (file-directory-p *mon-ebay-images-temp-path*)
;;| (file-directory-p *bug-HG-path*)
;;| (file-directory-p *mon-CL-scratch-path*)
;;| (file-directory-p *mon-HG-root-path*)
;;| (file-directory-p *mon-smith-poster-HG-path*)
;;| (file-directory-p (cadr (assoc-string "Emacs-Lisp" *mon-buffer-mode-defaults*)))
;;| (file-directory-p (cadr (assoc-string "eBay-Template" *mon-buffer-mode-defaults*)))
;;| (file-directory-p (cadr (assoc-string "NAF-mode" *mon-buffer-mode-defaults*)))
;;| (file-directory-p (cadr (assoc-string "Lisp" *mon-buffer-mode-defaults*)))
;;| 
;;| ;; :UNBIND-EM
;;|  (mapc '(lambda (x) 
;;|   (progn (makunbound x) (unintern x obarray))) *mon-dir-locals-alist-xrefs*)
;;`----
;;
;;; ==============================

;;; ==============================
(provide 'mon-dir-locals-alist)
;;; ==============================

 
;; Local Variables:
;; mode: EMACS-LISP
;; coding: utf-8
;; generated-autoload-file: "./mon-loaddefs.el"
;; End:

;;; ================================================================
;;; mon-dir-locals-alist.el ends here
;;; EOF
