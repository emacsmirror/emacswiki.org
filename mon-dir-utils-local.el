;;; mon-dir-utils-local.el --- local directory and file utilities 
;; -*- mode: EMACS-LISP; -*-

;;; ================================================================
;; Copyright © 2010 MON KEY. All rights reserved.
;;; ================================================================

;; FILENAME: mon-dir-utils-local.el
;; AUTHOR: MON KEY
;; MAINTAINER: MON KEY
;; CREATED: 2010-11-04T19:38:33-04:00Z
;; VERSION: 1.0.0
;; COMPATIBILITY: Emacs23.*
;; KEYWORDS: local, lisp

;;; ================================================================

;;; COMMENTARY: 

;; =================================================================
;; DESCRIPTION:
;; mon-dir-utils-local provides local directory and file utilities 
;; Mostly specific to MON site
;;
;; FUNCTIONS:►►►
;; `mon-file-map-elisp-fileset', `mon-dir-save-current',
;; `mon-explorer-naf-artist', `mon-explorer-naf-brand',
;; `mon-dired-naf-artist-letter' `mon-dired-naf-brand-letter',
;; `mon-dired-naf-image-dir', `mon-dir-nef-update-photos-alist',
;; `mon-bind-nefs-photos-at-loadtime', `mon-dir-nef-ranges',
;; `mon-dir-nef-name-to-head', `mon-dir-nef-remove-if-empty',
;; `mon-dir-nef-find-dups', `mon-dir-nef-conc-dups', `mon-dir-nef-converge',
;; `mon-dir-nef-conc-ranges', `mon-dir-nef-keep-3', `mon-dir-nef-big',
;; `mon-dired-nef-dir', `mon-dir-hash-images' `mon-dir-hashed-complete',
;; `mon-dir-try-comp', `mon-file-path-for-bug', `mon--local-url-for-bug',
;; `mon-get-local-url-for-bug',
;;
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
;; FACES:
;;
;; VARIABLES:
;;`*mon-img-hash*', `*mon-nef-img-hash*', `*mon-bmp-img-hash*',
;;`*mon-jpg-img-hash*',
;;
;; GROUPS:
;;
;; ALIASED/ADVISED/SUBST'D:
;;
;; DEPRECATED:
;;
;; RENAMED:
;; `*img-hash*'                               -> `*mon-img-hash*'
;; `mon-dir-nef-rmv-empt'                     -> `mon-dir-nef-remove-if-empty'
;; `mon-dir-nef-alist'                        -> `mon-dir-nef-name-to-head'
;;
;; MOVED:
;; `mon-dired-nef-dir'                        <- mon-dir-utils.el
;; `mon-dir-nef-big'                          <- mon-dir-utils.el
;; `mon-dir-nef-converge'                     <- mon-dir-utils.el
;; `mon-dir-nef-keep-3'                       <- mon-dir-utils.el
;; `mon-dir-nef-name-to-head'                 <- mon-dir-utils.el
;; `mon-dir-nef-conc-ranges'                  <- mon-dir-utils.el
;; `mon-dir-nef-ranges'                       <- mon-dir-utils.el
;; `mon-dir-nef-conc-dups'                    <- mon-dir-utils.el
;; `mon-dir-nef-find-dups'                    <- mon-dir-utils.el
;; `mon-dir-nef-remove-if-empty'              <- mon-dir-utils.el
;; `mon--local-url-for-bug'                   <- mon-dir-utils.el
;; `mon-get-local-url-for-bug'                <- mon-dir-utils.el
;; `mon-explorer-naf-artist'                  <- mon-dir-utils.el
;; `mon-explorer-naf-brand'                   <- mon-dir-utils.el
;; `mon-dired-naf-artist-letter'              <- mon-dir-utils.el
;; `mon-dired-naf-brand-letter'               <- mon-dir-utils.el
;; `mon-dir-save-current'                     <- mon-dir-utils.el
;; `mon-dir-save-current-to-file'             <- mon-dir-utils.el
;; `*mon-img-hash*'                           <- mon-dir-utils.el
;; `*mon-nef-img-hash*'                       <- mon-dir-utils.el
;; `*mon-jpg-img-hash*'                       <- mon-dir-utils.el
;; `*mon-bmp-img-hash*'                       <- mon-dir-utils.el
;;
;; TODO:
;;
;; NOTES:
;;
;; SNIPPETS:
;;
;; REQUIRES:
;;
;; :FILE mon-dir-locals-alist.el
;;  |-> `*mon-nefs_photos_nefs-alist*' `*mon-nef-scan-nefs-path*' `*mon-ebay-images-bmp-path*'
;; :SEE (URL `http://www.emacswiki.org/emacs/mon-dir-locals-alist.el')
;;
;; :FILE mon-hash-utils.el
;;  |-> `mon-dir-hash-images', `mon-dir-hashed-complete' -> `mon-hash-all-keys'
;; :SEE (URL `http://www.emacswiki.org/emacs/mon-hash-utils.el')
;;
;; THIRD-PARTY-CODE:
;;
;; URL: http://www.emacswiki.org/emacs/mon-dir-utils-local.el
;; FIRST-PUBLISHED:
;;
;; EMACSWIKI: { URL of an EmacsWiki describing mon-dir-utils-local. }
;;
;; FILE-CREATED:
;; <Timestamp: #{2010-11-04T19:38:33-04:00Z}#{10444} - by MON KEY>
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
;; Copyright © 2010 MON KEY 
;;; ==============================

;;; CODE:

(eval-when-compile (require 'cl))

(unless (and (intern-soft "*IS-MON-OBARRAY*")
             (bound-and-true-p *IS-MON-OBARRAY*))
(setq *IS-MON-OBARRAY* (make-vector 17 nil)))

(require 'mon-hash-utils)

(declare-function w32-shell-execute "w32fns.c" t t)
(declare-function mon-set-difference              "mon-seq-utils" (set1-lst set2-lst comparison-func))
(declare-function mon-mapcar                      "mon-seq-utils" (mapcar-fun mapcar-lst &rest more-lsts))
(declare-function mon--local-url-for-bug          "mon-dir-utils-local" (is-url file-string))
(declare-function mon-file-path-for-bug           "mon-dir-utils-local" (file-name-path insrtp yankp intrp))
(declare-function mon-get-local-url-for-bug       "mon-dir-utils-local" (file-string))
(declare-function mon-dir-nef-update-photos-alist "mon-dir-utils-local")

;;; ==============================
(defvar *mon-img-hash* nil
  "Hash-table for holding images in image directories.\n
Image directories defined in global vars:\n
 `*mon-nef-scan-path*', `*mon-nef-scan-nefs-path*', `*mon-nef-scan-nef2-path*',
 `*mon-ebay-images-path*',`*mon-ebay-images-bmp-path*', `*mon-ebay-images-jpg-path*',
 `*mon-ebay-images-temp-path*'\n
:CALLED-BY `mon-dir-hash-images', `mon-dir-try-comp', `mon-dir-hashed-complete'.\n
:SEE-ALSO .\n►►►")
;;
(unless (and (intern-soft "*mon-img-hash*" obarray)
             (bound-and-true-p *mon-img-hash*))
  (setq *mon-img-hash* (make-hash-table :test 'equal)))
;;
;;; :TEST-ME (boundp '*mon-img-hash*)
;;; :TEST-ME *mon-img-hash*
;;
;;;(progn (makunbound '*mon-img-hash*) (unintern "*mon-img-hash*" obarray) )

;;; ==============================
(defvar *mon-nef-img-hash* nil
  "Hash-table for holding images in image directories.\n
Image directories defined in global variable `*mon-nefs_photos_nefs-alist*':
 `*mon-nef-scan-path*', `*mon-nef-scan-nefs-path*', `*mon-nef-scan-nef2-path*',
 `*mon-ebay-images-path*', `*mon-ebay-images-bmp-path*', `*mon-ebay-images-jpg-path*',
 `*mon-ebay-images-temp-path*'\n
:CALLED-BY `mon-dir-hash-images', `mon-dir-try-comp', `mon-dir-hashed-complete'.\n
:SEE-ALSO .\n►►►")
;;
(unless (and (intern-soft "*mon-nef-img-hash*" obarray)
             (bound-and-true-p *mon-nef-img-hash*))
  (setq *mon-nef-img-hash* (make-hash-table :test 'equal)))
;;
;;; :TEST-ME (assoc ".nef1" *mon-ebay-images-lookup-path*)
;;; :TEST-ME (assoc ".nef2" *mon-ebay-images-lookup-path*)
;;; :TEST-ME (boundp '*mon-nef-img-hash*)
;;
;;;(progn (makunbound '*mon-nef-img-hash*) (unintern "*mon-nef-img-hash*" obarray) )

;;; ==============================
(defvar *mon-bmp-img-hash* nil
  "Hash-table for holding images in image `*mon-ebay-images-bmp-path*'.\n
Image directories defined in global variables:\n
 `*mon-nef-scan-path*', `*mon-nef-scan-nefs-path*', `*mon-nef-scan-nef2-path*',
 `*mon-ebay-images-path*', `*mon-ebay-images-bmp-path*', `*mon-ebay-images-jpg-path*',
 `*mon-ebay-images-temp-path*'\n
:CALLED-BY `mon-dir-hash-images', `mon-dir-try-comp', `mon-dir-hashed-complete'.\n
:SEE-ALSO .\n►►►")
;;
(unless (and (intern-soft "*mon-bmp-img-hash*" obarray)
             (bound-and-true-p *mon-bmp-img-hash*))
  (setq *mon-bmp-img-hash* (make-hash-table :test 'equal)))
;;
;;; :TEST-ME (assoc ".bmp" *mon-ebay-images-lookup-path*)
;;; :TEST-ME (boundp '*mon-bmp-img-hash*)
;;
;;;(progn (makunbound '*mon-bmp-img-hash*) (unintern "*mon-bmp-img-hash*" obarray) )

;;; ==============================
(defvar *mon-jpg-img-hash* nil
  "Hash-table for holding images in image `*mon-ebay-images-bmp-path*'.\n
Image directories defined in global variables: \n
 `*mon-nef-scan-path*', `*mon-nef-scan-nefs-path*', `*mon-nef-scan-nef2-path*',
 `*mon-ebay-images-path*', `*mon-ebay-images-bmp-path*', `*mon-ebay-images-jpg-path*',
 `*mon-ebay-images-temp-path*'\n
:CALLED-BY `mon-dir-hash-images', `mon-dir-try-comp', `mon-dir-hashed-complete'.\n
:SEE-ALSO .\n►►►")
;;
(unless (and (intern-soft "*mon-jpg-img-hash*" obarray)
             (bound-and-true-p *mon-jpg-img-hash*))
  (setq *mon-jpg-img-hash* (make-hash-table :test 'equal)))
;;
;;; :TEST-ME *mon-jpg-img-hash*
;;; :TEST-ME (boundp '*mon-jpg-img-hash*)
;;
;;;(progn (makunbound '*mon-jpg-img-hash*) (unintern "*mon-jpg-img-hash*" obarray) )

(when (and (intern-soft "*mon-naf-mode-root*" obarray) ;;*IS-MON-OBARRAY*
           (bound-and-true-p *mon-naf-mode-root*))
;;; ==============================
;;; :PREFIX "mfmef-"
;;; :MODIFICATIONS <Timestamp: #{2010-05-22T17:06:35-04:00Z}#{10206} - by MON KEY>
;;; :MODIFICATIONS <Timestamp: #{2010-04-06T12:11:52-04:00Z}#{10142} - by MON KEY>
;;; :CREATED <Timestamp: #{2009-08-21T18:02:47-04:00Z}#{09345} - by MON KEY>
(defun* mon-file-map-elisp-fileset (&optional (to-fileset-file nil got-tff) insrtp intrp 
                                              &rest these-dirs 
                                              &aux (got-tff (concat *mon-naf-mode-root* 
                                                                    "/mon-elisp-fileset.el")))
  "Return the list of *.el files in local Emacs load files.\n
When TO-FILESET-FILE is non-nil string write return value to the file named by
string.  Default is to write to file named:
 \(concat *mon-naf-mode-root* \"/mon-elisp-fileset.el\")\n
When called-interactively or INSRTP is non-nil insert return value at point.
When INSRTP moves point. When called-interactively does not move point.
When rest arg THESE-DIRS is non-nil these should identify existing directory
names to search for elisp files. Default is to map the values of follwing variables: 
 `*mon-emacs-root*', `*mon-naf-mode-root*', `*mon-ebay-tmplt-mode-root*'\n
When THESE-DIRS is non-nil and TO-FILESET-FILE is non-nil but
does not satisfy the predicate `stringp' signal an error.\n
:EXAMPLE\n\n(mon-file-map-elisp-fileset)\n
\(with-current-buffer 
    \(get-buffer-create \"*MON-FILE-MAP-ELISP-FILESET-TEST*\"\)
  \(mon-file-map-elisp-fileset nil nil t\)
  \(display-buffer \(current-buffer\) t\)\)\n\n
:NOTE Will not find files of type: *.el.gz IOW anything under subdir of Emacs'
lisp/ source tree when Emacs was built with the\n
:SEE-ALSO `*mon-el-library*', `mon-file-ensure-extension-is-el',
`mon-file-truename-p', `mon-file-dir-attributes->plist'.\n►►►"
  (interactive "i\ni\np")
  (let ((mfmef-to-flset (cond ((stringp to-fileset-file)
                               to-fileset-file)
                              ((and these-dirs (not (stringp to-fileset-file)))
                               (unless (or insrtp intrp)
                                 (error (concat ":FUNCTION `mon-file-map-elisp-fileset' " 
                                                "-- optional arg TO-FILESET-FILE missing"
                                                "with arg THESE-DIRS non-nil"))))
                              ((not (null to-fileset-file))
                               (concat *mon-naf-mode-root* "/mon-elisp-fileset.el"))
                              ((null to-fileset-file) t)))
        (mfmef-fls (let (mfmef-lcl-get-td)
                     (if these-dirs ;; Make sure we got strings.
                         (dolist (mfmef-D-1
                                  (dolist (mfmef-D-2 these-dirs these-dirs)
                                    (unless (stringp mfmef-D-2)
                                      (error (concat ":FUNCTION `mon-file-map-elisp-fileset' "
                                                     "-- elt in rest arg THESE-DIRS not `stringp', "
                                                     "got:  %S")
                                             mfmef-D-2)))
                                  mfmef-lcl-get-td)
                           (setq mfmef-lcl-get-td 
                                 (nconc (directory-files mfmef-D-1 t ".*\.el$") mfmef-lcl-get-td)))
                       (setq mfmef-lcl-get-td
                             (append (directory-files *mon-emacs-root* t ".*\.el$")
                                     (directory-files *mon-naf-mode-root* t ".*\.el$")
                                     (directory-files *mon-ebay-tmplt-mode-root* t ".*\.el$"))))
                     (dolist (mfmef-D-3
                              (dolist (mfmef-D-4 mfmef-lcl-get-td mfmef-lcl-get-td)
                                (when (string-match-p "\.#" (file-name-nondirectory mfmef-D-4))
                                  (setq mfmef-lcl-get-td (delete mfmef-D-4 mfmef-lcl-get-td))))
                              mfmef-lcl-get-td)
                       (unless (file-exists-p mfmef-D-3)
                         (error (concat ":FUNCTION `mon-file-map-elisp-fileset' "
                                        "-- afileset directory is non-existent, got: %S") 
                                mfmef-D-3))))))
    (cond ((or insrtp intrp)
           (if intrp
               (save-excursion
                 (newline)
                 (princ (mapconcat #'identity mfmef-fls "\n") (current-buffer)))
             (progn
               (newline)
               (princ (mapconcat #'identity mfmef-fls "\n") (current-buffer)))))
          ((stringp mfmef-to-flset)
           (with-temp-file mfmef-to-flset
             (newline)
             (mapc #'(lambda (mfmef-L-1)
                       (prin1 mfmef-L-1 (current-buffer))
                       (newline))
                   mfmef-fls))
           (with-current-buffer (find-file-noselect mfmef-to-flset t)
             (when (intern-soft "mon-file-stamp")
               (mon-file-stamp t))
             (mon-g2be -1)
             (princ ";; -*- mode: EMACS-LISP; no-byte-compile: t; -*-\n" (current-buffer))
             (write-file (buffer-file-name (current-buffer)))
             (kill-buffer (current-buffer)))
           mfmef-fls)
          (mfmef-to-flset mfmef-fls))))
) ;; CLOSE when
;;
;;,---- :UNCOMMENT-BELOW-TO-TEST
;;| (let (doldpth)
;;|   (dotimes (lp 3)
;;|     (push (nth (random (length load-path)) load-path) doldpth ))
;;|   (apply 'mon-file-map-elisp-fileset 
;;|          ;; nil nil doldpth)) ;; <- Default
;;|          ;; t nil doldpth))   ;; <- INSRTP
;;|          ;; nil t doldpth))   ;; <- INTRP
;;|          (concat temporary-file-directory ;; <- TO-FILESET-FILE Writes to temp-file
;;|                  "/TEST-mon-file-map-elisp-fileset.el")))
;;| 
;;| (find-file  (concat temporary-file-directory 
;;|                     "/TEST-mon-file-map-elisp-fileset.el"))
;;| 
;;| (delete-file (concat temporary-file-directory 
;;|                      "/TEST-mon-file-map-elisp-fileset.el"))
;;`----

(when (and (intern-soft "*mon-emacs-root*" obarray)  ;; *IS-MON-OBARRAY*
           (bound-and-true-p *mon-emacs-root*))
;;; ==============================
;;; :NOTE There are better ways to accomplish this and the :TODO's are misguided.
;;; :TODO Make a global var for this and default to it. 
;;; :TODO Add post save hook to record the directory for new or modified .naf's.
;;; :COURTESY Stefan Reichor, stefan@xsteve.at :HIS xsteve-functions.el :VERSION 2001-03-28
;;; :MODIFICATIONS <Timestamp: Monday February 09, 2009 @ 09:34.29 PM - by MON>
(defun mon-dir-save-current ()
  "Save the current directory to a specific file.\n
:NOTE This function is only relevant if the following evaluates non-nil:
 \(let \(\(rlvnt-p  \(concat *mon-emacs-root* \"/current-directory\"\)\)\)
  \(and \(file-exists-p rlvnt-p\) 
       \(file-readable-p rlvnt-p\)
       \(file-writable-p rlvnt-p\)\)\)\n
:SEE-ALSO `mon-dir-save-current-to-file', `mon-file-dir-attributes->plist'
`append-to-file'.\n►►►"
  (interactive)
  (let ((rlvnt-p  (concat *mon-emacs-root* "/current-directory")))
    (or (and (file-exists-p rlvnt-p) 
             (file-readable-p rlvnt-p)
             (file-writable-p rlvnt-p))
        (error (concat ":FUNCTION `mon-dir-save-current' "
                       "-- required file not available, need :FILE %S\n"
                       "-- :SEE `*mon-record-current-directory*'" rlvnt-p)))
    (let* ((current-sys-type rlvnt-p)
           (dir default-directory)
           (file-name (shell-quote-argument (expand-file-name dir))))
      (with-current-buffer (find-file-noselect current-sys-type)
        ;; Overwrites the file. As commented appends:
        ;; (delete-region (point-min) (point-max)) 
        (when (equal system-type 'windows-nt)
          (setq file-name (replace-regexp-in-string "/" "\\\\" file-name)))
        (mon-g2be 1)
        (princ file-name (current-buffer)) ;; (insert file-name)
        (newline)
        ;; :WAS (save-buffer)
        (basic-save-buffer)
        (message "Saved directory '%s' to %s" file-name current-sys-type)
        (kill-buffer (current-buffer))))))


;;; ==============================
;;; :PREIFX "mdsctf-"
;;; :MODIFICATIONS <Timestamp: #{2009-08-11T18:12:53-04:00Z}#{09332} - by MON>
(defun mon-dir-save-current-to-file (&optional intrp)
  "Save the current files directory path to a file.\n
Default file is held by global var `*mon-record-current-directory*'.\n
:SEE-ALSO `mon-dir-save-current', `mon-append-to-register',
`append-to-buffer', `append-output-to-file', `mon-cat',
`mon-file-dir-attributes->plist'.\n►►►"
  (interactive "p")
  (let* ((mdsctf-intrp intrp)
	 (mdsctf-wntng (when (and mdsctf-intrp)
			 (read-directory-name 
                          (concat ":FUNCTION `mon-dir-save-current-to-file' "
                                  "directory path: "))))
	 (mdsctf-alt-fl  (when (and mdsctf-intrp)
		      ;; (read-file-name
		      (read-string
                       (concat ":FUNCTION `mon-dir-save-current-to-file' "
                               "-- filename: ")
		       ;; mdsctf-wntng
		       nil
		       (concat "naf-directory-list-" (format-time-string "%m-%d-%Y") ".dbc"))))
	 (mdsctf-bld-fl (when (and mdsctf-intrp)
		       (concat mdsctf-wntng mdsctf-alt-fl)))
	 (mdsctf-sys-typ (if (and mdsctf-intrp)
			       mdsctf-bld-fl
                             ;; (concat *mon-emacs-root* "/current-directory")))
                             *mon-record-current-directory*))
	 (mdsctf-dir default-directory)
	 (mdsctf-fl-nm (shell-quote-argument (expand-file-name mdsctf-dir))))
    (with-current-buffer (find-file-noselect mdsctf-sys-typ)
      ;; Overwrites the file. As commented appends:
      ;; (delete-region (point-min) (point-max)) 
      (when (eq system-type 'windows-nt)
	(setq mdsctf-fl-nm (replace-regexp-in-string "/" "\\\\" mdsctf-fl-nm)))
      ;;(goto-char (point-max))
      (mon-g2be 1)
      (princ mdsctf-fl-nm (current-buffer)) ;; (insert mdsctf-fl-nm)
      (newline)
      ;; :WAS (save-buffer)
      (basic-save-buffer)
      (message  (concat ":FUNCTION `mon-dir-save-current-to-file' "
                        "-- saved directory: '%s' to: %s") mdsctf-fl-nm mdsctf-sys-typ)
      (kill-buffer (current-buffer)))))
;;
) ;; :CLOSE when `*mon-emacs-root*'



;;; ==============================
;;; :CREATED <Timestamp: Monday February 09, 2009 @ 09:35.31 PM - by MON>
(defun mon-explorer-naf-artist (prefix)
  "When `IS-W32-P' open w32 explorer in alphabetic NAF drive Artists dir.\n
PREFIX is a one letter string A-Z.
When called-interactively, prompt for an alphabetic directory PREFIX.
Default path held by global var: `*mon-artist-naf-path*'.\n
:USED-IN `naf-mode'.\n
:SEE-ALSO `mon-explorer-naf-brand',`mon-dired-naf-artist-letter',
`mon-dired-naf-brand-letter', `mon-explorer-open'.\n►►►"
  (interactive "p")
  (if IS-W32-P
    (let ((dl (format "%s-Artists names"
                       (if (numberp prefix)
		       (upcase (read-string "Alphabetic Artists Directory: "))
                       (upcase prefix))))
          ;; w32 explorer needs this backslashed.
          naf-alph-path)
      (setq naf-alph-path (concat *mon-artist-naf-path* "/" dl))
      (setq naf-alph-path (subst-char-in-string 47 92 naf-alph-path))
      (w32-shell-execute  "open" "explorer" (concat "/e, " naf-alph-path)))
    ;; :NOTE could use this as default fallback:
    ;; (mon-dired-naf-artist-letter prefix)
    (message (concat ":FUNCTION `mon-explorer-naf-artist' "
                     "-- set file-manager exectuable for system-type: %s")
             system-type)))
;;
;;; :TEST-ME (mon-explorer-naf-artist "b")


;;; ==============================
;;; :CREATED <Timestamp: Monday February 09, 2009 @ 09:35.31 PM - by MON>
(defun mon-explorer-naf-brand (w-prefix)
  "When `IS-W32-P' open w32 explorer in alphabetic NAF drive Brand dir.\n
W-PREFIX is a one letter string A-Z.\n
When called-interactively, prompt for an alphabetic naf brand directory.
Default path held in var: `*mon-brand-naf-path*'.\n
:USED-IN `naf-mode'.\n
:SEE-ALSO `mon-explorer-naf-artist',`mon-dired-naf-artist-letter',
`mon-dired-naf-brand-letter', `mon-dired-naf-image-dir',
`mon-explorer-open'.\n►►►"
  (interactive "p")
  (if (and (intern-soft "IS-W32-P" obarray) ;; *IS-MON-OBARRAY*
           (bound-and-true-p IS-W32-P))
      (let* ((dl (format "%s-Brand-NAFs"
                         (if (numberp w-prefix)
                             (upcase (read-string "Alphabetic Artists Directory:"))
                           (upcase w-prefix))))
             (naf-alph-path)) ;;win32 explorer needs this backslashed
        (setq naf-alph-path (concat *mon-brand-naf-path* "/" dl))
        (setq naf-alph-path (subst-char-in-string 47 92 naf-alph-path))
        (w32-shell-execute  "open" "explorer" (concat "/e, " naf-alph-path)))
    ;; :NOTE could use this as default fallback:
    ;; (mon-dired-naf-brand-letter w-prefix)
    (message (concat ":FUNCTION `mon-explorer-naf-artist' "
                     "-- set file-manager exectuable for system-type: %s")
             system-type)))
;;
;;; :TEST-ME (mon-explorer-naf-brand "b")

;;; ==============================
;;; :TODO Figure out how to take a second argument that heuristically
;;;       puts point at an appropriate location in the returned dired buffer.
;;; :CREATED <Timestamp: Monday February 09, 2009 @ 09:35.31 PM - by MON>
(defun mon-dired-naf-artist-letter (prefix)
  "Dired visit the alphabetic Artist naf-directory by letter PREFIX.\n
PREFIX is a one letter string A-Z.\n
When Called interactively prompts for:\"Alphabetic Brand Directory :\" 
concatatenating \"LETTER-Artist names/\" to the default naf path.
Default naf path held by the var: `*mon-artist-naf-path*'.\n
:EXAMPLE\n\n(mon-dired-naf-artist-letter \"b\")\n
:ALIASED-BY `naf-drive-dired-artist-letter'
:ALIASED-BY `naf-dired-artist-letter'\n
:USED-IN `naf-mode'.\n
:SEE-ALSO `mon-explorer-naf-artist',`mon-explorer-naf-brand',
`mon-dired-naf-brand-letter', `mon-dired-naf-image-dir',
`mon-open-explorer'.\n►►►"
  (interactive "p")
  (let* ((dl (format "/%s-Artists-names/"
                       (if (numberp prefix)
                           (upcase (read-string "Alphabetic Artists Directory: "))
                         (upcase prefix))))
	 (naf-alph-path (concat *mon-artist-naf-path* dl))
	 (default-directory naf-alph-path))
    (dired-other-window naf-alph-path)))

;;; :TEST-ME (mon-dired-naf-artist-letter "b")

;;; ==============================
;;; :CREATED <Timestamp: Monday February 09, 2009 @ 09:35.31 PM - by MON>
(defun mon-dired-naf-brand-letter (prefix); &optional intrp)
  "Dired the alphabetic Brand naf-directory by letter PREFIX.\n
PREFIX is a one letter string A-Z.
When Called interactively prompts for:\n\n \"Alphabetic Brand Directory: \"\n
concatatenating \"LETTER-Brand-NAFS/\" to the default naf path.
Default naf path held by the var: `*mon-brand-naf-path*'.\n
:EXAMPLE\n(mon-dired-naf-brand-letter \"b\")\n
:ALIASED-BY `naf-drive-dired-brand-letter'
:ALIASED-BY `naf-dired-brand-letter'\n
:USED-IN `naf-mode'.\n
:SEE-ALSO `mon-dired-naf-artist-letter', `mon-explorer-naf-artist',
`mon-explorer-naf-brand', `mon-dired-naf-image-dir', `mon-open-explorer'.\n►►►"
  (interactive "p")
  (let* ((dl (format "/%s-Brand-NAFs/"
                       (if (numberp prefix)
                           (upcase (read-string "Alphabetic Brand Directory: "))
                         (upcase prefix))))
	 (naf-alph-path (concat *mon-brand-naf-path* dl))
	 (default-directory naf-alph-path))
    (dired-other-window naf-alph-path)))

;;; :TEST-ME (mon-dired-naf-brand-letter  "b")

;;; ==============================
(eval-when-compile 
  (when (and (and (intern-soft "*mon-nef-scan-nefs-path*" obarray) ;; *IS-MON-OBARRAY*
                  (bound-and-true-p *mon-nef-scan-nefs-path*))
             (and (intern-soft "*mon-nef-scan-nef2-path*" obarray)
                  (bound-and-true-p *mon-nef-scan-nef2-path*))
             (and (intern-soft "*mon-ebay-images-bmp-path*")
                  (bound-and-true-p *mon-ebay-images-bmp-path*))
             (and (intern-soft "*mon-ebay-images-jpg-path*" obarray)
                  (bound-and-true-p *mon-ebay-images-jpg-path*)))
;;
;;; ==============================
;;; :CREATED <Timestamp: Thursday June 25, 2009 @ 06:03.54 PM - by MON>
(defun mon-dired-naf-image-dir (pth-nm &optional intrp)
  "Dired to an image directory.\n
PTH-NM is an image directory udner one of the following strings:\n
 \"nefs-archived\" \"nefs-working\" \"ebay-bmp\" \"ebay-jpg\"\n
These are bound as alist keys in the vars:\n
 `*mon-nef-scan-nefs-path*', `*mon-nef-scan-nef2-path*',
 `*mon-ebay-images-bmp-path*', `*mon-ebay-images-jpg-path*'\n
When called-interactively complete the key to dired to the directory val.\n
:EXAMPLE\n\n(mon-dired-naf-image-dir \"ebay-bmp\")\n
:SEE-ALSO `mon-dired-naf-artist-letter', `mon-explorer-naf-artist', 
`mon-explorer-naf-brand', `mon-open-explorer', `mon-dired-nef-dir',
`mon-dir-nef-big', `*mon-nefs_photos_nefs-alist*'.\n►►►"
  (interactive "p\nP")
  (let* ((img-pths (mon-mapcar 
                    'cons
                    '("nefs-archived" "nefs-working" "ebay-bmp"  "ebay-jpg")               
                    `(,*mon-nef-scan-nefs-path* ,*mon-nef-scan-nef2-path* 
                      ,*mon-ebay-images-bmp-path* ,*mon-ebay-images-jpg-path*)))
         (in-d (cond (intrp (completing-read "Which path: " img-pths nil t))
                     ((assoc pth-nm img-pths) pth-nm)
                     ((not (assoc pth-nm img-pths))
                      (completing-read "Which path: " img-pths nil t))))
         (to-d (cdr (assoc in-d img-pths))))
    ;; Complete on each directories of nefs-archived with a cached alist.
    (if (string= in-d "nefs-archived")
        (let ((nef-alist (mon-dir-nef-big *mon-nefs_photos_nefs-alist*))
              (get-nef-dir))
          (setq get-nef-dir
                (cadr (assoc-string 
                       (completing-read "Which nef directory (tab-completes): " nef-alist) 
                       nef-alist)))
          (dired (concat *mon-nef-scan-nefs-path* "/" get-nef-dir)))
      (dired (cdr (assoc in-d  img-pths))))))
;;
;;; :TEST-ME (mon-dired-naf-image-dir "nefs-archived")
;;
)) ;; :CLOSE when *mon-vars*



(when (and (intern-soft "*mon-nef-scan-nefs-path*" obarray) ;; *IS-MON-OBARRAY*
           (bound-and-true-p *mon-nef-scan-nefs-path*))
;;; ==============================
;;; :PREFIX "mdnupa-"
;;; :CREATED <Timestamp: Saturday June 27, 2009 @ 04:01.56 PM - by MON>
(defun mon-dir-nef-update-photos-alist ()
  "Helper function for `mon-bind-nefs-photos-at-loadtime'.\n
Refresh or build alist contents for variable `*mon-nefs_photos_nefs-alist*'.\n
Assumes directory names of `*mon-nef-scan-nefs-path*' have one of the formats:\n
 NNN_Name-of-folder_\(NNNN-NNNN\)\n 1^^^2*^^^^^^^^^^^^^3^^^^^^^^^^\n
 NNN_\(NNNN-NNNN\)\n 1^^^3^^^^^^^^^^\n
 NNN_Name-of-folder_\(NNNN-NNNN, NNNN,NNNN,NNNN\)
 1^^^2*^^^^^^^^^^^^^3^^^^^^^^^^^^^^^^^^^^^^^^^\n
 NNN_\(NNNN-NNNN, NNNN\)\n 1^^^3^^^^^^^^^^^^^^^^\n
1\) [Image-directory-number
    :Integer {3}
    :Underscore: \"_\" char: 95]\n
2\) [Folder-name-string                       ; :NOTE Optional
    :Namestring ::= [A-z0-9,]
    :Namestring-delimiter ::= \"-\" char: 45]\n
3\) [Image-Range
    :Underscore \"_\" char: 95
    :Opening-Paren ::= char: 40
    :Integer ::= {1,5}
    :Range-delimter ::= delimit with \"-\" char: 45
    :Multi-range ::= delimit second with \", \"  char: 44 char 32
    :Subsequent-multi-ranges ::=  \",=\" char: 44
    :Integer ::= {2,5}]\n
Assuming directory ontains the two folders:
 \"001_Lesson-Humming-Brds_\(1-21\)\"\n \"242_\(12390-12400\)\"\n
Return an alist as:\n
 \(\(\"001_Lesson-Humming-Birds_\(1-21\)\" \"001\" \"Lesson-Humming-Birds\" \"\(1-21\)\"\)
 \(\"242_\(12390-12400\)\" \"242\" \"\(12390-12400\)\)\"\)\)\n
:CALLED-BY `mon-dired-nef-dir', `mon-dir-nef-big'.\n
:SEE-ALSO `mon-bind-cifs-vars-at-loadtime', `mon-set-register-tags-loadtime',
`mon-bind-iptables-vars-at-loadtime', `mon-bind-doc-help-proprietery-vars-at-loadtime',
`mon-CL-cln-colon-swap'.\n►►►"
  (or (and (intern-soft "*mon-nef-scan-nefs-path*" obarray)
           (bound-and-true-p *mon-nef-scan-nefs-path*))
      (warn (concat ":FUNCTION `mon-dir-nef-update-photos-alist' "
                    "-- :VARIABLE `*mon-nef-scan-nefs-path*' not `bound-and-true-p' "
                    "so :FUNCTION `mon-bind-nefs-photos-at-loadtime' will not bind "
                    ":VARIABLE `*mon-nefs_photos_nefs-alist*' at loadtime")))
  (let (mdnupa-cf-efct)
    (setq mdnupa-cf-efct
          ;; :PREFIX "mdnupa-L-1-"
          (mapcar #'(lambda (mdnupa-L-1-x) 
                      (let (mdnupa-L-1-lcl-splt mdnupa-L-1-lcl-splt-nm)
                        (setq mdnupa-L-1-lcl-splt 
                              (save-match-data (split-string mdnupa-L-1-x "_" )))
                        (setq mdnupa-L-1-lcl-splt-nm
                              (cond ((= (length mdnupa-L-1-lcl-splt) 3) 
                                     `(,mdnupa-L-1-x
                                       ,(car mdnupa-L-1-lcl-splt)
                                       ,(subst-char-in-string 45 32 (cadr mdnupa-L-1-lcl-splt))
                                       ,(caddr mdnupa-L-1-lcl-splt)))
                                    ((= (length mdnupa-L-1-lcl-splt) 2)
                                     `(,mdnupa-L-1-x ,(car mdnupa-L-1-lcl-splt) ,(cadr mdnupa-L-1-lcl-splt))) 
                                    ;; We should never see this.
                                    (t `(,mdnupa-L-1-x ,mdnupa-L-1-lcl-splt))))
                        mdnupa-L-1-lcl-splt-nm))
                  (directory-files *mon-nef-scan-nefs-path* nil "^\\([0-9]\\{2,3\\}_.*\\)")))))
;;



;;; ==============================
;;; :TODO As this is the top level interface to the callers it should check if
;;; anything has changed in the persistent cached list stored to file (which we don't have!).
;;; :CREATED <Timestamp: #{2010-02-04T16:33:33-05:00Z}#{10054} - by MON KEY>
(defun mon-bind-nefs-photos-at-loadtime ()
  "Build `*mon-nefs_photos_nefs-alist*' with at loadtime.\n
Finds `directory-files' in `*mon-nef-scan-nefs-path*' with
`mon-dir-nef-update-photos-alist'.\n
:SEE-ALSO `mon-bind-cifs-vars-at-loadtime', `mon-set-register-tags-loadtime',
`mon-bind-iptables-vars-at-loadtime', `mon-CL-cln-colon-swap'.\n►►►"
  ;; :NOTE __DON'T SNARF IF ON REMOTE MACHINES!!__
  (when (and (intern-soft "IS-MON-SYSTEM-P" obarray) ;; *IS-MON-OBARRAY*
             (bound-and-true-p IS-MON-SYSTEM-P)
             (not (and (intern-soft "IS-BUG-P-REMOTE" obarray)
                       (bound-and-true-p IS-BUG-P-REMOTE))))
    (unless (and (intern-soft "*mon-nefs_photos_nefs-alist*" obarray)
                 (bound-and-true-p *mon-nefs_photos_nefs-alist*))
      (setq *mon-nefs_photos_nefs-alist* (mon-dir-nef-update-photos-alist)))))
;;
;; (let ((new-alist (mon-dir-nef-update-photos-alist)))
;;   (setq *mon-nefs_photos_nefs-alist* new-alist))
;;
;;;(progn (makunbound '*mon-nefs_photos_nefs-alist*)  
;;;        (unintern "*mon-nefs_photos_nefs-alist*" obarray) )
;;
) ;; :CLOSE when `*mon-nef-scan-nefs-path*'

;;; ==============================
;;; :PREFIX "mdnr-"
;;; :CREATED <Timestamp: Saturday June 27, 2009 @ 04:27.24 PM - by MON>
(defun mon-dir-nef-ranges (folder-alist)
  "Return FOLDER-ALIST ranges with numeric ranges in head position.\n
Parens are stripped from ranges.\n
Return value is a two elt list each a string. It has the format:
 \(\"1-21\" \"001_Lesson-Humming-Birds_\(1-21\)\"\)\n
:EXAMPLE\n\n\(mon-dir-nef-ranges  *mon-nefs_photos_nefs-alist*\)\n
:SEE-ALSO `mon-dired-nef-dir', `mon-dir-nef-big', `mon-dir-nef-converge',
`mon-dir-nef-keep-3',`mon-dir-nef-name-to-head',`mon-dir-nef-conc-ranges',
`mon-dir-nef-conc-dups',`mon-dir-nef-find-dups',`mon-dir-nef-remove-if-empty',
`*mon-nefs_photos_nefs-alist*', `*mon-nef-scan-nefs-path*'.\n►►►"
  (let (mdnr-grtr-rng)
    (setq mdnr-grtr-rng 
          ;; :PREFIX "mdnr-L-1-"
          (mapcar #'(lambda (mdnr-L-1-x) 
                      (let* (;; Maybe use `assoc-string' instead? 
                             (mdnr-L-1-lcl-this (assoc (car mdnr-L-1-x) folder-alist))
                             (mdnr-L-1-lcl-len  (length mdnr-L-1-x))
                             (mdnr-L-1-lcl-pth  (car mdnr-L-1-lcl-this))
                             (mdnr-L-1-lcl-rng  (cond ((= mdnr-L-1-lcl-len 3)
                                                       (caddr mdnr-L-1-lcl-this))
                                                      ((= mdnr-L-1-lcl-len 4)
                                                       (cadddr mdnr-L-1-lcl-this)))))
                        (setq mdnr-L-1-lcl-rng (replace-regexp-in-string "(" "" mdnr-L-1-lcl-rng)
                              mdnr-L-1-lcl-rng (replace-regexp-in-string ")" "" mdnr-L-1-lcl-rng))
                        `(,mdnr-L-1-lcl-rng ,mdnr-L-1-lcl-pth)))
                  folder-alist))
    mdnr-grtr-rng))
;;
;;; :TEST-ME (mon-dir-nef-ranges  *mon-nefs_photos_nefs-alist*)

;;; ==============================
;;; :PREFIX "mdna-"
;;; :CREATED <Timestamp: Saturday June 27, 2009 @ 04:27.24 PM - by MON>
(defun mon-dir-nef-name-to-head (folder-alist)
  "Return FOLDER-ALIST with the directory numeric name in head position.
Return value is a two string list. It has the format:\n
 \(\"001\" \"001_Lesson-Humming-Birds_\(1-21\)\"\)\n
:EXAMPLE\n\n\(mon-dir-nef-name-to-head *mon-nefs_photos_nefs-alist*\)\n
:SEE-ALSO `mon-dired-nef-dir', `mon-dir-nef-big', `mon-dir-nef-converge',
`mon-dir-nef-keep-3',`mon-dir-nef-conc-ranges',`mon-dir-nef-ranges',
`mon-dir-nef-conc-dups',`mon-dir-nef-find-dups', `mon-dir-nef-remove-if-empty', 
`*mon-nefs_photos_nefs-alist*', `*mon-nef-scan-nefs-path*'.\n►►►"
  (let (mdna-grtr-fldr)
    (setq mdna-grtr-fldr 
          ;; :PREFIX "mdna-L-1-"
          (mapcar #'(lambda (mdna-L-1-x) 
                      (let* (;; Maybe use `assoc-string' instead?
                             (mdna-L-1-lcl-this  (assoc (car mdna-L-1-x) folder-alist))
                             (mdna-L-1-lcl-pth   (car mdna-L-1-lcl-this))
                             (mdna-L-1-fldr-num  (cadr mdna-L-1-lcl-this)))
                        `(,mdna-L-1-fldr-num ,mdna-L-1-lcl-pth)))
                  folder-alist))
    mdna-grtr-fldr))
;;    
;;; :TEST-ME (mon-dir-nef-name-to-head *mon-nefs_photos_nefs-alist*)

;;; ==============================
;;; :PREFIX "mdnre-"
;;; :CREATED <Timestamp: Saturday June 27, 2009 @ 04:27.24 PM - by MON>
(defun mon-dir-nef-remove-if-empty (folder-alist)
  "Return FOLDER-ALIST with empty `nil' folders removed.\n
Return value is a two string list of directory names.\n
Return with empty `nil' directories removed.\n
An directory is considered empty  if it has the format:\n
 NNN_(NNNN-NNNN)\n 1^^^3^^^^^^^^^^\n
:NOTE This is required before we can filter out duplicate folders named with
`mon-dir-nef-find-dups', otherwise `nil' entries will appear as well.\n
:SEE-ALSO `mon-dired-nef-dir', `mon-dir-nef-big', `mon-dir-nef-converge',
`mon-dir-nef-keep-3',`mon-dir-nef-name-to-head',`mon-dir-nef-conc-ranges',
`mon-dir-nef-ranges',`mon-dir-nef-conc-dups', `*mon-nefs_photos_nefs-alist*',
`*mon-nef-scan-nefs-path*'.\n►►►"
  ;; :NOTE The name is "free" in the sense that it may or may not occur at index 2.
  (let (mdnre-free-nm->idx-1) 
    (setq mdnre-free-nm->idx-1
          ;; :PREFIX "mdnre-L-1-lcl-"
          (mapcar #'(lambda (mdnre-L-1-x) 
                      (let* (;; Maybe use `assoc-string' instead?
                             (mdnre-L-1-lcl-this    (assoc (car mdnre-L-1-x) folder-alist))
                             (mdnre-L-1-lcl-len     (length mdnre-L-1-x))
                             (mdnre-L-1-lcl-pth     (car mdnre-L-1-lcl-this))
                             (mdnre-L-1-lcl-free-nm (third mdnre-L-1-lcl-this)))
                        (when (= mdnre-L-1-lcl-len 4) 
                          `(,mdnre-L-1-lcl-free-nm ,mdnre-L-1-lcl-pth))))
                  folder-alist)) 
    (setq mdnre-free-nm->idx-1 (delq '() mdnre-free-nm->idx-1))))
;;
;;; :TEST-ME (mon-dir-nef-remove-if-empty *mon-nefs_photos_nefs-alist*)

;;; ==============================
;;; :PREFIX "mdnfd-"
;;; :CREATED <Timestamp: Saturday June 27, 2009 @ 04:27.24 PM - by MON>
(defun mon-dir-nef-find-dups (folder-alist)
  "Find duplicate dir names in list returned by `mon-dir-nef-remove-if-empty'.\n
Rotate tail-position to head-position.\n
:EXAMPLE\n\n\(mon-dir-nef-find-dups *mon-nefs_photos_nefs-alist*\)\n
:SEE-ALSO `mon-dired-nef-dir', `mon-dir-nef-big', `mon-dir-nef-converge',
`mon-dir-nef-keep-3', `mon-dir-nef-name-to-head', `mon-dir-nef-conc-ranges',
`mon-dir-nef-ranges', `mon-dir-nef-conc-dups', `*mon-nefs_photos_nefs-alist*',
`*mon-nef-scan-nefs-path*'.\n►►►"
  ;; :NOTE The name is "free" in the sense that it may/may-not occur at index 2.
  (let (mdnfd-cmpr free-nm->idx-2) 
    (setq mdnfd-cmpr (mon-dir-nef-remove-if-empty folder-alist))    
    ;; :PREFIX "mdnfd-L-1-"
    (mapc #'(lambda (mdnfd-L-1-x)
              (let* ((mdnfd-L-1-lcl-hd (car mdnfd-L-1-x))
                     (mdnfd-L-1-lcl-tl (cadr mdnfd-L-1-x))
                     (mdnfd-L-1-lcl-mtch (assoc mdnfd-L-1-lcl-hd mdnfd-cmpr)))
                ;; :NOTE Use `string=' so we can use symbols too, e.g.
                ;;  (|some frobbed dir-name| {...} )
                (when (and mdnfd-L-1-lcl-mtch 
                           (not (string= mdnfd-L-1-lcl-tl (cadr mdnfd-L-1-lcl-mtch))))
                  (when (not (member mdnfd-L-1-lcl-mtch free-nm->idx-2))
                    (setq free-nm->idx-2 (cons mdnfd-L-1-lcl-mtch free-nm->idx-2)))
                  (when (not (member mdnfd-L-1-x free-nm->idx-2))
                    (setq free-nm->idx-2 (cons mdnfd-L-1-x free-nm->idx-2))
                    (setq mdnfd-cmpr (delq mdnfd-L-1-lcl-mtch mdnfd-cmpr))))))
          mdnfd-cmpr)
    ;; :WAS (setq free-nm->idx-2 (mapcar #'(lambda (x) (nreverse x))  free-nm->idx-2))))
    (setq mdnfd-cmpr nil)
    ;; :PREFIX "mdnfd-L-2-"
    (mapc #'(lambda (mdnfd-L-2-x) 
              (push (nreverse mdnfd-L-2-x) mdnfd-cmpr))
          free-nm->idx-2)
    (setq mdnfd-cmpr (nreverse mdnfd-cmpr))))
;;
;;; :TEST-ME (mon-dir-nef-find-dups *mon-nefs_photos_nefs-alist*)

;;; ==============================
;;; :CREATED <Timestamp: Saturday June 27, 2009 @ 04:27.24 PM - by MON>
(defun mon-dir-nef-conc-dups (folder-alist) ;*mon-nefs_photos_nefs-alist*
  "Concat directory name identifiers \(integer\) onto duplicate directory names in
list generated with `mon-dir-nef-find-dups'. Each folder with a namestring 
matching an existing namestring needs a unique name so build it.
:EXAMPLE\n\(mon-dir-nef-conc-dups *mon-nefs_photos_nefs-alist*\)\n
:SEE-ALSO `mon-dired-nef-dir', `mon-dir-nef-big', `mon-dir-nef-converge',
`mon-dir-nef-keep-3', `mon-dir-nef-name-to-head', `mon-dir-nef-conc-ranges',
`mon-dir-nef-ranges', `mon-dir-nef-remove-if-empty', `*mon-nefs_photos_nefs-alist*',
`*mon-nef-scan-nefs-path*'.\n►►►"
  (let ((freename>4 (mon-dir-nef-find-dups folder-alist))
        (fldrname> ;; Bind mon-dir-nef-name-to-head in wrapping defun.
         (mapcar #'(lambda (x) (nreverse x)) (mon-dir-nef-name-to-head folder-alist)))
        freename>3)
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
;;; :TEST-ME (mon-dir-nef-conc-dups *mon-nefs_photos_nefs-alist*)

;;; ==============================
;;; :CREATED <Timestamp: Saturday June 27, 2009 @ 04:27.24 PM - by MON>
(defun mon-dir-nef-converge (folder-alist) 
  "Return FOLDER-ALIST with renamed unambiguated duplicate directory names.\n
Directory names of FOLDER-ALIST are folded back into the surronding alist.\n
:SEE-ALSO `mon-dired-nef-dir', `mon-dir-nef-big', `mon-dir-nef-keep-3',
`mon-dir-nef-name-to-head', `mon-dir-nef-conc-ranges', `mon-dir-nef-ranges',
`mon-dir-nef-conc-dups', `mon-dir-nef-find-dups', `mon-dir-nef-remove-if-empty',
`*mon-nefs_photos_nefs-alist*', `*mon-nef-scan-nefs-path*'.\n►►►"
  (let ((big-dupd (mapcar #'(lambda (x) 
                              (nreverse x))
                          (mon-dir-nef-remove-if-empty folder-alist)))
        (dups-only (mon-dir-nef-conc-dups folder-alist))
        no-dups)
    (mapc #'(lambda (x)
              (let* ((is-dup (car x))
                     (old-dup (assoc is-dup big-dupd)))
                (when old-dup 
                  (setq big-dupd (delq old-dup big-dupd))
                  (setq big-dupd (cons x big-dupd)))))
          dups-only)
    (setq no-dups (sort big-dupd #'(lambda (x y) (string< (car x) (car y)))))
    (setq no-dups 
          (sort (setq no-dups (mapcar #'(lambda (x) (nreverse x))  no-dups))
                #'(lambda (x y) (string-lessp (car x) (car y)))))))
;;
;;; :TEST-ME (mon-dir-nef-converge *mon-nefs_photos_nefs-alist*)


;; *IS-MON-OBARRAY*
;;; ==============================
;;; :PREFIX "mdncr-"
;;; :CREATED <Timestamp: Saturday June 27, 2009 @ 04:27.24 PM - by MON>
(defun mon-dir-nef-conc-ranges (folder-alist) ;*mon-nefs_photos_nefs-alist*
  "Return FOLDER-ALIST as two strings with the directory name in head position.\n
Second elt is its range.\n
:SEE-ALSO; `mon-dired-nef-dir', `mon-dir-nef-big', `mon-dir-nef-converge',
`mon-dir-nef-keep-3', `mon-dir-nef-name-to-head', `mon-dir-nef-ranges',
`mon-dir-nef-conc-dups', `mon-dir-nef-find-dups', `mon-dir-nef-remove-if-empty',
`*mon-nefs_photos_nefs-alist*', `*mon-nef-scan-nefs-path*'.\n►►►"
  (let ((not-empt (mapcar #'(lambda (mdncr-L-1) (nreverse mdncr-L-1)) 
                          (mon-dir-nef-remove-if-empty folder-alist)))
        (ranges (mapcar #'(lambda (mdncr-L-2) (nreverse mdncr-L-2))
                        (mon-dir-nef-ranges  folder-alist)))
        rangename>)
    (mapc #'(lambda (mdncr-L-3)
              (let ((mk-rngnm (assoc (car mdncr-L-3) not-empt))
                    new)
                (when mk-rngnm 
                  (setq new `(,(concat  
                                (cadr mk-rngnm) 
                                " |In-Range-> " 
                                (cadr mdncr-L-3)) 
                              ,(car mk-rngnm)))
                  (setq rangename> (cons new rangename>)))))
          ranges)
    rangename>))
;;
;;; :TEST-ME (mon-dir-nef-conc-ranges *mon-nefs_photos_nefs-alist*)


;; (declare-function cl::set-difference "mon-cl-compat" t t)

;;; ==============================
;;; :PREFIX "mdnk-"
;;; :CREATED <Timestamp: Saturday June 27, 2009 @ 04:27.24 PM - by MON>
(defun mon-dir-nef-keep-3 (folder-alist)
  "Return FOLDER-ALIST folder names as string as two string alist.\n
Return FOLDER-ALIST names identified as two elt 'empty' `nil' directories,
e.g. those formatted as:\nNNN_(NNNN-NNNN)\n1...3..........\n
These were removed from surrounding list by `mon-dir-nef-remove-if-empty' for use by
`mon-dir-nef-find-dups'. We need them back, so get them.\n
:EXAMPLE\n\n\(mon-dir-nef-keep-3 *mon-nefs_photos_nefs-alist*\)\n
:SEE-ALSO `mon-dired-nef-dir', `mon-dir-nef-big', `mon-dir-nef-converge',
`mon-dir-nef-name-to-head', `mon-dir-nef-conc-ranges', `mon-dir-nef-ranges',
`mon-dir-nef-conc-dups', `mon-dir-nef-find-dups', `mon-dir-nef-remove-if-empty',
`*mon-nefs_photos_nefs-alist*', `*mon-nef-scan-nefs-path*'.\n►►►"
  (eval-when-compile (require 'mon-cl-compat nil t)) ;; <- `cl::set-difference'
  (let* ((mdnk-lst-1 (mapcar #'(lambda (mdnk-L-1) 
                         (cadr mdnk-L-1)) 
                     (mon-dir-nef-remove-if-empty folder-alist)))
         (mdnk-lst-2 (mapcar #'(lambda (mdnk-L-2) 
                         (nreverse mdnk-L-2)) 
                     (mon-dir-nef-ranges folder-alist)))
         (mdnk-lst-3 (mapcar #'(lambda (mdnk-L-3) (car mdnk-L-3)) mdnk-lst-2))
         ;; :WAS (mdnk-lst-4 (if (fboundp 'cl::set-difference)
         ;;     (cl::set-difference mdnk-lst-3 mdnk-lst-1)
         ;;   (cl::set-difference mdnk-lst-3 mdnk-lst-1))))
         ;; :TODO use `mon-set-difference' instead.
         ;; :WAS (mdnk-lst-4 (cl::set-difference mdnk-lst-3 mdnk-lst-1))
         (mdnk-lst-4 (mon-set-difference mdnk-lst-3 mdnk-lst-1 'equal))
         (mdnk-lst-5 (mapcar #'(lambda (mdnk-L-4) (assoc mdnk-L-4 mdnk-lst-2)) mdnk-lst-4))
         (mdnk-lst-6 (mapcar #'(lambda (mdnk-L-5)
                         (nreverse mdnk-L-5)) 
                     (mon-dir-nef-name-to-head folder-alist)))
         mdnk-lst-7)
    (mapc #'(lambda (mdnk-L-6)
              (let ((range-match (cadr (assoc mdnk-L-6 mdnk-lst-5)))
                    (folder-match (cadr (assoc mdnk-L-6 mdnk-lst-6))))
                (when (and folder-match range-match)
                  (setq mdnk-lst-7 
                        (cons `(,(concat range-match " |In Folder-> " folder-match) 
                                ,mdnk-L-6) mdnk-lst-7)))))
          mdnk-lst-4)
    mdnk-lst-7))
;;
;;; :TEST-ME (mon-dir-nef-keep-3 *mon-nefs_photos_nefs-alist*)

;;; ==============================
;;; :PREFIX "mdnb-"
;;; :CREATED <Timestamp: Saturday June 27, 2009 @ 04:27.24 PM - by MON>
(defun mon-dir-nef-big (folder-alist)
  "Return FOLDER-ALIST as one 'BIG' combined alist.\n
Return value includes those formatted with:\n
 `mon-dir-nef-converge' `mon-dir-nef-conc-ranges' `mon-dir-nef-keep-3'\n
:CALLED-BY `mon-dired-nef-dir', `mon-dired-naf-image-dir'\n
:SEE-ALSO `mon-dir-nef-keep-3', `mon-dir-nef-name-to-head', `mon-dir-nef-ranges',
`mon-dir-nef-conc-dups', `mon-dir-nef-find-dups', `mon-dir-nef-remove-if-empty',
`*mon-nefs_photos_nefs-alist*', `*mon-nef-scan-nefs-path*'.\n►►►"
  (let (mdnb-newp)
    (setq mdnb-newp (nconc 
                     (mon-dir-nef-converge folder-alist)
                     (mon-dir-nef-conc-ranges folder-alist)
                     (mon-dir-nef-keep-3 folder-alist)
                     mdnb-newp))
    mdnb-newp))
;;
;;; :TEST-ME (mon-dir-nef-big *mon-nefs_photos_nefs-alist*)


(when (and (intern-soft "*mon-nef-scan-nefs-path*" obarray) ;; *IS-MON-OBARRAY*
           (bound-and-true-p *mon-nef-scan-nefs-path*))
;;; ==============================
;;; :CREATED <Timestamp: Saturday June 27, 2009 @ 04:27.24 PM - by MON>
(defun mon-dired-nef-dir ()
  "Dired to one of the files in `*mon-nef-scan-nefs-path*'.\n
Prompts directory using completions generated `*mon-nefs_photos_nefs-alist*'.\n
:NOTE Use `mon-dired-naf-image-dir' for extended dir options.\n
:SEE-ALSO `mon-dired-other-window', `mon-dired-nef-dir', `mon-dir-nef-big',
`mon-dir-nef-converge', `mon-dir-nef-keep-3', `mon-dir-nef-name-to-head',
`mon-dir-nef-conc-ranges', `mon-dir-nef-ranges', `mon-dir-nef-conc-dups',
`mon-dir-nef-find-dups', `mon-dir-nef-remove-if-empty'.\n►►►"
  (interactive)
  (let ((nef-alist (mon-dir-nef-big *mon-nefs_photos_nefs-alist*))
        get-nef-dir)
    (setq get-nef-dir
          (cadr (assoc-string 
                 ;; :WAS (completing-read "Which nef directory (TAB completes): " nef-alist)
                 (completing-read 
                  (concat  ":FUNCTION `mon-dired-nef-dir' "
                           "-- which NEF directory (TAB completes): ") nef-alist)
                 nef-alist)))
    (dired (concat *mon-nef-scan-nefs-path* "/" get-nef-dir))))
;;
;;; :TEST-ME (mon-dired-nef-dir)
;;; :TEST-ME (call-interactively 'mon-dired-nef-dir)
;;
) ;; :CLOSE when `*mon-nef-scan-nefs-path*'

;;; ==============================
;;; :TODO Incorporate: (file-directory-p filename)
;;; :WORKING :NOT-FINISHED-AS-OF
;;; :CREATED <Timestamp: Friday May 15, 2009 @ 04:49.17 PM - by MON>
(defun mon-dir-hash-images (&optional dir-to-hash)
  "Hash the img dirs in path.\n
When non-nil DIR-TO-HASH supplies an alternate img directory to hash.
Defaults to value of global variable `*mon-img-hash*'.\n
:SEE-ALSO `mon-dir-try-comp', `mon-dir-hashed-complete', `mon-dir-build-list',
`mon-dir-hash-images'.\n►►►"
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
    (setq img-hash *mon-img-hash*) ;;(setq img-hash *temp-hash*) 
    (while dir-list
      (let ((in-dir (car dir-list)))
	(puthash in-dir '() img-hash)
	(setq dir-list (cdr dir-list))))
    (setq dir-list (mon-hash-all-keys *mon-img-hash*)) ;*mon-img-hash* ;*temp-hash*
    (while dir-list
      (let* ((in-dir (car dir-list))
             ;; Get partial-path of the .bmps in directory.
     	     (has-imgs (mon-get-ebay-bmps-in-dir t in-dir)) 
	     ;; (file-expand-wildcards (concat (file-name-as-directory in-dir "*.bmp"))
	     ;; (gethash 'last-hashed *mon-img-hash*) ;*temp-hash*
	     ;; Check the file for modification since last.
	     ;; (file-attributes file-to-examine
	     ;;		 (> mod-times now-time)
	     ;; (decode-time (cadr 
             ;;              (gethash (concat *mon-ebay-images-bmp-path* "/e1140") *temp-hash*))) 
	     ;; (decode-time (cadr (gethash SOME-HASH-ELT *temp-hash*))) 
	     )
	(if has-imgs
	    (puthash in-dir `(,has-imgs ,(current-time)) img-hash)
            (puthash in-dir `(,() ,(current-time)) img-hash))
	(setq dir-list (cdr dir-list))))
    ;; `unless' is _NOT_ what we want here - unless (> curr-time some-heuristic)
    (unless (gethash "last-hashed" img-hash) ;*mon-img-hash*) ;*temp-hash* 
      (puthash last-hashed hashed-on img-hash)) 
    img-hash))

;;; ==============================
;;; :WORKING NOT-FINISHED-AS-OF:
;;; :CREATED <Timestamp: Tuesday May 19, 2009 @ 05:55.58 PM - by MON>
(defun mon-dir-hashed-complete (comp-hsh dir-string common-string)
  "Return a buffer displaying possible completions for DIR-STRING in COMP-HSH.\n
COMP-HSH \(a hash-table\) should contain a common substring COMMON-STRING.\n
:SEE-ALSO `mon-dir-hash-images', `mon-dir-try-comp', `mon-dir-build-list'.\n►►►"
  (let ((compl (mon-hash-all-keys comp-hsh)) ; *temp-hash*))
	(dir-st dir-string)                  ; *mon-ebay-images-path*
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
;;; :TEST-ME (mon-dir-hashed-complete *mon-img-hash* (concat *mon-ebay-images-path* "/") "e1")

;;; ==============================
;;; :TODO Better heuristics on PTH and COLLECTION.
;;;       COLLECTION should build an a-list rather than defaulting to the hash.
;;; :TESTING-AS-OF
;;; :CREATED <Timestamp: Thursday May 21, 2009 @ 02:28.56 PM - by MON>
(defun mon-dir-try-comp (str &optional pth comp-collection)
  "Best completion of string STR in directory PTH using COMP-COLLECTION.\N
When non-nil PTH is a path name default is `*mon-ebay-images-bmp-path*'.\n
When non-nil COMP-COLLECTION is a list of direotories in PTH.\n
Default is `*mon-img-hash*'.\n
List value built with `mon-dir-build-list' per completion specs.\n
:SEE-ALSO `mon-dir-hashed-complete', `mon-dir-hash-images'.\n►►►"
  (let* ((comp-str str)
	 (path (if pth 
		   (directory-file-name pth)
		 (directory-file-name *mon-ebay-images-bmp-path*)))
	 (combo (concat path "/" str))
	 (in-coll (if comp-collection (mon-dir-build-list pth) *mon-img-hash*))) ; *temp-hash*
    (try-completion combo in-coll)))
;;
;;; :TEST-ME (mon-dir-try-comp "e1" *mon-ebay-images-jpg-path* t)
;;
;;;(progn (fmakunbound 'mon-dir-try-comp) (unintern 'mon-dir-try-comp) )


;; ,---- :UNCOMMENT-BELOW-TO-TEST
;; | (mon-dir-nef-ranges             *mon-nefs_photos_nefs-alist*)
;; | (mon-dir-nef-name-to-head       *mon-nefs_photos_nefs-alist*)
;; | (mon-dir-nef-remove-if-empty    *mon-nefs_photos_nefs-alist*)
;; | (mon-dir-nef-find-dups          *mon-nefs_photos_nefs-alist*)
;; | (mon-dir-nef-conc-dups          *mon-nefs_photos_nefs-alist*)
;; | (mon-dir-nef-converge           *mon-nefs_photos_nefs-alist*)
;; | (mon-dir-nef-conc-ranges        *mon-nefs_photos_nefs-alist*)
;; | (mon-dir-nef-keep-3             *mon-nefs_photos_nefs-alist*)
;; `----

;; 
(when (and (intern-soft "*bug-HG-path*" obarray)  ;; *IS-MON-OBARRAY*
           (bound-and-true-p *bug-HG-path*))
;;; ==============================
;;; :CREATED <Timestamp: Tuesday July 21, 2009 @ 05:36.07 PM - by MON>
(defun mon-file-path-for-bug (&optional file-name-path insrtp yankp intrp)
  "Provide portable file-name-path for BUGd systems.\n
FILE-NAME-PATH \(a string\) should be a full pathname string and be located 
beneath `*mon-emacs-root*'.\n
When `*bug-HG-path*' is local return a path suitable for a remote machine.
When `*bug-HG-path*' is remote return a path suitable for the local machine.\n
:SEE-ALSO `mon-get-local-url-for-bug'.\n►►►"
  (interactive "i\ni\nP\np")
  (let* ((fnp-tst (cond ((not file-name-path) "Path not under ")
                        ((and (not intrp) file-name-path)
                         (if (and (file-exists-p file-name-path)
                                  (string-match-p *mon-emacs-root* file-name-path))
                             file-name-path
                           "Path not under "))))
         (fnp (cond ((not (string-match "Path not" fnp-tst)) fnp-tst)
                    ((or intrp (string-match "Path not" fnp-tst))
                     (expand-file-name
                      (read-file-name 
                       (if (string-match "Path not" fnp-tst)
                           (concat fnp-tst *mon-emacs-root* " :")
                         "Which path shall we build?: ")
                       (expand-file-name default-directory)
                       (if (mon-buffer-written-p)
                           buffer-file-name
                           (expand-file-name default-directory))
                       t
                       (if (mon-buffer-written-p)
                           (file-name-nondirectory buffer-file-name)))))))
         (fnp-rel (concat "/" (file-relative-name fnp *mon-emacs-root*))))
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
;;; :TEST-ME (mon-file-path-for-bug (concat *mon-naf-mode-notes* "/latin-lorum-ipsum.dbc") t t)
;;; :TEST-ME (mon-file-path-for-bug (concat *mon-naf-mode-notes* "/latin-lorum-ipsum.dbc" t) 

;;; ==============================
;;; :RENAMED `mon-local-url-for-bug'  -> `mon-get-local-url-for-bug'
;;; :RENAMED `mon--local-url-for-bug' -> `mon--local-url-for-bug'
;;; :CREATED <Timestamp: Wednesday July 15, 2009 @ 02:19.43 PM - by MON>
(defun mon--local-url-for-bug (is-url file-string)
  "Helper function for interactive `mon-get-local-url-for-bug'.\n
:SEE-ALSO `mon-get-local-url-for-bug' and `*bug-HG-path*'.\n►►►"
  (concat "file:///" *bug-HG-path* "/" file-string))
;;
(defun mon-get-local-url-for-bug (file-string)
  "Prompt for a url to concat onto the common path shared between the MON KEY 
machine and the BUG'd system.\n
:SEE-ALSO `mon--local-url-for-bug', `*bug-HG-path*'.\n►►►"
  (interactive (list (read-string (format "Url beneath file: %s" 
                                  (concat (nth 6 (assoc 3 *mon-emacsd*)) "/")))))
   ;; (prin1 (mon--local-url-for-bug file-string) (current-buffer))
  (prin1 (mon--local-url-for-bug nil file-string)  (current-buffer)))
;;
;;; :TEST-ME (mon--local-url-for-bug "bubba")
;;; :TEST-ME (call-interactively 'mon-get-local-url-for-bug)
;;
) ;; CLOSE when `*bug-HG-path*'


;;; ==============================
(provide 'mon-dir-utils-local)
;;; ==============================

 
;; Local Variables:
;; mode: EMACS-LISP
;; generated-autoload-file: "./mon-loaddefs.el"
;; coding: utf-8
;; End:

;;; ====================================================================
;;; mon-dir-utils-local.el ends here
;;; EOF
