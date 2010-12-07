;;; mon-image-utils.el --- interfacing with image manipulation processes
;; -*- mode: EMACS-LISP; -*-

;;; ================================================================
;; Copyright © 2010 MON KEY. All rights reserved.
;;; ================================================================

;; FILENAME: mon-image-utils.el
;; AUTHOR: MON KEY
;; MAINTAINER: MON KEY
;; CREATED: 2010-11-20T19:05:38-05:00Z
;; VERSION: 1.0.0
;; COMPATIBILITY: Emacs23.*
;; KEYWORDS: external, external, files, processes

;;; ================================================================

;;; COMMENTARY: 

;; =================================================================
;; DESCRIPTION:
;; mon-image-utils provides utility procedures for interfacing with image
;; manipulation processes. 
;;
;; FUNCTIONS:►►►
;; `mon-image-rotate', `mon-image-identify', `mon-image-verify-type',
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
;; `*mon-image-identify-delim*',
;;
;; GROUPS:
;; `mon-image-utils'
;;
;; ALIASED/ADVISED/SUBST'D:
;; `mon-identify-image'          -> `mon-image-identify'
;; `mon-rotate-images'           -> `mon-image-rotate'
;; `boxcutter-verify-image-type' -> `mon-image-verify-type'
;; :NOTE Aliases defined in mon-aliases.el
;;
;; DEPRECATED:
;;
;; RENAMED:
;; `boxcutter-verify-image-type'  -> `mon-image-verify-type'
;;
;; MOVED:
;; `boxcutter-verify-image-type'     -> `mon-image-verify-type'
;; `boxcutter-verify-image-type'     <- mon-boxcutter.el, mon-utils.el
;;
;; TODO:
;;
;; NOTES:
;;
;; SNIPPETS:
;;
;; REQUIRES:
;;
;; lisp/image.el
;; mon-utils.el      (URL `http://www.emacswiki.org/emacs/mon-utils.el')
;; mon-time-utils.el (URL `http://www.emacswiki.org/emacs/mon-time-utils.el')
;;
;; ImageMagick, see discussion in :FILE mon-boxcutter.el for details
;;
;; THIRD-PARTY-CODE:
;;
;; URL: http://www.emacswiki.org/emacs/mon-image-utils.el
;; FIRST-PUBLISHED: <Timestamp: #{2010-11-20T19:16:47-05:00Z}#{10466} - by MON>
;;
;; EMACSWIKI: { URL of an EmacsWiki describing mon-image-utils. }
;;
;; FILE-CREATED:
;; <Timestamp: #{2010-11-20T19:05:38-05:00Z}#{10466} - by MON KEY>
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

(declare-function mon-g2be                 "mon-buffer-utils"      (&optional min/max-go no-go))
(declare-function mon-string-from-sequence "mon-utils"      (stringify-seq &rest other-seqs))
(declare-function mon-string-after-index   "mon-utils"      (in-str after-str))
(declare-function mon-string-to-symbol     "mon-utils"      (str-to-sym &optional start end))
(declare-function mon-format-iso-8601-time "mon-time-utils" (&optional time insrtp intrp))

;;; ==============================
;;; :CHANGESET 2321
;;; :CREATED <Timestamp: #{2010-11-20T19:24:47-05:00Z}#{10466} - by MON KEY>
(defgroup mon-image-utils nil
  "Customizations for mon-image-utils related features.\n
►►►"
  :link  '(url-link :tag ":EMACSWIKI-FILE" "http://www.emacswiki.org/emacs/mon-image-utils.el")
  :link  '(emacs-library-link "mon-image-utils.el")
  :prefix "mon-image-"
  :group 'mon-base)


;;; ==============================
;;; :CHANGESET 2317
;;; :CREATED <Timestamp: #{2010-11-20T13:06:48-05:00Z}#{10466} - by MON KEY>
(defcustom *mon-image-identify-delim* (make-string 68 126)
  "Delimiter inserted into buffer named \"*IDENTIFY*\" by `mon-identify-image'.\n
Used to separate records when multiple records are generated into the buffer.\n
Default is value of:\n
 \(make-string 68 126\)\n
:SEE-ALSO `mon-image-rotate', `mon-image-verify-type'.\n►►►"
  :type  'string
  :group 'mon-image-utils)

;;; ==============================
;;; :PREFIX "mii-"
;;; :CHANGESET 2317
;;; :CREATED <Timestamp: #{2010-11-19T18:39:04-05:00Z}#{10465} - by MON KEY>
(defun mon-image-identify (image-name &optional no-verbose)
  "Return value of ImageMagick's `identify` command for IMAGE-NAME.\n
Return results at eob in buffer named \"*IDENTIFY*\" delimited with value of
`*mon-image-identify-delim*'.\n
When optional arg NO-VERBOSE is non-nil do not pass `identify` the -verbose flag.\n
:EXAMPLE\n
\(let \(\(default-directory doc-directory\)
      emacs-png\)
  \(setq emacs-png
        \(file-expand-wildcards \"images/icons/hicolor/128x128/apps/*.png\" t\)\)
  \(when emacs-png \(setq emacs-png \(car emacs-png\)\)
        \(mon-identify-image emacs-png\)
        \(mon-identify-image emacs-png t\)\)\)
:ALIASED-BY `mon-identify-image'\n
:SEE-ALSO `mon-image-verify-type', `mon-image-rotate',
`*mon-image-identify-delim*'.\n►►►"
  ;; :NOTE weird stuff seems to happen around the combined interactions of
  ;; `shell-command-to-string', `display-buffer', and `with-current-buffer'
  ;; following works... So, don't go trying to improve it without first
  ;; understanding the root of the funk.
  (let ((mii-get-id
         (shell-command-to-string 
          (format "identify %s %s" (or (and no-verbose "") "-verbose")  image-name)))
        (mii-id-bfr (get-buffer-create "*IDENTIFY*"))
        (mii-mrk (make-marker)))
    (with-current-buffer mii-id-bfr      
      (when (= (buffer-size) 0) 
        (insert *mon-image-identify-delim*"\n"))
      (mon-g2be (set-marker mii-mrk (mon-g2be 1 t) mii-id-bfr))
      (insert mii-get-id "\n" *mon-image-identify-delim* "\n")
      (mon-g2be mii-mrk)
      (insert ":IDENTITY-AT <Timestamp: " (mon-format-iso-8601-time) ">\n\n")
      (set-marker mii-mrk nil)
      ;; :NOTE Following doesn't work, must use `pop-to-buffer' outside
      ;; `with-current-buffer':
      ;; (display-buffer (get-buffer-create "*IDENTIFY*"))
      )
    (pop-to-buffer mii-id-bfr nil t)
    mii-get-id))

;;; ==============================
;;; :TODO Should report `identify` -verbose <FILENAME> upon completion per file
;;; :TODO Should verify that all images in `rotate-list' exist and are readable.
;;; :PREFIX "mir-"
;;; :CHANGESET 2317
;;; :CREATED <Timestamp: #{2010-11-19T17:18:00-05:00Z}#{10465} - by MON KEY>
(defun mon-image-rotate (rotate-list rotate-degrees)
  "Rotate images in ROTATE-LIST by ROTATE-DEGREES.\n
ROTATE-LIST is a list of strings namiming filename paths of images to rotate.\n
ROTATE-DEGREES an integer value rotate images by, e.g. -90, 90, 180, 270, etc.
Rotation is performed with `call-process-shell-command' with ImageMagick's
`convert` command with arg `-rotate` e.g.:
shell> convert <FILENAME> -rotate <INT> <FILENAME>\n
:EXAMPLE\n\n (mon-rotate-images '(\"/some/image/file.jpeg\" 
                                  \"/some/image/file2.jpeg\") 90)
:ALIASED-BY `mon-rotate-images'\n
:SEE `jpegtran` for an alternate command.\n
:SEE-ALSO `mon-image-identify'.\n►►►"  
  (dolist (mir-rt rotate-list)
    (call-process-shell-command "convert" nil nil nil 
                                (format "%s -rotate %d %s" mir-rt rotate-degrees mir-rt))))

;;; ==============================
;;; :NOTE Following performs some image extension-type checking. 
;;;       May be useful elsewhere as well.
;;; :REQUIRES `image-type-available-p', `image-type-from-file-name' in :FILE image.el
;;; :MODIFICATIONS <Timestamp: #{2010-05-28T13:40:43-04:00Z}#{10215} - by MON>
;;; :CREATED <Timestamp: #{2009-10-17T17:58:20-04:00Z}#{09426} - by MON>
(defun mon-image-verify-type (verify-image-type)
  "Verify that image-type VERIFY-IMAGE-TYPE can be manipulated with ImageMagick's convert.\n
Return a canonical representation for VERIFY-IMAGE-TYPE.\n
Valid VERIFY-IMAGE-TYPE args are:\n
 {jpg png gif tiff jpeg xpm xbm pbm bmp psd}\n
:NOTE VERIFY-IMAGE-TYPE is not case sensitive, can be a string or symbol, and
any leading `.' in the file extension will be stripped away.\n
:EXAMPLE\n\(mon-image-verify-type 'jpg\)
\(mon-image-verify-type '.jpg\)
\(mon-image-verify-type \"jpg\"\)
\(mon-image-verify-type \".jpg\"\)
\(mon-image-verify-type \"I WILL FAIL with nil\"\)
\(mon-image-verify-type '\(I WILL FAIL with error\)\)\n
:NOTE Any potentially any file format that is RW by ImageMagick's 
convert command could be supported. For a complete list of formats supported:
:SEE (URL `http://www.imagemagick.org/script/formats.php')
:CALLED-BY `boxcutter-big-n-small'\n
:ALIASED-BY `boxcutter-verify-image-type' \(when \(and \(featurep 'mon-boxcutter\)\)\)\n
:SEE-ALSO `mon-check-image-type', `*boxcutter-conversion-program*',
`image-type-available-p', `image-type-from-file-name',
`image-file-name-extensions', `image-type-file-name-regexps',
`image-file-name-regexps'.\n►►►"
  (eval-when-compile (require 'image))
  (let ((v-type verify-image-type))
    (car (member
          (cond ((stringp v-type)
                 (if (image-type-from-file-name
                      (let* ((v-type-str v-type)
                             (chk-ftyp (string-match-p "\\." v-type-str)))
                        (if chk-ftyp
                            v-type-str
                          (setq v-type-str (concat "." v-type-str)))))
                     (let* ((ext-is-str (downcase v-type)) ;we still need to check for leading `.'
                            (ext-seq (mon-string-to-sequence ext-is-str)))
                       (mon-string-to-symbol
                        (if (eq (elt ext-seq 0) 46)
                            (mon-string-from-sequence (cdr ext-seq))
                          ext-is-str)))))
                ((symbolp v-type)
                 (let* ((f-sym-str (mon-string-from-symbol v-type))
                        (chk-ftyp (string-match-p "\\." f-sym-str)))
                   (if chk-ftyp
                       (if (<= chk-ftyp 1)
                           f-sym-str
                         (setq f-sym-str (concat "." f-sym-str)))
                     f-sym-str)
                   (if (image-type-available-p
                        (image-type-from-file-name f-sym-str))
                       (mon-string-to-symbol
                        (mon-string-after-index f-sym-str "."))
                     v-type)))
                ((or (listp v-type) (vectorp v-type)
                     (and (stringp v-type) (arrayp v-type)))
                 (error (concat ":FUNCTION `mon-image-verify-type' "
                                "-- %s is a %s - not a valid arg for `verify-image-type'")
                        v-type (type-of v-type)))
                (t (error (concat  ":FUNCTION `mon-image-verify-type' "
                                   "-- this argument sux no further type checking try again"))))
          ;; :NOTE Add svg now that Emacs 24 has native image-magick/libxml support?
          '(jpg png gif tiff jpeg xpm xbm pbm bmp)))))
;;
;;; :TEST-ME (mon-image-verify-type 'jpg)
;;; :TEST-ME (mon-image-verify-type '.jpg)
;;; :TEST-ME (mon-image-verify-type "jpg")
;;; :TEST-ME (mon-image-verify-type ".jpg")
;;; :TEST-ME (mon-image-verify-type "I WILL FAIL with nil")
;;; :TEST-ME (mon-image-verify-type '(I WILL FAIL with error))

;;; ==============================
(provide 'mon-image-utils)
;;; ==============================

 
;; Local Variables:
;; mode: EMACS-LISP
;; coding: utf-8
;; generated-autoload-file: "./mon-loaddefs.el"
;; End:

;;; ====================================================================
;;; mon-image-utils.el ends here
;;; EOF
