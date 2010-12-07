;;; mon-rename-image-utils.el --- utility fncns for image manipulation
;; -*- mode: EMACS-LISP; -*-

;;; ================================================================
;; Copyright © 2009, 2010 MON KEY. All rights reserved.
;;; ================================================================

;; FILENAME: mon-rename-image-utils.el
;; AUTHOR: MON KEY
;; MAINTAINER: MON KEY
;; CREATED: 2009-06-24T19:43:55-04:00Z
;; VERSION: 1.0.0
;; COMPATIBILITY: Emacs23.*
;; KEYWORDS: external, execute, hypermedia, multimedia, tools,

;;; ================================================================

;;; COMMENTARY: 

;; =================================================================
;; DESCRIPTION:
;; mon-rename-image-utils provides utility functions for converting, renaming
;; and extracting usefull information from images and Emacs Image manipulation.
;;
;; FUNCTIONS:►►►
;; `mon-check-image-type', `mon-rename-imgs-in-dir', `mon-check-image-type',
;; `mon-ebay-image-directory-not-ok', `mon-ebay-image-directory-ok-p',
;; `mon-image-rename-propertize', `mon-parse-rename-images',
;; `mon-shorten-rename-image-path', `mon-parse-rename-lengths',
;; `mon-pad-rename-lengths', `mon-build-rename-buffer' 
;; `mon-get-image-dimensions', `mon-get-image-dimensions-im',
;; `mon-get-image-md5',
;; FUNCTIONS:◄◄◄
;;
;; :EXTERNAL-FUNCTIONS
;; `mon-get-buffer-parent-dir'         -> mon-dir-utils.el
;; `mon-string-split-buffer-name'      -> mon-dir-utils.el
;; `mon-truncate-path-for-prompt'      -> mon-dir-utils.el
;; `mon-buffer-written-p'              -> mon-dir-utils.el
;; `mon-toggle-read-only-point-motion' -> mon-utils.el
;; `mon-dir-build-list'                -> mon-dir-utils.el
;; `mon-line-bol-is-eol'               -> mon-utils.el
;; `mon-delete-back-up-list'           -> mon-replacement-utils.el
;; `mon-cln-trail-whitespace'          -> mon-replacement-utils.el
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
;;
;; :EXTERNAL-VARIABLES
;; `*mon-ebay-images-lookup-path*'         -> mon-dir-locals-alist.el
;; `*mon-nef-scan-path*'                   -> mon-dir-locals-alist.el
;;
;; ALIASED/ADVISED/SUBST'D:
;; `get-bmps-in-dir'        ->  `mon-get-ebay-bmps-in-dir'
;; `mon-get-ebay-bmps-list' ->  `mon-get-ebay-bmps-in-dir'
;; `get-nefs-in-dir'        ->  `mon-get-nefs-in-dir'
;; :NOTE Aliases defined in :FILE mon-aliases.el
;;
;; DEPRECATED:
;;
;; RENAMED:
;;
;; MOVED:
;; `mon-ebay-image-directory-not-ok'    <- mon-dir-utils.el
;; `mon-ebay-image-directory-ok-p'      <- mon-dir-utils.el
;; `mon-get-image-dimensions'           <- mon-url-utils.el
;; `mon-get-image-dimensions-im'        <- mon-url-utils.el
;; `mon-get-image-md5'                  <- mon-url-utils.el
;; `mon-get-ebay-bmps-in-dir'           <- ebay-template-tools.el
;; `mon-get-nefs-in-dir'                <- ebay-template-tools.el
;; `mon-get-ebay-jpgs-list'             <- ebay-template-tools.el
;; `mon-insert-ebay-jpgs-in-file'       <- ebay-template-tools.el
;; `mon-get-ebay-jpgs-count'            <- ebay-template-tools.el
;; `mon-get-ebay-bmps-count'            <- ebay-template-tools.el
;; `mon-get-ebay-img-count-verify'      <- ebay-template-tools.el
;; `mon-cln-img-magic-hex'              <- ebay-template-tools.el
;; `mon-get-ebay-img-css'               <- ebay-template-tools.el
;; `mon-get-ebay-img-name-to-col'       <- ebay-template-tools.el
;; `mon-get-ebay-css-pp-region-to-file' <- ebay-template-tools.el	
;; `mon-get-ebay-css-pp'                <- ebay-template-tools.el
;; `mon-insert-css-colors'              <- ebay-template-tools.el
;;
;; REQUIRES:
;; mon-css-color.el (or the css-color.el provided with nXhtml)
;; :SEE (URL `http://www.emacswiki.org/emacs/mon-css-color.el')
;;
;; mon-dir-locals-alist.el 
;;  `*mon-nef-scan-drive*', `*mon-nef-scan-path*', `*mon-nef-scan-nefs-path*', 
;;  `*mon-nef-scan-nef2-path*', `*mon-ebay-images-path*', `*mon-ebay-images-jpg-path*' 
;;  `*mon-ebay-images-bmp-path*'
;; :SEE (URL `http://www.emacswiki.org/downloads/mon-dir-locals-alist.el')
;;
;; mon-utils.el
;; | -> `mon-toggle-read-only-point-motion', `mon-line-bol-is-eol',
;; | -> `mon-mapcar', `mon-g2be'
;; :SEE (URL `http://www.emacswiki.org/emacs/mon-utils.el')
;;
;; mon-dir-utils.el
;; | -> `mon-get-buffer-parent-dir', `mon-string-split-buffer-name'
;; | -> `mon-truncate-path-for-prompt', `mon-buffer-written-p'
;; | -> `mon-dir-build-list'
;; :SEE (URL `http://www.emacswiki.org/emacs/mon-dir-utils.el')
;;
;; mon-replacement-utils.el
;; |-> `mon-delete-back-up-list', `mon-cln-trail-whitespace'
;; :SEE (URL `http://www.emacswiki.org/emacs/mon-replacement-utils.el')
;; 
;; mon-rename-image-utils-supplemental.el 
;; :SEE (URL `http://www.emacswiki.org/emacs/mon-rename-image-utils-supplemental.el')
;;
;; :NOTE Additional discussion of required features is proved in the
;; installation and notes below.
;;
;; TODO:
;;
;; NOTES:
;; =================================================================
;; :INSTALLATION-AND-USAGE-NOTES
;;
;; :EXTRACT-CSS-COLOR-FROM-IMAGES
;; The Image-Color->CSS Extraction routines provided by this package are:
;; `mon-cln-img-magic-hex', `mon-get-ebay-img-css'
;; `mon-get-ebay-img-name-to-col', `mon-get-ebay-css-pp-region-to-file'
;; `mon-get-ebay-css-pp', `mon-insert-css-colors'
;;
;; This is a novel series of procedures which extract the most common colors in
;; an image and spit those out to a new buffer as CSS hex values. They are
;; useful (for example) for generation of per page CSS color schemes according
;; to a particular image or set of images.  For example, I have a set of three
;; images that I would like to use in an eBay auction listing and I would like
;; to build a one time color scheme/style for that listing according the colors
;; common to those three images.
;;
;; Another possible use for these routines, would be to identify and extract an
;; appropriate color for watermarking an image(s).
;;
;; :USE-OF-THE-PROVIDED-UTILITY
;; The bulk of this work is performed with `mon-get-ebay-img-css'.  This is
;; essentially inlined elisp image-magik  script which analyzes the images by: 
;; Reducing-image-size -> Posterizing-image -> Extracting-image-colors
;;
;; The basic routine is as follows: 
;;
;; Evaluating: (mon-get-ebay-css-pp)
;;
;; In, for example, the directory (or buffer's `default-directory'):
;; "/NEFS_PHOTOS/Nef_Drive2/EBAY/BMP-Scans/e1002/BMP" <- 
;;
;; Where that directory originally contained following .bmps: 
;; e1002-0.bmp e1002-1.bmp e1002-2.bmp e1002-3.bmp e1002-4.bmp e1002-5.bmp
;;
;; These are converted to low resolution .pngs: e1002-0.bmp -> e1002-0.png
;; e1002-1.bmp -> e1002-1.png e1002-2.bmp -> e1002-2.png
;; e1002-3.bmp -> e1002-3.png e1002-4.bmp -> e1002-4.png
;; e1002-5.bmp -> e1002-5.png
;;
;; Don't worry, the .bmps aren't destroyed! The .png's are new file instances.
;;
;; Next, an image-magick posterization routine is performed on the .png's.  The
;; output of this posterization is uniquified to 16 colors and returned as text.
;; The uniquified text output is then cleaned up in a temp buffer.  This is
;; written to a file "BMP-hex-colors" in the current directory.  A new buffer is
;; opened which displays the 16 most common colors for each of the .bmps->.png
;; conversions.  The display presented is as as a per image columnized display
;; and leverages `mon-css-color.el' for additional functionality to allow
;; tweaking the CSS values.
;;
;; :CSS-EXTRACTION-SCREENSHOTS
;; :SEE (URL `http://www.emacswiki.org/emacs/BMP-hex-colors-screenshot')
;; [picture: css color extraction buffer using `mon-get-ebay-css-pp' function.]
;; :SEE (URL `http://www.emacswiki.org/emacs/directory-and-hex-colors-annotated')
;; [picture: annotated image of css color extraction buffer and dired buffer
;; showing `mon-get-ebay-css-pp' function in use.]
;;
;; :RENAMING-FACILITIES
;; The image renaming facilities for generating an image renaming buffer are:
;;   `mon-image-rename-propertize' `mon-rename-imgs-in-dir'
;;   `mon-parse-rename-lengths' `mon-parse-rename-images'
;;   `mon-pad-rename-lengths' `mon-build-rename-buffer'
;;   `mon-shorten-rename-image-path' `mon-check-image-type'
;;   `mon-ebay-image-directory-not-ok' `mon-ebay-image-directory-ok-p'
;;
;; :NOTE All renaming routines aren't completed (yet).
;; Specifically, the text-property parsing of `mon-build-rename-buffer'.
;; This should be trivial but I haven't gotten to it yet.  Building the padding
;; routine winded me :(.
;;
;; :RENAMING-UTILITY-SCREENSHOT
;; :SEE (URL `http://www.emacswiki.org/emacs/rename-utility-screenshot')
;; [picture: 4 window split showing output of (mon-build-rename-buffer ".bmp")]
;;
;; :RENAME-UTILITY-USE
;; Basically, you set up your path vars for an image directory tree, then
;; assuming you are in a buffer-or-filename within that tree you can call:
;;
;;   (mon-build-rename-buffer ".bmp") 
;;   (mon-build-rename-buffer ".jpg")
;;   (mon-build-rename-buffer ".nef")
;;
;; and Emacs will either prompt for a better directory in the tree, or snarf
;; the image file names from the current directory and return them in a pretty
;; *rename-buffer* buffer full of text properties for futher processing the
;; images.
;;
;; As an example, say you are in the directory:
;;
;;  <YOUR-DRIVE-OR-ROOT:>/NEFS_PHOTOS/Nef_Drive2/EBAY/BMP-Scans/e1143/
;;  e.g. path variable `*mon-ebay-images-bmp-path*'/e1143/
;; 
;; or, in the buffer-file named:
;;  <YOUR-DRIVE-OR-ROOT:>/NEFS_PHOTOS/Nef_Drive2/EBAY/BMP-Scans/e1143/e1143.dbc
;;  e.g. path variable `*mon-ebay-images-jpg-path*'/e1143/e1143.dbc
;;
;; and you want to rename all of the ".jpg" files associated with the '.bmps" in
;; the current buffer-or-file's current directory e.g.:
;;
;;   .bmp's are in: 
;;    | -> <YOUR-DRIVE-OR-ROOT:>/NEFS_PHOTOS/Nef_Drive2/EBAY/BMP-Scans/e1143/ 
;;
;;   .jpg's are in: 
;;    | -> "<YOUR-DRIVE-OR-ROOT:>/NEFS_PHOTOS/Nef_Drive2/EBAY/BIG-cropped-jpg/e1143"
;; 
;; Evaluating the form: (mon-build-rename-buffer ".jpg") 
;; returns a 'rename-buffer' of all the .jpgs in the 'matching' .bmp directory.
;; If there aren't any .jpgs in that file it prompts for a new directory within
;; that tree.
;;
;; However, should you instead evaluate: (mon-build-rename-buffer ".bmp") 
;; Assuming there are .bmps in the current dir Emacs will instead return a
;; `*rename-images*' buffer with all the .bmp's in the 'current' directory ready
;; for marking for further processing.
;;
;; To get an idea of the types of follow up actions which might be taken
;; checkout the myriad text properties in the *rename-buffer* presentation to
;; get an idea of the types of processing that can be accomplished. i.e. call
;; the `describe-text-properties' command with point at different places in the
;; buffer. 
;;
;; If you have `mon-utils.el' in your load-path you can look at the read-only
;; buffer positions as well by evaluating the command:
;; `mon-toggle-read-only-point-motion' 
;; :SEE (URL `http://www.emacswiki.org/emacs/mon-utils.el')
;;
;; Currently `mon-build-rename-buffer' is only accepts the arg IMAGE-TYPE.
;;
;; The helper function `mon-rename-imgs-in-dir' takes an alternate path arg
;; ALT-PATH that will soon allow you to do:
;;  (mon-build-rename-buffer ".bmp" (expand-file-name "../e1214/")
;; i.e. build a *rename-images* buffer from files in some other dir.
;;
;; Assuming your var paths are set right the functions have fairly intelligent
;; heuristics for how they navigate the paths and include completion facilities
;; and nice prompts which attemtp to DWIM.
;;
;; I am particularly proud of the *rename-images* buffer generation code which
;; is smart about presentation padding according to the filename length of
;; images. Emacs-lisp `format' is not nearly as extensive a the format spec of
;; Common Lisp...
;;
;; :CONFIGURING-YOUR-SYSTEM-FOR-RENAME-UTILS
;;
;; :PATH-VARIABLES
;; The library mon-dir-locals-alist.el 
;; :SEE (URL `http://www.emacswiki.org/downloads/mon-dir-locals-alist.el')
;; supplies the following variables:
;;  `*mon-nef-scan-drive*', `*mon-nef-scan-path*', `*mon-nef-scan-nefs-path*', 
;;  `*mon-nef-scan-nef2-path*', `*mon-ebay-images-path*', `*mon-ebay-images-jpg-path*' 
;;  `*mon-ebay-images-bmp-path*'
;;
;; Starting with `*mon-nef-scan-drive*' the variables below will need to be adjusted
;; according to your local path and directory tree.  Ideally, one would mirror
;; those indicated below. On MON local system path variables relevant to
;; mon-rename-utils map out as follows:
;;
;; `*mon-nef-scan-drive*'
;;   | -> "<DRIVE-OR-ROOT:>" ; :NOTE Optional, will be used if/when defcustom'd
;; `*mon-nef-scan-path*'
;;   | -> "<DRIVE-OR-ROOT:>/NEFS_PHOTOS"
;; `*mon-nef-scan-nefs-path*'
;;   | -> "<DRIVE-OR-ROOT:>/NEFS_PHOTOS/NEFS"
;; `*mon-nef-scan-nef2-path*'
;;   | -> "<DRIVE-OR-ROOT:>/NEFS_PHOTOS/Nef_Drive2"
;; `*mon-ebay-images-path*'
;;   | -> "<DRIVE-OR-ROOT:>/NEFS_PHOTOS/Nef_Drive2/EBAY"
;; `*mon-ebay-images-bmp-path*'
;;   | -> "<DRIVE-OR-ROOT:>/NEFS_PHOTOS/Nef_Drive2/EBAY/BMP-Scans"
;; `*mon-ebay-images-jpg-path*' 
;;   | -> "<DRIVE-OR-ROOT:>/NEFS_PHOTOS/Nef_Drive2/EBAY/BIG-cropped-jpg"
;;
;; :NOTE The above variables there are not (yet) defined as `defcustom's b/c I
;; default these vars across multiple systems. A defcustom interface for
;; `mon-rename-image-utils' will be made available once development of this
;; package is no longer in flux. Stay tuned.
;;
;; :CSS-COLOR-EXTRACTION-REQUIREMENTS
;; I use a local copy of `css-color.el' named `mon-css-color.el' 
;; This is a modified (re)incarnation of Niels Giesen's `css-color.el' 
;; :VERSION "0.03" of 2008. 
;; :SEE (URL `http://www.emacswiki.org/emacs/mon-css-color.el')
;;
;; The css-color.el package was originally made available on the EmacsWiki: 
;; :SEE (URL `http://www.emacswiki.org/emacs/CssColor') 
;; However, the original css-color.el is no longer publically
;; available/maintained and an orphaned version of css-color.el was incorporated
;; as part of the with nXhtml suite. However, this is version is (needlessly)
;; locked up in that systems associated Bazaar repository and is not easily
;; accesible independently from the larger nxhtml suite, which means user
;; wanting only the css-color.el must download entire nxhtml suite just to
;; access the original css-color.el features. Additionally, we find that nxhtml
;; suite may (re)bind CSS related user hooks from other libraries in
;; un-expected/desired ways. As such, features which depend on mon-css-color.el
;; are not be 100% compatible with the version of css-color.el distributed with
;; nxhtml IOW and you should use mon-css-color.el
;;
;; :IMGAGE-MAGICK-REQUIREMENTS
;; You will also need a current Image Magick binary.  
;; :SEE (URL `http://www.imagemagick.org/')
;;
;; :CAVEATS-USING-IMAGE-MAGICK-ON-W32:
;; :NOTE the `mon-get-ebay-img-css' function calls out to ImageMagick's
;; convert command using the alias `imconvert' (as per the recommendations by
;; the Image-Magick distributors).
;;
;; It is strongly recommended that W32 users of Image Magick adopt this
;; practice.
;;
;; This is for W32 compatibility as convert.exe is a system level command
;; gnu-linux systems should substitute `convert' for `imconvert'. Likewise, on
;; W32 running Image Magick's `convert' as a shell-process (synchronous or
;; otherwise) is much more difficult to configure when this renaming isn't
;; provided on.
;;
;; To wit, this is needed because on a W32 system:
;;
;; (executable-find "convert") 
;;  => "c:/WINDOWS/system32/convert.exe"
;;
;; (executable-find "imconvert") 
;;  => "c:/Program Files/imagemagick-6.5.1-q8/imconvert.exe"
;;
;; For those that think this aliasing is unnecessary _PLEASE-NOTE_ "convert" is
;; a W32 command in the W32 system directory. It is used for converting FAT32
;; to NTFS. It is suggested you rename the IM convert command to
;; "imconvert.exe" to distinguish the two commands. You can't rename the system
;; command, as a Windoze service pack would just restore it, ignoring the
;; renamed version. For additional discussion:
;; :SEE (URL `http://www.imagemagick.org/Usage/windows/index.html')
;;
;; :REQUIRED/RECOMMENDED-PACKAGES
;; Additional discussion for and usage notes regarding integration of this
;; package into a local system is available in the header of the following file:
;; :SEE (URL `http://www.emacswiki.org/downloads/mon-rename-image-utils-supplemental.el')
;;
;; This `supplemental' package provides additional functions which may be
;; needed when using the current package (e.g. mon-rename-image-utils.el). 
;; The procedures of the supplemental package are esp. recommended if you aren't
;; already using the the litany of other `mon-*.el' packages e.g. those
;; packages are avaiable at: 
;; :SEE (URL `http://www.emacswiki.org/emacs/mon_key')
;;
;; =================================================================
;;
;; URL: http://www.emacswiki.org/emacs/mon-rename-image-utils.el
;; FILE-PUBLISHED: <Timestamp: #{2009-09-20} - by MON KEY>
;;
;; EMACSWIKI: http://www.emacswiki.org/emacs/RenameImageUtils
;; FILE-PUBLISHED: <Timestamp: #{2009-09-28} - by MON KEY>
;;
;; FILE-CREATED:
;; <Timestamp: #{2009-06-24T19:43:55-04:00Z} - by MON KEY>
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

(unless (and (intern-soft "*IS-MON-OBARRAY*")
             (bound-and-true-p *IS-MON-OBARRAY*))
(setq *IS-MON-OBARRAY* (make-vector 17 nil)))

;;; :NOTE The library mon-css-color is pulled in by a require in mon-dir-utils.el
(unless (featurep 'mon-css-color)
  (require 'css-color))

;;; ==============================
;;; :TODO Needs to take an interactive arg with `mon-get-imgs-in-dir-int'.
;;; :CREATED <Timestamp: Wednesday April 29, 2009 @ 04:03.46 PM - by MON KEY>
(defun mon-get-ebay-bmps-in-dir (&optional full-path alt-path)
  "Return a list of .bmps in file's directory.\n
When FULL-PATH is non-nil, return absolute file names. 
When ALT-PATH is non-nil use dir-path as value instead of current-buffers.
:CALLED-BY `mon-insert-ebay-bmps-in-file' to build a .bmp insertion list.
:SEE-ALSO `mon-get-ebay-nefs-in-dir', `mon-insert-ebay-bmps-in-file', 
`mon-get-ebay-jpgs-list', `mon-insert-ebay-jpgs-in-file', 
`mon-get-ebay-bmps-count', `mon-get-ebay-jpgs-count', 
`mon-get-ebay-bmps-in-dir'.\n►►►"
  (let (this-dir get-files)
    (if (and alt-path
	(file-exists-p alt-path))
	(setq this-dir (file-name-as-directory alt-path))
      (setq this-dir (file-name-directory buffer-file-name)))
    (setq get-files (directory-files this-dir full-path "\\.bmp"))
    get-files))
;;
;;; :TEST-ME  (mon-get-ebay-bmps-in-dir)
;;; :TEST-ME  (mon-get-ebay-bmps-in-dir t)

;;; ==============================
;;; :TODO Needs to take an interactive arg with `mon-get-imgs-in-dir-int' see below.
;;; :CREATED <Timestamp: Wednesday May 06, 2009 @ 04:32.45 PM - by MON KEY>
(defun mon-get-nefs-in-dir (&optional full-path alt-path)
  "Return a list of .nefs in buffers' directory.\n
When FULL-PATH is non-nil, return absolute file names. 
When ALT-PATH is non-nil use dir-path as value instead of current-buffers.\n
:CALLED-BY `mon-insert-ebay-bmps-in-file' to build a .nef insertion list.\n
:SEE-ALSO `mon-get-ebay-bmps-in-dir', `mon-insert-ebay-bmps-in-file', 
`mon-get-ebay-jpgs-list', `mon-insert-ebay-jpgs-in-file', `mon-get-ebay-bmps-count', 
`mon-get-ebay-jpgs-count', `mon-get-ebay-bmps-in-dir'.\n►►►"
  (let (this-dir get-files)
    (if (and alt-path
	(file-exists-p alt-path))	
	(setq this-dir (file-name-as-directory alt-path));(file-name-directory alt-path))
    (setq this-dir (file-name-directory buffer-file-name)))
    (setq get-files (directory-files this-dir full-path "\\.nef"))
    get-files))

;;; ==============================
;;; :CREATED <Timestamp: Wednesday April 29, 2009 @ 04:12.25 PM - by MON KEY>
(defun mon-insert-ebay-bmps-in-file (&optional full-path)
  "Insert in buffer names of .bmp files in buffers' directory.\n
When FULL-PATH non-nil insert full-path of .bmp files.\n
:SEE-ALSO  `*mon-ebay-template*', `mon-insert-ebay-dirs', 
`mon-insert-ebay-photo-per-scan-descr', `mon-get-ebay-bmps-in-dir'."
  (interactive "p")
  (let ((bmp-files (if full-path
		       (mon-get-ebay-bmps-in-dir t)
		     (mon-get-ebay-bmps-in-dir nil))))
    (while bmp-files 
      (insert (format "\n%s" (car bmp-files)))
	(setq bmp-files (cdr bmp-files)))))
;;
;;; :TEST-ME (mon-insert-ebay-bmps-in-file)
;;; :TEST-ME (mon-insert-ebay-bmps-in-file t)

;;; ==============================
;;; :CREATED <Timestamp: Wednesday April 29, 2009 @ 03:14.36 PM - by MON KEY>
(defun mon-get-ebay-jpgs-list (&optional full-path) 
  "Called from an ebay-template file, find items' .jpg path from relative dir.\n
:SEE-ALSO  `mon-insert-ebay-bmps-in-file', `mon-get-ebay-jpgs-list', 
`mon-insert-ebay-jpgs-in-file', `mon-get-ebay-bmps-count', 
`mon-get-ebay-jpgs-count', `mon-get-ebay-bmps-in-dir'."
  (let ((jpg-path (concat (expand-file-name "../../") "BIG-cropped-jpg/" 
		   (file-name-nondirectory (directory-file-name default-directory))))	
	(this-dir (file-name-nondirectory (directory-file-name default-directory)))
	collect-jpg-path)
    (if (file-exists-p jpg-path)
	(let ((in-path (directory-files jpg-path full-path ".jpg"))
	      walk-files)
	  (setq walk-files in-path)
	  (while walk-files
	    (add-to-list 'collect-jpg-path (car walk-files))
	    (setq walk-files (cdr walk-files))))
      nil)
    collect-jpg-path))
;;
;;; :TEST-ME (mon-get-ebay-jpgs-list t)
;;; :TEST-ME (mon-get-ebay-jpgs-list)

;;; ==============================
;;; :MODIFICATIONS <Timestamp: #{2009-09-05T15:51:57-04:00Z}#{09366} - by MON>
;;; :CREATED <Timestamp: Wednesday April 29, 2009 @ 03:14.41 PM - by MON KEY>
(defun mon-insert-ebay-jpgs-in-file ()
  "Called from an ebay-template file insert the items' jpg path from relative dir.\n
Return error message if items jpg path doesn't exist.\n
:SEE-ALSO `mon-insert-ebay-bmps-in-file', `mon-get-ebay-jpgs-list'.\n►►►"
  (interactive)
  (let ((find-jpgs (nreverse (mon-get-ebay-jpgs-list)))
	(jpg-dir (concat *mon-ebay-images-jpg-path* "/"
			 ;;WAS: (expand-file-name "../../")  "BIG-cropped-jpg/" 
			 (file-name-nondirectory (directory-file-name default-directory)) "/"))
	gather-jpgs)
    (while find-jpgs
      (princ (format "\n%s%s" jpg-dir (car find-jpgs))(current-buffer))
      (setq find-jpgs (cdr find-jpgs)))))
;;    
;;; :TEST-ME (mon-insert-ebay-jpgs-in-file)  

;;; ==============================
(defun mon-get-ebay-jpgs-count ()
  "Return count of jpgs associated with ebay templates' buffer.\n
:SEE-ALSO `mon-insert-ebay-bmps-in-file', `mon-get-ebay-jpgs-list',
`mon-insert-ebay-jpgs-in-file', `mon-get-ebay-bmps-count',
`mon-get-ebay-bmps-in-dir'.\n►►►"
  (interactive)
  (let ((jpg-count  (length (mon-get-ebay-jpgs-list t))))
    (message "%d" jpg-count)))
;;
;;; :TEST-ME (mon-get-ebay-jpgs-count)

;;; ==============================
(defun mon-get-ebay-bmps-count ()
  "Return count of bmps associated with ebay templates' buffer.\n
 :SEE-ALSO `mon-insert-ebay-bmps-in-file', `mon-get-ebay-jpgs-list',
`mon-insert-ebay-jpgs-in-file', `mon-get-ebay-jpgs-count',
`mon-get-ebay-bmps-in-dir'.\n►►►"
  (interactive)
  (let ((bmp-count  (length (mon-get-ebay-bmps-in-dir))))
    (message "%d" bmp-count)))
;;
;;; :TEST-ME (mon-get-ebay-bmp-count)

;;; ==============================
(defun mon-get-ebay-img-count-verify ()
  "Return message, and t or nil for image counts (bmp|jpg) of ebay buffers' dir.\n
:SEE-ALSO `mon-insert-ebay-bmps-in-file', `mon-get-ebay-jpgs-list',
`mon-insert-ebay-jpgs-in-file', `mon-get-ebay-bmps-count',
`mon-get-ebay-jpgs-count'.\n►►►"
  (interactive)
  (if (equal (mon-get-ebay-bmps-count) (mon-get-ebay-jpgs-count))
      (prog1
	  (message "Counts match. There are %s .bmps, and %s .jpg files."  
		   (mon-get-ebay-bmps-count) (mon-get-ebay-jpgs-count))
	t)
    (prog1
	(message "Counts _DO NOT_ match. There are %s .bmps, and %s .jpg files."  
		 (mon-get-ebay-bmps-count) (mon-get-ebay-jpgs-count))
      nil)))

;;; ==============================
;;; :MODIFICATIONS <Timestamp: #{2010-02-13T20:00:57-05:00Z}#{10067} - by MON KEY>
;;; The semantics of `mon-downcase-region-regexp' changed. 
;;; Now uses `mon-downcase-regexp'
;;; :CREATED <Timestamp: Thursday April 30, 2009 @ 05:38.49 PM - by MON KEY>
(defun mon-cln-img-magic-hex ()
  "Clean the formatting from output of ImageMagick color analysis script.\n
Return only hex color values.\n
:SEE-ALSO `mon-get-ebay-img-name-to-col', `mon-get-ebay-img-css',
`mon-get-ebay-css-pp-region-to-file', `mon-get-ebay-bmps-count',
`mon-get-ebay-bmps-in-dir', `mon-insert-css-colors', `mon-string-from-hex-list',
`mon-string-to-hex-list', `mon-string-to-hex-string',
`mon-string-to-hex-list-cln-chars',
`mon-hexcolor-add-to-font-lock',`hexl-hex-string-to-integer',
`url-hexify-string', `url-unhex-string', `url-unhex', `*css-color:hex-chars*',
`*regexp-rgb-hex*', `*regexp-css-color-hex*', `*regexp-hexcolor-keywords*',
`*regexp-hexcolor-keywords*'.\n►►►"
  (interactive)
  (let ((count 4))
    (search-forward-regexp "^\\(# Image\\)") ;"\\(^# Image\\)")
    (let* ((start-m (match-beginning 1))
	   (hld-mark (make-marker))
	   (rep-mark (set-marker hld-mark start-m))
	   (get-mark (marker-position rep-mark)))
      (while (> count 0)
	(goto-char get-mark)
        (with-no-warnings       
          (cond ((eq count 4) 
                 (replace-regexp  "# ImageMagick pixel enumeration: 1.,1,255,rgb" ""))
                ((eq count 3) 
                 (replace-regexp 
                  "^[0-9]\\{1,2\\},[0-9]\\{1,2\\}:[[:space:]].*\)[[:space:]]\\{2\\}" ""))
                ((eq count 2) (replace-regexp 
                               "\\([[:space:]]\\{2\\}\\(rgb\\|grey\\|white\\).*$\\)" ""))
                ((eq count 1) ( ;; :WAS `mon-downcase-region-regexp')
                               mon-downcase-regexp "^\\(#[A-Z0-9]\\{6,6\\}$\\)"))))
        (setq count (- count 1))))))
	;; (cond ((eq count 4) 
        ;;        (while (search-forward-regexp 
        ;;                "# ImageMagick pixel enumeration: 1.,1,255,rgb" nil t)
        ;;          (replace-match "")))
	;;       ((eq count 3) 
        ;;        (while (search-forward-regexp 
        ;;                "^[0-9]\\{1,2\\},[0-9]\\{1,2\\}:[[:space:]].*\)[[:space:]]\\{2\\}" nil t)
        ;;          (replace-match "")))
        ;;       ((eq count 2) 
        ;;        (while (search-forward-regexp
        ;;                "\\([[:space:]]\\{2\\}\\(rgb\\|grey\\|white\\).*$\\)" nil t)
        ;;          (replace-match "")))
	;;       ((eq count 1) (mon-downcase-regexp "^\\(#[A-Z0-9]\\{6,6\\}$\\)")))
	;; (setq count (- count 1))))))
      

;;; ================================================================
;;; :NOTE the `mon-get-ebay-img-css' function calls image-magick's `convert'
;;; command using the alias `imconvert'. 
;;; This is for W32 compatibility as convert.exe is a system level command
;;; gnu-linux systems should substitute `convert' for `imconvert'.
;;; This is needed because on a W32 system:
;;; (executable-find "convert") => "c:/WINDOWS/system32/convert.exe"
;;; (executable-find "imconvert") 
;;;  => "c:/Program Files/imagemagick-6.5.1-q8/imconvert.exe"
;;; :CREATED <Timestamp: Friday May 01, 2009 @ 04:24.15 PM - by MON KEY>
(defun mon-get-ebay-img-css ()          ;(img-list)
  "Return CSS hex colors of .bmps in current directory.\n
:SEE-ALSO `mon-get-ebay-img-name-to-col', `mon-get-ebay-css-pp-region-to-file',
`mon-get-ebay-bmps-count', `mon-get-ebay-bmps-in-dir', `mon-insert-css-colors',
`mon-cln-img-magic-hex'.\n►►►"
  (let ((buf-string) 
	(imgs (mon-get-ebay-bmps-in-dir)))
    (setq buf-string '())
    (while imgs
      (let* (in-buf
	     w-file
	     (img-bmp (car imgs))
	     (img-png (replace-regexp-in-string "\.bmp" "\.png" img-bmp t t)))
	(message (format "%s %s" img-bmp img-png))
	(with-temp-buffer ;;(switch-to-buffer "*css-img*")
	  (call-process-shell-command 
           (concat 
            ;; (if 
            "\"imconvert\" \"./" img-bmp
            "\" +dither -resize 100 -modulate 105,120 -posterize 6 " 
            "\"./" img-png "\"") nil t t)
          (call-process-shell-command 
           (concat 
            "\"imconvert\" "
            "\"" img-png "\""
            " \"" img-png "\""
            " -colors 16 +dither +matte -unique-colors txt:-") nil t t)
          (mon-g2be -1)
	(mon-cln-img-magic-hex)
	(kill-line)
	(setq in-buf (split-string (buffer-substring (point-min) (point-max))))
	(kill-buffer))
        (setq buf-string (cons `(,img-bmp ,in-buf) buf-string))
        (setq imgs (cdr imgs))))
    buf-string))

;;; ==============================
(defun mon-get-ebay-img-name-to-col ()
  "Inserts img file name from buffers' directory in buffer.\n
Helper function for `mon-get-ebay-css-pp' don't evaluate elsewhere.
:SEE-ALSO `mon-get-ebay-img-name-to-col', `mon-get-ebay-img-css',
`mon-get-ebay-css-pp-region-to-file', `mon-get-ebay-bmps-count',
`mon-get-ebay-bmps-in-dir', `mon-insert-css-colors',
`mon-cln-img-magic-hex'.\n►►►"
  (let ((put-cols (mon-get-ebay-bmps-in-dir))
	(img-cnt (string-to-number (mon-get-ebay-bmps-count)))
	img-strt)
    (setq img-strt 0)
    (while put-cols
      (let* ((img-nm-rp (car put-cols))
	     img-nm
	     (img-col (* img-strt 8))
	     (img-to-col (move-to-column img-col )))
	(if img-nm-rp
	    (setq img-nm img-nm-rp)
	  (setq img-nm ""))
	(setq img-nm (replace-regexp-in-string "\\.bmp" "" img-nm))
	img-to-col
	(insert (format "%s " img-nm))
	(setq put-cols (cdr put-cols))
	(setq img-strt (1+ img-strt)))))) 
	
;;; ==============================
;;; :CREATED <Timestamp: Tuesday May 05, 2009 @ 06:17.51 PM - by MON KEY>
(defun mon-get-ebay-css-pp-region-to-file ()
  "Helper function for `mon-get-ebay-css-pp' don't evaluate elsewhere.\n
:SEE-ALSO `mon-get-ebay-img-name-to-col', `mon-get-ebay-img-css', 
`mon-get-ebay-css-pp-region-to-file', `mon-get-ebay-bmps-count', 
`mon-get-ebay-bmps-in-dir', `mon-insert-css-colors'.\n►►►"
  (let (start-pnt end-pnt)
    ;; :WAS (previous-line) 
    (line-move -1 t) ;; (next-line -1) ;; (line-move-visual -1 t)
     (beginning-of-line)
     (setq start-pnt (point))
     (forward-line 16)
     (goto-char (point-at-eol))
     (setq end-pnt (point))
    (let ((css-start start-pnt)
	  (css-end end-pnt)
	  (atload ";;; -*- mode: html;  mode: CSS-COLOR-MODE; -*-\n")
	  (splt "/************************************************************/\n")
	  (css-stamp (format "/* %s */\n" (mon-stamp)))
          css-name
          in-buffer)
      (setq css-name 
	    (concat 
             (file-name-nondirectory (directory-file-name default-directory)) 
             "-hex-colors"))
      (setq in-buffer (buffer-substring css-start css-end))
      (with-temp-buffer css-name 
                        (dolist (nsert '(atload splt css-stamp splt in-buffer)
                                       (write-file css-name))
                          (insert nsert)))
      (delete-region css-start css-end)
    (find-file-other-window css-name))))

;;; ==============================
;;; :CREATED <Timestamp: Tuesday May 05, 2009 @ 10:37.17 AM - by MON KEY>
(defun mon-get-ebay-css-pp ()
  "Return columnized CSS extracted from bmps in new buffer.\n
0       8       16      24      32      40      48    
#001a33 #001a33 #001a33 #001a33 #001a33 #001a33 #001a33\n
:SEE-ALSO `mon-get-ebay-img-name-to-col', `mon-get-ebay-img-css', 
`mon-get-ebay-css-pp-region-to-file', `mon-get-ebay-bmps-count', 
`mon-get-ebay-bmps-in-dir', `mon-insert-css-colors'.\n►►►"
  (interactive)
  (let  ((assoc-bmp (mon-get-ebay-bmps-in-dir))
	 (css-col (mon-get-ebay-img-css))
	 (num-cols (mon-get-ebay-bmps-count))
	 col-cnt 
         pnt-mrkr)
    (if (not (buffer-modified-p))
         ;; Is this correct why aren't we at point already?
	(goto-char (point))
      (newline) (beginning-of-line)
      (setq pnt-mrkr (point-marker)))
    (setq col-cnt 0)
    (while assoc-bmp
      (let* ((walk-assoc (car assoc-bmp))
	     (css-vals (cadr (assoc walk-assoc css-col)))
	     put-cnt put-step put-size col-set)
	(setq col-set t) 
	(setq put-cnt 1)
	(setq put-size (length css-vals))
	(setq put-step (length css-vals)) ;presently not using 
	(while css-vals
	  (let* ((mtc (* col-cnt 8))
		 (mv-to-col (move-to-column mtc t))
		 (get-mark (marker-position pnt-mrkr))
		 (put-css  (car css-vals))
		 (this-css (cond ((= col-cnt 0)
				  (insert (format "%s \n" put-css)))
				 ((> col-cnt 0)
				  (insert (format "%s " put-css))))))
	    (progn 
	      (goto-char get-mark) 
	      (cond ((and (> col-cnt 0) (= put-cnt 1) (= put-size put-cnt))
		     (forward-line 0))
		    ((and (> col-cnt 0) (/= put-size put-cnt))
		     (forward-line put-cnt)))
	      mv-to-col
	      this-css)
	    (setq css-vals (cdr css-vals))
	    (setq put-cnt (1+ put-cnt))
	    (setq put-step (1- put-step))))
	(setq col-cnt (1+ col-cnt))
	(setq assoc-bmp (cdr assoc-bmp))))
    (progn
      (goto-char (marker-position pnt-mrkr))
      ;; :WAS (previous-line) 
      (line-move -1 t) ;; (next-line -1) ;; (line-move-visual -1 t)     
      (beginning-of-line)
      (mon-get-ebay-img-name-to-col)
      (goto-char (marker-position pnt-mrkr))
      (mon-get-ebay-css-pp-region-to-file))))

;;; ==============================
;;; :CREATED <Timestamp: Thursday April 30, 2009 @ 07:39.25 PM - by MON KEY>
(defun mon-insert-css-colors (frob-css-colors)
  "Insert css hex colors bound to arg FROB-CSS-COLORS.\n
:EXAMPLE\n\n\(let ((some-css '(\"#222220\" \"#663122\" \"#666631\"
                           \"#576061\" \"#996631\" \"#ac5341\")))
      \(mon-insert-css-colors some-css)\)\n
;=> #222220 #663122 #666631 #576061 #996631 #ac5341\n
:SEE-ALSO `mon-get-ebay-css-pp'.\n►►►"
  (interactive "X symbol holding list of CSS hex colors :")
  (let (my-css-insert frob-css-colors)
    ;;(setq my-css-insert css)
    (while my-css-insert
      (princ (concat " " (car my-css-insert)) (current-buffer))
      (setq my-css-insert (cdr my-css-insert)))))

;;; ==============================
;;; :COURTESY: Xah Lee :SEE (URL `http://xahlee.org/emacs/elisp_image_tag.html')
;;; :MODIFICATIONS <Timestamp: #{2010-03-30T20:02:13-04:00Z}#{10133} - by MON KEY>
;;; :CREATED: <Timestamp: Saturday April 25, 2009 @ 05:35.54 PM - by MON KEY>
(defun mon-get-image-dimensions (img-file-path)
  "Return image file's width and height as a list using function `create-image'.\n
:NOTE `mon-get-image-dimensions-im' for ImageMagick version which does similarly.\n
:SEE-ALSO `mon-get-image-md5'.\n►►►"
  (let (tmp dimen)
    (if (equal (file-name-extension img-file-path) "bmp")
        (mon-get-image-dimensions-im img-file-path)
        (progn
          (clear-image-cache)
          (setq tmp (create-image img-file-path))
          ;; :WAS (create-image (concat default-directory img-file-relative-path)))
          (setq dimen (image-size tmp t))
          `(,(car dimen) ,(cdr dimen))))))
;;
(defun mon-get-image-dimensions-im (img-file-path)
  "Return a image file's width and height as a list.\n
Function requires ImageMagick's \"identity\" shell command.
:NOTE `mon-get-image-dimensions' which returns same but uses `create-image'.\n
:SEE-ALSO `mon-get-image-md5'.\n►►►"
  (let* ((cmd-name (executable-find "identify"))
         (sh-output 
          (if cmd-name
              (shell-command-to-string (concat cmd-name " " img-file-path))
              (error (concat ":FUNCTION `mon-get-image-dimensions-im'"
                             " - Executable \"identify\" not in path"))))
         width 
         height)
    ;; sample output from “identify”:
    ;; xyz.png PNG 520x429+0+0 DirectClass 8-bit 9.1k 0.0u 0:01
    (string-match "^[^ ]+ [^ ]+ \\([0-9]+\\)x\\([0-9]+\\)" sh-output)
    (setq width  (match-string 1 sh-output))
    (setq height (match-string 2 sh-output))
    `(,(string-to-number width) ,(string-to-number height))))
;;
;;; ==============================
(defun mon-get-image-md5 (img-file-path)
"Return md5 checksum of image at IMG-FILE-PATH.\n
:SEE-ALSO `md5', `sha1', `mon-get-image-dimensions-im'.\n►►►"
  (let (cmd-name sh-output sum)
    (setq cmd-name "md5sum")
    (setq sh-output (shell-command-to-string (concat cmd-name " " img-file-path)))
    (string-match ;md5 sum regex
     "\\([A-z0-9]\\{32,32\\}\\)"  sh-output)
    (setq sum (match-string 1 sh-output))
    (list sum)))

;;; ==============================
;;; NOTES:
;;; Rename images in dir.
;;; *i) test if current buffers directory contains images -> (file-expand-wildcards (format "*.%s" img-typ) t)
;;; *i)get the filenames - put them in a var
;;; *ii) are we in a good directory for renaming images -> mon-ebay-image-directory-ok-p if  i & ii t 
;;; *iii) if not get a good directory for renaming images -> mon-ebay-image-directory-ok-p
;;; *iv) test if that directory contains images -> (file-expand-wildcards (format "*.%s" img-typ) t)
;;; *vi) get the filenames - put them in a var
;;; vii) strip the long pathnames
;;; ) query if the sort is correct
;;; ) if so
;;; -> query for rename prefix
;;; --> be smart about it and take the last dir name of split-path
;;; -> query for rename-suffix
;;; ) if not 
;;; -> rotate until correct
;;; 'rotations' are made by pivoting on the short name assoc list e.g. {1 2 3 4} -> {4 1 2 3} "
;;;; ==============================

(declare-function mon-mapcar "mon-seq-utils"  (mapcar-fun mapcar-lst &rest more-lsts))

;;; ==============================
;;; :FIXME This case isn't working:
;;; (mon-rename-imgs-in-dir ".jpg" (expand-file-name  "e1144" *mon-ebay-images-bmp-path*))
;;; Called from buffers default-directory: (expand-file-name  "e1144" *mon-ebay-images-bmp-path*)
;;; We should be changing default-directory when ALT-PATH passed in but the 
;;; `mon-ebay-image-directory-ok-p' isn't playing nice.
;;; :PARTIALLY-WORKING-AS-OF
;;; :CREATED <Timestamp: Thursday June 04, 2009 @ 08:01.23 PM - by MON KEY>
(defun mon-rename-imgs-in-dir (image-type &optional alt-path) 
  "IMAGE-TYPE is a string of type `.bmp' `.nef' `.jpg'.\n
When non-nil ALT-PATH specifies a directory conatining images of IMAGE-TYPE.
Else, path defaults to:\n
 a) current buffer's dirs if it has images-of type;\n
 b) if not, get the IMAGE-TYPE alist `mon-ebay-image-directory-ok-p' 
    and use `completing-read' to prompt for a dir beneath;\n
build a list of filenames of IMAGE-TYPE in resulting path.
:SEE-ALSO .\n►►►"
  (interactive "sRename images of type: ")
  (let (starting 
        rnm-prompt
        (alt-p alt-path))
    (setq starting (mon-get-buffer-parent-dir t))
    (unwind-protect
	(let (passed)
	  (while (not passed)
	    (setq default-directory (cond (alt-p alt-p) (t starting)))
	    (let* ((img-typ (mon-check-image-type image-type))
		   ;; what we really want here is: (mon-ebay-image-directory-ok-p img-type alt-path)
		   (this-dir-maybe (mon-ebay-image-directory-ok-p img-typ))
		   (maybe-pth (cadr this-dir-maybe))
		   (maybe-typ (car this-dir-maybe))
		   (maybe-fls (directory-files maybe-pth t maybe-typ))
		   (maybe-len (length maybe-fls))
		   (new-p maybe-pth))
              (setq default-directory new-p)
              (cond ((and (string= (directory-file-name default-directory) maybe-pth) (not (zerop maybe-len)))
                     (progn
                       (setq rnm-prompt `(,maybe-len ,maybe-typ ,maybe-pth ,maybe-fls))
                       (setq passed t)))
                    ((and (string= (directory-file-name default-directory) maybe-pth) (zerop maybe-len))
                     (setq alt-p maybe-pth))))))
      (setq default-directory  starting))
    (let* ((long-names 
            (sort (cadddr rnm-prompt) 
                  '(lambda (x y) (not (file-newer-than-file-p x y)))))
	   (img-seq (number-sequence 1 (car rnm-prompt)))
	   (img-pth (caddr rnm-prompt)) 
	   (img-pth-len (length (file-name-as-directory img-pth)))
           ;; :WAS (img-assoc-l (mapcar* 'cons img-seq long-names))
	   (img-assoc-l (mon-mapcar #'cons img-seq long-names)) 
	   img-assoc-s)
      (setq img-assoc-s
            ;; :WAS (mapcar* #'(lambda (x) 
            (mon-mapcar #'(lambda (x) 
                           (let* ((lng-pth-key (car x))
                                  (lng-pth (cdr x))
                                  (lng-pth-len (length lng-pth)))
                             (cons lng-pth-key  (substring lng-pth img-pth-len lng-pth-len))))
                       img-assoc-l))
      `(,img-assoc-s ,img-assoc-l))))
;;
;;; :TEST-ME (mon-rename-imgs-in-dir ".bmp" (expand-file-name  "e1144" *mon-ebay-images-bmp-path*))

;; mon-check-image-type
;;; ==============================
;;; :CREATED <Timestamp: Saturday May 23, 2009 @ 01:34.43 PM - by MON KEY>
(defun mon-check-image-type (image-type)
  "Check IMAGE-TYPE matches the required format.
When IMAGE-TYPE (a string) is not one of: \".bmp\", \".jpg\", or \".nef\" prompt
for completion with require-match on choice defaults to \".bmp\".
Helper function for `ebay-template-mode's image related functions.
:SEE-ALSO `mon-image-verify-type',`image-type-available-p',
`image-type-from-file-name',\n `image-file-name-extensions',
`image-type-file-name-regexps', `image-file-name-regexps'.\n►►►"
  (let* ((img-typ '(".nef" ".jpg" ".bmp"))
         (prompt 
          (format "%s not a valid image-type. Select a string of type %s (TAB completes): " 
                  image-type img-typ)))
    (if (not (member image-type img-typ))
	(completing-read prompt img-typ nil t nil t ".bmp")
      image-type)))
;;
;;; :TEST-ME (mon-check-image-type ".mmm")
;;; :TEST-ME (mon-check-image-type ".bmp")

;;; ==============================
;;; :CREATED <Timestamp: Friday May 29, 2009 @ 07:54.24 PM - by MON KEY>
(defun mon-ebay-image-directory-not-ok (image-type in-directory from-dir) 
  "Helper function for `mon-ebay-image-directory-ok-p'.\n
:SEE-ALSO .\n►►►"
  (let* ((img-type (mon-check-image-type image-type))
	 (img-alist *mon-ebay-images-lookup-path*)
	 (dir-type (nth 1 (assoc img-type img-alist)))
	 (collect-from (eval dir-type))
	 (this-dir (split-string in-directory "/"))
	 (head-match (eval dir-type))
	 (match-with (mapcar 'caddr  *mon-ebay-images-lookup-path*))
	 caught
         make-ok)
    (while (and match-with (not caught))
      (let ((looking (car match-with)))
	(when (member looking this-dir)
	  (setq caught (member looking this-dir)))
	(setq match-with (cdr match-with))))
    (let ((maybe-collect  (mon-dir-build-list collect-from t)))
      (when (member (cadr caught) maybe-collect)
	(setq caught (concat head-match "/" (cadr caught))))
      (setq make-ok
	    ;;(read-directory-name (format "Find a better directry  for this %s image type : " img-type)
	    (completing-read 
	     (format "..%s not a good %s directory - Get new path: " from-dir img-type)
	     (mon-dir-build-list collect-from) ;collection
	     nil				;predicate 
	     t					;require-match
	     (if caught caught collect-from)	;initial-input 
	     nil			        ;hist 
	     (when caught caught))))	        ;def 
    make-ok))

;;; ==============================
;;; :CREATED <Timestamp: Friday May 29, 2009 @ 07:54.24 PM - by MON KEY>
;;; TODO: This procedure really needs to take an ALT-PATH arg and/or play nice
;;; with `mon-rename-imgs-in-dir' which can cond reset the default-directory.
;;; ==============================
(defun mon-ebay-image-directory-ok-p (image-type)
   "Test if we are in the correct directory for operation on IMAGE-TYPE.
IMAGE-TYPE is a string, one of `.bmp' `.nef' `.jpg'.
When directory doesn't match a valid path name for IMAGE-TYPE prompts 
for a better completion with `mon-ebay-image-directory-not-ok'.
Return a list of three elements:\n\car: image-type;\n
\cadr: path containing the image type;\n
\caddr: calling buffer's filename (if any, else nil)\n
:EXAMPLE\n\(mon-ebay-image-directory-ok-p \".bmp\")\n
:SEE-ALSO .\n►►►"
  (let* (;;(get-back-to-def (default-directory))
	 (img-type (mon-check-image-type image-type))
	 (img-alist *mon-ebay-images-lookup-path*)
	 (dir-type (nth 1 (assoc img-type img-alist)))
	 (dir-matcher (nth 2 (assoc img-type img-alist))) ;(nth 2 (assoc ".jpg" *mon-ebay-images-lookup-path*))
	 (split-buff (mon-string-split-buffer-name))
	 buff-ok
         alt-ok
         this-file
         ret-swp
         ret
         rel-pth)
    (let* ((tested-ok)
	   (test-buf (car (member dir-matcher split-buff)))
	   nope)
      (cond (test-buf (setq tested-ok test-buf))
	    ((not test-buf)
	     (while (not nope)
	       (let ((test-new 
		      (mon-ebay-image-directory-not-ok 
		       img-type                         ;; image-type
		       (mon-get-buffer-parent-dir t)    ;; in-dir
                       (mon-truncate-path-for-prompt))) ;; Called from dir
                     looked-at)
		 (when test-new
		   (setq looked-at (split-string test-new "/"))
		   (when (car (member dir-matcher looked-at))
		     (setq tested-ok (car (member dir-matcher looked-at)))
		     (setq alt-ok looked-at)
		     ;;`file-realtive-name' defaults to `default-directory' 
		     ;; are there situations in this function where this is not the desired behavior?
		     (setq rel-pth (file-relative-name test-new *mon-nef-scan-path*)))))
	       (when  tested-ok (setq nope t)))))
      (when tested-ok (setq buff-ok tested-ok)))
    (setq this-file (cond (alt-ok nil)
			  ((mon-buffer-written-p) (file-name-nondirectory (buffer-file-name)))))
    (setq ret ())
    (when buff-ok 
      (cond (alt-ok (setq ret-swp (nreverse alt-ok))) ; `nrevrse' conditional on alt-ok 
	    ((not alt-ok) (setq ret-swp (nreverse split-buff))))
      (if (member this-file ret-swp)
	  (progn
	    (setq ret (cons this-file ret))
	    (setq ret-swp (cdr ret-swp)))
	(setq ret (cons '() ret)))
      (when (not rel-pth)
	(setq rel-pth 
	      (file-relative-name (directory-file-name (expand-file-name "./")) *mon-nef-scan-path*)))
      (setq ret (cons (expand-file-name rel-pth *mon-nef-scan-path*)  ret))
      (setq ret (cons image-type  ret)))
    ret))

;;; ==============================
;;; :WORKING-AS-OF
;;; :CREATED <Timestamp: Monday June 15, 2009 @ 08:21.27 PM - by MON KEY>
(defun mon-image-rename-propertize (&optional from-point)
  "Add text-properties to image-rename form.
Characters to the left of '[' carry read-only,
and intangible properites. Additionally specific fields get these properties:
:divider, :img-count, 
:orginal-image-number, :orginal-image-name, :new-count-delim
:image-rename-prefix-delim, :image-rename-prefix
:image-rename-suffix-delim, :image-rename-suffix, 
:image-rename-start-num-delim, :image-rename-start-num\n\n
:EXAMPLE
------------------------------\nRenaming images in directory: 
> ../some/path/to/somewhere\n------------------------------
Renaming images of type:  .bmp\nNumber of images to rename: 00
------------------------------\nimage-rename-prefix: [ ]
image-rename-suffix: [ ]\nimage-rename-start#: [ ]
------------------------------\n07) this-is-a-file-namexxxxxxxxx ▪ [ ]
2) this-is-a-file-namexxxxxxxx ▪▪▪ [ ]\n04) this-is-a-file-namexxxxxxx ▪▪▪ [ ]
9) this-is-a-file-namexxxxxx ▪▪▪▪▪ [ ]\n01) this-is-a-file-namexxxxx ▪▪▪▪▪ [ ]
8) this-is-a-file-namexxxx ▪▪▪▪▪▪▪ [ ]\n10) this-is-a-file-namexxx ▪▪▪▪▪▪▪ [ ]
03) this-is-a-file-namexx ▪▪▪▪▪▪▪▪ [ ]\n3) this-is-a-file-namexx ▪▪▪▪▪▪▪▪▪ [ ]
06) this-is-a-file-name ▪▪▪▪▪▪▪▪▪▪ [ ]\n5) this-is-a-file-namex ▪▪▪▪▪▪▪▪▪▪ [ ]\n\n
:SEE-ALSO `mon-rename-imgs-in-dir', `mon-parse-rename-images',
`mon-shorten-rename-image-path', `mon-parse-rename-lengths',
`mon-pad-rename-lengths', `mon-build-rename-buffer'.\n►►►"
  (unwind-protect
       (mon-toggle-read-only-point-motion)
    (save-excursion
      (let ((regex-img 
             ;;"^\\(\\([0-9]\\{1,2\\}\\)\\()[[:space:]]\\)\\(.*\\)\\([[:space:]]_+[[:space:]]\\)\\(\\[\\)\\(.*\\)\\(\\]\\)\\)"
             ;;^^^^^1^^2^^^^^^^^^^^^^^^^^^^3^^^^^^^^^^^^^^^^^4^^^^^^^5^^^^^^^^^^^^^^^^^^^^^^^^^^^^^6^^^^^^^^7^^^^^^^8^^^^^^^^
             `((,(concat "^\\("                 ;; <- grp-1 - full match
                         "\\([0-9]\\{1,2\\}\\)" ;; <- grp-2 - original image number
                         "\\()[[:space:]]\\)" ;; <- grp-3 - presentation of no signifigance
                         "\\(.*\\)"	      ;; <- grp-4 - original image name
                         "\\([[:space:]]▪+[[:space:]]\\)" ;; <- grp-5 - presentation underscore e.g. " ▪▪▪▪▪▪▪▪▪ "
                         "\\(\\[\\)" ;; <- grp-6 - new image number delimiter
                         "\\(.*\\)"  ;; <- grp-7 - image-rename-new-image
                         "\\(\\]\\)" ;; <- grp-8 - closing delim
                         "\\)")	     ; :image-name-regex-matches
                (8 8 read-only t rear-nonsticky t :rename-image-original-cls-delim t)
                (7 7 :image-rename-new-image-key t)
                (6 6 read-only t intangible t rear-nonsticky t :rename-image-original-opn-delim t)
                (5 5 read-only t intangible t rear-nonsticky t :rename-image-original-padding t)
                (4 4 read-only t intangible t rear-nonsticky t :rename-image-original-name t) ;NAME (buffer-substring)
                (3 3 read-only t intangible t rear-nonsticky t) ;; presentation whitespace
                (2 2 read-only t intangible t rear-nonsticky t :rename-image-original-key t) ;COUNT (buffer-substring)
                ;;(1 1 read-only t intangible t rear-nonsticky t) ;; full-match - basecase
                )
               ("^\\(Renaming images in directory:\\)"
                (1 1 read-only t intangible t :rename-image-in-directory t))
               ("^\\(\\(> \\)\\(\.\./.*\\)\\)"
               ;;^^^^1^^2^^^^^^^3^^^^^^^^^^^^
                (3 3 :image-rename-directory t)
                (2 2 :rename-image-directory-opn-delim t)
                (1 1 read-only t intangible t))
               ;; Disregarding four letter extensions like .tiff 
               ("^\\(\\(Renaming images of type:  \\)\\(\.[a-z]\\{3,3\\}\\)\\)" 
               ;;^^^^1^^2^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^3^^^^^^^^^^^^^^^^^^^^^    
                (3 3 :image-rename-type t)
                (2 2 :rename-image-of-type t)
                (1 1 read-only t intangible t))
               ("^\\(-\\{25,45\\}\\)"  
                (1 1 read-only t intangible t :rename-image-divider t))
               ("^\\(\\(Number of images to rename: \\)\\([0-9]\\{1,2\\}\\)\\)" 
               ;;^^^1^^2^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^3^^^^^^^^^^^^^^^^^^^
                (3 3 :image-rename-count t)
                (2 2 :rename-image-count t)
                (1 1 read-only t intangible t))
               ("\\(\\(image-rename-prefix:[[:space:]]\\)\\(\\[\\)\\(.*\\)\\(\\]\\)\\)"
               ;;^^^1^^2^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^3^^^^^^^4^^^^^^^5^^^^^^^^^
                (5 5 read-only t rear-nonsticky t :rename-image-prefix-cls-delim t)
                (4 4 :image-rename-prefix t)
                ;;(4 4 read-only nil intangible nil :image-rename-prefix t)
                (3 3 read-only t intangible t rear-nonsticky t :rename-image-prefix-opn-delim t)
                (2 2 read-only t intangible t rear-nonsticky t :rename-image-with-prefix t))
               ;;(1 1 read-only t intangible t rear-nonsticky t))
               ("\\(\\(image-rename-suffix:[[:space:]]\\)\\(\\[\\)\\(.*\\)\\(\\]\\)\\)"
               ;;^^1^^^2^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^3^^^^^^^4^^^^^^^^5^^^^^^^^
                (5 5 read-only t rear-nonsticky t :rename-image-suffix-cls-delim t)
                (4 4  :image-rename-suffix t)
                ;;(4 4 read-only nil intangible nil :image-rename-suffix t)
                (3 3 read-only t intangible t rear-nonsticky t :rename-image-suffix-opn-delim t)
                (2 2 read-only t intangible t rear-nonsticky t :rename-image-with-suffix t))
               ;;(1 1 read-only t intangible t rear-nonsticky t))
               ("\\(\\(image-rename-start#:[[:space:]]\\)\\(\\[\\)\\(.*\\)\\(\\]\\)\\)"
               ;;^^^1^^2^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^3^^^^^^^4^^^^^^^^5^^^^^^^^
                (5 5 read-only t rear-nonsticky t :rename-image-start-number-cls-delim t)
                (4 4 :image-rename-start-number t)
                (3 3 read-only t intangible t rear-nonsticky t :rename-image-start-number-opn-delim t)
                (2 2 read-only t intangible t rear-nonsticky t :rename-image-with-start-number t))
               ;;(1 1 read-only t intangible t rear-nonsticky t))
               ("\\(\\( \\[.*\\)\\(\\]\\)\\)" 
                (3 3 read-only t rear-nonsticky t :rename-image-cls-delim t)))) ;; generic closing brace
            (this-point (if from-point from-point (point)))
            (walk-regexps))
        (goto-char this-point)
        (setq walk-regexps regex-img)
        (while walk-regexps
          (let ((the-regex (caar walk-regexps))
                (the-matches))
            (setq the-matches (cdar walk-regexps))
            (while the-matches
              (let* ((the-match (car the-matches))
                     (mb (car the-match))
                     (me (cadr the-match))
                     (props (cddr the-match)))
                (while (search-forward-regexp the-regex nil t)
                  (add-text-properties (match-beginning mb) (match-end me) props))
                (setq the-matches (cdr the-matches))
                (goto-char this-point)))
            (setq walk-regexps (cdr walk-regexps)))))))
  (mon-toggle-read-only-point-motion))

;;; ==============================
;;; :NOTE The IMG-LIST arg acquired with: `mon-rename-imgs-in-dir'
;;; :CREATED <Timestamp: Thursday June 18, 2009 @ 04:09.20 PM - by MON KEY>
(defun mon-parse-rename-images (img-list)
  "Parse IMG-LIST into three elt list of image-type, parent directory, image-path.
IMG-LIST acquired with: `mon-rename-imgs-in-dir'.\n\n
:SEE-ALSO `mon-rename-imgs-in-dir',`mon-shorten-rename-image-path',
`mon-parse-rename-lengths',`mon-pad-rename-lengths',
`mon-image-rename-propertize',`mon-build-rename-buffer'.\n►►►"
  (let* ((prs--rnm img-list)
	 (shrt-pth-alist (car prs--rnm))
	 (lng-pth-alist (cadr prs--rnm))
	 (common-spec (cdr (assoc 1 lng-pth-alist)))
	 (file-ext (file-name-extension common-spec))
	 (files-dir (directory-file-name (file-name-directory common-spec)))
	 (file-dir (split-string files-dir "/" ))
	 (parent-dir (car (last file-dir))))
    `(,file-ext ,parent-dir ,files-dir)))

;;; ==============================
;;; :CREATED <Timestamp: Thursday June 18, 2009 @ 06:19.14 PM - by MON KEY>
(defun mon-shorten-rename-image-path (shorten-path)
  "Shorten a path for presentation in rename buffer.
:SEE-ALSO `mon-rename-imgs-in-dir',`mon-parse-rename-images',
,`mon-parse-rename-lengths',`mon-pad-rename-lengths',
`mon-image-rename-propertize',`mon-build-rename-buffer'.\n►►►"
  (let* ( ;;include `file-name-sans-extension' if we want to use this on files also
	 (shorten  (last (split-string (directory-file-name shorten-path) "/" ) 3 ))
	 (showpath (concat "../" (mapconcat 'identity shorten "/"))))
    showpath))

;;; ==============================
;;; :CREATED <Timestamp: Monday June 22, 2009 @ 04:36.16 PM - by MON KEY>
(defun mon-parse-rename-lengths (images)
  "Return a list of two alists. car is alist1, cdr is alist2.\n
Values of alist1 are string lengths of file names in second alist
Keys of alist1 are shared alist2, so keys of alist1 are indexes into alist2.
IMAGES should be a list of two alists generated with `mon-rename-imgs-in-dir'.\n\n
:SEE-ALSO `mon-rename-imgs-in-dir',`mon-parse-rename-images',
`mon-shorten-rename-image-path', `mon-pad-rename-lengths',
`mon-image-rename-propertize',`mon-build-rename-buffer'.\n►►►"
  (let (get-imgs 
	short-assoc 
	parse-short-keys 
	new-val)
    (setq get-imgs (copy-tree images));(copy-tree this-3))
    (setq short-assoc (car get-imgs))
    (setq parse-short-keys (mapcar 'car short-assoc)) ;=>(1 2 3 4 5 6 7 8)
    (setq new-val ())
    ;; :WAS (mapcar  #'(lambda (m) 
    (mapc  #'(lambda (m) 
                 (let* ((w (cdr (assoc m short-assoc))) ;; The short filename
                        (x (file-name-sans-extension w)) ;; Short filename without .ext
                        (y (length x)) ;; Length of filename without .ext
                        z)
	  (setq new-val (cons `(,m ,y) new-val))
	  (rplacd (assoc m short-assoc) x)))
     parse-short-keys)
    (setq get-imgs (car get-imgs)
	  get-imgs (cons (sort new-val #'(lambda (x y) (> (cadr x) (cadr y)))) get-imgs))))

;;; ==============================
;;; :CREATED <Timestamp: Monday June 22, 2009 @ 07:29.14 PM - by MON KEY>
(defun mon-pad-rename-lengths (pad-list)
  "PAD-LIST is an alist of variable width strings padded to match > length string.
Return a PAD-LIST as a multi valued list of padded strings.
car of returned list is the length of the longest _padded_ string in alist.
cdr is an alist of strings padded to match max-length of car.\n
:EXAMPLE
This is constant: \(length \"1)  ▪ [ ]\") ;=> 9\n
This is constant maybe: \(length \"------------------------------\") ;=> 30\n
This is variable: \(length \"image-xyzqqqqqqqqqqqq\") ;=> 21\n
So is this: \(length \"this-is-a-file-namexxxxxxxxxxxxx\") ;=> 32\n
---\nThis is a width when < all-image-names 30\n
------------------------------
1) image-xyzqqqqqqqqqqqq ▪ [ ]\n..3!..................24!...30!\n
---\nThis is a width when > one-or-more-image-names 30\n
-----------------------------------------
1) this-is-a-file-namexxxxxxxxxxxxx ▪ [ ]
..3!.............................35!...41!\n
---\nThese are the printed alist vals when < img-count 10 and > length 30:\n
7) this-is-a-file-namexxxxxxxxxxxxx ▪ [ ]
8) this-is-a-file-namexxxxxxxxxx ▪▪▪▪ [ ]
9) this-is-a-file-namexxxx ▪▪▪▪▪▪▪▪▪▪ [ ]\n
---\nThese are the printed alist vals when < img-count 10 and < length 30:\n
7) e1456 ▪▪▪▪▪▪▪▪▪▪▪▪▪▪▪▪▪ [ ]\n8) e1456 ▪▪▪▪▪▪▪▪▪▪▪▪▪▪▪▪▪ [ ]
9) e1455 ▪▪▪▪▪▪▪▪▪▪▪▪▪▪▪▪▪ [ ]\n
---\nThese are the printed alist vals when > img-count 10 and < length 30:\n
08) e1455 ▪▪▪▪▪▪▪▪▪▪▪▪▪▪▪▪ [ ]\n09) e1456 ▪▪▪▪▪▪▪▪▪▪▪▪▪▪▪▪ [ ]
10) e1457 ▪▪▪▪▪▪▪▪▪▪▪▪▪▪▪▪ [ ]\n
---\nThese are the printed alist vals when > img-count 10 and > lenghth 30:\n
08) this-is-a-file-namexxxxxxxxxxxxx ▪ [ ]
09) this-is-a-file-namexxxxxxxxx ▪▪▪▪▪ [ ]
10) this-is-a-file-namexxxxxxx ▪▪▪▪▪▪▪ [ ]\n\n
:SEE-ALSO `mon-rename-imgs-in-dir',`mon-parse-rename-images',
`mon-shorten-rename-image-path',`mon-parse-rename-lengths',
`mon-pad-rename-lengths',`mon-image-rename-propertize',
`mon-build-rename-buffer'.\n►►►"
  (let* ((cp-pad pad-list) ;;(cp-pad this-4)
         (to-pad (copy-tree cp-pad))
         (len-nms (car to-pad))
         (put-nms (cdr to-pad))
         (img-cnt (length len-nms))
         (lngst-nm (cadar len-nms))
         (comp-len (if (> img-cnt 9) 31 30))
         (comp-mod (if (> comp-len 30) 10 9))
         (new-len  (if (>= lngst-nm (- comp-len comp-mod))
                       (+ lngst-nm comp-mod)
                     comp-len)))
    ;; :WAS (mapcar #'(lambda (pd-x)
    (mapc #'(lambda (pd-x)
              (let* ((len-x (cadr pd-x))
                     (id-x (car pd-x))
                     (assoc-x (cdr (assoc id-x put-nms)))              
                     (img-num (if (> comp-len 30)
                                  (if (> id-x 9)
                                      (concat (number-to-string id-x) ") " )
                                    (concat "0" (number-to-string id-x) ") " ))
                                (concat (number-to-string id-x) ") " )))
                     (pad-str  
                      (concat img-num assoc-x " " 
                              (make-string  
                               (cond ((and (= new-len 30) (<= lngst-nm 21)(= comp-mod 9))
                                      (let* ((a (- new-len len-x))  (b (- a comp-mod))  (c (1+ b)))  c))
                                     ((and (= new-len 30) (= lngst-nm 21) (= comp-mod 9)) 1)
                                     ((and (= new-len 31) (<= lngst-nm 21)(= comp-mod 10))
                                      (let* ((a (- new-len len-x))  (b (- a comp-mod)))  b)) 
                                     ((and (= new-len 31) (= lngst-nm 21)(= comp-mod 10)) 1)
                                     ((and (>= new-len 31) (= len-x lngst-nm)) 1)
                                     ((and (>= new-len 31) (< len-x lngst-nm)) (1+ (- lngst-nm len-x)))
                                     ;; :NOTE We should not see this last one 
                                     (t (1+ (- lngst-nm len-x))))
                               9642) 
                              " [ ]"))) ;; :NOTE This can be extended as an &optional arg
                (setcdr (assoc id-x put-nms) pad-str)))
          len-nms)
    (setq put-nms (cons new-len put-nms))
    put-nms))

;;; ==============================
;;; :CREATED <Timestamp: Wednesday June 24, 2009 @ 05:54.11 PM - by MON KEY>
(defun mon-build-rename-buffer (image-type) ;&optional alt-path
  "Generate a rename buffer for marking images to name.\n
Return value generated with: 
 `mon-rename-imgs-in-dir', `mon-parse-rename-images',
 `mon-shorten-rename-image-path', `mon-parse-rename-lengths',
 `mon-pad-rename-lengths', `mon-image-rename-propertize'\n
:EXAMPLE\n
--------------------------------------
Renaming images in directory:\n> ../some/path/to/somewhere
--------------------------------------
Renaming images of type:  .bmp\nNumber of images to rename: NN
--------------------------------------
image-rename-prefix: [ ]\nimage-rename-suffix: [ ]\nimage-rename-start#: [ ]
--------------------------------------\n07) this-is-a-file-namexxxxxxxxx ▪ [ ]
04) this-is-a-file-namexxxxxxx ▪▪▪ [ ]\n01) this-is-a-file-namexxxxx ▪▪▪▪▪ [ ]
10) this-is-a-file-namexxx ▪▪▪▪▪▪▪ [ ]\n03) this-is-a-file-namexx ▪▪▪▪▪▪▪▪ [ ]
06) this-is-a-file-namex ▪▪▪▪▪▪▪▪▪ [ ]\n2) this-is-a-file-namexxxxxxxx ▪▪▪ [ ]
9) this-is-a-file-namexxxxxx ▪▪▪▪▪ [ ]\n8) this-is-a-file-namexxxxx ▪▪▪▪▪▪ [ ]
3) this-is-a-file-namexx ▪▪▪▪▪▪▪▪▪ [ ]\n5) this-is-a-file-namexx ▪▪▪▪▪▪▪▪▪ [ ]
--------------------------------------\n
:SEE-ALSO .\n►►►"
  (let* ((get-images (mon-rename-imgs-in-dir image-type)) ;alt-path?
	 (image-count (length (car get-images)))
	 (parse-imgs (mon-parse-rename-images get-images))
	 (shorten-dir (mon-shorten-rename-image-path (caddr parse-imgs)))
 	 (parsed-type (car parse-imgs)) ;is this needed if we're passing IMAGE-TYPE as the arg?
	 (prefix-guess (cadr parse-imgs))
         (img-lengths (mon-parse-rename-lengths get-images))
         (image-nm-frmt (mon-pad-rename-lengths img-lengths)) ;(mon-parse-rename-lengths this-3)=>
         (div-len (pop image-nm-frmt))
         ;; ==============================
         (img-rnm-in-dir "Renaming images in directory:")
         (img-rnm-dir (concat "> " shorten-dir))
         (img-rnm-typ (concat "Renaming images of type:  ." parsed-type))
         (img-rnm-prfx  (concat "image-rename-prefix: [" prefix-guess "]"))
         (img-rnm-sfx   "image-rename-suffix: [ ]")
         (img-rnm-strt-num (concat "image-rename-start#: [" " ]"))
         (img-rnm-div (make-string div-len 45))
         (img-rnm-cnt (concat "Number of images to rename: " 
			      (when (< image-count 9) "0") (number-to-string image-count))))
    (progn
      (get-buffer-create "*rename-images*") 
      (with-current-buffer "*rename-images*"
        (princ (format "%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s"
                       img-rnm-div  img-rnm-in-dir    img-rnm-dir  img-rnm-div
                       img-rnm-typ  img-rnm-cnt       img-rnm-div  img-rnm-prfx
                       img-rnm-sfx  img-rnm-strt-num  img-rnm-div)  (current-buffer))
        (mapc '(lambda (x) 
                 (let ((tail-nm (cdr x)))
                   (princ (format "\n%s" tail-nm) (current-buffer)))) 
              image-nm-frmt)
        (princ (format "\n%s" img-rnm-div) (current-buffer))
        (mon-image-rename-propertize (point-min))))
    (display-buffer "*rename-images*")))

;;; ==============================
;;; :TEST-CASES
;;;
;;; :NOTE
;;; Before running tests build some test-cases in following path:
;;; (concat *mon-ebay-images-bmp-path* "/e6666") if they don't already exist. 
;;; Evaluate the commented form below:
;; ,---- 
;; | (save-excursion
;; |   (make-directory (concat *mon-ebay-images-bmp-path* "/e6666"))
;; |   (cd (concat *mon-ebay-images-bmp-path* "/e6666"))
;; |   (let ((x 8))
;; |     (while (> x 0)
;; |       (with-temp-file 
;; |           (concat "this-is-a-file-name-" (make-string (1+ (random 7)) 120) ".bmp"))
;; |       (setq x (1- x)))))
;; `----

;;; ==============================
;;; :TESTING `mon-parse-rename-lengths' 
;;; Following symbols: 
;;; `this-3', `this-3-b', `this-3-c', `this-3-d' 
;;; Are comparable to return value of:
;;; (mon-rename-imgs-in-dir ".bmp" (expand-file-name  "e6666" *mon-ebay-images-bmp-path*))
;;
;; ,---- `this-3' -> 9 elts >= 21
;; | (setq this-3 
;; |  '(((1 . "this-is-a-file-namexxxxxxxxx.bmp") 
;; |     (2 . "this-is-a-file-namexxxxxxxxxxxx.bmp") (3 . "this-is-a-file-namexxxxxx.bmp") 
;; |     (4 . "this-is-a-file-namexxxxxxxxxxx.bmp") (5 . "this-is-a-file-namexxxxx.bmp") 
;; |     (6 . "this-is-a-file-namexxxx.bmp") (7 . "this-is-a-file-namexxxxxxxxxxxxx.bmp") 
;; |     (8 . "this-is-a-file-namexxxxxxxx.bmp") (9 . "this-is-a-file-namexxxxxxxxxx.bmp"))
;; |    ((1 . "u:/plv-1/plv2/plv3/e6666/this-is-a-file-namexxxxxxxxx.bmp") 
;; |     (2 . "u:/plv-1/plv2/plv3/e6666/this-is-a-file-namexxxxxxxxxxxx.bmp") 
;; |     (3 . "u:/plv-1/plv2/plv3/e6666/this-is-a-file-namexxxxxx.bmp") 
;; |     (4 . "u:/plv-1/plv2/plv3/e6666/this-is-a-file-namexxxxxxxxxxx.bmp") 
;; |     (5 . "u:/plv-1/plv2/plv3/e6666/this-is-a-file-namexxxxx.bmp")
;; |     (6 . "u:/plv-1/plv2/plv3/e6666/this-is-a-file-namexxxx.bmp") 
;; |     (7 . "u:/plv-1/plv2/plv3/e6666/this-is-a-file-namexxxxxxxxxxxxx.bmp") 
;; |     (8 . "u:/plv-1/plv2/plv3/e6666/this-is-a-file-namexxxxxxxx.bmp") 
;; |     (9 . "u:/plv-1/plv2/plv3/e6666/this-is-a-file-namexxxxxxxxxx.bmp"))))
;; `----
;;
;; ,---- `this-3-c' -> 9 elts <= 21
;; | (setq this-3-c 
;; |   '(((1 . "e1449") (2 . "e1450.bmp") (3 . "e1451.bmp") 
;; |      (4 . "e1452.bmp") (5 . "e1453.bmp") (6 . "e1454.bmp") (7 . "e1455.bmp")
;; |      (8 . "e1456.bmp") (9 . "e1456.bmp"))
;; |     ((1 . "u:/plv-1/plv2/plv3/e6666/e1449.bmp")(2 . "u:/plv-1/plv2/plv3/e6666/e1450.bmp") 
;; |      (3 . "u:/plv-1/plv2/plv3/e6666/e1451.bmp")(4 . "u:/plv-1/plv2/plv3/e6666/e1452.bmp") 
;; |      (5 . "u:/plv-1/plv2/plv3/e6666/e1453.bmp")(6 . "u:/plv-1/plv2/plv3/e6666/e1454.bmp") 
;; |      (7 . "u:/plv-1/plv2/plv3/e6666/e1455.bmp")(8 . "u:/plv-1/plv2/plv3/e6666/e1456.bmp") 
;; |      (9 . "u:/plv-1/plv2/plv3/e6666/e1456.bmp"))))
;; `----
;;
;; ,---- `this-3-b' -> 10 elts >= 21
;; | (setq this-3-b  
;; |  `(,(acons  10  "this-is-a-file-namexxxxxxx.bmp" (cdar this-3))
;; |    ,(acons 10 "u:/plv-1/plv2/plv3/e6666/this-is-a-file-namexxxxxxx.bmp"(cadr this-3))))
;; `----
;;
;; ,---- `this-3-d' -> 10 elts <= 21
;; | (setq this-3-d 
;; |   `(,(acons 10 "e1456.bmp" (cdar this-3-c)) 
;; |     ,(acons 10 "u:/plv-1/plv2/plv3/e6666/e1456.bmp" (cadr this-3-c))))
;; `----
;;
;; ,---- :UNCOMMENT-TO-TEST `mon-parse-rename-lengths'
;; | (mon-parse-rename-lengths this-3)
;; | (mon-parse-rename-lengths this-3-b) 
;; | (mon-parse-rename-lengths this-3-c)
;; | (mon-parse-rename-lengths this-3-d)
;; | (cadaar (mon-parse-rename-lengths this-3))
;; |  ;=> 32  length of longest name
;; | (car (mon-parse-rename-lengths this-3))
;; |  ;=> ((7 32) (2 31) (4 30) (9 29) (1 28) (8 27) (10 26) (3 25) (5 24) (6 23))
;; | (cdr (mon-parse-rename-lengths this-3))
;; |  ;=> ((1 . "this-is-a-file-namexxxxxxxxx") (2 . "this-is-a-file-namexxxxxxxxxxxx")
;; |  ; |  (3 . "this-is-a-file-namexxxxxx") (4 . "this-is-a-file-namexxxxxxxxxxx") 
;; |  ; |  (5 . "this-is-a-file-namexxxxx") (6 . "this-is-a-file-namexxxx") 
;; |  ; |  (7 . "this-is-a-file-namexxxxxxxxxxxxx") (8 . "this-is-a-file-namexxxxxxxx") 
;; |  ; |  (9 . "this-is-a-file-namexxxxxxxxxx") (10 . "this-is-a-file-namexxxxxxx"))
;; `----

;;; ==============================
;;; :TESTING `mon-parse-rename-images'
;;; Following symbols: `this-3', `this-3-b', `this-3-c', `this-3-d' 
;;; Comparable to return value of:
;;; (mon-parse-rename-images (mon-rename-imgs-in-dir ".bmp" "u:/plv-1/plv2/plv3/e6666"))
;;;  
;;
;; ,---- :UNCOMMENT-TO-TEST
;; | (mon-parse-rename-images this-3)
;; |  ;=> ("bmp" "e6666" "u:/plv-1/plv2/plv3/e6666")
;; | (mon-parse-rename-images this-3-b)
;; |  ;=> ("bmp" "e6666" "u:/plv-1/plv2/plv3/e6666")
;; | (mon-parse-rename-images this-3-c)
;; |  ;=> ("bmp" "e6666" "u:/plv-1/plv2/plv3/e6666")
;; | (mon-parse-rename-images this-3-d)
;; |  ;=> ("bmp" "e6666" "u:/plv-1/plv2/plv3/e6666")
;; `----

;;; ==============================
;;; :TESTING `mon-pad-rename-lengths'
;;; Following symbols: 
;;; `this-4', `this-4-b', `this-4-c', `this-4-d' 
;;; Comparable to return value of:
;;; (mon-parse-rename-lengths 
;;;  (mon-rename-imgs-in-dir ".bmp" (expand-file-name  "e6666" *mon-ebay-images-bmp-path*))
;;
;; ,---- :UNCOMMENT-TO-TEST `mon-pad-rename-lengths'
;; | ;; `this-4'   -> 10 elts >= 21 `this-4-b' -> elts >= 21
;; | ;; `this-4-c' -> 9 elts <= 21  `this-4-d' -> 10 elts <= 21
;; | (setq this-4   (mon-parse-rename-lengths this-3))   
;; | (setq this-4-b (mon-parse-rename-lengths this-3-b)) 
;; | (setq this-4-c (mon-parse-rename-lengths this-3-c)) 
;; | (setq this-4-d (mon-parse-rename-lengths this-3-d)) 
;; | ;;
;; | (mon-pad-rename-lengths this-4)
;; | (mon-pad-rename-lengths this-4-b)
;; | (mon-pad-rename-lengths this-4-c)
;; | (mon-pad-rename-lengths this-4-d)
;; `----
;;; ==============================
;;; :TESTING `mon-shorten-rename-image-path'
;;; Following symbols: `this-5', `this-5-b', `this-5-c', `this-5-d' 
;;; Comparable to return value of:
;;  (mon-shorten-rename-image-path 
;;   (caddr (mon-parse-rename-images 
;;          (mon-rename-imgs-in-dir ".bmp" (expand-file-name  "e6666" *mon-ebay-images-bmp-path*))
;;
;; ,---- :UNCOMMENT-TO-TEST `mon-shorten-rename-image-path'
;; | (setq this-5   (caddr (mon-parse-rename-images this-3)))
;; | (setq this-5-b (caddr (mon-parse-rename-images this-3-b)))
;; | (setq this-5-c (caddr (mon-parse-rename-images this-3-c)))
;; | (setq this-5-d (caddr (mon-parse-rename-images this-3-d)))
;; | ;;
;; | (mon-shorten-rename-image-path  this-5)
;; | (mon-shorten-rename-image-path  this-5-b)
;; | (mon-shorten-rename-image-path  this-5-c)
;; | (mon-shorten-rename-image-path  this-5-d)
;; | (mon-shorten-rename-image-path  "u:/plv-1/plv2/plv3/e6666")
;; | (mon-shorten-rename-image-path  "u:/plv-1/plv2/plv3/e6666/")
;; | (mon-shorten-rename-image-path  (caddr '("bmp" "e6666" "u:/plv-1/plv2/plv3/e6666")))
;; | ;; Following should fail with: => "../plv3/e6666/e1456.bmp"
;; | (mon-shorten-rename-image-path  "u:/plv-1/plv2/plv3/e6666/e1456.bmp") 
;; `----

;;; ==============================
;;; :TESTING `mon-image-rename-propertize'
;; 
;; ,---- :UNCOMMENT-TO-TEST `mon-image-rename-propertize'
;; | (with-current-buffer (get-buffer-create "*MON-IMAGE-RENAME-PROPERTIZE-TEST*")
;; |   (insert
;; | "------------------------------ 
;; | Renaming images in directory: 
;; | > ../some/path/to/somewhere 
;; | ------------------------------
;; | Renaming images of type:  .bmp 
;; | Number of images to rename: NN
;; | ------------------------------ 
;; | image-rename-prefix: [ ]
;; | image-rename-suffix: [ ] 
;; | image-rename-start#: [ ]
;; | ------------------------------
;; | 07) this-is-a-file-namexxxxxxxxxxxxx ▪ [ ]
;; | 2) this-is-a-file-namexxxxxxxxxxxx ▪▪▪ [ ]
;; | 04) this-is-a-file-namexxxxxxxxxxx ▪▪▪ [ ]
;; | 9) this-is-a-file-namexxxxxxxxxx ▪▪▪▪▪ [ ]
;; | 01) this-is-a-file-namexxxxxxxxx ▪▪▪▪▪ [ ]
;; | 8) this-is-a-file-namexxxxxxxx ▪▪▪▪▪▪▪ [ ]
;; | 10) this-is-a-file-namexxxxxxx ▪▪▪▪▪▪▪ [ ]
;; | 03) this-is-a-file-namexxxxxx ▪▪▪▪▪▪▪▪ [ ]
;; | 3) this-is-a-file-namexxxxxx ▪▪▪▪▪▪▪▪▪ [ ]
;; | 06) this-is-a-file-namexxxx ▪▪▪▪▪▪▪▪▪▪ [ ]
;; | 5) this-is-a-file-namexxxxx ▪▪▪▪▪▪▪▪▪▪ [ ]")
;; |   (mon-image-rename-propertize (point-min))
;; |   (display-buffer (current-buffer) t))
;; `----

;;; ==============================
;;; :TESTING `mon-build-rename-buffer'
;;;
;; ,---- :UNCOMMENT-TO-TEST `mon-build-rename-buffer'
;; | (let ((chk-6666 default-directory)
;; |       (the-6666 (concat *mon-ebay-images-bmp-path* "/e6666")))
;; |   (unless (equal chk-6666 the-6666)
;; |     (if (file-directory-p the-6666) 
;; |         (cd the-6666)
;; |         (error (format (concat ":TESTING :FUNCTION `mon-build-rename-buffer'"
;; |                                " -- must be in directory: \n%40c%s")
;; |                        32 the-6666))))
;; |     (mon-build-rename-buffer ".bmp"))
;; `----

;;; ==============================
;;; :CLEANUP Testing Variables
;;
;; ,----
;; | (mapc (lambda (x)(progn (makunbound x)(unintern x)))
;; |   '(this-3 this-3-b this-3-c this-3-d
;; |     this-4 this-4-b this-4-c this-4-d
;; |     this-5 this-5-b this-5-c this-5-d))
;; `----



;;; ==============================
;;; :TODO Finish below so `mon-get-nefs-in-dir' and `mon-get-ebay-bmps-in-dir'
;;; can take an interactive arg. The definition below is different than the one
;;; provided in ebay-template.el
;;;
;; (defun mon-get-imgs-in-dir-int (bmp-or-nef fll-pth &optional alt-pth intp)
;;   "Helper function for retrieving imgs in dir.
;;  :CALLED-BY `get-bmps-in-dir' and `get-nefs-in-dir'."
;; (interactive 
;;  (list 
;;   (let ((choice '("bmp" "nef")))
;;     (completing-read "get img of type [bmp|nef]: " choice nil t "bmp"))
;;   (yes-or-no-p "with full path:")
;;   (yes-or-no-p "Use an alt path: ")
;;   t))
;;   (let* ((was-int intp)
;; 	 (fp fll-pth)
;; 	 (altp alt-pth)
;; 	 (bmpORnef bmp-or-nef)
;; 	 (def-dir (if (string= bmp-or-nef "bmp")
;; 		     *mon-ebay-images-bmp-path*
;; 		    *mon-nef-scan-nefs-path*))
;; 	 (alt (cond ((and altp was-int)
;; 		     (read-directory-name "Find in this path:" def-dir default-directory))
;; 		    ((and (not intp) (not altp)) 
;; 		     (
;; 		      default-directory
;; 			      )))
;;
;; (let* ((img-check  '("\\.bmp" "\\.nef"))
;;        (path-check `(,default-directory ,*mon-ebay-images-bmp-path* ,*mon-nef-scan-nefs-path* ))
;;        (test-imgp 
;; 	(setq path-check (car patch-check)
;; 	(setq img-check  (car img-check) ;(setq img-check (cdr img-check)
;; 	(while (not (directory-files path-check nil img-check))
;;      (setq path-check (lambda (img-check) 
;; 	  (unless (directory-files default-directory nil img-check))
;;
;; ;(and  was-int altp))
;; ;(mon-get-imgs-in-dir-int "bmp" nil nil)
;; ;(concat *mon-ebay-images-bmp-path* "/e1072/") t)
;; ;
;;
;; 	 (alt (if (and altp was-int)
;; 		     (read-directory-name "Find in this path:" def-dir default-directory)
;; 		    altp))
;; 	 (img-type
;; 	  (if (string= bmp-or-nef "bmp")
;; 	      (get-bmps-in-dir fp alt)
;; 	    (get-nefs-in-dir fp alt)))
;; 	 (vals-rtrn))
;;     (setq vals-rtrn SOME_VAL)
;;     (message "get imgs of type: %s | show full-path %s | in this path: %s" bmpORnef fp alt)
;;   vals-rtrn))
;;;
;; (defalias 'get-imgs-in-dir 'mon-get-imgs-in-dir-int)
;;
;;; :TEST-ME (mon-get-imgs-in-dir-int "bmp" t (concat *mon-ebay-images-bmp-path* "/e1072/"))
;;; :TEST-ME (mon-get-imgs-in-dir-int "bmp" nil (concat *mon-ebay-images-bmp-path* "/e1072/"))
;;; :TEST-ME (mon-get-imgs-in-dir-int "bmp" t nil)
;;; :TEST-ME (call-interacively 'mon-get-imgs-in-dir-int)
;;; ==============================

;;; ==============================
(provide 'mon-rename-image-utils)
;;; ==============================

 
;; Local Variables:
;; generated-autoload-file: "./mon-loaddefs.el"
;; coding: utf-8
;; End:

;;; ================================================================
;;; mon-rename-image-utils.el ends here
;;; EOF
