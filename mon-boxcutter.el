;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*- mode: EMACS-LISP; -*-
;;; this is mon-boxcutter.el
;;; ================================================================
;;; DESCRIPTION:
;;; For w32 screencaptures from emacs using Matthew D. Rasmussen's Boxcutter
;;; screen-capture program executables: boxcutter-fs.exe and boxcutter.exe
;;; :SEE below.
;;;
;;; mon-boxcutter.el provides features such as:
;;;
;;; * setting an arbitrary crop as X1,Y1,X2,Y2;
;;; * capturing a specific window (w/ option for either inside/outside borders);
;;; * capturing a specific frame;
;;; * capturing fullscreen;
;;; * capturing windows of external apps.
;;;
;;; My goal w/ boxcutter was to allow me to invoke an interactive Emacs command
;;; to capture the windows of _other_ apps and the ability to specify
;;; introspective captures of Emacs objects was an after thought. The primary
;;; target was firefox - e.g. screen scraping -> OCR of content trapped in
;;; images :)
;;;
;;; However, firefox windows needn't be the only target of capture. One might
;;; just as easily capture a portion of a zoomed image.
;;;
;;; There is a pkg screenshot.el which also supports
;;; screen-captures. Sreenshot.el was entirely w32 agnostic when I began
;;; developing mon-boxcutter this was a primary motivation.
;;;
;;; boxcutter.exe is one of the few (perhaps only) screenshot utility that can
;;; be called from the command line AND has a compatible license LGPL. This is
;;; important, esp. on w32. Many w32 graphics apps lack FOSS licensing...
;;;
;;; :NOTE mon-boxcutter is a work in progress. Posting so I don't forget :)
;;;
;;; FUNCTIONS:►►►
;;; `boxcutter-gen-tstamp'
;;; `boxcutter-incr-cntr'
;;; `boxcutter-verify-image-type'
;;; `boxcutter-gen-fname'
;;; `boxcutter-big-n-small'
;;; `boxcutter-get-win-coords'
;;; `boxcutter-get-frame-coords'
;;; `boxcutter-capture'
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
;;; `*boxcutter-conversion-program*'
;;; `*boxcutter-path*'
;;; `*boxcutter-captures*'
;;; `*boxcutter-title-bar-vig*'
;;; `*boxcutter-counter*'
;;; `*boxcutter-tstamp*'
;;; `*boxcutter-captured-last*'
;;;
;;; ALIASED/ADVISED/SUBST'D:
;;;
;;; DEPRECATED:
;;;
;;; RENAMED:
;;;
;;; MOVED:
;;;
;;; TODO:
;;; Currently doesn't work on multi-head setups (e.g. more than one monitor)
;;; :SEE (URL `http://lists.gnu.org/archive/html/emacs-devel/2006-07/msg00305.html')
;;; :SEE (URL `http://john.fremlin.de/programs/linux/read-edid/')
;;;
;;; NOTES:
;;; Following forms evaluated to these values when this package was created.
;;;
;;; (about-emacs) ;=>GNU Emacs 23.1.50.1 (i386-mingw-nt5.1.2600)
;;;                  of 2009-06-30 on LENNART-69DE564 (patched)
;;;
;;; (display-pixel-width) ;=> 1024
;;; (display-pixel-height) ;=> 768
;;; (display-mm-height) ;=> 270
;;; (display-mm-width) ;=> 360
;;; (x-server-vendor) ;=> "Microsoft Corp."
;;; (x-server-version) ;=> (5 1 2600)
;;; (frame-terminal) ;=> #<terminal 1 on >
;;; (terminal-list) ;-> (#<terminal 1 on >)
;;; (terminal-name) ;-> ""
;;; (terminal-parameters)
;;; ;-> ((normal-erase-is-backspace . 1) (x-setup-function-keys . t))
;;; :NOTE Even though I'm actually running a 3(three) head display on two cards:
;;; (display-screens) ;=> 1
;;;
;;; SNIPPETS:
;;; Following forms useful for investigating your current setup:
;;; image-library-alist
;;; (current-frame-configuration)
;;; (princ (pp (frame-parameters))(current-buffer))
;;; (executable-find "boxcutter-fs.exe") (executable-find "boxcutter.exe")
;;; (or (executable-find "imconvert.exe") (executable-find "convert.exe"))
;;;
;;; REQUIRES:
;;; Matthew D. Rasmussen's Boxcutter screen-capture program executables:
;;; boxcutter-fs.exe and boxcutter.exe
;;; Tested with boxcutter :VERSION 1.3 of 2009.06.04 LGPL v2.1 Feb. 1999
;;; :SEE (URL `http://rasm.ods.org/boxcutter/')
;;;      (URL `http://rasm.ods.org/boxcutter/download/boxcutter-1.3.zip')
;;;      (URL `http://rasm.ods.org/boxcutter/download/boxcutter-1.3/')
;;;      (URL `http://people.csail.mit.edu/rasmus/')
;;;
;;; ImageMagic executables: convert.exe 
;;; Tested with imagemagick-6.5.1-q8 :VERSION  ImageMagick 6.5.1-3 2009-04-13 Q8
;;; :NOTE convert.exe should be renamed to imconvert.exe on W32 
;;; :SEE (URL `http://www.imagemagick.org/script/index.php')
;;;      (URL `http://www.imagemagick.org/script/binary-releases.php#windows')
;;;
;;; CAVEATS USING IMAGE-MAGICK ON W32:
;;; :Note the function `boxcutter-capture' calls out to ImageMagick's
;;; convert command which on MON system is renamed to `imconvert' 
;;; (This is per the recommendations of the Image-Magick distributors).
;;; :SEE (URL `http://www.imagemagick.org/Usage/windows/')
;;; It is strongly recommended that W32 users of Image Magick adopt this
;;; practice. This is done because of a W32 compatibility issue as convert.exe
;;; is a system level command and users of ImageMagic running non GNU-*NIX
;;; systems should substitute `convert' for `imconvert'. Likewise, on W32 when
;;; running ImageMagick's `convert' as a shell-process (synchronous or
;;; otherwise) configuration is much more difficult when this renaming isn't
;;; provided.
;;;
;;; To wit, this renaming is needed because on a W32 system:
;;; (executable-find "convert") ;=> c:/WINDOWS/system32/convert.exe"
;;; (executable-find "imconvert") ;=>c:/SOMEPATH/imagemagick-6x-q8/imconvert.exe
;;;
;;; For those that think this aliasing is unnecessary _PLEASE-NOTE_ "convert" is
;;; a W32 command in the W32 system directory. It is used for converting FAT32
;;; to NTFS. It is suggested you rename the IM convert command to
;;; "imconvert.exe" to distinguish the two commands. You can't rename the system
;;; command, as a windows service pack would just restore it, ignoring the
;;; renamed version. :SEE additional discussion here:
;;; (URL `http://www.imagemagick.org/Usage/windows/index.html')
;;;
;;; !!!You've now been warned three times!!! :)
;;;
;;; (require 'cl)
;;; (require 'thumbs)  ;MAYBE
;;; (reuquire 'mon-utils) ;Uses various procedures from that package.
;;;
;;; THIRD PARTY CODE:
;;; boxcutter executables
;;; ImageMagic executables
;;; 
;;; AUTHOR: MON KEY
;;; MAINTAINER: MON KEY
;;;
;;; PUBLIC-LINK: (URL `http://www.emacswiki.org/emacs/mon-boxcutter.el')
;;; FIRST-PUBLISHED: <Timestamp: #{2009-11-06T18:23:43-05:00Z}#{09455} - by MON>
;;;
;;; FILE-CREATED: 
;;; <Timestamp: #{2009-10-15T15:16:36-04:00Z}#{09424} - by MON>
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

;;; :NOTE mon-boxcutter.el utilizes procedures from :FILE mon-utils.el
(unless (featurep 'mon-utils)
  (eval-when-compile (require 'mon-utils)))
;;
(eval-when-compile (require 'cl))
;;
(require 'thumbs)
      
;;; ==============================
;;; CREATED: <Timestamp: #{2009-10-21T20:14:43-04:00Z}#{09434} - by MON>
(defvar *boxcutter-conversion-program*
  (or (executable-find "imconvert.exe")
      thumbs-conversion-program
      (executable-find "convert.exe"))
  "If imagemagick is installed system it ought to be named imconvert.exe.
If it isn't consider renaming it from convert.exe to imconvert.exe
Unfortunately, Windows XP has a program called CONVERT.EXE in
C:/WINDOWS/SYSTEM32/ for partioning NTFS system.
In order for Emacs to find the 'convert' in your ImageMagick directory, you may
need to customize this value to the absolute filename.
:CALLED-BY 
:SEE-ALSO `*boxcutter-path*', `boxcutter-capture'\n►►►")

;;; ==============================
;;; :NOTE 
;;; On MON system the environmental variable "SP_BXC" -> to the boxcutter path.
;;; Either (setenv "SP_BXC" <PATH/TO/BOXCTR-EXECS>) or supply the path var below.
;;; (executable-find "boxcutter-fs.exe") (executable-find "boxcutter.exe")
;;; CREATED: <Timestamp: #{2009-10-17T17:58:20-04:00Z}#{09426} - by MON>
(defvar *boxcutter-path* 
  (or (getenv "SP_BXC")
      (file-name-sans-extension
       (executable-find "boxcutter.exe")))
  "*Path to the local boxcutter exectuable.
:CALLED-BY `boxcutter-capture'
:SEE-ALSO `*boxcutter-conversion-program*'\n►►►")
;;
;;; :TEST-ME *boxcutter-path*

;;; ==============================
;;; CREATED: <Timestamp: #{2009-10-17T17:58:53-04:00Z}#{09426} - by MON>
(defvar *boxcutter-captures*
  (concat mon-naf-mode-notes "/Screenshots/boxcutter-tests")
  "*Path or for holding boxcutter screen-captures.
:CALLED-BY `boxcutter-gen-fname'\n:SEE-ALSO `boxcutter-capture'\n►►►")
;;
;;; :TEST-ME *boxcutter-captures*

;;; ==============================
;;; CREATED: <Timestamp: #{2009-10-21T18:33:08-04:00Z}#{09433} - by MON>
(defvar *boxcutter-title-bar-vig* 20
  "*The approximate pixel height of the title-bar.
To Access this setting on winxp find the Display Properties dialog.
Right click on the desktop and select 'Properties'
Or, from Control Panel select 'Display'
Once the Display Properties dialog is open select
Appearance -> Advanced -> Item -> Active Title Bar
:NOTE the 'Size' field, this is the vigorish stolen by MS-Windows manager.
:CALLED-BY `*boxcutter-title-bar-vig*'
:SEE-ALSO `boxcutter-capture'\n►►►")
;;; `boxcutter-get-frame-coords', `boxcutter-get-win-coords'
;;
;;; :TEST-ME *boxcutter-title-bar-vig*

;;; ==============================
;;; CREATED: <Timestamp: #{2009-10-17T18:29:55-04:00Z}#{09426} - by MON>
(defvar *boxcutter-counter* 0
  "*Counter for incrementing screnshots.\n
:CALLED-BY `boxcutter-incr-cntr', and `boxcutter-gen-fname'.\n
:SEE-ALSO `*boxcutter-tstamp*', `boxcutter-capture'\n►►►")

;;; ==============================
;;; CREATED: <Timestamp: #{2009-10-17T18:29:51-04:00Z}#{09426} - by MON>
(defvar *boxcutter-tstamp* "%y-%m-%d"
  "*Timestring concatenated with`*boxcutter-counter*'s incrementer.
Return timestring formatted  yy-mm-dd e.g. 09-10-17.\n
:EXAMPLE *boxcutter-tstamp*\n\n:CALLED-BY `boxcutter-gen-tstamp'
:SEE-ALSO `boxcutter-capture'.\n►►►")
;;
;;; :TEST-ME *boxcutter-tstamp*

;;; ==============================
;;; CREATED: <Timestamp: #{2009-10-21T20:44:03-04:00Z}#{09434} - by MON>
(defvar *boxcutter-captured-last* nil
  "*List of filenames from the most recently evaluated `boxcutter-capture' event.
List has the form (\"/path/to/capturefile.bmp\" \"/path/to/capturefile.{reduced}\")
List generated with `boxcutter-big-n-small's interface to `boxcutter-gen-fname'.
:SEE-ALSO `*boxcutter-captures*'.\n►►►")

;;; ==============================
;;; CREATED: <Timestamp: #{2009-10-19T18:21:00-04:00Z}#{09431} - by MON>
(defun boxcutter-gen-tstamp ()
"Return a time-string per the `*boxcutter-tstamp*' specs.\n
:EXAMPLE\n(boxcutter-gen-tstmp)\n\n:CALLED-BY `boxcutter-gen-fname'
:SEE-ALSO `boxcutter-incr-cntr',`*boxcutter-counter*',`boxcutter-capture'\n►►►"
  (format-time-string *boxcutter-tstamp*))
;;
;;; :TEST-ME (boxcutter-gen-tstamp)

;;; ==============================
;;; CREATED: <Timestamp: #{2009-10-17T18:29:49-04:00Z}#{09426} - by MON>
(defun boxcutter-incr-cntr (&optional step-val reset)
  (interactive "i\nP")
  "Increment the the current file-name counter by 1 or STEP-VAL.
When RESET is non-nil reset `*boxcutter-counter*' to 0 before incrementing.
:CALLED-BY `boxcutter-gen-fname'.
:SEE-ALSO `boxcutter-gen-tstamp',\n►►►"
  (when reset (setq *boxcutter-counter* 0))
  (format "%04d" (incf *boxcutter-counter* (or step-val 1))))
;;
;;; :TEST-ME (boxcutter-incr-cntr nil t)
;;; :TEST-ME (boxcutter-incr-cntr 3)
;;; :TEST-ME (boxcutter-incr-cntr 7 t)
;;; :TEST-ME (boxcutter-incr-cntr)

;;; ==============================
;;; Following does image extension type checking. Can be used elsewhere as well.
;;; :REQUIRES
;;; `image-type-available-p', `image-type-from-file-name' in :FILE image.el
;;; `mon-string-after-index', `mon-string-to-symbol', `mon-string-from-symbol'
;;; `mon-string-to-sequence', `mon-string-from-sequence' in :FILE mon-utils.el
;;; CREATED: <Timestamp: #{2009-10-17T17:58:20-04:00Z}#{09426} - by MON>
(defun boxcutter-verify-image-type (img-typ)
  "Verify that image-type IMG-TYP can be manipulated with ImageMagick's convert.
Return a canonical representation for IMG-TYP.\n
Valid img-typ args are:\n {jpg png gif tiff jpeg xpm xbm pbm bmp psd}\n
:NOTE IMG-TYP is not case sensitive, can be a string or symbol, strips away `.'.
:EXAMPLE\n\(boxcutter-verify-image-type 'jpg\)
\(boxcutter-verify-image-type '.jpg\)
\(boxcutter-verify-image-type \"jpg\"\)
\(boxcutter-verify-image-type \".jpg\"\)
\(boxcutter-verify-image-type \"I WILL FAIL with nil\"\)
\(boxcutter-verify-image-type '\(I WILL FAIL with error\)\)
:CALLED-BY `boxcutter-big-n-small'
:SEE-ALSO `image-type-available-p', `image-type-from-file-name'
`*boxcutter-conversion-program*' `image-type-file-name-regexps'.
:NOTE Any potentially any file format that is RW by ImageMagick's 
convert command could be supported. For a complete list of formats supported
:SEE (URL `http://www.imagemagick.org/script/formats.php')\n►►►"
  (eval-when-compile (require 'image))
  (let ((ftype img-typ))
    (car
     (member
      (cond ((stringp ftype)
             (if (image-type-from-file-name
                  (let* ((ftype-str ftype)
                         (chk-ftyp (string-match-p "\\." ftype-str)))
                    (if chk-ftyp
                        ftype-str
                        (setq ftype-str (concat "." ftype-str)))))
                 (let* ((ext-is-str (downcase ftype)) ;we still need to check for leading `.'
                        (ext-seq (mon-string-to-sequence ext-is-str)))
                   (mon-string-to-symbol
                    (if (eq (elt ext-seq 0) 46)
                        (mon-string-from-sequence (cdr ext-seq))
                        ext-is-str)))))
            ((symbolp ftype)
             (let* ((f-sym-str (mon-string-from-symbol ftype))
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
                   ftype)))
            ((or (listp ftype) (vectorp ftype)
                 (and (stringp ftype) (arrayp ftype)))
                 (error (format "%s is a %s - not a valid arg for `img-typ'"
                                ftype (type-of ftype))))
            (t (error "This argument sux - we're not type checking further so try again")))
      '(jpg png gif tiff jpeg xpm xbm pbm bmp)))))
;;
;;; :TEST-ME (boxcutter-verify-image-type 'jpg)
;;; :TEST-ME (boxcutter-verify-image-type '.jpg)
;;; :TEST-ME (boxcutter-verify-image-type "jpg")
;;; :TEST-ME (boxcutter-verify-image-type ".jpg")
;;; :TEST-ME (boxcutter-verify-image-type "I WILL FAIL with nil")
;;; :TEST-ME (boxcutter-verify-image-type '(I WILL FAIL with error))

;;; ==============================
;;; CREATED: <Timestamp: #{2009-10-17T18:46:51-04:00Z}#{09426} - by MON>
(defun boxcutter-gen-fname (&optional fname-string step-val reset)
  "Generate a boxcutter filename with a timestamp and counter.\n
When filename FNAME-STRING \(a number or string\) is non-nil concatenate
When STEP-VAL is non-nil increment file-number by step-val.
When RESET is non-nil reset `*boxcutter-counter*' to 0 before incrementing.
:EXAMPLE\n(boxcutter-gen-fname)\n(boxcutter-gen-fname 3)\n
\(boxcutter-gen-fname \"three\" 3)
\(boxcutter-gen-fname \"reset-me-n-incr-3\" 3 t)\n
:CALLED-BY `boxcutter-big-n-small'
:SEE-ALSO `boxcutter-capture', `boxcutter-gen-tstamp' `boxcutter-incr-cntr',
`*boxcutter-counter*',`*boxcutter-tstamp*'\n►►►"
(let (bxc-gf)
  (unwind-protect
     (setq bxc-gf
           (concat
            *boxcutter-captures* "/"
            (if fname-string
                (concat
                 (cond ((stringp fname-string)
                        fname-string)
                       ((numberp fname-string) (number-to-string fname-string))
                       (t (error "Must provide a string or number value for fname-string")))
                 "-"))
            (boxcutter-gen-tstamp) "-"
            (boxcutter-incr-cntr step-val reset) ".bmp"))
  (setq *boxcutter-captured-last* nil))
(if bxc-gf (setq *boxcutter-captured-last* bxc-gf))))

;;
;;; :TEST-ME (progn (boxcutter-gen-fname) *boxcutter-captured-last*)
;;; :TEST-ME (progn (boxcutter-gen-fname "three" 3) *boxcutter-captured-last*)
;;; :TEST-ME (progn (boxcutter-gen-fname "three" 3 t) *boxcutter-captured-last*)
;;; :TEST-ME (progn (boxcutter-gen-fname "reset-three-now-incr-six" 6 t) *boxcutter-captured-last*)
;;; Following signals an error and sets *boxcutter-captured-last* to nil
;;; :TEST-ME  (boxcutter-gen-fname [3lls]) *boxcutter-captured-last*

;;; ==============================
;;; CREATED: <Timestamp: #{2009-10-21T19:32:34-04:00Z}#{09433} - by MON>
(defun boxcutter-big-n-small (smaller-type &optional fname-string step-val reset)
  "SMALLER-TYPE is a file extension valid.
FNAME-STRING is an alternate string to use as filename prefix.
When STEP-VAL is non-nil increment file-number by step-val.
When RESET is non-nil reset `*boxcutter-counter*' to 0 before incrementing.
:CALLED-BY `boxcutter-capture'
:SEE-ALSO `boxcutter-gen-fname', `*boxcutter-captured-last*', 
`boxcutter-verify-image-type'.\n►►►"
  (let* ((bcg-f  (boxcutter-gen-fname fname-string step-val reset))
         (gen-fname-ok-p *boxcutter-captured-last*)
         (sm-ok-p  (if gen-fname-ok-p
                       (boxcutter-verify-image-type smaller-type)
                       (error (format "Can not set *boxcutter-captured-last* with %s"
                                      fname-string))))
         (bcg-sm   (concat
                    (file-name-sans-extension bcg-f)
                    (format ".%s"
                            (if sm-ok-p sm-ok-p
                                'jpg)))))
    (if gen-fname-ok-p (setq *boxcutter-captured-last* `(,bcg-f ,bcg-sm)))))
;;
;;; :TEST-ME (boxcutter-big-n-small '.png)
;;; :TEST-ME (boxcutter-big-n-small 'png)
;;; :TEST-ME (boxcutter-big-n-small ".png")
;;; :TEST-ME (boxcutter-big-n-small ".PNG")
;;; :TEST-ME (boxcutter-big-n-small "no-dice" '(i fail))

;;; ==============================
;;; CREATED: <Timestamp: #{2009-10-19T19:06:09-04:00Z}#{09431} - by MON>
(defun boxcutter-get-win-coords (&optional buffer inside)
  "Return coordinates at windows pixel edges as comma delimited string.
When BUFFER is non-nil return coordinates of buffer's window if its a live one.
Default is to use current-buffer.
When INSIDE is non-nil return buffer's window coordinates with
`window-inside-pixel-edges'. Default is with `window-pixel-edges'.\n
:EXAMPLE\n\(boxcutter-get-win-coords\)
\(boxcutter-get-win-coords \(get-buffer \"*Help*\"\) t\)
\(boxcutter-get-win-coords \(window-buffer \(previous-window\)\) t\)
:CALLED-BY `boxcutter-capture'
:SEE-ALSO `boxcutter-get-frame-coords', `*boxcutter-title-bar-vig*'\n►►►"
  (let* ((buf-nm buffer)
         (buf-nm-or-cb
          (get-buffer-window
           (if buf-nm
               (get-buffer buf-nm)
               (current-buffer))))
         (the-win
          (if (window-live-p buf-nm-or-cb)
              buf-nm-or-cb
              (error "not a live window")))
         (dstrct-wipe (if inside
                          (window-inside-pixel-edges the-win)
                          (window-pixel-edges the-win)))
         (lft (+ (elt dstrct-wipe  0)
                 (elt (car (boxcutter-get-frame-coords)) 0)
                 ;; inside arg we need to un-compensate
                 (if inside (- (* (frame-parameter nil 'border-width) 2)) 0)))
         (top (+ (elt dstrct-wipe  1)
                 (elt (car (boxcutter-get-frame-coords)) 1)
                 ;; Note to devels - `height' is a  _stupid_ name for this param!
                 ;; Is this modeline?
                 (- (/ (frame-parameter nil 'height) 2)
                    (% (frame-parameter nil 'height) 2))))
         (rgt (+ lft
                 (elt dstrct-wipe  2)))
         (btm (+ (elt (car (boxcutter-get-frame-coords)) 1)
                 (- (/ (frame-parameter nil 'height) 2)
                    (% (frame-parameter nil 'height) 2))
               (elt dstrct-wipe  3))))
    (format "%d,%d,%d,%d" lft top rgt btm)))
;;
;;; :TEST-ME (boxcutter-get-win-coords)
;;; :TEST-ME (boxcutter-get-win-coords nil t)
;;; :TEST-ME (boxcutter-get-win-coords (get-buffer "*Help*") t)
;;; :TEST-ME (boxcutter-get-win-coords (window-buffer (previous-window)) t)

;;boxcutter-get-frame-coords
;;; ==============================
;;; :NOTE
;;;  (display-pixel-width) (display-pixel-height)
;;;  (assoc 'top (frame-parameters)) ;e.g. (top + -4)
;;;  (assoc 'left (frame-parameters)) ;e.g. (left + -4)
;;; :CREATED <Timestamp: #{2009-10-19T19:52:13-04:00Z}#{09431} - by MON>
(defun boxcutter-get-frame-coords ()
  "Return coordiantes of current frame on display.\n
:EXAMPLE\n\(car \(boxcutter-get-frame-coords\)\)
\(cdr \(boxcutter-get-frame-coords\)\)\n
:CALLED-BY `boxcutter-capture', `boxcutter-get-win-coords'.
:SEE-ALSO `*boxcutter-title-bar-vig*'\n►►►"
  (let ((t-frm  (frame-parameter nil 'top))
         (l-frm (frame-parameter nil 'left))
         (fph (frame-pixel-height)) ;text-line height no menu-bars
        (fpw (frame-pixel-width))
        (brdr-h (+ (if (> (frame-parameter nil 'tool-bar-lines) 0)
                       tool-bar-images-pixel-height
                       0)
                   (if (> (frame-parameter nil 'menu-bar-lines) 0)
                       (frame-char-height)
                       0)))
        (brdr-w (+  (* (frame-parameter nil 'border-width) 2)
                         (* (frame-parameter nil 'internal-border-width) 2)
                         (frame-parameter nil 'scroll-bar-width)
                         (frame-parameter nil 'right-fringe)
                         (frame-parameter nil 'left-fringe)))
        (frm-coords))
    ;; If frame isn't full size cut up the top and left params.
    (setq t-frm (if (listp t-frm) (abs (cadr t-frm)) t-frm))
    ;; Subtract away for the stupid w32 title-bar.
    (setq t-frm (+ t-frm *boxcutter-title-bar-vig*))
    (setq l-frm (if (listp l-frm) (abs (cadr l-frm)) l-frm))
    ;; Add offsets to the pix-width/height.
    (setq fph (+ t-frm fph brdr-h))
    (setq fpw (+ l-frm fpw brdr-w))
    (setq frm-coords `(,l-frm ,t-frm ,fpw ,fph))
    (setq frm-coords (cons
                      frm-coords
                      (format "%d,%d,%d,%d"
                              (elt frm-coords 0)
                              (elt frm-coords 1)
                              (elt frm-coords 2)
                              (elt frm-coords 3))))
    frm-coords))
;;
;;; :TEST-ME (car (boxcutter-get-frame-coords))
;;; :TEST-ME (cdr (boxcutter-get-frame-coords))
;;
;;; ==============================
;;; CREATED: <Timestamp: #{2009-10-19T19:51:18-04:00Z}#{09431} - by MON>
(defun* boxcutter-capture (conv->ftype &key 
                                       bmp-prfx step-val reset-count
                                       do-screen do-frame do-crop do-window w-buffer inside-edge)
"CONV->FNAME  is the file type to convert screen capture to.
BMP-PRFX    - When non-nil concat prefix \(a number or string\) onto filename.
STEP-VAL    - When non-nil increment file-name's file-number by step-val.
RESET-COUNT - When non-nil reset `*boxcutter-counter*' to 0 before incrementing.
DO-SCREEN   - When non-nil capture the full screen on display.
DO-FRAME    - When non-nil capture the full frame. e.g. frame is not maximized.
DO-CROP     - When non-nil capture a crop selection. Can do windows of ext APP.
DO-WINDOW   - When non-nil capture current-buffer window or window W-BUFFER name.
W-BUFFER    - When DO-WINDOW is non-nil capture the window W-BUFFER name.
INSIDE-EDGE - When non-nil capture the inside of frame or window.\n
:SEE-ALSO
►►►"
  (let* ((gen-fname (boxcutter-big-n-small conv->fname
                                           ))
         (big-fname (if *boxcutter-captured-last* 
                        (car *boxcutter-captured-last*)
                        (error "Did not find a valid filename pair")))
           (small-fname (cadr *boxcutter-captured-last*)))
    (if do-crop
        (w32-shell-execute
         "open"
         (concat *boxcutter-path* "\\boxcutter.exe") ;open->exec
         big-fname
         3)
        (w32-shell-execute
         "open"
         (concat *boxcutter-path*
                 (cond ((or do-crop do-window)
                        "\\boxcutter.exe")
                       (do-screen "\\boxcutter-fs.exe")
                       ((or do-frame t)
                        "\\boxcutter.exe")))
         (concat
          (cond (do-window (format " -c %s " (boxcutter-get-win-coords
                                              (when w-buffer w-buffer)
                                              (when inside-edge inside-edge))))
                (do-frame
                 (format " -c %s " (cdr (boxcutter-get-frame-coords)))))
          big-fname)
         (cond ((or do-screen do-frame) 3)
               (do-window 0))))))
;;
;;; :TEST-ME (boxcutter-capture nil nil nil t)
;;; :TEST-ME (boxcutter-capture t) nil t)
;;; (boxcutter-capture nil nil nil t "frame.el")
;;; :TEST-ME (boxcutter-capture 'jpg nil nil t)         ;set-crop
;;; :TEST-ME (boxcutter-capture 'jpg nil t)             ;full frame
;;; :TEST-ME (boxcutter-capture 'jpg t)                 ;full-screen
;;; :TEST-ME (boxcutter-capture 'jpg nil nil nil t nil) ; t) ;do-this-window
;;; :TEST-ME (boxcutter-capture 'jpg nil nil nil t nil t) ;do-this-window-inside
;;; :TEST-ME (boxcutter-capture 'jpg nil nil nil t "mon-doc-help-utils.el" t) ;do-other-window


;;; ==============================
;;; CREATED: <Timestamp: #{2009-10-21T19:42:46-04:00Z}#{09433} - by MON>
(defun boxcutter-capture-set-crop (&optional small-format)
  (interactive)
  (boxcutter-capture
   (if small-format small-format 'jpg)
   nil nil t))

;;(shell-command (concat (executable-find "imconvert.exe")
;; "C:\\"Program Files\"\\imagemagick-6.5.1-q8\\imconvert.exe"
;;; " " cap-big " " cap-small))))
;;;   (boxcutter-call-convert cap-big cap-small "")))

;;; ==============================
;;; COURTESY :FILE thumbs.el :WAS `thumbs-call-convert'
(defun boxcutter-call-convert (filein fileout action
                               &optional arg output-format action-prefix)
  "Call the convert program.
w32 _ought_ to use \"imconvert.exe\" :SEE *boxcutter-conversion-program*.
FILEIN is the input file,
FILEOUT is the output file,
ACTION is the command to send to convert.
Optional arguments are:
ARG any arguments to the ACTION command,
OUTPUT-FORMAT is the file format to output (default is jpg),
ACTION-PREFIX is the symbol to place before the ACTION command
              (defaults to '-' but can sometimes be '+')."
  (call-process *boxcutter-conversion-program* nil nil nil
		(or action-prefix "-")
		action
		(or arg "")
		filein
		(format "%s:%s"	(or output-format "jpg") fileout)))

;;; (call-process-shell-command 
;;;  (concat 
;;;   "\"imconvert\" \"./" img-bmp
;;;   "\" +dither -resize 100 -modulate 105,120 -posterize 6 " 
;;;   "\"./" img-png "\"") nil t t)
;;
;; (defvar dragbox-image-options "-density 150x150 -compress none -monochrome")
;;; ==============================

;;; ==============================
(provide 'mon-boxcutter)
;;; ==============================

;;; ================================================================
;;; mon-boxcutter.el ends here
;;; EOF
