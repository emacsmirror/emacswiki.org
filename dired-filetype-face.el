;;;; dired-filetype-face.el  --- faces for different filetype in dired buffer.
;;;  Time-stamp: <jixiuf 2011-04-04 16:05:47>

;; Filename: dired-filetype-face.el
;; Description: set faces for different file type in dired buffer.
;; Author: Joseph <jixiuf@gmail.com>
;; Maintainer: Joseph <jixiuf@gmail.com>
;; Copyright (C) 2011~, Joseph, all rights reserved.
;; Created: 2011-04-04
;; Version: 0.1.0
;; URL: http://www.emacswiki.org/emacs/download/dired-filetype-face.el
;; Keywords: dired filetype face custom
;; Compatibility: (Test on GNU Emacs 23.2.1 ,24.0.50)
;;
;; Features that might be required by this library:
;;
;; `dired' `custom'
;;
;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;;  Set faces for different file type in dired buffer. I use a dark
;;  background, so maybe the default face doesn't meet your request.
;;  you can : M-x: customize-group dired-filetype-face  RET
;;  And maybe M-x: customize-group dired-faces  RET
;;  may do some help for you.
;;
;;
;;; Installation:
;;
;; Just put `dired-filetype-face.el' to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; Add the following to your ~/.emacs startup file.
;;
;; (eval-after-load 'dired '(progn (require 'dired-filetype-face)))
;;
;; No need more.
;;

;;; Commands:
;;
;; Below are complete command list:
;;
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;

;;; Code:

(require 'dired)
(require 'custom)

(defgroup dired-filetype-face nil
  "set faces for different filetype in dired buffer."
  :prefix "dired-file-type-"
  :group 'dired-faces)

(defface dired-filetype-omit
  '((t (:foreground "Chartreuse")))
  "face for omit files"
  :group 'dired-filetype-face )

(defface dired-filetype-document
  '((t (:foreground "DarkCyan")))
  "face for rich document files "
  :group 'dired-filetype-face )

(defface   dired-filetype-plain
  '((t (:foreground "LightSalmon")))
  "face for plain text files "
  :group 'dired-filetype-face)

(defface dired-filetype-common
  '((t (:foreground "Peru")))
  "face for common file names"
  :group 'dired-filetype-face)

(defface dired-filetype-xml
  '((t (:foreground "Chocolate")))
  "face for xml type files"
  :group 'dired-filetype-face)

;;SlateBlue

(defface dired-filetype-compress
  '((t (:foreground "Orchid")))
  "face for compressed files "
  :group 'dired-filetype-face)

(defface dired-filetype-source
  '((t (:foreground "SpringGreen")))
  "face for source code files "
  :group 'dired-filetype-face)

(defface dired-filetype-execute
  '((((class color)) :foreground "green"))
  "face for executable files "
  :group 'dired-filetype-face)

(defface dired-filetype-music
  '((t (:foreground "SteelBlue")))
  "face for music files "
  :group 'dired-filetype-face)

(defface dired-filetype-video
  '((t (:foreground "SandyBrown")))
  "face for video files "
  :group 'dired-filetype-face)

(defface dired-filetype-image
  '((t (:foreground "MediumPurple")))
  "face for pictures."
  :group 'dired-filetype-face)

(defface dired-filetype-lnk
  '((((class color) (background dark)) :foreground "yellow" :background "forest green") (t ()))
  "face for lnk files "
  :group 'dired-filetype-face)

(defun dired-filetype-set-document-face ()
  "dired-filetype-face for rich documents:"
  (font-lock-add-keywords
   nil '(("^  .*\\.\\(pdf\\|chm\\|CHM\\|tex\\|doc\\|docx\\|xls\\|xlsx\\|ppt\\|pptx\\|odt\\|ott\\|rtf\\|sdw\\|ods\\|sxc\\|odp\\|otp\\|sdx\\|kdh\\|shx\\)$"
          (".+"
           (dired-move-to-filename)
           nil
           (0 'dired-filetype-document)))))
  )

(defun dired-filetype-set-plain-face ()
  "dired-filetype-face for plain text:"
  (font-lock-add-keywords
   nil '(("^  .*\\.\\(TXT\\|txt\\|Txt\\|ini\\|INI\\|lrc\\|org\\|log\\|conf\\|CFG\\|cfg\\|properties\\|config\\|diff\\|patch\\|ebuild\\|inf\\|cnf\\|example\\|sample\\|default\\|m4\\)$"
          (".+"
           (dired-move-to-filename)
           nil
           (0 'dired-filetype-plain )))))
  )

(defun dired-filetype-set-common-face ()
  "dired-filetype-face for common files :"
  (font-lock-add-keywords
   nil '(( "^  .*\\(configure\\|INSTALL\\|README\\|readme\\|COPYING\\|CHANGES\\|LICENSE\\|ChangeLog\\|Makefile.in\\|MANIFEST.MF\\|NOTICE.txt\\|build.xml\\|Manifest\\|metadata.xml\\|install-sh\\|NEWS\\|HACKING\\|AUTHORS\\|todo\\|Todo\\|TODO\\|makefile\\)$"
          (".+"
           (dired-move-to-filename)
           nil
           (0 'dired-filetype-common)))))
  )


(defun dired-filetype-set-xml-face ()
  "dired-filetype-face for xml:"
  (font-lock-add-keywords
   nil '(("^  .*\\.\\(html?\\|HTML?\\|xml\\|XML\\|xsl\\|xsd\\|rng\\|dtd\\|mht\\|jsp\\|asp\\|js\\)$"
          (".+"
           (dired-move-to-filename)
           nil
           (0 'dired-filetype-xml)))))
  )

(defun dired-filetype-set-compress-face ()
  "dired-filetype-face for compressed files:"
  (font-lock-add-keywords
   nil '(("^  .*\\.\\(tar\\|zip\\|ZIP\\|rar\\|RAR\\|tgz\\|gz\\|bzip2\\|bz2\\|7z\\|7Z\\|Z\\|z\\|xz\\|XZ\\|rpm\\|deb\\|lzma\\|cab\\|gzip\\|taz\\|wim\\|iso\\|tbz2\\|xar\\|XAR\\|jar\\|war\\|img\\)$"
          (".+"
           (dired-move-to-filename)
           nil
           (0 'dired-filetype-compress)))))
  )

(defun dired-filetype-set-source-face ()
  "dired-filetype-face for source code files :"
  (font-lock-add-keywords
   nil '(("^  .*\\.\\(c\\|cpp\\|java\\|JAVA\\|C\\|php\\|h\\|rb\\|pl\\|css\\|el\\|lua\\)$"
          (".+"
           (dired-move-to-filename)
           nil
           (0 'dired-filetype-source)))))
  )

(defun dired-filetype-set-omit-face ()
  "dired-filetype-face for files can be omitted:"
  (font-lock-add-keywords
   nil '(("^  .*\\.\\(elc\\|class\\|so\\|ko\\|la\\|o\\|al\\|ix\\|db\\|td\\|\\|dat\\|dll\\|Dll\\|DLL\\|sav\\|rdp\\|sys\\|SYS\\|prf\\|tlb\\|cat\\|bak\\)$"
          (".+"
           (dired-move-to-filename)
           nil
           (0 'dired-filetype-omit)))))
  )

(defun dired-filetype-set-omit2-face ()
  "dired-filetype-face for backup and cache files:"
  (font-lock-add-keywords
   nil '(( "^  .*\\(\\.git\\|\\.svn\\|~\\|#\\|%\\|\\.tmp\\|$DATA\\|:encryptable\\|\\.db_encryptable\\)$"
          (".+"
           (dired-move-to-filename)
           nil
           (0 'dired-filetype-omit)))))
  )

(defun dired-filetype-set-omit3-face ()
  "dired-filetype-face for hidden files:"
  (font-lock-add-keywords
   nil '(( " \\.\\(.*$\\)"
           (".+"
            (dired-move-to-filename)
            nil
            (0 'dired-filetype-omit)))))
  )



(defun dired-filetype-set-exe-face ()
  "dired-filetype-face for executable files:"
  (font-lock-add-keywords
   nil '(("^  .*\\.\\(exe\\|EXE\\|bat\\|BAT\\|msi\\|MSI\\|\\|sh\\|run\\|reg\\|REG\\|com\\|COM\\|\\|vbx\\|VBX\\|bin\\|xpi\\|bundle\\)$"
          (".+"
           (dired-move-to-filename)
           nil
           (0 'dired-filetype-execute)))))
  )

(defun dired-filetype-set-music-face ()
  "dired-filetype-faces for audio files:"
  (font-lock-add-keywords
   nil '(("^  .*\\.\\(mp3\\|MP3\\|wma\\|WMA\\|wav\\|WAV\\|mid\\|MID\\)$"
          (".+"
           (dired-move-to-filename)
           nil
           (0 'dired-filetype-music )))))
  )
(defun dired-filetype-set-video-face ()
  "dired-filetype-face for video files"
  (font-lock-add-keywords
   nil '(("^  .*\\.\\(flv\\|avi\\|AVI\\|mkv\\|rmvb\\|RMVB\\|mpeg\\|mpg\\|MPG\\|rm\\|RM\\|mp4\\|wmv\\|m4v\\|mov\\|ogg\\|ogv\\|3gp\\|f4v\\|swf\\)$"
          (".+"
           (dired-move-to-filename)
           nil
           (0 'dired-filetype-video)))))
  )

(defun dired-filetype-set-image-face ()
  "dired-filetype-face for images."
  (font-lock-add-keywords
   nil '(("^  .*\\.\\(gif\\|GIF\\|jpg\\|JPG\\|bmp\\|BMP\\|jpeg?\\|JPEG?\\|png\\|PNG\\|xpm\\|svg\\)$"
          (".+"
           (dired-move-to-filename)
           nil
           (0 'dired-filetype-image)))))
  )

(defun dired-filetype-set-lnk-face ()
  "dired-filetype-face for lnk files"
  (font-lock-add-keywords
   nil '(("^  .*\\.\\(lnk\\|LNK\\|desktop\\|torrent\\|url\\|URL\\)$"
          (".+"
           (dired-move-to-filename)
           nil
           (0 'dired-filetype-lnk)))))
  )
;;;###autoload
(defun dired-mode-hook-func()
  "this function will be added to `dired-mode-hook'"
  (dired-filetype-set-document-face)
  (dired-filetype-set-plain-face)
  (dired-filetype-set-common-face)
  (dired-filetype-set-exe-face)
  (dired-filetype-set-omit-face)
  (dired-filetype-set-omit2-face)
  (dired-filetype-set-omit3-face)
  (dired-filetype-set-source-face)
  (dired-filetype-set-compress-face)
  (dired-filetype-set-music-face)
  (dired-filetype-set-video-face)
  (dired-filetype-set-image-face)
  (dired-filetype-set-xml-face)
  (dired-filetype-set-lnk-face)
  )

(add-hook 'dired-mode-hook 'dired-mode-hook-func)

(provide 'dired-filetype-face)

;;; `dired-filetype-face.el'ends here.
