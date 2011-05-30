;;; dired-filetype-face.el  --- faces for different filetype in dired buffer.
;;;  Time-stamp: <Joseph 2011-05-18 11:56:53>

;; Filename: dired-filetype-face.el
;; Description: set faces for different file type in dired buffer.
;; Author: Joseph <jixiuf@gmail.com>
;; Maintainer: Joseph <jixiuf@gmail.com>
;; Copyright (C) 2011~, Joseph, all rights reserved.
;; Created: 2011-04-04
;; Update: 2011/05/18 11:56:09
;; Version: 0.2.1
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
;;  `dired-filetype-omit1-regexp'
;;    regexp to match unimportanted file types
;;    default = "^  .*\\.\\(elc\\|class\\|so\\|ko\\|la\\|o\\|al\\|ix\\|db\\|td\\|\\|dat\\|dll\\|Dll\\|DLL\\|sav\\|rdp\\|sys\\|SYS\\|prf\\|tlb\\|cat\\|bak\\)$"
;;  `dired-filetype-omit2-regexp'
;;    regexp to match backup or cache filetypes.
;;    default = "^  .*\\(\\.git\\|\\.svn\\|~\\|#\\|%\\|\\.tmp\\|$DATA\\|:encryptable\\|\\.db_encryptable\\)$"
;;  `dired-filetype-omit3-regexp'
;;    regexp to match hidden files
;;    default = " \\.\\(.*$\\)"
;;  `dired-filetype-document-regexp'
;;    regexp to match rich document filetypes
;;    default = "^  .*\\.\\(pdf\\|chm\\|CHM\\|tex\\|doc\\|docx\\|xls\\|xlsx\\|ppt\\|pptx\\|odt\\|ott\\|rtf\\|sdw\\|ods\\|sxc\\|odp\\|otp\\|sdx\\|kdh\\|shx\\)$"
;;  `dired-filetype-plain-regexp'
;;    regexp to match plain text filetype
;;    default = "^  .*\\.\\(TXT\\|txt\\|Txt\\|ini\\|INI\\|lrc\\|org\\|log\\|conf\\|CFG\\|cfg\\|properties\\|config\\|diff\\|patch\\|ebuild\\|inf\\|cnf\\|example\\|sample\\|default\\|m4\\)$"
;;  `dired-filetype-common-regexp'
;;    regexp to match common files
;;    default = "^  .*\\(configure\\|INSTALL\\|README\\|readme\\|COPYING\\|CHANGES\\|LICENSE\\|ChangeLog\\|Makefile.in\\|MANIFEST.MF\\|NOTICE.txt\\|build.xml\\|Manifest\\|metadata.xml\\|install-sh\\|NEWS\\|HACKING\\|AUTHORS\\|todo\\|Todo\\|TODO\\|makefile\\)$"
;;  `dired-filetype-xml-regexp'
;;    regexp to match xml filetype
;;    default = "^  .*\\.\\(html?\\|HTML?\\|xml\\|XML\\|xsl\\|xsd\\|rng\\|dtd\\|mht\\|jsp\\|asp\\|js\\)$"
;;  `dired-filetype-compress-regexp'
;;    regexp to match compressed filetypes
;;    default = "^  .*\\.\\(tar\\|zip\\|ZIP\\|rar\\|RAR\\|tgz\\|gz\\|bzip2\\|bz2\\|7z\\|7Z\\|Z\\|z\\|xz\\|XZ\\|rpm\\|deb\\|lzma\\|cab\\|gzip\\|taz\\|wim\\|iso\\|tbz2\\|xar\\|XAR\\|jar\\|war\\|img\\)$"
;;  `dired-filetype-source-regexp'
;;    regexp to match source code filetypes
;;    default = "^  .*\\.\\(c\\|cpp\\|java\\|JAVA\\|C\\|php\\|h\\|rb\\|pl\\|css\\|el\\|lua\\)$"
;;  `dired-filetype-execute-regexp'
;;    regexp to match executable filetypes
;;    default = "^  .*\\.\\(exe\\|EXE\\|bat\\|BAT\\|msi\\|MSI\\|\\|sh\\|run\\|reg\\|REG\\|com\\|COM\\|\\|vbx\\|VBX\\|bin\\|xpi\\|bundle\\)$"
;;  `dired-filetype-music-regexp'
;;    regexp to match music files
;;    default = "^  .*\\.\\(mp3\\|MP3\\|wma\\|WMA\\|wav\\|WAV\\|mid\\|MID\\)$"
;;  `dired-filetype-video-regexp'
;;    regexp to match video filetypes
;;    default = "^  .*\\.\\(flv\\|avi\\|AVI\\|mkv\\|rmvb\\|RMVB\\|mpeg\\|mpg\\|MPG\\|rm\\|RM\\|mp4\\|wmv\\|m4v\\|mov\\|ogg\\|ogv\\|3gp\\|f4v\\|swf\\)$"
;;  `dired-filetype-image-regexp'
;;    regexp to match images filetypes
;;    default = "^  .*\\.\\(gif\\|GIF\\|jpg\\|JPG\\|bmp\\|BMP\\|jpeg?\\|JPEG?\\|png\\|PNG\\|xpm\\|svg\\)$"
;;  `dired-filetype-lnk-regexp'
;;    regexp to match lnk filetypes
;;    default = "^  .*\\.\\(lnk\\|LNK\\|desktop\\|torrent\\|url\\|URL\\)$"

;;; Code:

(require 'dired)
(require 'custom)

(defgroup dired-filetype-face nil
  "set faces for different filetype in dired buffer."
  :prefix "dired-filetype-face-"
  :group 'dired-faces)

(defface dired-filetype-omit
  '((t (:foreground "Chartreuse")))
  "face for omit files"
  :group 'dired-filetype-face )

(defcustom dired-filetype-omit1-regexp
  "^  .*\\.\\(elc\\|class\\|so\\|ko\\|la\\|o\\|al\\|ix\\|db\\|td\\|\\|dat\\|dll\\|Dll\\|DLL\\|sav\\|rdp\\|sys\\|SYS\\|prf\\|tlb\\|cat\\|bak\\)$"
  "regexp to match unimportanted file types"
  :type 'string
  :group 'dired-filetype-face)

(defcustom dired-filetype-omit2-regexp
  "^  .*\\(\\.git\\|\\.svn\\|~\\|#\\|%\\|\\.tmp\\|$DATA\\|:encryptable\\|\\.db_encryptable\\)$"
  "regexp to match backup or cache filetypes."
  :type 'string
  :group 'dired-filetype-face)

(defcustom dired-filetype-omit3-regexp
  " \\.\\(.*$\\)"
  "regexp to match hidden files"
  :type 'string
  :group 'dired-filetype-face)

(defface dired-filetype-document
  '((t (:foreground "DarkCyan")))
  "face for rich document files "
  :group 'dired-filetype-face )

(defcustom dired-filetype-document-regexp
  "^  .*\\.\\(pdf\\|chm\\|CHM\\|tex\\|doc\\|docx\\|xls\\|xlsx\\|ppt\\|pptx\\|odt\\|ott\\|rtf\\|sdw\\|ods\\|sxc\\|odp\\|otp\\|sdx\\|kdh\\|shx\\)$"
  "regexp to match rich document filetypes"
  :type 'string
  :group 'dired-filetype-face)

(defface   dired-filetype-plain
  '((t (:foreground "LightSalmon")))
  "face for plain text files "
  :group 'dired-filetype-face)

(defcustom dired-filetype-plain-regexp
  "^  .*\\.\\(TXT\\|txt\\|Txt\\|ini\\|INI\\|lrc\\|org\\|log\\|conf\\|CFG\\|cfg\\|properties\\|config\\|diff\\|patch\\|ebuild\\|inf\\|cnf\\|example\\|sample\\|default\\|m4\\)$"
  "regexp to match plain text filetype"
  :type 'string
  :group 'dired-filetype-face)

(defface dired-filetype-common
  '((t (:foreground "Peru")))
  "face for common file names"
  :group 'dired-filetype-face)

(defcustom dired-filetype-common-regexp
  "^  .*\\(configure\\|INSTALL\\|README\\|readme\\|COPYING\\|CHANGES\\|LICENSE\\|ChangeLog\\|Makefile.in\\|MANIFEST.MF\\|NOTICE.txt\\|build.xml\\|Manifest\\|metadata.xml\\|install-sh\\|NEWS\\|HACKING\\|AUTHORS\\|todo\\|Todo\\|TODO\\|makefile\\)$"
  "regexp to match common files"
  :type 'string
  :group 'dired-filetype-face)

(defface dired-filetype-xml
  '((t (:foreground "Chocolate")))
  "face for xml type files"
  :group 'dired-filetype-face)

(defcustom dired-filetype-xml-regexp
  "^  .*\\.\\(html?\\|HTML?\\|xml\\|XML\\|xsl\\|xsd\\|rng\\|dtd\\|mht\\|jsp\\|asp\\|js\\)$"
  "regexp to match xml filetype"
  :type 'string
  :group 'dired-filetype-face)
;;SlateBlue

(defface dired-filetype-compress
  '((t (:foreground "Orchid")))
  "face for compressed files "
  :group 'dired-filetype-face)

(defcustom dired-filetype-compress-regexp
  "^  .*\\.\\(tar\\|zip\\|ZIP\\|rar\\|RAR\\|tgz\\|gz\\|bzip2\\|bz2\\|7z\\|7Z\\|Z\\|z\\|xz\\|XZ\\|rpm\\|deb\\|lzma\\|cab\\|gzip\\|taz\\|wim\\|iso\\|tbz2\\|xar\\|XAR\\|jar\\|war\\|img\\)$"
  "regexp to match compressed filetypes"
  :type 'string
  :group 'dired-filetype-face)

(defface dired-filetype-source
  '((t (:foreground "SpringGreen")))
  "face for source code files "
  :group 'dired-filetype-face)

(defcustom dired-filetype-source-regexp
  "^  .*\\.\\(c\\|cpp\\|java\\|JAVA\\|C\\|php\\|h\\|rb\\|pl\\|css\\|el\\|lua\\)$"
  "regexp to match source code filetypes"
  :type 'string
  :group 'dired-filetype-face)


(defface dired-filetype-execute
  '((((class color)) :foreground "green"))
  "face for executable files "
  :group 'dired-filetype-face)

(defcustom dired-filetype-execute-regexp
  "^  .*\\.\\(exe\\|EXE\\|bat\\|BAT\\|msi\\|MSI\\|\\|sh\\|run\\|reg\\|REG\\|com\\|COM\\|\\|vbx\\|VBX\\|bin\\|xpi\\|bundle\\)$"
  "regexp to match executable filetypes"
  :type 'string
  :group 'dired-filetype-face)

(defface dired-filetype-music
  '((t (:foreground "SteelBlue")))
  "face for music files "
  :group 'dired-filetype-face)

(defcustom dired-filetype-music-regexp
  "^  .*\\.\\(mp3\\|MP3\\|wma\\|WMA\\|wav\\|WAV\\|mid\\|MID\\)$"
  "regexp to match music files"
  :type 'string
  :group 'dired-filetype-face)

(defface dired-filetype-video
  '((t (:foreground "SandyBrown")))
  "face for video files "
  :group 'dired-filetype-face)

(defcustom dired-filetype-video-regexp
  "^  .*\\.\\(flv\\|avi\\|AVI\\|mkv\\|rmvb\\|RMVB\\|mpeg\\|mpg\\|MPG\\|rm\\|RM\\|mp4\\|wmv\\|m4v\\|mov\\|ogg\\|ogv\\|3gp\\|f4v\\|swf\\)$"
  "regexp to match video filetypes"
  :type 'string
  :group 'dired-filetype-face)

(defface dired-filetype-image
  '((t (:foreground "MediumPurple")))
  "face for pictures."
  :group 'dired-filetype-face)

(defcustom dired-filetype-image-regexp
  "^  .*\\.\\(gif\\|GIF\\|jpg\\|JPG\\|bmp\\|BMP\\|jpeg?\\|JPEG?\\|png\\|PNG\\|xpm\\|svg\\)$"
  "regexp to match images filetypes"
  :type 'string
  :group 'dired-filetype-face)

(defface dired-filetype-lnk
  '((((class color) (background dark)) :foreground "yellow" :background "forest green") (t ()))
  "face for lnk files "
  :group 'dired-filetype-face)

(defcustom dired-filetype-lnk-regexp
  "^  .*\\.\\(lnk\\|LNK\\|desktop\\|torrent\\|url\\|URL\\)$"
  "regexp to match lnk filetypes"
  :type 'string
  :group 'dired-filetype-face)

;;; Custom ends here.

(defun dired-filetype-set-document-face ()
  "dired-filetype-face for rich documents:"
  (font-lock-add-keywords
   nil `((,dired-filetype-document-regexp
          (".+"
           (dired-move-to-filename)
           nil
           (0 'dired-filetype-document)))))
  )

(defun dired-filetype-set-plain-face ()
  "dired-filetype-face for plain text:"
  (font-lock-add-keywords
   nil `((,dired-filetype-plain-regexp
          (".+"
           (dired-move-to-filename)
           nil
           (0 'dired-filetype-plain )))))
  )

(defun dired-filetype-set-common-face ()
  "dired-filetype-face for common files :"
  (font-lock-add-keywords
   nil `((,dired-filetype-common-regexp
          (".+"
           (dired-move-to-filename)
           nil
           (0 'dired-filetype-common))))))

(defun dired-filetype-set-xml-face ()
  "dired-filetype-face for xml:"
  (font-lock-add-keywords
   nil `((,dired-filetype-xml-regexp
          (".+"
           (dired-move-to-filename)
           nil
           (0 'dired-filetype-xml))))))

(defun dired-filetype-set-compress-face ()
  "dired-filetype-face for compressed files:"
  (font-lock-add-keywords
   nil `((,dired-filetype-compress-regexp
          (".+"
           (dired-move-to-filename)
           nil
           (0 'dired-filetype-compress))))))

(defun dired-filetype-set-source-face ()
  "dired-filetype-face for source code files :"
  (font-lock-add-keywords
   nil `((,dired-filetype-source-regexp
          (".+"
           (dired-move-to-filename)
           nil
           (0 'dired-filetype-source))))))

(defun dired-filetype-set-omit-face ()
  "dired-filetype-face for files can be omitted:"
  (font-lock-add-keywords
   nil `((,dired-filetype-omit1-regexp
          (".+"
           (dired-move-to-filename)
           nil
           (0 'dired-filetype-omit))))))

(defun dired-filetype-set-omit2-face ()
  "dired-filetype-face for backup and cache files:"
  (font-lock-add-keywords
   nil `(( ,dired-filetype-omit2-regexp
           (".+"
            (dired-move-to-filename)
            nil
            (0 'dired-filetype-omit))))))

(defun dired-filetype-set-omit3-face ()
  "dired-filetype-face for hidden files:"
  (font-lock-add-keywords
   nil `(( ,dired-filetype-omit3-regexp
           (".+"
            (dired-move-to-filename)
            nil
            (0 'dired-filetype-omit))))))

(defun dired-filetype-set-exe-face ()
  "dired-filetype-face for executable files:"
  (font-lock-add-keywords
   nil `((,dired-filetype-execute-regexp
          (".+"
           (dired-move-to-filename)
           nil
           (0 'dired-filetype-execute))))))

(defun dired-filetype-set-music-face ()
  "dired-filetype-faces for audio files:"
  (font-lock-add-keywords
   nil `((,dired-filetype-music-regexp
          (".+"
           (dired-move-to-filename)
           nil
           (0 'dired-filetype-music ))))))

(defun dired-filetype-set-video-face ()
  "dired-filetype-face for video files"
  (font-lock-add-keywords
   nil `((,dired-filetype-video-regexp
          (".+"
           (dired-move-to-filename)
           nil
           (0 'dired-filetype-video))))))

(defun dired-filetype-set-image-face ()
  "dired-filetype-face for images."
  (font-lock-add-keywords
   nil `((,dired-filetype-image-regexp
          (".+"
           (dired-move-to-filename)
           nil
           (0 'dired-filetype-image))))))

(defun dired-filetype-set-lnk-face ()
  "dired-filetype-face for lnk files"
  (font-lock-add-keywords
   nil `((,dired-filetype-lnk-regexp
           (".+"
            (dired-move-to-filename)
            nil
            (0 'dired-filetype-lnk))))))

;;;###autoload
(defun dired-filetype-face-mode-func()
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

(add-hook 'dired-mode-hook 'dired-filetype-face-mode-func)

(defadvice  dired-toggle-read-only (after  dired-filetype-face activate)
  "set different faces for different file type."
  (dired-filetype-face-mode-func)
 )
(defadvice  wdired-exit (after dired-filetype-face activate)
  "set different faces for different file type."
  (dired-filetype-face-mode-func)
)

(provide 'dired-filetype-face)

;;; `dired-filetype-face.el'ends here.
