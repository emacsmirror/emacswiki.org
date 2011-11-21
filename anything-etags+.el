;;; anything-etags+.el ---Another Etags anything.el interface

;; Description: Another Etags anything.el interface
;; Filename: anything-etags+.el
;; Created: 2011-02-23
;; Last Updated: Joseph 2011-11-21 14:15:38 星期一
;; Version: 0.1.4
;; Author: 纪秀峰(Joseph) <jixiuf@gmail.com>
;; Maintainer: Joseph <jixiuf@gmail.com>
;; Copyright (C) 2011~, Joseph, all rights reserved.
;; URL:http://www.emacswiki.org/emacs/anything-etags+.el
;;     https://github.com/jixiuf/anything-etags-plus
;; screencast:http://screencast-repos.googlecode.com/files/emacs-anything-etags-puls.mp4.bz2
;; Keywords: anything, etags
;; Compatibility: (Test on GNU Emacs 23.2.1)
;;   I am trying to make it work with XEmacs ,
;;   but I haven't tested it on XEmacs.
;;  .
;;
;; Features that might be required by this library:
;;
;; `anything' `etags'
;;
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
;; This package use `anything' as a interface to find tag with Etags.
;;
;;  it support multiple tag files.
;;  and it can recursively searches each parent directory for a file named
;;  'TAGS'. so you needn't add this special to `tags-table-list'
;;
;;  if you use GNU/Emacs ,you can set `tags-table-list' like this.
;;  (setq tags-table-list '("/java/tags/TAGS"
;;                          "/java/tags/linux.tag"
;;                          "/java/tags/tag3"))
;;
;;  if you use XEmacs ,you can set `tag-table-alist' like this.
;;  (setq tag-table-alist
;;        '(("/usr/src/public/perl/" . "/usr/src/public/perl/perl-3.0/")
;;          ("\\.el$" . "/usr/local/emacs/src/")
;;          ("/jbw/gnu/" . "/usr15/degree/stud/jbw/gnu/")
;;          ("" . "/usr/local/emacs/src/")
;;  ))
;;
;;  (global-set-key "\M-." 'anything-etags+-select-one-key)
;;       `M-.' call  anything-etags+-select-at-point
;;       `C-uM-.' call anything-etags+-select
;;   or
;; (define-key anything-command-map (kbd "e") 'anything-etags+-select-at-point)
;; (define-key anything-command-map (kbd "C-e") 'anything-etags+-select)
;;
;; anything-etags+.el also support history go back ,go forward and list tag
;; histories you have visited.(must use commands list here:)
;;  `anything-etags+-history'
;;    List all tag you have visited with `anything'.
;;  `anything-etags+-history-go-back'
;;    Go back cyclely.
;;  `anything-etags+-history-go-forward'
;;    Go Forward cyclely.
;;
;; if you want to work with `etags-table.el' ,you just need
;; add this line to to init file after loading etags-table.el
;;
;;     (add-hook 'anything-etags+-select-hook 'etags-table-recompute)
;;    (setq etags-table-alist
;;     (list
;;        '("/home/me/Projects/foo/.*\\.[ch]$" "/home/me/Projects/lib1/TAGS" "/home/me/Projects/lib2/TAGS")
;;        '("/home/me/Projects/bar/.*\\.py$" "/home/me/Projects/python/common/TAGS")
;;        '(".*\\.[ch]$" "/usr/local/include/TAGS")
;;        ))
;;
;;; Installation:
;;
;; Don't need anything-etags.el (another etags interface).
;; Just put anything-etags+.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'anything-etags+)
;;
;; No need more.
;;
;; I use GNU/Emacs,and this is my config file about etags
;; (require 'anything-etags+)
;; (setq anything-etags+-use-short-file-name nil)
;; ;;you can use  C-uM-. input symbol (default thing-at-point 'symbol)
;; (global-set-key "\M-." 'anything-etags+-select-one-key)
;; ;;list all visited tags
;; (global-set-key "\M-*" 'anything-etags+-history)
;; ;;go back directly
;; (global-set-key "\M-," 'anything-etags+-history-action-go-back)
;; ;;go forward directly
;; (global-set-key "\M-/" 'anything-etags+-history-action-go-forward)
;;
;; and how to work with etags-table.el
;; (require 'etags-table)
;; (setq etags-table-alist
;;       (list
;;        '("/home/me/Projects/foo/.*\\.[ch]$" "/home/me/Projects/lib1/TAGS" "/home/me/Projects/lib2/TAGS")
;;        '("/home/me/Projects/bar/.*\\.py$" "/home/me/Projects/python/common/TAGS")
;;        '("/tmp/.*\\.c$"  "/java/tags/linux.tag" "/tmp/TAGS" )
;;        '(".*\\.java$"  "/opt/sun-jdk-1.6.0.22/src/TAGS" )
;;        '(".*\\.[ch]$"  "/java/tags/linux.ctags")
;;        ))
;; (add-hook 'anything-etags+-select-hook 'etags-table-recompute)

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `anything-eta
