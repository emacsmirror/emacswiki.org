;;; sys.el --- Determine which kind of system Emacs is running.

;; Copyright (C) 2001, 2006 Vinicius Jose Latorre

;; Author:	Vinicius Jose Latorre <viniciusjl@ig.com.br>
;; Maintainer:	Vinicius Jose Latorre <viniciusjl@ig.com.br>
;; Keywords:	internal, maintenance, debug
;; Time-stamp:	<2006/09/14 12:06:24 vinicius>
;; Version:	1.0
;; X-URL:	http://www.emacswiki.org/cgi-bin/wiki/ViniciusJoseLatorre

;; This file is *NOT* (yet?) part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;; Sometimes you need to know on which system Emacs is running, so it's
;; possible to fine tune/adapt/parameterize your code.
;;
;; This package determines which kind of system Emacs is running by setting
;; the variables:
;;
;; `sys-cygwin-system'	Non-nil means Emacs is running on Cygwin (Windows
;;			9x/NT system).
;;
;; `sys-windows-system'	Non-nil means Emacs is running on Windows 9x/NT.
;;
;; `sys-lp-system'	Non-nil means Emacs is running on Unix which has lp as
;;			printing utility.
;;
;; `sys-emacs-type'	Specify which kind of Emacs is running.
;;
;; `sys-path-style'	Specify which path style to use for external commands.
;;
;; To use `sys', insert in your package:
;;
;;    (require 'sys)
;;
;; So, if you're fine tunning a package, the code will look like:
;;
;;    (require 'sys)
;;    (eval-and-compile
;;      ;; define `some-function' depending on Emacs kind.
;;      (cond ((eq sys-emacs-type 'emacs)
;;             ...
;;             (defun some-function (some-arg)
;;               ;; code for GNU Emacs
;;               )
;;             ...
;;             )
;;            ((eq sys-emacs-type 'xemacs)
;;             ...
;;             (defun some-function (some-arg)
;;               ;; code for XEmacs
;;               )
;;             ...
;;             ))
;;      ;; initialize `some-variable' depending on which system is running
;;      (defvar some-variable
;;        (cond (sys-cygwin-system
;;               ;; initialization for Cygwin
;;               )
;;              (sys-windows-system
;;               ;; initialization for Windows
;;               )
;;              (sys-lp-system
;;               ;; initialization for Unix using lp utility
;;              (t
;;               ;; initialization for GNU Linux/Unix (using lpr utility)
;;               ))))
;;
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; code:


(defgroup sys nil
  "System Parameters group"
  :tag "System Parameters"
  :link '(emacs-library-link :tag "Source Lisp File" "sys.el")
  :prefix "sys-"
  :group 'internal)


(defcustom sys-windows-system
  (memq system-type '(emx win32 w32 mswindows ms-dos windows-nt))
  "*Non-nil means Emacs is running on Windows 9x/NT."
  :type 'boolean
  :group 'sys)


(defcustom sys-lp-system
  (memq system-type '(usg-unix-v dgux hpux irix))
  "*Non-nil means Emacs is running on Unix which has lp as printing utility."
  :type 'boolean
  :group 'sys)


(defcustom sys-cygwin-system
  (and sys-windows-system
       (getenv "OSTYPE")
       (string-match "cygwin" (getenv "OSTYPE")))
  "*Non-nil means Emacs is running on Cygwin (Windows 9x/NT system)."
  :type 'boolean
  :group 'sys)


(defcustom sys-emacs-type
  (cond ((string-match "XEmacs" emacs-version) 'xemacs)
	((string-match "Lucid" emacs-version) 'lucid)
	((string-match "Epoch" emacs-version) 'epoch)
	(t 'emacs))
  "*Specify which kind of Emacs is running.

Valid values are:

   xemacs	It's running XEmacs.

   lucid	It's running Lucid.

   epoch	It's running Epoch.

   emacs	It's running GNU Emacs."
  :type '(choice :menu-tag "Emacs Type"
		 :tag "Emacs Type"
		 (const :tag "XEmacs" xemacs)
		 (const :tag "Lucid" lucid)
		 (const :tag "Epoch" epoch)
		 (const :tag "GNU Emacs" emacs))
  :group 'sys)


(defcustom sys-path-style
  (if (and (not sys-cygwin-system)
	   sys-windows-system)
      'windows
    'unix)
  "*Specify which path style to use for external commands.

Valid values are:

   windows	Windows 9x/NT style (\\)

   unix		Unix style (/)"
  :type '(choice :tag "Path style"
                 (const :tag "Windows 9x/NT Style (\\)" :value windows)
                 (const :tag "Unix Style (/)" :value unix))
  :group 'sys)


(provide 'sys)


;;; sys.el ends here
