;;; volctrl.el --- Elisp interface to controling audio output 
;;  intensity (volume) on Win32.

;; Copyright (C) 2003  Free Software Foundation, Inc.

;; Author: Anand B Pillai <abpillai at lycos dot com>
;; Keywords: tools, convenience, processes, multimedia, hardware, lisp

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; NOTE: This plugin needs the utility "volctl32.exe" which should
;;       work on all Win32 platforms. You can download this file
;;       at http://members.lycos.co.uk/anandpillai/emacs/volctrl32.exe .
;;       Get this utility from the above url and put it inside the
;;       "bin" directory of your emacs installation.

;; 

;;; Code:

(defgroup volctrl nil
  " Volctrl is an elisp plugin for controlling your system's
   audio master output from inside emacs."
  :tag "Emacs volume controller"
  :version "1.0"
  :group 'tools)

(defcustom volctrl-exe "volctrl32"
  " Volume control utility for Win32 platform "
  :type 'string
  :group 'volctrl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Increase system's sound output intensity ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun volctrl-vol-inc()
  (interactive)
  (call-process volctrl-exe nil "*Messages*" " " "/E"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Decrease system's sound output intensity ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun volctrl-vol-dec()
  (interactive)
  (call-process volctrl-exe nil "*Messages*" " " "/D"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mute system's sound output ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun volctrl-vol-mute()
  (interactive)
  (call-process volctrl-exe nil "*Messages*" " " "/M"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Maximize system's sound output ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun volctrl-vol-peak()
  (interactive)
  (call-process volctrl-exe nil "*Messages*" " " "/H"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loop volume intensity ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun volctrl-vol-loop()
  (interactive)
  (call-process volctrl-exe nil "*Messages*" " " "/L"))


(provide 'volctrl)
;;; volctrl.el ends here
