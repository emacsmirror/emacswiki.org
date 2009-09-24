;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*- mode: EMACS-LISP; -*-
;;; This is mon-w32-load.el.el
;;; ================================================================
;;; DESCRIPTION:
;;; Common load defs for w32 systems and commands to launch various tasks 
;;; Some apps evan come with predefined switches.
;;;
;;; FUNCTIONS:►►►
;;; `mon-maximize-frame-w32', `mon-minimize-frame', `mon-restore-frame',
;;; `mon-menu-bar', `mon-open-explorer', `mon-open-program-files',
;;; `mon-open-abbyy', `mon-open-notepad++', `mon-open-fastone',
;;; `mon-open-photoshop', `mon-open-images-ed-swap', `mon-open-moz-down',
;;; `mon-open-workspace',
;;; FUNCTIONS:◄◄◄
;;;
;;; MACROS:
;;;
;;; CONSTANTS:
;;;
;;; VARIABLES:
;;;
;;; ALIASED/ADVISED/SUBST'D:
;;;
;;; DEPRECATED:
;;;
;;; RENAMED:
;;;
;;; MOVED: 
;;; `mon-insert-user-name-cond'   -> mon-insertion-utils.el
;;; `mon-insert-system-type-cond' -> mon-insertion-utils.el
;;;
;;; REQUIRES:
;;;
;;; TODO:
;;;
;;; NOTES:
;;;
;;; SNIPPETS:
;;;
;;; THIRD PARTY CODE:
;;;
;;; AUTHOR: MON KEY
;;; MAINTAINER: MON KEY
;;;
;;; PUBLIC-LINK: (URL `http://www.emacswiki.org/emacs/mon-w32-loads.el')
;;; FIRST-PUBLISHED: <Timestamp: #{2009-09-23T16:06:39-04:00Z}#{09393} - by MON>
;;;
;;; FILE CREATED:
;;; <Timestamp: Winter 2008 - by MON KEY>
;;; HEADER-ADDED: <Timestamp: #{2009-09-23T16:12:48-04:00Z}#{09393} - by MON KEY>
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
;;; Copyright (C) 2009 MON KEY 
;;; ==========================
;;; CODE:

;;; ==============================
;;; MODIFICATIONS: <Timestamp: #{2009-08-14T12:35:21-04:00Z}#{09335} - by MON KEY>
(cond (IS-MON-P-W32
       (require 'woman)
       (woman-manpath-add-locales
	`("c:/usr/man" "C:/usr/share/man" "/usr/local/man" "C:/usr/local/share/man"
          ,(concat (getenv "HOME") "\\Emacs\\EmacsW32\\gnuwin32\\man")
          "c:/usr/X11R6/share/man" 
          "c:/usr/ssl/man" "/usr/local/man"))))

;;; ==============================
;;; Launch various tasks (apps with predefined switches).
;;; ==============================

;;; ==============================
(defun mon-maximize-frame-w32 ()
  "Maximize the current frame \(windows only\). 
Conditional evaluation of system type.
See also; `mon-menu-bar', `mon-restore-frame' `mon-minimize-frame'."
  (interactive)
  (cond 
   (win32p  (w32-send-sys-command 61488))
   (gnu-linuxp (message "I'm a GNU - that hurts."))))
;;
(defun mon-minimize-frame ()
  "Send frame a Windows WM_SYSCOMMAND of type COMMAND - #xf020
Minimize current frame \(windows only\). Conditional evaluation of system type.
See also; `mon-menu-bar', `mon-restore-frame', `mon-minimize-frame'."
  (interactive)
  (cond 
   (win32p
    (w32-send-sys-command #xf020))
   (gnu-linuxp (message "I'm a GNU - that hurts."))))
;; 
(defun mon-restore-frame ()
  "Send frame a Windows WM_SYSCOMMAND of type COMMAND - #xf120
Restore current frame \(windows only\). Conditional evaluation of system type.
See also; `mon-menu-bar', ,`mon-minimize-frame', `mon-maximize-frame-w32'."
  (interactive)
  (cond (win32p (w32-send-sys-command #xf120))
        (gnu-linuxp (message "I'm a GNU - that hurts."))))
;;
(defun mon-menu-bar ()
  "Send frame a Windows WM_SYSCOMMAND of type COMMAND - #xf120
Restore current frame \(windows only\). Conditional on evaluation of system type.
See also; , `mon-restore-frame' `mon-minimize-frame', `mon-maximize-frame-w32'."
  (interactive)
  (cond  (win32p (w32-send-sys-command #xf100))
         (gnu-linuxp (message "I'm a GNU - that hurts."))))

;;;test-me;(mon-menu-bar)
;;;test-me;(mon-restore-frame)
;;;test-me;(mon-minimize-frame)
;;;test-me;(mon-maximize-frame-w32)

;;; ==============================
(defun mon-open-abbyy ()
  "Launch the ABBYY Finereader OCR editor.\n
See also; `mon-open-notepad++', `mon-open-fastone', `mon-open-photoshop'."
  (interactive)
  (cond (IS-MON-P-W32
         (w32-shell-execute 
          "open" 
          (concat (cadr (assoc 'the-fynrdr-pth *mon-misc-path-alist*))
                  "FineReader.exe")))
        ((or IS-BUG-P IS-BUG-P-REMOTE)
         (message "ABBYY isn't installed. Find a Crack and fix this ASAP."))
        (gnu-linuxp (message "I'm a GNU - that hurts."))))

;;;test-me; (mon-open-abbyy)

;;; ==============================
(defun mon-open-notepad++ ()
  "Launch the notepad++ editor.\n
See also;`mon-open-abbyy', `mon-open-fastone', `mon-open-photoshop'."
  (interactive)
  (cond (win32p (w32-shell-execute "open" 
                                   (concat 
                                    (cadr (assoc 'the-ntpd++-pth *mon-misc-path-alist*)) 
                                    "notepad++.exe")))
        (gnu-linuxp (message "I'm a GNU - that hurts. USE EMACS!!!"))))

;;;test-me;(mon-open-notepad++)

;;; ==============================
(defun mon-open-fastone ()
  "Launch the fastone screen capture editor.\n
See also; `mon-open-abbyy', `mon-open-notepad++',`mon-open-photoshop'."
  (interactive)
  (cond (win32p (w32-shell-execute 
                 "open" 
                 (concat 
                  (cadr (assoc 'the-fstone-pth *mon-misc-path-alist*))
                  "FSCapture.exe")))
        (gnu-linuxp (message "I'm a GNU - that hurts."))))

;;;test-me;(mon-open-fastone)
;;;test-me;(call-interactively 'mon-open-fastone)

;;; ==============================
(defun mon-open-photoshop ()
  "Launch Ad0be Ph0t0sh0p editor.\n
See also; `mon-open-abbyy', `mon-open-notepad++', `mon-open-fastone'."

  (interactive)
  (cond (win32p (w32-shell-execute "open" 
                                   (concat 
                                    (cadr (assoc 'the-ps-pth *mon-misc-path-alist*))
                                    "Photoshop.exe")))
        (gnu-linuxp (message "I'm a GNU - AND I got no WINE - that hurts."))))

;;;test-me;(mon-open-photoshop)
;;;test-me;(mon-open-photoshop )

;;; ==============================
;;; OPEN DIRECTORIES W/ EXTERNAL APP:
;;; ==============================
(defun mon-open-explorer ()
  "Launch the w32 explorer in the directory of current file.
See also; `mon-open-program-files', `mon-open-images-ed-swap',
`mon-open-moz-down', `mon-open-workspace'."
  (interactive)
  (cond (win32p (w32-shell-execute 
                 "explore" 
                 (convert-standard-filename default-directory)))
        (gnu-linuxp (message "I'm a GNU - that hurts."))))
         
;;;test-me;(mon-open-explorer)

;;; ==============================
(defun mon-open-images-ed-swap ()
  "Open the explorer window in \"Program Files\" folder.
See also; `mon-open-program-files', `mon-open-explorer', `mon-open-moz-down'
`mon-open-workspace'."
  (interactive)
  (cond (win32p (w32-shell-execute "open" "explorer"
                                   (concat "/e, " (cadr (assoc
                                                  (cond (IS-MON-P-W32 'the-mon-img-ed-swp)
                                                        (IS-BUG-P     'the-bug-img-ed-swp))
                                                  *mon-misc-path-alist*)))))
        (gnu-linuxp (message "I'm a GNU and probably not on your Samba right now..."))))

;;;test-me;(mon-open-images-ed-swap)
 
;;; ==============================
(defun mon-open-program-files ()
  "Open the w32 explorer in the a 'Program Files' folder.\n
See also; `mon-open-explorer', `mon-open-images-ed-swap',`mon-open-moz-down', 
`mon-open-workspace'."
  (interactive)
  (cond (win32p (w32-shell-execute "open" "explorer" (concat "/e, " "C:\\Program Files")))
   (gnu-linuxp  (message "I'm a GNU - that hurts."))))

;;;test-me;(mon-open-program-files )

;;; ==============================
;;; MODIFICATIONS: <Timestamp: #{2009-08-11T21:32:02-04:00Z}#{09333} - by MON KEY>
(defun mon-open-moz-down ()
  (interactive)
  "Open the explorer window of the Mozilla Downloads folder.\n
See also; `mon-open-program-files', `mon-open-explorer',`mon-open-workspace',
`mon-open-images-ed-swap'."
  (cond (win32p (w32-shell-execute 
                 "open" 
                 "explorer" 
                 (concat "/e, " (nth 4  (assoc (cond (IS-MON-P-W32 1)
                                                     (IS-BUG-P-REMOTE 4)
                                                     (IS-BUG-P 3))
                                               *mon-emacsd*)))))
        (gnu-linuxp (message "I'm a GNU - do you have a default download folder?"))))

;;;test-me;(mon-open-moz-down )

;;; ==============================
(defun mon-open-workspace ()
  "Open the explorer window in Mon_Workspaces folder.
See also; `mon-open-program-files', `mon-open-explorer',  `mon-open-moz-down'
`mon-open-images-ed-swap'."
  (interactive)
    (cond (IS-MON-P-W32
	   (w32-shell-execute  "open" "explorer"
			       (concat "/e, " 
                                       (nth 3 (assoc 1 *mon-emacsd*)))))
	  ((or IS-BUG-P IS-BUG-P-REMOTE IS-MON-P-GNU)
               (message "This directory isn't set."))))

;;;test-me;(mon-open-workspace )
;;;test-me;(call-interactively 'mon-open-workspace)

;;; ==============================
(provide 'mon-w32-load)
;;; ==============================
;;; mon-w32-load.el ends here
;;; EOF
