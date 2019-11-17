; -*-Emacs-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Copyright (C) 1996 Ming-Jen Chan (mjchan@cs.cmu.edu)
;
; File:         bc.el 
; RCS:          $Header: g:/home/mjchan/lib/lisp/comint-hack/RCS/bc.el,v 1.4 1997-06-16 09:38:35-04 mjchan Exp mjchan $
; Abstract:	make a shell buffer running bc
; Author:       Ming-Jen Chan (mjchan@cs.cmu.edu)
; Created:      Mon Jun 09 10:07:33 1997
; Modified:     Ming-Jen Chan (mjchan@cs.cmu.edu)
; Language:     Emacs-Lisp
; Package:      N/A
; Status:       Experimental
;
; $Log: bc.el,v $
; Revision 1.4  1997-06-16 09:38:35-04  mjchan
; added bc-cmushell alias so that HT will see no difference.
;
; Revision 1.3  1997-06-13 09:24:56-04  mjchan
; directory-sep-char is set to make sure ~/.bc is passed with correct
; path. The bc program is a UNIX program that expects forward-slash is
; used in a path.
;
; Revision 1.2  1997-06-11 12:21:16-04  mjchan
; added a frame control that tells whether a new frame will be used to
; display bc buffer. Also, if this frame has been created and will be
; re-used/popped up if bc-shell is called again.
;
; Revision 1.1  1997/06/09 14:30:30  mjchan
; Initial revision
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Verbatim copies of this file may be freely redistributed.
;;;
;;; Modified versions of this file may be redistributed provided that this
;;; notice remains unchanged, the file contains prominent notice of
;;; author and time of modifications, and redistribution of the file
;;; is not further restricted in any way.
;;;
;;; This file is distributed `as is', without warranties of any kind.
;;; See the GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar bc-shell-use-new-frame nil
  "*non-nil if bc should be running in a separate frame.")

(defvar bc-frame nil 
  "internal variable to keep track of the frame running bc.")

;;(defalias 'bc-cmushell 'bc-shell)

(defun bc-shell ()
  "create a shell buffer running bc."
  (interactive)
  (let* ((shell-name "bc")
	 (prog (or explicit-shell-file-name
		   (getenv "ESHELL")
		   (getenv "SHELL")
		   shell-file-name))
	 (name (file-name-nondirectory prog))
	 (command-switch (if (or (string-match "csh" name)
                             (string-match "bash" name))
                         "-c" "/c"))
	 (rcfile (let ((directory-sep-char ?/))
		   (expand-file-name "~/.bc")))
	 (win32-quote-process-args nil)
	 (args (if (file-exists-p rcfile)
		   (list command-switch (concat "bc -i -l " rcfile))
		 (list command-switch "bc -i -l"))))
    (if bc-shell-use-new-frame
	(progn
	  (if (not (frame-live-p bc-frame))
	      (setq bc-frame (make-frame 
			     '((minibuffer . nil) (menu-bar-lines . 0)
			       (height . 10) (width . 40)))))
	  (raise-frame bc-frame)
	  (select-frame bc-frame)))
    (set-buffer (apply 'make-comint shell-name prog nil args))
    (shell-mode)
    (switch-to-buffer (concat "*" shell-name "*"))))
