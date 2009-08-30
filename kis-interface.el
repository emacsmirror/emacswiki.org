;;; kis-interface.el --- Extensions for kis-mode.
;;
;; Filename: kis-interface.el
;; ------------------------------------------------------------------------------
;; $Author: ffrances $
;; $Date: 2006/02/14 00:11:48 $
;; $Revision: 1.7 $
;; $Id: kis-interface.el,v 1.7 2006/02/14 00:11:48 ffrances Exp $
;; ------------------------------------------------------------------------------
;; 
;; This is not part of Gnu emacs.
;; This is part of kis-mode.
;;
;;  Copyright (C) 2005  Frederic Frances (frances _ frederic at yahoo dot fr)
;;
;;  This program is free software; you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation; either version 2 of the License, or
;;  (at your option) any later version.
;;
;;  This program is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.
;;
;;  You should have received a copy of the GNU General Public License
;;  along with this program; see the file COPYING . If not, write to the
;;  Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;; ------------------------------------------------------------------------------
;;
;; provide kis-interface
;;
;; kis-mode was developped on Gnu Emacs (21.3) it probably need some adaptation
;; for others version of emacs (xemacs,...)
;;
;; This file contains interface parts of kis-mode.
;;   + it provide the folowing interactive function for design center:
;;   |- kis-interface-start-design-center      : swdc -c start
;;   |- kis-interface-stop-design-center       : swdc -c stop 
;;   |- kis-interface-forcestop-design-center  : swdc -c forcestop
;;   |- kis-interface-clobber-design-center    : swdc -c clobber   
;;   |- kis-interface-status-design-center     : swdc -c status
;;   |
;;   |-kis-interface-audit (oscfile)           : swbuild -a oscfile
;;   \-kis-interface-build (oscfile)           : swbuild    oscfile
;;
;;  + it provide the folowing interactive function for custom command on 
;;  | (remote) kabira node (need kis-project)
;;  |- kis-interface-do-project-action (action): send some remote command.
;;  \- kis-interface-create-frame              : friendly mouse click interface
;; 
;; ------------------------------------------------------------------------------
;;
;; TO DO for this file (if I've time):
;; - Auto detection of remote/local node (with getenv SW_HOME)
;; - Action with swsrv (start|stop|forcestop|status|clobber)
;; - Use tramp instead of shell-command with rsh 
;; - Manage more than one node (eventually)
;; - stop using cus-edit widget face.
;; - auto-size frame and allow some interface change (color for example)
;; - Correct bugs (If any and if I found them, and have time to correct them).
;;
;; ------------------------------------------------------------------------------
;;
;; $Log: kis-interface.el,v $
;; Revision 1.7  2006/02/14 00:11:48  ffrances
;; Add comment about emacs/xemacs.
;;
;; Revision 1.6  2006/02/13 23:42:51  ffrances
;; translation from french to english for comments.
;;
;; Revision 1.5  2006/02/13 22:46:06  ffrances
;; Ajout d'un header.
;; Add an header.
;;
;; ------------------------------------------------------------------------------

(message "kis-interface message: Loading kis-interface")

(require 'dired)       ;; for cd
(require 'cus-edit)    ;; for customize command and button face
(require 'widget)      ;; for widget
(eval-when-compile
  (require 'wid-edit)) ;; for widget

(require 'kis-project) ;; for remote action.

;; ------------------------------------------------------------------------------
;; Define sub group kis-interface
;; ------------------------------------------------------------------------------
(defgroup kis-interface nil
  "Sub group of `kis-mode' for remote actions"
  :group 'kis-mode)

;; ------------------------------------------------------------------------------
;; Some useful string function
;; ------------------------------------------------------------------------------
(defun kis-interface-clean-string (STRING)
  "Replace all occurence of [ \\t\\n]+ with \" \" in STRING and return the result
if STRING is not a string return nil"
  (cond ((stringp STRING)
	 (replace-regexp-in-string  
	  "[ \t\n]+$" ""
	  (replace-regexp-in-string  
	   "^[ \t\n]+" ""
	   (replace-regexp-in-string 
	    "[ \n\t]+" " " STRING))))
	(t 
	 nil)))

;; ------------------------------------------------------------------------------
;; Trace mode
;; ------------------------------------------------------------------------------
(defcustom kis-interface-trace-mode
  t
  "Enable (t) / Disable (nil) some trace message"
  :type 'boolean
  :group 'kis-interface)

;; simple trace function
(defun kis-interface-trace (kis-interface-message)
  "Trace function for kis-interface"
  (cond (kis-interface-trace-mode
	 (message 
	  (let ((Smessage     "")
		(lMessage))
	    (cond ((listp kis-interface-message)
		   (setq lMessage (cdr kis-interface-message))
		   (setq Smessage 
			 (mapcar 
			  '(lambda (x) 
			     (concat x))
			  lMessage))
		   ;; Formated list (first argument = name)
		   (format  "kis-interface message: %s = %s"		   
			    (car  kis-interface-message) Smessage))	  
		  ;; Simple string format
		  ((stringp kis-interface-message)
		   (format  "kis-interface message: %s" 
			    kis-interface-message))))))))

;; Test of trace function
(cond (kis-interface-trace-mode
       (kis-interface-trace "Trace Mode is active")
       (kis-interface-trace (list "Trace " "Mode " "can " "use " "list")))
      (t
       (message "kis-interface message: (Trace Mode is inactive)")))
      
;; ------------------------------------------------------------------------------
;; Parameters for remote connection
;; ------------------------------------------------------------------------------
(defcustom kis-interface-host
  "localhost"
  "Remote hostname (or ip) where kabira environment is installed
Assume kabira environement is installed on local computer when \"localhost\" is specified.
this parameter is ignored if `kis-interface-host' is set to localhost"
  :type 'string
  :group 'kis-interface)

(defcustom kis-interface-user
  ""
  "Remote login for `kis-interface-host'
this parameter is ignored if `kis-interface-host' is set to localhost"
  :type 'string
  :group 'kis-interface)

(defcustom kis-interface-net-connection-command
  "rsh"
  "Shell command to use for connection with `kis-interface-host'
this parameter is ignored if `kis-interface-host' is set to localhost"
  :type 'string
  :group 'kis-interface)

(defcustom kis-interface-first-cmd
  ". ~/.profile > /dev/null && echo \"Start of treatment at \" `date -u`"
  "Invoke command before doing action (like environment setup)

known problem:
when using rsh on emacs for windows version : 
       - insert \" at the begining of this line

when using rsh on emacs for cygwin  version :
       - use \\$ instead of $ for environement variables"
  :type 'string
  :group 'kis-interface)

(defcustom kis-interface-last-cmd
  "echo \"End of treatment at \"`date -u`"
  "Invoke some command after doing action
  
known problem:

when using rsh on emacs for windows version : 
       - insert \" at the end of this line

when using rsh on emacs for cygwin  version :
      - use \\$ instead of $ for environement variables"
  :type 'string
  :group 'kis-interface)

(defun kis-interface-start-command ()
  "Primitive to execute command on `kis-interface-host'."
  (if (string-equal "localhost" kis-interface-host)
      (concat kis-interface-first-cmd " ")
    (concat  kis-interface-net-connection-command " "
	     kis-interface-host  " "
	     kis-interface-user " "
	     kis-interface-first-cmd " ")))

(kis-interface-trace "Read parameters for remote connection OK")
;; ------------------------------------------------------------------------------
;; Parameters for design center.
;; ------------------------------------------------------------------------------

(defcustom kis-interface-design-center-directory
  "~/design" 
  "Remote directory for design center"
  :type 'string
  :group 'kis-interface)

(defcustom kis-interface-start-design-center-cmd
  "swdc -c start"
  "Design center start command"
  :type 'string
  :group 'kis-interface)

(defcustom kis-interface-stop-design-center-cmd
  "swdc -c stop"
  "Design center stop command"
  :type 'string
  :group 'kis-interface)

(defcustom kis-interface-forcestop-design-center-cmd
  "swdc -c forcestop"
  "Design center forcestop command"
  :type 'string
  :group 'kis-interface)

(defcustom kis-interface-clobber-design-center-cmd
  "swdc -c clobber"
  "Design center clobber command"
  :type 'string
  :group 'kis-interface)

(defcustom kis-interface-status-design-center-cmd
  "swdc -c status"
  "Design center status command"
  :type 'string
  :group 'kis-interface)

(defcustom kis-interface-audit-cmd
  "swbuild -a"
  "Design center audit command"
  :type 'string
  :group 'kis-interface)

(defcustom kis-interface-build-cmd
  "swbuild"
  "Design center build command"
  :type 'string
  :group 'kis-interface)

(kis-interface-trace "Read parameters for design center OK")

;; ------------------------------------------------------------------------------
;; Design center interaction.
;; ------------------------------------------------------------------------------
(defun kis-interface-cd (directory)
  "Go to directory if directory is stringp"
  (cond ((stringp directory)
	 (cd directory))))

(defun kis-interface-start-design-center ()
  "Invoke `kis-interface-start-design-center-cmd' on `kis-interface-host'"
  (interactive)
  (let ((kis-old-default-dir default-directory)
	(kis-shell-cmd (concat (kis-interface-start-command)
			       " && cd "
			       kis-interface-design-center-directory
			       " && "
			       kis-interface-start-design-center-cmd " && "
			       kis-interface-last-cmd
			       " &")))
    (kis-interface-cd (getenv "TMP"))
    (kis-interface-trace kis-shell-cmd)
    (shell-command kis-shell-cmd
		   "* kis-interface.log:command on design center *"
		   "* kis-interface.log:command on design center *")
    (kis-interface-cd kis-old-default-dir)))

(defun kis-interface-stop-design-center ()
  "Invoke `kis-interface-stop-design-center-cmd' on `kis-interface-host'"
  (interactive)
  (let ((kis-old-default-dir default-directory)
	(kis-shell-cmd (concat (kis-interface-start-command)
			   " && cd "
			   kis-interface-design-center-directory
			   " && "
			   kis-interface-stop-design-center-cmd " && "
			   kis-interface-last-cmd
			   " &")))
    (kis-interface-cd (getenv "TMP"))
    (kis-interface-trace kis-shell-cmd)
    (shell-command kis-shell-cmd
		   "* kis-interface.log:command on design center *"
		   "* kis-interface.log:command on design center *")
    (kis-interface-cd kis-old-default-dir)))
  
(defun kis-interface-forcestop-design-center ()
  "Invoke `kis-interface-forcestop-design-center-cmd' on `kis-interface-host'"
  (interactive)

  (let ((kis-old-default-dir default-directory)
	(kis-shell-cmd (concat (kis-interface-start-command)
		   " && cd "
		   kis-interface-design-center-directory
		   " && "
		   kis-interface-forcestop-design-center-cmd " && "
		   kis-interface-last-cmd
		   " &")))
    (kis-interface-cd (getenv "TMP"))
    (kis-interface-trace kis-shell-cmd)
    (shell-command kis-shell-cmd
		   "* kis-interface.log:command on design center *"
		   "* kis-interface.log:command on design center *")
    (kis-interface-cd kis-old-default-dir)))
  
(defun kis-interface-clobber-design-center ()
  "Invoke `kis-interface-clobber-design-center-cmd' on `kis-interface-host'"
  (interactive)
  (let ((kis-old-default-dir default-directory)
	(kis-shell-cmd (concat (kis-interface-start-command)
			   " && cd "
			   kis-interface-design-center-directory
			   " && "
			   kis-interface-clobber-design-center-cmd
			   " && "
			   kis-interface-last-cmd
			   " &")))
    (kis-interface-cd (getenv "TMP"))
    (kis-interface-trace kis-shell-cmd)
    (shell-command kis-shell-cmd
		   "* kis-interface.log:command on design center *"
		   "* kis-interface.log:command on design center *")
    (kis-interface-cd kis-old-default-dir)))

(defun kis-interface-status-design-center ()
  "Invoke `kis-interface-status-design-center-cmd' on `kis-interface-host'"
  (interactive)
  (let ((kis-old-default-dir default-directory)
	(kis-shell-cmd (concat (kis-interface-start-command)
			       " && cd "
			       kis-interface-design-center-directory
			       " && "
			       kis-interface-status-design-center-cmd
			       " && "
			       kis-interface-last-cmd
			       " &")))
    (kis-interface-cd (getenv "TMP"))
    (shell-command kis-shell-cmd
		   "* kis-interface.log:command on design center *"
		   "* kis-interface.log:command on design center *")
    (kis-interface-cd kis-old-default-dir)))
  
(defun kis-interface-audit (oscfile)
  "Invoke `kis-interface-audit-cmd' on `kis-interface-host'"
  (interactive "sOSC File:")
  (let ((kis-old-default-dir default-directory)
	(kis-compile-cmd (concat (kis-interface-start-command)
				 " && cd "
				 kis-interface-design-center-directory
				 " && "
				 kis-interface-audit-cmd
				 " "
				 oscfile " && "
				 kis-interface-last-cmd
				 " &")))
    (kis-interface-cd (getenv "TMP"))
    (kis-interface-trace kis-compile-cmd)
    (compile kis-compile-cmd)
    (kis-interface-cd kis-old-default-dir)))

(defun kis-interface-build (oscfile)
  "Invoke `kis-interface-build-cmd' on `kis-interface-host'"
  (interactive "sOSC File:")
  (let ((kis-old-default-dir default-directory)
	(kis-compile-cmd (concat (kis-interface-start-command)
			   " && cd "
			   kis-interface-design-center-directory
			   " && "
			   kis-interface-build-cmd
                           " "
                           oscfile " && "
			   kis-interface-last-cmd
			   " &")))
    (kis-interface-cd (getenv "TMP"))
    (kis-interface-trace kis-compile-cmd)
    (compile  kis-compile-cmd)
    (kis-interface-cd kis-old-default-dir)))

(kis-interface-trace "Read function for design center OK")

;; ------------------------------------------------------------------------------
;; Project interaction.
;; ------------------------------------------------------------------------------
(defun kis-interface-do-project-action (action)
  "Look in `kis-project-action-list' for action and perfom it on remote `kis-interface-host'
If action is not found do nothing"
  ;; Reading input parameter.
  (interactive (let ((vaction)
		     (enable-recursive-minibuffers t))
		 (setq vaction 
		       (completing-read
			(format "Action (%s):" 
				(nth 0 (nth 0 kis-project-action-list)))
			(mapcar    
			 '(lambda (kis-interface-action)
			    (kis-interface-trace 
			     (concat " Reading action name " 
				     (nth 0 kis-interface-action)))
			    (make-list 2 (nth 0 kis-interface-action)))
			 kis-project-action-list)
			t
			nil
			nil
			nil
			(nth 0 kis-interface-action)
			nil))
		 (list vaction)))

  (kis-interface-trace (concat "Action " action))

  (let ((kis-shell-cmd-begin (concat (kis-interface-start-command) "&& "))
	(kis-action action)
	(kis-old-default-dir default-directory))
    (mapcar '(lambda (x) 
	       (if (string-equal (kis-interface-clean-string kis-action) 
				 (kis-interface-clean-string (car x)))
		   (let (
                         (kis-buffer-name 
			  (concat "* kis-interface.log :" 
				  (kis-interface-clean-string (nth 0 x)) " *"))
			 (kis-shell-cmd 
			  (concat kis-shell-cmd-begin
				  (if (or  (string= "<NOCOPY>" (nth 1 x))
					   (string= "<NOCOPY>" (nth 2 x))
					   (string= "<NOCOPY>" (nth 3 x)))
				      (concat (nth 4 x) " && " )
				    (concat
				     "cp "
				     (nth 2 x) "/"
				     (nth 1 x) "  "
				     (nth 3 x) "/"
				     (nth 1 x) " && "
				     (nth 4 x) " && "
				     "sum -r " 
				     (nth 2 x) "/"
				     (nth 1 x) " "
				     (nth 3 x) "/"
				     (nth 1 x) " " 
				     " && "))
				  kis-interface-last-cmd
				  " &")))
		     (kis-interface-trace kis-shell-cmd)
		     (kis-interface-cd (getenv "TMP"))	
		     (shell-command
                      kis-shell-cmd
                      kis-buffer-name kis-buffer-name)
		     (kis-interface-cd kis-old-default-dir)
		     (switch-to-buffer
                      kis-buffer-name)
		     (delete-other-windows))))
	    kis-project-action-list)))

(kis-interface-trace "Read function for remote action OK")
;; ------------------------------------------------------------------------------
;;  Project interaction graphical interface.
;; ------------------------------------------------------------------------------
;; Frame Parameters.
(defcustom kis-interface-maximum-number-of-button-per-line
  5
  "Maximum number of push button present on a line in `kis-interface-frame'"
  :type 'integer
  :group 'kis-interface)

(defcustom kis-interface-button-width
  10
  "Width of push button present on `kis-interface-frame'"
  :type 'integer
  :group 'kis-interface)


;; Inspired from net samples
(defun kis-interface-move-and-invoke (event)
  "Move to where you click, and if it is an active field, invoke it."
  (interactive "e")
  (kis-interface-trace "kis-interface-move-and-invoke")
  (save-excursion 
    (if (widget-event-point event)
        (let* ((pos (widget-event-point event))
               (button (get-char-property pos 'button)))
          (if button
              (widget-button-click event))))))

(defun kis-interface-button-click (action)
  "Move to where you click, and if it is an active field, invoke it."
  (kis-interface-trace (concat "kis-interface-button-click " action))
  (kis-interface-do-project-action action))

(defvar kis-interface-mode-map nil
  "Keymap for frame `kis-interface-frame'.")

(unless kis-interface-mode-map
  ;; This keymap should be dense, but a dense keymap would prevent inheriting
  ;; "\r" bindings from the parent map.
  (setq kis-interface-mode-map (make-sparse-keymap))
  (set-keymap-parent kis-interface-mode-map widget-keymap)
  (suppress-keymap kis-interface-mode-map)
  (define-key kis-interface-mode-map " " 'scroll-up)
  (define-key kis-interface-mode-map "\177" 'scroll-down)
  (define-key kis-interface-mode-map "n" 'widget-forward)
  (define-key kis-interface-mode-map "p" 'widget-backward)
  (define-key kis-interface-mode-map [mouse-1] 'kis-interface-move-and-invoke))

(defun kis-interface-place-next-widget (kis-interface-widget-number)
  "Decide to insert space or new line accroding to 
`kis-interface-maximum-number-of-button-per-line'

then return the new position number indicator
"
  ;; Insert new line when kis-interface-widget-number is equal to 0 
  (if (= 0 kis-interface-widget-number)
      (widget-insert "\n")
    (widget-insert " "))
  
  ;; Increment by 1 kis-interface-widget-number and set it to 0
  ;; when it is equal to kis-interface-maximum-number-of-button-per-line
  (setq kis-interface-widget-number 
	(mod (+ 1 kis-interface-widget-number) 
	     kis-interface-maximum-number-of-button-per-line)))

(defconst kis-interface-frame-parameters
  (list (cons 'name  "kis-interface-frame")
	(cons 'left                   0)
	(cons 'top                    0)
	(cons 'width                 80)
	(cons 'height                30)
	(cons 'minibuffer           nil)
	(cons 'user-position          t)
	(cons 'vertical-scroll-bars nil)
	(cons 'scrollbar-width        0)
	(cons 'menu-bar-lines         0)
	(cons 'tool-bar-lines         0)
	(cons 'menu-bar-lines       nil)
	(cons 'tool-bar-lines       nil)
	(cons 'buffer-predicate     nil)
	(cons 'window-id              1)
	(cons 'auto-lower           nil)
	(cons 'unsplittable           t)
	(cons 'auto-raise           nil)
	(cons 'window-dedicated       t)
	(cons 'mode-line              0)
	(cons 'visibility             t)
	(cons 'focus-follows-mouse  nil)
	(cons 'foreground-color "black")
	(cons 'background-color "white")
	(cons 'visible              nil))
  "Frame parameters for `kis-interface-create-frame'")

;; Frame creation need to some cut
(defun kis-interface-create-frame ()
  "Create a frame for perform action that are in `kis-project-action-list'"
  (interactive)
  (save-excursion 

    ;; Create a new dedicated frame named kis-interface-frame.
    ;; -------------------------------------------------------------------------
    (kill-buffer (get-buffer-create "kis-interface-frame"))
    (special-display-popup-frame (get-buffer-create "kis-interface-frame")
				 kis-interface-frame-parameters)                  
    (select-frame-by-name "kis-interface-frame")

    ;; Create push button as custom mode.
    ;; -------------------------------------------------------------------------
    (make-local-variable       'widget-button-face)
    (setq widget-button-face   'custom-button-face)
    (set (make-local-variable  'widget-button-pressed-face)
                               'custom-button-pressed-face)
    (set (make-local-variable  'widget-mouse-face)
                               'custom-button-pressed-face)
    
    ;; Insert widget into frame.
    ;; -------------------------------------------------------------------------
    (let (;; Format for push button
          (kis-interface-format (format "%%-%ds" 
				     (abs kis-interface-button-width)))
          ;; Number of widget per line.
          (kis-interface-widget-number 0) 
          ;; Create action category list
          (kis-interface-category-list  
	   (mapcar '(lambda (kis-interface-category-name)
		      (kis-interface-trace 
		       (concat "Reading category name " 
			       (nth 5 kis-interface-category-name)))
		      (kis-interface-clean-string
		       (nth 5 kis-interface-category-name)))
		   kis-project-action-list))
          (kis-interface-category-list-unique)) ;; cleaned list for display
      
      ;; Insert design center action widget.
      ;; -----------------------------------------------------------------------
      (widget-insert "\nEmacs action on kabira design center:")
      
      ;; Start design center button
      (setq kis-interface-widget-number 
	    (kis-interface-place-next-widget kis-interface-widget-number))
      (widget-create 'push-button
		     :notify (lambda (&rest ignore)
			       (kis-interface-start-design-center))
		     :help-echo "Start design center"
		     (format  kis-interface-format  
			      (substring 
			       "Start Design Center" 
			       0 (min  
				  (abs kis-interface-button-width)
				  (length "Start Design Center")))))

      ;; stop design center button
      (setq kis-interface-widget-number 
	    (kis-interface-place-next-widget kis-interface-widget-number))
      (widget-create 'push-button
		     :notify (lambda (&rest ignore)
			       (kis-interface-stop-design-center))
		     :help-echo "Stop design center"
		     (format  kis-interface-format  
			      (substring 
			       "Stop Design Center"
			       0 (min  
				  (abs kis-interface-button-width)
				  (length "Stop Design Center")))))

      ;; Status design center button
      (setq kis-interface-widget-number 
	    (kis-interface-place-next-widget kis-interface-widget-number))
      (widget-create 'push-button
		     :notify (lambda (&rest ignore)
			       (kis-interface-status-design-center))
		     :help-echo "get the status of design center"
		     (format  kis-interface-format  
			      (substring 
			       "Status Design Center"
			       0 (min  
				  (abs kis-interface-button-width)
				  (length "Status Design Center")))))
      ;; Clobber design center button
      (setq kis-interface-widget-number 
	    (kis-interface-place-next-widget kis-interface-widget-number))
      (widget-create 'push-button
		     :notify (lambda (&rest ignore)
			       (kis-interface-clobber-design-center))
		     :help-echo "Clobber design center"
		     (format  kis-interface-format  
			      (substring 
			       "Clobber Design Center"
			       0 (min  
				  (abs kis-interface-button-width)
				  (length "Clobber Design Center")))))
      (widget-insert "\n\n")

      ;; Insert Custom Action list widget.
      ;; -----------------------------------------------------------------------
      ;; Sort action category list to have unique name
      (setq kis-interface-category-list-unique 
	    (make-list 1
		       (car kis-interface-category-list)))
      (mapcar '(lambda (x)
		 (cond ((eq nil (member x kis-interface-category-list-unique))
			(setq kis-interface-category-list-unique 
			      (cons x kis-interface-category-list-unique)))))
	      kis-interface-category-list)
      (kis-interface-trace 
       (cons "kis-interface-category-list-unique is" 
	     kis-interface-category-list-unique))

      ;; Select all category.
      (mapcar '(lambda (kis-interface-category-name)
		 
		 (kis-interface-trace (format "Insert widget for category %s" 
					      kis-interface-category-name))
	      
		 ;; Insert Category name.
		 (widget-insert (kis-interface-clean-string
				 kis-interface-category-name))
				 
		 ;; Reset number of widget per line counter.
		 (setq kis-interface-widget-number 0)
		 
		 ;; select all action
		 (mapcar '(lambda (kis-interface-action)
			    ;; if action category match 
			    ;; kis-interface-category-name
			    ;; then insert a push button widget.
			    (if (string-equal 
				 (kis-interface-clean-string
				  (nth 5 kis-interface-action))
				 (kis-interface-clean-string
				  kis-interface-category-name))
				
				(let ((action  (car kis-interface-action))
				      (comment (nth 6 kis-interface-action)))
				  
				  (setq kis-interface-widget-number 
					(kis-interface-place-next-widget
					 kis-interface-widget-number))
				  ;; Create the push button 
				  ;; with comment for help.
				  ;; and kis-interface-button-click for notify
				  (kis-interface-trace 
				   (format "Try to create widget %s" action))

				  (widget-create 
				   'push-button
				   :notify (lambda (widget &rest ignore)
					     (kis-interface-button-click 
					      (widget-value widget)))
				   :help-echo comment
				   :tag 
				   (format   
				    kis-interface-format 
				    (kis-interface-clean-string
				     (substring 
				      action 0 
				      (min (abs kis-interface-button-width)
					   (length action)))))
				   :format "%[[%t]%]"
				   :value action)))
			    ) ;; '(lambda (kis-interface-action)
			 kis-project-action-list)
		 (widget-insert "\n\n")
		 );; '(lambda (kis-interface-category-name)
	      kis-interface-category-list-unique)

      ;; Some usefull button like customize/reload/exit.
      ;; -----------------------------------------------------------------------
      (widget-insert "Kis Interface Frame Menu:")
      ;; Reset number of widget per line counter.
      (setq kis-interface-widget-number 0)
      
      ;; Customize button
      (kis-interface-trace 
       (format "Try to create widget Customise"))
      
      (setq kis-interface-widget-number 
	    (kis-interface-place-next-widget
	     kis-interface-widget-number))
      (widget-create 'push-button
		     :notify (lambda (&rest ignore)
			       (customize-variable 'kis-project-action-list))
		     :help-echo "Customize button for this frame"
		     (format  kis-interface-format  
			      (substring 
			       "Customize"
			       0 (min  
				  (abs kis-interface-button-width)
				  (length "Customize")))))
      ;; Reload Button
      (kis-interface-trace 
       (format "Try to create widget Relaod"))
      (setq kis-interface-widget-number 
	    (kis-interface-place-next-widget
	     kis-interface-widget-number))
      (widget-create 'push-button
		     :notify (lambda (&rest ignore)
			       (kis-interface-create-frame))
		     :help-echo "Reload This Frame"
		     (format  kis-interface-format  
			      (substring 
			       "Reload"
			       0 (min  
				  (abs kis-interface-button-width)
				  (length "Reload")))))
      ;; Quit Button
      (kis-interface-trace 
       (format "Try to create widget Quit" ))
      (setq kis-interface-widget-number 
	    (kis-interface-place-next-widget
	     kis-interface-widget-number))
      (widget-create 'push-button
		     :notify (lambda (&rest ignore)
			       (kill-buffer
				(get-buffer-create "kis-interface-frame")))
		     :help-echo "Quit This Frame"
		     (format  kis-interface-format  
			      (substring 
			       "Exit"
			       0 (min  
				  (abs kis-interface-button-width)
				  (length "Exit")))))
      (setq kis-interface-widget-number 
	    (kis-interface-place-next-widget
	     kis-interface-widget-number)))
    ;; Set the keymap frame
    (use-local-map kis-interface-mode-map)
    (widget-setup)
    ;; Go to the beginning of buffer.
    (beginning-of-buffer)))

(kis-interface-trace "Read function for graphical frame OK")
;; ------------------------------------------------------------------------------
;; For loading this library.
;; ------------------------------------------------------------------------------
(kis-interface-trace "Provide kis-interface")
(provide 'kis-interface)
;; ------------------------------------------------------------------------------
;; End Of file.
;; ------------------------------------------------------------------------------
