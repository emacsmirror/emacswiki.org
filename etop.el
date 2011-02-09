;;; top.el --- run "top" to display information about processes

;; Author: Tom Wurgler, Bill Benedetto <twurgler@goodyear.com>
;; Created: 1/19/98
;; Keywords: extensions processes

;; top.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; top.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This code sets up a buffer to handle running the "top" program written
;; by William LeFebvre.  "top" is avaiable at ftp.groupsys.com:/pub/top
;; When you exit "top", the sentinel kills the buffer.

;;; Change Log:

;
; $Id: etop.el,v 1.3 2007-03-26 13:52:18 jaalto Exp $
;
; $Log: etop.el,v $
; Revision 1.3  2007-03-26 13:52:18  jaalto
; up
;
; Revision 1.2  2006-11-16 21:08:04  jaalto
; up
;
; Revision 1.1  2006-11-16 20:26:39  jaalto
; up
;
; Revision 1.2  1998/01/29  12:05:44  jaalto
; - checkdoc, bc
;
; Revision 1.1  1998/01/29  12:00:19  jaalto
; Initial revision
;
; Revision 1.2  1998-01-27 12:00:04-05  t901353
; changed arg to concat to be string and not integer.
;
; Revision 1.1  1998-01-27 09:54:29-05  t901353
; Initial revision
;
;;; Code:

(eval-and-compile
  (autoload 'term-mode      "term")
  (autoload 'term-char-mode "term"))

(defvar etop-command "top"
  "*Command to run the correct 'top' version for your machine.
Use absolute path to make top run faster.")

(defvar etop-delay-seconds 5
  "*Update delay. Default is 5 seconds.")

(defvar etop-delay-option
  (if (string-match "solaris" (emacs-version))
      "-s%d"
    "-d %d")
  "*Delay option format for top(1) command. See `top-delay-seconds'")

(defvar etop-display-active nil
  "*If t, display only active processes by default.
If nil, include the inactive ones too.")

(defvar etop-number-of-processes-to-show nil
  "*Number of processes to show in the 'top' list.  If nil, fills the window.")

(defvar etop-original-buffer nil
  "The buffer being visited when 'top' was started.")

(defun etop ()
  "Run 'top' in a terminal buffer."
  (interactive)
  (setq etop-original-buffer (buffer-name))
  (if (get-buffer "*etop*")
      (kill-buffer "*etop*"))
  (let ((tnopts
         (if etop-number-of-processes-to-show
             etop-number-of-processes-to-show
           (- (window-height) 8))))
    (set-buffer (make-term "top" etop-command
                           (if etop-display-active
                               "-I"
                             nil)
                           (format etop-delay-option
                                   etop-delay-seconds)
                           (int-to-string tnopts))))
  (term-mode)
  (term-char-mode)
  (switch-to-buffer "*top*"))

(defun etop-sentinel ()
  (set-process-sentinel (get-buffer-process "*top*")
                        (function etop-clear-sentinel)))

(defun etop-clear-sentinel (proc str)
  (if (get-buffer "*top*")
      (progn
        (switch-to-buffer etop-original-buffer)
        (kill-buffer "*top*"))))

(add-hook 'term-exec-hook 'etop-sentinel)

(provide 'etop)

;;; top.el ends here
