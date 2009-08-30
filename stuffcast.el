;;; stuffcast --- Stuff a PMP with content

;; Copyright (C) 2007 Yoni Rabkin
;;
;; Author: Yoni Rabkin <yonirabkin@member.fsf.org>
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of
;; the License, or (at your option) any later version.
;;     
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;     
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary:
;;
;; (eq PMP Portable Media Player)
;;
;; stuffcast.el is a script to wipe all the previous content from my
;; PMP and then load only the newest stuff back on.
;;
;; It uses `gnupod' for at the backend to manipulate an Ipod Shuffle.
;;
;; Run M-x stuffcast to make it happen.

;;; History:
;; In the beginning there was TECO...

;;; Code:

(defvar stuffcast-pmp-pre-command nil
  "Command to be executed before accessing the PMP.")

(defvar stuffcast-pmp-wipe-command '("gnupod_search" "-m" "/media/IPOD/" "--delete")
  "Command to wipe the PMP clean of previous content.")

(defvar stuffcast-pmp-load-track-command '("gnupod_addsong" "-m" "/media/IPOD/")
  "Arguments to load a single track to the PMP.")

(defvar stuffcast-pmp-post-command '("mktunes" "-m" "/media/IPOD/")
  "Command to be executed after accessing the PMP.")

(defvar stuffcast-source-file-directory "~/audio/talk/"
  "Pathname for the top of the content directory tree.")

(defvar stuffcast-pmp-compatible-file-list '("mp3")
  "A list of formats your PMP can play.")

(defmacro with-message (string &rest body)
  "Convenience macro for being verbose.

Argument STRING message to the user.
Optional argument BODY forms to evaluate."
  `(let* ((before (concat ,string "..."))
	  (after (concat before "...done")))
     (message before)
     ,@body
     (message after)))

(defun stuffcast-newest-file (dir)
  "Return the newest file in DIR."
  (let ((queue (directory-files dir t (regexp-opt stuffcast-pmp-compatible-file-list)))
	(newest nil))
    (when queue (setq newest (car queue)))
    (setq queue (cdr queue))
    (while queue
      (when (file-regular-p (car queue))
	(if (time-less-p
	     (nth 4 (file-attributes newest))
	     (nth 4 (file-attributes (car queue))))
	    (setq newest (car queue))))
      (setq queue (cdr queue)))
    newest))

(defun stuffcast-do-directory-list (root)
  "Return a list of the newest files for each directory in ROOT."
  (let ((queue (directory-files root t))
	(out nil))
    (while queue
      (let ((element (car queue)))
	(when (file-directory-p element)
	  (setq out (append out (list (stuffcast-newest-file element)))))
	(setq queue (cdr queue))))
    out))

(defun stuffcast-load-file-list (fl)
  "Load each file in FL onto the PMP."
  (dolist (file fl)
    (when (and file (file-regular-p file))
      (let ((program (car stuffcast-pmp-load-track-command))
	    (arguments (cdr stuffcast-pmp-load-track-command)))
	(with-message
	 (concat "loading " file)
	 (apply 'call-process
		program
		nil
		nil
		nil
		(append arguments (list file))))))))

(defun stuffcast-do-command (command)
  "Excecute COMMAND with `call-process'."
  (when command
    (apply 'call-process
	   (car command)
	   nil
	   nil
	   nil
	   (cdr command))))

(defun stuffcast ()
  "Wipe the PMP clean and load the newest content back on."
  (interactive)

  (with-message
   "running pre command"
   (stuffcast-do-command stuffcast-pmp-pre-command))

  (with-message
   "nuking the current contents of the PMP"
   (stuffcast-do-command stuffcast-pmp-wipe-command))

  (stuffcast-load-file-list
   (stuffcast-do-directory-list stuffcast-source-file-directory))

  (with-message
   "running post command"
   (stuffcast-do-command stuffcast-pmp-post-command)))

(provide 'stuffcast)

;;; stuffcast.el ends here.
