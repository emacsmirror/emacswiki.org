;;; outlook-org.el --- Outlook org
;; 
;; Filename: outlook-org.el
;; Description: 
;; Author: Matthew L. Fidler
;; Maintainer: 
;; Created: Mon May 10 09:44:59 2010 (-0500)
;; Version: 
;; Last-Updated: Fri May 14 15:01:55 2010 (-0500)
;;           By: Matthew L. Fidler
;;     Update #: 34
;; URL: 
;; Keywords: 
;; Compatibility: 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; Adapted from http://superuser.com/questions/71786/can-i-create-a-link-to-a-specific-email-message-in-outlook
;; Allows selecting then inserting
;;
;; org-outlook-task creates task(s) from the selected item(s).
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

(require 'org)

(defvar org-outlook-dir (file-name-directory (or
                                  load-file-name
                                  buffer-file-name)))

(defun org-outlook-task ()
  (interactive)
  (let (
        ret
        )
    (with-temp-buffer
      (insert 
       (shell-command-to-string 
        (format "cscript %s\\task.vbs"
                org-outlook-dir
                )
        )
       )
      (goto-char (point-min))
      (forward-line 2)
      (end-of-line)
      (delete-region (point-min) (point))
      (goto-char (point-min))
      (while (search-forward "ÿ" nil t)
        (replace-match ""))
      (goto-char (point-min))
      (re-search-forward "]]")
      (while (re-search-forward "^.+$" nil t)
        (fill-paragraph))
      (setq ret (buffer-substring (point-min) (point-max)))
      )
    (insert ret)
    )
  )

(defun org-outlook-copy () 
  (interactive)
  (let (
        ret
        )
    (with-temp-buffer
        (insert 
         (shell-command-to-string 
          (format "cscript %s\\guid.vbs"
                  org-outlook-dir
                  )
          )
         )
        (goto-char (point-min))
        (forward-line 2)
        (end-of-line)
        (delete-region (point-min) (point))
        (setq ret (buffer-substring (point-min) (point-max)))
        )
    (insert ret)
    )
  )

(org-add-link-type "outlook" 'org-outlook-open)

(defgroup outlook-org nil
  "Outlook Org"
  :group 'org-mode
  )

(defcustom org-outlook-location (w32-short-file-name "c:/Program Files/Microsoft Office/OFFICE12/OFFICE12/OUTLOOK.exe")
  "* Microsoft Outlook 2007 location."
  :type 'string
  :group 'outlook-org
  )

(defun org-outlook-open (id)
   "Open the Outlook item identified by ID.  ID should be an Outlook GUID."
   ; Change this to work with Outlook 2007 without changing the
   ; registry.
   (setq debug-on-error 't)
   (if (and org-outlook-location (file-exists-p org-outlook-location))
       (shell-command (concat "\"" org-outlook-location "\" /select \"outlook:" id "\"&"))
     (w32-shell-execute "open" (concat "outlook:" id))
     )
   )


(provide 'outlook-org)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; outlook-org.el ends here
