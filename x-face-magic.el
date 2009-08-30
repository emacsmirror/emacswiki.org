;;; x-face-magic.el -- various x-face utils

;; Author: Raymond Wiker <etorwi@eto.ericsson.se>

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2 of the License, or any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this program; if not, write to the Free Software Foundation, Inc., 59 Temple
;; Place - Suite 330, Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(defvar x-faces-file (expand-file-name "~/.x-faces")
  "*File for storing x-faces.")

(defvar x-faces-alist 
  '(("daemon" . "9UkhV`QK.99Z*sZay&{&pFrf#2X{w)mjyf3C$m\\0FUbiu8
        PtKc*yw.F.Yx7CQ_ARGr/^M\\p-6(qiTNt\"gNI7$'gK?vUMAgo-k@h+
        5A<ZUhVs2xp~S .R1-&'`P.jku28X1vk3sDRm6jbFM.@%AKgB-PDlFZE")))

(load x-faces-file t t t)

(defun x-face-insert ()
  "Insert an x-face in the current compose buffer (mail or news!)"
  (interactive)
  (let ((face (completing-read "Insert which x-face? " 
          x-faces-alist nil t)))
    (save-excursion
      (save-restriction
 (widen)
 (goto-char 0)
 (save-match-data
   (if (re-search-forward "^X-face:" nil t)
       (error "There is already an X-face in the buffer!"))
   (if (re-search-forward (concat "^" mail-header-separator "$")
     nil t)
       (progn
  (goto-char (match-beginning 0))
  (insert "X-face: " (cdr (assoc face x-faces-alist))
   "\n"))
     (error "Couldn't find end of headers!")))))))

(defun x-face-steal-internal ()
  (goto-char (point-min))
  (save-match-data
    (and (re-search-forward
   "^X-Face:\\s-+\\(\\S-+\\s-*\n\\(\\s-+\\S-+\\s-*\n\\)*\\)"
   nil t)
  (buffer-substring (match-beginning 1)
      (match-end 1)))))
  
(defun x-face-steal ()
  "Steal an x-face from a News article or a mail message."
  (interactive)
  (let (face name)
    (cond ((and (equal (buffer-name) "*Summary*")
  (get-buffer "*Article*"))
    (save-excursion
      (set-buffer (get-buffer "*Article*"))
      (save-restriction
        (widen)
        (setq face (x-face-steal-internal)))))
   ((equal major-mode 'vm-mode)
    (setq face (x-face-steal-internal))
    (or face
        (progn
   (vm-expose-hidden-headers)
   (setq face (x-face-steal-internal))
   (vm-expose-hidden-headers))))
   (t
    (error "Couldn't deduce which buffer to extract x-face from.")))
    (if face 
 (progn
   (while (not name)
     (setq name (read-string "Name for this face? "))
     (if (memq name x-faces-alist)
  (if (yes-or-no-p-maybe-dialog-box 
       "There is already a face called %s. Overwrite? ")
      (delq name x-faces-alist)
    (setq name nil))))
   (setq x-faces-alist (append x-faces-alist 
          (list (cons name face)))))
      (error "Couldn't find any x-faces!"))))

(defun x-face-save-faces ()
  "Save the contents of x-faces-alist to x-faces-file (typically
\"~/.x-faces\"."
  (interactive)
  (save-excursion
    (set-buffer 
     (find-file-noselect x-faces-file))
    (erase-buffer)
    (insert (format 
      "(setq x-faces-alist '%s)" (prin1-to-string
      x-faces-alist)))
    (save-buffer)
    (kill-buffer nil)))

(provide 'x-face-magic)
;;; x-face-magic.el ends here
