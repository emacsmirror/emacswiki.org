;;; w32-browser.el --- Run Windows application associated with a file.
;;
;; Filename: w32-browser.el
;; Description: Run Windows application associated with a file.
;; Author: Emacs Wiki, Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2004-2012, Drew Adams, all rights reserved.
;; Created: Thu Mar 11 13:40:52 2004
;; Version: 21.0
;; Last-Updated: Thu Aug 23 20:17:57 2012 (-0700)
;;           By: dradams
;;     Update #: 220
;; URL: http://www.emacswiki.org/emacs-en/w32-browser.el
;; Doc URL: http://emacswiki.org/emacs/MsShellExecute
;; Keywords: mouse, dired, w32, explorer
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x
;;
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Run Windows application associated with a file.
;;
;; `w32-browser' & `dired-w32-browser' are taken from the Emacs Wiki
;; (author unknown).
;;
;; I modified `w32-browser' to `find-file' if it cannot
;; `w32-shell-execute'.  I modified `dired-multiple-w32-browser' to
;; use `w32-browser-wait-time'.  I wrote `dired-mouse-w32-browser',
;; `w32explore', `dired-w32explore', and dired-mouse-w32explore.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2012/03/10 dadams
;;     dired-w32-browse(-reuse-dir-buffer), w32explore:
;;       Use subst-char-in-string, not dired-replace-in-string or substitute.
;; 2010/01/21 dadams
;;     Added: dired(-mouse)-w32-browser-reuse-dir-buffer.
;; 2010/01/12 dadams
;;     dired-mouse-w32-browser, dired-mouse-w32explore:
;;       save-excursion + set-buffer -> with-current-buffer.
;; 2008/09/22 dadams
;;     dired(-mouse)-w32(-browser|explore): Use t as 2nd arg for dired-get-filename.
;; 2008/07/18 dadams
;;     Added: (dired-(mouse-))w32explore.
;;     No longer require dired+.el - do it the other way around.
;;     No longer make any bindings here.  Do it only in dired+.el.
;;     Made w32-browser interactive.
;; 2006/01/02 dadams
;;     Added: w32-browser-wait-time, soft require of dired+.el.
;;     Uncommented and updated dired-multiple-w32-browser and its binding.
;;     Thanks to Mathias Dahl [brakjoller@gmail.com] for recognizing this actually works.
;;     Conditionalized dired+ vs standard dired in bindings.
;; 2005/11/05 dadams
;;     Renamed menu-bar-dired-immediate-menu to diredp-menu-bar-immediate-menu.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;

(when (eq system-type 'windows-nt)

  (defcustom w32-browser-wait-time 0.1
    "*Delay to wait between `w32-browser' in `dired-multiple-w32-browser'.
On at least some Windows systems, this delay is needed between calls
to `w32-browser' within command `dired-multiple-w32-browser'.
Depending on your system, you might be able to set this to 0, meaning
no wait."
    :type 'integer :group 'convenience)

  (defun w32-browser (file)
    "Run default Windows application associated with FILE.
If no associated application, then `find-file' FILE."
    (interactive "fFile: ")
    (or (condition-case nil
            (w32-shell-execute nil file) ; Use Windows file association
          (error nil))
        (find-file file)))              ; E.g. no Windows file association

  (defun dired-w32-browser ()
    "Run default Windows application associated with current line's file.
If file is a directory, then `dired-find-file' instead.
If no application is associated with file, then `find-file'."
    (interactive)
    (let ((file  (dired-get-filename nil t)))
      (if (file-directory-p file)
          (dired-find-file)
        (w32-browser (subst-char-in-string ?/ ?\\ file)))))

  (defun dired-mouse-w32-browser (event)
    "Run default Windows application associated with file under mouse.
If file is a directory or no application is associated with file, then
`find-file' instead."
    (interactive "e")
    (let (file)
      (with-current-buffer (window-buffer (posn-window (event-end event)))
        (save-excursion
          (goto-char (posn-point (event-end event)))
          (setq file (dired-get-filename nil t))))
      (select-window (posn-window (event-end event)))
      (if (file-directory-p file)
          (find-file (file-name-sans-versions file t))
        (w32-browser (file-name-sans-versions file t)))))

  (defun dired-w32-browser-reuse-dir-buffer ()
    "Like `dired-w32-browser', but reuse Dired buffers."
    (interactive)
    (let ((file  (dired-get-filename nil t)))
      (if (file-directory-p file)
          (find-alternate-file file)
        (w32-browser (subst-char-in-string ?/ ?\\ file)))))

  (defun dired-mouse-w32-browser-reuse-dir-buffer (event)
    "Like `dired-mouse-w32-browser', but reuse Dired buffers."
    (interactive "e")
    (let (file)
      (with-current-buffer (window-buffer (posn-window (event-end event)))
        (save-excursion
          (goto-char (posn-point (event-end event)))
          (setq file (dired-get-filename nil t))))
      (select-window (posn-window (event-end event)))
      (if (file-directory-p file)
          (find-alternate-file (file-name-sans-versions file t))
        (w32-browser (file-name-sans-versions file t)))))

  (defun dired-multiple-w32-browser ()
    "Run default Windows applications associated with marked files."
    (interactive)
    (let ((files (dired-get-marked-files)))
      (while files
        (w32-browser (car files))
        (sleep-for w32-browser-wait-time)
        (setq files (cdr files)))))

  (defun w32explore (file)   
    "Open Windows Explorer to FILE (a file or a folder)."
    (interactive "fFile: ")
    (let ((w32file (subst-char-in-string ?/ ?\\ (expand-file-name file))))
      (if (file-directory-p w32file)
          (w32-shell-execute "explore" w32file "/e,/select,")
        (w32-shell-execute "open" "explorer" (concat "/e,/select," w32file)))))

  (defun dired-w32explore ()   
    "Open Windows Explorer to current file or folder."
    (interactive)
    (w32explore (dired-get-filename nil t)))

  (defun dired-mouse-w32explore (event)   
    "Open Windows Explorer to file or folder under mouse."
    (interactive "e")
    (let (file)
      (with-current-buffer (window-buffer (posn-window (event-end event)))
        (save-excursion
          (goto-char (posn-point (event-end event)))
          (setq file (dired-get-filename nil t))))
      (select-window (posn-window (event-end event)))
      (w32explore file)))

;;; This doesn't work, nor do other variants.
;;; Apparently, /select can specify only one file, and only one /select can be used.
;;;   (defun dired-multiple-w32explore ()   
;;;     "Open Windows Explorer to current directory, with marked files selected."
;;;     (interactive)
;;;     (let ((files (dired-get-marked-files)))
;;;       (w32-shell-execute
;;;        "open" "explorer"
;;;        (concat "/e,/select," (mapconcat (lambda (f)
;;;                                           (substitute ?\\ ?/ (expand-file-name f)))
;;;                                         files ",")))))

;;; No longer make any bindings here.  Do it only in `dired+.el'.
;;;   (if (boundp 'diredp-menu-bar-immediate-menu) ; Use Dired+ if loaded.
;;;       (eval-after-load "dired+"
;;;         '(progn
;;;           (define-key dired-mode-map [f3] 'dired-w32-browser)
;;;           (define-key dired-mode-map [f4] 'dired-w32explore)
;;;           (define-key diredp-menu-bar-immediate-menu [dired-w32-browser]
;;;             '("Open Associated Application" . dired-w32-browser))
;;;           (define-key diredp-menu-bar-immediate-menu [dired-w32explore]
;;;             '("Open in Windows Explorer" . dired-w32explore))
;;;           (define-key dired-mode-map [mouse-2] 'dired-mouse-w32-browser)
;;;           (define-key diredp-menu-bar-operate-menu [dired-w32-browser]
;;;             '("Open Associated Applications" . dired-multiple-w32-browser))))
;;;     (eval-after-load "dired"
;;;       '(progn
;;;         (define-key dired-mode-map [f3] 'dired-w32-browser)
;;;         (define-key dired-mode-map [f4] 'dired-w32explore)
;;;         (define-key dired-mode-map [menu-bar immediate dired-w32-browser]
;;;           '("Open Associated Application" . dired-w32-browser))
;;;         (define-key diredp-menu-bar-immediate-menu [dired-w32explore]
;;;           '("Windows Explorer" . dired-w32explore))
;;;         (define-key dired-mode-map [mouse-2] 'dired-mouse-w32-browser)
;;;         (define-key dired-mode-map [menu-bar immediate dired-w32-browser]
;;;           '("Open Associated Applications" . dired-multiple-w32-browser)))))

  )

;;;;;;;;

(provide 'w32-browser)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; w32-browser.el ends here
