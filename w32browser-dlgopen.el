;;; w32browser-dlgopen.el --- Use w32browser with standard Windows Open File box.
;;
;; Filename: w32browser-dlgopen.el
;; Description:  Use w32browser with standard Windows Open File box.
;; Author: Binu Jose Philip, Drew Adams
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 2000-2017, Drew Adams, all rights reserved.
;; Created: Thu Dec  7 09:32:12 2000
;; Version: 0
;; Package-Requires: ()
;; Last-Updated: Sun Jan  1 11:57:59 2017 (-0800)
;;           By: dradams
;;     Update #: 192
;; URL: http://www.emacswiki.org/w32browser-dlgopen.el
;; Doc URL: http://emacswiki.org/MsShellExecute
;; Keywords: files, extensions, convenience, dialog
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x, 24.x, 25.x
;;
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Opens MS Windows standard Open file dialog box.
;;
;; The executable `getfile.exe' should be in the system path somewhere
;; or var `dlgopen-executable-path' should have executable path+name.
;;
;; Command `dlgopen-open-files' opens the Windows Open dialog box.
;;
;; If function `w32-browser' is defined, then it is used to open files
;; selected in the Windows Open dialog box.  Otherwise, standard Emacs
;; `find-file' functions are used.
;;
;; If `w32-browser' is not defined, then `dlgopen-other-window'
;; controls how selected files are opened:
;; Non-nil => display chosen file(s) in separate windows.
;; Nil => display a single chosen file in the current window; don't
;; display multiple chosen files.  (The effect is reversed if
;; you supply a prefix arg to `dlgopen-open-files'.)
;;
;; NOTE: This is a minor tweak of file `dlgopen.el', by Binu Jose
;;       Philip.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2011/12/19 dadams
;;     get-current-line: Use line-(beginning|end)-position.
;; 2010/01/12 dadams
;;     dlgopen-open-files: set-buffer -> with-current-buffer.
;; 2009/04/26 dadams
;;     get-current-line: Bind inhibit-field-text-motion to t.
;; 2000/06/12 dadams
;;     Tweaked dlgopen to use w32-browser if it is defined.
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


(eval-when-compile (require 'cl)) ;; decf (plus, for Emacs <20: when, unless)

(provide 'w32browser-dlgopen)

;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar dlgopen-executable-path "getfile.exe"
  "*Executable path for open dialog.")

(defvar dlgopen-other-window t
  "*Non-nil => display chosen file(s) in separate windows.
Nil => display a single chosen file in the current window;
       don't display multiple chosen files.")

(defsubst get-current-line ()
  (let ((inhibit-field-text-motion  t))
    (buffer-substring (line-beginning-position) (line-end-position))))

;;;###autoload
(defun dlgopen-open-files (&optional flip)
  "Open files using the Windows standard Open dialog box.
Var `dlgopen-executable-path' is path of executable `getfile.exe'.

If `w32-browser' is defined, then it is used to open the selected
files.  Otherwise, standard Emacs `find-file' functions are used.

If `w32-browser' is not defined, then `dlgopen-other-window'
controls how selected files are opened:
  non-nil => Display chosen file(s) in separate windows.
  nil     => Display a single chosen file in the current window;
             don't display multiple chosen files.

Optional prefix arg FLIP non-nil reverses the effect of variable
`dlgopen-other-window', for this call."
  (interactive "P")
  (unless (eq system-type 'windows-nt)
    (error "Command `dlgopen-open-files' is for Windows only"))
  (let ((buffer "")
        (file-fqn "")
        (lines-in-page 0)
        (dir-path "")
        (other-win (if flip
                       (not dlgopen-other-window)
                     dlgopen-other-window)))
    (setq buffer (generate-new-buffer "files-to-open"))
    (when (call-process dlgopen-executable-path nil buffer)
      (with-current-buffer buffer
        (save-excursion
          (goto-line 1)
          (setq dir-path (get-current-line))
          ;; If buffer empty, then user has cancelled or the open failed.
          ;; If only one line in buffer, then only one file selected, so use it.
          (when (> (buffer-size) 0)
            (if (= 1 (setq lines-in-page (count-lines 1 (buffer-size))))
                (if (fboundp 'w32-browser)
                    (w32-browser dir-path)
                  (if other-win
                      (find-file-other-window dir-path)
                    (find-file dir-path)))
              (while (> lines-in-page 1)
                (decf lines-in-page)
                (forward-line)
                (setq file-fqn (concat dir-path "/" (get-current-line)))
                (save-excursion
                  (if (fboundp 'w32-browser)
                      (w32-browser file-fqn)
                    (if other-win
                        (find-file-other-window file-fqn)
                      (find-file-noselect file-fqn)))))))))) ; no display
    (kill-buffer buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; w32browser-dlgopen.el ends here
