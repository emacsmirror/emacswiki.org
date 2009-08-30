;;; download-icicles.el -- Download the Icicles Package
;;
;; This file is NOT part of Emacs
;;
;; Filename: download-icicles.el
;; Description: Download the Icicles Package
;; Author: Anupam Sengupta, Tom Tromley
;; Maintainer: Anupam Sengupta
;; Copyright (C) 2007 Anupam Sengupta, all rights reserved.
;; Created: Wed May 24 14:05:13 2007
;; Version: 1.0
;; Last-Updated: Wed May 24 14:05:17 2007
;;           By: anupamsg
;;
;; URL: http://www.emacswiki.org/cgi-bin/wiki/download-icicles.el
;; Compatibility: GNU Emacs 20.x, GNU Emacs 21.x, GNU Emacs 22.x
;;
;; Features that might be required by this library:
;;
;; `cl', `url'
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Commentary:
;;
;; Function to download the Icicles code library from
;; http://www.emacswiki.org

;; Acknowledgements:
;;
;; Code based largely on the package.el install implementation by Tom Tromley.
;;
;; See: http://tromey.com/elpa/package-install.el
;; See: http://www.emacswiki.org/cgi-bin/wiki/TomTromley

;; Define some variables for use later
;; You can change the `icicle-download-dir' variable
(let ((icicles-archive-base "http://www.emacswiki.org/cgi-bin/wiki/download/")
      (icicle-download-dir (expand-file-name "~/icicles"))
      (icicle-file-list (list
                         "icicles.el"
                         "icicles-cmd.el"
                         "icicles-face.el"
                         "icicles-fn.el"
                         "icicles-mac.el"
                         "icicles-mcmd.el"
                         "lacarte.el"
                         "icicles-mode.el"
                         "icicles-opt.el"
                         "icicles-var.el"
                         "icomplete+.el"
                         "synonyms.el"
                         "hexrgb.el")))

  (require 'cl)                         ; For flet and pop

  (flet ((download (url)                ; Define the download function
                   (if (fboundp 'url-retrieve-synchronously)
                       ;; If available, Use URL to download.
                       (let ((buffer (url-retrieve-synchronously url)))
                         (save-excursion
                           (set-buffer buffer)
                           (goto-char (point-min))
                           (re-search-forward "^$" nil 'move)
                           (forward-char)
                           (delete-region (point-min) (point))
                           buffer))
                     ;; Else use wget to download
                     (save-excursion
                       (with-current-buffer
                           (get-buffer-create
                            (generate-new-buffer-name " *Download*"))
                         (shell-command (concat "wget -q -O- " url)
                                        (current-buffer))
                         (goto-char (point-min))
                         (current-buffer))))))
 
    ;; Make the icicles directory if not present
    (make-directory icicle-download-dir t)

    ;; Download the icicle lisp files and put it in the download directory.
    (while icicle-file-list
      (let ((icicle-file-name (pop icicle-file-list)))
        (let ((pkg-buffer
               (download (concat icicles-archive-base icicle-file-name))))
          ;; Save the downloaded buffer contents in the file
          (save-excursion
            (set-buffer pkg-buffer)
            (setq buffer-file-name
                  (concat (file-name-as-directory icicle-download-dir)
                          icicle-file-name))
            (save-buffer)
            (kill-buffer pkg-buffer)
            (sleep-for 2)               ; Sleep to prevent overloading emacs
                                        ; wiki
            (message "Downloaded %s"
                     (concat icicles-archive-base icicle-file-name))))))
    ;; Done!!!
    (message "Downloaded icicles successfully in %s" icicle-download-dir)))



