;;; anything-emms.el --- Integrate EMMS with `anything.el'

;; Filename: anything-emms.el
;; Description: Integrate EMMS with `anything.el'
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2009, Andy Stewart, all rights reserved.
;; Created: 2009-02-12 22:44:37
;; Version: 0.1
;; Last-Updated: 2009-05-12 23:19:31
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/anything-emms.el
;; Keywords: emms, anything
;; Compatibility: GNU Emacs 23.0.60.1
;;
;; Features that might be required by this library:
;;
;; `emms-source-file' `emms-extension' `emms-playlist-mode'
;; `emms' `anything'
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Integrate EMMS with `anything.el'.
;;
;; Below are commands you can use:
;;
;; `anything-emms-playlist'
;;      Play EMMS playlist with `anything'.
;; `anything-emms-directory'
;;      Play EMMS directory with `anything'.
;; `anything-emms-file'
;;      Play EMMS directory with `anything'.
;;
;; Or you can also make this package integrate with `anything',
;; just setup like below:
;;
;; (setq anything-sources
;;       (list
;;        anything-c-source-emms-playlist
;;        anything-c-source-emms-directory
;;        anything-c-source-emms-file
;;        ))
;;

;;; Installation:
;;
;; Put anything-emms.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'anything-emms)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET anything-emms RET
;;

;;; Change log:
;;
;; 2009/02/12
;;      * First released.
;;

;;; Acknowledgements:
;;
;;
;;

;;; TODO
;;
;;
;;

;;; Require
(require 'emms)
(require 'emms-source-file)
(require 'emms-playlist-mode)
(require 'emms-extension)
(require 'anything)

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Variable ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar anything-c-source-emms-playlist
  '((name . "EMMS Playlist")
    (init . (lambda ()
              (when (and emms-playlist-buffer
                         (bufferp emms-playlist-buffer))
                (anything-candidate-buffer (get-buffer emms-playlist-buffer)))))
    (candidates-in-buffer)
    (get-line . buffer-substring)
    (action . (("Play" . (lambda (candidate)
                           (save-excursion
                             (with-current-emms-playlist
                               (goto-char (point-min))
                               (search-forward candidate)
                               (emms-playlist-mode-play-smart)))))
               ("Edit Tag" . (lambda (candidate)
                               (save-excursion
                                 (with-current-emms-playlist
                                   (goto-char (point-min))
                                   (search-forward candidate)
                                   (forward-line -1)
                                   (emms-tag-editor-edit)))))
               ("Jump to file" . (lambda (candidate)
                                   (save-excursion
                                     (with-current-emms-playlist
                                       (goto-char (point-min))
                                       (search-forward candidate)
                                       (emms-jump-to-file)))))
               ("Switch to playlist" . (lambda (candidate)
                                         (emms-playlist-mode-go)))))))

(defvar anything-c-source-emms-directory
  '((name . "EMMS directory")
    (init . (anything-emms-directory-init))
    (candidates-in-buffer)
    (action . (("Play directory" . (lambda (candidate)
                                     (emms-play-directory-tree candidate)))
               ("Add directory" . (lambda (candidate)
                                    (emms-add-directory-tree candidate)))))))

(defvar anything-c-source-emms-file
  '((name . "EMMS file")
    (init . (anything-emms-file-init))
    (candidates-in-buffer)
    (action . (("Add and Play file" . (lambda (candidate)
                                        (emms-add-file candidate)
                                        (with-current-emms-playlist
                                          (goto-char (point-max))
                                          (forward-line -1)
                                          (emms-playlist-mode-play-smart))))
               ("Play file" . (lambda (candidate)
                                (emms-play-file candidate)))
               ("Add file" . (lambda (candidate)
                               (emms-add-file candidate)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Interactive Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun anything-emms-playlist ()
  "Play EMMS playlist with `anything'."
  (interactive)
  (anything 'anything-c-source-emms-playlist))

(defun anything-emms-directory ()
  "Play EMMS directory with `anything'."
  (interactive)
  (anything 'anything-c-source-emms-directory))

(defun anything-emms-file ()
  "Play EMMS file with `anything'."
  (interactive)
  (anything 'anything-c-source-emms-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Utilities Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun anything-emms-directory-init ()
  "Init `anything-c-source-emms-directory'."
  (let ((anything-buffer (anything-candidate-buffer 'global)))
    (with-current-buffer anything-buffer
      (anything-emms-directory-list emms-source-file-default-directory))))

(defun anything-emms-directory-list (dirs)
  "Generate directory list under DIRS."
  (dolist (file (directory-files dirs t))
    (if (file-directory-p file)
        (unless (string-match "^\\.\\.?$" (file-name-nondirectory file))
          (insert (format "%s\n" file))
          (anything-emms-directory-list file)))))

(defun anything-emms-file-init ()
  "Init `anything-c-source-emms-file'."
  (let ((anything-buffer (anything-candidate-buffer 'global)))
    (with-current-buffer anything-buffer
      (anything-emms-file-list emms-source-file-default-directory))))

(defun anything-emms-file-list (dirs)
  "Generate music file list under DIRS."
  (dolist (file (directory-files dirs t))
    (if (file-directory-p file)
        (unless (string-match "^\\.\\.?$" (file-name-nondirectory file))
          (anything-emms-file-list file))
      (if (string-match (emms-player-get (emms-player-for (emms-playlist-current-selected-track)) 'regex) file)
          (insert (format "%s\n" file))))))

(provide 'anything-emms)

;;; anything-emms.el ends here


;;; LocalWords:  emms dirs regex mplayer
