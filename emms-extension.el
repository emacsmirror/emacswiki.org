;;; emms-extension.el --- Some extensions for emms

;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2008, 2009, Andy Stewart, all rights reserved.
;; Created: 2008-07-19 23:43:46
;; Version: 0.2
;; Last-Updated: 2008-07-19 23:43:54
;; URL: not distributed yet
;; Keywords: emms, w3m
;; Compatibility: GNU Emacs 23.0.60.1

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

;; Features that might be required by this library:
;;
;; `emms' `emms-playlist-mode' `emms-source-file' `emms-playlist-sort'
;; `emms-mark' `basic-toolkit' `emms-last-played'
;;

;;; Installation:
;;
;; Copy emms-extension.el to your load-path and add to your ~/.emacs
;;
;;  (require 'emms-extension)
;;
;; No need more

;;; Commentary:
;;
;; Some extensions for emms.
;;

;;; Change log:
;;
;; 2008/09/25
;;         Add two advice to make `emms-random' and `emms-playlist-sort-by-natural-order'
;;         execute after `emms-play-directory-tree'.
;;
;; 2008/07/19
;;         First release.
;;

;;; Acknowledgments:
;;
;; Not yet
;;

;;; TODO
;;
;; None
;;

;;; Code:

;;; Require
(require 'emms-mark)
(require 'emms-last-played)
(require 'emms-playlist-mode)
(require 'emms-source-file)
(require 'emms-playlist-sort)
(require 'emms)
(require 'basic-toolkit)

(defadvice emms-play-directory-tree (after emms-random-play-1 activate)
  "This advice to make `emms-random' execute after emms-play-directory-tree"
  (emms-random))

(defadvice emms-play-directory-tree (after emms-playlist-sort-by-natural-order-1 activate)
  "This advice to make `emms-playlist-sort-by-natural-order' execute after emms-play-directory-tree"
  (emms-playlist-sort-by-natural-order))

(defadvice emms-history-load (after play-default activate)
  "If after `emms-history-load', get empty playlist,
play default music directory."
  (when (not emms-player-playing-p)
    (emms-play-now)
    (message "Emms history load failed, load default music directory...")))

(defun emms-play-matching (pattern)
  "Play matching song."
  (interactive "sPlay song matching: ")
  ;; Won't clear playlist when
  ;; `current-prefix-arg' is non-nil.
  (unless current-prefix-arg
    (with-current-emms-playlist
      (emms-playlist-clear)))
  (emms-play-find emms-source-file-default-directory pattern))

(defun emms-jump-to-file ()
  "Jump to postion of current playing music."
  (interactive)
  (let* ((music-file (emms-track-name (emms-playlist-current-selected-track))) ;get playing music file name
         (music-folder (file-name-directory music-file))) ;get playing music directory
    (dired-x-find-file music-folder)                      ;jump to music directory
    (dired-goto-file music-file)))                        ;jump to music file postion

(defun kid-emms-info-simple (track)
  "Parse current singer and song name bases file name."
  (when (eq 'file (emms-track-type track))
    (let ((regexp "/\\([^/]+\\)/\\([^/]+\\)\\.[^.]+$") (name (emms-track-name track)))
      (if (string-match regexp name)
          (progn (emms-track-set track 'info-artist (match-string 1 name))
                 (emms-track-set track 'info-title (match-string 2 name)))
        (emms-track-set track 'info-title (file-name-nondirectory name))))))

(defun emms-delete-file-from-disk ()
  "Delete this file from disk."
  (interactive)
  (let* ((current-track (emms-track-name (emms-playlist-track-at))))
    (when (yes-or-no-p (format "Are you really want to delete \' %s \' from disk? " current-track))
      (if (string-equal current-track (emms-playlist-play-filename))
          (emms-stop))
      (emms-playlist-mode-kill-entire-track)
      (dired-delete-file current-track)
      (message (format "Have delete \' %s \' from disk." current-track)))))

(defun emms-playlist-play-filename ()
  "Return the filename the current play."
  (cdr (assoc 'name (emms-playlist-current-selected-track))))

(defun emms-play-now()
  "Play default music directory."
  (interactive)
  (emms-play-directory-tree emms-source-file-default-directory))

(defun emms-play-online()
  "Play online music use emms."
  (interactive)
  (if (and (require 'w3m nil t)
           (w3m-anchor))
      (emms-play-url (w3m-anchor))
    (message "No valid url in here.")))

(defun lazycat/emms-info-track-description (track)
  "Return a description of the current track."
  (let* ((name (emms-track-name track))
         (type (emms-track-type track))
         (short-name (file-name-nondirectory name))
         (play-count (or (emms-track-get track 'play-count) 0))
         (last-played (or (emms-track-get track 'last-played) '(0 0 0)))
         (empty "..."))
    (prog1
        (case (emms-track-type track)
          ((file url)
           (let* ((artist (or (emms-track-get track 'info-artist) empty))
                  (playing-time (or (emms-track-get track 'info-playing-time) 0))
                  (min (/ playing-time 60))
                  (sec (% playing-time 60))
                  (album (or (emms-track-get track 'info-album) empty))
                  (short-name (file-name-sans-extension
                               (file-name-nondirectory name)))
                  (title (or (emms-track-get track 'info-title) short-name))
                  )
             (format "%8s         %-50s    %-50s"
                     ;; time
                     (if (or (> min 0)  (> sec 0))
                         (format "%02d:%02d" min sec)
                       ".....")
                     ;; Title
                     (prettyfy-string title 40)
                     ;; Artis
                     (prettyfy-string artist 40)
                     )))
          ((url)
           (concat (symbol-name type) ":" name))
          (t
           (format "%-3d%s"
                   play-count
                   (concat (symbol-name type) ":" name))))
      )))

(defun de-kill-covers-and-parents (dir tracks)
  "Delete current directory, if not have music file in directory when delete current track."
  (when (> (length tracks) 1)
    ;; if we're not deleting an individual file, delete covers too
    (dolist (cover '("cover.jpg"
                     "cover_med.jpg"
                     "cover_small.jpg"
                     "folder.jpg"))
      (condition-case nil
          (delete-file (concat dir cover))
        (error nil)))
    ;; try and delete empty parents - we actually do the work of the
    ;; calling function here, too
    (let (failed)
      (while (and (not (string= dir "/"))
                  (not failed))
        (condition-case nil
            (delete-directory dir)
          (error (setq failed t)))
        (setq dir (file-name-directory (directory-file-name dir)))))))

(defun emms-mark-track-and-move-next ()
  "Mark the current track, and move next track."
  (interactive)
  (call-interactively 'emms-mark-track)
  (call-interactively 'next-line))

(defun emms-mark-unmark-track-and-move-next ()
  "Unmark the current track, and move next track."
  (interactive)
  (call-interactively 'emms-mark-unmark-track)
  (call-interactively 'next-line))

(defun emms-tag-editor-next-same-field (&optional reverse)
  "Jump to next same field."
  (interactive)
  (let (filed-name)
    (save-excursion
      (beginning-of-line)
      (if (search-forward-regexp "^[^ ]*[ \t]+= " (line-end-position) t)
          (setq filed-name (buffer-substring (match-beginning 0) (match-end 0)))))
    (when filed-name
      (if (null reverse)
          (search-forward-regexp filed-name (point-max) t)
        (beginning-of-line)
        (search-backward-regexp filed-name (point-min) t))
      (goto-char (match-end 0)))))

(defun emms-tag-editor-prev-same-field ()
  "Jump to previous same field."
  (interactive)
  (emms-tag-editor-next-same-field t))

(defun emms-tag-editor-set-all+ ()
  "Set TAG to VALUE in all tracks.
If transient-mark-mode is turned on, you can apply the command to
a selected region.

 If `transient-mark-mode' is on and the mark is active, the
changes will only take effect on the tracks in the region.

This function is extension `emms-tag-editor-set-all',
make user can modified TAG content, not just type."
  (interactive)
  (let (tag current-value value)
    (setq tag (completing-read "Set tag: " emms-tag-editor-tags nil t))
    (save-excursion
      (goto-char (point-min))
      (search-forward-regexp (concat "^" tag "[ \t]+= ") (point-max) t 1)
      (setq current-value (buffer-substring (match-end 0) (line-end-position))))
    (setq value (read-from-minibuffer "To: " current-value))
    (save-excursion
      (save-restriction
        (if (and mark-active transient-mark-mode)
            (narrow-to-region (region-beginning) (region-end)))
        (goto-char (point-min))
        (while (re-search-forward (concat "^" (regexp-quote tag)) nil t)
          (skip-chars-forward " \t=")
          (delete-region (point) (line-end-position))
          (insert value))))))

(defun emms-tag-editor-set-tracknumber ()
  "Set `info-tracknumber' tag with a init increment value
and special alternation number."
  (interactive)
  (let (init-number alternation-number times)
    (setq init-number (read-number "Init number: " 1))
    (setq alternation-number (read-number "Alternation number: " 1))
    (setq times 0)
    (save-excursion
      (save-restriction
        (if (and mark-active transient-mark-mode)
            (narrow-to-region (region-beginning) (region-end)))
        (goto-char (point-min))
        (while (re-search-forward (concat "^info-tracknumber") nil t)
          (skip-chars-forward " \t=")
          (delete-region (point) (line-end-position))
          (insert (format "%s" (+ init-number (* alternation-number times))))
          (setq times (1+ times))
          )))))

(defun emms-tag-editor-set-tracknumber+ ()
  "Set `info-tracknumber' tag with a init increment value
and special alternation number."
  (interactive)
  (let (init-number alternation-number times)
    (setq init-number 1)
    (setq alternation-number 1)
    (setq times 0)
    (save-excursion
      (save-restriction
        (if (and mark-active transient-mark-mode)
            (narrow-to-region (region-beginning) (region-end)))
        (goto-char (point-min))
        (while (re-search-forward (concat "^info-tracknumber") nil t)
          (skip-chars-forward " \t=")
          (delete-region (point) (line-end-position))
          (insert (format "%s" (+ init-number (* alternation-number times))))
          (setq times (1+ times))
          )))))

(defun emms-mark-duplicate-track ()
  "Mark duplicate track."
  (interactive)
  (let (have-duplicate-track
        current-track-title
        next-track-title
        original-tracks
        selected-point
        original-point)
    ;; Backup playlist data.
    (setq original-point (point))       ;backup cursor position
    (emms-playlist-mode-center-current) ;goto current play track
    (setq selected-point (point))       ;backup select track
    (setq original-tracks (nreverse     ;backup playlist with sequence
                           (emms-playlist-tracks-in-region
                            (point-min) (point-max))))
    ;; Find duplicate track.
    (emms-playlist-sort-by-info-title)
    (goto-char (point-min))
    (while (not (eobp))
      (save-excursion
        (setq current-track-title (emms-playlist-current-title))
        (forward-line +1)
        (setq next-track-title (emms-playlist-current-title)))
      (if (string-equal current-track-title next-track-title)
          (progn
            (or have-duplicate-track (setq have-duplicate-track t))
            (emms-mark-track)
            (forward-line +1)
            (emms-mark-track))
        (forward-line +1)))
    ;; Last step.
    (if have-duplicate-track
        (emms-first-mark-track)
      (emms-playlist-clear)             ;clear playlist
      (dolist (track original-tracks)   ;restore origianl playlist
        (emms-playlist-insert-track track))
      (emms-playlist-select selected-point) ;select play track
      (goto-char original-point)            ;restore original point
      (message "Haven't found duplicate track."))))

(defun emms-first-mark-track ()
  "Jump to first mark track."
  (interactive)
  (let ((original-point (point))
        (original-column (current-column)))
    (goto-char (point-min))
    (if (search-forward-regexp (format "^%c" emms-mark-char) nil t)
        (move-to-column original-column t)
      (goto-char original-point)
      (message "No mark track."))))

(defun emms-last-mark-track ()
  "Jump to last mark track."
  (interactive)
  (let ((original-point (point))
        (original-column (current-column)))
    (goto-char (point-max))
    (if (search-backward-regexp (format "^%c" emms-mark-char) nil t)
        (move-to-column original-column t)
      (goto-char original-point)
      (message "No mark track."))))

(defun emms-next-mark-track ()
  "Jump to next mark track."
  (interactive)
  (let ((original-point (point))
        (original-column (current-column)))
    (if (bolp)
        (forward-char +1))
    (if (search-forward-regexp (format "^%c" emms-mark-char) nil t)
        (move-to-column original-column t)
      (goto-char original-point)
      (message "No next mark track."))))

(defun emms-prev-mark-track ()
  "Jump to previous mark track."
  (interactive)
  (let ((original-point (point))
        (original-column (current-column)))
    (if (not (bolp))
        (beginning-of-line))
    (if (search-backward-regexp (format "^%c" emms-mark-char) nil t)
        (move-to-column original-column t)
      (goto-char original-point)
      (message "No previous mark track."))))

(defun emms-playlist-current-title ()
  "Return the filename the current play."
  (cdr (assoc 'info-title (emms-playlist-track-at))))

(provide 'emms-extension)

;;; emms-extension.el ends here

;;; LocalWords:  emms toolkits playlist sPlay postion online lazycat prettyfy
;;; LocalWords:  tracknumber zerop de unmark nreverse origianl
