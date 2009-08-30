;;; amarok.el --- Simple interface to Amarok music player
;;
;; Copyright (C) 2006 Mathias Dahl
;;
;; Version: 0.1
;; Keywords: multimedia
;; Author: Mathias Dahl <mathias.rem0veth1s.dahl@gmail.com>

;; This file is NOT part of GNU Emacs.

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

;;; Commentary:

;; This file provides a simple interface to play music in Amarok.  It
;; just provides some basic commands to pause/play and manipulate
;; playlist.  Its primary use is for control from Dired, while browsing
;; your music collection there.
;;
;; For convenience, a command, `amarok-setup-dired-key-bindings', is
;; provided for setting up useful keybindings in Dired. Use that or
;; create your own bindings.
;;

;;; History:

;; Version 0.1, 2006-08-12
;;
;; * First release.

;;; Code:

(defun amarok-dired-add-files ()
  "Add marked files to Amarok playlist."
  (interactive)
  (dired-map-over-marks
   (let ((file (dired-get-filename)))
     (shell-command
      (format "dcop amarok playlist addMedia \"%s\"" file)))
   nil))

(defun amarok-dired-play-current-file ()
  "Play current file in Amarok."
  (interactive)
  (shell-command
   (format "dcop amarok playlist playMedia \"%s\"" (dired-get-filename))))

(defun amarok-clear-playlist ()
  "Clear Amarok playlist."
  (interactive)
  (shell-command
   (format "dcop amarok playlist clearPlaylist")))

(defun amarok-play ()
  "Start playback in Amarok."
  (interactive)
  (amarok-player "play"))

(defun amarok-pause ()
  "Pause playback in Amarok."
  (interactive)
  (amarok-player "pause"))

(defun amarok-play-pause ()
  "Toggle play/pause in Amarok."
  (interactive)
  (amarok-player "playPause"))

(defun amarok-player (command)
  "Send COMMAND to Amarok player."
  (shell-command
   (format "dcop amarok player %s" command)))

(defun amarok-setup-dired-key-bindings ()
  "Setup convenient bindings for Amarok in dired.

The following bindings will be set up:

C-c a p - Play/pause
C-c a c - Clear playlist
C-c a a - Add marked files to playlist
C-c a . - Play current file"
  (interactive)
  (define-key dired-mode-map (kbd "C-c a p") 'amarok-play-pause)
  (define-key dired-mode-map (kbd "C-c a c") 'amarok-clear-playlist)
  (define-key dired-mode-map (kbd "C-c a a") 'amarok-dired-add-files)
  (define-key dired-mode-map (kbd "C-c a .") 'amarok-dired-play-current-file))

(provide 'amarok)

;;; amarok.el ends here
