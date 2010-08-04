;;; audel.el --- An audtool frontend for Emacs

;; LastChange: Jun 4, 2007
;; Version:    0.1
;; Author:     Amir Mohammad Saied <amirsaied AT gmail DOT com>

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation
;; 51 Franklin Street, Fifth Floor
;; Boston, MA 02110-1301
;; USA


;;; Commentary:
;; 
;; audel.el is a audtool frontend for Emacs. audtool is a command line tool to
;; interface audacious. With audel.el you can control audacious through audtool.


;; Installation :
;;
;; Add this line to your .emacs:
;;     (load "/path/to/audel.el")

;; Provides:
;; M-x
;;    audel-version
;;    audel-next
;;    audel-prev
;;    audel-pause
;;    audel-play
;;    audel-stop
;;    audel-fetch-current-song-info
;;    audel-shuffle-toggle
;;    audel-repeat-toggle
;;    audel-volume
;;    audel-playlist

(defcustom audel-audtool-program (executable-find "audtool")
  "Audtool Program"
  :type 'string
  :group 'audel)

(setq audel-version 0.1)

(defun audel-version ()
  "audel version"
  (interactive)
  (message "Version: %.1f." audel-version))

(defun audel-next ()
  "Plays the next item in playlist"
  (interactive)
  (call-process audel-audtool-program nil nil nil "--playlist-advance")
  (sleep-for 0 20)
  (audel-fetch-current-song-info))

(defun audel-prev ()
  "Play the previouse item in playlist"
  (interactive)
  (call-process audel-audtool-program nil nil nil "--playlist-reverse")
  (sleep-for 0 20)
  (audel-fetch-current-song-info))

(defun audel-pause ()
  "Pause!"
  (interactive)
  (call-process audel-audtool-program nil nil nil "--playback-pause"))

(defun audel-play ()
  "Please play play play"
  (interactive)
  (call-process audel-audtool-program nil nil nil "--playback-play")
  (sleep-for 0 20)
  (audel-fetch-current-song-info))

(defun audel-goto (item)
  "Jump Jump Jump!"
  (interactive "MItem No.: ")
  (call-process audel-audtool-program nil nil nil "--playlist-jump" item)
  (sleep-for 0 20)
  (audel-fetch-current-song-info))

(defun audel-stop ()
  "STOP IT!"
  (interactive)
  (call-process audel-audtool-program nil nil nil "--playback-stop"))

(defun audel-fetch-current-song-info ()
  "Fetches playin' songs required info"
  (interactive)
  (setq current-song-title (shell-command-to-string "audtool --current-song"))
  (setq current-song-length(shell-command-to-string "audtool --current-song-length"))
  (message "%s [%s]" current-song-title current-song-length))

(defun audel-shuffle-toggle ()
  "Toggle Shuffle mode"
  (interactive)
  (if (string-match "off" (shell-command-to-string "audtool --playlist-shuffle-status"))
    (message "Shuffle: ON")
    (message "Shuffle: OFF"))
  (call-process audel-audtool-program nil nil nil "--playlist-shuffle-toggle"))

(defun audel-repeat-toggle ()
  "Toggle Repeat mode"
  (interactive)
  (if (string-match "off" (shell-command-to-string "audtool --playlist-repeat-status"))
    (message "Repeat: ON")
    (message "Repeat: OFF"))
  (call-process audel-audtool-program nil nil nil "--playlist-repeat-toggle"))

(defun audel-volume (vol)
  "Set volume"
  (interactive "M[+|-]percent: ")
  (call-process audel-audtool-program nil nil nil "--set-volume" vol))

(defun audel-playlist ()
  "Print Audaciou's playlist"
  (interactive)
  (shell-command "audtool --playlist-display | sed -e '1d' | sed -e '/^Total length/d'"))

(provide 'audel)
;;; audel.el ends here
