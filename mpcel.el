;;; mpcel.el --- A mpd (Music Player Daemon) client

;; Copyleft (C)  2006-2009  Jean-Baptiste Bourgoin <monsieur.camille@gmail.com>
;; GPLv3

;; Author: Jean-Baptiste Bourgoin <monsieur.camille@gmail.com>
;; Maintainer: Jean-Baptiste Bourgoin <monsieur.camille@gmail.com>
;; Version: 1.3
;; Keywords: music

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



;; Description :
;; 
;; mpcel.el is a mpc frontend for Emacs. mpc is a command line tool to
;; interface mpd (the Music Player Daemon). With mpcel.el you can control
;; mpd through mpc with the aim of playing your music collection.



;; Installation :
;;
;; 1) install mpc (http://www.musicpd.org/mpc.shtml) 
;;    & mpd (http://www.musicpd.org)
;; 2) type in your .emacs :
;;     (load "/path/to/mpcel.el")
;; 3) evaluate the previous expression, or restart Emacs.
;; 4) to start the mpd daemon type M-x mpcel-mpd-start
;; 5) to add songs type M-x mpcel-add-songs
;; 6) more help can be find on my website (EmacsWiki doesn't accept
;; it, mail me for more information)



;; Acknowledgments :
;;
;; Kim F. Storm : who give me a lot of advice about LISP code 
;;                (defcustom ; call-process ; eq ...). He's the source of
;;                almost all the differences between v1.0 & v1.1
;;
;; Rene Borchers : the author of the mpcel-playlist-load function
;;
;; Richard M. Stallman & Erwin David : who pointed out to me
;;                that my old project description was rather laconic.
;;
;; John Sullivan : who pointed out to me that the adress of the FSF
;;                has changed ;)


(defcustom mpcel-mpd-program (executable-find "mpd")
  "MPD program"
  :type 'string
  :group 'mpcel)

(defcustom mpcel-mpc-program (executable-find "mpc")
  "MPC program"
  :type 'string
  :group 'mpcel)



;; MPCEL Version
(defun mpcel-version ()
  "mpcel version. This is the first."
  (interactive)
  (message "mpcel version 1.1 - written in 2006 by Bourgoin Jean-Baptiste"))


;; MPD

;; Start mpd daemon :
(defun mpcel-mpd-start ()
  "Start mpd daemon"
  (interactive)
  (if (eq 0 (call-process mpcel-mpd-program nil nil nil))
      (message "mpd is ready")
    (message "mpd failed to start")))

;; update database
(defun mpcel-mpd-update ()
  "Update mpd database"
  (interactive)
  (if (eq 0 (call-process mpcel-mpc-program nil nil nil "update"))
      (message "i'm updating your database ;)")
    (message "Hey ! Have you made a database ?")))

;; MPC

;; Player control :

; mpc play track number ### :
(defun mpcel-play (number)
  "Play the track number ## with mpc"
  (interactive "MTrack number (type 0 to begin or resume): ")
  (call-process mpcel-mpc-program nil nil nil "play" number)
  (message "playing : %s" 
	   (shell-command-to-string "mpc status | head -n1")))

;; mpc stop playing music :
(defun mpcel-stop ()
  "Stop playing music in mpd"
  (interactive)
  (call-process mpcel-mpc-program nil nil nil "stop")
  (message "music is stoped"))

;; toggle random mode.
(defun mpcel-random-mode (onoroff)
  "mpcel : toggle random mode."
  (interactive 
   "MRandomize ? ( \"on\" or \"off\") : ")
  (call-process mpcel-mpc-program nil nil nil "random" onoroff)
  (message (concat "Random mode : " onoroff)))

;; toggle repeat mode.
(defun mpcel-repeat-mode (offoron)
  "mpcel : toggle repeat mode."
  (interactive 
   "MRandomize ? ( \"on\" or \"off\") : ")
  (call-process mpcel-mpc-program nil nil nil "reapeat" offoron)
  (message (concat "Repeat mode : " offoron)))

;; mpc pause playing music :
(defun mpcel-pause ()
  "Pause music in mpd"
  (interactive)
  (call-process mpcel-mpc-program nil nil nil "pause")
  (message "status : %s is paused"
	   (shell-command-to-string "mpc status | head -1")))

;; mpc play the next music :
(defun mpcel-next ()
  "Play the next music in mpd"
  (interactive)
  (call-process mpcel-mpc-program nil nil nil "next")
  (message "playing : %s"
	   (shell-command-to-string "mpc status | head -1")))

;; mpc play the previous music :
(defun mpcel-prev ()
  "Play the previous music in mpd"
  (interactive)
  (call-process mpcel-mpc-program nil nil nil "prev")
  (message "playing : %s"
	   (shell-command-to-string "mpc status | head -1")))

;; Crossfading
(defun mpcel-crossfade (cfade)
  "mpd : Sets  and  gets  the  current amount of crossfading between song"
  (interactive "MCrossfading between songs in seconds (0 to disable) : ")
  (call-process mpcel-mpc-program nil nil nil "crossfade" cfade)
  (message "amount of crossfading in seconds : %s" cfade))

;; set the volume
(defun mpcel-volume (vol)
  "mpcel : increase or decrease volume"
  (interactive "M[+-]value : ")
  (call-process mpcel-mpc-program nil nil nil "volume" vol))


;; Playlist control :

;; load playlist -- THANKS TO RENE BORCHERS !

(defun mpcel-playlist-load (playlist)
 "mpcel : Loads playlist"
 (interactive "MPlaylist name: ")
 (if (eq 0 (call-process mpcel-mpc-program nil nil nil "load" playlist))
     (message "Playlist %s loaded" playlist)
  (message "Playlist %s not found" playlist))
)

;; prints the playlist :
(defun mpcel-playlist-print ()
  "Prins entire mpd's playlist"
  (interactive)
  (shell-command 
   "mpc --format \"[%artist%--[%album%--[%title%]]]|[%file%]\" playlist"))

;; clear playlist
(defun mpcel-playlist-clear ()
  "Clear the mpd playlist"
  (interactive)
  (call-process mpcel-mpc-program nil nil nil "clear")
  (message "the playlist is cleared"))

;; add song :
(defun mpcel-add-songs (art rest)
  "Add songs in the mpd playlist"
  (interactive 
   "MArtist : \nMChoose album or song (type 0 for all) : ")
(let (mall mch)
  (setq muall 
	(concat "mpc search artist " art " | mpc add"))
  (setq mchse 
	(concat "mpc search artist " art " | grep -i " rest " | mpc add"))
  (if (eq nil rest)
      (shell-command muall)
    (shell-command mchse)))
  (message "Tracks : %s"
	   (shell-command-to-string "mpc playlist | wc -l")))


;; shuffle playlist
(defun mpcel-playlist-shuffle ()
  "mpcel : a Troll come to make your playlist untidiness"
  (interactive)
  (call-process mpcel-mpc-program nil nil nil "shuffle")
  (message "what a mess !"))

;; move song
(defun mpcel-playlist-move (pos1 pos2)
  "mpcel : Moves song at position x to the postion y in the playlist"
  (interactive 
   "nMove song number : \nnTo : ")
  (let (pstn)
    (setq pstn 
	  (concat "mpc move " 
		  (number-to-string pos1) " " (number-to-string pos2)))
    (shell-command-to-string pstn))
    (message (concat 
	      "Track number " (number-to-string pos1)
	      " move to position " (number-to-string pos2))))

;; crop -- Remove all songs except for the currently playing song:
(defun mpcel-playlist-crop ()
  "mpd : Remove all songs except for the currently playing song"
  (interactive)
  (if (eq 0 (call-process mpcel-mpc-program nil nil nil "crop"))
      (message "The only survivor is : %s" 
	       (shell-command-to-string "mpc status | head -1"))
    (message "no song is playing")))

(provide 'mpcel)
;;; mpcel.el ends here
