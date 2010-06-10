;;; @(#) topsongs.el -- a song database for the lazy emacser

;; This file is not part of Emacs

;; Copyright (C) 2007, 2008 by Alessandro Di Marco
;; Author:          Alessandro Di Marco (dmr@c0nc3pt.com)
;; Maintainer:      Alessandro Di Marco (dmr@c0nc3pt.com)
;; Created:         October 10, 2007
;; Keywords:        emms m3u
;; Latest Version:

;; COPYRIGHT NOTICE

;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along with
;; this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;;  TopSongs provides a song database for the lazy emacser. TopSongs interacts
;;  with both EMMS and mp3info, in order to provide handy commands such as
;;  `topsongs-insert-current-selected-track', recording your favorite songs
;;  on-the-fly. Next it will generate for you a m3u playlist, playable with
;;  `topsongs-play' in any moment.
;;
;;  For a full-featured playlist generation don't forget to install mp3info.

;;; Installation:
;;
;;  Put this file on your Emacs-Lisp load path, then add the following to your
;;  ~/.emacs startup file.
;;
;;     (require 'topsongs)

;;; Usage:
;;
;;  Do you really need help for this?

;;; Known Bugs:
;;
;;  Too simple to have one (hopefully ;-)

;;; Comments:
;;
;;  Any comments, suggestions, bug reports or upgrade requests are welcome.
;;  Please send them to Alessandro Di Marco (dmr@c0nc3pt.com).
;;
;;  This version of EM3u was developed and tested with GNU Emacs 22.1.50.1
;;  under Linux. Please, let me know if it works with other OS and versions of
;;  Emacs.

;;(require 'emms-player-mplayer)

(defgroup topsongs nil
  "A simple songs database."
  :tag "topsongs"
  :prefix "topsongs-"
  :group 'emms)

(defcustom topsongs-directory "~/.topsongs"
  "*Metadata directory."
  :group 'topsongs
  :type 'string)

(defcustom topsongs-temporary "/tmp/topsongs.m3u"
  "*Temporary m3u file (used for playing)."
  :group 'topsongs
  :type 'string)

(make-directory topsongs-directory t)

(defun emms-info-track-description-m3u (track)
  "Return a m3u description of the current track."
  (let ((artist (emms-track-get track 'info-artist "unknown"))
        (title (emms-track-get track 'info-title "unknown"))
        (time (emms-track-get track 'info-playing-time "")))
    (format "#EXTINF:%s,%s - %s" time artist title)))

(defun topsongs-insert-current-selected-track ()
  "Insert the current selected track into top-songs database."
  (interactive)
  (if emms-player-playing-p
      (let ((track (emms-playlist-current-selected-track)))
	(if track
	    (let ((song (emms-track-name track)))
	      (let ((file (format "%s/%s.ttd"
				  topsongs-directory
				  (sha1 song)))
		    (name (emms-track-get track
					  'info-title "unknown"))
		    (doit "inserted"))
		(if (file-exists-p file)
		    (let ((prompt
			   (format "'%s' already in database, substitute? "
				   name)))
		      (setq doit
			    (if (y-or-n-p-with-timeout prompt 5 nil)
				"replaced"
			      nil))))
		(if doit
		    (progn
		      (write-region
		       (format "%s\n%s\n"
			       (emms-info-track-description-m3u track) song)
		       nil file nil 'quietly)
		      (message "'%s' has been %s into top-songs database"
			       name doit))
		  (message "top-songs operation on '%s' aborted"
			   name))))
	  (message "No track information available")))
    (message "Nothing playing right now")))

(defun topsongs-remove-current-selected-track ()
  "Remove the current selected track from the top-songs database."
  (interactive)
  (if emms-player-playing-p
      (let ((track (emms-playlist-current-selected-track)))
	(if track
	    (let ((song (emms-track-name track)))
	      (let ((file (format "%s/%s.ttd"
				  topsongs-directory
				  (sha1 song)))
		    (name (emms-track-get track
					  'info-title "unknown")))
		(if (file-exists-p file)
		    (progn
		      (delete-file file)
		      (message "'%s' has been deleted from the top-songs database"
			       name))
		  (message "'%s' not in top-songs database" name))))
	  (message "No track information available")))
    (message "Nothing playing right now")))

(defun topsongs-save (file)
  "Convert the top-songs database in m3u format."
  (interactive)
  (save-excursion
    (let ((temp-buf (generate-new-buffer "topsongs")))
      (set-buffer temp-buf)
      (bury-buffer temp-buf)
      (insert "#EXTM3U\n")
      (dolist (file (directory-files topsongs-directory nil "\.ttd$" t))
	(insert-file-contents (format "%s/%s" topsongs-directory file)))
      (write-file file nil))))

(defun topsongs-play ()
  "Play the whole top-songs database."
  (interactive)
  (topsongs-save topsongs-temporary)
  (emms-play-m3u-playlist topsongs-temporary))

(provide 'topsongs)
