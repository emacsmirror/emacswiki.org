;;; mpris-dbus.el --- MPRIS call via dbus

;; Copyright (C) 2009, Changyuan Yu

;; Author: Changyuan Yu <reivzy@gmail.com>
;; Created: 2009-08-09
;; Version: 0.2
;; Keywords: dus, mpris
;; Compatibility: Emacs 23


;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; A simple library for simplify MPRIS(http://wiki.xmms2.xmms.se/wiki/MPRIS)
;; dbus call.

;; Example:
;;   (mpris-call "/TrackList" "GetMetadata" :int32 0)
;;   (mpris-track-list-call "GetLength")
;;   (mpris-player-call "GetCaps")
;;   (mpris-player-call "Next")
;;   (mpris-play)
;;   (mpris-toggle-play)

;;; Change Log:
;; 2010-06-05 v0.2
;;     convert 'tracknumber' filed to integer when necessary.
;; 2009-08-09 v0.1
;;     initial version.

;;; Code:

(require 'dbus)

(defgroup mpris nil
  "Media player remote control via dbus,
goto http://wiki.xmms2.xmms.se/wiki/MPRIS for detail."
  :group 'multimedia
  :group 'applications)

(defcustom mpris-service
  "org.mpris.audacious"
  "Default MPRIS dbus service."
  :type 'string
  :group 'mpris)

(defcustom mpris-timeout
  2000
  "Default MPRIS dbus timeout(in milliseconds)."
  :type 'integer
  :group 'mpris)

(defun mpris-call (interface method &rest args)
  "Make mpris dbus call."
  (apply 'dbus-call-method
         (append `(:session
                   ,mpris-service
                   ,interface
                   "org.freedesktop.MediaPlayer"
                   ,method
                   :timeout
                   ,mpris-timeout)
                 args)))

(defun mpris-track-list-call (method &rest args)
  "Call /TrackList object method."
  (apply 'mpris-call
         (append `("/TrackList" ,method) args)))


(defun mpris-player-call (method &rest args)
  "Call /Player object method."
  (apply 'mpris-call 
         (append `("/Player" ,method) args)))

(defun mpris-identity ()
  (mpris-call "/" "Identity"))

(defun mpris-quit ()
  (mpris-call "/" "Quit"))

(defun mpris-version ()
  (interactive)
  (mpris-call "/" "MprisVersion"))


;; /TrackList method

(defun mpris-get-metadata (&optional pos)
  (if pos (mpris-track-list-call "GetMetadata" :int32 pos)
    (mpris-player-call "GetMetadata")))

(defun mpris-get-current-track ()
  (mpris-track-list-call "GetCurrentTrack"))

(defun mpris-get-length ()
  (mpris-track-list-call "GetLength"))

(defun mpris-add-track (file instantly)
  (mpris-track-list-call "AddTrack" 
                    :string file
                    :boolean instantly))

(defun mpris-del-track (pos)
  (mpris-track-list-call "DelTrack"
                    :int32 pos))

(defun mpris-set-loop (set-loop)
  (mpris-track-list-call "SetLoop" :boolean set-loop))

(defun mpris-set-random (set-random)
  (mpris-track-list-call "SetRandom" :boolean set-random))

;; /Player method

;;;###autoload
(defun mpris-next ()
  (interactive)
  (mpris-player-call "Next"))

;;;###autoload
(defun mpris-prev ()
  (interactive)
  (mpris-player-call "Prev"))

;;;###autoload
(defun mpris-pause ()
  (interactive)
  (mpris-player-call "Pause"))

;;;###autoload
(defun mpris-stop ()
  (interactive)
  (mpris-player-call "Stop"))

;;;###autoload
(defun mpris-play ()
  (interactive)
  (mpris-player-call "Play"))

(defun mpris-repeat (set-repeat)
  (mpris-player-call "Repeat" :boolean set-repeat))

(defun mpris-get-status ()
  (mpris-player-call "GetStatus"))

(defun mpris-get-caps ()
  (mpris-player-call "GetCaps"))

(defun mpris-volume-get ()
  (mpris-player-call "VolumeGet"))

;;;###autoload
(defun mpris-volume-set (volume)
  (interactive "nvolume:")
  (mpris-player-call "VolumeSet" :int32 volume))

(defun mpris-position-get ()
  (mpris-player-call "PositionGet"))

;;;###autoload
(defun mpris-position-set (pos)
  (interactive "nposition(in ms):")
  (mpris-player-call "PositionSet" :int32 pos))


;; utils
(defun mpris-simplify-metadata (data)
  "Simpify metadata from format ((keyword (value))) to ((keyword . value))"
  (mapcar (lambda (i)
            (let ((field (intern (car i)))
                  (value (caadr i)))
              (when (and (member field '(audio-bitrate
                                         audio-samplerate
                                         mtime
                                         time
                                         tracknumber))
                         (stringp value))
                (setq value (string-to-number value)))
              (cons field value)))
          data))

(defun mpris-toggle-play ()
  "Toggle play/pause, when not stop."
  (interactive)
  (let ((p (car (mpris-get-status))))
    (when (< p 2)
      (if (= p 0) (mpris-pause)
        (mpris-play)))))

(provide 'mpris-dbus)

;;; mpris-dbus.el ends here
