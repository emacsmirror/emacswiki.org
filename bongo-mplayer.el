;;; bongo-mplayer.el --- MPlayer backend glue for Bongo

;; Copyright (C) 2005, 2006, 2007  Daniel Brockman
;; Copyright (C) 2005  Lars Öhrman

;; Author: Lars Öhrman <larohr@gmail.com>
;; Maintainer: Daniel Brockman <daniel@brockman.se>
;; URL: http://www.emacswiki.org/cgi-bin/wiki/BongoMPlayer
;; Created: September 3, 2005
;; Updated: February 4, 2007

;; This file is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty
;; of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;; See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program (see the file `COPYING');
;; if not, write to the Free Software Foundation, 51 Franklin
;; Street, Fifth Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;; Due to the fact that MPlayer recommends use of non-free
;; software (and in part due to the poor quality of its
;; backend interface), we have chosen to remove MPlayer
;; support from the main Bongo repository.

;; We recommend VLC as a replacement player.  If you still
;; want to use MPlayer, this package lets you do it.

;;; Code:

(require 'bongo)

(define-bongo-backend mplayer
  :constructor 'bongo-start-mplayer-player

  ;; We define this variable manually so that we can get
  ;; some other customization variables to appear before it.
  :extra-program-arguments-variable nil

  ;; Play generic URLs and files if the file extension
  ;; matches that of some potentially supported format.
  :matcher '((local-file "file:" "http:" "ftp:")
             "ogg" "flac" "mp3" "mka" "wav" "wma"
             "mpg" "mpeg" "vob" "avi" "ogm" "mp4" "mkv"
             "mov" "asf" "wmv" "rm" "rmvb" "ts")

  ;; Play special media URIs regardless of the file name.
  :matcher '(("mms:" "mmst:" "rtp:" "rtsp:" "udp:" "unsv:"
              "dvd:" "vcd:" "tv:" "dvb:" "mf:" "cdda:" "cddb:"
              "cue:" "sdp:" "mpst:" "tivo:") . t)

  ;; Play all HTTP URLs (necessary for many streams).
  ;; XXX: This is not a good long-term solution.  (But it
  ;;      would be good to keep this matcher as a fallback
  ;;      if we could somehow declare that more specific
  ;;      matchers should be tried first.)
  :matcher '(("http:") . t)

  ;; Transform CDDA URIs into the right syntax for mplayer.
  :file-name-transformer
  (cons (eval-when-compile
          (rx (and string-start
                   (submatch (and (or "cdda" "cddb") "://"))
                   ;; Device file name.
                   (optional (submatch
                              (one-or-more (not (any "@")))))
                   ;; Track number.
                   (optional (and "@" (submatch
                                       (zero-or-more anything))))
                   string-end)))
        "\\1\\3/\\2"))

(defun bongo-mplayer-available-drivers (type)
  (unless (memq type '(audio video))
    (error "Invalid device type"))
  (when (executable-find bongo-mplayer-program-name)
    (let ((result nil))
      (with-temp-buffer
        (call-process bongo-mplayer-program-name nil t nil
                      (ecase type
                        (audio "-ao")
                        (video "-vo"))
                      "help")
        (goto-char (point-min))
        (search-forward (concat "Available " (ecase type
                                               (audio "audio")
                                               (video "video"))
                                " output drivers:\n"))
        (while (looking-at
                (eval-when-compile
                  (rx (and line-start
                           (one-or-more space)
                           (submatch (one-or-more word))
                           (one-or-more space)
                           (submatch (zero-or-more not-newline))
                           line-end))))
          (setq result (cons (cons (match-string 1)
                                   (match-string 2))
                             result))
          (forward-line)))
      (reverse result))))

(defcustom bongo-mplayer-audio-driver nil
  "Audio driver to be used by mplayer.
This corresponds to the `-ao' option of mplayer."
  :type `(choice (const :tag "System default" nil)
                 ,@(mapcar (lambda (entry)
                             `(const :tag ,(concat (car entry)
                                                   " (" (cdr entry) ")")))
                           (bongo-mplayer-available-drivers 'audio))
                 (string :tag "Other audio driver"))
  :group 'bongo-mplayer)

(bongo-define-obsolete-variable-alias
  'bongo-mplayer-audio-device
  'bongo-mplayer-audio-driver)

(defcustom bongo-mplayer-video-driver nil
  "Video driver to be used by mplayer.
This corresponds to the `-vo' option of mplayer."
  :type `(choice (const :tag "System default" nil)
                 ,@(mapcar (lambda (entry)
                             `(const :tag ,(concat (car entry)
                                                   " (" (cdr entry) ")")))
                           (bongo-mplayer-available-drivers 'video))
                 (string :tag "Other video driver"))
  :group 'bongo-mplayer)

(bongo-define-obsolete-variable-alias
  'bongo-mplayer-video-device
  'bongo-mplayer-video-driver)

(defcustom bongo-mplayer-interactive t
  "If non-nil, use the slave mode of mplayer.
Setting this to nil disables the pause and seek functionality."
  :type 'boolean
  :group 'bongo-mplayer)

(defcustom bongo-mplayer-time-update-delay-after-seek 1
  "Number of seconds to delay time updates from mplayer after seeking.
Such delays may prevent jerkiness in the visual seek interface."
  :type 'number
  :group 'bongo-mplayer)

(defcustom bongo-mplayer-extra-arguments nil
  "Extra command-line arguments to pass to `mplayer'.
These will come at the end or right before the file name, if any."
  :type '(repeat (choice string variable sexp))
  :group 'bongo-mplayer)

(bongo-define-obsolete-function-alias
  'bongo-mplayer-player-interactive-p
  'bongo-default-player-interactive-p)

(bongo-define-obsolete-function-alias
  'bongo-mplayer-player-paused-p
  'bongo-default-player-paused-p)

(defun bongo-mplayer-player-pause/resume (player)
  (when (not (bongo-player-interactive-p player))
    (error (concat "This mplayer process is not interactive "
                   "and so does not support pausing")))
  (process-send-string (bongo-player-process player) "pause\n")
  (bongo-player-put player 'paused
    (not (bongo-player-get player 'paused)))
  (bongo-player-paused/resumed player))

(defun bongo-mplayer-player-seek-to (player seconds)
  (when (not (bongo-player-interactive-p player))
    (error (concat "This mplayer process is not interactive "
                   "and so does not support seeking")))
  (process-send-string (bongo-player-process player)
                       (format "seek %f 2\n" (max seconds 0)))
  (bongo-player-sought player seconds))

(bongo-define-obsolete-function-alias
  'bongo-mplayer-player-seek-by
  'bongo-default-player-seek-by)

(defun bongo-mplayer-player-start-timer (player)
  (bongo-mplayer-player-stop-timer player)
  (let ((timer (run-with-timer 0 1 'bongo-mplayer-player-tick player)))
    (bongo-player-put player 'timer timer)))

(defun bongo-mplayer-player-stop-timer (player)
  (let ((timer (bongo-player-get player 'timer)))
    (when timer
      (cancel-timer timer)
      (bongo-player-put player 'timer nil))))

(defun bongo-mplayer-player-tick (player)
  (cond ((not (bongo-player-running-p player))
         (bongo-mplayer-player-stop-timer player))
        ((not (bongo-player-paused-p player))
         (let ((process (bongo-player-process player)))
           (process-send-string
            process "pausing_keep get_time_pos\n")
           (when (null (bongo-player-total-time player))
             (process-send-string
              process "pausing_keep get_time_length\n"))))))

;;; XXX: What happens if a record is split between two calls
;;;      to the process filter?
(defun bongo-mplayer-process-filter (process string)
  (condition-case condition
      (let ((player (bongo-process-get process 'bongo-player)))
        (with-temp-buffer
          (insert string)
          (goto-char (point-min))
          (while (not (eobp))
            (cond ((looking-at "^ANS_TIME_POSITION=\\(.+\\)$")
                   (bongo-player-update-elapsed-time
                    player (string-to-number (match-string 1)))
                   (bongo-player-times-changed player))
                  ((looking-at "^ANS_LENGTH=\\(.+\\)$")
                   (bongo-player-update-total-time
                    player (string-to-number (match-string 1)))
                   (bongo-player-times-changed player)))
            (forward-line))))
    ;; Getting errors in process filters is not fun, so stop.
    (error (bongo-stop)
           (signal (car condition) (cdr condition)))))

(defun bongo-start-mplayer-player (file-name &optional extra-arguments)
  (let* ((process-connection-type nil)
         (arguments (append
                     (when bongo-mplayer-audio-driver
                       (list "-ao" bongo-mplayer-audio-driver))
                     (when bongo-mplayer-video-driver
                       (list "-vo" bongo-mplayer-video-driver))
                     (when bongo-mplayer-interactive
                       (list "-quiet" "-slave"))
                     (bongo-evaluate-program-arguments
                      bongo-mplayer-extra-arguments)
                     extra-arguments
                     (list file-name)))
         (process (apply 'start-process "bongo-mplayer" nil
                         bongo-mplayer-program-name arguments))
         (player
          (list 'mplayer
                (cons 'process process)
                (cons 'file-name file-name)
                (cons 'buffer (current-buffer))
                (cons 'interactive bongo-mplayer-interactive)
                (cons 'pausing-supported bongo-mplayer-interactive)
                (cons 'seeking-supported bongo-mplayer-interactive)
                (cons 'time-update-delay-after-seek
                      bongo-mplayer-time-update-delay-after-seek)
                (cons 'paused nil)
                (cons 'pause/resume 'bongo-mplayer-player-pause/resume)
                (cons 'seek-to 'bongo-mplayer-player-seek-to)
                (cons 'seek-unit 'seconds))))
    (prog1 player
      (set-process-sentinel process 'bongo-default-player-process-sentinel)
      (bongo-process-put process 'bongo-player player)
      (when bongo-mplayer-interactive
        (set-process-filter process 'bongo-mplayer-process-filter)
        (bongo-mplayer-player-start-timer player)))))

;;; Local Variables:
;;; coding: utf-8
;;; time-stamp-format: "%:b %:d, %:y"
;;; time-stamp-start: ";; Updated: "
;;; time-stamp-end: "$"
;;; time-stamp-line-limit: 20
;;; End:

(provide 'bongo-mplayer)
;;; bongo-mplayer.el ends here
