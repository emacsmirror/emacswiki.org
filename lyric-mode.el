;;; lyric-mode.el --- Major mode for editing lyric (.lrc) files

;; Copyright (C) 2008  John Sturdy

;; Author: John Sturdy <john.sturdy@ul.ie>
;; Keywords: multimedia, hypermedia

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Edit lyric files, with playing of the music file at a controlled
;; speed and recording of the synchronization information.

;; This uses the external program `ogg123'.  Potentially, it could use
;; other, similar, programs for playing other types of music file.  It
;; assumes that the music file and the lyric file have the same
;; basename, e.g. another-gnu.ogg and another-gnu.lrc

;; If the file is already marked up with synchronization tags, an
;; overlay is displayed as the song is played.

;; Mostly, the mode is controlled by C-c C-whatever, but while the
;; song is playing, some other keys are redefined for easy capture of
;; the synchronization information.  The space bar inserts a
;; synchronization tag at point, and moves to the start of the next
;; line, and the return key moves to the start of the next line.
;; Lines matching `lyric-mode-skip-lines' (normally, blank lines) are
;; skipped by the space bar, so you get through marking a song with
;; just the space bar if there are no complications.

;; When point is on a tag, you can nudge the tag time backward or
;; forward by half a second using `<' and '>'.

;;; History:

;; Originally written 2008-08-16 by John C. G. Sturdy.

;;; Code:

(defgroup lyric-mode nil
  "Customization for lyric mode.")

(defcustom lyric-mode-skip-lines "^\\s-*$"
  "Regexp for lines to skip automatically.
This is normally set to skip blank lines.
You could also write one to skip titles, verse numbers, etc."
  :group 'lyric-mode
  :type 'regexp)

(defcustom lyric-tag-delta 0.5
  "Number of seconds by which to adjust tag times."
  :group 'lyric-mode
  :type 'number)

(defcustom lyric-ogg-command "ogg123"
  "The command to play an ogg file."
  :group 'lyric-mode
  :type 'string)

(defcustom lyric-ogg-command "mpg123"
  "The command to play an mp3 file.
This won't work yet;
more work is needed to support anything but ogg files."
  :group 'lyric-mode
  :type 'string)

(defcustom lyric-mode-hook nil
  "Functions to run on entering lyric mode."
  :group 'lyric-mode
  :type 'hook)

(defvar lyric-mode-slowdown 1
  "Slowdown for recording synchronization.")

(defvar lyric-mode-music-file nil
"The music file corresponding to this buffer.")

(make-variable-buffer-local 'lyric-mode-music-file)

(defvar lyric-mode-player nil
  "The shell command to use to play the music for this buffer.")

(make-variable-buffer-local 'lyric-mode-player)

(defvar lyric-mode-player-process nil
  "Player process for this buffer.")

(make-variable-buffer-local 'lyric-mode-player-process)

(defvar lyric-mode-latest-time-string "0:0"
  "The latest time observed back from the process, as a string.")

(make-variable-buffer-local 'lyric-mode-latest-time-string)

(defvar lyric-mode-latest-time-seconds 0
  "The latest time observed back from the process, as seconds.")

(make-variable-buffer-local 'lyric-mode-latest-time-seconds)

(defvar lyric-mode-stop-string nil
  "A string telling the player when to stop.
Normally nil, to let the player run on.")

(make-variable-buffer-local 'lyric-mode-stop-string)

(defvar lyric-mode-time-overlay nil
  "An overlay for showing where in the song the player is.")

(make-variable-buffer-local 'lyric-mode-time-overlay)

(defvar lyric-mode-sync-regexp
  "\\[\\(\\([0-9]+\\):\\([0-9.]+\\)\\)\\]"
  "Regexp to recognize a lyric synchronization tag.")

(defvar lyric-mode-ogg123-time-regexp
  "Time: \\(\\([0-9]+\\):\\([0-9.]+\\)\\)"
  "Regexp to get the time from ogg123.")

(defun lyric-mode-next-synchronization-tag (&optional from)
  "Move to the next synchronization tag, and return it in seconds.
Optional FROM says where to look from."
  (when from (goto-char from))
  (if (re-search-forward lyric-mode-sync-regexp (point-max) t)
      (progn
	(goto-char (match-beginning 0))
	(+ (* 60 (string-to-number (match-string-no-properties 2)))
	   (string-to-number (match-string-no-properties 3))))
    nil))

(defun lyric-mode-next-synchronization-tag-position (&optional from)
  "Return the position of the next synchronization tag.
Optional FROM says where to look from."
  (save-excursion
    (when from (goto-char from))
    (if (re-search-forward lyric-mode-sync-regexp (point-max) t)
	(match-beginning 0)
      (point-max))))

(defun lyric-mode-find-time (time &optional hint)
  "Find the start of the tag most nearly before TIME.
Optional HINT is a suggestion for where to start looking."
  ;; todo: something is going wrong for the last two lines of the song
  (unless hint (setq hint (point-min)))
  (let ((wrapped nil)
	(here hint)
	there
	scratch-time)
    (catch 'found
      (while t
	(cond
	 ((null (setq there (next-single-property-change here 'lyric-time)))
	  (when wrapped (throw 'found nil)) ; just to be sure
	  (setq there (point-min)
		wrapped t))
	 ((and (setq scratch-time (get-text-property there 'lyric-time))
	       (> scratch-time time))
	  (throw 'found here))
	 ((and wrapped
	       (>= there hint))
	  (throw 'found nil)))
	(setq here there)))))

(defun lyric-mode-prepare-time-data (&optional from)
  "Prepare time data in the buffer.
If FROM is given, start there, otherwise from the beginning."
  (unless from (setq from (point-min)))
  (save-excursion
    (goto-char from)
    (let* ((modified (buffer-modified-p))
	   (this-time (lyric-mode-next-synchronization-tag from))
	   (this-place (point))
	   next-time
	   next-place)
      ;; lyric-mode-next-synchronization-tag sets the match data as a
      ;; side effect
      (put-text-property (match-beginning 0) (match-end 0)
			 'keymap
			 lyric-mode-tag-keymap)
      (while (setq next-time (lyric-mode-next-synchronization-tag
			      (1+ this-place)))
	(setq next-place (point))
	(put-text-property (match-beginning 0) (match-end 0)
			   'keymap
			   lyric-mode-tag-keymap)
	(put-text-property this-place next-place
			   'lyric-time this-time)
	(setq this-time next-time
	      this-place next-place))
      (restore-buffer-modified-p modified))))

(defun lyric-mode-move-overlay ()
  "Move the time overlay to the current time in playing the song."
  (condition-case evar
      (let* ((start (lyric-mode-find-time
		     lyric-mode-latest-time-seconds))
	     (next-start (lyric-mode-next-synchronization-tag-position
			  (1+ start)))
	     (next-skip (save-excursion
			  (goto-char start)
			  (re-search-forward lyric-mode-skip-lines
					     (point-max) t)))
	     (end (if (and next-start next-skip)
		      (min next-start next-skip)
		    next-start)))
	(if (overlayp lyric-mode-time-overlay)
	    (move-overlay lyric-mode-time-overlay start end)
	  (setq lyric-mode-time-overlay (make-overlay start end))
	  (overlay-put lyric-mode-time-overlay
		       'face (cons 'background-color "yellow"))))
    (error nil)))

(defun lyric-mode-filter (process text)
  "Handle output of player PROCESS which is TEXT."
  (when (string-match lyric-mode-ogg123-time-regexp text)
    (setq lyric-mode-latest-time-string (match-string 1 text)
	  lyric-mode-latest-time-seconds
	  (+ (* 60
		(string-to-number (match-string 2 text)))
	     (string-to-number (match-string 3 text))))
    (lyric-mode-move-overlay)
    (force-mode-line-update)))

(defun lyric-mode-make-player-args (player file)
  "Make the player args list for PLAYER to play FILE."
  (cond
   ((string= lyric-mode-player "ogg123")
    (append
     (if lyric-mode-stop-string
	 (list "-K" lyric-mode-stop-string)
       nil)
     (list
      "-k" lyric-mode-latest-time-string
      "-y" (number-to-string lyric-mode-slowdown)
      file)))
   ((string= lyric-mode-player "mpg123")
    (error "Cannot do this with this player"))))

(defun lyric-mode-resume-playing ()
  "Resume playing of the associated music file."
  (interactive)
  (lyric-mode-stop-playing)		; stop any previous player
  (let* ((player-args (lyric-mode-make-player-args
		       lyric-mode-player
		       lyric-mode-music-file)))
    (setq lyric-mode-player-process
	  (apply 'start-process
		 (format "*Player for %s*" lyric-mode-music-file)
		 nil
		 lyric-mode-player
		 player-args)))
  (set-process-filter lyric-mode-player-process
		      (symbol-function 'lyric-mode-filter)))

(defun lyric-mode-full-speed ()
  "Set the speed to full, and continue."
  (interactive)
  (setq lyric-mode-slowdown 1)
  (lyric-mode-resume-playing))

(defun lyric-mode-half-speed ()
  "Set the speed to half, and continue."
  (interactive)
  (setq lyric-mode-slowdown 2)
  (lyric-mode-resume-playing))

(defun lyric-mode-third-speed ()
  "Set the speed to one-third, and continue."
  (interactive)
  (setq lyric-mode-slowdown 3)
  (lyric-mode-resume-playing))

(defun lyric-mode-quarter-speed ()
  "Set the speed to one-quarter, and continue."
  (interactive)
  (setq lyric-mode-slowdown 4)
  (lyric-mode-resume-playing))

(defun lyric-mode-start-playing ()
  "Start playing of the associated music file, from the beginning."
  (interactive)
  (setq lyric-mode-latest-time-string "0:0")
  (lyric-mode-resume-playing)
  (message (substitute-command-keys
	    "\\[lyric-mode-stop-playing] to stop playing; \
\\[lyric-mode-space-or-tag] to mark the time; \
\\[lyric-mode-newline-or-next-line] to move down")))

(defun lyric-mode-go ()
  "Resume playing, starting at the marker above point, if possible."
  (interactive)
  (save-excursion
    (when (re-search-backward lyric-mode-sync-regexp
			      (point-min) t)
      (setq lyric-mode-latest-time-string
	    (match-string-no-properties 1))))
  (lyric-mode-resume-playing))

(defun lyric-mode-play-line ()
  "Play between the two markers around point."
  (interactive)
  (save-excursion
    (when (re-search-backward lyric-mode-sync-regexp (point-min) t)
      (setq lyric-mode-latest-time-string (match-string-no-properties 1)))
    (forward-char 1)		       ; don't find the same one again
    (let ((lyric-mode-stop-string
	   (if (re-search-forward lyric-mode-sync-regexp
				  (point-max) t)
	       (match-string-no-properties 1)
	     nil)))
      (lyric-mode-resume-playing))))

(defun lyric-mode-stop-playing ()
  "Toggle playing of the associated music file."
  (interactive)
  (when (and (processp lyric-mode-player-process)
	     (eq (process-status lyric-mode-player-process) 'run))
    (kill-process lyric-mode-player-process))
  (setq lyric-mode-player-process nil)
  (message (substitute-command-keys
	    "\\[lyric-mode-resume-playing] to resume playing")))

(defun lyric-mode-toggle-playing ()
  "Toggle playing of the associated music file."
  (interactive)
  (if (and (processp lyric-mode-player-process)
	   (eq (process-status lyric-mode-player-process) 'run))
      (lyric-mode-stop-playing)
    (lyric-mode-start-playing)))

(defun lyric-mode-insert-synchronization-tag ()
  "Insert a synchronization tag at point."
  (interactive)
  (let ((start (point)))
    (insert "[" lyric-mode-latest-time-string "]")
    (lyric-mode-prepare-time-data start)))

(defun lyric-move-tag-at-point (delta)
  "Add DELTA to the time of the tag around point."
  (goto-char (previous-single-property-change
	      (point) 'keymap))
  (if (looking-at lyric-mode-sync-regexp)
      (let* ((old-time (+ (* 60 (string-to-number
				 (match-string-no-properties 2)))
			  (string-to-number
			   (match-string-no-properties 3))))
	     (new-time (+ old-time delta))
	     (minutes (floor (/ new-time 60.0)))
	     (seconds (floor (- new-time (* minutes 60))))
	     (centiseconds (* 100 (- new-time (* minutes 60) seconds)))
	     (new-time-string (format "[%02d:%02d.%d]"
				      minutes seconds centiseconds)))
	(replace-match (propertize new-time-string
				   'keymap
				   lyric-mode-tag-keymap)
		       t t)
	(goto-char (match-end 0)))
    (error "Not on a synchronization tag")))

(defun lyric-tag-move-earlier ()
  "Move the tag at point half a second earlier."
  (interactive)
  (save-excursion
    (lyric-move-tag-at-point (- lyric-tag-delta))
    (lyric-mode-play-line)))

(defun lyric-tag-move-later ()
  "Move the tag at point half a second later."
  (interactive)
  (save-excursion
    (lyric-move-tag-at-point lyric-tag-delta)
    (lyric-mode-play-line)))

(defun lyric-mode-space-or-tag ()
  "If currently playing, insert a tag, otherwise insert space."
  (interactive)
  (if lyric-mode-player-process
      (progn
	(lyric-mode-insert-synchronization-tag)
	(beginning-of-line 2)
	(when lyric-mode-skip-lines
	  (while (and (not (eobp))
		      (looking-at lyric-mode-skip-lines))
	    (beginning-of-line 2))))
    (insert " ")))

(defun lyric-mode-newline-or-next-line ()
  "If currently playing, move to the next line, else add newline."
  (interactive)
  (if lyric-mode-player-process
      (beginning-of-line 2)
    (newline)))

(defvar lyric-mode-map
  (let ((map (make-sparse-keymap "Lyric mode")))
    (define-key map "\C-c\C-p" 'lyric-mode-start-playing)
    (define-key map "\C-c\C-g" 'lyric-mode-go)
    (define-key map "\C-c\C-r" 'lyric-mode-resume-playing)
    (define-key map "\C-c\C-s" 'lyric-mode-stop-playing)
    (define-key map "\C-c\C-c" 'lyric-mode-toggle-playing)
    (define-key map "\C-c\C-l" 'lyric-mode-play-line)
    (define-key map "\C-c1" 'lyric-mode-full-speed)
    (define-key map "\C-c2" 'lyric-mode-half-speed)
    (define-key map "\C-c3" 'lyric-mode-third-speed)
    (define-key map "\C-c4" 'lyric-mode-quarter-speed)
    (define-key map " " 'lyric-mode-space-or-tag)
    (define-key map "\r" 'lyric-mode-newline-or-next-line)
    map)
  "Keymap for `lyric-mode'.")

(defvar lyric-mode-tag-keymap
  (let ((map (make-sparse-keymap "Lyric mode")))
    (define-key map "<" 'lyric-tag-move-earlier)
    (define-key map ">" 'lyric-tag-move-later)
    map)
  "Keymap for tag text in `lyric-mode'.")

(defun lyric-mode ()
  "Major mode for editing lyric files.
Commands are provided to start and stop a music player, and to insert
timestamp tags.

\\<lyric-mode-map>

Use \\[lyric-mode-start-playing] to start playing from the beginning.

Use the variable `lyric-mode-slowdown' to set the speed.

\\[lyric-mode-stop-playing] stops playing,
and \\[lyric-mode-resume-playing] resumes.

\\[lyric-mode-go] starts playing at the tag nearest above point.

When not playing, the space and return keys insert space and
newline; when playing, they insert a tag and move to the next
line, respectively, letting you move rapidly through a
ready-typed text to add a tag to each line.

Full commands are:
\\{lyric-mode-map}

While on a tag, you can adjust its time by `lyric-tag-delta' using
some extra commands:
\\{lyric-mode-tag-keymap}"
  (interactive)
  (fundamental-mode)
  (let* ((file-name-base (file-name-sans-extension
			  (buffer-file-name)))
	 (ogg (concat file-name-base ".ogg"))
	 (mp3 (concat file-name-base ".mp3")))
    (setq major-mode 'lyric-mode
	  mode-name "Lyric"
	  font-lock-defaults '(("\\[[0-9:.]+\\]") t))
    (unless (eq (car (cdr (cdr mode-line-modes)))
		'lyric-mode-latest-time-string)
      ;; todo: make this buffer-local
      (rplacd (cdr mode-line-modes)
	      (cons 'lyric-mode-latest-time-string
		    (cons " " (cdr (cdr mode-line-modes))))))
    (use-local-map lyric-mode-map)
    (cond
     ((file-exists-p ogg)
      (setq lyric-mode-music-file ogg
	    lyric-mode-player lyric-ogg-command))
     ((file-exists-p ogg)
      (setq lyric-mode-music-file mp3
	    lyric-mode-player lyric-mp3-command)))
    (lyric-mode-prepare-time-data)
    (message (substitute-command-keys
	      "\\[lyric-mode-start-playing] to start playing"))))

(add-to-list 'auto-mode-alist (cons "\\.lrc$" 'lyric-mode))

(provide 'lyric-mode)

;;; lyric-mode.el ends here
