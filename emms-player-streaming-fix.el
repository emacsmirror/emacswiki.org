;;; emms-player-streaming-fix.el --- mplayer url fix

;; Copyright (C) 2008 Free Software Foundation, Inc.

;; Author: Alessandro Di Marco <dmr@c0nc3pt.com>

;; This file is not part of EMMS.

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

;; This fixes subtitle check in streaming. Subtitle checking resorts to
;; `file-exists-p', leveraging on tramp. Unfortunately, this does not handle
;; URLs, so an error is triggered at each `emms-player-mplayer-start'
;; invocation.

;;; Code:

(emms-standard)
(emms-default-players)

(defcustom emms-player-mplayer-subtitle-check-in-streaming
  nil
  "Enable subtitle check in streaming."
  :type 'symbol
  :group 'emms)

(defun emms-url-p (name)
  (string-match-p 
   (concat "^" (regexp-opt '("http://" "https://"))) name))

(defun emms-url-or-file-exists-p (name)
  (if (emms-url-p name)
      (if emms-player-mplayer-subtitle-check-in-streaming
	  (cond ((string-match-p "^http://" name) (url-http-file-exists-p name))
		(t (url-https-file-exists-p name))))
    (file-exists-p name)))

(defun emms-player-mplayer-subtitle-checker ()
  (let* ((track (emms-playlist-current-selected-track))
         (name (emms-track-name track))
         (ext (file-name-extension name))
         (choices
          (emms-remove-if-not 'emms-url-or-file-exists-p
                              (mapcar (lambda (el)
                                        (emms-replace-regexp-in-string
                                         (concat ext "$") el name))
                                      emms-player-mplayer-subtitle-extensions)))
         (subtitle (mapconcat (lambda (el) el) choices ",")))
    (unless (string= subtitle "")
      (setq emms-player-mplayer-parameters
            (append emms-player-mplayer-parameters
                    (list "-sub" subtitle))))))

(defun emms-track-simple-description (track)
  "Simple function to give a user-readable description of a track.
If it's a file track, just return the file name.
Otherwise, return the type and the name with a colon in between."
  (if (eq 'file (emms-track-type track))
      (emms-track-name track)
    (format "%s [%s]"
	    (let ((name (emms-track-name track)))
	      (if (emms-url-p name)
		  (progn
		    (setq name (w3m-url-decode-string name))
		    (subst-char-in-string
		     ?/ ?>
		     (substring
		      name (+ (posix-string-match "/" name 7) 1)) t))
		name))
	    (symbol-name (emms-track-type track)))))

(provide 'emms-player-streaming-fix)
