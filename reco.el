;;; reco.el --- 'Reco'gnize what is playing on a streaming audio
;;; channel.

;; Copyright (C) 2004  Yoni Rabkin Katzenell <yoni-r@actcom.com>
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Commentary:
;;
;; 'reco' establishes a TCP connection with the server and sends an
;; HTTP request string.  The server (hopefully) responds with some
;; header information describing the streaming audio channel, some
;; audio data and then the name of the song being played (usually in
;; that order).
;;
;; Some stations like WCPE [http://wcpe.org], while giving excellent
;; broadcasts do not support title streaming over MP3 or Ogg.  Using
;; this software on such stations will only result in general station
;; information and not the artist name or title of the track being
;; played.

;;; Functionality:
;;
;; Currently supports Icecast and Shoutcast servers with Ogg and MP3
;; streams.

;;; Important Notes:
;;
;; 1) This software does not parse, cache or save audio data at
;;    all. This software downloads between of 2k-65K Bytes of data
;;    from a given streaming audio channel per call. This software is
;;    optimized to download as little as possible from a given
;;    streaming audio channel and then to immediately disconnect.
;;
;; 2) This software disregards and then discards all audio data
;;    automatically after each call.
;; 
;; 3) This software connects for a maximum of 10 seconds and then
;;    immediately disconnects. Usually the software will disconnect
;;    long before the 10 second limit is reached.
;;
;; 4) It is the responsibility of the user to read the Terms of
;;    Service of the streaming audio channel before running this
;;    software on that channel's service. Some streaming audio
;;    channels explicitly request 3rd party applications not to
;;    connect to their service. This is their prerogative. Respect it.

;;; Use:
;;
;; To use eval (reco-get URL) where URL is a string.

;;; History:
;; 
;; 02/10/04 - First release "fluffy".
;; 07/10/04 - Second release "badger".

;; $Id: reco.el,v 1.25 2004/10/09 13:22:39 johnr Exp $

;;; Code:

(require 'url)
(require 'url-vars)
(require 'url-parse)

;; A higher value for this gives us a correspondingly higher chance of
;; grabbing the title information from a stream but incurs a price in
;; the additional time it takes to download more of the stream. Note:
;; This is not relevant for Ogg streams since the title info in Ogg
;; streams arrives almost immediately.
(defconst reco-max 60000
  "Byte limit for downloads.")

(defconst reco-timeout 10
  "Seconds to timeout connection (dead or alive).")

(defconst reco-verbose t
  "Output real-time information about the connection.")

(defconst reco-version
  "$Revision: 1.25 $"
  "Software version.")

(defconst reco-char-alter-regexp "[-,'=:%+&0-9A-Za-z\.()/ ]"
  "Unified character alternative clause for regular expressions.")

(defconst reco-shoutcast-regexp (concat reco-char-alter-regexp ".*?")
  "Regular expression for Shoutcast.")

(defconst reco-icecast-regexp (concat reco-char-alter-regexp "+")
  "Regular expression for Icecast.")

(defconst reco-shoutcast-title-regexp (concat "StreamTitle='\\(" reco-shoutcast-regexp "\\)';")
  "Regular expression for Shoutcast.")

;; Reference: http://www.xiph.org/ogg/vorbis/doc/framing.html
(defconst reco-icecast-capture-pattern "Oggs\\(.*\\)BCV"
  "Regular Expression for the beggining of an Ogg bitstream page.")

;; For all servers
(defconst reco-stream-header-regexp (concat reco-char-alter-regexp "+")
  "Regular expression for metainformation headers.")

;; When t output debugging info
(defconst reco-debugging nil
  "If t then reco will spill the stream into a buffer. Set to NIL
unless you want a buffer filled with binary junk.")

(defconst reco-debug-buffer "*reco-debug*"
  "Buffer for debugging information.")

(defconst reco-vocab (list "name" "genre" "pub" "metaint" "br" "bitrate" "description" "public" "audio-info")
  "List of header keys.")

(defvar reco-url nil
  "Server URL.")

(defvar reco-port nil
  "Server port.")

(defvar reco-found nil
  "Results of our search.")

(defvar reco-procname "reco-process"
  "Name of network connection process.")

(defvar reco-downloaded 0
  "Amount of stream data downloaded.")

(defvar reco-read-inhibit nil
  "When t do not attempt to read 'reco-found'.")

(defvar reco-return-hook nil
  "Activated after the disconnection from the streaming audio server.")

(defvar reco-read-hook nil
  "Activated after the disconnection from the streaming audio
server. This hook is for integration purposes, for general user
functions use 'reco-return-hook'.")

(defvar reco-header-flag nil
  "Non-nil means header information has been captured.")

(defvar reco-title-flag nil
  "Non-nil means title information has been captured.")

(defvar reco-request-string nil
  "String sent to streaming audio server.")

;; Output a human readable message
(defun reco-pretty-print (&optional string-out)
  "Output a human readable message. If STRING-OUT is non-nil, do
not output a message and only return a string."
  (let ((bitrate nil)
	(str nil)
	(title (reco-get-key "songtitle"))
	(name (reco-get-key "name")))

    ;; Protection from overflowing title information.
    (if (> (length title) 256)
	(setq title nil))

    ;; Compose bitrate, needs support for sampling rate as well.
    (cond ((reco-get-key "br")
	   (setq bitrate (concat "Bitrate: " (reco-get-key "br") "KB/sec")))
	  ((reco-get-key "bitrate")
	   (setq bitrate (concat "Bitrate: " (reco-get-key "bitrate") "KB/sec")))
	  (t (setq bitrate "Unknown bitrate")))

    ;; Compose message
    (cond ((reco-get-key "songtitle")
	   (setq str (format "Now playing: %s, Station: %s, %s" title name bitrate)))
	  (t (setq str (format "Could not get song title from station: %s" name))))

    ;; Either output a message or return a string.
    (if string-out
	str
      (message str))))

;; Useful
(defun list-to-string (l)
  "Return a STRING which is the concatenation of the elements of
L."
  (if (not l)
      nil
    (if (stringp (car l))
	(concat (car l) (list-to-string (cdr l)))
      (list-to-string (cdr l)))))

(defun reco-get-key (key)
  "Return STRING associated with KEY."
  (unless reco-read-inhibit
    (cdr (assoc key reco-found))))

(defun reco-get-keys (keys)
  "Return a list of strings associated with each key in
KEYS. KEYS should be a list of strings."
  (mapcar (lambda (e)
	    (reco-get-key e))
	  keys))

;; BEGIN to END should typically be a segment of about 250 Bytes
;; length for Ogg streams.
(defun reco-decode-ogg (begin end)
  "Parse Ogg stream segment from BEGIN to END."
  (let ((artist nil)
	(title nil))

    (goto-char begin)
    (re-search-forward (concat "artist=\\(" reco-icecast-regexp "\\)") end t)
    (setq artist (match-string-no-properties 1))
    
    (goto-char begin)
    (re-search-forward (concat "title=\\(" reco-icecast-regexp "\\)") end t)
    (setq title (match-string-no-properties 1))

    ;; ugh
    (if (or artist title)
	(list (cons "songtitle" (concat artist
					(if (and artist title)
					    " - "
					  " ")
					title))
	      (cons "artist" artist)
	      (cons "title" title))
      nil)))

;; BEGIN to END should be about 20 Bytes longs
(defun reco-decode-mp3 (begin end)
  "Parse Shoutcast/Icecast-MP3 segment from BEGIN to END."
  (let ((split nil)
	(songtitle nil)
	(artist nil)
	(title nil))

    (goto-char begin)
    (setq songtitle (buffer-substring begin end)
	  split (split-string songtitle "-"))

    (if (cdr split)
	(setq artist (car split)
	      title (list-to-string (cdr split))))

    (list (cons "songtitle" songtitle)
	  (cons "artist" artist)
	  (cons "title" title))))

(defun reco-filter (proc str)
  "Filter function for the network process.
Argument PROC Process.
Argument STR Quanta of data."

  ;; Debugging flag dependent
  (if reco-debugging
      (with-current-buffer reco-debug-buffer
	(insert str)))

  (with-temp-buffer
    (setq reco-downloaded (+ reco-downloaded (length str)))

    ;; Insert a quantum of data.
    (insert str)

    ;; Look for headers
    (unless reco-header-flag
      (mapcar (lambda (term)
		(goto-char (point-min))
		(if (re-search-forward (concat (regexp-opt (list "icy-" "ice-"))
					       term
					       ":\\(" reco-stream-header-regexp "\\)")
				       (point-max) t)
		    (progn
		      (add-to-list 'reco-found (cons term (match-string-no-properties 1)))
		      (setq reco-header-flag t))))
	      reco-vocab))

    ;; Look for title
    (unless reco-title-flag
      (goto-char (- (point)
		    (length str)))
      (cond ((re-search-forward reco-icecast-capture-pattern (point-max) t)
	     (setq reco-found (append reco-found (reco-decode-ogg (match-beginning 1) (match-end 1))))
	     (setq reco-title-flag t))
	    ;; In retrospect this section mimics input_http.c from
	    ;; the Xine project only that it uses buffer searching.
	    ((re-search-forward reco-shoutcast-title-regexp (point-max) t)
	     (setq reco-found (append reco-found (reco-decode-mp3 (match-beginning 1) (match-end 1))))
	     (setq reco-title-flag t)))))

  ;; Be chatty at the user
  (if reco-verbose
      (message "Connection %s. Downloaded %d/%d bytes."
	       (process-status proc) reco-downloaded reco-max))

  ;; Find out if we need to kill the connection
  (if (or (> reco-downloaded reco-max)
	  (and reco-header-flag reco-title-flag))
      (reco-kill-process proc)))

;; Closing the connection proves to be the most difficult part of the
;; program. There is a distinct difference in the behavior of stream
;; connection closing between 21.3.50.1 and 21.3.1. This seems to work
;; on both.
(defun reco-kill-process (proc)
  "Hold Emacs while trying to close the connection.
Argument PROC Process."
  (while (not (equal (process-status proc) 'closed))
    (delete-process proc))
  (if (process-filter proc)
      (set-process-filter proc nil)))

(defun reco-after-function ()
  "Evalutated when the connection ends."
  (setq reco-read-inhibit nil)
  (run-hooks 'reco-read-hook)
  (run-hooks 'reco-return-hook))

(defun reco-sentinel (proc ev)
  "Sentinel function for network process.
Argument PROC Process.
Argument EV Event string."
  
  (cond ((string-match "finished" ev)
	 (message "Finished getting stream information.")
	 (reco-after-function))
	
	((string-match "deleted" ev)
	 (message "Finished getting stream information.")
	 (reco-after-function))

	((string-match "broken" ev)
	 (message "Failed to get stream info: Peer broke connection.")
	 (reco-after-function))

	(t (princ (format "Process: %s had the event `%s'" proc ev)))))

(defun reco-make-request-string (file)
  "Return a valid HTTP request string with FILE as a URI."
  (concat "GET "
	  (if (equal file "")
	      "/"
	    file)
	  " HTTP/1.0\r\n"
	  "User-Agent: Free software, reads title of currently playing track (does not parse audio).\r\n"
	  "Icy-MetaData:1\r\n"
	  "\r\n"))

(defun reco-parse-url (urlstring)
  "Set the global variables for connecting to the streaming audio
server at URLSTRING."
  (let* ((url (url-generic-parse-url urlstring))
	 (hostname (elt url 3))
	 (port (elt url 4))
	 (file (elt url 5))
	 (protocol (elt url 0)))

    (cond ((or (not (equal protocol "http"))
	       (equal hostname ""))
	   (error "Invalid URL"))

	  ;; eg. "http://music.station.com:8014"
	  ((and (empty-string-p file)
		port)
	   (setq reco-port port))

	  ;; eg. "http://ogg.smgradio.com/vr96.ogg"
	  ((and (not (empty-string-p file))
		(or (equal port "")
		    (equal port nil)))
	   (setq reco-port 80))

	  ;; eg. "http://audio.ibiblio.org:8010/wcpe.ogg"
	  ((and (not (empty-string-p file))
		port)
	   (setq reco-port port))

	  (t (error "Invalid URL")))

    (setq reco-url hostname
	  reco-request-string (reco-make-request-string file))))

(defun empty-string-p (str)
  "Return t if STR is equal to the empty string."
  (equal str ""))

(defun reco-get (urlstring)
  "Get streaming audio server header metadata and song title from stream at URL.
Argument URLSTRING Address of streaming audio server as a string."
  (interactive)
  (reco-parse-url urlstring)

  ;; Reset state
  (setq reco-downloaded 0)     ;; restart fallback
  (setq reco-title-flag nil)   ;; forget title flag
  (setq reco-header-flag nil)  ;; forget header flag
  (setq reco-found nil)	       ;; forget output
  (setq reco-read-inhibit t)   ;; Do not read output vars until done

  ;; Debugging flag dependent
  (if reco-debugging
      (progn
	(if (get-buffer reco-debug-buffer)
	    (kill-buffer reco-debug-buffer))
	(get-buffer-create reco-debug-buffer)))

  ;; Open connection
  (open-network-stream reco-procname nil reco-url reco-port)
  ;; Connection timeone
  (run-at-time reco-timeout nil 'reco-kill-process (get-process reco-procname))
  ;; Start download
  (process-send-string reco-procname reco-request-string)
  (set-process-sentinel (get-process reco-procname) 'reco-sentinel)
  (set-process-filter (get-process reco-procname) 'reco-filter))

(provide 'reco)

;;; reco.el ends here

