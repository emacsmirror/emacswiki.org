;;; oggel.el --- OGG encoding for EmacsLisp 

;; Copyright (C) 2003  Free Software Foundation, Inc.

;; Author: Mario Domgörgen <kanaldrache@gmx.de>
;; Keywords: ripping, encoding, ogg-vorbis

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
;; There seems no good tool in Emacs (except eshell :) to encode a cd
;; or single tracks to the great ogg-vorbis format.  Although there
;; are similar tool for mp3s like Monk or 'The Idiot Jukebox' i
;; decided to write my own major mode for that task as i find it
;; pretty hard to work through their code and understand it (as both
;; are very complex and not just ripping tools) and i always wanted
;; to program my own mode and learn from that.  Because of that you
;; will find a lot of little (or greater :) bugs and nasty code
;; here.  Feel free to work through the code and change everything you
;; want, send me a patch or a bugreport.
;; To install the code put it simply in your load-path and write that
;; piece in you .emacs:
;;
;; (add-hook 'cdi-mode-hook (lambda ()(require 'oggel)))
;;
;; Now you just type M-x cdi and gets to the tracklist.  You can
;; mark you favourite tracks with "m" and press "C" to start encoding.  If you
;; don't mark any track oggel will encode the whole cd for you! Yeah!
;; The files will be produced with the file name format
;; artist-album-tracknumber-songtitle.ogg unless you change
;; oggel-filename-list.  The standard options should work well on all
;; normal machine configurations (machines like mine).
;;
;; Oggel bases heavily on the great mode cdi.el from Matt Hodges for
;; playing the tracks or produce the tracklist.  You can get it from:
;; http://www.emacswiki.org/elisp/cdi.el
;;  
;;; History: 
;; 2003-08-24 A lot of very crude hacks. but i want a
;; working oggel.el again so don't look too exactly at the code ... i
;; will fix that... :)

;;; Code:

(require 'cl) 

(defgroup oggel nil
  "Mode for ripping and encoding tracks from a cd."
  :group 'multimedia 
  :group 'external)

(defcustom oggel-encoding-programm "/usr/bin/oggenc"
  "*Programm to encode tracks.
Be sure that the program is in your PATH or set it with its full
  path name."
  :group 'oggel
  :type 'file)

(defcustom oggel-ripping-programm "/usr/bin/cdda2wav"
  "*Programm to rip tracks.
Be sure that the program is in your PATH or set it with its full
path name."
  :group 'oggel 
  :type '(file :must-match t)) 

(defcustom oggel-encode-during-ripping t
  "If non-nil start encoding during ripping process.
Depends on how fast you have to be or if you still want to work on
  that machine."
  :group 'oggel
  :type 'boolean) 

(defcustom oggel-device "0,0,0"
  "Device for the cd to rip from.
You can get the value you need here with `cdrecord -scanbus'."
  :group 'oggel
  :type 'string)

(defcustom oggel-encoding-quality "5"
  "Quality for ogg-vorbis encoding.  
Vorbis' audio quality is not best measured in kilobits per second, but
on a scale from -1 to 10 called 'quality'. This change in terminology
was brought about by a tuning of the variable-bitrate algorithm that
produces better sound quality for a given average bitrate, but which
does not adhere as strictly to that average as a target. This new
scale of measurement is not tied to a quantifiable characteristic of
the stream, like bitrate, so it's a fairly subjective metric, but
provides a more stable basis of comparison to other codecs and is
relatively future-proof. As Segher Boessenkool explained, 'if you
upgrade to a new vorbis encoder, and you keep the same quality
setting, you will get smaller files which sound the same. If you keep
the same nominal bitrate, you get about the same size files, which
sound somewhat better.' The former behavior is the aim of the quality
metric, so encoding to a target bitrate is now officially deprecated
for all uses except streaming over bandwidth-critical connections.
For now, quality 0 is roughly equivalent to 64kbps average, 5 is
roughly 160kbps, and 10 gives about 400kbps. Most people seeking
very-near-CD-quality audio encode at a quality of 5 or, for lossless
stereo coupling, 6. The default setting is quality 3, which at
approximately 110kbps gives a smaller filesize and significantly
better fidelity than .mp3 compression at
128kbps. (http://www.vorbis.com/faq.psp#quality)."
  :group 'oggel
  :type 'integer)
  
(defcustom oggel-working-directory "~/ogg/"
  "Directory of the whole oggel process.
Oggel uses that directory for all its temporary working fiels and the
  produced ogg-files."
  :group 'oggel
  :type 'directory)

(defcustom oggel-ripping-switches 
  '("-D" oggel-device
    "-x" 
    "-t" tracknumber)
  "*Switches for the 'oggel-ripping-programm'.
You can use every function, variable or string, but you have to
  remember that the value are evaluated in the order as they
  appear." 
  :group 'oggel
  :type 'string)

(defcustom oggel-encoding-switches 
  '("-q" oggel-encoding-quality 
    "-a" oggel-artist 
    "-l" cdi-current-title 
    "-t" trackname 
    "-N" tracknumber
    "-d" (format-time-string "%d.%m.%Y"))
  "*Switches for the 'oggel-encoding-programm'.
You can use every function, variable or string, but you have to
  remember that the value are evaluated in the order as they
  appear."
  :group 'oggel 
  :type 'sexp)

(defcustom oggel-delete-temp-files t
  "If non-nil oggel will delete all temporary file like *inf.
In the future there should be oggel-temp-regexp if another
  ripping or encoding programm produces different files."
  :group 'oggel
  :type 'boolean)

(defcustom oggel-delete-wav t
  "If non-nil oggel delete all wav files after successful encoded it.
When no encoding process is started the wav files won't be deleted even
if this variable is set to t."
  :group 'oggel
  :type 'boolean)

(defcustom oggel-parallel-encoding 2
  "Number of tracks to encode synchronous.
Don't set this variable to high as you computer will be
extraordinary slow downed.  But if you don't mind set it to 99."
  :group 'oggel
  :type 'integer)

(defcustom oggel-filename-list 
  '(oggel-artist 
    cdi-current-title 
    tracknumber 
    trackname)
  "*This list controls the later file names of the encoded files.  
You can use every function, variable or string, but you have to remember
that the value are evaluated in the order as they appear.  The
elemenst will be concated with 'oggel-concat-char'."
  :group 'oggel
  :type '(repeat (choice :tag "Element of trackname"
		  (variable-item :format "%t\n" :tag "Artist" oggel-artist )
		  (variable-item :format "%t\n":tag "Title" cdi-current-title)
		  (variable-item :format "%t\n" :tag "Tracknumber" tracknumber)
		  (variable-item :format "%t\n" :tag "Trackname" trackname)
		  (function-item :format "%t\n" :tag "Date" '(format-time-string "%d_%m_%Y"))
		  (string :tag "User-customizable string")
		  (function :tag "User-customizable function")
		  (variable :tag "User-customizable variable")
		  )))

(defcustom oggel-concat-char "-" 
  "String which works as seperator for `oggel-filename-list'."
  :group 'oggel
  :type 'string)

(defcustom oggel-various-artists nil
  "* If non-nil, use 'various artists' as artist.
Otherwise try to get the real artist from the trackname."
  :group 'oggel
  :type 'boolean)

(defcustom oggel-eject-after-ripping nil
  "*If non-nil, eject cd after ripping process."
  :group 'oggel
  :type 'boolean)

;;internal variables

(defvar oggel-track-regexp "^[ ]*\\([0-9]+\\).*[ ]"
  "This regexp matches a track in *cdi* buffer.
Supported are both momentary cdi-programs cda and cdcd, but i made it
 a var to make is easier in further version to change it.  The first 
parens matches the tracknumber and the seconds the trackname.")

(defvar oggel-rip-list () 
  "Tracknumbers to rip.  
All tracknumbers are converted to have two digits")

(make-variable-buffer-local 'oggel-rip-list)

(defvar oggel-encode-list () 
  "Tracknumbers to encode.
All tracknumbers are converted to have two digits")

(make-variable-buffer-local 'oggel-encode-list) 

(defvar oggel-various-regexp '("Various Artists" "various artists" "diverse"))

;;Initialisation

(add-hook 'cdi-quit-hooks 'oggel-quit nil t)
(define-key cdi-mode-map (kbd "m") 'oggel-mark-track)
(define-key cdi-mode-map (kbd "C") 'oggel-start-ripping))

;; Functions

(defun oggel-quit ()
  "If there are some oggel-processes ask before quitting." 
  (let* (result
	 (living-processes (lambda ()
			     (dolist (process (process-list) result) 
			       (if (string-match "oggel" (process-name process)) 
				(setq result t))))))
    (and (if living-processes 
	     (yes-or-no-p "There are still some oggel processes. Really kill them and quit? "))
	 (cdi-quit)
	 )))

(defun oggel-normalize-number (number)
  "Create a two digit string number."
  (let ((number (number-to-string number)))
  (if (equal 1 (length number))
      (concat "0" number)
    number)))


(defun oggel-start-ripping (arg)
  "Start the ripping-process.  
If there are any marked tracks only rip and encode them. If not, rip
and encode all tracks.With prefix-argument not-nil, rip tracks but NOT
encode them." ;;future versions
  (interactive "P")
  (unless (file-directory-p oggel-working-directory)
    (make-directory oggel-working-directory))
  (unless (cdi-active-tracks)
    (cdi-toggle-all-tracks-ignored)) 
  (cdi-refresh-display-buffer) ; to clear the screen for oggel-find-highest-column
  (setq oggel-artist cdi-current-artist
	oggel-highest-column (1+ (oggel-find-highest-column))
	oggel-track-list (mapcar 
			  (lambda (list)
			    (oggel-generate-trackname list))
			  (cdi-active-tracks))
	oggel-rip-list (mapcar 'car oggel-track-list))
  (cdi-stop);Stop playing cd before ripping
  (oggel-rip-track (pop oggel-rip-list))
  )
           
(defun oggel-rip-track (tracknumber)
  (let ((proc (apply 'start-process (concat "oggel-ripping-" tracknumber)
		     "*cdi*" oggel-ripping-programm 
		     (nconc 
		      (mapcar 'eval oggel-ripping-switches)
		      (list (nth 3 (assoc tracknumber oggel-track-list))))
		     ))
	(inhibit-read-only t))
    (message (concat "Rip track: " tracknumber))
    (set-process-filter proc 'oggel-ripping-filter)
    (set-process-sentinel proc 'oggel-ripping-sentinel)
    (set-marker (process-mark proc) 
		(save-excursion
		  (set-buffer "*cdi*")
		  (goto-line 4)
		  (re-search-forward (car (rassoc (string-to-number tracknumber) cdi-track-alist)))
		  (move-to-column oggel-highest-column t)
		  (point)))
    
    proc))

(defun oggel-encode-track (tracknumber)
  "Start encoding track tracknumber."
  (let* ((trackname (nth 2 (assoc tracknumber oggel-track-list)))
	 (artist (nth 1 (assoc tracknumber oggel-track-list)))
	 (filename (nth 3 (assoc tracknumber oggel-track-list)))
	 (proc (apply 'start-process (concat "oggel-encoding-" tracknumber)
		      "*cdi*" oggel-encoding-programm 
		      (nconc 
		       (mapcar 'eval oggel-encoding-switches)
		       (list (concat filename ".wav")))))
	 (inhibit-read-only t))
    (message (concat "Encode track: " tracknumber))    
    (set-process-filter proc 'oggel-encoding-filter)
    (set-process-sentinel proc 'oggel-encoding-sentinel)
    (set-marker (process-mark proc) 
		(save-excursion 
		  (set-buffer "*cdi*") 
		  (goto-line 4)
		  (re-search-forward (car (rassoc (string-to-number tracknumber) cdi-track-alist)))
		  (move-to-column oggel-highest-column t)
		  (point)))
    proc))

(defun oggel-ripping-sentinel (proc string)
  "Start a new ripping process if PROC is successfully ended."
  (with-current-buffer (process-buffer proc)
    (let ((tracknumber (progn
	 		 (string-match "[0-9]+$" (process-name proc))
			 (match-string 0 (process-name proc)))))
      (if (string-match "finished\n" string)
	  (progn
	    (add-to-list 'oggel-encode-list tracknumber t)
	    (if oggel-rip-list 
		   (oggel-rip-track (pop oggel-rip-list))
		(if oggel-eject-after-ripping
		    (cdi-eject)))
	    (and (or 
		  (not oggel-rip-list) 
		  oggel-encode-during-ripping)
		 (oggel-encoding-test)
		 (oggel-encode-track (pop oggel-encode-list))
		 ))
	))))

	  
(defun oggel-encoding-sentinel (proc string)
  "Start a new encoding process if proc is succecssfully ended."
  (with-current-buffer (process-buffer proc)
    (let ((tracknumber (progn
			 (string-match "[0-9]+" (process-name proc))
			 (match-string 0 (process-name proc))))
	  (inhibit-read-only t))
      (if (string-match "finished\n" string)
	  (save-excursion 
	    (set-buffer (process-buffer proc))
	    (goto-char (process-mark proc))
	    (kill-line)
	    (insert (concat "progress of encoding: 100%")) ;a little hacky but oggenc does *NOT* return all percentages...
	    (if oggel-delete-temp-files 
		(delete-file 
		 (concat (nth 3 (assoc tracknumber oggel-track-list))  ".inf")))
	    (if oggel-delete-wav 
		(delete-file 
		 (concat (nth 3 (assoc tracknumber oggel-track-list)) ".wav")))
	    (if (and oggel-encode-list 
		 (oggel-encoding-test))
		 (oggel-encode-track (pop oggel-encode-list))
	      (cdi-refresh-display-buffer)
	      (message "Finished encoding!"))
	    )))))
		       	 
(defun oggel-ripping-filter (proc string)
  "Get procents of progess"
  (save-excursion
    (set-buffer (process-buffer proc))
    (let ((inhibit-read-only t))
	(if (string-match "[0-9]+%" string)
	    (progn (goto-char (process-mark proc))
		   (kill-region (point) (line-end-position))
		   (insert (concat "progress of ripping: " (match-string 0 string)))
		   (set-marker (process-mark proc) (re-search-backward  "progress of ripping: [0-9]+%" nil t))
		   )))))

(defun oggel-encoding-filter (proc string)
  "Get procents of progess"
  (save-excursion
    (set-buffer (process-buffer proc))
    (let ((inhibit-read-only t))
      (if (string-match "[0-9]+[\\.,][0-9]%" string)
	  (progn (goto-char (process-mark proc))
		 (kill-line)
		 (add-to-list 'mario string)
		 (insert (concat "progress of encoding: " (match-string 0 string)))
		 (set-marker (process-mark proc) (re-search-backward "progress of encoding: [0-9]+[\\.,][0-9]%" nil t)))))))
			       
(defun oggel-encoding-test ()
  "Test if there are oggel-parallel-encoding encoding processes."
  (let ((result 0))
    (> oggel-parallel-encoding
       (dolist (process (process-list) result)
	 (if (string-match "oggel-encoding" (process-name process))
	     (incf result)))
       )))

(defun oggel-mark-track ()
  "Mark track."
  (interactive)
  (if cdi-ignored-tracks
      (cdi-toggle-track-at-point-ignored)
    (cdi-toggle-all-tracks-ignored)
    (cdi-toggle-track-at-point-ignored)))

(defun oggel-find-highest-column ()
  "Returns the highest column in the track list to align process informations."
  (let ((column 0)) 
  (save-excursion 
    (goto-char (point-min)) 
    (while (re-search-forward "[ ]*[0-9]+.*[ ]\\{2\\}.*$" nil t)
      (if (> (current-column) column)
	  (setq column (current-column))))
    column)))			    

(defun oggel-generate-trackname (track)
  "Create tracknames"
  (let* (result
	 (tracknumber (oggel-normalize-number (cdr track)))
	 (various 
	    (dolist (pattern oggel-various-regexp result)
	      (if (string-match pattern cdi-current-artist)
		  (setq result t))))
	(oggel-artist (if various (split-string trackname " - ") cdi-current-artist))
	(trackname (if various (car (cdr (split-string track " - "))) (car track))))
    (list tracknumber oggel-artist trackname
    (concat oggel-working-directory (downcase 
     (replace-regexp-in-string "_$" ""		 
     (replace-regexp-in-string "_-" "-"		 
     (replace-regexp-in-string "__" "_" 	 
     (replace-regexp-in-string "[,.() '\"]" "_"  
			       (mapconcat 'eval oggel-filename-list "-"))))))))))

     	
(provide 'oggel)
;;; oggel.el ends here
