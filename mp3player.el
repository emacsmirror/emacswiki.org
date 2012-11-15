;;; mp3player.el --- Interface to mpg123 or winamp
;;
;; Author & Maintainer: Jean-Philippe Theberge (jphil21@sourceforge.net)
;; Developers: Jean-Philippe Theberge (jphil21@sourceforge.net)
;;             Lucas Bonnet (lukhas@sourceforge.net)
;;
;; version :
(defconst mp3player-version "1.9")
;; last update: 08/05/2003

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1998 - 1999 Free Software Foundation, Inc.
;;
;; This file is not part of GNU Emacs. :-(
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Commentary:
;; 
;; Some codes and ideas borrowed from mpg123 by HIROSE Yuuji [yuuji@gentei.org]
;; and from mp3-tools by Steve Kemp <skx@tardis.ed.ac.uk>
;; Many Thanks to you Yuuji and Steve!!
;;
;; This package was developed and tested under GNU/Linux - Suze 6.2 and  
;; Emacs 20.5.1.  It was reported to work also under some version of
;; Xemacs with some errors at compile time.
;; 
;; [30/01/2003] - I'm now using it under Emacs 21.2.1 - Windows XP
;;                I made some changes in version 1.3 to support windows.
;;                I hate windows.
;;
;; The programs mpg123 and aumix are required.  They can be downloaded from:
;; 
;; http://www.mpg123.de/ and http://jpj.net/~trevor/aumix.html
;;
;; Windows user need to download mpg123 and the mixer program from
;; 
;; http://www3.tky.3web.ne.jp/~takuroho/files/mpg123/
;;
;; CAUTION: The mixer.exe program IS NOT and upgrade for mixer.exe already 
;;          present in your c:/WINDOWS directory.  DO NOT REPLACE.
;;
;; Developped while listening: Chara, the Butthole Surfers, Black Sabbath
;;                             and a lot of Ska!
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; History:
;; 
;; New to version 1.3: Support for the MS-windows OS
;;                     Thanks to the ntemacs mailing list. 
;;                     Some cosmetics changes.
;;
;; New to version 1.4: Skipping with '>' while the help page is displayed
;;                      update the "now playing" entry.
;;                     Fixed the randomizer. 
;;                     Somes minor bugs fixed
;;                     Now on SourceForge at emacsmp3player.sourceforge.net
;; 
;; New to version 1.5: mp3player-make-playlist-from-directory-with-genre
;;                      Go down a directory structure and add all mp3
;;                      with genre x to a given playlist.  This is cool!
;;                      
;;                     Stupid bug in 'mp3player-show-help' solved.
;;                     Début de francisation... 
;;                       set 'mp3player-interface-language'
;;
;; New to version 1.6: mp3player-export-playlist-to-b4s Convert a
;;                     playlist to the winamp's b4s format (xml)
;;                     Support for winamp as the external
;;                     program.(beta) id3 stored in struct instead of
;;                     list.  Include the concept of playrules (to
;;                     eventualy replace playlist)
;;                     dired-do-play-mp3-winamp use a temp .b4s file
;;                     
;; New to version 1.7: New customizable variable mp3player-path list
;;                     path where music is stored.  *  New command
;;                     'mp3player-play-all' just play everything that
;;                     can be found into the paths listed in the
;;                     variable above.  *  mp3player-play, when given a
;;                     list of files, will now work with Winamp (using
;;                     a temp playlist like dired-do-play-mp3) *
;;                     Playlist playing via
;;                     "mp3player-select-playlist' now work with
;;                     Winamp.  *  Support for Winamp m3u playlist format
;;                     (winamp 2) * Experimental function to view (and
;;                     edit) id3 tags for whole directory.  * Rewrote
;;                     mp3player-edit-tag for the new id3 internal
;;                     format * Set all line < 80 col.
;;
;; New to version 1.8  fix a bug in mp3player-dired-save-marked-as-playlist
;;                     control winamp via lukhas's command-line interface.
;;                     changed  mp3player-winamp-playlist-extension and  
;;                     mp3player-winamp-playlist-format from variable to
;;                     function so reconfiguration does'nt need reloading
;;                     Basic mood-based scoring system added.
;;                     Added mp3player-resume to restart playing after a 
;;                     mp3player-stop
;;
;; New to version 1.9  Fixed somes bugs in mp3player-scores.
;;
;;
;;; Code:
(require 'cl)
(require 'thingatpt)
(require 'dired)
(require 'wid-edit)
(require 'font-lock)


;; using windows?
(defconst micro~1-p (equal 'windows-nt system-type))

(defgroup mp3player nil
  "Interface to command line mp3 player."
  :group 'external)

(defcustom mp3player-program-id 
  (if micro~1-p 'winamp 'mpg123)
  "*Must match `mp3player-program'.  Can be 'mpg123  or 'winamp."
  :type 'string
  :options '(winamp 'mpg123)
  :group 'mp3player)

(defcustom mp3player-program 
  (if micro~1-p "C:/Program Files/Winamp3/Studio.exe" "mpg123")
  "*External mp3 player."
  :type 'string
  :group 'mp3player)

(defcustom mp3player-command-line-winamp-interface
  (if (eq mp3player-program-id 'winamp)
      "command-line.exe"
    nil)
  "Command line program to control winamp via emacs.  
Set to nil if you dont want/need to use that"
  :type 'string
  :group 'mp3player)

(defcustom mp3player-winamp-playlist-format 'b4s
  "Playlist format for Winamp,  of course you just dont care it you dont use 
Winamp!."
  :type 'string
  :options '(b4s m3u pls) ; pls not yet supported!
  :group 'mp3player)

(defcustom mp3player-mixer-program 
  (if micro~1-p "mixer" "aumix")
  "*External mixer program  (to adjust volume)."
  :type 'string
  :group 'mp3player)
;; NOTE: The program assume theses programs are used
;;       this variable is only to ajust their path of
;;       if, for some reasons, you change their name.
;;       I mean if you use some sound ajusting program
;;       you download from the net and just enter it's name
;;       here, Emacs will not automagicaly understant it's use
;;       and know what arguments to pass to it.
;;       However, you can tell me about this program and I can add 
;;       support for it (or you can add support for it and send me the new 
;;       mp3player.el!!
;;
;;       OTHER NOTE: The mixer.exe program for Windows is not the one you'll 
;;                   find in the WINDOWS directory; 
;;                   you should download it from :
;;                   http://www3.tky.3web.ne.jp/~takuroho/files/mpg123/
;;                   (dont replace the one from windows!!)
;;
(defcustom mp3player-playlists-directory "~/.emacs-mpg123/playlists"
  "*Directory to store playlists."
  :type 'string
  :group 'mp3player)

(defcustom mp3player-play-process-buffer "*Playing...*"
  "Name of process buffer."
  :type 'string
  :group 'mp3player)

(defcustom mp3player-volume-increment 5
  "Volume increment value."
  :type 'integer
  :group 'mp3player)

(defcustom mp3player-modeline-info-format " [%s - %a] "
  "Formating info for modeline.
NIL mean dont show info on the modeline.
else, the following apply:
    %s  Song name
    %a  Artist name
    %b  Album title
    %i  Additional info
    %y  Release year
    %g  Genre
    %f  Filename
    %r  Score
    %%  a literal %"
  :type 'string
  :group 'mp3player)

(defcustom mp3player-minibuffer-info-format " [%s - %a] "
  "Formating info for minibuffer.
NIL mean dont show info.
See 'mp3player-modeline-info-format' for formatting codes."
  :type 'string
  :group 'mp3player)

(defcustom mp3player-random-status nil
  "Default value for RANDOM."
  :type 'boolean
  :group 'mp3player)

(defcustom mp3player-song-info-buffer-name "*MP3 Info*"
  "Buffer name for showing info."
  :type 'string
  :group 'mp3player)

(defcustom mp3player-use-keypad-as-remote t
  "If you want to use the keypad as a remote control."
  :type 'boolean
  :group 'mp3player)

(defcustom mp3player-use-id3-for-description t
  "If t use id3 tag for file description, else 
it use the output of 'mp3player-program'."
  :type 'boolean
  :group 'mp3player)

(defcustom mp3player-temp-dir 
  (if micro~1-p "c:/windows/temp" "/tmp")
  "Temporary directory"
  :type 'string
  :group 'mp3player)


(defcustom mp3player-interface-language 'en
  "Language for interface and messages: supported value 'fr 'en"
  :type 'string
  :options '(fr en)
  :group 'mp3player)

(defcustom mp3player-path '("~/My Music")
  "List of paths where to find music."
  :type 'string
  :group 'mp3player)

(defcustom mp3player-use-scoring nil
  "use of the (experimental) scoring system"
  :type 'boolean
  :group 'mp3player)

(defstruct id3 song artist album note year genre file)

(defvar mp3player-temp-filename 
  (concat mp3player-temp-dir "/emacs-mp3player-tmp.mp3"))

(defun mp3player-winamp-playlist-extension ()
  (symbol-name mp3player-winamp-playlist-format))

(defun mp3player-temp-playlist  ()
  (concat mp3player-temp-dir "/emacs-mp3player-tmp."  
	  (mp3player-winamp-playlist-extension)))

(defvar mp3player-coding-system 'utf-8)

(defvar mp3player-now-playing nil)
(defvar mp3player-now-playing-filename nil)
(defvar mp3player-queued-files nil)
(defvar mp3player-played-files nil)
(defvar mp3player-mp3-file-regexp ".*\\.[Mm][Pp][23]$")
(defvar mp3player-url-regexp "^\\(f\\|ht\\)tp")
(defvar mp3player-remote-file-regexp "^/[^/:]*:")
(defvar mp3player-modeline-string nil)
(defvar mp3player-playlist-edition-font-lock-keywords nil)
(defvar mp3player-genre-table
  (list
   (cons 0  "Blues")
   (cons 1  "Classic Rock")
   (cons 2  "Country")
   (cons 3  "Dance")
   (cons 4  "Disco")
   (cons 5  "Funk")
   (cons 6  "Grunge")
   (cons 7  "Hip-Hop")
   (cons 8  "Jazz")
   (cons 9  "Metal")
   (cons 10  "New Age")
   (cons 11  "Oldies")
   (cons 12  "Other")
   (cons 13  "Pop")
   (cons 14  "R&B")
   (cons 15  "Rap")
   (cons 16  "Reggae")
   (cons 17  "Rock")
   (cons 18  "Techno")
   (cons 19  "Industrial")
   (cons 20  "Alternative")
   (cons 21  "Ska")
   (cons 22  "Death Metal")
   (cons 23  "Pranks")
   (cons 24  "Soundtrack")
   (cons 25  "Euro-Techno")
   (cons 26  "Ambient")
   (cons 27  "Trip-Hop")
   (cons 28  "Vocal")
   (cons 29  "Jazz+Funk")
   (cons 30  "Fusion")
   (cons 31  "Trance")
   (cons 32  "Classical")
   (cons 33  "Instrumental")
   (cons 34  "Acid")
   (cons 35  "House")
   (cons 36  "Game")
   (cons 37  "Sound Clip")
   (cons 38  "Gospel")
   (cons 39  "Noise")
   (cons 40  "Alt. Rock")
   (cons 41  "Bass")
   (cons 42  "Soul")
   (cons 43  "Punk")
   (cons 44  "Space")
   (cons 45  "Meditative")
   (cons 46  "Instrumental Pop")
   (cons 47  "Instrumental Rock")
   (cons 48  "Ethnic")
   (cons 49  "Gothic")
   (cons 50  "Darkwave")
   (cons 51  "Techno-Industrial")
   (cons 52  "Electronic")
   (cons 53  "Pop-Folk")
   (cons 54  "Eurodance")
   (cons 55  "Dream")
   (cons 56  "Southern Rock")
   (cons 57  "Comedy")
   (cons 58  "Cult")
   (cons 59  "Gangsta")
   (cons 60  "Top 40")
   (cons 61  "Christian Rap")
   (cons 62  "Pop/Funk")
   (cons 63  "Jungle")
   (cons 64  "Native US")
   (cons 65  "Cabaret")
   (cons 66  "New Wave")
   (cons 67  "Psychadelic")
   (cons 68  "Rave")
   (cons 69  "Showtunes")
   (cons 70  "Trailer")
   (cons 71  "Lo-Fi")
   (cons 72  "Tribal")
   (cons 73  "Acid Punk")
   (cons 74  "Acid Jazz")
   (cons 75  "Polka")
   (cons 76  "Retro")
   (cons 77  "Musical")
   (cons 78  "Rock & Roll")
   (cons 79  "Hard Rock")
   (cons 80  "Folk")
   (cons 81  "Folk-Rock")
   (cons 82  "National Folk")
   (cons 83  "Swing")
   (cons 84  "Fast Fusion")
   (cons 85  "Bebob")
   (cons 86  "Latin")
   (cons 87  "Revival")
   (cons 88  "Celtic")
   (cons 89  "Bluegrass")
   (cons 91  "Gothic Rock")
   (cons 92  "Progressive Rock")
   (cons 93  "Psychedelic Rock")
   (cons 94  "Symphonic Rock")
   (cons 95  "Slow Rock")
   (cons 96  "Big Band")
   (cons 97  "Chorus")
   (cons 98  "Easy Listening")
   (cons 99  "Acoustic")
   (cons 100  "Humour")
   (cons 101  "Speech")
   (cons 102  "Chanson")
   (cons 103  "Opera")
   (cons 104  "Chamber Music")
   (cons 105  "Sonata")
   (cons 106  "Symphony")
   (cons 107  "Booty Bass")
   (cons 108  "Primus")
   (cons 109  "Porn Groove")
   (cons 110  "Satire")
   (cons 111  "Slow Jam")
   (cons 112  "Club")
   (cons 113  "Tango")
   (cons 114  "Samba")
   (cons 115  "Folklore")
   (cons 116  "Ballad")
   (cons 117  "Power Ballad") ;; What is a power balad????
   (cons 118  "Rhytmic Soul")
   (cons 119  "Freestyle")
   (cons 120  "Duet")
   (cons 121  "Punk Rock")
   (cons 122  "Drum Solo")
   (cons 123  "Acapella")
   (cons 124  "Euro-House")
   (cons 125  "Dance Hall")
   (cons 126  "Goa")
   (cons 127  "Drum & Bass")
   (cons 128  "Club-House")
   (cons 129  "Hardcore")
   (cons 130  "Terror")
   (cons 131  "Indie")
   (cons 132  "BritPop")
   (cons 133  "Negerpunk")
   (cons 134  "Polsk Punk")
   (cons 135  "Beat")
   (cons 136  "Christian Gangsta Rap")
   (cons 137  "Heavy Metal")
   (cons 138  "Black Metal")
   (cons 139  "Crossover")
   (cons 140  "Contemporary Christian")
   (cons 141  "Christian Rock")
   (cons 142  "Merengue" )
   (cons 143  "Salsa")
   (cons 144  "Trash Metal")
   (cons 255  "<Error>")
   )
  "This list contains the standard description codes that are used for MP3's.")

(if mp3player-modeline-info-format
    (if (not (member 'mp3player-modeline-string global-mode-string))
	(setq global-mode-string 
	      (append global-mode-string 
		      (list 'mp3player-modeline-string)))))

(ignore-errors 
  (make-directory mp3player-playlists-directory t))

;; (if mp3player-use-keypad-as-remote
;;     (progn
;;       (global-set-key [kp-up] 'mp3player-increment-volume)
;;       (global-set-key [kp-down] 'mp3player-decrement-volume)
;;       (global-set-key [C-kp-begin] 'mp3player-stop)
;;       (global-set-key [kp-begin] 'mp3player-play-from-playlist)
;;       (global-set-key [C-kp-right] 'mp3player-skip)))


(if mp3player-use-scoring
    (require 'mp3player-scores))


;; above work on linux, below work on windows!
;; How to make it 'bilingual'?

(if mp3player-use-keypad-as-remote
    (progn
      (global-set-key [M-kp-add] 'mp3player-increment-volume)
      (global-set-key [M-kp-subtract] 'mp3player-decrement-volume)
      (global-set-key [C-kp-begin] 'mp3player-stop)
      (global-set-key [kp-begin] 'mp3player-play-from-playlist)
      (global-set-key [C-kp-right] 'mp3player-skip)))


(when (boundp 'toolbar-mode)
  (progn 
    (define-key global-map [tool-bar mp3player]
      '(menu-item 
	"mp3player" mp3player-select-playlist
	:image 
	(image :type xpm 
	       :file "/usr/X11/include/X11/3dpixmaps/small/small.Music.xpm")))
    (define-key global-map [tool-bar S-mp3player] 'mp3player-stop)
    (define-key global-map [tool-bar M-mp3player] 'mp3player-skip)))
  
(defun mp3player-multilingual (L)
  "Return the value associated with mp3player-interface-language in L"
  (cdr (or (assoc mp3player-interface-language L)
	   (assoc 'en L))))

(defun mp3player-message (L)
  "Like message but check the value of 
'mp3player-interface-language' to select the string 
to send in the alist L"
  ;; This use an alist like '((fr . "une chaine de caractères")
  ;;                          (en . "a string"))
  ;; the default value is en so it MUST be present.
  (message (mp3player-multilingual L)))
  
(defun mp3player-flatten (list)
  "Flatten LST.  (i.e.  (a (b) c) => (a b c)."
;;   (if (null lst)
;;       nil
;;     (if (listp (car lst))
;; 	(append (mp3player-flatten (car lst))
;; 		(mp3player-flatten (cdr lst)))
;;       (cons (car lst)
;; 	    (mp3player-flatten (cdr lst))))))

;; The flatten from gnus is better than mine, let's use it instead!
  (cond ((consp list)
	 (apply 'append (mapcar 'mp3player-flatten list)))
	(list
	 (list list))))

(defun mp3player-files-in-below-directory (directory)
  "List the mp3 files in DIRECTORY and in its sub-directories."
  ;; Derived from the 'emacs-lisp-intro' info file.
  (let (mp3-files-list
        (current-directory-list
         (directory-files-and-attributes directory t)))
    (while current-directory-list
      (cond
       ((equal ".mp3" (substring (car (car current-directory-list)) -4))
        (setq mp3-files-list
              (cons (car (car current-directory-list)) mp3-files-list)))
       ((eq t (car (cdr (car current-directory-list))))
        (if
            (equal (or "." "..")
                   (substring (car (car current-directory-list)) -1))
            ()
          (setq mp3-files-list
                (append
                 (mp3player-files-in-below-directory
                  (car (car current-directory-list)))
                 mp3-files-list)))))
      (setq current-directory-list (cdr current-directory-list)))
    mp3-files-list))
		 
(defun mp3player-randomize-list (l)
  "Shuffle the content of L randomly."
  (mapcar
   'cdr
   (sort*
    (mapcar
     (lambda (x)
       (cons (random 1000000) x)) l) '>
    :key  'car)))

(defun mp3player-remote-file-p (filename)
  "Return t if FILENAME is a valid remote file name for ange-ftp." ;; or tramp?
  (string-match mp3player-remote-file-regexp filename))

(defun mp3player-trim-right (string)
  "Trim all space from the right of STRING."
  (let ((l (- (length string) 1)))
    (cond ((< l 1) "")
	  ((equal 32 (elt string l)) (mp3player-trim-right 
				      (substring string 0 l)))
	  (t string))))

(defun mp3player-dump-to-winamp-playlist-and-play (L)
  (with-temp-buffer
    (insert
     (mp3player-list-to-winamp-playlist-str
      L))
    (write-file (mp3player-temp-playlist)))
  (mp3player-start-playing-process (mp3player-temp-playlist)))

;;;###autoload
(defun mp3player-play (f &optional random)
  "Play F.  It can be a file (local or remote), a list of file or a playlist.
Play randomly if RANDOM is t."
  (interactive "sPlay what? ")
  (if (listp f)
      (let ((L (if random (mp3player-randomize-list f) f)))
	(if (equal mp3player-program-id 'winamp)
	    (mp3player-dump-to-winamp-playlist-and-play L)
	  (mp3player-play-files-or-urls L)))
    (if (string-match mp3player-mp3-file-regexp f)
	(if (string-match mp3player-url-regexp f) 
	    (mp3player-play-url f)(mp3player-play-file f))
      (if (file-exists-p (concat mp3player-playlists-directory "/" f))
	  (mp3player-play-files-or-urls 
	   (if random (mp3player-randomize-list (mp3player-read-playlist f)) 
	     (mp3player-read-playlist f)))))))

;; (defun mp3player-play (f &optional random)
;;   "Play F.  It can be a file (local or remote), a list of file or a playlist.
;; Play randomly if RANDOM is t."
;;   (interactive "sPlay what? ")
;;   (if (listp f)(mp3player-play-files-or-urls 
;; 		(if random (mp3player-randomize-list f) f))
;;     (if (string-match mp3player-mp3-file-regexp f)
;; 	(if (string-match mp3player-url-regexp f) 
;; 	    (mp3player-play-url f)(mp3player-play-file f))
;;       (if (file-exists-p (concat mp3player-playlists-directory "/" f))
;; 	  (mp3player-play-files-or-urls 
;; 	   (if random (mp3player-randomize-list (mp3player-read-playlist f)) 
;; 	     (mp3player-read-playlist f)))))))



(defun mp3player-play-file (f)
  "Play the file F."
  (if (or (not mp3player-use-scoring)(mp3player-check-score f))
      (cond ((and (file-exists-p f)(string-match mp3player-mp3-file-regexp f))
	     (progn
	       (setq mp3player-now-playing-filename f
		     mp3player-now-playing (mp3player-get-id3-tag-from-file f))
	       (mp3player-start-playing-process f)
	       (mp3player-alter-modeline)
	       t))
	    ((mp3player-remote-file-p f)
	     (progn 
	       (mp3player-message 
		'((en . "Copying file... Please wait...")
		  (fr . "Copie du fichier... Veuillez Patienter...")))
	       (if (file-exists-p mp3player-temp-filename)
		   (delete-file mp3player-temp-filename))
	       (copy-file f mp3player-temp-filename)
	       (mp3player-play mp3player-temp-filename)
	       t)))))

(defun mp3player-resume ()
  (interactive)
  (mp3player-play-files mp3player-queued-files))
  
(defun mp3player-play-all ()
  (interactive)
  (mp3player-play
   (mp3player-flatten 
    (mapcar 'mp3player-files-in-below-directory 
	    mp3player-path))
   mp3player-random-status))

(defun mp3player-format-info-string (str)
  "Return a string with %x element from STR replaced by their equivalent.
See 'mp3player-modeline-info-format' for description of %x element."
  (mapcar
   (lambda (x)
     (let ((result "")
	   (start 0)
	   mb me)
       (while (string-match (car x) str start)
	 (setq mb (match-beginning 0)
	       me (match-end 0)
	       result (concat result (substring str start mb) (cdr x))
	       start me
	       str (concat result (substring str start))))))
   `(("%s" . ,(car mp3player-now-playing))
     ("%a" . ,(nth 1 mp3player-now-playing))
     ("%b" . ,(nth 2 mp3player-now-playing))
     ("%i" . ,(nth 3 mp3player-now-playing))
     ("%y" . ,(nth 4 mp3player-now-playing))
     ("%g" . ,(cdr (assoc (nth 5 mp3player-now-playing) 
			  mp3player-genre-table)))
     ("%f" . ,(nth 6 mp3player-now-playing))
					;     ("%t" . ,mp3player-time)
     ("%%" . "%")))
  str)

(defun mp3player-alter-modeline ()
  "Update the modeline with song info."
  (if (and mp3player-modeline-info-format
	   (not (null mp3player-now-playing))
	   (listp mp3player-now-playing))
      (setq mp3player-modeline-string (mp3player-format-info-string 
				       mp3player-modeline-info-format))))

(defun mp3player-play-url (url)
  "Play URL."
  (mp3player-start-playing-process url)
  (setq mp3player-now-playing "remote file"))

(defun mp3player-play-files-or-urls (foul)
  "Play, in turn, all the elements of FOUL.  They can be files or urls."
  (if foul
      (progn
	(if (string-match "^\\(f\\|ht\\)tp" (car foul))
	    (mp3player-play-url (car foul))
	  (if (not (mp3player-play-file (car foul)))
	      (mp3player-play-files-or-urls (cdr foul))))
	(setq mp3player-queued-files (cdr foul)))))

(defun mp3player-play-from-playlist (playlist &optional random)
  "Play the given PLAYLIST.  If RANDOM is t, play it randomly."
  (interactive (list 
		(completing-read "Playlist : " 
				 (map 'list 'list (mp3player-playlists)))))
  (let ((p (concat mp3player-playlists-directory "/" playlist)))
    (if (equal mp3player-program-id 'winamp)
	(let ((p2 (concat (file-name-sans-extension p) (concat "." (mp3player-winamp-playlist-extension)))))
	  (if (file-exists-p p2)
	      (mp3player-start-playing-process p2)
	    (progn 
	      (mp3player-export-playlist-to-winamp playlist)
	      (write-file p2)
	      (kill-buffer (current-buffer))
	      (mp3player-start-playing-process p2))))
      (with-temp-buffer
	(insert-file p)
	(let ((pl (split-string (buffer-substring (point-min) 
						  (point-max)) "\n")))
	  (mp3player-play-files-or-urls 
	   (if random (mp3player-randomize-list pl) pl)))))))
  
(defun mp3player-read-playlist (playlist)
  "Convert PLAYLIST (a file) into a list for internal processing."
  (with-temp-buffer
    (insert-file (concat mp3player-playlists-directory "/" playlist))
    (split-string (buffer-substring (point-min) (point-max)) "\n")))

(defalias 'mp3player-play-files 'mp3player-play-files-or-urls)
(defalias 'mp3player-play-urls 'mp3player-play-files-or-urls)

;; (defun mp3player-start-playing-process (f)
;;   "Initialize the playing process of file F."
;;   (let ((process (apply 'start-process "mp3player-process" mp3player-play-process-buffer
;; 			 mp3player-program
;; 			 (if (equal mp3player-program-id 'mpg123) "-v" " ")
;; 			 (list f))))
;;     (set-process-filter process 'mp3player-play-filter)
;;     (set-process-sentinel process 'mp3player-play-sentinel)))


(defun mp3player-start-playing-process (f)
  "Initialize the playing process of file F."
  (let ((process
	 (eval (mp3player-flatten (list 'start-process
					"mp3player-process"
					mp3player-play-process-buffer
					mp3player-program
					(if (equal mp3player-program-id 
						   'mpg123) 
					    "-v" " ")
					f)))))
    (set-process-filter process 'mp3player-play-filter)
    (set-process-sentinel process 'mp3player-play-sentinel)

(defun mp3player-dump-to-winamp-playlist-and-play (L)
  (with-temp-buffer
    (insert
     (mp3player-list-to-winamp-playlist-str
      L))
    (write-file (mp3player-temp-playlist)))
  (mp3player-start-playing-process (mp3player-temp-playlist))))
  f)

(defun mp3player-play-filter (proc string) nil)

;;;###autoload
(defun dired-do-play-mp3 (arg)
  "Play the marked files.  If ARG is t, play randomly."
  (interactive "P")
  (if (equal mp3player-program-id 'winamp)
      (dired-do-play-mp3-winamp arg)
    (let ((L (dired-get-marked-files)))
      (mp3player-play-files
       (if arg (mp3player-randomize-list L) L)))))

(defun dired-do-play-mp3-winamp (arg)
  (interactive "P")
  (let* ((L (dired-get-marked-files))
	(rL (if arg (mp3player-randomize-list L) L)))
    (mp3player-dump-to-winamp-playlist-and-play rL)))

(defun mp3player-dired-save-marked-as-playlist (name)
  "Save marked files in playlist NAME."
  (interactive "sPlaylist name ")
  (let ((fl (dired-get-marked-files)))
    (find-file (concat  mp3player-playlists-directory "/" name ".epl"))
    (mapcar (lambda (x)
	      (insert (concat x "\n")))
	    fl)
    (save-buffer)
    (kill-buffer (current-buffer))))

(defun mp3player-make-playlist-from-directory (directory playlist)
  "Add the content of DIRECTORY (and included subdir) to PLAYLIST."
  (interactive "DDirectory: \nsPlaylist: ")
  (let ((fl (mp3player-files-in-below-directory directory)))
    (find-file (concat  mp3player-playlists-directory "/" playlist))
    (mapcar (lambda (x)
	      (insert (concat x "\n")))
	    fl)
    (save-buffer)
    (kill-buffer (current-buffer))))

(defun mp3player-make-playlist-from-directory-with-genre (directory playlist)
  "Add the content of DIRECTORY (and included subdir) to PLAYLIST."
  (interactive "DDirectory: \nsPlaylist: ")
  (let* ((fl (mp3player-files-in-below-directory directory))
	 (genre (completing-read "Genre: "
				 (mapcar
				  (lambda (x)
				    (list (cdr x)))
				  mp3player-genre-table))))
    (find-file (concat  mp3player-playlists-directory "/" playlist))
    (mapcar (lambda (x)
	      (let ((gnr (nth 5 (mp3player-get-id3-tag-from-file x))))
		(if (equal gnr genre)
		    (insert (concat x "\n")))))
	    fl)
    (save-buffer)
    (kill-buffer (current-buffer))))

(defun mp3player-play-sentinel (process string)
  "Sentinel watching the mpg123 process.  At the end of it, it restart a 
new one with the next song in the playlist.  Args are PROCESS and STRING."
;; This is buggy with windows... 
  (message (symbol-name (process-status process)))
  (if (eq (process-status process) 'exit)
      (progn
	(add-to-list 'mp3player-played-files 
		     (if (listp mp3player-now-playing)
			 (car (last mp3player-now-playing))
		       mp3player-now-playing))
	(setq mp3player-now-playing nil
	      mp3player-modeline-string nil)
	(mp3player-alter-modeline)
	(if mp3player-queued-files
	    (mp3player-play-files mp3player-queued-files)))))

(defun mp3player-insert-url-content (url)
  "Insert the contents of URL at point."
  (ignore-errors (kill-buffer "*mp3player-temp-buffer*"))
  (let* ((splitted (split-string url "/"))
	 (host (caddr splitted))
	 (path (apply 'concat (map 'list
				   (lambda (x)
				     (concat x "/"))
				   (cdddr splitted))))
	 (coding-system-for-read 'binary)
         (coding-system-for-write 'binary)
         (http (open-network-stream
                "mp3player-retrieval-process"
		"*mp3player-temp-buffer*"
                host
		80))
	 (pbuf (process-buffer http)))
    (process-send-string
     http (concat "GET /" path " HTTP/1.0\r\n\r\n"))
    (while (eq (process-status http) 'open)
      (sleep-for 1))
    (insert-buffer pbuf)
    (kill-buffer pbuf)))

(defun mp3player-toggle-randomness ()
  "Toggle randomness for playlist playing."
  (interactive)
  (if mp3player-random-status
      (setq mp3player-random-status nil)
    (progn
      (setq mp3player-random-status t)
      (if mp3player-queued-files
	  (setq mp3player-queued-files
		(mp3player-randomize-list 
		 mp3player-queued-files)))))
  (mp3player-show-random-status))

(defun mp3player-show-random-status ()
  "Show the random status."
  (interactive)
  (mp3player-message
   (if mp3player-random-status
       '((en . "Random is on")
	 (fr . "Ordre aléatoire activé"))
     '((en . "Random is off")
       (fr . "Ordre aléatoire désactivé")))))

(defun mp3player-stop ()
  "Stop the mpg123 subprocess, if any."
  (interactive)
  (ignore-errors
    (progn
      (delete-process "mp3player-process")
      (kill-buffer mp3player-play-process-buffer)))
  (setq mp3player-now-playing nil)
  (setq mp3player-modeline-string nil)
  (mp3player-alter-modeline))


(defun mp3player-skip ()
  "Stop playing the current song and skip to the next in memory playlist."
  (interactive)
  (if (equal mp3player-program-id 'winamp)
      (if mp3player-command-line-winamp-interface
	  (shell-command (concat mp3player-command-line-winamp-interface " -n")))
    (let ((lst mp3player-queued-files))
      (mp3player-stop)
      (if lst (mp3player-play-files-or-urls lst)))))

(defun mp3player-playlists ()
  "Return the list of availables playlists."
  (directory-files mp3player-playlists-directory nil "^[^\.].*\.epl$"))

(defun mp3player-local-filelist (path)
  "Return a list of mp3 available at PATH."
  (reverse (directory-files path t "mp3$")))

(defun mp3player-select-local-filename (path)
  "Display a list of available mp3 in PATH in a separate buffer."
  (interactive "FFilename: ")
  (let ((filesL (mp3player-local-filelist path)))
    (pop-to-buffer "*mp3player-files-selector*")
    (map 'list (lambda (x) (insert (concat x "\n"))) filesL)))

(defun mp3player-remote-filelist-via-http (url)
  "Return a list of mp3 available at URL.
It is asumed that URL point to a directory with no index.html and
the resulting page is a standard apache file listing."
  (with-temp-buffer
    (mp3player-insert-url-content url)
    (goto-char (point-min))
    (let ((L nil))
      (while (re-search-forward "A HREF=\"\\(.*.mp3\\)\"" nil t)
	(add-to-list 'L
		     (concat url
			     (buffer-substring
			      (match-beginning 1)
			      (match-end 1)))))
      L)))

(defun mp3player-select-remote-filename (url)
  "List the file available at URL in a separate buffer.
It is asumed that URL point to a directory with no index.html and
the resulting page is a standard apache file listing."
  (interactive "sUrl: ")
  (let ((filesL (mp3player-remote-filelist-via-http url)))
    (pop-to-buffer "*mp3player-files-selector*")
    (map 'list (lambda (x) (insert (concat x "\n"))) filesL)))
 
(define-derived-mode mp3player-playlist-edition-mode text-mode 
  "Playlist edition"
  "Major mode for editing mpg123 playlists.
Special commands:
\\{mp3player-playlist-edition-mode-map}"
  (setq mp3player-playlist-edition-font-lock-keywords
	(list
	 '("^#.*$" . font-lock-comment-face)))
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults 
	'(mp3player-playlist-edition-font-lock-keywords nil t)))

(defun mp3player-edit-playlist (playlist)
  "Open PLAYLIST in a buffer for edition."
  (interactive (list 
		(let ((default-directory mp3player-playlists-directory)
		      (insert-default-directory nil))
		  (read-file-name "File: "))))
  (find-file (concat mp3player-playlists-directory "/" playlist))
  (mp3player-playlist-edition-mode))

(defun mp3player-play-mp3-at-point ()
  "If the thing at point is a mp3 filename or url, play it."
  (interactive)
  (let* ((file-or-url 
	  (if 
	      (thing-at-point-looking-at mp3player-remote-file-regexp) 
	      'url 
	    'filename))
	 (thing (thing-at-point file-or-url)))
    (if (not (string-match mp3player-mp3-file-regexp thing))
	(mp3player-message '((en . "File is not a mp3")
			     (fr . "Ce fichier n'est pas un mp3")))
      (if (string-match mp3player-remote-file-regexp thing)
	  (mp3player-play-url thing)
	(if (file-exists-p (expand-file-name thing))
	    (mp3player-play-file (expand-file-name thing))
	  (mp3player-message 
	   `((en . ,(concat "File " thing " not found"))
	     (fr . ,(concat "Le fichier " thing " est introuvable")))))))))

(defun mp3player-select-playlist ()
  "Display a list of available playlist as witgets.
Play the one clicked and close the buffer."
  (interactive)
  (require 'widget)
  (let ((bufname "*Mp3player - playlists*"))
    (pop-to-buffer bufname)
    (erase-buffer)
    (widget-minor-mode 1)
    (mapcar
     (lambda (x)
       (lexical-let ((x x))
	 (widget-create 'push-button
			:notify (lambda  (widget &rest ignore)
				  (kill-buffer (current-buffer))
				  (mp3player-play-from-playlist 
				   x 
				   mp3player-random-status))
			(file-name-sans-extension x))
	 (widget-insert "\n")))
     (mp3player-playlists))
    (widget-setup)))

(defun mp3player-play-all-files-from-url (url &optional random)
  "Play all the files at URL.  If RANDOM is t, play them randomly."
  (interactive "sUrl: ")
  (let ((filename (concat "." (prin1-to-string (gensym))".tmp")))
    (mp3player-select-remote-filename url)
    (write-file (concat mp3player-playlists-directory "/" filename))
    (kill-buffer (current-buffer))
    (mp3player-play-from-playlist filename random)))
  
(defun mp3player-play-all-files-from-url-randomly (url)
  "Play all the files at URL randomly."
  (interactive "sUrl: ")
  (mp3player-play-all-files-from-url url t))

(defun mp3player-change-volume (num)
  "Ajust volume using 'mp3player-mixer-program' by NUM.
A negative number mean to lower the volume."
  (if micro~1-p 
      (start-process 
       "mixer-process" nil mp3player-mixer-program " vol"
       (int-to-string   
	(+ num
	   (string-to-number 
	    (let ((str 
		   (shell-command-to-string mp3player-mixer-program)))
	      (string-match "\\([0-9]+\\):" str)
	      (substring str (match-beginning 1)(match-end 1)))))))
    (start-process "mixer-process" nil mp3player-mixer-program "-v"
		   (concat (if (wholenump num) "+" "-")
			   (int-to-string (abs num))))))

(defun mp3player-increment-volume ()
  "Ajust volume up by default increment."
  (interactive)
  (mp3player-message '((en . "Volume up")(fr . "Plus fort")))
  (mp3player-change-volume mp3player-volume-increment))

(defun mp3player-decrement-volume ()
  "Ajust volume down by default increment."
  (interactive)
  (mp3player-message '((en . "Volume down")(fr . "Moin fort")))
  (mp3player-change-volume (- 0 mp3player-volume-increment)))


;;;###autoload
(defun mp3player (action)
  "Main interface to mp3player.  
ACTION is a one key shortcut to a major functionality."
  (interactive "cps>qx+-? ")
  (let ((help-buffer-name "*mp3player help*"))
    (cond ((equal action ?+)
	   (mp3player-increment-volume)(call-interactively 'mp3player))
	  ((equal action ?-)
	   (mp3player-decrement-volume)(call-interactively 'mp3player))
	  ((equal action ?p)(mp3player-select-playlist))
	  ((equal action ?r)
	   (mp3player-toggle-randomness)
	   (if (member help-buffer-name (mapcar 'buffer-name (buffer-list)))
	       (progn (mp3player-show-help help-buffer-name)
		      (call-interactively 'mp3player))
	     (call-interactively 'mp3player)))
	  ((equal action ?e)
	   (progn
	     (mp3player-edit-tag-of-current)(call-interactively 'mp3player)))
 	  ((equal action ?s)
	   (progn
	     (mp3player-display-info-of-current)(call-interactively 'mp3player)))
	  ((equal action ?>)
	   (progn
	     (mp3player-skip)
	     (if (member help-buffer-name 
			 (mapcar 'buffer-name (buffer-list)))
		 (progn (kill-buffer help-buffer-name)
			(mp3player-show-help help-buffer-name)))
	     (call-interactively 'mp3player)))
	  ((equal action ?x)(mp3player-stop))
	  ((equal action ??)
	   (if (member help-buffer-name (mapcar 'buffer-name (buffer-list)))
	       (progn (kill-buffer help-buffer-name)
		      (call-interactively 'mp3player))
	     (progn
	       (mp3player-show-help help-buffer-name)
	       (call-interactively 'mp3player))))
	  ((not (equal action ?q))(call-interactively 'mp3player))
	  (t (if (member help-buffer-name (mapcar 'buffer-name (buffer-list)))
		 (kill-buffer help-buffer-name))))))

  
(defun mp3player-show-help (name)
  "Show a help page in buffer NAME."
  (switch-to-buffer name)
  (delete-region (point-min)(point-max))
  (insert
   (mp3player-multilingual
    `((en . ,(concat "MP3 Player -- Help Page\n\n"
		    "NOW PLAYING: " 
		    (if mp3player-now-playing-filename
			 (file-name-nondirectory mp3player-now-playing-filename)
		      "Nothing")
		    "\n\n"
		    "+/- Control volume.\n"
		    "p   Playlist selection\n"
		    "r   Toggle randomness (now " 
		    (if mp3player-random-status "on" "off") 
		    ")\n"
		    "s   Show info about the song currently playing\n"
		    "e   Edit tag of song currently playing\n"
		    ">   Skip to next song\n"
		    "q   Quit\n"
		    "x   Quit and stop playing\n"
		    "?   Help (this page)\n"
		    ))
      (fr . ,(concat "MP3 Player -- Sommaire des commandes\n\n"
		    "Vous écoutez présentement: : " 
		    (if mp3player-now-playing-filename
			 (file-name-nondirectory mp3player-now-playing-filename)
		      "Rien du tout")
		    "\n\n"
		    "+/- Contrôle du volume.\n"
		    "p   Selection d'une liste\n"
		    "r   Ordre aléatoire (présentement " 
		    (if mp3player-random-status "activé" "désactivé") 
		    ")\n"
		    "s   Information sur la sélection courante\n"
		    "e   Édition du tag de la sélection courante\n"
		    ">   Prochaine chanson\n"
		    "q   Quitter\n"
		    "x   Terminer\n"
		    "?   Aide (cete page)\n"
		    ))))))

(defun mp3player-line-up ()
  (interactive)
  (save-excursion
    (transpose-lines 1)))

(defun mp3player-line-down ()
  (interactive)
  (save-excursion
    (next-line 1)
    (transpose-lines 1)))

;;;###autoload  

;; STUFF FOR ID3 TAGS MANAGEMENTS

(defun mp3player-pad-string (str size)
  "Pad STR with space or truncate it so it's length is SIZE."
  (let ((l (length str)))
    (if (> l size)
	(substring str 0 size)
      (progn
	(while (< l size)
	  (setq str (concat str " ")
		l (length str)))
	str))))
 
(defun mp3player-get-genre-description (code)
  "Return the string description matching the given genre CODE.
The descriptions are taken from the standard ones used by id3tool, mp3tag, 
and Winamp."
  (cdr (assoc code mp3player-genre-table)))

(defun mp3player-edit-tag (file)
  "Edit or create the Id3 tag of FILE."
  (interactive "fMP3 File to edit : ")
  (let* ((info (mp3player-get-id3-tag-from-file file))
	 (tag (lambda (strL x num)
		(mp3player-pad-string 
		 (read-from-minibuffer
		  (mp3player-multilingual strL)
		  x) num))))
    (with-temp-buffer
      (insert-file file)
      (if info (kill-region (- (point-max) 128) (point-max)))
      (goto-char (point-max))
      (insert
       (concat "TAG"
	       (funcall tag '((en . "Song name: ")
			      (fr . "Titre: ")) (id3-song info) 30)
	       (funcall tag '((en . "Artist name: ")
			      (fr . "Artiste: ")) (id3-artist info) 30)
	       (funcall tag '((en . "Album name: ")
			      (fr . "Album: ")) (id3-album info) 30)
	       (funcall tag '((en . "Release year: ")
			      (fr . "Année: ")) (id3-year info) 4)
	       (funcall tag '((en . "Note: ")) (id3-note info) 30)
	       (mp3player-pad-string
		(char-to-string
		 (cdr 
		  (assoc
		   (completing-read "Genre: "
				    (mapcar
				     (lambda (x)
				       (list (cdr x)))
				     mp3player-genre-table) nil nil
				     (cdr 
				      (assoc 
				       (id3-genre info) 
				       mp3player-genre-table)))
		   (mapcar 
		    (lambda (x) 
		      (cons (cdr x)
			    (car x))) 
		    mp3player-genre-table)))) 1)))
      (write-file file))))


(defun mp3player-edit-tag-of-current ()
  "Edit id3 tag of sont currently playing."
  (interactive)
  (mp3player-edit-tag mp3player-now-playing-filename))

(defun mp3player-replace-regexp-in-string (regexp rep string &optional
					fixedcase literal subexp start)

  ;; Defun stolen from BBDB - Thank's!!

  "Replace all matches for REGEXP with REP in STRING.

Return a new string containing the replacements.

Optional arguments FIXEDCASE, LITERAL and SUBEXP are like the
arguments with the same names of function `replace-match'.  If START
is non-nil, start replacements at that index in STRING.

REP is either a string used as the NEWTEXT arg of `replace-match' or a
function.  If it is a function it is applied to each match to generate
the replacement passed to `replace-match'; the match-data at this
point are such that match 0 is the function's argument.

To replace only the first match (if any), make REGEXP match up to \\'
and replace a sub-expression, e.g.
  (replace-regexp-in-string \"\\(foo\\).*\\'\" \"bar\" \" foo foo\" nil nil 1)
    => \" bar foo\"
"

  ;; To avoid excessive consing from multiple matches in long strings,
  ;; don't just call `replace-match' continually.  Walk down the
  ;; string looking for matches of REGEXP and building up a (reversed)
  ;; list MATCHES.  This comprises segments of STRING which weren't
  ;; matched interspersed with replacements for segments that were.
  ;; [For a `large' number of replacments it's more efficient to
  ;; operate in a temporary buffer; we can't tell from the function's
  ;; args whether to choose the buffer-based implementation, though it
  ;; might be reasonable to do so for long enough STRING.]
  (let ((l (length string))
	(start (or start 0))
	matches str mb me)
    (save-match-data
      (while (and (< start l) (string-match regexp string start))
	(setq mb (match-beginning 0)
	      me (match-end 0))
	;; If we matched the empty string, make sure we advance by one char
	(when (= me mb) (setq me (min l (1+ mb))))
	;; Generate a replacement for the matched substring.
	;; Operate only on the substring to minimize string consing.
	;; Set up match data for the substring for replacement;
	;; presumably this is likely to be faster than munging the
	;; match data directly in Lisp.
	(string-match regexp (setq str (substring string mb me)))
	(setq matches
	      (cons (replace-match (if (stringp rep)
				       rep
				     (funcall rep (match-string 0 str)))
				   fixedcase literal str subexp)
		    (cons (substring string start mb) ; unmatched prefix
			  matches)))
	(setq start me))
      ;; Reconstruct a string from the pieces.
      (setq matches (cons (substring string start l) matches)) ; leftover
      (apply #'concat (nreverse matches)))))


(defun mp3player-get-id3-tag-from-file (file)
  "Get id3 information from FILE. Return a id3 structure"
  (with-temp-buffer
    (insert-file file)
    (goto-char (- (point-max) 128))
    (if (string-match "TAG" (buffer-substring (point) (+ (point) 3)))
	(progn
	  (flet ((retrieve-info (start end)
				(mp3player-trim-right
				 (car
				  (split-string
				   (buffer-substring 
				    (+ (point) start) 
				    (+ (point) end))
				   "\x0")))))
	    (make-id3
	     :song (retrieve-info 3 33)
	     :artist (retrieve-info 33 63)
	     :album (retrieve-info 63 93)
	     :note (retrieve-info 93 97)
	     :year (retrieve-info 97 127)
	     :genre (mp3player-get-genre-description
		     (string-to-char 
		      (buffer-substring (+ (point) 127)
					(+ (point) 128))))
	     :file file))))))

(defun mp3player-display-tag-info (file)
  "Display information from the id3 tag of FILE."
  (interactive "fFile: ")
  (let ((tag (mp3player-get-id3-tag-from-file file)))
    (if tag	
	(progn
	  (pop-to-buffer mp3player-song-info-buffer-name)
	  (delete-region (point-min)(point-max))
	  (insert 
	   (format 
	    (concat 
	     "Song:   %s\nArtist: %s\nAlbum:  %s\nYear:   %s\nGenre:"
	     "%s\nNote:   %s\nFile:   %s\n")
	    (id3-song tag)
	    (id3-artist tag)
	    (id3-album tag)
	    (id3-year tag)
	    (id3-genre tag)
	    (id3-note tag)
	    (id3-file tag)
	    )))
      (message 
       "No id3 tag in file. Please use 'mp3player-edit-tag' to create one."))))

(defun mp3player-display-info-of-current ()
  "Show info about current song in a info buffer."
  (interactive)
  (mp3player-display-tag-info mp3player-now-playing-filename))

(defun mp3player-list-to-winamp-playlist-str (L &optional name)
  (cond ((equal mp3player-winamp-playlist-format 'b4s)
	 (mp3player-list-to-b4s-str L name))
	((equal mp3player-winamp-playlist-format 'm3u)
	 (mp3player-list-to-m3u-str L name))))

(defun mp3player-list-to-b4s-str (L &optional name)
  (concat
   "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n"
   "  <WinampXML>\n"
   "  <!-- Generated by: Emacs " emacs-version " / mp3player " 
   mp3player-version " -->\n"
   "  <playlist num_entries=\"" (number-to-string (length L)) "\" label=\"" 
   (or name "tmp") "\">\n"
   (apply 'concat 
	  (mapcar
	   (lambda (x)
	     (let ((encoded (mp3player-replace-regexp-in-string 
			     "&" "&amp;" x)))
	       (concat 
		"    <entry Playstring=\"file:" encoded  "\">\n"
		"      <Name>" (file-name-sans-extension 
				(file-name-nondirectory encoded)) 
		"</Name>\n"
		"    </entry>\n")))
	   L))
   "  </playlist>\n  </WinampXML>\n"))

(defun mp3player-list-to-m3u-str (L &optional name)
  (apply 'concat (mapcar 
		  (lambda (x)
		    (concat x "\n"))
		  L)))

(defun mp3player-export-playlist-to-winamp (playlist)
  (interactive 
   (list 
    (completing-read 
     (mp3player-multilingual '((en . "Playlist : ")
			       (fr . "Liste : ")))
     (mapcar 'list (mp3player-playlists)))))
    (let* ((file (concat mp3player-playlists-directory "/" playlist))
	   (pl (with-temp-buffer 
		 (insert-file file)
		 (split-string (buffer-substring (point-min) 
						 (point-max)) "\n")))
	   (name playlist))
      (switch-to-buffer (concat name "." 
				(mp3player-winamp-playlist-extension)))
      (insert (mp3player-list-to-winamp-playlist-str pl name))))



;;;; UNFINISHED AND UNDER DEVELOPEMENT DEFUNs BELOW. HANDLE WITH CARE!


;; Obsolete 
;; (defun mp3player-browse (&optional dir)
;;   (interactive)
;;   (require 'widget)
;;   (let* ((cdir (or dir (pwd)))
;; 	 (id3L (mapcar 
;; 		(lambda (x)
;; 		  (mp3player-get-id3-tag-from-file x))
;; 		(mp3player-files-in-below-directory cdir))))
;;     (switch-to-buffer "*MP3 File List*")
;;     (hscroll-mode)
;;     (delete-region (point-min)(point-max))
;;     (mapcar
;;      (lambda (x)
;;        (insert (format "%-30s | %-25s | %-10s\n" (cadr x) (car x) (nth 5 x))))
;;      id3L)))

;;; NEW CONCEPT: Playrules

;; So far I based mp3player on playlists, but I realise (from usage)
;; that this is not interesting as I'm constantly rebuilding them as I
;; add new song to my mp3 directories.

;; What I was doing was building playlist according to mood
;; I have my morning playlist; classical, balad.. all thing slow
;; as day avance, I'm ready for more heavy stuff.

;; So I think, instead of building playlist, why dont let emacs
;; do this for me.  So instead of playlist I plan to use playrules.

;; Basicaly a playrules is: 
;;  - a starting directory 
;;  - a list style to encourage
;;  - a list style to avoid  
;;  - a list artist to encourage 
;;  - artist to avoid.

;; Maybe I can also include a score system like gnus?

;; anyway... let's hack!

(defstruct playrule 
  dir 
  genres-yes 
  genres-no 
  artists-yes 
  artists-no 
  files-yes 
  files-no)

(defun mp3player-make-playlist-from-playrule (playlist playrule)
  (let* ((fl (mp3player-files-in-below-directory (playrule-dir playrule)))
	 (nL))
    (mapcar 
     (lambda (x)
       (let ((tag (mp3player-get-id3-tag-from-file x)))
	 (if tag
	     (if (and (or (member (id3-genre tag) (playrule-genres-yes p))
			  (member (id3-artist tag)(playrule-artists-yes p))
			  (member (id3-file tag) (playrule-files-yes p)))
		      (not (or (member (id3-genre tag)  
				       (playrule-genres-no p))
			       (member (id3-artist tag) 
				       (playrule-artists-no p))
			       (member (id3-file tag)   
				       (playrule-files-no p)))))
			(setq nL (cons x nL))))))
     fl)
    nL))

;; Or maybe this one...

(defun mp3player-make-playlist-from-playrule (playrule)
  (let* ((fl (mp3player-files-in-below-directory 
	      (playrule-dir playrule)))
	 (nL)
	 (regbuild (lambda (L)
		     (concat "\\("
			     (apply 'concat 
				    (mapcar 
				     (lambda (x) 
				       (concat x "\\|")) L))
			     "\\)"))))		     
    (mapcar 
     (lambda (x)
       (let ((tag (mp3player-get-id3-tag-from-file x)))
	 (if tag
	     (if (and (or (member (id3-genre tag) (playrule-genres-yes p))
			  (string-match 
			   (funcall 'regbuild (playrule-artists-yes p))
			   (id3-artist tag))
			  (string-match 
			   (funcall 'regbuild (playrule-files-yes p))
			   (id3-file tag)))
		      (not (or (member (id3-genre tag)  
				       (playrule-genres-no p))
			       (string-match 
				(funcall 'regbuild (playrule-artists-no p))
					     (id3-artist tag) )
			       (string-match 
				(funcall 'regbuild (playrule-files-no p))
					     (id3-file tag) ))))
		 (setq nL (cons x nL))))))
     fl)
    nL))



;; mp3player-view-mode

(defvar mp3player-view-mode-filename-regexp "file:\\([^]]*\\)")

(define-derived-mode mp3player-id3-view-mode text-mode 
  "id3 View"
  "Major mode browsing music files with id3.
Special commands:
\\{mp3player-id3-view-mode-map}"
  (setq mp3player-id3-view-font-lock-keywords
	`((,mp3player-view-mode-filename-regexp . font-lock-comment-face))
	font-lock-defaults 
	'(mp3player-id3-view-font-lock-keywords nil t)
	truncate-lines t)
  (make-local-variable 'font-lock-defaults)
  (font-lock-fontify-buffer)
  (define-key mp3player-id3-view-mode-map "q" 'kill-buffer)
  (define-key mp3player-id3-view-mode-map "p" 'mp3player-id3-view-mode-play-file)
  )

(defun mp3player-get-filename-of-current-id3-info-line ()
  (let ((str (buffer-substring (line-beginning-position)(line-end-position))))
    (string-match 
     mp3player-view-mode-filename-regexp str)
    (match-string 1 str))) 

(defun mp3player-view-mode-insert-id3-info (file)
  "Insert information from the id3 tag of FILE on current line."
  (let ((tag (mp3player-get-id3-tag-from-file file)))
    (if tag
	(progn
	  (insert 
	   (format "%-30s %-20s %-20s %-4s %-10s %-20s [file:%s]\n"
		   (id3-song tag)
		   (id3-artist tag)
		   (id3-album tag)
		   (id3-year tag)
		   (id3-genre tag)
		   (id3-note tag)
		   (id3-file tag)
		   )))
      (insert 
       (concat 
	"No id3 tag in file.  [file:" 
	file "]\n")))))

(defun mp3player-id3-view-mode-play-file ()
  (interactive)
  (mp3player-play (mp3player-get-filename-of-current-id3-info-line)))

(defun mp3player-view-mode-browse (dir)
  (interactive "Ddir: ")
  (let ((fL (directory-files dir t)))
    (switch-to-buffer (concat "*id3-View - " dir ))
    (mp3player-id3-view-mode)
    (mapcar (lambda (x) 
	      (if (string-match mp3player-mp3-file-regexp x)
		  (mp3player-view-mode-insert-id3-info x))) fL)
    (setq truncate-lines t)
    (goto-char (point-min))))

(defalias 'mp3player-browse-id3 'mp3player-view-mode-browse)

(defun mp3player-id3-view-mode-get-filename ()
  (let ((str (buffer-substring (line-beginning-position)
			       (line-end-position))))
    (string-match mp3player-filename-in-id3-view-mode-regexp str)
    (match-string 1 str)))

(defun mp3player-id3-view-mode-edit-tag ()
  (interactive)
  (mp3player-edit-tag (mp3player-get-filename-of-current-id3-info-line)))

(if (featurep 'erc)
    (defun erc-cmd-NP (&rest ignore)
      "Post what Winamp is playing." 
      (interactive)
      (erc-send-message 
       (concat "NP: "
	       (if mp3player-now-playing
		   (concat  (id3-song mp3player-now-playing) " (" (id3-artist mp3player-now-playing) ")")
		 (file-name-sans-extension (file-name-nondirectory mp3player-now-playing-filename)))))))


(defun mp3player-now-playing-string ()
  (concat 
    (if mp3player-now-playing
	(concat  (id3-song mp3player-now-playing) " (" (id3-artist mp3player-now-playing) ")")
      (file-name-sans-extension (file-name-nondirectory mp3player-now-playing-filename)))
    (if mp3player-use-scoring
	(concat " [" 
		(number-to-string (mp3player-get-score mp3player-now-playing-filename)) 
		"]") "")))

(defun mp3player-what ()
  (interactive)
  (message (mp3player-now-playing-string)))

(provide 'mp3player)

;;; mp3player.el ends here
