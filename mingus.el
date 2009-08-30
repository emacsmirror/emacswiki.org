;;; mingus.el --- Time-stamp: <2009-07-06 20:22:26 sharik>

;;            _
;;  _ __ ___ (_)_ __   __ _ _   _ ___
;; | '_ ` _ \| | '_ \ / _` | | | / __|
;; | | | | | | | | | | (_| | |_| \__ \
;; |_| |_| |_|_|_| |_|\__, |\__,_|___/
;;                    |___/
;; -----------------------------------------------------------
;; MPD Interface that's No Garbage, but (just) Utterly Stylish
;; -----------------------------------------------------------
;; ....................but actually named after a man so named
;;

;; Copyright (C) 2006-2009 Niels Giesen <com dot gmail at niels dot giesen, in
;; reversed order>

;; Author: Niels Giesen <pft on #emacs>
;; Version: New Now Know How, or: 0.29
;; Latest version can be found at http://niels.kicks-ass.org/index.php/emacs/mingus/
;; For Changes, please view http://niels.kicks-ass.org/public/mingus/src/ChangeLog

;; Keywords: multimedia, elisp, music, mpd

;; This file is *NOT* part of GNU Emacs

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
;; Mingus is a client for the Music Player Daemon (MPD). It provides an
;; interactive interface, where most emphasis lies on on-screen display/editing
;; of the playlist, and browsing in a buffer. However, minibuffer operations are
;; becoming more intelligent with each version (with completive browsing
;; somewhat like in `find-file', and searching on multiple fields, also with
;; auto-completion).

;; INSTALLATION INSTRUCTIONS:

;; make sure you have libmpdee.el in your load-path. NOTE for old-time users:
;; mpc is not required anymore. Everything is done in lisp. This also means that
;; mingus has become multi-platform (in an easy way).

;; 1. When you install both the main mingus AND mingus-stays-home (recommended
;; when the computer running the mpd service is the same as the one from which
;; mingus is being run, see more on the reasons for installing mingus-stays-home
;; (id3 tagging, cd-burning, integration with dired and the shell etc etc in the
;; file mingus-stays-home.el)) :

;; byte-compile, IN ORDER, repeat: IN ORDER, the files mingus.el and
;; mingus-stays-home.el

;; Add the following to your .emacs:

;; (add-to-list 'load-path "/path/where/mingus-and-mingus-stays-home-reside")
;; (autoload 'mingus "mingus-stays-home")

;; 2. Mingus only (so NO mingus-stays-home) :

;; byte-compile the file mingus.el

;; Add the following to your .emacs:

;; (add-to-list 'load-path "/path/where/mingus/resides")
;; (autoload 'mingus "mingus")

;; Design issues:

;; No editing of metadata tags is provided in mingus itself. This is because mpd is
;; designed to be run in a network as a server (although it can be used on a single
;; system, which, in fact, is what I do); as such, clients to mpd are unaware of mpd's
;; root dir, and possibly/probably do not have write permissions on the music
;; files.

;; If you DO use mingus-stays-home, rough metadata-editing IS provided. `mingus-id3-set'
;; tries to guess the values for artist, song, track number, and album from the name
;; encountered in the playlist. Use it with caution though, as as I said, it is still
;; rough, e.g. having to abstract away from differences between the various tagging
;; formats. I AM looking into taglib for an elegant solution. But that will take some
;; time. So be patient.

;; The interface is roughly based on that on ncmpc. Many keybindings are alike,
;; except for some notoriously vi-style-ones.  Some significant features (main
;; reasons to write this stuff) :

;; MARKING Notice specifically the possibility to mark multiple songs in the playlist
;; for movement or deletion (by pressing the spacebar one toggles the mark at the
;; current line; if there is a region, it marks all songs in the region.) Pressing 'y'
;; asks for a regular expression against which to match the songs. Pressing 'Y' unmarks
;; alike. If a song matches, it is marked. Unmarking all marks happens with a single
;; capital "U".

;; INSERTION POINT Another nice feature is "mingus-set-insertion-point" (Key:
;; "i") : mark a song after which you would like your next insertions to take
;; place. Then go inserting. Unset this behaviour with "u"
;; (mingus-unset-insertion-point), and songs will be added to the end of the
;; playlist again. As of version 0.24 this is NOT time-consuming. Yeah!

;; NOTE: right now these two functions are mutually exclusive.

;; DIRED:

;; Ability to snap to the file location of a song instantly in `dired', so as
;; to perform file management or other actions on these files easily (such as
;; removal, movement or renaming), or just to check wtfs '3.ogg' actually
;; refers to.

;; You might want to change the `dired-mode-map' so that it will play well with
;; Mingus.  If you want to, you can set the variable `mingus-dired-add-keys' to
;; t; this can be done with `mingus-customize'. It will set "SPC" to
;; `mingus-dired-add', "C-u SPC" to `mingus-dired-add-and-play' and add an item
;; for `mingus-dired-add' to the menu-bar in dired. `mingus-dwim-add' and
;; `mingus-dwim-add-and-play' (see below) calls mingus-dired-add when in dired,
;; so binding this to a global key might be a nice solution too.

;; For those already familiar with mpd, and have set that up, you're done now.

;; If you get a message like

;; MPD_HOST and/or MPD_PORT environment variables are not set message: problems
;; getting a response from "localhost" on port 6600 : Connection refused

;; there are two options:

;; 1. you want to run locally, so run mpd
;; first. Do so from somewhere else or simply evaluate (mingus-start-daemon).
;; On some configurations of mpd this must be done as root.

;; For those unfamiliar with mpd, to set it up, put something like the following
;; in ~/.mpdconf (this is for when run a user)

;; port                "6600"
;; music_directory     "/your/music/directory"
;; playlist_directory  "~/playlists"
;; log_file            "~/.mpd.log"
;; message_file        "~/.mpd.err"
;;
;; then run mpd

;; 2. you want to connect to a remote host, but have not set the
;; environment variables MPD_HOST and/or MPD_PORT. Do so by calling
;; (mingus-set-variables-interactively) (settings lost when emacs
;; restarted) or by means of customization (mingus-customize) or
;; (customize-group 'mingus). 

;; NEW in mingus 0.21: `mingus-wake-up-call'; fixed the lisp-max-eval-depth
;; error message when leaving mingus-info on for a while; allowing spaces in
;; minibuffer operations, such as loading and saving of playlists, radio-streams
;; and the like, but most of all: inclusion of mingus-stays-home, which provides
;; nice integration features. See that file for more information. Emacs21
;; compatablity, except for parts of mingus-stays-home.

;; Known bugs:

;; * a file name cannot have a double quotes (") or a backtick (`) in it. Do not
;; know how to fix that, so if anyone feels so inclined... You CAN query your
;; database (M-x mingus-query-regexp " RET) to know if you are in the possession
;; of such files, so you can adjust their names (with mingus-stays-home
;; installed: press 0 (zero) to go to dired to do so). The only way to insert
;; such files currently is by inserting their parent directory.

;; point-of-insertion only works with one file or directory at a time

;;; Code:
;; (@> "requirements")
(require 'cl)
(eval-when-compile (load "cl-macs"))
(require 'dired)
(require 'time-date)
(require 'libmpdee)
(require 'thingatpt)

;; (@> "globals")
(defvar mingus-header-height 0)
(defvar mingus-marked-list nil)
(defvar mingus-wake-up-call nil)
(defvar mingus-modeline-timer nil)
(defvar mingus-status nil)
(defvar mingus-marked-list nil
  "List of marked songs, identified by songid")
(defvar *mingus-point-of-insertion* nil "Insertion point for mingus")
(defvar *mingus-positions* nil "Cursor positions retained in *Mingus Browser*")
(defvar *mingus-header-when-empty* "Press ? for help, 3 for Mingus Browser, 0 for dired."
  "Header to show when the playlist is empty")
(defstruct (mingus-data)
  (playlist -1)
  (song nil))
(defvar mingus-data (make-mingus-data))

;; (@> "faces")
(defgroup mingus-faces ()
  "Customization group for faces in Mingus"
  :group 'mingus)

(defface mingus-directory-face
  '((default)
    (((background light)) (:foreground "#a0606d"))
    (((background dark)) (:foreground "orange")))
  "Face for displaying directories"
  :group 'mingus-faces)

(defface mingus-song-file-face
  '((default)
    (((background light)) (:foreground "#616fa2"))
    (((background dark)) (:foreground "lightgreen")))
  "Face for displaying song files"
  :group 'mingus-faces)

(defface mingus-playlist-face
  '((default)
    (((background light)) (:foreground "#918e2d"))
    (((background dark)) (:foreground "yellow")))
  "Face for displaying playlist files"
  :group 'mingus-faces)

(defun mingus-exec (string)
  (mpd-execute-command mpd-inter-conn string))

;; (@> "currentsongdata")
(defun mingus-get-song-pos ()
  (getf (mingus-data-song mingus-data) 'pos))

(defun mingus-set-song-pos (&optional pos)
  (setf (getf (mingus-data-song mingus-data) 'pos)
        (or pos (getf (mpd-get-status mpd-inter-conn) 'song))))

;; (@> "playlist versioning")
(defun mingus-set-playlist-version (&optional to)
  "Set internal playlist version to TO or to true current version."
  (setf (mingus-data-playlist mingus-data)
        (or to (getf (mpd-get-status mpd-inter-conn) 'playlist))))

(defun mingus-get-old-playlist-version ()
  "Get old internal playlist version"
  (mingus-data-playlist mingus-data))

(defun mingus-get-new-playlist-version ()
  "Get current playlist version"
  (getf (mpd-get-status mpd-inter-conn) 'playlist))

;; configuration
(defun mingus-get-config-option (file option)
  (if (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (re-search-forward (format "%s[[:blank:]]+" option))
        (goto-char (re-search-forward "\""))
        (buffer-substring-no-properties (point) (1- (re-search-forward "\""))))
    nil))

(defgroup mingus nil "Group customization for mingus mpd interface"
 :group 'external
 :group 'multimedia
 :group 'applications)

(defcustom mingus-mpd-config-file "~/.mpdconf"
  "File used by mpd as a configuration file"
  :group 'mingus
  :type '(string))

(defcustom mingus-mpd-playlist-dir
  (expand-file-name
   (concat (mingus-get-config-option
            mingus-mpd-config-file "playlist_directory") 
           "/"))
  "Directory where MPD puts its playlists"
  :group 'mingus
  :type '(string))

(defgroup mingus-mode-line nil
  "Customization group to control the modeline for `mingus'"
  :group 'mingus)

(defcustom mingus-mpd-env-set-p nil
  "Whether to set environment variables from emacs.
Do not set when nil.
Do set when t.
Default: nil.
These variables are set when loading mingus or callinge `mingus-set-variables'."
  :group 'mingus
  :type '(boolean))

(defun mingus-set-host (sym host)
  (let ((mpd-interactive-connection-parameters
         (list host
               (or (and 
                    (boundp 'mingus-mpd-port)
                    mingus-mpd-port)
                   (or (and (getenv "MPD_PORT")
                            (string-to-number (getenv "MPD_PORT")))
                       6600))
               10.0)))
    (when (processp (aref mpd-inter-conn 1))
      (stop-process (aref mpd-inter-conn 1)))
    (setq mpd-inter-conn
          (apply 'mpd-conn-new `(,@(mpd-connection-tidy
                                    mpd-interactive-connection-parameters)
                                 nil))))
  (set-default sym host)
  (save-window-excursion
    (when (get-buffer "*Mingus*")
      (mingus))
    (when (get-buffer "*Mingus Browser*")
      (mingus-ls ""))))

(defun mingus-set-port (sym port)
  (let ((mpd-interactive-connection-parameters
         (list 
          (or (and 
               (boundp 'mingus-mpd-host)
               mingus-mpd-host)
              (or (getenv "MPD_HOST") "localhost"))
          port 10.0)))
    (when (processp (aref mpd-inter-conn 1))
      (stop-process (aref mpd-inter-conn 1)))
    (setq mpd-inter-conn
          (apply 'mpd-conn-new `(,@(mpd-connection-tidy
                                    mpd-interactive-connection-parameters)
                                 nil))))
  (set-default sym port)
  (save-window-excursion
    (when (get-buffer "*Mingus*")
      (mingus))
    (when (get-buffer "*Mingus Browser*")
      (mingus-ls ""))))

(defcustom mingus-mpd-host (getenv "MPD_HOST")
  "Setting for environment variable MPD_HOST"
  :group 'mingus
  :type '(string)
  :set 'mingus-set-host)

(defcustom mingus-mpd-port (if (getenv "MPD_PORT")
                               (string-to-number (getenv "MPD_PORT"))
                             6600)
  "Setting for environment variable MPD_PORT"
  :group 'mingus
  :type '(integer)
  :set 'mingus-set-port)

(defcustom mingus-mpd-root
  (expand-file-name
   (concat (mingus-get-config-option
            mingus-mpd-config-file
            "music_directory") "/"))
  "Music directory used by MPD.

Note that you can use tramp, as in

\"/ssh:username@host:/var/lib/mpd/music/\" 

\(don't forget the trailing slash)"
  :group 'mingus
  :type '(string))

(defcustom mingus-seek-amount 10
  "Default amount of seconds or percents to seek by when using `mingus-seek'."
  :group 'mingus
  :type '(integer))

(when mingus-mpd-env-set-p
  (setenv "MPD_HOST" mingus-mpd-host)
  (setenv "MPD_PORT" (number-to-string mingus-mpd-port)))

(defcustom mingus-mode-always-modeline nil
  "Behaviour of modeline: NIL shows current mpd status only in
mingus buffers; Current mpd status is shown in all buffers when
set to t."
  :group 'mingus-mode-line
  :type '(boolean))

;; (defcustom mingus-mode-line-string "[[%artist% - ]%title%]|[%file%]"
;;   "Format-string to display in modeline;
;; `mingus-mode-line-show-elapsed-time' and
;; `mingus-mode-line-show-elapsed-percentage'."
;;   :group 'mingus-mode-line
;;   :type '(string))
;; FIXME: add customization widget.
(defcustom mingus-mode-line-string-max 40
  "Maximum length for (result of) `mingus-mode-line-string'."
  :group 'mingus-mode-line
  :type '(integer))

(defcustom mingus-mode-line-show-elapsed-time t
  "Whether or not to display elapsed time in the mode-line."
  :group 'mingus-mode-line
  :type '(boolean))

(defcustom mingus-mode-line-show-elapsed-percentage nil
  "Whether or not to display elapsed time in the mode-line."
  :group 'mingus-mode-line
  :type '(boolean))

(defcustom mingus-mode-line-show-status t
  "Display status information on volume, repeat and random in mode-line?
See also the variables `mingus-mode-line-show-volume' and
`mingus-mode-line-show-random-and-repeat-status'"
  :group 'mingus-mode-line
  :type '(boolean))

(defcustom mingus-mode-line-show-volume t
  "Display volume information in the mode-line?

Set `mingus-mode-line-show-status' to non-`nil' value for this variable to
have effect"
  :group 'mingus-mode-line
  :type '(boolean))

(defcustom mingus-mode-line-show-random-and-repeat-status t
  "Display random and repeat status in the mode-line?

If random is shown, a letter z is shown, if repeat is on, a letter r is shown
too. Set the variable `mingus-mode-line-show-status' to a non-`nil' value for
this variable to have effect."
  :group 'mingus-mode-line
  :type '(boolean))

;; (@> "emacs21") some emacs21 compatibility:
(if (not (fboundp 'read-number))
    (defun read-number (prompt &optional default)
      (let ((n nil))
        (when default
          (setq prompt
                (if (string-match "\\(\\):[ \t]*\\'" prompt)
                    (replace-match (format " (default %s)" default)
                                   t t prompt 1)
                  (replace-regexp-in-string "[ \t]*\\'"
                                            (format " (default %s) " default)
                                            prompt t t))))
        (while
            (progn
              (let ((str (read-from-minibuffer
                          prompt nil nil nil nil
                          (and default
                               (number-to-string default)))))
                (setq n (cond
                         ((zerop (length str)) default)
                         ((stringp str) (read str)))))
              (unless (numberp n)
                (message "Please enter a number.")
                (sit-for 1)
                t))) n)))

;; fixme: use `mpd-inter-conn' directly. Doc this and get rid of these vars.
' (defun mingus-set-variables-interactively ()
    "Set environment variables for mpd connection.

Default to `mingus-mpd-host' and `mingus-mpd-port'. Do not use this for
customizing these values; use `mingus-customize' for that"
    (interactive)
    (setenv "MPD_HOST" (read-string "MPD_HOST: " mingus-mpd-host))
    (setenv "MPD_PORT"
            (number-to-string (read-number "MPD_PORT: " mingus-mpd-port))))

(defun mingus-set-variables-interactively ()
  "Set environment variables for mpd connection.

Default to `mingus-mpd-host' and `mingus-mpd-port'. Do not use this for
customizing these values; use `mingus-customize' for that."
  (interactive)
  (let ((mpd-interactive-connection-parameters
         (list (read-string "MPD_HOST: " mingus-mpd-host)
               (read-number "MPD_PORT: " mingus-mpd-port)
               (read-number "Timeout: " 10.0))))
    ;; clean up for new connection - bit too low level actually
    (stop-process (aref mpd-inter-conn 1))
    ;; make new connection and process
    (setq mpd-inter-conn
          (apply 'mpd-conn-new `(,@(mpd-connection-tidy
                                    mpd-interactive-connection-parameters)
                                 nil))))
  "The global mpd connection used for interactive queries.")



(defun mingus-customize ()
  "Call the customize function with mingus as argument."
  (interactive)
  (customize-group 'mingus))

(defvar mingus-version "New Now Know How, or: 0.29")

(defun mingus-version ()
  "Echo `mingus-version' in minibuffer."
  (interactive)
  (message "Version: %s" mingus-version))

(defvar mingus-stream-regexp
  "http:[^<>'\"?{}() ]+\.\\([Mm][Pp]3\\|[Oo][Gg][Gg]\\|[fF][lL][aA][cC]\\|[wW][aA][vV]\\|[0-9]{4}\\)")

(defvar mingus-last-query-results nil
  "Variable to hold last results of mingus-query")

(make-variable-buffer-local 'mingus-last-query-results)

(defvar mingus-help-text ""
  "Text to display in mingus-help")

(setq mingus-help-text
      (format
       "           _
 _ __ ___ (_)_ __   __ _ _   _ ___
| '_ ` _ \\| | '_ \\ / _` | | | / __|
| | | | | | | | | | (_| | |_| \\__ \\
|_| |_| |_|_|_| |_|\\__, |\\__,_|___/
                   |___/
=====================================================
MPD Interface, Nice, GPL'ed, User-friendly and Simple
=====================================================
.........but actually just named after Charles Mingus

Version: %s

REFCARD: (see further down for more elaborate instructions)

Those familiar with dired-mode should find themselves at home;
those familiar with ncmpc too, AMAP that is

MAIN CONTROLS:

mingus-help:       1
mingus-playlist:   2
mingus-browser:    3
mingus-dired-file: 0

Global keys:

p                       mingus-toggle (toggle play/pause)
>                       mingus-next
<                       mingus-prev
q                       mingus-git-out
s                       mingus-stop
?,1,H                   mingus-help
+,right,*, C-<mouse-4>  mingus-vol-up
-,left,/,  C-<mouse-5>  mingus-vol-down
a                       mingus-insert
~                       mingus-add-stream
`                       mingus-add-podcast
b,S-<mouse-5>           mingus-seek-backward
f,S-<mouse-4>           mingus-seek-forward
%%                       mingus-seek-percents
$                       mingus-seek-from-start
c                       mingus-crop
C                       mingus-clear
L                       mingus-load-all
z                       mingus-random
Z                       mingus-shuffle
r                       mingus-repeat
C-x C-s                 mingus-save-playlist
R                       mingus-remove-playlist
l                       mingus-load-playlist
o                       mingus-open-playlist
Q                       mingus-query
M-%%                     mingus-query-regexp
\\                       mingus-last-query-results
k                       forward-line -1
j                       forward-line
v                       mingus-show-version


Playlist keys:

d,C-d,
<delete>, C-w           mingus-del
D                       mingus-del-marked
O                       mingus-del-other-songs
M                       mingus-move-all
C-l                     mingus-goto-current-song
C-k                     mingus-move-up
C-j                     mingus-move-down
RET,[mouse-3]           mingus-play
SPC,m,[mouse-2]         mingus-mark
y                       mingus-mark-regexp
Y                       mingus-unmark-regexp
U                       mingus-unmark-all
!                       run a command on the marked songs

Browser keys:

RET, [mouse-1]          mingus-down-dir-or-play-song
:,^, [mouse-3]          mingus-dir-up
SPC  [mouse-2]          mingus-insert
P                       mingus-insert-and-play
S                       mingus-browse-sort

MORE ELABORATE INSTRUCTIONS:

Requirements:

- cl-macs.el
- dired.el (included in emacs)
- the program symlinks (for dired)
- access to a connection with an mpd server, either locally or on another
  server.

- Emacs22

- Issues with emacs21:

although the function line-number-at-pos is replaced with a
custom mingus-line-number-at-pos, and the call to
`while-no-input' is left out when running emacs21, and whereas
for the previously unsupported read-number, I simply replicated
its function definition from the emacs22 subr.el, annoying issues
now have crept up so that you have to call C-g whenever switching
to and fro the mingus-buffers. The rest /seems/ to work somewhat
now.

Getting started:

This help is always available with the command mingus-help, or
the keys ? or 1 from the buffers *Mingus* or *Mingus
Browser*

When mpd is already playing a playlist, the command M-x mingus
will show this playlist; when not so, load a playlist with l,
or make a new one with M-x mingus-browse (default key: 3).

Starting mpd:                    mingus-start-mpd-daemon
Providing environment variables: mingus-set-variables-interactively
                                 (see also mingus-customize)

SELECTION OF SONGS:

Browsing:  command: mingus-browse key: 3

movement and insertion:

SPACEBAR always inserts everything under point or region

P          same as SPACEBAR, and plays the inserted song(s) instantly

RET        same as SPACEBAR, except on a dir and no mark, then descend into dir.

^  or :    go up a directory

Minibuffer browsing:

a          insert a file or directory through the use of the minibuffer;
           follow instructions there provided

Playlist loading:

l          load playlist

Streaming audio:

Mpd supports streaming audio.  Aside from the fact that one can
always save a link in a playlist, this provides a way to take
one's own presets with you as a client, as streaming audio does
not require storage of songs on the server. Mingus takes
advantage of this fact by providing the customizable variables
`mingus-stream-alist' and `mingus-podcast-alist': alists of
conses whose key is a pretty name, and whose value is the url of
the respective radio stream or podcast file.

~          load an audio stream, read from minibuffer, with completion
           from the customizable variable `mingus-stream-alist';

           defaults to link (in w3m and possibly in gnus html mail buffers)
           or url under point.

`          same as ~, but loads all podcasts found in a link.
           Completion provided by the customizable variable
           `mingus-podcast-alist'.

           Actually ~ will load a podcast too if a podcast is under point.
           However, to provide two separate variables for completion,
           this option is provided separately.

Making sure an insertion is instantly played:

If any of the insertion commands is prefinged, they will play the insertion
instantly after insertion.

C-u a      mingus-insert-and-play
C-u l      mingus-load-playlist-and-play
C-u ~      mingus-add-stream-and-play
C-u `      mingus-add-podcast-and-play

Querying:

Q          query the mpd database for artist, album, filename, title,
           or regexp on filename (type read from minibuffer)
M-%%        query for regexp on filename
\\          show last query results again


Results are shown in the *Mingus Browser* buffer,
where all commands for browsing are available

PLAYING CONTROLS:

see the refcard, and documentation of various commands, just try
them out. They should be quite self-evident, but let me know when
they are not. Not every command is (already) mapped to a
key, so M-x mingus- TAB to your delight to find everything.

PLAYLIST EDITING:

Deletion:

on marked songs: see section `Marking'

C-d, d, C-w or DEL

delete single file, or region when there is a region;

NB: this leaves the marking of other songs intact. As such it can
be slow, esp. when the region is large; it is then highly
recommended to mark the songs first, and then issue the command
mingus-del-marked (until I rewrite this function :])

Movement:

of marked songs: see section `Marking'

of single song:

C-k                 Move song up one position
C-j                 Move song up down position

Marking:

Marking songs is useful for movement or deletion of multiple songs in or from
the playlist; first mark them, then delete or move them (to point).

m or SPACEBAR       (un)mark a song, or region, when there is a region
D (upcased)         delete marked songs (this will have the same effect as
                    mingus-del when there are no marked songs)
M                   move marked songs to point
!                   get prompted for an operation on the marked songs

Point of insertion:

Use mingus-set-insertion-point to specify where new insertions from the
insertion commands from the *Mingus Browser* buffer or from
minibuffer-insertion will take place. If *mingus-point-of-insertion* is
unset (nil), insertions will take place at the end of the playlist.

i                   set insertion point
u                   unset insertion point (available from everywhere)
C-u i               show current insertion point and move point there

Saving your playlist:

what about C-x C-s, can you memorize that?


=================================================
AUTHOR:  Niels Giesen
CONTACT: nielsDINOSAURgiesen@gmailDODOcom, but with the extinct creatures replaced with dots.
WEBSITE:  http://niels.kicks-ass.org/index.php/emacs/mingus
" mingus-version))

;; regexps
 
(defcustom mingus-playlist-separator " + "
  "Separator for fields (artist, song, genre etc.) in Mingus playlist.

You might want to put something like the following in your .emacs:

  (setq mingus-playlist-separator
     (if window-system
          \" ● \"
        \" + \"))

Or, you might show me how to use a function/string choice in customize ;)"
 :group 'mingus
 :type '(string))

(defcustom mingus-current-song-marker ">>> "

  "Marker for the currently playing song.

You might want to put something like the following in your .emacs:

  (setq mingus-current-song-marker
     (if window-system
          \"‣‣‣ \"
        \">>> \"))

Or, you might show me how to use a function/string choice in customize ;)"
  :group 'mingus
  :type '(string))

(defmacro mingus-define-color-line-or-region (name params)
  `(defun ,name (&optional beg end)
     (put-text-property (or beg (point-at-bol)) (or end (point-at-bol 2))
                        'face ,params)))

(mingus-define-color-line-or-region
 mingus-mark-line
 '((((class color) (background light)) (:foreground "pink" :weight bold))
   (((class color) (background dark)) (:foreground "pink"))))

(mingus-define-color-line-or-region
 mingus-mark-as-current
 '((:height 300 :foreground "lightblue" :background "white")))

(mingus-define-color-line-or-region mingus-unmark-line 'default)

;; fixme: delete this (remove help)
(defconst mingus-help-font-lock-keywords
  (list
   '("mingus[a-zA-Z 0-9-]*" . font-lock-function-name-face)
   '("^[A-Z ]+:" . font-lock-warning-face)
   '("^[A-Z][a-z ]+:" . font-lock-constant-face)
   '("=" . font-lock-variable-name-face)
   '("`.+'")))

;; keys
 
(defconst mingus-global-map (make-keymap) "Global keymap for `mingus'")

(define-key mingus-global-map "k" (lambda () (interactive) (forward-line -1)))

;; add some keys to the various modes for dired look-ups
(define-key mingus-global-map "0" 'mingus-dired-file)
(define-key mingus-global-map "q" 'mingus-git-out)
(define-key mingus-global-map "Q" 'mingus-query)
(define-key mingus-global-map "\M-%" 'mingus-query-regexp)
(define-key mingus-global-map "\\" 'mingus-last-query-results)
(define-key mingus-global-map "j" 'forward-line)
(define-key mingus-global-map "s" 'mingus-stop)
(define-key mingus-global-map "@" 'mingus-update)
(define-key mingus-global-map "p" 'mingus-toggle)
(define-key mingus-global-map "%" 'mingus-seek-percents)
(define-key mingus-global-map ">" 'mingus-next)
(define-key mingus-global-map "<" 'mingus-prev)
(mapc (lambda (key) (define-key mingus-global-map key 'mingus-vol-up))
        '("+" [(right)] "*"))
(mapc (lambda (key) (define-key mingus-global-map key 'mingus-vol-down))
        '("-" [(left)] "/"))
(define-key mingus-global-map "b" 'mingus-seek-backward)
(define-key mingus-global-map "f" 'mingus-seek)
(define-key mingus-global-map "$" 'mingus-seek-from-start)
(define-key mingus-global-map "x" 'mingus-crossfade)
(define-key mingus-global-map "C" 'mingus-clear)
(define-key mingus-global-map "c" 'mingus-crop)
(define-key mingus-global-map "L" 'mingus-load-all)
(define-key mingus-global-map "v" 'mingus-show-version)
(define-key mingus-global-map "z" 'mingus-random)
(define-key mingus-global-map "Z" 'mingus-shuffle)
(define-key mingus-global-map "r" 'mingus-repeat)
(define-key mingus-global-map "u" 'mingus-unset-insertion-point)
(define-key mingus-global-map "l" 'mingus-load-playlist)
(define-key mingus-global-map "R" 'mingus-remove-playlist)
(mapc (lambda (key) (define-key mingus-global-map key 'mingus-help))
        '("H" "?" "1"))
(define-key mingus-global-map "a" 'mingus-insert)
(define-key mingus-global-map "P" 'mingus-insert-and-play)
(define-key mingus-global-map "~" 'mingus-add-stream)
(define-key mingus-global-map "`" 'mingus-add-podcast)
(define-key mingus-global-map "\C-x\C-s" 'mingus-save-playlist)
(define-key mingus-global-map "2" 'mingus)
(define-key mingus-global-map "3" 'mingus-browse)
(define-key mingus-global-map "w" 'mingus-wake-up-call)
(define-key mingus-global-map
  (if (featurep 'xemacs)[(control button5)][C-mouse-5]) 'mingus-vol-down)
(define-key mingus-global-map
  (if (featurep 'xemacs)[(control button4)][C-mouse-4]) 'mingus-vol-up)
(define-key mingus-global-map
  (if (featurep 'xemacs)[(shift button5)][S-mouse-5]) 'mingus-seek-backward)
(define-key mingus-global-map
  (if (featurep 'xemacs)[(shift button4)][S-mouse-4]) 'mingus-seek)

;; build the menu
(define-key mingus-global-map [menu-bar mingus]
  (cons "Mingus" (make-sparse-keymap "mingus")))

(define-key mingus-global-map [menu-bar mingus customization]
  (cons  "Customization"  (make-sparse-keymap "mingus customization")))

;; fixme : HOST & PORT unnecessary after libmpdee.el
(define-key mingus-global-map [menu-bar mingus customization port]
  '(menu-item "MPD PORT"
              (lambda () (interactive) (customize-variable 'mingus-mpd-port))
              :help "Port for connecting to mpd server"))

(define-key mingus-global-map [menu-bar mingus customization host]
  '(menu-item "MPD HOST"
              (lambda () (interactive) (customize-variable 'mingus-mpd-host))
              :help "Host to connect to"))

(define-key mingus-global-map [menu-bar mingus customization seek-amount]
  '("Seek Amount" .
    (lambda () (interactive) (customize-variable 'mingus-seek-amount))))

(define-key mingus-global-map [menu-bar mingus customization mode-line]
  '("Mode-line" . (lambda ()
                    (interactive)
                    (customize-group 'mingus-mode-line))))

(define-key mingus-global-map [menu-bar mingus customization stream-alist]
  '(menu-item "Streams"
              (lambda ()
                (interactive)
                (customize-variable 'mingus-stream-alist))
              :help "Customize stream presets"))

(define-key mingus-global-map [menu-bar mingus customization podcast-alist]
  '(menu-item "Podcasts"
              (lambda ()
                (interactive)
                (customize-variable 'mingus-podcast-alist))
              :help "Customize podcast presets"))

(define-key mingus-global-map [menu-bar mingus customization all]
  '(menu-item "All" mingus-customize
    :help "customize all mingus variables"))

(define-key mingus-global-map [menu-bar mingus sep1]
  '(menu-item "--"))

(define-key mingus-global-map [menu-bar mingus query-regexp]
  '(menu-item "Query regexp"  mingus-query-regexp
    :help "Query the mpd database with a regexp"))

(define-key mingus-global-map [menu-bar mingus query]
  '(menu-item "Query" mingus-query
    :help "Query the mpd database"))

(define-key mingus-global-map [menu-bar mingus sep-above-query]
  '(menu-item "--"))

(define-key mingus-global-map [menu-bar mingus streams]
  (cons "Streams and Podcasts" (make-sparse-keymap "mingus streams")))

(define-key mingus-global-map [menu-bar mingus streams podcast-alist]
  '(menu-item "Customize Podcasts"
              (lambda ()
                (interactive)
                (customize-variable 'mingus-podcast-alist))
              :help "Customize podcast presets"))

(define-key mingus-global-map [menu-bar mingus streams stream-alist]
  '(menu-item "Customize Streams"
              (lambda ()
                (interactive)
                (customize-variable 'mingus-stream-alist))
              :help "Customize stream presets"))

(define-key mingus-global-map [menu-bar mingus streams sep]
  '(menu-item "--"))

(define-key mingus-global-map [menu-bar mingus streams podcast-and-play]
  '("Add Podcast and Play"       . mingus-add-podcast-and-play))


(define-key mingus-global-map [menu-bar mingus streams podcast]
  '("Add Podcast"       . mingus-add-podcast))


(define-key mingus-global-map [menu-bar mingus streams stream-and-play]
  '("Add Stream and Play"  . mingus-add-stream-and-play))


(define-key mingus-global-map [menu-bar mingus streams stream]
  '("Add Stream"       . mingus-add-stream))

(define-key mingus-global-map [menu-bar mingus playlists]
  (cons "Playlist loading/saving/removing"
        (make-sparse-keymap "mingus playlists")))

(define-key mingus-global-map [menu-bar mingus playlists remove]
  '("Remove"       . mingus-remove-playlist))

(define-key mingus-global-map [menu-bar mingus playlists save]
  '("Save"       . mingus-save-playlist))

(define-key mingus-global-map [menu-bar mingus playlists load-and-play]
  '("Load entire database" . mingus-load-all))

(define-key mingus-global-map [menu-bar mingus playlists load-all]
  '("Load and Play" . mingus-load-playlist-and-play))

(define-key mingus-global-map [menu-bar mingus playlists load]
  '("Load" . mingus-load-playlist))

(define-key mingus-global-map [menu-bar mingus sep-above-playlists-and-streams]
  '(menu-item "--"))

(define-key mingus-global-map [menu-bar mingus repeat]
  '("Repeat (toggle)"          . mingus-repeat))

(define-key mingus-global-map [menu-bar mingus shuffle]
  '("Shuffle"          . mingus-shuffle))

(define-key mingus-global-map [menu-bar mingus random]
  '("Random (toggle)"          . mingus-random))

(define-key mingus-global-map [menu-bar mingus clear]
  '("Clear Playlist"          . mingus-clear))

(define-key mingus-global-map [menu-bar mingus crop]
  '(menu-item "Crop Playlist" mingus-crop
    :help "Delete all but the playing song"))

(define-key mingus-global-map [menu-bar mingus vol-up]
  '("Vol up" . mingus-vol-up))

(define-key mingus-global-map [menu-bar mingus vol-down]
  '("Vol down" . mingus-vol-down))

(define-key mingus-global-map [menu-bar mingus seek]
  '("Seek Forward"          . mingus-seek))

(define-key mingus-global-map [menu-bar mingus seek-back]
  '("Seek Backward"          . mingus-seek-backward))

(define-key mingus-global-map [menu-bar mingus next]
  '("Next"          . mingus-next))

(define-key mingus-global-map [menu-bar mingus previous]
  '("Previous"      . mingus-prev))

(define-key mingus-global-map [menu-bar mingus stop]
  '("Stop"          . mingus-stop))

(define-key mingus-global-map [menu-bar mingus play]
  '("Play"          . mingus-play))

(define-key mingus-global-map [menu-bar mingus toggle]
  '("Toggle play/pause"  . mingus-toggle))

(defconst mingus-help-map (copy-keymap mingus-global-map)
  "Help keymap for `mingus'")

(define-key mingus-help-map "0" #'(lambda ()
                                    (interactive)
                                    (dired mingus-mpd-root)))

(define-key mingus-help-map " " 'scroll-up)

(define-key mingus-help-map [menu-bar mingus sep-playlist-editing]
  '(menu-item "--"))

(define-key mingus-help-map [menu-bar mingus unset]
  '("Unset Insertion Point" . mingus-unset-insertion-point))

(define-key mingus-help-map [menu-bar mingus sep3]
  '(menu-item "--"))

(define-key mingus-help-map [menu-bar mingus browser]
  '(menu-item "Browser" mingus-browse :help "go to browser"))

(define-key mingus-help-map [menu-bar mingus playlist]
  '(menu-item "Playlist" mingus :help "go to playlist"))

(defconst mingus-playlist-map (copy-keymap mingus-global-map)
  "Playlist keymap for `mingus'")

;;deletion keys
(mapc (lambda (key)
          (define-key mingus-playlist-map key
            (lambda () (interactive)
              (if (mingus-mark-active)
                  (call-interactively 'mingus-del-region)
                (mingus-del-marked))))) '("D" "\C-w"))

(mapc (lambda (key) (define-key mingus-playlist-map key
                   '(lambda () (interactive)
                      (if (mingus-mark-active)
                          (call-interactively 'mingus-del-region)
                        (mingus-del)))))
        '("d" "\C-d"))

(define-key mingus-playlist-map "O" 'mingus-del-other-songs)

;;movement keys
(define-key mingus-playlist-map "M" 'mingus-move-all)

(define-key mingus-playlist-map "\C-k" 'mingus-move-up)

(define-key mingus-playlist-map "\C-j" 'mingus-move-down)

;;marking keys
(define-key mingus-playlist-map "U" 'mingus-unmark-all)

(mapc (lambda (key)
          (define-key mingus-playlist-map key
            (lambda () (interactive)
              (if (mingus-mark-active)
                  (call-interactively 'mingus-mark-region)
                (mingus-mark)))))
        '("m" " "))

(define-key mingus-playlist-map "n" 'mingus-unmark-region)

(define-key mingus-playlist-map "y" 'mingus-mark-regexp)

(define-key mingus-playlist-map "Y" 'mingus-unmark-regexp)

(define-key mingus-playlist-map "i" 'mingus-set-insertion-point)

(define-key mingus-playlist-map "t" 'mingus-toggle-marked)

(define-key mingus-playlist-map "U" 'mingus-update-thing-at-p)

(define-key mingus-playlist-map "!"
  (lambda ()
    (interactive)
    (if (or mingus-marked-list)
        (progn
          (let ((command (read-key-sequence
                          "(D)elete, (M)ove here, delete (O)thers? " )))
            (cond ((string-match "d\\|D" command)
                   (mingus-del-marked))
                  ((string-match "m\\|M" command)
                   (mingus-move-all))
                  ((string-match "o\\|O" command)
                   (mingus-del-other-songs))
                  (t nil))))
      (message "No marked songs"))))

;; miscellaneous keys
(define-key mingus-playlist-map "\r" 'mingus-play)
(define-key mingus-playlist-map "o" 'mingus-browse-to-song-at-p)
(define-key mingus-playlist-map "\C-l" 'mingus-goto-current-song)

;; menu keys
(define-key mingus-playlist-map
  [menu-bar mingus sep-playlist-editing]
  '("---" . separador))

(define-key mingus-playlist-map
  [menu-bar mingus playlist-editing]
  (cons "Playlist Editing" (make-sparse-keymap "mingus playlist editing")))

(define-key mingus-playlist-map
  [menu-bar mingus playlist-editing mingus-toggle-marked]
  '("Toggle Marked Songs" . mingus-toggle-marked))

(define-key mingus-playlist-map
  [menu-bar mingus playlist-editing mingus-unmark-all]
  '("Unmark All Songs" . mingus-unmark-all))

(define-key mingus-playlist-map
  [menu-bar mingus playlist-editing del-other]
  '("Delete Unmarked Songs" . mingus-del-other-songs))

(define-key mingus-playlist-map
  [menu-bar mingus playlist-editing del-marked]
  '("Delete Marked Songs or Song at Point" . mingus-del-marked))

(define-key mingus-playlist-map
  [menu-bar mingus playlist-editing move]
  '("Move Marked Songs" . mingus-move-all))

(define-key mingus-playlist-map
  [menu-bar mingus playlist-editing mark]
  '("Mark Region or (un)Mark Line" .
    (lambda () (interactive) (if (mingus-mark-active)
                            (call-interactively 'mingus-mark-region)
                          (mingus-mark)))))

(define-key mingus-playlist-map
  [menu-bar mingus playlist-editing unmark]
  '("Unmark Region" . 'unmark-region))

(define-key mingus-playlist-map
  [menu-bar mingus playlist-editing uns-ins-point]
  '("Unset Point of Insertion" . mingus-unset-insertion-point))

(define-key mingus-playlist-map
  [menu-bar mingus playlist-editing ins-point]
  '("Set Point of Insertion" . mingus-set-insertion-point))

(define-key mingus-playlist-map
  [menu-bar mingus playlist-editing del-region]
  '("Delete Region" . mingus-del-region))

(define-key mingus-playlist-map
  [menu-bar mingus sep3]
  '(menu-item "--"))

(define-key mingus-playlist-map
  [menu-bar mingus help]
  '(menu-item "Help" mingus-help
              :help "go to help"))

(define-key mingus-playlist-map
  [menu-bar mingus browser]
  '(menu-item "Browser" mingus-browse
              :help "go to browser"))

(define-key mingus-playlist-map 
  [menu-bar mingus dired]
  '(menu-item "Dired file" mingus-dired-file
              :help "find file in dired"))

;; mouse keys
(define-key mingus-playlist-map
  (if (featurep 'xemacs) [button1] [mouse-1])
  (lambda (ev)
    (interactive "e")
    (mouse-set-point ev)
    (mingus-play)))

(define-key mingus-playlist-map
  (if (featurep 'xemacs) [button2] [mouse-2])
  (lambda (ev)
    (interactive "e")
    (if (mingus-mark-active)
        (call-interactively (quote mingus-mark-region))
      (mouse-set-point ev)
      (mingus-mark))))

(define-key mingus-playlist-map 
  (if (featurep 'xemacs) [button3] [mouse-3])
  (lambda (ev)
    (interactive "e")
    (mouse-set-point ev)
    (mingus-dired-file)))

(defconst mingus-browse-map (copy-keymap mingus-global-map)
  "Browse keymap for `mingus'")

(define-key mingus-browse-map "\r" 'mingus-down-dir-or-play-song)
(define-key mingus-browse-map "S" 'mingus-browse-sort)
(define-key mingus-browse-map "U" 'mingus-update-thing-at-p)

(define-key mingus-browse-map
  [(down-mouse-1)]
  (lambda (event)
    (interactive "e")
    (mouse-set-point event)
    (if (cddr event)
	(mingus-insert)
      (mingus-down-dir-or-play-song))))

(define-key mingus-browse-map
  (if (featurep 'xemacs) [button2] [mouse-2])
  'mingus-insert-at-mouse)

(define-key mingus-browse-map
  (if (featurep 'xemacs) [button3] [mouse-3])
  'mingus-dir-up)

(mapc (lambda (key)
          (define-key mingus-browse-map key 'mingus-dir-up))
        '(":" "^"))

(define-key mingus-browse-map
  [menu-bar mingus sep-playlist-editing]
  '("---" . separador))

(define-key mingus-browse-map
  [menu-bar mingus unset]
  '("Unset Insertion Point" . mingus-unset-insertion-point))

(define-key mingus-browse-map
  [menu-bar mingus sep3]
  '(menu-item "--"))

(define-key mingus-browse-map
  [menu-bar mingus help]
  '(menu-item "Help" mingus-help :help "go to help"))

(define-key mingus-browse-map
  [menu-bar mingus playlist]
  '(menu-item "Playlist" mingus :help "go to playlist"))

(define-key mingus-browse-map " " 'mingus-insert)

;;;some generic functions:
 
;;;; {{xemacs compatibility}}
(when (featurep 'xemacs)
  (defun mingus-line-number-at-pos ()
    (line-number)))

(defmacro mingus-save-excursion (&rest body)
  "Execute BODY, and \"restore\" point to line-number and column."
  (let ((line (gensym))
        (col (gensym)))
    `(let ((,line (mingus-line-number-at-pos))
           (,col (- (point) (point-at-bol))))
       ,@body
       (goto-line ,line)
       (move-to-column ,col))))

;;;;  {{Generic Functions}}
(defun _mingus-bol-at (pos)
  "Return the position at beginning of line relative to POS."
  (save-excursion (goto-char pos)
                  (point-at-bol)))

(defun _mingus-eol-at (pos)
  "Return the position at end of line relative to POS."
  (save-excursion (goto-char pos)
                  (point-at-eol)))
;; List processing
(defun mingus-make-alist (list)
  "Make an alist out of a flat list (plist-style list)."
  (if (endp list)
      nil
    (cons (cons (car list) (cadr list))
          (mingus-make-alist (cddr list)))))

(defun mingus-make-alist-reversed (list)
  "Make an alist out of a flat list, whereby every pair is reversed."
  (if (endp list)
      nil
    (cons (cons (cadr list) (car list))
          (mingus-make-alist-reversed (cddr list)))))

(defun mingus-pretext (string)
  "Return part of STRING before first numeric occurence or nil otherwise."
  (if (string-match "\\(^[^[:digit:]]*\\)\\([[:digit:]]+\\)" string)
      (match-string 1 string)
    nil))


(defun mingus-logically-less-p (s1 s2)
  "Compare S1 and S2 logically, or numerically.

E.g.: \"Artist 3 my beautiful song\" is logically less than \"Artist 11 blue sea\"."
  (let ((p1 (mingus-pretext s1))
        (end1 (match-end 1))
        (end2 (match-end 2)))
    (if (and p1 (string= p1 (mingus-pretext s2)))
        (let ((n1 (string-to-number (substring s1 end1)))
              (n2 (string-to-number (substring s2 end1))))
          (if (= n1 n2)
              (mingus-logically-less-p (substring s1 end2)
                                       (substring s2 (match-end 2)))
            (< n1 n2)))
      (string< s1 s2))))

;;(mingus-logically-less-p "Jazz" "Jazz")

(defun mingus-keywordify-plist (list)
  "Turn a nasty looking plist into a nice one, with lower-cased keywords."
  (mapcar (lambda (item)
            (typecase item
              (symbol (intern-soft
                       (format ":%s" (downcase (symbol-name item)))))
              (t item))) list))

(defun mingus-mark-active ()
  (if (featurep 'xemacs)
      (mark)
    mark-active))

(defun mingus-min:sec->secs (min:secs)
  "Convert MIN:SECS (a string) to seconds (an integer)."
  (cond ((string-match "^[0-9]*:[0-9]*$" min:secs)
         (multiple-value-bind (min sec)
             (mapcar 'string-to-number (split-string min:secs ":"))
           (+ (* 60 min) sec)))
        ((string-match "^[0-9]+$" min:secs) (string-to-number min:secs))
        (t (error "Not a valid value entered (expected: [min:]secs )"))))

(defun mingus-sec->min:sec (sec)
  "Convert SEC (as integer or string) to MIN:SEC (a string)."
  (multiple-value-bind (min sec) (floor* sec 60)
    (format "%02d:%02d" min sec)))

(defun mingus-ldots (string max)
  (let ((len (length string)))
    (if (< max len)
        (concat (substring string 0 (- max 3)) "...")
      string)))

(defun mingus-line-number-at-pos (&optional pos)
  "Return (narrowed) buffer line number at position POS.
If POS is nil, use current buffer location.
This is an exact copy of line-number-at-pos for use in emacs21."
  (let ((opoint (or pos (point))) start)
    (save-excursion
      (goto-char (point-min))
      (setq start (point))
      (goto-char opoint)
      (forward-line 0)
      (1+ (count-lines start (point))))))

;; Thanks to piyo-w3m--read-query-smart and offby1
(defun mingus-completing-read-allow-spaces (prompt table &optional predicate
                                            require-match initial-input
                                            hist def inherit-input-method)
  "`completing-read', allowing space input and ignoring case."
  (let* ((completion-ignore-case t)
         (former-function (cdr (assoc 32 minibuffer-local-completion-map))))
                                        ;save former function of space character
    (setcdr (assoc 32 minibuffer-local-completion-map) 'self-insert-command)
                                        ; change space character to simply
                                        ; insert a space
    (unwind-protect
        (completing-read prompt table predicate
                         require-match initial-input
                         hist def inherit-input-method)
      (setcdr (assoc 32 minibuffer-local-completion-map) former-function))))
                                        ;change back the space character to its
                                        ;former value

(defun mingus-delete-line ()
  "Delete line at point."
  (delete-region (point-at-bol 1) (point-at-bol 2))
  (when (eobp)
    (delete-region (point-at-bol) (point-at-eol 0))
    (beginning-of-line)))

(defun mingus-strip-last-line ()
  (let (pos (point))
    (goto-char (point-max))
    (delete-region (point-at-bol) (point-at-eol 0))
    (goto-char pos)))

;; {{basic mpd functions}}
 
(defun mingus-get-songs (cmd &optional foreach)
  (mpd-get-songs mpd-inter-conn cmd foreach))

(defun mingus-pos->id (pos)
  (getf (car (mingus-get-songs (format "playlistinfo %d" pos))) 'Id))

(defun mingus-id->pos (id)
  (getf (car (mingus-get-songs (format "playlistid %d" id))) 'Pos))

(defun mingus-idlist->poslist (list)
  (mapcar 'mingus-id->pos list))

;; {{mingus-marked-list}}
 

(defun mingus-pos->mlist (pos)
  (add-to-list 'mingus-marked-list (mingus-pos->id pos)))

(defun mingus-pos-mlist-> (pos)
  (setf mingus-marked-list (remove (mingus-pos->id pos) mingus-marked-list)))

(defun mingus-pos<->mlist (pos)
  (if (member (mingus-pos->id pos) mingus-marked-list)
      (mingus-pos-mlist-> pos)
    (mingus-pos->mlist pos)))

(defun mingus-toggle-mark (pos)
  (mingus-pos<->mlist pos))

;; do me in color and bold!! And in a single function

(defalias 'mingus-toggle-mark-at-p 'mingus-mark)

(defun mingus-mark ()
  "In Mingus, mark a song for movement or deletion.
 Unmark song when already marked.
 To mark a region, use mingus-mark-region."
  (interactive)
  (mingus-toggle-mark (1- (mingus-line-number-at-pos)))
  (beginning-of-line)
  (let (buffer-read-only)
    (if (member (mingus-pos->id (1- (mingus-line-number-at-pos)))
                mingus-marked-list)
        (mingus-mark-line)
      (mingus-unmark-line)))
  (forward-line 1))

(defun mingus-mark-regexp (re)
  "In Mingus, mark all songs containing regexp RE."
  (interactive "sMark containing regexp: ")
  (save-excursion
    (let (buffer-read-only)
      (goto-char (point-min))
      (while (re-search-forward re nil t)
        (mingus-pos->mlist (1- (mingus-line-number-at-pos)))
        (mingus-mark-line)))))

(defun mingus-unmark-regexp (re)
  "In Mingus, mark all songs containing regexp RE."
  (interactive "sUnmark containing regexp: ")
  (save-excursion
    (let (buffer-read-only)
      (goto-char (point-min))
      (while (re-search-forward re nil t)
        (mingus-unmark-line)
        (mingus-pos-mlist-> (1- (mingus-line-number-at-pos)))))))

(defun mingus-set-marks ()
  (let (buffer-read-only)
    (mapcar (lambda (pos)
              (goto-line (1+ pos))
              (beginning-of-line)
              (mingus-mark-line)
              (forward-line 2))
            (mingus-idlist->poslist mingus-marked-list))))

(defun mingus-clr-mlist ()
  (interactive)
  (setq mingus-marked-list nil)
  (put 'mingus-marked-list :changed t))

;; this one is old
(defun mingus-mark-operation ()
  (interactive)
  (if (or mingus-marked-list)
      (progn
        (let ((command (read-key-sequence "(D)elete or (M)ove here?")))
          (cond ((string-match "d\\|D" command)
                 (mingus-del-marked))
                ((string-match "m\\|M" command)
                 (mingus-move-all))
                (t nil))))
    (message "No marked songs")))

;;;; {{mouse functions}}
 
(defun mingus-insert-at-mouse (ev)
  "Insert song or dir at mouse."
  (interactive "e")
  (mouse-set-point ev)
  (mingus-insert))

(defun mingus-down-at-mouse (ev)
  "Insert song or dir at mouse."
  (interactive "e")
  (mouse-set-point ev)
  (if (cddr ev) 
      (mingus-insert)
    (mingus-down-dir-or-play-song)))

(defun mingus-show-version ()
  (interactive)
  (message "Version of mingus: %s" mingus-version))

;; {{dispatchers}}
 
(defun mingus-help ()
  "Help screen for `mingus'."
  (interactive)
  (cond
   ((get-buffer-window "*Mingus Help*")
    (select-window (get-buffer-window "*Mingus Help*")))
   (t (switch-to-buffer "*Mingus Help*")))
  (when (string= (buffer-string) "")
    (insert mingus-help-text))
  (goto-char (point-min))
  (mingus-help-mode))

(defun mingus-help-mode ()
  "Help screen for `mingus';
\\{mingus-help-map}"
  (set (make-local-variable 'font-lock-defaults)
       '(mingus-help-font-lock-keywords))
  (font-lock-mode t)
  (setq major-mode 'mingus-help-mode)
  (setq mode-name "Mingus-help")
  (use-local-map mingus-help-map)
  (setq buffer-read-only t))

(defun mingus-switch-to-playlist ()
  (cond
   ((get-buffer-window "*Mingus*")
    (select-window (get-buffer-window "*Mingus*")))
   (t
    (switch-to-buffer "*Mingus*")))
  (mingus-playlist-mode))

(defun mingus-switch-to-browser ()
  (switch-to-buffer "*Mingus Browser*")
  (mingus-browse-mode))

(defun mingus-buffer-p (&optional buffer)
  (member (or buffer (buffer-name))
          '("*Mingus Browser*" "*Mingus Help*"
            "*Mingus*" "*Mingus Burns*")))

(defun mingus-git-out (&optional x)
  "Bury all Mingus buffers."
  (interactive)
  (while (mingus-buffer-p)
    (bury-buffer)))

(defun mingus-playlist-mode ()
  "Mingus playlist mode;
see function `mingus-help' for instructions.
\\{mingus-playlist-map}"
  (use-local-map mingus-playlist-map)
  (setq major-mode 'mingus-playlist-mode)
  (setq mode-name "Mingus-playlist")
  (font-lock-mode -1)
  (setq buffer-read-only t)
  (run-hooks 'mingus-playlist-hook))

(defun mingus-browse-mode ()
  "Mingus browse mode.
\\{mingus-browse-map}"
  (let ((res mingus-last-query-results))
    (use-local-map mingus-browse-map)
    (setq major-mode 'mingus-browse-mode)
    (setq mode-name "Mingus-browse")
    (run-hooks 'mingus-browse-hook)
    (set (make-local-variable '*mingus-positions*) nil)
    (setq buffer-read-only t)
    (setq mingus-last-query-results res)))

(defun mingus-mode-line-kill ()
  (interactive)
  (cancel-timer mingus-modeline-timer))

(defvar mingus-mode-line-object
  '(:propertize
    (t mingus-status)
    help-echo (concat
               (mingus-make-mode-line-help-echo)
               (if *mingus-point-of-insertion*
                   (concat "\nPOI: " (cadar
                                      *mingus-point-of-insertion*)))
               "mouse-1: menu or switch to mingus;
 mouse-3: toggle playing;
 mouse-4: vol-up;
 mouse-5: vol-down")
    mouse-face mode-line-highlight local-map
    (keymap
     (mode-line keymap
                (mouse-4 . mingus-vol-up)   ;
                (mouse-5 . mingus-vol-down) ;
                (down-mouse-3 . mingus-toggle)
                (down-mouse-1 . (lambda ()
                                  (interactive)
                                  (if (mingus-buffer-p)
                                      (mouse-major-mode-menu t)
                                    (mingus))))))))



(defconst mingus-mode-line-song-format '((artist album title)(file)(id))
  "Format for showing current song data in modeline")
;; customize this feature
;; :group 'mingus-mode-line
;; :type '(list))
;; fixme: how do i do this correctly??????

(defconst mingus-tag-list
  '(time artist album title file track pos id genre comment))

(defun mingus-remove-non-tags-from-list (list)
  (mingus-make-lists-compatible mingus-tag-list list))

(defun mingus-make-lists-compatible (taglist list)
  (cond
   ((atom list) (if (member list taglist) list))
   ((and (listp (car list)) (null (cdr list)))
    (list (mingus-remove-non-tags-from-list (car list))))
   ((listp (car list)) (cons (mingus-remove-non-tags-from-list (car list))
                             (mingus-remove-non-tags-from-list (cdr list))))
   (t (remove-if (lambda (item) (not (member item taglist))) list))))

(defmacro mingus-bind-plist (plist &rest body)
  "Execute BODY with KEYS from PLIST bound to VALUES; downcase KEYS in the act."
  (let* ((plist (eval plist)))
    `(multiple-value-bind
         ,(loop for i in plist by #'cddr collect
                (intern-soft (downcase (symbol-name i))))
         (quote ,(loop for i in (cdr plist) by #'cddr collect i))
       ,@body)))

(defun mingus-make-status-string ()
  "Make status string of elapsed time, volume, repeat and random status etc."
  (let (time-elapsed time-total)
    (eval
     `(mingus-bind-plist
       (mpd-get-status mpd-inter-conn)
       (let ((percentage (and mingus-mode-line-show-elapsed-percentage
                              (eq state 'play)
                              time-elapsed
                              time-total
                              (format " (%d%%)" (round (/ (float time-elapsed)
                                                          (/ (float time-total)
                                                             100)))))))
         (concat (and mingus-mode-line-show-elapsed-time time-elapsed
                      (format " %2d:%.2d"
                              (/ time-elapsed 60)
                              (mod time-elapsed 60)))
                 percentage
                 (and volume
                      (format
                       " <%d%%%s> "
                       volume
                       (or (and mingus-mode-line-show-random-and-repeat-status
                                (format "%s%s%s"
                                        (if (eq repeat 1) "r" "")
                                        (if (eq random 1) "z" "")
                                        (if (< 0 xfade)
                                            (format "#%d" xfade)
                                          ""))) "")))))))))
(defun mingus-make-cond-exp-aux (item)
  (cond ((atom item) (mingus-make-cond-exp-aux (list item)))
        ((listp (car item))
         (cons (append (cons 'and (car item)) (list (cons 'list (car item))))
               (when (not (null (cdr item)))
                 (mingus-make-cond-exp-aux (cdr item)))))
        ((listp item) (append (cons 'and item) (list (cons 'list item))))))

(defun mingus-make-cond-exp (atom-or-list)
  "Build conditional expression of ATOM-OR-LIST.

Parameter ATOM-OR-LIST is an atom or a list of atoms or a list of lists.

Usage: in code that makes use of a customizable variables of the form for
ATOM-OR-LIST just described.  For instance, if a user defines a variable
`mingus-mode-line-format' as ((artist album title)(artist title)(filename))
this would mean that this user prefers the sequence 'artist album title', but
if that is impossible, the next best would be 'artist title', or - the least
preferred - 'filename'.

Examples:

Return (and ATOM (list ATOM)) for (mingus-make-cond-exp 'ATOM);

Return (and ATOM1 ATOM2 (list ATOM1 ATOM2)) for (mingus-make-cond-exp '(ATOM1
ATOM2));

Return (or (and ATOM1 ATOM2 (list ATOM1 ATOM2)) (and ATOM8 ATOM10 (list ATOM8
ATOM10))) for (mingus-make-cond-exp '((ATOM1 ATOM2)(ATOM8 ATOM10))).
"
  (let ((formula (mingus-make-cond-exp-aux atom-or-list)))
    (cond ((atom (cadr formula)) formula)
          (t (cons 'or formula)))))

(defconst mingus-mode-line-format-to-use
  (mingus-make-cond-exp
   (mingus-remove-non-tags-from-list mingus-mode-line-song-format))
  "Expanded conditional for runtime use")

(defconst mingus-playlist-format
  '((time genre album title comment)
    (time artist album title comment)
    (time artist title)
    (time file)
    (file)))

(defconst mingus-playlist-format-to-use
  (mingus-make-cond-exp
   (mingus-remove-non-tags-from-list mingus-playlist-format)))

(defun mingus-make-mode-line-string ()
  "Make a string to use in the mode-line for Mingus."
  (condition-case nil
      (concat (if (member (getf (mpd-get-status mpd-inter-conn) 'state)
                          '(play pause))
                   (concat
                    (let* ((data (car (mingus-get-songs "currentsong")))
                           (str
                            (mingus-make-song-string
                             data
                             mingus-mode-line-format-to-use
                             " -*- ")))
                      ;; a small (?) side effect, but only if playlist buffer is
                      ;; shown:
                      (and (get-buffer-window-list "*Mingus*")
                           (mingus-triangle-current nil (getf data 'Pos)))
                      (mingus-ldots str mingus-mode-line-string-max))
                    (mingus-make-status-string))))
    ;; (error (cancel-timer mingus-modeline-timerpppp)
    ;;     (cancel-timer mingus-generic-timer))
    ))

(defun mingus-triangle-current (override &optional pos)
  "Put a triangle before currently playing song.

Optional argument POS gives possibility of supplying the currentsong without
making a connection.

Argument OVERRIDE defines whether to treat the situation as new."
  (condition-case nil
      (let ((pos (or pos (getf (mpd-get-status mpd-inter-conn) 'song))))
        (if (or override (/= pos (mingus-get-song-pos)))
            (save-excursion
              (save-window-excursion
                (switch-to-buffer "*Mingus*")
                (let (buffer-read-only)
                  (unless override
                    (goto-char (point-min))
                    (while (re-search-forward
                            mingus-current-song-marker (point-max) t)
                      (replace-match "" nil nil)))
                  (goto-line (1+ pos))
                  (insert mingus-current-song-marker)
                  (put-text-property
                   (point-at-bol)
                   (+ 3 (point-at-bol))
                   'face 'mingus-song-face)
                  (unless override
                    (goto-line (1+ (mingus-get-song-pos)))
                    (beginning-of-line)
                    (when (looking-at mingus-current-song-marker)
                      (delete-char 4))))
                (if (member (mingus-get-song-pos) mingus-marked-list)
                    (mingus-mark-line)))
              (mingus-set-song-pos pos))))))


;;; help echo:
(defconst mingus-mode-line-help-echo-format
  '((artist album title time)
    (file time))
  "Format for showing current song data in help echo")

(defconst mingus-mode-line-help-echo-format-to-use
  (mingus-make-cond-exp
   (mingus-remove-non-tags-from-list mingus-mode-line-help-echo-format))
  "Expanded conditional for runtime use")

(defun mingus-make-mode-line-help-echo ()
  "Make a string to use in the mode-line-help-echo for Mingus."
  (condition-case nil
      (concat (if (member (getf (mpd-get-status mpd-inter-conn) 'state)
                          '(play pause))
                  (concat
                   (let* ((data (car (mingus-get-songs "currentsong")))
                          (str
                           (mingus-make-song-string
                            data
                            mingus-mode-line-help-echo-format-to-use
                            " - ")))
                     str)
                   (mingus-make-status-string))))))
;; filling the buffer:

(defun mingus-playlist (&optional refresh)
  "Fill the playlist buffer so as to reflect current status in most proper way.
Optional argument REFRESH means not matter what is the status, do a refresh"
  (interactive)
  (condition-case err
      (save-window-excursion
        (mingus-switch-to-playlist)
        (when (or
               refresh
               (/= (mingus-get-old-playlist-version)
                   (mingus-get-new-playlist-version))
               (get 'mingus-marked-list :changed))
          (let ((songs (mingus-get-songs "playlistinfo"))
                (buffer-read-only nil)
                (pos (mingus-line-number-at-pos)))
            (put 'mingus-marked-list :changed nil)
            (mingus-set-playlist-version)
            (erase-buffer)
            (if songs
                (progn
                  (insert
                   (replace-regexp-in-string
                    "\n\n" "\n" ;<<< circumvent a bug in libmpdee concerning
                                ;non-unique vorbiscomment tags
                    (mapconcat
                     (lambda (list)
                       (propertize 
			(mingus-make-song-string
			 list
			 mingus-playlist-format-to-use
			 mingus-playlist-separator)
			'mouse-face 'highlight))
                     songs "\n")))
                  (mingus-playlist-set-detail-properties songs)
                  (mingus-set-marks)
                  (mingus-triangle-current t))
              (insert *mingus-header-when-empty*))
            (goto-line pos))))
    (error err)))

(defun mingus-playlist-set-detail-properties (songs)
  (mapc
   (lambda (sublist)
     (goto-line (1+ (plist-get sublist 'Pos)))
     (put-text-property (point-at-bol) (point-at-eol) 'details (list sublist)))
   songs))

(defun mingus-make-song-string (plist expression &optional separator)
  "Make a string from PLIST, using EXPRESSION for the priority of values.

 Concatenate the results for the values with SEPARATOR, where SEPARATOR
 defaults to the string \" - \".

 See the documentation for the variable `mingus-mode-line-format' for the
 syntax of EXPRESSION."
  (let (artist album title albumartist
               track name
               genre date
               composer performer
               comment disc
               time pos id file)
    (eval `(mingus-bind-plist
            ',plist
            (progn (let ((time
                          (and time
                               (format "%02d:%.2d" (/ time 60) (mod time 60))))
                         (pos (and pos (number-to-string pos)))
                         (id (and id (number-to-string id)))
                         (file
                          (and file
                               (replace-regexp-in-string
                                "\\(.*/\\)+" "" file t t 1)))
                         (genre (or genre nil))
			 (albumartist (or albumartist nil))
                         (comment (or comment nil)))
                     (mapconcat 'identity (eval expression)
                                (or ,separator " - "))))))))

(defvar mingus-generic-timer nil)

(defun mingus ()
  "MPD Interface by Niels Giesen, Useful and Simple.

Actually it is just named after that great bass player."
  (interactive)
  (mingus-switch-to-playlist)
  (cond ((boundp 'mode-line-modes)
         (add-to-list 'mode-line-modes mingus-mode-line-object))
        ((boundp 'global-mode-string)
         (add-to-list 'global-mode-string mingus-mode-line-object)))
  (unless (timerp mingus-modeline-timer)
    (setq mingus-modeline-timer
          (run-with-timer 1 1
                          (lambda ()
                            (if (or mingus-mode-always-modeline
                                    (member (buffer-name)
                                            '("*Mingus Browser*"
                                              "*Mingus Help*"
                                              "*Mingus*"
                                              "*Mingus Burns*")))
                                    (setq mingus-status
                                          (mingus-make-mode-line-string))
                                  (setq mingus-status nil))))))
  (mingus-playlist (or (get-buffer-window-list "*Mingus*") t)))

(defun mingus-start-daemon ()
  "Start mpd daemon for `mingus'."
  (interactive)
  (start-process "mpddaemon" nil "mpd"))

(defun mingus-minibuffer-feedback (key)
  "Get a status from mpd, where status is the value for KEY;"
  (let ((val (getf (mpd-get-status mpd-inter-conn) key)))
    (message "Mingus: %S set to %S" key val)))

(defun mingus-shuffle ()
  (interactive)
  (mpd-shuffle-playlist mpd-inter-conn)
  (mingus-playlist 1))

;; (defmacro mingus-define-mpd->mingus (name &rest body)
;;   "Curry an mpd function as a mingus function."
;;   (funcall
;;    (lambda ()
;;      `(defun ,name ()
;;         (interactive)
;;         (,(intern-soft
;;            (replace-regexp-in-string "mingus-" "mpd-" (symbol-name name)))
;;          mpd-inter-conn)
;;         ,@body))))

(defmacro mingus-define-mpd->mingus (name &rest body)
  (funcall
   (lambda ()
     `(defun ,name (&rest args)
        (interactive)
        (apply #',(intern-soft
           (replace-regexp-in-string "mingus-" "mpd-" (symbol-name name)))
         mpd-inter-conn
	 args)
        ,@body))))

(mingus-define-mpd->mingus
 mingus-update
 (and (member 'updating_db (mpd-get-status mpd-inter-conn))
      (message "Updating DB")))

(mingus-define-mpd->mingus mingus-pause (mingus-minibuffer-feedback 'state))

(defalias 'mingus-toggle 'mingus-pause)

(mingus-define-mpd->mingus mingus-prev)

(mingus-define-mpd->mingus mingus-next)

(mingus-define-mpd->mingus mingus-stop)

(defun mingus-boolean->string (bool)
  (case bool
    ((1 t) 'on)
    ((0 nil) 'off)))

(defun mingus-repeat ()
  "Toggle mpd repeat mode."
  (interactive)
  (let ((newval (abs (1- (getf (mpd-get-status mpd-inter-conn) 'repeat)))))
    (mpd-execute-command mpd-inter-conn (format "repeat %d" newval))
    (message "Mingus: repeat set to %S" (mingus-boolean->string newval))))

(defun mingus-random ()
  "Toggle mpd repeat mode."
  (interactive)
  (let ((newval (abs (1- (getf (mpd-get-status mpd-inter-conn) 'random)))))
    (mpd-execute-command mpd-inter-conn (format "random %d" newval))
    (message "Mingus: random set to %S" (mingus-boolean->string newval))))

(defun mingus-setvol (arg)
  (mpd-execute-command mpd-inter-conn
                       (format "setvol %d"
                               (if (numberp arg) arg
                                 (funcall
                                  (case arg
                                    (+ '1+)
                                    (- '1-))
                                  (getf (mpd-get-status mpd-inter-conn)
                                        'volume)))))
  (setq mingus-status (mingus-make-mode-line-string))
  (mingus-minibuffer-feedback 'volume))

(defun mingus-vol-up ()
  (interactive)
  (mingus-setvol '+))

(defun mingus-vol-down ()
  (interactive)
  (mingus-setvol '-))

(defmacro mingus-advice (func-name buffer-name &optional docstring)
                                        ;fixme: should make this dependent on a
                                        ;keyword
  (funcall
   (lambda ()
     `(defadvice ,func-name (around mingus-around-advice activate)
        ,docstring
        (if (string= ,buffer-name (buffer-name))
            ad-do-it
          (message ,(format "Not in %s buffer" buffer-name)))))))

(defun mingus-insertion-point-set-p ()
  (save-window-excursion
    (mingus-switch-to-playlist)
    (when (boundp '*mingus-point-of-insertion*)
        (caar *mingus-point-of-insertion*))))

(defun mingus-get-insertion-number (&optional stringify)
  (save-window-excursion
    (mingus-switch-to-playlist)
    (if *mingus-point-of-insertion*
        (if stringify (number-to-string (caar *mingus-point-of-insertion*))
          (caar *mingus-point-of-insertion*))
      (if stringify (number-to-string (mingus-playlist-length))
        (mingus-playlist-length)))))

(defun mingus-goto-point-of-insertion ()
  "Move point to *mingus-point-of-insertion*.
Switch to *Mingus* buffer if necessary."
  (interactive)
  (mingus-switch-to-playlist)
  (goto-line (mingus-get-insertion-number)))

(mingus-advice mingus-toggle-marked "*Mingus*")
(mingus-advice mingus-goto-current-song "*Mingus*")
(mingus-advice mingus-del-region "*Mingus*")
(mingus-advice mingus-mark-region "*Mingus*")
(mingus-advice mingus-unmark-region "*Mingus*")
(mingus-advice mingus-move-down "*Mingus*")
(mingus-advice mingus-set-insertion-point "*Mingus*")
(mingus-advice mingus-move-up "*Mingus*")
(mingus-advice mingus-mark "*Mingus*")
(mingus-advice mingus-down-dir-or-play-song "*Mingus Browser*")

(mapc 'ad-activate '(mingus-goto-current-song
		     mingus-del-region
		     mingus-down-dir-or-play-song
		     mingus-move-down
		     mingus-move-up
		     mingus-set-insertion-point))

(defmacro mingus-insertion-advice (func-name)
  "Move inserted songs to *mingus-point-of-insertion* after insertion.
 Argument FUNC-NAME is the name of the function to advice."
  `(defadvice ,func-name (around mingus-insertion-advice activate)
     (let ((old-version (mingus-get-new-playlist-version))
           (end-of-playlist (1+ (mingus-playlist-length)))
           (insertion-point (if (mingus-insertion-point-set-p) ;fixme
                                (mingus-get-insertion-number))))
       (when ad-do-it
         (save-window-excursion
           (let* ((new-version (mingus-get-new-playlist-version))
                  (changes (mpd-execute-command
                            mpd-inter-conn
                            (format "plchangesposid %d" old-version)))
                  (howmanysongs (if (car changes) (- new-version old-version)))
                  (song (if (< 1 howmanysongs) "songs" "song")))
             ;;back out when nothing is inserted:
             (when howmanysongs
               (message "Processing request...")
               (if insertion-point
                   (progn
                     ;;move all just inserted songs to their destination:
                     (mingus-move
                      (loop for i in (cdr changes) by #'cddr collect
                            (string-to-number (cdr i)))
                      (make-list howmanysongs insertion-point) nil)
                     ;; some informative message:
                     (message "%d %s added at %s"
                              howmanysongs song
                              (cadar *mingus-point-of-insertion*)))
                 (message "%d %s added at end of playlist."
                          howmanysongs song)) (mingus))))))))

(defun mingus-undo ()
  (mpd-execute-command mpd-inter-conn
                       (format "plchangesposid %d"
                               (1- (mingus-get-new-playlist-version)))))

(mingus-insertion-advice mingus-add-stream)
(mingus-insertion-advice mingus-add-podcast)
(mingus-insertion-advice mingus-insert)

(defmacro mingus-and-play (func-name new-func-name)
  "Transform `insert functions' to \"(insert)-and-play\" functions."
  `(defun ,new-func-name ()
     (interactive)
     (let ((mingus-playing-point (mingus-get-insertion-number)))
       (,func-name)
       (mingus-play mingus-playing-point))))

(mingus-and-play mingus-add-stream mingus-add-stream-and-play)
(mingus-and-play mingus-add-podcast mingus-add-podcast-and-play)
(mingus-and-play mingus-insert mingus-insert-and-play)
(mingus-and-play mingus-load-playlist mingus-load-playlist-and-play)
(mingus-and-play mingus-load-all mingus-load-all-and-play)

(defun* mingus-seek (amount &optional percentage from-start)
  "Seek song played by mpd in seconds or percentage.

 (Prefix) argument AMOUNT specifies movement forward or backward. 
 Defaults to variable `mingus-seek-amount'.
 When PERCENTAGE is specified, seek to PERCENTAGE of song.
 If PERCENTAGE is specified and AMOUNT is negative, seek PERCENTAGE backwards."
  (interactive "p")
  (let* ((data (mpd-get-status mpd-inter-conn))
         (time-total (plist-get data 'time-total))
         (time-elapsed (plist-get data 'time-elapsed))
         (amount (if (and (null from-start) (= 1 amount))
                     mingus-seek-amount
                   amount))
         (amount-final
          (cond (percentage (round (* (/ time-total 100.0) amount)))
                ((not from-start) (+ time-elapsed amount))
                (t amount))))
    (mpd-seek mpd-inter-conn (mingus-cur-song-id) amount-final t)))

(defun mingus-seek-percents (amount)
  "Seek song played by mpd in percentage."
  (interactive "p")
  (cond ((= 1 amount)
         (message "Usage: give prefix argument to specify absolute percentage of song.\n(eg: C-u 40 %% seeks to the point at 40%% of current song)\nNegative argument seeks backward.\n(eg: C-u -10 %% to seek backward 10 percent)"))
        (t
         (mingus-seek amount t))))

(defun mingus-seek-from-start (amount)
  "Seek to PREFIX seconds from start of current song played by mpd."
  (interactive "p")
  (if (= 1 amount)
      (message "Usage: seek to PREFIX seconds from start of current song.
(eg: C-U 30 seeks to thirtieth second of song)")
    (mingus-seek amount nil t)))

(defun mingus-seek-min-sec ()
  "Seek to minute:second point in song."
  (interactive)
  (mingus-seek-from-start
   (mingus-min:sec->secs (read-from-minibuffer
                               "Minutes and seconds (eg 2:30): "))))

(defun mingus-seek-backward (amount)
  "Seek song played by mpd in seconds or percentage backwards."
  (interactive "p")
  (mingus-seek (- 0 (if (= 1 amount) mingus-seek-amount amount))))

(defun mingus-crossfade (p)
  "Set crossfade time for mpd;
 prefix argument of 0 sets crossfade off."
  (interactive "P")
  (mpd-execute-command
   mpd-inter-conn
   (format "crossfade %S" (and p (if (listp p) (car p) p))))
  (if p (message "Mingus: crossfade set to %d" p)))

(defun mingus-cur-line (&optional stringify)
  "In Mingus, return number of song under point"
  (if stringify
      (number-to-string (mingus-line-number-at-pos))
    (mingus-line-number-at-pos)))

(defun mingus-unmark-all ()
  "In Mingus, unset `mingus-marked-list'."
  (interactive)
  (setq mingus-marked-list)
  (mingus-playlist t)
  (message "No songs marked anymore"))

(defun mingus-cur-song-number ()
  "Return number of song currently played by mpd.
 Return nil if no song playing."
  (getf (mpd-get-status mpd-inter-conn) 'song))

(defun mingus-cur-song-id ()
  "Return id of song currently played by mpd.
 Return nil if no song playing."
  (getf (mpd-get-status mpd-inter-conn) 'songid))

(defun mingus-goto-current-song ()
  "In Mingus, move point to currently playing song."
  (interactive)
  (goto-line (or (1+ (mingus-cur-song-number)) 1)))

(defun mingus-playlist-length ()
  "Return length of current mpd playlist."
  (getf (mpd-get-status mpd-inter-conn) 'playlistlength))

(defun mingus-volume ()
  "Return mpd volume as string."
  (number-to-string (getf (mpd-get-status mpd-inter-conn) 'volume)))

(defun mingus-move (from to &optional use-id)
  "Move mpd playlist id FROM to mpd playlist position TO."
  (mpd-move mpd-inter-conn from to use-id))

;; now for my little pearls:
(defvar uplist `(1 . ,(current-time))
  "Cons of the form (COUNT . TIME) for checking repeating commands;
 COUNT is the number of repeated commands;
 TIME is the last time the command has been invoked")

(defun mingus-update-command-list (&optional inc)
  (setcdr uplist (time-to-seconds (current-time)))
  (if inc (incf (car uplist))
    (setcar uplist 1)))

(defun mingus-move-up ()
  "In Mingus, move song at point up one position, visually."
  (interactive)
  (if (= (mingus-line-number-at-pos) 1)
      (progn
        (and (> (car uplist) 1)         ;there were previous calls so do
                                        ;something
             (mingus-move (1- (car uplist)) 0))
        (mingus-update-command-list))   ;set the count of calls to 1
    (let ((buffer-read-only nil))
      (if (and (eq last-command this-command)
                                        ;quick repetition of keypresses,
                                        ;or holding down a key
               (< (- (time-to-seconds (current-time))(cdr uplist)) 0.04))
          (progn (mingus-update-command-list t) ;increase the count of calls
                                        ;with one
                 (transpose-lines 1)    ;change positions in buffer
                 (forward-line -2)
                 (lexical-let ((count (car uplist)))
                   (run-with-timer
                    0.05 nil
                    (lambda ()
                                        ;check if this was the last call
                      (if (= count (car uplist))
                          (progn
                                        ;move the song to its new position
                            (mingus-move
                             (- (+ (car uplist)
                                   (mingus-line-number-at-pos)) 2)
                             (max 0 (- (mingus-line-number-at-pos) 1)))
                            (message
                             "Pos %d moved to pos %d"
                             (max 0 (- (+ (car uplist)
                                          (mingus-line-number-at-pos)) 2))
                             (- (mingus-line-number-at-pos) 1))
                                        ;reset the count
                            (mingus-update-command-list)))))))
                                        ;single keypress handled individually
        (cond ((= (mingus-line-number-at-pos) 1) (mingus-update-command-list))
              (t
               (and
                (mingus-move
                 (1- (mingus-line-number-at-pos))
                 (max (-  (mingus-line-number-at-pos) 1 (car uplist)) 0))
                (transpose-lines 1)
                (mingus-update-command-list)
                (mingus-set-song-pos)
                (forward-line -2)
                (message "Moved 1 song up."))))))))


(defun mingus-move-down ()
  "In Mingus, move song at point down one position, visually."
  (interactive)
  (if (= (mingus-line-number-at-pos) (count-lines (point-min) (point-max)))
      (progn
        (and (> (car uplist) 1)         ;there were previous calls so do
                                        ;something
             (mingus-move
              (- (mingus-line-number-at-pos) (car uplist))
              (1- (mingus-line-number-at-pos))))
        (mingus-update-command-list))   ;set the count of calls to 1
    (let ((buffer-read-only nil))
      (if (and (eq last-command this-command)
               (< (- (time-to-seconds (current-time))(cdr uplist)) 0.04))
                                        ;quick repetition of keypresses, or
                                        ;holding down a key
          (progn (mingus-update-command-list t) ;increase the count of calls
                                        ;with one
                 (forward-line 1)       ;change positions in buffer
                 (transpose-lines 1)
                 (forward-line -1)
                 (lexical-let ((count (car uplist)))
                   (run-with-timer
                    0.05 nil
                    (lambda ()
                      (if (= count (car uplist))
                                        ;check if this was the last call
                          (progn
                            (mingus-move
                             (max (- (mingus-line-number-at-pos)(car uplist)) 0)
                             (- (mingus-line-number-at-pos) 1)) ;move the song
                                                                ;to its new
                                                                ;position
                            (message "Pos %d moved to pos %d"
                                     (max (- (mingus-line-number-at-pos)
                                             (car uplist)) 0)
                                     (- (mingus-line-number-at-pos) 1))
                            (mingus-update-command-list) ;reset the count
                            ;; (mingus-set-song-pos)
                            ))))))
        (cond ((= (mingus-line-number-at-pos)
                  (count-lines (point-min) (point-max)))
               (mingus-update-command-list))
                                        ;just a single keypress, handled
                                        ;individually
              (t
               (and
                (mingus-move (- (mingus-line-number-at-pos) 1)
                             (mingus-line-number-at-pos))
                (mingus-update-command-list)
                (mingus-set-song-pos)
                (forward-line 1)
                (transpose-lines 1)
                (forward-line -1)
                (message "Moved 1 song down."))))))))

(defun mingus-move-all ()
  "In Mingus, move all marked songs to current position in buffer."
  (interactive)
  (if (null mingus-marked-list)
      (message "No marked songs")
    (mingus-move mingus-marked-list
                 (make-list (length mingus-marked-list)
                            (1- (mingus-line-number-at-pos))) t)
    (mingus-playlist t)))



(defmacro mingus-define-region-mark-operation
  (name function &optional docstring)
  (funcall
   (lambda ()
     `(defun ,name (beg end)
        ,docstring
        (interactive "r")
        (let* ((buffer-read-only nil)
               (beg (1- (mingus-line-number-at-pos beg)))
               (end (1- (if (bolp)
                            (mingus-line-number-at-pos end)
                          (1+ (mingus-line-number-at-pos end)))))
               newsongs
               (mlist-as-pos
                (remove nil
                        (if mingus-marked-list
                            (mapcar 'mingus-id->pos mingus-marked-list))))
               (howmanysongs (- end beg)))
          (dotimes (count howmanysongs)
            (setq newsongs (cons (+ beg count) newsongs)))
          (setq mingus-marked-list
                (,function mingus-marked-list
                           (mapcar 'mingus-pos->id newsongs)))
          (mingus-playlist t))))))

(mingus-define-region-mark-operation
 mingus-mark-region union
 "In Mingus, mark region between BEG and END for subsequent operations.")
(mingus-define-region-mark-operation
 mingus-unmark-region set-difference
 "In Mingus, unmark region between BEG and END.")

(defun mingus-toggle-marked ()
  "In Mingus, toggle wich songs are marked."
  (interactive)
  (setq mingus-marked-list
        (nset-difference
         (mapcar
          (lambda (song-item)
            (getf song-item 'Id))
          (mingus-get-songs "playlistinfo")) mingus-marked-list))
  (mingus-playlist t)
  )

(defun mingus-del ()
  (interactive)
  (let ((pos (1- (mingus-line-number-at-pos)))
        (buffer-read-only))
    (mingus-pos-mlist-> (1- (mingus-line-number-at-pos)))
    (mpd-delete mpd-inter-conn pos)
    (delete-region (point-at-bol) (point-at-bol 2))
    (mingus-set-playlist-version)))

(defun mingus-reset-point-of-insertion ()
  "Reset the variable `*mingus-point-of-insertion*'.

This is according to the situation where the song at point will have been
deleted."
  (cond ((and *mingus-point-of-insertion*
              (= (mingus-get-insertion-number) (mingus-line-number-at-pos)))
         (mingus-unset-insertion-point))
        ((and *mingus-point-of-insertion*
              (> (mingus-get-insertion-number) (mingus-line-number-at-pos)))
         (decf (caar *mingus-point-of-insertion*)))))

(defun mingus-del-region (beg end)
  "In Mingus, delete region.
 Leave `mingus-marked-list' intact."
  (interactive "r")
  ;;no need for consuming computation and bindings when whole buffer is selected
  (if (and (= beg (point-min)) (= end (point-max)))
      (mingus-clear t)
    (let* ((buffer-read-only nil)
           (beg (1- (mingus-line-number-at-pos beg)))
           (end (1- (if (bolp)
                        (mingus-line-number-at-pos end)
                      (1+ (mingus-line-number-at-pos end)))))
           (howmanysongs (- end beg))
           deletablelist
           (mlist-as-pos
            (remove nil
                    (if mingus-marked-list
                        (mapcar 'mingus-id->pos mingus-marked-list)))))
      ;; make the shell-command-string
      (dotimes (count howmanysongs)
        (setq deletablelist (cons (+ beg count) deletablelist)))
      ;; delete the files
      (let ((newmlist (mapcar 'mingus-pos->id
                              (set-difference
                               mlist-as-pos
                               (intersection mlist-as-pos deletablelist)))))
        (mpd-delete mpd-inter-conn deletablelist)
        (setq mingus-marked-list newmlist))
      ;; remove all songs that are deleted from the mingus-marked-list (mapcar
      ;; 'mingus-pos->id (set-difference '(3 111 4 5) (intersection '(3 111) '(2
      ;; 3 111))))
      (mingus))))

(defun mingus-delete-lines (lines)
  "Delete every line in LINES, where 0 is the first line in the buffer.
 LINES can also be a single line."
  (let ((lines (etypecase lines
                 (list (sort lines '<))
                 (integer (list lines)))))
    (save-excursion
      (mapc (lambda (lines)
              (goto-line (1+ lines))
              (delete-region (point-at-bol) (point-at-eol))) lines)
      (goto-char (point-min))
      (delete-matching-lines "^$"))))

(defun mingus-del-marked ()
  "Delete songs marked in *Mingus* buffer."
  (interactive)
  (let ((buffer-read-only nil)
        (cur-line (mingus-line-number-at-pos)))
    (if mingus-marked-list
        (when (y-or-n-p (format "Remove %d marked songs? "
                                (length mingus-marked-list)))
          (progn
            (let ((mlist-as-pos (mingus-idlist->poslist mingus-marked-list)))
              (mpd-delete mpd-inter-conn mingus-marked-list t)
              (mingus-delete-lines mlist-as-pos))
            (goto-line (- cur-line (count-if (lambda (item) (> cur-line item))
                                             mingus-marked-list)))
            (setq mingus-marked-list nil)))
      (mingus-del)))
  (when (eobp)
    (delete-region (point-at-bol) (point-at-bol 2))
    (beginning-of-line)))

(defun mingus-del-other-songs ()
  "In the *Mingus* buffer, delete all but the songs in `mingus-marked-list'."
  (interactive)
  (when mingus-marked-list
    (mpd-delete mpd-inter-conn (set-difference
                                (loop for i in
                                      (mingus-get-songs "playlistinfo")
                                      collect (getf i 'Id))
                                mingus-marked-list) t)
    (mingus-playlist)
    (message "Other songs deleted")))

(defun mingus-play (&optional position)
  "Start playing the mpd playlist, only if not yet playing.
 When called with argument POSITION, play playlist id POSITION."
  (interactive)
  (mpd-play mpd-inter-conn (or position (1- (mingus-line-number-at-pos)))))

(defun mingus-play-pos (position)
  "Play song in mpd playlist at position specified by prefix argument."
  (interactive "p")
  (mingus-play (number-to-string position)))

(defun mingus-clear (&optional dontask)
  "Clear mpd playlist;
 Does prompting.
 Optional argument DONTASK means no prompting."
  (interactive "P")
  (if (or dontask (yes-or-no-p "Clear the playlist? "))
      (progn (mpd-clear-playlist mpd-inter-conn)
             (with-current-buffer "*Mingus*"
               (mingus-playlist t)))
    (message "Playlist not cleared")))

(defun mingus-load-all (&optional and-play)
  "Load all songs in mpd database into mpd playlist.
 Optional argument AND-PLAY means start playing after loading."
  (interactive "P")
  (if and-play (mingus-load-all-and-play)
    (when (yes-or-no-p "Load the WHOLE mpd database? " )
      (mpd-clear-playlist mpd-inter-conn)
      (mpd-execute-command mpd-inter-conn "add /")
      (with-current-buffer "*Mingus*"
        (mingus-playlist t)))))

(defun mingus-crop ()
  "Crop mpd playlist."
  (interactive)
  (condition-case err
      (let (song)                       ;silence the compiler
        (mingus-bind-plist
         (mpd-get-status mpd-inter-conn)
         (let (list)
           (and (> playlistlength 1)
                (mpd-delete mpd-inter-conn
                            (remove song
                                    (dotimes (count playlistlength list)
                                      (push count list))))
                (save-window-excursion
                  (mingus))))))
    (error "Mingus error: %s" err)))

(defun mingus-add (string &optional mingus-url)
  "In Mingus, add a song."
  (mingus-exec
   (format "add %s" (if mingus-url
                        string
                      (mpd-safe-string string))))
  ;;hmm, where IS this mingus-url for?
  (save-window-excursion (mingus)))


(defcustom mingus-stream-alist
  '(("CRo 1 - Radiozurnal (czech)" . "http://amp1.cesnet.cz:8000/cro1-256.ogg")
    ("CRo 2 - Praha (czech)" . "http://amp1.cesnet.cz:8000/cro2-256.ogg")
    ("CRo 3 - Vltava czech)" . "http://amp1.cesnet.cz:8000/cro3-256.ogg")
    ("open radio" . "http://open-radio.nl:8000/org.ogg")
    ("Radio Rota" . "http://streamer.radiorota.cz:8000/rota64.ogg"))
  "Alist of radio stations to be used by the function `mingus-add-stream'."
  :group 'mingus
  :type '(alist))

(defcustom mingus-podcast-alist ()
  "Alist of podcasts to be used by the function `mingus-add-podcast'."
  :group 'mingus
  :type '(alist))

(defun mingus-add-stream (&optional and-play)
  "Add a url-stream to the mpd playlist.
When point is at the beginning of a url, add that url;
 In w3m, add the link under point;
 Completion is provided by the entries in `mingus-stream-alist' to choose from.
 With prefix argument, instantly play the insertion.
Optional prefix argument AND-PLAY says: and play it now!"
  (interactive "P")
  (if and-play (mingus-add-stream-and-play)
    (let ((url (mingus-completing-read-allow-spaces "Url: "
                                                    mingus-stream-alist nil nil
                                             (mingus-extract-url))))
      (case (mingus-playlist-type
             (setq url (or
                        (cdr (assoc url mingus-stream-alist))
                        (assoc url mingus-stream-alist)
                        url)))
        (m3u (mingus-add (mingus-site-to-string)))
        (pls (mingus-add-podcast))
        (t (mingus-add url t))))))

(defun mingus-extract-url ()
  "Return url at point;
If no url at point, return nil."
  (let ((url (and (not (member major-mode
                               '(mingus-playlist-mode mingus-browse-mode)))
                  (or (plist-get (text-properties-at (point)) 'w3m-href-anchor)
                      (thing-at-point-url-at-point)))))
    (when (not (null url))
      (if (string-match "\?" url)
          (replace-match "\\?" nil t url)
        url))))

(defun mingus-playlist-type (url)
  "Return type of playlist for URL."
  (cond ((string-match ".*\.[mM]3[Uu]" (or url "something else")) 'm3u)
        ((string-match ".*\.\\([pP][lL][sS]\\|[aA][sS][xX]\\)"
                       (or url "something else")) 'pls)
        (t nil)))

(defun mingus-site-to-string (&optional url)
  "Return contents of URL as string."
  (let ((url (or url (mingus-extract-url))))
    (with-current-buffer
        (url-retrieve-synchronously url)
      (buffer-string))))

(defun mingus-add-podcast (&optional and-play)
  "Add all streams in podcast at point.
Actually it tries to retrieve any stream from a given url.
 The variable `mingus-podcast-alist' can be used for input selection.
 With prefix argument, play directly."
  (interactive "P")
  (if and-play (mingus-add-podcast-and-play)
    (do*  ((item nil (substring (match-string 0 xml) 0))
          (res nil (if (not (member item res)) (push item res) res))
          (xml (or (mingus-site-to-string)
                   (mingus-site-to-string
                    (let ((url (completing-read "Add a podcast: "
                                                mingus-podcast-alist)))
                      (or (cdr (assoc url mingus-podcast-alist))
                          (assoc url mingus-podcast-alist)
                          url)))))
          (count 0 nil))
        ;; get out of loop when no match is found
        ((not (string-match mingus-stream-regexp xml (or count (match-end 0))))
         (if (null res) (message "No valid podcast or empty podcast")
           (do ((end-result (car res) (concat end-result '(? ) (cadr res)))
                (res res (cdr res)))
               ((null res)
                (mapcar 'mingus-add (split-string end-result)))
             nil))))))

(defun mingus-browse ()
  "Switch to buffer *Mingus Browser* and start the Mingus browsing experience."
  (interactive)
  (cond
   ((get-buffer-window "*Mingus Browser*")
    (select-window (get-buffer-window "*Mingus Browser*")))
   ((bufferp (get-buffer "*Mingus Browser*"))
    (mingus-switch-to-browser))
   (t
    (mingus-switch-to-browser)
    (let ((buffer-read-only nil))
      (erase-buffer)                    ;only if not yet in browsing mode
      (goto-char (point-min))
      (mingus-down-dir-or-play-song)))))

(defun mingus-browse-invisible ()
  "Hide $PWD in file and directory names in *Mingus Browser* buffer."
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (put-text-property
       (point-at-bol)
       (or (re-search-forward ".*/" (point-at-eol) t 1) (point-at-bol))
       'invisible t)
      (forward-line 1))))

;; fixme: Problem if a playlist is contained within.
(defun* mingus-add-song-at-p (&optional beg end)
  "Add song or directory at point.
If active region, add everything between BEG and END."
  (interactive "r")
  (let ((song (buffer-substring-no-properties
               (or beg (point-at-bol))
               (or end (point-at-eol)))))
    (mpd-execute-command mpd-inter-conn
                         (mapconcat
                          (lambda (song)
                            (format "add %s" (mpd-safe-string song)))
                          (split-string song "\n") "\n"))))

(defun mingus-down-dir-or-play-song ()
  "In *Mingus Browser* buffer, go to dir at point, or play song at point."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (cond
     ((mingus-songp)                    ;is it a song?
      (mingus-insert))
     ((mingus-playlistp)                ;is it a playlist?
      (mpd-load-playlist mpd-inter-conn (cdr (mingus-playlistp))))
     (t                                 ;it's  a directory!
      (push (mingus-line-number-at-pos) *mingus-positions*)
      (mingus-ls
       (buffer-substring-no-properties
        (point-at-bol) (point-at-eol)))))))

;; Idea: bind cdr and car of text-property 'details to two vars. Act upon these
;; vars.
(defun mingus-get-details-for-song ()
  "Get details for song from text-property `details'"
  (get-text-property
   (+ (if (and (equal major-mode 'mingus-playlist-mode)
	       (= (1- (mingus-line-number-at-pos)) (mingus-get-song-pos)))
	  (length mingus-current-song-marker)
	0)
      (point-at-bol)) 'details))

(defun _mingus-playlist-get-filename-at-p ()
  (plist-get (car (mingus-get-details-for-song)) 'file))

(defun mingus-playlistp ()
  "In *Mingus Browser* buffer, is thing-at-p a playlist.
 Return cons of form '(\"playlist\" . playlistname) or nil if not a playlist."
  (assoc "playlist" (mingus-get-details-for-song)))

(defun mingus-songp ()
  "In *Mingus Browser* buffer, is thing-at-p a song.
 Return cons of the form '(\"song\" . songname) or nil if not a song."
  (assoc "file" (mingus-get-details-for-song)))

(defun mingus-directoryp ()
  "In *Mingus Browser* buffer, is thing-at-p a directory.
 Return cons of form '(\"directory\" . dirname) or nil if not a directory."
  (assoc "directory" (mingus-get-details-for-song)))

(defun _mingus-string->parent-dir (child)
  (if (string-match "^https?://" child) ;URLS are illegal here
      (error "Not a local file!")
    (string-match "\\(.*\\)/" child)
    (match-string 1 child)))

(defun mingus-ls (string)
  "List songs/dirs in directory STRING in dedicated *Mingus Browser* buffer."
  (mingus-switch-to-browser)
  (save-excursion)
  (let ((buffer-read-only nil)
        (newcontents
	 (sort*
	  (loop for i in
		(remove-if
		 (lambda (item) (or 
                            (null (cdr item))
                            (not (string-match (car item)
                                               "file|directory|playlist"))))
		 (cdr (mingus-exec (format "lsinfo %s"
					   (mpd-safe-string string)))))
		collect i)
          #'mingus-logically-less-p 
	  :key #'cdr)))
    (erase-buffer)
    (if (null newcontents)
        (message "No songs in database; check your mpd settings")
      (mapc (lambda (item)
                (insert (propertize (cdr item) 'mouse-face 'highlight) "\n")
                (put-text-property (point-at-bol 1) (point-at-eol -1)
                                   'details (list item))
                (put-text-property (point-at-bol 1) (point-at-eol -1)
                                   'face
                                   (cond
                                    ((string= (car item) "playlist")
                                     'mingus-playlist-face)
                                    ((string= (car item) "directory")
                                     'mingus-directory-face)
                                    ((string= (car item) "file")
                                     'mingus-song-file-face))))
            newcontents))
    (mingus-browse-invisible)))

(defun mingus-browse-to-song-at-p ()
  (interactive)
  (let ((file (_mingus-playlist-get-filename-at-p)))
    (mingus-browse-to-file file)))

(defun mingus-browse-to-file (file)
  (mingus-ls (_mingus-string->parent-dir file))
  (goto-char (point-min))
  (re-search-forward (file-name-nondirectory file) nil t)
  (beginning-of-line))

(defun mingus-browse-to-dir (dir)
  (mingus-ls dir)
  (goto-char (point-min)))

(defun mingus-dir-up ()
  "In Mingus-Browse, go up one directory level."
  (interactive)
  (end-of-line)
  (let ((buffer-read-only nil)
        (goal (buffer-substring-no-properties
               (or (re-search-backward "/" (point-at-bol) t 1) (point))
               (point-at-bol))))
    (end-of-line)
    (if (re-search-backward "/" (point-at-bol) t 2)
        (progn
          (mingus-ls
           (buffer-substring-no-properties (point-at-bol) (point))))
      (progn
        (mingus-ls "")))
    (re-search-backward goal)))

(defun mingus-insert (&optional and-play)
  "In *Mingus Browser* buffer, insert stuff under point or region into playlist.

 Anywhere else, call `mingus-add-read-input'.
 With prefix argument, instantly play the insertion."
  (interactive "P")
  (if and-play (mingus-insert-and-play)
    (if (not (eq major-mode 'mingus-browse-mode))
        (mingus-add-read-input)
      (if (mingus-mark-active)
          (mingus-add-song-at-p (min (mark)(point))(max (mark)(point)))
                                        ;FIXME: cannot handle playlists in an
                                        ;active region right now
        (cond
         ((mingus-playlistp) (mpd-load-playlist mpd-inter-conn
                                                (cdr (mingus-playlistp))))
         (t (mingus-add-song-at-p)))))
    (if (eq major-mode 'mingus-playlist-mode)
        (mingus)
      (unless (mingus-mark-active) (forward-line 1)))))

(defun* mingus-set-insertion-point (&optional p)
  "In Mingus, set *mingus-point-of-insertion* for new songs.
They will be added after this point.
Prefix argument shows value of *mingus-point-of-insertion*, and moves there."
  (interactive "P")
  (cond ((string= "*Mingus*" (buffer-name))
         (cond ((null p)
                (set '*mingus-point-of-insertion*
                     (list (list (mingus-line-number-at-pos)
                                 (buffer-substring-no-properties
                                  (point-at-bol) (point-at-eol))))))
               (*mingus-point-of-insertion*
                (goto-line (caar *mingus-point-of-insertion*))))
         (message "*mingus-point-of-insertion* set at %s"
                  (or (cadar *mingus-point-of-insertion*)
                      "end of playlist (unset)")))
        (t (message "Not in \"*Mingus*\" buffer"))))
                                        ;fixme do something with text-properties
                                        ;here once I find out how to...

(defun mingus-set-insertion-point-at-currently-playing-song ()
  (interactive)
  (save-window-excursion
    (mingus-switch-to-playlist)
    (mingus-goto-current-song)
    (mingus-set-insertion-point)))

(defun mingus-unset-insertion-point ()
  "Unset Mingus' *mingus-point-of-insertion*."
  (interactive)
  (save-window-excursion
    (mingus-switch-to-playlist)
    (set '*mingus-point-of-insertion* nil)
    (message "*mingus-point-of-insertion* unset")))

;; (@> "playlists")

(defun mingus-list-playlists ()
  (remove nil (mapcar (lambda (item)
                        (if (string= (car item) "playlist") (cdr item)))
                      (cdr (mpd-execute-command mpd-inter-conn "lsinfo")))))

(defun mingus-load-playlist (&optional and-play)
  "Load an mpd playlist.
Append playlist to current playlist.
With prefix argument, instantly play the insertion.
Optional argument AND-PLAY means start playing thereafter."
  (interactive "P")
  (if and-play (mingus-load-playlist-and-play)
    (let ((lst (mingus-list-playlists)))
      (if (null lst)
          (message "No playlist present")
        (let* ((playlist  (mingus-completing-read-allow-spaces "Load playlist: "
                                                        lst nil t))
               (quoted-playlist (mpd-safe-string playlist)))
          (if (string= "" playlist)
              (message "No playlist selected")
            (progn
              (mpd-load-playlist mpd-inter-conn quoted-playlist)
              (message (format "Playlist %s loaded" playlist))
              (mingus))))))))

(defun mingus-save-playlist ()
  "Save an mpd playlist."
  (interactive)
  (let* ((lst (mingus-list-playlists))
         (playlist (mingus-completing-read-allow-spaces
                    "Save playlist as: "
                    lst nil nil))
         (quoted-playlist (mpd-safe-string playlist)))
    (if (null playlist)
        (message "No name for playlist provided, won't save...")
      (mpd-remove-playlist mpd-inter-conn quoted-playlist)
      (mpd-save-playlist mpd-inter-conn quoted-playlist)
      (message "Playlist saved as %s" playlist))))

(defun mingus-remove-playlist ()
  "Remove an mpd playlist"
  (interactive)
  (let ((list (mingus-list-playlists)))
    (cond ((null list)
           (message "No playlist to remove"))
          (t
           (let* ((playlist  (mingus-completing-read-allow-spaces
                              "Remove playlist: "
                              list nil nil))
                  (quoted-playlist (mpd-safe-string playlist)))
             (if (null list)
                 (message "No name for playlist provided, won't remove")
               (progn
                 (mpd-remove-playlist mpd-inter-conn quoted-playlist))
               (message "Playlist %s removed" playlist)))))))

 
;; {{minibuffer addition of tracks/dirs}}
(defun mingus-complete-path (input)
  "Complete mpd path based on INPUT.
INPUT is supposed to be supplied by current minibuffer contents."
  (let ((res (mingus-exec (concat "lsinfo " (mpd-safe-string input)))))
    (append
     (if (and (car res)
                                        ;let the dir itself be sufficient too
              (not (string= "" input)))
                                        ;do not show empty string or single "/"
         (list (replace-regexp-in-string "/*$" "/" input)))
                                        ;mingus-switch-car-and-cdr
     (mapcar 'cdr
             (cdr (mingus-exec
                   (concat "lsinfo " (mpd-safe-string
                                      (if (car res)
                                          input
                                        ;search on dir if no match found here:
                                        (replace-regexp-in-string
                                         "\\(/\\|[^/]*\\)$" "" input))))))))))

(defun mingus-complete-from-minibuffer (prompt &optional predicate require-match
					       initial-input hist def
					       inherit-input-method)
  (completing-read
   prompt
    (if (fboundp 'completion-table-dynamic)
	(completion-table-dynamic (function mingus-complete-path))
      (dynamic-completion-table mingus-complete-path)) 
    predicate require-match
    initial-input hist def
    inherit-input-method))

(defun mingus-add-read-input ()
  "Add song or dir to mpd playlist using minibuffer input.

Complete in the style of the function `find-file'."
  (interactive)
  (mingus-add 
   (mingus-complete-from-minibuffer "Add to playlist: " nil t)))

(defun mingus-update-partially ()
  "Update the database partially."
  (interactive)
  (let ((updatable 
	 (mingus-complete-from-minibuffer 
	  "Update database for: " nil nil)))
    (mingus-update updatable)))

(defun mingus-update-thing-at-p ()
  "Update the database partially for song or directory at point."
  (interactive)
  (let ((updatable 
	 (cdar 
	  (mingus-get-details-for-song))))
    (if (listp updatable)		
	;;have to fix weird differences in details tss..
	(setq updatable (car updatable)))
    (mingus-update updatable)))

' (defun mingus-switch-car-and-cdr (cons)
    (cons (cdr cons) (car cons)))
 
;;; Searching section
(defun mingus-completing-search-type (type query)
  "Both TYPE and QUERY must be supplied as string."
  (if (string= type "regexp on filename") nil
    (remove-duplicates
     (mapcar (lambda (item)
               (downcase (plist-get item (if (string= "filename" type) 'file
                                        ;special case...
                                           (intern-soft
                                            (concat (capitalize type)))))))
             (loop for i in
                   (mingus-get-songs
                    (format "search %s %s" type (mpd-safe-string query)))
                   if (eq 'file (car i)) collect i))
     :test 'string=)))

(defun mingus-query (&optional type)
  "Query the mpd database.

Show results in dedicated *Mingus Browser* buffer for further selection.  Use
apropos matching, even with function `icicle-mode' turned on (no switching
possible).  Optional argument TYPE predefines the type of query."
  ;; Author does not know how to handle this stuff well.

  ;; Too bad that completing-read does not simply allow one to specify a
  ;; function to return a list, but that once one specifies a function, it has
  ;; got to handle all possible cases. Handling it with dynamic-completion-table
  ;; strips the list of apropos matches.
  (interactive)
  (let* ((type (or type (mingus-completing-read-allow-spaces
                         "Search type: "
                         '("album" "artist" "genre"
			   "composer" "filename" "title"
			   "regexp on filename")
                         nil t)))
         (buffer (buffer-name))
         (pos (point))
         (query (mingus-completing-read-allow-spaces
                 (format "%s query: " (capitalize type))
                 (lambda (string predicate mode)
                   (with-current-buffer
                       (let ((window (minibuffer-selected-window)))
                         (if (window-live-p window)
                             (window-buffer window)
                           (current-buffer)))
                     (cond ((eq mode t)
                            (mingus-completing-search-type type string))
                           ((not mode)
                            (let ((hits
                                   (mingus-completing-search-type type string)))
                              (if hits
                                  (if (= 1 (length hits))
                                      (car hits)
                                    (if (fboundp 'icicle-expanded-common-match) ;changed from icicle-longest-common-match
                                        (icicle-expanded-common-match
                                         string hits)
                                      (try-completion string hits))))))
                           (t (test-completion
                               string
                               (mingus-completing-search-type type string)
                               predicate))))))))
    (mingus-query-do-it type query pos buffer)))

(defun mingus-query-regexp ()
  "Query the filenames in the mpd database with a regular expression;
Show results in dedicated *Mingus Browser* buffer for further selection."
  (interactive)
  (mingus-query "regexp on filename"))

(defun mingus-query-do-it (type query pos buffer)
  "Perform the query provided by either `mingus-query' or `mingus-query-regexp'.
Argument TYPE specifies the kind of query.
Argument QUERY is a query string.
Argument POS is the current position in the buffer to revert to (?)."
  (mingus-switch-to-browser)
  (let ((buffer-read-only nil)
        (prev (buffer-string)))
    (erase-buffer)
    (cond ((string-match "regexp on filename" type)
           (mapc (lambda (item) (and (string= (car item) "file")
                                     (string-match query (cdr item))
                                     (insert (cdr item) "\n")))
                 (cdr (mingus-exec "listall"))))
          (t (insert
              (mapconcat 'identity
                         (sort*
                          (loop for i in (cdr (mingus-exec
                                              (format "search %s %s"
                                                      type
                                                      (mpd-safe-string query))))
                               if (string= (car i) "file")
                               collect (cdr i))
                          #'mingus-logically-less-p)
                         "\n"))))
    (mingus-browse-invisible)
    (goto-char (point-min))
    (mingus-revert-from-query pos prev buffer)))

(defun mingus-revert-from-query (pos prev buffer)
  "Restore previous situation when `mingus-query-do-it' returned nothing."
  (cond ((eobp)
         (insert prev)
         (switch-to-buffer buffer)
         (goto-char pos)
         (message "No hits!"))
        (t
         (setq mode-name "Query results")
         (set (make-local-variable 'mingus-last-query-results)
              (buffer-string)))))

(defun mingus-last-query-results ()
  "Show last query results again in dedicated *Mingus Browser* buffer"
  (interactive)
  (cond ((save-window-excursion
           (mingus-switch-to-browser)
           (null mingus-last-query-results))
         (message "No succesful search yet"))
        (t (switch-to-buffer "*Mingus Browser*")
           (setq mode-name "Query results")
           (let ((buffer-read-only nil))
             (goto-char (point-min))))))

(defalias 'mingus-search 'mingus-query)

(defun mingus-browse-sort ()
  "In *Mingus Browser*, sort hits."
  (interactive)
  (if (eq major-mode 'mingus-browse-mode)
      (let ((re "\\(^\\|/\\)[^\/]+$")
            list
            buffer-read-only
            (line (mingus-line-number-at-pos)))
        (goto-char (point-max))
        (while (re-search-backward re nil t)
          (push (cons
                 (match-string-no-properties 0)
                 (buffer-substring (point-at-bol) (point-at-eol)))
                list))
        (erase-buffer)
        (mapc (lambda (item)
                (insert (concat (cdr item) "\n")))
              (sort* list (lambda (str1 str2)
                            (if (get this-command 'reverse)
                                (null (mingus-logically-less-p (car str1) (car str2)))
                              (mingus-logically-less-p (car str1) (car str2))))))
        (put this-command 'reverse (null (get this-command 'reverse)))
        (goto-line line))
    (message "Buffer not in mingus-browse-mode")))

;;;; {{Wake up call}}
(defun mingus-date-to-sec-from-epoch (datestring)
  (apply #'encode-time
         (mapcar #'string-to-number
                 (list
                  (substring datestring 12 14)  ;seconds
                  (substring datestring 10 12)  ;minute
                  (substring datestring 8 10)   ;day
                  (substring datestring 6 8)    ;month
                  (substring datestring 4 6)
                  (substring datestring 0 4)))))

(defun mingus-wake-up-call (&optional p)
  "Set a time for mingus to start playing.
TIME will be interpreted to always lie in the future.
With prefix argument, cancel the wake-up call.

The timer-object is referenced to by the variable `mingus-wake-up-call'"
  (interactive "P")
  (cond ((and p (timerp mingus-wake-up-call))
         (cancel-timer mingus-wake-up-call)
         (message "Wake-up call cancelled"))
        (t
         (setq
          mingus-wake-up-call
          (run-at-time
           (let ((time (mingus-date-to-sec-from-epoch
                        (concat
                         (format-time-string "%Y%m%d")
                         (format "%02d" (read-number "Hour: "))
                         (format "%02d" (read-number "Minute: ")) "00"))))
             (if (time-less-p time (current-time))
                 (time-add (days-to-time 1) time)
               time))
           nil 'mingus-play))
         (message (format
                   "%sake sure you have a playlist set before dozing off!"
                   (if (=  0 (mingus-playlist-length)) "Playlist is empty, m"
                     "M"))))))

;;;; {{Shell/Dired}}
;;; Functions for retrieving the true filenames for interaction with a shell and
;;; `dired'; this needs reviewing for consistency and use
(defun _mingus-get-parent-dir ()
  "Get parent dir of song at point."
  (_mingus-string->parent-dir
   (mingus-get-filename)))

(defun _mingus-burner-get-filename-at-p ()
  (plist-get (mingus-get-details-for-song) :file))

(defun _mingus-make-absolute-filename (filename)
  "Turn a filename relative to `mingus-mpd-root' into an absolute one."
  (if (string-match "^https?://" filename) ;URLS are legal here (!)
      filename
    (concat mingus-mpd-root filename)))

(defun _mingus-get-filename-from-playlist-relative ()
  "In buffer *Mingus*, retrieve filename-at-p, relative to `mingus-mpd-root'."
  (_mingus-playlist-get-filename-at-p))

(defun _mingus-get-filename-from-burner-relative ()
  "In buffer *Mingus*, retrieve filename-at-p, relative to `mingus-mpd-root'."
  (_mingus-burner-get-filename-at-p))

(defun _mingus-get-filename-from-browser-relative ()
  "In *Mingus Browse*, retrieve filename-at-p, relative to `mingus-mpd-root'."
  (buffer-substring-no-properties (point-at-bol) (point-at-eol)))

(defun _mingus-get-filename-relative ()
  "Retrieve filename of song at point, relative to `mingus-mpd-root'"
  (case major-mode
    ('mingus-browse-mode
     (_mingus-get-filename-from-browser-relative))
    ('mingus-playlist-mode
     (_mingus-get-filename-from-playlist-relative))
    ('mingus-burn-mode
     (_mingus-get-filename-from-burner-relative))))

(defun _mingus-get-filename-absolute ()
  "Get absolute filename for song-at-p."
  (_mingus-make-absolute-filename (_mingus-get-filename-relative)))

(defalias 'mingus-get-filename
  '_mingus-get-filename-absolute "Get absolute filename for song-at-p.")

(defun mingus-get-filename-for-shell ()
  "Retrieve filename of song at point, and shell-quote it."
  (shell-quote-argument (mingus-get-filename)))

;; Unused, but probably useful someday:
(defun mingus-get-filenames-from-browser-for-shell (beg end)
  "Get everything under the region, sloppily.
Region is between (beginning of line of) BEG and (beginning of line of) END."
  (interactive "r")
  (let ((beg (if mark-active (_mingus-bol-at beg) (point-at-bol)))
        (end (if mark-active (_mingus-bol-at end) (point-at-eol))))
    (mapcar '_mingus-make-absolute-filename
            (split-string (buffer-substring-no-properties beg end) "\n" t))))

;;;; {{Dired}}
;; get all symlinks under mingus-mpd-root:
(defun mingus-retrieve-symlinks ()
  "Retrieve all symlinks under mpd root, and make an alist out of them.
To be used for passing the right data to mpd in dired."
  (cons
   (cons mingus-mpd-root "")
   (mingus-make-alist-reversed
    (remove
     ""
     (split-string
      (let  ((answer-from-shell
	      (shell-command-to-string
	       (format "symlinks -crv %s" mingus-mpd-root))))
	(if (string-match "command not found" answer-from-shell)
	    (error answer-from-shell)
	  answer-from-shell))
      (format
       "\\(\\(\n*relative:\\|\n*other_fs:\\|\n*dangling:\\| ->\\) \\|%s\\|\n\\)"
       mingus-mpd-root))))))

;; TODO: TEST without mpd-safe-string
;; DONE: DID NOT work...
(defun mingus-abs->rel (string)
  "In STRING, replace absolute path with symlink under `mingus-mpd-root';

If found, also quote it.  Used in `mingus-dired-add'."
  (let ((found-association
         (assoc* string (mingus-retrieve-symlinks) :test
                 '(lambda (place-of-file parent-dir)
                    ;; account for relative interpretation by the shell of
                    ;; symlinks
                    (string-match
                     (replace-regexp-in-string "\\.\\./" "" parent-dir)
                     place-of-file)))))
    (if found-association
        (mpd-safe-string
         (concat
          (cdr found-association)
          (substring string
                     ;; account for relative interpretation by the shell of
                     ;; symlinks
                     (apply (if (string-match "\\.\\./"
                                              (car found-association))
                                '1+
                              '+)
                            ;; account for relative interpretation by the shell
                            ;; of symlinks
                            (length (replace-regexp-in-string
                                     "\\.\\./"
                                     ""
                                     (car found-association)))
                            nil)))
)
)))

(defun mingus-dired-add ()
  "In `dired', add marked files or file at point to the mpd playlist; ; ;

If mpd is unwary of these files, ask whether to make a symlink.
Create a symlink, and optionally update database.

If updating takes too long, it might be that this does not work in one go.
Try again at a later time if this happens."
  (interactive)
  (let
      ((playlist-length (mingus-playlist-length)))
    (mingus-add (mapconcat
                 'mingus-abs->rel
                 (dired-get-marked-files) "\nadd ") t)
    ;; make sure mingus-add has indeed added the stuff:
    (if (= playlist-length (mingus-playlist-length))
        ;; if not, ask some questions
        (when (y-or-n-p
               (format "Something has gone wrong.  Possibly mpd does not know about the file.
Do you want to symlink the parent directory? " ))
          (shell-command-to-string
           (format "ln -s %s %s"
                   (shell-quote-argument (dired-current-directory))
                   (shell-quote-argument mingus-mpd-root)))

          (when (y-or-n-p "Update the database? ")
            (mingus-update)
            (message "This might take a while, repeat `mingus-dired-add' when that rattling sound of your harddisk has subsided")))
      t))) ;we have to return a non-`nil' value for the success message to bbbe displayed.

;; create function mingus-dired-add-and-play
(mingus-and-play mingus-dired-add mingus-dired-add-and-play)

;; make sure mingus-dired-add handles point-of-insertion and mingus-marked-list
(mingus-insertion-advice mingus-dired-add)

;; alright, a customization is in place for interfering with dired's defaults:
(defvar mingus-dired-space-function nil)

(defun mingus-dired-remove-keys ()
  (when (symbol-value 'mingus-dired-space-function)
    (define-key dired-mode-map " " mingus-dired-space-function))
  (define-key dired-mode-map [menu-bar operate mingus] nil))

(defun mingus-dired-add-keys ()

  (when
      (or (not (boundp 'mingus-dired-add-keys))
          (null (symbol-value 'mingus-dired-add-keys)))
    (setq mingus-dired-space-function (lookup-key dired-mode-map " ")))

  (define-key dired-mode-map " " 'mingus-dired-add)

  (define-key-after dired-mode-map [menu-bar operate mingus]
    '("Add to Mingus"  . mingus-dired-add) 'query-replace))

(defcustom mingus-dired-add-keys nil
  "Add keys for interaction to dired-mode-map;

The file \"mingus-stays-home.elc\" needs to be reloaded for a
change here to have effect. If `mingus-dired-add-keys' has a
non-`nil' value, \"SPC\" will add a song, play it immediately
when invoked with a prefix. Plus an item under Operate to add
songs to Mingus."
  :group 'mingus
  :type '(boolean)
  :set (lambda (sym val)
         (if val
             (mingus-dired-add-keys)
           (mingus-dired-remove-keys))
         (set-default sym val)))

(defun mingus-dired-file ()
  "Open dired with parent dir of song at point."
  (interactive)
  (dired
   (cond
    ((mingus-directoryp)
     (_mingus-make-absolute-filename (cdr (mingus-directoryp))))
    ((mingus-playlistp) mingus-mpd-playlist-dir)
    (t (_mingus-get-parent-dir))) "-al"))

(defun mingus-dwim-add ()
  "In `dired', add marked files or file at point to the mpd playlist.
In other modes, use the minibuffer.


If mpd is unwary of these files, ask whether to make a symlink.
Create a symlink, and optionally update database.

If updating takes too long, it might be that this does not work in one go.
Try again at a later time if this happens."
  (interactive)
  (let ((old-length (mingus-playlist-length)))
    (case major-mode
      ('dired-mode
       (mingus-add (mapconcat 'mingus-abs->rel (dired-get-marked-files) " ")))
      (t
       (mingus-add (mingus-abs->rel
                    (expand-file-name (read-file-name "Add: "))))))
    ;; make sure mingus-add has indeed added the stuff:
    (if (= old-length (mingus-playlist-length))
        ;; if not, ask some questions
        (when (yes-or-no-p (format "Something has gone wrong. Possibly mpd does not know about the file.
Do you want to symlink the parent directory? : " ))
          (shell-command-to-string
           (format "ln -s %s %s" (shell-quote-argument (dired-current-directory))
                   (shell-quote-argument mingus-mpd-root)))

          (when (yes-or-no-p "Update the database ? : ")
            (mingus-update)
            (message "This might take a while, repeat `mingus-dired-add' when that rattling sound of your harddisk has subsided")))
      (case major-mode
        ('mingus-burn-mode (with-no-warnings 
                             ;; `mingus-burns' will be defined in
                             ;; mingus-stays-home
                             (mingus-burns)))))))

(mingus-and-play mingus-dwim-add mingus-dwim-add-and-play)

;; (@> "development stuff")
' (mapconcat (lambda (list)
               (mingus-make-song-string list mingus-playlist-format-to-use
                                        mingus-playlist-separator))
             (mingus-get-songs "playlistinfo") "\n")

' (push mingus-modeline-timer timer-list)
' (push mingus-generic-timer timer-list)

(provide 'mingus)
;;; mingus.el ends here
