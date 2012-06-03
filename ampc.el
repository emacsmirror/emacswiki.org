;;; ampc.el --- Asynchronous Music Player Controller

;; Copyright (C) 2011-2012 Free Software Foundation, Inc.

;; Author: Christopher Schmidt <christopher@ch.ristopher.com>
;; Maintainer: Christopher Schmidt <christopher@ch.ristopher.com>
;; Version: 0.1.3
;; Created: 2011-12-06
;; Keywords: ampc, mpc, mpd
;; Compatibility: GNU Emacs: 24.x

;; This file is part of ampc.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;; * description
;; ampc is a controller for the Music Player Daemon (http://mpd.wikia.com/).

;;; ** installation
;; If you use GNU ELPA, install ampc via M-x package-list-packages RET or
;; (package-install 'ampc).  Otherwise, grab this file and put it somewhere in
;; your load-path or add the directory the file is in to it, e.g.:
;;
;; (add-to-list 'load-path "~/.emacs.d/ampc")
;;
;; Then add one autoload definition:
;;
;; (autoload 'ampc "ampc" nil t)
;;
;; Optionally bind a key to this function, e.g.:
;;
;; (global-set-key (kbd "<f9>") 'ampc)
;;
;; or
;;
;; (global-set-key (kbd "<f9>") (lambda () (interactive) (ampc "host" "port")))
;;
;; Byte-compile ampc (M-x byte-compile-file RET /path/to/ampc.el RET) to improve
;; its performance!

;;; ** usage
;; To invoke ampc, call the command `ampc', e.g. via M-x ampc RET.  When called
;; interactively, `ampc' reads host address and port from the minibuffer.  If
;; called non-interactively, the first argument to `ampc' is the host, the
;; second is the port.  Both values default to nil, which will make ampc connect
;; to localhost:6600.  Once ampc is connected to the daemon, it creates its
;; window configuration in the selected window.  To make ampc use the full frame
;; rather than the selected window, customise `ampc-use-full-frame'.
;;
;; ampc offers three independent views which expose different parts of the user
;; interface.  The current playlist view, the default view at startup, may be
;; accessed using the `J' (that is `S-j') key.  The playlist view may be
;; accessed using the `K' key.  The outputs view may be accessed using the `L'
;; key.

;;; *** current playlist view
;; The playlist view should look like this
;;
;; .........................
;; . 1      . 3  . 4  . 5  .
;; ..........    .    .    .
;; . 2      .    .    .    .
;; .        .    .    .    .
;; .        .    .    .    .
;; .        ................
;; .        . 6            .
;; .        .              .
;; .........................
;;
;; Window one exposes basic information about the daemon, such as the current
;; state (stop/play/pause), the song currently playing, or the volume.
;;
;; All windows, except the status window, contain a tabular list of items.  Each
;; item may be selected/marked.  There may be multiple selections.
;;
;; To mark an entry, move the point to the entry and press `m' (ampc-mark).  To
;; unmark an entry, press `u' (ampc-unmark).  To unmark all entries, press `U'
;; (ampc-unmark-all).  To toggle marks, press `t' (ampc-toggle-marks).  Pressing
;; `<down-mouse-1>' with the mouse mouse cursor on a list entry will move point
;; to the entry and toggle the mark.  To navigate to the next entry, press `n'
;; (ampc-next-line).  Analogous, pressing `p' (ampc-previous-line) moves the
;; point to the previous entry.
;;
;; Window two shows the current playlist.  The song that is currently played by
;; the daemon, if any, is highlighted.  To delete the selected songs from the
;; playlist, press `d' (ampc-delete).  Pressing `<down-mouse-3>' will move the
;; point to the entry under cursor and delete it from the playlist.  To move the
;; selected songs up, press `<up>' (ampc-up).  Analogous, press `<down>'
;; (ampc-down) to move the selected songs down.  Pressing `<return>'
;; (ampc-play-this) or `<down-mouse-2>' will play the song at point/cursor.
;;
;; Windows three to five are tag browsers.  You use them to narrow the song
;; database to certain songs.  Think of tag browsers as filters, analogous to
;; piping `grep' outputs through additional `grep' filters.  The property of the
;; songs that is filtered is displayed in the header line of the window.
;;
;; Window six shows the songs that match the filters defined by windows three to
;; five.  To add the selected song to the playlist, press `a' (ampc-add).
;; Pressing `<down-mouse-3>' will move the point to the entry under the cursor
;; and execute `ampc-add'.  These key bindings works in tag browsers as well.
;; Calling `ampc-add' in a tag browser adds all songs filtered up to the
;; selected browser to the playlist.
;;
;; The tag browsers of the (default) current playlist view (accessed via `J')
;; are `Genre' (window 3), `Artist' (window 4) and `Album' (window 5).  The key
;; `M' may be used to fire up a slightly modified current playlist view.  There
;; is no difference to the default current playlist view other than that the tag
;; browsers filter to `Genre' (window 3), `Album' (window 4) and `Artist'
;; (window 5).  Metaphorically speaking, the order of the `grep' filters defined
;; by the tag browsers is different.

;;; *** playlist view
;; The playlist view resembles the current playlist view.  The window, which
;; exposes the playlist content, is split, though.  The bottom half shows a list
;; of stored playlists.  The upper half does not expose the current playlist
;; anymore.  Instead, the content of the selected (stored) playlist is shown.
;; All commands that used to work in the current playlist view and modify the
;; current playlist now modify the selected (stored) playlist.  The list of
;; stored playlists is the only view in ampc that may have only one marked
;; entry.
;;
;; Again, the key `<' may be used to setup a playlist view with a different
;; order of tag browsers.

;;; *** outputs view
;; The outputs view contains a single list which shows the configured outputs of
;; mpd.  To toggle the enabled property of the selected outputs, press `a'
;; (ampc-toggle-output-enabled) or `<mouse-3>'.

;;; *** global keys
;; Aside from `J', `M', `K', `<' and `L', which may be used to select different
;; views, ampc defines the following global keys, which may be used in every
;; window associated with ampc:
;;
;; `k' (ampc-toggle-play): Toggle play state.  If mpd does not play a song
;; already, start playing the song at point if the current buffer is the
;; playlist buffer, otherwise start at the beginning of the playlist.  With
;; prefix argument 4, stop player rather than pause if applicable.
;;
;; `l' (ampc-next): Play next song.
;; `j' (ampc-previous): Play previous song
;;
;; `c' (ampc-clear): Clear playlist.
;; `s' (ampc-shuffle): Shuffle playlist.
;;
;; `S' (ampc-store): Store playlist.
;; `O' (ampc-load): Load selected playlist in the current playlist.
;; `R' (ampc-rename-playlist): Rename selected playlist.
;; `D' (ampc-delete-playlist): Delete selected playlist.
;;
;; `y' (ampc-increase-volume): Increase volume.
;; `M-y' (ampc-decrease-volume): Decrease volume.
;; `h' (ampc-increase-crossfade): Increase crossfade.
;; `M-h' (ampc-decrease-crossfade): Decrease crossfade.
;;
;; `e' (ampc-toggle-repeat): Toggle repeat state.
;; `r' (ampc-toggle-random): Toggle random state.
;; `f' (ampc-toggle-consume): Toggle consume state.
;;
;; `P' (ampc-goto-current-song): Select the current playlist window and move
;; point to the current song.
;;
;; `T' (ampc-trigger-update): Trigger a database update.
;; `Z' (ampc-suspend): Suspend ampc.
;; `q' (ampc-quit): Quit ampc.
;;
;; The keymap of ampc is designed to fit the QWERTY United States keyboard
;; layout.  If you use another keyboard layout, feel free to modify
;; `ampc-mode-map'.  For example, I use a regular QWERTZ German keyboard
;; (layout), so I modify `ampc-mode-map' in my init.el like this:
;;
;; (eval-after-load 'ampc
;;   '(flet ((substitute-ampc-key
;;            (from to)
;;            (define-key ampc-mode-map to (lookup-key ampc-mode-map from))
;;            (define-key ampc-mode-map from nil)))
;;      (substitute-ampc-key (kbd "z") (kbd "Z"))
;;      (substitute-ampc-key (kbd "y") (kbd "z"))
;;      (substitute-ampc-key (kbd "M-y") (kbd "M-z"))
;;      (substitute-ampc-key (kbd "<") (kbd ";"))))
;;
;; If ampc is suspended, you can still use every interactive command that does
;; not directly operate on or with the user interace of ampc.  For example it is
;; perfectly fine to call `ampc-increase-volume' or `ampc-toggle-play' via M-x
;; RET.  To display the information that is displayed by the status window of
;; ampc, call `ampc-status'.

;;; Code:
;;; * code
(eval-when-compile
  (require 'easymenu)
  (require 'cl))
(require 'network-stream)
(require 'avl-tree)

;;; ** declarations
;;; *** variables
(defgroup ampc ()
  "Asynchronous client for the Music Player Daemon."
  :prefix "ampc-"
  :group 'multimedia
  :group 'applications)

;;; *** customs
(defcustom ampc-debug nil
  "Non-nil means log communication between ampc and MPD."
  :type 'boolean)

(defcustom ampc-use-full-frame nil
  "If non-nil, ampc will use the entire Emacs screen."
  :type 'boolean)

(defcustom ampc-truncate-lines t
  "If non-nil, truncate lines in ampc buffers."
  :type 'boolean)

(defcustom ampc-status-tags nil
  "List of additional tags of the current song that are added to
the internal status of ampc and thus are passed to the functions
in `ampc-status-changed-hook'.  Each element may be a string that
specifies a tag that is returned by MPD's `currentsong'
command.")

;;; **** hooks
(defcustom ampc-before-startup-hook nil
  "A hook run before startup.
This hook is called as the first thing when ampc is started."
  :type 'hook)

(defcustom ampc-connected-hook nil
  "A hook run after ampc connected to MPD."
  :type 'hook)

(defcustom ampc-suspend-hook nil
  "A hook run when suspending ampc."
  :type 'hook)

(defcustom ampc-quit-hook nil
  "A hook run when exiting ampc."
  :type 'hook)

(defcustom ampc-status-changed-hook nil
  "A hook run whenever the status of the daemon (that is volatile
properties such as volume or current song) changes.  The hook is
run with one arg, an alist that contains the new status.  The car
of each entry is a symbol, the cdr is a string.  Valid keys are:

    volume
    repeat
    random
    consume
    xfade
    state
    song
    Artist
    Title

and the keys in `ampc-status-tags'.  Not all keys may be present
all the time!"
  :type 'hook)

;;; *** faces
(defface ampc-mark-face '((t (:inherit font-lock-constant-face)))
  "Face of the mark.")
(defface ampc-marked-face '((t (:inherit warning)))
  "Face of marked entries.")
(defface ampc-face '((t (:inerhit default)))
  "Face of unmarked entries.")
(defface ampc-current-song-mark-face '((t (:inherit region)))
  "Face of mark of the current song.")
(defface ampc-current-song-marked-face '((t (:inherit region)))
  "Face of the current song if marked.")

;;; *** internal variables
(defvar ampc-views
  (let* ((songs '(1.0 song :properties (("Track" :title "#")
                                        ("Title" :offset 6)
                                        ("Time" :offset 26))))
         (rs_a `(1.0 vertical
                     (0.7 horizontal
                          (0.33 tag :tag "Genre" :id 1)
                          (0.33 tag :tag "Artist" :id 2)
                          (1.0 tag :tag "Album" :id 3))
                     ,songs))
         (rs_b `(1.0 vertical
                     (0.7 horizontal
                          (0.33 tag :tag "Genre" :id 1)
                          (0.33 tag :tag "Album" :id 2)
                          (1.0 tag :tag "Artist" :id 3))
                     ,songs))
         (pl-prop '(("Title")
                    ("Artist" :offset 20)
                    ("Album" :offset 40)
                    ("Time" :offset 60))))
    `(("Current playlist view (Genre|Artist|Album)"
       ,(kbd "J")
       horizontal
       (0.4 vertical
            (6 status)
            (1.0 current-playlist :properties ,pl-prop))
       ,rs_a)
      ("Current playlist view (Genre|Album|Artist)"
       ,(kbd "M")
       horizontal
       (0.4 vertical
            (6 status)
            (1.0 current-playlist :properties ,pl-prop))
       ,rs_b)
      ("Playlist view (Genre|Artist|Album)"
       ,(kbd "K")
       horizontal
       (0.4 vertical
            (6 status)
            (1.0 vertical
                 (0.8 playlist :properties ,pl-prop)
                 (1.0 playlists)))
       ,rs_a)
      ("Playlist view (Genre|Album|Artist)"
       ,(kbd "<")
       horizontal
       (0.4 vertical
            (6 status)
            (1.0 vertical
                 (0.8 playlist :properties ,pl-prop)
                 (1.0 playlists)))
       ,rs_b)
      ("Outputs view"
       ,(kbd "L")
       outputs :properties (("outputname" :title "Name")
                            ("outputenabled" :title "Enabled" :offset 10))))))

(defvar ampc-connection nil)
(defvar ampc-host nil)
(defvar ampc-port nil)
(defvar ampc-outstanding-commands nil)

(defvar ampc-working-timer nil)
(defvar ampc-yield nil)

(defvar ampc-buffers nil)
(defvar ampc-buffers-unordered nil)
(defvar ampc-all-buffers nil)

(defvar ampc-type nil)
(make-variable-buffer-local 'ampc-type)
(defvar ampc-dirty nil)
(make-variable-buffer-local 'ampc-dirty)

(defvar ampc-internal-db nil)
(defvar ampc-status nil)

;;; *** mode maps
(defvar ampc-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map (kbd "k") 'ampc-toggle-play)
    (define-key map (kbd "l") 'ampc-next)
    (define-key map (kbd "j") 'ampc-previous)
    (define-key map (kbd "c") 'ampc-clear)
    (define-key map (kbd "s") 'ampc-shuffle)
    (define-key map (kbd "S") 'ampc-store)
    (define-key map (kbd "O") 'ampc-load)
    (define-key map (kbd "R") 'ampc-rename-playlist)
    (define-key map (kbd "D") 'ampc-delete-playlist)
    (define-key map (kbd "y") 'ampc-increase-volume)
    (define-key map (kbd "M-y") 'ampc-decrease-volume)
    (define-key map (kbd "h") 'ampc-increase-crossfade)
    (define-key map (kbd "M-h") 'ampc-decrease-crossfade)
    (define-key map (kbd "e") 'ampc-toggle-repeat)
    (define-key map (kbd "r") 'ampc-toggle-random)
    (define-key map (kbd "f") 'ampc-toggle-consume)
    (define-key map (kbd "P") 'ampc-goto-current-song)
    (define-key map (kbd "q") 'ampc-quit)
    (define-key map (kbd "z") 'ampc-suspend)
    (define-key map (kbd "T") 'ampc-trigger-update)
    (loop for view in ampc-views
          do (define-key map (cadr view)
               `(lambda ()
                  (interactive)
                  (ampc-change-view ',view))))
    map))

(defvar ampc-item-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map (kbd "m") 'ampc-mark)
    (define-key map (kbd "u") 'ampc-unmark)
    (define-key map (kbd "U") 'ampc-unmark-all)
    (define-key map (kbd "n") 'ampc-next-line)
    (define-key map (kbd "p") 'ampc-previous-line)
    (define-key map [remap next-line] 'ampc-next-line)
    (define-key map [remap previous-line] 'ampc-previous-line)
    (define-key map (kbd "<down-mouse-1>") 'ampc-mouse-toggle-mark)
    (define-key map (kbd "<mouse-1>") 'ampc-mouse-align-point)
    map))

(defvar ampc-current-playlist-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map (kbd "<return>") 'ampc-play-this)
    (define-key map (kbd "<down-mouse-2>") 'ampc-mouse-play-this)
    (define-key map (kbd "<mouse-2>") 'ampc-mouse-align-point)
    (define-key map (kbd "<down-mouse-3>") 'ampc-mouse-delete)
    map))

(defvar ampc-playlist-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map (kbd "t") 'ampc-toggle-marks)
    (define-key map (kbd "d") 'ampc-delete)
    (define-key map (kbd "<up>") 'ampc-up)
    (define-key map (kbd "<down>") 'ampc-down)
    (define-key map (kbd "<down-mouse-3>") 'ampc-mouse-delete)
    map))

(defvar ampc-playlists-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map (kbd "l") 'ampc-load)
    (define-key map (kbd "r") 'ampc-rename-playlist)
    (define-key map (kbd "d") 'ampc-delete-playlist)
    map))

(defvar ampc-tag-song-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map (kbd "t") 'ampc-toggle-marks)
    (define-key map (kbd "a") 'ampc-add)
    (define-key map (kbd "<down-mouse-3>") 'ampc-mouse-add)
    (define-key map (kbd "<mouse-3>") 'ampc-mouse-align-point)
    map))

(defvar ampc-outputs-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map (kbd "t") 'ampc-toggle-marks)
    (define-key map (kbd "a") 'ampc-toggle-output-enabled)
    (define-key map (kbd "<down-mouse-3>") 'ampc-mouse-toggle-output-enabled)
    (define-key map (kbd "<mouse-3>") 'ampc-mouse-align-point)
    map))

;;; **** menu
(easy-menu-define nil ampc-mode-map nil
  `("ampc"
    ("Change view" ,@(loop for view in ampc-views
                           collect (vector (car view)
                                           `(lambda ()
                                              (interactive)
                                              (ampc-change-view ',view)))))
    "--"
    ["Play" ampc-toggle-play
     :visible (and ampc-status
                   (not (equal (cdr (assq 'state ampc-status)) "play")))]
    ["Pause" ampc-toggle-play
     :visible (and ampc-status
                   (equal (cdr (assq 'state ampc-status)) "play"))]
    ["Stop" (lambda () (interactive) (ampc-toggle-play 4))
     :visible (and ampc-status
                   (equal (cdr (assq 'state ampc-status)) "play"))]
    ["Next" ampc-next]
    ["Previous" ampc-previous]
    "--"
    ["Clear playlist" ampc-clear]
    ["Shuffle playlist" ampc-shuffle]
    ["Store playlist" ampc-store]
    ["Queue Playlist" ampc-load :visible (ampc-playlist)]
    ["Rename Playlist" ampc-rename-playlist :visible (ampc-playlist)]
    ["Delete Playlist" ampc-delete-playlist :visible (ampc-playlist)]
    "--"
    ["Increase volume" ampc-increase-volume]
    ["Decrease volume" ampc-decrease-volume]
    ["Increase crossfade" ampc-increase-crossfade]
    ["Decrease crossfade" ampc-decrease-crossfade]
    ["Toggle repeat" ampc-toggle-repeat
     :style toggle
     :selected (equal (cdr-safe (assq 'repeat ampc-status)) "1")]
    ["Toggle random" ampc-toggle-random
     :style toggle
     :selected (equal (cdr-safe (assq 'random ampc-status)) "1")]
    ["Toggle consume" ampc-toggle-consume
     :style toggle
     :selected (equal (cdr-safe (assq 'consume ampc-status)) "1")]
    "--"
    ["Trigger update" ampc-trigger-update]
    ["Suspend" ampc-suspend]
    ["Quit" ampc-quit]))

(easy-menu-define ampc-selection-menu ampc-item-mode-map
  "Selection menu for ampc"
  '("ampc Mark"
    ["Add to playlist" ampc-add
     :visible (not (eq (car ampc-type) 'outputs))]
    ["Toggle enabled" ampc-toggle-output-enabled
     :visible (eq (car ampc-type) 'outputs)]
    "--"
    ["Next line" ampc-next-line]
    ["Previous line" ampc-previous-line]
    ["Mark" ampc-mark]
    ["Unmark" ampc-unmark]
    ["Unmark all" ampc-unmark-all]
    ["Toggle marks" ampc-toggle-marks
     :visible (not (eq (car ampc-type) 'playlists))]))

(defvar ampc-tool-bar-map
  (let ((map (make-sparse-keymap)))
    (tool-bar-local-item
     "mpc/prev" 'ampc-previous 'previous map
     :help "Previous")
    (tool-bar-local-item
     "mpc/play" 'ampc-toggle-play 'play map
     :help "Play"
     :visible '(and ampc-status
                    (not (equal (cdr (assq 'state ampc-status)) "play"))))
    (tool-bar-local-item
     "mpc/pause" 'ampc-toggle-play 'pause map
     :help "Pause"
     :visible '(and ampc-status
                    (equal (cdr (assq 'state ampc-status)) "play")))
    (tool-bar-local-item
     "mpc/stop" (lambda () (interactive) (ampc-toggle-play 4)) 'stop map
     :help "Stop"
     :visible '(and ampc-status
                    (equal (cdr (assq 'state ampc-status)) "play")))
    (tool-bar-local-item
     "mpc/next" 'ampc-next 'next map
     :help "Next")
    map))

;;; ** code
;;; *** macros
(defmacro ampc-with-buffer (type &rest body)
  (declare (indent 1) (debug t))
  `(let* ((type- ,type)
          (b (loop for b in ampc-buffers
                   when (with-current-buffer b
                          (cond ((windowp type-)
                                 (eq (window-buffer type-)
                                     (current-buffer)))
                                ((symbolp type-)
                                 (eq (car ampc-type) type-))
                                (t
                                 (equal ampc-type type-))))
                   return b
                   end)))
     (when b
       (with-current-buffer b
         (let ((buffer-read-only))
           ,@(if (eq (car body) 'no-se)
                 (cdr body)
               `((save-excursion
                   (goto-char (point-min))
                   ,@body))))))))

(defmacro ampc-fill-skeleton (tag &rest body)
  (declare (indent 1) (debug t))
  `(let ((tag- ,tag)
         (data-buffer (current-buffer)))
     (ampc-with-buffer tag-
       no-se
       (let ((point (point)))
         (goto-char (point-min))
         (loop until (eobp)
               do (put-text-property (point) (1+ (point)) 'updated t)
               (forward-line))
         (goto-char (point-min))
         ,@body
         (goto-char (point-min))
         (loop until (eobp)
               when (get-text-property (point) 'updated)
               do (delete-region (point) (1+ (line-end-position)))
               else
               do (add-text-properties
                   (+ (point) 2)
                   (progn (forward-line nil)
                          (1- (point)))
                   '(mouse-face highlight))
               end)
         (goto-char point)
         (ampc-align-point))
       (ampc-set-dirty nil)
       (with-selected-window (if (windowp tag-) tag- (ampc-get-window tag-))
         (recenter)))))

(defmacro ampc-with-selection (arg &rest body)
  (declare (indent 1) (debug t))
  `(let ((arg- ,arg))
     (if (and (not arg-)
              (save-excursion
                (goto-char (point-min))
                (search-forward-regexp "^* " nil t)))
         (loop initially (goto-char (point-min))
               finally (ampc-align-point)
               while (search-forward-regexp "^* " nil t)
               for index from 0
               do (save-excursion
                    ,@body))
       (loop until (eobp)
             for index from 0 to (1- (if (numberp arg-)
                                         arg-
                                       (prefix-numeric-value arg-)))
             do (save-excursion
                  (goto-char (line-end-position))
                  ,@body)
             until (ampc-next-line)))))

;;; *** modes
(define-derived-mode ampc-outputs-mode ampc-item-mode "ampc-o"
  nil)

(define-derived-mode ampc-tag-song-mode ampc-item-mode "ampc-ts"
  nil)

(define-derived-mode ampc-current-playlist-mode ampc-playlist-mode "ampc-cpl"
  nil)

(define-derived-mode ampc-playlist-mode ampc-item-mode "ampc-pl"
  nil)

(define-derived-mode ampc-playlists-mode ampc-item-mode "ampc-pls"
  nil)

(define-derived-mode ampc-item-mode ampc-mode ""
  nil)

(define-derived-mode ampc-mode special-mode "ampc"
  nil
  (buffer-disable-undo)
  (set (make-local-variable 'tool-bar-map) ampc-tool-bar-map)
  (setf truncate-lines ampc-truncate-lines
        font-lock-defaults '((("^\\(\\*\\)\\(.*\\)$"
                               (1 'ampc-mark-face)
                               (2 'ampc-marked-face))
                              ("^ .*$" 0 'ampc-face))
                             t)))

(define-minor-mode ampc-highlight-current-song-mode ""
  nil
  nil
  nil
  (funcall (if ampc-highlight-current-song-mode
               'font-lock-add-keywords
             'font-lock-remove-keywords)
           nil
           '((ampc-find-current-song
              (1 'ampc-current-song-mark-face)
              (2 'ampc-current-song-marked-face)))))

;;; *** internal functions
(defun ampc-change-view (view)
  (if (equal ampc-outstanding-commands '((idle)))
      (ampc-configure-frame (cddr view))
    (message "ampc is busy, cannot change window layout")))

(defun ampc-quote (string)
  (concat "\"" (replace-regexp-in-string "\"" "\\\"" string) "\""))

(defun ampc-on-p ()
  (and ampc-connection
       (member (process-status ampc-connection) '(open run))))

(defun ampc-in-ampc-p ()
  (when (ampc-on-p)
    ampc-type))

(defun ampc-add-impl (&optional data)
  (cond ((null data)
         (loop for d in (get-text-property (line-end-position) 'data)
               do (ampc-add-impl d)))
        ((avl-tree-p data)
         (avl-tree-mapc (lambda (e) (ampc-add-impl (cdr e))) data))
        ((stringp data)
         (if (ampc-playlist)
             (ampc-send-command 'playlistadd
                                t
                                (ampc-quote (ampc-playlist))
                                data)
           (ampc-send-command 'add t (ampc-quote data))))
        (t
         (loop for d in (reverse data)
               do (ampc-add-impl (cdr (assoc "file" d)))))))

(defun* ampc-skip (N &aux (song (cdr-safe (assq 'song ampc-status))))
  (when song
    (ampc-send-command 'play nil (max 0 (+ (string-to-number song) N)))))

(defun* ampc-find-current-song
    (limit &aux (point (point)) (song (cdr-safe (assq 'song ampc-status))))
  (when (and song
             (<= (1- (line-number-at-pos (point)))
                 (setf song (string-to-number song)))
             (>= (1- (line-number-at-pos limit)) song))
    (goto-char (point-min))
    (forward-line song)
    (save-restriction
      (narrow-to-region (max point (point)) (min limit (line-end-position)))
      (search-forward-regexp "\\(?1:\\(\\`\\*\\)?\\)\\(?2:.*\\)$"))))

(defun ampc-set-volume (arg func)
  (when (or arg ampc-status)
    (ampc-send-command
     'setvol
     nil
     (or (and arg (prefix-numeric-value arg))
         (max (min (funcall func
                            (string-to-number
                             (cdr (assq 'volume ampc-status)))
                            5)
                   100)
              0)))))

(defun ampc-set-crossfade (arg func)
  (when (or arg ampc-status)
    (ampc-send-command
     'crossfade
     nil
     (or (and arg (prefix-numeric-value arg))
         (max (funcall func
                       (string-to-number (cdr (assq 'xfade ampc-status)))
                       5)
              0)))))

(defun* ampc-fix-pos (f &aux buffer-read-only)
  (save-excursion
    (move-beginning-of-line nil)
    (let* ((data (get-text-property (+ 2 (point)) 'data))
           (pos (assoc "Pos" data)))
      (setf (cdr pos) (funcall f (cdr pos)))
      (put-text-property (+ 2 (point))
                         (line-end-position)
                         'data
                         data))))

(defun* ampc-move-impl (up &aux (line (1- (line-number-at-pos))))
  (when (or (and up (eq line 0))
            (and (not up) (eq (1+ line) (line-number-at-pos (1- (point-max))))))
    (return-from ampc-move-impl t))
  (save-excursion
    (move-beginning-of-line nil)
    (if (ampc-playlist)
        (ampc-send-command 'playlistmove
                           nil
                           (ampc-quote (ampc-playlist))
                           line
                           (funcall (if up '1- '1+)
                                    line))
      (ampc-send-command 'move nil line (funcall (if up '1- '1+) line)))
    (unless up
      (forward-line))
    (unless (ampc-playlist)
      (save-excursion
        (forward-line -1)
        (ampc-fix-pos '1+))
      (ampc-fix-pos '1-))
    (let ((buffer-read-only))
      (transpose-lines 1)))
  (if up
      (ampc-align-point)
    (ampc-next-line))
  nil)

(defun* ampc-move (up N &aux (point (point)))
  (goto-char (if up (point-min) (point-max)))
  (if (and (not N)
           (funcall (if up 'search-forward-regexp 'search-backward-regexp)
                    "^* "
                    nil
                    t))
      (loop until (ampc-move-impl up)
            unless up
            do (search-backward-regexp "^* " nil t)
            end
            until (not (funcall (if up
                                    'search-forward-regexp
                                  'search-backward-regexp)
                                "^* "
                                nil
                                t))
            finally (unless up
                      (forward-char 2)))
    (goto-char point)
    (unless (eobp)
      (unless N
        (setf N 1))
      (unless up
        (unless (eq (1- N) 0)
          (setf N (- (- (forward-line (1- N)) (1- N))))))
      (loop repeat N
            until (ampc-move-impl up)))))

(defun ampc-toggle-state (state arg)
  (when (or arg ampc-status)
    (ampc-send-command
     state
     nil
     (cond ((null arg)
            (if (equal (cdr (assq state ampc-status)) "1")
                0
              1))
           ((> (prefix-numeric-value arg) 0) 1)
           (t 0)))))

(defun ampc-playlist ()
  (ampc-with-buffer 'playlists
    (if (search-forward-regexp "^* \\(.*\\)$" nil t)
        (match-string 1)
      (unless (eobp)
        (buffer-substring-no-properties
         (+ (line-beginning-position) 2)
         (line-end-position))))))

(defun* ampc-mark-impl (select N &aux result buffer-read-only)
  (when (eq (car ampc-type) 'playlists)
    (assert (or (not select) (null N) (eq N 1)))
    (ampc-with-buffer 'playlists
      (loop while (search-forward-regexp "^\\* " nil t)
            do (replace-match "  " nil nil))))
  (loop repeat (or N 1)
        until (eobp)
        do (move-beginning-of-line nil)
        (delete-char 1)
        (insert (if select "*" " "))
        (setf result (ampc-next-line nil)))
  (ampc-post-mark-change-update)
  result)

(defun ampc-post-mark-change-update ()
  (ecase (car ampc-type)
    ((current-playlist playlist outputs))
    (playlists
     (ampc-update-playlist))
    ((song tag)
     (loop for w in (ampc-windows)
           with found
           when found
           do (with-current-buffer (window-buffer w)
                (when (member (car ampc-type) '(song tag))
                  (ampc-set-dirty t)))
           end
           if (eq w (selected-window))
           do (setf found t)
           end)
     (ampc-fill-tag-song))))

(defun ampc-align-point ()
  (unless (eobp)
    (move-beginning-of-line nil)
    (forward-char 2)))

(defun ampc-pad (alist)
  (loop for (offset . data) in alist
        with first = t
        with current-offset = 0
        when (<= current-offset offset)
        when (and (not first) (eq (- offset current-offset) 0))
        do (incf offset)
        end
        and concat (make-string (- offset current-offset) ? )
        and do (setf current-offset offset)
        else
        concat " "
        and do (incf current-offset)
        end
        concat data
        do (setf current-offset (+ current-offset (length data))
                 first nil)))

(defun ampc-update-header ()
  (if (eq (car ampc-type) 'status)
      (setf header-line-format nil)
    (setf header-line-format
          (concat
           (make-string (floor (fringe-columns 'left t)) ? )
           (ecase (car ampc-type)
             (tag
              (concat "  " (plist-get (cdr ampc-type) :tag)))
             (playlists
              "  Playlists")
             (t
              (ampc-pad (loop for p in (plist-get (cdr ampc-type) :properties)
                              collect `(,(or (plist-get (cdr p) :offset) 2) .
                                        ,(or (plist-get (cdr p) :title)
                                             (car p)))))))
           (when ampc-dirty
             " [ Updating... ]")))))

(defun ampc-set-dirty (tag-or-dirty &optional dirty)
  (if (or (null tag-or-dirty) (eq tag-or-dirty t))
      (progn (setf ampc-dirty tag-or-dirty)
             (ampc-update-header))
    (loop for w in (ampc-windows)
          do (with-current-buffer (window-buffer w)
               (when (eq (car ampc-type) tag-or-dirty)
                 (ampc-set-dirty dirty))))))

(defun ampc-update ()
  (if ampc-status
      (loop for b in ampc-buffers
            do (with-current-buffer b
                 (when ampc-dirty
                   (ecase (car ampc-type)
                     (outputs
                      (ampc-send-command 'outputs))
                     (playlist
                      (ampc-update-playlist))
                     ((tag song)
                      (if (assoc (ampc-tags) ampc-internal-db)
                          (ampc-fill-tag-song)
                        (push `(,(ampc-tags) . nil) ampc-internal-db)
                        (ampc-send-command 'listallinfo)))
                     (status
                      (ampc-send-command 'status)
                      (ampc-send-command 'currentsong))
                     (playlists
                      (ampc-send-command 'listplaylists))
                     (current-playlist
                      (ampc-send-command 'playlistinfo))))))
    (ampc-send-command 'status)
    (ampc-send-command 'currentsong)))

(defun ampc-update-playlist ()
  (ampc-with-buffer 'playlists
    (if (search-forward-regexp "^\\* " nil t)
        (ampc-send-command 'listplaylistinfo
                           nil
                           (get-text-property (point) 'data))
      (ampc-with-buffer 'playlist
        (delete-region (point-min) (point-max))
        (ampc-set-dirty nil)))))

(defun ampc-send-command-impl (command)
  (when ampc-debug
    (message (concat "ampc: " command)))
  (process-send-string ampc-connection (concat command "\n")))

(defun ampc-send-command (command &optional unique &rest args)
  (if (equal command 'idle)
      (when ampc-working-timer
        (cancel-timer ampc-working-timer)
        (setf ampc-yield nil
              ampc-working-timer nil)
        (ampc-fill-status))
    (unless ampc-working-timer
      (setf ampc-yield 0
            ampc-working-timer (run-at-time nil 0.1 'ampc-yield))))
  (setf command `(,command ,@args))
  (when (equal (car-safe ampc-outstanding-commands) '(idle))
    (setf (car ampc-outstanding-commands) '(noidle))
    (ampc-send-command-impl "noidle"))
  (setf ampc-outstanding-commands
        (nconc (if unique
                   ampc-outstanding-commands
                 (remove command ampc-outstanding-commands))
               `(,command))))

(defun ampc-send-next-command ()
  (unless ampc-outstanding-commands
    (ampc-send-command 'idle))
  (ampc-send-command-impl (concat (symbol-name (caar ampc-outstanding-commands))
                                  (loop for a in
                                        (cdar ampc-outstanding-commands)
                                        concat " "
                                        concat (cond ((integerp a)
                                                      (number-to-string a))
                                                     (t a))))))

(defun ampc-tree< (a b)
  (string< (car a) (car b)))

(defun ampc-create-tree ()
  (avl-tree-create 'ampc-tree<))

(defun ampc-extract (tag &optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (if (listp tag)
        (ampc-extract (plist-get tag :tag))
      (save-excursion
        (goto-char (point-min))
        (when (search-forward-regexp
               (concat "^" (regexp-quote tag) ": \\(.*\\)$")
               nil
               t)
          (let ((result (match-string 1)))
            (when (equal tag "Time")
              (setf result (ampc-transform-time result)))
            result))))))

(defun ampc-insert (element data &optional cmp)
  (save-excursion
    (goto-char (point-min))
    (ecase
        (loop until (eobp)
              for tp = (get-text-property (+ (point) 2) 'data)
              finally return 'insert
              thereis
              (cond ((eq cmp t)
                     (let ((s (buffer-substring-no-properties
                               (+ (point) 2)
                               (line-end-position))))
                       (cond ((equal s element)
                              (unless (member data tp)
                                (put-text-property (+ (point) 2)
                                                   (1+ (line-end-position))
                                                   'data
                                                   `(,data . ,tp)))
                              'update)
                             ((string< element s)
                              'insert))))
                    (cmp
                     (let ((r (funcall cmp data tp)))
                       (if (memq r '(update insert))
                           r
                         (forward-line (1- r))
                         nil)))
                    ((equal tp data)
                     'update)
                    (t
                     (let ((s (buffer-substring-no-properties
                               (+ (point) 2)
                               (line-end-position))))
                       (unless (string< s element)
                         'insert))))
              do (forward-line))
      (insert
       (insert "  ")
       (let ((start (point)))
         (insert element "\n")
         (put-text-property start (point) 'data (if (eq cmp t)
                                                    `(,data)
                                                  data))))
      (update
       (remove-text-properties (point) (1+ (point)) '(updated))
       (equal (buffer-substring (point) (1+ (point))) "*")))))

(defun ampc-fill-tag (trees)
  (put-text-property (point-min) (point-max) 'data nil)
  (loop with new-trees
        finally return new-trees
        for tree in trees
        when tree
        do (avl-tree-mapc (lambda (e)
                            (when (ampc-insert (car e) (cdr e) t)
                              (push (cdr e) new-trees)))
                          tree)
        end))

(defun ampc-fill-song (trees)
  (loop
   for songs in trees
   do (loop for song in songs
            do (ampc-insert
                (ampc-pad
                 (loop for (p . v) in (plist-get (cdr ampc-type) :properties)
                       collect `(,(- (or (plist-get v :offset) 2) 2)
                                 . ,(or (cdr-safe (assoc p song)) ""))))
                `((,song))))))

(defun* ampc-narrow-entry (&optional (delimiter "file"))
  (narrow-to-region (move-beginning-of-line nil)
                    (or (progn (goto-char (line-end-position))
                               (when (search-forward-regexp
                                      (concat "^" (regexp-quote delimiter) ": ")
                                      nil
                                      t)
                                 (move-beginning-of-line nil)
                                 (1- (point))))
                        (point-max))))

(defun ampc-get-window (type)
  (loop for w in (ampc-windows)
        thereis (with-current-buffer (window-buffer w)
                  (when (eq (car ampc-type) type)
                    w))))

(defun* ampc-fill-playlist (&aux properties)
  (ampc-fill-skeleton 'playlist
    (setf properties (plist-get (cdr ampc-type) :properties))
    (with-current-buffer data-buffer
      (loop
       for i from 0
       while (search-forward-regexp "^file: " nil t)
       do (save-restriction
            (ampc-narrow-entry)
            (let ((file (ampc-extract "file"))
                  (text
                   (ampc-pad
                    (loop for (tag . tag-properties) in properties
                          collect `(,(- (or (plist-get tag-properties
                                                       :offset)
                                            2)
                                        2)
                                    . ,(or (ampc-extract tag)
                                           "[Not Specified]"))))))
              (ampc-with-buffer 'playlist
                (ampc-insert text
                             `(("file" . ,file)
                               (index . ,i))
                             (lambda (a b)
                               (let ((p1 (cdr (assoc 'index a)))
                                     (p2 (cdr (assoc 'index b))))
                                 (cond ((< p1 p2) 'update)
                                       ((eq p1 p2)
                                        (if (equal (cdr (assoc "file" a))
                                                   (cdr (assoc "file" b)))
                                            'update
                                          'insert))
                                       (t (- p1 p2)))))))))))))

(defun* ampc-fill-outputs (&aux properties)
  (ampc-fill-skeleton 'outputs
    (setf properties (plist-get (cdr ampc-type) :properties))
    (with-current-buffer data-buffer
      (loop
       while (search-forward-regexp "^outputid: " nil t)
       do (save-restriction
            (ampc-narrow-entry "outputid")
            (let ((outputid (ampc-extract "outputid"))
                  (outputenabled (ampc-extract "outputenabled"))
                  (text
                   (ampc-pad
                    (loop for (tag . tag-properties) in properties
                          collect `(,(- (or (plist-get tag-properties :offset)
                                            2)
                                        2)
                                    . ,(ampc-extract tag))))))
              (ampc-with-buffer 'outputs
                (ampc-insert text `(("outputid" . ,outputid)
                                    ("outputenabled" . ,outputenabled))))))))))

(defun* ampc-fill-current-playlist (&aux properties)
  (ampc-fill-skeleton 'current-playlist
    (setf properties (plist-get (cdr ampc-type) :properties))
    (with-current-buffer data-buffer
      (loop
       while (search-forward-regexp "^file: " nil t)
       do (save-restriction
            (ampc-narrow-entry)
            (let ((file (ampc-extract "file"))
                  (pos (ampc-extract "Pos"))
                  (text
                   (ampc-pad
                    (loop for (tag . tag-properties) in properties
                          collect `(,(- (or (plist-get tag-properties :offset)
                                            2)
                                        2)
                                    . ,(or (ampc-extract tag)
                                           "[Not Specified]"))))))
              (ampc-with-buffer 'current-playlist
                (ampc-insert text
                             `(("file" . ,file)
                               ("Pos" . ,(string-to-number pos)))
                             (lambda (a b)
                               (let ((p1 (cdr (assoc "Pos" a)))
                                     (p2 (cdr (assoc "Pos" b))))
                                 (cond ((< p1 p2) 'insert)
                                       ((eq p1 p2)
                                        (if (equal (cdr (assoc "file" a))
                                                   (cdr (assoc "file" b)))
                                            'update
                                          'insert))
                                       (t (- p1 p2)))))))))))))

(defun ampc-fill-playlists ()
  (ampc-fill-skeleton 'playlists
    (with-current-buffer data-buffer
      (loop while (search-forward-regexp "^playlist: \\(.*\\)$" nil t)
            for playlist = (match-string 1)
            do (ampc-with-buffer 'playlists
                 (ampc-insert playlist playlist))))))

(defun ampc-yield ()
  (setf ampc-yield (1+ ampc-yield))
  (ampc-fill-status))

(defun ampc-fill-status ()
  (ampc-with-buffer 'status
    (delete-region (point-min) (point-max))
    (funcall (or (plist-get (cadr ampc-type) :filler)
                 (lambda (_)
                   (insert (ampc-status) "\n")))
             ampc-status)
    (ampc-set-dirty nil)))

(defun ampc-fill-tag-song ()
  (loop
   with trees = `(,(cdr (assoc (ampc-tags) ampc-internal-db)))
   for w in (ampc-windows)
   do
   (ampc-with-buffer w
     (when (member (car ampc-type) '(tag song))
       (if ampc-dirty
           (ampc-fill-skeleton w
             (ecase (car ampc-type)
               (tag (setf trees (ampc-fill-tag trees)))
               (song (ampc-fill-song trees))))
         (setf trees nil)
         (loop while (search-forward-regexp "^* " nil t)
               do (setf trees (append (get-text-property (point) 'data)
                                      trees))))))))

(defun* ampc-transform-time (data &aux (time (string-to-number data)))
  (concat (number-to-string (/ time 60))
          ":"
          (when (< (% time 60) 10)
            "0")
          (number-to-string (% time 60))))

(defun ampc-handle-idle ()
  (loop until (eobp)
        for subsystem = (buffer-substring (point) (line-end-position))
        when (string-match "^changed: \\(.*\\)$" subsystem)
        do (case (intern (match-string 1 subsystem))
             (database
              (setf ampc-internal-db nil)
              (ampc-set-dirty 'tag t)
              (ampc-set-dirty 'song t))
             (output
              (ampc-set-dirty 'outputs t))
             ((player options mixer)
              (setf ampc-status nil)
              (ampc-set-dirty 'status t))
             (stored_playlist
              (ampc-set-dirty 'playlists t)
              (ampc-set-dirty 'playlist t))
             (playlist
              (ampc-set-dirty 'current-playlist t)
              (ampc-set-dirty 'status t)))
        end
        do (forward-line))
  (ampc-update))

(defun ampc-handle-setup (status)
  (unless (and (string-match "^ MPD \\(.+\\)\\.\\(.+\\)\\.\\(.+\\)$"
                             status)
               (let ((version-a (string-to-number (match-string 1 status)))
                     (version-b (string-to-number (match-string 2 status)))
                     ;; (version-c (string-to-number (match-string 2 status)))
                     )
                 (or (> version-a 0)
                     (>= version-b 15))))
    (error (concat "Your version of MPD is not supported.  "
                   "ampc supports MPD (protocol version) 0.15.0 "
                   "and later"))))

(defun ampc-fill-internal-db (running)
  (loop for origin = (and (search-forward-regexp "^file: " nil t)
                          (line-beginning-position))
        then next
        while origin
        do (goto-char (1+ origin))
        for next = (and (search-forward-regexp "^file: " nil t)
                        (line-beginning-position))
        while (or (not running) next)
        do (save-restriction
             (narrow-to-region origin (or next (point-max)))
             (ampc-fill-internal-db-entry))
        do (when running
             (delete-region origin next)
             (setf next origin))))

(defun ampc-tags ()
  (loop for w in (ampc-windows)
        for tag = (with-current-buffer (window-buffer w)
                    (when (eq (car ampc-type) 'tag)
                      (plist-get (cdr ampc-type) :tag)))
        when tag
        collect tag
        end))

(defun ampc-fill-internal-db-entry ()
  (loop
   with data-buffer = (current-buffer)
   with tree = (assoc (ampc-tags) ampc-internal-db)
   for w in (ampc-windows)
   do
   (with-current-buffer (window-buffer w)
     (ampc-set-dirty t)
     (ecase (car ampc-type)
       (tag
        (let ((data (or (ampc-extract (cdr ampc-type) data-buffer)
                        "[Not Specified]")))
          (unless (cdr tree)
            (setf (cdr tree) (ampc-create-tree)))
          (setf tree (avl-tree-enter (cdr tree)
                                     `(,data . nil)
                                     (lambda (data match)
                                       match)))))
       (song
        (push (loop for p in `(("file")
                               ,@(plist-get (cdr ampc-type) :properties))
                    for data = (ampc-extract (car p) data-buffer)
                    when data
                    collect `(,(car p) . ,data)
                    end)
              (cdr tree))
        (return))))))

(defun ampc-handle-current-song ()
  (loop for k in (append ampc-status-tags '("Artist" "Title"))
        for s = (ampc-extract k)
        when s
        do (push `(,(intern k) . ,s) ampc-status)
        end)
  (ampc-fill-status)
  (run-hook-with-args ampc-status-changed-hook ampc-status))

(defun ampc-handle-status ()
  (loop for k in '("volume" "repeat" "random" "consume" "xfade" "state" "song")
        for v = (ampc-extract k)
        when v
        do (push `(,(intern k) . ,v) ampc-status)
        end)
  (ampc-with-buffer 'current-playlist
    (when ampc-highlight-current-song-mode
      (font-lock-fontify-region (point-min) (point-max)))))

(defun ampc-handle-update ()
  (message "Database update started"))

(defun ampc-handle-command (status)
  (cond
   ((eq status 'error)
    (pop ampc-outstanding-commands))
   ((eq status 'running)
    (case (caar ampc-outstanding-commands)
      (listallinfo (ampc-fill-internal-db t))))
   (t
    (case (car (pop ampc-outstanding-commands))
      (idle
       (ampc-handle-idle))
      (setup
       (ampc-handle-setup status))
      (currentsong
       (ampc-handle-current-song))
      (status
       (ampc-handle-status))
      (update
       (ampc-handle-update))
      (listplaylistinfo
       (ampc-fill-playlist))
      (listplaylists
       (ampc-fill-playlists))
      (playlistinfo
       (ampc-fill-current-playlist))
      (listallinfo
       (ampc-fill-internal-db nil))
      (outputs
       (ampc-fill-outputs)))
    (unless ampc-outstanding-commands
      (ampc-update)))))

(defun ampc-filter (_process string)
  (assert (buffer-live-p (process-buffer ampc-connection)))
  (with-current-buffer (process-buffer ampc-connection)
    (when string
      (when ampc-debug
        (message "ampc: -> %s" string))
      (goto-char (process-mark ampc-connection))
      (insert string)
      (set-marker (process-mark ampc-connection) (point)))
    (save-excursion
      (goto-char (point-min))
      (let ((success))
        (if (or (and (search-forward-regexp
                      "^ACK \\[\\(.*\\)\\] {.*} \\(.*\\)\n\\'"
                      nil
                      t)
                     (message "ampc command error: %s (%s)"
                              (match-string 2)
                              (match-string 1))
                     t)
                (and (search-forward-regexp "^OK\\(.*\\)\n\\'" nil t)
                     (setf success t)))
            (progn
              (let ((match-end (match-end 0)))
                (save-restriction
                  (narrow-to-region (point-min) match-end)
                  (goto-char (point-min))
                  (ampc-handle-command (if success (match-string 1) 'error)))
                (delete-region (point-min) match-end))
              (ampc-send-next-command))
          (ampc-handle-command 'running))))))

;;; **** window management
(defun ampc-windows (&optional unordered)
  (loop for f being the frame
        thereis (loop for w being the windows of f
                      when (eq (window-buffer w) (car-safe ampc-buffers))
                      return (loop for b in (if unordered
                                                ampc-buffers-unordered
                                              ampc-buffers)
                                   collect
                                   (loop for w being the windows of f
                                         thereis (and (eq (window-buffer w)
                                                          b)
                                                      w))))))

(defun* ampc-configure-frame-1 (split &aux (split-type (car split)))
  (if (member split-type '(vertical horizontal))
      (let* ((sizes))
        (loop with length = (if (eq split-type 'horizontal)
                                (window-width)
                              (window-height))
              with rest = length
              with rest-car
              for subsplit in (cdr split)
              for s = (car subsplit)
              if (equal s 1.0)
              do (push t sizes)
              and do (setf rest-car sizes)
              else
              do (let ((l (if (integerp s) s (floor (* s length)))))
                   (setf rest (- rest l))
                   (push l sizes))
              finally do (setf (car rest-car) rest))
        (let ((first-window (selected-window)))
          (setf sizes (nreverse sizes))
          (loop for size in (loop for s in sizes
                                  collect s)
                for window on (cdr sizes)
                do (select-window
                    (setf (car window)
                          (split-window nil
                                        size
                                        (eq split-type 'horizontal)))))
          (setf (car sizes) first-window))
        (loop for subsplit in (cdr split)
              for window in sizes
              do (with-selected-window window
                   (ampc-configure-frame-1 (cdr subsplit)))
              if (plist-get (cddr subsplit) :point)
              do (select-window window)
              end))
    (setf (window-dedicated-p (selected-window)) nil)
    (ecase split-type
      ((tag song)
       (pop-to-buffer-same-window
        (get-buffer-create (concat "*ampc "
                                   (or (plist-get (cdr split) :tag) "Song")
                                   "*")))
       (ampc-tag-song-mode))
      (outputs
       (pop-to-buffer-same-window (get-buffer-create "*ampc Outputs*"))
       (ampc-outputs-mode))
      (current-playlist
       (pop-to-buffer-same-window (get-buffer-create "*ampc Current Playlist*"))
       (ampc-current-playlist-mode)
       (ampc-highlight-current-song-mode 1))
      (playlist
       (pop-to-buffer-same-window (get-buffer-create "*ampc Playlist*"))
       (ampc-playlist-mode))
      (playlists
       (pop-to-buffer-same-window (get-buffer-create "*ampc Playlists*"))
       (ampc-playlists-mode))
      (status
       (pop-to-buffer-same-window (get-buffer-create "*ampc Status*"))
       (ampc-mode)))
    (destructuring-bind (&key (dedicated t) (mode-line t) &allow-other-keys)
        (cdr split)
      (setf (window-dedicated-p (selected-window)) dedicated)
      (unless mode-line
        (setf mode-line-format nil)))
    (setf ampc-type split)
    (add-to-list 'ampc-all-buffers (current-buffer))
    (push `(,(or (plist-get (cdr split) :id)
                 (if (eq (car ampc-type) 'song) 9998 9999))
            . ,(current-buffer))
          ampc-buffers)
    (ampc-set-dirty t)))

(defun ampc-configure-frame (split)
  (if ampc-use-full-frame
      (progn (setf (window-dedicated-p (selected-window)) nil)
             (delete-other-windows))
    (loop with live-window = nil
          for w in (nreverse (ampc-windows t))
          if (window-live-p w)
          if (not live-window)
          do (setf live-window w)
          else
          do (delete-window w)
          end
          end
          finally do (if live-window (select-window live-window))))
  (setf ampc-buffers nil)
  (ampc-configure-frame-1 split)
  (setf ampc-buffers-unordered (mapcar 'cdr ampc-buffers)
        ampc-buffers (mapcar 'cdr (sort ampc-buffers
                                        (lambda (a b) (< (car a) (car b))))))
  (ampc-update))

(defun ampc-mouse-play-this (event)
  (interactive "e")
  (select-window (posn-window (event-end event)))
  (goto-char (posn-point (event-end event)))
  (ampc-play-this))

(defun ampc-mouse-delete (event)
  (interactive "e")
  (select-window (posn-window (event-end event)))
  (goto-char (posn-point (event-end event)))
  (ampc-delete 1))

(defun ampc-mouse-add (event)
  (interactive "e")
  (select-window (posn-window (event-end event)))
  (goto-char (posn-point (event-end event)))
  (ampc-add-impl))

(defun ampc-mouse-toggle-output-enabled (event)
  (interactive "e")
  (select-window (posn-window (event-end event)))
  (goto-char (posn-point (event-end event)))
  (ampc-toggle-output-enabled 1))

(defun* ampc-mouse-toggle-mark (event &aux buffer-read-only)
  (interactive "e")
  (let ((window (posn-window (event-end event))))
    (when (with-selected-window window
            (goto-char (posn-point (event-end event)))
            (unless (eobp)
              (move-beginning-of-line nil)
              (ampc-mark-impl (not (eq (char-after) ?*)) 1)
              t))
      (select-window window))))

(defun ampc-mouse-align-point (event)
  (interactive "e")
  (select-window (posn-window (event-end event)))
  (goto-char (posn-point (event-end event)))
  (ampc-align-point))

;;; *** interactives
(defun* ampc-unmark-all (&aux buffer-read-only)
  "Remove all marks."
  (interactive)
  (assert (ampc-in-ampc-p))
  (save-excursion
    (goto-char (point-min))
    (loop while (search-forward-regexp "^\\* " nil t)
          do (replace-match "  " nil nil)))
  (ampc-post-mark-change-update))

(defun ampc-trigger-update ()
  "Trigger a database update."
  (interactive)
  (assert (ampc-on-p))
  (ampc-send-command 'update))

(defun* ampc-toggle-marks (&aux buffer-read-only)
  "Toggle marks.  Marked entries become unmarked, and vice versa."
  (interactive)
  (assert (ampc-in-ampc-p))
  (save-excursion
    (loop for (a . b) in '(("* " . "T ")
                           ("  " . "* ")
                           ("T " . "  "))
          do (goto-char (point-min))
          (loop while (search-forward-regexp (concat "^" (regexp-quote a))
                                             nil
                                             t)
                do (replace-match b nil nil))))
  (ampc-post-mark-change-update))

(defun ampc-up (&optional arg)
  "Go to the previous ARG'th entry.
With optional prefix ARG, move the next ARG entries after point
rather than the selection."
  (interactive "P")
  (assert (ampc-in-ampc-p))
  (ampc-move t arg))

(defun ampc-down (&optional arg)
  "Go to the next ARG'th entry.
With optional prefix ARG, move the next ARG entries after point
rather than the selection."
  (interactive "P")
  (assert (ampc-in-ampc-p))
  (ampc-move nil arg))

(defun ampc-mark (&optional arg)
  "Mark the next ARG'th entries.
ARG defaults to 1."
  (interactive "p")
  (assert (ampc-in-ampc-p))
  (ampc-mark-impl t arg))

(defun ampc-unmark (&optional arg)
  "Unmark the next ARG'th entries.
ARG defaults to 1."
  (interactive "p")
  (assert (ampc-in-ampc-p))
  (ampc-mark-impl nil arg))

(defun ampc-increase-volume (&optional arg)
  "Decrease volume.
With prefix argument ARG, set volume to ARG percent."
  (interactive "P")
  (assert (ampc-on-p))
  (ampc-set-volume arg '+))

(defun ampc-decrease-volume (&optional arg)
  "Decrease volume.
With prefix argument ARG, set volume to ARG percent."
  (interactive "P")
  (assert (ampc-on-p))
  (ampc-set-volume arg '-))

(defun ampc-increase-crossfade (&optional arg)
  "Increase crossfade.
With prefix argument ARG, set crossfading to ARG seconds."
  (interactive "P")
  (assert (ampc-on-p))
  (ampc-set-crossfade arg '+))

(defun ampc-decrease-crossfade (&optional arg)
  "Decrease crossfade.
With prefix argument ARG, set crossfading to ARG seconds."
  (interactive "P")
  (assert (ampc-on-p))
  (ampc-set-crossfade arg '-))

(defun ampc-toggle-repeat (&optional arg)
  "Toggle MPD's repeat state.
With prefix argument ARG, enable repeating if ARG is positive,
otherwise disable it."
  (interactive "P")
  (assert (ampc-on-p))
  (ampc-toggle-state 'repeat arg))

(defun ampc-toggle-consume (&optional arg)
  "Toggle MPD's consume state.
With prefix argument ARG, enable consuming if ARG is positive,
otherwise disable it.

When consume is activated, each song played is removed from the playlist."
  (interactive "P")
  (assert (ampc-on-p))
  (ampc-toggle-state 'consume arg))

(defun ampc-toggle-random (&optional arg)
  "Toggle MPD's random state.
With prefix argument ARG, enable random playing if ARG is positive,
otherwise disable it."
  (interactive "P")
  (ampc-toggle-state 'random arg))

(defun ampc-play-this ()
  "Play selected song."
  (interactive)
  (assert (ampc-in-ampc-p))
  (unless (eobp)
    (ampc-send-command 'play nil (1- (line-number-at-pos)))
    (ampc-send-command 'pause nil 0)))

(defun* ampc-toggle-play
    (&optional arg &aux (state (cdr-safe (assq 'state ampc-status))))
  "Toggle play state.
If mpd does not play a song already, start playing the song at
point if the current buffer is the playlist buffer, otherwise
start at the beginning of the playlist.

If ARG is 4, stop player rather than pause if applicable."
  (interactive "P")
  (assert (ampc-on-p))
  (when state
    (when arg
      (setf arg (prefix-numeric-value arg)))
    (ecase (intern state)
      (stop
       (when (or (null arg) (> arg 0))
         (ampc-send-command
          'play
          nil
          (if (and (eq (car ampc-type) 'current-playlist) (not (eobp)))
              (1- (line-number-at-pos))
            0))))
      (pause
       (when (or (null arg) (> arg 0))
         (ampc-send-command 'pause nil 0)))
      (play
       (cond ((or (null arg) (< arg 0))
              (ampc-send-command 'pause nil 1))
             ((eq arg 4)
              (ampc-send-command 'stop)))))))

(defun ampc-next (&optional arg)
  "Play next song.
With prefix argument ARG, skip ARG songs."
  (interactive "p")
  (assert (ampc-on-p))
  (ampc-skip (or arg 1)))

(defun ampc-previous (&optional arg)
  "Play previous song.
With prefix argument ARG, skip ARG songs."
  (interactive "p")
  (assert (ampc-on-p))
  (ampc-skip (- (or arg 1))))

(defun ampc-rename-playlist (new-name)
  "Rename selected playlist to NEW-NAME.
Interactively, read NEW-NAME from the minibuffer."
  (interactive "MNew name: ")
  (assert (ampc-in-ampc-p))
  (if (ampc-playlist)
      (ampc-send-command 'rename nil (ampc-playlist) new-name)
    (error "No playlist selected")))

(defun ampc-load ()
  "Load selected playlist in the current playlist."
  (interactive)
  (assert (ampc-in-ampc-p))
  (if (ampc-playlist)
      (ampc-send-command 'load nil (ampc-quote (ampc-playlist)))
    (error "No playlist selected")))

(defun ampc-toggle-output-enabled (&optional arg)
  "Toggle the next ARG outputs.
If ARG is omitted, use the selected entries."
  (interactive "P")
  (assert (ampc-in-ampc-p))
  (ampc-with-selection arg
    (let ((data (get-text-property (point) 'data)))
      (ampc-send-command (if (equal (cdr (assoc "outputenabled" data)) "1")
                             'disableoutput
                           'enableoutput)
                         nil
                         (cdr (assoc "outputid" data))))))

(defun ampc-delete (&optional arg)
  "Delete the next ARG songs from the playlist.
If ARG is omitted, use the selected entries.  If ARG is non-nil,
all marks after point are removed nontheless."
  (interactive "P")
  (assert (ampc-in-ampc-p))
  (let ((point (point)))
    (ampc-with-selection arg
      (let ((val (1- (- (line-number-at-pos) index))))
        (if (ampc-playlist)
            (ampc-send-command 'playlistdelete
                               t
                               (ampc-quote (ampc-playlist))
                               val)
          (ampc-send-command 'delete t val))))
    (goto-char point)
    (ampc-align-point)))

(defun ampc-shuffle ()
  "Shuffle playlist."
  (interactive)
  (assert (ampc-on-p))
  (if (not (ampc-playlist))
      (ampc-send-command 'shuffle)
    (ampc-with-buffer 'playlist
      (let ((shuffled
             (mapcar
              'car
              (sort (loop until (eobp)
                          collect `(,(cdr (assoc "file" (get-text-property
                                                         (+ 2 (point))
                                                         'data)))
                                    . ,(random))
                          do (forward-line))
                    (lambda (a b)
                      (< (cdr a) (cdr b)))))))
        (ampc-clear)
        (loop for s in shuffled
              do (ampc-add-impl s))))))

(defun ampc-clear ()
  "Clear playlist."
  (interactive)
  (assert (ampc-on-p))
  (if (ampc-playlist)
      (ampc-send-command 'playlistclear nil (ampc-quote (ampc-playlist)))
    (ampc-send-command 'clear)))

(defun ampc-add (&optional arg)
  "Add the songs associated with the next ARG entries after point
to the playlist.
If ARG is omitted, use the selected entries in the current buffer."
  (interactive "P")
  (assert (ampc-in-ampc-p))
  (ampc-with-selection arg
    (ampc-add-impl)))

(defun ampc-status ()
  "Display the information that is displayed in the status window."
  (interactive)
  (assert (ampc-on-p))
  (let* ((flags (mapconcat
                 'identity
                 (loop for (f . n) in '((repeat . "Repeat")
                                        (random . "Random")
                                        (consume . "Consume"))
                       when (equal (cdr (assq f ampc-status)) "1")
                       collect n
                       end)
                 "|"))
         (state (cdr (assq 'state ampc-status)))
         (status (concat "State:     " state
                         (when ampc-yield
                           (concat (make-string (- 10 (length state)) ? )
                                   (nth (% ampc-yield 4) '("|" "/" "-" "\\"))))
                         "\n"
                         (when (equal state "play")
                           (concat "Playing:   "
                                   (or (cdr-safe (assq 'Artist ampc-status))
                                       "[Not Specified]")
                                   " - "
                                   (or (cdr-safe (assq 'Title ampc-status))
                                       "[Not Specified]")
                                   "\n"))
                         "Volume:    " (cdr (assq 'volume ampc-status)) "\n"
                         "Crossfade: " (cdr (assq 'xfade ampc-status))
                         (unless (equal flags "")
                           (concat "\n" flags)))))
    (when (called-interactively-p 'interactive)
      (message "%s" status))
    status))

(defun ampc-delete-playlist ()
  "Delete selected playlist."
  (interactive)
  (assert (ampc-in-ampc-p))
  (ampc-with-selection nil
    (let ((name (get-text-property (point) 'data)))
      (when (y-or-n-p (concat "Delete playlist " name "?"))
        (ampc-send-command 'rm nil (ampc-quote name))))))

(defun ampc-store (name)
  "Store current playlist as NAME.
Interactively, read NAME from the minibuffer."
  (interactive "MSave playlist as: ")
  (assert (ampc-in-ampc-p))
  (ampc-send-command 'save nil (ampc-quote name)))

(defun* ampc-goto-current-song
    (&aux (song (cdr-safe (assq 'song ampc-status))))
  "Select the current playlist window and move point to the current song."
  (interactive)
  (assert (ampc-in-ampc-p))
  (when song
    (ampc-with-buffer 'current-playlist
      no-se
      (select-window (ampc-get-window 'current-playlist))
      (goto-char (point-min))
      (forward-line (string-to-number song))
      (ampc-align-point))))

(defun ampc-previous-line (&optional arg)
  "Go to previous ARG'th entry in the current buffer.
ARG defaults to 1."
  (interactive "p")
  (assert (ampc-in-ampc-p))
  (ampc-next-line (* (or arg 1) -1)))

(defun ampc-next-line (&optional arg)
  "Go to next ARG'th entry in the current buffer.
ARG defaults to 1."
  (interactive "p")
  (assert (ampc-in-ampc-p))
  (forward-line arg)
  (if (eobp)
      (progn (forward-line -1)
             (forward-char 2)
             t)
    (ampc-align-point)
    nil))

(defun* ampc-suspend (&optional (run-hook t))
  "Suspend ampc.
This function resets the window configuration, but does not close
the connection to mpd or destroy the internal cache of ampc.
This means subsequent startups of ampc will be faster."
  (interactive)
  (when ampc-working-timer
    (cancel-timer ampc-working-timer))
  (loop with found-window
        for w in (nreverse (ampc-windows t))
        when (window-live-p w)
        when found-window
        do (delete-window w)
        else
        do (setf found-window t
                 (window-dedicated-p w) nil)
        end
        end)
  (loop for b in ampc-all-buffers
        when (buffer-live-p b)
        do (kill-buffer b)
        end)
  (setf ampc-buffers nil
        ampc-all-buffers nil
        ampc-working-timer nil)
  (when run-hook
    (run-hooks 'ampc-suspend-hook)))

(defun ampc-quit (&optional arg)
  "Quit ampc.
If called with a prefix argument ARG, kill the mpd instance that
ampc is connected to."
  (interactive "P")
  (when (ampc-on-p)
    (set-process-filter ampc-connection nil)
    (when (equal (car-safe ampc-outstanding-commands) '(idle))
      (ampc-send-command-impl "noidle")
      (with-current-buffer (process-buffer ampc-connection)
        (loop do (goto-char (point-min))
              until (search-forward-regexp "^\\(ACK\\)\\|\\(OK\\).*\n\\'" nil t)
              do (accept-process-output ampc-connection nil 50))))
    (ampc-send-command-impl (if arg "kill" "close")))
  (when ampc-working-timer
    (cancel-timer ampc-working-timer))
  (ampc-suspend nil)
  (setf ampc-connection nil
        ampc-internal-db nil
        ampc-outstanding-commands nil
        ampc-status nil)
  (run-hooks 'ampc-quit-hook))

;;;###autoload
(defun ampc (&optional host port)
  "ampc is an asynchronous client for the MPD media player.
This function is the main entry point for ampc.

Non-interactively, HOST and PORT specify the MPD instance to
connect to.  The values default to localhost:6600."
  (interactive "MHost (localhost): \nMPort (6600): ")
  (run-hooks 'ampc-before-startup-hook)
  (when (or (not host) (equal host ""))
    (setf host "localhost"))
  (when (or (not port) (equal port ""))
    (setf port 6600))
  (when (and ampc-connection
             (or (not (equal host ampc-host))
                 (not (equal port ampc-port))
                 (not (ampc-on-p))))
    (ampc-quit))
  (unless ampc-connection
    (let ((connection (open-network-stream "ampc"
                                           (with-current-buffer
                                               (get-buffer-create " *ampc*")
                                             (delete-region (point-min)
                                                            (point-max))
                                             (current-buffer))
                                           host
                                           port
                                           :type 'plain :return-list t)))
      (unless (car connection)
        (error "Failed connecting to server: %s"
               (plist-get ampc-connection :error)))
      (setf ampc-connection (car connection)
            ampc-host host
            ampc-port port))
    (set-process-coding-system ampc-connection 'utf-8-unix 'utf-8-unix)
    (set-process-filter ampc-connection 'ampc-filter)
    (set-process-query-on-exit-flag ampc-connection nil)
    (setf ampc-outstanding-commands '((setup))))
  (ampc-configure-frame (cddar ampc-views))
  (run-hooks 'ampc-connected-hook)
  (ampc-filter (process-buffer ampc-connection) nil))

(provide 'ampc)

;; Local Variables:
;; eval: (outline-minor-mode 1)
;; outline-regexp: ";;; \\*+"
;; lexical-binding: t
;; fill-column: 80
;; indent-tabs-mode: nil
;; End:
