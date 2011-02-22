;;; pianobar.el --- Run pianobar as an inferior process in emacs

;;; Copyright (C) 2010 Joseph Hanson

;;; Author: Joseph Hanson <jrh0090@gmail.com>
;;; Maintainer: Joseph Hanson <jrh0090@gmail.com>
;;; Created: 12/12/10
;;; Version: 0.0.1
;;; Package-Requires: (comint)
;;; Keywords: pianobar

;;; Piano bar is a console based pandora client. It works pretty well
;;; out of the box with Emacs except that the command intiators (e.g. "p"
;;; for pause) do not work without sending the string directly to the process
;;; with (comint-send-string )

;;; Right now pianobar expects a lot of the interaction to be from
;;; the pianobar inferior process.

;; INSTALLATION
;; ================

;; Installation instructions:
;;
;; 1. Put this file in your emacs load path OR add the following to
;;    your .emacs file (modifying the path appropriately):
;;
;;    (add-to-list 'load-path "/path/to/pianobar.el")
;;
;; 2. Add the following to your .emacs file to load this file
;;    automatically when M-x pianobar is run:
;;
;;    (autoload 'pianobar "pianobar" nil t)
;;


;; License
;; =========
;; pianobar.el is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; {at your option} any later version.

;; pianobar.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING, or type `C-h C-c'. If
;; not, write to the Free Software Foundation at this address:

;;   Free Software Foundation
;;   51 Franklin Street, Fifth Floor
;;   Boston, MA 02110-1301
;;   USA

;;; Commentary:
;;; pianobar.el lets you control your pandora radio station from the comfort of emacs
;;;
;;  Usage:

;;; History:
;;

;;; Todo:
;;; - implement "a"
;;; Code:

(require 'comint)

(defvar pianobar-key [f7]
  "The prefix key for pianobar access.
Call `pianobar-key-setup' when changed to have a correct keymap.")

(defvar pianobar-key-map nil
  "The key map for pianobar.")

(defvar pianobar-current-song nil
  "The song that is currently playing in pianobar")

(defcustom pianobar-program-command "pianobar"
  "the path to the executable 'pianobar', the default assumes it is on your path"
  :group 'pianobar)

(defgroup pianobar nil
  "Run a pianobar process in a buffer."
  :group 'pianobar)

(defcustom pianobar-mode-hook nil
  "*Hook for customizing inferior-pianobar mode."
  :type 'hook
  :group 'pianobar)

(defcustom pianobar-username nil
  "User name for pandora"
  :group 'pianobar)

(defcustom pianobar-password nil
  "Password for pando"
  :group 'pianobar)

(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'pianobar-mode-hook 'ansi-color-for-comint-mode-on)
(defvar pianobar-buffer "*pianobar*")

(defun pianobar-start-process (cmd)
  "Run an inferior pianobar process, input and output via buffer `*pianobar*'.
If there is a process already running in `*pianobar*', switch to that buffer.
With argument, allows you to edit the command line (default is value
of `inferior-pianobar-program-command').
Runs the hook `pianobar-mode-hook' \(after the `comint-mode-hook'
is run).
\(Type \\[describe-mode] in the process buffer for a list of commands.)"

  (interactive (list
                (if current-prefix-arg
                    (read-string "Run pianobar: " pianobar-program-command)
                  pianobar-program-command)))

    (if (not (comint-check-proc pianobar-buffer))
        (save-excursion
          (let ((cmdlist (split-string cmd)))
            (set-buffer (apply 'make-comint "pianobar" (car cmdlist)
                               nil (cdr cmdlist)))
            (pianobar-mode))

          (accept-process-output (get-buffer-process pianobar-buffer) 5)
          ;; give it a second to start up
          (sleep-for 0 1)
          (setq pianobar-buffer "*pianobar*")
          (if pianobar-username
              (comint-simple-send (pianobar-proc) pianobar-username))
          (if pianobar-password
              (comint-simple-send (pianobar-proc) pianobar-password))
          (add-hook 'comint-output-filter-functions
                    'pianobar-comint-output-filter-function nil t)
          ;; make them select the station
          (switch-to-pianobar)
          (run-hooks 'pianobar-mode-hook))))

(defun pianobar-send-string (string)
  "Evaluate string in from pianobar process."
  (interactive "sCommand: ")
  (comint-send-string (pianobar-proc) string))

(defun pianobar-proc ()
  "Return the current pianobar process.
See variable `inferior-pianobar-buffer'.  Starts a new process if necessary."
  (unless (comint-check-proc pianobar-buffer)
    (pianobar-start-process pianobar-program-command))
  (get-buffer-process (if (derived-mode-p 'pianobar-mode)
              (current-buffer)
            pianobar-buffer)))

(defun switch-to-pianobar ()
  "Switch to the pianobar process buffer."
  (interactive)
  (if (or (and pianobar-buffer (get-buffer pianobar-buffer))
          (pianobar-start-process pianobar-program-command))
      (if (get-buffer pianobar-buffer)
          (pop-to-buffer pianobar-buffer)
        (push-mark)
        (goto-char (point-max)))))


(defun pianobar-maybe-message (string)
  "This will message the user if the user is not
currently in pianobar"
  (if (not (eq (buffer-name (current-buffer)) pianobar-buffer))
      (message string)))


;;;; Pianobar commands
;;; TODO redo this using macros maybe, i don't know lisp very well
;; +    love current song
(defun pianobar-love-current-song ()
  "Tells pandora that you like this song"
  (interactive)
  (pianobar-send-string "+"))

;; -    ban current song
(defun pianobar-ban-current-song ()
  "Tells pandora that you do not like this song"
  (interactive)
  (pianobar-send-string "-"))

;; p    pause/continue
(defun pianobar-pause-song ()
  "Pauses or unpauses the pianobar"
  (interactive)
  (pianobar-send-string "p"))

;; c    create new station
(defun pianobar-create-station ()
  "Initiates the create station interaction"
  (interactive)
  (pianobar-send-string "c"))

;; d    delete current station
(defun pianobar-delete-current-station()
  (interactive)
  (pianobar-send-string "d"))

;; e    explain why this song is played
(defun pianobar-explain-song ()
  (interactive)
  (pianobar-send-string "e"))

;; g    add genre station
(defun pianobar-add-genre-station ()
  (interactive)
  (pianobar-send-string "g"))

;; h    song history
(defun pianobar-song-history ()
  (interactive)
  (pianobar-send-string "h"))

;; i    print information about current song/station
(defun pianobar-song-information ()
  (interactive)
  (pianobar-send-string "i"))

;; j    add shared station
(defun pianobar-add-shared-station ()
  (interactive)
  (pianobar-send-string "j"))

;; m    move song to different station
(defun pianobar-move-song-different-station ()
  (interactive)
  (pianobar-send-string "m"))

;; n    next song
(defun pianobar-next-song ()
  (interactive)
  (pianobar-send-string "n"))

;; q    quit
(defun pianobar-quit ()
  (interactive)
  (pianobar-send-string "q"))

;; r    rename current station
(defun pianobar-rename-current-station ()
  (interactive)
  (pianobar-send-string "r"))

;; s    change station
(defun pianobar-change-station ()
  (interactive)
  (pianobar-send-string "s")
  (switch-to-pianobar))

;; t    tired (ban song for 1 month)
(defun pianobar-tired-of-song ()
  (interactive)
  (pianobar-send-string "t"))

;; u    upcoming songs
(defun pianobar-upcoming-songs ()
  (interactive)
  (pianobar-send-string "u"))

;; x    select quickmix stations
(defun pianobar-select-quickmix-stations ()
  (interactive)
  (pianobar-send-string "x"))

;; b    bookmark song/artist
(defun pianobar-bookmark-song ()
  (interactive)
  (pianobar-send-string "b"))

(defvar inferior-pianobar-mode-map
  (let ((m (make-sparse-keymap)))
    ;(define-key m "\C-x\C-e" 'pianobar-send-last-sexp)
    ;(define-key m "\C-cl" 'pianobar-load-file)
    m))

(defun pianobar-window-show ()
  "Show the pianobar window."
  (interactive)
  (let ((pianobar-buff-window pianobar-buffer))
    (unless (windowp pianobar-buff-window)
      ;; bottom window
      (select-window (frame-first-window))
      (other-window -1)
      ;; cut into two if big enough  (This is three lines)
      (when (< (* 2 window-min-height) (window-height))
        (split-window)
        (other-window 1))
      (set-window-buffer (selected-window) pianobar-buffer)
      ;; make it tiny
      (enlarge-window (- window-min-height (window-height)))
      nil)
    pianobar-buff-window))

(defun pianobar-window-hide ()
  "Hide the pianobar window."
  (delete-windows-on pianobar-buffer))

(defun pianobar-window-toggle ()
  "Toggle the visiblity of the pianobar window."
  (interactive)
  ;; will start the pianobar if not started already
  (pianobar-proc)
  (let ((thiswin (selected-window)))
    (if (get-buffer-window pianobar-buffer)
        (pianobar-window-hide)
      (progn
        (pianobar-window-show)
        (sit-for 0)
    (select-window thiswin)))))

(defun pianobar-format-current-song (string)
  "Removes extranous characters so when the song is displayed to
the user it looks a little better. TODO remove backslashes"
  string)
  ;;(replace-in-string (replace-in-string string "|>" "" ) "\n" ""))

(defun pianobar-comint-output-filter-function (string)
  "Watch output and keep our current song up to date, also
message when the song changes."
  (if (string-match "|>" string)
      (let ((old-song pianobar-current-song)
            (new-song (pianobar-format-current-song string)))
        (if (not (string= old-song new-song))
            (progn
              (setq pianobar-current-song new-song)
              (pianobar-currently-playing))))))

(defun pianobar-currently-playing ()
  "Messages the currently playing song"
  (interactive)
  (if pianobar-current-song
      (prog2
          (message (concat "Now playing: "  pianobar-current-song))
          ;; override the growl function with an external notification system if you wish
          (if (functionp 'growl) (growl "Now Playing" pianobar-current-song)))))

(defun pianobar-key-setup (&optional key)
  "Setup the KEY map for telling pianobar what to do."
  (unless key
    (setq key pianobar-key))
  (setq pianobar-key-map (make-sparse-keymap))
  (defalias 'pianobar-key-prefix pianobar-key-map)

  (global-set-key key 'pianobar-key-prefix)

  (let ((map pianobar-key-map)
        (mode-map inferior-pianobar-mode-map))
    ;; map the keys
    (define-key map key     'pianobar-window-toggle)
    (define-key map "+"     'pianobar-love-current-song)
    (define-key map "-"     'pianobar-ban-current-song)
    (define-key map "c"     'pianobar-create-station)
    (define-key map "d"     'pianobar-delete-current-station)
    (define-key map "e"     'pianobar-explain-song)
    (define-key map "g"     'pianobar-add-shared-station)
    (define-key map "h"     'pianobar-song-history)
    (define-key map "i"     'pianobar-currently-playing)
    (define-key map "j"     'pianobar-add-shared-station)
    (define-key map "m"     'pianobar-move-song-different-station)
    (define-key map "q"     'pianobar-quit)
    (define-key map "r"     'pianobar-rename-current-station)
    (define-key map "s"     'pianobar-change-station)
    (define-key map "t"     'pianobar-tired-of-song)
    (define-key map "u"     'pianobar-upcoming-songs)
    (define-key map "x"     'pianobar-select-quickmix-stations)
    (define-key map "b"     'pianobar-bookmark-song)

    ;; next song
    (define-key map [right] 'pianobar-next-song)
    (define-key map "n" 'pianobar-next-song)
    ;; pause/unpause
    (define-key map " "     'pianobar-pause-song)
    (define-key map "p"     'pianobar-pause-song)

    nil))

;; globally set the pianobar interactions
(pianobar-key-setup)

;;;###autoload
(defun pianobar ()
  "Starts piano bar see \\{pianobar-mode} for a list of valid keys"
  (interactive)
  (pianobar-proc)
  (switch-to-pianobar))



(define-derived-mode pianobar-mode comint-mode "pianobar"
  "Major mode for interacting with an inferior pianobar process.

A pianobar process can be fired up with M-x pianobar.

Since pianobar can no read direct input from the buffer to issue a command
you must first press the \\{pianobar-key} (usually f7) and then press one
of the following commands:
+    love current song
-    ban current song
a    add music to current station
c    create new station
d    delete current station
e    explain why this song is played
g    add genre station
h    song history
i    print information about current song/station
j    add shared station
m    move song to different station
n    next song
p    pause/continue
q    quit
r    rename current station
s    change station
t    tired (ban song for 1 month)
u    upcoming songs
x    select quickmix stations
b    bookmark song/artist

When pianobar is expecting you to type something normal comint input will work.

Customization: Entry to this mode runs the hooks on comint-mode-hook and
 pianobar-mode-hook (in that order).

You can send text to the inferior pianobar process from othber buffers containing
pianobar source.
    switch-to-pianobar switches the current buffer to the pianobar process buffer.
    pianobar-send-region sends the current region to the pianobar process.


"
  (pianobar-key-setup)
  (use-local-map pianobar-mode-map))

(provide 'pianobar)
