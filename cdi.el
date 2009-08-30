;;; cdi.el --- interface between Emacs and command line CD players

;;; Copyright (C) 2000, 2001, 2002, 2003, 2004 Matthew P. Hodges

;; Author: Matthew P. Hodges <MPHodges@member.fsf.org>
;; Version: $Id: cdi.el,v 1.90 2005/01/17 09:38:34 mphodges-guest Exp $

;; cdi.el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; cdi.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;;; Commentary:
;;
;; Package to control command line CD players.
;;
;; Just run the cdi-start command, which will pop up a *cdi* buffer
;; with the status of the CD (eg Playing) and track/artist
;; information. Various functions are bound to keys in the *cdi*
;; buffer, eg ">" is bound to cdi-next-track -- use ? or C-h b to find
;; all the bindings.

;;; Code:

(defconst cdi-version "2.0.0"
  "Version number of this package.")

;; Settings to customize

(defvar cdi-mode-line-pixmap nil)

;; Per-program regexps for status

(defvar cdi-status-list
  '(("cda"
     (no-disc . "No_Disc")
     (stopped . "CD_Stopped")
     (playing . "CD_Playing")
     (paused  . "CD_Paused"))
    ("cdcd"
     (no-disc . "No disc")
     (stopped . "Stopped")
     (playing . "Playing")
     (paused  . "Paused")))
    "Mapping of programs with disc statuses.")

;; Per-program information mining

(defvar cdi-program-list
  `(("cda"
     ;; Commands
     (start-daemon . "on")
     (stop-daemon . "off")
     (eject . "disc eject")
     (load . "disc load")
     (play . "play")
     (pause . "pause")
     (resume . "play")
     (stop . "stop")
     (next . "track next")
     (previous . "track prev")
     (shuffle-on . "shuffle on")
     (shuffle-off . "shuffle off")
     (repeat-on . "repeat on")
     (repeat-off . "repeat off")
     (set-volume . "volume")
     (set-program . "program")
     (clear-program . "program clear")
     ;; Matchers for static data
     (album-title . ("toc" "^.* / \\(.*\\)"))
     (album-artist . ("toc" "^\\(.*\\) / "))
     (album-tracks . ("toc" "^.[0-9]+ [0-9]+:[0-9]+\\s-+\\(.*\\)$"))
     ;; Matchers for transient data
     (status . ("status"
                ,(concat
                  "^"
                  (regexp-opt
                   (mapcar (lambda (elt)
                             (cdr elt))
                           (cdr (assoc "cda" cdi-status-list))) t))))
     (current-track . ("status" "CD_\\w+\\s-+disc:[0-9]+\\s-+\\([0-9]+\\)"))
     (shuffle-status . ("status" "\\+shuf"))
     (repeat-status . ("status" "\\+rept"))
     (volume . ("volume" "volume: \\([0-9]+\\)"))
     (volume-max . ("volume" "volume: [0-9]+ (range [0-9]+-\\([0-9]+\\))"))
     (volume-min . ("volume" "volume: [0-9]+ (range \\([0-9]\\)+-[0-9]+)"))
     ;; Miscellaneous
     (dump-info . "toc")
     (print-status . ("status" "CD_\\|No_Disc")))
    ("cdcd"
     (eject . "open")
     (load . "close")
     (play . "play")
     (pause . "pause")
     (resume ."resume")
     (stop . "stop")
     (next . "next")
     (previous . "prev")
     (skip-forward . "ff")
     (skip-backward . "rew")
     (set-volume . "setvol")
     ;; Matchers for static data
     (album-title . ("tracks" "Album name:[ \t]+\\(.*\\)"))
     (album-artist . ("tracks" "Album artist:[ \t]+\\(.*\\)"))
     (album-tracks . ("tracks" "^[^]]*\\][ \t]+\\(.*?\\)\\s-?$"))
     ;; Matchers for transient data
     (status . ("info"
                ,(concat
                  "^"
                  (regexp-opt
                   (mapcar (lambda (elt)
                             (cdr elt))
                           (cdr (assoc "cdcd" cdi-status-list))) t))))
     (current-track . ("status" "^\\w+ n.\\([0-9]+\\)"))
     (volume . ("getvol" "Front\\s-+\\([0-9]+\\)"))
     (volume-max . ("help setvol" "Valid volumes: [0-9]+ - \\([0-9]+\\)"))
     (volume-min . ("help setvol" "Valid volumes: \\([0-9]+\\) - [0-9]+"))
     ;; Miscellaneous
     (dump-info . "tracks")
     (print-status . ("status" ".*"))))
  "Association of commands with obtaining information.")

;; Customizable variables

(defgroup cdi nil
  "Interface between Emacs and command line CD players."
  :group 'tools
  :link '(url-link "http://www.tc.bham.ac.uk/~matt/published/Public/CdiEl.html"))

(defcustom cdi-program "cdcd"
  "*The name of the program used by the interface.
Interfaces can be added by adapting `cdi-program-list' and changed
using `cdi-change-interface'."
  :group 'cdi
  :type
  (let ((choices
         (mapcar (lambda (elt) (list 'const (car elt)))
                 cdi-program-list)))
    (nconc '(choice) choices)))

(defcustom cdi-timer-period 30
  "*The number of seconds between updates of information."
  :group 'cdi
  :type 'integer)

(defcustom cdi-volume-increment 10
  "*The amount to increase/decrease the volume by."
  :group 'cdi
  :type 'integer)

(defcustom cdi-verbose nil
  "*If non-nil, cdi will issue extra messages."
  :group 'cdi
  :type 'boolean)

(defcustom cdi-display-type 'all-tracks
  "*Display mode used by `cdi-refresh-display-buffer'."
  :group 'cdi
  :type '(choice (const :tag "Current track" one-track)
                 (const :tag "All tracks" all-tracks))
  :set (lambda (sym val)
         (set sym val)
         (when (fboundp 'cdi-refresh-display-buffer)
           (cdi-refresh-display-buffer))))

(defcustom cdi-auto-stop-daemon t
  "*If nil, query before stopping daemon process."
  :group 'cdi
  :type 'boolean)

;; Faces

(defface cdi-status-face
  '((((class color))
     (:foreground "purple" :bold t)))
  "Face used for CDI status information."
  :group 'cdi)

(defface cdi-header-face
  '((((class color))
     (:foreground "blue")))
  "Face used for CDI header information."
  :group 'cdi)

(defface cdi-contents-face
  '((((class color))
     (:foreground "green4" :italic t)))
  "Face used for CDI contents information."
  :group 'cdi)

(defface cdi-current-track-face
  '((((class color))
     (:inherit cdi-contents-face :background "lightblue")))
  "Face used for CDI current tracks."
  :group 'cdi)

;; Hook variables

(defvar cdi-refresh-display-buffer-hooks nil
  "Hooks run after *cdi* buffer is updated.
When this hook is run, *cdi* is the current buffer, and
`inhibit-read-only' is non-nil.")

(defvar cdi-quit-hooks nil
  "Hooks run after quitting cdi.")

;; Variables associated with CD status

(defvar cdi-current-status "Unknown"
  "Status of current CD.")

(defvar cdi-timer nil)

;; Variables associated with CD data

(defvar cdi-current-artist nil
  "Artist of current CD.")

(defvar cdi-current-title nil
  "Name of current CD.")

(defvar cdi-current-track nil
  "Name of track being played (if any).")

(defvar cdi-track-alist nil
  "List of tracks on current CD.")

(defvar cdi-shuffle-status nil
  "Set to t if CD is not being shuffled.")

(defvar cdi-repeat-status nil
  "Set to t if CD is not being repeated.")

(defvar cdi-current-volume nil
  "Volume the CD is set to.
This isn't initialized until a volume command is used.")

(defvar cdi-maximum-volume nil
  "Maximum volume the CD can be set to.")

(defvar cdi-minimum-volume nil
  "Minimum volume the CD can be set to.")

(defvar cdi-ignored-tracks nil
  "List of tracks not to be played.")

;; Miscellaneous variables

(defvar cdi-inhibit-get-info nil)

(defvar cdi-current-line nil)

(defvar cdi-current-column nil)

;; XEmacs support

(defconst cdi-xemacs-p
  (or (featurep 'xemacs)
      (string-match "XEmacs\\|Lucid" (emacs-version)))
  "True if we are using CDI under XEmacs.")

;; Other version-dependent configuration

(defalias 'cdi-line-beginning-position
  (cond
   ((fboundp 'line-beginning-position) 'line-beginning-position)
   ((fboundp 'point-at-bol) 'point-at-bol)))

(defalias 'cdi-line-end-position
  (cond
   ((fboundp 'line-end-position) 'line-end-position)
   ((fboundp 'point-at-eol) 'point-at-eol)))

(defconst cdi-face-property
  (if (with-temp-buffer
        ;; We have to rename to something without a leading space,
        ;; otherwise font-lock-mode won't get activated.
        (rename-buffer "*test-font-lock*")
        (font-lock-mode 1)
        (and (boundp 'char-property-alias-alist)
             (member 'font-lock-face
                     (assoc 'face char-property-alias-alist))))
      'font-lock-face
    'face)
  "Use font-lock-face if `add-text-properties' supports it.
Otherwise, just use face.")

;; Interactive commands

;;;###autoload
(defun cdi-start ()
  "Setup the cdi buffer and perhaps start a daemon."
  (interactive)
  ;; Check that cdi-program is set to a string
  (if (not (stringp cdi-program))
      (error "The variable cdi-program must be set to a string"))
  (cdi-start-daemon)
  (cdi-refresh-display-buffer)
  (if (equal (buffer-name (current-buffer)) "*cdi*")
      (message
       (concat "Welcome to the Emacs CD interface. "
               "Press ? or C-h b for keybindings."))
    (display-buffer "*cdi*")))

(defalias 'cdi 'cdi-start)

(defun cdi-get-status-string (status)
  "Get string associated with STATUS from `cdi-status-list'."
  (cdr
   (assoc status
          (assoc cdi-program cdi-status-list))))

(defun cdi-play-or-pause ()
  "Play the CD (or pause if it's playing)."
  (interactive)
  (cond
   ((string-match (cdi-get-status-string 'playing) cdi-current-status)
    (cdi-command 'pause)
    (cdi-stop-timer))
   ((string-match (cdi-get-status-string 'paused) cdi-current-status)
    (cdi-command 'resume))
   ((string-match (cdi-get-status-string 'stopped) cdi-current-status)
    (cdi-command 'play))))

(defun cdi-stop ()
  "Stop the CD."
  (interactive)
  (setq cdi-current-track nil)
  (cdi-command 'stop))

(defun cdi-eject ()
  "Eject the CD."
  (interactive)
  (cdi-clear-cd-info)
  (let ((cdi-inhibit-get-info t))
    (cdi-command 'eject)))

(defun cdi-load ()
  "Load the CD."
  (interactive)
  (cdi-clear-cd-info)
  (cdi-command 'load))

(defun cdi-next-track ()
  "Choose the next track."
  (interactive)
  (or (cdi-barf-if-cd-stopped 'next)
      (cdi-command 'next)))

(defun cdi-prev-track ()
  "Choose the previous track."
  (interactive)
  (or (cdi-barf-if-cd-stopped 'previous)
      (cdi-command 'previous)))

(defun cdi-skip-forward ()
  "Skip forward in the current track."
  (interactive)
  (or (cdi-barf-if-cd-stopped 'skip-forward)
      (cdi-command 'skip-forward)))

(defun cdi-skip-backward ()
  "Skip backward in the current track."
  (interactive)
  (or (cdi-barf-if-cd-stopped 'skip-backward)
      (cdi-command 'skip-backward)))

(defun cdi-refresh-display-buffer ()
  "Refresh the displayed CD information."
  (interactive)
  ;; Set up the window
  (if (not (get-buffer "*cdi*"))
      (progn
        (get-buffer-create "*cdi*")
        (set-buffer "*cdi*")
        (setq buffer-read-only t)
        (let ((window (display-buffer "*cdi*")))
          (select-window window))
        (cdi-mode)
        ;; Needed for GNU Emacs 20?
        (make-local-hook 'kill-buffer-hook)
        (add-hook 'kill-buffer-hook 'cdi-stop-timer nil t)
        (cdi-update-modeline)
        (force-mode-line-update)))
  ;; Get information and display it
  (if (not cdi-inhibit-get-info)
      (progn
        (cdi-get-transient-info)
        (if (and
             (not (string-match (cdi-get-status-string 'no-disc)
                                cdi-current-status))
             (not (string-match "Unknown"
                                cdi-current-status))
             (not cdi-current-title))
            (cdi-get-static-info))))
  (with-current-buffer "*cdi*"
    (let ((inhibit-read-only t)
          cdi-current-line cdi-current-column)
      (cdi-save-position)
      (erase-buffer)
      (insert (format "[%s%s%s]"
                      cdi-current-status
                      (if cdi-shuffle-status
                          " / shuffle"
                        "")
                      (if cdi-repeat-status
                          " / repeat"
                        "")))
      (add-text-properties (cdi-line-beginning-position)
                           (cdi-line-end-position)
                           `(,cdi-face-property cdi-status-face))
      (insert "\n")
      (when cdi-current-artist
        (cdi-insert-field "Artist:" cdi-current-artist)
        (cdi-insert-field "Title: " cdi-current-title)
        (cond
         ((eq cdi-display-type 'one-track)
          (when cdi-current-track
            (cdi-insert-one-track-data)))
         ((eq cdi-display-type 'all-tracks)
          (cdi-insert-all-tracks-data))
         (t
          (error "Unrecognized cdi-display-type"))))
      (cdi-restore-position)
      (if (and (cdi-cd-playing-or-paused-p) (> cdi-current-track 0))
          (if (not cdi-timer)
              (cdi-start-timer))
        (cdi-stop-timer))
      (run-hooks 'cdi-refresh-display-buffer-hooks))
    (set-buffer-modified-p nil)))

(defun cdi-save-position ()
  "Save buffer position (line and column)."
  (setq cdi-current-line (count-lines (point-min)
                                      (cdi-line-beginning-position))
        cdi-current-column (current-column)))

(defun cdi-restore-position ()
  "Restore buffer position (line and column)."
  (let (line-length)
    (goto-char (point-min))
    (forward-line cdi-current-line)
    (setq line-length (- (cdi-line-end-position)
                         (cdi-line-beginning-position)))
    (forward-char
     (if (> cdi-current-column line-length)
         line-length
       cdi-current-column))))

(defun cdi-choose-track ()
  "Choose a track from `cdi-track-alist'."
  (interactive)
  (let* ((completion-ignore-case t)
         (track (completing-read "Choose track: " cdi-track-alist))
         (number (cdr (assoc track cdi-track-alist))))
    (if number
        (cdi-command 'play (format "%d" number))
      (error "%s is not a valid track selection" track))))

(defun cdi-toggle-shuffle ()
  "Turn CD shuffling on/off."
  (interactive)
  (if cdi-shuffle-status
      (cdi-command 'shuffle-off)
    (cdi-command 'shuffle-on)))

(defun cdi-toggle-repeat ()
  "Turn CD repeating on/off."
  (interactive)
  (if cdi-repeat-status
      (cdi-command 'repeat-off)
    (cdi-command 'repeat-on)))

(defun cdi-first-track ()
  "Play the first track of the CD."
  (interactive)
  (cdi-command 'play "1"))

(defun cdi-dump-cdinfo ()
  "Dump the CD information into the current buffer."
  (interactive)
  (let ((command (cdi-get-command 'dump-info)))
    (if command
        (call-process cdi-program nil t nil command))))

(defun cdi-volume-up ()
  "Increase volume by `cdi-volume-increment'."
  (interactive)
  (cdi-volume-change cdi-volume-increment))

(defun cdi-volume-down ()
  "Decrease volume by `cdi-volume-increment'."
  (interactive)
  (cdi-volume-change (- cdi-volume-increment)))

(defun cdi-volume-set ()
  "Set volume to value read from the minibuffer."
  (interactive)
  (let (prompt volume)
    (setq prompt (format "Enter volume%s: "
                         (if cdi-minimum-volume
                             (format " (%d - %d)"
                                     cdi-minimum-volume
                                     cdi-maximum-volume)
                           "")))
    (setq volume (string-to-int
                  (read-from-minibuffer prompt nil nil nil nil)))
    (cdi-volume-change volume t)))

(defun cdi-change-interface ()
  "Change the command line interface used.
The list is inferred from `cdi-program-list'."
  (interactive)
  (let* ((interface-list
          (mapcar
           (lambda (w)
             (make-list 2 (car w)))
           cdi-program-list))
         (chosen-interface
          (completing-read "Choose CD interface: " interface-list nil t)))
    (if (not (string-equal cdi-program chosen-interface))
        (progn
          (when (cdi-cd-playing-or-paused-p)
            (cdi-command 'stop))
          (cdi-stop-daemon)
          (setq cdi-program chosen-interface)
          (cdi-start-daemon)
          (cdi-clear-cd-info)
          (setq cdi-current-volume nil
                cdi-maximum-volume nil
                cdi-minimum-volume nil)
          (if (get-buffer "*cdi*")
              (progn
                (set-buffer "*cdi*")
                (cdi-update-modeline)))
          (cdi-refresh-display-buffer)))))

(defun cdi-quit ()
  "Quit and clean up cdi-related stuff."
  (interactive)
  (when (y-or-n-p "Really quit CDI? ")
    (cdi-stop-daemon)
    (cdi-clear-cd-info)
    (cdi-stop-timer)
    (kill-buffer "*cdi*")
    (run-hooks 'cdi-quit-hooks)))

(defun cdi-bury-buffer ()
  "Bury the *cdi* buffer."
  (interactive)
  (if (eq (window-buffer) (get-buffer "*cdi*"))
      (quit-window)))

(defun cdi-clear-ignored-tracks ()
  "Clear list of ignored tracks.
The list is held in `cdi-ignored-tracks'."
  (interactive)
  (setq cdi-ignored-tracks nil)
  (cdi-refresh-display-buffer)
  (when (cdi-get-command 'clear-program)
    (cdi-command 'clear-program)))

(defun cdi-toggle-current-track-ignored ()
  "Toggle ignored status of the current track."
  (interactive)
  (when (cdi-cd-playing-or-paused-p)
    (cdi-get-transient-info)
    (cdi-toggle-track-ignored cdi-current-track)
    (cdi-refresh-display-buffer)
    (cdi-set-program)))

(defun cdi-toggle-selected-track-ignored ()
  "Toggle ignored status of a chosen track from `cdi-track-alist'."
  (interactive)
  (let* ((completion-ignore-case t)
         (track (completing-read "Choose track: " cdi-track-alist))
         (number (cdr (assoc track cdi-track-alist))))
    (when number
      (cdi-toggle-track-ignored number)
      (cdi-refresh-display-buffer)
      (cdi-set-program))))

(defun cdi-toggle-all-tracks-ignored ()
  "Toggle ignored status of all tracks."
  (interactive)
  (mapcar (lambda (track)
            (cdi-toggle-track-ignored (cdr track)))
          cdi-track-alist)
  (cdi-refresh-display-buffer)
  (cdi-set-program))

(defun cdi-toggle-track-at-point-ignored ()
  "Toggle ignored status of the track at point."
  (interactive)
  (let ((track (cdi-track-at-point)))
    (when track
      (cdi-toggle-track-ignored track)
      (cdi-refresh-display-buffer)
      (cdi-set-program))))

(defun cdi-all-tracks-ignored ()
  "Mark all tracks as ignored."
  (interactive)
  (setq cdi-ignored-tracks nil)
  (dotimes (i (length cdi-track-alist))
    (setq cdi-ignored-tracks (cons (1+ i) cdi-ignored-tracks)))
  (setq cdi-ignored-tracks (nreverse cdi-ignored-tracks))
  (cdi-refresh-display-buffer)
  (cdi-set-program))

(defun cdi-play-track-at-point ()
  "Play track at point."
  (interactive)
  (let ((track (cdi-track-at-point)))
    (when track
      (cdi-command 'play (int-to-string track)))))

(defun cdi-move-to-next-track (&optional arg)
  "Move point to the ARG next track.
ARG may be negative to move backward."
  (interactive "p")
  (unless (equal major-mode 'cdi-mode)
    (error "Not in CDI buffer"))
  (cond
   ;; No links
   ((or (null cdi-track-alist)
        (and (eq cdi-display-type 'one-track)
             (not (cdi-cd-playing-or-paused-p))))
    (message "No track to move to"))
   ;; One link
   ((or (eq cdi-display-type 'one-track)
        (= (length cdi-track-alist) 1))
    (goto-char (point-min))
    (goto-char (next-single-property-change (point)
                                                 'cdi-track-number)))
   (t
    (let ((pos (point))
          (number arg)
          (old (cdi-track-at-point))
          new)
      ;; Forward.
      (while (> arg 0)
        (cond ((eobp)
               (goto-char (point-min)))
              (t
               (goto-char (or (next-single-property-change
                               (point) 'cdi-track-number)
                              (point-max)))))
        (let ((new (cdi-track-at-point)))
          (when new
            (unless (eq new old)
              (setq arg (1- arg))
              (setq old new)))))
      ;; Backward.
      (while (< arg 0)
        (cond ((bobp)
               (goto-char (point-max)))
              (t
               (goto-char (or (previous-single-property-change
                               (point) 'cdi-track-number)
                              (point-min)))))
        (let ((new (cdi-track-at-point)))
          (when new
            (unless (eq new old)
              (setq arg (1+ arg))))))
      ;; Go to beginning of field.
      (let ((new (cdi-track-at-point)))
        (while (eq (cdi-track-at-point) new)
          (backward-char)))
      (forward-char)))))

(defun cdi-move-to-previous-track (&optional arg)
  "Move point to the ARG previous track.
ARG may be negative to move forward."
  (interactive "p")
  (cdi-move-to-next-track (- arg)))

(defun cdi-print-status ()
  "Print cdi status to echo area."
  (interactive)
  (let ((command (car (cdi-get-command 'print-status)))
        (regexp (cadr (cdi-get-command 'print-status))))
    (when command
      (with-temp-buffer
        (apply 'call-process cdi-program nil t nil command nil)
        (when regexp
          (goto-char (point-min))
          (keep-lines regexp))
        (unless (= (point-min) (point-max))
          (message (buffer-substring (point-min) (1- (point-max)))))))))

;; Internal functions

(defun cdi-start-timer ()
  "Start the cdi timer (see `cdi-timer')."
  (if cdi-timer (cancel-timer cdi-timer))
  (setq cdi-timer
        (run-with-timer cdi-timer-period cdi-timer-period
                        'cdi-refresh-display-buffer)))

(defun cdi-stop-timer ()
  "Stop the cdi timer (see `cdi-timer')."
  (if cdi-timer
      (progn
        (cancel-timer cdi-timer)
        (setq cdi-timer nil))))

(defun cdi-start-daemon ()
  "Start the associated daemon process (if one is run)."
  (let ((command (cdi-get-command 'start-daemon)))
    (if command
        (call-process cdi-program nil 0 nil command))))

(defun cdi-stop-daemon ()
  "Stop the associated daemon process (if one is run)."
  (let ((command (cdi-get-command 'stop-daemon)))
    (if (and command
             (or cdi-auto-stop-daemon
                 (y-or-n-p (format "Do you want to kill the %s process? "
                                   cdi-program))))
        (call-process cdi-program nil 0 nil command))))

(defun cdi-clear-cd-info ()
  "Clear out static CD data."
  (setq cdi-current-artist nil
        cdi-current-title nil
        cdi-current-track nil
        cdi-ignored-tracks nil
        cdi-track-alist nil
        cdi-current-status "Unknown"))

(defun cdi-get-static-info ()
  "Get static CD info.
This includes artist name (`cdi-current-artist') album title
\(`cdi-current-title') and album tracks (`cdi-track-alist')."
  ;; We get this using the track-list, album-title and album-artist
  ;; properties in cdi-program-list for the program given by
  ;; cdi-program. We do this all in a temporary buffer which we try to
  ;; reuse if the same command is used for different properties.
  (let ((last-command nil)
        info
        this-command
        regexp)
    (with-temp-buffer
      ;; Get the artist
      (setq info (cdi-get-command 'album-artist))
      (setq this-command (split-string (or (car info) "")))
      (setq regexp (nth 1 info))
      ;; Insert if necessary
      (or (equal this-command last-command)
          (erase-buffer)
          (apply 'call-process cdi-program nil t nil this-command))
      (setq last-command this-command)
      ;; Search for the artist
      (goto-char (point-min))
      (if (re-search-forward regexp (point-max) t)
          (setq cdi-current-artist (match-string 1))
        (setq cdi-current-artist "Unknown"))
      ;; Get the title
      (setq info (cdi-get-command 'album-title))
      (setq this-command (split-string (or (car info) "")))
      (setq regexp (nth 1 info))
      ;; Insert if necessary
      (or (equal this-command last-command)
          (erase-buffer)
          (apply 'call-process cdi-program nil t nil this-command))
      (setq last-command this-command)
      ;; Search for the title
      (goto-char (point-min))
      (re-search-forward regexp)
      (setq cdi-current-title (match-string 1))
      (when (string-equal cdi-current-title "")
        (setq cdi-current-title "Unknown"))
      ;; Get the tracks
      (setq info (cdi-get-command 'album-tracks))
      (setq this-command (split-string (or (car info) "")))
      (setq regexp (nth 1 info))
      ;; Insert if necessary
      (or (equal this-command last-command)
          (erase-buffer)
          (apply 'call-process cdi-program nil t nil this-command))
      (setq last-command this-command)
      ;; Search for the tracks
      (goto-char (point-min))
      (setq cdi-track-alist nil)
      (let ((track-number 1)
            match)
        (while (re-search-forward regexp (point-max) t)
          (setq match (match-string 1))
          (when (string-equal match "")
              (setq match (concat "Track " (int-to-string track-number))))
          (setq cdi-track-alist
                (cons (cons match track-number) cdi-track-alist))
          (setq track-number (1+ track-number))))
      (setq cdi-track-alist (reverse cdi-track-alist)))))

(defun cdi-get-transient-info ()
  "Get transient CD info.
This includes the current status (`cdi-current-status'), the current
track (`cdi-current-track'), shuffle status (`cdi-shuffle-status') and
repeat status (`cdi-repeat-status')."
  (let ((last-command nil)
        info
        this-command
        regexp)
    (with-temp-buffer
      ;; Get the status (playing, paused etc.)
      (setq info (cdi-get-command 'status))
      (setq this-command (split-string (or (car info) "")))
      (setq regexp (nth 1 info))
      ;; Insert if necessary
      (or (equal this-command last-command)
          (erase-buffer)
          (apply 'call-process cdi-program nil t nil this-command))
      (setq last-command this-command)
      ;; Search for the status
      (goto-char (point-min))
      (if (re-search-forward regexp (point-max) t)
          (setq cdi-current-status (match-string 0))
        (setq cdi-current-status "Unknown"))
      ;; Get shuffle-status
      (setq info (cdi-get-command 'shuffle-status))
      (setq this-command (split-string (or (car info) "")))
      (setq regexp (nth 1 info))
      (if regexp
          (progn
            ;; Insert if necessary
            (or (equal this-command last-command)
                (erase-buffer)
                (apply 'call-process cdi-program nil t nil this-command))
            (setq last-command this-command)
            ;; Search for the shuffle-status
            (goto-char (point-min))
            (if (re-search-forward regexp (point-max) t)
                (setq cdi-shuffle-status t)
              (setq cdi-shuffle-status nil)))
        (setq cdi-shuffle-status nil))
      ;; Get repeat-status
      (setq info (cdi-get-command 'repeat-status))
      (setq this-command (split-string (or (car info) "")))
      (setq regexp (nth 1 info))
      (if regexp
          (progn
            ;; Insert if necessary
            (or (equal this-command last-command)
                (erase-buffer)
                (apply 'call-process cdi-program nil t nil this-command))
            (setq last-command this-command)
            ;; Search for the repeat-status
            (goto-char (point-min))
            (if (re-search-forward regexp (point-max) t)
                (setq cdi-repeat-status t)
              (setq cdi-repeat-status nil)))
        (setq cdi-repeat-status nil))
      ;; Get the current track if CD is playing/paused
      (if (cdi-cd-playing-or-paused-p)
          (progn
            (setq info (cdi-get-command 'current-track))
            (setq this-command (split-string (or (car info) "")))
            (setq regexp (nth 1 info))
            ;; Insert if necessary
            (or (equal this-command last-command)
                (erase-buffer)
                (apply 'call-process cdi-program nil t nil this-command))
            (setq last-command this-command)
            ;; Search for the current-track
            (goto-char (point-min))
            (if (re-search-forward regexp (point-max) t)
                (setq cdi-current-track (string-to-int (match-string 1)))
              (error "Cannot find current track")))))))

;; Assuming that each interface will contain play or Pause

(defun cdi-cd-playing-or-paused-p ()
  "Return t is CD is playing or paused, otherwise nil."
  (let ((case-fold-search t))
    (if (or
         (string-match (cdi-get-status-string 'playing) cdi-current-status)
         (string-match (cdi-get-status-string 'paused) cdi-current-status))
        t
      nil)))

(defun cdi-command (command &rest args)
  "Pass COMMAND to the process with ARGS as arguments."
  ;; Get the command associated with the command name
  (let ((command-string (cdi-get-command command))
        command-list)
    (if (not command-string)
        (error "No action defined for %s" (symbol-name command)))
    (setq command-list (split-string command-string))
    (setq command (car command-list))
    ;; If any arguments to the command add them to the list
    (if (cdr command-list)
        (if args
            (setq args (cons args (cdr command-list)))
          (setq args (cdr command-list))))
    (if cdi-verbose
        (progn
          (message "Calling %s: %s %s" cdi-program command
                   (cond
                    ((stringp args)
                     args)
                    ((listp args)
                     (mapconcat #'concat args " "))))
          (run-at-time 2 nil 'message nil)))
    (apply 'call-process cdi-program nil nil nil command args)
    (cdi-refresh-display-buffer)))

(defun cdi-get-command (command)
  "Get the command associated with COMMAND."
  (cdr (assoc command (cdr (assoc cdi-program cdi-program-list)))))

(defun cdi-barf-if-cd-stopped (&optional command)
  "Complain if cd is not running for COMMAND."
  (cond
   ((string-match (cdi-get-status-string 'stopped) cdi-current-status)
    (message (format "CD is stopped%s"
                     (if command
                         (format " (can't %s)" command)
                       "")))
    t)
   ((string-match (cdi-get-status-string 'no-disc) cdi-current-status)
    (message (format "No Disc%s"
                     (if command
                         (format " (can't %s)" command)
                       "")))
    t)
   ((string-match "Unknown" cdi-current-status)
    (message (format "CD status unknown%s"
                     (if command
                         (format " (can't %s)" command)
                       "")))
    t)
   (t
    nil)))

(defun cdi-volume-change (amount &optional absolute)
  "Change the CD volume by AMOUNT.
Optional argument ABSOLUTE can be used to set the absolute volume
rather than incremented or decrementing."
  (let ((last-command nil)
        info
        this-command
        regexp)
  (with-temp-buffer
      ;; Get the current volume
      (setq info (cdi-get-command 'volume))
      (setq this-command (split-string (or (car info) "")))
      (setq regexp (nth 1 info))
      (if regexp
          (progn
            ;; Insert if necessary
            (or (equal this-command last-command)
                (erase-buffer)
                (apply 'call-process cdi-program nil t nil this-command))
            (setq last-command this-command)
            ;; Search for the current volume
            (goto-char (point-min))
            (re-search-forward regexp)
            (setq cdi-current-volume (string-to-int (match-string 1)))))
      ;; Get the maximum volume
      (setq info (cdi-get-command 'volume-max))
      (setq this-command (split-string (or (car info) "")))
      (setq regexp (nth 1 info))
      (if regexp
          (progn
            ;; Insert if necessary
            (or (equal this-command last-command)
                (erase-buffer)
                (apply 'call-process cdi-program nil t nil this-command))
            (setq last-command this-command)
            ;; Search for the current volume
            (goto-char (point-min))
            (re-search-forward regexp)
            (setq cdi-maximum-volume (string-to-int (match-string 1)))))
      ;; Get the minimum volume
      (setq info (cdi-get-command 'volume-min))
      (setq this-command (split-string (or (car info) "")))
      (setq regexp (nth 1 info))
      (if regexp
          (progn
            ;; Insert if necessary
            (or (equal this-command last-command)
                (erase-buffer)
                (apply 'call-process cdi-program nil t nil this-command))
            (setq last-command this-command)
            ;; Search for the current volume
            (goto-char (point-min))
            (re-search-forward regexp)
            (setq cdi-minimum-volume (string-to-int (match-string 1)))))
      ;; Set the volume accordingly; if the absolute flag is set,
      ;; change to amount, otherwise treat this as an increment
      (if absolute
          (setq cdi-current-volume amount)
        (setq cdi-current-volume (+ cdi-current-volume amount)))
      ;; Keep volume inside the limits
      (cond ((> cdi-current-volume cdi-maximum-volume)
             (setq cdi-current-volume cdi-maximum-volume))
            ((< cdi-current-volume cdi-minimum-volume)
             (setq cdi-current-volume cdi-minimum-volume)))
      ;; Now set the volume
      (cdi-command 'set-volume (format "%d" cdi-current-volume)))))

(defun cdi-update-modeline ()
  "Update the modeline to contain `cdi-program'."
  (setq mode-line-format
        (format
         "  Emacs CD player (interface: %s) %%-"
         cdi-program))
  (if (and cdi-mode-line-pixmap
           (file-readable-p cdi-mode-line-pixmap)
           (featurep 'image))
      (let ((image
             (create-image cdi-mode-line-pixmap
                           nil
                           nil
                           :ascent 'center)))
        (put-text-property 0 1 'display image mode-line-format))))

(defun cdi-insert-one-track-data ()
  "Insert data for `cdi-display-type' equal to 'one-track."
  (when cdi-current-track
    (cdi-insert-field "Track: "
                      (concat (car (nth (1- cdi-current-track)
                                        cdi-track-alist))
                              (format " [%d/%d]" cdi-current-track
                                      (length cdi-track-alist))
                              )
                      `(,cdi-face-property cdi-contents-face cdi-track-number
                             ,cdi-current-track))))

(defun cdi-insert-all-tracks-data ()
  "Insert data for `cdi-display-type' equal to 'all-tracks."
  (insert "\n")
  (let (face)
    (mapcar (lambda (elt)
              (cond
               ((and (cdi-cd-playing-or-paused-p)
                     (eq (cdr elt) cdi-current-track))
                (setq face 'cdi-current-track-face))
               (t
                (setq face 'cdi-contents-face)))
              (cdi-insert-field (format "%1s%1s%3d: "
                                        (if (and (cdi-cd-playing-or-paused-p)
                                                 (eq (cdr elt) cdi-current-track)) ">" " ")
                                        (if (cdi-ignored-track-p (cdr elt)) "*" " ")
                                        (cdr elt))
                                (car elt)
                                `(,cdi-face-property ,face cdi-track-number ,(cdr elt))))
            cdi-track-alist)))

(defun cdi-insert-field (header contents &optional props)
  "Insert a string comprising HEADER and CONTENTS to *cdi* buffer.
When PROPS is given, text properties overriding the default face
\(`cdi-contents-face') are added."
  (when contents
    (insert (format "%s " header))
    (add-text-properties (cdi-line-beginning-position)
                         (cdi-line-end-position)
                         `(,cdi-face-property cdi-header-face))
    (insert contents)
    (add-text-properties (+ (cdi-line-beginning-position)
                            (length header) 1)
                         (cdi-line-end-position)
                         (if props
                             props
                           `(,cdi-face-property cdi-contents-face)))
    (insert "\n")))

(defun cdi-ignored-track-p (track-number)
  "Non-nil if TRACK-NUMBER is a member of `cdi-ignored-tracks'."
  (member track-number cdi-ignored-tracks))

(defun cdi-toggle-track-ignored (track)
  "Toggle ignored status of TRACK.
Do this by modifying `cdi-ignored-tracks'."
  (if (cdi-ignored-track-p track)
      (setq cdi-ignored-tracks (delete (cdr
                                        (rassoc track cdi-track-alist))
                                       cdi-ignored-tracks))
    (setq cdi-ignored-tracks
          (nconc cdi-ignored-tracks (list (cdr
                                           (rassoc track cdi-track-alist)))))))

(defun cdi-active-tracks ()
  "Return a list of active tracks.
That is, those in `cdi-track-alist' and not `cdi-ignored-tracks'."
  (let ((tracks (copy-alist cdi-track-alist)))
    (mapcar (lambda (elt)
              (setq tracks (delete (rassoc elt cdi-track-alist) tracks)))
            cdi-ignored-tracks)
    tracks))

(defun cdi-echo-ignored-tracks ()
  "Display message listing ignored tracks."
  (when (eq cdi-display-type 'one-track)
    (message (concat "Ignored tracks: "
                     (mapconcat #'int-to-string cdi-ignored-tracks
                                " ")))))

(defun cdi-set-program ()
  "Play list of programmed tracks."
  (when (cdi-get-command 'set-program)
    (apply 'cdi-command 'set-program
           (mapcar (lambda (elt)
                     (number-to-string (cdr elt)))
                   (cdi-active-tracks))))
  (cdi-echo-ignored-tracks))

(defun cdi-track-at-point ()
  "Get CD track at point."
  (get-text-property (point) 'cdi-track-number))

;; define cdi-mode for the *cdi* buffer

(defvar cdi-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "b") 'cdi-bury-buffer)
    (define-key map (kbd "c") 'cdi-choose-track)
    (define-key map (kbd "e") 'cdi-eject)
    (define-key map (kbd "i") 'cdi-change-interface)
    (define-key map (kbd "l") 'cdi-load)
    (define-key map (kbd "p") 'cdi-play-or-pause)
    (define-key map (kbd "P") 'cdi-print-status)
    (define-key map (kbd "q") 'cdi-quit)
    (define-key map (kbd "r") 'cdi-refresh-display-buffer)
    (define-key map (kbd "R") 'cdi-toggle-repeat)
    (define-key map (kbd "s") 'cdi-stop)
    (define-key map (kbd "S") 'cdi-toggle-shuffle)
    (define-key map (kbd "T RET") 'cdi-toggle-current-track-ignored)
    (define-key map (kbd "T a") 'cdi-all-tracks-ignored)
    (define-key map (kbd "T p") 'cdi-toggle-track-at-point-ignored)
    (define-key map (kbd "T s") 'cdi-toggle-selected-track-ignored)
    (define-key map (kbd "T t") 'cdi-toggle-all-tracks-ignored)
    (define-key map (kbd "T x") 'cdi-clear-ignored-tracks)
    (define-key map (kbd "v") 'cdi-volume-set)
    (define-key map (kbd ">") 'cdi-next-track)
    (define-key map (kbd "<") 'cdi-prev-track)
    (define-key map (kbd "?") 'describe-mode)
    (define-key map (kbd "^") 'cdi-first-track)
    (define-key map (kbd "+") 'cdi-volume-up)
    (define-key map (kbd "-") 'cdi-volume-down)
    (define-key map (kbd "SPC") 'cdi-skip-forward)
    (define-key map (kbd "TAB") 'cdi-move-to-next-track)
    (define-key map (kbd "DEL") 'cdi-skip-backward)
    (define-key map (kbd "RET") 'cdi-play-track-at-point)
    (define-key map [(shift iso-lefttab)] 'cdi-move-to-previous-track)
    (define-key map [(shift tab)]         'cdi-move-to-previous-track)
    map))

;; Menus

(defvar cdi-menu nil
  "Menu to use for `cdi-mode'.")

(when (fboundp 'easy-menu-define)

  (easy-menu-define cdi-menu cdi-mode-map "CDI Menu"
    '("CDI"
      ["Play or Pause"    cdi-play-or-pause t]
      ["Stop"             cdi-stop t]
      ["Load"             cdi-load t]
      ["Eject"            cdi-eject t]
      "---"
      ["Next Track"          cdi-next-track t]
      ["Previous Track"      cdi-prev-track t]
      ["Skip Forward"        cdi-skip-forward t]
      ["Skip Backward"       cdi-skip-backward t]
      ["Choose Track"        cdi-choose-track t]
      ["First Track"         cdi-first-track t]
      ["Play Track at Point" cdi-play-track-at-point t]
      "---"
      ["Move to Next Track"     cdi-move-to-next-track t]
      ["Move to Previous Track" cdi-move-to-previous-track t]
      "---"
      ["Toggle Current Track Ignored"  cdi-toggle-current-track-ignored t]
      ["Toggle Track at Point Ignored" cdi-toggle-track-at-point-ignored t]
      ["Toggle Selected Track Ignored" cdi-toggle-selected-track-ignored t]
      ["Toggle All Tracks Ignored"     cdi-toggle-all-tracks-ignored t]
      ["Clear Ignored Tracks"          cdi-clear-ignored-tracks t]
      "---"
      ["Toggle Repeat"    cdi-toggle-repeat t]
      ["Toggle Shuffle"   cdi-toggle-shuffle t]
      "---"
      ["Volume Up"        cdi-volume-up t]
      ["Volume Down"      cdi-volume-down t]
      ["Volume Set"       cdi-volume-set t]
      "---"
      ["Change Interface" cdi-change-interface t]
      ["Refresh Display"  cdi-refresh-display-buffer t]
      "---"
      ["Bury Buffer"      cdi-bury-buffer t]
      ["Quit"             cdi-quit t])))

(defun cdi-mode ()
  "Major mode for controlling command line CD players.

\\{cdi-mode-map}"
  (kill-all-local-variables)
  (use-local-map cdi-mode-map)
  (setq major-mode 'cdi-mode)
  (setq mode-name "CDI")
  (setq buffer-undo-list t)
  ;; XEmacs
  (when (and (fboundp 'easy-menu-add)
             cdi-menu)
    (easy-menu-add cdi-menu))
  (run-hooks 'cdi-mode-hook))

(provide 'cdi)

;;; cdi.el ends here
