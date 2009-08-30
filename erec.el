;;; erec.el --- Minor mode to record and play back audio

;; Copyright (C) 2003  Free Software Foundation, Inc.

;; Author: Jorgen Schaefer <forcer@forcix.cx>
;; Keywords: multimedia

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

;; This is a minor mode that lets you record audio fragments and plays
;; them back to you when you want.

;; By default, the following keys are bound:

;; C-c C-r C-r  Record some piece of audio. Typing this again wil stop
;;              the recording.
;; C-c C-r C-p  Play the last recorded fragment back to you
;; C-c C-r C-s  Save the last recorded fragment to a file

;;; Code:

;; Custom variables

(defgroup erec nil
  "*Erec, a minor mode to record and play back audio samples."
  :prefix "erec-"
  :group 'external)

(defcustom erec-start-recording 'erec-external-start
  "*A function to start a recording. Its argument is the file to
record to."
  :group 'erec
  :type 'function)

(defcustom erec-stop-recording 'erec-external-stop
  "*A function to stop a currently running recording."
  :group 'erec
  :type 'function)

(defcustom erec-play-recording 'erec-external-play
  "*A function to play an audio file. Its argument is the file play."
  :group 'erec
  :type 'function)

;; Variables

(defvar erec-recording-in-progress nil
  "This is non-nill iff there's a recording in progress currently.")

(defvar erec-temp-file nil
  "The temporary file for audio recordings. Initialized and used by
`erec-audio-file'.")

(defvar erec-mode-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(control ?s)] 'erec-save)
    (define-key map [(control ?p)] 'erec-play)
    (define-key map [(control ?r)] 'erec-record)
    map)
  "The prefix keymap for erec-mode.")

(defvar erec-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(control ?c) (control ?r)] erec-mode-prefix-map)
    map)
  "The minor mode map for erec.")

;; Minor mode definition

(define-minor-mode erec-mode
  "Minor mode for recording and playing back audio samples.
Runs the hook `erc-mode-hook' when being toggled.

\\{erec-mode-map}"
  :group 'erec
  :global t
  (cond
   (erec-mode
    (add-hook 'kill-emacs-hook 'erec-cleanup))
   (t
    (erec-cleanup)
    (delete-hook 'kill-emacs-hook 'erec-cleanup))))

;; Commands

(defun erec-record ()
  "Start recording if `erec-recording-in-progress' is nil, else stop
the current recording. See `erec-start-recording' and
`erec-stop-recording'."
  (interactive)
  (cond
   (erec-recording-in-progress
    (funcall erec-stop-recording)
    (setq erec-recording-in-progress nil)
    (message "Stopped recording."))
   (t
    (setq erec-recording-in-progress t)
    (funcall erec-start-recording (erec-audio-file t))
    (message "Recording."))))

(defun erec-play ()
  "Play the last file we recorded."
  (interactive)
  (unless (erec-audio-file)
    (error "Can't play: Didn't record anything yet!"))
  (when erec-recording-in-progress
    (erec-record))
  (message "Playing recording.")
  (funcall erec-play-recording (erec-audio-file)))

(defun erec-save (filename)
  "Save the last file we recorded to a FILENAME."
  (interactive "FSave file to: ")
  (unless (erec-audio-file)
    (error "Can't save: Didn't record anything yet!"))
  (copy-file (erec-audio-file) filename))

;; Helper functions

(defun erec-audio-file (&optional createp)
  "Returns the name of a temporary file to be used to temporarily
store audio data. If CREATP is true, create it if there's none yet."
  (when (and (not erec-temp-file)
             createp)
    (setq erec-temp-file (make-temp-file "erec-audio-")))
  erec-temp-file)

(defun erec-cleanup ()
  "Clean up the temporary file we left lying around..."
  (when (erec-audio-file)
    (delete-file (erec-audio-file))))

;; Normal shell command backend ("external")

(defgroup erec-external nil
  "*Using external commands to record and play back audio samples."
  :prefix "erec-external-"
  :group 'erec)

(defcustom erec-external-record-command "sound-recorder -qk %s &>/dev/null"
  "*The command to run to record something. A %s in the string will be
replaced by the filename to record to, which should be overwritten if
it already exists."
  :group 'erec-external
  :type 'string)

(defcustom erec-external-play-command "play-sample %s &>/dev/null"
  "*The command to run to record something. A %s in the string will be
replaced by the filename to record to, which should be overwritten if
it already exists."
  :group 'erec-external
  :type 'string)

(defvar erec-external-process nil
  "External process, if there's one currently running.")

(defun erec-external-start (filename)
  "Start an external recording process by running
`erec-external-record-command'."
  (setq erec-external-process
        (start-process-shell-command "erec-record"
                                     nil
                                     (format erec-external-record-command
                                             filename))))
  
(defun erec-external-stop ()
  "Stop the currently running external process."
  (when erec-external-process
    (interrupt-process erec-external-process)
    (setq erec-external-process nil)))

(defun erec-external-play (filename)
  "Play FILENAME using `erec-external-play'."
  (start-process-shell-command "erec-play"
                               nil
                               (format erec-external-play-command
                                       filename)))

(provide 'erec)
;;; erec.el ends here
