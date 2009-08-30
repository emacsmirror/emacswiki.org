;;; rcirc-sound.el --- CTCP SOUND support for rcirc 
;; Copyright (C) 2006  Free Software Foundation, Inc.

;; Author: Daniel Brockman <daniel@brockman.se>
;; Created: 2006-09-17
;; Updated: 2006-09-17

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(defgroup rcirc-sound nil
  "Play sounds through IRC using CTCP SOUND."
  :group 'rcirc)

(defcustom rcirc-enable-sound t
  "Whether rcirc should play sounds on CTCP SOUND requests."
  :type 'boolean
  :group 'rcirc-sound)

(defcustom rcirc-sound-directory "~/.rcirc-sound-files"
  "Directory of sound files to use with CTCP SOUND requests."
  :type 'directory
  :group 'rcirc-sound)

(defcustom rcirc-sound-file-name-regexp "[a-zA-Z0-9_.-]+"
  "Regexp matching names of sound files in `rcirc-sound-directory'."
  :type 'regexp
  :group 'rcirc-sound)

(defcustom rcirc-play-sound-function 'play-sound-file
  "Function to use for playing sound files."
  :type 'function
  :group 'rcirc-sound)

(defun rcirc-play-sound (file-name)
  (when rcirc-enable-sound
    (when (rcirc-sound-file-available-p file-name)
      (funcall rcirc-play-sound-function
               (rcirc-expand-sound-file-name file-name)))))

(defun rcirc-available-sound-files ()
  (directory-files rcirc-sound-directory nil "^[^.]"))

(defun rcirc-sound-file-available-p (file-name)
  (file-readable-p (rcirc-expand-sound-file-name file-name)))

(defun rcirc-expand-sound-file-name (file-name)
  (expand-file-name file-name rcirc-sound-directory))

(defun rcirc-handler-ctcp-SOUND (process target sender text)
  (cond
   ((string= text "")
    (rcirc-send-message
     process sender
     (concat "Available sound files: "
             (mapconcat 'identity (rcirc-available-sound-files) ", "))
     'notice))
   ((string-match rcirc-sound-file-name-regexp text)
    (let ((file-name (match-string 0 text)))
      (rcirc-play-sound file-name)))))

(defun rcirc-toggle-sound ()
  (interactive)
  (setq rcirc-enable-sound (not rcirc-enable-sound))
  (message "Sound is now %s."
           (if rcirc-enable-sound "enabled" "disabled")))

(define-key rcirc-mode-map (kbd "C-c C-M-s") 'rcirc-toggle-sound)

(defun-rcirc-command sound (args)
  "Send a CTCP SOUND request with ARGS.
ARGS looks like \"SOUND-FILE-NAME [COMMENT]\"."
  (rcirc-send-string process
                     (concat "PRIVMSG " target " "
                             ":\C-a"
                             "SOUND" (unless (string= args "") " ") args
                             "\C-a"))
  (when (string-match rcirc-sound-file-name-regexp args)
    (let ((file-name (match-string 0 args)))
      (rcirc-play-sound file-name))))

(provide 'rcirc-sound)
;;; rcirc-sound ends here.
