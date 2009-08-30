;;; mu.el --- play on a MUSH or MUD within Emacs

;; Copyright (C) 2001, 2004  Alex Schroeder <alex@gnu.org>

;; Emacs Lisp Archive Entry
;; Filename: mu.el
;; Version: 1.0.1
;; Keywords: comm, games
;; Author: Alex Schroeder <alex@gnu.org>
;; Maintainer: Alex Schroeder <alex@gnu.org>
;; Description: Play in a MUSH or MUD within Emacs.
;; Compatibility: Emacs20

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary:

;; This is is a MUSH or MUD client.  Instead of using telnet or
;; standalone client to connect to your favorite MUSH or MUD, you can
;; use mu.el to play within Emacs.

;; I used to play using tinymud.el, but decided to rewrite it based on
;; comint-mode.  :)

;; Before playing, customize `mu-worlds'.  Then use `mu-open' to open a
;; connection to one of the worlds.  This will automaticall create a mu
;; connection buffer and a mu buffer.  You can type commands in either
;; buffer and send them to the host with RET.  The output will be in the
;; mu connection buffer.

;; If you load ansi-color.el, you should be able to get ANSI colors.

;;; Code:

(require 'comint)
(load "ansi-color" t)

(defgroup mu nil
  "A MUSH or MUD client."
  :group 'processes)

(defcustom mu-worlds nil
  "List of worlds you play in.
You need to define the worlds you play in before you can get
started.  In most worlds, you can start playing using a guest account.

Each element WORLD of the list has the following form:

\[NAME HOST PORT CHARACTER PASSWORD]

NAME identifies the connection, HOST and PORT specify the network
connection, CHARACTER and PASSWORD are used to connect automatically.

Note that this will be saved in your `custom-file' -- including your
passwords!  If you don't want that, specify nil as your password."
  :type '(repeat
	  (vector :tag "World"
		  (string :tag "Name")
		  (string :tag "Host")
		  (integer :tag "Port")
		  (string :tag "Char" :value "guest")
		  (string :tag "Pwd" :value "guest")))
  :group 'mu)

;; Accessing the fields

(defsubst mu-world-name (world)
  "Return the name for WORLD as a string."
  (concat (aref world 3) "@" (aref world 0)))

(defsubst mu-world-network (world)
  "Return the network details for WORLD as a cons cell (HOST . PORT)."
  (cons (aref world 1) (aref world 2)))

(defsubst mu-world-character (world)
  "Return the character for WORLD as a string."
  (aref world 3))

(defsubst mu-world-password (world)
  "Return the password for WORLD as a string."
  (aref world 4))

;;; Modes

(defvar mu-input-mode-map
  (let ((map (make-sparse-keymap)))
    (if (functionp 'set-keymap-parent)
	(set-keymap-parent map text-mode-map); Emacs
      (set-keymap-parents map (list text-mode-map))); XEmacs
    (if (functionp 'set-keymap-name)
	(set-keymap-name map 'mu-input-mode-map)); XEmacs
    (define-key map (kbd "<RET>") 'mu-send)
    map)
  "Mode map used for `mu-input-mode'.
Based on `text-mode-map'.")

(defvar mu-connection nil
  "Local variable for the connection.")

(defun mu-input-mode (&optional conn)
  "Major mode to type commands for the mu connection.
This is called a mu input buffer.

Use \\[mu-open] to open a connection.
Use \\[mu-choose-connection] to choose a connection.
Use \\[mu-send] to send commands to the current connection.

This function will run `mu-input-mode-hook' at the end.

\\{mu-input-mode-map}"
  (interactive)
  (setq conn (or conn mu-connection (mu-get-connection)))
  (kill-all-local-variables)
  (setq major-mode 'mu-input-mode)
  (setq mode-name "MU* Input")
  (use-local-map mu-input-mode-map)
  ;; Make each buffer in mu-input-mode remember the current connection.
  (set (make-local-variable 'mu-connection) conn)
  ;; Run hook
  (run-hooks 'mu-input-mode-hook))

(defvar mu-connection-mode-map
  (let ((map (make-sparse-keymap)))
    (if (functionp 'set-keymap-parent)
	(set-keymap-parent map comint-mode-map); Emacs
      (set-keymap-parents map (list comint-mode-map))); XEmacs
    (if (functionp 'set-keymap-name)
	(set-keymap-name map 'mu-connection-mode-map)); XEmacs
    ;; (define-key map (kbd "C-j") 'mu-accumulate-and-indent)
    map)
  "Mode map used for `mu-connection-mode'.
Based on `comint-mode-map'.")

(defvar mu-name nil
  "Local variable for the connection name.")

(defun mu-connection-mode (name)
  "Major mode for a mu connection.

Use \\[comint-send-input] to send commands.
Use \\[mu-open] to open other connections.
Use \\[mu-input-buffer] to create a mu input buffer.

This function will run `mu-connection-mode-hook' at the end.

\\{mu-connection-mode-map}"
  (comint-mode)
  (setq major-mode 'mu-connection-mode)
  (setq mode-name "MU* Conn")
  (use-local-map mu-connection-mode-map)
  (set (make-local-variable 'mu-name) name)
  (setq fill-column 80)
  (add-to-list 'comint-output-filter-functions 'mu-fill)
  ;; User stuff.
  (run-hooks 'mu-connection-mode-hook))

(put 'mu-connection-mode 'mode-class 'special)

;;; Opening connections

(defvar mu-world-history nil
  "History for `mu-get-world'.")

(defun mu-get-world ()
  "Let the user choose a world from `mu-worlds'.  
The return value is a cons cell, the car is the name of the connection,
the cdr holds the connection defails from `mu-worlds'."
  (let ((world-completions
	 (mapcar (lambda (w)
		   (cons (mu-world-name w) w))
		 mu-worlds)))
    (if world-completions
	(cdr (assoc (completing-read "World: " world-completions
				     nil t nil mu-world-history)
		    world-completions))
      (customize-option 'mu-worlds)
      nil)))

(defun mu-open (world)
  "Create a new mu connection."
  (interactive (list (mu-get-world)))
  (when world
    (message "Opening connection...")
    (let ((buf (make-comint (mu-world-name world) (mu-world-network world))))
      (pop-to-buffer buf)
      (when (mu-world-password world)
	(mu-login world))
      (mu-connection-mode (mu-world-name world))
      (mu-input-buffer buf)
      (message "Opening connection...done"))))

(defun mu-reconnect (world)
  "Renew the connection in a mu output buffer."
  (interactive (list (mu-get-world)))
  (open-network-stream (mu-world-name world) (current-buffer)
		       (car (mu-world-network world))
		       (cdr (mu-world-network world))))

(defun mu-login (world)
  "Login for WORLD in the current buffer.
This just sends the login string and hopes for the best."
  (process-send-string
   (current-buffer)
   (format "\nconnect %s %s\n"
	   (mu-world-character world)
	   (mu-world-password world))))

;; Creating mu mode buffers

(defun mu-input-buffer (buf)
  "Create a mu input buffer for connection BUF.
The current buffer must be a mu connection."
  (interactive (list (mu-get-connection)))
  (set-buffer buf)
  (pop-to-buffer (get-buffer-create
		     (concat "*Input for " mu-name "*")))
  (mu-input-mode buf))

(defvar mu-connection-history nil
  "History for `mu-get-connection'.")

(defun mu-get-connection ()
  "Let the user choose a connection from all buffers.
Only buffers with `mu-name' set are eligible.
Note that `default-value' of `mu-name' must be nil for this to work."
  (let ((buffers (buffer-list))
	buf conns)
    (while buffers
      (setq buf (car buffers)
	    buffers (cdr buffers))
      (set-buffer buf)
      (when mu-name
	(setq conns (cons (cons mu-name buf) conns))))
    (cdr (assoc (completing-read "Connection: " conns 
				 nil t nil mu-connection-history)
		conns))))

;; Sending stuff

(defun mu-send ()
  "Send current line to the current connection.
The current connection is stored in `mu-connection'."
  (interactive)
  (unless mu-connection
    (error "No connection"))
  (let ((pos (point)))
    (save-excursion
      (beginning-of-line)
      (process-send-string
       mu-connection
       (concat (buffer-substring-no-properties (point) pos) "\n"))))
  (when (looking-at "\\'")
    (newline)))

;; Receiving stuff

(defun mu-fill (str)
  "Fill text received from the host.
This fills each line between `comint-last-output-start' and the buffer's
`process-mark'."
  (save-excursion
    (let ((pos (point-marker)))
      (goto-char comint-last-output-start)
      (while (< (point) pos)
	(let (start)
	  (beginning-of-line)
	  (setq start (point))
	  (forward-line)
	  (fill-region start (point) nil t))))))

(provide 'mu)

;;; mu.el ends here
