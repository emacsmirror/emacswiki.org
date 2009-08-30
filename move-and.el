;;; move-and.el

;; Copyright (C) Aidan Schofield 2007

;; Maintainer: Aidan Schofield
;; Version 0.1
;; Keywords: delete, kill, copy, keys
;; Time-stamp: <2007-04-05 14:54:14 aidanschofield>

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;; move-and.el

;; Commentary. I just saw a blog where there was a complaint about the
;; complexity of key-bindings for killing some piece of text which was
;; contrasted with the simplicity of vi where one prepends a motion
;; keysequence with `d' in order to delete the text passed over by the
;; motion command. Independently I have been annoyed by the difficulty
;; of deleting text without putting it into the kill ring. The blog
;; suggested using `M-d' as a prefix command to various deleting
;; commands. Here I propose a better idea. Given some keysequence
;; `key' that defines a motion command, the keysequence `M-d key'
;; deletes without saving the region passed over; then the keysequence
;; `M-k key' kills the region passed over and finally the keysequence
;; `M-c key' copies the region passed over. This file also provides a
;; general way to combine a motion command with a region command to
;; obtain a command that applies the region command to the region
;; passed over by the motion command.

;; There is some difficulty to doing this consistently because emacs
;; is not consistent in its treatment of motion commands; some of them
;; have an optional numeric argument whilst some of them have a forced
;; numeric argument. Also the sequence `M-2 M-b' is two not one
;; keysequence. So to kill two words backward one must type `M-2kb'
;; rather than `M-k2b'. To set it up the other way would be more
;; complex though possible. What do others think should be done?

;; Code begins here

(eval-when-compile
  (require 'cl))

(defun move-and-execute-command (arg motion-key region-command)
  "executes REGION-COMMAND between point and where we move to.
ARG is given as argument to the key-binding of MOTION-KEY. If
MOTION-KEY is a self-inserting character then we search backward
when it is lowercase and forward when uppercase to produce our
motion."
  (interactive "P\nk\nC")
  (and (consp arg) ;; it seems that C-u provides the list (4) as argument
       (setq arg (car arg)))
  (funcall region-command
	   (point)
	   (progn
	     (case (key-binding motion-key)
	       ('self-insert-command
;; silly hack to squeeze in forward and backward search. I think I
;; prefer to use search-backward hence the choice for which way round
;; to bind them
		(if (string= motion-key (upcase motion-key))
		    (search-forward motion-key (point-max) t arg)
		  (search-backward motion-key (point-min) t arg)))
	       (t
		(funcall (key-binding motion-key) arg)))
	     (point))))

(defsubst move-and-delete (arg motion-key)
  "deletes text between point and where we move to."
  (interactive "P\nk")
  (move-and-execute-command arg motion-key 'delete-region))

(defsubst move-and-kill (arg motion-key)
  "kills text between point and where we move to."
  (interactive "P\nk")
  (move-and-execute-command arg motion-key 'kill-region))

(defsubst move-and-copy (arg motion-key)
  "copies text between point and where we move to."
  (interactive "P\nk")
  (move-and-execute-command arg motion-key 'copy-region-as-kill))

(defsubst move-and-copy-dont-move (arg motion-key)
  "copies text between point and where we indicate without moving."
  (interactive "P\nk")
  (save-excursion
    (move-and-execute-command arg motion-key 'copy-region-as-kill)))

(defsubst move-and-act (arg motion-key action-key)
  "applies keybinding of ACTION-KEY to text moved over by MOTION-KEY"
  (interactive "P\nk\nk")
  (move-and-execute-command arg motion-key (key-binding action-key)))



;; These are the keys I propose assigning to the commands here but
;; since they may conflict with choices others have made I have
;; commented them out here. Uncomment them here to set them up as I
;; have done or assign keys to these commands in your .emacs

;; (global-set-key "\M-d" 'move-and-delete)
;; (global-set-key "\M-k" 'move-and-kill)
;; (global-set-key "\M-c" 'move-and-copy)
;; (global-set-key "\M-r" 'move-and-act)

(provide 'move-and)

