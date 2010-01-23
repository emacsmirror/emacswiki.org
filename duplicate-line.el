;;;
; From: nickson@cs.uq.oz.au (Ray Nickson)
; Subject: duplicating lines
; Date: 29 Jun 1994 00:58:56 GMT
; Organization: Computer Science Dept, University of Queensland
; 
; Here's a couple of commands I find indispensible; they might as well
; see the light of day now, while I'm thinking of them.  I bind them
; thus:
; 
; (global-set-key "\M-p" 'duplicate-previous-line)
; (global-set-key "\M-n" 'duplicate-following-line)
; 
; and they behave in ordinary text buffers something like the same keys
; do in buffers with history, duplicating lines from nearby in the
; buffer.
; 
; I included my `point-at' macro rather than expanding it out by hand.
; 
; Copyright notice included even though I doubt it's needed on something
; so small.

;; Copyright (C) 1994 by Ray Nickson (nickson@cs.uq.oz.au)

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

(defmacro point-at (&rest forms)
  "Evaluate list of forms, and return the value of point after evaluation."
  (` (progn (,@ forms)
	    (,@ '((point))))))

(defun duplicate-line (n dir)
  "Copy the Nth following line to point.
If the last command was a duplication, replace the current line by the next
line in direction DIR."
  (if (eq (car-safe last-command) 'duplicate-line)
      (progn
        (delete-region (point-at (beginning-of-line nil))
                       (point-at (end-of-line nil)))
        (setq n (+ dir (cdr last-command))))
    (kill-region (point-at (beginning-of-line nil))
                 (point-at (end-of-line nil))))
  (if (= n 0)
      (insert (current-kill 0))
    (insert (save-excursion
              (beginning-of-line (1+ n))
              (buffer-substring (point)
                                (point-at (end-of-line nil)))))
    (setq this-command (cons 'duplicate-line n))))

(defun duplicate-previous-line (n)
  "Copy the Nth previous line to point.
If repeated, replace by the line preceding the one that was copied last time.
This command can be interleaved with \\[duplicate-following-line]."
  (interactive "p")
  (duplicate-line (- n) -1))

(defun duplicate-following-line (n)
  "Copy the Nth following line to point.
If repeated, replace by the line following the one that was copied last time.
This command can be interleaved with \\[duplicate-previous-line]."
  (interactive "p")
  (duplicate-line n 1))
