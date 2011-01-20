;; fastnav.el -- Fast navigation and editing routines.
;;
;; Version 1.05
;;
;; Copyright (C) 2008, 2009, 2010  Zsolt Terek <zsolt@google.com>
;;
;; Compatibility: GNU Emacs 22, 23.
;;
;; Inspired by zap-to-char, this library defines different routines operating on
;; the next/previous N'th occurrence of a character.  When invoking one of these
;; commands, the user is interactively queried for a character while the
;; potential target positions are highlighted.
;;
;; For example, META-s (jump-to-char-forward) highlights the next occurrences of
;; each character and prompts for one.  Once the user picks a char, the point is
;; moved to that position.  Subsequent invocations of META-s before picking a
;; character increases N, that is, the second, third, etc. occurrences are
;; highlighted and targeted.
;;
;; The sprint-forward/backward commands apply iterative jumping until return/C-G
;; is hit, making it possible to reach any point of the text with just a few
;; keystrokes.
;;
;; To use it, simply put this file under ~/.emacs.d/, add (require 'fastnav) to
;; your emacs initialization file and define some key bindings, for example:
;;
;; (global-set-key "\M-z" 'zap-up-to-char-forward)
;; (global-set-key "\M-Z" 'zap-up-to-char-backward)
;; (global-set-key "\M-s" 'jump-to-char-forward)
;; (global-set-key "\M-S" 'jump-to-char-backward)
;; (global-set-key "\M-r" 'replace-char-forward)
;; (global-set-key "\M-R" 'replace-char-backward)
;; (global-set-key "\M-i" 'insert-at-char-forward)
;; (global-set-key "\M-I" 'insert-at-char-backward)
;; (global-set-key "\M-j" 'execute-at-char-forward)
;; (global-set-key "\M-J" 'execute-at-char-backward)
;; (global-set-key "\M-k" 'delete-char-forward)
;; (global-set-key "\M-K" 'delete-char-backward)
;; (global-set-key "\M-m" 'mark-to-char-forward)
;; (global-set-key "\M-M" 'mark-to-char-backward)
;;
;; (global-set-key "\M-p" 'sprint-forward)
;; (global-set-key "\M-P" 'sprint-backward)
;;
;; Changes:
;;   2010-02-05: Fix for org mode, all commands were broken.
;;               Fix for electric characters in certain modes.
;;   2010-02-11: Yet another minor fix for switching to next/previous char.
;;   2010-05-28: Added sprint commands.
;;   2011-01-19: Fixed removal of other overlays (like bookmarks for example).
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

(defun search-char-forward (arg char)
  "Moves to the arg-th occurrence of char forward (backward if N
is negative).  If there isn't room, go as far as possible (no
error)."
  (let ((old-case-fold-search case-fold-search))
    (setq case-fold-search nil)
    (if (< arg 0)
	(search-backward (char-to-string char) nil nil (- arg))
      (progn
	(forward-char 1)
	(search-forward (char-to-string char) nil nil arg)
	(backward-char 1)))
    (setq case-fold-search old-case-fold-search)))

(defun search-char-backward (arg char)
  "Moves to the arg-th occurrence of char backward (forward if N
is negative).  If there isn't room, go as far as possible (no
error)."
  (search-char-forward (- arg) char))  

(defun get-nth-chars (arg)
  "Computes and returns the positions of the ARG'th occurrence of
characters of the range 1 .. 255."
  (let ((seq '())
	(result '()))
    ;; Create character sequence to look for.
    (setq char 255)
    (while (>= char 1)
      (setq seq (cons char seq))
      (setq char (1- char)))
    ;; Find of nth occurrence of each character
    (let ((old-case-fold-search case-fold-search))
      (setq case-fold-search nil)
      (setq result
	    (mapcar (lambda (char)
		      (let ((old-point (point)))
			(save-excursion
			  (if (< arg 0)
			      (search-backward (char-to-string char) nil t (- arg))
			    (progn
			      (forward-char 1)
			      (search-forward (char-to-string char) nil t arg)
			      (backward-char 1)))
			  (if (= (point) old-point)
			      nil
			    (point)))))
		    seq))
      (setq case-fold-search old-case-fold-search)
      result)))

(defun highlight-read-char (text arg forwarder backwarder)
  "Highlights the ARG'th occurences of each character while
querying one using message TEXT. Negative ARG means backward
search of occurences."
  (if (not (minibufferp))
      (message text))
  (unwind-protect
      (let ((result nil)
	    (forwarders `(,forwarder forward-char next-line))
	    (backwarders `(,backwarder backward-char previous-line)))
	(while (not result)
	  (remove-overlays nil nil 'fastnav t)
	  (mapcar (lambda (p)
		    (if p
			(let ((ov (make-overlay p (1+ p))))
			  (overlay-put ov 'priority 100)
			  (overlay-put ov 'face lazy-highlight-face)
			  (overlay-put ov 'fastnav t)
			  ov)))
		  (get-nth-chars arg))
	  (let* ((event (read-event))
		 (char (event-basic-type event))
		 (delta 0)
		 (command (key-binding (vector event))))
	    (if (or
		 (and (numberp event) (< event 256))
		 (member command
			 ;; which commands have a keystroke
			 ;; that is valid for search
			 '(self-insert-command
			   org-self-insert-command
			   newline newline-and-indent)))
		(setq result (list arg event))
	      (progn
		(if (member command forwarders)
		    ;; increase argument
		    (setq delta +1)
		  (if (member command backwarders)
		      ;; decrease argument
		      (setq delta -1)
		    (keyboard-quit)))
		;; ignore arg = 0
		(setq arg (if (= (+ arg delta) 0)
			      (+ arg (* 2 delta))
			    (+ arg delta)))))))
	result)
    (remove-overlays nil nil 'fastnav t)))

;; For debugging.
;;(key-binding (vector (read-event)))
;;(event-basic-type (read-event))

(defun highlight-read-char-backward (text arg forwarder backwarder)
  "Highlights the backward ARG'th occurences of each character
while querying one using message TEXT."
  (let ((args (highlight-read-char text (- arg) forwarder backwarder)))
    (list (- (car args)) (cadr args))))

(defun jump-to-char-forward (arg)
  "Jump to the ARG'th occurence of a character that is queried
interactively while highlighting the possible positions."
  (interactive "p")
  (apply 'search-char-forward (highlight-read-char "Jump to char:" arg
						   'jump-to-char-forward
						   'jump-to-char-backward)))

(defun jump-to-char-backward (arg)
  "Jump backward to the ARG'th occurence of a character that is
queried interactively while highlighting the possible positions."
  (interactive "p")
  (apply 'search-char-backward
	 (highlight-read-char-backward "Jump to char backward:" arg
				       'jump-to-char-forward
				       'jump-to-char-backward)))

(defun mark-to-char-forward (arg)
  "Set mark before the ARG'th occurence of a character queried
interactively."
  (interactive "p")
  (let ((args (highlight-read-char "Copy to char: " arg
				   'mark-to-char-forward
				   'mark-to-char-backward)))
    (set-mark (point))
    (apply 'search-char-forward args)
    (exchange-point-and-mark)))

(defun mark-to-char-backward (arg)
  "Set mark backward after the ARG'th occurence of a character
queried interactively."
  (interactive "p")
  (let ((args (highlight-read-char-backward "Copy to char backward: " arg
					    'mark-to-char-forward
					    'mark-to-char-backward)))
    (set-mark (point))
    (apply 'search-char-backward args)
    (exchange-point-and-mark)))

(defun zap-up-to-char-forward (arg)
  "Kill text up to the ARG'th occurence of a character queried
interactively."
  (interactive "p")
  (let ((args (highlight-read-char "Zap up to char: " arg
				   'zap-up-to-char-forward
				   'zap-up-to-char-backward)))
    (delete-region (point)
		   (progn 
		     (apply 'search-char-forward args)
		     (point)))))

(defun zap-up-to-char-backward (arg)
  "Kill text backward to the ARG'th occurence of a character
queried interactively."
  (interactive "p")
  (let ((args (highlight-read-char-backward "Zap up to char backward: " arg
					    'zap-up-to-char-forward
					    'zap-up-to-char-backward)))
    (delete-region (point)
		   (progn 
		     (apply 'search-char-backward args)
		     (point)))))

(defun replace-char-forward (arg)
  "Interactively replaces the ARG'th occurence of a character."
  (interactive "p")
  (let ((args (highlight-read-char "Replace char: " arg
				   'replace-char-forward
				   'replace-char-backward)))
    (save-excursion
      (apply 'search-char-forward args)
      (let ((char (read-char (if (minibufferp) nil "With char: "))))
	(delete-char +1)
	(insert char)))))

(defun replace-char-backward (arg)
  "Interactively replaces the ARG'th backward occurence of a
character."
  (interactive "p")
  (let ((args (highlight-read-char-backward "Replace char backward: " arg
					    'replace-char-forward
					    'replace-char-backward)))
    (save-excursion
      (apply 'search-char-backward args)
      (let ((char (read-char (if (minibufferp) nil "With char: "))))
	(delete-char +1)
	(insert char)))))

(defun insert-at-char-forward (arg)
  "Queries for a character and a string that is insterted at
the ARG'th occurence of the character."
  (interactive "p")
  (let ((args (highlight-read-char "Execute forward before: " arg
				   'insert-at-char-forward
				   'insert-at-char-backward)))
    (save-excursion
      (apply 'search-char-forward args)
      (insert (read-string "String: ")))))

(defun insert-at-char-backward (arg)
  "Queries for a character and a string that is insterted at
the backward ARG'th occurence of the character."
  (interactive "p")
  (let ((args (highlight-read-char-backward "Execute backward before: " arg
					    'insert-at-char-forward
					    'insert-at-char-backward)))
    (save-excursion
      (apply 'search-char-backward args)
      (insert (read-string "String: ")))))

(defun execute-at-char-forward (arg)
  "Queries for a character and a key sequence that is executed at
the ARG'th occurence of the character."
  (interactive "p")
  (let ((args (highlight-read-char "Execute forward before: " arg
				   'execute-at-char-forward
				   'execute-at-char-backward)))
    (save-excursion
      (apply 'search-char-forward args)
      (execute-kbd-macro (read-key-sequence-vector
			  (if (minibufferp) nil "Key sequence: "))))))

(defun execute-at-char-backward (arg)
  "Queries for a character and a key sequence that is executed at
the backward ARG'th occurence of the character."
  (interactive "p")
  (let ((args (highlight-read-char-backward "Execute backward before: " arg
					    'execute-at-char-forward
					    'execute-at-char-backward)))
    (save-excursion
      (apply 'search-char-backward args)
      (execute-kbd-macro (read-key-sequence-vector
			  (if (minibufferp) nil "Key sequence: "))))))

(defun delete-char-forward (arg)
  "Deletes the ARG'th occurence of a character, which is queried
interactively while highlighting the possible positions."
  (interactive "p")
  (let ((args (highlight-read-char "Delete forward before: " arg
				   'delete-char-forward
				   'delete-char-backward)))
    (save-excursion
      (apply 'search-char-forward args)
      (delete-char +1))))

(defun delete-char-backward (arg)
  "Deletes the backward ARG'th occurence of a character, which is
queried interactively while highlighting the possible positions."
  (interactive "p")
  (let ((args (highlight-read-char-backward "Delete backward before: \n" arg
					    'delete-char-forward
					    'delete-char-backward)))
    (save-excursion
      (apply 'search-char-backward args)
      (delete-char +1))))

(defun sprint-forward (arg)
  "Performs a sequence of jumping forward to the next character
matching the keyboard event."
  (interactive "p")
  (let ((result t))
    (while result
      (if (setq result (highlight-read-char "Sprint:" arg
					    'sprint-forward
					    'sprint-backward))
	  (progn
	    (apply 'search-char-forward result)
	    (setq arg (if (> (car result) 0) 1 -1)))))))

(defun sprint-backward (arg)
  "Performs a sequence of jumping backward to the next character
matching the keyboard event."
  (interactive "p")
  (sprint-forward (- arg)))

(provide 'fastnav)
