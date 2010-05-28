;;; repdet.el --- detect repetition in user activity

;; Copyright (C) 2002  ZwaX

;; Author: ZwaX <?>
;; Maintainer: ZwaX <?>
;; Version: 1.0.2
;; Keywords: lisp
;; URL: http://www.emacswiki.org/cgi-bin/wiki.pl?RepetitionDetectionPackage

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.

;; This is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This can be used to define keyboard macros after the event, and to
;; suggest good candidates for saving as keyboard macros.

(require 'loops)

(defgroup repdet nil
  "Detect repetitions in user input.")

(defcustom repdet-min-repetitions 3
  "the minimum number of times a sequence must occur before it is detected"
  :type 'integer
  :group 'repdet)

(defcustom repdet-min-length 15
  "the minimum length of a detected sequence"
  :type 'integer
  :group 'repdet)

(defcustom repdet-max-length 720
  "the maximum length of a detected sequence"
  :type 'integer
  :group 'repdet)

(defcustom repdet-buffer-size 10000
  "the number of input events to keep"
  :type 'integer
  :group 'repdet)

(defcustom repdet-buffer-name "*Detected Patterns*"
  "if non-nil, the name of a buffer to which to log detected patterns"
  :type 'string
  :group 'repdet)

(defcustom repdet-ignore-adjacent-identical-patterns t
  "if non-nil, if a detected pattern is identical to the previously detected pattern, ignore it"
  :type 'boolean
  :group 'repdet)

(defcustom repdet-show-patterns-in-message-area nil
  "if non-nil, show each pattern in the message area as it is detected"
  :type 'boolean
  :group 'repdet)

(defcustom repdet-trim-buffer-delay 1000
  "number of keystrokes to wait before trimming repdet-input-event-list buffer"
  :type 'integer
  :group 'repdet)

(defvar repdet-debug nil
  "set to t for debug windows")

(defvar repdet-most-recent-sequence nil
  "a list containing the most recently detected sequence of repeating keystrokes")

(defvar repdet-input-event-list nil
  "list of recent events")

(setq repdet-steps-until-trim repdet-trim-buffer-delay)

(defun turn-on-repdet()
  "Switches repetition detection on"
  (interactive)
  (add-hook 'pre-command-hook 'repdet-pre-command-get-command t))

(defun turn-off-repdet()
  "Switches repetition detection off"
  (interactive)
  (remove-hook 'pre-command-hook 'repdet-pre-command-get-command))

;; this sets the buffer to be full of junk, so you get an instant
;; indication of how slow things will be once the buffer is full
(let ((i repdet-buffer-size))
  (while (> i 0)
    (setq repdet-input-event-list (cons (random 256) repdet-input-event-list)
	  i (1- i))))

(defun repdet-pre-command-get-command ()
  (let ((keys (this-command-keys))
	(mode major-mode)
	(cmd this-command))

    (if (not (or (eq cmd 'repdet-use-as-macro)
		 (eq cmd 'call-last-kbd-macro)
		 ;; 'keys' seems to be empty after running a keyboard macro?
		 (equal keys "")))
	(progn
	  (setq repdet-input-event-list (cons keys repdet-input-event-list)
		repdet-steps-until-trim (1- repdet-steps-until-trim))
	  (if (= 0 repdet-steps-until-trim)
	      (progn
		(setq repdet-steps-until-trim repdet-trim-buffer-delay)
		(if (> (length repdet-input-event-list) repdet-buffer-size)
		    (setcdr (nthcdr (1- repdet-buffer-size) repdet-input-event-list) nil))))))

    ;; (repdet-show-list-for-debugging-purposes)

    (if (not executing-kbd-macro)
	(progn
	  (if repdet-debug
	      (save-excursion
		(let (deactivate-mark)
		  (set-buffer (get-buffer-create "*repdet*"))
		  (goto-char (point-min))
		  (insert (format "search: keys '%s' cmd '%s'\n" keys cmd)))))
	  (let (ret time)
	    (setq time (time-eval '(setq ret (loop-find-longest repdet-input-event-list
								repdet-min-repetitions
								repdet-min-length
								repdet-max-length))))
	    (if ret
		(let ((previous-sequence repdet-most-recent-sequence))
		  (setq repdet-most-recent-sequence (nreverse ret))
		  (if (or (not repdet-ignore-adjacent-identical-patterns)
			  (not (equal previous-sequence repdet-most-recent-sequence)))
		      (progn
			(if repdet-show-patterns-in-message-area
			    (message "%s found sequence: %s" time repdet-most-recent-sequence))
			(if repdet-buffer-name
			    (save-excursion
			      (let (deactivate-mark)
				(set-buffer (get-buffer-create repdet-buffer-name))
				(goto-char (point-min))
				(insert (format "%s : %s\n" (current-time-string)
						repdet-most-recent-sequence))))))))))))))

(defun repdet-show-list-for-debugging-purposes ()
  (save-excursion
    (set-buffer "*repdet*")
    (delete-region (point-min) (point-max))
    (insert (format "%s" repdet-input-event-list))))

(defun time-eval (expr)
  (let (before after)
    (setq before (current-time))
    (eval expr)
    (setq after (current-time))
    (format "%.3f" (+ (/ (- (caddr after) (caddr before)) 1000000.0)
		      (- (cadr after)  (cadr before))
		      (* (- (car after)   (car before))   65536)))))

(defun repdet-use-as-macro ()
  (interactive)
  (let ((mac "")
	(seq repdet-most-recent-sequence)
	next)
    (while seq
      (setq next (car seq)
	    seq (cdr seq))
      (if (stringp mac)
	  (if (stringp next)
	      (setq mac (concat mac next))
	    (if (vectorp next)
		(setq mac (vconcat mac next))
	      ;; debugging - shouldn't happen?
	      (message "what is type %s doing in macro?" (type-of next))
	      (read-char)))
	(if (vectorp mac)
	    (setq mac (vconcat mac next))
	  ;; debugging - shouldn't happen?
	  (message "what is type %s doing in mac?" (type-of mac))
	  (read-char))))

    (setq last-kbd-macro mac)
    ;; (insert (format "\n\nmacro: '%s'\n" mac))
    ))

(turn-on-repdet)
(global-set-key [f10] '(lambda () (interactive) (message "%d" (length repdet-input-event-list))))
(global-set-key [f11] 'repdet-use-as-macro)
(global-set-key [f12] 'call-last-kbd-macro)
(global-set-key [f5] 'turn-on-repdet)
(global-set-key [f6] 'turn-off-repdet)

(provide 'repdet)
;;; `repdet.el' ends here
