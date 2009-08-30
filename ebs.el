;;; ebs.el --- easy buffer switch (ebs)

;; Copyright (C) 2004 Yan Tang

;; Author: Yan Tang <tangyan@pku.org.cn>
;; Maintainer: Yan Tang <tangyan@pku.org.cn>
;; Created: 9 Sep 2004
;; Version: $Id: ebs.el,v 1.2 2004/09/11 08:46:14 tangyan Exp $
;; Keywords: files convenience

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:

;; Ebs package offers two ways for switching buffers in GNU
;; Emacs/XEmacs, number indexed switch and cycle switch.
;;
;; In number indexed switch, buffer names in current buffer list will
;; be displayed in the echo area like this:
;;
;; [1]buffa|[2]buffb|[3]buffc|[4]buffd|[5]buffe
;;
;; You can switch to whichever buffer you like by simply pressing the
;; number before its name.  Shortcut keys are provided for 2-bit numbers
;; ([10], [11], ...).  The default key mapping is as the following:
;;
;; [10] -> 0
;; [11] -> q
;; [12] -> w
;; [13] -> e
;; [19] -> o
;; [20] -> p
;; [21] -> a
;; ...
;; [39] -> .
;; [40] -> /
;;
;; Easy to remember and press?!  What about the buffer named
;; [41]buffodd?  Sorry, remember that you can ignore annoying buffers
;; by setting them in `ebs-exclude-buffer-regexps' so I don't think
;; you need to simultaneously edit more than 40 buffers.  If you
;; really need to the ability, crack this package yourself or cite
;; more than three real examples to persuade me.
;;
;; Then, what's cycle switch and what the hell does it do?
;;
;; Cycle switch is much alike the classic alt-tab in windows.  That
;; is, when you press the default key binding C-tab more than once,
;; you will cycle the buffer list until arriving at your interested
;; buffer, like this:
;;
;; Press C-tab once:
;;
;; [1]buffa|[2]buffb|[3]buffc|[4]buffd|[5]buffe
;;          ~~~~~~~~ highlighted 
;;
;; Press C-tab twice:
;;
;; [1]buffa|[2]buffb|[3]buffc|[4]buffd|[5]buffe
;;                   ~~~~~~~~
;; Press C-tab the third time:
;;
;; [1]buffa|[2]buffb|[3]buffc|[4]buffd|[5]buffe
;;                            ~~~~~~~~
;;
;; Cycle switch is essential because when you want to switch to
;; buffers near the head of the buffer list, it is much more _direct_
;; and _convenient_ than number indexed switch.
;;
;; NOTES: When you press C-tab more than once (consecutively), ebs
;; will enter cycle switch mode.  That means number indexed switch are
;; temporarily disabled until you select your interested buffer and
;; do something in it.
;;
;; I design the behavior of ebs like this is because I think if you
;; want to use number index switch, after you first press C-tab you
;; _SHOULD_ press the buffer number.  If not, it indicates that you
;; intend to cycle buffer. Am I right?

;;; Installation:

;; Put this file on your emacs load path and add the following
;; three lines into your ~/.emacs startup file.
;; ---------------------------------------------------
;; (require 'ebs)
;; (ebs-initialize)
;; (global-set-key [(control tab)] 'ebs-switch-buffer)
;; ---------------------------------------------------

;;; Todo:

;; 1. Code structure & coding convention, enough comments, English
;; grammar&style.
;; 2. History info, recent used buffer list.
;; 3. Customization.

;;; Acknowledgment:

;; Crazycool@smth: 
;;    the original idea of number index switch (probable).
;; Ann77@smth:
;;    the idea of waiting for a key after pressing control-tab.
;; David Ponce <david@dponce.com>:
;;    swbuff.el helps me a lot.

;;; Code:

(defgroup ebs nil
  "Easy Buffer Switch: Buffer switching with number index and recent history info."
  :group 'convenience)

(defcustom ebs-exclude-buffer-regexps '("^ "
					"^\\*Messages\\*"
					"^\\*Buffer List\\*"
					"^\\*Completions\\*")
  "*List of regular expressions for annoying buffers.
The default setting ignores buffers whose name begin with a blank character."
  :group 'ebs
  :type '(repeat regexp))

(defcustom ebs-delay-time 1
  "*Buffer list display time."
  :group 'ebs
  :type 'integer)

;; Internal buffer name list.  It is not an essential variable in this
;; package, but holding this variable is much better for adding new
;; features, such as `history info'
(defvar ebs-buffer-list nil)

;; Cycle buffer temporary list.
(defvar ebs-cycle-buffer-string nil)

;; Highlight buffer _name_ range in `ebs-cycle-buffer-string'.
(defvar ebs-highlight-buffer-start 0)
(defvar ebs-highlight-buffer-end 0)

;; Buffer number counter.
(defvar ebs-buffer-counter 0)

;; Buffer switch state flag, `t' indicates number index switch and
;; `nil' indicates cycle switch.
(defvar ebs-switch-state t)

(defun ebs-include-buffer-p (name)
  "Return non-nil if buffer NAME can be included.
That is if NAME matches none of the `ebs-exclude-buffer-regexps'."
  (let ((rl ebs-exclude-buffer-regexps))
    (while (and rl (not (string-match (car rl) name)))
      (setq rl (cdr rl)))
    (null rl)))

(defun ebs-calc-buffer-list ()
  (setq ebs-buffer-list nil)
  (let ((bl (buffer-list)))
    (while bl
      ;; There is another elegent way to do this, see `swbuff.el'.
      ;; What I have done here is checking each element manually in a loop.
      (if (ebs-include-buffer-p (buffer-name (car bl)))
	  (setq ebs-buffer-list
		(append ebs-buffer-list (list (buffer-name (car bl))))))
      (setq bl (cdr bl)))))

(defun ebs-mapconcat-function (buffer)
  (setq ebs-buffer-counter (1+ ebs-buffer-counter))
  (let ((bf buffer))
    (concat "[" (format "%d" ebs-buffer-counter) "]" bf)))

(defun ebs-show-buffer-list ()
  "Display the highlighted buffer _string_ in echo area.  NOTES: The
highlighted buffer list displayed in echo area is NOT a real list. It
is a string.  This function is only responsible for displaying the
initial highlight buffer string and the latter cycling effect is done
in `ebs-cycle-switch'."
  (let ((start 1)
	end)
    (setq ebs-buffer-counter 0)
    (setq ebs-cycle-buffer-string (mapconcat 'ebs-mapconcat-function
					     ebs-buffer-list
					     "|"))
    (cond ((= (length ebs-buffer-list) 1)
	   (setq start 0
		 end (length ebs-cycle-buffer-string)))
	  ((= (length ebs-buffer-list) 2)
	   (setq start (string-match "\\[" ebs-cycle-buffer-string start)
		 end (length ebs-cycle-buffer-string)))
	  (t
	   (setq start (string-match "\\[" ebs-cycle-buffer-string start)
		 end (string-match "|" ebs-cycle-buffer-string start))))
    (setq ebs-highlight-buffer-start
	  (1+ (string-match "\\]" ebs-cycle-buffer-string start))
	  ebs-highlight-buffer-end end)
    ;; Display the _inital_ highlight buffer list.
    (add-text-properties start end '(face highlight)
			 ebs-cycle-buffer-string)
    (message ebs-cycle-buffer-string)))

(defun ebs-switch-buffer ()
  (interactive)
  (if ebs-switch-state
      (ebs-index-switch)
    (ebs-cycle-switch)))

(defun ebs-index-switch ()
  "Number index buffer switch."
  (let (key-stroke keystroke-value keystroke-hash-value start end)
    ;; Caclculate internal buffer list.
    (ebs-calc-buffer-list)
    (ebs-show-buffer-list)
    (cond ((= (length ebs-buffer-list) 1) ;; if there is only one buffer, do nothing.
	   (run-with-idle-timer ebs-delay-time nil (lambda() (message nil))))
	  ((= (length ebs-buffer-list) 2) ;; if there are only two buffers, directly switch buffer.
	   (progn 
	     (switch-to-buffer
	      (substring ebs-cycle-buffer-string
			 ebs-highlight-buffer-start
			 ebs-highlight-buffer-end))
	     (run-with-idle-timer ebs-delay-time nil (lambda() (message nil)))))
	  (t
	   (progn ;; buffer number >= 3
	     (switch-to-buffer
	      (substring ebs-cycle-buffer-string
			 ebs-highlight-buffer-start
			 ebs-highlight-buffer-end))
	     (setq keystroke (read-key-sequence-vector nil)
		   keystroke-value (aref keystroke 0))
	     (if (numberp keystroke-value)
		 (progn
		   (setq keystroke-hash-value
			 (gethash keystroke-value ebs-keyboard-hashtb))
		   (if (and keystroke-hash-value
			    (<= keystroke-hash-value (length ebs-buffer-list)))
		       (switch-to-buffer
			(nth (- keystroke-hash-value 1) ebs-buffer-list))))
	       ;; If user presses "C-tab" consequently, we will enter cycle mode.
	       (if (equal (key-binding keystroke) 'ebs-switch-buffer)
		   (progn
		     (setq ebs-switch-state nil)
		     (ebs-cycle-switch)))))))
    ))

(defun ebs-cycle-switch ()
  "Cycle buffer switch.  NOTES: Buffer list can NEVER change in this
mode, so we just use the fixed buffer string."
  (let ((start (string-match "\\[" ebs-cycle-buffer-string
			     ebs-highlight-buffer-end))
	end)
    (unless start
      (setq start 0))
    (setq end (string-match "|" ebs-cycle-buffer-string start))
    (unless end
      (setq end (length ebs-cycle-buffer-string)))
    (remove-text-properties 0 (length ebs-cycle-buffer-string)
			    '(face highlight)
 			    ebs-cycle-buffer-string)
    ;; Highlight forward one step.
    (add-text-properties start end '(face highlight)
 			 ebs-cycle-buffer-string)
    (setq ebs-highlight-buffer-start
	  (1+ (string-match "\\]" ebs-cycle-buffer-string start))
	  ebs-highlight-buffer-end end)    
    (message ebs-cycle-buffer-string)
    (switch-to-buffer (substring ebs-cycle-buffer-string
				 ebs-highlight-buffer-start
				 ebs-highlight-buffer-end))  
    (run-with-idle-timer ebs-delay-time nil (lambda() (message nil)))
    (add-hook 'pre-command-hook 'ebs-pre-command-hook)))

(defun ebs-pre-command-hook ()
  (unless (memq this-command '(ebs-switch-buffer))
    (progn ;; Go back to number index mode and cancel pre-command-hook.
      (setq ebs-switch-state t)
      (remove-hook 'pre-command-hook 'ebs-pre-command-hook))))

(defvar ebs-keyboard-hashtb (make-hash-table))

(defun ebs-initialize ()
  ;; init keyboard hash table
  ;; 1 -> 1, 2 -> 2, 9 -> 9, 0 -> 10,
  ;; q -> 11, w -> 12, p -> 20,
  ;; a -> 21, s -> 22, ; -> 30,
  ;; z -> 31, x -> 32, / -> 40.
  ;; You can change the default hash table as you like, ;-)
  ;; ===========================================================
  ;; ASCII Table:
  ;; 1-49, 2-50, 3-51, 4-52, 5-53, 6-54, 7-55, 8-56, 9-57, 0-48,
  ;; a-97, ... , z-122,
  ;; ;-59, ,-44, .-46, /-47.
  (puthash 49 1 ebs-keyboard-hashtb)
  (puthash 50 2 ebs-keyboard-hashtb)
  (puthash 51 3 ebs-keyboard-hashtb)
  (puthash 52 4 ebs-keyboard-hashtb) 
  (puthash 53 5 ebs-keyboard-hashtb)
  (puthash 54 6 ebs-keyboard-hashtb)
  (puthash 55 7 ebs-keyboard-hashtb)
  (puthash 56 8 ebs-keyboard-hashtb)
  (puthash 57 9 ebs-keyboard-hashtb)
  (puthash 49 1 ebs-keyboard-hashtb)
  (puthash 48 0 ebs-keyboard-hashtb)
  (puthash 113 11 ebs-keyboard-hashtb)
  (puthash 119 12 ebs-keyboard-hashtb)
  (puthash 101 13 ebs-keyboard-hashtb)
  (puthash 114 14 ebs-keyboard-hashtb)
  (puthash 116 15 ebs-keyboard-hashtb)
  (puthash 121 16 ebs-keyboard-hashtb)
  (puthash 117 17 ebs-keyboard-hashtb)
  (puthash 105 18 ebs-keyboard-hashtb)
  (puthash 111 19 ebs-keyboard-hashtb)
  (puthash 112 20 ebs-keyboard-hashtb)
  (puthash 97 21 ebs-keyboard-hashtb)
  (puthash 115 22 ebs-keyboard-hashtb)
  (puthash 100 23 ebs-keyboard-hashtb)
  (puthash 102 24 ebs-keyboard-hashtb)
  (puthash 103 25 ebs-keyboard-hashtb)
  (puthash 104 26 ebs-keyboard-hashtb)
  (puthash 106 27 ebs-keyboard-hashtb)
  (puthash 107 28 ebs-keyboard-hashtb)
  (puthash 108 29 ebs-keyboard-hashtb)
  (puthash 59 30 ebs-keyboard-hashtb)
  (puthash 122 31 ebs-keyboard-hashtb)
  (puthash 120 32 ebs-keyboard-hashtb)
  (puthash 99 33 ebs-keyboard-hashtb)
  (puthash 118 34 ebs-keyboard-hashtb)
  (puthash 98 35 ebs-keyboard-hashtb)
  (puthash 110 36 ebs-keyboard-hashtb)
  (puthash 109 37 ebs-keyboard-hashtb)
  (puthash 44 38 ebs-keyboard-hashtb)
  (puthash 46 39 ebs-keyboard-hashtb)
  (puthash 47 40 ebs-keyboard-hashtb))

(provide 'ebs)

;;; ebs.el ends here
