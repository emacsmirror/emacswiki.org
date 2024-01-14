;;; event-apply+.el --- Extensions to `event-apply-modifier'
;; Copyright (C) 2024 Brian Kavanagh

;; Author: Brian Kavanagh (concat "Brians.Emacs" "@" "gmail.com")
;; Maintainer: Brian Kavanagh (concat "Brians.Emacs" "@" "gmail.com")
;; Created: 27 June, 2020
;; Version: 2024.01.15
;; Package-Requires: ((emacs "25.1"))
;; Keywords: convenience
;; Homepage: https://github.com/BriansEmacs/event-apply-plus.el

;; -------------------------------------------------------------------
;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this programe.  If not, see
;; <https://www.gnu.org/licenses/>.

;; -------------------------------------------------------------------
;;; Commentary:
;;
;; This file defines a set of extensions to the
;; (`event-apply-modifier') command to allow for more than a single
;; modifier to be applied.  This will allow you to enter more complex
;; key combinations in environments without the ability to enter them
;; directly via the input device.
;;
;; It offers 15 functions that offer functional versions of all of the
;; 'two key' modifiers:
;;
;;  ?\A-\C-
;;  ?\A-\H-   ?\C-\H
;;  ?\A-\M-   ?\C-\M   ?\H-\M
;;  ?\A-\S-   ?\C-\S   ?\H-\S   ?\M-\S
;;  ?\A-\s-   ?\C-\s   ?\H-\s   ?\M-\s   ?\S-\s
;;
;; Plus an interactive function: `event-apply+-read-modifiers' which
;; will allow the user to enter combinations of all six modifiers
;; interactively.

;; -------------------------------------------------------------------
;;; Installation:
;;
;; Add the following to your `.emacs' file:
;;
;;  (require 'event-apply+)
;;
;;  ; Then something like...
;;  (define-key function-key-map (kbd "C-x @ x")
;;                               'event-apply+-read-modifiers)
;;
;;  (define-key function-key-map (kbd "C-x @ C-a")
;;                               'event-apply+-alt-control-modifier)
;;  (define-key function-key-map (kbd "C-x @ C-m")
;;                               'event-apply+-control-meta-modifier)
;;  (define-key function-key-map (kbd "C-x @ C-h")
;;                               'event-apply+-control-hyper-modifier)
;;  (define-key function-key-map (kbd "C-x @ C-s")
;;                               'event-apply+-control-super-modifier)
;;
;;  (define-key function-key-map (kbd "C-x @ A")
;;                               'event-apply+-alt-shift-modifier)
;;  (define-key function-key-map (kbd "C-x @ C")
;;                               'event-apply+-control-shift-modifier)
;;  (define-key function-key-map (kbd "C-x @ M")
;;                               'event-apply+-meta-shift-modifier)
;;  (define-key function-key-map (kbd "C-x @ H")
;;                               'event-apply+-hyper-shift-modifier)
;;  (define-key function-key-map (kbd "C-x @ S")
;;                               'event-apply+-shift-super-modifier)
;;
;;  etc...

;; -------------------------------------------------------------------
;;; Code:
(defun event-apply+-modifiers (event modifier-list)
  "Apply a set of modifier flags to EVENT.

MODIFIER-LIST is a list of 3-tuples containing:
  SYMBOL- the name of the modifier, as a symbol.
  LSHIFTBY - the numeric value of the modifier, as a keyboard event.
  PREFIX - the string that represents the modifier in an event type
  symbol."
  (if (numberp event)
      (dolist (modifier modifier-list)
	(let ((symbol (car modifier))
	      (lshiftby (cadr modifier))
	      (down (downcase event)))
	  (setq event (cond ((eq symbol 'control)
			     (if (and (<= down ?z) (>= down ?a))
				 (- down ?a -1)
			       (if (and (<= down ?Z) (>= down ?A))
				   (- down ?A -1)
				 (logior (ash 1 lshiftby) event))))
			    ((eq symbol 'shift)
			     (if (and (<= down ?z) (>= down ?a))
				 (upcase event)
			       (logior (ash 1 lshiftby) event)))
			    (t
			     (logior (ash 1 lshiftby) event))))))
    (dolist (modifier modifier-list)
      (when (not (memq (car modifier) (event-modifiers event)))
	(let ((event-type (if (symbolp event) event (car event))))
	  (setq event-type (intern (concat (caddr modifier)
					   (symbol-name
					    event-type)))
		event (if (symbolp event)
			  event-type
			(cons event-type (cdr event))))))))
  event)

(defun event-apply+-read-modifiers (_ignore-prompt)
  "Read in mnemonic characters to apply keyboard modifiers.

This command reads in mnemonic characters (`a': Alt, `c': Control,
`h': Hyper, `m': Meta, `s': super, `S': shift) to apply various
modifiers and then apply them to the following event.

Triggering a non-`mnemonic character' event will apply the modifiers
the that event.  To apply the modifiers to one of the mnemonic
characters itself, either enter the same mnemonic twice, or type <RET>
to end entering mnemonic characters.

\\<function-key-map>For example type:

  \\[event-apply+-read-modifiers] a !                   \
 (to enter `Alt-!' ).
  \\[event-apply+-read-modifiers] a a                   \
 (to enter `Alt-a' )
  \\[event-apply+-read-modifiers] c <RET> a             \
 (to enter `Ctrl-a').
  \\[event-apply+-read-modifiers] a c m h s <RET> a \
 (to enter `Alt-Ctrl-Meta-Hyper-Super-a' ).
  \\[event-apply+-read-modifiers] a <RET> <RET>     \
 (to enter `Alt-<RET>')."
  (let ((event)
	(modifier t)
	(modifier-list))
    (while modifier
      (setq event    (read-event)
	    modifier (cond ((equal event ?a) '(alt     22 "A-"))
			   ((equal event ?c) '(control 26 "C-"))
			   ((equal event ?h) '(hyper   24 "H-"))
			   ((equal event ?m) '(meta    27 "M-"))
			   ((equal event ?s) '(super   23 "s-"))
			   ((equal event ?S) '(shift   25 "S-"))))
      (if (and modifier
	       (not (seq-filter (lambda (elt)
				  (equal (car modifier) (car elt)))
				modifier-list)))
	  (setq modifier-list (append modifier-list (list modifier)))
        (setq modifier nil)))
    (vector (event-apply+-modifiers (if (or (equal event ?\C-m)
					    (equal event 'return))
					(read-event)
				      event)
				    modifier-list))))

;; -------------------------------------------------------------------
;;;; Two Key modifier functions:
;; -------------------------------------------------------------------

;; -------------------------------------------------------------------
;;; Alt + modifiers
;; -------------------------------------------------------------------

(defun event-apply+-alt-control-modifier (_ignore-prompt)
  "Add the Alt and Control modifiers to the following event.

\\<function-key-map>For example, type \
\\[event-apply+-alt-control-modifier] ( to enter Alt-Control-(."
  (vector (event-apply+-modifiers (read-event)
				  '((alt 22 "A-") (control 26 "C-")))))

(defun event-apply+-alt-hyper-modifier (_ignore-prompt)
  "Add the Alt and Hyper modifiers to the following event.

\\<function-key-map>For example, type
\\[event-apply+-alt-hyper-modifier] ( to enter Alt-Hyper-(."
  (vector (event-apply+-modifiers (read-event)
				  '((alt 22 "A-") (hyper 24 "H-")))))

(defun event-apply+-alt-meta-modifier (_ignore-prompt)
  "Add the Alt and Meta modifiers to the following event.

\\<function-key-map>For example, type
\\[event-apply+-alt-meta-modifier] ( to enter Alt-Meta-(."
  (vector (event-apply+-modifiers (read-event)
				  '((alt 22 "A-") (meta 27 "M-")))))

(defun event-apply+-alt-shift-modifier (_ignore-prompt)
  "Add the Alt and Shift modifiers to the following event.

\\<function-key-map>For example, type
\\[event-apply+-alt-shift-modifier] ( to enter Alt-Shift-(."
  (vector (event-apply+-modifiers (read-event)
				  '((alt 22 "A-") (shift 25 "S-")))))

(defun event-apply+-alt-super-modifier (_ignore-prompt)
  "Add the Alt and Super modifiers to the following event.

\\<function-key-map>For example, type
\\[event-apply+-alt-super-modifier] ( to enter Alt-Super-(."
  (vector (event-apply+-modifiers (read-event)
				  '((alt 22 "A-") (super 23 "s-")))))

;; -------------------------------------------------------------------
;;; Control + modifiers
;; -------------------------------------------------------------------

(defun event-apply+-control-hyper-modifier (_ignore-prompt)
  "Add the Control and Hyper modifiers to the following event.

\\<function-key-map>For example, type
\\[event-apply+-control-hyper-modifier] ( to enter Control-Hyper-(."
  (vector (event-apply+-modifiers (read-event)
				  '((control 26 "C-") (hyper 24 "H-")))))

(defun event-apply+-control-meta-modifier (_ignore-prompt)
  "Add the Control and Meta modifiers to the following event.

\\<function-key-map>For example, type
\\[event-apply+-control-meta-modifier] ( to enter Control-Meta-(."
  (vector (event-apply+-modifiers (read-event)
				  '((control 26 "C-") (meta 27 "M-")))))

(defun event-apply+-control-shift-modifier (_ignore-prompt)
  "Add the Control and Shirt modifiers to the following event.

\\<function-key-map>For example, type
\\[event-apply+-control-shift-modifier] ( to enter Control-Shift-(."
  (vector (event-apply+-modifiers (read-event)
				  '((control 26 "C-") (shift 25 "S-")))))

(defun event-apply+-control-super-modifier (_ignore-prompt)
  "Add the Control and Super modifiers to the following event.

\\<function-key-map>For example, type
\\[event-apply+-control-super-modifier] ( to enter Control-Super-(."
  (vector (event-apply+-modifiers (read-event)
				  '((control 26 "C-") (super 23 "s-")))))

;; -------------------------------------------------------------------
;;; Hyper + modifiers
;; -------------------------------------------------------------------

(defun event-apply+-hyper-meta-modifier (_ignore-prompt)
  "Add the Hyper and Meta modifiers to the following event.

\\<function-key-map>For example, type
\\[event-apply+-hyper-meta-modifier] ( to enter Hyper-Meta-(."
  (vector (event-apply+-modifiers (read-event)
				  '((hyper 24 "H-") (meta 27 "M-")))))

(defun event-apply+-hyper-shift-modifier (_ignore-prompt)
  "Add the Hyper and Shift modifiers to the following event.

\\<function-key-map>For example, type
\\[event-apply+-hyper-shift-modifier] ( to enter Hyper-Shift-(."
  (vector (event-apply+-modifiers (read-event)
				  '((hyper 24 "H-") (shift 25 "S-")))))

(defun event-apply+-hyper-super-modifier (_ignore-prompt)
  "Add the Hyper and Super modifiers to the following event.

\\<function-key-map>For example, type
\\[event-apply+-hyper-super-modifier] ( to enter Hyper-Super-(."
  (vector (event-apply+-modifiers (read-event)
				  '((hyper 24 "H-") (super 23 "s-")))))

;; -------------------------------------------------------------------
;;; Meta + modifiers
;; -------------------------------------------------------------------

(defun event-apply+-meta-shift-modifier (_ignore-prompt)
  "Add the Meta and Shift modifiers to the following event.

\\<function-key-map>For example, type
\\[event-apply+-meta-shift-modifier] ( to enter Meta-Shift-(."
  (vector (event-apply+-modifiers (read-event)
				  '((meta 27 "M-") (shift 25 "S-")))))

(defun event-apply+-meta-super-modifier (_ignore-prompt)
  "Add the Meta and Super modifiers to the following event.

\\<function-key-map>For example, type
\\[event-apply+-meta-super-modifier] ( to enter Meta-Super-(."
  (vector (event-apply+-modifiers (read-event)
				  '((meta 27 "M-") (super 23 "s-")))))

;; -------------------------------------------------------------------
;;; Shift + modifiers
;; -------------------------------------------------------------------

(defun event-apply+-shift-super-modifier (_ignore-prompt)
  "Add the Shift and Super modifiers to the following event.

\\<function-key-map>For example, type
\\[event-apply+-meta-super-modifier] ( to enter Meta-Super-(."
  (vector (event-apply+-modifiers (read-event)
				  '((shift 25 "S-") (super 23 "s-")))))

;; -------------------------------------------------------------------

(provide 'event-apply+)

;;; event-apply+.el ends here
