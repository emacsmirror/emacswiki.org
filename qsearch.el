;;; qsearch.el --- simple isearch implemented as a quasimode

;; Copyright (C) 2002  Alex Schroeder

;; Author: Alex Schroeder <alex@gnu.org>
;; Maintainer: Alex Schroeder <alex@gnu.org>
;; Version: 1.0.2
;; Keywords: convenience
;; URL: http://www.emacswiki.org/cgi-bin/wiki.pl?QuasimodeSearch

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; After loading, type a string while pressing Super to search forward,
;; type any string while pressing Hyper to search backwards.  Thus, you
;; can skipt to the end of the sentence using s-. or to the end of the
;; line using s-<return> or to the end of the paragraph using s-<return>
;; s-<return>.  This saves you the C-s keystroke used for isearch.  At
;; the same time, qsearch is quite crude -- no overlays, no repeating of
;; searches, no support for environments without Super and Hyper keys,
;; no support for regexps, no customizations.
;; 
;; Note that you might need to setup Super and Hyper using xmodmap.
;; Example code for your .xmodmaprc or .Xmodmap file:
;;
;; keycode 115 = Hyper_L
;; keycode 116 = Super_R
;; clear mod3
;; clear mod4
;; clear mod5
;; add mod3 = Hyper_L
;; add mod4 = Super_R

;;; Code:

(defvar qsearch-modifiers '(super hyper)
  "A list of two modifier symbols.
The first is used to qsearch forward,
the second is used to qsearch backwards.")

(defvar qsearch-string ""
  "Current string to search for.")

(defvar qsearch-translations '((return . ?\n)
			       (home . ?\n)
			       (end . ?\n)
			       (tab . ?\t)
			       (escape . ?\e)
			       (prior . ?\f)
			       (next . ?\f))
  "Translations from event types to characters.")

(defvar qsearch-mode-map
  (let ((map (make-sparse-keymap)))
    (mapc (lambda (key)
	    (mapc (lambda (modifier)
		    (define-key map
		      (vector (list modifier (aref key 0)))
		      'qsearch))
		  qsearch-modifiers))
	  (where-is-internal 'self-insert-command))
    (mapc (lambda (key)
	    (mapc (lambda (modifier)
		    (define-key map (vector (list modifier key)) 'qsearch))
		  qsearch-modifiers))
	  (mapcar 'car qsearch-translations))
    (define-key map (kbd "C-s-s") 'qsearch-forward)
    (define-key map (kbd "C-H-s") 'qsearch-forward)
    (define-key map (kbd "C-s-r") 'qsearch-backward)
    (define-key map (kbd "C-H-r") 'qsearch-backward)
    map)
  "Keymap for `qsearch-mode'.
All keys bound to `self-insert-command' and all keys listed in
`qsearch-translations' get additional bindings according to
`qsearch-modifiers'.")

(defun qsearch-continue-p ()
  "Return non-nil if the current event continues a qsearch."
  (let* ((size (1- (length (recent-keys))))
	 (m1 (event-modifiers (aref (recent-keys) size)))
	 (m2 (event-modifiers (aref (recent-keys) (1- size)))))
    (or (and (memq (nth 0 qsearch-modifiers) m1)
	     (memq (nth 0 qsearch-modifiers) m2))
	(and (memq (nth 1 qsearch-modifiers) m1)
	     (memq (nth 1 qsearch-modifiers) m2)))))

(defun qsearch-type ()
  "Return type of qsearch.
Depending on modifier pressed and `qsearch-regexp-flag' this is either
forward, backward, forward-regexp or backward-regexp."
  (cond ((memq (nth 0 qsearch-modifiers)
	       (event-modifiers last-command-event))
	 'forward)
	((memq (nth 1 qsearch-modifiers)
	       (event-modifiers last-command-event))
	 'backward)))

(defun qsearch-append-event ()
  "Add current event to `search-string'."
  (let ((type (event-basic-type last-command-event))
	(modifiers (event-modifiers last-command-event)))
    (unless (integerp type)
      (setq type (cdr (assq type qsearch-translations))))
    (when (integerp type)
      (when (memq 'shift modifiers)
	(setq type (upcase type)))
      (setq qsearch-string
	(concat qsearch-string (string type)))
      (message "qsearch: %s" qsearch-string))))

(defun qsearch-forward ()
  "Run current `qsearch' forward."
  (interactive)
  (search-forward qsearch-string))

(defun qsearch-backward ()
  "Run current `qsearch' backward."
  (interactive)
  (search-backward qsearch-string))

(defun qsearch ()
  "Do a qsearch.
This function must be bound to normals keys with one of the
modifiers in `qsearch-modifiers'.  This is accomplished via
`qsearch-mode'.  You must activate it for `qsearch' to work."
  (interactive)
  (unless (qsearch-continue-p)
    (setq qsearch-string ""))
  (qsearch-append-event)
  (unless (looking-at qsearch-string)
    (if (eq 'forward (qsearch-type))
	(qsearch-forward)
      (qsearch-backward))
    (goto-char (match-beginning 0))))

(define-minor-mode qsearch-mode
  "A quasimode interactive search.
Typing letters with a modifier in `qsearch-modifiers'
with start a search."
  :group 'qsearch
  :init-value t
  :global t)

(provide 'qsearch)

;;; qsearch.el ends here
