;;; rcirc-color.el -- color nicks
;; Copyright 2005, 2006, 2007  Alex Schroeder

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

;; Use /COLOR to list all colored nicks with their color
;; Use /COLOR NICK COLOR to color NICK using COLOR

;;; Code:

(require 'rcirc)

(defvar rcirc-colors
  (if (fboundp 'color-distance)
      (let ((min-distance (* 0.23 (color-distance "black" "white")))
	    (bg (face-background 'default))
	    (fg (face-foreground 'rcirc-my-nick))
	    candidates)
	(dolist (item color-name-rgb-alist)
	  (let ((color (car item)))
	    (when (and (not (color-gray-p color))
		       (> (color-distance color bg) min-distance)
		       (> (color-distance color fg) min-distance))
	      (setq candidates (cons color candidates)))))
	candidates)
    (delete (face-background 'default) (defined-colors)))
  "Colors to use for nicks in rcirc.
By default, all the non-grey colors that are very different from
the default background are candidates.  The minimum
color-distance is half the distance between black and red as
computed by `color-distance'.

To check out the list, evaluate (list-colors-display rcirc-colors).")

(defvar rcirc-color-mapping (make-hash-table :test 'equal)
  "Hash-map mapping nicks to color names.")

(defadvice rcirc-facify (before rcirc-facify-colors activate)
  "Add colors to other nicks based on `rcirc-colors'.
If the sender of the current message is a new nick, pick a random
color from `rcirc-colors' and store the nick and the new color in
`rcirc-color-mapping'. This hash map is also used by
`rcirc-markup-nick-colors'."
  (when (and (eq face 'rcirc-other-nick)
	     (not (string= string "")))
    (let ((color (gethash string rcirc-color-mapping)))
      (unless color
	(setq color (elt rcirc-colors (random (length rcirc-colors))))
	(puthash string color rcirc-color-mapping))
      (setq face `((foreground-color . ,color))))))

(defun rcirc-markup-nick-colors (&rest ignore)
  "Color nicks mentioned in ordinary messages.
The colors are based on `rcirc-color-mapping'. The advice to
`rcirc-facify' adds new entries to this hash map."
  ;; The number of arguments differes between Emacs 22 and Emacs
  ;; 23. Since we don't need either of them, let's ignore them.
    (with-syntax-table rcirc-nick-syntax-table
      (maphash (lambda (nick color)
		 (let ((face (cons 'foreground-color color)))
		   (goto-char (point-min))
		   (while (re-search-forward
			   (concat "\\b" (regexp-quote nick) "\\b") nil t)
		     (rcirc-add-face (match-beginning 0) (match-end 0) face))))
	     rcirc-color-mapping)))

(add-to-list 'rcirc-markup-text-functions 'rcirc-markup-nick-colors)

(defun-rcirc-command color (args)
  "Change one of the nick colors."
  (interactive)
  (setq args (split-string args))
  (rcirc-do-color (car args) (cadr args) process target))

(defun rcirc-do-color (nick color process target)
  "Implement /COLOR."
  (if (not nick)
      (let (names)
	(maphash (lambda (key value)
		   (add-text-properties
		    0 (length key)
		    `(face ((foreground-color . ,value)) help-echo ,value)
		    key)
		   (setq names (cons key names)))
		 rcirc-color-mapping)
	(rcirc-print process (rcirc-nick process) "NOTICE" target
		     (mapconcat	'identity names " ")))
    (unless color
      (error "Use what color?"))
    (puthash nick color rcirc-color-mapping)))

(defadvice rcirc-handler-NICK (before rcirc-handler-NICK-colors activate)
  "Update colors in `rcirc-color-mapping'."
  (let* ((old-nick (rcirc-user-nick sender))
	 (color (gethash old-nick rcirc-color-mapping))
         (new-nick (car args)))
    ;; don't delete the old mapping
    (when color
      (puthash new-nick color rcirc-color-mapping))))

(provide 'rcirc-color)

;;; rcirc-color.el ends here
