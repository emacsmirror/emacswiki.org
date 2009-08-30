;;; wiki-inter.el --- interwikis for wiki.el

;; Copyright (C) 2001, 2002  Alex Schroeder <alex@gnu.org>

;; Emacs Lisp Archive Entry
;; Filename: wiki-inter.el
;; Version: 1.0.3
;; Keywords: hypermedia
;; Author: Alex Schroeder <alex@gnu.org>
;; Maintainer: Alex Schroeder <alex@gnu.org>
;; Description: Support InterWiki links for wiki.el
;; URL: http://www.emacswiki.org/cgi-bin/wiki.pl?%WikiInter

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

;; Support Interwiki link such as JargonFile:luser for wiki.el without
;; aktually hacking wiki.el.

;; This appends `wiki-inter-highlight' to `wiki-highlight-buffer-hook'
;; such that interlink are highlighted and get their own local keymap,
;; and it advises `wiki-no-name-p' such that wiki names within the
;; interlink are not recognized as such.

;; Customize `wiki-inter-links' to add more magic.

;; Note that in order to publish, `wiki-inter-link-publish' must be on
;; `wiki-pub-rules'.  Loading this file puts it there, if you have
;; customized the rules, however, you will have to add
;; `wiki-inter-link-publish' manually.

;;; Code:

(require 'wiki)
(require 'thingatpt); for thing-at-point-url-path-regexp et al.

(defgroup wiki-inter nil
  "InterWiki links."
  :group 'wiki)

(defvar wiki-inter-local-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'wiki-inter-link-at-point)
    (if (featurep 'xemacs)
	(define-key map (kbd "<button2>") 'wiki-inter-link-at-mouse)
      (define-key map (kbd "<mouse-2>") 'wiki-inter-link-at-mouse))
    map)
  "Local keymap used by wiki minor mode while on a InterWiki link.")

(defcustom wiki-inter-links
  '(("Emacs" . "http://www.emacswiki.org/cgi-bin/wiki.pl?%s")
    ("Meatball" . "http://www.usemod.com/cgi-bin/mb.pl?%s")
    ("Wiki" . "http://www.c2.com/cgi/wiki?%s")
    ("Jargon" . "http://www.tuxedo.org/~esr/jargon/html/entry/%s.html"))
  "Alist of InterWiki link names and URL templates.
Interwiki links are written as HOST:PAGE where PAGE is a wiki page on
the HOST wiki.  Each element in this list has the form (HOST
. TEMPLATE).  Every Interwiki link HOST:PAGE will then point to a new
URL.  URL is the result of passing TEMPLATE and PAGE to `format'.

Example: In order to make Meatball:StartingPoints a valid link, use an
element (\"Meatball\" . \"http://usemod.com/cgi-bin/mb.pl?%s\").  This
will result in http://usemod.com/cgi-bin/mb.pl?StartingPoints."
  :type '(repeat (cons (string :tag "Interwiki Host")
		       (string :tag "URL Fragment")))
  :group 'wiki-inter)

(defun wiki-inter-hosts ()
  "Return the host names in `wiki-inter-links'."
  (mapcar (lambda (w) (car w)) wiki-inter-links))

(defun wiki-inter-regexp ()
  "Return the regexp to match interlinks.
See `wiki-inter-links'."
  (concat "\\(" (mapconcat 'identity (wiki-inter-hosts) "\\|") "\\)"
	  ":\\(" thing-at-point-url-path-regexp "\\)"))

(defun wiki-inter-link-p ()
  "Return non-nil if point is at an interlink."
  (thing-at-point-looking-at (wiki-inter-regexp)))

(defun wiki-inter-url (interlink)
  "Return URL based on INTERLINK and `wiki-inter-links'."
  (save-match-data
    (if (string-match (wiki-inter-regexp) interlink)
	(let ((host (match-string 1 interlink))
	      (page (match-string 2 interlink)))
	  (format (cdr (assoc host wiki-inter-links)) page))
      (error "Unknown interlink: %S" interlink))))

;;; Highlighting

(defun wiki-inter-highlight ()
  "Highlight Interwiki links as defined by `wiki-inter-links'."
  (save-excursion
    (goto-char (point-min))
    (let ((regexp (wiki-inter-regexp)))
      (while (re-search-forward regexp nil t)
	(wiki-make-extent (match-beginning 0) (match-end 0)
			  wiki-inter-local-map nil)))))

(add-hook 'wiki-highlight-buffer-hook 'wiki-inter-highlight t)

(defadvice wiki-no-name-p (after wiki-inter-no-name-p activate)
  "Return nil if point is within an Interlink."
  (setq ad-return-value 
	(or ad-return-value
	    (looking-at (wiki-inter-regexp)))))

;;; Following Links

(defun wiki-inter-link (interlink)
  "Follow the INTERLINK.
Uses `wiki-inter-url' to extract the URL and `browse-url' to follow it."
  (browse-url (wiki-inter-url interlink)))

(defun wiki-inter-link-at-point ()
  "Find Interlink at point.
See `wiki-inter-link-p' and `wiki-inter-link'."
  (interactive)
  (if (wiki-inter-link-p)
      (wiki-inter-link (match-string 0))
    (error "Point is not at an Interlink")))

(defun wiki-inter-link-at-mouse (event)
  "Find Interlink at the mouse position.
See `wiki-inter-link-at-point'."
  (interactive "e")
  (save-excursion
    (if (and (functionp 'event-start); Emacs
	     (functionp 'posn-window)
	     (functionp 'posn-point))
	(let ((posn (event-start event)))
	  (set-buffer (window-buffer (posn-window posn)))
	  (goto-char (posn-point posn)))
      (let ((posn (event-point event))); XEmacs
	(set-buffer (event-buffer event))
	(goto-char posn)))
    (wiki-inter-link-at-point)))

;;; Publishing

(defun wiki-inter-link-publish ()
  "Replaces Interwiki links."
  (goto-char (point-min))
  (let ((regexp (wiki-inter-regexp)))
    (while (re-search-forward regexp nil t)
      (let* ((link (match-string 0))
	     (url (wiki-inter-url link)))
	(replace-match (concat "<a href=\"" url "\">" link "</a>") t)))))

(add-hook 'wiki-pub-rules 'wiki-inter-link-publish t)

(provide 'wiki-inter)

;;; wiki-inter.el ends here
