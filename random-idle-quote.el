;;; random-idle-quote.el --- display a random quote during idle

;; Copyright (C) 2004  Jeremy Cowgar <jeremy@cowgar.com>

;; Version: 1.0
;; Author: Jeremy Cowgar <jeremy@cowgar.com>
;; URL: http://jeremy.cowgar.com/EmacsRandomIdleQuote.html

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

;; Add:
;;
;;   (require 'random-idle-quote)
;;   (random-idle-quote)
;;
;; to your ~/.emacs ... when your emacs goes into idle mode, a quote
;; will appear in your echo window. I use it to help me remember new
;; things. The quotes that have come with it are straight from
;; http://www.emacswiki.org/cgi-bin/wiki/EmacsNiftyTricks
;;
;; But feel free to customize the random-idle-quotes to something more
;; useful to yourself.

;;; Thanks:

;; Many people on irc.freenode.net#emacs

;;; Code:

;; Customization

;;(defgroup random-idle-quote nil
;;  "Options concerning the configuration of random-idle-quote"
;;  :group 'random-idle-quote)

(defcustom random-idle-quote-delay 10
  "Number of seconds to display quote after emacs enters idle mode"
  :group 'random-idle-quote
  :type 'number)

(defcustom random-idle-quotes
  '("When doing isearch, C-w adds the word following point to the search buffer. Very useful."
    "Remove trailing whitespace with delete-trailing-whitespace in Emacs 21 and later."
    "M-x auto-insert-mode -- have your files start up with a user defined template."
    "M-z zap-to-char -- Zap to the char of your choice."
    "M-x line-number-mode and column-number-mode (or (column-number-mode 1) and/or (line-number-mode 1) in your .emacs)"
    "M-/ auto-completes a word. Good for long variable names."
    "Trouble building a regex? M-x re-builder!"
    "C-u M-! inserts the result of the shell-command directly into the buffer, quite handy for stuff like uptime, uname, etc..."
    "C-u M-| replaces the current region with the output of a shell command which gets the region passed as input. e.g. C-x h C-u M-| uniq RET can be used to apply the uniq command to the current buffer")
  "List of quotes to show during idle"
  :group 'random-idle-quote
  :type '(repeat string))

(defun random-idle-quote-get ()
  (nth (random (length random-idle-quotes)) random-idle-quotes))

(defun random-idle-quote-show ()
  (interactive)
  (message (format "Quote Time: %s" (random-idle-quote-get))))

(defun random-idle-quote()
  (interactive)
  (setq random-idle-quote-timer (run-with-idle-timer random-idle-quote-delay 5 'random-idle-quote-show)))

(defun random-idle-quote-stop()
  (interactive)
  (cancel-timer random-idle-quote-timer))

(provide 'random-idle-quote)

;;; random-idle-quote.el ends here
