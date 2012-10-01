;;; dpaste.el --- Emacs integration for dpaste.com

;; Copyright (C) 2008, 2009 Greg Newman <20seven.org>

;; Version: 0.2
;; Keywords: paste pastie pastebin dpaste python
;; Created: 01 Dec 2008
;; Author: Greg Newman <grep@20seven.org>
;; Guilherme Gondim <semente@taurinus.org>
;; Maintainer: Greg Newman <greg@20seven.org>

;; This file is NOT part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary:

;; dpaste.el provides functions to post a region or buffer to
;; <http://dpaste.com> and put the paste URL into the kill-ring.

;; Inspired by gist.el

;; Current dpaste.com API usage example:

;; curl -si -F 'content=<-' http://dpaste.com/api/v1/ | \
;; grep ^Location: | colrm 1 10

;; Thanks to Paul Bissex (http://news.e-scribe.com) for a great paste
;; service.

;; Installation and setup:

;; Put this file in a directory where Emacs can find it. On GNU/Linux
;; it's usually /usr/local/share/emacs/site-lisp/ and on Windows it's
;; something like "C:\Program Files\Emacs<version>\site-lisp". Then
;; add the follow instructions in your .emacs.el:

;; (require 'dpaste nil)
;; (global-set-key (kbd "C-c p") 'dpaste-region-or-buffer)
;; (setq dpaste-poster "Guido van Rossum")

;; Then with C-c p you can run `dpaste-region-or-buffer'. With a prefix
;; argument (C-u C-c p), your paste will use the hold option.

;; Todo:

;; - Use emacs lisp code to post paste instead curl (version 0.3)

;;; Code:
(defvar dpaste-poster "dpaste.el"
  "Paste author name or e-mail. Don't put more than 30 characters here.")

(defvar dpaste-supported-modes-alist '((css-mode . "Css")
                                       (diff-mode . "Diff")
                                       (haskell-mode . "Haskell")
                                       (html-mode . "DjangoTemplate")
                                       (javascript-mode . "JScript")
                                       (js2-mode . "JScript")
                                       (python-mode . "Python")
                                       (inferior-python-mode . "PythonConsole")
                                       (ruby-mode . "Ruby")
                                       (sql-mode . "Sql")
                                       (sh-mode . "Bash")
                                       (xml-mode . "Xml")))


;;;###autoload
(defun dpaste-region (begin end title &optional arg)
  "Post the current region or buffer to dpaste.com and yank the
url to the kill-ring.

With a prefix argument, use hold option."
  (interactive "r\nsPaste title: \nP")
  (let* ((file (or (buffer-file-name) (buffer-name)))
         (name (file-name-nondirectory file))
         (lang (or (cdr (assoc major-mode dpaste-supported-modes-alist))
                  ""))
         (hold (if arg "on" "off"))
         (output (generate-new-buffer "*dpaste*")))
    (shell-command-on-region begin end
(concat "curl -si"
                                     " -F 'content=<-'"
                                     " -F 'language=" lang "'"
                                     " -F 'title=" title "'"
                                     " -F 'poster=" dpaste-poster "'"
                                     " -F 'hold=" hold "'"
                                     " http://dpaste.com/api/v1/")
output)
    (with-current-buffer output
      (beginning-of-buffer)
      (search-forward-regexp "^Location: \\(http://dpaste\\.com/\\(hold/\\)?[0-9]+/\\)")
      (message "Paste created: %s (yanked)" (match-string 1))
      (kill-new (match-string 1)))
    (kill-buffer output)))

;;;###autoload
(defun dpaste-buffer (title &optional arg)
  "Post the current buffer to dpaste.com and yank the url to the
kill-ring.

With a prefix argument, use hold option."
  (interactive "sPaste title: \nP")
  (dpaste-region (point-min) (point-max) title arg))

;;;###autoload
(defun dpaste-region-or-buffer (title &optional arg)
  "Post the current region or buffer to dpaste.com and yank the
url to the kill-ring.

With a prefix argument, use hold option."
  (interactive "sPaste title: \nP")
  (condition-case nil
      (dpaste-region (point) (mark) title arg)
    (mark-inactive (dpaste-buffer title arg))))


(provide 'dpaste)
;;; dpaste.el ends here.
