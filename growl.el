;;; growl.el --- Growl notifications

;; Copyright (C) 2006 BT Templeton

;; Author: BT Templeton <bpt@tunes.org>
;; Keywords: growl notification mac osx

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
;; Free Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston,
;; MA 02110-1301, USA.

;;; Code:

(defvar growl-program "growlnotify")

(defun growl (title message)
  (start-process "growl" " growl"
                 growl-program
                 title
                 "-a" "Emacs")
  (process-send-string " growl" message)
  (process-send-string " growl" "\n")
  (process-send-eof " growl"))

(defun growl-rcirc-print-hook (process sender response target text)
  (when (and (string-match (rcirc-nick process) text)
             (not (string= (rcirc-nick process) sender))
             (not (string= (rcirc-server process) sender)))
    (growl "You Were Mentioned"
           (format "You were mentioned by %s in %s" sender target))))

(eval-after-load 'rcirc
  '(add-hook 'rcirc-print-hooks 'growl-rcirc-print-hook))

(provide 'growl)
;;; growl.el ends here
