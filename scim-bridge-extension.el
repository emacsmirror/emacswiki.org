;;; scim-bridge-extension.el --- Simple extension for scim-bridge.el

;; Author: Andy Stewart lazycat.manatee@gmail.com
;; Maintainer: Andy Stewart lazycat.manatee@gmail.com
;; Copyright (C) 2008, 2009, Andy Stewart, all rights reserved.
;; Created: 2008-10-11 15:12:06
;; Version: 0.1
;; Last-Updated: 2008-10-11 15:12:09
;; URL:
;; Keywords: scim, scim-bridge, scim-agent
;; Compatibility: GNU Emacs 23.0.60.1

;; This file is not part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Features that might be required by this library:
;;
;; `scim-bridge-zh-si'
;;

;;; Overview:
;;
;; This package is some simple extension for `scim-bridge'.
;;

;;; Commentary:
;;
;;
;;

;;; Installation:
;;
;; Put scim-bridge-extension.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'scim-bridge-extension)
;;
;; No need more

;;; Configuration:
;;
;;
;;

;;; Change log:
;;
;; 2008/10/11
;;      First released.
;;

;;; Acknowledgements:
;;
;;      S. Irie <irieshinsuke@yahoo.co.jp>      for `scim-bridge.el'.
;;

;;; TODO
;;
;;
;;

;;; Require
(require 'scim-bridge-zh-si)

;;; Code:

;; Add some alternate keystroke with `scim-preedit-map'.
;; Example make keystroke `M-o' implement same effect with `BACKSPACE'.
(defadvice scim-make-preedit-map
  (around scim-add-preedit-keybindings () activate)
  "This advice for make user can use alternate key-event
do same command with original key-event of SCIM."
  (let ((map ad-do-it)
        (fun (lambda (&optional arg)
               (interactive)
               (setq unread-command-events
                     (cons (cdr (assq last-command-event
                                      '(
                                        (?\M-c . escape)
                                        (?\M-C . escape)
                                        (?\M-m . return)
                                        (?\M-M . return)
                                        (?\M-o . backspace)
                                        (?\M-O . backspace)
                                        (?\M-p . ?,)
                                        (?\M-P . ?,)
                                        (?\M-n . ?.)
                                        (?\M-N . ?.)
                                        )))
                           unread-command-events)))))
    (define-key map [?\M-c] fun)
    (define-key map [?\M-C] fun)
    (define-key map [?\M-m] fun)
    (define-key map [?\M-M] fun)
    (define-key map [?\M-o] fun)
    (define-key map [?\M-O] fun)
    (define-key map [?\M-p] fun)
    (define-key map [?\M-P] fun)
    (define-key map [?\M-n] fun)
    (define-key map [?\M-N] fun)
    map))

(provide 'scim-bridge-extension)

;;; scim-bridge-extension.el ends here

;;; LocalWords:  scim zh si Irie preedit keybindings
