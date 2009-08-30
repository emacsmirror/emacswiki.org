;;; lisppaste-extension.el --- Some extension for lisppaste

;; Filename: lisppaste-extension.el
;; Description: Some extension for lisppaste
;; Author: Andy Stewart lazycat.manatee@gmail.com
;; Maintainer: Andy Stewart lazycat.manatee@gmail.com
;; Copyright (C) 2008, 2009, Andy Stewart, all rights reserved.
;; Created: 2008-12-05 14:49:32
;; Version: 0.1
;; Last-Updated: 2008-12-05 14:49:34
;;           By: Andy Stewart
;; URL:
;; Keywords: lisppaste, irc
;; Compatibility: GNU Emacs 23.0.60.1
;;
;; Features that might be required by this library:
;;
;; `lisppaste'
;;

;;; This file is NOT part of GNU Emacs

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

;;; Commentary:
;;
;; Some extension for lisppaste
;;

;;; Installation:
;;
;; Put lisppaste-extension.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'lisppaste-extension)
;;
;; No need more.

;;; Change log:
;;
;; 2008/12/05
;;      First released.
;;

;;; Acknowledgements:
;;
;;
;;

;;; TODO
;;
;;
;;

;;; Require
(require 'lisppaste)

;;; Code:

(defvar window-configuration-before-lisppaste nil
  "The window configuration before do `lisppaste-create-new-paste' command.")

(defadvice lisppaste-create-new-paste (before record-window-configuration activate)
  "Record current window configuration before `lisppaste-create-new-paste'."
  (or window-configuration-before-lisppaste
      (setq window-configuration-before-lisppaste (current-window-configuration))))

(defadvice lisppaste-send-paste (after revert-window-configuration activate)
  "Revert window configuration before do `lisppaste-create-new-paste'."
  (when window-configuration-before-lisppaste
    (set-window-configuration window-configuration-before-lisppaste)
    (setq window-configuration-before-lisppaste nil)))

(defun lisppaste-irc-paste ()
  "Create a new paste in irc."
  (interactive)
  (let (channel-name)
    ;; Get channel name
    (cond
     ((eq major-mode 'erc-mode)
      (setq channel-name (buffer-name)))
     ((eq major-mode 'rcirc-mode)
      (setq channel-name (buffer-name))
      (setq channel-name (replace-match+ channel-name "@.*$" "")))
     (t
      (setq channel-name (read-string "Channel name: "))))
    ;; Get channel list
    (unless lisppaste-channels
      (message "Get support channel list of lisppaste.")
      (setq lisppaste-channels (lisppaste-channels)))
    ;; Paste some content to special IRC channel
    (if (member channel-name lisppaste-channels)
        (lisppaste-create-new-paste channel-name (or erc-nick rcirc-default-nick))
      (message "Sorry, current channel can't use lisppaste."))))

(provide 'lisppaste-extension)

;;; lisppaste-extension.el ends here

;;; LocalWords:  lisppaste
