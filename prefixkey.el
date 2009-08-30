;;; prefixkey.el --- Prefix key processing.

;; Copyright (C) 2003 by Free Software Foundation, Inc.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Maintainer: none, mail me if you want to maintain this code.
;; Created: Wed Nov 26 09:59:20 MSK 2003
;; Keywords: keyboard
;; X-CVS: $Id$

;; This file is NOT part of XEmacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Synched up with: Not in FSF

;;; Commentary:

;; This program provides prefix key, i.e. special key which in
;; combination with other keys gives C-u (universal-argument)
;; functionality.
;;
;; For example, let prefix key will be P, then:
;;
;;   `P-a' is equivalent to `C-u a', i.e. inserts 'aaaa'
;;
;;   `P-M-!' is equivalent to `C-u M-!', i.e. run shell command and
;;   insert output to current buffer.
;;
;; Usage:
;;
;;    (set-keymap-default-binding global-map 'pk-command)

;;; Code:

(defcustom keyboard-prefix-key 'super
  "Modifier to use as prefix key."
  :type '(choice (const :tag "Super" super)
		 (const :tag "Hyper" hyper)
		 (const :tag "Alt" alt)
		 (const :tag "Meta" meta)
		 (const :tag "Control" control))
  :group 'keyboard)


;;;###autoload
(defun pk-command (keys)
  "Default keyboard command."
  (interactive (list (this-command-keys)))

  (let* ((le (car (last (append keys nil)))) ; last key pressed

	 ;; NOTE: we can't just remove `keyboard-prefix-key' from
	 ;; event's modifiers list by side effect, because if
	 ;; `keyboard-prefix-key' is car of that list, it is just
	 ;; impossible, if I wrong please e-mail me.
	 (newev (make-event (event-type le)
			    (nconc
			     (list 'channel (event-channel le)
				   'modifiers (delete keyboard-prefix-key (event-modifiers le))
				   'timestamp (event-timestamp le))
			     (cond ((eq (event-type le) 'key-press)
				    (list 'key (event-key le)))

				   ((member (event-type le) '(button-press button-release motion misc-user))
				    (list 'x (event-x le)
					  'y (event-y le))))

			     (cond ((member (event-type le) '(button-press button-release misc-user))
				    (list 'button (event-button le))))

			     (cond ((eq (event-type le) 'misc-user)
				    (list 'function (event-function le))))
			     )))
	 (newkeys (vconcat (butlast (append keys nil)) (list newev)))
	 (defn (key-binding newkeys)))

    (if (not defn)
	(signal 'undefined-keystroke-sequence (list newkeys))

      (setq last-command-event newev)

      (setq current-prefix-arg '(4))	; XXX
      (call-interactively defn))))

    
(provide 'prefixkey)

;;; prefixkey.el ends here
