;;; elscreen-edit-server.el --- work together chrome and elscreen

;; Copyright (C) 2010  khiker

;; Author: khiker <khiker.mail+elisp@gmail.com>
;; Keywords: elscreen, edit-server

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Work together a GoogleChrome extension "Edit with Emacs"
;; and elscreen.el.

;; 1. Download and put this file to your `load-path' directory.
;; 2. Require `elscreen.el', `edit-server.el' and this file.
;;
;; For example:
;;
;; (require 'elscreen)
;; (require 'edit-server)
;; (require 'elscreen-edit-server)
;;
;;
;; This file `defadvice' to function of edit-server.el.
;;
;; - edit-server-done
;; - edit-server-create-frame

;;; Code:

;; Variables:

(defvar elscreen-edit-server-new-screen-created nil)


;; Advices:

(defadvice edit-server-done
  (after elscreen-edit-server-done)
  (when elscreen-edit-server-new-screen-created
    (elscreen-kill)
    (setq elscreen-edit-server-new-screen-created nil)))

(defadvice edit-server-create-frame
  (around elscreen-edit-server-create-frame (arg))
  (setq elscreen-edit-server-new-screen-created nil)
  (cond
   ((> 10 (elscreen-get-number-of-screens))
    (setq elscreen-edit-server-new-screen-created t)
    (elscreen-create)
    (switch-to-buffer (ad-get-arg 0)))
   (t
    ad-do-it)))


;; Functions

(defun elscreen-edit-server-toggle ()
  "The function that toggles `defadvice' of `edit-server-done'
and `edit-server-create-frame'."
  (interactive)
  (cond
   ((and (ad-is-advised 'edit-server-done)
         (ad-is-active 'edit-server-done))
    (ad-deactivate 'edit-server-done)
    (message "[edit-server-done] defadvice is disabled."))
   (t
    (ad-activate 'edit-server-done)
    (message "[edit-server-done] defadvice is enabled.")))
  (cond
   ((and (ad-is-advised 'edit-server-create-frame)
         (ad-is-active 'edit-server-create-frame))
    (ad-deactivate 'edit-server-create-frame)
    (message "[edit-server-create-frame] defadvice is disabled."))
   (t
    (ad-activate 'edit-server-create-frame)
    (message "[edit-server-create-frame] defadvice is enabled."))))

;; Enabling `defadvice'
(elscreen-edit-server-toggle)


(provide 'elscreen-edit-server)

;; elscreen-edit-server.el ends here
