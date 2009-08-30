;;; rcirc-ding.el -- do something such as beep on channel activity
;; Copyright 2009  Alex Schroeder

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Use M-x rcirc-ding in a channel when you want to get a ding when
;; there is any activity there.  This only happens if the buffer is
;; not visible and `rcirc-ignore-buffer-activity-flag' is non-nil.

;;; Code:

(define-minor-mode rcirc-ding
  "Make some noise whenever there is activity in the current buffer."
  nil " Ding")

(defvar rcirc-ding-this 'ding
  "A function to call when `rcirc-ding' is active.
The variable buffer will be dynamically bound to the current buffer
where activity is occuring.

Example value:

    (lambda (&rest ignore)
      (call-process \"play\" nil nil nil \"/some/file.ogg\"))")

(defun rcirc-ding-maybe (&rest ignore)
  "When minor-mode `rcirc-ding' is active, `rcirc-ding-this' is called.
See `rcirc-activity-hooks' for more."
  (when rcirc-ding
    (run-with-idle-timer
     3 nil rcirc-ding-this)))

(add-hook 'rcirc-activity-hooks 'rcirc-ding-maybe)

(provide 'rcirc-ding)

;; rcirc-ding.el ends here
