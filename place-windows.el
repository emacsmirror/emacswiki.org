;;; place-windows.el --- commands for placing active windows
;;
;; Copyright 2012 Horishnii Oleksii
;;
;; Author: Horishnii Oleksii <desu@horishniy.org.ua>
;; Version: 0.1
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;; Commentary:
;;
;; This package provides `place-windows' for placing active windows
;; with different variations of rows quantity.
;; Each row height is equal, each column height equal in current row.
;;
;; Add something like the following to your .emacs:
;;
;; (global-set-key [f12] 'place-windows) ; it can be your key instead of [f12]
;;
;; To use: Assume, that you bind it fo f12. Hit f12 a few times, until
;; you satisfied. Or just use universal argument, if you know how much
;; rows do you want: C-u 2 f12 -- that will place all windows in two
;; rows.
;;
;; Feedback:
;;
;; Send any ideas, bugs, feature requests or questions on my mail.
;;
;; References:
;; http://www.emacswiki.org/emacs/place-windows.el -- self-reference
;; http://emacswiki.org/emacs/CategoryWindows -- self-explanatory :)
;;
;;; Code:

(defvar place-windows-level
  1)

(defun place-windows (&optional rows)
  "Used to place active windows, so they use same amount of space. It can be done in many ways, so using it again and again increase number of rows(should be bound to a key to have this effect)."
  (interactive "p")
  (if (and (eq last-command this-command) (= 1 rows))
    (setq rows (incf place-windows-level))
    (setq place-windows-level 1))
  (let* ((visible-buffers (mapcar 'window-buffer (window-list)))
          (buffers-left (length visible-buffers))
          (window-min-width 2)
          (window-min-height 1)
          current-rows)
    (delete-other-windows)
    (when (< buffers-left rows)
      (setq rows buffers-left))
    (setq current-rows rows)
    (loop for i from 1 below rows do
      (split-window-vertically)
      (balance-windows))
    (loop for i from 0 below rows do
      (loop for j from 1 below (ceiling buffers-left current-rows) do
        (switch-to-buffer (pop visible-buffers))
        (split-window-horizontally)
        (balance-windows)
        (other-window 1))
      (switch-to-buffer (pop visible-buffers))
      (other-window 1)
      (decf buffers-left (ceiling buffers-left current-rows))
      (decf current-rows 1))
    (balance-windows)))

(provide 'place-windows)
