;;; comint-scroll-to-bottom.el --- Keep the comint prompt at the bottom

;; Copyright (C) 2004  Jorgen Schaefer

;; Version: 1.0
;; Keywords: processes
;; Author: Jorgen Schaefer <forcer@forcix.cx>
;; URL: http://www.emacswiki.org/elisp/comint-scroll-to-bottom.el

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Commentary:

;; This will cause comint buffers to always display the maximum amount
;; of output by keeping the prompt at the bottom of the respective
;; window. The code is taken from ERC, and adapted to comint-mode.

;; To use, just add the following to your .emacs:

;; (add-hook 'comint-mode-hook 'comint-add-scroll-to-bottom)

;;; Code:

(defun comint-add-scroll-to-bottom ()
  "Activate `comint-scroll-to-bottom'.
This should be put in `comint-mode-hook' or any derived mode."
  (add-hook 'window-scroll-functions 'comint-scroll-to-bottom nil t))

(defun comint-scroll-to-bottom (window display-start)
  "Recenter WINDOW so that point is on the last line.

This is added to `window-scroll-functions' by
`comint-add-scroll-to-bottom'.

The code is shamelessly taken (but adapted) from ERC."
  (let ((proc (get-buffer-process (current-buffer))))
    (when (and proc
               window
               (window-live-p window))
      (let ((resize-mini-windows nil))
        (save-selected-window
          (select-window window)
          (save-restriction
            (widen)
            (when (>= (point) (process-mark proc))
              (save-excursion
                (recenter -1)
                (sit-for 0)))))))))

(provide 'comint-scroll-to-bottom)
;;; comint-scroll-to-bottom.el ends here
