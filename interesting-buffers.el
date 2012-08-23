;;; interesting-buffers.el --- Add keybinding to cleanup displayed buffers

;; Copyright © 2010, 2011, 2012 Jörg Walter <info@syntax-k.de>
;; Author: Jörg Walter <info@syntax-k.de>
;; URL: http://www.emacswiki.org/emacs/InterestingBuffers
;; Created: 2010
;; Version: 0.1
;; Keywords: buffer dispose

;; This file is not part of GNU Emacs.

;;; License:

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 2 of the License, or
;; (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides a secondary behaviour to `keyboard-escape-quit'.
;; Basically, it will clean up your windows to get a more useful state.
;;
;; Upon activation, several things happen. First, all buffers that are
;; disposable (as specified in `non-interesting-modes') will be killed.  By
;; default, this is used for completion lists and the like.
;;
;; Next, any buffer that are neither bound to a file nor have a mode specified
;; in `interesting-modes' are buried (but not killed).  Furthermore, if an
;; interesting buffer is showing in `next-window', it is buried as well
;; (assuming it has been auto-opened through some help command and should now be
;; hidden again).
;;
;; Finally, any duplicate visible buffers are replaced by the next interesting
;; buffer that is not already showing.
;;
;; All other effects of `keyboard-escape-quit' still apply, of course. Think of
;; this as the ultimate "Get me out of here and make things nice again!"

;;; Installation:

;; To use, save interesting-buffers.el to a a directory on your load-path
;; (e.g., ~/.emacs.d/elisp), then add the following to your .emacs file:
;;
;; (require 'interesting-buffers)

(defgroup interesting-modes nil
  "Options for `interesting-modes.el'.")

(defcustom non-interesting-modes '(help-mode completion-list-mode apropos-mode image-mode)
  "Buffers that should always be disposed of."
  :type 'sexp
  :group 'interesting-modes
  )

(defcustom interesting-modes '(Info-mode dired-mode man-mode text-mode)
  "Non-file buffers that usually interesting, unless showing in `next-window'."
  :type 'sexp
  :group 'interesting-modes
  )

(defun interesting-buffer-p (buffer)
  (or (buffer-local-value 'buffer-file-name buffer)
      (find (buffer-local-value 'major-mode buffer) interesting-modes)
      (minibufferp buffer)))

(defun show-only-interesting-buffers ()
  (interactive)
  (let ((curwin (selected-window)))

					; A non-file buffers that's interesting
					; will still be buried if it is
					; currently visible in (next-window). It
					; may reappear later if there are less
					; interesting buffers than windows
    (select-window (next-window))
    (if (find major-mode interesting-modes) (bury-buffer))

					; bury other non-file buffers, kill
					; disposable ones
    (loop for buffer being the buffers
	  if (find buffer non-interesting-modes)
	  do (kill-buffer buffer)
	  else if (not (interesting-buffer-p buffer))
	  do (bury-buffer buffer))

					; switch away from precious buffer,
					; although it may reappear later if
					; there are only few interesting buffers
    (if (find major-mode interesting-modes) (switch-to-buffer nil))

					; make sure we only have interesting
					; buffers visible
    (loop for win being the windows
	  if (not (interesting-buffer-p (window-buffer win)))
	  do
	  (select-window win)
	  (switch-to-buffer nil))

					; eliminate duplicates
    (let ((buffers (list (window-buffer curwin))))

      (loop for win = (next-window curwin 0) then (next-window win 0)
	    while (not (eq win curwin))
	    if (find (window-buffer win) buffers)
	    do
	    (select-window win)
	    (switch-to-buffer nil)
	    (setq buffers (cons (current-buffer) buffers))))

    (select-window curwin)))

(defadvice keyboard-escape-quit (around keyboard-escape-quit-interesting-buffers activate)
  (if (not (minibufferp (current-buffer)))
      (show-only-interesting-buffers))
  (save-window-excursion ad-do-it))

(provide 'interesting-buffers)

;;; interesting-buffers.el ends here
