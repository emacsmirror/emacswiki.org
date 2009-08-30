;;; pc-keys.el --- Smart `home' and `end' keys.

;; Copyright (C) 1998 by Kai Grossjohann.

;; Author: Kai Grossjohann <Kai.Grossjohann@CS.Uni-Dortmund.DE>
;; Keywords: convenience
;; Version: $Id: pc-keys.el,v 1.3 1998/12/22 23:44:05 grossjoh Exp $

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Some useful bindings for Home and End keys: Hit the key once to go
;; to the beginning/end of a line, hit it twice in a row to go to the
;; beginning/end of the window, three times in a row goes to the
;; beiginning/end of the buffer.  NB that there is no timeout
;; involved.

;;; Code:

(defun pc-keys-home ()
  "Go to beginning of line/window/buffer.
First hitting key goes to beginning of line, second in a row goes to
beginning of window, third in a row goes to beginning of buffer."
  (interactive)
  (let* ((keys (recent-keys))
         (len (length keys))
         (key1 (if (> len 0) (elt keys (- len 1)) nil))
         (key2 (if (> len 1) (elt keys (- len 2)) nil))
         (key3 (if (> len 2) (elt keys (- len 3)) nil))
         (key-equal-1 (equal key1 key2))
         (key-equal-2 (and key-equal-1 (equal key2 key3))))
    (cond (key-equal-2 (if mark-active
                           (goto-char (point-min))
                         (beginning-of-buffer)))
          (key-equal-1 (if mark-active () (push-mark))
		       (move-to-window-line 0))
          (t (beginning-of-line)))))

(defun pc-keys-end ()
  "Go to end of line/window/buffer.
First hitting key goes to end of line, second in a row goes to end
of window, third in a row goes to end of buffer."
  (interactive)
  (let* ((keys (recent-keys))
         (len (length keys))
         (key1 (if (> len 0) (elt keys (- len 1)) nil))
         (key2 (if (> len 1) (elt keys (- len 2)) nil))
         (key3 (if (> len 2) (elt keys (- len 3)) nil))
         (key-equal-1 (equal key1 key2))
         (key-equal-2 (and key-equal-1 (equal key2 key3))))
    (cond (key-equal-2 (if mark-active
                           (goto-char (point-max))
                         (end-of-buffer)))
          (key-equal-1 (if mark-active () (push-mark))
		       (move-to-window-line -1)
		       (end-of-line))
          (t (end-of-line)))))


(global-set-key [home] 'pc-keys-home)
(global-set-key [end] 'pc-keys-end)

(provide 'pc-keys)
;;; pc-keys.el ends here
