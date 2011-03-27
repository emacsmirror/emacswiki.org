;;; buffer-ring.el --- A torus for buffer navigation. A ring of buffers, and a ring of buffer rings.

;; Copyright (C) 2009 Mike Mattie
;; Author: Mike Mattie codermattie@gmail.com
;; Maintainer: Mike Mattie codermattie@gmail.com
;; Created: 2009-4-16
;; Version: 0.1.0

;; This file is NOT a part of Gnu Emacs.

;; License: GPL-v3

;; buffer-ring.el is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(defconst buffer-ring-version "0.1.0" "buffer-ring version")
(require 'dynamic-ring)

;;
;; default keymap
;;

(global-set-key (kbd "C-c C-b b") 'buffer-ring-list-buffers)
(global-set-key (kbd "C-c C-b r") 'buffer-torus-list-rings)

(global-set-key (kbd "C-c C-b a") 'buffer-ring-add)
(global-set-key (kbd "C-c C-b d") 'buffer-ring-delete)

(global-set-key (kbd "C-c C-b f") 'buffer-ring-next-buffer)
(global-set-key (kbd "C-c C-b b") 'buffer-ring-prev-buffer)
(global-set-key (kbd "C-c C-b c") 'buffer-ring-cycle)

(global-set-key (kbd "C-c C-b n") 'buffer-torus-next-ring)
(global-set-key (kbd "C-c C-b p") 'buffer-torus-prev-ring)
(global-set-key (kbd "C-c C-b e") 'buffer-torus-delete-ring)

(defvar buffer-ring-torus (make-dyn-ring)
  "a global ring of all the buffer rings. A torus I believe.")

(defvar buffer-ring-default nil
  "The default buffer ring")

;;
;;  buffer ring structure
;;

(defun bfr-ring-name ( buffer-ring )
  (car (dyn-ring-element-value buffer-ring)))

(defun bfr-ring-ring ( buffer-ring )
  (cdr (dyn-ring-element-value buffer-ring)))

(defun make-bfr-ring ( name )
  (cons name (make-dyn-ring)))

;;
;;  buffer torus functions.
;;

(defun bfr-torus-find-ring ( name )
  "bfr-torus-find-ring NAME

   Search the buffer torus for a ring NAME and return it if found
   or nil otherwise.
  "
  (let
    ((found (dyn-ring-find buffer-ring-torus
              (lambda ( buffer-ring )
                (when (string-equal (bfr-ring-name buffer-ring) name) t)) )))
    (when found
      (car found)) ))

(defun bfr-torus-get-ring ( name )
  "bfr-torus-get-ring NAME

   Find a existing buffer ring, or create a new buffer ring with name.
   buffer-ring-default is updated. The buffer-ring is returned.
  "
  (let
    ((buffer-ring (bfr-torus-find-ring name)))

    (if buffer-ring
      ;; if it already exists return the ring.
      (progn
        (message "Adding to existing ring: %s" name)
        buffer-ring)

      ;; otherwise create a new ring buffer, which is a cons of the
      ;; name and a ring. insert the ring into the global ring.
      (progn
        (message "Creating a new ring \"%s\"" name)
        (setq buffer-ring-default name)
        (dyn-ring-insert buffer-ring-torus (dyn-ring-make-element (make-bfr-ring name))) )) ))

;;
;; buffer ring functions
;;

(defun bfr-ring-buffer-ptr ( buffer )
  "bfring-buffer-ptr BUFFER

   make sure BUFFER is a string so that it will still point to a
   buffer when a buffer is destroyed and re-created.
  "
  (if (bufferp buffer)
    (buffer-name buffer)
    buffer))

(defun bfr-ring-find-buffer ( buffer-ring buffer-name )
  "bfr-ring-find-buffer RING BUFFER

   Search buffer RING for BUFFER. return the buffer ring element
   if found, otherwise nil.
  "
  (let
    ((found (dyn-ring-find buffer-ring
              (lambda ( ring-element )
                (when (string-equal (dyn-ring-element-value ring-element) buffer-name) t)) )))
    (when found
      (car found)) ))

(defun bfr-ring-add-buffer ( buffer-ring buffer )
  "bfr-ring-add-buffer RING BUFFER

   Add BUFFER to buffer RING. If the buffer is already in the ring return
   the existing buffer element, or a new one inserted in the buffer RING.
  "
  (let*
    ((buffer-ptr     (bfr-ring-buffer-ptr buffer))
     (buffer-element (bfr-ring-find-buffer (bfr-ring-ring buffer-ring) buffer-ptr)))

    (if buffer-element
      (progn
        (message "buffer %s is already in ring \"%s\"" (buffer-name) (bfr-ring-name buffer-ring))
        buffer-element)
      (dyn-ring-insert (bfr-ring-ring buffer-ring) (dyn-ring-make-element buffer-ptr))) ))

(defun bfr-in-ring-p ( &optional buffer )
  "bfr-in-ring-p &optional BUFFER

   return t if BUFFER is in a ring. The argument is optional,
   it defaults to the current buffer.
  "
  (with-current-buffer (or buffer (current-buffer))
    (and (local-variable-p 'buffer-ring) (local-variable-p 'buffer-ring-this-buffer))))

(defun bfr-ring-size ( &optional buffer )
  "bfr-ring-size &optional BUFFER

   Returns the number of buffers in the ring for BUFFER.
   If the buffer is not in a ring it returns -1 so that
   you can always use a numeric operator.
  "
  (with-current-buffer (or buffer (current-buffer))
    (if (bfr-in-ring-p buffer)
      (dyn-ring-size (bfr-ring-ring buffer-ring))
      -1) ))

;;
;; buffer ring interface
;;

(defun buffer-ring-add ( name )
  "buffer-ring-add

   Add the current buffer to a ring. It will prompt for the ring
   to add the buffer to.
  "
  (interactive "sAdd to ring ? ")

  (if (boundp 'buffer-ring)
    (message
      "This buffer is already in ring %s, delete it before adding it to another ring"
      (bfr-ring-name buffer-ring))
    (progn
      (set (make-local-variable 'buffer-ring) (bfr-torus-get-ring name))
      (set (make-local-variable 'buffer-ring-this-buffer) (bfr-ring-add-buffer buffer-ring (current-buffer)))
      (set (make-local-variable 'buffer-ring-modeline) (concat " Ring (" name ") "))

      (add-hook 'kill-buffer-hook 'buffer-ring-delete t t))) )

(defun buffer-ring-delete ()
  "buffer-ring-delete

   Delete the buffer from the ring. This modifies the ring, it does not
   kill the buffer.
  "
  (interactive)
  (if (bfr-in-ring-p)
    (progn
      (dyn-ring-delete (bfr-ring-ring buffer-ring) buffer-ring-this-buffer)

      (kill-local-variable 'buffer-ring)
      (kill-local-variable 'buffer-ring-this-buffer)
      (kill-local-variable 'buffer-ring-modeline)

      (remove-hook 'kill-buffer-hook 'bfr-del-buffer t))
    (message "This buffer is not in a ring")))

(defun buffer-ring-list-buffers ()
  "buffer-ring-list-buffers

   List the buffers in the buffer-ring associated with the current buffer.
  "
  (interactive)

  (if (boundp 'buffer-ring)
    (let
      ((buffer-list nil))

      (dyn-ring-map
        (bfr-ring-ring buffer-ring)
        (lambda ( name )
          (setq buffer-list
            (if buffer-list
              (concat name "," buffer-list)
              name))))

    (message "buffers in [%s]: %s" (bfr-ring-name buffer-ring) buffer-list))
    (message "This buffer is not in a ring.")) )

(defun bfr-rotate-buffer-ring ( direction )
  (if (bfr-in-ring-p)
    (let
      ((ring (bfr-ring-ring buffer-ring)))

      (if (< (dyn-ring-size ring) 2)
        (message "There is only one buffer in the ring.")
        (progn
          (funcall direction ring)
          (switch-to-buffer (dyn-ring-value ring))) ))
    (message "This buffer is not in a ring") ))

(defun buffer-ring-prev-buffer ()
  "buffer-ring-prev-buffer

   Switch to the previous buffer in the buffer ring.
  "
  (interactive)
  (bfr-rotate-buffer-ring 'dyn-ring-rotate-left))

(defun buffer-ring-next-buffer ()
  "buffer-ring-next-buffer

   Switch to the previous buffer in the buffer ring.
  "
  (interactive)
  (bfr-rotate-buffer-ring 'dyn-ring-rotate-right))

(defun buffer-ring-cycle ()
  "buffer-ring-cycle

   When the buffer is in a ring cycle to the next buffer in the
   ring. If the buffer is not in a ring use other-buffer.
  "
  (interactive)
  (if (> (bfr-ring-size) 0)
    (buffer-ring-next-buffer)
    (switch-to-buffer (other-buffer))))

;;
;; buffer torus interface
;;

(defun bfr-current-name ()
  (car (dyn-ring-value buffer-ring-torus)))

(defun bfr-current-ring ()
  (cdr (dyn-ring-value buffer-ring-torus)))

(defun bfr-rotate-buffer-torus ( direction )
  (if (< (dyn-ring-size buffer-ring-torus) 2)
    (message "There is only one buffer ring; ignoring the rotate command")
    ;; rotate past any empties
    (if (dyn-ring-rotate-until
          buffer-ring-torus
          direction
          (lambda ( buffer-ring )
            (not (dyn-ring-empty-p buffer-ring))))
      (progn
        (message "switching to ring %s" (bfr-current-name))
        (switch-to-buffer (dyn-ring-value (bfr-current-ring))))

      (message "All of the buffer rings are empty. Keeping the current ring position")) ))

(defun buffer-torus-next-ring ()
  "buffer-torus-next-ring

   Switch to the previous buffer in the buffer ring.
  "
  (interactive)
  (bfr-rotate-buffer-torus 'dyn-ring-rotate-right))

(defun buffer-torus-prev-ring ()
  "buffer-torus-prev-ring

   Switch to the previous buffer in the buffer ring.
  "
  (interactive)
  (bfr-rotate-buffer-torus 'dyn-ring-rotate-left))

(defun buffer-torus-list-rings ()
  "buffer-torus-list-rings.

   List the buffer rings in the buffer torus.
  "
  (interactive)

  (let
    ((ring-list nil))

    (mapc
      (lambda ( name )
        (setq ring-list
          (if ring-list
            (concat name "," ring-list)
            name)))
      (dyn-ring-map buffer-ring-torus 'car))

    (message "buffer rings: %s" ring-list) ))

(defun buffer-torus-delete-ring ()
  "buffer-torus-delete-ring

   Delete the entire current buffer-ring.
  "
  (interactive)

  (save-excursion
    (mapc
      (lambda ( buffer-name )
        (with-current-buffer buffer-name
          (buffer-ring-delete)))

      (dyn-ring-map (bfr-current-ring) (lambda ( buffer-name )
                                         buffer-name)) )
    (dyn-ring-delete buffer-ring-torus (car buffer-ring-torus)) ))

(buffer-torus-delete-ring)

(provide 'buffer-ring)
;;; buffer-ring.el ends here
