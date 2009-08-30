;;; saw-client.el --- Interaction with sawfish

;; Copyright (C) 2007-2009 Jeremy Hankins

;; Author: Jeremy Hankins nowan at nowan dot org
;; Keywords: sawfish

;; Last-Updated: Thu Mar 12 2009

;;{{{ Commentary:

;; This is for folks who want closer integration between the sawfish
;; window manager and emacs.  `saw-client-eval' can be used to have
;; sawfish execute lisp code; if you're using my emacs.jl addon for
;; sawfish (which runs emacs lisp from sawfish) you should use this
;; rather than the similar function in sawfish.el because it checks to
;; see if `running-from-sawfish' is set.
;;
;; Other than that, there's advice here that will switch to a frame in
;; sawfish if `display-buffer' is called, or `y-or-n-p' and
;; `yes-or-no-p'.
;;
;; TODO: Is it bad form to enable the advice automatically?  Maybe it
;; should be enabled via an activate function.

;;}}}
;;{{{ GPL blurb

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;}}}

;; This file is *NOT* part of GNU Emacs.

;;; Code:

;;{{{ sawfish interaction

(defvar sawfish-client "sawfish-client"
  "Command to use to interact with sawfish.")

(defvar running-from-sawfish-p nil
  "Indicates whether emacs is currently running code on behalf of
sawfish, to prevent deadlocks.")

(defun saw-client-eval (sexp &optional  target-buffer)
  "Evaluate sexp using sawfish-client."
  (when window-system
    (let ((expression (concat "(let ((running-from-emacs-p t)) "
			      (if (stringp sexp) sexp (format "%S" sexp))
			      ")")))
    (if running-from-sawfish-p
	(message "Refusing to call sawfish-client from sawfish.")
      (call-process sawfish-client nil target-buffer nil "-e"
		    expression)))))

(defun saw-client-raise (frame)
  "Tell sawfish to raise a frame."
  (let* ((xid (frame-parameter frame 'outer-window-id)))
    (when xid
      (saw-client-eval
       (concat "(display-window (get-window-by-id " xid "))")))))

;;}}}
;;{{{ advice

(ad-define-subr-args 'raise-frame '(&optional frame))
(defadvice raise-frame (after saw-client-raise-frame activate)
  "Raise frame in sawfish."
  (when window-system
    (saw-client-raise frame))
  ad-return-value)

(ad-define-subr-args 'display-buffer '(buffer-or-name
				       &optional not-this-window frame))
(defadvice display-buffer (after saw-client-display-buffer activate)
  "Raise selected frame in sawfish."
  (when window-system
    (let* ((current-window (get-buffer-window buffer-or-name 0))
           (current-frame (if current-window
                              (window-frame current-window)
                            nil)))
      (saw-client-raise frame)))
  ad-return-value)

(ad-define-subr-args 'lower-frame '(&optional frame))
(defadvice lower-frame (after saw-client-lower-frame activate)
  "Lower frame in sawfish."
  (when window-system
    (saw-client-lower frame))
  ad-return-value)

(ad-define-subr-args 'yes-or-no-p '(prompt))
(defadvice yes-or-no-p (before saw-client-yes-or-no-p activate)
  "Raise frame when asking in sawfish."
  (when window-system
    (saw-client-raise (selected-frame))))

(ad-define-subr-args 'y-or-n-p '(prompt))
(defadvice y-or-n-p (before saw-client-y-or-n-p activate)
  "Raise frame when asking in sawfish."
  (when window-system
    (saw-client-raise (selected-frame))))

;;}}}

(provide 'saw-client)
;;; saw-client.el ends here.
