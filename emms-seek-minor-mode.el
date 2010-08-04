;;; emacs-seek-minor-mode.el --- like emacs-volume-minor-mode except +/- seek

;; Copyright (C) 2006, 2007 Free Software Foundation, Inc.

;; Author: Martin Schoenmakers <aiviru@diamond-age.net>
;; Maintainer: Ævar Arnfjörð Bjarmason <avar@cpan.org>

;; This file is not a part of EMMS.

;; EMMS is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; EMMS is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with EMMS; see the file COPYING. If not, write to the
;; Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This code is all ripped from emms-volume.el with the following
;; changes:

;; 1. M-% -volume- -seek-
;; 2. M-% emms-volume-raise emms-volume-lower
;; 3. M-% emms-seek-forward emms-seek-backward
;; 4. M-% (provide 'emms-volume) (provide 'emms-seek-minor-mode)

;:: Code:

;; Code specific to the minor mode.
(define-minor-mode emms-seek-minor-mode
    "Allows volume setting with + and - after an initial key combo."
  :global t
  :init-value nil
  :lighter " (+/-)"
  :keymap '(("+" . emms-seek-mode-plus)
            ("-" . emms-seek-mode-minus)))

(defvar emms-seek-mode-timeout 2
  "*The timeout in amount of seconds used by `emms-seek-minor-mode'.")

(defvar emms-seek-mode-timer nil
  "The timer `emms-seek-minor-mode' uses.")

;;;###autoload
(defun emms-seek-mode-plus ()
  "Raise volume and enable or extend the `emms-seek-minor-mode' timeout."
  (interactive)
  (emms-seek-forward)
  (emms-seek-mode-start-or-extend))

;;;###autoload
(defun emms-seek-mode-minus ()
  "Lower volume and enable or extend the `emms-seek-minor-mode' timeout."
  (interactive)
  (emms-seek-backward)
  (emms-seek-mode-start-or-extend))

(defun emms-seek-mode-disable-timer ()
  "Disable `emms-seek-minor-mode' timer."
  (cancel-timer emms-seek-mode-timer)
  (setq emms-seek-mode-timer nil))

(defun emms-seek-mode-set-timer ()
  "Set a new `emms-seek-minor-mode' timer."
  (when emms-seek-mode-timer
    (emms-seek-mode-disable-timer))
  (setq emms-seek-mode-timer (run-at-time emms-seek-mode-timeout
                                          nil
                                          'emms-seek-mode-timer-timeout)))

(defun emms-seek-mode-timer-timeout ()
  "Function to disable `emms-seek-minor-mode' at timeout."
  (setq emms-seek-mode-timer nil)
  (emms-seek-minor-mode -1))

(defun emms-seek-mode-start-or-extend ()
  "Start `emms-seek-minor-mode' or extend its running time."
  (when (null emms-seek-minor-mode)
    (emms-seek-minor-mode 1))
  (emms-seek-mode-set-timer))

(provide 'emms-seek-minor-mode)
;;; emms-seek-minor-mode.el ends here
