;;; org-cua-dwim.el --- Org-mode and Cua mode compatibility layer
;; 
;; Filename: org-cua-dwim.el
;; Description: Org-mode and Cua mode compatibility layer
;; Author: Matthew L. Fidler
;; Maintainer: Matthew L. Fidler
;; Created: Thu Dec  8 15:06:13 2011 (-0600)
;; Version: 0.2
;; Last-Updated: Fri Dec  9 09:14:32 2011 (-0600)
;;           By: Matthew L. Fidler
;;     Update #: 70
;; URL: 
;; Keywords: org-mode cua-mode
;; Compatibility: 
;; 
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;;  To use this, just install via elpa or (require 'org-cua-dwim)
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;; 09-Dec-2011    Matthew L. Fidler  
;;    Last-Updated: Fri Dec  9 09:13:37 2011 (-0600) #69 (Matthew L. Fidler)
;;    Last time the system worked because an error removed the
;;    pre-command-hook. This time the system works without turning off
;;    the pre-command-hook.
;; 08-Dec-2011    Matthew L. Fidler  
;;    Last-Updated: Thu Dec  8 17:00:24 2011 (-0600) #6 (Matthew L. Fidler)
;;    Initial Release
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:
(setq org-support-shift-select t)
;;;###autoload
(defun turn-on-org-cua-mode-partial-support ()
  "This turns on org-mode cua-mode partial support; Assumes
shift-selection-mode is available."
  (interactive)
  (cua-mode 1)
  (add-hook 'pre-command-hook 'cua--pre-command-handler nil t)
  (add-hook 'post-command-hook 'cua--post-command-handler nil t)
  (set (make-local-variable 'cua-mode) t)
  (set (make-local-variable 'org-cua-dwim-was-move) nil)
  (set (make-local-variable 'shift-select-mode) nil))

;;;###autoload
(add-hook 'org-mode-hook 'turn-on-org-cua-mode-partial-support)

(defvar org-cua-dwim-was-move nil)

(defadvice handle-shift-selection (around org-cua-dwim)
  (let ((is-org-mode (and (not (minibufferp))
                          (eq major-mode 'org-mode)))
        (do-it t))
    (when (and is-org-mode this-command-keys-shift-translated
               (not org-cua-dwim-was-move))
      (setq shift-select-mode t)
      (setq cua-mode nil)
      (setq org-cua-dwim-was-move t)
      (setq cua--last-region-shifted t)
      (setq cua--explicit-region-start nil))
    (when (and is-org-mode (not this-command-keys-shift-translated)
               org-cua-dwim-was-move)
      (setq shift-select-mode nil)
      (setq cua-mode t)
      (setq org-cua-dwim-was-move nil))
    (when do-it
      ad-do-it)))

(ad-activate 'handle-shift-selection)
(provide 'org-cua-dwim)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org-cua-dwim.el ends here
