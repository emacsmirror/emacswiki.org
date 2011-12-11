;;; org-cua-dwim.el --- Org-mode and Cua mode compatibility layer
;;
;; Filename: org-cua-dwim.el
;; Description: Org-mode and Cua mode compatibility layer
;; Author: Matthew L. Fidler
;; Maintainer: Matthew L. Fidler
;; Created: Thu Dec  8 15:06:13 2011 (-0600)
;; Version: 0.4
;; Last-Updated: Sun Dec 11 00:21:48 2011 (-0600)
;;           By: Matthew L. Fidler
;;     Update #: 186
;; URL: 
;; Keywords: org-mode cua-mode
;; Compatibility: 
;; 
;; Features that might be required by this library:
;;
;;   `help-fns'.
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
;; 11-Dec-2011    Matthew L. Fidler  
;;    Last-Updated: Sun Dec 11 00:20:36 2011 (-0600) #185 (Matthew L. Fidler)
;;    Another major bug fix.  It seems to work now.
;; 10-Dec-2011    Matthew L. Fidler  
;;    Last-Updated: Sat Dec 10 23:14:30 2011 (-0600) #133 (Matthew L. Fidler)
;;    Another Major bug fix when woring on a Mac.  Hopefully this works.
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
(defvar org-cua-dwim-debug nil)

(defadvice handle-shift-selection (around org-cua-dwim)
  (let ((is-org-mode (and (not (minibufferp))
                          (eq major-mode 'org-mode)))
        (do-it t))
    (setq org-cua-dwim-shift-translated this-command-keys-shift-translated)
    (when (and is-org-mode this-command-keys-shift-translated
               (not org-cua-dwim-was-move))
      (when org-cua-dwim-debug
        (message "Turn ON shift-select-mode"))
      (set (make-local-variable 'org-cua-dwim-was-move) t)
      (set (make-local-variable 'cua--last-region-shifted) t)
      (set (make-local-variable 'cua--explicit-region-start) nil)
      ;;(setq cua--prefix-override-timer nil)
      (set (make-local-variable 'shift-select-mode) t)
      (set (make-local-variable 'cua-mode) nil)
      )
    (when (and is-org-mode (not this-command-keys-shift-translated)
               org-cua-dwim-was-move)
      (when org-cua-dwim-debug
        (message "Turn Off shift-select-mode"))
      (set (make-local-variable 'shift-select-mode) nil)
      (set (make-local-variable 'cua-mode) t)
      (set (make-local-variable 'org-cua-dwim-was-move) nil)
      ;;(setq cua--prefix-override-timer nil)
      )
    (when do-it
      ad-do-it)
    (when mark-active
      (cua--select-keymaps))))

(defadvice cua--prefix-override-handler (around org-cua-dwim)
  "Try to fix the org copy and paste problem."
  (when (and (not (minibufferp)) (not cua-mode)
             (eq major-mode 'org-mode))
    (when org-cua-dwim-debug
      (message "Turn Off shift-select-mode"))
    (set (make-local-variable 'shift-select-mode) nil)
    (set (make-local-variable 'cua-mode) t)
    (set (make-local-variable 'org-cua-dwim-was-move) nil)
    (cua--pre-command-handler))
  ad-do-it)


(ad-deactivate 'handle-shift-selection)
(ad-activate   'handle-shift-selection)

(ad-deactivate 'cua--prefix-override-handler)
(ad-activate   'cua--prefix-override-handler)

(provide 'org-cua-dwim)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org-cua-dwim.el ends here
