;;; org-cua-dwim.el --- Org-mode and Cua mode compatibility layer
;; 
;; Filename: org-cua-dwim.el
;; Description: Org-mode and Cua mode compatibility layer
;; Author: Matthew L. Fidler
;; Maintainer: Matthew L. Fidler
;; Created: Thu Dec  8 15:06:13 2011 (-0600)
;; Version: 0.1
;; Last-Updated: Thu Dec  8 17:04:25 2011 (-0600)
;;           By: Matthew L. Fidler
;;     Update #: 7
;; URL: 
;; Keywords: org-mode cua-mode
;; Compatibility: 
;; 
;; Features that might be required by this library:
;;
;;   Cannot open load file: org-cua-dwim.
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
  (set (make-local-variable 'cua-mode) t)
  (set (make-local-variable 'shift-select-mode) t))

;;;###autoload
(add-hook 'org-mode-hook 'turn-on-org-cua-mode-partial-support)

(defadvice cua--pre-command-handler (around org-cua-dwim)
  (if (and (eq major-mode 'org-mode)
           (eq (get this-command 'CUA) 'move)
           (memq 'shift (event-modifiers
                         (aref (this-single-command-raw-keys) 0))))
      (progn
        (setq cua-mode t)
        (setq shift-select-mode t)
        (setq cua--last-region-shifted t)
        (setq cua--explicit-region-start nil))
    ad-do-it))
(defadvice cua--post-command-handler (around org-cua-dwim)
  (if (and (eq major-mode 'org-mode)
           (eq (get this-command 'CUA) 'move)
           (memq 'shift (event-modifiers
                         (aref (this-single-command-raw-keys) 0))))
      (progn
        (setq cua-mode t)
        (setq shift-select-mode t)
        (setq cua--last-region-shifted t)
        (setq cua--explicit-region-start nil))
    ad-do-it))
(ad-activate 'cua--pre-command-handler)
(ad-activate 'cua--post-command-handler)
(provide 'org-cua-dwim)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org-cua-dwim.el ends here
