;;; org-cua-dwim.el --- Org-mode and Cua mode compatibility layer
;;
;; Filename: org-cua-dwim.el
;; Description: Org-mode and Cua mode compatibility layer
;; Author: Matthew L. Fidler
;; Maintainer: Matthew L. Fidler
;; Created: Thu Dec  8 15:06:13 2011 (-0600)
;; Version: 0.5
;; Last-Updated: Sun Dec 11 10:06:44 2011 (-0600)
;;           By: Matthew L. Fidler
;;     Update #: 214
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
;;    Last-Updated: Sun Dec 11 10:01:49 2011 (-0600) #212 (Matthew L. Fidler)
;;    Turned on Delete-Selection-mode whe shift-selection-mode is enabled
;; 11-Dec-2011    Matthew L. Fidler  
;;    Last-Updated: Sun Dec 11 09:58:30 2011 (-0600) #205 (Matthew L. Fidler)
;;    Added more CUA advices.
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


;;;###autoload
(defun org-cua-dwim-turn-on-org-cua-mode-partial-support ()
  "This turns on org-mode cua-mode partial support; Assumes
shift-selection-mode is available."
  (interactive)
  (set (make-local-variable 'org-support-shift-select) t)
  (cua-mode 1)
  (add-hook 'pre-command-hook 'cua--pre-command-handler nil t)
  (add-hook 'post-command-hook 'cua--post-command-handler nil t)
  (set (make-local-variable 'cua-mode) t)
  (set (make-local-variable 'org-cua-dwim-was-move) nil)
  (set (make-local-variable 'shift-select-mode) nil))

;;;###autoload
(add-hook 'org-mode-hook 'org-cua-dwim-turn-on-org-cua-mode-partial-support)

(defvar org-cua-dwim-was-move nil)
(defvar org-cua-dwim-debug nil)
(defvar org-cua-dwim t)

(defadvice handle-shift-selection (around org-cua-dwim)
  (let ((is-org-mode (and (not (minibufferp))
                          (eq major-mode 'org-mode)))
        (do-it t))
    (setq org-cua-dwim-shift-translated this-command-keys-shift-translated)
    (when (and org-cua-dwim
               is-org-mode this-command-keys-shift-translated
               (not org-cua-dwim-was-move))
      (when org-cua-dwim-debug
        (message "Turn ON shift-select-mode & delete-selection-mode"))
      (delete-selection-mode 1)
      (set (make-local-variable 'org-cua-dwim-was-move) t)
      (set (make-local-variable 'cua--last-region-shifted) t)
      (set (make-local-variable 'cua--explicit-region-start) nil)
      (set (make-local-variable 'shift-select-mode) t)
      (set (make-local-variable 'cua-mode) nil))
    (when (and org-cua-dwim
               is-org-mode (not this-command-keys-shift-translated)
               org-cua-dwim-was-move)
      (when org-cua-dwim-debug
        (message "Turn Off shift-select-mode & delete-selection-mode"))
      (delete-selection-mode -1)
      (set (make-local-variable 'shift-select-mode) nil)
      (set (make-local-variable 'cua-mode) t)
      (set (make-local-variable 'org-cua-dwim-was-move) nil))
    (when do-it
      ad-do-it)
    (when (and org-cua-dwim
               is-org-mode
               mark-active)
      (cua--select-keymaps))))

(defmacro org-cua-dwim-fix-cua-command (cmd)
  "Defines advice for a CUA-command that will turn on CUA mode
before runnind ant hen run the `cua--precommand-handler'"
  `(progn
     (defadvice ,(intern cmd) (around org-cua-dwim)
     "Try to fix the org copy and paste problem."
     (when (and (not (minibufferp)) (not cua-mode)
                (eq major-mode 'org-mode))
       (when org-cua-dwim-debug
         (message "Turn Off shift-select-mode & delete-selection-mode  (CUA command)"))
       (delete-selection-mode -1)
       (set (make-local-variable 'shift-select-mode) nil)
       (set (make-local-variable 'cua-mode) t)
       (set (make-local-variable 'org-cua-dwim-was-move) nil)
       (cua--pre-command-handler))
     ad-do-it)
     (ad-activate ',(intern cmd))))

;; Advise all CUA commands active when selection is active
(org-cua-dwim-fix-cua-command "cua--prefix-override-handler")
(org-cua-dwim-fix-cua-command "cua-repeat-replace-region")
(org-cua-dwim-fix-cua-command "cua--shift-control-c-prefix")
(org-cua-dwim-fix-cua-command "cua--shift-control-x-prefix")
(org-cua-dwim-fix-cua-command "cua-toggle-rectangle-mark")
(org-cua-dwim-fix-cua-command "cua-delete-region")
(org-cua-dwim-fix-cua-command "cua-cut-region")
(org-cua-dwim-fix-cua-command "cua-copy-region")
(org-cua-dwim-fix-cua-command "cua-cancel")
(org-cua-dwim-fix-cua-command "cua-toggle-global-mark")
(org-cua-dwim-fix-cua-command "cua-paste")
(org-cua-dwim-fix-cua-command "cua-exchange-point-and-mark")
(org-cua-dwim-fix-cua-command "cua-scroll-down")
(org-cua-dwim-fix-cua-command "cua-scroll-up")
(org-cua-dwim-fix-cua-command "cua-set-mark")
(org-cua-dwim-fix-cua-command "cua-paste-pop")



(ad-activate 'handle-shift-selection)


(provide 'org-cua-dwim)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org-cua-dwim.el ends here
