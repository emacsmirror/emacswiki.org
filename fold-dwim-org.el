;;; fold-dwim-org.el --- Fold DWIM bound to org key-strokes.
;; 
;; Filename: fold-dwim-org.el
;; Description: 
;; Author: Matthew L. Fidler & Shane Celis
;; Maintainer: Matthew L. Fidler
;; Created: Tue Oct  5 12:19:45 2010 (-0500)
;; Version: 
;;Last-Updated: Mon Nov 15 11:30:32 2010 (-0600)
;;           By: Matthew L. Fidler
;;     Update #: 92
;; URL: 
;; Keywords: 
;; Compatibility: 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; Modification of hideshow-org which is located originally at:
;;
;; git clone git://github.com/secelis/hideshow-org.git
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Change Log:
;; 15-Nov-2010    Matthew L. Fidler  
;;    Last-Updated: Mon Oct 25 10:57:19 2010 (-0500) #33 (Matthew L. Fidler) #91 (Matthew L. Fidler)
;;    Bug fix -- make sure to save excursion.
;; 05-Nov-2010      
;;    Last-Updated: Mon Oct 25 10:57:19 2010 (-0500) #33 (Matthew L. Fidler) #87 (US041375)
;;    Will not hide when there is a region selected.
;; 02-Nov-2010    Matthew L. Fidler  
;;    Last-Updated: Tue Nov  2 10:15:01 2010 (-0500) #79 (Matthew L. Fidler)
;;    Made post-command-hook enclosed in condition-case
;; 28-Oct-2010    Matthew L. Fidler  
;;    Last-Updated: Thu Oct 28 15:42:44 2010 (-0500) #77 (Matthew L. Fidler)
;;    Do not fold while expanding a yasnippet.
;; 25-Oct-2010    Matthew L. Fidler  
;;    Last-Updated: Mon Oct 25 12:27:52 2010 (-0500) #75 (Matthew L. Fidler)
;;    Removed string check
;; 25-Oct-2010    Matthew L. Fidler  
;;    Last-Updated: Mon Oct 25 11:35:23 2010 (-0500) #72 (Matthew L. Fidler)
;;    Changed symbol
;; 25-Oct-2010    Matthew L. Fidler  
;;    Last-Updated: Mon Oct 25 11:25:15 2010 (-0500) #70 (Matthew L. Fidler)
;;    Added check based on last point is equal to current point and current line is equal to what is was before.
;; 25-Oct-2010    Matthew L. Fidler  
;;    Last-Updated: Mon Oct 25 11:18:43 2010 (-0500) #62 (Matthew L. Fidler)
;;    Added interface to allow pre and post command hooks instead of overwriting the definition of [TAB].  Doesn't mess with as many key bindings...
;; 25-Oct-2010    Matthew L. Fidler  
;;    Last-Updated: Mon Oct 25 10:57:19 2010 (-0500) #33 (Matthew L. Fidler)
;;    Added indent-for-tab-command when key is undefined...
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
;;(setq debug-on-error 't)




(require 'fold-dwim)
(setq fold-dwim-org/trigger-keys-block nil)
(defvar fold-dwim-org/trigger-keys-block nil;(list (kbd "TAB"))
  "The keys to bind to toggle block visibility.")

(defvar fold-dwim-org/trigger-keys-all (list [S-tab] [S-iso-lefttab] [(shift tab)] [backtab])
  "The keys to bind to toggle all block visibility.")

(defvar fold-dwim-org/minor-mode-map nil
  "The keymap of fold-dwim-org/minor-mode")

(defvar fold-dwim-org/hide-show-all-next nil
  "Keeps the state of how the buffer was last toggled by Shift TABing.")


(unless fold-dwim-org/minor-mode-map
  (setq fold-dwim-org/minor-mode-map (make-sparse-keymap)))

(dolist (var '(fold-dwim-org/minor-mode
               fold-dwim-org/hide-show-all-next
               ))
  (make-variable-buffer-local var))

(defmacro fold-dwim-org/define-keys ()
  `(progn 
     ,@(when fold-dwim-org/trigger-keys-block (mapcar (lambda (key) `(fold-dwim-org/define-key ,key fold-dwim-org/toggle)) fold-dwim-org/trigger-keys-block))
     ,@(mapcar (lambda (key) `(fold-dwim-org/define-key ,key fold-dwim-org/hideshow-all)) fold-dwim-org/trigger-keys-all)
     ))
(defvar fold-dwim-org/last-point nil
  )
(defvar fold-dwim-org/last-txt nil)
(defun fold-dwim-org/should-fold (last-point current-point)
  "* Checks to see if buffer has changed.  If not folding should occur."
  (equal last-point current-point)
  )
(defvar fold-dwim-org/mark-active nil)
(make-variable-buffer-local 'fold-dwim-org/mark-active)
(defun fold-dwim-org/hs-pre ()
  "* Pre-command hook to save last point.  Only used if `fold-dwim-org/trigger-keys-block' is nil"
  (when fold-dwim-org/minor-mode
    (unless fold-dwim-org/trigger-keys-block
      (unless (minibufferp)
        (setq fold-dwim-org/mark-active mark-active)
        (setq fold-dwim-org/last-point (point))
        (setq fold-dwim-org/last-txt (buffer-substring (point-at-bol) (point-at-eol)))
        )
      )
    )
  )
(defun fold-dwim-org/hs-post ()
  "* Post-command hook to hide/show if `fold-dwim-org/trigger-keys-block' is nil"
  (condition-case error
      (progn
        (when fold-dwim-org/minor-mode
          (unless fold-dwim-org/trigger-keys-block
            (unless (minibufferp)
              (unless fold-dwim-org/mark-active
                (when (eq ?\t last-command-event)
                                        ;          (unless (string= fold-dwim-org/last-txt
                                        ;                           (buffer-substring (point-at-bol) (point-at-eol)))
                  (unless (and (fboundp 'yas/snippets-at-point)
                               (< 0 (length (yas/snippets-at-point 'all-snippets)))
                               )
                    (fold-dwim-org/toggle nil fold-dwim-org/last-point)
                    )
                  
                                        ;            )
                  )
                )
              )
            )
          )
        )
    (error
     (message "HS Org post-command hook error: %s" (error-message-string error)))
    )
  )
(add-hook 'post-command-hook 'fold-dwim-org/hs-post)
(add-hook 'pre-command-hook 'fold-dwim-org/hs-pre)
;; No closures is killing me!
(defmacro fold-dwim-org/define-key (key function)
  `(define-key fold-dwim-org/minor-mode-map ,key (lambda () (interactive)
                                                   (,function ,key))))

(define-minor-mode fold-dwim-org/minor-mode
  "Toggle fold-dwim-org minor mode.
With no argument, this command toggles the mode.
positive prefix argument turns on the mode.
Negative prefix argument turns off the mode.

When fold-dwim-org minor mode is enabled, the TAB key toggles the
visible state of the code, and shift TAB toggles the visible
state of the entire file.

You can customize the key through `fold-dwim-org/trigger-key-block'."
  ;; The initial value.
  nil
  ;; The indicator for the mode line.  Nothing.
  ""
  :group 'editing

  (fold-dwim-org/define-keys)
  ;; We want hs-minor-mode on when fold-dwim-org/minor-mode is on.
  (let (
        (hs (assoc 'hs-minor-mode minor-mode-alist))
        )
    (when hs
      (setq hs (cdr hs))
      (if fold-dwim-org/minor-mode
          (setcar hs (replace-regexp-in-string "[*]*$" "*" (car hs)))
        (setcar hs (replace-regexp-in-string "[*]+$" "" (car hs)))
        )
      )
    ;; TODO add indicators in other modes.
    )
  )

(defun fold-dwim-org/toggle (&optional key lst-point)
  "Hide or show a block."
  (interactive)
  (save-excursion
    (let* ((last-point (or lst-point (point)))
           (fold-dwim-org/minor-mode nil)
           (command (if key (key-binding key) nil))
           (other-keys fold-dwim-org/trigger-keys-block))
      (unless command
        (setq command 'indent-for-tab-command)
        )
      (while (and (null command)
                  (not (null other-keys)))
        (setq command (key-binding (car other-keys)))
        (setq other-keys (cdr other-keys)))
      (unless lst-point
        (if (commandp command)
            (call-interactively command)
          )
        )
      (when (fold-dwim-org/should-fold last-point (point))
        (fold-dwim-toggle)
        ))))

(defun fold-dwim-org/hideshow-all (&optional key)
  "Hide or show all blocks."
  (interactive)
  (let* ((last-point (point))
         (fold-dwim-org/minor-mode nil)
         (command (key-binding key))
         (other-keys fold-dwim-org/trigger-keys-all))
    (while (and (null command)
                (not (null other-keys)))
      (setq command (key-binding (car other-keys)))
      (setq other-keys (cdr other-keys)))
    (when (commandp command)
      (call-interactively command))
    (when (equal last-point (point))
      (if fold-dwim-org/hide-show-all-next
          (fold-dwim-show-all)
        (fold-dwim-hide-all))
      (setq fold-dwim-org/hide-show-all-next (not fold-dwim-org/hide-show-all-next)))))

(provide 'fold-dwim-org)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fold-dwim-org.el ends here
