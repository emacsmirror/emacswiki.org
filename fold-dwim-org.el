;;; fold-dwim-org.el --- Fold DWIM bound to org key-strokes.
;; 
;; Filename: fold-dwim-org.el
;; Description: 
;; Author: Matthew L. Fidler & Shane Celis
;; Maintainer: Matthew L. Fidler
;; Created: Tue Oct  5 12:19:45 2010 (-0500)
;; Version: 
;; Last-Updated: Tue Oct  5 12:57:21 2010 (-0500)
;;           By: Matthew L. Fidler
;;     Update #: 17
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
;;; Change log:
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
(setq debug-on-error 't)
(require 'fold-dwim)

(defvar fold-dwim-org/trigger-keys-block (list (kbd "TAB"))
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
     ,@(mapcar (lambda (key) `(fold-dwim-org/define-key ,key fold-dwim-org/toggle)) fold-dwim-org/trigger-keys-block)
     ,@(mapcar (lambda (key) `(fold-dwim-org/define-key ,key fold-dwim-org/hideshow-all)) fold-dwim-org/trigger-keys-all)
    ))

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
          (setcar hs (replace-regexp-in-string "!*$" "!" (car hs)))
        (setcar hs (replace-regexp-in-string "!+$" "" (car hs)))
        )
      )
    ;; TODO add indicators in other modes.
    )
  )

(defun fold-dwim-org/toggle (&optional key)
  "Hide or show a block."
  (interactive)
  (let* ((last-point (point))
         (fold-dwim-org/minor-mode nil)
         (command (key-binding key))
         (other-keys fold-dwim-org/trigger-keys-block))
    (while (and (null command)
                (not (null other-keys)))
      (setq command (key-binding (car other-keys)))
      (setq other-keys (cdr other-keys)))
    (when (commandp command)
      (call-interactively command))
    (when (equal last-point (point))
      (fold-dwim-toggle)
      )))

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
