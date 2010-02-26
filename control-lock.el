;;; control-lock.el --- Like caps-lock, but for your control key.  Give your pinky a rest!

;; Copyright (C) 2008 Craig Muth

;; Author: Craig Muth
;; Maintainer: Craig Muth
;; Created 10 November 2007
;; Version 1.1.2
;; Version Keywords: control key lock caps-lock

;;;; Commentary
;; Quick Start / installation:
;; 1. Download this file and put it next to other files emacs includes
;; 2. Add this to you .emacs file and restart emacs:
;;      (require 'control-lock)
;;      (control-lock-keys)
;; 3. Type C-z and follow the following steps
;;
;; Use Case:
;;   - Type C-z
;;     - The cursor changes to a red underscore, so you know control-lock is on
;;   - Type n to go to the next line
;;   - Type v to scroll down
;;   - Type xf to open a file
;;   - Type any other chars, and emacs will behave as though control is held down
;;   - Type z to exit control-lock
;;     - The cursor changes back to a box, so you know control-lock is off
;;
;; Input from commands:
;;   When in control lock and a command gets input from the minibuffer, control-lock
;;   doesn't interfere (i.e. chars are temporarily not converted to control chars).
;;
;; Inserting literal char:
;;   Pressing ' will temporarily turn control-lock off for the following key stroke.
;;   This is useful, for example, for typing 's to sort in dired mode.
;;
;; Todo
;;   Are there problems when using emacs in the terminal?
;;     - Changing the cursor to an underscore might not work
;;

;;; Change Log:
;; 2007-11-24 - Initial release
;; 2008-01-03 - Use C-z to enable/disable (C-, doesn't work in terminals)
;; 2008-01-22 - Holding down shift acts to disable control-lock

(defun control-lock-letter (l ch)
  "Called when keys are pressed.  If we deem control-lock to be
enabled, it returns the control-version of the key.  Otherwise
it just returns the key."
  (if (control-lock-enabled-p)
    ch l))

(defun control-lock-enabled-p ()
  "Returns whether control lock should be enabled at a given point"
  (and control-lock-mode-p
    ; If not disable once (turning off if set)
    (if control-lock-disable-once
      (progn
        (setq control-lock-disable-once nil)
        nil  ; Not enabled this time
        )
      t  ; It's enabled as far as we know here
      )
    (not isearch-mode)
    (not (string-match "\\*Minibuf" (buffer-name)))))

; Make ctrl-lock be off by default
(setq control-lock-mode-p nil)

(defun control-lock-quote (p)
  "Make ' disable ctrl-lock for next key"
  (if (control-lock-enabled-p)
    (progn
      (setq control-lock-disable-once t)
      "")
    "'"))
(setq control-lock-disable-once nil)
(define-key key-translation-map "'" 'control-lock-quote)

(defun control-lock-map-key (l ch fun)
  "Makes function to handle one key, and maps it to that key"
  (eval (read
    (concat
      "(progn"
        "(defun control-lock-" fun " (p) (control-lock-letter \"" l "\" (kbd \"" ch "\")))"
        "(define-key key-translation-map \"" l "\" 'control-lock-" fun "))"
      ")"
      ))))

; Map lowercase keys
(let ((c ?a) s)
  (while (<= c ?z)
    (setq s (char-to-string c))
    (control-lock-map-key s (concat "C-" s) s)
    (setq c (+ c 1))))

; Map uppercase keys to lowercase
(let ((c ?A) s)
  (while (<= c ?Z)
    (setq s (char-to-string c))
    (control-lock-map-key s (downcase s) s)
    (setq c (+ c 1))))

; Map numbers
(let ((c ?0) s)
  (while (<= c ?9)
    (setq s (char-to-string c))
    (control-lock-map-key s (concat "C-" s) s)
    (setq c (+ c 1))))


; Map misc keys
(control-lock-map-key "," "C-," "comma")
(control-lock-map-key "`" "C-`" "backtick")
(control-lock-map-key "\\t" "C-<tab>" "tab")
(control-lock-map-key "/" "C-/" "slash")
(control-lock-map-key " " "C-@" "space")
(control-lock-map-key "[" "C-[" "lsqrbracket")
(control-lock-map-key "\\\\" "C-\\\\" "backslash")
(control-lock-map-key ";" "C-;" "semicolon")
(control-lock-map-key "." "C-." "period")
(control-lock-map-key "=" "C-=" "equals")
(control-lock-map-key "-" "C--" "dash")
; Uncomment to remap <return>.  Useful for Proof General.
;(control-lock-map-key "\\r" "C-<return>" "return")


(defun control-lock-keys ()
  "Sets default keys - C-z enables control lock."
  (global-set-key (kbd "C-z") 'control-lock-enable)
  (global-set-key (kbd "C-,") 'control-lock-enable)
)


(defun control-lock-enable () (interactive)
  "Enable control lock.  This function should be mapped to the key the
user uses to enable control-lock."
  (if control-lock-mode-p
    (progn
      (setq control-lock-mode-p nil)
      ; Set cursor color back to orig
      (set-face-background 'cursor control-lock-orig-cursor-color)
      (customize-set-variable 'cursor-type 'box))
    ; Else
    (progn
      (setq control-lock-mode-p t)
      ; Save orig color and set to orange
      (setq control-lock-orig-cursor-color 
        (face-background 'cursor nil 'default))
      (set-face-background 'cursor "#ff3300")
      (customize-set-variable 'cursor-type '(hbar . 3)))))

(provide 'control-lock)
