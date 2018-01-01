;;; doremi-mac.el --- A macro for defining Do Re Mi commands.
;;
;; Filename: doremi-mac.el
;; Description: A macro for defining Do Re Mi commands.
;; Author: Drew Adams
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 2004-2018, Drew Adams, all rights reserved.
;; Created: Tue Sep 14 16:45:30 2004
;; Version: 0
;; Package-Requires: ()
;; Last-Updated: Mon Jan  1 10:58:55 2018 (-0800)
;;           By: dradams
;;     Update #: 225
;; URL: https://www.emacswiki.org/emacs/download/doremi-mac.el
;; Doc URL: https://www.emacswiki.org/emacs/DoReMi
;; Keywords: extensions, convenience, keys, repeat, cycle
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x, 24.x, 25.x, 26.x
;;
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  A macro for defining Do Re Mi commands.
;;
;;  Defines a Do Re Mi command and adds it to a `Do Re Mi' menu-bar
;;  menu.  See library `doremi.el'.
;;
;;
;;; User options defined here:
;;
;;    `define-doremi-after-hook', `define-doremi-before-hook'.
;;
;;  Macro defined here:
;;
;;    `define-doremi'.
;;
;;
;;  Add this to your initialization file (~/.emacs or ~/_emacs):
;;
;;    (autoload 'define-doremi "doremi-mac"
;;      "Define a Do Re Mi command." nil 'macro)
;;
;;
;;  See also these other Do Re Mi libraries:
;;
;;    `doremi-frm.el' - Incrementally adjust frame properties.
;;    `doremi-cmd.el' - Other Do Re Mi commands.
;;
;;
;;  Example test commands defined using the macro.
;;
;;  1. Command `doremi-frame-height+' sets the frame height.
;;
;;     (define-doremi frame-height+
;;       "Set frame height, changing it incrementally."   ; Doc string
;;       "Set Frame Height"                        ; Command menu name
;;       (lambda (new-val)                         ; Setter function
;;         (set-frame-height (selected-frame) new-val) new-val)
;;       (frame-height (selected-frame)))          ; Initial value
;;
;;  2. Command `doremi-set-bg+' cycles through
;;     (x-defined-colors), setting the background color.
;;
;;     (define-doremi set-bg+
;;       ;; Doc string
;;       "Set background color, choosing from a list of all colors."
;;       "Set Background Color"                    ; Command menu name
;;       ;; Setter function
;;       (lambda (newval) (set-background-color newval) newval)
;;       ;; Initial value
;;       (frame-parameter (selected-frame) 'background-color)
;;       nil                                       ; Ignored
;;       (x-defined-colors)                        ; Cycle enumeration
;;       t)    ; Add current color to enumeration if not there already
;;
;;     Command `doremi-set-bg+' runs this hook after running `doremi':
;; (setq define-doremi-after-hook
;;       ;; Update the way faces display with new bg.
;;       (lambda () (frame-set-background-mode (selected-frame))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2006/01/06 dadams
;;     Corrected :group.
;; 2004/09/26 dadams
;;     Renamed do-re-mi* to doremi*.
;;     Prefixed everything here with doremi-.
;;     Wrapped eval-and-compile around menu definition.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
;;; User Options (Variables)

;;;###autoload
(defcustom define-doremi-before-hook nil
  "*Normal hook (list of functions) run before `doremi' is run.
See `run-hooks'."
  :type 'hook :group 'doremi)

;;;###autoload
(defcustom define-doremi-after-hook nil
  "*Normal hook (list of functions) run after `doremi' is run.
See `run-hooks'."
  :type 'hook :group 'doremi)
 
;;; Internal Variables

;; Define menu if not already defined.
(eval-and-compile
  (unless (boundp 'menu-bar-doremi-menu)
    (defvar menu-bar-doremi-menu (make-sparse-keymap "Do Re Mi"))
    (define-key global-map [menu-bar doremi]
      (cons "Do Re Mi" menu-bar-doremi-menu))))
 
;;; Macros

;;;###autoload
(defmacro define-doremi (cmd-name doc-string cmd-menu-name setter-fn init-val
                                  &optional grow-fn-p enum allow-new-p)
  "Define a Do Re Mi command.
CMD-NAME is the name of the command, to be prefixed by `doremi-'.
DOC-STRING is the documentation string for the new command.
CMD-MENU-NAME is the menu name for the command (a string).

The other arguments are as for command `doremi', except that the
`doremi' increment argument is not an argument to
`define-doremi'. The new command has a single, optional argument,
INCREMENT, provided interactively by the prefix argument."
  `(progn
     (defun ,(intern (concat "doremi-" (if (stringp cmd-name)
                                             cmd-name
                                           (symbol-name cmd-name))))
       (&optional increment)
       ,(concat doc-string "\nSee `doremi' for INCREMENT.")
       (interactive "p")
       (run-hooks 'define-doremi-before-hook)
       (doremi ,setter-fn ,init-val increment ,grow-fn-p ,enum ,allow-new-p)
       (run-hooks 'define-doremi-after-hook))
     (let ((cmd ',(intern (concat "doremi-" (if (stringp cmd-name)
                                                 cmd-name
                                               (symbol-name cmd-name))))))
       (define-key menu-bar-doremi-menu
         (make-vector 1 cmd)
         (cons ,cmd-menu-name cmd)))))
 
;;; Example Test Functions

;; (define-doremi frame-height+
;;   "Set frame height, changing it incrementally."         ; doc string
;;   "Set Frame Height"                                     ; command menu name
;;   (lambda (new-val)                                      ; setter function
;;     (set-frame-height (selected-frame) new-val) new-val)
;;   (frame-height (selected-frame)))                       ; initial value

;; (define-doremi set-bg+
;;   "Set background color, choosing from a list of all colors." ; doc string
;;   "Set Background Color"                                      ; command menu name
;;   (lambda (newval) (set-background-color newval) newval)      ; setter function
;;   (frame-parameter (selected-frame) 'background-color)        ; initial value
;;   nil                                                         ; ignored
;;   (x-defined-colors)                                          ; cycle enumeration
;;   t)                                                          ; add current color

;; ;; New command `doremi-set-bg+' runs this hook after running `doremi':
;; (setq define-doremi-after-hook
;;       (lambda () (frame-set-background-mode (selected-frame))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'doremi-mac)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; doremi-mac.el ends here
