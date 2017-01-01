;;; wid-edit+.el --- Extensions to standard library `wid-edit.el'.
;;
;; Filename: wid-edit+.el
;; Description: Extensions to standard library `wid-edit.el'.
;; Author: Drew Adams, Lennart Borgman
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 2007-2017, Drew Adams, all rights reserved.
;; Created: Fri Dec 21 10:25:32 2007
;; Version: 0
;; Package-Requires: ()
;; Last-Updated: Sun Jan  1 11:58:24 2017 (-0800)
;;           By: dradams
;;     Update #: 271
;; URL: http://www.emacswiki.org/wid-edit%2b.el
;; Doc URL: http://emacswiki.org/UseCustomizeForKeyBindings
;; Keywords: widget, color
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x, 24.x, 25.x
;;
;; Features that might be required by this library:
;;
;;   `wid-edit', `widget'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  Extensions to standard library `wid-edit.el'.
;;
;;  New widgets defined here:
;;
;;    `conditional-key-definition', `key-definition'.
;;
;;  Functions defined here:
;;
;;    `widgetp-color-match', `widgetp-color-validate',
;;    `widgetp-define-key-from-key-def',
;;    `widgetp-display-Completions', `widgetp-keyboard-quit',
;;    `widgetp-key-def-set', `widgetp-remove-Completions',
;;    `widgetp-rgb-hex-string-p'.
;;
;;
;;  ***** NOTE: The following widgets defined in `wid-edit.el'
;;              have been REDEFINED HERE:
;;
;;  `color' - Added :match and :validate values.
;;            Increased :size value to max color name length.
;;
;;  `editable-field' - `C-g' also removes *Completions*.
;;
;;
;;  ***** NOTE: The following functions defined in `wid-edit.el' have
;;              been REDEFINED HERE:
;;
;;  `widget-color-complete' - Made compatible with Emacs 20.
;;                            Don't use `facemenu-color-alist'.
;;                            Delete *Completions* window when done.
;;                            Keep focus in Customize frame.
;;
;;  `widget-color-notify'   - Remove *Completions* if color changes.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2012/03/20 dadams
;;     editable-field: Updated for Emacs 24: added :value-set entry.
;; 2008/05/11 dadams
;;     Added: (conditional-)key-definition, widgetp-define-key-from-key-def,
;;            widgetp-key-def-set.
;; 2008/03/28 dadams
;;     widgetp-color-match: Ensure string and downcase before member test.
;; 2007/12/26 dadams
;;     Redefined: widget-color-complete: show/remove Completions.
;;     Redefined: editable-field: Bind C-g to also remove Completions.
;;     Redefined: widget-color-notify: Remove Completions if color changes.
;;     Redefined: widget-color-complete: Remove Completions if color changes.
;;                                       Show Completions if complete prefix.
;;     Added: widgetp-display-Completions, widgetp-remove-Completions,
;;            widgetp-keyboard-quit.
;;     Removed: widgetp-max-color-name-length.
;; 2007/12/22 dadams
;;     Created.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
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

(require 'wid-edit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
 

;;; `editable-field' widget

;;;###autoload
(defun widgetp-keyboard-quit ()
  "Same as `keyboard-quit', but also removes *Completions* window."
  (interactive)
  (widgetp-remove-Completions)
  (keyboard-quit))

(define-key widget-field-keymap "\C-g" 'widgetp-keyboard-quit)


;;; REPLACE ORIGINAL `editable-field' in `wid-edit.el'.
;;; No change from standard definition.
;;; Redefine it just to pick up the keymap change (`C-g').
;;;
(define-widget 'editable-field 'default
  "An editable-text field widget.
In an `editable-field' widget, `%v' must be preceded by some other
text in the `:format' string (if specified)."
  :convert-widget 'widget-value-convert-widget
  :keymap widget-field-keymap
  :format "%v"
  :help-echo "M-TAB: complete field; RET: enter value"
  :value ""
  :prompt-internal 'widget-field-prompt-internal
  :prompt-history 'widget-field-history
  :prompt-value 'widget-field-prompt-value
  :action 'widget-field-action
  :validate 'widget-field-validate
  :valid-regexp ""
  :error "Field's value doesn't match allowed forms"
  :value-create 'widget-field-value-create
  :value-set (if (fboundp 'widget-field-value-set)
                 'widget-field-value-set
               'widget-default-value-set)
  :value-delete 'widget-field-value-delete
  :value-get 'widget-field-value-get
  :match 'widget-field-match)
 

;;; `color' widget

(defun widgetp-color-match (widget value)
  ":match function for `color' widget."
  (and (stringp value)
       (or (member (downcase value)
                   (mapcar #'downcase (x-defined-colors)))
           (widgetp-rgb-hex-string-p value))))

(defun widgetp-color-validate (widget)
  ":validate function for `color' widget.
Return nil if color validates, or WIDGET otherwise."
  (let ((value (widget-value widget)))
    (unless (widgetp-color-match widget value)
      (widget-put widget :error (format "Invalid color: %S" value))
      widget)))


;;; REPLACE ORIGINAL `widget-color-notify' in `wid-edit.el'.
;;; Remove *Completions* window if color changes.
;;;
(defun widget-color-notify (widget child &optional event)
  "Update WIDGET's sample, and notify its PARENT."
  (let* ((ovly (widget-get widget :sample-overlay))
         (old-color (overlay-get ovly 'face))
         (new-color (widget-apply widget :sample-face-get)))
    (unless (equal old-color new-color) (widgetp-remove-Completions))
    (overlay-put ovly 'face new-color))
  (widget-default-notify widget child event))


;;; REPLACE ORIGINAL `widget-color-complete' in `wid-edit.el'.
;;; Make compatible with older Emacs than 22 (`x-defined-colors' etc.)
;;; Don't use `facemenu-color-alist' or load `facemenu'.
;;; Delete *Completions* window when appropriate.
;;; Keep focus in Customize frame (not *Completions* frame).
;;;
(defun widget-color-complete (widget)
  "Complete the color value in `color' widget WIDGET."
  (let* ((prefix (buffer-substring-no-properties
                  (widget-field-start widget) (point)))
         (colors (mapcar #'list (x-defined-colors)))
         (completion (try-completion prefix colors)))
    (cond ((eq completion t)
           (widgetp-remove-Completions)
           (message "Sole completion"))
          ((null completion)
           (widgetp-remove-Completions)
           (error "No completion for \"%s\"" prefix))
          ((not (string-equal prefix completion))
           (insert-and-inherit (substring completion (length prefix)))
           (message "Making completion list...")
           (widgetp-display-Completions prefix colors)
           (message "Complete but not unique"))
          (t
           (message "Making completion list...")
           (widgetp-display-Completions prefix colors)
           (message "Complete but not unique")))))

(defun widgetp-rgb-hex-string-p (string)
  "Return non-nil if STRING is an RGB hex string #XXXXXXXXXXXX.
Each X is a hexadecimal digit.  The number of Xs must be a multiple of
three, with the same number of Xs for each of red, green, and blue."
  (save-match-data
    (string-match "^#\\([a-fA-F0-9][a-fA-F0-9][a-fA-F0-9]\\)+$" string)))


;;; REPLACE ORIGINAL `color' widget in `wid-edit.el'.
;;; Base :size on longest color name.
;;; Add :match and :validate.
;;;
(define-widget 'color 'editable-field
  "A color widget (with sample)."
  :format   "%{%t%}: %v (%{sample%})\n"
  :size     (1+ (apply #'max (mapcar #'length (x-defined-colors))))
  :tag      "Color"
  :match    'widgetp-color-match
  :validate 'widgetp-color-validate
  :value    "black"
  :complete 'widget-color-complete
  :sample-face-get 'widget-color-sample-face-get
  :notify   'widget-color-notify
  :action   'widget-color-action)
 

;;; Key definition widgets

(define-widget 'conditional-key-definition 'lazy
  "Conditional key definition.
A list of four components: KEYMAP, KEY, COMMAND, CONDITION, that
represents a binding of command COMMAND in keymap KEYMAP according to
KEY, if CONDITION evaluates to non-nil.

KEY is either a key sequence (string or vector) or a command.
COMMAND is a command.
CONDITION is a sexp.

If KEY is a command, then the binding represented is its remapping to
COMMAND."
  :tag "Key Definition" :indent 1 :offset 0
  :type
  '(list
    (restricted-sexp :tag "Keymap"
     :match-alternatives ((lambda (x) (keymapp (eval x))))
     :value global-map)
    (choice
     (key-sequence :tag "Key" :value [ignore])
     (restricted-sexp :tag "Command to remap"
      :match-alternatives (commandp) :value ignore))
    (restricted-sexp :tag "Command"
     :match-alternatives (commandp) :value ignore)
    (sexp :tag "Condition")))

(define-widget 'key-definition 'lazy
  "Key definition.
A list of three components: KEYMAP, KEY, COMMAND, that represents a
binding of command COMMAND in keymap KEYMAP according to KEY.
KEY is either a key sequence (string or vector) or a command.
If KEY is a command, then the binding represented is its remapping to
command COMMAND."
  :tag "Key Definition" :indent 1 :offset 0
  :type
  '(list
    (restricted-sexp :tag "Keymap"
     :match-alternatives ((lambda (x) (keymapp (eval x))))
     :value global-map)
    (choice
     (key-sequence :tag "Key" :value [ignore])
     (restricted-sexp :tag "Command to remap"
      :match-alternatives (commandp) :value ignore))
    (restricted-sexp :tag "Command"
     :match-alternatives (commandp) :value ignore)))

;;; Functions useful in :set for key-definition `defcustom's.

(defun widgetp-define-key-from-key-def (key-def)
  "Define a key using a key-definition data structure
Argument KEY-DEF is a list of type `key-definition' or
`conditional-key-definition'."
  (let ((map (eval (car key-def)))
        (key (cadr key-def))
        (command (caddr key-def))
        (condition (cadddr key-def)))
    (when (or (not condition) (eval condition))
      (if (symbolp key)
          (define-key map (vector 'remap key) command)
        (define-key map key command)))))

(defun widgetp-key-def-set (var key-def)
  "Set function for types `key-definition', `conditional-key-definition'."
  (custom-set-default var key-def)
  (define-key-from-key-def key-def))
 

;;; Helper functions

(defun widgetp-remove-Completions ()
  "Remove *Completions* window, if shown."
  (when (get-buffer-window "*Completions*" 0)
    (delete-window (get-buffer-window "*Completions*" 0))))

(defun widgetp-display-Completions (string table)
  "Display matches of STRING against TABLE in *Completions* window.
Arguments are as for `all-completions'."
  (let ((orig-frame (selected-frame)))
    (with-output-to-temp-buffer "*Completions*"
      (display-completion-list (all-completions string table nil)))
    (select-frame-set-input-focus orig-frame)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'wid-edit+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; wid-edit+.el ends here
