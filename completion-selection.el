;;; completion-selection.el --- Completion mechanism selection for completion-ui
;;
;; Author: Henry G. Weller <hweller0@gmail.com>
;; Maintainer: Henry G. Weller
;;
;; Created: Thu Aug 7 18:42:56 2008 (+0100)
;; Version: 0.2
;; Last-Updated: Sat Apr 11 14:47:37 2009 (+0100)
;;           By: Henry Weller
;;     Update #: 2
;; URL: http://www.emacswiki.org/emacs/completion-selection.el
;; Keywords: completion, ui, user interface
;; Compatibility: GNU Emacs 23.x (may work with earlier versions)
;;
;; This file is NOT part of Emacs.
;;
;; ----------------------------------------------------------------------------
;;
;;; Commentary:
;;
;; A global minor mode that enables selection of completion methods.
;; It is a light wrapper around the `completion-ui' package which allows
;; the completion mechanism to be changed easily either via a keymap or menu
;; accessible from the menu-bar.
;;
;; The current completion mechanism is displayed hight-lighted in the mode-line
;; which can also be clicked on to pop-up the selection menu.
;;
;; This work relies on the good work of Toby Cubitt on completion-ui and also
;; his continued help and patience with this work -- thanks.
;;
;; ----------------------------------------------------------------------------
;;
;;; Change log:
;;
;; Version 0.1
;; * Initial release
;; Version 0.2
;; * Upgraded to work with completion-ui 0.11.?
;;
;; ----------------------------------------------------------------------------
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
;; ----------------------------------------------------------------------------
;;
;;; Code:

(provide 'completion-selection)

(require 'completion-ui)
(require 'completion-ui-more-sources)

;; ----------------------------------------------------------------------------
;;;  Customization variables

(defgroup completion-selection nil
  "Completion selection user interface."
  :group 'completion-ui
  :group 'convenience
  )

(defface completion-selection-face
  '((((type tty) (class color))
     (:background "blue"))
    (((type tty) (class mono))
     (:inverse-video t))
    (((type x w32 mac))
     (:foreground "blue")))
  "The face used for the completion selection in the mode-line."
  :group 'completion-selection
  )

(defcustom completion-selection-prefix "\C-zc"
  "Key prefix for completion type selection, pop-up menu etc."
  :type 'key-sequence
  :group 'completion-selection
  )

(defcustom completion-selection-change-prefix "\C-c"
  "Key prefix for changing the completion type selection."
  :type 'key-sequence
  :group 'completion-selection
  )

(defcustom completion-selection-setup-functions
  '(
    ("Dynamic word expansion"                 "Dabbrev"            "d"
     complete-dabbrev-ordered)
    ("BBDB Email address lookup"              "BBDB"               "b"
     complete-bbdb)
    ("File name expansion"                    "Files"              "f"
     complete-files)
    ("Ispell word lookup"                     "Lookup"             "l"
     complete-ispell-lookup)
    ("Ispell sub-string lookup"               "Lookup Sub-string"  "L"
     complete-ispell-lookup-substring)
    ("Org-mode keyword and macro completion"  "Org-mode"           "o"
     complete-org)
    ("Ispell word correction"                 "Spell"              "s"
     complete-ispell)
    ("Elisp symbol lookup"                    "Elisp"              "e"
     complete-elisp)
    ("Etags symbol lookup"                    "Etags"              "E"
     complete-etags)
    ("Pcomplete completion"                   "Pcomplete"          "p"
     complete-pcomplete)
    ("Semantic completion"                    "Semantic"           "S"
     complete-semantic)
    )
  "List of completion setup functions.
An entry in the list should have the forms

  \(DESCRIPTION MENU KEY FUNCTION)

The entry is considered active if FUNCTION is bound to a function.

When selecting the completion mechanism only the currently active entries
in this list are shown."
  :type '(repeat
          (list (string :tag "Description")
                (string :tag "Menu entry")
                (string :tag "Key")
                (choice (function :tag "Defined function")
                        (symbol  :tag "Undefined function"))))
  :group 'completion-selection
  )

(defcustom completion-selection-try-change t
  "Try changing the completion mechanism but revert to the previous
when finished."
  :type 'boolean
  :group 'completion-selection
  )

;; ----------------------------------------------------------------------------
;;;  Variables

(defvar completion-selection-function nil
  "Completion mechanism setup function
called by `completion-selection-set-complete' to setup the chosen
completion mechanism.")
(make-variable-buffer-local 'completion-selection-function)

(defvar completion-selection-name nil
  "Completion type name set by `completion-selection-function'")
(make-variable-buffer-local 'completion-selection-name)

(defun completion-selection-string ()
  "Returns the completion selection string for the mode line"
  (propertize
   (concat " " completion-selection-name)
   'face
   'completion-selection-face))

(defvar completion-selection-mode-map nil
  "Completion-selection control keymap.
Key bindings to control the completion type
Initialised when `completion-selection-mode' is first enabled")

(defvar completion-selection-menu-map nil
  "Completion-selection menu.
Initialised on first when `completion-selection-mode' is enabled")

(defvar completion-selection-select-menu-map nil
  "Completion-selection change menu.
Initialised on first when `completion-selection-mode' is enabled")

(defvar completion-selection-select-map nil
  "Keymap used when completion-selection change tooltip is displayed.")

(defvar completion-selection-tooltip-enable nil
  "Is tooltip pop-up enabled during completion mechanism change.")

(defvar completion-selection-tooltip-text nil
  "Completion-selection change tooltip.
Initialised on first when `completion-selection-mode' is enabled")

;; ----------------------------------------------------------------------------
;;;  Minor-mode definition

(define-minor-mode completion-selection-mode
  "A global minor mode that enables selection of completion methods.
It is a light wrapper around the `completion-ui' package which allows
the completion mechanism to be changed easily either via a keymap or menu
accessible from the menu-bar.

The current completion mechanism is displayed hight-lighted in the mode-line
which can also be clicked on to pop-up the selection menu.

Commands:
\\{completion-selection-mode-map}"
  :global nil
  :init-value nil
  :lighter (:eval (completion-selection-string))

  (cond
   (completion-selection-mode

    ;; Check that tooltip is supported and disable otherwise
    (setq completion-selection-tooltip-enable
          (and (eq completion-auto-show 'tooltip)
               window-system (fboundp 'tooltip-show)))

    ;; If the key-maps have not yet been created create them
    (unless completion-selection-mode-map
      (completion-selection-define-maps))

    ;; Add completion-selection-mode-map to minor-mode-map-alist
    ;; if it is not present
    (let ((existing (assq 'completion-selection-mode minor-mode-map-alist)))
      (if existing
          (setcdr existing completion-selection-mode-map)
        (add-to-list 'minor-mode-map-alist
                     (cons 'completion-selection-mode
                           completion-selection-mode-map))))

    (completion-selection-set 'complete-dabbrev-ordered))
   )
  )

;; ----------------------------------------------------------------------------
;;;  Initialisation Functions

(defun completion-selection-define-maps ()
  "Define `completion-selection-mode-map' and `completion-selection-menu-map"

  ;; Completion-selection key map
  (setq completion-selection-mode-map
        (let ((map (make-sparse-keymap "Completion-selection")))
          (define-key map [M-tab] 'completion-selection-complete)
          (define-key map (concat completion-selection-prefix "\C-m")
            'completion-selection-menu)
          map))

  ;; Completion-selection menu
  (easy-menu-define completion-selection-menu-map
    completion-selection-mode-map
    "Completion-selection"
    '((vector "C-S" :visible nil)
      "--"
      ["Pop-up Menu" completion-selection-menu]
      ["Help" (lambda () (interactive)
                (describe-function 'completion-selection-mode))]
      ["Turn Off Completion Select" completion-selection-mode :keys ""]
      ))

  ;; Completion-selection change menu
  (easy-menu-define completion-selection-select-menu-map
    completion-overlay-map
    "Completion-selection"
    '((vector "" :visible nil)))

  ;; Create the selection keymap
  (setq completion-selection-select-map
        (let ((map (make-sparse-keymap "Completion-selection-select")))
          (define-key map [S-down] 'completion-selection-select-tooltip)
          (define-key map [M-down] 'completion-selection-change-menu)
          map))

  ;; Reset the tooltip text
  (setq completion-selection-tooltip-text nil)

  (let (tooltip-str ;; String to hold a line of tooltip-text
        (menulen   ;; maximum menu item length
          (apply
           'max
           (mapcar (lambda (rec) (length (nth 1 rec)))
                   completion-selection-setup-functions))))

    ;; Add the active functions to the key map and menu
    (dolist (rec completion-selection-setup-functions)
      (let ((func (nth 3 rec)))
        (when (fboundp func)
          (let* ((desc (nth 0 rec))
                 (menu (nth 1 rec))
                 (key (nth 2 rec))
                 (prefixed-key (concat completion-selection-prefix key))
                 (change-key (concat completion-selection-change-prefix key))
                 (setup-func `(lambda ()
                                (interactive)
                                ,desc
                                (setq completion-selection-name ,menu)
                                (setq completion-selection-function ',func))))

            ;; Create a line of the tooltip text
            (setq tooltip-str
                  (concat menu (make-string (- menulen (length menu))? ) " "
                  (format "(%s)" (key-description key))))

            ;; Append the line to the tooltip text
            (setq completion-selection-tooltip-text
                  (concat completion-selection-tooltip-text tooltip-str "\n"))

            (define-key completion-selection-mode-map prefixed-key setup-func)
            (easy-menu-add-item
             completion-selection-menu-map nil
             (vector menu setup-func
                     :help desc
                     :keys (key-description prefixed-key)) "--")

            (define-key completion-selection-select-map key
              `(lambda ()
                 (interactive)
                 ,desc
                 (completion-selection-change ,setup-func)))

            (easy-menu-add-item
             completion-selection-select-menu-map nil
             (vector menu setup-func
                     :help desc
                     :keys (key-description key)))

            ;; Initialise completion-selection-function
            (unless completion-selection-function
              (funcall setup-func))
          )))))

  (define-key completion-overlay-map completion-selection-change-prefix
    'completion-selection-select)
  )

;; ----------------------------------------------------------------------------
;;;  Support Functions

(defun completion-selection-set (setup-function)
  "Set the completion mechanism for the given completion SETUP-FUNCTION."
  (dolist (rec completion-selection-setup-functions)
    (let ((menu (nth 1 rec))
          (func (nth 3 rec)))
      (when (and (fboundp func) (eq func setup-function))
        (setq completion-selection-name menu)
        (setq completion-selection-function func))))
  )

(defun completion-selection-set-complete (setup-function)
  "Cycle through available completions if there are any, otherwise set the
completion mechanism using SETUP-FUNCTION and complete the word at point."
  (if (completion-ui-overlay-at-point)
      (completion-cycle)
    (funcall setup-function)
    (complete-word-at-point))
  )

(defun completion-selection-change (selection-function)
  "Change to another completion mechanism during completion.
The new completion mechanism is chosen by SELECTION-FUNCTION and is only
used for the current completion if `completion-selection-try-change' is true
otherwise it becomes the buffer default completion mechanism."

  ;; First reject the current completion
  (completion-reject)

  ;; Cache the current completion mechanism
  (let ((current-cs-func completion-selection-function))
    (funcall selection-function)    ;; Change to the new completion mechanism
    (completion-selection-complete) ;; Generate new completions
    (when completion-selection-try-change
      ;; Change back to previous completion mechanism if required
      (completion-selection-set current-cs-func)))
  )

;; ----------------------------------------------------------------------------
;;;  Pop-up functions

(defun completion-selection-select-popup (&optional overlay)
  "Pop-up the selection tooltip or menu according to `completion-auto-show'."
  (interactive)

  ;; If no overlay supplied, try to find one at point
  (unless overlay (setq overlay (completion-ui-overlay-at-point)))

  (when overlay
    (cond
     ((eq completion-auto-show 'completion-show-tooltip)
      (completion-selection-select-tooltip overlay))
     ((eq completion-auto-show 'completion-show-menu)
      (completion-selection-change-menu overlay))))
  )

(defun completion-show-tooltip-text (text &optional overlay interactive)
  "Show given TEXT in a tooltip for the completion OVERLAY.
The point had better be within OVERLAY or you'll have bad luck
in all your flower-arranging endevours for fourteen years.

If OVERLAY is not supplied, try to find one at point. If
INTERACTIVE is supplied, pretend we were called interactively."
  (interactive)

  ;; if no overlay was supplied, try to find one at point
  (unless overlay (setq overlay (completion-ui-overlay-at-point)))
  ;; deactivate other auto-show interfaces
  (completion-ui-deactivate-auto-show-interface overlay)
  ;; if we can display a tooltip and there are completions to display in it...
  (when (and overlay (overlay-get overlay 'completions)
             window-system (fboundp 'x-show-tip))
    ;; if called manually, flag this in overlay property and call
    ;; auto-show-helpers, since they won't have been called by
    ;; `completion-ui-auto-show'
    (when (or (interactive-p) interactive)
      (overlay-put overlay 'completion-interactive-tooltip t)
      (completion-ui-call-auto-show-interface-helpers overlay))

    ;; calculate tooltip parameters
    (let ((mouse-pos (mouse-pixel-position))
          (pos (save-excursion
                 (goto-char (overlay-start overlay))
                 (completion-frame-posn-at-point)))
          (fg (face-attribute 'completion-tooltip-face :foreground))
          (bg (face-attribute 'completion-tooltip-face :background))
          (font (face-attribute 'completion-tooltip-face :family))
          params)

      ;; mouse position can be nil if mouse is outside Emacs frame in
      ;; certain window systems (e.g. windows); in this case, we move
      ;; mouse into frame (there's no way to restore its position
      ;; afterwards, since we can't find out its position)
      (unless (and (numberp (cadr mouse-pos))
                   (numberp (cddr mouse-pos)))
        (set-mouse-position (selected-frame) 1 0)
        (setq mouse-pos (mouse-pixel-position)))

      ;; set face and frame parameters
      (when (stringp fg)
        (setq params
              (tooltip-set-param params 'foreground-color fg))
        (setq params (tooltip-set-param params 'border-color fg)))
      (when (stringp bg)
        (setq params
              (tooltip-set-param params 'background-color bg)))
      (when (stringp font)
        (setq params (tooltip-set-param params 'font font)))
      (setq params
            (tooltip-set-param params 'internal-border-width 0))
      (setq params
            (tooltip-set-param params 'border-width 0))
      ;;      (setq params
      ;;            (tooltip-set-param
      ;;             params 'left
      ;;             (+ (car pos) completion-tooltip-x-offset)))
      ;;      (setq params
      ;;            (tooltip-set-param
      ;;             params 'top
      ;;             (+ (cdr pos) completion-tooltip-y-offset)))

      ;; make sure tooltip is cancelled before displaying it, otherwise
      ;; x-show-tip "magically" moves it to the top of the frame!
      (completion-cancel-tooltip)
      ;; show tooltip
      ;; Note: there's no reliable way to directly display a tooltip at the
      ;; *screen* position (which is what x-show-tip requires) of point, so we
      ;; use the kludge of calculating an offset from the mouse position and
      ;; displaying the tooltip relative to the mouse
      (x-show-tip text nil params completion-tooltip-timeout
                  (+ (- (car pos) (cadr mouse-pos))
                     (car completion-tooltip-offset))
                  (+ (- (cdr pos) (cddr mouse-pos)) (frame-char-height)
                     (cdr completion-tooltip-offset)))

      ;; set flag to indicate tooltip is active for this overlay (this should
      ;; enable tooltip-related key bindings, but doesn't when tooltip is
      ;; auto-displayed, so we also add them to overlay's keymap)
      (setq completion-tooltip-active overlay)
      )))

(defun completion-selection-select-tooltip (&optional overlay)
  "Show completion-selection keymap tooltip which shows the key-bindings for
the alternative completion mechanisms."
  (interactive)
  (completion-show-tooltip-text completion-selection-tooltip-text overlay)
  )

(defun completion-selection-menu ()
  "Change to another completion mechanism from a pop-up menu.
See `completion-selection-change' for more details."
  (interactive)
  (popup-menu completion-selection-select-menu-map))

(defun completion-selection-change-menu (&optional overlay)
  "Change to another completion mechanism during completion.
The new completion mechanism is chosen from a pop-up menu.
See `completion-selection-change' for more details."
  (interactive)

  ;; If no overlay supplied, try to find one at point
  (unless overlay (setq overlay (completion-ui-overlay-at-point)))

  (when overlay
    (let* ((start-pos (overlay-start overlay))
           (pos (save-excursion
                  (goto-char start-pos)
                  (completion-frame-posn-at-point))))
      (completion-selection-change
       (lambda ()
         (popup-menu
          completion-selection-select-menu-map
          (list
           (list
            (+ (car pos) (car completion-menu-offset))
            (+ (cdr pos) (cdr completion-menu-offset) (frame-char-height) 3))
           (selected-window)))))))
  )

;; ----------------------------------------------------------------------------
;;;  User Functions

(defun completion-selection-complete ()
  "Call `completion-selection-set-complete' for the currently selected
completion mechanism and cycle or complete word at point."
  (interactive)
  (completion-selection-set-complete completion-selection-function)
  )

(defun completion-selection-select (&optional overlay)
  "Select a new completion mechanism during completion either by key binding
 (with the optional display of a tooltip showing the bindings) or pop-up menu.

The choice of tooltip or pop-up is is made by `completion-auto-show' for
consistency with `completion-ui'.

Commands:
\\{completion-selection-select-map}"
  (interactive)

  ;; If no overlay supplied, try to find one at point
  (unless overlay (setq overlay (completion-ui-overlay-at-point)))

  (when overlay
    ;; Change the overlay map to the selection map by setting the parent key-map
    ;; and copying the key-bindings from the `completion-selection-select-map'
    (let ((map (copy-keymap completion-selection-select-map)))
      (overlay-put overlay 'keymap map)
      (set-keymap-parent map (if auto-completion-mode
                                 auto-completion-map
                               completion-overlay-map)))

    ;; Cancel any running timer so we don't end up being called twice
    (cancel-timer completion--auto-timer)

    (if completion-auto-show-delay
        (setq completion--auto-timer
              (run-with-timer completion-auto-show-delay nil
                              'completion-selection-select-popup overlay))
      (completion-selection-select-popup overlay)))
  )

;; ----------------------------------------------------------------------------
;;;  Completion-ui additional functions

(defun completion-cycle-complete ()
  "Cycle through available completions if there are any,\
 otherwise complete the word at point.

Note: this should be defined in `completion-ui'."
  (interactive)
  (if (completion-ui-overlay-at-point)
      (completion-cycle)
    (complete-word-at-point)))

;; ----------------------------------------------------------------------------
;;; completion-selection.el ends here
