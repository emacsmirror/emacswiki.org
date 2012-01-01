;;; bindings+.el --- Enhancements to standard library `bindings.el'.
;; 
;; Filename: bindings+.el
;; Description: Enhancements to standard library `bindings.el'.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2007-2012, Drew Adams, all rights reserved.
;; Created: Thu Oct 04 10:54:38 2007
;; Version: 22.0
;; Last-Updated: Sun Jan  1 14:29:13 2012 (-0800)
;;           By: dradams
;;     Update #: 158
;; URL: http://www.emacswiki.org/cgi-bin/wiki/bindings+.el
;; Keywords: 
;; Compatibility: GNU Emacs 22.x, GNU Emacs 23.x
;; 
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;;  This lets you click `mouse-1' on a minor-mode lighter in the
;;  mode-line, to pop up the corresponding minor-mode menu, if there
;;  is one.
;;
;;  This is equivalent to what `mouse-1' does when you click the
;;  major-mode lighter.  Each minor-mode lighter brings up its own
;;  menu. If a minor mode has no menu-bar menu, then `mouse-1'
;;  displays a message stating that.
;;
;;  Example: Minor mode Icicles has lighter "Icy" in the mode line,
;;  and it has a menu-bar menu `Icicles'. Whether or not the menu-bar
;;  is showing, if you click the lighter "Icy" then the `Icicles' menu
;;  pops up.
;;
;;  Emacs 23+:
;;
;;   I added the functionality provided here to vanilla GNU Emacs, so
;;   this library is not strictly needed for Emacs 23.  The code here
;;   just provides slightly better feedback than the vanilla version.
;;
;;  To use this library, put the following in your init file
;;  (~/.emacs):
;;
;;    (require 'bindings+)
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;;
;; 2009/06/11 dadams
;;     Added Emacs 23 version of mode-line definition.
;;     Conditionalize stuff that is not needed for Emacs 23.
;; 2008/02/20 dadams
;;     minor-mode-menu-from-indicator: Create a simple menu if there is none, instead of error.
;; 2007/10/04 dadams
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

;; Quiet the byte-compiler
(defvar mode-line-column-line-number-mode-map)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
(unless (> emacs-major-version 22)
  (setq mode-line-minor-mode-keymap
        (let ((map (make-sparse-keymap)))
          (define-key map [mode-line down-mouse-1] 'mouse-minor-mode-menu)
          (define-key map [mode-line mouse-2] 'mode-line-minor-mode-help)
          (define-key map [mode-line down-mouse-3] 'mode-line-mode-menu-1)
          (define-key map [header-line down-mouse-3] 'mode-line-mode-menu-1)
          map))

  (defun mouse-minor-mode-menu (event)
    "Show minor-mode menu for EVENT on minor modes area of the mode line."
    (interactive "@e")
    (let ((indicator (car (nth 4 (car (cdr event))))))
      (minor-mode-menu-from-indicator indicator)))

  (defun minor-mode-menu-from-indicator (indicator) ; e.g. " Icy"
    "Show menu for minor mode specified by INDICATOR.
Interactively, INDICATOR is read using completion.
If there is no menu defined for the minor mode, then create one with
items `Turn Off' and `Help'."
    (interactive (list (completing-read "Minor mode indicator: "
                                        (describe-minor-mode-completion-table-for-indicator))))
    (let ((minor-mode (lookup-minor-mode-from-indicator indicator)))
      (unless minor-mode (error "Cannot find minor mode for `%s'" indicator))
      (let* ((map (cdr-safe (assq minor-mode minor-mode-map-alist)))
             (menu (and (keymapp map) (lookup-key map [menu-bar]))))
        (if menu
            (popup-menu menu)
          (read-event)                  ; Swallow the mouse up event.
          (setq menu `(keymap
                       (,(intern indicator) ,indicator
                         keymap
                         (turn-off menu-item "Turn Off"
                                   (lambda ()
                                     (interactive)
                                     (,minor-mode -1)
                                     (message ,(format "`%S' turned OFF" minor-mode))
                                     (sleep-for 1)))
                         (help menu-item "Help"
                               (lambda () (interactive) (describe-function ',minor-mode))))))
          (popup-menu menu))))))

(unless (> emacs-major-version 22)
  (let* ((help-echo
          "mouse-1: Select (drag to resize), mouse-2: \
Delete others, mouse-3: Delete this")
         (dashes (propertize "--" 'help-echo help-echo))
         (standard-mode-line-format
          (list "%e" (propertize "-" 'help-echo help-echo) 'mode-line-mule-info
                'mode-line-client 'mode-line-modified 'mode-line-remote
                'mode-line-frame-identification 'mode-line-buffer-identification
                (propertize "   " 'help-echo help-echo) 'mode-line-position '(vc-mode vc-mode)
                (propertize "  " 'help-echo help-echo) 'mode-line-modes
                `(which-func-mode ("" which-func-format ,dashes))
                `(global-mode-string (,dashes global-mode-string))
                (propertize "-%-" 'help-echo help-echo)))
         (standard-mode-line-modes
          (list (propertize "%[(" 'help-echo help-echo)
                `(:propertize ("" mode-name)
                              help-echo "mouse-1: major-mode menu, mouse-2: major-mode help, \
mouse-3: toggle minor modes"
                              mouse-face mode-line-highlight
                              local-map ,mode-line-major-mode-keymap)
                '("" mode-line-process)
                `(:propertize ("" minor-mode-alist)
                              mouse-face mode-line-highlight
                              help-echo "mouse-1: minor-mode menu, mouse-2: minor-mode help, \
mouse-3: toggle minor modes"
                              local-map ,mode-line-minor-mode-keymap)
                (propertize "%n" 'help-echo "mouse-2: widen"
                            'mouse-face 'mode-line-highlight
                            'local-map (make-mode-line-mouse-map
                                        'mouse-2 #'mode-line-widen))
                (propertize ")%]--" 'help-echo help-echo)))
         (standard-mode-line-position `((-3 ,(propertize "%p" 'help-echo help-echo))
                                        (size-indication-mode
                                         (8 ,(propertize " of %I" 'help-echo help-echo)))
                                        (line-number-mode
                                         ((column-number-mode
                                           (10 ,(propertize " (%l,%c)" 'help-echo help-echo))
                                           (6 ,(propertize " L%l" 'help-echo help-echo))))
                                         ((column-number-mode
                                           (5 ,(propertize " C%c" 'help-echo help-echo))))))))
    (setq-default mode-line-format standard-mode-line-format)
    (put 'mode-line-format 'standard-value (list `(quote ,standard-mode-line-format)))

    (setq-default mode-line-modes standard-mode-line-modes)
    (put 'mode-line-modes 'standard-value (list `(quote ,standard-mode-line-modes)))

    (setq-default mode-line-position standard-mode-line-position)
    (put 'mode-line-position 'standard-value (list `(quote ,standard-mode-line-position)))))

(when (> emacs-major-version 22)
  (let* ((help-echo
          ;; The multi-line message doesn't work terribly well on the
          ;; bottom mode line...  Better ideas?
          ;;        "\
          ;; mouse-1: select window, mouse-2: delete others, mouse-3: delete,
          ;; drag-mouse-1: resize, C-mouse-2: split horizontally"
          "mouse-1: Select (drag to resize), mouse-2: \
Delete others, mouse-3: Delete this")
         (recursive-edit-help-echo "Recursive edit, `C-M-c' to exit")
         (dashes (propertize "--" 'help-echo help-echo))
         (standard-mode-line-format
          (list "%e" (propertize "-" 'help-echo help-echo) 'mode-line-mule-info
                'mode-line-client 'mode-line-modified 'mode-line-remote
                'mode-line-frame-identification 'mode-line-buffer-identification
                (propertize "   " 'help-echo help-echo) 'mode-line-position '(vc-mode vc-mode)
                (propertize "  " 'help-echo help-echo) 'mode-line-modes
                `(which-func-mode ("" which-func-format ,dashes))
                `(global-mode-string (,dashes global-mode-string))
                (propertize "-%-" 'help-echo help-echo)))
         (standard-mode-line-modes
          (list (propertize "%[" 'help-echo recursive-edit-help-echo)
                (propertize "(" 'help-echo help-echo)
                `(:propertize ("" mode-name)
                              help-echo "mouse-1: Major-mode menu, mouse-2: Major-mode help, \
mouse-3: Toggle minor modes"
                              mouse-face mode-line-highlight
                              local-map ,mode-line-major-mode-keymap)
                '("" mode-line-process)
                `(:propertize ("" minor-mode-alist)
                              mouse-face mode-line-highlight
                              help-echo "mouse-1: Minor-mode menu, mouse-2: Minor-mode help, \
mouse-3: Toggle minor modes"
                              local-map ,mode-line-minor-mode-keymap)
                (propertize "%n" 'help-echo "mouse-2: Widen"
                            'mouse-face 'mode-line-highlight
                            'local-map (make-mode-line-mouse-map
                                        'mouse-2 #'mode-line-widen))
                (propertize ")" 'help-echo help-echo)
                (propertize "%]" 'help-echo recursive-edit-help-echo)
                (propertize "--" 'help-echo help-echo)))
       
         (standard-mode-line-position
          `((-3 ,(propertize
                  "%p"
                  'local-map mode-line-column-line-number-mode-map
                  'mouse-face 'mode-line-highlight
                  'help-echo "Buffer position, mouse-1: Line/col menu"))
            (size-indication-mode
             (8 ,(propertize
                  " of %I"
                  'local-map mode-line-column-line-number-mode-map
                  'mouse-face 'mode-line-highlight
                  'help-echo "Buffer position, mouse-1: Line/col menu")))
            (line-number-mode
             ((column-number-mode
               (10 ,(propertize
                     " (%l,%c)"
                     'local-map mode-line-column-line-number-mode-map
                     'mouse-face 'mode-line-highlight
                     'help-echo "Line and column, mouse-1: Line/col menu"))
               (6 ,(propertize
                    " L%l"
                    'local-map mode-line-column-line-number-mode-map
                    'mouse-face 'mode-line-highlight
                    'help-echo "Line number, mouse-1: Line/col menu"))))
             ((column-number-mode
               (5 ,(propertize
                    " C%c"
                    'local-map mode-line-column-line-number-mode-map
                    'mouse-face 'mode-line-highlight
                    'help-echo "Column number, mouse-1: Line/col menu"))))))))

    (setq-default mode-line-format standard-mode-line-format)
    (put 'mode-line-format 'standard-value (list `(quote ,standard-mode-line-format)))
    
    (setq-default mode-line-modes standard-mode-line-modes)
    (put 'mode-line-modes 'standard-value (list `(quote ,standard-mode-line-modes)))
    
    (setq-default mode-line-position standard-mode-line-position)
    (put 'mode-line-position 'standard-value (list `(quote ,standard-mode-line-position)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'bindings+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; bindings+.el ends here
