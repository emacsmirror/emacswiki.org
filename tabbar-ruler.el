;;; tabbar-ruler.el --- Setup tabbar to look pretty...
;; 
;; Filename: tabbar-setup.el
;; Description: Changes tabbar setup to be similar to Aquaemacs.
;; Author: Matthew Fidler, Nathaniel Cunningham
;; Maintainer: 
;; Created: Mon Oct 18 17:06:07 2010 (-0500)
;; Version: 
;; Last-Updated: Thu Nov  4 09:45:01 2010 (-0500)
;;           By: us041375
;;     Update #: 336 
;; URL: 
;; Keywords: Tabbar, Ruler Mode, Menu, Tool Bar.
;; Compatibility: Windows Emacs 23.x
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; Tabbar appearance based on reverse engineering Aquaemacs code and
;; changing to my preferences, and Emacs Wiki.
;;
;; Tabbar/Ruler integration is new. Tabbar should be active on mouse
;; move.  Ruler should be active on self-insert commands.
;;
;; Also allows auto-hiding of toolbar and menu.
;;
;; To use this, put the library in your load path and use
;;
;;
;; (setq EmacsPortable-global-tabbar 't) ; If you want tabbar
;; (setq EmacsPortable-global-ruler 't) ; if you want a global ruler
;; (setq EmacsPortable-popup-menu 't) ; If you want a popup menu.
;; (setq EmacsPortable-popup-toolbar 't) ; If you want a popup toolbar
;;
;; (require 'tabbar-ruler)
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;; 04-Nov-2010      
;;    Last-Updated: Thu Nov  4 09:39:14 2010 (-0500) (us041375)
;;    Made tabbar mode default.
;; 02-Nov-2010    Matthew L. Fidler  
;;    Last-Updated: Tue Nov  2 10:14:12 2010 (-0500) (Matthew L. Fidler)
;;    Make post-command-hook handle errors gracefully.
;; 20-Oct-2010    Matthew L. Fidler  
;;    Last-Updated: Tue Oct 19 15:37:53 2010 (-0500) (us041375)
;;
;;    Changed behavior when outside the window to assume the last
;;    known mouse position. This fixes the two problems below.
;;
;; 20-Oct-2010    Matthew L. Fidler  
;;    Last-Updated: Tue Oct 19 15:37:53 2010 (-0500) (us041375)
;;
;;    As it turns out when the toolbar is hidden when the mouse is
;;    outside of the emacs window, it also hides when navigating the
;;    menu.  Switching behavior back.
;;
;; 20-Oct-2010    Matthew L. Fidler  
;;    Last-Updated: Tue Oct 19 15:37:53 2010 (-0500) (us041375)
;;    Made popup menu and toolbar be hidden when mouse is oustide of emacs window.
;; 20-Oct-2010    Matthew L. Fidler  
;;    Last-Updated: Tue Oct 19 15:37:53 2010 (-0500) (us041375)
;;    Changed to popup ruler-mode if tabbar and ruler are not displayed.
;; 19-Oct-2010    Matthew L. Fidler  
;;    Last-Updated: Tue Oct 19 15:37:53 2010 (-0500) (us041375)
;;    Changed tabbar, menu, toolbar and ruler variables to be buffer
;;    or frame local.
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

(require 'tabbar)
(set-face-attribute 'tabbar-default nil
		    :inherit nil
		    :weight 'normal
		    :width 'normal
		    :slant 'normal
		    :underline nil
		    :strike-through nil
                    ;; inherit from frame		    :inverse-video
		    :stipple nil
		    :background "gray80"
		    :foreground "black"
                    ;;		    :box '(:line-width 2 :color "white" :style nil)
		    :box nil
		    :family "Lucida Grande"
                    )

(set-face-attribute 'tabbar-selected nil
		    :background "gray95"
		    :foreground "gray20"
		    :inherit 'tabbar-default
		    :box '(:line-width 3 :color "grey95" :style nil))
;; 		    :box '(:line-width 2 :color "white" :style released-button))

(set-face-attribute 'tabbar-unselected nil
		    :inherit 'tabbar-default
		    :background "gray80"
		    :box '(:line-width 3 :color "grey80" :style nil))

(defface tabbar-selected-highlight '((t
                                      :foreground "black"
                                      :background "gray95"))
  "Face for selected, highlighted tabs."
  :group 'tabbar)

(defface tabbar-unselected-highlight '((t
                                        :foreground "black"
                                        :background "grey75"
                                        :box (:line-width 3 :color "grey75" :style nil)))
  "Face for unselected, highlighted tabs."
  :group 'tabbar)

(set-face-attribute 'tabbar-button nil
		    :inherit 'tabbar-default
		    :box nil)

(set-face-attribute 'tabbar-separator nil
		    :background "grey50"
 		    :foreground "grey50"
		    :height 1.0)

(setq tabbar-home-button-enabled-image
      '((:type xpm :file "down.xpm")))

(setq tabbar-home-button-disabled-image
      '((:type xpm :file "up.xpm")))

(setq tabbar-home-button
      (cons (cons "[o]" tabbar-home-button-enabled-image)
            (cons "[x]" tabbar-home-button-disabled-image)))

(setq tabbar-buffer-home-button
      (cons (cons "[+]" tabbar-home-button-enabled-image)
            (cons "[-]" tabbar-home-button-disabled-image)))

(setq tabbar-scroll-left-button-enabled-image
      '((:type xpm :file "left.xpm")))

(setq tabbar-scroll-left-button-disabled-image
      '((:type xpm :file "left_disabled.xpm")))

(setq tabbar-scroll-left-button
      (cons (cons " <" tabbar-scroll-left-button-enabled-image)
            (cons " =" tabbar-scroll-left-button-disabled-image)))

(setq tabbar-scroll-right-button-enabled-image
      '((:type xpm :file "right.xpm")))

(setq tabbar-scroll-right-button-disabled-image
      '((:type xpm :file "right_disabled.xpm")))

(setq tabbar-scroll-right-button
      (cons (cons " >" tabbar-scroll-right-button-enabled-image)
            (cons " =" tabbar-scroll-right-button-disabled-image)))

(setq tabbar-close-tab-button
      '((:type xpm :file "close-tab.xpm")))

(defsubst tabbar-normalize-image (image &optional margin nomask)
  "Make IMAGE centered and transparent.
If optional MARGIN is non-nil, it must be a number of pixels to add as
an extra margin around the image.  If optional NOMASK is non-nil, no mask
property is included." 
  (let ((plist (cdr image)))
    (or (plist-get plist :ascent)
        (setq plist (plist-put plist :ascent 'center)))
    (or (plist-get plist :mask)
        (unless nomask
          (setq plist (plist-put plist :mask '(heuristic t)))))
    (or (not (natnump margin))
        (plist-get plist :margin)
        (plist-put plist :margin margin))
    (setcdr image plist))
  image)

(defvar tabbar-close-tab-function nil
  "Function to call to close a tabbar tab.  Passed a single argument, the tab
construct to be closed.")

(defvar tabbar-new-tab-function nil
  "Function to call to create a new buffer in tabbar-mode.  Optional single
argument is the MODE for the new buffer.")

;; for buffer tabs, use the usual command to close/kill a buffer
(defun tabbar-buffer-close-tab (tab)
  (let (
        (buffer (tabbar-tab-value tab))
	)
    (with-current-buffer buffer
      (kill-buffer buffer)
      )
    )
  )

(setq tabbar-close-tab-function 'tabbar-buffer-close-tab)

(defsubst tabbar-click-on-tab (tab &optional type action)
  "Handle a mouse click event on tab TAB.
Call `tabbar-select-tab-function' with the received, or simulated
mouse click event, and TAB.
Optional argument TYPE is a mouse click event type (see the function
`tabbar-make-mouse-event' for details)."
  (let* ((mouse-event (tabbar-make-mouse-event type))
	 (mouse-button (event-basic-type mouse-event)))
    (if (eq action 'close-tab)
	(when (and (eq mouse-button 'mouse-1) tabbar-close-tab-function)
	  (funcall tabbar-close-tab-function tab))
      (when tabbar-select-tab-function
	(funcall tabbar-select-tab-function
		 (tabbar-make-mouse-event type) tab)
	(tabbar-display-update)))))

(defun tabbar-select-tab-callback (event)
  "Handle a mouse EVENT on a tab.
Pass mouse click events on a tab to `tabbar-click-on-tab'."
  (interactive "@e")
  (when (tabbar-click-p event)
    (let ((target (posn-string (event-start event))))
      (tabbar-click-on-tab
       (get-text-property (cdr target) 'tabbar-tab (car target))
       event
       (get-text-property (cdr target) 'tabbar-action (car target))))))

(defsubst tabbar-line-tab (tab)
  "Return the display representation of tab TAB.
That is, a propertized string used as an `header-line-format' template
element.
Call `tabbar-tab-label-function' to obtain a label for TAB."
  (let* ( (selected-p (tabbar-selected-p tab (tabbar-current-tabset)))
          (modified-p (buffer-modified-p (tabbar-tab-value tab)))
          (close-button-image (tabbar-find-image tabbar-close-tab-button))
          (face (if selected-p
                    (if modified-p
                        'tabbar-selected-modified
                      'tabbar-selected
                      )
                  (if modified-p
                      'tabbar-unselected-modified
                    'tabbar-unselected
                    )
                  ))
          )
    (concat 
     (propertize "[x]"
                 'display (tabbar-normalize-image close-button-image 0)
                 'face face
                 'pointer 'hand
                 'tabbar-tab tab
                 'local-map (tabbar-make-tab-keymap tab)
                 'tabbar-action 'close-tab
                 )
     (propertize " " 'face face
                 'tabbar-tab tab
                 'local-map (tabbar-make-tab-keymap tab)
                 'help-echo 'tabbar-help-on-tab
                 'face face
                 'pointer 'hand
                 )
     (propertize
      (if tabbar-tab-label-function
          (funcall tabbar-tab-label-function tab)
        tab)
      'tabbar-tab tab
      'local-map (tabbar-make-tab-keymap tab)
      'help-echo 'tabbar-help-on-tab
      'mouse-face 'tabbar-highlight
      'face face
      'pointer 'hand)
     (propertize (if modified-p (with-temp-buffer (ucs-insert "207A") (insert " ") (buffer-substring (point-min) (point-max))) " ") 'face face
                 'tabbar-tab tab
                 'local-map (tabbar-make-tab-keymap tab)
                 'help-echo 'tabbar-help-on-tab
                 'face face
                 'pointer 'hand
                 )
     tabbar-separator-value)
    )
  )

(defface tabbar-selected-modified
  '((t
     :inherit tabbar-selected
     :weight bold
     ))
  "Face used for unselected tabs."
  :group 'tabbar)

(defface tabbar-unselected-modified
  '((t
     :inherit tabbar-unselected
     :weight bold
     ))
  "Face used for unselected tabs."
  :group 'tabbar)

(defface tabbar-key-binding '((t
                               :foreground "white"))
  "Face for unselected, highlighted tabs."
  :group 'tabbar)

(setq tabbar-separator '(0.25)) 

(defface tabbar-selected-modified
  '((t
     :inherit tabbar-selected
     :weight bold
     ))
  "Face used for unselected tabs."
  :group 'tabbar)

(defface tabbar-unselected-modified
  '((t
     :inherit tabbar-unselected
     :weight bold
     ))
  "Face used for unselected tabs."
  :group 'tabbar)

(defface tabbar-key-binding '((t
                               :foreground "white"))
  "Face for unselected, highlighted tabs."
  :group 'tabbar)

;; Hooks based on yswzing's hooks, but modified for this function state.
;; called each time the modification state of the buffer changed
(defun ep-modification-state-change ()
  (tabbar-set-template tabbar-current-tabset nil)
  (tabbar-display-update))
;; first-change-hook is called BEFORE the change is made
(defun ep-on-buffer-modification ()
  (set-buffer-modified-p t)
  (ep-modification-state-change))
(add-hook 'after-save-hook 'ep-modification-state-change)

(defcustom EmacsPortable-global-tabbar 't
  "* Should EmacsPortable have a global tabbar?"
  :type 'boolean
  :group 'EmacsPortable
  )
(defcustom EmacsPortable-global-ruler nil
  "* Should EmacsPortable have a global ruler?"
  :type 'boolean
  :group 'EmacsPortable
  )
(defcustom EmacsPortable-popup-menu nil
  "* Should EmacsPortable have a popup menu.  As mouse moves toward top of window, the menu pops up."
  :type 'boolean
  :group 'EmacsPortable
  )
(defcustom EmacsPortable-popup-toolbar nil
  "* Should EmacsPortable have a popup toolbar.  As mouse moves toward top of window, the toolbar pops up."
  :type 'boolean
  :group 'EmacsPortable
  )
(defcustom EmacsPortable-popup-menu-min-y 5
  "* Minimum number of pixels from the top before a menu/toolbar pops up."
  :type 'integer
  :group 'EmacsPortable
  )
(defcustom EmacsPortable-popup-menu-min-y-leave 50
  "* Minimum number of pixels form the top before a menu/toolbar disappears."
  :type 'integer
  :group 'EmacsPortable
  )
(defcustom EmacsPortable-do-not-switch-on-ruler-when-tabbar-is-on-y 75
  "* Minimum number of pixels to switch on ruler when tabbar is on."
  :type 'integer
  :group 'EmacsPortable
  )

(defvar ep-tabbar-off 't
  )
(defvar ep-ruler-off 't
  )
(set (make-variable-buffer-local 'ep-toolbar-off) nil)
(set (make-variable-buffer-local 'ep-ruler-off) nil)

(defvar ep-toolbar-off nil
  )
(defvar ep-menu-off nil
  )

                                        ;(make-variable-frame-local 'ep-menu-off)
                                        ;(make-variable-frame-local 'ep-toolbar-off)
(add-hook 'find-file-hook (lambda() (interactive) (ep-tabbar-ruler-fight 't)))
(defcustom ep-ruler-display-commands '(
                                       ac-trigger-commands
                                       esn-upcase-char-self-insert
                                       esn-magic-$
                                       )
  "* Ruler display commands."
  :group 'EmacsPortable
  :type '(repeat symbol)
  )
(defun ep-tabbar-ruler-fight (&optional initialize)
  "* Defines the fighting behavior of the EmacsPortable ruler and tabbar."
  (condition-case error
      (progn
        (cond
         ( (eq last-command 'mouse-drag-region)
           (ep-mouse-movement)
           )
         ( (and EmacsPortable-global-ruler EmacsPortable-global-tabbar)
           (cond
            ( (memq last-command ep-ruler-display-commands)
              (when ep-ruler-off
                (ruler-mode 1)
                (setq ep-ruler-off nil)
                )
              (unless ep-tabbar-off
                (tabbar-mode -1)
                (setq ep-tabbar-off 't)
                )
              (when EmacsPortable-popup-menu
                (unless ep-menu-off
                  (menu-bar-mode -1)
                  (setq ep-menu-off 't)
                  )
                )
              (when EmacsPortable-popup-toolbar
                (unless ep-toolbar-off
                  (tool-bar-mode -1)
                  (setq ep-toolbar-off 't)
                  )
                )
              )
            ( (string-match "\(mouse\|ignore\|window\|frame\)" (format "%s" last-command))
              (when nil ;; Took this out;  Afterward it works much better...
                (unless ep-ruler-off
                  (ruler-mode -1)
                  (setq ep-ruler-off 't)
                  )
                (when ep-tabbar-off
                  (tabbar-mode 1)
                  (setq ep-tabbar-off nil)
                  )
                )
              )
            ( 't
              (when (or initialize (and ep-ruler-off ep-tabbar-off))
                (when ep-ruler-off
                  (ruler-mode 1)
                  (setq ep-ruler-off nil)
                  )
                (unless ep-tabbar-off
                  (tabbar-mode -1)
                  (setq ep-tabbar-off 't)
                  )
                )
              )
            )
           )
         ( EmacsPortable-global-ruler
           (when ep-ruler-off
             (ruler-mode 1)
             (setq ep-ruler-off nil)
             )
           )
         ( EmacsPortable-global-tabbar
           (when ep-tabbar-off
             (tabbar-mode 1)
             (setq ep-tabbar-off nil)
             )
           )
         )
        )
    (error
     (message "Error in post-command-hook for Ruler/Tabbar: %s" (error-message-string error)))
    )
  )
(add-hook 'post-command-hook 'ep-tabbar-ruler-fight)
(defvar ep-movement-timer nil
  )
(defvar ep-movement-x nil
  )
(defvar ep-movement-y nil
  )

(defun ep-mouse-movement ()
  "* Mouse Movement function"
  (interactive)
  (when ep-movement-timer
    (cancel-timer ep-movement-timer)
    )
  (let* (
         (y-pos (cddr (mouse-pixel-position)))
         (x-pos (cadr (mouse-pixel-position)))
         )
    (unless y-pos
      (setq y-pos ep-movement-y)
      )
    (unless x-pos
      (setq x-pos ep-movement-x)
      )
    (when (or (not ep-movement-x) (not ep-movement-y)
              (and ep-movement-x ep-movement-y
                   (not
                    (and
                     (= ep-movement-x x-pos)
                     (= ep-movement-y y-pos)
                     )
                    )
                   )
              )
      (when (and x-pos y-pos)
        (setq ep-movement-x x-pos)
        (setq ep-movement-y y-pos)
        (unless ep-ruler-off
          (ruler-mode -1)
          (setq ep-ruler-off 't)
          )
        (when ep-tabbar-off
          (tabbar-mode 1)
          (setq ep-tabbar-off nil)
          )
        (if (>= (if (or ep-menu-off ep-toolbar-off)
                    EmacsPortable-popup-menu-min-y
                  EmacsPortable-popup-menu-min-y-leave) y-pos)
            (progn
              (when EmacsPortable-popup-menu
                (when ep-menu-off
                  (menu-bar-mode 1)
                  (setq ep-menu-off nil)
                  )
                )
              (when EmacsPortable-popup-toolbar
                (when ep-toolbar-off
                  (tool-bar-mode 1)
                  (setq ep-toolbar-off nil)
                  )
                )
              )
          (when EmacsPortable-popup-menu
            (unless ep-menu-off
              (menu-bar-mode -1)
              (setq ep-menu-off 't)
              )
            )
          (when EmacsPortable-popup-toolbar
            (unless ep-toolbar-off
              (tool-bar-mode -1)
              (setq ep-toolbar-off 't)
              )
            )
          )
        )
      )
    (setq ep-movement-timer (run-with-timer 
                             0.5
                             nil 
                             'ep-mouse-movement))

    )
  )
(ep-mouse-movement)

(defadvice ep-switch-to-buffer (around switch-to-buffer activate)
  "* EP Switch to buffer advice, adds hook to initialize tabbar or ruler mode."
  ad-do-it
  (ruler-mode 1)
  (tabbar-mode -1)
  (when ep-ruler-off
    (setq ep-ruler-off nil)
    )
  (unless ep-tabbar-off
    (setq ep-tabbar-off 't)
    )
  )
(defadvice ep-iswitchb-buffer (around switch-to-buffer activate)
  "* EP Switch to buffer advice, adds hook to initialize tabbar or ruler mode."
  ad-do-it
  (ruler-mode 1)
  (tabbar-mode -1)
  (when ep-ruler-off
    (setq ep-ruler-off nil)
    )
  (unless ep-tabbar-off
    (setq ep-tabbar-off 't)
    )
  )
(defadvice ep-icicle-buffer (around icicle-buffer activate)
  "* EP Switch to buffer advice, adds hook to initialize tabbar or ruler mode."
  ;; I'm not sure this works...
  ad-do-it
  (ruler-mode 1)
  (tabbar-mode -1)
  (when ep-ruler-off
    (setq ep-ruler-off nil)
    )
  (unless ep-tabbar-off
    (setq ep-tabbar-off 't)
    )
  )
(provide 'tabbar-ruler)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; tabbar-ruler.el ends here
