;;; tabbar-ruler.el --- Setup tabbar to look pretty...
;;
;; Filename: tabbar-setup.el
;; Description: Changes tabbar setup to be similar to Aquaemacs.
;; Author: Matthew Fidler, Nathaniel Cunningham
;; Maintainer: Matthew L. Fidler
;; Created: Mon Oct 18 17:06:07 2010 (-0500)
;; Version: 0.5
;; Last-Updated: Thu Mar  1 08:26:44 2012 (-0600)
;;           By: Matthew L. Fidler
;;     Update #: 656
;; URL: http://github.com/mlf176f2/tabbar-ruler.el
;; Keywords: Tabbar, Ruler Mode, Menu, Tool Bar.
;; Compatibility: Windows Emacs 23.x
;; Package-Requires: ((tabbar "2.0.1"))
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
;; (setq tabbar-ruler-global-tabbar 't) ; If you want tabbar
;; (setq tabbar-ruler-global-ruler 't) ; if you want a global ruler
;; (setq tabbar-ruler-popup-menu 't) ; If you want a popup menu.
;; (setq tabbar-ruler-popup-toolbar 't) ; If you want a popup toolbar
;;
;; (require 'tabbar-ruler)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;; 9-Feb-2012    Matthew L. Fidler  
;;    Last-Updated: Thu Feb  9 19:18:21 2012 (-0600) #651 (Matthew L. Fidler)
;;    Will not change the menu bar in a Mac.  Its always there.
;; 14-Jan-2012    Matthew L. Fidler  
;;    Last-Updated: Sat Jan 14 21:58:51 2012 (-0600) #648 (Matthew L. Fidler)
;;    Added more commands that trigger the ruler.
;; 14-Jan-2012    Matthew L. Fidler  
;;    Last-Updated: Sat Jan 14 21:44:32 2012 (-0600) #641 (Matthew L. Fidler)
;;    Added more ruler commands.   It works a bit better
;;    now. Additionally I have changed the ep- to tabbar-ruler-.
;; 14-Jan-2012    Matthew L. Fidler  
;;    Last-Updated: Tue Feb  8 15:01:27 2011 (-0600) #639 (Matthew L. Fidler)
;;    Changed EmacsPortable to tabbar-ruler
;; 08-Feb-2011    Matthew L. Fidler  
;;    Last-Updated: Tue Feb  8 14:59:57 2011 (-0600) #638 (Matthew L. Fidler)
;;    Added ELPA tags.  
;; 08-Feb-2011    Matthew L. Fidler  
;;    Last-Updated: Tue Feb  8 12:47:09 2011 (-0600) #604 (Matthew L. Fidler)
;;    Removed xpm dependencies.  Now no images are required, they are built by the library.
;; 04-Dec-2010    Matthew L. Fidler  
;;    Last-Updated: Sat Dec  4 16:27:07 2010 (-0600) #551 (Matthew L. Fidler)
;;    Added context menu.
;; 01-Dec-2010    Matthew L. Fidler  
;;    Last-Updated: Wed Dec  1 15:26:37 2010 (-0600) #341 (Matthew L. Fidler)
;;    Added scratch buffers to list.
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
(eval-when-compile
  (require 'cl))
(require 'tabbar)
(require 'easymenu)

(defun tabbar-popup-menu ()
  "Keymap for pop-up menu.  Emacs only."
  `(,(format "%s" (nth 0 tabbar-last-tab))
    ["Close" tabbar-popup-close]
    ["Close all BUT this" tabbar-popup-close-but]
    "--"
    ["Save" tabbar-popup-save]
    ["Save As" tabbar-popup-save-as]
    "--"
    ["Rename File" tabbar-popup-rename
     :active (and (buffer-file-name (tabbar-tab-value tabbar-last-tab)) (file-exists-p (buffer-file-name (tabbar-tab-value tabbar-last-tab))))]
    ["Delete File" tabbar-popup-delete
     :active (and (buffer-file-name (tabbar-tab-value tabbar-last-tab)) (file-exists-p (buffer-file-name (tabbar-tab-value tabbar-last-tab))))]
    "--"
    ["Gzip File" tabbar-popup-gz
     :active (and (executable-find "gzip") (buffer-file-name (tabbar-tab-value tabbar-last-tab))
                  (file-exists-p (buffer-file-name (tabbar-tab-value tabbar-last-tab)))
                  (not (string-match "\\.gz\\(?:~\\|\\.~[0-9]+~\\)?\\'" (buffer-file-name (tabbar-tab-value tabbar-last-tab)))))]
    ["Bzip File" tabbar-popup-bz2
     :active (and (executable-find "bzip2") (buffer-file-name (tabbar-tab-value tabbar-last-tab))
                  (file-exists-p (buffer-file-name (tabbar-tab-value tabbar-last-tab)))
                  (not (string-match "\\.bz2\\(?:~\\|\\.~[0-9]+~\\)?\\'" (buffer-file-name (tabbar-tab-value tabbar-last-tab)))))]
    ["Decompress File" tabbar-popup-decompress
     :active (and
              (file-exists-p (buffer-file-name (tabbar-tab-value tabbar-last-tab)))
              (string-match "\\(?:\\.\\(?:Z\\|gz\\|bz2\\|tbz2?\\|tgz\\|svgz\\|sifz\\|xz\\|dz\\)\\)\\(\\(?:~\\|\\.~[0-9]+~\\)?\\)\\'"
                            (buffer-file-name (tabbar-tab-value tabbar-last-tab))))
     ]
    ;;    "--"
    ;;    ["Print" tabbar-popup-print]
    )
  )

(defun tabbar-popup-print ()
  "Print Buffer"
  (interactive))


(defun tabbar-popup-close ()
  "Tab-bar pop up close"
  (interactive)
  (funcall tabbar-close-tab-function tabbar-last-tab))

(defun tabbar-popup-close-but ()
  "Tab-bar close all BUT this buffer"
  (interactive)
  (let ((cur (symbol-value (funcall tabbar-current-tabset-function))))
    (mapc (lambda(tab)
            (unless (eq tab tabbar-last-tab)
              (funcall tabbar-close-tab-function tab)))
          cur)))

(defun tabbar-popup-save-as ()
  "Tab-bar save as"
  (interactive)
  (let* ((buf (tabbar-tab-value tabbar-last-tab)))
    (save-excursion
      (set-buffer buf)
      (call-interactively 'write-file))))

(defun tabbar-popup-rename ()
  "Tab-bar rename"
  (interactive)
  (let* ((buf (tabbar-tab-value tabbar-last-tab))
         (fn (buffer-file-name buf)))
    (save-excursion
      (set-buffer buf)
      (when (call-interactively 'write-file)
        (if (string= fn (buffer-file-name (current-buffer)))
            (error "Buffer has same name.  Just saved instead.")
          (delete-file fn))))))

(defun tabbar-popup-delete ()
  "Tab-bar delete file"
  (interactive)
  (let* ((buf (tabbar-tab-value tabbar-last-tab))
         (fn (buffer-file-name buf)))
    (when (yes-or-no-p (format "Are you sure you want to delete %s?" buf))
      (save-excursion
        (set-buffer buf)
        (set-buffer-modified-p nil)
        (kill-buffer (current-buffer))
        (delete-file fn)))))

(defun tabbar-popup-remove-compression-ext (file-name &optional new-compression)
  "Removes compression extension, and possibly adds a new extension"
  (let ((ret file-name))
    (when (string-match "\\(\\(?:\\.\\(?:Z\\|gz\\|bz2\\|tbz2?\\|tgz\\|svgz\\|sifz\\|xz\\|dz\\)\\)?\\)\\(\\(?:~\\|\\.~[0-9]+~\\)?\\)\\'" ret)
      (setq ret (replace-match (concat (or new-compression "") (match-string 2 ret)) t t ret)))
    (symbol-value 'ret)))

(defun tabbar-popup-gz (&optional ext err)
  "Gzips the file"
  (interactive)
  (let* ((buf (tabbar-tab-value tabbar-last-tab))
         (fn (buffer-file-name buf))
         (nfn (tabbar-popup-remove-compression-ext fn (or ext ".gz"))))
    (if (string= fn nfn)
        (error "Already has that compression!")
      (save-excursion
        (set-buffer buf)
        (write-file nfn)
        (if (not (file-exists-p nfn))
            (error "%s" (or err "Could not gzip file!"))
          (when (file-exists-p fn)
            (delete-file fn)))))))

(defun tabbar-popup-bz2 ()
  "Bzip file"
  (interactive)
  (tabbar-popup-gz ".bz2" "Could not bzip the file!"))

(defun tabbar-popup-decompress ()
  "Decompress file"
  (interactive)
  (tabbar-popup-gz "" "Could not decompress the file!"))

(defun tabbar-context-menu ()
  "Pop up a context menu."
  (interactive)
  (popup-menu (tabbar-popup-menu)))


(set-face-attribute 'tabbar-default nil
                    :inherit nil
                    :weight 'normal
                    :width 'normal
                    :slant 'normal
                    :underline nil
                    :strike-through nil
                    ;; inherit from frame                   :inverse-video
                    :stipple nil
                    :background "gray80"
                    :foreground "black"
                    ;;              :box '(:line-width 2 :color "white" :style nil)
                    :box nil
                    :family "Lucida Grande"
                    )

(set-face-attribute 'tabbar-selected nil
                    :background "gray95"
                    :foreground "gray20"
                    :inherit 'tabbar-default 
                    :box '(:line-width 3 :color "grey95" :style nil))
;;                  :box '(:line-width 2 :color "white" :style released-button))

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

(defun* tabbar-ruler-image (&key type disabled color)
  "Returns the scroll-images"
  (let ((clr (or color (if disabled "#B4B4B4" "#979797"))))
    (if (eq type 'close)
        (format "/* XPM */
        static char * close_tab_xpm[] = {
        \"14 11 3 1\",
        \"       c None\",
        \".      c %s\",
        \"+      c #D2D4D1\",
        \"     .....    \",
        \"    .......   \",
        \"   .........  \",
        \"  ... ... ... \",
        \"  .... . .... \",
        \"  ..... ..... \",
        \"  .... . .... \",
        \"  ... ... ... \",
        \"   .........  \",
        \"    .......   \",
        \"     .....    \"};" clr)
      
      (format
       "/* XPM */
static char * scroll_%s_%s_xpm[] = {
\"17 17 2 1\",
\"       c None\",
\".      c %s\",
\"                 \",
\"                 \",
\"                 \",
\"                 \",
\"                 \",
%s
\"                 \",
\"                 \",
\"                 \",
\"                 \",
\"                 \",
\"                 \"};
" (symbol-name type)
(if disabled "disabled" "enabled")
clr
(cond
 ((eq 'right type)
  "\"                 \",
\"     ..          \",
\"     ....        \",
\"     ......      \",
\"     .....       \",
\"     ...         \",
"
  )
 ((eq 'left type)
  "\"                 \",
\"          ..     \",
\"        ....     \",
\"      ......     \",
\"       .....     \",
\"         ...     \","
  )
 ((eq 'up type)
  "\"        .        \",
\"       ..        \",
\"       ...       \",
\"      ....       \",
\"      .....      \",
\"      .....      \",")
 ((eq 'down type)
  "\"      .....      \",
\"      .....      \",
\"      ....       \",
\"       ...       \",
\"       ..        \",
\"        .        \","))))))

(setq tabbar-home-button-enabled-image
      `((:type xpm :data ,(tabbar-ruler-image :type 'down))))

(setq tabbar-home-button-disabled-image
      `((:type xpm :data ,(tabbar-ruler-image :type 'up))))


(setq tabbar-home-button
      (cons (cons "[o]" tabbar-home-button-enabled-image)
            (cons "[x]" tabbar-home-button-disabled-image)))

(setq tabbar-buffer-home-button
      (cons (cons "[+]" tabbar-home-button-enabled-image)
            (cons "[-]" tabbar-home-button-disabled-image)))

(setq tabbar-scroll-left-button-enabled-image
      `((:type xpm :file ,(tabbar-ruler-image :type 'left))))

(setq tabbar-scroll-left-button-disabled-image
      `((:type xpm :file ,(tabbar-ruler-image :type 'left :disabled t))))

(setq tabbar-scroll-left-button
      (cons (cons " <" tabbar-scroll-left-button-enabled-image)
            (cons " =" tabbar-scroll-left-button-disabled-image)))

(setq tabbar-scroll-left-button-value nil)

(setq tabbar-scroll-right-button-enabled-image
      `((:type xpm :data ,(tabbar-ruler-image :type 'right))))

(setq tabbar-scroll-right-button-disabled-image
      `((:type xpm :data  ,(tabbar-ruler-image :type 'right :disabled t))))

(setq tabbar-scroll-right-button
      (cons (cons " >" tabbar-scroll-right-button-enabled-image)
            (cons " =" tabbar-scroll-right-button-disabled-image)))

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
  (let ((buffer (tabbar-tab-value tab)))
    (with-current-buffer buffer
      (kill-buffer buffer))))

(setq tabbar-close-tab-function 'tabbar-buffer-close-tab)
(defvar tabbar-last-tab nil)
(defsubst tabbar-click-on-tab (tab &optional type action)
  "Handle a mouse click event on tab TAB.
Call `tabbar-select-tab-function' with the received, or simulated
mouse click event, and TAB.
Optional argument TYPE is a mouse click event type (see the function
`tabbar-make-mouse-event' for details)."
  (let* ((mouse-event (tabbar-make-mouse-event type))
         (mouse-button (event-basic-type mouse-event)))
    (if  (eq mouse-button 'mouse-3)
        (progn
          (setq tabbar-last-tab tab)
          (tabbar-context-menu))
      (if (eq action 'close-tab)
          (when (and (eq mouse-button 'mouse-1) tabbar-close-tab-function)
            (funcall tabbar-close-tab-function tab))
        (when tabbar-select-tab-function
          (funcall tabbar-select-tab-function
                   (tabbar-make-mouse-event type) tab)
          (tabbar-display-update))))))

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
          (close-button-image (tabbar-find-image 
                               `((:type xpm :data ,(tabbar-ruler-image :type 'close :disabled (not modified-p))))))
          (face (if selected-p
                    (if modified-p
                        'tabbar-selected-modified
                      'tabbar-selected
                      )
                  (if modified-p
                      'tabbar-unselected-modified
                    'tabbar-unselected
                    ))))
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
                 'pointer 'hand)
     tabbar-separator-value)))

(defface tabbar-selected-modified
  '((t
     :inherit tabbar-selected
     :weight bold))
  "Face used for unselected tabs."
  :group 'tabbar)

(defface tabbar-unselected-modified
  '((t
     :inherit tabbar-unselected
     :weight bold))
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
     :weight bold))
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
(defun tabbar-ruler-modification-state-change ()
  (tabbar-set-template tabbar-current-tabset nil)
  (tabbar-display-update))
;; first-change-hook is called BEFORE the change is made
(defun tabbar-ruler-on-buffer-modification ()
  (set-buffer-modified-p t)
  (tabbar-ruler-modification-state-change))
(add-hook 'after-save-hook 'tabbar-ruler-modification-state-change)

(defcustom tabbar-ruler-global-tabbar 't
  "* Should tabbar-ruler have a global tabbar?"
  :type 'boolean
  :group 'tabbar-ruler
  )
(defcustom tabbar-ruler-global-ruler nil
  "* Should tabbar-ruler have a global ruler?"
  :type 'boolean
  :group 'tabbar-ruler
  )
(defcustom tabbar-ruler-popup-menu nil
  "* Should tabbar-ruler have a popup menu.  As mouse moves toward top of window, the menu pops up."
  :type 'boolean
  :group 'tabbar-ruler
  )
(defcustom tabbar-ruler-popup-toolbar nil
  "* Should tabbar-ruler have a popup toolbar.  As mouse moves toward top of window, the toolbar pops up."
  :type 'boolean
  :group 'tabbar-ruler
  )
(defcustom tabbar-ruler-popup-menu-min-y 5 ;
  "* Minimum number of pixels from the top before a menu/toolbar pops up."
  :type 'integer
  :group 'tabbar-ruler
  )
(defcustom tabbar-ruler-popup-menu-min-y-leave 50
  "* Minimum number of pixels form the top before a menu/toolbar disappears."
  :type 'integer
  :group 'tabbar-ruler
  )
(defcustom tabbar-ruler-do-not-switch-on-ruler-when-tabbar-is-on-y 75
  "* Minimum number of pixels to switch on ruler when tabbar is on."
  :type 'integer
  :group 'tabbar-ruler
  )

(defcustom tabbar-ruler-excluded-buffers '("*Messages*" "*Completions*" "*ESS*")
  "* Excluded buffers in tabbar."
  :type '(repeat (string :tag "Buffer Name"))
  :group 'tabbar-ruler)

(defvar tabbar-ruler-tabbar-off 't
  )
(defvar tabbar-ruler-ruler-off 't
  )
(set (make-variable-buffer-local 'tabbar-ruler-toolbar-off) nil)
(set (make-variable-buffer-local 'tabbar-ruler-ruler-off) nil)

(defvar tabbar-ruler-toolbar-off nil
  )
(defvar tabbar-ruler-menu-off nil
  )
(add-hook 'find-file-hook (lambda() (interactive) (tabbar-ruler-tabbar-ruler-fight 't)))
(defcustom tabbar-ruler-ruler-display-commands '(ac-trigger-commands
                                                 esn-upcase-char-self-insert
                                                 esn-magic-$
                                                 right-char
                                                 left-char
                                                 previous-line
                                                 next-line
                                                 backward-paragraph
                                                 forward-paragraph
                                                 cua-scroll-down
                                                 cua-scroll-up
                                                 cua-paste
                                                 cua-paste-pop
                                                 autopair-newline
                                                 autopair-insert-opening
                                                 autopair-skip-close-maybe
                                                 autopair-backspace
                                                 backward-delete-char-untabify
                                                 delete-backward-char
                                                 self-insert-command)
  "* Ruler display commands."
  :group 'tabbar-ruler
  :type '(repeat symbol)
  )
(defun tabbar-ruler-tabbar-ruler-fight (&optional initialize)
  "* Defines the fighting behavior of the tabbar-ruler ruler and tabbar."
  (condition-case error
      (progn
        (cond
         ( (eq last-command 'mouse-drag-region)
           (tabbar-ruler-mouse-movement))
         ( (and tabbar-ruler-global-ruler tabbar-ruler-global-tabbar)
           (cond
            ( (memq last-command tabbar-ruler-ruler-display-commands)
              (when tabbar-ruler-ruler-off
                (ruler-mode 1)
                (setq tabbar-ruler-ruler-off nil))
              (unless tabbar-ruler-tabbar-off
                (tabbar-mode -1)
                (setq tabbar-ruler-tabbar-off 't))
              (when tabbar-ruler-popup-menu
                (unless tabbar-ruler-menu-off
                  (unless (eq system-type 'darwin)
		    (menu-bar-mode -1))
                  (setq tabbar-ruler-menu-off 't)))
              (when tabbar-ruler-popup-toolbar
                (unless (eq system-type 'darwin)
                  (unless tabbar-ruler-toolbar-off
                    (tool-bar-mode -1)
                    (setq tabbar-ruler-toolbar-off 't)))))
            ( (string-match "\\(mouse\\|ignore\\|window\\|frame\\)" (format "%s" last-command))
              (when nil ;; Took this out;  Afterward it works much better...
                (unless tabbar-ruler-ruler-off
                  (ruler-mode -1)
                  (setq tabbar-ruler-ruler-off 't))
                (when tabbar-ruler-tabbar-off
                  (tabbar-mode 1)
                  (setq tabbar-ruler-tabbar-off nil))))
            ( 't
              (when (or initialize (and tabbar-ruler-ruler-off tabbar-ruler-tabbar-off))
                (when tabbar-ruler-ruler-off
                  (ruler-mode 1)
                  (setq tabbar-ruler-ruler-off nil))
                (unless tabbar-ruler-tabbar-off
                  (tabbar-mode -1)
                  (setq tabbar-ruler-tabbar-off 't))))))
         ( tabbar-ruler-global-ruler
           (when tabbar-ruler-ruler-off
             (ruler-mode 1)
             (setq tabbar-ruler-ruler-off nil)))
         ( tabbar-ruler-global-tabbar
           (when tabbar-ruler-tabbar-off
             (tabbar-mode 1)
             (setq tabbar-ruler-tabbar-off nil)
             )
           )
         ))
    (error
     (message "Error in post-command-hook for Ruler/Tabbar: %s" (error-message-string error)))))

(add-hook 'post-command-hook 'tabbar-ruler-tabbar-ruler-fight)
(defvar tabbar-ruler-movement-timer nil
  )
(defvar tabbar-ruler-movement-x nil
  )
(defvar tabbar-ruler-movement-y nil
  )

(defun tabbar-ruler-mouse-movement ()
  "* Mouse Movement function"
  (interactive)
  (when tabbar-ruler-movement-timer
    (cancel-timer tabbar-ruler-movement-timer)
    )
  (let* (
         (y-pos (cddr (mouse-pixel-position)))
         (x-pos (cadr (mouse-pixel-position)))
         )
    (unless y-pos
      (setq y-pos tabbar-ruler-movement-y)
      )
    (unless x-pos
      (setq x-pos tabbar-ruler-movement-x)
      )
    (when (or (not tabbar-ruler-movement-x) (not tabbar-ruler-movement-y)
              (and tabbar-ruler-movement-x tabbar-ruler-movement-y
                   (not
                    (and
                     (= tabbar-ruler-movement-x x-pos)
                     (= tabbar-ruler-movement-y y-pos)
                     )
                    )
                   )
              )
      (when (and x-pos y-pos)
        (setq tabbar-ruler-movement-x x-pos)
        (setq tabbar-ruler-movement-y y-pos)
        (unless tabbar-ruler-ruler-off
          (ruler-mode -1)
          (setq tabbar-ruler-ruler-off 't))
        (when tabbar-ruler-tabbar-off
          (tabbar-mode 1)
          (setq tabbar-ruler-tabbar-off nil))
        (if (>= (if (or tabbar-ruler-menu-off tabbar-ruler-toolbar-off)
                    tabbar-ruler-popup-menu-min-y
                  tabbar-ruler-popup-menu-min-y-leave) y-pos)
            (progn
              (when tabbar-ruler-popup-menu
                (when tabbar-ruler-menu-off
                  (unless (eq system-type 'darwin)
		    (menu-bar-mode 1))
                  (setq tabbar-ruler-menu-off nil)))
              (when tabbar-ruler-popup-toolbar
                (unless (eq system-type 'darwin)
                  (when tabbar-ruler-toolbar-off
                    (tool-bar-mode 1)
                    (setq tabbar-ruler-toolbar-off nil)))))
          (when tabbar-ruler-popup-menu
            (unless tabbar-ruler-menu-off
              (unless (eq system-type 'darwin)
		(menu-bar-mode -1))
              (setq tabbar-ruler-menu-off 't)))
          (when tabbar-ruler-popup-toolbar
            (unless (eq system-type 'darwin)
              (unless tabbar-ruler-toolbar-off
                (tool-bar-mode -1)
                (setq tabbar-ruler-toolbar-off 't)))))))
    (setq tabbar-ruler-movement-timer (run-with-timer
                                       0.01
                             nil
                             'tabbar-ruler-mouse-movement))))
(tabbar-ruler-mouse-movement)


(setq tabbar-buffer-groups-function 'tabbar-buffer-groups)

(defun last-tabbar-ruler-tabbar-buffer-groups nil)

(defun tabbar-ruler-tabbar-buffer-groups ()
  "Return the list of group names the current buffer belongs to.
Return a list of one element based on major mode."
  (setq last-tabbar-ruler-tabbar-buffer-groups
        (list
         (cond
          ;;          ((or (get-buffer-process (current-buffer))
          ;;               ;; Check if the major mode derives from `comint-mode' or
          ;;               ;; `compilation-mode'.
          ;;               (tabbar-buffer-mode-derived-p
          ;;                major-mode '(comint-mode compilation-mode)))
          ;;           "Process")
          ;;    ((string-match "^ *[*]" (buffer-name))
          ;;     "Common"
          ;;     )
          ((eq major-mode 'dired-mode)
           "Dired")
          ((memq major-mode
                 '(help-mode apropos-mode Info-mode Man-mode))
           "Help")
          ((memq major-mode
                 '(rmail-mode
                   rmail-edit-mode vm-summary-mode vm-mode mail-mode
                   mh-letter-mode mh-show-mode mh-folder-mode
                   gnus-summary-mode message-mode gnus-group-mode
                   gnus-article-mode score-mode gnus-browse-killed-mode))
           "Mail")
          (t
           "Files"
           ))))
  (symbol-value 'last-tabbar-ruler-tabbar-buffer-groups))
(setq tabbar-buffer-groups-function 'tabbar-ruler-tabbar-buffer-groups)

(defun tabbar-ruler-tabbar-buffer-list ()
  "Return the list of buffers to show in tabs.
Exclude buffers whose name starts with a space or *, when they are not
visiting a file.  The current buffer is always included."
  (delq nil
        (mapcar #'(lambda (b)
                    (cond
                     ;; Always include the current buffer.
                     ((eq (current-buffer) b) b)
                     ((buffer-file-name b) b)
                     ((member (buffer-name b) tabbar-ruler-excluded-buffers) nil)
                     ;; ((string= "*Messages*" (format "%s" (buffer-name b))))
                     ((char-equal ?\  (aref (buffer-name b) 0)) nil)
                     ;;((char-equal ?* (aref (buffer-name b) 0)) nil)
                     ((buffer-live-p b) b)))
                (buffer-list))))
(setq tabbar-buffer-list-function 'tabbar-ruler-tabbar-buffer-list)
(provide 'tabbar-ruler)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; tabbar-ruler.el ends here
