;;; textmenu.el --- alternate text mode access to menu-bar

;; Copyright (C) 2001  Alex Schroeder <alex@gnu.org>

;; Version: 1.0.1
;; Keywords: menu
;; Author: Alex Schroeder <alex@gnu.org>
;; Maintainer: Alex Schroeder <alex@gnu.org>
;; URL: http://www.emacswiki.org/cgi-bin/wiki.pl?TextMenu

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary

;; This is an laternative to using F10 (runs the command tmm-menubar).
;; One possible way to install this is to add the following to your
;; ~/.emacs file:

;; (autoload 'textmenu "textmenu" "Text mode substitute for menubar" t)
;; (global-set-key [f10] 'textmenu)

;;; Code

(require 'tmm); for the incredible `tmm-get-keybind'

(unless (fboundp 'dolist);; older Emacs doesn't have this macro
  (require 'cl))
(unless (fboundp 'overlay-put)
  (require 'overlay));; newer XEmacs has this file

(defgroup textmenu nil
  "Alternate text mode access to menu-bar."
  :group 'menu
  :version "20.7")

(defcustom textmenu-buffer-name "*Menu Bar*"
  "Name used for the temporary `textmenu' buffer."
  :type 'string
  :group 'menu)

(defvar textmenu-buffer nil
  "The buffer used by `textmenu'.")

(defvar textmenu-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'textmenu-follow-item)
    (define-key map (kbd "q") 'bury-buffer)
    ;; debug
    (define-key map (kbd "d")
      (lambda ()
        (interactive)
        (message "%S" (textmenu-overlay-get (point)))))
    map)
  "Mode map used for `textmenu-mode'.
\\[")

(define-derived-mode textmenu-mode fundamental-mode
  "Textmenu"
  "Major mode for `textmenu' buffers.

\\[textmenu-mode-map]"
  (setq buffer-read-only t))

(defun textmenu-create ()
  "Create a `textmenu' buffer."
  (setq textmenu-buffer 
        (get-buffer-create textmenu-buffer-name))
  (set-buffer textmenu-buffer)
  (textmenu-mode))

(defun textmenu-choices (menu)
  "Return the top level of MENU.
MENU is a keymap.  The top level of MENU is a list of
entries having the one of the following forms:

\(LABEL KEYMAP)
LABEL is a text string to use in the menu.
KEYMAP is a keymap.

\(LABEL EVENT COMMAND)
LABEL is a text string to use in the menu.
EVENT is what to use as `last-command-event'.
COMMAND is what to `call-interactively'.
See `textmenu-follow-item'."
  (delq nil
        (mapcar 
         (lambda (item)
           ;; I can't test wether it is a real list or only a cons cell,
           ;; therefore just do it and catch any errors...
           (condition-case nil
               (cond ((and (symbolp (nth 0 item)); eg. Files keymap
                           (stringp (nth 1 item))
                           (keymapp (cddr item)))
                      (let ((label (nth 1 item))
                            (keymap (cddr item)))
                        (when (symbolp keymap);; eg. Compare popup (keymap)
                          (setq keymap (indirect-function keymap)))
                        (list label keymap)))
                     ;; from here on item must have at least three elements
                     ((and (symbolp (nth 0 item)); eg. Mule keymap
                           (stringp (nth 2 item))
                           (keymapp (nth 3 item)))
                      (let ((label (nth 2 item))
                            (keymap (nth 3 item)))
                        (list label keymap)))
                     ((and (symbolp (nth 0 item)); eg. search-forward command
                           (stringp (nth 1 item))
                           (listp (nth 2 item)))
                      (let ((label (nth 1 item))
                            (event (nth 2 item))
                            (command (cdddr item)))
                        (when (equal '(nil) event);; eg. Chinese-GB
                          (setq event (nth 0 item)))
                        (list label event command)))
                     ((stringp (nth 1 item)); eg. buffer and frame menu
                      (let ((label (nth 1 item))
                            (event (nth 0 item))
                            (command (cdddr item)))
                        (list label event command))))
             (error)))
         menu)))

(defun textmenu-overlay-put (start end prop)
  "Create new overlay from START to END.
Add property textmenu with value PROP to the overlay."
  (let ((o (make-overlay start end)))
    (overlay-put o 'textmenu prop)))

(defun textmenu-overlay-get (pos)
  "Return property textmenu for any of the overlays at POS."
  (let ((overlays (overlays-at (point)))
        o result)
    (while (and overlays (not result))
      (setq o (car overlays)
            overlays (cdr overlays)
            result (overlay-get o 'textmenu)))
    result))

(defun textmenu-setup (menu)
  "Creates a `textmenu' for MENU.
MENU is a menu keymap or part of a menu keymap.
The buffer is populated with the menu items in MENU."
  (unless (buffer-live-p textmenu-buffer)
    (textmenu-create))
  (switch-to-buffer textmenu-buffer)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (let ((choices (textmenu-choices menu)))
      (dolist (item choices)
        (let ((label (car item))
              (start (point)))
          (insert label)
          (textmenu-overlay-put start (point) item)
          (newline))))
    (goto-char (point-min))))

(defun textmenu-obey-menu-bar-final-items (menu-bar)
  "Obey `menu-bar-final-items'; put those items last.
Rearranges MENU-BAR which is a keymap."
  ;; Ripped from `tmm-menubar'.
  (let ((list menu-bar-final-items))
    (while list
      (let ((item (car list)))
        ;; ITEM is the name of an item that we want to put last.
        ;; Find it in MENU-BAR and move it to the end.
        (let ((this-one (assq item menu-bar)))
          (setq menu-bar (append (delq this-one menu-bar)
                                 (list this-one)))))
      (setq list (cdr list))))
  menu-bar)

(defun textmenu ()
  "Create the textmenu for the menu-bar."
  (interactive)
  (run-hooks 'menu-bar-update-hook)
  ;; If you want to obey menu-bar-final-items, look at `tmm-menubar'.
  (textmenu-setup (textmenu-obey-menu-bar-final-items
                   (tmm-get-keybind [menu-bar]))))

(defun textmenu-follow-item ()
  "Follow item at point and do the right thing."
  ;; See `textmenu-choices' for the data structure used.
  (interactive)
  (let ((prop (textmenu-overlay-get (point))))
    (cond ((= 2 (length prop))
           (textmenu-setup (nth 1 prop)))
          ((commandp (nth 2 prop))
           (bury-buffer)
           (setq last-command-event (nth 1 prop))
           (call-interactively (nth 2 prop))))))

;;; textmenu.el ends here
