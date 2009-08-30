;;; yatm.el -- Yet Another Text Menu

;; Copyright (C) 2003 Sacha Chua <sacha@free.net.ph>

;; The canonical URL for this file is:
;;   http://www.purl.org/net/sachac/notebook/emacs/yatm.el

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

;; This is heavily based on textmenu.el (Alex Schroeder)

;;; Code
(require 'tmm); for the incredible `tmm-get-keybind'
(unless (fboundp 'overlay-put)
  (require 'overlay));; newer XEmacs has this file

(defvar yatm-buffer-name "*Menu Bar*" "*Name used for the temporary `yatm' buffer.")
(defvar yatm-menu-count-limit 9 "*The maximum number of menu items on a page.")

(defvar yatm-buffer nil "The buffer used by `yatm'.")
(defvar yatm-buffer-menu nil "The menu for the `yatm' buffer.")
(defvar yatm-start nil "The offset into `yatm-buffer-menu'.")
(defvar yatm-breadcrumbs nil "A list of (NAME KEYBINDING) to allow returning to the parent menu.")

(defvar yatm-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'yatm-follow-item)
    (define-key map (kbd "q") 'bury-buffer)
    (define-key map (kbd "SPC") 'yatm-scroll-up)
    (define-key map (kbd "DEL") 'yatm-scroll-down)
    (define-key map (kbd "<next>") 'yatm-scroll-up)
    (define-key map (kbd "<prior>") 'yatm-scroll-down)
    (define-key map "u" 'yatm-parent)
    (define-key map "p" 'yatm-parent)
    (define-key map "j" 'yatm-goto-item)
    (define-key map "." 'yatm-item-number)
    (define-key map "1" 'yatm-follow-1)
    (define-key map "2" 'yatm-follow-2)
    (define-key map "3" 'yatm-follow-3)
    (define-key map "4" 'yatm-follow-4)
    (define-key map "5" 'yatm-follow-5)
    (define-key map "6" 'yatm-follow-6)
    (define-key map "7" 'yatm-follow-7)
    (define-key map "8" 'yatm-follow-8)
    (define-key map "9" 'yatm-follow-9)
    ;; For easier use on Twiddlers (www.handykey.com)
    (define-key map "a" 'yatm-follow-1)
    (define-key map "b" 'yatm-follow-2)
    (define-key map "c" 'yatm-follow-3)
    (define-key map "d" 'yatm-follow-4)
    (define-key map "e" 'yatm-follow-5)
    (define-key map "f" 'yatm-follow-6)
    (define-key map "g" 'yatm-follow-7)
    (define-key map "h" 'yatm-follow-8)
    (define-key map "i" 'yatm-follow-9)
    map))


(define-derived-mode yatm-mode fundamental-mode
  "Yatm"
  "Major mode for `yatm' buffers.

\\{yatm-mode-map}"
  (setq buffer-read-only t))

(defun yatm-create ()
  "Create a `yatm' buffer."
  (set-buffer
   (setq yatm-buffer
         (get-buffer-create yatm-buffer-name)))
  (yatm-mode))

;; Taken from textmenu.el (Alex Schroeder) and tmm.el
(defun yatm-obey-menu-bar-final-items (menu-bar)
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

;; Taken from textmenu.el practically verbatim, except for some bugfixes.
(defun yatm-choices (menu &optional start end)
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
See `yatm-follow-item'."
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
                     ;; Remove separators
                     ((< (length item) 3)
                      nil)
                     ;; From here on item must have at least three elements
                     ((and (symbolp (nth 0 item)); eg. Mule keymap
                           (stringp (nth 2 item))
                           (keymapp (nth 3 item)))
                      (let ((label (nth 2 item))
                            (keymap (nth 3 item)))
                        (list label keymap)))
                     ((and (symbolp (nth 0 item)); eg. Mule keymap
                           (stringp (nth 2 item))
                           (functionp (nth 3 item)))
                      (let ((label (nth 2 item))
                            (function (nth 3 item)))
                        (list label label function)))
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
         (if (and (symbolp menu)
                  (boundp menu))
             (eval menu)
           menu))))

(defun yatm-follow-1 () "Executes the first item." (interactive) (yatm-follow-item 1))
(defun yatm-follow-2 () "Executes the second item." (interactive) (yatm-follow-item 2))
(defun yatm-follow-3 () "Executes the third item." (interactive) (yatm-follow-item 3))
(defun yatm-follow-4 () "Executes the fourth item." (interactive) (yatm-follow-item 4))
(defun yatm-follow-5 () "Executes the fifth item." (interactive) (yatm-follow-item 5))
(defun yatm-follow-6 () "Executes the sixth item." (interactive) (yatm-follow-item 6))
(defun yatm-follow-7 () "Executes the seventh item." (interactive) (yatm-follow-item 7))
(defun yatm-follow-8 () "Executes the eigth item." (interactive) (yatm-follow-item 8))
(defun yatm-follow-9 () "Executes the ninth item." (interactive) (yatm-follow-item 9))

(defun yatm-setup (menu-name menu &optional start)
  "Creates a `yatm' for MENU.
MENU is a menu keymap or part of a menu keymap.
The buffer is populated with the menu items in MENU."
  (unless (buffer-live-p yatm-buffer)
    (yatm-create))
  (switch-to-buffer yatm-buffer)
  (let ((inhibit-read-only t)
        (count 0)
        (current (or start 0))
        max
        (limit yatm-menu-count-limit))
    (erase-buffer)
    (insert menu-name "\n")
    (setq yatm-start (or start 0))
    (if menu (setq yatm-buffer-menu (yatm-choices menu)))
    (setq max (1- (length yatm-buffer-menu)))
    (while (and (or (not limit) (<= limit 0) (< count limit)) (<= current max))
      (let ((item (nth current yatm-buffer-menu)))
        (setq count (1+ count))
        (when item
          (setq current (1+ current))
          (if (< count 10)
              (insert (number-to-string count) ". " (car item) ".")
            (insert "   " (car item) "."))  
          (yatm-overlay-put (line-beginning-position) (point) item)
          (yatm-overlay-put (line-beginning-position) (point) count 'yatm-item-no)
          (insert "\n"))))
    (when (and start (> start 0))
      (insert "Previous entries.")
      (yatm-overlay-put (line-beginning-position) (point) 
                        '(yatm-scroll-down))
      (insert "\n"))
    (when (< (1+ current) max)
      (insert "Next entries.")
      (yatm-overlay-put (line-beginning-position) (point) 
                        '(yatm-scroll-up))
      (insert "\n"))
    (when (> (length yatm-breadcrumbs) 1)
      (insert "Return to parent menu.")
      (yatm-overlay-put (line-beginning-position) (point)
                        '(yatm-parent))
      (insert "\n")))
  (goto-char (point-min)))

(defun yatm-scroll-up ()
  "Shows the next `yatm-menu-count-limit' entries in the menu."
  (interactive)
  (let ((limit yatm-menu-count-limit))
    (if (and limit (> limit 0) (< (+ yatm-start limit) (length yatm-buffer-menu)))
        (yatm-setup (nth 0 (nth 0 yatm-breadcrumbs)) nil (+ yatm-start limit))
      (error "End of entries"))))

(defun yatm-scroll-down ()
  "Shows the previous `yatm-menu-count-limit' entries in the menu."
  (interactive)
  (if (and yatm-start (> yatm-start 0))
      (let ((limit
             (if (and yatm-menu-count-limit
                      (> yatm-menu-count-limit 0))
                 yatm-menu-count-limit
               10)))
        (yatm-setup (nth 0 (nth 0 yatm-breadcrumbs)) nil
                    (if (< yatm-start limit) 0 (- yatm-start limit))))
    (error "Beginning of entries")))

;; Taken from textmenu.el (Alex Schroeder)
(defun yatm ()
  "Create the text menu for the menu-bar."
  (interactive)
  (run-hooks 'menu-bar-update-hook)
  (let ((menu (yatm-obey-menu-bar-final-items
               (tmm-get-keybind [menu-bar]))))
    (setq yatm-breadcrumbs (list (list "Menu bar" menu)))
    ;; If you want to obey menu-bar-final-items, look at `tmm-menubar'.
    (yatm-setup "Menu bar" menu)))

(defun yatm-overlay-put (start end prop &optional symbol)
  "Create new overlay from START to END.
Add property yatm with value PROP to the overlay."
  (let ((o (make-overlay start end)))
    (overlay-put o (or symbol 'yatm) prop)))

;; Taken from textmenu.el (Alex Schroeder)
(defun yatm-overlay-get (pos &optional symbol)
  "Return property yatm for any of the overlays at POS."
  (let ((overlays (overlays-at (point)))
        o result)
    (while (and overlays (not result))
      (setq o (car overlays)
            overlays (cdr overlays)
            result (overlay-get o (or symbol 'yatm))))
    result))

;; Taken from textmenu.el (Alex Schroeder) and modified
(defun yatm-follow-item (&optional num)
  "Follow item and do the right thing.
If NUM is specified, go to that item first.
If NUM is non-nil, follow the item at point."
  ;; See `yatm-choices' for the data structure used.
  (interactive)
  (if num (goto-line (1+ num)))
  (yatm-execute-item (yatm-overlay-get (point))))

(defun yatm-execute-item (item)
  "Follow item and do the right thing.
If NUM is specified, go to that item first.
If NUM is non-nil, follow the item at point."
  ;; See `yatm-choices' for the data structure used.
  (cond ((= 1 (length item))
         (call-interactively (nth 0 item)))
        ((= 2 (length item))
          (push (list (nth 0 item) (nth 1 item)) yatm-breadcrumbs)
          (yatm-setup (nth 0 item) (nth 1 item)))
        ((commandp (nth 2 item))
         (bury-buffer)
         (setq last-command-event (nth 1 item))
         (call-interactively (nth 2 item)))))


;; Taken from textmenu.el (Alex Schroeder) and modified
(defun yatm-goto-item (num)
  "Prompt for an item number and follow it."
  (interactive "nItem number: ")
  (yatm-follow-item num))

(defun yatm-item-number ()
  "Returns the item number at point."
  (interactive)
  (if (interactive-p)
      (message "%d" (yatm-overlay-get (point) 'yatm-item-no))
    (yatm-overlay-get (point) 'yatm-item-no))
  )

(defun yatm-parent ()
  "Displays the parent menu."
  (interactive)
  (when yatm-breadcrumbs
    (pop yatm-breadcrumbs))
  (when yatm-breadcrumbs
    (yatm-setup (nth 0 (nth 0 yatm-breadcrumbs))
                (nth 1 (nth 0 yatm-breadcrumbs)))))

(provide 'yatm)
;;; yatm.el ends here
