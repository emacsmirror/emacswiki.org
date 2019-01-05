;;; lazy-search.el --- Mark current symbol and jump in all matching symbols.

;; Filename: lazy-search.el
;; Description: Lazy Search
;; Author: Andy Stewart lazycat.manatee@gmail.com
;; Maintainer: Andy Stewart lazycat.manatee@gmail.com
;; Copyright (C) 2008, 2009, Andy Stewart, all rights reserved.
;; Created: 2008-12-23 23:05:10
;; Version: 1.2
;; Last-Updated: 2019-01-05 14:28:28
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/lazy-search.el
;; Keywords: lazy-search
;; Compatibility: GNU Emacs 23.0.60.1
;;
;; Features that might be required by this library:
;;
;; `thingatpt'
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Mark current symbol and jump in all matching symbols.
;;

;;; Installation:
;;
;; Put lazy-search.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'lazy-search)
;;
;; Bind any keystroke you like to `lazy-search'.
;;
;; (global-set-key (kbd "M-s") 'lazy-search)
;;

;;; Change log:
;;
;; 20019/01/05
;;      * Remove `one-key' dependences and unused code, and refactory code.
;;      * Add `lazy-search-to-color-rg'
;;
;; 2009/01/22
;;      * Make function `lazy-search-menu' respond
;;        `execute-last-command-when-miss-match' argument
;;        in function `one-key-menu'.
;;
;; 2008/12/26
;;      * Add functions name beginning with `lazy-search-copy-*'.
;;        Thanks the advice of `QinGW'.
;;
;; 2008/12/24
;;      * Rewrite all functions, base on `one-key.el'.
;;      * Keep common function in `lazy-search.el', and move special function
;;        to file `lazy-search-extension.el' (below functions):
;;              `lazy-search-mark-parentheses'
;;              `lazy-search-moccur'
;;              `lazy-search-moccur-all'.
;;
;; 2008/07/03
;;      * First released.
;;

;;; Acknowledgements:
;;
;;      QinGW (IRC nickname)    <qingwuking@gmail.com>
;;              For advice.
;;

;;; TODO
;;

;;; Require
(require 'thingatpt)

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Customize ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup lazy-search nil
  "Lazy Search."
  :group 'search)

(defface lazy-search-highlight-current
  '((((class color) (background dark))
     (:foreground "black" :background "gold2" :bold t)))
  "Face for highlighting current object."
  :group 'lazy-search)

(defface lazy-search-highlight-background
  '((((class color) (background dark))
     (:foreground "grey80" :background "grey15" :bold t)))
  "Face for highlighting background object."
  :group 'lazy-search)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Variable ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar lazy-search-highlight-current-overlay nil
  "The overlay for `lazy-search-highlight-current'.")

(defvar lazy-search-highlight-background-overlays nil
  "The overlay for `lazy-search-highlight-background'.")

(defvar lazy-search-object nil
  "Current search object.")

(defvar lazy-search-object-cache nil
  "The cache search object that last available.")

(defvar lazy-search-object-offset 0
  "The offset that from object-beginning to cursor position.")

(defvar lazy-search-mark-init-pos 0
  "The initialization mark position.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Utilise Function ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun lazy-search ()
  (interactive)
  (lazy-search-mode 1))

(defun lazy-search-quit ()
  "Lazy search quit."
  (interactive)
  (lazy-search-mode -1)
  (message "Quit from lazy-search"))

(defun lazy-search-abort ()
  "Handle abort with `lazy-search'."
  ;; Clean overlay from buffer.
  (lazy-search-highlight-current-clean)
  (lazy-search-highlight-background-clean)
  ;; Save search cache if current search
  ;; object is valid value.
  (when lazy-search-object
    (setq lazy-search-object-cache lazy-search-object)
    (setq lazy-search-object nil))
  ;; Reset some value.
  (setq lazy-search-object-offset 0)
  (setq lazy-search-mark-init-pos 0))

(defun lazy-search-highlight-current-highlight (object-beg object-end)
  "Highlight current search object.
`OBJECT-BEG' the begin position of search-object.
`OBJECT-END' the end position of search-object."
  (save-excursion
    (if lazy-search-highlight-current-overlay
        (move-overlay lazy-search-highlight-current-overlay object-beg object-end)
      (setq lazy-search-highlight-current-overlay (make-overlay object-beg object-end))
      (overlay-put lazy-search-highlight-current-overlay 'face 'lazy-search-highlight-current))))

(defun lazy-search-highlight-background-highlight (object)
  "Background highlight all match object.
`OBJECT' is search object."
  (let ((object-length (length object))
        overlay)
    ;; Clean background highlight overlay in buffer.
    (lazy-search-highlight-background-clean)
    ;; Bind background highlight overlay in buffer.
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward object nil t)
        (setq overlay
              (lazy-search-highlight-bind-overlay
               (- (point) object-length)
               (point)
               overlay
               'lazy-search-highlight-background))
        (push overlay lazy-search-highlight-background-overlays)))))

(defun lazy-search-highlight-current-clean ()
  "Clean current highlight overlay in buffer."
  (when lazy-search-highlight-current-overlay
    (delete-overlay lazy-search-highlight-current-overlay)
    (setq lazy-search-highlight-current-overlay nil)))

(defun lazy-search-highlight-background-clean ()
  "Clean all background overlays."
  (when lazy-search-highlight-background-overlays
    (mapc 'delete-overlay lazy-search-highlight-background-overlays)
    (setq lazy-search-highlight-background-overlays nil)))

(defun lazy-search-highlight-bind-overlay (beg end overlay overlay-face)
  "Bind some bound with `OVERLAY' and `OVERLAY-FACE'.
And return overlay that have bind face."
  (setq overlay (make-overlay beg end))
  (overlay-put overlay 'face overlay-face)
  overlay)

(defun lazy-search-highlight-bound (beg end)
  "Highlight bound between `BEG' and `END'.
And return search string match bound."
  (let (search-string)
    (if (< beg end)
        (progn
          (setq search-string (buffer-substring beg end))
          (lazy-search-highlight-background-highlight search-string)
          (lazy-search-highlight-current-highlight beg end))
      (message "Invalid search bound from %s (beg) to %s (end)" beg end))
    search-string))

(defun lazy-search-mark (cursor-position beg end)
  "Mark special bound that from `BEG' to `END'.
`CURSOR-POSITOIN' is the point of current cursor."
  (let ((search-object (lazy-search-highlight-bound beg end)))
    (when search-object
      (setq lazy-search-object search-object)
      (setq lazy-search-mark-init-pos cursor-position)
      (setq lazy-search-object-offset (- cursor-position beg)))))

(defun lazy-search-search (object &optional backward)
  "Search forward `OBJECT'.
Try to jump to beginning and search again when reach last one.

Search backward `OBJECT' if option `BAKCWARD' is `non-nil'.
Try to jump to end and search again when reach first one.

Return search position when search successful."
  (let (counter)
    (if (and object
             (not (equal object "")))
        (if backward
            (progn
              ;; Search `twice' if have search object in cursor,
              ;; otherwise search `once'.
              (setq counter (if (looking-back object) 2 1))
              (or (re-search-backward object nil t counter)
                  (progn
                    ;; Jump to end of buffer and search again,
                    ;; if have reach first one.
                    (goto-char (point-max))
                    (re-search-backward object nil t))))
          ;; Search `twice' if have search object in cursor,
          ;; otherwise search `once'.
          (setq counter (if (looking-at object) 2 1))
          (or (re-search-forward object nil t counter)
              (progn
                ;; Jump to beginning of buffer and search again,
                ;; if have reach last one.
                (goto-char (point-min))
                (re-search-forward object nil t))))
      nil)))

(defun lazy-search-search-string (str &optional reverse)
  "Search string with `lazy-search-search'.
Search forward and move cursor after `STR' if `STR' have exit in current buffer,
or search backward move cursor before `STR' if option `REVERSE' is `non-nil'.
Otherwise keep original point before search."
  (let ((original-point (point))
        (str-length (length str)))
    (if (lazy-search-search str reverse)
        (if reverse
            (lazy-search-mark
             (point)
             (point)
             (+ (point) str-length))
          (lazy-search-mark
           (point)
           (- (point) str-length)
           (point)))
      (goto-char original-point)
      (message "Haven't match string with: %s" str))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Mark Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun lazy-search-mark-symbol-or-region ()
  "Mark symbol."
  (interactive)
  (if (or (thing-at-point 'symbol)
          (region-active-p))
      (save-excursion
        (if (region-active-p)
            (progn
              (lazy-search-mark
               (point)
               (region-beginning)
               (region-end))
              (setq mark-active nil))
          (lazy-search-mark
           (point)
           (beginning-of-thing 'symbol)
           (end-of-thing 'symbol))))
    (message "Please move cursor some thing, lazy-search can mark it")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Copy Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun lazy-search-copy (object-beg object-end)
  "A fast edit complexes object.
`OBJECT-BEG' the begin position that object.
`OBJECT-END' the end position of object."
  (interactive)
  (message "%s copied." (buffer-substring object-beg object-end))
  (kill-ring-save object-beg object-end))

(defun lazy-search-copy-object ()
  "Copy current search object."
  (interactive)
  (save-excursion
    (lazy-search-copy (overlay-start lazy-search-highlight-current-overlay)
                      (overlay-end lazy-search-highlight-current-overlay))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Move Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun lazy-search-jump-to-next-match (&optional reverse)
  "Move forward and mark next search object.
Move backward and mark previous search object if option `REVERSE' is `non-nil'."
  (interactive)
  (let ((object-length (length lazy-search-object))
        (original-point (point))
        object-beg-point)
    (if (lazy-search-search lazy-search-object reverse)
        (progn
          (if reverse
              (setq object-beg-point (point))
            (setq object-beg-point (- (point) object-length)))
          ;; Highlight search object.
          (lazy-search-highlight-current-highlight object-beg-point (+ object-beg-point object-length))
          ;; Adjust offset with search object.
          (goto-char (+ object-beg-point lazy-search-object-offset))
          ;; Show search result.
          (if (equal original-point (point))
              (message "Only this one.")
            (message "Search %s %s." lazy-search-object (if reverse "backward" "forward"))))
      ;; Revert original point
      (goto-char original-point)
      (message "Search failed with %s" (or lazy-search-object "")))))

(defun lazy-search-jump-to-previous-match ()
  "Move backward and mark previous search object."
  (interactive)
  (lazy-search-jump-to-next-match t))

(defun lazy-search-jump-to-first-match ()
  "Move the first search object."
  (interactive)
  (goto-char (point-min))
  (lazy-search-jump-to-next-match))

(defun lazy-search-jump-to-last-match ()
  "Move the last search object."
  (interactive)
  (goto-char (point-max))
  (lazy-search-jump-to-previous-match))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; View Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun lazy-search-view-next-line ()
  "Next line."
  (interactive)
  (ignore-errors
    (forward-line +1)))

(defun lazy-search-view-previous-line ()
  "View previous line."
  (interactive)
  (ignore-errors
    (forward-line -1)))

(defun lazy-search-view-forward-char ()
  "View forward char."
  (interactive)
  (ignore-errors
    (call-interactively 'forward-char)))

(defun lazy-search-view-backward-char ()
  "View backward char."
  (interactive)
  (ignore-errors
    (call-interactively 'backward-char)))

(defun lazy-search-scroll-up-one-line ()
  "View scroll up one line."
  (interactive)
  (ignore-errors
    (scroll-up 1)))

(defun lazy-search-scroll-down-one-line ()
  "View scroll down one line."
  (interactive)
  (ignore-errors
    (scroll-down 1)))

(defun lazy-search-scroll-up-one-page ()
  "View scroll up one page."
  (interactive)
  (ignore-errors
    (scroll-up)))

(defun lazy-search-scroll-down-one-page ()
  "View scroll down one page."
  (interactive)
  (ignore-errors
    (scroll-down)))

(defun lazy-search-view-line-beginning ()
  "Move to the start position of object"
  (interactive)
  (goto-char (overlay-start lazy-search-highlight-current-overlay))
  (setq lazy-search-object-offset 0))

(defun lazy-search-view-line-end ()
  "Move to the end position of object"
  (interactive)
  (goto-char (overlay-end lazy-search-highlight-current-overlay))
  (setq lazy-search-object-offset (length lazy-search-object)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Search Function ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun lazy-search-search-previous-cache (&optional reverse)
  "Search forward object cache.
Search backward object cache if option `REVERSE' is `non-nil'."
  (interactive)
  (if lazy-search-object-cache
      (lazy-search-search-string lazy-search-object-cache reverse)
    (message "No search cache for use.")))

(defun lazy-search-search-yank (&optional reverse)
  "Search forward last-object of kill ring.
Search backward last-object of kill ring if option `REVERSE' is `non-nil'."
  (interactive)
  (lazy-search-search-string (or (kill-append nil nil) "") reverse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Isearch Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun lazy-search-to-isearch ()
  "Transform current search object to isearch."
  (interactive)
  (isearch-mode t)
  (setq isearch-string "")
  (isearch-yank-string lazy-search-object)
  (lazy-search-quit))

(defun isearch-to-lazy-search ()
  "Transform isearch to `lazy-search'."
  (interactive)
  (isearch-exit)
  (setq lazy-search-object isearch-string)
  (lazy-search-mark (point)
                    (- (point) (length lazy-search-object))
                    (point))
  (call-interactively 'lazy-search))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Others Function ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun lazy-search-edit-object (&optional reverse)
  "Edit search object and search forward.
Search backward if option `REVERSE' is `non-nil'."
  (interactive)
  (let (new-search-object)
    (or lazy-search-object (setq lazy-search-object ""))
    (setq new-search-object (read-string (format "Edit and research: " lazy-search-object) lazy-search-object))
    (lazy-search-search-string new-search-object reverse)))

(defun lazy-search-jump-to-init ()
  "Return init position that before mark."
  (interactive)
  (if lazy-search-mark-init-pos
      (let (object-beg object-length)
        (goto-char lazy-search-mark-init-pos)
        (setq object-beg (- (point) lazy-search-object-offset))
        (setq object-length (length lazy-search-object))
        (lazy-search-highlight-current-highlight object-beg (+ object-beg object-length))
        (message "Return search start position."))
    (message "No search start position.")))

(defun lazy-search-to-color-rg ()
  (interactive)
  (if (featurep 'color-rg)
      (let ((search-object lazy-search-object))
        (lazy-search-quit)
        (color-rg-search-input search-object default-directory))
    (message "You need install color-rg.el from https://github.com/manateelazycat/color-rg first")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Customize Keystroke ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-minor-mode lazy-search-mode
  "Lazy search mode."
  nil
  "Lazy Search"
  '()
  (if lazy-search-mode
      (lazy-search-mode-enable)
    (lazy-search-mode-disable)))

(defun lazy-search-mode-enable ()
  (lazy-search-mark-symbol-or-region)
  (setq buffer-read-only t))

(defun lazy-search-mode-disable ()
  (lazy-search-abort)
  (setq buffer-read-only nil))

(defun lazy-search-set-key (key-alist &optional keymap key-prefix)
  "This function is to little type when define key binding.
`KEYMAP' is a add keymap for some binding, default is `current-global-map'.
`KEY-ALIST' is a alist contain main-key and command.
`KEY-PREFIX' is a add prefix for some binding, default is nil."
  (let (key def)
    (or keymap (setq keymap (current-global-map)))
    (if key-prefix
        (setq key-prefix (concat key-prefix " "))
      (setq key-prefix ""))
    (dolist (element key-alist)
      (setq key (car element))
      (setq def (cdr element))
      (cond ((stringp key) (setq key (read-kbd-macro (concat key-prefix key))))
            ((vectorp key) nil)
            (t (signal 'wrong-type-argument (list 'array key))))
      (define-key keymap key def))))

(lazy-search-set-key
 '(
   ;; Move
   ("s" . lazy-search-jump-to-next-match)
   ("r" . lazy-search-jump-to-previous-match)
   ("." . lazy-search-jump-to-first-match)
   ("," . lazy-search-jump-to-last-match)
   ("i" . lazy-search-jump-to-init)
   ;; View
   ("j" . lazy-search-view-next-line)
   ("k" . lazy-search-view-previous-line)
   ("h" . lazy-search-view-backward-char)
   ("l" . lazy-search-view-forward-char)
   ("H" . lazy-search-view-line-beginning)
   ("L" . lazy-search-view-line-end)
   ("K" . lazy-search-scroll-down-one-line)
   ("J" . lazy-search-scroll-up-one-line)
   ("e" . lazy-search-scroll-down-one-page)
   ("SPC" . lazy-search-scroll-up-one-page)
   ;; Mark
   ("S" . lazy-search-mark-symbol-or-region)
   ;; Other
   ("w" . lazy-search-copy-object)
   ("c" . lazy-search-search-previous-cache)
   ("t" . lazy-search-to-isearch)
   ("E" . lazy-search-edit-object)
   ("Y" . lazy-search-search-yank)
   ("g" . lazy-search-to-color-rg)
   ;; Quit
   ("q" . lazy-search-quit)
   )
 lazy-search-mode-map)

(provide 'lazy-search)

;;; lazy-search.el ends here
