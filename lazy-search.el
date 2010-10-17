;;; lazy-search.el --- Lazy Search

;; Filename: lazy-search.el
;; Description: Lazy Search
;; Author: Andy Stewart lazycat.manatee@gmail.com
;; Maintainer: Joe Bloggs vapniks@yahoo.com
;; Copyright (C) 2008, 2009, 2010, Andy Stewart, all rights reserved.
;; Created: 2008-12-23 23:05:10
;; Version: 0.2.2
;; Last-Updated: 2010-09-24 17:01:26
;;           By: Joe Bloggs
;; URL: http://www.emacswiki.org/emacs/download/lazy-search.el
;; Keywords: lazy-search
;; Compatibility: GNU Emacs 23.0.60.1
;;
;; Features that might be required by this library:
;;
;; `thingatpt' `one-key'
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
;; Lazy Search
;;
;; Like this name, this extension provide a lazy-search mode.
;; This extension is base on `one-key' mode.
;;
;; It can take the search string from the buffer around point.
;; Then jump next or previous match fast.
;;
;; Simple describe use step:
;;
;; 1-> Make sure you have install `one-key', `lazy-search' depend it.
;;
;; 2-> Bind some global keystroke with function `lazy-search-menu'.
;;     Like me: (global-set-key (kbd "M-s") 'lazy-search-menu)
;;
;; 3-> Move cursor to some symbol, and type `M-s', then current symbol
;;     will mark in all buffer and will popup a help window.
;;
;; 4-> Then you just type keystroke 's', you can jump to next symbol,
;;     you just type keystroke 'r', you can jump to previous symbol.
;;
;; 5-> If you type 'w' can switch current search type from SYMBOL (default)
;;     switch to WORD.
;;
;; 6-> That's all, you just call function, will popup a keystroke help
;;     window, then you just try type keystroke in window.
;;
;; Have two function transform search object between `lazy-search'
;; `isearch':
;;      `lazy-search-to-isearch' can transform search object to `isearch'.
;;      `isearch-to-lazy-search' can transform search object to `lazy-search',
;;              just bind this function with `isearch-to-lazy-search'.
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
;; Bind any keystroke you like to `lazy-search-menu'.
;;
;; (global-set-key (kbd "M-s") 'lazy-search-menu)
;;

;;; Change log:
;; 2010/10/17
;;    * Joe Bloggs
;;       * Added lazy-search-toggle-keep-region function and lazy-search-keep-region customization variable.
;;       
;; 2010/09/24
;;    * Joe Bloggs
;;       * Moved function for mark/copy parentheses from lazy-search-extension.el to here.
;;         Added function to change to `query-replace'. Changed some keybindings.
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
;;      * Search and toggle REGEXP with search object.
;;        With different color between text and regexp text.
;;
;;      * Support search text through in `outline-mode'.
;;

;;; Require
(require 'thingatpt)
(require 'one-key)

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Customize ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup lazy-search nil
  "Lazy Search."
  :group 'search)

(defface lazy-search-highlight-current
  '((((class color) (background dark))
     (:background "DodgerBlue3" :foreground "black")))
  "Face for highlighting current object."
  :group 'lazy-search)

(defface lazy-search-highlight-background
  '((((class color) (background dark))
     (:background "grey10")))
  "Face for highlighting background object."
  :group 'lazy-search)

(defcustom lazy-search-keep-region t
  "If t then highlighted region will be kept after quitting lazy-search.
Otherwise region will be cleared."
  :type '(boolean)
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

(defvar lazy-search-menu-first-time-p t
  "Whether the first time call `lazy-search-menu'.")

(defvar lazy-search-menu-alist nil
  "The `one-key' menu alist for `lazy-search'.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Utilise Function ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun lazy-search-menu ()
  "The `one-key' menu for `lazy-search'."
  (interactive)
  (lazy-search-init)
  (one-key-menu "Lazy Search" lazy-search-menu-alist t t 'lazy-search-abort nil t))

(defun lazy-search-init ()
  "Lazy search initialization."
  (if lazy-search-menu-first-time-p
      (if (thing-at-point 'symbol)
          (progn
            (setq lazy-search-menu-first-time-p nil)
            (lazy-search-mark-symbol))
        (error "Please move cursor some thing, lazy-search can mark it"))))

(defun lazy-search-toggle-keep-region (val)
  "If called interactively with a prefix, or non-interactively with val equal to 4,
set the current value of `lazy-search-keep-region' to t. 
If called with a double prefix, or with val equal to 16, set `lazy-search-keep-region' to nil.
Otherwise, toggle the current value of `lazy-search-keep-region'."
  (interactive "p")
  (if (equal val 4) (setq lazy-search-keep-region t)
    (if (equal val 16) (setq lazy-search-keep-region nil)
      (if lazy-search-keep-region (setq lazy-search-keep-region nil)
	(setq lazy-search-keep-region t)))))

(defun lazy-search-abort ()
  "Handle abort with `lazy-search'."
  (interactive)
  ;; Clean overlay from buffer.
  (if lazy-search-keep-region
      (progn (set-mark (overlay-start lazy-search-highlight-current-overlay))
	     (goto-char (overlay-end lazy-search-highlight-current-overlay))))
  (lazy-search-highlight-current-clean)
  (lazy-search-highlight-background-clean)
  ;; Save search cache if current search
  ;; object is valid value.
  (when lazy-search-object
    (setq lazy-search-object-cache lazy-search-object)
    (setq lazy-search-object nil))
  ;; Reset some value.
  (setq lazy-search-object-offset 0)
  (setq lazy-search-mark-init-pos 0)
  (setq lazy-search-menu-first-time-p t))

(defun lazy-search-quit ()
  "Lazy search quit."
  (keyboard-quit)
  (message "Quit from lazy-search"))

(defun lazy-search-highlight-current-highlight (object-beg object-end)
  "Highlight current search object.
`OBJECT-BEG' the begin position of search-object.
`OBJECT-END' the end position of search-object."
  (save-excursion
    ;; Clean current highlight overlay in buffer.
    (lazy-search-highlight-current-clean)
    ;; Remove overlay with face from `OBJECT-BEG' to `OBJECT-ENG'.
    (remove-overlays object-beg object-end
                     lazy-search-highlight-current-overlay 'lazy-search-highlight-current)
    ;; Bind current highlight overlay in buffer.
    (setq lazy-search-highlight-current-overlay
          (lazy-search-highlight-bind-overlay
           object-beg object-end
           lazy-search-highlight-current-overlay
           'lazy-search-highlight-current))))

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
`CURSOR-POSITION' is the point of current cursor."
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
(defun lazy-search-mark-word ()
  "Mark word."
  (interactive)
  (save-excursion
    (lazy-search-mark
     (point)
     (beginning-of-thing 'word)
     (end-of-thing 'word))))

(defun lazy-search-mark-symbol ()
  "Mark symbol."
  (interactive)
  (save-excursion
    (lazy-search-mark
     (point)
     (beginning-of-thing 'symbol)
     (end-of-thing 'symbol))))

(defun lazy-search-mark-url ()
  "Mark url."
  (interactive)
  (save-excursion
    (lazy-search-mark
     (point)
     (beginning-of-thing 'url)
     (end-of-thing 'url))))

(defun lazy-search-mark-filename ()
  "Mark filename."
  (interactive)
  (save-excursion
    (lazy-search-mark
     (point)
     (beginning-of-thing 'filename)
     (end-of-thing 'filename))))

(defun lazy-search-mark-email ()
  "Mark email."
  (interactive)
  (save-excursion
    (lazy-search-mark
     (point)
     (beginning-of-thing 'email)
     (end-of-thing 'email))))

(defun lazy-search-mark-sexp ()
  "Mark sexp."
  (interactive)
  (save-excursion
    (lazy-search-mark
     (point)
     (beginning-of-thing 'sexp)
     (end-of-thing 'sexp))))

(defun lazy-search-mark-line ()
  "Mark current line."
  (interactive)
  (save-excursion
    (lazy-search-mark
     (point)
     (line-beginning-position)
     (line-end-position))))

(defun lazy-search-mark-parentheses ()
  "Mark parentheses."
  (interactive)
  (save-excursion
    (if (paredit-in-string-p)
        (lazy-search-mark
         (point)
         (1+ (car (paredit-string-start+end-points)))
         (cdr (paredit-string-start+end-points)))
      (lazy-search-mark
       (point)
       (progn
         (backward-up-list)
         (forward-char +1)
         (point))
       (progn
         (up-list)
         (forward-char -1)
         (point))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Copy Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun lazy-search-copy (object-beg object-end)
  "A fast edit complexes object.
`OBJECT-BEG' the begin position that object.
`OBJECT-END' the end position of object."
  (interactive)
  (message "%s copied." (buffer-substring object-beg object-end))
  (kill-ring-save object-beg object-end))

(defun lazy-search-copy-search ()
  "Copy current search object."
  (interactive)
  (save-excursion
    (lazy-search-copy (overlay-start lazy-search-highlight-current-overlay)
                      (overlay-end lazy-search-highlight-current-overlay))))

(defun lazy-search-copy-symbol ()
  "Copy symbol that at current point."
  (interactive)
  (save-excursion
    (lazy-search-copy (beginning-of-thing 'symbol)
                      (end-of-thing 'symbol))))

(defun lazy-search-copy-word ()
  "Copy words at point.
Kill object if `KILL-CONDITIONAL' is non-nil,
otherwise copy object."
  (interactive)
  (save-excursion
    (lazy-search-copy (beginning-of-thing 'word)
                      (end-of-thing 'word))))

(defun lazy-search-copy-filename ()
  "Copy filename at current point.
Optional argument KILL-CONDITIONAL default is do copy handle, if KILL-CONDITIONAL is non-nil do paste handle."
  (interactive)
  (save-excursion
    (lazy-search-copy (beginning-of-thing 'filename)
                      (end-of-thing 'filename))))

(defun lazy-search-copy-email ()
  "Copy email at current point."
  (interactive)
  (save-excursion
    (lazy-search-copy (beginning-of-thing 'email)
                      (end-of-thing 'email))))

(defun lazy-search-copy-url ()
  "Copy url at current point."
  (interactive)
  (save-excursion
    (lazy-search-copy (beginning-of-thing 'url)
                      (end-of-thing 'url))))

(defun lazy-search-copy-sexp ()
  "Copy regular expression at current point."
  (interactive)
  (save-excursion
    (lazy-search-copy (beginning-of-thing 'sexp)
                      (end-of-thing 'sexp))))

(defun lazy-search-copy-line ()
  "Copy current line into Kill-Ring without mark the line."
  (interactive)
  (save-excursion
    (lazy-search-copy (beginning-of-thing 'line)
                      (end-of-thing 'line))))

(defun lazy-search-copy-parentheses ()
  "Copy parentheses at point."
  (interactive)
  (save-excursion
    (if (paredit-in-string-p)
        (lazy-search-mark
         (point)
         (1+ (car (paredit-string-start+end-points)))
         (cdr (paredit-string-start+end-points)))
      (lazy-search-copy
       (progn
         (backward-up-list)
         (forward-char +1)
         (point))
       (progn
         (up-list)
         (forward-char -1)
         (point))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Move Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun lazy-search-move-forward (&optional reverse)
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

(defun lazy-search-move-backward ()
  "Move backward and mark previous search object."
  (interactive)
  (lazy-search-move-forward t))

(defun lazy-search-move-first ()
  "Move the first search object."
  (interactive)
  (goto-char (point-min))
  (lazy-search-move-forward))

(defun lazy-search-move-last ()
  "Move the last search object."
  (interactive)
  (goto-char (point-max))
  (lazy-search-move-backward))

(defun lazy-search-move-start ()
  "Move to the start position of object"
  (interactive)
  (goto-char (overlay-start lazy-search-highlight-current-overlay))
  (setq lazy-search-object-offset 0))

(defun lazy-search-move-end ()
  "Move to the end position of object"
  (interactive)
  (goto-char (overlay-end lazy-search-highlight-current-overlay))
  (setq lazy-search-object-offset (length lazy-search-object)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; View Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun lazy-search-view-next-line ()
  "Next line."
  (interactive)
  (forward-line +1)
  (message "Next line."))

(defun lazy-search-view-previous-line ()
  "View previous line."
  (interactive)
  (forward-line -1)
  (message "Previous line."))

(defun lazy-search-view-forward-char ()
  "View forward char."
  (interactive)
  (ignore-errors
    (call-interactively 'forward-char))
  (message "Forward char."))

(defun lazy-search-view-backward-char ()
  "View backward char."
  (interactive)
  (ignore-errors
    (call-interactively 'backward-char))
  (message "Backward char."))

(defun lazy-search-view-scroll-up-one-line ()
  "View scroll up one line."
  (interactive)
  (ignore-errors
    (scroll-up 1))
  (message "Scroll up one line"))

(defun lazy-search-view-scroll-down-one-line ()
  "View scroll down one line."
  (interactive)
  (ignore-errors
    (scroll-down 1))
  (message "Scroll down one line."))

(defun lazy-search-view-scroll-up-one-page ()
  "View scroll up one page."
  (interactive)
  (ignore-errors
    (scroll-up))
  (message "Scroll up one page."))

(defun lazy-search-view-scroll-down-one-page ()
  "View scroll down one page."
  (interactive)
  (ignore-errors
    (scroll-down))
  (message "Scroll down one page."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Search Function ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun lazy-search-search-cache (&optional reverse)
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
  (setq lazy-search-menu-first-time-p nil)
  (setq lazy-search-object isearch-string)
  (lazy-search-mark (point)
                    (- (point) (length lazy-search-object))
                    (point))
  (call-interactively 'lazy-search-menu))

(defun lazy-search-to-query-replace ()
  "Perform query-replace with current search object."
  (interactive)
  (setq isearch-string lazy-search-object)
  (isearch-query-replace)
  (lazy-search-quit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Others Function ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun lazy-search-edit-object (&optional reverse)
  "Edit search object and search forward.
Search backward if option `REVERSE' is `non-nil'."
  (interactive)
  (let (new-search-object)
    (or lazy-search-object (setq lazy-search-object ""))
    (setq new-search-object (read-string (format "Search object: (%s)" lazy-search-object) lazy-search-object))
    (lazy-search-search-string new-search-object reverse)))

(defun lazy-search-return-mark-init-position ()
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Customize Keystroke ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(dolist (elt-cons
         '(
           ;; Mark.
           (("w" . "Mark Word") . lazy-search-mark-word)
           (("b" . "Mark Symbol") . lazy-search-mark-symbol)
           (("u" . "Mark Url") . lazy-search-mark-url)
           (("f" . "Mark Filename") . lazy-search-mark-filename)
           (("m" . "Mark Email") . lazy-search-mark-email)
           (("x" . "Mark Sexp") . lazy-search-mark-sexp)
           (("l" . "Mark Line") . lazy-search-mark-line)
	   (("[" . "Mark Parentheses") . lazy-search-mark-parentheses)
           ;; Copy
           (("S" . "Copy Search Object") . lazy-search-copy-search)
           (("W" . "Copy Word") . lazy-search-copy-word)
           (("B" . "Copy Symbol") . lazy-search-copy-symbol)
           (("U" . "Copy Url") . lazy-search-copy-url)
           (("F" . "Copy Filename") . lazy-search-copy-filename)
           (("M" . "Copy Email") . lazy-search-copy-email)
           (("X" . "Copy Sexp") . lazy-search-copy-sexp)
           (("L" . "Copy Line") . lazy-search-copy-line)
	   (("{" . "Copy Parentheses") . lazy-search-copy-parentheses)
           ;; Move.
           (("s" . "Move Forward") . lazy-search-move-forward)
           (("r" . "Move Backward") . lazy-search-move-backward)
           (("<home>" . "Move First") . lazy-search-move-first)
           (("<end>" . "Move Last") . lazy-search-move-last)
           (("<" . "Move Start of Object") . lazy-search-move-start)
           ((">" . "Move End of Object") . lazy-search-move-end)
           ;; View
           (("<down>" . "View Next Line") . lazy-search-view-next-line)
           (("<up>" . "View Previous Line") . lazy-search-view-previous-line)
           (("<left>" . "View Backward Char") . lazy-search-view-backward-char)
           (("<right>" . "View Forward Char") . lazy-search-view-forward-char)
           (("<C-up>" . "View Scroll Down One Line") . lazy-search-view-scroll-down-one-line)
           (("<C-down>" . "View Scroll Up One Line") . lazy-search-view-scroll-up-one-line)
           (("<prior>" . "View Scroll Down One Page") . lazy-search-view-scroll-down-one-page)
           (("<next>" . "View Scroll Up One Page") . lazy-search-view-scroll-up-one-page)
           ;; Search.
           (("c" . "Search Object Cache") . lazy-search-search-cache)
           (("Y" . "Search Yank") . lazy-search-search-yank)
           ;; Isearch.
           (("C-s" . "Switch To Isearch") . lazy-search-to-isearch)
	   (("%" . "Switch to query-replace") . lazy-search-to-query-replace)
           ;; Others.
	   (("t" . "Toggle keep region") . lazy-search-toggle-keep-region)
           (("E" . "Edit Search Object") . lazy-search-edit-object)
           (("." . "Return Mark Init Position") . lazy-search-return-mark-init-position)
           ))
  (add-to-alist 'lazy-search-menu-alist elt-cons))

(provide 'lazy-search)

;;; lazy-search.el ends here

;;; LocalWords:  QinGW DodgerBlue pos elt POSITOIN BAKCWARD str Backeard SPC
