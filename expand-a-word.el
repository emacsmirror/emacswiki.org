;;; expand-a-word.el --- expand-a-word
;;
;; Copyright (C) 2000, 2001, 2002 Peter Milliken
;;
;; Author: Peter Milliken peter.milliken@gtech.com
;;
;; $Date: 2002/10/30 20:58:10 $
;; Version: 1.2
;; Keywords: word expansion completion
;;
;;
;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;; expand-a-word.el is free software
;;
;;; Commentary:
;; These functions will find the next, or previous occurrence of the word
;; that the cursor is currently positioned over.
;;
;; Find the beginning of the word, then search forward for the end of the
;; word, extract the word to 'target-string' then search forward for the
;; next occurrence of 'target-string'.
;;
;;; Installation
;; 1. Copy anywhere into your load path. Add the following to your .emacs file:
;;
;; (require 'expand-a-word)
;;
;; 2. Bind the command 'expand-a-word to some convenient key sequence
;;    i.e. I use <F2> on my keyboard.
;;
;; Usage: Invoke 'expand-a-word after typing some text, if there is a
;; single possible completion that will be done, if there are more
;; than one possibility, then the possibilities will be displayed in a
;; buffer and point positioned at the first possible choice. Navigate
;; up and down using the arrow keys (point wraps when it hits the top
;; or bottom), select the word at point using either the 's' key or
;; invoking the 'expand-a-word command again (convenient when you
;; don't want to move one hand in the process :-)).
;;
;; Use 'q' to quit from the command without making any completion.
;;
;;; Change Log:

;;; Code:

(defvar expand-menu-mode-map nil "")

(defvar expand-selected-text nil
  "This is the string selected by the user when multiple choices in the Expand
   word function are available"
  )


;;
;; Make sure that all key-bindings that bind to the expansion command are
;; echoed in the menu selection keymap ie the user doesn't have to move his
;; fingers from the command that caused a menu pick list to be displayed.
;;
(defun enable-dups (command-to-search-out replacement-command)
  (let (abc)
    (setq abc (where-is-internal command-to-search-out))
    (while abc
      (progn
        (define-key expand-menu-mode-map (car abc) replacement-command)
        (setq abc (cdr abc))
        )
      )
    )
  )
;;
;; Block out forward and backward char motion.
;;
(defun expand-menu-block-movement ()
;;  [Documentation]
  (interactive)
  (let ()
    )
  )

;;
;; Next line in the menu, wraps when moving past last line
;;
(defun expand-menu-previous-line ()
;;  [Documentation]
  (interactive)
  (let ((here (point)))
    (progn
      (if (not (= (point) (point-min)))
          (progn
            (backward-char)
            (beginning-of-line)
            )
        (progn
          (goto-char (1- (point-max)))
          (beginning-of-line)
          )
        )
      )
    )
  )

;;
;; Quit the menu pick list processing.
;;
(defun expand-menu-quit ()
  "Quit from the menu pick list."
  (interactive)
  (let ()
    (setq expand-selected-text nil)
    (exit-recursive-edit)
    )
  )

;;
;; Next line in the menu, wraps when moving past last line
;;
(defun expand-menu-next-line ()
;;  [Documentation]
  (interactive)
  (let ((here (point)))
    (progn
      (end-of-line)
      (if (not (= (point) (1- (point-max))))
          (forward-char)
        (goto-char (point-min))
        )
      )
    )
  )


(defun expand-menu-select ()
  (interactive)
  (let (my-position)
    (end-of-line)
    (setq my-position (point))
    (beginning-of-line)
    (setq expand-selected-text (buffer-substring (point) my-position))
    (exit-recursive-edit)
    )
  )

;
; This routine displays the list of possible matches for the expand-a-word
; function, it is not called unless there is more than one possible match
;
(defun expand-display (possible-matches)
  (let (my-buffer
        (oldbuf (current-buffer))
        (upper-window (selected-window))
        current-element
        lower-window
        temp-list)
    (save-window-excursion
      (save-excursion
        (setq expand-selected-text nil)
        (setq my-buffer (get-buffer-create "*Expand List*"))
        (set-buffer my-buffer)
        (erase-buffer)
        (setq possible-matches (sort possible-matches 'string<))
        (setq current-element (car possible-matches))
        (insert current-element)
        (newline)
        (setq temp-list possible-matches)
        (while (setq temp-list (cdr temp-list))
          (progn
            (insert (car temp-list))
            (newline)
            )
          )
        (goto-char (point-min))
        ;; The call to split-window results in a return value which is the lower
        ;; of the two windows. Want to display the menu in the "upper" window,
        ;; so use variables appropriately.
        (setq lower-window (split-window (selected-window)))
        (set-window-buffer upper-window my-buffer)
        (shrink-window-if-larger-than-buffer upper-window)
        (select-window upper-window)
        (setq major-mode 'expand-display)
        (if (not expand-menu-mode-map)
            (expand-create-menu-keymap)
          )
        (use-local-map expand-menu-mode-map)
        (setq mode-name "Expand mode")
        (recursive-edit)
        )
      )
    )
  )

(defun expand-create-menu-keymap ()
  (let ()
    (progn
      (if (not expand-menu-mode-map)
          (progn
            (setq expand-menu-mode-map (make-keymap))
            (suppress-keymap expand-menu-mode-map t)
            (define-key expand-menu-mode-map "q" 'expand-menu-quit)
            (define-key expand-menu-mode-map "Q" 'expand-menu-quit)
            (define-key expand-menu-mode-map "s" 'expand-menu-select)
            (define-key expand-menu-mode-map "S" 'expand-menu-select)
            (define-key expand-menu-mode-map " " 'next-line)
            (define-key expand-menu-mode-map "n" 'next-line)
            (define-key expand-menu-mode-map "N" 'next-line)
            (define-key expand-menu-mode-map "p" 'previous-line)
            (define-key expand-menu-mode-map "P" 'previous-line)
            (define-key expand-menu-mode-map "?" 'expand-menu-quit)
            (enable-dups 'expand-a-word 'expand-menu-select)
            (enable-dups 'next-line 'expand-menu-next-line)
            (enable-dups 'previous-line 'expand-menu-previous-line)
            )
        )
      )
    )
  )

(defun expand-a-word ()
  "Expand the (partial) word located before point."
  (interactive)
  (let ((target-string)
        (original-string)
        (start-of-string)
        (insert-position)
        (matched-string)
        (possible-matches)
        (case-search case-fold-search))
    (setq insert-position (point-marker))
    (setq start-of-string (+ (re-search-backward "[^A-Za-z0-9_-]") 1))
    (setq original-string (buffer-substring
                                       start-of-string
                                       (marker-position insert-position)))
    ;; check to see whether we should make the search case sensitive i.e. follow
    ;; the same rules as incremental search, if there is an upper case character
    ;; then make it case-sensitive.
    (setq case-fold-search nil)
    (if (not (string-match "[A-Z]" original-string))
        ;; Search should not be case sensitive so change case-fold-search back
        ;; to it's original setting
        (setq case-fold-search case-search)
      ;; Otherwise leave it as a case sensitive search
      )

    (setq target-string (concat "\\b"
                                original-string
                                "[A-Za-z0-9_-]*"))
    ;;
    ;; Search for the target string, but make sure that the target is at
    ;; the start of a word, ie we don't want to match the middle of a word!
    ;;
    (while (re-search-backward target-string nil t)
      (progn
        (if (and (not (member (match-string 0) possible-matches))
                 (not (string= (match-string 0) original-string)))
            (setq possible-matches
                  (cons (match-string 0) possible-matches))
          )
        )
      )
    ;;
    ;; Search in both directions from point
    ;;
    (goto-char insert-position)
    (while (re-search-forward target-string nil t)
      (progn
        (if (and (not (member (match-string 0) possible-matches))
                 (not (string= (match-string 0) original-string)))
            (setq possible-matches
                  (cons (match-string 0) possible-matches))
          )
        )
      )
    ;; Now test if anything was found, if not then just put back the
    ;; target-string
    ;;
    (cond ((= (length possible-matches) 0)
           (setq matched-string original-string)
           )
          ((> (length possible-matches) 1)
           ;; If there are multiple choices then display a menu of all of the
           ;; choices and allow the user to make a selection.
           (progn
             ;; Make sure we are positioned at the point where the search was
             ;; started by the user.
             (goto-char (marker-position insert-position))
             (expand-display possible-matches)
             (if expand-selected-text
                 (setq matched-string expand-selected-text)
               (setq matched-string original-string)
               )
             )
           )
          (t
           ;; Must be exactly one match
           (setq matched-string (car possible-matches))
           )
    )
    (goto-char (marker-position insert-position))
    (if (not (string= matched-string original-string))
        (progn
          (delete-region start-of-string (marker-position insert-position))
          (insert matched-string)
          )
      )

    ;; Make sure that we reset the case-fold-search variable back to entry
    ;; conditions
    (setq case-fold-search case-search)
    )
  )

(provide 'expand-a-word)

;;; expand-a-word.el ends here
