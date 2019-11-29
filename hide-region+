;;; hide-region.el --- hide regions of text using overlays
;;
;; Copyright (C) 2001, 2005  Mathias Dahl
;;
;; Version: 1.0.2
;; Keywords: hide, region
;; Author: Mathias Dahl <mathias.rem0veth1s.dahl@gmail.com>
;; Maintainer: Corwin Brust <corwin@bru.st>
;; Contributers: <your-name-in-comments>
;; URL: https://gist.github.com/mplscorwin/7832d7de7530ff355bff5778e46f477d
;;
;; This file is not part of GNU Emacs.
;;
;; This is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;;; Commentary:
;;
;; The version on GITHUB is the latest STABLE version.  It may (will)
;; lag the development copy, from:
;;
;;   https://www.emacswiki.org/emacs/download/hide-region%2b
;;
;; The function `hide-region-hide' hides the region. You can hide many
;; different regions and they will be "marked" by two configurable
;; strings (so that you know where the hidden text is).
;;
;; The hidden regions are pushed on a kind of hide-region \"ring".
;;
;; The function `hide-region-unhide' "unhides" one region, starting
;; with the last one hidden.
;;
;; The best is to try it out. Test on the following:
;;
;; Test region 1
;; Test region 2
;; Test region 3
;;
;; If you are annoyed by the text getting "stuck" inside the hidden
;; regions, call the function `hide-region-setup-keybindings' to setup
;; local keybindings to a couple of functions trying to be smart and
;; guessing if the point is inside a hidden region and if so, move out
;; of it in the correct direction.
;;
;;; Version history
;;
;; Version 1.0.2
;;
;; * Updated defvar to defvar-local and removed associated
;;   (make-variable-buffer-local) thoughout.
;;
;; Version 1.0.1
;;
;; * Seems that the getting-stuck problem have disappeared since Emacs
;; 21.3 was released, so no need anymore for the extra movement
;; commands.
;;
;; * Added the intangible property to the overlays because that seemed
;; to remove a minor getting-stuck problem (the overlay "ate" one
;; keystroke) when navigating around an overlay.  Adding the intangible
;; property makes it impossible to navigate into the overlay.
;;
;; * Added custom option to propertize the overlay markers for greater
;; visibility.
;;
;; * Minor code cleanup
;;
;;
;;; Bugs
;;
;; Probably many, but none that I know of.  Comments and suggestions
;; are welcome!

;; Modify by corwin
;;  return new overlay from `hide-region-hide'
;;  display hidden text as an overlay
;;  add text properties for "handles" (pre/postfix)
;;   as setup to make them clickable soonish
;;  move "handles" face out to a defcustom
;;  defvar to deflocal
;;  rm :type from defc immediately following defg
;;  docstring and misc
;; Modify by yupeng
;; set variable `hide-region-overlays' as buffer local
;; Add to function:
;; `hide-region-unhide-below'
;; unhide a region just below the point
;;  `hide-region-unhide-all'
;;  unhide all the region in the current buffer
;; `hide-region-toggle'
;; toggle all the gide region in the current buffer

;;; Code:

(defgroup hide-region nil
  "Functions to hide region using an overlay with the invisible property.
The text is not affected."
  :prefix "hide-region-"
  :group 'convenience)

(defcustom hide-region-before-string "@["
  "String to display before the beginning of each invisible region.

This string is not really placed in the text, it is just shown in the overlay."
  :type '(string))

(defcustom hide-region-after-string "]@"
  "String to place after the end each invisible region.

This string is not really placed in the text, it is just shown in the overlay."
  :type '(string))

(defcustom hide-region-propertize-markers t
  "If non-nil, add text properties to the region markers."
  :type 'boolean)

;; (defvar my-keymap (let ((map (make-sparse-keymap)))
;; 		    (define-key map [mouse-2] 'hide-region-toggle)
;; 		    (define-key map [follow-link] 'mouse-face)
;; 		    map))

(defcustom hide-region-marker-properties
  '(font-lock-face region keymap my-keymap mouse-face underline)
  "If non-nil, add text properties to the region markers."
  :type '(repeat (list sexp sexp)))

(defvar-local hide-region-overlays nil
  "Variable to store the regions we put an overlay on.")

(defvar-local hide-region-show-flag nil
  "Indicates whether regions will be shown or hidden when toggled.

Controls the behaviour of `hide-region-toggle'. If nil, regions
are currently hidden and will be shown upon the next call to
`hide-region-toggle'.  Otherwise they will be shown.")

(defun hide-region-hide ()
  "Hide the current region.

Add an overlay to the current region (from `mark' to `point') making it
invisable, then add to the \"ring\" in `hide-region-overlays'.  Use this
method when you have selected some text you want to hide."
  (interactive)
  (let ((new-overlay (make-overlay (mark) (point)))
	(help-text (buffer-substring-no-properties (mark) (point))))
    (setq hide-region-overlays
	  (append
	   (list new-overlay) hide-region-overlays))
    (overlay-put new-overlay 'invisible t)
    (overlay-put new-overlay 'intangible t)
    (overlay-put new-overlay 'evaporate t)
    ;;(overlay-put new-overlay 'help-echo help-text);;"click to hide"?

    ;; setup the handles
    ;; TODO: defsubst or defmarco or something for the duplicated bit
    (overlay-put new-overlay 'before-string
                 (if hide-region-propertize-markers
                     (apply 'propertize hide-region-before-string
			    'help-echo help-text
			    hide-region-marker-properties) ;region
                   hide-region-before-string))
    (overlay-put new-overlay 'after-string
                 (if hide-region-propertize-markers
                     (apply 'propertize
			    hide-region-after-string
			    'help-echo help-text
			    hide-region-marker-properties) ;region)
                   hide-region-after-string))

    ;; return the overlay
    new-overlay))

(defun hide-region-unhide ()
  "Unhide the last region hidden.

The last region found on the \"ring\" is removed from `hide-region-overlays'
and the overlay is removed revealing the original content in the buffer."
  (interactive)
  (if (car hide-region-overlays)
      (progn
	(delete-overlay (car hide-region-overlays))
	(setq hide-region-overlays (cdr hide-region-overlays)))))

(defun hide-region-unhide-below (point)
  "Unhide a region just below the POINT."
  (interactive "d")
  (let ((number (length hide-region-overlays))
        (tmp-overlay nil)
        (tmp-start nil)
        (tmp-len nil)
        (tmp-number nil))
    (setq number (- number 1))
    (while (>= number 0)
      (setq tmp-start (overlay-start (nth number hide-region-overlays)))
      (if (and (>  tmp-start point) (or (eq tmp-len nil) (< tmp-start tmp-len)))
          (progn
            (setq tmp-len tmp-start)
            (setq tmp-number number)))
      (setq number (- number 1)))
    (if tmp-number
        (progn
          (setq tmp-overlay (nth tmp-number hide-region-overlays))
          (delete-overlay tmp-overlay)
          (if (equal tmp-number 0)
              (setq hide-region-overlays (cdr hide-region-overlays))
          (delq tmp-overlay hide-region-overlays)))
      (if (car hide-region-overlays)
      (progn
	(delete-overlay (car hide-region-overlays))
	(setq hide-region-overlays (cdr hide-region-overlays)))))
    ))

(defun hide-region-unhide-all ()
  "Unhide all the region in the current buffer."
  (interactive)
  (while hide-region-overlays
    (if (car hide-region-overlays)
        (progn
          (delete-overlay (car hide-region-overlays))
          (setq hide-region-overlays (cdr hide-region-overlays)
		hide-region-show-flag t)))))

(defun hide-region-toggle (&optional pos)
  "Toggle visability of all hide-region-overlays in the current buffer.

At some pooint this should accept POS to specify a single region to toggle."
  (interactive)
  (ignore pos) ;; TODO: support toggle of a single region
  (let ((number (length hide-region-overlays)))
    (while (>= (setq number (- number 1)) 0)
      (overlay-put (nth number hide-region-overlays)
		   'invisible hide-region-show-flag))
    (setq hide-region-show-flag (not hide-region-show-flag))))

(provide 'hide-region)
;;; hide-region+.el ends here
