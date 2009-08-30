;;; hide-region.el --- hide regions of text using overlays
;;
;; Copyright (C) 2001, 2005  Mathias Dahl
;;
;; Version: 1.0.1
;; Keywords: hide, region
;; Author: Mathias Dahl <mathias.rem0veth1s.dahl@gmail.com>
;; Maintainer: Mathias Dahl
;; URL: http://mathias.dahl.net/pgm/emacs/elisp/hide-region.el
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
;; The function `hide-region-hide' hides the region. You can hide many
;; different regions and they will be "marked" by two configurable
;; strings (so that you know where the hidden text is).
;;
;; The hidden regions is pushed on a kind of hide-region \"ring".
;;
;; The function `hide-region-unhide' "unhides" one region, starting
;; with the last one you hid.
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
;; Version 1.0.1
;;
;; * Seems that the getting-stuck problem have disappeared since Emacs
;; 21.3 was released, so no need anymore for the extra movement
;; commands.
;;
;; * Added the intangible property to the overlays because that seemed
;; to remove a minor getting-stuck problem (the overlay "ate" one
;; keystroke) when navigating around an overlay. Adding the intangible
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
;; Probably many, but none that I know of. Comments and suggestions
;; are welcome!

;;; Code:

(defgroup hide-region nil
  "Functions to hide region using an overlay with the invisible
property. The text is not affected."
  :prefix "hide-region-"
  :group 'convenience)

(defcustom hide-region-before-string "@["
  "String to mark the beginning of an invisible region. This string is
not really placed in the text, it is just shown in the overlay"
  :type '(string)
  :group 'hide-region)

(defcustom hide-region-after-string "]@"
  "String to mark the beginning of an invisible region. This string is
not really placed in the text, it is just shown in the overlay"
  :type '(string)
  :group 'hide-region)

(defcustom hide-region-propertize-markers t
  "If non-nil, add text properties to the region markers."
  :type 'boolean
  :group 'hide-region)

(defvar hide-region-overlays nil
  "Variable to store the regions we put an overlay on.")

(defun hide-region-hide ()
  "Hides a region by making an invisible overlay over it and save the
overlay on the hide-region-overlays \"ring\""
  (interactive)
  (let ((new-overlay (make-overlay (mark) (point))))
    (setq hide-region-overlays
	  (append
	   (list new-overlay) hide-region-overlays))
    (overlay-put new-overlay 'invisible t)
    (overlay-put new-overlay 'intangible t)
    (overlay-put new-overlay 'before-string
                 (if hide-region-propertize-markers
                     (propertize hide-region-before-string
                                 'font-lock-face 'region)
                   hide-region-before-string))
    (overlay-put new-overlay 'after-string
                 (if hide-region-propertize-markers
                     (propertize hide-region-after-string
                                 'font-lock-face 'region)
                   hide-region-after-string))))

(defun hide-region-unhide ()
  "Unhide a region at a time, starting with the last one hidden and
deleting the overlay from the hide-region-overlays \"ring\"."
  (interactive)
  (if (car hide-region-overlays)
      (progn
	(delete-overlay (car hide-region-overlays))
	(setq hide-region-overlays (cdr hide-region-overlays)))))

(provide 'hide-region)

;;; hide-region.el ends here
