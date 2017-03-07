;;; mb-depth+.el --- Indicate minibuffer-depth in prompt
;;
;; Filename: mb-depth+.el
;; Description: Indicate minibuffer-depth in prompt
;; Author: Drew Adams
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 2006-2017, Drew Adams, all rights reserved.
;; Created: Sat Nov 18 16:37:53 2006
;; Version: 0
;; Package-Requires: ()
;; Last-Updated: Tue Mar  7 08:44:40 2017 (-0800)
;;           By: dradams
;;     Update #: 86
;; URL: https://www.emacswiki.org/emacs/download/mb-depth%2b.el
;; Doc URL: http://emacswiki.org/MinibufferDepthIndicator
;; Keywords: convenience
;; Compatibility: GNU Emacs: 22.x, 23.x, 24.x, 25.x
;;
;; Features that might be required by this library:
;;
;;   `mb-depth'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  This library modifies library `mb-depth.el' slightly, to let you
;;  decide what depth indicator format to use, and which face to
;;  highlight it in.  It provides a minor tweak to function
;;  `minibuffer-depth-setup', which, in `mb-depth.el', hard-codes the
;;  face and indicator format.
;;
;;  In addition, the default indicator format is simpler than that in
;;  `mb-depth.el', and the default face is `default' instead of
;;  `highlight'.
;;
;;  Faces defined here:
;;
;;    `minibuffer-depth-indicator'.
;;
;;  User options defined here:
;;
;;    `minibuffer-depth-indicator-format'.
;;
;;
;;  To use this library, put this in your init file (~/.emacs):
;;
;;  ;; Use `condition-case' because if `mb-depth.el' can't be found,
;;  ;; then `mb-depth+.el' is not provided.
;;  (condition-case nil (require 'mb-depth+ nil t) (error nil))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2008/08/04
;;     Added Commentary note about using condition-case when requiring mb-depth+.el.
;; 2008/08/01 dadams
;;     Updated for mb-depth.el version that is included in Emacs 23 (renamings).
;;     Renamed: mb-depth-indicator to minibuffer-depth-indicator,
;;              mb-depth-indicator-format to minibuffer-depth-indicator-format.
;;     Do nothing if we cannot load mb-depth.el.
;; 2006/12/31 dadams
;;     Changed require of mb-depth.el to soft require.
;; 2006/11/18 dadams
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

;; Do nothing if we cannot load `mb-depth.el'.
(when (require 'mb-depth nil t)
  ;; minibuffer-depth, minibuffer-depth-indicator-function,
  ;; minibuffer-depth-overlay, minibuffer-depth-setup

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defface minibuffer-depth-indicator '((t (:inherit default)))
    "*Face used to indicate minibuffer depth."
    :group 'convenience :group 'faces)

  (defcustom minibuffer-depth-indicator-format "%d) "
    "*Format string for minibuffer depth indicator."
    :type 'string :group 'convenience)


  ;; REPLACE original defined in `mb-depth.el'.
  ;; Use face `minibuffer-depth-indicator' and option `minibuffer-depth-indicator-format'.
  ;;
  ;; This function goes on `minibuffer-setup-hook'.
  (defun minibuffer-depth-setup ()
    "Set up a minibuffer for `minibuffer-depth-indicate-mode'.
The prompt should already have been inserted."
    (when (> (minibuffer-depth) 1)
      (setq minibuffer-depth-overlay (make-overlay (point-min) (1+ (point-min))))
      (overlay-put minibuffer-depth-overlay 'before-string
                   (if minibuffer-depth-indicator-function
                       (funcall minibuffer-depth-indicator-function (minibuffer-depth))
                     (propertize (format minibuffer-depth-indicator-format (minibuffer-depth))
                                 'face 'minibuffer-depth-indicator)))
      (overlay-put minibuffer-depth-overlay 'evaporate t)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (provide 'mb-depth+)

  )

;;; mb-depth+.el ends here
