;;; setnu+.el --- Extensions to `setnu.el'.
;;
;; Filename: setnu+.el
;; Description: Extensions to `setnu.el'.
;; Author: Drew Adams
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 2000-2015, Drew Adams, all rights reserved.
;; Created: Thu Nov 30 08:51:07 2000
;; Version: 0
;; Package-Requires: ()
;;; Last-Updated: Thu Jan  1 11:13:14 2015 (-0800)
;;           By: dradams
;;     Update #: 177
;; URL: http://www.emacswiki.org/setnu%2b.el
;; Doc URL: http://emacswiki.org/LineNumbers
;; Keywords: lines
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x, 24.x, 25.x
;;
;; Features that might be required by this library:
;;
;;   `avoid', `fit-frame', `frame-cmds', `frame-fns', `misc-fns',
;;   `setnu', `strings', `thingatpt', `thingatpt+'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Extensions to `setnu.el'.
;;
;;  1. Enhances setnu mode so that it fits the frame to accomodate the
;;     line numbers.  This only occurs in one-window frames.
;;
;;  2. Fixes setnu mode so that deletions of newlines are taken into
;;     account.  This code is based on `jde.el', by Paul Kinnucan
;;     <paulk@mathworks.com>, which, in turn, was apparently based on
;;     code by Jonathan Epstein <Jonathan_Epstein@nih.gov>.
;;     I'm not sure this fix is still needed.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2011/01/04 dadams
;;     Added autoload cookies for defgroup and defcustom.
;; 2010/12/04 dadams
;;     Removed make-local-hook calls (no longer exists in Emacs 24).
;; 2006/10/02 dadams
;;     Added soft require of fit-frame.el.  Thx to Andreas Roehler.
;; 2005/12/26 dadams
;;     Added: setnu-set-glyph-face.
;; 2005/04/21 dadams
;;     setnu-mode: Fit frame afterward, if `one-window-p.
;;     Changed defvar to defcustom.  Added defgroup.
;;     Renamed: setnu+-newline-deletion-check to setnu+-newline-deletion-flag.
;;     Added: setnu+-fit-frame-flag.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'setnu)

(require 'frame-cmds nil t) ;; enlarge-frame-horizontally
(require 'fit-frame nil t) ;; fit-frame

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (remove-hook 'before-change-functions 'setnu-before-change-function)
;;; (remove-hook 'after-change-functions 'setnu-after-change-function)
;;; (make-local-hook 'before-change-functions)
;;; (make-local-hook 'after-change-functions)

;;;###autoload
(defgroup Setnu-Plus nil "Setnu options." :group 'convenience)

;;;###autoload
(defcustom setnu+-newline-deletion-flag t
  "*Non-nil means check for newline deletions when numbering lines
via `setnu'."
  :type 'boolean :group 'Setnu-Plus)
(make-variable-buffer-local 'setnu+-newline-deletion-flag)

;;;###autoload
(defcustom setnu+-fit-frame-flag t
  "*Non-nil means `setnu-mode' fits frame to buffer, if `one-window-p'.
This has no effect if function `fit-frame' is not defined."
  :type 'boolean :group 'Setnu-Plus :group 'Fit-Frame)

(defun setnu+-after-change (start end length)
  "When in setnu-mode and newlines have been deleted, refreshes
by turning setnu-mode off, then back on.
START and END are the positions of the beginning and end of the range
  of changed text.
LENGTH is the length in bytes of the pre-change text replaced by that
  range. (For an insertion, the pre-change length is zero; for a
  deletion, that length is the number of bytes deleted and the
  post-change beginning and end are at the same place.)"
  (if setnu-mode
      (when (or (and (> length 0) setnu+-newline-deletion-flag)
                (string-match "[\n\r]" (buffer-substring-no-properties start end)))
        (run-with-timer 0.001 nil (lambda () (setnu-mode) (setnu-mode)))) ; Toggle
    (setq setnu+-newline-deletion-flag nil)))

(defun setnu+-before-change (start end)
  "Determines whether any newlines are about to be deleted.
START and END are as for `setnu+-after-change'."
  (when (and setnu-mode (> end start))
    (setq setnu+-newline-deletion-flag
          (string-match "[\n\r]" (buffer-substring-no-properties start end)))))


;; REPLACES ORIGINAL in `setnu.el':
;; 1. Adds/removes before/after change hooks.
;; 2. Fits frame to buffer if `one-window-p'.
;;;###autoload
(defun setnu-mode (&optional arg)
  "Toggle setnu-mode on/off.
Positive prefix argument ARG turns it on; negative turns it off.
When on, a line number appears to the left of each line."
  (interactive "P")
  (let ((oldmode setnu-mode)
        (inhibit-quit t))
    (setq setnu-mode (if arg
                         (natnump (prefix-numeric-value arg))
                       (not setnu-mode)))
    (unless (eq oldmode setnu-mode)
      (cond (setnu-mode
             (add-hook 'before-change-functions 'setnu-before-change-function t t)
             (add-hook 'before-change-functions 'setnu+-before-change t t)
             (add-hook 'after-change-functions 'setnu-after-change-function t t)
             (add-hook 'after-change-functions 'setnu+-after-change t t)
             (setnu-mode-on))
            (t
             (remove-hook 'before-change-functions 'setnu-before-change-function t)
             (remove-hook 'before-change-functions 'setnu+-before-change t)
             (remove-hook 'after-change-functions 'setnu-after-change-function t)
             (remove-hook 'after-change-functions 'setnu+-after-change t)
             (setnu-mode-off)))
      (when (and (one-window-p t) setnu+-fit-frame-flag (fboundp 'fit-frame))
        (fit-frame)
        (when (and setnu-mode (fboundp 'enlarge-frame-horizontally))
          (enlarge-frame-horizontally   ; Defined in `frame-cmds.el'.
           (length (format setnu-line-number-format
                           (count-lines (point-min) (point-max))))))))))

;; REPLACES ORIGINAL in `setnu.el':
;; Make `font-lock-face' property nil.
;;
(unless (and setnu-running-under-xemacs (fboundp 'set-glyph-face))
  (defun setnu-set-glyph-face (g face)
    (put-text-property 0 (length g) 'face face g)
    (put-text-property 0 (length g) 'font-lock-face nil g)))


;;;;;;;;;;;;;;;;;;;;;;;

(provide 'setnu+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; setnu+.el ends here
