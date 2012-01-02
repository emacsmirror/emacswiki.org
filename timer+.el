;;; timer+.el --- Extensions to `timer.el'.
;;
;; Filename: timer+.el
;; Description: Extensions to `timer.el'.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 1996-2012, Drew Adams, all rights reserved.
;; Created: Mon Jul 15 08:45:19 1996
;; Version: 20.0
;; Last-Updated: Sun Jan  1 14:05:08 2012 (-0800)
;;           By: dradams
;;     Update #: 80
;; URL: http://www.emacswiki.org/cgi-bin/wiki/timer+.el
;; Keywords: processes, calendar
;; Compatibility: GNU Emacs 20.x
;;
;; Features that might be required by this library:
;;
;;   `advice', `advice-preload', `apropos', `apropos+',
;;   `apropos-fn+var', `avoid', `backquote', `bookmark', `bookmark+',
;;   `bookmark+-1', `bookmark+-bmu', `bookmark+-key',
;;   `bookmark+-lit', `bookmark+-mac', `bytecomp', `cl', `cus-edit',
;;   `cus-face', `cus-load', `cus-start', `custom', `dired',
;;   `dired+', `dired-aux', `dired-x', `doremi', `easymenu',
;;   `ediff-diff', `ediff-help', `ediff-init', `ediff-merg',
;;   `ediff-mult', `ediff-util', `ediff-wind', `el-swank-fuzzy',
;;   `ffap', `ffap-', `fit-frame', `frame-cmds', `frame-fns',
;;   `fuzzy', `fuzzy-match', `help+20', `hexrgb', `icicles',
;;   `icicles-cmd1', `icicles-cmd2', `icicles-face', `icicles-fn',
;;   `icicles-mcmd', `icicles-mode', `icicles-opt', `icicles-var',
;;   `image-dired', `info', `info+', `kmacro', `levenshtein',
;;   `menu-bar', `menu-bar+', `misc-cmds', `misc-fns', `mkhtml',
;;   `mkhtml-htmlize', `mouse3', `mwheel', `naked', `pp', `pp+',
;;   `regexp-opt', `ring', `ring+', `second-sel', `strings',
;;   `thingatpt', `thingatpt+', `timer', `unaccent', `w32-browser',
;;   `w32browser-dlgopen', `wid-edit', `wid-edit+', `widget'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Extensions to `timer.el'.
;;
;;
;;  ***** NOTE: The following function defined in `timer.el' has
;;              been REDEFINED HERE:
;;
;;  `cancel-function-timers' -
;;     This now uses `completing-read' in the interactive spec, with,
;;     as default, `symbol-nearest-point'.
;;
;;
;;  This file should be loaded after loading the standard GNU file
;;  `timer.el'.  So, in your `~/.emacs' file, do this:
;;  (eval-after-load "timer" '(require 'timer+))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2005/10/31 dadams
;;     Use nil as init-value arg in calls to completing-read, everywhere.
;; 1999/03/17 dadams
;;     1. Protect with fboundp.
;;     2. Updated to correspond to Emacs 19.34.1 version.
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

(require 'timer)
(and (< emacs-major-version 21)
     (eval-when-compile (require 'cl))) ;; pop (plus, for Emacs <20: when)

(require 'thingatpt nil t) ;; (no error if not found): symbol-at-point
(require 'thingatpt+ nil t) ;; (no error if not found): symbol-nearest-point
(require 'icicles nil t) ;; (no error if not found): completing-read

;;;;;;;;;;;;;;;;;;;;;;;


;; REPLACES ORIGINAL in `timer.el':
;; Uses `completing-read' in interactive spec, with `symbol-nearest-point'.
;; `symbol-nearest-point' is defined in `thingatpt+.el'.
;; `symbol-at-point' is defined in `thingatpt.el'.
;; `function-called-at-point' is defined in `help.el'.
;;;###autoload
(defun cancel-function-timers (function)
  "Cancel all timers set by `run-at-time' that would run FUNCTION."
  (interactive
   (let ((fn (cond ((fboundp 'symbol-nearest-point) (symbol-nearest-point))
                   ((fboundp 'symbol-at-point) (symbol-at-point))
                   ((fboundp 'function-called-at-point)
                    (function-called-at-point)) ; Defined in `help.el'.
                   (t nil)))
         (enable-recursive-minibuffers t))
     (list (intern (completing-read "Cancel timers of function: "
                                    obarray 'fboundp t nil nil
                                    (and fn (symbol-name fn)) t)))))
  (let ((tail timer-list))
    (while tail
      (when (eq (aref (car tail) 5) function)
        (setq timer-list (delq (car tail) timer-list)))
      (pop tail)))
  (let ((tail timer-idle-list))
    (while tail
      (when (eq (aref (car tail) 5) function)
        (setq timer-idle-list (delq (car tail) timer-idle-list)))
      (pop tail))))


;;;;;;;;;;;;;;;;;;;;;;;

(provide 'timer+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; timer+.el ends here
