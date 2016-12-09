;;; wimpy-del.el --- Require confirmation for large region deletion.
;;
;; Filename: wimpy-del.el
;; Description: Require confirmation for large region deletion.
;; Author: Bard Bloom, bard@theory.lcs.mit.edu, Drew Adams
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 1996-2016, Drew Adams, all rights reserved.
;; Copyright (C) Bard Bloom, June 1989
;; Created: Wed Nov 22 14:57:17 1995
;; Version: 0
;; Package-Requires: ()
;; Last-Updated: Fri Dec  9 06:55:33 2016 (-0800)
;;           By: dradams
;;     Update #: 192
;; URL: http://www.emacswiki.org/wimpy-del.el
;; Keywords: region, cut, kill, copy
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x, 24.x, 25.x
;;
;; Features that might be required by this library:
;;
;;   `avoid', `frame-fns', `misc-fns', `strings', `thingatpt',
;;   `thingatpt+'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Require confirmation for large region deletion.
;;  Replacements for `kill-region' and `clipboard-kill-region'.
;;
;;  Original code by Bard Bloom, bard@theory.lcs.mit.edu.
;;  Modifications by Drew Adams.
;;
;;  This provides `kill-region-wimpy', a replacement for
;;  `kill-region'.  If the region is larger than `wimpy-delete-size'
;;  characters, then `kill-region-wimpy' asks you if you really want
;;  to delete it.  The prompt tells you how big the region is, and
;;  indicates the region's text.  This can thus also be used as an
;;  alternative to `C-x C-x' to determine where the region is.
;;
;;  Similarly, `clipboard-kill-region-wimpy' is provided as a
;;  replacement for `clipboard-kill-region'.
;;
;;  New functions defined here:
;;
;;    `clipboard-kill-region-wimpy', `kill-region-wimpy'.
;;
;;  New user options (variables) defined here:
;;
;;    `wimpy-delete-dopey-message', `wimpy-delete-size'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2016/12/09 dadams
;;     clipboard-kill-region-wimpy: Emacs 25 renamed x-select-enable-clipboard to gui-select-enable-clipboard.
;; 2014/07/15 dadams
;;     kill-region-wimpy: Let mode-line-pos.el highlight region specially in mode line.
;;                        Added optional arg MSGP.
;; 2010/01/04 dadams
;;     Removed autoload cookies from defvars.
;; 2005/11/08 dadams
;;     Updated menu-enable properties.
;; 1996/02/06 dadams
;;     kill-region-wimpy: No msg if wimpy-delete-dopey-message is nil.
;; 1995/12/28 dadams
;;     kill-region-wimpy: Interactive allows for completion.el:
;;                        Use inactive mark too.
;; 1995/12/05 dadams
;;     kill-region-wimpy: Take completion.el into account:
;;                        Remove the most recent completion.
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

(require 'frame-fns nil t) ;; (no error if not found): flash-ding
(require 'strings nil t) ;; (no error if not found): region-description

;; Free vars here: CMPL-LAST-INSERT-LOCATION, CMPL-ORIGINAL-STRING,
;;                 COMPLETION-TO-ACCEPT

(defvar cmpl-last-insert-location)
(defvar cmpl-original-string)
(defvar completion-to-accept)

;;;;;;;;;;;;;;;;;

(defvar wimpy-delete-size 2000
   "*`kill-region-wimpy' asks you to confirm killing more than this many chars.
Setting this to nil inhibits deletion confirmation altogether.")

(defvar wimpy-delete-dopey-message "OK, region not killed."
  "*Message `kill-region-wimpy' displays when told not to delete the region.
If nil, no message is displayed.")

;; CMPL-LAST-INSERT-LOCATION, CMPL-ORIGINAL-STRING and COMPLETION-TO-ACCEPT
;; are free here.
;;;###autoload
(defun kill-region-wimpy (beg end &optional msgp)
  "Kill the text between BEG and END, putting it in the kill ring.
\(Interactively, uses the region.)

If the previous command was a completion, just remove the completion.

Else, if the region is > `wimpy-delete-size', you must confirm the kill."
  (interactive
   (if (and (eq last-command 'complete)  (boundp 'cmpl-last-insert-location)) ; See `completion.el'.
       (let ((mark-even-if-inactive  t)) (list (region-beginning) (region-end)))
     (list (region-beginning) (region-end) 'MSGP)))
  (cond ((not msgp) (kill-region beg end))
        (;; Remove the most recent completion----See `completion.el'.
         (and (eq last-command 'complete) (boundp 'cmpl-last-insert-location))
         (delete-region (point) cmpl-last-insert-location)
         (insert cmpl-original-string)  ; Defined in `completion.el'.
         (setq completion-to-accept  nil)) ; Defined in `completion.el'.
        ;; Only kill large region if user confirms.
        ((and wimpy-delete-size
              (> (- end beg) wimpy-delete-size)
              (let ((icicle-change-region-background-flag  nil) ; Inhibit changing face `region' in minibuffer.
                    (modelinepos-region-acting-on ; `mode-line-pos.el' highlights region in mode line.
                     (and (fboundp 'use-region-p)
                          (or (use-region-p)  (and (boundp 'isearchp-reg-beg)  isearchp-reg-beg)))))
                (when (fboundp 'flash-ding) (flash-ding))
                (not (y-or-n-p (if (fboundp 'region-description)
                                   (region-description (- (frame-width) 6) "Really kill?:     " "    " beg end)
                                 (message "Really kill region (%d chars)? " (- end beg)))))))
         (when (and msgp  wimpy-delete-dopey-message) (message "%s" wimpy-delete-dopey-message)))
        (t (kill-region beg end))))
     ; Kill small region.

;;; Identical to `clipboard-kill-region', defined in `menu-bar.el',
;;; except that it uses `kill-region-wimpy' instead of `kill-region'.
;;;###autoload
(defun clipboard-kill-region-wimpy (beg end)
  "Kill the region, and save it in the X clipboard.
Interactively, uses the current region.
Otherwise, BEG and END are the region boundaries.

If the previous command was a completion, just remove the completion.

Else, if the region is > `wimpy-delete-size', you must confirm the kill."
  (interactive "r")
  (let ((gui-select-enable-clipboard  t)
        (x-select-enable-clipboard    t))
    (kill-region-wimpy beg end)))

;;; For use in menu-bar.
(put 'clipboard-kill-region-wimpy 'menu-enable '(and mark-active (not buffer-read-only)))
(put 'kill-region-wimpy 'menu-enable '(and mark-active (not buffer-read-only)))

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'wimpy-del)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; wimpy-del.el ends here
