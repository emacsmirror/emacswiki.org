;;; ffap-.el --- Extensions to library `ffap.el'
;;
;; Filename: ffap-.el
;; Description: Extensions to library `ffap.el'
;; Author: Drew Adams
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 2005-2017, Drew Adams, all rights reserved.
;; Created: Wed Feb 08 10:47:56 2006
;; Version: 0
;; Package-Requires: ()
;; Last-Updated: Sun Jan  1 09:38:17 2017 (-0800)
;;           By: dradams
;;     Update #: 130
;; URL: http://www.emacswiki.org/ffap-.el
;; Doc URL: http://emacswiki.org/FindFileAtPoint
;; Keywords: files, hypermedia, matching, mouse, convenience
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x, 24.x, 25.x
;;
;; Features that might be required by this library:
;;
;;   `ffap'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  Extensions to library `ffap.el'.
;;
;;  To use this library, add this to your initialization file
;;  (~/.emacs or ~/_emacs):
;;
;;      (require 'ffap-) ; Load this library.
;;
;;  You should explicitly load *only* `ffap-.el', not also `ffap.el'
;;  (this will automatically load `ffap.el').
;;
;;  This library redefines variable `ffap-bindings' as a user option
;;  (not just an internal variable).  The default key bindings in
;;  variable `ffap-bindings' are also changed.  No bindings are
;;  created by this library however; to create the default bindings,
;;  you must call command `ffap-bindings' or evaluate (ffap-bindings).
;;
;;  By default, this library inhibits the behavior of ffap in Dired
;;  buffers, because I usually do *not* want to find the file where
;;  the cursor is.  Instead, I want to use completion to provide the
;;  file name.  This inhibition is done by setting option
;;  `ffap-inhibit-ffap-flag' to t in Dired buffers:
;;
;;    (add-hook 'dired-mode-hook 'ffap-inhibit-here)
;;
;;  If you do *not* want to inhibit ffap in Dired, then do this after
;;  loading this library:
;;
;;    (remove-hook 'dired-mode-hook 'ffap-inhibit-here)
;;
;;
;;  User options defined here:
;;
;;    `ffap-inhibit-ffap-flag'.
;;
;;  Functions defined here:
;;
;;    `ffap-inhibit-ffap-here'.
;;
;;  Internal variables defined here:
;;
;;    `ffap-max-region-length'.
;;
;;
;;  ***** NOTE: The following variables defined in `ffap.el' have
;;              been REDEFINED HERE:
;;
;;  `ffap-bindings' - Use defcustom, not defvar.
;;                    Change mouse bindings.
;;
;;
;;  ***** NOTE: The following functions defined in `ffap.el' have
;;              been REDEFINED HERE:
;;
;;  `ffap-guesser' - Respect `ffap-inhibit-ffap-flag'.
;;
;;  `ffap-read-file-or-url-internal' - Bug fix:
;;     If the cursor is on a URL when you use `find-file-at-point',
;;     and you delete the URL in the minibuffer and then try to use
;;     completion (to a file name), you get an error with message
;;     "Wrong type argument: stringp, nil."
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2016/12/29 dadams
;;     Renamed ffap-max-region-size to ffap-max-region-length, per bug #25243.
;; 2016/12/21 dadams
;;     Added: ffap-max-region-size.
;;     ffap-guesser: Deactivate region if larger than ffap-max-region-size.
;; 2011/01/04 dadams
;;     Added autoload cookies for defcustom.
;; 2006/12/29 dadams
;;     Removed top-level call to ffap-bindings - users do it explicitly or not.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; ;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;;;###autoload
(defcustom ffap-bindings
  '(
    (global-set-key [S-mouse-2] 'ffap-at-mouse)
    (global-set-key [C-S-mouse-2] 'ffap-menu)
    (global-set-key "\C-x\C-f" 'find-file-at-point)
    (global-set-key "\C-x4f"   'ffap-other-window)
    (global-set-key "\C-x5f"   'ffap-other-frame)
    (global-set-key "\C-xd"    'dired-at-point)

    (when (fboundp 'ffap-alternate-file) ; Emacs 22
      (global-set-key "\C-x\C-v" 'ffap-alternate-file)
      (global-set-key "\C-x\C-r" 'ffap-read-only)
      (global-set-key "\C-x4r"   'ffap-read-only-other-window)
      (global-set-key "\C-x5r"   'ffap-read-only-other-frame)
      (global-set-key "\C-x4d"   'ffap-dired-other-window)
      (global-set-key "\C-x5d"   'ffap-dired-other-frame)
      (global-set-key "\C-x\C-d" 'ffap-list-directory))

    (add-hook 'gnus-summary-mode-hook 'ffap-gnus-hook)
    (add-hook 'gnus-article-mode-hook 'ffap-gnus-hook)
    (add-hook 'vm-mode-hook           'ffap-ro-mode-hook)
    (add-hook 'rmail-mode-hook        'ffap-ro-mode-hook)
    ;; (setq dired-x-hands-off-my-keys t) ; the default
    )
  "*List of key-binding forms evaluated by function `ffap-bindings'."
  :type 'sexp :group 'ffap)

;;;###autoload
(defcustom ffap-inhibit-ffap-flag nil
  "*Non-nil means that `ffap-*' functions should do nothing special.
For example, `ffap' then acts simply as `find-file' (or, more
precisely, as `ffap-file-finder')."
  :type 'boolean :group 'ffap)

(defvar ffap-max-region-length 1024       ; See also Emacs bug #25243.
  "Max size of active region used to obtain file-name defaults.
An active region larger than this many characters prevents
`ffap-guesser' using it to obtain a file-name guess.")

;; Get the rest of the code, then redefine some functions.
(require 'ffap)

(defun ffap-inhibit-ffap-here ()
  "Set `ffap-inhibit-ffap-flag' to t in this buffer."
  (set (make-local-variable 'ffap-inhibit-ffap-flag) t))

;; Inhibit ffap in Dired.
(add-hook 'dired-mode-hook 'ffap-inhibit-ffap-here)



;;; REPLACE ORIGINAL `ffap-guesser' defined in `ffap.el'.
;;;
;;; 1. Return nil if `ffap-inhibit-ffap-flag' is non-nil.
;;; 2. Deactivate a large active region first.  Emacs bug #25243.
;;;
(defun ffap-guesser nil
  "Return file or URL or nil, guessed from text around point."
  (and (not ffap-inhibit-ffap-flag)
       (let ((mark-active  (and mark-active
                                (< (buffer-size) ffap-max-region-length))))
         (or (and ffap-url-regexp
                  (ffap-fixup-url (or (ffap-url-at-point)
                                      (ffap-gopher-at-point))))
             (ffap-file-at-point)       ; may yield url!
             (ffap-fixup-machine (ffap-machine-at-point))))))



;;; REPLACE ORIGINAL defined in `ffap.el'.
;;; Bug fix.  It comes from the Emacs 21 code.
;;;
(when (= emacs-major-version 20)
  (defun ffap-read-file-or-url-internal (string dir action)
    (unless dir
      (setq dir default-directory))
    (unless string
      (setq string default-directory))
    (if (ffap-url-p string)
        (ffap-read-url-internal string dir action)
      (read-file-name-internal string dir action))))

;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'ffap-)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ffap-.el ends here
