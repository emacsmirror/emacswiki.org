;;; faces+.el --- Extensions to `faces.el'.
;;
;; Filename: faces+.el
;; Description: Extensions to `faces.el'.
;; Author: Drew Adams
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 1996-2018, Drew Adams, all rights reserved.
;; Created: Fri Jun 28 15:07:06 1996
;; Version: 0
;; Package-Requires: ()
;; Last-Updated: Mon Jan  1 11:22:10 2018 (-0800)
;;           By: dradams
;;     Update #: 304
;; URL: https://www.emacswiki.org/emacs/download/faces%2b.el
;; Keywords: faces, local
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x, 24.x, 25.x, 26.x
;;
;; Features that might be required by this library:
;;
;;   `faces', `thingatpt', `thingatpt+'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Extensions to `faces.el'.
;;
;;
;;  New functions defined here:
;;
;;    `face-background-20+', `face-foreground-20+'.
;;
;;
;;
;;  ***** NOTE: The following functions defined in `faces.el' have
;;              been REDEFINED HERE:
;;
;;  `make-face' - Uses `completing-read' in the interactive spec,
;;                with, as default, `symbol-nearest-point'.
;;
;;  `read-face-name' - `highlight' is the default (Emacs < 21 only).
;;
;;
;;  This file should be loaded after loading the standard GNU file
;;  `faces.el'.  So, in your `~/.emacs' file, do this:
;;  (eval-after-load "faces" '(require 'faces+))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2012/08/21 dadams
;;     Call tap-put-thing-at-point-props after load thingatpt+.el.
;; 2012/08/18 dadams
;;     Invoke tap-define-aliases-wo-prefix if thingatpt+.el is loaded.
;; 2011/01/04 dadams
;;     Removed autoload cookies from non def* sexps and non-interactive functions.
;; 2009/11/16 dadams
;;     face-(foreground|background)-20+: Use condition-case.  Don't test for face-attribute.
;; 2006/06/25 dadams
;;     Removed set-face-(fore|back)ground-RGB-(hex-)at-* to facemenu+.el.
;; 2006/06/24 dadams
;;     Remove (see eyedropper.el and palette.el): face-(back|fore)ground-at-*.
;; 2006/06/23 dadams
;;     Removed (see eyedropper.el and palette.el):
;;       picked-(back|fore)ground, pick-(back|fore)ground-at-(point|mouse-pointer).
;;     Renamed: *-at-mouse-pointer to *-at-mouse.
;; 2005/10/31 dadams
;;     Use nil value for initial-value arg to completing-read, everywhere.
;; 2005/07/02 dadams
;;     Added: set-face-(fore|back)ground-RGB-(hex-)at-*.
;;     face-(fore|back)ground-at-mouse-pointer: Discard extra, pending input (Windows bug).
;; 2005/06/30 dadams
;;     Added: face-(fore|back)ground-20+, face-(fore|back)ground-at-mouse-pointer,
;;            face-(fore|back)ground-at-point, picked-(fore|back)ground,
;;            pick-(fore|back)ground-at-mouse-pointer, pick-(fore|back)ground-at-point.
;; 2004/10/10 dadams
;;     read-face-name: for Emacs < 21 only.
;; 2004/09/21 dadams
;;     make-face: created Emacs 21 version.
;; 2001/01/05 dadams
;;     '(lambda...) -> (function (lambda...))
;; 1999/03/17 dadams
;;     1. Protect calls with fboundp.
;;     2. Updated to corrspond with version Emacs 19.34.1.
;; 1996/07/15 dadams
;;     Added redefinition of make-face.
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

(require 'faces)
(eval-when-compile (when (< emacs-major-version 21) (require 'cl))) ;; dolist, pop, push

(require 'thingatpt nil t) ;; (no error if not found): symbol-at-point
(when (and (require 'thingatpt+ nil t) ;; (no error if not found): symbol-nearest-point
           (fboundp 'tap-put-thing-at-point-props)) ; >= 2012-08-21
  (tap-define-aliases-wo-prefix)
  (tap-put-thing-at-point-props))

;;;;;;;;;;;;;;;;;;;;



;; REPLACES ORIGINAL in `faces.el': `highlight' face is the default.
;;
(when (< emacs-major-version 21)
  (defun read-face-name (prompt)
    "Read name of a face (default: \"highlight\") and return it as a symbol.
Prompts with arg PROMPT (a string)."
    (let (face)
      (while (zerop (length face))
        (setq face (completing-read
                    prompt
                    (mapcar (function (lambda (x) (list (symbol-name x)))) (face-list))
                    nil t nil 'minibuffer-history "highlight" t)))
      (intern face))))



;; REPLACES ORIGINAL in `faces.el':
;; Uses `completing-read' in interactive spec, with `symbol-nearest-point'.
;; `symbol-nearest-point' is defined in `thingatpt+.el'.
;; `symbol-at-point' is defined in `thingatpt.el'.
;;
(if (< emacs-major-version 21)
    (defun make-face (name &optional no-resources)
      "Define a new face named NAME, on all frames.
You can modify the font, color, etc of this face with the `set-face-*'
functions.

If NO-RESOURCES is non-nil, then we ignore X resources
and always make a face whose attributes are all nil.

If the face already exists, it is unmodified.
The argument, NAME, is returned."
      (interactive
       (let ((symb (cond ((fboundp 'symbol-nearest-point) (symbol-nearest-point))
                         ((fboundp 'symbol-at-point) (symbol-at-point))
                         (t nil)))
             (enable-recursive-minibuffers t))
         (list (intern (completing-read "Make face: " obarray nil nil
                                        nil nil (symbol-name symb) t)))))
      (unless (internal-find-face name)
        (let ((face (make-vector 12 nil)))
          (aset face 0 'face)
          (aset face 1 name)
          (let* ((frames (frame-list))
                 (inhibit-quit t)
                 (id (internal-next-face-id)))
            (when (fboundp 'make-face-internal) (make-face-internal id))
            (aset face 2 id)
            (while frames
              (set-frame-face-alist (car frames)
                                    (cons (cons name (copy-sequence face))
                                          (frame-face-alist (car frames))))
              (pop frames))
            (push (cons name face) global-face-data))
          ;; When making a face after frames already exist.
          (unless no-resources
            (when (memq window-system '(x w32 win32)) (make-face-x-resource-internal face)))
          ;; Add to face menu.
          (when (fboundp 'facemenu-add-new-face) (facemenu-add-new-face name))))
      name)                             ; Return the NAME.
  (defun make-face (face &optional no-init-from-resources)
    "Define a new face with name FACE, a symbol.
NO-INIT-FROM-RESOURCES non-nil means don't initialize frame-local
variants of FACE from X resources.  (X resources recognized are found
in the global variable `face-x-resources'.)  If FACE is already known
as a face, leave it unmodified.  Value is FACE."
    (interactive
     (let ((symb (cond ((fboundp 'symbol-nearest-point) (symbol-nearest-point))
                       ((fboundp 'symbol-at-point) (symbol-at-point))
                       (t nil)))
           (enable-recursive-minibuffers t))
       (list (intern (completing-read "Make face: " obarray nil nil
                                      nil nil (symbol-name symb) t)))))
    (unless (facep face)
      ;; Make frame-local faces (this also makes the global one).
      (dolist (frame (frame-list))
        (internal-make-lisp-face face frame))
      ;; Add the face to the face menu.
      (when (fboundp 'facemenu-add-new-face)
        (facemenu-add-new-face face))
      ;; Define frame-local faces for all frames from X resources.
      (unless no-init-from-resources
        (make-face-x-resource-internal face)))
    face))

(defun face-foreground-20+ (face &optional frame inherit)
  "A version of `face-foreground' that will work with Emacs 20 and later."
  (condition-case nil
      (face-foreground face frame inherit) ; Emacs 22+.
    (error (face-foreground face frame))))

(defun face-background-20+ (face &optional frame inherit)
  "A version of `face-background' that will work with Emacs 20 and later."
  (condition-case nil
      (face-background face frame inherit) ; Emacs 22+.
    (error (face-background face frame))))

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'faces+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; faces+.el ends here
