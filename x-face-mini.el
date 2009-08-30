;;; x-face-mini.el --- Create MINI X-Face image for XEmacs/Emacs21+.
 
;; Copyright (C) 1998-2001 Yuuichi Teranishi
 
;; Author: Yuuichi Teranishi <teranisi@gohome.org>
;; Created: 1998-02-25
;; Keywords: Emacs, X-Face, thumbnail, netpbm
 
;; This file is not part of any package.
 
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
 
;;; Commentary:
;;
 
;;; History:
;;
 
;;; Code:
 
(eval-when-compile (require 'cl))
(require 'x-face)
 
(defvar x-face-mini-prog-xbmtopbm "xbmtopbm")
(defvar x-face-mini-prog-xpmtoppm "xpmtoppm")
(defvar x-face-mini-prog-pnmscale "pnmscale")
(defvar x-face-mini-prog-ppmquant "ppmquant")
(defvar x-face-mini-prog-ppmtoxpm "ppmtoxpm")
 
(defvar x-face-mini-cache nil)
(defmacro x-face-mini-cache-put (string width height image)
  "Put IMAGE on the cache with key (STRING WIDTH HEIGHT)."
  (` (progn
       (setq x-face-mini-cache
             (cons (cons (list (, string) (, width) (, height)) (, image))
                   x-face-mini-cache))
       (, image))))
 
(defmacro x-face-mini-cache-get (string width height)
  "Get IMAGE from the cache with the key (STRING WIDTH HEIGHT)."
  (` (cdr (assoc (list (, string) (, width) (, height))
                 x-face-mini-cache))))
 
(defun x-face-mini-make-xpm (x-face-string width height &optional header)
;; Create a thumbnail XPM of X-FACE-STRING with WIDTH and HEIGHT.
;; If X-FACE-STRING is a header string, optional argument HEADER should
;; be given as non-nil. Returns nil if creation of thumbnail glyph was
;; failed.
  (with-temp-buffer
    (let (spec xpm)
      (if header
          (insert x-face-string)
        (insert "X-Face: " x-face-string))
      (save-restriction
        (goto-char (point-min))
        (setq spec (car (x-face-extract-x-face-fields-to-pictures
                         (current-buffer) nil)))
        (when spec
          (with-current-buffer (cdr spec)
            (unwind-protect
                (and
                 (zerop
                  (call-process-region (point-min) (point-max)
                                       (if (string= "xbm" (car spec))
                                           x-face-mini-prog-xbmtopbm
                                         x-face-mini-prog-xpmtoppm) t
                                         '(t nil) nil))
                 (zerop
                  (call-process-region (point-min) (point-max)
                                       x-face-mini-prog-pnmscale t
                                       '(t nil) nil
                                       "-xsize" (int-to-string width)
                                       "-ysize" (int-to-string height)))
                 (zerop
                  (call-process-region (point-min) (point-max)
                                       x-face-mini-prog-ppmtoxpm
                                       t '(t nil)  nil))
                 (setq xpm (buffer-string)))
              (kill-buffer (cdr spec)))))
        xpm))))
 
 
(static-cond
 ((featurep 'xemacs)
  (defun x-face-mini (x-face-string width height &optional header)
    "Create a thumbnail glyph of X-FACE-STRING with WIDTH and HEIGHT.
If X-FACE-STRING is a header string, optional argument HEADER should
be given as non-nil. Returns nil if creation of thumbnail glyph was failed."
    (or (x-face-mini-cache-get x-face-string width height)
        (x-face-mini-cache-put x-face-string width height
                               (make-glyph
                                (vector 'xpm
                                        :data
                                        (x-face-mini-make-xpm
                                         x-face-string
                                         width height header)))))))
 ((eq emacs-major-version 21)
  (defun x-face-mini (x-face-string width height &optional header)
    "Create a thumbnail image of X-FACE-STRING with WIDTH and HEIGHT.
If X-FACE-STRING is a header string, optional argument HEADER should
be given as non-nil. Returns nil if creation of thumbnail glyph was failed."
    (or (x-face-mini-cache-get x-face-string width height)
        (x-face-mini-cache-put x-face-string width height
                               (find-image
                                (list (list
                                       :type 'xpm
                                       :data
                                       (x-face-mini-make-xpm
                                        x-face-string
                                        width height header)
                                       :ascent 'center))))))))
 
(provide 'x-face-mini)
 
;;; x-face-mini.el ends here
