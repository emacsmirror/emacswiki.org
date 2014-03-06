;;; eldoc-extension.el --- Some extension for eldoc

;; Filename: eldoc-extension.el
;; Description: Some extension for eldoc
;; Author: Andy Stewart lazycat.manatee@gmail.com
;; Maintainer: rubikitch <rubikitch@ruby-lang.org>
;; Copyright (C) 2008, 2009, Andy Stewart, all rights reserved.
;; Created: 2008-12-07 21:44:29
;; Version: 0.1
;; Last-Updated: 2008-12-07 21:44:31
;;           By: Andy Stewart
;; URL:
;; Keywords: eldoc
;; Compatibility: GNU Emacs 23.0.60.1
;;
;; Features that might be required by this library:
;;
;; `eldoc'
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Some extension for eldoc
;;

;;; Installation:
;;
;; Put eldoc-extension.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'eldoc-extension)
;;
;; No need more.

;;; Change log:
;;
;; 2008/12/07
;;      First released.
;;

;;; Acknowledgements:
;;
;;
;;

;;; TODO
;;
;;
;;

;;; Require
(require 'eldoc)

;;; Code:

(defun eldoc-argument-list (string)
  "Down case and fortify STRING for use with `eldoc-mode'."
  (propertize (downcase string)
              'face 'font-lock-variable-name-face))

(defadvice eldoc-highlight-function-argument
    (around my-formatting (sym args index) compile activate preactivate)
  "Replace original to apply my style of formatting."
  ;; HACK: intercept the call to eldoc-docstring-format-sym-doc at the
  ;; end of the advices function. This is obviously brittle, but the
  ;; alternative approach of copy/pasting the original also has
  ;; downsides...
  (cl-flet ((eldoc-docstring-format-sym-doc
          (sym doc face)
          (let* ((function-name (propertize (symbol-name sym)
                                            'face face))
                 (spec (format "%s (%s)" function-name doc))
                 (docstring (or (eldoc-docstring-first-line
                                 (documentation sym t))
                                "Undocumented."))
                 (docstring (propertize docstring
                                        'face 'font-lock-doc-face))
                 ;; TODO: currently it strips from the start of spec by
                 ;; character instead of whole arguments at a time.
                 (fulldoc (format "%s: %s" spec docstring))
                 (ea-width (1- (window-width (minibuffer-window)))))
            (cond ((or (<= (length fulldoc) ea-width)
                       (eq eldoc-echo-area-use-multiline-p t)
                       (and eldoc-echo-area-use-multiline-p
                            (> (length docstring) ea-width)))
                   fulldoc)
                  ((> (length docstring) ea-width)
                   (substring docstring 0 ea-width))
                  ((>= (- (length fulldoc) (length spec)) ea-width)
                   docstring)
                  (t
                   ;; Show the end of the partial symbol name, rather
                   ;; than the beginning, since the former is more likely
                   ;; to be unique given package namespace conventions.
                   (setq spec (substring spec (- (length fulldoc) ea-width)))
                   (format "%s: %s" spec docstring))))))
    ad-do-it))

(provide 'eldoc-extension)

;;; eldoc-extension.el ends here

;;; LocalWords:  eldoc sym args docstring
