;;; show-wspace.el --- Highlight whitespace of various kinds.
;;
;; Filename: show-wspace.el
;; Description: Highlight whitespace of various kinds.
;; Author: Peter Steiner <unistein@isbe.ch>, Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2000-2009, Drew Adams, all rights reserved.
;; Created: Wed Jun 21 08:54:53 2000
;; Version: 21.0
;; Last-Updated: Sat Aug  1 15:42:17 2009 (-0700)
;;           By: dradams
;;     Update #: 282
;; URL: http://www.emacswiki.org/cgi-bin/wiki/show-wspace.el
;; Keywords: highlight, whitespace
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x
;;
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Highlight whitespace of various kinds.
;;
;; To use this library:
;;
;;    Add this to your initialization file (~/.emacs or ~/_emacs):
;;
;;      (require 'show-wspace) ; Load this library.
;;
;; Then you can use commands `toggle-*' (see below) to turn the
;; various kinds of whitespace highlighting on and off in Font-Lock
;; mode.
;;
;; If you want to always use a particular kind of whitespace
;; highlighting, by default, then add the corresponding
;; `show-ws-highlight-*' function (see below) to the hook
;; `font-lock-mode-hook'.  Then, whenever Font-Lock mode is turned on,
;; whitespace highlighting will also be turned on.
;;
;; For example, you can turn on tab highlighting by default by adding
;; function `show-ws-highlight-tabs' to `font-lock-mode-hook' in your
;; .emacs file, as follows:
;;
;;     (add-hook 'font-lock-mode-hook 'show-ws-highlight-tabs)
;;
;;
;; Faces defined here:
;;
;;    `show-ws-hard-space', `show-ws-tab', `show-ws-trailing-whitespace'.
;;
;; Commands defined here:
;;
;;    `show-ws-toggle-show-hard-spaces', `show-ws-toggle-show-tabs',
;;    `show-ws-toggle-show-trailing-whitespace',
;;    `toggle-show-hard-spaces-show-ws' (alias),
;;    `toggle-show-tabs-show-ws' (alias),
;;    `toggle-show-trailing-whitespace-show-ws' (alias).
;;
;; Non-interactive functions defined here:
;;
;;    `show-ws-dont-highlight-hard-spaces',
;;    `show-ws-dont-highlight-tabs',
;;    `show-ws-dont-highlight-trailing-whitespace',
;;    `show-ws-highlight-hard-spaces', `show-ws-highlight-tabs',
;;    `show-ws-highlight-trailing-whitespace'.
;;
;; Internal variables defined here:
;;
;;    `show-ws-highlight-hard-spaces-p', `show-ws-highlight-tabs-p',
;;    `show-ws-highlight-trailing-whitespace-p'.
;;
;; Drew Adams wrote the `toggle-*' commands and `*-p' variables.
;;
;; Peter Steiner wrote the original code that did the equivalent of
;; the `show-ws-highlight-*' commands here in his `hilite-trail.el'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;; 2009/06/25 dadams
;;     show-ws-dont-*: Should be no-op's for Emacs 20, 21.
;; 2009/06/17 dadams
;;     Added: show-ws-dont-highlight-*.
;;     show-ws-toggle-show-*: Remove the font-lock keywords. Needed for Emacs 22+.
;; 2007/09/25 dadams
;;     Renamed to use prefix show-ws-.  Thx to Cyril Brulebois.
;; 2006/11/11 dadams
;;     Corrected doc strings.  Clarified: hard space is non-breaking space, \240.
;;     Included hard space in highlight-trailing-whitespace.
;; 2006/04/06 dadams
;;     highlight-*: Use font-lock-add-keywords.  Thanks to Karl Chen.
;; 2006/02/20 dadams
;;     Mentioned in Commentary how to use non-interactively.
;; 2006/01/07 dadams
;;     Added :link for sending bug report.
;; 2006/01/06 dadams
;;     Added defgroup and use it.
;; 2005/12/30 dadams
;;     Removed require of def-face-const.el.
;;     Renamed faces, without "-face".
;; 2005/01/25 dadams
;;     Removed ###autoload for defvars.
;; 2004/06/10 dadams
;;     Fixed minor bug in highlight-* functions.
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

(and (< emacs-major-version 20) (eval-when-compile (require 'cl))) ;; when, push

;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup Show-Whitespace nil
  "Highlight whitespace of various kinds."
  :prefix "show-ws-"
  :group 'convenience :group 'matching
  :link `(url-link :tag "Send Bug Report"
          ,(concat "mailto:" "drew.adams" "@" "oracle" ".com?subject=\
show-wspace.el bug: \
&body=Describe bug here, starting with `emacs -q'.  \
Don't forget to mention your Emacs and library versions."))
  :link '(url-link :tag "Other Libraries by Drew"
          "http://www.emacswiki.org/cgi-bin/wiki/DrewsElispLibraries")
  :link '(url-link :tag "Download"
          "http://www.emacswiki.org/cgi-bin/wiki/show-wspace.el")
  :link '(url-link :tag "Description"
          "http://www.emacswiki.org/cgi-bin/wiki/ShowWhiteSpace#ShowWspace")
  :link '(emacs-commentary-link :tag "Commentary" "show-wspace")
  )

(defface show-ws-tab '((t (:background "LemonChiffon")))
  "*Face for highlighting tab characters (`C-i') in Font-Lock mode."
  :group 'Show-Whitespace :group 'font-lock :group 'faces)

(defface show-ws-trailing-whitespace '((t (:background "Gold")))
  "*Face for highlighting whitespace at line ends in Font-Lock mode."
  :group 'Show-Whitespace :group 'font-lock :group 'faces)

(defface show-ws-hard-space '((t (:background "PaleGreen")))
  "*Face for highlighting non-breaking spaces (`\240')in Font-Lock mode."
  :group 'Show-Whitespace :group 'font-lock :group 'faces)


(defvar show-ws-highlight-tabs-p nil
  "Non-nil means font-lock mode highlights TAB characters (`C-i').")

(defvar show-ws-highlight-trailing-whitespace-p nil
  "Non-nil means font-lock mode highlights whitespace at line ends.")

(defvar show-ws-highlight-hard-spaces-p nil
  "Non-nil means font-lock mode highlights non-breaking spaces (`\240').")

;;;###autoload
(defalias 'toggle-show-tabs-show-ws 'show-ws-toggle-show-tabs)
;;;###autoload
(defun show-ws-toggle-show-tabs ()
  "Toggle highlighting of TABs, using face `show-ws-tab'."
  (interactive)
  (setq show-ws-highlight-tabs-p (not show-ws-highlight-tabs-p))
  (if show-ws-highlight-tabs-p
      (add-hook 'font-lock-mode-hook 'show-ws-highlight-tabs)
    (remove-hook 'font-lock-mode-hook 'show-ws-highlight-tabs)
    (show-ws-dont-highlight-tabs))
  (font-lock-mode) (font-lock-mode)
  (message "TAB highlighting is now %s." (if show-ws-highlight-tabs-p "ON" "OFF")))

;;;###autoload
(defalias 'toggle-show-hard-spaces-show-ws 'show-ws-toggle-show-hard-spaces)
;;;###autoload
(defun show-ws-toggle-show-hard-spaces ()
  "Toggle highlighting of non-breaking space characters (`\240').
Uses face `show-ws-hard-space'."
  (interactive)
  (setq show-ws-highlight-hard-spaces-p (not show-ws-highlight-hard-spaces-p))
  (if show-ws-highlight-hard-spaces-p
      (add-hook 'font-lock-mode-hook 'show-ws-highlight-hard-spaces)
    (remove-hook 'font-lock-mode-hook 'show-ws-highlight-hard-spaces)
    (show-ws-dont-highlight-hard-spaces))
  (font-lock-mode) (font-lock-mode)
  (message "Hard (non-breaking) space highlighting is now %s."
           (if show-ws-highlight-hard-spaces-p "ON" "OFF")))

;;;###autoload
(defalias 'toggle-show-trailing-whitespace-show-ws
    'show-ws-toggle-show-trailing-whitespace)
;;;###autoload
(defun show-ws-toggle-show-trailing-whitespace ()
  "Toggle highlighting of trailing whitespace.
Uses face `show-ws-trailing-whitespace'."
  (interactive)
  (setq show-ws-highlight-trailing-whitespace-p
        (not show-ws-highlight-trailing-whitespace-p))
  (if show-ws-highlight-trailing-whitespace-p
      (add-hook 'font-lock-mode-hook 'show-ws-highlight-trailing-whitespace)
    (remove-hook 'font-lock-mode-hook 'show-ws-highlight-trailing-whitespace)
    (show-ws-dont-highlight-trailing-whitespace))
  (font-lock-mode) (font-lock-mode)
  (message "Trailing whitespace highlighting is now %s."
           (if show-ws-highlight-trailing-whitespace-p "ON" "OFF")))

(defun show-ws-highlight-tabs ()
  "Highlight tab characters (`C-i')."
  (font-lock-add-keywords nil '(("[\t]+" (0 'show-ws-tab t)))))
(defun show-ws-highlight-hard-spaces ()
  "Highlight hard (non-breaking) space characters (`\240')."
  (font-lock-add-keywords nil '(("[\240]+" (0 'show-ws-hard-space t)))))
(defun show-ws-highlight-trailing-whitespace ()
  "Highlight whitespace characters at line ends."
  (font-lock-add-keywords
   nil '(("[\240\040\t]+$" (0 'show-ws-trailing-whitespace t)))))

;; These are no-ops for Emacs 20, 21:
;; `font-lock-remove-keywords' is not defined, and we don't need to use it.
(defun show-ws-dont-highlight-tabs ()
  "Don't highlight tab characters (`C-i')."
  (when (fboundp 'font-lock-remove-keywords)
    (font-lock-remove-keywords nil '(("[\t]+" (0 'show-ws-tab t))))))

(defun show-ws-dont-highlight-hard-spaces ()
  "Don't highlight hard (non-breaking) space characters (`\240')."
  (when (fboundp 'font-lock-remove-keywords)
    (font-lock-remove-keywords nil '(("[\240]+" (0 'show-ws-hard-space t))))))

(defun show-ws-dont-highlight-trailing-whitespace ()
  "Don't highlight whitespace characters at line ends."
  (when (fboundp 'font-lock-remove-keywords)
    (font-lock-remove-keywords
     nil '(("[\240\040\t]+$" (0 'show-ws-trailing-whitespace t))))))

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'show-wspace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; show-wspace.el ends here
