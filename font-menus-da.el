;;; font-menus-da.el --- Additional font menus.
;; 
;; Filename: font-menus-da.el
;; Description: Additional font menus.  `font-menus.el' fixed for Emacs 24+.
;; Author: Simon Marshal, Francis J. Wright
;; Maintainer: Drew Adams
;; Copyright (C) 2000 Francis J. Wright
;; Copyright (C) 2012, Drew Adams, all rights reserved.
;; Created: Sun Aug 26 07:06:14 2012 (-0700)
;; Version: 
;; Last-Updated: Sun Aug 26 21:18:24 2012 (-0700)
;;           By: dradams
;;     Update #: 142
;; URL: http://www.emacswiki.org/emacs-en/start.el
;; Doc URL: 
;; Keywords: font, highlighting, syntax, decoration
;; Compatibility: 
;; 
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; Additional font menus.  `font-menus.el' fixed for Emacs 24+.
;;
;; This is `font-menus.el', by Francis J. Wright, modified so that it
;; continues to work with GNU Emacs 24 and later (as well as older
;; versions), and with minor enhancements.
;;
;; Enhancements:
;;
;; * User option `font-lock-menu-wrap': non-nil means wrap around when
;;   changing levels, instead of just raising an error.  This has no
;;   effect when the commands are called from a menu, but you can bind
;;   commands `font-lock-fontify-more' and `font-lock-fontify-less' to
;;   keys.
;;
;;   If the non-nil value is `off' (the default value) then font-lock
;;   mode is turned off (absolute minimum font locking) when cycling
;;   wraps around.  Any other non-nil value means cycle only among
;;   font-lock states.  If the value is `off' then you can cycle
;;   on/off even if there is only one font-lock level.
;;
;; -------------------------------------------------------------------
;;
;; Here is the original Commentary, by F.J. Wright:
;;
;; This package is intended for use with GNU Emacs 20 and adds
;; submenus to the Edit menu to control font lock mode and provide
;; font display.
;;
;;; Installation:
;;
;; Put this file somewhere where Emacs can find it (i.e. in one of the
;; directories in your `load-path' such as `site-lisp'), optionally
;; byte-compile it, and put this in your .emacs:
;;
;;  (require 'font-menus)
;;
;;; Font Display:
;;
;; Extracted from font-lock.el for GNU Emacs 20.3 and
;; `font-lock-menu.el' for GNU Emacs 19, both by Simon Marshal
;; <simon@gnu.ai.mit.edu> and revised to use easymenu and run as a
;; stand-alone package by Francis J. Wright.  (It would be better put
;; back into font-lock.el!)
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;;
;; 2012/08/26 dadams
;;     Added user option font-lock-menu-wrap.
;;     font-lock-fontify-level: Updated for Emacs 22-24+.
;;     font-lock-set-menu: Fix for Emacs 22-24+: Do nothing if font-lock-fontified.
;;     font-lock-fontify-(less|more): Wrap around if font-lock-menu-wrap.
;;     Use only font-lock-defaults if font-lock-defaults-alist no longer exists (24+).
;;     Don't put `Display Fonts' at end of menu. Put it after `Display Colors'.
;;     Changed menu item name: appended (Font Lock).
;;
;;     Created from font-menus.el.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
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

(eval-when-compile
  (require 'easymenu)
  (require 'font-lock))

;; `Syntax Highlighting (Font Lock)' menu.
;; Add to `Edit' menu, before `Text Properties' menu.
(easy-menu-add-item			; (map path item &optional before)
 menu-bar-edit-menu nil
 (easy-menu-create-menu			; (menu-name menu-items)
  "Syntax Highlighting (Font Lock)"
  '(["In All Buffers" global-font-lock-mode
     :style toggle :selected global-font-lock-mode :active t]
    ["In Current Buffer" font-lock-mode
     :style toggle :selected font-lock-mode :active t]
    "--"
    ["More In Current Buffer" font-lock-fontify-more
     (nth 2 font-lock-fontify-level)]
    ["Less In Current Buffer" font-lock-fontify-less
     (nth 1 font-lock-fontify-level)]))
 'props)

(defvar font-lock-fontify-level nil	; For less/more fontification.
  "Font-lock levels for the current buffer.
The form is (CURRENT-LEVEL EXISTS-LOWER-LEVEL-P EXISTS-HIGHER-LEVEL-P)
where CURRENT-LEVEL is the current level and the other elements are
Boolean values specifying whether there is a lower/higher level than
CURRENT-LEVEL, respectively.")

(defun font-lock-fontify-level (level)
  "Set font-lock highlighting level for current buffer to LEVEL."
  (let ((font-lock-maximum-decoration  level))
    (when font-lock-mode (font-lock-mode -1))
    (kill-local-variable 'font-lock-set-defaults)
    (font-lock-mode 1)
    (when font-lock-verbose (message "Fontifying `%s'... level %d" (buffer-name) level))))

(defcustom font-lock-menu-wrap 'off
  "Non-nil (on) means `font-lock-fontify-(more|less)' wrap around.
If nil (off), these commands raise an error when you cannot fontify
any more/less.

If the non-nil value is `off' (the default value) then cycling turns
font-lock mode off as the first state of wrapping (absolute minimum).
Any other non-nil value cycles only among font-lock levels."
  :type '(choice
          (const :tag "Do not wrap - raise an error"  nil)
          (const :tag "Wrap to OFF"                   off)
          (other :tag "Wrap, but stay font-locked"    t))
  :group 'font-lock)

(defun font-lock-fontify-less ()
  "Fontify the current buffer using less highlighting (decoration).
See `font-lock-maximum-decoration'."
  (interactive)
  (if (nth 1 font-lock-fontify-level)
      (font-lock-fontify-level (1- (car font-lock-fontify-level)))
    (if font-lock-menu-wrap
        (if (and font-lock-mode  (eq font-lock-menu-wrap 'off))
            (progn (font-lock-mode -1) (message "Font lock turned OFF"))
          (font-lock-fontify-level
           (1- (length (or (nth 0 font-lock-defaults)
                           (and (boundp 'font-lock-defaults-alist)
                                (nth 1 (assq major-mode font-lock-defaults-alist))))))))
      (error "It is not possible to fontify less"))))

(defun font-lock-fontify-more ()
  "Fontify the current buffer using more highlighting (decoration).
See `font-lock-maximum-decoration'."
  (interactive)
  (if (nth 2 font-lock-fontify-level)
      (font-lock-fontify-level (1+ (car font-lock-fontify-level)))
    (if font-lock-menu-wrap
        (if (and font-lock-mode  (eq font-lock-menu-wrap 'off))
            (progn (font-lock-mode -1) (message "Font lock turned OFF"))
          (font-lock-fontify-level 1))
      (error "It is not possible to fontify more"))))

;; This should be called by `font-lock-set-defaults'.
(defun font-lock-set-menu ()
  "Activate fewer/more fontification entries.
Do nothing if there are not multiple levels for the current buffer.
Sets `font-lock-fontify-level'."
  (unless font-lock-fontified
    (let ((keywords  (or (nth 0 font-lock-defaults)
                         (and (boundp 'font-lock-defaults-alist)
                              (nth 1 (assq major-mode font-lock-defaults-alist)))))
          (level     (font-lock-value-in-major-mode font-lock-maximum-decoration)))
      (make-local-variable 'font-lock-fontify-level)
      (if (or (symbolp keywords)  (= (length keywords) 1))
          (font-lock-unset-menu)
        (cond ((eq level t) (setq level  (1- (length keywords))))
              ((or (null level)  (zerop level))
               ;; The default level is usually, but not necessarily, level 1.
               (setq level  (- (length keywords)
                               (length (member (eval (car keywords))
                                               (mapcar 'eval (cdr keywords))))))))
        (setq font-lock-fontify-level  (list level (> level 1)
                                             (< level (1- (length keywords)))))))))

;; This should be called by `font-lock-unset-defaults'.
(defun font-lock-unset-menu ()
  "Deactivate fewer/more fontification entries."
  (setq font-lock-fontify-level  nil))

;; Added by FJW:

(defadvice font-lock-set-defaults
  (after font-lock-set-defaults-advice activate)
  "Font Lock Mode Menu support added."
  (font-lock-set-menu))

(defadvice font-lock-unset-defaults
  (after font-lock-unset-defaults-advice activate)
  "Font Lock Mode Menu support added."
  (font-lock-unset-menu))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Font Display:

;; Based on code by "Daniel, Elijah" <Elijah.Daniel@compaq.com>
;; and `list-faces-display' in `faces.el'.

(defun display-fonts ()
  "Sort and display all fonts that Emacs knows about."
  (interactive)
  (with-output-to-temp-buffer "*Fonts*"
    (save-excursion
      (set-buffer standard-output)
      (mapcar (lambda (font) (insert font "\n"))
	      (sort (x-list-fonts "*") 'string-lessp)))
    (print-help-return-message)))

(define-key-after facemenu-menu [display-fonts] '("Display Fonts" . display-fonts) 'dc)


;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'font-menus)                   ; Need provide this also.
(provide 'font-menus-da)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; font-menus-da.el ends here
