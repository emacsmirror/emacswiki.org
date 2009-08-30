;;; display-buffer-for-wide-screen.el --- Set `display-buffer-function' for wide-screen display
;; $Id: display-buffer-for-wide-screen.el,v 1.1 2009/05/25 16:35:48 rubikitch Exp $

;; Copyright (C) 2009  rubikitch

;; Author: rubikitch <rubikitch@ruby-lang.org>
;; Original Author: Tassilo Horn
;; Keywords: extensions
;; URL: http://www.emacswiki.org/cgi-bin/wiki/download/display-buffer-for-wide-screen.el

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Set `display-buffer-function' to
;; `display-buffer-function-according-to-window-width'. It determines
;; how to split window according to `window-width'. The threshold
;; value is 125 by default.
;; 

;;; Commands:
;;
;; Below are complete command list:
;;
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;
;;  `split-window-horizontally-threshold-width'
;;    *If the current window is more than this-value columns wide, split horizontally, else split vertically.
;;    default = 125

;;; Installation:
;;
;; Put display-buffer-for-wide-screen.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'display-buffer-for-wide-screen)
;;
;; No need more.

;;; Customize:
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET display-buffer-for-wide-screen RET
;;


;;; History:

;; $Log: display-buffer-for-wide-screen.el,v $
;; Revision 1.1  2009/05/25 16:35:48  rubikitch
;; Initial revision
;;

;;; Code:

(defvar display-buffer-for-wide-screen-version "$Id: display-buffer-for-wide-screen.el,v 1.1 2009/05/25 16:35:48 rubikitch Exp $")
(eval-when-compile (require 'cl))
(defgroup display-buffer-for-wide-screen nil
  "display-buffer-for-wide-screen"
  :group 'emacs)

(defcustom split-window-horizontally-threshold-width 125
  "*If the current window is more than this-value columns wide, split horizontally, else split vertically."
  :type 'integer  
  :group 'display-buffer-for-wide-screen)


;;; This function is originally written by Tassilo Horn.
;;; Rubikitch modified slightly.
;;; http://www.mail-archive.com/emacs-pretest-bug@gnu.org/msg11469.html
(defun display-buffer-function-according-to-window-width (buffer force-other-window &rest ignored)
  "If BUFFER is visible, select it.

If it's not visible and there's only one window, split the
current window and select BUFFER in the new window. If the
current window (before the split) is more than
`split-window-horizontally-threshold-width' columns wide,
split horizontally, else split vertically.

If the current buffer contains more than one window, select
BUFFER in the least recently used window.

This function returns the window which holds BUFFER.

FORCE-OTHER-WINDOW is ignored."
  (or (get-buffer-window buffer)
      (and special-display-function
           (or (member (buffer-name buffer) special-display-buffer-names)
               (some (lambda (re) (string-match re (buffer-name buffer))) special-display-regexps))
           (funcall special-display-function buffer))
      (if (one-window-p)
          (let ((new-win (if (> (window-width) 125) ;originally 165
                             (split-window-horizontally)
                           (split-window-vertically))))
            (set-window-buffer new-win buffer)
            new-win)
        (let ((new-win (get-lru-window)))
          (set-window-buffer new-win buffer)
          new-win))))

(setq display-buffer-function 'display-buffer-function-according-to-window-width)

(provide 'display-buffer-for-wide-screen)

;; How to save (DO NOT REMOVE!!)
;; (emacswiki-post "display-buffer-for-wide-screen.el")
;;; display-buffer-for-wide-screen.el ends here
