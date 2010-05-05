;;; display-buffer-for-wide-screen.el --- Set `display-buffer-function' for wide-screen display
;; $Id: display-buffer-for-wide-screen.el,v 1.3 2010/05/04 09:05:42 rubikitch Exp $

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


;;; Bug Report:
;;
;; If you have problem, send a bug report via M-x display-buffer-for-wide-screen-send-bug-report.
;; The step is:
;;  0) Setup mail in Emacs, the easiest way is:
;;       (setq user-mail-address "your@mail.address")
;;       (setq user-full-name "Your Full Name")
;;       (setq smtpmail-smtp-server "your.smtp.server.jp")
;;       (setq mail-user-agent 'message-user-agent)
;;       (setq message-send-mail-function 'message-smtpmail-send-it)
;;  1) Be sure to use the LATEST version of display-buffer-for-wide-screen.el.
;;  2) Enable debugger. M-x toggle-debug-on-error or (setq debug-on-error t)
;;  3) Use Lisp version instead of compiled one: (load "display-buffer-for-wide-screen.el")
;;  4) Do it!
;;  5) If you got an error, please do not close *Backtrace* buffer.
;;  6) M-x display-buffer-for-wide-screen-send-bug-report and M-x insert-buffer *Backtrace*
;;  7) Describe the bug using a precise recipe.
;;  8) Type C-c C-c to send.
;;  # If you are a Japanese, please write in Japanese:-)

;;; History:

;; $Log: display-buffer-for-wide-screen.el,v $
;; Revision 1.3  2010/05/04 09:05:42  rubikitch
;; Added bug report command
;;
;; Revision 1.2  2010/05/04 09:04:53  rubikitch
;; * Bug fix
;; * Use split-width-threshold
;;
;; Revision 1.1  2009/05/25 16:35:48  rubikitch
;; Initial revision
;;

;;; Code:

(defvar display-buffer-for-wide-screen-version "$Id: display-buffer-for-wide-screen.el,v 1.3 2010/05/04 09:05:42 rubikitch Exp $")
(eval-when-compile (require 'cl))
(defgroup display-buffer-for-wide-screen nil
  "display-buffer-for-wide-screen"
  :group 'emacs)

(if (boundp 'split-width-threshold)
    (defvaralias 'split-window-horizontally-threshold-width 'split-width-threshold)
  (defcustom split-window-horizontally-threshold-width 125
    "*If the current window is more than this-value columns wide, split horizontally, else split vertically."
    :type 'integer  
    :group 'display-buffer-for-wide-screen))



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
          (let ((new-win (if (> (window-width) split-window-horizontally-threshold-width) ;originally 165
                             (split-window-horizontally)
                           (split-window-vertically))))
            (set-window-buffer new-win buffer)
            new-win)
        (let ((new-win (get-lru-window)))
          (set-window-buffer new-win buffer)
          new-win))))

(setq display-buffer-function 'display-buffer-function-according-to-window-width)

;;;; Bug report
(defvar display-buffer-for-wide-screen-maintainer-mail-address
  (concat "rubiki" "tch@ru" "by-lang.org"))
(defvar display-buffer-for-wide-screen-bug-report-salutation
  "Describe bug below, using a precise recipe.

When I executed M-x ...

How to send a bug report:
  1) Be sure to use the LATEST version of display-buffer-for-wide-screen.el.
  2) Enable debugger. M-x toggle-debug-on-error or (setq debug-on-error t)
  3) Use Lisp version instead of compiled one: (load \"display-buffer-for-wide-screen.el\")
  4) If you got an error, please paste *Backtrace* buffer.
  5) Type C-c C-c to send.
# If you are a Japanese, please write in Japanese:-)")
(defun display-buffer-for-wide-screen-send-bug-report ()
  (interactive)
  (reporter-submit-bug-report
   display-buffer-for-wide-screen-maintainer-mail-address
   "display-buffer-for-wide-screen.el"
   (apropos-internal "^display-buffer-for-wide-screen-" 'boundp)
   nil nil
   display-buffer-for-wide-screen-bug-report-salutation))


(provide 'display-buffer-for-wide-screen)

;; How to save (DO NOT REMOVE!!)
;; (emacswiki-post "display-buffer-for-wide-screen.el")
;;; display-buffer-for-wide-screen.el ends here
