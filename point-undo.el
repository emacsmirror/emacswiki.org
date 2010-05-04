;;; point-undo.el --- undo/redo position

;;  Copyright (C) 2006,2008 rubikitch <rubikitch atmark ruby-lang.org>
;;  Version: $Id: point-undo.el,v 1.6 2009/10/16 20:37:37 rubikitch Exp rubikitch $

;;  This program is free software; you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation; either version 2 of the License, or
;;  (at your option) any later version.
;;    This program is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.
;;    You should have received a copy of the GNU General Public License
;;  along with this program; if not, write to the Free Software
;;  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

;;; Commentary:

;; This package allows you to undo/redo point and window-start.
;; It is like w3m's UNDO/REDO commands.

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `point-undo'
;;    Undo position.
;;  `point-redo'
;;    Redo position.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;

;;; Setup:

;; (require 'point-undo)
;; (define-key global-map [f5] 'point-undo)
;; (define-key global-map [f6] 'point-redo)

;;; Bug Report:
;;
;; If you have problem, send a bug report via M-x point-undo-send-bug-report.
;; The step is:
;;  0) Setup mail in Emacs, the easiest way is:
;;       (setq user-mail-address "your@mail.address")
;;       (setq user-full-name "Your Full Name")
;;       (setq smtpmail-smtp-server "your.smtp.server.jp")
;;       (setq mail-user-agent 'message-user-agent)
;;       (setq message-send-mail-function 'message-smtpmail-send-it)
;;  1) Be sure to use the LATEST version of point-undo.el.
;;  2) Enable debugger. M-x toggle-debug-on-error or (setq debug-on-error t)
;;  3) Use Lisp version instead of compiled one: (load "point-undo.el")
;;  4) Do it!
;;  5) If you got an error, please do not close *Backtrace* buffer.
;;  6) M-x point-undo-send-bug-report and M-x insert-buffer *Backtrace*
;;  7) Describe the bug using a precise recipe.
;;  8) Type C-c C-c to send.
;;  # If you are a Japanese, please write in Japanese:-)

;;; History:
;; 
;; $Log: point-undo.el,v $
;; Revision 1.6  2009/10/16 20:37:37  rubikitch
;; point-undo-list records position info only when point is moved.
;;
;; Revision 1.5  2008/12/27 15:21:03  rubikitch
;; *** empty log message ***
;;
;; Revision 1.4  2008/12/27 15:20:26  rubikitch
;; *** empty log message ***
;;
;; Revision 1.3  2008/12/27 15:19:38  rubikitch
;; refactoring
;;
;; Revision 1.2  2008/12/27 14:53:54  rubikitch
;; undo/redo not only point but also window-start.
;;

;; 2006/02/27: initial version

;;; Code:
(eval-when-compile (require 'cl))

(defvar point-undo-list nil)
(make-variable-buffer-local 'point-undo-list)

(defvar point-redo-list nil)
(make-variable-buffer-local 'point-redo-list)

(defun point-undo-pre-command-hook ()
  "Save positions before command."
  (unless (or (eq this-command 'point-undo)
              (eq this-command 'point-redo))
    
    (let ((cell (cons (point) (window-start))))
      (unless (equal cell (car point-undo-list))
       (setq point-undo-list (cons cell point-undo-list))))
    (setq point-redo-list nil)))
(add-hook 'pre-command-hook 'point-undo-pre-command-hook)

(defun point-undo-doit (list1 list2)
  ;; list1, list2 = {point-undo-list, point-redo-list}
  (destructuring-bind (pt . wst)
      (or (car (symbol-value list1)) '(nil)) ;nil-safe
    (when pt
      (set list1 (cdr (symbol-value list1)))
      (set list2 (cons (cons (point) (window-start)) (symbol-value list2)))
      (goto-char pt)
      (set-window-start (selected-window) wst))))

(defun point-undo ()
  "Undo position."
  (interactive)
  (point-undo-doit 'point-undo-list 'point-redo-list))

(defun point-redo ()
  "Redo position."
  (interactive)
  (when (or (eq last-command 'point-undo)
            (eq last-command 'point-redo))
    (point-undo-doit 'point-redo-list 'point-undo-list)))

;;;; Bug report
(defvar point-undo-maintainer-mail-address
  (concat "rubiki" "tch@ru" "by-lang.org"))
(defvar point-undo-bug-report-salutation
  "Describe bug below, using a precise recipe.

When I executed M-x ...

How to send a bug report:
  1) Be sure to use the LATEST version of point-undo.el.
  2) Enable debugger. M-x toggle-debug-on-error or (setq debug-on-error t)
  3) Use Lisp version instead of compiled one: (load \"point-undo.el\")
  4) If you got an error, please paste *Backtrace* buffer.
  5) Type C-c C-c to send.
# If you are a Japanese, please write in Japanese:-)")
(defun point-undo-send-bug-report ()
  (interactive)
  (reporter-submit-bug-report
   point-undo-maintainer-mail-address
   "point-undo.el"
   (apropos-internal "^point-undo-" 'boundp)
   nil nil
   point-undo-bug-report-salutation))

(provide 'point-undo)

;; How to save (DO NOT REMOVE!!)
;; (emacswiki-post "point-undo.el")
;;; point-undo.el ends here
