;;; protbuf-by-name.el --- Extend protbuf.el to allow protecting specific buffer names.
;; 
;; Filename: protbuf-by-name.el
;; Description: 
;; Author: Ryan C. Thompson
;; Maintainer: 
;; Created: Fri Jul  3 17:40:53 2009 (-0400)
;; Version: 
;; Last-Updated: 
;;           By: 
;;     Update #: 0
;; URL: 
;; Keywords: 
;; Compatibility: 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; This library extends protbuf.el to protect specific buffers, either
;; by name, regex, or function. Require this library, then M-x
;; customize-group protect-buffer.
;; 
;; If you are NoahFriedman, feel free to incorporate this
;; functionality into your own package.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;; 
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
;; Floor, Boston, MA 02110-1301, USA.(require 'cl)

;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

(require 'protbuf)

(defcustom protect-buffer-names 
  '("*scratch*")
  "List of names of buffers to protect from killing."
  :type '(repeat string)
  :group 'protect-buffer)

(defcustom protect-buffer-matches
  '()
  "List of regexps or functions matching buffer names to protect."
  :type '(repeat (choice regexp function))
  :group 'protect-buffer)

(defun buffer-protected-by-name-p (buf)
  "Returns non-nil if buffer is protected by name"
  (let ((bufname (buffer-name buf)))
    (or
     (some (lambda (protected-name) 
             (string= protected-name bufname))
           protect-buffer-names)
     (some (lambda (protected-match)
             (cond
              ((stringp protected-match) (string-match protected-match bufname))
              ((functionp protected-match) (funcall protected-match bufname))
              (t nil)))
           protect-buffer-matches))))

(defadvice protect-buffer-from-kill (before protect-by-name activate)
  "Protect buffers with certain names from killing."
  (when (and (not protect-buffer-from-kill-mode)
	     (buffer-protected-by-name-p (current-buffer)))
    (protect-buffer-from-kill-mode t)))

(defcustom protect-process-buffer-names 
  '()
  "List of names of buffers to protect while they have active processes."
  :type '(repeat string)
  :group 'protect-buffer)

(defcustom protect-process-buffer-matches
  '()
  "List of regexps or functions matching names of buffers to protect while they have active processes."
  :type '(repeat (choice regexp function))
  :group 'protect-buffer)

(defun process-buffer-protected-by-name-p (buf)
  "Returns non-nil if buffer is process-protected by name"
  (let ((bufname (buffer-name buf)))
    (or
     (some (lambda (protected-name) 
             (string= protected-name bufname))
           protect-process-buffer-names)
     (some (lambda (protected-match)
             (cond
              ((stringp protected-match) (string-match protected-match bufname))
              ((functionp protected-match) (funcall protected-match bufname))
              (t nil)))
           protect-process-buffer-matches))))

(defadvice protect-process-buffer-from-kill (before protect-by-name activate)
  "Protect active process buffers with certain names from killing."
  (when (and (not protect-buffer-from-kill-mode)
	     (buffer-protected-by-name-p (current-buffer)))
    (protect-buffer-from-kill-mode t)))

(provide 'protbuf-by-name)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; protbuf-by-name.el ends here
