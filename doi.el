;;; doi.el --- Do Or Insert

;; Filename: doi.el
;; Description: Do Or Insert
;; Author: Andy Stewart lazycat.manatee@gmail.com
;; Maintainer: Andy Stewart lazycat.manatee@gmail.com
;; Copyright (C) 2008, 2009, Andy Stewart, all rights reserved.
;; Created: 2008-12-11 11:01:59
;; Version: 0.2
;; Last-Updated: 2009-02-28 13:27:36
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/doi.el
;; Keywords:
;; Compatibility: GNU Emacs 23.0.60.1
;;
;; Features that might be required by this library:
;;
;;
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
;; Do Or Insert
;;
;; In Emacs, have some mode mix `read-only' and `edit' area,
;; So you need you DO command to with `read-only' area,
;; insert in `edit' area.
;;
;; This this extension is make one keystroke DO OR INSERT in different
;; area automatically.
;;

;;; Installation:
;;
;; Put doi.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'doi)
;;
;; No need more.

;;; Customize:
;;
;; `doi-insert-function'
;; The insert function for `doi'.
;; It accepts one prefix argument for insert.
;; Default is `doi-insert-default'.
;;

;;; Change log:
;;
;; 2009/02/28
;;      * Remove option `doi-use-predictive', not necessary.
;;      * Add option `doi-insert-function' for customize insert function.
;;
;; 2008/12/11
;;      * First released.
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

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Customize ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup doi nil
  "Do Or Insert."
  :group 'editing)

(defcustom doi-insert-function 'doi-insert-default
  "The insert function for `doi'.
It accepts one prefix argument for insert.
Default is `doi-insert-default'."
  :type 'function
  :group 'doi)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Interactive Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun doi-scroll-up (&optional arg)
  "Do `doi' with `scroll-up'.
ARG is prefix argument."
  (interactive "p")
  (doi 'scroll-up arg))

(defun doi-scroll-down (&optional arg)
  "Do `doi' with `scroll-down'.
ARG is prefix argument."
  (interactive "p")
  (doi 'scroll-down arg))

(defun doi-scroll-up-one-line (&optional arg)
  "Do `doi' with `scroll-up', but just one line.
ARG is prefix argument."
  (interactive "p")
  (doi '(lambda ()
          (scroll-up 1))
       arg))

(defun doi-scroll-down-one-line (&optional arg)
  "Do `doi' with `scroll-down', but just one line.
ARG is prefix argument."
  (interactive "p")
  (doi '(lambda ()
          (scroll-down 1))
       arg))

(defun doi-previous-line (&optional arg)
  "Do `doi' with `previous-line'.
ARG is prefix argument."
  (interactive "p")
  (doi 'previous-line arg))

(defun doi-next-line (&optional arg)
  "Do `doi' with `next-line'.
ARG is prefix argument."
  (interactive "p")
  (doi 'next-line arg))

(defun doi-backward-char (&optional arg)
  "Do `doi' with `backward-char'.
ARG is prefix argument."
  (interactive "p")
  (doi 'backward-char arg))

(defun doi-forward-char (&optional arg)
  "Do `doi' with `forward-char'.
ARG is prefix argument."
  (interactive "p")
  (doi 'forward-char arg))

(defun doi-backward-word (&optional arg)
  "Do `doi' with `backward-word'.
ARG is prefix argument."
  (interactive "p")
  (doi 'backward-word arg))

(defun doi-forward-word (&optional arg)
  "Do `doi' with `forward-word'.
ARG is prefix argument."
  (interactive "p")
  (doi 'forward-word arg))

(defun doi-isearch-forward (&optional arg)
  "Do `doi' with `isearch-forward'.
ARG is prefix argument."
  (interactive "p")
  (doi 'isearch-forward arg))

(defun doi-isearch-backward (&optional arg)
  "Do `doi' with `isearch-backward'.
ARG is prefix argument."
  (interactive "p")
  (doi 'isearch-backward arg))

(defun doi-move-beginning-of-line (&optional arg)
  "Do `doi' with `move-beginning-of-line'.
ARG is prefix argument."
  (interactive "p")
  (doi '(lambda ()
          (call-interactively 'move-beginning-of-line)) arg))

(defun doi-move-end-of-line (&optional arg)
  "Do `doi' with `move-end-of-line'.
ARG is prefix argument."
  (interactive "p")
  (doi '(lambda ()
          (call-interactively 'move-end-of-line)) arg))

(defun doi-beginning-of-buffer (&optional arg)
  "Do `doi' with `beginning-of-buffer'.
ARG is prefix argument."
  (interactive "p")
  (doi 'beginning-of-buffer arg))

(defun doi-end-of-buffer (&optional arg)
  "Do `doi' with `end-of-buffer'.
ARG is prefix argument."
  (interactive "p")
  (doi 'end-of-buffer arg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Utilities Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun doi (command &optional arg)
  "Do Or Insert.
If cursor is at `text-read-only' area do COMMAND,
otherwise insert self.
ARG is prefix argument."
  (condition-case nil
      ;; If at `edit' area, INSERT self.
      (funcall doi-insert-function arg)
    ;; If at `read-only' area, DO command.
    (text-read-only
     (funcall command))))

(defun doi-insert-default (arg)
  "The default insert function for `doi'.
ARG is prefix argument."
  (if (and (require 'completion-ui nil t)
           (member 'auto-completion-mode minor-mode-list))
      ;; Do `completion-self-insert' when
      ;; `auto-completion-mode' turn on.
      (completion-self-insert)
    ;; Otherwise do `self-insert-command'.
    (self-insert-command (or arg 1))))

(provide 'doi)

;;; doi.el ends here

;;; LocalWords:  doi ui
