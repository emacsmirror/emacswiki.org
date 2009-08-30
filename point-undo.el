;;; point-undo.el --- undo/redo position

;;  Copyright (C) 2006,2008 rubikitch <rubikitch@ruby-lang.org>
;;  Version: $Id: point-undo.el,v 1.5 2008/12/27 15:21:03 rubikitch Exp $

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

;;; Setup:

;; (require 'point-undo)
;; (define-key global-map [f5] 'point-undo)
;; (define-key global-map [f6] 'point-redo)


;;; History:
;; 
;; $Log: point-undo.el,v $
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
    (setq point-undo-list (cons (cons (point) (window-start)) point-undo-list))
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

(provide 'point-undo)

;; How to save (DO NOT REMOVE!!)
;; (emacswiki-post "point-undo.el")
;;; point-undo.el ends here
