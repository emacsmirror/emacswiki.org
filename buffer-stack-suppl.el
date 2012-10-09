;;; buffer-stack-suppl.el --- Bubble though buffers of the same major-mode

;; Copyright 2010 Le Wang
;;
;; Version: $Id: buffer-stack-suppl.el,v 0.0 2010/12/22 15:24:23 Le Exp $
;; Keywords:
;; X-URL: not distributed yet

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Change buffer-stack default behaviour to only bubble through buffers of the
;; same major-mode.  Switching between all buffers is done with new *-thru-all
;; functions.

;; Put this file into your load-path and the following into your ~/.emacs:
;;
;;   (require 'buffer-stack-suppl)
;;
;;   (global-set-key [(f10)] 'buffer-stack-bury)
;;   (global-set-key [(control f10)] 'buffer-stack-bury-and-kill)
;;   (global-set-key [(f9)] 'buffer-stack-up)
;;   (global-set-key [(f11)] 'buffer-stack-down)
;;   (global-set-key [(shift f10)] 'buffer-stack-bury-thru-all)
;;   (global-set-key [(shift f9)] 'buffer-stack-up-thru-all)
;;   (global-set-key [(shift f11)] 'buffer-stack-down-thru-all)
;;

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'buffer-stack)

(setq buffer-stack-filter 'buffer-stack-same-major-mode-filter)


(defun* buffer-stack-same-major-mode-filter (proposed-buffer)
  "exclusive, valid filename, same (basis) major-mode  filter.

addition: if the frame has a 'major-mode parameter, filter out
all buffers that don't match it.

Rinari minor-mode files can show different major mode based on
context (mumamo).  We group all rinari minor mode buffers
together.

"
  (and (buffer-stack-filter-exclusive proposed-buffer)
       (let ((proposed-buffer-major-mode (with-current-buffer proposed-buffer major-mode)))
         (cond ((and (boundp 'rinari-minor-mode)
                     rinari-minor-mode)
                (with-current-buffer proposed-buffer
                  rinari-minor-mode))
               ((eq major-mode proposed-buffer-major-mode)
                t)
               (t
                ;; use substring search to group these modes
                (let((case-fold-search t)
                     (mode-groups '("lisp" "ruby")))
                  (dolist (mode mode-groups nil)
                    (when (and
                           (string-match mode (symbol-name major-mode))
                           (string-match mode (symbol-name proposed-buffer-major-mode)))
                      (return t)))))))))


(defun buffer-stack-no-compilation-filter (proposed-buffer)
  "specifically removes some major-modes, and specific buffers"
  (and (buffer-stack-filter-exclusive proposed-buffer)
       (let ((proposed-buffer-major-mode (with-current-buffer proposed-buffer major-mode))
             (skip-modes '(compilation-mode)))
         (not (memq proposed-buffer-major-mode skip-modes)))))


(defun buffer-stack-no-compilation-funcall (func)
  (let ((buffer-stack-filter 'buffer-stack-no-compilation-filter))
    (funcall func)))

;;;###autoload
(defun buffer-stack-bury-thru-all ()
  "move through all buffers except `compilation-mode'"
  (interactive)
  (buffer-stack-no-compilation-funcall
   'buffer-stack-bury))

;;;###autoload
(defun buffer-stack-up-thru-all ()
  "move through all buffers except `compilation-mode'"
  (interactive)
  (buffer-stack-no-compilation-funcall
   'buffer-stack-up))

;;;###autoload
(defun buffer-stack-down-thru-all ()
  "move through all buffers except `compilation-mode'"
(interactive)
  (buffer-stack-no-compilation-funcall
   'buffer-stack-down))

(provide 'buffer-stack-suppl)



 
;;;;##########################################################################
;;;;  User Options, Variables
;;;;##########################################################################





;;; buffer-stack-suppl.el ends here
