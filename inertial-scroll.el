;;; inertial-scroll.el --- global minor mode for inertial scrolling 

;; Copyright (C) 2010  SAKURAI Masashi

;; Author: SAKURAI Masashi <m.sakurai at kiwanami.net>
;; Keywords: scroll, deferred

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a demonstration for deferred.el and co-routine.

;;; Installation:

;; This program needs deferred.el 
;; [http://github.com/kiwanami/emacs-deferred/blob/master/deferred.el].

;; Put following lines in your .emacs file.

;; (require 'inertial-scroll)
;; (inertias-global-minor-mode 1)

;;; Customize:

;; Scrolling parameters can be able to modify. You may make scrolling
;; smoother, customizing the interval time or velocity.
;; Here is a sample customize code.

;; (setq inertias-initial-velocity 50)
;; (setq inertias-friction 120)
;; (setq inertias-update-time 50)
;; (setq inertias-rest-coef 0.1)
;; (setq inertias-global-minor-mode-map 
;;       (inertias-define-keymap
;;        '(
;;          ;; Mouse wheel scrolling
;;          ("<wheel-up>"   . inertias-down-wheel)
;;          ("<wheel-down>" . inertias-up-wheel)
;;          ("<mouse-4>"    . inertias-down-wheel)
;;          ("<mouse-5>"    . inertias-up-wheel)
;;          ;; Scroll keys
;;          ("<next>"  . inertias-up)
;;          ("<prior>" . inertias-down)
;;          ("C-v"     . inertias-up)
;;          ("M-v"     . inertias-down)
;;          ) inertias-prefix-key))

;;; History:

;; Revision 1.1  2010/10/10  sakurai
;;  Added an option switch: inertias-rebound-flash.
;;  Added mouse wheel scrolling (thx @peccu).
;;  Modified default parameters.
;; 
;; Revision 1.0  2010/10/08  sakurai
;;  First release.

;;; Code:

(require 'deferred)

;;; Customize

(defvar inertias-initial-velocity 50.0
"Initial scrolling velocity (lines/sec). ")

(defvar inertias-initial-velocity-wheel 10.0
"Initial scrolling velocity for wheel (lines/sec).")

(defvar inertias-friction 120.0
"Frictional coefficient (lines/sec^2). The larger value stops
scrolling more quickly.")

(defvar inertias-update-time 50
"Interval time for scrolling (millisecond). The smaller value
makes scrolling smoother, but the emacs needs more machine
power.")

(defvar inertias-rest-coef 0.1
"Restitusion coefficient. The value 1.0 means elastic rebounding
and 0.0 does viscous.")

(defvar inertias-brake-coef 0.65
"Brake friction coefficient. This value should be less than
1.0. At the value 0.0, scrolling stops suddenly. At larger value,
scrolling needs more time to stop.")

(defvar inertias-rebound-flash t
"Rebounding flash effect at buffer edges. If nil, no flash
effect is shown.")

 

;;; Macros

(defmacro inertias-jslambda (args &rest body)
  (let ((argsyms (loop for i in args collect (gensym))))
  `(lambda (,@argsyms)
     (lexical-let (callee)
       (setq callee (lambda( ,@args ) ,@body))
       (funcall callee ,@argsyms)))))

(eval-when-compile
  (defun inertias-thread-line (wait-time chain line)
    (cond
     ;; function object
     ((functionp line)
      `(setq ,chain (deferred:nextc ,chain ,line)))
     ;; while loop form
     ((eq 'while (car line))
      (let ((condition (cadr line))
            (body (cddr line))
            (retsym (gensym)))
        `(setq ,chain 
               (deferred:nextc ,chain 
                 (inertias-jslambda
                  (x) (if ,condition 
                          (deferred:nextc 
                            (let ((,retsym (progn ,@body)))
                              (if (deferred-p ,retsym) ,retsym
                                (deferred:wait ,wait-time)))
                            callee)))))))
     ;; statement
     (t
      `(setq ,chain 
             (deferred:nextc ,chain 
               (lambda (x) ,line)))))))

(defmacro inertias-thread (wait-time &rest argbody)
  (let ((chain (gensym))
        (dstart (gensym)))
    `(lexical-let*
         (,chain
          (,dstart (deferred:new)))
       (setq ,chain ,dstart)
       ,@(loop for i in argbody
               collect
               (inertias-thread-line wait-time chain i))
       (deferred:callback ,dstart))))

 

;;; Commands

(defun inertias-up ()
  (interactive)
  (inertias-scrolling
   inertias-initial-velocity
   (selected-window)))

(defun inertias-down ()
  (interactive)
  (inertias-scrolling 
   (- inertias-initial-velocity)
   (selected-window)))

(defun inertias-up-wheel ()
  (interactive)
  (inertias-scrolling
   inertias-initial-velocity-wheel
   (selected-window)))

(defun inertias-down-wheel ()
  (interactive)
  (inertias-scrolling
   (- inertias-initial-velocity-wheel)
   (selected-window)))

(defun inertias-stop (&optional window)
  (interactive)
  (unless window
    (setq window (selected-window)))
  (inertias-window-velocity-clear window))

 

;;; Minor mode

(defun inertias-define-keymap (keymap-list &optional prefix)
  (let ((map (make-sparse-keymap)))
    (mapc 
     (lambda (i)
       (define-key map
         (if (stringp (car i))
             (read-kbd-macro 
              (if prefix 
                  (replace-regexp-in-string "prefix" prefix (car i))
                (car i)))
           (car i))
         (cdr i)))
     keymap-list)
    map))

(defvar inertias-prefix-key "C-c ' ")
(defvar inertias-minor-mode-child-map (make-sparse-keymap))
(defvar inertias-global-minor-mode-map 
  (inertias-define-keymap
   '(
     ("prefix u"     . inertias-down)
     ("prefix d"     . inertias-up)
     ("prefix <SPC>" . inertias-stop)
     ) inertias-prefix-key))

(defvar inertias-global-minor-mode-hook nil)

(define-minor-mode inertias-global-minor-mode
  "Inertial scrolling mode"
  :init-value nil
  :global t
  :lighter " IS"
  :keymap inertias-minor-mode-child-map
  :group 'inertias-global-mode
  (if inertias-global-minor-mode
      (progn
        (set-keymap-parent inertias-minor-mode-child-map 
                           inertias-global-minor-mode-map)
        (run-hooks 'inertias-global-minor-mode-hook))))

;;; Window-Velocity map

(defvar inertias-window-velocity-alist nil
"[internal] Alist of mapping from a window object to a velocity
value.")

(defun inertias-window-velocity-get (window)
  (assq window inertias-window-velocity-alist))

(defun inertias-window-velocity-set (window velocity)
  (inertias-window-velocity-clear window)
  (push (cons window velocity) inertias-window-velocity-alist))

(defun inertias-window-velocity-clear (window)
  (setq inertias-window-velocity-alist
        (assq-delete-all window inertias-window-velocity-alist)))

;;; Scrolling functions

(defun inertias-scrolling (init-vel window)
  (let ((pair (inertias-window-velocity-get window)))
    (cond
     ((null pair)
      (inertias-window-velocity-set window init-vel)
      (inertias-start-scroll-thread window))
     (t
      (let ((vel (cdr pair)))
        (if (and (> 0 (signum (* vel init-vel))) ; negative direction
                 (> (abs vel) (abs (* 0.5 init-vel)))) ; want to stop ?
            (inertias-brake-scrolling vel window) ; stop
          (incf (cdr pair) init-vel) ; acceleration
          ))))))

(defun inertias-start-scroll-thread (window)
  (lexical-let* ((pos 0) (window window) pair 
                 (last-time (float-time)))
    (inertias-thread
     inertias-update-time
     (while (progn
              (setq pair (inertias-window-velocity-get window))
              (and pair (cdr pair)))
       (let* ((prev-vel (cdr pair))
              (vel prev-vel)
              (dt (max (/ inertias-update-time 1000.0)
                       (- (float-time) last-time)))
              (frc (* dt inertias-friction))
              (scrnum 0)
              (prev-window-start (window-start window)))
         (setq last-time (float-time))
         (incf pos (* dt vel)) ; pos += vel * dt
         (setq scrnum
               (cond
                ((< 0 vel) ; upward scrolling
                 (decf vel frc) ; vel -= frc * dt
                 (floor pos))
                (t ; downward scrolling
                 (incf vel frc) ; vel += frc * dt
                 (ceiling pos))))
         (decf pos scrnum) ; saving residual value
         (cond
          ((or (>= 0 (signum (* prev-vel vel)))
               (not (window-live-p window)))
           (inertias-stop window))
          (t
           (setf (cdr pair) vel)
           (when (/= 0 scrnum)
             (inertias-scroll-window scrnum window)
             (when (eql prev-window-start (window-start window))
               ;; rebounding on the edge of buffer
               (setf (cdr pair) (* vel (- inertias-rest-coef)))
               (if inertias-rebound-flash
                   (inertias-rebound-effect window))))))))
     )))

(defun inertias-brake-scrolling (vel window)
  (inertias-stop)
  (lexical-let ((vel vel) (window window)
                (dt (/ inertias-update-time 1000.0)))
    (deferred:next
      (inertias-jslambda (x)
        (setq vel (* vel inertias-brake-coef))
        (let ((num (* vel dt)))
          (when (> (abs num) 1.0)
            (inertias-scroll-window (round num) window)
            (deferred:nextc
              (deferred:wait inertias-update-time)
              callee)))))))

(defun inertias-rebound-effect (window)
  (lexical-let (overlay (window window))
    (deferred:$
      (deferred:next
        (lambda (x)
          (with-selected-window window
            (setq overlay
                  (make-overlay (point-min) (point-max))))
          (overlay-put overlay 'face 'highlight)
          (deferred:wait inertias-update-time)))
      (deferred:nextc it
        (lambda (x)
          (when overlay
            (delete-overlay overlay)))))))

(defun inertias-scroll-window (num window)
  (ignore-errors
    (with-selected-window window
      (scroll-up num)))
  ;; for follow-mode
  (when (and (symbol-plist 'follow-mode)
             (buffer-local-value 'follow-mode (window-buffer window)))
    (loop for i in (get-buffer-window-list (window-buffer window))
          unless (eq window i)
          do (ignore-errors
               (with-selected-window i
                 (scroll-up num))))))


(provide 'inertial-scroll)
;;; inertial-scroll.el ends here
