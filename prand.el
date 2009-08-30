;;; prand.el --- generate pesudo-random number

;; Copyright 2006 Ye Wenbin
;;
;; Author: wenbinye@163.com
;; Time-stamp: <Ye Wenbin 2006-11-08 09:00:53>
;; Version: $Id: random.el,v 0.0 <2006-11-07 20:24:10> ywb Exp $
;; Keywords: 
;; X-URL:n not distributed yet

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

;; 

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'prand)

;;; Code:

(provide 'prand)
(eval-when-compile
  (require 'cl))

(require 'calc)
(require 'calc-misc)
(require 'calc-ext)
(defvar calc-command-flags nil)         ; how to avoid this error

(defvar prand-seed 1)
(defvar prand-k 16807)
(defvar prand-m (math-sub (math-pow 2 31) 1))

(defun prand-rand-1 ()
  (setq prand-seed
        (math-mod (math-mul prand-k prand-seed)
                  prand-m)))

(defun prand-rand (&optional n)
  (cond ((eq n 't)
         ;; although (random t) is much more easier, use time
         ;; to make this function independent to build-in function
         ;; `random'
         (setq prand-seed
               (floor (* (mod (float-time) 1) #xfffffff))))
        ((null n) (string-to-number
                   (math-format-number
                    (math-mod (prand-rand-1) most-positive-fixnum))))
        ((numberp n) (string-to-number
                      (math-format-number
                       (math-mod (prand-rand-1) n))))
        (t (error "Wrong type of arguments"))))

;;; prand.el ends here
