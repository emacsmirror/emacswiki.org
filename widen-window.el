;;; widen-window.el --- Widening selecting window

;; Copyright (C) 2008  Yuto Hayamizu

;; Author: Yuto Hayamizu <y.hayamizu@gmail.com>
;; Keywords: convenience
;; Version: 0.1.1

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
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

;; This minor mode, widen window mode, provides a function that widen
;; selected window automatically.
;; It was tested only on Emacs 22.

;; In order to use this minor mode, put this file into
;; a directory included in load-path,
;; and add following code to your .emacs.
;; +------------------------+
;; (require 'widen-window)
;; (global-widen-window-mode t)
;; +------------------------+

;; You can change the window size ratio by customizing `ww-ratio'.
;; `ww-ratio' must be greater than 0.0 and less than 1.0 .

;; If you want to disable widen window mode in a certain
;; major mode(say `foo-mode'), add `foo-mode' to the variable `ww-nonwide-modes'.

;; If `ww-width' is non-nil, horizontal window widening is done.
;; You can turn it off by setting `ww-width' nil.
;; `ww-height' is the same as.

;; Window widening function `widen-current-window' is called after the
;; invocation of a function listed in `ww-advised-functions'.
;; By adding functions to or removing from this variable, you can
;; control the invocation of window widening.

;;; Code:

(require 'easy-mmode)
(require 'cl)

(defgroup widen-window nil
  "Widen selected window"
  :group 'convenience
  :prefix "widen-window-")

(defcustom ww-ratio 0.625
  "This is a ratio which the selected window takes up in window subtree."
  :group 'widen-window
  :type 'number
  )

(defcustom ww-nonwide-modes
  '(dummy1-mode dummy2-mode)
  "Major modes `widen-current-window' cannot run."
  :type '(list symbol)
  :group 'widen-window)

(defcustom ww-height
  t
  "If `ww-height' is non-nil, widen-window for height will work."
  :type '(choice (const :tag "Yes" t)
		 (const :tag "No" nil))
  :group 'widen-window)

(defcustom ww-width
  t
  "If `ww-width' is non-nil, widen-window for width will work."
  :type '(choice (const :tag "Yes" t)
		 (const :tag "No" nil))
  :group 'widen-window)

(defcustom ww-advised-functions
  '(other-window
    split-window
    switch-to-buffer
    mouse-drag-region
    delete-window
    add-change-log-entry-other-window
    help-do-xref
    )
  "Functions to be advised. Window widening function `widen-current-window' is fired after advised function was called."
  :type '(list symbol)
  :group 'widen-window)

(defun widen-current-window ()
  "The very function which resizes the current window."

  (interactive)

  (unless (minibufferp (current-buffer))
    (cond
     ((>= 0 ww-ratio) (setq ww-ratio 0.2))
     ((<= 1 ww-ratio) (setq ww-ratio 0.8)))
  
    (let* ((current-window (selected-window))
	   (window-tree (bw-get-tree (selected-frame))))
      (when window-tree
	;; Sometimes, you cannot get correctly resized windows
	;; by calling ww-subtree only once.
	;; So ww-subtree is called repeatedly until
	;; www-subtree makes no change.
	(let ((sizeinfo-history nil)
	      (last-sizeinfo nil)
	      (windows (window-list nil nil)))
	  (while (not (member last-sizeinfo sizeinfo-history))
	    (setq sizeinfo-history
		  (cons last-sizeinfo sizeinfo-history))
	    (setq last-sizeinfo
		  (mapcar (lambda (w)
			    (window-edges w))
			  windows))	    
	    (ww-subtree
	     window-tree current-window
	     (- (bw-r window-tree) (bw-l window-tree))
	     (- (bw-b window-tree) (bw-t window-tree)))
	    )))
	)))

(defun ww-bw-wid (window-or-tree)
  "Returns the width of WINDOW-OR-TREE"
  (- (bw-r window-or-tree) (bw-l window-or-tree)))

(defun ww-bw-hei (window-or-tree)
  "Returns the height of WINDOW-OR-TREE"
  (- (bw-b window-or-tree) (bw-t window-or-tree)))

(defun ww-sign (num)
  (if (>= num 0)
      +1
    -1))

(defun ww-adjust-window (wtree delta horiz-p)
  "Smart wrapper of `bw-adjust-window'

If `bw-adjust-window' fails to change the size of a window to specified size(ex. tried too big size), it does nothing (on Emacs22), and `widen-current-window' thinks that resizing iteration was finished, and `widen-current-window' actually does nothing.
If `ww-adjust-window' fails to resize, it tries smaller change than specified."
  (if horiz-p
	;; width changes
      (let (last-width)
	(while (> (abs delta) 0)
	  (setq last-width (ww-bw-wid wtree))
	  (bw-adjust-window wtree delta horiz-p)
	  (let ((wid-change (- (ww-bw-wid wtree) last-width)))
	    (if (eq wid-change 0)
		(setq delta (* (ww-sign delta) (floor (* 0.66 (abs delta)))))
	      (setq delta (- delta wid-change))))))

    ;; height changes
    (let (last-height)
	(while (> (abs delta) 0)
	  (setq last-height (ww-bw-hei wtree))
	  (bw-adjust-window wtree delta horiz-p)
	  (let ((hei-change (- (ww-bw-hei wtree) last-height)))
	    (if (eq hei-change 0)
		(setq delta (* (ww-sign delta) (floor (* 0.66 (abs delta)))))
	      (setq delta (- delta hei-change))))))
	)
    )

(defun ww-subtree (wtree cur-win wid hei)
  (setq wtree (bw-refresh-edges wtree))
  (unless wid (setq wid (ww-bw-wid wtree)))
  (unless hei (setq hei (ww-bw-hei wtree)))
  (let ((wtree-wid (ww-bw-wid wtree))
	(wtree-hei (ww-bw-hei wtree)))
    (if (windowp wtree)
	(progn
	  (when wid
	    (let ((dw (- wid wtree-wid)))
	      (when (/= 0 dw)
		(ww-adjust-window wtree dw t))))
	  (when hei
	    (let ((dh (- hei wtree-hei)))
	      (when (/= 0 dh)
		(ww-adjust-window wtree dh nil))))
	  )
      (let* ((children (cdr (assq 'childs wtree)))
	     (cwin-num (length children))
	     (cwin-bigger-wid wid)
	     (cwin-bigger-hei hei)
	     (cwin-smaller-wid wid)
	     (cwin-smaller-hei hei))
	(case (bw-dir wtree)
	  ((hor)
	   (setq cwin-smaller-wid
		 (floor (/ (* wtree-wid (- 1 ww-ratio))
			   (- cwin-num 1))))
	   (setq cwin-bigger-wid
		 (- wtree-wid (* (- cwin-num 1) 
			       cwin-smaller-wid))))
	  ((ver)
	   (setq cwin-smaller-hei
		 (floor (/ (* wtree-hei (- 1 ww-ratio))
			   (- cwin-num 1))))
	   (setq cwin-bigger-hei
		 (- wtree-hei (* (- cwin-num 1) 
			       cwin-smaller-hei)))))
	(dolist (cwin children)
	  (if (ww-find-window-in-subtree cwin cur-win)
	      (ww-subtree
	       cwin cur-win
	       (if ww-width cwin-bigger-wid (ww-bw-wid cwin))
	       (if ww-height cwin-bigger-hei (ww-bw-hei cwin)))
	    (ww-subtree
	     cwin cur-win
	     (if ww-width cwin-smaller-wid (ww-bw-wid cwin))
	     (if ww-height cwin-smaller-hei (ww-bw-hei cwin)))))
	))))

(defun ww-find-window-in-subtree (wt window)
  (block func
    (cond
     ((windowp wt)
      (if (equal wt window)
	  window
	nil))
     (t
      (dolist (subwt (cdr (assq 'childs wt)))
	(let ((ret (ww-find-window-in-subtree subwt window)))
	  (when ret
	    (return-from func window))))
      nil))))

(defun ww-setup-advice ()
  (dolist (func ww-advised-functions)
    (when (fboundp func)
      (eval `(defadvice ,func (after widen-window-advice)
	       (if (and widen-window-mode (not (memq major-mode ww-nonwide-modes)))
		   (widen-current-window))))))
  (ad-activate-regexp "widen-window"))

(define-minor-mode widen-window-mode
  "Widen Window mode"
  :lighter " WW"
  :group 'widen-window
  (if widen-window-mode
      (progn
	(ww-setup-advice)
	(if (memq major-mode ww-nonwide-modes)
	    (widen-window-mode nil)))
    nil))

(defun widen-window-mode-maybe ()
  "Return t and enable widen-window-mode if `widen-current-window' can called on current buffer."
  (if (and (not (minibufferp (current-buffer)))
	   (not (memq major-mode ww-nonwide-modes)))
      (widen-window-mode t)))

(define-global-minor-mode global-widen-window-mode
  widen-window-mode widen-window-mode-maybe
  :group 'widen-window)

;;; for anything.el
;; (defadvice anything (around disable-ww-mode activate)
;;   (ad-deactivate-regexp "widen-window")
;;   (unwind-protect
;;       ad-do-it
;;     (ad-activate-regexp "widen-window")))

;; (if (fboundp 'anything)
;;     (ad-activate-regexp "disable-ww-mode"))

(provide 'widen-window)
;;; widen-window.el ends here
