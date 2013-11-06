;;; window-end-visible.el --- Find the last visible point in a window
;;
;; Copyright (c) 2012-13 Roland Walker
;;
;; Author: Roland Walker <walker@pobox.com>
;; Homepage: http://github.com/rolandwalker/window-end-visible
;; URL: http://raw.github.com/rolandwalker/window-end-visible/master/window-end-visible.el
;; Version: 0.1.0
;; Last-Updated: 29 Oct 2013
;; EmacsWiki: WindowEndVisible
;; Keywords: extensions
;;
;; Simplified BSD License
;;
;;; Commentary:
;;
;; Quickstart
;;
;;     (require 'window-end-visible)
;;
;;     ;; open a buffer larger than the window
;;
;;     ;; may return nil
;;     M-: (pos-visible-in-window-p (window-end)) RET
;;
;;     ;; always returns t
;;     M-: (pos-visible-in-window-p (window-end-visible)) RET
;;
;; Explanation
;;
;; Window-end-visible.el has no user-level interface, and is only
;; useful when programming Emacs Lisp.
;;
;; This library provides the function `window-end-visible', which
;; works around a limitation of `window-end', at a speed penalty.
;;
;; The issue this function solves is that the following is not true
;; as might be expected:
;;
;;    (pos-visible-in-window-p (window-end))
;;
;; `window-end-visible' returns the "true" window end: the last
;; visible position in the window, verified by testing with
;; `pos-visible-in-window-p'.
;;
;; The speed penalty of `window-end-visible' over `window-end' varies
;; depending on your configuration.  For example, tabbar.el makes
;; calling `pos-visible-in-window-p' quite expensive.
;;
;; To use window-end-visible, place the window-end-visible.el library
;; somewhere Emacs can find it, and add the following to your ~/.emacs
;; file:
;;
;;     (require 'window-end-visible)
;;
;; Notes
;;
;; Compatibility and Requirements
;;
;;     GNU Emacs version 24.4-devel     : yes, at the time of writing
;;     GNU Emacs version 24.3           : yes
;;     GNU Emacs version 23.3           : yes
;;     GNU Emacs version 22.2           : yes, with some limitations
;;     GNU Emacs version 21.x and lower : unknown
;;
;;     No external dependencies
;;
;; Bugs
;;
;; TODO
;;
;;; License
;;
;; Simplified BSD License:
;;
;; Redistribution and use in source and binary forms, with or
;; without modification, are permitted provided that the following
;; conditions are met:
;;
;;    1. Redistributions of source code must retain the above
;;       copyright notice, this list of conditions and the following
;;       disclaimer.
;;
;;    2. Redistributions in binary form must reproduce the above
;;       copyright notice, this list of conditions and the following
;;       disclaimer in the documentation and/or other materials
;;       provided with the distribution.
;;
;; This software is provided by Roland Walker "AS IS" and any express
;; or implied warranties, including, but not limited to, the implied
;; warranties of merchantability and fitness for a particular
;; purpose are disclaimed.  In no event shall Roland Walker or
;; contributors be liable for any direct, indirect, incidental,
;; special, exemplary, or consequential damages (including, but not
;; limited to, procurement of substitute goods or services; loss of
;; use, data, or profits; or business interruption) however caused
;; and on any theory of liability, whether in contract, strict
;; liability, or tort (including negligence or otherwise) arising in
;; any way out of the use of this software, even if advised of the
;; possibility of such damage.
;;
;; The views and conclusions contained in the software and
;; documentation are those of the authors and should not be
;; interpreted as representing official policies, either expressed
;; or implied, of Roland Walker.
;;
;;; Code:
;;

;;; requirements

;; for callf
(require 'cl)

;;; variables

(defvar window-end-visible--cache               nil "Cached value of last visible position.")
(defvar window-end-visible--pos-visible-p-cache nil "Cache data for `window-end-visible--pos-visible-p'.")

;;; utility functions

(defun window-end-visible--pos-visible-p (pos window window-configuration &optional partially)
  "A cached version of `pos-visible-in-window-p'.

POS and WINDOW are as documented at `pos-visible-in-window-p'.

WINDOW-CONFIGURATION should be the current window configuration.
When WINDOW-CONFIGURATION changes, the cache data is cleared.

PARTIALLY is as documented at `pos-visible-in-window-p'."
  (unless (equal (car window-end-visible--pos-visible-p-cache)
                 window-configuration)
    (setq window-end-visible--pos-visible-p-cache (list window-configuration nil)))
  (let ((vis nil))
    (if (assoc (list pos window partially) (cdr window-end-visible--pos-visible-p-cache))
        (setq vis (cdr (assoc (list pos window partially) (cdr window-end-visible--pos-visible-p-cache))))
      ;; else
      (setq vis (pos-visible-in-window-p pos window partially))
      (push (cons (list pos window partially) vis) (cdr window-end-visible--pos-visible-p-cache))
      vis)))

;;; main interface

;;;###autoload
(defun window-end-visible (&optional window update partially)
  "Return the last visible position in WINDOW.

Works around a limitation of `window-end', at a speed penalty.

The issue this function solves is that the following is not true
as might be expected:

   (pos-visible-in-window-p (window-end))

The speed penalty varies greatly depending on your configuration.
For example, tabbar.el makes calling `pos-visible-in-window-p'
quite expensive.

WINDOW and UPDATE are as documented at `window-end'.

PARTIALLY is as documented at `pos-visible-in-window-p'."
  (callf or window (selected-window))
  (let ((cwc (current-window-configuration)))
    (if (equal (car window-end-visible--cache) (list window cwc partially))
        (cdr window-end-visible--cache)
      ;; else
      (setq window-end-visible--cache nil)
      (let* ((pos (window-end window update))
             (orig-pos pos)
             (lim (max (window-start window) (with-current-buffer (window-buffer window) (point-min)))))
        ;; hop back and forth to minimize the number of tests, may not
        ;; matter much now that it is cached
        (while (and (not (window-end-visible--pos-visible-p pos window cwc partially))
                    (> (- pos 20) lim))
          (decf pos 20))
        (while (and (window-end-visible--pos-visible-p pos window cwc partially)
                    (< (+ pos 10) orig-pos))
          (incf pos 10))
        (while (and (not (window-end-visible--pos-visible-p pos window cwc partially))
                      (> (- pos 5) lim))
          (decf pos 5))
        (while (and (window-end-visible--pos-visible-p pos window cwc partially)
                    (< (+ pos 1) orig-pos))
          (incf pos))
        (while (and (not (window-end-visible--pos-visible-p pos window cwc partially))
                    (> (- pos 1) lim))
          (decf pos))
        (if (window-end-visible--pos-visible-p pos window cwc partially)
            (progn
              (setq window-end-visible--cache (cons (list window cwc partially) pos))
              pos)
          nil)))))

(provide 'window-end-visible)

;;
;; Emacs
;;
;; Local Variables:
;; indent-tabs-mode: nil
;; mangle-whitespace: t
;; require-final-newline: t
;; coding: utf-8
;; byte-compile-warnings: (not cl-functions redefine)
;; End:
;;
;; LocalWords:  WindowEndVisible ARGS alist callf
;;

;;; window-end-visible.el ends here
