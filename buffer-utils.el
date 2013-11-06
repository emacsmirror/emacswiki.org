;;; buffer-utils.el --- Buffer-manipulation utility functions
;;
;; Copyright (c) 2012 Roland Walker
;;
;; Author: Roland Walker <walker@pobox.com>
;; Homepage: http://github.com/rolandwalker/buffer-utils
;; URL: http://raw.github.com/rolandwalker/buffer-utils/master/buffer-utils.el
;; Version: 0.1.0
;; Last-Updated: 21 Oct 2013
;; EmacsWiki: BufferUtils
;; Keywords: extensions
;;
;; Simplified BSD License
;;
;;; Commentary:
;;
;; Quickstart
;;
;;     (require 'buffer-utils)
;;
;;     (buffer-utils-save-order
;;       (bury-buffer "*scratch*"))
;;
;;     ;; buffer order is now restored
;;
;; Explanation
;;
;; Buffer-utils.el is a collection of functions for buffer manipulation.
;;
;; This library exposes very little user-level interface; it is
;; generally useful only for programming in Emacs Lisp.
;;
;; To use buffer-utils, place the buffer-utils.el library somewhere
;; Emacs can find it, and add the following to your ~/.emacs file:
;;
;;     (require 'buffer-utils)
;;
;; The following functions and macros are provided:
;;
;;     `buffer-utils-all-in-mode'
;;     `buffer-utils-all-matching'
;;     `buffer-utils-bury-and-forget'   ; can be called interactively
;;     `buffer-utils-first-matching'
;;     `buffer-utils-huge-p'
;;     `buffer-utils-in-mode'
;;     `buffer-utils-most-recent-file-associated'
;;     `buffer-utils-narrowed-p'
;;     `buffer-utils-save-order'
;;     `buffer-utils-set-order'
;;
;; of which `buffer-utils-save-order' is the most notable.
;;
;; `buffer-utils-save-order' is a macro, similar to `save-current-buffer',
;; which saves and restores the order of the buffer list.
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
;;    No external dependencies
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

;; for callf, loop, gensym
(require 'cl)

;;; declarations

(declare-function buffer-utils-system-bury-buffer "buffer-utils.el")
(declare-function window-normalize-buffer         "window.el")

;;; variables

(defvar buffer-utils-huge-cutoff 256000
  "A buffer is considered to be huge if it has this many characters.

This defaults to the most recent value of `font-lock-maximum-size',
which is being obsoleted.")

;;; aliases and fsets

;; this fset is done so that the following construct is legal:
;;
;;     (flet ((bury-buffer (&optional buffer-or-name)
;;                         (buffer-utils-bury-and-forget buffer-or-name)))
;;          ... do something here...)

(fset 'buffer-utils-system-bury-buffer (symbol-function 'bury-buffer))

;;; compatibility functions

(unless (fboundp 'with-demoted-errors)
  ;; added in 23.x
  (defmacro with-demoted-errors (&rest body)
    "Run BODY and demote any errors to simple messages."
    (declare (debug t) (indent 0))
    (let ((err (make-symbol "err")))
      `(condition-case ,err
           (progn ,@body)
         (error (message "Error: %S" ,err) nil)))))

;;; macros

;;;###autoload
(defmacro buffer-utils-save-order (&rest body)
  "Execute BODY, then restore previous buffer order.

This macro saves the buffer order, executes BODY, then restores
the saved order.  The return value is the last form in BODY.

The buffer order is also restored if BODY exits nonlocally.

Note: BODY should not add or remove buffers, but a graceful
recovery will be attempted if that does occur."
  (declare (indent 0) (debug t))
  (let ((o (gensym "--buff-order--")))
    `(let ((,o (buffer-list)))
       (unwind-protect (progn ,@body)
         ;; same logic as buffer-utils-set-order
         (dolist (b ,o)
           (when (and (bufferp b)
                      (buffer-live-p b))
             (bury-buffer b)))
         (dolist (b (buffer-list))
           (unless (memq b ,o)
             (bury-buffer b)))))))

;;; utility functions

;;;###autoload
(defun buffer-utils-huge-p (&optional buffer)
  "Return t if BUFFER is huge.

BUFFER is optional, and defaults to the current buffer.

The size cutoff is defined by the variable
`buffer-utils-huge-cutoff'."
  (callf or buffer (current-buffer))
  (with-current-buffer buffer
    (> (point-max) buffer-utils-huge-cutoff)))

;;;###autoload
(defun buffer-utils-narrowed-p (&optional buffer)
  "Return t if BUFFER is narrowed.

BUFFER is optional, and defaults to the current buffer."
  (callf or buffer (current-buffer))
  (with-current-buffer buffer
    (if (fboundp 'buffer-narrowed-p)
        (buffer-narrowed-p)
      (or (/= (point-min) 1)
          (/= (point-max) (1+ (buffer-size)))))))

;;;###autoload
(defun buffer-utils-in-mode (buffer mode &optional derived)
  "Return non-nil if BUFFER is in `major-mode' MODE.

If BUFFER is nil, tests the current buffer.

If optional DERIVED is non-nil, return non-nil if the
`major-mode' of BUFFER is the same as or derived from MODE."
  (callf or buffer (current-buffer))
  (with-current-buffer buffer
    (if derived
        (derived-mode-p mode)
      ;; else
      (eq mode major-mode))))

;;;###autoload
(defun buffer-utils-all-matching (predicate &optional skip-current)
  "Return all buffers where PREDICATE applied to the buffer is non-nil.

Return value is a list of buffers, or nil if there is no match.

If optional SKIP-CURRENT is non-nil, skip the current buffer."
  (loop for buf in (if skip-current (cdr (buffer-list)) (buffer-list))
        when (with-current-buffer buf (funcall predicate buf))
        collect buf))

;;;###autoload
(defun buffer-utils-first-matching (predicate &optional skip-current)
  "Return the first buffer where PREDICATE applied to the buffer is non-nil.

Return value is a buffer object, or nil if there is no match.

If optional SKIP-CURRENT is non-nil, skip the current buffer."
  (loop for buf in (if skip-current (cdr (buffer-list)) (buffer-list))
        when (with-current-buffer buf (funcall predicate buf))
        return buf))

;;;###autoload
(defun buffer-utils-all-in-mode (mode &optional derived skip-current)
  "Return all buffers in `major-mode' MODE.

Return value is a list of buffers, or nil if there is no match.

If optional DERIVED is non-nil, return all buffers whose
`major-mode' is the same as or derived from MODE.

If optional SKIP-CURRENT is non-nil, skip the current buffer."
  (buffer-utils-all-matching #'(lambda (buf)
                                 (buffer-utils-in-mode buf mode derived)) skip-current))

;;;###autoload
(defun buffer-utils-most-recent-file-associated (&optional skip-current)
  "Return the most-recently visited buffer which is associated with a file.

If optional SKIP-CURRENT is non-nil, skip the current buffer."
  (buffer-utils-first-matching 'buffer-file-name skip-current))

;; this logic duplicated in macro buffer-utils-save-order
;;;###autoload
(defun buffer-utils-set-order (new-order)
  "Reorders the buffer list to match NEW-ORDER.

Elements of NEW-ORDER which are not active buffers are silently
ignored.

Current buffers which are not members of NEW-ORDER are moved to
the end of the buffer list, maintaining their current relative
order."
  (dolist (b new-order)
    (when (and (bufferp b)
               (buffer-live-p b))
      (bury-buffer b)))
  (dolist (b (buffer-list))
    (unless (memq b new-order)
      (bury-buffer b))))

;;;###autoload
(defun buffer-utils-bury-and-forget (&optional buffer-or-name win)
  "Bury BUFFER-OR-NAME and forget about it.

BUFFER-OR-NAME is optional, and defaults to the current buffer.

\"Burying\" refers to the list of buffers returned by
`buffer-list'.  A buried buffer is moved to the end of the list.

\"Forgetting\" refers to window WIN's memory of what it has
displayed.  This only has an effect in GNU Emacs 24.1 or higher.
WIN is optional, and defaults to the selected window.

See `bury-buffer' and `unrecord-window-buffer'."
  (interactive)
  (buffer-utils-system-bury-buffer buffer-or-name)
  (when (fboundp 'unrecord-window-buffer)
    (unrecord-window-buffer win (window-normalize-buffer buffer-or-name)))
  nil)

(provide 'buffer-utils)

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
;; LocalWords: BufferUtils ARGS alist utils nonlocally callf WIN's
;; LocalWords: fsets fset flet devel gensym
;;

;;; buffer-utils.el ends here
