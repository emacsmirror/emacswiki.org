;;; truthy.el --- Test the content of a value
;;
;; Copyright (c) 2012 Roland Walker
;;
;; Author: Roland Walker <walker@pobox.com>
;; Homepage: http://github.com/rolandwalker/truthy
;; URL: http://raw.github.com/rolandwalker/truthy/master/truthy.el
;; Version: 0.2.2
;; Last-Updated: 30 Oct 2012
;; Package-Requires: ((list-utils "0.1.2"))
;; EmacsWiki: Truthy
;; Keywords: extensions
;;
;; Simplified BSD License
;;
;;; Commentary:
;;
;; Quickstart
;;
;;     (require 'truthy)
;;
;;     (truthy "")                   ; nil
;;     (truthy '[])                  ; nil
;;     (truthy 0)                    ; nil
;;     (truthy (lambda ()))          ; nil
;;     (truthy (make-sparse-keymap)) ; nil
;;     (truthy 1)                    ; t
;;     (truthy '(a b c))             ; t
;;     (truthy '(nil nil nil))       ; nil
;;
;; Explanation
;;
;; This library has no user-level interface; it is only useful
;; for programming in Emacs Lisp.  Two functions are provided:
;;
;;     `truthy'
;;     `truthy-s'
;;
;; Truthy provides an alternative measure of the "truthiness" of a
;; value.  Whereas Lisp considers any non-nil value to be "true" when
;; evaluating a Boolean condition, `truthy' considers a value to be
;; "true" if it has *content*.  If the value is a string or buffer, it
;; must have non-zero length.  If a number, it must be non-zero.  If a
;; hash, it must have keys.  If a window, it must be live.  See the
;; docstring to `truthy' for more details.
;;
;; To use truthy, place the truthy.el library somewhere Emacs can find
;; it, and add the following to your ~/.emacs file:
;;
;;     (require 'truthy)
;;
;; Notes
;;
;; Compatibility and Requirements
;;
;;     GNU Emacs version 24.3-devel     : yes
;;     GNU Emacs version 24.1 & 24.2    : yes
;;     GNU Emacs version 23.3           : yes
;;     GNU Emacs version 22.3 and lower : no
;;
;;     Uses if present: list-utils.el
;;
;; Bugs
;;
;; TODO
;;
;;     obarray per string-utils
;;
;;     Tests for truthy-s.
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

;; for subseq
(require 'cl)

(require 'eieio nil t)
(require 'list-utils nil t)

;;; declarations

(declare-function object-p "eieio.el")
(declare-function process-live-p "subr.el")
(declare-function ring-elements "ring.el")

(eval-when-compile
  (defvar eieio-unbound))

;;; compatibility functions

(unless (fboundp 'process-live-p)
  (defun process-live-p (process)
    "Returns non-nil if PROCESS is alive.
A process is considered alive if its status is `run', `open',
`listen', `connect' or `stop'."
    (memq (process-status process)
          '(run open listen connect stop))))

;;; external interface

;;;###autoload
(defun truthy (obj &optional shallow)
  "Return non-nil if OBJ has \"truthiness\".

Whereas Lisp considers any non-nil value to be \"true\" when
evaluating a Boolean condition, `truthy' considers a value to be
\"true\" if it has *content*.

If OBJ is a constant or symbol other than nil, it is always truthy.

If OBJ is a number, it must be non-zero.

If OBJ is a hash table, it must have keys.

If OBJ is a function or a macro, it must have a body containing
at least one truthy value.

If OBJ is compiled byte-code, it must have a body.

If OBJ is a keymap, it must have defined keys.

If OBJ is a char-table, it must have keys as returned by
`map-char-table'.

If OBJ is a abbrev-table, it must have at least one value.

If OBJ is a defstruct or EIEIO object, at least one slot must
contain a truthy value.

If OBJ is a ring, it must contain at least one truthy element.

If OBJ is a string, it must have length greater than zero.

If OBJ is a sequence, if must contain at least one truthy
value.

If OBJ is a marker or overlay, it must be associated
with a buffer and have a position.

If OBJ is a buffer, it must be live and of non-zero length.

If OBJ is a frame, window, or process, it must be live, according
to (eg) `window-live-p'.

If OBJ is a font, at least one property of the font must be
specified.

When optional SHALLOW is non-nil, recursive considerations
do not examine truthiness, simply whether constituent elements
are non-nil.  Therefore

    (truthy '(0 0 0 0))

returns nil, because no truthy element is found in the list.  But

    (truthy '(0 0 0 0) 'shallow)

returns non-nil, because the list holds non-nil elements.

The function `truthy-s' is provided as shorthand for
\(truthy OBJ 'shallow\)."

  (cond

    ;; nil
    ((null obj)
     nil)

    ;; '[]
    ((and (vectorp obj)
          (= 0 (length obj)))
     nil)

    ;; defstruct
    ((and (vectorp obj)
          (symbolp (aref obj 0))
          (string-match-p "\\`cl-" (symbol-name (aref obj 0))))
     (catch 'truthy
       (dolist (elt (append (subseq obj 1 (length obj)) nil))
         (when (or (and shallow elt)
                   (truthy elt))
           (throw 'truthy t)))
       nil))

    ;; compiled byte-code
    ((byte-code-function-p obj)
     (aref obj 1))

    ;; ring
    ((ring-p obj)
     (catch 'truthy
       (dolist (elt (ring-elements obj))
         (when (or (and shallow elt)
                   (truthy elt))
           (throw 'truthy t)))
       nil))

    ;; macro
    ((and (listp obj)
          (eq 'macro (car obj))
          (functionp (cdr obj)))
     (catch 'truthy
       (dolist (elt (cdddr obj))
         (when (or (and shallow elt)
                   (truthy elt))
           (throw 'truthy t)))
       nil))

    ;; function
    ((functionp obj)
     (catch 'truthy
       (dolist (elt (cddr obj))
         (when (or (and shallow elt)
                   (truthy elt))
           (throw 'truthy t)))
       nil))

    ;; frame-configuration
    ((frame-configuration-p obj)
     (catch 'truthy
       (dolist (elt (cdr obj))
         (when (or (and shallow elt)
                   (truthy elt))
           (throw 'truthy t)))
       nil))

    ;; keymap
    ((keymapp obj)
     (not (equal obj (make-sparse-keymap))))

    ;; number
    ((numberp obj)
     (/= 0 obj))

    ;; buffer
    ((bufferp obj)
     (and (buffer-live-p obj)
          (> (with-current-buffer obj (point-max)) 1)))

    ;; frame
    ((framep obj)
     (frame-live-p obj))

    ;; window
    ((windowp obj)
     (window-live-p obj))

    ;; marker
    ((markerp obj)
     (and (marker-buffer obj)
          (marker-position obj)))

    ;; overlay
    ((overlayp obj)
     (and (overlay-buffer obj)
          (overlay-start obj)
          (overlay-end obj)))

    ;; process
    ((processp obj)
     (process-live-p obj))

    ;; EIEIO object
    ((and (fboundp 'object-p)
          (object-p obj))
     (catch 'truthy
       (dolist (elt (remq eieio-unbound (append (subseq obj 3 (length obj)) nil)))
         (when (or (and shallow elt)
                   (truthy elt))
           (throw 'truthy t)))
       nil))

    ;; font object
    ((fontp obj)
     (catch 'truthy
       (dolist (k '(:family :weight :slant :width :foundry :adstyle :registry :size :name :script :otf))
         (when (font-get obj k)
           (throw 'truthy t)))
       nil))

    ;; abbrev-table
    ((ignore-errors (abbrev-table-p obj))
     (catch 'truthy
       (mapatoms #'(lambda (sym)
                     (when (> (length (symbol-name sym)) 0)
                       (throw 'truthy t))) obj)
       nil))

    ;; hash-table
    ((hash-table-p obj)
     (catch 'truthy
       (maphash #'(lambda (k v)
                    (throw 'truthy t))
                obj)
       nil))

    ;; char-table
    ((char-table-p obj)
     (catch 'truthy
       (map-char-table #'(lambda (k v)
                           (throw 'truthy t))
                       obj)
       nil))

    ;; string
    ((stringp obj)
     (> (length obj) 0))

    ;; list
    ((listp obj)
     (let* ((measurer (if (fboundp 'list-utils-safe-length) 'list-utils-safe-length 'safe-length))
            (len (funcall measurer obj)))
       (cond
         ((and (consp obj)
               (> len 0)
               (not (listp (nthcdr len obj))))
          ;; cons or improper list would choke mapcar
          (and (truthy (subseq obj 0 len))
               (truthy (nthcdr len obj))))
         (t
          (catch 'truthy
            ;; subseq is to break circular lists
            (dolist (elt (subseq obj 0 (funcall measurer obj)))
              (when (or (and shallow elt)
                        (truthy elt))
                (throw 'truthy t)))
            nil)))))

    ;; non-list sequence
    ((sequencep obj)
     (catch 'truthy
       (dolist (elt (mapcar 'identity (append obj nil)))
         (when (or (and shallow elt)
                   (truthy elt))
           (throw 'truthy t)))
       nil))

    ;; fallback
    (t
     obj)))

;;;###autoload
(defun truthy-s (obj)
  "Return non-nil if OBJ has shallow \"truthiness\".

This is equivalent to calling `truthy' with the SHALLOW parameter
set."
  (truthy obj 'shallow))

(provide 'truthy)

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
;; LocalWords: Truthy truthiness ARGS alist devel truthy mapcar EIEIO
;; LocalWords: defstruct subseq utils eieio subr
;;

;;; truthy.el ends here
