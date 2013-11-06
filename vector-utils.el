;;; vector-utils.el --- Vector-manipulation utility functions
;;
;; Copyright (c) 2012-13 Roland Walker
;;
;; Author: Roland Walker <walker@pobox.com>
;; Homepage: http://github.com/rolandwalker/vector-utils
;; URL: http://raw.github.com/rolandwalker/vector-utils/master/vector-utils.el
;; Version: 0.1.2
;; Last-Updated: 24 Oct 2013
;; EmacsWiki: VectorUtils
;; Keywords: extensions
;;
;; Simplified BSD License
;;
;;; Commentary:
;;
;; Quickstart
;;
;;     (require 'vector-utils)
;;
;;     (vector-utils-flatten '[1 2 [3 4 [5 6 7]]])
;;     ;; '[1 2 3 4 5 6 7]
;;
;;     (vector-utils-depth '[1 2 [3 4 [5 6 7]]])
;;     ;; 3
;;
;; Explanation
;;
;; Vector-utils is a collection of functions for vector manipulation.
;; This library has no user-level interface; it is only useful for
;; programming in Emacs Lisp.
;;
;; Furthermore (when programming in Emacs Lisp), be aware that the
;; modification of a vector is not permitted: only vector *elements*
;; may be changed.  All "modification" operations herein can only
;; work by making copies, which is not efficient.
;;
;; The following functions are provided:
;;
;;     `vector-utils-depth'
;;     `vector-utils-flatten'
;;     `vector-utils-insert-before'
;;     `vector-utils-insert-after'
;;     `vector-utils-insert-before-pos'
;;     `vector-utils-insert-after-pos'
;;
;; To use vector-utils, place the vector-utils.el library somewhere
;; Emacs can find it, and add the following to your ~/.emacs file:
;;
;;     (require 'vector-utils)
;;
;; Notes
;;
;; Compatibility and Requirements
;;
;;     GNU Emacs version 24.4-devel     : yes, at the time of writing
;;     GNU Emacs version 24.3           : yes
;;     GNU Emacs version 23.3           : yes
;;     GNU Emacs version 22.2           : yes
;;     GNU Emacs version 21.x and lower : unknown
;;
;;     No external dependencies
;;
;; See Also
;;
;;     Why you should not modify vectors
;;     http://emacswiki.org/emacs/VectorUsage
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

;; for assert, subseq
(require 'cl)

;;; customizable variables

;;;###autoload
(defgroup vector-utils nil
  "Vector-manipulation utility functions."
  :version "0.1.2"
  :link '(emacs-commentary-link :tag "Commentary" "vector-utils")
  :link '(url-link :tag "GitHub" "http://github.com/rolandwalker/vector-utils")
  :link '(url-link :tag "EmacsWiki" "http://emacswiki.org/emacs/VectorUtils")
  :prefix "vector-utils-"
  :group 'extensions)

;;;###autoload
(defun vector-utils-depth (vec)
  "Find the depth of vector VEC, which may contain other vectors.

If VEC is not a vector or is an empty vector, returns a depth
of 0.

If VEC is vector which does not contain other vectors, returns
a depth of 1."
  (cond
    ((or (not (vectorp vec))
         (= 0 (length vec)))
     0)
    (t
     (+ 1 (apply 'max (mapcar 'vector-utils-depth vec))))))

(defun vector-utils--flatten-1 (vec)
  "Driver for `vector-utils-flatten'.

VEC is as documented at `vector-utils-flatten'."
  (cond
    ((and (vectorp vec)
          (= 0 (length vec)))
     vec)
    ((vectorp vec)
     (vconcat (vector-utils--flatten-1 (aref vec 0))
              (vector-utils--flatten-1 (subseq vec 1 (length vec)))))
    (t
     (vector vec))))

;;;###autoload
(defun vector-utils-flatten (vec)
  "Flatten vector VEC, which may contain other vectors."
  (assert (vectorp vec) nil "VEC must be a vector")
  (vector-utils--flatten-1 vec))

;;;###autoload
(defun vector-utils-insert-before-pos (vec pos new-element)
  "Look in VEC for position POS, and insert NEW-ELEMENT before.

POS is zero-indexed.

A modified copy of VEC is returned."
  (assert (vectorp vec) nil "VEC must be a vector")
  (assert (and (integerp pos)
               (>= pos 0)
               (< pos (length vec))) nil "No such position %s" pos)
  (vconcat
   (subseq vec 0 pos)
   (vector new-element)
   (subseq vec pos (length vec))))

;;;###autoload
(defun vector-utils-insert-after-pos (vec pos new-element)
  "Look in VEC for position POS, and insert NEW-ELEMENT after.

POS is zero-indexed.

A modified copy of VEC is returned."
  (assert (vectorp vec) nil "VEC must be a vector")
  (assert (and (integerp pos)
               (>= pos 0)
               (< pos (length vec))) nil "No such position %s" pos)
  (vconcat
   (subseq vec 0 (1+ pos))
   (vector new-element)
   (subseq vec (1+ pos) (length vec))))

;;;###autoload
(defun vector-utils-insert-before (vec element new-element)
  "Look in VEC for ELEMENT and insert NEW-ELEMENT before it.

A modified copy of VEC is returned."
  (let ((pos (position element vec)))
    (assert pos nil "Element not found: %s" element)
    (vector-utils-insert-before-pos vec pos new-element)))

;;;###autoload
(defun vector-utils-insert-after (vec element new-element)
  "Look in VEC for ELEMENT and insert NEW-ELEMENT after it.

A modified copy of VEC is returned."
  (let ((pos (position element vec)))
    (assert pos nil "Element not found: %s" element)
    (vector-utils-insert-after-pos vec pos new-element)))

(provide 'vector-utils)

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
;; LocalWords: VectorUtils devel utils subseq
;;

;;; vector-utils.el ends here
