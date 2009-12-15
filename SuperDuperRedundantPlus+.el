;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*- mode: EMACS-LISP; -*-
;; This is SuperDuperRedundantPlus+.el
;; ================================================================
;; DESCRIPTION:
;; If SuperDuperRedundantPlus+® provided something it would be described here. 
;; You don't need SuperDuperRedundantPlus+®, if you did you could get it here:
;; (URL `http://www.emacswiki.org/emacs/SuperDuperRedundantPlus+.el')
;; ================================================================
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; ================================================================
;; Permission is granted to copy, distribute and/or modify this
;; document under the terms of the GNU Free Documentation License,
;; Version 1.3 or any later version published by the Free Software
;; Foundation; with no Invariant Sections, no Front-Cover Texts,
;; and no Back-Cover Texts. A copy of the license is included in
;; the section entitled "GNU Free Documentation License".
;; A copy of the license is also available from the Free Software
;; Foundation Web site at:
;; (URL `http://www.gnu.org/licenses/fdl-1.3.txt').
;; ================================================================
;; CODE:

(eval-when-compile (require 'cl))


(defun insert-SuperDuperRedundantPlus+-template ()
  "Insert a `SuperDuperRedundantPlus+' EmacsWiki template at point.
Does not move point."
  (interactive)
  (save-excursion
    (princ
     (concat
      "\n== The SuperDuperRedundantPlus+® Does Not Do XXXX ==\n\n"
      "SuperDuperRedundantPlus+® does not do {Description here}\n\n"
      "Don't use SuperDuperRedundantPlus+® to do `XXXX'\n\n"
      "Instead, just do: `code-snippet-here'\n\n"
      "See, you don't need SuperDuperRedundantPlus+®.\n")
     (current-buffer))))

;;; (insert-SuperDuperRedundantPlus+-template)

;;; ==============================
(provide 'SuperDuperRedundantPlus+)
;;; ==============================

;;; ================================================================
;;; SuperDuperRedundantPlus+.el ends here
;;; EOF
