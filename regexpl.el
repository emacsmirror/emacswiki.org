;;; regexpl.el --- Search and replace list of patterns and replacements.

;; Copyright (C) 2007, 2008  Aaron S. Hawley

;; Author: Aaron S. Hawley
;; Keywords: lisp
;; Version: %Id: 4%
;; RCS Version: $Id: regexpl.el,v 1.5 2008/12/27 02:33:22 aaronh Exp $
;; URL: http://www.emacswiki.org/elisp/regexpl.el

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; The function `regexpl-search-replace-list' takes a cons-ed list of
;; patterns and replacements.  It find the nearest match (ties defer
;; to list order) then replaces with the corresponding match.

;; For example,

;; (regexpl-search-replace-list '(("stopwatch" . "timer")
;;                                ("watch" . "wristwatch")))

;; will replace occurrences of "stopwatch" first with "timer", but all
;; other occurrences of "watch" will be replaced with "wristwatch".

;;; History:

;; Written on October 31, 2007 in South Burlington, Vermont, USA.

;;; Code:

(defun regexpl-filter (condp lst)
  "Apply CONDP to elements of LST keeping those that return non-nil.

Example:
    (regexpl-filter 'symbolp '(a \"b\" 3 d4))
         => (a d4)

This procedure does not work when CONDP is the `null' function."
  (delq nil
        (mapcar (lambda (l) (and (funcall condp l) l)) lst)))

(defun regexpl-transpose-lists (lst)
  "Transpose 2-dimensional list LST.

Example:
    (regexpl-transpose-lists '((1 2 3) (one two three)))
         => ((1 one) (2 two) (3 three))"
  (if (null lst)
      nil
    (if (null (cdr lst))
        lst
      (cons (mapcar 'car lst)
            (regexpl-transpose-lists (delq nil (mapcar 'cdr
                                                       lst)))))))

(defun regexpl-combine-lists (&rest args)
  "Combine lists of ARGS by transposing the respective elements.

For example,

  (regexpl-combine-lists '(1 2 3) '(one two three))

evaluates to.
  
  ((1 one) (2 two) (3 three))

See `regexpl-transpose-lists'."
  (regexpl-transpose-lists args))

(defun regexpl-re-search-forward-list (regexp-list)
  "Search forward to first regular expression match in REGEXP-LIST.
Return index of matching regular expression in list, else nil."
  (let ((n (regexpl-re-closest-search-forward regexp-list)))
    (if (numberp n)
        (and (re-search-forward (nth n regexp-list) nil 'noerror)
             n)
      nil)))

(defun regexpl-car-minimum+non-nil (p1 p2)
  "Return P1 if `car' value is greater than or equal to P2's, else P2.
Value of `car' must be integer, or else other value is returned."
  (if (null (car p2))
      p1
    (if (null (car p1))
        p2
      (if (<= (car p1) (car p2))
          p1
        p2))))

(defun regexpl-re-closest-search-forward (regexp-list)
  "Search forward for first regular expression match in REGEXP-LIST.
Return index of matching regular expression in list, else nil."
  (cadr
   (reduce
    'regexpl-car-minimum+non-nil
    (regexpl-combine-lists
     (mapcar
      (lambda (re)
        (save-excursion
          (and (re-search-forward re nil 'noerror)
               (match-beginning 0))))
      regexp-list)
     (number-sequence 0 (1- (length regexp-list)))))))

(defun regexpl-nth-replacement (n regexp-replace-list)
  "The Nth replacement in REGEXP-REPLACE-LIST."
  (cdr (nth n regexp-replace-list)))

(defun regexpl-search-replace-list (regexp-replace-list)
  "Replace all search-replacements in REGEXP-REPLACE-LIST.
For example,

  (regexpl-search-replace-list '((\"stopwatch\" . \"timer\")
                                 (\"watch\" . \"wristwatch\")))

will replace occurrences of \"stopwatch\" first with \"timer\", but all
other occurrences of \"watch\" will be replaced with \"wristwatch\"."
  (interactive
   (let ((n 0)
         (tmplist '()))
     (while (let ((s (read-from-minibuffer
                      (format "Regexp (%d), or RET: " (1+ n)))))
              (and (not (string-equal "" s))
                   (let ((r (read-from-minibuffer
                             (format "Replace %s with: " s))))
                     (setq tmplist (append tmplist
                                           (list (cons s r)))))))
       (setq n (1+ n)))
     (list tmplist)))
  ;; end interactive
  (let ((count 0))
    (while (let ((n (regexpl-re-search-forward-list
                     (mapcar 'car regexp-replace-list))))
             (and (numberp n)
                  (if (or (> n (length regexp-replace-list))
                          (< n 0))
                      (error "Replacement %d not found" n)
                    (let ((r (regexpl-nth-replacement
                              n regexp-replace-list)))
                      (replace-match r)
                      (setq count (1+ count)))))))
    (when (called-interactively-p)
      (message "Made %d replacements" count))))

(defun regexpl-search-replace-list-in-string (regexp-replace-list str)
  "Replace all search-replacements in REGEXP-REPLACE-LIST in STR.

See `regexpl-search-replace-list'."
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (regexpl-search-replace-list regexp-replace-list)
    (buffer-string)))

(defun regexpl-test ()
  "Test `regexpl-search-replace-list'."
  ;(interactive)
  (if (and
       (string-equal "Bar foo syn"
                     (regexpl-search-replace-list-in-string
                      '(("foo" . "bar") ("ackbar" . "foo")
                        ("ack" . "syn"))
                      "Foo ackbar ack"))
       (string-equal "My wristwatch is a timer"
                     (regexpl-search-replace-list-in-string
                      '(("stopwatch" . "timer")
                        ("watch" . "wristwatch"))
                      "My watch is a stopwatch")))
      (message "Tests succeeded")
    (message "Tests failed")))

(provide 'regexpl)
;;; regexpl.el ends here
