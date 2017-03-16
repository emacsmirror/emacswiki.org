;;; 1040tt.el --- U.S. tax calculator for form 1040 filers

;; Copyright (C) 2009, 2012, 2017  Aaron S. Hawley

;; Author: Aaron S. Hawley
;; Keywords: games, financial
;; Version: %Id: 2%
;; URL: http://www.emacswiki.org/elisp/1040tt.el
;; EmacsWiki: FormTenForty

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
;; Boston, MA 02110-1301, USA.  See <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Calculate the income tax in the year 2017 for filing one's 2016
;; taxes with form 1040 from the U.S. Internal Revenue Service.
;; Provide your taxable income amount and "filing status" to generate
;; the tax table entry if it exists, else compute the exact tax.
;; These commands will not determine your exemptions, adjustments,
;; credits to find you taxable income.  Follow the instructions with
;; Form 1040 to calculate your taxable income.

;; Based on tax table and worksheet in ''Instructions for Form 1040,
;; U.S.  Individual Income Tax Return'' (i1040tt), 15 December, 2016.
;; Internal Revenue Service.  http://www.irs.gov/

;;; Usage:

;; M-x 1040tt-income-tax

;;   Calculate your income tax by asking you your taxable income and
;;   your filing status.  Your filing status can be either
;;   "filing-jointly", "filing-separately", "head-of-household", or
;;   "single".

;; Evaluate as Emacs Lisp with

;; (1040tt-income-tax 22000 'single)
;;   ==> 2840

;; M-x 1040tt-income-tax-estimate

;;   Estimate your income tax as above but calculate the value
;;   based on the schedule and not the tax tables.

;; Evaluate as Emacs Lisp with

;; (1040tt-income-tax-estimate 22000 'single)
;;   ==> 2836.25

;;; History:

;; Written on 22 February, 2009 by Aaron S. Hawley in Burlington, Vermont.

;;; Code:

(defvar 1040tt-tax-brackets
  '((single
     ;; Schedule X from 1040 tax table instructions for 2017
     (415050 . 0.396)
     (413350 . 0.35)
     (190150 . 0.33)
     ( 91150 . 0.28)
     ( 37650 . 0.25)
     (  9275 . 0.15)
     (     0 . 0.1))
    (filing-jointly
     ;; Schedule Y-1 from 1040 tax table instructions for 2017
     (466950 . 0.396)
     (413350 . 0.35)
     (231450 . 0.33)
     (151900 . 0.28)
     ( 75300 . 0.25)
     ( 18550 . 0.15)
     (     0 . 0.1))
    (filing-separately
     ;; Schedule Y-2 from 1040 tax table instructions for 2017
     (233475 . 0.396)
     (206675 . 0.35)
     (115725 . 0.33)
     ( 75950 . 0.28)
     ( 37650 . 0.25)
     (  9275 . 0.15)
     (     0 . 0.1))
    (head-of-household
     ;; Schedule Z from 1040 tax table instructions for 2017
     (441000 . 0.396)
     (413350 . 0.35)
     (210800 . 0.33)
     (130150 . 0.28)
     ( 50400 . 0.25)
     ( 13250 . 0.15)
     (     0 . 0.1)))
  "Tax schedule for 2017 form 1040.")

(defvar 1040tt-use-dynamic-programming nil
  "If non-nil compute with dynamic programming heuristic.
Dynamic programming avoids computing values that have already
been determined by storing the associations.")

(defvar 1040tt-tax-hash nil
  "Hash table used for dynamic programming algorithm.
See `1040tt-use-dynamic-programming'.")

(defconst 1040tt-tax-prompt "Taxable income: "
  "Prompt string for taxable income.")

(defun 1040tt-income-tax (income status)
  "Compute tax for INCOME when filing as STATUS.

If INCOME is in tax table, return tax for its entry.
Otherwise, compute value using `1040tt-income-tax-estimate'."
  (interactive
   (let ((i (read-number 1040tt-tax-prompt))
         (s (1040tt-read-filing-status)))
     (list i s)))
  (let* ((line (1040tt-tax-line-for income))
         (tax (if (null line)
                  (1040tt-income-tax-estimate income status)
                (1040tt-round
                 (1040tt-income-tax-estimate line status)))))
    (message (format "You owe $%.2f in income tax" tax))
    tax))

(defun 1040tt-read-filing-status ()
  "Read filing status from minibuffer.

A status must be caar value of `1040tt-tax-brackets'."
  (intern
   (completing-read "Filing status: "
                    (mapcar
                     'symbol-name
                     (mapcar 'car
                             1040tt-tax-brackets))
                    nil 'require-match)))

(defun 1040tt-tax-line-for (income)
  "Determine line in tax table for INCOME.
If entry doesn't exist for INCOME, then return INCOME."
  (let ((range (1040tt-tax-range income))
        (offset (cond ((< income  5.0) 0)
                      ((< income 25.0) 5)
                      (t 0))))
    (if (null range)
        nil
      (+ offset (/ range 2.0)
         (* (/ (truncate income) (+ offset range))
            range)))))

(defun 1040tt-tax-range (income)
  "Return span of row in tax table for INCOME."
  (cond ((< income     5.0) 5)
        ((< income    25.0) 10)
        ((< income  3000.0) 25)
        ((< income 100000.0) 50)
        (t nil)))

(defun 1040tt-round (n)
  "Round to nearest integer of N.
If equidistant between integers, round up.

For example, (1040tt-round 2.5) returns 3, never 2.

Emacs's `round' is system-dependent."
  (+ (truncate n) ;; Avoid Emacs's biased rounding to evens.
     (1- (round (1+ (mod n 1))))))

(defun 1040tt-income-tax-estimate (income status)
  "Compute tax for INCOME when filing as STATUS without tax tables.

For finding the tax with tax tables where necessary, use
the `1040tt-income-tax' command."
  (interactive
   (let ((i (read-number 1040tt-tax-prompt))
         (s (1040tt-read-filing-status)))
     (list i s)))
  (let ((brackets (assoc status 1040tt-tax-brackets)))
    (if (null brackets)
        (error "Unknown filing status %s" (symbol-name status))
      (let ((tax (1040tt-income-tax-estimate- income status (cdr brackets))))
        (message (format "Your estimated income tax is $%.2f" tax))
        tax))))

(defun 1040tt-income-tax-estimate- (income status brackets)
  "Calculate tax for INCOME filing as STATUS using BRACKETS.

Income tax is calculated by adding together the dollars taxed in
each tax bracket.  It is not done by multiply a taxable income by
the rate for its bracket."
  (if (or (< income 0) (null brackets))
      0
    (when 1040tt-use-dynamic-programming
      (if (null 1040tt-tax-hash)
          (setq 1040tt-tax-hash (make-hash-table)))
      (if (null (gethash status 1040tt-tax-hash))
          (puthash status (make-hash-table) 1040tt-tax-hash)))
    (let* ((income-level (caar brackets))
           (tax-rate (cdar brackets))
           (tax
            (if (and 1040tt-use-dynamic-programming
                     (numberp (gethash income
                                       (gethash status
                                                1040tt-tax-hash))))
                (gethash income (gethash status 1040tt-tax-hash))
              (if (> income income-level)
                  (+ (* (- income income-level) tax-rate)
                     (1040tt-income-tax-estimate- income-level status
                                         (cdr brackets)))
                (1040tt-income-tax-estimate- income status
                                    (cdr brackets))))))
      (if (not 1040tt-use-dynamic-programming)
          tax
        (puthash income tax (gethash status 1040tt-tax-hash))))))

(defun 1040tt-tax-worksheet (income status)
  "Compute tax with 1040tt worksheet for INCOME when filing as STATUS."
  (let* ((rate (or (1040tt-tax-rate income status)
                   0))
         (income-level (or (1040tt-tax-level income status)
                           0))
         (tax (- (* income rate)
                 (1040tt-income-tax-estimate income status))))
    ;; (message (format "You owe $%.2f in income tax" tax))
    tax))

(defun 1040tt-tax-reduction (income status)
  "Compute reduction in worksheet for INCOME when filing as STATUS.

See `1040tt-tax-worksheet'."
  (let* ((rate (1040tt-tax-rate income status))
         (tax (1040tt-income-tax-estimate income status)))
    (if (null rate)
        0
      (let ((reduction (- (* rate income)
                          tax)))
        ;; (message (format "Your worksheet reduction is $%.2f"
        ;;          reduction))
        reduction))))

(defun 1040tt-tax-level (income status)
  "Display income level for INCOME when filing as STATUS."
  (interactive
   (let ((i (read-number 1040tt-tax-prompt))
         (s (1040tt-read-filing-status)))
     (list i s)))
  (let ((brackets (assoc status 1040tt-tax-brackets)))
    (if (null brackets)
        (error "Unknown filing status %s" (symbol-name status))
      (let* ((levels (progn (delq nil
                                  (mapcar (lambda (b)
                                            (and (> income (car b)) (car b)))
                                          (cdr brackets)))))
             (level (if (> (length levels) 0)
                        (apply 'max levels)
                      levels)))
        (if (null level)
            (message "No income tax level found")
          (message "Your income tax level is $%d" level))
        level))))

(defun 1040tt-tax-rate (income status)
  "Display tax rate for INCOME when filing as STATUS."
  (interactive
   (let ((i (read-number 1040tt-tax-prompt))
         (s (1040tt-read-filing-status)))
     (list i s)))
  (let ((brackets (assoc status 1040tt-tax-brackets)))
    (if (null brackets)
        (error "Unknown filing status %s" (symbol-name status))
      (let* ((level (1040tt-tax-level income status))
             (rate (cdr (assoc level brackets))))
        (if (null rate)
            (message "No income tax rate found")
          (message "Your income tax rate is %d%% (%.2f%%)" (* 100.0 rate)
                   ;; Display average tax rate parenthetically.
                   (* 100
                      (/ (1040tt-income-tax-estimate income status)
                         income))))
        rate))))

(defun 1040tt-insert-tax-table ()
  "Insert Form 1040tt tax table."
  (interactive "*")
  (let ((rows (vconcat  [0 5 15]
                        (number-sequence 25 2975 25)
                        (number-sequence 3000 99950 50)))
        (statuses (mapcar 'car 1040tt-tax-brackets))
        (1040tt-use-dynamic-programming t))
    (mapc
     (lambda (r)
       (insert (number-to-string r)
               "\t"
               (number-to-string (+ r (or (1040tt-tax-range r)
                                          0)))
               "\t")
       (insert
        (mapconcat 'number-to-string
                   (mapcar (lambda (s)
                             (1040tt-income-tax r s))
                           statuses)
                   "\t"))
       (newline))
     rows)))

(provide '1040tt)
;;; 1040tt.el ends here
