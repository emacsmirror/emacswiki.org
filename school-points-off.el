;;; school-points-off.el --- Display grades based on number of problems on the test

;;; This is free software and in the public domain.
;;; Written by Terrence Brannon
;;; Version: 20041107

;;; DISCUSSION
;;; Often, in my 1-month career as a math teacher, I had to calculate a
;;; student's grade as a function of the number of problems missed. For
;;; a 20 or 10-problem test, this was easy. But sometimes by grade-making
;;; software would generate a 13-problem test and I would have to figure
;;; out how much each problem was worth. So, I wrote this to make it easier

;;; USAGE

;;  To use, put this in your .xemacs/init.el or .emacs:
;;  (autoload 'school-points-off "school-points-off"
;;     "calculate amount of points to remove per question missed" t)

;;  Then type M-x school-points-off and enter the number of problems
;;  on the test at the prompt.

(defun school-points-off (number-of-problems)
  (interactive "nTotal number of problems: ")
  (let ((points-per-problem (/ 100.0 number-of-problems)))
    (school-display-points-off number-of-problems points-per-problem)))

(defun school-display-points-off (number-of-problems points-per-problem)
  (let* ((bufname (format "*%d-problems-school-points-off*"
			  number-of-problems))
	 (buf (get-buffer-create bufname))
	 (problem-count 0)
	 (banner "------\t-----------\n")
	 )
    (switch-to-buffer buf)
    (erase-buffer)
    (lisp-interaction-mode)
    (insert (format 
	     "Grade Calculation for %d Problems\n" number-of-problems))

    (insert  "======================="     
	     (make-string (length (format "%d" points-per-problem)) ?=)
	     "=========\n")
    (insert  (format "Each problem is worth %.3f points\n" points-per-problem))
    (insert  (format "1/2 off => %.3f off\n" (/ points-per-problem 2.0)))
    (insert  (format "1/3 off => %.3f off\n" (/ points-per-problem 3.0)))
    (insert  (format "1/4 off => %.3f off\n" (/ points-per-problem 4.0)))
    (insert banner "missed\tgrade\n" banner)
    (while (<= problem-count number-of-problems)
      (insert
       (format "%d\t%.3f\n"
	       problem-count
	       (- 100 (* problem-count points-per-problem))))
      (setq problem-count (1+ problem-count)))
    ))


(provide 'school)
(provide 'school-points-off)
;;; school-points-off.el ends here
