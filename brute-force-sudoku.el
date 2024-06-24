;;;
;; Sudoku.el
;; Brute-force a Sudoku puzzle

(defvar sudoku-puzzle nil)

(defconst +sudoku-error+ "sudoku-error")

(defun sudoku-error (puzzle n)
  (or (not (listp puzzle))
      (not (= (length puzzle) 81))
      (not (numberp n))
      (< n 0)
      (> n 80)))

(defun sudoku-row (puzzle n)
  (if (sudoku-error puzzle n)
      +sudoku-error+
    (let* ((start (* (/ n 9) 9))
           (end (+ start 9)))
      (seq-subseq puzzle start end))))

(defun sudoku-col (puzzle n)
  (if (sudoku-error puzzle n)
      +sudoku-error+
    (let* ((start (mod n 9))
           (end (+ start 72))
           (out '()))
      (while (<= start end)
        (setq out (cons (nth start puzzle) out))
        (setq start (+ start 9)))
      out)))

(defconst +sudoku-blocks+
  '(
    0  0  0  3  3  3  6  6  6
    0  0  0  3  3  3  6  6  6
    0  0  0  3  3  3  6  6  6
    27 27 27 30 30 30 33 33 33
    27 27 27 30 30 30 33 33 33
    27 27 27 30 30 30 33 33 33
    54 54 54 57 57 57 60 60 60
    54 54 54 57 57 57 60 60 60
    54 54 54 57 57 57 60 60 60
    ))

(defun sudoku-block (puzzle n)
  (if (sudoku-error puzzle n)
      +sudoku-error+
    (let* ((start (nth n +sudoku-blocks+))
           (end (+ start 20))
           (out '()))
      (dotimes (i 3)
        (dotimes (j 3)
          (setq out (cons (nth (+ start i (* j 9)) puzzle) out))))
      out)))

(defun sudoku-allowed-values (puzzle n)
  (if (sudoku-error puzzle n)
      +sudoku-error+
    (let ((nval (car (nth n puzzle))))
      (seq-uniq
       (seq-remove
        (lambda (n) (<= n nval))
        (seq-difference '(1 2 3 4 5 6 7 8 9)
                        (sort
                         (mapcar
                          (lambda (x) (car x))
                          (append (sudoku-row puzzle n)
                                  (sudoku-col puzzle n)
                                  (sudoku-block puzzle n)))
                         '<)))))))

(defun sudoku-copy-puzzle (puzzle)
  (if (sudoku-error puzzle 0)
      +sudoku-error+
    (let ((new-puzzle '()))
      (dotimes (i 81)
        (let* ((n (nth i puzzle))
               (mutable (list (= n 0))))
          (setq new-puzzle (append new-puzzle (list (cons n mutable))))))
      new-puzzle)))

(defun sudoku-clean-puzzle (puzzle)
  (if (sudoku-error puzzle 0)
      +sudoku-error+
    (mapcar
     (lambda (x) (car x))
     puzzle)))

(defun sudoku-solve (puzzle)
  (if (sudoku-error puzzle 0)
      +sudoku-error+
    (sudoku-solve-actual puzzle)))

(defun sudoku-solve-actual (puzzle)
  (let ((p (sudoku-copy-puzzle puzzle))
        (n 0)
        (act 0)
        (max 1000000)
        (v 1))
    (while (and (>= n 0) (< n 81) (< act max))
      (setq act (1+ act))
      (let* ((cell (nth n p))
             (mutable (cadr cell))
             (values (sudoku-allowed-values p n)))
        (if mutable
            (if (> (length values) 0)
                (progn
                  (setf (car cell) (car values))
                  (setq v 1)
                  (setq n (+ n v)))
              (setf (car cell) 0)
              (setq v -1)
              (setq n (+ n v)))
          (setq n (+ n v)))))
    (if (>= n 81)
        (list :act act :puzzle (sudoku-clean-puzzle p))
      (list :act act :error +sudoku-error+))))

;;(insert (format "\n\n;;%s" (prin1 (sudoku-solve sudoku-puzzle))))

;; #100 in book
(setq sudoku-puzzle '(
                      0 8 9 0 0 0 3 4 0
                      7 0 0 0 0 0 0 0 2
                      0 0 0 0 3 0 0 0 0
                      0 0 0 4 0 8 0 0 0
                      0 0 6 0 0 0 2 0 0
                      1 4 0 0 0 0 0 7 6
                      0 0 0 0 1 0 0 0 0
                      2 0 0 7 0 6 0 0 1
                      0 5 0 0 0 0 0 3 0
                      ))

;;   $ time emacsclient -e '(sudoku-solve sudoku-puzzle)'
;;   (:act 972417 :puzzle
;;         (6 8 9 2 7 1 3 4 5
;;          7 1 3 5 8 4 9 6 2
;;          4 2 5 6 3 9 7 1 8
;;          5 7 2 4 6 8 1 9 3
;;          3 9 6 1 5 7 2 8 4
;;          1 4 8 9 2 3 5 7 6
;;          8 6 7 3 1 5 4 2 9
;;          2 3 4 7 9 6 8 5 1
;;          9 5 1 8 4 2 6 3 7))
;;
;;   real	0m44.170s
;;   user	0m0.000s
;;   sys	0m0.003s

;; #99 in book
(setq sudoku-puzzle '(
                      0 0 7 2 0 0 0 0 0
                      0 6 0 7 0 0 9 5 0
                      0 0 0 0 0 0 0 2 0
                      8 0 0 0 5 0 0 0 4
                      0 0 2 0 0 0 8 0 0
                      4 0 0 0 3 0 0 0 1
                      0 7 0 0 0 0 0 0 0
                      0 5 1 0 0 6 0 4 0
                      0 0 0 0 0 3 1 0 0
                      ))

;;   #99
;;   $ time emacsclient -e '(sudoku-solve sudoku-puzzle)'
;;   (:act 75249 :puzzle
;;         (5 4 7 2 9 8 3 1 6
;;          2 6 3 7 1 4 9 5 8
;;          1 8 9 3 6 5 4 2 7
;;          8 3 6 1 5 7 2 9 4
;;          7 1 2 6 4 9 8 3 5
;;          4 9 5 8 3 2 6 7 1
;;          9 7 8 4 2 1 5 6 3
;;          3 5 1 9 8 6 7 4 2
;;          6 2 4 5 7 3 1 8 9))
;;
;;   real	0m3.382s
;;   user	0m0.002s
;;   sys	0m0.001s

;; #98 in book
(setq sudoku-puzzle '(
                      0 9 0 0 7 5 0 0 0
                      7 0 0 0 0 0 6 2 0
                      0 0 0 0 0 0 0 3 0
                      0 0 0 0 0 8 0 0 0
                      1 0 0 0 0 0 0 0 3
                      2 0 0 3 0 9 0 0 4
                      0 5 0 0 0 0 0 9 0
                      0 4 6 0 0 0 2 0 0
                      0 0 0 0 2 4 0 0 5
                      ))

;;   #98
;;   $ time emacsclient -e '(sudoku-solve sudoku-puzzle)'
;;   (:act 255441 :puzzle
;;         (6 9 3 2 7 5 1 4 8
;;          7 8 5 4 3 1 6 2 9
;;          4 2 1 8 9 6 5 3 7
;;          5 3 4 7 6 8 9 1 2
;;          1 7 9 5 4 2 8 6 3
;;          2 6 8 3 1 9 7 5 4
;;          3 5 2 1 8 7 4 9 6
;;          8 4 6 9 5 3 2 7 1
;;          9 1 7 6 2 4 3 8 5))
;;
;;   real	0m11.457s
;;   user	0m0.001s
;;   sys	0m0.002s
