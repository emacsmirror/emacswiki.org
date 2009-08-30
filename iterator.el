;;; iterator.el --- A library to create and use elisp iterators objects.
 
;; Filename: iterator.el
;; Description: Create objects that iterate on themself. 
;; Author: Thierry Volpiatto
;; Maintainer: Thierry Volpiatto

;; Created: jeu fév  5 15:48:16 2009 (+0100)

;; X-URL: 

;; Keywords: 

;; Compatibility: 
 
 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:

(require 'cl)

(defmacro iter-list (list-obj)
  "Return an iterator from list `list-obj'."
  `(lexical-let ((lis ,list-obj))
     (lambda ()
       (let ((elm (car lis)))
         (setq lis (cdr lis))
         elm))))

(defmacro iter-apply-fun-on-list (fun list-obj)
  "Create an iterator that apply function `fun'
on each elm of `list-obj'."
  `(lexical-let ((lis ,list-obj)
                 (fn ,fun))
     (lambda ()
       (let ((elm (if (car lis)
                      (funcall fn (car lis)))))
         (setq lis (cdr lis))
         elm))))

(defun iter-next (iterator)
  "Return next elm of `iterator'.
create `iterator' with `tve-list-iterator'."
  (funcall iterator))

(defmacro sub-iter-next (seq elm)
  "Create iterator from position of `elm' to end of `seq'."
  `(lexical-let* ((pos (position ,elm ,seq))
                  (sub (subseq ,seq (1+ pos)))
                  (iterator (iter-list sub)))
     (lambda ()
       (iter-next iterator))))

(defmacro sub-iter-prec (seq elm)
  "Create iterator from position of `elm' to beginning of `seq'."
  `(lexical-let* ((pos (position ,elm ,seq))
                  (sub (reverse (subseq ,seq 0 pos)))
                  (iterator (iter-list sub)))
     (lambda ()
       (iter-next iterator))))

;; This version compatible with cl (just change lexical let)
(defmacro iter-scroll-list (seq size)
  "Create an iterator of the subseq of the cdr of seq ending to size."
  `(lexical-let* ((lis ,seq)
                  (end ,size))
     (lambda ()
       (let ((sub (subseq lis 0 end)))
         (setq lis (cdr lis))
         (if (< (length lis) end)
             (setq end (- end 1)))
         (remove nil sub)))))

(defmacro iter-scroll-up (seq elm size)
  `(lexical-let* ((pos (position (car (last ,elm)) ,seq))
                  (sub (reverse (subseq ,seq 0 pos)))
                  (iterator (iter-scroll-list sub ,size)))
     (lambda ()
       (reverse (iter-next iterator)))))

(defmacro iter-scroll-down (seq elm size)
  `(lexical-let* ((pos (position (car (last ,elm)) ,seq))
                  (sub (subseq ,seq pos))
                  (iterator (iter-scroll-list sub ,size)))
     (lambda ()
       (iter-next iterator))))
                  
(defsubst iter-map (fn iterator)
  (catch 'break
  (while t
    (let ((elm (funcall iterator)))
      (if elm
          (funcall fn elm)
          (throw 'break nil))))))

(defmacro iter-mapc (fn iterator)
  `(let ((init-iter ,iterator))
     (catch 'break 
       (while t 
         (let ((elm (funcall ,iterator))) 
           (if elm 
               (funcall ,fn elm) 
               (throw 'break nil)))))
     init-iter))

(defmacro iter-mapcar (fn iterator)
  `(let ((stock))
     (catch 'break 
       (while t 
         (let ((elm (funcall ,iterator))) 
           (if elm 
               (push (funcall ,fn elm) stock) 
               (throw 'break nil)))))
     (nreverse stock)))


(defun eshell-iterator (scom)
  (let ((output-list
         (split-string (with-temp-buffer
                         (insert (eshell-command-result scom))
                         (buffer-string))
                       "\n")))
    (iter-list output-list)))


;;; buffer and files processing.
(defun flines-iterator (file &optional startpos to-pos count)
  "Return an iterator that return lines of file one by one.
`startpos' and `bufsize' are the byte options to give to
`insert-file-contents'.
`startpos' ==> byte-offset.
`bufsize' ==> size of block in bits to process."
  (lexical-let ((fname file)
                (pos 1)
                (start-at (or startpos 1))
                (proc-size to-pos)
                (cnt (or count 0)))

    (lambda ()
      (with-temp-buffer
        (let ((output)
              (end-line (if proc-size
                            (+ start-at proc-size))))
          (insert-file-contents fname
                                nil
                                start-at
                                (if proc-size end-line))
          (goto-char (point-max))
          (delete-region (point-at-bol) (point-at-eol))
          (goto-char pos)
          (forward-line 1)
          (let ((cur-pt (point)))
            (if (not (eq pos cur-pt))
                (progn
                  (if cnt
                      (progn
                        (incf cnt)
                        (setq output (list (number-to-string cnt)
                                           (replace-regexp-in-string
                                            "\n"
                                            ""
                                            (buffer-substring-no-properties pos cur-pt)))))
                      (setq output (replace-regexp-in-string
                                    "\n"
                                    ""
                                    (buffer-substring-no-properties pos cur-pt))))
                  (setq pos cur-pt)
                  output))))))))


;;; Provide
(provide 'iterator)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; iterator.el ends here
