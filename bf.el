;;; bf.el --- BrainFuck compiler written in Emacs-Lisp

;; Copyright (C) 2002  Free Software Foundation, Inc.

;; Author: Mario Lang <mlang@delysid.org>
;; Keywords: lisp, matching, games, languages

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This implements a BrainFuck interpreter and a compiler written in
;; Emacs-Lisp.  To demonstrate that the implementation is really working,
;; a weird test-case is included.  A BrainFuck interpreter
;; written in BrainFuck compiled to emacs-lisp executing brainfuck code.

;;; Code:

(defun matching-bracket (string start)
  "Utility function used to find the matching ] for a [."
  (let ((open 1) (pos (1+ start)) (l (length string)))
    (while (and (> open 0) (< pos l))
      (cond ((eq (aref string pos) ?\[) (incf open))
            ((equal (aref string pos) ?\]) (decf open)))
      (incf pos))
    (when (eq (aref string (1- pos)) ?\])
      (1- pos))))

(defun bf-slow (string)
  "The BrainFuck interpreter.
This function takes BrainFuck code in the STRING argument and interprets
and executes it at run-time.  It is much slower than the compiled version."
  (let ((mem (make-vector 30000 0))
        (p 0) (ic 0))
    (bf-execute string)
    (message "%S instructions executed" ic)))

(defun bf-execute (string)
  "BrainFuck interpreter.
This is a internal function which assumes the necessary variables for
a BrainFuck machine are already bound and initialized."
  (let ((i 0) (l (length string)))
    (while (< i l)
      (let ((c (aref string i)))
        (cond
         ((= c ?+) (incf (aref mem p)))
         ((= c ?-) (decf (aref mem p)))
         ((= c ?>) (incf p))
         ((= c ?<) (decf p))
         ((= c ?.) (insert (aref mem p)))
         ((= c ?,) (progn (let ((char (char-after (point))))
                            (if (null char)
                                (setq char 0))
                            (setf (aref mem p) char)
                            (ignore-errors
                              (forward-char 1))
                            (sit-for 0))))
         ((= c ?\[) (let ((end (matching-bracket string i)))
                      (while (not (eq (aref mem p) 0))
                        (bf-execute (substring string (1+ i) end)))
                      (setq i end))))
        (incf i) (incf ic)))))

;;; The compiler

(defun bf-compile-1 (string)
  "Internal function for the compiler.
It creates the list representing the code in STRING.
The generated code will use the variables MEM and P,
which must be available before the generated code will
run. Use `bf-compile' to compile the generated code."
  (let ((i 0)
	(l (length string))
	result)
    (while (< i l)
      (let ((c (aref string i)))
        (cond
         ((= c ?+)
          (setq result (cons
                        '(incf (aref mem p))
                        result)))
         ((= c ?-)
          (setq result (cons
                        '(decf (aref mem p))
                        result)))
         ((= c ?>)
          (setq result (cons
                        '(incf p)
                        result)))
         ((= c ?<)
          (setq result (cons
                        '(decf p)
                        result)))
         ((= c ?.)
          (setq result (cons
                        '(insert (aref mem p))
                        result)))
         ((= c ?,)
          (setq result (cons '(progn
                                (setf (aref mem p) (or (char-after (point)) 0))
                                (ignore-errors
                                  (forward-char 1))
                                (sit-for 0)) result)))
         ((= c ?\[)
          (let ((end (matching-bracket string i)))
            (setq result (cons
            `(while (not (eq (aref mem p) 0))
               ,@(bf-compile-1 (substring string (1+ i) end))) result))
            (setq i end))))
        (incf i)))
    (reverse result)))

(defun bf-compile (string)
  "Compile the BrainFuck code in STRING.
This returns a byte-compiled function with one optional argument,
DUMP. To run the code, simply `funcall' the return value of this
function. If you provide the optional argument, it will dump core
when it is done using `bf-core'."
  (message "Compiling bf-code...")
  (prog1 (byte-compile
	  `(lambda (&optional dump)
	     (let ((mem (make-vector 30000 0))
		   (p 0))
	       ,@(bf-compile-1 string)
	       (when dump
		 (bf-core mem p)))))
    (message "Compiling bf-code...done")))

(defun bf-run (string)
  "Compiles and runs BrainFuck code.
Call interactively or provide the code in STRING."
  (interactive "sBrainFuck code: ")
  (funcall (bf-compile string)))

(defun bf-core (vector pos)
  "Print core in VECTOR."
  (let ((i 0) (m (length vector)) result (zeroes 0))
    (while (< i m)
      (if (= (aref vector i) 0)
	  (setq zeroes (1+ zeroes))
	(while (> zeroes 0)
	  (insert "0 ")
	  (setq zeroes (1- zeroes)))
	(insert (number-to-string (aref vector i)) " "))
      (setq i (1+ i))))
  (insert "[" (number-to-string pos) "]"))
      
(defun bf-dump (string)
  "Compiles and runs BrainFuck code, then dumps core."
  (funcall (bf-compile string) 1))

(defun bf-test ()
  "A test for the BrainFuck compiler.
This function compiles a Brainfuck interpreter written in BrainFuck
to native emacs-lisp byte-code and execute the interpreter which itself
executes a simple brainfuck program.
NOTE: The BrainFuck interpreter written in BrainFuck is not written
by me."
  (interactive)
  (set-buffer (get-buffer-create "*bf-io*"))
  (switch-to-buffer (current-buffer))
  (erase-buffer)
  (insert "++++++++++++++++++++++++++++++++[>+>+<<-]>>+++++++++++++++++++++++++<<++++++++++[>>.-<.<-]!")
  (goto-char (point-min))
  (sit-for 0)
  (bf-run "
>>>,[->+>+<<]>>[-<<+>>]>++++[<++++++++>-]<+<[->>+>>+<<<<]>>>>[-<<<<+>>
>>]<<<[->>+>+<<<]>>>[-<<<+>>>]<<[>[->+<]<[-]]>[-]>[[-]<<<<->-<[->>+>>+
<<<<]>>>>[-<<<<+>>>>]<<<[->>+>+<<<]>>>[-<<<+>>>]<<[>[->+<]<[-]]>[-]>]<
<<<[->>+<<]>[->+<]>[[-]<<<[->+>+<<]>>[-<<+>>]>++++++[<+++++++>-]<+<[->
>>+>+<<<<]>>>>[-<<<<+>>>>]<<<[->+>>+<<<]>>>[-<<<+>>>]<[<[->>+<<]>[-]]<
[-]>>[[-]<<<<->-<[->>>+>+<<<<]>>>>[-<<<<+>>>>]<<<[->+>>+<<<]>>>[-<<<+>
>>]<[<[->>+<<]>[-]]<[-]>>]<<<<[->>>+<<<]>[->>+<<]>+>[<->[-]]<[<<<<+>>>
>[-]]<<<[->+>+<<]>>[-<<+>>]>+++++[<+++++++++>-]<<[->>>+>+<<<<]>>>>[-<<
<<+>>>>]<<<[->+>>+<<<]>>>[-<<<+>>>]<[<[->>+<<]>[-]]<[-]>>[[-]<<<<->-<[
->>>+>+<<<<]>>>>[-<<<<+>>>>]<<<[->+>>+<<<]>>>[-<<<+>>>]<[<[->>+<<]>[-]
]<[-]>>]<<<<[->>>+<<<]>[->>+<<]>+>[<->[-]]<[<<<<++>>>>[-]]<<<[->+>+<<]
>>[-<<+>>]>++++++[<++++++++++>-]<<[->>>+>+<<<<]>>>>[-<<<<+>>>>]<<<[->+
>>+<<<]>>>[-<<<+>>>]<[<[->>+<<]>[-]]<[-]>>[[-]<<<<->-<[->>>+>+<<<<]>>>
>[-<<<<+>>>>]<<<[->+>>+<<<]>>>[-<<<+>>>]<[<[->>+<<]>[-]]<[-]>>]<<<<[->
>>+<<<]>[->>+<<]>+>[<->[-]]<[<<<<+++>>>>[-]]<<<[->+>+<<]>>[-<<+>>]>+++
+++[<++++++++++>-]<++<[->>>+>+<<<<]>>>>[-<<<<+>>>>]<<<[->+>>+<<<]>>>[-
<<<+>>>]<[<[->>+<<]>[-]]<[-]>>[[-]<<<<->-<[->>>+>+<<<<]>>>>[-<<<<+>>>>
]<<<[->+>>+<<<]>>>[-<<<+>>>]<[<[->>+<<]>[-]]<[-]>>]<<<<[->>>+<<<]>[->>
+<<]>+>[<->[-]]<[<<<<++++>>>>[-]]<<<[->+>+<<]>>[-<<+>>]>+++++[<+++++++
++>-]<+<[->>>+>+<<<<]>>>>[-<<<<+>>>>]<<<[->+>>+<<<]>>>[-<<<+>>>]<[<[->
>+<<]>[-]]<[-]>>[[-]<<<<->-<[->>>+>+<<<<]>>>>[-<<<<+>>>>]<<<[->+>>+<<<
]>>>[-<<<+>>>]<[<[->>+<<]>[-]]<[-]>>]<<<<[->>>+<<<]>[->>+<<]>+>[<->[-]
]<[<<<<+++++>>>>[-]]<<<[->+>+<<]>>[-<<+>>]>++++[<+++++++++++>-]<<[->>>
+>+<<<<]>>>>[-<<<<+>>>>]<<<[->+>>+<<<]>>>[-<<<+>>>]<[<[->>+<<]>[-]]<[-
]>>[[-]<<<<->-<[->>>+>+<<<<]>>>>[-<<<<+>>>>]<<<[->+>>+<<<]>>>[-<<<+>>>
]<[<[->>+<<]>[-]]<[-]>>]<<<<[->>>+<<<]>[->>+<<]>+>[<->[-]]<[<<<<++++++
>>>>[-]]<<<[->+>+<<]>>[-<<+>>]>+++++++[<+++++++++++++>-]<<[->>>+>+<<<<
]>>>>[-<<<<+>>>>]<<<[->+>>+<<<]>>>[-<<<+>>>]<[<[->>+<<]>[-]]<[-]>>[[-]
<<<<->-<[->>>+>+<<<<]>>>>[-<<<<+>>>>]<<<[->+>>+<<<]>>>[-<<<+>>>]<[<[->
>+<<]>[-]]<[-]>>]<<<<[->>>+<<<]>[->>+<<]>+>[<->[-]]<[<<<<+++++++>>>>[-
]]<<<[->+>+<<]>>[-<<+>>]>+++++++[<+++++++++++++>-]<++<[->>>+>+<<<<]>>>
>[-<<<<+>>>>]<<<[->+>>+<<<]>>>[-<<<+>>>]<[<[->>+<<]>[-]]<[-]>>[[-]<<<<
->-<[->>>+>+<<<<]>>>>[-<<<<+>>>>]<<<[->+>>+<<<]>>>[-<<<+>>>]<[<[->>+<<
]>[-]]<[-]>>]<<<<[->>>+<<<]>[->>+<<]>+>[<->[-]]<[<<<<++++++++>>>>[-]]<
<<<[->>+>+<<<]>>>[-<<<+>>>]<[<<<[->>>>>>>>>+<+<<<<<<<<]>>>>>>>>[-<<<<<
<<<+>>>>>>>>]<<<<<<<[->>>>>>>>>+<<+<<<<<<<]>>>>>>>[-<<<<<<<+>>>>>>>]>[
<[->>>>>+<<<<<]>[->>>>>+<<<<<]>[->>>>>+<<<<<]>>>+>-]>>[-]<[->+<]<<[[-<
<<<<+>>>>>]<<<<<-]<<<<<<<<+>[-]>>[-]]<,[->+>+<<]>>[-<<+>>]>++++[<+++++
+++>-]<+<[->>+>>+<<<<]>>>>[-<<<<+>>>>]<<<[->>+>+<<<]>>>[-<<<+>>>]<<[>[
->+<]<[-]]>[-]>[[-]<<<<->-<[->>+>>+<<<<]>>>>[-<<<<+>>>>]<<<[->>+>+<<<]
>>>[-<<<+>>>]<<[>[->+<]<[-]]>[-]>]<<<<[->>+<<]>[->+<]>]<<<<<[-][->>>>>
>>>>+<<<<<<+<<<]>>>[-<<<+>>>]>>>>>>[<[->>>>>+<<<<<]>[->>>>>+<<<<<]>>>>
+>-]>>[[-<+<+>>]<<[->>+<<]>[-<+>[<->[-]]]<[[-]<[->+>+<<]>>[-<<+>>]<<[[
-<<<<<+>>>>>]>[-<<<<<+>>>>>]<<<<<<-]<<<<<<<<[-]>>>>>>>>>[-<<<<<<<<<+>>
>>>>>>>]<<<<<<<<<<[->>>>>>>>>>+<+<<<<<<<<<]>>>>>>>>>[-<<<<<<<<<+>>>>>>
>>>]>[<[->>>>>+<<<<<]>[->>>>>+<<<<<]>>>>+>-]>>>+<<<<[[-<<<<<+>>>>>]<<<
<<-]<<<<<<<<+[->>>>>>>>>+<<<<<<+<<<]>>>[-<<<+>>>]>>>>>>[<[->>>>>+<<<<<
]>[->>>>>+<<<<<]>>>>+>-][-]]>>[-<+<+>>]<<[->>+<<]>[-[-<+>[<->[-]]]]<[[
-]<[->+>+<<]>>[-<<+>>]<<[[-<<<<<+>>>>>]>[-<<<<<+>>>>>]<<<<<<-]<<<<<<<<
[-]>>>>>>>>>[-<<<<<<<<<+>>>>>>>>>]<<<<<<<<<<[->>>>>>>>>>+<+<<<<<<<<<]>
>>>>>>>>[-<<<<<<<<<+>>>>>>>>>]>[<[->>>>>+<<<<<]>[->>>>>+<<<<<]>>>>+>-]
>>>-<<<<[[-<<<<<+>>>>>]<<<<<-]<<<<<<<<+[->>>>>>>>>+<<<<<<+<<<]>>>[-<<<
+>>>]>>>>>>[<[->>>>>+<<<<<]>[->>>>>+<<<<<]>>>>+>-][-]]>>[-<+<+>>]<<[->
>+<<]>[-[-[-<+>[<->[-]]]]]<[[-]<[->+>+<<]>>[-<<+>>]<<[[-<<<<<+>>>>>]>[
-<<<<<+>>>>>]<<<<<<-]<<<<<<<<[-]>>>>>>>>>[-<<<<<<<<<+>>>>>>>>>]<<<<<<<
<<<->+[->>>>>>>>>+<<<<<<+<<<]>>>[-<<<+>>>]>>>>>>[<[->>>>>+<<<<<]>[->>>
>>+<<<<<]>>>>+>-][-]]>>[-<+<+>>]<<[->>+<<]>[-[-[-[-<+>[<->[-]]]]]]<[[-
]<[->+>+<<]>>[-<<+>>]<<[[-<<<<<+>>>>>]>[-<<<<<+>>>>>]<<<<<<-]<<<<<<<<[
-]>>>>>>>>>[-<<<<<<<<<+>>>>>>>>>]<<<<<<<<<<+>+[->>>>>>>>>+<<<<<<+<<<]>
>>[-<<<+>>>]>>>>>>[<[->>>>>+<<<<<]>[->>>>>+<<<<<]>>>>+>-][-]]>>[-<+<+>
>]<<[->>+<<]>[-[-[-[-[-<+>[<->[-]]]]]]]<[[-]<[->+>+<<]>>[-<<+>>]<<[[-<
<<<<+>>>>>]>[-<<<<<+>>>>>]<<<<<<-]<<<<<<<<[-]>>>>>>>>>[-<<<<<<<<<+>>>>
>>>>>]<<<<<<<<<<[->>>>>>>>>>+<+<<<<<<<<<]>>>>>>>>>[-<<<<<<<<<+>>>>>>>>
>]>[<[->>>>>+<<<<<]>[->>>>>+<<<<<]>>>>+>-]>>>.<<<<[[-<<<<<+>>>>>]<<<<<
-]<<<<<<<<+[->>>>>>>>>+<<<<<<+<<<]>>>[-<<<+>>>]>>>>>>[<[->>>>>+<<<<<]>
[->>>>>+<<<<<]>>>>+>-][-]]>>[-<+<+>>]<<[->>+<<]>[-[-[-[-[-[-<+>[<->[-]
]]]]]]]<[[-]<[->+>+<<]>>[-<<+>>]<<[[-<<<<<+>>>>>]>[-<<<<<+>>>>>]<<<<<<
-]<<<<<<<<[-]>>>>>>>>>[-<<<<<<<<<+>>>>>>>>>]<<<<<<<<<<[->>>>>>>>>>+<+<
<<<<<<<<]>>>>>>>>>[-<<<<<<<<<+>>>>>>>>>]>[<[->>>>>+<<<<<]>[->>>>>+<<<<
<]>>>>+>-]>>>,<<<<[[-<<<<<+>>>>>]<<<<<-]<<<<<<<<+[->>>>>>>>>+<<<<<<+<<
<]>>>[-<<<+>>>]>>>>>>[<[->>>>>+<<<<<]>[->>>>>+<<<<<]>>>>+>-][-]]>>[-<+
<+>>]<<[->>+<<]>[-[-[-[-[-[-[-<+>[<->[-]]]]]]]]]<[[-]<[->+>+<<]>>[-<<+
>>]<<[[-<<<<<+>>>>>]>[-<<<<<+>>>>>]<<<<<<-]<<<<<<<<[-]>>>>>>>>>[-<<<<<
<<<<+>>>>>>>>>]<<<<<<<<<<[->>>>>>>>>>+<+<<<<<<<<<]>>>>>>>>>[-<<<<<<<<<
+>>>>>>>>>]>[<[->>>>>+<<<<<]>[->>>>>+<<<<<]>>>>+>-]>>>[-<<<+>+>>]<<[->
>+<<]<<[[-<<<<<+>>>>>]>[-<<<<<+>>>>>]<<<<<<-]>[-<<<<<<+>>>>>>]>+<<<<<<
<[>>>>>>>-<<<<<<<[-]]<<<[->>>>>>>>>+<<<<<<+<<<]>>>[-<<<+>>>]>>>>>>[<[-
>>>>>+<<<<<]>[->>>>>+<<<<<]>[->>>>>+<<<<<]>>>+>-]>[[-]<+[<[->>>>>+<<<<
<]>[->>>>>+<<<<<]>>>>+>>>[->>+<<<+>]<[->+<]>>>[-[-[-[-[-[-[-<<<+>>>[<<
<->>>[-]]]]]]]]]<<<[<+>[-]]>[->>+<<<+>]<[->+<]>>>[-[-[-[-[-[-[-[-<<<+>
>>[<<<->>>[-]]]]]]]]]]<<<[<->[-]]<]>[-]]<<[->>>>>+<<<<<]>>>>>+>[-]]>>[
-<+<+>>]<<[->>+<<]>[-[-[-[-[-[-[-[-<+>[<->[-]]]]]]]]]]<[[-][-]<[->+>+<
<]>>[-<<+>>]<<[[-<<<<<+>>>>>]>[-<<<<<+>>>>>]<<<<<<-]<<<<<<<<[-]>>>>>>>
>>[-<<<<<<<<<+>>>>>>>>>]<<<<<<<<<<[->>>>>>>>>>+<+<<<<<<<<<]>>>>>>>>>[-
<<<<<<<<<+>>>>>>>>>]>[<[->>>>>+<<<<<]>[->>>>>+<<<<<]>>>>+>-]>>>[-<<<+>
+>>]<<[->>+<<]<<[[-<<<<<+>>>>>]>[-<<<<<+>>>>>]<<<<<<-]>[-<<<<<<+>>>>>>
]<<<<<<[->>>>>>>+<<<<<<<]<<<[->>>>>>>>>+<<<<<<+<<<]>>>[-<<<+>>>]>>>>>>
[<[->>>>>+<<<<<]>[->>>>>+<<<<<]>[->>>>>+<<<<<]>>>+>-]>[[-]<+[<[-<<<<<+
>>>>>]>[-<<<<<+>>>>>]<<<<<<->>>[->>+<<<+>]<[->+<]>>>[-[-[-[-[-[-[-<<<+
>>>[<<<->>>[-]]]]]]]]]<<<[<->[-]]>[->>+<<<+>]<[->+<]>>>[-[-[-[-[-[-[-[
-<<<+>>>[<<<->>>[-]]]]]]]]]]<<<[<+>[-]]<]>[-]]<<[->>>>>+<<<<<]>>>>>+>[
-]]>>]
"))

(provide 'bf)
;;; bf.el ends here
