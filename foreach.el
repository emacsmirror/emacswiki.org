;;; foreach.el --- simple list iteration macros.

;; Copyright (C) 2003 Free Software Foundation, Inc.

;; Author: Marvin Taylor <mltaylor at pobox dot com>

;; This file is free software -- for use with GNU Emacs.

;;; Commentary:

;; 'foreach' is a macro for simple processing of items in a list,
;; similar to the TCL and Perl command/keyword of the same name.

;; This example will insert load-path directories into the current buffer:
;;     (foreach dir load-path (insert "\n" dir))

;;; Code:
;;;###autoload
(defmacro foreach ( var list &rest body )
  "Iteratively set VAR to each item in LIST and execute BODY.  
Like 'mapcar' but executes in-line forms and uses (local) variable VAR.
Use `foreach-stop' to end the loop before all of LIST is used."
  (` (let ((_foreach_list (, list))) 
       (while _foreach_list
	 (setq (, var) (car _foreach_list)
	       _foreach_list (cdr _foreach_list))
	 (progn (,@ body))
	 )))
  )

;;;###autoload
(defmacro foreach-stop ()
  "Used within a `foreach' loop to stop iteration after the current item.
Note that in a nested `foreach' loop, only the inner-most will be stopped."
  (list 'setq '_foreach_list 'nil))

(provide 'foreach)

;;; foreach.el ends here.
