;;; org-table-jb-extras.el --- Extra commands & functions for working with org-tables

;; Filename: org-table-jb-extras.el
;; Description: Extra commands & functions for working with org-tables
;; Author: Joe Bloggs <vapniks@yahoo.com>
;; Maintainer: Joe Bloggs <vapniks@yahoo.com>
;; Copyleft (Ↄ) 2024, Joe Bloggs, all rites reversed.
;; Created: 2024-05-23 22:44:11
;; Version: 20240523.2317
;; Last-Updated: Thu May 23 23:17:48 2024
;;           By: Joe Bloggs
;;     Update #: 1
;; URL: https://github.com/vapniks/org-table-jb-extras
;; Keywords: tools 
;; Compatibility: GNU Emacs 25.2.2
;; Package-Requires: ((org-mode "9.4.6") (cl-lib "1") (ido-choose-function "0.1"))
;;
;; Features that might be required by this library:
;;
;; org-mode cl-lib ido-choose-function
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.
;; If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Commentary: 
;;
;; When `org-table-dispatch' is called while point is in an org-table an ido menu of possible
;; actions from `org-table-dispatch-actions' is offered to the user. These actions include:
;; "copy table", "copy rectangle", "kill/clear cells", "export to file", "copy cols to calc",
;; "plot graph", "fit curve to cols", "transpose table", "split/join columns", "join rows/flatten columns",
;; "toggle display of row/column refs".
;; 
;; 
;;;;;;;;

;;; Commands:
;;
;; Below is a complete list of commands:
;;
;;  `org-table-flatten-columns'
;;    Apply FN to next NROWS cells in selected columns and replace cells in current row with results.
;;    Keybinding: M-x org-table-flatten-columns
;;  `org-table-dispatch'
;;    Do something with column(s) of org-table at point.
;;    Keybinding: M-x org-table-dispatch
;;
;;; Customizable Options:
;;
;; Below is a list of customizable options:
;;
;;  `org-table-flatten-functions'
;;    Alist of (NAME . FUNCTION) pairs for use with `org-table-flatten-column'.
;;  `org-table-graph-types'
;;    List of graph types for `org-plot/gnuplot'.
;;  `org-table-dispatch-actions'
;;    Actions that can be applied when `org-table-dispatch' is called.

;;
;; All of the above can be customized by:
;;      M-x customize-group RET org-table RET
;;

;;; Installation:
;;
;; Put org-table-jb-extras.el in a directory in your load-path, e.g. ~/.emacs.d/
;; You can add a directory to your load-path with the following line in ~/.emacs
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;; where ~/elisp is the directory you want to add 
;; (you don't need to do this for ~/.emacs.d - it's added by default).
;;
;; Add the following to your ~/.emacs startup file.
;;
;; (require 'org-table-jb-extras)

;;; History:

;;; Require
(require 'org)
(require 'cl-lib)
(require 'ido-choose-function)

;;; Code:

;; REMEMBER TODO ;;;###autoload's 
(defvar org-table-vline-delim-regexp "^[[:blank:]]*\\($\\|#\\+\\|\\*[[:blank:]]+\\)"
  "A regular expression used by `org-table-insert-or-delete-vline' for matching 
  lines immediately before or after a table.")

;;;###autoload
(cl-defun org-table-insert-or-delete-vline (&optional ndelete)
  "Insert a vertical line in the current column, or delete some if NDELETE is non-nil.
  If NDELETE is a positive integer, or if called interactively with a positive numeric prefix arg, 
  then NDELETE of the following vertical lines will be deleted. If NDELETE is negative then 
  previous vertical lines will be deleted."
  (interactive (list (and current-prefix-arg
			  (prefix-numeric-value current-prefix-arg))))
  (if ndelete (dotimes (x (abs ndelete))
		(if (> 0 ndelete)
		    (re-search-backward "|\\|+" (line-beginning-position))
		  (re-search-forward "|\\|+" (line-end-position))
		  (forward-char -1))
		(org-table-delete-vline))
    (let ((startn (save-excursion
		    (re-search-backward org-table-vline-delim-regexp)
		    (line-number-at-pos)))
	  (endn (save-excursion
		  (re-search-forward org-table-vline-delim-regexp)
		  (line-number-at-pos)))
	  (thisn (line-number-at-pos)))
      (org-table-insert-vline (- startn thisn))
      (delete-char 1)
      (org-table-insert-vline (- endn thisn))))
  (org-table-align))

;;;###autoload
(defun org-table-insert-vline (len)
  "Insert a vertical line of length LEN starting at point.
  If LEN is negative the line goes upwards, otherwise it goes downwards."
  (let ((col (current-column))
	(start (point)))
    (save-excursion
      (end-of-line (if (> len 0) len (+ 2 len)))
      (let* ((col2 (current-column))
	     (diff (- col2 col))
	     (end
	      (if (>= diff 0) (- (point) diff)
		(insert (make-string (- diff) 32))
		(point))))
	(if (< start end)
	    (string-rectangle start end "|")
	  (string-rectangle end (if (< diff 0) (- start diff) start) "|"))))))

;;;###autoload
(defun org-table-delete-vline nil
  "Delete the vertical line in the current column (if any)."
  (let ((col (current-column))
	(start (point)))
    (cl-flet ((vlineend (start col fwd)
			(save-excursion
			  (while (and (eq (current-column) col)
				      (or (eq (char-after) 124)
					  (eq (char-after) 43)))
			    (forward-line (if fwd 1 -1))
			    (forward-char-same-line (- col (current-column))))
			  (unless (eq (point) start)
			    (forward-line (if fwd -1 1))
			    (forward-char-same-line (- col (current-column)))
			    (if fwd (1+ (point)) (point))))))
      (delete-extract-rectangle (vlineend start col nil) (vlineend start col t)))))

;;;###autoload
(defcustom org-table-flatten-functions
  '(("append" . (lambda (sep lst)
		  (interactive (list (read-string "Separator (default \" \"): " nil nil " ") '<>))
		  (mapconcat 'identity lst sep)))
    ("prepend" . (lambda (sep lst)
		   (interactive (list (read-string "Separator (default \" \"): " nil nil " ") '<>))
		   (mapconcat 'identity (reverse lst) sep)))
    ("sum" . (lambda (lst) (apply '+ (mapcar 'string-to-number lst))))
    ("mean" . (lambda (lst) (/ (apply '+ (mapcar 'string-to-number lst)) (length lst))))
    ("max" . (lambda (lst) (apply 'max (mapcar 'string-to-number lst))))
    ("min" . (lambda (lst) (apply 'min (mapcar 'string-to-number lst))))
    ("delete all but nth" . (lambda (n lst)
			      (interactive (list (1- (read-number "Row to keep (1 = first, etc.): " 1)) '<>))
			      (nth n lst))))
  "Alist of (NAME . FUNCTION) pairs for use with `org-table-flatten-column'."
  :group 'org-table
  :type 'alist)

;;;###autoload
(defun org-table-flatten-column (nrows fn)
  "Replace current cell with results of applying FN to NROWS cells under, and including, current one.
  If NROWS is a positive integer then the NROWS cells below and including the current one will be used.
  If NROWS is a negative integer then the NROWS cells above and including the current one will be used.
  If NROWS is not a number then all cells in the current column between horizontal separator lines will
  be used. Function FN should take a single argument; a list of the contents of the cells. 
  Return value is the number of rows used."
  (unless (org-at-table-p) (error "Not in org-table"))
  (unless (numberp nrows)
    (let ((col (org-table-current-column)))
      (re-search-forward org-table-hline-regexp (org-table-end) 1)
      (forward-line -1)
      (let ((endline (line-number-at-pos (point))))
	(re-search-backward org-table-hline-regexp (org-table-begin) 1)
	(forward-line 1)
	(org-table-goto-column col)
	(setq nrows (1+ (- endline (line-number-at-pos (point))))))))
  (let* ((count nrows)
	 (l (if (> count 0) 1 -1))
	 (startpos (point))
	 (col (org-table-current-column))
	 (fields (cl-loop for n from 1 to (abs count) while (org-at-table-p)
			  collect (org-trim (org-table-blank-field))
			  do (forward-line l)
			  (while (org-at-table-hline-p) (forward-line l))
			  (org-table-goto-column col))))
    (goto-char startpos)
    (if fn (insert (format "%s" (funcall fn fields))))
    (org-table-align)
    nrows))

;;;###autoload
(defun org-table-flatten-columns (nrows ncols fn &optional repeat)
  "Apply FN to next NROWS cells in selected columns and replace cells in current row with results.
  If NROWS is a positive integer then the NROWS cells below and including the current one will be used.
  If NROWS is a negative integer then the NROWS cells above and including the current one will be used.
  If NROWS is not a number (e.g. when called interactively with a C-u prefix), then cells between
  the separator lines above and below the current line will be used.
  If NCOLS is non-nil then flatten the next NCOLS columns (including the current one), otherwise
  flatten all columns. 
  Alternatively, when called interactively, if region is active then that will be used to determine 
  which cells are used.
  This function calls `org-table-flatten-column' (which see) on columns in the current row.
  If REPEAT is supplied then repeat this process REPEAT times."
  (interactive (let* ((regionp (region-active-p))
		      (regionstart (if regionp (region-beginning)))
		      (regionend (if regionp (region-end))))
		 (list 
		  (or (if regionp
			  (1+ (- (line-number-at-pos regionend)
				 (line-number-at-pos regionstart))))
		      current-prefix-arg
		      (read-number "Number of rows (-ve numbers count backwards): "))
		  (if regionp
		      (1+ (- (save-excursion (goto-char (region-end))
					     (org-table-current-column))
			     (progn (goto-char (region-beginning))
				    (org-table-current-column))))
		    (if (not (y-or-n-p "Flatten entire line? "))
			(read-number "Number of columns (-ve numbers count backwards): " 1)))
		  (ido-choose-function
		   org-table-flatten-functions nil "User function with one arg (list of fields): " t)
		  (if regionp 1 (read-number "Number or repetitions: " 1)))))
  (let* ((startline (org-table-current-line))
	 (line startline)
	 (col (org-table-current-column))
	 (maxcol (save-excursion (end-of-line)
				 (org-table-previous-field)
				 (org-table-current-column)))
	 (startcol (if ncols col 1))
	 (endcol (if ncols (if (> ncols 0)
			       (min (1- (+ col ncols)) maxcol)
			     (max (1+ (+ col ncols)) 1))
		   maxcol))
	 (numreps (or repeat 1))
	 l)
    (org-table-goto-line startline)
    (org-table-goto-column col)
    (while (and (org-at-table-p) (> numreps 0))
      (cl-loop for c from (min startcol endcol) to (max startcol endcol)
	       do (org-table-goto-line line)
	       (org-table-goto-column c)
	       (setq nrows (org-table-flatten-column nrows fn)))
      (setq l (if (> nrows 0) 1 -1))
      (forward-line l)
      (dotimes (n (1- (abs nrows)))
	(while (looking-at "^[-+|]*$") (forward-line l))
	(unless (not (org-at-table-p))
	  (if (looking-at "^[ |]*$")
	      (progn (kill-region (point-at-bol)
				  (min (1+ (point-at-eol)) (point-max)))
		     (if (< nrows 0) (forward-line l)))
	    (forward-line l))))
      (while (looking-at "^[-+|]*$") (forward-line l))
      (org-table-goto-column col)
      (setq line (org-table-current-line))
      (setq numreps (1- numreps)))
    (org-table-goto-line startline)
    (org-table-goto-column col)))

;;;###autoload
(cl-defun org-table-grab-columns (top bottom arg &optional kill)
  "Copy/kill columns or region of table and return as list(s).
  The return value is a list of lists - one for each row of the copied/killed data.
  If KILL is non-nill clear the copied fields, otherwise leave them.
  With no prefix ARG copy entire table, with a single C-u prefix copy the entire current column,
  with a numeric prefix copy that many columns from the current one rightwards, with a double C-u 
  prefix copy the data in the current column between horizontal separator lines.
  If region is active, or TOP & BOTTOM args are non-nil copy all cells in rectangle defined by region 
   (or TOP & BOTTOM args), ignoring horizontal separator lines."
  (interactive "r\nP")
  (if (org-at-table-p)
      (save-excursion
	(org-table-check-inside-data-field)
	(let (col beg end (org-timecnt 0) diff h m s org-table-clip)
	  ;; copy column(s) to `org-table-clip' 
	  (cond
	   ((org-region-active-p)
	    (org-table-copy-region top bottom kill))
	   ;; separator delimited column(s) 
	   ((and (listp arg) (> (prefix-numeric-value current-prefix-arg) 10))
	    (setq col (org-table-current-column))
	    (if (re-search-backward org-table-hline-regexp beg t)
		(forward-line 1)
	      (goto-char (org-table-begin)))
	    (org-table-goto-column col)
	    (setq beg (point))
	    (unless (re-search-forward org-table-hline-regexp end t)
	      (goto-char (org-table-end)))
	    (forward-line -1)
	    (org-table-goto-column col)
	    (org-table-copy-region beg (point) kill))
	   ;; whole column(s)
	   (arg
	    (setq col (org-table-current-column))
	    (goto-char (org-table-begin))
	    (org-table-goto-column col)
	    (setq beg (point))
	    (goto-char (org-table-end))
	    (forward-line -1)
	    (org-table-goto-column
	     (if (listp arg) col
	       (+ col (- arg (/ arg (abs arg))))))
	    (org-table-copy-region beg (point) kill))
	   ;; whole table
	   (t 
	    (goto-char (org-table-begin))
	    (org-table-goto-column 1)
	    (setq beg (point))
	    (goto-char (org-table-end))
	    (forward-line -1)
	    (org-end-of-line)
	    (org-table-previous-field)
	    (org-table-copy-region beg (point) kill)))
	  org-table-clip))))

;;;###autoload
(defun org-table-to-calc (lsts &optional unpack)
  "Add data in LSTS to calc as a matrix.
  The lists in LSTS will form the rows of the calc matrix created.
  If optional argument UNPACK is non-nil then unpack the vector/matrix."
  (let ((data
	 (cl-loop for col in lsts
		  collect
		  (nconc
		   (list 'vec)
		   (cl-remove-if-not
		    (lambda (x) (memq (car-safe x) '(float nil)))
		    (mapcar 'math-read-expr col))))))
    (calc)
    (calc-slow-wrapper
     (calc-enter-result
      0 "grab" (nconc (list 'vec)
		      (if (= (length data) 1) (cdar data) data))))
    (if unpack (calc-unpack nil))))

;;;###autoload
(defun org-table-plot-list (lst)
  (require 'org-plot)
  (let* ((name (ido-completing-read
		"Graph type: "
		(append '("Default" "User defined")
			(mapcar 'car org-table-graph-types))))
	 (params (cdr-safe (assoc name org-table-graph-types)))
	 (plotrx "[[:space:]]*#\\+PLOT:[[:space:]]*")
	 params2)
    (if (null params)
	(save-excursion
	  (goto-char (org-table-begin))
	  (if (equal name "User defined")
	      (setq params2 (org-plot/add-options-to-plist
			     params2 (read-string "Plotting parameters (set, title, ind, deps, type, with, labels, line, map, script, timefmt):\n"
						  (if (re-search-backward
						       (concat plotrx "\\(.*\\)")
						       (save-excursion (forward-line -3) (point)) t)
						      (match-string 1)
						    "ind:1 with:lines plot-type:2d"))))
	    ;; if there are no params use the default ones
	    (while (and (equal 0 (forward-line -1))
			(looking-at plotrx))
	      (setf params2 (org-plot/collect-options params2)))))
      (cl-loop for prop in params by 'cddr
	       do (let ((val (plist-get params prop)))
		    (setq params2
			  (plist-put params2 prop
				     (if (eq val 'prompt)
					 (case prop
					   (:plot-type (make-symbol (ido-completing-read "Plot type: "
											 '("2d" "3d" "grid"))))
					   (:script (ido-read-file-name "Gnuplot script to include: "))
					   (:line (read-string "Line to be added to gnuplot script: "))
					   (:set (read-string "Set an option (e.g. ylabel 'Name'): "))
					   (:title (read-string "Title of plot: "))
					   (:ind (read-number "Column containing independent var, relative to selected columns: " 1))
					   (:deps (read--expression
						   "Columns containing dependent vars as a list, e.g. (2 3 4), and relative to selected columns : "))
					   (:with (make-symbol
						   (ido-completing-read
						    "Style: "
						    '("lines" "dots" "steps" "errorbars" "xerrorbar" "xyerrorlines"
						      "points" "impulses" "fsteps" "errorlines" "xerrorlines" "yerrorbars"
						      "linespoints" "labels" "histeps" "financebars" "xyerrorbars" "yerrorlines"
						      "vectors" "boxes" "candlesticks" "image" "circles" "boxerrorbars"
						      "filledcurves" "rgbimage" "ellipses" "boxxyerrorbars" "histograms"
						      "rgbalpha" "pm3d" "boxplot"))))
					   (:labels (let (label labels)
						      (while (> (length
								 (setq label
								       (read-string
									"Labels to be used for the dependent vars (enter to stop): ")))
								0)
							(push label labels))
						      labels))
					   (:map  (y-or-n-p "Use flat mapping for grid plot"))
					   (:timefmt  (read-string "Timefmt (e.g. %Y-%m-%d): ")))
				       val))))))
    (setq tofile (y-or-n-p "Save to file?")
	  filename (if tofile
		       (substring-no-properties
			(ido-read-file-name "Filename: "))))
    (if (= (length (setq tlst (org-table-transpose lst))) 1)
	(setq lst
	      (org-table-transpose
	       (cons (cl-loop for i from 1 to (length (car tlst))
			      collect (number-to-string i))
		     tlst))
	      params2 (plist-put params2 :ind 1))
      (setq lst (org-table-transpose tlst)))
    (if tofile
	(setq params2 (plist-put params2 :file filename)))
    (with-temp-buffer
      (insert (org-table-lisp-to-string lst))
      (forward-line -1)
      (org-plot/gnuplot params2))
    (if (and tofile (y-or-n-p "View saved graph"))
	(find-file filename))
    (message "Plotting parameters: %s" params2)))

;;;###autoload
(defcustom org-table-graph-types nil
  "List of graph types for `org-plot/gnuplot'.

  An assoc-list of (NAME . PLIST) pairs where NAME is the name of the graph type,
  and PLIST is a property list to be used as an argument for `org-plot/gnuplot'.
  For a list of possible properties and values see the org-plot.org file.
  If any of the property values is 'prompt then the user will be prompted for a
  value when the graph is chosen."
  :group 'org-table
  :type '(repeat (cons string
		       (plist :key-type sexp :value-type sexp))))

;;;###autoload
(defcustom org-table-dispatch-actions '(("copy table" . (lambda (lst) (kill-new (org-table-lisp-to-string lst))))
					("copy rectangle" . (lambda (lst)
							      (setq killed-rectangle
								    (split-string (org-table-lisp-to-string lst)
										  "\n"))))
					("kill/clear cells" . (lambda (lst) (kill-new (org-table-lisp-to-string lst))))
					("export to file" . (lambda (lst)
							      (with-temp-buffer
								(insert (org-table-lisp-to-string lst))
								(forward-line -1)
								(org-table-export))))
					("copy cols to calc" .
					 (lambda (lst) (org-table-to-calc (org-table-transpose lst))))
					("plot graph" . (lambda (lst) (org-table-plot-list lst)))
					("fit curve to cols" .
					 (lambda (lst) (org-table-to-calc (org-table-transpose lst) nil)
					   (calc-curve-fit nil)))
					("transpose table" . (lambda nil (org-table-transpose-table-at-point)))
					("split/join columns" .
					 (lambda nil (call-interactively 'org-table-insert-or-delete-vline)))
					("join rows/flatten columns" .
					 (lambda nil (call-interactively 'org-table-flatten-columns)))
					("Toggle display of row/column refs" .
					 (lambda nil (org-table-toggle-coordinate-overlays))))
  "Actions that can be applied when `org-table-dispatch' is called.
  Each element should be of the form (NAME . FUNC) where NAME is a name for the action,
  and FUNC is a function of zero or one arguments. If FUNC has one argument then a list containing 
  columns of data returned by `org-table-graph-columns' will be passed in.
  If the NAME contains the string \"kill\" then the selected columns will be deleted from the table."
  :group 'org-table
  :type '(alist :key-type string :value-type (function :tag "Function acting on list of lists")))

;; TODO - do something similar for .csv files?
;;;###autoload
(defun org-table-dispatch nil
  "Do something with column(s) of org-table at point.
  Prompt the user for an action in `org-table-dispatch-actions' and apply the corresponding function.
  If the function takes a single argument then pass in a subtable list obtained from `org-table-grab-columns'.
  If in addition, the name of the action contains the word \"kill\" then the cells in the selected columns/region 
  will be cleared."
  (interactive)
  (let ((pair (assoc (ido-completing-read "Action: " (mapcar 'car org-table-dispatch-actions))
		     org-table-dispatch-actions)))
    (if (= (length (caddr pair)) 1)
	(funcall (cdr pair) (org-table-grab-columns (if (region-active-p) (region-beginning))
						    (if (region-active-p) (region-end))
						    current-prefix-arg
						    (string-match "\\<kill\\>" (car pair))))
      (funcall (cdr pair)))))



(provide 'org-table-jb-extras)

;; (org-readme-sync)
;; (magit-push)

;;; org-table-jb-extras.el ends here
