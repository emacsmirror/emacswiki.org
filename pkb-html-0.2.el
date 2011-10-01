;;; pkb-formatting --- print bindings in a pretty fashion in html format
;;; v.0.2

;; Copyright (C) 2011 Jonathan Ganc
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see http://www.gnu.org/licenses/.

;; Short usage guide:
;;  Run (pkb-html-save-keymap KEYMAP OUTPUT-FILE-NAME).

;; See pkb.el for more design information
;; See also: http://www.ph.utexas.edu/~jonganc/emacs.html

;; Some data structures that come up:
;; PK-OPTIONS, MK-OPTIONS, BK-OPTIONS, BIND-OPTIONS can be either a string
;;  giving the printed form of the object (i.e. the mk, bk, or bind,
;;  respectively) or an alist with key-value pairs (e.g. like (anchor-name
;;  . "pk1"), where `anchor-name' is a symbol. Some of the meaningful
;;  keys:
;;   `outp-str': the string representation of the object for output
;;   `replace-fl': if non-nil, `outp-str'-value will be matched against the
;;     ...-REPLACE-... lists (e.g. EVENT-REPLACE-REGEXP). Otherwise, the
;;     item won't be won't be matched against.
;;   `class-list': a list of strings to be concatenated and placed in the
;;     class tag in the cell containing the object.
;;   `esc-map-fl' (PK-OPTIONS): If non-nil, the last key of this keymap's
;;     key-sequence is an ESC, so that certain bindings should be ignored
;;     because they are actually meta keys of the parent keymap
;;   `anchor-name' (probably only PK-OPTIONS): Give the name for an html
;;     anchor to be placed above the object
;; ONE-GROUP-FOR-OUTPUT (like a ONE-GROUP) is of the form:
;;   (TYPE-SYMB (GROUP-NAME OPTION) BLOCKS),
;;  where TYPE-SYMB is either `full-gr' or `compact-gr'.

(require 'pkb (concat
	       (file-name-directory
		(if load-file-name load-file-name buffer-file-name))
	       "pkb-0.2.el"))

(defun escape-html (stringi) ;; OK
  "Escape string STRINGI for use in html."
  (dolist (rep '(("&" . "&amp;") ("<" . "&lt;") (">" . "&gt;")
		  ("\"" . "&quot;") ("'" . "&#39;") ("\\\\" . "&#92;")) )
    (setq stringi (replace-regexp-in-string (car rep) (cdr rep) stringi)))
  stringi
)

(defun pkb-html-sect-head (pk-options) ;; OK
  "Return the header of section with title TITLE."
  (concat "<h2>"
	  (if (consp pk-options)
	      (cdr (assq 'outp-str pk-options))
	    pk-options)
	  "</h2>")
)

(defvar pkb-html-sect-foot
  ;; "<br><br>\n\n"
  ""
  "The footer of a section."
)

(defun pkb-html-ins-full (full-sect pk-options max-bks-per-compact-row) ;; OK
  "Insert (at cursor) a section for a full pk based on FULL-SECT, using PK-OPTIONS to print the title (and for other options).

Groups printed as `compact' will have MAX-BKS-PER-COMPACT-ROW bks per row.

The format of FULL-SECT should be
\(REMAINING-BKS-W-BINDS ONE-GROUP-1 ONE-GROUP-2 ...), i.e. the same as the
output from `pkb-split-full-to-groups' without the first element. However,
the bks and binds should be ready for printing (i.e. have their string
representations set)."
  ;; process REMAINING-BKS-W-BINDS
  (when (car full-sect)
    (setcar full-sect
	    (pkb-process-blocks
	     (pkb-bks-w-binds-to-blocks-prelim (car full-sect)
					       max-bks-per-compact-row))))
  ;; process GROUPS
  (dolist (one-group (cdr full-sect))
    (cond
     ((eq (car one-group) 'full-gr)
      (setcdr (cdr one-group)
	      (pkb-process-blocks (nthcdr 2 one-group))))
     ((eq (car one-group) 'compact-gr)
      (setcdr (cdr one-group)
	      (pkb-process-blocks
	       (pkb-bks-w-binds-to-blocks-prelim
		(nthcdr 2 one-group) max-bks-per-compact-row))))))

  ;; now, each ONE-GROUP is in the format:
  ;;   ('type (GROUP-NAME OPTIONS) . BLOCKS),
  ;;  where type is either `full-gr' or `compact-gr'.

  (insert
   ;; ** header of full section **
   (let (anchor-asscn)
     (concat
      "<div class=\"full-sect\">\n"
      (when (and (listp pk-options)
		 (setq anchor-asscn (assq 'anchor-name pk-options)))
	(concat "<a name=\"" (cdr anchor-asscn) "\">"))
      (pkb-html-sect-head pk-options)
      (when anchor-asscn
	"</a>")
      "\n")))
  (pkb-dolist-cons (one-group-cons (cdr full-sect))
    ;; ** between groups
    (unless (eq one-group-cons (cdr full-sect))
      (insert ""))
    (cond
     ((eq (caar one-group-cons) 'full-gr)
      (insert "<span class=\"full-gr\">\n"))
     ((eq (caar one-group-cons) 'compact-gr)
      (insert "<span class=\"compact-gr\">\n"))
     (t (error "Should not get here")))
    (pkb-html-ins-full-one-gr (car (nth 1 (car one-group-cons)))
			      (nthcdr 2 (car one-group-cons)))
    (insert "</span>\n"))
  (when (car full-sect)
    (insert "<div class=\"remaining-gr\">")
    (pkb-html-ins-full-one-gr "Remaining keys" (car full-sect))
    (insert "</div>"))
  (insert
  ;; ** footer of full section **
   "</div>\n\n"
   pkb-html-sect-foot)
)

(defun pkb-html-ins-full-one-gr (group-name blocks)
  "Insert (at cursor) a `full-gr' (i.e. a full group) with name GROUP-NAME and
BLOCKS."
  (insert
   ;; ** group header
   "<h3>" group-name "</h3>\n"

   (pkb-html-pr-bk-table blocks)

   ;; ** group footer
   ""
   )
)

(defun pkb-html-ins-compact (bks-w-binds pk-options max-bks-per-row) ;; OK
  "Insert (at cursor) a compact keymap using BKS-W-BINDS with
MAX-BKS-PER-COMPACT-ROW bks per row and using PK-OPTIONS to print the title
\(and for other options). BKS-W-BINDS. BKS-W-BINDS should be prepared for
printing (i.e. the string settings in BK-OPTIONS and BIND-OPTIONS should be
set)."
  (let ((blocks
	 (pkb-process-blocks
	  (pkb-bks-w-binds-to-blocks-prelim bks-w-binds max-bks-per-row))))
    (insert
     ;; ** header of compact section
     "<div class=\"compact-sect\">"
     (let (anchor-asscn)
       (concat
	"<div class=\"compact-sect\">\n"
	(when (and (listp pk-options)
		   (setq anchor-asscn (assq 'anchor-name pk-options)))
	  (concat "<a name=\"" (cdr anchor-asscn) "\">"))
	(pkb-html-sect-head pk-options)
	(when anchor-asscn
	  "</a>")
	"\n"))
     (pkb-html-pr-bk-table blocks)
     ;; ** footer of full section
     pkb-html-sect-foot
     "</div>\n\n"))
)

(defun pkb-html-ins-simple (mks pk-options num-columns) ;; OK
  "Insert (at cursor) a based on MKS with NUM-COLUMNS columns and using
PK-OPTIONS to print the title (and for other options). MKS should be
prepared for printing (i.e. the string settings in MK-OPTIONS and
BIND-OPTIONS should be set)."
  (insert
   ;; ** header of simple section
   "<div class=\"simple-sect\">"
   (let (anchor-asscn)
     (concat
      "<div class=\"simple-sect\">\n"
      (when (and (listp pk-options)
		 (setq anchor-asscn (assq 'anchor-name pk-options)))
	(concat "<a name=\"" (cdr anchor-asscn) "\">"))
      (pkb-html-sect-head pk-options)
      (when anchor-asscn
	"</a>")
      "\n"))

   ;; ** simple keymap bindings
   (pkb-html-pr-pairs-columns mks num-columns)

   ;; ** footer of simple section
   pkb-html-sect-foot
   "</div>\n\n")
)

(defun pkb-html-pr-bk-table (blocks) ;; OK
  "Return a string containing the output."
  (let ((max-elems-in-block 0) txt)
  ;; ;; make it so that all blocks have the same number of cells
  ;; (dolist (one-block blocks)
  ;;   (when (> (length (car one-block)) max-elems-in-block)
  ;;     (setq max-elems-in-block (length (car one-block)))))
  ;; ** start of table
  (setq txt "<table class=\"bk-table\">\n")
  (let (leng
	(next-leng (length (caar blocks))))
   ;; LENG, NEXT-LENG will hold the number of columns in the current, next
   ;;  row, respectively
    (pkb-dolist-cons (one-block-cons blocks)
    ;; (let ((empty-col (- max-elems-in-block leng)))
      (setq leng next-leng)
      (setq next-leng (length (car (cadr one-block-cons))))
      ;; ** start of block
      ;; (pkb-concat= 'txt "<div class=\"one-block\">")

      ;; Print bks
      ;; first check that block begin with 'bks
      (unless (eq (caar (car one-block-cons)) 'bks)
	(error "Blocks not properly formatted."))
      ;; ** start of bks row
      (pkb-concat= 'txt "<tr class=\"bk-row\"><th class=\"mod-cell\"></th>")
      (pkb-dolist-cons (obj-options-cons (cdar (car one-block-cons)))
	;; ** print bks
	(let ((options (cdar obj-options-cons)))
	(pkb-concat=
	 'txt "<th"
	 ;; code labeled below is duplicated in the section labeled 'print
	 ;;  bindings' and in `pkb-html-pr-pairs-columns'
	 ;; <common-code>
	 (let* ((classes (when (consp options)
			   (cdr (assq 'class-list options))))
		(classes-txt
		 (mapconcat 'identity classes " ")))
	   (when classes
	     (concat " class=\"" classes-txt "\"")))
	 ">"
	 (if (consp options)
	     (cdr (assq 'outp-str options))
	   options)
	 ;; </common-code>
	 "</th>")))
      ;; ;; print empty columns if neccessary so that each row has same
      ;; ;;  number of columns
      ;; (unless (= empty-col 0)
      ;;   (pkb-concat= 'txt
      ;; 		   "<th class=\"empty-col\" colspan="
      ;; 		   (number-to-string empty-col)
      ;; 		   "></th>"))
      ;; ** end of bks row
      (pkb-concat= 'txt "</tr>\n")

      ;; Print the bindings for the block
      (pkb-dolist-cons (mods-type-w-binds-cons (cdar one-block-cons))
	(let* ((mods-type (caar mods-type-w-binds-cons))
	       (css-row-class "mod") mods-desc
	       (known-mods-types
		'((nil "unmod" "&nbsp;")
		  ((control) "mod-c" "C") ((meta) "mod-m" "M")
		  ((control meta) "mod-c-m" "C-M") ((shift) "mod-s" "S")))
	       (mod-types-descs
		'((control "C") (meta "M") (shift "S") (hyper "H") (alt "A")
		  (super "s" "su")))
	       (asscn (assoc mods-type known-mods-types)))
	;; ** start of bindings row
	;; set CSS-ROW-CLASS, MODS-DESC
	(catch 'quit
	  ;; is MODS-TYPE in KNOWN-MODS-TYPES?
	  (when asscn
	    (setq css-row-class (nth 1 asscn))
	    (setq mods-desc (nth 2 asscn))
	    (throw 'quit nil))
	  ;; at this point, mods-type not in KNOWN-MODS-TYPES
	  (dolist (one-mod-type mods-type)
	    (let ((desc-asscn (assoc one-mod-type mod-types-descs)))
	      (pkb-concat= 'css-row-class "-"
			   (cond ((nth 2 desc-asscn))
				 ((nth 1 desc-asscn))
				 (t (prin1-to-string one-mod-type))))
	      (pkb-concat= 'mods-desc
			   (when (not (eq one-mod-type (car mods-type))) "-")
			   (cond ((nth 1 desc-asscn))
				 (t (prin1-to-string one-mod-type)))))))
	(pkb-concat= 'txt
		     "<tr class=\"mod-row " css-row-class
		     ;; (when (eq mods-type-w-binds-cons
		     ;; 	     (cdr (car one-block-cons)))
		     ;;   " first-bind-row-of-block")
		     ;; (when (null (cdr mods-type-w-binds-cons))
		     ;;   " last-bind-row-of-block")
		     "\"><th class=\"mod-cell\">"
		     mods-desc
		     "</th>")
	(pkb-dolist-cons (obj-options-cons (cdar mods-type-w-binds-cons))
	  ;; ** print bindings
	  (let ((options (cdar obj-options-cons)))
	  (pkb-concat= 'txt
	     "<td"
	     ;; code labeled below is duplicated in the section labeled 'print
	     ;;  bindings'
	     ;; <common-code>
	     (let* ((classes (when (consp options)
			       (cdr (assq 'class-list options))))
		    (classes-txt
		     (mapconcat 'identity classes " ")))
	       (when classes
		 (concat " class=\"" classes-txt "\"")))
	     ">"
	     (if (consp options)
		 (cdr (assq 'outp-str options))
	       options)
	     ;; </common-code>
	     "</td>"))))

	;; ;; print empty columns if neccessary so that each row has same
	;; ;;  number of columns
	;; (unless (= empty-col 0)
	;; 	(pkb-concat= 'txt
	;; 		     "<td class=\"empty-col\" colspan="
	;; 		     (number-to-string empty-col)
	;; 		     "></td>"))
	;; ** end of bind row
	(pkb-concat= 'txt "</tr>\n"))

      ;; ** end of block
      ;; (pkb-concat= 'txt "</div>")

      ;; ** between blocks
      (when (cdr one-block-cons)
	(pkb-concat= 'txt "<tr class=\"between-blocks\"><td colspan="
		     (number-to-string leng)
		     "></td>"
		     (when (and next-leng
				(> (- next-leng leng) 0))
		       (concat "<td class=\"overhang\" colspan="
			       (number-to-string (- next-leng leng))
			       "></td>"))
		     "</tr>\n"))))

  ;; ** end of table
   (pkb-concat= 'txt "</table>"))
)

(defun pkb-html-pr-pairs-columns (pairs num-columns) ;; OK
"Return a string showing NUM-COLUMNS columns of PAIRS, a list of ONE-PAIRS
each of which is of the form
\(KEY KEY-OPTIONS VALUE . VALUE-OPTIONS), i.e. a more abstract form of a ONE-MK.
KEY, VALUE should be prepared for printing (i.e. the string settings in
KEY-OPTIONS and VALUE-OPTIONS should be set)."
  ;; adding num-columns
  (let* ((leng (length pairs))
	 (remaind (mod leng num-columns))
	 ;; NUM-ROWS should be leng/num-columns rounded up.
	 (num-rows (+ (/ (length pairs) num-columns)
		      (if (= remaind 0) 0 1)))
	 txt)
  ;; ** start of columns
  (pkb-concat= 'txt "")
  ;; ** start of table
  (pkb-concat= 'txt "<table>\n")

  ;; ** print rows of table
  (dotimes (row num-rows)
    ;; ** start of row
    (pkb-concat= 'txt "<tr>")
    (dotimes (col num-columns)
      ;; ** between columns
      (let ((cur (+ (* col num-rows) row))
	    one-pair)
	(cond
	 ((< cur leng)
	  (unless (= col 0)
	    (pkb-concat= 'txt "<td class=\"col-sep\">&nbsp;</td>"))
	  ;; ** one pair
	  (setq one-pair (nth cur pairs))
	  (let ((options (nth 1 one-pair)))
	  (pkb-concat= 'txt
	     ;; print KEY
	     "<th"
	       ;; code labeled below is duplicated in the section labeled
	       ;;  'print VALUE' and in `pkb-html-pr-bk-table'
	     ;; <common-code>
	     (let* ((classes (when (consp options)
			       (cdr (assq 'class-list options))))
		    (classes-txt
		     (mapconcat 'identity classes " ")))
	       (when classes
		 (concat " class=\"" classes-txt "\"")))
	     ">"
	     (if (consp options)
		 (cdr (assq 'outp-str options))
	       options)
	     ;; </common-code>
	     "</th>"))
	  ;; print VALUE
	  ;; (setq one-pair (nthcdr 2 one-pair))
	  (let ((options (cdr (nthcdr 2 one-pair))))
	  (pkb-concat= 'txt
	     "<td"
	       ;; code labeled below is duplicated in the section labeled
	       ;;  'print KEY' and in `pkb-html-pr-bk-table'
	     ;; <common-code>
	     (let* ((classes (when (consp options)
			       (cdr (assq 'class-list options))))
		    (classes-txt
		     (mapconcat 'identity classes " ")))
	       (when classes
		 (concat " class=\"" classes-txt "\"")))
	     ">"
	     (if (consp options)
		 (cdr (assq 'outp-str options))
	       options)
	     ;; </common-code>
	     "</td>")))
	 ((and (= cur leng) (/= leng 1))
	  (pkb-concat= 'txt
		   "<td class=\"col-sep\" colspan="
		   (number-to-string (* (- num-columns remaind) 3))
		   "></td>")))))
    (pkb-concat= 'txt "</tr>\n"))

  ;; ** end of table
  (pkb-concat= 'txt "</table>\n"))
)

(defvar pkb-key-groups
  '( ( ("Main" 10)
       (?` ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?0 ?- ?= backspace)
       (?~ ?! ?@ ?# ?$ ?% ?^ ?& ?* ?( ?) ?_ ?+)
       (?q ?w ?e ?r ?t ?y ?u ?i ?o ?p ?[ ?] ?\\ ?{ ?} ?|)
       (?a ?s ?d ?f ?g ?h ?j ?k ?l ?\; ?' ?: ?\" return)
       (?z ?x ?c ?v ?b ?n ?m ?, ?. ?/ ?< ?> ?? ?\s))
     ( ("Function keys")
       (escape f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12))
     ( ("Miscellaneous keys")
       (Scroll_Lock pause insert delete home prior next end)
       (left right up down lwindow rwindow))
     ( ("Mouse events" 50)
       (mouse-1 mouse-2 mouse-3 mouse-4 mouse-5 wheel-down wheel-up))
     )
  "A list of key-groups for a general keyboard"
)

(defvar pkb-html-named-pks
  '()
  "An alist of key-bindings intended to be passed to the NAMED-PKS argument of `pkb-html-save-keymap'."
)

(defvar pkb-html-event-replace-bk
  '( (backspace "Backspace" 0 t)
     (delete "Delete" 0 t)
     (escape "ESC" 0 t))
  "A list of proposed replacements intended to be passed to the
EVENT-REPLACE-BK argument of `pkb-html-save-keymap'."
)

(defvar pkb-html-event-replace-regexp
  '(("^&lt;" "" 0) ("&gt;$" "" 0)
    ("delete" "Delete" 0) ("backspace" "Backspace" 0)
    ("escape" "ESC" 0) ("home$" "Home" 0) ("prior$" "PgUp" 0)
    ("insert$" "Insert" 0) ("next$" "Next" 0) ("end$" "End" 0)
    ("return" "Return" 0 t)
    ("\\(\\W\\|^\\)left\\(\\W\\|$\\)"
     "\\1<span class=\"symb big2\">&#x27F5;</span>\\2"
     ("left" . "<span class=\"symb big2\">&#x27F5;</span>"))
    ("\\(\\W\\|^\\)right\\(\\W\\|$\\)"
     "\\1<span class=\"symb big2\">&#x27F6;</span>\\2"
     ("right" . "<span class=\"symb big2\">&#x27F6;</span>"))
    ("\\(\\W\\|^\\)up\\(\\W\\|$\\)"
     "\\1<span class=\"symb big2\">&#x2191;</span>\\2"
     ("up" . "<span class=\"symb big2\">&#x2191;</span>"))
    ("\\(\\W\\|^\\)down\\(\\W\\|$\\)"
     "\\1<span class=\"symb big2\">&#x2193;</span>\\2"
     ("down" . "<span class=\"symb big2\">&#x2193;</span>"))
    ("\\(\\W\\|^\\)f\\([0-9].*\\)$" "\\1F\\2" 0)
    )
  "A list of proposed replacements intended to be passed to the
EVENT-REPLACE-REGEXP argument of `pkb-html-save-keymap'."
)

(defvar pkb-html-binding-replace-obj
  '( (digit-argument "<span class=\"symb big1\">&#x2460;</span>" nil t) )
  "A list of proposed replacements intended to be passed to the
BINDING-REPLACE-OBJ argument of `pkb-html-save-keymap'."
)

(defvar pkb-html-binding-replace-regexp
  '(("\\(\\W\\|^\\)forward\\(\\W\\|$\\)"
     "\\1<span class=\"symb big2\">&#x21D1;</span>\\2"
     ("forward" . "<span class=\"symb big2\">&#x21D1;</span>"))
    ("\\(\\W\\|^\\)backward\\(\\W\\|$\\)"
     "\\1<span class=\"symb big2\">&#x21D3;</span>\\2"
     ("backward" . "<span class=\"symb big2\">&#x21D3;</span>"))
    ("\\(\\W\\|^\\)next\\(\\W\\|$\\)"
     "\\1<span class=\"symb big1\">&#x21D2;</span>\\2"
     ("next" . "<span class=\"symb big1\">&#x21D2;</span>"))
    ("\\(\\W\\|^\\)previous\\(\\W\\|$\\)"
     "\\1<span class=\"symb big1\">&#x21D0;</span>\\2"
     ("previous" . "<span class=\"symb big1\">&#x21D0;</span>"))
    ("\\(\\W\\|^\\)paragraph\\(\\W\\|$\\)"
     "\\1<span class=\"symb big1\">&#x00B6;</span>\\2"
     ("paragraph" . "<span class=\"symb big1\">&#x00B6;</span>"))
    ("\\(\\W\\|^\\)tab\\(\\W\\|$\\)"
     "\\1<span class=\"symb big1\">&#x21B9;</span>\\2"
     ("tab" . "<span class=\"symb big2\">&#x21B9;</span>"))
    ("\\(\\W\\|^\\)backspace\\(\\W\\|$\\)"
     "\\1<span class=\"symb big1\">&#x232B;</span>\\2"
     ("backspace" . "<span class=\"symb big1\">&#x232B;</span>"))
    ("\\(\\W\\|^\\)kill\\(\\W\\|$\\)"
     "\\1<span class=\"symb big1\">&#x2327;</span>\\2"
     ("kill" . "<span class=\"symb big1\">&#x2327;</span>"))
    ("\\(\\W\\|^\\)delete\\(\\W\\|$\\)"
     "\\1<span class=\"symb big1\">&#x232B;</span>\\2"
     ("delete" . "<span class=\"symb big1\">&#x232B;</span>")))
  "A list of proposed replacements intended to be passed to the
BINDING-REPLACE-REGEXP argument of `pkb-html-save-keymap'."
)

(defvar pkb-translate-events-list
  '( (?\d (backspace) t) (?\C-\d (delete) t) (?\M-\d (M-backspace M-delete) t)
     (?\r (return)) (?\M-\r (M-return)))
  "A default argument for TRANSLATE-EVENTS-LIST."
)

(defvar pkb-html-css-header
  "<style type=\"text/css\">
<!--
body,table
{
font-size:10px
}

table
{
padding:0px;
margin:0px;
word-wrap: break-word;
}

.full-sect
{
max-width:10in;
}

.bk-table
{
border-collapse: collapse;
max-width:10.5in;
}

.bk-table td, .bk-table th
{
padding:0px;
margin:0px;
max-width:75px;
}

.bk-table td
{
min-width:40px;
}

.simple-sect table, .legend table
{
border-collapse: collapse;
}

.simple-sect td, .legend td
{
border:1px solid black;
}

td, th
{
border:1px solid black;
}

td.col-sep
{
border:0px solid black;
}

.bk-table tr.bk-row th
{
border-top:2px solid black;
}

.bk-table tr.bk-row + tr td, .bk-table tr.bk-row + tr th
{
border-top:2px solid green;
}

.bk-table tr:last-of-type td, .bk-table tr:last-of-type th
{
border-bottom:2px solid black;
}

.bk-table tr th:first-of-type
 {
border-left:2px solid black;
}

.bk-table td:last-of-type, .bk-table th:last-of-type, .bk-table th:first-of-type
{
border-right:2px solid black;
}

table.bk-table tr.between-blocks td
{
border-left:2px solid black;
border-right:2px solid black;
color: #f00;
background-color: #f00;
height:4px;
}

table.bk-table td.overhang
{
border-bottom:2px solid black;
}

td.keymap
{
text-decoration:underline;
}

hr
{
margin-top:20px;
color: yellow;
background-color: yellow;
border:1px solid black;
height: 3px;
}

.symb
{

}

.big1
{
font-size:14px;
}

.big2
{
font-size:16px;
}

.big3
{
font-size:18px;
}

.big4
{
font-size:20px;
}
-->
</style>"
  "Code to be printed in the html header, primarily for specifying the css style for the output.")

(defun pkb-html-save-keymap
  (keymapi file-name
   &optional title include-list exclude-list key-groups
   named-pks replace-lists formatting-options translate-events-list)
  "Save KEYMAPI to FILE-NAME in html format.

TITLE (def=no title) specifies the title printed in the header of the file.

INCLUDE-LIST gives a list of prefix-keys (as vectors) to process.  Any
prefix-keys beginning with an element of EXCLUDE-LIST will not be included.
See `pkb-accessible-keymaps' for more about the syntax of INCLUDE-LIST and
EXCLUDE-LIST.

NAMED-PKS is a list of elements of the form: (PK NEW-BIND &optional
SECTION-TITLE). It is intended to allow prefix-keys to be given effective
descriptions. If PK has a binding and the binding is a keymap, the binding is
changed to NEW-BIND (a string). KEYMAP-TITLE (a string) is used as the title
of the section for PK (if KEYMAP-TITLE is not given, NEW-BIND is used as the
title).

KEY-GROUPS is passed as input to `pkb-split-full-to-groups'; see that function for the format.

REPLACE-LISTS allows the user to specify that certain events or bindings
should be should be displayed in a certain way. It is a list of the form:
\(EVENT-REPLACE-BK EVENT-REPLACE-REGEXP BIND-REPLACE-OBJ
  BIND-REPLACE-REGEXP),
where each element is a list of proposed replacements. Each such list is of
the form:
\(MATCH REPLACE %optional LEGEND-ENTRY NO-CNT). For BIND-REPLACE-OBJ,
matching is performed before the events are printed as text, i.e. if an
event is `eq' to MATCH it is changed to REPLACE (which should be a
string). EVENT-REPLACE-BK, is similar except that MATCH is only compared to
the basic-type of event. For the two arguments whose names have the suffix
'...REGEXP', matching is performed after entries are printed as text and
escaped using `escape-html'; a MATCH is replaced by REPLACE using a regexp
search. If a match has been found and LEGEND-ENTRY is a cons-cell
\(LEGEND-MATCH . LEGEND-REPLACE), nil, then an entry will be included in the
legend which displays (LEGEND-MATCH . LEGEND-REPLACE),
\( (prin1-to-string MATCH) . REPLACE), respectively; if LEGEND-ENTRY is
non-nil but not a cons-cell, no legend entry will be included. Lastly, if a
match has been found and NO-CNT is t, no further matching will be attempted
on that item (though, for example, even if an event has been matched and
NO-CNT is t, the corresponding binding will be still be matched against).

FORMATTING-OPTIONS allows various formatting options to be selected. It is a list of the form:
\(MAX-MKS-FOR-SIMPLE MAX-BKS-FOR-COMPACT PERCENT-FOR-FULL-GR MAX-BKS-PER-COMPACT-ROW SIMPLE-NUM-COLUMNS LEGEND-NUM-COLUMNS).
All options are optional. Here are partial explanations of the options (further explanations can be found in some of the functions in pkb):
MAX-MKS-FOR-SIMPLE - the maximum numbers of keys before a keymap is
displayed as compact or full.
MAX-BKS-FOR-COMPACT - the maximum numbers of bks before a keymap is
displayed as full.
PERCENT-FOR-FULL-GR - by default, a group in a full keymap is displayed as a full-gr if it has definition for this percentage of bks.
MAX-BKS-PER-COMPACT-ROW - the maximum length of each row of bks in a compact keymap
SIMPLE-NUM-COLUMNS, LEGEND-NUM-COLUMNS - the number of columns when printing
a simple keymap, the legend, respectively.

TRANSLATE-EVENTS-LIST is passed to `pkb-transl-events', `pkb-transl-simp'
through `pkb-categorize-key-list'. See `pkb-transl-events' for syntax. By
default, `pkb-translate-events-list' is used; if no translation wanted, set
0 for TRANSLATE-EVENTS-LIST."
  (interactive (concat "*Xkeymap (or elisp expression evaluating to one): "
		       "\nFfile-name: "))
  (save-current-buffer
  (catch 'outer-quit
  (when (not (file-writable-p file-name))
   ;; if file can't be written, quit
    (error (concat file-name " not writeable"))
    (throw 'outer-quit nil))
   ;; if file exists, prompt to overwrite
  (when (file-exists-p file-name)
    (if (yes-or-no-p (concat file-name " exists. OK to overwrite? "))
     ;; if OK to overwrite
	(delete-file file-name)
     ;; else shouldn't overwrite file, so quit
      (throw 'outer-quit nil)))
  (set-buffer (find-file-noselect file-name))
  (erase-buffer) ;; in case there was previous text in the file
  
  (let (pks-list
	(options-symb (make-symbol "options-symb"))
	event-replace-bk event-replace-regexp
	bind-replace-obj bind-replace-regexp
	(max-mks-for-simple
	 (cond ((nth 0 formatting-options)) (t 7)))
	(max-bks-for-compact 
	 (cond ((nth 1 formatting-options)) (t 11)))
	(percent-for-full-gr
	 (cond ((nth 2 formatting-options)) (t 0.7)))
	(max-bks-per-compact-row 
	 (cond ((nth 3 formatting-options)) (t 11)))
	(simple-num-columns
	 (cond ((nth 4 formatting-options)) (t 2)))
	(legend-num-columns
	 (cond
	  ((nth 5 formatting-options)) ((nth 4 formatting-options)) (t 2)))
	matched-replace)
    ;; PKS-LIST is a list of the pks as they are being processed. Each
    ;;  element is called a ONE-PKS-LIST-ELEM. After some processing, each
    ;;  ONE-PKS-LIST-ELEM will be in one of the following forms ((1), (2),
    ;;  (3a) is almost the same as the output of `pkb-categorize-key-list'
    ;;  and (3b) is almost the same as the output of
    ;;  `pkb-split-full-to-groups'):
    ;;   1)  (PK PK-OPTIONS 'simple . MKS)
    ;;   2)  (PK PK-OPTIONS 'compact . BKS-W-BINDS)
    ;;   3a) (PK PK-OPTIONS 'full BKS-W-BINDS . BINDS-CHAR-TABLE)
    ;;   3b) (PK PK-OPTIONS 'full REMAINING-BKS-W-BINDS . GROUPS)
    ;; OPTIONS-SYMB is an uninterned symbol that I will use to designate
    ;;  that an object (e.g. an event or binding) has been partially
    ;;  processed and is carrying certain options with it. The object should
    ;;  be of the form (OPTIONS-SYMB OBJECT-W-OPTIONS). See the comments
    ;;  below `pkb-html-int-print-object-w-options' for more about
    ;;  OBJECT-W-OPTIONS.
    ;; MATCHED-REPLACE holds any matched entries from ...-REPLACE-..., to be
    ;;  later included in a legend.

  (unless key-groups
    (setq key-groups pkb-key-groups))
  (if replace-lists
      (progn
	(setq event-replace-bk (nth 0 replace-lists))
	(setq event-replace-regexp (nth 1 replace-lists))
	(setq bind-replace-obj (nth 2 replace-lists))
	(setq bind-replace-regexp (nth 3 replace-lists)))
    (setq event-replace-bk pkb-html-event-replace-bk)
    (setq event-replace-regexp pkb-html-event-replace-regexp)
    (setq bind-replace-obj pkb-html-binding-replace-obj)
    (setq bind-replace-regexp pkb-html-binding-replace-regexp))
  (cond
   ((and (integerp translate-events-list)
	 (= translate-events-list 0))
    (setq translate-events-list nil))
   ((null translate-events-list)
    (setq translate-events-list pkb-translate-events-list)))
  ;; generate PKS-LIST
  (setq pks-list (pkb-accessible-keymaps keymapi include-list exclude-list))
  ;; PKS-LIST is now a list with elements (PK PK-OPTIONS . KEYMAP).
  (dolist (one-pks-list-elem pks-list)
    (let ((pk (car one-pks-list-elem)))
    (setcdr (cdr one-pks-list-elem)
	    (pkb-list-keys (cddr one-pks-list-elem)
			   (assq 'esc-map-fl (cadr one-pks-list-elem))
			   (equal pk [])))))
  ;; PKS-LIST is now a list with elements (PK PK-OPTIONS . KEY-LIST).

  ;; Match section titles and bindings against NAMED-PKS.
  (dolist (one-named-pk named-pks)
    (let* ((cur-named-pk (car one-named-pk))
	   cur-named-pk-but-last cur-named-pk-last
	   ;; (where-stored (pkb-key-sequence-location cur-named-pk))
	   asscn event-info)
     ;; ;; WHERE-STORED, in the form (KEYMAP . EVENT), is where CUR-PK would
     ;; ;;  be stored in a tree of keymaps
    (when (setq asscn (assoc cur-named-pk pks-list))
      (setcar (cdr asscn)
	      (pkb-update-options (nth 1 asscn)
				  (if (nth 2 one-named-pk)
				      (nth 2 one-named-pk)
				    (concat
				     (nth 1 one-named-pk)
				     " - "
				     (escape-html
				      (key-description cur-named-pk)))))))
    (unless (equal cur-named-pk [])
      (setq cur-named-pk-but-last (substring cur-named-pk 0 -1))
      (setq cur-named-pk-last (aref (substring cur-named-pk -1) 0))
      (when (and (setq asscn (assoc cur-named-pk-but-last pks-list))
		 (setq event-info
		       (pkb-int-find-event-defn
			(cddr asscn) cur-named-pk-last))
		 (keymapp (nth 1 event-info)))
	;; event-info is of form (MK-OPTIONS BIND . BIND-OPTIONS)
	(setcdr (cdr event-info)
		(pkb-update-options (cddr event-info)
				    (nth 1 one-named-pk)))))))

  ;; Match keymaps to pks.
  (let ((name-idx 0))
  (dolist (one-pks-list-elem pks-list)
    (let ((cur-pk (car one-pks-list-elem)))
    (unless (equal (car one-pks-list-elem) [])
      (let (;; (where-stored (pkb-key-sequence-location cur-pk))
	    (cur-pk-but-last (substring cur-pk 0 -1))
	    (cur-pk-last (aref (substring cur-pk -1) 0))
	    event-defn event to-add)
      ;; CUR-PK is the pk of ONE-PKS-LIST-ELEM
      ;; ;; WHERE-STORED, in the form (KEYMAP . EVENT), is where CUR-PK would
      ;; ;;  be stored in a tree of keymaps

      (setq event-defn
	    (cdr (pkb-int-find-event-defn
		  (cddr (assoc cur-pk-but-last pks-list))
		  cur-pk-last)))
      ;; EVENT-DEFN (if defined) is of the form (BIND . BIND-OPTIONS)
      (when event-defn
	(let ((anchor-name
	       (concat (escape-html "pk-")
		       (number-to-string name-idx))))
	(setcar (cdr one-pks-list-elem)
		(pkb-update-options
		 (nth 1 one-pks-list-elem)
		 (cons 'anchor-name anchor-name)))
	(setcdr event-defn
	 (pkb-html-int-update-event-desc
		 (car event-defn) (cdr event-defn) event-replace-bk
		 event-replace-regexp 'matched-replace))
	(let ((new-outp-str
	       (if (consp (cdr event-defn))
		   (cdr (assq 'outp-str (cdr event-defn)))
		 (cdr event-defn))))
	(setq new-outp-str
	      (concat "<a href=\"#" anchor-name "\">"
		      new-outp-str
		      "</a>"))
	(setcdr event-defn
		(pkb-update-options (cdr event-defn) new-outp-str)))
	(setq name-idx (1+ name-idx))))))
    ;; we now need to make sure CUR-PK is printed
    (unless (or (stringp (nth 1 one-pks-list-elem))
		(and (consp (nth 1 one-pks-list-elem))
		     (assq 'outp-str (nth 1 one-pks-list-elem))))
      (setcar (cdr one-pks-list-elem)
	      (pkb-update-options
	       (nth 1 one-pks-list-elem)
	       (if (equal cur-pk [])
		   "Root"
		 (escape-html (key-description cur-pk)))))))))

  (dolist (one-pk-w-binds pks-list)
    (setcdr (cdr one-pk-w-binds)
	    (pkb-categorize-key-list
	     (cddr one-pk-w-binds) max-mks-for-simple max-bks-for-compact
	     translate-events-list))
    (unless (cddr one-pk-w-binds)
      (delq one-pk-w-binds pks-list)))
  ;; PKS-LIST is now in its final format (described above below the `let'
  ;;  statement defining it)

  ;; sort PKS-LIST
  (setq pks-list
	(sort pks-list '(lambda (pk1 pk2) (pkb-compare-key-sequences
					   (car pk1) (car pk2)))))

  (dolist (cur-pk-catd pks-list)
    ;; process the different types (i.e. 'simple, 'compact', or 'full) of
    ;;  elements
    (cond
     ((eq (nth 2 cur-pk-catd) 'simple)
      (setcdr (nthcdr 2 cur-pk-catd)
	      (sort (cdr (nthcdr 2 cur-pk-catd))
		    (lambda (mk-w-bind-1 mk-w-bind-2)
		      (pkb-compare-modified-keys
		       (car mk-w-bind-1) (car mk-w-bind-2)))))
      (dolist (one-mk (cdr (nthcdr 2 cur-pk-catd)))
	;; (let ((to-prepend (escape-html (key-description (car cur-pk-catd)))))
	 ;; TO-PREPEND is the string describing the prefix-key to the event
	 ;;  to prepend to the event description
	(setcar (nthcdr 1 one-mk)
		(pkb-html-int-update-event-desc
		 (nth 0 one-mk) (nth 1 one-mk)
		 event-replace-bk event-replace-regexp 'matched-replace))
	(setcdr (nthcdr 2 one-mk)
		(pkb-html-int-update-bind-desc
		 (nth 2 one-mk) (cdr (nthcdr 2 one-mk)) 
		 bind-replace-obj bind-replace-regexp 'matched-replace))
	;; )
	))
     ((eq (nth 2 cur-pk-catd) 'compact)
      (setcdr (nthcdr 2 cur-pk-catd)
	      (sort (cdr (nthcdr 2 cur-pk-catd))
		    (lambda (bk1 bk2)
		      (pkb-compare-base-keys (car bk1) (car bk2)))))
      (pkb-html-int-pr-bks (cdr (nthcdr 2 cur-pk-catd)) 'matched-replace
			   event-replace-bk event-replace-regexp
			   bind-replace-obj bind-replace-regexp))
     ((eq (nth 2 cur-pk-catd) 'full)
      (let ((temp-full-data
	     (pkb-split-full-to-groups (nthcdr 3 cur-pk-catd) key-groups
				       percent-for-full-gr)))
	(pkb-html-int-pr-bks (append (nth 0 temp-full-data)
				     (nth 1 temp-full-data))
			     'matched-replace
			     event-replace-bk event-replace-regexp
			     bind-replace-obj bind-replace-regexp)
	(setcdr (nthcdr 2 cur-pk-catd) (cdr temp-full-data))))))
  
  ;; *Print keymap*

  ;; ** header of document
  (insert
   "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\"
   \"http://www.w3.org/TR/html4/strict.dtd\">
<html lang=\"en\">
<head>
<meta http-equiv=\"content-type\" content=\"text/html; charset=UTF-8\">\n"
;; <link rel=\"stylesheet\" type=\"text/css\" href=\"style.css\">
   pkb-html-css-header
   "\n<title>"
   (if title title "Key-bindings")
   "</title></head>\n"
   "<body>\n"
   (if title (concat "<h1>" title "</h1>\n") ""))

  (dolist (one-pk-w-binds pks-list)
    ;; ** Code between pks
    (unless (eq one-pk-w-binds (car pks-list))
      (insert "<hr>\n"))
    (cond
     ((eq (nth 2 one-pk-w-binds) 'full)
      (pkb-html-ins-full (nthcdr 3 one-pk-w-binds)
			 (nth 1 one-pk-w-binds)
			 max-bks-per-compact-row))
     ((eq (nth 2 one-pk-w-binds) 'compact)
      (pkb-html-ins-compact (cdr (nthcdr 2 one-pk-w-binds))
			    (nth 1 one-pk-w-binds)
			    max-bks-per-compact-row))
     ((eq (nth 2 one-pk-w-binds) 'simple)
      (pkb-html-ins-simple (cdr (nthcdr 2 one-pk-w-binds))
			   (nth 1 one-pk-w-binds)
			   simple-num-columns))
     (t (error "Should never get here"))))

  ;; Print legend
  ;; first, prepare legend
  (when matched-replace
    (let (leg-pairs)
    (dolist (rep-list
	     (list event-replace-bk event-replace-regexp
		   bind-replace-obj bind-replace-regexp))
      (dolist (pot-match rep-list)
	(let ((legend-entry (nth 2 pot-match)))
	  (when (and
		 (memq pot-match matched-replace)
		 (or (consp legend-entry) (null legend-entry)))
	    (add-to-list 'leg-pairs
			 ;; we have to format LEG-PAIRS for
			 ;;  `pkb-html-pr-pairs-columns'
			 (cond
			  ((consp legend-entry)
			   (append (list nil (car legend-entry))
				   (cons nil (cdr legend-entry))))
			  ((null legend-entry)
			   (append
			    (list nil (escape-html (prin1-to-string
						    (nth 0 pot-match))))
			    (cons nil (nth 1 pot-match)))))
			 t 'ignore)))))
    ;; LEG-PAIRS filled now
    (insert
     ;; ** header of legend
     "<hr>\n<div class=\"legend\">"
     (pkb-html-sect-head "Legend")

     (pkb-html-pr-pairs-columns leg-pairs legend-num-columns)

     ;; ** footer of legend
     "</div>")))

  ;; ** footer of document
  (insert "\n</body></html>")
  (setq buffer-file-coding-system
	(select-safe-coding-system 1 (buffer-end 1) 'utf-8))
  (save-buffer))))
)

(defun pkb-html-int-update-event-desc
  (item item-options
   event-replace-bk replace-regexp matched-replace-symb) ;; OK
  "Return an updated value for EVENT-OPTIONS to describe EVENT for output,
using the arguments EVENT-REPLACE-... and filling MATCHED-REPLACE.
EVENT-REPLACE-BK is similar to BIND-REPLACE-OBJ except that it matches and
replaces only the bk.

\(fn EVENT EVENT-OPTIONS EVENT-REPLACE-BK EVENT-REPLACE-REGEXP MATCHED-REPLACE-SYMB)"
  ;; Much of the code in this function and `pkb-html-int-update-bind-desc' are the
  ;;  same. There will be markers denoting the common and different regions,
  ;;  so they can be changed together.
  ;; <common-code-1>
  (let (replace-fl output-string)
  (cond ((stringp item-options)
	 (setq output-string item-options))
	((not item-options)
	 (setq replace-fl t))
	((setq output-string (cdr (assq 'outp-str item-options)))
	 (setq replace-fl (cdr (assq 'replace-fl item-options))))
	;; if no `outp-str' set, match unless `replace-fl' explicitly set
	(t
	 (let ((asscn (assq 'replace-fl item-options)))
	   (if asscn
	       (setq replace-fl (cdr asscn))
	     (setq replace-fl t)))))
  ;; convert item to string if necessary
  (unless output-string
    (when replace-fl
  ;; </common-code-1>
  ;; <different-region-1> Apply EVENT-REPLACE-BK
      ;; Match bk of ITEM to EVENT-REPLACE-BK
      (let* ((basic-type
	      (if (consp item)
		  item
		;; make sure that `event-basic-type' works
		(when (symbolp item)
		  (internal-event-symbol-parse-modifiers item))
		(event-basic-type item)))
	     (pot-match (assq basic-type event-replace-bk)))
	(when pot-match
	  (if (consp item)
	      (setq output-string (nth 1 pot-match))
	    (let ((mod-pref
		   (substring
		    (prin1-to-string item) 0
		    (- (length (prin1-to-string basic-type))))))
	     ;; MOD-PREF is a string describing the modifiers on ITEM,
	     ;;  e.g. if item = ?\C-\M-a, MOD-PREF = "C-M-"
	    (setq output-string
		  (concat (escape-html mod-pref) (nth 1 pot-match)))))
	  (let ((legend-entry (nth 2 pot-match)))
	    ;; add a legend unless we don't need one
	    (when (or (consp (nth 2 pot-match)) (null (nth 2 pot-match)))
	      (add-to-list matched-replace-symb pot-match)))
	  (setq replace-fl (not (nth 3 pot-match)))))
  ;; </different-region-1>
  ;; <common-code-2>
      ))
  ;; if still no string, print ITEM
  (unless output-string 
    (setq output-string
	  (escape-html 
  ;; </common-code-2>
  ;; <different-region-2> Print ITEM as a string
	   (if (and (consp item)
		    (integerp (car item))
		    (integerp (cdr item)))
	       ;; if ITEM is a char-range
	       (concat (key-description (vector (car item)))
		       " ... "
		       (key-description (vector (cdr item))))
	     ;; else ITEM is not a char-range
	     (key-description (vector item)))
  ;; </different-region-2>
  ;; <common-code-3>
	   )))
  ;; OUTPUT-STRING is set at this point
  (when replace-fl
    (catch 'quit
    (dolist (pot-match replace-regexp)
      (when (string-match (nth 0 pot-match) output-string)
	(setq output-string
		(replace-regexp-in-string
		 (nth 0 pot-match) (nth 1 pot-match) output-string))
	(let ((legend-entry (nth 2 pot-match)))
	(when (or (consp (nth 2 pot-match)) (null (nth 2 pot-match)))
	  (add-to-list matched-replace-symb pot-match)))
	(when (nth 3 pot-match)
	 ;; when NO-CNT
	  (throw 'quit nil))))))
  ;; OUTPUT-STRING is ready to be stored in ITEM-OPTIONS
  (setq item-options
	(pkb-update-options item-options output-string))
  (when (consp item-options)
    (setq item-options (delq (assq 'replace-fl item-options) item-options)))
  ;; </common-code-3>
  )
  item-options
)

(defun pkb-html-int-update-bind-desc
  (item item-options
   bind-replace-obj replace-regexp matched-replace-symb) ;; OK
  "Return an updated value for BIND-OPTIONS to describe BIND for output,
using the arguments BIND-REPLACE-... and filling MATCHED-REPLACE. 

\(fn (bind bind-options bind-replace-obj bind-replace-regexp matched-replace-symb)"
  ;; Much of the code in this function and `pkb-html-int-update-event-desc' are the
  ;;  same. There will be markers denoting the common and different regions,
  ;;  so they can be changed together.
  ;; <common-code-1>
  (let (replace-fl output-string)
  (cond ((stringp item-options)
	 (setq output-string item-options))
	((not item-options)
	 (setq replace-fl t))
	((setq output-string (cdr (assq 'outp-str item-options)))
	 (setq replace-fl (cdr (assq 'replace-fl item-options))))
	;; if no `outp-str' set, match unless `replace-fl' explicitly set
	(t
	 (let ((asscn (assq 'replace-fl item-options)))
	   (if asscn
	       (setq replace-fl (cdr asscn))
	     (setq replace-fl t)))))
  ;; convert item to string if necessary
  (unless output-string
    (when replace-fl
  ;; </common-code-1>
  ;; <different-region-1> Apply BIND-REPLACE-OBJ
      ;; Match ITEM to BIND-REPLACE-OBJ
      (let ((pot-match (assq item bind-replace-obj)))
	(when pot-match
	  ;; when item has an entry in REPLACE-OBJ
	  (setq output-string (nth 1 pot-match))
	  (let ((legend-entry (nth 2 pot-match)))
	    ;; add a legend unless we don't need one
	    (when (or (consp (nth 2 pot-match)) (null (nth 2 pot-match)))
	      (add-to-list matched-replace-symb pot-match)))
	  (setq replace-fl (not (nth 3 pot-match)))))
  ;; </different-region-1>
  ;; <common-code-2>
      ))
  ;; if still no string, print ITEM
  (unless output-string 
    (setq output-string
	  (escape-html 
  ;; </common-code-2>
  ;; <different-region-2> Print ITEM as a string
	   (cond
	    ((null item) "")
	    ((and (consp item) (eq (car item) 'keymap))
	     "keymap")
	    (t (prin1-to-string item)))
  ;; </different-region-2>
  ;; <common-code-3>
	   )))
  ;; OUTPUT-STRING is set at this point
  (when replace-fl
    (catch 'quit
    (dolist (pot-match replace-regexp)
      (when (string-match (nth 0 pot-match) output-string)
	(setq output-string
		(replace-regexp-in-string
		 (nth 0 pot-match) (nth 1 pot-match) output-string))
	(let ((legend-entry (nth 2 pot-match)))
	(when (or (consp (nth 2 pot-match)) (null (nth 2 pot-match)))
	  (add-to-list matched-replace-symb pot-match)))
	(when (nth 3 pot-match)
	 ;; when NO-CNT
	  (throw 'quit nil))))))
  ;; OUTPUT-STRING is ready to be stored in ITEM-OPTIONS
  (setq item-options
	(pkb-update-options item-options output-string))
  (when (consp item-options)
    (setq item-options (delq (assq 'replace-fl item-options) item-options)))
  ;; </common-code-3>
  ;; is item a keymap?
  (when (keymapp item)
    (let ((class-list
	   (when (consp item-options)
	     (cdr (assq 'class-list item-options)))))
      (add-to-list 'class-list "keymap")
      (setq item-options
	    (pkb-update-options item-options (cons 'class-list class-list)))
      )))
  item-options
)

(defun pkb-html-int-pr-bks
  (bks-w-binds matched-replace-symb
   event-replace-bk event-replace-regexp bind-replace-obj bind-replace-regexp)
  ;; OK
  "Prepare BKS-W-BINDS for output, using ...-REPLACE-... (as described in
`pkb-html-save-keymap') and storing matches in MATCHED-REPLACE-SYMB."
  (dolist (one-bk-w-binds bks-w-binds)
    (setcar (cdr one-bk-w-binds)
	    (pkb-html-int-update-event-desc
	     (nth 0 one-bk-w-binds) (nth 1 one-bk-w-binds)
	     event-replace-bk event-replace-regexp 'matched-replace))
    (dolist (mods-type-w-binds (nthcdr 2 one-bk-w-binds))
      (setcdr (cdr mods-type-w-binds)
	      (pkb-html-int-update-bind-desc
	       (cadr mods-type-w-binds) (cddr mods-type-w-binds)
	       bind-replace-obj bind-replace-regexp 'matched-replace))))
  bks-w-binds
)

(provide 'pkb-html)
