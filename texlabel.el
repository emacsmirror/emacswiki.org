;;; texlabel.el --- Auto insertion of equation labels in TeX

;; Copyright (C) 2012 tama.sh

;; Author: tama.sh <tama.csh@gmail.com>
;; Version: 1.1.0
;; Keywords: TeX

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Installation
;;
;; Put texlabel.el to your load-path.
;; Add the following command to your ~/.emacs startup file.
;;
;; (require 'texlabel)

;;; Command List
;;
;; `texlabel-auto-labeling'
;;    Insert equation labels automatically (need to input prefix)
;;
;; `texlabel-auto-labeling-default'
;;    Insert equation labels automatically (use default prefix)
;;
;; `texlabel-auto-labeling-force'
;;    Insert equation labels automatically (use default prefix and force to rename all labels)
;;
;; `texlabel-rename-prefix'
;;    Rename prefix for equation labels
;;
;; `texlabel-change-default-prefix'
;;    Change default prefix
;;
;; `texlabel-copy'
;;    Copy text without labels


;;; Customize
;;
;;  * M-x customize-group RET texlabel RET
;;

;;; Change Log:
;;
;; * 2012/02/22
;;     First released
;; * 2012/04/18
;;     Correct the regexp for searching environments
;; * 2012/05/25
;;     Fix the bug when inserting labels in lines with comments
;; * 2012/12/27
;;     Fix the bug that the separator "\\" in a paren is also recognized.
;; * 2012/12/28
;;     Fix the bug that the program recognizes the label in a comment line.
;; * 2012/01/07
;;     Add `texlabel-copy' function

;;; Code:

;;; Customize variables:

(defgroup texlabel nil
  "TeX label insertion support."
  :group 'tex
  :prefix "texlabel-" )

(defcustom texlabel-math-env-list '("equation" "eqnarray" "align" "gather"
				    "multline" "alignat" "xalignat" "xxalignat"
				    "flalign")
  "*List of environments where labels are used."
  :type '(repeat string)
  :group 'texlabel )

(defcustom texlabel-non-sep-math-env-list '("multline")
  "*List of environments where line breaks do not create new labels."
  :type '(repeat string)
  :group 'texlabel )

(defcustom texlabel-refcmd-list '("ref" "eqref")
  "*List of reference commands."
  :type '(repeat string)
  :group 'texlabel)

(defcustom texlabel-notag-list '("\\notag" "\\nonumber")
  "*List of commands which indicate no label."
  :type '(repeat string)
  :group 'texlabel )

(defcustom texlabel-line-sep-list '("\\\\")
  "*List of line break commands in math environment."
  :type '(repeat string)
  :group 'texlabel)

(defcustom texlabel-pre-label-string "  "
  "*String inserted before labels."
  :type 'string
  :group 'texlabel)

;;; Buffer local variables

(defcustom texlabel-prefix "eq"
  "*Prefix for equation labels."
  :type 'string
  :group 'texlabel)
(make-variable-buffer-local 'texlabel-prefix)
(put 'texlabel-prefix 'safe-local-variable 'stringp)

(defvar texlabel-offset 1
  "*Label number offset.")
(make-variable-buffer-local 'texlabel-offset)
(put 'texlabel-offset 'safe-local-variable 'numberp)

;;; Functions:

;; regexp:

(defun texlabel-make-tex-cmd (cmd label)
  "Make a tex command from CMD and LABEL. The output is \\CMD{LABEL}."
  (concat "\\" cmd "{" label "}"))

(defun texlabel-make-or-regexp (regexp-list)
  "Make a regexp which matches all regexps in REGEXP-LIST."
  (concat "\\(" (mapconcat 'identity regexp-list "\\|") "\\)") )

(defun texlabel-make-tex-cmd-regexp (cmd label &optional cmd-with-backslash)
  "Make a regexp which matches CMD commands with LABEL."
  (if (consp cmd)
      (setq cmd (texlabel-make-or-regexp cmd)) )
  (if (consp label)
      (setq label (texlabel-make-or-regexp label)) )
  (concat "\\\\" cmd "{" label "}") )

(defun texlabel-cmd-list-to-regexp (cmd-list)
  "Make a regexp which matches all commands in CMD-LIST."
  (texlabel-make-or-regexp (mapcar 'regexp-quote cmd-list)) )

(defun texlabel-re-search-forward-cmd (regexp &optional bound noerror count)
  "Search tex commands by regexp."
  (and (re-search-forward (concat regexp "\\W") (and bound (1+ bound)) noerror count)
       (progn (forward-char -1) (point)) ))

;; parser:

(defun texlabel-in-paren-p (point &optional start)
  "Check whether the POINT is in a paren."
  (save-excursion
    (let ((parse-rst (parse-partial-sexp (or start (line-beginning-position)) point)))
      (if (nth 0 parse-rst)
	(nth 1 parse-rst) ))))

(defun texlabel-in-comment-p (point)
  "Check whether the POINT is in a comment, and return the beginning of the comment."
  (save-excursion
    (let ((parse-rst (parse-partial-sexp (line-beginning-position) point)))
      (if (nth 4 parse-rst)
	(nth 8 parse-rst) ))))

(defun texlabel-make-env-cmd-stack (region)
  "Make a stack of environment commands."
  (let (env-cmd-stack
	(begin-region (car region))
	(end-region (cdr region))
	(env-cmd-regexp
	 (texlabel-make-tex-cmd-regexp '("begin" "end") '(".+?")) )
	(case-fold-search nil) )
    (save-excursion
      (goto-char begin-region)
      (while (re-search-forward env-cmd-regexp end-region t)
	(unless (texlabel-in-comment-p (point))
	  (let* ((begin-or-end (match-string-no-properties 1))
		 (env-name (match-string-no-properties 2))
		 (env-point (if (string= begin-or-end "begin") (match-beginning 0)
			      (match-end 0) )))
	    (setq env-cmd-stack
		  (cons (list begin-or-end env-name env-point) env-cmd-stack)) )))
      (nreverse env-cmd-stack) )))

(defun texlabel-parse-env-1 ()
  (let (rst)
    (while (string= (nth 0 (car input-stack)) "begin")
      (setq tmp-stack (cons (car input-stack) tmp-stack))
      (setq input-stack (cdr input-stack))
      (setq rst (cons (texlabel-parse-env-1) rst)))
    (if input-stack
	(if (string= (nth 1 (car input-stack)) (nth 1 (car tmp-stack)))
	    (let ((env-name (nth 1 (car input-stack)))
		  (begin (nth 2 (car tmp-stack)))
		  (end (nth 2 (car input-stack))) )
	      (setq input-stack (cdr input-stack))
	      (setq tmp-stack (cdr tmp-stack))
	      (cons env-name (cons (cons begin end) (nreverse rst))) )
	  (error "L.%d: Can not find the corresponding end command."
		 (line-number-at-pos (nth 2 (car tmp-stack)))) )
      (nreverse rst) )))

(defun texlabel-parse-env (region)
  "Parse environments. Return an environment list whose elements have the form (env-name env-region inner-env-list)."
  (let ((input-stack (texlabel-make-env-cmd-stack region)) tmp-stack)
    (texlabel-parse-env-1) ))

(defun texlabel-collect-key-rec-1 (regexp list)
  (if (and (stringp (car list)) (string-match regexp (car list)))
      (setq matched-list (cons list  matched-list)) )
  (if (consp (cddr list))
      (dolist (elm (cddr list))
	(texlabel-collect-key-rec-1 regexp elm) )))

(defun texlabel-collect-key-rec (regexp list)
  "Collect keys from LIST recursively by REGEXP."
  (let (matched-list)
    (dolist (elm list)
      (texlabel-collect-key-rec-1 regexp elm) )
    (nreverse matched-list) ))

(defun texlabel-search-env (env-list region)
  "Search environments and return a list of labeled regions.
This function ignores environments in comment lines."
    (texlabel-collect-key-rec (concat "^" (texlabel-make-or-regexp env-list) "$")
			      (texlabel-parse-env region)))

(defun texlabel-search-math-env (region)
  "Search math enviroments indicated by `texlabel-math-env-list'. Return a list of labeled regions."
  (texlabel-search-env texlabel-math-env-list region) )

(defun texlabel-in-region-p (point region)
  "Return t if the POINT is in the REGION."
  (and (>= point (car region)) (< point (cdr region))) )

(defun texlabel-in-region-list-p (point region-list)
  "Return t if the POINT is in the region that contained in REGION-LIST."
  (catch 'found
    (dolist (region region-list)
      (if (texlabel-in-region-p point region)
	  (throw 'found t) ))))

(defun texlabel-trim-begin-end (region)
  "Cut begin and end from the REGION and return the trimed region."
  (let ((begin-region (car region))
	(end-region (cdr region))
	(begin-env-regexp (texlabel-make-tex-cmd-regexp "begin" '(".+?")))
	(end-env-regexp (texlabel-make-tex-cmd-regexp "end" '(".+?"))) )
    (save-excursion
      (goto-char begin-region)
      (if (and (re-search-forward begin-env-regexp end-region t)
	       (= (match-beginning 0) begin-region) )
	  (setq begin-region (match-end 0)) )
      (goto-char end-region)
      (if (and (re-search-backward end-env-regexp begin-region t)
	       (= (match-end 0) end-region) )
	  (setq end-region (match-beginning 0)) )
      (cons begin-region end-region) )))

(defun texlabel-make-inner-region-list (env-obj)
  "Make a list of inner regions from ENV-OBJ."
  (let (rst)
    (dolist (inner-obj (cddr env-obj) rst)
      (setq rst (cons (cadr inner-obj) rst)))))

(defun texlabel-separate-env (sep-regexp env-obj)
  "Separate an environment region by separaters SEP-REGEXP.
ENV-OBJ has the form (env-name env-region inner-env-list).
This function igonres separaters in inner environments and in comment lines."
  (let* ((inner-region-list (texlabel-make-inner-region-list env-obj))
	 (region (texlabel-trim-begin-end (cadr env-obj)))
	 (begin-region (car region))
	 (end-region (cdr region))
	 (begin-sub-region begin-region)
	 end-sub-region
	 sub-region-list
	 (case-fold-search nil))
    (save-excursion
      (goto-char begin-region)
      (while (texlabel-re-search-forward-cmd sep-regexp end-region t)
	(if (and (not (texlabel-in-paren-p (point) begin-region))
	         (not (texlabel-in-comment-p (point)))
		 (not (texlabel-in-region-list-p (point) inner-region-list)))
	    (progn
	      (setq end-sub-region (point))
	      (setq sub-region-list (cons (cons begin-sub-region end-sub-region) sub-region-list))
	      (setq begin-sub-region (point)) )))
      (setq end-sub-region end-region)
      (setq sub-region-list (cons (cons begin-sub-region end-sub-region) sub-region-list)))
    (nreverse sub-region-list) ))

(defun texlabel-separate-math-env (env-obj)
  "Separate math env region by `texlabel-line-sep-list'."
  (let ((sep-regexp (texlabel-cmd-list-to-regexp texlabel-line-sep-list)))
    (texlabel-separate-env sep-regexp env-obj)))

(defun texlabel-exclude-region-by-regexp (cmd-regexp region-list)
  "Exclude regions that contain CMD-REGEXP from REGION-LITS."
  (let (rst (case-fold-search nil))
    (save-excursion
      (dolist (region region-list (nreverse rst))
	(let ((begin-region (car region))
	      (end-region (cdr region))
	      (exclude-flag nil))
	  (goto-char begin-region)
	  (while (and (not exclude-flag)
		      (texlabel-re-search-forward-cmd cmd-regexp end-region t))
	    (unless (texlabel-in-comment-p (point))
	      (setq exclude-flag t)))
	  (unless exclude-flag
	    (setq rst (cons region rst))))))))

(defun texlabel-exclude-notag-region (region-list)
  "Exclude notag regions from REGION-LITS according to the `texlabel-notag-list'."
  (let ((exclude-regexp (texlabel-cmd-list-to-regexp texlabel-notag-list)))
    (texlabel-exclude-region-by-regexp exclude-regexp region-list)))

(defun texlabel-adjust-region (sep-regexp region)
 "Adjust the REGION not to include separators.
Separators are assumed to be the end of regions.
This function also removes line breaks and spaces
from the beginning and end of the region."
 (let* ((begin-region (car region))
	(end-region (cdr region))
	(case-fold-search nil))
   (save-excursion
     (goto-char begin-region)
     (if (looking-at "[ \t]*\n")
	 (goto-char (match-end 0)))
     (if (> (point) end-region)
	 (goto-char end-region))
     (setq begin-region (point))
     (goto-char end-region)
     (if (and (re-search-backward sep-regexp begin-region t)
	      (= (match-end 0) end-region))
	 (goto-char (match-beginning 0))
       (goto-char end-region))
     (skip-chars-backward " \t\n")
     (if (< (point) begin-region)
	 (goto-char begin-region))
     (setq end-region (point))
   (cons begin-region end-region))))

(defun texlabel-adjust-region-list (sep-regexp region-list)
  (mapcar '(lambda (region) (texlabel-adjust-region sep-regexp region)) region-list) )

(defun texlabel-adjust-eq-region-list (region-list)
  (let ((sep-regexp (texlabel-cmd-list-to-regexp texlabel-line-sep-list)))
    (texlabel-adjust-region-list sep-regexp region-list)))

(defun texlabel-collect-eq-region (region)
  "Collect equation regions as a list of dot pairs."
  (let ((env-obj-list (texlabel-search-math-env region))
	eq-region-list)
    (dolist (env-obj env-obj-list)
      (if (member (car env-obj) texlabel-non-sep-math-env-list) ;; non-sep env
	  (setq eq-region-list
		(nconc eq-region-list (list (texlabel-trim-begin-end (cadr env-obj)))))
	(setq eq-region-list
	      (nconc eq-region-list (texlabel-separate-math-env env-obj)))))

    (texlabel-adjust-eq-region-list (texlabel-exclude-notag-region eq-region-list))))

(defun texlabel-make-label-point-list (region-list)
  "Make a list that contains (label . point) pairs."
  (let (label-point-list
	(case-fold-search nil))
    (save-excursion
      (dolist (region region-list (nreverse label-point-list))
   	(let ((begin-region (car region))
	      (end-region (cdr region))
	      (label-cmd-regexp (texlabel-make-tex-cmd-regexp "label" '(".*?")))
	      label point)
	  (goto-char begin-region)
	  (if (and (re-search-forward label-cmd-regexp end-region t)
		   (not (texlabel-in-comment-p (point))))
	      (progn
		(setq label (match-string-no-properties 1))
		(setq point (match-beginning 0)) )
	    (setq point end-region) )
	  (setq label-point-list (cons (cons label point) label-point-list)) )))))

(defun texlabel-collect-eq-label-point (region)
  (texlabel-make-label-point-list (texlabel-collect-eq-region region)))

(defun texlabel-make-marker (point)
  (save-excursion
    (goto-char point)
    (point-marker)))

(defun texlabel-make-label-marker-list (label-point-list)
  "Make a list of label and marker pairs."
  (mapcar '(lambda (label-point)
	     (cons (car label-point) (texlabel-make-marker (cdr label-point))) )
	  label-point-list ))

(defun texlabel-collect-eq-label-marker (region)
  (texlabel-make-label-marker-list (texlabel-collect-eq-label-point region)))

;; utility:

(defun texlabel-trim-whitespace (str)
  (if (string-match "^[ \t]*" str)
      (setq str (replace-match "" nil nil str)))
  (if (string-match "[ \t]*$" str)
      (setq str (replace-match "" nil nil str))))

(defun texlabel-insert-label (label-name &optional point)
  "Insert a label that has LABEL-NAME."
  (save-excursion
    (if point
	(goto-char point))
    (delete-horizontal-space)
    (insert (concat texlabel-pre-label-string
		    (texlabel-make-tex-cmd "label" label-name)))))

;; reference:

(defun texlabel-rename-reference-1 (refcmd rename-label-list)
  "Rename references from old-list to new-list.
Labels in the REFCMD commands are replaced to new labels.
RENAME-LABEL-LIST is a list of dot pairs (old-label . new-label)."
  (let* ((old-label-list (mapcar 'car rename-label-list))
	 (old-ref-regexp (texlabel-make-tex-cmd-regexp refcmd old-label-list t))
	 (case-fold-search nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward old-ref-regexp nil t)
	(let* ((old-label (match-string-no-properties 1))
	       (new-label (cdr (assoc old-label rename-label-list))))
	  (replace-match new-label t nil nil 1))))))  ; replace substr only

(defun texlabel-rename-reference (rename-label-list)
  "Rename all kinds of refereneces indicated by the `texlabel-refcmd-list'."
  (if rename-label-list
      (dolist (refcmd texlabel-refcmd-list)
	(texlabel-rename-reference-1 refcmd rename-label-list) )))

;; label conflict check:

(defun texlabel-count-key (key alist)
  (let ((counter 0))
    (while alist
      (if (equal key (caar alist))
	  (setq counter (1+ counter)))
      (setq alist (cdr alist)) )
    counter))

(defun texlabel-assoc-all (key alist)
  (let ((val-list))
    (while alist
      (if (equal key (caar alist))
	  (setq val-list (cons (cdar alist) val-list)))
      (setq alist (cdr alist)) )
    (if val-list
	(cons key (nreverse val-list)))))

(defun texlabel-check-key-conflict (alist)
  (let (key-list conflict-list)
    ;; make key list
    (dolist (elm alist)
      (let ((key (car elm)))
	(if (and key (not (member key key-list)))
	    (setq key-list (cons key key-list)))))
    (setq key-list (nreverse key-list))
    ;; make conflict alist
    (dolist (key key-list (nreverse conflict-list))
      (if (> (texlabel-count-key key alist) 1)
	  (setq conflict-list
		(cons (texlabel-assoc-all key alist) conflict-list)) ))))

(defun texlabel-collect-existing-label-point (region)
  (let ((begin-region (car region))
	(end-region (cdr region))
	(label-cmd-regexp (texlabel-make-tex-cmd-regexp "label" '(".*?")))
	label-point-list)
    (goto-char begin-region)
    (while (and (re-search-forward label-cmd-regexp end-region t)
		(not (texlabel-in-comment-p (point))) )
      (setq label-point-list
	    (cons (cons (match-string-no-properties 1) (match-beginning 0))
		  label-point-list )))
    (nreverse label-point-list) ))

(defun texlabel-check-label-conflict ()
  (save-excursion
    (widen)
    (texlabel-check-key-conflict
     (texlabel-collect-existing-label-point (cons (point-min) (point-max))) )))

(defun texlabel-make-and-str (str-list)
  (if (cdr str-list)
      (if (cddr str-list)
	  (concat (car str-list) ", " (texlabel-make-and-str (cdr str-list)))
	(concat (car str-list) " and " (texlabel-make-and-str (cdr str-list))))
    (car str-list)))

(defun texlabel-warn-label-conflict-if-exist ()
  (interactive)
  (let ((conflict-list-1 (car (texlabel-check-label-conflict))) str)
    (if conflict-list-1
	(progn
	  (setq str (texlabel-make-and-str
		     (mapcar '(lambda (pos) (concat "L." (number-to-string (line-number-at-pos pos))))
			     (cdr conflict-list-1) )))
	  (setq str (concat "Label conflict `" (car conflict-list-1) "' in " str))
	  (message str)
	  t )
      nil )))

;; main:

(defun texlabel-auto-labeling (prefix &optional force)
  "Insert equation labels automatically.
This function also renames labels represented by PREFIX + number.
It also modifies related references."
  (interactive
   (list (read-string
	  (concat "Input label prefix (default : " texlabel-prefix "): "))))
  (setq prefix (texlabel-trim-whitespace prefix))
  (unless (string= prefix "") (setq texlabel-prefix prefix))

  (save-restriction
    (widen)
    (unless (texlabel-warn-label-conflict-if-exist)
      (let ((label-marker-list (texlabel-collect-eq-label-marker (cons (point-min) (point-max))))
	    (counter texlabel-offset)
	    (regular-label-regexp (concat "^" texlabel-prefix ":[0-9]+$"))
	    (label-cmd-regexp (texlabel-make-tex-cmd-regexp "label" '(".*?")))
	    rename-label-list
	    (case-fold-search nil))
	(if force (setq regular-label-regexp ".*"))
	(save-excursion
	  (dolist (label-marker label-marker-list)
	    (let ((label (car label-marker))
		  (marker (cdr label-marker))
		  (new-label (concat texlabel-prefix
				     ":" (number-to-string counter))))
	      (if label
		  (unless (string= label new-label)
		    (if (string-match regular-label-regexp label)
			(progn
			  (goto-char marker)
			  (looking-at label-cmd-regexp)
			  (replace-match new-label t nil nil 1)
			  (setq rename-label-list
				(cons (cons label new-label) rename-label-list) ))))
		;; if label is nil
		(goto-char marker)
		(texlabel-insert-label new-label (texlabel-in-comment-p (point))) )
	      (setq counter (1+ counter))
	      (set-marker marker nil) )))
	;; rename reference accoding to rename referenc list
	(texlabel-rename-reference (nreverse rename-label-list))
	;; update reftex reference
	(if (and (boundp 'reftex-mode) reftex-mode) (reftex-reset-mode)) ))))

(defun texlabel-auto-labeling-default ()
  "Insert equation labels automatically using the default prefix `texlabel-prefix'."
  (interactive)
  (texlabel-auto-labeling texlabel-prefix))

(defun texlabel-auto-labeling-force ()
  "Insert equation labels automatically using the default prefix `texlabel-prefix'.
This function rename all labels to regular expressions."
  (interactive)
  (texlabel-auto-labeling texlabel-prefix t))

(defun texlabel-rename-prefix-1 (cmd-name old-prefix new-prefix)
  (let ((label-regexp (concat (regexp-quote cmd-name) "{\\(" old-prefix "\\):.*?}")))
    (save-restriction
      (widen)
      (save-excursion
	(goto-char (point-min))
	(while (re-search-forward label-regexp nil t)
	  (replace-match new-prefix t nil nil 1))))))

(defun texlabel-rename-prefix (old-prefix new-prefix)
  "Rename label prefix."
  (interactive (list (read-string "Input old prefix: ")
		     (read-string "Input new prefix: ")))
  (setq old-prefix (texlabel-trim-whitespace old-prefix))
  (setq new-prefix (texlabel-trim-whitespace new-prefix))

  (let ((cmd-list (cons "\\label" texlabel-refcmd-list)))
    (dolist (cmd-name cmd-list)
      (texlabel-rename-prefix-1 cmd-name old-prefix new-prefix)))
  (if (and (boundp 'reftex-mode) reftex-mode) (reftex-reset-mode)))

(defun texlabel-change-default-prefix (new-prefix)
  "Change default label prefix."
  (interactive (list (read-string "Input new prefix: ")))
  (setq new-prefix (texlabel-trim-whitespace new-prefix))
  (setq texlabel-prefix new-prefix))


;; modifying copy

(defun texlabel-copy (beg end)
  "Copy the text and delete the labels in it."
  (interactive "r")
  (kill-ring-save beg end)
  (setcar kill-ring
   (replace-regexp-in-string "[ \t\n]*\\\\label{.*?}[ \t]*"
			    ""
			    (car kill-ring))))


(provide 'texlabel)

;;; texlabel.el ends here
