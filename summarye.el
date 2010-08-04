;;; summarye.el --- list up matched strings from a buffer, and display them in summary buffer
;;; -*-emacs-lisp-*- -*- coding:iso-2022-7bit; -*-
(defconst se/version "summary-edit Version 2.5.2a")
;; Time-stamp: <narazaki@xingu.cs.cis.nagasaki-u.ac.jp 2005-07-25T09:28:12>
;; Author: narazaki@cs.cis.nagasaki-u.ac.jp
;; Home Page: http://www.cs.cis.nagasaki-u.ac.jp/~narazaki/index.cgi/resources/summarye.html

;; This file is part of GNU Emacs.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

;; Commentary:
;  Todo:
;; CHANGE LOG
;; Ver 2.5.2 [2005-07-25T09:28:12.00+9:00]
;;   use histories in selecting category/regexp
;;   fix a typo in se/show-cluster
;;   use isearch-lazy-highlight-face in soccur
;; Ver 2.5.1
;; narazaki@cs.cis.nagasaki-u.ac.jp [2005-06-30T14:30:47.00+9:00]
;;   few tweaks for reducing compile errors
;;   replace string-to-int with string-to-number
;;   mock up a regexp for python-mode
;;   use Japanese menu, popup-menu if possible and appropriate
;;   fix text-property bug
;; rubikitch@ruby-lang.org [2005-06-30T13:08:07.00+9:00]
;;   add soccur
;; Ver 2.4.1 
;; rubikitch@ruby-lang.org [2005-06-29T11:47:11.00+9:00]
;;  - use window-width instead of frame-width
;;  - add rd-mode, yatex-mode and ruby-*-mode to mode-delimiter-alist
;; narazaki@Inetq.or.jp			Fri Jan 17 13:07:29 1997
;;  prompt for regexp smartly(Ver 2.3)
;; narazaki@csce.kyushu-u.ac.jp		Thu May 11 00:42:38 1995
;;  not always display item at the top of window(Ver 2.2)
;;  add backward scroll function(Ver 2.1)
;; narazaki@csce.kyushu-u.ac.jp		Sat Apr  8 11:00:51 1995
;;  Ver 2.0
;; narazaki@csce.kyushu-u.ac.jp 	Wed Nov 10 16:53:35 1993
;;  Ver 1.0
;; narazaki@hazumi			Sun Nov  8 05:14:49 1992
;;  Ver 0.1

;;; Commentary:

;; Usage:
;; 1. At first set autoload function in your .emacs like this:
;;    (autoload 'se/make-summary-buffer "summarye" nil t)
;;    (autoload 'soccur "summarye" nil t)
;; 2. [Optional] bind se/make-summary-buffer to your favorite key
;; sequence(or menu)
;;    like the following:
;;    (define-key help-map "M" 'se/make-summary-buffer)
;;    NOTE: You can use summarye from menu (Tools->Make summary) or
;;    M-x se/make-summary-buffer 
;; 3. Invoke it. You will get the summary buffer of current buffer. You
;;    will use it easily, I think.
;; 4. If you want to specify the item pattern, set the value to buffer-local
;;    variable se/item-delimiter-regexp like the following examples. The
;;    value must be either a regular expression string or a list of a list
;;    of a tag string and a regexp string. See examples.
;; 5. And if you want to specify the displayed string in summary buffer,
;;    assign a function to buffer-local variable
;;    se/item-name-constructor-function.

;; Coding memo:
;; * While cluster is an internal structure which index starts from
;;   zero, item means objects user can view like a displayed line or
;;   the corresponding text. Thus every commands do not have cluster
;;   in their names.
;; * In this program, term `face' is used. But it means not face but overlay.
;;; Code:

(message se/version)(sit-for 0)

(eval-and-compile
  (require 'cl)
  (or (fboundp 'cl-gethash) (load "cl-extra"))
  (defvar summary-edit-mode-menu-bindings)
  (defvar se/shortcut-menu-list)
  (require 'which-func)
  (cond ((featurep 'mule)
	 (defalias 'se/string-display-width 'string-width))
	(t 
	 (defun se/string-display-width (str) (length str)))))

;;; requires common lisp package
;;; This file uses: dotimes, push, pop, when, unless.
(require 'cl)

(defvar summary-edit-mode-map (make-sparse-keymap))

;;; requires common lisp extra package
;; they are used in se/only-once
(or (fboundp 'hash-table-p) (autoload 'hash-table-p "cl-extra" nil nil))
(or (fboundp 'cl-gethash) (autoload 'cl-gethash "cl-extra" nil nil))
(or (fboundp 'cl-puthash) (autoload 'cl-puthash "cl-extra" nil nil))
(if (fboundp 'remove-regexp-in-string)
    (defalias 'se/remove-regexp-in-string 'remove-regexp-in-string)
  (defun se/remove-regexp-in-string (regexp string)
    (cond ((not (string-match regexp string))
	   string)
	  (t (let ((str nil)
		   (ostart 0)
		   (oend (match-beginning 0))
		   (nstart (match-end 0)))
	       (setq str (concat str (substring string ostart oend)))
	       (while (string-match regexp string nstart)
		 (setq ostart nstart)
		 (setq oend (match-beginning 0))
		 (setq nstart (match-end 0))
		 (setq str (concat str (substring string ostart oend))))
	       (concat str (substring string nstart)))))))

(defvar se/incremental-compile-p t
  "If non-nil, each non-compiled formatter is compiled at its
first use.")
(defvar se/display-flush-time 300
  "Time to display overlay temporally. The unit is milisecond.")
(defvar se/*tmp*
  "Bound in se/make-summary-buffer. The first value is nil.
se/only-once bounds it to a hash-table.")
(defvar se/mode-delimiter-alist
  '(;; algol family
    ((ada-mode pascal-mode) "\\(procedure\\|function\\) \\([^ {]+\\)"
     (lambda (beg end category) (se/matched-pattern 1)))
    ((c-mode c++-mode cc-mode)
     "^\\(\\*?[A-Za-z_].*(.*\\)$"
     (lambda (beg end category)
       (let ((name (buffer-substring-no-properties beg end)))
	 (if (string-match "\\([^ 	(]+\\)[ 	]*(.*$" name)
	     (substring name (match-beginning 1) (match-end 0))
	   name))))
    (html-mode
     "^<[Hh][1-6][^>]*>"
     (lambda (beg end category)
       (let ((size (- (char-after (+ beg 2)) ?0)))
	 (if (re-search-forward "</[Hh]" nil t)
	     (concat (make-string (* 2 size) ?\  )
		     (se/remove-regexp-in-string
		      "[\t\n]"
		      (buffer-substring-no-properties end (match-beginning 0))))))))
    ;; (la)tex family
    (bibtex-mode "^@[^{s][^{]*{\\([^,]+\\),$")
    (latex-mode
     "^[ 	]*\\(\\\\chapter\\*?{\\(.*\\)\\|\\\\section\\*?{\\(.*\\)\\|\\\\subsection\\*?{\\(.*\\)\\|\\\\subsubsection\\*?{\\(.*\\)\\|\\\\paragraph{\\(.*\\)$\\)"
     (lambda (beg end category)
       (se/set-face (match-beginning 0) (match-end 0))
       (let ((name (buffer-substring-no-properties beg end)))
	 (cond ((string-match "\\\\section" name) name)
	       ((string-match "\\\\chapter" name) name)
	       ((string-match "\\\\subsection" name) (concat " " name))
	       ((string-match "\\\\subsubsection" name) (concat "  " name))
	       ((string-match "\\\\paragraph" name) (concat "   " name))))))
    (yatex-mode
     "^[ 	]*\\(\\\\chapter\\*?{\\(.*\\)\\|\\\\section\\*?{\\(.*\\)\\|\\\\subsection\\*?{\\(.*\\)\\|\\\\subsubsection\\*?{\\(.*\\)\\|\\\\paragraph{\\(.*\\)$\\)"
     (lambda (beg end category)
       (se/set-face (match-beginning 0) (match-end 0))
       (let ((name (buffer-substring-no-properties beg end)))
	 (cond ((string-match "\\\\section" name) name)
	       ((string-match "\\\\chapter" name) name)
	       ((string-match "\\\\subsection" name) (concat " " name))
	       ((string-match "\\\\subsubsection" name) (concat "  " name))
	       ((string-match "\\\\paragraph" name) (concat "   " name))))))
    ;; lisp dialects
    ((emacs-lisp-mode lisp-interaction-mode lisp-mode)
     (("function" "^(def\\(un\\|method\\|generic\\|macro\\|subst\\) \\([^ \t\n]+\\)\\b")
      ("variable" "^(def\\(var\\|class\\|parameter\\|const\\)\\W+\\([^ \t\n]+\\)\\W"))
     (lambda (beg end category)
       (cond ((member category '("function" "variable"))
	      (se/matched-pattern 2))
	     (t (se/matched-pattern 1)))))
    (outline-mode
     outline-regexp
     (lambda (beg end category)
       (goto-char end)
       (end-of-line 1)
       (se/string-subst-char ?\  ?\t
			     (buffer-substring-no-properties beg (min (+ beg (window-width))
							(point))))))
    (perl-mode "^sub \\([^ {]+\\)")
    (prolog-mode
     "^\\(\\w+\\)\\W*("
     (lambda (beg end category)
       (let ((result (se/only-once (se/matched-pattern 1))))
	 (if result (se/set-face (match-beginning 1) (match-end 1)))
	 result)))
    ;; python 2005-06-30T14:00:27.00+9:00
    (python-mode
     "^\\([ \t]*\\(def\\|class\\)\\W+.*\\):"
     (lambda (beg end category)
       (buffer-substring-no-properties (match-beginning 0)
				       (match-end 0))))
    ;; ruby 2005-06-29T11:49:39.00+9:00
    (rd-mode
     "^=+ .+$")
    ((ruby-mode inferior-ruby-mode)
     "^\\( *\\(class\\|module\\|def\\|if +__FILE__ *== *\\$0\\|if +\\$0 *== *__FILE__\\|=begin\\).+\\)$")
    (scheme-mode
     "(define[ \t]+\\([^ \t\n].+\\)"
     (lambda (beg end category)
       (let ((id (se/matched-pattern 1)))
	 (save-excursion
	   (goto-char (match-beginning 0))
	   (concat (make-string (current-column) ?\ ) id)))))
    ;; scheme
    ((shell-mode comint-mode)
     "^[^#$%>]*[#$%>][ \t]+\\(.+\\)$" ;     "^[^#$%>\n]*[#$%>]\\W+\\(.+\\)$"
     (lambda (beg end category)
       (se/set-face beg end)
       (save-excursion
	 (goto-char (match-beginning 1))
	 (while (looking-at "[ \t]+") (goto-char (match-end 0)))
	 (if (= (point) end)
	     nil
	   (buffer-substring-no-properties (point) end)))))
    (t nil)				; "^*+[ 	]+\\(.*\\)$"
    )
  "alist of lists of (MODE REGEXP [FORMATTER]), where
MODE is a symbol of mode, or list of symbols of mode;
REGEXP is a regexp string or alist of lists of name (string) and its
regexp (string);
FORMATTER is lambda function (don't enclose in function) that accept
three args as the beginning point (integer) of a matched string to
REGEXP, the end  point (integer) of it, and the name (string) of the
REGEXP; returns string that is used as name of matched string if you
think it is what you want to search exactly. Otherwise returns nil. In
this case, matched string is discarded.")

(defvar se/item-delimiter-regexp 'undefined
  "*delimiter of item. This is a buffer local variable.")
(make-variable-buffer-local 'se/item-delimiter-regexp)
(set-default 'se/summary-delimiter-regexp 'undefined)

(defvar se/item-name-constructor-function nil
  "buffer local function which genereates the appriciate name string to
the item. The function is required accepting three args. First one is the
position(integer or marker) of beginning of the item-delimiter
matched. The second is the end of the item-delimiter matched. The function
is invoked in save-excursion and save-restriction, thun you can move point
anywhere. The third is category(string) or nil.")
(make-variable-buffer-local 'se/item-name-constructor-function)
 
(cond ((and (< 21 emacs-major-version)
	    (string= current-language-environment "Japanese"))
       (defvar se/shortcut-menu-list 
	 (list "summary menu"
	       (list "summary menu"
		     '("表示" . se/show-current-item)
		     '("ジャンプ" . se/goto-current-item)
		     '("強調表示する/しない" . se/toggle-this-item-face)
		     '("--" . nil)
		     '("対象領域をマーク" . se/mark-current-item)
		     '("マーク取り消し" . se/unmark-current-item)
		     '("次の項目に結合" . se/merge-items)
		     '("サマリーから削除" . se/delete-from-summary)
		     '("全マーク削除" . se/unmark-all)
		     '("--" . nil)
		     '("項目文字列による整列" . se/sort-summary-by-name)
		     '("出現順による整列" . se/sort-summary-by-position)
		     '("同一項目をまとめる(uniq)" . se/unique)
		     '("全項目を強調表示する/しない" . se/toggle-all-item-faces)
		     '("全項目の強調表示取り消し" . se/clear-all-faces)
		     '("サマリーの再生成" . se/remake-summary)
		     '("項目の定義を見る" . se/show-item-documentation)))))
      (t
       (defvar se/shortcut-menu-list 
	 (list "summary menu"
	       (list "summary menu"
		     '("Show" . se/show-current-item)
		     '("Go to" . se/goto-current-item)
		     '("Toggle face" . se/toggle-this-item-face)
		     '("--" . nil)
		     '("Mark" . se/mark-current-item)
		     '("Unmark" . se/unmark-current-item)
		     '("Merge to next" . se/merge-items)
		     '("Delete" . se/delete-from-summary)
		     '("Clear all mark" . se/unmark-all)
		     '("--" . nil)
		     '("Sort by name" . se/sort-summary-by-name)
		     '("Sort by position" . se/sort-summary-by-position)
		     '("Make unique" . se/unique)
		     '("Toggle all faces" . se/toggle-all-item-faces)
		     '("Clear all faces" . se/clear-all-faces)
		     '("Remake buffer" . se/remake-summary)
		     '("Item definition" . se/show-item-documentation))))))

(defvar se/summary-regexp-history nil)
(defvar se/summary-category-history nil)
(defvar se/summary-soccur-history nil)
(defvar se/summary-category nil)
(make-variable-buffer-local 'se/summary-category)
(defvar se/summary-order-by 'position
  "The value must be either position (symbol) or name")
(make-variable-buffer-local 'se/summary-order-by)
(defvar se/default-item-face (if window-system 'secondary-selection 'underline))
(make-variable-buffer-local 'se/default-item-face)
(defvar se/show-line-top-of-window t)
(make-variable-buffer-local 'se/show-line-top-of-window)

;; mark
(defconst se/summary-item-flags (regexp-quote "*"))
(defconst se/summary-item-flag-regexp (regexp-quote "*"))
(defconst se/mark-character ?\*)

;;;
;;; programming stuff
;;;
(defun se/list-assq-cdr (item list)
  (let (exit)
    (while (and (not exit) list)
      (cond ((eq (car (car list)) item) (setq exit (car list)))
	    ((and (consp (car (car list))) (memq item (car (car list))))
	     (setq exit (car list)))
	    (t (setq list (cdr list)))))
    (cdr exit)))

(defun se/set-item-delimiters-from-mode (mode)
  (let ((regexp-func (or (se/list-assq-cdr mode se/mode-delimiter-alist)
			 (se/list-assq-cdr t se/mode-delimiter-alist)
			 (list "\n\n.+$"))))
    (setq se/item-delimiter-regexp (car regexp-func))
    (when (symbolp se/item-delimiter-regexp)
      (setq se/item-delimiter-regexp (symbol-value se/item-delimiter-regexp)))
    (when (cdr regexp-func)
      (when (and se/incremental-compile-p
		 (not (byte-code-function-p (car (cdr regexp-func)))))
	(setcar (cdr regexp-func) (byte-compile (car (cdr regexp-func)))))
      (setq se/item-name-constructor-function (car (cdr regexp-func))))))

(defsubst se/string-subst-char (new old string)
  (let (index)
    (setq old (regexp-quote (char-to-string old)))
    (while (setq index (string-match old string index))
      (aset string index new)))
  string)

(cond ((featurep 'mule)
       (defalias 'se/string-display-width 'string-width))
      (t 
       (defun se/string-display-width (str) (length str))))

(defsubst se/string-cut-down-to (string width &optional cut-head)
  "make substring of STRING which string-length is WIDTH. If 3rd optional
arg CUT-HEAD is non-nil, the head is ommited. "
  (let* ((dir (if cut-head -1 1))
	 (pos (if cut-head (1- (length string)) 0))
	 (len (char-width (aref string pos)))
	 str)
    (if (<= (string-width string) width)
	(setq str string)
      (while (<= len width)
	(setq pos (+ pos dir))
	(setq len (+ len (char-width (aref string pos)))))
      (setq str (substring string
			   (if cut-head (1+ pos) 0)
			   (if cut-head (length string) pos))))
    (if (= width (string-width str))
	str
      (concat str (make-string (- width (string-width str)) ? )))))

(defun se/untabify-string (str)
  (let ((i 0)
	(j 0)
	(last (length str))
	(not-found t)
	(tmp nil))
    ;; check TAB
    (while (and not-found (< i last))
      (if (= (aref str i) ?\t)
	  (setq not-found nil)
	(setq i (1+ i))))
    (if not-found
	str
      ;; replace TAB
      (setq tmp (concat (substring str 0 (max 0 i))
			(make-string (- (* (+ (/ i 8) 1) 8) i) ? )))
      (setq j (* (+ (/ i 8) 1) 8))
      (setq i (1+ i))
      (while (< i last)
	(if (not (= (aref str i) ?\t))
	    (setq tmp (format "%s%c" tmp (aref str i))
		  j (1+ j))
	  (setq tmp (concat tmp
			    (make-string (- (* (+ (/ j 8) 1) 8) j) ? ))
		j (* (+ (/ j 8) 1) 8)))
	(setq i (1+ i)))
      tmp)))

(defun se/only-once (string)
  "Return STRING if it's not-yet-matched string.
Note: only use in the extent of se/make-summary-buffer, since the
occurence check uses the hash-table in se/*tmp* ."
  (or (hash-table-p se/*tmp*) (setq se/*tmp* (make-hash-table :test 'equal)))
  (if (cl-gethash string se/*tmp* nil) ; (member match se/*tmp*)
      nil
    (cl-puthash string t se/*tmp*)	; (push match se/*tmp*) ;
    string))

(defsubst se/matched-pattern (beg &optional end)
  (buffer-substring-no-properties (match-beginning beg) (match-end (or end beg))))

(defconst se/summary-vector 0)
(defconst se/parent-buffer 1)
(defconst se/narrow-in-showing-item 2)
(defconst se/last-shown-cluster 3)
(defconst se/regexp 4)
(defconst se/formatter 5)
(defconst se/show-face 6)
(defconst se/finalize 7)
(defconst se/scroll-when-show 8)
(defconst se/item-documentation 9)
(defconst se/summary-order-list 10)

(defvar se/*memory-structure* nil
  "List of memories required by the summary edit package and managed by
program automagically. This memory is accessed by both original buffer
and summary buffer. Thus Changing to buffer-local variable is not convenient.")
(make-variable-buffer-local 'se/*memory-structure*)
(defun se/make-se/memory ()
  (vector (list (cons 0 nil)) nil nil nil nil nil nil nil nil nil nil nil))
(defmacro se/ref (name) (` (aref se/*memory-structure* (, name))))
(defmacro se/set (name val) (` (aset se/*memory-structure* (, name) (, val))))

(defun se/ref-summary-vector (&optional category)
  (let ((alist (se/ref se/summary-vector)))
    (cdr (assoc (or category se/summary-category) alist))))
(defalias 'se/cluster-vector 'se/ref-summary-vector)
(defun se/set-summary-vector (category vec)
  (let* ((vec-alist (se/ref se/summary-vector))
	 (cons (assoc category vec-alist)))
    (if cons
	(rplacd cons vec)
      (rplacd vec-alist (cons (cons category vec) (cdr vec-alist))))))

(defun se/cluster-of (nth &optional category)
  "return NTH(zero-base) cluster in se/summary-vector in se/*memory-structure* ."
  (aref (se/ref-summary-vector (or category se/summary-category)) nth))

;;; cluster (vector)		cluster (structure) is created from the
;;;				item (text chank)
;;; that has the following seven fields:.
;;;  postion (integer)		order of this cluster (0-th base)
;;;  beg (marker)		start position of this cluster
;;;  end (or marker nil)	end position of this cluster
;;;  name (string)		the name
;;;  face-block (list)    	list of on face beg end.
;;;  display-string (string)	(cached) displayed string on summary buffer
;;;  position-on-summary(marker)	start position on summary buffer
;;;  marked (string)		operation flag (to delete)

(defmacro se/cluster-position (cluster) (` (aref (, cluster) 0)))
(defmacro se/set-cluster-position (cluster position)
  (` (aset (, cluster) 0 (, position))))
(defmacro se/cluster-name (cluster) (` (aref (, cluster) 1)))
(defmacro se/cluster-beg (cluster) (` (aref (, cluster) 2)))
(defmacro se/cluster-buffer (cluster) 
  (` (and (, cluster) (marker-buffer (aref (, cluster) 2)))))
(defmacro se/cluster-end (cluster) (` (aref (, cluster) 3)))
(defmacro se/set-cluster-end (cluster end) (` (aset (, cluster) 3 (, end))))
(defmacro se/cluster-face-block (cluster) (` (aref (, cluster) 4)))
(defmacro se/set-cluster-face-block (cluster list)
  (` (aset (, cluster) 4 (, list))))
(defmacro se/cluster-display-string (cluster)
  (` (aref (, cluster) 5)))
(defmacro se/set-cluster-display-string (cluster str)
  (` (aset (, cluster) 5 (, str))))
(defmacro se/set-cluster-position-on-summary (cluster point)
  (` (aset (, cluster) 6 (, point))))
(defmacro se/cluster-position-on-summary (cluster)
  (` (aref (, cluster) 6)))
(defmacro se/cluster-marked (cluster)
  (` (aref (, cluster) 7)))
(defmacro se/set-cluster-marked (cluster str)
  (` (aset (, cluster) 7 (, str))))
(defun se/make-cluster (nth name beg &optional end)
  "NTH BEG END &optional NAME"
  (or (markerp beg) (error "a cluster is not bound to a buffer"))
  (vector nth name beg end nil nil nil nil))
(defun se/update-cluster (cluster nth name beg &optional end face string mark)
  "NTH BEG END &optional NAME"
  (aset cluster 0 nth)
  (aset cluster 1 name)
  (if (markerp beg) (aset cluster 2 beg)
    (error "a cluster is not bound to a buffer"))
  (aset cluster 3 end)
  (if face (aset cluster 4 face))
  (if string (aset cluster 5 string))
  (if mark (aset cluster 7 mark))
  cluster)

(defun se/map-on-cluster (function)
  (let ((cluster-vector (se/cluster-vector)))
    (dotimes (index (length cluster-vector))
      (funcall function (aref cluster-vector index)))))

(defun se/make-summary-vector (category widen &optional old-vec)
  (save-excursion
    (save-restriction
      (if widen (widen))
      (let ((vec-index 0)
	    (case-fold-search (se/case-fold category))
	    list tmp number-of-item vec)
	(goto-char (point-min))
	(if (vectorp old-vec)
	    (while
		(setq tmp (se/search-next-cluster
			   category
			   (if (< vec-index (length old-vec))
			       (aref old-vec vec-index))))
	      (push tmp list)
	      (setq vec-index (1+ vec-index)))
	  (while (setq tmp (se/search-next-cluster category))
	    (push tmp list)))
	(setq number-of-item (length list))
	(if (= number-of-item 0) (error "No item found"))
	(setq vec (make-vector number-of-item nil))
	(dotimes (index number-of-item)
	  (aset vec (- number-of-item (1+ index)) (pop list)))
	(dotimes (index number-of-item)
	  (se/set-cluster-position (aref vec index) index))
	;; initialize end
	(dotimes (index (1- number-of-item))
	  (se/set-cluster-end (aref vec index)
			      (se/cluster-beg (aref vec (1+ index)))))
	(se/set-cluster-end (aref vec (1- number-of-item)) (point-max))
	(se/set-summary-vector category vec)))))

(defun se/get-delimiter (category)
  "return delimiter (regexp string) of CATEGORY (string)"
  (let ((regexp (or (se/ref se/regexp) se/item-delimiter-regexp)))
    (cond ((stringp regexp) regexp)
	  ((and (consp regexp) (symbolp (cdr regexp))) (car regexp))
	  (t (car (cdr (assoc category regexp)))))))

(defun se/case-fold (category)
  "return  case-fold search flag(nil/t) for CATEGORY (string)"
  (let ((regexp (or (se/ref se/regexp) se/item-delimiter-regexp)))
    (cond ((stringp regexp) nil)
	  ((and (consp regexp) (symbolp (cdr regexp))) (cdr regexp))
	  (t (cdr (cdr (assoc category regexp)))))))

(defun se/get-non-delimiter (category)
  (and (listp se/item-delimiter-regexp)
       (car (cdr (cdr (assoc category se/item-delimiter-regexp))))))

(defun se/search-next-cluster (category &optional cluster)
  "search the next cluster CATEGORY (string) from current point (not
passed as an arg)."
  (let (tmp)
    (while (not (or (setq tmp (se/search-next-cluster-aux category cluster))
		    (eobp))))
    tmp))

(defvar *the-cluster*)
(defun se/search-next-cluster-aux (category &optional cluster)
  "CATEGORY (string)"
  (if (re-search-forward (se/get-delimiter category) (point-max) t)
      (let ((name nil)
	    (end (match-end 0))
	    (beg (match-beginning 0))
	    (marker nil))
	(when (se/get-non-delimiter category)
	  (goto-char beg)
	  (re-search-forward (se/get-delimiter category) (point-max) t))
	(if (and cluster (markerp (se/cluster-beg cluster)))
	    (setq marker (se/cluster-beg cluster))
	  (setq marker (make-marker)))
	(set-marker marker beg)
	(goto-char end)
	(or cluster (setq cluster (se/make-cluster 0 nil marker)))
	(let ((*the-cluster* cluster))
	  (if (se/ref se/formatter)
	      ;;(narrow-to-region beg (match-end 0))
	      (setq name (funcall (se/ref se/formatter)
				  beg (match-end 0) category))
	    (let ((mbeg (or (match-beginning 1) (match-beginning 0)))
		  (mend (or (match-end 1) (match-end 0))))
	      (se/set-face mbeg mend)
	      (setq name (se/string-subst-char ?\  ?\	 (buffer-substring-no-properties mbeg mend))))))
	(if name (se/update-cluster cluster 0 name marker)))
    (goto-char (point-max))
    nil))

(defun se/summary-buffer-name (category string)
  (format "%s-in-%s" (or category "Items") string))

;; [command/program protocol]
;;;###autoload
(defun se/make-summary-buffer-mouse (e)
  (interactive "e")
  (let (cat-name)
    (let ((cat-name nil))
      (when (eq se/item-delimiter-regexp 'undefined)
	(se/set-item-delimiters-from-mode major-mode))
      (cond ((stringp se/item-delimiter-regexp)
	     (se/make-summary-buffer cat-name))
	    ((consp se/item-delimiter-regexp)
	     (setq cat-name
		   (x-popup-menu
		    e
		    (list "Smmary Category"
			  (cons "Summary Category"
				(mapcar
				 (lambda (name+regex)
				   (cons (car name+regex) (car name+regex)))
				 se/item-delimiter-regexp)))))
	    (if cat-name (se/make-summary-buffer cat-name)))))))

(defun se/make-summary-buffer (&optional cat-name widen/interactive memory regexp
					 formatter finalize not-show-line-top-of-window)
  "Make summary buffer. If prefix arg is larger than 1, search items from
whole buffer. Otherwise, and if buffer is narrowed, search items from
narrowed region. If cat-name is t, set regexp interactively.
If prefix arg is minus and invoked interactively, then you can set regexp
interactively.
"
  (interactive
   (list
    (progn
      (when (eq se/item-delimiter-regexp 'undefined)
	(se/set-item-delimiters-from-mode major-mode))
      (cond ((stringp se/item-delimiter-regexp) nil)
	    ((null se/item-delimiter-regexp) t)
	    ((< (prefix-numeric-value current-prefix-arg) 0)
	     "")
	    (t (completing-read "Category or RETURN(for new regexp): "
				se/item-delimiter-regexp ; table
				nil			 ; predicate
				nil			 ; require-match
				(car-safe se/summary-category-history) ; initial-input
				'se/summary-category-history ; history
				))))
    (prefix-numeric-value current-prefix-arg)))
  ;; If non-interactive invocation is done before interactive one,
  ;; first, we must check the value of se/item-delimiter-regexp.
  (when (eq se/item-delimiter-regexp 'undefined)
    (se/set-item-delimiters-from-mode major-mode))
  (when (or (eq cat-name t) ; (and (stringp se/item-delimiter-regexp))
	    (and (not (stringp se/item-delimiter-regexp))
		 (not regexp)
		 (not memory)
		 (not (assoc cat-name se/item-delimiter-regexp)))
	    (and widen/interactive	; prefix arg
		 (< widen/interactive 0)
		 (interactive-p))
	    (equal cat-name ""))		; completing-read
    (setq regexp (read-from-minibuffer "Regexp or NEWLINE: "
				       (car se/summary-regexp-history)
				       nil
				       nil
				       'se/summary-regexp-history))
    (if (string-equal regexp "")
	(let ((name.regexp se/item-delimiter-regexp))
	  (setq regexp nil)
	  (when (listp name.regexp)
	    (setq cat-name (caar name.regexp))))
      (setq cat-name nil
	    formatter t)))
  ;; 2005-06-30T14:08:29.00+9:00
  (setq se/show-line-top-of-window (not not-show-line-top-of-window))
  (let* ((se/*tmp* nil)
	 (se/memory nil)
	 (summary-buffer-name (se/summary-buffer-name cat-name (buffer-name)))
	 (widen-p (and (numberp widen/interactive)
		       (< 1 (abs widen/interactive))))
	 (buf (get-buffer-create summary-buffer-name))
	 (parent-buffer (current-buffer)))
    (let ((se/*memory-structure* (se/make-se/memory)))
      (se/set se/parent-buffer (current-buffer))
      (se/set se/regexp
	      (if memory
		  (aref memory se/regexp)
		(or regexp se/item-delimiter-regexp)))
      (se/set se/formatter
	      (if memory (aref memory se/formatter) ; specail branch
		(cond ((eq formatter t) nil)
		      (formatter formatter)
		      (t se/item-name-constructor-function))))
      (se/set se/finalize (if memory (aref memory se/finalize) finalize))
      (se/make-summary-vector cat-name (and widen/interactive widen-p)
			      (if memory
				  (aref memory se/summary-vector)
				(se/ref-summary-vector cat-name)))
      (setq se/memory se/*memory-structure*))
    ;; change buffer
    (se/pop-to-buffer buf)
    (summary-edit-summary-mode)
    (setq se/*memory-structure* se/memory)
    (setq se/summary-category cat-name)
    (se/initialize-local-variables buf cat-name)
    (se/update-summary-buffer)
    (and (eq se/summary-order-by 'name) (se/sort-summary-by-name))
    (if (se/ref se/show-face) (se/set-all-item-faces 'on))
    (goto-char (point-min))
  (and (se/ref se/finalize) (funcall (se/ref se/finalize)))))

(defun se/insert-cluster-here (cluster newline width lineformatter cachedp)
  (if newline (insert "\n"))
  (se/set-cluster-position-on-summary cluster (point))
  (insert (se/summary-display-format cluster width lineformatter cachedp))
  (put-text-property (save-excursion (beginning-of-line)
				     (forward-char 9)
				     (point))
		     (save-excursion (end-of-line) (point))
		     'mouse-face
		     'highlight))

(defun se/update-summary-buffer (&optional buf lineformatter cachedp)
  (save-excursion
    (and buf (pop-to-buffer buf))
    (let ((buffer-read-only nil)
	  (summary (se/ref-summary-vector se/summary-category))
	  (list (se/ref se/summary-order-list))
	  (slist (se/ref se/summary-order-list))
	  (width (window-width)))
      (erase-buffer)
      (if (or (null list) (eq se/summary-order-by 'position))
	  (dotimes (index (length summary))
	    (se/insert-cluster-here (aref summary index) (< 0 index)
				    width lineformatter cachedp))
	(se/insert-cluster-here (pop list) nil width lineformatter cachedp)
	(while list
	  (se/insert-cluster-here (pop list) t width lineformatter cachedp))))
    (shrink-window-if-larger-than-buffer)))

(defun se/summary-display-format (cluster width &optional lineformatter cachedp)
  (let* ((str (se/cluster-display-string cluster))
	 (line-format "%c  %3d: %s %3s")
	 (name-width (- width 13)) ; 3 + 3 + 2 + 1 + 3 = 12
	 (lin (cond ((null lineformatter)
		     (save-excursion
		       (set-buffer (se/cluster-buffer cluster))
		       (save-restriction
			 (widen)
			 (format "%3d"
				 (count-lines (se/cluster-beg cluster)
					      (se/cluster-end cluster))))))
		    ((stringp lineformatter) (format "%3s" lineformatter))
		    ((or (byte-code-function-p lineformatter)
			 (symbolp lineformatter))
		     (format "%3s" (funcall lineformatter cluster))))))
    (unless (and cachedp str)
      (setq str (se/untabify-string 
		 (se/string-cut-down-to (se/cluster-name cluster) name-width)))
      (setq str (concat str (make-string (- name-width 
					    (se/string-display-width str)) ?\ )))
      (se/set-cluster-display-string cluster str))
    (format line-format
	    (or (se/cluster-marked cluster) ? )
	    (1+ (se/cluster-position cluster)) str lin)))

(defun se/shortcut-menu (event)
  (interactive "e")
  (set-buffer (window-buffer (posn-window (event-end event))))
  (goto-char (posn-point (event-end event)))
  (beginning-of-line)
  (sit-for 0)
  (let ((op (x-popup-menu
	     event
	     se/shortcut-menu-list)))
    (and op (call-interactively op))))

(defun summary-edit-summary-mode ()
  "mode for displaying other buffer's items as summary.
\\[se/show-current-item]	display the item, and scroll up if already shown.
\\[se/show-current-item-backward]	display the item, and scroll down if already shown.
\\[se/jump-to/current-item]	display/scroll current or NUMBER item.
\\[se/forward-line]	go to next line and display the item in other window.
\\[se/previous-line]	go to previous line and display the item in other window.
\\[se/goto-current-item]	go to the current item in original buffer.
\\[se/delete-from-summary]	delete current line from summary buffer.
\\[se/unique]	sort and `uniq' items.
\\[se/remake-summary]	remake summary buffer(this buffer).
\\[se/jump-to-item-of]	jump to the Nth item and display it.
\\[se/quit-and-go-parent-buffer]	erase the summary buffer and go to original buffer.
\\[se/merge-items]		merge current item and the next.
\\[se/mark-current-item]	mark the current item.
\\[se/unmark-current-item]	unmark the current item.
\\[se/unmark-all]		unmark all items.
\\[se/narrow-in-showing-item]	toggle the display style of item.
\\[se/sort-summary-by-position]	sort summary by the position.
\\[se/sort-summary-by-name]	sort summary by item name.
\\[se/toggle-this-item-face]	toggle face of the current item.
\\[se/toggle-all-item-faces]	toggle faces of all items.
\\[se/clear-all-faces]	clear all faces(overlay exactly) even if genearated by other package.
\\[se/show-item-documentation]		about regexp used to generate this summary.
"
  (interactive)
  (let ((old-summary-order se/summary-order-by)
	(ml mode-line-buffer-identification))
    (kill-all-local-variables)
    (use-local-map summary-edit-mode-map)
    (make-local-variable 'se/summary-category)
    (make-local-variable 'se/summary-order-by)
    (if old-summary-order (setq se/summary-order-by old-summary-order))
    (setq buffer-read-only t)
    (setq major-mode 'summary-edit-summary-mode)
    (setq mode-name "Item-Summary")
    (setq mode-line-buffer-identification ml))
  (run-hooks 'summary-edit-summary-mode-hook))

(defun se/initialize-local-variables (buffer category)
  (save-excursion
    (set-buffer buffer)
    (setq se/summary-category category)))

(defun se/current-cluster (&optional category)
  (save-excursion
    (beginning-of-line)
    (if (re-search-forward
	 (concat "^" se/summary-item-flag-regexp "? +\\([0-9]+\\):")
	 nil t)
	(se/cluster-of (1- (string-to-number (se/matched-pattern 1))) category))))

(defun se/current-cluster-number ()
  (save-excursion
    (beginning-of-line)
    (if (re-search-forward
	 (concat "^" se/summary-item-flag-regexp "? +\\([0-9]+\\):")
	 nil t)
	(1- (string-to-number (se/matched-pattern 1))))))

(defun se/current-item-number ()
  (save-excursion
    (beginning-of-line)
    (if (re-search-forward
	 (concat "^" se/summary-item-flag-regexp "? +\\([0-9]+\\):")
	 nil t)
	(string-to-number (se/matched-pattern 1)))))

;; * soccur by rubikitch
(defun se/which-func-formatter (s e category)
  (save-excursion
    (concat (when (and (boundp 'which-func-mode)
		       which-func-mode
		       which-func-current)
              (concat
               "["
               (progn
                 (goto-char s)
                 (which-func-update)
                 which-func-current)
               "]"))
            (let ((str (buffer-substring-no-properties (point-at-bol) (point-at-eol))))
              (add-text-properties (- s (point-at-bol)) (- e (point-at-bol))
                                   '(face isearch-lazy-highlight-face) str)
              str))))

(defvar soccur-formatter 'se/which-func-formatter
  "Formatter used by soccur.
This is lambda function (don't enclose in function) that accept
three args as the beginning point (integer) of a matched string to
REGEXP, the end  point (integer) of it, and the name (string) of the
REGEXP; returns string that is used as name of matched string if you
think it is what you want to search exactly. Otherwise returns nil. In
this case, matched string is discarded.")

;;;###autoload
(defun soccur (regexp)
  "Show all lines in the current buffer containing a match for regexp.
Through the generated buffer, you can scroll, hilighten or get some statistics
data of the original buffer."
  (interactive 
   (list (completing-read (if (car-safe se/summary-soccur-history)
			      (format "Regexp for SOccur (default %s): " 
				      (car se/summary-soccur-history))
			    (format "Regexp for SOccur: "))
			  nil
			  nil
			  nil
			  nil
			  'se/summary-soccur-history)))
  (and (string= regexp "")
       (car-safe se/summary-soccur-history)
       (setq regexp (car se/summary-soccur-history)))
  (se/make-summary-buffer nil 1 nil regexp soccur-formatter nil t))

;; * display commands
(defun se/goto-current-item ()
  (interactive)
  (se/set se/last-shown-cluster nil)
  (se/show-cluster (se/current-cluster)))

(defun se/show-current-item ()
  (interactive)
  (beginning-of-line)
  (se/show-cluster (se/current-cluster) (current-buffer)))

(defun se/show-current-item-backward ()
  (interactive)
  (beginning-of-line)
  (se/show-cluster (se/current-cluster) (current-buffer) 'backward))

(defun se/mouse-show-current-item (event)
  (interactive "e")
  (set-buffer (window-buffer (posn-window (event-end event))))
  (goto-char (posn-point (event-end event)))
  (beginning-of-line)
  (se/show-current-item)
  (select-window (posn-window (event-end event))))

(defun se/jump-to/current-item (nth)
  (interactive "P")
  (if nth
      (se/jump-to-item-of (prefix-numeric-value nth))
    (se/show-current-item)))

(defun se/jump-to-item-of (nth)
  (interactive "P")
  (or nth
      (setq nth (string-to-number (read-from-minibuffer "Item number to jump: "))))
  (se/jump-to-cluster-of (1- nth)))

(defun se/jump-to-cluster-of (nth)
  (goto-char (se/cluster-position-on-summary (se/cluster-of nth)))
  (se/show-cluster (se/cluster-of nth) (current-buffer)))

(defmacro se/walk-frame-for-buffer (buffer &rest body)
  (` (let ((buffer (, buffer))
	   (cwindow (selected-window))
	   (cframe (selected-frame)))
       (mapcar
	(function
	 (lambda (frame)
	   (select-frame frame)
	   (walk-windows
	    (function
	     (lambda (win)
	       (when (eq (window-buffer win) buffer)
		 (select-window win)
		 (,@ body))))
	    nil 'only)))
	(frame-list))
       (select-frame cframe)
       (select-window cwindow))))

;(macroexpand '(se/walk-frame-for-buffer (current-buffer) (test)))
;(put 'se/walk-frame-for-buffer 'lisp-indent-function 1)
(defun se/show-point (p)
  (if se/show-line-top-of-window
      (set-window-start (get-buffer-window (current-buffer)) p)
    (goto-char p)
    ))
                    
(defun se/show-cluster (cluster &optional point-continuation backward-p)
  (let ((last-item)
	(mem se/*memory-structure*)
	(already-shown-p (eq (se/ref se/last-shown-cluster) cluster))
	(original-buffer (current-buffer)))
    (when (se/pop-to-buffer (se/cluster-buffer cluster))
      (let ((se/*memory-structure* mem)
	    (buffer (current-buffer)))
	(if already-shown-p
	    (se/walk-frame-for-buffer buffer
 	      (if backward-p
		  (condition-case beg
		      (scroll-down)
		    (beginning-of-buffer
		     (message "End of item")
		     (se/show-point (se/cluster-beg cluster))))
		(condition-case end
		    (scroll-up)
		  (end-of-buffer
		   (message "End of item")
		   (se/show-point (se/cluster-beg cluster))))))
	  (se/walk-frame-for-buffer buffer
	    (widen)
	    (goto-char (se/cluster-beg cluster))
	    (if (and 'will-be-replaced-by-a-slot-in-memory-structure
		     (se/cluster-beg cluster)
		     (se/cluster-end cluster)
		     (pos-visible-in-window-p (se/cluster-beg cluster))
		     (pos-visible-in-window-p (se/cluster-end cluster)))
		'nothing
	      (se/show-point
	       (save-excursion
		 (beginning-of-line
		  (se/ref se/scroll-when-show))
		 (point))))
	    (if (se/ref se/narrow-in-showing-item)
		(narrow-to-region (point) (se/cluster-end cluster)))
	    (se/set se/last-shown-cluster cluster))))
      ;; the following sexp must be out of let of se/*memory-structure*
      (let ((sec (if (numberp se/display-flush-time) 0 1))
	    (msec (and (numberp se/display-flush-time) se/display-flush-time)))
	(if point-continuation (se/pop-to-buffer point-continuation))
	(when (and (not already-shown-p)
		   (sit-for sec 0))		; redisplay but without overlay
	  (se/set-cluster-face cluster 'toggle)
	  (if msec (sit-for 0 msec))
	  (se/set-cluster-face cluster 'toggle))))))

(defun se/pop-to-buffer (buffer)
  "Return BUFFER (as non-nil value) when BUFFER exists. Otherwise nil."
  (if (and (bufferp buffer) (buffer-name buffer))
      (progn
	(if (not (get-buffer-window buffer 'visible))
	    (pop-to-buffer buffer)
	  (select-frame (window-frame (or (get-buffer-window buffer)
					  (get-buffer-window buffer 'visible))))
	  (pop-to-buffer buffer))
	buffer)
    (message "Parent buffer does not exist now.")
    nil))

(defun se/remake-summary ()
  (interactive)
  (let ((category se/summary-category)
	(mem se/*memory-structure*)
 	(nth (se/current-cluster-number)))
    (message "remaking...")
    (set-buffer (se/ref se/parent-buffer))
    (se/make-summary-buffer category nil mem)
    (let ((length (length (se/cluster-vector))))
      (when (and (numberp nth) (plusp length))
	(se/jump-to-cluster-of (min nth (1- length)))))
    (message "remaking...done")))

(defun se/previous-line (arg)
  (interactive "p")
  (let ((pos (point)))
    (forward-line (- arg))
    (beginning-of-line)
    (if (= (point) pos)
	(message "Beginning of buffer")
      (se/show-current-item))))

(defun se/forward-line (arg)
  (interactive "p")
  (let ((pos (point)))
    (forward-line arg)
    (beginning-of-line)
    (if (= (point) pos)
	(message "End of buffer")
      (se/show-current-item))))

(defun se/narrow-in-showing-item ()
  "Toggle item display style."
  (interactive)
  (save-excursion
    (se/set se/narrow-in-showing-item
	    (not (se/ref se/narrow-in-showing-item)))
    (if (se/ref se/narrow-in-showing-item)
	(message "Item is displayed with narrowing.")
      (message "All text is displayed when displaying item.")))
    (se/show-current-item))

;; * mark
(defun se/mark-current-item (&optional char)
  (interactive)
  (or char (setq char se/mark-character))
  (se/set-cluster-marked (se/current-cluster se/summary-category) char)
  (let (buffer-read-only)
    (beginning-of-line)
    (delete-char 1)
    (insert (char-to-string char)))
  (se/forward-line 1))

(defun se/unmark-current-item ()
  (interactive)
  (se/set-cluster-marked (se/current-cluster se/summary-category) nil)
  (let (buffer-read-only)
    (beginning-of-line)
    (delete-char 1)
    (insert " "))
  (se/previous-line 1))

(defun se/unmark-all ()
  (interactive)
  (let ((current (se/current-cluster-number)))
    (se/map-on-cluster
     (function (lambda (cluster) (se/set-cluster-marked cluster nil))))
    (se/update-summary-buffer nil nil 'already-cached)
    (se/jump-to-cluster-of current)))

;; * sort, order control
(defun se/sort-cluster (predicate)
  (let* ((vec (se/ref-summary-vector se/summary-category))
	 (length (length vec))
	 (last (1- length))
	 (tmp nil)
	 (new nil))
    (dotimes (index length) (push (aref vec (- last index)) tmp))
    (setq new (sort tmp predicate))
    (se/set se/summary-order-list new)))

(defun se/sort-summary-by-name ()
  (interactive)
  (let ((buffer-read-only nil)
	(nth (se/current-cluster-number)))
    ;; We can use both of the following two methods.
    ;; 1. string on buffer based searach
    ;; 2. cluster structure based search
    ;; The first seems faster than the second.
    ;; But to separate displayed style from internal data structure,
    ;; I dediced to use the second method.
    (se/sort-cluster
      (function
       (lambda (c1 c2) (string< (se/cluster-name c1) (se/cluster-name c2)))))
    (setq se/summary-order-by 'name)
    (se/update-summary-buffer nil nil 'already-cached)
    (se/jump-to-cluster-of nth)))

(defun se/sort-summary-by-position ()
  (interactive)
  (let ((buffer-read-only nil)
	(nth (se/current-cluster-number)))
    (se/sort-cluster
      (function
       (lambda (c1 c2) (string< (se/cluster-name c1) (se/cluster-name c2)))))
    (setq se/summary-order-by 'position)
    (se/update-summary-buffer nil nil 'already-cached)
    (se/jump-to-cluster-of nth)))

(defun se/sort-item-by-summary ()
  (interactive)
  (yes-or-no-p "You want to sort the text really?"))

(defun se/quit-and-go-parent-buffer ()
  (interactive)
  (let ((buf (current-buffer)))
    (if (se/pop-to-buffer (se/cluster-buffer (se/ref se/last-shown-cluster)))
	(progn (widen)
	       (delete-other-windows))
      (switch-to-buffer nil))		; Wed Apr 19 13:41:44 1995
    (kill-buffer buf)))

(defun se/unique ()
  (interactive)
  (let ((buffer-read-only nil)
	(cat se/summary-category)
	(nth (se/current-cluster-number)))
    (or (eq se/summary-order-by 'name) (se/sort-summary-by-name))
    (let* ((vec (se/ref-summary-vector cat))
	   (items (se/unique-cluster-number-list))
	   (num (length items))
	   (index 0)
	   (new (make-vector num nil)))
      (while items
	(aset new index (aref vec (car items)))
	(setq items (cdr items)
	      index (1+ index)))
      (se/numbering-summary-vector new)
      ;(se/face-off)
      (se/set-summary-vector cat new)
      (se/set se/summary-order-list nil)
      (se/update-summary-buffer nil "" t))))

(defun se/numbering-summary-vector (vec)
  (dotimes (index (length vec))
    (se/set-cluster-position (aref vec index) index)))

(defun se/delete-from-summary (&optional list)
  (interactive)
  (let ((cat se/summary-category)
	(buf (current-buffer)))
    (or list
	(setq list (let ((tmp))
		    (se/map-on-cluster
		     (function (lambda (c)
				 (if (se/cluster-marked c)
				     (push (se/cluster-position c) tmp)))))
		    tmp))
	(setq list (se/current-cluster-number)))
    (if (numberp list) (setq list (list list)))
    (beginning-of-line)
    (se/delete-clusters-from-summary list cat buf)
    (se/jump-to-cluster-of
     (min (max 0 (1- (length (se/ref-summary-vector cat)))) (car list)))))

(defun se/merge-items (&optional num)
  (interactive)
  (let ((cat se/summary-category)
	cluster next)
    (or num (setq num (se/current-cluster-number)))
    (setq cluster (se/cluster-of num))
    (or (setq next (se/cluster-of (1+ num))) (error "No next item to merge"))
    (se/set-cluster-end cluster (se/cluster-end next))
    ;; 2005-06-30T18:03:08.00+9:00 (fix an illegal logical structure)
    (let* ((curr-attr (se/cluster-face-block cluster))
	   (next-attr (se/cluster-face-block next))
	   (beg (nth 2 curr-attr))
	   (newend (nth 3 next-attr)))
      (and (se/cluster-face-block cluster)
	   (se/cluster-face-block next)
	   (= (nth 3 curr-attr) (nth 2 next-attr))
	   (setcdr (nthcdr 2 curr-attr) (nthcdr 3 next-attr))))
    (se/delete-from-summary (1+ num))
    (se/jump-to-cluster-of num)
    (if (car (se/cluster-face-block cluster))
	(se/set-cluster-face cluster 'on))))

(defun se/unique-cluster-number-list ()
  (save-excursion
    (goto-char (point-min))		; [Thu Feb 22 12:43:47 1996]
    (let ((list (list (se/current-cluster-number)))
	  (cur 0))
      (while (not (eobp))
	(setq cur (se/current-cluster-number))
	(if (not (string= (se/cluster-display-string (se/cluster-of (car list)))
			  (se/cluster-display-string (se/cluster-of cur))))
	    (push cur list))
	(forward-line 1))
      (nreverse list))))

(defun se/delete-clusters-from-summary (list cat buf)
  (let* ((vec (se/ref-summary-vector cat))
	 (len (- (length vec) (length list)))
	 (index 0)
	 (last 0)
	 (cluster)
	 (new (make-vector len nil)))
    (dolist (nth list) (se/set-cluster-face (se/cluster-of nth) 'off))
    (while (< last len)
      (setq cluster (aref vec index))
      (unless (memq (se/cluster-position cluster) list)
	(aset new last cluster)
	(setq last (1+ last)))
      (setq index (1+ index)))
    (when (se/ref se/summary-order-list)
      (se/set se/summary-order-list (cons nil (se/ref se/summary-order-list)))
      (let ((rest (se/ref se/summary-order-list)))
	(while (cdr rest)
	  (if (member (se/cluster-position (car (cdr rest))) list)
	      (setcdr rest (cdr (cdr rest))))
	  (pop rest)))
      (se/set se/summary-order-list (cdr (se/ref se/summary-order-list))))
    ;; rebuild number sequence
    (se/numbering-summary-vector new)
    (se/set-summary-vector cat new)
    (se/update-summary-buffer nil nil t)
    (and (eq se/summary-order-by 'name)
	 (not (se/ref se/summary-order-list))
	 (se/sort-summary-by-name))))

;;; * face
(defun se/face-on-region (attr beg end)
  (let ((flag (buffer-modified-p))
	(overlay (make-overlay beg end)))
    (overlay-put overlay 'face attr)
    (overlay-put overlay 'summarye t)
    (set-buffer-modified-p flag)))

(defun se/face-off-region (attr beg end)
  (let ((flag (buffer-modified-p))
	(lstart 0))
    (while (and beg (> beg lstart) (< beg end))
      (mapcar (function (lambda (ovr)
			  (and (overlay-get ovr 'summarye)
			       (delete-overlay ovr))))
	      (overlays-at beg))
      (setq lstart beg beg (next-overlay-change beg)))
    (set-buffer-modified-p flag)))

;; [initialize protocol]
(defun se/set-face (beg end &optional attr on)
  "set face memory on region BEG END as ATTRIBUTE, and
use(display) it now if ON is non-nil. The type of BEG and END is either
integer or marker. The type of ATTR is symbol. Its default value is bold."
  (if (numberp beg) (setq beg (set-marker (make-marker) beg)))
  (if (numberp end) (setq end (set-marker (make-marker) end)))
  (or attr (setq attr se/default-item-face))
  ;; *the-cluster* is defined in search-next-cluster-aux
  (se/set-cluster-face-block *the-cluster* (list on attr beg end))
  (if on
      (save-excursion
	(if (markerp beg) (set-buffer (marker-buffer beg)))
	(se/face-on-region attr beg end))))

(defun se/set-cluster-face (cluster flag)
  (if (eq flag 'toggle)
      (se/set-cluster-face cluster
			   (if (car (se/cluster-face-block cluster)) 'off 'on))
    (let ((val (cond ((eq flag 'on) t)
		     ((eq flag 'off) nil))))
      (if (se/cluster-face-block cluster)
	  (rplaca (se/cluster-face-block cluster) val)
	(se/set-cluster-face-block
	 cluster (list val se/default-item-face (se/cluster-beg cluster)
		       (se/cluster-end cluster))))
      (save-excursion
	(set-buffer (se/cluster-buffer cluster))
	(apply (if val 'se/face-on-region 'se/face-off-region)
	       (cdr (se/cluster-face-block cluster)))))))

(defun se/toggle-this-item-face ()
  (interactive)
  (se/set-cluster-face (se/current-cluster) 'toggle))

(defun se/clear-all-faces ()
  (interactive)
  (save-excursion
    (set-buffer (se/ref se/parent-buffer))
    (se/face-off-region nil (point-min) (point-max))))

(defun se/toggle-all-item-faces ()
  (interactive)
  (se/set-all-item-faces 'toggle))

(defun se/set-all-item-faces (flag)
  (if (eq flag 'toggle)
      (se/set-all-item-faces (if (se/ref se/show-face) 'off 'on))
    (let ((mes (cond ((eq flag 'on) "Face on...")
		     ((eq flag 'off) "Face off...")))
	  (val (eq flag 'on)))
      (message mes)
      (se/set se/show-face val)
      (dotimes (i (length (se/ref-summary-vector se/summary-category)))
	(se/set-cluster-face (se/cluster-of i) flag))
      (message (concat mes " done.")))))

;; * help system
(defun se/show-item-documentation ()
  "Display item-documentation of current summary."
  (interactive)
  (with-output-to-temp-buffer "*Help*"
    (let ((cat se/summary-category))
      (princ "Category Name:\n")
      (princ (or se/summary-category "anonymous"))
      (terpri)
      (princ "Regexp:\n")
      (print (or (se/get-delimiter cat) "?"))
      (terpri)
      (when (stringp (se/ref se/item-documentation))
	(princ "Documentation:\n")
	(princ (se/ref se/item-documentation))))
    (print-help-return-message)))

;; * key bindings
(dotimes (index 10)
  (define-key summary-edit-mode-map (int-to-string index) 'digit-argument))
(defvar summary-edit-mode-key-bindings
  (list '("n" se/forward-line)
	'([down] se/forward-line)
	'("p" se/previous-line)
	'([up] se/previous-line)
	'("[?\177]" se/show-current-item-backward)
	'("\177" se/show-current-item-backward)
	'([delete] se/show-current-item-backward)
	'([backspace] se/show-current-item-backward)
	'("" se/show-current-item-backward)
	'("f" se/goto-current-item)
	'(" " se/show-current-item)
	'("b" se/show-current-item-backward)
	'("m" se/mark-current-item)
	'("u" se/unmark-current-item)
	'("M" se/merge-items)
	'("d" se/delete-from-summary)
	'("g" se/remake-summary)
	'("q" se/quit-and-go-parent-buffer)
	'("j" se/jump-to-item-of)
	'("\C-m" se/jump-to/current-item)
	'("\C-c\C-dn" se/narrow-in-showing-item)
	'("\C-c\C-sn" se/sort-summary-by-name)
	'("\C-c\C-sp" se/sort-summary-by-position)
	'("\C-c\C-ss" se/sort-item-by-summary)
	'("a" se/toggle-this-item-face)
	'("A" se/toggle-all-item-faces)
	'("C" se/clear-all-faces)
	'("h" se/show-item-documentation)))

(cond ((not window-system)
       (defvar summary-edit-mode-menu-bindings nil))
      ;; 日本語メニュー
      ((and (< 21 emacs-major-version)
	    (string= current-language-environment "Japanese"))
       (defvar summary-edit-mode-menu-bindings
	 (list (list [menu-bar summary] (cons "Summary" (make-sparse-keymap "Summary")))
	       '([menu-bar summary item-documentation]
		 ("項目の定義を見る" . se/show-item-documentation))
	       '([menu-bar summary delete]
		 ("サマリーからこの項目名を削除" . se/delete-from-summary))
	       '([menu-bar summary merge]
		 ("次の項目に結合" . se/merge-items))
	       '([menu-bar summary unique] ("同一項目をまとめる(uniq)" . se/unique))
	       '([menu-bar summary show] ("表示" . se/show-current-item))
	       '([menu-bar summary go] ("表示してポイント移動" . se/goto-current-item))
	       '([menu-bar summary remake] ("サマリーの再生成" . se/remake-summary))
	       '([menu-bar summary mark] ("項目をマーク" . se/mark-current-item))
	       '([menu-bar summary unmark] ("マーク取り消し" . se/unmark-current-item))
	       '([menu-bar summary sort-by-position]
		 ("出現順による整列" . se/sort-summary-by-position))
	       '([menu-bar summary sort-by-name]
		 ("項目文字列による整列" . se/sort-summary-by-name))
	       '([menu-bar summary narrow-display]
		 ("ナロー表示モードのトグル" . se/narrow-in-showing-item))
	       '([menu-bar summary toggle-face]
		 ("この項目を強調表示する/しない" . se/toggle-this-item-face))
	       '([menu-bar summary clear-face]
		 ("全項目の強調表示取り消し" . se/clear-all-faces))
	       '([menu-bar summary toggle-face-all]
		 ("全項目を強調表示する/しない" . se/toggle-all-item-faces))
	       '([mouse-2] se/mouse-show-current-item)
	       '([down-mouse-3] se/shortcut-menu))))
      (t
       (defvar summary-edit-mode-menu-bindings
	 (list (list [menu-bar summary] (cons "Summary" (make-sparse-keymap "Summary")))
	       '([menu-bar summary item-documentation]
		 ("Item definition" . se/show-item-documentation))
	       '([menu-bar summary delete]
		 ("Delete it from summary" . se/delete-from-summary))
	       '([menu-bar summary merge]
		 ("Merge current item and the next" . se/merge-items))
	       '([menu-bar summary unique] ("Make items unique" . se/unique))
	       '([menu-bar summary show] ("Show it" . se/show-current-item))
	       '([menu-bar summary go] ("Go to it" . se/goto-current-item))
	       '([menu-bar summary remake] ("Remake summary" . se/remake-summary))
	       '([menu-bar summary mark] ("Mark" . se/mark-current-item))
	       '([menu-bar summary unmark] ("Unmark" . se/unmark-current-item))
	       '([menu-bar summary sort-by-position]
		 ("Sort by position" . se/sort-summary-by-position))
	       '([menu-bar summary sort-by-name]
		 ("Sort by name" . se/sort-summary-by-name))
	       '([menu-bar summary narrow-display]
		 ("Toggle narrowed display mode" . se/narrow-in-showing-item))
	       '([menu-bar summary toggle-face]
		 ("(de)highlight it" . se/toggle-this-item-face))
	       '([menu-bar summary clear-face]
		 ("Clear all highlights" . se/clear-all-faces))
	       '([menu-bar summary toggle-face-all]
		 ("(de)highlight all" . se/toggle-all-item-faces))
	       '([mouse-2] se/mouse-show-current-item)
	       '([down-mouse-3] se/shortcut-menu)))))

(let ((list summary-edit-mode-key-bindings))
  (while list (apply (function define-key) summary-edit-mode-map (pop list))))
(let ((list summary-edit-mode-menu-bindings))
  (while list (apply (function define-key) summary-edit-mode-map (pop list))))

(define-key-after (lookup-key global-map
			      (if (< 20 emacs-major-version)
				  [menu-bar tools]
				  [menu-bar search]))
  [se:make-summary]
  '("Make Summary" . se/make-summary-buffer)
  t)

(define-key-after (lookup-key global-map
			      (if (< 20 emacs-major-version)
				  [menu-bar tools]
				  [menu-bar search]))
  [soccur]
  '("Summarized Occur" . soccur)
  t)

(define-key-after (lookup-key global-map
			      (if (< 20 emacs-major-version)
				  [menu-bar tools]
				  [menu-bar search]))
  [se:check-document]
  '("Load se-doc" . (lambda () (interactive) (load-library "se-doc")))
  t)

(provide 'summarye)
 
;; summarye.el ends here
;; Local Variables:
;; time-stamp-format: "%u@%h %:y-%02m-%02dT%02H:%02M:%02S"
;; End:
;; L
