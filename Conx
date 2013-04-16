;;; conx.el --- Yet another dissociater

;; Copyright status unknown

;; Author: Jamie Zawinski <jwz@netscape.com>
;; Keywords: games

;; This file is part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; XEmacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Synched up with: Not in FSF

;;; Commentary:

;;; conx.el: Yet Another Dissociator.
;;; Original design by Skef Wholey <skef@cs.cmu.edu>;
;;; ported to Emacs-Lisp by Jamie Zawinski <jwz@netscape.com>, 5-mar-91.
;;; Run this compiled.  It will be an order of magnitude faster.
;;;
;;; Select a buffer with a lot of text in it.  Say M-x conx-buffer
;;; or M-x conx-region.  Repeat on as many other bodies of text as
;;; you like.
;;;
;;; M-x conx will use the word-frequency tree the above generated
;;; to produce random sentences in a popped-up buffer.  It will pause
;;; at the end of each paragraph for two seconds; type ^G to stop it.
;;;
;;; M-x conx-init will clear the data structures so you can start
;;; over.  Note that if you run it twice consecutively on the same
;;; body of text, word sequences in that buffer will be twice as
;;; likely to be generated.
;;;
;;; Once you have sucked in a lot of text and like the kinds of
;;; sentences conx is giving you, you can save the internal data
;;; structures to a file with the M-x conx-save command.  Loading
;;; this file with M-x conx-load will be a lot faster and easier
;;; than re-absorbing all of the text files.  Beware that loading a
;;; saved conx-file clears the conx database in memory.
;;;
;;; M-x conx-emit-c will write out C source code which, when compiled,
;;; will produce a standalone program which generates sentences from
;;; a copy of the database currently loaded.
;;;
;;; Ideas for future improvement:
;;;
;;;  o  It would be nice if we could load in more than one saved
;;;     file at a time.
;;;
;;;  o  use it to collect statistics on newsgroup conversations by
;;;     examining the tree for the most common words and phrases
;;;
;;;  o  when replying to mail, insert an X-CONX: header field which
;;;     contains a sentence randomly generated from the body of the
;;;     message being replied to.
;;;
;;;  o  It could stand to be faster...

;;; Code:
(defconst conx-version "1.6,  6-may-94.")

(defvar conx-bounce 10) ; 1/x
(defvar conx-hashtable-size 9923)  ; 9923 is prime
(defconst conx-words-hashtable nil)
(defconst conx-words-vector nil)
(defconst conx-words-vector-fp 0)

(defconst conx-last-word nil)

(defvar conx-files nil "FYI")

(defun conx-init ()
  "Forget the current word-frequency tree."
  (interactive)
  (if (and conx-words-hashtable
	   (>= (length conx-words-hashtable) conx-hashtable-size))
      (fillarray conx-words-hashtable 0)
      (setq conx-words-hashtable (make-vector conx-hashtable-size 0)))
  (if conx-words-vector
      (fillarray conx-words-vector nil)
      (setq conx-words-vector (make-vector 1000 nil))) ; this grows
  (setq conx-words-vector-fp 0)
  (setq conx-last-word nil
	conx-files nil))

(defun conx-rehash ()
  ;; misnomer; this just grows the linear vector, growing the hash table
  ;; is too hard.
  (message "Rehashing...")
  (let* ((L (length conx-words-vector))
	 (v2 (make-vector (+ L L) nil)))
    (while (< 0 L)
      (aset v2 (1- L) (aref conx-words-vector (setq L (1- L)))))
    (setq conx-words-vector v2)
    )
  (message "Rehashing...done"))

(defmacro conx-count  (word) (list 'aref word 0))
(defmacro conx-cap    (word) (list 'aref word 1))
(defmacro conx-comma  (word) (list 'aref word 2))
(defmacro conx-period (word) (list 'aref word 3))
(defmacro conx-quem   (word) (list 'aref word 4))
(defmacro conx-bang   (word) (list 'aref word 5))
(defmacro conx-succ   (word) (list 'aref word 6))
(defmacro conx-pred   (word) (list 'aref word 7))
(defmacro conx-succ-c (word) (list 'aref word 8))
(defmacro conx-pred-c (word) (list 'aref word 9))
(defconst conx-length 10)

(defmacro conx-make-word ()
  '(copy-sequence '[1 0 0 0 0 0 nil nil 0 0]))

(defmacro conx-setf (form val)  ; mind-numbingly simple
  (setq form (macroexpand form (and (boundp 'byte-compile-macro-environment)
				    byte-compile-macro-environment)))
  (cond ((symbolp form) (list 'setq form val))
	((eq (car form) 'aref) (cons 'aset (append (cdr form) (list val))))
	((eq (car form) 'cdr) (list 'setcdr (nth 1 form) val))
	((eq (car form) 'car) (list 'setcar (nth 1 form) val))
	(t (error "can't setf %s" form))))

(defmacro conx-push (thing list)
  (list 'conx-setf list (list 'cons thing list)))

(defconst conx-most-positive-fixnum (lsh -1 -1)
  "The largest positive integer that can be represented in this emacs.")

(defmacro conx-rand (n)
  (list '% (list 'logand 'conx-most-positive-fixnum '(random)) n))

(defmacro conx-relate-succ (word related)
  `(let ((vec (symbol-value (, word))))
     (conx-setf (conx-succ-c vec) (1+ (conx-succ-c vec)))
     (let ((rel (assq (, related) (conx-succ vec))))
       (if rel
           (setcdr rel (1+ (cdr rel)))
         (conx-push (cons (, related) 1) (conx-succ vec))))))

(defmacro conx-relate-pred (word related)
  `(let ((vec (symbol-value (, word))))
     (conx-setf (conx-pred-c vec) (1+ (conx-pred-c vec)))
     (let ((rel (assq (, related) (conx-pred vec))))
       (if rel
           (setcdr rel (1+ (cdr rel)))
         (conx-push (cons (, related) 1) (conx-pred vec))))))

(defmacro conx-add-word (word)
  `(let* ((word (, word))
          (fc (aref word 0)))
     (setq word (intern (downcase word) conx-words-hashtable))
     (let ((vec (and (boundp word) (symbol-value word))))
       (if vec
           (conx-setf (conx-count vec) (1+ (conx-count vec)))
         (if (= conx-words-vector-fp (length conx-words-vector))
             (conx-rehash))
         (set word (setq vec (conx-make-word)))
         (aset conx-words-vector conx-words-vector-fp word)
         (setq conx-words-vector-fp (1+ conx-words-vector-fp)))
       (or (< fc ?A) (> fc ?Z)
           (conx-setf (conx-cap vec) (1+ (conx-cap vec)))))
     (if conx-last-word
         (progn
           (conx-relate-succ conx-last-word word)
           (conx-relate-pred word conx-last-word)))
     (setq conx-last-word word)))

(defmacro conx-punx (char)
  `(if conx-last-word
       (let ((char (, char))
             (vec (symbol-value conx-last-word)))
         (cond ((eq char ?\,)
                (conx-setf (conx-comma vec) (1+ (conx-comma vec))))
               ((or (eq char ?\.)
                    (eq char ?\;))
                (conx-setf (conx-period vec) (1+ (conx-period vec)))
                (setq conx-last-word nil))
               ((eq char ?\?)
                (conx-setf (conx-quem vec) (1+ (conx-quem vec)))
                (setq conx-last-word nil))
               ((eq char ?\!)
                (conx-setf (conx-bang vec) (1+ (conx-bang vec)))
                (setq conx-last-word nil))))))

(defun conxify-internal ()
  (let (p w)
    (while (not (eobp))
      (skip-chars-forward "^A-Za-z0-9'")
      (while (memq (following-char) '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?\'))
	;; ignore words beginning with digits
	(skip-chars-forward "A-Za-z0-9'")
	(skip-chars-forward "^A-Za-z0-9'"))
      (setq p (point))
      (skip-chars-forward "A-Za-z0-9'")
      (if (= ?\' (preceding-char)) (forward-char -1))
      (if (eq p (point))
	  nil
	(setq w (buffer-substring p (point)))
	(if (equal "nil" w)  ; hey, nil is totally magic, this doesn't work!
	    nil
	  (conx-add-word w)
	  (setq n (1+ n))
	  (skip-chars-forward " \t\n\r")
	  (if (memq (setq p (following-char)) '(?\, ?\. ?\! ?\? ?\;))
	      (conx-punx p)))))))

;;;###autoload
(defun conx-buffer ()
  "Absorb the text in the current buffer into the tree."
  (interactive)
  (or conx-words-vector (conx-init))
  (let ((i conx-words-vector-fp)
	(n 0)
	(pm (point-max)))
    (save-excursion
      (goto-char (point-min))
      (save-restriction
	(widen)
	(while (< (setq p (point)) pm)
	  (search-forward "\n\n" pm 0)
	  (narrow-to-region p (point))
	  (goto-char (prog1 p (setq p (point))))
	  (conxify-internal)
	  (widen)
	  (message "%d%%..." (/ (* p 100) (point-max))))))
    (if buffer-file-name
	(setq conx-files (nconc conx-files (list buffer-file-name))))
    (message "%s words, %d unique" n (- conx-words-vector-fp i))))

;;;###autoload
(defun conx-region (p m)
  "Absorb the text in the current region into the tree."
  (interactive "r")
  (save-restriction
    (widen)
    (narrow-to-region p m)
    (conx-buffer)))

(defun conx-mail-buffer ()
  "Conxify a buffer in /bin/mail format."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (skip-chars-forward "\n \t")
    (let ((case-fold-search nil)
	  (buffer-file-name nil)
	  p p2 p3)
      (or (looking-at "^From ") (error "not in /bin/mail format"))
      (while (not (eobp))
	(search-forward "\n\n" nil 0)
	(setq p (point))
	(search-forward "\nFrom " nil 0)
	(setq p3 (setq p2 (point)))
	;; don't count ".signature" sections.
	(and (re-search-backward "\n--+\n" nil t)
	     (< (count-lines (point) p2) 9)
	     (setq p2 (point)))
	(conx-region p (point))
	(goto-char p3)))
    (if buffer-file-name
	(setq conx-files (nconc conx-files (list buffer-file-name))))
    ))

;;; output

(defun conx-random-related (count list)
  (let ((foll (if (= 0 count) 0 (conx-rand count)))
	ans)
    (while list
      (if (<= foll (cdr (car list)))
	  (setq ans (car (car list))
		list nil)
	  (setq foll (- foll (cdr (car list)))
		list (cdr list))))
    ans))

(defun conx-random-succ (word)
  (if (= 0 (conx-succ-c (symbol-value word)))
      word
      (let ((next (conx-random-related
		    (conx-succ-c (symbol-value word))
		    (conx-succ (symbol-value word)))))
	(if (= 0 (conx-rand conx-bounce))
	    (conx-random-succ
	      (conx-random-related
		(conx-pred-c (symbol-value next))
		(conx-pred (symbol-value next))))
	    next))))


(defun conx-sentence ()
  (or (> conx-words-vector-fp 0)
      (error "no conx data is loaded; see `conx-buffer'."))
  (let* ((word (aref conx-words-vector (conx-rand conx-words-vector-fp)))
	 (first-p t)
	 (p (point))
	 vec punc str)
    (while word
      (setq punc (conx-rand (conx-count (setq vec (symbol-value word)))))
      (if (or first-p
	      ;; (< (conx-rand (conx-count vec)) (conx-cap vec))
	      (= (conx-count vec) (conx-cap vec))
	      )
	  (progn
	    (setq first-p nil)
	    (setq str (symbol-name word))
	    (insert (+ (- ?A ?a) (aref str 0)))
	    (insert (substring str 1)))
	  (insert (symbol-name word)))
      (cond ((< punc (conx-comma vec))
	     (insert ", "))
	    ((< (setq punc (- punc (conx-comma vec))) (conx-period vec))
	     (setq word nil)
	     (if (= 0 (conx-rand 5))
		 (if (= 0 (conx-rand 4))
		     (insert ": ")
		     (insert "; "))
		 (insert ".  ")))
	    ((< (setq punc (- punc (conx-period vec))) (conx-quem vec))
	     (setq word nil)
	     (insert "?  "))
	    ((< (setq punc (- punc (conx-quem vec))) (conx-bang vec))
	     (setq word nil)
	     (insert "!  "))
	    (t
	     (insert " ")
	     (if (= 0 (conx-succ-c vec)) (setq word nil))))
      (if word
	  (setq word (conx-random-succ word))))
    (fill-region-as-paragraph (save-excursion
				(goto-char p)
				(beginning-of-line)
				(point))
			      (point))
    (if (= (preceding-char) ?\n)
	(if (= 0 (conx-rand 4))
	    (insert "\n")
	  (delete-char -1)
	  (insert "  "))))
  nil)

;;;###autoload
(defun conx ()
  "Generate some random sentences in the *conx* buffer."
  (interactive)
  (display-buffer (set-buffer (get-buffer-create "*conx*")))
  (select-window (get-buffer-window "*conx*"))
  (message "type ^G to stop.")
  (while t
    (goto-char (point-max))
    (sit-for (if (= (preceding-char) ?\n) 2 0))
    (conx-sentence)))

 
;;; GNUS interface; grab words from the current message.

(defun conx-gnus-snarf ()
  "For use as a gnus-select-article-hook."
  (set-buffer gnus-article-buffer)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (search-forward "\n\n" nil t)
      (conx-region (point) (point-max)))))

;;(add-hook 'gnus-select-article-hook 'conx-gnus-snarf)

(defun psychoanalyze-conx ()
  "Mr. Random goes to the analyst."
  (interactive)
  (doctor)				; start the psychotherapy
  (message "")
  (switch-to-buffer "*doctor*")
  (sit-for 0)
  (while (not (input-pending-p))
    (conx-sentence)
    (if (= (random 2) 0)
	(conx-sentence))
    (sit-for 0)
    (doctor-ret-or-read 1)))

 
;;; Saving the database

(defun conx-save (file)
  "Save the current CONX database to a file for future retrieval.
You can re-load this database with the \\[conx-load] command."
  (interactive "FSave CONX corpus to file: ")
  (save-excursion
   (let (b)
    (unwind-protect
      (progn
	(set-buffer (setq b (get-buffer-create "*conx-save-tmp*")))
	(delete-region (point-min) (point-max))
	(insert ";;; -*- Mode:Emacs-Lisp -*-\n")
	(insert ";;; This is a CONX database file.  Load it with `conx-load'.\n")
	(if conx-files
	    (insert ";;; Corpus: " (mapconcat 'identity conx-files ", ") "\n"))
	(insert ";;; Date: " (current-time-string) "\n\n")
	;; The file format used here is such a cute hack that I'm going to
	;; leave it as an excercise to the reader to figure it out.
	(let ((p (point))
	      (fill-column 78)
	      (fill-prefix "\t")
	      (i 0))
	  (insert "(!! [\t")
	  (while (< i conx-words-vector-fp)
	    (prin1 (aref conx-words-vector i) (current-buffer))
	    (insert " ")
	    (setq i (1+ i)))
	  (insert "])\n")
	  (fill-region-as-paragraph p (point))
	  (insert "\n"))
	(mapatoms (function (lambda (sym)
		    (if (not (boundp sym))
			nil
		      (insert "\(! ")
		      (prin1 sym (current-buffer))
		      (insert " ")
		      (prin1 (symbol-value sym) (current-buffer))
		      (insert "\)\n"))))
		  conx-words-hashtable)
	(goto-char (point-min))
	(while (re-search-forward "\\bnil\\b" nil t)
	  (replace-match "()"))
	(set-visited-file-name file)
	(save-buffer)))
    (and b (kill-buffer b)))))

;;;###autoload
(defun conx-load (file)
  "Load in a CONX database written by the \\[conx-save] command.
This clears the database currently in memory."
  (interactive "fLoad CONX corpus from file: ")
  (conx-init)
  (fset (intern "!!" conx-words-hashtable)
	(function (lambda (vec)
	  (setq conx-words-vector vec
		conx-words-vector-fp (length vec)))))
  (fset (intern "!" conx-words-hashtable)
	(symbol-function 'setq))
  (let ((obarray conx-words-hashtable))
    (load file)))

 
;;; Emitting C code

(defun conx-emit-c-data (&optional ansi-p)
  (let ((all '())
	(standard-output (current-buffer))
	(after-change-functions nil) ; turning off font-lock speeds it up x2
	(before-change-functions nil)
	(after-change-function nil)
	(before-change-function nil)
	(float-output-format "%.2f")
	count total total100)
    (or conx-words-hashtable (error "no words"))
    (let ((i 0))
      (mapatoms (function (lambda (x)
			    (if (boundp x)
				(setq all (cons (cons i x) all)
				      i (1+ i)))))
		conx-words-hashtable))
    (setq all (nreverse all))
    (setq total (* 4 (length all))
	  total100 (max 1 (if (featurep 'lisp-float-type)
			      (/ (float total) 100)
			    (/ total 100)))
	  count 0)
    (let ((rest all)
	  (i 5)
	  rest2
	  word)
      (insert "static unsigned short D[] = {")
      (while rest
	(setq word (symbol-value (cdr (car rest))))
	(setq rest2 (conx-pred word))
	(setq count (1+ count))
	(while rest2
	  (princ (cdr (car rest2))) (insert ",")
	  (princ (car (rassq (car (car rest2)) all)))
	  (insert ",")
	  (setq i (1+ i))
	  (cond ((> i 10)
		 (insert "\n")
		 (setq i 0)))
	  (setq rest2 (cdr rest2)))
	(message "Writing C code... %s%%" (/ count total100))
	(setq count (1+ count))
	(setq rest2 (conx-succ word))
	(while rest2
	  (princ (cdr (car rest2)))
	  (insert ",")
	  (princ (car (rassq (car (car rest2)) all)))
	  (insert ",")
	  (setq i (1+ i))
	  (cond ((> i 10)
		 (insert "\n")
		 (setq i 0)))
	  (setq rest2 (cdr rest2)))
	(message "Writing C code... %s%%" (/ count total100))
	(setq count (1+ count))
	(setq rest (cdr rest))))
    (insert "0};\nstatic char T[] = \"")
    (let ((rest all)
	  (i 0) (j 20)
	  k word)
      (while rest
	(setq word (symbol-name (cdr (car rest))))
	(setq k (1+ (length word))
	      i (+ i k)
	      j (+ j k 3))
	(cond ((> j 77)
	       (insert (if ansi-p "\"\n\"" "\\\n"))
	       (setq j (+ k 3))))
	(insert word)		; assumes word has no chars needing backslashes
	(insert "\\000")
	(message "Writing C code... %s%%" (/ count total100))
	(setq count (1+ count))
	(setq rest (cdr rest))))
    (insert "\";\nstatic struct conx_word words [] = {")
    (let ((rest all)
	  (i 0) (j 0)
	  cons name word)
      (while rest
	(setq cons (car rest)
	      name (symbol-name (cdr cons))
	      word (symbol-value (cdr cons)))
	(insert "{") (princ (conx-count word))
	(insert ",") (princ (conx-cap word))
	(insert ",") (princ (conx-comma word))
	(insert ",") (princ (conx-period word))
	(insert ",") (princ (conx-quem word))
	(insert ",") (princ (conx-bang word))
	(if (null (conx-pred word))
	    (insert ",0")
	  (insert ",")
	  (princ i)
	  (setq i (+ i (* 2 (length (conx-pred word))))))
	(if (null (conx-succ word))
	    (insert ",0,")
	  (insert ",")
	  (princ i)
	  (insert ",")
	  (setq i (+ i (* 2 (length (conx-succ word))))))
	(princ (conx-pred-c word)) (insert ",")
	(princ (conx-succ-c word)) (insert ",")
	(princ j)
	(setq j (+ j (length name) 1))
	(insert (if (cdr rest) (if (= 0 (% (car cons) 2)) "},\n" "},") "}"))
	(message "Writing C code... %s%%" (/ count total100))
	(setq count (1+ count))
	(setq rest (cdr rest))
	))
    (insert "};\n#define conx_bounce ")
    (princ conx-bounce)
    (insert "\n")
    (message "Writing C code... done.")
    ))

(defvar conx-c-prolog "\
#if __STDC__
#include <stddef.h>
#include <unistd.h>
extern long random (void);
extern void srandom (int);
extern void abort (void);
#endif
#include <stdio.h>
#include <time.h>

struct conx_word {
  unsigned short count;
  unsigned short cap;
  unsigned short comma;
  unsigned short period;
  unsigned short quem;
  unsigned short bang;
  unsigned short pred;
  unsigned short succ;
  unsigned short npred;
  unsigned short nsucc;
  unsigned short text;
};
")

(defvar conx-c-code "\
#define countof(x) (sizeof((x)) / sizeof(*(x)))
#define conx_rand(n) (random()%(n))

static struct conx_word *
conx_random_related (count, which_list)
     unsigned short count, which_list;
{
  unsigned short *list = D + which_list;
  int i = 0;
  unsigned short foll = (count == 0 ? 0 : conx_rand (count));
  while (1)
    {
      if (foll <= list [i * 2])
	{
	  if ((list [i * 2 + 1]) > countof (words))
	    abort ();
	  return &words [list [i * 2 + 1]];
	}
      foll -= list [i * 2];
      i++;
    }
}

static struct conx_word *
conx_random_succ (word)
     struct conx_word *word;
{
  if (word->nsucc == 0)
    return word;
  else
    {
      struct conx_word *next = conx_random_related (word->nsucc, word->succ);
      if (conx_rand (conx_bounce) != 0)
	return next;
      return conx_random_succ (conx_random_related (next->npred, next->pred));
    }
}

static void
conx_sentence ()
{
  static int x = 0;
  struct conx_word *word = 0;
  int first_p = 1;
  int done = 0;
  int count = 0;
  while (!done)
    {
      int punc;
      char *text;
      int L;
      if (word)
	word = conx_random_succ (word);
      else
	word = &words [conx_rand (countof (words))];
      count++;
      punc = conx_rand (word->count);
      text = T + word->text;
      L = strlen (text);
      if (x + L > 70)
	{
	  putchar ('\\n');
	  x = 0;
	}
      x += L+1;

      if (first_p || (word->count == word->cap))
	{
	  putchar ((*text >= 'a' && *text <= 'z') ? *text + ('A'-'a') : *text);
	  fputs (text+1, stdout);
	  first_p = 0;
	}
      else
	fputs (text, stdout);

      if (punc < word->comma)
	{
	  fputs (\", \", stdout);
	  x++;
	}
      else if ((punc -= word->comma) < word->period)
	{
	  x++;
	  if (count > 120 || conx_rand (5) != 0)
	    {
	      done = 1;
	      fputs (\".  \", stdout);
	      x++;
	    }
	  else
	    {
	      word = 0;
	      if (conx_rand (4) == 0)
		fputs (\": \", stdout);
	      else
		fputs (\"; \", stdout);
	    }
	}
      else if ((punc -= word->period) < word->quem)
	{
	  done = 1;
	  fputs (\"?  \", stdout);
	  x += 2;
	}
      else if ((punc -= word->quem) < word->bang)
	{
	  done = 1;
	  fputs (\"!  \", stdout);
	  x += 2;
	}
      else
	{
	  if (word->nsucc == 0)
	    {
	      fputs (\".  \", stdout);
	      x += 2;
	      done = 1;
	    }
	  else
	    putchar (' ');
	}
    }
  if (conx_rand (3) == 0)
    {
      fputs (\"\\n\\n\", stdout);
      x = 0;
    }
}

main (argc, argv)
     int argc;
     char **argv;
{
  unsigned int howmany, delay;
  char dummy;
  if (argc == 1)
    {
      howmany = 1;
      delay = 0;
    }
  else if (argc == 2 &&
      1 == sscanf (argv[1], \"%ud%c\", &howmany, &dummy))
    delay = 0;
  else if (argc == 3 &&
	   1 == sscanf (argv[1], \"%ud%c\", &howmany, &dummy) &&
	   1 == sscanf (argv[2], \"%ud%c\", &delay, &dummy))
    ;
  else
    {
      fprintf (stderr, \"usage: %s [count [delay]]\\n\", argv [0]);
      exit (1);
    }

  srandom (time (0));
  if (howmany == 0)
    howmany = ~0;
  while (howmany > 0)
    {
      conx_sentence ();
      fflush (stdout);
      howmany--;
      if (delay) sleep (delay);
    }
  putchar ('\\n');
  exit (0);
}
")

(defun conx-emit-c (file &optional non-ansi-p)
  "Write the current CONX database to a file as C source code.
The generated program will have the same effect as M-x conx,
except that it runs without emacs.

With a prefix argument, write K&R C instead of ANSI C.  ANSI is
the default because, without a certain ANSI feature, large databases
will overflow static limits in most K&R preprocessors."
  (interactive "FWrite C file: \nP")
  (find-file file)
  (erase-buffer)
  (let ((buffer-undo-list t))
    (insert conx-c-prolog)
    (if (not non-ansi-p)
	(insert "\n#if !__STDC__\n"
		"error! this file requires an ANSI C compiler\n"
		"#endif\n\n"))
    (conx-emit-c-data (not non-ansi-p))
    (insert conx-c-code))
  (goto-char (point-min)))

 
;;; Reporting stats

(defun conx-stats ()
  (set-buffer (get-buffer-create "*conx-stats*"))
  (delete-region (point-min) (point-max))
  (mapatoms (function (lambda (x)
	      (or (not (boundp x))
		  (progn
		    (insert (format "%s" (conx-count (symbol-value x))))
		    (insert "\t\t")
		    (insert (symbol-name x))
		    (insert "\n")))))
	    conx-words-hashtable)
  (sort-numeric-fields -1 (point-min) (point-max)))

;;; conx.el ends here
