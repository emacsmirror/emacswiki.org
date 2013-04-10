;;; multipoint.el --- A minor mode for multiple simultaneous regions
;;
;; Filename: multipoint.el
;; Created: 2013-04-09
;; Last edited: 2013-04-10
;;
;; URL: http://www.emacswiki.org/emacs/download/multipoint.el
;;
;; Tested on (but not very much): GNU Emacs 24.2.1 (i486-pc-linux-gnu,
;; GTK+ Version 3.4.2) of 2013-01-22 on biber, modified by Debian
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;
;; WHAT IS THIS?
;;
;; This is a local minor mode that allows you to operate on several
;; points (and marks) simultaneously, including entering text multiple
;; times but typing it only once.
;;
;; It's a proof-of-concept implementation (or disproof of concept, as
;; the case might be: I don't find I like this as much as I thought I
;; would). It might be useful, at this stage, to impress your
;; coworkers, but that's about it.
;;
;; I've always been slightly annoyed by the "rectangle" C-x r
;; commands. It would be nice to have rectangles go on the regular
;; kill ring, have the highlighting fixed, and in fact I suspected
;; there was a slightly more abstract way of thinking about the region
;; that would fit the rectangle problem nicely.
;;
;; The point-mark region can be thought of, or at least I do, as a
;; cons cell of two markers: (point . mark). That tells us where to
;; insert text, what to kill, and which point to keep on the
;; screen. The first two of those things can be generalized.
;;
;; What I want that cons cell to be replaced by is "a sexp of
;; ranges"; a rectangle would be a list of ranges (all of the same
;; length), a table would yield list of lists of ranges (not all of
;; the same length, though all the lists in the top list are). Killing
;; a rectangle would put on the kill ring a list of strings, and
;; killing a table a list of lists of strings: in general, killing
;; something that has the structure of a sexp could be accomplished by
;; setting the point-range variable to a sexp of ranges.
;;
;; The terminology I use is that there is always one region, which is
;; a structured expression of ranges. Each range has a point and a
;; mark, and also some other status. In effect, some buffer-local
;; variables become range-local, and have separate values for each
;; range in the region.
;;
;; A list of search results could be made, in entirety, a region
;; composed of one range for each instance of the search string.  That
;; suggests a use for distinguishing the points and marks: commands
;; that affect the point by doing something once should iterate over
;; the points of the ranges of the region, repeating their action
;; everywhere. In other words, I want self-insert-command to be lifted
;; to something that actually inserts many characters. The obvious use
;; case would be a search command, followed by a kill, followed by a
;; replacement inserted into all the points at once.
;;
;; Other editors have discontinuous regions, but this is not the same
;; thing at all: they only insert at one point, and they can't capture
;; the multidimensionality of a table (or sexp) when killed.
;;
;; Of course, going through the Emacs source code to replace every
;; reference to (point) and (mark) by one to a potentially complicated
;; region object would take weeks, so I decided to go for a proof of
;; concept as a regular minor mode.
;;
;; In addition to capturing many textual search results at once, it is
;; easy to imagine major modes providing search commands that make the
;; region cover all identifiers used for a variable, but not instances
;; of the same identifier string referring to another variable (that
;; metadata could be provided by the compiler, for instance): renaming
;; a variable would be as easy as running the highlight command,
;; killing, and entering a new identifier.
;;
;; The third function of point was to let us know which part of the
;; buffer to display: that's not generalizable to many points, or at
;; least I cannot think of a satisfactory way of doing so. Instead,
;; the convention I've adopted is that the "car-most" point in
;; region is where the traditional cursor is: we try the car,
;; caar, etc. of region until we find one that's a marker. You can
;; then rotate the structured region to jump to another point, quite
;; literally.
;;
;; I've decided that all points and all regions should be displayed
;; using overlays, but haven't taken care of overlapping regions or
;; the rather pathological case of two points pointing to the same
;; character.
;;
;; The simplest use case is to set point and mark, run M-x
;; double-cursor, and continue typing at two points at once.
;;
;; Point and mark have to be in the same buffer, but is that a
;; requirement for all the points and marks in multipoint mode? I
;; don't think so, and I think multipoint would be particularly useful
;; for renaming a variable across files, but the current
;; implementation is a per-buffer minor mode.
;; 
;; There are a number of new functions that might benefit from
;; convenient key bindings:
;;  * double-cursor
;;  * extrude-cursor-vertically
;;  * extrude-cursor-point-to-mark
;;  * select-rectangle
;;  * rotate-regions
;;  * rotate-region-contents
;;  * (i/regexp/plain)search-as-regions
;;  * drop-first-region
;;  * swap-coordinates (turns '((a b) (c d)) into '((a c) (b d))
;;
;; I hope that I've missed what this might be most useful for, of
;; course, and would be glad to hear about it.
;;
;; I'm not suggesting this mode is generally useful, and it is very
;; easy to accidentally edit large chunks of your buffer with it;
;; there are several cases in which such damage could be avoided, and
;; I would like to add warnings for them; in particular, multiple
;; points at the same character are dangerous, and so are off-screen
;; points that might have been forgotten about.
;;
;; Multipoint editing benefits, I imagine, from more commands that
;; depend on context, like upcase-word.
;;
;; The code for "lifting" existing commands to act on each range of
;; the structured region is similar to what is described as a functor
;; in many programming languages. It's also very ugly.
;;
;; For a similar feature, see
;; http://www.emacswiki.org/emacs/multiple-line-edit.el
;;
;; My personal conclusion is that multipoint editing is very hard and
;; only marginally more efficient than single-point editing. Don't do
;; it. Multiple points in different buffers are too dangerous to be
;; useful, but fascinating to watch.
;;
;; BUGS
;;
;; There are many.
;;
;;  * it seems to be impossible to simulate displaying a cursor at eol
;;    without confusing movement commands
;;  * points at the end of the buffer misbehave
;;  * a nil mark is likely to cause problems (new buffers have a nil
;;    mark, but once it is set it stays set)
;;  * overwrite-mode hasn't been tested
;;  * mouse-set-point behaves unexpectedly
;;  * the kill ring isn't limited in size
;;  * overlays are recreated from scratch after each change
;;  * the region can become nil. Arguably, this could evolve into a
;;    feature: new buffers have no mark, why should they have a point?
;;    Most importantly, many "pointless" puns become possible with a
;;    nil region.
;;  * inserts from unknown commands aren't lifted. This is a fundamental
;;    problem: there's no way to know in after-change whether the insert
;;    that happened depended on (point) or not. Inserts might happen at
;;    (point) by sheer accident, or they might happen away from
;;    (point) but still depend on it (think indentation).
;;  * should this use defadvice?

(defvar sp/region () "The multipoint region.")
(defvar sp/kill-ring () "The kill ring for multipoint regions.")
(defvar sp/kill-ring-max "The maximum number of entries on the kill ring for multipoint regions.")

(defvar sp/overlays ())

;; test whether we're dealing with a "range" object or a list of
;; (lists of ...) range objects.  A "range" object looks like this:
;; '(range (point . <marker>) (mark . <marker>) (mark-active . <bool>) ...)
(defun sp/rangep (r) (equal (car-safe r) 'range))

(defun sp/alist (r) (cdr r))
(defun sp/range-point (r) (cdr-safe (assoc 'point (cdr r))))
(defun sp/range-mark  (r) (cdr-safe (assoc 'mark  (cdr r))))

(defun sp/recurse (f2 l)
  (if (sp/rangep l)
      (funcall f2 (sp/alist l))
    (mapcar (lambda (l2) (sp/recurse f2 l2)) l)))

;; this function has been written with set-mark-command in mind. Its
;; mangling of last-command and this-command, in addition to depending
;; on dynamic scope, is probably something we shouldn't do.
;;
;; I'm also confused by the need for quasiquoting here; it would be
;; much cleaner to use a closure, but 

(defun sp/do1 (function3 symbol)
  `(lambda (alist)
     (save-excursion
       (let ((this-command ',symbol)
	     (last-command (cdr-safe (assoc 'last-command alist))))
	 (with-current-buffer (marker-buffer (cdr (assoc 'point alist)))
	   (goto-char (cdr (assoc 'mark alist)))
	   (set-mark (point))
	   (if (not (cdr (assoc 'mark-active alist)))
	       (deactivate-mark))
	   (goto-char (cdr (assoc 'point alist)))
	   (setq this-command ',symbol)
	   (setq last-command (cdr-safe (assoc 'last-command alist)))
	   (apply ',function3 nil)
	   (list 'range
		 (cons 'point (copy-marker (point-marker)))
		 (cons 'mark (copy-marker (mark-marker)))
		 (cons 'mark-active mark-active)
		 (cons 'last-command this-command)
		 ))))))

(defun sp/do (f sym)
  (let ((new-region (sp/recurse (sp/do1 f sym) sp/region)))
    (setq sp/region new-region))
  (sp/update-point-mark))


;; lift the command called "<name>" to a command called "sp/<name>",
;; which operates on each region separately.  Copy the interactive form
;; and extend the docstring.

(defun sp/liftp (name)
   `((defun ,(intern (concat "sp/" name)) (&rest args)
       ,(concat "At every point of the region, this command performs the following action:\n\n"
		(documentation (intern name)))
       ,(interactive-form (intern name))
       (sp/do (lambda () (apply ',(intern name) args))
	      ',(intern name)))
     
     (substitute-key-definition ',(intern name) ',(intern (concat "sp/" name))
				sp/keymap (current-global-map))))

(defun sp/lift (name)
  (eval (car (sp/liftp name)))
  (eval (cadr (sp/liftp name))))

(defun sp/sexp-signature (sexp)
  "Mangle a sexp of strings or a sexp of ranges to test whether one can be pasted into the other."
  (if (or (stringp sexp)
	  (sp/rangep sexp))
      "."
    (concat "(" (mapconcat 'sp/sexp-signature sexp "") ")")))

(defun sp/kill1 (alist)
  (let ((p (cdr (assoc 'point alist)))
	(m (cdr (assoc 'mark alist))))
    (if (< p m)
	(let ((s (buffer-substring p m)))
	  (delete-region p m)
	  s)
      (let ((s (buffer-substring m p)))
	(delete-region m p)
	s))))

(defun sp/kill-recursive (l)
	(sp/recurse 'sp/kill1 l))

(defun sp/kill ()
  (interactive)
  (setq sp/kill-ring
	(cons (sp/kill-recursive sp/region)
	      sp/kill-ring)))

(defun sp/kill-line1 (alist)
  (let* ((p (cdr (assoc 'point alist)))
	 (m (save-excursion
	      (with-current-buffer (marker-buffer p)
		(goto-char p)
		(end-of-line)
		(when (equal p (point)) (forward-char))
		(point)))))
    (if (< p m)
	(let ((s (buffer-substring p m)))
	  (delete-region p m)
	  s)
      (let ((s (buffer-substring m p)))
	(delete-region m p)
	s))))

(defun sp/kill-line-recursive (l)
	(sp/recurse 'sp/kill-line1 l))

(defun sp/kill-line ()
  (interactive)
  (setq sp/kill-ring
	(cons (sp/kill-line-recursive sp/region)
	      sp/kill-ring)))

(defun sp/yank-pop ()
  (interactive)
  (if (member last-command '(sp/yank sp/yank-pop))
     (progn
       (sp/kill-recursive sp/region) ; throw away result
       (setq sp/yank-kill-ring-index (1+ sp/yank-kill-ring-index))
       (sp/yank))
    (error "previous command was not an sp/yank")))
       

(defun sp/kill-as-sexp ()
  (interactive)
  (setq sp/kill-ring
	(cons (sp/kill-sexp-recursive sp/region))))
  
(defun sp/search-mark (string)
  (interactive "s") ; XXX limit to region
  (let ((pm ()))
    (while (search-forward string nil t)
      (let ((p)
	    (m))
	(save-excursion
	  (setq m (copy-marker (point-marker)))
	  (backward-char (length string))
	  (setq p (copy-marker (point-marker)))
	(setq pm (append pm (list (list 'range
					(cons 'point p)
					(cons 'mark m)
					(cons 'mark-active t)))))))
    (setq sp/region pm)
    (message (prin1-to-string sp/region))
    (sp/update-point-mark)
    (message (prin1-to-string sp/region))
    )))

(defun sp/kr-as-sexp-recursive (kr)
  (if (stringp kr)
      kr
    (concat "("
	    (mapconcat 'sp/kr-as-sexp-recursive kr " ")
	    ")")))

(defun sp/yank-as-sexp ()
  (interactive)
  (message (prin1-to-string (car sp/kill-ring)))
  (insert (sp/kr-as-sexp-recursive (car sp/kill-ring))))
  

(defun sp/yank-recursive (pm kr)
  (when pm
    (if (stringp kr)
	(save-excursion
	  (with-current-buffer (marker-buffer (cdr (assoc 'point (sp/alist pm))))
	    (goto-char (cdr (assoc 'point (sp/alist pm))))
	    (insert kr)
	    (setcdr (assoc 'mark (sp/alist pm)) (copy-marker (point-marker)))
	    ))
      (progn
	(sp/yank-recursive (car pm) (car kr))
	(sp/yank-recursive (cdr pm) (cdr kr))))))

(defvar sp/yank-kill-ring-index 0)

(defun sp/yank ()
  (interactive)
  (when (not (equal this-command
		    'sp/yank-pop))
    (setq sp/yank-kill-ring-index 0))
  (let ((kr sp/kill-ring)
	(skip sp/yank-kill-ring-index))
    (while (> skip 0)
      (setq kr (cdr-safe kr))
      (setq skip (1- skip)))
	    
    (while (and kr
		(not (equal (sp/sexp-signature sp/region)
			    (sp/sexp-signature (car kr)))))
      (setq skip (1+ skip))
      (setq kr (cdr kr)))
    (if kr
	(progn
	  (when (> skip 0) (message (concat "skipped "
					    (number-to-string skip) " kill-ring entries")))
	  (sp/yank-recursive sp/region (car kr))
	  (sp/update-point-mark))
      (message "couldn't find kill-ring entry that works"))))

(defun sp/update-overlays (l)
  (when l
    (if (not (sp/rangep l))
	(append (sp/update-overlays (car l))
		(sp/update-overlays (cdr l)))
      (let* ((p (cdr (assoc 'point (sp/alist l))))
	     (m (cdr (assoc 'mark (sp/alist l))))
	     (b (if (< p m) p m))
	     (e (if (> p m) p m))
	     (region-overlay (make-overlay b e (marker-buffer p) nil nil))
	     (point-overlay (make-overlay p (if (equal p (point-max)) p (1+ p)) (marker-buffer p) nil nil)))
	(overlay-put point-overlay 'priority 11)
	(overlay-put point-overlay 'face (list :foreground "white"
					       :background "grey60"))
	(with-current-buffer (marker-buffer p)
	  (when (equal (buffer-substring-no-properties p (+ p 1)) "\n")
	    ;; this is buggy: display " \n" looks worse but doesn')
	    ;; break keyboard 'after-string "\n" looks better but break)
	    ;; cursor movement.)
	    ;;(overlay-put point-overlay 'display " ")
	    ;;(overlay-put point-overlay 'after-string "\n")
	    
	    )
	  (when (equal (buffer-substring-no-properties p (+ p 1)) "\t")
	    ;; (overlay-put point-overlay 'display " ")
	    ;; (overlay-put point-overlay 'after-string "\t"))
	    )
	  (when (cdr (assoc 'mark-active (sp/alist l)))
	    (overlay-put region-overlay 'priority 10)
	    (overlay-put region-overlay 'face (list :foreground "black"
						    :background "grey80"))))
	(list region-overlay point-overlay)))))

(defun sp/update-region-recursive (l)
  (when l
    (if (sp/rangep l)
	(let ((alist (sp/alist l)))
	  (progn (setcdr (assoc 'point alist) (copy-marker (point-marker)))
		 (setcdr (assoc 'mark alist) (copy-marker (mark-marker)))))
      (sp/update-region-recursive (car l)))))

;; update: set point and mark of the carmost range in the region to
;; the real point and mark.

(defun sp/update-region ()
  (sp/update-region-recursive sp/region))

(defun sp/update-point-mark-recursive (l)
  (when l
    (if (sp/rangep l)
	(let ((mark-was-active mark-active)
	      (alist (sp/alist l)))
	  (switch-to-buffer (marker-buffer (cdr (assoc 'point alist))))
	  (set-mark (cdr (assoc 'mark alist)))
	  (setcdr (assoc 'mark alist) (copy-marker (mark-marker)))
	  (when (not mark-was-active) (deactivate-mark))
	  (goto-char (cdr (assoc 'point alist)))
	  (setcdr (assoc 'point alist) (copy-marker (point-marker))))
      (sp/update-point-mark-recursive (car l)))))

;; "reverse" update: set point and mark to point and mark of the
;; carmost range in the region.

(defun sp/update-point-mark ()
  (sp/update-point-mark-recursive sp/region))

(defun sp/post-command ()
  (sp/update-region)
  (mapcar 'delete-overlay sp/overlays)
  (setq sp/overlays (sp/update-overlays sp/region)))

(defun sp/recursive-insert (l string)
  ; distinguishing pairs from lists? No, listp won't.
  (if (sp/rangep l)
    (save-excursion
      (with-current-buffer (marker-buffer (sp/range-point l))
	(goto-char (cdr (assoc 'point (sp/alist l))))
	(insert string)
	(setcdr (assoc 'point (sp/alist l)) (copy-marker (point-marker)))))
    (mapcar (lambda (l2) (sp/recursive-insert l2 string)) l)))

(defun sp/insert (string)
  (interactive "s")
  (sp/recursive-insert sp/region string)
  (sp/update-point-mark-recursive sp/region))

(defun sp/extrude-cursor-vertically (count)
  (interactive "Nhow many lines:")
  (let ((ps ()))
    (while (not (equal count 0))
      (setq count (- count 1))
      (setq region (cons (list 'range
			       (cons 'point (copy-marker (point-marker)))
			       (cons 'mark (copy-marker (point-marker)))
			       (cons 'mark-active ()))
			 region))
      (forward-line 1)

      (setq sp/region ps) ; XXX mark stack
      ;;      (sp/update-point-mark)
      )))

(defun sp/extrude-cursor-pointtomark ()
  (interactive)
  )

;; this is ugly, and only distinguishes between simple lists of point-mark and
;; a single pair.

(defun sp/append-range (pm1 pm)
  (if (sp/rangep pm)
      (cons pm (cons pm1 nil))
    (append pm (list pm1))))

;; this is ugly, and only distinguishes between simple lists of point-mark and
;; a single pair.

(defun sp/prepend-range (pm1 pm)
  (if (sp/rangep pm)
      (cons pm1 (cons pm nil))
    (cons pm1 pm)))

(defun sp/find-point-at (p pm)
  (if pm
      (if (sp/rangep pm)
	  (equal (marker-position (cdr (assoc 'point (sp/alist pm))))
		 p)
	(or (sp/find-point-at p (car pm))
	    (sp/find-point-at p (cdr pm))))))

(defun sp/extrude-cursor-up ()
  (interactive)
  (let ((pm sp/region))
    (sp/update-region)
    (while (sp/find-point-at (point) sp/region)
      (previous-line))             ; or forward-line -1 ?
    (set-mark (point))             ; or apply the previous-line commands to mark
    (setq sp/region (sp/prepend-range 
			 (list 'range
			       (cons 'point (copy-marker (point-marker)))
			       (cons 'mark (copy-marker (point-marker))))
			 pm))
    (sp/update-point-mark)))

(defun sp/extrude-cursor-down ()
  (interactive)
  (let ((pm sp/region))
    (while (sp/find-point-at (point) pm)
      (next-line))
    (set-mark (point))
    (setq sp/region (sp/prepend-range
			 (list 'range
			       (cons 'point (copy-marker (copy-marker (point-marker))))
			       (cons 'mark (copy-marker (copy-marker (point-marker)))))
			 pm))
    (sp/update-point-mark)))

(defun sp/carmost-range (pm)
  (if (sp/rangep pm)
      pm
    (sp/carmost-range (car pm))))

(defun sp/drop-carmost-range-recursive (pm)
  (if (sp/rangep (car pm))
      (cdr pm)
    (cons (sp/drop-carmost-range-recursive (car pm))
	  (cdr pm))))

(defun sp/drop ()
  (interactive)
  (setq sp/region (sp/drop-carmost-range-recursive sp/region))
  (sp/update-point-mark))

(defun sp/rotate ()
  (interactive)
  (let* ((pm sp/region)
	 (carmost (sp/carmost-range pm)))
    (setq sp/region
	  (sp/append-range carmost
			   (sp/drop-carmost-range-recursive pm)))
    (sp/update-point-mark)))

(defun sp/split-ranges-recursive (pm regexp)
  (if (sp/rangep pm)
      (let* ((pm2 ())
	     (p (cdr (assoc 'point (sp/alist pm))))
	     (m (cdr (assoc 'mark (sp/alist pm))))
	     (b (if (< p m) p m))
	     (e (if (< p m) m p))
	     last
	     next
	     prev
	     )
	(save-excursion
	  (goto-char b)
	  (while (< (point) e)
	    (setq prev (copy-marker (point-marker)))
	    (setq last (if (re-search-forward regexp e t) (match-beginning 0) e))
	    (if (equal last e) (goto-char e))
	    ;; avoid an initial empty range. Other empty ranges are okay, though.
	    (when (not (equal last b))
	      (setq pm2
		    (append pm2
			    (list (list 'range
					(cons 'point prev)
					(cons 'mark (copy-marker last))
					(cons 'mark-active t))))))
	    
	    ))
	pm2)
    (mapcar (lambda (pm2) (sp/split-ranges-recursive pm2 regexp))
	    pm)))

(defun sp/split-ranges (regexp)
  (interactive "sregexp: ")
  (setq sp/region (sp/split-ranges-recursive sp/region regexp))
  (sp/update-point-mark))
  
(defun sp/kill-lines (beg end)
  "Kill a list of lines. Kills entire rest of line."
  (interactive "r")
  (let ((pm ())
	(kre ())
	(p (if (< beg end) beg end))
	(m (if (< beg end) end beg)))
    (save-excursion
      (goto-char p)
      (while (< (point) m)
	(save-excursion
	  (set-mark (point))
	  (end-of-line)
	  (setq kre (append kre (list (buffer-substring (mark) (point))))))
	(next-line)))
    (push kre sp/kill-ring)))

(defun sp/mark-rectangle (beg end)
  "Mark a rectangle as a list of lines."
  (interactive "r")
  (let ((pm ())
	(kre ())
	(p (if (< beg end) beg end))
	(m (if (< beg end) end beg))
	begcol endcol)
    (save-excursion
      (goto-char m)
      (setq endcol (current-column))
      (goto-char p)
      (setq begcol (current-column))
      (while (<= (point) m)
	(let ((point (copy-marker (point-marker)))
	      (mark (progn
		      (move-to-column endcol)
		      (copy-marker (point-marker)))))
	  (push (list 'range
		      (cons 'point point)
		      (cons 'mark mark)
		      (cons 'mark-active t))
		pm)
	  (move-to-column begcol)
	  (next-line))))
    (setq sp/region (reverse pm))
    (sp/update-point-mark)))

;; do we even need this? It's easy enough to extrude-cursor, set mark, and
;; sp/forward-char.

(defun sp/kill-rectangle (beg end)
  "Kill a rectangle as a list of lines. Kills entire rest of line."
  (interactive "r")
  (let ((pm ())
	(kre ())
	(p (if (< beg end) beg end))
	(m (if (< beg end) end beg))
	startcol endcol)
    (save-excursion
      (goto-char m)
      (setq endcol (current-colum))
      (goto-char p)
      (setq startcol (current-column))
      (while (< (point) m)
	(save-excursion
	  (set-mark (point))
	  (end-of-line)
	  (setq kre (append kre (list (buffer-substring (mark) (point))))))
	(next-line)))
    (push kre sp/kill-ring)))
	  
(defun sp/rotate-contents-recursive (pm)
  (if (sp/rangep (car pm))
      (let* ((kre (car (sp/kill-recursive pm))))
	(setq kre (append (cdr kre) (list (car kre))))
	(message (prin1-to-string kre))
	(sp/yank-recursive pm kre)
	(sp/update-point-mark))
    (mapcar (lambda (pm2) (sp/rotate-contents-recursive pm2)) pm)))

(defun sp/rotate-contents ()
  (interactive)
  (sp/rotate-contents-recursive sp/region))

;; It's more symmetrical to handle self-insert-command like this:
;; first we undo the insertion, then we perform a regular multi-point
;; insertion, which happens to redo the original insertion at point.
;;
;; newline fakes a self-insert. Not nice.
;;
;; BUGS
;;
;; It would be nice, I think, to print a "%d characters inserted"
;; message after the insert to remind the user there are multiple
;; points.
(defun sp/post-self-insert-command ()
  (when (not (equal last-input-event 'return))
    (delete-region (- (point) 1) (point))
    (sp/insert (char-to-string last-input-event))
    ))

(defvar sp/keymap (make-sparse-keymap))

;; about this list: commands that affect the kill ring could be
;; lifted, in theory, but that would make the lifting for all other
;; commands more costly, and it's not clear that's the right thing to
;; do.
;;
;; quoted-insert could be lifted, but I think it's precisely the
;; point of quoting that a single character is inserted.
  
(sp/lift "backward-char")
(sp/lift "backward-delete-char-untabify")
(sp/lift "backward-kill-word")
(sp/lift "backward-sentence")
(sp/lift "backward-sexp")
(sp/lift "beginning-of-defun")
(sp/lift "comment-or-uncomment-region")
(sp/lift "completion-at-point")
(sp/lift "delete-char")
(sp/lift "down-list")
(sp/lift "eval-defun")
(sp/lift "exchange-point-and-mark")
(sp/lift "expand-abbrev")
(sp/lift "forward-char")
(sp/lift "forward-sexp")
(sp/lift "forward-sentence")
(sp/lift "indent-for-tab-command")
(sp/lift "indent-pp-sexp")
(sp/lift "left-char")
(sp/lift "left-word")
(sp/lift "mark-sexp")
(sp/lift "move-beginning-of-line")
(sp/lift "move-end-of-line")
(sp/lift "newline")
(sp/lift "newline-and-indent")
(sp/lift "next-line")
(sp/lift "open-line")
(sp/lift "previous-line")
(sp/lift "recenter-top-bottom")
(sp/lift "right-char")
(sp/lift "right-word")
(sp/lift "scroll-down-command")
(sp/lift "scroll-up-command")
(sp/lift "set-mark-command")
(sp/lift "transpose-chars")
(sp/lift "transpose-lines")
(sp/lift "up-list")
(sp/lift "zap-to-char")

;;(sp/lift "kill-line")
;;(sp/lift "kill-region")
;;(sp/lift "yank")
;;(sp/lift "kill-whole-line")

(substitute-key-definition 'yank 'sp/yank sp/keymap (current-global-map))
(substitute-key-definition 'yank-pop 'sp/yank-pop sp/keymap (current-global-map))
(substitute-key-definition 'kill-region 'sp/kill sp/keymap (current-global-map))
(substitute-key-definition 'kill-line 'sp/kill-line sp/keymap (current-global-map))

; for elisp mode.
(substitute-key-definition 'backward-delete-char-untabify 'sp/backward-delete-char-untabify sp/keymap (current-local-map))

;; DO NOT USE THIS FUNCTION. Thank you. For it to operate properly, we
;; should be a global minor mode rather than a local minor mode. When
;; NOT USING this function, pass 2 as argument.

(defun sp/double-cursor-transbuffer (arg)
  " DO NOT USE "
  (interactive "P")
  (if arg
      (let* ((mr (copy-sequence global-mark-ring))
	     (n (+ 1 (length mr)))
	     (l (append (list (copy-marker (point-marker)))
			(butlast mr (- n arg)))))
	(setq sp/region 
	      (mapcar (lambda (m) (list 'range
					(cons 'point (copy-marker m))
					(cons 'mark (copy-marker m))
					(cons 'mark-active ())))
		      l))
	(sp/update-point-mark) ;; XXX unnecessary
	
  )))
  
(defun sp/double-cursor (arg)
  "Turn point and mark of one active region into the points and
marks of two active (empty) ranges. With a prefix argument, make
as many active (empty) ranges as the prefix by taking marks off
the mark stack."
  (interactive "P")
  (if arg
      (let* ((mr (copy-sequence mark-ring))
	     (n (+ 2 (length mr)))
	     (l (append (list (copy-marker (point-marker))
			      (copy-marker (mark-marker)))
			(butlast mr (- n arg)))))
	(setq sp/region 
	      (mapcar (lambda (m) (list 'range
					(cons 'point (copy-marker m))
					(cons 'mark (copy-marker m))
					(cons 'mark-active ())))
		      l))
	(sp/update-point-mark) ;; XXX unnecessary
	)
    (setq sp/region
	  (list 
	   (list 'range
		 (cons 'point (copy-marker (point-marker)))
		 (cons 'mark (copy-marker (point-marker)))
		 (cons 'mark-active ()))
	   (list 'range
		 (cons 'point (copy-marker (mark-marker)))
		 (cons 'mark (copy-marker (mark-marker)))
		 (cons 'mark-active ())))))
  )

;; Ideally, C-g would run this. I think.
(defun sp/collapse ()
  "Make the active region a single empty range at the current cursor position."
  (interactive)
  (setq sp/region (list 'range
			    (cons 'point (copy-marker (point-marker)))
			    (cons 'mark (copy-marker (point-marker)))
			    (cons 'mark-active mark-active)))
  (sp/update-point-mark))
      
(define-minor-mode multipoint-mode
  "A minor mode which behaves as though point and mark, instead of forming a single pair, could be generalized to a sexp of pairs."
  nil " Multipoint" sp/keymap
  (if multipoint-mode
      (progn (add-hook 'post-command-hook 'sp/post-command nil t)
	     (add-hook 'post-self-insert-hook 'sp/post-self-insert-command nil t)
	     (sp/collapse))
    (progn (remove-hook 'post-command-hook 'sp/post-command t)
	   (remove-hook 'post-self-insert-hook 'sp/post-self-insert-command  t )
	   (remove-overlays)
   )))


;; some suggested key bindings.

(when ()
  (global-set-key (kbd "C-c C-c") 'sp/collapse)
  (global-set-key (kbd "C-c C-d") 'sp/drop)
  (global-set-key (kbd "C-c C-f") 'sp/rotate)
  (global-set-key (kbd "C-c C-2") 'sp/double-cursor)
  (global-set-key (kbd "C-c C-p") 'sp/extrude-cursor-up)
  (global-set-key (kbd "C-c C-n") 'sp/extrude-cursor-down)
  (global-set-key (kbd "C-c C-(") 'sp/rotate-contents))
