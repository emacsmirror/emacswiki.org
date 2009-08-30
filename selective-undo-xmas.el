;;; selective-undo-xmas.el

;; This file is free software

;; Xemacs port of selective undo.


(defun undo (&optional arg)
  "Undo some previous changes.
Repeat this command to undo more changes.
A numeric argument serves as a repeat count."
  (interactive "*p")
  ;; If we don't get all the way through, make last-command indicate that
  ;; for the following command.
  (setq this-command t)
  (let ((modified (buffer-modified-p))
	(recent-save (recent-auto-save-p)))
    (or (eq (selected-window) (minibuffer-window))
	(display-message 'command "Undo!"))
    (unless (and (eq last-command 'undo)
		 (eq (current-buffer) last-undo-buffer)) ; XEmacs
	(if (region-active-p)
	    (undo-start (region-beginning) (region-end))
	  (undo-start))
	(undo-more 1))
   (undo-more (or arg 1))
    ;; Don't specify a position in the undo record for the undo command.
    ;; Instead, undoing this should move point to where the change is.
   (let ((tail buffer-undo-list)
	 (prev nil))
     (while (car tail)
       (when (integerp (car tail))
	 (let ((pos (car tail)))
	   (if (null prev)
	       (setq buffer-undo-list (cdr tail))
	     (setcdr prev (cdr tail)))
	   (setq tail (cdr tail))
	   (while (car tail)
	     (if (eq pos (car tail))
		 (if prev
		     (setcdr prev (cdr tail))
		   (setq buffer-undo-list (cdr tail)))
	       (setq prev tail))
	     (setq tail (cdr tail)))
	   (setq tail nil)))
       (setq prev tail tail (cdr tail))))
   
   (and modified (not (buffer-modified-p))
	(delete-auto-save-file-if-necessary recent-save)))
  ;; If we do get all the way through, make this-command indicate that.
  (setq this-command 'undo))

;; Deep copy of a list
(defun undo-copy-list (list)
  "Make a copy of undo list LIST."
  (mapcar 'undo-copy-list-1 list))

(defun undo-copy-list-1 (elt)
  (if (consp elt)
      (cons (car elt) (undo-copy-list-1 (cdr elt)))
    elt))

(defun undo-start (&optional beg end)
  "Set `pending-undo-list' to the front of the undo list.
The next call to `undo-more' will undo the most recently made change.
If BEG and END are specified, then only undo elements
that apply to text between BEG and END are used; other undo elements
are ignored.  If BEG and END are nil, all undo elements are used."
  (if (eq buffer-undo-list t)
      (error "No undo information in this buffer"))
  (setq pending-undo-list
	(if (and beg end (not (= beg end)))
	    (undo-make-selective-list (min beg end) (max beg end))
	  buffer-undo-list)))

(defvar undo-adjusted-markers)

(defun undo-make-selective-list (start end)
  "Return a list of undo elements for the region START to END.
The elements come from `buffer-undo-list', but we keep only
the elements inside this region, and discard those outside this region.
If we find an element that crosses an edge of this region,
we stop and ignore all further elements."
  (let ((undo-list-copy (undo-copy-list buffer-undo-list))
	(undo-list (list nil))
	undo-adjusted-markers
	some-rejected
	undo-elt temp-undo-list delta)
    (while undo-list-copy
      (setq undo-elt (car undo-list-copy))
      (let ((keep-this
	     (cond ((and (consp undo-elt) (eq (car undo-elt) t))
		    ;; This is a "was unmodified" element.
		    ;; Keep it if we have kept everything thus far.
		    (not some-rejected))
		   (t
		    (undo-elt-in-region undo-elt start end)))))
	(if keep-this
	    (progn
	      (setq end (+ end (cdr (undo-delta undo-elt))))
	      ;; Don't put two nils together in the list
	      (if (not (and (eq (car undo-list) nil)
			    (eq undo-elt nil)))
		  (setq undo-list (cons undo-elt undo-list))))
	  (if (undo-elt-crosses-region undo-elt start end)
	      (setq undo-list-copy nil)
	    (setq some-rejected t)
	    (setq temp-undo-list (cdr undo-list-copy))
	    (setq delta (undo-delta undo-elt))

	    (when (/= (cdr delta) 0)
	      (let ((position (car delta))
		    (offset (cdr delta)))

		;; Loop down the earlier events adjusting their buffer
		;; positions to reflect the fact that a change to the buffer
		;; isn't being undone. We only need to process those element
		;; types which undo-elt-in-region will return as being in
		;; the region since only those types can ever get into the
		;; output

		(while temp-undo-list
		  (setq undo-elt (car temp-undo-list))
		  (cond ((integerp undo-elt)
			 (if (>= undo-elt position)
			     (setcar temp-undo-list (- undo-elt offset))))
			((atom undo-elt) nil)
			((stringp (car undo-elt))
			 ;; (TEXT . POSITION)
			 (let ((text-pos (abs (cdr undo-elt)))
			       (point-at-end (< (cdr undo-elt) 0 )))
			   (if (>= text-pos position)
			       (setcdr undo-elt (* (if point-at-end -1 1)
						   (- text-pos offset))))))
			((integerp (car undo-elt))
			 ;; (BEGIN . END)
			 (when (>= (car undo-elt) position)
			   (setcar undo-elt (- (car undo-elt) offset))
			   (setcdr undo-elt (- (cdr undo-elt) offset))))
			((null (car undo-elt))
			 ;; (nil PROPERTY VALUE BEG . END)
			 (let ((tail (nthcdr 3 undo-elt)))
			   (when (>= (car tail) position)
			     (setcar tail (- (car tail) offset))
			     (setcdr tail (- (cdr tail) offset))))))
		  (setq temp-undo-list (cdr temp-undo-list))))))))
      (setq undo-list-copy (cdr undo-list-copy)))
    (nreverse undo-list)))

(defun undo-elt-in-region (undo-elt start end)
  "Determine whether UNDO-ELT falls inside the region START ... END.
If it crosses the edge, we return nil."
  (cond ((integerp undo-elt)
	 (and (>= undo-elt start)
	      (<=  undo-elt end)))
	((eq undo-elt nil)
	 t)
	((atom undo-elt)
	 nil)
	((stringp (car undo-elt))
	 ;; (TEXT . POSITION)
	 (and (>= (abs (cdr undo-elt)) start)
	      (<= (abs (cdr undo-elt)) end)))
	((and (consp undo-elt) (markerp (car undo-elt)))
	 ;; This is a marker-adjustment element (MARKER . ADJUSTMENT).
	 ;; See if MARKER is inside the region.
	 (let ((alist-elt (assq (car undo-elt) undo-adjusted-markers)))
	   (unless alist-elt
	     (setq alist-elt (cons (car undo-elt)
				   (marker-position (car undo-elt))))
	     (setq undo-adjusted-markers
		   (cons alist-elt undo-adjusted-markers)))
	   (and (cdr alist-elt)
		(>= (cdr alist-elt) start)
		(<= (cdr alist-elt) end))))
	((null (car undo-elt))
	 ;; (nil PROPERTY VALUE BEG . END)
	 (let ((tail (nthcdr 3 undo-elt)))
	   (and (>= (car tail) start)
		(<= (cdr tail) end))))
	((integerp (car undo-elt))
	 ;; (BEGIN . END)
	 (and (>= (car undo-elt) start)
	      (<= (cdr undo-elt) end)))))

(defun undo-elt-crosses-region (undo-elt start end)
  "Test whether UNDO-ELT crosses one edge of that region START ... END.
This assumes we have already decided that UNDO-ELT
is not *inside* the region START...END."
  (cond ((atom undo-elt) nil)
	((null (car undo-elt))
	 ;; (nil PROPERTY VALUE BEG . END)
	 (let ((tail (nthcdr 3 undo-elt)))
	   (not (or (< (car tail) end)
		    (> (cdr tail) start)))))
	((integerp (car undo-elt))
	 ;; (BEGIN . END)
	 (not (or (< (car undo-elt) end)
		  (> (cdr undo-elt) start))))))

;; Return the first affected buffer position and the delta for an undo element
;; delta is defined as the change in subsequent buffer positions if we *did*
;; the undo.
(defun undo-delta (undo-elt)
  (if (consp undo-elt)
      (cond ((stringp (car undo-elt))
	     ;; (TEXT . POSITION)
	     (cons (abs (cdr undo-elt)) (length (car undo-elt))))
	    ((integerp (car undo-elt))
	     ;; (BEGIN . END)
	     (cons (car undo-elt) (- (car undo-elt) (cdr undo-elt))))
	    (t
	     '(0 . 0)))
    '(0 . 0)))

;;; selective-undo-xmas.el ends here.
