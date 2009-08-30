;;; imdbs.el -- compute movies statistic using your imdb vote history
;; imdbs.el is free software
;;
;; CAUTION!  Depending on the lengh of your vote history, your OS and
;; the memory available on your computer, the use of this package can
;; freeze your emacs or make it crash.  You have been warned.
;;
;; To use this program, you must:
;; 
;; 1) vote for all the movie you have seen in your life in the imdb
;; 2) save your vote-history html page "as text" with mozilla.
;; 3) Download the imdnb text files from ftp://ftp.fu-berlin.de/pub/misc/movies/database/
;;    Required are: movies, keywords, genres, countries, actors, actresses and directors
;; 4) set theses variables: 
(defvar imdbs-list-path "~/imdb/lists/")
(defvar imdbs-history-file "~/imdb/lists/list.html")
(defvar imdbs-load-dir "~/imdb")
; the following are the minimum movies required to be included in the stats.
(defvar imdbs-directors-min 3) 
(defvar imdbs-actresses-min 5)
(defvar imdbs-actors-min 5)

(defun imdbs-parse-history ()
  "initialise the imdbs-history-list variable"
  (setq imdbs-historyH (make-hash-table))
  (with-temp-buffer 
   (insert-file imdbs-history-file)
   (goto-char (point-min))
   (while (re-search-forward 
"^[ \t]*\\([^<]*\\)</title/tt\\([0-9][0-9][0-9][0-9][0-9][0-9][0-9]\\)/>[ \t]*\\([0-9]+\\)"
;	   "^[ \t]*\\([^<]*\\)</title\\?\\([0-9][0-9][0-9]+\\)>[ \t]*\\([0-9]+\\)"
	   nil t)
     (let ((title (match-string 1))
	   (code (match-string 2))
	   (vote (match-string 3)))
       (puthash (intern (imdbs-string-strip (delete ?\n title) t t)) 
	      (cons code (car (read-from-string vote)))
	      imdbs-historyH))))
  (hash-table-count imdbs-historyH))

(defun imdbs-get-keywords (movie)
  (let ((kL))
    (find-file (concat imdbs-list-path "keywords.list"))
    (goto-char (point-min))
    (while (re-search-forward 
	    (concat "^" (imdbs-string-strip movie t t) "[\t]+\\(.*\\)$") nil t nil)
      (setq kL (cons (match-string 1) kL)))
    (bury-buffer)
    kL))

(defun imdbs-get-ratings (movie)
  (let ((kL))
    (find-file (concat imdbs-list-path "mpaa-ratings-reasons.list"))
    (goto-char (point-min))
    (while (re-search-forward 
	    (concat "MV: " (imdbs-string-strip movie t t) "\nRE: Rated \\([^ ]*\\) ") nil t nil)
      (setq kL (cons (match-string 1) kL)))
    (bury-buffer)
    kL))

(defun imdbs-get-countries (movie)
  (let ((kL))
    (find-file (concat imdbs-list-path "countries.list"))
    (goto-char (point-min))
    (while (re-search-forward 
	    (concat "^" (imdbs-string-strip movie t t) "[\t]+\\(.*\\)$") nil t nil)
      (setq kL (cons (match-string 1) kL)))
    (bury-buffer)
    kL))

(defun imdbs-get-genres (movie)
  (let ((kL))
    (find-file (concat imdbs-list-path "genres.list"))
    (goto-char (point-min))
    (while (re-search-forward 
	    (concat "^" (imdbs-string-strip movie t t) "[\t]+\\(.*\\)$") nil t nil)
      (setq kL (cons (match-string 1) kL)))
    (bury-buffer)
    kL))
  
(defun imdbs-get-actresses (movie)
  (let ((kL))
    (find-file (concat imdbs-list-path "actresses.list"))
    (goto-char (point-min))
    (while (re-search-forward (imdbs-string-strip (remove ?\n movie) t t) nil t nil)
      (re-search-backward "^\n" nil t nil)
      (next-line 1)
      (setq p (point))
      (re-search-forward "\t" nil t nil)
      (setq kL (cons (buffer-substring p (point)) kL))
      (re-search-forward "\n\n" nil t nil))
    (bury-buffer)
    kL))

(defun imdbs-get-actors (movie)
  (let ((kL))
    (find-file (concat imdbs-list-path "actors.list"))
    (goto-char (point-min))
    (while (re-search-forward (imdbs-string-strip (remove ?\n movie) t t) nil t nil)
      (re-search-backward "^\n" nil t nil)
      (next-line 1)
      (setq p (point))
      (re-search-forward "\t" nil t nil)
      (setq kL (cons (buffer-substring p (point)) kL))
      (re-search-forward "\n\n" nil t nil))
    (bury-buffer)
    kL))

(defun imdbs-get-directors (movie)
  (let ((kL))
    (find-file (concat imdbs-list-path "directors.list"))
    (goto-char (point-min))
    (while (re-search-forward  (concat "\t" (imdbs-string-strip (remove ?\n movie) t t))
			       nil t nil)
      (re-search-backward "^\n" nil t nil)
      (next-line 1)
      (setq p (point))
      (re-search-forward "\t" nil t nil)
      (setq kL (cons (imdbs-string-strip (buffer-substring p (point)) t t) kL))
      (re-search-forward "\n\n" nil t nil))
    (bury-buffer)
    kL))


(defun imdbs-assoc-delete-all (key alist)
  "Delete from ALIST all elements whose car is KEY.
Return the modified alist."
  (let ((tail alist))
    (while tail
      (if (equal (car (car tail)) key)
	  (setq alist (delete (car tail) alist)))
      (setq tail (cdr tail)))
    alist))

(defun imdbs-string-strip (str beforep afterp)
  "Strip STR of any leading (if BEFOREP) and/or trailing (if AFTERP) space."
  (string-match (concat "\\`" (if beforep "\\s-*")
			"\\(.*?\\)" (if afterp "\\s-*\n?")
			"\\'") str)
  (match-string 1 str))

(defun imdbs-print-genres-summary ()
  (interactive)
  (if (not (boundp 'imdbs-historyH)) 
      (imdbs-parse-history))
  (let ((imdbs-genresH (make-hash-table))
	(Hl (hash-table-count imdbs-historyH)))
    (maphash 
     (lambda (K V)
       (let* ((m (symbol-name K))
	      (L (imdbs-get-genres m)))
	 (mapcar
	  (lambda (x)
	    (message (concat m " - " x))
	    (let ((ix (intern x)))
	      (setq num (gethash ix imdbs-genresH))
	      (puthash ix (if num (+ 1 num) 1) imdbs-genresH)))
	  L)))
     imdbs-historyH)
    (switch-to-buffer "*IMDBS_GENRES*")
    (delete-region (point-min)(point-max))
    (maphash 
     (lambda (K V)
       (insert (format "%2d %% [ %-4d] %-20s\n" 
		       (/ (* V 100) Hl) V (symbol-name K))))
   imdbs-genresH)
    (sort-numeric-fields 4 (point-min)(point-max)) 
    (reverse-region (point-min)(point-max))))

(defun imdbs-print-keywords-summary ()
  (interactive)
  (if (not (boundp 'imdbs-historyH)) 
      (imdbs-parse-history))
  (let ((imdbs-keywordsH (make-hash-table))
	(Hl (hash-table-count imdbs-historyH)))
    (maphash 
     (lambda (K V)
       (let* ((m (symbol-name K))
	      (L (imdbs-get-keywords m)))
	 (mapcar
	  (lambda (x)
	    (message (concat m " - " x))
	    (let ((ix (intern x)))
	      (setq num (gethash ix imdbs-keywordsH))
	      (puthash ix (if num (+ 1 num) 1) imdbs-keywordsH)))
	  L)))
     imdbs-historyH)
    (switch-to-buffer "*IMDBS_KEYWORDS*")
    (delete-region (point-min)(point-max))
    (maphash 
     (lambda (K V)
       (insert (format "%-2d%% [%-4d] %-20s\n" 
		       (/ (* V 100) Hl) V (symbol-name K))))
   imdbs-keywordsH)
    (sort-numeric-fields 2 (point-min)(point-max)) 
    (reverse-region (point-min)(point-max))))

(defun imdbs-print-countries-summary ()
  (interactive)
  (if (not (boundp 'imdbs-historyH)) 
      (imdbs-parse-history))
  (let ((imdbs-countriesH (make-hash-table))
	(Hl (hash-table-count imdbs-historyH)))
    (maphash 
     (lambda (K V)
     (if (not (string-match "^\\\"" (symbol-name K)))
	 (let* ((m (symbol-name K))
		(L (imdbs-get-countries m)))
	   (mapcar
	    (lambda (x)
	      (message (concat m " - " x))
	      (let ((ix (intern x)))
		(setq num (gethash ix imdbs-countriesH))
		(puthash ix (if num (+ 1 num) 1) imdbs-countriesH)))
	    L))))
     imdbs-historyH)
    (switch-to-buffer "*IMDBS_COUNTRIES*")
    (delete-region (point-min)(point-max))
    (maphash 
     (lambda (K V)
       (insert (format "%2d%% [ %4d ] %-20s\n" 
		       (/ (* V 100) Hl) V (symbol-name K))))
     imdbs-countriesH)
    (sort-numeric-fields 3 (point-min)(point-max)) 
    (reverse-region (point-min)(point-max))))


(defun imdbs-print-actresses-summary ()
  (interactive)
  (imdbs-parse-history)
  (setq imdbs-actressesH (make-hash-table))
  (maphash 
   (lambda (K V)
     (if (not (string-match "^\\\"" (symbol-name K)))
     (let* ((m (symbol-name K))
	    (L (imdbs-get-actresses m))
	    (s (cdr V)))
       (if L
	   (mapcar
	    (lambda (x)
	      (let* ((ix (intern x))
		     (arg (gethash ix imdbs-actressesH))
		     (sco (car arg))
		     (num (cdr arg)))
		(message (concat m))
		(puthash ix (cons 
			     (if sco (+ sco s) s)
			     (if num (+ 1 num) 1))  
			 imdbs-actressesH)))
	    L)))))
   imdbs-historyH)
  (switch-to-buffer "*IMDBS_ACTRESSES*")
  (delete-region (point-min)(point-max))
  (setq count 0)
  (maphash 
   (lambda (K V)
     (setq count (+ 1 count))
     (if (>= (cdr V) imdbs-actresses-min)
	 (insert (format "%-4g  %-4d %-20s\n" 
			 (/ (round (* 100 (/ (float (car V))
					     (float (cdr V))))) 100.0) 
			 (cdr V) (symbol-name K)))))
   imdbs-actressesH)
  (sort-numeric-fields 1 (point-min)(point-max)) 
  (reverse-region (point-min)(point-max)))

(defun imdbs-print-directors-summary ()
  (interactive)
  (if (not (boundp 'imdbs-historyH)) 
      (imdbs-parse-history))
  (setq imdbs-directorsH (make-hash-table))
  (maphash 
   (lambda (K V)
     (if (not (string-match "^\\\"" (symbol-name K)))
     (let* ((m (symbol-name K))
	    (L (imdbs-get-directors m))
	    (s (cdr V)))
       (if L
	   (mapcar
	    (lambda (x)
	      (let* ((ix (intern x))
		     (arg (gethash ix imdbs-directorsH))
		     (sco (car arg))
		     (num (cdr arg)))
		(message (concat m))
		(puthash ix (cons 
			     (if sco (+ sco s) s)
			     (if num (+ 1 num) 1))  
			 imdbs-directorsH)))
	    L)))))
   imdbs-historyH)
  (switch-to-buffer "*IMDBS_DIRECTORS*")
  (delete-region (point-min)(point-max))
  (setq count 0)
  (maphash 
   (lambda (K V)
     (setq count (+ 1 count))
     (if (>= (cdr V) imdbs-directors-min)
	 (insert (format "%-4g  %-4d %-20s\n" 
			 (/ (round (* 100 (/ (float (car V))
					     (float (cdr V))))) 100.0) 
			 (cdr V) (symbol-name K)))))
   imdbs-directorsH)
  (sort-numeric-fields 1 (point-min)(point-max)) 
  (reverse-region (point-min)(point-max)))

(defun imdbs-print-actors-summary ()
  (interactive)
  (imdbs-parse-history)
  (setq imdbs-actorsH (make-hash-table))
  (maphash 
   (lambda (K V)
     (if (not (string-match "^\\\"" (symbol-name K)))
	   
	 (let* ((m (symbol-name K))
		(L (imdbs-get-actors m))
		(s (cdr V)))
	   (if L
	       (mapcar
		(lambda (x)
		  (let* ((ix (intern x))
			 (arg (gethash ix imdbs-actorsH))
			 (sco (car arg))
			 (num (cdr arg)))
		    (message (concat m))
		    (puthash ix (cons (if sco (+ sco s) s)(if num (+ 1 num) 1))  imdbs-actorsH)))
		L)))))
   imdbs-historyH)
  (switch-to-buffer "*IMDBS_ACTORS*")
  (delete-region (point-min)(point-max))
  (setq count 0)
  (maphash 
   (lambda (K V)
     (setq count (+ 1 count))
     (if (>= (cdr V) imdbs-actors-min)
	 (insert (format "%-4g  %-4d %-20s\n" 
			 (/
			  (round 
			   (* 
			    100 
			    (/ 
			     (float (car V))
			     (float (cdr V))))) 
			  100.0) (cdr V) 
			  (symbol-name K)))))
   imdbs-actorsH)
  (sort-numeric-fields 1 (point-min)(point-max)) 
  (reverse-region (point-min)(point-max)))


(defun imdbs-print-ratings-summary ()
  (interactive)
  (if (not (boundp 'imdbs-historyH)) 
      (imdbs-parse-history))
  (let ((imdbs-ratingsH (make-hash-table))
	(Hl (hash-table-count imdbs-historyH)))
    (maphash 
     (lambda (K V)
       (let* ((m (symbol-name K))
	      (L (imdbs-get-ratings m)))
	 (mapcar
	  (lambda (x)
	    (message (concat m " - " x))
	    (let ((ix (intern x)))
	      (setq num (gethash ix imdbs-ratingsH))
	      (puthash ix (if num (+ 1 num) 1) imdbs-ratingsH)))
	  L)))
     imdbs-historyH)
    (switch-to-buffer "*IMDBS_RATINGS*")
    (delete-region (point-min)(point-max))
    (maphash 
     (lambda (K V)
       (insert (format "%2d %% [ %4d ] %-20s\n" 
		       (/ (* V 100) Hl) V (symbol-name K))))
   imdbs-ratingsH)
    (sort-numeric-fields 4 (point-min)(point-max)) 
    (reverse-region (point-min)(point-max))))

(defun imdbs-get-movies-from (director)
;; this is for debugging only, use imdbs-get-movies-from-director
  (let ((L nil))
    (maphash 
     (lambda (K V) 
       (let ((d (symbol-name K)))
	 (message d)
	 (if (member director (imdbs-get-directors d))
	     (setq L (cons (imdbs-string-strip d t t) L)))))
     imdbs-historyH)
    L))


(defun imdbs-flatten (list)
  "Flatten LIST.  (i.e.  (a (b) c) => (a b c)."
  ;; stolen from gnus
  (cond ((consp list)
	 (apply 'append (mapcar 'mp3player-flatten list)))
	(list
	 (list list))))

(defun imdbs-get-movies-from-director (director)
  (find-file (concat imdbs-list-path "directors.list"))
  (goto-char (point-min))
  (re-search-forward
   (concat "^" director "\t+\\(.*\n\\)+?\n"))
  (setq data (match-string 0))
  (bury-buffer)
  (mapcar 
   (lambda (z)
     (let ((v (gethash (intern z) imdbs-historyH)))
       (cons z (or (cdr v) 0))))
   (mapcar 
    (lambda (y)
      (if (string-match 
	   "^\\(.*\\)\\((as [^)]+)\\|(videos?[^)]+)\\|(uncredited)\\|(action sequences)\\|(episode[^)]+)\\|(segment[^)]+)\\)" y)
	  (imdbs-string-strip (match-string 1 y) nil t)
	y))
    (delete "" (mapcar (lambda (x) (imdbs-string-strip x nil t))
		       (cdr (split-string data "\t")))))))

(defun imdbs-average-of-seen (L)
  (let ((x (delete 0 (mapcar 'cdr L))))
    (if x (format "%-4g" 
		  (/ 
		   (round (* 100 
			     (/ (apply '+ x) 
				(float (length x))))) 100.0)) 0)))

(defun imdbs-average-of-seen (L)
  (let ((x (delete 0 (mapcar 'cdr L))))
    (if x (/ (apply '+ x) (float (length x))) 0)))


(defun imdbs-average-of-same-directors (movie)
  (imdbs-average-of-seen
   (car
    (mapcar 'imdbs-get-movies-from-director 
	    (imdbs-get-directors movie)))))

(defun imdbs-print-years-summary ()
  (interactive)
  (switch-to-buffer "*IMDBS-YEARS*")
  (let ((imdbs-years (make-hash-table)))
    (maphash
     (lambda (K V)
       (let* ((x (symbol-name K))
	      (m (progn (string-match "(\\([0-9][0-9][0-9][0-9]\\)" x)
			(intern (match-string 1 x))))
	      (s (cdr V))
	      (y (gethash m imdbs-years)))
	 (if y
	     (puthash  m (cons (+ (car y) 1) (+ s (cdr y))) imdbs-years)
	   (puthash m (cons 1 s) imdbs-years))))
       imdbs-historyH)
    (maphash
     (lambda (K V)
       (insert (concat (symbol-name K) "     " 
		       (number-to-string (car V)) "     " 
		       (number-to-string (/ (cdr V)(float (car V))))   "\n")))
     imdbs-years))
  (sort-numeric-fields 1 (point-min)(point-max)))

(defun imdbs-sort-by-rating ()
  (interactive)
  (sort-numeric-fields 1 (point-min)(point-max))
  (reverse-region (point-min)(point-max)))

(defun imdbs-sort-by-number-of-movies ()
  (interactive)
  (sort-numeric-fields 2 (point-min)(point-max))
  (reverse-region (point-min)(point-max)))

(defun imdbs-get-movies-with-actor (actor)
  (find-file (concat imdbs-list-path "actors.list"))
  (goto-char (point-min))
  (re-search-forward
   (concat "^" actor "\t+\\(.*\n\\)+?\n"))
  (setq data (match-string 0))
  (bury-buffer)
  (mapcar 
   (lambda (z)
     (let ((v (gethash (intern z) imdbs-historyH)))
       (cons z (or (cdr v) 0))))
   (mapcar 
    (lambda (y)
      (if (string-match 
	   "^\\(.*\\)[[<].*" y)
	  (imdbs-string-strip (match-string 1 y) nil t)
	y))
    (delete "" (mapcar (lambda (x) (imdbs-string-strip x nil t))
		       (cdr (split-string data "\t")))))))

(provide 'imdbs)
;;; imdbs.el ends here
