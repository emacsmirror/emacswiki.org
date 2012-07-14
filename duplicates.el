;;;; Find marked files in a dired buffer and display which ones have identical contents
;;;; by Justin Heyes-Jones, 2011
;;;; How to use... mark files you're interest in the dired window, perhaps with `dired-mark-files-regexp'
;;;; then `dired-show-marked-duplicate-files' will open a buffer with a list of duplicated files

(defvar *duplicate-buffer* nil)

(defun md5-file(filename)
  "Open FILENAME, load it into a buffer and generate the md5 of its contents"
  (interactive "f")
  (with-temp-buffer 
    (insert-file-contents filename)
    (md5 (current-buffer))))

(defun show-duplicate(key value)
  "Given the KEY and VALUE of a map entry for a given md5, if there is more than one filename in the list 
of files then display them as duplicates"
  (if (> (length value) 1)
      (let ((str (format "%d duplicates of %s\n" (length value) (first value))))
	(dolist (filename (rest value))
	  (setf str (concat str (format "%s\n" filename))))
	(insert str))))

(defun dired-show-marked-duplicate-files() 
  "For each marked file in a dired buffer determine which have the same contents"
  (interactive)
  (if (eq major-mode 'dired-mode)
      (let ((md5-map (make-hash-table :test 'equal :size 40)))
	(let ((filenames (dired-get-marked-files)))
	  (dolist (fn filenames)
	    (let ((md5 (md5-file fn)))
	      (let ((map-entry (gethash md5 md5-map nil)))
		(if (null map-entry)
		    (puthash md5 (list fn) md5-map)
		  (puthash md5 (cons fn map-entry) md5-map))))))
	(setf *duplicate-buffer* (get-buffer-create "Duplicated files"))
	(goto-line 1 *duplicate-buffer*)
	(erase-buffer)
	(maphash #'show-duplicate md5-map))
    (error (format "Not a Dired buffer \(%s\)" major-mode))))
