;;; dim-google.el
;;
;; Ask google about current region, select word-at-point if no region is
;; selected
;;
;; Allow user to confirm search keywords

(require 'browse-url)

(defun dim:google (keywords)
  "Form a google query URL and give it to browse-url"
  (interactive 
   (list
    (if (use-region-p)
	(buffer-substring (region-beginning) (region-end))
      (read-string "Search google for: " (thing-at-point 'word)))))


  (browse-url 
   (read-string "Browse google URL: " 
		(concat "http://www.google.com/search?q=" 
			(replace-regexp-in-string 
			 "[[:space:]]+"
			 "+"
			 keywords)))))

(global-set-key (kbd "C-c g") 'dim:google)

(provide 'dim-google)
