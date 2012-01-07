;;; diatheke.el

;; DTK: grab and insert, into current buffer, a Bible verse
;; - ensure diatheke (a SWORD CLI -- see http://www.crosswire.org/wiki/Frontends:Diatheke) is installed
(defun dtk ()
  (interactive)
  (let* ((book
	 (minibuffer-with-setup-hook 'minibuffer-complete 
	   (completing-read (concat "Book: ")
			    '("Ge" "Ex" "Le"))))
	(ch (read-from-minibuffer "Ch: "))
	(vs (read-from-minibuffer "Vs: "))
	(ch-vs (concat ch ":" vs))) 
    (call-process "diatheke" nil (current-buffer) t "-b" "KJV" "-k" book ch-vs)))
