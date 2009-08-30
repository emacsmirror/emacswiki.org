;;; eval-to-texi.el

;; This is free software
;; Written by José E. Marchesi


(defun eval-to-texi (form &optional tostring)
  "Evaluates FORM, dumping a @lisp texinfo environment with the
action.

If TOSTRING is t, then the @lisp environment is returned into a string,
rather than being inserted into the buffer."
  
  (interactive "sForm to evaluate: ")

  (let (environment-text)

    (setq environment-text 

	  (with-temp-buffer 

	    (condition-case error-description

		(let (expression result output)

		  ;; Begin of the sample
		  (insert "@lisp\n")

		  ;; Dump the form itself into the sample
		  (let ((tform form))

		     (setq tform (replace-regexp-in-string "@" "@@" tform))
		     (setq tform (replace-regexp-in-string "{" "@{" tform))
		     (setq tform (replace-regexp-in-string "}" "@}" tform))

		     (insert tform "\n"))

		  ;; Parse the form to a valid expression
		  (setq expression (read form))



		  ;; Get the result of the eval, and the output if there is one
		  (setq output 
			(with-output-to-string
			  (setq result (prin1-to-string (eval expression)))))

		  ;; If there is any output, dump a @print{} entry into the sample
		  (if (not (equal output ""))

		      (progn
			;; Escape texinfo special characters on the output
			(setq output (replace-regexp-in-string "@" "@@" output))
			(setq output (replace-regexp-in-string "{" "@{" output))
			(setq output (replace-regexp-in-string "}" "@}" output))

			;; Indent multilines
			(setq output (replace-regexp-in-string "\n" "\n          " output))

			(insert "      @print{} " output "\n")))

		  ;; If the expression is a macro, dump an @expansion{}
		  (let ((macroexp (macroexpand expression)))
		    (if (not (equal macroexp expression))  ; macro-p???

			(let ((met (prin1-to-string macroexp)))

			  ;; Escape texinfo special characters on the macro expansion text
			  (setq met (replace-regexp-in-string "@" "@@" met))
			  (setq met (replace-regexp-in-string "{" "@{" met))
			  (setq met (replace-regexp-in-string "}" "@}" met))

			  ;; Indent multilines
			  (setq met (replace-regexp-in-string "\n" "\n          " met))

			  (insert "      @expansion{} " met "\n"))))

		  ;; Escape texinfo special characters on the result
		  (setq result (replace-regexp-in-string "@" "@@" result))
		  (setq result (replace-regexp-in-string "{" "@{" result))
		  (setq result (replace-regexp-in-string "}" "@}" result))

		  ;; Indent multilines
		  (setq result (replace-regexp-in-string "\n" "\n          " result))

		  ;; Dump the @result{} entry into the sample
		  (insert "      @result{} " result "\n"))

	      ;; Was an error => Dump an @error{} entry into the sample with the error
	      ;; description from the interpreter
	      (error (insert "      @error{} " (error-message-string error-description) "\n")))

	    ;; End of the sample
	    (insert "@end lisp")

	    ;; Return buffer's contents
	    (buffer-substring (point-min) (point-max))))


    (if (not tostring)
	(insert environment-text)
      environment-text)))    

;;; eval-to-texi.el ends here.
