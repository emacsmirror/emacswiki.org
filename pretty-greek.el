(defun pretty-greek ()
	               ;;first upper
  (let* ((greek '("Alpha" "Beta" "Gamma" "Delta" "Epsilon" "Zeta" "Eta" "Theta"
								 "Iota" "Kappa" "Lambda" "Mu" "Nu" "Xi" "Omicron" "Pi" "Rho" 
								 "Ò" "Sigma" "Tau" "Upsilon" "Phi" "Chi" "Psi" "Omega" 
								 ;;hack to skip these characters
								 "Ú" "Û" "Ü" "Ý" "Þ" "ß" "à"
								 ;;lower starts after them
								 "alpha" "beta" "gamma" "delta" "epsilon" "zeta" "eta" "theta" "iota" "kappa" "lambda" "mu" "nu" "xi" "omicron" 
								 "pi" "rho" "sigma_final" "sigma" "tau" "upsilon" "phi" "chi" "psi" "omega"))
				(pos 65))
		(while greek
			(let* ((word (car greek))
						 (greek-char (make-char 'greek-iso8859-7 pos)))
				;;sigh side effect hackery
				(setq greek (cdr greek))
				(setq pos (+ pos 1))
				(font-lock-add-keywords nil
																`((,(concat "\\(^\\|[^a-zA-Z0-9]\\)\\(" word "\\)[a-zA-Z]")
																	 (0 (progn (decompose-region
																							(match-beginning 2)
																							(match-end 2))
																						 nil)))))
				(font-lock-add-keywords nil 
																`((,(concat "\\(^\\|[^a-zA-Z0-9]\\)\\(" word "\\)[^a-zA-Z]")
																	 (0 (progn (compose-region 
																							(match-beginning 2) 
																							(match-end 2)
																							,greek-char) 
																						 nil)))))))))
  
(provide 'pretty-greek)
