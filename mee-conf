;;; which package should been load??

;; which file should beed loaded??
(setq my-file-list 
      '(
	"mymisc"			; some miscellanous settings.
	"mycedet"			; Emacs development evironment settings.
	"myibus"			; ibus settings(IME).
	"myweblogger"			; blog writting.
	))

(defun load-my-file (file-name-list)
  (dolist (file file-name-list)
    (load (concat 
	   (file-name-directory load-file-name)
	   file))))


(load-my-file my-file-list)

