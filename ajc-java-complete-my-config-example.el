;;; ajc-java-complete-my-config-example.el --- Auto Java Completion for GNU Emacs
        
;;{{{ yasnippet  
 
 
  (add-to-list 'load-path
               (expand-file-name (concat  "~/.emacs.d/" "yasnippet-0.6.1c/")))  
(require 'yasnippet) ;; 
(yas/initialize)
(yas/load-directory (concat   "~/.emacs.d/" "yasnippet-0.6.1c/snippets/"))
(setq yas/prompt-functions '( yas/dropdown-prompt yas/x-prompt  yas/ido-prompt yas/completing-prompt))  

;;}}}
;;{{{  auto-complete 
(eval-and-compile
  (add-to-list 'load-path
               (expand-file-name (concat "~/.emacs.d/" "auto-complete-1.3/"))) )
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories (concat  "~/.emacs.d/" "auto-complete-1.3/ac-dict/") )
(ac-config-default)
;; After completion is started, operations in the following table will be enabled temporarily
;; TAB,  C-i 	ac-expand 	Completion by TAB         
;; RET,  C-m 	ac-complete 	Completion by RET 
;; down, M-n 	ac-next 	Select next candidate   
;; up,   M-p 	ac-previous 	Select previous candidate  
;; C-?,  f1 	ac-help 	Show buffer help       
;; C-s 
(setq ac-use-menu-map t) 
(define-key ac-menu-map "\C-n" 'ac-next) 
(define-key ac-menu-map "\C-p" 'ac-previous)
(setq ac-menu-height 20)
;; that is a case that an user wants to complete without inserting any character or
;; a case not to start auto-complete-mode automatically by settings

(define-key ac-mode-map (kbd "M-1") 'auto-complete)
;;(define-key ac-mode-map (kbd "TAB") 'auto-complete)
(setq ac-use-quick-help nil) 
;;; (setq ac-quick-help-delay 10)

;;; (setq ac-auto-start nil);; 
;; (ac-set-trigger-key "TAB")  
;;(setq ac-auto-start 4)  
;;(ac-use-comphist nil) 
;;(setq ac-user-dictionary '("aaa" "bbb"))
 ;auto-complete-mode won't be enabled automatically for modes that are not in ac-modes. So you need to set if necessary:
;;
(add-to-list 'ac-modes 'jde-mode)
(add-to-list 'ac-modes 'java-mode)

;(setq ac-ignore-case 'smart);;
(setq ac-ignore-case nil)
;;dwim  do what i mean 
;; * After selecting candidates, TAB will behave as RET
;; * TAB will behave as RET only on candidate remains
(setq ac-dwim  t)
(defun my_ac-java-mode-setup ()
       (setq ac-sources '( ac-source-filename
                           ac-source-files-in-current-dir
                           ac-source-yasnippet
                           ac-source-semantic
                           ac-source-semantic-raw
                           ac-source-gtags 
                           ac-source-abbrev 
                           ac-source-dictionary
                           ac-source-words-in-all-buffer
                            )))

;;}}} 

;;my config file

  (add-to-list 'load-path
               (expand-file-name (concat  "~/.emacs.d/" "ajc-java-complete/"))) 
(require 'ajc-java-complete-config)
