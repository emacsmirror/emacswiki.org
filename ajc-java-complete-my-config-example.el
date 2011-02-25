;;; ajc-java-complete-my-config-example.el --- Auto Java Completion for GNU Emacs
        
 ;;{{{ yasnippet 的设置

;;;yasnippet ,a autocomplete plugins
  
(require 'yasnippet) ;; 
(yas/initialize)
(yas/load-directory (concat joseph_site-lisp_install_path  "yasnippet-0.6.1c/snippets/"))
(setq yas/prompt-functions '( yas/dropdown-prompt yas/x-prompt  yas/ido-prompt yas/completing-prompt)) ;;设置提示方式，文本/X
;;}}}

;;{{{  auto-complete 的配置

  
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories (concat joseph_site-lisp_install_path  "auto-complete-1.3/ac-dict/") )
(ac-config-default)
;; After completion is started, operations in the following table will be enabled temporarily
;;这几个键是临时的，补全完毕会即不可用
;; TAB,  C-i 	ac-expand 	Completion by TAB         
;; RET,  C-m 	ac-complete 	Completion by RET  同TAB，但是他是级联的，也会完成补全的补全
;; down, M-n 	ac-next 	Select next candidate   选择下一个，我修改为C-n
;; up,   M-p 	ac-previous 	Select previous candidate  
;; C-?,  f1 	ac-help 	Show buffer help       
;; C-s 在出现候选项后，可以C-s 进行过滤，只过滤想要的选项
(setq ac-use-menu-map t) ;;只在菜单出现的出时进行C-n C-p 选择菜单的操作
(define-key ac-menu-map "\C-n" 'ac-next) ;;选择下一个候选项
(define-key ac-menu-map "\C-p" 'ac-previous)
(setq ac-menu-height 20);;设置菜单栏的高度20行
;; that is a case that an user wants to complete without inserting any character or
;; a case not to start auto-complete-mode automatically by settings
;;好像是说在还没有调入任何字符的时候,或者默认没启动auto-complete-mode 时，使用这个快捷键进行补全
(define-key ac-mode-map (kbd "M-1") 'auto-complete)
;;(define-key ac-mode-map (kbd "TAB") 'auto-complete)
(setq ac-use-quick-help nil) ;;不显示帮助信息,默认是启用的
;;; (setq ac-quick-help-delay 10)  ;;或者设置显示帮助的延迟
;;;列在这里，但不用它
;;; (setq ac-auto-start nil);; 将不会进行自动补全，结合ac-set-trigger-key 使用
;; (ac-set-trigger-key "TAB")   ;;当ac-auto-start=nil 时哪个键触发补全
;;(setq ac-auto-start 4)  ;;设置当输入几个字符后开始进行补全
;;(ac-use-comphist nil) 默认会根据用户输入频度调整候选词顺序，不想用可禁用之
(setq ac-comphist-file  (concat joseph_root_install_path "cache/ac-comphist.dat"))

;;使用字典 ~/.dict 
;;或者用这个命令,一个个加入1
;(setq ac-user-dictionary '("aaa" "bbb"))
 ;auto-complete-mode won't be enabled automatically for modes that are not in ac-modes. So you need to set if necessary:
;;将jde-mode 加入到ac-modes ,auto-complete 只对ac-modes 中的mode 开启，如果默认没加入进去，需手工加入
(add-to-list 'ac-modes 'jde-mode)
(add-to-list 'ac-modes 'java-mode)

;(setq ac-ignore-case 'smart);; 智能的处理大小写的匹配 ，当有大写字母的时候不忽略大小写，
(setq ac-ignore-case nil)
;;dwim  do what i mean 
;; * After selecting candidates, TAB will behave as RET
;; * TAB will behave as RET only on candidate remains
;;当用C-n c-p 选中候选项时tab 表现为return 的行为，即令其上屏
(setq ac-dwim  t)

;; (defun my_ac-java-mode-setup ()
;;        (setq ac-sources '( ac-source-filename
;;                            ac-source-files-in-current-dir
;;                            ac-source-yasnippet
;;                            ac-source-semantic
;;                            ac-source-semantic-raw
;;                            ac-source-gtags 
;;                            ac-source-abbrev 
;;                            ac-source-dictionary
;;                             )))
;; (add-hook 'java-mode-hook 'my_ac-java-mode-setup)

 (require 'auto-complete+)
;; add (ac+-apply-source-elisp-faces) to your emacs-lisp-mode-hook.
(setq ac+-filename-ignore-regexp "^#.*#$\\|.*~$\\|^\\./?$\\|^\\.\\./?$\\|^.svn\\|^CVS$\\|^.git$")
(add-hook 'emacs-lisp-mode-hook 'ac+-apply-source-elisp-faces)

;;}}}
;;{{{ Auto Java Complete

;;my config file
(require 'ajc-java-complete-config)
(add-hook 'java-mode-hook 'ajc-java-complete-mode)
(add-hook 'find-file-hook 'ajc-4-jsp-find-file-hook)

;;}}}
