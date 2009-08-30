;;;
;; 加载目录和子目录
(defun add-subdirs-to-load-path (dir)
  "Recursive add directories to `load-path'."
  (let ((default-directory (file-name-as-directory dir)))
    (add-to-list 'load-path dir)
    (normal-top-level-add-subdirs-to-load-path)))
(add-subdirs-to-load-path "~/MyEmacs/Site-Lisp/")

;; 启动时是否连接网络。
;; 如果是， Emacs 会执行一些自动启动一些联网程序
;; 否则不启动
(defvar startup-connect-network t
  "Whether connect network when startup Emacs.")

;; 加载个人设置
(require 'LazyCatFont)                  ;字体
(require 'LazyCatDepend)                ;依赖
(require 'LazyCatRedefine)              ;重定义
(require 'LazyCatKeystoke)              ;按键
(require 'LazyCatCustomize)             ;自定义
(require 'LazyCatSetup)                 ;设置
(require 'LazyCatTheme)                 ;主题
(require 'LazyCatStartup)               ;启动
