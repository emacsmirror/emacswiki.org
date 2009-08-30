;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 启动设置 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar LazyCatStartup-execute t
  "Just eval current buffer when startup.")

(when LazyCatStartup-execute
  (newsticker-start+)                   ;启动新闻聚合器
  (server-start)                        ;启动emacs服务
  (emms-history-load)                   ;自动加载播放列表历史
  (fullscreen)                          ;启动时全屏函数
  (gnus-switch)                         ;加载gnus
  (startup-open)                        ;打开指定的文件
  (startup-close)                       ;关闭指定的文件
  (resume-windows 'a)                   ;加载窗口布局
  (generate-tag-table-of-emacs)         ;自动更新 Emacs 的 Tag
  (showtip "Emacs load completed.")     ;显示一个消息, 顺便避免显示showtip的bug
  (setq LazyCatStartup-execute nil)     ;关闭启动标志
  )

(provide 'LazyCatStartup)
