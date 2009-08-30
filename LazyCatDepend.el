;;; 搜集的扩展加载
(require 'tooltip)                      ;工具提示
(require 'eldoc)                        ;函数参数提示
(require 'avoid)                        ;鼠标行为
(require 'shell)                        ;Shell模式
(require 'shell-command)                ;增强的shell-command
(require 'ansi-color)                   ;转换shell中的颜色序列为face
(require 'flymake)                      ;实时语法检查
(require 'flymake-shell)                ;对shell模式的实时语法检查
(require 'kill-ring-search)             ;Kill ring 搜索
(require 'highlight-parentheses)        ;智能括号
(require 'top-mode)                     ;top
(require 'pick-backup)                  ;备份浏览
(require 'autoload)                     ;用于'autoload'的处理
(require 'ppindent)                     ;管理C预处理格式
(require 'find-dired-lisp)              ;文件搜索
(require 'rx)                           ;导入正则表达式生成
(require 'sregex)                       ;导入另一个正则表达式生成
(require 'windresize)                   ;窗口缩放
(require 'etags)                        ;源代码导航
(require 'save-abbreviation-mode)       ;LISP别名
(require 'zone)                         ;锁屏
(require 'maxima)                       ;代数计算系统
(require 'imaxima)                      ;用Tex显示代数表达式
(require 'tempbuf)                      ;自动关闭临时buffer
(require 'doxymacs)                     ;文档系统
(require 'info)                         ;Info
(require 'info+)                        ;Info 增强
(require 'apt-sources)                  ;Apt源编辑
(require 'rect)                         ;矩形编辑
(require 'rect-mark)                    ;矩形操作
(require 'term)                         ;终端模拟器
(require 'uniquify)                     ;如果有两个重名buffer, 则再前面加上路径区别
(require 'org)                          ;个人信息管理
(require 'color-moccur)                 ;人性化的搜索功能
(require 'moccur-edit)                  ;搜索编辑
(require 'apt-utils)                    ;APT搜索管理工具
(require 'g)                            ;Google Client, 包括Google日历, 博客, 阅读器
(require 'cycle-buffer)                 ;buffer循环切换
(require 'tramp)                        ;很强大远程文件访问工具
(require 'rcirc)                        ;Emacs里面一个轻量IRC客户端
(require 'rcirc-color)                  ;rcirc高亮, 随机颜色妮称
(require 'rcirc-sound)                  ;CTCP声音支持
(require 'windows)                      ;窗口保存
(require 'festival)                     ;语音合成
(require 'what-domain)                  ;域名查询
(require 'goto-last-change)             ;跳转到最后编辑的地方
(require 'iman)                         ;更好的man手册
(require 'yasnippet)                    ;类似TextMate超酷的模版模式
(require 'showtip)                      ;一种提示信息的弹出小窗口, 基于tooltip
(require 'less)                         ;浏览模式
(require 'google-define)                ;Google 定义查询
(require 'java-mode-indent-annotations) ;Java 缩进
(require 'jde-complete)                 ;Java 补全
(require 'beanshell)                    ;Beanshell
(require 'font-lock)                    ;Font-lock
(require 'abbrev)                       ;用于补全
(require 'jde)                          ;Java Development Environment
(require 'crosshairs)                   ;高亮行和列
(require 'mldonkey)                     ;mldonkey 的管理界面
(require 'wget)                         ;Wget的管理界面
(require 'ansi-color)                   ;shell里面显示颜色
(require 'miniedit)                     ;编辑minibuffer
(require 'buffer-move)                  ;buffer移动功能
(require 'scroll-mode-line-mode)        ;Mode-line滚动
(require 'cc-mode)                      ;C语言模式
(require 'redo)                         ;重做命令
(require 'ascii)                        ;ASCII编码
(require 'ediff)                        ;文件比较功能
(require 'ediff+)                       ;ediff增强
(require 'ctypes)                       ;C模式的类型定义(typedef)语法加亮
(require 'modeline-posn)                ;modeline信息标识
(require 'show-wspace)                  ;空格提示
(require 'windmove)                     ;窗口移动功能
(require 'compile-dwim)                 ;智能编译运行程序
(require 'eassist)                      ;h, cpp文件跳转函数, 支持即时按键选择
(require 'chart)                        ;Emacs的内存使用
(require 'ibuffer)                      ;ibuffer模式
(require 'ido)                          ;ido模式
(require 'tabbar)                       ;标签管理
(require 'browse-kill-ring)             ;超强恢复
(require 'recentf)                      ;打开最近的文件
(require 'xcscope)                      ;cscope
(require 'linum)                        ;显示行号
(require 'smiley)                       ;整加simpley
(require 'emms-source-file)             ;Emms 文件
(require 'emms-source-playlist)         ;Emms 列表
(require 'emms-player-simple)           ;Emms simple
(require 'emms-player-mplayer)          ;Emms mplayer播放器
(require 'emms-playlist-mode)           ;Emms 播放列表模式
(require 'emms-info)                    ;Emms信息模式
(require 'emms-cache)                   ;Emms缓存
(require 'emms-playing-time)            ;Emms 播放时间
(require 'emms-volume)                  ;Emms 声音调节
(require 'emms-lyrics)                  ;Emms 歌词
(require 'emms-setup)                   ;Emms 设置
(require 'emms-player-mpg321-remote)    ;Emms 远程播放
(require 'emms-streams)                 ;Emms 流媒体
(require 'emms-volume-amixer)           ;Emms 声音控制
(require 'emms-lyrics-download)         ;自动下载歌词
(require 'emms-i18n)                    ;自动识别音乐标签的编码
(require 'emms-history)                 ;自动保存和导入上次的播放列表
(require 'emms-browser)                 ;EMMS 浏览器
(require 'emms-cache)                   ;EMMS 缓存
(require 'emms-tag-editor)              ;EMMS 标签编辑器
(require 'executable)                   ;执行解释脚本
(require 'w3m)                          ;W3M
(require 'w3m-search)                   ;w3m搜索
(require 'w3m-bookmark)                 ;w3m书签
(require 'w3m-tabmenu)                  ;W3m标签菜单
(require 'w3m-wget)                     ;W3m wget 集成
(require 'w3m-lnum)                     ;W3M 数字快速导航
(require 'w3m-form)                     ;W3m 表格
(require 'w3m-util)                     ;w3m 工具
(require 'w3m-symbol)                   ;w3m 符号处理
(require 'dired)                        ;Dired
(require 'dired+)                       ;增强dired
(require 'dired-details)                ;Dired详细信息
(require 'dired-details+)               ;Dired详细消息切换
(require 'dired-tar)                    ;在tar文件上按T打包或解包文件
(require 'dired-single)                 ;在Dired模式下用单一的Buffer打开以便于Buffer之间的切换
(require 'dired-x)                      ;Dired增强
(require 'dired-view)                   ;Dired中的文件名跳转
(require 'dired-isearch)                ;dired增量搜索功能
(require 'wdired)                       ;Dired 编辑模式
(require 'smooth-scrolling)             ;平滑滚动
(require 'fvwm-mode)                    ;fvwm模式
(require 'nnmairix)                     ;邮件搜索
(require 'bbdb)                         ;数据库
(require 'bbdb-autoloads)               ;bbdb 自动加载
(require 'shell-history)                ;Shell历史
(require 'paredit)                      ;括号编辑模式
(require 'mwe-log-commands)             ;命令日志
(require 'predictive)                   ;英文助手
(require 'contentswitch)                ;按内容切换缓存或文件
(require 'etest)                        ;测试工具
(require 'fringe-helper)                ;边缘帮助
(require 'newcomment)                   ;关于注释的函数
(require 'icicles)                      ;智能minibuffer补全
(require 'icicles-install)              ;icicles自动安装
(require 'cus-edit+)                    ;cus-edit的增强
(require 'eyedropper)                   ;提取当前光标处的前景和背景色
(require 'help-mode+)                   ;help-mode增强
(require 'help-fns+)                    ;help-fns增强
(require 'lib-requires)                 ;列出Emacs Lisp库的依赖
(require 'timid)                        ;增强find-file和iswitch中的历史搜索
(require 'gtk-look)                     ;以HTML的形式查看GTK文档
(require 'starttls)                     ;SSL加密协议
(require 'smtpmail)                     ;SMTP协议
(require 'supercite)                    ;出色的邮件引用
(require 'eperiodic)                    ;化学元素周期表
(require 'gnus-srvr)                    ;gnus虚拟服务器
(require 'gnus-demon)                   ;Gnus固守程序
(require 'apropos)                      ;帮助系统
(require 'redo+)                        ;Extension redo.el
(require 'uptimes)                      ;记录运行时间
(require 'ub)                           ;强大的撤销系统
(require 'cal-china-x)                  ;中文日历
(require 'ielm)                         ;ELISP交互环境
(require 'traverselisp)                 ;在目录中搜索和替换, 支持AVFS!
(require 'goto-chg)                     ;在最近修改中跳转
(require 'levents)                      ;支持xgtags.el
(require 'gtags)                        ;gtags.el
(require 'xgtags)                       ;gtags的界面
(require 'macros+)                      ;macro的扩展
(require 'switch-to-new-buffer)         ;切换到一个新的buffer
(require 'apropos)                      ;参数查询
(require 'apropos-fn+var)               ;apropos增强
(require 'pp+)                          ;pp增强
(require 'elscreen)                     ;工作区
(require 'breadcrumb)                   ;快速书签导航
(require 'unbound)                      ;查找一个没有绑定的按键
(require 'window-number)                ;窗口数字导航
(require 're-builder)                   ;可视化构建正则表达式
(require 'scim-bridge-zh-si)            ;scim-brige 管理SCIM输入法
(require 'highlight-fixmes-mode)        ;高亮fixme等关键字
(require 'cedet)                        ;集成开发环境
(require 'ecb)                          ;Emacs代码浏览器
(require 'semantic)                     ;semantic
(require 'semantic-ia)                  ;semantic-ia
(require 'hideshow-fringe)              ;hideshow 的 fringe 显示
(require 'cursor-chg)                   ;光标随着状态改变形状和颜色
(require 'crontab-mode)                 ;crontab 模式
(require 'edit-env)                     ;编辑环境变量
(require 'intel-hex-mode)               ;Intel 十六进制模式
(require 'wtf)                          ;查询对话缩写词
(require 'slime)                        ;Common Lisp 的开发环境
(require 'slime-autodoc)                ;在slime模式中显示参数列表
(require 'slime-asdf)                   ;asdf支持
(require 'slime-banner)                 ;bannber支持
(require 'slime-clipboard)              ;SLIME的对象剪切板
(require 'slime-enclosing-context)      ;SLIME分析工具
(require 'slime-fuzzy)                  ;模糊的符号补全
(require 'slime-parse)                  ;分析 Common Lisp 源代码
(require 'slime-references)             ;引用文档, (SBCL)
(require 'slime-scheme)                 ;支持 scheme 程序运行在 Common Lisp
(require 'cldoc)                        ;显示common lisp的操作符和变量信息
(require 'haskell-mode)                 ;Haskell模式
(require 'haskell-ghci)                 ;Haskell GHCi 交互模式
(require 'haskell-indent)               ;Haskell 智能缩进
(require 'eimp)                         ;图像操作
(require 'look-mode)                    ;快速文件浏览模式
(require 'anything)                     ;Anything
(require 'anything-config)              ;Anything config
(require 'anything-complete)            ;Anything 补全
(require 'anything-match-plugin)        ;Anything 匹配算法的人性话提升
(require 'anything-gtags)               ;Anything 结合 Gtags
(require 'anything-c-yasnippet)         ;Anything yasnippet
(require 'anything-c-moccur)            ;Anything 和 moccur搜索
(require 'anything-etags)               ;Anything etags
(require 'sys-apropos)                  ;系统相关查询
(require 'ireplace)                     ;isearch 模式中的替换函数
(require 'file-journal)                 ;文件按日期访问
(require 'babel)                        ;网络翻译接口
(require 'auto-complete)                ;自动补全
(require 'auto-complete-cpp)            ;集成 auto-complete 和 C++
(require 'auto-complete-emacs-lisp)     ;集成 auto-complete 和 emacs-lisp
(require 'auto-complete-gtags)          ;集成 auto-complete 和 gtags
(require 'auto-complete-semantic)       ;集成 auto-complete 和 semantic
(require 'auto-complete-yasnippet)      ;集成 auto-complete 和 yasnippet
(require 'auto-complete-css)            ;集成 auto-complete 和 css
(require 'ac-dabbrev)                   ;auto-complete 的 `dabbrev' 支持
(require 'boxquote)                     ;文本引用工具
(require 'erc)                          ;IRC聊天
(require 'erc-highlight-nicknames)      ;不同昵称的颜色
(require 'files+)                       ;file.el 增强
(require 'ls-lisp+)                     ;ls-lisp.el 增强
(require 'magit)                        ;git 函数
(require 'egg)                          ;基于 magit 的界面
(require 'egg-grep)                     ;egg grep 支持
(require 'sr-speedbar)                  ;集成speedbar到当前frame
(require 'winpoint)                     ;记录每一个窗口buffer的特定位置
(require 'xray)                         ;显示Emacs对象的内部结构
(require 'ispell)                       ;拼写检查
(require 'color-grep)                   ;彩色显示 grep buffer
(require 'grep-edit)                    ;grep 编辑的高级模式
(require 'hs-lint)                      ;`hs-lint' 建议
(require 'find-lisp)                    ;查找lisp文件
(require 'hl-sexp)                      ;高亮 sexp
(require 'jump-dls)                     ;查找符号的定义
(require 'auto-document)                ;自动生成文档
(require 'highlight-cl)                 ;高亮 `cl' 函数
(require 'company)                      ;代码自动补全

;;; 自己的扩展加载
(require 'basic-edit-toolkit)           ;基础编辑包
(require 'fullscreen)                   ;全屏
(require 'shell-command-extension)      ;shell command 扩展
(require 'lazycat-toolkit)              ;工具包
(require 'wget-extension)               ;wget 扩展
(require 'find-func-extension)          ;`find-func' 的扩展
(require 'window-extension)             ;窗口增强函数
(require 'buffer-extension)             ;缓存增强函数
(require 'lazy-set-key)                 ;懒惰按键设置
(require 'sdcv)                         ;星际译王的支持
(require 'w3m-extension)                ;W3m扩展
(require 'thing-edit)                   ;基于thingatpt的编辑扩展
(require 'thing-edit-extension)         ;thing-edit 增强
(require 'rcirc-notify+)                ;rcirc提醒
(require 'rcirc-extension)              ;rcirc的扩展
(require 'doc-view-extension)           ;doc-view扩展
(require 'org-extension)                ;Org增强
(require 'org-w3m)                      ;Org w3m 交互转换
(require 'alarm)                        ;闹钟
(require 'mail-notify)                  ;邮件提醒
(require 'gnus-notify+)                 ;Gnus提醒
(require 'speedbar-extension)           ;speedbar扩展
(require 'etags-extension)              ;etags扩展
(require 'multi-term)                   ;多标签SHELL
(require 'lazycat-c-style)              ;C 的编程风格
(require 'festival-extension)           ;Festival扩展
(require 'emms-extension)               ;emms扩展
(require 'paredit-extension)            ;Paredit扩展
(require 'xgtags-extension)             ;xgtags的扩展
(require 'c-mode-extension)             ;C 模式的扩展
(require 'newsticker-extension)         ;newsticker扩展
(require 'scim-bridge-extension)        ;scim-brige 的一些扩展
(require 'dired-extension)              ;dired 的一些扩展
(require 'wdired-extension)             ;wdired 的扩展
(require 'dired-sort)                   ;排序 dired 文件
(require 'dired-open)                   ;在 dired 中打开不同文件
(require 'flymake-extension)            ;flymake 的一些扩展
(require 'haskell-extension)            ;Haskell的一些扩展
(require 'show-help)                    ;用 showtip 显示帮助信息
(require 'rect-extension)               ;矩形编辑扩展
(require 'stripe-buffer)                ;斑纹buffer
(require 'auto-scroll)                  ;自动滚屏
(require 'tabbar-extension)             ;Tabbar 的扩展
(require 'auto-complete-extension)      ;Auto-complete 的扩展
(require 'erc-extension)                ;ERC的一些扩展
(require 'erc-nick-notify)              ;ERC 消息提醒
(require 'lisppaste-extension)          ;lisppaste 的扩展
(require 'eldoc-extension)              ;Eldoc扩展
(require 'gnus-switch)                  ;智能gnus切换
(require 'gnus-summary-stripe)          ;Gnus Summary 斑纹化
(require 'paste2)                       ;快速粘贴
(require 'multi-shell)                  ;多shell管理器
(require 'auto-install)                 ;自动下载安装elisp
(require 'auto-install-extension)       ;auto-install.el 扩展
(require 'doi)                          ;Do Or Insert
(require 'doi-extension)                ;doi 的扩展
(require 'anything-extension)           ;Anything 的一些扩展
(require 'anything-auto-install)        ;auto-install 和 anything 集成
(require 'anything-emms)                ;emms 和 anything 集成
(require 'anything-irfc)                ;irfc 和 anything 集成
(require 'newsticker-notify)            ;newsticker提醒
(require 'isearch-extension)            ;isearch扩展
(require 'one-key)                      ;one-key
(require 'one-key-config)               ;one-key 扩展
(require 'lazy-search)                  ;懒惰搜索
(require 'lazy-search-extension)        ;lazy-seach 扩展
(require 'gnus-extension)               ;gnus扩展
(require 'ispell-extension)             ;ispell扩展
(require 'yaoddmuse)                    ;另一种 oddmuse 模式
(require 'yaoddmuse-extension)          ;yaoddmuse 的扩展
(require 'go-to-char)                   ;跳转到某个字符
(require 'elisp-depend)                 ;查找elisp文件依赖
(require 'irfc)                         ;RFC 文档阅读
(require 'oicq)                         ;QQ
(require 'elisp-format)                 ;elisp 代码格式化
(require 'multi-scratch)                ;多重草稿
(require 'chm-view)                     ;CHM 文件阅读
(require 'org-oddmuse)                  ;转换 Org-mode 到 Oddmuse 模式

(provide 'LazyCatDepend)
