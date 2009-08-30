;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;按键设置;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ### Unset key ###
;;; --- 卸载按键
(lazy-unset-key                         ;全局按键的卸载
 '("C-z" "C-q" "s-W" "s-z" "M-h" "C-x C-c"))
;;; ### Sdcv ###
;;; --- 星际译王命令行
(defvar sdcv-key-alist nil
  "The key alist that sdcv.")
(setq sdcv-key-alist
      '(("p" . sdcv-search-pointer)     ;光标处的单词, buffer显示
        ("y" . sdcv-search-pointer+)    ;光标处的单词, tooltip显示
        ("i" . sdcv-search-input)       ;输入的单词, buffer显示
        (";" . sdcv-search-input+)))    ;输入的单词, tooltip显示
(lazy-set-key sdcv-key-alist nil "C-z") ;sdcv的全局按键绑定
;;; ### Vi-move ###
;;; --- Vi式移动
(defvar vi-move-key-alist nil
  "The key alist that like vi move.")
(setq vi-move-key-alist
      '(("j" . next-line)               ;上一行
        ("k" . previous-line)           ;下一行
        ("h" . backward-char)           ;向后移动
        ("l" . forward-char)            ;向前移动
        ("e" . scroll-down)             ;向下滚动一屏
        ("SPC" . scroll-up)))           ;向上滚动一屏
;;; ### Doi ###
;;; --- Do Or Insert
(defvar doi-key-alist nil
  "The key alist that like doi.")
(setq doi-key-alist
      '(("SPC" . doi-scroll-up)            ;向上滚动一屏
        ("e" . doi-scroll-down)            ;向下滚动一屏
        ("k" . doi-previous-line)          ;上一行
        ("j" . doi-next-line)              ;下一行
        ("h" . doi-backward-char)          ;后一个字符
        ("l" . doi-forward-char)           ;前一个字符
        ("J" . doi-scroll-up-one-line)     ;向上滚动一行
        ("K" . doi-scroll-down-one-line)   ;向下滚动一行
        ("H" . doi-backward-word)          ;后一个词
        ("L" . doi-forward-word)           ;前一个词
        ("y" . doi-sdcv-search-pointer+)   ;翻译
        ("s" . doi-isearch-forward)        ;向前搜索
        ("r" . doi-isearch-backward)       ;向后搜索
        ("A" . doi-move-beginning-of-line) ;移动到行首
        ("E" . doi-move-end-of-line)       ;移动到行末
        ("," . doi-end-of-buffer)          ;移动到buffer末尾
        ("." . doi-beginning-of-buffer)    ;移动到buffer开头
        ))
;;; ### Rect ###
;;; --- 矩形操作
(lazy-set-key
 '(
   ("s-M" . rm-set-mark)                         ;矩形标记
   ("s-X" . rm-exchange-point-and-mark)          ;矩形对角交换
   ("s-D" . rm-kill-region)                      ;矩形删除
   ("s-S" . rm-kill-ring-save)                   ;矩形保存
   ("s-Y" . yank-rectangle)                      ;粘帖矩形
   ("s-O" . open-rectangle)                      ;用空白填充矩形, 并向右移动文本
   ("s-C" . clear-rectangle)                     ;清空矩形
   ("s-T" . string-rectangle)                    ;用字符串替代矩形的每一行
   ("s-I" . string-insert-rectangle)             ;插入字符串在矩形的每一行
   ("s-F" . delete-whitespace-rectangle)         ;删除矩形中空格
   ("s-:" . mark-rectangle-to-end)               ;标记矩形到行末
   ("s-H" . execute-command-with-region-replace) ;在选择的区域中执行命令并替换
   ("s-P" . execute-command-with-region-kill)    ;在选择的区域中执行命令并删除
   ("s-\"" . copy-rectangle-to-register)         ;拷贝矩形到寄存器
   ))
;;; ### Customize ###
;;; --- 自定义模式
(lazy-set-key doi-key-alist custom-mode-map)          ;doi 的局部按键绑定
;;; ### Compilation ###
;;; --- 编译模式
(lazy-set-key sdcv-key-alist compilation-mode-map)    ;sdcv 按键局部绑定
(lazy-set-key vi-move-key-alist compilation-mode-map) ;vi 模式按键局部绑定
;;; ### Font ###
;;; --- 字体命令
(lazy-set-key
 '(
   ("s--" . text-scale-decrease)        ;减小字体大小
   ("s-=" . text-scale-increase)        ;增加字体大小
   ("M--" . text-scale-decrease-global) ;减少字体大小, 全局
   ("M-+" . text-scale-increase-global) ;增加字体大小, 全局
   ("M-=" . text-scale-default-global)  ;恢复字体大小, 全局
   ))
;;; ### Winner-mode ###
;;; --- 窗口设置撤销或返回
(lazy-set-key
 '(
   ("s-<" . winner-undo)                ;窗口设置撤销
   ("s->" . winner-redo)                ;窗口设置返回
   ))
;;; ### Scim-bridge ###
;;; --- SCIM Bridge 的 emacs 接口
(scim-define-common-key (kbd "s-SPC") t) ;scim切换键， 对应于SCIM中的GUI设置
(scim-define-common-key (kbd "C-SPC") nil)
(scim-define-common-key (kbd "C-\\") nil)
;;; ### Yoaddmuse ###
;;; --- Yet another oddmuse mode
(lazy-set-key
 '(
   ("M-s-;" . one-key-menu-yaoddmuse)   ;yaoddmuse 菜单
   ))
;;; ### Thingh-edit ###
;;; --- 增强式编辑当前光标的对象
(lazy-set-key
 '(
   ("M-s-h" . one-key-menu-thing-edit)  ;thing-edit 菜单
   ))
;;; ### Tabbar ###
;;; --- 多标签浏览
(lazy-set-key
 '(
   ("M-7" . tabbar-backward-tab)              ;移动到后一个标签
   ("M-8" . tabbar-forward-tab)               ;移动到前一个标签
   ("M-9" . tabbar-backward-group)            ;移动到后一个标签组
   ("M-0" . tabbar-forward-group)             ;移动到前一个标签组
   ("M-&" . tabbar-backward-tab-other-window) ;向前移动其他窗口的标签
   ("M-*" . tabbar-forward-tab-other-window)  ;向后移动其他窗口的标签
   ("M-s-7" . tabbar-select-beg-tab)          ;移动到最左边的标签
   ("M-s-8" . tabbar-select-end-tab)          ;移动到最右边的标签
   ))
;;; ### EMMS ###
;;; --- Emacs 多媒体系统
(lazy-set-key
 '(
   ("C-c p" . one-key-menu-emms)        ;播放器菜单
   ("<up>" . emms-volume-mode-plus)     ;增加音量
   ("<down>" . emms-volume-mode-minus)  ;减少音量
   ("<left>" . emms-seek-backward)      ;后退
   ("<right>" . emms-seek-forward)      ;前进
   ("M-A" . emms-pause)                 ;暂停/播放
   ("M-X" . emms-random)                ;随机播放
   ("M-Z" . emms-stop)                  ;停止
   ))
;;; ### Emms Playlist ###
;;; --- EMMS 播放列表
(lazy-unset-key
 '("s" "m" "u" "M-<" "M->")
 emms-playlist-mode-map)                ;卸载按键
(lazy-set-key
 '(
   ("C-x C-s" . emms-playlist-save)             ;保存播放列表
   ("C-y" . emms-playlist-mode-yank)            ;剪切
   ("C-k" . emms-playlist-mode-kill-track)      ;删除当前TRACK
   ("C-w" . emms-playlist-mode-kill)            ;删除
   ("C-/" . emms-playlist-mode-undo)            ;撤销
   ("J" . scroll-up-one-line)                   ;向上滚动一行
   ("K" . scroll-down-one-line)                 ;向下滚动一行
   ("." . emms-playlist-mode-first)             ;浏览最上面一行
   ("," . emms-playlist-mode-last)              ;浏览最下面一行
   ("C-j" . emms-playlist-mode-insert-newline)  ;新建一行
   ("M-y" . emms-playlist-mode-yank-pop)        ;YANK弹出
   ("M-n" . emms-playlist-mode-next)            ;下一个播放列表
   ("M-p" . emms-playlist-mode-previous)        ;上一个播放列表
   ("a" . emms-playlist-mode-add-contents)      ;向当前播放列表添加内容
   ("d" . emms-playlist-mode-kill-entire-track) ;从播放列表中移除当前TRACK
   ("C" . emms-playlist-mode-clear)             ;清空当前的播放列表
   ("f" . emms-playlist-mode-play-smart)        ;播放当前TRACK
   ("b" . emms-playlist-set-playlist-buffer)    ;设定当前播放列表BUFFER
   ("n" . emms-next)                            ;播放下一首
   ("p" . emms-previous)                        ;播放上一首
   ("r" . emms-random)                          ;随机播放下一首
   (">" . emms-seek-forward)                    ;前进
   ("<" . emms-seek-backward)                   ;后退
   ("X" . emms-pause)                           ;暂停
   ("T" . emms-stop)                            ;停止
   ("Z" . emms-show)                            ;显示播放信息
   ("q" . emms-playlist-mode-bury-buffer)       ;退出
   ("?" . describe-mode)                        ;帮助
   ("g" . emms-playlist-mode-center-current)    ;跳转到当前播放TRACK
   ("G" . emms-jump-to-file)                    ;定位当前音乐文件的位置
   ("D" . emms-delete-file-from-disk)           ;丛磁盘删除当前的文件
   (";" . emms-tag-editor-edit-marked-tracks)   ;编辑标记的TAG
   ("H" . emms-last-mark-track)                 ;最后一个标记
   ("L" . emms-first-mark-track)                ;第一个标记
   ("N" . emms-next-mark-track)                 ;下一个标记
   ("P" . emms-prev-mark-track)                 ;上一个标记
   ("s" . one-key-menu-emms-playlist-sort)      ;列表排序菜单
   ("m" . one-key-menu-emms-playlist-mark)      ;列表标记菜单
   )
 emms-playlist-mode-map
 )
(lazy-set-key vi-move-key-alist emms-playlist-mode-map) ;vi-move 的局部按键
;;; ### Emms Tag Editor ###
;;; --- Emms 标签编辑器
(lazy-set-key
 '(
   ("C-c C-j" . emms-tag-editor-next-same-field)  ;下一个相同的区域
   ("C-c C-k" . emms-tag-editor-prev-same-field)  ;上一个相同的区域
   ("C-c C-r" . emms-tag-editor-set-all+)         ;替换所有标签
   ("C-c C-l" . emms-tag-editor-set-tracknumber)  ;插入轨迹序号, 要确认
   ("C-c C-i" . emms-tag-editor-set-tracknumber+) ;插入轨迹序号, 不用确认
   )
 emms-tag-editor-mode-map
 )
;;; ### EMMS Browser ###
;;; --- EMMS 浏览器
(lazy-set-key
 '(
   ("J" . emms-browser-next-non-track)      ;下一个节点
   ("K" . emms-browser-prev-non-track)      ;上一个节点
   ("f" . emms-browser-toggle-subitems)     ;显示
   ("s" . one-key-menu-emms-browser-search) ;搜索菜单
   ("L" . one-key-menu-emms-browser-lookup) ;查询菜单
   )
 emms-browser-mode-map
 )
(lazy-set-key sdcv-key-alist emms-browser-mode-map)    ;sdcv 的局部按键
(lazy-set-key vi-move-key-alist emms-browser-mode-map) ;vi-move 的局部按键
;;; ### EMMS Stream ###
;;; --- EMMS 流媒体
(lazy-set-key
 '(
   ("a" . emms-stream-add-bookmark)          ;添加
   ("d" . emms-stream-delete-bookmark)       ;删除
   ("E" . emms-stream-edit-bookmark)         ;编辑
   ("q" . emms-stream-quit)                  ;退出
   ("s" . emms-stream-save-bookmarks-file)   ;保存
   ("t" . emms-stream-toggle-default-action) ;切换
   ("i" . emms-stream-info-bookmark)         ;信息
   ("f" . emms-stream-play)                  ;播放
   )
 emms-stream-mode-map
 )
(lazy-set-key vi-move-key-alist emms-stream-mode-map) ;vi-move 的局部按键
;;; ### ECB ###
;;; --- Emacs 代码浏览器
(lazy-set-key
 '(
   ("s-c s-c" . ecb-toggle-visible)     ;切换ECB
   ("s-i" . one-key-menu-ecb)           ;ECB菜单
   ))
;;; ### Dired ###
;;; --- 文件浏览器
(lazy-set-key
 '(
   ("h" . dired-next-subdir)                   ;下一个子目录
   ("l" . dired-prev-subdir)                   ;上一个子目录
   ("j" . dired-next-file-line)                ;下一行
   ("k" . dired-previous-file-line)            ;上一行
   ("n" . dired-next-dirline)                  ;下一个目录
   ("p" . dired-prev-dirline)                  ;上一个目录
   ("f" . dired-find-file+)                    ;打开当前文件或目录
   ("C-m" . dired-find-file+)                  ;打开当前文件或目录
   ("P" . dired-do-kill-lines)                 ;删除标记的行
   ("4" . dired-serial-rename)                 ;批量重命名
   ("5" . dired-translate-to-html)             ;转换到HTML格式
   ("7" . dired-move-to-last-file)             ;移动到最后一个文件
   ("8" . dired-move-to-first-file)            ;移动到第一个文件
   ("9" . auto-install-from-dired)             ;自动从EmacsWiki安装标记的文件
   ("E" . dired-touch-now)                     ;Touch命令
   ("z" . dired-do-moccur)                     ;搜索dired
   ("I" . image-dired)                         ;打开浏览模式
   ("w" . wdired-change-to-wdired-mode)        ;切换到dired编辑模式
   ("W" . dired-x-find-file)                   ;查找文件
   ("\"" . find-lisp-find-dired)               ;查找特定的lisp文件
   ("J" . dired-goto-file)                     ;跳到某个文件
   ("K" . dired-open-file)                     ;用W3M打开各种文件
   ("X" . traverse-cp-or-mv-extfiles-in-dir)   ;拷贝或移动目录下指定扩展名的文件
   ("V" . traverse-dired-browse-archive)       ;浏览压缩文件
   (";" . dired-view-minor-mode-toggle)        ;字母输入导航模式
   ("," . dired-diff)                          ;比较文件
   ("'" . dired-up-directory-single)           ;返回上一级目录
   ("C-s" . dired-isearch-forward)             ;向后搜索
   ("C-r" . dired-isearch-backward)            ;向前搜索
   ("ESC C-s" . dired-isearch-forward-regexp)  ;向前正则表达式搜索
   ("ESC C-r" . dired-isearch-backward-regexp) ;向后正则表达式搜索
   ("SPC" . scroll-up)                         ;向下翻页
   ("e" . scroll-down)                         ;向上翻页
   ("c" . kill-this-buffer)                    ;关闭当前标签
   ("/" . copy-buffer-file-name-as-kill)       ;显示路径或名称
   ("[" . dired-rename-with-copy)              ;重命名函数
   ("]" . dired-nautilus)                      ;用 Nautils 加载当前目录
   ("{" . dired-gnome-open-file)               ;用GNOME方式打开文件
   ("s" . one-key-menu-dired-sort)             ;排序
   ("?" . dired-get-size)                      ;得到文件的大小
   ("M-o" . dired-toggle-omit)                 ;切换忽略状态
   )
 dired-mode-map
 )
;;; ### Wdired ###
;;; --- Dired 的编辑模式
(lazy-set-key
 '(
   ("C-c C-e" . wdired-format-filename) ;格式化文件名
   )
 wdired-mode-map
 )
;;; ### W3m ###
;;; --- 网页浏览器
(lazy-set-key
 '(
   ("C-z C-z" . w3m)                          ;启动W3M
   ("C-z z" . w3m-startup-background)         ;启动W3M, 后台
   ("C-x C-z" . toggle-w3m-with-other-buffer) ;在W3M和buffer间切换
   ("s-W" . one-key-menu-w3m-search)          ;w3m 搜索菜单
   ))
(lazy-set-key
 '(("1" . emms-play-online)                             ;在线听音乐
   ("2" . kill-google-define-windows)                   ;关闭Google定义窗口
   ("3" . google-define)                                ;查找输入单词的Google定义
   ("4" . google-define-pointer)                        ;查找当前光标处的Google定义
   ("5" . w3m-open-rcirc-window)                        ;打开RCIRC窗口
   ("6" . w3m-session-save)                             ;保存浏览纪录
   ("7" . w3m-session-select)                           ;加载退出前的浏览器纪录
   ("8" . w3m-emacswiki-view-other-version)             ;查看当前wiki页面的其他版本
   ("9" . w3m-auto-install-elisp)                       ;自动安装elisp文件
   ("0" . w3m-gmail-toggle-mark)                        ;切换标记选项框
   ("(" . w3m-gmail-mark-all)                           ;标记选项框
   (")" . w3m-gmail-unmark-all)                         ;取消标记选项框
   ("c" . w3m-delete-buffer-and-select-right)           ;关闭当前标签并选择右边的标签
   ("/" . w3m-next-form)                                ;下一个表格处
   ("e" . w3m-scroll-down-or-previous-url)              ;向上翻页
   ("b" . w3m-edit-current-url)                         ;编辑当前页面
   ("z" . w3m-zoom-in-image)                            ;放大图片
   ("x" . w3m-zoom-out-image)                           ;缩小图片
   ("O" . w3m-goto-linknum)                             ;数字连接快速跳转
   ("f" . w3m-view-this-url)                            ;在当前标签打开
   ("o" . w3m-view-this-url-new-session)                ;在后台标签打开
   ("M-o" . w3m-open-link-file-under-current-directory) ;open link file under current directory
   ("m" . tabbar-forward-tab)                           ;切换到右边的标签
   ("n" . tabbar-backward-tab)                          ;切换到左边的标签
   ("'" . w3m-open-dead-link-with-external-browser)     ;打开死的连接
   ("s-j" . w3m-visual-scroll-up)                       ;可视化向上滚动
   ("s-k" . w3m-visual-scroll-down)                     ;可视化向下滚动
   ("b" . w3m-history)                                  ;历史
   ("D" . w3m-dtree)                                    ;显示本地目录树
   ("B" . w3m-view-previous-page)                       ;后退
   ("F" . w3m-view-next-page)                           ;前进
   ("S" . w3m-google-desktop-url-open)                  ;Google桌面打开连接
   ("L" . w3m-submit-form)                              ;提交form中的内容
   ("C" . w3m-delete-other-buffers)                     ;关闭后台标签
   ("d" . w3m-download-with-wget-current-position)      ;用Wget异步下载当前地连接
   ("Y" . wget-web-page)                                ;网页下载
   ("-" . org-w3m-copy-for-org-mode)                    ;转换网页成 `org-mode' 的链接格式
   ("_" . w3m-copy-link-in-region)                      ;拷贝w3m buffer 的所有链接
   ("&" . yaoddmuse-w3m-edit-emacswiki-page)            ;编辑 emacswiki 页面
   ("*" . w3m-emacswiki-view-diff)                      ;查看当前wiki页面的不同
   ("\"" . w3m-emacswiki-recent-changes)                ;最近的修改
   ("C-u s" . w3m-db-history)                           ;历史数据库
   ("<up>" . emms-volume-mode-plus)                     ;增加音量
   ("<down>" . emms-volume-mode-minus)                  ;减少音量
   ("<left>" . emms-seek-backward)                      ;后退
   ("<right>" . emms-seek-forward)                      ;前进
   ("<" . w3m-shift-left)                               ;向左滚动屏幕一像素
   (">" . w3m-shift-right)                              ;向右滚动屏幕一像素
   ("." . go-to-char-forward-word)                      ;向后查找某一个字符, 以单词为单位前进
   ("," . go-to-char-backward-word)                     ;向前查找某一个字符, 以单词为单位后退
   ("M-s" . lazy-search-menu)                           ;懒惰搜索
   ("M-A" . emms-pause)                                 ;暂停
   ("M-j" . scim-handle-event)                          ;设置为输入法切换键
   ("C-M-7" . w3m-tab-move-left)                        ;移动当前标签到左边
   ("C-M-8" . w3m-tab-move-right)                       ;移动当前标签到右边
   ("C-S-7" . w3m-delete-left-tabs)                     ;删除左边的标签
   ("C-S-8" . w3m-delete-right-tabs)                    ;删除右边的标签
   )
 w3m-mode-map
 )
(lazy-set-key sdcv-key-alist w3m-mode-map) ;sdcv的局部按键绑定
(lazy-unset-key
 '("s")
 w3m-mode-map)                          ;卸载按键
(lazy-set-key
 '(
   ("s" . one-key-menu-w3m-search)      ;w3m 搜索菜单
   )
 w3m-mode-map)
;;; ### Etags ###
;;; --- 代码搜索
(lazy-set-key
 '(
   ("s-E" . one-key-menu-etags)))
;;; ### IRC ###
;;; --- 聊天
(lazy-set-key
 '(
   ("C-c i" . switch-to-erc)                     ;切换到IRC或自动登录IRC
   ("C-c I" . erc-nick-notify-jump-last-channel) ;自动跳转到最后收到消息的频道
   ("M-U" . one-key-menu-irc-channel)            ;跳转到IRC频道
   ))
;;; ### ERC ###
;;; --- IRC 客户端
(lazy-set-key
 '(
   ("C-c C-y" . paste2-buffer-create)   ;粘贴大段内容
   ("/" . doi-erc-command)              ;erc命令
   )
 erc-mode-map
 )
(lazy-set-key doi-key-alist erc-mode-map) ;doi 的局部按键
;;; ### Rcirc ###
;;; --- IRC 客户端
(lazy-set-key
 '(
   ("M-o" . backward-delete-char-untabify)
   ("M-O" . rcirc-omit-mode)                    ;切换忽略模式
   ("C-c SPC" . rcirc-cmd-read-current-message) ;阅读当前消息
   ("C-c C-v" . rcirc-cmd-talk-to)              ;键入某人的昵称
   )
 rcirc-mode-map
 )
(lazy-set-key doi-key-alist rcirc-mode-map) ;doi 的局部按键
;;; ### Auto-Install ###
;;; --- 自动下载安装elisp文件
(lazy-set-key
 '(
   ("C-z e" . anything-auto-install-from-emacswiki) ;从emacswiki.org安装elisp文件
   ("C-z u" . auto-install-from-url)                ;从链接安装elisp文件
   ("C-z b" . auto-install-from-buffer)             ;从当前buffer安装
   ("C-z x" . anything-auto-install-from-library)   ;更新库
   ))
;;; ### Paste2 ###
;;; --- paste2.org 的粘贴服务
(lazy-set-key
 '(
   ("C-s-s" . paste2-send-paste)                     ;发送粘贴
   ("C-s-z" . paste2-get-paste)                      ;得到粘贴的内容
   ("C-s-a" . paste2-buffer-append)                  ;附加当前的内容到粘贴buffer
   ("C-s-x" . paste2-buffer-create)                  ;创建粘贴buffer
   ("C-s-c" . paste2-send-paste-with-command-output) ;粘贴命令行输出
   ))
;;; ### Alarm Clock ###
;;; --- 闹钟
(lazy-set-key
 '(
   ("s-x s-s" . alarm-clock)            ;设定消息提醒
   ("s-x s-c" . alarm-clock-cancel)     ;取消消息提醒
   ))
;;; ### irfc ###
;;; --- RFC 文档阅读
(lazy-set-key sdcv-key-alist irfc-mode-map)
(lazy-set-key
 '(
   ("c" . kill-this-buffer)                         ;关闭当前buffer
   ("C" . kill-current-mode-buffers-except-current) ;关闭所有后台标签
   ("m" . tabbar-forward-tab)                       ;向右一个标签
   ("n" . tabbar-backward-tab)                      ;向左一个标签
   ("." . go-to-char-forward-word)                  ;向左快速移动
   ("," . go-to-char-backward-word)                 ;向右快速移动
   ("<" . end-of-buffer)                            ;最下面
   (">" . beginning-of-buffer)                      ;最上面
   )
 irfc-mode-map
 )
;;; ### Less ###
;;; --- 快速浏览模式
(lazy-set-key
 '(
   ("M-s-l" . less-minor-mode)          ;打开less模式
   ))
(lazy-set-key
 '(
   ("J" . less-scroll-up-one-line)      ;向下浏览
   ("K" . less-scroll-down-one-line)    ;向上浏览
   ("." . go-to-char-forward)           ;向后查找某一个字符
   ("," . go-to-char-backward)          ;向前查找某一个字符
   (">" . beginning-of-buffer)          ;BUFFER结尾
   ("<" . end-of-buffer)                ;BUFFER开始
   ("q" . less-quit)                    ;退出less模式
   ("b" . one-key-menu-hideshow)        ;hideshow 菜单
   ("t" . one-key-menu-etags)           ;Etags 菜单
   ("dd" . auto-scroll-mode)            ;开始滚屏
   ("df" . auto-scroll-faster)          ;滚动的快一点
   ("ds" . auto-scroll-slower)          ;滚动的慢一点
   )
 less-minor-mode-map
 )
(lazy-set-key sdcv-key-alist less-minor-mode-map)    ;sdcv的局部按键绑定
(lazy-set-key vi-move-key-alist less-minor-mode-map) ;vi-move 的局部按键
;;; ### Hexl ###
;;; --- 十六进制模式
(lazy-set-key
 '(
   ("s-c hh" . hexl-mode)               ;十六进制模式
   ("s-c hf" . hexl-find-file)          ;以十六进制打开文件
   ))
;;; ### Compile DWIM ###
;;; --- 智能编译
(lazy-set-key
 '(
   ("C-9" . compile-dwim-compile+)      ;智能编译程序
   ("C-0" . compile-dwim-run)           ;智能运行程序
   ))
;;; ### Re-builder ###
;;; --- 可视化正则表达式建立
(lazy-set-key
 '(
   ("C-c b" . reb-change-target-buffer) ;改变目标buffer
   ("C-c c" . reb-toggle-case)          ;切换大小写
   ("C-c e" . reb-enter-subexp-mode)    ;进入表达式模式
   ("C-c r" . reb-prev-match)           ;前一个匹配
   ("C-c s" . reb-next-match)           ;后一个匹配
   ("C-c u" . reb-force-update)         ;更新
   ("C-c w" . reb-copy)                 ;拷贝
   ("C-c q" . reb-quit)                 ;退出
   ("C-c TAB" . reb-change-syntax)      ;改变语法
   )
 reb-mode-map
 )
;;; ### Company Mode ###
;;; --- 直观的列表式补全
(lazy-unset-key
 '("TAB")
 company-mode-map)                      ;卸载按键
(lazy-unset-key
 '("M-p" "M-n" "M-1"
   "M-2" "M-3" "M-4"
   "M-5" "M-6" "M-7"
   "M-8" "M-9" "M-0"
   "C-m")
 company-active-map)
(lazy-set-key
 '(
   ("M-h" . company-complete-common)    ;补全公共部分
   ("M-H" . company-complete-selection) ;补全选择的
   ("M-w" . company-show-location)      ;显示局部的
   ("M-s" . company-search-candidates)  ;搜索候选
   ("M-S" . company-filter-candidates)  ;过滤候选
   ("M-," . company-select-next)        ;下一个
   ("M-." . company-select-previous)    ;上一个
   )
 company-active-map
 )
;;; ### Auto-complete ###
;;; --- 自动补全
(lazy-unset-key
 '("RET" "TAB")
 ac-complete-mode-map)
(lazy-set-key
 '(
   ("M-h" . ac-complete)                ;补全当前选中的
   ("M-H" . ac-expand-common)           ;补全公共部分
   ("M-U" . ac-stop)                    ;停止
   ("M-," . ac-next)                    ;下一个
   ("M-." . ac-previous)                ;上一个
   )
 ac-complete-mode-map
 )
;;; ### C ###
;;; --- C 语言模式
(lazy-set-key
 '(
   ("M-j" . toggle-input-method)                  ;切换输入法
   ("M-[" . my-c-function-init-indent)            ;函数括号格式化
   ("M-'" . my-c-mode-auto-newline-break)         ;C模式auto-newline最后一空行跳出, 到花括号右边
   ("M-," . my-c-mode-auto-newline-break-newline) ;C模式auto-newline最后一空行跳出, 并换行
   ("RET" . newline-and-indent)                   ;当在C语言模式时按RET自动新行和对齐
   ("M-:" . my-c-previous-line-comment)           ;C语言上一行注释
   ("M-s-o" . eassist-switch-h-cpp)               ;在头文件间跳转
   ("M-s-m" . eassist-list-methods)               ;选择方法
   )
 c-mode-base-map
 )
;;; ### Xgtags ###
;;; --- Gtags 的界面, 快速代码搜索
(lazy-set-key
 '(
   ("s-G" . one-key-menu-gtags)         ;gtags 按键
   ))
(lazy-set-key vi-move-key-alist xgtags-select-mode-map) ;vi-move 的局部按键
(lazy-set-key sdcv-key-alist xgtags-select-mode-map)    ;sdcv的局部按键
(lazy-set-key
 '(
   ("f" . xgtags-select-tag-other-window)   ;在其他窗口选择TAG视图
   ("r" . xgtags-select-tag-return)         ;返回选择前的窗口配置
   ("J" . xgtags-select-next-tag-line)      ;下一个引用行
   ("K" . xgtags-select-prev-tag-line)      ;上一个引用行
   ("N" . xgtags-select-next-tag-line-show) ;在其他窗口显示下一个引用
   ("P" . xgtags-select-prev-tag-line-show) ;在其他窗口显示上一个引用
   ("H" . xgtags-select-next-file)          ;下一个文件
   ("L" . xgtags-select-prev-file)          ;上一个文件
   )
 xgtags-select-mode-map
 )
;;; ### Emacs-Lisp ###
;;; --- Emacs Lisp 模式
(lazy-unset-key
 '("M-TAB")
 emacs-lisp-mode-map)                   ;卸载按键
(lazy-set-key
 '(
   ("<s-f1>" . elisp-index-search+)     ;在elisp手册中查找函数
   ))
(lazy-set-key
 '(
   ("RET" . comment-indent-new-line)    ;自动换行并注释
   )
 emacs-lisp-mode-map
 )
;;; ### Wget ###
;;; --- 下载程序
(lazy-set-key
 '(
   ("s-c dd" . wget-show)               ;显示下载信息
   ("s-c dh" . wget-hide)               ;隐藏下载信息
   ("s-c dq" . wget-quit-and-exit)      ;停止下载
   ))
;;; ### Hideshow ###
;;; --- 代码折叠
(lazy-set-key
 '(
   ("C-c b" . one-key-menu-hideshow)    ;结构化菜单
   ))
;;; ### Google Define ###
;;; --- Google 定义
(lazy-set-key
 '(
   ("C-z g" . google-define)              ;查找输入单词的Google定义
   ("C-z s" . google-define-pointer)      ;查找当前光标单词的Google定义
   ("C-z h" . kill-google-define-windows) ;关闭Google定义窗口
   ))
;;; ### Org ###
;;; --- 笔记管理和组织
(lazy-set-key
 '(
   ("s-s" . one-key-menu-org-file)      ;Org 文件
   ("C-c r" . org-remember)             ;Org-remeber
   ))
(lazy-set-key
 '(
   ("s-u" . one-key-menu-org-mode)           ;Org-mode 菜单
   ("s-U" . one-key-menu-org-mode-recursive) ;Org-mode 菜单, 但是递归的
   ("M-O" . org-display-all-todo-item)       ;显示所有TODO列表
   )
 org-mode-map
 )
;;; ### Newsticker ###
;;; --- 新闻阅读器
(lazy-set-key
 '(
   ("M-D" . newsticker-show-news)       ;打开新闻阅读器
   ))
(defconst newsticker-treeview-mode-map
  (let ((map (make-sparse-keymap 'newsticker-treeview-mode-map)))
    (lazy-set-key
     '(
       ("i" . newsticker-treeview-toggle-item-immortal)             ;切换保持状态
       ("e" . newsticker-treeview-prev-page)                        ;上一屏
       (" " . newsticker-treeview-next-page)                        ;下一屏
       ("." . newsticker-treeview-scroll-item+)                     ;向上滚动浏览窗口
       ("," . newsticker-treeview-scroll-item)                      ;向下滚动浏览窗口
       ("L" . newsticker-treeview-first-feed)                       ;第一个种子
       ("H" . newsticker-treeview-last-feed)                        ;最后一个种子
       ("P" . newsticker-treeview-prev-feed)                        ;上一个种子
       ("N" . newsticker-treeview-next-feed)                        ;下一个种子
       ("j" . newsticker-treeview-next-item)                        ;下一个项目
       ("k" . newsticker-treeview-prev-item)                        ;上一个项目
       ("K" . newsticker-treeview-prev-new-or-immortal-item)        ;上一个新的条目
       ("J" . newsticker-treeview-next-new-or-immortal-item)        ;下一个新的条目
       ("g" . newsticker-treeview-get-news)                         ;抓取当前种子的新闻
       ("v" . newsticker-treeview-jump)                             ;跳转
       ("O" . newsticker-treeview-mark-list-items-old)              ;标记整个列表的条目为旧的
       ("o" . newsticker-treeview-mark-item-old)                    ;标记条目为旧的
       ("q" . newsticker-treeview-quit)                             ;隐藏
       ("S" . newsticker-treeview-save-item)                        ;保存网页
       ("s" . newsticker-treeview-save)                             ;保存条目
       ("u" . newsticker-treeview-update)                           ;更新
       ("f" . newsticker-treeview-browse-url-with-w3m)              ;浏览连接
       ("a" . newsticker-add-url)                                   ;添加连接
       ("G" . newsticker-get-all-news)                              ;抓取所有新闻
       ("w" . newsticker-switch-to-w3m)                             ;切换到w3m视图
       ("M" . newsticker-treeview-browse-url-with-external-browser) ;用外部浏览器浏览
       ("M-m" . newsticker-group-move-feed)                         ;移动种子到组
       ("M-a" . newsticker-group-add-group)                         ;添加组
       )
     map
     )
    map)
  "Mode map for newsticker treeview.")
;;; ### Festival ###
;;; --- 语音阅读
(lazy-set-key
 '(
   ("s-x r" . one-key-menu-festival)    ;语音阅读菜单
   ))
;;; ### Revie ###
;;; --- 窗口配置管理
(lazy-set-key
 '(
   ("s-v s" . save-current-configuration) ;保存当前的窗口配置方案
   ("s-v f" . resume)                     ;恢复上一次的窗口配置方案
   ("s-v k" . wipe)                       ;清空窗口配置方案
   ))
;;; ### Multi-Term ###
;;; --- 多标签式的shell
(lazy-set-key
 '(
   ("s-e" . multi-term-next)                 ;下一个终端
   ("s-w" . multi-term-prev)                 ;上一个终端
   ("s-n" . multi-term)                      ;新建一个终端
   ("s-x s-x" . multi-term-dedicated-toggle) ;切换专注终端
   ("s-x s-z" . multi-term-dedicated-select) ;选择专注终端
   ))
;;; ### Multi-Scratch ###
;;; --- 多重草稿
(lazy-set-key
 '(
   ("C-1" . multi-scratch-prev)         ;上一个草稿
   ("C-2" . multi-scratch-next)         ;下一个草稿
   ("C-3" . multi-scratch-new)          ;新建草稿
   ))
;;; ### Ido ###
;;; --- 交互式管理文件和缓存
(lazy-set-key
 '(
   ("C-x C-f" . ido-find-file)          ;交互式查找文件
   ("C-x b" . ido-switch-buffer)        ;交互式切换buffer
   ("C-x i" . ido-insert-buffer)        ;插入缓存
   ("C-x I" . ido-insert-file)          ;插入文件
   ))
(add-hook 'ido-setup-hook
          '(lambda ()
             (interactive)
             (ido-my-keys ido-completion-map)))
(defun ido-my-keys (keymap)
  "Add my keybindings for ido."
  (lazy-set-key
   '(
     ("M-s-p" . ido-prev-match)              ;上一个匹配
     ("M-s-n" . ido-next-match)              ;下一个匹配
     ("M-s-h" . ido-next-work-directory)     ;下一个工作目录
     ("M-s-l" . ido-prev-work-directory)     ;上一个工作目录
     ("M-o" . backward-delete-char-untabify) ;向前删除字符
     ("M-O" . ido-delete-backward-updir)     ;删除字符或进入上一级目录
     )
   keymap
   ))
;; ### Icicles ###
;; --- Minibuffer 输入补全和切换
(add-hook 'icicle-mode-hook 'bind-icicles-minibuffer-keys)
(defun bind-icicles-minibuffer-keys ()
  "Replace some default Icicles minibuffer bindings with others."
  (dolist
      (map (list
            minibuffer-local-isearch-map             ;isearch
            minibuffer-local-ns-map                  ;当空格不允许时
            minibuffer-local-shell-command-map       ;补全shell命令时
            minibuffer-local-map                     ;从minibuffer读取
            minibuffer-local-completion-map          ;输入补全
            minibuffer-local-must-match-map          ;输入补全精确匹配
            minibuffer-local-filename-completion-map ;文件名补全
            minibuffer-local-filename-must-match-map ;文件名补全精确匹配
            ))
    (when icicle-mode
      (lazy-set-key
       '(
         ("s-o" . icicle-insert-history-element) ;插入历史元素
         )
       map
       )
      (ido-my-keys map)))
  (when icicle-mode
    (lazy-set-key
     '(
       ("TAB" . isearch-complete-edit)
       ("M-k" . isearch-delete-ring-element))
     minibuffer-local-isearch-map
     )))
(lazy-set-key
 '(
   ("M-s-z" . icicle-switch-to-Completions-buf) ;切换到提示buffer
   ("M-s-x" . icicle-switch-to/from-minibuffer) ;在minibuffer和其他buffer之间切换
   ("M-s-m" . icicle-complete-keys)             ;查看当前模式的按键
   ))
;;; ### Predictive ###
;;; --- 英文助手
(lazy-unset-key
 '("TAB")
 completion-dynamic-map)                ;卸载按键
(lazy-set-key
 '(
   ("M-r" . predictive-mode)            ;英文助手
   ))
(lazy-set-key
 '(
   ("M-h" . completion-accept)          ;接受辅助补全
   ("M-H" . completion-reject)          ;拒绝辅助补全
   )
 completion-map
 )
;;; ### Maxima ###
;;; --- 代数计算系统
(lazy-set-key
 '(
   ("s-A" . my-imaxima)                 ;代数计算系统
   ))
(defun my-maxima-keybind ()             ;代数计算系统按键
  (lazy-set-key
   '(
     ("TAB" . maxima-complete)          ;补全
     ("C-p" . comint-previous-input)    ;上一个输入
     ("C-n" . comint-next-input)        ;下一个输入
     ("M-p" . go-to-next-pair-right)    ;括号跳转
     ("M-n" . go-to-next-pair-left))
   (current-local-map)
   ))
;;; ### Pick-backup ###
;;; --- 快速恢复对比备份文件
(lazy-set-key
 '(
   ("s-p v" . pick-backup-and-view)     ;查看备份版本
   ("s-p d" . pick-backup-and-ediff)    ;比较备份版本的不同
   ("s-p r" . pick-backup-and-revert)   ;恢复指定的备份版本
   ))
;;; ### Flymake ###
;;; --- 及时拼写检查
(lazy-set-key
 '(
   ("M-s-j" . flymake-show-next-error)  ;显示下一个错误
   ("M-s-k" . flymake-show-prev-error)  ;显示上一个错误
   ))
;;; ### Speedbar ###
;;; --- 快速访问文件和tags
(setq speedbar-buffers-key-map nil)     ;卸载一些按键
(setq speedbar-file-key-map nil)
(lazy-set-key
 '(
   ("s-z s-z" . sr-speedbar-toggle)        ;显示/隐藏speedbar
   ("s-z s-x" . sr-speedbar-select-window) ;选中speedbar窗口
   ))
(lazy-set-key
 '(
   ;; 导航操作
   ("f" . speedbar-edit-line)             ;进入当前条目
   ("C-m" . speedbar-edit-line)           ;进入当前条目
   ("j" . speedbar-next)                  ;下一行
   ("k" . speedbar-prev)                  ;上一行
   ("n" . speedbar-forward-list)          ;下一条目
   ("p" . speedbar-backward-list)         ;上一条目
   ("u" . speedbar-forced-contract)       ;跳到上一级
   ("F" . speedbar-files)                 ;切换文件视图
   ("B" . speedbar-buffers)               ;切换缓存视图
   ("q" . sr-speedbar-toggle)             ;退出
   ;; 树操作
   ("x" . speedbar-expand-line)           ;展开当前行
   ("z" . speedbar-contract-line)         ;收缩当前行
   ("v" . speedbar-toggle-line-expansion) ;切换当前行的状态
   ;; 文件操作
   ("g" . speedbar-refresh)             ;刷新
   ("'" . speedbar-up-directory)        ;上一级目录
   ("i" . speedbar-item-info)           ;显示信息
   ("b" . speedbar-item-byte-compile)   ;编译
   ("l" . speedbar-item-load)           ;加载
   ("c" . speedbar-item-copy)           ;拷贝
   ("d" . speedbar-item-delete)         ;删除
   ("o" . speedbar-item-object-delete)  ;删除对象
   ("r" . speedbar-item-rename)         ;重命令
   ("m" . speedbar-create-directory)    ;创建目录
   ("K" . speedbar-buffer-kill-buffer)  ;关闭当前buffer
   )
 speedbar-key-map
 )
;;; ### Top ###
;;; --- 进程管理器
(lazy-set-key
 '(
   ("<s-f8>" . top)                     ;TOP
   ))
(lazy-set-key
 '(
   ("s" . isearch-forward)              ;搜索
   ("g" . top)                          ;刷新
   ("q" . quit-window)                  ;退出
   ("d" . top-mode-kill)                ;删除
   ("D" . top-mode-kill-noconfirm)      ;不需要确认删除
   ("t" . top-mode-strace)
   ("T" . top-mode-strace-noconfirm)
   ("r" . top-mode-renice)
   ("R" . top-mode-renice-noconfirm)
   ("m" . top-mode-mark)                ;标记
   ("u" . top-mode-unmark)              ;删除标记
   ("U" . top-mode-show-specific-user))
 top-mode-map
 )
(lazy-set-key sdcv-key-alist top-mode-map)    ;sdcv 的局部按键
(lazy-set-key vi-move-key-alist top-mode-map) ;vi-mode的局部按键
;;; ### Color-moccur ###
;;; --- 增强的moccur
(lazy-set-key
 '(
   ("s-x v" . moccur-grep)              ;搜索当前目录下的文件
   ("s-x g" . moccur-grep-find)         ;递归搜索当前目录下的文件
   ))
;;; ### Doc-view ###
;;; --- 文档阅读器
(lazy-unset-key
 '(".")
 doc-view-mode-map)                     ;卸载按键
(lazy-set-key
 '(
   ("C-M-j" . doc-view-scroll-up-or-next-page+)       ;翻另一个窗口中图书的下一页
   ("C-M-k" . doc-view-scroll-down-or-previous-page+) ;翻另一个窗口中图书的上一页
   ))
(lazy-unset-key
 '("x" "M-<" "M->")
 doc-view-mode-map)                     ;卸载一些按键
(lazy-set-key
 '(
   ([remap scroll-up] . doc-view-next-line-or-next-page) ;重新定向按键, 支持 auto-scroll
   )
 doc-view-mode-map
 )
(lazy-set-key
 '(
   ("N" . doc-view-next-page)                      ;下一页
   ("P" . doc-view-previous-page)                  ;上一页
   ("." . doc-view-first-page)                     ;第一页
   ("," . doc-view-last-page)                      ;最后一页
   ("g" . doc-view-goto-page)                      ;跳到第几页
   ("e" . doc-view-scroll-down-or-previous-page)   ;向上滚动一屏
   ("SPC" . doc-view-scroll-up-or-next-page)       ;向下滚动一屏
   ("j" . doc-view-next-line-or-next-page)         ;下一行或下一屏
   ("k" . doc-view-previous-line-or-previous-page) ;上一行或上一屏
   ("t" . doc-view-show-tooltip)                   ;当前页提示
   ("q" . bury-buffer)                             ;隐藏buffer
   ("Q" . doc-view-kill-proc-and-buffer)           ;退出并结束进程
   ("C-s" . doc-view-search)                       ;搜索
   ("C-S-n" . doc-view-search-next-match)          ;下一个匹配
   ("C-S-p" . doc-view-search-previous-match)      ;上一个匹配
   ("+" . doc-view-enlarge)                        ;放大页面
   ("-" . doc-view-shrink)                         ;缩小页面
   ("C-c C-c" . doc-view-toggle-display)           ;在文本和图像间切换
   ("C-c C-t" . doc-view-open-text)                ;打开文本
   ("r" . revert-buffer)                           ;刷新
   ("s" . auto-scroll-mode)                        ;自动滚屏
   ("<" . auto-scroll-faster)                      ;加快滚屏速度
   (">" . auto-scroll-slower)                      ;减慢滚屏速度
   )
 doc-view-mode-map
 )
(lazy-set-key sdcv-key-alist doc-view-mode-map) ;sdcv的局部按键绑定
;;; ### Gnus ###
;;; --- 新闻阅读器
(lazy-unset-key
 '("M-K" "s")
 gnus-summary-mode-map)                 ;卸载按键
(lazy-set-key
 '(
   ("M-E" . gnus-switch)                ;切换 Gnus
   ))
(add-hook 'gnus-group-mode-hook
          (lambda ()
            (local-set-key (kbd "q") 'gnus-switch)      ;切换Gnus
            (local-set-key (kbd "Q") 'gnus-group-exit)) ;退出
          )
(lazy-set-key
 '(
   ("s" . one-key-menu-gnus-summary-sort)     ;邮件排序
   ("S" . gnus-summary-isearch-article)       ;搜索邮件
   ("f" . gnus-summary-next-page)             ;显示邮件
   ("v" . gnus-summary-followup)              ;跟随, 但不引用原作者的邮件
   ("E" . gnus-summary-edit-article)          ;编辑邮件
   ("d" . gnus-summary-delete-article)        ;删除邮件
   ("y" . gnus-summary-select-article-buffer) ;显示对应的 article
   )
 gnus-summary-mode-map
 )
(lazy-set-key
 '(
   ("y" . gnus-article-show-summary)    ;在摘要和相应文章之间跳转
   ("x" . gnus-mime-save-part)          ;保存mime部分
   )
 gnus-article-mode-map
 )
(lazy-set-key vi-move-key-alist gnus-summary-mode-map) ;vi-move 的局部按键
(lazy-unset-key '("t" "T") gnus-group-mode-map)        ;卸载一些按键
(lazy-set-key
 '(
   ("f" . gnus-group-read-group-no-prompt) ;读取组, 不提醒
   ("K" . gnus-group-list-groups)          ;列出组
   ("t" . one-key-menu-gnus-topic-edit)    ;编辑菜单
   ("T" . one-key-menu-gnus-topic-sort)    ;排序菜单
   )
 gnus-group-mode-map
 )
(lazy-set-key vi-move-key-alist gnus-group-mode-map) ;vi-move 的局部按键
(lazy-set-key
 '(
   ("f" . gnus-server-read-server)      ;读取服务器
   ("d" . gnus-server-kill-server)      ;删除服务器
   )
 gnus-server-mode-map
 )
(lazy-set-key vi-move-key-alist gnus-server-mode-map) ;vi-move 的局部按键
(lazy-set-key
 '(
   ("J" . scroll-up-one-line)           ;向上滚动一行
   ("K" . scroll-down-one-line)         ;向下滚动一行
   ("f" . gnus-browse-read-group)       ;阅读当前组
   )
 gnus-browse-mode-map
 )
(lazy-set-key vi-move-key-alist gnus-browse-mode-map) ;vi-move 的局部按键
;;; ### Apt-utils ###
;;; --- Apt 管理工具
(lazy-unset-key
 '("s")
 apt-utils-mode-map)                    ;卸载按键
(lazy-set-key
 '(
   ("s-x z" . apt-utils-search)         ;APT搜索
   ))
(lazy-set-key
 '(
   ("#" . apt-utils-rebuild-package-lists)    ;重建包列表
   ("*" . apt-utils-list-package-files)       ;列出包文件
   ("F" . apt-utils-choose-package-link)      ;选择包连接
   ("f" . apt-utils-follow-link)              ;进入连接
   ("<backtab>" . apt-utils-previous-package) ;上一个连接
   ("TAB" . apt-utils-next-package)           ;下一个连接
   ("q" . apt-utils-quit)                     ;退出
   ("d" . apt-utils-describe-package)         ;解释
   ("B" . apt-utils-view-previous-package)    ;上一个视图
   ("J" . scroll-up-one-line)                 ;向上滚动一行
   ("K" . scroll-down-one-line)               ;向下滚动一行
   ("t" . apt-utils-toggle-package-info)      ;切换info
   ("S" . apt-utils-show-package)             ;显示包
   ("v" . one-key-menu-apt-utils-view)        ;查看菜单
   ("s" . one-key-menu-apt-utils-search)      ;搜索菜单
   ("b" . one-key-menu-apt-utils-browse)      ;浏览菜单
   )
 apt-utils-mode-map
 )
(lazy-set-key sdcv-key-alist apt-utils-mode-map)    ;sdcv的局部按键绑定
(lazy-set-key vi-move-key-alist apt-utils-mode-map) ;vi-move 的局部按键
;;; ### Paredit ###
;;; --- 结构化编程
(lazy-unset-key
 '("M-J" "M-r" "M-s" "M-;" "C-M-f" "C-M-b")
 paredit-mode-map)                      ;卸载按键
(lazy-set-key
 '(
   ;; 符号插入
   ("(" . paredit-open-parenthesis)             ;智能 (
   (")" . paredit-close-parenthesis)            ;智能 )
   ("[" . paredit-open-bracket)                 ;智能 [
   ("]" . paredit-close-bracket)                ;智能 ]
   ("{" . paredit-open-curly)                   ;智能 {
   ("}" . paredit-close-curly)                  ;智能 }
   ("C-s-," . paredit-open-angled)              ;智能 <
   ("C-s-." . paredit-close-angled)             ;智能 >
   ("\"" . paredit-doublequote)                 ;智能 "
   ("\\" . paredit-backslash)                   ;智能 \
   ;; 删除
   ("M-o" . paredit-backward-delete)            ;向后删除
   ("C-d" . paredit-forward-delete)             ;向前删除
   ("C-k" . paredit-kill)                       ;删除
   ("C-M-m" . paredit-forward-kill-word)        ;向前按词删除
   ("C-M-n" . paredit-backward-kill-word)       ;向后按词删除
   ;; 移动
   ("C-M-S-m" . paredit-forward)                ;向前移动
   ("C-M-S-n" . paredit-backward)               ;向后移动
   ;; 包围
   ("M-\"" . paredit-meta-doublequote)          ;用 " " 包围对象, 或跳出字符串
   ("M-(" . paredit-wrap-sexp)                  ;用 ( ) 包围当前对象
   ("M-[" . paredit-wrap-square)                ;用 [ ] 包围对象
   ("M-{" . paredit-wrap-curly)                 ;用 { } 包围对象
   ("C-(" . paredit-wrap-angled)                ;用 < > 包围对象
   ("M-)" . paredit-splice-sexp+)               ;去除包围对象的括号, 并删除空行
   ;; 跳出并换行缩进
   ("M-:" . paredit-close-round-and-newline+)   ;跳出 ( ) 或 " " 并换行
   ("M-?" . paredit-forward-sexp-and-newline)   ;移动到下一个表达式, 并换行
   ("M-}" . paredit-close-curly-and-newline)    ;跳出 { } 并换行
   ("M-]" . paredit-close-square-and-newline)   ;跳出 [ ] 并换行
   ("C-)" . paredit-close-angled-and-newline)   ;跳出 < > 并换行
   ;; 注释
   ("C-M-:" . paredit-comment-list-and-newline) ;注释当前LIST并换行
   ;; 其他
   ("C-j" . paredit-newline)            ;智能换行并缩进
   ("M-q" . paredit-reindent-defun)     ;重新格式化函数
   ("M-s-r" . paredit-raise-sexp)       ;提取表达式, 并删除同一等级的其他表达式
   ("M-s-b" . paredit-convolute-sexp)   ;嵌套表达式
   ("M-s-'" . one-key-menu-paredit)     ;Paredit 菜单
   )
 paredit-mode-map
 )
;;; ### Haskell ###
;;; --- Haskell 语言模式
(lazy-set-key
 '(
   ("M-;" . comment-dwim-with-haskell-style) ;注释
   )
 haskell-mode-map
 )
;;; ### Contentswitch ###
;;; --- 按内容快速搜索
(lazy-set-key
 '(
   ("C-S-s-y" . contentswitch)          ;按内容进行搜索
   ))
(lazy-set-key
 '(
   ("C-n" . contentswitch-next-line)     ;下一行
   ("C-p" . contentswitch-previous-line) ;上一行
   ("C-v" . contentswitch-next-page)     ;下一屏
   ("M-v" . contentswitch-previous-page) ;上一屏
   ("C-m" . exit-minibuffer)             ;选择搜索结果
   )
 contentswitch-map
 )
;;; ### Anything ###
;;; --- 快速buffer切换
(lazy-set-key
 '(("s-y" . anything)                   ;anything
   ("C-s-y" . anything-call-source)     ;调用特定的源
   ))
(lazy-set-key
 '(
   ("C-n" . anything-next-line)                  ;下一行
   ("C-p" . anything-previous-line)              ;上一行
   ("C-s" . anything-isearch)                    ;搜索
   ("C-m" . anything-exit-minibuffer)            ;执行动作, 并退出
   ("C-j" . anything-execute-persistent-action)  ;执行动作, 但不退出
   ("C-v" . anything-next-page)                  ;下一页
   ("M-v" . anything-previous-page)              ;上一页
   ("M-s-y" . anything-insert-selection)         ;插入当前项目
   ("M-s-i" . anything-insert-symbol)            ;插入当前符号
   ("M-s-o" . anything-insert-buffer-name)       ;插入缓存名字
   ("M-s-j" . anything-next-source)              ;下一个种类
   ("M-s-k" . anything-previous-source)          ;上一个种类
   ("M-s-h" . anything-select-action)            ;选择动作或切换回源
   ("M-s-l" . anything-select-source)            ;选择源
   ("M-s-n" . anything-next-history-element)     ;下一个历史记录
   ("M-s-p" . anything-previous-history-element) ;上一个历史记录
   )
 anything-map
 )
;;; ### Timid ###
;;; --- 快速补全
(setq timid-keys
      (quote (("C-m" . timid-select-file)   ;选择
              ("ESC" . timid-cleanup)       ;清理
              ("C-p" . timid-previous-line) ;上一行
              ("C-n" . timid-next-line)     ;下一行
              ("M-v" . timid-previous-page) ;上一页
              ("C-v" . timid-next-page))))  ;下一页
;;; ### Apropos ###
;;; --- 程序员命令查询
(lazy-set-key
 '(
   ("C-m" . apropos-follow)                ;进入
   ("N" . forward-button-with-line-begin)  ;下一个条目
   ("P" . backward-button-with-line-begin) ;上一个条目
   ("J" . scroll-up-one-line)              ;向上滚动一行
   ("K" . scroll-down-one-line)            ;向下滚动一行
   ("q" . quit-window)                     ;退出
   ("f" . push-button)                     ;确定
   )
 apropos-mode-map
 )
(lazy-set-key sdcv-key-alist apropos-mode-map)    ;sdcv的局部按键绑定
(lazy-set-key vi-move-key-alist apropos-mode-map) ;vi-move 的局部按键
;;; ### Sys-apropos ###
;;; --- 系统相关查询
(lazy-set-key
 '(
   ("f" . sys-apropos-run-woman)        ;查找
   )
 sys-apropos-mode-map
 )
(lazy-set-key sdcv-key-alist sys-apropos-mode-map)    ;sdcv 的局部按键
(lazy-set-key vi-move-key-alist sys-apropos-mode-map) ;vi-mode 的局部按键
;;; ### Help ###
;;; --- 帮助模式
(lazy-unset-key
 '("e" "h" "y")
 view-mode-map)                         ;卸载按键
(lazy-set-key
 '(
   ("C-h". one-key-menu-help)           ;帮助菜单
   ))
(lazy-set-key
 '(
   ("J" . scroll-up-one-line)           ;向下滚动一行
   ("K" . scroll-down-one-line)         ;向上滚动一行
   ("H" . describe-mode)                ;帮助
   ("f" . help-go-forward)              ;前一个帮助
   ("b" . help-go-back)                 ;后一个帮助
   ("y" . sdcv-search-pointer+)         ;翻译
   ("<tab>" . forward-button)           ;前一个按钮
   )
 help-mode-map
 )
(lazy-set-key vi-move-key-alist help-mode-map) ;vi-move 的局部按键
;;; ### Info ###
;;; --- Info 模式
(lazy-set-key
 '(
   ("f" . Info-follow-nearest-node)     ;进入当前节点
   ("<tab>" . Info-next-reference)      ;下一个引用
   ("<backtab>" . Info-prev-reference)  ;上一个引用
   ("E" . Info-edit)                    ;编辑
   ("?" . Info-summary)                 ;帮助
   ("N" . Info-next)                    ;下一个同级节点
   ("P" . Info-prev)                    ;上一个同级节点
   ("J" . scroll-up-one-line)           ;向下滚动一行
   ("K" . scroll-down-one-line)         ;向上滚动一行
   ("." . go-to-char-forward)           ;向后查找某一个字符
   ("," . go-to-char-backward)          ;向前查找某一个字符
   ("<" . Info-forward-node)            ;下一个节点
   (">" . Info-backward-node)           ;上一个节点
   ("C-<" . Info-final-node)            ;最后一个节点
   ("C->" . Info-top-node)              ;最前一个节点
   ("s" . Info-search)                  ;搜索
   ("S" . Info-search-case-sensitively) ;区分大小写搜索
   ("g" . Info-goto-node)               ;跳到指定的节点
   ("q" . Info-exit)                    ;退出
   ("m" . Info-menu)                    ;菜单补全
   ("d" . Info-directory)               ;总目录
   ("I" . Info-index)                   ;索引
   ("o" . Info-follow-reference)        ;随后的引用补全
   ("H" . Info-history)                 ;历史
   ("F" . Info-history-forward)         ;历史向前
   ("B" . Info-history-back)            ;历史向后
   ("M-s" . Info-search)                ;节点搜索
   ("C" . clone-buffer)                 ;克隆当前buffer
   ("c" . Info-copy-current-node-name)  ;拷贝当前节点名字
   ("u" . Info-up)                      ;跳到上一级
   ("T" . Info-toc)                     ;内容索引
   ("e" . Info-scroll-down)             ;向上滚动, vi-move 的后面重新加载
   (" " . Info-fscroll-up)              ;向下滚动
   )
 Info-mode-map
 )
(lazy-set-key sdcv-key-alist Info-mode-map)    ;sdcv的局部按键绑定
(lazy-set-key vi-move-key-alist Info-mode-map) ;vi-move 的局部按键
;;; ### Undo Browse ###
;;; --- 强大的撤销系统
(lazy-set-key
 '(
   ("C-s-?" . ub-mode-on)               ;打开撤销系统
   ))
(lazy-set-key
 '(
   ("m" . ub-movie)                     ;电影观看
   ("s" . ub-movie-stop)                ;电影停止
   ("n" . ub-movie-forward)             ;下一个电影
   ("p" . ub-movie-backward)            ;上一个电影
   ("S" . ub-movie-history)             ;电影历史
   ("f" . ub-frame-forward)             ;前一帧
   ("b" . ub-frame-backward)            ;后一帧
   ("A" . ub-frame-beginning)           ;第一帧
   ("E" . ub-frame-end)                 ;最后一帧
   ("g" . ub-frame-goto)                ;跳到某一帧
   ("C-m" . ub-frame-retain-redo)       ;保留重做
   ("q" . ub-mode-quit)                 ;退出
   ("?" . ub-help)                      ;帮助
   ("J" . scroll-up-one-line)           ;向上滚动一行
   ("K" . scroll-down-one-line)         ;向下
   )
 ub-mode-map
 )
(lazy-set-key vi-move-key-alist ub-mode-map-default ) ;vi-move 的局部按键
;;; ### Calc ###
;;; --- 计算器
(lazy-set-key
 '(
   ("C-x c" . calc)                     ;计算器
   ))                                   ;;; ### Calendar ###
;;; --- 日历
(lazy-set-key
 '(("C-c c" . calendar)))
(lazy-unset-key
 '("a")
 calendar-mode-map)                     ;卸载按键
(lazy-set-key
 '(
   ("j" . calendar-forward-week)              ;下一个星期
   ("k" . calendar-backward-week)             ;上一个星期
   ("l" . calendar-forward-day)               ;下一天
   ("h" . calendar-backward-day)              ;上一天
   ("L" . calendar-forward-month)             ;下一月
   ("H" . calendar-backward-month)            ;上一月
   ("J" . calendar-forward-year)              ;下一年
   ("K" . calendar-backward-year)             ;上一年
   ("aw" . calendar-beginning-of-week)        ;一星期的第一天
   ("ew" . calendar-end-of-week)              ;一星期的最后一天
   ("am" . calendar-beginning-of-month)       ;一月的第一天
   ("em" . calendar-end-of-month)             ;一月的最后一天
   ("ay" . calendar-beginning-of-year)        ;一年的第一天
   ("ey" . calendar-end-of-year)              ;一年的最后一天
   (";" . calendar-goto-today)                ;跳到今天
   ("," . calendar-scroll-left)               ;向左滚动一月
   ("." . calendar-scroll-right)              ;向右滚动一月
   ("<" . calendar-scroll-left-three-months)  ;向左滚动三月
   (">" . calendar-scroll-right-three-months) ;向右滚动三月
   ("q" . calendar-exit)                      ;退出
   )
 calendar-mode-map)
;;; ### Keyboard Macro ###
;;; --- 键盘宏
(lazy-set-key
 '(
   ("M-s-s" . kmacro-start-macro-or-insert-counter) ;开始键盘宏或插入
   ("M-s-d" . kmacro-end-or-call-macro)             ;结束键盘宏或调用
   ("M-s-c" . kmacro-delete-ring-head)              ;删除当前的键盘宏
   ("M-s-w" . kmacro-cycle-ring-next)               ;下一个键盘宏
   ("M-s-e" . kmacro-cycle-ring-previous)           ;上一个键盘宏
   ("M-s-a" . kmacro-edit-macro)                    ;编辑键盘宏
   ("M-s-v" . name-last-kbd-macro)                  ;命令当前键盘宏
   ("M-s-f" . insert-kbd-macro)                     ;插入键盘宏
   ("M-s-q" . apply-macro-to-region-lines)          ;应用键盘宏到选择的区域
   ))
;;; ### Man ###
;;; --- Man
(lazy-set-key
 '(("C-<f1>" . woman)))
(lazy-set-key
 '(
   ("J" . scroll-up-one-line)           ;向上滚动一行
   ("K" . scroll-down-one-line)         ;向下滚动一行
   ("," . end-of-buffer)                ;buffer末尾
   ("." . beginning-of-buffer)          ;buffer开始
   ("M-n" . Man-next-section)           ;下一节
   ("M-p" . Man-previous-section)       ;上一节
   ("g" . Man-goto-section)             ;跳转到某一节
   ("G" . Man-goto-see-also-section)    ;跳转到 see-also
   ("f" . Man-follow-manual-reference)  ;当前处的man手册引用
   ("F" . man-follow)                   ;某man手册引用
   ("N" . Man-next-manpage)             ;下一个页面
   ("P" . Man-previous-manpage)         ;上一个页面
   ("q" . Man-quit)                     ;隐藏
   ("Q" . Man-kill)                     ;退出
   )
 Man-mode-map
 )
(lazy-set-key sdcv-key-alist Man-mode-map)    ;sdcv的局部按键绑定
(lazy-set-key vi-move-key-alist Man-mode-map) ;vi-move 的局部按键
;;; ### Ielm ###
;;; --- Emacs Lisp 解释模式
(lazy-unset-key
 '("M-p" "M-n")
 ielm-map)                              ;卸载按键
(lazy-set-key
 '(
   ("M-s-i" . ielm-toggle)              ;切换ielm
   ))
(lazy-set-key
 '(
   ("C-s-p" . comint-previous-input)    ;上一个输入
   ("C-s-n" . comint-next-input)        ;下一个输入
   )
 ielm-map
 )
;;; ### Go Change ###
;;; --- 修改轨迹
(lazy-set-key
 '(
   ("s-/" . goto-last-change)           ;跳转到最近修改, 向前
   ("s-?" . goto-last-change-reverse)   ;跳转到最近修改, 向后
   ))
;;; ### Archive ###
;;; --- 压缩模式
(lazy-set-key
 '(
   ("j" . archive-next-line)            ;下一行
   ("k" . archive-previous-line)        ;上一行
   ("C-m" . archive-extract)            ;解压
   ("E" . archive-extract-other-window) ;解压道其他窗口
   ("m" . archive-mark)                 ;标记
   ("d" . archive-flag-deleted)         ;删除标记
   ("x" . archive-expunge)              ;擦除有删除标记的文件
   ("u" . archive-unflag)               ;解除标记, 向下移动
   ("i" . archive-unflag-backwards)     ;解除标记, 并向上移动
   ("U" . archive-unmark-all-files)     ;解除所有标记
   ("g" . revert-buffer)                ;刷新
   ("q" . quit-window)                  ;退出
   ("f" . archive-view)                 ;浏览
   ("r" . archive-rename-entry)         ;重命令
   ("e" . scroll-down)                  ;向下滚动一屏
   (" " . scroll-up)                    ;向上滚动一屏
   ("M" . archive-chmod-entry)          ;chmod操作
   ("G" . archive-chgrp-entry)          ;chgrp操作
   ("O" . archive-chown-entry)          ;chown操作
   )
 archive-mode-map
 )
;;; ### Completion List ###
;;; --- 补全列表
(lazy-set-key vi-move-key-alist completion-list-mode-map) ;vi-move的局部按键
(lazy-set-key sdcv-key-alist completion-list-mode-map)    ;sdcv 的局部按键
;;; ### Isearch ###
;;; --- 交互式搜索
(lazy-set-key
 '(
   ("TAB" . isearch-complete)               ;isearch补全
   ("C-s" . isearch-repeat-forward)         ;重复向前搜索, 第一次可以用来搜索上一次的历史哟
   ("C-r" . isearch-repeat-backward)        ;重复向后搜索
   ("C-g" . isearch-abort)                  ;中止搜索
   ("C-w" . isearch-yank-word-or-char)      ;粘帖光标后的词或字符作为搜索对象
   ("C-y" . isearch-yank-line)              ;粘帖光标后的行作为搜索对象
   ("M-o" . isearch-delete-char)            ;删除
   ("M-l" . isearch-to-lazy-search)         ;切换到lazy-search
   ("M-p" . isearch-ring-retreat)           ;搜索历史向后
   ("M-n" . isearch-ring-adjust)            ;搜索历史向前
   ("M-y" . isearch-yank-kill)              ;从 kill ring 中粘帖最后一项到搜索对象后
   ("M-h" . isearch-yank-char)              ;粘帖光标后的字符到搜索对象
   ("M-e" . isearch-edit-string)            ;编辑搜索对象
   ("M-c" . isearch-toggle-case-fold)       ;切换大小写
   ("M-r" . isearch-toggle-regexp)          ;切换正则表达式
   ("M-w" . isearch-toggle-word)            ;切换词
   ("M-g" . isearch-moccur)                 ;moccur 当前 buffer
   ("M-G" . isearch-moccur-all)             ;moccur 所有 buffer
   ("M->" . isearch-beginning-of-buffer)    ;跳转到buffer开头并重新搜索, 搜索最前面一个
   ("M-<" . isearch-end-of-buffer)          ;跳转到buffer末尾并重新搜索, 搜索最后面一个
   ("M-%" . isearch-query-replace)          ;替换
   ("M-d" . isearch-find-duplicate-word)    ;查找重复的单词
   ("M-z" . isearch-find-duplicate-line)    ;查找重复的行
   ("C-M-%" . isearch-query-replace-regexp) ;正则表达式替换
   )
 isearch-mode-map
 )
;;; ### kill-ring-search ###
;;; --- 删除环的递增式搜索
(lazy-set-key
 '(
   ("M-s-y" . kill-ring-search)         ;kill ring 搜索
   ))
(lazy-set-key
 '(
   ("C-s" . kill-ring-search-prev))     ;下一个匹配
 kill-ring-search-keymap
 )
;;; ### Babel ###
;;; --- 网络翻译接口
(lazy-set-key
 '(
   ("s-t" . babel-smart)                ;智能翻译
   ))
(lazy-set-key
 '(
   ("q" . babel-quit)                   ;退出
   ("," . end-of-buffer)                ;最后面
   ("." . beginning-of-buffer)          ;最前面
   ("s" . isearch-forward)              ;向前搜索
   ("r" . isearch-backward)             ;向后搜索
   )
 babel-mode-map
 )
(lazy-set-key vi-move-key-alist babel-mode-map)
;;; ### Breadcrumb ###
;;; --- 书签管理导航
(lazy-set-key
 '(
   ("s-7" . bc-local-next)              ;局部下一个
   ("s-8" . bc-local-previous)          ;局部上一个
   ("s-9" . bc-next)                    ;全局下一个
   ("s-0" . bc-previous)                ;全局上一个
   ("s-o" . bc-goto-current)            ;跳到当前
   ("s-l" . bc-list)                    ;书签列表
   ("s-'" . bc-set)                     ;书签设定
   ))
(lazy-set-key
 '(
   ("j" . next-line)                    ;下一行
   ("k" . previous-line)                ;上一行
   ("d" . bc-menu-mark-delete)          ;标记删除当前
   ("D" . bc-menu-mark-all-delete)      ;标记删除所有
   ("x" . bc-menu-commit-deletions)     ;确认删除
   ("u" . bc-menu-unmark-delete)        ;去标记当前
   ("U" . bc-menu-unmark-all-delete)    ;去标记所有
   ("v" . bc-menu-visit-other)          ;在其他窗口中浏览
   ("f" . bc-menu-jump)                 ;跳到书签处
   )
 *bc-menu-mode-map*
 )
;;; ### Text Translator ###
;;; --- 文本翻译
(lazy-set-key
 '(
   ("s-x ti" . text-translator)                             ;全文翻译, 输入
   ("s-x tt" . text-translator-translate-by-auto-selection) ;全文翻译自动选择
   ))
;;; ### Functin key ###
;;; --- 功能函数
(lazy-set-key
 '(
   ("<f1>" . sh-show-help)                       ;elisp help
   ("<f2>" . refresh-file)                       ;自动刷新文件
   ("<f3>" . visit-tags-table)                   ;查找TAGS文件 (更新TAGS表)
   ("<f4>" . generate-gtags-files)               ;生成gtags引用文件
   ("<f5>" . emacs-exit)                         ;退出emacs
   ("<f6>" . lock-screen)                        ;锁屏
   ("<f7>" . one-key-menu-ui)                    ;用户界面菜单
   ("<f8>" . dired-jump)                         ;文件管理起
   ("<f9>" . list-load-path-shadows)             ;显示重复加载的库
   ("<f10>" . open-current-log-keyboard-command) ;打开命令日志
   ("<f12>" . hibernate-disk)                    ;休眠
   ("M-1" . strip-blank-lines)                   ;删除选中区域的所有空行
   ("M-2" . indent-buffer)                       ;自动格式化当前Buffer
   ("M-3" . delete-trailing-whitespace)          ;删除行末空格
   ("M-4" . whitespace-cleanup)                  ;清理空格
   ("M-5" . insert-line-number+)                 ;自动在行首添加行号
   ("M-6" . strip-line-number)                   ;删除选中区域的行号
   ("C-4" . insert-changelog-date)               ;插入日志时间 (%Y/%m/%d)
   ("C-5" . insert-standard-date)                ;插入标准时间 (%Y-%m-%d %T)
   ("C-&" . switch-to-messages)                  ;跳转到 *Messages* buffer
   ("C-7" . jump-back)                           ;返回查找符号定义前的位置
   ("C-8" . find-function-or-variable-at-point)  ;查找符号的定义
   ))
;;; ### Buffer Move ###
;;; --- 缓存移动
(lazy-set-key
 '(
   ("s-N" . move-text-down)                ;把光标所在的整行文字(或标记)下移一行
   ("s-P" . move-text-up)                  ;把光标所在的整行文字(或标记)上移一行
   ("s-J" . scroll-up-one-line)            ;向上滚动一行
   ("s-K" . scroll-down-one-line)          ;向下滚动一行
   ("M-J" . scroll-other-window)           ;向下滚动其他窗口
   ("M-K" . scroll-other-window-down)      ;向上滚动其他窗口
   ("M-<" . scroll-other-window-up-line)   ;向下滚动其他窗口一行
   ("M->" . scroll-other-window-down-line) ;向上滚动其他窗口一行
   ("C-o" . open-newline-above)            ;在上面一行新建一行
   ("C-l" . open-newline-below)            ;在下面一行新建一行
   ("C-z k" . beginning-of-buffer)         ;缓存开始
   ("C-z j" . end-of-buffer)               ;缓存结尾
   ("M-p" . go-to-next-pair-right)         ;在( ),' ', " ", [ ], { }中跳到匹配符号的右边
   ("M-n" . go-to-next-pair-left)          ;在( ), ' ', " ", [ ], { }中跳到匹配符号的左边
   ("%" . match-paren)                     ;当在括号上按 % 时, 自动跳转到与当前括号匹配的另一个括号
   ("s-g" . goto-percent)                  ;跳转到当前Buffer的文本百分比, 单位为字符
   ("M-g" . goto-line)                     ;到指定行
   ("M-G" . goto-column)                   ;到指定列
   ("C-M-f" . forward-paragraph)           ;下一个段落
   ("C-M-b" . backward-paragraph)          ;上一个段落
   ("C-M-y" . backward-up-list)            ;向左跳出 LIST
   ("C-M-o" . up-list)                     ;向右跳出 LIST
   ("C-M-u" . backward-down-list)          ;向左跳进 LIST
   ("C-M-i" . down-list)                   ;向右跳进 LIST
   ("C-M-a" . beginning-of-defun)          ;函数开头
   ("C-M-e" . end-of-defun)                ;函数末尾
   ("C-c j" . go-to-char-forward)          ;快速字母导航, 向前
   ("C-c k" . go-to-char-backward)         ;快速字母导航, 向后
   ("C->" . remember-init)                 ;记忆初始函数
   ("C-<" . remember-jump)                 ;记忆跳转函数
   ("M-s" . lazy-search-menu)              ;懒惰搜索
   ("M-s-," . point-stack-pop)             ;buffer索引跳转
   ("M-s-." . point-stack-push)            ;buffer索引标记
   ("s-{" . current-line-move-to-top)      ;移动当前行到最上面一行
   ))
;;; ### Buffer Edit ###
;;; --- 缓存编辑
(lazy-set-key
 '(
   ("M-N" . kill-syntax-backward+)            ;向后进行语法删除
   ("M-M" . kill-syntax-forward+)             ;向前进行语法删除
   ("C-:" . comment-or-uncomment-region+)     ;注释当前行
   ("C-s-n" . comment-dwim-next-line)         ;移动到上一行并注释
   ("C-s-p" . comment-dwim-prev-line)         ;移动到下一行并注释
   ("M-s-n" . comment-part-move-down)         ;向下移动注释
   ("M-s-p" . comment-part-move-up)           ;向上移动注释
   ("C-x C-x" . exchange-point-and-mark)      ;交换当前点和标记点
   ("M-o" . backward-delete-char-untabify)    ;向前删除字符
   ("M-z" . zap-to-char)                      ;向前删除到第一个相符的字符
   ("C-M-z" . zap-back-to-char)               ;向后删除到第一个相符的字符
   ("C-/" . undo)                             ;撤销
   ("C-?" . redo)                             ;重做
   ("s-k" . kill-and-join-forward)            ;在缩进的行之间删除
   ("C-x u" . mark-line)                      ;选中整行
   ("C-M-S-h" . mark-paragraph)               ;选中段落
   ("C-S-o" . duplicate-line-or-region-above) ;向上复制当前行或区域
   ("C-S-l" . duplicate-line-or-region-below) ;向下复制当前行或区域
   ("C-S-s-o" . duplicate-line-above-comment) ;复制当前行到上一行, 并注释当前行
   ("C-S-s-l" . duplicate-line-below-comment) ;复制当前行到下一行, 并注释当前行
   ("M-SPC" . just-one-space)                 ;只有一个空格在光标处
   ))
;;; ### Buffer Name ###
;;; --- 缓存名字
(lazy-set-key
 '(
   ("s-c r" . rename-file-and-buffer)        ;更改当前文件的名字
   ("s-c g" . move-buffer-file)              ;更改当前文件的目录
   ("s-c n" . copy-buffer-file-name-as-kill) ;拷贝buffer名字
   ))
;;; ### Completion Operation ###
;;; --- 补全操作
(lazy-set-key
 '(
   ("C-c l" . semantic-ia-complete-symbol-menu) ;弹出补全菜单
   ("C-c SPC" . senator-completion-menu-popup)  ;弹出补全菜单
   ("M-/" . hippie-expand)                      ;智能补全
   ))
;;; ### VC ###
;;; --- 版本控制
(lazy-set-key
 '(
   ("C-x v" . one-key-menu-VC)          ;版本控制
   ))
;;; ### Window Operation ###
;;; --- 窗口操作
(lazy-set-key
 '(
   ("C-c v" . split-window-vertically)                ;纵向分割窗口
   ("C-c h" . split-window-horizontally)              ;横向分割窗口
   ("C-'" . delete-current-buffer-and-window)         ;关闭当前buffer, 并关闭窗口
   ("C-\"" . delete-current-buffer-window)            ;删除当前buffer的窗口
   ("C-;" . kill-this-buffer)                         ;关闭当前buffer
   ("C-M-;" . kill-other-window-buffer)               ;关闭其他窗口的buffer
   ("C-x ;" . delete-other-windows)                   ;关闭其它窗口
   ("C-c V" . delete-other-windows-vertically+)       ;关闭上下的其他窗口
   ("C-c H" . delete-other-windows-horizontally+)     ;关闭左右的其他窗口
   ("s-Q" . kill-current-mode-buffers)                ;关闭与当前模式相同的所有buffers
   ("s-q" . kill-current-mode-buffers-except-current) ;关闭当前模式所有buffers, 除了当前buffer
   ("s-;" . one-key-menu-window-navigation)           ;快速窗口导航
   ("s-j" . window-number-jump)                       ;窗口快速选择
   ("C-s-7" . select-next-window)                     ;选择下一个窗口
   ("C-s-8" . select-prev-window)                     ;选择上一个窗口
   ("M-s-o" . toggle-one-window)                      ;切换一个窗口
   ))
;;; ### Ispell ###
;;; --- 拼写检查
(lazy-set-key
 '(("s-v s-v" . ispell-buffer)))        ;检查当前buffer
;;; ### Toolkit ###
;;; --- 工具函数
(lazy-set-key
 '(
   ("C-x C-c" . checkdoc)                ;检查文档
   ("C-c ns" . notes-search)             ;便签搜索
   ("C-c nn" . notes-new)                ;新建便签
   ("s-c o" . one-key-menu-directory)    ;目录打开菜单
   ("M-C" . one-key-menu-cycle-buffer)   ;特定模式切换
   ("s-*" . one-key-menu-backup-file)    ;备份资料
   ("s-," . bury-buffer)                 ;隐藏当前buffer
   ("s-." . unbury-buffer)               ;反隐藏当前buffer
   ("s-&" . killall)                     ;杀掉进程
   ("C-x w" . count-words)               ;计算单词的数量
   ("C-z l" . linum-mode)                ;行号模式切换
   ("s-Z" . dot-emacs)                   ;打开dot-emacs文件
   ("C-x f" . find-file-at-point)        ;文件跳转
   ("s-f" . find-file-root)              ;用root打开文件
   ("s-r" . find-file-smb)               ;访问samba
   ("<print>" . save-screenshots)        ;截屏
   ("M-s-/" . toggle-debug-on-error)     ;切换调试模式
   ("s-R" . re-builder)                  ;可视化构建正则表达式
   ("s-1" . elisp-depend-insert-require) ;插入 (require '...) 语句
   ("s-2" . elisp-depend-insert-comment) ;插入 `...' 注释代码
   ("s-3" . hanconvert-region)           ;转换简体或繁体中文
   ("s-[" . eval-expression)             ;执行表达式
   ("s-\\" . artist-mode)                ;绘制模式
   ("M-s-u" . one-key-menu-boxquote)     ;引用框
   ("C-s-q" . quoted-insert)             ;读取系一个输入字符并插入
   ))

(provide 'LazyCatKeystoke)
