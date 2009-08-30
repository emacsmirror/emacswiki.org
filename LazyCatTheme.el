;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Emacs 主题设置 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 用户自定义变量
(custom-set-variables
 ;; Term
 '(term-default-bg-color "#000000")     ;term默认背景色
 '(term-default-fg-color "#dddd00")     ;term默认前景色
 ;; Imaxima
 '(imaxima-bg-color "black")            ;背景
 '(imaxima-fg-color "DarkGreen")        ;前景
 '(imaxima-equation-color "Green3")     ;表达式
 '(imaxima-label-color "slategrey")     ;标签
 '(imaxima-fnt-size "Large")            ;字体大小
 '(imaxima-max-scale 0.85)              ;表达式最大比例
 '(imaxima-pt-size 12)                  ;点大小
 ;; 高亮括号结构
 '(hl-paren-colors (quote (
                           "Cyan"       ;一级颜色
                           "Gold"       ;二级颜色
                           "Red"        ;三级颜色
                           )))
 ;; Smiley
 '(smiley-style (quote medium))         ;笑脸的风格, 中等, 10种颜色
 )

;; 用户自定义外观
(custom-set-faces
 ;; Default
 '(default
    ((t
      (:background "black"               ;前景
                   :foreground "#137D11" ;背景
                   ))))
 ;; Header line
 '(header-line                          ;标题栏
   ((t (:background "Black"
                    :foreground "Green"))))
 ;; Cursor
 '(cursor                               ;光标
   ((t (:background "red"))))
 ;; Newsticker
 '(newsticker-date-face                 ;时间
   ((t (:foreground "red"
                    :slant italic
                    :height 0.8
                    ))))
 '(newsticker-default-face              ;默认
   ((((class color) (background dark))
     (:inherit default
               ))))
 '(newsticker-enclosure-face            ;附件
   ((t (:background "orange"
                    :weight bold
                    ))))
 '(newsticker-extra-face                ;额外
   ((t (:foreground "gray50"
                    :slant italic
                    :height 0.9
                    ))))
 '(newsticker-feed-face                 ;种子
   ((t (:foreground "Green"
                    :weight bold
                    :height 1.2
                    ))))
 '(newsticker-immortal-item-face        ;永久的条目
   ((t (:foreground "green"
                    :slant italic
                    :weight bold
                    ))))
 '(newsticker-new-item-face             ;新条目
   ((t (:foreground "Gold"
                    :weight bold
                    ))))
 '(newsticker-obsolete-item-face        ;陈旧的条目
   ((t (:strike-through t
                        :weight bold
                        ))))
 '(newsticker-old-item-face             ;老条目
   ((t (:foreground "purple"
                    :weight bold
                    ))))
 '(newsticker-statistics-face           ;统计
   ((t (:foreground "red"
                    :slant italic
                    :height 0.8
                    ))))
 '(newsticker-treeview-face             ;浏览树
   ((t (:foreground "Green4"
                    :weight normal
                    ))))
 '(newsticker-treeview-new-face         ;新条目
   ((t (:inherit newsticker-treeview-face
                 :foreground "DodgerBlue"
                 :weight bold
                 ))))
 '(newsticker-treeview-old-face         ;就条目
   ((((class color) (background dark))
     (:inherit newsticker-treeview-face
               :foreground "purple"))))
 '(newsticker-treeview-selection-face   ;选中
   ((((class color) (background dark))
     (:background "DarkRed"
                  :foreground "White"))))
 ;; Org-mode
 '(org-date                             ;日期
   ((((class color) (background dark))
     (
      :foreground "ivory4"
                  :underline t))))
 '(org-special-keyword                  ;关键字
   ((((class color) (min-colors 16) (background dark))
     (:foreground "rosybrown1"))))
 '(org-level-3                          ;第三级
   ((t (
        :inherit outline-3
                 :foreground "DeepSkyBlue"))))
 '(org-level-5                          ;第五级
   ((t (
        :inherit outline-5
                 :foreground "VioletRed3"))))
 '(org-level-6                          ;第六级
   ((t (
        :inherit outline-6
                 :foreground "violet"))))
 '(org-level-7                          ;第七级
   ((t (
        :inherit outline-7
                 :foreground "khaki3"))))
 '(org-level-8                          ;第八级
   ((t (
        :inherit outline-8
                 :foreground "DarkSeaGreen"))))
 '(org-hide                             ;隐藏星号
   ((((background dark))
     (:foreground "black"))))
 '(org-ellipsis                         ;省略号
   ((((class color) (background dark))
     (
      :background "black"
                  :foreground "Cyan"
                  :strike-through nil
                  ))))
 ;; Minibuffer
 '(minibuffer-prompt                    ;提示
   ((((background dark))
     (:foreground "green"))))
 ;; Isearch
 '(isearch                              ;搜索关键字
   ((((class color) (min-colors 88) (background dark))
     (
      :background "brown"
                  :foreground "white"))))
 '(isearch-fail                         ;搜索失败
   ((((class color) (min-colors 88) (background dark))
     (:background "red4"
                  :foreground "white"))))
 ;; Dired
 '(diredp-date-time                     ;修改时间
   ((t (:foreground "Grey60"))))
 '(diredp-deletion                      ;删除标记
   ((t (:background "Black" :foreground "red"))))
 '(diredp-deletion-file-name            ;删除文件
   ((t (:foreground "red"))))
 '(diredp-dir-heading                   ;目录
   ((t (:background "Black" :foreground "Gold"))))
 '(diredp-dir-priv                      ;目录掩码
   ((t (:background "Black" :foreground "DodgerBlue"))))
 '(diredp-display-msg                   ;路径
   ((t (:foreground "Gold"))))
 '(diredp-exec-priv                     ;可执行掩码
   ((t (:background "Black" :foreground "DeepSkyBlue3"))))
 '(diredp-file-name                     ;文件
   ((t (:foreground "Green3"))))
 '(diredp-file-suffix                   ;文件扩展名
   ((t (:foreground "Green4"))))
 '(diredp-flag-mark                     ;选中标记
   ((t (:background "Black" :foreground "Cyan"))))
 '(diredp-flag-mark-line                ;选中文件
   ((t (:background "Black" :foreground "Cyan"))))
 '(diredp-ignored-file-name             ;忽略的文件
   ((t (:foreground "grey40"))))
 '(diredp-no-priv                       ;无掩码
   ((t (:background "Black" :foreground "Green"))))
 '(diredp-other-priv                    ;其他掩码
   ((t (:background "Black" :foreground "khaki"))))
 '(diredp-rare-priv                     ;稀有的掩码
   ((t (:background "Black" :foreground "Red"))))
 '(diredp-read-priv                     ;读取掩码
   ((t (:background "Black" :foreground "IndianRed"))))
 '(diredp-write-priv                    ;写入掩码
   ((t (:background "Black" :foreground "Gold3"))))
 ;; Yasnippet
 '(yas/field-highlight-face             ;模版区域
   ((t (
        :background "grey20"
                    :foreground "gold"))))
 '(yas/mirror-highlight-face            ;同步模版区域
   ((t (
        :background "brown"
                    :foreground "white"))))
 ;; Tabbar标签颜色
 '(tabbar-default                       ;默认
   ((((class color grayscale)
      (background dark))
     (
      :inherit variable-pitch
               :height 1.1
               :family "DejaVu Sans YuanTi"
               ))))
 '(tabbar-separator                     ;分隔线
   ((t (
        :inherit tabbar-default
                 :background "black"
                 :foreground "brown" :height 0.1
                 ))))
 '(tabbar-button-highlight              ;按钮
   ((t (
        :inherit tabbar-default
                 :background "black"
                 :foreground "green"
                 :box (:color "red")
                 ))))
 '(tabbar-button
   ((t (
        :inherit tabbar-default
                 :background "black"
                 :foreground "red"
                 :box (
                       :line-width 1
                                   :color "black"
                                   :style released-button)))))
 '(tabbar-selected                      ;当前正在使用的标签
   ((t (
        :inherit tabbar-default
                 :background "black"
                 :foreground "LawnGreen"
                 :box (
                       :line-width 1
                                   :color "#014500"
                                   :style released-button)))))
 '(tabbar-selected-face
   ((t (
        :inherit tabbar-default-face
                 :background "black"
                 :foreground "grey"
                 :box (
                       :line-width -1
                                   :color "grey"
                                   :style released-button)))))
 '(tabbar-unselected                    ;未使用的标签
   ((t (
        :inherit tabbar-default
                 :background "black"
                 :foreground "#10650F"
                 :box (
                       :line-width 1
                                   :color "Grey10"
                                   :style pressed-button)))))
 '(tabbar-unselected-face
   ((t (
        :inherit tabbar-default-face
                 :background "black"
                 :foreground "white"
                 :box (
                       :line-width -1
                                   :color "black"
                                   :style pressed-button)))))
 ;; Widget
 '(widget-field                         ;输入区域
   ((((class grayscale color) (background dark))
     (
      :background "grey10"
                  :foreground "DeepSkyBlue"))))
 '(widget-single-line-field             ;单行输入区域
   ((((class grayscale color) (background dark))
     (
      :background "grey10"
                  :foreground "DeepSkyBlue"))))
 ;; ECB
 '(ecb-analyse-face                     ;分析
   ((((class color) (background dark))
     (
      :inherit ecb-default-highlight-face
               :background "black"
               :foreground "DeepSkyBlue"))))
 '(ecb-default-highlight-face           ;默认高亮
   ((((class color) (background dark))
     (
      :background "black"
                  :foreground "DeepSkyBlue"))))
 '(ecb-directory-face                   ;目录
   ((((class color) (background dark))
     (
      :inherit ecb-default-highlight-face
               :background "black"
               :foreground "DeepSkyBlue"))))
 '(ecb-history-face                     ;历史记录
   ((((class color) (background dark))
     (
      :inherit ecb-default-highlight-face
               :background "black"
               :foreground "DeepSkyBlue"))))
 '(ecb-method-face                      ;方法
   ((((class color) (background dark))
     (
      :inherit ecb-default-highlight-face
               :background "black"
               :foreground "DeepSkyBlue"))))
 '(ecb-source-face                      ;源代码
   ((((class color) (background dark))
     (
      :inherit ecb-default-highlight-face
               :background "black"
               :foreground "DeepSkyBlue"))))
 '(ecb-tag-header-face                  ;标签头
   ((((class color) (background dark))
     (
      :background "grey30"
                  :foreground "white"))))
 '(ecb-tree-guide-line-face             ;向导线
   ((((class color) (background dark))
     (
      :inherit ecb-default-general-face
               :foreground "DeepSkyBlue"
               :height 1.0))))
 ;; Comint
 '(comint-highlight-input               ;命令行输入
   ((t (
        :background "black"
                    :foreground "gold3"
                    :weight bold))))
 '(comint-highlight-prompt              ;命令行提示
   ((((min-colors 88)
      (background dark))
     (
      :foreground "Green"))))
 ;; W3M浏览器颜色
 '(w3m-anchor                           ;未访问的标题
   ((((class color) (background dark))
     (:foreground "DodgerBlue2"
                  :underline t))))
 '(w3m-arrived-anchor                   ;已访问的标题
   ((((class color) (background dark))
     (:foreground "Purple4"
                  :underline t))))
 '(w3m-bold                             ;高亮光键字
   ((t (:foreground "Green3"
                    :weight bold))))
 '(w3m-current-anchor                   ;当前标题
   ((t (:box (:line-width -1
                          :color "Grey30")
             :underline t))))
 '(w3m-form                             ;表格
   ((((class color) (background dark))
     (:foreground "Red"
                  :box nil
                  :underline "DarkRed"
                  ))))
 '(w3m-form-button                      ;表格按钮
   ((((type x w32 mac) (class color))
     (:background "black"
                  :foreground "LawnGreen"
                  :box (:line-width -1
                                    :color "#014500"
                                    :style released-button)))))
 '(w3m-form-button-mouse                ;表格按钮鼠标经过
   ((((type x w32 mac) (class color))
     (:background "Black"
                  :foreground "Red"
                  :box (:line-width -1
                                    :color "Grey30"
                                    :style released-button)))))
 '(w3m-form-button-pressed              ;表格按钮鼠标按下
   ((((type x w32 mac) (class color))
     (:background "Black"
                  :foreground "DarkRed"
                  :box (:line-width -1
                                    :color "Grey60"
                                    :style pressed-button)))))
 '(w3m-form-face                        ;表格中字体
   ((((class color) (background dark))
     (:foreground "khaki2"
                  :underline "brown"
                  ))) t)
 '(w3m-header-line-location-content     ;地址内容
   ((((class color) (background dark))
     (:background "black"
                  :foreground "Green"))))
 '(w3m-header-line-location-title       ;地址标题
   ((((class color) (background dark))
     (:background "black"
                  :foreground "brown"))))
 '(w3m-history-current-url              ;当前历史连接
   ((t (:background "black"
                    :foreground "DodgerBlue"))))
 '(w3m-image                            ;图像
   ((((class color) (background dark))
     (:background "Black"
                  :foreground "DarkRed"))))
 '(w3m-image-anchor                     ;图像锚定
   ((((class color) (background dark))
     (:background "Black"))))
 '(w3m-session-select                   ;任务选择
   ((((class color) (background dark))
     (:foreground "grey50"))))
 '(w3m-tab-background                   ;标签背景
   ((((type x w32 mac) (class color))
     (:background "black"
                  :foreground "black"))))
 '(w3m-tab-selected-background          ;标签选择背景
   ((((type x w32 mac) (class color))
     (:background "black"
                  :foreground "black"))))
 '(w3m-tab-mouse                        ;鼠标点击标签
   ((((type x w32 mac) (class color))
     (:background "DarkRed"
                  :foreground "white"
                  :box (:line-width -1
                                    :color "Red"
                                    :style released-button)))))
 '(w3m-tab-selected                     ;选择的浏览过的标签
   ((((type x w32 mac) (class color))
     (:background "black"
                  :foreground "LawnGreen"
                  :box (:line-width -1
                                    :color "#014500"
                                    :style released-button)))))
 '(w3m-tab-selected-retrieving          ;选择的死掉的标签
   ((((type x w32 mac) (class color))
     (:background "black"
                  :foreground "grey80"
                  :box (:line-width -1
                                    :color "Grey40"
                                    :style released-button)))))
 '(w3m-tab-unselected                   ;未选择已浏览的标签
   ((((type x w32 mac) (class color))
     (:background "black"
                  :foreground "#10650F"
                  :box (:line-width 1
                                    :color "Black"
                                    :style pressed-button)))))
 '(w3m-tab-unselected-retrieving        ;未选择的死掉的标签
   ((((type x w32 mac) (class color))
     (:background "black"
                  :foreground "grey30"
                  :box (:line-width 1
                                    :color "Black"
                                    :style pressed-button)))))
 '(w3m-tab-unselected-unseen            ;未选择的没有浏览过的标签
   ((((type x w32 mac) (class color))
     (:background "black"
                  :foreground "DodgerBlue"
                  :box (:line-width 1
                                    :color "black"
                                    :style pressed-button)))))
 '(w3m-link-numbering                   ;数字连接
   ((((class color) (background dark))
     (:background "Black"
                  :foreground "Grey"))))
 ;; 行号
 '(linum                                ;行号
   ((t (
        :background "black"
                    :foreground "green3"
                    ))))
 ;; Tooltip
 '(tooltip                              ;默认
   ((((class color))
     (
      :inherit variable-pitch
               :background "DarkRed"
               :foreground "White"
               :family "DejaVu Sans YuanTi"
               ))))
 ;; Showtip
 '(showtip-face
   ((((class color))
     (:inherit tooltip
               :background "#730D0D"
               :foreground "White"
               :height 1.0
               :family "DejaVu Sans YuanTi"
               ))))
 ;; 语法高亮
 '(show-paren-match                     ;括号匹配
   ((((class color)
      (background dark))
     (
      :background "green"
                  :foreground "black"))))
 '(show-paren-mismatch                  ;括号没有匹配
   ((((class color))
     (
      :background "red"
                  :foreground "white"))))
 '(font-lock-warning-face               ;警告
   ((((class color) (min-colors 88)
      (background dark))
     (
      :foreground "red"
                  :weight bold))))
 '(font-lock-doc-face                   ;解释文档
   ((t (
        :inherit font-lock-string-face
                 :foreground "khaki4"))))
 '(font-lock-builtin-face               ;内建
   ((((class color) (min-colors 88)
      (background dark))
     (:foreground "RosyBrown4"))))
 '(font-lock-constant-face              ;常量
   ((((class color) (min-colors 88)
      (background dark))
     (:foreground "magenta4"))))
 '(font-lock-string-face                ;字符
   ((((class color) (min-colors 88)
      (background dark))
     (:foreground "DarkKhaki"))))
 '(font-lock-comment-face               ;注释
   ((((class color) (min-colors 88)
      (background dark))
     (:foreground "OrangeRed3"))))
 '(font-lock-keyword-face               ;关键字
   ((((class color) (min-colors 88)
      (background dark))
     (:foreground "#0048FF"
                  :weight bold
                  ))))
 '(font-lock-function-name-face         ;函数
   ((((class color) (min-colors 88)
      (background dark))
     (:foreground "gold3"
                  ))))
 '(font-lock-type-face                  ;类型
   ((((class color) (min-colors 88)
      (background dark))
     (:foreground "DeepSkyBlue2"))))
 '(font-lock-preprocessor-face          ;预处理字
   ((t (
        :inherit font-lock-builtin-face
                 :foreground "Cyan3"
                 :weight bold
                 ))))
 '(font-lock-variable-name-face         ;变量
   ((((class color) (min-colors 88)
      (background dark))
     (:foreground "DarkOrchid"))))
 ;; 高亮 CL 函数
 '(highlight-cl                         ;`cl' 函数
   ((t (:foreground "#20ABFC"
                    :underline nil))))
 '(highlight-cl-and-other               ;`cl' 函数， 但是定义在其他包
   ((t (:foreground "#20ABFC"
                    :underline nil))))
 '(highlight-cl-macro                   ;`cl' 宏
   ((t (:underline nil))))
 ;; Mode-line
 '(mode-line                            ;正在使用的标签迷你BUFFER
   ((t (
        :foreground "White"               ;前景色
                    :background "DarkRed" ;背景色
                    :box (
                          :line-width -1
                                      :style released-button)))))
 '(mode-line-inactive                   ;未使用的标签迷你BUFFER
   ((default (:inherit mode-line))
    (((class color) (min-colors 88)
      (background dark))
     (
      :background "Black"
                  :foreground "Green4"
                  :box (
                        :line-width -1
                                    :color "#013500"
                                    :style released-button)
                  :weight light))))
 '(mode-line-highlight                  ;高亮
   ((((class color) (min-colors 88))
     (:box (:line-width 1
                        :color "Green4"
                        :style released-button)))))
 '(region                               ;选中区域的颜色
   ((((class color)
      (min-colors 88) (background dark))
     (
      :background "Green3"
                  :foreground "black"))))
 '(which-func                           ;当前函数
   ((((class color) (min-colors 88)
      (background dark))
     (:foreground "Yellow"))))
 ;; Xrefactory
 '(custom-group-tag                     ;组标签
   ((t (
        :inherit variable-pitch
                 :foreground "DodgerBlue"
                 :underline t
                 :weight bold
                 :height 1.2))))
 '(custom-variable-tag                  ;变量标签
   ((t (
        :foreground "gold"
                    :underline t
                    :weight bold))))
 '(xref-keyword-face                    ;关键字
   ((t (:foreground "grey"))) t)
 '(xref-list-pilot-face                 ;引导
   ((t (:foreground "gold"))) t)
 '(xref-list-symbol-face                ;符号
   ((t (:foreground "green"))) t)
 ;; Whitespace
 '(whitespace-highlight                 ;空格
   ((((class color)
      (background dark))
     (
      :background "yellow2"
                  :foreground "black"))))
 ;; Highlight
 '(highlight ((((class color)
                (min-colors 88) (background dark))
               (:background "DarkRed"               ;背景色
                            :foreground "White")))) ;前景色
 ;; Highlight symbol
 '(highlight-symbol-face
   ((((class color)
      (background dark))
     (
      :background "grey"                ;背景色
                  :foreground "white"   ;前景色
                  ))))
 ;; 窗口边缘
 '(fringe ((((class color)
             (background dark))
            (:background "grey10"))))
 ;; Speedbar
 '(speedbar-file-face                   ;文件
   ((((class color)
      (background dark))
     (:foreground "SeaGreen2"))))
 '(speedbar-highlight-face              ;高亮
   ((((class color)
      (background dark))
     (
      :background "LightGoldenrod"
                  :foreground "black"))))
 '(speedbar-selected-face               ;选中
   ((((class color)
      (background dark))
     (
      :foreground "Cyan"
                  :underline t))))
 '(speedbar-separator-face              ;分隔线
   ((((class color)
      (background dark))
     (
      :background "DarkRed"
                  :foreground "white" :overline "gray"))))
 ;; Flymake
 '(flymake-errline                      ;错误
   ((((class color))
     (:background "DarkRed"
                  :foreground "White"))))
 '(flymake-warnline                     ;警告
   ((((class color))
     (:background "Gold3"
                  :foreground "Black"))))
 ;; Lazy highlight
 '(lazy-highlight
   ((((class color)
      (min-colors 88)
      (background dark))
     (:background "grey20"))))
 ;; Isearch
 '(isearch
   ((((class color)
      (min-colors 88)
      (background dark))
     (:background "khaki" :foreground "black"))))
 ;; Completion dynamic
 '(completion-dynamic-face
   ((((class color)
      (background dark))
     (
      :background "DarkOrange"
                  :foreground "black"))))
 ;; Top-mode
 '(top-mode-mark-face
   (quote isearch))
 ;; Pabbrev
 '(pabbrev-suggestions-face             ;关键字补全
   ((((class color)
      (background dark))
     (:background "Black"
                  :foreground "khaki1"))))
 '(pabbrev-suggestions-label-face       ;标签
   ((t (:background "Black"
                    :foreground "Grey"
                    :inverse-video nil))))
 ;; Completion ui
 '(completion-tooltip-face
   ((t (:inherit tooltip
                 :background "grey5"
                 :foreground "khaki1"
                 :family "DejaVu Sans YuanTi"
                 ))))
 ;; modelinepos-column-warning
 '(modelinepos-column-warning           ;列数限制警告
   ((t (:foreground "Yellow"))))
 ;; Gnus
 '(gnus-header-content                  ;报头内容
   ((t (
        :foreground "Green"
                    :slant italic))))
 '(gnus-header-from                     ;报头发信
   ((((class color)
      (background dark))
     (:foreground "khaki"))))
 '(gnus-header-name                     ;报头名字
   ((((class color)
      (background dark))
     (:foreground "DodgerBlue"))))
 '(gnus-header-subject                  ;报头题目
   ((((class color)
      (background dark))
     (:foreground "HotPink"))))
 '(gnus-signature                       ;签名
   ((t (
        :foreground "Orange"
                    :slant italic))))
 '(gnus-summary-high-read               ;已读, 高兴趣
   ((t (
        :foreground "Gold2"
                    :weight bold
                    ))))
 '(gnus-summary-normal-read             ;已读, 正常兴趣
   ((((class color) (background dark))
     (:foreground "khaki2"
                  ))))
 '(gnus-summary-low-read                ;已读, 低兴趣
   ((t (
        :foreground "Gold4"
                    :slant italic
                    ))))
 '(gnus-summary-high-ancient            ;旧的, 高兴趣
   ((t (
        :foreground "Grey50"
                    :weight bold
                    ))))
 '(gnus-summary-normal-ancient          ;旧的, 正常兴趣
   ((((class color) (background dark))
     (:foreground "Grey40"
                  ))))
 '(gnus-summary-low-ancient             ;旧的, 低兴趣
   ((t (
        :foreground "Grey10"
                    :slant italic
                    ))))
 '(gnus-cite-1                          ;引用, 一级
   ((((class color) (background dark))
     (:foreground "Grey50"))))
 '(gnus-button                          ;按钮
   ((t (:foreground "khaki3" :weight bold))))
 '(italic                               ;发件人
   ((t (:underline nil
                   :slant normal))))
 ;; Mail
 '(mm-uu-extract                        ;摘录
   ((((class color)
      (background dark))
     (:background "Black"
                  :foreground "Gold3"))))
 ;; hideshow
 '(hs-face                              ;折叠颜色
   ((t (
        :background "DarkRed"
                    :foreground "grey"
                    :box (:line-width 1 :color "grey50")))))
 '(hs-fringe-face                       ;折叠边缘颜色
   ((t (
        :background "DarkRed"
                    :foreground "grey"
                    :box (:line-width 2 :color "grey75" :style released-button)))))
 ;; Anything
 '(anything-header                      ;标题
   ((t (
        :background "Black"
                    :foreground "Gold"
                    :underline t))) t)
 '(anything-isearch-match               ;isearch 匹配
   ((t (
        :background "White"
                    :foreground "DarkRed"))))
 '(anything-file-name                   ;文件名
   ((t (:foreground "Green3"))))
 '(anything-dir-priv                    ;目录名
   ((t (:foreground "Grey"))))
 ;; Completions
 '(completions-common-part              ;补全相同部分
   ((t (:foreground "Green3"))))
 '(completions-first-difference         ;补全不同部分
   ((t (:foreground "Grey60"))))
 ;; Ascii
 '(ascii-ascii-face                     ;ascii字符的编码
   ((((class color)
      (background dark))
     (
      :background "Black"
                  :foreground "Grey"))))
 '(ascii-non-ascii-face                 ;Non-ascii字符的编码
   ((((class color)
      (background dark))
     (
      :background "Black"
                  :foreground "Gold"))))
 ;; Info
 '(info-menu-header                     ;菜单标题
   ((t (
        :inherit variable-pitch
                 :foreground "khaki3"
                 :weight bold
                 ))))
 '(info-title-1                         ;标题1
   ((t (
        :inherit info-title-2
                 :foreground "Gold"
                 :height 1.1
                 ))))
 '(info-title-2                         ;标题2
   ((t (
        :inherit info-title-3
                 :foreground "red"
                 :height 1.1
                 ))))
 '(info-title-3                         ;标题3
   ((t (
        :inherit info-title-4
                 :foreground "DodgerBlue"
                 :height 1.1
                 ))))
 '(info-title-4                         ;标题4
   ((t (
        :inherit variable-pitch
                 :foreground "Green"
                 :weight bold
                 ))))
 '(info-elisp-command-ref-item          ;elisp命令引用项目
   ((t (:background "Black"
                    :foreground "yellow3"))))
 '(info-elisp-function-ref-item         ;elisp函数引用项目
   ((t (:background "Black"
                    :foreground "Gold3"))))
 '(info-elisp-macro-ref-item            ;elisp宏引用项目
   ((t (:background "Black"
                    :foreground "Yellow3"))))
 '(info-elisp-reference-item            ;elisp引用项目
   ((t (:background "Black"
                    :foreground "DarkRed"))))
 '(info-elisp-special-form-ref-item     ;elisp特殊表格引用项目
   ((t (:background "Black"
                    :foreground "OrangeRed2"))))
 '(info-elisp-syntax-class-item         ;elisp语法类型项目
   ((t (:background "Black"
                    :foreground "Khaki3"))))
 '(info-elisp-user-option-ref-item      ;elisp用户选项引用项目
   ((t (:background "Black"
                    :foreground "LawnGreen"))))
 '(info-elisp-variable-ref-item         ;elisp变量引用项目
   ((t (:background "Black"
                    :foreground "#0048FF"))))
 '(info-file                            ;文件
   ((t (:background "Black"
                    :foreground "Blue"))))
 '(info-menu                            ;菜单
   ((t (:foreground "DarkRed"))))
 '(info-quoted-name                     ;引用名字
   ((t (:foreground "Purple"))))
 '(info-string                          ;字符串
   ((t (:foreground "Grey50"))))
 ;; Customzie
 '(custom-button
   ((((type x w32 ns)                   ;自定义按钮
      (class color))
     (
      :background "darkred"
                  :foreground "white"
                  :box (:line-width 1 :style released-button)
                  ))))
 '(custom-comment                       ;自定义注释
   ((((class grayscale color) (background dark))
     (
      :background "grey5"
                  :foreground "green"
                  ))))
 '(custom-group-tag                     ;组标签
   ((t (:inherit variable-pitch
                 :foreground "gold"
                 :underline t
                 :weight bold
                 :height 1.2
                 ))))
 ;; icicles
 '(icicle-candidate-part                ;候选部分
   ((((background dark))
     (:background "Black"
                  :foreground "Purple"))))
 '(icicle-complete-input                ;补全输入
   ((((background dark))
     (:foreground "Gold"))))
 '(icicle-completion                    ;补全
   ((((background dark))
     (:foreground "Gold"))))
 '(icicle-current-candidate-highlight   ;当前候选高亮
   ((((background dark))
     (:background "DarkRed" :foreground "White"))))
 '(icicle-input-completion-fail         ;输入补全失败
   ((((background dark))
     (:background "DarkRed"
                  :foreground "White"))))
 '(icicle-input-completion-fail-lax     ;lax输入补全失败
   ((((background dark))
     (:background "khaki"
                  :foreground "Black"))))
 '(icicle-match-highlight-Completions   ;匹配补全高亮
   ((((background dark))
     (:foreground "DodgerBlue1"))))
 '(icicle-multi-command-completion      ;多重命令补全
   ((((background dark))
     (:foreground "Gold"))))
 '(icicle-mustmatch-completion          ;匹配补全
   ((((type x w32 mac graphic) (class color))
     (:inherit nil))))
 '(icicle-saved-candidate               ;保存候选
   ((((background dark))
     (:background "Black"
                  :foreground "khaki"))))
 '(icicle-special-candidate             ;特殊候选
   ((((background dark))
     (:background "Black"
                  :foreground "Grey"))))
 '(icicle-whitespace-highlight          ;空格高亮
   ((((background dark))
     (:background "DarkRed"))))
 ;; hl-line+
 '(hl-line ((t                          ;当前行高亮背景色
             (:background "grey5"))))
 ;; col-highlight
 '(col-highlight                        ;当前列的高亮背景色
   ((t (:background "Grey5"))))
 ;; hl-sexp
 '(hl-sexp-face                         ;高亮 sexp
   ((((class color) (background dark))
     (:background "gray2"))))
 ;; Emms Playlist
 '(emms-playlist-selected-face          ;设定选中项目文字的颜色
   ((t (:foreground "Green"))))
 '(emms-playlist-track-face             ;设定播放列表文字的底色
   ((t (:foreground "DarkGreen"))))
 ;; Emms Browser
 '(emms-browser-album-face              ;专辑
   ((((class color) (background dark))
     (:foreground "Green3"
                  :height 1.1))))
 '(emms-browser-artist-face             ;艺术家
   ((((class color) (background dark))
     (:foreground "Gold3"
                  :height 1.3))))
 '(emms-browser-track-face              ;歌曲
   ((((class color) (background dark))
     (:foreground "khaki3"
                  :height 1.0))))
 ;; Rcirc
 '(rcirc-bright-nick                    ;关注的妮称
   ((((class color) (min-colors 88) (background dark))
     (:foreground "White"))))
 '(rcirc-dim-nick                       ;忽略的妮称
   ((t (:foreground "Grey25"))))
 '(rcirc-keyword
   ((t (:foreground "khaki"             ;关键字高亮
                    :slant normal
                    :weight ultra-bold))))
 '(rcirc-my-nick                        ;我的妮称
   ((((class color) (min-colors 88) (background dark))
     (:foreground "Green3"
                  :weight semi-bold))))
 '(rcirc-nick-in-message                ;消息中我的妮称
   ((((class color) (min-colors 88) (background dark))
     (:foreground "Gold"))))
 '(rcirc-nick-in-message-full-line      ;消息中我的妮称, 全行
   ((t (:underline "grey20"))))
 '(rcirc-other-nick                     ;其他妮程
   ((((class color) (min-colors 88) (background dark))
     (:foreground "tomato"))))
 '(rcirc-prompt                         ;提示符
   ((((min-colors 88) (background dark))
     (:foreground "Purple"))))
 '(rcirc-server                         ;服务器消息
   ((((class color) (min-colors 88) (background dark))
     (:foreground "DarkRed"))))
 '(rcirc-server-prefix                  ;服务器消息前缀
   ((default (:foreground "khaki4"))
    (((class color) (min-colors 16)) nil)))
 '(rcirc-timestamp                      ;时间轴
   ((t (:foreground "grey35"))))
 '(rcirc-track-keyword                  ;关键字跟踪
   ((t (:foreground "Yellow"
                    :weight bold))))
 '(rcirc-track-nick                     ;妮称跟踪
   ((t (:foreground "Green"))))
 '(rcirc-url                            ;连接
   ((t (:foreground "Grey50"
                    :weight ultra-light))))
 ;; eperiodic (化学元素周期表)
 '(eperiodic-d-block-face               ;D 块
   ((((class color) (background dark))
     (:inherit eperiodic-generic-block-face
               :background "DarkRed"
               :foreground "White"))))
 '(eperiodic-f-block-face               ;F 块
   ((((class color) (background dark))
     (:inherit eperiodic-generic-block-face
               :background "DarkRed"
               :foreground "Grey"))))
 '(eperiodic-s-block-face               ;S 块
   ((((class color))
     (:inherit eperiodic-generic-block-face
               :background "tan3"
               :foreground "cornsilk2"))))
 '(eperiodic-generic-block-face         ;通用 块
   ((((class color)) nil)))
 '(eperiodic-p-block-face               ;P 块
   ((((class color))
     (:inherit eperiodic-generic-block-face
               :background "Green4"
               :foreground "Grey"))))
 '(eperiodic-header-face                ;标题
   ((t (:foreground "Gold"
                    :weight bold))))
 '(eperiodic-group-number-face          ;组数
   ((t (:inherit eperiodic-generic-block-face
                 :foreground "grey"
                 :weight bold))))
 '(eperiodic-period-number-face         ;周期数
   ((t (:foreground "grey"
                    :weight bold))))
 ;; Company-mode

 ;; cal-china-x
 '(cal-china-x-priority1-holiday-face   ;假日
   ((((class color) (background dark))
     (:background "DarkRed"
                  :foreground "White"))))
 '(cal-china-x-priority2-holiday-face   ;假日
   ((((class color) (background dark))
     (:background "Khaki"
                  :foreground "Black"))))
 ;; Xgtags
 '(xgtags-file-face                     ;文件
   ((((class color) (background dark))
     (:foreground "Grey50"))))
 '(xgtags-file-selected-face            ;选择的文件
   ((t (:foreground "Grey70"
                    :weight bold))))
 '(xgtags-line-face                     ;搜索行
   ((((class color) (background dark))
     (:foreground "khaki4"))))
 '(xgtags-line-selected-face            ;选择的搜索行
   ((t (:foreground "khaki"))))
 '(xgtags-line-number-face              ;行号
   ((((class color) (background dark))
     (:foreground "DarkRed"))))
 '(xgtags-line-number-selected-face     ;选择的行号
   ((t (:foreground "Red"
                    :weight bold))))
 '(xgtags-match-face                    ;关键字
   ((((class color) (background dark))
     (:foreground "Green4"))))
 '(xgtags-match-selected-face           ;选择的关键字
   ((t (:foreground "Green"
                    :weight bold))))
 ;; eldoc
 '(eldoc-highlight-function-argument    ;参数颜色
   ((t (:inherit bold
                 :foreground "Red"))))
 ;; elscreen
 '(elscreen-tab-background-face         ;背景标签
   ((((type x w32 mac) (class color))
     (:background "Black"))))
 '(elscreen-tab-control-face            ;控制标签
   ((((type x w32 mac) (class color))
     (:background "Black"
                  :foreground "Green"))))
 '(elscreen-tab-current-screen-face     ;当前标签
   ((((class color))
     (:background "DarkRed"
                  :foreground "Grey"
                  :box (:line-width -1
                                    :color "Red"
                                    :style released-button)))))
 '(elscreen-tab-other-screen-face       ;背景
   ((((type x w32 mac) (class color))
     (:background "Black"
                  :foreground "Green3"
                  :box (:line-width -1
                                    :color "Grey20"
                                    :style released-button)))))
 ;; Window nubmer
 '(window-number-face ((((type x w32 mac)) (:foreground "Gold"))))
 ;; re-builder
 '(reb-match-0
   ((((class color) (background dark))
     (:background "khaki3"
                  :foreground "Black"))))
 '(reb-match-1
   ((((class color) (background dark))
     (:background "dodgerblue3"
                  :foreground "black"))))
 '(reb-match-2
   ((((class color) (background dark))
     (:background "chartreuse3"
                  :foreground "black"))))
 '(reb-match-3
   ((((class color) (background dark))
     (:background "sienna3"
                  :foreground "black"))))
 ;; Highlight fixme
 '(fixme-face
   ((t (:foreground "orange"
                    :box (:line-width 1
                                      :color "orange")
                    :weight bold))))
 ;; Basic
 '(secondary-selection                  ;次要级的选择
   ((((class color) (min-colors 88) (background dark))
     (:background "Black"))))
 ;; Go-to-char
 '(go-to-char-highlight                 ;跳转到字符高亮
   ((((class color) (background dark))
     (:background "Pink"
                  :foreground "Black"))))
 ;; Match
 '(match                                ;匹配的
   ((((class color) (min-colors 88) (background dark))
     (:background "Black"
                  :foreground "Grey70"
                  :weight extra-bold))))
 ;; Woman
 '(woman-addition                       ;附加的
   ((t (:foreground "Gold3"))))
 '(woman-bold                           ;标题
   ((((background dark))
     (:foreground "Green3"
                  :weight bold))))
 '(woman-italic                         ;参数
   ((((background dark))
     (:foreground "DarkRed"
                  :underline t))))
 '(woman-unknown                        ;未知的
   ((((min-colors 88) (background dark))
     (:foreground "Cyan3"))))
 ;; Company
 '(company-preview                      ;预览
   ((t (:background "gold3"
                    :foreground "black"))))
 '(company-preview-common               ;预览 （公共部分）
   ((t (:background "gold3"
                    :foreground "grey20"))))
 '(company-preview-search               ;预览 （搜索）
   ((t (:background "green4"
                    :foreground "green"))))
 '(company-tooltip                      ;tooltip
   ((t (:background "darkred"
                    :foreground "grey"))))
 '(company-tooltip-common               ;tooltip (公共部分）
   ((t (:inherit company-tooltip
                 :foreground "gold"))))
 '(company-tooltip-common-selection     ;tooltip （选择公共部分）
   ((t (:inherit company-tooltip-selection
                 :foreground "gold"))))
 '(company-tooltip-selection            ;tooltip (选择）
   ((default (:background "red3"
                          :foreground "black"))
    (((class color) (min-colors 88)) (:background "orange1"))))
 ;; Auto-complete
 '(ac-menu-face                         ;菜单颜色
   ((t (:background "Grey10"
                    :foreground "Grey40"))))
 '(ac-selection-face                    ;选择颜色
   ((t (:background "Green4"
                    :foreground "Green"))))
 '(ac-yasnippet-menu-face               ;Yasnippet 菜单颜色
   ((t (:background "Grey10"
                    :foreground "Grey40"))))
 '(ac-yasnippet-selection-face          ;Yasnippet 选择颜色
   ((t (:background "DarkRed"
                    :foreground "Grey"))))
 ;; ERC
 '(erc-direct-msg-face                  ;直接消息
   ((t (:foreground "DodgerBlue"))))
 '(erc-input-face                       ;输入
   ((t (:foreground "Green2"))))
 '(erc-my-nick-face                     ;我的昵称
   ((t (:foreground "DarkRed"
                    :weight bold))))
 '(erc-notice-face                      ;注意
   ((t (:foreground "Gray20"
                    :weight bold))))
 '(erc-prompt-face                      ;提示
   ((t (:background "Black"
                    :foreground "Gold"
                    :weight bold))))
 ;; Egg
 '(egg-diff-hunk-header                 ;不同的标题
   ((((class color) (background dark))
     (:background "grey30"
                  :foreground "Gold"))))
 '(egg-log-HEAD                         ;日志标题
   ((t (:background "Black"
                    :foreground "Red"))))
 '(egg-text-help                        ;文本帮助
   ((t (:inherit egg-text-base
                 :height 0.9))))
 ;; scim-bridge
 '(scim-preedit-default-face            ;预选窗口的默认外观
   ((t (:background "DarkRed"
                    :foreground "White"))))
 ;; rfcview
 '(rfcview-headlink-face                ;链接
   ((t (:foreground "DodgerBlue"))))
 '(rfcview-headname-face                ;节名称
   ((t (:foreground "DarkRed"
                    :underline t :weight bold))))
 '(rfcview-headnum-face                 ;节数字
   ((t (:foreground "DarkRed"
                    :weight bold))))
 '(rfcview-mouseover-face               ;鼠标移动
   ((t (:background "DarkRed"
                    :foreground "white" :weight bold))))
 '(rfcview-rfcnum-face                  ;RFC ID
   ((t (:foreground "Green3"
                    :weight bold))))
 '(rfcview-stdnum-face                  ;STD ID
   ((t (:foreground "Grey"
                    :weight bold))))
 '(rfcview-title-face                   ;标题
   ((t (:foreground "Gold"
                    :weight bold))))
 )

(setq
 ;; rcirc 妮称的随机颜色
 rcirc-colors '("LightGrey" "SlateBlue" "DeepPink"
                "HotPink" "DodgerBlue1" "OliveDrab2"
                "Chartreuse" "LightCyan" "DarkMagenta"
                "DarkKhaki" "Grey" "Pink" "FireBrick3"
                "Chocolate3" "sienna" "Orange2")
 ;; 日历颜色
 calendar-load-hook
 '(lambda ()
    (set-face-foreground 'diary-face "skyblue")
    (set-face-background 'holiday-face "slate blue")
    (set-face-foreground 'holiday-face "white"))
 ;; 邮件图标
 display-time-mail-icon
 '(image
   :type jpeg
   :file "~/MyEmacs/Image/mail1.jpeg"
   :background "DarkRed"
   :ascent center)
 ;; Scim-bridge 光标颜色 (开启 . 关闭)
 scim-cursor-color
 '("Gold3" . "Red3")
 )

(provide 'LazyCatTheme)
