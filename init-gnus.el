;;; init-gnus.el --- Configuration file for gnus

;; Filename: init-gnus.el
;; Description: Configuration file for gnus
;; Author: Andy Stewart lazycat.manatee@gmail.com
;; Maintainer: Andy Stewart lazycat.manatee@gmail.com
;; Copyright (C) 2008, 2009, Andy Stewart, all rights reserved.
;; Created: 2008-12-08 23:25:43
;; Version: 0.1
;; Last-Updated: 2008-12-08 23:25:45
;;           By: Andy Stewart
;; URL:
;; Keywords: gnus
;; Compatibility: GNU Emacs 23.0.60.1
;;
;; Features that might be required by this library:
;;
;;
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Configuration file for gnus
;;

;;; Installation:
;;
;; Put init-gnus.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-gnus)
;;
;; No need more.

;;; Change log:
;;
;; 2008/12/08
;;      First released.
;;

;;; Acknowledgements:
;;
;;
;;

;;; TODO
;;
;;
;;

;;; Require


;;; Code:

;; 存储设置
(setq gnus-startup-file "~/.emacs.d/deepin-emacs/Gnus/.newsrc")                  ;初始文件
(setq gnus-default-directory "~/.emacs.d/deepin-emacs/Gnus/")                    ;默认目录
(setq gnus-home-directory "~/.emacs.d/deepin-emacs/Gnus/")                       ;主目录
(setq gnus-dribble-directory "~/.emacs.d/deepin-emacs/Gnus/")                    ;恢复目录
(setq gnus-directory "~/.emacs.d/deepin-emacs/Gnus/News/")                       ;新闻组的存储目录
(setq gnus-article-save-directory "~/.emacs.d/deepin-emacs/Gnus/News/")          ;文章保存目录
(setq gnus-kill-files-directory "~/.emacs.d/deepin-emacs/Gnus/News/trash/")      ;文件删除目录
(setq gnus-agent-directory "~/.emacs.d/deepin-emacs/Gnus/News/agent/")           ;代理目录
(setq gnus-cache-directory "~/.emacs.d/deepin-emacs/Gnus/News/cache/")           ;缓存目录
(setq gnus-cache-active-file "~/.emacs.d/deepin-emacs/Gnus/News/cache/active")   ;缓存激活文件
(setq message-directory "~/.emacs.d/deepin-emacs/Gnus/Mail/")                    ;邮件的存储目录
(setq message-auto-save-directory "~/.emacs.d/deepin-emacs/Gnus/Mail/drafts")    ;自动保存的目录
(setq mail-source-directory "~/.emacs.d/deepin-emacs/Gnus/Mail/incoming")        ;邮件的源目录
(setq nnmail-message-id-cache-file "~/.emacs.d/deepin-emacs/Gnus/.nnmail-cache") ;nnmail的消息ID缓存
(setq nnml-newsgroups-file "~/.emacs.d/deepin-emacs/Gnus/Mail/newsgroup")        ;邮件新闻组解释文件
(setq nntp-marks-directory "~/.emacs.d/deepin-emacs/Gnus/News/marks")            ;nntp组存储目录
(setq mml-default-directory "~/.gnus/")                            ;附件的存储位置
;; 接收邮件或新闻
(setq user-full-name my-full-name)            ;全名
(setq user-mail-address my-mail)              ;邮件地址
(setq gnus-select-method '(nntp "localhost")) ;首要选择方法, 从本地缓冲文件中读取新闻 (leafnode抓取)
(setq gnus-refer-article-method '(nntp "localhost")) ;抓取文章的方法, 和 `gnus-select-method' 一样
(setq gnus-secondary-select-methods                  ;次要选择方法
      '(
        (nnmaildir "Gmail"                        ;nnmaildir后端, 从本地文件中读邮件 (getmail 抓取)
                   (directory "~/.emacs.d/deepin-emacs/Mail/")) ;读取目录
        ))
(setq mail-sources                                 ;邮件源设置
      '((maildir :path "~/.emacs.d/deepin-emacs/Mail/"           ;本地邮件存储位置
                 :subdirs ("cur" "new" "tmp"))))   ;本地邮件子目录划分
;; 发送邮件
(setq send-mail-function 'sendmail-send-it         ;设置邮件发送方法
      message-send-mail-function 'sendmail-send-it ;设置消息发送方法
      sendmail-program "/usr/bin/msmtp"            ;设置发送程序
      mail-specify-envelope-from t                 ;发送邮件时指定信封来源
      mail-envelope-from 'header)                  ;信封来源于 header
(setq gnus-message-archive-group                   ;设置消息归档的组
      '((if (message-news-p)
            "nnfolder+archive:nnfolder"             ;新闻归档
          "nnmaildir+Gmail:inbox")))                ;邮件归档
;; 常规设置
(gnus-agentize)                                     ;开启代理功能, 以支持离线浏览
(setq gnus-inhibit-startup-message t)               ;关闭启动时的画面
(setq gnus-novice-user nil)                         ;关闭新手设置, 不进行确认
(setq gnus-expert-user t)                           ;不询问用户
(setq gnus-show-threads t)                          ;显示邮件线索
(setq gnus-interactive-exit nil)                    ;退出时不进行交互式询问
(setq gnus-use-dribble-file nil)                    ;不创建恢复文件
(setq gnus-always-read-dribble-file nil)            ;不读取恢复文件
(setq gnus-asynchronous t)                          ;异步操作
(setq gnus-large-newsgroup 100)                     ;设置大容量的新闻组默认显示的大小
(setq gnus-large-ephemeral-newsgroup nil)           ;和上面的变量一样, 只不过对于短暂的新闻组
(setq gnus-summary-ignore-duplicates t)             ;忽略具有相同ID的消息
(setq gnus-treat-fill-long-lines t)                 ;如果有很长的行, 不提示
(setq message-confirm-send t)                       ;防止误发邮件, 发邮件前需要确认
(setq message-kill-buffer-on-exit t)                ;设置发送邮件后删除buffer
(setq message-from-style 'angles)                   ;`From' 头的显示风格
(setq message-syntax-checks '((sender . disabled))) ;语法检查
(setq nnmail-expiry-wait 7)                         ;邮件自动删除的期限 (单位: 天)
(setq nnmairix-allowfast-default t)                 ;加快进入搜索结果的组
;; 窗口布局
(gnus-add-configuration
 '(article
   (vertical 1.0
             (summary .35 point)
             (article 1.0))))
;; 显示设置
(setq mm-text-html-renderer 'w3m)                     ;用W3M显示HTML格式的邮件
(setq mm-inline-large-images t)                       ;显示内置图片
(auto-image-file-mode)                                ;自动加载图片
(add-to-list 'mm-attachment-override-types "image/*") ;附件显示图片
;; 概要显示设置
(setq gnus-summary-gather-subject-limit 'fuzzy) ;聚集题目用模糊算法
(setq gnus-summary-line-format "%4P %U%R%z%O %{%5k%} %{%14&user-date;%}   %{%-20,20n%} %{%ua%} %B %(%I%-60,60s%)\n")
(defun gnus-user-format-function-a (header) ;用户的格式函数 `%ua'
  (let ((myself (concat "<" my-mail ">"))
        (references (mail-header-references header))
        (message-id (mail-header-id header)))
    (if (or (and (stringp references)
                 (string-match myself references))
            (and (stringp message-id)
                 (string-match myself message-id)))
        "X" "│")))
(setq gnus-user-date-format-alist             ;用户的格式列表 `user-date'
      '(((gnus-seconds-today) . "TD %H:%M")   ;当天
        (604800 . "W%w %H:%M")                ;七天之内
        ((gnus-seconds-month) . "%d %H:%M")   ;当月
        ((gnus-seconds-year) . "%m-%d %H:%M") ;今年
        (t . "%y-%m-%d %H:%M")))              ;其他
;; 线程的可视化外观, `%B'
(setq gnus-summary-same-subject "")
(setq gnus-sum-thread-tree-indent "    ")
(setq gnus-sum-thread-tree-single-indent "◎ ")
(setq gnus-sum-thread-tree-root "● ")
(setq gnus-sum-thread-tree-false-root "☆")
(setq gnus-sum-thread-tree-vertical "│")
(setq gnus-sum-thread-tree-leaf-with-other "├─► ")
(setq gnus-sum-thread-tree-single-leaf "╰─► ")
;; 时间显示
(add-hook 'gnus-article-prepare-hook 'gnus-article-date-local) ;将邮件的发出时间转换为本地时间
(add-hook 'gnus-select-group-hook 'gnus-group-set-timestamp)   ;跟踪组的时间轴
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)              ;新闻组分组
;; 设置邮件报头显示的信息
(setq gnus-visible-headers
      (mapconcat 'regexp-quote
                 '("From:" "Newsgroups:" "Subject:" "Date:"
                   "Organization:" "To:" "Cc:" "Followup-To" "Gnus-Warnings:"
                   "X-Sent:" "X-URL:" "User-Agent:" "X-Newsreader:"
                   "X-Mailer:" "Reply-To:" "X-Spam:" "X-Spam-Status:" "X-Now-Playing"
                   "X-Attachments" "X-Diagnostic")
                 "\\|"))
;; 用 Supercite 显示多种多样的引文形式
(setq sc-attrib-selection-list nil
      sc-auto-fill-region-p nil
      sc-blank-lines-after-headers 1
      sc-citation-delimiter-regexp "[>]+\\|\\(: \\)+"
      sc-cite-blank-lines-p nil
      sc-confirm-always-p nil
      sc-electric-references-p nil
      sc-fixup-whitespace-p t
      sc-nested-citation-p nil
      sc-preferred-header-style 4
      sc-use-only-preference-p nil)
;; 线程设置
(setq
 gnus-use-trees t                                                       ;联系老的标题
 gnus-tree-minimize-window nil                                          ;用最小窗口显示
 gnus-fetch-old-headers 'some                                           ;抓取老的标题以联系线程
 gnus-generate-tree-function 'gnus-generate-horizontal-tree             ;生成水平树
 gnus-summary-thread-gathering-function 'gnus-gather-threads-by-subject ;聚集函数根据标题聚集
 )
;; 排序
(setq gnus-thread-sort-functions
      '(
        (not gnus-thread-sort-by-date)                               ;时间的逆序
        (not gnus-thread-sort-by-number)))                           ;跟踪的数量的逆序
;; 自动跳到第一个没有阅读的组
(add-hook 'gnus-switch-on-after-hook 'gnus-group-first-unread-group) ;gnus切换时
(add-hook 'gnus-summary-exit-hook 'gnus-group-first-unread-group)    ;退出Summary时
;; 自动更新新消息
(add-hook 'gnus-summary-exit-hook 'gnus-notify+)        ;退出summary模式后
(add-hook 'gnus-group-catchup-group-hook 'gnus-notify+) ;当清理当前组后
(add-hook 'mail-notify-pre-hook 'gnus-notify+)          ;更新邮件时
;; 斑纹化
(setq gnus-summary-stripe-regexp        ;设置斑纹化匹配的正则表达式
      (concat "^[^"
              gnus-sum-thread-tree-vertical
              "]*"))
;; 最后设置
;;(gnus-compile)                          ;编译一些选项, 加快速度

(provide 'init-gnus)

;;; init-gnus.el ends here
