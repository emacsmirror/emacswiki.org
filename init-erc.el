;;; init-erc.el --- Configuration for erc

;; Filename: init-erc.el
;; Description: Configuration for erc
;; Author: Andy Stewart lazycat.manatee@gmail.com
;; Maintainer: Andy Stewart lazycat.manatee@gmail.com
;; Copyright (C) 2008, 2009, Andy Stewart, all rights reserved.
;; Created: 2008-12-04 11:32:27
;; Version: 0.1
;; Last-Updated: 2008-12-04 11:32:30
;;           By: Andy Stewart
;; URL:
;; Keywords: erc
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
;; Configuration for erc
;;

;;; Installation:
;;
;; Put init-erc.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-erc)
;;
;; No need more.

;;; Change log:
;;
;; 2008/12/04
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

(require 'erc)
(require 'erc-highlight-nicknames)
(require 'erc-extension)
(require 'erc-nick-notify)
(require 'init-doi)
(require 'init-irc)
(require 'init-git)
(require 'paste2)
(require 'doi-extension)

;;; Code:

(setq erc-user-full-name (get-git-user-name))                         ;设置全名
(setq erc-autojoin-mode t)                                            ;自动加入
(setq erc-join-buffer 'bury)                                          ;在隐藏的Buffer中加入
(setq erc-autoaway-mode t)                                            ;开启自动离开模块
(setq erc-server-auto-reconnect t)                                    ;开启自动重新链接
(setq erc-prompt ">")                                                 ;提示符号
(setq erc-prompt-for-password nil)                                    ;不用提示密码
(setq erc-prompt-for-nickserv-password nil)                           ;鉴别时不提示密码
(setq erc-fill-static-center 0)                                       ;中间填充
(setq erc-fill-column 100)                                            ;折叠列数
(setq erc-timestamp-right-column 110)                                 ;插入时间戳的列数
(setq erc-timestamp-format "[%H:%M:%S]")                              ;时间错显示格式
(setq erc-timestamp-format-right " [%H:%M:%S]")                       ;右边时间戳显示格式
(setq erc-insert-timestamp-function 'erc-insert-timestamp-right)      ;插入时间戳的方式(右边)
(setq erc-insert-away-timestamp-function 'erc-insert-timestamp-right) ;插入离开时间戳的方式(右边)
(setq erc-track-position-in-mode-line t)                              ;在Mode-line显示频道信息
(setq erc-truncate-mode t)                                            ;开启截断模式
(setq erc-enable-logging nil)                                         ;禁止日志
(setq erc-log-mode nil)                                               ;关闭日志模式
(setq erc-header-line-uses-tabbar-p t)                                ;默认开启 Tabbar
(setq erc-nick-notify-delay '(0 1 0))                                 ;设置延迟为1分钟
(setq erc-nickserv-passwords                                          ;设置自动登录时需要的密码
      '((freenode (((eval erc-nick) . (eval erc-password))))))
(setq erc-autojoin-channels-alist       ;自动加入的服务器和频道
      `(,(cons "freenode.net" '("#emacs"))))
(setq erc-log-channels-directory        ;日志的记录目录
      "~/.emacs.d/deepin-emacs/Configure-File/ERC/logs/")
(dolist (hooked (list
                 'turn-on-eldoc-mode    ;开启elisp参数提醒模式
                 ))
  (add-hook 'erc-mode-hook hooked))
(setq erc-modules                       ;加载的模块
      '(
        autojoin                        ;自动加入
        button                          ;按钮
        completion                      ;补全
        dcc                             ;文件传输
        fill                            ;填充
        irccontrols                     ;IRC控制
        list                            ;列表
        match                           ;匹配
        menu                            ;菜单
        move-to-prompt                  ;移动到提示符
        netsplit                        ;发觉Netsplit
        networks                        ;网络
        noncommands                     ;不显示非IRC命令
        readonly                        ;显示行只读
        ring                            ;输入历史
        services                        ;自动鉴别
        smiley                          ;笑脸转换
        sound                           ;声音
        stamp                           ;时间戳
        track                           ;跟踪
        ;; unmorse                         ;转换莫尔斯码
        highlight-nicknames
        ))
(setq erc-hide-list                     ;需要隐藏的IRC消息的类型
      '(
        "353"                           ;忽略昵称列表
        ;; "JOIN"                          ;加入
        ;; "PART"                          ;离开
        ;; "QUIT"                          ;退出
        ))
(lazy-set-mode-autoload-key
 '(
   ("C-c C-y" . paste2-buffer-create)   ;粘贴大段内容
   ("/" . doi-erc-command)              ;erc命令
   )
 erc-mode-map nil "init-erc"
 )
(lazy-set-mode-autoload-key doi-key-alist erc-mode-map nil "init-erc") ;doi 的局部按键

(provide 'init-erc)

;;; init-erc.el ends here
