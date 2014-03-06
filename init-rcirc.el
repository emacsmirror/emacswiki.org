;;; init-rcirc.el --- Rcirc init

;; Filename: init-rcirc.el
;; Description: Rcirc init
;; Author: Andy Stewart lazycat.manatee@gmail.com
;; Maintainer: Andy Stewart lazycat.manatee@gmail.com
;; Copyright (C) 2008, 2009, Andy Stewart, all rights reserved.
;; Created: 2008-10-20 09:28:25
;; Version: 0.1
;; Last-Updated: 2008-10-20 09:28:28
;;           By: Andy Stewart
;; URL:
;; Keywords: rcirc
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
;; Rcirc init
;;

;;; Installation:
;;
;; Put init-rcirc.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-rcirc)
;;
;; No need more.

;;; Change log:
;;
;; 2008/10/20
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

(setq rcirc-default-full-name (eval my-full-name)) ;设置全名
(setq rcirc-default-nick (eval my-irc-nick))       ;设置昵称
(setq rcirc-default-user-name (eval my-name))      ;名字
(setq rcirc-server-alist                           ;rcirc服务器和加入频道
      `(("irc.freenode.net" :channels ,my-irc-channel-list)))
(setq rcirc-log-directory "~/.emacs.d/deepin-emacs/Configure-File/Rcirc/logs") ;rcirc聊天记录
(setq rcirc-notify-open t)                                       ;默认打开消息提醒模式
(setq rcirc-notify-timeout 1)           ;同一个人发信息给我的延迟 (单位: 秒)
(setq rcirc-authinfo                    ;rcirc用户名和密码
      '(("freenode" nickserv (eval my-irc-nick) (eval my-irc-passwd))))
(setq rcirc-omit-responses              ;设置忽略的响应类型
      (quote ("JOIN" "PART" "QUIT" "NICK" "AWAY" "MODE")))
(setq rcirc-prompt "> ")                                   ;提示的符号
(setq rcirc-time-format "[%H:%M] ")                        ;时间格式
(setq rcirc-track-minor-mode nil)                          ;关闭mode-line提示
(add-hook 'rcirc-mode-hook '(lambda () (rcirc-omit-mode))) ;默认打开忽略模式
(add-hook 'rcirc-print-hooks 'rcirc-write-log)             ;写入日志

(provide 'init-rcirc)

;;; init-rcirc.el ends here
