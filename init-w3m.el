;;; init-w3m.el --- W3m configuration

;; Filename: init-w3m.el
;; Description: W3m configuration
;; Author: Andy Stewart lazycat.manatee@gmail.com
;; Maintainer: Andy Stewart lazycat.manatee@gmail.com
;; Copyright (C) 2008, 2009, Andy Stewart, all rights reserved.
;; Created: 2008-10-20 09:35:25
;; Version: 0.1
;; Last-Updated: 2008-10-20 09:35:35
;;           By: Andy Stewart
;; URL:
;; Keywords: w3m
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
;; W3m configuration
;;

;;; Installation:
;;
;; Put init-w3m.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-w3m)
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

;; 常规设置
(setq browse-url-browser-function 'w3m-browse-url)      ;设定用Emacs－wiki打开URL
(setq browse-url-new-window-flag t)                     ;用适当的浏览器打开新窗口
(setq w3m-home-page my-homepage)                        ;设置首页
(setq w3m-make-new-session t)                           ;当浏览一个新的页面时开启一个新的缓存页面
(setq w3m-use-cookies t)                                ;启用cookie
(setq w3m-use-header-line-title t)                      ;显示标题
(setq w3m-cookie-accept-bad-cookies t)                  ;接收 BAD cookie
(setq w3m-view-this-url-new-session-in-background t)    ;后台打开连接
(setq w3m-new-session-in-background t)                  ;后台建立新任务
(setq w3m-session-time-format "%Y-%m-%d %A %H:%M")      ;上次浏览记录的时间显示格式
(setq w3m-favicon-use-cache-file t)                     ;使用网站图标的缓存文件
(setq w3m-show-graphic-icons-in-mode-line nil)          ;不在mode-line显示网站图标
(setq w3m-keep-arrived-urls 50000)                      ;浏览历史记录的最大值
(setq w3m-keep-cache-size 1000)                         ;缓存的大小
(setq w3m-edit-function (quote find-file-other-window)) ;在其他窗口编辑当前页面
(setq w3m-session-automatic-save t)                     ;退出时自动保存
(setq w3m-session-deleted-save nil)                     ;关闭一个标签时不保存
(setq w3m-default-display-inline-images nil)            ;默认不显示网页中的图像
(setq w3m-toggle-inline-images-permanently t)           ;继续保持当前buffer的图像状态
(setq w3m-enable-google-feeling-lucky nil)              ;禁止使用 Google Lucky
(setq w3m-use-filter t)                                 ;开启过滤
(setq w3m-filter-google-separator "")                   ;设置过滤器的分离字符
(setq w3m-fb-mode t)                                    ;让标签和创建它的FRAME关联
(setq w3m-session-load-crashed-sessions t)              ;默认加载崩溃的对话
(w3m-fb-mode 1)                                         ;可以显示FRAME

;; 存储设置
(setq w3m-default-save-directory my-default-download-directory)             ;设置默认的保存目录
(setq w3m-bookmark-file "~/MyEmacs/Configure-File/Emacs-W3M/bookmark.html") ;设定书签文件
(setq w3m-cookie-file "~/MyEmacs/Configure-File/Emacs-W3M/W3m-cookie")      ;设置w3m-cookie保存位置
(setq w3m-session-file "~/MyEmacs/Configure-File/Emacs-W3M/W3m-session")    ;设定任务保存的文件位置
;; 修改Buffer名字为网页的标题或链接, 以利于 anything 搜索
(add-hook 'w3m-display-hook
          (lambda (url)
            (rename-buffer
             (format "*w3m: %s*"
                     (prettyfy-string (or w3m-current-title
                                          w3m-current-url) 50)) t)))
;; Google 广告过滤
(defun my-w3m-filter-rules-for-google (&rest args)
  "Filter rules for Google in w3m."
  (goto-char (point-min))
  (while (re-search-forward             ;remove publicize from google.cn or google.com
          "\\(赞助商链接\\|<h2>Sponsored Links</h2>\\).*aclk.*\\(</cite></ol><p>\\|在此展示您的广告\\)"
          nil t)
    (replace-match ""))
  (while (re-search-forward             ;remove publicize from google.com (English)
          "<h2>Sponsored Links</h2>.*aclk.*<h2>Search Results</h2>"
          nil t)
    (replace-match "")))
(eval-after-load "w3m-filter"
  '(add-to-list 'w3m-filter-rules
                '("\\`http://www\\.google\\.\\(cn\\|com\\)/"
                  my-w3m-filter-rules-for-google)))

(provide 'init-w3m)

;;; init-w3m.el ends here
