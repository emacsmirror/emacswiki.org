;;; init-emms.el --- Emms configuration

;; Filename: init-emms.el
;; Description: Emms configuration
;; Author: Andy Stewart lazycat.manatee@gmail.com
;; Maintainer: Andy Stewart lazycat.manatee@gmail.com
;; Copyright (C) 2008, 2009, Andy Stewart, all rights reserved.
;; Created: 2008-10-20 09:45:31
;; Version: 0.1
;; Last-Updated: 2008-10-20 09:45:31
;;           By: Andy Stewart
;; URL:
;; Keywords: emms
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
;; Emms configuration
;;

;;; Installation:
;;
;; Put init-emms.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-emms)
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

(emms-devel)                                                              ;选择开发者模式
;; 目录
(setq emms-directory "~/MyEmacs/Configure-File/Emms/")                    ;设置EMMS的目录
(setq emms-history-file "~/MyEmacs/Configure-File/Emms/history")          ;播放列表历史记录
(setq emms-cache-file "~/MyEmacs/Configure-File/Emms/cache")              ;缓存文件
(setq emms-stream-bookmarks-file "~/MyEmacs/Configure-File/Emms/streams") ;网络电台保存文件
(setq emms-score-file "~/MyEmacs/Configure-File/Emms/scores")             ;分数文件
(setq emms-source-file-default-directory "/data/Music/")                  ;设定默认的播放目录
;; 播放设置
(add-hook 'emms-player-finished-hook 'emms-random)          ;当播放完当前的歌曲时随机选择下一首歌曲
(setq emms-playlist-default-major-mode 'emms-playlist-mode) ;设定EMMS用播放列表的主模式
(add-to-list 'emms-track-initialize-functions 'emms-info-initialize-track) ;设定音轨初始化信息
(setq emms-player-next-function 'emms-next)                                ;修复该死的播放完后的BUG
(setq emms-repeat-playlist t)                                              ;设定EMMS启动列表循环播放
(setq emms-history-start-playing t)
(setq emms-playlist-sort-function       ;设置播放列表用自然的方法排序: 艺术家 -> 专辑 -> 序号
      'emms-playlist-sort-by-natural-order)
(emms-mode-line-disable)                    ;不在Mode-line上显示歌曲信息
(setq emms-playing-time-display-format "")  ;不显示歌曲播放时间
(when (fboundp 'emms-cache) (emms-cache 1)) ;设置EMMS缓存
(setq emms-player-list                      ;设定EMMS播放器的优先顺序
      '(emms-player-mplayer
        emms-player-timidity
        emms-player-mpg321
        emms-player-ogg123))
(setq emms-info-asynchronously nil)            ;关闭EMMS信息异步模式, 不然会处错
(setq emms-playlist-buffer-name "*Music*")     ;设定播放列表的缓存标题
(setq emms-source-file-directory-tree-function ;设定更快和灵活的文件目录查找模式
      'emms-source-file-directory-tree-find)
(setq emms-show-format "%s")                 ;设置 `emms-show' 的显示格式
;; 歌词设置
(ad-activate 'emms-lyrics-find-lyric)        ;自动下载歌词
(setq emms-lyrics-dir my-lyrics-directory)   ;EMMS的歌词目录
(setq emms-lyrics-display-format "%s")       ;设置歌词显示格式
(setq emms-lyrics-scroll-timer-interval 1.0) ;歌词滚动延迟
(setq emms-lyrics-display-on-minibuffer nil) ;在minibuffer中显示歌词
(setq emms-lyrics-display-on-modeline nil)   ;在modeline中显示歌词
;; 解析歌手和歌名
(add-to-list 'emms-info-functions 'kid-emms-info-simple)
;; Emms-browser
(setq emms-browser-info-genre-format "%i● %n"
      emms-browser-info-artist-format "%i● %n"
      emms-browser-info-album-format "%i◎ %n"
      emms-browser-info-title-format "%i♪ %n")
;; 设置时间显示格式
(setq emms-last-played-format-alist                      ;最后播放时间格式化
      '(((emms-last-played-seconds-today) . "%H:%M")     ;今天
        (604800                           . "W%w %H:%M") ;这个星期
        ((emms-last-played-seconds-month) . "%d")        ;这个月
        ((emms-last-played-seconds-year)  . "%m-%d")     ;今年
        (t                                . "")))
;; 设置播放列表显示
(setq emms-track-description-function
      'lazycat/emms-info-track-description)
;; EMMS 浏览器
(emms-browser-make-filter               ;显示所有
 "EVERYTHING" 'ignore)
(emms-browser-set-filter                ;设置默认的为显示所有
 (assoc "EVERYTHING" emms-browser-filters))
(emms-browser-make-filter               ;只显示文件
 "ALL-FILES" (emms-browser-filter-only-type 'file))
(emms-browser-make-filter               ;最近一个月播放的
 "LAST-MONTH-PLAYED" (emms-browser-filter-only-recent 30))
(emms-browser-make-filter               ;最近一个月都没有播放的
 "LAST-MONTH-NOT-PLAYED"
 (lambda (track)
   (not (funcall (emms-browser-filter-only-recent 30) track))))
(put 'emms-browser-delete-files 'disabled nil) ;删除文件不提醒
(add-hook 'emms-browser-delete-files-hook ;删除封面和目录, 如果删除当前文件后目录已经没有音乐文件了
          'de-kill-covers-and-parents)
;; 设定 mplayer 支持的格式
(emms-player-set emms-player-mplayer 'regex
                 "\\.ogg\\|\\.mp3\\|\\.wav\\|\\.mpg\\|\\.mpeg\\|\\.wmv\\|\\.wma\\|\\.mov\\|\\.avi\\|\\.divx\\|\\.ogm\\|\\.asf\\|\\.mkv\\|http://\\|mms://\\|\\.rm\\|\\.rmvb\\|\\.mp4\\|\\.flac\\|\\.vob\\|\\.m4a\\|\\.ape\\|\\.mpc")

(provide 'init-emms)

;;; init-emms.el ends here
