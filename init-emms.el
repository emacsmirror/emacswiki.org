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
(require 'emms-extension)               ;emms扩展

;;; Code:

(emms-devel)                            ;选择开发者模式
;; 目录
(make-directory "~/.emacs.d/deepin-emacs/Configure-File/Emms/" t)
(setq emms-directory "~/.emacs.d/deepin-emacs/Configure-File/Emms/")           ;设置EMMS的目录
(setq emms-history-file "~/.emacs.d/deepin-emacs/Configure-File/Emms/history") ;播放列表历史记录
(setq emms-cache-file "~/.emacs.d/deepin-emacs/Configure-File/Emms/cache")     ;缓存文件
(setq emms-stream-bookmarks-file "~/.emacs.d/deepin-emacs/Configure-File/Emms/streams") ;网络电台保存文件
(setq emms-score-file "~/.emacs.d/deepin-emacs/Configure-File/Emms/scores")             ;分数文件
(setq emms-source-file-default-directory "/space/data/Music/") ;设定默认的播放目录
;; 播放设置
(setq emms-repeat-playlist t)           ;设定EMMS启动列表循环播放
(setq emms-playlist-sort-function       ;设置播放列表用自然的方法排序: 艺术家 -> 专辑 -> 序号
      'emms-playlist-sort-by-natural-order)
(emms-mode-line-disable)                ;不在Mode-line上显示歌曲信息
(setq emms-player-list                  ;设定EMMS播放器的优先顺序
      '(emms-player-mplayer
        emms-player-timidity
        emms-player-mpg321
        emms-player-ogg123))
(setq emms-info-asynchronously nil)            ;关闭EMMS信息异步模式
(setq emms-source-file-directory-tree-function ;设定更快和灵活的文件目录查找模式
      'emms-source-file-directory-tree-find)
;; 歌词设置
(ad-activate 'emms-lyrics-find-lyric)        ;自动下载歌词
(setq emms-lyrics-dir "~/.lyrcis")           ;EMMS的歌词目录
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EMMS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar one-key-menu-emms-alist nil
  "The `one-key' menu alist for EMMS.")

(setq one-key-menu-emms-alist
      '(
        (("g" . "Playlist Go") . emms-playlist-mode-go)
        (("d" . "Play Directory Tree") . emms-play-directory-tree)
        (("f" . "Play File") . emms-play-file)
        (("i" . "Play Playlist") . emms-play-playlist)
        (("m" . "Play Matching") . emms-play-matching)
        (("t" . "Add Directory Tree") . emms-add-directory-tree)
        (("c" . "Toggle Repeat Track") . emms-toggle-repeat-track)
        (("v" . "Jump To File") . emms-jump-to-file)
        (("w" . "Toggle Repeat Playlist") . emms-toggle-repeat-playlist)
        (("u" . "Play Now") . emms-play-now)
        (("z" . "Show") . emms-show)
        (("l" . "Lyrics Toggle Show") . emms-lyrics-toggle-display-on-minibuffer)
        (("r" . "Lyrics Re download") . emms-lyrics-redownload-lyric)
        (("e" . "Lyrics Visit") . emms-lyrics-visit-lyric)
        (("s" . "Emms Streams") . emms-streams)
        (("b" . "Emms Browser") . emms-browser)
        (("p" . "Anything Playlist") . anything-emms-playlist)
        (("o" . "Anything Directory") . anything-emms-directory)
        ((";" . "Anything File") . anything-emms-file)
        ))

(defun one-key-menu-emms ()
  "The `one-key' menu for EMMS."
  (interactive)
  (one-key-menu "EMMS" one-key-menu-emms-alist t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EMMS Playlist Sort ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar one-key-menu-emms-playlist-sort-alist nil
  "The `one-key' menu alist for EMMS-PLAYLIST-SORT.")

(setq one-key-menu-emms-playlist-sort-alist
      '(
        (("h" . "Shuffle") . emms-shuffle)
        (("n" . "Name") . emms-playlist-sort-by-name)
        (("t" . "Title") . emms-playlist-sort-by-info-title)
        (("a" . "Artist") . emms-playlist-sort-by-info-artist)
        (("b" . "Album") . emms-playlist-sort-by-info-album)
        (("y" . "Year") . emms-playlist-sort-by-info-year)
        (("e" . "Note") . emms-playlist-sort-by-info-note)
        (("s" . "Scroe") . emms-playlist-sort-by-score)
        (("i" . "List") . emms-playlist-sort-by-list)
        (("o" . "Natural Order") . emms-playlist-sort-by-natural-order)
        (("l" . "Last Played") . emms-playlist-sort-by-last-played)
        (("c" . "Play Count") . emms-playlist-sort-by-play-count)
        ))

(defun one-key-menu-emms-playlist-sort ()
  "The `one-key' menu for EMMS-PLAYLIST-SORT."
  (interactive)
  (one-key-menu "EMMS-PLAYLIST-SORT" one-key-menu-emms-playlist-sort-alist t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EMMS Playlist Mark ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar one-key-menu-emms-playlist-mark-alist nil
  "The `one-key' menu alist for EMMS-PLAYLIST-MARK.")

(setq one-key-menu-emms-playlist-mark-alist
      '(
        (("m" . "Mark Current and Move Next") . emms-mark-track-and-move-next)
        (("a" . "Mark All") . emms-mark-all)
        (("r" . "Mark Regexp") . emms-mark-regexp)
        (("c" . "Mark Copy") . emms-mark-copy-marked-tracks)
        (("x" . "Mark Delete") . emms-mark-delete-marked-tracks)
        (("d" . "Mark Duplicate") . emms-mark-duplicate-track)
        (("t" . "Mark Toggle") . emms-mark-toggle)
        (("u" . "Umark Current") . emms-mark-unmark-track-and-move-next)
        (("U" . "Umark All") . emms-mark-unmark-all)
        ))

(defun one-key-menu-emms-playlist-mark ()
  "The `one-key' menu for EMMS-PLAYLIST-MARK."
  (interactive)
  (one-key-menu "EMMS-PLAYLIST-MARK" one-key-menu-emms-playlist-mark-alist t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EMMS Browser Search ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar one-key-menu-emms-browser-search-alist nil
  "The `one-key' menu alist for EMMS-BROWSER-SEARCH.")

(setq one-key-menu-emms-browser-search-alist
      '(
        (("a" . "Search Artist") . emms-browser-search-by-artist)
        (("b" . "Search Album") . emms-browser-search-by-album)
        (("c" . "Search Composer") . emms-browser-search-by-composer)
        (("n" . "Search Name") . emms-browser-search-by-names)
        (("p" . "Search Performer") . emms-browser-search-by-performer)
        (("t" . "Search Title") . emms-browser-search-by-title)
        ))

(defun one-key-menu-emms-browser-search ()
  "The `one-key' menu for EMMS-BROWSER-SEARCH."
  (interactive)
  (one-key-menu "EMMS-BROWSER-SEARCH" one-key-menu-emms-browser-search-alist t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EMMS Browser Lookup ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar one-key-menu-emms-browser-lookup-alist nil
  "The `one-key' menu alist for EMMS-BROWSER-LOOKUP.")

(setq one-key-menu-emms-browser-lookup-alist
      '(
        (("b" . "Lookup Album Pitchfork") . EMMS-browser-lookup-album-on-pitchfork)
        (("p" . "Lookup Performer Pitchfork") . emms-browser-lookup-performer-on-pitchfork)
        (("c" . "Lookup Composer Pitchfork") . emms-browser-lookup-composer-on-pitchfork)
        (("a" . "Lookup Artist Pitchfork") . emms-browser-lookup-artist-on-pitchfork)
        (("B" . "Lookup Album Wikipedia") . emms-browser-lookup-album-on-wikipedia)
        (("P" . "Lookup Performer Wikipedia") . emms-browser-lookup-performer-on-wikipedia)
        (("C" . "Lookup Composer Wikipedia") . emms-browser-lookup-composer-on-wikipedia)
        (("A" . "Lookup Artist Wikipeda") . emms-browser-lookup-artist-on-wikipedia)
        ))

(defun one-key-menu-emms-browser-lookup ()
  "The `one-key' menu for EMMS-BROWSER-LOOKUP."
  (interactive)
  (one-key-menu "EMMS-BROWSER-LOOKUP" one-key-menu-emms-browser-lookup-alist t))


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

(provide 'init-emms)

;;; init-emms.el ends here
