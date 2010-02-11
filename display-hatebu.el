;;;; display-hatebu.el --- はてなブックマークでブックマークされた数をモードラインに表示する
;; $Id: display-hatebu.el,v 1.5 2010/02/11 09:23:38 rubikitch Exp $

;; Copyright (C) 2010  rubikitch

;; Author: rubikitch <rubikitch@ruby-lang.org>
;; Keywords: hypermedia
;; URL: http://www.emacswiki.org/cgi-bin/wiki/download/display-hatebu.el

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; はてなブックマークでブックマークされた数をモードラインに表示します。
;;
;; M-x display-hatebu-set-url でURLをセットすると1分おきにブックマーク数を更新します。

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `display-hatebu-set-url'
;;    ブックマーク数監視対象のURLをセットする。
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;
;;  `display-hatebu-interval'
;;    *ブックマーク数更新の間隔(秒数)。
;;    default = 60
;;  `display-hatebu-ruby-program'
;;    *display-hatebu.elが使うRubyインタプリタ。
;;    default = "ruby"

;;; Installation:
;;
;; Put display-hatebu.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'display-hatebu)
;;
;; No need more.

;;; Customize:
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET display-hatebu RET
;;


;;; History:

;; $Log: display-hatebu.el,v $
;; Revision 1.5  2010/02/11 09:23:38  rubikitch
;; 未ブックマーク時のエラーを修正。
;;
;; Revision 1.4  2010/02/11 08:27:56  rubikitch
;; * display-hatebu-ruby-program: Rubyインタプリタ
;; * バイトコンパイルを黙らせた
;;
;; Revision 1.3  2010/02/11 08:23:48  rubikitch
;; display-hatebu-interval: 更新間隔
;;
;; Revision 1.2  2010/02/11 08:21:49  rubikitch
;; commentary
;;
;; Revision 1.1  2010/02/11 08:18:49  rubikitch
;; Initial revision
;;

;;; Code:

(defvar display-hatebu-version "$Id: display-hatebu.el,v 1.5 2010/02/11 09:23:38 rubikitch Exp $")
(eval-when-compile (require 'cl))
(defgroup display-hatebu nil
  "display-hatebu"
  :group 'emacs)
(defcustom display-hatebu-interval 60
  "*ブックマーク数更新の間隔(秒数)。"
  :type 'integer  
  :group 'display-hatebu)
(defcustom display-hatebu-ruby-program "ruby"
  "*display-hatebu.elが使うRubyインタプリタ。"
  :type 'string
  :group 'display-hatebu)

(defvar display-hatebu-string nil)
(defvar display-hatebu-api-url nil)
(defvar display-hatebu-string-timer nil)
(add-to-list 'global-mode-string 'display-hatebu-string t)

(defun start-process-to-variable (variable command &rest args)
  "コマンドの実行結果を変数に格納する。(非同期)"
  (lexical-let ((variable variable)
                (buf (get-buffer-create " start-process-to-variable")))
    (set-process-sentinel
     (apply 'start-process "start-process-to-variable" buf command args)
     (lambda (&rest ignore)
       (with-current-buffer buf
         (set variable (buffer-substring 1 (1- (point-max))))
         (erase-buffer))))))
;; (start-process-to-variable 'display-hatebu-string "date")

(defun update-display-hatebu-string ()
  (when display-hatebu-api-url
    (start-process-to-variable
     'display-hatebu-string
     display-hatebu-ruby-program "-rjson" "-ropen-uri" "-e" 
     (format "puts(((JSON.parse(open('%s').read)['count'] rescue '0')) << 'users')"
             display-hatebu-api-url))))

;; (update-display-hatebu-string)

(defun display-hatebu-set-url (url)
  "ブックマーク数監視対象のURLをセットする。"
  (interactive "sHatebu URL: ")
  (setq display-hatebu-api-url
        (format "http://b.hatena.ne.jp/entry/json/?url=%s" url))
  (message "Set display-hatebu URL: %s" url)
  (update-display-hatebu-string))

(setq display-hatebu-string-timer
      (run-with-timer 0 display-hatebu-interval 'update-display-hatebu-string))

(provide 'display-hatebu)

;; How to save (DO NOT REMOVE!!)
;; (emacswiki-post "display-hatebu.el")
;;; display-hatebu.el ends here
