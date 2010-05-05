;;;; display-hatebu.el --- はてなブックマークでブックマークされた数をモードラインに表示する
;; $Id: display-hatebu.el,v 1.7 2010/05/04 09:07:47 rubikitch Exp $

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


;;; Bug Report:
;;
;; If you have problem, send a bug report via M-x display-hatebu-send-bug-report.
;; The step is:
;;  0) Setup mail in Emacs, the easiest way is:
;;       (setq user-mail-address "your@mail.address")
;;       (setq user-full-name "Your Full Name")
;;       (setq smtpmail-smtp-server "your.smtp.server.jp")
;;       (setq mail-user-agent 'message-user-agent)
;;       (setq message-send-mail-function 'message-smtpmail-send-it)
;;  1) Be sure to use the LATEST version of display-hatebu.el.
;;  2) Enable debugger. M-x toggle-debug-on-error or (setq debug-on-error t)
;;  3) Use Lisp version instead of compiled one: (load "display-hatebu.el")
;;  4) Do it!
;;  5) If you got an error, please do not close *Backtrace* buffer.
;;  6) M-x display-hatebu-send-bug-report and M-x insert-buffer *Backtrace*
;;  7) Describe the bug using a precise recipe.
;;  8) Type C-c C-c to send.
;;  # If you are a Japanese, please write in Japanese:-)

;;; History:

;; $Log: display-hatebu.el,v $
;; Revision 1.7  2010/05/04 09:07:47  rubikitch
;; Added bug report command
;;
;; Revision 1.6  2010/02/12 20:23:08  rubikitch
;; `update-display-hatebu-string': append space
;;
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

(defvar display-hatebu-version "$Id: display-hatebu.el,v 1.7 2010/05/04 09:07:47 rubikitch Exp $")
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
     (format "puts(((JSON.parse(open('%s').read)['count'] rescue '0')) << 'users ')"
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

;;;; Bug report
(defvar display-hatebu-maintainer-mail-address
  (concat "rubiki" "tch@ru" "by-lang.org"))
(defvar display-hatebu-bug-report-salutation
  "Describe bug below, using a precise recipe.

When I executed M-x ...

How to send a bug report:
  1) Be sure to use the LATEST version of display-hatebu.el.
  2) Enable debugger. M-x toggle-debug-on-error or (setq debug-on-error t)
  3) Use Lisp version instead of compiled one: (load \"display-hatebu.el\")
  4) If you got an error, please paste *Backtrace* buffer.
  5) Type C-c C-c to send.
# If you are a Japanese, please write in Japanese:-)")
(defun display-hatebu-send-bug-report ()
  (interactive)
  (reporter-submit-bug-report
   display-hatebu-maintainer-mail-address
   "display-hatebu.el"
   (apropos-internal "^display-hatebu-" 'boundp)
   nil nil
   display-hatebu-bug-report-salutation))

(provide 'display-hatebu)

;; How to save (DO NOT REMOVE!!)
;; (emacswiki-post "display-hatebu.el")
;;; display-hatebu.el ends here
