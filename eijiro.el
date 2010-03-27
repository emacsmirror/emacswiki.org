;;;; eijiro.el --- 英辞郎をanything.elを使って検索する
;; $Id: eijiro.el,v 1.3 2010/03/26 23:43:08 rubikitch Exp $

;; Copyright (C) 2010  rubikitch

;; Author: rubikitch <rubikitch@ruby-lang.org>
;; Keywords: languages, data, dictionary
;; URL: http://www.emacswiki.org/cgi-bin/wiki/download/eijiro.el

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
;; インターネットで販売されている最強の英和辞典・和英辞典である英辞郎を
;; anything.elで検索する。
;;
;; http://www.eijiro.jp/ から購入する。

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `eijiro'
;;    文字列QUERYを英辞郎検索する。日本語の場合は和英辞郎で検索する。
;;  `eijiro-at-point'
;;    カーソル位置の単語を英辞郎検索する。
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;
;;  `eijiro-directory'
;;    *英辞郎の辞書ディレクトリ
;;    default = "/home/dict/"
;;  `eijiro-display-function'
;;    *英辞郎バッファの表示方法
;;    default = (quote anything-default-display-buffer)

;;; Installation:
;; 
;; 1. http://www.emacswiki.org/cgi-bin/wiki/download/auto-install.el をインストールする
;; 2. M-x auto-install-batch anything を実行する
;; 3. 以下の設定を.emacsに加える
;; (require 'eijiro)
;; (setq eijiro-directory "/home/dict/") ; 英辞郎の辞書を置いているディレクトリ
;;

;;; Customize:
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET eijiro RET
;;


;;; History:

;; $Log: eijiro.el,v $
;; Revision 1.3  2010/03/26 23:43:08  rubikitch
;; anything-match-plugin.elを使って書き換えた。
;;
;; Revision 1.2  2010/02/01 10:34:19  rubikitch
;; anything-match-plugin-grep.rbを使って再実装。
;;
;; Revision 1.1  2010/02/01 04:31:06  rubikitch
;; Initial revision
;;

;;; Code:

(defvar eijiro-version "$Id: eijiro.el,v 1.3 2010/03/26 23:43:08 rubikitch Exp $")
(eval-when-compile (require 'cl))
(require 'anything)
(require 'anything-match-plugin)
(require 'thingatpt)
(defgroup eijiro nil
  "eijiro"
  :group 'emacs)

(defcustom eijiro-directory "/home/dict/"
  "*英辞郎の辞書ディレクトリ"
  :type 'string
  :group 'eijiro)
(defcustom eijiro-display-function 'anything-default-display-buffer
  "*英辞郎バッファの表示方法"
  :type 'symbol
  :group 'eijiro)

(defvar anything-c-source-eijiro
  '((name . "Eijiro")
    (candidates . anything-c-eijiro-candidates)
    (grep-candidates . eijiro-dictionaries-by-query)
    (requires-pattern . 2)
    (header-name . (lambda (x) (concat x ": " anything-pattern)))
    (candidate-transformer . anything-c-eijiro-candidate-transformer)
    (action ("Put into kill-ring" . kill-new))))
;; (anything 'anything-c-source-eijiro)

(defun anything-c-eijiro-candidate-transformer (c)
  (mapcar (lambda (line)
            (replace-regexp-in-string
             "【" "\n  【"
             (replace-regexp-in-string "\t・" "\n    ・"
                                       (decode-coding-string line 'sjis-dos))))
          c))

(defun eijiro-dictionaries-by-query ()
  (if (string-match "^^\\.\\.[a-zA-Z0-9]+" anything-pattern)
      (eijiro-dictionaries eijiro-directory 'eiwa)
    (eijiro-dictionaries eijiro-directory 'waei)))

(defun eijiro-dictionaries (dir type)
  (case type
    ('eiwa
     (delq nil
           (append (directory-files dir t "eiji.*.txt")
                   (directory-files dir t "otoji.*.txt")
                   (directory-files dir t "reiji.*.txt")
                   (directory-files dir t "ryaku.*.txt"))))
    ('waei (directory-files dir t "waei.*.txt"))))
;; (eijiro-dictionaries eijiro-directory 'eiwa)
;; (eijiro-dictionaries eijiro-directory 'waei)

(defun anything-c-eijiro-candidates ()
  (let* ((anything-pattern
          (concat "^.." (encode-coding-string anything-pattern 'sjis)))
         (proc (agp-candidates)))
    (set-process-coding-system proc 'sjis-dos 'utf-8)
    proc))

(defun eijiro (query)
  "文字列QUERYを英辞郎検索する。日本語の場合は和英辞郎で検索する。"
  (interactive "sEijiro: ")
  (if (string-match "^[a-zA-Z]\\{,2\\}$" query)
      (error "Too short query!")
    (let ((anything-input-idle-delay anything-idle-delay)
          (anything-enable-shortcuts)
          (anything-display-function eijiro-display-function))
      (anything-attrset 'query query anything-c-source-eijiro)
      (anything 'anything-c-source-eijiro query nil nil nil "*eijiro*"))))
(defun eijiro-at-point ()
  "カーソル位置の単語を英辞郎検索する。"
  (interactive)
  (eijiro (word-at-point)))

(provide 'eijiro)

;; How to save (DO NOT REMOVE!!)
;; (emacswiki-post "eijiro.el")
;;; eijiro.el ends here
