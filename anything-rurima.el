;;;; anything-rurima.el --- Look up Japanese Ruby Reference Manual with anything.el

;; Copyright (C) 2009  rubikitch

;; Author: rubikitch <rubikitch@ruby-lang.org>
;; Keywords: convenience, languages
;; URL: http://www.emacswiki.org/cgi-bin/wiki/download/anything-rurima.el

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
;; Rubyリファレンスマニュアル刷新計画(るりま)をanythingで検索します。
;;
;; (1) るりまのリポジトリをチェックアウトする。~/compile/rurima以下に展開するものとする。
;;     $ cd ~/compile; mkdir rurima; cd rurima
;;     $ svn co http://jp.rubyist.net/svn/rurema/doctree/trunk rubydoc
;; (2) http://www.rubyist.net/~rubikitch/archive/ar-index.rb から
;;     ダウンロードして、以下のコマンドでドキュメントのインデックスを作成する。
;;     Ruby 1.9必須！
;;     ~/compile/rurima/rubydoc/rurima.eというファイルが作成される。
;;     $ ruby1.9 ar-index.rb ~/compile/rurima/rubydoc rurima.e
;; (3) http://www.emacswiki.org/cgi-bin/wiki/download/auto-install.elをインストールする。
;; (4) auto-install.elの設定を加える。
;;      (require 'auto-install)
;;      (setq auto-install-directory "~/.emacs.d/auto-install/")
;;      (auto-install-update-emacswiki-package-name t)
;;      (auto-install-compatibility-setup)
;; (5) M-x auto-install-batch anything を実行して anything 一式をインストールする。
;; (6) anything-rurima.elの設定を加える。
;;      (require 'anything-rurima)
;;      (setq anything-rurima-index-file "~/compile/rurima/rubydoc/rurima.e")
;;
;; M-x anything-rurima でマニュアルを検索するためのプロンプトが出ます。
;; M-x anything-rurima-at-point でカーソル位置の単語をマニュアル検索します。
;;      
;;; Commands:
;;
;; Below are complete command list:
;;
;;  `anything-rurima'
;;    るりまを検索する。
;;  `anything-rurima-at-point'
;;    カーソル位置の単語をるりまで調べる。
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;
;;  `anything-rurima-index-file'
;;    *るりまRDの目次ファイル。ドキュメントディレクトリに置く。
;;    default = "~/compile/rurima/rubydoc/rurima.e"

;;; Customize:
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET anything-rurima RET
;;


;;; Code:

(require 'anything-config)
(require 'anything-match-plugin)
(defvar anything-rurima-version "$Id: 50autoinsert.el,v 1.19 2009/09/15 10:29:59 rubikitch Exp rubikitch $")
(eval-when-compile (require 'cl))
(defgroup anything-rurima nil
  "anything-rurima"
  :group 'emacs)

(defcustom anything-rurima-index-file "~/compile/rurima/rubydoc/rurima.e"
  "*るりまRDの目次ファイル。ドキュメントディレクトリに置く。"
  :type 'string  
  :group 'anything-rurima)

(defun find-rurima (&rest args)
  (apply 'ar/find-file
         (append (butlast args)
                 (list (expand-file-name
                        (car (last args))
                        (file-name-directory anything-rurima-index-file))))))

(defun ar/find-file (&rest args)
  (let ((pos-spec-list-reversed (butlast args 1))
        (filename (car (last args))))
    (find-file filename)
    (apply 'ar/goto-position (reverse pos-spec-list-reversed))
    (recenter 0)))
;; (setq anything-rurima-document-directory "/log/compile/rurima/rubydoc/")
;; (find-rurima  "--- try_convert(obj) -> Array | nil" "== Class Methods" "= class Array < Object"  "refm/api/src/_builtin/Array")

;;; Borrowed from eev.el
(defun ar/goto-position (&optional pos-spec &rest rest)
  (when pos-spec
    (cond ((numberp pos-spec)
	   (goto-char (point-min))
	   (forward-line (1- pos-spec)))
	  ((stringp pos-spec)
	   (goto-char (save-excursion	          ; This used to be just:
			(goto-char (point-min))	  ; (goto-char (point-min))
			(search-forward pos-spec) ; (search-forward pos-spec)
			(point))))		  ;
	  (t (error "This is not a valid pos-spec: %S" pos-spec)))
    (if rest (ar/goto-rest rest))))

(defun ar/goto-rest (list)
  (cond ((null list))
	((stringp (car list))
	 (search-forward (car list))
	 (ar/goto-rest (cdr list)))
	((numberp (car list))
	 (forward-line (car list))
	 (ar/goto-rest (cdr list)))
	((consp (car list))
	 (eval (car list))
	 (ar/goto-rest (cdr list)))
	(t (error "Not a valid pos-spec item: %S" (car list)))))

(defvar anything-c-source-rurima
  '((name . "るりま")
    (candidates-file . anything-rurima-index-file)
    (type . sexp)
    (migemo)))
;; (anything 'anything-c-source-rurima)

(defun anything-rurima (&optional pattern)
  "るりまを検索する。"
  (interactive)
  (anything 'anything-c-source-rurima pattern nil nil nil "*rurima*"))

(defun anything-rurima-at-point ()
  "カーソル位置の単語をるりまで調べる。"
  (interactive)
  (anything-rurima (concat "---  " (thing-at-point 'symbol))))

(provide 'anything-rurima)

;; How to save (DO NOT REMOVE!!)
;; (emacswiki-post "anything-rurima.el")
;;; anything-rurima.el ends here
