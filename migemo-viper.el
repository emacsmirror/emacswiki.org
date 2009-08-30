;;; migemo-viper.el --- A extension for viper 'f,F,t,T' commands by migemo.

;; Copyright (C) 2008  Ryoichi Kanetaka

;; Author: Ryoichi Kanetaka <nilburn+migemo-viper_at_gmail.com>
;; Keywords:
;; Version: 1.2

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
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

;; Description:
;;
;; vi の編集モードでは、`f,F,t,T' キーに、単一文字を検索し、その上に
;; cursor を移動する機能が割り当てられています。
;;
;; その機能を、emacs viper-mode の Vi state で、migemo を用い、日本語に
;; も使えるようにするための拡張です。
;;
;; また、
;;
;; * 削除の`d'コマンド、置換の`c'コマンドの後の範囲指定
;; * `.'キーや、`;'キー、`,'キーを用いてのリピート
;; * 数値 prefix を用いた指定回数の移動
;;
;; も、行えます。
;;
;; ※実行には migemo が必要です。
;;
;; 下記サイトより入手するか、お使いのOSに適した方法で行ってください。
;;
;; * migemo
;; http://0xcc.net/migemo/
;;
;; * cmigemo
;; http://www.kaoriya.net/#CMIGEMO

;; Installation:
;;
;; このファイルを load-path の通っている所において、
;;
;; (require 'migemo-viper)
;;
;; と、してください。
;; ~/.viper もしくは、`viper-custom-file-name' で指定したファイルの中で
;; 上記 require 式を書くと良いかもしれません。
;;
;; 特別な設定無く、そのまま使えますが、使い勝手の微調整に、いくつかパラ
;; メータを用意しています。
;;
;; 詳しくは、M-x customize-group RET migemo-viperとするか、
;; 下のＳ式を評価してください。
;; (customize-group "migemo-viper")

;; Usage:
;;
;; 例文: あいうえおかきくけこ
;;
;; 例文の上で、'fa' 'Tka'などと、入力してみてください。
;; migemo での検索結果の上に cursor が移動します。
;;
;; 簡単な動作の説明
;;
;; 1. `f,F,t,T'キーを押した後、何らかのキーを押すまでは、いつまでも待ち
;; つづけます。
;;
;; 2. 最初のキーを押下後、変数`read-string-with-timer-interval-seconds'にて
;; 指定した秒数以上、ストロークの間隔が開くと、キーの取り込みを終了し、
;; 検索を開始します。
;;
;; なので、タイピングがあまり素早くない方は、
;;
;; 1. `f,F,t,T'キーを押す
;;
;; 2. 移動したい場所の文字列を見定めてから、一息に入力
;;
;; と、心持ち、２段階に入力すると良いかもしれません。

;; Miscellaneous:
;;
;; 1. 機能のON/OFF切り替えに、関数`migemo-viper-mode'が有ります。
;;
;; 2. viper では、デフォルトの`f,F,t,T'コマンドに於いて、cursor のある
;; 行以外にも移動してしまいます。これは、オリジナルの vi と動作が異なり
;; ますが、変数`viper-allow-multiline-replace-regions'にて、変更できま
;; す。
;;
;; 3. migemo の速度が気になる方は、以下のＳ式を書くと事態が改善するかも
;; しれません。
;;
;; (setq migemo-use-pattern-alist t)
;; (setq migemo-use-frequent-pattern-alist t)
;;
;; 4. `read-string-with-timer'に、待ち時間を増加させる機能が有ります。
;;
;; migemo では、文節の切れ目に大文字を入力する事で連文節の検索ができま
;; すが、shift キーを押す時に押下間隔が長めに開いてしまい、キー入力を完
;; 了出来なかった為、キー入力に応じて、最長待ち時間間隔を引き延ばす仕組
;; みを作りました。
;;
;; 詳しくは、`read-string-with-timer-interval-seconds-increased'の、
;; document を、ご覧ください。

;; History:
;; Version 1.2 2008/09/13
;; migemo-viper を minor mode 化
;;
;; Version 1.1 2008/08/24
;; `read-string-with-timer'の、待ち時間を増加させる機能を追加
;;
;; Version 1.0 2008/05/16
;; first release.


;;; Code:

(unless (>= emacs-major-version 22)
  (error "migemo-viper can be executed with emacs that is newer than version 22."))

(eval-when-compile (require 'cl))
(require 'migemo)

(defgroup migemo-viper nil
  "A extension for viper 'f,F,t,T' commands by migemo."
  :group 'viper
  :group 'migemo)

(defcustom read-string-with-timer-interval-seconds 0.2
  "キー取り込みを終了するまでの間隔を指定する。

関数 `read-string-with-timer' にて、キーを打ち始めてから
ここで指定した秒数以上打鍵間隔が空くと、キー取り込みを終了します。

* point
 - 長く設定すると、カーソル移動を開始するまで長く待たされる。
 - 短く設定すると、意図した入力を全てタイプしきることが出来ない。

適当に調節して使ってください。"
  :type 'float
  :group 'migemo-viper)

(defcustom read-string-with-timer-interval-seconds-increased nil
  "`read-string-with-timer-interval-seconds' を、変更する仕組み。

二種類有ります。

1. 一キー押下毎に、徐々に増加する
2. 指定した回数目のキー入力で、値を変更する。

値の設定は、customize より行ってください。
"
  :type '(set
	  (list :tag "gradually"
		(const :tag "一キー押下毎に、徐々に増加する" :gradually)
		(float :format "%t: %v%d"
		       :tag "adding number"
		       :doc "一キー押下毎に、加えられる値（秒）"
		       :value 0.05)
		(float :format "%t: %v %d"
		       :tag "limit value  "
		       :doc "増加後に取りうる値の最大値（秒）"
		       :value 0.5))
	  (list :tag "at a specified number"
		(const :tag "指定した回数目のキー入力で、値を変更する。" :count)
		(integer :format "%t: %v %d"
			 :tag "what times changed at"
			 :doc "変更を行う回の指定"
			 :value 5)
		(float   :format "%t: %v %d"
			 :tag "changed value        "
			 :doc "変更後に取る値（秒）"
			 :value 0.5)))
  :group 'migemo-viper)

(defcustom migemo-viper-f-command-cursor-position nil
  "2文字以上の文字列を viper f コマンドにて検索した後の cursor の位置。"
  :tag "Migemo Viper `f' Command Cursor Position"
  :type '(choice :tag "cursor on" (const :tag "first char" nil) (const :tag "last char" t))
  :group 'migemo-viper)

(defcustom migemo-viper-F-command-cursor-position nil
  "2文字以上の文字列を viper F コマンドにて検索した後の cursor の位置。"
  :tag "Migemo Viper `F' Command Cursor Position"
  :type '(choice :tag "cursor on" (const :tag "first char" t) (const :tag "last char" nil))
  :group 'migemo-viper)

(defun read-string-with-timer ()
  "キーを打ち始めてから、一定のstroke間隔が空くまで、typingを取り込みつづける。

返り値は、string。

`read-string-with-timer-interval-seconds'に、最長のタイピングの間隔を指定する。(秒)"
  (labels((get-inc (type)
		   (member* type
			    read-string-with-timer-interval-seconds-increased
			    :key 'car)))
    (let ((interval-sec read-string-with-timer-interval-seconds)
	  (count 0)
	  sec-inc sec-limit sec-count sec-changed
	  c cs)
      (when read-string-with-timer-interval-seconds-increased
	(setq sec-inc (cadar (get-inc :gradually))
	      sec-limit (caddar (get-inc :gradually))
	      sec-count (cadar (get-inc :count))
	      sec-changed (caddar (get-inc :count))))
      ;; read-char ではなく、read-char-exclusive をつかっているのは、
      ;; viper-find-char-{forward,backward} 内で使っているread-charと
      ;; 被らないようにする為です。
      (setq c (read-char-exclusive))
      (while c
	(when (and sec-count count
		   (>= (incf count) sec-count))
	  (setq interval-sec sec-changed
		count nil))
	(setq cs (cons c cs))
	(setq c (read-char-exclusive nil nil interval-sec))
	(when (and sec-inc c)
	  (incf interval-sec sec-inc)
	  (when (> interval-sec sec-limit)
	    (setq interval-sec sec-limit
		  sec-inc nil))))
      ;; (message "%f sec" interval-sec)	; debug
      (concat (nreverse cs)))))

(defadvice viper-find-char (around migemo-viper-find-char activate compile)
  "関数内で使う`search-forward'を、`migemo-forward'で置き換える。"
  (unwind-protect
      (let ((ad-redefinition-action 'accept))
	(ad-deactivate 'search-forward)
	(flet ((search-forward
		(string &optional bound noerror count)
		(let ((com (if (eq viper-intermediate-command 'viper-repeat)
			       (nth 5 viper-d-com)
			     (viper-array-to-string (this-command-keys))))
		      (adjust-position t))

		  (when (and (string-match "^[[:digit:]]*\\(c\\|d\\)?[[:digit:]]*\\(f\\|F\\|t\\|T\\)\\(.*\\)" com)
			     (let ((m1 (match-string-no-properties 1 com))
				   (m2 (match-string-no-properties 2 com)))
			       (or (and (string-equal "f" m2)
					(or migemo-viper-f-command-cursor-position
					    (string-equal "d" m1)
					    (string-equal "c" m1)))
				   (and (string-equal "F" m2)
					(or migemo-viper-F-command-cursor-position
					    (string-equal "d" m1)
					    (string-equal "c" m1))))))
		    (setq adjust-position nil))

		  (if (eq viper-intermediate-command 'viper-repeat)
		      (setq string (match-string 3 com))
		    (setq viper-this-command-keys
			  (concat com (substring-no-properties string 1))))

		  (let ((result (funcall 'migemo-forward string bound noerror count)))
		    (when (and adjust-position result)
		      (funcall (if (plusp count) 'backward-char 'forward-char)
			       (1- (length (match-string-no-properties 0)))))
		    result)))

	       (char-to-string (args)
			       (if (stringp args)
				   args
				 (concat (list args)))))
	  (condition-case err
	      ad-do-it
	    (error (if (string= (error-message-string err)
				"Format specifier doesn't match argument type")
		       (error "Command `%s':  `%s' not found"
			      (if (eq viper-intermediate-command 'viper-repeat)
				  (nth 5 viper-d-com)
				(let ((tck (viper-array-to-string (this-command-keys))))
				  (if (string-match "[0-9]*;" tck)
				      tck
				    (concat tck (substring-no-properties (ad-get-arg 1) 1)))))
			      (ad-get-arg 1))
		     (error (error-message-string err)))))))
    (ad-activate 'search-forward)))

(defadvice viper-find-char-forward
  (around migemo-viper-find-char-forward activate)
  "関数内で使う`read-char'を、`read-string-with-timer'で置き換える。"
  (letf (((symbol-function 'read-char) (symbol-function 'read-string-with-timer)))
    ad-do-it))

(defadvice viper-find-char-backward
  (around migemo-viper-find-char-backward activate)
  "関数内で使う`read-char'を、`read-string-with-timer'で置き換える。"
  (letf (((symbol-function 'read-char) (symbol-function 'read-string-with-timer)))
    ad-do-it))

(defadvice viper-goto-char-forward
  (around migemo-viper-goto-char-forward activate)
  "関数内で使う`read-char'を、`read-string-with-timer'で置き換える。"
  (letf (((symbol-function 'read-char) (symbol-function 'read-string-with-timer)))
    ad-do-it))

(defadvice viper-goto-char-backward
  (around migemo-viper-goto-char-backward activate)
  "関数内で使う`read-char'を、`read-string-with-timer'で置き換える。"
  (letf (((symbol-function 'read-char) (symbol-function 'read-string-with-timer)))
    ad-do-it))

;;;###autoload
(define-minor-mode migemo-viper-mode
  "toggle migemo-viper advices."
  :group 'migemo-viper
  :global t
  :init-value t
  (labels ((toggle-advice (str)
			  (let ((func (intern str))
				(adv (intern (concat "migemo-" str))))
			    (if migemo-viper-mode
				(ad-enable-advice func 'around adv)
			      (ad-disable-advice func 'around adv))
			    (ad-activate func))))
    (mapcar 'toggle-advice
	    '("viper-find-char-forward"
	      "viper-find-char-backward"
	      "viper-goto-char-forward"
	      "viper-goto-char-backward"
	      "viper-find-char"))))

(provide 'migemo-viper)
;;; migemo-viper.el ends here
