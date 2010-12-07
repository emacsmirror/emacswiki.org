;;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;;; menu-tree.el --- Localize menu-bar items

;; Copyright (C) 2008, 2009, 2010 IRIE Shinsuke

;; Original Author: YAGI Tatsuya
;; Author: IRIE Shinsuke
;; Maintainer: IRIE Shinsuke
;; Keywords: Localization, Japanese, menu-bar

(defconst menu-tree-version "0.97"
  "Version number of the menu-tree package.")

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Loading menu-tree.el replaces the texts of menu-bar items
;; with the various languages' translations. By default, Japanese
;; translation data are applied.
;;
;; This program, originally written by YAGI Tatsuya in 1999
;; (http://www.ysnb.net/meadow/meadow-users-jp/1999/msg01278.html),
;; has been rewritten completely by current maintainer, and now
;; can be used for GNU Emacs 22 and 23. (Emacs 21 or earlier is no
;; longer supported.)

;;
;; Installation:
;;
;; Save this file as menu-tree.el in a directory listed in
;; load-path, and put the following into your .emacs file:
;;
;;   (require 'menu-tree)
;;
;; If many unreadable characters appear in menu-bar, set
;; `menu-tree-coding-system' the appropriate coding system
;; before loading this program. For example,
;;
;;   (if (and (= emacs-major-version 22)
;;            (eq window-system 'x))
;;       (setq menu-tree-coding-system 'utf-8))
;;   (require 'menu-tree)
;;

;;; History:
;;  2010-11-30  S. Irie
;;          * Version 0.97
;;          * Fixed bug: Loading CEDET causes errors for undefined keymaps
;;
;;  2010-08-06  S. Irie
;;          * Version 0.96
;;          * Fixed bug: `rmail' and `compose-mail' are not updated dynamically
;;
;;  2010-06-30  S. Irie
;;          * Version 0.95
;;          * Updated for Emacs 23.2
;;          * Added functions `menu-tree-convert', `menu-tree-get', `menu-tree-pp'
;;          * Modified Japanese translations
;;
;;  2010-04-20  S. Irie
;;          * Version 0.94
;;          * Changed license to GPL3
;;          * Rewrote all functions
;;          * Made translations for calendar-mode more configurable
;;          * Added/modified Japanese translations
;;          * Fixed bug: Can't override ispell menu in Emacs 23
;;          * Changed file encoding to UTF-8
;;
;;  2008-11-09  S. Irie
;;          * Version 0.93
;;          * Added Japanese translations
;;          * Fixed bug: An error occurs in a certain revision of Emacs 23
;;
;;  2008-05-15  S. Irie
;;          * Version 0.92
;;          * Modified Japanese translations
;;
;;  2008-05-08  S. Irie
;;          * Version 0.91
;;          * Chanded to use different `menu-tree-alist' according to Emacs version
;;
;;  2008-03-26  S. Irie
;;          * Version 0.90
;;          * Supported GNU Emacs 22,23

;; ToDo:

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Setting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar menu-tree-coding-system nil
  "Coding system used for menu-bar items.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Translation data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar menu-tree-alist-ja-langs
  '((Default "標準")
    (Chinese "中国語"
	     (Chinese-GB "中国語(GB)")
	     (Chinese-BIG5 "中国語(BIG5)")
	     (Chinese-CNS "中国語(CNS)")
	     (Chinese-EUC-TW "中国語(EUC-TW)")
	     (Chinese-GBK "中国語(GBK)")
	     (Chinese-GB18030 "中国語(GB18030)")
	     )
    (Cyrillic "キリル文字"
	      (Cyrillic-ISO "キリル文字(ISO)")
	      (Cyrillic-KOI8 "キリル文字(KOI8)")
	      (Russian "ロシア語")
	      (Cyrillic-ALT "キリル文字(ALT)")
	      (Tajik "タジク語")
	      (Bulgarian "ブルガリア語")
	      (Belarusian "ベラルーシ語")
	      (Ukrainian "ウクライナ語")
	      )
    (Indian "インドの言語"
	    (Devanagari "デーヴァナーガリー文字")
	    (Bengali "ベンガル語")
	    (Punjabi "パンジャーブ語")
	    (Gujarati "グジャラート語")
	    (Oriya "オリヤー語")
	    (Tamil "タミル語")
	    (Telugu "テルグ語")
	    (Kannada "カンナダ語")
	    (Malayalam "マラヤーラム語")
	    )
    (Sinhala "シンハラ語")
    (English "英語")
    (ASCII "ASCII")
    (Ethiopic "エチオピア語")
    (European "ヨーロッパ系言語"
	      (Latin-1 "ラテン系言語(1)")
	      (Latin-2 "ラテン系言語(2)")
	      (Latin-3 "ラテン系言語(3)")
	      (Latin-4 "ラテン系言語(4)")
	      (Latin-5 "ラテン系言語(5)")
	      (Latin-8 "ラテン系言語(8)")
	      (Latin-9 "ラテン系言語(9)")
	      (Esperanto "エスペラント")
	      (Dutch "オランダ語")
	      (German "ドイツ語")
	      (French "フランス語")
	      (Italian "イタリア語")
	      (Slovenian "スロべニア語")
	      (Spanish "スペイン語")
	      (Polish "ポーランド語")
	      (Welsh "ウェールズ語")
	      (Latin-6 "ラテン系言語(6)")
	      (Latin-7 "ラテン系言語(7)")
	      (Lithuanian "リトアニア語")
	      (Latvian "ラトビア語")
	      (Swedish "スウェーデン語")
	      (Croatian "クロアチア語")
	      (Brazilian\ Portuguese "ポルトガル語(ブラジル)")
	      (Czech "チェコ語")
	      (Slovak "スロバキア語")
	      (Romanian "ルーマニア語")
	      (Georgian "グルジア語")
	      )
    (Turkish "トルコ語")
    (Greek "ギリシャ語")
    (Hebrew "ヘブライ語")
    (Japanese "日本語")
    (Korean "韓国語")
    (Lao "ラオス語")
    (TaiViet "タイ系言語(TaiViet)")
    (Thai "タイ語")
    (Tibetan "チベット語")
    (Vietnamese "ベトナム語")
    (IPA "国際発音記号")
    (UTF-8 "UTF-8")
    (Khmer "クメール語")
    (Burmese "ビルマ語")
    (Cham "チャム語")
    )
  "Japanese translation data for language names sub menu.
Used as the sub-tree of the default value of `menu-tree-alist-ja'.")

(defvar menu-tree-alist-ja
  `((global-map
     (buffer "バッファ"
	     ;; ...
;	     (frames-separator "--")
	     (frames "フレーム")
;	     (command-separator "--")
	     (next-buffer "次のバッファ")
	     (previous-buffer "前のバッファ")
	     (select-named-buffer "バッファ名を指定...")
	     (list-all-buffers "バッファ一覧")
	     )
     (file "ファイル"
	    (new-file "新規...")
	    (open-file "開く...")
	    (dired "ディレクトリを開く...")
	    (insert-file "ファイルを挿入...")
	    (kill-buffer "閉じる")
;	    (separator-save "--")
	    (save-buffer "保存")
	    (write-file "名前をつけて保存...")
	    (revert-buffer "再読み込み")
	    (recover-session "セッションを復旧")
;	    (separator-print "--")
	    (print-buffer "印刷")
	    (print-region "選択領域を印刷")
	    (ps-print-buffer-faces "Postscriptで印刷")
	    (ps-print-region-faces "Postscriptで選択領域を印刷")
	    (ps-print-buffer "Postscriptで印刷(白黒)")
	    (ps-print-region "Postscriptで選択領域の印刷(白黒)")
;	    (separator-window "--")
	    (split-window "ウィンドウを分割")
	    (one-window "分割を解除")
	    (make-frame "新規フレーム")
	    (make-frame-on-display "新規フレーム(画面を指定)...")
	    (delete-this-frame "フレームを閉じる")
;	    (separator-exit "--")
	    (exit-emacs "終了")
	    )
     (tools "ツール"
	    (grep "ファイル内を検索(Grep)...")
	    (compile "コンパイル...")
	    (shell "シェルコマンド...")
	    (shell-on-region "選択領域をシェルコマンドで処理...")
	    (gdb "デバッガ(GDB)")
	    ,@(if (= emacs-major-version 23) ; >= 23.2
		  '((ede "プロジェクト支援 (EDE)")
		    (semantic "ソースコード構文解析 (Semantic)")))
;	    (separator-prog "--")
 	    (spell "スペルチェック"
		   (ispell-buffer "バッファをチェック")
		   (ispell-message "メールとニュースをチェック")
		   (ispell-region "選択領域をチェック")
		   (ispell-comments-and-strings "コメントと文字列をチェック")
		   (ispell-word "単語をチェック")
		   (ispell-continue "スペルチェックを再開")
		   (ispell-complete-word-interior-frag "単語の断片を補完")
		   (ispell-complete-word "単語を補完")
		   (flyspell-mode "自動でスペルチェック(Flyspell)")
		   (ispell-help "ヘルプ")
		   (ispell-customize "設定...")
		   (ispell-pdict-save "辞書を保存")
		   (ispell-kill-ispell "強制終了")
		   (ispell-change-dictionary "辞書を変更...")
		   (american "アメリカ英語の辞書を選択")
		   (british "イギリス英語の辞書を選択")
		   (canadian "カナダ英語の辞書を選択")
		   (english "英語の辞書を選択")
		   )
;	    (separator-spell "--")
 	    (compare "比較(Ediff)"
		     (ediff-files "2つのファイル...")
		     (ediff-buffers "2つのバッファ...")
		     (ediff-files3 "3つのファイル...")
		     (ediff-buffers3 "3つのバッファ...")
;		     (separator-ediff-files "--")
		     (ediff-directories "2つのディレクトリ...")
		     (ediff-directories3 "3つのディレクトリ...")
;		     (separator-ediff-directories "--")
		     (ediff-revision "ファイルのバージョン間...")
		     (ediff-dir-revision "ディレクトリのバージョン間...")
;		     (separator-ediff-regions "--")
		     (ediff-regions-wordwise "選択領域間(単語ごと)...")
		     (ediff-regions-linewise "選択領域間(行ごと)...")
;		     (separator-ediff-windows "--")
		     (ediff-windows-wordwise "ウィンドウ間(単語ごと)...")
		     (ediff-windows-linewise "ウィンドウ間(行ごと)...")
		     (window "このウィンドウと隣のウィンドウ")
;		     (separator-ediff-misc "--")
		     ,@(if (= emacs-major-version 23) ; >= 23.2
			   '((ediff-misc "Ediff その他いろいろ"
					 (ediff-doc "Ediffマニュアル")
					 (ediff-cust "Ediffをカスタマイズ")
					 (eregistry "Ediffのセッション一覧")
					 (emultiframe "コントロールバッファフレームの使用/不使用")
					 )))
		     )
	    (ediff-merge "マージ"
			 (ediff-merge-files "ファイル...")
			 (ediff-merge-files-with-ancestor "ファイル(祖先を参照)...")
			 (ediff-merge-buffers "バッファ...")
			 (ediff-merge-buffers-with-ancestor "バッファ(祖先を参照)...")
;			 (separator-ediff-merge-dirs "--")
			 (ediff-merge-directories "ディレクトリ...")
			 (ediff-merge-directories-with-ancestor "ディレクトリ(祖先を参照)...")
;			 (separator-ediff-merge "--")
			 (ediff-merge-revisions "バージョン間...")
			 (ediff-merge-revisions-with-ancestor "バージョン間(祖先を参照)...")
			 (ediff-merge-dir-revisions "ディレクトリのバージョン間...")
			 (ediff-merge-dir-revisions-with-ancestor "ディレクトリのバージョン間(祖先を参照)...")
			 )
	    (epatch "パッチを適用"
		    (ediff-patch-file "ファイルに対して...")
		    (ediff-patch-buffer "バッファに対して...")
		    )
	    (ediff-misc "Ediff その他いろいろ"
			(ediff-doc "Ediffマニュアル")
			(ediff-cust "Ediffをカスタマイズ")
			(eregistry "Ediffのセッション一覧")
			(emultiframe "コントロールバッファフレームの使用/不使用")
			)
;	    (separator-compare "--")
	    (vc "バージョン管理"
		,@(if (= emacs-major-version 23)
		      '((vc-dir "VCディレクトリ")))
		(vc-register "登録")
		(vc-next-action "チェックイン/アウト")
		(vc-update "最新バージョンに更新")
		,@(if (= emacs-major-version 23)
		      '((vc-revert "変更を破棄して元に戻す")))
		,@(if (= emacs-major-version 22)
		      '((vc-revert-buffer "変更を破棄して元に戻す")))
		(undo "最後のチェックインを取り消す")
		(vc-insert-header "ヘッダを挿入")
;		(separator2 "----")
		,@(if (= emacs-major-version 23) ; >= 23.2
		      '((vc-print-root-log "ツリーの履歴を表示")))
		(vc-print-log "履歴を表示")
		(vc-update-change-log "履歴を更新")
		,@(if (= emacs-major-version 23) ; >= 23.2
		      '((vc-root-diff "ツリーを元のバージョンと比較")))
		(vc-diff "元のバージョンと比較")
		,@(if (= emacs-major-version 23)
		      '((vc-revision-other-window "他のバージョンを見る")))
		,@(if (= emacs-major-version 22)
		      '((vc-version-other-window "他のバージョンを見る")))
		(vc-rename-file "ファイル名を変更")
		(vc-annotate "変更時期で色分け(Annotate)")
;		(separator1 "----")
		(vc-directory "VCディレクトリの一覧")
		(vc-create-snapshot "スナップショットを作成")
		(vc-retrieve-snapshot "スナップショットを取り出す")
		,@(if (= emacs-major-version 23)
		      '((vc-create-tag "タグを付ける")
			(vc-retrieve-tag "タグ付けされた物を取り出す")))
		)
	    (pcl-cvs "PCL-CVS"
		(examine "ディレクトリを調査")
		(update "ディレクトリを更新")
		(checkout "モジュールをチェックアウト")
		(status "ディレクトリのステータス")
		)
;	    (separator-vc "--")
	    (gnus "ネットニュースを読む(Gnus)")
	    (rmail "メールを読む(%sを使用)")
	    (compose-mail "メールを送信(%sを使用)")
	    (directory-search "ディレクトリ検索"
		(load "サーバのホットリストを取得")
		(new "新規サーバ")
;		(separator-eudc-query "--")
		(query "フォームで問い合わせ")
		(expand-inline "インラインクエリを展開")
;		(separator-eudc-email "--")
		(email "電子メールアドレスを取得")
		(phone "電話番号を取得")
		(Server "サーバ"
;			(--- "---")
			(Bookmark\ Current\ Server "現在のサーバをブックマーク")
			(Edit\ Server\ List "サーバのリストを編集")
			(New\ Server "新規サーバ")
			)
;		(--- "---")
		(Query\ with\ Form "フォームで問い合わせ")
		(Expand\ Inline\ Query "インラインクエリを展開")
		(Insert\ Record\ into\ BBDB "BBDBへレコードを挿入")
		(Insert\ All\ Records\ into\ BBDB "BBDBへ全てのレコードを挿入")
;		(--- "---")
		(Get\ Email "電子メールアドレスを取得")
		(Get\ Phone "電話番号を取得")
		(List\ Valid\ Attribute\ Names "有効な属性名の一覧")
;		(--- "---")
		(Customize "カスタマイズ"
			   (Eudc\.\.\. "Eudc...")
;			   (nil "--")
			   (Eudc\ Ph "Eudc Ph")
			   (Eudc\ Ldap "Eudc Ldap")
			   (Eudc\ Bbdb "Eudc Bbdb")
			   (Eudc\ Server\.\.\. "サーバ...")
			   (Eudc\ Protocol\.\.\. "プロトコル...")
			   (Eudc\ Strict\ Return\ Matches\.\.\. "応答のマッチを厳密にする...")
			   (Eudc\ Default\ Return\ Attributes\.\.\. "応答の属性のデフォルト...")
			   (Eudc\ Multiple\ Match\ Handling\ Method\.\.\. "複数マッチした場合の処理方法...")
			   (Eudc\ Duplicate\ Attribute\ Handling\ Method\.\.\. "重複する属性を含む場合の処理方法...")
			   (Format\ of\ Inline\ Expansion\ Queries\.\.\. "インライン展開クエリのフォーム...")
			   (Eudc\ Expansion\ Overwrites\ Query\.\.\. "クエリの文字列を上書きして展開...")
			   (Eudc\ Inline\ Expansion\ Format\.\.\. "インライン展開の書式...")
			   (Eudc\ Inline\ Expansion\ Servers\.\.\. "インライン展開サーバ...")
			   (Max\ Number\ of\ Servers\ to\ Query\.\.\. "問い合わせるサーバの最大数...")
			   (Attributes\ in\ Query\ Forms\.\.\. "クエリフォームの属性...")
			   (User-defined\ Names\ of\ Directory\ Attributes\.\.\. "ディレクトリ属性のユーザー定義名...")
			   (Eudc\ Use\ Raw\ Directory\ Names\.\.\. "生のディレクトリ属性名を使用...")
			   (Attribute\ Decoding\ Functions\.\.\. "属性値をデコードする関数...")
			   (External\ Viewer\ Programs\.\.\. "外部ビューアプログラム...")
			   (Eudc\ Options\ File\.\.\. "サーバーホットリストの保存ファイル...")
			   (Eudc\ Mode\ Hook\.\.\. "Eudcモードフック...")
		))
;	    (separator-net "--")
	    (calendar "カレンダー")
	    (calc "数式処理")
	    (simple-calculator "電卓")
;	    (separator-encryption-decryption "--")
	    (encryption-decryption "暗号化/復号化"
				   (decrypt-file "ファイルを復号化...")
				   (encrypt-file "ファイルを暗号化...")
				   (verify-file "ファイルを検証...")
				   (sign-file "ファイルに署名...")
				   (separator-file "--")
				   (decrypt-region "範囲を復号化")
				   (encrypt-region "範囲を暗号化")
				   (verify-region "範囲を検証")
				   (sign-region "範囲に署名")
				   (separator-keys "--")
				   (list-keys "鍵の一覧")
				   (import-keys "ファイルから鍵をインポート...")
				   (import-keys-region "範囲から鍵をインポート")
				   (export-keys "鍵をエクスポート")
				   (insert-keys "鍵を挿入")
				   )
;	    (separator-games "--")
	    (games "ゲーム"
	        (5x5 "5x5")
		(adventure "アドベンチャー")
		(black-box "ブラックボックス")
		(bubbles "バブルズ")
		(gomoku "五目並べ")
		(hanoi "ハノイの塔")
		(life "ライフゲーム")
		(mult "掛け算パズル")
		(pong "ピンポン")
		(snake "スネーク")
		(solitaire "ソリティア")
		(tetris "テトリス")
		(zone "ゾーンアウト")
		)
	    )
     (edit "編集"
	   (undo "元に戻す")
	   (redo "やり直し")
	   (cut "切り取り")
	   (copy "コピー")
	   (paste "貼り付け")
	   (paste-from-menu "貼り付け(キルメニューから)"
			 ;; ...
			 )
	   (clear "削除")
	   (mark-whole-buffer "全て選択")
;	   (separator-edit "--")
	   (search "検索"
		   (search-forward "下へ...")
		   (search-backward "上へ...")
		   (re-search-forward "下へ(正規表現)...")
		   (re-search-backward "上へ(正規表現)...")
;		   (separator-repeat-search "--")
		   (repeat-search-fwd "次を検索")
		   (repeat-search-back "前を検索")
;		   (separator-tag-search "--")
		   (tags-srch "タグ付けしたファイルで検索...")
		   (tags-continue "タグ検索の繰り返し")
;		   (separator-tag-isearch"--")
		   (i-search "インクリメンタルサーチ"
			     (isearch-forward "下へ...")
			     (isearch-backward "上へ...")
			     (isearch-forward-regexp "下へ(正規表現)...")
			     (isearch-backward-regexp "上へ(正規表現)...")
			     ))
	   (replace "置換"
		    (query-replace "置換...")
		    (query-replace-regexp "置換(正規表現)...")
;		    (separator-replace-tags "--")
		    (tags-repl "タグ付けしたファイルで置換...")
		    (tags-repl-continue "タグ置換の繰り返し")
		    )
	   (goto "ジャンプ"
		 (go-to-line "指定行にジャンプ...")
		 (go-to-pos "バッファの指定位置にジャンプ...")
		 (beg-of-buf "バッファの先頭にジャンプ")
		 (end-of-buf "バッファの末尾にジャンプ")
;		 (separator-tags "--")
		 (find-tag "タグを検索...")
		 (find-tag-otherw "他のウィンドウでタグを検索...")
		 (next-tag "次のタグを検索")
		 (next-tag-otherw "他のウィンドウで次のタグを検索")
		 (apropos-tags "タグを検索(正規表現)...")
;		 (separator-tag-file "--")
		 (set-tags-name "タグファイル名の設定...")
		 )
	   (bookmark "ブックマーク"
		     (jump "ブックマークへジャンプ...")
		     (set "ブックマークを設定...")
		     (insert "内容を挿入...")
		     (locate "場所を挿入...")
		     (rename "ブックマーク名を変更...")
		     (delete "ブックマークを削除...")
		     (edit "ブックマークリストを編集")
		     (save "ブックマークを保存")
		     (write "名前をつけて保存...")
		     (load "ブックマークファイルを読み込む..."))
;	   (separator-bookmark "--")
	   (fill "行の長さを揃える")
	   (props "書式"
		  (fc "書体"
		      (100 "標準")
		      (98 "太字")
		      (105 "斜体")
		      (108 "太字斜体")
		      (117 "下線付き")
		      (111 "その他..."))
		  (fg "描画色"
		      (111 "その他..."))
		  (bg "背景色"
		      (111 "その他..."))
		  (sp "特殊属性"
		      (114 "書込み禁止")
		      (118 "不可視")
		      (116 "不可触")
		      (115 "特殊属性の取り消し"))
;		  (s2 "--")
		  (ju "整列"
		      (117 "なし")
		      (108 "左寄せ")
		      (114 "右寄せ")
		      (98 "幅一杯")
		      (99 "中寄せ"))
		  (in "インデント"
		      (increase-left-margin "増やす")
		      (decrease-left-margin "減らす")
		      (increase-right-margin "増やす(右)")
		      (decrease-right-margin "減らす(右)"))
;		  (s1 "--")
		  (rm "フェイス(文字の属性)を標準に戻す")
		  (ra "全て標準に戻す")
		  (dp "属性の状態を見る")
		  (df "フェイスの一覧を見る")
		  (dc "色の一覧を見る"))
	   )
     (options "オプション"
	      (transient-mark-mode "選択領域の強調表示")
	      (highlight-paren-mode "一致する括弧を強調表示(括弧表示モード)")
;	      (highlight-separator "--")
	      ,@(if (= emacs-major-version 23)
		    '((line-wrapping "行の折り返し"
				     (window-wrap "ウィンドウの端で折り返す")
				     (truncate "長い行を切り詰めて表示")
				     (word-wrap "ワードラップ(Visual line mode)")
				     )))
	      (truncate-lines "長い行を折り返さずに表示")
	      (auto-fill-mode "テキストモードで自動折り返し")
	      (case-fold-search "検索で大文字・小文字を区別しない")
	      (cua-emulation-mode "Shift+カーソルキーで範囲選択(CUA)")
	      (cua-mode "C-x/C-c/C-vでカット＆ペースト(CUA)")
;	      (edit-options-separator "--")
	      (uniquify "バッファ名にディレクトリ名を含める")
	      (save-place "セッションの状態を保存")
;	      (cursor-separator "--")
	      (blink-cursor-mode "点滅するカーソル")
;	      (debugger-separator "--")
	      (debug-on-error "エラー発生時にデバッガへ移る")
	      (debug-on-quit "中断(C-g)時にデバッガへ移る")
;	      (mule-separator "--")
	      (mule "言語"
		    ,(append '(set-language-environment "言語環境の設定") menu-tree-alist-ja-langs)
;		    (separator-mule "--")
		    (toggle-input-method "インプットメソッドのオン/オフ")
		    (set-input-method "インプットメソッドを選択...")
;		    (separator-input-method "--")
		    (set-various-coding-system "文字コードを設定"
					       (universal-coding-system-argument "次に実行するコマンド")
;					       (separator-1 "--")
					       (set-buffer-file-coding-system "現在のバッファ")
					       (revert-buffer-with-coding-system  "このファイルを再読み込み")
					       (set-file-name-coding-system "ファイル名")
					       (set-keyboard-coding-system "キーボード")
					       (set-terminal-coding-system "ターミナル")
					       (set-selection-coding-system "ウィンドウシステムのクリップボード")
					       (set-next-selection-coding-system "次のクリップボード操作")
					       (set-buffer-process-coding-system "サブプロセスに対する入出力")
					       )
;		    (separator-coding-system "--")
		    (view-hello-file "多言語を含む文書の例")
		    ,(append '(describe-language-environment "言語環境の説明") menu-tree-alist-ja-langs)
		    (describe-input-method "インプットメソッドの説明...")
		    (describe-coding-system "文字コードの説明...")
		    (list-character-sets "キャラクタセットの一覧")
		    (mule-diag "多言語化機能の状態を表示"))
;	      (showhide-separator "--")
	      (showhide "表示/非表示"
			(showhide-tool-bar "ツールバー")
			(menu-bar-mode "メニューバー")
			(showhide-tooltip-mode "ツールチップ")
			(showhide-scroll-bar "スクロールバー"
					     (none "なし")
					     (left "左側")
					     (right "右側")
					     )
			(showhide-fringe "フリンジ"
					 (none "なし")
					 (left "左側")
					 (right "右側")
					 (default "標準")
					 (customize "フリンジのカスタマイズ")
					 (indicate-empty-lines "空行のインジケータ")
					 (showhide-fringe-ind "バッファの境界"
							      (none "インジケータなし")
							      (left "左側")
							      (right "右側")
							      (box "向かい側(矢印なし)")
							      (mixed "向かい側(右側に矢印)")
							      (customize "その他(カスタマイズ)")
							      ))
			(showhide-speedbar "スピードバー")
;			(datetime-separator "--")
			(showhide-date-time "時刻・負荷・メール")
			(showhide-battery "バッテリーの状態")
;			(linecolumn-separator "--")
			(size-indication-mode "バッファサイズ")
			(line-number-mode "行番号")
			(column-number-mode "桁番号")
			)
	      ,@(if (= emacs-major-version 23)
		    '((menu-set-font "標準のフォントを設定...")))
	      (mouse-set-font "フォント/フォントセット...")
;	      (custom-separator "--")
	      (save "オプションの保存")
	      (customize "カスタマイズ"
			 (customize "トップレベルグループ")
			 (customize-browse "グループの階層表示")
;			 (separator-3 "--")
			 (customize-saved "保存された古いオプション")
			 (customize-changed-options "新設のオプション...")
			 (customize-option "指定したオプション...")
			 (customize-face "指定したフェイス...")
			 (customize-group "指定したグループ...")
;			 (separator-2 "--")
			 (customize-apropos "正規表現で検索...")
			 (customize-apropos-options "正規表現で検索(オプション)...")
			 (customize-apropos-faces "正規表現で検索(フェイス)...")
			 (customize-apropos-groups "正規表現で検索(グループ)..."))
	      )
     (help-menu "ヘルプ"
		(emacs-tutorial "Emacsチュートリアル")
		(emacs-tutorial-language-specific "Emacsチュートリアル(言語を選択)...")
		(emacs-faq "Emacsのよくある質問とその回答")
		(emacs-news "Emacsニュース")
		(emacs-known-problems "Emacsの既知の問題")
		(send-emacs-bug-report "バグレポートを送る...")
		(emacs-psychotherapist "Emacsの精神科医")
		,@(if (= emacs-major-version 22)
		      '((emacs-problems "Emacsの既知の問題")
			(report-emacs-bug "バグレポートを送る...")
			(eliza "Emacsの精神科医")))
;		(sep1 "--")
		(debian-emacs-readme "Debian README")
		(debian-emacs-news "Debianニュース")
		(debian-emacs-changelog "Debian変更履歴")
;		(sep2 "--")
		(search-documentation "文書を検索"
				      (emacs-terminology "Emacsに関する用語")
				      (lookup-subject-in-emacs-manual "ユーザーマニュアルで検索...")
				      (lookup-subject-in-elisp-manual "ELispマニュアルで検索...")
				      (lookup-key-in-manual "ユーザーマニュアルでキーバインドを検索...")
				      (lookup-command-in-manual "ユーザーマニュアルでコマンドを検索...")
;				      (sep1 "--")
				      (find-commands-by-name "コマンド名で調べる...")
				      (find-options-by-name "オプション名で調べる...")
				      (find-option-by-value "オプションの値で調べる...")
				      (find-any-object-by-name "全てのオブジェクトを対象に名前で調べる...")
				      (search-documentation-strings "関数や変数の説明文字列を検索...")
				      )
		,@(if (= emacs-major-version 22)
		      '((apropos "文書を検索"
				 (emacs-glossary "Emacsに関する用語")
				 (emacs-index-search "ユーザーマニュアルで検索...")
				 (elisp-index-search "ELispマニュアルで検索...")
				 (emacs-key-command-node "ユーザーマニュアルでキーバインドを検索...")
				 (emacs-command-node "ユーザーマニュアルでコマンドを検索...")
;				 (sep1 "--")
				 (apropos-commands "コマンド名で調べる...")
				 (apropos-variables "オプション名で調べる...")
				 (apropos-value "オプションの値で調べる...")
				 (apropos "全てのオブジェクトを対象に名前で調べる...")
				 (apropos-documentation "関数や変数の説明文字列を検索...")
				 )))
		(describe "説明"
			  (describe-mode "バッファのモード")
			  (describe-key-1 "キーボードとマウスの操作...")
			  (describe-function "関数...")
			  (describe-variable "変数...")
			  (describe-face "フェイス...")
			  (describe-current-display-table "表示テーブル")
			  (list-keybindings "キー割り付け一覧")
			  ,(append '(describe-language-environment "言語環境") menu-tree-alist-ja-langs)
			  (describe-input-method "インプットメソッド...")
			  (describe-coding-system "文字コード...")
			  (describe-coding-system-briefly "文字コード(要約)")
			  (mule-diag "多言語化機能の状態を全て表示")
			  )
		(emacs-manual "Emacsマニュアルを読む")
		(more-manuals "他のマニュアル"
			      ,@(if (= emacs-major-version 23) ; >= 23.2
				    '((emacs-lisp-intro "Emacs Lisp入門")))
			      (emac-lisp-intro "Emacs Lisp入門") ; < 23.2
			      (emacs-lisp-reference "Emacs Lispリファレンス")
			      (other-manuals "他の全てのマニュアル(Info)")
			      (lookup-subject-in-all-manuals "全てのマニュアルから検索...")
			      (order-emacs-manuals "マニュアルの注文方法")
;			      (sep2 "--")
			      (man "Man Pageを読む...")
			      )
		(find-emacs-packages "Emacsパッケージを調べる")
		(external-packages "外部パッケージ")
;		(sep3 "--")
		(getting-new-versions "新しいバージョンのEmacsの入手方法")
		,@(if (= emacs-major-version 22)
		      '((manuals "他のマニュアル"
				 (info-elintro "Emacs Lisp入門")
				 (info-lisp "Emacs Lispリファレンス")
				 (info "他の全てのマニュアル(Info)")
				 (info-apropos "全てのマニュアルから検索...")
				 (order-emacs-manuals "マニュアルの注文方法")
;				 (sep2 "--")
				 (man "Man Pageを読む...")
				 )
			(finder-by-keyword "Emacsパッケージを調べる")
			(more "外部パッケージ")
			(describe-distribution "新しいバージョンのEmacsの入手方法")
			))
		(describe-copying "ライセンス文書を見る")
		(describe-no-warranty "(無)保証")
;		(sep4 "--")
		(about-emacs "Emacsについて")
		,@(if (= emacs-major-version 22)
		      '((about "Emacsについて")))
		(about-gnu-project "GNUについて")
		)
     )
    (minibuffer-local-map
     (minibuf "ミニバッファ"
	      (return "入力")
	      (quit "終了")
	      ))
    (minibuffer-local-completion-map
     (minibuf "ミニバッファ"
	      (tab "補完")
	      (space "単語を補完")
	      (63 "補完候補の一覧")
	      ))
    (help-mode-map
     (Help-Mode "ヘルプモード"
		(Show\ Help\ for\ Symbol "カーソル位置にあるシンボルのヘルプ")
		(Previous\ Topic "前のトピックに戻る")
		(Next\ Topic "次のトピックに進む")
		(Move\ to\ Previous\ Button "上のボタンへ移動")
		(Move\ to\ Next\ Button "下のボタンへ移動")
		))
    (Info-mode-map
     (Info "Infoモード"
	   (Up "上へ")
	   (Next "次の節")
	   (Previous "前の節")
	   (Backward "次のページ")
	   (Forward "前のページ")
	   (Beginning "このページの最初")
	   (Top "一番上へ")
	   (Final\ Node "一番最後のページ")
	   (Menu\ Item "このページのメニュー")
	   (Reference "このページからの参照")
	   (Search\.\.\. "検索...")
	   (Search\ Next "次を検索")
	   (Go\ to\ Node\.\.\. "ページを指定...")
	   (Back\ in\ history "戻る")
	   (Forward\ in\ history "進む")
	   (History "履歴")
	   (Table\ of\ Contents "目次")
	   (Index "索引"
		  (Lookup\ a\ String\.\.\. "文字列を検索...")
		  (Next\ Matching\ Item "次を検索")
		  (Lookup\ a\ string\ in\ all\ indices\.\.\. "全ての索引群から検索...")
	    )
	   (Copy\ Node\ Name "ページのタイトルをコピー")
	   (Clone\ Info\ buffer "このInfoバッファを複製")
	   (Exit "終了")
	   ))
    (outline-mode-map
     (headings "見出し"
	       (outline-up-heading "上の階層")
	       (outline-next-visible-heading "次")
	       (outline-previous-visible-heading "前")
	       (outline-forward-same-level "次(同じ階層)")
	       (outline-backward-same-level "前(同じ階層)")
	       (outline-insert-heading "新規の見出し")
	       (copy "コピー")
	       (move-subtree-up "前へ移動させる")
	       (move-subtree-down "後ろへ移動させる")
	       (promote-subtree "階層を上げる")
	       (demote-subtree "階層を下げる")
	       )
     (show "表示"
	   (show-all "全て")
	   (show-entry "現在位置の本文")
	   (show-branches "下の階層の見出し")
	   (show-children "1つ下の階層の見出し")
	   (show-subtree "下の階層")
	   )
     (hide "隠す"
	   (hide-leaves "下の階層の本文")
	   (hide-body "文書全体の本文")
	   (hide-entry "現在位置の本文")
	   (hide-subtree "下の階層")
	   (hide-sublevels "文書全体の下の階層")
	   (hide-other "現在位置以外の本文")
	   ))
    (custom-mode-map
     (Custom "カスタム"
	     (Customize "カスタマイズ")
	     ,@(if (= emacs-major-version 23)
		   '((Set\ for\ current\ session "現在のセッションに適用")
		     (Save\ for\ future\ sessions "変更を保存して適用")
		     (Undo\ edits "編集を取り消す")
		     (Reset\ to\ saved "保存されている状態に戻す")
		     (Erase\ customizations "全ての設定を標準の状態に戻す")
		     (Help\ for\ Customize "カスタマイズのヘルプ")
		     (Exit "終了")))
	     ,@(if (= emacs-major-version 22)
		   '((Set "現在のセッションに適用")
		     (Save "変更を保存して適用")
		     (Undo\ Edits "編集を取り消す")
		     (Reset\ to\ Saved "保存されている状態に戻す")
		     (Erase\ Customization "全ての設定を標準の状態に戻す" )
		     (Info "情報")))
	     ))
    ,@(if (= emacs-major-version 23)
	  '((lisp-interaction-mode-map
	     (lisp-interaction "Lisp-Interaction"
			       (complete-symbol "Lispシンボルを補完") ; >= 23.2
			       (lisp-complete-symbol "Lispシンボルを補完") ; < 23.2
			       (indent-pp-sexp "インデントまたは整形")
			       (edebug-defun-lisp-interaction "関数をインスツルメントしてデバッグ")
			       (eval-print-last-sexp "評価して表示")
			       (eval-defun "関数定義を評価")
			       ))))
    (emacs-lisp-mode-map
     (emacs-lisp "Emacs-Lisp"
		 (indent-line "行をインデント")
		 (indent-region "選択領域をインデント")
		 (comment-region "選択領域をコメントアウト")
;		 (separator-format "--")
		 (eval-sexp "S-式を評価")
		 (eval-region "選択領域を評価")
		 (eval-buffer "バッファを評価")
		 ,@(if (= emacs-major-version 23)
		       '((ielm "インタラクティブに式を評価")))
;		 (separator-eval "--")
		 (byte-compile "このファイルをバイトコンパイル")
		 (emacs-byte-compile-and-load "バイトコンパイルして読み込む")
		 (byte-recompile "ディレクトリをバイトコンパイル...")
		 ,@(if (= emacs-major-version 23)
		       '((disas "バイトコンパイルされたオブジェクトを逆コンパイル...")))
;		 (separator-byte "--")
		 (edebug-defun "関数をデバッガに登録")
		 ,@(if (= emacs-major-version 23)
		       '((lint "コードを検査(elint)" ; >= 23.2
			       (lint-d "Defunを検査")
			       (lint-b "バッファを検査")
			       (lint-f "ファイルを検査...")
			       (lint-di "ディレクトリを検査..."))
			 (profiling "プロファイリング"
				    (prof-func "関数をインスツルメント...")
				    (prof-pack "パッケージをインスツルメント...")
				    (prof-res "プロファイリングの結果を表示")
				    (prof-resfunc "関数のカウンタをリセット...")
				    (prof-resall "全ての関数のカウンタをリセット")
;				    (sep-rem "--")
				    (prof-restfunc "関数のインスツルメンテーションを解除...")
				    (prof-restall "全ての関数のインスツルメンテーションを解除")
				    )
			 (tracing "トレース"
				  (tr-f "関数をトレース...")
				  (tr-q "関数をトレース(出力を抑制)...")
;				  (tr-sep "--")
				  (tr-uf "関数のトレースを解除...")
				  (tr-a "全ての関数のトレースを解除")
				  )
			 (re-builder "正規表現を作成(RE-Builder)")
			 (checkdoc "説明文字列を検査する")
			 (eldoc "説明文字列を自動的に表示")))
		 ))
    ,@(if (= emacs-major-version 23)
	  '((reb-mode-map
	     (reb-mode "Re-Builder"
		       (rc "現在の正規表現をコピー")
		       (rp "前のマッチへ移動")
		       (rn "次のマッチへ移動")
		       (ru "強制的に更新")
		       (re "部分正規表現モードに入る")
		       (rs "シンタックスを変更...")
		       (rb "対象のバッファを変更...")
		       (rt "大文字/小文字を区別する")
		       (rq "終了")
		       ))))
    (dired-mode-map
     (operate "ファイル操作"
	      (copy "コピー...")
	      (rename "名前を変更...")
	      (delete "削除")
	      (command "ファイルにコマンドを実行...")
	      (symlink "シンボリックリンクを作成...")
	      (hardlink "ハードリンクを作成...")
	      (print "印刷...")
	      (compress "圧縮")
	      (compile "バイトコンパイル")
	      (load "読み込み")
	      (touch "タイムスタンプを変更")
	      (chmod "属性を変更...")
	      (chgrp "グループを変更...")
	      (chown "所有者を変更...")
	      ,@(if (= emacs-major-version 23)
		    '((isearch "ファイル内をインクリメンタル検索...")
		      (isearch-regexp "ファイル内を正規表現でインクリメンタル検索...")))
	      (search "ファイル内を検索...")
	      (query-replace "ファイル内で置換...")
;	      (dashes-3 "--")
	      (image-dired-display-thumbs "画像のサムネイルを表示")
	      (image-dired-dired-comment-files "画像にコメントを付加...")
	      (image-dired-tag-files "イメージタグを付加...")
	      (image-dired-delete-tag "イメージタグを削除...")
	      )
     (mark "マーク"
	   (toggle-marks "マークを反転")
	   (mark "マークする")
	   (unmark "マークを消す")
	   (deletion "削除用にマーク")
	   (auto-save-files "自動保存ファイル")
	   (backup-files "バックアップファイル")
	   (garbage-files "不要なファイル")
	   (executables "実行ファイル")
	   (directory "古いバックアップファイル")
	   (directories "ディレクトリ")
	   (symlinks "シンボリックリンク")
	   (unmark-all "マークを全て消す")
	   (marks "マークの種類を変更...")
	   (next "次のマークへ移動")
	   (prev "前のマークへ移動"))
     (regexp "正規表現"
	     (mark-cont "ファイルの内容でマーク...")
	     (mark "マーク...")
	     (flag "削除用にマーク...")
	     (copy "コピー...")
	     (rename "名前を変更...")
	     (symlink "シンボリックリンクを作成...")
	     (hardlink "ハードリンクを作成...")
	     (upcase "ファイル名を大文字にする")
	     (downcase "ファイル名を小文字にする")
;	     (dashes-1 "--")
	     (image-dired-mark-tagged-files "イメージタグでマーク...")
	     )
     (immediate "即実行"
	      (wdired-mode "ファイル名をDiredバッファで編集(WDired)")
	      (create-directory "ディレクトリを作成...")
	      (find-file "ファイルを編集")
	      (find-file-other-window "別のウィンドウで編集")
	      (display "別のウィンドウで閲覧")
	      (view "ファイルを閲覧")
	      (diff "差分...")
	      (backup-diff "バックアップファイルとの差分")
	      (compare-directories "ディレクトリを比較...")
	      ,@(if (= emacs-major-version 23)
		    '((isearch-filenames "ファイル名をインクリメンタル検索...")
		      (isearch-filenames-regexp "ファイル名を正規表現でインクリメンタル検索...")))
;	      (dashes "--")
	      (revert-buffer "再表示")
;	      (dashes-4 "--")
	      (image-dired-dired-display-image "画像を表示")
	      (image-dired-dired-display-external "外部ビューアで画像表示")
;	      (dashes-4 "--")
	      ,@(if (= emacs-major-version 23)
		    '((epa-dired-do-encrypt "暗号化")
		      (epa-dired-do-sign "署名")
		      (epa-dired-do-verify "検証")
		      (epa-dired-do-decrypt "復号化")))
	      )
     (subdir "サブディレクトリ"
	     (insert "サブディレクトリも表示")
	     (next-dirline "次のディレクトリ行")
	     (prev-dirline "前のディレクトリ行")
	     (next-subdir "次のサブディレクトリ")
	     (prev-subdir "前のサブディレクトリ")
	     (up "上のディレクトリ")
	     (tree-up "上のサブディレクトリ")
	     (tree-down "下のサブディレクトリ")
	     (hide-subdir "サブディレクトリを表示/非表示")
	     (hide-all "全てのサブディレクトリを表示/非表示")
	     ))
    (wdired-mode-map
     (wdired "Diredバッファの編集"
	     (wdired-finish-edit "変更を適用")
	     (wdired-abort-changes "中止")
;	     (dashes "--")
	     (wdired-customize "オプション")
	     ))
    (image-dired-display-image-mode-map
     (image-dired "画像ディレクトリエディタ"
		  (image-dired-display-current-image-full "元のサイズ")
		  (image-dired-display-current-image-sized "ウインドウに合わせる")
		  (image-dired-kill-buffer-and-window "終了")
		  ))
    (image-dired-thumbnail-mode-map
     (image-dired "画像ディレクトリエディタ"
		  (image-dired-display-thumbnail-original-image "画像を表示")
		  (image-dired-thumbnail-display-external "外部ビューアで表示")
		  (image-dired-mark-thumb-original-file "ファイルをマークする")
		  (image-dired-unmark-thumb-original-file "ファイルのマークを消す")
		  (image-dired-flag-thumb-original-file "ファイルを削除用にマークする")
		  (image-dired-track-original-file "このファイルに移動")
		  (image-dired-jump-original-dired-buffer "Diredバッファへジャンプ")
		  (image-dired-toggle-movement-tracking "選択ファイルへの移動をON/OFF")
		  (image-dired-rotate-original-right "元のファイルを右回転")
		  (image-dired-rotate-original-left "元のファイルを左回転")
		  (image-dired-rotate-thumbnail-right "サムネイルを右回転")
		  (image-dired-rotate-thumbnail-left "サムネイルを左回転")
		  (image-dired-line-up "サムネイルを整列")
		  (image-dired-line-up-dynamic "動的に整列")
		  (image-dired-refresh-thumb "サムネイルを更新")
		  (image-dired-comment-thumbnail "サムネイルにコメントを付加")
		  (image-dired-tag-thumbnail "サムネイルにタグ付け")
		  (image-dired-tag-thumbnail-remove "サムネイルのタグを削除")
		  (image-dired-delete-char "サムネイルを削除")
		  (image-dired-kill-buffer-and-window "終了")
		  ))
    (eudc-hotlist-mode-map
     (EUDC\ Hotlist\ Edit "EUDCホットリスト編集"
;	(--- "---")
	(Add\ New\ Server "新規サーバ")
	(Delete\ Server "サーバを削除")
	(Select\ Server "サーバを選択")
	(Transpose\ Servers "サーバを入れ替え")
	(Save\ and\ Quit "保存して終了")
	(Exit\ without\ Saving "保存せずに終了")
	))
    (calendar-mode-map
     ,@(if (= emacs-major-version 23)
	   '((Scroll "スクロール"
		     (Scroll\ Commands "スクロールコマンド")
		     (Forward\ 1\ Month "1ヶ月後")
		     (Forward\ 3\ Months "3ヶ月後")
		     (Forward\ 1\ Year "1年後")
		     (Backward\ 1\ Month "1ヶ月前")
		     (Backward\ 3\ Months "3ヶ月前")
		     (Backward\ 1\ Year "1年前")
;		     (nil "--")
		     (Motion\ Commands "移動コマンド")
		     (Forward\ 1\ Day "1日進む")
		     (Forward\ 1\ Week "1週間進む")
		     (Forward\ 1\ Month-11 "1ヶ月進む")
		     (Forward\ 1\ Year-12 "1年進む")
		     (Backward\ 1\ Day "1日戻る")
		     (Backward\ 1\ Week "1週間戻る")
		     (Backward\ 1\ Month-15 "1ヶ月戻る")
		     (Backward\ 1\ Year-16 "1年戻る")
		     )
	     (Goto "ジャンプ"
		   (Today "今日")
		   (Beginning\ of\ Week "週の初め")
		   (End\ of\ Week "週の終わり")
		   (Beginning\ of\ Month "月の初め")
		   (End\ of\ Month "月の終わり")
		   (Beginning\ of\ Year "年の初め")
		   (End\ of\ Year "年の終わり")
		   (Other\ Date "年月日を指定")
		   (Day\ of\ Year "年初からの日数を指定")
		   (ISO\ Week "年初からの週数を指定")
		   (ISO\ Date "年初からの週数と曜日を指定")
		   (Astronomical\ Date "天文学的な暦(ユリウス日)")
		   (Hebrew\ Date "ユダヤ暦")
		   (Persian\ Date "ペルシャ暦")
		   (Baha\'i\ Date "バハーイー暦")
		   (Islamic\ Date "イスラム暦")
		   (Julian\ Date "ユリウス暦")
		   (Chinese\ Date "中国暦")
		   (Coptic\ Date "コプト暦")
		   (Ethiopic\ Date "エチオピア暦")
		   (Mayan\ Date "マヤ暦"
				(Next\ Tzolkin "次のツォルキン")
				(Previous\ Tzolkin "前のツォルキン")
				(Next\ Haab "次のハアブ")
				(Previous\ Haab "前のハアブ")
				(Next\ Round "次のラウンド")
				(Previous\ Round "前のラウンド")
				)
		   (French\ Date "フランス暦")
		   )))
     ,@(if (= emacs-major-version 22)
	   '((scroll "スクロール"
		     (fwd-1 "1ヶ月後")
		     (fwd-3 "3ヶ月後")
		     (fwd-12 "1年後")
		     (bk-1 "1ヶ月前")
		     (bk-3 "3ヶ月前")
		     (bk-12 "1年前")
		     )
	     (goto "ジャンプ"
		   (today "今日")
		   (beginning-of-week "週の初め")
		   (end-of-week "週の終わり")
		   (beginning-of-month "月の初め")
		   (end-of-month "月の終わり")
		   (beginning-of-year "年の初め")
		   (end-of-year "年の終わり")
		   (gregorian "年月日を指定")
		   (day-of-year "年初からの日数を指定")
		   (iso-week "年初からの週数を指定")
		   (iso "年初からの週数と曜日を指定")
		   (astro "天文学的な暦(ユリウス日)")
		   (hebrew "ユダヤ暦")
		   (persian "ペルシャ暦")
		   (islamic "イスラム暦")
		   (julian "ユリウス暦")
		   (chinese "中国暦")
		   (coptic "コプト暦")
		   (ethiopic "エチオピア暦")
		   (mayan "マヤ暦"
			  (next-tzol "次のツォルキン")
			  (prev-tzol "前のツォルキン")
			  (next-haab "次のハアブ")
			  (prev-haab "前のハアブ")
			  (nxt-rnd "次のラウンド")
			  (prev-rnd "前のラウンド")
			  )
		   (french "フランス暦")
		   )))
     (Holidays "祝日"
	       ,@(if (= emacs-major-version 23)
		     '((For\ Cursor\ Date\ -	(concat "カーソル位置の日 - " (calendar-date-string (calendar-cursor-to-date) t t)))
		       (For\ Window\ -		(concat "ウィンドウ内 - " (cal-menu-holiday-window-suffix)))
		       (For\ Today\ -		(concat "今日 - " (calendar-date-string (calendar-current-date) t t)))
;		       (nil "--")
		       (hol-year-0 (format "%d年" (+ displayed-year -5)))
		       (hol-year-1 (format "%d年" (+ displayed-year -4)))
		       (hol-year-2 (format "%d年" (+ displayed-year -3)))
		       (hol-year-3 (format "%d年" (+ displayed-year -2)))
		       (hol-year-4 (format "%d年" (+ displayed-year -1)))
		       (hol-year-5 (format "%d年" (+ displayed-year 0)))
		       (hol-year-6 (format "%d年" (+ displayed-year 1)))
		       (hol-year-7 (format "%d年" (+ displayed-year 2)))
		       (hol-year-8 (format "%d年" (+ displayed-year 3)))
		       (hol-year-9 (format "%d年" (+ displayed-year 4)))
		       (hol-year-10 (format "%d年" (+ displayed-year 5)))
		       ))
	       ,@(if (= emacs-major-version 22)
		     '((1-day	(concat "カーソル位置の日 - " (calendar-date-string (calendar-cursor-to-date) t t)))
		       (3-month	(concat "ウィンドウ内 - " (cal-menu-holiday-window-suffix)))
		       (today	(concat "今日 - " (calendar-date-string (calendar-current-date) t t)))
;		       (nil "--")
		       ((intern (format "For Year %d" (+ displayed-year -5))) (format "%d年" (+ displayed-year -5)))
		       ((intern (format "For Year %d" (+ displayed-year -4))) (format "%d年" (+ displayed-year -4)))
		       ((intern (format "For Year %d" (+ displayed-year -3))) (format "%d年" (+ displayed-year -3)))
		       ((intern (format "For Year %d" (+ displayed-year -2))) (format "%d年" (+ displayed-year -2)))
		       ((intern (format "For Year %d" (+ displayed-year -1))) (format "%d年" (+ displayed-year -1)))
		       ((intern (format "For Year %d" (+ displayed-year 0))) (format "%d年" (+ displayed-year 0)))
		       ((intern (format "For Year %d" (+ displayed-year 1))) (format "%d年" (+ displayed-year 1)))
		       ((intern (format "For Year %d" (+ displayed-year 2))) (format "%d年" (+ displayed-year 2)))
		       ((intern (format "For Year %d" (+ displayed-year 3))) (format "%d年" (+ displayed-year 3)))
		       ((intern (format "For Year %d" (+ displayed-year 4))) (format "%d年" (+ displayed-year 4)))
		       ((intern (format "For Year %d" (+ displayed-year 5))) (format "%d年" (+ displayed-year 5)))
		       ))
;	       (nil "--")
	       (Unmark\ Calendar "カレンダーのマークを消す")
	       (Mark\ Holidays "祝日をマークする")
	       )
     ,@(if (= emacs-major-version 23)
	   '((Diary "日記"
		    (Other\ File "他のファイル")
		    (Cursor\ Date "カーソル位置の日")
		    (Mark\ All "全てマークする")
		    (Show\ All "全て表示")
		    (Insert\ Diary\ Entry "日記を書く")
		    (Insert\ Weekly "週単位")
		    (Insert\ Monthly "月単位")
		    (Insert\ Yearly "年単位")
		    (Insert\ Anniversary "記念日")
		    (Insert\ Block "ブロック")
		    (Insert\ Cyclic "周期的")
		    (Insert\ Baha\'i "バハーイー暦"
				     (\  (concat "  " (calendar-bahai-date-string (calendar-cursor-to-date))))
				     (One\ time "1回限り")
				     (Monthly "月単位")
				     (Yearly "年単位")
				     )
		    (Insert\ Islamic "イスラム暦"
				     (\  (concat "  " (calendar-islamic-date-string (calendar-cursor-to-date))))
				     (One\ time "1回限り")
				     (Monthly "月単位")
				     (Yearly "年単位")
				     )
		    (Insert\ Hebrew "ユダヤ暦"
				    (\  (concat "  " (calendar-hebrew-date-string (calendar-cursor-to-date))))
				    (One\ time "1回限り")
				    (Monthly "月単位")
				    (Yearly "年単位")
				    )
		    )
	     (Moon "月"
		   (Lunar\ Phases "月齢")
		   )
	     (Sun/Moon "太陽/月"
		       (Lunar\ Phases "月齢")
		       (Sunrise/sunset\ for\ cursor\ date "カーソル位置の日の日の出/日の入")
		       (Sunrise/sunset\ for\ cursor\ month "カーソル位置の月の日の出/日の入")
		       )
	     ))
     ,@(if (= emacs-major-version 22)
	   '((diary "日記"
		    (view "他のファイル")
		    (mark "全てマークする")
		    (all "全て表示")
		    (ent "日記を書く")
		    (wk "週単位")
		    (mon "月単位")
		    (yr "年単位")
		    (ann "記念日")
		    (blk "ブロック")
		    (cyc "周期的")
		    (baha "バハーイー暦")
		    (isl "イスラム暦")
		    (heb "ユダヤ暦")
		    )
	     (moon "月"
		   (moon "月齢")
		   )
	     ))
     )
    (calc-mode-map
     (Calc "数式処理"
	   (Arithmetic "算術"
		       (Basic "基本")
		       (Rounding "整数化")
		       (Complex\ Numbers "複素数")
		       (Conversion "変換")
		       (Binary "2進数")
;		       (nil "-------")
		       (Help\ on\ Arithmetic "算術のヘルプ")
		       )
	   (Scientific\ Functions "科学関数"
				  (Constants "定数")
				  (Logs\ and\ Exps "対数・指数")
				  (Trigonometric\ Functions "三角関数")
				  (Hyperbolic\ Functions "双曲線関数")
				  (Advanced\ Math\ Functions "高度な関数")
				  (Combinatorial\ Functions "順列・組合せ")
;				  (nil "----")
				  (Help\ on\ Scientific\ Functions "科学関数のヘルプ")
				  )
	   (Algebra "代数"
		    (Simplification "簡単化")
		    (Manipulation "式の操作")
		    (Polynomials "多項式")
		    (Calculus "微積分")
		    (Solving "方程式")
		    (Curve\ Fitting "曲線近似")
;		    (nil "----")
		    (Help\ on\ Algebra "代数のヘルプ")
		    )
	   (Graphics "グラフ"
		     (Graph\ 2D\ \[\(1:\)=\ y\ values\,\ \(2:\)=\ x\ values\] "平面のグラフ [(1:)= y座標, (2:)= x座標]")
		     (Graph\ 3D\ \[\(1:\)=\ z\ values\,\ \(2:\)=\ y\ values\,\ \(3:\)=\ x\ values\] "空間のグラフ [(1:)= z座標, (2:)= y座標, (3:)= x座標]")
;		     (nil "----")
		     (Help\ on\ Graphics "グラフのヘルプ")
		     )
	   (Matrices/Vectors "行列・ベクトル"
			     (Matrices "行列")
			     (Vectors "ベクトル")
			     (Vectors\ As\ Sets "数値の組としてのベクトル")
			     (Statistics\ On\ Vectors "ベクトルの統計量")
			     (Abbreviate\ long\ vectors "長いベクトルを短縮表示")
;			     (nil "----")
			     (Help\ on\ Matrices/Vectors "行列・ベクトルのヘルプ")
			     )
	   (Units "単位"
		  (Convert\ units\ in\ \(1:\) "単位を(1:)に変換")
		  (Convert\ temperature\ in\ \(1:\) "温度を(1:)に変換")
		  (Simplify\ units\ in\ \(1:\) "単位を(1:)に簡単化")
		  (View\ units\ table "単位の一覧表")
;		  (nil "----")
		  (Help\ on\ Units "単位のヘルプ")
		  )
	   (Variables "変数"
		      (Store\ \(1:\)\ into\ a\ variable "(1:)を変数に代入")
		      (Recall\ a\ variable\ value "変数の値を読み出す")
		      (Edit\ the\ value\ of\ a\ variable "変数の値を編集")
		      (Exchange\ \(1:\)\ with\ a\ variable\ value "(1:)と変数の値を交換")
		      (Clear\ variable\ value "変数をクリア")
		      (Evaluate\ variables\ in\ \(1:\) "(1:)で変数を評価")
		      (Evaluate\ \(1:\)\,\ assigning\ a\ value\ to\ a\ variable "変数に一時的に値を割り付けて(1:)を評価")
;		      (nil "----")
		      (Help\ on\ Variables "変数のヘルプ")
		      )
	   (Stack "スタック"
		  (Remove\ \(1:\) "(1:)を取り除く")
		  (Switch\ \(1:\)\ and\ \(2:\) "(1:)と(2:)を交換")
		  (Duplicate\ \(1:\) "(1:)を複製")
		  (Edit\ \(1:\) "(1:)を編集")
;		  (nil "----")
		  (Help\ on\ Stack "スタックのヘルプ")
		  )
	   (Undo "取消し"
		 (Undo "元に戻す")
		 (Redo "やり直し")
;		 (nil "----")
		 (Help\ on\ Undo "取消しのヘルプ")
		 )
	   (Modes "モード"
		  (Precision "精度")
		  (Fraction\ mode "分数モード")
		  (Symbolic\ mode "シンボリックモード")
		  (Infinite\ mode "無限大モード")
		  (Abbreviate\ long\ vectors "長いベクトルを短縮表示")
		  (Angle\ Measure "角度の単位")
		  (Radix "基数")
		  (Float\ Format "浮動小数点の形式")
		  (Complex\ Format "複素数の形式")
		  (Algebraic "代数")
		  (Language "言語")
;		  (nil "----")
		  (Save\ mode\ settings "モードの設定を保存")
;		  (nil "----")
		  (Help\ on\ Modes "モードのヘルプ")
		  )
	   (Help "ヘルプ"
		 (Manual "マニュアル")
		 (Tutorial "チュートリアル")
		 (Summary "概要")
;		 (nil "----")
		 (Help\ on\ Help "ヘルプのヘルプ")
		 )
	   (Reset "リセット")
	   (Quit "終了")
	   ))
    (calculator-mode-map
     (Calculator "電卓"
		 (Help "ヘルプ")
;		 (nil "---")
		 (Copy "コピー")
		 (Paste "貼り付け")
;		 (nil "---")
		 (Electric\ mode "エレクトリックモード")
		 (Normal\ mode "標準モード")
;		 (nil "---")
		 (Functions "関数"
			    (Repeat-right "右の演算を繰り返す")
			    (Repeat-left "左の演算を繰り返す")
;			    (nil "------一般------")
			    (Reciprocal "逆数")
			    (Log "対数")
			    (Square-root "平方根")
			    (Factorial "階乗")
;			    (nil "------三角関数------")
			    (Sinus "正弦(sin)")
			    (Cosine "余弦(cos)")
			    (Tangent "正接(tan)")
			    (Inv-Sinus "逆正弦(arcsin)")
			    (Inv-Cosine "逆余弦(arccos)")
			    (Inv-Tangent "逆正接(arctan)")
;			    (nil "------ビット計算------")
			    (Or "論理和(AND)")
			    (Xor "排他的論理和(XOR)")
			    (And "論理積(AND)")
			    (Not "否定(NOT)")
			    )
		 (Saved\ List "保存されたリスト"
			      (Eval+Save "式を評価してリストに追加")
			      (Prev\ number "前の要素")
			      (Next\ number "次の要素")
			      (Delete\ current "この要素をクリア")
			      (Delete\ all "全ての要素をクリア")
;			      (nil "---")
			      (List-total "リストの合計")
			      (List-average "リストの平均")
			      )
		 (Registers "レジスタ"
			    (Get\ register "レジスタを読み出す")
			    (Set\ register "レジスタに書き込む")
			    )
		 (Modes "モード"
			(Radians "ラジアン")
			(Degrees "度")
;			(nil "---")
			(Decimal "10進法")
			(Binary "2進法")
			(Octal "8進法")
			(Hexadecimal "16進法")
			(Separate\ I/O "入出力で別々の基数"
				       (Decimal\ Input "10進法の入力")
				       (Binary\ Input "2進法の入力")
				       (Octal\ Input "8進法の入力")
				       (Hexadecimal\ Input "16進法の入力")
;				       (nil "---")
				       (Decimal\ Output "10進法の出力")
				       (Binary\ Output "2進法の出力")
				       (Octal\ Output "8進法の出力")
				       (Hexadecimal\ Output "16進法の出力")
				       ))
		 (Decimal\ Display "表示形式"
				   (Standard\ display\,\ decimal\ point\ or\ scientific "標準")
				   (Eng\ display "工学")
				   (Standard\ display\,\ decimal\ point "小数")
				   (Standard\ display\,\ scientific "科学")
				   (Emacs\ printer "Emacsプリンタ")
;				   (nil "---")
				   (Change\ Prev\ Display "仮数部の桁数を減らす")
				   (Change\ Next\ Display "仮数部の桁数を増やす")
				   )
;		 (nil "---")
		 (Copy+Quit "コピーして終了")
		 (Quit "終了")
		 ))
    (bubbles-mode-map
     (Bubbles "バブルズ"
	      (bubbles-undo "元に戻す")
;	      (bubbles-separator-2 "--")
	      (bubbles-graphics-theme-menu "テーマ"
					   (bubbles-set-graphics-theme-circles "丸")
					   (bubbles-set-graphics-theme-squares "正方形")
					   (bubbles-set-graphics-theme-diamonds "菱形")
					   (bubbles-set-graphics-theme-balls "球")
					   (bubbles-set-graphics-theme-emacs "Emacs")
					   (bubbles-set-graphics-theme-ascii "ASCII")
					   )
	      (bubbles-game-theme-menu "難易度"
				       (bubbles-set-game-easy "簡単")
				       (bubbles-set-game-medium "中間")
				       (bubbles-set-game-difficult "難しい")
				       (bubbles-set-game-hard "厳しい")
				       (bubbles-set-game-userdefined "ユーザー定義")
				       )
	      (bubbles-customize "カスタマイズ")
	      (bubbles-save-settings "設定を保存")
;	      (bubbles-separator-1 "--")
	      (bubbles "新規ゲーム")
	      (bubbles-quit "終了")
	      ))
    (global-ede-mode-map
     (cedet-menu "開発"
		 (ede-build-forms-menu "プロジェクトをビルド")
		 (ede-project-options "プロジェクトのオプション")
		 (ede-target-options "ターゲットのオプション")
		 (ede-new "プロジェクトを作成")
		 (ede "プロジェクトを読み込む")
		 (ede-speedbar "プロジェクトのツリーを見る")
		 (ede-find-file "プロジェクト内のファイルを検索...")
;		 (cedet-menu-separator "--")
		 ))
    (semantic-mode-map
     (cedet-menu "開発"
		 (global-semanticdb-minor-mode "Semanticデータベース")
		 (global-semantic-idle-scheduler-mode "アイドル時に再解析")
		 (global-semantic-idle-summary-mode "タグのサマリを表示")
		 (global-semantic-idle-completions-mode "タグの補完を表示")
		 (global-semantic-decoration-mode "タグを装飾")
		 (global-semantic-highlight-func-mode "現在位置の関数を強調表示")
;		 (semantic-options-separator "--")
		 (navigate-menu "タグ間を移動"
				(senator-previous-tag "前のタグ")
				(senator-next-tag "次のタグ")
				(senator-go-to-up-reference "親タグ")
;				(semantic-navigation-separator "--")
				(semantic-complete-jump-local "このバッファ内でタグを検索...")
				(semantic-complete-jump "グローバルにタグを検索...")
				(semantic-symref-symbol "タグの参照を検索...")
;				(semantic-narrow-to-defun-separator "--")
				(senator-narrow-to-defun "タグでナローイング")
				)
		 (semantic-edit-menu "タグを編集"
				     (senator-kill-tag "タグを切り取り")
				     (senator-copy-tag "タグをコピー")
				     (senator-copy-tag-to-register "タグをレジスタにコピー")
				     (senator-yank-tag "タグを貼り付け")
;				     (semantic-edit-separator "--")
				     (senator-transpose-tags-up "上のタグと入れ替え")
				     (senator-transpose-tags-down "下のタグと入れ替え")
;				     (semantic-completion-separator "--")
				     (semantic-complete-analyze-inline "インラインでタグを補完")
				     (semantic-analyze-possible-completions "補完の一覧")
				     )
		 (semantic-force-refresh "バッファを再解析")
		 ))
    )
  "Alist of Japanese translation data for items in menu-bar.
Give the default value of `menu-tree-alist' used by `menu-tree-update'.")

(defvar menu-tree-alist menu-tree-alist-ja
  "Alist of translation data for items in menu-bar.
Used by `menu-tree-update'.

The each element is a cons cell like (KEYMAP-SYMBOL . MENU-TREE).

KEYMAP-SYMBOL is a symbol which indicates the target keymap.

MENU-TREE is a list giving the translation data recursively. The each
element is a list as (MENU-EVENT TEXT) or (MENU-EVENT TEXT . SUB-TREE).
Here, MENU-EVENT specifies a menu event, and TEXT is the translation
data for the menu item. If MENU-EVENT isn't a symbol or TEXT isn't a
string, it will be evaluated when updating the menu item. SUB-TREE is
the same as MENU-TREE except for corresponding to sub menu.")

;; calendar-mode
(defvar menu-tree-calendar-month-name-array
  [ "1月" "2月" "3月"  "4月"  "5月"  "6月"
    "7月" "8月" "9月" "10月" "11月" "12月" ]
  "Array of strings giving the value of the variable
`calendar-month-array'.")

(defvar menu-tree-calendar-date-display-form
  '(year "年" monthname day "日"
	 (if dayname (concat "(" (substring dayname 0 3) ")")))
  "Pseudo-pattern giving the value of the variable
`calendar-date-display-form'.")

(defvar menu-tree-cal-menu-holiday-window-suffix-form
  '(year1 "年" monthname1 "-"
	  (unless (equal year1 year2) (concat year2 "年")) monthname2)
  "Pseudo-pattern governing the string suffix for the \"Window\" entry
in `cal-menu-holidays-menu'.

Used by the function `cal-menu-holiday-window-suffix' (which see),
a pseudo-pattern is a list of expressions that can involve the keywords
`year1' and `year2' (both numbers in string form), and `monthname1' and
`monothname2' (both alphabetic strings).")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun menu-tree-convert (menu-map)
  "Return a menu-tree corresponding to a keymap MENU-MAP.

The menu-tree is a list showing a menu hierarchy given by MENU-MAP. The each
element is a list as (MENU-EVENT TEXT) or (MENU-EVENT TEXT . SUB-TREE).
Here, MENU-EVENT is a symbol indicating a menu event and TEXT is a string
shown in the menu. SUB-TREE is the same as a menu-tree except for
corresponding to sub menu."
  (when (keymapp menu-map)
    (if (symbolp menu-map)
	(setq menu-map (symbol-function menu-map)))
    (let ((entries (cdr menu-map))
	  tree)
      (while entries
	(let ((item (pop entries)))
	  (when (consp (cdr-safe item))
	    (let* ((sym (pop item))
		   (str (pop item))
		   child subtree)
	      ;; Branch for 2 types of menu item
	      (cond
	       ((stringp str)
		;; 1) (EVENT "TEXT" . KEYMAP)
		(setq child item))
	       ((eq str 'menu-item)
		;; 2) (EVENT menu-item "TEXT" KEYMAP)
		(setq str (pop item)
		      child (car item))
		(if (eq (car-safe str) 'format)
		    ;; 2') (EVENT menu-item (format "TEXT" ...) KEYMAP)
		    (setq str (cadr str)))))
	      (if menu-tree-coding-system
		  (setq str (decode-coding-string str menu-tree-coding-system)))
	      (push (cons sym (cons str (menu-tree-convert child)))
		    tree)))))
      (nreverse tree))))

(defun menu-tree-get (&optional keymap)
  "Return a menu-tree corresponding to a menu-bar keymap included in KEYMAP.
Omitting KEYMAP means use current global-map.

The menu-tree is a list showing a menu hierarchy given by the keymap
which menu-bar event is bound to in KEYMAP. The each element is a list as
\(MENU-EVENT TEXT) or (MENU-EVENT TEXT . SUB-TREE). Here, MENU-EVENT is
a symbol indicating a menu event and TEXT is a string shown in the menu.
SUB-TREE is the same as a menu-tree except for corresponding to sub menu."
  (menu-tree-convert (lookup-key (or keymap (current-global-map)) [menu-bar])))

(defun menu-tree-pp (&optional keymap)
  "Pretty-print a menu-tree corresponding to a menu-bar keymap included in
KEYMAP. Omitting KEYMAP means use current global-map.

The menu-tree is a list showing a menu hierarchy given by the keymap
which menu-bar event is bound to in KEYMAP. The each element is a list as
\(MENU-EVENT TEXT) or (MENU-EVENT TEXT . SUB-TREE). Here, MENU-EVENT is
a symbol indicating a menu event and TEXT is a string shown in the menu.
SUB-TREE is the same as a menu-tree except for corresponding to sub menu."
  (interactive
   (let ((map (make-sparse-keymap))
	 (arg t))
     (set-keymap-parent map read-expression-map)
     (define-key map (kbd "TAB") 'lisp-complete-symbol)
     (while (and arg (not (keymapp arg)))
       (setq arg (eval (read-from-minibuffer "Keymap (global-map): " nil map t
					     'read-expression-history "nil"))))
     (list arg)))
  (with-output-to-temp-buffer "*Menu-Tree*"
    (with-current-buffer standard-output
      (pp (menu-tree-get keymap))
      (emacs-lisp-mode)
      (display-buffer (current-buffer))
      (buffer-enable-undo))))

(defun menu-tree-override (keymap menu-tree)
  "Replace all texts recursively in a menu defined by KEYMAP according
to MENU-TREE.

MENU-TREE is a list giving the translation data recursively. The each
element is a list as (MENU-EVENT TEXT) or (MENU-EVENT TEXT . SUB-TREE).
Here, MENU-EVENT specifies a menu event, and TEXT is the translation
data for the menu item. If MENU-EVENT isn't a symbol or TEXT isn't a
string, it will be evaluated when updating the menu item. SUB-TREE is
the same as MENU-TREE except for corresponding to sub menu."
  (when (keymapp keymap)
    (if (symbolp keymap)
	(setq keymap (symbol-function keymap)))
    (while menu-tree
      (let* ((subst (pop menu-tree))
	     (sym (pop subst))
	     (str (pop subst))
	     ;; Some items are dynamically changed every time
	     (item (cdr (assq (if (symbolp sym) sym (eval sym)) keymap))))
	(when (consp item)
	  (unless (stringp str) (setq str (eval str)))
	  (if menu-tree-coding-system
	      (setq str (encode-coding-string str menu-tree-coding-system)))
	  ;; Branch for 2 types of menu item
	  (cond
	   ((stringp (car item))
	    ;; 1) (EVENT "TEXT" . KEYMAP)
	    (setcar item str)
	    (menu-tree-override (cdr item) subst))
	   ((eq (pop item) 'menu-item)
	    ;; 2) (EVENT menu-item "TEXT" KEYMAP)
	    (setcar (if (eq (car-safe (car item)) 'format)
			;; 2') (EVENT menu-item (format "TEXT" ...) KEYMAP)
			(cdar item)
		      item)
		    str)
	    (menu-tree-override (cadr item) subst))))))))

(define-obsolete-function-alias
  'menu-tree-override-by-alist 'menu-tree-update "menu-tree.el version 0.94")

(defun menu-tree-update (keymap-symbol &optional menu-path)
  "Update the texts of menu items according to `menu-tree-alist'.
KEYMAP-SYMBOL specifies a keymap defining the menu.

Omitting MENU-PATH means all texts in the menu specified by
KEYMAP-SYMBOL are updated. Otherwise, it must be a list of symbols
specifying the sub menu or the menu item."
  (menu-tree-override
   ;; Find target menu
   (lookup-key (if (eq keymap-symbol 'global-map)
		   (current-global-map)
		 (symbol-value keymap-symbol))
	       (let ((seq (cons 'menu-bar (copy-sequence menu-path))))
		 (setcdr (nthcdr (1- (length menu-path)) seq) nil)
		 (vconcat seq)))
   ;; Find source from `menu-tree-alist'
   (if menu-path
       (let ((map (assq keymap-symbol menu-tree-alist)))
	 (while menu-path
	   (setq map (assq (pop menu-path) (cdr map))))
	 (list map))
     (cdr (assq keymap-symbol menu-tree-alist))))
  (force-mode-line-update))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Static overriding menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-tree-update 'global-map)
(menu-tree-update 'minibuffer-local-map)
(menu-tree-update 'minibuffer-local-completion-map)
(if (= emacs-major-version 23)
    (menu-tree-update 'lisp-interaction-mode-map))
(menu-tree-update 'emacs-lisp-mode-map)
(if (= emacs-major-version 23)
    (eval-after-load "re-builder"
      '(menu-tree-update 'reb-mode-map)))
(add-hook 'help-mode-hook
	  (lambda ()
	    (menu-tree-update 'help-mode-map)) t)
(eval-after-load "outline"
  '(menu-tree-update 'outline-mode-map))
(eval-after-load "cus-edit"
  '(menu-tree-update 'custom-mode-map))
(eval-after-load "dired"
  '(menu-tree-update 'dired-mode-map))
(eval-after-load "wdired"
  '(menu-tree-update 'wdired-mode-map))
(add-hook 'image-dired-display-image-mode-hook
	  (lambda ()
	    (menu-tree-update 'image-dired-display-image-mode-map)) t)
(add-hook 'image-dired-thumbnail-mode-hook
	  (lambda ()
	    (menu-tree-update 'image-dired-thumbnail-mode-map)) t)
(add-hook 'calc-mode-hook
	  (lambda ()
	    (menu-tree-update 'calc-mode-map)) t)
(eval-after-load "calculator"
  '(menu-tree-update 'calculator-mode-map))
(eval-after-load "eudc"
  '(menu-tree-update 'global-map '(tools directory-search)))
(eval-after-load "eudc-hotlist"
  '(menu-tree-update 'eudc-hotlist-mode-map))
(eval-after-load "bubbles"
  '(menu-tree-update 'bubbles-mode-map))
(eval-after-load "redo+"
  '(menu-tree-update 'global-map '(edit redo)))
(eval-after-load "ede"
  '(if (boundp 'global-ede-mode-map)
       (menu-tree-update 'global-ede-mode-map)))
(eval-after-load "semantic"
  '(if (boundp 'semantic-mode-map)
       (menu-tree-update 'semantic-mode-map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dynamic overriding menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'menu-bar-update-hook
	  (lambda ()
	    ;; Buffer menu
	    (menu-tree-update 'global-map '(buffer))
	    ;; ispell
	    (menu-tree-update 'global-map '(tools spell))
	    ) t)

;; Info-mode
(add-hook 'Info-mode-hook
	  (lambda ()
	    ;; Dynamic overriding
	    (add-hook 'menu-bar-update-hook
		      (lambda ()
			(menu-tree-update 'Info-mode-map)) t t)
	    ) t)

;; calendar-mode
(eval-after-load "calendar"
  '(setq calendar-month-name-array menu-tree-calendar-month-name-array
	 calendar-date-display-form menu-tree-calendar-date-display-form))

(eval-after-load "cal-menu"
  '(defun cal-menu-holiday-window-suffix ()
     "Return a string suffix for the \"Window\" entry in `cal-menu-holidays-menu',
driven by the variable `menu-tree-cal-menu-holiday-window-suffix-form'."
     (let* ((my1 (if (fboundp 'calendar-increment-month-cons)
		     (calendar-increment-month-cons -1)
		   (calendar-increment-month -1)))
	    (my2 (if (fboundp 'calendar-increment-month-cons)
		     (calendar-increment-month-cons 1)
		   (calendar-increment-month 1)))
	    (monthname1 (calendar-month-name (car my1) 'abbrev))
	    (monthname2 (calendar-month-name (car my2) 'abbrev))
	    (year1 (number-to-string (cdr my1)))
	    (year2 (number-to-string (cdr my2))))
       (eval `(concat ,@menu-tree-cal-menu-holiday-window-suffix-form)))))

(add-hook 'calendar-mode-hook
	  (lambda ()
	    ;; Dynamic overriding
	    (add-hook 'menu-bar-update-hook
		      (lambda ()
			(menu-tree-update 'calendar-mode-map)) t t)
	    ) t)

(provide 'menu-tree)

;;;
;;; menu-tree.el ends here
