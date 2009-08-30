;;; cmd-mode.el --- Edit of MS Windows cmd and bat files ;; -*- coding: sjis-dos -*-
;;
;; Copyright (C) 2001-2005 by Tadamegu Furukawa. <tfuruka1 at nifty dot com>
;;
;; Author: Tadamegu Furukawa <tfuruka1 at nifty dot com>
;; Maintainer: Lennart Borgman <lennart dot borgman dot 073 at student dot lu dot se>
;; Created: 2001-08-28
;; Version: 1.4
;; Keywords:
;;
;;
;; Kindly translated to English 2005 by Kenichi Handa after an inquiry
;; by Lennart Borgman.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file is not part of GNU Emacs.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary:
;; 
;; MS-DOSのバッチファイル及びWindows NTのコマンドスクリプトを編集する
;; 為のへなちょこモードです。%が沢山あって、何がなんだかわからないバッ
;; チファイルを読み易くする目的で作成したので、基本的にfont-lock以外の
;; 機能は殆どありません。しかもemacs 20.7 "Meadow-1.14 (AWSAKA:62)" で
;; しか試していません(これ以外で使用される事を想定していません)。
;;
;; Tiny mode for editing batch files of MS-DOS and command scripts of
;; Windows NT.  The purpose is to improve the readablity of those
;; files that contains many `%' and are difficult to read.  So,
;; basically this provides only a proper font-lock setting.  This
;; program has been tested for Emacs 20.7 "Meadow-1.14 (AWSAKA:62)
;; (the other versions of Emacsen are out of my focus).
;;
;; It has now been tested also on Emacs 21.3 and 22.0.50 on W2k.



;; .emacsの何処かに以下を追加してください。拡張子がcmd又はbatのファイ
;; ルを開くと自動でcmd-modeになります。
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Usage:
;;
;; Please input the following code somewhere in your .emacs.  Then,
;; cmd-mode is automatically activated when you open a file whose
;; extention is "cmd" or "bat".

;;    (autoload 'cmd-mode "cmd-mode" "CMD mode." t)
;;    (setq auto-mode-alist (append '(("\\.\\(cmd\\|bat\\)$" . cmd-mode))
;;                                  auto-mode-alist))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; History:
;; 
;;;;;;;;;;;;;;;;; What is this:
;; $Id: cmd-mode.el,v 1.1 2005/07/18 14:38:50 Administrator Exp $
;; $Log: cmd-mode.el,v $
;; Revision 1.1  2005/07/18 14:38:50  Administrator
;; *** empty log message ***
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Revision 1.4  2005/10/18
;; 
;; o Translation to English. (Kenichi Handa)
;; o Changed set-mark to push-mark. (Lennart Borgman)


;; Revision 1.3  2001/09/11 12:39:03  tfuruka1
;;
;; ●メニューをつけてみた。
;; o Add menu.
;;
;; ●任意のラベルへジャンプする機能を追加。
;; o Add a facility to jump to any label.


;; Revision 1.2  2001/08/31 13:25:50  tfuruka1
;;
;; ●SETの環境変数にキーワード文字列が含まれていると、色付けが変になる問題
;;   を修正
;; o Fix the problme of incorrect coloring in the case that an
;;   environment varialbe of SET contains a keyword string.
;;
;; ●SET /A の時に演算子の一部を環境変数として、間違えて色付けする問題を
;;   修正
;; o FIx the problem of incorrectly coloring a part of operand in SET
;;   /A as an environment variable in

;; Revision 1.1  2001/08/28 13:14:43  tfuruka1
;;
;; o Initial version.



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:


;; Added by L Borgman:
(require 'font-lock)

;; (replace-regexp "[\t ]+$" "")

(defconst cmd-mode-revision-number "1.4" "cmd-mode.el version number.")

;;; *** メニューバー
;;; *** menu bar
(defvar cmd-menu-bar (make-sparse-keymap "cmd-mode-menu")
  ;; "メニュー"
  "Menu bar entry.")
(defvar cmd-submenu-jump (make-sparse-keymap "cmd-mode-submenu-jump")
  ;; "ポップアップメニュー（ジャンプ）"
  "Popup menu for jump.")

;;; *** hook
(defvar cmd-mode-hook nil
  ;; "cmd-modeを作成するタイミングで呼ばれる hook です。"
  "Hook called when `cmd-mode' is entered.")
(defvar cmd-help-mode-hook nil
  ;; "cmd-help-modeを作成するタイミングで呼ばれる hook です。"
  "Hook called when `cmd-help-mode' is entered.")

;;; *** 外部コマンド
;;; *** External command
(defvar cmd-help-command "help"
  ;; "helpコマンドのコマンド名。Windows NT系以外には存在しません。"
  "Name of `help' command.  It exists only on Windows NT.")

;;; *** buffer関係
;;; *** Buffer related things.
(defvar cmd-temp-buf " *cmd-temp-buf*"
  ;; "作業用の隠しバッファ"
  "Hidden buffer for work.")
(defvar cmd-help-buf-base-name "*cmd-help "
  ;; "ヘルプバッファのベース名"
  "Base name of help buffer.")

;;; *** regexp
(defvar cmd-full-size-space-etc-pattern "\\(　+\\)"
  ;; "所謂「全角の空白」の正規表現"
  "Regular expression for fullwidth space sequence.")
(defvar cmd-tab-pattern "\\(\t+\\)"
  ;; "TAB文字の正規表現"
  "Regular expression for TAB character sequence.")
(defvar cmd-comment-pattern "^[ \t]*\\(@*\\)\\(rem\\)\\([\t ].*$\\|$\\)"
  ;; "コメント行の正規表現"
  "Regular expression for a comment line.")
(defvar cmd-variable-pattern
  (concat "\\("
          "%\\("                     ;最初の文字が%  the first char is `%'
          "\\*"                      ;%* の場合	in case of "%*"
          "\\|"
          "~[^0-9 \n]+[0-9a-z]"      ;~f1 等		e.g. ~f1
          "\\|"
          "[0-9a-z]\\b"              ;%1 等		e.g. %1
          "\\|"
          "[^%\n 　-龠]+%"           ;通常の環境変数	normal env. var.
          "\\|"
          "%[0-9a-z]"               ;%% 等		e.g. %$
          "\\)"
          "\\)")
;;   "環境変数の展開の正規表現

;; 想定しているパターンは以下の通りです。

;; 例            意味
;; -----------   -----------------------
;; %*            すべての引数を参照する
;; %~fi, %~dp1   バッチパラメータ(%n)及びFOR変数参照の置換
;; %文字列%      通常の環境変数の参照
;; %1, %i        バッチパラメータ(%n)及びFOR変数参照
;; %%, %%1, %%i  %そのもの、バッチパラメータ(%n)及びFOR変数参照（バッチ
;;               ファイル内で使用した場合）"
  "Regular expression for expanding an environment variable.

The following patterns are concerned.

example		meaning
-------		-------
%*		refer to all of the arguments
%~f1, %~dp1	replacement for batch parameter (%n) and FOR variable reference
%STRING%	normal reference of an environment variable
%1, %i		batch parameter (%n) and FOR variable reference
%%, %%1, %%i	% itself, batch parameter (%n) and FOR variable reference
		(when used in a batch file)"
)
(defvar cmd-const-pattern
  "\\(^[ \t]*@\\|nul\\|:eof\\|&&\\||\\|\\^\\|&[12]?\\|[,;]\\)"
  ;; "条件付き処理記号等の正規表現"
  "Regular expression for conditional symbols.")
(defvar cmd-set-pattern
  (concat "\\b"
          "\\(set\\b\\)\\([ \t]+/a\\)*"
          "\\(\\([ \t]+[_A-Za-z-][_A-Za-z0-9-]*\\)*\\)"
          "\\([-+/\\*]\\|\\W\\)*")
  ;; "SETコマンドの正規表現"
  "Regular expression for SET command.")
(defvar cmd-label-pattern "^[ \t]*\\(:[:A-Za-z0-9_-]+\\)"
  ;; "ラベルの正規表現"
  "Regular expression for a label.")
(defvar cmd-redirect-pattern
  (concat "\\("
          "[12]?>>?"                 ;1> 2> 1>> 2>> > >>
          "\\|"
          "<[12]?"                      ;<1 <2 <
          "\\|"
          "2>&1"
          "\\|"
          "1>&2"
          "\\)")
  ;; "リダイレクトの正規表現"
  "Regular expression for redirection.")
(defvar cmd-option-pattern
  (concat "\\b"
          (regexp-opt
           '(
             ;; if
             "not" "exist" "defined" "errorlevel" "cmdextversion"
             "equ" "neq" "lss" "leq" "gtr" "geq"
             ;; for
             "eol" "skip" "delims" "tokens" "in" "do") t) "\\b")
  ;; "IF文等のオプションの正規表現"
  "Regular expression for options to a statement like IF.")
(defvar cmd-command-pattern
  (concat "\\b"
          (regexp-opt '("assoc" "break" "call" "cd" "chdir" "cls"
                        "color" "copy" "date" "del" "dir" "echo"
                        "echo." "endlocal" "erase" "exit" "for"
                        "ftype" "goto" "if" "md" "mkdir" "move" "path"
                        "pause" "popd" "prompt" "pushd" "rd" "ren"
                        "rename" "rmdir" "setlocal" "shift" "start"
                        "time" "title" "type" "ver" "verify" "vol" )
                      t) "\\b")
;;   "コマンドの正規表現

;; コマンドは内部コマンドのみです。Windows NTのHELPコマンドで一覧表示され
;; るコマンドから、外部コマンド(*.exe, *.com)を除いたものです。但し、SET
;; と REM は他で定義していますので、ここでは除いています。また、ECHO は
;; ECHO. も含めています"

  "Regular expression for internal commands.

Only internal commands.  Actually they are commands listed by
HELP command of Windows NT (such external commands as *.ext and
*.com are excluded).  SET and REM are also excluded because they
are defined in the other place.  As for ECHO, ECHO.  is included
too.")

;; font-lockの設定
;; Setting for font-lock.
(defvar cmd-font-lock-keywords
  (list
   ;; コメント
   ;; command
   (list cmd-comment-pattern
         '(1 font-lock-constant-face)   ;行頭の@	@ at bol
         '(2 font-lock-keyword-face)    ;rem
         '(3 font-lock-comment-face)    ;コメント文字	command character
         )
   ;; SET
   (list cmd-set-pattern
         '(1 font-lock-keyword-face)    ;SET
         '(3 font-lock-type-face)       ;環境変数名	name of env. var.
         )
   ;; ラベル
   ;; label
   (list cmd-label-pattern
         '(1 (cons font-lock-function-name-face '(underline))))
   ;; リダイレクト記号
   ;; redirect symbol
   (list cmd-redirect-pattern 1 font-lock-warning-face)
   ;; 環境変数の参照
   ;; reference of environment variable
   (list cmd-variable-pattern 1 font-lock-variable-name-face)
   ;; 内部コマンド
   ;; internal command
   (list cmd-command-pattern 1 font-lock-keyword-face)
   ;;条件付き処理記号等
   ;; e.g. conditional symbols
   (list cmd-const-pattern 1 font-lock-constant-face)
   ;; IF文とFOR文のオプション等
   ;; e.g. options of IF and FOR
   (list cmd-option-pattern 1 font-lock-builtin-face)
   ;; 全角スペース
   ;; fullwidth space
   (list cmd-full-size-space-etc-pattern '(1 '(underline)))
   ;; TAB文字
   ;; TAB character
   (list cmd-tab-pattern '(1 '(highlight)))
   )
;;   "cmd-mode, cmd-help-modeで使用するfont-lockの設定です。
;; 詳細はfont-lock-defaultsを参照してください。"
  "Setting for font-lock used in `cmd-mode' and `cmd-help-mode'.
See `font-lock-defaults' for detail.")

(defun cmd-mode-version ()
  ;; "cmd-modeのVersion表示"
  "Show the version of `cmd-mode'."
  (interactive)
  (message
   (concat "cmd-mode version " cmd-mode-revision-number)))

(defun cmd-help (arg)
;;   "helpコマンドを実行して、結果を表示します。
;; この関数は、Windows NT系以外では、動作しません。

;; ※Windows \\(9[58]\\|Me\\)でも動作するように作ったつもりだったんですが、
;; 見事に動作しません。"
  "Execute help command and show the result.
This functinos works only on Windows NT.

Implementers note: I intended that this work also on Windows
\\(9[58]\\|Me\\), but actually failed.
Argument ARG is the command to describe."
  (interactive
   (list (let* ((command (current-word))
                (input (read-string
                        (format "Help for command%s: " ;; "コマンド%s: "
                                (if (string-equal command "")
                                    ""
                                  (format " (%s)" command))))))
                (if (string-equal input "")
                    (if (string-equal command "")
                        "help"
                      command)
                  input))))

  (let* ((case-fold-search t)
        (cmd-help-buffer (format "%s%s*"
                                 cmd-help-buf-base-name (downcase arg)))
        (comspec (getenv "ComSpec"))
        (cmd-help-arg
         (cond
          ((string-match "cmd\\.exe$" comspec)
           (list "\\/c" cmd-help-command
                 (if (string-equal arg "help") "" arg)))
          ((string-match "command\\.com$" comspec)
           (if (string-equal arg "help")
               (error "Insert command" ;; "コマンドを入力してください"
		      )
             (list "\\/c" arg "\\/?")))
          (t
           (error (concat "Work only on WindowsXX ComSpec="
			  ;;"WindowsXX以外では動作しません ComSpec="
			  comspec)))))
        )
    (set-buffer (get-buffer-create cmd-help-buffer))
    (setq buffer-read-only nil)
    (erase-buffer)
    (apply (function call-process) comspec nil t nil cmd-help-arg)
    (goto-char (point-min))
    (display-buffer cmd-help-buffer)
    (cmd-help-mode)
    ))

(defun cmd-help-mode-exit ()
  ;; "cmd-help-modeを終了します。"
  "Terminate `cmd-help-mode'."
  (interactive)
  (if (eq major-mode 'cmd-help-mode)
      (kill-buffer (current-buffer)))
  )

(defun cmd-help-mode ()
;;   "cmd-helpから呼び出されます。

;; ●ヘルプコマンドを実行し、結果を表示します。
;;   \\[cmd-help]

;; ●cmd-help-modeを終了します。
;;   \\[cmd-help-mode-exit]"

  "Mode for `cmd-help'.

o Execute help command and show the result.
  \\[cmd-help]

o Terminate `cmd-help-mode'.
  \\[cmd-help-mode-exit]"
  (interactive)
  (kill-all-local-variables)
  ;;モード名の設定と、モードラインのモード名フィールドの設定
  ;; Setting the mode name and the mode-name field of the mode-line.
  (setq major-mode 'cmd-help-mode
        mode-name "cmd-help"
        buffer-auto-save-file-name nil  ;自動保存しない	suppresss auto saving
        buffer-read-only t              ;読み込み専用	for read only
        )
  ;; キーマップの設定
  ;; Setting keymap.
  (setq cmd-help-local-map (make-keymap))
  ;; キーの割り当て
  ;; Assigning keys
  (define-key cmd-help-local-map "\C-ch" 'cmd-help)
  (define-key cmd-help-local-map "\C-m" 'cmd-help)
  (define-key cmd-help-local-map " " 'cmd-help)
  (define-key cmd-help-local-map "q" 'cmd-help-mode-exit)
  (define-key cmd-help-local-map "Q" 'cmd-help-mode-exit)
  (define-key cmd-help-local-map "n" 'next-line)
  (define-key cmd-help-local-map "p" 'previous-line)

  ;; ローカルマップの使用宣言
  ;; Declare to use of the local map
  (use-local-map cmd-help-local-map)

  ;; フォントロックの設定
  ;; Setting for font-lock
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
        '((cmd-font-lock-keywords) t t))
  (font-lock-mode t)

  (run-hooks 'cmd-help-mode-hook)
  )

(defun cmd-right-trim-region (start end)
  ;; "リージョンで指定された範囲の行末の空白文字を削除します。"
  "Delete space characters at the end of line of the specified region.
Region is between START and END."
  (interactive "r")
  (save-excursion
    (goto-char start)
    (while (re-search-forward "[\t ]+$" end t)
      (replace-match ""))))

(defun cmd-exec ()
;;   "バッファの内容を（必要であれば）ファイルに保存し、保存したファイル
;; を実行する。かなり手抜きをしています。"

  "Save the buffer (if necessary) and execute it.
corner-cutting work."
  (interactive)
  (save-buffer)
  (shell-command (buffer-file-name))
  )

(defun cmd-recenter ()
  ;; "カレント位置を中央に配置した後、余分な行末のスペースを削除する"
  "Set the point to the center, and delete extra spaces at the end of line."
  (interactive)
  (recenter)
  (cmd-right-trim-region (point-min) (point-max)))

(defun cmd-next-label ()
  ;; "次のラベルへ跳ぶ"
  "Jump to the next label."
  (interactive)
  (if (re-search-forward cmd-label-pattern nil t)
      (goto-char (match-end 0))))
(defun cmd-prev-label ()
  ;; "前のラベルへ跳ぶ"
  "Jump to the previous label."
  (interactive)
  (if (re-search-backward cmd-label-pattern nil t)
      (goto-char (match-beginning 0))))

(defun cmd-fill-paragraph ()
;;   "コメント行等の行詰めを行います。かなり手抜きをしています(こんな事を
;; して良いのだろうか)。"

  "Fill comment lines.
Fairly corner-cutting (is it allowed to do this kind of thing?)."
  (interactive)
  (save-excursion
    (let (;;大文字・小文字を区別しない
	  ;;Ignore case
	  (case-fold-search t)
          (cmd-rem-regexp
           "\\([\t ]*@?\\(rem\\|echo\\)\\)\\([ \t].*$\\|$\\)")
          rem-begin
          rem-end
          rem-paragraph
          match-str
          (cmd-fill-column fill-column)
          (current-buffer (buffer-name))
          )
      (beginning-of-line)
      (if (looking-at cmd-rem-regexp)
          (progn
            (setq match-str (buffer-substring
                             (match-beginning 1) (match-end 1))
                  rem-begin (point))
            (message ;; "cmd-fill-paragraph【%s】"
	     "cmd-fill-paragraph [%s]" match-str)
            (while (not (bobp))
              (forward-line -1)
              (if (looking-at (concat match-str "\\([\t ]\\|$\\)"))
                  (setq rem-begin (point))
                (goto-char (point-min))))

            (goto-char rem-begin)
            (setq cmd-rem-regexp
                  (concat "\\("
                          "\\(^" match-str "[^\n]*\n\\)+"
                          "\\(^" match-str "[^\n]*\\)?\\|"
                          "^" match-str ".*$\\)"))
            (if (looking-at cmd-rem-regexp)
                (progn
                  (setq rem-end (match-end 0)
                        rem-paragraph (buffer-substring rem-begin rem-end))
                  (set-buffer (get-buffer-create cmd-temp-buf))
                  (erase-buffer)
                  (insert rem-paragraph)
                  (indented-text-mode)

                  (goto-char (point-min))
                  (while (re-search-forward
                          (concat "^" match-str " ?") nil t)
                    (replace-match ""))

                  (setq fill-column (- cmd-fill-column (length match-str)))
                  (goto-char (point-min))
                  (while (not (eobp))
                    (fill-paragraph nil)
                    (forward-paragraph))

                  (goto-char (point-min))
                  (while (not (eobp))
                    (beginning-of-line)
                    (insert (concat match-str
                                    (if (looking-at "$") "" " ")))
                    (beginning-of-line)
                    (if (looking-at "[ \t]*@?echo$")
                        (progn
                          (end-of-line);
                          (insert ".")))
                    (forward-line 1))

                  (goto-char (point-max))
                  (beginning-of-line)
                  (if (looking-at (concat match-str "$"))
                      (replace-match ""))

                  (cmd-right-trim-region (point-min) (point-max))

                  (setq rem-paragraph
                        (buffer-substring (point-min) (point-max)))
                  (kill-buffer cmd-temp-buf)
                  (set-buffer current-buffer)
                  (delete-region rem-begin rem-end)
                  (insert rem-paragraph)
                  (message "done.")
              )
              (progn
                (ding)
                (error ;; "cmd-fill-paragraph【バグでしょう】"
		 "cmd-fill-paragraph [seems like a bug]")))
            )
        (message
	 ;;"cmd-fill-paragraph: 行頭が REM 又は ECHO ではありません"
	 "cmd-fill-paragraph: The line doesn't start with REM nor ECHO.")
        )
      )
    )
  )

(defun cmd-goto-label ()
;;   "指定したラベルへジャンプします。

;; ラベルはミニバッファから補完入力できます。ミニバッファには、現在のカー
;; ソルの下の文字列がデフォルト表示されます。(既存のラベルの何れかに一致
;; する場合)"

  "Jump to the specified label.

Label can be completed at the mini-buffer.  A word at the cursor
position is shown as default (if it matches one of existing
labels)."
  (interactive)
  (let ((label-alist '())
        (label nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              "^[ \t]*:\\([:A-Za-z0-9_-]+\\)\\([\t ]\\|$\\)" nil t)
        (setq label-alist (cons (list (buffer-substring (match-beginning 1)
                                                        (match-end 1)))
                                label-alist))))
    (if (not label-alist)
        (error ;; "ラベルが見つかりません"
	       "No label found"))
    (setq label (completing-read "Label:" label-alist nil t
                                 (if (assoc (current-word) label-alist)
                                     (current-word) "")))
    (when (and label (not (string= label "")))
      (push-mark (point))                ;マークを付ける		Set mark
      (goto-char (point-min))
      (re-search-forward (concat "^[\t ]*:" label "\\([ \t]\\|$\\)"))
      )
    )
  )

(defun cmd-mode ()
;;   "MS-DOSのバッチファイルやWindows NT のコマンドスクリプトファイルを編
;; 集する為のへなちゃこもーどです。

;; ●バッファを保存後、保存したファイルを実行する
;;   \\[cmd-exec]

;; ●コマンドのヘルプを表示する。
;;   \\[cmd-help]

;; ●指定したラベルへジャンプ
;;   \\[cmd-goto-label]

;; ●現在のカーソル位置を画面の中央に配置し、行末の余分な空白文字を削除
;;   する。
;;   \\[cmd-recenter]

;; ●コメント行等(REM or ECHO)の行詰めを行なう。
;;   \\[cmd-fill-paragraph]

;; ●前のラベルへ
;;   \\[cmd-prev-label]

;; ●次のラベルへ
;;   \\[cmd-next-label]"

  "Tiny mode for editing batch files (MS-DOS) and command scripts (Windows NT).

* Save buffer and execute it.

  \\[cmd-exec]

* Show help of a command.
  \\[cmd-help]

* Jump to the specified label.
  \\[cmd-goto-label]

* Set the point to the center, and delete extra spaces at the end
  of line.
  \\[cmd-recenter]

* Fill comment lines (e.g REM, ECHO).
  \\[cmd-fill-paragraph]

* Jump to the previous label.
  \\[cmd-prev-label]

* Jump to the next label.
  \\[cmd-next-label]"
  (interactive)
  (kill-all-local-variables)
  ;;モード名の設定と、モードラインのモード名フィールドの設定
  ;; Setting the mode name and the mode-name field of the mode-line.
  (setq major-mode 'cmd-mode
        ;;mode-name "CMDとBAT")
        mode-name "CMD")
  ;; キーマップの設定
  ;; Setting keymap.
  (setq cmd-local-map (make-keymap))
  ;; キーの割り当て
  ;; Assigning keys
  (define-key cmd-local-map "\C-c\C-c" 'cmd-exec)
  (define-key cmd-local-map "\C-cg" 'cmd-goto-label)
  (define-key cmd-local-map "\C-ch" 'cmd-help)
  (define-key cmd-local-map "\C-l" 'cmd-recenter)
  ;;(define-key cmd-local-map "\eq" 'cmd-fill-paragraph)
  (define-key cmd-local-map "\M-q" 'cmd-fill-paragraph)
  ;;(define-key cmd-local-map "\e\C-a" 'cmd-prev-label)
  (define-key cmd-local-map "\M-\C-a" 'cmd-prev-label)
  ;;(define-key cmd-local-map "\e\C-e" 'cmd-next-label)
  (define-key cmd-local-map "\M-\C-e" 'cmd-next-label)

  ;; メニューバーの作成
  ;; Create the menu bar.
  (define-key cmd-submenu-jump [sub-goto-label]
    '(;; "任意のラベル..."
      "Specified label" . cmd-goto-label))
  (define-key cmd-submenu-jump [sub-next-label]
    '(;; "次のラベル"
      "Next label" . cmd-next-label))
  (define-key cmd-submenu-jump [sub-prev-label]
    '(;; "前のラベル"
      "Previous label" . cmd-prev-label))

  (define-key cmd-local-map [menu-bar cmd] (cons mode-name cmd-menu-bar))
  (define-key cmd-menu-bar [submenu-jump] (cons "Jump" ;; "ジャンプ"
						cmd-submenu-jump))
  (define-key cmd-menu-bar [sep-1] '("--"))
  (define-key cmd-menu-bar [help] '("Help" ;; "ヘルプ..."
				    . cmd-help))
  (define-key cmd-menu-bar [sep-2] '("--"))
  (define-key cmd-menu-bar [recenter] '("Delete trailing spaces"
					;; "行末の空白文字削除"
					. cmd-recenter))
  (define-key cmd-menu-bar [fill] '("Fill comments" ;; "コメントの行詰め"
				    . cmd-fill-paragraph))
  (define-key cmd-menu-bar [exec] '("実行" . cmd-exec))

  ;; ローカルマップの使用宣言
  ;; Declare to use of the local map
  (use-local-map cmd-local-map)

  ;; font-lockの設定
  ;; Setting for font-lock.
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
        '((cmd-font-lock-keywords) t t))
  (font-lock-mode t)

  (run-hooks 'cmd-mode-hook)
)

;; Added by L Borgman
(provide 'cmd-mode)

;;; cmd-mode.el ends here
