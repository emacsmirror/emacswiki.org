;;; tategaki.el --- a simple 縦
;;                           書
;;                           き typesetter

;;; Copyright (C) 1999 Tamakoshi Hiroki <hiroki-t@is.aist-nara.ac.jp>

;;; Last modified on Wed Apr  4 01:57:41 2001

;; Author: Tamakoshi Hiroki <hiroki-t@is.aist-nara.ac.jp>
;; Maintainer: Tamakoshi Hiroki <hiroki-t@is.aist-nara.ac.jp>
;; Created: 19 Mar 1999
;; Version: 1.3
;; Keywords: wp

;;{{{ Licence

;; このソフトウェアは GNU 一般公有使用許諾書には従いません。GNU
;; General Public Licence に従う場合、プログラムは無保証(NO WARRANTY)
;; で提供されますが、ここがひっかかります。「このソフトウェアを使って
;; 何が起こっても私は知らないよ。」と言うわけですが、それでは物の作り
;; 手として責任がなさすぎます。私はこのソフトウェアについて、説明書の
;; 通りに動くであろうことについて責任を持ちます。例えばバグがある場合、
;; それは私の責任の下で発生していることになります。
;; ただしそれ以上の責任については負えません。例えば「おまえのソフトウェ
;; アを使ったら子供が出来た。どうしてくれる。」などです。もちろん世の
;; 中にはバタフライ効果というものがありますが、その場合は理論的根拠と
;; 十分で具体的な証拠の提示が必要です。

;;}}}

;;{{{ Introduction

;;{{{ Documentation:

;; 1. Set region, and invoke the program by typing M-x tategaki-region.
;; 2. Then it asks the number of letters per line, please answer.
;; 3. The result is what you will see.
;;
;; 1. 範囲を指定して、M-x tategaki-region して下さい。
;; 2. 一行の字数を聞いてくるので入力します。
;; 3. 縦書きに整形されます。

;;}}}

;;{{{ Features:

;; * ー を ｜ に、↑ を → にするなど 90 度回転したときに正しくなるように
;;   変換します。句読点なども同様です。
;; * 禁則処理を行います。
;; * 箇条書きの段下げを行います。
;; * 横書きのままにする文字列を指定出来ます。
;; * heuristic に段落分けを行います。
;;   具体的には行頭がスペースで始まるときと空行があるときです。
;; * 鈎括弧で囲まれた部分を台詞として独立した行にすることも出来るので、
;;   小説などに適しています。
;; * 元の文章での改行位置に従って縦書き時に改行することも出来るので、
;;   詩などの整形も可能です。
;; * 横幅がある場合は区切ってページごとに縦に並べることも出来ます。

;;}}}

;;{{{ Tips:

;; * HTML の中では等幅フォントならば <pre> </pre> で囲めば
;;   大体うまく見られるようです。
;;   文字間の隙間に関して若干の修正が必要な場合もあります。
;; * TeX では素直に pTeX を使いましょう。
;; * 縦書きのメールも乙なものです。

;;}}}

;;{{{ Bugs:

;; * 本気でパーズ、レイアウトを考えたために複雑なプログラムに
;;   なってしまいました。
;; * 変換不能な文字がかなりあります。例えば ( ) に対応する、
;;   縦書きに使える記号が存在しません(あれば教えて下さい)。

;;}}}

;;{{{ A simple sample

;;                                           せ の す な ？ き 日  　こ
;;                                           で 強 よ ん 縦 だ 本  　ん
;; こんにちは。                              す い  ゜て 書 と 語  　に
;;                                            ゜文 あ  ｀き 思 は  　ち
;; 日本語はやはり縦書きだと                   　化 る 病 が い や  　は
;; 思いませんか？                  →         　の い ん 出 ま は  　 ゜
;; 縦書きが出来ないなんて、                   　お は で 来 せ り  　 　
;; 病んでいますよ。あるいは                   　仕 西 い な ん 縦  　 　
;; 西欧の強い文化のお仕着せです。             　着 欧 ま い か 書  　 　

;;}}}

;;{{{ Feature Work:

;; * 縦書きにしたものを横書きに戻す機構を考える。
;; * HTML出力をより美しくする。
;;   (HTML出力は今のところおまけのようなものだと思って下さい)

;;}}}

;;{{{ History:

;; 1999年03月19日: 一から作り直してみる。
;; 1999年03月15日: なんとなく公開してみる。
;;                 http://mimi.aist-nara.ac.jp/%7Ehiroki-t/Distribution/Tategaki/

;;}}}

;;}}}

;;; Code:

;;{{{ variables and functions

;;{{{ customisable variables

(require 'cl)

(defvar tategaki-original
  "Emacs\\|NHK"
  "横書きのままになっていて欲しい文字列を指定するための正規表現。")

(defvar tategaki-itemize
  "^\\( *\\)\\(○\\|・\\|([0-9]*)\\|\\[[0-9]*\\]\\|[0-9]*\\.\\|([a-zA-z])\\|\\[[a-zA-z]\\]\\)"
  "箇条書きを指定するための正規表現。")

(defvar tategaki-original-limit 2
  "半角文字列も、この長さまでは横書きで表示します。")

(defvar tategaki-return-immediately-means-new-paragraph nil
  "この値が nil でない場合、
縦書きにしたときの改行位置は元の文章の改行位置に従います。
なので、漢詞などの詩において改行位置が重要なものは
t にするといいと思います。")

(defvar tategaki-bracket-make-new-line nil
  "この値が nil でないとき、鈎括弧部分を独立した行にします。
小説などでは有効でしょう。")

(defvar tategaki-fill "-"
  "この値が nil でないとき、
整形結果の横幅を `fill-column' の値以下にし、残りは下に続けます。
また、そのときこの値でページの区切り線を引きます。
従って、nil 以外の場合は文字列を指定して下さい。")

(defvar tategaki-brackets '(?「 . ?」)
  "括弧として扱う記号。
car 部が開き括弧で cdr 部が閉じ括弧。")

(defvar tategaki-default-length 20
  "一行の長さ。")

;;}}}

;;{{{ auxiliary functions

(defun tategaki-string-to-char-list (string)
  (cond ((string-match "20" emacs-version)
	 (mapcar 'identity string))
	(t
	 (string-to-char-list string))))

(defun tategaki-char-list-to-string (list)
  (apply 'concat
	 (mapcar (lambda (char)
		   (char-to-string char))
		 list)))
;   (cond ((string-match "20" emacs-version)
; 	 (char-list-to-string list))
; 	(t
; 	 (apply 'concat
; 		(mapcar (lambda (char)
; 			  (char-to-string char))
; 			list)))))

(defun tategaki-fill-line (tobe-filled-list number)
  (reverse (append (make-list (- (1+ number) (length tobe-filled-list)) ? )
		   tobe-filled-list)))

;;}}}

;;{{{ parsing and layouting engine

(defun tategaki-parse-hankaku-orig (subresult number end)
  (let ((limit (- number (length subresult)))
	(n 0))
    (while (and (< n limit) (< (point) end)
		(= 1 (char-width (char-after (point)))))
      (setq subresult (cons (char-after (point)) subresult)
	    n (1+ n))
      (forward-char))
    (if (= 1 (save-excursion (forward-char)
			     (if (char-after (point))
				 (char-width (char-after (point)))
			       0)))
	(cons t subresult)
      (cons nil subresult))))

(defun tategaki-parse-hankaku (hankaku-orig subresult number end)
  (let ((start (point))
	(n 0))
    (if hankaku-orig
	(tategaki-parse-hankaku-orig subresult number end)
      (let ((origp (save-excursion
		     (catch 'which
		       (while (and (<= n tategaki-original-limit)
				   (< (point) end))
			 (if (= 1 (char-width (char-after (point))))
			     (setq n (1+ n))
			   (throw 'which n))
			 (forward-char))
		       (throw 'which nil)))))
	(if origp
	    (let ((newsubresult (cons (tategaki-string-to-char-list (buffer-substring start (+ start n)))
				      subresult)))
	      (goto-char (+ start n))
	      (if (= 1 (save-excursion (forward-char) (char-width (char-after (point)))))
		  (cons t newsubresult)
		(cons nil newsubresult)))
	  (tategaki-parse-hankaku-orig subresult number end)
	  )))))

(defvar tategaki-kinsoku-letters
  '(?、 ?。 ?」))

(defun tategaki-parse (number start end)
  (let (orig-start orig-end
	itemize-start item-start item-end item-space-length item-space-length-pre
	(result '())
	(subresult '())
	(itemize-mode nil)
	(may-change-paragraph nil)
	(hankaku-orig nil)
	)
    (save-excursion
      (if (re-search-forward tategaki-original end t 1)
	  (setq orig-start (match-beginning 0)
		orig-end (match-end 0))))
    (save-excursion
      (if (re-search-forward tategaki-itemize end t 1)
	  (setq itemize-start (match-beginning 0)
		item-start (match-beginning 2)
		item-end (match-end 2)
		item-space-length-pre (- (match-end 1) (match-beginning 1)))))
    (while (< (point) end)
      (cond
       ((and orig-start
	     (= orig-start (point)))
	(setq subresult (cons (tategaki-string-to-char-list (buffer-substring orig-start orig-end)) subresult)
	      may-change-paragraph nil)
	(goto-char orig-end)
	(save-excursion
	  (if (re-search-forward tategaki-original end t 1)
	      (setq orig-start (match-beginning 0)
		    orig-end (match-end 0))))
	)
       ((and itemize-start
	     (= itemize-start (point)))
	(if subresult
	    (setq subresult (tategaki-fill-line subresult number)
		  result (cons subresult result)
		  subresult '()))
	(setq item-space-length item-space-length-pre
	      subresult (cons (tategaki-string-to-char-list (buffer-substring item-start item-end))
			      (make-list item-space-length ? ))
	      may-change-paragraph nil
	      itemize-mode t)
	(goto-char item-end)
	(save-excursion
	  (if (re-search-forward tategaki-itemize end t 1)
	      (setq itemize-start (match-beginning 0)
		    item-start (match-beginning 2)
		    item-end (match-end 2)
		    item-space-length-pre (- (match-end 1) (match-beginning 1)))))
	)
       ((and tategaki-bracket-make-new-line
	     (= (char-after (point)) (car tategaki-brackets)))
	(if subresult
	    (setq subresult (tategaki-fill-line subresult number)
		  result (cons subresult result)
		  subresult '()
		  may-change-paragraph nil
		  itemize-mode nil))
	(setq subresult (cons (char-after (point)) subresult))
	(forward-char)
	)
       ((and tategaki-bracket-make-new-line
	     (= (char-after (point)) (cdr tategaki-brackets)))
	(if subresult
	    (setq subresult (cons (char-after (point)) subresult)
		  subresult (tategaki-fill-line subresult number)
		  result (cons subresult result)
		  subresult '()
		  may-change-paragraph nil
		  itemize-mode nil))
	(forward-char)
	)
       ((< 1 (char-width (char-after (point))))
	(if (and (null subresult) itemize-mode)
	    (setq subresult (make-list (1+ item-space-length) ? )))
	(setq subresult (cons (char-after (point)) subresult)
	      may-change-paragraph nil)
	(forward-char)
	)
       ((= ?  (char-after (point)))
	(if (and (null subresult) itemize-mode)
	    (setq subresult (make-list (1+ item-space-length) ? )))
	(if may-change-paragraph
	    (setq subresult (tategaki-fill-line subresult number)
		  result (cons subresult result)
		  subresult '()
		  may-change-paragraph nil
		  itemize-mode nil
		  ))
	(setq subresult (cons (char-after (point)) subresult))
	(forward-char)
	)
       ((= ?\n (char-after (point)))
	(if tategaki-return-immediately-means-new-paragraph
	    (if subresult
		(setq subresult (tategaki-fill-line subresult number)
		      result (cons subresult result)
		      subresult '()
		      itemize-mode nil))
	  (if may-change-paragraph
	      (setq subresult (tategaki-fill-line subresult number)
		    result (cons subresult result)
		    subresult '()
		    result (cons (tategaki-fill-line '() number) result)
		    may-change-paragraph nil
		    itemize-mode nil)
	    (setq may-change-paragraph t)))
	(forward-char)
	)
       ((= 1 (char-width (char-after (point))))
	(let ((hankaku-parsed (tategaki-parse-hankaku hankaku-orig subresult number end)))
	  (setq hankaku-orig (car hankaku-parsed)
		subresult (cdr hankaku-parsed)
		may-change-paragraph nil))
	)
       (t ()))
      (cond ((and (= number (length subresult)) (< (point) end))
	     (if (memq (char-after (point)) tategaki-kinsoku-letters)
		 (progn
		   (setq subresult (reverse (cons (char-after (point)) subresult))
			 result (cons subresult result)
			 subresult '())
		   (forward-char))
	       (setq subresult (reverse (cons ?  subresult))
		     result (cons subresult result)
		     subresult '())))
	    ((= (1+ number) (length subresult))
	     (setq result (cons subresult result)
		   subresult '()))
	    (t ())))
    (if subresult
	(setq result (cons (tategaki-fill-line subresult number) result)))
    (reverse result)))

(defvar tategaki-tobe-converted-letters
  '(
    (?- . "|") (?| . "-") (?/ . "\\") (?\\ . "/")
    ; japanese characters
    (?。 . "゜") (?、 . "｀") (?ー . "｜") (?｜ . "—")
    (?→ . "↓") (?← . "↑") (?↑ . "→") (?↓ . "←")
    (?「 . "┐") (?」 . "└")
    (?＜ . "∧") (?＞ . "∨") (?： . "‥") (?‥ . "：")
    ; chinese characters
    )
  )

(defvar tategaki-offsets
  '((?。 . 1) (?、 . 1) (?「 . 1)
    (?」 . -1)))

(defun tategaki-layout-max-width (line)
  (let ((max 0))
    (while line
      (let* ((box (car line))
	     (boxwidth (if (listp box)
			   (string-width (tategaki-char-list-to-string box))
			 (char-width box))))
	(if (< max boxwidth)
	    (setq max boxwidth)))
      (setq line (cdr line)))
    max))

(defun tategaki-layout (strings start number)
  (let ((columns 0))
    (while strings
      (let ((aline (car strings)))
	(let ((maxwidth (tategaki-layout-max-width aline)))
	  (if (and tategaki-fill
		   (< fill-column (+ columns maxwidth)))
	      (progn
		(forward-line (1+ number))
		(if (= 0 (string-width tategaki-fill))
		    ()
		  (let ((n 0)
			(limit (/ fill-column (string-width tategaki-fill))))
		    (while (< n limit)
		      (insert tategaki-fill)
		      (setq n (1+ n)))))
		(newline) (newline)
		(setq start (point)
		      columns 0)))
	  (if (= 0 columns)
	      (progn
		(insert (make-string (+ number 2) ?\n)) ; ?
		(goto-char start)))
	  (setq columns (+ columns maxwidth 1))
	  (while aline
	    (let ((box (car aline)))
	      (if (listp box)
		  (let* ((word (tategaki-char-list-to-string box))
			 (word-width (string-width word))
			 (lwidth (/ (- maxwidth word-width) 2))
			 (rwidth (if (evenp (- maxwidth word-width))
				     (/ (- maxwidth word-width) 2)
				   (/ (1+ (- maxwidth word-width)) 2))))
		    (insert (make-string lwidth ? ))
		    (insert " ")
		    (insert word)
		    (insert (make-string rwidth ? ))
		    (forward-line 1))
		(let* ((convertedp (assq box tategaki-tobe-converted-letters))
		       (offsetp (assq box tategaki-offsets))
		       (word-width (char-width box))
		       (lwidth (/ (- maxwidth word-width) 2))
		       (rwidth (if (evenp (- maxwidth word-width))
				   (/ (- maxwidth word-width) 2)
				 (/ (1+ (- maxwidth word-width)) 2))))
		  (insert (make-string lwidth ? ))
		  (if offsetp
		      (cond ((= 1 (cdr offsetp))
			     (insert "  "))
			    ((= -1 (cdr offsetp))
			     ()))
		    (insert " "))
		  (if convertedp
		      (insert (cdr convertedp))
		    (insert (char-to-string box)))
		  (if offsetp
		      (cond ((= 1 (cdr offsetp))
			     (if (and (< (point) (point-max))
				      (= ?└ (char-after (point))))
				 (forward-char))
			     (if (and (< (point) (point-max))
				      (not (eolp)))
				 (delete-char 1)))
			    ((= -1 (cdr offsetp))
			     (insert " "))))
; 		(if (= 1 (char-width box))
; 		    (progn (insert "　")
; 			   (if (and (< (point) (point-max))
; 				    (= ?└ (char-after (point))))
; 			       (forward-char))
; 			   (if (< (point) (point-max))
; 			       (delete-char 1))))
		  (insert (make-string rwidth ? ))
		  (forward-line 1))))
	    (setq aline (cdr aline)))))
      (goto-char start)
      (setq strings (cdr strings)))))

(defun tategaki-region (start end)
  "指定した範囲を縦書きに組版します。
以下の特徴があります。
1. 縦書きにしたときに正しくなるように文字の変換を行う。
2. 禁則処理を行う。
3. 箇条書きの段下げを行う。
4. 横書きのままにする文字列を指定出来る。"
  (interactive "r")
  (let* ((default (number-to-string tategaki-default-length))
	 (number (string-to-number
		  (read-from-minibuffer "Line width: " default))))
    (if (< 0 number)
	(save-excursion
	  (goto-char start)
	  (let ((strings (tategaki-parse number start end)))
	    (delete-region start end)
	    (goto-char start)
	    (tategaki-layout strings start number)
	    (setq tategaki-default-length number))))))

;;}}}

;;{{{ additional functions for HTML output

(defun tategaki-layout-html (strings start number)
  (while strings
    (let ((aline (car strings)))
      (while aline
	(let ((box (car aline)))
	  (if (listp box)
	      (progn
		(insert "<td>")
		(insert (tategaki-char-list-to-string box))
		(insert "</td>")
		(forward-line 1))
	    (let ((convertedp (assq box tategaki-tobe-converted-letters))
		  (offsetp (assq box tategaki-offsets)))
	      (insert "<td>")
	      (if offsetp
		  (cond ((= 1 (cdr offsetp))
			 (insert " "))
			((= -1 (cdr offsetp))
			 ())))
	      (if convertedp
		  (insert (cdr convertedp))
		(insert (char-to-string box)))
	      (if offsetp
		  (cond ((= 1 (cdr offsetp))
			 ())
			((= -1 (cdr offsetp))
			 ())))
	      (insert "</td>")
	      (forward-line 1))))
	(setq aline (cdr aline))))
    (goto-char start)
    (setq strings (cdr strings))))

(defun tategaki-region-html (number start end)
  "指定した範囲を縦書きに整形し、HTML として出力します。
内部動作は `tategaki-region' と同じです。"
  (interactive "nLine width: \nr")
  (save-excursion
    (goto-char start)
    (let ((strings (tategaki-parse number start end))
	  (n 0))
      (delete-region start end)
      (insert "<table>\n")
      (setq start (point))
      (while (< n (1+ number))
	(insert "</tr>\n")
	(setq n (1+ n)))
      (goto-char start)
      (tategaki-layout-html strings start number)
      (goto-char start)
      (let ((n 0))
	(while (< n (1+ number))
	  (insert "<tr>")
	  (setq n (1+ n))
	  (forward-line 1)))
      (insert "</table>"))))

;;}}}

;;}}}

(provide 'tategaki)

;;; tategaki.el ends here
