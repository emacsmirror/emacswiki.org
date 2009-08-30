;;; configures and codes for tex mode

(add-to-list 'load-path (concat emacs-site-lisp-dir "auctex"))

(add-hook 'latex-mode-hook 'turn-on-reftex)
(require 'tex-site)
(require 'tex-mik)
;; (require 'preview)
;; (require 'out-xtra)

(setq TeX-auto-save t)
(setq TeX-parse-self t)


(defun wuxch-latex-mode-setup-key ()
  "wuxch-latex-mode-setup-key:"
;;;   (define-key outline-minor-mode-map [(control left)] 'outline-promote)
;;;   (define-key outline-minor-mode-map [(control right)] 'outline-demote)

  (local-set-key [(f5)] 'latex-syntax-pdf)
  (local-set-key [(f6)] 'run-gbk2uni)

  (set (make-local-variable 'compile-command) "make -s -r")

  (local-set-key [(f5)] 'wuxch-compile-make-pdf)
  (local-set-key [(control f5)] 'wuxch-compile-make-re)
  (local-set-key [(control f7)] 'wuxch-compile-make-rm)
  (local-set-key [(f7)] 'wuxch-compile-make-current)
  ;; (local-set-key [(f8)] 'phonetic-add)
  (local-set-key [(control y)]   'phonetic-add)

  (local-set-key [(control c)(i)] 'latex-textit-paragraph)
  (local-set-key [(control c)(c)] 'check-latex-char)
  (local-set-key [(control c)(d)] 'wuxch-tex-vocabulary-current-word-insert)
  (local-set-key [(control e)] 'wuxch-insert-environment)
  ;; (local-set-key [tab]  'TeX-complete-symbol)
  (local-set-key [tab]  'wuxch-expand-or-insert-tab)
  ;; (local-set-key [(control b)]  'ignore)
  ;; (local-set-key [(control b)]  'wuxch-latex-bold)
  (local-set-key [(meta c)]     'wuxch-latex-c-code)
  (local-set-key [(control c)(\,)] 'wuxch-point-stack-push)
  (local-set-key [(control c)(\.)] 'wuxch-point-stack-pop)

  (local-set-key [(control c)(control v)] 'wuxch-view-pdf)

  (local-set-key [(control c)(return)] 'ignore)
  (local-set-key [(control c)(return)] 'LaTeX-insert-item)

  (local-set-key [(control c)(l)]   'ignore)
  (local-set-key [(control c)(l)]   'lettrine)
  )

(defun wuxch-environment-to-string (input-str)
  "wuxch-environment-to-string:"
  (cond
   ((or (string= input-str "code")(string= input-str "c"))
    (cons (concat "\\begin{lstlisting}\n") (concat "\\end{lstlisting}\n"))
    )
   ((or (string= input-str "itshape")(string= input-str "i"))
    (cons (concat "\\begin{itshape}\n") (concat "\\end{itshape}\n"))
    )
   ((or (string= input-str "snippet")(string= input-str "s"))
    (cons (concat "\\lstinline$") (concat "$"))
    )
   ((or (string= input-str "bold")(string= input-str "b"))
    (cons (concat "\\textbf{") (concat "}"))
    )
   ((or (string= input-str "emph")(string= input-str "e"))
    (cons (concat "\\emph{") (concat "}"))
    )
   (t
    (cons (concat "\\" input-str "{") (concat "}"))
    )
   )
  )

(defun wuxch-insert-environment (name)
  "wuxch-insert-environment:"
  (interactive "sEnvironment Name:")
  (let* ((begin-pos (point))(end-pos begin-pos)
         (insert-str)(return-pos))
    (if mark-active
        (progn
          (setq begin-pos (region-beginning))
          (setq end-pos (region-end))
          )
      )
    (setq insert-str (wuxch-environment-to-string name))
    (goto-char begin-pos)
    (insert (car insert-str))
    (goto-char (+ end-pos (length (car insert-str))))
    (setq return-pos (point))
    (insert (cdr insert-str))
    (goto-char return-pos)
    )
  )

(defun wuxch-compile-make-pdf ()
  "wuxch-compile-make-pdf:"
  (interactive)
  (wuxch-compile-make-arg "pdf")
  )

(defun wuxch-change-file-name-suffix (file-name suffix)
  "wuxch-change-file-name-suffix:"
  (let ((file-name-no-dir (file-name-nondirectory file-name)))
    (concat (substring file-name-no-dir 0
                       (- (length file-name-no-dir) (length (file-name-extension file-name-no-dir))))
            suffix)
    )
  )

(defun wuxch-check-makeindex-flag ()
  "wuxch-check-makeindex-flag:"
  (let ((make-index-string "^\\\\makeindex")(current-pos (point))(ret))
    (goto-char (point-min))
    (setq ret (search-forward-regexp make-index-string nil t))
    (goto-char current-pos)
    (if (eq ret nil)
        nil
      t
      )))

(defun wuxch-is-master-tex-file ()
  "wuxch-is-master-tex-file:"
  (let ((file-name-no-dir (file-name-nondirectory (buffer-file-name))))
    (string= file-name-no-dir (concat (TeX-master-file) ".tex"))
    )
  )

(defun wuxch-compile-make-current-master ()
  "wuxch-compile-make-current-master:"
  (let ((make-target (wuxch-change-file-name-suffix (buffer-file-name) "pdf")))
    (if (wuxch-check-makeindex-flag)
        (wuxch-compile-make-arg (concat make-target " index=on"))
      (wuxch-compile-make-arg (concat make-target))
      )
    )
  )

(defun wuxch-compile-make-current-slave ()
  "wuxch-compile-make-current-slave:"
  (let ((master-file-name (concat (TeX-master-file) ".tex")))
    (wuxch-compile-make-arg (concat "master file=" master-file-name))
    )
  )

(defun wuxch-compile-make-current ()
  "wuxch-compile-make-current:"
  (interactive)
  (if (wuxch-is-master-tex-file)
      (wuxch-compile-make-current-master)
    (wuxch-compile-make-current-slave)
    )
  )


(defun wuxch-expand-or-insert-tab ()
  "wuxch-expand-or-insert-tab:"
  (interactive)
  (if (and (looking-at "$") (not (looking-back "^\\s-*")))
      (hippie-expand nil)
    (tab-to-tab-stop)
    )
  )

(defun wuxch-latex-pair-setup ()
  "wuxch-latex-pair-setup:"

  ;;输入左边的括号，就会自动补全右边的部分.包括(), "", [] , {} , 等等。
  (make-local-variable 'skeleton-pair-alist)
  (setq skeleton-pair-alist  '(
                               (?\(  _ ")")
                               (?\[  _ "]")
                               (?\" _ "\"")
                               (?\'  _ "'")
                               (?{  _ "}")
                               ))
  (setq skeleton-pair t)
  (local-set-key (kbd "(") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "{") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "[") 'skeleton-pair-insert-maybe)
  ;; (local-set-key (kbd "\"") 'skeleton-pair-insert-maybe) ;;we need latex's double quote
  )

(defun wuxch-latex-c-code ()
  "wuxch-latex-c-code:"
  (interactive)
  (let ((current-pos (point)))
    (if mark-active
        (let ((start-pos (region-beginning))
              (end-pos (region-end))
              (bold-string-prefix "\\begin{lstlisting}\n"))
          (deactivate-mark)
          (goto-char start-pos)
          (insert bold-string-prefix)
          (goto-char (+ end-pos (length bold-string-prefix)))
          (insert "\n\\end{lstlisting}")
          )
      (progn
        (insert "list_")
        )
      )
    ;; (goto-char current-pos)
    )
  )

(defun wuxch-latex-bold ()
  "wuxch-latex-bold:"
  (interactive)
  (let ((current-pos (point)))
    (if mark-active
        (let ((start-pos (region-beginning))
              (end-pos (region-end))
              (bold-string-prefix "\\textbf{"))
          (deactivate-mark)
          (goto-char start-pos)
          (insert bold-string-prefix)
          (goto-char (+ end-pos (length bold-string-prefix)))
          (insert "}")
          )
      (progn
        (insert "\\textbf{}")
        )
      )
    (fill-paragraph t)
    (goto-char current-pos)
    )
  )


(defun wuxch-latex-outline-minor-mode ()
  "wuxch-latex-outline-minor-mode:"
  (interactive)
  (outline-minor-mode 1)
  (set (make-local-variable 'outline-regexp)
       (concat "^\\("
               "\\\\section"          "\\|"
               "\\\\subsection"       "\\|"
               "\\\\subsubsection"    "\\|"
               "\\\\documentclass"    "\\|"
               "\\\\begin{document}"  "\\|"
               "\\\\end{document}"    "\\|"
               "\\\\include{"         "\\|"
               "\\\\vocabulary"       "\\|"
               "\\\\pagebreak"        "\\|"
               "\\\\makeindex"        "\\|"
               "\\\\printindex"       "\\|"
               "\\\\begin{CJK"        "\\|"
               "\\\\begin{ztetable"   "\\|"
               "\\\\begin{ztefigure"  "\\|"
               "\\\\begin{itemize"    "\\|"
               "\\\\begin{enumerate"  "\\|"
               "\\\\begin{frame"      "\\|"
               "\\\\bibliography"     "\\|"
               "\\\\end{CJK"
               "\\).*"))
  (set (make-local-variable 'outline-level) 'wuxch-latex-outline-level)
  ;; Cause use of ellipses for invisible text.
  (add-to-invisibility-spec '(outline . t))

  (define-key outline-minor-mode-map [(control x) (left)] 'hide-entry)
  (define-key outline-minor-mode-map [(control x) (right)] 'show-entry)
  (define-key outline-minor-mode-map [(control meta left)] 'hide-body)
  (define-key outline-minor-mode-map [(control meta right)] 'show-all)
  )

 (defun wuxch-latex-outline-level ()
    (looking-at outline-regexp)
    (let ((match (match-string 1)))
      (cond
       ((eq match "\\\\section" ) 1)
       ((eq match "\\\\include{" ) 1)
       ((eq match "\\\\vocabulary" ) 1)
       ((eq match "\\\\pagebreak" ) 1)
       ((eq match "\\\\makeindex" ) 1)
       ((eq match "\\\\printindex" ) 1)
       ((eq match "\\\\subsection") 2)
       ((eq match "\\\\subsubsection") 3)
       ((eq match "\\\\documentclass" ) 1)
       ((eq match "\\\\begin{document}" ) 1)
       ((eq match "\\\\end{document}" ) 1)
       ((eq match "\\\\begin{CJK" ) 1)
       ((eq match "\\\\end{CJK" ) 1)
       ((eq match "\\\\begin{ztetable" ) 4)
       ((eq match "\\\\begin{ztefigure" ) 4)
       ((eq match "\\\\begin{itemize" ) 4)
       ((eq match "\\\\begin{enumerate" ) 4)
       ((eq match "\\\\begin{frame" ) 4)
       ((eq match "\\\\bibliography" ) 1)
       (t 3)
       )))

(defun wuxch-latex-command-setup ()
  "wuxch-latex-command-setup:"

  (custom-set-variables
   ;; custom-set-variables was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(TeX-command-list
     (quote
      (
       ;; if tex-master-file is a.tex then:
       ;; %t -- a.tex     %d -- a.dvi     %s -- a
       ;; %f -- a.ps
       ("latex" "latex.exe %t" TeX-run-TeX nil (latex-mode doctex-mode) :help "Run latex.exe")
       ("xelatex" "xelatex.exe -synctex=1 %t" TeX-run-TeX nil (latex-mode doctex-mode) :help "Run Xetex.exe")
       ;; ("dvi2pdf" "dvipdfmx.exe -v %d" TeX-run-TeX nil (latex-mode doctex-mode) :help "Run dvipdfmx.exe")
       ("edvi2ps" "dvips.exe %d" TeX-run-TeX nil (latex-mode doctex-mode) :help "Run dvips.exe")
       ("ps2pdf14" "ps2pdf14.bat %f" TeX-run-TeX nil (latex-mode doctex-mode) :help "Run ps2pdf14.bat")
       ("tex2pdf" "pdflatex.exe %s" TeX-run-TeX nil (latex-mode doctex-mode) :help "Run pdflatex.exe")
       ("toc" "gbk2uni.exe %s" TeX-run-TeX nil (latex-mode doctex-mode) :help "Run gbk2uni.exe")
       ("wuxch-batch" "wuxchbatch %s" TeX-run-TeX nil (latex-mode doctex-mode) :help "Run wuxch batch")
       ("BibTex" "bibtexutf8.bat %s" TeX-run-BibTeX nil t :help "Run BibTeX")
       ("Original BibTex" "bibtex %s" TeX-run-BibTeX nil t :help "Run BibTeX")
       ("View" "wuxch-view-pdf" TeX-run-function nil t :help "Run Viewer")
       ;; ("Queue" "%q" TeX-run-background nil t :help "View the printer queue" :visible TeX-queue-command)
       ("Index" "makeindex.exe %s" TeX-run-command nil t :help "Create index file")
       ("Clean" "TeX-clean" TeX-run-function nil t :help "Delete generated intermediate files")
       ("All Clean" "(TeX-clean t)" TeX-run-function nil t :help "Delete all generated files")
       ("delete" "pdfclose --file %s.pdf" TeX-run-TeX nil (latex-mode doctex-mode) :help "Close pdf file")
       )
      )
     )
   )
  (setq LaTeX-clean-intermediate-suffixes
        (append LaTeX-clean-intermediate-suffixes (list "\\.synctex\\.gz")))
  (setq TeX-command-default "xelatex")
  (setq TeX-save-query   nil)
  ;; (setq TeX-show-compilation t)
  )

;; By default, AUCTeX shares its abbreviations with the major mode text-mode. This means that
;; abbreviations saved with C-x a l (add-mode-abbrev) are saved in the table text-mode-abbrev-table
;; and become available in all buffers using text-mode. This may not be the expected behavior.
(define-abbrev-table 'TeX-mode-abbrev-table ())

(defun wuxch-latex-ref-setup ()
  "wuxch-latex-ref-setup:"
  ;; (LaTeX-add-environments
  ;;  '("ztetable" LaTeX-env-label)
  ;;  '("ztetablelong" LaTeX-env-label)
  ;;  '("ztefigure" LaTeX-env-label))
  (setq reftex-label-alist
        '(
          ("ztefigure"    ?f "fig:" "~\\ref{%s}" caption
           (regexp "figure?[sn]?" "figs?\\." "Abbildung\\(en\\)?" "Abb\\."))
          ;; ("ztetable"     ?t "tab:" "~\\ref{%s}" caption
          ;;  (regexp "table?" "tab\\." "Tabellen?"))
          ("\\begin{ztetable}{}{*}{}" ?t nil nil 1)
          ("\\begin{ztelongtable}{}{*}{}" ?t nil nil 1)
          )
        )
  )

(defun wuxch-latex-mode-hook ()
  ;; (TeX-fold-mode 1)
  (TeX-PDF-mode 1)
  ;; (setq TeX-DVI-via-PDFTeX t)
  (auto-fill-mode)
  ;; (flyspell-mode)
  (abbrev-mode 1)
  (setq local-abbrev-table TeX-mode-abbrev-table)

  (reftex-mode)
  ;; (tex-source-specials-mode)
  (wuxch-latex-outline-minor-mode)
  (wuxch-latex-mode-setup-key)
  (wuxch-latex-pair-setup)
  (wuxch-latex-command-setup)
  (wuxch-latex-ref-setup)
  (modify-syntax-entry '\ "w")

  )
(add-hook 'LaTeX-mode-hook 'wuxch-latex-mode-hook)


(copy-face	'font-lock-string-face	'font-latex-verbatim-face)

(defun wuxch-view-pdf ()
  "wuxch-view-pdf:"
  (interactive)
  (let* ((tex-file (concat (TeX-master-file nil nil nil) ".tex"))
         (pdf-file (concat (TeX-master-file nil nil nil) ".pdf"))
         (pdf-dir (file-name-directory (buffer-file-name)))
         (synctex-file (concat (TeX-master-file nil nil nil) ".synctex.gz"))
         (line-num (line-number-at-pos))
         (start (point-min))
         (cmd)
         )

    ;; we must consider narrow, in this case, we should and line-number of start then minus 1
    (unless (= start 1)
      (save-excursion
        (save-restriction
          (widen)
          (setq line-num (+ line-num (line-number-at-pos start) -1))
          )
        )
      )


    ;; (w32-browser (dired-replace-in-string "/" "\\" (concat pdf-dir pdf-file)))
    (cd pdf-dir)
    ;; (if (file-exists-p synctex-file)
    ;;     (shell-command (concat "gunzip -d " synctex-file))
    ;;   )
    ;; (setq cmd (format "synctex view -i %d" line-num))
    (setq cmd (format "wuxch_synctex.exe view -i %d:0:%s -o %s" line-num tex-file pdf-file))
    (setq cmd (concat cmd " -x \"pdfopen --file %{output} --page %{page+1}\""))
    ;; (message "cmd is %s" cmd)
    (wuxch-shell-command-background cmd)
    ;; (shell-command cmd)
    )
  )

(defun wuxch-latex-pdf ()
  "wuxch-latex-pdf-and-view:"
  (interactive)
  ;; (remove-syntaxonly)
  ;; (save-buffer)
  (TeX-command "LaTeX" 'TeX-master-file -1)
  )

(defun temp-add-highlight (string-length)
  "temp-add-highlight:"
  (overlay-put (make-overlay (- (point) string-length) (point) (current-buffer)) 'face 'highlight)
  )

(defun temp-remove-highlight (string-length)
  "temp-remove-highlight:"
  (remove-overlays (- (point) string-length) (point) 'face 'highlight)
  )

(defun string-fit-regex (str reg)
  "string-fit-regex:"
  (not (eq nil (string-match reg str)))
  )

(defun wuxch-qurey (prompt)
  "wuxch-yes-or-no:"
  (interactive)
  (let ((input (read-char prompt)))
    (lookup-key query-replace-map (vector input))
    ;; using:
;;;     (cond ((eq def 'help)
;;; 			 ...)
;;;           'act 'act-and-exit 'act-and-show 'automatic
    ;; 'skip 'recenter 'edit 'edit-replacement 'delete-and-edit
    )
  )

(defun update-latex-char ()
  "update-latex-char:"
  (let ((check-regex)(matched)(matched-length)
        (original-double-quote "[\"“”]")
        (original-single-quote "’")
        (original-bar "—")
        (original-french-e "é")
        (original-add-slash-char "[%&_#$]")
        (original-add-dolloar-char "[<>]")
        (original-dots "\\.[ ]*\\.[ ]*\\.")
        )
    (setq check-regex (concat "\\("
                              "\\("     original-double-quote	    "\\)"
                              "\\|\\("  original-single-quote	    "\\)"
                              "\\|\\("  original-bar                "\\)"
                              "\\|\\("  original-french-e           "\\)"
                              "\\|\\("  original-add-slash-char     "\\)"
                              "\\|\\("  original-add-dolloar-char	"\\)"
                              "\\|\\("  original-dots	            "\\)"
                              "\\)"))
    (message "check-regex is %s" check-regex)
    (while (re-search-forward check-regex nil t)
      (progn
        (setq matched (match-string 0))
        (setq matched-length (length matched))
        (temp-add-highlight matched-length)
        (if (y-or-n-p (concat "update [" matched "] ?"))
            (progn
              (backward-char matched-length)
              (delete-char matched-length)
              (cond
               ((string-fit-regex matched original-double-quote)
                (TeX-insert-quote force)
                )
               ((string-fit-regex matched original-single-quote)
                (insert "'")
                )
               ((string-fit-regex matched original-bar)
                (insert "--")
                )
               ((string-fit-regex matched original-french-e)
                (insert "\\'e")
                )
               ((string-fit-regex matched original-add-slash-char)
                (insert (concat "\\" matched))
                )
               ((string-fit-regex matched original-add-dolloar-char)
                (insert (concat "\$" matched "\$"))
                )
               ((string-fit-regex matched original-dots)
                (insert "\\ldots")
                )
               )
              )
          )
        (temp-remove-highlight matched-length)
        )
      )
    )
  )

(defun latex-textit-paragraph ()
  "latex-textit-paragraph:"
  (interactive)
  ;; (move-beginning-of-line nil)
  (move-end-of-line nil)
  (start-of-paragraph-text)
  (insert "\\textit\{")
  ;; (move-end-of-line nil)
  (end-of-paragraph-text)
  (insert "\}")
  (fill-paragraph t)
  )

(defun remove-syntaxonly ()
  "remove_syntaxonly:"
  (interactive)
  (let* ((current_position (point)))
    ;; (beginning-of-buffer)
    (flush-lines "\\(\\usepackage{syntonly}\\)\\|\\(\\\\syntaxonly\\)" (point-min) (point-max))
    (goto-char current_position)
    )
  )

(defun add-syntaxonly ()
  "add-syntaxonly:"
  (let* ((current_position (point))
         (documentclass-str "\\documentclass")
         (syntaxonly-str "\\usepackage{syntonly}")
         (syntaxonly-str-all (concat "\n" syntaxonly-str "\n" "\\syntaxonly")))
    (beginning-of-buffer)
    (if (not (re-search-forward syntaxonly-str nil t))
        ;; if there isn't syntonly package, then add it.
        (progn
          (if (re-search-forward documentclass-str nil t)
              (progn
                (move-end-of-line nil)
                (insert syntaxonly-str-all)
                (setq current_position (+ current_position (length syntaxonly-str-all)))
                )
            )
          )
      )
    (goto-char current_position)
    )
  )

(defun latex-syntax-check ()
  "latex-compile:"
  (interactive)
  (add-syntaxonly)
  (save-buffer)
  (wuxch-latex-pdf)
  )

(defun latex-syntax-pdf ()
  "latex-compile:"
  (interactive)
  (remove-syntaxonly)
  (save-buffer)
  (wuxch-latex-pdf)
  )

(defun run-gbk2uni ()
  "run-gdbuni:"
  (interactive)
  (let* ((current-file-name (buffer-file-name))
         (tex-master-file-name (TeX-master-file nil nil nil))
         (out-file-name (concat (file-name-directory current-file-name) tex-master-file-name ".out"))
         (gbk2uni-command-str "gbk2uni"))
    ;; (message "out-file-name is %s" out-file-name)
    (wuxch-shell-command-background (concat gbk2uni-command-str " " out-file-name))
    )
  )

(require 'wuxch-phonetic)
(require 'thingatpt+)

(require 'wuxch-cua)
(defun phonetic-add (word phonetic term explan)
  "latex-add-phonetic:"
  (interactive (list (read-from-minibuffer
                      (concat "Word to add"
                              (let ((str (thing-at-point 'word)))
                                (when str (concat " (default \"" str "\")")))
                              ": "))
                     (read-from-minibuffer "Phonetic:")
                     (read-from-minibuffer "Term:")
                     (read-from-minibuffer "Explan:")
                     ))
  (if (string= word "")
      (setq word (thing-at-point 'word))
    )
  (let* ((string-latex (do-phonetic-translate phonetic))
         (current-pos))
    (move-to-end-of-word)
    (setq current-pos (point))
    (insert (concat "\\index{" word "}"))
    (insert (concat "\\hypertarget{" word "}{}"))
    (insert (concat "\\footnote{\\textbf{" word "}:"))
    (insert (concat string-latex ". "))
    (insert (concat "\\textit{" term "}. "))
    (insert (concat explan "\\hyperlink{" word "}{\\textasciicircum}}"))
    (fill-paragraph t)
    (goto-char current-pos)
    )
  )

(defvar wuxch-special-char-map
  '(("\"" . "Tex-insert-quote") ;; " should be treated differently
    ;; 需要加一个 \ 的
    ("%"  . "\\\\%")  ("&"  . "\\\\&")    ("#"  . "\\\\#")
    ("_"  . "\\\\_")  ("\\$" . "\\\\$") ("€" . "\\\\texteuro")
    ;; ("{" . "\\\\{") ("}" . "\\\\}")
    ;; 需要加$$的
    ("@" . "$@$") ("\\[" . "$[$") ("\\]" . "$]$")
    ("<" . "$<$") (">" . "$>$")
    ;; 其他
    ("é" . "\\\\'e") ("Á" . "\\\\'A") ("ó" . "\\\\'o") ("&"  . "\\\\pounds")
    ("ä" . "\\\\\"a") ("ï" . "\\\\\"i") ("£" . "\\\\textsterling")
    ("ñ" . "\\\\~{n}") ("â" . "\\\\^{a}") ("á" . "\\\\'a") ("û" . "\\\\^{u}")
    ("è" . "\\\\`e") ("ë" . "\\\\\"e")

    ;; 需要特殊处理的
    ("\\.[ ]*\\.[ ]*\\." . "\\ldots")
    ;; ((evaql wuxch-not-sentence-over-char) . "special")
    ;; 中文全角字符
    ("１" . "1") ("２" . "2")  ("３" . "3") ("４" . "4")    ("５" . "5")
    ("６" . "6") ("７" . "7")  ("８" . "8") ("９" . "9")    ("Ａ" . "A")
    ("Ｂ" . "B") ("Ｃ" . "C")  ("Ｄ" . "D") ("Ｅ" . "E")    ("Ｆ" . "F")
    ("Ｇ" . "G") ("Ｈ" . "H")  ("Ｉ" . "I") ("Ｊ" . "J")    ("Ｋ" . "K")
    ("Ｌ" . "L") ("Ｍ" . "M")  ("Ｎ" . "N") ("Ｏ" . "O")    ("Ｐ" . "P")
    ("Ｑ" . "Q") ("Ｒ" . "R")  ("Ｓ" . "S") ("Ｔ" . "T")    ("Ｕ" . "U")
    ("Ｖ" . "V") ("Ｗ" . "W")  ("Ｘ" . "X") ("Ｙ" . "Y")    ("Ｚ" . "Z")
    ("ａ" . "a") ("ｂ" . "b")  ("ｃ" . "c") ("ｄ" . "d")    ("ｅ" . "e")
    ("ｆ" . "f") ("ｇ" . "g")  ("ｈ" . "h") ("ｉ" . "i")    ("ｊ" . "j")
    ("ｋ" . "k") ("ｌ" . "l")  ("ｍ" . "m") ("ｎ" . "n")    ("ｏ" . "o")
    ("ｐ" . "p") ("ｑ" . "q")  ("ｒ" . "r") ("ｓ" . "s")    ("ｔ" . "t")
    ("ｕ" . "u") ("ｖ" . "v")  ("ｗ" . "w") ("ｘ" . "x")    ("ｙ" . "y")
    ("ｚ" . "z") ("“" . "``") ("”" . "''") ("？" . "?")    ("！" . "!")
    ("（" . "(") ("）" . ")")  ("－" . "-")  ("％" . "\\\\%")("‘" . "'")
    ("’" . "'") ("—" . "--")
    ))


(defvar wuxch-not-sentence-over-char nil)
(defun wuxch-build-not-sentence-over-char ()
  "wuxch-build-not-sentence-over-char:"
  (let ((str))
    (setq str "\\(")
    ;; title
    (setq str (concat str "Mr\\|Mrs\\|Ms\\|Dr\\|Jr\\|J\\|Capt\\|Lt\\|Col"))
    ;; Month
    (setq str (concat str "\\|Jan\\|Feb\\|Mar\\|Apr\\|Jun\\|Jul\\|Aug\\|Sep\\|Sept\\|Oct\\|Nev\\|Dec"))
    ;; Commercial
    (setq str (concat str "\\|Co\\|Corp\\|Inc\\|Ltd"))
    ;; Country
    (setq str (concat str "\\|U\\.S\\|U\\.S\\.A"))
    ;; abbrev
    (setq str (concat str "\\|No"))
    ;; midname
    (setq str (concat str "\\|[ ][A-Z]"))
    (setq str (concat str "\\)\\.[ ]"))
    (setq wuxch-not-sentence-over-char str)
    )
  )

(wuxch-build-not-sentence-over-char)


(setq wuxch-special-char-map
      (append wuxch-special-char-map  (list (cons wuxch-not-sentence-over-char "special"))))

(defun wuxch-tex-vocabulary-current-word-insert (new-word)
  "vocabulary-current-word-insert:"
  (interactive
   (let* ((str (read-string "insert word:" (word-at-point))))
     (list str)))
  (insert (concat "\\cite{" new-word "}"))
  (fill-paragraph t)
  (do-vocabulary-insert new-word)
  )

(defun wuxch-build-regexp-string (list-name)
  (let ((new-list (copy-alist list-name))
        (item)(regexp-string "\\(")
        )
    (while (not (eq new-list nil))
      (setq item (car (pop new-list)))
      (if (not (eq item nil))
          (progn
            (setq regexp-string (concat regexp-string "\\(" item "\\)\\|" ))
            )
        )
      )
    ;; delete last "\\|" str
    (setq regexp-string (substring regexp-string 0 (- (length regexp-string) 2)))
    (setq regexp-string (concat regexp-string "\\)"))
    ))

(defun latex-replace-double-quote-with-latex-quote ()
  (backward-char 1)
  (delete-char 1)
  (TeX-insert-quote nil)
  )


(defun y-or-n-ex (prompt)
  "y-or-n-ex:"
;;;     (cond
;;;      ((eq ret 'quit)
;;;       (message "quit" ))
;;;      ((eq ret 'act)
;;;       (message "act" ))
;;;      )
;;;   The valid answers include `act', `skip', `act-and-show',
;;; `exit', `act-and-exit', `edit', `delete-and-edit', `recenter',
;;; `automatic', `backup', `exit-prefix', and `help'.

  (lookup-key query-replace-map (vector (read-event prompt)))
  )

(defun latex-special-char-replace-string (matched string-for-replace)
  (cond
   ((string= matched "\"")
    (latex-replace-double-quote-with-latex-quote))
   ((string-fit-regex matched "\\.[ ]*\\.[ ]*\\.")
    (progn
      (backward-char (length matched))
      (delete-char (length matched))
      (insert "\\ldots")
      )
    )
   ((string-fit-regex matched wuxch-not-sentence-over-char)
    (progn
      (backward-char 1)
      (delete-char 1)
      (insert "~")
      )
    )
   (t
    (replace-match string-for-replace))
   )
  )

(defun latex-special-char ()
  (interactive)
  (let ((regex-string (wuxch-build-regexp-string wuxch-special-char-map))
        (string-for-replace)(matched)(matched-length)(item)(ret)
        (loop-flag t)
        ;; 设定大小写敏感，这样可以替换Ms.，而不是systems.了。
        (case-fold-search nil)
        )
    (while (and loop-flag (re-search-forward regex-string nil t))
      (setq matched (match-string 0))
      (setq item (assoc matched wuxch-special-char-map))
      (if (eq nil item)
          (setq item (assoc (concat "\\" matched) wuxch-special-char-map))
        )
      (setq string-for-replace (cdr item))
      (if (eq nil string-for-replace)
          (setq string-for-replace "special treat")
        )

      (if (not (eq ret 'automatic))
          (setq ret (y-or-n-ex (concat "replace \"" matched "\" with " string-for-replace "?")))
        )

      (cond
       ((eq ret 'quit)
        (setq loop-flag nil))

       ((eq ret 'skip)
        nil)

       ((eq ret 'act)
        (latex-special-char-replace-string matched string-for-replace))

       ((eq ret 'act-and-exit)
        (progn
          (latex-special-char-replace-string matched string-for-replace)
          (setq loop-flag nil)
          )
        )

       ((eq ret 'automatic)
        (progn
          (latex-special-char-replace-string matched string-for-replace)
          )
        )
       )
      )
    ret
    )
  )

(defun check-latex-char ()
  "check-latex-char:"
  (interactive)
  (let ((start-pos (point))(ret))
    (setq ret (latex-special-char))
    (if (or (eq ret 'act-and-exit) (eq ret 'automatic))
        (fill-region start-pos (point))
      (if (y-or-n-p "fill paragraph?")
          (fill-region start-pos (point))
        )
      )
    (goto-char start-pos)
    )
  )

(defun zte-table ()
  "zte-table:"
  (interactive)
  ;; remove last empty line
  (goto-char (point-max))
  (if (eq (line-end-position) (line-beginning-position))
      (delete-backward-char 1)
    )

  (goto-char (point-min))
  (while (re-search-forward "\\([[:alnum:]]+\\)" (line-end-position) t)
    (replace-match "\\\\ztehead{\\1}")
    )

  (goto-char (point-min))
  (while (re-search-forward "[\t]+" nil t)
    (replace-match " & ")
    )

  (goto-char (point-min))
  (while (re-search-forward "\n" nil t)
    (replace-match " \\\\\\\\\n")
    )

  (goto-char (point-min))
  (insert "\\begin{ztetable}{cap}{tab:}{m{.2\\textwidth}|m{.2\\textwidth}|m{.4\\textwidth}}\n")
  (insert "\\toprule\n")
  (beginning-of-line 2)
  (insert "\\midrule\n")

  (while (not (eq (line-number-at-pos (point)) (line-number-at-pos (point-max))))
    (beginning-of-line 2)
    (insert "\\hline\n")
    )

  (goto-char (point-max))
  (insert " \\\\\n\\bottomrule\n")
  (insert "\\end{ztetable}")
  )

(defun zte-enum ()
  "zte-enum:"
  (interactive)
  (goto-char (point-min))
  (insert "\\begin{enumerate}\n")
  (while (re-search-forward "^.*\t" nil t)
    (replace-match "  \\\\item ")
    )
  (goto-char (point-max))
  (insert "\\end{enumerate}")
  (kill-new (buffer-substring (point-min) (point-max)))
  )

(defun zte-list ()
  "zte-list:"
  (interactive)
  (let ((pos (point))
        (list-str "^.*\t"))

    )
  )

(defun zte-section ()
  "zte-section:"
  (interactive)
  (let ((case-fold-search nil)
        (pos (point)))
    (while (re-search-forward
            "^\\([0-9]+\\)\\(\\.[0-9]+\\)?\\(\\.[0-9]+\\)?\\(\\.[0-9]+\\)?\\(\\.[0-9]+\\)?\t\\(.*\\)" nil t)
      ;; (message "found:%s,%s,%s,%s" (match-string 1) (match-string 2) (match-string 3) (match-string 4))
      (cond
       ((match-string 5)
        (replace-match "\\\\subparagraph{\\6}"))
       ((match-string 4)
        (replace-match "\\\\paragraph{\\6}"))
       ((match-string 3)
        (replace-match "\\\\subsubsection{\\6}"))
       ((match-string 3)
        (replace-match "\\\\subsubsection{\\6}"))
       ((match-string 2)
        (replace-match "\\\\subsection{\\6}"))
       ((match-string 1)
        (replace-match "\\\\section{\\6}"))
       )
      )
    (goto-char pos)
    )
  )

(defun lettrine ()
  "latterine:"
  (interactive)
  (insert "\\lettrine{")
  (forward-char 1)
  (insert "}{")
  (forward-word 1)
  (insert "}")
  (fill-paragraph t)
  )

(provide 'wuxch-tex)
