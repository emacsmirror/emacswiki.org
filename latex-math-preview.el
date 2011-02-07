;;; latex-math-preview.el --- preview LaTeX mathematical expressions.

;; Author: Takayuki YAMAGUCHI <d@ytak.info>
;; Keywords: LaTeX TeX
;; Version: 0.5.4
;; Created: Mon Feb  7 11:08:36 2011
;; URL: http://www.emacswiki.org/latex-math-preview.el
;; Site: http://www.emacswiki.org/LaTeXMathPreview

;; latex-math-preview.el is a modified version which is based on
;; tex-math-preview.el and has been created at July 2009.
;; This emacs lisp is made by reducing some features of tex-math-preview.el
;; and adjusting it to LaTeX files.
;; tex-math-preview.el is made by Kevin Ryde and 
;; has some features which latex-math-preview does not have.
;; Please see http://user42.tuxfamily.org/tex-math-preview/index.html
;; for details of tex-math-preview.el.

;; Copyright 2006, 2007, 2008, 2009 Kevin Ryde
;; Copyright 2009, 2010, 2011 Takayuki YAMAGUCHI
;;
;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 3 of the License, or (at your option) any later 
;; version. 
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT 
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details. 
;; 
;; You should have received a copy of the GNU General Public License along with
;; this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; latex-math-preview creates images of particular region in LaTeX file
;; and display them in emacs.
;; latex-math-preview has the following main commands.
;; 
;; M-x `latex-math-preview-expression' previews mathematical expressions pointed
;; by cursor in LaTeX files or strings of selected region with transient-mark on.
;; The result of this command is shown as image in new buffer.
;; 
;; M-x `latex-math-preview-insert-symbol' displays the list of LaTeX symbols.
;; Selecting a LaTeX symbol from it, you can insert it.
;; Depending on whether cursor is in mathematical expression or not,
;; this commands choose the symbol list.
;; If you don't want to use the automatic mode selection,
;; alternatively you may use M-x `latex-math-preview-insert-mathematical-symbol'
;; and M-x `latex-math-preview-insert-text-symbol' for mathematical symbol
;; and normal text symbol, respectively.
;;
;; M-x `latex-math-preview-save-image-file' makes an image for the same object
;; as `latex-math-preview-expression' and save it as a file which is png or eps.
;; When making an image, this command may remove number of mathematical formulas.
;; 
;; Buffer local variable `latex-math-preview-usepackage-cache' has
;; the values of \usepackage for previewing.
;; If you want to reload this variable, you use
;; M-x `latex-math-preview-reload-usepackage'.
;; 
;; M-x `latex-math-preview-beamer-frame' makes an image of one frame of beamer,
;; which is LaTeX style for presentation.
;; This feature is experimental for now and it is possible not to work on your environment.

;; Requirements;
;; Because latex-math-preview displays images in emacs,
;; it is not work in emacs on terminal.
;; 
;; * Version of Emacs *
;; The version of emacs is 22 or 23 on Linux.
;; latex-math-preview is tested on Ubuntu 10.04.
;; But on Meadow 3 on Windows latex-math-preview probably works.
;; 
;; * Image Conversion *
;; latex-math-preview uses some commands to convert tex to png, tex to eps, and so on.
;; Only for previewing mathematical expressions, latex-math-preview requires
;; latex and dvipng commands.
;; According to your environment and settings of latex-math-preview,
;; latex-math-preview creates preview images by combining the following commands.
;; 
;;  - dvipng
;;  - dvips
;;  - latex
;;  - platex
;;  - pdflatex
;;  - dvipdf
;;  - dvipdfm
;;  - dvipdfmx
;;  - gs

;;; Install of Emacs Lisp:
;; * Load latex-math-preview *
;; Put latex-math-preview.el to your load-path of Emacs and
;; write the following code in ~/.emacs.el.
;; 
;;   (autoload 'latex-math-preview-expression "latex-math-preview" nil t)
;;   (autoload 'latex-math-preview-insert-symbol "latex-math-preview" nil t)
;;   (autoload 'latex-math-preview-save-image-file "latex-math-preview" nil t)
;;   (autoload 'latex-math-preview-beamer-frame "latex-math-preview" nil t)
;; 
;; * Key Bindings *
;; Please set key bindings of TeX mode which you prefer if desired.
;; 
;; For example, for YaTeX mode we add the following settings to ~/.emacs.el.
;;
;;   (add-hook 'yatex-mode-hook
;;            '(lambda ()
;;            (YaTeX-define-key "p" 'latex-math-preview-expression)
;;            (YaTeX-define-key "\C-p" 'latex-math-preview-save-image-file)
;;            (YaTeX-define-key "j" 'latex-math-preview-insert-symbol)
;;            (YaTeX-define-key "\C-j" 'latex-math-preview-last-symbol-again)
;;            (YaTeX-define-key "\C-b" 'latex-math-preview-beamer-frame)))
;;   (setq latex-math-preview-in-math-mode-p-func 'YaTeX-in-math-mode-p)
;; 
;; In particular case latex-math-preview-in-math-mode-p does not work well.
;; So you may use YaTeX-in-math-mode-p alternatively.
;;
;; This setting almost binds latex-math-preview-expression to "C-c p",
;; latex-math-preview-save-image-file to "C-c C-p",
;; latex-math-preview-insert-symbol to "C-c j",
;; latex-math-preview-last-symbol-again to "C-c C-j",
;; and latex-math-preview-beamer-frame to "C-c C-b".
;; 
;; * Path of Programs *
;; latex-math-preview.el uses 'latex', 'dvipng' and 'dvips'.
;; If the programs (dvipng, dvips, latex, and so on) 
;; are not in the load path of system or
;; you want to use the different programs from the default,
;; you need to change the variable `latex-math-preview-command-path-alist'.
;; For example,
;;    (setq latex-math-preview-command-path-alist
;;          '((latex . "/usr/bin/latex") (dvipng . "/usr/bin/dvipng") (dvips . "/usr/bin/dvips")))
;; 

;;; Usage:
;; * latex-math-preview-expression *
;; If you type M-x `latex-math-preview-expression' when cursor points to 
;; a mathematical expression, new buffer including an image is created.
;; In this buffer, you check the result of LaTeX mathematical
;; expression and type 'q' to exit the window.
;; Also, you can preview the strings of selected region 
;; if transient-mark is on (type "\C-@" twice).
;; 
;; In preview window, the following binded key is applicable.
;;  q: exit preview buffer
;;  Q: delete preview buffer
;;  j: scroll up
;;  k: scroll down
;; 
;; * latex-math-preview-insert-symbol *
;; You can browse the list of LaTeX symbols if you execute 
;; "M-x latex-math-preview-insert-symbol".
;; Then you select the symbol by cursor which you want to insert and
;; may type RET to insert it.
;; When inserting symbols, you may look at the description of key map 
;; on the head of buffer.
;; 
;; When you open viewing buffer, if there is not image caches then
;; you want for a while until the program finish making images.
;; Also, you can make all image caches by
;; M-x `latex-math-preview-make-all-cache-images'.
;; 
;; C-u M-x `latex-math-preview-insert-symbol' asks you the page
;; that you want to open before preview buffer is created.
;; M-x `latex-math-preview-last-symbol-again' make you
;; insert the last inserted symbol.
;; Also, if you insert a symbol, not depending on the context, you can use
;; M-x `latex-math-preview-mathematical-symbol-datasets' or
;; M-x `latex-math-preview-insert-text-symbol'.
;; 
;; * latex-math-preview-save-image-file *
;; First, M-x `latex-math-preview-save-image-file' asks you abount path of
;; an outputted image. Then, you must input the path of which extention is
;; 'png' or 'eps'. The program makes an image and you may get a desired one.
;; 
;; * latex-math-preview-beamer-frame *
;; If we execute M-x `latex-math-preview-beamer-frame'
;; in \frame{ ... } or \begin{frame} ... \end{frame},
;; we can preview particular one page of beamer presentation
;; same as `latex-math-preview-expression'.

;;; Settings:
;; * LaTeX template *
;; latex-math-preview.el makes a temporary file and gets an image
;; by commands 'latex' and 'dvipng'.
;; The construction of a temporary latex file is the following.
;; -------------------------------------------------------------------
;;   (part of `latex-math-preview-latex-template-header'
;;    the default value is the following)
;;   \documentclass{article}
;;   \pagestyle{empty}
;;   
;;   (part of usepackages
;;    the values of \usepackage searched from current buffer or
;;    `latex-math-preview-latex-usepackage-for-not-tex-file')
;;   
;;   \begin{document}
;;   (some mathematical expressions)
;;   \par
;;   \end{document}
;; -------------------------------------------------------------------
;; 
;; If you can change the header in temporary latex files,
;; you should set the customized value to
;; `latex-math-preview-latex-template-header'.
;; latex-math-preview searches '\usepackage' in current buffer and
;; uses its value when making images.
;; But when there is no '\usepackage' strings, alternatively
;; `latex-math-preview-latex-usepackage-for-not-tex-file' is used.
;; So you can set a preferd value to
;; `latex-math-preview-latex-usepackage-for-not-tex-file'.
;; The variable `latex-math-preview-template-header-for-save-image' is prepared for making image files.
;; 
;; * Conversion Process *
;; The default value of `latex-math-preview-tex-to-png-for-preview' is
;; 
;;   (defvar latex-math-preview-tex-to-png-for-preview
;;    '(latex dvipng))
;; 
;; This means when creating png images for preview latex-math-preview uses
;; `latex-math-preview-execute-latex' (tex to dvi) and
;; `latex-math-preview-execute-dvipng' (dvi to png) in series.
;; If you use other programs to create png images, please edit this variable.
;; For example, to use platex (tex to dvi), dvipdfmx (dvi to pdf), and gs (pdf to png),
;; 
;;   (defvar latex-math-preview-tex-to-png-for-preview
;;    '(platex dvipdfmx gs-to-png))
;; 
;; The variables `latex-math-preview-tex-to-png-for-save', `latex-math-preview-tex-to-eps-for-save',
;; `latex-math-preview-tex-to-ps-for-save', and `latex-math-preview-beamer-to-png' is the same.
;; The default functions we can uses to convert images are
;;  - `latex-math-preview-execute-latex'
;;  - `latex-math-preview-execute-platex'
;;  - `latex-math-preview-execute-pdflatex-to-dvi'
;;  - `latex-math-preview-execute-pdflatex-to-pdf'
;;  - `latex-math-preview-execute-dvipdf'
;;  - `latex-math-preview-execute-dvipdfm'
;;  - `latex-math-preview-execute-dvipdfmx'
;;  - `latex-math-preview-execute-dvipng'
;;  - `latex-math-preview-execute-dvips-to-ps'
;;  - `latex-math-preview-execute-dvips-to-eps'
;;  - `latex-math-preview-execute-gs-to-png'
;; 
;; For japanese, we recommend the following settings to use platex:
;;    (setq latex-math-preview-tex-to-png-for-preview '(platex dvipng))
;;    (setq latex-math-preview-tex-to-png-for-save '(platex dvipng))
;;    (setq latex-math-preview-tex-to-eps-for-save '(platex dvips-to-eps))
;;    (setq latex-math-preview-tex-to-ps-for-save '(platex dvips-to-ps))
;;    (setq latex-math-preview-beamer-to-png '(platex dvipdfmx gs-to-png))
;; 
;; * Options of Commands *
;; The options of commands are specified by `latex-math-preview-command-option-alist' and
;; the options for triming margins of images are specified by `latex-math-preview-command-trim-option-alist'.
;; If you configure the commands, please modify these variables.
;; Because this customize is advanced, we want you to refer to source code for details.
;; 
;; The color options of 'dvipng' is determined by
;; `latex-math-preview-image-foreground-color' and
;; `latex-math-preview-image-background-color', which define
;; the foreground and background colors of png images respectively.
;; If these variables are nil, these colors are the same as it of
;; the default face.
;; 
;; * Matching mathematical expression *
;; When you make preview images, 
;; the default setting supports the following LaTeX mathematical
;; expressions for "M-x latex-math-preview-expression".
;;  $ ... $
;;  $$ ... $$
;;  \[ ... \]
;;  \begin{math} ... \end{math}
;;  \begin{displaymath} ... \end{displaymath}
;;  \begin{equation} ... \end{equation}
;;  \begin{gather} ... \end{gather}
;;  \begin{align} ... \end{align}
;;  \begin{alignat} ... \end{alignat}
;; 
;; If you want to preview other LaTeX mathematical expressions,
;; please customize the variable `latex-math-preview-match-expression'.
;; 
;; * List of symbols for insertion *
;; To change symbol set, you may customize the variable
;; `latex-math-preview-mathematical-symbol-datasets'.
;; If you always open the same inital page,
;; you set nil to `latex-math-preview-restore-last-page-of-symbol-list'
;; and string of initial page name to 
;; `latex-math-preview-initial-page-of-symbol-list'.
;; 
;; This program keep the created images as cache.
;; These images saved in `latex-math-preview-cache-directory-for-insertion'
;; of which default value is "~/.emacs.d/latex-math-preview-cache".
;; If you change the cache directory, please put the following code
;; in "~/.emacs.el".
;; (setq latex-math-preview-cache-directory-for-insertion
;;       "cache directory in your system")

;; ChangeLog:
;; 2011/02/07 version 0.5.4 yamaguchi
;;     Support gswin32c.exe for meadow.
;; 2010/09/19 version 0.5.3 yamaguchi
;;     Refactoring.
;; 2010/09/13 version 0.5.2 yamaguchi
;;     Modify to pass byte-compile.
;; 2010/09/13 version 0.5.1 yamaguchi
;;     Some bug fixes.
;; 2010/08/29 version 0.5.0 yamaguchi
;;     Support beamer (add new command `latex-math-preview-beamer-frame').
;;     Remove some functions and variables and add new functions and variables. 
;; 2010/01/20 version 0.4.2 yamaguchi
;;     Add buffer local variable `latex-math-preview-usepackage-cache'
;;     for splitted tex files.
;; 2009/12/09 version 0.4.1 yamaguchi
;;     Add some key binding on latex-math-preview-expression-mode-map.
;; 2009/12/08 version 0.4.0 yamaguchi
;;     Use image-mode when previewing mathematical expressions.
;; 2009/10/18 version 0.3.11 yamaguchi
;;     Change function latex-math-preview-bounds-of-latex-math
;;     to distingush '$$ ... $$' from '$ ... $'.
;; 2009/09/16 version 0.3.10 yamaguchi
;;     Add new functions latex-math-preview-insert-isearch-forward and
;;     latex-math-preview-insert-isearch-backward.
;; 2009/08/27 version 0.3.9 yamaguchi
;;     Fix bug that makes order of usepackage reverse.
;; 2009/08/21 version 0.3.8 yamaguchi
;;     Some bug fixes.
;; 2009/08/14 version 0.3.7 yamaguchi
;;     Bug fix of `latex-math-preview-search-header-usepackage'.
;; 2009/08/13 version 0.3.6 yamaguchi
;;     Bug fix of `latex-math-preview-search-header-usepackage'.
;; 2009/08/12 version 0.3.5 yamaguchi
;;     Display error message of TeX processing.
;; 2009/08/11 version 0.3.4 yamaguchi
;;     Add `latex-math-preview-search-header-usepackage'.
;; 2009/08/08 version 0.3.3 yamaguchi
;;     Add detailed commentary.
;; 2009/08/07 version 0.3.2 yamaguchi
;;     Bug fix.
;; 2009/08/06 version 0.3.1 yamaguchi
;;     New function `latex-math-preview-save-image-file'.
;; 2009/08/06 version 0.3.0 yamaguchi
;;     New function `latex-math-preview-move-to-beginning-of-candidates'.
;;     New function `latex-math-preview-move-to-end-of-candidates'.
;;     New function `latex-math-preview-move-to-current-line-first-item'.
;;     New function `latex-math-preview-move-to-current-line-last-item'.
;;     New function `latex-math-preview-insert-mathematical-symbol'
;;     New function `latex-math-preview-insert-text-symbol'
;;     Extention of `latex-math-preview-expression'.
;;     Remove feature viewing dvi file.
;; 2009/08/04 version 0.2.3 yamaguchi
;;     New function `latex-math-preview-toggle-window-maximization'.
;;     New function `latex-math-preview-last-symbol-again'.
;; 2009/08/03 version 0.2.2 yamaguchi
;;     Change name of function `latex-math-preview-insert-sign' to `latex-math-preview-insert-symbol'.
;;     Add some groups of symbol candidates.
;;     Some bug fixes.
;; 2009/08/02 version 0.2.1 yamaguchi
;;     New command latex-math-preview-next-candidates-for-insertion.
;;     Some adjustments.
;; 2009/08/02 version 0.2.0 yamaguchi
;;     New command latex-math-preview-insert-sign.
;;     Change name of function `latex-math-preview' to `latex-math-preview-expression'.
;;     bug fixes.
;; 2009/07/31 version 0.1.1 yamaguchi
;;     adjust background and foreground colors of png to default face.
;; 2009/07/25 version 0.1.0 yamaguchi
;;     support Meadow3.

;;; Code:

(require 'cl)
(require 'image-mode)
(require 'thingatpt)

;;;###autoload
(defgroup latex-math-preview nil
  "LaTeX Math Preview."
  :prefix "latex-math-preview-"
  :group 'applications
  )

(defvar latex-math-preview-expression-buffer-name
  "*latex-math-preview-expression*"
  "Name of buffer which displays preview image.")

(defvar latex-math-preview-tex-processing-error-buffer-name
  "*latex-math-preview-tex-processing-error*"
  "Name of buffer displaying TeX temporary file which raises error")

(defvar latex-math-preview-insert-symbol-buffer-name
  "*latex-math-preview-candidates*"
  "Name of buffer which displays candidates of LaTeX mathematical symbols.")

(defvar latex-math-preview-latex-command
  "latex" "Path to latex.")

(defvar latex-math-preview-command-dvips
  "dvips" "Path to dvips.")

(defvar latex-math-preview-temporary-file-prefix
  "temp_latex_math"
  "The prefix name of some temporary files which is produced in making an image.")

(defvar latex-math-preview-cache-directory-for-insertion
  (concat (getenv "HOME") "/.emacs.d/latex-math-preview-cache")
  "Cache directory.")

(defvar latex-math-preview-latex-template-header
  "\\documentclass{article}\n\\pagestyle{empty}\n"
  "Insert string to beginning of temporary latex file to make image.")

(defvar latex-math-preview-latex-usepackage-for-not-tex-file
  '("\\usepackage{amsmath, amssymb, amsthm}")
  "List of strings which are \\usepackage commands.")

(defvar latex-math-preview-template-header-for-save-image
  "\\documentclass{article}\n\\pagestyle{empty}\n"
  "Insert string to beginning of temporary latex file to make image.")

(defvar latex-math-preview-match-expression-remove-formula-number
  '("equation" "gather" "align" "alignat")
  "List of LaTeX enviroments string to remove number of formula when making image file.")

(defvar latex-math-preview-image-foreground-color nil
  "Foreground color of image created by dvipng.")

(defvar latex-math-preview-image-background-color nil
  "Background color of image created by dvipng.")

(defvar latex-math-preview-dvipng-color-option nil
  "Temporary variable. You must not set this variable.")

(defvar latex-math-preview-window-configuration nil
  "Temporary variable in which window configuration is saved.")

(defvar latex-math-preview-current-insert-mode nil
  "Temporary variable of which value is 'math or 'text.")

(defvar latex-math-preview-list-name-for-candidates-defined-as-list
  '((math . latex-math-preview-candidates-defined-as-list-for-math)
    (text . latex-math-preview-candidates-defined-as-list-for-text))
  "Symbols of list which includes candidates defined as list.")

(defvar latex-math-preview-candidates-defined-as-list-for-math nil
  "Temporary variable.")

(defvar latex-math-preview-candidates-defined-as-list-for-text nil
  "Temporary variable.")

(defvar latex-math-preview-match-expression
  '(
    ;; \[...\]
    (0 . "\\\\\\[\\(.\\|\n\\)*?\\\\]")

    ;; \(...\)
    (0 . "\\\\(\\(.\\|\n\\)*?\\\\)")

    ;; \begin{math}...\end{math}
    (0 . "\\\\begin{math}\\(\\(.\\|\n\\)*?\\)\\\\end{math}")

    ;; \begin{displaymath}...\end{displaymath}
    (0 . "\\\\begin{displaymath}\\(\\(.\\|\n\\)*?\\)\\\\end{displaymath}")

    ;; \begin{equation}...\end{equation}
    (0 . "\\\\begin{equation\\(\\|\\*\\)}\\(\\(.\\|\n\\)*?\\)\\\\end{equation\\(\\|\\*\\)}")

    ;; \begin{gather}...\end{gather}
    (0 . "\\\\begin{gather\\(\\|\\*\\)}\\(\\(.\\|\n\\)*?\\)\\\\end{gather\\(\\|\\*\\)}")

    ;; \begin{align}...\end{align}
    (0 . "\\\\begin{align\\(\\|\\*\\)}\\(\\(.\\|\n\\)*?\\)\\\\end{align\\(\\|\\*\\)}")

    ;; \begin{alignat}...\end{alignat}
    (0 . "\\\\begin{alignat\\(\\|\\*\\)}\\(\\(.\\|\n\\)*?\\)\\\\end{alignat\\(\\|\\*\\)}")

    )
  "We use these expressions when extracting a LaTeX mathematical expression.
The elements of this list is the form which is a list of \(integer regular-expression\).
The regular-expression matchs a string including LaTeX mathematical expressions.
The integer is the number to access needed string from regular-expressin.")

(defvar latex-math-preview-list-name-symbol-datasets
  '((math . latex-math-preview-mathematical-symbol-datasets)
    (text . latex-math-preview-text-symbol-datasets))
  "Name of list of datasets.")

(defvar latex-math-preview-text-symbol-datasets
  '(("SpecialCharacter"
     ("special character" nil
      ("\\#" "\\$" "\\%" "\\&" "\\_" "\\{" "\\}" "\\S" "\\P" "\\dag" "\\ddag"
       "\\copyright" "\\pounds" "\\oe" "\\OE" "\\ae" "\\AE" "\\aa" "\\AA"
       "\\o" "\\O" "\\l" "\\L" "\\ss" "?`" "!`" "\\i"
       "\\j" "`" "'" "``"
       "''" "*" "``\\,'" "'\\,``" "-" "--" "---" "\\textregistered"
       "\\texttrademark" "\\textvisiblespace" "\\textbackslash"
       "\\textasciitilde" "\\textasciicircum" ("\\textcircled{" "s" "}")
       ))
     ("special charactor (2)" ("\\usepackage[T1]{fontenc}")
      ("\\DH" "\\dh" "\\DJ" "\\dj" "\\NG" "\\ng" "\\TH" "\\th"
       "\\guillemotleft" "\\guilsinglleft" "\\quotedblbase" "\\textquotedbl"
       "\\guillemotright" "\\guilsinglright" "\\quotesinglbase")))
    ("AccentText"
     ("accent (1)" nil
      (("\\`{" "o" "}") ("\\'{" "o" "}") ("\\^{" "o" "}") ("\\\"{" "o" "}")
       ("\\~{" "o" "}") ("\\={" "o" "}") ("\\.{" "o" "}") ("\\u{" "o" "}")
       ("\\v{" "o" "}") ("\\H{" "o" "}") ("\\t{" "oo" "}") ("\\c{" "o" "}")
       ("\\d{" "o" "}") ("\\b{" "o" "}") ("\\r{" "a" "}")
       ))
     ("accent (2)" ("\\usepackage[T1]{fontenc}")
      (("\\k{" "a" "}"))))
    ("Logo"
     ("logo (1)" nil
      ("\\TeX" "\\LaTeX" "\\LaTeXe"))
     ("logo (2)" ("\\usepackage{mflogo}")
      ("\\MF" "\\MP")))
    )
  "List of candidates for insertion of symbol.
The elements is the data having the following structure.
\(\"unique id\" \(\"usepackage command 1\" \"usepackage command 2\" ...\)
\(data of symbol candidates\)\)

Each data of symbol candidates is string or list. In the case of string, 
just insert it in the buffer. If the data is list, list must have three strings,
that is \(StringA, StringB StringC\). Then, insert the composition 
StringA and StringB.")

(defvar latex-math-preview-mathematical-symbol-datasets
  '(("DelimitersArrows"
     ("delimiters" nil
      (("(" "x" ")") ("[" "x" "]") ("\\{" "x" "\\}")
       ("\\lfloor " "x" " \\rfloor") ("\\lceil " "x" " \\rceil")
       ("\\langle " "x" " \\rangle")
       "\\backslash" "\\|" "\\uparrow" "\\Uparrow" "\\downarrow" "\\Downarrow"
       "\\updownarrow" "\\Updownarrow"))
     ("arrows" nil
      ("\\gets" "\\Leftarrow" "\\to" "\\Rightarrow"
       ;; omit "\\leftarrow" and "\\rightarrow"
       "\\leftrightarrow" "\\Leftrightarrow" "\\mapsto" "\\hookleftarrow"
       "\\leftharpoonup" "\\leftharpoondown" "\\longleftarrow" "\\Longleftarrow"
       "\\longleftrightarrow" "\\Longleftrightarrow" "\\longmapsto"
       "\\hookrightarrow" "\\rightharpoonup" "\\rightharpoondown"
       "\\iff" "\\nearrow" "\\searrow" "\\swarrow" "\\nwarrow"
       "\\rightleftharpoons")))
    ("GreekLetters"
     ("greek letters" nil
      ("\\alpha" "\\beta" "\\gamma" "\\delta" "\\epsilon" "\\zeta" "\\eta"
       "\\theta" "\\iota" "\\kappa" "\\lambda" "\\mu" "\\nu" "\\xi" "\\pi"
       "\\rho" "\\sigma" "\\tau" "\\upsilon" "\\phi" "\\chi" "\\psi" "\\omega"
       "\\varepsilon" "\\vartheta" "\\varpi" "\\varrho" "\\varsigma" "\\varphi"
       "\\Gamma" "\\Delta" "\\Theta" "\\Lambda" "\\Xi" "\\Pi" "\\Sigma"
       "\\Upsilon" "\\Phi" "\\Psi" "\\Omega"))
     ("italic greek letters" ("\\usepackage{amsmath}")
      ("\\varGamma" "\\varDelta" "\\varTheta" "\\varLambda" "\\varXi" "\\varPi"
       "\\varSigma" "\\varUpsilon" "\\varPhi" "\\varPsi" "\\varOmega")))
    ("BinaryOperator1"
     ("binary operators (1)" nil
      ("\\pm" "\\mp" "\\times" "\\div"  "\\ast" "\\star" "\\circ" "\\bullet"
       "\\cdot" "\\cap" "\\cup" "\\uplus" "\\sqcap" "\\sqcup" "\\vee" "\\wedge"
       "\\setminus" "\\wr" "\\diamond" "\\bigtriangleup" "\\bigtriangledown"
       "\\triangleleft" "\\triangleright" "\\oplus" "\\ominus" "\\otimes"
       "\\oslash" "\\odot" "\\bigcirc" "\\dagger" "\\ddagger" "\\amalg")))
    ("RelationalOperator1"
     ("relational operators (1)" nil
      ("\\le" "\\prec" "\\preceq" "\\ll" "\\subset" "\\subseteq" "\\vdash"
       "\\in" "\\notin" "\\ge" "\\succ" "\\succeq" "\\gg" "\\supset" "\\supseteq"
       "\\sqsupseteq" "\\dashv" "\\ni"
       ;; omit "\\leq" and "geq" because this is the same as "\\le" and "\\ge", respectively.
       "\\equiv" "\\sim" "\\simeq" "\\asymp" "\\approx" "\\cong" "\\neq"
       "\\doteq" "\\propto" "\\models" "\\perp" "\\mid" "\\parallel" "\\bowtie"
       "\\smile" "\\frown"
       "\\not\\equiv")))
    ("BinaryOperator2"
     ("binary operators (2)" ("\\usepackage{amssymb}")
      ("\\boxdot" "\\boxplus" "\\centerdot" "\\boxminus" "\\veebar" "\\barwedge"
       "\\doublebarwedge" "\\Cup" "\\Cap" "\\curlywedge" "\\curlyvee"
       "\\leftthreetimes" "\\rightthreetimes" "\\dotplus" "\\intercal"
       "\\circledcirc" "\\circledast" "\\circleddash" "\\divideontimes" "\\lessdot"
       "\\gtrdot" "\\ltimes" "\\rtimes" "\\smallsetminus")))
    ("RelationalOperator2"
     ("relational operators (2)" ("\\usepackage{amssymb}")
      ("\\circlearrowright" "\\circlearrowleft" "\\rightleftharpoons"
       "\\leftrightharpoons" "\\Vdash" "\\Vvdash" "\\vDash" "\\twoheadrightarrow"
       "\\twoheadleftarrow" "\\leftleftarrows" "\\rightrightarrows" "\\upuparrows"
       "\\downdownarrows" "\\upharpoonright" "\\downharpoonright" "\\upharpoonleft"
       "\\downharpoonleft" "\\rightarrowtail" "\\leftarrowtail" "\\rightleftarrows"
       "\\Lsh" "\\Rsh" "\\rightsquigarrow" "\\leftrightsquigarrow" "\\looparrowleft"
       "\\looparrowright" "\\circeq" "\\succsim" "\\gtrsim" "\\gtrapprox"
       "\\multimap" "\\therefore" "\\because" "\\doteqdot" "\\triangleq" "\\precsim" 
       "\\lesssim" "\\lessapprox" "\\eqslantless" "\\eqslantgtr" "\\curlyeqprec"
       "\\curlyeqsucc")))
    ("RelationalOperator3"
     ("relational operators (3)" ("\\usepackage{amssymb}")
      ("\\preccurlyeq" "\\leqq" "\\leqslant" "\\lessgtr" "\\risingdotseq"
       "\\fallingdotseq" "\\succcurlyeq" "\\geqq" "\\geqslant" "\\gtrless"
       "\\sqsubset" "\\sqsupset" "\\vartriangleright" "\\vartriangleleft"
       "\\trianglerighteq" "\\trianglelefteq" "\\between" "\\blacktriangleright"
       "\\blacktriangleleft" "\\vartriangle" "\\eqcirc" "\\lesseqgtr" "\\gtreqless"
       "\\lesseqqgtr" "\\gtreqqless" "\\Rrightarrow" "\\Lleftarrow" "\\varpropto"
       "\\smallsmile" "\\smallfrown" "\\Subset" "\\Supset" "\\subseteqq"
       "\\supseteqq" "\\bumpeq" "\\Bumpeq" "\\lll" "\\ggg" "\\pitchfork"
       "\\backsim" "\\backsimeq")))
    ("RelationalOperator4"
     ("relational operators (4)" ("\\usepackage{amssymb}")
      ("\\lvertneqq" "\\gvertneqq" "\\nleq" "\\ngeq" "\\nless" "\\ngtr" "\\nprec"
       "\\nsucc" "\\lneqq" "\\gneqq" "\\nleqslant" "\\ngeqslant" "\\lneq" "\\gneq"
       "\\npreceq" "\\nsucceq" "\\precnsim" "\\succnsim" "\\lnsim" "\\gnsim"
       "\\nleqq" "\\ngeqq" "\\precneqq" "\\succneqq" "\\precnapprox" "\\succnapprox"
       "\\lnapprox" "\\gnapprox" "\\nsim" "\\ncong" "\\varsubsetneq" "\\varsupsetneq"
       "\\nsubseteqq" "\\nsupseteqq" "\\subsetneqq" "\\supsetneqq" "\\varsubsetneqq"
       "\\varsupsetneqq" "\\subsetneq" "\\supsetneq" "\\nsubseteq" "\\nsupseteq"
       "\\nparallel" "\\nmid" "\\nshortmid" "\\nshortparallel" "\\nvdash"
       "\\nVdash" "\\nvDash" "\\nVDash" "\\ntrianglerighteq" "\\ntrianglelefteq"
       "\\ntriangleleft" "\\ntriangleright" "\\nleftarrow" "\\nLeftarrow"
       "\\nRightarrow" "\\nLeftrightarrow" "\\nleftrightarrow" "\\eqsim"
       "\\shortmid" "\\shortparallel" "\\thicksim" "\\thickapprox" "\\approxeq"
       "\\succapprox" "\\precapprox" "\\curvearrowleft" "\\curvearrowright"
       "\\backepsilon")))
    ("BigSymbolsIntegralDots"
     ("big symbols" nil
      ("\\sum" "\\prod" "\\coprod" "\\int" "\\oint" "\\bigcap" "\\bigcup"
       "\\bigsqcup" "\\bigvee" "\\bigwedge" "\\bigodot" "\\bigotimes"
       "\\bigoplus" "\\biguplus"))
     ("integral" ("\\usepackage{amsmath}" "\\usepackage{amssymb}")
      ("\\int" "\\iint" "\\iiint" "\\iiiint" "\\idotsint"))
     ("dots" ("\\usepackage{amsmath}" "\\usepackage{amssymb}")
      ("\\dots" "\\dotsc" "\\dotsb" "\\dotsm" "\\dotsi")))
    ("MiscellaneousSymbols"
     ("miscellaneous" nil
      ("\\aleph" "\\hbar" "\\imath" "\\jmath" "\\ell" "\\wp" "\\Re" "\\Im"
       "\\partial" "\\infty" "\\prime" "\\emptyset" "\\nabla" "\\surd"
       "\\top" "\\bot" "\\angle" "\\triangle" "\\forall" "\\exists"
       "\\neg" "\\flat" "\\natural" "\\sharp" "\\clubsuit" "\\diamondsuit"
       "\\heartsuit" "\\spadesuit"))
     ("other symbols" ("\\usepackage{amssymb}")
      ("\\square" "\\blacksquare" "\\lozenge" "\\blacklozenge" "\\backprime"
       "\\bigstar" "\\blacktriangledown" "\\blacktriangle" "\\triangledown"
       "\\angle" "\\measuredangle" "\\sphericalangle" "\\circledS" "\\complement"
       "\\diagup" "\\diagdown" "\\varnothing" "\\nexists" "\\Finv" "\\Game"
       "\\mho" "\\eth" "\\beth" "\\gimel" "\\daleth" "\\digamma"
       "\\varkappa" "\\Bbbk" "\\hslash" "\\hbar")))
    ("Functions"
     ("often used" nil
      (("\\frac{" "x" "}{y}") ("" "x" "^{n}") "\\sum_{i=0}^{\\infty}"
       ("\\sqrt{" "x" "}") ("\\sqrt[" "3" "]{x}")))
     ("fraction" ("\\usepackage{amsmath}" "\\usepackage{amssymb}")
      (("\\tfrac{" "a" "}{b}") ("\\dfrac{" "a" "}{b}") ("\\cfrac{" "a" "}{b}")
       ("\\binom{" "a" "}{b}")))
     ("functions" nil
      ("\\arccos" "\\arcsin" "\\arctan" "\\arg" "\\cos" "\\cosh" "\\cot" "\\coth"
       "\\csc" "\\deg" "\\det" "\\dim" "\\exp" "\\gcd" "\\hom" "\\inf" "\\ker"
       "\\lg" "\\lim" "\\liminf" "\\limsup" "\\ln" "\\log" "\\max" "\\min"
       "\\Pr" "\\sec" "\\sin" "\\sinh" "\\sup" "\\tan" "\\tanh"
       "\\bmod" "\\pmod")))
    ("AccentMath"
     ("accent (1)" nil
      (("\\hat{" "a" "}") ("\\check{" "a" "}") ("\\breve{" "a" "}")
       ("\\acute{" "a" "}") ("\\grave{" "a" "}") ("\\tilde{" "a" "}")
       ("\\bar{" "a" "}") ("\\vec{" "a" "}") ("\\dot{" "a" "}") ("\\ddot{" "a" "}")))
     ("accent (2)" ("\\usepackage{amsmath}" "\\usepackage{amssymb}")
      (("\\Hat{" "A" "}") ("\\Dot{" "A" "}") ("\\Check{" "A" "}")
       ("\\Ddot{" "A" "}") ("\\Tilde{" "A" "}") ("\\Breve{" "A" "}")
       ("\\Acute{" "A" "}") ("\\Bar{" "A" "}") ("\\Vec{" "A" "}")
       ("\\dddot{" "x" "}") ("\\ddddot{" "x" "}"))))
    ("Decoration"
     ("decoration 1" nil
      (("\\overline{" "xy" "}") ("\\underline{" "xy" "}") ("\\widehat{" "xy" "}")
       ("\\widetilde{" "xy" "}") ("\\overbrace{" "xy" "}")
       ("\\underbrace{" "xy" "}") ("\\overrightarrow{" "\\mathrm{OA}" "}")
       ("\\overleftarrow{" "\\mathrm{OA}" "}") ("\\stackrel{" "f" "}{\\to}")
       "\\stackrel{\\mathrm{def}}{=}"))
     ("decoration 2" ("\\usepackage{amsmath}" "\\usepackage{amssymb}")
      (("\\overleftrightarrow{" "A" "}") ("\\underleftrightarrow{" "A" "}")
       ("\\xrightarrow{" "\\text{text}" "}") ("\\xrightarrow[" "abc" "]{}")
       ("\\xleftarrow{" "\\text{text}" "}") ("\\xleftarrow[" "abc" "]{}"))))
    ("Typefaces"
     ("typefaces (1)" nil
      (("\\mathrm{" "abcdefghABCDEFGH" "}") ("\\mathbf{" "abcdefghABCDEFGH" "}")
       ("\\mathit{" "abcdefghABCDEFGH" "}") ("\\mathcal{" "abcdefghABCDEFGH" "}")
       ("\\mathsf{" "abcdefghABCDEFGH" "}") ("\\mathtt{" "abcdefghABCDEFGH" "}")))
     ("typefaces (2)" ("\\usepackage{amssymb}")
      (("\\mathfrak{" "abcdefghABCDEFGH" "}") ("\\mathbb{" "abcdefghABCDEFGH" "}"))))
    )
  "List of candidates for insertion of LaTeX mathematical symbol.
For data structure, refer to `latex-math-preview-text-symbol-datasets'")


(defvar latex-math-preview-always-maximize-window nil
  "Always maximize preview window for `latex-math-preview-insert-symbol' if non-nil.")

(defvar latex-math-preview-restore-last-page-of-symbol-list t
  "Restore last page of symbol list at next insertion
 if this `latex-math-preview-restore-last-page-of-symbol-list' is non-nil.")

(defvar latex-math-preview-inserted-last-symbol '((math . nil) (text . nil))
  "Inserted last symbol.")

(defvar latex-math-preview-initial-page-of-symbol-list
  `((math . ,(car (nth 0 latex-math-preview-mathematical-symbol-datasets)))
    (text . ,(car (nth 0 latex-math-preview-text-symbol-datasets))))
  "Page of symbol list which is displayed initially.")

(defvar latex-math-preview-current-page-of-symbol-list '((math . nil) (text . nil))
  "Page of symbol list on present buffer displaying mathematical symbols.")

(defvar latex-math-preview-information-line-number nil
  "Temporary variable. List of line numbers at which various information is descripted.")

(defface latex-math-preview-candidate-for-insertion-face
  '((t (:foreground "dark orange")))
  "Face for notations of LaTeX mathematical symbol.")

(defface latex-math-preview-key-for-insertion-face
  '((t (:foreground "dodger blue")))
  "Face for notations of LaTeX mathematical symbol.")

(defvar latex-math-preview-selection-face-for-insertion 'highlight
  "Face for currently selected item.")

(defvar latex-math-preview-selection-overlay-for-insertion nil
  "Overlay for highlighting currently selected item.")

(defvar latex-math-preview-not-delete-tmpfile nil
  "Not delete temporary files and directory if this value is true. Mainly for debugging.")

(defvar latex-math-preview-dvipng-log-buffer nil
  "Buffer name for output by dvipng. Mainly for debugging.")

(defvar latex-math-preview-in-math-mode-p-func 'latex-math-preview-in-math-mode-p
  "Symbol of function is used for determining whether cursor is in mathematical expression.
If you use YaTeX mode then the recommended value of this variable is YaTeX-in-math-mode-p.")

(define-derived-mode latex-math-preview-expression-mode image-mode "LaTeXPreview"
  "Major mode for latex-math-preview-expression. This mode is derived from image-mode."
  (define-key latex-math-preview-expression-mode-map "n" 'next-line)
  (define-key latex-math-preview-expression-mode-map "p" 'previous-line)
  (define-key latex-math-preview-expression-mode-map "f" 'forward-char)
  (define-key latex-math-preview-expression-mode-map "b" 'backward-char)
  (define-key latex-math-preview-expression-mode-map "j" 'next-line)
  (define-key latex-math-preview-expression-mode-map "k" 'previous-line)
  (define-key latex-math-preview-expression-mode-map "l" 'forward-char)
  (define-key latex-math-preview-expression-mode-map "h" 'backward-char)
  (define-key latex-math-preview-expression-mode-map "o" 'delete-other-windows)
  (define-key latex-math-preview-expression-mode-map "g" 'latex-math-preview-quit-window)
  (define-key latex-math-preview-expression-mode-map "q" 'latex-math-preview-quit-window)
  (define-key latex-math-preview-expression-mode-map "Q" 'latex-math-preview-delete-buffer))

(defvar latex-math-preview-insert-symbol-map nil
  "Keymap for insertion mode of latex-math-preview-insert-symbol.")

(or latex-math-preview-insert-symbol-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "q") 'latex-math-preview-quit-window)
      (define-key map (kbd "Q") 'latex-math-preview-delete-buffer)
      (define-key map (kbd "g") 'latex-math-preview-quit-window)
      (define-key map (kbd "o") 'latex-math-preview-toggle-window-maximization)
      (define-key map (kbd "c") 'latex-math-preview-symbols-of-other-page)
      (define-key map (kbd "j") 'latex-math-preview-move-to-downward-item)
      (define-key map (kbd "k") 'latex-math-preview-move-to-upward-item)
      (define-key map (kbd "l") 'latex-math-preview-move-to-right-item)
      (define-key map (kbd "h") 'latex-math-preview-move-to-left-item)
      (define-key map (kbd "a") 'latex-math-preview-move-to-current-line-first-item)
      (define-key map (kbd "e") 'latex-math-preview-move-to-current-line-last-item)
      (define-key map (kbd "C-a") 'latex-math-preview-move-to-current-line-first-item)
      (define-key map (kbd "C-e") 'latex-math-preview-move-to-current-line-last-item)
      (define-key map (kbd "n") 'latex-math-preview-move-to-downward-item)
      (define-key map (kbd "p") 'latex-math-preview-move-to-upward-item)
      (define-key map (kbd "C-n") 'latex-math-preview-move-to-downward-item)
      (define-key map (kbd "C-p") 'latex-math-preview-move-to-upward-item)
      (define-key map (kbd "f") 'latex-math-preview-move-to-right-item)
      (define-key map (kbd "b") 'latex-math-preview-move-to-left-item)
      (define-key map (kbd "C-f") 'latex-math-preview-move-to-right-item)
      (define-key map (kbd "C-b") 'latex-math-preview-move-to-left-item)
      (define-key map (kbd "<down>") 'latex-math-preview-move-to-downward-item)
      (define-key map (kbd "<up>") 'latex-math-preview-move-to-upward-item)
      (define-key map (kbd "<right>") 'latex-math-preview-move-to-right-item)
      (define-key map (kbd "<left>") 'latex-math-preview-move-to-left-item)
      (define-key map (kbd "C-v") 'latex-math-preview-scroll-up)
      (define-key map (kbd "M-v") 'latex-math-preview-scroll-down)
      (define-key map (kbd "SPC") 'latex-math-preview-scroll-up)
      (define-key map (kbd "S-SPC") 'latex-math-preview-scroll-down)
      (define-key map (kbd "s") 'latex-math-preview-insert-isearch-forward)
      (define-key map (kbd "C-s") 'latex-math-preview-insert-isearch-forward)
      (define-key map (kbd "r") 'latex-math-preview-insert-isearch-backward)
      (define-key map (kbd "C-r") 'latex-math-preview-insert-isearch-backward)
      (define-key map (kbd "i") 'latex-math-preview-next-candidates-for-insertion)
      (define-key map (kbd "u") 'latex-math-preview-previous-candidates-for-insertion)
      (define-key map (kbd "C-d") 'latex-math-preview-delete-current-cache)
      (define-key map (kbd "C-m") 'latex-math-preview-put-selected-candidate)
      (define-key map (kbd "M-<") 'latex-math-preview-move-to-beginning-of-candidates)
      (define-key map (kbd "M->") 'latex-math-preview-move-to-end-of-candidates)
      (define-key map (kbd "<return>") 'latex-math-preview-put-selected-candidate)
      (define-key map (kbd "<mouse-1>") 'latex-math-preview-put-candidate-mouse-selecting)
      (setq latex-math-preview-insert-symbol-map map)))

;;-----------------------------------------------------------------------------
;; Convert image

(defvar latex-math-preview-working-directory nil
  "Working directory. This variable must not be set.")

(defvar latex-math-preview-command-buffer "*latex-math-preview-command*"
  "Buffer name of command process.")

(defvar latex-math-preview-trim-image nil
  "If this variable is t, trim margin when creating images.
This variable must not be set.")

(defvar latex-math-preview-gs-resolution 100
  "Resolution for `latex-math-preview-execute-gs-to-png'.")

(defvar latex-math-preview-command-path-alist nil
  "List of pair of command and path.")

(defvar latex-math-preview-command-option-alist
  '((pdflatex-to-pdf "-output-format" "pdf") (pdflatex-to-dvi "-output-format" "dvi")
    (dvipng "-x" "1728") (dvips-to-ps "-Ppdf") (dvips-to-eps "-Ppdf")
    (gs-to-png
     "-dSAFER" "-dNOPAUSE" "-sDEVICE=png16m" "-dTextAlphaBits=4" "-dBATCH" "-dGraphicsAlphaBits=4" "-dQUIET")
    (gswin32c-to-png
     "-dSAFER" "-dNOPAUSE" "-sDEVICE=png16m" "-dTextAlphaBits=4" "-dBATCH" "-dGraphicsAlphaBits=4" "-dQUIET"))
  "Options of commands.")

(defvar latex-math-preview-command-trim-option-alist
  '((dvipng "-T" "tight") (dvips-to-ps "-E" "-x" "3000") (dvips-to-eps "-E" "-x" "3000") (convert "-trim"))
  "Options of commands to trim margin.")

(eval-and-compile
  (defvar latex-math-preview-command-output-extension-alist
    '((latex . "dvi") (platex . "dvi") (dvipng . "png") (dvipdf . "pdf") (dvipdfm . "pdf") (dvipdfmx . "pdf"))
    "List of command and extension of output file."))

(defvar latex-math-preview-tex-to-png-for-preview
  '(latex dvipng)
  "Sequence of end of function names to create png image for preview.")

(defvar latex-math-preview-tex-to-png-for-save
  '(latex dvipng)
  "Sequence of end of function names to create png image for save.")

(defvar latex-math-preview-tex-to-eps-for-save
  '(latex dvips-to-eps)
  "Sequence of end of function names to create eps image for save.")

(defvar latex-math-preview-tex-to-ps-for-save
  '(latex dvips-to-ps)
  "Sequence of end of function names to create ps image for save.")

(defvar latex-math-preview-beamer-to-png
  '(pdflatex-to-pdf gs-to-png)
  "Sequence of end of function names to create png image for previewing beamer page.")

(defun latex-math-preview-get-command-path (command-key)
  (or (cdr (assoc command-key latex-math-preview-command-path-alist)) (symbol-name command-key)))

(defun latex-math-preview-get-command-option (key)
  (let ((args (cdr (assoc key latex-math-preview-command-option-alist))))
    (if latex-math-preview-trim-image
	(append args (cdr (assoc key latex-math-preview-command-trim-option-alist))) args)))

(defun latex-math-preview-call-command-process (command args)
  (= 0 (apply 'call-process command nil latex-math-preview-command-buffer nil args)))

(defun latex-math-preview-execute-latex-command (command input opts extension)
  (let ((dir (or latex-math-preview-working-directory (file-name-directory input) ".")))
    (if (latex-math-preview-call-command-process command `(,@opts ,(concat "-output-directory=" dir) ,input))
	(concat dir "/" (file-name-sans-extension (file-name-nondirectory input)) "." extension)
      nil)))

(eval-and-compile
  (defun latex-math-preview-get-command-output-extension (key)
    (or (cdr (assoc key latex-math-preview-command-output-extension-alist))
	(let ((name (symbol-name key))) (if (string-match "-to-\\(.*\\)$" name) (match-string 1 name) nil))))

  (defmacro latex-math-preview-define-latex-function (command)
    `(defun ,(intern (concat "latex-math-preview-execute-" (symbol-name command))) (input)
       (latex-math-preview-execute-latex-command
	(latex-math-preview-get-command-path (quote ,(intern (car (split-string (symbol-name command) "-"))))) input
	(latex-math-preview-get-command-option (quote ,command))
	,(latex-math-preview-get-command-output-extension command)))))

(latex-math-preview-define-latex-function latex)
(latex-math-preview-define-latex-function platex)
(latex-math-preview-define-latex-function pdflatex-to-dvi)
(latex-math-preview-define-latex-function pdflatex-to-pdf)

(defvar latex-math-preview-convert-dvipng-color-mode nil)

(defun latex-math-preview-read-image-color-argument (prompt default-val)
  (catch :get
    (while t
      (let ((val (read-from-minibuffer
		  (concat prompt " [RGB] (default " (mapconcat 'identity default-val " ") "): "))))
	(if (= (length val) 0) (setq val default-val) (setq val (split-string val " ")))
	(catch :invalid-color
	  (when (= (length val) 3)
	    (dolist (s val)
	      (let ((num (string-to-number s))) (when (or (< num 0.0) (> num 1.0)) (throw :invalid-color t))))
	    (throw :get val)))
	(message "Invalid color.")
	(sleep-for 1)))))

(defun latex-math-preview-argument-dvipng (command input)
  (let* ((output (concat (file-name-sans-extension input) "."
			 (latex-math-preview-get-command-output-extension command)))
	 (args (append (latex-math-preview-get-command-option command)
		       (list "-o" output input))))
    (cond
     ((eq 'read latex-math-preview-convert-dvipng-color-mode)
      (setq args
	    (append args (list "-bg" (concat "rgb " (mapconcat 'identity
							       (latex-math-preview-read-image-color-argument
								"Background" '("1.0" "1.0" "1.0")) " "))
			       "-fg" (concat "rgb " (mapconcat 'identity
							       (latex-math-preview-read-image-color-argument
								"Foreground" '("0.0" "0.0" "0.0")) " "))))))
     ((eq 'buffer latex-math-preview-convert-dvipng-color-mode)
      (setq args
	    (append args
		    (or latex-math-preview-dvipng-color-option
			(setq latex-math-preview-dvipng-color-option
			      (latex-math-preview-get-dvipng-color-option)))))))
    (cons output args)))

(defun latex-math-preview-change-file-extension (command input)
  (concat (file-name-sans-extension input) "." (latex-math-preview-get-command-output-extension command)))

(defun latex-math-preview-argument-convert-dvi (command input)
  (let* ((out (latex-math-preview-change-file-extension command input))
	 (args (append (latex-math-preview-get-command-option command) (list "-o" out input))))
    (cons out args)))

(defun latex-math-preview-argument-dvipdf (command input)
  (let* ((out (latex-math-preview-change-file-extension command input))
	 (args (append (latex-math-preview-get-command-option command) (list input out))))
    (cons out args)))

(defun latex-math-preview-argument-gs-to-png (command input)
  (let* ((out (latex-math-preview-change-file-extension command input))
	 (args (append (latex-math-preview-get-command-option command)
		       (list (concat "-r" (number-to-string latex-math-preview-gs-resolution))
			     (concat "-sOutputFile=" out) input))))
    (cons out args)))

(defvar latex-math-preview-command-create-argument-alist
  '((dvipng . latex-math-preview-argument-dvipng)
    (dvips-to-ps . latex-math-preview-argument-convert-dvi)
    (dvips-to-eps . latex-math-preview-argument-convert-dvi)
    (dvipdf . latex-math-preview-argument-dvipdf)
    (dvipdfm . latex-math-preview-argument-convert-dvi)
    (dvipdfmx . latex-math-preview-argument-convert-dvi)
    (gs-to-png . latex-math-preview-argument-gs-to-png)
    (gswin32c-to-png . latex-math-preview-argument-gs-to-png))
  "List of command and function name to create arguments.")

(defun latex-math-preview-get-command-argument (command input)
  (let ((func (cdr (assoc command latex-math-preview-command-create-argument-alist))))
    (if func (funcall func command input) nil)))

(defun latex-math-preview-call-latex-command (command-path command input)
  (let ((tmp (latex-math-preview-get-command-argument command input)))
    (if (latex-math-preview-call-command-process command-path (cdr tmp)) (car tmp) nil)))

(defmacro latex-math-preview-define-convert-function (command)
  "Define function to convert images. Use first element of
a list created by splitting COMMAND by \"-\" as a command name."
  `(defun ,(intern (concat "latex-math-preview-execute-" (symbol-name command))) (input)
     (latex-math-preview-call-latex-command
      (latex-math-preview-get-command-path (quote ,(intern (car (split-string (symbol-name command) "-")))))
      (quote ,command) input)))

(latex-math-preview-define-convert-function dvipng)
(latex-math-preview-define-convert-function dvips-to-ps)
(latex-math-preview-define-convert-function dvips-to-eps)
(latex-math-preview-define-convert-function dvipdf)
(latex-math-preview-define-convert-function dvipdfm)
(latex-math-preview-define-convert-function dvipdfmx)
(latex-math-preview-define-convert-function gs-to-png)
(latex-math-preview-define-convert-function gswin32c-to-png)

(defun latex-math-preview-successive-convert (input &rest convert-list)
  (let ((product input))
    (catch :no-output
      (dolist (conv convert-list)
	(setq product (funcall (intern (concat "latex-math-preview-execute-" (symbol-name conv))) product))
	(if (not product) (throw :no-output nil))))
    product))

(defvar latex-math-preview-custom-convert nil)

(defvar latex-math-preview-convert-command-list nil)

(defun latex-math-preview-reset-custom-convert ()
  (interactive)
  (setq latex-math-preview-convert-command-list nil)
  (setq latex-math-preview-custom-convert nil))

(defun latex-math-preview-completing-read-convert-command ()
  (if (not latex-math-preview-convert-command-list)
      (setq latex-math-preview-convert-command-list
	    (mapcar (lambda (func) (list (replace-regexp-in-string 
					  "^latex-math-preview-execute-" "" (symbol-name func))))
		    (apropos-internal "^latex-math-preview-execute-"))))
  (completing-read "Command: " latex-math-preview-convert-command-list nil t))

(defun latex-math-preview-set-custom-convert ()
  (interactive)
  (latex-math-preview-reset-custom-convert)
  (catch :end
    (while t
      (let ((s (latex-math-preview-completing-read-convert-command)))
	(if (= 0 (length s)) (throw :end t))
	(setq latex-math-preview-custom-convert (append latex-math-preview-custom-convert (list (intern s))))))))

;; (defun latex-math-preview-convert-imagemagick-trim (input)
;;   (let ((old (make-temp-name input)))
;;     (rename-file input old)
;;     (if (= 0 (apply 'call-process (car latex-math-preview-command-imagemagick-convert) nil
;; 		    latex-math-preview-command-buffer nil (list "-trim" old input)))
;; 	(progn
;; 	  (delete-file old)
;; 	  input)
;;       (rename-file old input)
;;       nil)))

;;-----------------------------------------------------------------------------
;; Search usepackage

(defvar latex-math-preview-usepackage-cache nil)
(make-variable-buffer-local 'latex-math-preview-usepackage-cache)

(defun latex-math-preview-search-header-usepackage ()
  "Return list of \\usepackage which is used in current buffer."
  (save-excursion
    (let (cmds beg-doc tmp-str)
      (goto-char (point-min))
      (if (search-forward "\\begin{document}" nil t)
	  (setq beg-doc (point)) (setq beg-doc (point-max)))
      (goto-char (point-min))
      (while (re-search-forward "\\\\usepackage[^}]*}" beg-doc t)
	(setq tmp-str (match-string-no-properties 0))
	(save-excursion 
	  (if (not (re-search-backward "\\(^\\|[^\\\\]\\)%" (line-beginning-position) t))
	      (add-to-list 'cmds tmp-str))))
      (nreverse cmds))))

(defun latex-math-preview-search-header-usepackage-other-file (filename)
  "Return list of \\usepackage in other file."
  (if (and (file-exists-p filename) (not (file-directory-p filename)))
      (with-temp-buffer
	(insert-file-contents filename)
	(goto-char (point-min))
	(latex-math-preview-search-header-usepackage))
    nil))

(defun latex-math-preview-get-header-usepackage ()
  "Return cache of usepackage and create cache data if needed"
  (when (and (not latex-math-preview-usepackage-cache) (string-match "\\.tex$" (buffer-file-name)))
    (let (cache)
      (setq cache (latex-math-preview-search-header-usepackage))
      (if (and (not cache)
	       (let ((filename (buffer-file-name (current-buffer))))
		 (and filename (string-match "\\.tex" (buffer-file-name (current-buffer))))))
	  (setq cache (latex-math-preview-search-header-usepackage-other-file
		       (read-file-name "Main TeX file: " nil default-directory))))
      (setq latex-math-preview-usepackage-cache (or cache t))))
  (if (listp latex-math-preview-usepackage-cache) latex-math-preview-usepackage-cache nil))

(defun latex-math-preview-reload-usepackage (&optional other-file)
  "Reload usepackage cache from current buffer. If you want to get the cache
from other file, you use C-u M-x `latex-math-preview-reload-usepackage'."
  (interactive "P")
  (setq latex-math-preview-usepackage-cache
	(if other-file
	    (or (latex-math-preview-search-header-usepackage-other-file
		 (read-file-name "Main TeX file: " nil default-directory)) t)
	  (latex-math-preview-get-header-usepackage))))

(defun latex-math-preview-bounds-of-latex-math ()
  "A `bounds-of-thing-at-point' function for a LaTeX mathematical expression.
See `latex-math-preview-match-expression' for what's matched.
The return is a pair of buffer positions (START . END), or nil if
no recognised expression at or surrounding point."

  ;; TeX style $...$ could easily match some huge chunk of the buffer, and
  ;; even @math{...} or <math>...</math> could occur in comments or some
  ;; unrelated context.  So it's not reliable just to take the first of
  ;; these which match, instead the strategy is to check for all forms
  ;; around point and take the one that's the smallest.
  ;;
  ;; Only the start position of the match is considered for "smallest", the
  ;; one that's the shortest distance before point (but covering point of
  ;; course) in the buffer is taken.

  (let (case-fold-search beg end)

    ;; $...$ and $$...$$
    ;; thing-at-point-looking-at doesn't work on "$...$".  The way the start
    ;; and end are the same (ie. "$") breaks the straightforward
    ;; implementation of that function; so the idea here is to search back
    ;; for the starting "$", and one not "\$" escaped, then check the $...$
    ;; extent covers point
    (save-excursion
      (while (and (search-backward "$" nil t) ;; $ not preceded by \
                  (eq ?\\ (char-before))))
      (skip-chars-backward "$")
      (when (or (looking-at "\\(\\$\\$\\(?:\\\\\\$\\|[^$]\\)+?\\$\\$\\)")
		(looking-at "\\(\\$\\(?:\\\\\\$\\|[^$]\\)+?\\$\\)"))
        (setq beg (match-beginning 1) end (match-end 1))))

    (dolist (elem latex-math-preview-match-expression)
      (when (thing-at-point-looking-at (cdr elem))
        ;; if no other match, or this match is later, then override
        (if (or (not beg)
                (> (match-beginning (car elem)) beg))
            (setq beg (match-beginning (car elem)) end (match-end (car elem))))))

    (and beg
         (cons beg end))))

(put 'latex-math 'bounds-of-thing-at-point 'latex-math-preview-bounds-of-latex-math)

(defun latex-math-preview-in-math-mode-p ()
  "Return non-nil if current position is in mathematical expression.
This function may make mistake when there is sequence of '$'.
If you use YaTeX, then you should use YaTeX-in-math-mode-p alternatively."
  (thing-at-point 'latex-math))

(defun latex-math-preview-clear-working-directory ()
  (if (not latex-math-preview-not-delete-tmpfile)
      (latex-math-preview-clear-tmp-directory latex-math-preview-working-directory))
  (setq latex-math-preview-working-directory nil))

(defun latex-math-preview-create-temporary-tex-filename ()
  (setq latex-math-preview-working-directory (make-temp-file "latex-math-preview-" t))
  (concat latex-math-preview-working-directory "/" latex-math-preview-temporary-file-prefix ".tex"))

(defun latex-math-preview-make-temporary-tex-file (math-exp template-header &optional usepackages)
  (let ((dot-tex (latex-math-preview-create-temporary-tex-filename))
	(usepck (or usepackages (latex-math-preview-get-header-usepackage)
		    latex-math-preview-latex-usepackage-for-not-tex-file))
	(coding-system buffer-file-coding-system))
    (with-temp-file dot-tex
      (insert template-header
	      (if usepck (mapconcat 'identity usepck "\n") "")
	      "\n\\begin{document}\n" math-exp "\n\\par\n\\end{document}\n")
      (set-buffer-file-coding-system coding-system))
    dot-tex))

(defun latex-math-preview-raise-can-not-create-image (dot-tex)
  (with-current-buffer (get-buffer-create latex-math-preview-command-buffer)
    (goto-char (point-min))
    (insert "% " (make-string 5 ?-) " Created by latex-math-preview.el at "
	    (format-time-string "%Y/%m/%d %H:%M:%S") " " (make-string 5 ?-) "\n")
    (save-excursion
      (insert "\n% " (make-string 5 ?-) " Error message " (make-string 5 ?-) "\n"))
    (insert-file-contents dot-tex)
    (goto-char (point-min)))
  (pop-to-buffer latex-math-preview-command-buffer)
  (rename-buffer latex-math-preview-tex-processing-error-buffer-name)
  (signal 'tex-processing-error '("TeX processing error")))

(defun latex-math-preview-make-png-file (dot-tex)
  "Make png image from DOT-TEX."
  (let ((latex-math-preview-convert-dvipng-color-mode 'buffer)
	(latex-math-preview-trim-image t))
    (or (apply 'latex-math-preview-successive-convert dot-tex latex-math-preview-tex-to-png-for-preview)
	(latex-math-preview-raise-can-not-create-image dot-tex))))

(defun latex-math-preview-clear-tmp-directory (dir)
  "Delete temporary directory and files contained in it."
  (if (file-directory-p dir)
      (progn
	(let ((directories))
	  (dolist (file (directory-files dir))
	    (let ((path (concat dir "/" file)))
	      (cond ((and (file-directory-p path) (not (string-match "^\\.+$" file)))
		     (add-to-list 'directories file))
		    ((file-regular-p path)
		     (condition-case nil (delete-file path)
		       (message "Can not delete '%s'" path))))))
	  (dolist (del-dir directories)
	    (message del-dir)
	    (latex-math-preview-clear-tmp-directory (concat dir "/" del-dir))))
	(condition-case nil (delete-directory dir)
	  (message "Can not delete '%s'" dir)))))

;;-----------------------------------------------------------------------------
;; view png in a buffer

(defvar latex-math-preview-display-whole-image nil)

(defun latex-math-preview-png-image (image)
  "Display png image IMAGE in a buffer."
  (or (and (image-type-available-p 'png) (display-images-p)) (error "Cannot display PNG in this Emacs"))
  (with-current-buffer (get-buffer-create latex-math-preview-expression-buffer-name)
    (setq cursor-type nil)
    (let ((inhibit-read-only t))
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert-file-contents image)
      (goto-char (point-min))
      (setq buffer-read-only t))
    (latex-math-preview-expression-mode)
    (buffer-disable-undo))
  (pop-to-buffer latex-math-preview-expression-buffer-name)
  (if (and latex-math-preview-display-whole-image (not (pos-visible-in-window-p (point-max))))
      (with-current-buffer latex-math-preview-expression-buffer-name (delete-other-windows))))

(defun latex-math-preview-get-dvipng-color-option ()
  "Get string for dvipng options '-bg' and '-fg'."
  (let ((max (car (color-values "#ffffff"))))
    (list "-bg"
	  (concat "rgb "
		  (mapconcat
		   (lambda (col)
		     (let ((val (/ (float col) max)))
		       (cond ((> val 1.0) (setq val 1.0))
			     ((< val 0.0) (setq val 0.0)))
		       (format "%.02f" val)))
		   (color-values (or latex-math-preview-image-background-color
				     (face-background 'default))) " "))
	  "-fg"
	  (concat "rgb "
		  (mapconcat
		   (lambda (col)
		     (let ((val (/ (float col) max)))
		       (cond ((> val 1.0) (setq val 1.0))
			     ((< val 0.0) (setq val 0.0)))
		       (format "%.02f" val)))
		   (color-values (or latex-math-preview-image-foreground-color
				     (face-foreground 'default))) " ")))))

(defun latex-math-preview-convert-to-output-file (func args output)
  (let ((out (apply func args)))
    (if (and out output (not (file-exists-p output)))
	(progn
	  (rename-file out output)
	  output)
      out)))

(defun latex-math-preview-cut-mathematical-expression (&optional remove-num-expression)
  (let ((str))
    (if (and transient-mark-mode mark-active)
	(progn
	  (setq str (buffer-substring (region-beginning) (region-end)))
	  (setq mark-active nil))
      ;; If you use (region-active-p), then the program can not work on emacs 22.
      (setq str (thing-at-point 'latex-math)))
    (if (and str remove-num-expression)
	(dolist (env remove-num-expression)
	  (setq str (replace-regexp-in-string (format "{%s}" env) (format "{%s\*}" env) str))))
      str))

(defun latex-math-preview-expression ()
  "Preview a TeX maths expression at (or surrounding) point.
The `latex-math-preview-function' variable controls the viewing method. 
The LaTeX notations which can be matched are $...$, $$...$$ or
the notations which are stored in `latex-math-preview-match-expression'."
  (interactive)
  (let ((str (latex-math-preview-cut-mathematical-expression)))
    (if str
	(let ((dot-tex (latex-math-preview-make-temporary-tex-file str latex-math-preview-latex-template-header)))
	  (setq latex-math-preview-window-configuration (current-window-configuration))
	  (latex-math-preview-png-image (latex-math-preview-make-png-file dot-tex))
	  (latex-math-preview-clear-working-directory))
      (message "Not in a TeX mathematical expression."))))

(defun latex-math-preview-make-image-for-save (func-sequence output dot-tex)
  "Create an image from DOT-TEX by FUNC-SEQUENCE and save as OUTPUT."
  (let ((latex-math-preview-convert-dvipng-color-mode 'read)
	(latex-math-preview-trim-image t))
    (or (latex-math-preview-convert-to-output-file
	 'latex-math-preview-successive-convert (cons dot-tex func-sequence) output)
	(latex-math-preview-raise-can-not-create-image dot-tex))))

(defun latex-math-preview-prompt-for-save-image-file (use-custom-conversion)
  (if use-custom-conversion
      (let ((extension (latex-math-preview-get-command-output-extension
			(nth (1- (length latex-math-preview-custom-convert)) latex-math-preview-custom-convert))))
	(concat "Save as" (if extension (concat " (*." extension ")") "")": "))
    "Save as (*.png, *.eps, or *.ps): "))

(defun latex-math-preview-save-image-file (use-custom-conversion &optional output)
  (interactive "P")
  (let ((prompt (latex-math-preview-prompt-for-save-image-file use-custom-conversion)))
    (if (not output) (setq output (read-file-name prompt nil default-directory)))
    (while (file-directory-p (expand-file-name output))
      (message "Please specify filename not directory.")
      (sleep-for 1)
      (setq output (read-file-name prompt nil default-directory))))
  (if (or (not (file-exists-p output)) (y-or-n-p "File exists. Overwrite? "))
      (let ((str (latex-math-preview-cut-mathematical-expression
		  latex-math-preview-match-expression-remove-formula-number)))
	(if str
	    (let ((func-series
		   (cond
		    (use-custom-conversion latex-math-preview-custom-convert)
		    ((string-match "\\.png$" output) latex-math-preview-tex-to-png-for-save)
		    ((string-match "\\.eps$" output) latex-math-preview-tex-to-eps-for-save)
		    ((string-match "\\.ps$" output) latex-math-preview-tex-to-ps-for-save)
		    (t nil)))
		  (dot-tex (latex-math-preview-make-temporary-tex-file
			    str latex-math-preview-template-header-for-save-image)))
	      (if (not func-series) (message "Can not specify conversion.")
		(if (latex-math-preview-make-image-for-save func-series output dot-tex)
		    (message "Save image as %s" output) (message "Can not create an image file.")))
	      (latex-math-preview-clear-working-directory))
	  (message "Not in a TeX mathematical expression.")))
    (message "Stop making image.")))

;;-----------------------------------------------------------------------------
;; Manage window

(defun latex-math-preview-quit-window ()
  "Quit preview window."
  (interactive)
  (set-window-configuration latex-math-preview-window-configuration))

(defun latex-math-preview-delete-buffer ()
  "Delete buffer which is created for preview."
  (interactive)
  (kill-buffer (buffer-name))
  (set-window-configuration latex-math-preview-window-configuration))

					; Clear dvipng option for coloring.
(setq latex-math-preview-dvipng-color-option nil)

;;-----------------------------------------------------------------------------
;; Insert Mathematical expression 

(defun latex-math-preview-clear-cache-for-insertion (&optional key)
  "Delete cache images in directory of which name is KEY.
If KEY is nil then all directories saving caches is deleted."
  (interactive)
  (if key
      (dolist (name-and-sets latex-math-preview-list-name-symbol-datasets)
	(if (eval `(assoc key ,(cdr name-and-sets)))
	    (progn
	      (latex-math-preview-clear-tmp-directory
	       (concat latex-math-preview-cache-directory-for-insertion "/" key))
	      (message "Finish deleting image caches of \"%s\"" key))))
    (latex-math-preview-clear-tmp-directory latex-math-preview-cache-directory-for-insertion)
    (message "Finish deleting all image caches.")))

(defun latex-math-preview-make-symbol-candidate-image (dirpath num dot-tex)
  "Create a cache image from latex file for including LATEX-SYMBOL.
Image is saved in directory of which path is DIRPATH.
 NUM is used for distingushing other images."
  (let* ((latex-math-preview-convert-dvipng-color-mode 'buffer)
	 (latex-math-preview-trim-image t)
	 (png (latex-math-preview-convert-to-output-file
	       'latex-math-preview-successive-convert (cons dot-tex latex-math-preview-tex-to-png-for-preview)
	       (concat dirpath "/" (format "%05d" num) "_" (format-time-string "%Y%m%d%H%M%S") ".png"))))
    (if png
	(progn
	  (latex-math-preview-clear-working-directory)
	  png)
      (latex-math-preview-raise-can-not-create-image dot-tex))))

(defun latex-math-preview-make-symbol-caches (key symbol-datasets type)
  "Create cache images which are associated with KEY in directory of which name is KEY.
TYPE is 'math or 'text."
  (let ((dataset (cdr (assoc key symbol-datasets)))
	(dirpath (concat latex-math-preview-cache-directory-for-insertion "/" key))
	(num 0))
    (if (file-directory-p dirpath)
	(message "'%s' exists. Cache may be used." dirpath)
      (progn
	(make-directory dirpath t)
	(message "Creating images. Please wait for a while.")
	(dolist (subcat dataset)
	  (let ((desc (car subcat))
		(packages (cadr subcat))
		(sym-set (nth 2 subcat)))
	    (dolist (sym sym-set)
	      (let* ((latex-symbol (if (listp sym) (eval `(concat ,@sym)) sym))
		     (dot-tex (latex-math-preview-make-temporary-tex-file
			       (if (eq 'math type) (concat "$" latex-symbol "$") latex-symbol)
			       latex-math-preview-latex-template-header packages)))
		(latex-math-preview-make-symbol-candidate-image dirpath num dot-tex))
	      (setq num (1+ num)))))
	(message "Finish making cache images of \"%s\"." key)))))

(defun latex-math-preview-make-all-cache-images ()
  "Create all cache images."
  (interactive)
  (dolist (name-and-sets latex-math-preview-list-name-symbol-datasets)
    (let ((symbol-datasets (eval `,(cdr name-and-sets)))
	  (type (car name-and-sets)))
      (dolist (dataset symbol-datasets)
	(latex-math-preview-make-symbol-caches (car dataset) symbol-datasets type))))
  (message "Finish making all cache images."))

(defun latex-math-preview-strings-and-images-sizes (imagepaths sym-set)
  "Look over cache images.
Return maximum size of images and maximum length of strings and images"
  (let ((candidates-defined-as-list
	 (cdr (assq latex-math-preview-current-insert-mode 
		    latex-math-preview-list-name-for-candidates-defined-as-list)))
	(max-img-size 0) (max-str-length 0) (syms sym-set)
	(img-list nil) (ret-list nil))
    (eval `(setq ,candidates-defined-as-list nil))
    (dolist (pngpath imagepaths)
      (let* ((img (create-image pngpath	'png nil :ascent 'center))
	     (size (car (image-size img t))))
	(if (< max-img-size size) (setq max-img-size size))
	(let ((s (car syms)) (str))
	  (if (listp s)
	      (progn
		(setq str (eval `(concat ,@s)))
		(add-to-list candidates-defined-as-list `(,str ,s)))
	    (setq str s))
	  (let ((len (length str)))
	    (if (< max-str-length len) (setq max-str-length len)))
	  (add-to-list 'img-list `(,str ,img)))
	(setq syms (cdr syms))))
    `(,max-img-size ,max-str-length ,(nreverse img-list))))

(defun latex-math-preview-insert-key-explanations ()
  "Insert explanations of key map."
  (insert "key:")
  (add-text-properties (point-min) (point) '(face bold))
  (let ((max (window-width)))
    (dolist (text '("[RET] insert" "[j] down" "[k] up" "[h] left" "[l] right"
		    "[i] next page" "[u] previous page" "[o] change window size"
		    "[c] change page" "[q] quit"))
      (if (> (+ (current-column) (length text)) max)
	  (insert "\n    "))
      (insert "   " text)))
  (insert "\n")
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\[" nil t)
      (backward-char)
      (let ((start-pt (point)))
	(if (re-search-forward "\\]" nil t)
	    (add-text-properties start-pt (point)
				 '(face latex-math-preview-key-for-insertion-face)))))))

(defun latex-math-preview-insert-subcategory-infomation (subcat-str packages)
  "Insert information of images associated with SUBCAT-STR."
  (let* ((desc (concat (make-string 5 ?-) " * " subcat-str " * "))
	 (package-str)
	 (num-dash (- (window-width) (length desc)))
	 (start-pt (point)))
    (insert desc)
    (add-text-properties start-pt (point) '(face bold))
    (setq start-pt (point))
    (if packages
	(progn
	  (setq package-str (concat (mapconcat 'identity packages " ") " "))
	  (insert package-str)
	  (setq num-dash (- num-dash (length package-str)))
	  (add-text-properties start-pt (point)
			       '(face latex-math-preview-key-for-insertion-face))))
    (insert (make-string num-dash ?-)))
  (insert "\n"))

(defun latex-math-preview-insert-candidate-images (key symbol-datasets)
  "Insert images and expressions."
  (setq latex-math-preview-information-line-number nil)
  (let* ((dataset (cdr (assoc key symbol-datasets)))
	 (dirpath (concat latex-math-preview-cache-directory-for-insertion "/" key))
	 (pngpaths (mapcar (lambda (path) (concat latex-math-preview-cache-directory-for-insertion
						  "/" key "/" path))
			   (sort (delete-if (lambda (file) (not (string-match "^[0-9]+_" file)))
					    (directory-files dirpath)) 'string<)))
	 (imgdata) (all-sym-set))
    (dolist (subcat dataset)
      (setq all-sym-set (append all-sym-set (nth 2 subcat))))
    (setq imgdata (latex-math-preview-strings-and-images-sizes pngpaths all-sym-set))
    (let* ((new-tab-width (+ 4 (ceiling (/ (float (car imgdata)) (float (frame-char-width))))))
	   (str-size (* new-tab-width (ceiling (/ (float (+ 6 (car (cdr imgdata)))) (float new-tab-width)))))
	   ;; You must not remove (+ 6 ...).
	   ;; Implementation of latex-math-preview-set-overlay-for-selected-item needs redundant space.
	   (str-format (format "%%-%ds" str-size))
	   (col (floor (/ (window-width)
			  (+ str-size (* (ceiling (/ (float (car imgdata))
						     (float (* (frame-char-width) new-tab-width))))
					 new-tab-width)))))
	   (current-col 0)
	   (num-of-items 0)
	   (num-subcat-end 0))
      (setq tab-width new-tab-width)
      (let ((pngset (nth 2 imgdata)))
	(dolist (subcat dataset)
	  (latex-math-preview-insert-subcategory-infomation (car subcat) (cadr subcat))
	  (add-to-list 'latex-math-preview-information-line-number (1- (line-number-at-pos)))

	  (setq num-subcat-end (+ num-subcat-end (length (nth 2 subcat))))
	  (while (< num-of-items num-subcat-end)
	    (let ((png (car pngset)))
	      (insert-image (cadr png))
	      (insert "\t")
	      (let ((start-pt (point)))
		(insert (format str-format (car png)))
		(add-text-properties start-pt (point) '(face latex-math-preview-candidate-for-insertion-face))))
	    (setq pngset (cdr pngset))
	    (setq current-col (1+ current-col))
	    (setq num-of-items (1+ num-of-items))
	    (if (>= current-col col)
		(progn (setq current-col 0) (insert "\n"))))
	  (if (> current-col 0)
	      (progn (insert "\n") (setq current-col 0)))
	  )))))

(defun latex-math-preview-create-buffer-for-insertion (key)
  "Create buffer displaying cache images in KEY."
  (or (and (image-type-available-p 'png)
           (display-images-p))
      (error "Cannot display PNG in this Emacs"))

  (setcdr (assq latex-math-preview-current-insert-mode
		latex-math-preview-current-page-of-symbol-list) key)
  
  (let ((symbol-datasets (eval `,(cdr (assq latex-math-preview-current-insert-mode 
					    latex-math-preview-list-name-symbol-datasets)))))
    (latex-math-preview-make-symbol-caches key symbol-datasets latex-math-preview-current-insert-mode)
    (setq latex-math-preview-window-configuration (current-window-configuration))

    (pop-to-buffer latex-math-preview-insert-symbol-buffer-name)
    (if latex-math-preview-always-maximize-window (delete-other-windows))

    (with-current-buffer (get-buffer-create latex-math-preview-insert-symbol-buffer-name)
      (setq cursor-type nil)
      (setq truncate-lines t)
      (setq line-spacing 8)
      (setq buffer-read-only nil)
      (erase-buffer)

      (latex-math-preview-insert-key-explanations)
      (latex-math-preview-insert-candidate-images key symbol-datasets)

      (goto-line (nth (1- (length latex-math-preview-information-line-number))
		      latex-math-preview-information-line-number))
      (latex-math-preview-move-to-right-item)
      (buffer-disable-undo)
      (setq buffer-read-only t)
      (use-local-map latex-math-preview-insert-symbol-map)
      (setq mode-name "LaTeXPreview"))))

(defun latex-math-preview-insert-symbol-base (num)
  (if (or (not num) (= num 1))
      (latex-math-preview-create-buffer-for-insertion
       (if latex-math-preview-restore-last-page-of-symbol-list
	   (or (cdr (assq latex-math-preview-current-insert-mode
			  latex-math-preview-current-page-of-symbol-list))
	       (cdr (assq latex-math-preview-current-insert-mode
			  latex-math-preview-initial-page-of-symbol-list)))
	 (cdr (assq latex-math-preview-current-insert-mode
		    latex-math-preview-initial-page-of-symbol-list))))
    (let ((key (completing-read "page: " 
				(eval `,(cdr (assq latex-math-preview-current-insert-mode
						   latex-math-preview-list-name-symbol-datasets)))
				nil t)))
      (latex-math-preview-create-buffer-for-insertion key))))

(defun latex-math-preview-insert-mathematical-symbol (&optional num)
  "Insert LaTeX mathematical symbols with displaying."
  (interactive "p")
  (setq latex-math-preview-current-insert-mode 'math)
  (latex-math-preview-insert-symbol-base num))

(defun latex-math-preview-insert-text-symbol (&optional num)
  "Insert symbols for text part with displaying."
  (interactive "p")
  (setq latex-math-preview-current-insert-mode 'text)
  (latex-math-preview-insert-symbol-base num))

(defun latex-math-preview-set-current-insert-mode ()
  (setq latex-math-preview-current-insert-mode
	(if (funcall latex-math-preview-in-math-mode-p-func) 'math 'text)))

(defun latex-math-preview-insert-symbol (&optional num)
  "Insert LaTeX mathematical symbols with displaying."
  (interactive "p")
  (latex-math-preview-set-current-insert-mode)
  (latex-math-preview-insert-symbol-base num))

(defun latex-math-preview-symbols-of-other-page ()
  "Change other page."
  (interactive)
  (latex-math-preview-quit-window)
  (latex-math-preview-insert-symbol -1))

(defun latex-math-preview-get-page-number (dataset)
  "Get number of page for DATASET."
  (let* ((num 0) (cont t)
	 (symbol-datasets (eval `,(cdr (assq latex-math-preview-current-insert-mode
					     latex-math-preview-list-name-symbol-datasets))))
	 (max-num (length symbol-datasets)))
    (while (and cont (< num max-num))
      (if (string= dataset (car (nth num symbol-datasets))) (setq cont nil))
      (setq num (1+ num)))
    (if (not cont) (- num 1) nil)))

(defun latex-math-preview-next-candidates-for-insertion (num)
  "Next page of candidates buffer for insertion."
  (interactive "p")
  (let* ((page (latex-math-preview-get-page-number
		(cdr (assq latex-math-preview-current-insert-mode
			   latex-math-preview-current-page-of-symbol-list))))
	 (symbol-datasets (eval `,(cdr (assq latex-math-preview-current-insert-mode
					     latex-math-preview-list-name-symbol-datasets))))
	 (len (length symbol-datasets)))
    (while (< num 0) (setq num (+ num len)))
    (if page
	(let ((dataset (car (nth (% (+ page num) len) symbol-datasets))))
	  (latex-math-preview-quit-window)
	  (latex-math-preview-create-buffer-for-insertion dataset)))))

(defun latex-math-preview-previous-candidates-for-insertion (num)
  "Previous page of candidates buffer for insertion."
  (interactive "p")
  (latex-math-preview-next-candidates-for-insertion (- 0 num)))

(defun latex-math-preview-toggle-window-maximization ()
  "Toggle maximization of window displaying candidates of mathematical symbols."
  (interactive)
  (setq latex-math-preview-always-maximize-window (not latex-math-preview-always-maximize-window))
  (latex-math-preview-quit-window)
  (latex-math-preview-create-buffer-for-insertion
   (cdr (assq latex-math-preview-current-insert-mode
	      latex-math-preview-current-page-of-symbol-list))))

(defun latex-math-preview-symbol-insertion (str)
  "Execute insertion from STR."
  (let ((sym (eval `(assoc str
			   ,(cdr (assq latex-math-preview-current-insert-mode 
				       latex-math-preview-list-name-for-candidates-defined-as-list))))))
    (if sym (progn
	      (insert (car (car (cdr sym))))
	      (save-excursion
		(insert (car (cdr (cdr (car (cdr sym))))))))
      (insert str))))

(defun latex-math-preview-put-selected-candidate ()
  "Insert selected LaTeX mathematical symboled to original buffer."
  (interactive)
  (let* ((str (buffer-substring
	       (overlay-start latex-math-preview-selection-overlay-for-insertion)
	       (overlay-end latex-math-preview-selection-overlay-for-insertion))))
    (latex-math-preview-quit-window)
    (latex-math-preview-symbol-insertion str)

    (setcdr (assq latex-math-preview-current-insert-mode
		  latex-math-preview-inserted-last-symbol) str)))

(defun latex-math-preview-put-candidate-mouse-selecting (event)
  "Insert mouse selecting candidate."
  (interactive "e")
  (mouse-set-point event)
  (latex-math-preview-set-overlay-for-selected-item)
  (latex-math-preview-put-selected-candidate))

(defun latex-math-preview-delete-current-cache ()
  "Delete cache and make cache again."
  (interactive)
  (latex-math-preview-quit-window)
  (latex-math-preview-clear-cache-for-insertion
   (cdr (assq latex-math-preview-current-insert-mode
	      latex-math-preview-current-page-of-symbol-list)))
  (latex-math-preview-create-buffer-for-insertion
   (cdr (assq latex-math-preview-current-insert-mode
	      latex-math-preview-current-page-of-symbol-list))))

(defun latex-math-preview-last-symbol-again ()
  "Insert last symbol which is inserted by `latex-math-preview-insert-symbol'"
  (interactive)
  (latex-math-preview-set-current-insert-mode)
  (let ((last-symbol (cdr (assq latex-math-preview-current-insert-mode
				latex-math-preview-inserted-last-symbol))))
    (if last-symbol (latex-math-preview-symbol-insertion last-symbol))))

;;-----------------------------------------------------------------------------
;; Move to other item

(defun latex-math-preview-set-overlay-for-selected-item ()
  "Set overlay and highlight."
  (save-excursion
    (let ((start-ol))
      (skip-chars-backward "^\t")
      (setq start-ol (point))
      (re-search-forward "  " nil t)
      (skip-chars-backward " ")
      (if latex-math-preview-selection-overlay-for-insertion
	  (move-overlay latex-math-preview-selection-overlay-for-insertion start-ol (point))
	(progn
	  (setq latex-math-preview-selection-overlay-for-insertion (make-overlay start-ol (point)))
	  (overlay-put latex-math-preview-selection-overlay-for-insertion 'face latex-math-preview-selection-face-for-insertion))))))

(defun latex-math-preview-move-to-right-item ()
  "Move to right item."
  (interactive)
  (if (re-search-forward "\t\\([^\t]+\\)  " nil t)
      (progn
	(goto-char (match-beginning 1))
	(latex-math-preview-set-overlay-for-selected-item))))

(defun latex-math-preview-move-to-left-item ()
  "Move to left item."
  (interactive)
  (if (re-search-backward "\t\\([^\t]+\\)  " nil t)
      (progn
	(goto-char (match-beginning 1))
	(latex-math-preview-set-overlay-for-selected-item))))

(defun latex-math-preview-move-to-upward-item ()
  "Move to upward item."
  (interactive)
  (let ((lnum (1- (line-number-at-pos)))
	(col (current-column)))
    (if (< (nth (1- (length latex-math-preview-information-line-number))
		latex-math-preview-information-line-number) lnum)
	(progn
	  (if (not (member lnum latex-math-preview-information-line-number))
	      (forward-line -1) (forward-line -2))
	  (move-to-column col)
	  (skip-chars-backward "^\t")
	  (backward-char)
	  (latex-math-preview-move-to-right-item)))))

(defun latex-math-preview-move-to-downward-item ()
  "Move to downward item."
  (interactive)
  (let ((lnum (1+ (line-number-at-pos)))
	(col (current-column)))
    (if (< lnum (line-number-at-pos (point-max)))
	(progn
	  (if (not (member lnum latex-math-preview-information-line-number))
	      (forward-line 1) (forward-line 2))
	  (move-to-column col)
	  (skip-chars-backward "^\t")
	  (backward-char)
	  (latex-math-preview-move-to-right-item)))))

(defun latex-math-preview-move-to-current-line-first-item ()
  "Move to first item in current line."
  (interactive)
  (beginning-of-line)
  (latex-math-preview-move-to-right-item))

(defun latex-math-preview-move-to-current-line-last-item ()
  "Move to last item in current line."
  (interactive)
  (end-of-line)
  (latex-math-preview-move-to-left-item))

(defun latex-math-preview-move-to-beginning-of-candidates ()
  "Move to first candidate item in current buffer."
  (interactive)
  (goto-line (nth (1- (length latex-math-preview-information-line-number))
		  latex-math-preview-information-line-number))
  (latex-math-preview-move-to-right-item))

(defun latex-math-preview-move-to-end-of-candidates ()
  "Move to last candidate item in current buffer."
  (interactive)
  (goto-char (point-max))
  (latex-math-preview-move-to-left-item))

(defun latex-math-preview-scroll-up ()
  "Scroll up and move to nearestd item."
  (interactive)
  (let ((lnum (1+ (line-number-at-pos)))
	(col (current-column)))
    (if (< lnum (line-number-at-pos (point-max)))
	(progn
	  (scroll-up)
	  (if (member (line-number-at-pos) latex-math-preview-information-line-number)
	      (forward-line -1))
	  (move-to-column col)
	  (skip-chars-backward "^\t")
	  (backward-char)
	  (latex-math-preview-move-to-right-item)))))

(defun latex-math-preview-scroll-down ()
  "Scroll down and move to nearestd item."
  (interactive)
  (let ((start-num (nth (1- (length latex-math-preview-information-line-number))
			latex-math-preview-information-line-number))
	(lnum (1+ (line-number-at-pos)))
	(col (current-column)))
    (if (< start-num lnum)
	(progn
	  (scroll-down)
	  (cond
	   ((< (line-number-at-pos) start-num)
	    (goto-char start-num))
	   ((member (line-number-at-pos) latex-math-preview-information-line-number)
	    (forward-line 1)))
	  (move-to-column col)
	  (skip-chars-backward "^\t")
	  (backward-char)
	  (latex-math-preview-move-to-right-item)))))

(defvar latex-math-preview-insert-isearch-map
  (let ((map (copy-keymap isearch-mode-map)))
    (define-key map (kbd "<return>") 'latex-math-preview-insert-isearch-exit)
    map)
  "Keymap for latex-math-preview-insert-isearch.")

(defun latex-math-preview-insert-isearch-exit ()
  "Search insertion item."
  (interactive)
  (latex-math-preview-set-overlay-for-selected-item)
  (isearch-exit))

(defun latex-math-preview-insert-isearch-forward ()
  (interactive)
  (let ((isearch-mode-map latex-math-preview-insert-isearch-map))
    (isearch-forward)))

(defun latex-math-preview-insert-isearch-backward ()
  (interactive)
  (let ((isearch-mode-map latex-math-preview-insert-isearch-map))
    (isearch-backward)))

;;-----------------------------------------------------------------------------
;; Preview beamer frame

(defun latex-math-preview-search-beamer-frame-region ()
  (let (beg end)
    (if (and transient-mark-mode mark-active)
	(progn
	  (setq beg (region-beginning))
	  (setq end (region-end))
	  (setq mark-active nil))
      (save-excursion
	(let ((start-point (point)))
	  (when (search-backward "\\begin{frame}" nil t)
	    (setq beg (point))
	    (if (and (search-forward "\\end{frame}" nil t) (< start-point (point)))
		(setq end (point))))
	  (if (not (and beg end))
	      (progn
		(goto-char start-point)
		(when (search-backward-regexp "\\\\frame[^a-z]" nil t)
		  (setq beg (point))
		  (catch :finish-search
		    (let ((count 0))
		      (when (search-forward "{" nil t)
			(forward-char)
			(while (not (eobp))
			  (skip-chars-forward "^{}")
			  (cond
			   ((looking-at "{")
			    (forward-char)
			    (setq count (1+ count)))
			   ((looking-at "}")
			    (forward-char)
			    (if (= 0 count)
				(progn
				  (setq end (point))
				  (throw :finish-search t))
			      (setq count (1- count))))
			   (t (throw :finish-search t)))))))))))))
    (if (and beg end) (buffer-substring-no-properties beg end) nil)))

(defun latex-math-preview-search-beamer-preamble ()
  (save-excursion
    (let (preamble)
      (goto-char (point-min))
      (when (search-forward "\\begin{document}" nil t)
	(search-backward "\\begin{document}" nil t)
	(setq preamble (buffer-substring-no-properties (point-min) (point))))
      preamble)))

(defun latex-math-preview-make-temporary-beamer-tex-file ()
  (let ((preamble (latex-math-preview-search-beamer-preamble))
	(frame (latex-math-preview-search-beamer-frame-region)))
    (if (and preamble frame)
	(let ((dot-tex (latex-math-preview-create-temporary-tex-filename))
	      (coding-system buffer-file-coding-system))
	  (with-temp-file dot-tex
	    (insert preamble)
	    (insert "\\begin{document}\n" frame "\n\\end{document}")
	    (set-buffer-file-coding-system coding-system)
	    dot-tex))
      nil)))

(defun latex-math-preview-beamer-frame ()
  "Display beamer frame at current position."
  (interactive)
  (let ((dot-tex (latex-math-preview-make-temporary-beamer-tex-file)))
    (if dot-tex
	(let ((png (apply 'latex-math-preview-successive-convert dot-tex latex-math-preview-beamer-to-png)))
	  (if png
	      (progn
		(setq latex-math-preview-window-configuration (current-window-configuration))
		(latex-math-preview-png-image png))
	    (latex-math-preview-raise-can-not-create-image dot-tex)))
      (message "Here is no beamer frame.")))
  (latex-math-preview-clear-working-directory))

(provide 'latex-math-preview)

;;; latex-math-preview.el ends here
