;;; ts-mode.el --- An Emacs major mode for editing TypoScript files

;; Copyright (C) 2009  Joachim Mathes
;;
;; Author: Joachim Mathes <joachim <underscore> mathes <at> web <dot> de>
;; Created: July 2009
;; Version: 0.1
;; Last-Updated: Thu Aug 13 00:18:18 CEST 2009
;;           By: Joachim Mathes
;;     Update #: 4
;; Keywords: files
;; URL: http://www.emacswiki.org/emacs/ts-mode.el
;; EmacsWiki: TypoScriptMode

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

;; Installation:

;; To install just drop this file into a directory on your load-path and
;; byte-compile it.  To set up Emacs to automatically edit files ending in ".ts"
;; using ts-mode add the following to your ~/.emacs file (GNU Emacs) or
;; ~/.xemacs/init.el file (XEmacs):
;;    (setq auto-mode-alist (cons '("\\.ts$" . ts-mode) auto-mode-alist))
;;    (autoload 'ts-mode "ts-mode" "TypoScript file editing mode." t)

;; Description:

;; This is a major mode for editing TypoScript input files.  It is developed to
;; support syntax highlighting, indentation and folding of blocks.

;; This file is *NOT* part of GNU Emacs.

;;; History:
;; 

;;; Code:

(defconst ts-version "0.1"
  "`ts-mode' version number.")

;; User definable variables
;; vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv

(defgroup typoscript nil
  "Major mode for editing TypoScript files."
  :prefix "ts-"
  :group 'languages)

(defcustom ts-newline-function 'newline-and-indent
  "Function to be called upon pressing `RET'."
  :type '(choice (const newline)
		 (const newline-and-indent)
		 (const reindent-then-newline-and-indent))
  :group 'typoscript)

(defcustom ts-block-indentation 2
  "The indentation relative to a predecessing line which begins a new block.

  In TypoScript blocks start with the left parenthesis `(' or the left brace
  `{'."
  :type 'integer
  :group 'typoscript)

(defcustom ts-fold-foreground-color "white"
  "The foreground color used to highlight the folded block.

  The default value is `white'.  For a list of all available colors use `M-x
list-colors-display'"
  :type 'color
  :group 'typoscript)

(defcustom ts-fold-background-color "DodgerBlue1"
  "The background color used to highlight the folded block.

  The default value is `DodgerBlue1'.  For a list of all available colors use
`M-x list-colors-display'"
  :type 'color
  :group 'typoscript)

;; Internal variables
;; vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv

(defvar ts-mode-hook nil
  "Hook called by `ts-mode'.")

(defvar ts-classes-face 'ts-classes-face
  "Face for TypoScript classes.")
(make-face 'ts-classes-face)

(defvar ts-path-face 'ts-path-face
  "Face for TypoScript paths.")
(make-face 'ts-path-face)

(defvar ts-block-face 'ts-block-face
  "Face for TypoScript blocks.")
(make-face 'ts-block-face)

(defvar ts-conditional-face 'ts-conditional-face
  "Face for TypoScript conditionals.")
(make-face 'ts-conditional-face)

(defvar ts-html-face 'ts-html-face
  "Face for TypoScript HTML tags.")
(make-face 'ts-html-face)

(defun ts-font-lock-mode-hook ()
  "Defines a TypoScript font lock mode hook."
  (or (face-differs-from-default-p 'ts-classes-face)
      (copy-face 'font-lock-keyword-face 'ts-classes-face))
  (copy-face 'font-lock-builtin-face 'ts-path-face)
  (set-face-foreground 'ts-path-face
		       "DarkTurquoise" nil)
  (copy-face 'font-lock-builtin-face 'ts-block-face)
  (set-face-foreground 'ts-block-face
		       "DodgerBlue1" nil)
  (copy-face 'font-lock-builtin-face 'ts-conditional-face)
  (set-face-foreground 'ts-conditional-face
		       "maroon" nil)
  (copy-face 'font-lock-builtin-face 'ts-html-face)
  (set-face-foreground 'ts-html-face
		       "ForestGreen" nil))

(defvar ts-font-lock-keywords
  (let ((kw1 (mapconcat 'identity
			;; Basic TypoScript classes
			'("CONFIG"   "PAGE"  "TEXT"       "COA"  "COA_INT"
			  "FILE"     "IMAGE" "GIFBUILDER" "CASE" "TEMPLATE"
			  "HMENU"    "GMENU" "CONTENT")
			"\\|")))
    (list
     ;; Paths
     '("^[ \t]*\\([[:alnum:]-_\\.]+\\)[ \t]*[=<>]" 1 'ts-path-face)
     ;; Blocks
     '("^[ \t]*\\([[:alnum:]-_\\.]+\\)[ \t]*[{(]" 1 'ts-block-face)
     ;; Periods
     ;;'("^[ \t]*" "\\(\\.\\)" nil nil (1 'default t))
     ;; Classes (keywords)
     (list (concat "\\<\\(" kw1 "\\)\\>") 1 'ts-classes-face t)
     ;; Conditional expressions `[...]'
     '("^[ \t]*\\(\\[.+?\\]\\)[ \t]*$" 1 'ts-conditional-face)
     ;; Comment lines beginning with hash symbol `#'
     '("^[ \t]*\\(#.*\\)$" 1 'font-lock-comment-face)
     ;; HTML special character encodings on the right side of the operator
     '("\\(=\\|=<\\|>\\|:=\\)" "\\(&[#[:alnum:]]+;\\)" nil nil (0 'ts-html-face))
     ;; HTML tags
     '("=<?\\|>\\|:=\\|[ \t]*" "\\(<[^<]+?>\\)" nil nil (0 'ts-html-face))
     ;; HTML color definitions
     '("#[[:xdigit:]]\\{6\\}[ \t\n]+" 0 'ts-html-face t)))
  "Expressions to highlight in TypoScript mode.")

(defvar ts-mode-syntax-table nil
  "Syntax table used in TypoScript Mode buffers.")

(defvar ts-mode-map ()
  "Key map used in TypoScript Mode buffers.")

(defvar ts-highlight-overlays [nil nil]
  "A vector of different overlay to do highlighting.
This vector concerns only highlighting of horizontal lines.")

;; Functions
;; vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv

;;;###autoload
(defun ts-mode ()
  "Major mode for editing TypoScript files.
Bug reports, suggestions for new features and critics should go to
`joachim_mathes@web.de'.

This mode knows about syntax highlighting, indentation and folding of
blocks.

COMMANDS
\\{ts-mode-map}
VARIABLES

ts-newline-function\t\tbehaviour after pressing `RET'
ts-block-indentation\t\tindentation value
ts-fold-foreground-color\t\tforeground color of folded measurement block
ts-fold-background-color\t\tbackground color of folded measurement block"
  (interactive)
  ;; Set up local variables
  (kill-all-local-variables)
  (make-local-variable 'font-lock-defaults)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-start-skip)
  (make-local-variable 'indent-line-function)
  (make-local-variable 'defun-prompt-regexp)

  (when (not ts-mode-syntax-table)
    (setq ts-mode-syntax-table (make-syntax-table))
    ;; Parenthesis, brackets and braces
    (modify-syntax-entry ?\( "()" ts-mode-syntax-table)
    (modify-syntax-entry ?\) ")(" ts-mode-syntax-table)
    (modify-syntax-entry ?\[ "(]" ts-mode-syntax-table)
    (modify-syntax-entry ?\] ")[" ts-mode-syntax-table)
    (modify-syntax-entry ?\{ "(}" ts-mode-syntax-table)
    (modify-syntax-entry ?\} "){" ts-mode-syntax-table)
    ;; Comment delimiters
    (modify-syntax-entry ?/ ". 124b" ts-mode-syntax-table)
    (modify-syntax-entry ?* ". 23" ts-mode-syntax-table)
    (modify-syntax-entry ?\n "> b" ts-mode-syntax-table)
    (modify-syntax-entry ?\" "." ts-mode-syntax-table)
    (modify-syntax-entry ?. "." ts-mode-syntax-table))

  (set-syntax-table ts-mode-syntax-table)

  (add-hook 'font-lock-mode-hook 'ts-font-lock-mode-hook)

  (setq defun-prompt-regexp "^[ \t]*\\([[:alnum:]-_\\.]+\\)[ \t]*")

  (if ts-mode-map
      nil
    (setq ts-mode-map (make-sparse-keymap))
    (define-key ts-mode-map "\r"           'ts-newline)
    (define-key ts-mode-map "\C-c\C-e"     'ts-fold-block)
    (define-key ts-mode-map "\C-c\C-a"     'ts-unfold-block)
    (define-key ts-mode-map "\C-c\C-u\C-r" 'ts-unfold-region)
    (define-key ts-mode-map "\C-c\C-u\C-b" 'ts-unfold-buffer)
    (define-key ts-mode-map "}" 'ts-electric-brace)
    (define-key ts-mode-map ")" 'ts-electric-brace))
  (use-local-map ts-mode-map)

  (setq major-mode           'ts-mode
	mode-name            "TypoScript"
	font-lock-defaults   '(ts-font-lock-keywords)
	comment-start        "# "
	comment-end          ""
	comment-start-skip   "# "
	indent-line-function 'ts-indent-line)

  ;; Run the mode hook.
  (if ts-mode-hook
      (run-hooks 'ts-mode-hook)))

(defun ts-newline ()
  "Call the dedicated newline function.

The variable `ts-newline-function' decides which newline function to
use."
  (interactive)
  (funcall ts-newline-function))

(defun ts-indent-line ()
  "Indent current line for TypoScript mode."
  (let ((cp (point))                ; current point
	(cc (current-column))       ; current column
	(ci (current-indentation))  ; current indentation
	(cl (line-number-at-pos))   ; current line
	(counter 0)
	ps                          ; parser state
	psp			    ; parser state position
	save-indent-column)

    ;; Evaluate parser state
    (save-excursion
      (beginning-of-line)
      (setq ps (ts-parser-state))

      (cond
       ;; Check if parser state position is:
       ;; -> Inside a comment
       ((nth 8 ps)
	(setq psp (nth 8 ps))
	(goto-char psp)
	(setq save-indent-column (+ (current-column)
				    1)))
       ;; Check if parser state position is:
       ;; -> Inside a parenthetical grouping
       ((nth 1 ps)
	(setq psp (nth 1 ps))
	(cond
	 ;; Check if point is looking at a string and a closing curly brace
	 ((looking-at "[ \t[:alnum:]]*[)}]")
	  (goto-char psp)
	  (back-to-indentation)
	  (setq save-indent-column (current-column)))
	 (t
	  (goto-char psp)
	  (back-to-indentation)
	  (setq save-indent-column (+ (current-column)
				      ts-block-indentation)))))
       ;; Check if parser state position is:
       ;; -> nil
       (t
       	;; Skip empty lines
       	(forward-line -1)
       	(while (and (looking-at "^[ \t]*\n")
       		    (not (bobp)))
       	  (forward-line -1))
       	(back-to-indentation)
	(setq save-indent-column (current-column)))))

    ;; Set indentation value on current line
    (back-to-indentation)
    (backward-delete-char-untabify (current-column))
    (indent-to save-indent-column)
    (if (> cc ci)
	(forward-char (- cc ci)))))

(defun ts-parser-state ()
  "Return the parser state at point."
  (save-excursion
    (let ((here (point))
	  sps)
      ;; For correct indentation the character position of the start of the
      ;; innermost parenthetical grouping has to be found.
      (goto-char (point-min))
      ;; Now get the parser state, i.e. the depth in parentheses.
      (save-excursion
	(setq sps (parse-partial-sexp (point) here)))
      sps)))

(defun ts-block-start ()
  "Return buffer position of the last unclosed enclosing block.

If nesting level is zero, return nil."
  (let ((status (ts-parser-state)))
    (if (<= (car status) 0)
	nil
    (car (cdr status)))))

;; Electric characters

(defun ts-electric-brace (arg)
  "Insert closing brace.
Argument ARG prefix."
  (interactive "*P")
  ;; Insert closing brace.
  (self-insert-command (prefix-numeric-value arg))

  (when (and (looking-at "[ \t]*$")
	     (looking-back "^[ \t]*[})]"))
    (ts-indent-line)))

;; Folding

(defun ts-fold-block ()
  "Hide the block on which point currently is located."
  (interactive)
  (let ((current-point (point))
	(block-start (ts-block-start)))

    (if (not block-start)
	(message "Point is not within a block.")

      ;; Look for block start
      (save-excursion
	(goto-char (ts-block-start))
	(beginning-of-line)
	(setq block-start (point)))
    
      (when block-start
	(let ((block-name
	       ;; Save block name
	       (save-excursion
		 (goto-char block-start)
		 (beginning-of-line)
		 (looking-at
		  "^[ \t]*\\(.*?\\)[ \t]*{")
		 (match-string 1)))
	      (block-end
	       ;; Look for block end
	       (save-excursion
		 (goto-char block-start)
		 (forward-list)
		 (point)))
	      ;; Variable for overlay
	      skampi-overlay)

	  ;; ------------------------------------------------------------------
	  ;; The following local variables are defined up to here:
	  ;; [1] block-start: point of block start, at the beginning
	  ;;                  of the line; nil otherwise
	  ;; [2] block-name : name of block, i.e. the object path
	  ;; [3] block-end  : point of block end, at the end of the
	  ;;                  line which contains the closing curly brace `}
	  ;; ------------------------------------------------------------------

	  ;; Check if end of measurement block is beyond point;
	  ;; call fold function otherwise
	  (if (>= block-end current-point)
	      (ts-fold block-start block-end block-name)
	    (message "Error: No valid block found."))

	  ;; Indent overlay
	  (goto-char block-start)
	  (beginning-of-line)
	  (ts-indent-line))))))

(defun ts-fold (block-start block-end block-name)
  "Fold block.

The block starts at BLOCK-START and ends at BLOCK-END.  Its
BLOCK-NAME is the TypoScript object path."
  (let (ts-overlay)
    ;; Check if block-start and block-end are valid values, i.e. not nil
    (if (or (eq block-start nil)
	    (eq block-end nil))
	(message "Error: No valid block found.")
      ;; Make an overlay and hide block
      (setq ts-overlay (make-overlay block-start block-end
					     (current-buffer) t nil))
      (overlay-put ts-overlay 'category 'ts-fold)
      (overlay-put ts-overlay 'evaporate t)
      (overlay-put ts-overlay 'mouse-face 'highlight)
      (overlay-put ts-overlay 'display (concat "["
					       (propertize block-name
							   'face
							   nil)
					       "]"))
      (overlay-put ts-overlay 'font-lock-face `(:foreground ,ts-fold-foreground-color
							    :background ,ts-fold-background-color))
      (overlay-put ts-overlay 'help-echo (concat
					  "Folded block: "
					  block-name)))))

(defun ts-unfold-buffer ()
  "Unfold all blocks in the buffer."
  (interactive)
  (ts-unfold-region (point-min) (point-max)))

(defun ts-unfold-region (start end)
  "Unfold all blocks in the region.

The region delimiters are START and END."
  (interactive "r")
  (let ((ts-overlays (overlays-in start end)))
    (ts-unfold-overlays ts-overlays)))

(defun ts-unfold-block ()
  "Unfold block at point."
  (interactive)
  (let ((ts-overlays (overlays-at (point))))
    (ts-unfold-overlays ts-overlays)))

(defun ts-unfold-overlays (ts-overlays)
  "Unfold all overlays set by ts-fold in TS-OVERLAYS.

Return non-nil if an unfold happened, nil otherwise."
  (let (found)
    (dolist (overlay ts-overlays)
      (when (eq (overlay-get overlay 'category) 'ts-fold)
	(delete-overlay overlay)
	(setq found t)))
    found))

(provide 'ts-mode)

;;; ts-mode.el ends here
