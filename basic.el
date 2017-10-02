;;; basic.el --- major mode for editing basic and visual basic code
;;; Copyright (C) 2002 Agnar Renolen <agnar.renolen@emap.no>
;;; Copyright (C) 2009 Samuel Bronson <naesten@gmail.com>

;; basic.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

;; Version: 1.1
;; Last-Updated: Sat Apr 18 15:27:37 2009 (-0400)
;; By: Samuel Bronson

;;; Commentary:

;; An editing mode for basic code.  It knows a lot about visual basic and
;; tries to provide proper indentation and syntax highlighting.  This
;; version is fairly crude.  It provides syntax highlighting and
;; indentation.
;;
;; WISH LIST:
;;
;;  I would very much like to automatically append a "_" at the end of a
;;  line that gets overflown in auto-fill-mode.  I'll leave that to a
;;  later version.
;;
;;  I would also like to append the proper block end when the user
;;  presses enter at a block beginning.  For example, if the
;;  user typed a "for" statement, the ending "next" statement would be
;;  automatically added when the user pressed enter.


;;; Change Log:
;;
;; 2009-04-18:  Samuel Bronson (1.0)
;;      * Switch to `font-lock-syntactic-keywords' for handling of REM
;;        comments, so that other keywords on a comment line won't
;;        prevent the comment from being recognized
;;      * Explicitly create the syntax table for `basic-mode'
;;      * Use `define-derived-mode' to define the `basic-mode'
;;        command, shortening it considerably
;;      * Fix `checkdoc' warnings
;;      * Other small changes
;;
;; 2002-08-20:  Agnar Renolen (1.1)
;;      * Wrote `basic-mode'

;;; Code:


(defgroup basic nil
  "Major mode for editing basic code"
  :prefix "basic-"
  :group 'languages)

(defcustom basic-mode-hook nil
  "Hook called when `basic-mode' fires up."
  :type 'hook
  :group 'basic)

(defvar basic-mode-map nil
  "Keymap used with basic code.")

(defcustom basic-indent-level 4
  "Amount by which basic subexpressions are indented."
  :type 'integer
  :group 'basic)

(defvar basic-font-lock-keywords
  (eval-when-compile
    (list
     ;; function name declarations.
     '("\\<\\(sub\\|function\\)\\>[ \t]*\\(\\sw+\\)?"
       (1 font-lock-keyword-face) (2 font-lock-function-name-face nil t))
     ;; variable name declaration.
     '("\\<\\(dim\\|byref\\|byval\\)\\>[ \t]*\\(\\sw+\\)?"
       (1 font-lock-keyword-face) (2 font-lock-variable-name-face nil t))
     ;; keywords.
     (concat "\\<\\(a\\(ddressof\\|nd\\|s\\|ttribute\\)\\|"
	     "b\\(inary\\|y\\(ref\\|val\\)\\)\\|"
	     "c\\(a\\(ll\\|se\\)\\|onst\\)\\|"
	     "d\\(e\\(clare\\|f\\(bool\\|byte\\|"
	     "int\\|lng\\|cur\\|sng\\|dbl\\|dec\\|"
	     "date\\|str\\|obj\\|var\\)\\)\\|im\\|o\\)\\|"
	     "e\\(ach\\|lse\\(\\|if\\)\\|n\\(d\\|um\\)"
	     "\\|qv\\|rror\\|xit\\|vent\\)\\|"
	     "f\\(or\\|unction\\)\\|"
	     "g\\(et\\|o\\(sub\\|to\\)\\)\\|"
	     "i\\(f\\|m\\(p\\(\\|lements\\)\\)\\|[ns]\\)\\|"
	     "l\\(e[nt]\\|i\\(ke\\|b\\)\\|oop\\)\\|"
	     "m\\(e\\|od\\)\\|"
	     "n\\(ame\\|e\\(w\\|xt\\)\\|ot\\)\\|"
	     "o\\([nr]\\|ption\\(\\|al\\)\\)\\|"
	     "pr\\(eserve\\|operty\\)\\|"
	     "r\\(e\\(dim\\|sume\\)\\|set\\)\\|"
	     "s\\(e\\(ek\\|lect\\|t\\)\\|t\\(ep\\|op\\)\\|ub\\)\\|"
	     "t\\(hen\\|ime\\|o\\|ype\\)\\|"
	     "until\\|w\\(end\\|ithevents\\|hi\\(le\\|th\\)\\)\\|"
	     "xor\\)\\>")
     ;; storage specifiers
     '("\\(friend\\|global\\|p\\(rivate\\|ublic\\)\\|static\\)"
       . font-lock-keyword-face)
     ;; constants
     '("\\<\\(empty\\|false\\|nothing\\|null\\|true\\)\\>"
       . font-lock-constant-face)
     ;; standard types
     '("\\<\\(b\\(oolean\\|yte\\)\\|currency\\|d\\(ate\\|ecimal\\|ouble\\)\\|integer\\|long\\|object\\|s\\(ingle\\|tring\\)\\|variant\\)\\>"
       . font-lock-type-face)
      )))

(defvar basic-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\' "<" st)
    (modify-syntax-entry ?\n ">" st)
    (modify-syntax-entry ?\_ "w" st)
    st))

(defvar basic-font-lock-syntactic-keywords
  '(("^[ \t]*\\<rem\\>" . (0 "<"))))

;;;###autoload
(define-derived-mode basic-mode nil "Basic"
  "Major mode for editing basic code."

  (set (make-local-variable 'indent-line-function) 'basic-indent-line)
  (set (make-local-variable 'comment-start) "'")
  (set (make-local-variable 'comment-start-skip) "'[ \t]*")
  (set (make-local-variable 'font-lock-defaults)
       '(basic-font-lock-keywords
         ;; keywords-only: OFF
         nil
         ;; case-fold:     ON
         t
         ;; syntax-alist
         nil
         ;; syntax-begin - func. move outside syntactic block?
         nil
         ;; buffer-local font-lock vars
         (font-lock-syntactic-keywords . basic-font-lock-syntactic-keywords)))
  )
  
(defun basic-indent-line ()
  "Indent current line as basic script."
  (let ((indent (basic-calculate-indent))
	beg shift-amt
	(old-pos (- (point-max) (point))))
    (beginning-of-line)
    (setq beg (point))
    (skip-chars-forward " \t")
    (if (looking-at "case\\|e\\(lse\\(\\|if\\)\\|nd\\)\\|loop\\|next\\|wend\\>")
	(setq indent (max (- indent basic-indent-level))))
    (message "prev indent: %d" indent)
    (setq shift-amt (- indent (current-column)))
    (if (not (zerop shift-amt))
	(progn
	  (delete-region beg (point))
	  (indent-to indent)
	  (if (> (- (point-max) old-pos) (point))
	      (goto-char (- (point-max) old-pos)))))
    shift-amt))
    
			  
(defun basic-calculate-indent ()
  "Return appropriate indentation for the current line as basic code."
  (save-excursion
    (beginning-of-line)
    (current-indentation)
    (if (bobp)
	0
      (if (re-search-backward "^[ \t]*[^ \t\n\r]" nil t)
	  (if (looking-at (concat "[ \t]*\\(case\\|do\\|for\\|if\\|select"
				  "\\|while\\|\\(\\(public\\|private"
				  "\\|static\\|declare\\|friend\\)[ \t]+\\)*"
				  "\\(function\\|property\\|enum\\|sub"
				  "\\|type\\)\\)\\>"))
	      (+ (current-indentation) basic-indent-level)
	    (current-indentation))
	0))))
  
(provide 'basic)
;;; basic.el ends here
