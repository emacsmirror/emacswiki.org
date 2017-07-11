;;; pic-basic-mode.el --- Editing mode for MELabs PIC Basic

;; This file is not part of Emacs

;; Copyright (C) 1996 Fred White <fwhite@alum.mit.edu>
;; Copyright (C) 1998 Free Software Foundation, Inc.
;;   (additions by Dave Love)
;; Copyright (C) 2008-2009 Free Software Foundation, Inc.
;;   (additions by Randolph Fritz and Vincent Belaiche (VB1) )
;; Copyright (C) 2010 Free Software Foundation, Inc.
;;   (additions by John Pritchard)

;; Author: Fred White <fwhite@alum.mit.edu>
;; Adapted-by: Dave Love <d.love@dl.ac.uk>
;;           : Kevin Whitefoot <kevin.whitefoot@nopow.abb.no>
;;           : Randolph Fritz <rfritz@u.washington.edu>
;;           : Vincent Belaiche (VB1) <vincentb1@users.sourceforge.net>
;; Adapted-by: John Pritchard <jdp@syntelos.org>
;; Version: 1.5.2 (2011-01-07)
;; Keywords: languages, basic
;; X-URL:  http://www.emacswiki.org/cgi-bin/wiki/pic-basic-mode.el

;;{{{ GPL

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3,
;; or (at your option) any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with program. If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;
;; Visit <http://www.gnu.org/copyleft/gpl.html> for more information

;;}}}
;;{{{ Installation

;;
;;      To install this mode, put this file on your Emacs `load-path'
;;      (or extend the load path to include the directory containing
;;      this file) and optionally byte compile it.
;;
;;      For autoload installation, insert the following into your
;;      .emacs file.
;;
;;          (autoload 'pic-basic-mode "pic-basic-mode" "PIC Basic mode" t)
;;          (setq auto-mode-alist (append '(("\\.\\(bas\\)$" .
;;          				 pic-basic-mode)) auto-mode-alist))

;;}}}

;;; Commentary:
;; 

;;; History:
;; 

;; basic-mode by Fred White
;; 
;; visual-basic-mode by Dave Love
;; 
;; pic-basic-mode by John Pritchard

;;; Known bugs:
;;  Couple, see comments.
;;

;;; Code:
(eval-when-compile (require 'cl))

(defvar pic-basic-xemacs-p (string-match "XEmacs\\|Lucid" (emacs-version)))
(defvar pic-basic-winemacs-p (string-match "Win-Emacs" (emacs-version)))
(defvar pic-basic-win32-p (eq window-system 'w32))

;;
;;{{{ Customization
;;
;;     The default configuration may be modified here, or in your .emacs
;;

(defgroup pic-basic nil
  "Customization of the Pic Basic mode."
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :group 'languages   )

(defcustom pic-basic-mode-indent 4
  "*Default indentation per nesting level."
  :type 'integer
  :group 'pic-basic)

(defcustom pic-basic-fontify-p t
  "*Whether to fontify Basic buffers."
  :type 'boolean
  :group 'pic-basic)

(defcustom pic-basic-wild-files "*.bas"
  "*Wildcard pattern for BASIC source files."
  :type 'string
  :group 'pic-basic)

(defcustom pic-basic-allow-single-line-if t
  "*Whether to allow single line if."
  :type 'boolean
  :group 'pic-basic)

(defcustom pic-basic-use-untabify t
  "*Whether to run untabify on write file (convert tab chars to spaces)."
  :type 'boolean
  :group 'pic-basic)

(defcustom pic-basic-auto-check-style-level -1
  "Select which style errors are automatically corrected.

* -1 : all errors correction need confirmation by user

*  0 : punctuation errors are automatically corrected"
  :type 'integer
  :group 'pic-basic)

;;}}}
;;
;;
(defvar pic-basic-mode-syntax-table nil)
(if pic-basic-mode-syntax-table
    ()
  (setq pic-basic-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\' "\<" pic-basic-mode-syntax-table) ; Comment starter
  (modify-syntax-entry ?\n ">" pic-basic-mode-syntax-table)
  (modify-syntax-entry ?\\ "w" pic-basic-mode-syntax-table)
  (modify-syntax-entry ?_ "_" pic-basic-mode-syntax-table)
  ; Make operators puncutation so that regexp search \_< and \_> works properly
  (modify-syntax-entry ?+ "." pic-basic-mode-syntax-table)
  (modify-syntax-entry ?- "." pic-basic-mode-syntax-table)
  (modify-syntax-entry ?* "." pic-basic-mode-syntax-table)
  (modify-syntax-entry ?/ "." pic-basic-mode-syntax-table)
  (modify-syntax-entry ?\\ "." pic-basic-mode-syntax-table)
  ; Make =, etc., punctuation 
  (modify-syntax-entry ?\= "." pic-basic-mode-syntax-table)
  (modify-syntax-entry ?\< "." pic-basic-mode-syntax-table)
  (modify-syntax-entry ?\> "." pic-basic-mode-syntax-table))


(defvar pic-basic-mode-map nil)
(if pic-basic-mode-map
    ()
  (setq pic-basic-mode-map (make-sparse-keymap))
  (define-key pic-basic-mode-map "\t" 'pic-basic-indent-line)
  (define-key pic-basic-mode-map "\r" 'pic-basic-newline-and-indent)
  (define-key pic-basic-mode-map "\M-\r" 'pic-basic-insert-item)
  (define-key pic-basic-mode-map "\C-c\C-j" 'pic-basic-insert-item)
  (define-key pic-basic-mode-map "\M-\C-a" 'pic-basic-beginning-of-defun)
  (define-key pic-basic-mode-map "\M-\C-e" 'pic-basic-end-of-defun)
  (define-key pic-basic-mode-map "\M-\C-h" 'pic-basic-mark-defun)
  (define-key pic-basic-mode-map "\M-\C-\\" 'pic-basic-indent-region)
  (define-key pic-basic-mode-map "\M-q" 'pic-basic-fill-or-indent)
  (define-key pic-basic-mode-map "\M-\C-j" 'pic-basic-split-line)
  (define-key pic-basic-mode-map "\C-c]" 'pic-basic-close-block)

  (if pic-basic-xemacs-p
      (progn
        (define-key pic-basic-mode-map "\M-G" 'pic-basic-grep)
        (define-key pic-basic-mode-map '(meta backspace) 'backward-kill-word))))


;; 

(defvar pic-basic-mode-hook ())

;; subroutine

(defconst pic-basic-defun-start-regexp  "^\\([a-zA-Z0-9_]+\\):[ \t]*.*$")

(defconst pic-basic-defun-end-regexp
  (concat
   "^\\(\\(.*[ \t]+[Rr]eturn[ \t\r\n]+\\)\\|\\("
   pic-basic-defun-start-regexp
   "\\)\\):"
   )
  )

;; declarations (bug?)

(defconst pic-basic-dim-regexp
  "^[ \t]*\\(\\w+\\)[ \t]+\\([Cc]on\\|[Vv]ar\\)[ \t]*\\(\\w*\\)"  )

;;

(defconst pic-basic-if-regexp
  "^[ \t]*#?[Ii]f[ \t]+.*[ \t_]+")

(defconst pic-basic-ifthen-regexp "^[ \t]*#?[Ii]f.+\\<[Tt]hen\\>\\s-\\S-+")

(defconst pic-basic-else-regexp "^[ \t]*#?[Ee]lse")
(defconst pic-basic-endif-regexp "[ \t]*#?[Ee]nd[Ii]f")

(defconst pic-basic-looked-at-continuation-regexp   "_[ \t]*$")

(defconst pic-basic-continuation-regexp
  (concat "^.*" pic-basic-looked-at-continuation-regexp))

(defconst pic-basic-select-regexp "^[ \t]*[Ss]elect[ \t]+[Cc]ase\\_>")
(defconst pic-basic-case-regexp "^\\([ \t]*\\)[Cc]ase\\_>")
(defconst pic-basic-case-else-regexp "^\\([ \t]*\\)[Cc]ase\\(\\s-+[Ee]lse\\)\\_>")
(defconst pic-basic-select-end-regexp "^\\([ \t]*\\)[Ee]nd[ \t]+[Ss]elect\\_>")


(defconst pic-basic-for-regexp "^[ \t]*[Ff]or\\b")
(defconst pic-basic-next-regexp "^[ \t]*[Nn]ext\\b")

(defconst pic-basic-do-regexp "^[ \t]*[Dd]o\\b")
(defconst pic-basic-loop-regexp "^[ \t]*[Ll]oop\\b")

(defconst pic-basic-while-regexp "^[ \t]*[Ww]hile\\b")
(defconst pic-basic-wend-regexp "^[ \t]*[Ww]end\\b")


(defconst pic-basic-begin-regexp "^[ \t]*[Bb]egin)?")

;; This has created a bug.  End on its own in code should not outdent.
;; How can we fix this?  They are used in separate Lisp expressions so
;; add another one.
(defconst pic-basic-end-begin-regexp "^[ \t]*[Ee]nd")

(defconst pic-basic-with-regexp "^[ \t]*[Ww]ith\\b")
(defconst pic-basic-end-with-regexp "^[ \t]*[Ee]nd[ \t]+[Ww]ith\\b")

(defconst pic-basic-blank-regexp "^[ \t]*$")
(defconst pic-basic-long-comment-regexp "^[ \t]*\\s*[';].*$")

;; PBP 1.08
;;
(eval-and-compile
  (defvar pic-basic-all-keywords
    '("abs" "adcin" "and" "asm"
      "byte" "begin" "boolean" "branch" "branchl" "button"
      "call" "case" "clear" "clearwdt" "con" "cos" "count"
      "data" "dcd" "debug" "debugin" "define" "dig" "disable" "div32" "do" "dtfmout"
      "eeprom" "else" "enable" "end" "endasm" "endif" "erasecode"
      "for" "freqout"
      "gosub" "goto" 
      "high" "hpwm" "hserin" "hserin2" "hserout" "hserout2" 
      "i2cread" "i2cwrite" "if" "input" "interrupt"
      "lcdin" "lcdout" "let" "long" "lookdown" "lookdown2" "lookup" "lookup2" "low"
      "min" "max"
      "nap" "ncd"
      "on" "or" "output" "owin" "owout" 
      "pause" "pauseus" "peek" "peekcode" "poke" "pokecode" "pot" "pulsin" "pulsout" "pwm" 
      "random" "rctime" "read" "readcode" "repeat" "resume" "return" "rev" "reverse"
      "select" "serin" "serin2" "serout" "serout2" "shiftin" "shiftout" "sleep" "sin" "sound" "sqr" "stop" "swap" 
      "toggle" "then" 
      "usbin" "usbinit" "usbout" "usbservice" 
      "var"
      "until" 
      "wend" "while" "word" "write" "writecode" 
      "xin" "xout")))

(defvar pic-basic-font-lock-keywords-1
  (eval-when-compile
    (list
     ;; Names of functions.
     (list pic-basic-defun-start-regexp
           '(1 font-lock-function-name-face))

     ;; Comments
     (list "\\([';].*\\)" 1 'font-lock-comment-face)

     ;; ASM strings
     (list "^[ \t]*\\(@[^';]*\\)" 1 'font-lock-keyword-face)

     ;; Case values
     ;; String-valued cases get font-lock-string-face regardless.
     (list "^[ \t]*case[ \t]+\\([^:'\n]+\\)" 1 'font-lock-keyword-face t)

     ;; Any keywords you like.
     (list (regexp-opt
            '("true" "false") 'words)
           1 'font-lock-keyword-face))))

(defvar pic-basic-font-lock-keywords-2
  (append pic-basic-font-lock-keywords-1
          (eval-when-compile
            `((, (regexp-opt pic-basic-all-keywords 'words)
                 1 font-lock-keyword-face)))))

(defvar pic-basic-font-lock-keywords pic-basic-font-lock-keywords-1)


(put 'pic-basic-mode 'font-lock-keywords 'pic-basic-font-lock-keywords)

;;;###autoload
(defun pic-basic-mode ()
  "A mode for editing MELabs PIC Basic programs.
Features automatic indentation and font locking.
Commands:
\\{pic-basic-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map pic-basic-mode-map)
  (setq major-mode 'pic-basic-mode)
  (setq mode-name "Pic Basic")
  (set-syntax-table pic-basic-mode-syntax-table)

  (if pic-basic-use-untabify
      (add-hook 'local-write-file-hooks 'pic-basic-untabify))

  (make-local-variable 'comment-start)
  (setq comment-start "' ")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "'+ *")
  (make-local-variable 'comment-column)
  (setq comment-column 40)
  (make-local-variable 'comment-end)
  (setq comment-end "")

  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'pic-basic-indent-line)

  (if pic-basic-fontify-p
      (pic-basic-enable-font-lock))

  ;; doing this here means we need not check to see if it is bound later.
  (add-hook 'find-file-hooks 'pic-basic-load-associated-files)

  (run-hooks 'pic-basic-mode-hook))


(defun pic-basic-enable-font-lock ()
  "Enable font locking."
  ;; Emacs 19.29 requires a window-system else font-lock-mode errs out.
  (cond ((or pic-basic-xemacs-p window-system (not (string-equal (emacs-version) "19.29")))

         ;; In win-emacs this sets font-lock-keywords back to nil!
         (if pic-basic-winemacs-p
             (font-lock-mode 1))

         ;; Accomodate emacs 19.29+
         ;; From: Simon Marshall <Simon.Marshall@esrin.esa.it>
         (cond ((boundp 'font-lock-defaults)
                (make-local-variable 'font-lock-defaults)
                (setq font-lock-defaults
                      `((pic-basic-font-lock-keywords
                         pic-basic-font-lock-keywords-1
                         pic-basic-font-lock-keywords-2)
                        nil t ((,(string-to-char "_") . "w")))))
               (t
                (make-local-variable 'font-lock-keywords)
                (setq font-lock-keywords pic-basic-font-lock-keywords)))

         (if pic-basic-winemacs-p
             (font-lock-fontify-buffer)
           (font-lock-mode 1)))))


(defun pic-basic-in-code-context-p ()
  "Predicate true when pointer is in code context."
  (save-match-data
  (if (fboundp 'buffer-syntactic-context) ; XEmacs function.
      (null (buffer-syntactic-context))
    ;; Attempt to simulate buffer-syntactic-context
    ;; I don't know how reliable this is.
    (let* ((beg (save-excursion
                  (beginning-of-line)
                  (point)))
           (list
            (parse-partial-sexp beg (point))))
      (and (null (nth 3 list))          ; inside string.
	     (null (nth 4 list)))))))      ; inside comment


(defun pic-basic-newline-and-indent (&optional count)
  "Insert a newline, updating indentation.  Argument COUNT is ignored."
  (interactive)
  (save-excursion
    (pic-basic-indent-line))
  (call-interactively 'newline-and-indent))

(defun pic-basic-beginning-of-defun ()
  "Set the pointer at the beginning of the Sub/Function/Property within which the pointer is located."
  (interactive)
  (re-search-backward pic-basic-defun-start-regexp))

(defun pic-basic-end-of-defun ()
  "Set the pointer at the beginning of the Sub/Function/Property within which the pointer is located."
  (interactive)
  (re-search-forward pic-basic-defun-end-regexp))

(defun pic-basic-mark-defun ()
  "Set the region pointer around Sub/Function/Property within which the pointer is located."
  (interactive)
  (beginning-of-line)
  (pic-basic-end-of-defun)
  (set-mark (point))
  (pic-basic-beginning-of-defun)
  (if pic-basic-xemacs-p
      (zmacs-activate-region)))

(defun pic-basic-indent-defun ()
  "Indent the function within which the pointer is located.  This has a border on mark."
  ;; VB1 to Lennart: is border effect on mark an issue ?
  (interactive)
  (save-excursion
    (pic-basic-mark-defun)
    (call-interactively 'pic-basic-indent-region)))


(defun pic-basic-fill-long-comment ()
  "Fills block of comment lines around point."
  ;; Derived from code in ilisp-ext.el.
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((comment-re "^[ \t]*\\s[';]+[ \t]*"))
      (if (looking-at comment-re)
          (let ((fill-prefix
                 (buffer-substring
                  (progn (beginning-of-line) (point))
                  (match-end 0))))

            (while (and (not (bobp))
                        (looking-at pic-basic-long-comment-regexp))
              (forward-line -1))
            (if (not (bobp)) (forward-line 1))

            (let ((start (point)))

              ;; Make all the line prefixes the same.
              (while (and (not (eobp))
                          (looking-at comment-re))
                (replace-match fill-prefix)
                (forward-line 1))

              (if (not (eobp))
                  (beginning-of-line))

              ;; Fill using fill-prefix
              (fill-region-as-paragraph start (point))))))))


(defun pic-basic-fill-or-indent ()
  "Fill long comment around point, if any, else indent current definition."
  (interactive)
  (cond ((save-excursion
           (beginning-of-line)
           (looking-at pic-basic-long-comment-regexp))
         (pic-basic-fill-long-comment))
        (t
         (pic-basic-indent-defun))))



(defun pic-basic-untabify ()
  "Do not allow any tabs into the file."
  (if (eq major-mode 'pic-basic-mode)
      (untabify (point-min) (point-max)))
  nil)

(defun pic-basic-default-tag ()
  "Return default TAG at point to search by grep."
  ;; VB1 to Lennart: is border effect on match-data an issue
  (if (and (not (bobp))
           (save-excursion
             (backward-sexp)
             (looking-at "\\w")))
      (backward-word 1))
  (let ((s (point))
        (e (save-excursion
             (forward-sexp)
             (point))))
    (buffer-substring s e)))

(defun pic-basic-grep (tag)
  "Search BASIC source files in current directory for TAG."
  (interactive
   (list (let* ((def (pic-basic-default-tag))
                (tag (read-string
                      (format "Grep for [%s]: " def))))
           (if (string= tag "") def tag))))
  (grep (format "grep -n %s %s" tag pic-basic-wild-files)))


;;; Indentation-related stuff.

(defun pic-basic-indent-region (start end)
  "Perform `pic-basic-indent-line' on each line in region delimited by START and END."
  (interactive "r")
  (save-excursion
    (goto-char start)
    (beginning-of-line)
    (while (and (not (eobp))
                (< (point) end))
      (if (not (looking-at pic-basic-blank-regexp))
          (pic-basic-indent-line))
      (forward-line 1)))

  (cond ((fboundp 'zmacs-deactivate-region)
         (zmacs-deactivate-region))
        ((fboundp 'deactivate-mark)
         (deactivate-mark))))



(defun pic-basic-previous-line-of-code ()
  "Set point on previous line of code, skipping any blank or comment lines."
  (if (not (bobp))
      (forward-line -1))        ; previous-line depends on goal column
  (while (and (not (bobp))
              (or (looking-at pic-basic-blank-regexp)
                  (looking-at pic-basic-long-comment-regexp)))
    (forward-line -1)))

(defun pic-basic-next-line-of-code ()
  "Set point on next line of code, skipping any blank or comment lines."
  (if (null (eobp))
      (forward-line 1))        ; next-line depends on goal column
  (while (and (null (eobp))
              (looking-at pic-basic-long-comment-regexp))
    (forward-line 1)))


(defun pic-basic-find-original-statement ()
  "If the current line is a continuation, move back to the original stmt."
  (let ((here (point)))
    (pic-basic-previous-line-of-code)
    (while (and (not (bobp))
                (looking-at pic-basic-continuation-regexp))
      (setq here (point))
      (pic-basic-previous-line-of-code))
    (goto-char here)))

(defun pic-basic-find-predicate-matching-stmt (open-p close-p)
  "Find opening statement statisfying OPEN-P predicate for which matching closing statement statisfies CLOSE-P predicate.

Point is set on line statifying OPEN-P predicate, with ignoring
any line satifying OPEN-P but for which a matching line
statifying CLOSE-P was visited before during this search."
  ;; Searching backwards
  (let ((level 0))
    (while (and (>= level 0) (not (bobp)))
      (pic-basic-previous-line-of-code)
      (pic-basic-find-original-statement)
      (cond ((funcall close-p)
             (setq level (+ level 1)))
            ((funcall open-p)
             (setq level (- level 1)))))))

(defun pic-basic-find-matching-stmt (open-regexp close-regexp)
  "Same as function `pic-basic-find-predicate-matching-stmt' except that regexps OPEN-REGEXP CLOSE-REGEXP are supplied instead of predicate, equivalent predicate being to be looking at those regexps."
  (pic-basic-find-predicate-matching-stmt
   (lambda () (looking-at open-regexp))
   (lambda () (looking-at close-regexp))))

(defun pic-basic-get-complete-tail-of-line ()
  "Return the tail of the current statement line, starting at
point and going up to end of statement line. If you want the
complete statement line, you have to call functions
`pic-basic-find-original-statement' and then
`beginning-of-line' before"
  (let* ((start-point (point))
	 complete-line
	 (line-beg start-point)
	 line-end)
    (while (null line-end)
      (end-of-line)
      (setq line-end (point))
      (if (search-backward "_" line-beg t)
	  (if (looking-at  pic-basic-looked-at-continuation-regexp)
	      ;; folded line
	      (progn
		(setq line-end (1- (point))
		      complete-line (cons
				     (buffer-substring-no-properties
				      line-beg line-end)
				     complete-line)
		      line-end nil)
		(beginning-of-line 2)
		(setq line-beg (point)))
	    ;; _ found, but not a folded line (this is a syntax error)
	    (setq complete-line
		  (cons (buffer-substring-no-properties line-beg line-end) complete-line)))
	;; not a folded line
	(setq complete-line
	      (cons (buffer-substring-no-properties line-beg line-end)
		    complete-line))))
    (mapconcat 'identity (nreverse complete-line) " ")))

(defun pic-basic-if-not-on-single-line ()
  "Return non-`nil' when the If statement is not on a single statement
line, i.e. requires a matching End if. Note that a statement line may
be folded over several code lines."
  (if (looking-at pic-basic-if-regexp)
      (save-excursion
	(beginning-of-line)
	(let (p1
	      p2
	      ;; 1st reconstruct complete line
	      (complete-line (pic-basic-get-complete-tail-of-line)) )

	  ;; now complete line has been reconstructed, drop confusing elements

	  ;; remove any VB string from complete line, as strings may disrupt : and ' detection
	  (while (and (setq p1 (string-match "\"" complete-line))
		      (setq p2 (string-match "\"" complete-line (1+ p1))))
	    (setq complete-line (concat (substring complete-line 0 p1)
					(substring complete-line (1+ p2)))))
	  ;; now drop tailing comment if any
	  (when (setq p1 (string-match "'" complete-line))
	    (setq complete-line (substring complete-line 0 (1- p1))))
	  ;; now drop 1st concatenated instruction if any
	  (when (setq p1 (string-match ":" complete-line))
	    (setq complete-line (substring complete-line p1)))
	  ;;
	  (string-match "Then\\s-*$" complete-line))); end (save-excursion ...)
    ;; else, not a basic if
    nil))

(defun pic-basic-find-matching-if ()
  "Set pointer on the line with If stating the If ... Then ... [Else/Elseif ...] ... End If block containing pointer."
  (pic-basic-find-predicate-matching-stmt 'pic-basic-if-not-on-single-line
                                             (lambda () (looking-at pic-basic-endif-regexp))))

(defun pic-basic-find-matching-select ()
  "Set pointer on the line with Select Case stating the Select Case ... End Select block containing pointer."
  (pic-basic-find-matching-stmt pic-basic-select-regexp
                                   pic-basic-select-end-regexp))

(defun pic-basic-find-matching-for ()
  "Set pointer on the line with For stating the `For ... Next' block containing pointer."
  (pic-basic-find-matching-stmt pic-basic-for-regexp
                                   pic-basic-next-regexp))

(defun pic-basic-find-matching-do ()
  "Set pointer on the line with Do stating the `Do ... Loop' block containing pointer."
  (pic-basic-find-matching-stmt pic-basic-do-regexp
                                   pic-basic-loop-regexp))

(defun pic-basic-find-matching-while ()
  "Set pointer on the line with While stating the `While ... Wend' block containing pointer."
  (pic-basic-find-matching-stmt pic-basic-while-regexp
                                   pic-basic-wend-regexp))

(defun pic-basic-find-matching-with ()
  "Set pointer on the line with With stating the `With ... End with' block containing pointer."
  (pic-basic-find-matching-stmt pic-basic-with-regexp
                                   pic-basic-end-with-regexp))

;;; If this fails it must return the indent of the line preceding the
;;; end not the first line because end without matching begin is a
;;; normal simple statement
(defun pic-basic-find-matching-begin ()
  "Set pointer on the line with Begin stating the `Begin ... End' block containing pointer."
  (let ((original-point (point)))
    (pic-basic-find-matching-stmt pic-basic-begin-regexp
                                     pic-basic-end-begin-regexp)
    (if (bobp) ;failed to find a matching begin so assume that it is
                                        ;an end statement instead and use the indent of the
                                        ;preceding line.
        (progn (goto-char original-point)
               (pic-basic-previous-line-of-code)))))


(defun pic-basic-calculate-indent ()
  "Return indent count for the line of code containing pointer."
  (let ((original-point (point)))
    (save-excursion
      (beginning-of-line)
      ;; Some cases depend only on where we are now.
      (cond ((or (looking-at pic-basic-defun-start-regexp)
                 (looking-at pic-basic-defun-end-regexp))
             0)

            ;; The outdenting stmts, which simply match their original.
            ((or (looking-at pic-basic-else-regexp)
                 (looking-at pic-basic-endif-regexp))
             (pic-basic-find-matching-if)
             (current-indentation))

            ;; All the other matching pairs act alike.
            ((looking-at pic-basic-next-regexp) ; for/next
             (pic-basic-find-matching-for)
             (current-indentation))

            ((looking-at pic-basic-loop-regexp) ; do/loop
             (pic-basic-find-matching-do)
             (current-indentation))

            ((looking-at pic-basic-wend-regexp) ; while/wend
             (pic-basic-find-matching-while)
             (current-indentation))

            ((looking-at pic-basic-end-with-regexp) ; with/end with
             (pic-basic-find-matching-with)
             (current-indentation))

            ((looking-at pic-basic-select-end-regexp) ; select case/end select
             (pic-basic-find-matching-select)
             (current-indentation))

            ;; A case of a select is somewhat special.
            ((looking-at pic-basic-case-regexp)
             (pic-basic-find-matching-select)
             (+ (current-indentation) pic-basic-mode-indent))

            ;; Added KJW: Make sure that this comes after the cases
            ;; for if..endif, end select because end-regexp will also
            ;; match "end select" etc.
            ((looking-at pic-basic-end-begin-regexp) ; begin/end
             (pic-basic-find-matching-begin)
             (current-indentation))

            (t
             ;; Other cases which depend on the previous line.
             (pic-basic-previous-line-of-code)

             (cond
              ((looking-at pic-basic-continuation-regexp)
               (pic-basic-find-original-statement)
               ;; Indent continuation line under matching open paren,
               ;; or else one word in.
               (let* ((orig-stmt (point))
                      (matching-open-paren
                       (condition-case ()
                           (save-excursion
                             (goto-char original-point)
                             (beginning-of-line)
                             (backward-up-list 1)
                             ;; Only if point is now w/in cont. block.
                             (if (<= orig-stmt (point))
                                 (current-column)))
                         (error nil))))
                 (cond (matching-open-paren
                        (1+ matching-open-paren))
                       (t
                        ;; Else, after first word on original line.
                        (back-to-indentation)
                        (forward-word 1)
                        (while (looking-at "[ \t]")
                          (forward-char 1))
                        (current-column)))))
              (t
               (pic-basic-find-original-statement)

               (let ((indent (current-indentation)))
                 ;; All the various +indent regexps.
                 (cond ((looking-at pic-basic-defun-start-regexp)
                        (+ indent pic-basic-mode-indent))

                       ((or (pic-basic-if-not-on-single-line)
                            (and (looking-at pic-basic-else-regexp)
                                 (not (and pic-basic-allow-single-line-if
                                           (looking-at pic-basic-ifthen-regexp)))))
                        (+ indent pic-basic-mode-indent))

                       ((or (looking-at pic-basic-select-regexp)
                            (looking-at pic-basic-case-regexp))
                        (+ indent pic-basic-mode-indent))

                       ((or (looking-at pic-basic-do-regexp)
                            (looking-at pic-basic-for-regexp)
                            (looking-at pic-basic-while-regexp)
                            (looking-at pic-basic-with-regexp)
                            (looking-at pic-basic-begin-regexp))
                        (+ indent pic-basic-mode-indent))

                       (t
                        ;; By default, just copy indent from prev line.
                        indent))))))))))

(defun pic-basic-indent-to-column (col)
  "Indent line of code containing pointer up to column COL."
  (let* ((bol (save-excursion
                (beginning-of-line)
                (point)))
         (point-in-whitespace
          (<= (point) (+ bol (current-indentation))))
         (blank-line-p
          (save-excursion
            (beginning-of-line)
            (looking-at pic-basic-blank-regexp))))

    (cond ((/= col (current-indentation))
           (save-excursion
             (beginning-of-line)
             (back-to-indentation)
             (delete-region bol (point))
             (indent-to col))))

    ;; If point was in the whitespace, move back-to-indentation.
    (cond (blank-line-p
           (end-of-line))
          (point-in-whitespace
           (back-to-indentation)))))


(defun pic-basic-indent-line ()
  "Indent current line for BASIC."
  (interactive)
  (pic-basic-indent-to-column (pic-basic-calculate-indent)))


(defun pic-basic-split-line ()
  "Split line at point, adding continuation character or continuing a comment."
  (interactive)
  (let ((pps-list (parse-partial-sexp (save-excursion
                                        (beginning-of-line)
                                        (point))
                                      (point))))
    ;; Dispatch on syntax at this position.
    (cond ((equal t (nth 4 pps-list))  ; in comment
           (indent-new-comment-line))
          ((equal t (nth 4 pps-list))   ; in string
           (error "Can't break line inside a string"))
          (t (just-one-space)           ; leading space on next line
                                        ; doesn't count, sigh
             (insert "_")
             (pic-basic-newline-and-indent)))))

(defun pic-basic-detect-idom ()
  "Detects whether this is a VBA or VBS script. Returns symbol
`vba' if it is VBA, `nil' otherwise."
  (let (ret
        (case-fold-search t))
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (point-min))
	(cond
	 ((looking-at "^\\s-*Attribute\\s-+VB_Name\\s-+= ")
	  (setq ret 'vba))
	 ((looking-at "^\\s-*Version\\s-+[^ \t\n\r]+Class\\s-*$")
	  (setq ret 'vba)))
        ))
    ret))

(defun pic-basic-close-block ()
  "Insert `End If' is current block is a `If Then ...', `End
With' if the block is a `With ...', etc..."
  (interactive)
  (let (end-statement end-indent)
    (save-excursion
      (save-match-data
	(while
	    (unless  (bobp)
	      (pic-basic-previous-line-of-code)
	      (pic-basic-find-original-statement)
	      (cond
	       ;; Cases where the current statement is a start-of-smthing statement
	       ((looking-at pic-basic-defun-start-regexp)
		(let ((smt (match-string 2)))
		  (when (string-match "\\`Prop" smt)
		    (setq smt "Property"))
		  (setq end-statement (concat "End " smt)
			end-indent 0))
		nil)
	       ((looking-at pic-basic-select-regexp)
		(setq  end-statement "End Select"
		       end-indent (current-indentation))
		nil)
	       ((looking-at pic-basic-with-regexp)
		(setq  end-statement "End With"
		       end-indent (current-indentation))
		nil)
	       ((looking-at pic-basic-case-regexp)
		(setq  end-statement  "End Select"
		       end-indent (max 0 (- (current-indentation) pic-basic-mode-indent)))
		nil)
	       ((looking-at pic-basic-begin-regexp)
		(setq  end-statement "End"
		       end-indent (current-indentation))
		nil)
	       ((or (pic-basic-if-not-on-single-line)
		    (looking-at pic-basic-else-regexp))
		(setq  end-statement "End If"
		       end-indent (current-indentation))
		nil)

	       ((looking-at pic-basic-do-regexp)
		(setq  end-statement "Loop"
		       end-indent (current-indentation))
		nil)

	       ((looking-at pic-basic-while-regexp)
		(setq  end-statement "Wend"
		       end-indent (current-indentation))
		nil)

	       ((looking-at pic-basic-for-regexp)
		(goto-char (match-end 0))
		(setq  end-statement "Next"
		       end-indent (current-indentation))
		(let ((vb-idom (pic-basic-detect-idom)))
		  (cond
		   ;; for VBA add the variable name after Next.
		   ((eq vb-idom 'vba)
		    (when (looking-at "\\s-+\\(Each\\s-+\\|\\)\\([^ \t\n\r]+\\)")
		      (setq end-statement (concat end-statement " " (match-string 2)))))))
		nil)
	       ;; Cases where the current statement is an end-of-smthing statement
	       ((or (looking-at pic-basic-else-regexp)
		    (looking-at pic-basic-endif-regexp))
		(pic-basic-find-matching-if)
		t)
	       ((looking-at pic-basic-next-regexp) ; for/next
		(pic-basic-find-matching-for)
		t)
	       ((looking-at pic-basic-loop-regexp) ; do/loop
		(pic-basic-find-matching-do)
		t)
	       ((looking-at pic-basic-wend-regexp) ; while/wend
		(pic-basic-find-matching-while)
		t)
	       ((looking-at pic-basic-end-with-regexp) ; with/end with
		(pic-basic-find-matching-with)
		t)
	       ((looking-at pic-basic-select-end-regexp) ; select case/end select
		(pic-basic-find-matching-select)
		t)


	       ;; default is to loop again back to previous line of code.
	       (t t))))))
    (when end-statement
      (insert end-statement)
      (pic-basic-indent-to-column end-indent))))

(defun pic-basic-insert-item ()
  "Insert a new item in a block.

This function is under developement, and for the time being only
Dim and Case items are handled.

Interting an item means:

* Add a `Case' or `Case Else' into a `Select ... End Select'
  block. **under construction** Pressing again toggles between
  `Case' and `Case Else'. `Case Else' is possible only if there
  is not already a `Case Else'.

* Split a Dim declaration over several lines. Split policy is
  that:

  - the split always occur just before or just after the
    declaration of the variable V such that the pointer is
    located over this declaration. For instance if the
    declaration is `V(2) as T' then pointer position maybe any
    `^' as follows:

       Dim X, V(2) As T, Y
              ^^^^^^^^^^^

  - the split being after or before `V(2) as T' decalration and
    the position of pointer after split depends on where the
    pointer was before the split:

    - if the pointer is over variable name (but with array size
      inclusive) like this:

       Dim X, V(2) As T, Y
              ^^^^

      then the split is as follows (split is before declaration
      and pointer goes to next line):

       Dim X
       Dim V(2) As T, Y
           ^

    - if the pointer is not over variable name like this:


       Dim X, V(2) As T, Y
                  ^^^^^^^

      then the split is as follows (split is after declaration
      and pointer remains on same line):

       Dim X, V(2) As T
                       ^
       Dim Y

* **under construction** Add an `Else' or `ElseIf ... Then' into
  an `If ... Then ... End If' block.  Pressing again toggles
  between `Else' and `ElseIf ... Then'.  `Else' is possible only
  if therei s not already an `Else'."
  (interactive)
  ;; possible cases are

  ;; dim-split-before => pointer remains before `Dim' inserted by split
  ;; dim-split-after => pointer goes after `Dim' inserted by split
  ;; if-with-else
  ;; if-without-else
  ;; select-with-else
  ;; select-without-else
  ;; not-itemizable
  (let (item-case
	item-ident
	split-point
	org-split-point
	prefix
	is-const
	tentative-split-point
	block-stack (cur-point (point)) previous-line-of-code)
    (save-excursion
      (save-match-data
	(beginning-of-line)
	(while
	    (progn
	      (pic-basic-find-original-statement)
	      (cond
	       ;; dim case
	       ;;--------------------------------------------------------------
	       ((and (null previous-line-of-code)
		     (looking-at pic-basic-dim-regexp)
		     (null (save-match-data (looking-at pic-basic-defun-start-regexp))))
		(setq prefix (buffer-substring-no-properties
			      (point)
			      (goto-char (setq split-point (match-end 0)
					       org-split-point split-point)))
		      is-const (string-match "\\_<Const\\_>" prefix)
		      item-case ':dim-split-after)
		;; determine split-point, which is the point at which a new
		;; Dim item is to be inserted. To that purpose the line is gone through
		;; from beginning until cur-point is past
		(while
                    (if
			(looking-at "\\(\\s-*\\)\\(?:\\sw\\|\\s_\\)+\\s-*"); some symbol
			(if (>  (setq tentative-split-point (match-end 0)) cur-point)
                            (progn
			      (setq item-case (if (>= cur-point (match-end 1))
						  ':dim-split-after
                                                ':dim-split-before))
			      nil;; stop loop
			      )
			  (goto-char tentative-split-point)
			  (setq item-case ':dim-split-before)
			  (let ((loop-again t))
			    (while
				(or
				 ;; array variable
				 (when (looking-at "\\(([^)\n]+)\\)\\s-*")
                                   (if (< cur-point (match-end 1))
                                       (setq item-case ':dim-split-after
                                             loop-again nil)
                                     t))
				 ;; continuation
				 (and loop-again
				      (looking-at pic-basic-looked-at-continuation-regexp) ))
                              (goto-char (setq tentative-split-point (match-end 0))))
			    (when loop-again
			      (when (looking-at "As\\s-+\\(?:\\sw\\|\\s_\\)+\\s-*")
				(setq item-case ':dim-split-after)
				(goto-char (setq tentative-split-point (match-end 0))))
			      (when (looking-at pic-basic-looked-at-continuation-regexp)
				(beginning-of-line 2))
			      (if (looking-at ",")
				  (goto-char (setq split-point (match-end 0)))
				(setq split-point (point))
				nil))))
		      nil))
		;; now make the split. This means that some comma may need to be deleted.
		(goto-char split-point)
		(looking-at "\\s-*")
		(delete-region split-point (match-end 0))
		(cond
		 ((looking-back ",")
		  (while
		      (progn
			(delete-region split-point (setq split-point (1- split-point)))
			(looking-back "\\s-"))))
		 ((= split-point org-split-point)
		  (insert " ")
		  (setq split-point (point))))
		(insert "\n" prefix " ")
		(setq cur-point (point))
		nil)

	       ;;  case of Case (in Select ... End Select)
	       ;;----------------------------------------------------------------------
	       ((looking-at pic-basic-case-regexp)
		(if (looking-at pic-basic-case-else-regexp)
		    ;; if within a Case Else statement, then insert
		    ;; a Case just before with same indentation
		    (let ((indent (current-indentation)))
		      (beginning-of-line)
		      (insert "Case ")
		      (pic-basic-indent-to-column indent)
		      (setq item-case ':select-with-else
			    split-point (point))
		      (insert ?\n))
		  (setq item-case ':select-without-else))
		nil; break loop
		)

	       ;; next
	       ((looking-at pic-basic-next-regexp)
		(push (list 'next) block-stack))
	       ;; default
	       ;;--------------------------------------------------------------
	       (t (if (bobp)
		      (setq item-case 'not-itemizable)))
	       )
	      (when (null item-case)
		(pic-basic-previous-line-of-code)
		(setq previous-line-of-code t))
	      (null item-case)))))
    (case item-case
      ((:dim-split-after)   (message "split after") (goto-char cur-point))
      ((:dim-split-before)  (message "split before") (goto-char split-point))
      ((:select-with-else)  (goto-char split-point))
      ((:select-without-else)
       ;; go forward until the End Select or next case is met in order to
       ;; to insert the new case at this position
       (let ((select-case-depth 0))
	 (while
	     (progn
	       (pic-basic-next-line-of-code)
               (cond
		;; case was found, insert case and exit loop
		((and (= 0 select-case-depth)
		      (looking-at pic-basic-case-regexp))
		 (let ((indent (current-indentation)))
		   (beginning-of-line)
		   (insert "Case ")
		   (pic-basic-indent-to-column indent)
		   (save-excursion (insert ?\n))
		   nil))
		((looking-at pic-basic-select-regexp)
		 (setq select-case-depth (1+ select-case-depth))
		 (if
		     (re-search-forward (concat pic-basic-select-regexp
						"\\|"
						pic-basic-select-end-regexp)
					nil nil)
		     (progn
		       (beginning-of-line)
		       t ; loop again
		       )
		   (let ((l (line-number-at-pos)))
		     (goto-char cur-point)
		     (error "Select Case without matching end at line %d" l))))
		((looking-at pic-basic-select-end-regexp)
		 (setq select-case-depth (1- select-case-depth))
		 (if (= select-case-depth -1)
		     (let ((indent (current-indentation)))
		       (insert  "Case ")
		       (save-excursion (insert ?\n ))
		       (pic-basic-indent-to-column
		        (+ indent pic-basic-mode-indent))
		       nil;; break loop
                       )
		   t; loop again
                   ))
		((eobp)
		 (goto-char cur-point)
		 (error "Case without ending"))
		;; otherwise loop again
		(t t)))))) ; end of select-case-without-else
      )))


;;; Some experimental functions

;;; Load associated files listed in the file local variables block
(defun pic-basic-load-associated-files ()
  "Load files that are useful to have around when editing the
source of the file that has just been loaded.  The file must have
a local variable that lists the files to be loaded.  If the file
name is relative it is relative to the directory containing the
current buffer.  If the file is already loaded nothing happens,
this prevents circular references causing trouble.  After an
associated file is loaded its associated files list will be
processed."
  (if (boundp 'pic-basic-associated-files)
      (let ((files pic-basic-associated-files)
            (file nil))
        (while files
          (setq file (car files)
                files (cdr files))
          (message "Load associated file: %s" file)
          (pic-basic-load-file-ifnotloaded file default-directory)))))



(defun pic-basic-load-file-ifnotloaded (file default-directory)
  "Load file if not already loaded.
If FILE is relative then DEFAULT-DIRECTORY provides the path."
  (let((file-absolute (expand-file-name file default-directory)))
    (if (get-file-buffer file-absolute); don't do anything if the buffer is already loaded
        ()
      (find-file-noselect file-absolute ))))

(defun pic-basic-check-style ()
  "Check coding style of currently open buffer, and make
corrections under the control of user.

This function is under construction"
  (interactive)
  (flet
      ((insert-space-at-point
	()
	(insert " "))
       ;; avoid to insert space inside a floating point number
       (check-plus-or-minus-not-preceded-by-space-p
	()
	(save-match-data
	  (and
	   (pic-basic-in-code-context-p)
	   (null (looking-back "\\([0-9]\\.\\|[0-9]\\)[eE]")))))
       (check-plus-or-minus-not-followed-by-space-p
	()
	(save-match-data
	  (and
	   (pic-basic-in-code-context-p)
	   (null  (looking-at "\\(\\sw\\|\\s_\\|\\s\(\\|[.0-9]\\)"))
	   (null (looking-back "\\([0-9]\\.\\|[0-9]\\)[eE]\\|,\\s-*\\(\\|_\\s-*\\)\\|:=\\s-*")))));
       (check-comparison-sign-not-followed-by-space-p
	()
	(save-match-data
	  (and
	   (pic-basic-in-code-context-p)
	   (let ((next-char (match-string 2))
		 (str--1 (or (= (match-beginning 1) (point-min))
			     (buffer-substring-no-properties (1- (match-beginning 1))
							     (1+ (match-beginning 1))))))
	     (null (or
		    (and (stringp str--1)
			 (string= str--1 ":="))
		    (string-match "[<=>]" next-char ))) ))));
       (replace-by-&
	()
	(goto-char (1- (point)))
	(let* ((p1 (point))
	       (p2 (1+ p1)))
	  (while (looking-back "\\s-")
	    (goto-char (setq p1 (1- p2))))
	  (goto-char p2)
	  (when (looking-at "\\s-+")
	    (setq p2 (match-end 0)))
	  (delete-region p1 p2)
	  (insert " & ")));
       (check-string-concatenation-by-+
	()
	(save-match-data
	  (and
	   (pic-basic-in-code-context-p)
	   (or
	    (looking-at "\\s-*\\(\\|_\n\\s-*\\)\"")
	    (looking-back "\"\\(\\|\\s-*_\\s-*\n\\)\\s-*\\+")))));
       )
    (let (vb-other-buffers-list
	  ;; list of found error styles
	  ;; each element is a list (POSITION PROMPT ERROR-SOLVE-HANDLER)
	  next-se-list
	  next-se
	  case-fold-search
	  (hl-style-error (make-overlay 1 1)); to be moved
	  (style-errors
	   '(
	     ;; each element is a vector
	     ;;   0	 1	2	3	  4		      5		    6
	     ;; [ REGEXP PROMPT GET-POS RE-EXP-NB ERROR-SOLVE-HANDLER ERROR-CONFIRM LEVEL]
	     [ "\\(\\s\)\\|\\sw\\|\\s_\\)[-+]"
	       "Plus or minus not preceded by space"
	       match-end 1
	       insert-space-at-point
	       check-plus-or-minus-not-preceded-by-space-p
	       0 ]
	     [ "\\(\\s\)\\|\\sw\\|\\s_\\)[/\\*&]"
	       "Operator not preceded by space"
	       match-end 1
	       insert-space-at-point
	       pic-basic-in-code-context-p
	       0 ]
	     [ "[/\\*&]\\(\\s\(\\|\\sw\\|\\s_\\|\\s.\\)"
	       "Operator not followed by space"
	       match-beginning 1
	       insert-space-at-point
	       pic-basic-in-code-context-p
	       0 ]
	     [ "[-+]\\(\\s\(\\|\\sw\\|\\s_\\|\\s.\\)"
	       "Plus or minus not followed by space"
	       match-beginning 1
	       insert-space-at-point
	       check-plus-or-minus-not-followed-by-space-p
	       0 ]
	     [ "\\(\\s\)\\|\\sw\\|\\s_\\)\\(=\\|<\\|>\\)"
	       "Comparison sign not preceded by space"
	       match-end 1
	       insert-space-at-point
	       pic-basic-in-code-context-p
	       0 ]
	     [ "\\(=\\|<\\|>\\)\\(\\s\(\\|\\sw\\|\\s_\\|\\s.\\)"
	       "Comparison sign not followed by space"
	       match-end 1
	       insert-space-at-point
	       check-comparison-sign-not-followed-by-space-p
	       0 ]
	     [ ",\\(\\sw\\|\\s_\\)"
	       "Comma not followed by space"
	       match-beginning 1
	       insert-space-at-point
	       pic-basic-in-code-context-p
	       0 ]
	     [ "\\+"
	       "String should be concatenated with & rather than with +"
	       match-end 0
	       replace-by-&
	       check-string-concatenation-by-+
	       0 ]
	     )); end of style error types
	  )
      (condition-case nil 
	  (progn
	    (overlay-put hl-style-error 'face hl-line-face)
	    (overlay-put hl-style-error 'window (selected-window))
	    (dolist (x (buffer-list))
	      (if (and (save-excursion
			 (set-buffer x)
			 (derived-mode-p 'pic-basic-mode))
		       (null (eq x (current-buffer))))
		  (push x vb-other-buffers-list)))
	    (save-excursion
	      (save-restriction
		(widen)
		(goto-char (point-min))
		(while
		    (progn
		      (setq next-se-list nil)
		      (dolist (se style-errors)
			(save-excursion
			  (when
			      (and
			       (re-search-forward (aref se 0) nil t)
			       (progn
				 (goto-char  (funcall (aref se 2)
						      (aref se 3)))
				 (or (null (aref se 5))
				     (funcall  (aref se 5))
				     (let (found)
				       (while (and
					       (setq found (re-search-forward (aref se 0) nil t))
					       (null (progn
						       (goto-char  (funcall (aref se 2)
									    (aref se 3)))
						       (funcall  (aref se 5))))))
				       found))))
			    (push (list (point)
					(match-beginning 0) 
					(match-end 0)
					(aref se 1)
					(and (> (aref se 6) pic-basic-auto-check-style-level)
					     (aref se 4)))
				  next-se-list))))
		      (when next-se-list
			(setq next-se-list
			      (sort next-se-list (lambda (x y) (< (car x) (car y))))
			      next-se (pop next-se-list))
			(goto-char (pop next-se))
			(move-overlay hl-style-error (pop next-se) (pop next-se))
			(when (y-or-n-p (concat (pop next-se)
						", solve it ? "))
			  (funcall (pop next-se)))
			t; loop again
			))))) )
	;; error handlers
	(delete-overlay hl-style-error))
      (delete-overlay hl-style-error)))
  (message "Done Pic Basic style check"))

(provide 'pic-basic-mode)

(provide 'pic-basic-mode)

;;; pic-basic-mode.el ends here
