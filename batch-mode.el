;;; batch-mode.el --- major mode for editing ESRI batch scrips
;;; Copyright (C) 2002, Agnar Renolen <agnar.renolen@emap.no>
;;; Modified (c) 2009, Matthew Fidler <matthew.fidler at gmail.com>
;;; Fixed indents (and labels)

;; batch-mode.el is free software; you can redistribute it and/or modify
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

;; This is version 1.0 of 21 August 2002.

;;; Comentary:

;; The batch-mode provides syntax hilighting and auto-indentation for
;; DOS batch files (.bat).  and auto-idendation.  

;; Agnar Renolen, <agnar.renolen@emap.no>

;;; Code:

(defgroup batch nil
  "Major mode for editing batch code"
  :prefix "batch-"
  :group 'languages)

; (defvar batch-mode-hook nil
;   "Hooks called when batch mode fires up."
;   :type 'hook
;   :group 'batch)

(defvar batch-mode-map nil
  "Keymap used with batch code")

(defcustom batch-indent-level 4
  "Amount by which batch subexpressions are indented."
  :type 'integer
  :group 'batch)

(defvar batch-font-lock-keywords
  (eval-when-compile
    (list
     ; since we can't specify bacth comments through the syntax table,
     ; we have to specify it here, and override whatever is hilihgted
     '( "^[ \t]*rem\\>.*" (0 font-lock-comment-face t))

     ; sice the argument to the echo command is a string, we format it
     ; as a string
     '( "\\<echo\\>[ \t]*\\(.*\\)" (1 font-lock-string-face t))

     ; the argument of the goto statement is a label
     '( "\\<goto\\>[ \t]*\\([a-zA-Z0-9_]+\\)" (1
					      font-lock-constant-face))
     
     ; the keywords of batch (which are not built-in commands)
     (concat "\\<\\(cmdextversion\\|"
	     "d\\(efined\\|isableextensions\\|o\\)\\|"
	     "e\\(lse\\|n\\(ableextensions\\|dlocal\\)"
	     "\\|qu\\|rrorlevel\\|xist\\)\\|for\\|"
	     "goto\\|i[fn]\\|n\\(eq\\|ot\\)\\|setlocal\\)\\>")

     ; built-in DOS commands
     (cons (concat "\\<\\(a\\(ssoc\\|t\\(\\|trib\\)\\)\\|break\\|"
		   "c\\(a\\(cls\\|ll\\)\\|d\\|h\\(cp\\|dir\\|k\\("
		   "dsk\\|ntfs\\)\\)\\|ls\\|md\\|o\\(lor\\|mp\\(\\|act\\)"
		   "\\|nvert\\|py\\)\\)\\|d\\(ate\\|el\\|i\\("
		   "r\\|skco\\(mp\\|py\\)\\)\\|oskey\\)\\|"
		   "e\\(cho\\|rase\\|xit\\)\\|"
		   "f\\(c\\|ind\\(\\|str\\)\\|for\\(\\|mot\\)\\|type\\)\\|"
		   "graftabl\\|help\\|label\\|"
		   "m\\(d\\|mkdir\\|o[dvr]e\\)\\|p\\(a\\(th\\|use\\)"
		   "\\|opd\\|r\\(int\\|opmt\\)\\|ushd\\)\\|"
		   "r\\(d\\|e\\(cover\\|n\\(\\|ame\\)\\|place\\)\\|mdir\\)\\|"
		   "s\\(et\\|hift\\|ort\\|tart\\|ubst\\)\\|"
		   "t\\(i\\(me\\|tle\\)\\|ree\\|ype\\)\\|"
		   "v\\(er\\(\\|ify\\)\\|ol\\)\\|xcopy\\)\\>")
	   'font-lock-builtin-face)
     
     ; variables are embeded in percent chars
     '( "%[a-zA-Z0-9_]+%?" . font-lock-variable-name-face)
     ; labels are formatted as constants
     '( ":[a-zA-Z0-9_]+" . font-lock-constant-face)

     ; command line switches are hilighted as type-face
     '( "[-/][a-zA-Z0-9_]+" . font-lock-type-face)

     ; variables set should also be hilighted with variable-name-face
     '( "\\<set\\>[ \t]*\\([a-zA-Z0-9_]+\\)" (1 font-lock-variable-name-face))
    )))
     

;;;###autoload
(defun batch-mode ()
  "Major mode for editing batch scripts."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'batch-mode)
  (setq mode-name "Avenue")
  (set (make-local-variable 'indent-line-function) 'batch-indent-line)
  (set (make-local-variable 'comment-start) "rem")
  (set (make-local-variable 'comment-start-skip) "rem[ \t]*")
  (set (make-local-variable 'font-lock-defaults)
       '(batch-font-lock-keywords nil t nil))
  (run-hooks 'batch-mode-hook))
  
(defun batch-indent-line ()
  "Indent current line as batch script"
  (let ((indent (batch-calculate-indent))
	beg shift-amt 
	(old-pos (- (point-max) (point))))
    (beginning-of-line)
    (setq beg (point))
    (skip-chars-forward " \t")
    (if (looking-at ")")
	(setq indent (max (- indent batch-indent-level))))
    (message "prev indent: %d" indent)
    (setq shift-amt (- indent (current-column)))
    (if (not (zerop shift-amt))
	(progn
	  (delete-region beg (point))
	  ; ArcView replaces tabs with single spaces, so we only insert
	  ; spaces to make indentation correct in ArcView.
	  (insert-char ?  indent)
	  (if (> (- (point-max) old-pos) (point))
	      (goto-char (- (point-max) old-pos)))))
    shift-amt))
    
(defun batch-calculate-indent ()
  "Return appropriate indentation for the current line as batch code."
  (save-excursion
    (beginning-of-line)
    (current-indentation)
    (if (bobp)
	0
      (if (re-search-backward "^[ \t]*[^ \t\n\r]" nil t)
	  (if (looking-at "[ \t]*\\()[ \t]*else\\|for\\|if\\)\\>[^(\n]*([^)\n]*")
	      (+ (current-indentation) batch-indent-level)
	    (if (looking-at "[ \t]*[^(]*)[ \t]*")
		(- (current-indentation) batch-indent-level)
	      (current-indentation)))
	0))))

(add-to-list 'auto-mode-alist '("\\.bat\\'" . batch-mode))
  
(provide 'batch-mode)

;;; batch-mode.el ends here
