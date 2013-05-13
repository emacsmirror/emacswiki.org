;;; spss.el --- Major-mode for editing SPSS program files

;; Filename: spss.el
;; Description: Major-mode for editing SPSS program files
;; Author: Joe Bloggs <vapniks@yahoo.com>
;; Maintainer: Joe Bloggs <vapniks@yahoo.com>
;; Copyleft (Ↄ) 2013, Joe Bloggs, all rites reversed.
;; Created: 2008/08/01
;; Version: 0.1
;; Last-Updated: 2013-05-13 17:16:16
;;           By: Joe Bloggs
;; URL: https://github.com/vapniks/spss
;; Keywords: languages
;; Compatibility: GNU Emacs 24.3.1
;; Package-Requires:  
;;
;; Features that might be required by this library:
;;
;; 
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary: 
;;
;; Bitcoin donations gratefully accepted: 1MSYn98e4FpXYgihMpqFN4AMSZKsCGKHKX
;;
;; This library should provide font highlighting for SPSS program files.
;; It doesn't seem to work very well, but since I don't use SPSS anymore
;; I leave it to someone else to fix up.
;;
;;;;


;;; Installation:
;;
;; Put spss.el in a directory in your load-path, e.g. ~/.emacs.d/
;; You can add a directory to your load-path with the following line in ~/.emacs
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;; where ~/elisp is the directory you want to add 
;; (you don't need to do this for ~/.emacs.d - it's added by default).
;;
;; Add the following to your ~/.emacs startup file.
;;
;; (require 'spss)

;;; Customize:
;;
;; To automatically insert descriptions of customizable variables defined in this buffer
;; place point at the beginning of the next line and do: M-x auto-document

;;
;; All of the above can customized by:
;;      M-x customize-group RET spss RET
;;

;;; Change log:
;;	
;; 2013/05/13
;;      * First released.
;; 

;;; Acknowledgements:
;;
;; 
;;

;;; TODO
;;
;; This needs some work, but I don't really use SPSS anymore so I'm leaving it to someone else
;; to take over.
;;

;;; Require


;;; Code:

(defvar spss-mode-hook nil)
(defvar spss-mode-map
  (let ((spss-mode-map (make-keymap)))
    (define-key spss-mode-map "\C-j" 'newline-and-indent)
    spss-mode-map)
  "Keymap for spss major mode.")
(add-to-list 'auto-mode-alist '("\\.sps\\'" . spss-mode))
(defconst spss-font-lock-keywords-1
  (list
   ;; subcommand begin with /
   '("/[a-z]+\\>" . font-lock-type-face)
   ;; operators
   '("\\<\\(a\\(?:ll\\|nd\\)\\|by\\|eq\\|g[et]\\|l[et]\\|n\\(?:e\\|ot\\)\\|or\\|to\\|with\\)\\>" . font-lock-keyword-face)
   ;; macros
   '("\\<\\(!\\(?:BLANKS\\|CONCAT\\|DO\\(?:END\\)?\\|E\\(?:NDDEFINE\\|VAL\\)\\|HEAD\\|I\\(?:F\\(?:END\\)?\\|NDEX\\)\\|LE\\(?:NGTH\\|T\\)\\|NULL\\|O\\(?:\\(?:FF\\|N\\)EXPAND\\)\\|QUOTE\\|SUBSTR\\|T\\(?:AIL\\|HEN\\|O\\)\\|U\\(?:\\(?:NQUOT\\|PCAS\\)E\\)\\)\\|DEFINE\\)\\>" . font-lock-preprocessor-face)
   ;; builtin functions
   '("\\<\\(2SLS\\|A\\(?:C\\(?:F\\|TIVATE\\)\\|DD \\(?:DOCUMENT\\|\\(?:FILE\\|VALUE LABEL\\)S\\)\\|GGREGATE\\|IM\\|L\\(?:SCAL\\|TER\\)\\|N\\(?:ACOR\\|OVA\\|Y\\)\\|PPLY DICTIONARY\\|UTORECODE\\)\\|B\\(?:EGIN \\(?:DATA\\|GPL\\)\\|REAK\\)\\|C\\(?:A\\(?:CHE\\|SE\\(?:PLOT\\|STOVARS\\)\\|T\\(?:PCA\\|REG\\)\\)\\|CF\\|D\\|L\\(?:EAR T\\(?:IME PROGRAM\\|RANSFORMATIONS\\)\\|OSE\\|USTER\\)\\|O\\(?:M\\(?:MENT\\|PUTE\\)\\|NJOINT\\|RRE\\(?:LATIONS\\|SPONDENCE\\)\\|UNT\\|XREG\\)\\|R\\(?:EATE\\|OSSTABS\\)\\|S\\(?:COXREG\\|DESCRIPTIVES\\|GLM\\|LOGISTIC\\|ORDINAL\\|PLAN\\|SELECT\\|TABULATE\\)\\|TABLES\\|URVEFIT\\)\\|D\\(?:AT\\(?:A\\(?: LIST\\|FILE ATTRIBUTES?\\|SET \\(?:ACTIVATE\\|C\\(?:LOSE\\|OPY\\)\\|D\\(?:ECLARE\\|ISPLAY\\)\\|NAME\\)\\)\\|E\\)\\|E\\(?:LETE VARIABLES\\|SCRIPTIVES\\|TECTANOMALY\\)\\|IS\\(?:CRIMINANT\\|PLAY\\)\\|O\\(?: \\(?:IF\\|REPEAT\\)\\|CUMENT\\)\\|ROP DOCUMENTS\\)\\|E\\(?:CHO\\|JECT\\|LSE IF\\|ND \\(?:CASE\\|DATA\\|FILE\\(?: TYPE\\)?\\|GPL\\|INPUT PROGRAM\\|LOOP\\|MATRIX\\|REPEAT\\)\\|RASE\\|X\\(?:AMINE\\|ECUTE\\|PORT\\|TENSION\\)\\)\\|F\\(?:ACTOR\\|I\\(?:L\\(?:E \\(?:HANDLE\\|LABEL\\|TYPE\\)\\|TER\\)\\|NISH\\|T\\)\\|LIP\\|\\(?:ORMAT\\|REQUENCIE\\)S\\)\\|G\\(?:E\\(?:NL\\(?:IN\\|OG\\)\\|T\\(?: \\(?:CAPTURE\\|DATA\\|S\\(?:AS\\|TATA\\)\\|TRANSLATE\\)\\)?\\)\\|GRAPH\\|LM\\|RAPH\\)\\|H\\(?:ILOGLINEAR\\|O\\(?:MALS\\|ST\\)\\)\\|I\\(?:F\\|GRAPH\\|MPORT\\|N\\(?:CLUDE\\|FO\\|PUT PROGRAM\\|SERT\\)\\)\\|K\\(?:EYED DATA LIST\\|M\\)\\|L\\(?:EAVE\\|IST\\|O\\(?:G\\(?:ISTIC REGRESSION\\|LINEAR\\)\\|OP\\)\\)\\|M\\(?:A\\(?:NOVA\\|T\\(?:CH FILES\\|RIX\\(?: DATA\\)?\\)\\)\\|CONVERT\\|EANS\\|I\\(?:SSING VALUES\\|XED\\)\\|LP\\|ODEL\\(?: \\(?:CLOSE\\|HANDLE\\|LIST\\)\\)?\\|RSETS\\|ULT\\(?:\\(?: RESPONS\\|IPLE CORRESPONDENC\\)E\\)\\|VA\\)\\|N\\(?: OF CASES\\|A\\(?:IVEBAYES\\|ME\\)\\|EW FILE\\|LR\\|O\\(?:MREG\\|NPAR CORR\\)\\|PAR TESTS\\|UMERIC\\)?\\|O\\(?:LAP CUBES\\|MS\\(?:END\\|INFO\\|LOG\\)?\\|NEWAY\\|P\\(?:EN\\|TIMAL BINNING\\)\\|RTHOPLAN\\|UTPUT\\(?: \\(?:ACTIVATE\\|CLOSE\\|DISPLAY\\|N\\(?:AME\\|EW\\)\\|OPEN\\|SAVE\\)\\)?\\|VERALS\\)\\|P\\(?:A\\(?:CF\\|RTIAL CORR\\)\\|ER\\(?: \\(?:ATTRIBUTES\\|CO\\(?:NNECT\\|PY\\)\\)\\|MISSIONS\\)\\|L\\(?:ANCARDS\\|S\\|UM\\)\\|OINT\\|PLOT\\|R\\(?:E\\(?:DICT\\|FSCAL\\|SERVE\\)\\|IN\\(?:CALS\\|T\\(?: \\(?:EJECT\\|FORMATS\\|SPACE\\)\\)?\\)\\|O\\(?:BIT\\|CEDURE OUTPUT\\|X\\(?:IMITIES\\|SCAL\\)\\)\\)\\)\\|QUICK CLUSTER\\|R\\(?:A\\(?:N\\(?:GE\\|K\\)\\|TIO STATISTICS\\)\\|BF\\|E\\(?:AD MODEL\\|CO\\(?:\\(?:D\\|RD TYP\\)E\\)\\|FORMAT\\|GRESSION\\|LIABILITY\\|NAME \\(?:VARIABLES\\)?\\|P\\(?:EATING DATA\\|ORT\\)\\|READ\\|STORE\\)\\|MV\\|OC\\)\\|S\\(?:A\\(?:MPLE\\|VE\\(?: \\(?:DIMENSIONS\\|MODEL\\|TRANSPLANT\\)\\)?\\)\\|CRIPT\\|E\\(?:ASON\\|LECT\\(?: IF\\|PRED\\)\\|T\\)\\|HOW\\|ORT \\(?:\\(?:CAS\\|VARIABL\\)ES\\)\\|P\\(?:ACE\\|CHART\\|ECTRA\\|LIT FILE\\)\\|TRING\\|U\\(?:BTITLE\\|MMARIZE\\|RVIVAL\\)\\|YSFILE INFO\\)\\|T\\(?:-TEST\\|DISPLAY\\|EMPORARY\\|I\\(?:ME PROGRAM\\|TLE\\)\\|M\\(?:E END\\|S \\(?:BEGIN\\|MERGE\\)\\)\\|REE\\|S\\(?:APPLY\\|ET\\|HOW\\|MODEL\\|PLOT\\)\\|WOSTEP CLUSTER\\|YPE\\)\\|U\\(?:NIANOVA\\|\\(?:PDAT\\|S\\)E\\)\\|V\\(?:A\\(?:L\\(?:IDATE DATA\\|UE LABELS\\)\\|R\\(?:COMP\\|IABLE\\(?: \\(?:A\\(?:LIGNMENT\\|TTRIBUTE\\)\\|L\\(?:ABELS\\|EVEL\\)\\|WIDTH\\)\\|S\\)\\|STOCASES\\)\\)\\|E\\(?:CTOR\\|RIFY\\)\\)\\|W\\(?:EIGHT\\|LS\\|RITE\\(?: \\(?:FORMATS\\)?\\)?\\)\\|X\\(?:GRAPH\\|SAVE\\)\\)\\>" . font-lock-builtin-face)
   ;; statistical distribution functions
   '("\\<\\(\\(\\(?:CDF\\|IDF\\|N\\(?:[CP]DF\\)\\|PDF\\|RV\\|SIG\\)\\.\\(B\\(?:E\\(?:RNOULLI\\|TA\\)\\|INOM\\|VNOR\\)\\|C\\(?:AUCHY\\|HISQ\\)\\|EXP\\|G\\(?:AMMA\\|EOM\\)\\|H\\(?:ALFNRM\\|YPER\\)\\|IGAUSS\\|L\\(?:APLACE\\|NORMAL\\|OGISTIC\\)\\|N\\(?:EGBIN\\|ORMAL\\)\\|P\\(?:ARETO\\|OISSON\\)\\|S\\(?:MOD\\|RANGE\\)\\|UNIFORM\\|WEIBULL\\|[FT]\\)\\)\\|NORMAL\\|UNIFORM\\)\\>" . font-lock-builtin-face)
   ;; arithmetic functions
   '("\\<\\(A\\(?:BS\\|R\\(?:\\(?:SI\\|TA\\)N\\)\\)\\|C\\(?:FVAR\\|OS\\)\\|EXP\\|L\\(?:G10\\|N\\(?:GAMMA\\)?\\)\\|M\\(?:AX\\|EAN\\|IN\\|OD\\)\\|RND\\|S\\(?:D\\|IN\\|QRT\\|UM\\)\\|TRUNC\\|VARIANCE\\)\\>" . font-lock-builtin-face)
   ;; date and string functions
   '("\\<\\(C\\(?:HAR\\.\\(?:L\\(?:ENGTH\\|PAD\\)\\|MBLEN\\|R\\(?:INDEX\\|PAD\\)\\|SUBSTR\\)\\|ONCAT\\|TIME\\.\\(?:\\(?:DAY\\|HOUR\\|MINUTE\\|SECOND\\)S\\)\\)\\|DATE\\(?:\\.\\(?:DMY\\|M\\(?:DY\\|OYR\\)\\|QYR\\|WKYR\\|YRDAY\\)\\|DIFF\\|SUM\\)\\|INDEX\\|L\\(?:ENGTH\\|OWER\\|PAD\\|TRIM\\)\\|M\\(?:AX\\|BLEN\\.BYTE\\|I\\(?:N\\|SSING\\)\\)\\|N\\(?:MISS\\|ORMALIZE\\|TRIM\\|UMBER\\|VALID\\)\\|R\\(?:E\\(?:GION\\|PLACE\\)\\|INDEX\\|PAD\\|TRIM\\)\\|S\\(?:TR\\(?:ING\\|UNC\\)\\|UBSTR\\|YSMIS\\)\\|TIME\\.\\(?:\\(?:DAY\\|HM\\)S\\)\\|UPCASE\\|VALUE\\(?:LABEL\\)?\\|XDATE\\.\\(?:DATE\\|HOUR\\|JDAY\\|M\\(?:DAY\\|INUTE\\|ONTH\\)\\|QUARTER\\|SECOND\\|T\\(?:DAY\\|IME\\)\\|W\\(?:EEK\\|KDAY\\)\\|YEAR\\)\\|YRMODA\\)\\>" . font-lock-builtin-face)
   ;; transformation functions
   '("\\<\\(APPLYMODEL\\|C\\(?:HAR\\.\\(?:INDEX\\|L\\(?:ENGTH\\|PAD\\)\\|MBLEN\\|R\\(?:INDEX\\|PAD\\)\\|SUBSTR\\)\\|UMHAZARD\\)\\|DATE\\(?:DIFF\\|SUM\\)\\|N\\(?:ORMALIZE\\|TRIM\\)\\|REPLACE\\|STR\\(?:APPLYMODEL\\|UNC\\)\\|VALUELABEL\\)\\>" . font-lock-builtin-face)
   ;; system variables
   '("\\<\\(\\$\\(?:CASENUM\\|DATE\\(?:11\\)?\\|JDATE\\|LENGTH\\|SYSMIS\\|TIME\\|WIDTH\\)\\)\\>" . font-lock-constant-face)
   ;; temporary variables start with # in SPSS syntax
   '("#[a-z]+\\>" . font-lock-variable-name-face)
   "Highlighting expressions for spss mode."))

(defvar spss-font-lock-keywords spss-font-lock-keywords-1
  "Default highlighting expressions for spss mode.")

(defun spss-indent-line ()
  "Indent current line as spss code."
  (interactive)
  (beginning-of-line)
  (if (bobp) 
      ;; if at beginning of buffer, indent to start of line
      (indent-line-to 0)
    (let ((not-indented t) cur-indent)
      (if (looking-at "^[ \t]*\\(else\\|end if\\|end repeat\\)")
	  ;; if we are currently at the end of a block reduce indentation
	  (progn
	    (save-excursion
	      (forward-line -1)
	      (setq cur-indent (- (current-indentation) default-tab-width)))
	    (if (< cur-indent 0)
		(setq cur-indent 0)))
	(progn
	  (save-excursion
	    ;; if we are not at the end of a block search backwards through buffer
	    (while not-indented
	      (forward-line -1)
	      (if (looking-at "^[ \t]*\\(end if\\|end repeat\\|tms end\\)")
		  ;; if we find a block ending then we are not in a block, 
		  ;; so indent to same level as block ending
		  (progn
		    (setq cur-indent (current-indentation))
		    (setq not-indented nil))
		(if (looking-at "^[ \t]*\\(do if\\|else\\|do repeat\\|tms end\\)")
		    ;; if we find a block start then we are in a block,
		    ;; so indent a bit further
		    (progn
		      (setq cur-indent (+ (current-indentation) default-tab-width))
		      (setq not-indented nil))
		  (if (bobp)
		      ;; if there were no blocks before current position then don't change indentation
		      (setq not-indented nil))))))
	  (if (looking-at "^[ \t]*/")
	      ;; if we are at a subcommand increase the indentation by 2
	      (setq cur-indent (if cur-indent (+ cur-indent 2) 2)))))
      (if cur-indent
	  (indent-line-to cur-indent)
	(indent-line-to 0)))))

(defvar spss-mode-syntax-table
  (let ((spss-mode-syntax-table (make-syntax-table)))
    (modify-syntax-entry ?* "_ 1" spss-mode-syntax-table)
    (modify-syntax-entry ?\s "- 2" spss-mode-syntax-table)
    (modify-syntax-entry ?. ". 3" spss-mode-syntax-table)
    (modify-syntax-entry ?\n "- 4" spss-mode-syntax-table)
    (modify-syntax-entry ?' "\"" spss-mode-syntax-table)
    (modify-syntax-entry ?\\ "@ " spss-mode-syntax-table)
    spss-mode-syntax-table)
  "Syntax table for spss-mode.")

(defun spss-mode ()
  "Major mode for editing spss programs."
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table spss-mode-syntax-table)
  (use-local-map spss-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(spss-font-lock-keywords nil t))
  (set (make-local-variable 'indent-line-function) 'spss-indent-line)
  (setq comment-start "*")
  (setq comment-end ".")
  (local-set-key (kbd "C-c C-c") 'comment-region)
  (local-set-key (kbd "C-c C-u") 'uncomment-region)
  (setq major-mode 'spss-mode)
  (setq mode-name "SPSS syntax")
  (run-hooks 'spss-mode-hook))

(provide 'spss)

;; (magit-push)
;; (yaoddmuse-post "EmacsWiki" "spss.el" (buffer-name) (buffer-string) "update")

;;; spss.el ends here



	  



