;;;
;; unibasic.el -- Copyright (C) 1998 Pat Thoyts <pat@zsplat.freeserve.co.uk>
;;
;; Major mode for working with UniBasic files in EMACS.
;;
;; Author: Pat Thoyts <pat@zsplat.freeserve.co.uk>
;; Revised: Norman Bauer <normanbauer[@]gmail.com>
;; Version: 1.20   (3 March 2009)
;; Version: 1.10   (27 Aug 1999)
;; Maintainer: Pat Thoyts <pat@zsplat.freeserve.co.uk>
;; Keywords: languages
;; 
;; 
;; ----------------------------------------------------------------------
;;
;; Copyright (C) 1998 Pat Thoyts <pat@zsplat.freeserve.co.uk>
;;
;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This program  is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program; see the file `Copying'.  If not, write to the Free 
;; Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;; ----------------------------------------------------------------------
;;
;;; COMMENTARY:
;;  -----------
;;
;;   Provides a major mode that understands the syntax of UniBasic program
;;   files. This is a basic style language used to program the UniData
;;   relational database. unibasic-mode provides text coloration using 
;;   font-lock mode and understands how to indent the source code.
;;
;;; INSTALLATION
;;  ------------
;;
;;   You may need to have make-regexp available. More recent versions of
;;   FSF Emacs have got `regex-opt' already. unibasic-mode will try and
;;   use this. If it isn't available it will go for make-regexp and if
;;   that isn't available it'll ask for it.
;;
;;   Variables you should consider changing are:
;;   `unibasic-initial-indent'      `unibasic-default-indent'
;;   `unibasic-case-indent'         `unibasic-comment-column'
;;   `unibasic-capitalize-keywords' `unibasic-no-blank-lines'
;;
;;   To use this mode, put unibasic-mode.el somewhere in your emacs load
;;   path, compile it (using M-x byte-compile-file) and add the
;;   following to your init file:
;;
;;   (autoload 'unibasic-mode "unibasic-mode" "Unibasic mode." t)
;;     (setq auto-mode-alist 
;;       (append '(("\\.ub$" . unibasic-mode)) auto-mode-alist))
;;
;;   _OR_
;;     type in `M-x unibasic-mode' once the file has been loaded.
;;
;;
;;; SEE ALSO:
;;  --------
;;  proc-mode - a major mode for editing Pick style PROC buffers.
;;
;;; KNOWN BUGS:
;;  -----------
;;
;;   Conversion of blank-lines to commented lines doesn't work yet.
;;
;;   UNTIL and WHILE can appear anywhere within a FOR NEXT loop.
;;   We don't handle this at the moment.
;;
;;; REVISON 1.20
;;  ------------
;;
;;  Norman Bauer: I found this on the gmane mailing list. After 10 years the code still 
;;  worked flawlessly. Byte-compile file warned of obsolete "string-to-int" function which 
;;  I changed to "string-to-number". I hope this file serves someother UniBASIC programmer   
;;  as well as it has serverd me.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst unibasic-rcs-version
  "@(#)$Id: unibasic.el,v 1.10.0.3 1999/09/29 15:56:22 pat Exp pat $"
  "RCS version info for `unibasic-mode'.")

(defconst unibasic-mode-version
  (if (string-match "\\<[0-9]+\\.[0-9]+\\>" unibasic-rcs-version)
      (substring unibasic-rcs-version (match-beginning 0) (match-end 0))
    "0.0")
  "The current version of `unibasic-mode' in use.
The current version was set up to conform to reserved words specified for
UniData 3.3. For other versions you may need to adjust the lists of special
words at the end of unibasic.el and then regenerate the font-lock regular
expressions.")

;; We need to compile our lists of words into regular expressions.
;; In order to do this we need either regexp-opt which is provided with
;; recent emacsen, or we must provide make-regexp.el. Once the library
;; has been byte compiled it will be a lot quicker to load as we won't
;; need to run this for the compiled code.
(eval-when-compile
  (if (condition-case () (require 'regexp-opt) (error nil))
      (defun make-regexp (strings &optional paren lax) nil
        (regexp-opt strings paren))
    (if (not (condition-case () (fboundp 'make-regexp) (error nil)))
        (if (not (load-library "make-regexp"))
            (error "Failed to load make-regexp.el")))))

(defvar unibasic-emacs-type
  (cond ((string-match "XEmacs" emacs-version) 'xemacs)
        ((string-match "Lucid" emacs-version) 'lucid)
        ((string-match "Epoch" emacs-version) 'epoch)
        (t 'emacs)))

;; version checking (modified from XEmacs sample .emacs file).
(if (and (not (boundp 'emacs-major-version))
         (string-match "^[0-9]+" emacs-version))
    (setq emacs-major-version
          (string-to-number (substring emacs-version ;; NVB- Changed string-to-int to string-to-number
                                    (match-beginning 0) (match-end 0)))))
(if (and (not (boundp 'emacs-minor-version))
         (string-match "^[0-9]+\\.\\([0-9]+\\)" emacs-version))
    (setq emacs-minor-version
          (string-to-number (substring emacs-version ;; NVB- Changed string-to-int to string-to-number
                                    (match-beginning 1) (match-end 1)))))
(defun unibasic-emacs-version-or-newer (major minor)
  (or (> emacs-major-version major)
      (and (= emacs-major-version major)
           (>= emacs-minor-version minor))))

;;; handle versions of emacs without custom.
(unless (and (featurep 'custom) (fboundp 'defcustom))
  (defmacro defgroup (&rest args) nil)
  (defmacro defface (&rest args) nil)
  (defmacro defcustom (var value doc &rest args) 
    "For versions without `custom', use defvar.
The args are (name default-value description &optional .....)"
    (defvar var value doc)))

(defgroup unibasic nil
  "Major mode for editing UniBasic source in Emacs."
  :prefix "unibasic-"
  :group 'languages)
(defcustom unibasic-mode-hook ()
  "*User hook for `unibasic-mode' called after the mode starts up."
  :type 'hook
  :group 'unibasic)
(defcustom unibasic-initial-indent 6
  "*The value for the initial indentation of unibasic statements. This gives
a bit of room for labels. The default value is 6."
  :type 'integer
  :group 'unibasic)
(defcustom unibasic-default-indent 4
  "*Indentation of unibasic statements with respect to containing block."
  :type 'integer
  :group 'unibasic)
(defcustom unibasic-case-indent -2
  "*Indentation offset for unibasic case statements within the case block."
  :type 'integer
  :group 'unibasic)
(defcustom unibasic-continuation-indent 2
  "*Indentation offset for unibasic statements continued to the next line."
  :type 'integer
  :group 'unibasic)
(defcustom unibasic-comment-column 40
  "*Indentation for unibasic inline comments."
  :type 'integer
  :group 'unibasic)
(defcustom unibasic-tab-always-indent t
  "*Non-nil means TAB in `unibasic-mode' should always reindent the current
line, regardless of where in the line the point is when a TAB is used."
  :type 'boolean
  :group 'unibasic)
(defcustom unibasic-capitalize-keywords t
  "*Non-nil means capitalise all keywords as they are typed."
  :type '(choice (const null) boolean)
  :group 'unibasic)
(defcustom unibasic-no-blank-lines t
  "*Non-nil means no blank lines are to appear in the file.
Any blank lines will be converted into a comment line by prefixing a '*' in
column 0 (as done by IPG). This doesn't work yet."
  :type 'boolean
  :group 'unibasic)
(defcustom unibasic-use-magic-labels t
  "*Non-nil means labels after GOTO and GOSUB are made into hot-spots which
can be clicked with shift-mouse-1 taking you to the location of the label
definition."
  :type 'boolean
  :group 'unibasic)
(defcustom unibasic-font-lock-syntactify
  (cond
   ((and (eq unibasic-emacs-type 'emacs)
         (unibasic-emacs-version-or-newer 20 0))
    t)
   (t nil))
  "*Non-nil means use font-lock-mode to deal with setting the proper
syntax for the * character within the buffer. By default set true if
your version of emacs supports this. Emacs 20.3 does, XEmacs 20 does not.
It is definately preferable to have this set if possible."
  :type '(choice (const null) boolean)
  :group 'unibasic)

(defvar unibasic-label-regexp
  "^[ \t]*\\(\\([0-9]+:?\\)\\|\\([a-zA-Z][a-zA-Z0-9_$\\.]*:\\)\\)"
  "Regular expression used to recognise unibasic labels.")

(defvar unibasic-comment-regexp
  "\\(^[ \t]*[!\\*]\\)\\|\\(;[ \t]*\\*.*\\)"
  "Regular expression used to recognise unibasic comments.")

(defvar unibasic-block-start-regexp
  "\\<\\(THEN\\|ELSE\\|LOCKED\\|ON ERROR\\)[ \t]*\\($\\|;[ \t]*\\*\\)"
  "Regular expression used to recognise the start of a unibasic block of
statements for indentation.")

(defvar unibasic-block-end-regexp
  "END\\|REPEAT\\|NEXT\\|WHILE\\|UNTIL"
  "Regular expression used to recognise the end of a unibasic block of
statements for indentation.")

(defvar unibasic-continuation-char
  ?|
  "Line continuation character used in `unibasic-mode'.
This is used by `unibasic-continued-line' to decide if the next line should be
indented by `unibasic-continuation-indent'.")

(defvar unibasic-separator-char
  ?\;
  "Separator character used in `unibasic-mode'.")

(defvar unibasic-labels-timer
  nil
  "Variable to hold the last timeout event. Used in `unibasic-indent-line'.")

(defvar unibasic-imenu-generic-expression
  (list (list "LABELS"  unibasic-label-regexp  0))
  "Imenu expression for `unibasic-mode'.  See `imenu-generic-expression'.
To use imenu for all unibasic files, put
(add-hook 'unibasic-mode-hook 'imenu-add-menubar-index)
into your .emacs file to get an `Index' menu item containing all the labels
in you source code.")

(defvar unibasic-labels-alist nil
  "Association list used for hot-spot labels.")

(defvar unibasic-mode-abbrev-table nil
  "Abbreviation table used in `unibasic-mode' buffers.")
(define-abbrev-table 'unibasic-mode-abbrev-table ())

(defvar unibasic-mode-syntax-table nil
  "Syntax table used in `unibasic-mode' buffers.")
(if unibasic-mode-syntax-table
    ()
  (setq unibasic-mode-syntax-table (make-syntax-table))
  (if unibasic-font-lock-syntactify
      (modify-syntax-entry ?\* "."    unibasic-mode-syntax-table)
    (modify-syntax-entry ?\* "<"    unibasic-mode-syntax-table))
  (modify-syntax-entry ?\; "."    unibasic-mode-syntax-table)
  (modify-syntax-entry ?\n ">"    unibasic-mode-syntax-table)
  (modify-syntax-entry ?\! "<"    unibasic-mode-syntax-table)
  (modify-syntax-entry ?\( "()"   unibasic-mode-syntax-table)
  (modify-syntax-entry ?\) ")("   unibasic-mode-syntax-table)
  (modify-syntax-entry ?\< "(>"   unibasic-mode-syntax-table)
  (modify-syntax-entry ?\> ")<"   unibasic-mode-syntax-table)
  (modify-syntax-entry ?\[ "(]"   unibasic-mode-syntax-table)
  (modify-syntax-entry ?\] ")["   unibasic-mode-syntax-table)
  (modify-syntax-entry ?\" "\"\"" unibasic-mode-syntax-table)
  (modify-syntax-entry ?\' "\"'"  unibasic-mode-syntax-table)
  (modify-syntax-entry ?\\ "."    unibasic-mode-syntax-table)
  (modify-syntax-entry ?\+ "."    unibasic-mode-syntax-table)
  (modify-syntax-entry ?\- "."    unibasic-mode-syntax-table)
  (modify-syntax-entry ?\= "."    unibasic-mode-syntax-table)
  (modify-syntax-entry ?\# "."    unibasic-mode-syntax-table)
  (modify-syntax-entry ?\% "."    unibasic-mode-syntax-table)
  (modify-syntax-entry ?\& "."    unibasic-mode-syntax-table)
  (modify-syntax-entry ?\| "."    unibasic-mode-syntax-table))

(defvar unibasic-mode-map ()
  "Keymap used in `unibasic-mode' buffers.")
(defvar unibasic-menu-map ()
  "Menu keymap used in `unibasic-mode' buffers by GNU Emacs.")
(if unibasic-mode-map
    ()
  (setq unibasic-mode-map (make-sparse-keymap))
  (setq unibasic-menu-map (make-sparse-keymap "Unibasic"))
  ;; make the ';' key electric.
  (let ((ekey (char-to-string unibasic-separator-char)))
    (define-key unibasic-mode-map ekey     'unibasic-electric-separator))
  (define-key unibasic-mode-map "!"        'unibasic-electric-star)
  (define-key unibasic-mode-map "*"        'unibasic-electric-star)
  (define-key unibasic-mode-map ":"        'unibasic-electric-colon)
  (define-key unibasic-mode-map "\t"       'unibasic-electric-tab)
  (define-key unibasic-mode-map "\r"       'unibasic-electric-newline)
  (define-key unibasic-mode-map "\M-\r"    'unibasic-continued-newline)
  (define-key unibasic-mode-map "\C-ca"    'unibasic-prev-command)
  (define-key unibasic-mode-map "\C-ce"    'unibasic-next-command)
  (define-key unibasic-mode-map "\C-cg"    'unibasic-grep)
  (define-key unibasic-mode-map "\C-cU"    'unibasic-insert-emacs-tag)
  (define-key unibasic-mode-map "\M-;"     'unibasic-append-comment)
  (define-key unibasic-mode-map "\C-c*"    'unibasic-append-comment)
  (define-key unibasic-mode-map "\M-a"     'unibasic-backward-to-separator)
  (define-key unibasic-mode-map "\M-e"     'unibasic-forward-to-separator)
  (define-key unibasic-mode-map "\M-\C-a"  'unibasic-skip-to-prev-label)
  (define-key unibasic-mode-map "\M-\C-e"  'unibasic-skip-to-next-label)
  (define-key unibasic-mode-map "\M-[."    'unibasic-skip-to-prev-label)
  (define-key unibasic-mode-map "\M-];"    'unibasic-skip-to-next-label)
  (define-key unibasic-mode-map "\M-\C-h"  'unibasic-mark-label-block)
  (define-key unibasic-mode-map "\M-q"     'indent-region)
  (define-key unibasic-mode-map "\C-c\C-b" 'unibasic-submit-bug-report)
  (cond
   ((eq unibasic-emacs-type 'xemacs)
    (define-key unibasic-mode-map [(shift button1)] 'unibasic-follow-label))
   (t
    (define-key unibasic-mode-map [S-down-mouse-1]  'unibasic-follow-label)))
  ;; Insert some functions into the menu-bar.
  (define-key unibasic-mode-map [menu-bar insert]
    (cons "Unibasic" unibasic-menu-map))

  (define-key unibasic-menu-map [ub-bug]
    '("Send bug report" . unibasic-send-bug-report))
  (define-key unibasic-menu-map [ub-sep1]
    '("--"))
  (define-key unibasic-menu-map [ub-etag]
    '("Append Emacs Tag" . unibasic-insert-emacs-tag))
  (define-key unibasic-menu-map [ub-comm]
    '("Append Comment" . unibasic-append-comment))
  (define-key unibasic-menu-map [ub-sep2]
    '("--"))
  (define-key unibasic-menu-map [ub-indent]
    '("Indent Region" . unibasic-indent-region))
  (define-key unibasic-menu-map [ub-mlabel]
    '("Mark Label Block" . unibasic-mark-label-block))
  (define-key unibasic-menu-map [ub-blabel]
    '("Backward by Label" . unibasic-skip-to-prev-label))
  (define-key unibasic-menu-map [ub-flabel]
    '("Forward by Label" . unibasic-skip-to-next-label))
  (define-key unibasic-menu-map [ub-bcmd]
    '("Backward by Command" . unibasic-prev-command))
  (define-key unibasic-menu-map [ub-fcmd]
    '("Forward by Command" . unibasic-next-command)) )

(defconst unibasic-xemacs-menu
  '("Unibasic"
    ["Forward by Command"  unibasic-next-command       t]
    ["Backward by Command" unibasic-prev-command       t]
    ["Forward by Label"    unibasic-skip-to-next-label t]
    ["Backward by Label"   unibasic-skip-to-prev-label t]
    ["Mark Label Block"    unibasic-mark-label-block   t]
    ["Indent Region"       unibasic-indent-region      t]
    ["Comment Region"      comment-region              t]
    ["Uncomment Region"    unibasic-uncomment-region   nil]
    "----"
    ["Append Comment"      unibasic-append-comment     t]
    ["Append Emacs Tag"    unibasic-insert-emacs-tag   t]
    "----"
    ["Send bug report"     unibasic-submit-bug-report  t])
  "Unibasic menu for use with XEmacs.")

;;;###autoload
(defun unibasic-mode ()
  "Major mode for display and editing of UniBasic programs.

This is a major mode that understands the syntax of UniBasic program
files. This is a basic style language used to program the UniData
relational database. `unibasic-mode' provides text coloration using 
\\[font-lock-mode] and understands how to indent the source code.
Starting `unibasic-mode' runs `unibasic-mode-hook'.

Variables you might consider changing are:
 `unibasic-initial-indent'      the size of the lefthand margin (label space)
 `unibasic-default-indent'      the normal amount to indent by.
 `unibasic-case-indent'         the adjustment for CASE within BEGIN/END CASE
 `unibasic-continuation-indent' the adjustment for continued lines
 `unibasic-comment-column'      column for comments after code (M-;)
 `unibasic-no-blank-lines'      blank lines are converted to comments (for IPG)
 `unibasic-capitalize-keywords' raise the case of keywords.

Special commands:
\\{unibasic-mode-map}
"
  (interactive)
  (kill-all-local-variables)
  (use-local-map unibasic-mode-map)
  (setq major-mode 'unibasic-mode
        mode-name "unibasic")
  (unibasic-construct-keyword-abbrev-table)
  (setq local-abbrev-table unibasic-mode-abbrev-table)
  (set-syntax-table unibasic-mode-syntax-table)
  (make-local-variable 'indent-line-function)
  (make-local-variable 'parse-sexp-ignore-comments)
  (make-local-variable 'case-fold-search)
  (make-local-variable 'comment-start-skip)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (make-local-variable 'unibasic-initial-indent)
  (make-local-variable 'unibasic-default-indent)
  (make-local-variable 'unibasic-case-indent)
  (make-local-variable 'unibasic-continuation-indent)
  (make-local-variable 'unibasic-comment-column)
  (make-local-variable 'unibasic-capitalize-keywords)
  (make-local-variable 'unibasic-no-blank-lines)
  (setq indent-line-function 'unibasic-indent-line ;; for indent-region
        indent-tabs-mode nil           ;; make all tabs spaces.
        comment-indent-function 'unibasic-indent-comment
        parse-sexp-ignore-comments t   ;; allow lisp embedded in comments
        case-fold-search nil           ;; make searches case-sensitive
        comment-start "*"              ;; string used in comment-region
        comment-end ""                 ;; string used for ending comments.
        comment-start-skip unibasic-comment-regexp)
  (if unibasic-capitalize-keywords
      (progn
        (make-local-variable 'pre-abbrev-expand-hook)
        (add-hook 'pre-abbrev-expand-hook 'unibasic-pre-abbrev-expand-hook)
        (abbrev-mode 1)))
  ;; Font lock support
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(unibasic-font-lock-keywords))
  (cond
   (unibasic-font-lock-syntactify
    (make-variable-buffer-local 'parse-sexp-lookup-properties)
    (set 'parse-sexp-lookup-properties t)
    (make-variable-buffer-local 'font-lock-syntactic-keywords)
    (setq font-lock-syntactic-keywords unibasic-font-lock-syntactic-keywords)))
  ;; Imenu support
  (set (make-local-variable 'imenu-generic-expression)
       unibasic-imenu-generic-expression)
  (setq imenu-case-fold-search t)
  ;; function-menu support
  (cond
   ((condition-case () (require 'func-menu) (error nil))
    (add-to-list 'fume-function-name-regexp-alist
                 '(unibasic-mode . unibasic-fume-function-name-regexp))
    (add-to-list 'fume-find-function-name-method-alist
                 '(unibasic-mode . unibasic-fume-find-next-function-name))))
  (if (equal unibasic-emacs-type 'xemacs)
      (add-submenu nil unibasic-xemacs-menu))

  ;; Allow users to add functions to be run for this mode.
  (make-variable-buffer-local 'unibasic-labels-timer)
  (make-variable-buffer-local 'unibasic-labels-alist)
  (unibasic-set-labels-alist)
  (unibasic-find-magic-labels)
  (run-hooks 'unibasic-mode-hook))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                       The Functions.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; First, some utility functions and macros.
(defsubst unibasic-get-bol (&optional arg)
  (save-excursion (beginning-of-line arg) (point)))
(defsubst unibasic-get-eol (&optional arg)
  (save-excursion (end-of-line arg) (point)))
(defsubst unibasic-within-string ()
  (save-excursion (nth 3 (parse-partial-sexp (unibasic-get-bol) (point)))))
(defsubst unibasic-within-comment ()
  (save-excursion (nth 4 (parse-partial-sexp (unibasic-get-bol) (point)))))

(defsubst unibasic-vboundp (symbol)
  "Like fboundp. Return true if SYMBOL has been defined."
  (if (condition-case () (symbol-value symbol) (error nil))
      t
    nil))

;; More Emacs/XEmacs compatability functions.
(if (not (fboundp 'char-before))
    (defun char-before ( point )
      "GNU Emacs 19.34 doesn't have char-before. Hence...
Return charater in current buffer before position POS.
POS is an integer or buffer pointer."
      (char-after (1- point))) )

(defun unibasic-cancel-timer (arg)
  "Call the Emacs or XEmacs function to cancel a timer."
  (if (equal unibasic-emacs-type 'xemacs)
      (disable-timeout arg)
    (cancel-timer arg)))
        
(defun unibasic-add-timer (timeout func)
  "Call the Emacs or XEmacs function to set an idle timer.
TIMEOUT is the number of idle seconds to wait and FUNC is the function."
  (if (equal unibasic-emacs-type 'xemacs)
      (add-timeout timeout func nil)
    (run-with-idle-timer timeout nil func)))

(defsubst unibasic-label-only-regexp ()
  (concat "\\(" unibasic-label-regexp
          "\\)[ \t]*\\($\\|\\*\\|;[ \t]*\\*\\)"))

(defun unibasic-continued-line (&optional line)
  "Returns true if the line ends with the `unibasic-continuation-char'."
  (let* ((c (char-before (unibasic-get-eol line)))
         (c (if (null c) ?\0 c)));; code to cope with a nil for char-before.
    (char-equal c unibasic-continuation-char)))

(defun unibasic-syntactic-line (line)
  "Return the line offset for the next line of code.
Beginning at start, search back and skip any comments or blank lines until
the next line of unibasic code. This uses and return the same offsets as
`looking-at' and similar, i.e. 1 for the current line, 0 for the previous
line etc."
  (while (and (or (unibasic-looking-at unibasic-comment-regexp line)
                  (unibasic-looking-at "^[ \t]*$" line)
                  (unibasic-looking-at (unibasic-label-only-regexp) line))
              (> (unibasic-get-bol line) 1))
    (setq line (1- line)))
  line)

(defun unibasic-looking-at (regexp &optional line)
  "Look for a regular expression REGEXP at the beginning of line LINE.
Used mainly by `unibasic-indent-line'."
  (save-excursion
    (beginning-of-line line)
    (looking-at regexp)))

(defun unibasic-looking-at-line (regexp &optional line)
  "Look for a regular expression REGEXP which starts a unibasic syntactic
line. i.e: follows a label, or ; or opens a new line."
  (unibasic-looking-at 
   (concat "\\(" unibasic-label-regexp "\\|"
           "^\\|.*;\\)[ \t]*\\(" regexp "\\)" ) line))

;; electric keys for this mode.
(defun unibasic-electric-separator (&optional count)
  "Electric character for `unibasic-mode' when the `unibasic-separator-char'
is entered."
  (interactive "P")
  (if (not count) (setq count 1))
  (insert-char unibasic-separator-char count))

(defun unibasic-skip-space-backward ()
  "Skip back over whitespace from point and return the next non
white-space character on this line."
  (save-excursion 
    (backward-char)
    (let ((p (progn
               (skip-syntax-backward 
                " "
                (save-excursion (beginning-of-line) (point)))  
               (point)) ))
      (char-before p))))

(defun unibasic-electric-star (&optional count)
  "Insert '*' or '!' and indent to column 0 if this is an empty line."
  (interactive "p")
  (insert-char last-command-char count)
  (if (save-excursion (beginning-of-line) (looking-at "^[ \t]*\\(\\*\\|!\\)"))
      (save-excursion (beginning-of-line) (delete-horizontal-space))))

(defun unibasic-electric-colon (&optional count)
  "Insert a colon. If this is terminating a label then indent to 0."
  (interactive "p")
  (insert-char ?: count)
  (if (save-excursion (beginning-of-line) (looking-at unibasic-label-regexp))
      (progn
        (save-excursion
          (beginning-of-line)
          (delete-horizontal-space))
        (if (< (current-column) unibasic-default-indent)
            (insert-char ? (- unibasic-default-indent (current-column)))))))

(defun unibasic-electric-tab (&optional count)
  "Function called in `unibasic-mode' when a TAB is entered."
  (interactive "p")
  ;; Don't do anything special if in a string.
  (if (unibasic-within-string)
      (insert-char (string-to-char "\t") count)
    (if unibasic-tab-always-indent
        (save-excursion
          (beginning-of-line)
          (unibasic-indent-line))
      (if (save-excursion
            (skip-syntax-backward " ")
            (bolp))
          (unibasic-indent-line)
        (insert-char (string-to-char "\t") count)))
    (unibasic-indent-line)))

(defun unibasic-electric-newline (&optional count)
  "Insert a newline, updating indentation."
  (interactive "p")
  (if (not count) (setq count 1))
  (delete-horizontal-space)
  (save-excursion (unibasic-indent-line))
  (while (> count 0)
    (newline)
    (unibasic-indent-line)
    (setq count (1- count))))

(defun unibasic-continued-newline ()
  "Continue line by inserting `unibasic-continuation-char' at the end of the line.
Bound to \\[unibasic-continued-newline]"
  (interactive)
  (insert-char unibasic-continuation-char 1)
  (unibasic-electric-newline))

(defun unibasic-append-comment (&optional count)
  "Insert a comment at the default comment column on the current line."
  ;; If the optional count (or a numeric prefix) is given. Do count lines."
  (interactive "p")
  (if (not count) (setq count 1))
  (if (unibasic-within-comment)
      ()
    (end-of-line)
    (if (< (current-column) unibasic-comment-column)
        (indent-to unibasic-comment-column)
      (insert "  "))
    (insert-char unibasic-separator-char 1)
    (insert comment-start)))
  
(defun unibasic-pre-abbrev-expand-hook ()
  "Hook for capitalising keywords using abbrev-mode but only if we are
in the coding part of the buffer. Set with `unibasic-capitalize-keywords'"
  (setq local-abbrev-table
        (if (or (unibasic-within-comment)
                (unibasic-within-string))
            ()
          unibasic-mode-abbrev-table)))

(defun unibasic-construct-keyword-abbrev-table ()
  "Build an abbreviation table from the command words so that these
can be automagically entered in upper case."
  (let ((words (append unibasic-command-words
                       unibasic-keyword-words
                       unibasic-operator-words
                       unibasic-function-words))
        (word nil)
        (list nil))
    (while words
      (setq word (car words)
            words (cdr words))
      (setq list (cons (list (downcase word) word) list)))
    (define-abbrev-table 'unibasic-mode-abbrev-table list)))

(defsubst unibasic-forward-to-separator (&optional count)
  (interactive "p")
  (while (> count 0)
    (skip-chars-forward
     (concat "^\n" (char-to-string unibasic-separator-char)))
    (skip-chars-forward
     (concat "\n" (char-to-string unibasic-separator-char)))
    (setq count (1- count))))

(defsubst unibasic-backward-to-separator (&optional count)
  (interactive "p")
  (while (> count 0)
    (skip-chars-backward
     (concat "^\n" (char-to-string unibasic-separator-char)))
    (skip-chars-backward
     (concat "\n" (char-to-string unibasic-separator-char)))
    (setq count (1- count))))

(defun unibasic-skip-to-next-label (&optional count)
  "Jump to the next unibasic label using the `unibasic-label-regexp'
regular expression. Bound to \\[unibasic-skip-to-next-label]"
  (interactive "p")
  (if (not
       (re-search-forward unibasic-label-regexp (point-max) t count))
      (goto-char (point-max))))

(defun unibasic-skip-to-prev-label (&optional count)
  "Jump to the previous unibasic label using the `unibasic-label-regexp'
regular expression. Bound to \\[unibasic-skip-to-prev-label]"
  (interactive "p")
  (if (not
       (re-search-backward unibasic-label-regexp (point-min) t count))
      (goto-char (point-min))))

(defun unibasic-prev-command (&optional count)
  "Move point to the previous unibasic command word.
Bound to \\[unibasic-prev-command]."
  (interactive "p")
  (let ((r (re-search-backward unibasic-command-regexp (point-min) t count)))
    (if (not r)
        (goto-char (point-min))
      r)))

(defun unibasic-next-command (&optional count)
  "Move point to the next unibasic command word.
Bound to \\[unibasic-next-command]."
  (interactive "p")
  (let ((r (re-search-forward unibasic-command-regexp (point-max) t count)))
    (if (not r)
        (goto-char (point-max))
      r)))

(defun unibasic-mark-label-block ()
  "Mark a region between two unibasic labels.
Mark the region between the two nearest labels or the top and bottom of
the buffer if there are no labels.
Bound to \\[unibasic-mark-label-block]."
  (interactive)
  (push-mark (point))
  (unibasic-skip-to-prev-label)
  (push-mark (point))
  (if (not (bobp)) (unibasic-skip-to-next-label))
  (unibasic-skip-to-next-label)
  (if (fboundp 'zmacs-activate-region);; for XEmacs
      (zmacs-activate-region)))

(defun unibasic-default-tag ()
  "Find the word at point for use as the suggested tag for grep."
  (if (and (not (bobp))
           (save-excursion
             (backward-char 1)
             (looking-at "\\w")))
      (backward-word 1))
  (let ((s (point))
        (e (save-excursion
             (forward-word 1)
             (point))))
    (buffer-substring s e)))

(defun unibasic-grep (tag)
  "Search source files in current directory for a tag.
You need to have an external grep command available i.e.: GNU grep.
Bound to \\[unibasic-grep]"
  (interactive
   (list (let* ((def (unibasic-default-tag))
                (tag (read-string
                      (format "grep for [%s]: " def))))
           (if (string= tag "") def tag))))
  (grep (format "grep -n %s %s" tag "*")))

(defun unibasic-indent-level (&optional count)
  "Return the indent-level any line.
This finds the indent level for a line, offset from the current line
by the optional argument, as for `beginning-of-line'. The indent-level
is calculated excluding any unibasic label that might be present. If
the preceding line is a comment look back to the next non-comment for
the indentation level or use `unibasic-initial-indent'."
  (save-excursion
    (beginning-of-line count)
    (cond ((bobp)
           unibasic-initial-indent)
          ((and (unibasic-looking-at unibasic-comment-regexp)
                (not (bobp)))
           (unibasic-indent-level 0));; keep looking for non-comment.
          ((and (unibasic-looking-at (unibasic-label-only-regexp))
                (not (bobp)))
           (unibasic-indent-level 0))
          (t
           (if (unibasic-looking-at unibasic-label-regexp)
               (goto-char (match-end 0)))
           (skip-syntax-forward " ")
           (let ((ret (if (< (current-column) unibasic-initial-indent)
                          unibasic-initial-indent
                        (current-column))))
             (cond
              ((unibasic-continued-line 0)
               (setq ret (- ret unibasic-continuation-indent)))
              ;; doesn't cope with LOOP ; N+=1 ; UNTIL N>10 ; PRINT ; REPEAT
              ((unibasic-looking-at
                (concat "\\(" unibasic-label-regexp "\\|"
                        "^\\|.*;\\)[ \t]*\\<\\(UNTIL\\|WHILE\\)\\>"))
               (setq ret (+ ret unibasic-default-indent))) )
             ret)) )))

(defun unibasic-calculate-indent ()
  "Calculate the indent column for the current line.
This is done by finding the `unibasic-indent-level' for the preceeding
non-comment line and examining the code for opening or closing a new
indent level.
  Comments and labels are indented to column 0. Everthing else is indented
to `unibasic-initial-indent' plus `unibasic-default-indent' as needed."

  (let* ((line (unibasic-syntactic-line 0)) ; find the last syntactic line
         (prevcol (unibasic-indent-level line)) ; get indent of prev line
         (thiscol prevcol))             ; initial indent for this line

    (if (save-excursion;; look for block starts, skipping comments.
          (beginning-of-line (1+ line))
          (re-search-backward unibasic-block-start-regexp
                              (unibasic-get-bol 0) t))
        (setq thiscol (+ thiscol unibasic-default-indent)))
    (if (and (unibasic-looking-at-line "\\<FOR\\>" line)
             (not (unibasic-looking-at-line 
                   "\\<FOR[ \t].*;[ \t]*NEXT" line)))
        (setq thiscol (+ thiscol unibasic-default-indent)))
    (if (and (unibasic-looking-at-line "\\<LOOP\\>" line)
             (not (unibasic-looking-at-line
                   "\\<LOOP[ \t].*;[ \t]*UNLESS\\|WHILE\\|REPEAT" line)))
        (setq thiscol (+ thiscol unibasic-default-indent)))
    (if (unibasic-looking-at-line "\\<BEGIN CASE\\>" line)
        (setq thiscol (+ thiscol unibasic-default-indent)))
    (if (unibasic-looking-at-line "[ \t]*CASE" line)
        (setq thiscol (- prevcol unibasic-case-indent)))
    (if (unibasic-looking-at-line
         (concat "\\(" unibasic-block-end-regexp "\\)\\([; \t]\\|$\\)"))
        (setq thiscol (- thiscol unibasic-default-indent)))
    (cond
     ((unibasic-looking-at unibasic-comment-regexp);; leave comments alone.
      (current-indentation))
     ((unibasic-looking-at-line "\\<CASE\\>");; handle case statements
      (+ thiscol unibasic-case-indent))
     ((unibasic-continued-line 0);; the continuation character
      (+ thiscol unibasic-continuation-indent))
     (t
      thiscol))))

(defun unibasic-indent-line ()
  "Indent lines of code for `unibasic-mode'.
The amount to indent is calculated using `unibasic-calculate-indent'
and checking for the presence of `unibasic-label-regexp' or
`unibasic-comment-regexp' matches for comments or labels."
  (interactive)
  (let ((col (unibasic-calculate-indent)))
    (save-excursion
      (cond ((looking-at "[ \t]*;\\*")
             (delete-horizontal-space)
             (indent-to unibasic-comment-column 2))))
    (beginning-of-line)
    (cond
     ((looking-at unibasic-comment-regexp);; ignore comment lines.
      ())
     ;;     ((and (looking-at "^[ \t]*$")         ;; blank lines are commented
     ;;      (not (or (char-equal last-command-char (string-to-char "\n"))
     ;;          (char-equal last-command-char (string-to-char "\r"))))
     ;;      unibasic-no-blank-lines)        ;; if option is t
     ;;      (message "%s" last-command-char)
     ;;      (delete-horizontal-space)
     ;;      (insert "* "))
     ((looking-at unibasic-label-regexp);; labels to column 0
      (if unibasic-labels-timer
          (unibasic-cancel-timer unibasic-labels-timer))
      (setq unibasic-labels-timer
            (unibasic-add-timer 3 'unibasic-set-labels-alist))
      (delete-horizontal-space)
      (re-search-forward unibasic-label-regexp)
      (delete-horizontal-space)
      (if (looking-at "[ \t]*$")
          (indent-to col)
        (indent-to col 1)
        (if unibasic-use-magic-labels
            (unibasic-magic-labels (unibasic-get-bol) (unibasic-get-eol))) ))
     (t
      (delete-horizontal-space)
      (indent-to col)
      (if unibasic-use-magic-labels
	  (unibasic-magic-labels (unibasic-get-bol) (unibasic-get-eol))) ))))

(defconst unibasic-fume-function-name-regexp
  "^[ \t]*\\(\\([0-9]+:?\\)\\|\\([a-zA-Z][a-zA-Z0-9_$\\.]*:\\)\\)"
  "Expression to get unibasic label names for use with `function-menu'")

(defun unibasic-fume-find-next-function-name (buffer)
  "Support function for `function-menu' (used in XEmacs)."
  (interactive "b")
  (set-buffer buffer)
  (if (re-search-forward unibasic-fume-function-name-regexp nil t)
      (let ((beg (progn (beginning-of-line 1) (point))))
        (forward-sexp)
        (cons (buffer-substring (match-beginning 1) (match-end 1))
              (match-beginning 1)))))

(defun unibasic-magic-labels (&optional start end)
  "Add a mouse-face text property to labels following GOTO etc statements.
This allows the user to click the label with mouse-2 to goto that part
of the file."
  (interactive)
  (if (condition-case () (mark) (error nil))
      (setq start (mark)
            end (point))
    (if (not start)
        nil))
  (save-excursion
    (save-restriction
      (let ((iro inhibit-read-only)     ;; Work with read-only files
            (bul buffer-undo-list)      ;; Don't record this as a change.
            (bmp (buffer-modified-p)))
        (setq inhibit-read-only t
              buffer-undo-list t)
        (if (> start end)
            (narrow-to-region end start)
          (narrow-to-region start end))
        (goto-char (point-min))
        (remove-text-properties (point-min) (point-max) '(mouse-face nil))
        (while (re-search-forward 
                "GO\\(SUB\\|TO\\)?[ \t]*\\([^ \t\n\r,;]+\\)$?"
;;                "GO\\(SUB\\|TO\\)?\\w+\\([^;]+\\)\\(;\\|$\\)"
;; would use (match-string 0) and then more stuff.
                (point-max) t)
          (let ((m (match-data))
                (s 4))
            (while (elt m s)
              (add-text-properties (elt m s) (elt m (1+ s))
                                   '( mouse-face highlight
                                      rear-nonsticky (mouse-face)))
              (setq s (+ 2 s)))))
        (set-buffer-modified-p bmp)
        (setq inhibit-read-only iro
              buffer-undo-list bul)) )))

(defun unibasic-set-labels-alist (&optional arg)
  "Function to be called from a timer to reset the labels alist.
Clears the timer variable and calls `unibasic-scan-for-labels' to do the job."
  (setq unibasic-labels-timer nil)
  (message "Re-scanning unibasic labels.")
  (setq unibasic-labels-alist (unibasic-scan-for-labels (current-buffer))))

(defun unibasic-scan-for-labels (buffer)
  "Scan BUFFER looking for any Unibasic labels and put them into an alist.
The car of each element is the text of the label, the cdr is the buffer
position."
  (interactive "b")
  (save-excursion
    (set-buffer buffer)
    (goto-char (point-min))
    (let ((alist nil))
      (while (re-search-forward unibasic-label-regexp nil t)
        (forward-sexp)
        (setq alist 
              (append alist 
                      (list (cons (buffer-substring 
                                   (match-beginning 1)
                                   (match-end 1))
                                  (match-beginning 1))))) )
      alist)))

(defun unibasic-find-magic-labels ()
  "Mark all use of GOTO and GOSUB labels in the buffer. Usually called with
find-file-hooks."
  (interactive)
  (message "Highlighting unibasic labels.")
  (unibasic-magic-labels (point-min) (point-max)))

(defun unibasic-event-point (event)
  (if (eq unibasic-emacs-type 'xemacs)
      (event-point event)
    (posn-point (event-end event))))

(defun unibasic-event-window (event)
  (if (eq unibasic-emacs-type 'xemacs)
      (event-window event)
    (posn-window (event-end event))))

(defun unibasic-follow-label (event)
  "Handle mouse click. If over a label, jump to the label definition."
  (interactive "e")
  (let ((y (save-excursion
	     (set-buffer (window-buffer (unibasic-event-window event)))
	     (get-char-property (unibasic-event-point event) 'mouse-face))))
    (set-buffer (window-buffer (unibasic-event-window event)))
    (select-window (unibasic-event-window event))
    (goto-char (unibasic-event-point event))
    (if (not y)
	nil
      (if (get-char-property (point) 'mouse-face)
	  (let* ((pre (previous-single-property-change (point) 'mouse-face))
		 (pos (next-single-property-change (point) 'mouse-face))
                 (pre (if pre pre (point)))
                 (pos (if pos pos (point)))
		 (txt (buffer-substring-no-properties pre pos))
                 (mark (unibasic-label-position txt)))
	    (if mark 
                (goto-char mark)
              (message "Failed to match label %s" txt) ))))))

(defun unibasic-label-position (label)
  "Return the position of LABEL in the buffer."
  (let ((pos nil)
        (label2 (concat label ":")))
    (setq pos (assoc label unibasic-labels-alist))
    (if (not pos)
        (setq pos (assoc label2 unibasic-labels-alist)))
    (if pos
        (cdr pos)
      nil)))

;;;###autoload
(defun unibasic-insert-emacs-tag ()
  "Insert emacs notification at the end of the buffer to set the file to
unibasic mode whenever it is loaded.
Bound to \\[unibasic-insert-emacs-tag]"
  (interactive)
  (goto-char (point-max))
  (if (not (save-excursion
             (re-search-backward "End:[ \t]*$" 
                                 (save-excursion 
                                   (beginning-of-line -5) (point))
                                 t)))
      (insert "\n*\n* Local variables:\n* mode: unibasic\n* End:\n")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Words for font-lock-mode and abbrev-mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-and-compile
  (defvar unibasic-command-words
    '("$BASICTYPE" "$DEFINE" "$UNDEFINE" "$IFDEF" "$ELSE" "$ENDIF" "$IFNDEF"
      "$INCLUDE" "$INSERT" "ABORT" "ASSIGN" "BEGIN" "BPIOCP" "BREAK" "CALL"
      "CALLC" "CASE" "CHAIN" "CLEAR" "CLEARCOM" "CLEARCOMMON" "CLEARDATA"
      "CLEARFILE" "CLEARINPUT" "CLEARSELECT" "CLEARSQL" "CLOSE" "CLOSESEQ"
      "COM" "COMMENT" "COMMIT" "COMMON" "CONNECT" "CONTINUE" "CONVERT" "CRT"
      "DATA" "DEBUG" "DEFFUN" "DEL" "DELETE" "DELETELIST" "DELETION"
      "DELETEU" "DIM" "DIMENSION" "DISCONNECT" "DISPLAY" "ECHO" "END"
      "ENTER" "EQU" "EQUATE" "EXECUTE" "EXECUTESQL" "EXIT" "FILELOCK"
      "FILEUNLOCK" "FIND" "FINDSTR" "FOOTING" "FOR" "FORMLIST" "FUNCTION"
      "GARBAGECOLLECT" "GET" "GETCOLUMNDATA" "GETCOLUMNNAME" "GETLIST"
      "GETX" "GOSUB" "GOTO" "GROUPSTORE" "HEADING" "HUSH" "IF" "INPUT"
      "INPUTCLEAR" "INPUTERR" "INPUTIF" "INPUTNULL" "INPUTTRAP" "INS"
      "LOCATE" "LOCK" "LOOP" "MAT" "MATBUILD" "MATPARSE" "MATREAD"
      "MATREADL" "MATREADU" "MATWRITE" "MATWRITEU" "MDPERFORM" "NOCONVERT"
      "NULL" "ON" "OPEN" "OPENSEQ" "OSBREAD" "OSBWRITE" "OSCLOSE" "OSDELETE"
      "OSOPEN" "OSREAD" "OSWRITE" "PAGE" "PCPERFORM" "PERFORM" "PRECISION"
      "PRINT" "PRINTER" "PRINTERCLOSE" "PRINTERR" "PROCREAD" "PROCWRITE"
      "PROGRAM" "PROMPT" "READ" "READBCK" "READBCKL" "READBCKU" "READFWD"
      "READFWDL" "READFWDU" "READL" "READLIST" "READNEXT" "READNEXTTUPLE"
      "READSEQ" "READSELECT" "READT" "READU" "READV" "READVL" "READVU"
      "RECORDLOCKL" "RECORDLOCKU" "RELEASE" "REM" "REMOVE" "REPEAT"
      "RESIZET" "RETURN" "REWIND" "RNDSEED" "ROLLBACK" "RQM" "SELECT"
      "SELECTINDEX" "SEND" "SENDX" "SETINDEX" "SETMARK" "SLEEP" "STOP"
      "SUBROUTINE" "SWAP" "TIMEOUT" "UDTEXECUTE" "UNLOCK" "WEOF" "WEOFSEQ"
      "WRITE" "WRITELIST" "WRITESEQ" "WRITESEQF" "WRITET" "WRITEU" "WRITEV"
      "WRITEVU")
    "The list of unibasic commands. These are used for font-locking and for
auto-capitalization. If you add commands to this list you must re-make
`unibasic-command-regexp' using the code in the comments."))

(defvar unibasic-command-regexp
  (eval-when-compile
    (concat "\\(^\\|[; \t]\\)\\("
            (make-regexp unibasic-command-words)
            "\\)\\([; \t]\\|$\\)")))

(eval-and-compile
  (defvar unibasic-keyword-words
    '( "$F" "$FALSE" "$T" "$TRUE" "ALL" "APPEND" "AT" "BEFORE" "BY" "CALLING"
       "CAPTURING" "CASE" "DO" "ELSE" "ERROR" "FLUSH" "FROM" "GO" "IN"
       "INCLUDE" "KEY" "LENGTH" "LINEMARK" "LIT" "LITERALLY" "LOCKED" "MAT"
       "NEXT" "ON" "OFF" "PASSCOM" "PASSCOMMON" "PASSLIST" "READONLY"
       "RETURNING" "RTNLIST" "SETTING" "STEP" "THEN" "TO" "UNIT" "UNTIL"
       "USING" "WAITING" "WHILE" "WITH" "WORK")))

(defvar unibasic-keyword-regexp
  (eval-when-compile
    (concat "\\(^\\|[; \t]\\)\\(" 
            (make-regexp unibasic-keyword-words)
            "\\)\\([; \t]\\|$\\)")))

(eval-and-compile
  (defvar unibasic-operator-words
    '( "EQ" "NE" "LE" "LT" "GE" "GT" "AND" "OR" "MATCH" "MATCHES" "CAT")))

(defvar unibasic-operator-regexp
  (eval-when-compile
    (concat "\\b\\(" (make-regexp unibasic-operator-words) "\\)\\b")))

(eval-and-compile
  (defvar unibasic-function-words
    '( "ABS" "ACOS" "ALPHA" "ASCII" "ASIN" "ATAN" "BITAND" "BITOR"
       "BITXOR" "BITNOT" "CALCULATE" "CATS" "CHANGE" "CHAR" "CHECKSUM" "COL1"
       "COL2" "CONVERT" "COS" "COUNT" "COUNTS" "DATE" "DCOUNT" "DELETE" "DIR"
       "DOWNCASE" "DROUND" "DQUOTE" "DTX" "EBDIC" "EQS" "EXP" "EXTRACT"
       "FIELD" "FIELDS" "FIELDSTORE" "FMT" "FMTS" "GES" "GETENV" "GETMSG"
       "GETPU" "GETUSERNAME" "GETUSERGROUP" "GETPTR" "GROUP" "GTS" "HASH"
       "ICONV" "IN" "INDEX" "INDEXS" "INDICES" "INMAT" "INSERT" "INT" "ITYPE"
       "LEN" "LENS" "LES" "LN" "LOCATE" "LOWER" "LTS" "MATCHFIELD" "MAXIMUM"
       "MINIMUM" "MOD" "NEG" "NES" "NOT" "NOTS" "NUM" "NUMS" "OCONV" "OCONVS"
       "PWR" "QUOTE" "RAISE" "REM" "REMOVE" "REPLACE" "RETURN" "REUSE" "RND"
       "SADD" "SCMP" "SDIV" "SELECTINFO" "SEQ" "SEQS" "SIN" "SMUL" "SPACE"
       "SPACES" "SPLICE" "SPLICES" "SQRT" "SQUOTE" "SSUB" "STATUS" "STR"
       "STRS" "SUBSTRINGS" "SUM" "SYSTEM" "TAN" "TIME" "TIMEDATE" "TRIM"
       "TRIMB" "TRIMF" "TRIMS" "UNASSIGNED" "UPCASE" "XLATE" "XTD")))

(defvar unibasic-function-regexp
  (eval-when-compile
    (concat "\\b\\(" (make-regexp unibasic-function-words) "\\)\\b[ \t]*(")))

(eval-and-compile
  (defvar unibasic-special-words
    '( "@AM" "@VM" "@SM" "@RM" "@TM" "@SVM" "@ACCOUNT" "@COMMAND"
       "@CONV" "@CRTHIGH" "@CRTWIDE" "@DATA" "@DATE"
       "@DAY" "@DICT" "@FORMAT" "@GID" "@HEADER" "@ID" "@LEVEL" "@LOGNAME"
       "@LPTRHIGH" "@LPTRWIDE" "@MONTH" "@PARASENTENCE" "@PATH" "@RECORD"
       "@RECUR0" "@RECUR1" "@RECUR2" "@RECUR3" "@RECUR4" "@SENTENCE"
       "@SYS.BELL" "@SYSTEM.RETURN.CODE" "@TIME" "@TTY" "@TUPLE" "@UID" 
       "@UDTNO" "@USER0" "@USER1" "@USER2" "@USER3" "@USER4" "@USERNO" 
       "@USER.RETURN.CODE" "@USER.TYPE" "@WHO" "@YEAR" )
    "A list of special variables defined in unibasic."))

(defvar unibasic-special-regexp
  (eval-when-compile
    (make-regexp unibasic-special-words)))

(defvar unibasic-font-lock-syntactic-keywords
  (if (not unibasic-font-lock-syntactify)
      nil
    (list
     '("\\(^[ \t]*\\|;[ \t]*\\)\\(\\*\\)" 2 (11 . nil)) ))
  "*Regular expression to define comment syntax for `*' when used as comment
character and not as an operator.")

(defvar unibasic-font-lock-keywords
  (let ((clist
         (if (not unibasic-font-lock-syntactify)
             (list (cons "\\(^\\|;\\)[ \t]*\\(\\*\\|!.*\\)$" 
                         '(2 font-lock-comment-face)))))
        (mlist
         (list
          (cons unibasic-label-regexp    'font-lock-reference-face)
          (cons unibasic-special-regexp  'font-lock-variable-name-face)
          (cons unibasic-function-regexp '(1 font-lock-type-face))
          (cons unibasic-keyword-regexp  '(2 font-lock-function-name-face))
          (cons unibasic-command-regexp  '(2 font-lock-function-name-face))
          (cons unibasic-operator-regexp 'font-lock-keyword-face)
          (cons "@([^)]*)"               'font-lock-variable-name-face))))
    (append clist mlist))
  "*Unibasic mode regular expressions for font-lock-mode.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; In the unlikely event of an error occurring ... :o)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst unibasic-mode-help-address "unibasic@zsplat.freeserve.co.uk"
  "Address accepting submission of unibasic-mode bug reports.")

(defun unibasic-submit-bug-report ()
  "Submit bug report via mail for unibasic-mode to the address specified in
`unibasic-mode-help-address'. Bound to \\[unibasic-submit-bug-report]."
  (interactive)
  (require 'reporter)
  (and
   (y-or-n-p "Do you want to submit a report about unibasic-mode? ")
   (reporter-submit-bug-report
    unibasic-mode-help-address
    (concat "unibasic-mode " unibasic-rcs-version)
    (list
     ;; report only the vars that affect indentation
     'unibasic-initial-indent
     'unibasic-default-indent
     'unibasic-case-indent
     'unibasic-continuation-indent
     'unibasic-comment-column
     'unibasic-tab-always-indent
     'unibasic-capitalize-keywords
     'unibasic-no-blank-lines
     ))))

(provide 'unibasic-mode)

;; Local variables:
;;   mode: emacs-lisp
;;   auto-save-interval: 1000
;;   indent-tabs-mode: nil
;; End:
