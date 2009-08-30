;;; axcess.el --- Axcess and Netlinx editing support

;; axcess.el is free software
;; Author: Jeff Bowden <jlb@houseofdistraction.com>
;; License: GPL

;;; 28 November 2001 -- Initial release
;;
;; Needs emacs21 to get nested comments correctly fontlocked.
;;
;; Does not have enough smarts to really know when a statement is carried
;; over from one line to the next.  Assumes that lines beginning with FOR,
;; IF, ELSE, WHILE, LONG_WHILE, MEDIUM_WHILE, ACTIVE, or SELECT are
;; incomplete statements and therefor indents the next line if it doesn't
;; start with a brace.  Obviously this won't work too well if, for example,
;; you have a statement such as
;;
;;    IF (foo=1) SEND_STRING "'blah'"
;;
;; so don't do that.  What can say? This was a weekend hack.  I will get
;; around to it eventually.
;;
;; Bits and pieces of this file were borrowed from other progmodes that
;; were in the 20.7 distribution.

;;; USAGE (i.e. a .emacs fragment)
;;
;;(require 'axcess)
;;(setq auto-mode-alist (cons '("\\.axs$" . axcess-mode) auto-mode-alist))
;;(setq auto-mode-alist (cons '("\\.axi$" . axcess-mode) auto-mode-alist))
;;

(defgroup axcess nil
  "Major mode for editing Axcess code."
  :prefix "axs-"
  :group 'languages)

(defvar axs-mode-syntax-table nil
  "Syntax table in use in Axcess buffers.")

(if (null axs-mode-syntax-table)
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\\ "\\" table)
    (modify-syntax-entry ?\( ". 1n" table)
    (modify-syntax-entry ?\) ". 4n" table)
    (modify-syntax-entry ?* ". 23n" table)
    (modify-syntax-entry ?/ ". 12b" table)
    (modify-syntax-entry ?\n "> b" table)
    (modify-syntax-entry ?+ "." table)
    (modify-syntax-entry ?- "." table)
    (modify-syntax-entry ?= "." table)
    (modify-syntax-entry ?% "." table)
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?> "." table)
    (modify-syntax-entry ?& "." table)
    (modify-syntax-entry ?| "." table)
    (modify-syntax-entry ?^ "." table)
    (modify-syntax-entry ?! "." table)
    (modify-syntax-entry ?~ "." table)
    (modify-syntax-entry ?\" "." table)
    (modify-syntax-entry ?\' "\"" table)
    (setq axs-mode-syntax-table table)))

(defvar axs-mode-map nil
  "Keymap used in Axcess mode.")

(if (null axs-mode-map)
  (let ((map (make-sparse-keymap)))
    (define-key map "}" 'axs-electric-brace)
    (define-key map "{" 'axs-electric-brace)
    (setq axs-mode-map map)))

(defcustom axs-indent 4
  "*This variable gives the indentation in Axcess-Mode."
  :type 'integer
  :group 'axcess)
  
;;;###autoload
(defun axcess-mode ()
  "This is a mode intended to support program development in Axcess."
  (interactive)
  (kill-all-local-variables)
  (use-local-map axs-mode-map)
  (setq major-mode 'axcess-mode)
  (setq mode-name "Axcess")
  (set-syntax-table axs-mode-syntax-table)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'axcess-indent-line)
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)
  (make-local-variable 'comment-start)
  (setq comment-start "(* ")
  (make-local-variable 'comment-end)
  (setq comment-end " *)")
  (make-local-variable 'comment-column)
  (setq comment-column 40)
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "(\\*+ *")
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
	'((axcess-font-lock-keywords)
	  nil t ((?_ . "w") (?. . "w") (?< . ". 1") (?> . ". 4")) nil
	  ))
  (run-hooks 'axs-mode-hook))

(defconst axcess-font-lock-keywords
  `(
    ;;
    ;; sections
    (, (concat "\\<" (regexp-opt '(
                                   "DEFINE_COMBINE"
                                   "DEFINE_CONNECT_LEVEL"
                                   "DEFINE_DEVICE"
                                   "DEFINE_CONSTANT"
                                   "DEFINE_VARIABLE"
                                   "DEFINE_START"
                                   "DEFINE_PROGRAM"
                                   "DEFINE_LATCHING"
                                   "DEFINE_MUTUALLY_EXCLUSIVE"
                                   "DEFINE_TOGGLING"
                                   "LOCAL_VAR"
                                   ) t) "\\>" )
     (1 font-lock-keyword-face))
    ;;
    ;; preprocessor
    (, (concat "\\<" (regexp-opt '(
                                   "INCLUDE"
                                   "#DEFINE"
                                   "#END_IF"
                                   "#IF_DEFINED"
                                   "#IF_NOT_DEFINED"
                                   "#WARN"
                                   "__DATE__"
                                   "__FILE__"
                                   "__LINE__"
                                   "__NAME__"
                                   "__TIME__"
                                   "__VERSION__"

                                 ) t) "\\>" )
     (1 font-lock-keyword-face))

    ;;
    ;; command-style functions
    (, (concat "\\<" (regexp-opt '(
                                   "CALL"
                                   "CREATE_BUFFER"
                                   "CREATE_LEVEL"
                                   "CREATE_MULTI_BUFFER"
                                   "DEFINE_CALL"
                                   "SEND_COMMAND"
                                   "SEND_LEVEL"
                                   "SEND_STRING"
                                   "SYSTEM_CALL"

                                 ) t) "\\>" )
     (1 font-lock-keyword-face))

    ;;
    ;; input/output & events
    (, (concat "\\<" (regexp-opt '(
                                   "PUSH"
                                   "RELEASE"
                                   "WAIT"
                                   "WAIT_UNTIL"
                                   "CANCEL_ALL_WAIT"
                                   "CANCEL_ALL_WAIT_UNTIL"
                                   "CANCEL_WAIT"
                                   "CANCEL_WAIT_UNTIL"
                                   "MIN_TO"
                                   "OFF"
                                   "ON"
                                   "PAUSE_ALL_WAIT"
                                   "PAUSE_WAIT"
                                   "PULSE"
                                   "RESTART_ALL_WAIT"
                                   "RESTART_WAIT"
                                   "TO"
                                   "TOTAL_OFF"

                                 ) t) "\\>" )
     (1 font-lock-keyword-face))

    ;;
    ;; flow control
    (, (concat "\\<" (regexp-opt '(
                                   "ACTIVE"
                                   "ELSE"
                                   "IF"
                                   "LONG_WHILE"
                                   "MEDIUM_WHILE"
                                   "SELECT"
                                   "WHILE"
                                 ) t) "\\>" )
     (1 font-lock-keyword-face))

    ;;
    ;; types
    (, (concat "\\<" (regexp-opt '(
                                   "INTEGER"
                                 ) t) "\\>" )
     (1 font-lock-type-face))

    ;;
    ;; operators
    (, (concat "\\<" (regexp-opt '(
                                   "AND"
                                   "BAND"
                                   "BNOT"
                                   "BOR"
                                   "BXOR"
                                   "OR"
                                   "XOR"
                                   "NOT"

                                 ) t) "\\>" )
     (1 font-lock-keyword-face))

    ;;
    ;; builtin variables
    (, (concat "\\<" (regexp-opt '(
                                   "DATE"
                                   "DAY"
                                   "PUSH_CHANNEL"
                                   "PUSH_DEVICE"
                                   "RELEASE_CHANNEL"
                                   "RELEASE_DEVICE"
                                   "TIME"
                                   "MASTER_SLOT"
                                   "PROGRAM_NAME"

                                 ) t) "\\>" )
     (1 font-lock-builtin-face))
    ;;
    ;; builtin functions
    (, (concat "\\<" (regexp-opt '(
                                   "ATOI"
                                   "CLEAR_BUFFER"
                                   "DEVICE_ID"
                                   "DO_PUSH"
                                   "DO_RELEASE"
                                   "EXTERNAL_CONTROL"
                                   "FIND_STRING"
                                   "GET_BUFFER_CHAR"
                                   "GET_MULTI_BUFFER_STRING"
                                   "GET_PULSE_TIME"
                                   "GET_TIMER"
                                   "ITOA"
                                   "ITOHEX"
                                   "LEFT_STRING"
                                   "LENGTH_STRING"
                                   "LOWER_STRING"
                                   "MID_STRING"
                                   "REDIRECT_STRING"
                                   "REMOVE_STRING"
                                   "RIGHT_STRING"
                                   "SET_LENGTH_STRING"
                                   "SET_PULSE_TIME"
                                   "SET_TIMER"
                                   "UPPER_STRING"

                                 ) t) "\\>" )
     (1 font-lock-builtin-face))

    ;;
    ;; misc
    (, (concat "\\<" (regexp-opt '(
                                   "RANDOM_NUMBER"
                                   "SYSTEM_CALL_NUM"

                                 ) t) "\\>" )
     (1 font-lock-keyword-face))

    ;;
    ;; variables (disabled -- too busy)
;    ("\\<\\([a-z_][a-z0-9_]*\\)\\>"
;     (1 font-lock-variable-name-face))

    )
  "Highlighting for Axcess mode.")

(defun axcess-indent-line ()
  "Indent current line as Axcess code.
Return the amount the indentation changed by."
  (let ((indent (calculate-axcess-indent nil))
	beg
        shift-amt
	(case-fold-search nil)
	(pos (- (point-max) (point))))
    (beginning-of-line)
    (setq beg (point))
    (skip-chars-forward " \t")
    (setq shift-amt (- indent (current-column)))
    (if (zerop shift-amt)
	(if (> (- (point-max) pos) (point))
	    (goto-char (- (point-max) pos)))
      (delete-region beg (point))
      (indent-to indent)
      ;; If initial point was within line's indentation,
      ;; position after the indentation.  Else stay at same point in text.
      (if (> (- (point-max) pos) (point))
	  (goto-char (- (point-max) pos))))
    shift-amt))

(defun calculate-axcess-indent (&optional parse-start)
  "Return appropriate indentation for current line as Axcess code."
  (let ((unfinished (unfinished-statement-level)))
    (if unfinished
        (+ unfinished axs-indent)
      (let ((level
             (save-excursion (beginning-of-line)
                             (let ((parse-level (nth 0 (parse-partial-sexp (point-min) (point)))))
                               (if (looking-at "[ \t]*}")
                                   (- parse-level 1)
                                 parse-level)))))
        (if level
            (* axs-indent level)
          0)))))

(defun unfinished-statement-level ()
  (save-excursion
    (beginning-of-line)
    (if (looking-at "[ \t]*{")
        nil
      (progn
        (skip-syntax-backward "-<>")
        (beginning-of-line)
        (if (looking-at-unfinished)
            (current-indentation)
          nil)))))

;(defconst unfinished-statement-regexp
;  (concat "[ \t]*\\<" (regexp-opt (list "IF" "ELSE" "WAIT" "PUSH" "RELEASE" "WHILE" "LONG_WHILE" "MEDIUM_WHILE" "ACTIVE" "SELECT") t) "\\>"))

;; todo: make this sensitive to whether these statements are *actually* completed
(defconst unfinished-statement-regexp
  (concat "[ \t]*\\<" (regexp-opt (list "FOR" "IF" "ELSE" "WHILE" "LONG_WHILE" "MEDIUM_WHILE" "ACTIVE" "SELECT") t) "\\>"))

(defun looking-at-unfinished ()
  (looking-at unfinished-statement-regexp))
    
(defun axs-electric-brace (arg)
  "Insert a brace."
  (interactive "*P")
  (self-insert-command (prefix-numeric-value arg))
  (indent-for-tab-command))

(provide 'axcess)

;;; axcess.el ends here
