;;; This is mon-doc-help-utils.el
;;; ================================================================
;;; DESCRIPTION:
;;; mon-doc-help-utils consolidates functions that offer documentation and or
;;; enable generation of documentation. Generally these are for assistance
;;; generating Elisp as opposed to programmatic use in Elisp functions and
;;; programs. 
;;;
;;; Additional utilities are provided to help with modification of Aaron
;;; Hawley's Reference card page on the EmacsWiki. SEE:
;;; (URL `http://www.emacswiki.org/emacs/Reference_Sheet_by_Aaron_Hawley')
;;; (URL `http://www.emacswiki.org/emacs/Reference_Sheet_by_Aaron_Hawley_source')
;;;
;;; MOTIVATIONS: from /etc/TODO:
;;; ,----
;;; | ** Have a command suggestion help system that recognizes patterns
;;; |    of commands which could be replaced with a simpler common command.
;;; |    It should not make more than one suggestion per 10 minutes.
;;; `----
;;;
;;; FUNCTIONS:►►►
;;; `mon-help-reference-sheet', `mon-help-regexp-syntax',
;;; `mon-help-search-functions', `mon-help-info-incantation',
;;; `mon-help-tar-incantation', `mon-help-rename-incantation',
;;; `mon-help-install-info-incantation', `mon-help-format-width',
;;; `mon-help-file-dir-functions', `mon-help-process-functions',
;;; `mon-help-w32-env', `mon-help-insert-documentation', `mon-help-function-args',
;;; `mon-insert-doc-help-tail', `mon-insert-doc-help-cookie',
;;; `mon-help-function-spit-doc', `mon-tags-apropos', `mon-tags-naf-apropos'
;;; `mon-index-elisp-symbol', `mon-help-buffer-functions'
;;; `mon-help-basic-faces', `mon-help-color-functions',
;;; `mon-help-package-keywords', `mon-help-faces-themes'
;;; `mon-help-ISO-8859-1-chars', `mon-help-ASCII-chars', 
;;; `mon-help-cntl->hex->ecma-35', `mon-help-cntl->hex->ecma-48',
;;; `mon-help-text-property-stickyness', `mon-help-xml-functions'
;;; `mon-help-plist-functions', `mon-help-text-property-functions'
;;; `mon-help-emacs-introspect', `mon-help-make-faces'
;;; `mon-help-xref-symbol-value'
;;; FUNCTIONS:◄◄◄
;;;
;;; MACROS:
;;; `mon-help-swap-var-doc-const-val'
;;;
;;; CONSTANTS: or VARIABLES:
;;; `*reference-sheet-help-A-HAWLEY*', `*w32-env-variables-alist*',
;;; `*doc-cookie*', `*regexp-symbol-defs*', `*mon-help-interactive-spec-alist*'
;;;
;;; ALIASED/ADVISED/SUBST'D
;;; `mon-insert-documentation' -> `mon-help-insert-documentation' 
;;;
;;; RENAMED:
;;; `*emacs-reference-sheet-A-HAWLEY*' -> `*reference-sheet-help-A-HAWLEY*'
;;;
;;; MOVED:
;;; `mon-help-CL-time', `mon-help-CL-loop', `mon-help-slime-keys' -> mon-doc-help-CL.el
;;; `mon-index-elisp-symbol' <- mon-utils.el
;;;
;;; Following moved from `mon-insertion-utils.el' and renamed *insert* -> *help*
;;; `mon-insert-file-dir-functions'  -> `mon-help-file-dir-functions'
;;; `mon-insert-install-info-incantation' -> `mon-help-install-info-incantation'
;;; `mon-insert-rename-incantation' -> `mon-help-rename-incantation'
;;; `mon-insert-tar-incantation' -> `mon-help-tar-incantation'
;;; `mon-insert-info-incantation' -> `mon-help-info-incantation'
;;; `mon-insert-diacritics' -> `mon-help-diacritics'
;;;
;;; REQUIRES:
;;; cl.el `intersection' in `mon-help-function-spit-doc'
;;; regexpl.el available: (URL `http://www.emacswiki.org/emacs/regexpl.el')
;;;
;;; Following are provided in external packages and also inlined herein:
;;; `mon-insert-lisp-testme' ;-> mon-insertion-utils.el
;;; `*regexp-symbol-defs*' ;-> mon-regexp-symbols.el
;;; `mon-test->*regexp-symbol-defs*' ;-> mon-regexp-symbols.el
;;;
;;; NOTES:
;;; I think I remember lifting the `emacs-wiki-escape-lisp-string-region'
;;; and  from Pascal Bourguignon but I can no longer find the reference
;;; to the source... I have it elsewhere as `escape-lisp-string-region'.
;;;
;;; NOTE:
;;; mon-doc-help-utils.el USED_TO_BE reference-sheet-help-utils.el @
;;; WAS: (URL `http://www.emacswiki.org/emacs/reference-sheet-help-utils.el')
;;; IS: (URL `http://www.emacswiki.org/emacs/mon-doc-help-utils.el')
;;; The renaming is a breaking change change in both file-name and containing
;;; function/var prefix from ``reference-sheet-'' -> ``mon-doc-''
;;; I apologize to anyone adversely affected by this change. It is necessary
;;; in order to support/integrate with other packages I intend for distribution
;;; on the EmacsWiki which all have the `mon-' prefix.
;;; The webpage describing this package remains:
;;; (URL `http://www.emacswiki.org/emacs/ReferenceSheetHelpUtils')
;;;
;;; TODO:
;;;
;;; KEYBINDINGS:
;;; Assigning a keybinding for `mod-doc-help-insert-cookie'
;;; and make inserting the `*doc-cookie*' variable easier esp. when
;;; the doc-cookie is not default and/or you have trouble remembering
;;; the ucs char-code for inserting '►►►' \(ucs-insert \"25BA\"\) ;=> ►
;;;
;;; Assigning a keybinding for `mon-insert-lisp-testme'
;;; will make insertion of ';;;test-me;(.*)' strings at end of function defuns
;;; easier. This utility trys to DWIM.
;;;
;;; Alternatively, assigning a keybinding for `mon-insert-doc-help-tail'
;;; will give most of the functionality of `mon-insert-lisp-testme'
;;; by helping in generation of introspecting function bodies which utilize the
;;; tools included herein.
;;;
;;; SNIPPETS:
;;; (while (search-forward-regexp "\\(mon-help-function-spit-doc '\\(.*\\) nil nil t\\)")
;;;   (replace-match "mon-help-function-spit-doc '\\2 :insertp t"))
;;;
;;; THIRD PARTY CODE:
;;; Portions of code herein from:
;;; Aaron Hawley, his:
;;; (URL `http://www.emacswiki.org/emacs/Reference_Sheet_by_Aaron_Hawley_source')
;;;
;;; AUTHOR: MON KEY
;;; MAINTAINER: MON KEY
;;;
;;; PUBLIC-LINK: (URL `http://www.emacswiki.org/emacs/mon-doc-help-utils.el')
;;; FILE-PUBLISHED <Timestamp: #{2009-08-15} - by MON KEY>
;;; 
;;; FILE-CREATED:
;;; <Timestamp: Wednesday June 17, 2009 @ 11:29.15 AM - by MON KEY>
;;; ================================================================
;;; Copyright (C) 2009 - MON KEY
;;; ==============================
;;; CODE:

;;; ==============================
;;; `defun*' `intersection' in `mon-help-function-spit-doc'
(eval-when-compile (require 'cl))
;; USED-BY: `emacs-wiki-fy-reference-sheet'
(require 'regexpl)
(require 'mon-regexp-symbols) ;-> `*regexp-symbol-defs*'
(require 'mon-insertion-utils) ;->`mon-insert-lisp-testme'
;;; ==============================

;;; ==============================
;;; CREATED: <Timestamp: #{2009-08-21T19:00:14-04:00Z}#{09345} - by MON KEY>
(defun mon-tags-naf-apropos ()
  "Search tags-tables in for occurences of regexp \"*naf-\" with `tags-apropos'.
See also; `mon-tags-apropos', `*mon-tags-table-list*', 
`mon-update-tags-tables'.►►►"
  (interactive)
  (tags-apropos "*naf-"))

;;; ==============================
;;; CREATED: <Timestamp: #{2009-08-21T18:57:08-04:00Z}#{09345} - by MON KEY>
(defun mon-tags-apropos ()
  "Search tags-tables in for occurences of regexp \"*mon-\" with `tags-apropos'.
See also; `mon-tags-naf-apropos', `*mon-tags-table-list*',
`mon-update-tags-tables'. ►►►"
  (interactive)
  (tags-apropos "*mon-"))

;;; ==============================
;;; CREATED: <Timestamp: Friday July 03, 2009 @ 01:11.47 PM - by MON KEY>
(defvar *doc-cookie* nil
  "*Default 'documentation cookie' \"►►►\".
Cookie is used with `mon-help-function-spit-doc'. to delimit which portion of
 docstring should be commented out when inserting into buffer.
\(ucs-insert \"25BA\"\) ;=> ►\n
See also `mon-insert-doc-help-cookie'.►►►")
;;
(when (not (bound-and-true-p *doc-cookie*))
  (setq *doc-cookie* "►►►"))
;;;
(defun mon-insert-doc-help-cookie ()
  "Insert default 'documentation cookie' at point.
Everything up to `*doc-cookie*' is commented out when inserted into a buffer.
Default cookie is '►►►' \(ucs-insert \"25BA\"\) ;=> ►
Set value of *doc-cookie* variable otherwise if this is not acceptable.
Used-by: `mon-help-function-spit-doc' to find comment-begin of snarfed doc 
strings. ►►►"
  (interactive)
  (insert "►►►"))

;;; ==============================
;;; CREATED: <Timestamp: #{2009-09-14T14:16:34-04:00Z}#{09381} - by MON KEY>
(defmacro mon-help-swap-var-doc-const-val (var-name const-name xrefs &optional face-name)
  "Swap the value variable-documentation property of VAR-NAME into the
documentation-property of CONST-NAME's variable-documentation. After, put the
symbol value of CONST-NAME at the head of VAR-NAME's variable-documentation
property.
XREFS is a symbol holding a list of symbol names which cross-reference one another.
FACE-NAME is variable pointing bound to the symbol holding a face definintion.
e.g. the variable `naf-mode-institution-fface' is bound to the face 
`naf-mode-institution-face'. We do this because face documentation isn't accessible as in 
*Help* buffers i.e. using \[`describe-variable']. 
This procedure is implemented to extending *Help* documentation of `naf-mode'
constants, variables, and faces but could be used to extend any derived mode generating its
font-lock keywords from lists bound variables.\n
EXAMPLE:
\(mon-help-swap-var-doc-const-val
    *naf-school-names-english* naf-mode-school-names-english
    ;;^ VAR-NAME ^             ^ CONST-NAME ^
    *naf-mode-institution-xrefs* naf-mode-institution-fface)
    ;;^ XREV ^                   ^ FACE-NAME ^\n
NOTE: When compiling defvar and defconst forms mut be made known at compile time.
IOW wrap them _and_ the macro call in an `eval-and-compile'. ►►►"
  (declare (indent 1) (debug t))
  (let ((v-doc (make-symbol "v-doc"))
	(c-val (make-symbol "c-val"))
        (x-ref (make-symbol "x-ref"))
        (m-val (make-symbol "m-val"))
        (f-nam (make-symbol "f-nam")))
    `(let ((,v-doc ,(plist-get (symbol-plist var-name) 'variable-documentation))
	   (,c-val ,(symbol-value const-name))
           (,x-ref (remove ',var-name ,xrefs))
           (,f-nam (when ,face-name 
                     (concat "\n"
			     "FONT-LOCKING-FACE:            `" ,(symbol-name face-name) "'\n"
			     "                              (describe-face '"
			     (replace-regexp-in-string "fface" "face" ,(symbol-name face-name)) ")\n")))
           (,m-val))
       (setq ,m-val (concat ,v-doc 
                            "\n\nLIST-VALUE: held by VARIABLE: `" ,(symbol-name var-name) "'\n\n"
                            "REGEXPS: held by CONSTANT:    `" ,(symbol-name const-name) "'\n"
                            ,f-nam "\n"))
       (setq ,x-ref (concat "\nSee also;\n`" (mapconcat 'symbol-name ,x-ref "'\n`") "'\n►►►"))
       (setq ,v-doc (concat ,m-val
                            "--------------------------\n"
                            "REGEXP-VALUE: of CONSTANT: " ,(symbol-name const-name) " is:\n\n"
                            ,c-val
                            "\n\n--------"
                            ,x-ref))
       (plist-put (symbol-plist ',var-name) 'variable-documentation ,v-doc)
       (setq ,c-val ,(format "%S" (symbol-value var-name)))
       (setq ,v-doc (concat ,m-val
                            "------------------------\n"
                            "LIST-VALUE: of VARIABLE: " ,(symbol-name var-name) " is:\n\n"
                            ,c-val
                            "\n\n--------"
			    ,x-ref))
       (plist-put   (symbol-plist ',const-name) 'variable-documentation ,v-doc))))
;;
;;;(progn (fmakunbound 'mon-help-swap-var-doc-const-val) (unintern 'mon-help-swap-var-doc-const-val))
;;;
;;;; UNCOMMENT BELOW TO TEST:
;;;; NOTE: When compiling defvar and defconst forms mut be made known at compile time.
;;;; IOW wrap them _and_ the macro call in an `eval-and-compile'."
;;;test-me; 
;;;(defvar *test-swap-var-xrefs* 
;;;  '(*test-swap-var->const* *test-swap-var->const2* *test-swap-var->const3* *test-swap-var-xrefs*)
;;;  "List of symbol names of variables which xref each other in `test-swap-var-mode'.
;;;See FILE: \"./test-swap-var-mode.el\".")
;;;
;;;(defvar *test-swap-var->const* '("list" "of" "keywords" "to" "fontlock")
;;;  "Did this docstring get swapped to docstring of `test-swap-var->cons'?.
;;;   Is its value at the top of docstring?")
;;;
;;;(defconst test-swap-var->const (regexp-opt *test-swap-var->const* 'paren))
;;;
;;;(defvar test-swap-var->const-fface 'test-swap-var->const-face
;;;  "*Face for `naf-mode' font-locking of institution name keywords
;;;KEYWORDS-IN: the regexp defined in: {See also; list of XREFD var names}.
;;;Face definition in `test-swap-var->const-face'.")
;;;
;;;(defface test-swap-var->const-face 
;;;   '((((class color) (background light)) (:foreground "CornflowerBlue"))
;;;     (((class color) (background dark)) (:foreground "CornflowerBlue"))
;;;     (t (:bold t :italic t)))
;;;   "*Face fontlocking of institution name keywords in .naf files.
;;;Additional documentation in var `test-swap-var->const-fface'")
;;;
;;;(mon-help-swap-var-doc-const-val *test-swap-var->const* test-swap-var->const)
;;;
;;;(mon-help-swap-var-doc-const-val
;;;    *test-swap-var->const* test-swap-var->const
;;;    ;;^ VAR-NAME ^         ^ CONST-NAME ^
;;;    *test-swap-var-xrefs*  test-swap-var->const-fface)
;;;    ;;^ XREF ^             ^ FACE-NAME ^
;;;
;;;; CLEANUP:
;;;(progn (makunbound '*test-swap-var->const*)  (unintern '*test-swap-var->const*)
;;;        (makunbound 'test-swap-var->const)  (unintern 'test-swap-var->const)
;;;        (makunbound '*test-swap-var-xrefs*)  (unintern '*test-swap-var-xrefs*)
;;;        (makunbound 'test-swap-var->const-face)  (unintern 'test-swap-var->const-face)
;;;        (makunbound 'test-swap-var->const-fface)  (unintern 'test-swap-var->const-fface))

;;; ==============================
;;; CREATED: <Timestamp: #{2009-09-30T16:44:24-04:00Z}#{09403} - by MON KEY>
(defun mon-help-xref-symbol-value (sym)
  "Return the value of symbol SYM. 
When SYM is a function return `symbol-function' of symbol.
When SYM is a variable return `symbol-value' of symbol.
value returned is of the form:
\(\(SYMBOL <FUNCTION>|<VARIABLE>\) \(VALUE-OF-FUNICTION-OR-VARIABLE\)\n
EXAMPLE:\n(mon-help-xref-symbol-value 'mon-help-xref-symbol-value)\n
\(mon-help-xref-symbol-value '*w32-env-variables-alist*\)
See also; `mon-help-function-spit-doc', `mon-help-function-args',
`mon-help-swap-var-doc-const-val', `mon-help-parse-interactive-spec'
►►►"
  (let* ((is-sym (intern-soft sym))
         (sym-type-val (cond ((and(fboundp is-sym)(functionp is-sym))
                              `((,is-sym ,'<FUNCTION>),(symbol-function is-sym)))
                             ((bound-and-true-p is-sym)
                              `((,is-sym '<VARIABLE>) ,(symbol-value is-sym))))))
    sym-type-val))

;;; ==============================
;;; CREATED: <Timestamp: Thursday July 02, 2009 @ 05:16.20 PM - by MON KEY>
(defun* mon-help-function-spit-doc (sym-name &key alt-cookie do-var insertp do-face do-group do-theme)
  "Return documentation for function with SYM-NAME.
When :ALT-COOKIE \(a string\) is non-nil overrides the default comment delimiter
set in global var `*doc-cookie*' - \"►►►\" . If :ALT-COOKIE is not present in
SYM-NAME's docstring header of docstring is inserted uncommented.
When :INSERTP is non-nil insert documentation in current buffer.
When keyword arg :DO-VAR is non-nil get documentation of a variable or constant.
:DO-VAR should be t when invoked for variable, constant, custom documentation,
e.g. symbols defined inside a defvar, defconst, or defcustom form.
When keyword arg :DO-FACE is non-nil get face documentation for sym-name.
:DO-FACE should be t when invoked for face documentation, e.g. symbols defined
inside a defface form.
When keyword arg :DO-GROUP is non-nil get face documentation for sym-name.
:DO-GROUP should be t when invoked for group documentation, e.g. symbols defined
inside a defgroup form.
When keyword arg :DO-THEME is non-nil get face documentation for sym-name.
:DO-THEME should be t when invoked for group documentation, e.g. symbols defined
inside a defgroup form.
\n
EXAMPLE:
\(mon-help-function-spit-doc 'mon-help-function-spit-doc\) ;defun
\(mon-help-function-spit-doc '*doc-cookie* :do-var t\) ;defvar
\(mon-help-function-spit-doc 'eldoc-message-commands :do-var t\) ;defconst
\(mon-help-function-spit-doc 'completions-merging-modes :do-var t\) ;defcustom
\(mon-help-function-spit-doc 'font-lock-keyword-face :do-face t\) ;defface
\(mon-help-function-spit-doc 'apropos :do-group t\) ;defgroup\n
See also; `mon-insert-doc-help-cookie', `mon-insert-doc-help-tail'
`mon-help-xref-symbol-value', `mon-help-function-args'
`mon-help-insert-documentation'. ►►►"
  (let (mk-docstr)
    (save-excursion
      (setq mk-docstr
            (with-temp-buffer
              (emacs-lisp-mode)
              (let* ((check-opt (if alt-cookie (string-to-list alt-cookie)))
                     (dc (if (and alt-cookie (stringp alt-cookie))
                             (cond ((intersection
                                     (string-to-list "[*?^.+$\\")
                                     check-opt)
                                    (regexp-quote alt-cookie))
                                   (t alt-cookie))
                           *doc-cookie*))
                     (st-mrk (make-marker))
                     (cookie-mrk (make-marker))
                     (put-help)
                     (help-bnds)
                     (ret-str))
                (setq put-help
                      (cond ((functionp sym-name)
                             (or (documentation sym-name)
                                 (when (stringp (caddr (symbol-function 'mon-help-function-spit-doc)))
                                   (stringp (caddr (symbol-function 'mon-help-function-spit-doc)))))) ;funcs, macros
                       (do-var (or (plist-get (symbol-plist sym-name) 'variable-documentation)
                                   (documentation-property sym-name 'variable-documentation))) ;var, const, customs
                       (do-face (or (face-documentation sym-name)
                                    (plist-get (symbol-plist sym-name) 'face-documentation)
                                    (documentation-property sym-name 'face-documentation))) ;; faces
                       (do-group (or (plist-get (symbol-plist sym-name) 'group-documentation)
                                     (documentation-property sym-name 'group-documentation))) ;; groups
                       (do-theme (or (plist-get (symbol-plist sym-name) 'theme-documentation)
                                    (documentation-property sym-name 'theme-documentation))) ;; consider 'theme-settings
                       (t (documentation sym-name))))
                (set-marker st-mrk (point))
                (setq help-bnds (+ (marker-position st-mrk) (length put-help)))
                (princ put-help (current-buffer))
                ;;(prin1 put-help (current-buffer))
                (goto-char st-mrk)
                (search-forward-regexp (concat "\\(" dc "\\)") help-bnds t)
                (set-marker cookie-mrk (point))
                (if (equal st-mrk cookie-mrk)
                    nil
                  (progn
                    (comment-region st-mrk cookie-mrk)
                    (goto-char st-mrk)
                    (search-forward-regexp (concat "\\(" dc "\\)") cookie-mrk t)
                    (replace-match "")))
                (setq ret-str  (buffer-string))
                ret-str))))
    (when insertp (save-excursion (newline) (princ mk-docstr (current-buffer))))
    mk-docstr))

;;;test-me;(mon-help-function-spit-doc 'mon-help-function-spit-doc :alt-cookie nil :do-var nil :insertp t)
;;;test-me;(mon-help-function-spit-doc '*doc-cookie* :do-var t :insertp t)
;;;test-me;(mon-help-function-spit-doc 'font-lock-keyword-face :do-face t :insertp t)
;;;test-me;(mon-help-function-spit-doc 'apropos :do-group t :insertp t)
;;;test-me;(mon-help-function-spit-doc 'completions-merging-modes :do-var t)


;;; ==============================
;;; REQUIRED FEATURES
;;; `comment-divider' -> `mon-insertion-utils.el'
;;; ==============================

(unless (and (featurep 'mon-insertion-uitls) (fboundp 'comment-divider))
;;; ==============================
;;; MODIFICATIONS: <Timestamp: #{2009-08-25T14:09:37-04:00Z}#{09352} - by MON KEY>
(defun comment-divider (&optional not-insert intrp)
  "Insert default comment divider at point.
When called-interactively inserts at point:\n
;;; ==============================\n
When optional arg not-insert is non-nil return comment divider as string.\n
EXAMPLE:\n\(comment-divider t\)\n
See also; `mon-comment-divide->col', `mon-lisp-comment-to-col'
`mon-insert-php-comment-divider', `mon-insert-lisp-stamp'. ►►►"
  (interactive "i\np")
  (if (or (not not-insert) intrp) 
      (insert ";;; ==============================")
    ";;; =============================="))
) ;; close UNLESS
;;
;;;test-me;(comment-divider t)
;;;test-me;(comment-divider)
;;;test-me;(call-interactively 'comment-divider)

;;; ==============================
;;; REQUIRED FEATURES
;;; `*regexp-symbol-defs*' -> `mon-regexp-symbols.el'
;;; `mon-insert-lisp-testme' -> `mon-insertion-utils.el'

;;; ==============================
(unless (and (featurep 'mon-regexp-symbols)
             (bound-and-true-p *regexp-symbol-defs*))
;;; ==============================
;;; NOTE: Currently not checking for:
;;; defalias, defvaralias, defadvice or CL's: defstruct
;;; CREATED: <Timestamp: 2009-08-03-W32-1T11:04:11-0400Z - by MON KEY>
(defvar *regexp-symbol-defs* nil
  "*Regexp for finding lisp definition forms defun, defmacro, defvar.
CALLED-BY: `mon-insert-lisp-testme',`mon-insert-doc-help-tail',
`mon-test->*regexp-symbol-defs*'. ►►►")
;;
(when (not (bound-and-true-p *regexp-symbol-defs*))
  (setq *regexp-symbol-defs*
        (concat
         ;;FIXME: doesn't catch on cases where the lambda list is on the next line.
         ;;...1..
         "^\\((" ;;opening paren
         ;;grp 2 -> `defun' `defun*' `defmacro' `defmacro*' `defsubst' `defsubst*'
         ;;`defvar' `defconst' `defcustom' `deftheme'
         ;;..2................................................
         "\\(def\\(?:c\\(?:onst\\|ustom\\)\\|face\\|macro\\*?\\|subst\\*?\\|theme\\|un\\*?\\|var\\)\\)"
         ;;. .3....................     ;; note: leading whitepspace
         " \\([A-Za-z0-9/><:*-]+\\)"      ;; grp 3 -> *some/-symbol:->name<-2*
         ;;...4........................
         "\\(\\( (\\)\\|\\( '\\)\\|\\( `\\)\\)\\)" ;;grp 4 -> ` (' or ` ''
         )))
;;
;;;test-me; *regexp-symbol-defs*
;;; SEE-BELOW-FOR-ADDITIONAL-TESTS-WITH: `mon-test->*regexp-symbol-defs*'
;;
;;;(progn (makunbound '*regexp-symbol-defs*) (unintern '*regexp-symbol-defs*))
;;
;;;;;;;;;;;;CURRENT-REGEXP;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  ....1..2.................................................3.......................4........................
;;;  "\\(def\\(?:c\\(?:onst\\|ustom\\)\\|face\\|macro\\*?\\|subst\\*?\\|theme\\|un\\*?\\|var\\)\\)"
;;
;;;(concat "^\\(("
;;; (regexp-opt '("defun" "defun*" "defmacro" "defmacro*" "defsubst" "defsubst*"
;;;  "defconst" "defvar"  "defcustom" "defface" "deftheme" ) t)
;;;(regexp-opt-depth "\\(def\\(?:c\\(?:onst\\|ustom\\)\\|face\\|macro\\*?\\|subst\\*?\\|theme\\|un\\*?\\|var\\)\\)")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ==============================
;;; CREATED: <Timestamp: #{2009-09-02T16:11:07-04:00Z}#{09363} - by MON KEY>
(unless (fboundp 'mon-test->*regexp-symbol-defs*)
(defun mon-test->*regexp-symbol-defs* (&optional insertp intrp)
  "Test-function to ensure that the regexp in var `*regexp-symbol-defs*' works.
See also; `mon-insert-lisp-testme',`mon-insert-doc-help-tail'. ►►►"
  (interactive "i\np")
  (let ((find-def* *regexp-symbol-defs*)
        (the-str))
    (save-excursion  (search-backward-regexp find-def*))
    (setq the-str
          (concat "\nmatch-string1: "(match-string-no-properties 2) " start2: "
                  (number-to-string (match-beginning 2)) " end2: "
                  (number-to-string (match-end 2)) "\nmatch-string3: "
                  (match-string-no-properties 3)" start3: "
                  (number-to-string (match-beginning 3))  " end3: "
                  (number-to-string (match-end 3))"\nmatch-string4: "
                  (match-string-no-properties 4) " start4: "
                  (number-to-string (match-beginning 4))  " end4: "
                  (number-to-string (match-end 4))))
    (if (or insertp intrp)
        (save-excursion (princ  the-str (current-buffer)))
      (momentary-string-display the-str (point))))))
) ;CLOSE UNLESS BATP

;;;test-me; *regexp-symbol-defs*
;;(progn (makunbound '*regexp-symbol-defs*) (unintern '*regexp-symbol-defs*))

;;;; UNCOMMENT TO TEST:
;;;(defun some-function (&optional optional)
;;;(defun some-function-22 (&optional optional)
;;;(defun *some/-symbol:->name<-2* (somevar
;;;(defmacro some-macro ()
;;;(defmacro some-macro*:22 (&rest)
;;;(defun *some/-symbol:->name<-2* (somevar
;;;(defvar *some-var* 'var
;;;(defun *some/-symbol:->name<-2* 'somevar
;;;(defmacro some-macro*:22 (&rest)
;;;(defun *some/-symbol:->name<-2* (somevar
;;;(defvar *some-var* 'var
;;;(defun *some/-symbol:->name<-2* 'somevar
;;;(defmacro* some-macro*:22 (&rest)
;;;(defun* *some/-symbol:->name<-2* (somevar
;;;(defsubst *some/subtst-symbol:->name<-2* (
;;;(defsubst* *some/subtst-symbol:->name<-2* (
;;;(defconst *some/-symbol:->name<-2* (somevar
;;;(defcustom *some/-custom-symbol:->name<-2* 'somecustom
;;;(defconst *some/-symbol:->name<-2* (someconst
;;;(defface *some/-face-symbol:->name<-2* (someface
;;;(defgroup *some/-group-symbol:->name<-2* (somegroup
;;;(deftheme *some/-theme-symbol:->name<-2* (sometheme
;;;
;;;test-me;(mon-test->*regexp-symbol-defs*)

;;; ==============================

;;; ==============================
;;; REQUIRED FEATURES
;;; `mon-insert-lisp-testme'

(unless (and (featurep 'mon-insertion-utils) (fboundp 'mon-insert-lisp-testme))
;;; ==============================
;;; CREATED: <Timestamp: 2009-07-31-W31-5T13:53:52-0400Z - by MON KEY>
(defun mon-insert-lisp-testme (&optional search-func test-me-count insertp intrp)
  "Insert at point a newline and commented test-me string.
When non-nil SEARCH-FUNC will search backward for a function name and include it
in the test-me string.
When non-nil TEST-ME-COUNT insert test-me string N times. Default is 1\(one\).
When prefix arg TEST-ME-COUNT is non-nil inerts N number of ';;;test-me' strings
and prompts y-or-n-p if we want to include the function name in insertions.
When INSERTP is non-nil insert the test-me string(s) in current buffer at point.
Use at the end of newly created elisp functions to provide example test cases.
Regexp held by global var `*regexp-symbol-defs*'.\n
See also; `mon-insert-doc-help-tail', `mon-test->*regexp-symbol-defs*'
`mon-insert-doc-help-tail', `mon-insert-lisp-stamp', `mon-insert-copyright',
`mon-insert-lisp-CL-file-template', `comment-divider',
`comment-divider-to-col-four', `mon-insert-lisp-evald'. ►►►"
  (interactive "i\np\ni\np")
  (let* ((get-func)
         (tmc (cond ((and intrp (> test-me-count 1))
                      (if ((lambda () (yes-or-no-p "Search-function-name? :")))
                          (progn (setq get-func t) test-me-count)
                        (progn (setq get-func nil) test-me-count)))
                    ((not test-me-count) 1)
                    (t  test-me-count)))
         (func (if (or search-func get-func)
                   (save-excursion
                     (search-backward-regexp  *regexp-symbol-defs*)
                     (buffer-substring-no-properties (match-beginning 3) (match-end 3)))))
         (test-me-string (if (or search-func get-func)
                             (format ";;;test-me;(%s )" func)
                           ";;;test-me;"))
         (limit (make-marker))
         (cnt tmc)
         (return-tms))
    (while (>= cnt 1)
      (setq return-tms (concat test-me-string "\n" return-tms))
      (setq cnt (1- cnt)))
    (if (or intrp insertp)
          (progn
            (save-excursion
              (when insertp (newline))
              (when (not (bolp))(beginning-of-line))
              (princ return-tms (current-buffer))
              (set-marker limit (point)))
          (search-forward-regexp
           (format "%s$" test-me-string) (marker-position limit) t)
          t) ; t needed here to prevent returning buffer position when called externally?
        return-tms)))
 ) ;CLOSE UNLESS FBOUNDP

;;; ==============================
;;; UNCOMMENT TO TEST:
;;;(defun some-function (&optional optional)
;;;(defun some-function-22 (&optional optional)
;;;(defun *some/-symbol:->name<-2* (somevar
;;;(defmacro some-macro ()
;;;(defmacro some-macro*:22 (&rest)
;;;(defun *some/-symbol:->name<-2* (somevar
;;;(defvar *some-var* 'var
;;;(defun *some/-symbol:->name<-2* 'somevar

;;;test-me;
;;(let ((find-def* *regexp-symbol-defs*))
;; (search-backward-regexp find-def*))

;;;test-me;`(,(match-beginning 3) ,(match-end 3))
;;;test-me;(match-sring 1) ;grp 1=>"(defun* some-func:name* ("
;;;test-me;(match-sring 2) ;grp 2=>"(defun* "
;;;test-me;(match-string 3) ;grp 3=>"some-macro*:22"
;;;test-me;(match-sring 4) ;grp 4=>" ("

;;;test-me;(mon-insert-lisp-testme)
;;;test-me;(mon-insert-lisp-testme t 3 )
;;;test-me;(mon-insert-lisp-testme nil 3)
;;;test-me;(mon-insert-lisp-testme nil 3 t)
;;;test-me;(mon-insert-lisp-testme t 3 t)
;;;test-me;(mon-insert-lisp-testme t nil t)
;;;test-me;(mon-insert-lisp-testme nil nil t)
;;;test-me;(mon-insert-lisp-testme nil nil nil)

;;; ==============================

;;; ==============================
;;; TODO:
;;; - This function _should_ test if it found a variable.
;;;   If so, it should change the fomrat spec of 'fstrings'  to:
;;;   "\n(mon-help-function-spit-doc '%s nil t t)
;;; - The test-me subr should insert 4 'test-me's for functions
;;;   "test-me;(<FNAME>)"
;;;   "test-me;(<FNAME> t)"
;;;   "test-me;(describe-function '<FNAME>)"
;;;   "test-me;;(call-interactively '<FNAME>)"
;;;   When a variable is found: should insert:
;;;   "test-me; <VARNAME>"
;;;   "test-me;(describe-variable '<VARNAME>)"
;;; 
;;; NOTE: Use (beginning-of-defun 2) w/ *regexp-symbol-defs*
;;; CREATED: <Timestamp: Thursday July 16, 2009 @ 11:38.10 AM - by MON KEY>
(defun mon-insert-doc-help-tail (&optional fname test-me-cnt insertp intrp)
  "Return function body code template when body uses a docstring instrospection.
For functions which call `mon-help-function-spit-doc' in the body. Additionally,
insert ';;;test-me;' templates after the closing of defun. When FNAME \(a string\)
is non-nil don't search for function's name in head of defun substitute FNAME instead.
When TEST-ME-CNT is non-nil include N 'test-me' strings with returned template.
Default test-me-cnt is 3 'test-me's.
When called programmatically INSERTP is non-nil or if called interactively insert
code template in buffer at point; does not move point.
Regexp held by global var `*regexp-symbol-defs*'.\n
See also; `mon-insert-lisp-testme', `mon-test->*regexp-symbol-defs*'. ►►►"
  (interactive "i\ni\ni\np")
  (let* ((the-sym (if fname
                   fname
                 (save-excursion
                   (search-backward-regexp *regexp-symbol-defs*)
                   (buffer-substring-no-properties (match-beginning 3) (match-end 3)))))
         (tm-cnt (if test-me-cnt test-me-cnt 3))
         (found (match-string-no-properties 2))
         (fun-typs '("defun" "defun*" "defmacro" "defmacro*" "defsubst" "defsubst*"))
         (var-typs '("defvar" "defconst" "defcustom"))
         ;;(sym-name &key :alt-cookie :do-var :insertp :do-face :do-group :do-theme
         (sym-str-cond (cond ((car (member found fun-typs))
                                   "      (mon-help-function-spit-doc '%s :insertp t)\n")
                              ((car (member found var-typs))
                              "      (mon-help-function-spit-doc '%s :do-var t :insertp t)\n")
                              ((string= found "defface")
                              "      (mon-help-function-spit-doc '%s :do-face t :insertp t)\n")
                              ((string= found "defgroup")
                               "      (mon-help-function-spit-doc '%s :do-group t :insertp t)\n")
                              ((string= found "deftheme")
                               "      (mon-help-function-spit-doc '%s :do-theme t :insertp t)\n")
                              (t "      (mon-help-function-spit-doc '%s :insertp t)\n")))
         (fstring
          (format
           (concat
            "(interactive \"i%s%sP\")\n"
            "  (if (or insertp intrp)\n"
            sym-str-cond
            "    (message \"pass non-nil for optional arg INTRP\")))\n;;\n%s")
           (char-to-string 92)(char-to-string 110) ;;gets us a '\n'
           the-sym
           (if fname
               (replace-regexp-in-string "$" (concat "(" fname " )")
                                         (mon-insert-lisp-testme nil tm-cnt nil))
                     (mon-insert-lisp-testme t tm-cnt nil)))))
    (if (or intrp insertp)
        (save-excursion (newline) (princ fstring (current-buffer)))
      fstring)))

;;;test-me;(mon-insert-doc-help-tail)
;;;test-me;(mon-insert-doc-help-tail nil 3)
;;;test-me;(mon-insert-doc-help-tail nil nil t)
;;;test-me;(mon-insert-doc-help-tail)
;;;test-me;(mon-insert-doc-help-tail "some-function" 3)
;;;test-me;(mon-insert-doc-help-tail "some-function-name" 3 t)
;;;test-me;(mon-insert-doc-help-tail "some-function-name" nil t)
;;;test-me;(call-interactively 'mon-insert-doc-help-tail)
;;
;;;(mon-insert-lisp-testme nil 3 nil)

;;; ==============================
;;; MODIFICATIONS: <Timestamp: #{2009-09-07T19:54:58-04:00Z}#{09371} - by MON KEY>
;;; FIXES:  CL &key &aux args in the tail of Elisp &rest e.g. this-> ``&rest --cl-rest--''
;;; CREATED: <Timestamp: #{2009-08-20T21:24:31-04:00Z}#{09345} - by MON>
(defun mon-help-function-args (func)
  "Return arg list of FUNC.\n
EXAMPLE:\n\(mon-help-function-args 'mon-help-function-args\)
\(mon-help-function-args 'mon-help-function-spit-doc\) ;CL arg-list with &key
CALLED-BY: `mon-help-insert-documentation'.\n
See also; `mon-help-xref-symbol-value', `mon-help-parse-interactive-spec'
`mon-help-function-spit-doc'.\n►►►"
  (let* ((def (if (symbolp func)
                 (symbol-function func)
               nil))
	 (tst-def1 (member '&rest (cadr def)))
	 (tst-def2 (and tst-def1 (string= (cadr tst-def1) '--cl-rest--))))
    (if tst-def2
    (let (get-args)
      (setq get-args
	    (with-temp-buffer
	      (let (sexp-bnds)	 
		(princ (documentation def) (current-buffer))
		(goto-char (point-max))
		(backward-sexp)
		(setq sexp-bnds (bounds-of-thing-at-point 'sexp))
		(buffer-substring-no-properties (car sexp-bnds) (cdr sexp-bnds)))))
      (setq get-args (car (read-from-string (downcase get-args))))
      (setq get-args (delete 'fn get-args)))
    (help-function-arglist def))))
;;
;;;test-me;(mon-help-function-args 'mon-help-function-args)
;;;test-me;(mon-help-function-args 'mon-help-function-spit-doc)

;;; ==============================
;;; CREATED: <Timestamp: #{2009-09-30T17:22:54-04:00Z}#{09403} - by MON KEY>
(defvar *mon-help-interactive-spec-alist* nil
  "alist of interactive spec arguments and values.
Alist key (an intereractive spec letter) maps to shortform spec-type.
spec-type is a string delimited by `<' and `>'.
CALLED-BY: FUNCTION: `mon-help-parse-interactive-spec'
See also; `mon-help-xref-symbol-value', `mon-help-insert-documentation'
`mon-help-function-spit-doc'.\n►►►")
;;
(unless (bound-and-true-p *mon-help-interactive-spec-alist*)
  (setq *mon-help-interactive-spec-alist* 
        '((a "<FUNCTION-NAME>")
          (b "<EXISTING-BUFFER-NAME>")
          (B "<BUFFER-NAME-OR-NON-EXISTING>")
          (c "<CHARACTER-NO-INPUT-METHOD>")
          (C "<COMMAND-NAME>")
          (d "<VALUE-POINT-AS-NUMBER-NO-I/O>")
          (D "<DIRECTORY-NAME>")
          (E "<PARAMETRIZED-EVENT>")
          (f "<EXISTING-FILE-NAME>")
          (F "<FILE-NAME-OR-NON-EXISTING>")
          (G "<FILE-NAME-OR-NON-EXISTING-W/DIR-NAME>") 
          (i "<IGNORED-NOOP>")       ;; NO-I/O
          (K "<KEY-SEQUENCE-DOWNCASE-MAYBE>") ;; (downcase the last event if needed to get a definition).
          (K "<KEY-SEQUENCE-REDEFINE-NO-DOWNCASE>") ;; (do not downcase the last event).
          (m "<VALUE-MARK-AS-NUMBER>")              ;; NO-I/O
          (M "<ANY-STRING-W/INPUT-METHOD>")
          (N "<NUMBER<-MINIBUFFER>")
          (N "<NUMERIC-PREFIX-ARG>")
          (p "<PREFIX-ARG->NUMBER>") ;; NO-I/O
          (P "<PREFIX-ARG-RAW>")     ;; Does not do I/O. NO-I/O
          (r "<REGION>")             ;; NO-I/O
          (s "<ANY-STRING>")         ;; Does not inherit the current input method.
          (S "<ANY-SYMBOL>")         ;; NO-INPUT-METHOD
          (U "<MOUSE-UP-EVENT>")     ;; discarded by a previous k or K argument.
          (v "<VARIABLE-NAME>")
          (x "<READ-LISP-EXPRESSION-NO-EVALUATE>")
          (X "<READ-LISP-EXPRESSION-EVALUATE>")
          (z "<CODING-SYSTEM>")
          (Z "<CODING-SYSTEM-NIL-NO-PREFIX>"))))
;;
;;;test-me; *mon-help-interactive-spec-alist*
;;;test-me;(assoc 'z *mon-help-interactive-spec-alist*)
;;
;;; (progn (makunbound '*mon-help-interactive-spec-alist*) 
;;;       (unintern '*mon-help-interactive-spec-alist*))

;;; ==============================
;;; CREATED: <Timestamp: #{2009-09-07T20:04:57-04:00Z}#{09372} - by MON KEY>
;;; MODIFICATIONS: <Timestamp: #{2009-09-30T17:29:53-04:00Z}#{09403} - by MON KEY>
(defun mon-help-parse-interactive-spec (fname)
  (let* ((int-spec *mon-help-interactive-spec-alist*)
         (int-t (interactive-form fname))
         (int-has-spec (when int-t
                         (cond ((stringp (cadr int-t)) 
                                (concat 
                                 "<INTERACITVE-SPEC>\n;;; " 
                                 (mapconcat 
                                  (lambda (x)
                                    (concat (cadr (assoc-string (subseq x 0 1) int-spec))" " (subseq x 1)))
                                  (split-string (cadr int-t) "\n") "\n;;; ")))
                               ;; NOTE: Leave the trailing line for `mon-insert-dcoumentation'.
                               ((listp (cadr int-t)) "<INTERACTIVE-SPEC-IS-LIST>")))))
    int-has-spec))
;;
;;;test-me;(mon-help-parse-interactive-spec  'mon-insert-file-in-dirs)
;;;test-me;(mon-help-parse-interactive-spec 'mon-help-reference-sheet)
;;;test-me;(mon-help-parse-interactive-spec 'mon-insert-string-n-times)
;;;test-me;(mon-help-parse-interactive-spec 'mon-insert-string-n-fancy-times)
;;;test-me;(listp (cadr (interactive-form  'mon-insert-file-in-dirs)))
;;;test-me;(listp (cadr (interactive-form 'mon-insert-string-n-times)))

;;; ==============================
;;; CREATED: <Timestamp: #{2009-09-30T19:23:06-04:00Z}#{09403} - by MON KEY>
;;; Now put a doc-string on `mon-help-parse-interactive-spec'
;;; using value & docstring of var `*mon-help-parse-interactive-spec-alist*'.
(put 'mon-help-parse-interactive-spec 'function-documentation
     (let* ((chop-var
             (split-string 
              (plist-get (symbol-plist '*mon-help-interactive-spec-alist*) 'variable-documentation)
              "CALLED-BY: FUNCTION: `mon-help-parse-interactive-spec'"))
            (hd-chop (car chop-var))
            (tl-chop (cadr chop-var))
            (splc-chop)
            (out-chop))
       (setq splc-chop
             (with-temp-buffer 
               (insert-char 32 1)
               (save-excursion (pp *mon-help-interactive-spec-alist* (current-buffer)))
               (indent-pp-sexp)
               (buffer-substring-no-properties (buffer-end 0) (buffer-end 1))))
       (setq splc-chop 
             (concat 
              hd-chop 
              "\nAlist is held by the variable `*mon-help-interactive-spec-alist*'\n\n"
              splc-chop 
              tl-chop))))
;;
;;;test-me;(describe-function 'mon-help-parse-interactive-spec)

;;; ==============================
;;; TODO:
;;; Include this in the document snarfing routine of mon-help-doc-utils.el
;;; (interactive-form 'mon-insert-file-in-dirs)
;;; (interactive-form 'mon-insert-file-in-dirs)
;;; CREATED: <Timestamp: #{2009-08-20T21:30:15-04:00Z}#{09345} - by MON>
(defun mon-help-insert-documentation (&optional func-list var-list face-list alt-cookie)
  "Return documentation of symbols held by lists FUNC-LIST VAR-LIST FACE-LIST.
When non-nil ALT-COOKIE is a doc-cookie per `mon-help-function-spit-doc' spec.
Default is `*doc-cookie*'.
See also; `mon-help-function-args', `mon-help-xref-symbol-value'.\n►►►"
  (let ((fl func-list)
        (vl var-list)
        (fc-l face-list)
        (alt-c (when alt-cookie (format ":alt-cookie %s" alt-cookie)))
        (dlims)
        (the-docstrings))
    (setq dlims
          (mapcar '(lambda (x)
                     (concat
                      "\n;;; ============================================================\n"
                      ";;; `%s' " x"\n"
                      ";;; ============================================================\n"))
                  '("<FUNCTION>" "<VARIABLE>" "<FACE>")))
    (save-excursion
      (setq the-docstrings
            (with-temp-buffer
              (when fl
                (dolist (i fl)
                  (let ((f-dlim (format (car dlims) i))
                        (int-specs (if (mon-help-parse-interactive-spec i)
                                       (concat "\n;;; " (mon-help-parse-interactive-spec i))
                                               ""))
                        (args-lst (if (mon-help-function-args i) 
                                      (concat ";;; <ARG-LIST>\n;;; " (format "%S" (mon-help-function-args i)))
                                    ""))
                        (alt-ck (if alt-c
                                    (mon-help-function-spit-doc i alt-c)
                                  (mon-help-function-spit-doc i))))
                    (princ 
                     (concat f-dlim (format "%s%s\n%s\n%s" args-lst int-specs (comment-divider t) alt-ck) "\n")
                     (current-buffer)))))
              (when vl
                (dolist (j vl)
                  (let ((v-dlim (format (cadr dlims) j)))
                    (princ
                     (concat v-dlim (get j 'variable-documentation)  "\n")
                     (current-buffer)))))
              (when fc-l
                (dolist (k fc-l)
                  (let ((fc-dlim (format (caddr dlims) k)))
                    (princ
                     (concat fc-dlim  (get k 'face-documentation) "\n")
                     (current-buffer)))))
              (buffer-string)))
      (insert the-docstrings))))
;;
(defalias 'mon-insert-documentation 'mon-help-insert-documentation)
;;
;;;test-me;
;; (setq mon-tmp-func-l (list 'mon-insert-string-n-times 'mon-help-reference-sheet
;;                           'mon-insert-file-in-dirs 'mon-help-insert-documentation 
;;                           'mon-insert-string-n-times))
;;
;;;test-me;(mon-help-insert-documentation mon-tmp-func-l)
;;
;;(progn (makunbound 'mon-tmp-func-l) (unintern 'mon-tmp-func-l))

;;; ==============================
;;; CREATED: <Timestamp: Friday July 03, 2009 @ 04:53.29 PM - by MON KEY>
(defun mon-help-reference-sheet (&optional insertp intrp)
  "Help refrence-sheet to help you find help references.
Why not! :) ►►►
;; FUNCTION LISTS:
`mon-help-buffer-functions'
`mon-help-emacs-introspect'
`mon-help-file-dir-functions'
`mon-help-plist-functions'
`mon-help-process-functions'
`mon-help-read-functions'
`mon-help-xml-functions'\n
;; SEARCHING:
`mon-help-search-functions'
`mon-help-regexp-syntax'
`mon-help-syntax-class'\n
;; HELP:
`mon-tags-apropos'
`mon-tags-naf-apropos'
`mon-help-package-keywords'
;; TIME:
`mon-help-iso-8601'\n
;; CL:
`mon-help-CL:TIME'                        ;-> \"mon-doc-help-CL.el\"
`mon-help-CL:LOCAL-TIME'                  ;-> \"mon-doc-help-CL.el\"
`mon-help-CL:LOOP'                        ;-> \"mon-doc-help-CL.el\"\n
;; ASCII ART:
`mon-help-easy-menu'
`mon-help-widgets'
`mon-help-font-lock'
`mon-help-color-chart'\n
;; PRESENTATION:
`mon-help-make-faces'
`mon-help-basic-faces'
`mon-help-faces-themes'
`mon-help-text-property-functions'
`mon-help-text-property-stickyness'
`mon-help-color-functions'\n
;; KEYS:
`mon-help-keys'
`mon-help-slime-keys'\n
;; CHAR-TABLES:\n
`mon-help-diacritics'
`mon-help-ASCII-chars' 
`mon-help-ISO-8859-1-chars'
`mon-help-cntl->hex->ecma-35'
`mon-help-cntl->hex->ecma-48'\n
;; RECIPES:
`mon-help-info-incantation'
`mon-help-install-info-incantation'
`mon-help-rename-incantation'
`mon-help-tar-incantation'
`mon-help-format-width'\n
;; INTROSPECTION AND UTILITY:
`mon-help-w32-env'
`mon-index-elisp-symbol'
`references-sheet-help-emacs-introspect'
`mon-help-function-args'
`mon-help-parse-interactive-spec'
`mon-help-xref-symbol-value'
`mon-help-swap-var-doc-const-val'
`mon-help-function-spit-doc'
`mon-help-insert-documentation'
`mon-insert-doc-help-tail'
`mon-insert-doc-help-cookie'
`emacs-wiki-escape-lisp-string-region'
`emacs-wiki-unescape-lisp-string-region'\n
;; VARIABLES:
`*reference-sheet-help-A-HAWLEY*'         ;<VARIABLE>
`*w32-env-variables-alist*'               ;<VARIABLE>
`*doc-cookie*'                            ;<VARIABLE>"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-reference-sheet :insertp t)
    (message "pass non-nil for optional arg INTRP")))

;;;test-me(mon-help-reference-sheet t)

;;; ==============================
;;; CREATED: <Timestamp: Thursday July 02, 2009 @ 02:34.14 PM - by MON KEY>
(defun mon-help-emacs-introspect (&optional insertp intrp)
  "Variables and functions related to what this Emacs knows about this Emacs.
Unless indicated as a '<FUNCTION>' items listed are '<VARIABLE>'. ►►►\n
;; INITIALIZATION/BUILD:
`emacs-build-system'
`emacs-build-time'
`emacs-major-version'
`emacs-minor-version'
`emacs-version'          ;<FUNCTION>
`emacs-uptime'           ;<FUNCTION>
`emacs-priority'
`system-configuration'
`initial-environment'
`emacs-init-time'
`init-file-had-error'
`init-file-user'
`features'\n
;; PATHS/DIRECTORIES:
`build-files'
`load-path'
`load-history'
`path-separator'
`installation-directory'
`configure-info-directory'
`data-directory'
`source-directory'
`doc-directory'
`internal-doc-file-name'
`invocation-directory'
`invocation-name'
`exec-directory'
`exec-path'\n
;; STATE:
`cons-cells-consed'
`floats-consed'
`intervals-consed'
`misc-objects-consed'
`strings-consed'
`string-chars-consed'
`vector-cells-consed'
`obarray'
`memory-full'
`memory-signal-data'
`max-specpdl-size'
`garbage-collect'
`gc-cons-threshold'
`gc-cons-percentage'\n
;; I/O:
`initial-window-system'
`glyph-table'
`charset-list'
`keyboard-type'
`global-map'
`null-device'"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-emacs-introspect :insertp t)
    (message "pass non-nil for optional arg INTRP")))
;;
;;;test-me;(mon-help-emacs-introspect t)

;;; ==============================
;;; (dolist (i 
;;;          (list 'abbrev 'alloc 'applications 'auto-save 'bib 'c 'calendar 'comm
;;;          'convenience 'data 'debug 'development 'dired 'display 'dnd 'docs
;;;          'editing 'editing-basics 'emacs 'emulations 'environment 'execute
;;;          'extensions 'external 'faces 'files 'frames 'games 'hardware 'help
;;;          'hypermedia 'i18n 'installation 'internal 'keyboard 'languages 'limits
;;;          'lisp 'local 'mail 'maint 'matching 'menu 'mode-line 'mouse 'mule
;;;          'multimedia 'news 'oop 'outlines 'processes 'processes-basics
;;;          'programming 'terminals 'tex 'tools 'undo 'unix 'windows 'wp 'x))
;;;   (princ (format "%s %s\n---\n" i (mon-help-function-spit-doc i :do-group t)) 
;;;          (current-buffer)))
;;; CREATED: <Timestamp: #{2009-09-03T19:27:41-04:00Z}#{09364} - by MON KEY>
(defun mon-help-package-keywords (&optional insertp intrp)
  "Find packages matching a given keyword using `finder-by-keyword'
Keyword Search for Lisp Libraries with \\[finder-by-keyword] command
search the standard Emacs Lisp libraries by topic keywords.
See info node `\(emacs\)Library Keywords\'\.\n
EXAMPLE:\n\(finder-by-keyword\)\n
Here is a partial list of keywords for use with finder-by-keyword:\n
abbrev        Abbreviation handling, typing shortcuts, macros.
alloc         Storage allocation and gc for GNU Emacs Lisp interpreter.
applications  Applications written in Emacs.
auto-save     Preventing accidental loss of data.
bib           Code related to the `bib' bibliography processor.
c             Support for the C language and related languages.
calendar      Calendar and time management support.
comm          Communications, networking, remote access to files.
convenience   Convenience features for faster editing.
data          Support for editing files of data.
development   Support for further development of Emacs.
dired         Directory \"Editor\"
display       How characters are displayed in buffers.
dnd           Handling data from drag and drop.
docs          Support for Emacs documentation.
editing       Basic text editing facilities.
emacs         Customization of the One True Editor.
emulations    Emulations of other editors.
environment   Fitting Emacs with its environment.
execute       Executing external commands.
extensions    Emacs Lisp language extensions.
external      Interfacing to external utilities.
faces         Support for multiple fonts.
files         Support for editing and manipulating files.
frames        Support for Emacs frames and window systems.
games         Games, jokes and amusements.
hardware      Support for interfacing with exotic hardware.
help          Support for on-line help systems and Help commands.
hypermedia    Support for links between text or other media types.
i18n          Internationalization and alternate character-set support.
installation  Emacs installation.
internal      Code for Emacs internals, build process, defaults.
keyboard      Input from the keyboard.
languages     Specialized modes for editing programming languages.
lisp          Lisp support, including Emacs Lisp.
local         Code local to your site.
mail          Modes for electronic-mail handling.
maint         Maintenance aids for the Emacs development group.
matching      Various sorts of searching and matching.
menu 	      Input from the menus.
mode-line     Content of the modeline.
mouse 	      Input from the mouse.
mule 	      MULE Emacs internationalization.
multimedia    Non-textual support, specifically images and sound.
news 	      Support for netnews reading and posting.
oop 	      Support for object-oriented programming.
outlines      Support for hierarchical outlining.
processes     Process, subshell, compilation, and job control support.
programming   Support for programming in other languages.
terminals     Support for terminal types.
tex           Supporting code related to the TeX formatter.
tools         Programming tools.
unix          Front-ends/assistants for, or emulators of, UNIX-like features.
windows       Windows within a frame.
wp            Word processing.
x 	      The X Window system.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-package-keywords :insertp t)
    (message "pass non-nil for optional arg INTRP")))

;;;test-me;(mon-help-package-keywords)
;;;test-me;(mon-help-package-keywords t)
;;;test-me;(call-interactively 'mon-help-package-keywords)
;;;test-me;(describe-function 'mon-help-package-keywords)

;;; ==============================
;;; CREATED: <Timestamp: #{2009-08-08T13:00:11-04:00Z}#{09326} - by MON KEY>
(defun mon-help-regexp-syntax (&optional insertp intrp)
  "Regexp Syntax overview.
SIMPLIFIED! See info node `(elisp)Syntax of Regexps' for discussion.
                        or \(info \"\(elisp\)Syntax of Regexps\")
See also; `mon-help-search-functions'.\n►►►\n
;; SPECIAL CHARS
\.          --> match ANY
\*          --> match Preceeding - ALL
\+          --> match Preceeding - AT LEAST once.
\?          --> match Preceeding - once OR not at all
\*\? \+\? \?\?   --> match Preceeding - NON-GREEDY
\\=[...\]      --> Character ALTERNATIVE
\\=[^...\]     --> COMPLEMENTed Character Alternative
^          --> match BOL
$          --> match EOL
\\          --> backslash QUOTE special chars\n
;; BACKSLASH CONSTRUCTS:
\\|         --> ALTERNATIVE
\\={m\\}       --> REPEAT match exactly N times
\\={m,n\\}     --> REPEAT match n-N times
\\(...\\)    --> GROUPING Construct
\\(\\? ... \) --> SHY Grouping Construct
\\digit     --> match DIGITH occurence
\\w         --> match any WORD CONSTITUENT char
\\W         --> match any char NOT a Word Constituent
\\Scode     --> match any char with SYNTAX code
\\Scode     --> match any char NOT with Syntax code
\\cc        --> match any char with CATEGORY
\\Cc        --> match any char NOT with Category
\\`         --> match EMPTY String
\\\\\='         --> match Empty String only at EOB
\\\\==         --> match Empty String only at POINT
\\b         --> match Empty String only at BEGINNING OR END of Word
\\B         --> match Empty String NOT at beginning or end of Word
\\=\\<         --> match Empty String only at BEGINNING of Word
\\=\\>         --> match Empty String only at END of Word
\\_<        --> match Empty String only at BEGINNING of Symbol
\\_>        --> match Empty String only at END of Symbol\n
;; CHARACTER CLASSES:
\\=[:ascii:] [:nonascii:]
\\=[:alnum:] [:digit:] [:xdigit:]
\\=[:alpha:] [:lower:] [:upper:]
\\=[:blank:] [:cntrl:] [:graph:]
\\=[:print:] [:punct:] [:space:] [:word:]
\\=[:multibyte:] [:unibyte:]"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-regexp-syntax :insertp t)
    (message "pass non-nil for optional arg INTRP")))

;;;test-me;(describe-function 'mon-help-regexp-syntax)
;;;test-me;(mon-help-regexp-syntax)
;;;test-me;(mon-help-regexp-syntax t)
;;;test-me;(call-interactively 'mon-help-regexp-syntax)

;;; ==============================
;;; CREATED: <Timestamp: Wednesday June 17, 2009 @ 04:58.00 PM - by MON KEY>
(defun mon-help-syntax-class (&optional insertp intrp)
  "Syntax Class mappings.
See; info node `(elisp)Syntax Table Internals'
FILE: `syntax_spec_code', `syntax_code_spec' in syntax.c
List one maps from Int->Class->Code-Char.
List two maps Syntax class code character arguments to SYNTAX.\n
INTEGER CLASS              CODE-CHAR
0       whitespace         \(designated by ` ' or `-'\)
1       punctuation        \(designated by `.'\)
2       word               \(designated by `w'\)
3       symbol             \(designated by `_'\)
4       open parenthesis   \(designated by `\('\)
5       close parenthesi   \(designated by `\)'\)
6       expression prefi   \(designated by `''\)
7       string quote       \(designated by `\"'\)
8       paired delimiter   \(designated by `$'\)
9       escape             \(designated by `\\'\)
10      character quote    \(designated by `/'\)
11      comment-start      \(designated by `<'\)
12      comment-end        \(designated by `>'\)
13      inherit            \(designated by `@'\)
14      generic comment    \(designated by `!'\)
15      generic string     \(designated by `|'\)\n
Syntax class code character arguments to SYNTAX include:
Syntax class: whitespace character; \(designated by ` ' or `-'\)
Syntax class: word constituent; \(designated by `w'\)
Syntax class: symbol constituent; \(designated by `_'\)
Syntax class: punctuation character; \(designated by `.'\)
Syntax class: open parenthesis character; \(designated by `\('\)
Syntax class: close parenthesis character; \(designated by `\)'\)
Syntax class: string quote; \(designated by `\"'\)
Syntax class: escape-syntax character: \(designated by `\\'\)
Syntax class: character quote; \(designated by `/'\)
Syntax class: paired delimiter; \(designated by `$'\)
Syntax class: expression prefix; \(designated by `''\)
Syntax class: comment starter; \(designated by `<'\)
Syntax class: comment ender; \(designated by `>'\)
Syntax class: inherit standard syntax; \(designated by `@'\)
Syntax class: generic comment delimiter; \(designated by `!'\)
Syntax class: generic string delimiter; \(designated by `|'\)\n ►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-syntax-class :insertp t)
    (message "pass non-nil for optional arg INTRP")))

;;;test-me;(mon-help-syntax-class)
;;;test-me;(mon-help-syntax-class t)
;;;test-me;(call-interactively 'mon-help-syntax-class)
;;;test-me;(describe-function 'mon-help-syntax-class)

;;; ==============================
;;; CREATED: <Timestamp: #{2009-08-07T18:16:16-04:00Z}#{09325} - by MON KEY>
(defun mon-help-search-functions (&optional insertp intrp)
  "Common functions, vars, commands for searching, replacing, substituting.
See also `mon-help-regexp-syntax'. ►►► \n
;; MON-LOCAL
`replace-in-string-mon'
`replace-char-in-string-mon'
`mon-string-split-on-regexp'
`mon-string-csv-regexp'
`mon-string-rotate-to-regexp'
`mon-string-sub-old->new'
`replace-string-pairs-region-no-props'
`replace-string-pairs-region3'\n
;; SEARCHING:
`serach-forward'
`search-backward'
`search-forward-regexp'  ;-> `re-search-forward' ;See also; `posix-search-forward'
`search-backward-regexp' ;-> `re-search-backward' ;See also; `posix-search-backward'
`word-search-forward'
`word-search-backward'\n
;; INTERROGATE SEARCH:
`looking-at' ;also, `posix-looking-at'
`looking-back'
`match-data'
`match-string'
`match-string-no-properties'
`match-begining'
`match-end'
`subregexp-context-p'
`replace-match'
`replace-match-data'
`replace-match-maybe-edit'
`replace-match-string-symbols'\n
;; REPLACE-ACTIONS:
`replace'
`replace-rectangle'
`replace-regexp'
`replace-string'
`replace-regexp-in-string'
`replace-eval-replacement'
`replace-loop-through-replacements'
`perform-replace'
`map-query-replace-regexp'\n
;; STRING:
`string-match' ;also, `posix-string-match'
`string-match-p'
`string-equal'
`string='\n
;; MODIFY:
`regexp-opt'
`regexp-opt-depth'
`regexp-quote'
`replace-quote'\n
;; SUBSTITUTION:
`subst-char-in-region'
`subst-char-in-string'
`translate-region'\n
;; DIRED:
`tags-search'
`tags-query-replace'
`dired-do-search'
`dired-do-query-replace-regexp'\n
;; VARIABLES:
`search-spaces-regexp'
`case-replace'
`case-fold-search'
`default-case-fold-search'
`page-delimiter'
`paragraph-separate'
`paragraph-start'
`sentence-end' ;Also a function!
`sentence-end-base'
`sentence-end-double-space'
`sentence-end-without-space'
`sentence-end-without-period'"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-search-functions :insertp t)
    (message "pass non-nil for optional arg INTRP")))

;;;test-me;(mon-help-search-functions)
;;;test-me;(mon-help-search-functions t)
;;;test-me;(call-interactively 'mon-help-search-functions)

;;; ==============================
;;; CREATED: <Timestamp: #{2009-09-02T10:22:56-04:00Z}#{09363} - by MON KEY>
(defun mon-help-buffer-functions (&optional insertp intrp)
  "Buffer related functions. ►►► \n
;; BUFFFER POSITIONS:
`mark-whole-buffer'
`beginning-of-buffer'
`beginning-of-buffer-other-window'
`end-of-buffer'
`end-of-buffer-other-window'
`eobp'
`buffer-end'
`point-min'
`point-max'\n
;; BUFFER CONTENTS:
`insert-buffer'
`prepend-to-buffer'
`buffer-string'
`buffer-substring'
`filter-buffer-substring'
`buffer-substring-filters'       ;; <VARIABLE>
`buffer-substring-no-properties'
`buffer-swap-text'
`erase-buffer'
`append-to-buffer'
`copy-to-buffer'
`with-temp-buffer'
`with-output-to-temp-buffer'
\(read \(get-buffer \"SOME-BUFFER-NAME\"\)\)\n
;; BUFFER MOVEMENT & HANDLERS
`next-buffer'
`previous-buffer'
`unbury-buffer'
`create-file-buffer'
`get-buffer-create'
`set-buffer'
`pop-to-buffer'
`switch-to-buffer'
`get-buffer'
`bury-buffer'
`other-buffer'
`clone-buffer'
`clone-indirect-buffer'
`clone-indirect-buffer-other-window'
`display-buffer'
`display-buffer-other-frame'
`fit-window-to-buffer'
`make-indirect-buffer'
`replace-buffer-in-windows'
`switch-to-buffer'
`switch-to-buffer-other-frame'
`switch-to-buffer-other-window'
`view-buffer-other-frame'
`view-buffer-other-window'\n
;; ACTIONS ON BUFFERS:
`ispell-buffer'
`save-buffer'
`rename-buffer'
`revert-buffer'
`view-buffer'
`eval-buffer'
`eval-current-buffer'
`lock-buffer'
`unlock-buffer'\n
;; ACTIONS ON MULITPLE BUFFERS:
`grep-buffers'
`buffer-list'
`list-buffers'
`buffer-menu'
`buffer-menu-other-window'
`save-some-buffers'\n
;; KILLING BUFFERS:
`kill-buffer'
`kill-buffer-and-window'
`kill-matching-buffers'
`kill-some-buffers'
`kill-this-buffer'\n
;; BUFFER EXAMINATION:
`current-buffer'
`buffer-name'
`buffer-size'
`get-buffer-process'
`get-buffer-window'
`get-buffer-window-list'
`buffer-base-buffer'
`buffer-chars-modified-tick'
`buffer-disable-undo'
`buffer-enable-undo'
`buffer-face-mode'
`buffer-face-set'
`buffer-face-toggle'
`buffer-file-name'
`buffer-has-markers-at'
`gap-size'
`gap-position'
`buffer-live-p'
`buffer-local-value'
`buffer-local-variables'
`buffer-modified-p'
`buffer-modified-tick'
`list-buffers-directory'\n
;; BUFFER FORMAT & CODING:
`format-encode-buffer'
`format-decode-buffer'
`buffer-file-format' ;<VARIABLE>
`revert-buffer-with-coding-system'"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-buffer-functions :insertp t)
    (message "pass non-nil for optional arg INTRP")))

;;;test-me;(describe-function 'mon-help-buffer-functions)
;;; ==============================
;;; File/directory name functions.
;;; CREATED: <Timestamp: Wednesday May 06, 2009 @ 01:13.41 PM - by MON KEY>
(defun mon-help-file-dir-functions (&optional insertp intrp)
"Litany of file/directory name functions. e.g.\n
\(Note: backquoted functions for help-view xrefs are not inserted.).
-
`convert-standard-filename' - \(convert-standard-filename \"c:/Documents and Settings/All Users/Start Menu\"\)
`clear-visited-file-modtime'
-
`default-directory'
`directory-files' - \(directory-files default-directory\)
\(directory-files \(file-name-directory \(buffer-file-name\)\) nil \".jpg\"\) ;some extension
-
`directory-file-name' - \(directory-file-name default-directory\)
\(directory-file-name \(buffer-file-name\)\)
`directory-files-and-attributes' - \(directory-files-and-attributes default-directory\) ;&optional full match nosort id-format
-
\(concat \"../\" \(`buffer-file-name'\)\)
\(`expand-file-name' \"../\"\)
\(expand-file-name \"../../\"\)
\(expand-file-name \"../../../\"\)
-
\(`file-attributes' - (file-attributes (buffer-file-name)) ;&optional id-format
`file-directory-p' - \(file-directory-p doc-directory\)
`file-exists-p' - \(file-exists-p \(buffer-file-name\)\)
`file-expand-wildcards' - \(file-expand-wildcards \(concat doc-directory\"/*.el\"\)\) ;&optional full\)
-
`file-name-absolute-p' -\(file-name-absolute-p \(directory-file-name default-directory\)\)
`file-name-directory' - \(file-name-directory \(buffer-file-name\)\)
`file-name-nondirectory' - \(file-name-nondirectory \(directory-file-name default-directory\)\)
`file-name-as-directory' - \(file-name-as-directory default-directory\)
`file-name-nondirectory' - \(file-name-nondirectory \(buffer-file-name\)\)
`file-name-sans-extension' - \(file-name-sans-extension \(buffer-file-name\)\)
-
`file-newer-than-file-p' - \(file-newer-than-file-p \(buffer-file-name\) doc-directory\)
-
`file-regular-p' - \(file-regular-p doc-directory\)
\(file-regular-p \"~/.emacs\"\)
-
`file-relative-name' - \(file-relative-name default-directory\)
\(file-relative-name  \(buffer-file-name\)\)
-
`find-buffer-visiting' - \(find-buffer-visiting \(filename\)  ;&optional predicate
`find-file' - \(find-file \(buffer-file-name\)\) ; &optional wildcards
-
`make-directory' - \(make-directory \(dir &optional parents\)\)
-
`split-string' - \(split-string \(directory-file-name default-directory\) \"/\"\)
`mon-toggle-dired-dwim-target' - \(mon-toggle-dired-dwim-target\)
`mon-get-file-mod-times' - \(mon-get-file-mod-times mon-user-emacsd\)
`mon-insert-dirs-in-path' - \(mon-insert-dirs-in-path symbol path\)
`mon-insert-file-in-dirs' - \(mon-insert-file-in-dirs \(make-dir-list insert-text extension\)\)
`mon-insert-naf-file-in-dirs' - \(mon-insert-naf-file-in-dirs \(make-dir-list\)\)
`substitute-in-file-name' - \(substitute-in-file-name filename\) ;sub in environmental variables
-
`thing-at-point' - \(thing-at-point 'filename\)\)
`bounds-of-thing-at-point' - \(bounds-of-thing-at-point 'filename\)
-
\(file-attributes default-directory)
 list-returned consists of 11 elements:
 t = directory; nth 0
 number of names file has; nth 1
 UID; nth 2
 GID; nth 3
 last accessed; nth 4 -> (current-time) ->(HIGH LOW MICROSEC) -(current-time-zone) (current-time)
 last modified; nth 5-> (current-time) ->(HIGH LOW MICROSEC)
 size in bytes; nth 6
 files' modes; nth 7
 t - GID changes if deleted; nth 8
 file's inode number; nth 9
 file system number; nth 10\n ►►►"
(interactive "i\nP")
(if (or insertp intrp)
    (mon-help-function-spit-doc 'mon-help-file-dir-functions :insertp t)
  (message "pass non-nil for optional arg INTRP")))

;;;test-me;(mon-help-file-dir-functions)
;;;test-me;(mon-help-file-dir-functions t)
;;;test-me;(call-interactively 'mon-help-file-dir-functions)
;;;test-me;(describe-function 'mon-help-file-dir-functions)

;;; ==============================
;;; CREATED: <Timestamp: Friday July 03, 2009 @ 04:45.34 PM - by MON KEY>
(defun mon-help-process-functions (&optional insertp intrp)
"Process related functions.
Unless indicated as a 'variable' items listed are functions.\n►►►\n
`accept-process-output'
`call-process'
`call-process-shell-command'
`continue-process'
`delete-process'
`list-system-processes'
`list-processes'
`make-network-process'
`set-process-sentinel'
`shell-quote-argument'
`start-file-process-shell-command'
`set-process-query-on-exit-flag'
`start-process-shell-command'
`stop-process'
`system-process-attributes'
`waiting-for-user-input-p'
;; ==============================
`process-adaptive-read-buffering' ;<VARIABLE>
`process-attributes'
`process-buffer'
`process-coding-system'
`process-coding-system-alist'     ;<VARIABLE>
`process-command'
`process-connection-type'         ;<VARIABLE>
`process-contact'
`process-exit-status'
`process-environment'             ;<VARIABLE>
`process-file'
`process-file-shell-command'
`process-filter'
`process-filter-multibyte-p'
`process-get'
`process-id'
`process-inherit-coding-system-flag'
`process-kill-without-query'
`process-lines'
`process-list'
`process-mark'
`process-name'
`process-plist'
`process-put'
`process-query-on-exit-flag'
`process-running-child-p'
`process-send-eof'
`process-send-region'
`process-send-string'
`process-sentinel'
`process-status'
`process-tty-name'
`process-type'
`processp'"
(interactive "i\nP")
(if (or insertp intrp)
    (mon-help-function-spit-doc 'mon-help-process-functions :insertp t)
  (message "pass non-nil for optional arg INTRP")))

;;;test-me;(mon-help-process-functions t)

;;; ==============================
;;; TODO: add other xml related material from xml-rpc, and ./lisp/nxml
;;; CREATED: <Timestamp: #{2009-09-17T12:06:53-04:00Z}#{09384} - by MON KEY>
(defun mon-help-xml-functions (&optional insertp intrp)
  "XML related functions.\n►►►
;; From FILE: `./lisp/xml.el'
`xml-get-children'
`xml-parse-attlist'
`xml-parse-dtd'
`xml-parse-elem-type'
`xml-parse-file'
`xml-parse-fragment'
`xml-parse-region'
`xml-parse-string'
`xml-parse-tag'
`xml-print'
`xml-debug-print'
`xml-escape-string'\n
;; FILE: ./nxml/xsd-regexp.el
`xsdre-translate'
`xsdre-parse-regexp'
;; FILE: ./nxml/xmltok.el
`xmltok-unicode-to-char'
`xmltok-forward'
`xmltok-forward-prolog'
`xmltok-forward-special'
;; FILE: ./nxml/nxml-parse.el
`nxml-parse-file'"
  (interactive "i\np")
    (mon-help-function-spit-doc 'mon-help-xml-functions :insertp t)
  (message "pass non-nil for optional arg INTRP"))

;;;test-me;(mon-help-xml-functions t)

;;; ==============================
;;; CREATED: <Timestamp: #{2009-09-20T13:26:27-04:00Z}#{09387} - by MON>
(defun mon-help-read-functions (&optional insrtp intrp)
  "List of functions for reading.
Unless indicated all items in list are functions.
See FILE: `lread.c'                                               
See info node `(elisp)Read and Print'. ►►►\n
      _______                       ____________________    62.
     |       |                     |                    |     
     | READ: |                     | STANDARD-READERS:  |     
   __|_______|_______________     _|____________________|_    
  |                          |   |                        |   
  | `read'                   |   | `read-color'           |   
  | `read-from-string'       |   | `read-kbd-macro'       |   
  | `read-from-whole-string' |   | `read-number'          |   
  |__________________________|   | `read-passwd'          |   
        _________________        | `read-regexp'          |   
       |                 |       | `read-string'          |   
       | STANDARD-INPUT: |       | `read-shell-command'   |   
       |  READ STREAMS   |       |________________________|   
   ____|_________________|________________________________    
  |                                                       |   
  |  `standard-input'--+ <VARIABLE>                       |   
  |    |                 ______________________________   |   
  |    |-+ BUFFER       |                              |  |   
  |    |-+ MARKER       | `read-circle'                |  |   
  |    |-+ STRING       |  |-+ <VARIABLE>              |  |   
  |    |-+ FUNCTION     | `read-with-symbol-positions' |  |   
  |    |-+ SYMBOL       |  |-+ <VARIABLE>              |  |   
  |    |-+ t            | `read-symbol-positions-list' |  |   
  |    |-+ nil          |  |-+ <VARIABLE>              |  |   
  |                     |______________________________|  |   
  |_______________________________________________________|   
     ______________                          __________       
    |              |                        |          |      
    | MINI-BUFFER: |                        | BUFFERS: |      
   _|______________|_______      ___________|__________|__    
  |                        |    |                         |   
  | `read-minibuffer'      |    | `read-buffer'           |   
  | `read-from-minibuffer' |    | `read-buffer-function'  |   
  | `read-no-blanks-input' |    | `read-buffer-to-switch' |   
  |________________________|    |_________________________|   
     ________                                  ________       
    |        |                                |        |      
    | FILES: |                                | CHARS: |      
   _|________|________________    ____________|________|____  
  |                           |  |                          | 
  | `read-file-name'          |  | `read-char'              | 
  | `read-directory-name'     |  | `read-char-by-name'      | 
  | `read-file-name-internal' |  | `read-char-excvusive'    | 
  | `read-file-modes'         |  | `read-charset'           | 
  | `file-readable-p'         |  | `read-quoted-char'       | 
  | `desktop-read'            |  | `read-quoted-char-radix' | 
  | `read-abbrev-file'        |  |  |-+<VARIABLE>           | 
  |___________________________|  |__________________________| 
    _______________                            ________       
   |               |                          |        |      
   | `KEYS/EVENTS: |                          | FACES: |      
 __|_______________|_________   ______________|________|____  
|                            | |                            | 
| `read-event'               | | `read-face-font'           | 
| `read-key-sequence'        | | `read-face-name'           | 
| `read-key-sequence-vector' | | `read-face-attribute'      | 
| `read-command'             | | `read-all-face-attributes' | 
| `unread-command-events'    | | `read-face-and-attribute'  | 
|  |-+ <VARIABLE>            | |____________________________| 
|____________________________|  ______________                
                               |              |               
                               | ENVIRONMENT: |               
                      _________|______________|_____          
                     |                              |            
                     | `read-coding-system'         |         
                     | `read-non-nil-coding-system' |           
                     | `read-input-method-name'     |         
                     | `read-multilinlual-string'   |         
                     | `read-language-name'         |         
                     | `read-envvar-name'           |         
                     |______________________________|         
                                                            62^\n"
  (interactive "i\nP")
  (if (or insrtp intrp)
      (mon-help-function-spit-doc 'mon-help-read-functions :insertp t)
    (message "pass non-nil for optional arg INTRP")))

;;;test-me;(mon-help-read-functions)
;;;test-me;(mon-help-read-functions t)
;;;test-me;(describe-function 'mon-help-read-functions)
;;;test-me;(call-interactively 'mon-help-read-functions)

;;; ==============================
;;; CREATED: <Timestamp: #{2009-09-19T17:02:02-04:00Z}#{09386} - by MON KEY>
(defun mon-help-plist-functions (&optional insertp intrp)
  "plist or property list related functions.
See info node `(elisp)Documentation Tips'. ►►►
;; PLISTS:
`get'
`plist-get'
`lax-plist-get'
`put'
`plist-put'
`lax-plist-put'
`setplist'
`plist-member'
`symbol-plist'
`charset-plist'
`process-plist'
`documentation-property'
`buffer-access-fontified-property'
`variable-documentation'            ;; <PROPERTY>\n
;; ERROR PROPERTIES:
`error' ;; <PLIST> (symbol-plist 'error)
 |
 |----+ `error-conditions'          ;; <PROPERTY>                
 |	  
 |----+ `error-message'             ;; <PROPERTY>                  
 |	           
 |----+ `byte-compile-format-like'  ;; <PROPERTY>"

  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-plist-functions :insertp t)
    (message "pass non-nil for optional arg INTRP")))
;;
;;;test-me;(mon-help-plist-functions )
;;;test-me;(mon-help-plist-functions t)
;;;test-me;(call-interatively 'mon-help-plist-functions)
;;;test-me;(describe-function 'mon-help-plist-functions)

;;; ==============================
;;; DOCSTRING sourced from commented sections of `cus-face.el', and `custom.el'
;;; The Example theme, forest-theme.el
;;; COURTESY: Chong Yidong SOURCE: emacs-devel SUBJECT: Re: Color themes
;;; DATE: Sun, 09 Aug 2009 12:18:01 -0400
;;; (URL `http://lists.gnu.org/archive/html/emacs-devel/2009-08/msg00356.html')
;;; CREATED: <Timestamp: #{2009-08-24T18:30:56-04:00Z}#{09351} - by MON KEY>
(defun mon-help-faces-themes (&optional insertp intrp)
  "Functions for handling themes and their faces. ►►►\n
`custom-set-faces'
`custom-reset-faces'
`custom-declare-face'
`custom-face-attributes-get'
`custom-face-attributes'      ;<CONSTANT>
`deftheme'                    ;<MACRO>
`load-theme'
`provide-theme'
`enable-theme'
`disable-theme'
`custom-enabled-themes'       ;<VARIABLE>
`custom-enabling-themes'      ;<VARIABLE>
`custom-declare-theme'
`customize-create-theme'
`custom-push-theme'
`custom-make-theme-feature'
`custom-check-theme'
`custom-variable-theme-value'
`custom-theme-recalc-variable'
`custom-theme-directory'      ;<VARIABLE>
`custom-known-themes'         ;<VARIABLE>
`custom-theme-p'
`custom-theme-set-faces'
`custom-theme-reset-faces'
`custom-theme-recalc-face'
`custom-theme-set-variables'\n
SEE FILES: `cus-face.el', `custom.el', `cus-edit.el'.\n
CUSTOM-THEMES:\n
Custom themes are collections of settings that can be enabled or
disabled as a unit.\n
Each Custom theme is defined by a symbol, called the theme name.
The `theme-settings' property of the theme name records the
variable and face settings of the theme.  This property is a list
of elements, each of the form:\n
    (PROP SYMBOL THEME VALUE)\n
- PROP is either `theme-value' or `theme-face'
- SYMBOL is the face or variable name
- THEME is the theme name (redundant, but simplifies the code)
- VALUE is an expression that gives the theme's setting for SYMBOL.\n
The theme name also has a `theme-feature' property, whose value is
specified when the theme is defined (see `custom-declare-theme').
Usually, this is just a symbol named THEME-theme.  This lets
external libraries call \(require 'foo-theme\).\n
In addition, each symbol (either a variable or a face) affected by
an *enabled* theme has a `theme-value' or `theme-face' property,
which is a list of elements each of the form:\n
    (THEME VALUE)\n
which have the same meanings as in `theme-settings'.\n
The `theme-value' and `theme-face' lists are ordered by decreasing
theme precedence.  Thus, the first element is always the one that
is in effect.\n
Each theme is stored in a theme file, with filename THEME-theme.el.
Loading a theme basically involves calling \(load \"THEME-theme\"\)
This is done by the function `load-theme'.  Loading a theme
automatically enables it.\n
When a theme is enabled, the `theme-value' and `theme-face'
properties for the affected symbols are set.  When a theme is
disabled, its settings are removed from the `theme-value' and
`theme-face' properties, but the theme's own `theme-settings'
property remains unchanged.\n
DEFINING-THEMES:\n
A theme file should be named `THEME-theme.el' (where THEME is the theme
name), and found in either `custom-theme-directory' or the load path.
It has the following format:\n
\(deftheme THEME DOCSTRING\)
\(custom-theme-set-variables 'THEME  [THEME-VARIABLES]\)
\(custom-theme-set-faces 'THEME [THEME-FACES]\)
\(provide-theme 'THEME\)\n
EXAMPLE-THEME:\n
;;;; forest-theme.el\n
\(deftheme forest \"Created 2009-08-09.\"\)\n
\(custom-theme-set-faces 'forest\n
 '\(default \(\(t \(:foreground \"wheat\" :background \"black\"\)\)\)\)\n
 '\(font-lock-comment-face \(\(\(\(class color\) \(min-colors 88\)\)
                            \(:foreground  \"medium sea green\"\)\)\)\)\n
 '\(font-lock-constant-face \(\(\(\(class color\) \(min-colors 88\)\)
                             \(:foreground \"turquoise\"\)\)\)\)\n
 '\(font-lock-function-name-face \(\(\(\(class color\) \(min-colors 88\)\)
                                  \(:foreground \"pale green\"\)\)\)\)\n
 '\(font-lock-keyword-face \(\(\(\(class color\) \(min-colors 88\)\)
                            \(:foreground \"white\"\)\)\)\)\n\n
 '\(font-lock-string-face \(\(\(\(class color\) \(min-colors 88\)\)
                           \(:foreground \"dark khaki\"\)\)\)\)\n
 '\(font-lock-type-face \(\(\(\(class color\) \(min-colors 88\)\)
                         \(:foreground \"medium aquamarine\"\)\)\)\)\n
 '\(font-lock-variable-name-face \(\(\(\(class color\) \(min-colors 88\)\)
                                  \(:foreground \"yellow green\"\)\)\)\)\n
 '\(font-lock-warning-face \(\(\(\(class color\) \(min-colors 88\)\)
                            \(:foreground \"salmon1\"\)\)\)\)\n
 '\(font-lock-builtin-face \(\(\(\(class color\) \(min-colors 88\)\)
                            \(:foreground \"LightSteelBlue\"\)\)\)\)\n
 '\(region \(\(\(\(class color\) \(min-colors 88\)\)
            \(:foreground \"white\" :background \"dark green\"\)\)\)\)\n
 '\(highlight \(\(\(\(class color\) \(min-colors 88\)\)
               \(:foreground \"white\" :background \"dark green\"\)\)\)\)\)\n
\(provide-theme 'forest\)\n
;;;; end forest-theme.el \n ►►►"
  (interactive "i\np")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-faces :insertp t)
    (message "pass non-nil for optional arg INTRP")))

;;;test-me(mon-help-faces-themes t)

;;; ==============================
;;; CREATED: <Timestamp: #{2009-09-26T13:23:36-04:00Z}#{09396} - by MON KEY>
(defun mon-help-make-faces (&optional insertp intrp)
"Functions, Variables, Properties, etc. for defining faces.
See info node `(elisp)Defining Faces'. See info node `(elisp)Face Attributes'.
See also; `mon-help-basic-faces' `mon-help-font-lock', `mon-help-color-chart'.
►►►                    _________________                                  
		      |                 |                                 
		      | DEFINING-FACES: |                                 
 _____________________|_________________|______________________________73.
|                                         |                              |
| `defface' <MACRO>                       |   FACE DEFINING FUNCTIONS:   |
|  SPEC:                                  |______________________________|
|  | DISPLAY                              |                              |
|  | |-+ defalut                          |                              |
|  | |-+ t                                | `make-face'                  |
|  | |-+ list -> elts of form (Key Value) | `face-list'                  |
|  |   | (type {graphic x pc w32 tty})    | `copy-face'                  |
|  |   | (class {color grayscale mono})   | `face-id'                    |
|  |   | (background {light dark})        | `face-documentation'         |
|  |   | (min-colors {INTEGER})           | `face-equal'                 |
|  |   | (supports {VALUE})               | `face-differs-from-default-p'|
|  |                                      |                              |
|  +--------------------------------+     |______________________________|
|                                   |                                    |
|_____________________________   ___V____________________________________|
|                             | |                                        |
|  FACE ATTRIBUTE KEYWORDS:   | | :family - (a string)                   |
|_____________________________| | {Fontset name, Font family}            |
|                             | |  NOTE: `*' and `?' wildcards allowed.  |
|  :family :foundry           | |  See; `font-family-list'               |
|  :font :fontset             | |________________________________________|
|  :height                    | |                                        |
|  :width :weight :slant      | | :foundry - (a string)                  |
|  :foreground :background    | | NOTE: `*' `?' wildcards allowed.       |
|  :underline :overline       | |________________________________________|
|  :strike-through            | |                                        |
|  :box                       | | :font - (a font object)                |
|  :inverse-video             | | See info node `(elisp)Font Selection'  |
|  :stipple                   | | See info node `(elisp)Fontsets'        |
|                             | |________________________________________|
|_____________________________| |                                        |
|                             | | :height - (integer, floating point)    |
|  FACE ATTRIBUTE FUNCTIONS:  | | {1/10 point, float}                    |
|                             | |________________________________________|
|-----------------------------| |                                        |
|                             | | :width - (a symbol)                    |
| `font-family-list'          | | {normal, condensed, expanded           |
| `face-attribute'            | |  semi-condensed, semi-expanded         |
| `set-face-attribute'        | |  extra-condensed, extra-expanded       |
| `face-all-attributes'       | |  ultra-condensed, ultra-expanded}      |
| `merge-face-attribute'      | |________________________________________|
| `face-attribute-relative-p' | |                                        |
| `invert-face'               | | :weight - (a symbol)                   |
| `face-foreground'           | | {normal, bold, light                   |
| `face-background'           | |  semi-bold, semi-light                 |
| `face-stipple'              | |  extra-bold, extra-light               |
| `face-font'                 | |  ultra-bold, utltra-light}             |
| `face-bold-p'               | |________________________________________|
| `face-italic-p'             | |                                        |
| `face-underline-p'          | | :slant - (a symbol)                    |
| `face-inverse-video-p'      | |  {normal, italic, oblique,             |
|                             | |   reverse-italic, reverse-oblique}     |
|_____________________________| |________________________________________|
|                             | |                                        |
|  FACE ATTRIBUTE VARIABLES:  | | :foreground - (a string)               |
|                             | | :background - (a string)               |
|-----------------------------| | See info node `(elisp)Color Names'     |
|                             | |             `mon-help-color-chart'     |
| `bitmap-spec-p'             | |         `mon-help-color-functions'     |
| `face-attribute-name-alist' | |________________________________________|
| `custom-face-attributes'    | |                                        |
| `frame-background-mode'     | | :underline - (a boolean or string)     |
| `underline-minimum-offset'  | | :overline - (a boolean or string)      |
| `x-bitmap-file-path'        | | :strike-through - (boolean or string)  |
|                             | | {t - using face's color                |
|_____________________________| |  string - using specified color        |
|                             | |  nil - do not apply effect}            |
|    PROPERTY ATTRIBUTES:     | |________________________________________|
| (on face's symbol property) | |                                        |
|                             | | :box - (a boolean, string, list)       |
|-----------------------------| | {nil - no box                          |
|                             | |  t -  linewidth 1 in :foreground color |
| `face-alias'                | |  color - box w/ line width in COLOR    |
| `face-defface-spec'         | |  (:line-width WIDTH                    |
| `customized-face'           | |   :color COLOR                         |
| `face-documentation'        | |   :style STYLE)}                       |
| `saved-face'                | |________________________________________|
|                             | |                                        |
|_____________________________| | :inverse-video - (a boolean)           |
                                | {t - yes                               |
                                |  nil - no}                             |
                                |________________________________________|
                                |                                        |
                                | :stipple - (a string)                  |
                                | {bitmap -  See; `x-bitmap-file-path'   |
                                |  ,----                                 |
                                |  | WIDTH - Width in pixels             |
                                |  | HEIGHT - Height in pixels           |
                                |  | DATA - Data string of raw bits.     |
                                |  `----                                 |
                                |  nil - no stipple}                     |
                                |________________________________________|
                                |                                        |
                                | :inherit - (string or list)            |
                                | {face name, or list of face names}     |
                                |______________________________________73."
(interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-make-faces :insertp t)
    (message "pass non-nil for optional arg INTRP")))
;;
;;;test-me;(mon-help-make-faces)
;;;test-me;(mon-help-make-faces t)
;;;test-me;(describe-function 'mon-help-make-faces)
;;;test-me;(call-interactively 'mon-help-make-faces)

;;; ==============================
;;; CREATED: <Timestamp: #{2009-09-04T17:34:43-04:00Z}#{09365} - by MON KEY>
(defun mon-help-basic-faces (&optional insertp intrp)
  "basic-faces is a custom-group inheriting faces.
These standard Emacs faces are defined in faces.el
As of 23.1 there is no _formal_ indication that newly defined faces must inherit
from one of the basic-faces this practice is encouraged. See for example;
\(URL `http://lists.gnu.org/archive/html/emacs-devel/2009-08/msg00525.html') ►►► \n
;; BASIC-FACES:                    
\(describe-face 'default\)           
\(describe-face 'bold\)              
\(describe-face 'italic\)            
\(describe-face 'underline\)         
\(describe-face 'bold-italic\)       
\(describe-face 'fixed-pitch\)       
\(describe-face 'variable-pitch\)    
\(describe-face 'shadow\)            
\(describe-face 'link\)              
\(describe-face 'link-visited\)      
\(describe-face 'highlight\)         
\(describe-face 'region\)            
\(describe-face 'mode-line\)       
\(describe-face 'header-line\)                                          
\(describe-face 'secondary-selection\)
\(describe-face 'trailing-whitespace\)
\(describe-face 'escape-glyph\)
\(describe-face 'nobreak-space\)
\(describe-face 'mode-line-inactive\)
\(describe-face 'mode-line-highlight\)
\(describe-face 'mode-line-emphasis\)
\(describe-face 'mode-line-buffer-id\)
\(describe-face 'vertical-border\)
\(describe-face 'minibuffer-prompt\)
\(describe-face 'minibuffer-noticeable-prompt\)
\(describe-face 'fringe\)
\(describe-face 'scroll-bar\)
\(describe-face 'border\)
\(describe-face 'cursor\)
\(describe-face 'mouse\)
\(describe-face 'tool-bar\)
\(describe-face 'menu\)"
  (interactive "i\np")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-basic-faces :insertp t)
    (message "pass non-nil for optional arg INTRP")))

;;;test-me(mon-help-basic-faces t)
;;;test-me(describe-function 'mon-help-basic-faces)

;;; ==============================
;;; CREATED: <Timestamp: Wednesday June 17, 2009 @ 05:37.52 PM - by MON KEY>
(defun mon-help-font-lock (&optional insertp intrp)
"Each element of `font-lock-keywords' specifies how to find certain
cases of text, and how to highlight those cases:
►►►
 ___________________________________________________________________________79.
|                                                                             |
|REGEXP                                                                       |
|_____________________________________________________________________________|
|                                                                             |
|FUNCTION                                                                     |
|_____________________________________________________________________________|
|                                                                             |
|[MATCHER . SUBEXP]                                                           |
|         |->{REGEXP|FUNCTION}                                                |
|_____________________________________________________________________________|
|                                        +specify a proplist here**           |
|                                        |                                    |
|[MATCHER . FACESPEC]                    v                                    |
|	   	 |-> (FACESPEC (face FACE PROP1 VAL1 PROP2 VAL2...))          |
|                |                                                            |
|	   	 |-> (font-lock-extra-managed-props PROP1 VAL1 PROP2 VAL2)    |
|                         |                           |-> field VAL           |
|			  |       		         help-echo VAL        |
| **and/or specify it here^	 	                 category VAL         |
|_____________________________________________________________________________|
|                                                                             |
| [MATCHER] . [SUBEXP-HIGHLIGHTER]                                            |
|                    |-> [SUBEXP] . [FACESPEC]                                |
|                                    |-> [OVERRIDE                            |
|                                                {t|keep|prepend|append}      |
|                                         [LAXMATCH]]                         |
|                                                  {t|nil}                    |
|                                                   t-> NO ERROR if no find   |
|                                                   nil-> subexp missing ERROR|
|_____________________________________________________________________________|
|                                                                             |
|[MATCHER . ANCHORED-HIGHLIGHTER]                                             |
|         |-> [ANCHORED-MATCHER PRE-FORM POST-FORM                            |
|                                SUBEXP-HIGHLIGHTERS...]                      |
|_____________________________________________________________________________|
|                                                                             |
|[MATCHER . HIGHLIGHTERS...]                                                  |
|         |-> [SUBEXP-HIGHLIGHTER[ANCHORED-HIGHLIGHTER]]                      |
|_____________________________________________________________________________|
|                                                                             |
|[eval . FORM]                                                                |
|_____________________________________________________________________________|
                                                                            79^"
(interactive "i\nP")
(if (or insertp intrp)
    (mon-help-function-spit-doc 'mon-help-font-lock :insertp t)
  (message "pass non-nil for optional arg INTRP")))

;;;test-me;(mon-help-font-lock)
;;;test-me;(mon-help-font-lock t)
;;;test-me;(call-interactively 'mon-help-font-lock)
;;;test-me;(describe-function 'mon-help-font-lock)

;;; ==============================
;;; CREATED: <Timestamp: #{2009-09-20T13:46:12-04:00Z}#{09387} - by MON>
(defun mon-help-text-property-functions (&optional insrtp intrp)
  "list of text-property related functions.                                     
See info node `(elisp)Text Properties' and FILE: textprop.c ►►►                 
    ________________________              __________________________              
   |                        |            |                          |              
   | TEXT-PROPERTY GETTERS: |            | TEXT-PROPERTY SEARCHING: |              
 __|________________________|______    __|__________________________|________80.  
|                                  |  |                                        | 
| `buffer-substring-no-properties' |  | `next-property-change'                 | 
| `remove-list-of-text-properties' |  | `next-single-property-change'          | 
| `remove-text-properties'         |  | `next-char-property-change'            | 
| `get-text-property'              |  | `next-single-char-property-change'     | 
| `get-char-property'              |  | `previous-property-change'             | 
| `get-char-property-and-overlay'  |  | `previous-single-property-change'      |  
|__________________________________|  | `previous-char-property-change'        |
  ________________________            | `previous-single-char-property-change' |
 |                        |           | `text-properties-at'                   |
 | TEXT-PROPERTY SETTERS: |           | `text-property-any'                    |
 |________________________|           | `text-property-not-all'                |
|                          |          | `describe-text-properties'             |
| `propertize'             |          |________________________________________|
| `add-text-properties'    |                __________________________                                                 
| `put-text-property'      |               |                          |         
| `set-text-properties'    |               | SPECIAL TEXT-PROPERTIES: |         
|__________________________|              _|__________________________|____     
    ___________________________          |                                 |     
   |                           |         | category      fontified         |    
   | TEXT-PROPERTY VARIABLES:  |         | display       help-echo         |    
 __|___________________________|______   | syntax-table  composition       |      
|                                     |  | keymap        local-map         |         
|  `buffer-access-fontified-property' |  | face font-lock-face mouse-face  |    
|  `buffer-access-fontify-functions'  |  | read-only invisible intangible  |    
|  `default-text-properties'          |  | field     cursor    pointer     |    
|  `char-property-alias-alist'        |  | line-spacing        line-height |    
|  `minibuffer-allow-text-properties' |  | wrap-prefix         line-prefix |    
|  `text-property-default-nonsticky'  |  | modification-hooks              |    
|_____________________________________|  | insert-in-front-hooks           |    
                                         | insert-behind-hooks             |    
                                         | point-entered point-left        |    
                                         |_________________________________| 80^"
  (interactive "i\nP")
  (if (or insrtp intrp)
      (mon-help-function-spit-doc 'mon-help-text-property-functions :insertp t)
    (message "pass non-nil for optional arg INTRP")))

;;;test-me;(mon-help-text-property-functions)
;;;test-me;(mon-help-text-property-functions t)
;;;test-me;(describe-function 'mon-help-text-property-functions)
;;;test-me;(call-interactively 'mon-help-text-property-functions)

;;; ==============================
;;; COURTESY: /emacs/*/src/intervals.c
;;; CREATED: <Timestamp: #{2009-08-25T18:33:48-04:00Z}#{09352} - by MON KEY>
(defun mon-help-text-property-stickyness (&optional insertp intrp)
  "Any property might be front-sticky on the left, rear-sticky on the left,
front-sticky on the right, or rear-sticky on the right; the 16 combinations
can be arranged in a matrix with rows denoting the left conditions and
columns denoting the right conditions:
      _  __  _
_     FR FR FR FR
FR__   0  1  2  3
 _FR   4  5  6  7
FR     8  9  A  B
  FR   C  D  E  F\n
left-props  = '\(front-sticky \(p8 p9 pa pb pc pd pe pf\)
                rear-nonsticky \(p4 p5 p6 p7 p8 p9 pa pb\)
                p0 L p1 L p2 L p3 L p4 L p5 L p6 L p7 L
                p8 L p9 L pa L pb L pc L pd L pe L pf L\)\n
right-props = '\(front-sticky \(p2 p3 p6 p7 pa pb pe pf\)
                rear-nonsticky \(p1 p2 p5 p6 p9 pa pd pe\)
                p0 R p1 R p2 R p3 R p4 R p5 R p6 R p7 R
                p8 R p9 R pa R pb R pc R pd R pe R pf R\)\n
We inherit from whoever has a sticky side facing us.  If both sides
do \(cases 2, 3, E, and F\), then we inherit from whichever side has a
non-nil value for the current property.  If both sides do, then we take
from the left.\n
When we inherit a property, we get its stickiness as well as its value.
So, when we merge the above two lists, we expect to get this:\n
result      = '\(front-sticky \(p6 p7 pa pb pc pd pe pf\)
     	        rear-nonsticky \(p6 pa\)
                p0 L p1 L p2 L p3 L p6 R p7 R
                pa R pb R pc L pd L pe L pf L\)\n
The optimizable special cases are:
    left rear-nonsticky = nil, right front-sticky = nil \(inherit left\)
    left rear-nonsticky = t,   right front-sticky = t   \(inherit right\)
    left rear-nonsticky = t,   right front-sticky = nil \(inherit none\)
►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (save-excursion
        (mon-help-function-spit-doc 'mon-help-text-property-stickyness :insertp t))
    (message "pass non-nil for optional arg INTRP")))

;;;test-me;(mon-help-text-property-stickyness)
;;;test-me;(mon-help-text-property-stickyness t)
;;;test-me;(call-interactively 'mon-help-text-property-stickyness)
;;;test-me;(describe-function 'mon-help-text-property-stickyness)

;;; ==============================
;;; CREATED: <Timestamp: #{2009-09-04T17:54:39-04:00Z}#{09365} - by MON KEY>
(defun mon-help-color-functions (&optional insertp intrp)
"Color related functions. ►►► \n
`read-color'
`color-distance'
`display-color-p'
`color-values'
`color-defined-p'
`ansi-color-apply'
`ansi-color-make-color-map'
`background-color-at-point'
`foreground-color-at-point'
`set-cursor-color'
`set-border-color'
`set-background-color'
`set-foreground-color'
`set-mouse-color'
`defined-colors'
`list-colors-display'
`list-colors-duplicates'
`list-colors-print'
`x-colors'               ;<VARIABLE>
`color-name-rgb-alist'   ;<VARIABLE>
`ansi-color-map'         ;<VARIABLE>"
  (interactive "i\np")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-color-functions :insertp t)
    (message "pass non-nil for optional arg INTRP")))

;;;test-me(mon-help-color-functions t)
;;;test-me(describe-function 'mon-help-color-functions)

;;; ==============================
;;; CREATED: <Timestamp: Tuesday June 02, 2009 @ 12:09.40 PM - by MON KEY>
(defun mon-help-color-chart (&optional insertp intrp)
  "Chart of Netscape Color Names with their Color Values.
\(URL `http://www.timestream.com/mmedia/graphics/colors/ns3names.txt'\)
Chart prepared by Tay Vaughan, July, 1996. Timestream, Inc. ►►►\n
 ____________________________________________________________________________80.
| +-Netscape Name                                                              |
| |                   +-Hex Triplet                                            |
| |                   |       +-RGB Value                                      |
| |                   |       |            +-Director Mac Sys Approx.          |
| |                   |       |            |    +-Director Win Sys Approx.     |
| |                   |       |            |    |    +-SuperCard Approx.       |
| |                   |       |            |    |    |    +-Hex Approx.        |
| |                   |       |            |    |    |    |       +-RGB Approx |
| |                   |       |            |    |    |    |       |            |
| aliceblue           F0F8FF  240,248,255  000  000  001  FFFFFF  255,255,255  |
| antiquewhite        FAEBD7  250,235,215  001  -    002  FFFFCC  255,255,204  |
| aquamarine          7FFFD4  127,255,212  009  016  110  66FFCC  102,255,204  |
| azure               F0FFFF  240,255,255  000  000  001  FFFFFF  255,255,255  |
| beige               F5F5DC  245,245,220  001  -    002  FFFFCC  255,255,204  |
| bisque              FFE4C4  255,228,196  001  -    002  FFFFCC  255,255,204  |
| black               000000  0,0,0        255  255  256  000000  0,0,0        |
| blanchedalmond      FFEBCD  255,235,205  001  -    002  FFFFCC  255,255,204  |
| blue                0000FF  0,0,255      210  003  211  0000FF  0,0,255      |
| blueviolet          8A2BE2  138,43,226   097  097  098  9933CC  153,51,204   |
| brown               A52A2A  165,42,42    100  100  101  993333  153,51,51    |
| burlywood           DEB887  222,184,135  44   44   045  CCCC99  204,204,153  |
| cadetblue           5F9EA0  95,158,160   122  122  123  669999  102,153,153  |
| chartreuse          7FFF00  127,255,0    113  113  114  66FF00  102,255,0    |
| chocolate           D2691E  210,105,30   058  058  059  CC6633  204,102,51   |
| coral               FF7F50  255,127,80   021  023  022  FF6666  255,102,102  |
| cornflowerblue      6495ED  100,149,237  120  120  121  6699FF  102,153,255  |
| cornsilk            FFF8DC  255,248,220  001  -    002  FFFFCC  255,255,204  |
| cyan                00FFFF  0,255,255    180  001  181  00FFFF  0,255,255    |
| darkgoldenrod       B8860B  184,134,11   053  053  054  CC9900  204,153,0    |
| darkgreen           006400  0,100,0      203  201  204  006600  0,102,0      |
| darkkhaki           BDB76B  189,183,107  045  045  046  CCCC66  204,204,102  |
| darkolivegreen      556B2F  85,107,47    130  130  131  666633  102,102,51   |
| darkorange          FF8C00  255,140,0    017  019  018  FF9900  255,153,0    |
| darkorchid          9932CC  153,50,204   097  097  098  9933CC  153,51,204   |
| darksalmon          E9967A  233,150,122  015  -    016  FF9966  255,153,102  |
| darkseagreen        8FBC8F  143,188,143  080  080  081  99CC99  153,204,153  |
| darkslateblue       483D8B  72,61,139    170  170  171  333399  51,51,153    |
| darkslategray       2F4F4F  47,79,79     165  165  166  336666  51,102,102   |
| darkturquoise       00CED1  0,206,209    187  185  188  00CCCC  0,204,204    |
| darkviolet          9400D3  148,0,211    103  103  104  9900CC  153,0,204    |
| deeppink            FF1493  255,20,147   032  033  033  FF0099  255,0,153    |
| deepskyblue         00BFFF  0,191,255    186  184  187  00CCFF  0,204,255    |
| dimgray             696969  105,105,105  129  129  130  666666  102,102,102  |
| dodgerblue          1E90FF  30,144,255   156  156  157  3399FF  51,153,255   |
| firebrick           B22222  178,34,34    100  100  101  993333  153,51,51    |
| floralwhite         FFFAF0  255,250,240  000  000  001  FFFFFF  255,255,255  |
| forestgreen         228B22  34,139,34    160  160  161  339933  51,153,51    |
| gainsboro           DCDCDC  220,220,220  043  043  044  CCCCCC  204,204,204  |
| ghostwhite          F8F8FF  248,248,255  000  000  001  FFFFFF  255,255,255  |
| gold                FFD700  255,215,0    011  -    012  FFCC00  255,204,0    |
| goldenrod           DAA520  218,165,32   052  052  053  CC9933  204,153,51   |
| gray                808080  128,128,128  086  086  087  999999  153,153,153  |
| green               008000  0,128,0      197  195  198  009900  0,153,0      |
| greenyellow         ADFF2F  173,255,47   076  076  077  99FF33  153,255,51   |
| honeydew            F0FFF0  240,255,240  000  000  001  FFFFFF  255,255,255  |
| hotpink             FF69B4  255,105,180  019  021  020  FF66CC  255,102,204  |
| indianred           CD5C5C  205,92,92    057  057  058  CC6666  204,102,102  |
| ivory               FFFFF0  255,255,240  000  000  001  FFFFFF  255,255,255  |
| khaki               F0E68C  240,230,140  002  244  003  FFFF99  255,255,153  |
| lavender            E6E6FA  230,230,250  000  000  001  FFFFFF  255,255,255  |
| lavenderblush       FFF0F5  255,240,245  000  000  001  FFFFFF  255,255,255  |
| lawngreen           7CFC00  124,252,0    113  113  114  66FF00  102,255,0    |
| lemonchiffon        FFFACD  255,250,205  001  -    002  FFFFCC  255,255,204  |
| lightblue           ADD8E6  173,216,230  078  078  079  99CCFF  153,204,255  |
| lightcoral          F08080  240,128,128  014  240  015  FF9999  255,153,153  |
| lightcyan           E0FFFF  224,255,255  036  036  037  CCFFFF  204,255,255  |
| lightgoldenrod      EEDD82  238,221,130  008  -    009  FFCC99  255,204,153  |
| lightgldnrodyellow  FAFAD2  250,250,210  001  -    002  FFFFCC  255,255,204  |
| lightgray           D3D3D3  211,211,211  043  043  044  CCCCCC  204,204,204  |
| lightpink           FFB6C1  255,182,193  007  -    008  FFCCCC  255,204,204  |
| lightsalmon         FFA07A  255,160,122  015  -    016  FF9966  255,153,102  |
| lightseagreen       20B2AA  32,178,170   160  160  161  339933  51,153,153   |
| lightskyblue        87CEFA  135,206,250  078  078  079  99CCFF  153,204,255  |
| lightslate          8470FF  132,112,255  090  090  091  9966FF  153,102,255  |
| lightslategray      778899  119,136,153  122  122  123  669999  102,153,153  |
| lightsteelblue      B0C4DE  176,196,222  078  078  079  99CCFF  153,204,255  |
| lightyellow         FFFFE0  255,255,224  000  000  001  FFFFFF  255,255,255  |
| limegreen           32CD32  50,205,50    154  154  155  33CC33  51,204,51    |
| linen               FAF0E6  250,240,230  000  000  001  FFFFFF  255,255,255  |
| magenta             FF00FF  255,0,255    030  031  031  FF00FF  255,0,255    |
| maroon              B03060  176,48,96    107  107  108  990000  153,0,0      |
| mediumaquamarine    66CDAA  102,205,170  116  116  117  66CC99  102,204,153  |
| mediumblue          0000CD  0,0,205      211  208  212  0000CC  0,0,204      |
| mediumorchid        BA55D3  186,85,211   055  055  056  CC66CC  204,102,204  |
| mediumpurple        9370DB  147,112,219  091  091  092  9966CC  153,102,204  |
| mediumseagreen      3CB371  60,179,113   153  153  154  33CC66  51,204,102   |
| mediumslateblue     7B68EE  123,104,238  11126126  127  6666FF  102,102,255  |
| mediumspringgreen   00FA9A  0,250,154    182  181  183  00FF99  0,255,153    |
| mediumturquoise     48D1CC  72,209,204   15   151  152  33CCCC  51,204,204   |
| mediumviolet        C71585  199,21,133   068  068  069  CC0099  204,0,153    |
| midnightblue        191970  25,25,112    213  210  214  000066  0,0,102      |
| mintcream           F5FFFA  245,255,250  000  000  001  FFFFFF  255,255,255  |
| mistyrose           FFE4E1  255,228,225  000  000  001  FFFFFF  255,255,255  |
| moccasin            FFE4B5  255,228,181  007  -    008  FFCCCC  255,204,204  |
| navajowhite         FFDEAD  255,222,173  009  -    009  FFCC99  255,204,153  |
| navy                000080  0,0,128      212  209  213  000099  0,0,153      |
| oldlace             FDF5E6  253,245,230  000  000  001  FFFFFF  255,255,255  |
| olivedrab           6B8E23  107,142,35   124  124  125  669933  102,153,51   |
| orange              FFA500  255,165,0    017  019  018  FF9900  255,153,0    |
| orangered           FF4500  255,69,0     029  002  030  FF3300  255,51,0     |
| orchid              DA70D6  218,112,214  055  055  056  CC66CC  204,102,204  |
| palegoldenrod       EEE8AA  238,232,170  002  244  003  FFFF99  255,255,153  |
| palegreen           98FB98  152,251,152  074  074  075  99FF99  153,255,153  |
| paleturquoise       AFEEEE  175,238,238  072  072  073  99FFFF  153,255,255  |
| palevioletred       DB7093  219,112,147  056  056  057  CC6699  204,102,153  |
| papayawhip          FFEFD5  255,239,213  001  -    002  FFFFCC  255,255,204  |
| peachpuff           FFDAB9  255,218,185  007  -    008  FFCCCC  255,204,204  |
| peru                CD853F  205,133,63   052  052  053  CC9933  204,153,51   |
| pink                FFC0CB  255,192,203  007  -    008  FFCCCC  255,204,204  |
| plum                DDA0DD  221,160,221  049  049  050  CC99CC  204,153,204  |
| powderblue          B0E0E6  176,224,230  078  078  079  99CCFF  153,204,255  |
| purple              A020F0  160,32,240   096  096  097  9933FF  153,51,255   |
| red                 FF0000  255,0,0      035  035  036  FF0000  255,0,0      |
| rosybrown           BC8F8F  188,143,143  050  050  051  CC9999  204,153,153  |
| royalblue           4169E1  65,105,225   163  163  164  3366CC  51,102,204   |
| saddlebrown         8B4513  139,69,19    101  101  102  993300  153,51,0     |
| salmon              FA8072  250,128,114  015  -    016  FF9966  255,153,102  |
| sandybrown          F4A460  244,164,96   015  -    016  FF9966  255,153,102  |
| seagreen            2E8B57  46,139,87    159  159  160  339966  51,153,102   |
| seashell            FFF5EE  255,245,238  000  000  001  FFFFFF  255,255,255  |
| sienna              A0522D  160,82,45    094  094  095  996633  153,102,51   |
| skyblue             87CEEB  135,206,235  078  078  079  99CCFF  153,204,255  |
| slateblue           6A5ACD  106,90,205   127  127  128  6666CC  102,102,204  |
| slategray           708090  112,128,144  086  086  087  999999  153,153,153  |
| snow                FFFAFA  255,250,250  000  000  001  FFFFFF  255,255,255  |
| springgreen         00FF7F  0,255,127    183  182  184  00FF66  0,255,102    |
| steelblue           4682B4  70,130,180   157  157  158  3399CC  51,153,204   |
| tan                 D2B48C  210,180,140  044  044  045  CCCC99  204,204,153  |
| thistle             D8BFD8  216,191,216  043  043  044  CCCCCC  204,204,204  |
| tomato              FF6347  255,99,71    022  024  023  FF6633  255,102,51   |
| turquoise           40E0D0  64,224,208   151  151  152  33CCCC  51,204,204   |
| violet              EE82EE  238,130,238  012  -    013  FF99FF  255,153,255  |
| violetred           D02090  208,32,144   062  062  063  CC3399  204,51,153   |
| wheat               F5DEB3  245,222,179  007  -    008  FFCCCC  255,204,204  |
| white               FFFFFF  255,255,255  000  000  001  FFFFFF  255,255,255  |
| whitesmoke          F5F5F5  245,245,245  000  000  001  FFFFFF  255,255,255  |
| yellow              FFFF00  255,255,0    005  004  006  FFFF00  255,255,0    |
| yellowgreen         9ACD32  154,205,50   082  082  083  99CC33  153,204,51   |
|____________________________________________________________________________80^"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-color-chart :insertp t)
    (message "pass non-nil for optional arg INTRP")))

;;;test-me;(mon-help-color-chart)
;;;test-me;(mon-help-color-chart t)
;;;test-me;(call-interactively 'mon-help-color-chart)
;;;test-me;(describe-function 'mon-help-color-chart)
;;; ==============================
;;; CREATED: <Timestamp: Wednesday June 17, 2009 @ 05:36.08 PM - by MON KEY>
(defun mon-help-easy-menu (&optional insertp intrp)
"Following is the mapping for building menus with easy-menu `easy-menu-define':\n
►►►
 ____________________________________________________________________________79.
|                      ; MENU Root                                             |
| [SYMBOL MAPS DOC MENU                                                        |
.                  |                      ;Menu Child                          |
.                  + (NAME CALLBACK ENABLE                                     |
.                  | ...... NAME ;string                                       |
.                  | ...... CALLBACK ;command|List                             |
.                  | ...... ENABLE ;Expression                                 |
.                  :        | ... :filter . FUNCTION ;Function                 |
.                  :        | ... :visible . INCLUDE ;Expression               |
.                  :        | ... :active . ENABLE ;Expression                 |
.                  |  )                                                        |
.                  :__________.                      ;Menu Child ELEMENTS      |
.                             | [NAME CALLBACK ENABLE                          |
.                             |___.                                            |
.                             :   | ... :filter . FUNCTION ;Function           |
.                             :   | ... :visible . INCLUDE ;Expression         |
.                             :   | ... :active . ENABLE ;Expression           |
.                             :   | ... :label . FORM ;Expression              |
.                             :   | ... :keys . KEYS ;String                   |
.                             :   | ... :key-sequence . KEYS ;String|Vector    |
.                             :   | ... :help . HELP ;String                   |
.                             :   | ... :selected . SELECTED ;Expression       |
.                             :   | ... :style . STYLE ;SYMBOL                 |
.                             :   :            |... toggle: radio: button:     |
.                             | ]                                              |
| ]                                                                            |
|______________________________________________________________________________|
                                                                             79^"
(interactive "i\nP")
(if (or insertp intrp)
    (mon-help-function-spit-doc 'mon-help-easy-menu :insertp t)
  (message "pass non-nil for optional arg INTRP")))

;;;test-me;(mon-help-easy-menu)
;;;test-me;(mon-help-easy-menu t)
;;;test-me;(call-interactively 'mon-help-easy-menu)
;;;test-me;(describe-function 'mon-help-easy-menu)

;;; ==============================
;;; CREATED: <Timestamp: Friday June 19, 2009 @ 02:20.35 PM - by MON KEY>
(defun mon-help-widgets (&optional insertp intrp)
"Help table for the widget interface.\n
►►►
 __________________________
|                          | See info node `(elisp)Documentation Tips'.
| Widget Type - Syntax of: |
|__________________________|______________________________________________77.
|                                                                           |
| NAME ::= (NAME [KEYWORD ARGUMENT]... ARGS)                                |
|            |       |        |          |                                  |
|      widget-name   |        |          + widget-specific                  |
|                    |        + prop-val                                    |
|                    |                                                      |
|    .---------------+ prop-key                                             |
|    |                                                                      |
|    |--+ format                                                            |
|    |                                                                      |
|    |   `%[  %]' | `%{  %}' |  `%v',   `%d', `%h', `%t', `%%'              |
|    |      ^          ^                  ^     ^     ^                     |
|    |......|..........|..................|.....|.....|                     |
|           |          |                  |     |     |                     |
|           |          |--+ :sample-face  ._____.     |--+ :tag|:tag-glyph  |
|           |                                |                              |
|           |--+ :button-face                |--+ :doc                      |
|                                            |                              |
|                                            |--+ :documentation-property   |
|--+ :value          ;init-arg                                              |
|                                                                           |
|--+ :button-prefix  ;nil|String|Symbol                                     |
|                                                                           |
|--+ :button-suffix  ;nil|String|Symbol                                     |
|                                                                           |
|--+ :help-echo      ;{widget-forward|widget-backward}                      |
|                    ;String|[Function Arg]|[widget String]                 |
|                                                                           |
|--+ :follow-link    ;<mouse-1>                                             |
|                                                                           |
|--+ :indent         ;Integer                                               |
|                                                                           |
|--+ :offset         ;Integer                                               |
|                                                                           |
|--+ :extra-offset   ;Integer                                               |
|                                                                           |
|--+ :notify         ;[Function arg1 &optional arg2]                        |
|                                                                           |
|--+ :menu-tag       ;:tag                                                  |
|                                                                           |
|--+ :menu-tag-get   ;[Function (:menu-tag|:tag|:value)]                    |
|                                                                           |
|--+ :match          ;[widget value]                                        |
|                                                                           |
|--+ :validate       ;widget_._`widget-children-validate'_.                 |
|                            |                            |                 |
|                            |                            |--+ :children    |
|                            |--+ :error ;String                            |
|                                                                           |
|--+ :tab-order      ;{widget-forward|widget-backward}                      |
|                                                                           |
|--+ :parent         ;{`menu-choice item' | `editable-list element'}        |
|                                                                           |
|--+ :sibling-args   ;{radio-button-choice' `checklist'}                    |
|___________________________________________________________________________|
|  __________________   __________________            ___________________   |
| |                  | |                  |          |                   |  |
| |  widget-buttons  | | widget-functions |          | widget-navigation |  |
| |__________________| |__________________|  ________|___________________|  |
| |                  | |                  | |                            |  |
| |  Option|Field    | | `widget-value'   | | <TAB> | M-<TAB> | S-<TAB>  |  |
| |                  | | `widget-create'  | |      -----------------     |  |
| |  [INS]|[DEL]     | | `widget-delete'  | |     | widget-forward  |    |  |
| |                  | | `widget-insert'  | |     | widget-backward |    |  |
| |  [ ]|[X]         | | `widget-setup'   | |      -----------------     |  |
| |                  | | `widget-get'     | |____________________________|  |
| |  Embedded        | | `widget-put'     |    ________________             |
| |                  | |__________________|   |                |            |
| |  ( )|(*)         |  ____________________  | button-actions |            |
| |                  | |                    | |________________|__________  |
| |  [Apply Form]    | |  widget-faces      | |                           | |
| |                  | |____________________| |      <RET> | Mouse-2      | |
| |  [Reset Form]    | |                    | |   ---------------------   | |
| |__________________| | widget-button-face | |  | widget-button-press |  | |
|  __________________  | widget-field-face  | |  | widget-button-click |  | |
| |                  | | widget-mouse-face  | |   ---------------------   | |
| | widget-variables | |____________________| |___________________________| |
| |__________________|______________________                                |
| |                                         |                               |
| |  widget-keymap                          |                               |
| |  widget-global-map                      |                               |
| |  widget-glyph-directory    ;Directory   |                               |
| |  widget-glyph-enable       ;nil|t       |                               |
| |  widget-button-prefix      ;String      |                               |
| |  widget-button-suffix      ;String      |                               |
| |_________________________________________|                               |
|___________________________________________________________________________|
                                                                          77^"
(interactive "i\nP")
(if (or insertp intrp)
    (mon-help-function-spit-doc 'mon-help-widgets :insertp t)
  (message "pass non-nil for optional arg INTRP")))

;;;test-me;(mon-help-widgets)
;;;test-me;(mon-help-widgets t)
;;;test-me;(call-interactively 'mon-help-widgets)
;;;test-me;(describe-function 'mon-help-widgets))

;;; ==============================
;;; Map mon-insert-uniccode to -> (ucs-insert "12C")
(defun mon-help-diacritics (&optional insertp intrp)
  "Insert commonly used diacritics and their keymaps at point.\n
The Unicode latin scripts are found in several Unicode-Blocks, namely:\n
U+0000 - U+007F -> Controls and Basic Latin;\n\(URL `http://www.decodeunicode.org/en/basic_latin'\)\n
U+0080 - U+009F -> Controls and Latin-1;\n\(URL `http://www.decodeunicode.org/en/latin-1_supplement'\)\n
U+0100 - U+017F -> Latin Extended-A;\n\(URL `http://www.decodeunicode.org/en/latin_extended-a'\)\n
U+0180 - U+024F -> Latin Extended-B;\n\(URL `http://www.decodeunicode.org/en/latin_extended-b'\)\n
Character table for reverting ISO_8859-1 bytes -> UTF-8\n
\(URL `http://en.wikipedia.org/wiki/ISO_8859-1'\)
\(URL `http://en.wikipedia.org/wiki/ISO/IEC_8859'\)\n
See also; (URL `http://unicode.coeurlumiere.com/')\n
ECMA-094 code-tables:\n\(find-file \(concat mon-naf-mode-notes
                   \"/Ecma-094-8bit-single-byte-chars-iso-latin-1thru4.pdf\"\)\) ►►► \n
================
à À - C-x 8 ` a
á Á - C-x 8 ' a
ã Ã - C-x 8 ~ a
å Å - C-x 8 / a
â Â - C-x 8 ^ a
ä Ä - C-x 8 \" a
ă Ă - (ucs-insert \"103\") (ucs-insert \"102\")
æ Æ - C-x 8 / e
================
è È - C-x 8 ` e
é É - C-x 8 ' e
ë Ë - C-x 8 \" e
ê Ê - C-x 8 ^ e
ĕ Ĕ - (ucs-insert \"115\") (ucs-insert \"114\")
================
í Í - C-x 8 ' i
ì Ì - C-x 8 ` i
ï Ï - C-x 8 \" i
î Î - C-x 8 ^ i
ĭ Ĭ - (ucs-insert \"12D\") (ucs-insert \"12C\")
================
ó Ó - C-x 8 ' o
ò Ò - C-x 8 ` o
ø Ø - C-x 8 / o
ö Ö - C-x 8 \" o
ô Ô - C-x 8 ^ o
õ Õ - C-x 8 ~ o
ŏ Ŏ - (ucs-insert \"14F\") (ucs-insert \"14E\")
œ Œ - (ucs-insert \"153\") (ucs-insert \"152\")
================
ú Ú - C-x 8 ' u
ù Ù - C-x 8 ` u
ü Ü - C-x 8 \" u
û Û - C-x 8 ^ u
ů Ů - (ucs-insert \"16F\") (ucs-insert \"16E\")
ŭ Ŭ - (ucs-insert \"16D\") (ucs-insert \"16C\")
================
ý Ý - C-x 8 ' y
ÿ   - C-x 8 \" y
ç Ç - C-x 8 , c
č Č - (ucs-insert \"10D\") (ucs-insert \"10C\")
ñ Ñ - C-x 8 ~ n
ň Ň - (ucs-insert \"148\") (ucs-insert \"147\")
ß   - C-x 8 \" s
ř Ř - (ucs-insert \"159\") (ucs-insert \"158\")
š Š - (ucs-insert \"161\") (ucs-insert \"160\")
ź Ź - (ucs-insert \"17A\") (ucs-insert \"179\")
ž Ž - (ucs-insert \"17E\") (ucs-insert \"17D\")
þ Þ - C-x 8 ~ t
ð Ð - C-x 8 ~ d
=============
 « - C-x 8 <
 » - C-x 8 >
© - C-x 8 C
® - C-x 8 R
============
£ - C-x 8 L
¶ - C-x 8 P
§ - C-x 8 S
¥ - C-x 8 Y
¢ - C-x 8 c
============
÷ - C-x 8 / /
¬ - C-x 8 ~ ~
× - C-x 8 x
¤ - C-x 8 $
± - C-x 8 +
­ - C-x 8 -
· - C-x 8 .
¯ - C-x 8 =
µ - C-x 8 m
° - C-x 8 o    ;degree
º - C-x 8 _ o  ;ordinal
µ - C-x 8 u
¾ - C-x 8 3 / 4
½ - C-x 8 1 / 2
¼ - C-x 8 1 / 4
¹ - C-x 8 ^ 1  ;superscript 1
² - C-x 8 ^ 2  ;superscript 2
³ - C-x 8 ^ 3  ;superscript 3
============
¡ - C-x 8 !
¿ - C-x 8 ?
¦ - C-x 8 |
ª - C-x 8 _ a
' - C-x 8 ' SPC
´ - C-x 8 ' '
¨ - C-x 8 \" \"
¸ - C-x 8 , ,
  - C-x 8 * SPC
► - (ucs-insert \"25BA\")"
(interactive "i\nP")
(if (or insertp intrp)
  (save-excursion
    (let* ((test-llm (and (buffer-local-value longlines-mode (current-buffer))))
	   (is-on (and test-llm))
	   (llm-off))
    (progn
      (if (buffer-local-value 'longlines-mode (current-buffer))
	  (prog1
	      (longlines-mode nil)
	    (setq test-llm 'loc-buff-is-ll))
	(setq test-llm 'loc-buff-not-ll))
      (mon-help-function-spit-doc 'mon-help-diacritics :insertp t)
      (cond ((eq test-llm 'loc-buff-not-ll)
	     (setq test-llm nil))
	    ((eq test-llm 'loc-buff-is-ll)
	     (prog1
		 (longlines-mode t)	;
	       (setq test-llm nil)))))))
      (message "pass non-nil for optional arg INTRP")))

;;;test-me;(mon-help-diacritics)
;;;test-me;(mon-help-diacritics t)
;;;test-me;(describe-function 'mon-help-diacritics)

;;; ==============================
;;; CREATED: <Timestamp: #{2009-09-01T17:34:29-04:00Z}#{09362} - by MON KEY>
(defun mon-help-ASCII-chars (&optional insertp intrp)
  "ASCII Character Tables: ►►►\n
 _______________________________________________________________67.
|                                                                 |
| 000 NUL|001 SOH|002 STX|003 ETX|004 EOT|005 ENQ|006 ACK|007 BEL |
| 010 BS |011 HT |012 NL |013 VT |014 NP |015 CR |016 SO |017 SI  |
| 020 DLE|021 DC1|022 DC2|023 DC3|024 DC4|025 NAK|026 SYN|027 ETB |
| 030 CAN|031 EM |032 SUB|033 ESC|034 FS |035 GS |036 RS |037 US  |
| 040 SP |041  ! |042  \" |043  # |044  $ |045  % |046  & |047  '  |
| 050  ( |051  ) |052  * |053  + |054  , |055  - |056  . |057  /  |
| 060  0 |061  1 |062  2 |063  3 |064  4 |065  5 |066  6 |067  7  |
| 070  8 |071  9 |072  : |073  ; |074  < |075  = |076  > |077  ?  |
| 100  @ |101  A |102  B |103  C |104  D |105  E |106  F |107  G  |
| 110  H |111  I |112  J |113  K |114  L |115  M |116  N |117  O  |
| 120  P |121  Q |122  R |123  S |124  T |125  U |126  V |127  W  |
| 130  X |131  Y |132  Z |133  [ |134  \\ |135  ] |136  ^ |137  _  |
| 140  ` |141  a |142  b |143  c |144  d |145  e |146  f |147  g  |
| 150  h |151  i |152  j |153  k |154  l |155  m |156  n |157  o  |
| 160  p |161  q |162  r |163  s |164  t |165  u |166  v |167  w  |
| 170  x |171  y |172  z |173  { |174  | |175  } |176  ~ |177 DEL |
|_________________________________________________________________|
|                                                                 |
| 00 NUL| 01 SOH| 02 STX| 03 ETX| 04 EOT| 05 ENQ| 06 ACK| 07 BEL  |
| 08 BS | 09 HT | 0A NL | 0B VT | 0C NP | 0D CR | 0E SO | 0F SI   |
| 10 DLE| 11 DC1| 12 DC2| 13 DC3| 14 DC4| 15 NAK| 16 SYN| 17 ETB  |
| 18 CAN| 19 EM | 1A SUB| 1B ESC| 1C FS | 1D GS | 1E RS | 1F US   |
| 20 SP | 21  ! | 22  \" | 23  # | 24  $ | 25  % | 26  & | 27  '   |
| 28  ( | 29  ) | 2a  * | 2b  + | 2c  , | 2d  - | 2e  . | 2f  /   |
| 30  0 | 31  1 | 32  2 | 33  3 | 34  4 | 35  5 | 36  6 | 37  7   |
| 38  8 | 39  9 | 3a  : | 3b  ; | 3c  < | 3d  = | 3e  > | 3f  ?   |
| 40  @ | 41  A | 42  B | 43  C | 44  D | 45  E | 46  F | 47  G   |
| 48  H | 49  I | 4a  J | 4b  K | 4c  L | 4d  M | 4e  N | 4f  O   |
| 50  P | 51  Q | 52  R | 53  S | 54  T | 55  U | 56  V | 57  W   |
| 58  X | 59  Y | 5a  Z | 5b  [ | 5c  \\ | 5d  ] | 5e  ^ | 5f  _   |
| 60  ` | 61  a | 62  b | 63  c | 64  d | 65  e | 66  f | 67  g   |
| 68  h | 69  i | 6a  j | 6b  k | 6c  l | 6d  m | 6e  n | 6f  o   |
| 70  p | 71  q | 72  r | 73  s | 74  t | 75  u | 76  v | 77  w   |
| 78  x | 79  y | 7a  z | 7b  { | 7c  | | 7d  } | 7e  ~ | 7f DEL  |
|_________________________________________________________________|
|                                                                 |
|  0 NUL|  1 SOH|  2 STX|  3 ETX|  4 EOT|  5 ENQ|  6 ACK|  7 BEL  |
|  8 BS |  9 HT | 10 NL | 11 VT | 12 NP | 13 CR | 14 SO | 15 SI   |
| 16 DLE| 17 DC1| 18 DC2| 19 DC3| 20 DC4| 21 NAK| 22 SYN| 23 ETB  |
| 24 CAN| 25 EM | 26 SUB| 27 ESC| 28 FS | 29 GS | 30 RS | 31 US   |
| 32 SP | 33  ! | 34  \" | 35  # | 36  $ | 37  % | 38  & | 39  '   |
| 40  ( | 41  ) | 42  * | 43  + | 44  , | 45  - | 46  . | 47  /   |
| 48  0 | 49  1 | 50  2 | 51  3 | 52  4 | 53  5 | 54  6 | 55  7   |
| 56  8 | 57  9 | 58  : | 59  ; | 60  < | 61  = | 62  > | 63  ?   |
| 64  @ | 65  A | 66  B | 67  C | 68  D | 69  E | 70  F | 71  G   |
| 72  H | 73  I | 74  J | 75  K | 76  L | 77  M | 78  N | 79  O   |
| 80  P | 81  Q | 82  R | 83  S | 84  T | 85  U | 86  V | 87  W   |
| 88  X | 89  Y | 90  Z | 91  [ | 92  \\ | 93  ] | 94  ^ | 95  _   |
| 96  ` | 97  a | 98  b | 99  c |100  d |101  e |102  f |103  g   |
|104  h |105  i |106  j |107  k |108  l |109  m |110  n |111  o   |
|112  p |113  q |114  r |115  s |116  t |117  u |118  v |119  w   |
|120  x |121  y |122  z |123  { |124  | |125  } |126  ~ |127 DEL  |
|_______________________________________________________________67."
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-ASCII-chars  :insertp t)
    (message "pass non-nil for optional arg INTRP")))

;;;test-me;(mon-help-ASCII-chars)
;;;test-me;(mon-help-ASCII-chars t)
;;;test-me;(call-interactively 'mon-help-ASCII-chars)
;;;test-me;(describe-function 'mon-help-ASCII-chars)
;;;test-me;(documentation 'mon-help-ASCII-chars)

;;; ==============================
;;; CREATED: <Timestamp: #{2009-09-01T17:34:24-04:00Z}#{09362} - by MON KEY>
(defun mon-help-ISO-8859-1-chars (&optional insertp intrp)
"ISO-8859-1 Character Table: ►►►\n
 ______________________________________________________________________73.
|                                                                        |
|   0 NU    16 DL     32 SP    48 0     64 At   80 P      96 '!   112 p  |
|   1 SH    17 D1     33 !     49 1     65 A    81 Q      97 a    113 q  |
|   2 SX    18 D2     34 \"     50 2     66 B    82 R      98 b    114 r  |
|   3 EX    19 D3     35 Nb    51 3     67 C    83 S      99 c    115 s  |
|   4 ET    20 D4     36 DO    52 4     68 D    84 T     100 d    116 t  |
|   5 EQ    21 NK     37 %     53 5     69 E    85 U     101 e    117 u  |
|   6 AK    22 SY     38 &     54 6     70 F    86 V     102 f    118 v  |
|   7 BL    23 EB     39 '     55 7     71 G    87 W     103 g    119 w  |
|   8 BS    24 CN     40 (     56 8     72 H    88 X     104 h    120 x  |
|   9 HT    25 EM     41 )     57 9     73 I    89 Y     105 i    121 y  |
|  10 LF    26 SB     42 *     58 :     74 J    90 Z     106 j    122 z  |
|  11 VT    27 EC     43 +     59 ;     75 K    91 <(    107 k    123 (! |
|  12 FF    28 FS     44 ,     60 <     76 L    92 //    108 l    124 !! |
|  13 CR    29 GS     45 -     61 =     77 M    93 )>    109 m    125 !) |
|  14 SO    30 RS     46 .     62 >     78 N    94 '>    110 n    126 '? |
|  15 SI    31 US     47 /     63 ?     79 O    95 _     111 o    127 DT |
|________________________________________________________________________|
|                                                                        |
| 128 PA   144 DC   160 NS   176 DG   192 A!   208 D-   224 a!   240 d-  |
| 129 HO   145 P1   161 !I   177 +-   193 A'   209 N?   225 a'   241 n?  |
| 130 BH   146 P2   162 Ct   178 2S   194 A>   210 O!   226 a>   242 o!  |
| 131 NH   147 TS   163 Pd   179 3S   195 A?   211 O'   227 a?   243 o'  |
| 132 IN   148 CC   164 Cu   180 ''   196 A:   212 O>   228 a:   244 o>  |
| 133 NL   149 MW   165 Ye   181 My   197 AA   213 O?   229 aa   245 o?  |
| 134 SA   150 SG   166 BB   182 PI   198 AE   214 O:   230 ae   246 o:  |
| 135 ES   151 EG   167 SE   183 .M   199 C,   215 *X   231 c,   247 -:  |
| 136 HS   152 SS   168 ':   184 ',   200 E!   216 O/   232 e!   248 o/  |
| 137 HJ   153 GC   169 Co   185 1S   201 E'   217 U!   233 e'   249 u!  |
| 138 VS   154 SC   170 -a   186 -o   202 E>   218 U'   234 e>   250 u'  |
| 139 PD   155 CI   171 <<   187 >>   203 E:   219 U>   235 e:   251 u>  |
| 140 PU   156 ST   172 NO   188 14   204 I!   220 U:   236 i!   252 u:  |
| 141 RI   157 OC   173 --   189 12   205 I'   221 Y'   237 i'   253 y'  |
| 142 S2   158 PM   174 Rg   190 34   206 I>   222 TH   238 i>   254 th  |
| 143 S3   159 AC   175 'm   191 ?I   207 I:   223 ss   239 i:   255 y:  |
|______________________________________________________________________73."
  (interactive "i\nP")
  (if (or insertp intrp)
        (mon-help-function-spit-doc 'mon-help-ISO-8859-1-chars :insertp t)
    (message "pass non-nil for optional arg INTRP")))

;;;test-me;(mon-help-ISO-8859-1-chars)
;;;test-me;(mon-help-ISO-8859-1-chars t)
;;;test-me;(call-interactively 'mon-help-ISO-8859-1-chars)
;;;test-me;(describe-function 'mon-help-ISO-8859-1-chars)
;;;test-me;(documentation 'mon-help-ISO-8859-1-chars)
;;;test-me;(describe-function 'mon-help-ISO-8859-1-chars)

;;; ==============================
;;; CREATED: <Timestamp: 2009-08-13-W33-4T16:30:22-0400Z - by Stan>
;;; COURTESY: Micah Cowan  HIS: teseq.info of GNU Teseq VERSION: 1.0.0
(defun mon-help-cntl->hex->ecma-48 (&optional insertp intrp)
  "For reference, here's a table of the control characters (plus DEL). It
is based on the information from Table 1 of Ecma-48 / ISO/IEC 6429 (the
control-key representation has been added). ►►►\n
 __________________________________________________53.
|                                                    |
| Hex     Key     Name           Hex     Key    Name |
|____________________________________________________|
|                                                    |
| x00     ^@      NUL            x10     ^P      DLE |
| x01     ^A      SOH            x11     ^Q      DC1 |
| x02     ^B      STX            x12     ^R      DC2 |
| x03     ^C      ETX            x13     ^S      DC3 |
| x04     ^D      EOT            x14     ^T      DC4 |
| x05     ^E      ENQ            x15     ^U      NAK |
| x06     ^F      ACK            x16     ^V      SYN |
| x07     ^G      BEL            x17     ^W      ETB |
| x08     ^H      BS             x18     ^X      CAN |
| x09     ^I      TAB            x19     ^Y      EM  |
| x0A     ^J      LF             x1A     ^Z      SUB |
| x0B     ^K      VT             x1B     ^[      ESC |
| x0C     ^L      FF             x1C     ^\\      IS4|
| x0D     ^M      CR             x1D     ^]      IS3 |
| x0E     ^N      SO             x1E     ^^      IS2 |
| x0F     ^O      SI             x1F     ^_      IS1 |
| x7F     ^?      DEL                                |
|__________________________________________________53."
  (interactive "i\nP")
  (if (or insertp intrp)
        (mon-help-function-spit-doc 'mon-help-cntl->hex->ecma-48 :insertp t)
    (message "pass non-nil for optional arg INTRP")))

;;;test-me;(mon-help-cntl->hex->ecma-48)
;;;test-me;(mon-help-cntl->hex->ecma-48 t)
;;;test-me;(call-interactively 'mon-help-cntl->hex->ecma-48)
;;;test-me;(describe-function 'mon-help-cntl->hex->ecma-48)
;;;test-me;(documentation 'mon-help-cntl->hex->ecma-48)
;;;test-me;(describe-function 'mon-help-cntl->hex->ecma-48)

;;; ==============================
;;; COURTESY: Micah Cowan  HIS: teseq.info of GNU Teseq VERSION: 1.0.0
;;; CREATED: <Timestamp: 2009-08-13-W33-4T16:30:22-0400Z - by Stan>
(defun mon-help-cntl->hex->ecma-35 (&optional insertp intrp)
  "The Ecma-35 / ISO/IEC 2200 standard defines an escape sequence to be a
sequence of characters beginning with ESC, with a final byte in the
range 'x30'-'x7E', and any number (including zero) of intermediate
bytes in the range 'x20'-'x2F'.  The following table has been provided
as a reference for finding which characters match which codes. ►►►\n
 __________________________________________45.
|      |                                     |
|      |  x2X   x3X   x4X   x5X   x6X   x7X  |
|______|_____________________________________|
|      |                                     |
| xX0  |  SPC    0     @     P     `     p   |
| xX1  |   !     1     A     Q     a     q   |
| xX2  |   \"     2     B     R     b     r   |
| xX3  |   #     3     C     S     c     s   |
| xX4  |   $     4     D     T     d     t   |
| xX5  |   %     5     E     U     e     u   |
| xX6  |   &     6     F     V     f     v   |
| xX7  |   '     7     G     W     g     w   |
| xX8  |   (     8     H     X     h     x   |
| xX9  |   )     9     I     Y     i     y   |
| xXA  |   *     :     J     Z     j     z   |
| xXB  |   +     ;     K     [     k     {   |
| xXC  |   ,     <     L     \\     l     |   |
| xXD  |   -     =     M     ]     m     }   |
| xXE  |   .     >     N     ^     n     ~   |
| xXF  |   /     ?     O     _     o    DEL  |
|__________________________________________45."
  (interactive "i\nP")
  (if (or insertp intrp)
    	(mon-help-function-spit-doc 'stan-help-cntl->hex->ecma-35 :insertp t)
    (message "pass non-nil for optional arg INTRP")))

;;;test-me;(mon-help-cntl->hex->ecma-35)
;;;test-me;(mon-help-cntl->hex->ecma-35 t)
;;;test-me;(call-interactively 'mon-help-cntl->hex->ecma-35)
;;;test-me;(describe-function 'mon-help-cntl->hex->ecma-35)
;;;test-me;(documentation 'mon-help-cntl->hex->ecma-35)
;;;test-me;(describe-function 'mon-help-cntl->hex->ecma-35)

;;; ==============================
;;; CREATED: <Timestamp: #{2009-09-27T06:39:21-04:00Z}#{09397} - by MON>
;;;###autoload
(defun mon-help-nclose-functions (&optional insertp intrp)
  "Functions for working with nclosemacs.
See info node `(nclosemacs)Top'►►►\n
;; KNOWCESSING:
`*nclose-suggest*'
`*nclose-volunteer*'
`*nclose-knowcess*'
`*nclose-reset-session*'
`*nclose-reset-globales*'
`*nclose-reset-signs*'\n
;; PRINT NCLOSE OBJECTS/ENCYCLOPAEDIA:
`*nclose-print-wm*'
`*nclose-print-hypos*'
`*nclose-print-object*'
nn`*nclose-print-instances*'\n
;; THE ADD-TO-KB MACRO:
 (`add-to-kb'                                                         
   (`@LHS'= <PATTERN-LISP-FORM>)                                         
   (`@hypo' <HYPOTHESIS>)                                                
  [(`@RHS'= [*RHS-lisp-form*]+) *string-documentation*])               \n
<PATTERN-LISP-FORM> - (form which yields a boolean)                   
  ,---------.
  |`all-in' |                                                         
  |`some-in'|>----<'CLASS-NAME INTEGER 'AND-OR-ELISP-FORM>               
  |`oone-in'|                           |  ,-------------------------.                       
  |`none-in'|                           |  | string= string< string> | 
  `---------'                           `--| eq eql equal            | 
                                           | < > = /= >= <= + - * /  | 
                                           | and or not null '()     | 
                                           | memq memql member       | 
                                           | yes no                  | 
                                           `------------------------70
<HYPOTHESIS> - an hypothesis for the rule.\n
<RHS-LISP-FORM> - right-hand side actions+
  ,----
  | `@SET'
  |  |--+ `prop-in'
  |  |    (OBJECT-NAME PROPERTY-NAME [SCALAR-VALUE|LISP-FORM])
  |  |
  |  |--+ `member-in'
  |       (CLASS-NAME INTEGER PROPERTY-NAME [SCALAR-VALUE|LISP-FORM]) 
  `----\n
<STRING-DOCUMENTATION>\n"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-nclose-functions :insertp t)
    (message "pass non-nil for optional arg INTRP")))
;;
;;;test-me;(mon-help-nclose-functions)
;;;test-me;(mon-help-nclose-functions t)
;;;test-me;(describe-function 'mon-help-nclose-functions)

;;; ==============================
;;; CREATED: <Timestamp: Wednesday July 29, 2009 @ 11:57.11 AM - by MON KEY>
(defun mon-help-iso-8601 (&optional insertp intrp)
  "The full, extended format of ISO 8601 is as follows:\n
    1999-10-11T11:10:30,5-07:00\n
The elements are, in order:\n
   1. the year with four digits
   2. a hyphen \(omitted in the basic format\)
   3. the month with two digits
   4. a hyphen \(omitted in the basic format\)
   5. the day of month with two digits
   6. the letter T to separate date and time
   7. the hour in the 24-hour system with two digits
   8. a colon \(omitted in the basic format\)
   9. the minute with two digits
  10. a colon \(omitted in the basic format\)
  11. the second with two digits
  12. a comma
  13. the fraction of the second with unlimited precision
  14. a plus sign or hyphen \(minus\) to indicate sign of time zone
  15. the hours of the time zone with two digits
  16. a colon \(omitted in the basic format\)
  17. the minutes of the time zone with two digits\n
------------------------------
      2  4  6  8 10 12 14 16
      |  |  |  |  |  | |  |
      |  |  |  |  |  | |  |
  1999-10-11T11:10:30,5-07:00
   |    |  |  |  |  | |  |  |
   |    |  |  |  |  | |  |  |
   1    3  5  7  9 1113  15 17
------------------------------\n
The rules for omission of elements are quite simple. Elements from the time of
day may be omitted from the right and take their immediately preceding delimiter
with them. Elements from the date may be omitted from the left, but leave the
immediately following delimiter behind. When the year is omitted, it is replaced
by a hyphen. Elements of the date may also be omitted from the left, provided no
other elements follow, in which case they take their immediately preceding
delimiter with them. The letter T is omitted if the whole of the time of day or
the whole of the date are omitted. If an element is omitted from the left, it is
assumed to be the current value. \(In other words, omitting the century is really
dangerous, so I have even omitted the possibility of doing so.\) If an element is
omitted from the right, it is assumed to cover the whole range of values and
thus be indeterminate.\n
Every element in the time specification needs to be within the normal
bounds. There is no special consideration for leap seconds, although some might
want to express them using this standard.\n
A duration of time has a separate notation entirely, as follows:\n
    P1Y2M3DT4H5M6S>
    P7W\n
The elements are, in order:\n
   1. the letter P to indicate a duration
   2. the number of years
   3. the letter Y to indicate years
   4. the number of months
   5. the letter M to indicate months
   6. the number of days
   7. the letter D to indicate days
   8. the letter T to separate dates from times
   9. the number of hours
  10. the letter H to indicate hours
  11. the number of minutes
  12. the letter M to indicate minutes
  13. the number of seconds
  14. the letter S to indicate seconds\n
or for the second form, usually used alone\n
   1. the letter P to indicate a duration
   2. the number of weeks
   3. the letter W to indicate weeks\n
Any element \(number\) may be omitted from this specification and if so takes its
following delimited with it. Unlike the absolute time format, there is no
requirement on the number of digits, and thus no requirement for leading zeros.\n
A period of time is indicated by two time specifications, at least one of which
has to be absolute, separated by a single solidus \(slash\), and has the general
forms as follows:\n
    start/end
    start/duration
    duration/end\n
the end form may have elements of the date omitted from the left with the
assumption that the default is the corresponding value of the element from the
start form. Omissions in the start form follow the normal rules.\n
The standard also has specifications for weeks of the year and days of the week,
but these are used so rarely and are aesthetically displeasing so are gracefully
elided from the presentation.\n
---
Source: Erik Naggum's \"The Long, Painful History of Time\"
\(URL `http://naggum.no/lugm-time.html'\).\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-iso-8601 :insertp t)
    (message "pass non-nil for optional arg INTRP")))
;;
;;;test-me;(mon-help-iso-8601)
;;;test-me;(mon-help-iso-8601 t)
;;;test-me;(call-interactively 'mon-help-iso-8601)
;;;test-me;(describe-function 'mon-help-iso-8601)

;;; ==============================
;;; <Timestamp: Saturday May 23, 2009 @ 04:03.59 PM - by MON KEY>
(defun mon-help-info-incantation (&optional insertp intrp)
  "To reference an info node in a docstring use the idiom:\n
\"info node `\(elisp\)Documentation Tips\'\" <--Without the \"_\" dbl-quotes.\n
To jump to an info node with an elisp expression:
\(info \"\(elisp\)Documentation Tips\"\) <--With the \" \" dbl-quotes!. ►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-info-incantation :insertp t)
    (message "pass non-nil for optional arg INTRP")))

;;;test-me;(mon-help-info-incantation t)
;;;test-me;(mon-help-info-incantation)
;;;test-me;(call-interactively 'mon-help-info-incantation)
;;;test-me;(describe-function 'mon-help-info-incantation)

;;; ==============================
(defun mon-help-tar-incantation (&optional insertp intrp)
  "To help me remember how to do a tar.gz on a directory.
Because I never can remember tar's flags.
w32 NOTE(s):\nOn w32 with gnuwin32 to unzip use `gzip.exe -d'
On w32 with gnuwin32 to pipe a tar to gz on w32 use `bsdtar.exe xvzf'.\n
INCANTATION:
tar -czvf dir-name.tar.gz dir-name \n ►►►"
(interactive "i\nP")
(if (or insertp intrp)
    (mon-help-function-spit-doc 'mon-help-tar-incantation :insertp t)
  (message "pass non-nil for optional arg INTRP")))

;;;test-me;(mon-help-tar-incantation t)
;;;test-me;(mon-help-tar-incantation)
;;;test-me;(call-interactively 'mon-help-tar-incantation)
;;;test-me;(describe-function 'mon-help-tar-incantation)

;;; ==============================
(defun mon-help-rename-incantation (&optional insertp intrp)
  "Insert the rename idiom for BASH renaming.
# IDIOM:
# for f in *FILENAME; do
#  base=`basename $f *FILENAME` #<-- note backtick!
# mv $f $base.NEWNAME
# done

# EXAMPLE:
for f in *.html.tmp; do
 base=`basename $f .html.tmp`
 mv $f $base.html
done \n ►►►"
(interactive "i\nP")
(if (or insertp intrp)
    (mon-help-function-spit-doc 'mon-help-rename-incantation :insertp t)
  (message "pass non-nil for optional arg INTRP")))

;;;test-me(mon-help-rename-incantation t)
;;;test-me(mon-help-rename-incantation)
;;;test-me;(call-interactively 'mon-help-rename-incantation)
;;;test-me;(describe-function 'mon-help-rename-incantation)

;;; ==============================
;;; CREATED: <Timestamp: Sunday May 31, 2009 @ 04:35.09 PM - by MON KEY>
(defun mon-help-install-info-incantation (&optional insertp intrp)
  "Inserts the install-info spell. Esp. needed on w32 as I can never remember it.
M-x cygwin-shell\install-info  info-file  /\\\"Program Files\\\"/Emacs/emacs/info/dir
M-x shell\install-info  info-file  \"/usr/info/dir\". ►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
    (let ((info-incantation
           (cond
            (IS-MON-P-W32
             "M-x cygwin-shell install-info info-file /\"Program Files\"/Emacs/emacs/info/dir")
            (IS-MON-P-GNU
             "M-x shell\install-info  info-file  \"/usr/info/dir\"")
          ((eq system-type 'windows-nt)
             "\nM-x cygwin-shell install-info info-file /\"Program Files\"/Emacs/emacs/info/dir")
          ((or (eq system-type 'gnu/linux)  (eq system-type 'linux))
           "\nM-x shell\install-info  info-file  \"/usr/info/dir\"")
          (t "\nM-x shell\install-info  info-file  \"/path/to/info/dir\""))))
      (princ info-incantation (current-buffer)))
    (message "pass non-nil for optional arg INTRP")))

;;;test-me;(mon-help-install-info-incantation t)
;;;test-me;(mon-help-install-info-incantation t)
;;;test-me;(call-interactively 'mon-help-tar-incantation)
;;;test-me;(describe-function 'mon-help-tar-incantation)

;;; ==============================
;;; CREATED: <Timestamp: Tuesday June 23, 2009 @ 11:37.05 AM - by MON KEY>
(defun mon-help-format-width (&optional insertp intrp)
  "Idiom for `format' control string to specify padding using the width flag.\n
See info node `(elisp)Formatting Strings'\n
EXAMPLE:
  \(let \(\(x 'test\) \(y \"\"\)\)
     \(format \"This is a %-9s.\\nThis is a %9s.\\nThis is a %s %4s.\" x x x y\)\)
;; =>This is a test     .
;;   This is a      test.
;;   This is a test     .\n ►►►"
(interactive "i\nP")
(if (or insertp intrp)
    (mon-help-function-spit-doc 'mon-help-format-width :insertp t)
  (message "pass non-nil for optional arg INTRP")))

;;;test-me;(mon-help-format-width t)
;;;test-me;(mon-help-format-width)
;;;test-me;(call-interactively 'mon-help-format-width)
;;;test-me;(describe-function 'mon-help-format-width)

;;; ==============================
;;; FIX-ME: This function works... though it is ugly as hell... sorry Andy.
;;; NOTE: This fails: (elisp-index-search "elisp-index-search")
;;; COURTESY: Andy Stewart <lazycat.manatee@gmail.com>
;;; (URL `http://www.emacswiki.org/emacs/lazycat-toolkit.el')
;;; MODIFICATIONS: <Timestamp: #{2009-08-26T17:36:47-04:00Z}#{09353} - by MON KEY>
(defun mon-index-elisp-symbol ()
  "Find TOPIC in the indices of the Emacs Lisp Reference Manual.\n
EXAMPLE:\n\(elisp-index-search \"setq\") ►►►"
  (interactive)
  (let ((topic (symbol-name (symbol-at-point))))
    (setq topic (read-string (format "Subject to look up <%s>: " topic) nil nil topic))
    (info "elisp")
    (Info-index topic)))

;;;test-me;(elisp-index-search "setq")

;;; ==============================
;;; CREATED: <Timestamp: Thursday July 02, 2009 @ 11:50.50 AM - by MON KEY>
(defvar *w32-env-variables-alist* nil
  "*List of environment variables available in w32.
CALLED-BY: `mon-help-w32-env'.")
;;
(when (not (bound-and-true-p *w32-env-variables-alist*))
  (setq *w32-env-variables-alist*
 '((ALLUSERSPROFILE %ALLUSERSPROFILE%
    "Local returns the location of the All Users Profile.")
   (APPDATA %APPDATA%
    "Local returns the location where applications store data by default.")
   (CD %CD%
    "Local returns the current directory string.")
   (CMDCMDLINE %CMDCMDLINE%
    "Local returns the exact command line used to start the current cmd.exe")
   (CMDEXTVERSION %CMDEXTVERSION%
    "System returns the version number of the current Command Processor Extensions.")
   (COMPUTERNAME %COMPUTERNAME%
    "System returns the name of the computer.")
   (COMSPEC %COMSPEC%
    "System returns the exact path to the command shell executable.")
   (DATE %DATE%
    "System returns the current date. This variable uses the same format as the date
/t command. Cmd.exe generates this variable. For more information about the date
command, see the Date command.")
   (ERRORLEVEL %ERRORLEVEL%
    "System returns the error code of the most recently used command. A non-0 value
usually indicates an error.")
   (HOMEDRIVE %HOMEDRIVE%
    "System returns which local workstation drive letter is connected to the user's
home directory. This variable is set based on the value of the home
directory. The user's home directory is specified in Local Users and Groups.")
   (HOMEPATH %HOMEPATH%
    "System returns the full path of the user's home directory. This variable is set
based on the value of the home directory. The user's home directory is specified
in Local Users and Groups.")
   (HOMESHARE %HOMESHARE%
    "System returns the network path to the user's shared home directory. This
variable is set based on the value of the home directory. The user's home
directory is specified in Local Users and Groups.")
   (LOGONSERVER %LOGONSERVER%
    "Local returns the name of the domain controller that validated the current logon
session.")
   (NUMBER_OF_PROCESSORS %NUMBER_OF_PROCESSORS%
    "System specifies the number of processors installed on the computer.")
   (OS %OS%
    "System returns the OS name. Windows XP and Windows 2000 display the OS as
Windows_NT.")
   (PATH %PATH%
    "System specifies the search path for executable files")
   (PATHEXT %PATHEXT%
    "System returns a list of the file extensions that the OS considers to be
executable.")
   (PROCESSOR_ARCHITECTURE %PROCESSOR_ARCHITECTURE%
    "System returns the processor's chip architecture. Values: x86, IA64.")
   (PROCESSOR_IDENTIFIER %PROCESSOR_IDENTIFIER%
    "System returns a description of the processor.")
   (PROCESSOR_LEVEL %PROCESSOR_LEVEL%
    "System returns the model number of the computer's processor.")
   (PROCESSOR_REVISION %PROCESSOR_REVISION%
    "System returns the revision number of the processor.")
   (PROMPT %PROMPT%
    "Local returns the command-prompt settings for the current interpreter. Cmd.exe
generates this variable.")
   (RANDOM %RANDOM%
    "System returns a random decimal number between 0 and 32767. Cmd.exe generates
this variable.")
   (SYSTEMDRIVE %SYSTEMDRIVE%
    "System returns the drive containing the Windows root directory (i.e., the system
root.")
   (SYSTEMROOT %SYSTEMROOT%
    "System returns the location of the Windows root directory.")
   (TEMP %TEMP%
    "System and User return the default temporary directories for applications that
are available to users who are currently logged on. Some applications require
TEMP and others require TMP.")
   (TMP %TMP%
    "System and User return the default temporary directories for applications that
are available to users who are currently logged on. Some applications require
TEMP and others require TMP.")
   (TIME %TIME%
    "System returns the current time. This variable uses the same format as the time
/t command. Cmd.exe generates this variable. For more information about the time
command. See also; the Time command.")
   (USERDOMAIN %USERDOMAIN%
    "Local returns the name of the domain that contains the user's account.")
   (USERNAME %USERNAME%
    "Local returns the name of the user currently logged on.")
   (USERPROFILE %USERPROFILE%
    "Local returns the location of the profile for the current user.")
   (WINDIR %WINDIR%
    "System returns the location of the OS directory.")
   ("Program Files" %PROGRAMFILES%
    "Returns the location of the default install directory for applications."))))

;;;test-me;(assoc 'TMP *w32-env-variables-alist*)
;;;test-me;(car (assoc 'TMP *w32-env-variables-alist*))
;;;test-me;(cadr (assoc 'TMP *w32-env-variables-alist*))
;;;test-me;(caddr (assoc 'TMP *w32-env-variables-alist*))
;;;test-me;(assoc "Program Files" *w32-env-variables-alist*)
;;;test-me;(mapcar (lambda (x) (car x))*w32-env-variables-alist*)
;;;(unintern '*w32-env-variables-alist*)

;;; ==============================
;;; CREATED: <Timestamp: Thursday July 02, 2009 @ 11:50.50 AM - by MON KEY>
(defun mon-help-w32-env (&optional insertp intrp)
  (interactive "i\nP")
(let ((print-var-list
       (mapconcat
        (lambda (x)
          (let* ((k (car x))
                 (v (cdr (assoc k *w32-env-variables-alist*)))
                 (v-var (car v))
                 (v-doc (cadr v)))
            (format "-\n%s %s \n%s" k v-var v-doc)))
        *w32-env-variables-alist* "\n")))
  (plist-put
   (symbol-plist 'mon-help-w32-env)
   'function-documentation
   (concat
    "Environment variables available in w32.
Environmental variables accessible as alist in var `*w32-env-variables-alist*'.
Called interactively with Prefix arg non-nil prints to current-buffer.
List of the environment variables callable in w32.\n
Sourced from: \(URL `http://windowsitpro.com/article/articleid/23873/')\n
and \(URL `http://technet.microsoft.com/en-us/library/bb490954.aspx'\)\n
EXAMPLE: Open a cmd prompt and type echo %appdata% which should return
the full path to your profile's Application Data directory.
If calling from a batch file remember to quote the thusly %variable%
or set VARIABLE=value. ►►►"
    print-var-list)))
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-w32-env :insertp t)
    (message "pass non-nil for optional arg INTRP")))

;To see the current value of a particular variable, use:

;;;test-me;(mon-help-w32-env t)

;;; ==============================
;;; COURTESY: Aaaron Hawley  HIS:
;;; (URL `http://www.emacswiki.org/emacs/Reference_Sheet_by_Aaron_Hawley_source')
;;; MODIFICATIONS: <Timestamp: Wednesday June 17, 2009 @ 11:31.47 AM - by MON KEY>
(defvar *reference-sheet-help-A-HAWLEY* nil)
;;
(when (not (bound-and-true-p *reference-sheet-help-A-HAWLEY*))
  (setq *reference-sheet-help-A-HAWLEY*
"#:START:REFERENCE-SHEET#

open:
emacs& RET -- or click a graphical icon
emacs -nw RET -- open in terminal, not in a window
q -- clear splash screen

exit:
C-x C-c -- save buffers and quit
C-] -- recursive edit

cancel:
C-g -- a command
C-M-c -- recursive edit
M-x top-level -- all recursive edits

recursive edit:
C-] -- exit recursive edit
C-M-c -- cancel recursive edit
M-x top-level -- cancel all recursive edits

customize:
C-x C-f ~/.emacs RET -- Emacs initialization file
M-x customize -- main menu
M-x customize-variable -- variable
M-x customize-apropos -- search
M-x customize-mode -- mode
M-x global-set-key -- define key binding
M-x local-set-key -- define key binding for current buffer
M-x display-time -- show clock, system load and email flag in mode line
M-x display-battery-mode -- show system power
M-x size-indication-mode -- show size in mode line
M-x column-number-mode -- show column number in mode line
M-x ruler-mode -- add a ruler to the current buffer's window
M-x auto-revert-mode -- update file if changes on disk
M-x global-auto-revert-mode -- update any buffer's file
M-x menu-bar-mode -- toggle existence of drop-down menu
M-x tool-bar-mode -- toggle existence of clickable tool bar
M-x scroll-bar-mode -- toggle scroll bar
M-x toggle-scroll-bar -- toggle scroll bar in current frame
M-x blink-cursor-mode -- toggle blinking of cursor

menu:
M-` -- text interaction with drop-down menu
F10 -- same as previous
M-x menu-bar-mode -- toggle existence of drop-down menu
M-x tool-bar-mode -- toggle existence of clickable tool bar

help:
C-h ? -- menu
C-h C-h -- menu
SPC -- scroll down menu
DEL -- scroll up menu
q -- close menu
C-h t -- tutorial
C-h r -- Emacs info manual \(see \"info\" below\)
C-h F -- Emacs FAQ
C-h c <KEY> -- what is command for KEY
C-h k <KEY> -- describe command for KEY
C-h w <COMMAND> RET -- where is key binding for COMMAND
C-h m -- show current modes
C-h b -- show current key bindings
C-h a <SEARCH> <TERMS> RET -- list commands matching SEARCH TERMS
C-h f <FUNCTION> RET -- describe FUNCTION
C-h v <VARIABLE> RET -- describe and show values for VARIABLE
TAB -- forward to cross-reference link
M-TAB -- backward cross-reference link
S-TAB -- backward cross-reference link
RET -- follow cross-reference
C-c C-c -- follow cross-reference
C-c C-b -- go back
q -- quit

info help:
C-h i -- open directory of manuals
C-h r -- open Emacs manual
q -- close
t -- table of contents \(menu\)
d -- back to directory
m <ENTRY> -- visit menu ENTRY in table of contents
TAB -- forward to cross-reference link
M-TAB -- backward cross-reference link
S-TAB -- backward cross-reference link
RET -- follow link
l -- back to last visited page
r -- forward to last visited page
C-v -- scroll down
SPC -- scroll down
M-v -- scroll up
DEL -- scroll up
b -- scroll up
n -- next node
p -- previous node
i <WORD> RET -- look for WORD in current manual's index
i RET -- go to index node
s <PATTERN> -- search forward for regular expression PATTERN
S <PATTERN> -- case-insensitive search for regular expression PATTERN
C-s <SEARCH> -- forward to SEARCH \(see \"search\" below\)
C-r <SEARCH> -- reverse to SEARCH
M-n -- make a duplicate buffer in other window \(see \"window\" below\)

minibuffer:
M-p -- previous input
M-n -- recent input
TAB -- complete name of buffer, file, symbol \(see \"completion\" below\)
C-i -- same as previous
M-r -- search for previous input
M-s -- search for previous input
C-h e -- show recently echoed messages
C-g -- exit

completion:
TAB -- complete name of buffer, file, function, variable, ...
SPC -- completion, unless a file
? -- list completions
M-v -- go to \"completions\" buffer
<right> -- next completion
<left> -- previous completion
RET -- select completion
ESC ESC ESC -- exit back to minibuffer

mode:
C-h m -- help with current \(see \"help\" above\)
M-x text-mode -- for writing
M-x fundamental-mode -- a simple default
M-x normal-mode -- change back to what Emacs thought it was
M-x customize-mode -- customize current \(see \"customize\" above\)

file:
C-x C-f -- open
C-x C-f -- new
M-x make-directory RET RET -- including parent directories
C-x C-v -- close current and open another
C-x C-s -- save
C-x s -- prompt to save any buffer that has been modified
C-x C-f M-p RET -- open previously saved or opened
C-x C-w <PATH> RET -- save current to PATH
C-x C-w <DIRECTORY> RET -- save to DIRECTORY using file or buffer name
C-x C-q -- toggle as read only
C-x C-f archive.tar RET -- list contents of archive
C-x C-f file.gz RET -- open compressed Gzip
C-x C-f file.zip RET -- list contents of zip file
M-x rename-file -- rename current
M-x view-file -- \(see \"read only\" below\)
<insert> -- change whether to use overwrite or switch back to insert mode
M-x overwrite-mode -- same as previous
M-x binary-overwrite-mode -- edit file as literal bytes
C-x d M-p RET R -- rename previously saved or opened
C-x i -- insert other file into current buffer
M-x write-region -- save region
C-x h M-x write-region -- save buffer once to alternate
M-x find-file-at-point -- open file name at point
M-x revert-buffer -- restore buffer with file on disk
M-x recover-file -- recover auto-save data after a crash
M-x recover-session -- recover all files with auto-save data
M-x size-indication-mode -- show size in mode line
M-x auto-revert-mode -- update file if changes on disk
M-x global-auto-revert-mode -- update any buffer's file
M-x auto-revert-tail-mode -- update end of file with changes on disk

buffer:
M-< -- beginning of buffer
M-> -- end of buffer
C-x h -- mark
C-x k <BUFFER> RET -- kill BUFFER
C-x k RET -- kill current
C-x b RET -- switch to last buffer
C-x b <BUFFER> RET -- switch to BUFFER or make new BUFFER
C-x 4 b -- switch to a buffer in other window \(see \"window\" below\)
C-x 4 C-o -- show a buffer in other window \(see \"window\" below\)
C-x C-b -- list all \(see \"buffer menu\" below\)
M-x bury-buffer -- avoid switching to current buffer
C-x b M-p -- switch to previously switched buffer
C-x C-s -- save current contents to file on disk
C-x s -- save modified files
M-x rename-buffer RET <NAME> -- rename current to NAME
M-x rename-uniquely -- remove \"<X>\" suffix from buffer name if possible
M-x revert-buffer -- restore contents with file on disk \(see \"undo\" below\)
M-x erase-buffer -- delete everything \(see \"delete\" below\)
M-x clone-indirect-buffer -- open an indirect buffer based on current
C-x 4 c -- open an indirect buffer but in another window
C-h f car RET C-x o M-x clone-buffer RET C-h f cdr RET -- compare two functions

read only:
C-x C-r <FILE> RET -- open FILE as read only
C-x C-q -- toggle write status
M-x view-mode -- view mode for current buffer
M-x view-file <FILE> RET -- open FILE in view mode
M-x view-buffer -- view mode for other buffer
SPC -- scroll down
DEL -- scroll up
h -- view mode help
q -- turn off view mode
M-x normal-mode -- turn off view mode

window:
C-v -- scroll down
M-v -- scroll up
C-M-v -- scroll other window down
M-< -- beginning of buffer
M-> -- end of buffer
M-x beginning-of-buffer-other-window -- beginning of other buffer
M-x end-of-buffer-other-window -- end of other buffer
M-r -- move to first column of center line in display
C-0 M-r -- move to first column of first displayed line
M-- M-r -- move to first column of last displayed line
C-4 M-r -- move to first column of fourth displayed line
C-u - 3 M-r -- move to first column of third to last displayed line
C-x 2 -- split vertically in two
C-x o -- switch between windows
C-x 4 b -- switch to a buffer in other window
C-x 4 C-o -- show a buffer in other window
C-x 0 -- close current
C-x 1 -- close all others leaving current
C-x 4 f -- open file in other
C-x - -- shrink to fit text
C-x + -- equalize window heights
C-u 5 C-x ^ -- enlarge 5 lines taller
M- 5 C-x ^ -- shrink 5 lines shorter
C-x 3 -- split horizontally
C-u 5 C-x } -- enlarge 5 columns wider
M- 5 C-x } -- shrink 5 columns narrower
C-x < -- scroll horizontally right
C-x > -- scroll horizontally left
M-x toggle-truncate-lines -- change if long lines fold or are truncated

buffer menu:
C-x C-b -- list
C-u C-x C-b -- list only buffers associated with files
SPC -- move down
n -- move down
C-n -- move down
p -- move up
C-p -- move up
% -- toggle current as read only \(see \"read only\" above\)
? -- show modes for current
g -- update list
T -- toggle list to buffers associated with files
C-o -- view current in other window \(see \"window\" above\)
RET -- view current in this window
e -- goto current in this window
f -- goto current in this window
1 -- goto current in only 1 window
2 -- goto current in only 1 window
V -- open current buffer in View mode \(see \"read only\" above\)
b -- bury current \(see \"buffer\" above\)
m -- mark current and move down
C-d -- mark to delete current and move up
d -- mark to delete current and move down
k -- mark to delete current and move down
C-k -- mark to delete current and move down
x -- execute marks
q -- quit

redisplay:
C-l -- with line at center of window
C-0 C-l -- with current line at top of window
M-- C-l -- with current line at bottom of window
C-u -1 C-l -- same as previous
C-M-l -- try to make the top of the current function visible in the window
C-M-l C-M-l -- with current line at top of window

command:
C-h l -- show recently typed keys
C-h e -- show recently echoed messages
C-x z -- repeat last command
C-x M-ESC -- edit and re-evaluate last command as Emacs Lisp
C-x M-: -- same as previous
M-x command-history -- show recently run commands
x -- run command at line in history

iterative command:
C-u -- repeat next command 4 times
M-- -- next command once in opposite direction
C-u 8 -- repeat next command 8 times
M-8 -- repeat next command 8 times
C-8 -- repeat next command 8 times
C-u 8 C-u -- repeat next command 8 times
C-8 -- repeat next command 8 times
M-- 3 -- repeat next command 3 times in opposite direction
C-u -3 -- repeat next command 3 times in opposite direction
C-u C-u -- repeat next command 16 times
C-u C-u C-u -- repeat next command 64 times
C-u 369 C-u 0 -- insert 369 zeros

non-iterative command:
C-u -- toggle behavior of next command
M-- -- toggle behavior of next command with negative value

macro:
C-x \( -- start recording macro
F3  -- same as previous
C-x \) -- finish recording macro
F4  -- same as previous
C-x e -- finish recording macro and run macro
C-x e -- run last macro
C-x C-k r -- go to each line in region and run last macro - `apply-macro-to-region-lines'
C-x C-k n -- name last macro
M-x <MACRO> -- run macro MACRO
C-x C-k b -- bind last macro to a key `kmacro-bind-to-key'
C-x C-k e -- edit last macro - `edit-kbd-macro'
C-x C-k e M-x <MACRO> -- edit named MACRO
C-x C-k e C-h l -- edit and make recently typed keys
C-x C-f ~/.emacs RET M-x insert-kbd-macro -- save macro

key:
C-h l -- show recently typed keys
M-x global-set-key -- set for all buffers
M-x local-set-key -- set for current buffer
C-q -- insert next character literally
C-q TAB -- insert literal tab character
C-q C-j -- insert literal newline
C-q C-m -- insert literal carriage return
C-q C-l -- insert literal form feed \(page delimiter\)
C-x @ c -- modify next key with Control
C-x @ m -- modify next key with Meta
C-x @ S -- modify next key with Shift
C-x @ h -- modify next key with Hyper
C-x @ s -- modify next key with Super
C-x @ a -- modify next key with Alt

undo:
C-x u -- undo, repeat to further undo
C-_ -- undo, repeat to further undo
C-/ -- undo, repeat to further undo
C-/ C-g C-/ -- undo, then redo
C-/ C-/ C-g C-/ C-/ -- undo, undo, then redo, redo
M-x revert-buffer -- restore buffer with file on disk
M-x buffer-disable-undo -- turn off for current buffer
M-x buffer-enable-undo -- turn on for current buffer

search:
C-s <MATCH> -- forward to end of MATCH
C-r <MATCH> -- reverse to front of MATCH
C-r C-s <MATCH> -- forward to end of MATCH
C-s C-r <MATCH> -- reverse to front of MATCH
C-s <MATCH> C-s -- forward to end of second MATCH
C-r <MATCH> C-r -- reverse to front of second MATCH
DEL -- if not at first match, go to previous match
DEL -- if at first match, delete character from search string
C-M-w -- always delete character from search string
C-s <MATCH> C-s C-r -- forward to start of second MATCH
C-r <MATCH> C-r C-s -- reverse to end of second MATCH
C-s <MATCH> C-s C-s DEL -- forward to end of second MATCH
C-r <MATCH> C-r C-r DEL -- reverse to start of second MATCH
RET -- finish search
C-g -- cancel search if current search is successful
C-g -- undo search to last successful search
C-s C-j -- search for newline
C-s C-q C-m -- search for carriage return
C-s C-M-y -- search for current character
C-s C-M-y C-M-y -- search for next two characters
C-s C-M-y C-M-y DEL -- search for current character
C-s C-w -- search for rest of current word
C-s C-w C-w -- search for next two words
C-s C-w C-w DEL -- search for rest of current word
C-s C-y -- search for rest of current line
C-s C-y DEL -- undo search for rest of current line
C-s M-y -- search for last killed text \(see \"kill\" below\)
C-s M-y -- search for last killed text \(see \"kill\" below\)
C-s M-p -- show previous search
C-s M-n -- show oldest stored search
C-s M-TAB <BEGINNING>  -- complete for BEGINNING of stored searches
C-s C-s -- resume last search backward
C-r C-r -- resume last search forward
M-e -- edit search
M-r -- toggle regular expression search
M-c -- toggle case-sensitivity of search
M-% -- search, query, and replace \(see \"replace\" below\)
C-s <SEARCH> M-% <REPLACE> -- interactive query SEARCH and REPLACE
M-x isearch-toggle-case-fold -- change case-sensitivity of all searches
C-h f isearch-forward RET -- help

non-interactive search:
C-s RET -- forward case-sensitive
C-r RET -- backward case-sensitive
C-s RET C-w -- forward word-based ignoring punctuation and whitespace
C-r RET C-w -- backward word-based ignoring punctuation and whitespace

regular expression:
C-M-s -- search forward
C-M-r -- search reverse
M-r -- toggle off regexp
C-M-s C-s -- repeat last regexp forward
C-M-r C-s -- repeat last regexp backward
C-r -- suspend replacement and editing buffer
C-M-c -- resume query and replace \(see \"recursive edit\" above\)
C-M-% -- regexp replace
C-M-s <SEARCH> M-% <REPLACE> -- interactive query replace \(see \"replace\" below\)
M-x occur -- show matches in buffer
M-x count-matches -- count matches
M-x flush-lines -- delete matching lines
M-x keep-lines -- keep matching lines delete the rest

replace:
M-% -- search, query, and replace
C-M-% -- search regular expression, query, and replace
M-% RET -- resume last
C-M-% RET -- resume last as regexp
C-s <SEARCH> M-% <REPLACE> -- interactive
C-M-s <SEARCH> M-% <REPLACE> -- interactive with regexp
y -- replace one and go to next
SPC -- replace one and go to next
, -- replace but don't move
n -- skip
DEL -- skip
^ -- previous
! -- replace all
e -- edit replacement
C-r -- suspend to edit buffer
C-M-c -- finish edit and resume
RET -- stop
q -- stop
C-x d *.c RET Q int RET long -- replace \"long\" for \"int\" in .c files

delete:
C-d -- current character
C-u C-d -- next 4 characters
C-u 8 C-d -- next 8 characters
C-u C-u -- next 16 characters
DEL -- character backwards \(backspace\)
M-- C-d -- same as previous
C-u C-u C-u DEL -- previous 64 characters
C-u 5 DEL -- previous 5 characters
M-x delete-region -- region
M-x erase-buffer -- entire buffer

kill \(cut\):
C-SPC C-f C-w -- character
M-d -- word
C-k -- to end of line
C-0 C-k -- beginning of line
C-S-DEL -- entire line
C-1 C-k -- line including newline
M-- C-k -- to beginning of previous line
C-u C-k -- next 4 lines
M-k -- sentence \(see \"sentence\" below\)
C-w -- region \(see \"region\" below\)
M-w -- region but don't delete \(copy\)
M-d -- word \(see \"word\" below\)
C-M-k -- sexp \(see \"sexp\" below\)
M-DEL -- sexp backwards
C-M-w C-w -- append to next
C-M-w -- append to next
C-M-w C-w -- region appending to previous
C-M-w M-w -- region appending to previous, but don't delete \(copy\)
C-M-w C-k -- line appending to previous
C-M-w M-d -- word appending to previous
C-M-w M-k -- sentence appending to previous
C-M-w M-kill-paragraph -- paragraph appending to previous
C-M-w C-M-k -- sexp appending to previous
C-M-w M-DEL -- sexp backward appending to previous
M-z -- delete everything to a character
C-1 M-z -- same as previous
M-- M-z -- delete everything to a character backwards
C-u -1 M-z -- same as previous
C-u 3 M-z -- delete everything to 3rd occurrence of a character

yank \(paste\):
C-y -- the last kill sequence
M-y -- the 2nd to last kill sequence

mark:
C-SPC -- set at current point
C-@ -- set at current point
C-x C-x -- toggle between current point and mark
C-x C-SPC -- move to last set mark
C-x C-@ -- move to last set mark
C-x h -- buffer
M-h -- paragraph
C-M-h -- function
C-x C-p -- page separated by form feed
M-@ -- word
C-M-@ -- sexp \(see \"sexp\" below\)
C-M-SPC -- sexp
C-SPC C-SPC -- temporarily turn on transient mark mode
M-x transient-mark-mode -- turn on transient mark mode

region:
C-SPC -- set end-point of region
C-@ -- set end-point of region
C-w -- kill
M-w -- kill but don't delete \(copy\)
M-= -- count lines and characters
C-x n n -- narrow
C-x n w -- widen

whitespace:
SPC -- insert space
TAB -- indent or insert tab \(see \"indent\" below\)
C-q TAB -- insert literal tab character
C-q C-l -- insert page separator
C-q 0 RET -- insert null
M-SPC -- remove all whitespace at point except one space
M-x delete-trailing-whitespace -- remove at end of all lines in buffer
C-a M-x delete-whitespace-rectangle -- remove at beginning of all lines in region
C-x h M-x delete-whitespace-rectangle -- remove at beginning of all lines

indent:
TAB -- line with mode-specific rules
C-i -- line with mode-specific rules
M-m -- go to indentation at beginning of line
M-s -- center line
C-M-\\ -- region with mode-specific rules
C-x h C-M-\\ -- buffer
M-h C-M-\\ -- paragraph
C-M-h C-M-\\ -- defun
C-x C-p C-M-\\ -- page
C-M-SPC C-M-\\ -- sexp
C-x TAB -- region by one column
C-u 5 C-x TAB -- region by 5 columns
C-u - 2 C-x TAB -- region by 2 columns less
M-x tabify -- convert spaces to tabs

newline:
RET -- one
C-m -- one
C-j -- one and indent
C-o -- one below current and indent
C-M-o -- keep text following point at same column
C-u 3 RET -- three
C-u 3 C-m -- three
C-u 3 C-j -- three and indent
C-u 3 C-o -- three below current and indent
C-u 3 C-M-o -- move text following point at same column three lines down

line:
C-n -- next
C-p -- previous
C-a -- beginning
M-g g -- goto
M-g M-g -- goto
C-e -- end
C-k -- kill to end
C-0 C-k -- kill to beginning
C-a C-k -- kill from beginning to end
C-S-DEL -- kill from beginning to end including newline
C-a C-k C-k -- same as previous
C-1 C-k -- kill to end including newline
C-u C-k -- kill next 4
C-2 C-k -- kill next 2
M-- C-k -- kill to beginning of previous
M-^ -- merge current line with previous
C-u M-^ -- merge next line with current
C-x C-o -- when not empty line, remove all empty lines below current
C-x C-o -- when only empty line, remove all empty lines
C-x C-o -- when empty, remove all but one empty lines
M-= -- count lines in region
M-x occur -- show lines matches in buffer \(see \"occur\" below\)
M-x count-matches -- count matches
M-x flush-lines -- delete matching lines
M-x keep-lines -- keep matching lines delete the rest
M-x what-line -- display number
C-x RET f unix RET -- change file to UNIX style line endings
C-x RET f dos RET -- change file to DOS
C-x RET f dos RET -- change file to Mac
M-x line-number-mode -- show line number in mode line
M-x toggle-truncate-lines -- change if long lines fold or are truncated

character:
C-f -- forward
C-b -- backward
C-d -- delete
M-SPC -- remove all whitespace at point except one space
M-x describe-char -- properties
C-q 0 RET -- insert null
C-q 40 RET -- insert space using octal value 40
M-x set-variable RET read-quoted-char-radix 16 -- use hex for C-q
C-q 20 RET -- insert space using hex value 20
M-x set-variable RET read-quoted-char-radix 10 -- use decimal for C-q
C-q 32 RET -- insert space using decimal value 32
C-u 8 C-q 0 RET -- insert 8 null characters

word:
M-f -- forward
M-b -- backward
M-d -- kill forward
C-DEL -- kill backward
M-DEL -- kill backward
M-t -- transpose
M-@ -- mark
C-u 100 M-@ -- mark next 100
M-- 3 M-@ -- mark previous 3

capitalization:
M-l -- lowercase next word
M-- M-l -- lowercase previous word
C-u M-l -- lowercase next 4 words
M-u -- uppercase next word
M-- M-u -- uppercase previous word
C-u 2 M-l -- uppercase next 2
M-c -- capitalize next
M-- M-c -- capitalize previous
C-u 2 M-c -- capitalize next 2
C-x C-l -- lowercase region
C-x C-u -- uppercase region
M-x capitalize-region -- capitalize region

sentence:
M-a -- beginning
M-e -- end
M-k -- kill

paragraph:
M-} -- forward
M-{ -- backward
M-h -- mark
M-g -- fill
C-u M-g -- fill and full justify
M-x fill-region -- fill all in region
C-u 72 C-x f -- set fill column to 72
M-x kill-paragraph -- kill to end
M-{ M-x kill-paragraph -- kill
M-x transpose-paragraphs -- transpose
M-S -- center
M-x sort-paragraphs -- alphabetically
M-x reverse-region -- sort
M-x paragraph-indent-text-mode -- expect leading space rather than empty lines
M-x auto-refill-mode -- automatically fill at the end of the line
M-x refill-mode -- automatically fill entire paragraph after each edit

page:
C-q C-l -- insert separator
C-x ] -- forward page
C-x l -- count lines
C-x n p -- narrow
C-x n w -- widen
M-x sort-pages -- alphabetically
M-x what-page -- display number

sexp \(parenthetical expressions\):
M-\( -- insert opening and closing parentheses
C-M-f -- move to the next
C-M-b -- move backward
C-M-d -- move down into the expression
M-x up-list -- move forward and up and outside the current
C-M-k -- kill
C-M-DEL -- kill backward
C-M-@ -- mark
C-M-t -- transpose
M-x check-parens -- match all open and closed parentheses in buffer

comment:
C-u M-; -- kill
C-SPC -- set end point of region
M-x comment-region
M-x uncomment-region
M-x comment-kill -- kill
C-x ; -- set comments to start at point
M-- C-x ; -- kill comment on this line
C-u C-x ; -- insert and align or just align to column of previous comment

occur:
M-x occur -- show regexp match in buffer
C-u 3 M-x occur -- show matches with 3 lines of context
C-u - 3 M-x occur -- show matches with 3 lines before match

spell check:
M-$ -- word
M-x ispell-buffer -- buffer
M-x ispell-region -- region
M-x ispell-comments-and-strings -- words and comments in source file
q -- quit
M-x ispell-continue -- resume suspended session
M-r -- edit word at point in buffer with recursive edit
C-M-c -- return to spell check by exiting recursive edit
4 -- use third suggested choice
0 -- use first suggested choice
? -- quick help
SPC -- continue
a -- accept for this session
A -- add to buffer local dictionary
r -- replace word with typed version
R -- replace every occurrence of word with typed version
X -- suspend
M-x ispell-change-dictionary -- change default dictionary

transpose:
C-t -- characters \(see \"character\" above\)
M-- C-t -- previous with its previous
C-u 3 C-t -- forward 3 characters
C-u C-t -- forward 4 characters
M-- 3 C-t -- backward 3 characters
C-u C-t -- backward 4 characters
M-t -- words \(see \"word\" above\)
C-x C-t -- lines \(see \"line\" above\)
M-x transpose-paragraphs -- paragraphs \(see \"paragraph\" above\)
C-M-t -- parenthetical expressions

composition:
C-\\ france-postfix RET -- set to French characters
C-\\ -- disable input method, subsequent re-enables
C-h C-\\ RET -- help with current input method
e ' -- insert a letter E acute
e ' ' -- insert a letter E and a quote character
a ` -- insert a letter A grave
e ` -- insert a letter E grave
u ` -- insert a letter U grave
a ^ -- insert a letter A circumflex
e ^ -- insert a letter E circumflex
i ^ -- insert a letter I circumflex
o ^ -- insert a letter O circumflex
u ^ -- insert a letter U circumflex
c , -- insert a letter C with cedilla
c , , -- insert a letter C and comma
e \" -- insert a letter E umlaut
i \" -- insert a letter I umlaut
u \" -- insert a letter U umlaut
< < -- insert an open quotation mark
> > -- insert a closed quotation mark
C-x RET C-\\ spanish-postfix RET -- change to Spanish characters
i ` -- insert a letter I grave
o ` -- insert a letter O grave
n ~ -- insert a letter N with tilde
C-x RET C-\\ german-postfix RET -- change to German characters
a e -- insert a letter A umlaut
a e e -- insert the letters A and E, no umlaut
o e -- insert a letter O umlaut
o e e -- insert the letters O and E, no umlaut
u e -- insert a letter U umlaut
u e e -- insert the letters U and E, no umlaut
s z -- insert the ligature eszett
s z z -- insert the letters S and Z

text register:
C-x r s a -- store region as \"a\"
C-x r i a -- insert region stored in \"a\"
C-x r r a -- store rectangle as \"a\"

point register:
C-x r SPC a -- store current as \"a\"
C-x r j a -- move to point in \"a\"

window register:
C-x r w a -- store configuration of windows in frame
C-x r j a -- restore window configurations

frame register:
C-x r f a -- store window configuration for all frames
C-x r j a -- restore all window configurations

number register:
C-u 1 C-x r n a -- store 1 in \"a\"
C-u 1 C-x r + a -- add 1 to number in \"a\"
C-x r i a -- insert number in \"a\"

position register:
C-x r m RET -- save default
C-x r m <NAME> RET -- save as NAME
C-x r b RET -- move to default
C-x r b <NAME> RET -- move to NAME
C-x r l -- list
M-x bookmark-save -- save positions to file

column:
M-x column-number-mode -- show column number in mode line
C-u C-x C-n -- set column for line motion commands
C-x C-n -- unset goal column for line motion commands
M-x ruler-mode -- add a ruler to the current buffer's window

rectangle:
C-x r d -- delete, no kill
C-x r k -- kill
C-x r y -- yank
C-x r c -- blank out
C-x r t <STRING> -- replace each line with STRING
M-x string-insert-rectangle -- insert STRING at each line
M-x delete-whitespace-rectangle -- remove leading whitespace
C-x r r a -- store to register \"a\"

table:
M-x table-recognize-table -- activate table at point
C-u M-x table-recognize-table -- inactivate table at point
C-u 3 table-insert-column -- insert 3 columns
C-u 3 table-delete-column -- delete 3 columns

delimited text:
M-x delimit-columns-customize -- change settings
M-x delimit-columns-rectangle -- format rectangle
M-x delimit-columns-region -- format region

;align

directory \(folder\):
M-x cd -- change working
M-x make-directory RET <PATH> RET -- make PATH including any missing parents
C-x d RET -- list current
C-x C-f RET -- same as previous
C-x d .. RET -- list parent
C-x C-f .. RET -- same as previous
C-x C-d RET ^ -- same as previous
C-x C-d RET C-x C-j -- same as previous
C-x d <PATH> RET -- list PATH
C-x C-f <PATH> RET -- same as previous
M-x dired-jump RET -- go to current file in the listing
j -- move to file in listing
g -- reread the listing
C-s -- search listings \(see \"search\" above\)
RET -- open file or directory
+ -- add new
i -- show listing of current subdirectory
> -- skip between directory listings
< -- skip between directory listings
C-x C-f <FILE> RET -- visit FILE \(see \"file\" above\)
M o+r RET -- make current file world-readable
G <GROUP> RET -- change current file to GROUP
O <OWNER> RET -- change current file to OWNER
R <FILE> RET -- move current file to FILE
C <FILE> RET -- copy current file to file
P RET -- send current file to default printer
P RET M-DEL a2ps -- print current file in Postscript
P SPC -P SPC <PRINTER> -- send current file to <PRINTER>
T -- touch current file
H <FILE> RET -- hardlink current file to FILE
S <FILE> RET -- symlink current file to FILE
Y <FILE RET -- relative symlink current file to FILE
^ -- list parent
C-x C-j -- list parent
m -- mark current file
u -- unmark current file
d -- mark current file for deletion
t -- toggle marks
U -- unmark all files
C -- copy marked files to another directory
R -- move marked files to another directory
M-x wdired-change-to-wdired-mode -- manually edit listing with WDired
C-c C-c -- Exit WDired and commit the edits made to the listing

tramp:
C-x C-f /HOST:DIR/FILE -- open FILE in DIR on remote HOST
C-x C-f /scp:HOST:DIR/FILE -- same but use secure copy \(SCP\)
C-x C-f /ssh:HOST:DIR/FILE -- same but demand the use of SSH
C-x C-f /ssh1:HOST:DIR/FILE -- same but demand version 1 of SSH
C-x C-f /HOST:DIR -- list contents of DIR on remote HOST
C-x d /HOST:DIR -- same as previous

shell:
M-x shell -- new window
C-u M-x shell -- prompt for buffer name of new window
M-! -- run command
C-u M-! -- insert output of command
C-SPC -- set end point of region
M-| -- send region to command
C-u -- replace region with output of command
C-x C-w <FILE> RET -- save session transcript to FILE

shell script:
C-x C-f file.sh RET -- start a script named file.sh
M-x shell-script-mode -- use shell script mode for current buffer
M-x sh-mode -- same as previous
C-c : -- specify shell and insert header
C-c C-x -- run the script
C-M-x -- execute region
M-a -- beginning of command
M-e -- end of command
C-M-a -- beginning of function
C-M-e -- end of function
TAB -- indent
C-j -- newline and indent
C-c < -- use indentation level of current line
C-c > -- analyze buffer's indentation and show inconsistencies
C-c = -- set indentation level for syntactic type at point
C-c ? -- show indentation level at point
C-c TAB -- insert if statement
C-c C-f -- insert for statement
C-c C-c -- insert case statement
C-c C-t -- insert syntax for temporary file
C-c \( -- insert syntax for function

compile:
M-x compile -- execute a compilation command
M-x recompile -- execute last compilation command
C-c C-k -- kill
RET -- go to source code for error specified at point
C-c C-c -- same as previous
M-n -- next error
M-p -- previous error
M-} -- errors for next file
M-{ -- errors for previous file
C-x ` -- go to source code for next error

lisp:
C-M-a -- beginning of defun
C-M-e -- end of defun
C-M-n -- forward sexp
C-M-p -- backward sexp
C-M-u -- upward sexp
C-M-d -- down sexp
M-x up-list -- upward sexp and forward
M-\( -- insert parens for sexp
C-m-t -- transpose sexp
C-M-SPC -- mark sexp
C-c C-z -- run interpreter
C-M-x -- eval expression at point
M-; -- insert new comment
C-u M-; -- kill current comment

emacs lisp:
C-x C-e -- evaluate expression before point
C-u C-x C-e -- evaluate expression and insert result at point
C-M-x -- evaluate current defun
M-: -- prompt for expression then evaluate
C-u M-: -- eval expression and insert result at point
TAB -- indent
C-M-q -- indent expression after point
M-TAB -- complete symbol at point
M-x eval-region -- evaluate expressions in region
M-x eval-buffer -- evaluate buffer
M-x load-file RET <FILE> RET -- load FILE
M-x load-file RET RET -- load current file
M-x load-libary -- load library
M-x byte-compile-file RET <FILE> RET -- byte compile current FILE
M-x byte-compile-file RET RET -- same as previous
M-x byte-recompile-directory RET -- byte compile every file, recursively
M-x find-function RET <FUNCTION> RET -- go to definition of FUNCTION
M-x find-function RET RET -- go to definition of function at point
M-x find-variable RET <VARIABLE> RET -- go to definition of VARIABLE
M-x find-variable RET RET -- go to definition of variable at point
M-x find-library <LIBRARY> -- go to LIBRARY
M-x emacs-lisp-mode -- start Emacs Lisp mode if not started
M-x checkdoc -- validate coding style
M-x checkdoc-ispell -- and spell check comments and documentation strings
M-x set-variable RET debug-on-error RET t RET -- enable debugger on error
M-x set-variable RET debug-on-quit RET t RET -- enable debugger on quit
M-x set-variable RET debug-on-error RET nil RET -- disable debugger on error
M-x set-variable RET debug-on-quit RET nil RET -- enable debugger on quit

\(e\)lisp interaction:
M-x lisp-interaction-mode -- evaluate expressions interactively
C-j -- evaluate sexp before point and insert results on next line
C-M-x -- evaluate current defun \(see \"emacs lisp\" above\)

debug elisp:
M-x toggle-debug-on-error -- change whether to start session on error
M-x toggle-debug-on-quit -- change whether C-g starts session
M-x debug-on-entry RET <FUNCTION> RET -- debug FUNCTION
h -- help
SPC -- move down
C-n -- move down
5 SPC -- move 5 down
- 2 SPC -- move 2 up
C-p -- move up
2 C-p -- backward 2 lines
TAB -- go up level in expression
S-TAB -- go down lower level
RET -- visit help or source location for thing at point
c -- complete evaluation level at current point
q -- quit
d -- step into
b -- set breakpoint
u -- unset breakpoint
j -- set breakpoint and continue
r -- prompt for return value then continue
e -- prompt for expression then evaluate
R -- prompt for expression then evaluate and record it
l -- list functions debugged on entry
M-x cancel-debug-on-entry <FUNCTION> RET -- don't debug FUNCTION
M-x cancel-debug-on-entry RET RET -- don't debug for any function

source-level debugger:
M-x edebug-defun -- turn on instrumentation for current function definition
C-u C-M-x -- same as previous
C-M-x -- turn off instrumentation for current function definition
SPC -- step expression in source
C-x X SPC -- from any buffer, step into expression in source
t -- slowly step
T -- step fast
S -- stop stepping
n -- step to next expression
i -- step in
o -- step out
f -- step forward
r -- show last result again in minibuffer
b -- set breakpoint
u -- unset breakpoint
C-c C-d -- unset breakpoint
x <EXPRESSION> RET -- set conditional break on result of EXPRESSION
B -- move to next breakpoint
g -- continue until next breakpoint
B -- continue to next breakpoint
c -- continue to breakpoints slowly
C -- continue to breakpoints fast
S -- stop continuing
G -- stop debugging and finish
P -- visit buffer before running Edebug
v -- visit buffer before running Edebug
p -- momentarily visit buffer before running Edebug
w -- move back to current point in source
C-c C-l -- move back to current point in source
C-x X w -- from any buffer, move back to current point in source
? -- help
e -- prompt for expression then evaluate
d -- show backtrace
= -- display frequencies in comments for each line for current function
a -- abort
C-] -- abort
q -- quit
Q -- quit

finder:
C-h P -- list keywords
M-x finder-commentary RET <LIBRARY> RET -- Describe LIBRARY
? -- help
n -- move down
p -- move up
RET -- for keyword at point, list Emacs Lisp libraries
RET -- for package at point, show commentary for Emacs
f -- same as previous
SPC -- same as previous
d -- back to beginning of package directory
q -- quit

;c

tag:
M-! etags *.[ch] -- index .c and .h files in current directory
C-u M-x visit-tags-table -- set index file for current buffer
M-x visit-tags-table -- globally set index file
M-. -- go to definition of symbol in index
C-M-. -- go to definition for a regular expression in index
C-u M-. -- go to next definition
M-- M-. -- go to previous definition
M-* -- return back to before you started
M-x tags-search -- go to entry for regular expression in index
M-, -- go to next entry in index
M-x tags-query-replace -- search and replace for regular expression
M-TAB -- complete tag at point
C-u M-TAB -- complete language symbol, avoid tags, at point

;gdb

diff:
M-x diff RET <OLD> RET <NEW> RET -- compare OLD file with NEW file
C-u M-x diff -- compare files but prompt for Diff switches
M-x diff-buffer-with-file -- compare buffer with file on disk
M-x diff-backup -- compare current file with backup on disk
M-x diff RET RET -- same as previous
M-x diff-mode -- start Diff Mode if not already started for a file
C-c C-c -- go to corresponding location in target \(new\) file
C-u C-c C-c -- go to corresponding location in source \(old\) file
C-u C-u C-c C-c -- always go to corresponding location in source file
C-c C-a -- apply current hunk
C-u C-c C-a -- revert current hunk
C-c C-t -- test current hunk
C-c C-t -- test current hunk in reverse
M-n -- move start of next hunk
M-p -- move to start of previous hunk
M-} -- move to start of next file in multiple file patch
M-{ -- move to start of previous file in multiple file patch
C-c C-n -- narrow to hunk
C-x n w -- widen
C-u C-c C-n -- narrow to file of multiple file patch
M-k -- kill the current hunk
M-K -- kill the current file in multiple file patch
C-c C-s -- split the hunk in two
C-c C-r -- reverse direction of entire patch
C-u C-c C-r -- reverse direction of patch in region
C-x 4 a -- new change log entry using context of current location
C-c C-u -- convert the entire buffer from unified to context format
C-u C-c C-u -- convert the entire buffer from context to unified format
C-c C-u -- convert the entire buffer
C-c C-e -- start ediff session

version control \(vc\):
C-x v i -- register file
C-x v v -- check in or out, depending on the current state
C-c C-c -- finish editing log for check in
C-u C-x v v -- check in or out a specific revision
C-x v ~ -- open past revision in new window
C-x v = -- diff with current revision
C-u C-x v = -- diff with specific revision
C-x v l -- show log
C-x v u -- undo checkout
C-x v c -- delete the latest revision
C-x v g -- annotate file by each line showing when added and by whom
C-x v d -- show checked out files
C-x v s RET <NAME> RET -- tag all the files in directory with NAME
C-u C-x v s RET <NAME> RET -- tag files and create branch
C-x v r <NAME> -- recursively checkout files for a snapshot
C-x v a -- update ChangeLog \(see \"changelog\" below\)
C-x v m -- merge two revisions
C-x v h -- insert revision header keyword
M-x vc-resolve-conflicts -- start ediff-merge session on a file with conflict markers

changelog:
C-x 4 a -- start new entry using context of current file
C-x 4 a -- start new entry in current log file
C-c C-p -- insert previous log from version control
M-q -- fill paragraph following syntax rules
M-x change-log-merge RET <FILE> RET -- merge current with log FILE
C-x v a -- generate entries from version control

merge conflict:
M-x smerge-mode -- start Smerge Mode if not started
C-c ^ n -- move to next
C-c ^ p -- move to previous
C-c ^ b -- keep base
C-c ^ m -- keep mine
C-c ^ o -- keep other
C-c ^ RET -- keep what is under point
C-c ^ a -- keep all
C-c ^ c -- combine current with next
C-c ^ r -- auto resolve
M-x smerge-resolve-all -- auto resolve entire buffer

grep:
M-x grep RET <REGEXP> SPC <FILES> RET -- show matches in FILES for REGEXP
M-x lgrep RET <REGEXP> RET <FILES> RET -- show matches in FILES for REGEXP
M-x lgrep RET <REGEXP> RET RET -- show matches in all C files
M-x lgrep RET <REGEXP> RET ch RET -- same as previous
M-x lgrep RET <REGEXP> RET c RET -- show matches in C source files
M-x lgrep RET <REGEXP> RET h RET -- show matches in header files
M-x lgrep RET <REGEXP> RET l RET -- show matches in ChangeLog files
M-x lgrep RET <REGEXP> RET m RET -- show matches in Make files
M-x lgrep RET <REGEXP> RET tex RET -- show matches in TeX files
M-x lgrep RET <REGEXP> RET *.html RET -- show matches in HTML files
M-x egrep RET <REGEXP> RET <FILES> RET -- extended regular expressions
M-x igrep RET <REGEXP> RET <FILES> RET -- case insensitive matching
M-x grep-find RET <REGEXP> RET -- show matches in entire directory tree
M-x rgrep RET <REGEXP> RET *.html RET RET -- same, but HTML files
M-x rgrep RET <REGEXP> RET RET RET -- same, but C files
M-x rgrep RET <REGEXP> RET el RET RET -- same, but Emacs Lisp files

locate:
M-x locate RET <PATTERN> RET -- show files matching PATTERN
M-x locate-with-filter RET <PATTERN> RET <REGEXP> -- same, but also match REGEXP
M-x locate-with-filter -- show
C-n -- next matched file
C-p -- previous matched file
RET -- visit current file at
C-o -- open file in other window
V -- open current file in dired \(see \"directory\" above\)

;calendar

;diary

;abbrev

;dabbrev

;autoinsert

;ediff

;emerge

;sort

;browse-url

;email

;w3

HTML:
C-h m -- help
C-c C-v -- view current file in Web browser
C-c C-s -- toggle to view in Web browser on each save
C-c 8 -- toggle inserting of non-ASCII characters as entities
C-c TAB -- toggle invisibility of tags
M-x html-imenu-index -- add index menu to menu bar for current file
M-x set-variable RET sgml-xml-mode RET t -- turn on XHTML tags
C-c C-d -- delete current tag
C-c DEL -- delete current tag
C-u C-c C-d -- delete next 4 tags
C-c C-f -- skip forward tag
C-u 5 C-c C-f -- skip forward 5 tags
C-c C-f -- skip backward tag
C-u C-c C-f -- skip backward 4 tags
C-c C-t html RET <TITLE>  -- start file with TITLE
C-c 1 -- insert level one heading
C-c 2 -- insert level two heading
C-c 3 -- insert level three heading
C-c 4 -- insert level four heading
C-c 5 -- insert level five heading
C-c 6 -- insert level six heading
C-c RET -- insert paragraph tag
C-c / -- close paragraph tag
C-c C-j -- insert line break tag
C-c C-c - -- insert horizontal rule
C-c C-c h -- insert link
C-c C-c n -- insert page anchor
C-c C-c i -- insert image
C-c C-c o -- insert ordered list
C-c C-c u -- insert unordered list
C-c C-c i -- insert list-item
C-u M-o b -- insert bold tag
C-u M-o i -- insert italic tag
C-u M-o b -- insert bold tag
C-u M-o i -- insert italic tag
C-u M-o i -- insert underline tag
C-c C-a -- insert attributes to current tag
C-c C-t em RET -- insert emphasis tag
C-c C-t strong RET -- insert strong emphasis tag
C-c C-t code RET -- insert source code tag
C-c C-t dfn RET -- insert source code tag
C-c C-t kbd RET -- insert keyboard text tag
C-c C-t samp RET -- insert sample text tag
C-c C-t var RET -- insert sample text tag
C-c C-t pre RET -- insert preformatted text tag
C-c C-t span RET class RET <CLASS> RET -- insert span tag for text of CLASS
C-c C-t dl RET <TERM> RET RET -- insert definition list with TERM
C-c C-t table RET h RET d RET DEL RET -- insert 1-by-1 table
C-c C-t -- prompt for tag name and possible attributes, then insert
C-u 3 C-c C-t -- prompt for tag, and surround next 3 words with tag
C-1 C-c C-t -- prompt for tag, and surround next word with tag
M-- C-c C-t -- prompt for tag, and surround region with tag
C-c ? RET -- describe current tag
C-c C-n M-SPC -- insert non-breaking space entity
M-; -- insert comment
C-u M-; -- kill comment
M-x sgml-show-context -- display hierarchy of tags for point
M-x sgml-validate -- check markup with external tool

;outline

;sql

;calc

timeclock:
M-x timeclock-in -- start a project
M-x timeclock-out -- stop working on the current project
M-x timeclock-when-to-leave-string -- report time to leave
M-x timeclock-visit-timelog -- visit timelog file
M-x timeclock-reread-log -- reread timelog file after crash
M-x timeclock-reread-log -- reread timelog file after edited
M-x timeclock-reread-log -- reread timelog file after restarting emacs

games:
M-x 5x5 -- fill a 5-by-5 grid
M-x blackbox -- find balls in a box
M-x doctor -- psychoanalysis
M-x dunnet -- text adventure
M-x gomoku -- try to get 5 in a row
M-x mpuz -- multiplication puzzle
M-x pong -- classic video tennis
M-x snake -- eat dots but not yourself or the walls
M-x tetris -- stack blocks
M-x type-break-mode -- be told when to take breaks

animation:
M-x animate-birthday-present RET <NAME> RET -- Birthday wishes to NAME
M-x butterfly-mode -- strike the drive platter and flip the desired bit
M-x dissociated-press -- scramble current text in another buffer
M-x hanoi -- towers of hanoi
M-x life -- the game of life
M-x studlify-buffer -- give text in buffer strange capitalization
M-x studlify-region -- give text in region strange capitalization
M-x zone -- display text tricks

#:END:REFERENCE-SHEET#")) ;setq

;;;test-me;(symbol-value '*reference-sheet-help-A-HAWLEY*)

;;; ==============================
;;; Make variable `*reference-sheet-help-A-HAWLEY*' self documenting.
;;; Concat the 'documentation header' onto 'doc string' snarfed from itself.
;;; Put that value as the variable-documentation property value.
;;; "I'm hungry. Maybe I'll vomit into my own mouth and eat that..."
;;; (food-p self-vomit) => t
;;; ==============================
(let (self-puke)
    (setq self-puke
          (concat
           "Emacs reference card unlike others doesn't fit on a conveniently sized card.
Instead it tries to tell you everything about doing things in Emacs.
Use with `emacs-wiki-fy-reference-sheet' for rapid EmacsWikification.
This list can be kept properly escaped \(more-or-less\) by evaluating
`emacs-wiki-escape-lisp-string-region' and `emacs-wiki-unescape-lisp-string-region'
respectively.\n
See; \(URL `http://www.emacswiki.org/emacs/Reference_Sheet_by_Aaron_Hawley')\n
\(URL `http://www.emacswiki.org/emacs/Reference_Sheet_by_Aaron_Hawley_source'\).
►►►\n"
           (symbol-value '*reference-sheet-help-A-HAWLEY*)))
    (put
     '*reference-sheet-help-A-HAWLEY*
     'variable-documentation
     self-puke))

;;(symbol-value '*reference-sheet-help-A-HAWLEY*)))

;;(progn (makunbound '*reference-sheet-help-A-HAWLEY*)
;;  (unintern '*reference-sheet-help-A-HAWLEY*))

;;; ==============================
;;; Function documents itself by snarfing from the variable-documentation
;;; property value of `*reference-sheet-help-A-HAWLEY*' and putting that
;;; as the value for its own function-documentation prop.
;;; CREATED: <Timestamp: Thursday July 02, 2009 @ 05:59.17 PM - by MON KEY>
(defun mon-help-keys (&optional insertp intrp)
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-keys :insertp t)
    (message "pass non-nil for optional arg INTRP")))
;;
  (put
   'mon-help-keys
   'function-documentation
   (concat ""
           (plist-get
            (symbol-plist '*reference-sheet-help-A-HAWLEY*)
            'variable-documentation)))
;;
;;;test-me:(mon-help-keys t)
;;
;;(progn (makunbound 'mon-help-keys)
;;   (unintern 'mon-help-keys))

;;; ==============================
;;; COURTESY: Aaaron Hawley HIS:
;;; (URL `http://www.emacswiki.org/emacs/Reference_Sheet_by_Aaron_Hawley_source')
;;; MODIFICATIONS: <Timestamp: Wednesday June 17, 2009 @ 11:31.47 AM - by MON KEY>
(defun emacs-wiki-fy-reference-sheet (&optional use-var insertp)
  "When reference sheet is delimited at top and bottom with
'#:START:REFERENCE-SHEET#' and '#:END:REFERENCE-SHEET#'
Return contents between delimiters as wikified for insertion/update to
EmacsWiki at (URL `http://www.emacswiki.org/emacs/Reference_Sheet_by_Aaron_Hawley_source').
When optional arg USE-VAR is non-nil wikifiy contents of the global
`*reference-sheet-help-A-HAWLEY*' instead of contents of buffer from point.
When optional arg INSERTP is non-nil insert wikified reference sheet at point.\n
EXAMPLE:\n
#:START:REFERENCE-SHEET#\n
{...Here is the Reference Sheet by Aaron Hawley ...}\n
#:END:REFERENCE-SHEET#\n ►►►"
  (interactive)
  (let (in-bfr)
    (save-excursion
      (let (wikify-this start-at end-at)
	(search-forward-regexp "^\\(#:START:REFERENCE-SHEET#\\)$" nil t)
	(setq start-at  (match-beginning 1))
	(search-forward-regexp "^\\(#:END:REFERENCE-SHEET#\\)$" nil t)
	(setq end-at  (match-beginning 1))
	(setq wikify-this  (buffer-substring-no-properties start-at end-at))
	(setq in-bfr (with-temp-buffer
		       (if use-var
                           (mon-help-keys 1)
                         ;; (insert *reference-sheet-help-A-HAWLEY*)
			 (insert wikify-this))
		       (goto-char (point-min))
		       (let ((regexp-replace-list
			      '(("^\\(#:START:REFERENCE-SHEET#\\)" . "")
				("^\\(#:END:REFERENCE-SHEET#\\)" . "")
				("^\\(.*\\):$" . "||||**\\1**||")
				("^\\(.+?\\)\\( -- \\(.*\\)\\)?$" . "||##\\1##||\\3||")
				("^\n" . ""))))
			 (flush-lines "^;")
			 (regexpl-search-replace-list regexp-replace-list))
		       (buffer-substring-no-properties (point-min) (point-max))))))
    (if insertp
	(progn
	  (newline)
	  (princ in-bfr (current-buffer))
	  (newline))
      in-bfr)))

;;;test-me;(progn(mon-help-keys t)(emacs-wiki-fy-reference-sheet))
;;;test-me;(progn(mon-help-keys t)(emacs-wiki-fy-reference-sheet nil t))
;;;test-me(emacs-wiki-fy-reference-sheet t t)

;;; ==============================
;;; I think I remember stealing this from Pascal Bourguignon?
;;; MODIFICATIONS: <Timestamp: Saturday May 30, 2009 @ 06:26.12 PM - by MON KEY>
(defun emacs-wiki-escape-lisp-string-region (&optional start end ref-sheet)
  "Escape special characters in the region as if a Lisp string.
Inserts backslashes in front of special characters (namely  `\' backslash,
`\"' double quote, `(' `)' parens in the region, according to the docstring escape
requirements.\n
Don't expect good results evaluation this form on strings with regexps.
NOTE: region should only contain the characters actually comprising the string
supplied without the surrounding quotes.\n
See also; `emacs-wiki-unescape-lisp-string-region', `emacs-wiki-fy-reference-sheet'
`*reference-sheet-help-A-HAWLEY*'. ►►►"
  (interactive "*r")
  (save-excursion
    (save-restriction
      (let (ref-start ref-end)
	(if ref-sheet
	   (progn
	     (search-forward-regexp "^\\(#:START:REFERENCE-SHEET#\\)$" nil t)
	     (setq ref-start  (match-beginning 1))
	     (search-forward-regexp "^\\(#:END:REFERENCE-SHEET#\\)$" nil t)
	     (setq ref-end  (match-beginning 1)))
	  (progn
	    (setq ref-start start)
	    (setq ref-end end)))
      (narrow-to-region ref-start ref-end)
      (goto-char ref-start)
      (while (search-forward "\\" nil t)
	(replace-match "\\\\" nil t))
      (goto-char ref-start)
      (while (search-forward "\"" nil t)
	(replace-match "\\\"" nil t))
      ;;MON KEY additions
      (goto-char ref-start)
      (while (search-forward "(" nil t)
	(replace-match "\\\(" nil t))
      (goto-char ref-start)
      (while (search-forward ")" nil t)
	(replace-match "\\\)" nil t))))))

;;;test-me;
;; (save-excursion (let ((this-point (point)))
;; (newline)(princ *reference-sheet-help-A-HAWLEY* (current-buffer))
;; (goto-char this-point)(emacs-wiki-escape-lisp-string-region nil nil t)))

;;; ==============================
;;; I think i remember stealing this from Pascal Bourguignon?
;;; MODIFICATIONS: <Timestamp: Saturday May 30, 2009 @ 06:26.12 PM - by MON KEY>
(defun emacs-wiki-unescape-lisp-string-region (&optional start end ref-sheet)
  "Unescape special characters from the CL string specified by the region.
This amounts to removing preceeding backslashes from characters they escape.\n
Don't expect good results evaluation this form on strings with regexps.
Note: region should only contain the characters actually comprising the string
without the surrounding quotes.\nSee also; `emacs-wiki-escape-lisp-string-region',
`*reference-sheet-help-A-HAWLEY*', `emacs-wiki-fy-reference-sheet'. ►►►"
  (interactive "*r")
  (save-excursion
    (save-restriction
      (let (ref-start ref-end)
	(if ref-sheet
	   (progn
	     (search-forward-regexp "^\\(\"#:START:REFERENCE-SHEET#\\)$" nil t)
	     (setq ref-start  (match-beginning 1))
	     (search-forward-regexp "^\\(#:END:REFERENCE-SHEET#\"\\)$" nil t)
	     (setq ref-end  (match-beginning 1)))
	  (progn
	    (setq ref-start start)
	    (setq ref-end end)))
	  (narrow-to-region ref-start ref-end)
	  (goto-char ref-start)
	  (while (search-forward "\\" nil t)
	    (replace-match "" nil t)
	    (forward-char))))))

;;;test-me;
;; (save-excursion
;; (let ((this-point (point)))
;; (newline)
;; (prin1 *reference-sheet-help-A-HAWLEY* (current-buffer))
;; (goto-char this-point)
;; (emacs-wiki-unescape-lisp-string-region nil nil t)))

;;; ==============================
(provide 'mon-doc-help-utils)
;;; ==============================

;;; ================================================================
;;; mon-doc-help-utils.el ends here
;;; EOF
