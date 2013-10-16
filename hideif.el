;;; hideif.el --- hides selected code within ifdef

;; Copyright (C) 1988, 1994, 2001-2012, 2013  Free Software Foundation, Inc.

;; Author: Brian Marick
;;      Daniel LaLiberte <liberte@holonexus.org>
;; Maintainer: FSF
;; Keywords: c, outlines
;; Rewrite by Luke Lee in 2013.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; To initialize, toggle the hide-ifdef minor mode with
;;
;; M-x hide-ifdef-mode
;;
;; This will set up key bindings and call hide-ifdef-mode-hook if it
;; has a value.  To explicitly hide ifdefs using a buffer-local
;; define list (default empty), type
;;
;; M-x hide-ifdefs  or C-c @ h
;;
;; Hide-ifdef suppresses the display of code that the preprocessor wouldn't
;; pass through.  The support of constant expressions in #if lines is
;; limited to identifiers, parens, and the operators: &&, ||, !, and
;; "defined".  Please extend this.
;;
;; The hidden code is marked by ellipses (...).  Be
;; cautious when editing near ellipses, since the hidden text is
;; still in the buffer, and you can move the point into it and modify
;; text unawares.
;; You can make your buffer read-only while hide-ifdef-hiding by setting
;; hide-ifdef-read-only to a non-nil value.  You can toggle this
;; variable with hide-ifdef-toggle-read-only (C-c @ C-q).
;;
;; You can undo the effect of hide-ifdefs by typing
;;
;; M-x show-ifdefs  or C-c @ s
;;
;; Use M-x hide-ifdef-define (C-c @ d) to define a symbol.
;; Use M-x hide-ifdef-undef (C-c @ u) to undefine a symbol.
;;
;; If you define or undefine a symbol while hide-ifdef-mode is in effect,
;; the display will be updated.  Only the define list for the current
;; buffer will be affected.  You can save changes to the local define
;; list with hide-ifdef-set-define-alist.  This adds entries
;; to hide-ifdef-define-alist.
;;
;; If you have defined a hide-ifdef-mode-hook, you can set
;; up a list of symbols that may be used by hide-ifdefs as in the
;; following example:
;;
;; (add-hook 'hide-ifdef-mode-hook
;;      (lambda ()
;;       (unless hide-ifdef-define-alist
;;         (setq hide-ifdef-define-alist
;;              '((list1 ONE TWO)
;;                (list2 TWO THREE))))
;;       (hide-ifdef-use-define-alist 'list2))) ; use list2 by default
;;
;; You can call hide-ifdef-use-define-alist (C-c @ U) at any time to specify
;; another list to use.
;;
;; To cause ifdefs to be hidden as soon as hide-ifdef-mode is called,
;; set hide-ifdef-initially to non-nil.
;;
;; If you set hide-ifdef-lines to t, hide-ifdefs hides all the #ifdef lines.
;; In the absence of highlighting, that might be a bad idea.  If you set
;; hide-ifdef-lines to nil (the default), the surrounding preprocessor
;; lines will be displayed.  That can be confusing in its own
;; right.  Other variations on display are possible, but not much
;; better.
;;
;; You can explicitly hide or show individual ifdef blocks irrespective
;; of the define list by using hide-ifdef-block and show-ifdef-block.
;;
;; You can move the point between ifdefs with forward-ifdef, backward-ifdef,
;; up-ifdef, down-ifdef, next-ifdef, and previous-ifdef.
;;
;; If you have minor-mode-alist in your mode line (the default) two labels
;; may appear.  "Ifdef" will appear when hide-ifdef-mode is active.  "Hiding"
;; will appear when text may be hidden ("hide-ifdef-hiding" is non-nil).
;;
;; Written by Brian Marick, at Gould, Computer Systems Division, Urbana IL.
;; Extensively modified by Daniel LaLiberte (while at Gould).
;;
;; Rewrite by Luke Lee in 2013.

;;; Code:

(require 'cc-mode)
(require 'cl)

(defgroup hide-ifdef nil
  "Hide selected code within `ifdef'."
  :group 'c)

(defcustom hide-ifdef-initially nil
  "Non-nil means call `hide-ifdefs' when Hide-Ifdef mode is first activated."
  :type 'boolean
  :group 'hide-ifdef)

(defcustom hide-ifdef-read-only nil
  "Set to non-nil if you want buffer to be read-only while hiding text."
  :type 'boolean
  :group 'hide-ifdef)

(defcustom hide-ifdef-lines nil
  "Non-nil means hide the #ifX, #else, and #endif lines."
  :type 'boolean
  :group 'hide-ifdef)

(defcustom hide-ifdef-shadow nil
  "Non-nil means shadow text instead of hiding it."
  :type 'boolean
  :group 'hide-ifdef
  :version "23.1")

(defcustom hide-ifdef-exclude-define-regexp-pattern nil
  "Ignore #define name if that name match this exclusion pattern."
  :group 'hide-ifdef)

(defface hide-ifdef-shadow '((t (:inherit shadow)))
  "Face for shadowing ifdef blocks."
  :group 'hide-ifdef
  :version "23.1")


(defvar hide-ifdef-mode-submap
  ;; Set up the submap that goes after the prefix key.
  (let ((map (make-sparse-keymap)))
    (define-key map "d" 'hide-ifdef-define)
    (define-key map "u" 'hide-ifdef-undef)
    (define-key map "D" 'hide-ifdef-set-define-alist)
    (define-key map "U" 'hide-ifdef-use-define-alist)

    (define-key map "h" 'hide-ifdefs)
    (define-key map "s" 'show-ifdefs)
    (define-key map "\C-d" 'hide-ifdef-block)
    (define-key map "\C-s" 'show-ifdef-block)

    (define-key map "e" 'hif-evaluate-macro)

    (define-key map "\C-q" 'hide-ifdef-toggle-read-only)
    (define-key map "\C-w" 'hide-ifdef-toggle-shadowing)
    (substitute-key-definition
     'toggle-read-only 'hide-ifdef-toggle-outside-read-only map)
    map)
  "Keymap used by `hide-ifdef-mode' under `hide-ifdef-mode-prefix-key'.")

(defconst hide-ifdef-mode-prefix-key "\C-c@"
  "Prefix key for all Hide-Ifdef mode commands.")

(defvar hide-ifdef-mode-map
  ;; Set up the mode's main map, which leads via the prefix key to the submap.
  (let ((map (make-sparse-keymap)))
    (define-key map hide-ifdef-mode-prefix-key hide-ifdef-mode-submap)
    map)
  "Keymap used with `hide-ifdef-mode'.")

(easy-menu-define hide-ifdef-mode-menu hide-ifdef-mode-map
  "Menu for `hide-ifdef-mode'."
  '("Hide-Ifdef"
    ["Hide some ifdefs" hide-ifdefs
     :help "Hide the contents of some #ifdefs"]
    ["Show all ifdefs" show-ifdefs
     :help "Cancel the effects of `hide-ifdef': show the contents of all #ifdefs"]
    ["Hide ifdef block" hide-ifdef-block
     :help "Hide the ifdef block (true or false part) enclosing or before the cursor"]
    ["Show ifdef block" show-ifdef-block
     :help "Show the ifdef block (true or false part) enclosing or before the cursor"]
    ["Define a variable..." hide-ifdef-define
     :help "Define a VAR so that #ifdef VAR would be included"]
    ["Undefine a variable..." hide-ifdef-undef
     :help "Undefine a VAR so that #ifdef VAR would not be included"]
    ["Define an alist..." hide-ifdef-set-define-alist
     :help "Set the association for NAME to `hide-ifdef-env'"]
    ["Use an alist..." hide-ifdef-use-define-alist
     :help "Set `hide-ifdef-env' to the define list specified by NAME"]
    ["Toggle read only" hide-ifdef-toggle-read-only
     :style toggle :selected hide-ifdef-read-only
     :help "Buffer should be read-only while hiding text"]
    ["Toggle shadowing" hide-ifdef-toggle-shadowing
     :style toggle :selected hide-ifdef-shadow
     :help "Text should be shadowed instead of hidden"]))

(defvar hide-ifdef-hiding nil
  "Non-nil when text may be hidden.")

(or (assq 'hide-ifdef-hiding minor-mode-alist)
    (setq minor-mode-alist
          (cons '(hide-ifdef-hiding " Hiding")
                minor-mode-alist)))

;; fix c-mode syntax table so we can recognize whole symbols.
(defvar hide-ifdef-syntax-table
  (let ((st (copy-syntax-table c-mode-syntax-table)))
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?& "." st)
    (modify-syntax-entry ?\| "." st)
    st)
  "Syntax table used for tokenizing #if expressions.")

(defvar hide-ifdef-env nil
  "An alist of defined symbols and their values.")

(defvar hif-outside-read-only nil
  "Internal variable.  Saves the value of `buffer-read-only' while hiding.")

;;;###autoload
(define-minor-mode hide-ifdef-mode
  "Toggle features to hide/show #ifdef blocks (Hide-Ifdef mode).
With a prefix argument ARG, enable Hide-Ifdef mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

Hide-Ifdef mode is a buffer-local minor mode for use with C and
C-like major modes.  When enabled, code within #ifdef constructs
that the C preprocessor would eliminate may be hidden from view.
Several variables affect how the hiding is done:

`hide-ifdef-env'
        An association list of defined and undefined symbols for the
        current buffer.  Initially, the global value of `hide-ifdef-env'
        is used.

`hide-ifdef-define-alist'
        An association list of defined symbol lists.
        Use `hide-ifdef-set-define-alist' to save the current `hide-ifdef-env'
        and `hide-ifdef-use-define-alist' to set the current `hide-ifdef-env'
        from one of the lists in `hide-ifdef-define-alist'.

`hide-ifdef-lines'
        Set to non-nil to not show #if, #ifdef, #ifndef, #else, and
        #endif lines when hiding.

`hide-ifdef-initially'
        Indicates whether `hide-ifdefs' should be called when Hide-Ifdef mode
        is activated.

`hide-ifdef-read-only'
        Set to non-nil if you want to make buffers read only while hiding.
        After `show-ifdefs', read-only status is restored to previous value.

\\{hide-ifdef-mode-map}"
  :group 'hide-ifdef :lighter " Ifdef"
  (if hide-ifdef-mode
      (progn
	;; inherit global values
;;	(set (make-local-variable 'hide-ifdef-env) ;; [2012-09-27 11:05:26 +0800] Luke rem 2 ins 2
;;	     (default-value 'hide-ifdef-env))
	(set 'hide-ifdef-env (default-value 'hide-ifdef-env))
	(set (make-local-variable 'hide-ifdef-hiding)
	     (default-value 'hide-ifdef-hiding))
	(set (make-local-variable 'hif-outside-read-only) buffer-read-only)
	(set (make-local-variable 'line-move-ignore-invisible) t)
	(add-hook 'change-major-mode-hook
		  (lambda () (hide-ifdef-mode -1)) nil t)

	(add-to-invisibility-spec '(hide-ifdef . t))

	(if hide-ifdef-initially
	    (hide-ifdefs)
	  (show-ifdefs)))
    ;; else end hide-ifdef-mode
    (kill-local-variable 'line-move-ignore-invisible)
    (remove-from-invisibility-spec '(hide-ifdef . t))
    (when hide-ifdef-hiding
      (show-ifdefs))))


(defun hif-show-all ()
  "Show all of the text in the current buffer."
  (interactive)
  (hif-show-ifdef-region (point-min) (point-max)))

;; By putting this on after-revert-hook, we arrange that it only
;; does anything when revert-buffer avoids turning off the mode.
;; (That can happen in VC.)
(defun hif-after-revert-function ()
  (and hide-ifdef-mode hide-ifdef-hiding
       (hide-ifdefs t)))
(add-hook 'after-revert-hook 'hif-after-revert-function)

(defun hif-end-of-line ()
  (end-of-line)
  (while (= (logand 1 (skip-chars-backward "\\\\")) 1)
    (end-of-line 2)))

(defun merge-ifdef-region (start end)
  ;; Genernally there is no need to call itself recursively since there should originally
  ;; exists no un-merged regions; however, if a part of the file is hidden with 'hide-ifdef-lines'
  ;; as nil while another part with 't, this case happens.
  ;; TODO: should we merge? or just create a container overlay? -- this can prevent hideif-show-ifdef
  ;; expand too many since there is only a big overlay exists there without any smaller overlays.
  "Merge nearby ifdef regions to form a bigger overlay. This will decrease the
total number of overlays created."
  (save-restriction
    (widen) ;; otherwise (point-min) and (point-max) will be
            ;; restricted and thus fail to find neighbor overlays
    (let ((begovrs (overlays-in
                    (max (- start 2) (point-min))
                    (max (- start 1) (point-min))))
          (endovrs (overlays-in
                    (min (+ end 1) (point-max))
                    (min (+ end 2) (point-max))))
          (ob nil)
          (oe nil)
          b e)
      ;; merge overlays before 'start'
      (dolist (o begovrs)
        (when (overlay-get o 'hide-ifdef)
          (setq b (min start (overlay-start o))
                e (max end (overlay-end o)))
          (move-overlay o b e)
          (merge-ifdef-region b e)
          (setq ob o)))
      ;; merge overlays after 'end'
      (dolist (o endovrs)
        (when (overlay-get o 'hide-ifdef)
          (setq b (min start (overlay-start o))
                e (max end (overlay-end o)))
          (move-overlay o b e)
          (merge-ifdef-region b e)
          (setf oe o)))
      ;; if both 'start' 'end' merging happens, merge into bigger one
      (when (and ob oe)
        (let ((b (min (overlay-start ob) (overlay-start oe)))
              (e (max (overlay-end ob) (overlay-end oe))))
          (delete-overlay oe)
          (move-overlay ob b e)
          (merge-ifdef-region b e)))
      (or ob oe))))

(defun hide-ifdef-region-internal (start end)
  (unless (merge-ifdef-region start end)
    (let ((o (make-overlay start end)))
      (overlay-put o 'hide-ifdef t)
      (if hide-ifdef-shadow
          (overlay-put o 'face 'hide-ifdef-shadow)
        (overlay-put o 'invisible 'hide-ifdef)))))

(defun hide-ifdef-region (start end)
  "START is the start of a #if, #elif, or #else form.  END is the ending part.
Everything including these lines is made invisible."
  (save-excursion
    (goto-char start) (hif-end-of-line) (setq start (point))
    (goto-char end) (hif-end-of-line) (setq end (point))
    (hide-ifdef-region-internal start end)))

(defun hif-show-ifdef-region (start end)
  "Everything between START and END is made visible."
  (let ((onum (length (overlays-in start end))))
    (remove-overlays start end 'hide-ifdef t)
    (/= onum (length (overlays-in start end)))))

;;===%%SF%% evaluation (Start)  ===

;; It is not useful to set this to anything but `eval'.
;; In fact, the variable might as well be eliminated.
(defvar hide-ifdef-evaluator 'eval
  "The function to use to evaluate a form.
The evaluator is given a canonical form and returns t if text under
that form should be displayed.")

(defvar hif-undefined-symbol nil
  "...is by default considered to be false.")


(defun hif-set-var (var value)
  "Prepend (var value) pair to hide-ifdef-env."
  (setq hide-ifdef-env (cons (cons var value) hide-ifdef-env)))

(defun hif-table-lookup (var table)
  (let* ((sa (assoc var table))
         (val (cdr sa)))
    (if (null sa)
        hif-undefined-symbol
      val)))

(defun hif-lookup (var)
  ;; (message "hif-lookup %s" var)
  (hif-table-lookup var hide-ifdef-env))

(defun hif-defined (var)
   (if (assoc var hide-ifdef-env) 1 0))

;;===%%SF%% evaluation (End)  ===



;;===%%SF%% parsing (Start)  ===
;;;  The code that understands what ifs and ifdef in files look like.

(defconst hif-cpp-prefix "\\(^\\|\r\\)[ \t]*#[ \t]*")
(defconst hif-ifxdef-regexp (concat hif-cpp-prefix "if\\(n\\)?def"))
(defconst hif-ifndef-regexp (concat hif-cpp-prefix "ifndef"))
(defconst hif-ifx-regexp (concat hif-cpp-prefix "if\\(n?def\\)?[ \t]+"))
(defconst hif-elif-regexp (concat hif-cpp-prefix "elif"))
(defconst hif-else-regexp (concat hif-cpp-prefix "else"))
(defconst hif-endif-regexp (concat hif-cpp-prefix "endif"))
(defconst hif-ifx-else-endif-regexp
  (concat hif-ifx-regexp "\\|" hif-elif-regexp "\\|" hif-else-regexp "\\|" hif-endif-regexp))
(defconst hif-macro-expr-prefix-regexp (concat hif-cpp-prefix "\\(if\\(n?def\\)?\\|elif\\|define\\)[ \t]+"))

(defconst hif-white     "[ \t]*")
(defconst hif-define    (concat hif-cpp-prefix "\\(define\\|undef\\)"))
(defconst hif-id        (concat "[A-Za-z_][0-9A-Za-z_]*"))
(defconst hif-macroname (concat hif-white "\\(" hif-id "\\)" hif-white
                                "\\("
                                "(" hif-white
                                  "\\(" hif-id "\\)?" hif-white
                                  "\\(" "," hif-white hif-id hif-white "\\)*"
                                  "\\(\\.\\.\\.\\)?" hif-white
                                ")"
                                "\\)?" ))

;; Used to store the current token and the whole token list during parsing.
;; Only bound dynamically.
(defvar hif-token)
(defvar hif-token-list)

(defconst hif-token-alist
  '(("||"  . hif-or)
    ("&&"  . hif-and)
    ("|"   . hif-logior)
    ("^"   . hif-logxor)
    ("&"   . hif-logand)
    ("<<"  . hif-shiftleft)
    (">>"  . hif-shiftright)
    ("=="  . hif-equal)
    ("="   . hif-assign)
    ("!="  . hif-notequal)
    ("##"  . hif-token-concat)
    ("!"   . hif-not)
    ("~"   . hif-lognot)
    ("("   . hif-lparen)
    (")"   . hif-rparen)
    (">"   . hif-greater)
    ("<"   . hif-less)
    (">="  . hif-greater-equal)
    ("<="  . hif-less-equal)
    ("+"   . hif-plus)
    ("-"   . hif-minus)
    ("*"   . hif-multiply)
    ("/"   . hif-divide)
    ("%"   . hif-modulo)
    ("?"   . hif-conditional)
    (":"   . hif-colon)
    (","   . hif-comma)
    ("#"   . hif-stringify)
    ("..." . hif-etc)))

(defconst hif-valid-token-list (mapcar 'cdr hif-token-alist))

(defconst hif-token-regexp
  (concat (regexp-opt (mapcar 'car hif-token-alist))
          "\\|0x[0-9a-fA-F]+\\.?[0-9a-fA-F]*" ;; Luke added
          "\\|[0-9]+\\.?[0-9]*"  ;; Luke added, decimal/octal
          "\\|\\w+"))

(defconst hif-string-literal-regexp
  (concat hif-white "\\(\"[^\"]*\"\\)"))

(defvar hif-simple-token-only nil)

(defun hif-tokenize (start end)
  "Separate string between START and END into a list of tokens."
  (let ((token-list nil))
    (setq hif-simple-token-only t)
    (with-syntax-table hide-ifdef-syntax-table
      (save-excursion
        (goto-char start)
        (while (progn (forward-comment (point-max)) (< (point) end))
          ;; (message "expr-start = %d" expr-start) (sit-for 1)
          (cond
           ((looking-at "\\\\\n")
            (forward-char 2))

           ((looking-at hif-string-literal-regexp)
            (push (substring-no-properties (match-string 1)) token-list)
            (goto-char (match-end 0)))

           ((looking-at hif-token-regexp)
            (let ((token (buffer-substring-no-properties (point) (match-end 0))))
              (goto-char (match-end 0))
              ;; (message "token: %s" token) (sit-for 1)
              (push (or (cdr (assoc token hif-token-alist))
                        (if (string-equal token "defined") 'hif-defined)
                        (if (string-match "0x\\([a-fA-F0-9]+\\)" token)
                            (string-to-number (match-string 1 token) 16)) ;; hexadecimal
                        (if (string-match "\\`0[0-9]*\\(\\.[0-9]+\\)?\\'" token)
                            (string-to-number token 8)) ;; octal
                        (if (string-match "\\`[1-9][0-9]*\\(\\.[0-9]+\\)?\\'" token)
                            (string-to-number token)) ;; decimal
                        (prog1 (intern token)
                          (setq hif-simple-token-only nil)))
                    token-list)))

           ((looking-at "\r")
            (forward-char 1))

           (t (error "Bad #if expression: %s" (buffer-string)))))))
    (nreverse token-list)))

;;;-----------------------------------------------------------------
;;; Translate C preprocessor #if expressions using recursive descent.
;;; This parser is limited to the operators &&, ||, !, and "defined".
;;; Added ==, !=, +, and -.  Gary Oberbrunner, garyo@avs.com, 8/9/94
;;;
;;; Implement the C language operator precedence table. Add all those
;;; missing operators that could be used in macros. Luke Lee 2013-09-04

;;;  | Operator Type        | Operator                                    | Associativity |
;;;  +----------------------+---------------------------------------------+---------------+
;;;  | Primary Expression   | () [] . -> expr++ expr--                    | left-to-right |
;;;  | Unary Operators      | * & + - ! ~ ++expr --expr (typecast) sizeof | right-to-left |
;;;  | Binary Operators     | * / %                                       | left-to-right |
;;;  |                      | + -                                         |               |
;;;  |                      | >> <<                                       |               |
;;;  |                      | < > <= >=                                   |               |
;;;  |                      | == !=                                       |               |
;;;  |                      | &                                           |               |
;;;  |                      | ^                                           |               |
;;;  |                      | |                                           |               |
;;;  |                      | &&                                          |               |
;;;  |                      | ||                                          |               |
;;;  | Ternary Operator     | ?:                                          | right-to-left |
;;; x| Assignment Operators | = += -= *= /= %= >>= <<= &= ^= =            | right-to-left |
;;;  | Comma                | ,                                           | left-to-right |


(defsubst hif-nexttoken ()
  "Pop the next token from token-list into the let variable \"hif-token\"."
  (setq hif-token (pop hif-token-list)))

(defsubst if-valid-identifier (id)
  (not (or (numberp id)
           (stringp id))))

(defun hif-define-operator (tokens)
  "`Upgrade' hif-define xxx to '(hif-define xxx)' so that it won't be subsitituted"
  (let ((result nil)
        (tok nil))
    (while tokens
      (setq tok (car tokens)
            tokens (cdr tokens))
      (setq result
            (append
             result
             (list
              (if (eq tok 'hif-defined)
                  (progn
                    (setq tok (cadr tokens))
                    (if (eq (car tokens) 'hif-lparen)
                        (if (and (if-valid-identifier tok)
                                 (eq (caddr tokens) 'hif-rparen))
                            (setq tokens (cdddr tokens))
                          (error "operator \"defined\" requires an identifier"))
                      (setq tok (car tokens)
                            tokens (cdr tokens))
                      (unless (if-valid-identifier tok)
                        (error "operator \"defined\" requires an identifier")))
                    (list 'hif-defined 'hif-lparen tok 'hif-rparen))
                tok)))))
    result))

(defun hif-expand-token-list (tokens &optional macroname reflist) ;; reference list to prevent self-referencing
  "Perform expansion till everything expanded. No self-reference expansion."
  (catch 'self-referencing
    (let ((expanded nil)
          (remains (hif-define-operator
                    (hif-token-concatenation (hif-token-stringification tokens))))
          tok rep)
      (if macroname
          (setq reflist (cons macroname reflist)))
      ;; Expanding all tokens till list exhausted
      (while remains
        (setq tok (car remains)
              remains (cdr remains))
        (if (memq tok reflist) ;; for self-referencing tokens, don't expand it
            (throw 'self-referencing tokens))
        (setq expanded
              (append
               expanded
               (cond
                ((or (memq tok hif-valid-token-list)
                     (numberp tok)
                     (stringp tok))
                 (list tok))

                ((setq rep (hif-lookup tok))
                 (if (and (listp rep)
                          (eq (car rep) 'hif-define-macro)) ;; a defined macro
                     ;; recursively expand it
                     (if (cadr rep) ;; arglist is not nil
                         (if (eq (car remains) 'hif-lparen)
                             ;; Argumented macro, get arguments and invoke it
                             (let* ((hif-token-list (cdr remains)) ;; dynamic binding for hif-macro-supply-arguments
                                    (hif-token nil)                ;; dynamic binding for hif-macro-supply-arguments
                                    (parmlist (mapcar 'hif-expand-token-list
                                                      (hif-get-argument-list tok)))
                                    (result (hif-expand-token-list (hif-macro-supply-arguments tok parmlist)
                                                                   tok reflist)))
                               (setq remains (cons hif-token hif-token-list))
                               result)
                           ;; no argument supplied, no invocation for this macro
                           (list tok))
                       ;; arglist is nil, direct expansion
                       (setq rep (hif-expand-token-list (caddr rep) ;; macro's token list
                                                        tok reflist))
                       (setq remains (substitute tok rep remains)) ;; replace all remaining references immediately
                       (list rep))
                   ;; lookup tok returns an atom
                   (list rep)))

                ((= 1 (hif-defined tok)) ;; defined (hif-defined tok)=1, but empty (hif-lookup tok)=nil, thus remove this token
                 (setq remains (delete tok remains))
                 nil)

                (t ;; usual IDs
                 (list tok))))))
      (flatten expanded))))

(defun hif-parse-if-exp (token-list &optional macroname)
  "Parse the TOKEN-LIST.  Return translated list in prefix form."
  (let ((hif-token-list (hif-expand-token-list token-list macroname)))
    (hif-nexttoken)
    (prog1
        (and hif-token
             (hif-exprlist))
      (if hif-token ; is there still a token?
          (error "Error: unexpected token: %s" hif-token)))))

;;;
;;;To manually test these functions, try the following in scratch buffer:
;;;
;;;(hif-find-define (point)) ;; find only one #define
;;;(hif-add-new-defines) ;; find all #defines
;;;
;;;#define macro ( arg1 , arg2 , arg3 ... ) arg1 + arg2 * arg3
;;;
;;;(assoc 'macro hide-ifdef-env)
;;;
;;;(hif-canonicalize-tokens hif-ifx-regexp)
;;;(hif-expand-token-list (hif-canonicalize-tokens hif-ifx-regexp))
;;;(hif-canonicalize hif-ifx-regexp)
;;;
;;;#if macro( (a,2,3), b<<1, (c&7,d) )
;;;

(defun hif-exprlist ()
  "Parse an exprlist: expr { ',' expr}"
  (let ((result (hif-expr))
        comma)
    (while (eq hif-token 'hif-comma)
      (unless comma
        (setq result (list result)))
      (setq comma t)
      (hif-nexttoken)
      (setq result (append result (list (hif-expr)))))
    (if comma
        (append (list 'hif-comma) result)
      result)))

(defun hif-expr ()
  "Parse an expression as found in #if.
       expr : or-expr | or-expr '?' expr ':' expr."
  (let ((result (hif-or-expr))
        middle)
    (while (eq hif-token 'hif-conditional)
      (hif-nexttoken)
      (setq middle (hif-expr))
      (if (eq hif-token 'hif-colon)
          (progn
            (hif-nexttoken)
            (setq result (list 'hif-conditional result middle (hif-expr))))
        (error "Error: unexpected token: %s" hif-token)))
    result))

(defun hif-or-expr ()
  "Parse an or-expr : and-expr | or-expr '||' and-expr."
  (let ((result (hif-and-expr)))
    (while (eq hif-token 'hif-or)
      (hif-nexttoken)
      (setq result (list 'hif-or result (hif-and-expr))))
  result))

(defun hif-and-expr ()
  "Parse an and-expr : logior-expr | and-expr '&&' logior-expr."
  (let ((result (hif-logior-expr)))
    (while (eq hif-token 'hif-and)
      (hif-nexttoken)
      (setq result (list 'hif-and result (hif-logior-expr))))
    result))

(defun hif-logior-expr ()
  "Parse a logor-expr : logxor-expr | logor-expr '|' logxor-expr."
  (let ((result (hif-logxor-expr)))
    (while (eq hif-token 'hif-logior)
      (hif-nexttoken)
      (setq result (list 'hif-logior result (hif-logxor-expr))))
    result))

(defun hif-logxor-expr ()
  "Parse a logxor-expr : logand-expr | logxor-expr '^' logand-expr."
  (let ((result (hif-logand-expr)))
    (while (eq hif-token 'hif-logxor)
      (hif-nexttoken)
      (setq result (list 'hif-logxor result (hif-logand-expr))))
    result))

(defun hif-logand-expr ()
  "Parse a logand-expr : eq-expr | logand-expr '&' eq-expr."
  (let ((result (hif-eq-expr)))
    (while (eq hif-token 'hif-logand)
      (hif-nexttoken)
      (setq result (list 'hif-logand result (hif-eq-expr))))
    result))

(defun hif-eq-expr ()
  "Parse an eq-expr : comp | eq-expr `=='|`!=' comp."
  (let ((result (hif-comp-expr))
        (eq-token nil))
    (while (memq hif-token '(hif-equal hif-notequal))
      (setq eq-token hif-token)
      (hif-nexttoken)
      (setq result (list eq-token result (hif-comp-expr))))
    result))

(defun hif-comp-expr ()
  "Parse a comp-expr : logshift | comp-expr `<'|`>'|`>='|`<=' logshift."
  (let ((result (hif-logshift-expr))
        (comp-token nil))
    (while (memq hif-token '(hif-greater hif-less hif-greater-equal hif-less-equal))
      (setq comp-token hif-token)
      (hif-nexttoken)
      (setq result (list comp-token result (hif-logshift-expr))))
    result))

(defun hif-logshift-expr ()
  "Parse a logshift : math | logshift `<<'|`>>' math."
  (let ((result (hif-math))
        (shift-token nil))
    (while (memq hif-token '(hif-shiftleft hif-shiftright))
      (setq shift-token hif-token)
      (hif-nexttoken)
      (setq result (list shift-token result (hif-math))))
    result))

(defun hif-math ()
  "Parse an expression with + or -.
       math : muldiv | math '+|-' muldiv."
  (let ((result (hif-muldiv-expr))
        (math-op nil))
    (while (memq hif-token '(hif-plus hif-minus))
      (setq math-op hif-token)
      (hif-nexttoken)
      (setq result (list math-op result (hif-muldiv-expr))))
    result))

(defun hif-muldiv-expr ()
  "Parse an expression with *,/,%.
       muldiv : factor | muldiv '*|/|%' factor."
  (let ((result (hif-factor))
        (math-op nil))
    (while (memq hif-token '(hif-multiply hif-divide hif-modulo))
      (setq math-op hif-token)
      (hif-nexttoken)
      (setq result (list math-op result (hif-factor))))
    result))

(defun hif-factor ()
  "Parse a factor: '!' factor | '~' factor | '(' expr ')' | 'defined(' id ')' | 'id(parmlist)' | strings | id."
  (cond
   ((eq hif-token 'hif-not)
    (hif-nexttoken)
    (list 'hif-not (hif-factor)))

   ((eq hif-token 'hif-lognot)
    (hif-nexttoken)
    (list 'hif-lognot (hif-factor)))

   ((eq hif-token 'hif-lparen)
    (hif-nexttoken)
    (let ((result (hif-exprlist)))
      (if (not (eq hif-token 'hif-rparen))
          (error "Bad token in parenthesized expression: %s" hif-token)
        (hif-nexttoken)
        result)))

   ((eq hif-token 'hif-defined)
    (hif-nexttoken)
    (let ((paren (when (eq hif-token 'hif-lparen) (hif-nexttoken) t))
          (ident hif-token))
      (if (memq hif-token '(or and not hif-defined hif-lparen hif-rparen))
          (error "Error: unexpected token: %s" hif-token))
      (when paren
        (hif-nexttoken)
        (unless (eq hif-token 'hif-rparen)
          (error "Error: expected \")\" after identifier")))
      (hif-nexttoken)
      `(hif-defined (quote ,ident))))

   ((numberp hif-token)
    (prog1 hif-token (hif-nexttoken)))

   ((stringp hif-token)
    (hif-string-concatenation))

   ;; Unary plus/minus.
   ((memq hif-token '(hif-minus hif-plus))
    (list (prog1 hif-token (hif-nexttoken)) 0 (hif-factor)))

   (t                                   ; identifier
    (let ((ident hif-token))
      (hif-nexttoken)
      (if (eq hif-token 'hif-lparen)
          (hif-place-macro-invocation ident)
        `(hif-lookup (quote ,ident)))))))

(defun hif-get-argument-list (ident)
  (let ((nest 0)
        (parmlist nil) ;; a "token" list of parameters, will later be parsed
        (parm nil))
    (while (or (not (eq (hif-nexttoken) 'hif-rparen))
               (/= nest 0))
      (if (eq (car parm) 'hif-comma)
          (setq parm nil))
      (cond
       ((eq hif-token 'hif-lparen)
        (setq nest (1+ nest)))
       ((eq hif-token 'hif-rparen)
        (setq nest (1- nest))
        (if (< nest 0)
            (error "Unbalanced parenthesis when invoking macro %S" ident)))
       ((and (eq hif-token 'hif-comma)
             (= nest 0))
        (setq parmlist (append parmlist (list parm))
              parm nil)))
      (setq parm (append parm (list hif-token))))
    (setq parmlist (append parmlist (list parm))) ; okay even if parm is nil
    (hif-nexttoken) ; drop the hif-rparen, get next token
    parmlist))

(defun hif-place-macro-invocation (ident)
  (let ((parmlist (hif-get-argument-list ident)))
    `(hif-invoke (quote ,ident) (quote ,parmlist))))

(defun hif-string-concatenation ()
  "Parse concatenated strings: string | strings string"
  (let ((result (substring-no-properties hif-token)))
    (while (stringp (hif-nexttoken))
      (setq result (concat
                    (substring result 0 -1)    ; remove trailing '"'
                    (substring hif-token 1)))) ; remove leading  '"'
    result))

(defun hif-define-macro (parmlist token-body)
  "A marker for defined macro with arguments, cannot be evaluated alone with no parameters inputed."
  ;; TODO: input parameters at run time, use minibuffer to query all parameters
  (error "Argumented macro cannot be evaluated without passing any parameter."))

(defun hif-stringify (a)
  "Stringify a number, string or symbol"
  (cond
   ((numberp a)
    (number-to-string a))
   ((atom a)
    (symbol-name a))
   ((stringp a)
    (concat "\"" a "\""))
   (t
    (error "Invalid token to symbolify"))))

(defun intern-safe (str)
  (if (stringp str)
      (intern str)))

(defun hif-merge-operator-token (a b)
  "Support weird (but valid) use of token concatenation. Here we take care only those that can be evaluated during
preprocessing time. For those computable syntax that can only be evaluated during C runtime (like '++','--','+='...),
we ignore them all here."
  (cond
   ((and (eq a 'hif-logior) (eq b 'hif-logior)) ;; '|''|' -> '||'
    'hif-or)
   ((and (eq a 'hif-logand) (eq b 'hif-logand)) ;; '&''&' -> '&&'
    'hif-and)
   ((and (eq a 'hif-assign) (eq b 'hif-assign)) ;; '=''=' -> '=='
    'hif-equal)
   ((and (eq a 'hif-not) (eq b 'hif-assign)) ;; '!''=' -> '!='
    'hif-notequal)
   ((eq a 'hif-less)
    (if (eq b 'hif-less) ;; '<''<' -> '<<'
        'hif-shiftleft
      (if (eq b 'hif-assign) ;; '<''=' -> '<='
          'hif-less-equal)))
   ((eq a 'hif-greater)
    (if (eq b 'hif-greater) ;; '>''>' -> '>>'
        'hif-shiftleft
      (if (eq b 'hif-assign) ;; '>''=' -> '>='
          'hif-greater-equal)))))

(defun hif-token-concat (a b)
  "Concatenation two symbol name into a longer symbol, currently support only simple symbol concatenation."
  (if (or (memq a hif-valid-token-list)
          (memq b hif-valid-token-list))
      (let ((result (hif-merge-operator-token a b)))
        (or result
            ;;(error "Invalid token to concatenate")
            (error "pasting \"%s\" and \"%s\" does not give a valid preprocessing token"
                   (if (memq a hif-valid-token-list) (car (rassoc a hif-token-alist)) (symbol-name a))
                   (if (memq b hif-valid-token-list) (car (rassoc b hif-token-alist)) (symbol-name b)))))
    (intern-safe (concat (hif-stringify a)
                         (hif-stringify b)))))

(defun hif-mathify (val)
  "Treat VAL as a number: if it's t or nil, use 1 or 0."
  (cond ((eq val t) 1)
        ((null val) 0)
        (t val)))

(defun hif-conditional (a b c)
  (if (not (zerop (hif-mathify a))) (hif-mathify b) (hif-mathify c)))

(defun hif-and (a b)
  (and (not (zerop (hif-mathify a))) (not (zerop (hif-mathify b)))))
(defun hif-or (a b)
  (or (not (zerop (hif-mathify a))) (not (zerop (hif-mathify b)))))
(defun hif-not (a)
  (zerop (hif-mathify a)))
(defun hif-lognot (a)
  (lognot a))

(defmacro hif-mathify-binop (fun)
  `(lambda (a b)
     ,(format "Like `%s' but treat t and nil as 1 and 0." fun)
     (,fun (hif-mathify a) (hif-mathify b))))

(defun shiftleft (a b)
  (if (< a 0)
      (ash a b)
    (lsh a b)))

(defun shiftright (a b)
  (if (< a 0)
      (ash a (- 0 b))
    (lsh a (- 0 b))))

(defalias 'hif-multiply      (hif-mathify-binop *))
(defalias 'hif-divide        (hif-mathify-binop /))
(defalias 'hif-modulo        (hif-mathify-binop %))
(defalias 'hif-plus          (hif-mathify-binop +))
(defalias 'hif-minus         (hif-mathify-binop -))
(defalias 'hif-equal         (hif-mathify-binop =))
(defalias 'hif-notequal      (hif-mathify-binop /=))
(defalias 'hif-greater       (hif-mathify-binop >))
(defalias 'hif-less          (hif-mathify-binop <))
(defalias 'hif-greater-equal (hif-mathify-binop >=))
(defalias 'hif-less-equal    (hif-mathify-binop <=))
(defalias 'hif-logior        (hif-mathify-binop logior))
(defalias 'hif-logxor        (hif-mathify-binop logxor))
(defalias 'hif-logand        (hif-mathify-binop logand))
(defalias 'hif-shiftleft     (hif-mathify-binop shiftleft))
(defalias 'hif-shiftright    (hif-mathify-binop shiftright))

(defun hif-comma (&rest expr)
  "Evaluate a list of expr, return the result of the last item"
  (let ((result nil))
    (dolist (e expr)
      (ignore-errors
        (setq result (funcall hide-ifdef-evaluator e))))
    result))

(defun hif-token-stringification (l)
  "Scan token list for 'hif-stringify' ('#') token and stringify the next token"
  (let (result)
    (while l
      (setq result
            (append result
                    (list
                     (if (eq (car l) 'hif-stringify)
                         (prog1
                             (if (cadr l)
                                 (hif-stringify (cadr l))
                               (error "No token to stringify"))
                           (setq l (cdr l)))
                       (car l)))))
      (setq l (cdr l)))
    result))

(defun hif-token-concatenation (l)
  "Scan token list for 'hif-token-concat' ('##') token and concatenate two tokens"
  (let ((prev nil)
        result)
    (while l
      (while (eq (car l) 'hif-token-concat)
        (unless prev
          (error "No token before ## to concatenate"))
        (unless (cdr l)
          (error "No token after ## to concatenate"))
        (setq prev (hif-token-concat prev (cadr l)))
        (setq l (cddr l)))
      (if prev
        (setq result (append result (list prev))))
      (setq prev (car l)
            l (cdr l)))
    (if prev
        (append result (list prev))
      result)))

(defun flatten (l)
  "Flatten a tree"
  (if (and l (atom l))
      (list l)
    (if (consp l)
        (nconc (flatten (car l)) (flatten (cdr l))))))

(defun delimit (l a)
  "Delmit a list 'l' with delimiter atom 'd'"
  (nconc (mapcan (lambda (l)
                   (list l a))  (butlast l))
         (last l)))

;; Perform token replacement and re-parse
;;  If we did not re-parse the macro body itself, the following condition currently produce incorrect result:
;;  #define macro(a,b,c...) a+b*c
;;  macro( 1,2,3,4,5 ) will produce 1+2*(3,4,5) = 11. It should be (1+2*3,4,5) = 5.
;;  The reason is because the comma ',' changed the precedence sequence. Therefore we need to reparse.
(defun hif-macro-supply-arguments (macro-name actual-parms)
  "Expand a macro function, replace 'actual-parms' to the function body."
  (let* ((SA                   (assoc macro-name hide-ifdef-env))
         (macro                (and SA
                                    (cdr SA)
                                    (eq (cadr SA) 'hif-define-macro)
                                    (cddr SA)))
         (formal-parms         (and macro (car macro)))
         (macro-body           (and macro (cadr macro)))
         (hide-ifdef-local-env nil) ;; dynamic binding local table
         actual-count
         formal-count
         actual
         formal
         etc)

    (when (and actual-parms formal-parms macro-body)
      ;; For each actual parameter, evaluate each one and associate it
      ;; with the associated actual parameter, put it into local table and finally
      ;; evaluate the macro body.
      (if (setq etc (eq (car formal-parms) 'hif-etc)) ;; Take care of 'hif-etc first. Prefix 'hif-comma back if needed.
          (setq formal-parms (cdr formal-parms)))
      (setq formal-count (length formal-parms)
            actual-count (length actual-parms))

      (if (> formal-count actual-count)
          (error "Too few parmameter for macro %S" macro-name)
        (if (< formal-count actual-count)
            (or etc
                (error "Too many parameters for macro %S" macro-name))))

      ;; Perform token replacement on the macro-body on the parameters
      (while (setq formal (car formal-parms))
        (setq formal-parms (cdr formal-parms))
        ;; Prevent repettative substitutation, thus cannot use 'subst'
        ;; for example:
        ;; #define mac(a,b) (a+b)
        ;; #define testmac mac(b,y)
        ;; testmac should expand to (b+y): replace of argument a and b occurs simultaneously
        ;; not sequentially. If this occurs sequentially according to the argument order,
        ;;  it will become: 1. formal parm #1 'a' replaced by actual parm 'b', thus (a+b) becomes (b+b)
        ;;  2. formal parm #2 'b' replaced by actual parm 'y', thus (b+b) becomes (y+y).
        (setq macro-body
              ;; Unlike 'subst', 'substitute' replace only the top level instead of the whole tree,
              ;; more importantly, it's not destructive.
              (if (and etc (null formal-parms))
                  (substitute (delimit actual-parms 'hif-comma) formal macro-body)
                (substitute (car actual-parms) formal macro-body)))
        (setq actual-parms (cdr actual-parms)))

      ;; Replacement completed, flattern the whole token list,
      (setq macro-body (flatten macro-body))

      ;; Stringification and token concatenation happens here
      (hif-token-concatenation (hif-token-stringification macro-body)))))

(defun hif-invoke (macro-name actual-parms)
  "Invoke a macro by first expanding it, then reparse the macro-body, finally invoke the macro."
    ;; Reparse the macro body and evaluate it
    (funcall hide-ifdef-evaluator
             (hif-parse-if-exp
              (hif-macro-supply-arguments macro-name actual-parms)
              macro-name)))

;;;----------- end of parser -----------------------

(defun hif-canonicalize-tokens (regexp) ;; for debugging
  "Return the expanded result of the scanned tokens."
  (save-excursion
    (re-search-forward regexp)
    (let* ((curr-regexp (match-string 0))
           (defined (string-match hif-ifxdef-regexp curr-regexp))
           (negate (and defined
                        (string= (match-string 2 curr-regexp) "n"))))
      (let* ((tokens (hif-tokenize (point)
                                   (progn (hif-end-of-line) (point)))))
        (if defined
            (setq tokens (list 'hif-defined tokens)))
        (if negate
            (setq tokens (list 'hif-not tokens)))
        tokens))))

(defun hif-canonicalize (regexp)
  "When at beginning of `regexp' (i.e. #ifX), return a Lisp expression for its condition."
  (let ((case-fold-search nil))
    (save-excursion
      (re-search-forward regexp)
      (let* ((curr-regexp (match-string 0))
             (defined (string-match hif-ifxdef-regexp curr-regexp))
             (negate (and defined
                          (string= (match-string 2 curr-regexp) "n")))
             (tokens (hif-tokenize (point)
                                   (progn (hif-end-of-line) (point)))))
        (if defined
            (setq tokens (list 'hif-defined tokens)))
        (if negate
            (setq tokens (list 'hif-not tokens)))
        (hif-parse-if-exp tokens)))))

(defun hif-find-any-ifX ()
  "Move to next #if..., or #ifndef, at point or after."
  ;; (message "find ifX at %d" (point))
  (prog1
      (re-search-forward hif-ifx-regexp (point-max) t)
    (beginning-of-line)))

(defun hif-find-next-relevant ()
  "Move to next #if..., #elif..., #else, or #endif, after the current line."
  ;; (message "hif-find-next-relevant at %d" (point))
  (end-of-line)
  ;; avoid infinite recursion by only going to beginning of line if match found
  (if (re-search-forward hif-ifx-else-endif-regexp (point-max) t)
      (beginning-of-line)))

(defun hif-find-previous-relevant ()
  "Move to previous #if..., #else, or #endif, before the current line."
  ;; (message "hif-find-previous-relevant at %d" (point))
  (beginning-of-line)
  ;; avoid infinite recursion by only going to beginning of line if match found
  (if (re-search-backward hif-ifx-else-endif-regexp (point-min) t)
     (beginning-of-line)))


(defun hif-looking-at-ifX ()            ;; Should eventually see #if
  (looking-at hif-ifx-regexp))
(defun hif-looking-at-endif ()
  (looking-at hif-endif-regexp))
(defun hif-looking-at-else ()
  (looking-at hif-else-regexp))
(defun hif-looking-at-elif ()
  (looking-at hif-elif-regexp))



(defun hif-ifdef-to-endif ()
  "If positioned at #ifX, #elif, or #else form, skip to corresponding #endif."
  ;; (message "hif-ifdef-to-endif at %d" (point)) (sit-for 1)
  (hif-find-next-relevant)
  (cond ((hif-looking-at-ifX)
         (hif-ifdef-to-endif) ; find endif of nested if
         (hif-ifdef-to-endif)) ; find outer endif or else
        ((hif-looking-at-elif)
         (hif-ifdef-to-endif))
        ((hif-looking-at-else)
         (hif-ifdef-to-endif)) ; find endif following else
        ((hif-looking-at-endif)
         'done)
        (t
         (error "Mismatched #ifdef #endif pair"))))


(defun hif-endif-to-ifdef ()
  "If positioned at #endif form, skip backward to corresponding #ifX."
  ;; (message "hif-endif-to-ifdef at %d" (point))
  (let ((start (point)))
    (hif-find-previous-relevant)
    (if (= start (point))
        (error "Mismatched #ifdef #endif pair")))
  (cond ((hif-looking-at-endif)
         (hif-endif-to-ifdef) ; find beginning of nested if
         (hif-endif-to-ifdef)) ; find beginning of outer if or else
        ((hif-looking-at-elif)
         (hif-endif-to-ifdef))
        ((hif-looking-at-else)
         (hif-endif-to-ifdef))
        ((hif-looking-at-ifX)
         'done)
        (t
         (error "Mismatched #endif"))))                 ; never gets here


(defun forward-ifdef (&optional arg)
  "Move point to beginning of line of the next ifdef-endif.
With argument, do this that many times."
  (interactive "p")
  (or arg (setq arg 1))
  (if (< arg 0) (backward-ifdef (- arg))
    (while (< 0 arg)
      (setq arg (- arg))
      (let ((start (point)))
        (unless (hif-looking-at-ifX)
          (hif-find-next-relevant))
        (if (hif-looking-at-ifX)
            (hif-ifdef-to-endif)
          (goto-char start)
          (error "No following #ifdef"))))))


(defun backward-ifdef (&optional arg)
  "Move point to beginning of the previous ifdef-endif.
With argument, do this that many times."
  (interactive "p")
  (or arg (setq arg 1))
  (if (< arg 0) (forward-ifdef (- arg))
    (while (< 0 arg)
      (setq arg (1- arg))
      (beginning-of-line)
      (let ((start (point)))
        (unless (hif-looking-at-endif)
          (hif-find-previous-relevant))
        (if (hif-looking-at-endif)
            (hif-endif-to-ifdef)
          (goto-char start)
          (error "No previous #ifdef"))))))


(defun down-ifdef ()
  "Move point to beginning of nested ifdef or else-part."
    (interactive)
    (let ((start (point)))
      (hif-find-next-relevant)
      (if (or (hif-looking-at-ifX) (hif-looking-at-else))
          ()
        (goto-char start)
        (error "No following #ifdef"))))


(defun up-ifdef ()
  "Move point to beginning of enclosing ifdef or else-part."
  (interactive)
  (beginning-of-line)
  (let ((start (point)))
    (unless (hif-looking-at-endif)
      (hif-find-previous-relevant))
    (if (hif-looking-at-endif)
        (hif-endif-to-ifdef))
      (if (= start (point))
          (error "No previous #ifdef"))))

(defun next-ifdef (&optional arg)
  "Move to the beginning of the next #ifX, #else, or #endif.
With argument, do this that many times."
  (interactive "p")
  (or arg (setq arg 1))
  (if (< arg 0) (previous-ifdef (- arg))
    (while (< 0 arg)
      (setq arg (1- arg))
      (hif-find-next-relevant)
      (when (eolp)
        (beginning-of-line)
        (error "No following #ifdefs, #elses, or #endifs")))))

(defun previous-ifdef (&optional arg)
  "Move to the beginning of the previous #ifX, #else, or #endif.
With argument, do this that many times."
  (interactive "p")
  (or arg (setq arg 1))
  (if (< arg 0) (next-ifdef (- arg))
    (while (< 0 arg)
      (setq arg (1- arg))
      (let ((start (point)))
        (hif-find-previous-relevant)
        (if (= start (point))
            (error "No previous #ifdefs, #elses, or #endifs"))))))


;;===%%SF%% parsing (End)  ===


;;===%%SF%% hide-ifdef-hiding (Start)  ===


;;; A range is a structure with four components:
;;; START       The start of the range. (beginning of line)
;;; ELSE        The else marker (beginning of line)
;;; END         The end of the range.  (beginning of line)
;;; ELIF        A sequence of #elif markers (beginning of line)

(defsubst hif-make-range (start end &optional else elif)
  (list start else end elif))

(defsubst hif-range-start (range) (elt range 0))
(defsubst hif-range-else (range) (elt range 1))
(defsubst hif-range-end (range) (elt range 2))
(defsubst hif-range-elif (range) (elt range 3))


;;; Find-Range
;;; The workhorse, it delimits the #if region.  Reasonably simple:
;;; Skip until an #else or #endif is found, remembering positions.  If
;;; an #else was found, skip some more, looking for the true #endif.

(defun hif-find-range ()
  "Return a Range structure describing the current #if region.
Point is left unchanged."
  ;; (message "hif-find-range at %d" (point))
  (save-excursion
    (beginning-of-line)
    (let ((start (point))
          (elif nil)
          (else nil)
          (end nil))
      ;; Part one.  Look for either #elif, #else or #endif.
      ;; This loop-and-a-half dedicated to E. Dijkstra.
      (while (and (not else) (not end))
        (while (progn
                 (hif-find-next-relevant)
                 (hif-looking-at-ifX))          ; Skip nested ifdef
          (hif-ifdef-to-endif))
        ;; Found either a #else, #elif, or an #endif.
        (cond ((hif-looking-at-elif)
               (setq elif (nconc elif (list (point)))))
              ((hif-looking-at-else)
               (setq else (point)))
              (t
               (setq end (point)))))
      ;; If found #else, look for #endif.
      (when else
        (while (progn
                 (hif-find-next-relevant)
                 (hif-looking-at-ifX))  ; Skip nested ifdef
          (hif-ifdef-to-endif))
        (if (hif-looking-at-else)
            (error "Found two elses in a row?  Broken!"))
        (setq end (point)))            ; (line-end-position)
      (hif-make-range start end else elif))))


;;; A bit slimy.

(defun hif-hide-line (point)
  "Hide the line containing point.  Does nothing if `hide-ifdef-lines' is nil."
  (when hide-ifdef-lines
    (save-excursion
      (goto-char point)
      (hide-ifdef-region-internal
       (line-beginning-position) (progn (hif-end-of-line) (point))))))


;;;  Hif-Possibly-Hide
;;;  There are four cases.  The #ifX expression is "taken" if it
;;;  the hide-ifdef-evaluator returns T.  Presumably, this means the code
;;;  inside the #ifdef would be included when the program was
;;;  compiled.
;;;
;;;  Case 1:  #ifX taken, and there's an #else.
;;;     The #else part must be hidden.  The #if (then) part must be
;;;     processed for nested #ifX's.
;;;  Case 2:  #ifX taken, and there's no #else.
;;;     The #if part must be processed for nested #ifX's.
;;;  Case 3:  #ifX not taken, and there's an #elif
;;;     The #if part must be hidden, and then evaluate
;;;     the #elif condition like a new #ifX.
;;;  Case 4:  #ifX not taken, and there's just an #else.
;;;     The #if part must be hidden.  The #else part must be processed
;;;     for nested #ifs.
;;;  Case 5:  #ifX not taken, and there's no #else.
;;;     The #ifX part must be hidden.
;;;
;;;  Further processing is done by narrowing to the relevant region
;;;  and just recursively calling hide-ifdef-guts.
;;;
;;;  When hif-possibly-hide returns, point is at the end of the
;;;  possibly-hidden range.

(defun hif-recurse-on (start end &optional dont-go-eol)
  "Call `hide-ifdef-guts' after narrowing to end of START line and END line."
  (save-excursion
    (save-restriction
      (goto-char start)
      (unless dont-go-eol
        (end-of-line))
      (narrow-to-region (point) end)
      (hide-ifdef-guts))))

(defun hif-possibly-hide ()
  "Called at #ifX expression, this hides those parts that should be hidden.
It uses the judgment of `hide-ifdef-evaluator'."
  ;; (message "hif-possibly-hide") (sit-for 1)
  (let* ((case-fold-search nil)
         (test (hif-canonicalize hif-ifx-regexp))
         (range (hif-find-range))
         (elifs (hif-range-elif range))
         (complete nil))
    ;; (message "test = %s" test) (sit-for 1)

    (hif-hide-line (hif-range-end range))
    (while (not complete)
      (if (hif-not (funcall hide-ifdef-evaluator test))
          ;; ifX/elif is FALSE
          (if elifs
              ;; Case 3 - Hide the #ifX and eval #elif
              (let ((newstart (car elifs)))
                (hif-hide-line (hif-range-start range))
                (hide-ifdef-region (hif-range-start range)
                                   (1- newstart))
                (setcar range newstart)
                (goto-char newstart)
                (setq elifs (cdr elifs))
                (setq test (hif-canonicalize hif-elif-regexp)))

            ;; Check for #else
            (cond ((hif-range-else range)
                   ;; Case 4 - #else block visible
                   (hif-hide-line (hif-range-else range))
                   (hide-ifdef-region (hif-range-start range)
                                      (1- (hif-range-else range)))
                   (hif-recurse-on (hif-range-else range)
                                   (hif-range-end range)))
                  (t
                   ;; Case 5 - No #else block, hide #ifX
                   (hide-ifdef-region (point)
                                      (1- (hif-range-end range)))))
            (setq complete t))

        ;; ifX/elif is TRUE
        (cond (elifs
               ;; Luke fix: distinguish from #elif..#elif to #elif..#else [2013-08-30 14:10:40 +0800]
               (let ((elif (car elifs)))
                   ;; hide all elifs
                   (hif-hide-line elif)
                   (hide-ifdef-region elif (1- (hif-range-end range)))
                   (hif-recurse-on (hif-range-start range)
                                   elif)))
              ((hif-range-else range)
               ;; Case 1 - Hide #elif and #else blocks, recurse #ifX
               (hif-hide-line (hif-range-else range))
               (hide-ifdef-region (hif-range-else range)
                                  (1- (hif-range-end range)))
               (hif-recurse-on (hif-range-start range)
                               (hif-range-else range)))

              (t
               ;; Case 2 - No #else, just recurse #ifX
               (hif-recurse-on (hif-range-start range)
                               (hif-range-end range))))
        (setq complete t)))

    ;; complete = t
    (hif-hide-line (hif-range-start range)) ; Always hide start.
    (goto-char (hif-range-end range))
    (end-of-line)))


(defun hif-evaluate-region (start end)
  (let* ((tokens (ignore-errors ;; prevent C statement things like 'do { ... } while (0)'
                   (hif-tokenize start end)))
         (expr (and tokens
                    (condition-case nil
                        (hif-parse-if-exp tokens)
                      (error
                       tokens))))
         (result (funcall hide-ifdef-evaluator expr)))
    result))

(defun hif-evaluate-macro (rstart rend)
  "Evaluate the macro expansion result for a region. If no region active,
find the current #ifdefs and evaluate the result. Currently it support only
math calculation, no string or argumented macros can be expanded."
  (interactive "r")
  (let ((case-fold-search nil))
    (save-excursion
      (unless mark-active
        (setq rstart nil rend nil)
        (beginning-of-line)
        (when (and (re-search-forward hif-macro-expr-prefix-regexp nil t)
                   (string= "define" (match-string 2)))
          (re-search-forward hif-macroname nil t)))
      (let* ((start (or rstart (point)))
             (end   (or rend (progn (hif-end-of-line) (point))))
             (defined nil)
             (simple 't)
             (tokens (ignore-errors ;; prevent C statement things like 'do { ... } while (0)'
                       (hif-tokenize start end)))
             (expr (or (and (<= (length tokens) 1) ;; simple token
                            (setq defined (assoc (car tokens) hide-ifdef-env))
                            (setq simple (atom (hif-lookup (car tokens))))
                            (hif-lookup (car tokens)))
                       (and tokens
                            (condition-case nil
                                (hif-parse-if-exp tokens)
                              (error
                               nil)))))
             (result (funcall hide-ifdef-evaluator expr))
             (exprstring (replace-regexp-in-string
                          "^[ \t]*\\([^ \t]+\\)[ \t]*" "\\1"  ;; trim off leading/trailing whites
                          (replace-regexp-in-string
                           "\\(//.*\\)" "" ;; trim off end-of-line comments
                           (buffer-substring-no-properties start end)))))
        (cond
         ((and (<= (length tokens) 1) simple) ;; simple token
          (if defined
              (message "%S <= `%s'" result exprstring)
            (message "`%s' is not defined" exprstring)))
         ((integerp result)
          (if (or (= 0 result) (= 1 result))
              (message "%S <= `%s'" result exprstring)
            (message "%S (0x%x) <= `%s'" result result exprstring)))
         ((null result) (message "%S <= `%s'" 'false exprstring))
         ((eq t result) (message "%S <= `%s'" 'true exprstring))
         (t (message "%S <= `%s'" result exprstring)))
        result))))

(defun hif-parse-macro-arglist (str)
  "Parse argument list formatted as '( arg1 [ , argn] [...] )', includeing the '...'
Return a list of the arguments, if '...' exists the first arg will be hif-etc."
  (let ((tokenlist (cdr (hif-tokenize (- (point) (length str)) (point)))) ; remove hif-lparen
        result token)
    (while (not (eq (setq token (car tokenlist)) 'hif-rparen))
      (setq tokenlist (cdr tokenlist))
      (cond
       ((eq token 'hif-etc)
        (setq result (cons 'hif-etc result)))
       ((eq token 'hif-comma)
        t)
       (t
        (setq result (append result (cons token nil))))))
    result))

(defun hif-find-define (&optional min max)
  (interactive)
  (and min (goto-char min))
  (and (re-search-forward hif-define max t)
       (or
        (let* ((defining (string= "define" (match-string 2)))
               (name (and (re-search-forward hif-macroname max t)
                          (match-string 1)))
               (parsed nil)
               (parmlist (and (match-string 3) ;; first arg id found
                              (hif-parse-macro-arglist (match-string 2)))))
          (if defining
              ;; ignore name (still need to return 't), or define the name
              (or (and hide-ifdef-exclude-define-regexp-pattern
                       (string-match hide-ifdef-exclude-define-regexp-pattern name))

                  (let* ((start (point))
                         (end   (progn (hif-end-of-line) (point)))
                         (tokens (and name
                                      (condition-case nil ;; prevent C statement things like 'do { ... } while (0)'
                                          (hif-tokenize start end)
                                        (error ;; we can't just return nil since this will stop searching for the remaining #defines.
                                         (setq hif-simple-token-only t)
                                         (buffer-substring-no-properties start end)))))
                         ;; For simple token we save only the parsed result;
                         ;; or we save the tokens, parse it after parameter replacement
                         (expr (and tokens
                                    (or (and hif-simple-token-only
                                             (= (length tokens) 1)
                                             (hif-parse-if-exp tokens))
                                        `(hif-define-macro ,parmlist ,tokens))))
                         (SA (and name
                                  (assoc (intern name) hide-ifdef-env))))
                    (and name
                         (if SA
                             (setcdr SA expr) ;; Lazy evaluation, eval only if hif-lookup find it
                           ;; define it anyway, even if nil it's still in list and therefore considerred defined
                           (push (cons (intern name) expr) hide-ifdef-env)))))
            (and name
                 (hif-undefine-symbol (intern name))))))
       t))

(defun hif-add-new-defines (&optional min max)
  "Scan and add all #define macros between min .. max"
  (interactive)
  (while (hif-find-define min max)
    (setf min (point)))
  (if max (goto-char max)
    (goto-char (point-max))))

;;(defun mark-region (min max)
;;  (set-mark (or max (point-max)))
;;  (goto-char min))

(defun hide-ifdef-guts ()
  "Does most of the work of `hide-ifdefs'.
It does not do the work that's pointless to redo on a recursive entry."
  ;; (message "hide-ifdef-guts")
  (save-excursion
    (let ((case-fold-search nil)
          min max)
      (goto-char (point-min))
      (setf min (point))
      (loop do
            (setf max (hif-find-any-ifX))
            (save-excursion
              (save-restriction
                ;; (mark-region min max) ;; for debugging
                (hif-add-new-defines min max)))
            (if max
                (hif-possibly-hide))
            (setf min (point))
            while max))))

;;===%%SF%% hide-ifdef-hiding (End)  ===


;;===%%SF%% exports (Start)  ===

(defun hide-ifdef-toggle-read-only ()
  "Toggle `hide-ifdef-read-only'."
  (interactive)
  (setq hide-ifdef-read-only (not hide-ifdef-read-only))
  (message "Hide-Read-Only %s"
           (if hide-ifdef-read-only "ON" "OFF"))
  (if hide-ifdef-hiding
      (setq buffer-read-only (or hide-ifdef-read-only hif-outside-read-only)))
  (force-mode-line-update))

(defun hide-ifdef-toggle-outside-read-only ()
  "Replacement for `toggle-read-only' within Hide-Ifdef mode."
  (interactive)
  (setq hif-outside-read-only (not hif-outside-read-only))
  (message "Read only %s"
           (if hif-outside-read-only "ON" "OFF"))
  (setq buffer-read-only
        (or (and hide-ifdef-hiding hide-ifdef-read-only)
            hif-outside-read-only))
  (force-mode-line-update))

(defun hide-ifdef-toggle-shadowing ()
  "Toggle shadowing."
  (interactive)
  (set (make-local-variable 'hide-ifdef-shadow) (not hide-ifdef-shadow))
  (message "Shadowing %s" (if hide-ifdef-shadow "ON" "OFF"))
  (save-restriction
    (widen)
    (dolist (overlay (overlays-in (point-min) (point-max)))
      (when (overlay-get overlay 'hide-ifdef)
        (if hide-ifdef-shadow
            (progn
              (overlay-put overlay 'invisible nil)
              (overlay-put overlay 'face 'hide-ifdef-shadow))
          (overlay-put overlay 'face nil)
          (overlay-put overlay 'invisible 'hide-ifdef))))))

(defun hide-ifdef-define (var val)
  "Define a VAR, optionally to a specific value, so that #ifdef VAR
would be included."
  (interactive
   (let* ((default (save-excursion
                     (beginning-of-line)
                     (cond ((looking-at hif-ifx-else-endif-regexp)
                            (forward-word 2)
                            (current-word 'strict))
                           (t
                            nil))))
         (var (read-minibuffer "Define what? " default))
         (val (read-from-minibuffer (format "Set %s to? (default 1): " var)
                                    nil nil t nil "1")))
     (list var val)))
   (hif-set-var var (or val 1))
   (message "%s set to %s" var (or val 1))
   (sleep-for 1)
   (if hide-ifdef-hiding (hide-ifdefs)))

(defun hif-undefine-symbol (var)
  (setq hide-ifdef-env
        (delete (assoc var hide-ifdef-env) hide-ifdef-env)))

;;(defun hide-ifdef-undef (var)
;;  "Undefine a VAR so that #ifdef VAR would not be included."
;;  (interactive "SUndefine what? ")
;;  ;;(hif-set-var var nil);;Luke fixed: set it nil is still considered defined so #ifdef VAR is still true.
;;  (hif-undefine-symbol var)
;;  (if hide-ifdef-hiding (hide-ifdefs)))

(defun hide-ifdef-undef (start end)
  "Undefine a VAR so that #ifdef VAR would not be included."
  (interactive "r")
  (let* ((symstr
          (or (and mark-active
                   (buffer-substring-no-properties start end))
              (read-string "Undefine what? " (current-word))))
         (sym (and symstr
                   (intern symstr))))
    (if (zerop (hif-defined sym))
        (message "`%s' not defined, no need to undefine it" symstr)
      (hif-undefine-symbol sym)
      (if hide-ifdef-hiding (hide-ifdefs))
      (message "`%S' undefined" sym))))

(defun hide-ifdefs (&optional nomsg)
  "Hide the contents of some #ifdefs.
Assume that defined symbols have been added to `hide-ifdef-env'.
The text hidden is the text that would not be included by the C
preprocessor if it were given the file with those symbols defined.
If this command is prefixed, hide also the #ifdefs themselves.

Turn off hiding by calling `show-ifdefs'."

  (interactive)
  (let ((hide-ifdef-lines current-prefix-arg))
    (or nomsg
        (message "Hiding..."))
    (setq hif-outside-read-only buffer-read-only)
    (unless hide-ifdef-mode (hide-ifdef-mode 1)) ; turn on hide-ifdef-mode
    (if hide-ifdef-hiding
        (show-ifdefs))                    ; Otherwise, deep confusion.
    (setq hide-ifdef-hiding t)
    (hide-ifdef-guts)
    (setq buffer-read-only (or hide-ifdef-read-only hif-outside-read-only))
    (or nomsg
        (message "Hiding done"))))


(defun show-ifdefs ()
  "Cancel the effects of `hide-ifdef': show the contents of all #ifdefs."
  (interactive)
  (setq buffer-read-only hif-outside-read-only)
  (hif-show-all)
  (setq hide-ifdef-hiding nil))


(defun hif-find-ifdef-block ()
  "Utility for hide and show `ifdef-block'.
Return as (TOP . BOTTOM) the extent of ifdef block."
  (if (fboundp 'c-backward-conditional-with-else)
      ;;Use my modified functions in cc-mode.
      (save-excursion
        (cons
         (progn
           (if (= (line-number-at-pos)
                  (save-excursion
                    (beginning-of-line)
                    (re-search-forward hif-ifx-else-endif-regexp)
                    (line-number-at-pos)))
               (beginning-of-line)
             (c-backward-conditional-with-else 1))
           (point))
         (progn (c-forward-conditional-with-else 1) (1- (point)))))
    ;; original implementation in hideif.el
    (let (max-bottom)
      (cons (save-excursion
              (beginning-of-line)
              (unless (or (hif-looking-at-else) (hif-looking-at-ifX))
                (up-ifdef))
              (prog1 (point)
                (hif-ifdef-to-endif)
                (setq max-bottom (1- (point)))))
            (save-excursion
              (beginning-of-line)
              (unless (hif-looking-at-endif)
                (hif-find-next-relevant))
              (while (hif-looking-at-ifX)
                (hif-ifdef-to-endif)
                (hif-find-next-relevant))
              (min max-bottom (1- (point))))))))

(defun hide-ifdef-block (&optional start end)
  "Hide the ifdef block (true or false part) enclosing or before the cursor.
If prefixed, it will also hide #ifdefs themselves."
  (interactive "r")
  (let ((hide-ifdef-lines current-prefix-arg))
    (if mark-active
        (progn
          (hif-recurse-on start end t)
          (setq mark-active nil))
      (unless hide-ifdef-mode (hide-ifdef-mode 1))
      (let ((top-bottom (hif-find-ifdef-block)))
        (hide-ifdef-region (car top-bottom) (cdr top-bottom))
        (when hide-ifdef-lines
          (hif-hide-line (car top-bottom))
          (hif-hide-line (1+ (cdr top-bottom))))
        (setq hide-ifdef-hiding t))
      (setq buffer-read-only (or hide-ifdef-read-only hif-outside-read-only)))))

(defun show-ifdef-block (&optional start end)
  "Show the ifdef block (true or false part) enclosing or before the cursor."
  (interactive "r")
  (if mark-active
      (progn
        (dolist (o (overlays-in start end))
          (if (overlay-get o 'hide-ifdef)
              (delete-overlay o)))
        (setq mark-active nil))
    (let ((top-bottom (condition-case nil
                          (hif-find-ifdef-block)
                        (error
                         nil)))
          (ovrs (overlays-in (max (point-min) (1- (point)))
                             (min (point-max) (1+ (point)))))
          (del nil))
      (if top-bottom
          (if hide-ifdef-lines
              (hif-show-ifdef-region
               (save-excursion
                 (goto-char (car top-bottom)) (line-beginning-position))
               (save-excursion
                 (goto-char (1+ (cdr top-bottom)))
                 (hif-end-of-line) (point)))
            (setf del (hif-show-ifdef-region (1- (car top-bottom)) (cdr top-bottom)))))
      (if (not (and top-bottom
                    del))
          (dolist (o ovrs)
            ;;(dolist (o (overlays-in (1- (point)) (1+ (point))))
            ;;   (if (overlay-get o 'hide-ifdef) (message "%S" o)))
            (if (overlay-get o 'hide-ifdef)
                (delete-overlay o)))))))

;;;  definition alist support

(defvar hide-ifdef-define-alist nil
  "A global assoc list of pre-defined symbol lists.")

(defun hif-compress-define-list (env)
  "Compress the define list ENV into a list of defined symbols only."
  (let ((new-defs nil))
    (dolist (def env new-defs)
      (if (hif-lookup (car def)) (push (car env) new-defs)))))

(defun hide-ifdef-set-define-alist (name)
  "Set the association for NAME to `hide-ifdef-env'."
  (interactive "SSet define list: ")
  (push (cons name (hif-compress-define-list hide-ifdef-env))
        hide-ifdef-define-alist))

(defun hide-ifdef-use-define-alist (name)
  "Set `hide-ifdef-env' to the define list specified by NAME."
  (interactive
   (list (completing-read "Use define list: "
                          (mapcar (lambda (x) (symbol-name (car x)))
                                  hide-ifdef-define-alist)
                          nil t)))
  (if (stringp name) (setq name (intern name)))
  (let ((define-list (assoc name hide-ifdef-define-alist)))
    (if define-list
        (setq hide-ifdef-env
              (mapcar (lambda (arg) (cons arg t))
                      (cdr define-list)))
      (error "No define list for %s" name))
    (if hide-ifdef-hiding (hide-ifdefs))))

(provide 'hideif)

;;; hideif.el ends here 
