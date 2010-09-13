;;; ntcmd.el --- major mode for editing cmd scripts
;; Copyright (C) 2010 Daniel Colascione
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation version 3 or later.
;;
;; Author:   Daniel Colascione <dan.colascione@gmail.com>
;; Version:  1.0
;; Keywords: languages
;;
;;; Commentary:
;;
;; This is a mode for highlighting and indenting Windows cmd scripts.
;; It uses a parser in several passes to ensure the buffer is
;; interpreted the way that cmd would see it. It also features
;; electric indentation.
;; 
;; The default fontification of variable references is basic, but
;; there are several faces that can be adjusted to suit your fancy.
;;

(defconst ntcmd-mode-version "1.0" "ntcmd version number.")

(eval-when-compile
  (require 'cl)
  (require 'font-lock))

(defgroup ntcmd
  nil "Major mode for editing cmd.exe scripts scripts."
  :tag "ntcmd" :group 'languages)

(defcustom ntcmd-indent-level 4
  "Amount by which batch subexpressions are indented."
  :type 'integer
  :group 'ntcmd)

(defcustom ntcmd-auto-indent-flag t
  "Whether to automatically indent when typing punctuation characters.
If non-nil, the characters `(' and `)' also indent the current
line in dos mode."
  :type 'boolean
  :group 'ntcmd)

(defface ntcmd-backquote-face
  '((t (:weight bold :inherit default)))
  "Face used to highlight backquoted commands in a DOS `for'."
  :group 'ntcmd)

(defface ntcmd-immediate-var-ref-face
  '((t (:weight bold :inherit default)))
  "Face used to highlight immediately-expanded variable references."
  :group 'ntcmd)

(defface ntcmd-loop-var-ref-face
  '((t (:weight bold :inherit default)))
  "Face used to highlight loop variable references."
  :group 'ntcmd)

(defface ntcmd-delayed-var-ref-face
  '((t (:weight bold :inherit default)))
  "Face used to highlight delay-expanded variable references.'."
  :group 'ntcmd)

;; Variable references come in two styles, with two different grammers
;; (surprise, surprise). %-variables are expanded at _READ TIME_ and
;; can contain literally _any_ character except \n and %. !-variables
;; are expanded just before a command is run, and are read to some
;; extent as barewords, though without ^-escaping.

;; Incorporating these expansion styles into our parser below would be
;; painful. Instead, we cheat: before parsing, we replace all variable
;; references with an equivalent number of 'X' characters, and after,
;; we restore all the characters to their original values.
(eval-and-compile
  (defconst ntcmd-var-ref-%-rx
    `(: (group ?%)                      ; opening [1]
        (group (+ (not (in "%\n"))))    ; primary variable name [2]
        (? ":"
           (| (: "~" (group (+ (regexp "[0-9,-]")))) ; substring [3]
              (: (group (+ (not (in "%\n")))) ; substitution src=[4]
                 "="
                 (group (* (not (in "%\n"))))))) ; replacement=[5]
        (group ?%))                              ; closing [6]
    "Reference to an immediately-expanded variable."))

(eval-and-compile
  (defconst ntcmd-var-ref-!-rx-varname
    `(| (not (in "!&|<>^\n"))
        (: "^" (not (in "!\n"))))))

(eval-and-compile
  (defconst ntcmd-var-ref-!-rx
    `(: (group ?!)                               ; opening [1]
        (? (group (+ ,ntcmd-var-ref-!-rx-varname)) ; primary name [2]
           (? ":"
              (| (: "~" (group (+ (regexp "[0-9,-]")))) ; substring [3]
                 (: (group (+ ,ntcmd-var-ref-!-rx-varname)) ; subst src=[4]
                    "="
                    (group (* ,ntcmd-var-ref-!-rx-varname)))))) ; repl [5]
        (group ?!))
    "Reference to a delayed-expansion variable."))

(eval-and-compile
  (defconst ntcmd-positional-var-ref-rx
    `(: (group "%" )                    ; opening [1]
        (? (group "~")                  ; separator [2]
           (group (* (in "fdpnxsatz"))) ; flags [3]
           (? "$"
              (group (+ ,ntcmd-var-ref-!-rx-varname)) ; magical ref [4]
              ":"))
        (group (in "0-9"))              ; var name [5]
        )
    "Matches a reference to a numeric positional variable."))

;; God, I'd rather be using COBOL. Why do the parsing rules for this
;; have to be subtly different from those for
;; ntcmd-positional-var-ref?
(eval-and-compile
  (defconst ntcmd-loop-var-ref-rx
    `(: (group "%" (? "%"))             ; opening [1]
        (? (group "~")                  ; separator [2]
           (group (* (in "fdpnxsatz"))) ; flags [3]
           (? "$"
              (group (+ ,ntcmd-var-ref-!-rx-varname)) ; magical ref [4]
              ":"))
        (group (in "a-z0-9"))           ; var name [5]
        )
    "Matches a reference to a for-loop variable."))

(defconst ntcmd-font-lock-syntactic-keywords
  `(
    ;; Basic syntax
    (,(lambda (lim)
        (let ((*ntcmd-apply-syntactic-highlights* t))
          (ntcmd-font-lock-matcher lim))))))

(defconst ntcmd-font-lock-keywords
  `(
    ;; Basic syntax
    (,(lambda (lim)
        (let ((*ntcmd-apply-highlights* t))
          (ntcmd-font-lock-matcher lim))))

    ;; Switches --- from dos-mode.
    ("[ =][-/]+\\(\\w+\\)" (1 font-lock-type-face append))))

(eval-and-compile
  (defconst ntcmd-quoted-string-rx
    `(: (group "\"")
        (group (* (not (in "\n\""))))
        (group (| "\""
                  "\n"
                  buffer-end)))
    "Matches a quoted string in most contexts; group 1 is the
string beginning; group 2 is the string interior; group 3 is the
string end."))

(defconst ntcmd-separator-characters
  '(?\, ?\; ?\=)
  "Characters that separate DOS command arguments. cmd treats
these somewhat like whitespace." )

(defconst ntcmd-punctuation-characters
  `(,@ntcmd-separator-characters
    ?\< ?\> ?\& ?\| ?\\))

;; `:' isn't a legal filename constituent, but that doesn't affect how
;; the command interpreter parses it. `*' and `?' are wildcard
;; characters, but that doesn't affect how the command interpreter
;; parses them either, since wildcard expansion isn't handed by the
;; shell.
(defconst ntcmd-legal-command-characters
  '(?\! ?\# ?\$ ?\% ?\' ?\+ ?\- ?\. ?\[ ?\]
        ?\_ ?\` ?\{ ?\} ?\~ ?\* ?\? ?\: ?\@)
  "Characters commonly considered by parts of cmd.exe to be parts
of commands.")

(eval-and-compile
  (defconst ntcmd-builtin-separators
    '(?\[ ?\] ?: ?= ?, ?\; ?\. ?< ?> ?| ?& ?\ ?\t)
    "Characters that may appear in `ntcmd-legal-command-characters', but
that are nevertheless command separators when following shell
builtins." ))

(defconst ntcmd-mode-syntax-table
  (let ((table (make-syntax-table)))

    (modify-syntax-entry ?^  "/"   table)
    (modify-syntax-entry ?\" "."   table)

    (dolist (c ntcmd-punctuation-characters)
      (modify-syntax-entry c "." table))

    (dolist (c ntcmd-legal-command-characters)
      (modify-syntax-entry c "_" table))
    table)
  "Syntax table used for ntcmd-mode.")

(defconst ntcmd-string-syntax-table
  (let ((table (make-syntax-table ntcmd-mode-syntax-table)))
    (modify-syntax-entry ?^ "." table)
    table)
  "Syntax table used inside a string.")

(defvar *ntcmd-bareword-face* nil
  "Mark barewords in this face when we move over them. If nil, do nothing.")

(defvar *ntcmd-bareword-allow-space* t
  "When non-nil, barewords can include command separators if
they're escaped; when nil, even escaped command separator characters
end the bareword.")

(defvar *ntcmd-apply-highlights* nil
  "When non-nil, parsing commands apply font-lock highlights.")

(defvar *ntcmd-apply-syntactic-highlights* nil
  "When non-nil, parsing commands apply syntactic highlights.")

(defvar ntcmd-mode-abbrev-table nil)
(define-abbrev-table 'ntcmd-mode-abbrev-table ())

(defvar ntcmd-mode-map
  (let ((map (make-sparse-keymap)))
    (mapc (lambda (key)
	    (define-key map key #'ntcmd-insert-and-indent))
	  '("(" ")"))

    (define-key map [?\M-a]       'ntcmd-beginning-of-cmdline)
    (define-key map [?\M-e]       'ntcmd-end-of-cmdline)
    map))

(put 'ntcmd-syntax-error
     'error-conditions
     '(ntcmd-syntax-error error))

(put 'ntcmd-syntax-error
     'error-message "DOS syntax error")

(defun ntcmd-help-mode ()
  "Show help page for `ntcmd-mode'."
  (interactive)
  (describe-function 'ntcmd-mode)
  (switch-to-buffer "*Help*")
  (delete-other-windows)
  (message nil))

(defun ntcmd-re-search-forward? (regexp)
  "Like re-search-forward, but never fail --- return nil
  instead."
  (re-search-forward regexp nil t))

(defun ntcmd-re-search-backward? (regexp)
  "Like re-search-backward, but never fail --- return nil
  instead."
  (re-search-backward regexp nil t))

(defun ntcmd-forward-syntactic-ws (&optional horizontal-only)
  "Move forward over syntactic whitespace, allowing one line
continuation at the end."
  (skip-chars-forward "=;, \t")
  (unless horizontal-only
    (ntcmd-re-search-forward? (rx point "^\n"))))

(defun ntcmd-forward-mandatory-syntactic-ws (&optional horizontal-only)
  (unless (memq (char-after) '(?, ?= ?\; ?\ ?\t))
    (signal 'ntcmd-syntax-error '("expected whitespace")))
  (ntcmd-forward-syntactic-ws horizontal-only))

(defun ntcmd-apply-highlight (highlight)
  (when *ntcmd-apply-highlights*
    (font-lock-apply-highlight highlight)))

(defun ntcmd-apply-syntactic-highlight (highlight)
  (when *ntcmd-apply-syntactic-highlights*
    (font-lock-apply-syntactic-highlight highlight)))

(defun ntcmd-handle-parse-escape ()
  (forward-char)

  (unless (eobp)
    (forward-char))

  (when (eq (char-after) ?\n)
    (forward-char)))

(defun ntcmd-handle-parse-open-parenthesis ()
  ;; *sigh* It's impossible to get this fully correct
  ;; with only local knowledge.
  (set-match-data (list (point) (1+ (point))))
  (ntcmd-apply-syntactic-highlight
   '(0 "."))

  ;; Yes, this is annoying. You shouldn't be using
  ;; unquoted parenthesis except as cmd.exe parenthesis.
  (ntcmd-apply-highlight
   '(0 font-lock-warning-face))
  (forward-char))

(defun ntcmd-forward-normal-cmd-line ()
  ;; Now that we've read a command name, parse the command line
  ;; proper.
  (while
      (progn (skip-chars-forward "^()^\"<>&|\n")
             (ecase (char-after)
               (?^
                (ntcmd-handle-parse-escape)
                t)

               (?\(
                (ntcmd-handle-parse-open-parenthesis)
                t)

               (?\)
                ;; We can't tell whether this paren might be closing a
                ;; compound command --- if it is, we should close the
                ;; command. If it is not, the parenthesis is just
                ;; another character.

                ;; It seems least bad to always assume the former
                ;; case.
                nil)

               ((?< ?>)
                (when (ntcmd-re-search-backward?
                       (rx (in " \t,;=")
                           (? (in "0-9"))
                           point))
                  (forward-char))
                (ntcmd-forward-fdmanip)
                t)

               (?\"
                (ntcmd-forward-quoted-string)
                t)

               ((nil ?& ?| ?\n)
                nil))))
  (ntcmd-continue-forward-cmd))

(defun ntcmd-indent-line ()
  "Indent current line as batch script"
  (let ((continuation-fixup 0)
        (offset (- (current-column) (current-indentation))))
    (save-excursion
      (back-to-indentation)
      (when (looking-at "\\^+$")
        (setf continuation-fixup (- (match-end 0) (match-beginning 0)))
        (with-silent-modifications
          (replace-match ""))))
    (indent-line-to (ntcmd-calculate-indent))
    (save-excursion
      (end-of-line 1)
      (with-silent-modifications
        (insert-char ?^ continuation-fixup)))
    (decf offset continuation-fixup)
    (when (> offset 0) (forward-char offset))))

(defun ntcmd-current-line-continues-p ()
  (and (eq (char-before (point-at-eol)) ?^)
       (not (eq (char-before (1- (point-at-eol))) ?^))))

(defun ntcmd-comment-or-string-start ()
  (nth 8 (syntax-ppss)))

(defun ntcmd-continued-line-p ()
  "Is the current line continued from the previous?"
  (save-excursion
    (end-of-line 0)
    (and (not (bobp))
         (eq (char-before) ?^)

         ;; Make sure the previous line isn't a newline-
         ;; terminated string that has a closing -
         (not (eq (car (syntax-after (1- (point))))
                  15))

         (not (eq (char-before (1- (point)))
                  ?^)))))

(defun ntcmd-beginning-of-cmdline ()
  "Go to beginning of the current command, taking into account
line continuations.

Return t if current line was continued from previous."
  (interactive)
  (let ((continued-p (ntcmd-continued-line-p)))
    (skip-syntax-backward "->")
    (while (ntcmd-continued-line-p)
      (end-of-line 0))
    (back-to-indentation)
    continued-p))

(defun ntcmd-end-of-cmdline ()
  "Go to end of current command, taking into
account line continuations. Return t if we followed
a continuation"
  (interactive)
  (let (continued-p)
    (skip-syntax-forward "->")
    (while (and (not (eobp))
                (ntcmd-current-line-continues-p))
      (setf continued-p t)
      (forward-line 1))

    (end-of-line 1)
    (skip-syntax-backward "-" (point-at-bol))
    continued-p))

(defun ntcmd-calculate-indent ()
  "Return appropriate indentation for the current line as batch code."
  (save-excursion
    (let (inhibit-point-motion-hooks
          inhibit-field-text-motion
          reference-point (indent 0))

      ;; Deindent lines beginning with closing parenthesis
      (back-to-indentation)
      (skip-chars-forward ")")
      (setf reference-point (point))
      (back-to-indentation)

      (when (ntcmd-beginning-of-cmdline)
        (incf indent ntcmd-indent-level))

      (skip-chars-forward ")")
      (incf indent (current-indentation))
      (incf indent (* ntcmd-indent-level
                      (nth 0 (parse-partial-sexp
                              (point) reference-point))))
      (max 0 indent))))

(defun ntcmd-insert-and-indent (key)
  "Run the command bound to KEY, and indent if necessary.
Indentation does not take place if point is in a string or
comment."
  (interactive (list (this-command-keys)))
  (call-interactively (lookup-key (current-global-map) key))
  (let ((syntax (syntax-ppss)))
    (when (or (and (not (nth 8 syntax))
                   ntcmd-auto-indent-flag)
              (and (nth 4 syntax)
                   (eq (current-column)
                       (1+ (current-indentation)))))
      (indent-according-to-mode))))

(defun ntcmd-inplace-replace (replacement)
  "Replace the characters at point with REPLACEMENT without disturbing markers.

Leave point after replacement. The number of characters replaced
is the length of REPLACEMENT. Text properties from REPLACEMENT
are used."

  (loop for i below (length replacement)
        ;; Replace the character after point with the next character
        ;; from replacement. We must worry about two kinds
        ;; of marker: those pointing at point (including (point)), and
        ;; those pointing at (1+ (point)).
        ;;
        ;; Mentally run through the code below, and you'll see that
        ;; both kinds of marker are preserved.
        ;;
        do (progn
             (forward-char 1)
             (insert-before-markers (aref replacement i))
             (set-text-properties (1- (point)) (point)
                                  (text-properties-at i replacement))
             (backward-char 1)
             (delete-char -1)
             (forward-char 1))))

(defun ntcmd-apply-placeholder (begin end &rest highlights)
  "Replace text between BEGIN and END with a placeholder, saving
restoration information in the text property ntcmd-restore-info.
Leave point unchanged."
  (let ((placeholder-string (make-string (- end begin) ?X)))
    (put-text-property 0 1 'ntcmd-restore-info
                       (list* (buffer-substring begin end)
                              (match-data t)
                              highlights)
                       placeholder-string)
    (save-excursion
      (goto-char begin)
      (ntcmd-inplace-replace placeholder-string))))

(defun ntcmd-replace-var-refs ()
  "Replace all variable references in the current restriction
with a placeholder, storing restoration information in text
properties. "
  (interactive "r")

  ;; Pass 0: replace positional arguments
  (goto-char (point-min))
  (while (ntcmd-re-search-forward?
          (rx (eval ntcmd-positional-var-ref-rx)))
    (unless (eq (char-before (match-beginning 0)) ?%)
      (ntcmd-apply-placeholder
       (match-beginning 0)
       (match-end 0)
       '(0 'ntcmd-immediate-var-ref-face append))))

  ;; Pass 1: replace immediate expansion
  (goto-char (point-min))
  (while (ntcmd-re-search-forward?
          (rx (eval ntcmd-var-ref-%-rx)))
    (unless (eq (char-before (match-beginning 0)) ?%)
      (ntcmd-apply-placeholder
       (match-beginning 0)
       (match-end 0)
       '(0 'ntcmd-immediate-var-ref-face append))))

  ;; Pass 2: replace loop variables
  (goto-char (point-min))
  (while (ntcmd-re-search-forward?
          (rx (eval ntcmd-loop-var-ref-rx)))
    (ntcmd-apply-placeholder
     (match-beginning 0)
     (match-end 0)
     '(0 'ntcmd-loop-var-ref-face append)))

  ;; Pass 3: replace delay-expansion variables
  (goto-char (point-min))
  (while (ntcmd-re-search-forward?
          (rx (eval ntcmd-var-ref-!-rx)))
    (unless
        (save-excursion
          (goto-char (match-beginning 0))
          (looking-back "^^"))
      
      (ntcmd-apply-placeholder
       (match-beginning 0)
       (match-end 0)
       '(0 'ntcmd-delayed-var-ref-face append)))))

(defun ntcmd-unapply-placeholder-at-point (&optional skip-highlights)
  "If there is a placeholder at point, undo it. Otherwise,
do nothing. Leaves point unchanged. If SKIP-HIGHLIGHTS is
non-nil, don't apply stored highlights."
  (loop for (orig-string
             orig-match-data
             . highlights) = (get-text-property
                              (point) 'ntcmd-restore-info)
        while orig-string
        do (save-excursion
             (ntcmd-inplace-replace orig-string)
             (unless skip-highlights
               (set-match-data orig-match-data)
               (mapc #'ntcmd-apply-highlight highlights)))))

(defun ntcmd-unapply-placeholders ()
  "Undo the changes done by `ntcmd-replace-var-refs-in-region'
between point and point-max."

  (loop
   until (eobp)
   do (ntcmd-unapply-placeholder-at-point)
   and do (goto-char (next-single-property-change
                      (point)
                      'ntcmd-restore-info
                      nil
                      (point-max)))))

(defun ntcmd-font-lock-matcher (lim)
  "ntcmd font-lock matcher that actually does most of the fontification work."
  
  (save-excursion
    (save-restriction
      (narrow-to-region (point) lim)
      (with-silent-modifications
        (ntcmd-replace-var-refs))))

  (unwind-protect
      (save-excursion
        (loop with pos
              while (< (point) lim)
              do (progn
                   (setf pos (point))
                   (condition-case err
                       (ntcmd-forward-cmd)
                     ((end-of-buffer ntcmd-syntax-error)
                      (end-of-line 1))))))

    (save-restriction
      (narrow-to-region (point) lim)
      (with-silent-modifications
        (ntcmd-unapply-placeholders))))

  ;; Always tell font-lock we've failed in order to avoid confusion.
  nil)

(eval-when-compile
  (defvar font-lock-beg)
  (defvar font-lock-end))

(defun ntcmd-extend-font-lock-region ()
  (let ((old-beg font-lock-beg)
        (old-end font-lock-end))

    ;; Extend fontification region to cover all the lines the current
    ;; command space. Commands can span more than one line when
    ;; they're continued; we don't look at compound commands here and
    ;; don't need to.
    ;;
    ;; Make sure we always snap beginning and end to the beginning of
    ;; lines to avoid fighting with other font-lock region-extending
    ;; functions.
    ;;
    ;; Note: we're conservative here. Some of these ^ characters might
    ;; not represent real continuations (depending on unclosed string
    ;; state and such) but let's not rely on syntactic fontification
    ;; having been done here.

    (save-excursion
      (goto-char font-lock-beg)
      (while (and (not (bobp))
                  (save-excursion
                    (end-of-line 0)
                    (eq (char-before) ?^)))
        (forward-line -1))
      (setf font-lock-beg (point))

      (goto-char font-lock-end)
      (while (and (not (eobp))
                  (eq (char-before (point-at-eol))
                      ?^))
        (forward-line 1))
      (setf font-lock-end (point)))

    (or (/= old-beg font-lock-beg)
        (/= old-end font-lock-end))))

(defconst ntcmd-command-parsers nil)

(defun ntcmd-register-cmd-parser (regex function)
  (add-to-list 'ntcmd-command-parsers
               (cons (concat (rx point) regex) function)
               t))

(defconst ntcmd-bareword-rx
  (rx (+ (| (syntax word)
            (syntax symbol)
            (: "^" nonl)
            (: "^" "\n" anything)))))

(eval-and-compile
  (defconst ntcmd-bareword-part-rx
    `(+ (| (syntax word)
           (syntax symbol)
           (: "^" nonl)
           (: "^" "\n" anything)))
    "Matches part of a bareword."))

(eval-and-compile
  (defconst ntcmd-bareword-nospace-part-rx
    `(: (+ (| (syntax word)
              (syntax symbol)
              (: "^" (not (in ",;= \t\n")))
              (: "^" "\n" (not (in ",;= \t\n")))))
        (? "^\n"))
    "Matches part of a bareword in the spaceless variation."))

(defun ntcmd-forward-quoted-string ()
  (unless (ntcmd-re-search-forward?
           (rx point (eval ntcmd-quoted-string-rx)))
    (signal 'ntcmd-syntax-error '("expected quoted string")))
  (ntcmd-apply-syntactic-highlight
   `(1 "|"))
  (ntcmd-apply-syntactic-highlight
   `(2 ,ntcmd-string-syntax-table))
  (ntcmd-apply-syntactic-highlight
   `(3 "|"))
  (when (eq (char-before) ?\n)
    (backward-char)))

(defun ntcmd-forward-bareword-part ()
  "Move forward over part of a quoted string or a bareword and
return t, or return nil if there's no bareword at point."

  (cond ((eq (char-after) ?\")
         (ntcmd-forward-quoted-string)
         t)

        ((ntcmd-re-search-forward?
          (if *ntcmd-bareword-allow-space*
              (rx point (eval ntcmd-bareword-part-rx))
            (rx point (eval ntcmd-bareword-nospace-part-rx))))

         (when *ntcmd-bareword-face*
           (ntcmd-apply-highlight
            `(0 ,*ntcmd-bareword-face*)))

         t)))

(defun ntcmd-forward-bareword ()
  "Move forward over a word made up of one or more barewords and
quoted strings."
  (unless (ntcmd-forward-bareword-part)
    (signal 'ntcmd-syntax-error '("expected bareword or string")))
  (while (ntcmd-forward-bareword-part)))

(eval-and-compile
  (defconst ntcmd-fdmanip-begin-rx
    '(: (? (in "0-9"))
        (| ">>"
           (: (in "<>")
              (? "&" (in "0-9")))))))

(defun ntcmd-forward-fdmanip ()
  "Move forward over an IO manipulation. Must be at something
that matches ntcmd-fdmanip-begin-rx."

  (unless (ntcmd-re-search-forward?
           (rx point (eval ntcmd-fdmanip-begin-rx)))
    (signal 'ntcmd-syntax-error '("expected FD manipulation")))

  (ntcmd-apply-highlight
   '(0 font-lock-reference-face))

  (when (memq (char-before) '(?< ?>))
    (ntcmd-forward-syntactic-ws t)
    (ntcmd-forward-bareword)))

;; labels
(ntcmd-register-cmd-parser
 (rx (group bol ":"))
 #'ntcmd-forward-label-cmd)

(defun ntcmd-forward-label-cmd ()
  (let (comment-mode)
    (when (eq (char-after) ?:)
      (setq comment-mode t)
      (ntcmd-apply-syntactic-highlight
       '(0 "<")))
    (backward-char)
    (loop do (progn
               (set-match-data (list (point) (point-at-eol)))
               (unless comment-mode
                 (ntcmd-apply-highlight
                  '(0 font-lock-reference-face)))
               (end-of-line 1))
          while (when (eq (char-before) ?^)
                  (forward-char)
                  t))

    (when comment-mode
      (set-match-data (list (point) (1+ (point))))
      (ntcmd-apply-syntactic-highlight
       '(0 ">")))))

;; rem

(ntcmd-register-cmd-parser
 (rx (group "rem")
     symbol-end)
 #'ntcmd-forward-rem-cmd)

(defun ntcmd-forward-rem-cmd ()
  (ntcmd-apply-highlight
   '(1 font-lock-comment-face))

  (unless (memq (char-after) '(nil ?\n))
    ;; Mark the whitespace after "rem" instead of "rem" itself so
    ;; that "rem" retains word syntax and can be edited normally.
    (set-match-data (list (point) (1+ (point))))
    (ntcmd-apply-syntactic-highlight
     '(0 "<"))
    (forward-char)
    (skip-chars-forward "^\n")

    ;; bug-for-bug compatibility: we only follow one line
    ;; continuation when we're in comment-parsing mode.
    (when (eq (char-before) ?^)
      (forward-char)
      (skip-chars-forward "^\n")))

  (assert (memq (char-after) '(nil ?\n)))

  (when (char-after)
    ;; Close the comment if we're not at EOB
    (set-match-data (list (point) (1+ (point))))
    (ntcmd-apply-syntactic-highlight
     '(0 ">"))))

;; for

(ntcmd-register-cmd-parser
 (rx (group "for")
     (? "^\n")
     (in " \t,;="))
  #'ntcmd-forward-for-cmd)

(defun ntcmd-forward-for-cmd ()
  (let (backq-chars)

   (ntcmd-apply-highlight
    '(1 font-lock-keyword-face))

   (ntcmd-forward-syntactic-ws)
   
   (when (eq (char-after) ?/)
     (forward-char)
     (case (upcase (char-after))
       ((?D ?L)
        (forward-char)
        (ntcmd-forward-mandatory-syntactic-ws))

       (?F
        (forward-char)
        (ntcmd-forward-mandatory-syntactic-ws)
        (setq backq-chars '(?\'))

        (unless (or (eq (char-after) ?%)
                    (get-text-property (point) 'ntcmd-restore-info))
          (let ((orig (point)))
            (ntcmd-forward-bareword)

            ;; See whether the user specified "usebackq"
            (when (save-excursion
                    (save-restriction
                      (narrow-to-region orig (point))
                      (goto-char (point-min))
                      (ntcmd-re-search-forward?
                       (rx symbol-start "usebackq" symbol-end))))
              (setq backq-chars '(?\' ?\`))))

          (ntcmd-forward-mandatory-syntactic-ws)))

       (?R
        (forward-char)
        (ntcmd-forward-mandatory-syntactic-ws)
        (when (eq (char-after) ?%)
          (signal 'ntcmd-syntax-error '("expected /R argument")))
        (ntcmd-forward-bareword)
        (ntcmd-forward-mandatory-syntactic-ws))

       (t
        (signal 'ntcmd-syntax-error
                '("invalid for switch")))))

   ;; We might have a placeholder from an earlier fontification pass
   ;; --- undo it so we can highlight it differently here.
   (ntcmd-unapply-placeholder-at-point t)

   (unless (ntcmd-re-search-forward?
            (rx point "%" (? "%")
                (group (in "a-z"))
                symbol-end))
     (signal 'ntcmd-syntax-error
             '("expected loop variable")))
   (ntcmd-apply-highlight
    '(1 font-lock-variable-name-face))

   (ntcmd-forward-mandatory-syntactic-ws)

   (unless (ntcmd-re-search-forward?
            (rx point (group "in") symbol-end))

     (signal 'ntcmd-syntax-error
             '("expected \"in\"")))
   (ntcmd-apply-highlight
    '(1 font-lock-keyword-face))
   
   (ntcmd-forward-mandatory-syntactic-ws t)

   (unless (eq (char-after) ?\()
     (signal 'ntcmd-syntax-error '("expected (")))
   (forward-char)

   ;; Hairy --- for-loop parsing is strangely implemented. (I'd say
   ;; it's badly specified, but I don't think it's specified at all.)
   ;;
   ;; The interior of a for-loop isn't anything special from the point
   ;; of view of the command _reader_. ^ and " have their usual
   ;; function of escaping special characters.
   ;;
   ;; _After_ a command is read, however, ' and ` are interpreted by
   ;; the for-loop command specially IFF "usebackq" was given in the
   ;; options above. These characters are only special if the first
   ;; and only '`' or ''' comes after the opening '(' [optionally
   ;; preceded by syntactic whitespace] and the single (unescaped!)
   ;; closing '`' or ''' comes immediately before the closing ')'.
   ;;
   ;; These characters have no other effect on parsing and can be
   ;; escaped normally.
   ;;
   (let (backq-start backq-end backq-char)
     (ntcmd-forward-syntactic-ws)
     (when (memq (char-after) backq-chars)
       (setf backq-start (point))
       (setf backq-char (char-after))
       (forward-char))

     (while (progn
              (skip-chars-forward "^^()\"'`|&<>")
              (ecase (char-after)
                (?^
                 (ntcmd-handle-parse-escape)
                 t)

                (?\(
                (ntcmd-handle-parse-open-parenthesis)
                t)

                ((?& ?| ?< ?>)
                 (when backq-start
                   (set-match-data (list (point) (1+ (point))))
                   (ntcmd-apply-highlight
                    '(0 font-lock-warning-face)))
                 (forward-char)
                 t)

                ((nil ?\))
                 nil)

                (?\"
                 (ntcmd-forward-quoted-string)
                 t)

                ((?' ?`)
                 (forward-char)
                 (when (and (eq (char-before) backq-char)
                            (save-excursion
                              (ntcmd-forward-syntactic-ws)
                              (eq (char-after) ?\))))
                   (setf backq-end (point)))))))

     (when (and backq-start backq-end)
       (set-match-data (list backq-start backq-end))
       (ntcmd-apply-highlight
        '(0 'ntcmd-backquote-face)))

     (unless (eobp)
       (assert (eq (char-after) ?\)))
       (forward-char))

     ;; syntactic ws is _optional_ after the closing ')' in an if
     (ntcmd-forward-syntactic-ws)

     (unless (ntcmd-re-search-forward? (rx point "do" symbol-end))
       (signal 'ntcmd-syntax-error '("expected \"do\"")))
     (ntcmd-apply-highlight
      '(0 font-lock-keyword-face))
     (ntcmd-forward-mandatory-syntactic-ws)

     ;; ntcmd-continue-forward-cmd is the wrong thing here
     (ntcmd-forward-cmd))))

;; if

(ntcmd-register-cmd-parser
 (rx (group "if")
     (? "^\n")
     (in " \t,;="))
 #'ntcmd-forward-if-cmd)

(defun ntcmd-forward-if-cmd ()
  (ntcmd-apply-highlight
   '(1 font-lock-keyword-face))

  (backward-char)

  (when (ntcmd-re-search-forward?
         (rx point
             (+ (in " \t,;="))
             "not"))
    (ntcmd-apply-highlight
     '(0 font-lock-keyword-face)))

  (cond ( ;; errorlevel/exist/cmdextversion/defined
         (ntcmd-re-search-forward?
          (rx point
              (+ (in " \t,;"))
              (group (| "errorlevel"
                        "exist"
                        "cmdextversion"
                        "defined"))
              (+ (in " \t,;"))))

         (ntcmd-apply-highlight
          '(1 font-lock-keyword-face))
         (let ((*ntcmd-bareword-face*
                (if (equal (downcase (match-string 1)) "defined")
                    font-lock-variable-name-face)))
           (ntcmd-forward-bareword)))

        ;; the weird syntax
        ;; note: we don't need to do anything special
        ;; for /i
        ((let (saw-op)
           (while (progn
                    (skip-chars-forward "^^\")\n \t;,=")
                    (ecase (char-after)
                      (?^
                       (forward-char)
                       (unless (eolp)
                         (forward-char))
                       t)

                      (?\"
                       (ntcmd-forward-quoted-string)
                       t)

                      ((nil ?\n ?\))
                       (signal 'ntcmd-syntax-error
                               '("if condition ended early")))

                      ((?\ ?\t ?: ?, ?=)
                       (cond ((and (not saw-op)
                                   (ntcmd-re-search-forward?
                                    (rx point
                                        (group (| (: (* (in ";, \t"))
                                                     (| "==")
                                                     (* (in ";, \t")))
                                                  (: (+ (in ";, \t"))
                                                     (| "equ" "neq" "lss"
                                                        "leq" "gtr" "geq")
                                                     (+ (in ";, \t"))))))))

                              (ntcmd-apply-highlight
                               '(1 font-lock-keyword-face nil t))
                              (setf saw-op t)
                              t)

                             ((and saw-op
                                   (memq (char-after) '(?\( ?= ?,
                                                            ?\; ?\ ?\t)))
                              nil)

                             (:otherwise
                              (forward-char)
                              t))))))
           saw-op))

        (t
         (signal 'ntcmd-syntax-error
                 '("invalid if"))))

  (ntcmd-forward-syntactic-ws)
  (ntcmd-forward-cmd))

;; empty command
(ntcmd-register-cmd-parser
 (rx "\n")
 #'ntcmd-forward-empty-cmd)

(defun ntcmd-forward-empty-cmd ()
  nil)

;; generic command
(ntcmd-register-cmd-parser
 (rx (| word-boundary not-word-boundary)) ; always matches
 #'ntcmd-forward-generic-cmd)

(eval-and-compile
  (defconst ntcmd-speculative-command-name-rx
    `(+ (| (syntax word)
           (syntax symbol)
           (: "^" nonl)
           ,ntcmd-quoted-string-rx))
    "Matches a command name, at least up to a line continuation."))

(eval-and-compile
  (defconst ntcmd-never-commands
    '( "for" "if" "else")
    "These commands should never appear as regular commands."))

(eval-and-compile
  (defconst ntcmd-shell-builtins
    '("break"
      "cd"
      "chdir"
      "cls"
      "color"
      "copy"
      "date"
      "del"
      "dir"
      "dir"
      "echo"
      "endlocal"
      "erase"
      "exit"
                                        ; "for" ; handled specially
      "ftype"
      "goto"
                                        ; "if" handled specially
      "md"
      "mkdir"
      "move"
      "path"
      "pause"
      "popd"
      "prompt"
      "pushd"
      "rd"
      "rem"
      "rem"
      "ren"
      "rename"
      "rmdir"
      "set"
      "setlocal"
      "shift"
      "start"
      "time"
      "title"
      "type"
      "ver"
      "verify"
      "vol")
    "List of commands built into the shell. They have slightly
different parsing rules associated with them. Some have dedicated
command handlers."))

(defun ntcmd-forward-set-command-body ()
  "Parse a DOS set command."
  (skip-chars-forward " ")

  ;; Read a variable name --- but only if we have an '=' somewhere.

  (let ((start (point)) var-name-regions)

    (while (progn
             (skip-chars-forward "^()=&|<>^\"\n")
             (ecase (char-after)
               ;; The usual bizarre escape logic
               (?^
                (ntcmd-handle-parse-escape)
                t)

               (?\(
                (ntcmd-handle-parse-open-parenthesis)
                t)

               (?\)
                ;; We can't tell whether this paren might be closing a
                ;; compound command --- if it is, we should close the
                ;; command. If it is not, the parenthesis is just
                ;; another character.

                ;; It seems least bad to always assume the former
                ;; case.
                nil)


               (?\"
                (ntcmd-forward-quoted-string)
                t)

               ;; The character we're looking for
               (?=
                (if (looking-at (rx "=" (? (in "0-9")) (in "<>")))
                    ;; If this = will be part of an IO redirection,
                    ;; skip it.
                    (progn
                      (forward-char)
                      t)

                  ;; Otherwise, we're done.
                  (push (list start (point)) var-name-regions)
                  nil))

               ;; Redirections that can appear in the middle(!) of a
               ;; variable name, but that aren't part of it.
               ((?< ?>)
                (when (ntcmd-re-search-backward?
                       (rx (in " \t,;=")
                           (? (in "0-9"))
                           point))
                  (forward-char))

                (push (list start (point)) var-name-regions)
                (ntcmd-forward-fdmanip)
                (setf start (point))
                t)

               ((nil ?& ?| ?\n)
                nil))))

    (when (eq (char-after) ?=)
      ;; Highlight the regions we just read that make up the variable
      ;; name.
      (dolist (region var-name-regions)
        (set-match-data region)
        (font-lock-apply-highlight '(0 font-lock-variable-name-face)))

      ;; And move on to the value
      (while (progn
             (skip-chars-forward "^()&|<>^\"\n")
             (ecase (char-after)

               (?\(
                (ntcmd-handle-parse-open-parenthesis)
                t)

               (?\)
                ;; We can't tell whether this paren might be closing a
                ;; compound command --- if it is, we should close the
                ;; command. If it is not, the parenthesis is just
                ;; another character.

                ;; It seems least bad to always assume the former
                ;; case.
                nil)

               ;; Quoted string
               (?\"
                (ntcmd-forward-quoted-string)
                t)

               ;; The usual bizarre escape logic
               (?^
                (ntcmd-handle-parse-escape)
                t)

               ((?< ?>)
                (when (ntcmd-re-search-backward?
                       (rx (in " \t,;=")
                           (? (in "0-9"))
                           point))
                  (forward-char))
                (ntcmd-forward-fdmanip)
                t)

               ((nil ?& ?| ?\n)
                ;; What on earth...? Yes, set actually skips "||" for
                ;; some reason, but only in the value part. Actually,
                ;; it omits everything after the first "||" from the
                ;; value assigned to the environment variable, but we
                ;; don't care about that here.
                (if (looking-at (rx "||"))
                    (progn
                      (forward-char 2)
                      t)
                  nil))))))

    (ntcmd-continue-forward-cmd)))

(defun ntcmd-forward-generic-cmd ()
  "Parse a generic command line as for an external command."

  (when (looking-at (rx (eval ntcmd-fdmanip-begin-rx)))
    (ntcmd-forward-fdmanip)

    ;; We actually use a completely different algorithm for reading
    ;; the command name depending on whether we've seen an IO
    ;; redirection. I've determined these rules emperically.

    ;; Read all IO manipulations (which can only occur on the first
    ;; line).
    (while (progn (ntcmd-forward-syntactic-ws t)
                  (and (looking-at (rx (eval ntcmd-fdmanip-begin-rx)))
                       (ntcmd-forward-fdmanip)
                       t)))

    ;; Here's the tricky bit: see whether we need to discard the rest
    ;; of this line. We do that if we see a line continuation and
    ;; haven't yet finished reading a command name.
    (ntcmd-forward-syntactic-ws t)

    (when (save-excursion
            (ntcmd-re-search-forward?
             (rx
              point
              (| "^"
                 (: (group (eval ntcmd-speculative-command-name-rx))
                    "^")))))
      (ntcmd-apply-highlight
       '(1 font-lock-warning-face nil t))
      (forward-line 1)
      (ntcmd-forward-syntactic-ws t)))

  ;; We're allowed one continuation before the command name proper
  ;; begins. Any more than that is an error.
  (ntcmd-re-search-forward? (rx point "^\n"))

  (when (looking-at (rx (in "^" "\n" "<" ">" "|" "&")))
    (signal 'ntcmd-syntax-error '("cannot begin command here")))

  ;; Now we can read a command name normally. This is just like
  ;; reading any other bareword-or-string, except that *any*
  ;; command-terminator outside a string terminates the command, even
  ;; if it's escaped with ^.
  (cond
   ;; 1. Words that should never appear in this context because we
   ;; have separate parsers for them.
   ((looking-at (rx
                 (group (eval `(| ,@ntcmd-never-commands))) symbol-end))
    (ntcmd-apply-highlight '(0 font-lock-warning-face))
    (signal 'ntcmd-syntax-error `("not expected here"
                                ,(match-string-no-properties 1))))

   ;; 2. Shell builtins have slightly different parsing rules
   ((looking-at (rx (group (eval `(| ,@ntcmd-shell-builtins)))
                    (| (eval `(in ,@ntcmd-builtin-separators "\n"))
                       buffer-end)))

    (cond (;; 2a. Special case for echo --- bare echo is almost never
           ;; what you want
           (ntcmd-re-search-forward? (rx point (group "echo")
                                       (* (in " \t"))
                                       (| (in ?\n ?& ?|)
                                          buffer-end)))
           (ntcmd-apply-highlight '(1 font-lock-warning-face))
           (unless (eobp)
             (backward-char)
             (ntcmd-continue-forward-cmd)))

          ;; 2b. Special case for set
          ((ntcmd-re-search-forward?
            (rx point (group "set")
                (not (in "l"))))
           (ntcmd-apply-highlight '(1 font-lock-keyword-face))
           (ntcmd-forward-set-command-body))

          ;; 2e. Normal builtin command
          (t
           (goto-char (match-end 1))
           (ntcmd-apply-highlight '(1 font-lock-keyword-face))
           (ntcmd-forward-normal-cmd-line))))

   ;; 3. Special case for '('
   ((eq (char-after) ?\()
    (forward-char)
    (ntcmd-forward-syntactic-ws)
    (ntcmd-forward-cmd))

   ;; 4. Special case for ')'
   ((eq (char-after) ?\))
    (ntcmd-continue-forward-cmd))

   ;; 5. Non-built-in case
   (t
    (let ((*ntcmd-bareword-face* 'font-lock-builtin-face)
          *ntcmd-bareword-allow-space*)
      (ntcmd-forward-bareword))
    (ntcmd-forward-normal-cmd-line))))

;; end of buffer
(ntcmd-register-cmd-parser
 (rx buffer-end)
 (lambda ()))

;; command parsing machinery

(defun ntcmd-continue-forward-cmd ()
  (ntcmd-forward-syntactic-ws t)

  (cond ((eq (char-after) ?\))
         (forward-char)
         (if (save-excursion
               (ntcmd-forward-syntactic-ws)
               (looking-at (rx (group  "else")
                               (? "^\n")
                               (in " \t=,;"))))
             (progn
               (goto-char (match-end 1))
               (ntcmd-apply-highlight
                '(1 font-lock-keyword-face))
               (ntcmd-forward-cmd))
           (ntcmd-continue-forward-cmd)))

        ((ntcmd-re-search-forward?
          (rx point (| (: "&" (? "&"))
                       (: "|" (? "|")))))
         (ntcmd-forward-cmd)))

  (unless (or (bolp) (eolp))
    (signal 'ntcmd-syntax-error "command terminated early")))

(defun ntcmd-forward-cmd ()
  "Parse a single command."

  (interactive)

  ;; Skip any syntactically irrelevant command prefix
  (skip-chars-forward " \t,;=@")

  (funcall
   (loop for (re . func) in ntcmd-command-parsers
         when (ntcmd-re-search-forward? re)
         return func)))

;;;###autoload
(define-derived-mode ntcmd-mode nil "NT/CMD"
  "Major mode for editing CMD scripts.

\\{ntcmd-mode-map}"

  :group 'ntcmd
  :syntax-table ntcmd-mode-syntax-table
  :abbrev-table ntcmd-mode-abbrev-table

  (set (make-local-variable 'comment-start) "@rem")
  (set (make-local-variable 'imenu-generic-expression)
       '((nil "^:[^:].*" 0)))

  (set (make-local-variable 'font-lock-defaults)
       `(ntcmd-font-lock-keywords ; keywords
         nil                    ; keywords-only
         t                      ; case-fold
         nil                    ; syntax modifications
         nil                    ; syntax-begin
         (font-lock-syntactic-keywords
          . ntcmd-font-lock-syntactic-keywords)
         (jit-lock-contextually . nil)))

  (add-hook 'font-lock-extend-region-functions
    #'ntcmd-extend-font-lock-region
    nil t)

  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  (set (make-local-variable 'parse-sexp-lookup-properties) t)
  (set (make-local-variable 'outline-regexp) ":[^:]")
  (set (make-local-variable 'indent-line-function) #'ntcmd-indent-line)

  ;; Important to fontify the whole buffer syntactically! If we don't,
  ;; then we might have regular expression literals that aren't marked
  ;; as strings, which will screw up parse-partial-sexp, scan-lists,
  ;; etc. and and produce maddening "unbalanced parenthesis" errors.
  ;; When we attempt to find the error and scroll to the portion of
  ;; the buffer containing the problem, JIT-lock will apply the
  ;; correct syntax to the regular expresion literal and the problem
  ;; will mysteriously disappear.
  (font-lock-set-defaults)

  (let (font-lock-keywords) ; leaves syntactic keywords intact
    (font-lock-fontify-buffer)))

(provide 'ntcmd)

