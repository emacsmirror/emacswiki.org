;;; wcheck-mode.el --- General interface for text checkers

;; Copyright (C) 2009-2011 Teemu Likonen <tlikonen@iki.fi>

;; Author: Teemu Likonen <tlikonen@iki.fi>
;; Maintainer: Teemu Likonen <tlikonen@iki.fi>
;; Created: 2009-07-04
;; Version: 2011.02.20
;; Keywords: text spell check languages ispell


;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs. If not, see <http://www.gnu.org/licenses/>.


;; INSTALLATION
;;
;; Put this file to some directory in your Emacs's "load-path" and add
;; the following lines to Emacs's initialization file (~/.emacs):
;;
;;     (autoload 'wcheck-mode "wcheck-mode"
;;       "Toggle wcheck-mode." t)
;;     (autoload 'wcheck-change-language "wcheck-mode"
;;       "Switch wcheck-mode languages." t)
;;     (autoload 'wcheck-spelling-suggestions "wcheck-mode"
;;       "Spelling suggestions." t)
;;     (autoload 'wcheck-jump-forward "wcheck-mode"
;;       "Move point forward to next marked text area." t)
;;     (autoload 'wcheck-jump-backward "wcheck-mode"
;;       "Move point backward to previous marked text area." t)
;;
;; See customize group "wcheck" for information on how to configure
;; Wcheck mode. (M-x customize-group RET wcheck RET)


;;; Commentary:
;;
;; General interface for text checkers
;;
;; Wcheck is a minor mode for automatically checking and marking strings
;; in Emacs buffer. Wcheck sends (parts of) buffer's content to a
;; text-checker back-end and, based on its output, decides if some parts
;; of text should be marked.
;;
;; Wcheck can be used with external spell-checker programs such as
;; Ispell, Aspell and Enchant, but actually any tool that can receive
;; text stream from standard input and send text to standard output can
;; be used. The checker back-end can also be an Emacs Lisp function.


;;; Code:


;;; Settings


;;;###autoload
(defgroup wcheck nil
  "General interface for text checkers."
  :group 'applications)


(defconst wcheck-language-data-customize-interface
  '(choice
    :format "%[Option%] %v"

    (cons :tag "Program" :format "%v"
          (const :tag "Program" :format "%t: " program)
          (choice :format "%[Type%] %v"
                  (file :tag "Filename" :format "\n\t\t%t: %v")
                  (function :tag "Function" :format "\n\t\t%t: %v")))

    (cons :tag "Arguments" :format "%v"
          (const :format "" args)
          (repeat :tag "Arguments"
                  :value-to-internal
                  (lambda (widget value)
                    (cond ((stringp value)
                           (split-string-and-unquote value "[ \t\n]+"))
                          ((listp value)
                           value)))
                  :match (lambda (widget value)
                           (or (listp value)
                               (stringp value)))
                  (string :format "%v")))

    (cons :tag "Output parser function" :format "%v"
          (const :tag "Output parser" :format "%t: " parser)
          (choice :format "%[Parser%] %v" :value nil
                  (const :tag "Lines" wcheck-parser-lines)
                  (const :tag "Whitespace" wcheck-parser-whitespace)
                  (function :tag "Custom function"
                            :format "%t:\n\t\t%v")))

    (cons :tag "Connection type" :format "%v"
          (const :tag "Connection: " :format "%t" connection)
          (choice :format "%[Type%] %v" :value nil
                  (const :tag "pipe (nil)" nil)
                  (const :tag "pty" :match (lambda (widget value)
                                             (or (eq value t)
                                                 (eq value 'pty)))
                         pty)))

    (cons :tag "Face" :format "%v"
          (const :tag "Face" :format "%t: " face)
          (symbol :format "%v"))

    (cons :tag "Syntax table" :format "%v"
          (const :tag "Syntax table" :format "%t: " syntax)
          (variable :format "%v"))

    (cons :tag "Regexp start" :format "%v"
          (const :tag "Regexp start" :format "%t: " regexp-start)
          (regexp :format "%v"))

    (cons :tag "Regexp body" :format "%v"
          (const :tag "Regexp body" :format "%t: " regexp-body)
          (regexp :format "%v"))

    (cons :tag "Regexp end" :format "%v"
          (const :tag "Regexp end" :format "%t: " regexp-end)
          (regexp :format "%v"))

    (cons :tag "Regexp discard" :format "%v"
          (const :tag "Regexp discard" :format "%t: " regexp-discard)
          (regexp :format "%v"))

    (cons :tag "Regexp case" :format "%v"
          (const :tag "Regexp" :format "%t: " case-fold)
          (choice :format "%[Case%] %v" :value nil
                  (const :tag "sensitive" nil)
                  (const :tag "insensitive" t)))

    (cons :tag "Suggestion program" :format "%v"
          (const :tag "Suggestion program" :format "%t: " suggestion-program)
          (choice :format "%[Type%] %v"
                  (file :tag "Filename" :format "\n\t\t%t: %v")
                  (function :tag "Function" :format "\n\t\t%t: %v")))

    (cons :tag "Suggestion program's arguments" :format "%v"
          (const :format "" suggestion-args)
          (repeat :tag "Suggestion program's arguments"
                  (string :format "%v")))

    (cons :tag "Suggestion parser function" :format "%v"
          (const :tag "Suggestion parser" :format "%t: "
                 suggestion-parser)
          (choice :format "%[Parser%] %v" :value nil
                  (const :tag "Ispell" wcheck-parser-ispell-suggestions)
                  (const :tag "Lines" wcheck-parser-lines)
                  (const :tag "Whitespace" wcheck-parser-whitespace)
                  (function :tag "Custom function"
                            :format "%t:\n\t\t%v")))

    (cons
     :tag "Read or skip faces" :format "%v"
     (const :tag "Read or skip faces" :format "%t" read-or-skip-faces)
     (repeat
      :tag ""
      (cons :format "%v"
            (choice :format "%[Major mode%] %v"
                    (const :tag "All major modes"
                           :match (lambda (widget value) (not value))
                           nil)
                    (repeat
                     :tag "Select major modes"
                     :match (lambda (widget value)
                              (and value (or (symbolp value) (listp value))))
                     :value-to-internal (lambda (widget value)
                                          (if (symbolp value)
                                              (list value)
                                            value))
                     :value-to-external (lambda (widget value)
                                          (if (and (listp value)
                                                   (symbolp (car value))
                                                   (null (cdr value)))
                                              (car value)
                                            value))
                     (symbol :format "%v")))
            (choice :format "%[Operation mode%] %v"
                    (const :tag "Read everything" nil)
                    (cons :tag "Read selected faces" :format "%v"
                          (const :tag "Read selected faces"
                                 :format "%t" read)
                          (repeat :tag "" (sexp :format "%v")))
                    (cons :tag "Skip selected faces" :format "%v"
                          (const :tag "Skip selected faces"
                                 :format "%t" skip)
                          (repeat :tag "" (sexp :format "%v")))))))))


;;;###autoload
(defcustom wcheck-language-data nil
  "Language configuration for `wcheck-mode'.

Elements of this alist are of the form:

    (LANGUAGE (KEY . VALUE) [(KEY . VALUE) ...])

LANGUAGE is a name string for a language and KEY and VALUE pairs
denote settings for the language. Here is a description of
possible KEYs:

program
    VALUE can be a string or a function. If VALUE is a string, it
    must be the name of the external executable program which is
    responsible for text-checking LANGUAGE. Communication with
    the external program is managed through standard input and
    output streams.

    `wcheck-mode' collects text strings from the buffer and sends
    them (each on a separate line) to the external program for
    analyzing. The program must output (one way or another) the
    strings which it thinks should be marked in the Emacs buffer.
    The output of the program is then parsed with `parser'
    function (see below).

    VALUE can also be an Emacs Lisp function (a symbol or a
    lambda). Then that function is used as the text checker. The
    function is called with one argument: a list of strings
    collected from the buffer. The function is supposed to check
    them and return a list of strings (or nil). The returned
    strings will be marked in the buffer.

    See options `regexp-start', `regexp-body' and `regexp-end'
    below for details on how text is collected from the buffer.

args
    Optional command-line arguments for the program. The VALUE is
    a list of strings. Each string is a single argument for the
    program. (This option is ignored when the program is a
    function.)

parser
    VALUE of this option is an Emacs Lisp function which is
    responsible for parsing the output of `program'. This parser
    function is only used when `program' is an external
    executable program (not a function).

    The parser function is run without arguments and within the
    context of a buffer that contains all the output from the
    external program. The point is located at the beginning of
    the buffer. From that buffer the `parser' function should
    collect all the strings that are meant to be marked in the
    buffer that is being checked. The function must return them
    as a list of strings or nil if there are none.

    For the most common cases there are two parser functions
    already implemented:

        `wcheck-parser-lines' turns each line in program's output
        to a separate string. You should use this function as the
        output parser if you spell-check with Ispell-like program
        with its \"-l\" command-line option. They output each
        misspelled word on a separate line. This is the default
        output parser.

        `wcheck-parser-whitespace' turns each whitespace-
        separated token in the output to a separate string.

connection
    The VALUE is used to set variable `process-connection-type'
    when starting the process for LANGUAGE. If the VALUE is nil
    use a pipe for communication; if it's `pty' (or t) use a PTY.
    The default is to use a pipe (nil). (This option is ignored
    when the program is a function.)

face
    A symbol referring to the face which is used to mark text with
    this LANGUAGE. The default is `wcheck-default-face'.

syntax
    VALUE is a symbol referring to an Emacs syntax table. This
    will be the effective syntax table that is used with regular
    expressions. See the Info node `(elisp)Syntax Tables' for
    more information. The default value is
    `text-mode-syntax-table'.

regexp-start
regexp-body
regexp-end
    Regular expression strings which match the start of a string
    body, characters within the body and the end of the body,
    respectively.

    This is how they are used in practice: `wcheck-mode' scans
    buffer's content and looks for strings that match the
    construct `regexp-start + regexp-body + regexp-end'. Strings
    that match regexp-body (but not `regexp-discard', see below)
    are sent to the text checker program or function to analyze.

    Strings returned from the program or function are marked in
    Emacs buffer using the following construction: `regexp-start
    + (regexp-quote STRING) + regexp-end'. The middle part is
    marked with `face' (see above) .

    Do not use grouping constructs `\\( ... \\)' in the regular
    expressions because the back reference `\\1' is used for
    separating the body string from the start and end match. You
    can use \"shy\" groups `\\(?: ... \\)' which do not record
    the matched substring.

    The default values for the regular expressions are

        \\=\\<'*         (regexp-start)
        \\w+?         (regexp-body)
        '*\\=\\>         (regexp-end)

    Effectively they match a series of word characters defined in
    the effective syntax table. Single quotes (') at the start
    and end of a word are excluded. This is probably a good thing
    when using `wcheck-mode' as a spelling checker.

regexp-discard
    The string that matched `regexp-body' is then matched against
    the value of this option. If this regular expression matches,
    then the string is discarded and won't be sent to the
    text-checker program or function to analyze. You can use this
    to define exceptions to the regexp-body match. The default
    value is

        \\`'+\\'

    which discards the body string if it consists only of single
    quotes. This was chosen as the default because the default
    syntax table `text-mode-syntax-table' defines single quote as
    a word character. It's probably not useful to mark individual
    single quotes in a buffer when `wcheck-mode' is used as a
    spelling checker.

    If you don't want to have any discarding rules set this
    option to empty string.

case-fold
    This boolean value is used to set value for variable
    `case-fold-search' for LANGUAGE. Similarly to
    `case-fold-search' the nil value means case-sensitive and a
    non-nil means case-insensitive search. The default is
    case-sensitive (nil). Note that this only has effect on
    `wcheck-mode's internal regular expression search.

suggestion-program
suggestion-args
    `suggestion-program' is either name (a string) of an external
    executable program or an Emacs Lisp function (a symbol or a
    lambda). When it's the name of an executable program then
    `suggestion-args' are the command-line arguments (a list of
    strings) for the program.

    When user clicks the right mouse button on marked text, or
    executes command `wcheck-spelling-suggestions', the marked
    text will be sent to the `suggestion-program' as standard
    input stream. The program should send suggested substitutes
    to the standard output stream. The output is parsed with
    `suggestion-parser' function (see below).

    When `suggestion-program' is an Emacs Lisp function the
    function is called with one argument: the marked text (a
    string) which user wants spelling suggestions for. The
    function must return all substitute suggestions as a list of
    strings or nil if there are no suggestions.

suggestion-parser
    VALUE of this option is an Emacs Lisp function which is
    responsible for parsing the output of `suggestion-program'.
    This parser function is only used when `suggestion-program'
    is an external executable program (not a function).

    The parser function is run without arguments and within the
    context of a buffer that contains all the output from the
    external program. The point is located at the beginning of
    the buffer. `suggestion-parser' function should collect all
    the substitute suggestions from the buffer and return them as
    a list of strings or nil if there are no suggestions.

    For the most common cases there are three parser functions
    already implemented:

        `wcheck-parser-ispell-suggestions' parses substitute
        suggestions from the output of Ispell or compatible
        program, such as Enchant or Aspell. Use this function as
        the `suggestion-parser' if you get suggestions from an
        Ispell-like program with its \"-a\" command-line option.

        `wcheck-parser-lines' function turns each line in the
        output to individual substitute suggestions.

        `wcheck-parser-whitespace'. Each whitespace-separated
        token in the program's output is a separate suggestion.

read-or-skip-faces
    This option controls which faces `wcheck-mode' should read or
    skip when scanning buffer's content. The value must be a list
    and its items are also lists:

        (MAJOR-MODE [OPERATION-MODE [FACE ...]])

    MAJOR-MODE is a symbol or a list of symbols. Symbols refer to
    the major mode(s) which the settings are for. Use nil as the
    MAJOR-MODE to define default settings. Settings that come
    after the pseudo major-mode nil are ignored.

    OPERATION-MODE is symbol `read' or `skip' defining whether
    the FACEs should be read or skipped. If it's `read' then only
    the listed faces are read. If it's `skip' then the listed
    faces are skipped and all other faces are read. If
    OPERATION-MODE is nil or it doesn't exist at all then
    everything is read.

    The rest of the items are FACEs. They are typically symbols
    but some Emacs modes may use strings, property lists or cons
    cells for defining faces. For more information see Info
    node `(elisp) Special Properties'. Use nil as the face to
    refer to the normal text which does not have a face text
    property.

    Example:

        (read-or-skip-faces
         ((emacs-lisp-mode c-mode) read
          font-lock-comment-face font-lock-doc-face)
         (org-mode skip font-lock-comment-face org-link)
         (text-mode)
         (nil read nil))

    It says that in `emacs-lisp-mode' and `c-mode' only the text
    which have been highlighted with `font-lock-comment-face' or
    `font-lock-doc-face' is read (i.e., checked). In `org-mode'
    faces `font-lock-comment-face' and `org-link' are
    skipped (i.e., not checked) and all other faces are read. In
    `text-mode' everything is read. Finally, in all other major
    modes only the normal text (nil) is read.

    The global default is equivalent to

        (read-or-skip-faces
         (nil))

    which means that in all major modes read everything. It is
    sometimes useful to have this setting in language-specific
    options too because the parsing stops right there. Therefore
    it overrides all global settings which user may have
    configured with variable `wcheck-language-data-defaults'.

    Note: You can use command `\\[what-cursor-position]' with a
    prefix argument to see what faces are active at the cursor
    position. Then you can use the information to configure this
    option.

Here's an example value for the `wcheck-language-data' variable:

    ((\"Finnish\"
      (program . \"/usr/bin/enchant\")
      (args  \"-l\" \"-d\" \"fi\")
      (syntax . my-finnish-syntax-table)
      (suggestion-program . \"/usr/bin/enchant\")
      (suggestion-args \"-a\" \"-d\" \"fi\")
      (suggestion-parser . wcheck-parser-ispell-suggestions))
     (\"British English\"
      (program . \"/usr/bin/ispell\")
      (args \"-l\" \"-d\" \"british\")
      (suggestion-program . \"/usr/bin/ispell\")
      (suggestion-args \"-a\" \"-d\" \"british\")
      (suggestion-parser . wcheck-parser-ispell-suggestions))
     (\"Trailing whitespace\"
      (program . identity)
      (suggestion-program . (lambda (string) (list \"\")))
      (face . highlight)
      (regexp-start . \"\")
      (regexp-body . \"[ \\t]+\")
      (regexp-end . \"$\")
      (regexp-discard . \"\")
      (read-or-skip-faces)
       (nil))
     (\"Highlight FIXMEs\"
      (program . (lambda (strings)
                   (when (member \"FIXME\" strings)
                     (list \"FIXME\"))))
      (face . highlight)
      (read-or-skip-faces
       ((emacs-lisp-mode c-mode) read font-lock-comment-face)
       (nil))))

You can use variable `wcheck-language-data-defaults' to define
default values for all these options. The defaults are used when
language-specific option does not exist or is not valid."

  :group 'wcheck
  :type
  `(repeat
    (list :format "%v"
          (string :tag "Language")
          (repeat :inline t
                  :tag "Options"
                  ,wcheck-language-data-customize-interface))))


;;;###autoload
(defconst wcheck-language-data-defaults-hard-coded
  '((parser . wcheck-parser-lines)
    (connection . nil)
    (face . wcheck-default-face)
    (syntax . text-mode-syntax-table)
    (regexp-start . "\\<'*")
    (regexp-body . "\\w+?")
    (regexp-end . "'*\\>")
    (regexp-discard . "\\`'+\\'")
    (case-fold . nil)
    (read-or-skip-faces (nil)))
  "Hard-coded default language configuration for `wcheck-mode'.
This constant is for Wcheck mode's internal use only. This
provides useful defaults if both `wcheck-language-data' and
`wcheck-language-data-defaults' fail.")


;;;###autoload
(defcustom wcheck-language-data-defaults
  wcheck-language-data-defaults-hard-coded
  "Default language configuration for `wcheck-mode'.
These default values are used when language-specific settings
don't provide a valid value. `wcheck-mode' will choose some
useful defaults even if this variable is not (properly) set. See
variable `wcheck-language-data' for information about possible
settings.

Here's an example value for the variable:

    ((parser . wcheck-parser-lines)
     (suggestion-parser . wcheck-parser-ispell-suggestions)
     (connection . nil)
     (face . wcheck-default-face)
     (syntax . text-mode-syntax-table)
     (regexp-start . \"\\\\=\\<'*\")
     (regexp-body . \"\\\\w+?\")
     (regexp-end . \"'*\\\\=\\>\")
     (regexp-discard . \"\\\\`'+\\\\'\")
     (case-fold . nil)
     (read-or-skip-faces
      ((emacs-lisp-mode c-mode) read
       font-lock-comment-face font-lock-doc-face)
      (message-mode read nil
       message-header-subject message-cited-text)))"

  :group 'wcheck
  :type `(repeat ,wcheck-language-data-customize-interface))


(defvar wcheck-read-or-skip-faces nil
  "This variable is not used anymore.
This variable's functionality is now included in variables
`wcheck-language-data' and `wcheck-language-data-defaults'. See
the documentation of the former variable for information on how
to configure the feature.")


;;;###autoload
(defcustom wcheck-language ""
  "Default language for `wcheck-mode'.

Normally the global value defines the language for new buffers.
If a buffer-local value exists it is used instead. This variable
becomes automatically buffer-local when `wcheck-mode' is turned
on in a buffer, so changing the global value does not affect
buffers which already have `wcheck-mode' turned on.

User is free to set this variable directly (e.g., in programs)
but in interactive use it is usually better to use the command
`\\[wcheck-change-language]' instead. The command can change
language immediately while `wcheck-mode' is turned on, whereas
changing just the value of this variable takes effect only when
`wcheck-mode' is turned on next time."
  :type '(string :tag "Default language")
  :group 'wcheck)
(make-variable-buffer-local 'wcheck-language)


;;;###autoload
(defface wcheck-default-face
  '((t (:underline "red")))
  "Default face for marking strings in a buffer.
This is used when language does not define a face."
  :group 'wcheck)


;;; Variables


(defvar wcheck-mode nil)
(defvar wcheck-mode-map (make-sparse-keymap)
  "Keymap for `wcheck-mode'.")

(defvar wcheck-timer nil)
(defvar wcheck-timer-idle .3
  "`wcheck-mode' idle timer delay (in seconds).")
(defvar wcheck-timer-paint-event-count 0)

(defvar wcheck-timer-paint-event-count-std 3
  "Run buffer paint event this many times in a row.
With too low values all data from external processes may not have
arrived and window gets only partially painted. A higher value
increases the probability that windows get fully painted but it
also makes `wcheck-jump-forward' and `wcheck-jump-backward'
slower. A suitable compromise may be 3 or 4.")

(defvar wcheck-change-language-history nil
  "Language history for command `wcheck-change-language'.")

(defvar wcheck-buffer-data nil)

(defvar wcheck-jump-step 5000)


;;; Macros


(defmacro wcheck-define-condition (name superclass &optional message)
  `(progn
     (put ',name 'error-conditions
          (append (get ',superclass 'error-conditions) (list ',name)))
     (put ',name 'error-message ,message)
     ',name))


(defmacro wcheck-loop-over-reqs-engine (key var &rest body)
  `(dolist (,var (delq nil (mapcar (lambda (buffer)
                                     (when (wcheck-buffer-data-get
                                            :buffer buffer ,key)
                                       buffer))
                                   (wcheck-buffer-data-get-all :buffer))))
     (when (buffer-live-p ,var)
       (with-current-buffer ,var
         ,@body))))


(defmacro wcheck-loop-over-read-reqs (var &rest body)
  `(wcheck-loop-over-reqs-engine :read-req ,var ,@body))
(defmacro wcheck-loop-over-paint-reqs (var &rest body)
  `(wcheck-loop-over-reqs-engine :paint-req ,var ,@body))
(defmacro wcheck-loop-over-jump-reqs (var &rest body)
  `(wcheck-loop-over-reqs-engine :jump-req ,var ,@body))


(defmacro wcheck-with-language-data (language bindings &rest body)
  (let ((lang-var (make-symbol "--wck-language--")))
    `(let* ((,lang-var ,(cadr language))
            ,@(when (car language)
                `((,(car language) ,lang-var)))
            ,@(mapcar
               (lambda (var)
                 (cond ((symbolp var)
                        (list var `(wcheck-query-language-data
                                    ,lang-var ',var)))
                       ((and var (listp var))
                        (list (car var) `(wcheck-query-language-data
                                          ,lang-var ',(cadr var))))))
               bindings))
       ,@body)))


;;; Conditions


(wcheck-define-condition wcheck-error error)
(wcheck-define-condition wcheck-language-does-not-exist-error wcheck-error)
(wcheck-define-condition wcheck-program-not-configured-error wcheck-error)
(wcheck-define-condition wcheck-not-a-list-of-strings-error wcheck-error)
(wcheck-define-condition wcheck-funcall-error wcheck-error)
(wcheck-define-condition wcheck-suggestion-error wcheck-error)
(wcheck-define-condition wcheck-suggestion-program-error
                         wcheck-suggestion-error)
(wcheck-define-condition wcheck-parser-function-not-configured-error
                         wcheck-suggestion-error)
(wcheck-define-condition wcheck-overlay-not-found-error wcheck-error)


;;; Interactive commands


;;;###autoload
(defun wcheck-change-language (language &optional global)
  "Change language for current buffer (or globally).
Change `wcheck-mode' language to LANGUAGE. The change is
buffer-local but if GLOBAL is non-nil (prefix argument if called
interactively) then change the global default language."
  (interactive
   (let* ((comp (mapcar #'car wcheck-language-data))
          (default (cond ((and current-prefix-arg
                               (member (default-value 'wcheck-language) comp))
                          (default-value 'wcheck-language))
                         ((member wcheck-language comp)
                          wcheck-language)
                         ((car comp))
                         (t ""))))
     (list (completing-read
            (format (if current-prefix-arg
                        "Global default language (%s): "
                      "Language for the current buffer (%s): ")
                    default)
            comp nil t nil 'wcheck-change-language-history default)
           current-prefix-arg)))

  (condition-case error-data
      (when (stringp language)
        ;; Change the language, locally or globally, and update buffer
        ;; database, if needed.
        (if global
            ;; Just change the global value and leave.
            (setq-default wcheck-language language)

          ;; Change the buffer-local value.
          (setq wcheck-language language)
          ;; If the mode is currently turned on check if language's
          ;; checker program or function is configured and if all is OK
          ;; request update for the buffer.
          (when wcheck-mode
            (if (wcheck-program-configured-p wcheck-language)
                ;; It's OK; update the buffer.
                (progn
                  (wcheck-buffer-lang-proc-data-update
                   (current-buffer) wcheck-language)
                  (wcheck-buffer-data-set (current-buffer) :read-req t)
                  (wcheck-remove-overlays))

              (signal 'wcheck-program-not-configured-error wcheck-language))))

        ;; Return the language.
        language)

    (wcheck-program-not-configured-error
     (wcheck-mode -1)
     (message "Language \"%s\": checker program is not configured"
              (cdr error-data)))))


(defun wcheck-mode-turn-on ()
  ;; Turn the mode on, but first some checks.
  (let ((buffer (current-buffer))
        (language wcheck-language))
    (condition-case error-data
        (cond
         ((minibufferp buffer)
          (signal 'wcheck-error "Can't use `wcheck-mode' in a minibuffer"))

         ((not (wcheck-language-exists-p language))
          (signal 'wcheck-language-does-not-exist-error language))

         ((not (wcheck-program-configured-p language))
          (signal 'wcheck-program-not-configured-error language))

         (t
          (make-local-variable 'wcheck-language)
          (wcheck-add-local-hooks buffer)
          (wcheck-add-global-hooks)
          (wcheck-buffer-lang-proc-data-update buffer language)
          (wcheck-timer-start)
          (wcheck-buffer-data-set buffer :read-req t)))

      (wcheck-program-not-configured-error
       (wcheck-mode -1)
       (message "Language \"%s\": checker program not configured"
                (cdr error-data)))

      (wcheck-language-does-not-exist-error
       (wcheck-mode -1)
       (message "Language \"%s\" does not exist" (cdr error-data))))))


(defun wcheck-mode-turn-off ()
  (let ((buffer (current-buffer)))
    ;; We clear overlays form the buffer, remove the buffer from buffer
    ;; database.
    (wcheck-remove-overlays)
    (wcheck-buffer-lang-proc-data-update buffer nil)

    ;; If there are no buffers using wcheck-mode anymore, stop the idle
    ;; timer and remove global hooks.
    (when (null (wcheck-buffer-data-get-all :buffer))
      (wcheck-timer-stop)
      (wcheck-remove-global-hooks))
    (wcheck-remove-local-hooks buffer)))


(defun wcheck-mode-line-lang ()
  (condition-case nil
      (let (lang-code)
        (catch 'enough
          (mapc (lambda (c)
                  (when (char-equal ?w (char-syntax c))
                    (push c lang-code)
                    (when (>= (length lang-code) 2)
                      (throw 'enough t))))
                (wcheck-buffer-data-get :buffer (current-buffer) :language)))
        (apply #'string (nreverse lang-code)))
    (error "")))


;;;###autoload
(define-minor-mode wcheck-mode
  "General interface for text checkers.

With optional (prefix) ARG turn on the mode if ARG is positive,
otherwise turn it off. If ARG is not given toggle the mode.

Wcheck is a minor mode for automatically checking and marking
strings in Emacs buffer. Wcheck sends (parts of) buffer's content
to a text-checker back-end and, based on its output, decides if
some parts of text should be marked.

Wcheck can be used with external spell-checker programs such as
Ispell, Aspell and Enchant, but actually any tool that can
receive text stream from standard input and send text to standard
output can be used. The checker back-end can also be an Emacs
Lisp function.

In Wcheck mode different configuration units are called
\"languages\". See the documentation of variables
`wcheck-language-data', `wcheck-language-data-defaults' and
`wcheck-language' for information on how to configure Wcheck
mode. You can access the variables through customize group
`wcheck'.

Interactive command `wcheck-change-language' is used to switch
languages. Command `wcheck-spelling-suggestions' gives spelling
suggestions for marked text at point (also accessible through the
right-click mouse menu). Commands `wcheck-jump-forward' and
`wcheck-jump-backward' move point to next/previous marked text
area."

  :init-value nil
  :lighter (" W:" (:eval (wcheck-mode-line-lang)))
  :keymap wcheck-mode-map

  (condition-case error-data
      (if wcheck-mode
          (wcheck-mode-turn-on)
        (wcheck-mode-turn-off))

    (wcheck-error
     (wcheck-mode -1)
     (message "%s" (cdr error-data)))))


;;; Timers


(defun wcheck-timer-start ()
  "Start `wcheck-mode' idle timer if it's not running already."
  (unless wcheck-timer
    (setq wcheck-timer
          (run-with-idle-timer wcheck-timer-idle t
                               #'wcheck-timer-read-event))))


(defun wcheck-timer-stop ()
  "Stop `wcheck-mode' idle timer."
  (when wcheck-timer
    (cancel-timer wcheck-timer)
    (setq wcheck-timer nil)))


(defun wcheck-funcall-after-idle (function &rest args)
  (apply #'run-with-idle-timer
         (+ wcheck-timer-idle (wcheck-current-idle-time-seconds))
         nil function args))


(defun wcheck-timer-paint-event-run (&optional count)
  (if (integerp count)
      (let ((at-least (max count wcheck-timer-paint-event-count)))
        (if (> wcheck-timer-paint-event-count 0)
            (setq wcheck-timer-paint-event-count at-least)
          (setq wcheck-timer-paint-event-count at-least)
          (wcheck-funcall-after-idle #'wcheck-timer-paint-event)))
    (if (> (setq wcheck-timer-paint-event-count
                 (1- wcheck-timer-paint-event-count))
           0)
        (wcheck-funcall-after-idle #'wcheck-timer-paint-event)
      (wcheck-timer-jump-event))))


(defun wcheck-force-read (buffer)
  (redisplay t)
  (wcheck-buffer-data-set buffer :read-req t)
  (wcheck-timer-read-event))


(defun wcheck-timer-read-event ()
  "Send windows' content to checker program or function.

This function is usually called by the `wcheck-mode' idle timer.
The function walks through all windows which belong to buffers
that have requested update. It reads windows' content and sends
it checker program or function associated with the buffer's
language. Finally, this function starts another idle timer for
marking strings in buffers."

  (wcheck-loop-over-read-reqs
   buffer

   (unless (wcheck-buffer-data-get :buffer buffer :jump-req)
     ;; We are about to fulfill buffer's window-reading request so
     ;; remove the request. Reset also the list of received strings and
     ;; visible window areas.
     (wcheck-buffer-data-set buffer :read-req nil)
     (wcheck-buffer-data-set buffer :strings nil)
     (wcheck-buffer-data-set buffer :areas nil)

     ;; Walk through all windows which belong to this buffer.
     (let (area-alist strings)
       (walk-windows #'(lambda (window)
                         (when (eq buffer (window-buffer window))
                           ;; Store the visible buffer area.
                           (push (cons (window-start window)
                                       (window-end window t))
                                 area-alist)))
                     'nomb t)

       ;; Combine overlapping buffer areas and read strings from all
       ;; areas.
       (let ((combined (wcheck-combine-overlapping-areas area-alist)))
         (wcheck-buffer-data-set buffer :areas combined)
         (dolist (area combined)
           (setq strings (append (wcheck-read-strings
                                  buffer (car area) (cdr area))
                                 strings))))
       ;; Send strings to checker engine.
       (wcheck-send-strings buffer strings))))

  ;; Start a timer which will mark text in buffers/windows.
  (wcheck-timer-paint-event-run wcheck-timer-paint-event-count-std))


(defun wcheck-send-strings (buffer strings)
  "Send STRINGS for the process that handles BUFFER.
STRINGS is a list of strings to be sent as input for the external
process which handles BUFFER. Each string in STRINGS is sent as
separate line."
  (wcheck-with-language-data
   (language (wcheck-buffer-data-get :buffer buffer :language))
   (program)

   (condition-case nil
       (cond ((or (wcheck-buffer-data-get :buffer buffer :process)
                  (stringp program))
              (process-send-string
               (wcheck-start-get-process buffer)
               (concat (mapconcat #'identity strings "\n") "\n"))
              (condition-case nil
                  (with-current-buffer
                      (process-buffer (wcheck-buffer-data-get
                                       :buffer buffer :process))
                    (erase-buffer))
                (error nil)))

             ((functionp program)
              (when (buffer-live-p buffer)
                (with-current-buffer buffer
                  (let ((received
                         (save-match-data
                           (condition-case nil (funcall program strings)
                             (error (signal 'wcheck-funcall-error nil))))))
                    (if (wcheck-list-of-strings-p received)
                        (when received
                          (wcheck-buffer-data-set buffer :strings received)
                          (wcheck-buffer-data-set buffer :paint-req t))
                      (signal 'wcheck-not-a-list-of-strings-error nil)))))))

     (wcheck-not-a-list-of-strings-error
      (with-current-buffer buffer
        (wcheck-mode -1)
        (message (concat "Checker function did not return a list of "
                         "strings (or nil)"))))

     (wcheck-funcall-error
      (message "Checker function signaled an error")))))


(defun wcheck-receive-strings (process string)
  "`wcheck-mode' process output handler function."
  (let ((buffer (wcheck-buffer-data-get :process process :buffer))
        (parser (wcheck-query-language-data
                 (wcheck-buffer-data-get :process process :language)
                 'parser)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer

        ;; If process is running proceed to collect and paint the
        ;; strings.
        (condition-case error-data
            (if (wcheck-process-running-p process)
                (with-current-buffer (process-buffer process)
                  (save-excursion
                    (goto-char (point-max))
                    (insert string)
                    (let ((parsed-strings
                           (save-match-data
                             (save-excursion
                               (goto-char (point-min))
                               (condition-case nil (funcall parser)
                                 (error (signal 'wcheck-funcall-error
                                                nil)))))))
                      (when (and parsed-strings
                                 (wcheck-list-of-strings-p parsed-strings))
                        (wcheck-buffer-data-set buffer :strings parsed-strings)
                        (wcheck-buffer-data-set buffer :paint-req t)))))

              ;; It's not running. Turn off the mode.
              (wcheck-mode -1)
              (signal 'wcheck-error "Process is not running for buffer \"%s\""
                      (buffer-name buffer)))

          (wcheck-funcall-error
           (message "Checker output parser function signaled an error"))

          (wcheck-error
           (message "%s" (cdr error-data))))))))


(defun wcheck-timer-paint-event ()
  "Mark strings in windows.

This is normally called by the `wcheck-mode' idle timer. This
function marks (with overlays) strings in the buffers that have
requested it."

  (wcheck-loop-over-paint-reqs
   buffer

   (unless (wcheck-buffer-data-get :buffer buffer :jump-req)
     (wcheck-remove-overlays))
   ;; We are about to mark text in this buffer so remove this buffer's
   ;; request.
   (wcheck-buffer-data-set buffer :paint-req nil)
   ;; Walk through the visible text areas and mark text based on the
   ;; string list returned by an external process.
   (when wcheck-mode
     (dolist (area (wcheck-buffer-data-get :buffer buffer :areas))
       (wcheck-paint-strings buffer (car area) (cdr area)
                             (wcheck-buffer-data-get :buffer buffer
                                                     :strings)
                             ;; If jump-req is active then paint
                             ;; invisible text too.
                             (wcheck-buffer-data-get :buffer buffer
                                                     :jump-req)))))

  (wcheck-timer-paint-event-run))


(defun wcheck-timer-jump-event ()
  (wcheck-loop-over-jump-reqs
   buffer

   (let* ((jump-req (wcheck-buffer-data-get :buffer buffer :jump-req))
          (start (wcheck-jump-req-start jump-req))
          (bound (wcheck-jump-req-bound jump-req))
          (window (wcheck-jump-req-window jump-req)))

     (wcheck-buffer-data-set buffer :jump-req nil)

     (condition-case nil
         (cond ((> bound start)
                (let ((ol (wcheck-overlay-next start bound)))
                  (cond (ol
                         (if (and (window-live-p window)
                                  (eq buffer (window-buffer window)))
                             (set-window-point window (overlay-end ol))
                           (goto-char (overlay-end ol)))
                         (when (invisible-p (point))
                           (show-entry))
                         (message "Found from line %s"
                                  (line-number-at-pos (point)))
                         (wcheck-force-read buffer))
                        ((< bound (point-max))
                         (wcheck-jump-req buffer window (1+ bound)
                                          (+ (1+ bound) wcheck-jump-step)))
                        (t
                         (signal 'wcheck-overlay-not-found-error nil)))))
               ((< bound start)
                (let ((ol (wcheck-overlay-previous start bound)))
                  (cond (ol
                         (if (and (window-live-p window)
                                  (eq buffer (window-buffer window)))
                             (set-window-point window (overlay-start ol))
                           (goto-char (overlay-start ol)))
                         (when (invisible-p (point))
                           (show-entry))
                         (message "Found from line %s"
                                  (line-number-at-pos (point)))
                         (wcheck-force-read buffer))
                        ((> bound (point-min))
                         (wcheck-jump-req buffer window (1- bound)
                                          (- (1- bound) wcheck-jump-step)))
                        (t
                         (signal 'wcheck-overlay-not-found-error nil)))))
               (t
                (signal 'wcheck-overlay-not-found-error nil)))

       (wcheck-overlay-not-found-error
        (message "Found nothing")
        (wcheck-force-read buffer))))))


;;; Hooks


(defun wcheck-add-local-hooks (buffer)
  (with-current-buffer buffer
    (dolist (hook '((kill-buffer-hook . wcheck-hook-kill-buffer)
                    (window-scroll-functions . wcheck-hook-window-scroll)
                    (after-change-functions . wcheck-hook-after-change)
                    (change-major-mode-hook . wcheck-hook-change-major-mode)
                    (outline-view-change-hook
                     . wcheck-hook-outline-view-change)))
      (add-hook (car hook) (cdr hook) nil t))))


(defun wcheck-remove-local-hooks (buffer)
  (with-current-buffer buffer
    (dolist (hook '((kill-buffer-hook . wcheck-hook-kill-buffer)
                    (window-scroll-functions . wcheck-hook-window-scroll)
                    (after-change-functions . wcheck-hook-after-change)
                    (change-major-mode-hook . wcheck-hook-change-major-mode)
                    (outline-view-change-hook
                     . wcheck-hook-outline-view-change)))
      (remove-hook (car hook) (cdr hook) t))))


(defun wcheck-add-global-hooks ()
  (dolist (hook '((window-size-change-functions
                   . wcheck-hook-window-size-change)
                  (window-configuration-change-hook
                   . wcheck-hook-window-configuration-change)))
    (add-hook (car hook) (cdr hook))))


(defun wcheck-remove-global-hooks ()
  (dolist (hook '((window-size-change-functions
                   . wcheck-hook-window-size-change)
                  (window-configuration-change-hook
                   . wcheck-hook-window-configuration-change)))
    (remove-hook (car hook) (cdr hook))))


(defun wcheck-hook-window-scroll (window window-start)
  "`wcheck-mode' hook for window scroll.
Request update for the buffer when its window have been
scrolled."
  (with-current-buffer (window-buffer window)
    (when wcheck-mode
      (wcheck-buffer-data-set (current-buffer) :read-req t))))


(defun wcheck-hook-window-size-change (frame)
  "`wcheck-mode' hook for window size change.
Request update for the buffer when its window's size has
changed."
  (walk-windows #'(lambda (window)
                    (with-current-buffer (window-buffer window)
                      (when wcheck-mode
                        (wcheck-buffer-data-set (current-buffer)
                                                :read-req t))))
                'nomb
                frame))


(defun wcheck-hook-window-configuration-change ()
  "`wcheck-mode' hook for window configuration change.
Request update for the buffer when its window's configuration has
changed."
  (walk-windows #'(lambda (window)
                    (with-current-buffer (window-buffer window)
                      (when wcheck-mode
                        (wcheck-buffer-data-set (current-buffer)
                                                :read-req t))))
                'nomb
                'currentframe))


(defun wcheck-hook-after-change (beg end len)
  "`wcheck-mode' hook for buffer content change.
Request update for the buffer when its content has been edited."
  ;; The buffer that has changed is the current buffer when this hook
  ;; function is called.
  (when wcheck-mode
    (wcheck-buffer-data-set (current-buffer) :read-req t)))


(defun wcheck-hook-outline-view-change ()
  "`wcheck-mode' hook for outline view change.
Request update for the buffer when its outline view has changed."
  (when wcheck-mode
    (wcheck-buffer-data-set (current-buffer) :read-req t)))


(defun wcheck-hook-kill-buffer ()
  "`wcheck-mode' hook for kill-buffer operation.
Turn off `wcheck-mode' when buffer is being killed."
  (wcheck-mode -1))


(defun wcheck-hook-change-major-mode ()
  "`wcheck-mode' hook for major mode change.
Turn off `wcheck-mode' before changing major mode."
  (wcheck-mode -1))


;;; Processes


(defun wcheck-start-get-process (buffer)
  "Start or get external process for BUFFER.
Start a new process or get already existing process for BUFFER.
Return the object of that particular process or nil if the
operation was unsuccessful."
  ;; If process for this BUFFER exists return it.
  (or (wcheck-buffer-data-get :buffer buffer :process)
      ;; It doesn't exist so start a new one.
      (wcheck-with-language-data
       (language (wcheck-buffer-data-get :buffer buffer :language))
       (program args (process-connection-type connection))

       (when (wcheck-program-executable-p program)
         ;; Start the process.
         (let ((proc (apply #'start-process "wcheck" nil program args)))
           ;; Add the process Lisp object to database.
           (wcheck-buffer-data-set buffer :process proc)
           ;; Set the output handler function and the associated buffer.
           (set-process-filter proc #'wcheck-receive-strings)
           (set-process-buffer proc (generate-new-buffer
                                     (concat " *wcheck-process <"
                                             (buffer-name buffer) ">*")))
           ;; Prevent Emacs from querying user about running processes
           ;; when killing Emacs.
           (set-process-query-on-exit-flag proc nil)
           ;; Return the process object.
           proc)))))


(defun wcheck-buffer-lang-proc-data-update (buffer language)
  "Update process and language data for BUFFER.
Calling this function is the primary way to maintain the language
and process data associated to BUFFER. If LANGUAGE is nil remove
BUFFER from the list."
  (when (and (bufferp buffer)
             (or (stringp language)
                 (not language)))

    ;; Construct a list of currently used processes.
    (let ((old-processes (remq nil (wcheck-buffer-data-get-all :process))))

      ;; Remove dead buffers and possible minibuffers from the list.
      (dolist (item (wcheck-buffer-data-get-all :buffer))
        (when (or (not (buffer-live-p item))
                  (minibufferp item))
          (wcheck-buffer-data-delete item)))

      (if language
          (progn
            ;; LANGUAGE was given. If data for this buffer does not
            ;; exist create it.
            (unless (wcheck-buffer-data-get :buffer buffer)
              (wcheck-buffer-data-create buffer))
            ;; Add this BUFFER's language info and reset the process
            ;; info.
            (wcheck-buffer-data-set buffer :language language)
            (wcheck-buffer-data-set buffer :process nil))

        ;; LANGUAGE was not given so this normally means that
        ;; wcheck-mode is being turned off for this buffer. Remove
        ;; BUFFER's data.
        (wcheck-buffer-data-delete buffer))

      ;; Construct a list of processes that are still used.
      (let ((new-processes (remq nil (wcheck-buffer-data-get-all :process))))
        ;; Stop those processes which are no longer needed.
        (dolist (proc old-processes)
          (unless (memq proc new-processes)
            (kill-buffer (process-buffer proc))
            (delete-process proc))))))

  (wcheck-buffer-data-get :buffer buffer))


;;; Read and paint strings


(defun wcheck-read-strings (buffer beg end &optional invisible)
  "Return a list of text elements in BUFFER.
Scan BUFFER between positions BEG and END and search for text
elements according to buffer's language settings (see
`wcheck-language-data'). If INVISIBLE is non-nil read all buffer
areas, including invisible ones. Otherwise skip invisible text."

  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-excursion

        (when font-lock-mode
          (save-excursion
            (font-lock-fontify-region (min beg end) (max beg end))))

        (wcheck-with-language-data
         (language (wcheck-buffer-data-get :buffer buffer :language))
         (regexp-start regexp-body regexp-end regexp-discard
                       syntax (case-fold-search case-fold))

         (let ((regexp
                (concat regexp-start "\\(" regexp-body "\\)" regexp-end))
               (face-p (wcheck-generate-face-predicate language major-mode))
               (search-spaces-regexp nil)
               (old-point 0)
               strings)

           (with-syntax-table (eval syntax)
             (goto-char beg)
             (save-match-data
               (while (and (re-search-forward regexp end t)
                           (> (point) old-point))
                 (cond ((and (not invisible)
                             (invisible-p (match-beginning 1)))
                        ;; This point is invisible. Let's jump forward
                        ;; to next change of "invisible" property.
                        (goto-char (next-single-char-property-change
                                    (match-beginning 1) 'invisible buffer
                                    end)))

                       ((and (eval face-p)
                             (or (equal regexp-discard "")
                                 (not (string-match
                                       regexp-discard
                                       (match-string-no-properties 1)))))
                        ;; Add the match to the string list.
                        (add-to-list
                         'strings (match-string-no-properties 1))))
                 (setq old-point (point)))))
           strings))))))


(defun wcheck-paint-strings (buffer beg end strings &optional invisible)
  "Mark strings in buffer.

Mark all strings in STRINGS which are visible in BUFFER within
position range from BEG to END. If INVISIBLE is non-nil paint all
buffer areas, including invisible ones. Otherwise skip invisible
text."

  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-excursion

        (wcheck-with-language-data
         (language (wcheck-buffer-data-get :buffer buffer :language))
         (regexp-start regexp-end syntax (case-fold-search case-fold)
                       (ol-face face) suggestion-program)

         (let ((face-p (wcheck-generate-face-predicate language major-mode))
               (search-spaces-regexp nil)
               (ol-keymap (make-sparse-keymap))
               (ol-mouse-face nil)
               (ol-help-echo nil)
               regexp old-point)

           (when suggestion-program
             (define-key ol-keymap [down-mouse-3] 'wcheck-mouse-click-overlay)
             (define-key ol-keymap [mouse-3] 'undefined)
             (setq ol-mouse-face 'highlight
                   ol-help-echo "mouse-3: show suggestions"))

           (with-syntax-table (eval syntax)
             (save-match-data
               (dolist (string strings)
                 (setq regexp (concat regexp-start "\\("
                                      (regexp-quote string) "\\)"
                                      regexp-end)
                       old-point 0)
                 (goto-char beg)

                 (while (and (re-search-forward regexp end t)
                             (> (point) old-point))
                   (cond ((and (not invisible)
                               (invisible-p (match-beginning 1)))
                          ;; The point is invisible so jump forward to
                          ;; the next change of "invisible" text
                          ;; property.
                          (goto-char (next-single-char-property-change
                                      (match-beginning 1) 'invisible buffer
                                      end)))
                         ((eval face-p)
                          ;; Make an overlay.
                          (wcheck-make-overlay
                           buffer ol-face ol-mouse-face ol-help-echo ol-keymap
                           (match-beginning 1) (match-end 1))))
                   (setq old-point (point))))))))))))


;;; Jump forward or backward


(defun wcheck-overlay-next (start bound)
  (catch 'overlay
    (dolist (ol (overlays-at start))
      (when (overlay-get ol 'wcheck-mode)
        (throw 'overlay ol)))
    (let ((pos start))
      (while (and (setq pos (next-overlay-change pos))
                  (< pos (min bound (point-max))))
        (dolist (ol (overlays-at pos))
          (when (overlay-get ol 'wcheck-mode)
            (throw 'overlay ol)))))))


(defun wcheck-overlay-previous (start bound)
  (catch 'overlay
    (let ((pos start))
      (while (and (setq pos (previous-overlay-change pos))
                  (> pos (max bound (point-min))))
        (dolist (ol (overlays-at pos))
          (when (overlay-get ol 'wcheck-mode)
            (throw 'overlay ol)))))))


(defun wcheck-line-start-at (pos)
  (save-excursion
    (goto-char pos)
    (line-beginning-position)))


(defun wcheck-line-end-at (pos)
  (save-excursion
    (goto-char pos)
    (line-end-position)))


(defun wcheck-jump-req (buffer window start bound)
  (unless (= start bound)
    (with-current-buffer buffer
      (setq bound (funcall (if (> bound start)
                               'wcheck-line-end-at
                             'wcheck-line-start-at)
                           bound))
      (message "Searching in lines %d-%d..."
               (line-number-at-pos start)
               (line-number-at-pos bound))
      (wcheck-buffer-data-set buffer :jump-req (wcheck-jump-req-create
                                                window start bound))
      (wcheck-buffer-data-set buffer :areas (list (cons (min start bound)
                                                        (max start bound))))
      (wcheck-send-strings buffer (wcheck-read-strings
                                   buffer (min start bound)
                                   (max start bound) t))
      (wcheck-timer-paint-event-run wcheck-timer-paint-event-count-std))))


(defun wcheck-invisible-text-in-area-p (buffer beg end)
  (catch 'invisible
    (let ((pos (min beg end))
          (end (max beg end)))
      (when (invisible-p pos)
        (throw 'invisible t))
      (while (and (setq pos (next-single-char-property-change
                             pos 'invisible buffer))
                  (< pos end))
        (when (invisible-p pos)
          (throw 'invisible t))))))


;;;###autoload
(defun wcheck-jump-forward ()
  "Move point forward to next marked text area."
  (interactive)
  (let ((buffer (current-buffer))
        (window (selected-window)))
    (unless wcheck-mode
      (wcheck-mode 1))
    (when wcheck-mode
      (wcheck-buffer-data-set buffer :jump-req nil)
      (let ((ol (wcheck-overlay-next
                 (point) (window-end (selected-window) t))))
        (if (and ol (not (wcheck-invisible-text-in-area-p
                          buffer (point) (overlay-end ol))))
            (goto-char (overlay-end ol))
          (if (eobp)
              (message "End of buffer")
            (wcheck-jump-req buffer window (point)
                             (+ (point) wcheck-jump-step))))))))


;;;###autoload
(defun wcheck-jump-backward ()
  "Move point backward to previous marked text area."
  (interactive)
  (let ((buffer (current-buffer))
        (window (selected-window)))
    (unless wcheck-mode
      (wcheck-mode 1))
    (when wcheck-mode
      (wcheck-buffer-data-set buffer :jump-req nil)
      (let ((ol (wcheck-overlay-previous
                 (point) (window-start (selected-window)))))
        (if (and ol (not (wcheck-invisible-text-in-area-p
                          buffer (point) (overlay-start ol))))
            (goto-char (overlay-start ol))
          (if (bobp)
              (message "Beginning of buffer")
            (wcheck-jump-req buffer window (point)
                             (- (point) wcheck-jump-step))))))))


;;; Spelling suggestions


(defun wcheck-marked-text-at (pos)
  "Return information about marked text at POS.
POS is a buffer position. The return value is a vector of three
items: (1) the marked text string, (2) marker at the beginning of
the text and (3) marker at the end of the text."
  (let ((overlay (catch 'my-overlay
                   (dolist (ol (overlays-at pos))
                     (when (overlay-get ol 'wcheck-mode)
                       (throw 'my-overlay ol))))))
    (when overlay
      (let ((start (copy-marker (overlay-start overlay)))
            (end (copy-marker (overlay-end overlay))))
        (vector (buffer-substring-no-properties start end)
                start end)))))


;;;###autoload
(defun wcheck-spelling-suggestions (pos &optional event)
  "Offer spelling suggestions for marked text.

This function is usually called through a right mouse button
event or interactively by a user. In both cases function's
arguments are filled automatically.

If buffer position POS is on marked text (and substitute
suggestion program is properly configured) show a menu of
suggested substitutions. When this function is called
interactively POS is automatically the current point position.
Optional EVENT argument is a mouse event which is present if this
function is called through a right mouse button click on marked
text. If EVENT is non-nil use a graphic toolkit's menu (when
available) for selecting suggestions. Otherwise use a text menu.

When user chooses one of the suggestions from the menu the
original marked text is replaced with the chosen substitute.
Function returns the replacement text (string) or nil if nothing
was replaced."

  (interactive "d")
  (condition-case error-data
      (let ((overlay-data (or (wcheck-marked-text-at pos)
                              (wcheck-marked-text-at (1- pos))))
            (return-value nil))
        (if (not overlay-data)
            (signal 'wcheck-suggestion-error "There is no marked text here")
          (let* ((text (aref overlay-data 0))
                 (start (aref overlay-data 1))
                 (end (aref overlay-data 2))
                 (suggestions (wcheck-get-suggestions wcheck-language text))
                 (choice (if (and (display-popup-menus-p) event)
                             (wcheck-choose-suggestion-popup
                              suggestions event)
                           (wcheck-choose-suggestion-minibuffer
                            suggestions))))
            (when (and (stringp choice)
                       (markerp start)
                       (markerp end))
              (with-current-buffer (marker-buffer start)
                (if buffer-read-only
                    (signal 'wcheck-suggestion-error "Buffer is read-only")
                  (delete-region start end)
                  (goto-char start)
                  (insert choice)
                  (setq return-value choice))))
            (if (markerp start) (set-marker start nil))
            (if (markerp end) (set-marker end nil))))
        return-value)

    (wcheck-suggestion-program-error
     (message "Language \"%s\": suggestion program is not configured"
              (cdr error-data)))

    (wcheck-parser-function-not-configured-error
     (message "Language \"%s\": parser function is not configured"
              (cdr error-data)))

    (wcheck-not-a-list-of-strings-error
     (message (concat "Suggestion program or parser function did not return "
                      "a list of strings (or nil)")))

    (wcheck-error
     (message "%s" (cdr error-data)))))


(defun wcheck-get-suggestions (language text)
  "Get suggestions from external program or function.

If LANGUAGE uses an external program for suggestions, then this
function runs the program and sends TEXT as standard input stream
for the program. Program's output is parsed with user-configured
parser function (see `wcheck-language-data') and possible
substitute suggestions are returned as a list of strings (or nil
if there aren't any).

If LANGUAGE uses an Emacs Lisp function for suggestions, then
call the function with single argument TEXT. The function must
return substitute suggestions as a list of strings (or nil if
there aren't any)."

  (wcheck-with-language-data
   (nil language)
   ((program suggestion-program)
    (args suggestion-args)
    (parser suggestion-parser))

   (cond ((not (wcheck-suggestion-program-configured-p language))
          (signal 'wcheck-suggestion-program-error language))

         ((and (stringp program)
               (not parser))
          (signal 'wcheck-parser-function-not-configured-error language))

         ((stringp program)
          (with-temp-buffer
            (insert text)
            (apply #'call-process-region (point-min) (point-max)
                   program t t nil args)
            (goto-char (point-min))
            (let ((suggestions
                   (save-match-data
                     (condition-case nil (funcall parser)
                       (error (signal 'wcheck-funcall-error
                                      (concat "Suggestion parser function "
                                              "signaled an error")))))))
              (if (wcheck-list-of-strings-p suggestions)
                  suggestions
                (signal 'wcheck-not-a-list-of-strings-error nil)))))

         ((functionp program)
          (let ((suggestions
                 (save-match-data
                   (condition-case nil (funcall program text)
                     (error (signal 'wcheck-funcall-error
                                    (concat "Suggestion function signaled "
                                            "an error")))))))
            (if (wcheck-list-of-strings-p suggestions)
                suggestions
              (signal 'wcheck-not-a-list-of-strings-error nil)))))))


(defun wcheck-clean-string (string)
  (if (equal string "")
      "[Empty string]"
    (setq string (replace-regexp-in-string "[^[:print:]]+" "" string))
    (if (not (string-match "[^[:space:]]" string))
        "[Space or control chars]"
      (replace-regexp-in-string "\\(?:\\` +\\| +\\'\\)" "" string))))


(defun wcheck-choose-suggestion-popup (suggestions event)
  "Create a pop-up menu to choose a substitute suggestion.
SUGGESTIONS is a list of strings. EVENT is the mouse event that
originated this sequence of function calls. Return user's
choice (a string) or nil."
  (let ((menu (list "Choose a substitute"
                    (cons "" (if suggestions
                                 (mapcar #'(lambda (item)
                                             (cons (wcheck-clean-string item)
                                                   item))
                                         suggestions)
                               (list "[No suggestions]"))))))
    (x-popup-menu event menu)))


(defun wcheck-read-key (prompt)
  (if (fboundp 'read-key)
      (read-key prompt)
    (read-char prompt)))


(defun wcheck-choose-suggestion-minibuffer (suggestions)
  "Create a text menu to choose a substitute suggestion.
SUGGESTIONS is a list of strings. Return user's choice (a string)
or nil."
  (if suggestions
      (let ((chars (append (number-sequence ?1 ?9) (list ?0)
                           (number-sequence ?a ?z)))
            alist)

        (with-temp-buffer
          (setq mode-line-format (list "--- Choose a substitute %-")
                cursor-type nil
                truncate-lines t)

          (let (sug string)
            (while (and suggestions chars)
              (setq sug (car suggestions)
                    suggestions (cdr suggestions)
                    string (concat (propertize (format "%c)" (car chars))
                                               'face 'bold)
                                   " " (wcheck-clean-string sug) "  ")
                    alist (cons (cons (car chars) sug) alist)
                    chars (cdr chars))
              (insert string)
              (when (and suggestions chars
                         (> (+ (- (point) (line-beginning-position))
                               (length (concat "x) " (car suggestions))))
                            (window-width)))
                (delete-char -2)
                (newline 1))))

          (delete-char -2)
          (goto-char (point-min))
          (setq buffer-read-only t)

          (let* ((window-min-height 2)
                 (split-window-keep-point t)
                 (window (split-window-vertically
                          (- 0 (min (count-lines (point-min) (point-max))
                                    (- (window-body-height) 2))
                             1)))
                 (prompt
                  (apply #'propertize
                         (let ((last (caar alist)))
                           (format "Number %s(%s):"
                                   (if (memq last (number-sequence ?a ?z))
                                       "or letter "
                                     "")
                                   (cond ((= last ?1) "1")
                                         ((memq last (number-sequence ?2 ?9))
                                          (format "1-%c" last))
                                         ((= last ?0) "1-9,0")
                                         ((= last ?a) "1-9,0,a")
                                         ((memq last (number-sequence ?b ?z))
                                          (format "1-9,0,a-%c" last))
                                         (t ""))))
                         minibuffer-prompt-properties)))
            (set-window-buffer window (current-buffer))
            (set-window-dedicated-p window t)
            ;; Return the choice or nil.
            (cond ((cdr (assq (wcheck-read-key prompt) alist)))
                  (t (message "Abort") nil)))))
    (message "No suggestions")
    nil))


(defun wcheck-parser-lines ()
  "Parser for newline-separated output.
Return current buffer's lines as a list of strings."
  (delete-dups (split-string (buffer-substring-no-properties
                              (point-min) (point-max))
                             "\n+" t)))


(defun wcheck-parser-whitespace ()
  "Parser for whitespace-separated output.
Split current buffer's content to whitespace-separated tokens and
return them as a list of strings."
  (delete-dups (split-string (buffer-substring-no-properties
                              (point-min) (point-max))
                             "[ \f\t\n\r\v]+" t)))


(defun wcheck-parser-ispell-suggestions ()
  "Parser for Ispell-compatible programs' spelling suggestions."
  (let ((search-spaces-regexp nil))
    (when (re-search-forward "^& [^ ]+ \\([0-9]+\\) [0-9]+: \\(.+\\)$" nil t)
      (let ((count (string-to-number (match-string-no-properties 1)))
            (words (split-string (match-string-no-properties 2) ", " t)))
        (delete-dups (nbutlast words (- (length words) count)))))))


(define-obsolete-function-alias 'wcheck-parse-suggestions-lines
  'wcheck-parser-lines "2011.02.20")
(define-obsolete-function-alias 'wcheck-parse-suggestions-ws
  'wcheck-parser-whitespace "2011.02.20")
(define-obsolete-function-alias 'wcheck-parse-suggestions-ispell
  'wcheck-parser-ispell-suggestions "2011.02.20")


;;; Face information functions


(defun wcheck-collect-faces (beg end)
  "Return a list of faces between positions BEG and END."
  (let ((pos beg)
        face faces)
    (while (< pos end)
      (setq face (get-text-property pos 'face)
            pos (1+ pos))
      (if (and face (listp face))
          (setq faces (append face faces))
        (push face faces)))
    (delete-dups faces)))


(defun wcheck-major-mode-face-settings (language major-mode)
  "Return read/skip face settings for MAJOR-MODE."
  (let ((data (wcheck-query-language-data language 'read-or-skip-faces))
        conf)
    (catch 'answer
      (while data
        (setq conf (pop data))
        (when (or (eq nil (car conf))
                  (eq major-mode (car conf))
                  (and (listp (car conf))
                       (memq major-mode (car conf))))
          (throw 'answer conf))))))


(defun wcheck-face-found-p (user-faces buffer-faces)
  "Return t if a symbol in USER-FACES is found from BUFFER-FACES.
Both arguments are lists."
  (catch 'found
    (dolist (face user-faces)
      (when (member face buffer-faces)
        (throw 'found t)))))


(defun wcheck-generate-face-predicate (language major-mode)
  "Generates a face predicate expression for scanning buffer.
Return a predicate expression that is used to decide whether
`wcheck-mode' should read or paint text at the current point
position with LANGUAGE and MAJOR-MODE. Evaluating the predicate
expression will return a boolean."
  (let* ((face-settings (wcheck-major-mode-face-settings
                         language major-mode))
         (mode (nth 1 face-settings))
         (faces (nthcdr 2 face-settings)))
    (cond ((not font-lock-mode)
           t)
          ((eq mode 'read)
           `(wcheck-face-found-p
             ',faces (wcheck-collect-faces
                      (match-beginning 1) (match-end 1))))
          ((eq mode 'skip)
           `(not (wcheck-face-found-p
                  ',faces (wcheck-collect-faces
                           (match-beginning 1) (match-end 1)))))
          (t t))))


;;; Miscellaneous low-level functions


(defun wcheck-language-data-valid-p (key value)
  (cond ((and (eq key 'syntax)
              (syntax-table-p (and (boundp value) (eval value)))))
        ((and (eq key 'face)
              (facep value)))
        ((and (or (eq key 'regexp-start)
                  (eq key 'regexp-body)
                  (eq key 'regexp-end)
                  (eq key 'regexp-discard))
              (stringp value)))
        ((and (or (eq key 'program)
                  (eq key 'suggestion-program))
              (or (stringp value)
                  (functionp value))))
        ((and (eq key 'args)
              (or (wcheck-list-of-strings-p value)
                  ;; For backwards compatibility
                  (stringp value))))
        ((and (eq key 'suggestion-args)
              (wcheck-list-of-strings-p value)))
        ((and (or (eq key 'parser)
                  (eq key 'suggestion-parser))
              (functionp value)))
        ((or (eq key 'connection)
             (eq key 'case-fold)))
        ((and (eq key 'read-or-skip-faces)
              (wcheck-list-of-lists-p value)))))


(defun wcheck-query-language-data (language key)
  "Query `wcheck-mode' language data.
Return LANGUAGE's value for KEY as defined in variable
`wcheck-language-data'. If it does not define a (valid) value for
the KEY then query the value from `wcheck-language-data-defaults'
or `wcheck-language-data-defaults-hard-coded'."

  (when (wcheck-language-exists-p language)
    (let* ((data
            (and (wcheck-list-of-lists-p wcheck-language-data)
                 (assq key (cdr (assoc language wcheck-language-data)))))
           (default
             (and (wcheck-list-of-lists-p wcheck-language-data-defaults)
                  (assq key wcheck-language-data-defaults)))
           (hard-coded
            (and (wcheck-list-of-lists-p
                  wcheck-language-data-defaults-hard-coded)
                 (assq key wcheck-language-data-defaults-hard-coded)))
           (conf
            (list (when (wcheck-language-data-valid-p key (cdr data))
                    data)
                  (when (wcheck-language-data-valid-p key (cdr default))
                    default)
                  (when (wcheck-language-data-valid-p key (cdr hard-coded))
                    hard-coded))))

      (if (eq key 'read-or-skip-faces)
          (apply #'append (mapcar #'cdr conf))
        (cdr (assq key conf))))))


(defun wcheck-language-exists-p (language)
  "Return t if LANGUAGE exists in `wcheck-language-data'."
  (and (wcheck-list-of-lists-p wcheck-language-data)
       (member language (mapcar #'car wcheck-language-data))
       (stringp language)
       (> (length language) 0)
       t))


(defun wcheck-program-executable-p (program)
  "Return t if PROGRAM is executable regular file."
  (and (stringp program)
       (file-regular-p program)
       (file-executable-p program)
       t))


(defun wcheck-program-configured-p (language)
  (let ((program (wcheck-query-language-data language 'program)))
    (or (wcheck-program-executable-p program)
        (functionp program))))


(defun wcheck-suggestion-program-configured-p (language)
  (let ((program (wcheck-query-language-data language 'suggestion-program)))
    (or (wcheck-program-executable-p program)
        (functionp program))))


(defun wcheck-list-of-strings-p (object)
  (and (listp object)
       (not (memq nil (mapcar #'stringp object)))))


(defun wcheck-list-of-lists-p (object)
  (and (listp object)
       (not (memq nil (mapcar #'listp object)))))


(defun wcheck-process-running-p (process)
  (eq 'run (process-status process)))


(defun wcheck-current-idle-time-seconds ()
  "Return current idle time in seconds.
The returned value is a floating point number."
  (let* ((idle (or (current-idle-time)
                   '(0 0 0)))
         (high (nth 0 idle))
         (low (nth 1 idle))
         (micros (nth 2 idle)))
    (+ (* high
          (expt 2 16))
       low
       (/ micros 1000000.0))))


(defun wcheck-combine-overlapping-areas (alist)
  "Combine overlapping items in ALIST.
ALIST is a list of (A . B) items in which A and B are integers.
Each item denote a buffer position range from A to B. This
function returns a new list which has items in increasing order
according to A's and all overlapping A B ranges are combined."
  (let ((alist (sort (copy-tree alist)
                     #'(lambda (a b)
                         (< (car a) (car b)))))
        final previous)
    (while alist
      (while (not (equal previous alist))
        (setq previous alist
              alist (append (wcheck-combine-two (car previous) (cadr previous))
                            (nthcdr 2 previous))))
      (setq final (cons (car alist) final)
            alist (cdr alist)
            previous nil))
    (nreverse final)))


(defun wcheck-combine-two (a b)
  (let ((a1 (car a))
        (a2 (cdr a))
        (b1 (car b))
        (b2 (cdr b)))
    (cond ((and a b)
           (if (>= (1+ a2) b1)
               (list (cons a1 (if (> b2 a2) b2 a2)))
             (list a b)))
          ((not a) (list b))
          (t (append (list a) b)))))


;;; Overlays


(defun wcheck-make-overlay (buffer face mouse-face help-echo keymap beg end)
  "Create an overlay to mark text.
Create an overlay in BUFFER from range BEG to END. FACE,
MOUSE-FACE, HELP-ECHO and KEYMAP are overlay's properties."
  (let ((overlay (make-overlay beg end buffer)))
    (dolist (prop `((wcheck-mode . t)
                    (face . ,face)
                    (mouse-face . ,mouse-face)
                    (modification-hooks . (wcheck-remove-changed-overlay))
                    (insert-in-front-hooks . (wcheck-remove-changed-overlay))
                    (insert-behind-hooks . (wcheck-remove-changed-overlay))
                    (evaporate . t)
                    (keymap . ,keymap)
                    (help-echo . ,help-echo)))
      (overlay-put overlay (car prop) (cdr prop)))))


(defun wcheck-remove-overlays (&optional beg end)
  "Remove `wcheck-mode' overlays from current buffer.
If optional arguments BEG and END exist remove overlays from
range BEG to END. Otherwise remove all overlays."
  (remove-overlays beg end 'wcheck-mode t))


(defun wcheck-remove-changed-overlay (overlay after beg end &optional len)
  "Hook for removing overlay which is being edited."
  (unless after
    (delete-overlay overlay)))


(defun wcheck-mouse-click-overlay (event)
  "Overlay mouse-click event.
Send the mouse pointer position and mouse event to the spelling
suggestion function."
  (interactive "e")
  (wcheck-spelling-suggestions (posn-point (event-end event)) event))


;;; Buffer data access functions


(defconst wcheck-buffer-data-keys
  '(:buffer :process :language :read-req :paint-req :jump-req :areas :strings))


(defun wcheck-buffer-data-key-index (key)
  "Return the index of KEY in buffer data object."
  (let ((index 0))
    (catch 'answer
      (dolist (data-key wcheck-buffer-data-keys nil)
        (if (eq key data-key)
            (throw 'answer index)
          (setq index (1+ index)))))))


(defun wcheck-buffer-data-create (buffer)
  "Create data instance for BUFFER.
But only if it doesn't exist already."
  (unless (wcheck-buffer-data-get :buffer buffer)
    (let ((data (make-vector (length wcheck-buffer-data-keys) nil)))
      (aset data (wcheck-buffer-data-key-index :buffer) buffer)
      (push data wcheck-buffer-data))))


(defun wcheck-buffer-data-delete (buffer)
  "Delete all data associated to BUFFER."
  (let ((index (wcheck-buffer-data-key-index :buffer)))
    (setq wcheck-buffer-data
          (delq nil (mapcar (lambda (item)
                              (unless (eq buffer (aref item index))
                                item))
                            wcheck-buffer-data)))))


(defun wcheck-buffer-data-get (key value &optional target-key)
  "Query the first matching KEY VALUE pair and return TARGET-KEY.
If optional TARGET-KEY is not given return all data associated
with the matching KEY VALUE."
  (catch 'answer
    (dolist (item wcheck-buffer-data)
      (when (equal value (aref item (wcheck-buffer-data-key-index key)))
        (throw 'answer (if target-key
                           (aref item (wcheck-buffer-data-key-index
                                       target-key))
                         item))))))


(defun wcheck-buffer-data-get-all (&optional key)
  "Return every buffer's value for KEY.
If KEY is nil return all buffer's all data."
  (if key
      (let ((index (wcheck-buffer-data-key-index key)))
        (mapcar (lambda (item)
                  (aref item index))
                wcheck-buffer-data))
    wcheck-buffer-data))


(defun wcheck-buffer-data-set (buffer key value)
  "Set KEY's VALUE for BUFFER."
  (let ((item (wcheck-buffer-data-get :buffer buffer)))
    (when item
      (aset item (wcheck-buffer-data-key-index key) value))))


(defun wcheck-jump-req-create (window start bound)
  (when (and (number-or-marker-p start)
             (number-or-marker-p bound)
             (windowp window))
    (vector window start bound)))


(defun wcheck-jump-req-window (jump-req)
  (aref jump-req 0))
(defun wcheck-jump-req-start (jump-req)
  (aref jump-req 1))
(defun wcheck-jump-req-bound (jump-req)
  (aref jump-req 2))


(provide 'wcheck-mode)

;;; wcheck-mode.el ends here
