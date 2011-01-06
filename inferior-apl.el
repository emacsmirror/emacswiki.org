;;; Basic Apl (Aplus) in a buffer functionality
;;; copyright Rusi Mody

;;; Font lock and other fancy functionality currently incomplete
;;; Uses Markus Triska's apl.el for encodings, input-methods etc
;;; Make sure apl.el is on emacs' load path and a+ on execution path

;;; Run with M-x run-apl


  ;; where apl.el is Set this to your path
(add-to-list 'load-path "~/path/to/apl.el")
(require 'apl)
(require 'comint)

(defun apl-unicode2aplus-char (c)
"Converts unicode char to an Aplus single char string
All bets off when its not convertile :-)"
  (let ((c2 (rassq c apl-aplus-table)))
    (string (if c2 (car c2) c))))

(defun apl-unicode2aplus-str (s)
"Converts a unicode string to an Aplus string"
  (mapconcat (function apl-unicode2aplus-char) s ""))

(defun apl-comint-send (process s)
"`comint-simple-send' with string preprocessed with `apl-unicode2aplus-str'"
  (comint-simple-send process (apl-unicode2aplus-str s)))

;;replace comint-simple-send
(setq comint-input-sender 'apl-comint-send)

(defun apl-aplus2unicode-char (c)
"Converts Aplus char to a unicode single char string
All bets off when its not convertile :-)"
  (let ((c2 (assq c apl-aplus-table)))
    (string (if c2 (cdr c2) c))))

(defun apl-aplus2unicode-str (s)
"Converts a Aplus string to a unicode string"
  (mapconcat (function apl-aplus2unicode-char) s ""))

(add-hook 'comint-preoutput-filter-functions 'apl-aplus2unicode-str)

(defun run-apl()
  "Major mode for running APlus under emacs"
  (interactive)
  (if (not (comint-check-proc "*a+*"))
      (set-buffer (make-comint "a+" "a+")))
  (setq apl-buffer "*a+*")
  (inferior-apl-mode)
  (pop-to-buffer "*a+*"))


(font-lock-add-keywords 'inferior-apl-mode
    '(("\x235d.*"     0 font-lock-comment-face prepend)
      ("[^\\]\"\\([^\"\\]*\\|\\.\\)\"" 0 font-lock-string-face  prepend)
 ;    ("put ho operators here"  font-lock-builtin-face prepend)
 ;    ("ordinary operators here"  font-lock-variable-name-face prepend) ;; (FIXME: this is mixed up)
       ))

(custom-set-faces
 '(comint-highlight-input ((t (:foreground "DarkSeaGreen4"  :height 90))))
)


;;; Below hacked from scheme.el and very incomplete
(defvar apl-mode-abbrev-table nil)
(defvar apl-mode-syntax-table
  (let ((st (make-syntax-table))
	(i 0)
	(comment-char ?\x235d)
	)

    ;; Default is atom-constituent.
    (while (< i 256)
      (modify-syntax-entry i "_   " st)
      (setq i (1+ i)))

    ;; Word components.
    (setq i ?0)
    (while (<= i ?9)
      (modify-syntax-entry i "w   " st)
      (setq i (1+ i)))
    (setq i ?A)
    (while (<= i ?Z)
      (modify-syntax-entry i "w   " st)
      (setq i (1+ i)))
    (setq i ?a)
    (while (<= i ?z)
      (modify-syntax-entry i "w   " st)
      (setq i (1+ i)))

    ;; Whitespace
    (modify-syntax-entry ?\t "    " st)
    (modify-syntax-entry ?\n ">   " st)
    (modify-syntax-entry ?\f "    " st)
    (modify-syntax-entry ?\r "    " st)
    (modify-syntax-entry ?\s "    " st)

    ;; These characters are delimiters but otherwise undefined.
    ;; Brackets and braces balance for editing convenience.
    (modify-syntax-entry ?\[ "(]  " st)
    (modify-syntax-entry ?\] ")[  " st)
    (modify-syntax-entry ?{ "(}  " st)
    (modify-syntax-entry ?} "){  " st)
    (modify-syntax-entry ?\| "\" 23bn" st)
    ;; Guile allows #! ... !# comments.
    ;; But SRFI-22 defines the comment as #!...\n instead.
    ;; Also Guile says that the !# should be on a line of its own.
    ;; It's too difficult to get it right, for too little benefit.
    ;; (modify-syntax-entry ?! "_ 2" st)

    ;; Other atom delimiters
    (modify-syntax-entry ?\( "()  " st)
    (modify-syntax-entry ?\) ")(  " st)
    ;; It's used for single-line comments as well as for #;(...) sexp-comments.
;    (modify-syntax-entry comment-char "< 2 " st)
    (modify-syntax-entry comment-char "<" st)
    (modify-syntax-entry ?\" "\"   " st)
    (modify-syntax-entry ?' "'   " st)
    (modify-syntax-entry ?` "'   " st)
;    (modify-syntax-entry comment-char "'   " st)

    ;; Special characters
    (modify-syntax-entry ?, "'   " st)
    (modify-syntax-entry ?@ "'   " st)
    (modify-syntax-entry ?# "' 14b" st)
    (modify-syntax-entry ?\\ "\\   " st)
    st))

(define-derived-mode inferior-apl-mode comint-mode "Inferior APL"
  "Need to rewrite for apl :-)

Major mode for interacting with an inferior Scheme process.

The following commands are available:
\\{inferior-scheme-mode-map}

A Scheme process can be fired up with M-x run-scheme.

Customization: Entry to this mode runs the hooks on comint-mode-hook and
inferior-scheme-mode-hook (in that order).

You can send text to the inferior Scheme process from other buffers containing
Scheme source.
    switch-to-scheme switches the current buffer to the Scheme process buffer.
    scheme-send-definition sends the current definition to the Scheme process.
    scheme-compile-definition compiles the current definition.
    scheme-send-region sends the current region to the Scheme process.
    scheme-compile-region compiles the current region.

    scheme-send-definition-and-go, scheme-compile-definition-and-go,
        scheme-send-region-and-go, and scheme-compile-region-and-go
        switch to the Scheme process buffer after sending their text.
For information on running multiple processes in multiple buffers, see
documentation for variable scheme-buffer.

Commands:
Return after the end of the process' output sends the text from the
    end of process to point.
Return before the end of the process' output copies the sexp ending at point
    to the end of the process' output, and sends it.
Delete converts tabs to spaces as it moves back.
Tab indents for Scheme; with argument, shifts rest
    of expression rigidly with the current line.
C-M-q does Tab on each line starting within following expression.
Paragraphs are separated only by blank lines.  Semicolons start comments.
If you accidentally suspend your process, use \\[comint-continue-subjob]
to continue it."
  ;; Customize in inferior-scheme-mode-hook
  (setq comint-prompt-regexp "^[^>\n]*>+ *") ; OK for cscheme, oaklisp, T,...
  (apl-mode-variables)
  (setq mode-line-process '(":%s"))
 ; (setq comint-input-filter (function scheme-input-filter))
 ; (setq comint-get-old-input (function scheme-get-old-input))
)

(defun apl-mode-variables ()
  (set-syntax-table apl-mode-syntax-table)
  (setq local-abbrev-table apl-mode-abbrev-table)
  (make-local-variable 'comment-start)
;  (setq comment-start ";")
  (setq comment-start ?\x235d)
  (make-local-variable 'comment-column)
  (setq comment-column 40)
  (set-input-method "apl-ascii"))

