;;; tal-indent.el --- Handles indentation functions for tal-mode.

;; Copyright (C) 2001, 2004 Free Software Foundation, Inc.

;; Author: Rick Bielawski <rbielaws@i1.net>
;; Keywords: languages, extensions, Tandem, Compaq, HP, TAL, pTAL
;; Maintainer: Rick Bielawski <rbielaws@i1.net>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This file is written exclusively as an extension to tal-mode.el.  The
;; presence of this file or it's compiled version on the load path should be
;; detected by tal-mode and result in tal-mode using the routines here for
;; indent functions.

;;; Installing:

;; Before you can use tal-indent, emacs needs to be able to find it.  Place the
;; tal-indent.el file in a directory on the load-path; typically the
;; .../site-lisp or perhaps .../lisp/progmods directory.  Usually you would
;; also want to byte compile tal-indent.el but this is not required.  To do
;; this, visit the tal-indent.el file, type: M-x emacs-lisp-byte-compile
;; There should be no warnings or errors during byte compilation.
;;
;; tal-mode.el or it's compiled equivalent must also be on the load-path for
;; compilation to be successful.
;;
;; Please report any bugs!

;;; History:

;; 2005-01-15 RGB Finally useable enough to start tracking changes.
;; 2005-03-11 RGB Many bug fixes from Beta Testing.
;;                I'll just change the above date unless features change

;;; Code:

(eval-when-compile (require 'tal-mode))

;; Control variables for indenting

(defcustom tal-static-fixup-line 'tal-fixup-seq-column
  "Function to call after line is indented.  Can do things like re-align
comments in code after code position shifts.  See
`tal-trailing-seq-num-regexp'."
  :type 'function
  :group 'tal)

(defvar tal-statement-divider-regexp
  ;; Words that indicate a new statement will start afterwords or a prior
  ;; statement is complete.  The ; symbol must be searched seperately.
  (concat
   "\\(?:\\=\\|\\`\\|^\\|\\W\\)"
   (regexp-opt '("of" "while" "until" "do" "begin" "end" "->"
                 "else" "then" "otherwise") t)
   "\\(?:\\=\\|\\'\\|$\\|\\W\\)"))

(defvar tal-var-types
  (regexp-opt
   (append
    tal-keywords-unqualified-data-types
    tal-keywords-qualified-data-types
    tal-keywords-address-types)))

(defcustom tal-flower-box-regexp "!\\([#%*]\\).+\\1\\(!\\|\\s-*$\\)"
  "Use this to detect comments that are part of flower boxes.
Comments matching this regexp are treated differently according to the
function defined to handle %flower-box% indentation."
  :type 'regexp
  :group 'tal)

(defcustom tal-trailing-seq-num-regexp
  "\\(![0-9]\\{5\\}\\)\\([!-~]\\{3\\}\\)?\\s *$"
  "Identifies trailing sequence number comments such as used by ACI.
When non-nil, is used in conjunction with tal-static-fixup-line and
tal-trailing-seq-num-column.  Lines containing only a comment matching this
expression are considered blank."
  :type 'regexp
  :group 'tal)

(defcustom tal-trailing-seq-num-column 71
  "Column to which trailing sequence numbers are aligned.
See `tal-trailing-seq-num-regexp'."
  :type 'integer
  :group 'tal)

(defcustom tal-hard-comment-column -1
  "Comments starting at/prior to this column don't move.  -1 disables."
  :type 'integer
  :group 'tal)

(defcustom tal-dumb-defines-list '("wlform")
  "Be sure items in list are all LOWER case.
List of defines ending in ;#; so they are recognized as ending in ; When not
on this list the subsequent line appears to be a continuation of the line
containing the 'dumb-define'."
  :type '(repeat (string :tag "Dumb define name"))
  :group 'tal)

(defcustom tal-indent-get-prior-symbol ()
  "Variable pointing to a user function that determines indent context.
It takes 2 arguments. `tkn' and `tkn2' are strings containing the most
recent and next most recent buffer tokens respectively.  The function
must return nil or a string representing current context prior to point.
Point is looking at the 1st non-whitespace character in the line being
indented and must still be there upon return.  Match data need not be
preserved."
  :type 'function
  :group 'tal)

(defcustom tal-indent-get-looking-at-symbol ()
  "Variable pointing to a user function to determine indent context.
The function returns nil or a string representing current context.  Point
is looking at the 1st non-whitespace character in the line and must still
be there upon return.  Match data need not be preserved."
  :type 'function
  :group 'tal)

(defcustom tal-indent-determine-context-symbol ()
  "Variable pointing to a user function that determines indent context.
The function must take 3 strings as arguments.  The names of the symbols
returned by looking-at, prior, and within functions respecively are passed.
The function must return nil or a symbol representing current context.
Point will be within the line to be indented.  Returning nil means
use normal determination process."
  :type 'function
  :group 'tal)

(defvar tal-indent-symbol-prefix "tal-indent-"
  "The prefix of indent symbols used to insure uniqueness.")

;; UTILITY FUNCTIONS NOT CONNECTED TO TAL-MODE OR BUFFER CONTENTS

(defun tal-syntax-of (position &optional str) ;currently not used
;;  "Return the syntax of the char in str.  Looks up properties.
;; If str not supplied then position is in current buffer."
  (char-to-string
   (elt "-.w_()'\"$\\/<>@!|"                     ; "
        (car (let ((st (if str
                           (get-char-property position 'syntax-table str)
                         (setq str (buffer-substring position (1+ position)))
                         (get-char-property position 'syntax-table))))
               (if (consp st) st
                 (aref (or st (syntax-table)) (string-to-char str))))))))

(defun tal-next-token (&optional dont-move) ;currently not used
;;"Returns a string of all contiguous chars with the same syntax as
;;`char-after'.  If point is at `point-max' nil returns.  Point is moved
;;to the end of the token unless `dont-move' is t."
  (if (eobp)
      ()
    (looking-at (concat "\\s" (tal-syntax-of (point)) "+"))
    (if dont-move () (goto-char (match-end 0)))
    (match-string 0)))

(defun tal-string-match-mirror (regexp string &optional start)
;;   "Exactly like `string-match' only in a mirror.  The string to be
;; searched is mirrored -> derorrim si dehcraes ...  This lets you
;; match looking backward with the same expectations you have looking
;; forward.  Unfortunately you must somewhat contort your regexp.  For
;; example insted of \"\\bend\\b\" you need \"b\\endb\\\" putting slash
;; after rather than before.  Use `tal-match-string-mirror' to retrieve
;; matches.  Start is from the end of the string. \\` only works if
;; start=0"
  (let ((pxeger (concat (reverse (append regexp nil))))
        (gnirts (concat (reverse (append string nil)))))
    (string-match pxeger gnirts start)))

(defun tal-match-string-mirror (num string)
;;   "Exactly like `match-string' for use with `tal-string-match-mirror'.
;; The matching string is returned in the normal forward direction.  So
;; if you use \"\\bdne\\b\" to search for end then this returns \"end\"."
  (let* ((gnirts (concat (reverse (append string nil))))
         (hctam (match-string num gnirts)))
    (if hctam
        (concat (reverse (append hctam nil))))))

;; THESE RETURN T/NIL DEPENDING ON TEXT NEAR POINT.  POINT DOESN'T MOVE.

(defun tal-line-is-blank ()
;;   "Returns nil if current line contains any text other than a trailing
;; sequence number."
  (save-excursion
    (beginning-of-line)
    (or (looking-at "\\s *$")
        (and tal-trailing-seq-num-regexp
             (looking-at (concat "\\s *" tal-trailing-seq-num-regexp))))))

(defun tal-line-is-comment ()
;;  "Returns nil if current line contains non-comment non-blank text.
;;   If tal-line-is-blank then tal-line-is comment will also be true."
  (save-excursion
    (beginning-of-line)
    (save-match-data
      (or (looking-at "\\s-*$")
          (looking-at "\\s-*--")
          (looking-at "\\s-*![^!]*$")
          (looking-at "\\s-*\\(\\s-*!.[^!]*$\\|\\s-*!.[^!]*!\\)+\\s-*$")))))

(defun tal-line-is-directive ()
;;  "Returns nil unless `beginning-of-line' is `looking-at' \"?\""
  (save-excursion
    (beginning-of-line)
    (save-match-data
      (looking-at "?"))))

(defun tal-at-dumb-define-end ()
;;   "Detects the use of some defines whose definition includes ;#; This
;; type definition, when invoked, doesn't have a `;' terminator.  Returns
;; t if point is looking-back at `name( <params> )' and name is on
;; `tal-dumb-defines-list'.  Detecting use of this is unsupported as too
;; costly: define tkn = val;#;"
  (save-excursion
    (let* ((loc (condition-case () (scan-sexps
                                    (point) -1) (error nil))))
      (member (and loc
                   (save-excursion
                     (goto-char loc)
                     (downcase (or (tal-get-prior-token) ""))))
              tal-dumb-defines-list))))

;; THESE LOCATE TEXT AND RETURN T/NIL.  POINT IS LEFT NEAR LOCATED TEXT

(defun tal-next-code-line ()
;;   "Return t and move point to bol of first line after `point' with
;; non-comment, non-compiler directive text.  If no such line exists then nil
;; is returned and point will be at `point-max'"
  (while (and (not (eobp))
              (eq 0 (forward-line 1))
              (or (tal-line-is-directive)
                  (tal-line-is-comment) ;detects blank lines too
              )))
  (not (eobp)))

(defun tal-prior-code-line ()
;;   "Return t and move point to bol of first line prior to the line
;; containing `point' with non-comment, non-compiler directive text.  If no
;; such line exists nil is returned and point will be at `point-min'."
  (if (bobp)
      ()
    (while (and (not (bobp))
                (eq 0 (forward-line -1))
                (or (tal-line-is-directive)
                    (tal-line-is-comment) ;detects blank lines too
                )))
    (not (or (tal-line-is-directive)
             (tal-line-is-comment)))))

(defun tal-search-forward (regexp &optional bound)
;;   "Like re-search-forward but doesn't find occurances within comments
;; quotes or directive lines.  Regexp CANNOT span lines.  Point doesn't move
;; if not found.  Return nil if not found else point."
  (let ((here (point)))
    (save-excursion
      (while (and (re-search-forward regexp bound t)
                  (save-match-data
                    (or (tal-line-is-directive)
                        (not (string-match (concat regexp "\\'")
                                           (tal-code-only-backward)))
                        (not (setq here (point))))))))
    (if (eq (point) here) () (goto-char here))))

(defun tal-search-backward (regexp &optional bound)
;;   "Like re-search-backward but doesn't find occurances within comments
;; quotes or directive lines.  Regexp CANNOT span lines.  Point is looking-at
;; regexp if found else it doesn't move. Return nil if not found else t."
  (let ((here (point)))
    (save-excursion
      (while (and (re-search-backward regexp bound t)
                  (save-match-data
                    (or (tal-line-is-directive)
                        (not (string-match
                              ;; the spaces allow \Wword\W matches to work
                              regexp (concat " "
                                             (tal-code-only-forward)
                                             " ")))
                        (not (setq here (point))))))))
    (if (eq (point) here) () (goto-char here))))

; THESE RETURN TEXT NEAR POINT.  POINT MOVES IF APPLICABLE.

(defun tal-code-only-line ()
;;   "Returns current line with comments & string contents replaced by
;; spaces.  Do not count on trailing spaces creating an accurate
;; representation of line length.  Compiler directive lines are not
;; suppressed by this function."
  (let ((str (buffer-substring-no-properties (line-beginning-position)
                                             (line-end-position)))
        (offset 0)
        match
        end)                                ;loop to find comment or string start
    (while (and (< offset (length str))
                (setq offset (string-match "\\(--\\|!\\|\"\\)" str offset))) ;"
      (setq match (match-string 0 str)  ;save matching string
            end (match-end 0))          ;offset of 1st char after match
      (cond ((string= match "--")       ;truncate line shorter than end
             (setq str (substring str 0 offset)))
            ((string= match "!")        ;remove entire !comment!
             (setq str (if (not (string-match match str end))
                           (substring str 0 offset) ;truncate line
                         (setq end (match-end 0))
                         (concat (substring str 0 offset)
                                 (make-string (- end offset) ? )
                                 (substring str end)))        ;either way end points after removed text
             ))
            (t                          ;the "string" or "string cases
             (setq offset end)          ;remove only the "     " contents
             (setq str (if (not (string-match match str end))
                           (concat (substring str 0 end) "\"") ;" truncate line
                         (setq end (match-beginning 0))
                         (concat (substring str 0 offset)
                                 (make-string (- end offset) ? )
                                 (substring str end)))             ;either way end points to trailing "
             )
             (setq offset (1+ end))     ;so skip over it
            )))                                   ;end while
    str                                 ;return result
  ))

(defun tal-code-only-forward ()
;;   "Like `tal-code-only-line' but returns only the portion of the current
;; line that is after `point'.  Compiler directives are suppressed."
  (let ((offset (- (point) (line-beginning-position)))
        (str (tal-code-only-line)))
    (if (or (>= offset (length str))
            (string-match "\\`\\?" str))
        ""
      (substring str offset))))

(defun tal-code-only-backward ()
;;   "Like `tal-code-only-line' but returns only the portion of the
;; current line prior to `point'.  Compiler directives are suppressed."
  (let ((offset (- (point) (line-beginning-position)))
        (str (tal-code-only-line)))
    (cond ((string-match "\\`\\?" str)
           "")
          ((> offset (length str))
           str)
          (t
           (substring str 0 offset)))))

(defun tal-get-next-token (&optional point-prior)
;;   "Returns the next word following point as a string.  Comments, strings &
;; compiler directives are skipped.  nil means eobp reached before token found
;; in which case point is at eobp.  Otherwise point is left following the
;; found token unless point-prior is non-nil in which case it's looking-at
;; token rather than looking-back at token."
  (let ((str (tal-code-only-forward))
        (code))
    (if (and (string-match "\\`\\s *\\'" str) ;if line is blank
             (tal-next-code-line))            ;get non-blank line
      (setq str (tal-code-only-line))         ;now get text
    )
    (if (string-match "\\`\\s *\\'" str) ;blanks or empty = no more code
        (not (goto-char (point-max)))
      (if (string-match "\\`\\s +" str) ;skip leading whitespace
        (progn
          (goto-char (+ (point) (match-end 0)))
          (setq str (substring str (match-end 0)))))
      (setq code (char-syntax (string-to-char str)))
      (string-match (concat "\\s" (char-to-string code) "+") str)
      (if point-prior () (goto-char (+ (point)(match-end 0))))
      (match-string 0 str))))

(defun tal-get-prior-token (&optional point-after)
;;   "Returns the next word prior to point as a string.  Comments, string
;; contents & compiler directives are skipped.  nil means bobp reached
;; before token found.  Point is left at the start of the token or at
;; bobp if not found.  If found but point-after is non-nil point is
;; looking-back at token rather than looking-at."
  (let ((str (tal-code-only-backward))
        (code))
    (if (and (string-match "\\`\\s *\\'" str)
               (tal-prior-code-line))
      (progn
        (setq str (tal-code-only-line))
        (goto-char (+ (point) (length str)))))
    (if (string-match "\\`\\s *\\'" str)
        (progn (goto-char (point-min))
               nil)
      (if (tal-string-match-mirror "+ s\\`\\" str)
          (progn
            (goto-char (- (point) (match-end 0)))
            (setq str (substring str 0 (- (match-end 0))))))
      (setq code (char-syntax (string-to-char (substring str -1))))
      (tal-string-match-mirror (concat "+" (char-to-string code) "s\\") str)
      (if point-after () (goto-char (- (point)(match-end 0))))
      (tal-match-string-mirror 0 str))))

(defun tal-get-prior-statement-end ()
  ;; Sets point and returns the token determining prior statement end.
  ;; Where point is looking-back on the token ending the prior statement
  ;; The value returned will be ; or a keyword such as begin, then, do...
  ;; A value of `)' indicates a dumb define.  Not all can be detected!
  ;; If nil returns then this statement should be considered the first.
  (let* ((start (point))
         (limit ()))
    ;; a hard line end delimits searches always.
    (if (setq limit (tal-search-backward ";" limit))
        (goto-char start))
    ;; this is effectively a statement end and implicit define start.
    (if (setq limit (or (tal-search-backward "#," limit)
                        limit))
        (goto-char start))
    ;; this finds soft statement ends such as then, else, do...
    (when (tal-search-backward tal-statement-divider-regexp limit)
      (setq limit (match-beginning 1))
      (goto-char start))
    ;; dumb defines require jumping thru hoops.
    (if tal-dumb-defines-list
      (let ((rslt ()))
        (while (and (tal-search-backward ")" limit)
                    (save-excursion
                      (forward-char)
                      (not (setq rslt (tal-at-dumb-define-end))))))
        (if rslt
            (setq limit (point)))))
    ;; return nil & don't move unless statement end is found
    (if limit
      (progn
        (goto-char limit)
        (tal-get-next-token))
      (goto-char start)
      ())))

(defun tal-get-column-of (token-list limit)
  ;; Return the column of the start of the first token after point and
  ;; prior to limit that matches the description of a token identifier in
  ;; token-list.  Point is expected to be at the beginning of a statement.

)
;; FUNCTIONS THAT DETERMINE CURRENT CONTEXT AND FUNCTION TO HANDLE IT

(defun tal-looking-forward-context ()
  "Determines the token name identifying the current line context based
on the first token of the current line."
  (save-excursion
    (beginning-of-line)
    (skip-syntax-forward " ")
    (let (rslt)
      (cond                             ;user exit function
       ((and tal-indent-get-looking-at-symbol
             (setq rslt (funcall tal-indent-get-looking-at-symbol)))
        rslt)
       ((looking-at "$")
        "%blank-line%")
       ((looking-at "\?")
        "%compiler-dirctive%")
       ((looking-at "--")
        "%hard-comment%")
       ((looking-at "!")
        (cond
         ((and tal-trailing-seq-num-regexp
               (looking-at tal-trailing-seq-num-regexp))
          "%blank-line%")
         ((looking-at tal-flower-box-regexp)
          "%flower-box%")
         ((<= (current-column) tal-hard-comment-column)
          "%hard-comment%")
         (t
          "%soft-comment%")))
       (t
        (or (downcase (tal-get-next-token t))
            "%buffer-end%"))))))

(defun tal-looking-back-context ()
;;   "Returns a string identifying the current line's context based on the
;; most recent code line prior to point."
  (save-excursion
    (beginning-of-line)
    (let* ((pt    (point))               ;context point
           (tkn   (tal-get-prior-token)) ;last tkn of prior code line
           (tkn-pt (point))              ;context point
           (tkn2  (tal-get-prior-token)) ;token prior to tkn
           (tkn2-pt (point))             ;context point
           rslt                          ;possible/final result
          )
      (setq tkn (and tkn (downcase tkn))) ;can't downcase nil
      (setq tkn2 (and tkn2 (downcase tkn2)))
      (cond                             ;user exit function
       ((and tal-indent-get-prior-symbol
             (goto-char pt)             ;returns pt, never nil
             (setq rslt (funcall tal-indent-get-prior-symbol tkn tkn2)))
        rslt)
       ((not tkn)                       ;nothing prior = first code line
        (list "%after%-%nothing%" (point-min)))
       ;; Several special cases can follow a semicolon depending on the
       ;; statement being terminated by the semicolon
       ((string= ";" tkn)
        (cond
         ((not tkn2)                             ;first code line is nul
          (list "%after%-%nothing%" tkn-pt)    ;effectively it's first
         )
         ((string= "end" tkn2)          ;        end;
          (list "%after%-%end-semi%" tkn-pt))
         (t                             ;type can depend on prior stmt
          (if (not (tal-get-prior-statement-end))
              (goto-char (point-min))   ;prior stmt is first code line
          )
          (setq tkn2 (downcase (tal-get-next-token t)))
          (cond
           ((string= "struct" tkn2)
            (list "%after%-%struct-semi%" (point)))
           ((progn (beginning-of-line)
                   (looking-at tal-keyword-fcn-names-regexp))
            (if (string= "subproc" (match-string 1))
                (list "%after%-%subproc-semi%" (match-beginning 1))
              (list "%after%-%proc-semi%" (match-beginning 1))))
           (t (list "%after%-%semi%" tkn-pt))))))
       ((string= "#;" tkn)              ;end of define is not distinct
        (list "%after%-%semi%" tkn-pt))
       ((string= ";#;" tkn)             ;dumb define may need processing
        (list "%after%-%dumb-define-end%" tkn-pt))
       ((string= "," tkn)               ;comma used for params
        (list "%after%-%comma%" tkn-pt) ;and variable declarations.
       )
       ((string= "#," tkn)              ;define terminator
        (list "%after%-%comma-define-end%" tkn-pt))
       ((string= ";#," tkn)             ;dumb define may need processing
        (list "%after%-%dumb-define-next%" tkn-pt))
       ((string-match ")\\'" tkn)
        (if (and tal-dumb-defines-list  ;dumb defines have an implied ;
                 (save-excursion
                   (goto-char (1+ tkn-pt))
                   (tal-at-dumb-define-end)))
            (list "%after%-%semi%" tkn-pt) ;so say we saw the ; hidden inside
          (list (concat "%after%-" tkn) tkn-pt)))
       ((string-match tal-statement-divider-regexp tkn)
        (goto-char tkn-pt)
        (beginning-of-line)
        (if (string= tkn (downcase (tal-get-next-token t)))
            (list "%after%-%hard-divider%" (point))
          (list "%after%-%soft-divider%" tkn-pt)))
       (t
        (list (concat "%after%-" tkn) tkn-pt))))))

(defun tal-within-context ()
;;  "Return a string identifying the context of the current sexp."
  (save-excursion
    (beginning-of-line)
    ;; This was added so skeletons would indent properly
    (if font-lock-mode
        (font-lock-fontify-syntactic-keywords-region
         (point) (line-end-position)))
    (let* ((loc (condition-case ()
                    (scan-lists (point) -1 1)
                  (error nil))))
      (if (null loc)
          '("%within%-%none%" nil)
        (goto-char loc)
        (cond
         ((tal-line-is-directive)
          `("%within%-%directive-paren%" ,loc))
         ((looking-at (concat "[[(]\\w*\\($\\|"
                              tal-trailing-seq-num-regexp
                                 "\\)"))
          `("%within%-%paren-eol%" ,loc))
         ((looking-at "(")
          `("%within%-%paren%" ,loc))
         (t                             ;begin and define expected
          `(,(concat "%within%-" (tal-get-next-token)) ,loc)))))))

(defun tal-get-context-list ()
;;   "Returns a list whose car is the symbol to use as context for indenting.
;; The list also includes the strings indicating forward and backward tokens
;; used to determine the context of point, the token determining current paren
;; level and it's location."
  (let* ((within-context (tal-within-context))           ;
         (within-sym (car within-context))               ;a
         (forward-context (tal-looking-forward-context)) ;
         (forward-sym forward-context)                   ;b
         (backward-context (tal-looking-back-context))   ;
         (backward-sym (car backward-context))           ;c
         rslt)
    (cond
     ;; each test actually calculates a result if the result is non-nil no
     ;; further conditions are checked and so no further calculations are made.
     ((and tal-indent-determine-context-symbol ;call user exit if it exists
           (setq rslt (funcall tal-indent-determine-context-symbol
                               forward-context
                               backward-context
                               within-context))))
     ((and (string= "%compiler-dirctive%" forward-sym)
           (setq rslt (intern-soft      ;catch directive within paren before
                       (concat tal-indent-symbol-prefix ;within-paren test
                               forward-sym)))
           (symbol-plist rslt)))
     ((and (setq rslt (intern-soft
                       (concat tal-indent-symbol-prefix
                               within-sym "-" forward-sym "-" backward-sym)))
           (symbol-plist rslt)))
     ((and (setq rslt (intern-soft
                       (concat tal-indent-symbol-prefix
                               within-sym "-" forward-sym)))
           (symbol-plist rslt)))
     ((and (setq rslt (intern-soft
                       (concat tal-indent-symbol-prefix
                               within-sym "-" backward-sym)))
           (symbol-plist rslt)))
     ((and (setq rslt (intern-soft
                       (concat tal-indent-symbol-prefix
                               forward-sym "-" backward-sym)))
           (symbol-plist rslt)))
     ((and (setq rslt (intern-soft
                       (concat tal-indent-symbol-prefix
                               within-sym)))
           (symbol-plist rslt)))
     ((and (setq rslt (intern-soft
                       (concat tal-indent-symbol-prefix
                               forward-sym)))
           (symbol-plist rslt)))
     ((and (setq rslt (intern-soft
                       (concat tal-indent-symbol-prefix
                               backward-sym)))
           (symbol-plist rslt)))
     ((and (setq rslt (intern-soft      ;otherwise
                       (concat tal-indent-symbol-prefix
                               "%amid-statement%")))
           (symbol-plist rslt)))
     (t                                 ;this shouldn't happen
      (setq rslt ())                    ;but it's legal if it does!
     ))
    (list rslt forward-context backward-context within-context)))

(defun tal-show-indent-context ()
  "Displays the indent context tokens applicable to line containing `point'"
  (interactive)
  (let* ((context  (tal-get-context-list))
         (symbol   (car context))
         (fixup    (get symbol 'fixup))
         (function (get symbol 'relative-to))
         (offset   (get symbol 'offset))
         (column   (+ (or (and function ;function to return base column
                               (apply function context))
                          (get symbol 'static) ;else look for static base
                          0                    ;use bol if neither above
                      )
                      (or offset 0)     ;if offset to base exists add it
                   )))
    (message "SYM=%s FCN=%s OFF=%s COL=%s" context function offset column))) ;(global-set-key [C-S-up] 'tal-show-indent-context)

(defun tal-get-proper-indent-column ()
  "Returns the indent column applicable to line containing `point'"
  (interactive)
  (let* ((context  (tal-get-context-list))
         (symbol   (car context))
         (function (get symbol 'relative-to))
         (offset   (get symbol 'offset)))
    (+ (or (and function ;function to return base column
                (apply function context))
           (get symbol 'static) ;else look for static base
           0                    ;use bol if neither above
       )
       (or offset 0)     ;if offset to base exists add it
    )))

(defun tal-indent-line ()
  (interactive)
  "Indents the currrent line & does optional fixup."
  (let* ((context  (tal-get-context-list))
         (symbol   (car context))
         (fixup    (get symbol 'fixup))
         (function (get symbol 'relative-to))
         (offset   (get symbol 'offset))
         (column   (+ (or (and function ;function to return base column
                               (apply function context))
                          (get symbol 'static) ;else look for static base
                          0                    ;use bol if neither above
                      )
                      (or offset 0)     ;if offset to base exists add it
                   ))
         (old-mark (point-marker))      ;usually I don't want to move pt
         (old-col (current-column)))
    (back-to-indentation)
    (if (>= (current-column) old-col)   ;if point <= current indent
        (setq old-mark nil))            ;point goes to indentation
    (if (= column (current-column))     ;if indent is already correct
            (if old-mark(goto-char old-mark)) 
      (delete-region (line-beginning-position)(point))
      (if (> column 0) (indent-to column))
      (if old-mark (goto-char old-mark)))
    (or (and fixup
             (funcall fixup))
        (and tal-static-fixup-line
             (funcall tal-static-fixup-line)))));(global-set-key [C-S-up] 'tal-indent-line)

;;; INDENTATION ROUTINES START HERE

(defun tal-fixup-seq-column ()
  (interactive)
  (save-excursion
    (if tal-trailing-seq-num-regexp
        (progn
          (end-of-line)
          (if (looking-back tal-trailing-seq-num-regexp)
              (progn
                (goto-char (match-beginning 1))
                (let ((to-col tal-trailing-seq-num-column)
                      (offset (- tal-trailing-seq-num-column (current-column)))
                      (overwrite-mode nil)  ;needed for backward-delete-char
                     )
                  (cond ((> offset 0)
                         (if (eq (preceding-char) ?!)
                           (backward-char))
                         (insert (make-string offset ? )))
                        ((< offset 0)
                         ;; more trickey: only get rid of whitespace
                         (when (eq (preceding-char) ?!)
                           (setq to-col (1- to-col))
                           (backward-char))
                         (while (and (looking-back "\\s ")
                                     (> (current-column) to-col))
                           (backward-delete-char-untabify 1)))))))))))

(defun tal-fixup-compiler-directive () "Do nothing for now."
  (tal-fixup-seq-column))

(defmacro tal-create-indent-entity (entity &rest rest)
  "entity is a string; the name to declare. rest is it's plist."
  `(setplist ',(intern (concat tal-indent-symbol-prefix entity)) ',rest))
;; Only the above one is used internally, below are for customer use.
(defmacro tal-set-indent-entity-offset (entity offset)
  "entity must be a string, offset a number"
  `(put ',(intern (concat tal-indent-symbol-prefix entity)) 'offset ,offset))

(defmacro tal-set-indent-entity-relative-to (entity func)
  "entity must be a string, func a function returning an offset"
  `(put ',(intern (concat tal-indent-symbol-prefix entity)) 'relative-to ,func))

(defmacro tal-set-indent-entity-fixup (entity func)
  "entity must be a string, func a function to fix the current line after indent"
  `(put ',(intern (concat tal-indent-symbol-prefix entity)) 'fixup ,func))
;;  use this to get better indenting and viewing of the following code
;; (put 'tal-create-indent-entity 'lisp-indent-function 'defun)
;; (nconc lisp-font-lock-keywords-2
;;        '(("\\(tal-create-indent-entity\\)\\>[ 	]+\\(\"[^\"]+\"\\)"
;;           (1 font-lock-keyword-face)
;;           (2 font-lock-function-name-face 'append) ;; this can't work :-<
;;          ))
;; )
;; (emacs-lisp-mode)

(tal-create-indent-entity       "%after%-begin"
  relative-to   tal-indent-within-begin
  offset        +2                      ; should complement %after%-end
)

(tal-create-indent-entity       "%after%-end"
  relative-to   tal-indent-within-begin
  offset        -2                      ; should complement %after%-begin
)

(tal-create-indent-entity      "%after%-%comma%" ;variable declarations only
  relative-to   tal-indent-csv                   ;see %within%-%paren%
)

(tal-create-indent-entity      "%after%-\"," ;variable declarations only "
  relative-to   tal-indent-csv                   ;see %within%-%paren%
)

(tal-create-indent-entity       "%after%-%comma-define-end%"
  relative-to   tal-indent-define-keyword)
;; "%after%-%dumb-define-next%" when I write support for -> ;#,
(tal-create-indent-entity       "%after%-%end-semi%"
  relative-to   tal-indent-within-begin
  offset        +2)

(tal-create-indent-entity       "%after%-%hard-divider%"
  relative-to   tal-indent-prior-token-line
  offset        +2)

(tal-create-indent-entity       "%after%-%nothing%"
  static         0                      ; by definition, can't be relative
)
;; "%after%-%proc-semi%" possibly useful for adding eldoc entries?
(tal-create-indent-entity       "%after%-%proc-semi%"
  relative-to   tal-indent-within-begin)

(tal-create-indent-entity       "%after%-%semi%"
  relative-to   tal-indent-within-begin
  offset        +2)

(tal-create-indent-entity       "%after%-%soft-divider%"
  relative-to   tal-indent-prior-token-statement-start
  offset        +2)
;; "%after%-%subproc-semi%" possibly useful for adding eldoc entries?
(tal-create-indent-entity       "%amid-statement%"
  relative-to   tal-indent-amid-statement)

(tal-create-indent-entity       "begin-%after%-%struct-semi%"
  relative-to   tal-indent-prior-statement-start
  offset        +2)

(tal-create-indent-entity       "%after%-%struct-semi%"
  relative-to   tal-indent-within-begin
  offset        +2)

(tal-create-indent-entity       "%compiler-dirctive%"
  static        0                            ; language requirement.
  fixup         tal-fixup-compiler-directive ; does directive alignment
)

(tal-create-indent-entity       "%flower-box%"
  relative-to   tal-indent-prior-flower-box ; prior comment or dont move
)

(tal-create-indent-entity      "%hard-comment%"
  relative-to   tal-current-indentation ; use when line should not move
)

(tal-create-indent-entity       "%soft-comment%"
  relative-to   tal-indent-code-comment ; first non-blank line prior
)

(tal-create-indent-entity       "%within%-block-%after%-%semi%"
  static        0)

(tal-create-indent-entity       "%within%-block-%after%-%end-semi%"
  static        0)

(tal-create-indent-entity       "%within%-define-begin"
  relative-to   tal-indent-begin-within-define)

(tal-create-indent-entity       "%within%-%directive-paren%"
  static        0)

(tal-create-indent-entity       "%within%-%none%-%after%-%semi%"
  static        0)

(tal-create-indent-entity       "%within%-%none%-%after%-%end-semi%"
  relative-to   tal-indent-within-begin
  static        0)

(tal-create-indent-entity       "%within%-%none%-%after%-%struct-semi%"
  relative-to   tal-indent-within-begin
  static        0)

(tal-create-indent-entity       "%within%-%none%-begin"
  static        0)

(tal-create-indent-entity       "%within%-%none%-begin-%after%-%struct-semi%"
  relative-to   tal-indent-prior-statement-start
  offset        +2)

(tal-create-indent-entity       "%within%-%paren%"
  relative-to   tal-indent-1st-tkn-in-paren)

(tal-create-indent-entity       "%within%-%paren-eol%"
  relative-to   tal-indent-within-begin
  offset        +8)

(tal-create-indent-entity       "end"
  relative-to   tal-indent-within-begin
  offset        +0)

(tal-create-indent-entity       "begin"
  relative-to   tal-indent-within-begin
  offset        +4)

(tal-create-indent-entity       "else"
  relative-to   tal-indent-current-statement-start
  offset        -2)

(tal-create-indent-entity       "then"
  relative-to   tal-indent-current-statement-start
  offset        +0)

(tal-create-indent-entity       "do"
  relative-to   tal-indent-current-statement-start
  offset        +0)

;;; THESE ROUTINES ACTUALLY FIND THE OFFSET OF A SPECIFIC ENTITY

(defun tal-current-indentation (&rest context)
  "Returns the existing indentation of the line being indented."
  ;; wraper to toss automated arguments
  (current-indentation))

(defun tal-indent-1st-tkn-in-paren (&rest context)
  "Returns the column of the first item in the paren enclosed list we're within.
If the first item isn't in the same line as the open paren then call the
%within%-%paren-eol% defined function to return a value.  If none exists,
return the column of the paren character itself."
  (let* ((in-loc (nth 1 (nth 3 context))) ;within loc
         (symbol (intern-soft (concat tal-indent-symbol-prefix
                                      "%within%-%paren-eol%")))
         (fcn (if symbol (get symbol 'relative-to))))
    (save-excursion
      (goto-char in-loc)
      (tal-get-next-token)              ;consume paren token
      (if (looking-at (concat "\\s-*\\($\\|" tal-trailing-seq-num-regexp
                              "\\)"))   ;nothing after paren
          (if fcn                       ;if fcn defined for this
              (function fcn)            ;special case, use it
            in-loc                      ;else column of [( returns
          )
        (skip-chars-forward " \t")      ;start of 1st item even a comment
        (current-column)                ;return indent
      ))))

(defun tal-indent-current-statement-start (&rest context)
  "Returns the indent column of the beginning of the current statement."
  (save-excursion
    (beginning-of-line)
    (if (not (tal-get-prior-statement-end)) ;points to then^ or end;^
        (goto-char (point-min)))
    (tal-get-next-token t)
    (current-indentation)))

(defun tal-indent-define-keyword (&rest context)
  "Returns the indent column of the first identifier in a define.
That is, the first keyword following the word `define'.  If not within a
define declaration an error occurs."
  (let* ((within (nth 3 context))
         (keyword (car within))
         (loc     (nth 1 within)))
    (if (not (string= "%within%-define" keyword))
        (error "tal-indent-define-keyword called but not within define."))
    (save-excursion
      (goto-char loc)                   ;points to ^define
      (tal-get-next-token)              ;now at define^
      (tal-get-next-token t)            ;now at define   ^keyword
      (current-column)                  ;return column   ^
    )))

(defun tal-indent-within-begin (&rest context)
  "Returns indent column of line containing encompassing begin/end.
If none, return 0."
  (let* ((within (nth 3 context))
         (in-tkn (car within))
         (in-loc (nth 1 within)))
    (save-excursion
      (if (string= "%within%-begin" in-tkn)
          (goto-char in-loc)
        (beginning-of-line)
        (while
            (and (setq in-loc (condition-case ()
                                  (scan-lists (point) -1 1)
                                (scan-error nil)))
                 (goto-char in-loc)
                 (not (looking-at "begin")))))
      (if (null in-loc)
          0
        (current-indentation)))))

(defun tal-indent-code-comment (&rest context)
  "Returns the indent column of the prior line unless it's blank.
Otherwise the blank line's indentation is used.  In both cases trailing
sequence numbers are ignored if present."
  (save-excursion
    (if (/= 0 (forward-line -1))  ;see if this is the first line
        0                               ;return 0 if no prior context
      (while (and (tal-line-is-directive) ;skip any directive lines
                  (forward-line -1)))
      (if (tal-line-is-directive)
          0                      ;return 0 if all prior are directive
        (if (tal-line-is-blank)
            (tal-get-proper-indent-column) ;see how code should indent
          (current-indentation))))))

(defun tal-indent-prior-token-line (&rest context)
  "Returns the indent column of most recent prior non-blank code line.
If none, return 0. "
  (save-excursion
    (beginning-of-line)
    (tal-get-prior-token)               ;the tkn causing indent 
    (current-indentation)))

(defun tal-indent-prior-token-statement-start (&rest context)
  "Returns the indent column of start of the statement in the most recent
prior non-blank code line.  If none, return 0. "
  (save-excursion
    (beginning-of-line)
    (tal-get-prior-token)               ;the tkn causing indent 
    (beginning-of-line)
    (if (not (tal-get-prior-statement-end)) ;points to then^ or end;^
        (goto-char (point-min)))
    (tal-get-next-token t)
    (current-indentation)))

(defun tal-indent-prior-statement-start (&rest context)
  "Returns the indent column of the beginning of the prior statement.
If none, return 0."
  (save-excursion
    (beginning-of-line)
    (if (not (tal-get-prior-statement-end))
        0                             ;no prior statement
      (tal-get-prior-token)           ;move before stmt end
      (if (and (looking-at ";")(looking-back "end"))
          (tal-get-prior-token)       ;or it will be considered prior end
      )
      (if (not (tal-get-prior-statement-end))
          (goto-char (point-min))     ;next most prior statement
      )
      (tal-get-next-token t)          ;1st token of  statement
      (current-indentation))))

(defun tal-indent-amid-statement (&rest this-context)
  ;; context is everything returned by tal-get-context-list
  "Returns the indent column appropriate type of continuation."
  (let ((my-sym (car this-context))    ;sym causing this fcn to be called
        (fw-sym (nth 1 this-context))  ;1st sym in line
        (bk-sym (car (nth 2 this-context)))
        context)
    ;; If a continuation of a continuation I want to return the same
    ;; indentation as the prior line.  But sometimes even that isn't right
    ;; such as variable declarations that have leading qualifiers.  I don't
    ;; handle that yet!  Or when prior line is an array declaration
    ;; continuation line.  I.E. does not have the indentation appropriate for
    ;; this continuation.
    (save-excursion
      (tal-prior-code-line)             ;end of line prior to current
      (setq context (car (tal-looking-back-context)))
      (cond
       ((or (string-match "semi%" context) ;prior line is start of statement
            (string-match "divider%" context))
        (progn                         ;determine the type of statement
          (back-to-indentation)
          (cond
           ((looking-at tal-var-types) ;a variable declaration
            (looking-at "[a-z-A-z]+\\(([-a-zA-Z0-9])\\)?\\s-+")
            (goto-char (match-end 0))
            (current-column))
           ;; some other kind of statement
           (t (+ 4 (current-indentation))))))
       (t (current-indentation))     ;secondary continuation same as prior
      ))))

;;; THESE ROUTINES RETURN INDENTATION FOR SPECIFIC SITUATIONS

(defun tal-indent-begin-within-define (&rest context)
  (let* ((my-sym (car context))
         (offset (get my-sym 'offset))
         (in-loc (nth 1 (nth 3 context))) ;loc of define keyword
         loc                              ;loc of symbol being defined
        )
    (save-excursion
      (beginning-of-line)
      (unless (tal-search-backward "#," in-loc)
        (goto-char in-loc))
      (if (looking-at "\\(define\\b\\|#,\\)")
          (tal-get-next-token)          ;consume define start
      )
      (tal-get-next-token t)            ;start of symbol being defined
      (setq loc (current-column))       ;save for case of = <eol>
      (tal-get-next-token)              ;consume token begin defined
      (if (string= "(" (tal-get-next-token t)) ;don't consume ( or =
          (forward-sexp)                       ;skip define arguments
      )
      (if (string= "=" (tal-get-next-token)) ;consume =
          (if (string-match "^\\s-*$" (tal-code-only-forward))
              loc                       ;definition starts on new line
            (tal-get-next-token t)      ;first word of define code
            (- (current-column) (or offset 0)) ;suppress offset
          )
        0                               ;syntax error, no =
      ))))

(defun tal-indent-csv (&rest context)
  "Returns the indent column of the first identifier in the current statement.
This is intended for use in variable declaration lines."
;(tal-indent-%after%-%comma% %blank-line% (%after%-%comma% 6553) (%within%-block 2403))
  (let ((my-sym (car context))          ;sym that caused this fcn to be called
        (fw-sym (nth 1 context))        ;looking-forward symbol
        (bk-loc (nth 1 (nth 2 context))) ;bol of line being indented
        (offset 0))
    (save-excursion
      (beginning-of-line)
      (if (not (tal-get-prior-statement-end)) ;point follows prior stmt end
        (goto-char (point-min)))
      (tal-get-next-token t)            ;points to 1st word of my stmt
      (when (or (looking-at (tal-keyword-qualified-regexp
                             tal-keywords-qualified-data-types))
                (looking-at (tal-keyword-anywhere-regexp
                             tal-keywords-unqualified-data-types)))
        (goto-char (1- (match-end 0)))  ;point is after ex: int(32)
        ;; see if a prefix exists requiring an offset
        (if (not (string= "%blank-line%" fw-sym ))
            (save-excursion
              (goto-char bk-loc)        ;points to comma
              (tal-get-next-token)      ;consume comma
              (tal-get-next-token t)    ;looking at current var declaration
              (if (looking-at "\\(\\.ext\\s-\\|\\.\\)")
                  (setq offset (- (point) (match-end 0)))))                           ;go back to initial declaration
        )
        (tal-get-next-token t)
        (if (looking-at "\\(\\.ext\\s-\\|\\.\\)") ;1st dec has attributes?
            (goto-char (+ offset (match-end 0)))  ;skip them & apply offset
          (forward-char offset)                   ;else just apply offset
        ))
      (current-column))))

(defun tal-indent-prior-flower-box (&rest context)
  "Returns the indent of the prior line, only if it's also a flower-box comment.
Otherwise return the current indent which should result in no indent change."
  (or (save-excursion
        (if (and (eq 0 (forward-line -1))
                 (skip-syntax-forward " ")
                 (looking-at tal-flower-box-regexp))
            (current-indentation)))
      (current-indentation)))

(provide 'tal-indent)

;;; tal-indent.el ends here.
