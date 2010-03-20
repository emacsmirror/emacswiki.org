;;; csharp-mode.el --- C# mode derived mode

;; Author:     Dylan R. E. Moonfire
;; Maintainer: Dylan R. E. Moonfire <contact@mfgames.com>
;; Created:    Feburary 2005
;; Modified:   February 2010
;; Version:    0.7.2  - Dino Chiesa <dpchiesa@hotmail.com>
;; Keywords:   c# languages oop

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;;    This is a separate mode to implement the C# constructs and
;;    font-locking. It is based on the java-mode example from cc-mode.
;;
;;    Note: The interface used in this file requires CC Mode 5.30 or
;;    later.

;;; Bugs:
;;
;;   Literal strings @"" do not fontify correctly.
;;
;;   Method names are not fontified if you have an attribute before it.
;;
;;   This code doesn't seem to work when you compile it, then
;;   load/require in the emacs file. You will get an error (error
;;   "`c-lang-defconst' must be used in a file") which happens because
;;   cc-mode doesn't think it is in a buffer while loading directly
;;   from the init. However, if you call it based on a file extension,
;;   it works properly. Interestingly enough, this doesn't happen if
;;   you don't byte-compile cc-mode.

;;; .emacs (don't put in (require 'csharp-mode))
;; (autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
;; (setq auto-mode-alist
;;    (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))

;;; Versions:
;;
;;    0.1.0 - Initial release.
;;    0.2.0 - Fixed the identification on the "enum" keyword.
;;          - Fixed the font-lock on the "base" keyword
;;    0.3.0 - Added a regex to fontify attributes. It isn't the
;;            the best method, but it handles single-like attributes
;;            well.
;;          - Got "super" not to fontify as a keyword.
;;          - Got extending classes and interfaces to fontify as something.
;;    0.4.0 - Removed the attribute matching because it broke more than
;;            it fixed.
;;          - Corrected a bug with namespace not being properly identified
;;            and treating the class level as an inner object, which screwed
;;            up formatting.
;;          - Added "partial" to the keywords.
;;    0.5.0 - Found bugs with compiled cc-mode and loading from init files.
;;          - Updated the eval-when-compile to code to let the mode be
;;            compiled.
;;    0.6.0 - Added the c-filter-ops patch for 5.31.1 which made that
;;            function in cc-langs.el unavailable.
;;          - Added a csharp-lineup-region for indention #region and
;;            #endregion block differently.
;;    0.7.0 - Added autoload so update-directory-autoloads words
;;            (Thank you, Nikolaj Schumacher)
;;          - Fontified the entire #region and #endregion lines.
;;          - Initial work to get get, set, add, remove font-locked.
;;    0.7.1 - Added option to indent #if/endif with code
;;          - Fixed c-opt-cpp-prefix defn (it must not include the BOL
;;            char (^).
;;          - proper fontification and indent of classes that inherit
;;            (previously the colon was confusing the parser)
;;          - reclassified namespace as a block beginner
;;          - removed $ as a legal symbol char - not legal in C#.
;;          - added struct to c-class-decl-kwds so indent is correct
;;            within a struct.
;;    0.7.2 - Added automatic codedoc insertion.


(message  (concat "Loading " load-file-name))


;; This is a copy of the function in cc-mode which is used to handle
;; the eval-when-compile which is needed during other times.
(defun c-filter-ops (ops opgroup-filter op-filter &optional xlate)
  ;; See cc-langs.el, a direct copy.
  (unless (listp (car-safe ops))
    (setq ops (list ops)))
  (cond ((eq opgroup-filter t)
         (setq opgroup-filter (lambda (opgroup) t)))
        ((not (functionp opgroup-filter))
         (setq opgroup-filter `(lambda (opgroup)
                                 (memq opgroup ',opgroup-filter)))))
  (cond ((eq op-filter t)
         (setq op-filter (lambda (op) t)))
        ((stringp op-filter)
         (setq op-filter `(lambda (op)
                            (string-match ,op-filter op)))))
  (unless xlate
    (setq xlate 'identity))
  (c-with-syntax-table (c-lang-const c-mode-syntax-table)
    (delete-duplicates
     (mapcan (lambda (opgroup)
               (when (if (symbolp (car opgroup))
                         (when (funcall opgroup-filter (car opgroup))
                           (setq opgroup (cdr opgroup))
                           t)
                       t)
                 (mapcan (lambda (op)
                           (when (funcall op-filter op)
                             (let ((res (funcall xlate op)))
                               (if (listp res) res (list res)))))
                         opgroup)))
             ops)
     :test 'equal)))

;; This inserts the bulk of the code.
(require 'cc-mode)

;; These are only required at compile time to get the sources for the
;; language constants.  (The cc-fonts require and the font-lock
;; related constants could additionally be put inside an
;; (eval-after-load "font-lock" ...) but then some trickery is
;; necessary to get them compiled.)
(eval-when-compile
  (let ((load-path
         (if (and (boundp 'byte-compile-dest-file)
                  (stringp byte-compile-dest-file))
             (cons (file-name-directory byte-compile-dest-file) load-path)
           load-path)))
    (load "cc-mode" nil t)
    (load "cc-fonts" nil t)
    (load "cc-langs" nil t)))

(eval-and-compile
  ;; Make our mode known to the language constant system.  Use Java
  ;; mode as the fallback for the constants we don't change here.
  ;; This needs to be done also at compile time since the language
  ;; constants are evaluated then.
  (c-add-language 'csharp-mode 'java-mode))

;; Indention: csharp-mode follows normal indention rules except for
;; when indenting the #region and #endregion blocks. This function
;; defines a custom indention to indent the #region blocks as normal
;; text.
;;
;; To use this indenting just put the following in your emacs file:
;;   (c-set-offset 'cpp-macro 'csharp-lineup-region)
(defun csharp-lineup-region (langelem)
  "Indent all #region and #endregion blocks inline with code while
retaining normal column-zero indention for #if and the other
processing blocks."
  (save-excursion
    (back-to-indentation)
    (if (re-search-forward "#\\(end\\)?region" (c-point 'eol) [0]) 0  [0])))


;; Another option: indent both the if/endif and region/endregion
;; to lineup with whatever the current syntax state is.
;;
;; To use this indenting just put the following in your emacs file:
;;   (c-set-offset 'cpp-macro 'csharp-lineup-if-and-region)
(defun csharp-lineup-if-and-region (langelem)
  "Indent all #region/endregion blocks and #if/endif blocks inline with code while
retaining normal column-zero indention for any other
processing blocks."
  (save-excursion
    (back-to-indentation)
    (if (re-search-forward "#\\(\\(end\\)?\\(if\\|region\\)\\|else\\)" (c-point 'eol) [0]) 0  [0])))



;; There's a nifty thing called mumamo, for using multiple major modes
;; in one buffer. This makes editing HTML files with embedded CSS and
;; Javascript really nice.  But, for aome reason it suppresses
;; c-after-change in modes not derived from c-mode. csharp-mode is
;; included in that. Not sure why mumamo would want to do that, but it
;; does.  This defun was used only to test the premise.  Not needed at runtime.
;;
;; (defun csharp-is-derived-from-c-mode ()
;;   "tells whether current mode is derived from c-mode"
;;   (interactive)
;;   (message (concat "is mode derived from c-mode: "
;;       (cond ((derived-mode-p 'c-mode)
;;              "YES")
;;             (t "NO"))))
;;   )
;;


;; ==================================================================
;; handle font-lock for verbatim string literals



(defconst csharp-font-lock-syntactic-keywords
  '(("\\(@\\)\\(\"\\)[^\"]*\\(\"\\)\\(\"\\)[^\"]*\\(\"\\)[^\"]"
     (1 '(6)) (2 '(7)) (3 '(1)) (4 '(1)) (5 '(7))
                 ))
  "Highlighting of verbatim literal strings. See also the variable
  `font-lock-keywords'.")



;; (c-lang-defconst c-get-state-before-change-function
;;   csharp 'csharp-get-state-for-verbatim-strings)

;; (c-lang-defconst c-before-font-lock-function
;;   csharp 'csharp-prep-font-lock-for-verbatim-strings)






;; variables to track state: beginning-of-verbatim-literal-string
;; (BOVLS) and end-of-verbatim-literal-string (EOVLS).  These variables
;; are called "old" because they are the state prior to any change being
;; made in the buffer.
(defvar csharp-old-BOVLS 0)
(make-variable-buffer-local 'csharp-old-BOVLS)
(defvar csharp-old-EOVLS 0)
(make-variable-buffer-local 'csharp-old-EOVLS)

(defvar csharp-new-BOVLS 0)
(make-variable-buffer-local 'csharp-new-BOVLS)
(defvar csharp-new-EOVLS 0)
(make-variable-buffer-local 'csharp-new-EOVLS)



;;(debug-on-entry 'csharp-get-state-for-verbatim-strings)
;;(setq debug-on-error t)


(defun csharp-get-state-for-verbatim-strings (beg end)
  ;; DPC, 2009/12/30
  ;; need to check if the change will affect a verbatim string literal.
  ;;
  ;; Sets state variables csharp-old-BOVLS and csharp-old-EOVLS, which
  ;; point to the beginning and end of the verbatim literal string that
  ;; beg/end intersects with.  If there is no verbatim string, then this
  ;; function sets those state variables to zero.
  ;;
  ;; Point is undefined both before and after this function call; the buffer
  ;; has already been widened, and match-data saved.
  ;;
  ;; The return value is meaningless.
  ;;
  (message (format "C#: get-state:  beg(%d) end(%d) BOVLS(%d) EOVLS(%d)"
                   beg end csharp-new-BOVLS csharp-new-EOVLS))

  (cond

   ;; Are we at the top-of-file?  If so, this is likely the first run through.
   ;; This is our chance to parse all the verbatim string literals, and set the
   ;; syntax table overrides on each one.
   ((not (char-before))
    (csharp-fullscan-for-verbatim-literals-and-set-props beg end)
    (setq csharp-old-BOVLS 0
          csharp-old-EOVLS 0)
    )

   (t
    (if (or (< beg csharp-new-BOVLS)
            (> beg csharp-new-EOVLS))
        (let ((tmp (csharp-check-if-within-vliteral-and-find-limits beg end)))
          (setq csharp-old-BOVLS (car tmp)
                csharp-old-EOVLS (cdr tmp))

          ;; reset these
          (setq csharp-new-BOVLS 0
                csharp-new-EOVLS 0)
          )
      )
    )
   )
  )


(defun csharp-max-beginning-of-stmt ()
  "Return the greater of `c-beginning-of-statement-1' and
`c-beginning-of-statement' .  I don't understand why both of
these methods are necessary or why they differ. But they do."

  (let (dash
        nodash
        (curpos (point)))

    ;; I think this may need a save-excursion...
    ;; Calling c-beginning-of-statement-1 resets the point!

    (setq dash (progn (c-beginning-of-statement-1) (point)))
    (message (format "C#: max-bostmt dash(%d)" dash))
    (goto-char curpos)

    (setq nodash (progn (c-beginning-of-statement 1) (point)))
    (message (format "C#: max-bostmt nodash(%d)" nodash))
    (goto-char curpos)

    (max dash nodash)
    )
  )




(defun csharp-in-literal (&optional lim detect-cpp)
  "Return the type of literal point is in, if any.
The return value is `c' if in a C-style comment, `c++' if in a C++
style comment, `string' if in a string literal, `pound' if DETECT-CPP
is non-nil and in a preprocessor line, or nil if somewhere else.
Optional LIM is used as the backward limit of the search.  If omitted,
or nil, `c-beginning-of-defun' is used.

The last point calculated is cached if the cache is enabled, i.e. if
`c-in-literal-cache' is bound to a two element vector.

Note that this function might do hidden buffer changes.  See the
comment at the start of cc-engine.el for more info."

    (let ((rtn (save-excursion
                 (let* ((pos (point))
                        (lim (or lim (progn
                                       (c-beginning-of-syntax)
                                       (point))))
                        (state (parse-partial-sexp lim pos)))
                   (message (concat (format "C#: parse lim(%d) state: " lim) (prin1-to-string state)))
                   (cond
                    ((elt state 3)
                     (message (format "C#: in literal string (%d)" pos))
                     'string)
                    ((elt state 4)
                     (message (format "C#: in literal comment (%d)" pos))
                     (if (elt state 7) 'c++ 'c))
                    ((and detect-cpp (c-beginning-of-macro lim)) 'pound)
                    (t nil))))))
      rtn))


(defun csharp-fullscan-for-verbatim-literals-and-set-props (beg end)
  ;; Does a full scan of the buffer for verbatim literal strings.
  ;; When a verblit string is found, set override char properties
  ;; to allow proper syntax highlighting, indenting, and movement.

  (let ((curpos beg)
        (state 0)
        (cycle 0)
        literal
        eos
        limits
        (start 0))

    (message "C#: fullscan")
    (goto-char beg)

    (while (and (< curpos end) (< cycle 10000))
      (cond
       ;; current char is a @ sign
       ((= ?@ (char-after curpos))

        (message (format "C#: fullscan: @ before point(%d)" (point)))

        ;; are we in a comment?   a string?  let's find out.

        ;; having trouble getting beginning-of-statement to behave the way I expect.
        ;; even after setting the s-t overrides, it still skips back over
        ;; vlit strings that have a slash as the final char.  There are two forms, and
        ;; in some cases, one form works, and in others, the other form works.
        ;; so far I haven't discerned the pattern.

        (syntax-ppss-flush-cache 1)
        (parse-partial-sexp 1 curpos)

    (setq start (progn (c-beginning-of-statement-1) (point)))
    (message (format "C#: beg-o-stmt dash(%d)" start))
    (goto-char curpos)


        ;;(setq start (csharp-max-beginning-of-stmt))
        (goto-char curpos)

        ;;(setq literal (c-in-literal start))
        (setq literal (csharp-in-literal))

        (message (format "C#: fullscan: @ start(%d) cur(%d) c(%c) lit(%s)"
                         start curpos (char-after) (cond ((not literal) "nil")
                                                         (t literal))))

        (cond

         ((eq literal 'string)
          ;; should never happen.  We hop over strings.
          nil ;; do nothing - it's a @ within a string.
          )

         ((and (memq literal '(c c++))
               ;; This is a kludge for XEmacs where we use
               ;; `buffer-syntactic-context', which doesn't correctly
               ;; recognize "\*/" to end a block comment.
               ;; `parse-partial-sexp' which is used by
               ;; `c-literal-limits' will however do that in most
               ;; versions, which results in that we get nil from
               ;; `c-literal-limits' even when `c-in-literal' claims
               ;; we're inside a comment.
               ;;(setq limits (c-literal-limits start)))
               (setq limits (c-literal-limits )))

          ;; advance to the end of the comment
          (if limits
              (progn
              (message (format "C#: fullscan: jump to the end of the comment A (%d)" (cdr limits)))
              (setq curpos (cdr limits)))
            )
          )

         ;; it is not in a comment, nor in a string literal.
         ;; check if it is the beginning of a verbatim string literal.
         ((and (< (+ 2 curpos) end)
               (= ?\" (char-after (+ 1 curpos))))

          (setq eos (csharp-end-of-verbatim-literal-string))
          ;; set override syntax properties on the verblit string
          (csharp-set-vliteral-syntax-table-properties curpos eos)

          ;; Now that new syntax table property overrides are available,
          ;; reparse and discard result, in hopes of allowing future
          ;; (c-beginning-of-statement 1) to work properly.
          ;; (didn't work)
          ;;(parse-partial-sexp start eos)

          ;; jump to the end of the verblit string
          (message (format "C#: fullscan: jump to the end of the verblit string (%d)" eos))
          (setq curpos eos)
          )
         )
        )

       ;; current char is a double-quote
       ((= ?\" (char-after curpos))

        ;; are we in a comment?
        ;;(setq start (progn (c-beginning-of-statement-1) (point)))
        ;;(setq start (progn (c-beginning-of-statement 1) (point)))
        ;;(setq start (csharp-max-beginning-of-stmt))

        (goto-char curpos)
        ;;(setq literal (c-in-literal start))
        (setq literal (c-in-literal))

        (message (format "C#: fullscan: quote start(%d) cur(%d) c(%c) lit(%s)"
                         start curpos (char-after) (cond ((not literal) "nil")
                                                         (t literal))))

        (cond

         ((eq literal 'string)
          ;; should never happen
          nil ;; do nothing - it's a quote within a string.
          )

         ((and (memq literal '(c c++))
               ;; This is a kludge for XEmacs where we use
               ;; `buffer-syntactic-context', which doesn't correctly
               ;; recognize "\*/" to end a block comment.
               ;; `parse-partial-sexp' which is used by
               ;; `c-literal-limits' will however do that in most
               ;; versions, which results in that we get nil from
               ;; `c-literal-limits' even when `c-in-literal' claims
               ;; we're inside a comment.
               ;;(setq limits (c-literal-limits start)))
               (setq limits (c-literal-limits )))

          ;; advance to the end of the comment
          (if limits
              (progn
              (setq curpos (cdr limits))
              (message (format "C#: fullscan: jump to the end of the comment B (%d)" curpos))
              )
            )
          )

         ;; not in a comment, not in a string
         ;; this is the beginning of a literal string.
         (t
          (forward-char 1) ;; pass up the quote
          ;;(setq limits (c-literal-limits start))
          (setq limits (c-literal-limits ))
          ;; advance to the end of the literal string
          (if limits
              (progn
              (message (format "C#: fullscan: jump to the end of the literal (%d)" (cdr limits)))
              (setq curpos (cdr limits)))
            )
          )

         )
        )
       )

      (setq cycle (+ 1 cycle))
      (setq curpos (+ 1 curpos))
      (c-safe (goto-char curpos))
      )
    )
  )


(defun csharp-check-if-within-vliteral-and-find-limits (beg end)
  ;; DPC, 2009/12/30
  ;; Finds the begin and end of the verbatim literal string, starting
  ;; with beg/end limits.  returns a 2-element list with buffer
  ;; positions, or (0,0) for no verbatim literal string.
  (let ((rtnbeg 0) (rtnend 0) bostmt)

    (message (format "C#: check-if-within-vlit: beg(%d) end(%d)"
                     beg end))

    (goto-char beg)
    ;; calling c-beginning-of-statement-1 here did not work.
    ;; When point is within a literal string, as in an assignment, it jumps waaaay
    ;; back to i-don't-know-what. c-beginning-of-statement, intended
    ;; for interactive use, works fine.
    ;;(c-beginning-of-statement-1)
    ;;(c-beginning-of-statement 1)
    (csharp-max-beginning-of-stmt)
    (setq bostmt (point))

    (cond ((and
            (= ?\" (char-before))
            (= ?\@ (char-before (- bostmt 1) ) ) )
           ;; This is a literal string, within an initializer for a string array.
           ;; eg, in the following:
           ;;
           ;;  List<string> foo = new List<string> {
           ;;    @"Verbatim",
           ;;  };
           ;;
           ;; The c-beginning-of-statement-1 would put point right AFTER the " following the @.
           (setq rtnbeg bostmt)
           )

          (t
           ;; skip forward to first double-quote

           (let*
               ((distance (skip-chars-forward "^\"" beg))
                (posn (+ distance bostmt)) ;; I think this is just point
                )

             (cond
              ((and
                (> distance 0)
                (< posn beg)
                (= ?\" (char-after))     ;; redundant?
                (= ?@ (char-before))     ;; a verbatim literal string
                )
               ;; It looks like the beg position is within a verbatim literal string.
               (setq rtnbeg (- posn 1))
               )

              (t
               (message (format "C#: check-if-within-vlit: no quote?: dist(%d) posn(%d) after(%c) before(%c)"
                                distance posn (char-after)
                                (cond ((char-before)) (t ?- ))))
               nil

               )
              )
             )
           )
          )


    ;; if rtnbeg is set, then find rtnend
    (if (> rtnbeg 0)
        ;; find the end of the string, even through newlines.
        (setq rtnend (csharp-end-of-verbatim-literal-string))
      )

    (cons rtnbeg rtnend)

    )
  )



(defun csharp-end-of-verbatim-literal-string ()

  ;; Returns the position of the end quote of the verbatim literal
  ;; string.  Within a verbatim literal string, a doubled
  ;; double-quote escapes the double-quote.
    (message (format "C#: end-of-vlit-string: point(%d) c(%c)" (point) (char-after)))

  (let (curpos max)
    (forward-char 2) ;; pass up the @ sign and first quote
    (setq curpos (point))
    (setq max (point-max))
    (while (and
            (or
             (not (eq (char-after curpos) ?\")) ;; either it's not a quote
             (eq (char-after (+ curpos 1)) ?\")) ;; or, its a double double quote
            (< curpos max))

      (cond
       ((and
         (eq (char-after curpos) ?\")
         (eq (char-after (+ curpos 1)) ?\"))

        (setq curpos (+ 2 curpos)))
       (t

        (setq curpos (+ 1 curpos)))
       )
      )

    curpos
    )
  )


(defun csharp-end-of-literal-string ()

  ;; Returns the position of the end quote of the literal string.
  ;; Within a literal string, a backslash double-quote escapes the
  ;; double-quote.
  (let (curpos max)
    (forward-char 1) ;; pass up the first quote
    (setq curpos (point))
    (setq max (point-max))
    (while (and
            (or
             (not (eq (char-after curpos) ?\")) ;; either it's not a quote
             (eq (char-after (- curpos 1)) ?\\)) ;; or, it's escaped
            (< curpos max))

      (cond
       ((and
         (eq (char-after curpos) ?\")
         (eq (char-after (- curpos 1)) ?\\))

        (setq curpos (+ 2 curpos)))
       (t
        (setq curpos (+ 1 curpos)))
       )
      )

    curpos
    )
  )





;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; used in debugging/diagnostics
(defun csharp-show-syntax-table-prop (&optional pos)
  "shows the syntax table property of the current char"
   (interactive)
   (let ((prop (c-get-char-property (point) 'syntax-table)))

     (message (concat "C#: syntax table prop: " (prin1-to-string prop)))
     )
   )


(defun csharp-show-parse-state()
  "displays the result of parse-partial-sexp"
  (interactive)
  (let* ((pos (point))
         (lim (progn
                (c-beginning-of-syntax)
                (point)))
         (state (parse-partial-sexp lim pos)))

     (message (concat "C#: parse state: " (prin1-to-string state))))
  )

(defun csharp-interactive-c-in-literal()
  "displays the result of c-in-literal"
  (interactive)
  (let ((literal (c-in-literal)))

     (message (concat "C#: in-literal: " (prin1-to-string literal))))
  )


(defun csharp-interactive-beginning-of-syntax ()
  "goes to the closest previous point that is known to be outside a string literal"
  (interactive)
  (c-beginning-of-syntax)
  )


(defun csharp-interactive-beginning-of-statement ()
  "goes to the closest previous point that is the beginning of a statement"
  (interactive)
  (c-beginning-of-statement 1)
  )

(defun csharp-interactive-beginning-of-statement-1 ()
  "goes to the closest previous point that is the beginning of a statement"
  (interactive)
  (c-beginning-of-statement-1)
  )

;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++



(defun csharp-prep-font-lock-for-verbatim-strings (beg end old-len)
  ;; Expand the region (BEG END) as needed to (c-new-BEG c-new-END) then put
  ;; `syntax-table' properties on this region.
  ;;
  ;; This function is called from an after-change function, BEG END and
  ;; OLD-LEN being the standard parameters.
  ;;
  ;; Point is undefined both before and after this function call, the buffer
  ;; has been widened, and match-data saved.  The return value is ignored.
  ;;
  ;; It prepares the buffer for font
  ;; locking, hence must get called before `font-lock-after-change-function'.
  ;;
  ;; This function is the C# value for `c-before-font-lock-function'.
  ;; It does hidden buffer changes.

  (cond
   ((and
     (> csharp-old-BOVLS 0)
     (< csharp-old-BOVLS beg)
     (<= beg csharp-old-EOVLS))

    ;; make changes to text properties within a c-save-buffer-state
    (c-save-buffer-state ()

;;       (message (format "C#: b4-fontlock: verbatim string start(%d) end(%d)"
;;                        csharp-old-BOVLS
;;                        csharp-old-EOVLS
;;                        ))

      (let ((tmp (csharp-check-if-within-vliteral-and-find-limits beg end)))
        (setq c-new-BEG (car tmp))
        (setq c-new-END (cdr tmp))
        )

      (csharp-set-vliteral-syntax-table-properties
                       c-new-BEG
                       c-new-END)
      )

    )
   (t
;;     (message (format "C#: b4-fontlock: not in a verbatim string beg(%d) end(%d) BOVLS(%d) EOVLS(%d)"
;;                      beg end
;;                      csharp-old-BOVLS
;;                      csharp-old-EOVLS
;;                      ))

    (setq c-new-BEG beg
          c-new-END end)

    ))
)



;; DPC, 2010/02/09
;; ----------------------
;; This is the kind of thing that could be handled by YASnippet or one
;; of another similarly flexible snippet framework. But I don't want to
;; introduce a dependency on yasnippet to csharp mode. So this code must
;; live within csharp-mode itself.

(defun csharp-maybe-insert-codedoc (arg)

  "Insert an xml code documentation template as appropriate, when
typing slashes.  This fn gets bound to / (the slash key), in
csharp-mode.  If the slash being inserted is not the third
consecutive slash, the slash is inserted as normal.  If it is the
third consecutive slash, then a xml code documentation template
may be inserted in some cases. For example,

  a <summary> template is inserted if the prior line is empty,
        or contains only an open curly brace;
  a <remarks> template is inserted if the prior word
        closes the <summary> element;
  a <returns> template is inserted if the prior word
        closes the <remarks> element;
  an <example> template is inserted if the prior word closes
        the <returns> element;
  a <para> template is inserted if the prior word closes
        a <para> element.

In all other cases the slash is inserted as normal.

If you want the default cc-mode behavior, which implies no automatic
insertion of xml code documentation templates, then use this in
your `csharp-mode-hook' function:

     (local-set-key (kbd \"/\") 'c-electric-slash)

 "
  (interactive "*p")
  ;;(message "csharp-maybe-insert-codedoc")
  (let (
        (cur-point (point))
        (char last-command-char)
        (cb0 (char-before (- (point) 0)))
        (cb1 (char-before (- (point) 1)))
        is-first-non-whitespace
        did-auto-insert
        )

    ;; check if two prior chars were slash
    (if (and
         (= char ?/)
         cb0 (= ?/ cb0)
         cb1 (= ?/ cb1)
         )

        (progn

          ;;(message "yes - this is the third consecutive slash")

          (setq is-first-non-whitespace
                (save-excursion
                  (back-to-indentation)
                  (= cur-point (+ (point) 2))))

          (if is-first-non-whitespace
              ;; This is a 3-slash sequence.  It is the first non-whitespace text
              ;; on the line. Now we need to examine the surrounding context
              ;; in order to determine which xml cod doc template to insert.
              (let (word-back char0 char1
                    word-fore char-0 char-1
                    text-to-insert         ;; text to insert in lieu of slash
                    fn-to-call     ;; func to call after inserting text
                    (preceding-line-is-empty (or
                                              (= (line-number-at-pos) 1)
                                              (save-excursion
                                               (previous-line)
                                               (beginning-of-line)
                                               (looking-at "[ \t]*$\\|[ \t]*{[ \t]*$"))))
                    (flavor 0) ;; used only for diagnostic purposes
                    )

                ;;(message "starting a 3-slash comment")

                ;; get the prior word, and the 2 chars preceding it.
                (backward-word)

                (setq word-back (thing-at-point 'word)
                      char0 (char-before (- (point) 0))
                      char1 (char-before (- (point) 1))
                      )

                ;; restore prior position
                (goto-char cur-point)

                ;; get the following word, and the 2 chars preceding it.
                (forward-word)
                (backward-word)
                (setq word-fore (thing-at-point 'word)
                      char-0 (char-before (- (point) 0))
                      char-1 (char-before (- (point) 1)))

                ;; restore prior position again
                (goto-char cur-point)

                (cond
                 ;; The preceding line is empty, or all whitespace, or
                 ;; contains only an open-curly.  In this case, insert a
                 ;; summary element pair.
                 (preceding-line-is-empty
                  (setq text-to-insert  "/ <summary>\n///   \n/// </summary>"
                        flavor 1) )

                 ;; The preceding word closed a summary element.  In this case,
                 ;; if the forward word does not open a remarks element, then
                 ;; insert a remarks element.
                 ((and (string-equal word-back "summary") (eq char0 ?/)  (eq char1 ?<))
                  (if (not (and (string-equal word-fore "remarks") (eq char-0 ?<)))
                      (setq text-to-insert "/ <remarks>\n///   <para>\n///     \n///   </para>\n/// </remarks>"
                            flavor 2) )
                  )

                 ;; The preceding word closed the remarks section.  In this case,
                 ;; insert an example element.
                 ((and (string-equal word-back "remarks")  (eq char0 ?/)  (eq char1 ?<))
                  (setq text-to-insert "/ <example>\n///   \n/// </example>"
                        flavor 3) )

                 ;; The preceding word closed the example section.  In this
                 ;; case, insert an returns element.  This isn't always
                 ;; correct, because sometimes the xml code doc is attached to
                 ;; a class or a property, neither of which has a return
                 ;; value. A more intelligent implementation would inspect the
                 ;; syntax state and only inject a returns element if
                 ;; appropriate.
                 ((and (string-equal word-back "example")  (eq char0 ?/)  (eq char1 ?<))
                  (setq text-to-insert "/ <returns></returns>"
                        fn-to-call (lambda ()
                                     (backward-word)
                                     (backward-char)
                                     (backward-char)
                                     (c-indent-line-or-region)
                                     )
                        flavor 4) )

                 ;; The preceding word opened the remarks section, or it
                 ;; closed a para section. In this case, insert a para
                 ;; element, using appropriate indentation with respect to the
                 ;; prior tag.
                 ((or
                   (and (string-equal word-back "remarks")  (eq char0 ?<)  (or (eq char1 32) (eq char1 9)))
                   (and (string-equal word-back "para")     (eq char0 ?/)  (eq char1 ?<)))

                  (let (prior-point spacer)
                    (save-excursion
                      (backward-word)
                      (backward-char)
                      (backward-char)
                      (setq prior-point (point))
                      (skip-chars-backward "\t ")
                      (setq spacer (buffer-substring (point) prior-point))
                      ;;(message (format "pt(%d) prior(%d) spacer(%s)" (point) prior-point spacer))
                      )

                    (if (string-equal word-back "remarks")
                        (setq spacer (concat spacer "   "))
                        )

                    (setq text-to-insert (format "/%s<para>\n///%s  \n///%s</para>"
                                                 spacer spacer spacer)
                          flavor 6) )
                    )

                 ;; The preceding word opened a para element.  In this case, if
                 ;; the forward word does not close the para element, then
                 ;; close the para element.
                 ;; --
                 ;; This is a nice idea but flawed.  Suppose I have a para element with some
                 ;; text in it. If I position the cursor at the first line, then type 3 slashes,
                 ;; I get a close-element, and that would be inappropriate.  Not sure I can
                 ;; easily solve that problem, so the best thing might be to simply punt, and
                 ;; require people to close their own elements.
                 ;;
                 ;;              ( (and (string-equal word-back "para")  (eq char0 60)  (or (eq char1 32) (eq char1 9)))
                 ;;                (if (not (and (string-equal word-fore "para") (eq char-0 47) (eq char-1 60) ))
                 ;;                    (setq text-to-insert "/   \n/// </para>\n///"
                 ;;                          fn-to-call (lambda ()
                 ;;                                       (previous-line)
                 ;;                                       (end-of-line)
                 ;;                                       )
                 ;;                          flavor 7) )
                 ;;                )

                 ;; the default case - do nothing
                 (t nil)
                 )

                (if text-to-insert
                    (progn
                      ;;(message (format "inserting special text (f(%d))" flavor))

                      ;; set the flag, that we actually inserted text
                      (setq did-auto-insert t)

                      ;; save point of beginning of insertion
                      (setq cur-point (point))

                      ;; actually insert the text
                      (insert text-to-insert)

                      ;; indent the inserted string, and re-position point, either through
                      ;; the case-specific fn, or via the default progn.
                      (if fn-to-call
                          (funcall fn-to-call)

                        (let ((newline-count 0) (pos 0) ix)

                          ;; count the number of newlines in the inserted string
                          (while (string-match "\n" text-to-insert pos)
                            (setq pos (match-end 0)
                                  newline-count (+ newline-count 1) )
                            )

                          ;; indent what we just inserted
                          (c-indent-region cur-point (point) t)

                          ;; move up n/2 lines. This assumes that the
                          ;; inserted text is ~symmetric about the halfway point.
                          ;; The assumption holds if the xml code doc uses a
                          ;; begin-elt and end-elt on a new line all by themselves,
                          ;; and a blank line in between them where the point should be.
                          ;; A more intelligent implementation would use a specific
                          ;; marker string, like @@DOT, to note the desired point.
                          (previous-line (/ newline-count 2))
                          (end-of-line)
                          )
                        )
                      )
                  )
                )
            )
          )
      )

    (if (not did-auto-insert)
        (self-insert-command (prefix-numeric-value arg)))
    )
  )



(defun csharp-set-vliteral-syntax-table-properties (beg end)
  ;; Scan the buffer text between BEG and END, a verbatim literal
  ;; string, setting (and clearing) syntax-table properties where
  ;; necessary.
  ;;
  ;; We need to set/clear the syntax-table property on:
  ;; (i)  \ - (backslash) It is not an escape inside a verbatim literal string.
  ;; (ii) " - (double-quote) It can be a literal quote, when doubled ("")
  ;;
  ;; BEG is the @ delimiter. END is the "old" position of the ending quote.
  ;;
  ;; see http://www.sunsite.ualberta.ca/Documentation/Gnu/emacs-lisp-ref-21-2.7/html_node/elisp_592.html
  ;; for the list of syntax table numeric codes

  ;;(message (format "C#: set-vlit-syntax-table:  beg(%d) end(%d)" beg end))

  (if (and (> beg 0) (> end 0))

      (let ((curpos beg)
            (state 0))

        (setq csharp-new-BOVLS beg
              csharp-new-EOVLS end)

        (c-clear-char-properties beg end 'syntax-table)

        (while (<= curpos end)

          (cond
           ((= state 0)
            (if (= (char-after curpos) ?@)
                (progn
                  (c-put-char-property curpos 'syntax-table '(3)) ; (6) = expression prefix, (3) = symbol
                  ;;(message (format "C#: set-s-t: prefix pos(%d) chr(%c)" beg (char-after beg)))
                  )
              )
            (setq state (+ 1 state))
            )

           ((= state 1)
            (if (= (char-after curpos) ?\")
                (progn
                  (c-put-char-property curpos 'syntax-table '(7)) ; (7) = string quote
                  ;;(message (format "C#: set-s-t: open quote pos(%d) chr(%c)"
                  ;; curpos (char-after curpos)))
                  )
              )
            (setq state (+ 1 state))
            )

           ((= state 2)
            (cond
             ;; handle backslash
             ((= (char-after curpos) ?\\)
              (c-put-char-property curpos 'syntax-table '(2)) ; (1) = punctuation, (2) = word
              ;;(message (format "C#: set-s-t: backslash word pos(%d) chr(%c)" curpos (char-after curpos)))
              )

             ;; doubled double-quote
             ((and
               (= (char-after curpos) ?\")
               (= (char-after (+ 1 curpos)) ?\"))
              (c-put-char-property curpos 'syntax-table '(2)) ; (1) = punctuation, (2) = word
              (c-put-char-property (+ 1 curpos) 'syntax-table '(2)) ; (1) = punctuation
              ;;(message (format "C#: set-s-t: double doublequote pos(%d) chr(%c)" curpos (char-after curpos)))
              (setq curpos (+ curpos 1))
              )

             ;; a single double-quote, which should be a string terminator
             ((= (char-after curpos) ?\")
              (c-put-char-property curpos 'syntax-table '(7)) ; (7) = string quote
              ;;(message (format "C#: set-s-t: close quote pos(%d) chr(%c)" curpos (char-after curpos)))
              ;; go no further
              (setq state (+ 1 state))
              )

             ;; everything else
             (t
              ;;(message (format "C#: set-s-t: none pos(%d) chr(%c)" curpos (char-after curpos)))
              nil
              )
             )
            )
           )
          ;; next char
          (setq curpos (+ curpos 1))
          )
        )
    )
  )



;; finally, set up the advice on the various font-locking methods:

;; (defmacro csharp-advise-fl-for-region (function)
;;   `(defadvice ,function (before get-csharp-vlit-region activate)
;;      ;; When font-locking a C# Mode buffer, make sure that any verbatim literal
;;      ;; string is completely font-locked.
;;      (when
;;          (eq major-mode 'csharp-mode)
;;        (save-excursion
;;          (ad-set-arg 1 c-new-END)       ; end
;;          (ad-set-arg 0 c-new-BEG)))     ; beg
;;      ))

;; (csharp-advise-fl-for-region font-lock-after-change-function)
;; (csharp-advise-fl-for-region jit-lock-after-change)
;; (csharp-advise-fl-for-region lazy-lock-defer-rest-after-change)
;; (csharp-advise-fl-for-region lazy-lock-defer-line-after-change)



;; (defadvice c-literal-limits (around
;;                              csharp-ad-literal-limits
;;                              first
;;                              (&optional lim near not-in-delimiter)
;;                              activate
;;                              compile)
;;   (if
;;       (eq major-mode 'csharp-mode)
;;       ;; then
;;       (setq ad-return-value (csharp-literal-limits lim near not-in-delimiter))
;;     ;; else
;;     ad-do-it
;;     )
;;   )


;; (defun csharp-beginning-of-syntax (&optional pos)
;;   "shows the value of beginning-of-syntax at point"
;;   (interactive)
;;   (c-save-buffer-state (tmp beg)
;;     ;;(message (format "BOS %d" (c-beginning-of-syntax)))
;;     (cond
;;      ((save-excursion
;;         ;;(< (skip-syntax-backward "^\"|") 0)
;;         (setq tmp (skip-syntax-backward "\"|"))
;;         (< tmp 0))
;;       ;; if the above is true, then we moved back a non-zero number of
;;       ;; characters, and that means we're in a string. NO IT DOESN'T.
;;       (setq beg (c-safe (c-backward-sexp 1) (point)))
;;       (message (format "C#-BOS: beginning-of-string beg(%d) tmp(%d)" beg tmp))
;;       )
;;      (t (message "C#-BOS: not in a string")
;;       )
;;      )
;;     ;;(c-beginning-of-syntax)
;;     )
;;   )




;; ;; call c-slow-in-literal interactively
;; (defun csharp-is-in-literal ()
;;   "shows the value of the literal, if any, point is in."
;;   (interactive)

;;   (message (format "IN_LITERAL?: %s"
;;                    (cond
;;                     ((c-slow-in-literal))
;;                     (t 'nope))))

;;     )

;; ========================================================================





;; TODO
;; Defines our constant for finding attributes.
;;(defconst csharp-attribute-regex "\\[\\([XmlType]+\\)(")
;;(defconst csharp-attribute-regex "\\[\\(.\\)")
;; This doesn't work because the string regex happens before this point
;; and getting the font-locking to work before and after is fairly difficult
;;(defconst csharp-attribute-regex
;;  (concat
;;   "\\[[a-zA-Z][ \ta-zA-Z0-9.]+"
;;   "\\((.*\\)?"
;;))

;; Java uses a series of regexes to change the font-lock for class
;; references. The problem comes in because Java uses Pascal (leading
;; space in names, SomeClass) for class and package names, but
;; Camel-casing (initial lowercase, upper case in words,
;; i.e. someVariable) for variables. The notation suggested by EMCA for C# is
;; to use Pascal notation for everything, except inner variables. So,
;; the Java regex and formatting produces very wrong results in C#.
;;(error (byte-compile-dest-file))
;;(error (c-get-current-file))
(c-lang-defconst c-opt-after-id-concat-key
  csharp (if (c-lang-const c-opt-identifier-concat-key)
             (c-lang-const c-symbol-start)))

(c-lang-defconst c-basic-matchers-before
  csharp `(
           ;;;; Font-lock the attributes by searching for the
           ;;;; appropriate regex and marking it as TODO.
           ;;,`(,(concat "\\(" csharp-attribute-regex "\\)")
           ;;   0 font-lock-function-name-face)

           ;; Put a warning face on the opener of unclosed strings that
           ;; can't span lines.  Later font
           ;; lock packages have a `font-lock-syntactic-face-function' for
           ;; this, but it doesn't give the control we want since any
           ;; fontification done inside the function will be
           ;; unconditionally overridden.
           ,(c-make-font-lock-search-function
             ;; Match a char before the string starter to make
             ;; `c-skip-comments-and-strings' work correctly.
             (concat ".\\(" c-string-limit-regexp "\\)")
             '((c-font-lock-invalid-string)))

           ;; Fontify keyword constants.
           ,@(when (c-lang-const c-constant-kwds)
               (let ((re (c-make-keywords-re nil
                           (c-lang-const c-constant-kwds))))
                 `((eval . (list ,(concat "\\<\\(" re "\\)\\>")
                                 1 c-constant-face-name)))))

           ;; Fontify all keywords except the primitive types.
           ,`(,(concat "\\<" (c-lang-const c-regular-keywords-regexp))
              1 font-lock-keyword-face)

           ;; Fontify leading identifiers in fully qualified names like
           ;; "Foo.Bar".
           ,@(when (c-lang-const c-opt-identifier-concat-key)
               `((,(byte-compile
                    `(lambda (limit)
                       (while (re-search-forward
                               ,(concat "\\(\\<" ; 1
                                        "\\(" (c-lang-const c-symbol-key)
                                        "\\)" ; 2
                                        "[ \t\n\r\f\v]*"
                                        (c-lang-const
                                         c-opt-identifier-concat-key)
                                        "[ \t\n\r\f\v]*"
                                        "\\)"
                                        "\\("
                                        (c-lang-const
                                         c-opt-after-id-concat-key)
                                        "\\)")
                               limit t)
                         (unless (progn
                                   (goto-char (match-beginning 0))
                                   (c-skip-comments-and-strings limit))
                           (or (get-text-property (match-beginning 2) 'face)
                               (c-put-font-lock-face (match-beginning 2)
                                                     (match-end 2)
                                                     c-reference-face-name))
                           (goto-char (match-end 1)))))))))
           ))

;; C# does not allow a leading qualifier operator. It also doesn't
;; allow the ".*" construct of Java. So, we redo this regex without
;; the "\\|\\*" regex.
(c-lang-defconst c-identifier-key
  csharp (concat "\\(" (c-lang-const c-symbol-key) "\\)" ; 1
                 (concat "\\("
                         "[ \t\n\r\f\v]*"
                         (c-lang-const c-opt-identifier-concat-key)
                         "[ \t\n\r\f\v]*"
                         (concat "\\("
                                 "\\(" (c-lang-const c-symbol-key) "\\)"
                                 "\\)")
                         "\\)*")))

;; C# has a few rules that are slightly different than Java for
;; operators. This also removed the Java's "super" and replaces it
;; with the C#'s "base".
(c-lang-defconst c-operators
  csharp `((prefix "base")))


;; =======================================================
;; setting values of constants defined in cc-langs.el

;; C# uses CPP-like prefixes to mark #define, #region/endregion,
;; #if/else/endif, and #pragma.  This regexp matches the prefix,
;; not including the beginning-of-line (BOL), and not including
;; the term after the prefix (define, pragma, etc).  This regexp says
;; whitespace, followed by the prefix, followed by maybe more whitespace.
;; I think.

(c-lang-defconst c-opt-cpp-prefix
  csharp "\\s *#\\s *")


;; there are no message directives in C#
(c-lang-defconst c-cpp-message-directives
  csharp nil)

(c-lang-defconst c-cpp-expr-directives
  csharp '("if"))

(c-lang-defconst c-opt-cpp-macro-define
  csharp "define")

;; $ is not a legal char in an identifier in C#.  So we need to
;; create a csharp-specific definition of this constant.
(c-lang-defconst c-symbol-chars
  csharp (concat c-alnum "_"))


(c-lang-defconst c-colon-type-list-kwds
  csharp '("class"))

(c-lang-defconst c-block-prefix-disallowed-chars

  ;; Allow ':' for inherit list starters.
  csharp (set-difference (c-lang-const c-block-prefix-disallowed-chars)
                         '(?:)))


;; =======================================================



;; C# uses the following assignment operators
(c-lang-defconst c-assignment-operators
  csharp '("=" "*=" "/=" "%=" "+=" "-=" ">>=" "<<=" "&=" "^=" "|="))

;; This defines the primative types for C#
(c-lang-defconst c-primitive-type-kwds
  ;; ECMA-344, S8
  csharp '("object" "string" "sbyte" "short" "int" "long" "byte"
           "ushort" "uint" "ulong" "float" "double" "bool" "char"
           "decimal" "void"))

;; The keywords that define that the following is a type, such as a
;; class definition.
(c-lang-defconst c-type-prefix-kwds
  ;; ECMA-344, S?
  csharp '("class" "interface" "enum" "struct"))

;; Type modifier keywords. They appear anywhere in types, but modify
;; instead create one.
(c-lang-defconst c-type-modifier-kwds
  ;; EMCA-344, S?
  csharp '("readonly" "const"))

;; Structures that are similiar to classes.
(c-lang-defconst c-class-decl-kwds
  ;; EMCA-344, S?
  csharp '("class" "interface" "struct"))

;; The various modifiers used for class and method descriptions.
(c-lang-defconst c-modifier-kwds
  csharp '("public" "partial" "private" "const" "abstract"
           "protected" "ref" "out" "static" "virtual"
           "override" "params" "internal"))

;; We don't use the protection level stuff because it breaks the
;; method indenting. Not sure why, though.
(c-lang-defconst c-protection-kwds
  csharp nil)

;; Define the keywords that can have something following after them.
(c-lang-defconst c-type-list-kwds
  csharp '("struct" "class" "interface" "is" "as"
           "delegate" "event" "set" "get" "add" "remove"))

;; This allows the classes after the : in the class declartion to be
;; fontified.
(c-lang-defconst c-typeless-decl-kwds
  csharp '(":"))

;; Sets up the enum to handle the list properly
(c-lang-defconst c-brace-list-decl-kwds
  csharp '("enum"))


;; (c-lang-defconst c-ref-list-kwds
;;   csharp '("using" "namespace"))


(c-lang-defconst c-other-block-decl-kwds
  csharp '("using" "namespace"))

;; Statement keywords followed directly by a substatement
(c-lang-defconst c-block-stmt-1-kwds
  csharp '("do" "try" "finally"))


;; Statement keywords followed by a paren sexp and then by a substatement.
(c-lang-defconst c-block-stmt-2-kwds
  csharp '("for" "if" "switch" "while" "catch" "foreach"
           "checked" "unchecked" "lock"))


;; Statements that break out of braces
(c-lang-defconst c-simple-stmt-kwds
  csharp '("return" "continue" "break" "throw" "goto" ))

;; Statements that allow a label
;; TODO?
(c-lang-defconst c-before-label-kwds
  csharp nil)

;; Constant keywords
(c-lang-defconst c-constant-kwds
  csharp '("true" "false" "null"))

;; Keywords that start "primary expressions."
(c-lang-defconst c-primary-expr-kwds
  csharp '("this" "base"))

;; We need to treat namespace as an outer block so class indenting
;; works properly.
(c-lang-defconst c-other-block-decl-kwds
  csharp '("namespace"))

;; We need to include the "as" for the foreach
(c-lang-defconst c-other-kwds
  csharp '("in" "sizeof" "typeof" "is" "as"))

(c-lang-defconst c-overloadable-operators
  ;; EMCA-344, S14.2.1
  csharp '("+" "-" "*" "/" "%" "&" "|" "^"
           "<<" ">>" "==" "!=" ">" "<" ">=" "<="))


;; This c-cpp-matchers stuff is used for fontification.
;; see cc-font.el
;;

;; No cpp in this language, but there are still directives to fontify:
;; "#pragma", #region/endregion, #define, #undef, #if/else/endif.  (The definitions for
;; the extra keywords above are enough to incorporate them into the
;; fontification regexps for types and keywords, so no additional
;; font-lock patterns are required for keywords.)

(c-lang-defconst c-cpp-matchers
  csharp (cons
      ;; Use the eval form for `font-lock-keywords' to be able to use
      ;; the `c-preprocessor-face-name' variable that maps to a
      ;; suitable face depending on the (X)Emacs version.
      '(eval . (list "^\\s *\\(#pragma\\|undef\\|define\\)\\>\\(.*\\)"
                     (list 1 c-preprocessor-face-name)
                     '(2 font-lock-string-face)))
      ;; There are some other things in `c-cpp-matchers' besides the
      ;; preprocessor support, so include it.
      (c-lang-const c-cpp-matchers)))

(defcustom csharp-font-lock-extra-types nil
  "*List of extra types (aside from the type keywords) to recognize in C# mode.
Each list item should be a regexp matching a single identifier."
  :type 'list :group 'csharp)

(defconst csharp-font-lock-keywords-1 (c-lang-const c-matchers-1 csharp)
  "Minimal highlighting for C# mode.")

(defconst csharp-font-lock-keywords-2 (c-lang-const c-matchers-2 csharp)
  "Fast normal highlighting for C# mode.")

(defconst csharp-font-lock-keywords-3 (c-lang-const c-matchers-3 csharp)
  "Accurate normal highlighting for C# mode.")

(defvar csharp-font-lock-keywords csharp-font-lock-keywords-3
  "Default expressions to highlight in C# mode.")

(defvar csharp-mode-syntax-table nil
  "Syntax table used in csharp-mode buffers.")
(or csharp-mode-syntax-table
    (setq csharp-mode-syntax-table
          (funcall (c-lang-const c-make-mode-syntax-table csharp))))

(defvar csharp-mode-abbrev-table nil
  "Abbreviation table used in csharp-mode buffers.")
(c-define-abbrev-table 'csharp-mode-abbrev-table
  ;; Keywords that if they occur first on a line might alter the
  ;; syntactic context, and which therefore should trig reindentation
  ;; when they are completed.
  '(("else" "else" c-electric-continued-statement 0)
    ("while" "while" c-electric-continued-statement 0)
    ("catch" "catch" c-electric-continued-statement 0)
    ("finally" "finally" c-electric-continued-statement 0)))

(defvar csharp-mode-map (let ((map (c-make-inherited-keymap)))
                      ;; Add bindings which are only useful for C#
                      map)
  "Keymap used in csharp-mode buffers.")

;;(easy-menu-define csharp-menu csharp-mode-map "C# Mode Commands"
;;                ;; Can use `csharp' as the language for `c-mode-menu'
;;                ;; since its definition covers any language.  In
;;                ;; this case the language is used to adapt to the
;;                ;; nonexistence of a cpp pass and thus removing some
;;                ;; irrelevant menu alternatives.
;;                (cons "C#" (c-lang-const c-mode-menu csharp)))

;;; Autoload mode trigger
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.cs$" . csharp-mode))

;; Custom variables
;;;###autoload
(defcustom csharp-mode-hook nil
  "*Hook called by `csharp-mode'."
  :type 'hook
  :group 'c)

;;; The entry point into the mode
;;;###autoload
(defun csharp-mode ()
  "Major mode for editing C# code. This mode is derived from CC Mode to
support C#.

The hook `c-mode-common-hook' is run with no args at mode
initialization, then `csharp-mode-hook'.

Key bindings:
\\{csharp-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (make-local-variable 'beginning-of-defun-function)
  (make-local-variable 'end-of-defun-function)
  (c-initialize-cc-mode t)
  (set-syntax-table csharp-mode-syntax-table)


;;   (set (make-local-variable 'font-lock-defaults)
;;        (list js-font-lock-keywords
;;              nil nil '((?$ . "w") (?_ . "w")) nil
;;              '(font-lock-syntactic-keywords . csharp-font-lock-syntactic-keywords)))


;; (set (make-local-variable 'font-lock-defaults)
;;        (list espresso--font-lock-keywords
;;           nil nil nil nil
;;           '(font-lock-syntactic-keywords
;;                . espresso--font-lock-syntactic-keywords)))



  (setq major-mode 'csharp-mode
        mode-name "C#"
        local-abbrev-table csharp-mode-abbrev-table
        abbrev-mode t)
  (use-local-map csharp-mode-map)
  ;; `c-init-language-vars' is a macro that is expanded at compile
  ;; time to a large `setq' with all the language variables and their
  ;; customized values for our language.
  (c-init-language-vars csharp-mode)
  ;; `c-common-init' initializes most of the components of a CC Mode
  ;; buffer, including setup of the mode menu, font-lock, etc.
  ;; There's also a lower level routine `c-basic-common-init' that
  ;; only makes the necessary initialization to get the syntactic
  ;; analysis and similar things working.

  (c-common-init 'csharp-mode)

  (c-run-mode-hooks 'c-mode-common-hook 'csharp-mode-hook)

  ;;

  ;; allow fill-paragraph to work on xml code doc
  ;;   (make-local-variable 'paragraph-separate)
  ;;   (setq  paragraph-separate "[ \t]*\\(//+\\|\\**\\)\\([ \t]+\\|[ \t]+<.+?>\\)$\\|^\f")

   (set (make-local-variable 'paragraph-separate)
        "[ \t]*\\(//+\\|\\**\\)\\([ \t]+\\|[ \t]+<.+?>\\)$\\|^\f")


   ;; need this for parse-partial-sexp to work properly with verbatim literal strings
   (set (make-local-variable 'parse-sexp-lookup-properties)
        t)

   ;; DPC, 2010/02/09
   (local-set-key (kbd "/") 'csharp-maybe-insert-codedoc)

   (c-update-modeline)

  )


(message  (concat "Done loading " load-file-name))


 
(provide 'csharp-mode)

;;; csharp-mode.el ends here
