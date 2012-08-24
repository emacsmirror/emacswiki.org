;;; subr-21.el --- Provides Emacs 20 with Emacs 21 subr.el and built-in functions.
;; 
;; Filename: subr-21.el
;; Description: Provides Emacs 20 with Emacs 21 subr.el and built-in functions.
;; Author: GNU Emacs 21
;; Maintainer: Drew Adams
;; Created: Sun Oct 19 20:27:23 2003
;; Version: 20.0
;; Last-Updated: Thu Aug 23 17:02:33 2012 (-0700)
;;           By: dradams
;;     Update #: 29
;; URL: http://www.emacswiki.org/emacs-en/subr-21.el
;; Keywords: compatibility
;; Compatibility: Emacs 20.x
;; 
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; Provides Emacs 20 with some of the Emacs 21+ `subr.el' or built-in
;; functions.
;;
;; Some of the functions here do not exist in Emacs 20.  Others exist,
;; but have different signatures.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;;
;; 2006/06/24 dadams
;;     Added keywordp.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

(defun add-to-list (list-var element &optional append)
  "Add to the value of LIST-VAR the element ELEMENT if it isn't there yet.
The test for presence of ELEMENT is done with `equal'.
If ELEMENT is added, it is added at the beginning of the list,
unless the optional argument APPEND is non-nil, in which case
ELEMENT is added at the end.

If you want to use `add-to-list' on a variable that is not defined
until a certain package is loaded, you should put the call to `add-to-list'
into a hook function that will be run only after loading the package.
`eval-after-load' provides one way to do this.  In some cases
other hooks, such as major mode hooks, can do the job."
  (if (member element (symbol-value list-var))
      (symbol-value list-var)
    (set list-var
         (if append
             (append (symbol-value list-var) (list element))
           (cons element (symbol-value list-var))))))

(defun replace-regexp-in-string (regexp rep string &optional
                                 fixedcase literal subexp start)
  "Replace all matches for REGEXP with REP in STRING.

Return a new string containing the replacements.

Optional arguments FIXEDCASE, LITERAL and SUBEXP are like the
arguments with the same names of function `replace-match'.  If START
is non-nil, start replacements at that index in STRING.

REP is either a string used as the NEWTEXT arg of `replace-match' or a
function.  If it is a function it is applied to each match to generate
the replacement passed to `replace-match'; the match-data at this
point are such that match 0 is the function's argument.

To replace only the first match (if any), make REGEXP match up to \\'
and replace a sub-expression, e.g.
  (replace-regexp-in-string \"\\\\(foo\\\\).*\\\\'\" \"bar\" \" foo foo\" nil nil 1)
    => \" bar foo\"
"

  ;; To avoid excessive consing from multiple matches in long strings,
  ;; don't just call `replace-match' continually.  Walk down the
  ;; string looking for matches of REGEXP and building up a (reversed)
  ;; list MATCHES.  This comprises segments of STRING which weren't
  ;; matched interspersed with replacements for segments that were.
  ;; [For a `large' number of replacements it's more efficient to
  ;; operate in a temporary buffer; we can't tell from the function's
  ;; args whether to choose the buffer-based implementation, though it
  ;; might be reasonable to do so for long enough STRING.]
  (let ((l (length string))
        (start (or start 0))
        matches str mb me)
    (save-match-data
      (while (and (< start l) (string-match regexp string start))
        (setq mb (match-beginning 0)
              me (match-end 0))
        ;; If we matched the empty string, make sure we advance by one char
        (when (= me mb) (setq me (min l (1+ mb))))
        ;; Generate a replacement for the matched substring.
        ;; Operate only on the substring to minimize string consing.
        ;; Set up match data for the substring for replacement;
        ;; presumably this is likely to be faster than munging the
        ;; match data directly in Lisp.
        (string-match regexp (setq str (substring string mb me)))
        (setq matches
              (cons (replace-match (if (stringp rep)
                                       rep
                                     (funcall rep (match-string 0 str)))
                                   fixedcase literal str subexp)
                    (cons (substring string start mb) ; unmatched prefix
                          matches)))
        (setq start me))
      ;; Reconstruct a string from the pieces.
      (setq matches (cons (substring string start l) matches)) ; leftover
      (apply #'concat (nreverse matches)))))

;; This is built-in in Emacs 21.
(defun keywordp (object)
  "Return t if OBJECT is a keyword.
This means that it is a symbol with a print name beginning with `:'
interned in the initial obarray."
  (and (symbolp object) (string-match "^:" (symbol-name object)) t))

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'subr-21)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; subr-21.el ends here
