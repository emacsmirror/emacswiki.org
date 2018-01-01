;;; character-fold+.el --- Extensions to `character-fold.el'
;;
;; Filename: character-fold+.el
;; Description: Extensions to `character-fold.el'
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2015-2018, Drew Adams, all rights reserved.
;; Created: Fri Nov 27 09:12:01 2015 (-0800)
;; Version: 0
;; Package-Requires: ()
;; Last-Updated: Mon Jan  1 10:11:21 2018 (-0800)
;;           By: dradams
;;     Update #: 101
;; URL: https://www.emacswiki.org/emacs/download/character-fold%2b.el
;; Doc URL: https://emacswiki.org/emacs/CharacterFoldPlus
;; Keywords: isearch, search, unicode
;; Compatibility: GNU Emacs: 25.x builds ON OR BEFORE 2015-12-10 
;;
;; Features that might be required by this library:
;;
;;   `backquote', `button', `bytecomp', `cconv', `character-fold',
;;   `cl-extra', `cl-lib', `help-mode', `macroexp'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  Extensions to Isearch character folding.
;;
;;
;;  NOTE: This library is NOT UP-TO-DATE WRT EMACS 25.  The vanilla
;;        Emacs library `character-fold.el', which this library
;;        extends, was changed in incompatible ways after this library
;;        was written.  I have not yet had a chance to update this
;;        (and am waiting for Emacs 25 to be released to do so).
;;        Sorry about that.
;;
;;
;;  Choose One-Way or Symmetric Character Folding
;;  ---------------------------------------------
;;
;;  Non-nil option `char-fold-symmetric' means that char folding is
;;  symmetric: When you search for any of an equivalence class of
;;  characters you find all of them.  This behavior applies to
;;  query-replacing also - see option `replace-character-fold'.
;;
;;  The default value of `char-fold-symmetric' is `nil', which gives
;;  the same behavior as vanilla Emacs: you find all members of the
;;  equivalence class only when you search for the base character.
;;
;;  For example, with a `nil' value you can search for "e" (a base
;;  character) to find "é", but not vice versa.  With a non-`nil'
;;  value you can search for either, to find itself and the other
;;  members of the equivalence class - the base char is not treated
;;  specially.
;;
;;  Example non-`nil' behavior:
;;
;;    Searching for any of these characters and character compositions
;;    in the search string finds all of them.  (Use `C-u C-x =' with
;;    point before a character to see complete information about it.)
;;
;;      e 𝚎 𝙚 𝘦 𝗲 𝖾 𝖊 𝕖 𝔢 𝓮 𝒆 𝑒 𝐞 ｅ ㋎ ㋍ ⓔ ⒠
;;      ⅇ ℯ ₑ ẽ ẽ ẻ ẻ ẹ ẹ ḛ ḛ ḙ ḙ ᵉ ȩ ȩ ȇ ȇ
;;      ȅ ȅ ě ě ę ę ė ė ĕ ĕ ē ē ë ë ê ê é é è è
;;
;;    An example of a composition is "é".  Searching for that finds
;;    the same matches as searching for "é" or searching for "e".
;;
;;  If you also use library `isearch+.el' then you can toggle option
;;  `char-fold-symmetric' anytime during Isearch, using `M-s ='
;;  (command `isearchp-toggle-symmetric-char-fold').
;;
;;
;;  NOTE:
;;
;;    To customize option `char-fold-symmetric', use either Customize
;;    or a Lisp function designed for customizing options, such as
;;    `customize-set-variable', that invokes the necessary `:set'
;;    function.
;;
;;
;;  CAVEAT:
;;
;;    Be aware that character-fold searching can be much slower when
;;    symmetric - there are many more possibilities to search for.
;;    If, for example, you search only for a single "e"-family
;;    character then every "e" in the buffer is a search hit (which
;;    means lazy-highlighting them all, by default).  Searching with a
;;    longer search string is much faster.
;;
;;    If you also use library `isearch+.el' then you can turn off lazy
;;    highlighting using the toggle key `M-s h L'.  This can vastly
;;    improve performance when character folding is symmetric.
;;
;;
;;  Customize the Ad Hoc Character Foldings
;;  ---------------------------------------
;;
;;  In addition to the standard equivalence classes of a base
;;  character and its family of diacriticals, vanilla Emacs includes a
;;  number of ad hoc character foldings, e.g., for different quote
;;  marks.
;;
;;  Option `char-fold-ad-hoc' lets you customize this set of ad hoc
;;  foldings.  The default value is the same set provided by vanilla
;;  Emacs.
;;
;;
;;
;;  Options defined here:
;;
;;    `char-fold-ad-hoc', `char-fold-symmetric'.
;;
;;  Non-interactive functions defined here:
;;
;;    `update-char-fold-table'.
;;
;;  Internal variables defined here:
;;
;;    `char-fold-decomps'.
;;
;;
;;  ***** NOTE: The following function defined in `mouse.el' has
;;              been ADVISED HERE:
;;
;;    `character-fold-to-regexp'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2015/12/01 dadams
;;     char-fold-ad-hoc: Added :set.
;; 2015/11/28 dadams
;;     Added: char-fold-ad-hoc.
;;     update-char-fold-table: Use char-fold-ad-hoc.
;; 2015/11/27 dadams
;;     Created.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'character-fold)

;;;;;;;;;;;;;;;;;;;;;;;

(defvar char-fold-decomps ()
  "List of conses of a decomposition and its base char.")

(defun update-char-fold-table ()
  "Update the value of variable `character-fold-table'.
The new value reflects the current value of `char-fold-symmetric'."
  (setq char-fold-decomps  ())
  (setq character-fold-table
        (let* ((equiv  (make-char-table 'character-fold-table))
               (table  (unicode-property-table-internal 'decomposition))
               (func   (char-table-extra-slot table 1)))
          ;; Ensure that the table is populated.
          (map-char-table (lambda (ch val) (when (consp ch) (funcall func (car ch) val table))) table)
          ;; Compile a list of all complex chars that each simple char should match.
          (map-char-table
           (lambda (ch dec)
             (when (consp dec)
               (when (symbolp (car dec)) (setq dec  (cdr dec))) ; Discard a possible formatting tag.
               ;; Skip trivial cases like ?a decomposing to (?a).
               (unless (and (null (cdr dec))  (eq ch (car dec)))
                 (let ((dd           dec)
                       (fold-decomp  t)
                       kk found)
                   (while (and dd  (not found))
                     (setq kk  (pop dd))
                     ;; Is KK a number or letter, per unicode standard?
                     (setq found  (memq (get-char-code-property kk 'general-category)
                                        '(Lu Ll Lt Lm Lo Nd Nl No))))
                   (if found
                       ;; Check if the decomposition has more than one letter, because then
                       ;; we don't want the first letter to match the decomposition.
                       (dolist (kk  dd)
                         (when (and fold-decomp  (memq (get-char-code-property kk 'general-category)
                                                       '(Lu Ll Lt Lm Lo Nd Nl No)))
                           (setq fold-decomp  nil)))
                     ;; No number or letter on decomposition.  Take its first char.
                     (setq found  (car-safe dec)))
                   ;; Fold a multi-char decomposition only if at least one of the chars is
                   ;; non-spacing (combining).
                   (when fold-decomp
                     (setq fold-decomp  nil)
                     (dolist (kk  dec)
                       (when (and (not fold-decomp)
                                  (> (get-char-code-property kk 'canonical-combining-class) 0))
                         (setq fold-decomp  t))))
                   ;; Add II to the list of chars that KK can represent.  Maybe add its decomposition
                   ;; too, so we can match multi-char representations like (format "a%c" 769).
                   (when (and found  (not (eq ch kk)))
                     (let ((chr-strgs  (cons (char-to-string ch) (aref equiv kk))))
                       (aset equiv kk (if fold-decomp
                                          (cons (apply #'string dec) chr-strgs)
                                        chr-strgs))))))))
           table)
          ;; Add some manual entries.
          (dolist (it  char-fold-ad-hoc)
            (let ((idx        (car it))
                  (chr-strgs  (cdr it)))
              (aset equiv idx (append chr-strgs (aref equiv idx)))))

          ;; This is the essential bit added by `character-fold+.el'.
          (when (and (boundp 'char-fold-symmetric)  char-fold-symmetric)
            ;; Add an entry for each equivalent char.
            (let ((others  ()))
              (map-char-table
               (lambda (base val)
                 (let ((chr-strgs  (aref equiv base)))
                   (when (consp chr-strgs)
                     (dolist (strg  (cdr chr-strgs))
                       (when (< (length strg) 2)
                         (push (cons (string-to-char strg) (remove strg chr-strgs)) others))
                       ;; Add it and its base char to `char-fold-decomps'.
                       (push (cons strg (char-to-string base)) char-fold-decomps)))))
               equiv)
              (dolist (it  others)
                (let ((base       (car it))
                      (chr-strgs  (cdr it)))
                  (aset equiv base (append chr-strgs (aref equiv base)))))))

          (map-char-table ; Convert the lists of characters we compiled into regexps.
           (lambda (ch val) (let ((re  (regexp-opt (cons (char-to-string ch) val))))
                        (if (consp ch) (set-char-table-range equiv ch re) (aset equiv ch re))))
           equiv)
          equiv)))

(defcustom char-fold-ad-hoc '((?\" "＂" "“" "”" "”" "„" "⹂" "〞" "‟" "‟" "❞" "❝"
                               "❠" "“" "„" "〝" "〟" "🙷" "🙶" "🙸" "«" "»")
                              (?' "❟" "❛" "❜" "‘" "’" "‚" "‛" "‚" "󠀢" "❮" "❯" "‹" "›")
                              (?` "❛" "‘" "‛" "󠀢" "❮" "‹"))
  "Ad hoc character foldings.
Each entry is a list of a character and the strings that fold into it.

The default value includes those ad hoc foldings provided by vanilla
Emacs."
  :set (lambda (sym defs)
         (custom-set-default sym defs)
         (update-char-fold-table))
  :type '(repeat (cons
                  (character :tag "Fold to character")
                  (repeat (string :tag "Fold from string"))))
  :group 'isearch)

(defcustom char-fold-symmetric nil
  "Non-nil means char-fold searching treats equivalent chars the same.
That is, use of any of a set of char-fold equivalent chars in a search
string finds any of them in the text being searched.

If nil then only the \"base\" or \"canonical\" char of the set matches
any of them.  The others match only themselves, even when char-folding
is turned on."
  :set (lambda (sym defs)
         (custom-set-default sym defs)
         (update-char-fold-table))
  :type 'boolean :group 'isearch)

(defadvice character-fold-to-regexp (before replace-decompositions activate)
  "Replace any decompositions in `character-fold-table' by their base chars.
This allows search to match all equivalents."
  (when char-fold-decomps
    (dolist (decomp  char-fold-decomps)
      (ad-set-arg 0  (replace-regexp-in-string (regexp-quote (car decomp)) (cdr decomp)
                                               (ad-get-arg 0) 'FIXED-CASE 'LITERAL)))))
;;;;;;;;;;;;;;;;;;;;;;;

(provide 'character-fold+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; character-fold+.el ends here
