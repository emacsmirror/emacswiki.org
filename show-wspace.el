;;; show-wspace.el --- Highlight specified sets of characters, including whitespace.
;;
;; Filename: show-wspace.el
;; Description: Highlight specified sets of characters, including whitespace.
;; Author: Drew Adams, Peter Steiner <unistein@isbe.ch>
;; Maintainer: Drew Adams
;; Copyright (C) 2000-2012, Drew Adams, all rights reserved.
;; Created: Wed Jun 21 08:54:53 2000
;; Version: 21.0
;; Last-Updated: Thu Aug 23 16:54:33 2012 (-0700)
;;           By: dradams
;;     Update #: 720
;; URL: http://www.emacswiki.org/emacs-en/show-wspace.el
;; Doc URL: http://emacswiki.org/emacs/ShowWhiteSpace
;; Keywords: highlight, whitespace, characters, Unicode
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x, 24.x
;;
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Highlight specified sets of characters, including whitespace.
;;
;; Originally this library was only for highlighting whitespace, hence
;; the file name.  Now you can use it to highlight any set of
;; characters.  The main use case probably remains highlighting
;; whitespace or other easily confused characters, so I have kept the
;; same file name.
;;
;; This library provides commands and non-interactive functions for
;; highlighting the following:
;;
;; * Tab chars (command `ws-toggle-highlight-tabs').
;;
;; * Hard space (aka no-break space, aka non-breaking space) chars
;;   (command `ws-toggle-highlight-hard-spaces').
;;
;; * Hard hyphen (aka non-breaking hyphen) chars (command
;;   `ws-toggle-highlight-hard-hyphens').
;;
;; * Trailing whitespace: tabs, spaces, and hard spaces at the end of
;;   a line of text (command
;;   `ws-toggle-highlight-trailing-whitespace')
;;
;; * Any set of chars you choose (commands `ws-highlight-chars' and
;;   `ws-toggle-highlight-other-chars').  You can specify characters
;;   in four ways: (1) individually, (2) using ranges, (3) using
;;   character classes (e.g. [:digit:]), and (4) using character sets
;;   (e.g. `iso-8859-1' or `lao').
;;
;;   - Command `ws-highlight-chars' prompts you for the characters to
;;     highlight and the face to use.  With a prefix arg it
;;     unhighlights.
;;
;;   - Command `ws-toggle-highlight-other-chars' toggles highlighting
;;     of the characters specified in user option `ws-other-chars',
;;     using face `ws-other-char'.
;;
;;   For these particular commands and functions, option
;;   `ws-other-chars-font-lock-override' controls whether the current
;;   highlighting face overrides (`t'), is overridden by (`keep'), or
;;   merges with (`append' or `prepend') any existing highlighting.
;;
;; To use this library, add this to your init file (~/.emacs):
;;
;;      (require 'show-wspace) ; Load this library.
;;
;; You can then use the commands and functions defined here to turn
;; the various kinds of highlighting on and off when in Font-Lock
;; mode.  For example, you can bind a key to toggle highlighting of
;; trailing whitespace:
;;
;;      (global-set-key (kbd "<f11>")
;;                      'ws-toggle-highlight-trailing-whitespace)
;;
;; Because variable `font-lock-keywords' is buffer-local, that key
;; binding lets you use `f11' to toggle highlighting separately in
;; each buffer.
;;
;; But if you want to use a particular kind of highlighting by default
;; globally, then just add the corresponding `ws-highlight-*' function
;; to the hook `font-lock-mode-hook'.  Then, whenever Font-Lock mode
;; is turned on (in any buffer), the appropriate highlighting will
;; also be turned on.
;;
;; For example, you can turn on tab highlighting everywhere by default
;; by adding function `ws-highlight-tabs' to `font-lock-mode-hook' in
;; your init file (`~/.emacs'), as follows:
;;
;;     (add-hook 'font-lock-mode-hook 'ws-highlight-tabs)
;;
;; In addition to buffer-specific highlighting and global
;; highlighting, you can turn on a given kind of highlighting
;; automatically for all buffers that are in a certain major mode.
;;
;; For that, do the following, where `THE-MODE' is the appropriate
;; mode symbol (value of variable `major-mode'), such as `text-mode'.
;; This example turns on trailing whitespace highlighting - use
;; different `ws-highlight-*' and `ws-dont-highlight-*' functions for
;; other kinds of highlighting.
;;
;;      (add-hook 'change-major-mode-hook
;;                (lambda ()
;;                  (add-hook 'font-lock-mode-hook
;;                            'ws-highlight-trailing-whitespace)))
;
;;      (add-hook 'after-change-major-mode-hook
;;                (lambda ()
;;                  (when (eq major-mode 'THE-MODE)
;;                    (remove-hook 'font-lock-mode-hook
;;                                 'ws-highlight-trailing-whitespace)
;;                    (ws-dont-highlight-trailing-whitespace)))
;;                'APPEND)
;;
;;
;; Vanilla Emacs Highlighting of Hard Spaces and Hyphens
;; -----------------------------------------------------
;;
;; Vanilla Emacs can itself highlight hard spaces and hard hyphens,
;; and it does so whenever `nobreak-char-display' is non-nil, which it
;; is by default.  By "hard" space and hyphen I mean "no-break" or
;; non-breaking.  These are the non-ASCII Unicode characters with code
;; points 160 (#xa0) and 8209 (#x2011), respectively.
;;
;; This low-level vanilla Emacs highlighting does not use Font Lock
;; mode, and it cannot highlight only one of these characters and not
;; the other.
;;
;; Using `show-wspace.el' to highlight hard space and hyphen chars
;; requires turning off their default highlighting provided by vanilla
;; Emacs, that is, setting `nobreak-char-display' to nil.  This is
;; done automatically by the functions defined here.  Similarly, when
;; you turn off their font-lock highlighting, the vanilla highlighting
;; is automatically restored: `nobreak-char-display' is reset to its
;; last saved non-nil value.
;;
;; NOTE: Emacs bug #12054 currently defeats the highlighting of hard
;; spaces in Emacs 23+.
;;
;; NOTE: If you byte-compile this file in an older version of Emacs
;; (prior to Emacs 23) then the code for highlighting hard hyphens
;; will not work, even in Emacs 23+.  If you use Emacs 23+ then you
;; should either byte-compile it using Emacs 23+ or evaluate the
;; source code that defines `ws-highlight-hard-hyphens' and
;; `ws-dont-highlight-hard-hyphens'.  (This is because older Emacs
;; versions interpret [\u2011] as just [u2011].)
;;
;;
;; See Also:
;;
;; * Library `whitespace.el' for other ways to highlight whitespace.
;;
;;   Starting with Emacs 24, vanilla Emacs also provides library
;;   `whitespace.el', which does some things similar to what
;;   `show-wspace.el' does, plus a whole lot more.  It is also
;;   somewhat complicated to use (and it seems to have more than a few
;;   bugs).  It is definitely worth trying, and it might well do
;;   everything that you want.
;;
;;   Besides being much simpler, I think that `show-wspace.el' has an
;;   advantage of letting you easily highlight ONLY particular
;;   characters.  `whitespace.el' apparently makes you pick whether to
;;   highlight spaces and hard spaces together, or not, for instance.
;;
;;   (With `whitespace.el' you can get the effect of highlighting only
;;   one of these kinds of space, as a workaround, by customizing the
;;   face used to highlight the other one so that it is the same as
;;   the `default' face.  Maybe I'm missing something, but that seems
;;   to me the only way to do it.)
;;
;;   Nevertheless, `whitespace.el' seems promising, and given its
;;   complexity there might well be a way to do nearly everything you
;;   want.  You might nevertheless find `show-wspace.el' useful.
;;
;; * Library `highlight.el' for ways to highlight text more generally,
;;   not just specific characters.  It is available here:
;;   http://www.emacswiki.org/cgi-bin/wiki/highlight.el     (code)
;;   http://www.emacswiki.org/cgi-bin/wiki/HighlightLibrary (doc)
;;
;;
;; Faces defined here:
;;
;;    `ws-hard-hyphen' (Emacs 23+), `ws-hard-space', `ws-other-char',
;;    `ws-tab', `ws-trailing-whitespace'.
;;
;; User options defined here:
;;
;;    `ws-other-chars', `ws-other-chars-font-lock-override'.
;;
;; Commands defined here:
;;
;;    `toggle-highlight-hard-hyphens' (alias, Emacs 23+),
;;    `toggle-highlight-hard-spaces' (alias),
;;    `toggle-highlight-other-chars', `toggle-highlight-tabs' (alias),
;;    `toggle-highlight-trailing-whitespace' (alias),
;;    `ws-highlight-chars', `ws-toggle-highlight-hard-hyphens' (Emacs
;;    23+), `ws-toggle-highlight-hard-spaces',
;;    `ws-toggle-highlight-other-chars', `ws-toggle-highlight-tabs',
;;    `ws-toggle-highlight-trailing-whitespace'.
;;
;; Non-interactive functions defined here:
;;
;;    `ws-dont-highlight-hard-hyphens' (Emacs 23+),
;;    `ws-dont-highlight-hard-spaces',
;;    `ws-dont-highlight-other-chars', `ws-dont-highlight-tabs',
;;    `ws-dont-highlight-trailing-whitespace',
;;    `ws-highlight-hard-hyphens' (Emacs 23+),
;;    `ws-highlight-other-chars', `ws-highlight-hard-spaces',
;;    `ws-highlight-tabs', `ws-highlight-trailing-whitespace',
;;    `ws-other-chars-defcustom-spec',
;;    `ws-other-chars-font-lock-spec'.
;;
;; Internal variables defined here:
;;
;;    `ws-highlight-hard-hyphens-p' (Emacs 23+),
;;    `ws-highlight-hard-spaces-p', `ws-highlight-tabs-p',
;;    `ws-highlight-trailing-whitespace-p',
;;    `ws--saved-nobreak-char-display'.
;;
;;
;; Peter Steiner wrote the original code that did the equivalent of
;; some of the `ws-highlight-*' commands here, in his library
;; `hilite-trail.el'.
;;
;; Drew Adams wrote the `toggle-*' commands, `*-p' variables and all
;; the rest.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2012/07/29 dadams
;;     ws-other-chars-defcustom-spec, ws-highlight-chars, ws-other-chars-font-lock-spec,
;;       ws-toggle-highlight-other-chars:
;;         Handle charsets too now.
;;     ws-other-chars-font-lock-spec: Use regexp-opt-charset for range, for Emacs 22+.
;; 2012/07/28 dadams
;;     Added: ws-highlight-chars, ws-other-chars-font-lock-override.
;;     ws(-dont)-highlight-other-chars, ws-other-chars-font-lock-spec:
;;       Added optional args CHARS and FACE.
;;     ws-other-chars-font-lock-spec: Use ws-other-chars-font-lock-override.
;; 2012/07/26 dadams
;;     Changed prefix from show-ws- to just ws-.  Removed suffix -show-ws from toggle-*.
;;     Added: *-hard-hyphen*, *-other(s)*, ws--saved*.  Renamed *-show* to *-highlight*.
;;     Use 'APPEND with font-lock-add-keywords, so not overridden by other font-locking.
;;     Added note to hard-space functions and vars about Emacs bug #12054.
;;     Factored out font-lock :group to the defgroup.
;;     Added optional MSGP arg for commands - no msg otherwise.
;; 2011/01/04 dadams
;;     Added autoload cookies for defgroup and defface.
;; 2009/06/25 dadams
;;     show-ws-dont-*: Should be no-op's for Emacs 20, 21.
;; 2009/06/17 dadams
;;     Added: show-ws-dont-highlight-*.
;;     show-ws-toggle-show-*: Remove the font-lock keywords. Needed for Emacs 22+.
;; 2007/09/25 dadams
;;     Renamed to use prefix show-ws-.  Thx to Cyril Brulebois.
;; 2006/11/11 dadams
;;     Corrected doc strings.  Clarified: hard space is non-breaking space, ?\240.
;;     Included hard space in highlight-trailing-whitespace.
;; 2006/04/06 dadams
;;     highlight-*: Use font-lock-add-keywords.  Thanks to Karl Chen.
;; 2006/02/20 dadams
;;     Mentioned in Commentary how to use non-interactively.
;; 2006/01/07 dadams
;;     Added :link for sending bug report.
;; 2006/01/06 dadams
;;     Added defgroup and use it.
;; 2005/12/30 dadams
;;     Removed require of def-face-const.el.
;;     Renamed faces, without "-face".
;; 2005/01/25 dadams
;;     Removed ###autoload for defvars.
;; 2004/06/10 dadams
;;     Fixed minor bug in highlight-* functions.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;


;; Quiet the byte compiler.

(defvar nobreak-char-display)

;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defgroup Show-Whitespace nil
  "Highlight specified sets of characters, including whitespace."
  :prefix "ws-"
  :group 'convenience :group 'matching :group 'font-lock
  :link `(url-link :tag "Send Bug Report"
          ,(concat "mailto:" "drew.adams" "@" "oracle" ".com?subject=\
show-wspace.el bug: \
&body=Describe bug here, starting with `emacs -q'.  \
Don't forget to mention your Emacs and library versions."))
  :link '(url-link :tag "Other Libraries by Drew"
          "http://www.emacswiki.org/cgi-bin/wiki/DrewsElispLibraries")
  :link '(url-link :tag "Download"
          "http://www.emacswiki.org/cgi-bin/wiki/show-wspace.el")
  :link '(url-link :tag "Description"
          "http://www.emacswiki.org/cgi-bin/wiki/ShowWhiteSpace#ShowWspace")
  :link '(emacs-commentary-link :tag "Commentary" "show-wspace")
  )

;;;###autoload
(defface ws-tab '((t (:background "LemonChiffon")))
  "*Face for highlighting tab characters (`C-i') in Font-Lock mode."
  :group 'Show-Whitespace :group 'faces)

;;;###autoload
(defface ws-trailing-whitespace '((t (:background "Gold")))
  "*Face for highlighting whitespace at line ends in Font-Lock mode.
This includes tab, space, and hard (non-breaking) space characters."
  :group 'Show-Whitespace :group 'faces)

;;;###autoload
(defface ws-hard-space '((t (:background "Aquamarine")))
  "*Face for highlighting non-breaking spaces (`?\240')in Font-Lock mode."
  :group 'Show-Whitespace :group 'faces)

(when (> emacs-major-version 22)
  (defface ws-hard-hyphen '((t (:background "PaleGreen")))
    "*Face for highlighting non-breaking hyphens (`?\u2011')in Font-Lock mode."
    :group 'Show-Whitespace :group 'faces))

;;;###autoload
(defface ws-other-char '((t (:background "HotPink")))
  "*Face for highlighting chars in `ws-other-chars' in Font-Lock mode."
  :group 'Show-Whitespace :group 'faces)

(defun ws-other-chars-defcustom-spec ()
  "Custom :type spec for `ws-other-chars'."
  (if (> emacs-major-version 21)
      `(repeat
        (choice
         (string :tag "Characters (string)")
         (choice :tag "Character set" ,@(mapcar (lambda (cset) `(const ,cset))
                                                (if (fboundp 'charset-priority-list)
                                                    (charset-priority-list)
                                                  charset-list)))
         (cons   :tag "Character range" (character :tag "From") (character :tag "To"))
         (choice :tag "Character class"
          (const   :tag "ASCII char\t[:ascii:]"                        [:ascii:])
          (const   :tag "Non-ASCII char\t[:nonascii:]"                 [:nonascii:])
          (const   :tag "Word char\t[:word:]"                          [:word:])
          (const   :tag "Letter or digit\t[:alnum:]"                   [:alnum:])
          (const   :tag "Letter\t[:alpha:]"                            [:alpha:])
          (const   :tag "Lowercase letter\t[:lower:]"                  [:lower:])
          (const   :tag "Uppercase letter\t[:upper:]"                  [:upper:])
          (const   :tag "Digit\t[:digit:]"                             [:digit:])
          (const   :tag "Hex digit\t[:xdigit:]"                        [:xdigit:])
          (const   :tag "Punctuation char (non-word char)\t[:punct:]"  [:punct:])
          (const   :tag "Space or tab char\t[:blank:]"                 [:blank:])
          (const   :tag "Whitespace char\t[:space:]"                   [:space:])
          (const   :tag "Control char\t[:cntrl:]"                      [:cntrl:])
          (const   :tag "Not control or delete char\t[:print:]"        [:print:])
          (const   :tag "Not control, space or delete char\t[:graph:]" [:graph:])
          (const   :tag "Multibyte char\t[:multibyte:]"                [:multibyte:])
          (const   :tag "Unibyte char\t[:unibyte:]"                    [:unibyte:]))))
    '(repeat
      (choice
       (string :tag "Characters (string)")
       (cons :tag "Character range" (character :tag "From") (character :tag "To"))))))

;;;###autoload
(defcustom ws-other-chars ()
  "*Characters to highlight using face `ws-other-char'.
A list of entries, each of which can be any of these:
 * a string of individual characters
 * a character range, represented as a cons (FROM . TO),
   where FROM and TO are both included
 * a character class, such as [:nonascii:]
 * a character set, such as `iso-8859-1' or `lao'

The last two alternatives are available only for Emacs 22 and later.

For the first alternative, remember that you can insert any character
into the string using `C-q', and (for Emacs 23 and later) you can
insert any Unicode character using `C-x 8 RET'.

For Emacs 20, the first alternative is not well supported: Do not use
chars that are special within a regexp character alternative (i.e.,
\[...]).  In Emacs 20, the string you specify is simply wrapped with
\[...], which is not correct for all chars."
  :type (ws-other-chars-defcustom-spec) :group 'Show-Whitespace)

(defcustom ws-other-chars-font-lock-override 'append
  "*How to highlighting for other chars interacts with existing highlighting.
The values correspond to the values for an OVERRIDE spec in
`font-lock-keywords'.  See (elisp) `Search-based Fontification'.

This affects commands `ws-toggle-highlight-other-chars' and
 `ws-highlight-chars', and functions `ws-highlight-other-chars' and
 `ws-dont-highlight-other-chars'."
  :type '(choice
          (const :tag "Do not override existing highlighting (`keep')" keep)
          (const :tag "Merge after existing highlighting (`append')"   append)
          (const :tag "Merge before existing highlighting (`prepend')" prepend)
          (const :tag "Replace (override) existing highlighting"       t))
  :group 'Show-Whitespace)

;;;###autoload
(defvar ws-highlight-tabs-p nil
  "Non-nil means font-lock mode highlights TAB characters (`C-i').")

(defvar ws-highlight-trailing-whitespace-p nil
  "Non-nil means font-lock mode highlights whitespace at line ends.
This includes tab, space, and hard (non-breaking) space characters.")

(defvar ws-highlight-hard-spaces-p nil
  "Non-nil means font-lock mode highlights non-breaking spaces (`?\240').
Note: Emacs bug #12054 currently defeats the highlighting of hard
spaces in Emacs 23+.")

(when (> emacs-major-version 22)
  (defvar ws-highlight-hard-hyphens-p nil
    "Non-nil means font-lock mode highlights non-breaking hyphens (`?\u2011')."))

(defvar ws-highlight-other-chars-p nil
  "Non-nil means font-lock mode highlights the chars in `ws-other-chars'.")

(when (boundp 'nobreak-char-display)
  (defvar ws--saved-nobreak-char-display nobreak-char-display
    "Saved value of `nobreak-char-display', so that it can be restored.
Whenever you turn on `show-wspace' highlighting of hard spaces or hard
hyphens, if the value of `nobreak-char-display' is non-nil then this
is set to that value and `nobreak-char-display' is set to nil."))

;;;###autoload
(defun ws-highlight-chars (chars face &optional offp msgp)
  "Read a string of CHARS, read a FACE name, then highlight the CHARS.
With a prefix arg, unhighlight the CHARS.

In addition to being just a string of characters, CHARS can be any
of the following (the last two are only for Emacs 22+):

* A cons (C1 C2), where C1 and C2 are characters, that is, integers,
  which you can represent using character notation.  This represents
  the range of characters from C1 through C2.

  For example, you would enter `(?a . ?b)' to mean the characters from
  `a' through `g', inclusive.  Note that you enter the parentheses and
  the dot, and you can use character read syntax (e.g., `?a' for `a').

* A character class, such as `[:digit:]'.  This matches all characters
  in the class.  You must type the brackets and colons (`:').  (This
  possibility is available only for Emacs 22 and later.)

* A character set, such as `iso-8859-1' or `lao'.  This matches all
  characters in the set.

If you mistype CHARS in one of the above representations, then what
you type is interpreted as just a string of the characters to
highlight.  For example, if you mean to type `[:digit:]' but type
`[:digit]', then characters [, ], :, d, g, i, and t are highlighted."
  (interactive
   (let* ((prompt  (format "Chars to %shighlight: " (if current-prefix-arg "UN" "")))
          (chrs    (read-string prompt))
          (chrs    (progn (while (string= "" chrs)
                            (setq chrs  (read-string (concat (substring prompt 0 -2)
                                                             " (not empty): "))))
                          chrs))
          (face    (read-face-name "Face: ")))
     (list (let ((cs  (condition-case nil  (read chrs)  (error nil))))
             (cond
               ;; Charset
               ((and (fboundp 'charsetp)  (charsetp cs))
                (list cs))
               ;; Character range: (?a . ?g)
               ((and (consp cs)
                     (if (fboundp 'characterp)
                         (characterp (car cs))
                       (integerp (car cs)))
                     (if (fboundp 'characterp)
                         (characterp (cdr cs))
                       (integerp (cdr cs))))
                (list cs))
               ;; Character class: [:digit:]
               ((and (> emacs-major-version 21) ; No char classes < Emacs 22.
                     (vectorp cs)  (= 1 (length cs))  (keywordp (aref cs 0))
                     (let ((name  (symbol-name (aref cs 0))))
                       (eq ?: (aref name (1- (length name))))))
                (list cs))
               ;; Separate characters: abcd
               (t (list chrs))))
           face
           current-prefix-arg
           t)))
  (if (not offp)
      (add-hook 'font-lock-mode-hook
                `(lambda () (ws-highlight-other-chars ',chars ',face))
                'APPEND)
    (remove-hook 'font-lock-mode-hook
                 `(lambda () (ws-highlight-other-chars ',chars ',face)))
    (ws-dont-highlight-other-chars chars face))
  (font-lock-mode) (font-lock-mode)
  (when msgp
    (message "Highlighting of %s with face `%s' is now %s."
             (mapconcat
              (lambda (chars)
                (cond ((and (consp chars)
                            (if (fboundp 'characterp)
                                (characterp (car chars))
                              (integerp (car chars)))
                            (if (fboundp 'characterp)
                                (characterp (cdr chars))
                              (integerp (cdr chars))))
                       (let ((range  (format "%s to %s"
                                             (char-to-string (car chars))
                                             (char-to-string (cdr chars)))))
                         (if (fboundp 'propertize)
                             (format "`%s'" (propertize range 'face face))
                           range)))
                      ((and (fboundp 'charsetp)  (charsetp chars))
                       (format "charset `%s'" chars))
                      ((vectorp chars)
                       (if (fboundp 'propertize)
                           (format "`%s'" (propertize (format "%s" chars) 'face face))
                         (format "%s" chars)))
                      ((stringp chars)
                       (mapconcat
                        (if (fboundp 'propertize)
                            (lambda (chr) (propertize (char-to-string chr) 'face face))
                          #'char-to-string)
                        chars
                        ", "))))
              chars
              ", ")
             face
             (if offp "OFF" "ON"))))

;;;###autoload
(defalias 'toggle-highlight-tabs 'ws-toggle-highlight-tabs)
;;;###autoload
(defun ws-toggle-highlight-tabs (&optional msgp)
  "Toggle highlighting of TABs, using face `ws-tab'."
  (interactive "p")
  (setq ws-highlight-tabs-p  (not ws-highlight-tabs-p))
  (if ws-highlight-tabs-p
      (add-hook 'font-lock-mode-hook 'ws-highlight-tabs)
    (remove-hook 'font-lock-mode-hook 'ws-highlight-tabs)
    (ws-dont-highlight-tabs))
  (font-lock-mode) (font-lock-mode)
  (when msgp
    (message "TAB highlighting is now %s." (if ws-highlight-tabs-p "ON" "OFF"))))

;;;###autoload
(defalias 'toggle-highlight-hard-spaces 'ws-toggle-highlight-hard-spaces)
;;;###autoload
(defun ws-toggle-highlight-hard-spaces (&optional msgp)
  "Toggle highlighting of non-breaking space characters (`?\240').
Uses face `ws-hard-space'.
Note: Emacs bug #12054 currently defeats the highlighting of hard
spaces in Emacs 23+."
  (interactive "p")
  (setq ws-highlight-hard-spaces-p  (not ws-highlight-hard-spaces-p))
  (cond (ws-highlight-hard-spaces-p
         ;; Do this in `ws-highlight-hard-spaces', instead:
         ;; (when (and (boundp 'nobreak-char-display)  nobreak-char-display)
         ;;   (setq ws--saved-nobreak-char-display  nobreak-char-display
         ;;         nobreak-char-display            nil))
         (add-hook 'font-lock-mode-hook 'ws-highlight-hard-spaces))
        (t
         ;; Do this in `ws-dont-highlight-hard-spaces', instead:
         ;; (when (and (boundp 'nobreak-char-display)  (not nobreak-char-display))
         ;;   (setq nobreak-char-display  ws--saved-nobreak-char-display))
         (remove-hook 'font-lock-mode-hook 'ws-highlight-hard-spaces)
         (ws-dont-highlight-hard-spaces)))
  (font-lock-mode) (font-lock-mode)
  (when msgp (message "Hard (non-breaking) space highlighting is now %s."
                      (if ws-highlight-hard-spaces-p "ON" "OFF"))))

(when (> emacs-major-version 22)
  (defalias 'toggle-highlight-hard-hyphens 'ws-toggle-highlight-hard-hyphens)
  (defun ws-toggle-highlight-hard-hyphens (&optional msgp)
    "Toggle highlighting of non-breaking hyphen characters (`?\u2011').
Uses face `ws-hyphen-space'."
    (interactive "p")
    (setq ws-highlight-hard-hyphens-p  (not ws-highlight-hard-hyphens-p))
    (cond (ws-highlight-hard-hyphens-p
           ;; Do this in `ws-highlight-hard-hyphens', instead:
           ;; (when (and (boundp 'nobreak-char-display)  nobreak-char-display)
           ;;   (setq ws--saved-nobreak-char-display  nobreak-char-display
           ;;         nobreak-char-display            nil))
           (add-hook 'font-lock-mode-hook 'ws-highlight-hard-hyphens))
          (t
           ;; Do this in `ws-dont-highlight-hard-hyphens', instead:
           ;; (unless nobreak-char-display
           ;;   (setq nobreak-char-display  ws--saved-nobreak-char-display))
           (remove-hook 'font-lock-mode-hook 'ws-highlight-hard-hyphens)
           (ws-dont-highlight-hard-hyphens)))
    (font-lock-mode) (font-lock-mode)
    (when msgp (message "Hard (non-breaking) hyphen highlighting is now %s."
                        (if ws-highlight-hard-hyphens-p "ON" "OFF")))))

;;;###autoload
(defalias 'toggle-highlight-other-chars 'ws-toggle-highlight-other-chars)
;;;###autoload
(defun ws-toggle-highlight-other-chars (&optional msgp)
  "Toggle highlighting chars in `ws-other-chars', using face `ws-other-char'."
  (interactive "p")
  (when (and (not ws-other-chars)  msgp)
    (error "No chars in `ws-other-chars' to highlight"))
  (setq ws-highlight-other-chars-p  (not ws-highlight-other-chars-p))
  (cond (ws-highlight-other-chars-p
         ;; Do this in `ws-highlight-other-chars', instead:
         ;; (when (and (boundp 'nobreak-char-display)  nobreak-char-display)
         ;;   (setq ws--saved-nobreak-char-display  nobreak-char-display
         ;;         nobreak-char-display            nil))
         (add-hook 'font-lock-mode-hook 'ws-highlight-other-chars))
        (t
         ;; Do this in `ws-dont-highlight-other-chars', instead:
         ;; (when (and (boundp 'nobreak-char-display)  (not nobreak-char-display))
         ;;   (setq nobreak-char-display  ws--saved-nobreak-char-display))
         (remove-hook 'font-lock-mode-hook 'ws-highlight-other-chars)
         (ws-dont-highlight-other-chars)))
  (font-lock-mode) (font-lock-mode)
  (when msgp
    (message "Highlighting of %s is now %s."
             (mapconcat
              (lambda (chars)
                (cond ((and (fboundp 'charsetp)  (charsetp chars))
                       (format "character set `%s'" chars))
                      ((and (consp chars)
                        (if (fboundp 'characterp)
                                (characterp (car chars))
                              (integerp (car chars)))
                            (if (fboundp 'characterp)
                                (characterp (cdr chars))
                              (integerp (cdr chars))))
                       (format "%s to %s"
                               (char-to-string (car chars))
                               (char-to-string (cdr chars))))
                      ((vectorp chars) (format "%s" chars))
                      ((stringp chars) (mapconcat #'char-to-string chars ", "))))
              ws-other-chars
              ", ")
             (if ws-highlight-other-chars-p "ON" "OFF"))))

;;;###autoload
(defalias 'toggle-highlight-trailing-whitespace
    'ws-toggle-highlight-trailing-whitespace)
;;;###autoload
(defun ws-toggle-highlight-trailing-whitespace (&optional msgp)
  "Toggle highlighting of trailing whitespace.
This includes tab, space, and hard (non-breaking) space characters.
Uses face `ws-trailing-whitespace'."
  (interactive "p")
  (setq ws-highlight-trailing-whitespace-p  (not ws-highlight-trailing-whitespace-p))
  (if ws-highlight-trailing-whitespace-p
      (add-hook 'font-lock-mode-hook 'ws-highlight-trailing-whitespace)
    (remove-hook 'font-lock-mode-hook 'ws-highlight-trailing-whitespace)
    (ws-dont-highlight-trailing-whitespace))
  (font-lock-mode) (font-lock-mode)
  (when msgp (message "Trailing whitespace highlighting is now %s."
                      (if ws-highlight-trailing-whitespace-p "ON" "OFF"))))

(defun ws-highlight-tabs ()
  "Highlight tab characters (`C-i')."
  (font-lock-add-keywords nil '(("[\t]+" (0 'ws-tab t))) 'APPEND))

(defun ws-highlight-hard-spaces ()
  "Highlight hard (non-breaking) space characters (`?\240').
This also sets `nobreak-char-display' to nil, to turn off its
low-level, vanilla highlighting.  It saves the previous value of
`nobreak-char-display' in `ws--saved-nobreak-char-display'.

Note: Emacs bug #12054 currently defeats the highlighting of hard
spaces in Emacs 23+."
  (when (and (boundp 'nobreak-char-display)  nobreak-char-display)
    (setq ws--saved-nobreak-char-display  nobreak-char-display
          nobreak-char-display            nil))
  (font-lock-add-keywords nil '(("[\240]+" (0 'ws-hard-space t))) 'APPEND))

(when (> emacs-major-version 22)
  (defun ws-highlight-hard-hyphens ()
    "Highlight hard (non-breaking) hyphen characters (`?\u2011').
This also sets `nobreak-char-display' to nil, to turn off its
low-level, vanilla highlighting.  It saves the previous value of
`nobreak-char-display' in `ws--saved-nobreak-char-display'."
    (when (and (boundp 'nobreak-char-display)  nobreak-char-display)
      (setq ws--saved-nobreak-char-display  nobreak-char-display
            nobreak-char-display            nil))
    (font-lock-add-keywords nil '(("[\u2011]+" (0 'ws-hard-hyphen t))) 'APPEND)))

(defun ws-highlight-trailing-whitespace ()
  "Highlight whitespace characters at line ends.
This includes tab, space, and hard (non-breaking) space characters."
  (font-lock-add-keywords
   nil '(("[\240\040\t]+$" (0 'ws-trailing-whitespace t))) 'APPEND))

;; These are no-ops for Emacs 20, 21:
;; `font-lock-remove-keywords' is not defined, and we don't need to use it.
(defun ws-dont-highlight-tabs ()
  "Do not highlight tab characters (`C-i')."
  (when (fboundp 'font-lock-remove-keywords)
    (font-lock-remove-keywords nil '(("[\t]+" (0 'ws-tab t))))))

(defun ws-dont-highlight-hard-spaces ()
  "Do not highlight hard (non-breaking) space characters (`?\240').
This also resets `nobreak-char-display' to its saved value in
`ws--saved-nobreak-char-display', in order to restore its low-level,
vanilla highlighting."
  (when (and (boundp 'nobreak-char-display)  (not nobreak-char-display))
    (setq nobreak-char-display  ws--saved-nobreak-char-display))
  (when (fboundp 'font-lock-remove-keywords)
    (font-lock-remove-keywords nil '(("[\240]+" (0 'ws-hard-space t))))))

(when (> emacs-major-version 22)
  (defun ws-dont-highlight-hard-hyphens ()
    "Do not highlight hard (non-breaking) hyphen characters (`?\u2011').
This also resets `nobreak-char-display' to its saved value in
`ws--saved-nobreak-char-display', in order to restore the low-level,
vanilla highlighting."
    (unless nobreak-char-display
      (setq nobreak-char-display  ws--saved-nobreak-char-display))
    (when (fboundp 'font-lock-remove-keywords)
      (font-lock-remove-keywords nil '(("[\u2011]+" (0 'ws-hard-hyphen t)))))))

(defun ws-highlight-other-chars (&optional chars face)
  "Highlight CHARS using FACE.
CHARS is a list of character specifications acceptable as a value of
`ws-other-chars'.  It defaults to the value of `ws-other-chars'.
FACE defaults to face `ws-other-char'.

This also sets `nobreak-char-display' to nil, to turn off its
low-level, vanilla highlighting.  It saves the previous value of
`nobreak-char-display' in `ws--saved-nobreak-char-display'."
  (when (and (boundp 'nobreak-char-display)  nobreak-char-display)
    (setq ws--saved-nobreak-char-display  nobreak-char-display
          nobreak-char-display            nil))
  (font-lock-add-keywords nil (ws-other-chars-font-lock-spec chars face) 'APPEND))

(defun ws-dont-highlight-other-chars (&optional chars face)
  "Do not highlight CHARS using FACE.  That is, unhighlight any such.
CHARS and FACE are the same as for `ws-highlight-other-chars'.

This also resets `nobreak-char-display' to its saved value in
`ws--saved-nobreak-char-display', in order to restore its low-level,
vanilla highlighting."
  (when (and (boundp 'nobreak-char-display)  (not nobreak-char-display))
    (setq nobreak-char-display  ws--saved-nobreak-char-display))
  (when (fboundp 'font-lock-remove-keywords)
    (font-lock-remove-keywords nil (ws-other-chars-font-lock-spec chars face))))

(defun ws-other-chars-font-lock-spec (&optional characters face)
  "Font-lock spec used for highlighting CHARACTERS in FACE.
CHARS and FACE are the same as for `ws-highlight-other-chars'."
  (setq face        (or face  'ws-other-char)
        characters  (or characters  ws-other-chars))
  (mapcar (lambda (chars)
            (cond ((and (consp chars)
                        (if (fboundp 'characterp)
                            (characterp (car chars))
                          (integerp (car chars)))
                        (if (fboundp 'characterp)
                            (characterp (cdr chars))
                          (integerp (cdr chars))))
                   (let ((chr   (car chars))
                         (last  (cdr chars)))
                     (if (> emacs-major-version 20)
                         (let ((chs  ()))
                           (while (<= chr last)
                             (push chr chs)
                             (setq chr  (1+ chr)))
                           (list (regexp-opt-charset (nreverse chs))
                                 (list 0 `',face ws-other-chars-font-lock-override)))
                       (let ((class  "["))
                         (while (<= chr last)
                           (setq class  (concat class (string chr))
                                 chr    (1+ chr)))
                         (list (concat class "]+")
                               (list 0 `',face ws-other-chars-font-lock-override))))))
                  ((and (fboundp 'charsetp)  (charsetp chars))
                   (let ((chs    ()))
                     (map-charset-chars
                      (lambda (range ARG)
                        (let ((chr   (car range))
                              (last  (cdr range)))
                          (while (<= chr last)
                            (push chr chs)
                            (setq chr  (1+ chr)))))
                      chars)
                     (list (regexp-opt-charset (nreverse chs))
                           (list 0 `',face ws-other-chars-font-lock-override))))
                  ((vectorp chars)
                   (list (concat "[" (format "%s" chars) "]+")
                         (list 0 `',face ws-other-chars-font-lock-override)))
                  ((stringp chars)
                   (if (> emacs-major-version 20)
                       (list (regexp-opt-charset (append chars ()))
                             (list 0 `',face ws-other-chars-font-lock-override))
                     ;; Emacs 20 `regexp-opt-charset' just does not work.  Fake it.
                     (list (concat "[" chars "]+")
                           (list 0 `',face ws-other-chars-font-lock-override))))))
          characters))

(defun ws-dont-highlight-trailing-whitespace ()
  "Do not highlight whitespace characters at line ends.
See also `ws-highlight-trailing-whitespace'."
  (when (fboundp 'font-lock-remove-keywords)
    (font-lock-remove-keywords
     nil '(("[\240\040\t]+$" (0 'ws-trailing-whitespace t))))))

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'show-wspace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; show-wspace.el ends here
