;;; magpie.el --- acronym expansion and others. 

;; Copyright (C) Aidan Schofield 2007

;; Maintainer: Aidan Schofield
;; Version 0.15
;; Keywords: acronym, expansion, dabbrev, hippie
;; Time-stamp: <2007-03-24 16:16:19 aidanschofield>

;; Thanks to Stefan Kamphausen who added the customization.

;; This file is NOT part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Given the number of acronyms associated with emacs and gnu it is
;;; perhaps surprising that this idea for the dynamic expansion of
;;; acronyms is not already available in emacs. 

;;; This provides a method to type in acronyms and have them expanded
;;; in place. Its interface is similar to that of `dabbrev'. That is,
;;; an expansion of the acronym is looked for in the current buffer
;;; then other open buffers and if the expansion found is not what you
;;; want then calling `magpie-expand' again immediately rejects the
;;; present expansion and replaces it by a new one if such exists. It
;;; also includes ordinary dabbrev expansion. If there is no expansion
;;; of the acronym left it will try to expand prefixes of your acronym
;;; whilst retaining the unexpanded part of your acronym for later
;;; re-use.

;;; To be more precise, we type a string which takes the following
;;; form. The first character is often a control character which
;;; determines how the remaining characters should be transformed into
;;; a regexp. The next few characters are alphabetic and these are
;;; transformed by a recipe determined by the control character into a
;;; regexp of some sort. Finally, the last character or characters may
;;; be a number which is simply how many extra words should be glued
;;; on the end. Thus in this buffer ",pam4" could expand to "provides
;;; a method to type in acronyms" as would "prov6". The absence of a
;;; control character means that the expansion should be of `dabbrev'
;;; type and if the control character is "," then an acronym expansion
;;; should be used whilst if the control character is "." then an
;;; acronym expansion method is used but the text should begin inside
;;; a mathematics environment in a LaTeX document. For example,
;;; ".EEpAB" might expand to "\\Ext_{E^{\perp}}(A,B)". This
;;; illustrates other properties of the expansion method which is that
;;; the capitalization of the acronym determines the capitalization of
;;; the inserted string and also that the inserted string should
;;; consist of balanced sexps (balanced on the right but not
;;; necessarily on the left) if possible; in this particular case this
;;; would mean that ".EEpA" would also expand to the same phrase since
;;; the regexp match would include the first bracket "(" and the
;;; closure must then contain "(A,B)". There is a further wrinkle
;;; which is set up specifically for typing LaTeX. The string ",aat/i"
;;; will expand to "and assume that $$ is" with the cursor between the
;;; two dollar signs if this phrase (with something between the two
;;; dollar signs) occurs in the buffer. After typing here, "\C-."
;;; takes the cursor to the end of the inserted phrase. Finally, let
;;; us suppose that we type ",aattkifp" for which there is no
;;; expansion. Depending on the buffer contents, we may find that this
;;; is expanded to "and assume that ,tkifp" with the cursor at the
;;; space before the comma. If "and assume that" is not what we want
;;; then calling a`magpie-expand' will attempt to replace it; if it is
;;; what we want then `magpie-complete-expand' will attempt to expand
;;; the rest; for example we might get "and assume that the \kalg is
;;; finitely presented". I have provisionally bound `magpie-expand' to
;;; "\M-n" and `magpie-complete-expand' to "\M-N". However, for
;;; myself, I have bound them to "\H- " and "\A- ".

;;; one final wrinkle when typing LaTeX. If we call `magpie-expand' just
;;; after a $, it simply inserts the contents of the previous inline
;;; math environment that has length at least 6 and further calls to
;;; `magpie-expand' replace this by steadily more distant strings of the
;;; same type. If we call `magpie-expand' just after _{ it inserts a
;;; previous subscript of length at least 6 and if we call it after ^{
;;; we get the same for superscripts. Finally if we call it just after
;;; { not preceded by either _ or ^, it finds the contents of a
;;; previous pair {} provided that this is not a sub- or superscript.

;;; It is worth being precise about what an acronym
;;; matches. Specifically words are strings of alphabetic characters
;;; (for me this is [a-zA-Z]+ but this is customizable: see
;;; `magpie-alphabetic-chars') separated by non-alphabetic characters.

;;; I have also done similar things for editing Emacs Lisp files. Here
;;; again acronyms can be very useful. So I have set things up so that
;;; the control character "," introduces an acronym for strings from
;;; the buffer in a similar way to the above (using the same
;;; convention for words as in the previous paragraph). However, the
;;; control character "." introduces an acronym for symbols where
;;; words as above are separated by hyphens. Thus ".bdcu" expands to
;;; "backward-delete-char-untabify". This not only searches the
;;; current buffer but also looks at all symbols. This way of
;;; searching for names of symbols is substantially quicker than
;;; standard completion and I have also written functions below that
;;; incorporate this method (actually a related one) called
;;; `magpie-execute-extended-command', `magpie-describe-function' and
;;; `magpie-describe-variable' which begin by asking for a partial
;;; acronym for the command, function or variable you want (thus "bdc"
;;; matches "backward-delete-char" but it also matches
;;; "backward-delete-char-untabify") and then use a standard
;;; completion mechanism if there is more than one match. The reason
;;; for using prefix acronyms rather than full ones is that one may
;;; not remember the full name and this allows one to find it via the
;;; usual completion mechanism if one knows how the symbol's name
;;; begins.

;;; The system is highly extendable. As an illustration let us suppose
;;; that we wanted to include the ability to replace a regexp reg by
;;; some expansion from the buffer of the regexp. We choose a control
;;; character, let us say, ?;, and we add the association (cons ?;
;;; magpie-any-regexp) to `magpie-control-chars-alist'. The reader should
;;; then follow through the recipe for constructing `magpie-any-regexp'
;;; along the lines of `magpie-acr-template'. After this has been done
;;; then the string (concat ";" reg "4") would expand to a match for
;;; the regexp reg followed by the next 4 "words".

;;; The method is rather general because first we construct some
;;; regexp and find a match for it. Then we may adjust precisely which
;;; buffer-substring we use programmatically. We then apply a further
;;; function to compute what string we are goint to insert. Finally we
;;; have control over what we do when we insert the string (which is
;;; what allows us to set up a simple template mechanism).

;;; Things to do: Add further methods for constructing useful
;;; regexps. Think about ways to store past expansions though I am not
;;; convinced that this is wholly useful and it is not the way dabbrev
;;; operates.

;; Variables

;; Customizable variables

(defgroup magpie nil
  "An expansion mechanism similar to `dabbrev' but using acronyms."
  :tag "Magpie"
  :link '(url-link :tag "Home Page"
                   "http://www.emacswiki.org/cgi-bin/wiki/MagpieExpansion")
  :link '(emacs-commentary-link
          :tag "Commentary in magpie.el" "magpie.el")
  :prefix "magpie-"
  :group 'environment
  :group 'extensions
  :group 'convenience)

(defcustom magpie-ignore-extended-keybindings nil
  "Control the setting of more keybindings."
  :type 'boolean
  :group 'magpie)

(defcustom magpie-one-word "[^a-zA-Z]+[a-zA-Z]+"
  "Regexp to match one word."
  :type 'regexp
  :group 'magpie)

(defcustom magpie-alphabetic-chars "a-zA-Z"
  "a list of chars regarded as word characters for the expansion
  of acronyms and other regexps. Whatever is here should make
  sense when enclosed between \"[\" and \"]\" or between \"[^\"
  and \"]\" inside a regexp."
  :type 'regexp
  :group 'magpie)

(defcustom magpie-left-string-delimiters " \n\t([{$'`"
  "characters that determine the left hand end of the string-before-point"
  :type 'regexp
  :group 'magpie)

;; end of customisation pro tem

(defvar magpie-expansion nil
  "holds the currently proposed expansion")

(defvar magpie-expansion-start nil
  "point in the buffer at which magpie-expansion was inserted")

(defvar magpie-original-string nil
  "the original string before point when we first called
  `magpie-expand' in the present attempt to expand. This has 3
  parts -- the first part which may be empty is a control char to
  determine our expansion and this is read into
  `magpie-control-char' -- the last part at the end is a number
  which may again be empty and this is read into `magpie-number' --
  the remaining middle part is read into `magpie-word'.")

(defvar magpie-control-char 0
  "holds the current control char")

(defvar magpie-word nil
  "the (mostly) alphabetic part of magpie-original-string")

(defvar magpie-number nil
  "the number part of magpie-original-string")

(defvar magpie-regexp nil
  "the regexp computed from magpie-original-string")

(defvar magpie-expansion-functions nil
  "a list of functions to call in order to find a suitable
  expansion. each function returns nil if it fails to find an
  expansion and returns the expansion otherwise.")

(setq magpie-expansion-functions
      '(magpie-expand-from-buffer-backwards
    magpie-expand-from-buffer-forwards
    magpie-expand-from-buffer-list))

(defvar magpie-rejected-strings nil
  "a list of strings already rejected in the current bout of `magpie-expand'")

(defvar magpie-search-bound nil
  "records how far we have searched in the buffer")

(defvar magpie-list nil
  "the list of regexp matches when we search in an obarray")

;; FIXME how to defcustom this? need to rewrite a little because this
;; is entwined in the code at present.
(defvar magpie-control-chars-alist nil
  "an alist which determines how the first character of the
  string-before-point determines a regexp to search for. A key is
  a character which should be some punctuation character or at
  least something that won't make font-lock spit whilst it lives
  in the buffer and its value is a symbol. The function
  definition of this symbol transforms an alphabetic string into
  a regexp. The symbol's value is a predicate function (which is
  often just a function that always return t), and then the
  search functions in `magpie-expansion-functions' are roughly
  speaking `search-backward-regexp' and `search-forward-regexp'
  for `magpie-regexp' followed by `\(match-string 0\)'. Finally,
  the symbol's plist tells us what `magpie-fix-up' should be and
  the value of `magpie-follow-up'.  Specifically, its car is a
  lambda expression and we fset magpie-fix-up to this whilst its
  cdr is the value we give to magpie-follow-up-p.  This is where
  the user may customise this system by providing new control
  characters with new expansion symbols. " )
(make-variable-buffer-local 'magpie-control-chars-alist)
(setq-default magpie-control-chars-alist
          '((?, . magpie-acr-template)
        (?. . magpie-acr-math)))


(defvar magpie-backward-complete nil
  "a flag set to t when we have used up all possible expansions
  backward in the buffer")

(defcustom magpie-follow-up nil
  "determines whether we try some alternative method of expansion
  if our original method failed. At present this is used in an
  acronym expansion in order to look at acronym expansions of
  prefixes of our propsed acronym."
  :type 'boolean
  :group 'magpie)

(defvar magpie-buffers-list nil)

(defvar magpie-buffers-already-filched nil)

;; Code

(eval-when-compile
  (require 'cl))

;; Utilities of general use stolen from other homegrown files

(defun string-before-point ()
  ""
  (interactive)
  (save-excursion
    (buffer-substring-no-properties (point)
                    (progn
                      (skip-chars-backward
                       (concat "^"
                           magpie-left-string-delimiters))
                       (point)))))

(defun pop-string-before-point ()
  (interactive)
  (let (a)
    (prog1
    (buffer-substring-no-properties
     (setq a (point))
     (progn
       (skip-chars-backward (concat "^"
                    magpie-left-string-delimiters))
       (point)))
      (delete-region a (point)))))

(defun whitespace-to-space (str)
  (mapconcat 'identity (split-string str "[ \n\t]+") " "))

(defun make-multiple-string (n str)
  (mapconcat `(lambda (var) ,str) (make-list n 0) ""))

;; the next bit is independently useful since it allows us to separate
;; the distinct uses of the mark as defining the region and setting up
;; places we wish to visit later. I expect this already done somewhere
;; else.

(defvar editing-positions nil
  "a list of markers of positions we need to go to soon to edit;
  used as a stack")

(defun push-point-editing-position ()
  "pushes a marker at point onto editing-positions"
  (interactive)
  (setq editing-positions
    (cons (point-marker) editing-positions)))

(defun push-to-editing-positions (m)
  "pushes the position m as a marker onto the stack of editing-positions
whether m is a number or marker"
  (if (markerp m)
      (setq editing-positions (cons m editing-positions))
    (let ((l (make-marker)))
      (set-marker l m (current-buffer))
      (setq editing-positions (cons l editing-positions)))))

(defun pop-editing-position (arg)
  "goes to the next editing position which is removed from the
stack of editing positions as well. If this lies in another
buffer, then a prefix argument cause this to happen in another
window."
  (interactive "P")
  (let* ((l (pop editing-positions))
     (buff (marker-buffer l)))
    (or (equal buff (current-buffer))
    (if arg
        (switch-to-buffer-other-window buff)
      (switch-to-buffer buff)))
    (set-marker (goto-char l) nil)))

(defun copy-region-to-editing-position (arg start end)
  "copies the region to the next editing position which is
removed from the stack of editing positions as well. If this lies
in another buffer, then a prefix argument causes this to happen in
another window."
  (interactive "P\nr")
  (insert (prog1
          (buffer-substring-no-properties start end)
        (pop-editing-position arg))))


(unless magpie-ignore-extended-keybindings
  (global-set-key [(control ?,)] 'push-point-editing-position)
  (global-set-key [(control ?.)] 'pop-editing-position)
  (global-set-key [(control ?\;)] 'copy-region-to-editing-position))

;; the end of the code for editing-positions

(defun no-error-scan-sexps (a b)
  "a wrapper for scan-sexps so that it returns nil rather than
causing an error when it fails."
  (condition-case nil
      (scan-sexps a b)
    (error nil)))

;;; the next 2 functions are hacks which depend on AuCTeX to work
;;; properly so we make sure that nothing bad happens if AuCTeX is not
;;; installed or not running

(defun in-math-p (p)
  "under AuCTeX, returns t if inside a mathematics environment
and nil otherwise. If not using AuCTeX always return t"
  (if (and (intern-soft "font-latex-math-face")
       (facep 'font-latex-math-face))
      (let (it)
    (or (eq 'font-latex-math-face (setq it (get-text-property p 'face)))
        (and (listp it)
         (member 'font-latex-math-face it))))
    t))

(defun not-in-math-p (p)
  "under AuCTeX, returns t if not inside a mathematics environment
and nil otherwise. If not using AuCTeX always return t"
  (if (and (intern-soft "font-latex-math-face")
       (facep 'font-latex-math-face))
      (let (it)
    (not (or (eq 'font-latex-math-face
             (setq it (get-text-property p 'face)))
         (and (listp it)
              (member 'font-latex-math-face it)))))
    t))

;; we use the next function to run the search functions

(defun magpie-try (l)
  "l is a list of functions and magpie-try calls each in turn
until one of them returns non-nil. It returns nil if they all
return nil."
  (and l
       (or (funcall (car l))
       (magpie-try (cdr l)))))

;; the next function allows us to parametrise various functions and
;; values according to which expansion method we use. `magpie-method' is
;; an object which has a value and is a function. The function is
;; just a way to read the value which is an alist of symbols to
;; functions or values.

(defvar magpie-method nil
  "its value is the expansion method. In fact, this is a function
  that sets the functions and variables named in our main
  procedures.")


;; the main code beginning with the main function

(defun magpie-expand ()
  "Expand the string before point \"dynamically\".

If the string before point is alphabetic then the behaviour is
like that of `dabbrev-expand'. More generally, the
string-before-point will have the form of a control character (at
present , or .) followed by an alphabetic string and then a
number. The control character determines what kind of phrase is
looked for. If it is \",\" then the alphabetic string is expanded
as an acronym to some phrase in the buffer whilst if the control
character is a \".\" it is again expanded as an acronym for some
string occurring inside a math environment. The role of the
number is to determine how many words to glue on the end. Thus
the string \"alp4\" would expand to \"alphabetic then the
behaviour is\" as would the string \",att2\" if `magpie-expand'
were called in this buffer directly after each of these
strings. (See the first line of this paragraph for where this
expansion is taken from.) 

If the control character is \",\" then the alphabetic string may
also contain \"/\"s. In this case, we still expand as an acronym
but the slashes represent inline math environments. The expansion
will splice out the contents of the inline math environments and
will leave a template where these environments remain to be
filled in. For example, the string \",aat/i\" could expand to
\"and assume that $$ is\" where the cursor would be between the
two \"$\"s. After typing here, the key \\[pop-editing-position]
takes you to the next part of the template or to the end of the
expansion when all parts of the template have been dealt with.

If the character before point is \"$\" at the time `magpie-expand'
is called then the contents of the previous inline math
environment of length at least 6 is inserted at point. Similarly,
if point is just after \"_{\" or \"^{\" when `magpie-expand' is
called then the contents of the previous sub- or superscript
respectively (of length at least 6) is inserted at point.

If `magpie-expand' is called directly after it has been called then
the previous expansion is rejected and the next one if any is
inserted. 

If nothing can be done, then the original string-before-point is
re-inserted."
  (interactive)
  (if (magpie-just-expanded-p)
      (magpie-clean-up)
    (magpie-initialise))
  (setq magpie-expansion
    (magpie-try magpie-expansion-functions))
  (if magpie-expansion
      (magpie-insert magpie-expansion)
    (insert magpie-original-string)
    (and magpie-follow-up
     (magpie-transform-data)
     (magpie-expand))))

;; now we define all the undefined functions in the above

(defun magpie-just-expanded-p ()
  "returns t precisely when point lies within or at one end of
the expansion string inserted by the last call to magpie-expand
which has not been subsequently edited."
  (and magpie-expansion
       (eq (marker-buffer magpie-expansion-start) (current-buffer))
       (let ((pos (+ magpie-expansion-start (length magpie-expansion))))
     (and (<= pos (point-max))
          (<= magpie-expansion-start (point))
          (<= (point) pos)
          (string= magpie-expansion
               (buffer-substring-no-properties magpie-expansion-start
                               pos))))))

(defun magpie-clean-up ()
  "cleans up after the previous expansion which has just been rejected."
  (setq magpie-rejected-strings (cons magpie-expansion magpie-rejected-strings))
  (delete-region magpie-expansion-start
         (+ magpie-expansion-start (length magpie-expansion)))
;; next get rid of random template stuff left over if any
  (while (and editing-positions
          (= (car editing-positions) (point)))
    (pop-editing-position nil))
  (setq magpie-expansion nil))

(defun magpie-initialise ()
  ;; this is the point where one might do something with the
  ;; previously accepted expansion but for the moment we do nothing
  "this is run on the first call to `magpie-expand'. It sets up the
necessary variables and functions for `magpie-expand'."
  (let (i ass wrd)
    (setq magpie-original-string (pop-string-before-point)
      magpie-expansion-start (point-marker)
      magpie-expansion nil
      magpie-list nil
      magpie-rejected-strings nil
      magpie-backward-complete nil
      magpie-buffers-already-filched nil
      magpie-search-bound (point-marker)
      magpie-control-char nil)
    (if (string= "" magpie-original-string)
    (setq magpie-word ""
          magpie-number 0
          magpie-method 'magpie-empty)
      (if (setq ass (assoc (aref magpie-original-string 0)
               magpie-control-chars-alist))
      (setq magpie-control-char (car ass)
        magpie-method (cdr ass)
        wrd (substring magpie-original-string 1))
    (setq wrd magpie-original-string))
      (if (setq i (string-match "[0-9]" wrd))
      (setq magpie-word (substring wrd 0 i)
        magpie-number (string-to-number (substring wrd i)))
    (setq magpie-word wrd
          magpie-number 0))
      (or magpie-control-char
      (if (string= "" magpie-word)
          (setq magpie-method 'magpie-empty)
        (setq magpie-method 'magpie-dabbrev))))
; need to set up magpie-dabbrev according to mode
    (magpie-default)
    (funcall magpie-method)
    (setq magpie-regexp (magpie-regexp-maker magpie-word))))

(defun magpie-expand-from-buffer-backwards ()
  "searches backward through the buffer for a match for
magpie-regexp, computes a possibly different buffer-substring and
then normalizes it suitably before returning this normalized
string."
  (if magpie-backward-complete
      nil
    (save-excursion
      (while (and (goto-char magpie-search-bound)
          (search-backward-regexp magpie-regexp (point-min) 0)
          (or (member
               (setq magpie-expansion
                 (magpie-fix-up-function (magpie-buffer-substring)))
               magpie-rejected-strings)
              (not (magpie-regexp-predicate))
              (< (length magpie-expansion) 6)))
    (setq magpie-search-bound (point-marker)
          magpie-backward-complete (= magpie-search-bound (point-min))
          magpie-expansion nil))
      (setq magpie-search-bound (point-marker)
        magpie-backward-complete (= magpie-search-bound (point-min))))
    magpie-expansion))

(defun magpie-expand-from-buffer-forwards ()
  "searches forward through the buffer for a match for
magpie-regexp, computes a possibly different buffer-substring and
then normalizes it suitably before returning this normalized
string."
  (unless (memq (current-buffer) magpie-buffers-already-filched)
    (if magpie-buffers-already-filched
    (or (eq (marker-buffer magpie-search-bound) (current-buffer))
        (setq magpie-search-bound (point-min-marker)))
      (and (< magpie-search-bound (point))
       (setq magpie-search-bound (point-marker))))
    (save-excursion
      (while (and (goto-char magpie-search-bound)
          (search-forward-regexp magpie-regexp (point-max) 0)
          (or (member
               (setq magpie-expansion
                 (magpie-fix-up-function (magpie-buffer-substring)))
               magpie-rejected-strings)
              (not (magpie-regexp-predicate))
              (< (length magpie-expansion) 6)))
    (setq magpie-search-bound (point-marker)
          magpie-expansion nil)
    (if (= magpie-search-bound (point-max))
        (setq magpie-buffers-already-filched
          (cons (current-buffer) magpie-buffers-already-filched))))
      (setq magpie-search-bound (point-marker))
      (if (= magpie-search-bound (point-max))
      (setq magpie-buffers-already-filched
        (cons (current-buffer) magpie-buffers-already-filched))))
    magpie-expansion))

(defun magpie-expand-from-buffer-list ()
  (magpie-try (mapcar (lambda (f)
            `(lambda ()
               (magpie-expand-from-buffer ,f)))
              (buffer-list-by-mode))))

(defun magpie-expand-from-buffer (buf)
  (unless (member buf magpie-buffers-already-filched)
    (save-excursion
      (set-buffer buf)
      (magpie-expand-from-buffer-forwards))))

(defun buffer-major-mode (buf)
  (save-excursion
    (set-buffer buf)
    major-mode))

(defun buffer-list-by-mode ()
  "returns the buffer-list with those buffers in the same mode at
the front and without the temporary buffers"
  (let ((bl1 (buffer-list))
    bl2 bl3 buf)
    (setq bl1 (delq (current-buffer) bl1))
    (while bl1
      (if (eq (buffer-major-mode (setq buf (pop bl1))) major-mode)
      (setq bl2 (cons buf bl2))
    (or (string-match "\*" (buffer-name buf))
        (setq bl3 (cons buf bl3)))))
    (append bl2 bl3)))

(defun magpie-default-insert (str)
  "the default version of `magpie-insert' which is simply to
insert the string and return t."
  (insert str)
  t)

(defun magpie-insert-with-template (str)
  "inserts the string, sets up the template if any and returns t"
  (insert str)
  (let (p)
    (while (save-excursion
         (setq p (search-backward "\$\$" magpie-expansion-start t)))
      (push-point-editing-position)
      (goto-char (1+ p))))
  t)


;; this next function allows us to find acronym expansions that are
;; made from smaller acronyms. It is called after we have failed to
;; find a useful expansion for the whole acronym.

(defun magpie-default-transform-data ()
  "shuffles the buffer about a bit so that it will be easy to
call `magpie-expand' on a shorter acronym and retain the part of
the acronym we have not yet used."
  (if (and (< (1+ (point)) (point-max))
       (= (char-after (1+ (point))) magpie-control-char))
      (if (> (length (string-before-point)) 3)
      (progn
        (transpose-chars 2)
        (forward-char -3)
        t)
    (delete-char 2)
    (skip-chars-forward (concat "^" magpie-left-string-delimiters))
    (message "I've done all I can")
    nil)
    (if (> (length (string-before-point)) 3)
    (progn (forward-char -1)
           (save-excursion
         (insert " " magpie-control-char)
         t))
      (skip-chars-forward (concat "^" magpie-left-string-delimiters))
      (message "I've done all I can")
      nil)))

;; available to be called when we have partially expanded an acronym
;; and want to expand the rest.

(defun magpie-complete-expand ()
  "we like the expansion of the partial acronym and we wish to
call `magpie-expand' on the unused part of the acronym."
  (interactive)
  (forward-char 1)
  (skip-chars-forward (concat "^" magpie-left-string-delimiters))
  (magpie-expand))

;;; now we make our expansion methods

;;; the next objects are just labels so we only intern them.

(intern "magpie-regexp-maker")
(intern "magpie-regexp-predicate")
(intern "magpie-buffer-substring")
(intern "magpie-fix-up-function")
(intern "magpie-follow-up")
(intern "magpie-control-char")

(defun magpie-default-regexp-maker (str)
  str)

(defun magpie-default-regexp-predicate ()
  t)

(defun magpie-default-buffer-substring ()
  (match-string-no-properties 0))

(defun magpie-default-fix-up-function (str)
  (whitespace-to-space str))

(defun magpie-default ()
  "this sets the variable global functions and variables to their
default value before the expansion method sets them to specific
values."
  (fset 'magpie-insert 'magpie-default-insert)
  (fset 'magpie-transform-data 'magpie-default-transform-data)
  (fset 'magpie-regexp-maker 'magpie-default-regexp-maker)
  (fset 'magpie-regexp-predicate 'magpie-default-regexp-predicate)
  (fset 'magpie-buffer-substring 'magpie-default-buffer-substring)
  (fset 'magpie-fix-up-function 'magpie-default-fix-up-function)
  (setq magpie-expansion-functions '(magpie-expand-from-buffer-backwards
                   magpie-expand-from-buffer-forwards
                   magpie-expand-from-buffer-list)
    magpie-follow-up nil
    magpie-control-char 0))

;; our expansion methods are defined as functions that change the
;; definitions of various functions and variables. 

(defun magpie-dabbrev ()
  (fset 'magpie-regexp-maker (lambda (str)
                 (concat "\\<" str
                     "[" magpie-alphabetic-chars "]*\\>"
                     (make-multiple-string magpie-number
                               magpie-one-word))))
  (fset 'magpie-regexp-predicate 'magpie-acr-not-math-pred)
  (fset 'magpie-buffer-substring 'magpie-close-sexps-and-backslash)
  (fset 'magpie-fix-up-function (lambda (str)
                (and str
                     (string-match magpie-regexp str)
                     (aset str (car (match-data))
                       (aref magpie-word 0))
                     (whitespace-to-space str)))))

(defun magpie-acr-template ()
  (fset 'magpie-regexp-maker 'magpie-acr-template-regexp)
  (fset 'magpie-regexp-predicate 'magpie-acr-not-math-pred)
  (fset 'magpie-buffer-substring 'magpie-close-sexps-and-backslash)
  (fset 'magpie-fix-up-function 'magpie-acr-template-fix-up)
  (fset 'magpie-insert 'magpie-insert-with-template)
  (setq magpie-follow-up t
    magpie-control-char ?,))

(defun magpie-acr-math ()
  (fset 'magpie-regexp-maker 'magpie-acr-math-regexp)
  (fset 'magpie-regexp-predicate 'magpie-acr-math-pred)
  (fset 'magpie-buffer-substring 'magpie-close-sexps-and-backslash)
  (fset 'magpie-fix-up-function 'magpie-acr-template-fix-up)
  (setq magpie-follow-up t
    magpie-control-char ?\.))

(defun magpie-empty ()
  (fset 'magpie-regexp-maker 'magpie-empty-regexp)
  (fset 'magpie-regexp-predicate 'magpie-math-before)
  (fset 'magpie-buffer-substring 'magpie-brackets-internals))

;; now we fill in the functions undefined in the above methods

(defun magpie-acr-template-regexp (str)
  (concat (if (string= str "")
          ""
        (mapconcat
         (lambda (ch)
           ;; (logand 95 ) maps lowercase to uppercase chars
           (if (and (<= ?A (setq ch (logand 95 ch)))
            (<= ch ?Z))
           (concat "\\<\\([" (list ch (+ ch 32)) "]\\)["
               magpie-alphabetic-chars "]*\\>")
         (if (= ch 15) ;; this is (logand 95 ?/)
             "\\(\\$[^\\$]*\\$\\)"
           "")))
         str (concat "[^" magpie-alphabetic-chars "]*")))
      (make-multiple-string magpie-number magpie-one-word)))

(defun magpie-acr-template-fix-up (str)
  "capitalizes str according to magpie-word and sets up the template."
  (and str
       (setq str (whitespace-to-space str))
       (string-match magpie-regexp str)
       (let ((data (nreverse (cddr (match-data))))
         (i (1- (length magpie-word)))
         a b)
     (while data
       (setq a (car data)
         b (cadr data)
         data (cddr data))
;; the next bit is the template part
       (if (> (- a b) 1)
           (setq str (concat (substring str 0 (1+ b))
                 (substring str (1- a)))
             i (1- i))
         (aset str b (aref magpie-word i))
         (setq i (1- i))))
     str)))

(defun balance-region-right (b e)
  "given a region in the buffer compute the smallest region
containing this that contains all brackets of different type
matching left brackets in the original region"
  (let ((d e))
    (save-excursion
      (goto-char e)
      (while (and (< (skip-syntax-backward "^(" b) 0)
          (> (point) b))
    (forward-char -1)
    (setq d (max d (or (no-error-scan-sexps (point) 1)
               d)))))
    (cons b d)))

(defun magpie-close-sexps-and-backslash ()
  "called after a regexp search, this returns the buffer
substring containing the regexp match that contains all brackets
of any sort pairing a left bracket in the regexp match and also a
backslash if it immediately precedes the regexp match"
  (let* ((data (match-data))
     (cns (balance-region-right (car data)
                    (cadr data)))
     (b (car cns))
     (e (cdr cns)))
    (save-excursion
      (goto-char b)
      (and (= (preceding-char) ?\\)
       (setq b (1- b)))
      (buffer-substring-no-properties b e)))) 

(defun magpie-acr-math-regexp (str)
  (concat "\\\\?" (magpie-acr-template-regexp str)))

;; note that in fact this never picks up the backslash in a
;; `search-backward-regexp' so this is not useful. fixed this anyway
;; by re-adjusting the buffer-substring.

(defsubst magpie-acr-math-pred ()
  (in-math-p (point)))

(defsubst magpie-acr-not-math-pred ()
  (not-in-math-p (point)))

;; functions when we expand an empty string. Here we are filling in an
;; inline math environment or a subscript or superscript.

(defsubst magpie-math-before ()
  (in-math-p (1- (point))))

(defun magpie-empty-regexp (str)
  "does not depend on str at all which is OK because it is only
ever called with an empty string"
  (if (= (preceding-char) ?\$)
      "\\$"
    (if (= (preceding-char) ?{)
    (let ((ch (save-excursion
            (forward-char -1)
            (preceding-char))))
      (if (= ch ?_)
          "_{"
        (if (= ch ?^)
        "\\^{"
          "[^_^]{")))
      "")))

(defun magpie-brackets-internals ()
  "if at a $ return the text strictly between it and the
immediately preceding $\; otherwise after moving forward one
character we should be at a { and we return the text strictly
between it and its matching }. If this fails we return the empty
string."
  (let (b e)
  (if (= (following-char) ?\$)
      (setq e (point)
        b (and (setq b (no-error-scan-sexps (1+ (point)) -1))
           (1+ b)))
    (forward-char 1)
    (if (= (following-char) ?{)
    (setq e (1+ (point))
          b (and (setq b (no-error-scan-sexps (point) 1))
             (1- b)))))
  (if b
      (buffer-substring-no-properties b e)
    "")))

;; set some keys for what we have done

(global-set-key [(hyper ? )] 'magpie-expand)
(global-set-key [(alt ? )] 'magpie-complete-expand)
(global-set-key [(meta ?n)] 'magpie-expand)
(global-set-key [(meta ?N)] 'magpie-complete-expand)

;;; now for something related but different. we use acronyms to find
;;; symbols. 


(defun magpie-matching-symbols (reg &optional pred obarr)
  (let (sym-list)
    (mapatoms (lambda (sym)
        (if (and (string-match reg (symbol-name sym))
             (if pred
                 (funcall pred sym)
               t))
            (setq sym-list (cons (symbol-name sym) sym-list))))
          (or obarr obarray))
    sym-list))


(defun magpie-lisp-acr-regexp (str)
  (concat "\\<"
      (mapconcat
       (lambda (ch)
         (concat (char-to-string ch) "[" magpie-alphabetic-chars "]*"))
       str "[-]\\{1,2\\}")
      "\\>"))

(defun magpie-lisp-acr-full-symbol-regexp (str)
  (concat "\\`"
      (mapconcat
       (lambda (ch)
         (concat (char-to-string ch) "[" magpie-alphabetic-chars "]*"))
       str "[-]\\{1,2\\}")
      "\\'"))

(defun magpie-lisp-acr-prefix-regexp (str)
  (concat "\\<"
      (mapconcat
       (lambda (ch)
         (concat (char-to-string ch) "[" magpie-alphabetic-chars "]*"))
       str "[-]\\{1,2\\}")))

(defun magpie-lisp-acr-prefix-full-symbol-regexp (str)
  (concat "\\`"
      (mapconcat
       (lambda (ch)
         (concat (char-to-string ch) "[" magpie-alphabetic-chars "]*"))
       str "[-]\\{1,2\\}")))

;; at this stage we can write some functions which are not about
;; expansion but rather about calling commands quickly and finding
;; documentation more efficiently than ordinary completion allows.

(defun magpie-execute-extended-command (str)
  "replaces execute-extended-command by using acronyms for
commands and then completion if there is more than one
possibility. Thus to run eval-current-buffer, one types
M-XecbRET"
  (interactive "s")
  (let ((comm-lst (magpie-matching-symbols
           (magpie-lisp-acr-prefix-full-symbol-regexp str)
           'commandp))
    command)
  (setq command (and comm-lst
             (if (cdr comm-lst)
             (completing-read "Type some more: " comm-lst
                      'stringp t
                      (try-completion (substring str 0 1)
                              comm-lst))
               (car comm-lst))))
  (if command
      (call-interactively (intern command))
    (message "No such command"))))

(define-key global-map [(hyper ?x)] 'magpie-execute-extended-command)
(define-key esc-map "X" 'magpie-execute-extended-command)

(defun magpie-describe-function ()
  "works like describe-function except that it asks for a partial
acronym of the function you wish to be described. If there is
more than one possibility left the standard completion mechanism
takes over."
  (interactive)
  (describe-function
   (or (function-called-at-point)
       (let ((acr (read-from-minibuffer "Type a (partial) acronym: "))
         fn-lst)
     (setq fn-lst (magpie-matching-symbols
               (magpie-lisp-acr-prefix-full-symbol-regexp acr)
               'functionp))
     (intern (if fn-lst
             (if (cdr fn-lst)
             (completing-read "Type some more: " fn-lst
                      'stringp t
                      (try-completion (substring acr 0 1)
                              fn-lst))
               (car fn-lst))
           (error "No such function")))))))

(global-set-key "\C-h\M-f" 'magpie-describe-function)

(defun magpie-describe-variable ()
  "a version of `describe-variable' that uses partial acronyms: see the documentation of the command `magpie-describe-function'."
  (interactive)
  (describe-variable
   (if (equal (variable-at-point) 0)
       (let ((acr (read-from-minibuffer "Type a (partial) acronym: "))
         var-lst)
     (setq var-lst (magpie-matching-symbols
               (magpie-lisp-acr-prefix-full-symbol-regexp acr)
               'boundp))
     (intern (if var-lst
             (if (cdr var-lst)
             (completing-read "Type some more: " var-lst
                      'stringp t
                      (try-completion (substring acr 0 1)
                              var-lst))
               (car var-lst))
           (error "No such variable"))))
     (variable-at-point))))

(global-set-key "\C-h\M-v" 'magpie-describe-variable)


;; back to expansion methods

(defvar magpie-obarray-regexp nil
  "we may need a different regexp for searching in the obarray
  rather than in the buffer and this is it.")

(defun magpie-adjust-regexp-for-obarray ()
  magpie-regexp)

(defun magpie-expand-symbol-from-obarray ()
  (or magpie-list
      (setq magpie-obarray-regexp (magpie-adjust-regexp-for-obarray)
        magpie-list (set-difference
             (magpie-matching-symbols magpie-obarray-regexp)
             magpie-rejected-strings :test 'string=)))
  (if (cdddr magpie-list)
      (completing-read "" magpie-list
               'stringp t
               (try-completion (substring magpie-word 0 1)
                       magpie-list))
    (pop magpie-list)))

(defun magpie-acr-lisp-symbol ()
  "search method that looks for symbols via an acronym"
  (fset 'magpie-regexp-maker 'magpie-lisp-acr-regexp)
  (fset 'magpie-adjust-regexp-for-obarray (lambda ()
                        (concat "\\`"
                            magpie-regexp
                            "\\'")))
  (fset 'magpie-regexp-predicate (lambda ()
                   (and (not (= (preceding-char) ?-))
                    (not (= (char-after (cadr (match-data)))
                        ?-)))))
  (fset 'magpie-fix-up-function 'identity)
  (setq magpie-expansion-functions '(magpie-expand-from-buffer-backwards
                     magpie-expand-from-buffer-forwards
                     magpie-expand-from-buffer-list
                     magpie-expand-symbol-from-obarray)
    magpie-control-char ?\.))

(defun magpie-acr-lisp ()
  (fset 'magpie-regexp-maker 'magpie-acr-template-regexp)
  (fset 'magpie-buffer-substring 'magpie-close-sexps-and-backslash)
  (fset 'magpie-fix-up-function 'identity)
  (fset 'magpie-insert 'magpie-insert-and-indent)
  (setq magpie-follow-up t
    magpie-control-char ?,))

(defun magpie-insert-and-indent (str)
  "inserts the string and indents the region it occupies. It must
then update magpie-expansion so that magpie-just-expanded-p will
evaluate correctly."
  (insert str)
  (let ((pos (point-marker)))
    (save-excursion
      (indent-region (progn
               (goto-char magpie-expansion-start)
               (beginning-of-line)
               (point))
             pos nil)))
  (setq magpie-expansion (buffer-substring-no-properties magpie-expansion-start
                               (point))))

;; set up control chars for emacs lisp mode

(add-hook 'emacs-lisp-mode-hook
      (lambda ()
        (setq magpie-control-chars-alist '((?, . magpie-acr-lisp)
                         (?. . magpie-acr-lisp-symbol)))))

(provide 'magpie)


