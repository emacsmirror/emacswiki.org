;;; isearch-prop.el --- Search character-property contexts.
;;
;; Filename: isearch-prop.el
;; Description: Search character-property contexts.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2011-2013, Drew Adams, all rights reserved.
;; Created: Sun Sep  8 11:51:41 2013 (-0700)
;; Version: 0
;; Package-Requires: ()
;; Last-Updated: Tue Sep 10 21:52:30 2013 (-0700)
;;           By: dradams
;;     Update #: 257
;; URL: http://www.emacswiki.org/isearch-prop.el
;; Doc URL: http://www.emacswiki.org/IsearchPlus
;; Keywords: search, matching, invisible, thing, help
;; Compatibility: GNU Emacs: 23.x, 24.x
;;
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Search character-property contexts.
;;
;;  Such contexts are zones of text that have certain text properties
;;  or overlays with certain overlay properties.
;;
;;  This file is part of library Isearch+, which includes also file
;;  `isearch+.el'.  You can use either of the files without the other,
;;  if you like, but I recommend that you use them together.
;;
;;
;;  The features provided by this library are based on similar
;;  features introduced by Icicles (http://www.emacswiki.org/Icicles).
;;
;;  More description below - see Overview of Features.
;;
;;
;;  Index
;;  -----
;;
;;  If you have library `linkd.el' and Emacs 22 or later, load
;;  `linkd.el' and turn on `linkd-mode' now.  It lets you easily
;;  navigate around the sections of this doc.  Linkd mode will
;;  highlight this Index, as well as the cross-references and section
;;  headings throughout this file.  You can get `linkd.el' here:
;;  http://dto.freeshell.org/notebook/Linkd.html.
;;
;;  (@> "Overview of Features")
;;  (@> "Change Log")
;;  (@> "Variables")
;;  (@> "Keys")
;;  (@> "General Commands")
;;  (@> "General Non-Interactive Functions")
;;  (@> "Imenu Commands and Functions")
;;  (@> "THING Commands and Functions")
;;  (@> "Character-Property Search")
;;
;;
;;  Macros defined here:
;;
;;    `isearchp-with-comments-hidden'.
;;
;;  Commands defined here:
;;
;;    `isearchp-char-prop-backward',
;;    `isearchp-char-prop-backward-regexp',
;;    `isearchp-char-prop-forward',
;;    `isearchp-char-prop-forward-regexp',
;;    `isearchp-hide/show-comments', `isearchp-next-visible-thing',
;;    `isearchp-previous-visible-thing',
;;    `isearchp-put-prop-on-region',
;;    `isearchp-regexp-define-contexts', `isearchp-thing',
;;    `isearchp-thing-define-contexts',
;;    `isearchp-toggle-complementing-domain',
;;    `isearchp-toggle-ignoring-comments'.
;;
;;  User options defined here:
;;
;;    `isearchp-ignore-comments-flag'.
;;
;;  Faces defined here:
;;
;;    None.
;;
;;  Non-interactive functions defined here:
;;
;;    `isearchp-bounds-of-thing-at-point', `isearchp-char-prop-1',
;;    `isearchp-char-prop-default-match-fn', `isearchp-char-prop-end',
;;    `isearchp-char-properties-in-buffer',
;;    `isearchp-char-prop-filter-pred',
;;    `isearchp-char-prop-matches-p', `isearchp-defined-thing-p',
;;    `isearchp-message-prefix', `isearchp-next-visible-thing-1',
;;    `isearchp-next-visible-thing-2',
;;    `isearchp-next-visible-thing-and-bounds',
;;    `isearchp-read-face-names', `isearchp-read-face-names--read',
;;    `isearchp-read-sexps', `isearchp-remove-duplicates',
;;    `isearchp-some', `isearchp-thing-read-args',
;;    `isearchp-thing-scan', `isearchp-things-alist'.
;;
;;  Internal variables defined here:
;;
;;    `isearchp-char-prop-prop', `isearchp-char-prop-type',
;;    `isearchp-char-prop-values', `isearchp-filter-predicate-orig',
;;    `isearchp-last-thing-type'.
;;
;;
;;  Keys bound in `isearch-mode-map' here:
;;
;;    `C-t'        `isearchp-char-prop-forward'
;;    `C-M-t'      `isearchp-char-prop-forward-regexp'
;;    `C-M-;'      `isearchp-toggle-ignoring-comments'
;;    `C-M-~'      `isearchp-toggle-complementing-domain'
;;
;;
;;  This file should be loaded *AFTER* loading the standard GNU file
;;  `isearch.el'.  So, in your `~/.emacs' file, do this:
;;
;;  (eval-after-load "isearch" '(require 'isearch-prop))
 
;;(@* "Overview of Features")
;;
;;; Overview of Features ---------------------------------------------
;;
;;  * Ability to search within character-property zones.  Example:
;;    search within zones having a `face' text property with a value
;;    of `font-lock-comment-face' or `font-lock-string-face'.  Search
;;    overlays or text properties.  From within Isearch: `C-t' (or
;;    `C-M-t' for regexp search).  First time, or with a prefix
;;    argument, you are prompted for the property and its values.  See
;;    the doc string of command `isearchp-char-prop-forward'.
;;
;;  * Besides relying on other code to set `face' and other text
;;    properties for use with `C-t', you can use command
;;    `isearchp-put-prop-on-region' (outside of Isearch) to add a text
;;    property to a zone of text.  By default, it applies the last
;;    property and value whose zones you searched using `C-t', but a
;;    prefix arg lets you specify the property and value to apply.
;;    This gives you an interactive way to set up zones for
;;    text-property search (`C-t').  For property `face', empty input
;;    removes all faces from the region.
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2013/09/10 dadams
;;     isearchp-regexp-scan, isearchp-thing-scan: Added text property isearchp.
;;     Bound isearchp-hide/show-comments to M-; in isearch-mode-map.
;;     Forgot to require cl.el at compile time, for case.
;; 2013/09/09 dadams
;;     Added: isearchp-toggle-complementing-domain, isearchp-context-level,
;;            isearchp-complement-domain-p, isearchp-read-context-regexp, isearchp-regexp-scan,
;;            isearchp-regexp-define-contexts, isearchp-regexp-context-search,
;;            isearchp-add-regexp-as-property, isearchp-regexp-read-args, isearchp-text-prop-present-p,
;;            isearchp-ignore-comments-flag, isearchp-hide/show-comments, isearchp-with-comments-hidden,
;;            isearchp-toggle-ignoring-comments, isearchp-last-thing-type, isearchp-thing,
;;            isearchp-thing-read-args, isearchp-thing-define-contexts, isearchp-thing-scan,
;;            isearchp-things-alist, isearchp-defined-thing-p, isearchp-bounds-of-thing-at-point,
;;            isearchp-next-visible-thing-and-bounds, isearchp-previous-visible-thing,
;;            isearchp-next-visible-thing(-1|-2).
;;     isearchp-char-prop-1: Handle complementing. Deactivate region when done.
;;     isearchp-char-prop-filter-pred: Handle complementing.
;;     isearchp-char-prop-forward: Update doc to better describe behavior with no prefix arg.
;;     Bound isearchp-toggle-ignoring-comments to C-M-;, isearchp-toggle-complementing-domain' to C-M-~.
;; 2013/09/08 dadams
;;     Created from code factored out of isearch+.el.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:


(eval-when-compile (require 'cl)) ;; case
 
;;(@* "Variables")

;;; Variables ----------------------------------------------

;; Same as `ignore-comments-flag' in `hide-comnt.el'.
;;
(defcustom isearchp-ignore-comments-flag t
  "*Non-nil means `isearchp-with-comments-hidden' hides comments."
  :type 'boolean :group 'isearch-plus)

(defvar isearchp-char-prop-type nil
  "Last property type used for `isearchp-char-prop-*' commands.")

(defvar isearchp-char-prop-prop nil
  "Last property used for `isearchp-char-prop-*' commands.")

(defvar isearchp-char-prop-values nil
  "Last property values used for `isearchp-char-prop-*' commands.")

(defvar isearchp-complement-domain-p nil
  "Non-nil means complement the initial search candidates wrt the buffer.
This has an effect only on (some) Icicles search commands.
The scan function or regexp for the search command defines a set of
matches in the buffer.  If this option is non-nil then the actual
candidates used are the sections of buffer text that are separated by
the initial candidates, that is, the non-candidates as defined by the
scan or regexp.")

(defvar isearchp-context-level 0
  "Match level for Isearch context regexp.
0 means use whatever matches the whole context regexp as the search
context.  1 means use whatever matches the first subgroup of the
regexp as the search context, and so on.")

(defvar isearchp-filter-predicate-orig nil
  "Original value of `isearch-filter-predicate'.")

;;; Same as `thgcmd-last-thing-type' in `thing-cmds.el'.
(defvar isearchp-last-thing-type (if (boundp 'thgcmd-last-thing-type) thgcmd-last-thing-type 'sexp)
  "Type of thing last used by `isearchp-next-visible-thing' (or previous).")
 
;;(@* "Keys")

;;; Keys -------------------------------------------------------------

(define-key isearch-mode-map (kbd "C-t")   'isearchp-char-prop-forward)
(define-key isearch-mode-map (kbd "M-;")   'isearchp-hide/show-comments)
(define-key isearch-mode-map (kbd "C-M-t") 'isearchp-char-prop-forward-regexp)
(define-key isearch-mode-map (kbd "C-M-;") 'isearchp-toggle-ignoring-comments)
(define-key isearch-mode-map (kbd "C-M-~") 'isearchp-toggle-complementing-domain)
 
;;(@* "General Commands")

;;; General Commands -------------------------------------------------

(defun isearchp-char-prop-forward (arg) ; Bound to `C-t' in `isearch-mode-map'.
  "Isearch forward in text with a character (overlay or text) property.
That is, move to the next such property and search within it for text
matching your input.

If `isearchp-complement-domain-p' is non-nil then move to the next
zone that does *not* have the given property.  (Use `C-M-~' during
Isearch to toggle this variable.)  For example, this lets you search
for text that is NOT displayed using a certain face or combination of
faces.

By default, search for the character property that you last used in an
Isearch command.  This means any Isearch command that searches or
applies character properties, which includes the `isearch-char-prop-*'
commands and commands such as `isearchp-imenu*', `isearchp-thing', and
`isearchp-regexp-context-search'.

If you have not previously used such an Isearch command then you are
prompted for the following:

 * the property type (`text', `overlay', or `text and overlay')
 * the property (e.g., `face', `mumamo-major-mode')
 * the property values (e.g., a list of faces, for property `face')

Otherwise:

 With no prefix arg, use the settings (property type, property,
 property values) from the last time you invoked a command that
 searches character properties.

 With a prefix arg, you are prompted for the property and property
 values to use.  The particular prefix arg determines the property
 type to search, as follows:

  * plain prefix arg (`C-u'): both overlay and text property zones
  * negative prefix arg (e.g., `C--'): overlay property zones
  * non-negative prefix arg (e.g., `C-9'): text property zones

By default, an actual value of the property matches the value
you specify if it is `equal'.  Properties `mumamo-major-mode' and
`face' (or `font-lock-face') are exceptions.

For `mumamo-major-mode', you specify the major mode whose zones of
text you want to search.  The actual property value is a list whose
car is the major mode symbol.

For properties `face' and `font-lock-face', you can pick multiple
faces, using completion (hit `RET' with empty input to finish
choosing).  Text is searched that has a face property that includes
any of the faces you choose.  If you choose no face (empty input at
the outset), then text with any face at all is searched.

NOTE: If you search zones of property `face', and the property values
      include `font-lock' faces, then you might want to first make
      sure the entire buffer has been fontified.  You can do that
      using command `isearchp-fontify-buffer-now'.

NOTE: This command is available during normal Isearch, on key `C-t'.
      However, in order to be able to use a prefix arg with this
      command from within Isearch, you must set `isearch-allow-scroll'
      or `isearch-allow-prefix' (if available) to non-nil.  Otherwise,
      a prefix arg during Isearch exits Isearch."
  (interactive "P")
  (isearchp-char-prop-1 'isearch-forward arg))

(defun isearchp-char-prop-backward (arg)
  "Isearch backward in text with a character (overlay or text) property.
See `isearchp-char-prop-forward'."
  (interactive "P")
  (isearchp-char-prop-1 'isearch-backward arg))

(defun isearchp-char-prop-forward-regexp (arg) ; Bound to `C-M-t' in `isearch-mode-map'.
  "Regexp Isearch forward in text with a character (overlay or text) property.
NOTE: This command is available during normal Isearch, on key `C-M-t'.
      However, in order to be able to use a prefix arg with this
      command, you must set `isearch-allow-scroll' or
      `isearch-allow-prefix' (if available) to non-nil.
      Otherwise, a prefix arg during Isearch exits Isearch.
See `isearchp-char-prop-forward'."
  (interactive "P")
  (isearchp-char-prop-1 'isearch-forward-regexp arg))

(defun isearchp-char-prop-backward-regexp (arg)
  "Regexp Isearch backward in text with a character (overlay or text) property.
See `isearchp-char-prop-backward'."
  (interactive "P")
  (isearchp-char-prop-1 'isearch-backward-regexp arg))

(defun isearchp-toggle-complementing-domain () ; Bound to `C-M-~' during Isearch.
  "Toggle searching the complements of the normal search contexts.
This toggles internal variable `isearchp-complement-domain-p'.
Bound to `C-M-~' during Isearch."
  (interactive)
  (setq isearchp-complement-domain-p  (not isearchp-complement-domain-p))
  (message "%s the search domain now" (if isearchp-complement-domain-p
                                          "*COMPLEMENTING*"
                                        "*NOT* complementing")))

(defun isearchp-add-regexp-as-property (property regexp &optional beg end predicate action msgp)
  "Add PROPERTY with value (REGEXP . PREDICATE) to REGEXP matches.
If region is active, limit action to region.  Else, use whole buffer.
If REGEXP has subgroups, then use what the Nth subgroup matches as the
 search context (hit), where N = `isearchp-context-level'.
 If N=0, then use the overall match of REGEXP as the search context.
PROPERTY is a text property to add to the search-hit text.
 the value of the property is REGEXP (a string).
 Interactively, PREDICATE is nil, so the PROPERTY value is (REGEXP).

Non-interactively:

BEG, END are the region limits.  If nil, the buffer limits are used.
PREDICATE is nil or a boolean function that takes these arguments:
  - the search-context string
  - a marker at the end of the search-context
If PREDICATE is non-nil then act on only the hits for which it holds.
If ACTION is non-nil then it is a function that accepts no arguments.
 It is called after matching buffer text with REGEXP.  After ACTION,
 the search hit end position is extended or restricted to point.
Non-interactively, non-nil BEG and END are used as the region limits."
  (interactive (let* ((prop     (intern (read-string "Text property: ")))
                      (regionp  (and transient-mark-mode  mark-active)))
                 (list prop
                       (isearchp-read-context-regexp)
                       (if regionp (region-beginning) (point-min))
                       (if regionp (region-end) (point-max))
                       nil
                       nil
                       'MSGP)))
  (let ((prop-value  (isearchp-regexp-scan beg end property regexp predicate action)))
    (when msgp
      (if prop-value
          (message "Prop value added: `%s'" prop-value)
        (message "No property added - no match for %s" regexp)))))

(defun isearchp-regexp-context-search (new beg end property regexp &optional predicate action)
  "Search within contexts defined by a regexp.
If `isearchp-complement-domain-p' is non-nil then search *outside* the
contexts defined by the regexp.  (Use `C-M-~' during Isearch to toggle
this variable.

IMPORTANT: By default, this command assumes that the search contexts
have already been defined, by a previous use of this command,
`isearchp-imenu', or `isearchp-regexp-define-contexts'.

If you do not use a prefix arg then this command searches for the
character property that you last used in an Isearch command that
searches or applies character properties.  This includes the
`isearch-char-prop-*' commands, the `isearchp-imenu*' commands, and
`isearchp-thing'.  See `isearchp-char-prop-forward' for more
information.

If you use a prefix arg, or if you have not previously used this
command or similar commands, then you are prompted for the
context-defining regexp.

If the regexp has subgroups, then you are prompted for the subgroup to
use to define the contexts.  Subgroup 0 means use the entire regexp
match as a context.  1 means use the first regexp group match as a
context.  And so on.

With a negative prefix arg you are prompted also for a Boolean
function that takes these arguments:

  - the search-hit string (what matches the regexp or chosen subgroup)
  - a marker at the end of the search-context

Only the search hits for which the predicate holds are retained.

The search contexts are marked in the buffer using a text property.
You can also search them using `isearch-char-prop-*' commands.  The
text property used is the symbol whose name is the regexp.  The
property value is a cons whose car is the regexp (a string) and whose
cdr is PREDICATE."
  (interactive (cons current-prefix-arg (isearchp-regexp-read-args)))
  (when (or new
            (not (consp (car isearchp-char-prop-values)))
            (not (equal (caar isearchp-char-prop-values) regexp))
            (not (eq isearchp-char-prop-prop (intern regexp)))
            (not (isearchp-text-prop-present-p beg end (intern regexp) (cons regexp predicate))))
    (isearchp-regexp-define-contexts beg end property regexp predicate action))
  (isearchp-char-prop-forward nil))

(defun isearchp-regexp-define-contexts (beg end property regexp &optional predicate action)
  "Define search contexts for a future character-property search.
This command does not actually search the contexts.  For that, use
`isearchp-regexp-context-search' or `isearchp-char-prop-forward'."
  (interactive (isearchp-regexp-read-args))
  (message "Scanning for regexp matches...")
  (let ((matches-p  (isearchp-regexp-scan beg end property regexp predicate action)))
    (setq isearchp-char-prop-prop    property
          isearchp-char-prop-type    'text
          isearchp-char-prop-values  (list (cons regexp predicate)))
    (message (if matches-p "Scanning for regexp matches...done" "NO MATCH for regexp"))))
   
;;(@* "General Non-Interactive Functions")

;;; General Non-Interactive Functions --------------------------------

(defun isearchp-char-prop-1 (search-fn arg)
  "Helper for `isearchp-char-prop-(forward|backward)(-regexp)'."
  (isearch-done)
  (when isearch-mode
    (let ((message-log-max  nil))
      (message "CHAR PROP %s%s"
               (isearchp-message-prefix nil nil isearch-nonincremental) isearch-message))
    (sit-for 1))
  (setq isearch-success   t
        isearch-adjusted  t)
  (let* ((enable-recursive-minibuffers    t)
         ;; Prevent invoking `isearch-edit-string', from `isearch-exit'.
         (search-nonincremental-instead   nil)
         ;; Test *-prop, not *-type, for TYPE, because nil means both.
         (type     (if (or arg  (not isearchp-char-prop-prop))
                       (if (not isearchp-char-prop-prop)
                           (let ((typname
                                  (completing-read
                                   "Type: " '(("text") ("overlay") ("text and overlay"))
                                   nil t nil nil "text and overlay")))
                             (and (not (string= "text and overlay" typname))  (intern typname)))
                         (and (atom arg) ; `C-u' means nil (both).
                              (if (wholenump (prefix-numeric-value arg)) 'text 'overlay)))
                     isearchp-char-prop-type))
         (props    (and (or arg  (not isearchp-char-prop-prop))
                        (mapcar #'(lambda (prop) (list (symbol-name prop)))
                                (isearchp-char-properties-in-buffer
                                 (current-buffer) (point-min) (point-max) type))))
         (prop     (if (or arg  (not isearchp-char-prop-prop))
                       (intern (completing-read
                                (format "%s property to %ssearch: "
                                        (if type (capitalize (symbol-name type)) "Character")
                                        (if isearchp-complement-domain-p "NOT " ""))
                                props nil nil nil nil "face"))
                     isearchp-char-prop-prop))
         (values   (if (or arg  (not isearchp-char-prop-values))
                       (if (memq prop '(face font-lock-face))
                           (isearchp-read-face-names)
                         (isearchp-read-sexps))
                     isearchp-char-prop-values)))
    (setq isearchp-filter-predicate-orig  isearch-filter-predicate
          isearch-filter-predicate        (isearchp-char-prop-filter-pred type prop values)
          isearchp-char-prop-type         type
          isearchp-char-prop-prop         prop
          isearchp-char-prop-values       values))
  (add-hook 'isearch-mode-end-hook 'isearchp-char-prop-end)
  (when (and transient-mark-mode  mark-active) (deactivate-mark))
  (funcall search-fn))

;; Same as `icicle-char-properties-in-buffer', defined in `icicles-cmd2.el'.
(defun isearchp-char-properties-in-buffer (&optional buffer beg end type)
  "List of all character properties in BUFFER between BEG and END.
Only the character properties are included, not their values.
TYPE can be `overlay', `text', or nil, meaning overlay properties,
text properties, or both, respectively."
  (unless buffer (setq buffer  (current-buffer)))
  (let ((props  ())
        ovrlays curr-props)
    (when (bufferp buffer)     ; Do nothing if BUFFER is not a buffer.
      (with-current-buffer buffer
        (unless (and beg  end)
          (setq beg  (point-min)
                end  (point-max)))
        (when (or (not type)  (eq type 'overlay)) ; Get overlay properties.
          (setq ovrlays  (overlays-in beg end))
          (dolist (ovrly  ovrlays)
            (setq curr-props  (overlay-properties ovrly))
            (while curr-props
              (unless (memq (car curr-props) props) (push (car curr-props) props))
              (setq curr-props  (cddr curr-props)))))
        (when (or (not type)  (eq type 'text)) ; Get text properties.
          (while (< beg end)
            (setq beg         (or (next-property-change beg nil end)  end)
                  curr-props  (text-properties-at beg))
            (while curr-props
              (unless (memq (car curr-props) props) (push (car curr-props) props))
              (setq curr-props  (cddr curr-props)))))))
    props))

(defun isearchp-char-prop-filter-pred (type prop values)
  "Return a predicate that uses `isearchp-char-prop-matches-p'.
TYPE, PROP, and VALUES are used by that function.
The predicate is suitable as a value of `isearch-filter-predicate'."
  (let ((tag  (make-symbol "isearchp-char-prop-filter-pred")))
    `(lambda (beg end)
       (and (or
             (and (fboundp 'isearch-filter-visible) (isearch-filter-visible beg end))
             (and (boundp 'isearch-invisible) ; Emacs 24.4+
                  (not (or (eq search-invisible t)  (not (isearch-range-invisible beg end))))))
            (catch ',tag
              (while (< beg end)
                (let ((matches-p  (isearchp-char-prop-matches-p
                                   ',type ',prop ',values (isearchp-char-prop-default-match-fn ',prop)
                                   beg)))
                  (unless (if matches-p
                              (not isearchp-complement-domain-p)
                            isearchp-complement-domain-p)
                    (throw ',tag nil)))
                (setq beg  (1+ beg)))
              t)))))

(defun isearchp-text-prop-present-p (beg end property value)
  "Return non-nil if some text between BEG and END has PROPERTY of VALUE."
  (unless (and beg  end) (setq beg  (point-min)
                               end  (point-max)))
  (save-excursion
    (let ((pos  beg)
          propval)
      (goto-char pos)
      (catch 'isearchp-text-prop-present-p
        (while (<= pos end)
          (setq pos      (or (next-property-change pos nil end)  end)
                propval  (get-text-property pos property))
          (if (and propval  (equal propval value))
              (throw 'isearchp-text-prop-present-p t)
            (setq pos  (1+ pos))))
        nil))))

;; Same as `icicle-search-char-prop-matches-p', defined in `icicles-cmd2.el'.
(defun isearchp-char-prop-matches-p (type property values match-fn position)
  "Return non-nil if POSITION has PROPERTY with a value matching VALUES.
TYPE is `overlay', `text', or nil, and specifies the type of character
property - nil means look for both overlay and text properties.

Find text with a PROPERTY value that overlaps with VALUES: If the
value of PROPERTY is an atom, then it must be a member of VALUES.  If
it is a list, then at least one list element must be a member of
VALUES.

MATCH-FN is a binary predicate that is applied to each item of VALUES
and a zone of text with property PROP.  If it returns non-nil then the
zone is a search hit."
  (let* ((ovlyval  (and (or (not type)  (eq type 'overlay))
                        (get-char-property position property)))
         (textval  (and (or (not type)  (eq type 'text))
                        (get-text-property position property))))
    (or (and ovlyval  (isearchp-some values ovlyval match-fn))
        (and textval  (isearchp-some values textval match-fn)))))

;; Same as `icicle-some', defined in `icicles-fn.el'.
(defun isearchp-some (list arg2 predicate)
  "Apply binary PREDICATE successively to an item of LIST and ARG2.
Return the first non-nil value returned by PREDICATE, or nil if none.
PREDICATE must be a function with two required arguments."
  (let ((result  nil))
    (catch 'isearchp-some
      (dolist (arg1  list)
        (when (setq result  (funcall predicate arg1 arg2))  (throw 'isearchp-some result))))
    result))

;; Same as `icicle-search-property-default-match-fn', defined in `icicles-cmd2.el'.
(defun isearchp-char-prop-default-match-fn (prop)
  "Return the default match function for text or overlay property PROP.
Properties `face' and `mumamo-major-mode' are handled specially.
For other properties the values are matched using `equal'."
  (case prop
    ((face font-lock-face) (lambda (val rprop)
                             (if (consp rprop)
                                 (condition-case nil ; Allow for dotted cons.
                                     (member val rprop)
                                   (error nil))
                               (eq val rprop))))
    ((mumamo-major-mode)   (lambda (val rprop) (equal val (car rprop))))
    (t                     #'equal)))

(defun isearchp-char-prop-end ()
  "End Isearch for a character property."
  (setq isearch-filter-predicate  isearchp-filter-predicate-orig)
  (remove-hook 'isearch-mode-end-hook 'isearchp-char-prop-end))

(defun isearchp-put-prop-on-region (property value beg end)
  "Add text PROPERTY with VALUE to the region from BEG to END.
If you have already used any of the commands `isearchp-char-prop-*'
and you do not use a prefix argument, then use the property and (the
first of) its values that you last specified for such searching.

Otherwise, you are prompted for the property and its value.

If the property is not `face' or `font-lock-face', then you enter a
sexp, which is read as the Lisp value to use.  E.g., if the property
is `mumamo-major-mode' then you might enter `(emacs-lisp-mode)' as the
value.

If the property is `face' or `font-lock-face' then you can specify
more than one face - their union is used as the property value.  If
you specify none (empty input immediately) then *all* faces are
*removed* from the region."
  (interactive
   (if (and (not current-prefix-arg)  isearchp-char-prop-prop  (car isearchp-char-prop-values))
       (list isearchp-char-prop-prop isearchp-char-prop-values (region-beginning) (region-end))
     (let* ((props  (and (or current-prefix-arg  (not isearchp-char-prop-prop))
                         (mapcar #'(lambda (prop) (list (symbol-name prop)))
                                 (isearchp-char-properties-in-buffer
                                  (current-buffer) (point-min) (point-max) 'text))))
            (prop   (intern (completing-read "Text property: " props nil nil nil nil "face")))
            (vals   (if (memq prop '(face font-lock-face))
                        (isearchp-read-face-names 'EMPTY-MEANS-NONE-P)
                      (isearchp-read-sexps 'ONLY-ONE-P))))
       (list prop vals (region-beginning) (region-end)))))
  (let ((buffer-mod  (buffer-modified-p)))
    (add-text-properties beg end (list property value))
    (set-buffer-modified-p buffer-mod)))

(defun isearchp-message-prefix (&optional arg1 arg2 arg3)
  "Version of `isearch-message-prefix' that works for all Emacs releases."
  (if (or (< emacs-major-version 24)
          (and (= emacs-major-version 24)  (< emacs-minor-version 3)
               (not (string-match "^[0-9]+\\.[0-9]+\\.[0-9]+" emacs-version))))
      (isearch-message-prefix arg1 arg2 arg3) ; Emacs 20 through 24.2.
    (isearch-message-prefix arg1 arg2))) ; Emacs 24.1.N and 24.3+

(defun isearchp-read-face-names  (&optional empty-means-none-p only-one-p)
  "Read face names with completion, and return a list of their symbols.
If user hits `RET' with empty input immediately, then include all
faces.  Otherwise, read faces one by one, until user hits `RET' twice
consecutively.

Non-nil optional arg EMPTY-MEANS-NONE-P means return nil (no face
names) for empty user input.

Non-nil optional arg ONLY-ONE-P means read only one face name and
return its symbol.

If you use also library Icicles then face-name candidates show their
face in buffer `*Completions*' (WYSIWYG) - see option
`icicle-WYSIWYG-Completions-flag'."
  (let ((icicle-multi-completing-p                   t)
        (icicle-list-nth-parts-join-string           ": ")
        (icicle-list-join-string                     ": ")
        (icicle-list-use-nth-parts                   '(1))
        (icicle-proxy-candidates
         (and (boundp 'icicle-add-proxy-candidates-flag)  icicle-add-proxy-candidates-flag
              (append (and (fboundp 'eyedrop-face-at-point)  (list "*point face name*"))
                      (let ((ipc  ()))
                        (mapatoms
                         (lambda (cand)
                           (when (and (user-variable-p cand)  (eq (get cand 'custom-type) 'face))
                             (push `,(concat "'" (symbol-name cand) "'") ipc))))
                        ipc))))
        (face-cands                                  (mapcar
                                                      (if (and (boundp 'icicle-mode)  icicle-mode)
                                                          #'icicle-make-face-candidate
                                                        (lambda (face) (list (symbol-name face))))
                                                      (face-list)))
        (faces                                       ())
        (prompt1                                     "Face (RET for each, empty input to finish): ")
        (prompt2                                     "Face: ")
        (icicle-unpropertize-completion-result-flag  t)
        face)
    (when (and (boundp 'icicle-mode)  icicle-mode)
      (put-text-property 0 1 'icicle-fancy-candidates t prompt1)
      (put-text-property 0 1 'icicle-fancy-candidates t prompt2))
    (setq face  (isearchp-read-face-names--read prompt1 face-cands))
    (if (and empty-means-none-p  (string= "" face))
        ()
      (if only-one-p
          face
        (if (string= "" face)
            (setq faces  (face-list))
          (setq face-cands  (delete (assoc face face-cands) face-cands))
          (while (not (string= "" face))
            (add-to-list 'faces (intern face))
            (setq face        (isearchp-read-face-names--read prompt2 face-cands)
                  face-cands  (delete (assoc face face-cands) face-cands)))
          (nreverse faces))))))

(defun isearchp-read-face-names--read (prompt candidates)
  "Read a face name using PROMPT and face-name completion CANDIDATES."
  (if (and (boundp 'icicle-mode)  icicle-mode)
      (icicle-transform-multi-completion
       (completing-read
        prompt candidates nil (not (stringp icicle-WYSIWYG-Completions-flag)) nil
        (if (boundp 'face-name-history) 'face-name-history 'icicle-face-name-history)))
    (completing-read prompt candidates nil t nil 'face-name-history)))

(defun isearchp-read-sexps  (&optional only-one-p)
  "Read sexps with completion, and return them as a list.
Read sexps one by one, until user hits `RET' twice consecutively.
Non-nil ONLY-ONE-P means read only one sexp and return it."
  (let ((sexp-cands                         (mapcar #'list (isearchp-remove-duplicates
                                                            read-expression-history)))
        (sexps                              ())
        (prompt1                            "Sexp (RET for each, empty input to finish): ")
        (prompt2                            "Sexp: ")
        sexp)
    (setq sexp        (completing-read (if only-one-p prompt2 prompt1) sexp-cands
                                       nil nil nil 'read-expression-history)
          sexp-cands  (delete (assoc sexp sexp-cands) sexp-cands))
    (if only-one-p
        (car (read-from-string sexp))
      (while (not (string= "" sexp))
        (add-to-list 'sexps sexp)
        (setq sexp        (completing-read prompt2 sexp-cands nil nil nil 'read-expression-history)
              sexp-cands  (delete (assoc sexp sexp-cands) sexp-cands)))
      (prog1 (setq sexps  (nreverse (delete "" sexps)) ; Return the list of sexps.
                   sexps  (mapcar (lambda (sx) (car (read-from-string sx))) sexps))
        (when (interactive-p) (message "Sexps: %S" sexps))))))


(defun isearchp-read-context-regexp ()
  "Read context regexp and determine `isearchp-context-level'."
  (let* ((prompt  "Search %s contexts (regexp): ")
         (prompt  (format prompt (if isearchp-complement-domain-p "*OUTSIDE*" "within")))
         (regexp  (read-regexp prompt)))
    (while (string= "" regexp)
      (message "Regexp cannot be empty.  Try again...") (sit-for 2)
      (setq regexp  (read-regexp prompt)))
    (setq isearchp-context-level  (if (string-match "\\\\(" regexp)
                                      (truncate (read-number
                                                 "Subgroup to use as search context [0, 1, 2,...]: "
                                                 0))
                                    0))
    regexp))

(defun isearchp-regexp-scan (beg end property regexp &optional predicate action)
  "Scan buffer for REGEXP, adding text property PROPERTY to matches.
Return the value of PROPERTY, if added somewhere.  Else return nil.
See `isearchp-add-regexp-as-property' for the parameter descriptions."
  (setq regexp  (or regexp  (isearchp-read-context-regexp)))
  (when (stringp property) (setq property  (intern property)))
  (unless (and beg  end) (setq beg  (point-min)
                               end  (point-max)))
  (unless (< beg end) (setq beg  (prog1 end (setq end  beg)))) ; Ensure BEG is before END.
  (let ((last-beg      nil)
        (added-prop-p  nil))
    (condition-case-no-debug isearchp-regexp-scan
        (save-excursion
          (goto-char (setq last-beg  beg))
          (while (and beg  (< beg end)  (not (eobp))
                      (progn (while (and (setq beg  (re-search-forward regexp end t))
                                         (eq last-beg beg)
                                         (not (eobp)))
                               ;; Matched again, same place.  Advance 1 char.
                               (forward-char) (setq beg  (1+ beg)))
                             ;; Stop if no more match.  But if complementing then continue until eobp.
                             (or beg  isearchp-complement-domain-p)))
            (unless (or (not beg)  (match-beginning isearchp-context-level)) ; No `user-error': Emacs 23
              (error "Search context has no subgroup of level %d - try a lower number"
                     isearchp-context-level))
            (let* ((hit-beg     (if isearchp-complement-domain-p
                                    last-beg
                                  (match-beginning isearchp-context-level)))
                   (hit-end     (if isearchp-complement-domain-p
                                    (if beg
                                        (match-beginning isearchp-context-level)
                                      (point-max))
                                  (match-end isearchp-context-level)))
                   (IGNORE      (when action (save-excursion (funcall action) (setq hit-end  (point)))))
                   (hit-string  (buffer-substring-no-properties hit-beg hit-end))
                   end-marker)
              (if (and (not (string= "" hit-string))
                       (setq end-marker  (copy-marker hit-end))
                       (or (not predicate)
                           (save-match-data (funcall predicate hit-string end-marker))))
                  ;; Put (REGEXP . PREDICATE) on hit text as text property PROPERTY.
                  ;; It is not enough to record REGEXP.  E.g., `*-imenu-non-interactive-function' and
                  ;; `*-imenu-command' use the same REGEXP.  Only the PREDICATE is different.
                  (let ((buffer-mod  (buffer-modified-p))
                        (prop-value  (cons regexp predicate)))
                    (put-text-property hit-beg hit-end property prop-value)
                    (put-text-property hit-beg hit-end 'isearchp t)
                    (setq added-prop-p  prop-value)
                    (set-buffer-modified-p buffer-mod))
                (remove-text-properties hit-beg hit-end (list property 'IGNORED))))
            (setq last-beg  beg)))
      (error (error "%s" (error-message-string isearchp-regexp-scan))))
    added-prop-p)) ; Return property value if added, or nil otherwise.

(defun isearchp-regexp-read-args ()
  "Read args for `isearchp-regexp*'."
  (let* ((regionp  (and transient-mark-mode  mark-active))
         (beg      (if regionp (region-beginning) (point-min)))
         (end      (if regionp (region-end) (point-max)))
         (regxp    (if (or current-prefix-arg
                           (not (consp (car isearchp-char-prop-values)))
                           (not (stringp (caar isearchp-char-prop-values))))
                       (isearchp-read-context-regexp)
                     (caar isearchp-char-prop-values)))
         (prop     (if (or current-prefix-arg
                           (not (consp (car isearchp-char-prop-values)))
                           (not (stringp (caar isearchp-char-prop-values))))
                       (intern regxp)
                     isearchp-char-prop-prop))
         (pred     (and current-prefix-arg  (< (prefix-numeric-value current-prefix-arg) 0)
                        (read-from-minibuffer
                         "Predicate to filter search contexts: "
                         nil read-expression-map t (and (boundp 'function-name-history)
                                                        'function-name-history)))))
    (list beg end prop regxp pred)))

;; Same as `icicle-remove-duplicates'.
(defun isearchp-remove-duplicates (sequence &optional test)
  "Copy of SEQUENCE with duplicate elements removed.
Optional arg TEST is the test function.  If nil, test with `equal'.
See `make-hash-table' for possible values of TEST."
  (setq test  (or test  #'equal))
  (let ((htable  (make-hash-table :test test)))
    (loop for elt in sequence
       unless (gethash elt htable)
       do     (puthash elt elt htable)
       finally return (loop for i being the hash-values in htable collect i))))

 
;;(@* "Imenu Commands and Functions")

;;; Imenu Commands and Functions -------------------------------------

(defun isearchp-imenu ()
  (interactive)
  "Search Imenu entries.
A search context is the text between the beginning of the Imenu regexp
match and `forward-sexp' from there.

IMPORTANT: By default, this command assumes that the search contexts
have already been defined, by a previous use of this command,
`isearchp-regexp-context-search', or
`isearchp-regexp-define-contexts'.

If you do not use a prefix arg then this command searches for the
character property that you last used in an Isearch command that
searches or applies character properties.  This includes the
`isearch-char-prop-*' commands, the `isearchp-imenu*' commands, and
`isearchp-thing'.  See `isearchp-char-prop-forward' for more
information.

If you use a prefix arg, or if you have not previously used this
command or similar commands, this command defines the search contexts
and then searches them"
  (isearchp-imenu-1 current-prefix-arg))

(defun isearchp-imenu-command ()
  "Search Emacs command definitions.
This uses `commandp', so it finds only currently defined commands.
That is, if the buffer has not been evaluated, then its function
definitions are NOT considered commands by `isearchp-imenu-command'.
See `isearchp-imenu' for more information."
  (interactive)
  (unless (eq major-mode 'emacs-lisp-mode)
    (error "This command is only for Emacs-Lisp mode")) ; No `user-error' in Emacs 23.
  (isearchp-imenu-1 current-prefix-arg
                    (lambda (_hit _mrkr)
                      (commandp (intern-soft
                                 (buffer-substring-no-properties (match-beginning 2) (match-end 2)))))
                    (lambda (menus)
                      (or (car (assoc "Functions" menus))
                          (car (assoc "Other" menus))
                          (error "No command definitions in buffer"))))) ; No `user-error' in Emacs 23.

(defun isearchp-imenu-non-interactive-function ()
  "Search Emacs non-command function definitions.
See `isearchp-imenu' for more information."
  (interactive)
  (unless (eq major-mode 'emacs-lisp-mode)
    (error "This command is only for Emacs-Lisp mode")) ; No `user-error' in Emacs 23.
  (isearchp-imenu-1 current-prefix-arg
                    (lambda (_hit _mrkr)
                      (let ((fn  (intern-soft
                                  (buffer-substring-no-properties (match-beginning 2) (match-end 2)))))
                        (and (fboundp fn)  (not (commandp fn)))))
                    (lambda (menus)
                      (or (car (assoc "Functions" menus))
                          (car (assoc "Other" menus))
                          (error "No non-command function definitions in buffer"))))) ; No `user-error'

(defun isearchp-imenu-macro ()
  "Search Lisp macro definitions.
See `isearchp-imenu' for more information."
  (interactive)
  (unless (memq major-mode '(emacs-lisp-mode lisp-mode))
    (error "This command is only for Emacs-Lisp mode or Lisp mode")) ; No `user-error' in Emacs 23.
  (isearchp-imenu-1 current-prefix-arg
                    (lambda (_hit _mrkr)
                      (let ((fn  (intern-soft
                                  (buffer-substring-no-properties (match-beginning 2) (match-end 2)))))
                        (if (fboundp 'macrop) ; Emacs 24.4+
                            (macrop fn)
                          (and (fboundp fn)
                               (let ((def  (symbol-function fn)))
                                 (and (consp def)  (eq (car def) 'macro)))))))
                    (lambda (menus)
                      (or (car (assoc "Macro" menus))
                          (car (assoc "Other" menus))
                          (error "No macro definitions in buffer"))))) ; No `user-error' in Emacs 23.

(defun isearchp-imenu-1 (new &optional predicate submenu-fn)
  "Helper for `isearchp-imenu*' commands.
BEG, END are the region limits.  If nil, the buffer limits are used.
Non-nil PREDICATE means act on only the hits for it holds.  It is a
Boolean function that takes these args:
  - the search-context string
  - a marker at the end of the search-context
SUBMENU-FN is a function to apply to the list of Imenu submenus to
 choose one.  If nil then the user chooses one using completion."
  (unless imenu-generic-expression
    (error "No Imenu pattern for this buffer")) ; No `user-error' in Emacs 23.
  (let ((case-fold-search  (if (or (local-variable-p 'imenu-case-fold-search)
                                   (not (local-variable-p 'font-lock-defaults)))
                               imenu-case-fold-search
                             (nth 2 font-lock-defaults)))
        (old-table         (syntax-table))
        (table             (copy-syntax-table (syntax-table)))
        (slist             imenu-syntax-alist)
        (beg               (if (and transient-mark-mode  mark-active) (region-beginning) (point-min)))
        (end               (if (and transient-mark-mode  mark-active) (region-end) (point-max)))
        prop)
    (dolist (syn  slist) ; Modify the syntax table used while matching regexps.
      (if (numberp (car syn))
          (modify-syntax-entry (car syn) (cdr syn) table) ; Single character.
        (dolist (char  (car syn))  (modify-syntax-entry char (cdr syn) table)))) ; String.
    (unwind-protect
         (save-match-data
           (set-syntax-table table)
           (let* ((others   0)
                  (menus    (mapcar (lambda (menu)
                                      (when (equal (car menu) "Other")
                                        (setq others  (1+ others))
                                        (when (> others 1)
                                          (setcar menu (format "Other<%d>" others))))
                                      menu)
                                    (isearchp-remove-if-not
                                     #'isearchp-imenu-in-buffer-p ; Use only menus that match buffer.
                                     (mapcar (lambda (menu) ; Name unlabeled menu(s) `Other[<N>]'.
                                               (if (stringp (car menu))
                                                   menu
                                                 (cons "Other" (cdr menu))))
                                             imenu-generic-expression))))
                  (submenu  (if submenu-fn
                                (funcall submenu-fn menus)
                              (if (cadr menus)
                                  (let ((completion-ignore-case  t))
                                    (completing-read "Choose: " menus nil t))
                                (caar menus)))) ; Only one submenu, so use it.
                  (regexp   (cadr (assoc submenu menus))))
             (unless (stringp regexp) (error "No match")) ; No `user-error' in Emacs 23.
             (isearchp-regexp-context-search new beg end (intern regexp) regexp predicate
                                             ;; We rely on the match data having been preserved.
                                             ;; $$$$$$ An alternative fn for Lisp only:
                                             ;; (lambda () (up-list -1) (forward-sexp))))))
                                             (lambda ()
                                               (goto-char (match-beginning 0))
                                               (condition-case isearchp-imenu-1
                                                   (forward-sexp)
                                                 (error (goto-char (match-end 0))))))))
      (set-syntax-table old-table))))

(defun isearchp-imenu-in-buffer-p (menu)
  "Return non-nil if the regexp in MENU has a match in the buffer."
  (save-excursion (goto-char (point-min)) (re-search-forward (cadr menu) nil t)))

(defun isearchp-remove-if-not (pred xs)
  "A copy of list XS with only elements that satisfy predicate PRED."
  (let ((result  ()))
    (dolist (x xs) (when (funcall pred x) (push x result)))
    (nreverse result)))

 
;;(@* "THING Commands and Functions")

;;; THING Commands and Functions" ------------------------------------

;; Same as `hide/show-comments' in `hide-comnt.el'.
;;
(defun isearchp-hide/show-comments (&optional hide/show start end) ; Bound to `M-;' during Isearch
  "Hide or show comments from START to END.
Interactively, hide comments, or show them if you use a prefix arg.
Interactively, START and END default to the region limits, if active.
Otherwise, including non-interactively, they default to `point-min'
and `point-max'.

During Isearch this is bound to `M-;'.  However, in order to use a
prefix arg with it you must set `isearch-allow-scroll' or
`isearch-allow-prefix' (if available) to non-nil.  Otherwise, a prefix
arg during Isearch exits Isearch.

Uses `save-excursion', restoring point.

Be aware that using this command to show invisible text shows *all*
such text, regardless of how it was hidden.  IOW, it does not just
show invisible text that you previously hid using this command.

From Lisp, a HIDE/SHOW value of `hide' hides comments.  Other values
show them."
  (interactive (list (if current-prefix-arg 'show 'hide)
                     (if (and transient-mark-mode  mark-active) (region-beginning) (point-min))
                     (if (and transient-mark-mode  mark-active) (region-end) (point-max))))
  (when (require 'newcomment nil t)     ; `comment-search-forward'
    (comment-normalize-vars)     ; Per Stefan, should call this first.
    (unless start (setq start  (point-min)))
    (unless end   (setq end    (point-max)))
    (unless (<= start end) (setq start  (prog1 end (setq end  start))))
    (let ((bufmodp           (buffer-modified-p))
          (buffer-read-only  nil)
          cbeg cend)
      (unwind-protect
           (save-excursion
             (goto-char start)
             (while (and (< start end)  (setq cbeg  (comment-search-forward end 'NOERROR)))
               (setq cend  (if (string= "" comment-end)
                               (min (1+ (line-end-position)) (point-max))
                             (search-forward comment-end end 'NOERROR)))
               (when (and cbeg  cend)
                 (if (eq 'hide hide/show)
                     (put-text-property cbeg cend 'invisible t)
                   (put-text-property cbeg cend 'invisible nil)))))
        (set-buffer-modified-p bufmodp)))))

;;; Same as `with-comments-hidden' in `hide-comnt.el', except doc here mentions `C-M-;'.
(defmacro isearchp-with-comments-hidden (start end &rest body)
  "Evaluate the forms in BODY while comments are hidden from START to END.
But if `isearchp-ignore-comments-flag' is nil, just evaluate BODY,
without hiding comments.  Show comments again when BODY is finished.
You can toggle `isearchp-ignore-comments-flag' using `C-M-;' in the
minibuffer, but depending on when you do so you might need to invoke
the current command again.

See `isearchp-hide/show-comments', which is used to hide and show the
comments."
  (let ((result  (make-symbol "result"))
        (ostart  (make-symbol "ostart"))
        (oend    (make-symbol "oend")))
    `(let ((,ostart  ,start)
           (,oend    ,end)
           ,result)
       (unwind-protect
            (setq ,result  (progn (when isearchp-ignore-comments-flag
                                    (isearchp-hide/show-comments 'hide ,ostart ,oend))
                                  ,@body))
         (when isearchp-ignore-comments-flag (isearchp-hide/show-comments 'show ,ostart ,oend))
         ,result))))

(defun isearchp-toggle-ignoring-comments () ; Bound to `C-M-;' during Isearch.
  "Toggle the value of option `isearchp-ignore-comments-flag'.
If option `ignore-comments-flag' is defined (in library
`hide-comnt.el') then it too is toggled.
Bound to `C-M-;' during Isearch."
  (interactive)
  (setq isearchp-ignore-comments-flag  (not isearchp-ignore-comments-flag))
  (when (boundp 'ignore-comments-flag) (setq ignore-comments-flag  (not ignore-comments-flag)))
  (message "Ignoring comments is now %s" (if isearchp-ignore-comments-flag "ON" "OFF")))

;--------------

(defun isearchp-thing (new thing beg end property &optional predicate transform-fn)
  "Search within THING search contexts.
That is, each zone of text searched is a THING.
Enter the type of THING to search: `sexp', `sentence', `list',
`string', `comment', etc.

Possible THINGs are those for which
`isearchp-bounds-of-thing-at-point' returns non-nil (and for which the
bounds are not equal: an empty thing).  This does not include
everything THING that is defined as a thing-at-point type.

If user option `isearchp-ignore-comments-flag' is nil then include
THINGs located within comments.  Non-nil (the default value) means to
ignore things inside comments for searching.  You can toggle this
option using `C-M-;' during Isearch, but to see the effect you will
need to invoke this command again, and with a prefix arg (see below).

IMPORTANT: By default, this command assumes that the search contexts
have already been defined by a previous use of this command.

If you do not use a prefix arg then this command searches for the
character property that you last used in an Isearch command that
searches or applies character properties.  This includes the
`isearch-char-prop-*' commands, the `isearchp-imenu*' commands.  See
`isearchp-char-prop-forward' for more information.

If you use a prefix arg, or if you have not previously used this
command or similar commands, then you are prompted for the THING and
the search contexts are created.  Text properties are added to the
contexts to identify them and allow for character-property searching.

Non-interactively, if optional arg PREDICATE is non-nil then it is a
predicate that acceptable things must satisfy.  It is passed the thing
in the form of the cons returned by
`isearchp-next-visible-thing-and-bounds'.

Non-interactively, if optional arg TRANSFORM-FN is non-nil then it is
a function to apply to each thing plus its bounds and which returns
the actual target to search in place of THING.  Its argument is the
same as PREDICATE's.

It returns the replacement search context for the thing plus its
bounds, in the same form: a cons (STRING START . END), where STRING is
the search hit string and START and END are its bounds).

You can alternatively choose to search, not the THINGs as search
contexts, but the non-THINGs (non-contexts), that is, the buffer text
that is outside THINGs.  To do this, use
`C-M-~' (`isearchp-toggle-completing-domain') during Isearch.

NOTE:

1. For best results, use also library `thingatpt+.el'.

2. In some cases it can take a while to compute the THING search
   contexts.  Use the command on an active region when you do not need
   to search THINGS throughout an entire buffer.

3. In `nxml-mode', remember that option `nxml-sexp-element-flag'
   controls what a `sexp' means.  To use whole XML elements as search
   contexts, set the option to t, not nil.  (This is already done for
   the predefined Isearch+ commands.)

4. The scan candidate things moves forward a THING at a time.  In
   particular, if either PREDICATE or TRANSFORM-FN disqualifies the
   thing being scanned currently, then scanning skips forward to the
   next thing.  The scan does not dig inside the current thing to look
   for a qualified THING."
  (interactive (cons current-prefix-arg (isearchp-thing-read-args)))
  (when (or new
            (not (consp (car isearchp-char-prop-values)))
            (not (equal (caar isearchp-char-prop-values) thing))
            (not (eq isearchp-char-prop-prop property))
            (not (isearchp-text-prop-present-p beg end property (cons thing predicate))))
    (isearchp-thing-define-contexts thing beg end property predicate))
  (isearchp-char-prop-forward nil))

(defun isearchp-thing-read-args ()
  "Read args for `isearchp-thing*'."
  (let* ((thng  (intern
                 (completing-read
                  (format "%shing (type): " (if isearchp-complement-domain-p "*NOT* t" "T"))
                  (isearchp-things-alist) nil nil nil nil (symbol-name isearchp-last-thing-type))))
         (beg   (if (and transient-mark-mode  mark-active) (region-beginning) (point-min)))
         (end   (if (and transient-mark-mode  mark-active) (region-end) (point-max)))
         (pred  (and current-prefix-arg  (< (prefix-numeric-value current-prefix-arg) 0)
                     (read-from-minibuffer
                      "Predicate to filter search contexts: "
                      nil read-expression-map t (and (boundp 'function-name-history)
                                                     'function-name-history))))
         (prop  (if (or current-prefix-arg
                        (not (consp (car isearchp-char-prop-values)))
                        (not (equal thng (caar isearchp-char-prop-values))))
                    (intern (format "isearchp-thing-%s" thng))
                  isearchp-char-prop-prop)))
    (list thng beg end prop pred)))

(defun isearchp-thing-define-contexts (thing beg end property &optional predicate transform-fn)
  "Define search contexts for future thing searches.
This command does not actually search the contexts.  For that, use
`isearchp-thing' or `isearchp-char-prop-forward'."
  (interactive (isearchp-thing-read-args))
  (message "Scanning for thing: `%s'..." thing)
  (isearchp-thing-scan beg end thing property predicate transform-fn)
  (setq isearchp-char-prop-prop    property
        isearchp-char-prop-type    'text
        isearchp-char-prop-values  (list (cons thing predicate)))
  (message "Scanning for thing: `%s'...done" thing))

(defun isearchp-thing-scan (beg end thing property &optional predicate transform-fn)
  "Scan buffer from BEG to END for things of type THING.
Create THING search contexts: Add text property PROPERTY to THING
matches.

Return non-nil if PROPERTY was added somewhere, nil otherwise.

If PREDICATE is non-nil then it is a predicate that acceptable things
must satisfy.  It is passed the thing plus its bounds, in the form of
the cons returned by `isearchp-next-visible-thing-and-bounds'.

If TRANSFORM-FN is non-nil then it is a function to apply to each
thing plus its bounds.  Its argument is the same as PREDICATE's.  It
returns the actual search-target to propertize, in place of THING.
That is, it returns the replacement for the thing plus its bounds, in
the same form: a cons (STRING START . END), where STRING is the search
hit string and START and END are its bounds).  It can also return nil,
in which case it acts as another predicate: the thing is not
propertized.

NOTE: The scan moves forward a THING at a time.  In particular, if
either PREDICATE or TRANSFORM-FN disqualifies the thing being scanned
currently, then scanning skips forward to the next thing.  The scan
does not dig inside the current thing to look for a qualified THING.

This function respects both `isearchp-search-complement-domain-p' and
`isearchp-ignore-comments-flag'."
  (unless (and beg  end) (setq beg  (point-min)
                               end  (point-max)))
  (unless (< beg end) (setq beg  (prog1 end (setq end  beg)))) ; Ensure BEG is before END.
  (when (stringp property) (setq property  (intern property)))
  (let ((last-beg      nil)
        (added-prop-p  nil))
    (isearchp-with-comments-hidden
     beg end
     (condition-case-no-debug isearchp-thing-scan
         (save-excursion
           (goto-char (setq last-beg  beg)) ; `isearchp-next-visible-thing-and-bounds' uses point.
           (while (and last-beg  (< last-beg end))
             (while (and (< beg end)  (invisible-p beg)) ; Skip invisible, overlay or text.
               (when (get-char-property beg 'invisible)
                 (setq beg  (next-single-char-property-change beg 'invisible nil end))))
             (let ((thg+bnds  (isearchp-next-visible-thing-and-bounds thing beg end)))
               (if (and (not thg+bnds)  (not isearchp-complement-domain-p))
                   (setq beg  end)
                 (let* ((thg-beg       (cadr thg+bnds))
                        (thg-end       (cddr thg+bnds))
                        (tr-thg-beg    thg-beg)
                        (tr-thg-end    thg-end)
                        (hit-beg       (if isearchp-complement-domain-p last-beg tr-thg-beg))
                        (hit-end       (if isearchp-complement-domain-p
                                           (or tr-thg-beg  end)
                                         tr-thg-end))
                        (hit-string    (buffer-substring-no-properties hit-beg hit-end))
                        (end-marker    (copy-marker hit-end))
                        (filteredp     (or (not predicate)
                                           (not thg+bnds)
                                           (funcall predicate thg+bnds)))
                        (new-thg+bnds  (if isearchp-complement-domain-p
                                           thg+bnds
                                         (and filteredp
                                              thg+bnds
                                              (if transform-fn
                                                  (funcall transform-fn thg+bnds)
                                                thg+bnds)))))
                   (cond ((and (not (string= "" hit-string)) ; No-op if empty hit.
                               (or new-thg+bnds  isearchp-complement-domain-p))
                          (when (and transform-fn  (not isearchp-complement-domain-p))
                            (setq hit-string  (car  new-thg+bnds)
                                  tr-thg-beg  (cadr new-thg+bnds)
                                  tr-thg-end  (cddr new-thg+bnds)
                                  end-marker  (copy-marker tr-thg-end)))
                         
                          (when (and isearchp-ignore-comments-flag  isearchp-complement-domain-p)
                            (put-text-property 0 (length hit-string) 'invisible nil hit-string))
                          (let ((buffer-mod  (buffer-modified-p))
                                (prop-value  (cons thing predicate)))
                            (put-text-property hit-beg hit-end property prop-value)
                            (put-text-property hit-beg hit-end 'isearchp t)
                            (setq added-prop-p  prop-value)
                            (set-buffer-modified-p buffer-mod)))
                         (t
                          (remove-text-properties hit-beg hit-end (list property 'IGNORED))))
                   (if thg-end
                       ;; $$$$$$
                       ;; The correct code here is (setq beg end).  However, unless you use my
                       ;; library `thingatpt+.el' or unless Emacs bug #9300 is fixed (hopefully
                       ;; in Emacs 24), that will loop forever.  In that case we move forward a
                       ;; char to prevent looping, but that means that the position just after
                       ;; a THING is considered to be covered by the THING (which is incorrect).
                       (setq beg  (if (or (featurep 'thingatpt+)  (> emacs-major-version 23))
                                      thg-end
                                    (1+ thg-end)))
                     ;; If visible then no more things - skip to END.
                     (unless (invisible-p beg) (setq beg  end)))))
               (setq last-beg  beg))))
       (error (error "%s" (error-message-string isearchp-thing-scan)))))
    added-prop-p))  ; Return indication of whether property was added.

;;--------------------------------------

;;; Same as `thgcmd-things-alist' in `thing-cmds.el'.
(defun isearchp-things-alist ()
  "Alist of most thing types currently defined.
Each is a cons (STRING), where STRING names a type of text entity for
which there is a either a corresponding `forward-'thing operation, or
corresponding `beginning-of-'thing and `end-of-'thing operations.  The
list includes the names of the symbols that satisfy
`isearchp-defined-thing-p', but with these excluded: `thing', `buffer',
`point'."
  (let ((types  ()))
    (mapatoms
     (lambda (tt)
       (when (isearchp-defined-thing-p tt) (push (symbol-name tt) types))))
    (dolist (typ  '("thing" "buffer" "point")) ; Remove types that do not make sense.
      (setq types (delete typ types)))
    (setq types  (sort types #'string-lessp))
    (mapcar #'list types)))

;;; Same as `thgcmd-defined-thing-p' in `thing-cmds.el'.
(defun isearchp-defined-thing-p (thing)
  "Return non-nil if THING (type) is defined as a thing-at-point type."
  (let ((forward-op    (or (get thing 'forward-op)  (intern-soft (format "forward-%s" thing))))
        (beginning-op  (get thing 'beginning-op))
        (end-op        (get thing 'end-op))
        (bounds-fn     (get thing 'bounds-of-thing-at-point))
        (thing-fn      (get thing 'thing-at-point)))
    (or (functionp forward-op)
        (and (functionp beginning-op)  (functionp end-op))
        (functionp bounds-fn)
        (functionp thing-fn))))

(defun isearchp-bounds-of-thing-at-point (thing &optional syntax-table)
  "`thingatpt+.el' version of `bounds-of-thing-at-point', if possible.
`tap-bounds-of-thing-at-point' if defined, else
`bounds-of-thing-at-point'.
if non-nil, set SYNTAX-TABLE for the duration."
  (if (fboundp 'tap-bounds-of-thing-at-point)
      (tap-bounds-of-thing-at-point thing syntax-table)
    (if (and (fboundp 'with-syntax-table)  (syntax-table-p syntax-table)) ; Emacs 21+.
        (with-syntax-table syntax-table (bounds-of-thing-at-point thing))
      (bounds-of-thing-at-point thing)))) ; Punt - ignore any SYNTAX-TABLE arg.

(defun isearchp-next-visible-thing-and-bounds (thing start end)
  "Return the next visible THING and its bounds.
Start at BEG and end at END, when searching for THING.
Return (THING THING-START . THING-END), with THING-START and THING-END
 the bounds of THING.  Return nil if no such THING is found.

The \"visible\" in the name refers to ignoring things that are within
invisible text, such as hidden comments.

You can toggle hiding of comments using `C-M-;' during Isearch, but
depending on when you do so you might need to invoke the current
command again.."
  (save-excursion (isearchp-next-visible-thing thing start end)))

;; Simple version of `previous-visible-thing' from `thing-cmds.el'.
;;
(defun isearchp-previous-visible-thing (thing start &optional end)
  "Same as `isearchp-next-visible-thing', except it moves backward."
  (interactive
   (list (or (and (memq last-command '(isearchp-next-visible-thing isearchp-previous-visible-thing))
                  isearchp-last-thing-type)
             (prog1 (intern (completing-read "Thing (type): " (isearchp-things-alist) nil nil nil nil
                                             (symbol-name isearchp-last-thing-type)))))
         (point)
         (if (and mark-active  (not (eq (region-beginning) (region-end))))
             (min (region-beginning) (region-end))
           (point-min))))
  (if (interactive-p)
      (isearchp-with-comments-hidden start end (isearchp-next-visible-thing thing start end 'BACKWARD))
    (isearchp-next-visible-thing thing start end 'BACKWARD)))

;; Simple version of `next-visible-thing' from `thing-cmds.el'.
;;
(defun isearchp-next-visible-thing (thing &optional start end backward)
  "Go to the next visible THING.
Start at START.  If END is non-nil then look no farther than END.
Interactively:
 - START is point.
 - If the region is not active, END is the buffer end.  If the region
   is active, END is the region end: the greater of point and mark.

Ignores (skips) comments if `isearchp-ignore-comments-flag' is
non-nil.  You can toggle this ignoring of comments using `C-M-;'
during Isearch, but depending on when you do so you might need to
invoke the current command again.

If you use this command or `isearchp-previous-visible-thing'
successively, even mixing the two, you are prompted for the type of
THING only the first time.  You can thus bind these two commands to
simple, repeatable keys (e.g. `f11', `f12'), to navigate among things
quickly.

Non-interactively, THING is a symbol, and optional arg BACKWARD means
go to the previous thing.

Return (THING THING-START . THING-END), with THING-START and THING-END
the bounds of THING.  Return nil if no such THING is found."
  (interactive
   (list (or (and (memq last-command '(isearchp-next-visible-thing isearchp-previous-visible-thing))
                  isearchp-last-thing-type)
             (prog1 (intern (completing-read "Thing (type): " (isearchp-things-alist) nil nil nil nil
                                             (symbol-name isearchp-last-thing-type)))))
         (point)
         (if (and mark-active  (not (eq (region-beginning) (region-end))))
             (max (region-beginning) (region-end))
           (point-max))))
  (setq isearchp-last-thing-type  thing)
  (unless start (setq start  (point)))
  (unless end   (setq end    (if backward (point-min) (point-max))))
  (cond ((< start end) (when backward (setq start  (prog1 end (setq end  start)))))
        ((> start end) (unless backward (setq start  (prog1 end (setq end  start))))))
  (if (interactive-p)
      (isearchp-with-comments-hidden start end (isearchp-next-visible-thing-1 thing start end backward))
    (isearchp-next-visible-thing-1 thing start end backward)))

;;; Same as `thgcmd-next-visible-thing-1' in `thing-cmds.el'.
(if (fboundp 'thgcmd-next-visible-thing-1)
    (defalias 'isearchp-next-visible-thing-1 'thgcmd-next-visible-thing-1)
  (defun isearchp-next-visible-thing-1 (thing start end backward)
    "Helper for `isearchp-next-visible-thing'.  Get thing past point."
    (let ((thg+bds  (isearchp-next-visible-thing-2 thing start end backward)))
      (if (not thg+bds)
          nil
        ;; $$$$$$ Which is better, > or >=, < or <=, for the comparisons?
        ;; $$$$$$ Seems that < is better than <=, at least for `isearchp-search-thing':
        ;; $$$$$$ for XML elements and lists, <= misses the first one.
        (while (and thg+bds  (if backward (> (cddr thg+bds) (point)) (< (cadr thg+bds) (point))))
          (if backward
              (setq start  (max end (1- (cadr thg+bds))))
            (setq start  (min end (1+ (cddr thg+bds)))))
          (setq thg+bds  (isearchp-next-visible-thing-2 thing start end backward)))
        (when thg+bds (goto-char (cadr thg+bds)))
        thg+bds))))

;;; Same as `thgcmd-next-visible-thing-2' in `thing-cmds.el'.
(if (fboundp 'thgcmd-next-visible-thing-2)
    (defalias 'isearchp-next-visible-thing-2 'thgcmd-next-visible-thing-2)
  (defun isearchp-next-visible-thing-2 (thing start end &optional backward)
    "Helper for `isearchp-next-visible-thing-1'.  Thing might not be past START."
    (and (not (= start end))
         (save-excursion
           (let ((bounds  nil))
             ;; If BACKWARD, swap START and END.
             (cond ((< start end) (when   backward (setq start  (prog1 end (setq end  start)))))
                   ((> start end) (unless backward (setq start  (prog1 end (setq end  start))))))
             (catch 'isearchp-next-visible-thing-2
               (while (if backward (> start end) (< start end))
                 (goto-char start)
                 ;; Skip invisible text.
                 (when (and (if backward (> start end) (< start end))  (invisible-p start))
                   (setq start  (if (get-text-property start 'invisible) ; Text prop.
                                    (if backward
                                        (previous-single-property-change start 'invisible nil end)
                                      (next-single-property-change start 'invisible nil end))
                                  (if backward ; Overlay prop.
                                      (previous-overlay-change start)
                                    (next-overlay-change start))))
                   (goto-char start))
                 (when (and (setq bounds  (isearchp-bounds-of-thing-at-point thing))
                            (not (equal (car bounds) (cdr bounds)))) ; Not an empty thing, "".
                   (throw 'isearchp-next-visible-thing-2
                     (cons (buffer-substring (car bounds) (cdr bounds)) bounds)))
                 (setq start  (if backward (1- start) (1+ start))))
               nil))))))

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'isearch-prop)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; isearch-prop.el ends here
