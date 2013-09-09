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
;; Last-Updated: Sun Sep  8 19:40:23 2013 (-0700)
;;           By: dradams
;;     Update #: 110
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
;;  (@> "Commands")
;;  (@> "Non-Interactive Functions")
;;  (@> "Character-Property Search")
;;
;;
;;  Macros defined here:
;;
;;    None.
;;
;;  Commands defined here:
;;
;;    `isearchp-char-prop-backward',
;;    `isearchp-char-prop-backward-regexp',
;;    `isearchp-char-prop-forward',
;;    `isearchp-char-prop-forward-regexp',
;;    `isearchp-put-prop-on-region'.
;;
;;  User options defined here:
;;
;;    None.
;;
;;  Faces defined here:
;;
;;    None.
;;
;;  Non-interactive functions defined here:
;;
;;    `isearchp-char-prop-1', `isearchp-char-prop-default-match-fn',
;;    `isearchp-char-prop-end', `isearchp-char-properties-in-buffer',
;;    `isearchp-char-prop-filter-pred',
;;    `isearchp-char-prop-matches-p', `isearchp-message-prefix',
;;    `isearchp-read-face-names', `isearchp-read-face-names--read',
;;    `isearchp-read-sexps', `isearchp-remove-duplicates',
;;    `isearchp-some'.
;;
;;  Internal variables defined here:
;;
;;    `isearchp-char-prop-prop', `isearchp-char-prop-type',
;;    `isearchp-char-prop-values', `isearchp-filter-predicate-orig'.
;;
;;
;;  Keys bound in `isearch-mode-map' here:
;;
;;    `C-t'        `isearchp-char-prop-forward'
;;    `C-M-t'      `isearchp-char-prop-forward-regexp'
;;
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
 
;;(@* "Variables")

;;; Variables ----------------------------------------------

(defvar isearchp-filter-predicate-orig nil
  "Original value of `isearch-filter-predicate'.")

(defvar isearchp-char-prop-type nil
  "Last property type used for `isearchp-char-prop-*' commands.")

(defvar isearchp-char-prop-prop nil
  "Last property used for `isearchp-char-prop-*' commands.")

(defvar isearchp-char-prop-values nil
  "Last property values used for `isearchp-char-prop-*' commands.")

 
;;(@* "Keys")

;;; Keys -------------------------------------------------------------

(define-key isearch-mode-map "\C-t"    'isearchp-char-prop-forward)
(define-key isearch-mode-map "\C-\M-t" 'isearchp-char-prop-forward-regexp)
 
;;(@* "Commands")

;;; Commands ---------------------------------------------------------

(defun isearchp-char-prop-forward (arg) ; Bound to `C-t' in `isearch-mode-map'.
  "Isearch forward for a character (overlay or text) property.
If you have not previously used an `isearch-char-prop-*' command, you
are prompted for:

 * the property type (`text', `overlay', or `text and overlay')
 * the property (e.g., `face', `mumamo-major-mode')
 * the property values (e.g., a list of faces, for property `face')

Otherwise:

 With no prefix arg, use the settings (property type, property,
 property values) from the last time you invoked an
 `isearch-char-prop-*' command.

 With a prefix arg you are prompted for the property and property
 values to use.  The particular prefix arg determines the property
 type to search, as follows:

  * plain prefix arg (`C-u'): both overlay and text property zones
  * negative prefix arg (e.g., `C--'): overlay property zones
  * non-negative prefix arg (e.g., `C-9'): text property zones

By default, an actual value of the property matches the value
you specify if it is `equal'.  Properties `mumamo-major-mode' and
`face' (or `font-lock-face') are exceptions.

For `mumamo-major-mode' you specify the major mode whose zones of text
you want to search.  The actual property value is a list whose car is
the major mode symbol.

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
  "Isearch backward for a character (overlay or text) property.
See `isearchp-char-prop-forward'."
  (interactive "P")
  (isearchp-char-prop-1 'isearch-backward arg))

(defun isearchp-char-prop-forward-regexp (arg) ; Bound to `C-M-t' in `isearch-mode-map'.
  "Regexp Isearch forward for a character (overlay or text) property.
NOTE: This command is available during normal Isearch, on key `C-M-t'.
      However, in order to be able to use a prefix arg with this
      command, you must set `isearch-allow-scroll' or
      `isearch-allow-prefix' (if available) to non-nil.
      Otherwise, a prefix arg during Isearch exits Isearch.
See `isearchp-char-prop-forward'."
  (interactive "P")
  (isearchp-char-prop-1 'isearch-forward-regexp arg))

(defun isearchp-char-prop-backward-regexp (arg)
  "Regexp Isearch backward for a character (overlay or text) property.
See `isearchp-char-prop-backward'."
  (interactive "P")
  (isearchp-char-prop-1 'isearch-backward-regexp arg))
 
;;(@* "Non-Interactive Functions")

;;; Non-Interactive Functions

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
                                (format "%s property to search: "
                                        (if type (capitalize (symbol-name type)) "Character"))
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
             (and (fboundp 'isearch-filter-visible)  (isearch-filter-visible beg end))
             (and (boundp 'isearch-invisible)  (not (or (eq search-invisible t) ; Emacs 24.4+
                                                        (not (isearch-range-invisible beg end))))))
            (catch ',tag
              (while (< beg end)
                (unless (isearchp-char-prop-matches-p
                         ',type ',prop ',values
                         (isearchp-char-prop-default-match-fn
                          ',prop)
                         beg)
                  (throw ',tag nil))
                (setq beg  (1+ beg)))
              t)))))

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

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'isearch-prop)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; isearch-prop.el ends here
