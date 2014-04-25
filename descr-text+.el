;;; descr-text+.el --- Extensions to `descr-text.el'.
;;
;; Filename: descr-text+.el
;; Description: Extensions to `descr-text.el'.
;; Author: Drew Adams
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 2011-2014, Drew Adams, all rights reserved.
;; Created: Thu Nov 24 11:57:04 2011 (-0800)
;; Version: 0
;; Package-Requires: ()
;; Last-Updated: Thu Apr 24 19:51:28 2014 (-0700)
;;           By: dradams
;;     Update #: 280
;; URL: http://www.emacswiki.org/descr-text+.el
;; Keywords: help, characters, description
;; Compatibility: GNU Emacs: 22.x, 23.x, 24.x
;;
;; Features that might be required by this library:
;;
;;   `button', `descr-text', `help-mode'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Extensions to `descr-text.el'.
;;
;;    Note: As of Emacs 24.4, byte-compiling this file in one Emacs
;;    version and using the compiled file in another Emacs version
;;    does not work.
;;
;;
;;  ***** NOTE: The following functions defined in `descr-text.el'
;;              have been REDEFINED HERE:
;;
;;    `describe-char' (Emacs 23+), `describe-property-list',
;;    `describe-text-properties', `describe-text-properties-1',
;;    `describe-text-sexp'.
;;
;;
;;  Put this in your initialization file (`~/.emacs'):
;;
;;    (require 'descr-text+)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2014/04/24 dadams
;;     describe-text-sexp: Updated for Emacs 24.4: Use with-help-window if defined.  See bug #17109.
;; 2013/05/11 dadams
;;     describe-char: Updated per Emacs bug #14360 fix.
;; 2012/11/01 dadams
;;     Removed redefinition of describe-char for Emacs 22 - hassles with macros & defsubsts.
;; 2012/06/02 dadams
;;     describe-char: Updated for Emacs 24 (bidi stuff).
;; 2012/04/29 dadams
;;     describe-char for Emacs 22: Replaced charset-description by its macro expansion.
;; 2012/01/27 dadams
;;     Added: describe-property-list, describe-text-properties(-1), describe-text-sexp.
;;     describe-char: Added optional arg WIDTH.  Use it instead of function window-width.
;; 2012/01/25 dadams
;;     describe-char: Apply Kenichi H's Emacs 24 enhancement:
;;       http://lists.gnu.org/archive/html/emacs-devel/2012-01/msg00785.html
;; 2011/11/30
;;     Added compile-time require of mule.el for Emacs 22 (charset-description was a macro).
;; 2011/11/24 dadams
;;     Created.
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


(require 'descr-text)

(eval-when-compile
  (when (< emacs-major-version 23) (require 'mule))) ; charset-description (macro)


;; Quiet the byte-compiler.
(defvar help-window)
(defvar describe-char-unidata-list)
(defvar char-code-property-alist)

;;;;;;;;;;;;;;;;;;;;;;;;;;






;; REPLACE ORIGINAL in `descr-text.el':
;;
;; 1. Added optional arg WIDTH.
;;
;; 2. Use WIDTH arg or 70 (which is the default value of `fill-column'), not `window-width',
;;    since the current window is unrelated to the not-yet-displayed `*Help*' window.
;;
(defun describe-text-sexp (sexp &optional width)
  "Insert a short description of SEXP in the current buffer.
Optional arg WIDTH is the maximum width to use for the description.
If nil, a width of 70 is used, which is the default value of
`fill-column'."
  (let ((pp  (condition-case signal
                 (pp-to-string sexp)
               (error (prin1-to-string signal)))))
    (setq width  (or width 70))
    (when (if (fboundp 'string-match-p)
              (string-match-p "\n\\'" pp)
            (string-match "\n\\'" pp))
      (setq pp  (substring pp 0 (1- (length pp)))))
    (if (and (not (if (fboundp 'string-match-p)
                      (string-match-p "\n" pp)
                    (string-match "\n" pp)))
    	     (<= (length pp) (- width (current-column))))
	(insert pp)
      (insert-text-button
       "[Show]" 'action (if (fboundp 'with-help-window)
                            `(lambda (&rest ignore)
                               (with-help-window "*Pp Eval Output*" (princ ',pp)))
                          `(lambda (&rest ignore)
                             (with-output-to-temp-buffer "*Pp Eval Output*" (princ ',pp))))
       'help-echo "mouse-2, RET: pretty print value in another buffer"))))


;; REPLACE ORIGINAL in `descr-text.el':
;;
;; 1. Added optional arg MAX-WIDTH, the max width known so far (defaults to 0).
;; 2. Pass max width needed so far to `describe-text-sexp'.
;; 3. Return max width used.
;;
(defun describe-property-list (properties &optional max-width)
  "Insert a description of PROPERTIES in the current buffer.
PROPERTIES should be a list of overlay or text properties.
The `category', `face' and `font-lock-face' properties are made
into help buttons that call `describe-text-category' or
`describe-face' when pushed.

Optional arg MAX-WIDTH is the max width needed so far (defaults to 0)."
  (setq max-width  (or max-width  0))
  (dolist (elt  (sort (let (ret) ; Sort the properties by the size of their value.
                        (while properties
                          (push (list (pop properties) (pop properties)) ret))
                        ret)
                      (lambda (a b) (string< (prin1-to-string (nth 0 a) t)
                                        (prin1-to-string (nth 0 b) t)))))
    (let ((key    (nth 0 elt))
	  (value  (nth 1 elt)))
      (insert (propertize (format "  %-20s " key) 'face 'help-argument-name))
      (cond ((eq key 'category)
	     (insert-text-button
	      (symbol-name value)
	      'action `(lambda (&rest ignore) (describe-text-category ',value))
	      'follow-link t
	      'help-echo "mouse-2, RET: describe this category")
             (setq max-width  (max max-width (current-column))))
            ((memq key '(face font-lock-face mouse-face))
	     (insert-text-button
	      (format "%S" value)
	      'type 'help-face 'help-args (list value))
             (setq max-width  (max max-width (current-column))))
            ((widgetp value)
	     (describe-text-widget value)
             (setq max-width  (max max-width (current-column))))
	    (t
	     (describe-text-sexp value max-width)
             (setq max-width  (max max-width (current-column)))))
      (setq max-width  (max max-width (current-column)))
      (insert "\n")
      max-width)))


;; REPLACE ORIGINAL in `descr-text.el':
;;
;; 1. Added optional arg MAX-WIDTH, the max width known so far (defaults to 0).
;; 2. Return max width needed.
;;
(if (fboundp 'buffer-swap-text)         ; Emacs 23+
    (defun describe-text-properties (pos &optional output-buffer buffer max-width)
      "Describe widgets, buttons, overlays, and text properties at POS.
POS is taken to be in BUFFER or in current buffer if nil.
Interactively, describe them for the character after point.
If optional second argument OUTPUT-BUFFER is non-nil,
insert the output into that buffer, and don't initialize or clear it
otherwise.

Optional arg MAX-WIDTH is the max width needed so far (defaults to 0)."
      (interactive "d")
      (setq max-width  (or max-width  0))
      (let ((src-buf  (current-buffer)))
        (if buffer (set-buffer buffer) (setq buffer  (current-buffer)))
        (when (>= pos (point-max)) (error "No character follows specified position"))
        (if output-buffer
            (progn (describe-text-properties-1 pos output-buffer max-width)
                   (with-current-buffer output-buffer
                     (setq max-width  (max max-width (current-column)))))
          (if (not (or (text-properties-at pos)  (overlays-at pos)))
              (message "This is plain text.")
            (with-temp-buffer
              (setq output-buffer  (current-buffer))
              (insert "Text content at position " (format "%d" pos) ":\n\n")
              (setq max-width  (max max-width (current-column)))
              (set-buffer buffer)
              (describe-text-properties-1 pos output-buffer max-width)
              (setq max-width  (max max-width (current-column)))
              (set-buffer src-buf)
              (help-setup-xref (list 'describe-text-properties pos nil buffer)
                               (called-interactively-p 'interactive))
              (with-help-window (help-buffer)
                (with-current-buffer standard-output
                  (buffer-swap-text output-buffer)
                  (goto-char (point-min))))))))
      max-width)

  ;; Emacs 22
  (defun describe-text-properties (pos &optional output-buffer buffer max-width)
    "Describe widgets, buttons, overlays and text properties at POS.
Interactively, describe them for the character after point.
If optional second argument OUTPUT-BUFFER is non-nil,
insert the output into that buffer, and don't initialize or clear it
otherwise.

Optional arg MAX-WIDTH is the max width needed so far (defaults to 0)."
    (interactive "d")
    (setq max-width  (or max-width  0))
    (when (>= pos (point-max)) (error "No character follows specified position"))
    (if output-buffer
        (progn (describe-text-properties-1 pos output-buffer max-width)
               (with-current-buffer output-buffer
                 (setq max-width  (max max-width (current-column)))))
      (if (not (or (text-properties-at pos) (overlays-at pos)))
          (message "This is plain text.")
        (let ((buffer         (current-buffer))
              (target-buffer  "*Help*"))
          (when (eq buffer (get-buffer target-buffer)) (setq target-buffer  "*Help*<2>"))
          (save-excursion
            (with-output-to-temp-buffer target-buffer
              (set-buffer standard-output)
              (setq output-buffer  (current-buffer))
              (insert "Text content at position " (format "%d" pos) ":\n\n")
              (setq max-width  (max max-width (current-column)))
              (with-current-buffer buffer
                (describe-text-properties-1 pos output-buffer max-width)
                (setq max-width  (max max-width (current-column))))
              (goto-char (point-min)))))))
    max-width))


;; REPLACE ORIGINAL in `descr-text.el':
;;
;; 1. Added optional arg MAX-WIDTH, the max width known so far (defaults to 0).
;; 2. Return max width needed.
;;
(defun describe-text-properties-1 (pos output-buffer &optional max-width)
  (let* ((properties    (text-properties-at pos))
	 (overlays      (overlays-at pos))
	 (wid-field     (get-char-property pos 'field))
	 (wid-button    (get-char-property pos 'button))
	 (wid-doc       (get-char-property pos 'widget-doc))
	 ;; If button.el is not loaded, we have no buttons in the text.
	 (button        (and (fboundp 'button-at) (button-at pos)))
	 (button-type   (and button (button-type button)))
	 (button-label  (and button (button-label button)))
	 (widget        (or wid-field wid-button wid-doc)))
    (setq max-width  (or max-width  0))
    (with-current-buffer output-buffer
      ;; Widgets
      (when (widgetp widget)
	(newline)
	(insert (cond (wid-field "This is an editable text area")
		      (wid-button "This is an active area")
		      (wid-doc "This is documentation text")))
	(insert " of a ")
	(describe-text-widget widget)
	(insert ".")
        (setq max-width  (max max-width (current-column)))
        (insert "\n\n"))
      ;; Buttons
      (when (and button (not (widgetp wid-button)))
	(newline)
	(insert "Here is a `" (format "%S" button-type)
		"' button labeled `" button-label "'.\n\n")
        (setq max-width  (max max-width (current-column))))
      ;; Overlays
      (when overlays
	(newline)
	(if (eq (length overlays) 1)
	    (insert "There is an overlay here:\n")
	  (insert "There are " (format "%d" (length overlays))
			 " overlays here:\n"))
        (setq max-width  (max max-width (current-column)))
	(dolist (overlay  overlays)
	  (insert " From " (format "%d" (overlay-start overlay))
			 " to " (format "%d" (overlay-end overlay)) "\n")
          (setq max-width  (max max-width (current-column)))
	  (describe-property-list (overlay-properties overlay) max-width)
          (setq max-width  (max max-width (current-column))))
        (setq max-width  (max max-width (current-column)))
	(insert "\n"))
      ;; Text properties
      (when properties
	(newline)
	(insert "There are text properties here:\n")
        (setq max-width  (max max-width (current-column)))
	(describe-property-list properties max-width)
        (setq max-width  (max max-width (current-column))))
      max-width)))


;;; ;; REPLACE ORIGINAL in `descr-text.el':
;;; ;;
;;; ;; 1. Added optional arg WIDTH.
;;; ;;
;;; ;; 2. Use WIDTH arg or 70 (which is the default value of `fill-column'), not `window-width',
;;; ;;    since the current window is unrelated to the not-yet-displayed `*Help*' window.
;;; ;;
;;; ;; 3. In `*Help*', show also the position information that is in the message.
;;; ;;
;;; (when (= emacs-major-version 22)
;;;   (defun describe-char (pos &optional width)
;;;     "Describe the character after POS (interactively, the character after point).
;;; The information includes character code, charset and code points in it,
;;; syntax, category, how the character is encoded in a file,
;;; character composition information (if relevant),
;;; as well as widgets, buttons, overlays, and text properties."
;;;     (interactive "d")
;;;     (setq width  (or width 70))
;;;     (when (>= pos (point-max)) (error "No character follows specified position"))
;;;     (let* ((char              (char-after pos))
;;;            (charset           (char-charset char))
;;;            (composition       (find-composition pos nil nil t))
;;;            (component-chars   ())
;;;            (display-table     (or (window-display-table)
;;;                                   buffer-display-table
;;;                                   standard-display-table))
;;;            (disp-vector       (and display-table (aref display-table char)))
;;;            (multibyte-p       enable-multibyte-characters)
;;;            (overlays          (mapcar #'(lambda (o) (overlay-properties o))  (overlays-at pos)))
;;;            (char-description  (if (not multibyte-p)
;;;                                  (single-key-description char)
;;;                                (if (< char 128)
;;;                                    (single-key-description char)
;;;                                  (string-to-multibyte
;;;                                   (char-to-string char)))))
;;;            (text-props-desc
;;;             (let ((tmp-buf  (generate-new-buffer " *text-props*")))
;;;               (unwind-protect
;;;                    (progn (describe-text-properties pos tmp-buf nil width)
;;;                           (with-current-buffer tmp-buf (buffer-string)))
;;;                 (kill-buffer tmp-buf))))
;;;            item-list max-width unicode)
;;;       (if (or (< char 256)
;;;               (memq 'mule-utf-8 (find-coding-systems-region pos (1+ pos)))
;;;               (get-char-property pos 'untranslated-utf-8))
;;;           (setq unicode  (or (get-char-property pos 'untranslated-utf-8)
;;;                              (encode-char char 'ucs))))
;;;       (setq item-list
;;;             `(("position"
;;;                ,(let* ((beg      (point-min))
;;;                        (end      (point-max))
;;;                        (total    (buffer-size))
;;;                        (percent  (if (> total 50000) ; Avoid overflow multiplying by 100
;;;                                      (/ (+ (/ total 200) (1- pos))  (max (/ total 100) 1))
;;;                                    (/ (+ (/ total 2) (* 100 (1- pos)))  (max total 1))))
;;;                        (hscroll  (if (= (window-hscroll) 0)
;;;                                      ""
;;;                                    (format ", Hscroll: %d" (window-hscroll))))
;;;                        (col      (current-column)))
;;;                       (if (or (/= beg 1)  (/= end (1+ total)))
;;;                           (format "%d of %d (%d%%), column: %d%s, region: %d-%d"
;;;                                   pos total percent col hscroll beg end)
;;;                         (if (= pos end)
;;;                             (format "%d of %d (EOB), column: %d%s" pos total col hscroll)
;;;                           (format "%d of %d (%d%%), column: %d%s"
;;;                                   pos total percent col hscroll)))))
;;;               ("character"
;;;                ,(format "%s (displayed as %s) (%d, #o%o, #x%x)%s"
;;;                         char-description
;;;                         (apply 'propertize char-description
;;;                                (text-properties-at pos))
;;;                         char char char
;;;                         (if unicode
;;;                             (format ", U+%04X" unicode)
;;;                           "")))
;;;               ("charset"
;;;                ,`(insert-text-button
;;;                   ,(symbol-name charset)
;;;                   'type 'help-character-set 'help-args '(,charset))
;;;                ;; The next sexp is just the macro expansion of this:
;;;                ;; ,(format "(%s)" (charset-description charset)))
;;;                ;; Need to do this because `charset-description' is a macro in Emacs 22.
;;;                ,(format "(%s)" (if (charset-quoted-standard-p charset)
;;;                                    (aref (charset-info (nth 1 charset)) 13)
;;;                                  (list 'aref (list 'charset-info charset) 13))))
;;;               ("code point"
;;;                ,(let ((split  (split-char char)))
;;;                      `(insert-text-button
;;;                        ;; This is macro expansion of ,(if (= (charset-dimension charset) 1)
;;;                        ,(if (= (if (charset-quoted-standard-p charset)
;;;                                    (aref (charset-info (nth 1 charset)) 2)
;;;                                  (list 'aref (list 'charset-info charset) 2))
;;;                                1)
;;;                             (format "#x%02X" (nth 1 split))
;;;                             (format "#x%02X #x%02X" (nth 1 split)
;;;                                     (nth 2 split)))
;;;                        'action (lambda (&rest ignore)
;;;                                  (list-charset-chars ',charset)
;;;                                  (with-selected-window
;;;                                      (get-buffer-window "*Character List*" 0)
;;;                                    (goto-char (point-min))
;;;                                    (forward-line 2) ;Skip the header.
;;;                                    (let ((case-fold-search  nil))
;;;                                      (search-forward ,(char-to-string char) nil t))))
;;;                        'help-echo
;;;                        "mouse-2, RET: show this character in its character set")))
;;;               ("syntax"
;;;                ,(let ((syntax  (syntax-after pos)))
;;;                      (with-temp-buffer
;;;                        (internal-describe-syntax-value syntax)
;;;                        (buffer-string))))
;;;               ("category"
;;;                ,@(let ((category-set  (char-category-set char)))
;;;                       (if (not category-set)
;;;                           '("-- none --")
;;;                         (mapcar #'(lambda (x) (format "%c:%s"
;;;                                                       x (category-docstring x)))
;;;                                 (category-set-mnemonics category-set)))))
;;;               ,@(let ((props  (aref char-code-property-table char))
;;;                       ps)
;;;                      (when props
;;;                        (while props
;;;                          (push (format "%s:" (pop props)) ps)
;;;                          (push (format "%s;" (pop props)) ps))
;;;                        (list (cons "Properties" (nreverse ps)))))
;;;               ("to input"
;;;                ,@(let ((key-list  (and (eq input-method-function
;;;                                            'quail-input-method)
;;;                                        (quail-find-key char))))
;;;                       (and (consp key-list)
;;;                            (list "type"
;;;                                  (mapconcat #'(lambda (x) (concat "\"" x "\"")) key-list " or ")
;;;                                  "with"
;;;                                  `(insert-text-button
;;;                                    ,current-input-method
;;;                                    'type 'help-input-method
;;;                                    'help-args '(,current-input-method))))))
;;;               ("buffer code"
;;;                ,(encoded-string-description
;;;                  (string-as-unibyte (char-to-string char)) nil))
;;;               ("file code"
;;;                ,@(let* ((coding   buffer-file-coding-system)
;;;                         (encoded  (encode-coding-char char coding)))
;;;                        (if encoded
;;;                            (list (encoded-string-description encoded coding)
;;;                                  (format "(encoded by coding system %S)" coding))
;;;                          (list "not encodable by coding system"
;;;                                (symbol-name coding)))))
;;;               ("display"
;;;                ,(cond (disp-vector
;;;                        (setq disp-vector  (copy-sequence disp-vector))
;;;                        (dotimes (i  (length disp-vector))
;;;                          (setq char  (aref disp-vector i))
;;;                          (aset disp-vector i (cons char (describe-char-display
;;;                                                          pos (glyph-char char)))))
;;;                        (format "by display table entry [%s] (see below)"
;;;                                (mapconcat #'(lambda (x) (format "?%c" (glyph-char (car x))))
;;;                                           disp-vector " ")))
;;;                       (composition
;;;                        (let ((from        (car composition))
;;;                              (to          (nth 1 composition))
;;;                              (next        (1+ pos))
;;;                              (components  (nth 2 composition))
;;;                              ch)
;;;                          (setcar composition (and (< from pos) (buffer-substring from pos)))
;;;                          (setcar (cdr composition) (and (< next to)
;;;                                                         (buffer-substring next to)))
;;;                          (dotimes (i  (length components))
;;;                            (if (integerp (setq ch  (aref components i)))
;;;                                (push (cons ch (describe-char-display pos ch))
;;;                                      component-chars)))
;;;                          (setq component-chars  (nreverse component-chars))
;;;                          (format "composed to form \"%s\" (see below)"
;;;                                  (buffer-substring from to))))
;;;                       (t
;;;                        (let ((display  (describe-char-display pos char)))
;;;                          (if (display-graphic-p (selected-frame))
;;;                              (if display
;;;                                  (concat "by this font (glyph code)\n"
;;;                                          (format "     %s (#x%02X)"
;;;                                                  (car display) (cdr display)))
;;;                                "no font available")
;;;                            (if display
;;;                                (format "terminal code %s" display)
;;;                              "not encodable for terminal"))))))
;;;               ,@(let ((face  (if (not (or disp-vector composition))
;;;                                  (cond ((and show-trailing-whitespace
;;;                                              (save-excursion (goto-char pos)
;;;                                                              (looking-at "[ \t]+$")))
;;;                                         'trailing-whitespace)
;;;                                        ((and nobreak-char-display unicode (eq unicode '#xa0))
;;;                                         'nobreak-space)
;;;                                        ((and nobreak-char-display unicode (eq unicode '#xad))
;;;                                         'escape-glyph)
;;;                                        ((and (< char 32) (not (memq char '(9 10))))
;;;                                         'escape-glyph)))))
;;;                      (if face (list (list "hardcoded face"
;;;                                           `(insert-text-button
;;;                                             ,(symbol-name face)
;;;                                             'type 'help-face 'help-args '(,face))))))
;;;               ,@(let ((unicodedata  (and unicode (describe-char-unicode-data unicode))))
;;;                      (and unicodedata (cons (list "Unicode data" " ") unicodedata)))))
;;;       (setq max-width  (apply #'max (mapcar #'(lambda (x) (if (cadr x) (length (car x)) 0))
;;;                                             item-list)))
;;;       (help-setup-xref nil (interactive-p))
;;;       (with-output-to-temp-buffer (help-buffer)
;;;         (with-current-buffer standard-output
;;;           (set-buffer-multibyte multibyte-p)
;;;           (let ((formatter  (format "%%%ds:" max-width)))
;;;             (dolist (elt  item-list)
;;;               (when (cadr elt)
;;;                 (insert (format formatter (car elt)))
;;;                 (dolist (clm  (cdr elt))
;;;                   (if (eq (car-safe clm) 'insert-text-button)
;;;                       (progn (insert " ") (eval clm))
;;;                     (when (>= (+ (current-column)
;;;                                  (or (string-match "\n" clm) (string-width clm))
;;;                                  1)
;;;                               width)
;;;                       (insert "\n")
;;;                       (indent-to (1+ max-width)))
;;;                     (insert " " clm)))
;;;                 (insert "\n"))))
;;;           (when overlays
;;;             (save-excursion
;;;               (goto-char (point-min))
;;;               (re-search-forward "character:[ \t\n]+")
;;;               (let* ((end  (+ (point) (length char-description))))
;;;                 (mapc #'(lambda (props)
;;;                           (let ((o  (make-overlay (point) end)))
;;;                             (while props
;;;                               (overlay-put o (car props) (nth 1 props))
;;;                               (setq props  (cddr props)))))
;;;                       overlays))))

;;;           (when disp-vector
;;;             (insert "\nThe display table entry is displayed by ")
;;;             (if (display-graphic-p (selected-frame))
;;;                 (progn
;;;                   (insert "these fonts (glyph codes):\n")
;;;                   (dotimes (i  (length disp-vector))
;;;                     (insert (glyph-char (car (aref disp-vector i))) ?:
;;;                             (propertize " " 'display '(space :align-to 5))
;;;                             (if (cdr (aref disp-vector i))
;;;                                 (format "%s (#x%02X)" (cadr (aref disp-vector i))
;;;                                         (cddr (aref disp-vector i)))
;;;                               "-- no font --")
;;;                             "\n")
;;;                     (let ((face  (glyph-face (car (aref disp-vector i)))))
;;;                       (when face
;;;                         (insert (propertize " " 'display '(space :align-to 5)) "face: ")
;;;                         (insert (concat "`" (symbol-name face) "'"))
;;;                         (insert "\n")))))
;;;               (insert "these terminal codes:\n")
;;;               (dotimes (i  (length disp-vector))
;;;                 (insert (car (aref disp-vector i))
;;;                         (propertize " " 'display '(space :align-to 5))
;;;                         (or (cdr (aref disp-vector i)) "-- not encodable --")
;;;                         "\n"))))

;;;           (when composition
;;;             (insert "\nComposed")
;;;             (if (car composition)
;;;                 (if (cadr composition)
;;;                     (insert " with the surrounding characters \""
;;;                             (car composition) "\" and \"" (cadr composition) "\"")
;;;                   (insert " with the preceding character(s) \"" (car composition) "\""))
;;;               (if (cadr composition)
;;;                   (insert " with the following character(s) \"" (cadr composition) "\"")))
;;;             (insert " by the rule:\n\t("
;;;                     (mapconcat (lambda (x) (format (if (consp x) "%S" "?%c") x))
;;;                                (nth 2 composition)
;;;                                " ")
;;;                     ")")
;;;             (insert "\nThe component character(s) are displayed by ")
;;;             (if (display-graphic-p (selected-frame))
;;;                 (progn
;;;                   (insert "these fonts (glyph codes):")
;;;                   (dolist (elt  component-chars)
;;;                     (insert "\n " (car elt) ?:
;;;                             (propertize " " 'display '(space :align-to 5))
;;;                             (if (cdr elt)
;;;                                 (format "%s (#x%02X)" (cadr elt) (cddr elt))
;;;                               "-- no font --"))))
;;;               (insert "these terminal codes:")
;;;               (dolist (elt  component-chars)
;;;                 (insert "\n  " (car elt) ":"
;;;                         (propertize " " 'display '(space :align-to 5))
;;;                         (or (cdr elt) "-- not encodable --"))))
;;;             (insert "\nSee variable `reference-point-alist' for the meaning of the rule.\n"))
;;;           (when text-props-desc (insert text-props-desc))
;;;           (setq help-xref-stack-item  (list 'help-insert-string (buffer-string)))
;;;           (toggle-read-only 1)
;;;           (print-help-return-message))))))


;; REPLACE ORIGINAL in `descr-text.el':
;;
;; 1. Added optional arg WIDTH.
;;
;; 2. Use WIDTH arg or 70 (which is the default value of `fill-column'), not `window-width',
;;    since the current window is unrelated to the not-yet-displayed `*Help*' window.
;;
;; 3. In `*Help*', show also the position information that is in the message.
;;
(when (> emacs-major-version 22)
  (defun describe-char (pos &optional buffer width)
    "Describe position POS (interactively, point) and the char after POS.
POS is taken to be in BUFFER, or the current buffer if BUFFER is nil.
The information is displayed in buffer `*Help*'.

The position information includes POS; the total size of BUFFER; the
region limits, if narrowed; the column number; and the horizontal
scroll amount, if the buffer is horizontally scrolled.

The character information includes the character code; charset and
code points in it; syntax; category; how the character is encoded in
BUFFER and in BUFFER's file; the font and font glyphs used to display
the character; the character's canonical name and other properties
defined by the Unicode Data Base; and relevant widgets, buttons,
overlays, and text properties."
    (interactive "d")
    (unless (buffer-live-p buffer) (setq buffer  (current-buffer)))
    (setq width  (or width 70))
    (let ((src-buf  (current-buffer)))
      (set-buffer buffer)
      (when (>= pos (point-max)) (error "No character follows specified position"))
      (let* ((char              (char-after pos))
             (eight-bit-p       (and (not enable-multibyte-characters) (>= char 128)))
             (charset           (if eight-bit-p
                                    'eight-bit
                                  (or (get-text-property pos 'charset)
                                      (char-charset char))))
             (composition       (find-composition pos nil nil t))
             (component-chars   ())
             (display-table     (or (window-display-table)
                                    buffer-display-table
                                    standard-display-table))
             (disp-vector       (and display-table (aref display-table char)))
             (multibyte-p       enable-multibyte-characters)
             (overlays          (mapcar (lambda (o) (overlay-properties o))
                                        (overlays-at pos)))
             (char-description  (if (not multibyte-p)
                                    (single-key-description char)
                                  (if (< char 128)
                                      (single-key-description char)
                                    (string-to-multibyte (char-to-string char)))))
             (text-props-desc
              (let ((tmp-buf  (generate-new-buffer " *text-props*")))
                (unwind-protect
                     (progn (describe-text-properties pos tmp-buf nil width)
                            (with-current-buffer tmp-buf (buffer-string)))
                  (kill-buffer tmp-buf))))
             item-list max-width code)
        (if multibyte-p
            (or (setq code  (encode-char char charset))
                (setq charset  (char-charset char)
                      code     (encode-char char charset)))
          (setq code  char))
        (cond
          ;; Append a PDF character to directional embeddings and overrides, to prevent
          ;; potential messup of the following text.
          ((memq char '(?\x202a ?\x202b ?\x202d ?\x202e))
           (setq char-description  (concat char-description
                                           (propertize (string ?\x202c) 'invisible t))))
          ;; Append an LRM char to any strong char to avoid messing up the numerical codepoint.
          ((memq (get-char-code-property char 'bidi-class) '(R AL))
           (setq char-description  (concat char-description
                                           (propertize (string ?\x200e) 'invisible t)))))
        (when composition
          ;; When the composition is trivial (i.e. composed only with the
          ;; current character itself without any alternate characters),
          ;; we don't show the composition information.  Otherwise, store
          ;; two descriptive strings in the first two elements of
          ;; COMPOSITION.
          (or (catch 'tag
                (let ((from        (car composition))
                      (to          (nth 1 composition))
                      (components  (nth 2 composition))
                      ch)
                  (if (and (vectorp components) (vectorp (aref components 0)))
                      (let ((idx      (- pos from))
                            (nglyphs  (lgstring-glyph-len components))
                            (i        0)
                            j glyph glyph-from)
                        ;; COMPONENTS is a gstring.  Find a grapheme
                        ;; cluster containing the current character.
                        (while (and (< i nglyphs)
                                    (setq glyph  (lgstring-glyph components i))
                                    (< (lglyph-to glyph) idx))
                          (setq i  (1+ i)))
                        (when (or (not glyph) (= i nglyphs))
                          (throw 'tag nil)) ; The composition is broken.
                        (setq glyph-from  (lglyph-from glyph)
                              to          (+ from (lglyph-to glyph) 1)
                              from        (+ from glyph-from)
                              j           i)
                        (while (and (< j nglyphs)
                                    (setq glyph  (lgstring-glyph components j))
                                    (= (lglyph-from glyph) glyph-from))
                          (setq j  (1+ j)))
                        (if (and (= to (1+ from))
                                 (= i (1- j))
                                 (setq glyph  (lgstring-glyph components i))
                                 (= char (lglyph-char glyph)))
                            (throw 'tag nil)) ; The composition is trivial.
                        (nconc composition (list i (1- j))))
                    (dotimes (i  (length components))
                      (when (integerp (setq ch  (aref components i)))
                        (push (cons ch (describe-char-display pos ch))  component-chars)))
                    (setq component-chars  (nreverse component-chars)))
                  (if (< from pos)
                      (if (< (1+ pos) to)
                          (setcar composition
                                  (concat
                                   " with the surrounding characters \""
                                   (mapconcat 'describe-char-padded-string
                                              (buffer-substring from pos) "")
                                   "\" and \""
                                   (mapconcat 'describe-char-padded-string
                                              (buffer-substring (1+ pos) to) "")
                                   "\""))
                        (setcar composition
                                (concat
                                 " with the preceding character(s) \""
                                 (mapconcat 'describe-char-padded-string
                                            (buffer-substring from pos) "")
                                 "\"")))
                    (if (< (1+ pos) to)
                        (setcar composition
                                (concat
                                 " with the following character(s) \""
                                 (mapconcat 'describe-char-padded-string
                                            (buffer-substring (1+ pos) to) "")
                                 "\""))
                      (setcar composition nil)))
                  (setcar (cdr composition)
                          (format "composed to form \"%s\" (see below)"
                                  (buffer-substring from to)))))
              (setq composition  nil)))
        (setq item-list
              `(("position"
                 ,(let* ((beg      (point-min))
                         (end      (point-max))
                         (total    (buffer-size))
                         (percent  (if (> total 50000) ; Avoid overflow multiplying by 100
                                       (/ (+ (/ total 200) (1- pos))  (max (/ total 100) 1))
                                     (/ (+ (/ total 2) (* 100 (1- pos)))  (max total 1))))
                         (hscroll  (if (= (window-hscroll) 0)
                                       ""
                                     (format ", Hscroll: %d" (window-hscroll))))
                         (col      (current-column)))
                        (if (or (/= beg 1)  (/= end (1+ total)))
                            (format "%d of %d (%d%%), restriction: <%d-%d>, column: %d%s"
                                    pos total percent col beg end hscroll)
                          (if (= pos end)
                              (format "%d of %d (EOB), column: %d%s" pos total col hscroll)
                            (format "%d of %d (%d%%), column: %d%s"
                                    pos total percent col hscroll)))))
                ("character"
                 ,(format "%s (displayed as %s) (codepoint %d, #o%o, #x%x)"
                          char-description
                          (apply 'propertize char-description
                                 (text-properties-at pos))
                          char char char))
                ("preferred charset"
                 ,`(insert-text-button ,(symbol-name charset)
                                       'type 'help-character-set 'help-args '(,charset))
                 ,(format "(%s)" (charset-description charset)))
                ("code point in charset"
                 ,(let ((str (if (integerp code)
                                 (format (if (< code 256) "0x%02X" "0x%04X")
                                         code)
                               (format "0x%04X%04X" (car code) (cdr code)))))
                       (if (<= (charset-dimension charset) 2)
                           `(insert-text-button
                             ,str
                             'action (lambda (&rest ignore)
                                       (list-charset-chars ',charset)
                                       (with-selected-window
                                           (get-buffer-window "*Character List*" 0)
                                         (goto-char (point-min))
                                         (forward-line 2) ;Skip the header.
                                         (let ((case-fold-search nil))
                                           (if (search-forward
                                                ,(char-to-string char) nil t)
                                               (goto-char (match-beginning 0))))))
                             'follow-link t
                             'help-echo
                             "mouse-2, RET: show this character in its character set")
                         str)))
                ("syntax"
                 ,(let ((syntax  (syntax-after pos)))
                       (with-temp-buffer
                         (internal-describe-syntax-value syntax)
                         (buffer-string))))
                ("category"
                 ,@(if (not eight-bit-p)
                       (let ((category-set  (char-category-set char)))
                         (if category-set
                             (describe-char-categories category-set)
                           '("-- none --")))))
                ("to input"
                 ,@(if (not eight-bit-p)
                       (let ((key-list  (and (eq input-method-function 'quail-input-method)
                                             (quail-find-key char))))
                         (and (consp key-list)
                              (list "type"
                                    (concat "\"" (mapconcat 'identity key-list "\" or \"")
                                            "\"")
                                    "with"
                                    `(insert-text-button
                                      ,current-input-method
                                      'type 'help-input-method
                                      'help-args '(,current-input-method)))))))
                ("buffer code"
                 ,(if multibyte-p
                      (encoded-string-description
                       (string-as-unibyte (char-to-string char)) nil)
                      (format "#x%02X" char)))
                ("file code"
                 ,@(if multibyte-p
                       (let* ((coding buffer-file-coding-system)
                              (encoded (encode-coding-char char coding charset)))
                         (if encoded
                             (list (encoded-string-description encoded coding)
                                   (format "(encoded by coding system %S)"
                                           coding))
                           (list "not encodable by coding system"
                                 (symbol-name coding))))
                       (list (format "#x%02X" char))))
                ("display"
                 ,(cond (disp-vector
                         (setq disp-vector  (copy-sequence disp-vector))
                         (dotimes (i  (length disp-vector))
                           (aset disp-vector i
                                 (cons (aref disp-vector i)
                                       (describe-char-display
                                        pos (glyph-char (aref disp-vector i))))))
                         (format "by display table entry [%s] (see below)"
                                 (mapconcat (lambda (x) (format "?%c" (glyph-char (car x))))
                                            disp-vector
                                            " ")))
                        (composition
                         (cadr composition))
                        (t
                         (let ((display  (describe-char-display pos char)))
                           (if (display-graphic-p (selected-frame))
                               (if display
                                   (concat "by this font (glyph code)\n    " display)
                                 "no font available")
                             (if display
                                 (format "terminal code %s" display)
                               "not encodable for terminal"))))))
                ,@(let ((face
                         (and (not (or disp-vector composition))
                              (cond ((and show-trailing-whitespace
                                          (save-excursion (goto-char pos)
                                                          (looking-at-p "[ \t]+$")))
                                     'trailing-whitespace)
                                    ((and nobreak-char-display char (eq char '#xa0))
                                     'nobreak-space)
                                    ((and nobreak-char-display  char
                                          (memq char '(#xad #x2010 #x2011)))
                                     'escape-glyph)
                                    ((and (< char 32)  (not (memq char '(9 10))))
                                     'escape-glyph)))))
                       (and face  (list (list "hardcoded face" `(insert-text-button
                                                                 ,(symbol-name face)
                                                                 'type 'help-face
                                                                 'help-args '(,face))))))
                ,@(and (not eight-bit-p)
                       (let ((unicodedata  (describe-char-unicode-data char)))
                         (and unicodedata  (cons (list "Unicode data" "") unicodedata))))))
        (setq max-width  (apply 'max (mapcar (lambda (x) (if (cadr x) (length (car x)) 0))
                                             item-list)))
        (set-buffer src-buf)
        (help-setup-xref (list 'describe-char pos buffer)
                         (called-interactively-p 'interactive))
        (with-help-window (help-buffer)
          (with-current-buffer standard-output
            (set-buffer-multibyte multibyte-p)
            (let ((formatter  (format "%%%ds:" max-width)))
              (dolist (elt  item-list)
                (when (cadr elt)
                  (insert (format formatter (car elt)))
                  (dolist (clm  (cdr elt))
                    (if (eq (car-safe clm) 'insert-text-button)
                        (progn (insert " ") (eval clm))
                      (when (>= (+ (current-column)
                                   (or (string-match-p "\n" clm)  (string-width clm))
                                   1)
                                width)
                        (insert "\n")
                        (indent-to (1+ max-width)))
                      (unless (zerop (length clm))  (insert " " clm))))
                  (insert "\n"))))
            (when overlays
              (save-excursion
                (goto-char (point-min))
                (re-search-forward "character:[ \t\n]+")
                (let ((end  (+ (point) (length char-description))))
                  (mapc (lambda (props)
                          (let ((o  (make-overlay (point) end)))
                            (while props
                              (overlay-put o (car props) (nth 1 props))
                              (setq props  (cddr props)))))
                        overlays))))
            (when disp-vector
              (insert
               "\nThe display table entry is displayed by ")
              (if (display-graphic-p (selected-frame))
                  (progn (insert "these fonts (glyph codes):\n")
                         (dotimes (i  (length disp-vector))
                           (insert (glyph-char (car (aref disp-vector i))) ?:
                                   (propertize " " 'display '(space :align-to 5))
                                   (or (cdr (aref disp-vector i)) "-- no font --")
                                   "\n")
                           (let ((face  (glyph-face (car (aref disp-vector i)))))
                             (when face
                               (insert (propertize " " 'display '(space :align-to 5))
                                       "face: ")
                               (insert (concat "`" (symbol-name face) "'"))
                               (insert "\n")))))
                (insert "these terminal codes:\n")
                (dotimes (i  (length disp-vector))
                  (insert (car (aref disp-vector i))
                          (propertize " " 'display '(space :align-to 5))
                          (or (cdr (aref disp-vector i)) "-- not encodable --")
                          "\n"))))
            (when composition
              (insert "\nComposed")
              (when (car composition) (insert (car composition)))
              (if (and (vectorp (nth 2 composition))
                       (vectorp (aref (nth 2 composition) 0)))
                  (let* ((gstring  (nth 2 composition))
                         (font     (lgstring-font gstring))
                         (from     (nth 3 composition))
                         (to       (nth 4 composition))
                         glyph)
                    (if (fontp font)
                        (progn
                          (insert " using this font:\n  "
                                  (symbol-name (font-get font :type))
                                  ?:
                                  (aref (query-font font) 0)
                                  "\nby these glyphs:\n")
                          (while (and (<= from to)
                                      (setq glyph  (lgstring-glyph gstring from)))
                            (insert (format "  %S\n" glyph))
                            (setq from  (1+ from))))
                      (insert " by these characters:\n")
                      (while (and (<= from to)
                                  (setq glyph  (lgstring-glyph gstring from)))
                        (insert (format " %c (#x%x)\n"
                                        (lglyph-char glyph) (lglyph-char glyph)))
                        (setq from  (1+ from)))))
                (insert " by the rule:\n\t(")
                (let ((first  t))
                  (mapc (lambda (x)
                          (if first
                              (setq first  nil)
                            (insert " "))
                          (if (consp x)
                              (insert (format "%S" x))
                            (if (= x ?\t)
                                (insert (single-key-description x))
                              (insert ??)
                              (insert (describe-char-padded-string x)))))
                        (nth 2 composition)))
                (insert  ")\nThe component character(s) are displayed by ")
                (if (display-graphic-p (selected-frame))
                    (progn
                      (insert "these fonts (glyph codes):")
                      (dolist (elt  component-chars)
                        (when (/= (car elt) ?\t)
                          (insert "\n "
                                  (describe-char-padded-string (car elt))
                                  ?:
                                  (propertize " " 'display '(space :align-to 5))
                                  (or (cdr elt) "-- no font --")))))
                  (insert "these terminal codes:")
                  (dolist (elt  component-chars)
                    (insert "\n  " (car elt) ":"
                            (propertize " " 'display '(space :align-to 4))
                            (or (cdr elt) "-- not encodable --"))))
                (insert "\nSee the variable `reference-point-alist' for "
                        "the meaning of the rule.\n")))
            (unless eight-bit-p
              (insert (if (not describe-char-unidata-list)
                          "\nCharacter code properties are not shown: "
                        "\nCharacter code properties: "))
              (insert-text-button
               "customize what to show"
               'action (lambda (&rest _ignore) (customize-variable 'describe-char-unidata-list))
               'follow-link t)
              (insert "\n")
              (dolist (elt  (if (eq describe-char-unidata-list t)
                                (nreverse (mapcar 'car char-code-property-alist))
                              describe-char-unidata-list))
                (let ((val  (get-char-code-property char elt))
                      description)
                  (when val
                    (setq description  (char-code-property-description elt val))
                    (insert (if description
                                (format "  %s: %s (%s)\n" elt val description)
                              (format "  %s: %s\n" elt val)))))))
            (when text-props-desc (insert text-props-desc))
            (toggle-read-only 1)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'descr-text+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; descr-text+.el ends here
