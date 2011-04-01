;;; help-fns+.el --- Extensions to `help-fns.el'.
;;
;; Filename: help-fns+.el
;; Description: Extensions to `help-fns.el'.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2007-2011, Drew Adams, all rights reserved.
;; Created: Sat Sep 01 11:01:42 2007
;; Version: 22.1
;; Last-Updated: Thu Mar 31 07:42:11 2011 (-0700)
;;           By: dradams
;;     Update #: 483
;; URL: http://www.emacswiki.org/cgi-bin/wiki/help-fns+.el
;; Keywords: help, faces
;; Compatibility: GNU Emacs: 22.x, 23.x
;;
;; Features that might be required by this library:
;;
;;   `button', `help-fns', `help-mode', `view', `wid-edit',
;;   `wid-edit+'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Extensions to `help-fns.el'
;;
;;  Keys bound here:
;;
;;    `C-h c'      `describe-command'     (replaces `describe-key-briefly')
;;    `C-h o'      `describe-option'
;;    `C-h C-c'    `describe-key-briefly' (replaces `C-h c')
;;    `C-h C-o'    `describe-option-of-type'
;;    `C-h M-c'    `describe-copying'     (replaces `C-h C-c')
;;    `C-h M-f'    `describe-file'
;;    `C-h M-k'    `describe-keymap'
;;    `C-h M-l'    `find-function-on-key'
;;
;;  Commands defined here:
;;
;;    `describe-command', `describe-file', `describe-keymap',
;;    `describe-option', `describe-option-of-type'.
;;
;;  Non-interactive functions defined here:
;;
;;    `help-all-exif-data', `help-custom-type',
;;    `help-remove-duplicates', `help-value-satisfies-type-p',
;;    `help-var-inherits-type-p', `help-var-is-of-type-p',
;;    `help-var-matches-type-p', `help-var-val-satisfies-type-p'.
;;
;;  Internal variables defined here:
;;
;;    `variable-name-history'.
;;
;;
;;  ***** NOTE: The following functions defined in `help-fns.el'
;;              have been REDEFINED HERE:
;;
;;  `describe-function', `describe-function-1', `describe-variable'.
;;
;;
;;  Put this in your initialization file (`~/.emacs'):
;;
;;    (require 'help-fns+)
;;
;;  Acknowledgement: Passing text properties on doc strings to the
;;  *Help* buffer is an idea from Johan bockgard.  He sent it on
;;  2007-01-24 to emacs-devel@gnu.org, Subject
;;  "display-completion-list should not strip text properties".
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;; 2011/03/31 dadams
;;     help-var-(matches|inherits)-type-p: Wrap string-match with save-match-data.
;; 2011/03/17 dadams
;;     describe-file: Added clickable thumbnail image to the help for an image file.
;; 2011/03/02 dadams
;;     Added: help-all-exif-data
;;     describe-file: Show all EXIF data, using help-all-exif-data.
;; 2011/02/22 dadams
;;     describe-file: Show also EXIF data for an image file.
;; 2011/01/04 dadams
;;     Removed autoload cookies from non def* sexps and define-key.
;; 2010/02/12 dadams
;;     Added variable-name-history.
;; 2009/08/30 dadams
;;     describe-keymap: Don't print nil if the map has no doc.
;; 2009/05/26 dadams
;;     describe-variable: Updated wrt latest Emacs 23:
;;       Added file-name-non-directory; removed substitute-command-keys.
;; 2008/09/13 dadams
;;     Updated for latest Emacs 23 CVS.
;;       describe-variable: Create separate version for Emacs 23.
;;       describe-function-1: No longer needed for Emacs 23, since my patch added.
;;       Added: with-selected-frame, with-help-window, at least temporarily.
;;     Require wid-edit.el.
;; 2008/09/02 dadams
;;     describe-function-1, describe-variable:
;;       Emacs 23 uses find-lisp-object-file-name.  Thx to Per Nordlow.
;; 2008/08/19 dadams
;;     describe-keymap: Use insert instead of princ for map part.  Thx to Chong Yidong.
;; 2008/05/20 dadams
;;     describe-function: Different prompt if prefix arg.
;; 2008/03/02 dadams
;;     Moved describe-file here from misc-cmds.el.  Bound to C-h M-f.
;;     Require cl.el at compile time.
;; 2008/02/01 dadams
;;     Bound M-l to find-function-on-key.
;; 2008/01/03 dadams
;;     Added: describe-function-1.  The redefinition fills overlong lines.
;; 2007/12/25 dadams
;;     help-var-inherits-type-p:
;;       Recheck var-type match after set var-type to its car.
;;       Handle string (regexp) TYPES elements.
;;     help-value-satisfies-type-p: Skip type check for string type (regexp).
;;     help-var-is-of-type-p: Doc string.  Use help-var-matches-type-p.
;;     Added: help-var-matches-type-p.
;; 2007/12/24 dadams
;;     help-var-inherits-type-p: Recheck type match after set var-type to its car.
;;     Added: help-custom-type.
;; 2007/12/23 dadams
;;     help-var-is-of-type-p:
;;       Added MODE arg.  Use help-var-inherits-type-p, help-var-val-satisfies-type-p.
;;       Redefined as MODE choice, not just a simple or.  Treat more cases.
;;     Added: help-var-inherits-type-p, help-var-val-satisfies-type-p,
;;            help-value-satisfies-type-p.
;;     describe-option-of-type: Prefix arg means use mode inherit-or-value.
;; 2007/12/22 dadams
;;     help-var-is-of-type-p:
;;       Check supertypes also.  Use both :validate and :match.
;;       Wrap type check in condition-case. Use widget-put instead of plist-put.
;;     Added soft require of wid-edit+.el.
;; 2007/12/21 dadams
;;     help-var-is-of-type-p: Use :validate, not :match, for the test.
;; 2007/12/20 dadams
;;     Moved describe-option-of-type to C-h C-o.
;; 2007/12/15 dadams
;;     Bound C-h c to describe-command and C-h C-c to describe-key-briefly.
;; 2007/12/07 dadams
;;     describe-option-of-type:
;;       Call describe-variable with nil buffer.  Use "nil" as default value.
;; 2007/12/06 dadams
;;     describe-option-of-type:
;;       If nil type, all defcustom vars are candidates.  Use custom-variable-p.
;;       Specific error if no such custom type.
;; 2007/12/04 dadams
;;     Added: describe-option-of-type, help-remove-duplicates, help-var-is-of-type-p.
;;     Bound o to describe-option, M-o to describe-option-of-type,
;;       C-c to describe-command, M-c to describe-copying.
;; 2007/11/28 dadams
;;     Renamed describe-bindings-in-map to describe-keymap.  Added keymap's doc string.
;; 2007/11/22 dadams
;;     Added: describe-bindings-in-map.  Bound to C-h M-k.
;; 2007/11/01 dadams
;;     Corrected require typo: help-mode -> help-fns.
;; 2007/10/18 dadams
;;     Created.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
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

(require 'help-fns)

(require 'wid-edit+ nil t) ;; (no error if not found):
                           ;; redefined color widget (for help-var-is-of-type-p)
(require 'wid-edit) ;; widget-convert

(eval-when-compile (require 'cl)) ;; case


;; Quiet the byte-compiler.
(defvar help-window)
(defvar help-window-point-marker)

;;;;;;;;;;;;;;;;;;;;;;;;

(defvar variable-name-history () "Minibuffer history for variable names.")

(define-key help-map "c" 'describe-command)
(define-key help-map "o" 'describe-option)
(define-key help-map "\C-c" 'describe-key-briefly)
(define-key help-map "\C-o" 'describe-option-of-type)
(define-key help-map "\M-c" 'describe-copying)
(define-key help-map "\M-f" 'describe-file)
(define-key help-map "\M-k" 'describe-keymap)
(define-key help-map "\M-l" 'find-function-on-key)


;; REPLACES ORIGINAL in `help-fns.el':
;; Preferred candidate is `symbol-nearest-point'.
;; With a prefix argument, candidates are commands only.
;;
;;;###autoload
(defun describe-function (function &optional commandp)
  "Display the full documentation of FUNCTION (a symbol).
FUNCTION names an Emacs Lisp function, possibly a user command.
With a prefix argument, candidates are commands (interactive) only.
Default candidate is: preferably the `symbol-nearest-point', or else
the innermost function call surrounding point
\(`function-called-at-point').
Return the description that was displayed, as a string."
  (interactive
   (let ((fn (or (and (fboundp 'symbol-nearest-point) (symbol-nearest-point))
                 (function-called-at-point)))
         (enable-recursive-minibuffers t)
         val)
     (setq val (completing-read
                (if current-prefix-arg "Describe command: " "Describe function: ")
                obarray (if current-prefix-arg 'commandp 'fboundp) t nil nil
                (and fn (symbol-name fn))))
     (list (if (equal val "") fn (intern val))
           current-prefix-arg)))
  (if (not function)
      (message "You didn't specify a function")
    (unless (or (not commandp) (commandp function))
      (error "Not a defined Emacs command (interactive function): `%s'" function))
    ;;$$$  (unless (fboundp function)
    ;;       (error "Not a defined Emacs function: `%s'" function))
    (help-setup-xref (list #'describe-function function) (interactive-p))
    (save-excursion
      (with-output-to-temp-buffer (help-buffer)
        (prin1 function)
        ;; Use " is " instead of a colon so that
        ;; it is easier to get out the function name using forward-sexp.
        (princ " is ")
        (describe-function-1 function)
        (print-help-return-message)
        (with-current-buffer standard-output (buffer-string)))))) ; Return help text.


;; REPLACES ORIGINAL in `help.el':
;; Fill long lines.  Add `,' before "which".
;;
(when (< emacs-major-version 23)
  (defun describe-function-1 (function)
    (let* ((def (if (symbolp function)
                    (symbol-function function)
                  function))
           file-name string
           (beg (if (commandp def) "an interactive " "a "))
           (pt1 (with-current-buffer (help-buffer) (point))))
      (setq string
            (cond ((or (stringp def)
                       (vectorp def))
                   "a keyboard macro")
                  ((subrp def)
                   (if (eq 'unevalled (cdr (subr-arity def)))
                       (concat beg "special form")
                     (concat beg "built-in function")))
                  ((byte-code-function-p def)
                   (concat beg "compiled Lisp function"))
                  ((symbolp def)
                   (while (symbolp (symbol-function def))
                     (setq def (symbol-function def)))
                   (format "an alias for `%s'" def))
                  ((eq (car-safe def) 'lambda)
                   (concat beg "Lisp function"))
                  ((eq (car-safe def) 'macro)
                   "a Lisp macro")
                  ((eq (car-safe def) 'autoload)
                   (setq file-name (nth 1 def))
                   (format "%s autoloaded %s"
                           (if (commandp def) "an interactive" "an")
                           (if (eq (nth 4 def) 'keymap) "keymap"
                             (if (nth 4 def) "Lisp macro" "Lisp function"))
                           ))
                  ((keymapp def)
                   (let ((is-full nil)
                         (elts (cdr-safe def)))
                     (while elts
                       (if (char-table-p (car-safe elts))
                           (setq is-full t
                                 elts nil))
                       (setq elts (cdr-safe elts)))
                     (if is-full
                         "a full keymap"
                       "a sparse keymap")))
                  (t "")))
      (princ string)
      (with-current-buffer standard-output
        (save-excursion
          (save-match-data
            (if (re-search-backward "alias for `\\([^`']+\\)'" nil t)
                (help-xref-button 1 'help-function def)))))
      (or file-name
          (setq file-name (symbol-file function 'defun)))
      (setq file-name (describe-simplify-lib-file-name file-name))
      (when (equal file-name "loaddefs.el")
        ;; Find the real def site of the preloaded function.
        ;; This is necessary only for defaliases.
        (let ((location
               (condition-case nil
                   (find-function-search-for-symbol function nil "loaddefs.el")
                 (error nil))))
          (when location
            (with-current-buffer (car location)
              (goto-char (cdr location))
              (when (re-search-backward
                     "^;;; Generated autoloads from \\(.*\\)" nil t)
                (setq file-name (match-string 1)))))))
      (when (and (null file-name) (subrp def))
        ;; Find the C source file name.
        (setq file-name (if (get-buffer " *DOC*")
                            (help-C-file-name def 'subr)
                          'C-source)))
      (when file-name
        (princ " in `")
        ;; We used to add .el to the file name,
        ;; but that's completely wrong when the user used load-file.
        (princ (if (eq file-name 'C-source) "C source code" file-name))
        (princ "'")
        ;; Make a hyperlink to the library.
        (with-current-buffer standard-output
          (save-excursion
            (re-search-backward "`\\([^`']+\\)'" nil t)
            (help-xref-button 1 'help-function-def function file-name))))
      (princ ".")
      (with-current-buffer (help-buffer)
        (fill-region-as-paragraph (save-excursion (goto-char pt1) (forward-line 0) (point))
                                  (point)))
      (terpri)(terpri)
      (when (commandp function)
        (let ((pt2 (with-current-buffer (help-buffer) (point))))
          (if (and (eq function 'self-insert-command)
                   (eq (key-binding "a") 'self-insert-command)
                   (eq (key-binding "b") 'self-insert-command)
                   (eq (key-binding "c") 'self-insert-command))
              (princ "It is bound to many ordinary text characters.\n")
            (let* ((remapped (command-remapping function))
                   (keys (where-is-internal
                          (or remapped function) overriding-local-map nil nil))
                   non-modified-keys)
              ;; Which non-control non-meta keys run this command?
              (dolist (key keys)
                (if (member (event-modifiers (aref key 0)) '(nil (shift)))
                    (push key non-modified-keys)))
              (when remapped
                (princ "It is remapped to `")
                (princ (symbol-name remapped))
                (princ "'"))

              (when keys
                (princ (if remapped ", which is bound to " "It is bound to "))
                ;; If lots of ordinary text characters run this command,
                ;; don't mention them one by one.
                (if (< (length non-modified-keys) 10)
                    (princ (mapconcat 'key-description keys ", "))
                  (dolist (key non-modified-keys)
                    (setq keys (delq key keys)))
                  (if keys
                      (progn
                        (princ (mapconcat 'key-description keys ", "))
                        (princ ", and many ordinary text characters"))
                    (princ "many ordinary text characters"))))
              (when (or remapped keys non-modified-keys)
                (princ ".")
                (terpri))))
          (with-current-buffer (help-buffer) (fill-region-as-paragraph pt2 (point)))
          (terpri)))
      (let* ((arglist (help-function-arglist def))
             (doc (documentation function))
             (usage (help-split-fundoc doc function)))
        (with-current-buffer standard-output
          ;; If definition is a keymap, skip arglist note.
          (unless (keymapp def)
            (let* ((use (cond
                          (usage (setq doc (cdr usage)) (car usage))
                          ((listp arglist)
                           (format "%S" (help-make-usage function arglist)))
                          ((stringp arglist) arglist)
                          ;; Maybe the arglist is in the docstring of the alias.
                          ((let ((fun function))
                             (while (and (symbolp fun)
                                         (setq fun (symbol-function fun))
                                         (not (setq usage (help-split-fundoc
                                                           (documentation fun)
                                                           function)))))
                             usage)
                           (car usage))
                          ((or (stringp def)
                               (vectorp def))
                           (format "\nMacro: %s" (format-kbd-macro def)))
                          (t "[Missing arglist.  Please make a bug report.]")))
                   (high (help-highlight-arguments use doc)))
              (let ((fill-begin (point)))
                (insert (car high) "\n")
                (fill-region fill-begin (point)))
              (setq doc (cdr high))))
          (let ((obsolete (and
                           ;; function might be a lambda construct.
                           (symbolp function)
                           (get function 'byte-obsolete-info))))
            (when obsolete
              (princ "\nThis function is obsolete")
              (when (nth 2 obsolete)
                (insert (format " since %s" (nth 2 obsolete))))
              (insert ";\n"
                      (if (stringp (car obsolete)) (car obsolete)
                        (format "use `%s' instead." (car obsolete)))
                      "\n"))
            (insert "\n"
                    (or doc "Not documented."))))))))

;;;###autoload
(defun describe-command (function)
  "Describe an Emacs command (interactive function).
Same as using a prefix arg with `describe-function'."
  (interactive
   (let ((fn (or (and (fboundp 'symbol-nearest-point) (symbol-nearest-point))
                 (function-called-at-point)))
         (enable-recursive-minibuffers t)
         val)
     (setq val (completing-read "Describe command: " obarray 'commandp
                                t nil nil (and fn (symbol-name fn))))
     (list (if (equal val "") fn (intern val)))))
  (describe-function function t))



;; REPLACES ORIGINAL in `help.el':
;; With a prefix argument, candidates are user variables (options) only.
;; Preferred default candidate is `symbol-nearest-point'.
;; Uses `substitute-command-keys' on doc string.
;; Preserves text properties.
;;
(when (< emacs-major-version 23)
  (defun describe-variable (variable &optional buffer optionp)
    "Display the full documentation of VARIABLE (a symbol).
VARIABLE names an Emacs Lisp variable, possibly a user option.
With a prefix argument, candidates are user variables (options) only.
Default candidate is the `symbol-nearest-point'.
Return the documentation, as a string.
If VARIABLE has a buffer-local value in BUFFER (default to the current buffer),
it is displayed along with the global value."
    (interactive
     (let ((symb (or (and (fboundp 'symbol-nearest-point) (symbol-nearest-point))
                     (and (symbolp (variable-at-point)) (variable-at-point))))
           (enable-recursive-minibuffers t)
           val)
       (setq val (completing-read "Describe variable: " obarray
                                  (if current-prefix-arg
                                      (lambda (vv) (user-variable-p vv))
                                    (lambda (vv)
                                      (or (boundp vv) (get vv 'variable-documentation))))
                                  t nil nil (and (symbolp symb) (symbol-name symb))))
       (list (if (equal val "") symb (intern val))
             nil
             current-prefix-arg)))
    (unless (buffer-live-p buffer) (setq buffer (current-buffer)))
    (if (not (symbolp variable))
        (message "You did not specify a variable")
      (unless (or (not optionp) (user-variable-p variable))
        (error "Not a defined Emacs user option: `%s'" variable))
      ;;$$ (unless (boundp variable)
      ;;     (error "Not a defined Emacs variable: `%s'" variable))
      (save-excursion
        (let* ((valvoid (not (with-current-buffer buffer (boundp variable))))
               ;; Extract the value before setting up the output buffer,
               ;; in case `buffer' *is* the output buffer.
               (val (and (not valvoid) (buffer-local-value variable buffer)))
               val-start-pos)
          (help-setup-xref (list #'describe-variable variable buffer) (interactive-p))
          (with-output-to-temp-buffer (help-buffer)
            (with-current-buffer buffer
              (prin1 variable)
              ;; Make a hyperlink to the library if appropriate.  (Don't
              ;; change the format of the buffer's initial line in case
              ;; anything expects the current format.)
              (let ((file-name (symbol-file variable 'defvar)))
                (setq file-name (describe-simplify-lib-file-name file-name))
                (when (equal file-name "loaddefs.el")
                  ;; Find the real def site of the preloaded variable.
                  (let ((location
                         (condition-case nil
                             (find-variable-noselect variable file-name)
                           (error nil))))
                    (when location
                      (with-current-buffer (car location)
                        (when (cdr location) (goto-char (cdr location)))
                        (when (re-search-backward
                               "^;;; Generated autoloads from \\(.*\\)" nil t)
                          (setq file-name (match-string 1)))))))
                (when (and (null file-name)
                           (integerp (get variable 'variable-documentation)))
                  ;; It's a variable not defined in Elisp but in C.
                  (setq file-name (if (get-buffer " *DOC*")
                                      (help-C-file-name variable 'var)
                                    'C-source)))
                (if file-name
                    (progn
                      (princ " is a variable defined in `")
                      (princ (if (eq file-name 'C-source) "C source code" file-name))
                      (princ "'.\n")
                      (with-current-buffer standard-output
                        (save-excursion
                          (re-search-backward "`\\([^`']+\\)'" nil t)
                          (help-xref-button 1 'help-variable-def variable file-name)))
                      (if valvoid
                          (princ "It is void as a variable.")
                        (princ "Its ")))
                  (if valvoid
                      (princ " is void as a variable.")
                    (princ "'s "))))
              (unless valvoid
                (with-current-buffer standard-output
                  (setq val-start-pos (point))
                  (princ "value is ") (terpri)
                  (let ((from (point)))
                    (pp val)
                    ;; Hyperlinks in variable's value are quite frequently
                    ;; inappropriate e.g C-h v <RET> features <RET>
                    ;; (help-xref-on-pp from (point))
                    (when (< (point) (+ from 20)) (delete-region (1- from) from)))))
              (terpri)
              (when (local-variable-p variable)
                (princ (format "%socal in buffer %s; "
                               (if (get variable 'permanent-local) "Permanently l" "L")
                               (buffer-name)))
                (if (not (default-boundp variable))
                    (princ "globally void")
                  (let ((val (default-value variable)))
                    (with-current-buffer standard-output
                      (princ "global value is ") (terpri)
                      ;; Fixme: pp can take an age if you happen to
                      ;; ask for a very large expression.  We should
                      ;; probably print it raw once and check it's a
                      ;; sensible size before prettyprinting.  -- fx
                      (let ((from (point)))
                        (pp val)
                        ;; See previous comment for this function.
                        ;; (help-xref-on-pp from (point))
                        (when (< (point) (+ from 20)) (delete-region (1- from) from)))))))
              ;; Add a note for variables that have been make-var-buffer-local.
              (when (and (local-variable-if-set-p variable)
                         (or (not (local-variable-p variable))
                             (with-temp-buffer (local-variable-if-set-p variable))))
                (princ "\nAutomatically becomes buffer-local when set in any fashion.\n"))
              (terpri)
              ;; If the value is large, move it to the end.
              (with-current-buffer standard-output
                (when (> (count-lines (point-min) (point-max)) 10)
                  ;; Note that setting the syntax table like below
                  ;; makes forward-sexp move over a `'s' at the end
                  ;; of a symbol.
                  (set-syntax-table emacs-lisp-mode-syntax-table)
                  (goto-char val-start-pos)
                  ;; The line below previously read as
                  ;; (delete-region (point) (progn (end-of-line) (point)))
                  ;; which suppressed display of the buffer local value for
                  ;; large values.
                  (when (looking-at "value is") (replace-match ""))
                  (save-excursion
                    (insert "\n\nValue:")
                    (set (make-local-variable 'help-button-cache)
                         (point-marker)))
                  (insert "value is shown ")
                  (insert-button "below" 'action help-button-cache 'follow-link t
                                 'help-echo "mouse-2, RET: show value")
                  (insert ".\n")))
              ;; Mention if it's an alias
              (let* ((alias (condition-case nil
                                (indirect-variable variable)
                              (error variable)))
                     (obsolete (get variable 'byte-obsolete-variable))
                     (safe-var (get variable 'safe-local-variable))
                     (doc (or (documentation-property variable 'variable-documentation)
                              (documentation-property alias 'variable-documentation))))
                (unless (eq alias variable)
                  (princ (format "\nThis variable is an alias for `%s'.\n" alias)))
                (when (or obsolete safe-var) (terpri))
                (when obsolete
                  (princ "This variable is obsolete")
                  (if (cdr obsolete) (princ (format " since %s" (cdr obsolete))))
                  (princ ";") (terpri)
                  (princ (if (stringp (car obsolete))
                             (car obsolete)
                           (format "use `%s' instead." (car obsolete))))
                  (terpri))
                (when safe-var
                  (princ "This variable is safe as a file local variable ")
                  (princ "if its value\nsatisfies the predicate ")
                  (princ (if (byte-code-function-p safe-var)
                             "which is byte-compiled expression.\n"
                           (format "`%s'.\n" safe-var))))
                (princ "\nDocumentation:\n")
                ;; Use `insert', not `princ', to keep text properties.
                ;; (princ (or doc "Not documented as a variable.")))
                (with-current-buffer standard-output
                  (insert (or (substitute-command-keys doc)
                              "Not documented as a variable."))))
              ;; Make a link to customize if this variable can be customized.
              (if (custom-variable-p variable)
                  (let ((customize-label "customize"))
                    (terpri) (terpri)
                    (princ (concat "You can " customize-label " this variable."))
                    (with-current-buffer standard-output
                      (save-excursion
                        (re-search-backward (concat "\\(" customize-label "\\)") nil t)
                        (help-xref-button 1 'help-customize-variable variable)))))
              (print-help-return-message)
              ;; Return the text we displayed.
              (with-current-buffer standard-output (buffer-string)))))))))

;;; These two macros are no different from what is in vanilla Emacs 23.
;;; Add them here so this file can be byte-compiled with Emacs 22 and used with Emacs 23.
(defmacro with-selected-frame (frame &rest body)
  "Execute the forms in BODY with FRAME as the selected frame.
The value returned is the value of the last form in BODY.
See also `with-temp-buffer'."
  (declare (indent 1) (debug t))
  (let ((old-frame (make-symbol "old-frame"))
        (old-buffer (make-symbol "old-buffer")))
    `(let ((,old-frame (selected-frame))
           (,old-buffer (current-buffer)))
       (unwind-protect
            (progn (select-frame ,frame)
                   ,@body)
         (if (frame-live-p ,old-frame)
             (select-frame ,old-frame))
         (if (buffer-live-p ,old-buffer)
             (set-buffer ,old-buffer))))))

(defmacro with-help-window (buffer-name &rest body)
  "Display buffer BUFFER-NAME in a help window evaluating BODY.
Select help window if the actual value of the user option
`help-window-select' says so.  Return last value in BODY."
  (declare (indent 1) (debug t))
  ;; Bind list-of-frames to `frame-list' and list-of-window-tuples to a
  ;; list of one <window window-buffer window-start window-point> tuple
  ;; for each live window.
  `(let ((list-of-frames (frame-list))
         (list-of-window-tuples
          (let (list)
            (walk-windows
             (lambda (window)
               (push (list window (window-buffer window)
                           (window-start window) (window-point window))
                     list))
             'no-mini t)
            list)))
     ;; Make `help-window' t to trigger `help-mode-finish' to set
     ;; `help-window' to the actual help window.
     (setq help-window t)
     ;; Make `help-window-point-marker' point nowhere (the only place
     ;; where this should be set to a buffer position is within BODY).
     (set-marker help-window-point-marker nil)
     (prog1
         ;; Return value returned by `with-output-to-temp-buffer'.
         (with-output-to-temp-buffer ,buffer-name
           (progn ,@body))
       (when (windowp help-window)
         ;; Set up help window.
         (help-window-setup list-of-frames list-of-window-tuples))
       ;; Reset `help-window' to nil to avoid confusing future calls of
       ;; `help-mode-finish' with plain `with-output-to-temp-buffer'.
       (setq help-window nil))))



;; REPLACES ORIGINAL in `help.el':
;; With a prefix argument, candidates are user variables (options) only.
;; Preferred default candidate is `symbol-nearest-point'.
;; Preserves text properties.
;;
(unless (< emacs-major-version 23)
  (defun describe-variable (variable &optional buffer frame optionp)
    "Display the full documentation of VARIABLE (a symbol).
VARIABLE names an Emacs Lisp variable, possibly a user option.
With a prefix argument, candidates are user variables (options) only.
Default candidate is the `symbol-nearest-point'.
Return the documentation, as a string.
If VARIABLE has a buffer-local value in BUFFER or FRAME
\(default to the current buffer and current frame),
it is displayed along with the global value."
    (interactive
     (let ((symb (or (and (fboundp 'symbol-nearest-point) (symbol-nearest-point))
                     (and (symbolp (variable-at-point)) (variable-at-point))))
           (enable-recursive-minibuffers t)
           val)
       (setq val (completing-read "Describe variable: " obarray
                                  (if current-prefix-arg
                                      (lambda (vv) (user-variable-p vv))
                                    (lambda (vv)
                                      (or (boundp vv) (get vv 'variable-documentation))))
                                  t nil nil (and (symbolp symb) (symbol-name symb))))
       (list (if (equal val "") symb (intern val))
             nil
             nil
             current-prefix-arg)))
    (let (file-name)
      (unless (buffer-live-p buffer) (setq buffer (current-buffer)))
      (unless (frame-live-p frame) (setq frame (selected-frame)))
      (if (not (symbolp variable))
          (message "You did not specify a variable")
        (unless (or (not optionp) (user-variable-p variable))
          (error "Not a defined Emacs user option: `%s'" variable))
        ;;$$ (unless (boundp variable)
        ;;     (error "Not a defined Emacs variable: `%s'" variable))
        (save-excursion
          (let ((valvoid (not (with-current-buffer buffer (boundp variable))))
                val val-start-pos locus)
            ;; Extract the value before setting up the output buffer,
            ;; in case `buffer' *is* the output buffer.
            (unless valvoid
              (with-selected-frame frame
                (with-current-buffer buffer
                  (setq val (symbol-value variable)
                        locus (variable-binding-locus variable)))))
            (help-setup-xref (list #'describe-variable variable buffer)
                             (interactive-p))
            (with-help-window (help-buffer)
              (with-current-buffer buffer
                (prin1 variable)
                (setq file-name (find-lisp-object-file-name variable 'defvar))
                (if file-name
                    (progn
                      (princ " is a variable defined in `")
                      (princ (if (eq file-name 'C-source) "C source code"
                               (file-name-nondirectory file-name)))
                      (princ "'.\n")
                      (with-current-buffer standard-output
                        (save-excursion
                          (re-search-backward "`\\([^`']+\\)'" nil t)
                          (help-xref-button 1 'help-variable-def
                                            variable file-name)))
                      (if valvoid
                          (princ "It is void as a variable.")
                        (princ "Its ")))
                  (if valvoid
                      (princ " is void as a variable.")
                    (princ "'s "))))
              (if valvoid
                  nil
                (with-current-buffer standard-output
                  (setq val-start-pos (point))
                  (princ "value is ")
                  (terpri)
                  (let ((from (point)))
                    (pp val)
                    ;; Hyperlinks in variable's value are quite frequently
                    ;; inappropriate e.g C-h v <RET> features <RET>
                    ;; (help-xref-on-pp from (point))
                    (if (< (point) (+ from 20))
                        (delete-region (1- from) from)))))
              (terpri)

              (when locus
                (if (bufferp locus)
                    (princ (format "%socal in buffer %s; "
                                   (if (get variable 'permanent-local)
                                       "Permanently l" "L")
                                   (buffer-name)))
                  (princ (format "It is a frame-local variable; ")))
                (if (not (default-boundp variable))
                    (princ "globally void")
                  (let ((val (default-value variable)))
                    (with-current-buffer standard-output
                      (princ "global value is ")
                      (terpri)
                      ;; Fixme: pp can take an age if you happen to
                      ;; ask for a very large expression.  We should
                      ;; probably print it raw once and check it's a
                      ;; sensible size before prettyprinting.  -- fx
                      (let ((from (point)))
                        (pp val)
                        ;; See previous comment for this function.
                        ;; (help-xref-on-pp from (point))
                        (if (< (point) (+ from 20))
                            (delete-region (1- from) from))))))
                (terpri))

              ;; If the value is large, move it to the end.
              (with-current-buffer standard-output
                (when (> (count-lines (point-min) (point-max)) 10)
                  ;; Note that setting the syntax table like below
                  ;; makes forward-sexp move over a `'s' at the end
                  ;; of a symbol.
                  (set-syntax-table emacs-lisp-mode-syntax-table)
                  (goto-char val-start-pos)
                  ;; The line below previously read as
                  ;; (delete-region (point) (progn (end-of-line) (point)))
                  ;; which suppressed display of the buffer local value for
                  ;; large values.
                  (when (looking-at "value is") (replace-match ""))
                  (save-excursion
                    (insert "\n\nValue:")
                    (set (make-local-variable 'help-button-cache)
                         (point-marker)))
                  (insert "value is shown ")
                  (insert-button "below"
                                 'action help-button-cache
                                 'follow-link t
                                 'help-echo "mouse-2, RET: show value")
                  (insert ".\n")))
              (terpri)

              (let* ((alias (condition-case nil
                                (indirect-variable variable)
                              (error variable)))
                     (obsolete (get variable 'byte-obsolete-variable))
                     (use (car obsolete))
                     (safe-var (get variable 'safe-local-variable))
                     (doc (or (documentation-property variable 'variable-documentation)
                              (documentation-property alias 'variable-documentation)))
                     (extra-line nil))
                ;; Add a note for variables that have been make-var-buffer-local.
                (when (and (local-variable-if-set-p variable)
                           (or (not (local-variable-p variable))
                               (with-temp-buffer
                                 (local-variable-if-set-p variable))))
                  (setq extra-line t)
                  (princ "  Automatically becomes buffer-local when set in any fashion.\n"))

                ;; Mention if it's an alias
                (unless (eq alias variable)
                  (setq extra-line t)
                  (princ (format "  This variable is an alias for `%s'.\n" alias)))

                (when obsolete
                  (setq extra-line t)
                  (princ "  This variable is obsolete")
                  (if (cdr obsolete) (princ (format " since %s" (cdr obsolete))))
                  (princ (cond ((stringp use) (concat ";\n  " use))
                               (use (format ";\n  use `%s' instead." (car obsolete)))
                               (t ".")))
                  (terpri))
                (when safe-var
                  (setq extra-line t)
                  (princ "  This variable is safe as a file local variable ")
                  (princ "if its value\n  satisfies the predicate ")
                  (princ (if (byte-code-function-p safe-var)
                             "which is byte-compiled expression.\n"
                           (format "`%s'.\n" safe-var))))

                (if extra-line (terpri))
                (princ "Documentation:\n")
                (with-current-buffer standard-output
                  (insert (or doc "Not documented as a variable."))))

              ;; Make a link to customize if this variable can be customized.
              (when (custom-variable-p variable)
                (let ((customize-label "customize"))
                  (terpri)
                  (terpri)
                  (princ (concat "You can " customize-label " this variable."))
                  (with-current-buffer standard-output
                    (save-excursion
                      (re-search-backward
                       (concat "\\(" customize-label "\\)") nil t)
                      (help-xref-button 1 'help-customize-variable variable))))
                ;; Note variable's version or package version
                (let ((output (describe-variable-custom-version-info variable)))
                  (when output
                    (terpri)
                    (terpri)
                    (princ output))))
              ;; Return the text we displayed.
              (with-current-buffer standard-output (buffer-string)))))))))

;;;###autoload
(defun describe-option (variable &optional buffer)
  "Describe an Emacs user variable (option).
Same as using a prefix arg with `describe-variable'."
  (interactive
   (let ((symb (or (and (fboundp 'symbol-nearest-point) (symbol-nearest-point))
                   (and (symbolp (variable-at-point)) (variable-at-point))))
         (enable-recursive-minibuffers t))
     (list (intern (completing-read "Describe user option: " obarray 'user-variable-p
                                    t nil nil (and symb (symbol-name symb)) t)))))
  (describe-variable variable buffer t))

;;;###autoload
(defun describe-option-of-type (type option)
  "Describe an Emacs user OPTION (variable) of a given `defcustom' TYPE.
A prefix argument determines the type-checking behavior:
 - None:         OPTION is defined with TYPE or a subtype of TYPE.
 - Plain `C-u':  OPTION is defined with TYPE or a subtype of TYPE,
                 or its current value is compatible with TYPE.
 - Negative:     OPTION is defined with TYPE (exact match).
 - Non-negative: OPTION is defined with TYPE (exact match),
                 or its current value is compatible with TYPE.

If TYPE is nil (default value) then *all* `defcustom' variables are
potential candidates.  That is different from using `describe-option',
because `describe-option' includes user-variable candidates not
defined with `defcustom' (with `*'-prefixed doc strings)."
  (interactive
   (let* ((symb (or (and (fboundp 'symbol-nearest-point) (symbol-nearest-point))
                    (and (symbolp (variable-at-point)) (variable-at-point))))
          (typ
           (car (condition-case err
                    (read-from-string
                     (let ((types ()))
                       (mapatoms
                        (lambda (cand)
                          (when (custom-variable-p cand)
                            (push (list
                                   (format "%s" (format "%S" (get cand 'custom-type))))
                                  types))))
                       (completing-read "Describe option of type: "
                                        (help-remove-duplicates types)
                                        nil nil nil nil "nil")))
                  (end-of-file (error "No such custom type")))))
          (pref-arg current-prefix-arg))
     (list typ
           (intern
            (completing-read
             "Option: " obarray
             (lambda (v)
               (and (custom-variable-p v)
                    (or (not typ) ; Allow all vars if requested type = nil.
                        (help-var-is-of-type-p
                         v (list typ)
                         (cond ((not pref-arg) 'inherit)
                               ((consp pref-arg) 'inherit-or-value)
                               ((wholenump (prefix-numeric-value pref-arg))
                                'direct-or-value)
                               (t 'direct))))))
             t nil nil (and symb (symbol-name symb)) t)))))
  (describe-variable option nil t))

(defun help-var-is-of-type-p (variable types &optional mode)
  "Return non-nil if VARIABLE satisfies one of the custom types in TYPES.
TYPES is a list of `defcustom' type sexps or a list of regexp strings.
TYPES are matched, in order, against VARIABLE's type definition or
VARIABLE's current value, until one is satisfied or all are tried.

If TYPES is a list of regexps, then each is regexp-matched against
VARIABLE's custom type.

Otherwise, TYPES is a list of type sexps, each of which is a
definition acceptable for `defcustom' :type or the first symbol of
such a definition (e.g. `choice').  In this case, two kinds of type
comparison are possible:

1. VARIABLE's custom type, or its first symbol, is matched using
  `equal' against each type in TYPES.

2. VARIABLE's current value is checked against each type in TYPES to
   see if it satisfies one of them.  In this case, VARIABLE's own type
   is not used; VARIABLE might not even be typed - it could be a
   variable not defined using `defcustom'.

For any of the comparisons against VARIABLE's type, either that type
can be checked directly or its supertypes (inherited types) can also
be checked.

These different type-checking possibilities depend on the value of
argument MODE, as follows, and they determine the meaning of the
returned value:

`direct':   VARIABLE's type matches a member of list TYPES
`inherit':  VARIABLE's type matches or is a subtype of a TYPES member
`value':    VARIABLE is bound, and its value satisfies a type in TYPES
`inherit-or-value': `inherit' or `value', tested in that order
`direct-or-value':  `direct' or `value', tested in that order
anything else (default): `inherit'

VARIABLE's current value cannot satisfy a regexp type: it is
impossible to know which concrete types a value must match."
  (case mode
    ((nil inherit)     (help-var-inherits-type-p variable types))
    (inherit-or-value  (or (help-var-inherits-type-p variable types)
                           (help-var-val-satisfies-type-p variable types)))
    (value             (help-var-val-satisfies-type-p variable types))
    (direct            (help-var-matches-type-p variable types))
    (direct-or-value   (or (member (get variable 'custom-type) types)
                           (help-var-val-satisfies-type-p variable types)))
    (otherwise         (help-var-inherits-type-p variable types))))

(defun help-var-matches-type-p (variable types)
  "VARIABLE's type matches a member of TYPES."
  (catch 'help-type-matches
    (let ((var-type (get variable 'custom-type)))
      (dolist (type types)
        (when (if (stringp type)
                  (save-match-data (string-match type (format "%s" (format "%S" var-type))))
                (equal var-type type))
          (throw 'help-type-matches t))))
    nil))

(defun help-var-inherits-type-p (variable types)
  "VARIABLE's type matches or is a subtype of a member of list TYPES."
  (catch 'help-type-inherits
    (let ((var-type (get variable 'custom-type)))
      (dolist (type types)
        (while var-type
          (when (or (and (stringp type)
                         (save-match-data
                           (string-match type (format "%s" (format "%S" var-type)))))
                    (equal type var-type))
            (throw 'help-type-inherits t))
          (when (consp var-type) (setq var-type (car var-type)))
          (when (or (and (stringp type)
                         (save-match-data
                           (string-match type (format "%s" (format "%S" var-type)))))
                    (equal type var-type))
            (throw 'help-type-inherits t))
          (setq var-type (car (get var-type 'widget-type))))
        (setq var-type (get variable 'custom-type))))
    nil))

(defun help-var-val-satisfies-type-p (variable types)
  "VARIABLE is bound, and its value satisfies a type in the list TYPES."
  (and (boundp variable)
       (let ((val (symbol-value variable)))
         (and (widget-convert (get variable 'custom-type))
              (help-value-satisfies-type-p val types)))))

(defun help-value-satisfies-type-p (value types)
  "Return non-nil if VALUE satisfies a type in the list TYPES."
  (catch 'help-type-value-satisfies
    (dolist (type types)
      (unless (stringp type)            ; Skip, for regexp type.
        (setq type (widget-convert type))
        ;; Satisfies if either :match or :validate.
        (when (condition-case nil
                  (progn (when (and (widget-get type :match)
                                    (widget-apply type :match value))
                           (throw 'help-type-value-satisfies t))
                         (when (and (widget-get type :validate)
                                    (progn (widget-put type :value value)
                                           (not (widget-apply type :validate))))
                           (throw 'help-type-value-satisfies t)))
                (error nil))
          (throw 'help-type-value-satisfies t))))
    nil))

(defun help-custom-type (variable)
  "Returns the `defcustom' type of VARIABLE.
Returns nil if VARIABLE is not a user option.

Note: If the library that defines VARIABLE has not yet been loaded,
then `help-custom-type' loads it.  Be sure you want to do that
before you call this function."
  (and (custom-variable-p variable)
       (or (get variable 'custom-type) (progn (custom-load-symbol variable)
                                              (get variable 'custom-type)))))

;; Borrowed from `ps-print.el'
(defun help-remove-duplicates (list)
  "Copy of LIST with duplicate elements removed.  Tested with `equal'."
  (let ((tail list)
        new)
    (while tail
      (unless (member (car tail) new) (push (car tail) new))
      (pop tail))
    (nreverse new)))

;;;###autoload
(defun describe-file (filename)
  "Describe the file named FILENAME.
If FILENAME is nil, describe the current directory.

Starting with Emacs 22, if the file is an image file and you have
command-line tool `exiftool' installed and in your `$PATH' or
`exec-path', then EXIF data (metadata) about the image is included.
See standard Emacs library `image-dired.el' for more information about
`exiftool'."
  (interactive "FDescribe file: ")
  (unless filename (setq filename default-directory))
  (help-setup-xref (list #'describe-file filename) (interactive-p))
  (let ((attrs (file-attributes filename)))
    (unless attrs (error(format "Cannot open file `%s'" filename)))
    (let* ((type            (nth 0 attrs))
           (numlinks        (nth 1 attrs))
           (uid             (nth 2 attrs))
           (gid             (nth 3 attrs))
           (last-access     (nth 4 attrs))
           (last-mod        (nth 5 attrs))
           (last-status-chg (nth 6 attrs))
           (size            (nth 7 attrs))
           (permissions     (nth 8 attrs))
           ;; Skip 9: t iff file's gid would change if file were deleted and recreated.
           (inode           (nth 10 attrs))
           (device          (nth 11 attrs))
           (thumb-string    (and (fboundp 'image-file-name-regexp) ; In `image-file.el' (Emacs 22+).
                                 (if (fboundp 'string-match-p)
                                     (string-match-p (image-file-name-regexp) filename)
                                   (save-match-data
                                     (string-match (image-file-name-regexp) filename)))
                                 (if (fboundp 'display-graphic-p) (display-graphic-p) window-system)
                                 (require 'image-dired nil t)
                                 (image-dired-get-thumbnail-image filename)
                                 (apply #'propertize "XXXX"
                                        `(display ,(append (image-dired-get-thumbnail-image filename)
                                                           '(:margin 10))
                                          rear-nonsticky (display)
                                          mouse-face highlight
                                          follow-link t
                                          help-echo "`mouse-2' or `RET': Show full image"
                                          keymap
                                          (keymap
                                           (mouse-2 . (lambda (e) (interactive "e")
                                                              (find-file ,filename)))
                                           (13 . (lambda () (interactive)
                                                         (find-file ,filename))))))))
           (image-info      (and (require 'image-dired nil t)
                                 (fboundp 'image-file-name-regexp)
                                 (if (fboundp 'string-match-p)
                                     (string-match-p (image-file-name-regexp) filename)
                                   (save-match-data
                                     (string-match (image-file-name-regexp) filename)))
                                 (progn (message "Gathering image data...") t)
                                 (condition-case nil
                                     (let ((all  (help-all-exif-data (expand-file-name filename))))
                                       (concat
                                        (and all (not (zerop (length all)))
                                             (format "\nImage Data (EXIF)\n-----------------\n%s"
                                                     all))))
                                   (error nil))))
           (help-text
            (concat
             (format "Properties of `%s':\n\n" filename)
             (format "File Type:                       %s\n"
                     (cond ((eq t type) "Directory")
                           ((stringp type) (format "Symbolic link to `%s'" type))
                           (t "Normal file")))
             (format "Permissions:                %s\n" permissions)
             (and (not (eq t type)) (format "Size in bytes:              %g\n" size))
             (format-time-string
              "Time of last access:        %a %b %e %T %Y (%Z)\n" last-access)
             (format-time-string
              "Time of last modification:  %a %b %e %T %Y (%Z)\n" last-mod)
             (format-time-string
              "Time of last status change: %a %b %e %T %Y (%Z)\n" last-status-chg)
             (format "Number of links:            %d\n" numlinks)
             (format "User ID (UID):              %s\n" uid)
             (format "Group ID (GID):             %s\n" gid)
             (format "Inode:                      %S\n" inode)
             (format "Device number:              %s\n" device)
             image-info)))
      (with-output-to-temp-buffer "*Help*" (princ help-text))
      (when thumb-string
        (with-current-buffer "*Help*"
          (save-excursion
            (goto-char (point-min))
            (let ((buffer-read-only  nil))
              (when (re-search-forward "Device number:.+\n" nil t) (insert thumb-string))))))
      help-text)))                      ; Return displayed text.

(defun help-all-exif-data (file)
  "Return all EXIF data from FILE, using command-line tool `exiftool'."
  (with-temp-buffer
    (delete-region (point-min) (point-max))
    (unless (eq 0 (call-process shell-file-name nil t nil shell-command-switch
                                (format "exiftool -All \"%s\"" file)))
      (error "Could not get EXIF data"))
    (buffer-substring (point-min) (point-max))))

;;;###autoload
(defun describe-keymap (keymap)
  "Describe bindings in KEYMAP, a variable whose value is a keymap.
Completion is available for the keymap name."
  (interactive
   (list (intern
          (completing-read
           "Keymap: " obarray
           (lambda (m) (and (boundp m) (keymapp (symbol-value m))))
           t nil 'variable-name-history))))
  (unless (and (symbolp keymap) (boundp keymap) (keymapp (symbol-value keymap)))
    (error "`%S' is not a keymapp" keymap))
  (let ((name  (symbol-name keymap))
        (doc   (documentation-property keymap 'variable-documentation)))
    (help-setup-xref (list #'describe-keymap keymap) (interactive-p))
    (with-output-to-temp-buffer "*Help*"
      (princ name) (terpri)
      (princ (make-string (length name) ?-)) (terpri) (terpri)
      (when doc (princ doc) (terpri) (terpri))
      ;; Use `insert' instead of `princ', so control chars (e.g. \377) insert correctly.
      (with-current-buffer "*Help*"
        (insert (substitute-command-keys (concat "\\{" name "}")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'help-fns+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; help-fns+.el ends here
