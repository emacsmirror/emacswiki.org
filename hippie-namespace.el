;;; hippie-namespace.el --- Special treatment for namepsace prefixes in hippie-expand
;;
;; Copyright (c) 2012 Roland Walker
;;
;; Author: Roland Walker walker@pobox.com
;; URL: https://github.com/rolandwalker/hippie-namespace.el
;; Version: 0.5.0
;; Last-Updated: 19 Aug 2012
;; EmacsWiki: HippieNamespace
;; Keywords: completion
;;
;; Simplified BSD License
;;
;;; Commentary:
;;
;; The purpose of hippie-namespace is to save typing.
;;
;; Enabling the minor mode adds a limited number of very common
;; prefixes to the `hippie-expand' expansion list.  These prefixes
;; (deduced from buffer content) will be the first completions
;; considered.
;;
;; Furthermore, namespace completions are treated specially: when
;; `hippie-expand' proposes a namespace completion, it does not cycle.
;; Instead, the completion is immediately accepted, and further
;; invocations of `hippie-expand' build from the expanded text.
;;
;; For example, the common prefix of all symbols in this library is
;; "hippie-namespace-".  If the user types "hi [hippie-expand]" or
;; even just "h [hippie-expand]", the full prefix is expanded.
;;
;; "hi [hippie-expand] [hippie-expand]" will start cycling through the
;; completions which match the prefix.
;;
;; To use this library, install the file somewhere that Emacs can find
;; it and add the following to your ~/.emacs file
;;
;;    (require 'hippie-namespace)
;;    (global-hippie-namespace-mode 1)
;;
;; The minor mode will examine each buffer to guess namespace prefixes
;; dynamically.  If the guess is not good enough, you may add to the
;; list by executing
;;
;;    M-x hippie-namespace-add
;;
;; or by adding a file-local variable at the end of your file:
;;
;;    ;; Local Variables:
;;    ;; hippie-namespace-local-list: (namespace-1 namespace-2)
;;    ;; End:
;;
;; Note that you should also have `hippie-expand' bound to a key.
;; Many people override dabbrev expansion:
;;
;;    (define-key global-map (kbd "M-/") 'hippie-expand)
;;
;; See Also
;;
;;    M-x customize-group RET hippie-namespace RET
;;    M-x customize-group RET hippie-expand RET
;;
;; Notes
;;
;;    This mode makes more sense for some languages and less sense for
;;    others.  In most languages, the declared "namespace" is
;;    infrequently used in its own context.  (For Emacs Lisp that is
;;    not the case.)
;;
;;    Some attempt is made to detect the import of external
;;    namespaces, and a textual analysis is done, but nothing fancy.
;;
;; Compatibility
;;
;;     Tested only on GNU Emacs version 24.1
;;
;; Bugs
;;
;;     Breaks using C-u [hippie-expand] to undo. Workaround: use
;;     regular undo commands.
;;
;; TODO
;;
;;     more and better language-specific functions
;;
;;     javascript namespaces implicit, and a pain to deduce
;;
;;     clever interface to support identical subseqs in the
;;     namespace list
;;
;;     periodic refresh: idle-timer and save hook?
;;
;;; License
;;
;;  Simplified BSD License
;;
;;  Copyright (c) 2012, Roland Walker
;;  All rights reserved.
;;  Redistribution and use in source and binary forms, with or
;;  without modification, are permitted provided that the following
;;  conditions are met:
;;
;;     1. Redistributions of source code must retain the above
;;        copyright notice, this list of conditions and the following
;;        disclaimer.
;;
;;     2. Redistributions in binary form must reproduce the above
;;        copyright notice, this list of conditions and the following
;;        disclaimer in the documentation and/or other materials
;;        provided with the distribution.
;;
;;  This software is provided by Roland Walker "AS IS" and any express
;;  or implied warranties, including, but not limited to, the implied
;;  warranties of merchantability and fitness for a particular
;;  purpose are disclaimed. In no event shall Roland Walker or
;;  contributors be liable for any direct, indirect, incidental,
;;  special, exemplary, or consequential damages (including, but not
;;  limited to, procurement of substitute goods or services; loss of
;;  use, data, or profits; or business interruption) however caused
;;  and on any theory of liability, whether in contract, strict
;;  liability, or tort (including negligence or otherwise) arising in
;;  any way out of the use of this software, even if advised of the
;;  possibility of such damage.
;;
;;  The views and conclusions contained in the software and
;;  documentation are those of the authors and should not be
;;  interpreted as representing official policies, either expressed
;;  or implied, of Roland Walker.
;;
;;
;;; Code:
;;

;;;
;;; requires
;;;

;; for setf, loop, position, remove-if, remove-if-not, callf, callf2
(eval-when-compile
  (require 'cl))

(require 'imenu)

;;;
;;; customizable variables
;;;

;;;###autoload
(defgroup hippie-namespace nil
  "Special treatment for namepsace prefixes in `hippie-expand'."
  :version "0.5.0"
  :link '(emacs-commentary-link "hippie-namespace")
  :prefix "hippie-namespace-"
  :group 'hippie-expand
  :group 'abbreviations
  :group 'extensions)

(defcustom hippie-namespace-full-text-search nil
  "Run a full-text analysis of the buffer looking for prefixes.

This is somewhat slow, though it might find something overlooked
by imenu, which only considers definitions."
  :type 'boolean
  :group 'hippie-namespace)

(defcustom hippie-namespace-popularity-cutoff .20
  "A common prefix must be found on this fraction of symbols in a buffer to be considered.

The default value is .20, meaning 20%."
  :type 'float
  :group 'hippie-namespace)

(defcustom hippie-namespace-minimum-length 3
  "Prefixes smaller than this length will be discarded."
  :type 'integer
  :group 'hippie-namespace)

(defcustom hippie-namespace-max-elements 10
  "The maximum number of elements against which `try-expand-namespace' will attempt completion.

Set to nil or a very large number if you don't want a limit."
  :type 'integer
  :group 'hippie-namespace)

(defcustom hippie-namespace-mode-lighter " hipn"
  "This string appears in the mode-line when `hippie-namespace-mode' is active.

Set to nil or the empty string to disable the mode-line
lighter for `hippie-namespace-mode'."
  :type 'string
  :risky t
  :group 'hippie-namespace)

(defcustom hippie-namespace-less-feedback nil
  "Give less echo area feedback."
  :type 'boolean
  :group 'hippie-namespace)

(defcustom hippie-namespace-no-localize-try-functions nil
  "Don't make `try-functions-list' buffer-local."
  :type 'boolean
  :group 'hippie-namespace)

;;;###autoload
(defgroup hippie-namespace-global nil
  "Settings for `global-hippie-namespace-mode'."
  :group 'hippie-namespace)

(defcustom hippie-namespace-exclude-modes '(
                                            fundamental-mode
                                            Buffer-menu-mode
                                            bm-show-mode
                                            dired-mode
                                            eshell-mode
                                            gnus-article-mode
                                            mime/viewer-mode
                                            rmail-mode
                                            term-mode
                                            )
  "The minor mode will not be activated if a buffer's major-mode is included in this list."
  :type '(repeat symbol)
  :group 'hippie-namespace-global)

(defcustom hippie-namespace-buffer-name-exclude-pattern "\\`[* ]"
  "The minor mode will not be activated if a buffer's name matches this regular expression.

The default pattern is designed to match buffers which are
programatically generated or internal to Emacs."
  :type 'regexp
  :group 'hippie-namespace-global)

(defcustom hippie-namespace-buffer-include-functions '(buffer-file-name)
  "The minor mode will not be activated in a buffer unless all listed functions evaluate to non-nil.

Each function should take a single argument (a buffer).  The
default filter causes `hippie-namespace-mode' to consider only buffers
which are associated with a file.

Set this value to nil to disable."
  :type '(repeat function)
  :group 'hippie-namespace-global)

(defcustom hippie-namespace-buffer-exclude-functions '()
  "The minor mode will not be activated in a buffer if any listed functions evaluate to non-nil.

Each function should take a single argument (a buffer).

Set this value to nil to disable."
  :type '(repeat function)
  :group 'hippie-namespace-global)

;;; variables

(defvar hippie-namespace-local-list    nil "List of namespaces to be provided as a file-local variable.")
(defvar hippie-namespace-manual-list   nil "List of namespaces entered manually by `hippie-namespace-add'.")
(defvar hippie-namespace-computed-list nil "Computed list of namespaces derived from all sources.")

(make-variable-buffer-local 'hippie-namespace-local-list)
(make-variable-buffer-local 'hippie-namespace-manual-list)
(make-variable-buffer-local 'hippie-namespace-computed-list)

(put 'hippie-namespace-local-list 'safe-local-variable 'listp) ; value may be set in file-local fashion to any list

;;; macros

(defmacro hippie-namespace-called-interactively-p (&optional kind)
  "A backward-compatible version of `called-interactively-p'."
  `(if (eq 0 (cdr (subr-arity (symbol-function 'called-interactively-p))))
      (called-interactively-p)
    (called-interactively-p ,kind)))

;;; advice for hippie-expand functions

;; todo bug: breaks using C-u hippie-expand to undo - b/c
;; hippie-expand no longer knows the original string boundary.
(defadvice hippie-expand (after hippie-expand-accept-namespace activate)
  "Causes an expansion from `try-expand-namespace' to be accepted immediately."
  (when (and hippie-namespace-mode
             (= 0 he-num)
             (eq (nth he-num hippie-expand-try-functions-list) 'try-expand-namespace)
             (memq (car he-tried-table) hippie-namespace-computed-list))
    ;; (setq he-tried-table nil)
    ;; (setq he-num -1)
    (setq he-search-string (car he-tried-table))
    (pop he-tried-table)))

;;; minor-mode setup

(define-minor-mode hippie-namespace-mode
  "Turn on hippie-namespace-mode.

When called interactively with no prefix argument this command
toggles the mode.  With a prefix argument, it enables the mode
if the argument is positive and otherwise disables the mode.

When called from Lisp, this command enables the mode if the
argument is omitted or nil, and toggles the mode if the argument
is 'toggle."
  :lighter hippie-namespace-mode-lighter
  :group 'hippie-namespace
  (cond
    (hippie-namespace-mode
     (hippie-namespace-populate-list)
     (unless hippie-namespace-no-localize-try-functions
       (make-local-variable 'hippie-expand-try-functions-list))
     (push 'try-expand-namespace hippie-expand-try-functions-list)
     (delete-dups hippie-expand-try-functions-list)
     (when (and (hippie-namespace-called-interactively-p 'interactive) (not hippie-namespace-less-feedback))
       (message "hippie-namespace mode enabled")))
    (t
     (setq hippie-namespace-computed-list nil)
     (callf2 delq 'try-expand-namespace hippie-expand-try-functions-list)
     (when (and (hippie-namespace-called-interactively-p 'interactive) (not hippie-namespace-less-feedback))
       (message "hippie-namespace mode disabled")))))

;;; global minor-mode setup

(define-globalized-minor-mode global-hippie-namespace-mode hippie-namespace-mode hippie-namespace-maybe-turn-on
  :group 'hippie-namespace)

(defun hippie-namespace-maybe-turn-on (&optional arg)
  "Called by `global-hippie-namespace-mode' to activate `hippie-namespace-mode' in a buffer if appropriate.

`hippie-namespace-mode' will be activated in every buffer, except

   minibuffers
   buffers with names that begin with space
   buffers excluded by `hippie-namespace-exclude-modes'
   buffers excluded by `hippie-namespace-buffer-name-exclude-pattern'
   buffers that fail   `hippie-namespace-include-functions'
   buffers that pass   `hippie-namespace-exclude-functions'

If called with a negative ARG, deactivate `hippie-namespace-mode' in the buffer."
  (callf or arg 1)
  (when (or (< arg 0)
            (hippie-namespace-buffer-included-p (current-buffer)))
    (hippie-namespace-mode arg)))

(defun hippie-namespace-buffer-included-p (buf)
  "Return BUF if `global-hippie-namespace-mode' should enable `hippie-namespace-mode' in BUF."
  (when (and (not noninteractive)
             (bufferp buf)
             (buffer-name buf))
    (with-current-buffer buf
      (when (and (not (minibufferp buf))
                 (not (eq (aref (buffer-name) 0) ?\s))           ; overlaps with hippie-namespace-buffer-exclude-pattern
                 (not (memq major-mode hippie-namespace-exclude-modes))
                 (not (string-match-p hippie-namespace-buffer-name-exclude-pattern (buffer-name buf)))
                 (catch 'success
                   (dolist (filt hippie-namespace-buffer-exclude-functions)
                     (when (funcall filt buf)
                       (throw 'success nil)))
                   t)
                 (catch 'failure
                   (dolist (filt hippie-namespace-buffer-include-functions)
                     (unless (funcall filt buf)
                       (throw 'failure nil)))
                   t))
        buf))))

;;; interactive commands

(defun hippie-namespace-reload (arg)
  "Force a refresh of `hippie-namespace-computed-list', used by `try-expand-namespace'.

With prefix ARG, also wipe `hippie-namespace-manual-list'."
  (interactive "P")
  (when arg
    (setq hippie-namespace-manual-list nil))
  (hippie-namespace-populate-list 'force)
  (when (hippie-namespace-called-interactively-p 'interactive)
    (message "%s namespace string%s available." (length hippie-namespace-computed-list) (if (= 1 (length hippie-namespace-computed-list)) "" "s"))))

(defun hippie-namespace-add (namespace)
  "Manually add NAMESPACE to the list of namespaces available to `try-expand-namespace'.

Modifies `hippie-namespace-manual-list', and refreshes by running
`hippie-namespace-populate-list'."
  (interactive "sNamespace to add: ")
  (unless (string-match-p "\\`\\(?:\\sw\\|\\s_\\)+\\'" namespace)
    (error "Bad namespace"))
  (add-to-list 'hippie-namespace-manual-list namespace t)
  (hippie-namespace-populate-list 'force)
  (when (hippie-namespace-called-interactively-p 'interactive)
    (message "%s namespace string%s available." (length hippie-namespace-computed-list) (if (= 1 (length hippie-namespace-computed-list)) "" "s"))))

;;; the next two functions provide the interface with hippie-expand

(defun he-namespace-beg ()
  (save-excursion
    (skip-syntax-backward "^w_")
    (skip-syntax-backward "w_")
    (point)))

(defun try-expand-namespace (old)
  "Try to expand an element of `hippie-namespace-computed-list' in `hippie-expand'.

Intended to be used as the first element of
`hippie-expand-try-functions-list'."
  (unless old
    (he-init-string (he-namespace-beg) (point))
    (setq he-expand-list (sort
                          (all-completions he-search-string (mapcar 'list hippie-namespace-computed-list))
                          'string-lessp)))
  (while (and he-expand-list
              (he-string-member (car he-expand-list) he-tried-table))
    (pop he-expand-list))
  (if (null he-expand-list)
      (progn
        (when old (he-reset-string))
        ())
    (he-substitute-string (car he-expand-list))
    (setq he-tried-table (cons (car he-expand-list) (cdr he-tried-table)))
    (pop he-expand-list)
    t))

;;; functions to populate the computed namespace list

(defun hippie-namespace-populate-list (&optional force)
  "Populate `hippie-namespace-computed-list' from buffer contents.

When optional FORCE is set, repopulate even if
`hippie-namespace-computed-list' is already set."

  (when (or force (null hippie-namespace-computed-list))
    (setq hippie-namespace-computed-list (append hippie-namespace-local-list hippie-namespace-manual-list))

    (let ((fn (intern-soft (concat "hippie-namespace-finder-" (symbol-name major-mode)))))
      (when (fboundp fn)
        (save-excursion
          (save-restriction
            (save-match-data
              (callf2 append (funcall fn) hippie-namespace-computed-list)))))

      (setq hippie-namespace-computed-list (append (hippie-namespace-generic-finder 'imenu)
                                                   hippie-namespace-computed-list))

      (when hippie-namespace-full-text-search
        (setq hippie-namespace-computed-list (append (hippie-namespace-generic-finder 'fulltext)
                                                     hippie-namespace-computed-list)))

      (callf reverse hippie-namespace-computed-list)          ; now they are in order of priority
      (callf hippie-namespace-strip-prefix-matches hippie-namespace-computed-list)
      (delete-dups hippie-namespace-computed-list)

      (when (and hippie-namespace-max-elements
                 (> (length hippie-namespace-computed-list) hippie-namespace-max-elements))
        (setf (nthcdr hippie-namespace-max-elements hippie-namespace-computed-list) nil)))))

;;; generic namespace-finder functions

(defun hippie-namespace-generic-finder (&optional method)
  "This namespace finder works for any major mode supported by `imenu'.

If optional METHOD is 'fulltext, scans the full text of the buffer,
which is slower."
  (let ((sym-strings (if (eq method 'fulltext) (hippie-namespace-all-symbols) (hippie-namespace-all-imenu-definitions)))
        (abs-popular-cutoff nil)           ; computed from `hippie-namespace-popularity-cutoff'
        (namespaces nil))
    (setq abs-popular-cutoff (truncate (* hippie-namespace-popularity-cutoff (length sym-strings))))
    (catch 'exhausted
      (while (> (length sym-strings) abs-popular-cutoff)
        (let ((prefix (hippie-namespace-longest-prefix sym-strings abs-popular-cutoff)))
          (if (not (> (length prefix) hippie-namespace-minimum-length))
              (throw 'exhausted t)
            (push prefix namespaces)
            (callf2 remove-if #'(lambda (s) (string-prefix-p prefix s)) sym-strings)))))
    namespaces))

;; todo better removal of language keywords - autocomplete maintains some dictionaries
(defun hippie-namespace-all-symbols ()
  "Return a list of strings comprising possible symbols used in the buffer.

This is necessarily somewhat slow, and language keywords which are
not symbols may be included in the result."
  (let ((sym-strings nil))
    (save-excursion
      (save-match-data
        (goto-char (point-min))
        (while (re-search-forward "\\<\\(?:\\sw\\|\\s_\\)+" nil t)
          (let ((syntax (save-match-data (syntax-ppss))))
            (cond
              ((nth 4 syntax)               ; comment
               (forward-line 1))
              ((nth 3 syntax)               ; string
               (re-search-forward (string (nth 3 (syntax-ppss))) (line-end-position) t))
              (t
               (push (match-string-no-properties 0) sym-strings)))))))

    (delete-dups sym-strings)
    ;; todo why again? and why not +?
    (callf2 remove-if #'(lambda (s) (string-match-p "\\`[0-9.]\\'" s))               sym-strings)
    (callf2 remove-if #'(lambda (s) (<= (length s) hippie-namespace-minimum-length)) sym-strings)

    ;; emacs-lisp-specific kludge to remove language keywords
    (setq sym-strings (remove-if #'(lambda (s) (and (intern-soft s)
                                                    (> (length (symbol-plist (intern-soft s))) 2)))
                                 sym-strings))

    ;; font-lock kludge to remove language keywords
    (dolist (kw font-lock-keywords)
      (when (listp kw)
        (callf car kw))
      (when (stringp kw)
        (callf2 remove-if #'(lambda (s) (string-match-p (concat "\\`" kw "\\'") s)) sym-strings)))

    sym-strings))

(defun hippie-namespace-all-imenu-definitions ()
  "Returns a list strings representing all symbol definitions as determined by imenu."
  (ignore-errors
    (with-no-warnings
      (imenu--cleanup))
    (setq imenu--index-alist nil)
    (imenu--make-index-alist))
  (remove-if-not #'stringp (hippie-namespace-list-flatten (mapcar (lambda (item)
                                                               (if (and item (imenu--subalist-p item))
                                                                   (cdr item) item)) imenu--index-alist))))


;;; mode-specific namespace finder functions
;;
;; Howto:
;;
;; Create a function called hippie-namespace-finder-YOUR-FAVORITE-MODE.
;; The function should return a list of strings, in ascending order of
;; preference.
;;

(defalias 'hippie-namespace-finder-emacs-lisp-mode 'hippie-namespace-finder-lisp-mode)

(defun hippie-namespace-finder-lisp-mode ()
  (let ((namespaces nil))
    (goto-char (point-min))
    (while (re-search-forward "^[ \t]*\\((defgroup\\)" nil t)
      (save-excursion
        (goto-char (match-beginning 1))
        (let ((dg (sexp-at-point))
              (arg-pos nil))
          (setq arg-pos (position :prefix dg))
          (when (and arg-pos
                     (not (= arg-pos (length dg))))
            (push (cadr (nthcdr arg-pos dg)) namespaces)))))
    namespaces))

(defalias 'hippie-namespace-finder-perl-mode 'hippie-namespace-finder-cperl-mode)

(defun hippie-namespace-finder-cperl-mode ()
  (let ((namespaces nil))
    (goto-char (point-min))
    (while (re-search-forward "^[ \t]*use[ \t\n]+\\([A-Z][^ \t\n;]+\\)" nil t)
      (push (concat (match-string-no-properties 1) "::") namespaces))
    (goto-char (point-min))
    (while (re-search-forward "^[ \t]*package[ \t\n]+\\([^ \t\n;]+\\)" nil t)
      (push (concat (match-string-no-properties 1) "::") namespaces))
    namespaces))

(defun hippie-namespace-finder-python-mode ()
  (let ((namespaces nil))
    (goto-char (point-min))
    (while (re-search-forward "^[ \t]*from[ \t]+\\([^ \t\n]+\\)[ \t]+import" nil t)
      (push (concat (match-string-no-properties 1) ".") namespaces))
    (goto-char (point-min))
    (while (re-search-forward "^[ \t]*import[ \t]+\\([^ \t\n;]+\\)" nil t)
      (push (concat (match-string-no-properties 1) ".") namespaces))
    namespaces))

;; todo better to read the module names from the content of files loaded by "require"
(defun hippie-namespace-finder-ruby-mode ()
  (let ((namespaces nil))
    (goto-char (point-min))
    (while (re-search-forward "^[ \t]*module[ \t\n]+\\([^ \t\n]+\\)" nil t)
      (push (concat (match-string-no-properties 1) ".") namespaces))
    namespaces))

(defun hippie-namespace-finder-c++-mode ()
  (let ((namespaces nil))
    (goto-char (point-min))
    (while (re-search-forward "^[ \t]*namespace[ \t\n]+\\([^ \t\n]+\\)" nil t)
      (push (concat (match-string-no-properties 1) "::") namespaces))
    namespaces))

;; not sure how good this is. PHP code need not start at beginning-of-line, for one
(defun hippie-namespace-finder-php-mode ()
  (let ((namespaces nil))
    (goto-char (point-min))
    (while (re-search-forward "^[ \t]*namespace[ \t\n]+\\([^ \t\n;]+\\)" nil t)
      (push (concat "\\" (match-string-no-properties 1) "\\") namespaces))
    (goto-char (point-min))
    (while (re-search-forward "^[ \t]*use[ \t\n]+\\([^ \t\n;]+\\)\\(?:[ \t\n]+as[ \t\n]+\\([^ \t\n;]+\\)\\)?" nil t)
      (push (concat "\\" (or (match-string-no-properties 1) (match-string-no-properties 2)) "\\") namespaces))
    namespaces))

(defun hippie-namespace-finder-java-mode ()
  (let ((namespaces nil))
    (goto-char (point-min))
    (while (re-search-forward "^[ \t]*import[ \t\n]+\\(?:[^ \t\n;]+\\)\\.\\([^ \.\t\n;]+\\)" nil t)
      (push (concat (match-string-no-properties 1) ".") namespaces))
    (goto-char (point-min))
    (while (re-search-forward "^[ \t]*package[ \t\n]+\\([^ \t\n;]+\\)" nil t)
      (push (concat (match-string-no-properties 1) ".") namespaces))
    namespaces))

;; todo import() with parens is missing here
(defun hippie-namespace-finder-go-mode ()
  (let ((namespaces nil))
    (goto-char (point-min))
    (while (re-search-forward "^[ \t]*import[ \t\n]+\"\\(?:[^ \t\n;]+\\)/\\([^ /\t\n;]+\\)\"" nil t)
      (push (concat (match-string-no-properties 1) ".") namespaces))
    (goto-char (point-min))
    (while (re-search-forward "^[ \t]*package[ \t\n]+\\([^ \t\n;]+\\)" nil t)
      (push (concat (match-string-no-properties 1) ".") namespaces))
    namespaces))

;;; utility functions

;; this strips nils, which would be a bug in some contexts
(defun hippie-namespace-list-flatten (list)
  "Flatten LIST which may contain other lists."
  (cond
    ((null list)
     nil)
    ((listp list)
     (append (hippie-namespace-list-flatten (car list)) (hippie-namespace-list-flatten (cdr list))))
    (t
     (list list))))

(defun hippie-namespace-longest-prefix (collection cutoff)
  "Return the longest prefix string in COLLECTION found in at least CUTOFF elements."
  (let ((longest nil))
    (dolist (elt collection)
      (catch 'next
        (loop for i from (length longest) to (1- (length elt))
              if (hippie-namespace-prefix-popular-p (substring elt 0 i) collection cutoff)
              do (setq longest (substring elt 0 i))
              else do (throw 'next t))))
    longest))

(defun hippie-namespace-prefix-popular-p (prefix collection cutoff)
  "Returns t if string PREFIX matches CUTOFF members of COLLECTION."
  (>= (length (delq nil (mapcar #'(lambda (s) (string-prefix-p prefix s)) collection)))
      cutoff))

(defun hippie-namespace-strip-prefix-matches (collection)
  "Remove trailing members of COLLECTION which share a prefix match with any other member.

The prefix match may go in either direction.  The first matching
member of COLLECTION is kept, not the longest.

A modified copy of COLLECTION is returned."
  (let ((uniques nil))
    (dolist (elt collection)
      (catch 'next
        (dolist (pref uniques)
          (when (or (string-prefix-p elt pref)
                    (string-prefix-p pref elt))
            (throw 'next t)))
        (push elt uniques)))
    (nreverse uniques)))

(provide 'hippie-namespace)

;;
;; Emacs
;;
;; Local Variables:
;; indent-tabs-mode: nil
;; mangle-whitespace: t
;; require-final-newline: t
;; coding: utf-8
;; End:
;;

;;; hippie-namespace.el ends here
