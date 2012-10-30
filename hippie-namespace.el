;;; hippie-namespace.el --- Special treatment for namespace prefixes in hippie-expand
;;
;; Copyright (c) 2012 Roland Walker
;;
;; Author: Roland Walker <walker@pobox.com>
;; Homepage: http://github.com/rolandwalker/hippie-namespace
;; URL: http://raw.github.com/rolandwalker/hippie-namespace/master/hippie-namespace.el
;; Version: 0.5.6
;; Last-Updated: 25 Oct 2012
;; EmacsWiki: HippieNamespace
;; Keywords: convenience, lisp, tools, completion
;;
;; Simplified BSD License
;;
;;; Commentary:
;;
;; Quickstart
;;
;;     (require 'hippie-namespace)
;;
;;     (global-hippie-namespace-mode 1)
;;
;;     (define-key global-map (kbd "M-/") 'hippie-expand)
;;
;;     hi [M-/]     ; The first one or two letters of a namespace
;;                  ; found in the current buffer, followed by the
;;                  ; key bound to `hippie-expand'.
;;
;; Explanation
;;
;; The purpose of hippie-namespace is to save typing.
;;
;; Enabling this minor mode adds a limited number of very common
;; prefixes to the `hippie-expand' expansion list.  These prefixes
;; (deduced from buffer content) will be the first completions
;; considered.
;;
;; Furthermore, hippie-namespace completions are treated specially:
;; when `hippie-expand' proposes a namespace completion, it will not
;; cycle.  Instead, the namespace completion is implicitly accepted,
;; and further invocations of `hippie-expand' will build on the
;; expansion.
;;
;; For example, the common prefix of all symbols in this library is
;; "hippie-namespace-".  If, while editing this library, the user
;; types "hi [hippie-expand]" or even just "h [hippie-expand]",
;; the full prefix is expanded.
;;
;; "hi [hippie-expand] [hippie-expand] ..." will then cycle through
;; all completions which match the prefix.
;;
;; To use this library, install the file somewhere that Emacs can find
;; it and add the following to your ~/.emacs file
;;
;;     (require 'hippie-namespace)
;;     (global-hippie-namespace-mode 1)
;;
;; The minor mode will examine each buffer to guess namespace prefixes
;; dynamically.  If the guess is not good enough, you may add to the
;; list by executing
;;
;;     M-x hippie-namespace-add
;;
;; or by adding a file-local variable at the end of your file:
;;
;;     ;; Local Variables:
;;     ;; hippie-namespace-local-list: (namespace-1 namespace-2)
;;     ;; End:
;;
;; Note that you should also have `hippie-expand' bound to a key.
;; Many people override dabbrev expansion:
;;
;;     (define-key global-map (kbd "M-/") 'hippie-expand)
;;
;; See Also
;;
;;     M-x customize-group RET hippie-namespace RET
;;     M-x customize-group RET hippie-expand RET
;;
;; Notes
;;
;;     This mode makes more sense for some languages and less sense for
;;     others.  In most languages, the declared "namespace" is
;;     infrequently used in its own context.  (For Emacs Lisp that is
;;     not the case.)
;;
;;     Some attempt is made to detect the import of external
;;     namespaces, and a textual analysis is done, but nothing fancy.
;;
;;     Integrates with `expand-region', adding an expansion which is
;;     aware of the namespace and non-namespace portions of a symbol.
;;
;;     Mode-specific namespace plugins are easy to write.  Search for
;;     "Howto" in the source.
;;
;; Compatibility and Requirements
;;
;;     GNU Emacs version 24.3-devel     : yes, at the time of writing
;;     GNU Emacs version 24.1 & 24.2    : yes
;;     GNU Emacs version 23.3           : yes
;;     GNU Emacs version 22.3 and lower : no
;;
;;     Uses if present: expand-region.el
;;
;; Bugs
;;
;;     Breaks using C-u [hippie-expand] to undo.  Workaround: use
;;     regular undo commands.
;;
;; TODO
;;
;;     more and better language-specific functions
;;
;;     JavaScript namespaces are implicit, and a pain to deduce
;;
;;     clever interface to support identical subsequences in the
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
;;  purpose are disclaimed.  In no event shall Roland Walker or
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

;;; requirements

;; for setf, loop, callf, callf2, position, remove-if, remove-if-not
(require 'cl)

(require 'imenu)
(require 'hippie-exp)

;;; declarations

(eval-when-compile
  (defvar er/try-expand-list))

;;; customizable variables

;;;###autoload
(defgroup hippie-namespace nil
  "Special treatment for namespace prefixes in `hippie-expand'."
  :version "0.5.6"
  :link '(emacs-commentary-link "hippie-namespace")
  :prefix "hippie-namespace-"
  :group 'hippie-expand
  :group 'abbreviations
  :group 'convenience)

(defcustom hippie-namespace-full-text-search nil
  "Run a full-text analysis of the buffer looking for prefixes.

This is somewhat slow, though it might find something overlooked
by imenu, which only considers definitions."
  :type 'boolean
  :group 'hippie-namespace)

(defcustom hippie-namespace-popularity-cutoff .20
  "Fraction of symbols having a common prefix for namespace detection.

One of the methods used by `hippie-namespace' to autodetect
namespaces is popularity.  This is the minimum popularity needed
for consideration.

The default value is .20, meaning 20%."
  :type 'float
  :group 'hippie-namespace)

(defcustom hippie-namespace-minimum-length 3
  "Prefixes smaller than this length will be discarded."
  :type 'integer
  :group 'hippie-namespace)

(defcustom hippie-namespace-max-elements 10
  "Max elements against which `try-expand-namespace' will attempt completion.

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

(defcustom hippie-namespace-expand-region t
  "Integrate with `expand-region' if present.

Add an expansion to `expand-region' which matches the namespace
or non-namespace portion of a symbol."
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
  "Do not activate minor made in buffers matching this regular expression.

The default pattern is designed to match buffers which are
programatically generated or internal to Emacs."
  :type 'regexp
  :group 'hippie-namespace-global)

(defcustom hippie-namespace-buffer-include-functions '(buffer-file-name)
  "Do not activate minor mode in a buffer unless all functions evaluate non-nil.

Each function should take a single argument (a buffer).  The
default filter causes `hippie-namespace-mode' to consider only buffers
which are associated with a file.

Set this value to nil to disable."
  :type '(repeat function)
  :group 'hippie-namespace-global)

(defcustom hippie-namespace-buffer-exclude-functions '()
  "Do not activate minor mode in a buffer if any function evaluates non-nil.

Each function should take a single argument (a buffer).

Set this value to nil to disable."
  :type '(repeat function)
  :group 'hippie-namespace-global)

;;; variables

(defvar hippie-namespace-mode          nil "Mode variable for `hippie-namespace-mode'.")
(defvar hippie-namespace-local-list    nil "List of namespaces to be provided as a file-local variable.")
(defvar hippie-namespace-manual-list   nil "List of namespaces entered manually by `hippie-namespace-add'.")
(defvar hippie-namespace-computed-list nil "Computed list of namespaces derived from all sources.")

(make-variable-buffer-local 'hippie-namespace-mode)
(make-variable-buffer-local 'hippie-namespace-local-list)
(make-variable-buffer-local 'hippie-namespace-manual-list)
(make-variable-buffer-local 'hippie-namespace-computed-list)

;;;###autoload
(put 'hippie-namespace-local-list 'safe-local-variable 'listp) ; value may be set in file-local fashion to any list

;;; macros

(defmacro hippie-namespace-called-interactively-p (&optional kind)
  "A backward-compatible version of `called-interactively-p'.

Optional KIND is as documented at `called-interactively-p'
in GNU Emacs 24.1 or higher."
  (if (eq 0 (cdr (subr-arity (symbol-function 'called-interactively-p))))
      '(called-interactively-p)
    `(called-interactively-p ,kind)))

;;; compatibility functions

(unless (fboundp 'string-match-p)
  ;; added in 23.x
  (defun string-match-p (regexp string &optional start)
    "Same as `string-match' except this function does not change the match data."
    (let ((inhibit-changing-match-data t))
      (string-match regexp string start))))

(unless (fboundp 'string-prefix-p)
  ;; added in 23.x
  (defun string-prefix-p (str1 str2 &optional ignore-case)
    "Return non-nil if STR1 is a prefix of STR2.
If IGNORE-CASE is non-nil, the comparison is done without paying attention
to case differences."
    (eq t (compare-strings str1 nil nil
                           str2 0 (length str1) ignore-case))))

;;; interface to hippie-expand

;; todo better docstring
(defun he-namespace-beg ()
  "Find beginning of expansion."
  (save-excursion
    (skip-syntax-backward "^w_")
    (skip-syntax-backward "w_")
    (point)))

;; todo better docstring
(defun try-expand-namespace (old)
  "Expand an element of `hippie-namespace-computed-list' in `hippie-expand'.

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

;; advice for hippie-expand functions

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

;;; utility functions

;; buffer functions

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

;; general utility functions

(defun hippie-namespace-list-flatten (list)
  "Flatten LIST which may contain other lists."
  (cond
    ((null list)
     nil)
    ((and (listp list)
          (consp (car list)))
     (append (hippie-namespace-list-flatten (car list)) (hippie-namespace-list-flatten (cdr list))))
    ((listp list)
     (cons (car list) (hippie-namespace-list-flatten (cdr list))))
    (t
     (list list))))

;; functions to find and manipulate terms and prefixes

(defun hippie-namespace-prefix-popular-p (prefix collection cutoff)
  "Return t if string PREFIX matches into COLLECTION above CUTOFF members."
  (>= (length (delq nil (mapcar #'(lambda (s) (string-prefix-p prefix s)) collection)))
      cutoff))

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

(defun hippie-namespace-strip-prefix-matches (collection)
  "Remove trailing members of COLLECTION which share a prefix match.

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
  "Return a list strings representing all symbol definitions as determined by imenu."
  (ignore-errors
    (imenu--cleanup)
    (setq imenu--index-alist nil)
    (imenu--make-index-alist))
  (remove-if-not 'stringp (hippie-namespace-list-flatten (mapcar #'(lambda (item)
                                                                     (if (and item (imenu--subalist-p item))
                                                                         (cdr item) item)) imenu--index-alist))))
;;; generic namespace-finder plugin

(defun hippie-namespace-generic-plugin (&optional method)
  "This namespace-finder plugin works for all major modes supported by `imenu'.

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

;;; mode-specific namespace plugins
;;
;; Howto:
;;
;; Create a function called hippie-namespace-plugin-YOUR-FAVORITE-MODE.
;; The function should return a list of strings, in ascending order of
;; preference.
;;

(defalias 'hippie-namespace-plugin-emacs-lisp-mode 'hippie-namespace-plugin-lisp-mode)

(defun hippie-namespace-plugin-lisp-mode ()
  "Scan code for Lisp-specific namespace prefixes."
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

(defalias 'hippie-namespace-plugin-cperl-mode 'hippie-namespace-plugin-perl-mode)

(defun hippie-namespace-plugin-perl-mode ()
  "Scan code for Perl-specific namespace prefixes."
  (let ((namespaces nil))
    (goto-char (point-min))
    (while (re-search-forward "^[ \t]*use[ \t\n]+\\([A-Z][^ \t\n;]+\\)" nil t)
      (push (concat (match-string-no-properties 1) "::") namespaces))
    (goto-char (point-min))
    (while (re-search-forward "^[ \t]*package[ \t\n]+\\([^ \t\n;]+\\)" nil t)
      (push (concat (match-string-no-properties 1) "::") namespaces))
    namespaces))

(defun hippie-namespace-plugin-python-mode ()
  "Scan code for Python-specific namespace prefixes."
  (let ((namespaces nil))
    (goto-char (point-min))
    (while (re-search-forward "^[ \t]*from[ \t]+\\([^ \t\n]+\\)[ \t]+import" nil t)
      (push (concat (match-string-no-properties 1) ".") namespaces))
    (goto-char (point-min))
    (while (re-search-forward "^[ \t]*import[ \t]+\\([^ \t\n;]+\\)" nil t)
      (push (concat (match-string-no-properties 1) ".") namespaces))
    namespaces))

;; todo better to read the module names from the content of files loaded by "require"
(defun hippie-namespace-plugin-ruby-mode ()
  "Scan code for Ruby-specific namespace prefixes."
  (let ((namespaces nil))
    (goto-char (point-min))
    (while (re-search-forward "^[ \t]*module[ \t\n]+\\([^ \t\n]+\\)" nil t)
      (push (concat (match-string-no-properties 1) ".") namespaces))
    namespaces))

(defun hippie-namespace-plugin-c++-mode ()
  "Scan code for C++-specific namespace prefixes."
  (let ((namespaces nil))
    (goto-char (point-min))
    (while (re-search-forward "^[ \t]*namespace[ \t\n]+\\([^ \t\n]+\\)" nil t)
      (push (concat (match-string-no-properties 1) "::") namespaces))
    namespaces))

;; not sure how good this is. PHP code need not start at beginning-of-line, for one
(defun hippie-namespace-plugin-php-mode ()
  "Scan code for PHP-specific namespace prefixes."
  (let ((namespaces nil))
    (goto-char (point-min))
    (while (re-search-forward "^[ \t]*namespace[ \t\n]+\\([^ \t\n;]+\\)" nil t)
      (push (concat "\\" (match-string-no-properties 1) "\\") namespaces))
    (goto-char (point-min))
    (while (re-search-forward "^[ \t]*use[ \t\n]+\\([^ \t\n;]+\\)\\(?:[ \t\n]+as[ \t\n]+\\([^ \t\n;]+\\)\\)?" nil t)
      (push (concat "\\" (or (match-string-no-properties 1) (match-string-no-properties 2)) "\\") namespaces))
    namespaces))

(defun hippie-namespace-plugin-java-mode ()
  "Scan code for Java-specific namespace prefixes."
  (let ((namespaces nil))
    (goto-char (point-min))
    (while (re-search-forward "^[ \t]*import[ \t\n]+\\(?:[^ \t\n;]+\\)\\.\\([^ \.\t\n;]+\\)" nil t)
      (push (concat (match-string-no-properties 1) ".") namespaces))
    (goto-char (point-min))
    (while (re-search-forward "^[ \t]*package[ \t\n]+\\([^ \t\n;]+\\)" nil t)
      (push (concat (match-string-no-properties 1) ".") namespaces))
    namespaces))

;; todo import() with parens is missing here
(defun hippie-namespace-plugin-go-mode ()
  "Scan code for Go-specific namespace prefixes."
  (let ((namespaces nil))
    (goto-char (point-min))
    (while (re-search-forward "^[ \t]*import[ \t\n]+\"\\(?:[^ \t\n;]+\\)/\\([^ /\t\n;]+\\)\"" nil t)
      (push (concat (match-string-no-properties 1) ".") namespaces))
    (goto-char (point-min))
    (while (re-search-forward "^[ \t]*package[ \t\n]+\\([^ \t\n;]+\\)" nil t)
      (push (concat (match-string-no-properties 1) ".") namespaces))
    namespaces))

;;; populating the computed namespace list

;;;###autoload
(defun hippie-namespace-populate-list (&optional force)
  "Populate `hippie-namespace-computed-list' from buffer contents.

When optional FORCE is set, repopulate even if
`hippie-namespace-computed-list' is already set."

  (when (or force (null hippie-namespace-computed-list))
    (setq hippie-namespace-computed-list (append hippie-namespace-local-list hippie-namespace-manual-list))

    (let ((fn (intern-soft (concat "hippie-namespace-plugin-" (symbol-name major-mode)))))
      (when (fboundp fn)
        (save-excursion
          (save-restriction
            (save-match-data
              (callf2 append (funcall fn) hippie-namespace-computed-list)))))

      (setq hippie-namespace-computed-list (append (hippie-namespace-generic-plugin 'imenu)
                                                   hippie-namespace-computed-list))

      (when hippie-namespace-full-text-search
        (setq hippie-namespace-computed-list (append (hippie-namespace-generic-plugin 'fulltext)
                                                     hippie-namespace-computed-list)))

      (callf reverse hippie-namespace-computed-list)          ; now they are in order of priority
      (callf hippie-namespace-strip-prefix-matches hippie-namespace-computed-list)
      (delete-dups hippie-namespace-computed-list)

      (when (and hippie-namespace-max-elements
                 (> (length hippie-namespace-computed-list) hippie-namespace-max-elements))
        (setf (nthcdr hippie-namespace-max-elements hippie-namespace-computed-list) nil)))))

;;; expand-region integration

;;;###autoload
(defun hippie-namespace-mark-symbol-portion ()
  "Mark the namespace or non-namespace portion of a symbol under the point.

Intended for use with `expand-region' as an element of
`er/try-expand-list'.

If the point is in the namespace or non-namespace portion of
a symbol, mark only that portion of the symbol.

If the point is in a symbol which does not match a namespace,
there is no effect."
  (interactive)
  (when (and (boundp 'hippie-namespace-mode)
             hippie-namespace-mode
             (boundp 'hippie-namespace-computed-list)
             (> (length hippie-namespace-computed-list) 0))
    (let ((orig-point (point)))
      (save-match-data
        (skip-syntax-backward "_w")
        (when (looking-at (regexp-opt hippie-namespace-computed-list))
          (set-mark (match-end 0))
          (if (> (match-end 0) orig-point)
              (goto-char (match-beginning 0))
            ;; else
            (skip-syntax-forward "_w")
            (exchange-point-and-mark)))))))

(when hippie-namespace-expand-region
  (eval-after-load "expand-region"
    '(progn
       ;; insert in front of 'er/mark-symbol
       (unless (memq 'hippie-namespace-mark-symbol-portion er/try-expand-list)
         (push 'hippie-namespace-mark-symbol-portion
               (nthcdr (or (position 'er/mark-symbol er/try-expand-list) 0)
                       er/try-expand-list))))))

;;; minor-mode setup

;;;###autoload
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

;;;###autoload
(define-globalized-minor-mode global-hippie-namespace-mode hippie-namespace-mode hippie-namespace-maybe-turn-on
  :group 'hippie-namespace)

;;; interactive commands

;;;###autoload
(defun hippie-namespace-reload (arg)
  "Force a refresh of `hippie-namespace-computed-list'.

`hippie-namespace-computed-list' is used by `try-expand-namespace'.

With prefix ARG, also wipe `hippie-namespace-manual-list'."
  (interactive "P")
  (when arg
    (setq hippie-namespace-manual-list nil))
  (hippie-namespace-populate-list 'force)
  (when (hippie-namespace-called-interactively-p 'interactive)
    (message "%s namespace string%s available." (length hippie-namespace-computed-list) (if (= 1 (length hippie-namespace-computed-list)) "" "s"))))

;;;###autoload
(defun hippie-namespace-add (namespace)
  "Manually add NAMESPACE to the list available to `try-expand-namespace'.

Modifies `hippie-namespace-manual-list', and refreshes by running
`hippie-namespace-populate-list'."
  (interactive "sNamespace to add: ")
  (unless (string-match-p "\\`\\(?:\\sw\\|\\s_\\)+\\'" namespace)
    (error "Bad namespace"))
  (add-to-list 'hippie-namespace-manual-list namespace t)
  (hippie-namespace-populate-list 'force)
  (when (hippie-namespace-called-interactively-p 'interactive)
    (message "%s namespace string%s available." (length hippie-namespace-computed-list) (if (= 1 (length hippie-namespace-computed-list)) "" "s"))))

(provide 'hippie-namespace)

;;
;; Emacs
;;
;; Local Variables:
;; indent-tabs-mode: nil
;; mangle-whitespace: t
;; require-final-newline: t
;; coding: utf-8
;; byte-compile-warnings: (not cl-functions redefine)
;; End:
;;
;; LocalWords: HippieNamespace dabbrev setf callf imenu fulltext
;; LocalWords: Howto hipn defgroup devel
;;

;;; hippie-namespace.el ends here
