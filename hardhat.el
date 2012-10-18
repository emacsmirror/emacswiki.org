;;; hardhat.el --- Protect against clobbering user-writable files
;;
;; Copyright (c) 2012 Roland Walker
;;
;; Author: Roland Walker <walker@pobox.com>
;; Homepage: http://github.com/rolandwalker/hardhat
;; URL: http://raw.github.com/rolandwalker/hardhat/master/hardhat.el
;; Version: 0.3.6
;; Last-Updated: 18 Oct 2012
;; EmacsWiki: Hardhat
;; Package-Requires: ((ignoramus "0.6.2"))
;; Keywords: convenience
;;
;; Simplified BSD License
;;
;;; Commentary:
;;
;; Quickstart
;;
;;     (require 'hardhat)
;;
;;     (global-hardhat-mode 1)
;;
;;     ;; now you are protected from editing:
;;     ;;
;;     ;;     .git/index
;;     ;;     ~/.emacs.d/elpa/hardhat-0.1.0/hardhat.el
;;     ;;     ~/.emacs~
;;     ;;
;;     ;; and many others
;;
;; Explanation
;;
;; A recent unwholesome trend is for package managers to install files
;; in locations such as ~/.cabal/, ~/.rvm/, or ~/.emacs.d/elpa/.  It
;; is rarely meant for such files to be edited; doing so can cause
;; data loss in some circumstances.
;;
;; In addition, many user-writable files created in the course of
;; ordinary work should never be altered by a text editor, eg the
;; database stored in a .git directory.
;;
;; Hardhat.el provides an extra layer of protection in your work.  If
;; you visit a file which looks unsafe to edit, Emacs will make the
;; buffer read-only -- even when the underlying file is writable.
;;
;; The read-only protection can be turned off for a buffer by the
;; usual methods, or by toggling off buffer-local hardhat-mode via
;; the lighter menu or
;;
;;     M-x hardhat-mode RET
;;
;; If a buffer is not visiting a file, hardhat-mode has no effect.
;; If the visited file is not writable by the user, hardhat-mode
;; has no effect.
;;
;; To use hardhat, place the hardhat.el library somewhere
;; Emacs can find it, and add the following to your ~/.emacs file:
;;
;;     (require 'hardhat)
;;     (global-hardhat-mode 1)
;;
;; To inquire as to why hardhat has set or unset protection in
;; a buffer, the following interactive command is provided
;;
;;     `hardhat-status'
;;
;; but not bound to any key.
;;
;; See Also
;;
;;     M-x customize-group RET hardhat RET
;;
;; Notes
;;
;;     Hardhat-mode takes no action until the user attempts an
;;     interactive command in a buffer.  This is (out of an abundance
;;     of caution) for compatibility: an Emacs Lisp library may freely
;;     open and write to a file protected by hardhat-mode, so long as
;;     it is done programatically.
;;
;;     For any of the options settable in customize, rules making
;;     buffers "editable" override rules making buffers "protected".
;;
;;     A Boolean file-local variable `hardhat-protect' is provided.
;;     When `hardhat-protect' is set, no other rules are consulted.
;;
;;     Regular-expression matches are case-insensitive.  A case-
;;     sensitive test can be implemented by adding custom function
;;     to eg `hardhat-buffer-protected-functions'.
;;
;; Compatibility and Requirements
;;
;;     GNU Emacs version 24.3-devel     : yes, at the time of writing
;;     GNU Emacs version 24.1 & 24.2    : yes
;;     GNU Emacs version 23.3           : yes
;;     GNU Emacs version 22.3 and lower : no
;;
;;     Uses if present: ignoramus.el
;;
;; Prior art
;;
;;     do-not-edit.el
;;     http://user42.tuxfamily.org/do-not-edit/index.html
;;     Kevin Ryde <user42@zip.com.au>
;;
;; Bugs
;;
;;     This is alpha-quality software; bugs are expected.
;;
;;     More exceptions are certainly needed in `hardhat-fullpath-editable-regexps'
;;
;; TODO
;;
;;     include more build-dirs for python and ruby
;;
;;; License
;;
;; Simplified BSD License:
;;
;; Redistribution and use in source and binary forms, with or
;; without modification, are permitted provided that the following
;; conditions are met:
;;
;;    1. Redistributions of source code must retain the above
;;       copyright notice, this list of conditions and the following
;;       disclaimer.
;;
;;    2. Redistributions in binary form must reproduce the above
;;       copyright notice, this list of conditions and the following
;;       disclaimer in the documentation and/or other materials
;;       provided with the distribution.
;;
;; This software is provided by Roland Walker "AS IS" and any express
;; or implied warranties, including, but not limited to, the implied
;; warranties of merchantability and fitness for a particular
;; purpose are disclaimed.  In no event shall Roland Walker or
;; contributors be liable for any direct, indirect, incidental,
;; special, exemplary, or consequential damages (including, but not
;; limited to, procurement of substitute goods or services; loss of
;; use, data, or profits; or business interruption) however caused
;; and on any theory of liability, whether in contract, strict
;; liability, or tort (including negligence or otherwise) arising in
;; any way out of the use of this software, even if advised of the
;; possibility of such damage.
;;
;; The views and conclusions contained in the software and
;; documentation are those of the authors and should not be
;; interpreted as representing official policies, either expressed
;; or implied, of Roland Walker.
;;
;;; Code:
;;

;;; requires

(eval-when-compile
  (defvar ert--running-tests))

;; for callf, setf, callf2, assert, remove-if-not
(require 'cl)

(require 'ignoramus nil t)

;;; set-function for customize

(defvar hardhat-computed-regexps (make-hash-table :test 'eq)
  "Per-mode cache for regexps computed from defcustom settings.")

;;;###autoload
(defun hardhat-customize-set-regexp (symbol value)
  "Set function which clears the computed regexp cache.

SYMBOL and VALUE are passed to `custom-set-default'."
  (custom-set-default symbol value)
  (setq hardhat-computed-regexps (make-hash-table :test 'eq)))

;;; customizable variables

;;;###autoload
(defgroup hardhat nil
  "Protect against clobbering user-writable files."
  :version "0.3.6"
  :link '(emacs-commentary-link "hardhat")
  :prefix "hardhat-"
  :group 'convenience)

(defcustom hardhat-mode-lighter " hhat"
  "This string appears in the mode-line when `hardhat-mode' is active.

Set to nil or the empty string to disable the mode-line
lighter for `hardhat-mode'."
  :type 'string
  :risky t
  :group 'hardhat)

(defcustom hardhat-less-feedback nil
  "Give less echo area feedback."
  :type 'boolean
  :group 'hardhat)

(defcustom hardhat-bof-content-bound 250
  "How far from the start to search for regexps in content.

See `hardhat-bof-content-protected-regexps' and `hardhat-bof-content-editable-regexps'."
  :type 'integer
  :group 'hardhat)

(defcustom hardhat-eof-content-bound 250
  "How far from the end to search for regexps in content.

See `hardhat-eof-content-protected-regexps' and `hardhat-eof-content-editable-regexps'."
  :type 'integer
  :group 'hardhat)

;;;###autoload
(defgroup hardhat-protect nil
  "Rules for activating `hardhat-mode' protection in a buffer."
  :group 'hardhat)

(defcustom hardhat-bof-content-protected-regexps '(
                                                   (emacs-lisp-mode . "\\`;;;;[^\n]*--- automatically extracted\\>")
                                                   (perl-mode       . "^# Changes made here will be lost when autosplit is run again\\>")
                                                   (cperl-mode      . "^# Changes made here will be lost when autosplit is run again\\>")
                                                   "\\<THIS IS A GENERATED FILE\\>"
                                                   "\\<automatically generated\\>"
                                                   "\\<generated automatically\\>"
                                                   "\\<do not \\(change\\|edit\\|modify\\)\\>"
                                                   "\\<don't \\(change\\|edit\\|modify\\) this file\\>"
                                                   "\\`;+ *Emacs Bookmark Format Version [0-9]"
                                                   "^;+ *-+ smex-history -+"
                                                   "^;+ *EIEIO PERSISTENT OBJECT\\>"
                                                   "^;+ *Tramp connection history\\>"
                                                   )
  "Protect buffer from editing if these patterns match near beginning-of-file.

Patterns can be specified on a per-mode basis using a cons cell
in the form \(mode . regexp\).  The customization interface can
arrange this for you automatically.

All patterns are case-insensitive."
  :set 'hardhat-customize-set-regexp
  :type '(repeat (choice (regexp     :tag "Regular expression")
                         (cons       :tag "Mode-specific regular expression"
                             (symbol :tag "Mode")
                             (regexp :tag "Mode-specific regular expression"))))
  :group 'hardhat-protect)

(defcustom hardhat-eof-content-protected-regexps '(
                                                   )
  "Protect buffer from editing if these patterns match near end-of-file.

Patterns can be specified on a per-mode basis using a cons cell
in the form \(mode . regexp\).  The customization interface can
arrange this for you automatically.

All patterns are case-insensitive."
  :set 'hardhat-customize-set-regexp
  :type '(repeat (choice (regexp     :tag "Regular expression")
                         (cons       :tag "Mode-specific regular expression"
                             (symbol :tag "Mode")
                             (regexp :tag "Mode-specific regular expression"))))
  :group 'hardhat-protect)

(defcustom hardhat-basename-protected-regexps '(
                                                "~\\'"
                                                "\\.lock\\'"
                                                "\\.ix\\'"
                                                "\\`test\\.out\\'"
                                                "-autoloads\\.el\\'"
                                                "\\`Desktop\\.ini\\'"
                                                "\\`META\\.yml\\'"
                                                "\\`MYMETA\\.yml\\'"
                                                "\\`TAGS\\'"
                                                "\\`Thumbs\\.db\\'"
                                                "\\`\\.dropbox\\'"
                                                "\\`\\.dropbox\\.cache\\'"
                                                "\\`\\.emacs\\.desktop\\'"
                                                "\\`\\.emacs\\.desktop\\.lock\\'"
                                                "\\.orig\\'"
                                                "\\.rej\\'"
                                                )
  "Protect buffer from editing if these patterns match filename (sans directory).

Patterns can be specified on a per-mode basis using a cons cell
in the form \(mode . regexp\).  The customization interface can
arrange this for you automatically.

All patterns are case-insensitive."
  :set 'hardhat-customize-set-regexp
  :type '(repeat (choice (regexp     :tag "Regular expression")
                         (cons       :tag "Mode-specific regular expression"
                             (symbol :tag "Mode")
                             (regexp :tag "Mode-specific regular expression"))))
  :group 'hardhat-protect)

(defcustom hardhat-fullpath-protected-regexps '(
                                                "~/\\.emacs\\.d/elpa/"
                                                "~/\\.cpan/"
                                                "~/\\.cabal/"
                                                "~/perl5/perlbrew/"
                                                "~/\\.npm/"
                                                "~/\\.virtualenv/"
                                                "~/\\.virthualenv/"
                                                "~/\\.rvm/"
                                                "/[._]build/"
                                                "/\\.bzr/"
                                                "/\\.coverage/"
                                                "/\\.git/"
                                                "/\\.hg/"
                                                "/\\.rspec/"
                                                "/\\.sass-cache/"
                                                "/\\.svn/"
                                                "/_MTN/"
                                                "/_darcs/"
                                                "/CVS/"
                                                "/pm_to_blib/"
                                                "/RCS/"
                                                "/SCCS/"
                                                "/blib/"
                                                "/test_output/"
                                                )
  "Protect buffer from editing if these patterns match into full path to file.

A leading \"~/\" expression will be expanded with the user's home
directory.  The path this pattern is tested against will be fully
expanded.

Patterns can be specified on a per-mode basis using a cons cell
in the form \(mode . regexp\).  The customization interface can
arrange this for you automatically.

All patterns are case-insensitive."
  :set 'hardhat-customize-set-regexp
  :type '(repeat (choice (regexp     :tag "Regular expression")
                         (cons       :tag "Mode-specific regular expression"
                             (symbol :tag "Mode")
                             (regexp :tag "Mode-specific regular expression"))))
  :group 'hardhat-protect)

(defcustom hardhat-buffer-protected-functions '(
                                                hardhat-protected-by-ignoramus
                                                hardhat-protected-osx-homebrew
                                                (perl-mode  . hardhat-protected-by-perl-semantic-eof)
                                                (cperl-mode . hardhat-protected-by-perl-semantic-eof)
                                                )
  "Protect buffer from editing if any listed function evaluates non-nil.

Each function should take two arguments in the form \(buffer
&optional file\).

Functions can be specified on a per-mode basis using a cons cell
in the form \(mode . function\).  The customization interface can
arrange this for you automatically.

Set this value to nil to disable."
  :type '(repeat (choice (function   :tag "Function")
                         (cons       :tag "Mode-specific function"
                             (symbol :tag "Mode")
                             (regexp :tag "Mode-specific function"))))
  :group 'hardhat-protect)

;;;###autoload
(defgroup hardhat-editable nil
  "Rules for de-activating `hardhat-mode', making text editable."
  :group 'hardhat)

(defcustom hardhat-bof-content-editable-regexps '(
                                                  (sh-mode . "\\<THIS IS A GENERATED FILE\\\\") ; recommended by do-not-edit.el
                                                  )
  "Allow editing in a buffer if these patterns match near beginning-of-file.

\"Editable\" tests override \"protected\" tests.

Patterns can be specified on a per-mode basis using a cons cell
in the form \(mode . regexp\).  The customization interface can
arrange this for you automatically.

All patterns are case-insensitive."
  :set 'hardhat-customize-set-regexp
  :type '(repeat (choice (regexp     :tag "Regular expression")
                         (cons       :tag "Mode-specific regular expression"
                             (symbol :tag "Mode")
                             (regexp :tag "Mode-specific regular expression"))))
  :group 'hardhat-editable)

(defcustom hardhat-eof-content-editable-regexps nil
  "Allow editing in a buffer if these patterns match near end-of-file.

\"Editable\" tests override \"protected\" tests.

Patterns can be specified on a per-mode basis using a cons cell
in the form \(mode . regexp\).  The customization interface can
arrange this for you automatically.

All patterns are case-insensitive."
  :set 'hardhat-customize-set-regexp
  :type '(repeat (choice (regexp     :tag "Regular expression")
                         (cons       :tag "Mode-specific regular expression"
                             (symbol :tag "Mode")
                             (regexp :tag "Mode-specific regular expression"))))
  :group 'hardhat-editable)

(defcustom hardhat-basename-editable-regexps '(
                                               "\\`bzr_log\\.[[:alnum:]]+"            ; bzr
                                               "\\`hg-editor-[[:alnum:]]+\\.txt"      ; hg
                                               "\\`svn-commit\\.tmp\\'"               ; svn
                                               "\\`zshecl[0-9]+"                      ; zsh
                                               "\\`bash-fc-[0-9]+\\'"                 ; bash
                                               )
  "Allow editing in a buffer if these patterns match filename (sans directory).

\"Editable\" tests override \"protected\" tests.

Patterns can be specified on a per-mode basis using a cons cell
in the form \(mode . regexp\).  The customization interface can
arrange this for you automatically.

All patterns are case-insensitive."
  :set 'hardhat-customize-set-regexp
  :type '(repeat (choice (regexp     :tag "Regular expression")
                         (cons       :tag "Mode-specific regular expression"
                             (symbol :tag "Mode")
                             (regexp :tag "Mode-specific regular expression"))))
  :group 'hardhat-editable)

;; todo @@@ more editable exceptions needed here
(defcustom hardhat-fullpath-editable-regexps '(
                                               "~/\\.cpan/CPAN/MyConfig\\.pm\\'"
                                               "/\\.git/\\(?:COMMIT_EDITMSG\\|MERGE_MSG\\|SQUASH_MSG\\|rebase-merge/git-rebase-todo\\|description\\|hooks/\\|config\\)\\'"
                                               ;; "~/\\.cabal/"
                                               ;; "~/perl5/perlbrew/"
                                               ;; "~/\\.npm/"
                                               ;; "~/\\.virtualenv/"
                                               ;; "~/\\.virthualenv/"
                                               ;; "~/\\.rvm/"
                                               ;; "/\\.hg/"
                                               ;; "/\\.svn/"
                                               )
  "Allow editing in a buffer if these patterns match into full path to file.

\"Editable\" tests override \"protected\" tests.

A leading \"~/\" expression will be expanded with the user's home
directory.  The path this pattern is tested against will be fully
expanded.

Patterns can be specified on a per-mode basis using a cons cell
in the form \(mode . regexp\).  The customization interface can
arrange this for you automatically.

All patterns are case-insensitive."
  :set 'hardhat-customize-set-regexp
  :type '(repeat (choice (regexp     :tag "Regular expression")
                         (cons       :tag "Mode-specific regular expression"
                             (symbol :tag "Mode")
                             (regexp :tag "Mode-specific regular expression"))))
  :group 'hardhat-editable)

(defcustom hardhat-buffer-editable-functions '(
                                               (perl-mode  . hardhat-editable-by-perl-semantic-eof)
                                               (cperl-mode . hardhat-editable-by-perl-semantic-eof)
                                               )
  "Allow editing in a buffer if any listed function evaluates non-nil.

\"Editable\" tests override \"protected\" tests.

Functions can be specified on a per-mode basis using a cons cell
in the form \(mode . function\).  The customization interface can
arrange this for you automatically.

Each function should take two arguments in the form \(buffer
&optional file\).

Set this value to nil to disable."
  :type '(repeat (choice (function   :tag "Function")
                         (cons       :tag "Mode-specific function"
                             (symbol :tag "Mode")
                             (regexp :tag "Mode-specific function"))))
  :group 'hardhat-editable)

;;; variables

(defvar global-hardhat-mode nil
  "Mode variable for `global-hardhat-mode'.")
(defvar hardhat-mode nil
  "Mode variable for `hardhat-mode'.")
(defvar hardhat-protect :unset
  "User-settable file-local variable for `hardhat-mode'.")
(defvar hardhat-lighter-menu-mouse-button 1
  "Which mouse button invokes the modeline context menu.")
(defvar hardhat-lighter-keymap-property 'keymap
  "Which property sets the lighter keymap.")
(defvar hardhat-criteria '(function basename fullpath bof-content eof-content)
  "List of criteria for testing buffers.")
(defvar hardhat-directives '(editable protected)
  "Order-sensitive list of directives which may be applied to buffers.")
(defvar hardhat-reasons nil
  "Buffer-local results from testing hardhat criteria.")
(defvar hardhat--respect-narrowing t
  "Temporarily bound to control narrowing.")
(make-variable-buffer-local 'hardhat-mode)
(make-variable-buffer-local 'hardhat-protect)
(make-variable-buffer-local 'hardhat-reasons)

;;;###autoload
(put 'hardhat-protect 'safe-local-variable 'booleanp)

;;; lighter

(defvar hardhat-lighter-map  (let ((map (make-sparse-keymap))
                                   (menu-map (make-sparse-keymap "Hardhat")))
                               (define-key menu-map [customize]                       '(menu-item "Customize" (lambda (e) (interactive "e") (customize-group 'hardhat))))
                               (define-key menu-map [separator-1]                     '(menu-item "--"))
                               (define-key menu-map [turn-off-hardhat-mode-globally]  '(menu-item "Turn Off Hardhat Mode Globally"     (lambda (e) (interactive "e")
                                                                                                                                         (global-hardhat-mode -1))))
                               (define-key menu-map [turn-off-hardhat-mode]           '(menu-item "Toggle Hardhat Mode Locally"        hardhat-mode))
                               (define-key menu-map [hardhat-status]                  '(menu-item "Get Hardhat Status for This Buffer" hardhat-status))
                               (define-key map (read-kbd-macro (format "<mode-line> <down-mouse-%s>" hardhat-lighter-menu-mouse-button)) menu-map)
                               map) "Keymap for the global `hardhat-mode' lighter.")

;;; macros

(defmacro hardhat-called-interactively-p (&optional kind)
  "A backward-compatible version of `called-interactively-p'.

Optional KIND is as documented at `called-interactively-p'
in GNU Emacs 24.1 or higher."
  (if (eq 0 (cdr (subr-arity (symbol-function 'called-interactively-p))))
      '(called-interactively-p)
    `(called-interactively-p ,kind)))

;;; utility functions

(defun hardhat--propertize-lighter ()
  "Add properties to the `hardhat-mode' lighter."
  (when (and (stringp hardhat-mode-lighter)
             (> (length hardhat-mode-lighter) 0))
    (callf propertize hardhat-mode-lighter
                      hardhat-lighter-keymap-property hardhat-lighter-map
                     'help-echo (format "Hardhat: mouse-%s menu" hardhat-lighter-menu-mouse-button))))

(defun hardhat-compute-regexps ()
  "Compute regexps from customizable values."
  (unless (gethash major-mode hardhat-computed-regexps)
    (puthash major-mode (make-hash-table :test 'eq) hardhat-computed-regexps)
    (save-match-data
      ;; read variables
      (dolist (directive hardhat-directives)
        (puthash directive (make-hash-table :test 'eq)
                 (gethash major-mode hardhat-computed-regexps))
        (dolist (criterion (remq 'function hardhat-criteria))
          (let ((cust-sym (intern (format "hardhat-%s-%s-regexps" criterion directive)))
                (computed nil))
            (dolist (elt (symbol-value cust-sym))
              (cond
                ((null elt)
                 t)
                ((stringp elt)
                 (push elt computed))
                ((and (consp elt)
                      (stringp (cdr elt))
                      (symbolp (car elt)))
                 (when (eq major-mode (car elt))
                   (push (cdr elt) computed)))
                (t
                 (error "Bad element %s in %s" elt cust-sym)))
              ;; tilde expansion
              (when (and (eq criterion 'fullpath)
                         (string-match "\\`\\(~[^~/]*/\\)" (car computed)))
                (setf (car computed)
                      (replace-match
                       (regexp-quote (expand-file-name (match-string 1 (car computed))))
                       nil
                       'literal
                       (car computed)
                       1))))
            ;; sanity check
            (when computed
              (callf2 remove-if-not 'stringp computed)
              (callf2 remove-if-not #'(lambda (str-val)
                                        (> (length str-val) 0)) computed))
            ;; construct and store regexp
            (when computed
              (setq computed (format "\\(%s\\)" (mapconcat 'identity computed "\\|")))
              (puthash criterion computed (gethash directive (gethash major-mode hardhat-computed-regexps))))))))))

;;; functions which may de/activate protection

(defun hardhat-dispatch-generic-check (buf directive criterion &optional file basename)
  "Return non-nil if hardhat check is true for BUF.

DIRECTIVE may be a symbol listed in `hardhat-directives'.

CRITERION may be a symbol listed in `hardhat-criteria'.

Optional FILE and BASENAME override the file and basename
associated with BUF for the purpose of optimization."
  (when (and (or (not noninteractive) ert--running-tests)
             (bufferp buf)
             (or file basename (buffer-file-name buf)))
    (assert (memq directive hardhat-directives) nil "Bad DIRECTIVE")
    (assert (memq criterion hardhat-criteria)   nil "Bad CRITERION")
    (with-current-buffer buf
      (cond
        ((eq criterion 'function)
         (catch 'hit
           (dolist (cell (symbol-value (intern (format "hardhat-buffer-%s-functions" directive))))
             (let ((test (cond
                           ((symbolp cell)
                            cell)
                           ((and (consp cell)
                                 (eq major-mode (car cell)))
                            (cdr cell)))))
               (when (and (fboundp test)
                          (with-demoted-errors (funcall test buf file)))
                 (throw 'hit test))))))
        (t
         (hardhat-compute-regexps)
         (let ((case-fold-search t)
               (regexp (ignore-errors
                         (gethash criterion
                            (gethash directive
                               (gethash major-mode hardhat-computed-regexps))))))
           (when (and (stringp regexp)
                      (> (length regexp) 0))
             (save-excursion
               (save-restriction
                 (save-match-data
                   (unless hardhat--respect-narrowing
                     (widen))
                   (cond
                     ((eq criterion 'basename)
                      (callf or basename (file-name-nondirectory (file-truename (buffer-file-name buf))))
                      (when (string-match regexp basename)
                        (match-string 1 basename)))
                     ((eq criterion 'fullpath)
                      (callf or file (file-truename (buffer-file-name buf)))
                      (when (string-match regexp file)
                        (match-string 1 file)))
                     ((eq criterion 'bof-content)
                      (goto-char (point-min))
                      (when (re-search-forward regexp hardhat-bof-content-bound t)
                        (match-string-no-properties 1)))
                     ((eq criterion 'eof-content)
                      (goto-char (point-max))
                      (when (re-search-backward regexp (- (point-max) hardhat-eof-content-bound) t)
                        (match-string-no-properties 1)))
                     (t
                      (error "Bad DIRECTIVE")))))))))))))

(defun hardhat-protected-by-ignoramus (buf &optional file)
  "Return non-nil if `ignoramus-boring-p' is true for file associated with BUF.

Optional FILE overrides the file associated with BUF for the
purpose of optimization.

If ignoramus.el is not present, fails silently.

This function may be used as a member of `hardhat-buffer-protected-functions'."
  (when (and (fboundp 'ignoramus-boring-p)
             (bufferp buf))
    (callf or file (file-truename (expand-file-name (buffer-file-name buf))))
    (when (ignoramus-boring-p file)
      buf)))

(defun hardhat-dispatch-perl-check (buf directive &optional file)
  "Dispatch check for perl semantic eof functions on BUF.

DIRECTIVE may be a symbol listed in `hardhat-directives'.

Optional FILE overrides the file associated with BUF for the
purpose of optimization.

See `hardhat-editable-by-perl-semantic-eof' and
`hardhat-protected-by-perl-semantic-eof'"
  (with-current-buffer buf
    (when (and (or (not noninteractive) ert--running-tests)
               (bufferp buf)
               (or file (buffer-file-name buf)))
      (save-excursion
        (save-restriction
          (save-match-data
            (widen)
            (goto-char (point-min))
            (when (let ((case-fold-search nil))
                    (search-forward-regexp "^__\\(?:DATA\\|END\\)__$" nil t))
              (narrow-to-region (point-min) (line-beginning-position))
              (let ((hardhat--respect-narrowing t))
                (hardhat-dispatch-generic-check buf directive 'eof-content file)))))))))

(defun hardhat-protected-by-perl-semantic-eof (buf &optional file)
  "Return non-nil if protection regexps match near end of code in BUF.

`hardhat-eof-content-protected-regexps' are tested above a
__DATA__ or __END__ marker, which marks the end of code
in a Perl file.

Optional FILE overrides the file associated with BUF for the
purpose of optimization.

This function may be used as a member of `hardhat-buffer-protected-functions'."
  (hardhat-dispatch-perl-check buf 'protected file))

(defun hardhat-editable-by-perl-semantic-eof (buf &optional file)
  "Return non-nil if editable regexps match near end of code in BUF.

`hardhat-eof-content-editable-regexps' are tested above a
__DATA__ or __END__ marker, which marks the end of code
in a Perl file.

Optional FILE overrides the file associated with BUF for the
purpose of optimization.

This function may be used as a member of `hardhat-buffer-editable-functions'."
  (hardhat-dispatch-perl-check buf 'editable file))

(defun hardhat-protected-osx-homebrew (buf &optional file)
  "Return non-nil if BUF is visiting a Homebrew file on OS X.

The Homebrew project \(http://mxcl.github.com/homebrew/\)
encourages the installation of user-writable files under
/usr/local/.

Optional FILE overrides the file associated with BUF for the
purpose of optimization."
  (when (eq system-type 'darwin)
    (callf or file (file-truename (expand-file-name (buffer-file-name buf))))
    (when (and (string-match-p "\\`/usr/local/" file)
               (file-writable-p file))
      buf)))

(defun hardhat-buffer-included-p (buf)
  "Return BUF if `global-hardhat-mode' should enable `hardhat-mode' in BUF."
  (when (and (or (not noninteractive) ert--running-tests)
             (bufferp buf)
             (buffer-file-name buf)
             (not buffer-read-only)
             (not buffer-file-read-only))
    (let* ((file (file-truename (expand-file-name (buffer-file-name buf))))
           (basename (file-name-nondirectory file))
           (answer nil))
      (when (fboundp 'ignoramus-compute-common-regexps)
        (ignoramus-compute-common-regexps))
      (unless (eq 'file-local-variable (cadr hardhat-reasons))
        (setq hardhat-reasons (catch 'hardhat
                                (dolist (directive hardhat-directives)
                                  (dolist (criterion hardhat-criteria)
                                    (when (setq answer (hardhat-dispatch-generic-check buf directive criterion file basename))
                                      (throw 'hardhat (list directive criterion answer)))))))
        (when (eq 'protected (car hardhat-reasons))
          buf)))))

;;; minor-mode definition

;;;###autoload
(define-minor-mode hardhat-mode
  "Turn on `hardhat-mode'.

When called interactively with no prefix argument this command
toggles the mode.  With a prefix argument, it enables the mode
if the argument is positive and otherwise disables the mode.

When called from Lisp, this command enables the mode if the
argument is omitted or nil, and toggles the mode if the argument
is 'toggle."
  :group 'hardhat
  (cond
    (hardhat-mode
     (cond
       ((and (or (not noninteractive) ert--running-tests)
             (buffer-file-name (current-buffer))
             (not buffer-read-only)
             (not buffer-file-read-only))
        (when (and (stringp hardhat-mode-lighter)
                   (not (local-variable-p 'hardhat-mode-lighter)))
          (make-local-variable 'hardhat-mode-lighter)
          (callf concat hardhat-mode-lighter "[on]")
          (hardhat--propertize-lighter))
        (add-hook 'pre-command-hook 'hardhat-local-hook nil t)
        (when (hardhat-called-interactively-p 'interactive)
          (setq hardhat-reasons (list 'protected 'user-interactive 'hardhat-mode-toggled))
          (when (not hardhat-less-feedback)
            (message "hardhat-mode enabled"))))
       (t
        (when (and (hardhat-called-interactively-p 'interactive)
                   (not hardhat-less-feedback))
          (message "hardhat-mode cannot be enabled in this buffer"))
        (setq hardhat-mode nil))))
    (t
     (when (stringp (default-value 'hardhat-mode-lighter))
       (kill-local-variable 'hardhat-mode-lighter))
     (cond
       ((hardhat-called-interactively-p 'interactive)
        (setq hardhat-reasons (list 'editable 'user-interactive 'hardhat-mode-toggled)))
       ((eq (car hardhat-reasons) 'protected)
        (setq hardhat-reasons nil)))
     (remove-hook 'pre-command-hook 'hardhat-local-hook t)
     (unless buffer-file-read-only
       (setq buffer-read-only nil))
     (when (and (hardhat-called-interactively-p 'interactive)
                (not hardhat-less-feedback))
       (message "hardhat-mode disabled")))))

;;; global minor-mode definition

(defun hardhat-maybe-turn-on (&optional arg)
  "Activate `hardhat-mode' in a buffer if appropriate.

The buffer must be file-associated to be considered.  The pathname
of the associated file is tested according to

    `hardhat-basename-protected-regexps'
    `hardhat-fullpath-protected-regexps'

The content of the buffer is tested according to

    `hardhat-bof-content-protected-regexps'
    `hardhat-eof-content-protected-regexps'

In addition, the buffer is tested according to the functions in

    `hardhat-buffer-protected-functions'
    `hardhat-buffer-editable-functions'

If called with a negative ARG, deactivate `hardhat-mode' in the buffer."
  (callf or arg 1)
  (when (or (< arg 0)
            (hardhat-buffer-included-p (current-buffer)))
    (hardhat-mode arg)))

;; The global mode function is written by hand, avoiding the macro
;; `define-globalized-minor-mode', so as to confine checking to
;; specific hooks.  Otherwise checks would fire much more often.
;;;###autoload
(defun global-hardhat-mode (&optional arg)
  "Toggle Hardhat mode in all buffers.
With prefix ARG, enable Global-Hardhat mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil."
  (interactive "P")
  (cond
    ((hardhat-called-interactively-p 'interactive)
     (callf or arg (if global-hardhat-mode -1 1)))
    (t
     (callf or arg 1)))
  (callf prefix-numeric-value arg)
  (cond
    ((< arg 0)
     (remove-hook 'find-file-hook 'hardhat-global-hook)
     (remove-hook 'after-change-major-mode-hook 'hardhat-global-hook)
     (remove-hook 'hack-local-variables-hook 'hardhat-local-variables-hook)
     (setq global-hardhat-mode nil)
     (dolist (buf (remove-if-not #'(lambda (buf)
                                     (ignore-errors
                                       (buffer-local-value 'hardhat-mode buf)))
                                 (buffer-list)))
       (with-current-buffer buf
         (hardhat-mode -1)))
     (when (and (hardhat-called-interactively-p 'interactive)
                (not hardhat-less-feedback))
       (message "Global-Hardhat mode disabled")))
    (t
     (hardhat--propertize-lighter)
     (unless (assoc 'global-hardhat-mode minor-mode-alist)
       (push '(global-hardhat-mode hardhat-mode-lighter) minor-mode-alist))
     (dolist (buf (buffer-list))
       (with-current-buffer buf
         (hardhat-maybe-turn-on)))
     (add-hook 'find-file-hook 'hardhat-global-hook)
     (add-hook 'after-change-major-mode-hook 'hardhat-global-hook)
     (add-hook 'hack-local-variables-hook 'hardhat-local-variables-hook)
     (setq global-hardhat-mode t)
     (when (and (hardhat-called-interactively-p 'interactive)
                (not hardhat-less-feedback))
       (message "Global-Hardhat mode enabled")))))

;;; hooks

(defun hardhat-local-variables-hook (&rest args)
  "Hook to check a buffer for `hardhat-mode' protection based on file-locals.

The file-local variable `hardhat-protect' is tested."
  (when (and (or (not noninteractive) ert--running-tests)
             (buffer-file-name (current-buffer))
             (not buffer-read-only)
             (not buffer-file-read-only))
    (cond
      ((not (local-variable-p 'hardhat-protect))
       t)
      ((eq hardhat-protect t)
       (setq hardhat-reasons (list 'protected 'file-local-variable t))
       (hardhat-mode 1))
      ((eq hardhat-protect nil)
       (setq hardhat-reasons (list 'editable 'file-local-variable nil))
       (hardhat-mode -1)))))

(defun hardhat-local-hook (&rest args)
  "Hook to check a buffer for `hardhat-mode' protection.

ARGS are ignored."
  (remove-hook 'pre-command-hook 'hardhat-local-hook t)
  (when (and hardhat-mode
             (or (not noninteractive) ert--running-tests)
             (buffer-file-name (current-buffer))
             (not buffer-read-only)
             (not buffer-file-read-only))
    (setq buffer-read-only t)
    (unless hardhat-less-feedback
      (message "hardhat: protecting %s from edits because: %s test matched \"%s\""
               (current-buffer)
               (nth 1 hardhat-reasons)
               (nth 2 hardhat-reasons)))))

(defun hardhat-global-hook (&rest args)
  "Hook to check all buffers for `hardhat-mode' protection.

ARGS are ignored."
  (when global-hardhat-mode
    (with-demoted-errors
      (hardhat-maybe-turn-on))))

;;; interactive commands

;;;###autoload
(defun hardhat-status ()
  "Echo the `hardhat-mode' status of the current buffer."
  (interactive)
  (cond
    (hardhat-reasons
     (message "Hardhat marked this buffer %s, because: %s test matched \"%s\""
              (car hardhat-reasons)
              (nth 1 hardhat-reasons)
              (nth 2 hardhat-reasons)))
    ((and (not hardhat-reasons)
          hardhat-mode)
     (message "Bug: hardhat-mode is on but results are not available"))
    ((not hardhat-reasons)
     (message "Hardhat-mode did not mark this buffer"))
    ((not global-hardhat-mode)
     (message "Hardhat-mode is off globally"))
    (t
     (message "No status information available."))))

(provide 'hardhat)

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
;; LocalWords: Hardhat ARGS alist devel Ryde callf defcustom hhat
;; LocalWords: autosplit EIEIO smex virtualenv perlbrew virthualenv
;; LocalWords: setf cpan rspec darcs elpa
;;

;;; hardhat.el ends here
