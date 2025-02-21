;;; grep+.el --- Extensions to standard library `grep.el'.   -*- lexical-binding:nil -*-
;;
;; Filename: grep+.el
;; Description: Extensions to standard library `grep.el'.
;; Author: Drew Adams
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 2005-2025, Drew Adams, all rights reserved.
;; Created: Fri Dec 16 13:36:47 2005
;; Version: 0
;; Package-Requires: ()
;; Last-Updated: Thu Feb 20 17:00:22 2025 (-0800)
;;           By: dradams
;;     Update #: 768
;; URL: https://www.emacswiki.org/emacs/download/grep%2b.el
;; Doc URL: https://www.emacswiki.org/emacs/GrepPlus
;; Keywords: tools, processes, compile
;; Compatibility: GNU Emacs: 22.x, 23.x, 24.x, 25.x, 26.x, 27.x
;;
;; Features that might be required by this library:
;;
;;   `ansi-color', `avoid', `comint', `compile', `compile+',
;;   `compile-', `fit-frame', `frame-fns', `grep', `misc-fns',
;;   `regexp-opt', `ring', `thingatpt', `thingatpt+', `tool-bar'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  Extensions to standard library `grep.el':
;;
;;    1. Additional keys are bound here.
;;    2. Mouse-over is active on the entire hit line, not just on the
;;       file-name part.
;;    3. `grep' command provides a default search string in all cases,
;;       and that default value is better.
;;    4. Commands are provided to remove commented lines from `grep'
;;       output and toggle their automatic removal.
;;
;;  If you want the current `grep' buffer to be renamed automatically
;;  to reflect the particular `grep' command used, do this:
;;
;;  (add-hook 'grep-mode-hook 'grepp-rename-buffer-to-last-no-confirm)
;;
;;  Otherwise, you can use `r' to rename it on demand (command
;;  `grepp-rename-buffer-to-last').
;;
;;
;;  Put this in your initialization file (`~/.emacs'):
;;
;;    (require 'grep+)
;;
;;  Face suggestions:
;;
;;    `compilation-info-face':   Blue3' foreground,        no inherit
;;    `compilation-line-number': DarkGoldenrod foreground, no inherit
;;    `match':                   SkyBlue background,       no inherit
;;
;;  User options defined here:
;;
;;    `grepp-default-comment-line-regexp', `grepp-default-regexp-fn'.
;;
;;  Commands defined here:
;;
;;    `choose-grep-buffer', `grepp-choose-grep-buffer',
;;    `grepp-new-grep-buffer', `grepp-remove-comments',
;;    `grepp-rename-buffer-to-last', `grepp-toggle-comments',
;;    `new-grep-buffer', `remove-grep-comments',
;;    `toggle-grep-comments'.
;;
;;  Non-interactive functions defined here:
;;
;;    `grepp-buffers', `grepp-default-regexp-fn',
;;    `grepp-rename-buffer-to-last-no-confirm'.
;;
;;  Internal variables defined here:
;;
;;    `grepp-last-args'.
;;
;;
;;
;;  ***** NOTE: The following variables defined in `grep.el'
;;              have been REDEFINED HERE:
;;
;;  `grep-mode-font-lock-keywords', `grep-regexp-alist'
;;    - Mouse-over the whole line.
;;
;;
;;
;;  ***** NOTE: The following minor mode defined in `grep.el'
;;              has been REDEFINED HERE:
;;
;;  `grep-mode' - No change.  Redefined here so it uses modified value
;;                of `grep-regexp-alist'.
;;
;;
;;  ***** NOTE: The following functions defined in `grep.el'
;;              have been REDEFINED HERE:
;;
;;  `grep', `grep-default-command' - Use `grepp-default-regexp-fn'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2025/02/20 dadams
;;     compile+.el doesn't work for Emacs 28+.
;; 2021/01/03 dadams
;;     grep-default-command: Use grep-tag-default for Emacs 27.
;;     Don't bother to modify grep-tag-default for Emacs 26+.
;;     grep-mode-font-lock-keywords: Updated per vanilla Emacs 27.
;;     grep-mode: Use regexp-unmatchable for compilation-directory-matcher, for Emacs 27+.
;; 2020/12/06 dadams
;;     grep-regexp-alist: Put mouse-face on whole line for Emacs > 24 also.
;;     grep-mode-font-lock-keywords: Updated for Emacs 26.3.
;; 2018/09/21 dadams
;;     grepp-new-grep-buffer, grepp-choose-grep-buffer: Use pop-to-buffer-same-window, not switch-to-buffer.
;; 2016/09/23 dadams
;;     grep-regexp-alist: Also inappropriate for Emacs 24.
;; 2016/09/22 dadams
;;     grep-regexp-alist: Not needed for Emacs 25+.
;; 2016/02/12 dadams
;;     Added commands to Grep menu-bar menu.
;; 2016/02/11 dadams
;;     Added: grepp-rename-buffer-to-last, grepp-rename-buffer-to-last-no-confirm, grepp-last-args.
;;     Bind grepp-rename-buffer-to-last to r and R.
;;     grep: Save command args as grepp-last-args.
;; 2013/12/05 dadams
;;     grep-default-command: Made argument &optional, since can be called elsewhere.
;; 2013/07/03 dadams
;;     grepp-default-regexp-fn: Added arg TAG-DEFAULT (so it is computed only once).
;;     grep: Call grepp-default-regexp-fn only once, and pass value to grepp-default-regexp-fn.
;; 2012/08/21 dadams
;;     Call tap-put-thing-at-point-props after load thingatpt+.el.
;; 2012/08/18 dadams
;;     Invoke tap-define-aliases-wo-prefix if thingatpt+.el is loaded.
;; 2011/10/03 dadams
;;     Updated to fit latest Emacs 24.
;;       grep-default-command: Wrapped replacement of file pattern in condition-case.
;;       grep: Added read-shell-command from Emacs 24 code, but commented it out, for now.
;;       grep-regexp-alist: Removed initial regexp to match file and line.
;;                          Removed escape codes in regexps.  Instead, lambdas pick up match face.
;;       grep-mode-font-lock-keywords: Removed escape codes in regexps.  Emacs 24 code.
;;       grep-mode: Use Emacs 24 definition.
;; 2011/09/03 dadams
;;     Removed unneeded require of font-lock.el.
;; 2011/08/30 dadams
;;     grepp-default-regexp-fn: symbol-name-nearest-point -> non-nil-symbol-name-nearest-point.
;;                              Use functionp, not fboundp.
;;     grep: Test value of function, not option, grepp-default-regexp-fn, before funcall.
;;     Do not change any faces - just suggest.
;; 2011/02/15 dadams
;;     For Emacs 24+, do not set grep-hit-face to font-lock-keyword-face.
;;     grep-regexp-alist, grep-mode-font-lock-keywords, grep-mode: Updated for Emacs 24.
;; 2011/01/04 dadams
;;     Removed autoload cookies from non-interactive functions.  Added them for defalias commands.
;; 2007/12/04 dadams
;;     grep, grepp-default-regexp-fn: Changed single-quote to double-quote.
;; 2007/12/02 dadams
;;     grep and doc strings of grepp-default-regexp-fn (option and function):
;;       If active, nonempty region, use its (quoted) text as default regexp.
;;         Thx to Martin Nordholts for the suggestion.
;; 2006/12/11 dadams
;;     Added: grepp-toggle-comments.  Bound to M-;.
;; 2006/12/09 dadams
;;     Added: grep-mode-font-lock-keywords.  Needed for refontification (e.g. flush-lines).
;;     Added: grepp-remove-comments, grepp-default-comment-line-regexp.  Bound former to ;.
;; 2006/11/17 dadams
;;     grep-regexp-alist: Defined explicitly, not by modifying original.
;;                        Use nil for HIGHLIGHT part of structure.
;; 2006/11/14 dadams
;;     Added: grepp-buffers, grepp-choose-grep-buffer, grepp-new-grep-buffer.
;;            Bound: grepp-choose-grep-buffer, grepp-new-grep-buffer.
;;     Renamed: grep-default-regexp-fn to grepp-default-regexp-fn.
;; 2006/09/10 dadams
;;     Updated definition of grep-mode with latest from grep.el.
;; 2005/12/17 dadams
;;     Added: grep-default-regexp-fn, grep-default-command.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; ;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(when (< emacs-major-version 28)
  (require 'compile+ nil t)) ;; (no error if not found) - to pick up enhancements for grep too.
(require 'grep)

(when (and (require 'thingatpt+ nil t);; (no error if not found)
           (fboundp 'tap-put-thing-at-point-props)) ; >= 2012-08-21
  (tap-define-aliases-wo-prefix)
  (tap-put-thing-at-point-props))
 ;; non-nil-symbol-name-nearest-point

;;;;;;;;;;;;;;;;;;;;;;;


;;;###autoload
(defcustom grepp-default-comment-line-regexp ":[0-9]+: *;"
  "*Default regexp for a comment line, for use in `grepp-remove-comments'.
The default value matches lines that begin with a Lisp comment."
  :type 'string :group 'grep)

;;;###autoload
(defcustom grepp-default-regexp-fn (if (fboundp 'non-nil-symbol-name-nearest-point)
                                       'non-nil-symbol-name-nearest-point
                                     'word-at-point)
  "*Function of 0 args called to provide default search regexp to \\[grep].
Some reasonable choices are defined in `thingatpt+.el':
`word-nearest-point', `non-nil-symbol-name-nearest-point',
`region-or-non-nil-symbol-name-nearest-point', `sexp-nearest-point'.

This is ignored if Transient Mark mode is on and the region is active
and non-empty.  In that case, the quoted (\") region text is used as
the default regexp.

If `grepp-default-regexp-fn' is nil and no prefix arg is given to
`grep', then no defaulting is done.

Otherwise, if the value is not a function, then function
`grepp-default-regexp-fn' does the defaulting."
  :type '(choice
          (const :tag "No default search regexp (unless you use `C-u')" nil)
          (function :tag "Function of zero args to provide default search regexp"))
  :group 'grep)

(defun grepp-default-regexp-fn ()
  "*Function of 0 args called to provide default search regexp to \\[grep].
This is used only if both of the following are true:
- Transient Mark mode is off or the region is inactive or empty.
- The value of option `grepp-default-regexp-fn' is
  `grepp-default-regexp-fn'.

When this is used, the default regexp is provided by calling the
first of these that references a defined function:
  - variable `grepp-default-regexp-fn'
  - variable `find-tag-default-function'
  - the `find-tag-default-function' property of the `major-mode'
  - function `non-nil-symbol-name-nearest-point', if bound
  - function `grep-tag-default'"
  (cond ((functionp grepp-default-regexp-fn) grepp-default-regexp-fn)
        (find-tag-default-function)
        ((get major-mode 'find-tag-default-function))
        ((fboundp 'non-nil-symbol-name-nearest-point) 'non-nil-symbol-name-nearest-point)
        (t 'find-tag-default)))

(defvar grepp-last-args nil
  "String holding the arguments of the last `grep' command used.")


;;; REPLACE ORIGINAL in `grep.el'
;;;
;;; Added optional arg TAG-DEFAULT, passed from `grep'.  Needs to be optional, in case called elsewhere.
;;;
(defun grep-default-command (&optional tag-default)
  (let ((quoted-tag-def  (shell-quote-argument (or tag-default  (if (fboundp 'grep-tag-default) ; Emacs 27+
                                                                    (grep-tag-default)
                                                                  ""))))
	;; Regexp to match single shell arguments.
        (sh-arg-re     "\\(\\(?:\"\\(?:[^\"]\\|\\\\\"\\)+\"\\|'[^']+'\\|[^\"' \t\n]\\)+\\)")
        (grep-default  (or (car grep-history) grep-command)))
    ;; In the default command, find the arg that specifies the pattern.
    (when (or (string-match
               (concat "[^ ]+\\s +\\(?:-[^ ]+\\s +\\)*" sh-arg-re "\\(\\s +\\(\\S +\\)\\)?")
               grep-default)
              (string-match "\\(\\)\\'" grep-default)) ; If the string is not yet complete.
      ;; Maybe we will replace the pattern with the default tag.
      ;; But first, maybe replace the file name pattern.
      (condition-case nil
          (unless (or (not (stringp buffer-file-name))
                      (and (match-beginning 2)
                           (save-match-data
                             (string-match (wildcard-to-regexp (file-name-nondirectory
                                                                (match-string 3 grep-default)))
                                           (file-name-nondirectory buffer-file-name)))))
            (setq grep-default  (concat (substring grep-default 0 (match-beginning 2)) " *."
                                        (file-name-extension buffer-file-name))))
	;; In case wildcard-to-regexp gets an error from invalid data.
	(error nil))
      ;; Replace the pattern with the default tag.
      (replace-match quoted-tag-def t t grep-default 1))))


;;; REPLACE ORIGINAL in `grep.el'
;;;
;;; Use `grepp-default-regexp-fn' to define default search string (aka "tag default").
;;;
;;;###autoload
(defun grep (command-args &optional highlight-regexp)
  "Run `grep', with user-specified args, and collect output in a buffer.
The output goes to buffer `*grep*'.

COMMAND-ARGS are the user-specified arguments.
While `grep' runs asynchronously, you can use
\\[next-error] (M-x next-error), or \\<grep-mode-map>\\[compile-goto-error]
in output buffer `*grep*', to go to the lines where `grep' found matches.

This command uses a special history list for its COMMAND-ARGS, so you can
easily repeat a grep command.

The text (regexp) to find is defaulted as follows:

- If Transient Mark mode is on and the region is active and nonempty,
  then the double-quoted region text is used.  (If the region contains
  double-quotes (\"), then you will need to escape them by hand.)

- If option `grepp-default-regexp-fn' is a function, then it is called
  to return the default regexp.

- If `grepp-default-regexp-fn' is nil and no prefix arg is provided,
  then no default regexp is used.

If a prefix arg is provided, the default text is substituted into the
last grep command in the grep command history (or into `grep-command'
if that history list is empty).  That is, the same command options and
files to search are used as the last time.

If specified, optional second arg HIGHLIGHT-REGEXP is the regexp to
temporarily highlight in visited source lines."
  (interactive
   (progn
     (unless (and grep-command  (memq grep-use-null-device '(t nil))) (grep-compute-defaults))
     (let* ((tag-default  (funcall (grepp-default-regexp-fn)))
            (default-cmd  (grep-default-command tag-default)))
       (list
        (if nil ;;$$$$$$ UNCOMMENT if you prefer: (fboundp 'read-shell-command)
            (read-shell-command "grep <pattern> <files> :  "
                                (if current-prefix-arg
                                    default-cmd
                                  (concat
                                   grep-command
                                   (if (and transient-mark-mode mark-active
                                            (not (eq (region-beginning) (region-end))))
                                       ;; $$$$$ Would it be better to use `shell-quote-argument' on region?
                                       (concat "\"" (buffer-substring (region-beginning) (region-end)) "\"")
                                     tag-default)
                                   " "))
                                'grep-history
                                (if current-prefix-arg nil default-cmd))
          (read-from-minibuffer
           "grep <pattern> <files> :  "
           (if current-prefix-arg
               default-cmd
             (concat grep-command
                     (if (and transient-mark-mode mark-active
                              (not (eq (region-beginning) (region-end))))
                         ;; $$$$$ Would it be better to use `shell-quote-argument' on region?
                         (concat "\"" (buffer-substring (region-beginning) (region-end)) "\"")
                       tag-default)
                     " "))
           nil nil 'grep-history
           (if current-prefix-arg nil default-cmd)))))))
  (setq grepp-last-args  (substring command-args 4))
  ;; Setting process-setup-function makes exit-message-function work
  ;; even when async processes aren't supported.
  (compilation-start (if (and grep-use-null-device null-device)
                         (concat command-args " " null-device)
                       command-args)
                     'grep-mode nil highlight-regexp))


;;;###autoload
(defalias 'new-grep-buffer 'grepp-new-grep-buffer)
;;;###autoload
(defun grepp-new-grep-buffer ()
  "Rename current grep buffer and switch to new buffer *grep*.
Current buffer must be a grep buffer.  It is renamed to *grep*<N>."
  (interactive)
  (unless (string-match "\\*grep\\*" (buffer-name (current-buffer)))
    (error "Not in a grep buffer"))
  (rename-uniquely)
  (if (fboundp 'pop-to-buffer-same-window)
      (pop-to-buffer-same-window "*grep*")
    (switch-to-buffer "*grep*"))
  (grep-mode))


;;;###autoload
(defalias 'choose-grep-buffer 'grepp-choose-grep-buffer)
;;;###autoload
(defun grepp-choose-grep-buffer (buf)
  "Switch to a grep buffer."
  (interactive
   (let ((bufs  (grepp-buffers)))
     (unless bufs (error "No grep buffers"))
     (list (completing-read "Grep buffer: " bufs nil t nil nil
                            (and grep-last-buffer (buffer-name grep-last-buffer))))))
  (if (fboundp 'pop-to-buffer-same-window)
      (pop-to-buffer-same-window buf)
    (switch-to-buffer buf))
  (select-frame-set-input-focus (selected-frame))
  (grep-mode))

(defun grepp-buffers ()
  "List of names of grep buffers."
  (let ((bufs  ()))
    (dolist (buf (buffer-list))
      (when (string-match "\\*grep\\*" (buffer-name buf))
        (push (list (buffer-name buf)) bufs)))
    (nreverse bufs)))

;;;###autoload
(defun grepp-rename-buffer-to-last (&optional no-confirm)
  "Rename current `grep' buffer uniquely to reflect the command args."
  (interactive)
  (unless grepp-last-args (error "You must invoke Emacs command `grep' first"))
  (when (and (eq major-mode 'grep-mode)
             (or no-confirm  (y-or-n-p (format "Rename buffer to `*grep* %s'? " grepp-last-args))))
    (rename-buffer (concat "*grep* " grepp-last-args) 'UNIQUE)))

(defun grepp-rename-buffer-to-last-no-confirm ()
  "Rename current `grep' buffer uniquely to reflect the command args."
  (unless grepp-last-args (error "You must invoke Emacs command `grep' first"))
  (when (eq major-mode 'grep-mode) (rename-buffer (concat "*grep* " grepp-last-args) 'UNIQUE)))

;;;###autoload
(defalias 'remove-grep-comments 'grepp-remove-comments)
;;;###autoload
(defun grepp-remove-comments (&optional read-regexp-p)
  "Remove lines that are completely commented out.
With a prefix argument, you are prompted for the regexp used to match
 commented lines.  The default value is
 `grepp-default-comment-line-regexp'.
With no prefix argument, this default value is used as the regexp.

You can use command `grep-toggle-comments' to toggle automatic removal
of commented lines.

Note: This simply removes lines that begin with the regexp you
provide.  It does not, in general, remove multi-line comments.  Use it
to remove C++ comments that start with //, but not multi-line comments
between /* and */."
  (interactive "P")
  (when (eq major-mode 'grep-mode) ; Do nothing otherwise, so can use in `compilation-filter-hook'.
    (let ((inhibit-read-only  t)
          (regexp  (if read-regexp-p
                       (read-from-minibuffer "Comment regexp: " nil nil nil 'regexp-history
                                             grepp-default-comment-line-regexp)
                     grepp-default-comment-line-regexp)))
      (save-excursion (flush-lines regexp (point-min) (point-max))))))

;;;###autoload
(defalias 'toggle-grep-comments 'grepp-toggle-comments)
;;;###autoload
(defun grepp-toggle-comments ()
  "Toggle removal of commented lines in grep output."
  (interactive)
  (cond ((and (boundp 'compilation-filter-hook)
              (memq 'remove-grep-comments compilation-filter-hook))
         (remove-hook 'compilation-filter-hook 'remove-grep-comments)
         (when (consp grep-history) (grep (car grep-history)))
         (message "Automatic removal of commented lines is now OFF"))
        (t
         (add-hook 'compilation-filter-hook 'remove-grep-comments)
         (when (consp grep-history) (grep (car grep-history)))
         (message "Automatic removal of commented lines is now ON"))))


;; New bindings.
(define-key grep-mode-map "g" 'grep)
(define-key grep-mode-map "G" 'grep)
(define-key grep-mode-map "n" 'next-error-no-select)
(define-key grep-mode-map "N" 'next-error-no-select)
(define-key grep-mode-map "p" 'previous-error-no-select)
(define-key grep-mode-map "P" 'previous-error-no-select)
(define-key grep-mode-map "+" 'grepp-new-grep-buffer)
(define-key grep-mode-map "b" 'grepp-choose-grep-buffer)
(define-key grep-mode-map "B" 'grepp-choose-grep-buffer)
(define-key grep-mode-map "r" 'grepp-rename-buffer-to-last)
(define-key grep-mode-map "R" 'grepp-rename-buffer-to-last)
(define-key grep-mode-map ";" 'grepp-remove-comments)
(define-key grep-mode-map [(meta ?\;)] 'grepp-toggle-comments)

;; Menu-bar menu `Grep'.
(define-key-after grep-mode-map [menu-bar grep grepp-separator1] '("----") 'compilation-compile)
(define-key-after grep-mode-map [menu-bar grep grepp-new-grep-buffer]
  '(menu-item "Rename and Switch" grepp-new-grep-buffer
    :help "Rename current buffer to *grep*<N> and switch to new buffer *grep*.")
  'grepp-separator1)
(define-key-after grep-mode-map [menu-bar grep grepp-choose-grep-buffer]
  '(menu-item "Switch to Other Grep Buffer..." grepp-choose-grep-buffer)
  'grepp-new-grep-buffer)
(define-key-after grep-mode-map [menu-bar grep grepp-rename-buffer-to-last]
  '(menu-item "Rename Showing Args" grepp-rename-buffer-to-last
    :help "Rename current `grep' buffer uniquely to reflect the command args.")
  'grepp-choose-grep-buffer)
(define-key-after grep-mode-map [menu-bar grep grepp-separator2] '("----") 'grepp-rename-buffer-to-last)
(define-key-after grep-mode-map [menu-bar grep grepp-remove-comments]
  '(menu-item "Removed Commented Lines" grepp-remove-comments
    :help "Remove output lines that are completely commented out.")
  'grepp-separator2)
(define-key-after grep-mode-map [menu-bar grep grepp-toggle-comments]
  '(menu-item "Automatic Removal of Commented Lines" grepp-toggle-comments
    :help "Toggle removal of commented lines in grep output.")
  'grepp-remove-comments)


;;; REPLACE ORIGINAL `grep-regexp-alist' defined in `grep.el'.
;;;
;;; Use mouseover on whole line.  Same as original, except for this.
;;; Don't bother to even try for Emacs 26+, since it won't work with `xref' etc.
;;;
(unless (or (featurep 'grep+)  (> emacs-major-version 24))
  (if (= emacs-major-version 24)
      ;; Gross hack.  Append ".*" to regexps (but put it before a $ at end, if present).
      ;; If HIGHLIGHT is nil then the whole line will get `mouse-face'.
      (dolist (entry  grep-regexp-alist)
        (when (and (consp entry)  (stringp (car entry)))
          (let* ((regexp      (car entry))
                 (up-to-last  (substring regexp 0 -1))
                 (last        (substring regexp -1)))
            (setcar entry (if (string= "$" last) (concat up-to-last ".*$") (concat regexp ".*"))))))
    (setq grep-regexp-alist
          `(
            ;; Use as tight a regexp as possible to try to handle weird file names (with colons) as well as
            ;; possible.  E.g. use [1-9][0-9]* rather than [0-9]+ so as to accept ":034:" in file names.
            ;;
            ;; Appended `.*', to match whole line for `mouse-face'.
            ("^\\(.+?\\)\\(:[ \t]*\\)\\([1-9][0-9]*\\)\\2.*"
             1 3
             ;; Calculate column positions (col . end-col) of first grep match on a line
             ((lambda ()
                (when grep-highlight-matches
                  (let* ((beg   (match-end 0))
                         (end   (save-excursion (goto-char beg) (line-end-position)))
                         (mbeg  (text-property-any beg end 'font-lock-face ,(if (boundp 'grep-match-face)
                                                                                grep-match-face
                                                                              'match))))
                    (and mbeg (- mbeg beg)))))
              .
              (lambda ()
                (when grep-highlight-matches
                  (let* ((beg   (match-end 0))
                         (end   (save-excursion (goto-char beg) (line-end-position)))
                         (mbeg  (text-property-any beg end 'font-lock-face ,(if (boundp 'grep-match-face)
                                                                                grep-match-face
                                                                              'match)))
                         (mend  (and mbeg (next-single-property-change mbeg 'font-lock-face nil end))))
                    (and mend (- mend beg))))))
             nil
             nil)
            ("^Binary file \\(.+\\) matches$" 1 nil nil 0 1)))))


;;; REPLACE ORIGINAL `grep-mode-font-lock-keywords' defined in `grep.el'.
;;;
;;; Use mouseover on whole line.  Same as original, except for this.
;;;
(unless (featurep 'grep+)
  (setq grep-mode-font-lock-keywords
        (if (> emacs-major-version 23)
            '(;; Command output lines.
              ("^\\(.+?\\):\\([0-9]+\\):.*" (0 '(face nil mouse-face compilation-mouseover)))

              (": \\(.+\\): \\(?:Permission denied\\|No such \\(?:file or directory\\|device or \
address\\)\\)$"
               1 grep-error-face)
              ;; Remove match from `grep-regexp-alist' before fontifying.
              ("^Grep[/a-zA-Z]* started.*"
               (0 '(face nil compilation-message nil help-echo nil mouse-face nil) t))
              ("^Grep[/a-zA-Z]* finished with \\(?:\\(\\(?:[0-9]+ \\)?match\\(?:es\\)? \
found\\)\\|\\(no matches found\\)\\).*"
               (0 '(face nil compilation-message nil help-echo nil mouse-face nil) t)
               (1 compilation-info-face nil t)
               (2 compilation-warning-face nil t))
              ("^Grep[/a-zA-Z]* \\(exited abnormally\\|interrupt\\|killed\\|terminated\\)\\(?:.*with \
code \\([0-9]+\\)\\)?.*"
               (0 '(face nil compilation-message nil help-echo nil mouse-face nil) t)
               (1 grep-error-face)
               (2 grep-error-face nil t))
              ;; "filename-linenumber-" format is used for context lines in GNU grep,
              ;; "filename=linenumber=" for lines with function names in "git grep -p".
              ("^.+?\\([-=\0]\\)[0-9]+\\([-=]\\).*\n" (0 grep-context-face)
               (1 (and (eq (char-after (match-beginning 1)) ?\0)  `(face nil display ,(match-string 2)))))
              ("^find \\(\\. -type d .*\\\\)\\)" ; Hide excessive part of `rgrep' command
               (1 (if grep-find-abbreviate
                      grep-find-abbreviate-properties
                    '(face nil abbreviated-command t))))
              ("^grep \\( *--exclude.*--exclude[^ ]+\\)" ; Hide excessive part of `lgrep' command
               (1 (if grep-find-abbreviate
                      grep-find-abbreviate-properties
                    '(face nil abbreviated-command t)))))

          ;; Emacs 22, 23
          '(;; Command output lines.
            ("^\\(.+?\\):\\([0-9]+\\):.*" (0 '(face nil mouse-face compilation-mouseover)))
            (": \\(.+\\): \\(?:Permission denied\\|No such \\(?:file or directory\\|device or \
address\\)\\)$"
             1 grep-error-face)
            ;; Remove match from grep-regexp-alist before fontifying.
            ;; Set `compilation-message' and `message' to nil: Emacs before version 24 uses `message'.
            ("^Grep[/a-zA-Z]* started.*"
             (0 '(face nil compilation-message nil message nil help-echo nil mouse-face nil) t))
            ("^Grep[/a-zA-Z]* finished \\(?:(\\(matches found\\))\\|with \\(no matches found\\)\\).*"
             ;; Set `compilation-message' and `message' to nil: Emacs before version 24 uses `message'.
             (0 '(face nil compilation-message nil message nil help-echo nil mouse-face nil) t)
             (1 compilation-info-face nil t)
             (2 compilation-warning-face nil t))
            ("^Grep[/a-zA-Z]* \\(exited abnormally\\|interrupt\\|killed\\|terminated\\)\\(?:.*with \
code \\([0-9]+\\)\\)?.*"
             ;; Set `compilation-message' and `message' to nil: Emacs before version 24 uses `message'.
             (0 '(face nil compilation-message nil message nil help-echo nil mouse-face nil) t)
             (1 grep-error-face)
             (2 grep-error-face nil t))
            ("^.+?-[0-9]+-.*\n" (0 grep-context-face))
            ;; Highlight grep matches and delete markers
            ("\\(\033\\[01;31m\\)\\(.*?\\)\\(\033\\[[0-9]*m\\)"
             ;; Refontification does not work after the markers have been
             ;; deleted.  So we use the font-lock-face property here as Font
             ;; Lock does not clear that.
             (2 (list 'face nil 'font-lock-face grep-match-face))
             ((lambda (bound))
              (progn
                ;; Delete markers with `replace-match' because it updates
                ;; the match-data, whereas `delete-region' would render it obsolete.
                (when (> emacs-major-version 23) (syntax-ppss-flush-cache (match-beginning 0)))
                (replace-match "" t t nil 3)
                (replace-match "" t t nil 1))))
            ("\033\\[[0-9;]*[mK]"
             ;; Delete all remaining escape sequences
             ((lambda (bound))
              (when (> emacs-major-version 23) (syntax-ppss-flush-cache (match-beginning 0)))
              (replace-match "" t t)))))))


;;; REPLACE ORIGINAL `grep-mode' defined in `grep.el'.
;;; Re-create, so it uses the modified `grep-regexp-alist'.
;;; This definition SHOULD BE THE SAME AS THE ORIGINAL in `grep.el'.
;;;
(define-compilation-mode grep-mode "Grep"
  "Sets `grep-last-buffer' and `compilation-window-height'."
  (setq grep-last-buffer  (current-buffer))
  (when (boundp 'grep-mode-tool-bar-map)
    (set (make-local-variable 'tool-bar-map) grep-mode-tool-bar-map))
  (set (make-local-variable 'compilation-error-face) grep-hit-face)
  (set (make-local-variable 'compilation-error-regexp-alist) grep-regexp-alist)
  ;; compilation-directory-matcher can't be nil, so we set it to a regexp that
  ;; can never match.
  (set (make-local-variable 'compilation-directory-matcher) (if (boundp 'regexp-unmatchable)
                                                                (list regexp-unmatchable) ; Emacs 27+
                                                              '("\\`a\\`")))
  (set (make-local-variable 'compilation-process-setup-function) 'grep-process-setup)
  (set (make-local-variable 'compilation-disable-input) t)
  (set (make-local-variable 'compilation-error-screen-columns)
       grep-error-screen-columns)
  (when (fboundp 'grep-filter) (add-hook 'compilation-filter-hook 'grep-filter nil t)))

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'grep+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; grep+.el ends here
