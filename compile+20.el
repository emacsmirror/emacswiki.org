;;; compile+20.el --- Extensions to `compile.el'.
;;
;; Filename: compile+20.el
;; Description: Extensions to `compile.el'.
;; Author: Drew Adams
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 1999-2017, Drew Adams, all rights reserved.
;; Created: Fri Apr  2 16:55:16 1999
;; Version: 0
;; Last-Updated: Tue Feb 21 16:03:07 2017 (-0800)
;;           By: dradams
;;     Update #: 948
;; URL: https://www.emacswiki.org/emacs/download/compile%2b20.el
;; Doc URL: http://www.emacswiki.org/GrepPlus
;; Keywords: tools, processes
;; Compatibility: GNU Emacs 20.x, GNU Emacs 21.x
;;
;; Features that might be required by this library:
;;
;;   `apropos', `apropos+', `avoid', `compile', `compile-20',
;;   `easymenu', `fit-frame', `font-lock', `frame-fns', `help+20',
;;   `highlight', `info', `info+20', `menu-bar', `menu-bar+',
;;   `misc-cmds', `misc-fns', `naked', `second-sel', `strings',
;;   `thingatpt', `thingatpt+', `unaccent', `w32browser-dlgopen',
;;   `wid-edit', `wid-edit+', `widget'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Extensions to `compile.el'.
;;
;;  See also the companion file `compile-20.el'.
;;        `compile-20.el' should be loaded before `compile.el'.
;;        `compile+20.el' should be loaded after `compile.el'.
;;
;;  User options defined here:
;;
;;    `compile-buffer-mouse-face', `grep-case-insensitive-option',
;;    `grep-default-comment-line-regexp', `grep-default-regexp-fn',
;;    `grep-regexp-face'.
;;
;;  Commands defined here:
;;
;;    `choose-grep-buffer', `new-grep-buffer', `remove-grep-comments',
;;    `toggle-grep-comments'.
;;
;;  Non-interactive functions defined here:
;;
;;    `grep-buffers', `grep-default-regexp-fn',
;;    `select-frame-set-input-focus'.
;;
;;  Internal variables defined here: `grep-pattern'.
;;
;;
;;  ***** NOTE: The following functions defined in `compile.el'
;;              have been REDEFINED HERE:
;;
;;  `compilation-forget-errors' - Use `compile-buffer-mouse-face'.
;;  `compilation-goto-locus' - 1. Highlights `grep-pattern' at error.
;;                             2. Displays line #.
;;                             3. Raises frame.
;;  `compilation-mode' - Uses `fundamental-mode' instead of
;;                       `kill-all-local-variables'.
;;  `compilation-mode-font-lock-keywords' - Highlights `grep-pattern'
;;                                          in `*grep*' buffer.
;;  `compilation-next-error' - Calls `what-line' to display line #.
;;  `compile' - Resets `grep-pattern' from last grep.
;;  `compile-internal' - 1. Set `font-lock-fontified' to nil.
;;                       2. Don't let frame get shrunk.
;;  `compile-reinitialize-errors' - Use `compile-buffer-mouse-face',
;;                                  and put it on the whole line.
;;  `grep' - 1. Interactive spec uses `grep-default-regexp-fn'.
;;           2. Saves `grep-pattern' for highlighting.
;;
;;
;; Compile mode is now suitable only for specially formatted data:
;; That is, we do a `(put 'compile-mode 'mode-class 'special)'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2012/08/21 dadams
;;     Call tap-put-thing-at-point-props after load thingatpt+.el.
;; 2012/08/18 dadams
;;     Invoke tap-define-aliases-wo-prefix if thingatpt+.el is loaded.
;; 2011/12/19 dadams
;;     grep: Use buffer-substring-no-properties, not buffer-substring.
;;     compilation-goto-locus, compile-reinitialize-errors: Use line-(beginning|end)-position.
;; 2011/08/30 dadams
;;     grep-default-regexp-fn:
;;       symbol-name-nearest-point -> non-nil-symbol-name-nearest-point.
;;       Allow nil in defcustom.
;;       Use functionp, not fboundp.
;;     grep: Test value of function, not option, grep-default-regexp-fn, before funcall.
;; 2011/01/03 dadams
;;     Removed autoload cookies from non-interactive functions.
;;     Added some missing autoload cookies for commands.
;; 2007/12/04 dadams
;;     grep, grep-default-regexp-fn: Changed single-quote to double-quote.
;; 2007/12/02 dadams
;;     grep and doc strings of grepp-default-regexp-fn (option and function):
;;       If active, nonempty region, use its (quoted) text as default regexp.
;;         Thx to Martin Nordholts for the suggestion.
;; 2007/06/02 dadams
;;     Renamed: highlight-regexp-region to hlt-highlight-regexp-region.
;; 2007/03/15 dadams
;;     compilation-goto-locus: Raise frame, at end.
;; 2006/12/11 dadams
;;     Added: remove-grep-comments, , toggle-grep-comments, grep-default-comment-line-regexp.
;;     No longer (undefine-killer-commands compilation-mode-map (current-global-map)).
;;        So, no longer require misc-fns.el.
;; 2006/11/16 dadams
;;     choose-grep-buffer: Provide last grep buffer as default value.
;;     Added: select-frame-set-input-focus.
;; 2006/11/15 dadams
;;     Added: grep-buffers, choose-grep-buffer, new-grep-buffer.
;; 2006/09/20 dadams
;;     compilation-goto-locus: In-lined current-line.
;; 2006/09/19 dadams
;;     Make grep-pattern highlighting respect case-insensitivity option.
;;       Thx to Tamas Patrovics for the suggestion
;;       Added: grep-case-insensitive-option.
;;       grep: Update grep-pattern for grep-case-insensitive-option.
;;       compilation-mode-font-lock-keywords: Don't regexp-quote grep-pattern here (done in grep).
;;     Changed to defcustom: compile-buffer-mouse-face, grep-regexp-face, grep-default-regexp-fn.
;; 2006/03/31 dadams
;;     No longer use display-in-minibuffer.
;; 2006/01/01 dadams
;;     grep-regexp-face: Remove reference to skyblue-background-face.
;; 2005/12/18 dadams
;;     Use minibuffer-prompt face.  Removed require of def-face-const.el.
;; 2004/03/16 dadams
;;     compilation-goto-locus - Added message on removing highlighting
;; 2000/09/27 dadams
;;     Updated for Emacs 20.7:
;;     1. Removed compilation-sentinel.
;;     2. compile-internal: go to eob before running process.
;; 1999/08/12 dadams
;;     underline instead of highlight for mouse-face, and put on whole line.
;; 1999/04/14 dadams
;;     grep-regexp-face: Define as skyblue-background-face, if that is defined.
;; 1999/04/13  dadams
;;     compilation-sentinel: Put mouse-face only on the grep-regexp-alist part.
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

(eval-when-compile (require 'cl)) ;; incf (plus, for Emacs 20: pop,
                                  ;;       and, for Emacs <20: when, unless)
(require 'compile-20) ;; for new defvars from `compile.el'
(require 'compile)

(require 'thingatpt nil t) ;; (no error if not found): word-at-point
(when (and (require 'thingatpt+ nil t);; (no error if not found)
           (fboundp 'tap-put-thing-at-point-props)) ; >= 2012-08-21
  (tap-define-aliases-wo-prefix)
  (tap-put-thing-at-point-props))
 ;; non-nil-symbol-name-nearest-point

(require 'highlight nil t) ;; (no error if not found): hlt-highlight-regexp-region

;;; Free variables here - quiet byte compiler.
(defvar lazy-lock-defer-on-scrolling)
(defvar compilation-filter-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Compile mode is suitable only for specially formatted data.
(put 'compile-mode 'mode-class 'special)


;;; Faces

;; This is defined in `faces.el', Emacs 22+.  This definition is adapted to Emacs 20.
(unless (facep 'minibuffer-prompt)
  (defface minibuffer-prompt '((((background dark)) (:foreground "cyan"))
                               (t (:foreground "dark blue")))
    "Face for minibuffer prompts."
    :group 'basic-faces))


;;; User options

;;;###autoload
(defcustom grep-case-insensitive-option "-i"
  "*Option for grep command that indicates case-insensitivity.
This is taken into account for match highlighting.
If your grep command has no such option, set this to \"\"."
  :type 'string :group 'compilation)

;;;###autoload
(defcustom compile-buffer-mouse-face 'underline
  "*Face for highlighting mouse-overs in compilation buffer."
  :type 'sexp :group 'compilation)

;;;###autoload
(defcustom grep-regexp-face
  (or (and (fboundp 'set-face-background)
           (fboundp 'x-color-defined-p)
           (x-color-defined-p "SkyBlue")
           (prog1 (make-face 'grep-regexp-face)
             (set-face-background 'grep-regexp-face "SkyBlue")))
      'highlight)
  "*Face for highlighting `grep' regexps."
  :type 'sexp :group 'compilation)

;;;###autoload
(defcustom grep-default-regexp-fn (if (fboundp 'non-nil-symbol-name-nearest-point)
                                      'non-nil-symbol-name-nearest-point
                                    'word-at-point)
  "*Function of 0 args called to provide default search regexp to \\[grep].
Some reasonable choices are defined in `thingatpt+.el':
`word-nearest-point', `non-nil-symbol-name-nearest-point',
`region-or-non-nil-symbol-name-nearest-point', `sexp-nearest-point'.

This is ignored if Transient Mark mode is on and the region is active
and non-empty.  In that case, the quoted (\") region text is used as
the default regexp.

If `grep-default-regexp-fn' is nil and no prefix arg is given to
`grep', then no defaulting is done.

Otherwise, if the value is not a function, then function
`grep-default-regexp-fn' does the defaulting."
  :type '(choice
          (const :tag "No default search regexp (unless you use `C-u')" nil)
          (function :tag "Function of zero args to provide default search regexp"))
  :group 'grep)

;;;###autoload
(defcustom grep-default-comment-line-regexp ":[0-9]+: *;"
  "*Default regexp for a comment line, for use in `remove-grep-comments'.
The default value matches lines that begin with a Lisp comment."
  :type 'string :group 'grep)


;;; Internal variables (not user options)

(defvar grep-pattern nil "Search pattern used by latest \\[grep] command.")


(defun grep-default-regexp-fn ()
  "*Function of 0 args called to provide default search regexp to \\[grep].
This is used only if both of the following are true:
- Transient Mark mode is off or the region is inactive or empty.
- The value of option `grep-default-regexp-fn' is
  `grep-default-regexp-fn'.

When this is used, the default regexp is provided by calling the
first of these that references a defined function:
  - variable `grep-default-regexp-fn'
  - variable `find-tag-default-function'
  - the `find-tag-default-function' property of the `major-mode'
  - function `non-nil-symbol-name-nearest-point', if bound
  - function `grep-tag-default'"
  (cond ((functionp grep-default-regexp-fn) grep-default-regexp-fn)
        (find-tag-default-function)
        ((get major-mode 'find-tag-default-function))
        ((fboundp 'non-nil-symbol-name-nearest-point) 'non-nil-symbol-name-nearest-point)
        (t                              ; Use `grep-tag-default' instead of
         'grep-tag-default)))           ; `find-tag-default', to avoid loading etags.



;; REPLACES ORIGINAL in `compile.el':
;; Resets `grep-pattern' from last grep.
;;;###autoload
(defun compile (command)
  "Compile the program including the current buffer.  Default: run `make'.
Runs COMMAND, a shell command, in a separate process asynchronously
with output going to the buffer `*compilation*'.

You can then use the command \\[next-error] to find the next error message
and move to the source code that caused it.

Interactively, prompts for the command if `compilation-read-command' is
non-nil; otherwise uses `compile-command'.  With prefix arg, always prompts.

To run more than one compilation at once, start one and rename the
\`*compilation*' buffer to some other name with \\[rename-buffer].
Then start the next one.

The name used for the buffer is actually whatever is returned by the
function in `compilation-buffer-name-function', so you can set that to
a function that generates a unique name."
  (interactive
   (if (or compilation-read-command current-prefix-arg)
       (list (read-from-minibuffer "Compile command: "
                                   compile-command nil nil
                                   '(compile-history . 1)))
     (list compile-command)))
  ;; Reset `grep-pattern' from last grep.
  (setq grep-pattern     nil
        compile-command  command)
  (save-some-buffers (not compilation-ask-about-save) nil)
  (compile-internal compile-command "No more errors."))



;; REPLACES ORIGINAL in `compile.el':
;; 1. Interactive spec uses `grep-default-regexp-fn'.
;; 2. Saves `grep-pattern' for highlighting.
;;
;;;###autoload
(defun grep (command-args)
  "Run `grep' with user-specified args, and collect output in a buffer.
COMMAND-ARGS are the user-specified arguments.
While `grep' runs asynchronously, you can use the
\\[next-error] command (M-x next-error), or \\<compilation-minor-mode-map>\\[compile-goto-error]
in the grep output buffer, to find the text that `grep' hits refer to.

This command uses a special history list for its arguments, so you can
easily repeat a `grep' command.

The text (regexp) to find is defaulted as follows:

- If Transient Mark mode is on and the region is active and nonempty,
  then the double-quoted region text is used.  (If the region contains
  double-quotes (\"), then you will need to escape them by hand.)

- If option `grep-default-regexp-fn' is a function, then it is called
  to return the default regexp.

- If `grep-default-regexp-fn' is nil and no prefix arg is provided,
  then no default regexp is used.

If a prefix arg is provided, the default text is substituted into the
last grep command in the grep command history (or into `grep-command'
if that history list is empty).  That is, the same command options and
files to search are used as the last time."
  (interactive
   (let ((arg  current-prefix-arg)
         grep-default)
     (unless grep-command (grep-compute-defaults))
     (when arg
       (let ((tag-default  (funcall (grep-default-regexp-fn))))
         (setq grep-default  (or (car grep-history) grep-command))
         ;; Replace the thing matching for with that around cursor
         (when (string-match "[^ ]+\\s +\\(-[^ ]+\\s +\\)*\\(\"[^\"]+\"\\|[^ ]+\\)"
                             grep-default)
           (setq grep-default  (replace-match tag-default t t grep-default 2)))))
     (list (read-from-minibuffer
            "grep <pattern> <files> :  "
            (if arg
                (or grep-default grep-command)
              (concat grep-command
                      (if (and transient-mark-mode mark-active
                               (not (eq (region-beginning) (region-end))))
                          ;; Use double-quoted region text.
                          (concat "\"" (buffer-substring-no-properties (region-beginning)
                                                                       (region-end)) "\"")
                        (and (grep-default-regexp-fn) (funcall (grep-default-regexp-fn))))
                      " "))
            nil nil 'grep-history))))

  ;; Remember `grep-pattern' for highlighting, if highlighting is possible.
  (when (fboundp 'set-face-foreground)
    (cond (;; Quoted pattern (either "..." or '...')
           (string-match
            (concat
             grep-program
             "[ \t]*\\(-[a-zA-Z]+\\s-+\\)*[ \t]*\\('[^']+'\\|\"[^\"]+\"\\)") ;"
            command-args)
           (setq grep-pattern
                 (substring command-args (1+ (match-beginning 2)) (1- (match-end 2)))))
          (;; Unquoted pattern.
           (string-match
            (concat grep-program
                    "[ \t]*\\(-[a-zA-Z]+\\s-+\\)*[ \t]*\\([^ \n\t'\"]+\\)") ; "
            command-args)
           (setq grep-pattern  (substring command-args (match-beginning 2) (match-end 2))))
          (t;; Bad pattern.
           (setq grep-pattern  nil))))

  ;; Account for a case-insensitivity option.  Thx to Tamas Patrovics for the suggestion.
  (when (and (not (string= "" grep-case-insensitive-option))
             (string-match grep-case-insensitive-option command-args))
    (setq grep-pattern  (mapconcat (lambda (char)
                                     (if (or (and (>= char ?a) (<= char ?z))
                                             (and (>= char ?A) (<= char ?Z)))
                                         (concat "["  (char-to-string (downcase char))
                                                 (char-to-string (upcase char)) "]")
                                       (char-to-string char)))
                                   grep-pattern "")))

  ;; Setting process-setup-function makes exit-message-function work
  ;; even when async processes aren't supported.
  (let* ((compilation-process-setup-function  'grep-process-setup)
         (buf
          (compile-internal (if null-device (concat command-args " " null-device) command-args)
                            "No more grep hits" "grep"
                            ;; Give it a simpler regexp to match.
                            nil grep-regexp-alist)))))


;; REPLACES ORIGINAL in `compile.el':
;; Sets up font-lock mode to treat `grep-pattern'.
(defun compilation-mode-font-lock-keywords ()
  "Return expressions to highlight in Compilation mode."
  (nconc
   ;;
   ;; Compiler warning/error lines.
   (mapcar (function
            (lambda (item)
              ;; Prepend "^", adjusting FILE-IDX and LINE-IDX accordingly.
              (let ((file-idx  (nth 1 item))
                    (line-idx  (nth 2 item))
                    (col-idx   (nth 3 item))
                    keyword)
                (when (numberp col-idx)
                  (setq keyword  (cons (list (1+ col-idx) 'font-lock-type-face nil t) keyword)))
                (when (numberp line-idx)
                  (setq keyword  (cons (list (1+ line-idx) 'font-lock-variable-name-face)
                                       keyword)))
                (when (numberp file-idx)
                  (setq keyword  (cons (list (1+ file-idx) 'font-lock-warning-face) keyword)))
                (cons (concat "^\\(" (nth 0 item) "\\)") keyword))))
           compilation-error-regexp-alist)
   ;;
   ;; Non-nil `grep-pattern'.
   (and grep-pattern `((,(concat "\\(" grep-pattern "\\)") 1 grep-regexp-face)))
   ;;
   ;; Compiler output lines.  Recognize `make[n]:' lines too.
   (list
    '("^\\([A-Za-z_0-9/\.+-]+\\)\\(\\[\\([0-9]+\\)\\]\\)?[ \t]*:"
      (1 font-lock-function-name-face) (3 font-lock-comment-face nil t)))))


;; REPLACES ORIGINAL in `compile.el':
;; 1. Set `font-lock-fontified' to nil.
;; 2. Don't let frame get shrunk here.
(defun compile-internal (command error-message
                                 &optional name-of-mode parser
                                 error-regexp-alist name-function
                                 enter-regexp-alist leave-regexp-alist
                                 file-regexp-alist nomessage-regexp-alist)
  "Run compilation command COMMAND (low level interface).
ERROR-MESSAGE is a string to print if the user asks to see another error
and there are no more errors.  The rest of the arguments, 3-10 are optional.
For them nil means use the default.
NAME-OF-MODE is the name to display as the major mode in the compilation
buffer.  PARSER is the error parser function.  ERROR-REGEXP-ALIST is the error
message regexp alist to use.  NAME-FUNCTION is a function called to name the
buffer.  ENTER-REGEXP-ALIST is the enter directory message regexp alist to use.
LEAVE-REGEXP-ALIST is the leave directory message regexp alist to use.
FILE-REGEXP-ALIST is the change current file message regexp alist to use.
NOMESSAGE-REGEXP-ALIST is the nomessage regexp alist to use.
  The defaults for these variables are the global values of
\`compilation-parse-errors-function', `compilation-error-regexp-alist',
\`compilation-buffer-name-function', `compilation-enter-directory-regexp-alist',
\`compilation-leave-directory-regexp-alist', `compilation-file-regexp-alist',
\ and `compilation-nomessage-regexp-alist', respectively.
For arg 7-10 a value of t means an empty alist.

Return the compilation buffer created."
  (let (outbuf)
    (save-excursion
      (unless name-of-mode (setq name-of-mode  "Compilation"))
      (setq outbuf  (get-buffer-create
                     (funcall (or name-function compilation-buffer-name-function
                                  (function (lambda (mode)
                                    (concat "*" (downcase mode) "*"))))
                              name-of-mode)))
      (set-buffer outbuf)
      (let ((comp-proc  (get-buffer-process (current-buffer))))
        (when comp-proc
          (if (or (not (eq (process-status comp-proc) 'run))
                  (yes-or-no-p (format "A %s process is running; kill it? "
                                       name-of-mode)))
              (condition-case nil
                  (progn (interrupt-process comp-proc)
                         (sit-for 1)
                         (delete-process comp-proc))
                (error nil))
            (error "Cannot have two processes in `%s' at once"
                   (buffer-name)))))
      ;; In case the compilation buffer is current, make sure we get the global
      ;; values of compilation-error-regexp-alist, etc.
      (kill-all-local-variables))
    (unless error-regexp-alist (setq error-regexp-alist  compilation-error-regexp-alist))
    (unless enter-regexp-alist (setq enter-regexp-alist  compilation-enter-directory-regexp-alist))
    (unless leave-regexp-alist (setq leave-regexp-alist  compilation-leave-directory-regexp-alist))
    (unless file-regexp-alist  (setq file-regexp-alist   compilation-file-regexp-alist))
    (unless nomessage-regexp-alist
      (setq nomessage-regexp-alist  compilation-nomessage-regexp-alist))
    (unless parser (setq parser  compilation-parse-errors-function))
    (let ((thisdir  default-directory)
          outwin)
      (save-excursion
        ;; Clear out the compilation buffer and make it writable.
        ;; Change its default-directory to the directory where the compilation
        ;; will happen, and insert a `cd' command to indicate this.
        (set-buffer outbuf)
        (setq buffer-read-only  nil)
        (buffer-disable-undo (current-buffer))
        (erase-buffer)
        (buffer-enable-undo (current-buffer))
        (setq default-directory  thisdir)
        (insert "cd " thisdir "\n" command "\n")
        (setq font-lock-fontified  nil)  ; DDA
        (set-buffer-modified-p nil))
      ;; If we're already in the compilation buffer, go to the end
      ;; of the buffer, so point will track the compilation output.
      (when (eq outbuf (current-buffer)) (goto-char (point-max)))
      ;; Pop up the compilation buffer.
      ;; DDA: Don't let frame get resized now. - see `fit-frame.el'
      (setq outwin  (let ((fit-frame-inhibit-fitting-flag  t)) (display-buffer outbuf)))
      (save-excursion
        (set-buffer outbuf)
        ;; D. Adams: next line added to fix bug when my redefined version of `display-buffer' is
        ;; used.  Without it, the error msgs are inserted above the "cd ..." & "grep ..." lines.
        (goto-char (point-max))
        (compilation-mode name-of-mode)
        ;; (setq buffer-read-only  t)  ;;; Non-ergonomic.
        (set (make-local-variable 'compilation-parse-errors-function) parser)
        (set (make-local-variable 'compilation-error-message) error-message)
        (set (make-local-variable 'compilation-error-regexp-alist)
             error-regexp-alist)
        (set (make-local-variable 'compilation-enter-directory-regexp-alist)
             enter-regexp-alist)
        (set (make-local-variable 'compilation-leave-directory-regexp-alist)
             leave-regexp-alist)
        (set (make-local-variable 'compilation-file-regexp-alist)
             file-regexp-alist)
        (set (make-local-variable 'compilation-nomessage-regexp-alist)
             nomessage-regexp-alist)
        (set (make-local-variable 'compilation-arguments)
             (list command error-message
                   name-of-mode parser
                   error-regexp-alist name-function
                   enter-regexp-alist leave-regexp-alist
                   file-regexp-alist nomessage-regexp-alist))
        (make-local-variable 'lazy-lock-defer-on-scrolling) ; `lazy...' is a free var here.
        ;; This proves a good idea if the buffer's going to scroll with lazy-lock on.
        (setq lazy-lock-defer-on-scrolling  t
              default-directory             thisdir
              compilation-directory-stack   (list default-directory))
        (set-window-start outwin (point-min))
        (unless (eq outwin (selected-window)) (set-window-point outwin (point-min)))
        (compilation-set-window-height outwin)
        (when compilation-process-setup-function
          (funcall compilation-process-setup-function))
        ;; Start the compilation.
        (if (fboundp 'start-process)
            (let* ((process-environment  (cons "EMACS=t" process-environment))
                   (proc                 (start-process-shell-command (downcase mode-name)
                                                                      outbuf
                                                                      command)))
              (set-process-sentinel proc 'compilation-sentinel)
              (set-process-filter proc 'compilation-filter)
              (set-marker (process-mark proc) (point) outbuf)
              (setq compilation-in-progress  (cons proc compilation-in-progress)))
          ;; No asynchronous processes available.
          (message "Executing `%s'..." command)
          ;; Fake modeline display as if `start-process' were run.
          (setq mode-line-process  ":run")
          (force-mode-line-update)
          (sit-for 0)                   ; Force redisplay
          (let ((status  (call-process shell-file-name nil outbuf nil "-c" command)))
            (cond ((numberp status)
                   (compilation-handle-exit
                    'exit status (if (zerop status)
                                     "finished\n"
                                   (format "exited abnormally with code %d\n" status))))
                  ((stringp status)
                   (compilation-handle-exit 'signal status (concat status "\n")))
                  (t
                   (compilation-handle-exit 'bizarre status status))))
          (message "Executing `%s'...done" command)))
      (when compilation-scroll-output
        (save-selected-window (select-window outwin) (goto-char (point-max)))))
    ;; Make it so the next C-x ` will use this buffer.
    (setq compilation-last-buffer  outbuf)))


;;;###autoload
;; REPLACES ORIGINAL in `compile.el':
;; Use `fundamental-mode' instead of `kill-all-local-variables'.
(defun compilation-mode (&optional name-of-mode)
  "Major mode for compilation log buffers.
\\<compilation-mode-map>To visit the source for a line-numbered error,
move point to the error message line and type \\[compile-goto-error].
To kill the compilation, type \\[kill-compilation].

Runs `compilation-mode-hook' with `run-hooks' (which see).
NAME-OF-MODE is the name to use for the `mode-name' (default:
\"Compilation\").

The following bindings are in effect in this mode:

\\{compilation-mode-map}"
  (interactive)
  (fundamental-mode)
  (use-local-map compilation-mode-map)
  (setq major-mode  'compilation-mode
        mode-name   (or name-of-mode "Compilation"))
  (compilation-setup)
  (set (make-local-variable 'font-lock-defaults) '(compilation-mode-font-lock-keywords t))
  (set (make-local-variable 'revert-buffer-function) 'compilation-revert-buffer)
  (run-hooks 'compilation-mode-hook))



;; REPLACES ORIGINAL in `compile.el':
;; Calls `what-line' at end to display line number.
;;;###autoload
(defun compilation-next-error (nth)
  "Move point to the NTH next error in the compilation buffer.
Does NOT find the source line like \\[next-error].
NTH defaults to 1, meaning the next error."
  (interactive "p")
  (unless (compilation-buffer-p (current-buffer))
    (error "Not in a compilation buffer"))
  (setq compilation-last-buffer  (current-buffer))
  (let ((errors  (compile-error-at-point)))
    ;; Move to the error after the one containing point.
    (goto-char (car (if (< nth 0)
                        (let ((i  0)
                              (e  compilation-old-error-list))
                          ;; See how many cdrs away ERRORS is from the start.
                          (while (not (eq e errors)) (incf i) (pop e))
                          (if (> (- nth) i)
                              (error "Moved back past first error")
                            (nth (+ i nth) compilation-old-error-list)))
                      (let ((compilation-error-list  (cdr errors)))
                        (compile-reinitialize-errors nil nil nth)
                        (if compilation-error-list
                            (nth (1- nth) compilation-error-list)
                          (error "Moved past last error")))))))
  (what-line))


;; REPLACES ORIGINAL in `compile.el':
;; 1. Highlights `grep-pattern' at error location.
;; 2. Displays line number.
;; 3. Raises frame - especially useful when used with `thumb-frm.el'.
(defun compilation-goto-locus (next-error)
  "Jump to an error locus returned by `compilation-next-error-locus'.
Takes one argument, a cons (ERROR . SOURCE) of two markers.
Selects a window with point at SOURCE, with another window displaying ERROR.
NEXT-ERROR is the locus of the next compilation error."
  (if (eq (window-buffer (selected-window))
          (marker-buffer (car next-error)))
      ;; If the compilation buffer window is selected,
      ;; keep the compilation buffer in this window;
      ;; display the source in another window.
      (let ((pop-up-windows  t))
        (pop-to-buffer (marker-buffer (cdr next-error))))
    (if (and (window-dedicated-p (selected-window))
             (eq (selected-window) (frame-root-window)))
        (switch-to-buffer-other-frame (marker-buffer (cdr next-error)))
      (switch-to-buffer (marker-buffer (cdr next-error)))))
  (goto-char (cdr next-error))
  ;; If narrowing got in the way of going to the right place, then widen.
  (unless (= (point) (marker-position (cdr next-error)))
    (widen) (goto-char (cdr next-error)))
  ;; Show compilation buffer in other window, scrolled to this error.
  (let* ((pop-up-windows  t)
         ;; Use an existing window if it is in a visible frame.
         (w               (or (get-buffer-window (marker-buffer (car next-error)) 'visible)
                              ;; Pop up a window.
                              (display-buffer (marker-buffer (car next-error))))))
    (set-window-point w (car next-error))
    (set-window-start w (car next-error))
    ;; Highlight `grep-pattern' in compilation buffer, if possible.
    (when (and (fboundp 'hlt-highlight-regexp-region) grep-pattern)
      (hlt-highlight-regexp-region (line-beginning-position) (line-end-position)
                                   grep-pattern grep-regexp-face)
      (message (format "Line %s. %s"
                       (+ (count-lines (point-min) (point))
                          (if (= (current-column) 0) 1 0))
                       (substitute-command-keys
                        "`\\[negative-argument] \
\\[highlight]' to remove highlighting (in a region)."))))
    (compilation-set-window-height w))
  (raise-frame))



;; REPLACES ORIGINAL in `compile.el':
;; 1) Use `compile-buffer-mouse-face', not `highlight', as `mouse-face'.
;; 2) Put `mouse-face' on the whole line.
(defun compile-reinitialize-errors (reparse &optional limit-search find-at-least)
  "Parse new errors in compilation buffer, or reparse from the beginning
if the user has asked for that."
  (save-excursion
    (set-buffer compilation-last-buffer)
    ;; If we are out of errors, or if user says "reparse",
    ;; discard the info we have, to force reparsing.
    (when (or (eq compilation-error-list t)
              reparse)
      (compilation-forget-errors))
    ;; If `compilation-error-list' is non-nil, it points to a specific
    ;; error the user wanted.  So don't move it around.
    (unless (and compilation-error-list
                 (or (not limit-search)
                     (> compilation-parsing-end limit-search))
                 (or (not find-at-least)
                     (>= (length compilation-error-list) find-at-least)))
      ;; This was here for a long time (before my rewrite); why? --Roland
      ;;(switch-to-buffer compilation-last-buffer)
      (set-buffer-modified-p nil)
      (when (< compilation-parsing-end (point-max))
        ;; `compilation-error-list' might be non-nil if we have a non-nil
        ;; LIMIT-SEARCH or FIND-AT-LEAST arg.  In that case its value
        ;; records the current position in the error list, and we must
        ;; preserve that after reparsing.
        (let ((error-list-pos  compilation-error-list))
          (funcall compilation-parse-errors-function
                   limit-search
                   (and find-at-least
                        ;; We only need enough new parsed errors to reach
                        ;; FIND-AT-LEAST errors past the current
                        ;; position.
                        (- find-at-least (length compilation-error-list))))
          ;; Remember the entire list for `compilation-forget-errors'.  If
          ;; this is an incremental parse, append to previous list.  If
          ;; we are parsing anew, `compilation-forget-errors' cleared
          ;; compilation-old-error-list above.
          (setq compilation-old-error-list
                (nconc compilation-old-error-list compilation-error-list))
          (when error-list-pos
            ;; We started in the middle of an existing list of parsed
            ;; errors before parsing more; restore that position.
            (setq compilation-error-list  error-list-pos))
          ;; Mouse-Highlight (the first line of) each error message when the
          ;; mouse pointer moves over it:
          (let ((inhibit-read-only  t)
                (buffer-undo-list   t)
                (error-list         compilation-error-list)
                deactivate-mark)
            (while error-list
              (save-excursion (put-text-property (goto-char (car (car error-list)))
                                                 (line-end-position)
                                                 'mouse-face compile-buffer-mouse-face))
              (setq error-list  (cdr error-list)))))))))


;; REPLACES ORIGINAL in `compile.el':
;; Use `compile-buffer-mouse-face', not `highlight', as `mouse-face'.
(defun compilation-forget-errors ()
  "Set `compilation-error-list' to nil, and unchain markers
that point to the error messages and their text, so that they no
longer slow down gap motion.  This would happen anyway at the next
garbage collection, but it is better to do it right away."
  (while compilation-old-error-list
    (let ((next-error  (car compilation-old-error-list)))
      (set-marker (car next-error) nil)
      (if (markerp (cdr next-error))
          (set-marker (cdr next-error) nil)))
    (setq compilation-old-error-list  (cdr compilation-old-error-list)))
  (setq compilation-error-list       nil
        compilation-directory-stack  (list default-directory)
        compilation-parsing-end      1)
  ;; Remove the highlighting added by compile-reinitialize-errors:
  (let ((inhibit-read-only  t)
        (buffer-undo-list   t)
        deactivate-mark)
    (remove-text-properties (point-min) (point-max) (list 'mouse-face compile-buffer-mouse-face))))

;;;###autoload
(defun new-grep-buffer ()
  "Rename current grep buffer and switch to new buffer *grep*.
Current buffer must be a grep buffer.  It is renamed to *grep*<N>."
  (interactive)
  (unless (string-match "\\*grep\\*" (buffer-name (current-buffer)))
    (error "Not in a grep buffer"))
  (rename-uniquely)
  (switch-to-buffer "*grep*")
  (compilation-mode))

;;;###autoload
(defun choose-grep-buffer (buf)
  "Switch to a grep buffer."
  (interactive
   (let ((bufs  (grep-buffers)))
     (unless bufs (error "No grep buffers"))
     (list (completing-read "Grep buffer: " bufs nil t nil nil
                            (and (consp (cdr bufs)) (car (cadr bufs)))))))
  (switch-to-buffer buf)
  (select-frame-set-input-focus (selected-frame))
  (compilation-mode))

(defun grep-buffers ()
  "List of names of grep buffers."
  (let ((bufs  ()))
    (dolist (buf (buffer-list))
      (when (string-match "\\*grep\\*" (buffer-name buf)) (push (list (buffer-name buf)) bufs)))
    (nreverse bufs)))

(unless (fboundp 'select-frame-set-input-focus) ; Defined in Emacs 22.
  (defun select-frame-set-input-focus (frame)
    "Select FRAME, raise it, and set input focus, if possible."
    (select-frame frame)
    (raise-frame frame)
    ;; Ensure, if possible, that frame gets input focus.
    (cond ((eq window-system 'x) (x-focus-frame frame))
          ((eq window-system 'w32) (w32-focus-frame frame)))
    (cond (focus-follows-mouse (set-mouse-position (selected-frame) (1- (frame-width)) 0)))))

;;;###autoload
(defun remove-grep-comments (&optional read-regexp-p)
  "Remove lines that are completely commented out.
With a prefix argument, you are prompted for the regexp used to match
 commented lines.  The default value is
 `grep-default-comment-line-regexp'.
With no prefix argument, this default value is used as the regexp.

You can use command `grep-toggle-comments' to toggle automatic removal
of commented lines.

Note: This simply removes lines that begin with the regexp you
provide.  It does not, in general, remove multi-line comments.  Use it
to remove C++ comments that start with //, but not multi-line comments
between /* and */."
  (interactive "P")
  (let ((inhibit-read-only  t)
        (regexp             (if read-regexp-p
                                (read-from-minibuffer
                                 "Comment regexp: " nil nil nil 'regexp-history
                                 grep-default-comment-line-regexp)
                              grep-default-comment-line-regexp)))
    (save-excursion (goto-char (point-min))
                    (flush-lines regexp))))

;;;###autoload
(defun toggle-grep-comments ()
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




;;; CHECK TO SEE IF THIS BUG FIX IS STILL NEEDED.
;;;;; COMINT-FILE-NAME-PREFIX is free here.
;;;(defun compilation-parse-errors (limit-search find-at-least)
;;;  "Parse the current buffer as `grep', `cc' or `lint' error messages.
;;;See var `compilation-parse-errors-function' for its interface."
;;;  (setq compilation-error-list  nil)
;;;  (message "Parsing error messages ...")
;;;  (let (text-buffer orig orig-expanded parent-expanded
;;;        regexp enter-group leave-group error-group
;;;        alist subexpr error-regexp-groups
;;;        (found-desired nil)
;;;        (compilation-num-errors-found 0))
;;;    ;; Don't reparse messages already seen at last parse.
;;;    (goto-char compilation-parsing-end)
;;;    ;; Don't parse first two lines as error messages.  This matters for grep.
;;;    (when (bobp)
;;;      (forward-line 2)
;;;      ;; Move back so point is before the newline.
;;;      ;; This matters because some error regexps use \n instead of ^, in order
;;;      ;; to be faster.
;;;      (forward-char -1))
;;;    ;; Compile all the regexps we want to search for into one.
;;;    (setq regexp (concat "\\(" compilation-enter-directory-regexp "\\)\\|"
;;;                         "\\(" compilation-leave-directory-regexp "\\)\\|"
;;;                         "\\(" (mapconcat (function
;;;                                           (lambda (elt)
;;;                                             (concat "\\(" (car elt) "\\)")))
;;;                                          compilation-error-regexp-alist
;;;                                          "\\|") "\\)"))
;;;    ;; Find out how many \(...\) groupings are in each of the regexps, and set
;;;    ;; *-GROUP to the grouping containing each constituent regexp (whose
;;;    ;; subgroups will come immediately thereafter) of the big regexp we have
;;;    ;; just constructed.
;;;    (setq enter-group 1)
;;;    (setq leave-group (+ enter-group
;;;                         (count-regexp-groupings
;;;                          compilation-enter-directory-regexp)
;;;                         1))
;;;    (setq error-group (+ leave-group
;;;                         (count-regexp-groupings
;;;                          compilation-leave-directory-regexp)
;;;                         1))
;;;    ;; Compile an alist (IDX FILE LINE [COL]), where IDX is the number of
;;;    ;; the subexpression for an entire error-regexp, and FILE and LINE (and
;;;    ;; possibly COL) are the numbers for the subexpressions giving the file
;;;    ;; name and line number (and possibly column number).
;;;    (setq alist (or compilation-error-regexp-alist
;;;                    (error "List `compilation-error-regexp-alist' is empty")))
;;;    (setq subexpr (1+ error-group))
;;;    (while alist
;;;      (setq error-regexp-groups
;;;            (cons (list subexpr
;;;                        (+ subexpr (nth 1 (car alist)))
;;;                        (+ subexpr (nth 2 (car alist)))
;;;                        (and (nth 3 (car alist))
;;;                             (+ subexpr (nth 3 (car alist)))))
;;;                  error-regexp-groups))
;;;      (setq subexpr (+ subexpr 1 (count-regexp-groupings (caar alist))))
;;;      (pop alist))
;;;    (setq orig default-directory)
;;;    (setq orig-expanded (file-truename orig))
;;;    (setq parent-expanded (expand-file-name "../" orig-expanded))
;;;    (while (and (not found-desired)
;;;                ;; We don't just pass LIMIT-SEARCH to `re-search-forward'
;;;                ;; because we want to find matches containing LIMIT-SEARCH
;;;                ;; but which extend past it.
;;;                (re-search-forward regexp nil t))
;;;      ;; Figure out which constituent regexp matched.
;;;      (cond ((match-beginning enter-group)
;;;             ;; The match was the enter-directory regexp.
;;;             (let ((dir
;;;                    (file-name-as-directory
;;;                     (expand-file-name
;;;                      (buffer-substring (match-beginning (+ enter-group 1))
;;;                                        (match-end (+ enter-group 1)))))))
;;;               ;; The directory name in the "entering" message
;;;               ;; is a truename.  Try to convert it to a form
;;;               ;; like what the user typed in.
;;;               (setq dir (compile-abbreviate-directory dir orig orig-expanded
;;;                                                       parent-expanded))
;;;               (push dir compilation-directory-stack)
;;;               (when (file-directory-p dir) (setq default-directory dir)))
;;;             (when (and limit-search (>= (point) limit-search))
;;;               ;; The user wanted a specific error, and we're past it.
;;;               ;; We do this check here (and in the leave-group case)
;;;               ;; rather than at the end of the loop because if the last
;;;               ;; thing seen is an error message, we must carefully
;;;               ;; discard the last error when it is the first in a new
;;;               ;; file (see below in the error-group case).
;;;               (setq found-desired t)))
;;;            ((match-beginning leave-group)
;;;             ;; The match was the leave-directory regexp.
;;;             (let ((beg (match-beginning (+ leave-group 1)))
;;;                   (stack compilation-directory-stack))
;;;               (when beg
;;;                 (let ((dir (file-name-as-directory
;;;                             (expand-file-name
;;;                              (buffer-substring beg (match-end (+ leave-group
;;;                                                                  1)))))))
;;;                   ;; The directory name in the "entering" message is a
;;;                   ;; truename.  Try to convert it to a form like what the
;;;                   ;; user typed in.
;;;                   (setq dir (compile-abbreviate-directory
;;;                              dir orig orig-expanded parent-expanded))
;;;                   (while (and stack (not (string-equal (car stack) dir)))
;;;                     (pop stack))))
;;;               (setq compilation-directory-stack (cdr stack))
;;;               (setq stack (car compilation-directory-stack))
;;;               (when stack (setq default-directory stack)))
;;;             (when (and limit-search (>= (point) limit-search))
;;;               ;; The user wanted a specific error, and we're past it.
;;;               ;; We do this check here (and in the enter-group case)
;;;               ;; rather than at the end of the loop because if the last
;;;               ;; thing seen is an error message, we must carefully
;;;               ;; discard the last error when it is the first in a new
;;;               ;; file (see below in the error-group case).
;;;               (setq found-desired t)))
;;;            ((match-beginning error-group)
;;;             ;; The match was the composite error regexp.
;;;             ;; Find out which individual regexp matched.
;;;             (setq alist error-regexp-groups)
;;;             (while (and alist (null (match-beginning (caar alist))))
;;;               (pop alist))
;;;             (if alist
;;;                 (setq alist (car alist))
;;;               (error "COMPILATION-PARSE-ERRORS: Impossible regexp match"))
;;;             ;; Extract the file name and line number from the error message.
;;;             (let ((beginning-of-match (match-beginning 0)) ;looking-at nukes
;;;                   (filename (buffer-substring (match-beginning (nth 1 alist))
;;;                                               (match-end (nth 1 alist))))
;;;                   (linenum (string-to-number
;;;                             (buffer-substring
;;;                              (match-beginning (nth 2 alist))
;;;                              (match-end (nth 2 alist)))))
;;;                   (column (and (nth 3 alist)
;;;                                (string-to-number
;;;                                 (buffer-substring
;;;                                  (match-beginning (nth 3 alist))
;;;                                  (match-end (nth 3 alist)))))))
;;;               ;; Check for a COMINT-FILE-NAME-PREFIX and prepend it if
;;;               ;; appropriate.  (This is useful for `compilation-minor-mode'
;;;               ;; in an `rlogin-mode' buffer.)
;;;               (when (and (boundp 'comint-file-name-prefix)
;;;                          ;; If file name is relative, default-directory will
;;;                          ;; already contain COMINT-FILE-NAME-PREFIX (done by
;;;                          ;; compile-abbreviate-directory).
;;;                          (file-name-absolute-p filename))
;;;                 (setq filename (concat comint-file-name-prefix filename)))
;;;               (push default-directory filename)
;;;               ;; Locate the erring file and line.
;;;               ;; Cons a new elt onto `compilation-error-list',
;;;               ;; giving a marker for the current compilation buffer
;;;               ;; location, and the file and line number of the error.
;;;               (save-excursion
;;;                 (beginning-of-line 1)
;;;                 (let ((this (cons (point-marker)
;;;                                   (list filename linenum column))))
;;;                   ;; Don't add the same source line more than once.
;;;                   (unless (equal (cdr this) (cdar compilation-error-list))
;;;                     (push this compilation-error-list)
;;;                     (incf compilation-num-errors-found))))
;;;               (when (and (or (and find-at-least
;;;                                   (> compilation-num-errors-found
;;;                                      find-at-least))
;;;                        ;;; D. ADAMS: Second part of next test was:
;;;                        ;;; (>= (point) limit-search).
;;;                        ;;; Was thus bugged: Last error was removed from list.
;;;                              (and limit-search
;;;                                   (>= (save-excursion (end-of-line -1)
;;;                                                       (point))
;;;                                       limit-search)))
;;;                          ;; We have found as many new errors as user
;;;                          ;; wants, or past the buffer position he
;;;                          ;; indicated.  We continue to parse until we
;;;                          ;; have seen all the consecutive errors in
;;;                          ;; the same file, so the error positions
;;;                          ;; will be recorded as markers in this
;;;                          ;; buffer that might change.
;;;                          (cdr compilation-error-list) ; Must check at least 2.
;;;                          (not (equal (cadr (nth 0 compilation-error-list))
;;;                                      (cadr (nth 1 compilation-error-list)))))
;;;                 ;; Discard the error just parsed, so that the next
;;;                 ;; parsing run can get it and the following errors in
;;;                 ;; the same file all at once.  If we didn't do this, we
;;;                 ;; would have the same problem we are trying to avoid
;;;                 ;; with the test above, just delayed until the next run!
;;;                 (pop compilation-error-list)
;;;                 (goto-char beginning-of-match)
;;;                 (setq found-desired t))))
;;;            (t (error "COMPILATION-PARSE-ERRORS: Known groups didn't match")))
;;;      (message "Parsing error messages ... %d (%.0f%% of buffer)"
;;;               compilation-num-errors-found
;;;               ;; Use floating-point because (* 100 (point)) frequently
;;;               ;; exceeds the range of Emacs Lisp integers.
;;;               (/ (* 100.0 (point)) (point-max)))
;;;      (when (and limit-search (>= (point) limit-search))
;;;        ;; User wanted a specific error, and we're past it.
;;;        (setq found-desired t)))
;;;    (setq compilation-parsing-end (if found-desired
;;;                                      (point)
;;;                                    ;; We have searched the whole buffer.
;;;                                    (point-max))))
;;;  (setq compilation-error-list (nreverse compilation-error-list))
;;;  (message "Parsing error messages ... done."))

;;;;;;;;;;;;;;;;;;

(provide 'compile+20)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; compile+20.el ends here
