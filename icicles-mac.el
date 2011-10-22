;;; icicles-mac.el --- Macros for Icicles
;;
;; Filename: icicles-mac.el
;; Description: Macros for Icicles
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 1996-2011, Drew Adams, all rights reserved.
;; Created: Mon Feb 27 09:24:28 2006
;; Version: 22.0
;; Last-Updated: Fri Oct 21 13:20:31 2011 (-0700)
;;           By: dradams
;;     Update #: 933
;; URL: http://www.emacswiki.org/cgi-bin/wiki/icicles-mac.el
;; Keywords: internal, extensions, help, abbrev, local, minibuffer,
;;           keys, apropos, completion, matching, regexp, command
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x
;;
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  This is a helper library for library `icicles.el'.  It defines
;;  macros.  For Icicles documentation, see `icicles-doc1.el' and
;;  `icicles-doc2.el'.
;;
;;  Macros defined here:
;;
;;    `icicle-buffer-bindings', `icicle-condition-case-no-debug',
;;    `icicle-define-add-to-alist-command',
;;    `icicle-define-bookmark-command',
;;    `icicle-define-bookmark-command-1',
;;    `icicle-define-bookmark-other-window-command',
;;    `icicle-define-command', `icicle-define-file-command',
;;    `icicle-define-search-bookmark-command',
;;    `icicle-define-sort-command', `icicle-file-bindings',
;;    `icicle-kbd', `icicle-with-selected-window'.
;;
;;  Commands defined here:
;;
;;    `icicle-read-kbd-macro'.
;;
;;  Non-interactive functions defined here:
;;
;;    `icicle-edmacro-parse-keys'.
;;
;;  You might also be interested in my library `imenu+.el', which
;;  teaches the macros defined here to Imenu, so the functions defined
;;  with those macros show up in Imenu menus.
;;
;;  I've also included some commented-out code at the end, which you
;;  might want to use in your init file (~/.emacs).  It provides
;;  better indentation for the doc string when you use the macros here
;;  in your code.
;;
;;  For descriptions of changes to this file, see `icicles-chg.el'.
;;
;;  ******************
;;  NOTE: Whenever you update Icicles (i.e., download new versions of
;;  Icicles source files), I recommend that you do the following:
;;
;;      1. Delete all existing byte-compiled Icicles files
;;         (icicles*.elc).
;;      2. Load Icicles (`load-library' or `require').
;;      3. Byte-compile the source files.
;;
;;  In particular, always load `icicles-mac.el' (not
;;  `icicles-mac.elc') before you byte-compile new versions of the
;;  files, in case there have been any changes to Lisp macros (in
;;  `icicles-mac.el').
;;  ******************
 
;;(@> "Index")
;;
;;  If you have library `linkd.el' and Emacs 22 or later, load
;;  `linkd.el' and turn on `linkd-mode' now.  It lets you easily
;;  navigate around the sections of this doc.  Linkd mode will
;;  highlight this Index, as well as the cross-references and section
;;  headings throughout this file.  You can get `linkd.el' here:
;;  http://dto.freeshell.org/notebook/Linkd.html.
;;
;;  (@> "User Options")
;;  (@> "Macros")
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street,
;; Fifth Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;; Byte-compiling this file, you will likely get some error or warning
;; messages. All of the following are benign.  They are due to
;; differences between different versions of Emacs.
;;
;; Compiling in Emacs 20:
;;
;; the function x-focus-frame is not known to be defined.

(eval-when-compile (require 'cl)) ;; incf, plus for Emacs < 21: dolist, push

;; Quiet the byte compiler for Emacs versions before 22.  For some reason, a value is required.
(unless (boundp 'minibuffer-completing-symbol)
  (defvar minibuffer-completing-symbol nil)
  (defvar minibuffer-message-timeout 2)
  (defvar minibuffer-prompt-properties nil))

;; Quiet the byte-compiler.
(defvar icicle-inhibit-try-switch-buffer)
(defvar read-file-name-completion-ignore-case)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
;;(@* "Macros")

;;; Macros -----------------------------------------------------------

;; $$$$$$
;; Same as vanilla `condition-case-no-debug', which is available starting with Emacs 23.
;; (defmacro icicle-condition-case-no-debug (var bodyform &rest handlers)
;;   "Like `condition-case', but does not catch anything when debugging.
;; Specifically, non-nil `debug-on-error' means catch no signals.
;; This is the same as `condition-case-no-debug': added to use in older
;; Emacs versions too."
;;   (let ((bodysym  (make-symbol "body")))
;;     `(let ((,bodysym  (lambda () ,bodyform)))
;;       (if debug-on-error
;;           (funcall ,bodysym)
;;         (condition-case ,var
;;             (funcall ,bodysym)
;;           ,@handlers)))))


;; Same as `naked-edmacro-parse-keys' in `naked.el'.
(defun icicle-edmacro-parse-keys (string &optional need-vector angles)
  "Like `edmacro-parse-keys', but does not use angle brackets, by default.
Non-nil optional arg ANGLES means to use angle brackets, exactly like
`edmacro-parse-keys'.  See `icicle-read-kbd-macro' for more about
ANGLES."
  (let ((case-fold-search  nil)
	(len               (length string)) ; We won't alter string in the loop below.
        (pos               0)
        (res               []))
    (while (and (< pos len)  (string-match "[^ \t\n\f]+" string pos))
      (let* ((word-beg  (match-beginning 0))
	     (word-end  (match-end 0))
	     (word      (substring string word-beg len))
	     (times     1)
             (key       nil))
        (if (and angles  (string-match "\\`<[^ <>\t\n\f][^>\t\n\f]*>" word))
            (setq word  (match-string 0 word)
                  pos   (+ word-beg (match-end 0)))
          (setq word  (substring string word-beg word-end)
                pos   word-end))
        (when (string-match "\\([0-9]+\\)\\*." word)
          (setq times  (string-to-number (substring word 0 (match-end 1)))
                word   (substring word (1+ (match-end 1)))))
        (cond ((string-match "^<<.+>>$" word)
               (setq key  (vconcat (if (eq (key-binding [?\M-x])
                                           'execute-extended-command)
                                       [?\M-x]
                                     (or (car (where-is-internal
                                               'execute-extended-command))
                                         [?\M-x]))
                                   (substring word 2 -2) "\r")))
              ((or (equal word "REM") (string-match "^;;" word))
               (setq pos  (string-match "$" string pos)))
              ((and (string-match (if angles
                                      "^\\(\\([ACHMsS]-\\)*\\)<\\(..+\\)>$"
                                    "^\\(\\([ACHMsS]-\\)*\\)\\([^ \t\f\n][^ \t\f\n]+\\)$")
                                  word)
                    (or angles
                        ;; Do not count `C-' etc. when at end of string.
                        (save-match-data (not (string-match "\\([ACHMsS]-.\\)+$" word))))
                    (progn
                      (setq word  (concat (substring word (match-beginning 1) (match-end 1))
                                          (substring word (match-beginning 3) (match-end 3))))
                      (not (string-match "\\<\\(NUL\\|RET\\|LFD\\|ESC\\|SPC\\|DEL\\)$" word))))
               (setq key  (list (intern word))))
              (t
               (let ((orig-word  word)
                     (prefix     0)
                     (bits       0))
                 (while (string-match "^[ACHMsS]-." word)
                   (incf bits (cdr (assq (aref word 0) '((?A . ?\A-\^@) (?C . ?\C-\^@)
                                                         (?H . ?\H-\^@) (?M . ?\M-\^@)
                                                         (?s . ?\s-\^@) (?S . ?\S-\^@)))))
                   (incf prefix 2)
                   (callf substring word 2))
                 (when (string-match "^\\^.$" word)
                   (incf bits ?\C-\^@)
                   (incf prefix)
                   (callf substring word 1))
                 (let ((found  (assoc word '(("NUL" . "\0") ("RET" . "\r")
                                             ("LFD" . "\n") ("TAB" . "\t")
                                             ("ESC" . "\e") ("SPC" . " ")
                                             ("DEL" . "\177")))))
                   (when found (setq word  (cdr found))))
                 (when (string-match "^\\\\[0-7]+$" word)
                   (loop for ch across word
                         for n = 0 then (+ (* n 8) ch -48)
                         finally do (setq word  (vector n))))
                 (cond ((= bits 0) (setq key  word))
                       ((and (= bits ?\M-\^@) (stringp word)  (string-match "^-?[0-9]+$" word))
                        (setq key  (loop for x across word collect (+ x bits))))
                       ((/= (length word) 1)
                        (error "%s must prefix a single character, not %s"
                               (substring orig-word 0 prefix) word))
                       ((and (/= (logand bits ?\C-\^@) 0) (stringp word)
                             ;; Used to accept `.' and `?' here, but `.' is simply wrong,
                             ;; and `C-?' is not used (so use `DEL' instead).
                             (string-match "[@-_a-z]" word))
                        (setq key  (list (+ bits (- ?\C-\^@) (logand (aref word 0) 31)))))
                       (t (setq key  (list (+ bits (aref word 0)))))))))
        (when key (loop repeat times do (callf vconcat res key)))))
    (when (and (>= (length res) 4)  (eq (aref res 0) ?\C-x)  (eq (aref res 1) ?\()
               (eq (aref res (- (length res) 2)) ?\C-x)  (eq (aref res (- (length res) 1)) ?\)))
      (setq res  (edmacro-subseq res 2 -2)))
    (if (and (not need-vector)
	     (loop for ch across res
		   always (and (if (fboundp 'characterp)  (characterp ch)  (char-valid-p ch))
			       (let ((ch2  (logand ch (lognot ?\M-\^@))))
				 (and (>= ch2 0)  (<= ch2 127))))))
	(concat (loop for ch across res collect (if (= (logand ch ?\M-\^@) 0)  ch  (+ ch 128))))
      res)))

;; Same as `naked-read-kbd-macro' in `naked.el'.
(defun icicle-read-kbd-macro (start &optional end angles)
  "Read the region as a keyboard macro definition.
Like `read-kbd-macro', but does not use angle brackets, by default.

With a prefix arg use angle brackets, exactly like `read-kbd-macro'.
That is, with non-nil arg ANGLES, expect key descriptions to use angle
brackets (<...>).  Otherwise, expect key descriptions not to use angle
brackets.  For example:

 (icicle-read-kbd-macro  \"mode-line\"  t) returns [mode-line]
 (icicle-read-kbd-macro \"<mode-line>\" t t)   returns [mode-line]"
  (interactive "r\P")
  (if (stringp start)
      (icicle-edmacro-parse-keys start end angles)
    (setq last-kbd-macro  (icicle-edmacro-parse-keys (buffer-substring start end) nil angles))))

;; Same as `naked' in `naked.el'.
(defmacro icicle-kbd (keys &optional angles)
  "Like `kbd', but does not use angle brackets, by default.
With non-nil optional arg ANGLES, expect key descriptions to use angle
brackets (<...>), exactly like `kbd'.  Otherwise, expect key
descriptions not to use angle brackets.  For example:

 (icicle-kbd \"mode-line\")     returns [mode-line]
 (icicle-kbd \"<mode-line>\" t) returns [mode-line]

The default behavior lets you use, e.g., \"C-x delete\" and \"C-delete\"
instead of \"C-x <delete>\" and \"C-<delete>\"."
  (icicle-read-kbd-macro keys nil angles))

;; Same definition as in `icicles-fn.el'.
(defun icicle-remove-if (pred xs)
  "A copy of list XS with no elements that satisfy predicate PRED."
  (let ((result  ()))
    (dolist (x xs) (unless (funcall pred x) (push x result)))
    (nreverse result)))

(defmacro icicle-condition-case-no-debug (var bodyform &rest handlers)
  "Like `condition-case', but do not catch per `debug-on-(error|quit)'.
If both `debug-on-error' and `debug-on-quit' are non-nil, then handle
only other signals - enter the debugger for errors and `C-g'.

If `debug-on-error' is non-nil and `debug-on-quit' is nil, then handle
all signals except errors that would be caught by an `error' handler.
Enter the debugger on such errors.

If `debug-on-quit' is non-nil and `debug-on-error' is nil, then handle
all signals except quitting.  Enter the debugger on quit (`C-g').

NOTE:
1. This does not treat `error' and `quit' handlers specially when
   they are in a list that is the car of a handler.  In such a case
   the handler remains in effect in spite of the values of
   `debug-on-(error|quit)'.

2. Only errors that would be caught by an `error' handler (if one were
   present) enter the debugger when `debug-on-error' is non-nil.  When
   a specific error handler (e.g. `arith-error') is present, it still
   handles such an error - the debugger is not entered just because
   `debug-on-error' is non-nil."
  (let ((bodysym  (make-symbol "body")))
    `(let ((,bodysym  (lambda () ,bodyform)))
      (cond ((and debug-on-error debug-on-quit)
             (condition-case ,var
                 (funcall ,bodysym)
               ,@(icicle-remove-if
                  (lambda (hh) (memq (car hh) '(error quit)))
                  handlers)))
            (debug-on-error
             (condition-case ,var
                 (funcall ,bodysym)
               ,@(icicle-remove-if
                  (lambda (hh) (eq (car hh) 'error))
                  handlers)))
            (debug-on-quit
             (condition-case ,var
                 (funcall ,bodysym)
               ,@(icicle-remove-if
                  (lambda (hh) (eq (car hh) 'quit))
                  handlers)))
            (t
             (condition-case ,var
                 (funcall ,bodysym)
               ,@handlers))))))

(if (fboundp 'with-selected-window)     ; Emacs 22+
    (defalias 'icicle-with-selected-window (symbol-function 'with-selected-window))
  (defmacro icicle-with-selected-window (window &rest body)
    "Execute the forms in BODY with WINDOW as the selected window.
The value returned is the value of the last form in BODY.

This macro saves and restores the selected window, as well as the
selected window of each frame.  It does not change the order of
recently selected windows.  If the previously selected window of
some frame is no longer live at the end of BODY, that frame's
selected window is left alone.  If the selected window is no
longer live, then whatever window is selected at the end of BODY
remains selected.

This macro uses `save-current-buffer' to save and restore the
current buffer, since otherwise its normal operation could
potentially make a different buffer current.  It does not alter
the buffer list ordering."
    ;; Most of this code is a copy of save-selected-window.
    `(let ((save-selected-window-window  (selected-window))
           ;; It is necessary to save all of these, because calling
           ;; select-window changes frame-selected-window for whatever
           ;; frame that window is in.
           (save-selected-window-alist   (mapcar #'(lambda (frame)
                                                     (list frame (frame-selected-window frame)))
                                          (frame-list))))
      (save-current-buffer
        (unwind-protect
             (progn (if (> emacs-major-version 21)
                        (select-window ,window 'norecord) ; Emacs 22+
                      (select-window ,window))
                    ,@body)
          (dolist (elt save-selected-window-alist)
            (and (frame-live-p (car elt))
                 (window-live-p (cadr elt))
                 (if (> emacs-major-version 22)
                     (set-frame-selected-window (car elt) (cadr elt) 'norecord) ; Emacs 23+
                   (set-frame-selected-window (car elt) (cadr elt)))))
          (when (window-live-p save-selected-window-window)
            (if (> emacs-major-version 21)
                (select-window save-selected-window-window 'norecord) ; Emacs 22+
              (select-window save-selected-window-window))))))))

(defmacro icicle-define-add-to-alist-command (command doc-string construct-item-fn alist-var
                                              &optional dont-save)
  "Define COMMAND that adds an item to an alist user option.
Any items with the same key are first removed from the alist.
DOC-STRING is the doc string of COMMAND.
CONSTRUCT-ITEM-FN is a function that constructs the new item.
  It reads user input.
ALIST-VAR is the alist user option.
Optional arg DONT-SAVE non-nil means do not call
`customize-save-variable' to save the updated variable."
  `(defun ,command ()
    ,(concat doc-string "\n\nNote: Any items with the same key are first removed from the alist.")
    (interactive)
    (let ((new-item  (funcall ,construct-item-fn)))
      (setq ,alist-var  (icicle-assoc-delete-all (car new-item) ,alist-var))
      (push new-item ,alist-var)
      ,(unless dont-save `(customize-save-variable ',alist-var ,alist-var))
      (message "Added to `%s': `%S'" ',alist-var new-item))))

(defmacro icicle-buffer-bindings (&optional pre-bindings post-bindings)
  "Bindings to use in multi-command definitions for buffer names.
PRE-BINDINGS is a list of additional bindings, which are created
before the others.  POST-BINDINGS is similar, but the bindings are
created after the others."
  ;; We use `append' rather than backquote syntax (with ,@post-bindings in particular) because of a bug
  ;; in Emacs 20.  This ensures that you can byte-compile in, say, Emacs 20 and still use the result
  ;; in later Emacs releases.
  `,(append
     pre-bindings
     `((completion-ignore-case                      (or (and (boundp 'read-buffer-completion-ignore-case)
                                                         read-buffer-completion-ignore-case)
                                                     completion-ignore-case))
       (icicle-show-Completions-initially-flag      (or icicle-show-Completions-initially-flag
                                                     icicle-buffers-ido-like-flag))
       (icicle-top-level-when-sole-completion-flag  (or icicle-top-level-when-sole-completion-flag
                                                     icicle-buffers-ido-like-flag))
       (icicle-default-value                        (if (and icicle-buffers-ido-like-flag
                                                             icicle-default-value)
                                                        icicle-buffers-ido-like-flag
                                                      icicle-default-value))
       (icicle-must-match-regexp                    icicle-buffer-match-regexp)
       (icicle-must-not-match-regexp                icicle-buffer-no-match-regexp)
       (icicle-must-pass-after-match-predicate      icicle-buffer-predicate)
       (icicle-require-match-flag                   icicle-buffer-require-match-flag)
       (icicle-extra-candidates                     icicle-buffer-extras)
       (icicle-ignore-space-prefix-flag             icicle-buffer-ignore-space-prefix-flag)
       (icicle-delete-candidate-object              'icicle-kill-a-buffer) ; `S-delete' kills current buf
       (icicle-transform-function                   'icicle-remove-dups-if-extras)
       (icicle--temp-orders
        (append (list
                 '("by last access")    ; Renamed from "turned OFF'.
                 '("*...* last" . icicle-buffer-sort-*...*-last)
                 '("by buffer size" . icicle-buffer-smaller-p)
                 '("by major mode name" . icicle-major-mode-name-less-p)
                 (and (fboundp 'icicle-mode-line-name-less-p)
                  '("by mode-line mode name" . icicle-mode-line-name-less-p))
                 '("by file/process name" . icicle-buffer-file/process-name-less-p))
         (delete '("turned OFF") (copy-sequence icicle-sort-orders-alist))))
       ;; Put `icicle-buffer-sort' first.  If already in the list, move it, else add it, to beginning.
       (icicle-sort-orders-alist
        (progn (when (and icicle-buffer-sort-first-time-p icicle-buffer-sort)
                 (setq icicle-sort-comparer             icicle-buffer-sort
                       icicle-buffer-sort-first-time-p  nil))
               (if icicle-buffer-sort
                   (let ((already-there  (rassq icicle-buffer-sort icicle--temp-orders)))
                     (if already-there
                         (cons already-there (setq icicle--temp-orders  (delete already-there
                                                                                icicle--temp-orders)))
                       (cons `("by `icicle-buffer-sort'" . ,icicle-buffer-sort) icicle--temp-orders)))
                 icicle--temp-orders)))
       (icicle-candidate-alt-action-fn
        (or icicle-candidate-alt-action-fn (icicle-alt-act-fn-for-type "buffer")))
       (icicle-all-candidates-list-alt-action-fn
        (or icicle-all-candidates-list-alt-action-fn (icicle-alt-act-fn-for-type "buffer")))
       (icicle-bufflist
        (if current-prefix-arg
            (cond ((zerop (prefix-numeric-value current-prefix-arg))
                   (let ((this-mode  major-mode))
                     (icicle-remove-if-not #'(lambda (bf)
                                               (with-current-buffer bf (eq major-mode this-mode)))
                                           (buffer-list))))
                  ((< (prefix-numeric-value current-prefix-arg) 0)
                   (cdr (assq 'buffer-list (frame-parameters))))
                  (t
                   (icicle-remove-if-not #'(lambda (bf) (buffer-file-name bf)) (buffer-list))))
          (buffer-list))))
     post-bindings))

(defmacro icicle-file-bindings (&optional pre-bindings post-bindings)
  "Bindings to use in multi-command definitions for file names.
PRE-BINDINGS is a list of additional bindings, which are created
before the others.  POST-BINDINGS is similar, but the bindings are
created after the others."
  ;; We use `append' rather than backquote syntax (with ,@post-bindings in particular) because of a bug
  ;; in Emacs 20.  This ensures that you can byte-compile in, say, Emacs 20 and still use the result
  ;; in later Emacs releases.
  `,(append
     pre-bindings
     `((completion-ignore-case
        (or (and (boundp 'read-file-name-completion-ignore-case) read-file-name-completion-ignore-case)
         completion-ignore-case))
       (icicle-show-Completions-initially-flag      (or icicle-show-Completions-initially-flag
                                                     icicle-files-ido-like-flag))
       (icicle-top-level-when-sole-completion-flag  (or icicle-top-level-when-sole-completion-flag
                                                     icicle-files-ido-like-flag))
       (icicle-default-value                        (if (and icicle-files-ido-like-flag
                                                             icicle-default-value)
                                                        icicle-files-ido-like-flag
                                                      ;;  Get default via `M-n', but do not insert it.
                                                      (and (memq icicle-default-value '(t nil))
                                                           icicle-default-value)))
       (icicle-must-match-regexp                    icicle-file-match-regexp)
       (icicle-must-not-match-regexp                icicle-file-no-match-regexp)
       (icicle-must-pass-after-match-predicate      icicle-file-predicate)
       (icicle-require-match-flag                   icicle-file-require-match-flag)
       (icicle-extra-candidates                     icicle-file-extras)
       (icicle-transform-function                   'icicle-remove-dups-if-extras)
       ;; Put `icicle-file-sort' first.  If already in the list, move it, else add it, to beginning.
       (icicle--temp-orders                         (copy-sequence icicle-sort-orders-alist))
       (icicle-sort-orders-alist
        (progn (when (and icicle-file-sort-first-time-p icicle-file-sort)
                 (setq icicle-sort-comparer           icicle-file-sort
                       icicle-file-sort-first-time-p  nil))
               (if icicle-file-sort
                   (let ((already-there  (rassq icicle-file-sort icicle--temp-orders)))
                     (if already-there
                         (cons already-there (setq icicle--temp-orders  (delete already-there
                                                                                icicle--temp-orders)))
                       (cons `("by `icicle-file-sort'" ,@icicle-file-sort) icicle--temp-orders)))
                 icicle--temp-orders)))
       (icicle-candidate-help-fn                    #'(lambda (cand)
                                                        (icicle-describe-file cand current-prefix-arg)))
       (icicle-candidate-alt-action-fn
        (or icicle-candidate-alt-action-fn (icicle-alt-act-fn-for-type "file")))
       (icicle-all-candidates-list-alt-action-fn
        (or icicle-all-candidates-list-alt-action-fn (icicle-alt-act-fn-for-type "file")))
       (icicle-delete-candidate-object              'icicle-delete-file-or-directory))
     post-bindings))

(defmacro icicle-define-command
    (command doc-string function prompt collection &optional
     predicate require-match initial-input hist def inherit-input-method
     bindings first-sexp undo-sexp last-sexp not-interactive-p)
  ;; Hard-code these in doc string, because \\[...] prefers ASCII
  ;; `C-return' instead of `\\[icicle-candidate-action]'
  ;; `C-down'   instead of `\\[icicle-next-candidate-per-mode-action]'
  ;; `C-up', `C-wheel-up' instead of `\\[icicle-previous-candidate-per-mode-action]'
  ;; `C-next'   instead of `\\[icicle-next-apropos-candidate-action]'
  ;; `C-prior'  instead of `\\[icicle-previous-apropos-candidate-action]'
  ;; `C-end'    instead of `\\[icicle-next-prefix-candidate-action]'
  ;; `C-home'   instead of `\\[icicle-previous-prefix-candidate-action]'
  "Define COMMAND with DOC-STRING based on FUNCTION.
COMMAND is a symbol.  DOC-STRING is a string.
FUNCTION is a function that takes one argument, read as input.
  (If the argument to FUNCTION is a file name or directory name, then
  use macro `icicle-define-file-command', instead.)

BINDINGS is a list of `let*' bindings added around the command code.
  The following bindings are pre-included - you can refer to them in
  the command body (including in FIRST-SEXP, LAST-SEXP, UNDO-SEXP).

  `icicle-orig-buff'   is bound to (current-buffer)
  `icicle-orig-window' is bound to (selected-window)
BINDINGS is macroexpanded, so it can also be a macro call that expands
to a list of bindings.  For example, you can use
`icicle-buffer-bindings' here.

In case of user quit (`C-g') or error, an attempt is made to restore
the original buffer.

FIRST-SEXP is a sexp evaluated before the main body of the command.
UNDO-SEXP is a sexp evaluated in case of error or if the user quits.
LAST-SEXP is a sexp evaluated after the main body of the command.
 It is always evaluated, in particular, even in case of error or quit.
NOT-INTERACTIVE-P non-nil means to define COMMAND as a non-interactive
 function that reads multi-command input.

Other arguments are as for `completing-read'.

In order, the created command does this:

 - Uses DOC-STRING, with information about Icicles bindings appended.
 - Binds BINDINGS for the rest of the command.
 - Evaluates FIRST-SEXP.
 - Reads input with `completing-read', using PROMPT, COLLECTION,
   PREDICATE, REQUIRE-MATCH, INITIAL-INPUT, HIST, DEF, and
   INHERIT-INPUT-METHOD.
 - Calls FUNCTION on the input that was read.
 - Evaluates UNDO-SEXP in case of error or if the user quits.
 - Evaluates LAST-SEXP.

The created command also binds `icicle-candidate-action-fn' to a
function that calls FUNCTION on the current completion candidate.
Note that the BINDINGS are of course not in effect within
`icicle-candidate-action-fn'."
  `(defun ,command ()
    ,(concat doc-string "\n\nRead input, then "
             (and (symbolp function) (concat "call `" (symbol-name function) "'\nto "))
             "act on it.

Input-candidate completion and cycling are available.  While cycling,
these keys with prefix `C-' are active:

\\<minibuffer-local-completion-map>\
`C-mouse-2', `C-return' - Act on current completion candidate only
`C-down', `C-wheel-down' - Move to next completion candidate and act
`C-up', `C-wheel-up' - Move to previous completion candidate and act
`C-next'  - Move to next apropos-completion candidate and act
`C-prior' - Move to previous apropos-completion candidate and act
`C-end'   - Move to next prefix-completion candidate and act
`C-home'  - Move to previous prefix-completion candidate and act
`\\[icicle-all-candidates-action]'     - Act on *all* candidates, successively (careful!)

When candidate action and cycling are combined (e.g. `C-next'), user
option `icicle-act-before-cycle-flag' determines which occurs first.

With prefix `C-M-' instead of `C-', the same keys (`C-M-mouse-2',
`C-M-RET', `C-M-down', and so on) provide help about candidates.

Use `mouse-2', `RET', or `S-RET' to finally choose a candidate, or
`C-g' to quit.

This is an Icicles command - see command `icicle-mode'.")
    ,(and (not not-interactive-p) '(interactive))
    (let* ((icicle-orig-buff    (current-buffer))
           (icicle-orig-window  (selected-window))
           ,@(macroexpand bindings)
           (icicle-candidate-action-fn
            (lambda (candidate)
              (let ((minibuffer-completion-table      minibuffer-completion-table)
                    (minibuffer-completion-predicate  minibuffer-completion-predicate)
                    (minibuffer-completion-confirm    minibuffer-completion-confirm)
                    (minibuffer-completing-file-name  minibuffer-completing-file-name)
                    (minibuffer-completing-symbol     (and (boundp 'minibuffer-completing-symbol)
                                                           minibuffer-completing-symbol))
                    (minibuffer-exit-hook             minibuffer-exit-hook)
                    (minibuffer-help-form             minibuffer-help-form)
                    (minibuffer-history-variable      minibuffer-history-variable)
                    (minibuffer-history-case-insensitive-variables
                     minibuffer-history-case-insensitive-variables)
                    (minibuffer-history-sexp-flag     minibuffer-history-sexp-flag)
                    (minibuffer-message-timeout       (and (boundp 'minibuffer-message-timeout)
                                                           minibuffer-message-timeout))
                    (minibuffer-prompt-properties     (and (boundp 'minibuffer-prompt-properties)
                                                           minibuffer-prompt-properties))
                    (minibuffer-setup-hook            minibuffer-setup-hook)
                    (minibuffer-text-before-history   minibuffer-text-before-history))
                (icicle-condition-case-no-debug in-action-fn
                    ;; Treat 3 cases, because previous use of `icicle-candidate-action-fn'
                    ;; might have killed the buffer or deleted the window.
                    (cond ((and (buffer-live-p icicle-orig-buff) (window-live-p icicle-orig-window))
                           (with-current-buffer icicle-orig-buff
                             (save-selected-window (select-window icicle-orig-window)
                                                   (funcall #',function candidate))))
                          ((window-live-p icicle-orig-window)
                           (save-selected-window (select-window icicle-orig-window)
                                                 (funcall #',function candidate)))
                          (t
                           (funcall #',function candidate)))
                  (error (unless (string= "Cannot switch buffers in minibuffer window"
                                          (error-message-string in-action-fn))
                           (error "%s" (error-message-string in-action-fn)))
                         (when (window-live-p icicle-orig-window)
                           (select-window icicle-orig-window)
                           (select-frame-set-input-focus (selected-frame)))
                         (funcall #',function candidate)))
                (select-window (minibuffer-window))
                (select-frame-set-input-focus (selected-frame))
                nil))))                 ; Return nil for success.
      ,first-sexp
      (icicle-condition-case-no-debug act-on-choice
          (let ((cmd-choice  (completing-read ,prompt ,collection ,predicate ,require-match
                                              ,initial-input ,hist ,def ,inherit-input-method)))
            ;; Reset after reading input, so that commands can tell whether input has been read.
            (setq icicle-candidate-action-fn  nil)
            (funcall #',function cmd-choice))
        (quit  (icicle-try-switch-buffer icicle-orig-buff) ,undo-sexp)
        (error (icicle-try-switch-buffer icicle-orig-buff) ,undo-sexp
               (error "%s" (error-message-string act-on-choice))))
      ,last-sexp)))

(defmacro icicle-define-file-command
    (command doc-string function prompt &optional
     dir default-filename require-match initial-input predicate
     bindings first-sexp undo-sexp last-sexp not-interactive-p)
  ;; Hard-code these in doc string, because \\[...] prefers ASCII
  ;; `C-return' instead of `\\[icicle-candidate-action]'
  ;; `C-down'   instead of `\\[icicle-next-candidate-per-mode-action]'
  ;; `C-up', `C-wheel-up' instead of `\\[icicle-previous-candidate-per-mode-action]'
  ;; `C-next'   instead of `\\[icicle-next-apropos-candidate-action]'
  ;; `C-prior'  instead of `\\[icicle-previous-apropos-candidate-action]'
  ;; `C-end'    instead of `\\[icicle-next-prefix-candidate-action]'
  ;; `C-home'   instead of `\\[icicle-previous-prefix-candidate-action]'
  "Define COMMAND with DOC-STRING based on FUNCTION.
COMMAND is a symbol.  DOC-STRING is a string.
FUNCTION is a function that takes one file-name or directory-name
argument, read as input.  (Use macro `icicle-define-command' for a
FUNCTION whose argument is not a file or directory name.)

BINDINGS is a list of `let*' bindings added around the command code.
  The following bindings are pre-included - you can refer to them in
  the command body (including in FIRST-SEXP, LAST-SEXP, UNDO-SEXP).

  `icicle-orig-buff'   is bound to (current-buffer)
  `icicle-orig-window' is bound to (selected-window)
BINDINGS is macroexpanded, so it can also be a macro call that expands
to a list of bindings.  For example, you can use
`icicle-buffer-bindings' or `icicle-file-bindings' here.

In case of user quit (`C-g') or error, an attempt is made to restore
the original buffer.

FIRST-SEXP is a sexp evaluated before the main body of the command.
UNDO-SEXP is a sexp evaluated in case of error or if the user quits.
LAST-SEXP is a sexp evaluated after the main body of the command.
 It is always evaluated, in particular, even in case of error or quit.
NOT-INTERACTIVE-P non-nil means to define COMMAND as a non-interactive
 function that reads multi-command input.

Other arguments are as for `read-file-name'.

In order, the created command does this:

 - Uses DOC-STRING, with information about Icicles bindings appended.
 - Binds BINDINGS for the rest of the command.
 - Evaluates FIRST-SEXP.
 - Reads input with `read-file-name', using PROMPT, DIR,
   DEFAULT-FILENAME, REQUIRE-MATCH, INITIAL-INPUT, and PREDICATE.
 - Calls FUNCTION on the input that was read.
 - Evaluates UNDO-SEXP in case of error or if the user quits.
 - Evaluates LAST-SEXP.

The created command also binds `icicle-candidate-action-fn' to a
function that calls FUNCTION on the current completion candidate.
Note that the BINDINGS are of course not in effect within
`icicle-candidate-action-fn'."
  `(defun ,command ()
    ,(concat doc-string "\n\nRead input, then "
             (and (symbolp function) (concat "call `" (symbol-name function) "'\nto "))
             "act on it.

Input-candidate completion and cycling are available.  While cycling,
these keys with prefix `C-' are active:

\\<minibuffer-local-completion-map>\
`C-mouse-2', `C-return' - Act on current completion candidate only
`C-down', `C-wheel-down' - Move to next completion candidate and act
`C-up', `C-wheel-up' - Move to previous completion candidate and act
`C-next'  - Move to next apropos-completion candidate and act
`C-prior' - Move to previous apropos-completion candidate and act
`C-end'   - Move to next prefix-completion candidate and act
`C-home'  - Move to previous prefix-completion candidate and act
`\\[icicle-all-candidates-action]'     - Act on *all* candidates, successively (careful!)

When candidate action and cycling are combined (e.g. `C-next'), user
option `icicle-act-before-cycle-flag' determines which occurs first.

With prefix `C-M-' instead of `C-', the same keys (`C-M-mouse-2',
`C-M-RET', `C-M-down', and so on) provide help about candidates.

Use `mouse-2', `RET', or `S-RET' to finally choose a candidate, or
`C-g' to quit.

This is an Icicles command - see command `icicle-mode'.")
    ,(and (not not-interactive-p) '(interactive))
    (let* ((icicle-orig-buff    (current-buffer))
           (icicle-orig-window  (selected-window))
           ,@(macroexpand bindings)
           (icicle-candidate-action-fn
            (lambda (candidate)
              (let ((minibuffer-completion-table      minibuffer-completion-table)
                    (minibuffer-completion-predicate  minibuffer-completion-predicate)
                    (minibuffer-completion-confirm    minibuffer-completion-confirm)
                    (minibuffer-completing-file-name  minibuffer-completing-file-name)
                    (minibuffer-completing-symbol     (and (boundp 'minibuffer-completing-symbol)
                                                           minibuffer-completing-symbol))
                    (minibuffer-exit-hook             minibuffer-exit-hook)
                    (minibuffer-help-form             minibuffer-help-form)
                    (minibuffer-history-variable      minibuffer-history-variable)
                    (minibuffer-history-case-insensitive-variables
                     minibuffer-history-case-insensitive-variables)
                    (minibuffer-history-sexp-flag     minibuffer-history-sexp-flag)
                    (minibuffer-message-timeout       (and (boundp 'minibuffer-message-timeout)
                                                           minibuffer-message-timeout))
                    (minibuffer-prompt-properties     (and (boundp 'minibuffer-prompt-properties)
                                                           minibuffer-prompt-properties))
                    (minibuffer-setup-hook            minibuffer-setup-hook)
                    (minibuffer-text-before-history   minibuffer-text-before-history))
                (setq candidate  (expand-file-name candidate (icicle-file-name-directory
                                                              icicle-last-input)))
                (icicle-condition-case-no-debug in-action-fn
                    ;; Treat 3 cases, because previous use of `icicle-candidate-action-fn'
                    ;; might have deleted the file or the window.
                    (cond ((and (buffer-live-p icicle-orig-buff) (window-live-p icicle-orig-window))
                           (with-current-buffer icicle-orig-buff
                             (save-selected-window (select-window icicle-orig-window)
                                                   (funcall #',function candidate))))
                          ((window-live-p icicle-orig-window)
                           (save-selected-window (select-window icicle-orig-window)
                                                 (funcall #',function candidate)))
                          (t
                           (funcall #',function candidate)))
                  (error (unless (string= "Cannot switch buffers in minibuffer window"
                                          (error-message-string in-action-fn))
                           (error "%s" (error-message-string in-action-fn)))
                         (when (window-live-p icicle-orig-window)
                           (select-window icicle-orig-window)
                           (select-frame-set-input-focus (selected-frame)))
                         (funcall #',function candidate)))
                (select-window (minibuffer-window))
                (select-frame-set-input-focus (selected-frame))
                nil))))                 ; Return nil for success.
      ,first-sexp
      (icicle-condition-case-no-debug act-on-choice
          (let ((file-choice
                 (if (< emacs-major-version 21) ; No predicate arg for Emacs 20.
                     (read-file-name ,prompt ,dir ,default-filename ,require-match ,initial-input)
                   (read-file-name ,prompt ,dir ,default-filename ,require-match
                                   ,initial-input ,predicate))))
            ;; Reset after reading input, so that commands can tell whether input has been read.
            (setq icicle-candidate-action-fn  nil) ; Reset after completion.
            (funcall #',function file-choice))
        (quit  (icicle-try-switch-buffer icicle-orig-buff) ,undo-sexp)
        (error (icicle-try-switch-buffer icicle-orig-buff) ,undo-sexp
               (error "%s" (error-message-string act-on-choice))))
      ,last-sexp)))

;; Same definition as in `icicles-fn.el'.
(defun icicle-assoc-delete-all (key alist)
  "Delete from ALIST all elements whose car is `equal' to KEY.
Return the modified alist.
Elements of ALIST that are not conses are ignored."
  (while (and (consp (car alist)) (equal (car (car alist)) key))
    (setq alist  (cdr alist)))
  (let ((tail  alist)
        tail-cdr)
    (while (setq tail-cdr  (cdr tail))
      (if (and (consp (car tail-cdr))  (equal (car (car tail-cdr)) key))
          (setcdr tail (cdr tail-cdr))
        (setq tail  tail-cdr))))
  alist)

(defmacro icicle-define-sort-command (sort-order comparison-fn doc-string)
  "Define a command to sort completions by SORT-ORDER.
SORT-ORDER is a short string (or symbol) describing the sort order.
 It is used after the phrase \"Sorting is now \".  Examples: \"by date\",
 \"alphabetically\", \"directories first\", and \"previously used first\".

The new command is named by replacing any spaces in SORT-ORDER with
hyphens (`-') and then adding the prefix `icicle-sort-'.

COMPARISON-FN is a function that compares two strings, returning
 non-nil if and only if the first string sorts before the second.

DOC-STRING is the doc string of the new command."
  (unless (stringp sort-order) (setq sort-order  (symbol-name sort-order)))
  (let ((command  (intern (concat "icicle-sort-" (replace-regexp-in-string "\\s-+" "-" sort-order)))))
    `(progn
      (setq icicle-sort-orders-alist  (icicle-assoc-delete-all ,sort-order icicle-sort-orders-alist))
      (push (cons ,sort-order ',comparison-fn) icicle-sort-orders-alist)
      (defun ,command ()
        ,doc-string
        (interactive)
        (setq icicle-sort-comparer  #',comparison-fn)
        (message "Sorting is now %s%s" ,(icicle-propertize
                                         (concat sort-order (and icicle-reverse-sort-p ", REVERSED"))
                                                           'face 'icicle-msg-emphasis))
        (icicle-complete-again-update)))))

(defmacro icicle-define-bookmark-command (type &optional prompt &rest args)
  "Define an Icicles multi-command for jumping to bookmarks of type TYPE.
TYPE is a string to be used for the doc string, default prompt, and in
 function names.  It should be lowercase and contain no spaces.
Optional arg PROMPT is the completion prompt.
ARGS is a list of any additional arguments to be passed to the
appropriate `bmkp-TYPE-alist-only' function."
  `(icicle-define-bookmark-command-1 nil ,type ,prompt ,args))

(defmacro icicle-define-bookmark-other-window-command (type &optional prompt &rest args)
  "Same as `icicle-define-bookmark-command', but command uses other window."
  `(icicle-define-bookmark-command-1 t ,type ,prompt ,args))

;; Similar to `icicle-define-search-bookmark-command'.  Could combine them.
(defmacro icicle-define-bookmark-command-1 (otherp type prompt args)
  "Helper macro for `icicle-define*-bookmark-command' macros.
The command defined raises an error unless library `Bookmark+' can be
loaded."
  `(icicle-define-command
    ,(intern (format "icicle-bookmark-%s%s" type (if otherp "-other-window" ""))) ; Command name
    ,(format "Jump to a %s bookmark%s.
Like `icicle-bookmark%s',
 but with %s bookmarks only.
This is a multi-command version of
 `bmkp-%s-jump%s'.
You need library `Bookmark+' for this command."
             type (if otherp " in other window" "")
             (if otherp "-other-window" "") type
             type (if otherp "-other-window" "")) ; Doc string
    (lambda (cand) (,(if otherp 'icicle-bookmark-jump-other-window 'icicle-bookmark-jump) ; Action fn.
                     (icicle-transform-multi-completion cand)))
    prompt1 icicle-candidates-alist nil ; `completing-read' args
    (not icicle-show-multi-completion-flag)
    nil (if (boundp 'bookmark-history) 'bookmark-history 'icicle-bookmark-history)
    nil nil
    ((IGNORED1                               (unless (require 'bookmark+ nil t) ; Additional bindings
                                               (error "You need library `Bookmark+' for this command")))
     (IGNORED2                               (bookmark-maybe-load-default-file)) ; `bookmark-alist'.
     (enable-recursive-minibuffers           t) ; In case we read input, e.g. File changed on disk...
     (completion-ignore-case                 bookmark-completion-ignore-case)
     (prompt1                                ,(or prompt
                                                  (format "%s%s bookmark: "
                                                          (capitalize (substring type 0 1))
                                                          (substring type 1 (length type)))))
     (icicle-list-use-nth-parts              '(1))
     (icicle-candidate-properties-alist      (if (not icicle-show-multi-completion-flag)
                                                 nil
                                               (if (facep 'file-name-shadow)
                                                   '((2 (face file-name-shadow))
                                                     (3 (face bookmark-menu-heading)))
                                                 '((3 (face bookmark-menu-heading))))))
     (icicle-transform-function              (if (interactive-p) nil icicle-transform-function))
     (icicle-whole-candidate-as-text-prop-p  t)
     (icicle-transform-before-sort-p         t)
     (icicle-delete-candidate-object         'icicle-bookmark-delete-action)
     (icicle-sort-orders-alist
      (append
       '(("in *Bookmark List* order")	; Renamed from "turned OFF'.
         ("by bookmark name" . icicle-alpha-p)
         ("by last bookmark access" (bmkp-bookmark-last-access-cp) icicle-alpha-p)
         ("by bookmark visit frequency" (bmkp-visited-more-cp) icicle-alpha-p))
       (and (member ,type '("info" "region"))
        '(("by Info location" (bmkp-info-cp) icicle-alpha-p)))
       (and (member ,type '("gnus" "region"))
        '(("by Gnus thread" (bmkp-gnus-cp) icicle-alpha-p)))
       (and (member ,type '("url" "region"))
        '(("by URL" (bmkp-url-cp) icicle-alpha-p)))
       (and (not (member ,type '("bookmark-list" "desktop" "gnus" "info" "man" "url")))
        '(("by bookmark type" (bmkp-info-cp bmkp-url-cp bmkp-gnus-cp
                               bmkp-local-file-type-cp bmkp-handler-cp)
           icicle-alpha-p)))
       (and (not (member ,type '("bookmark-list" "desktop" "dired" "non-file")))
        '(("by file name" (bmkp-file-alpha-cp) icicle-alpha-p)))
       (and (member ,type '("local-file" "file" "dired" "region"))
        '(("by local file type" (bmkp-local-file-type-cp) icicle-alpha-p)
          ("by local file size" (bmkp-local-file-size-cp) icicle-alpha-p)
          ("by last local file access" (bmkp-local-file-accessed-more-recently-cp)
           icicle-alpha-p)
          ("by last local file update" (bmkp-local-file-updated-more-recently-cp)
           icicle-alpha-p)))
       (and (not (string= ,type "desktop"))
        '(("by last buffer or file access" (bmkp-buffer-last-access-cp
                                            bmkp-local-file-accessed-more-recently-cp)
           icicle-alpha-p)))
       (and (get-buffer "*Bookmark List*")
        '(("marked before unmarked (in *Bookmark List*)" (bmkp-marked-cp)
           icicle-alpha-p)))
       '(("by previous use alphabetically" . icicle-historical-alphabetic-p)
         ("case insensitive" . icicle-case-insensitive-string-less-p))))
     (icicle-candidate-help-fn
      #'(lambda (cand)
          (when icicle-show-multi-completion-flag
            (setq cand  (funcall icicle-get-alist-candidate-function cand))
            (setq cand  (cons (caar cand) (cdr cand))))
          (if current-prefix-arg
              (bmkp-describe-bookmark-internals cand)
            (bmkp-describe-bookmark cand))))
     (icicle-candidates-alist
      (mapcar (if icicle-show-multi-completion-flag
                  #'(lambda (bmk)
                      ;; Ignore errors, e.g. from bad or stale bookmark records.
                      (icicle-condition-case-no-debug nil
                          (let* ((bname     (bookmark-name-from-full-record bmk))
                                 (guts      (bookmark-get-bookmark-record bmk))
                                 (file      (bookmark-get-filename bmk))
                                 (buf       (bmkp-get-buffer-name bmk))
                                 (file/buf  (if (and buf (equal file bmkp-non-file-filename))
                                                buf
                                              file))
                                 (tags      (bmkp-get-tags bmk)))
                            ;; Emacs 20 byte-compiler bug prevents using backslash syntax here.
                            (cons (append (list (icicle-candidate-short-help
                                                 (icicle-bookmark-help-string bname)
                                                 (icicle-bookmark-propertize-candidate bname))
                                                file/buf)
                                          (and tags (list (format "%S" tags))))
                                  guts))
                        (error nil)))
                #'(lambda (bmk)
                    ;; Ignore errors, e.g. from bad or stale bookmark records.
                    (icicle-condition-case-no-debug nil
                        (let ((bname  (bookmark-name-from-full-record bmk))
                              (guts   (bookmark-get-bookmark-record bmk)))
                          (cons (icicle-candidate-short-help
                                 (icicle-bookmark-help-string bname)
                                 (icicle-bookmark-propertize-candidate bname))
                                guts))
                      (error nil))))
       (bmkp-sort-omit (funcall ',(intern (format "bmkp-%s-alist-only" type)) ,@args)))))
    nil                                 ; First code
    (icicle-bookmark-cleanup-on-quit)	; Undo code
    (icicle-bookmark-cleanup)))         ; Last code

;; Similar to `icicle-define-bookmark-command-1'.  Could combine them.
(defmacro icicle-define-search-bookmark-command (type &optional prompt &rest args)
  "Define Icicles multi-command for searching bookmarks of type TYPE.
TYPE is a string to be used for the doc string, default prompt, and in
 function names.  It should be lowercase and contain no spaces.
Optional arg PROMPT is the completion prompt.
ARGS is a list of any additional arguments to be passed to the
 appropriate `bmkp-TYPE-alist-only' function.

The command defined raises an error unless library `Bookmark+' can be
loaded."
  `(icicle-define-command
    ,(intern (format "icicle-search-%s-bookmark" type)) ; Command name
    ,(format "Search %s bookmark text.
Like `icicle-search-bookmark', but with %s bookmarks only.
You need library `Bookmark+' for this command." type type) ; Doc string
    icicle-search-bookmark-action       ; Action function
    prompt1 icicle-candidates-alist nil ; `completing-read' args
    (not icicle-show-multi-completion-flag)
    nil (if (boundp 'bookmark-history) 'bookmark-history 'icicle-bookmark-history)
    nil nil
    ((IGNORED1                                 (unless (require 'bookmark+ nil t) ; Bindings
                                                 (error "You need library `Bookmark+' for this \
command")))
     (IGNORED2                                 (bookmark-maybe-load-default-file)) ; `bookmark-alist'.
     (enable-recursive-minibuffers             t) ; In case we read input, e.g. File changed on...
     (completion-ignore-case                   bookmark-completion-ignore-case)
     (prompt1                                  ,(or prompt (format "Search %s bookmark: " type)))
     (icicle-list-use-nth-parts                '(1))
     (icicle-candidate-properties-alist        (if (not icicle-show-multi-completion-flag)
                                                   nil
                                                 (if (facep 'file-name-shadow)
                                                     '((2 (face file-name-shadow))
                                                       (3 (face bookmark-menu-heading)))
                                                   '((3 (face bookmark-menu-heading))))))
     (icicle-transform-function                (if (interactive-p) nil icicle-transform-function))
     (icicle-whole-candidate-as-text-prop-p    t)
     (icicle-transform-before-sort-p           t)
     (icicle-delete-candidate-object           'icicle-bookmark-delete-action)
     (regexp                                   (icicle-search-read-context-regexp))
     (bookmark-automatically-show-annotations  nil) ; Do not show annotations
     (icicle-sort-orders-alist
      (append
       '(("in *Bookmark List* order")   ; Renamed from "turned OFF'.
         ("by bookmark name" . icicle-alpha-p)
         ("by last bookmark access" (bmkp-bookmark-last-access-cp) icicle-alpha-p)
         ("by bookmark visit frequency" (bmkp-visited-more-cp) icicle-alpha-p))
       (and (member ,type '("info" "region"))
        '(("by Info location" (bmkp-info-cp) icicle-alpha-p)))
       (and (member ,type '("gnus" "region"))
        '(("by Gnus thread" (bmkp-gnus-cp) icicle-alpha-p)))
       (and (member ,type '("url" "region"))
        '(("by URL" (bmkp-url-cp) icicle-alpha-p)))
       (and (not (member ,type '("bookmark-list" "desktop" "gnus" "info" "man" "url")))
        '(("by bookmark type" (bmkp-info-cp bmkp-url-cp bmkp-gnus-cp
                               bmkp-local-file-type-cp bmkp-handler-cp)
           icicle-alpha-p)))
       (and (not (member ,type '("bookmark-list" "desktop" "dired" "non-file")))
        '(("by file name" (bmkp-file-alpha-cp) icicle-alpha-p)))
       (and (member ,type '("local-file" "file" "dired" "region"))
        '(("by local file type" (bmkp-local-file-type-cp) icicle-alpha-p)
          ("by local file size" (bmkp-local-file-size-cp) icicle-alpha-p)
          ("by last local file access" (bmkp-local-file-accessed-more-recently-cp)
           icicle-alpha-p)
          ("by last local file update" (bmkp-local-file-updated-more-recently-cp)
           icicle-alpha-p)))
       (and (not (string= ,type "desktop"))
        '(("by last buffer or file access" (bmkp-buffer-last-access-cp
                                            bmkp-local-file-accessed-more-recently-cp)
           icicle-alpha-p)))
       (and (get-buffer "*Bookmark List*")
        '(("marked before unmarked (in *Bookmark List*)" (bmkp-marked-cp)
           icicle-alpha-p)))
       '(("by previous use alphabetically" . icicle-historical-alphabetic-p)
         ("case insensitive" . icicle-case-insensitive-string-less-p))))
     (icicle-candidate-help-fn
      #'(lambda (cand)
          (when icicle-show-multi-completion-flag
            (setq cand  (funcall icicle-get-alist-candidate-function cand))
            (setq cand  (cons (caar cand) (cdr cand))))
          (if current-prefix-arg
              (bmkp-describe-bookmark-internals cand)
            (bmkp-describe-bookmark cand))))
     (icicle-candidates-alist
      (mapcar (if icicle-show-multi-completion-flag
                  #'(lambda (bmk)
                      ;; Ignore errors, e.g. from bad or stale bookmark records.
                      (icicle-condition-case-no-debug nil
                          (let* ((bname     (bookmark-name-from-full-record bmk))
                                 (guts      (bookmark-get-bookmark-record bmk))
                                 (file      (bookmark-get-filename bmk))
                                 (buf       (bmkp-get-buffer-name bmk))
                                 (file/buf  (if (and buf (equal file bmkp-non-file-filename))
                                                buf
                                              file))
                                 (tags      (bmkp-get-tags bmk)))
                            ;; Emacs 20 byte-compiler bug prevents using backslash syntax here.
                            (cons (append (list (icicle-candidate-short-help
                                                 (icicle-bookmark-help-string bname)
                                                 (icicle-bookmark-propertize-candidate bname))
                                                file/buf)
                                          (and tags (list (format "%S" tags))))
                                  guts))
                        (error nil)))
                #'(lambda (bmk)
                    ;; Ignore errors, e.g. from bad or stale bookmark records.
                    (icicle-condition-case-no-debug nil
                        (let ((bname  (bookmark-name-from-full-record bmk))
                              (guts   (bookmark-get-bookmark-record bmk)))
                          (cons (icicle-candidate-short-help
                                 (icicle-bookmark-help-string bname)
                                 (icicle-bookmark-propertize-candidate bname))
                                guts))
                      (error nil))))
       (bmkp-sort-omit (funcall ',(intern (format "bmkp-%s-alist-only" type)) ,@args)))))
    nil                                 ; First code
    (icicle-bookmark-cleanup-on-quit)   ; Undo code
    (icicle-bookmark-cleanup)))         ; Last code

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'icicles-mac)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; icicles-mac.el ends here
