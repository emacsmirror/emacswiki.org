;;; scheme48.el --- A major mode for Scheme48 development

;; Copyright (C) 1992  Jonathan Rees
;; Copyright (C) 2005, 2006  Jorgen Schaefer

;; Version: 9
;; Author: Jonathan Rees (cmuscheme48.el)
;;         Jorgen Schaefer <forcer@forcix.cx> (scheme48-mode)
;; URL: http://www.emacswiki.org/cgi-bin/emacs/Scheme48Mode

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 3. The name of the authors may not be used to endorse or promote products
;;    derived from this software without specific prior written permission.

;; THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND ANY EXPRESS OR
;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
;; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
;; IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Commentary:

;; This file provides `scheme48-mode', a major mode for improved
;; interaction with Scheme48. It's the same as the canonical
;; `scheme-mode', but provides some commands which tell Scheme48 from
;; which a specific definition came from. This allows Scheme48 to put
;; the definition in the correct package by itself.

;; This is based om the cmuscheme48.el which comes with Scheme48.

;; You can set a buffer-local variable named `scheme48-package' to
;; send definitions to that package. This can be done by file
;; variables, so the following works:

;;   -*- mode: scheme48; scheme48-package: mypackage -*-

;; To use the special packages CONFIG, USER and EXEC, use the package
;; name in parens, like this:

;;   -*- mode: scheme48; scheme48-package: (exec) -*-

;;; Thanks:

;; Thanks to Taylor Campbell (Riastradh on irc.freenode.net) for his
;; extensive list of indentation settings and the idea with the
;; scheme48-package local variable.

;; Thanks to Emilio Lopes for the idea to highlight the new keywords
;; as well.

;;; Code:

(require 'cmuscheme)
(require 'scheme)

(defcustom scheme48-compatibility-bindings-p nil
  "Use the compatbility bindings?
The old cmuscheme48.el provided a few non-standard bindings,
which can be re-enabled by setting this variable to a non-nil
value before loading this file."
  :group 'scheme
  :type 'boolean)

(defcustom scheme48-keywords
  '(;; R5RS
    (dynamic-wind 0)

    ;; Scheme48
    (destructure 1)
    (enum-case 2)
    (environment-define! 2 no-font-lock)
    (environment-set! 2 no-font-lock)
    (guard 1)
    (iterate 3)
    (make-usual-resumer 2 no-font-lock)
    (mvlet 1)
    (mvlet* 1)
    (search-tree-modify! 2 no-font-lock)
    (usual-resumer 0 no-font-lock)
    (with-exception-handler 1)
    (with-handler 1)
    (with-interaction-environment 1)
    (with-nondeterminism 0)

    ;; I/O-related
    (call-with-current-input-port 1)
    (call-with-current-noise-port 1)
    (call-with-current-output-port 1)
    (call-with-string-output-port 0)
    (limit-output 2 no-font-lock)
    (recurring-write 2 no-font-lock)
    (silently 0)
    (with-current-ports 3)

    ;; Configuration language
    (define-interface 1)
    (define-structure 2)
    (structure 1)
    (structures 1)
    ;; These don't improve (for some, even degrade) the readability.
    ;; (modify 1 no-font-lock)
    ;; (subset 1 no-font-lock)

    ;; Concurrency-related
    (atomically 0)
    (atomically! 0)
    (call-ensuring-atomicity 0)
    (call-ensuring-atomicity! 0)
    (ensure-atomicity 0)
    (ensure-atomicity! 0)
    (interrupt-thread 1 no-font-lock)
    (let-fluid 2)
    (let-fluids defun)
    (spawn-on-scheduler 1 no-font-lock)
    (with-new-proposal 1)

    ;; SCSH
    (with-current-input-port 2)
    (with-current-output-port 2)
    (awk 3)
    (close-after 2 no-font-lock)
    (if-match 2)
    (with-cwd 1)
    (with-cwd* 1)

    ;; Others
    (let-optionals scheme-let-indent)
    (let-optionals* scheme-let-indent)

    ;; SRFI-2
    (and-let* 1)

    ;; SRFI-8
    (receive 2)

    ;; SRFI-11
    (let-values 1)
    (let*-values 1)
    )
  "A list of Scheme48-related keywords.
The list consists of lists of the form (KEYWORD INDENT [NO-FONT-LOCK].
The keywords named KEYWORD will be indented according to INDENT,
and will also be highlighted as keywords unless NO-FONT-LOCK is
non-nil."
  :group 'scheme
  :type '(repeat (list symbol sexp boolean)))

(defvar scheme48-package nil
  "The name of the package definitions from this file should go to.")
(make-variable-buffer-local 'scheme48-package)

(put 'scheme48-package 'safe-local-variable 'scheme48-safe-variable)

(defun scheme48-safe-variable (var)
  "Return non-nil when VAR is a valid value of `scheme48-package'."
  (or (symbolp var)
      (stringp var)
      (and (consp var)
           (or (and (null (cdr var))
                    (memq (car var)
                          '(config user exec)))
               (and (null (cddr var))
                    (eq 'for-syntax (car var))
                    (or (symbolp (cadr var))
                        (stringp (cadr var))))))))

(defvar scheme48-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\M-\C-x" 'scheme48-send-definition) ;gnu convention
    (define-key map "\C-x\C-e" 'scheme48-send-last-sexp) ;gnu convention
    (define-key map "\C-c\C-e" 'scheme48-send-definition)
    (define-key map "\C-c\M-e" 'scheme48-send-definition-and-go)
    (define-key map "\C-c\C-r" 'scheme48-send-region)
    (define-key map "\C-c\M-r" 'scheme48-send-region-and-go)
    (define-key map "\C-c\C-l" 'scheme48-load-file)
    (when scheme48-compatibility-bindings-p
      (define-key map "\C-ce"    'scheme48-send-definition)
      (define-key map "\C-c\C-e" 'scheme48-send-definition-and-go)
      (define-key map "\C-cr"    'scheme48-send-region)
      (define-key map "\C-c\C-r" 'scheme48-send-region-and-go)
      (define-key map "\C-cl"    'scheme48-load-file))
    map)
  "The keymap used in `scheme48-mode'.")

(define-derived-mode scheme48-mode scheme-mode "Scheme48"
  "Major mode for improved Scheme48 interaction.
This mode is derived from `scheme-mode', so see there for
information.

The commands that send code to the Scheme48 process attach
information as to from which file the code comes from. This
allows Scheme48 to put the corresponding definitions in the
package associated with that file name.

\\{scheme48-mode-map}"
  (scheme48-initialize)
  (set (make-local-variable 'scheme-trace-command)
       ",trace %s")
  (set (make-local-variable 'scheme-untrace-command)
       ",untrace %s")
  (set (make-local-variable 'scheme-macro-expand-command)
       ",expand %s")
  (when (boundp 'package)
    (message "The `package' local variable is deprecated. Use `scheme48-package' instead.")
    (when (not scheme48-package)
      (setq scheme48-package package))))

(defvar scheme48-mode-initialized-p nil
  "This is non-nil when `scheme48-mode' has been initialized.
Set it to nil if you want the next invocation of `scheme48-mode'
to re-read `scheme48-keywords'.")

(defun scheme48-initialize ()
  "Initialize `scheme48-mode' from `scheme48-keywords'.
Only run when `scheme48-mode-initialized-p' is nil.
This is done so that the user can modify `scheme48-keywords'
before the first time the mode is run, but after this package has
been loaded."
  (when (not scheme48-mode-initialized-p)
    (mapc (lambda (entry)
            (put (car entry)
                 'scheme-indent-function
                 (cadr entry))
            (put (intern (upcase (symbol-name (car entry))))
                 'scheme-indent-function
                 (cadr entry)))
          scheme48-keywords)
    (let ((regexp (concat "("
                          (regexp-opt
                           (delete nil
                                   (mapcar (lambda (elt)
                                             (if (nth 2 elt)
                                                 nil
                                               (symbol-name (car elt))))
                                           scheme48-keywords))
                           t)
                          "\\>")))
      (font-lock-add-keywords 'scheme48-mode
                              (list (list regexp 1 'font-lock-keyword-face))))
    (setq scheme48-mode-initialized-p t)))

(defun scheme48-send-region (start end)
  "Send the current region to the inferior Scheme process."
  (interactive "r")
  (cond
   (scheme48-package
    (scheme48-send-with-prefix (scheme48-package-sender scheme48-package)
                               start
                               end))
   ((buffer-file-name (current-buffer))
    (comint-send-string (scheme-proc)
                        (concat ",from-file "
                                (scheme48-enough-scheme-file-name
                                 (buffer-file-name (current-buffer)))
                                "\n"))
    (comint-send-region (scheme-proc) start end)
    (comint-send-string (scheme-proc) " ,end\n"))
   (t
    (comint-send-region (scheme-proc) start end)
    (comint-send-string (scheme-proc) "\n"))))

(defun scheme48-send-with-prefix (prefix start end)
  "Send all Scheme definitions in the region to the Scheme process."
  (let ((region (buffer-substring-no-properties start end))
        (p prefix)) ; Emacs lossage - prefix is suddenly nil below
    (with-temp-buffer
      (insert region "\n")
      ;; `backward-sexp' relies on this. Thanks to Riastradh for
      ;; finding it out :-)
      (set-syntax-table scheme-mode-syntax-table)
      (set (make-local-variable 'parse-sexp-ignore-comments) t)
      ;; Add prefix
      (while (> (point)
                (progn (backward-sexp)
                       (point)))
        (save-excursion
          (insert "\n" p " ")))
      (comint-send-region (scheme-proc)
                          (point-min)
                          (point-max)))))

(defun scheme48-send-definition ()
  "Send the current definition to the inferior Scheme48 process."
  (interactive)
  (save-excursion
    (end-of-defun)
    (let ((end (point)))
      (beginning-of-defun)
      (scheme48-send-region (point) end))))

(defun scheme48-send-last-sexp ()
  "Send the previous sexp to the inferior Scheme process."
  (interactive)
  (scheme48-send-region (save-excursion (backward-sexp) (point)) (point)))

(defun scheme48-send-region-and-go (start end)
  "Send the current region to the inferior Scheme48 process,
and switch to the process buffer."
  (interactive "r")
  (scheme48-send-region start end)
  (switch-to-scheme t))

(defun scheme48-send-definition-and-go ()
  "Send the current definition to the inferior Scheme48,
and switch to the process buffer."
  (interactive)
  (scheme48-send-definition)
  (switch-to-scheme t))

(defun scheme48-load-file (file-name)
  "Load a Scheme file into the inferior Scheme48 process."
  (interactive (comint-get-source "Load Scheme48 file: "
				  scheme-prev-l/c-dir/file
				  scheme-source-modes t)) ; T because LOAD
                                                          ; needs an exact name
  (comint-check-source file-name) ; Check to see if buffer needs saved.
  (setq scheme-prev-l/c-dir/file (cons (file-name-directory    file-name)
				       (file-name-nondirectory file-name)))
  (comint-send-string
   (scheme-proc)
   (concat (if scheme48-package
               (concat (scheme48-package-sender scheme48-package)
                       " ")
             "")
           ",load "
           (scheme48-enough-scheme-file-name file-name)
           "\n")))

(defun scheme48-package-sender (package)
  "Return the prefix to send a definition to PACKAGE."
  (cond
   ((equal package '(config))
    ",config")
   ((equal package '(user))
    ",user")
   ((equal package '(exec))
    ",exec")
   ((and (consp package)
         (eq 'for-syntax (car package)))
    (format ",in %s ,for-syntax" (cadr package)))
   (t
    (format ",in %s" package))))

;;; This assumes that when you load things into Scheme 48, you type
;;; names of files in your home directory using the syntax "~/".
;;; Similarly for current directory. Maybe we ought to send multiple
;;; file names to Scheme and let it look at all of them.

(defcustom scheme48-home-directory-kludge t
  "*Whether the home directory should be simplified.

When telling Scheme48 about the file name, it's a difference
whether we send a file name beginning with \"~/\" or the actual
expanded path name. If this is non-nil and the file name for
`scheme48-enough-scheme-file-name' starts with the user's home
directory, that is replaced with \"~/\"."
  :group 'scheme
  :type 'boolean)

(defun scheme48-enough-scheme-file-name (file)
  "Return a canonical name for FILE.
This will trim off the directory used in the *scheme* buffer,
or replace a home directory at the beginning with ~/ if
`scheme48-home-directory-kludge' is non-nil."
  (let ((file (expand-file-name file))
        (scheme-dir (with-current-buffer scheme-buffer
                      (expand-file-name default-directory))))
    (or (scheme48-replace-prefix file scheme-dir)
        (and scheme48-home-directory-kludge
             (scheme48-replace-prefix file (expand-file-name "~/") "~/"))
        file)))

(defun scheme48-replace-prefix (file prefix &optional replace)
  "Replace PREFIX at the beginning of FILE with REPLACE, or \"\"."
  (let ((replace (or replace ""))
        (len (length prefix)))
    (if (and (> (length file)
                len)
             (string-equal (substring file 0 len)
                           prefix))
        (concat replace (substring file len))
      nil)))

(provide 'scheme48)
;;; scheme48.el ends here
