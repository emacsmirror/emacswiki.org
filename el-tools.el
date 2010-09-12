;;; el-tools.el --- Emacs-Lisp tools

;; Author: Hayashi Masahiro <mhayashi1120@gmail.com>
;; Keywords: lisp refactor lint
;; URL: http://github.com/mhayashi1120/Emacs-Lisp/raw/master/el-tools.el
;; URL: http://www.emacswiki.org/download/el-tools.el
;; Emacs: GNU Emacs 22 or later

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Following utilities for Emacs-Lisp.
;; * Refactoring.
;; * Lint.

;;; Install:

;; Download refactor.el from
;; http://github.com/mhayashi1120/Emacs-gauche-ext/raw/master/refactor.el

;; Put this file and refactor.el into load-path'ed directory, 
;; and byte compile its if desired. 
;; And put the following expression into your ~/.emacs.
;;
;;     (require 'el-tools)
;;     (add-hook 'emacs-lisp-mode-hook
;;        (lambda ()
;;          (define-key emacs-lisp-mode-map "\C-c\C-v" el-tools-map)))
;;
;; And set `el-tools-clean-lint-path-alist', `el-tools-clean-lint-emacsen'

;;; Usage:
;; C-c C-v l : elint current buffer
;; C-c C-v L : elint current buffer by multiple emacs binaries.
;;             See `el-tools-clean-lint-emacsen'
;; C-c C-v r : Rename symbol in current buffer. 
;;             Resolve `let' binding as log as i can.

;;; TODO:
;; * Flymake? Server process?

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'refactor)

;; externals
(defvar load-path)
(defvar file-name-coding-system)
(defvar obarray)
(defvar shell-command-switch)
(defvar shell-file-name)
(defvar user-emacs-directory)

(defgroup el-tools nil
  "Emacs Lisp Refactoring utilities"
  :group 'lisp
  :prefix "el-tools-")

 

(defcustom el-tools-refactor-module-file 
  (cond
   ((boundp 'user-emacs-directory)
    (expand-file-name ".refactor-cache" user-emacs-directory))
   (t
    "~/.emacs.d/.refactor-cache"))
  "*Cached file of module dependency."
  :group 'el-tools)

(defvar el-tools-refactor-read-symbol-history nil)
(defvar el-tools-refactor-read-prefix-history nil)
(defvar el-tools-refactor-module-alist nil)

(defun el-tools-refactor-matched-prefix (symbol)
  (let ((symbol-name (symbol-name symbol))
	most-prefix len p)
    (mapatoms
     (lambda (s)
       (when (setq p (get s 'custom-prefix))
	 (when (stringp p)
	   (when (string-match (concat "^" (regexp-quote p)) symbol-name)
	     (when (or (null len) (< len (match-end 0)))
	       (setq len (match-end 0))
	       (setq most-prefix p))))))
     obarray)
    most-prefix))

(defun el-tools-refactor-defined-source-files (prefix)
  (let ((prefix-regexp (concat "^" (regexp-quote prefix)))
	ret file)
    (mapatoms
     (lambda (s)
       (when (string-match prefix-regexp (symbol-name s))
	 (when (setq file (symbol-file s))
	   (when (string-match "^\\(.*\\.el\\)c$" file)
	     (setq file (match-string 1 file)))
	   (add-to-list 'ret file))))
     obarray)
    ret))

(defun el-tools-refactor-rename-symbol-read-args ()
  (refactor-rename-symbol-read-args 'el-tools-refactor-read-symbol-history))

(defun el-tools-refactor-change-prefix-read-args ()
  (refactor-change-prefix-read-args 'el-tools-refactor-read-prefix-history))

(defun el-tools-refactor-find-global-binding (symbol)
  (save-excursion
    (goto-char (point-min))
    (catch 'found
      (condition-case err
	  (let (form name)
	    (while (setq form (read (current-buffer)))
	      (while form
		(when (symbolp (car form))
		  (setq name (symbol-name (car form)))
		  ;; todo match colored regexp
		  (when (member name '("defun" "defmacro" "defvar"))
		    (throw 'found t)))
		(setq form (cdr form))))
	    nil)
	(end-of-file nil)))))

(defun el-tools-refactor-find-local-binding (name)
  (save-excursion
    (catch 'found
      (condition-case err
	  (while t
	    (backward-up-list)
	    ;;todo macroexpand
	    (let* ((start (point-marker))
		   (form (read (current-buffer)))
		   (end (point-marker)))
	      (when (or
		     ;; todo different let and let*
		     (and (memq (car form) '(let let*))
			  (el-tools-refactor-let-binding-contains-p (cadr form) name))
		     (and (memq (car form) '(defun defmacro))
			  (el-tools-refactor-lambda-binding-contains-p (caddr form) name))
		     (and (eq (car form) 'lambda)
			  (el-tools-refactor-lambda-binding-contains-p (cadr form) name))
		     (and (eq (car form) 'catch)
			  (el-tools-refactor-catch-binding-contains-p (cdr form) name)))
		(throw 'found (cons start end)))))
	(scan-error nil)))))

(defun el-tools-refactor-let-binding-contains-p (let-arg name)
  (catch 'found
    (mapc
     (lambda (item)
       (when (cond
	      ((listp item)
	       (string= (symbol-name (car item)) name))
	      ((atom item)
	       (string= (symbol-name item) name)))
	 (throw 'found t)))
     let-arg)
    nil))

(defun el-tools-refactor-lambda-binding-contains-p (lambda-arg name)
  (catch 'found
    (mapc
     (lambda (item)
       (when (string= (symbol-name item) name)
	 (throw 'found t)))
     lambda-arg)
    nil))

(defun el-tools-refactor-catch-binding-contains-p (catch-arg name)
  "Consider (catch variable ...) like form."
  (and (listp (car catch-arg))
       (eq (caar catch-arg) 'quote)
       (symbolp (cadar catch-arg))
       (string= (symbol-name (cadar catch-arg)) name)))

(defun el-tools-refactor-find-modules (file)
  (let ((cell (assoc file el-tools-refactor-module-alist)))
    (el-tools-ref 'modules cell)))

(defun el-tools-refactor-file-required-modules (file)
  (let (ret)
    (mapc
     (lambda (cell)
       (when (catch 'found
	       (mapc
		(lambda (mod)
		  (when (string= mod file)
		    (throw 'found t)))
		(el-tools-ref 'modules cell))
	       nil)
	 (setq ret (cons (car cell) ret))))
     el-tools-refactor-module-alist)
    (nreverse ret)))

(defun el-tools-refactor-file-make-module (file &optional force)
  (let* ((cell (assoc-string file el-tools-refactor-module-alist))
	 (modtime (nth 5 (file-attributes file)))
	 (celltime (el-tools-ref 'modtime cell)))
    (when (or force
	      (null cell)
	      (null celltime)
	      (time-less-p celltime modtime))
      (when cell
	(setq el-tools-refactor-module-alist
	      (remove cell el-tools-refactor-module-alist)))
      (setq cell (list file
		       (cons 'modtime modtime)
		       (cons 'modules (el-tools-refactor-file-requiring-modules file)))))
    (setq el-tools-refactor-module-alist
	  (cons cell el-tools-refactor-module-alist))
    cell))

;;TODO only writable???
(defun el-tools-refactor-file-requiring-files (file)
  (remove nil
	  (mapcar
	   (lambda (sym)
	     (let* ((name (symbol-name sym))
		    (file (locate-library name)))
	       (when file
		 (when (string-match "\\.el\\(c\\)$" file)
		   (setq file (replace-match "" nil nil file 1)))
		 (when (file-exists-p file)
		   file))))
	   (el-tools-refactor-file-requiring-modules file))))

(defun el-tools-refactor-file-requiring-modules (file)
  (with-temp-buffer
    (let (modules)
      (insert-file-contents file)
      (goto-char (point-min))
      (condition-case err
	  (let (topform required module)
	    (while (setq topform (read (current-buffer)))
	      (when (setq module (el-tools-refactor-form-requiring-module topform))
		(setq modules (cons module modules)))))
	(error nil))
      (nreverse modules))))

(defun el-tools-refactor-form-requiring-module (form)
  (let (require-form module)
    (catch 'found
      (when (and (eq (car form) 'require)
		 (eq 'quote (caadr form))
		 (symbolp (setq require-form (cadadr form))))
	(throw 'found require-form))
      (mapc
       (lambda (x)
	 (when (listp x)
	   (when (setq module (el-tools-refactor-form-requiring-module x))
	     (throw 'found module))))
       form)
      nil)))

(defun el-tools-refactor-module-alist-cleanup ()
  (let ((orig el-tools-refactor-module-alist)
	ret)
    (setq el-tools-refactor-module-alist nil)
    (mapc
     (lambda (cell)
       (unless (assoc (car cell) ret)
	 (setq ret (cons cell ret))))
     orig)
    (setq ret (nreverse ret))
    (setq el-tools-refactor-module-alist ret)
    (length ret)))

(defun el-tools-refactor-rename-symbol-in-package (old-name new-name)
  (interactive (el-tools-refactor-rename-symbol-read-args))
  (let* ((symbol (intern-soft old-name))
	 (prefix (el-tools-refactor-matched-prefix symbol))
	 guessed-files)
    (when prefix
      (setq guessed-files (el-tools-refactor-defined-source-files prefix)))
    (unless (member buffer-file-name guessed-files)
      (setq guessed-files (append (list buffer-file-name) guessed-files)))
    (mapc
     (lambda (file)
       (refactor-with-file file
	 (el-tools-refactor-rename-symbol-in-buffer old-name new-name)))
     guessed-files)))

(defun el-tools-refactor-rename-symbol-in-buffer (old-name new-name)
  "Rename symbol at point."
  (interactive (el-tools-refactor-rename-symbol-read-args))
  (let (region)
    (setq region (el-tools-refactor-find-local-binding old-name))
    (refactor-rename-region old-name new-name region)))

(defun el-tools-refactor-change-prefix-in-buffer (old-prefix new-prefix)
  "Rename symbol at point."
  (interactive (el-tools-refactor-change-prefix-read-args))
  (refactor-change-symbol-prefix old-prefix new-prefix))


;;TODO????
(defun el-tools-refactor-change-by-regexp-sample ()
  (interactive)
  (let (ret symbol new-symbol)
    (goto-char (point-min))
    (save-excursion
      (while (re-search-forward "^(defun[ \t]+\\(\\_<[^@\n]+\\_>@\\)[ \t]*" nil t)
	(setq symbol (match-string-no-properties 1))
	(setq new-symbol (concat (substring symbol 0 -1) "$"))
	(save-excursion
	  (goto-char (point-min))
	  (while (re-search-forward (concat "\\_<" (regexp-quote symbol)) nil t)
	    (replace-match new-symbol)))
	(setq ret (cons symbol ret))))))

;; (defun el-tools-refactor-build-modules (&optional force)
;;   (interactive "P")
;;   (when (y-or-n-p "Build module alist. This takes long time. Ready? ")
;;     (let* ((list (el-tools-library-files))
;; 	   (total (float (length list)))
;; 	   (count 0)
;; 	   message-log-max total)
;;       (mapc
;;        (lambda (el)
;; 	 (el-tools-refactor-file-make-module el force)
;; 	 (setq count (1+ count))
;; 	 (message "Building dependency (% 3.0f%%) for \"%s\""
;; 		  (* (/ count total) 100) (file-name-nondirectory el))
;; 	 (when (= (% count 100) 0)
;; 	   (garbage-collect)))
;;        list))
;;     (message "Done build dependency.")
;;     el-tools-refactor-module-alist))

;; (defun el-tools-refactor-build-module-buffer (buffer-name)
;;   (interactive "bBuffer name: ")
;;   (let ((buffer (get-buffer buffer-name)))
;;     (unless (buffer-file-name buffer)
;;       (error "Buffer has no file."))
;;     (el-tools-refactor-file-make-module (buffer-file-name buffer) t)
;;     ;;TODO add required module
;;     (message "Done.")))

(defun el-tools-refactor-build-module-directory (directory)
  (interactive "DDirectory: ")
  (let ((files (directory-files directory t "\\.el$")))
    (mapc
     (lambda (f)
       (el-tools-refactor-file-make-module f t))
     files)
    (message "Done.")))

(defun el-tools-refactor-save-module-alist ()
  (interactive)
  (let ((dir (file-name-directory el-tools-refactor-module-file)))
    (unless (file-directory-p dir)
      (unless (y-or-n-p (format "Directory \"%s\" is not exists. Create it? " dir))
	(error "Directory not exists"))
      (make-directory dir t))
    (when (y-or-n-p "This takes several time. Ready? ")
      (el-tools-refactor-lisp-save el-tools-refactor-module-alist el-tools-refactor-module-file)
      (message "Save done."))))

(defun el-tools-refactor-load-module-alist (&optional no-msg)
  (interactive "P")
  (unless (file-exists-p el-tools-refactor-module-file)
    (error "Saved file not exists."))
  (when (or no-msg (y-or-n-p "Load Dependency cache? "))
    (setq el-tools-refactor-module-alist
	  (el-tools-load-lisp el-tools-refactor-module-file))))

(defun el-tools-refactor-lisp-save (lisp file)
  (with-temp-buffer
    (pp lisp (current-buffer))
    (let ((coding-system-for-write file-name-coding-system))
      (write-region (point-min) (point-max) file nil 'no-msg))))

 

(defvar isearch-lazy-highlight-last-string)
(defvar lazy-highlight-cleanup)

(defun el-tools-highlight-current-symbol ()
  (interactive)
  (let ((symbol (thing-at-point 'symbol))
	region start end)
    (setq region (el-tools-refactor-find-local-binding symbol))
    (setq start (if region (car region) (point-min)))
    (setq end (if region (cdr region) (save-excursion (goto-char (point-max)) (point-marker))))
    (let ((isearch-string (refactor-create-regexp symbol))
	  (isearch-regexp t)
	  (search-whitespace-regexp nil)
	  (isearch-case-fold-search nil))
      (isearch-lazy-highlight-new-loop start end))))

(defun el-tools-dehighlight-symbol ()
  (interactive)
  (lazy-highlight-cleanup lazy-highlight-cleanup)
  (setq isearch-lazy-highlight-last-string nil))

 

(defcustom el-tools-clean-lint-emacsen nil
  "*Emacs executables.

Examples:
\(\"emacs-21\" \"emacs-22.1\" \"emacs-23.2\" \"emacs-current\")
"
  :group 'el-tools
  :type '(list file))

(defcustom el-tools-clean-lint-path-alist nil
  "*Associate list key is file name of Elisp. 
cdr is `load-path' required by linting key file.

Examples:

\(
  (\"/home/bob/.emacs.d/el-tools.el\"
    \"/home/bob/.emacs.d/misc\")
)

# \"/home/bob/.emacs.d/misc\" directory have \"refactor.el\"
"
  :group 'el-tools
  :type '(list (list file)))

(defun el-tools-clean-lint ()
  (interactive)
  (el-tools-clean-lint-initialize)
  (let ((command (expand-file-name (invocation-name) (invocation-directory)))
	(file (expand-file-name (buffer-file-name))))
    (let ((proc (el-tools-clean-lint-internal command file)))
      (set-process-sentinel proc
			    (lambda (p e)
			      (when (eq (process-status p) 'exit)
				(el-tools-clean-lint-exit-mode-line p)))))))

(defun el-tools-clean-lint-emacsen ()
  (interactive)
  (when (el-tools-clean-lint-running-p)
    (error "Active process is running"))
  (unless el-tools-clean-lint-emacsen
    (error "No command found."))
  (let ((file (expand-file-name (buffer-file-name))))
    (el-tools-clean-lint-initialize)
    (el-tools-clean-lint-async file el-tools-clean-lint-emacsen)))

(defun el-tools-clean-lint-running-p ()
  (let ((buffer (el-tools-clean-lint-get-buffer)))
    (get-buffer-process buffer)))

(defun el-tools-clean-lint-async (file commands)
  (let ((command (car commands))
	(rest (cdr commands)))
    (let ((proc (el-tools-clean-lint-internal command file)))
      (set-process-sentinel 
       proc
       `(lambda (p e)
	  (when (eq (process-status p) 'exit)
	    (with-current-buffer (process-buffer p)
	      (el-tools-clean-lint-append "\n\n")
	      (el-tools-clean-lint-exit-mode-line p))
	    (when ',rest
	      (el-tools-clean-lint-async ,file ',rest))))))))

(defun el-tools-clean-lint-exit-mode-line (process)
  (with-current-buffer (process-buffer process)
    (let* ((code (process-exit-status process))
	   (msg  (format " (Exit [%d])" code)))
      (setq mode-line-process 
	    (propertize msg 'face (if (= code 0) 'compilation-info 'compilation-error))))))

;; TODO 22 or 23 not works?
(defun el-tools-clean-lint-internal (command file)
  (let* ((path (el-tools-ref file el-tools-clean-lint-path-alist))
	 (version (el-tools-emacs-version command t))
	 (sexp `(progn 
		  ,(when (<= version 21)
		     '(require 'cl))
		  (setq load-path (append load-path ',path)) 
		  (find-file ,file)
		  (elint-initialize)
		  (elint-current-buffer)
		  (with-current-buffer "*Elint*"
		    (princ (buffer-string)))))
	 (eval-form (prin1-to-string sexp))
	 (buffer (el-tools-clean-lint-get-buffer))
	 cmdline)
    (setq eval-form (replace-regexp-in-string "\n" " " eval-form))
    (setq eval-form (replace-regexp-in-string "\"" "\\\\\"" eval-form))
    (setq cmdline (format "%s -batch -eval \"%s\"" command eval-form))
    (display-buffer buffer)
    (with-current-buffer buffer
      (el-tools-clean-lint-append cmdline "\n")
      (let ((proc (start-process "Clean Lint" (current-buffer) 
				 shell-file-name shell-command-switch
				 cmdline)))
	(set-process-sentinel proc (lambda (p e)))
	(setq mode-line-process (propertize " (Running)" 'face 'compilation-warning))
	proc))))

(defun el-tools-clean-lint-initialize ()
  (with-current-buffer (el-tools-clean-lint-get-buffer)
    (let ((inhibit-read-only t)
	  buffer-read-only)
      (erase-buffer))))

(defun el-tools-clean-lint-get-buffer ()
  (get-buffer-create "*Async Elint*"))

(defun el-tools-clean-lint-append (&rest strings)
  (let (buffer-read-only)
    (goto-char (point-max))
    (apply 'insert strings)))

 

(defun el-tools-load-lisp (file &optional coding-system)
  "Read file and return Lisp list.
FILE to load.
Optional argument CODING-SYSTEM FILE's coding-system."
  (let ((auto-image-file-mode nil)
	(after-insert-file-functions nil)
	(coding-system-for-read (or coding-system 'undecided)))
    (with-temp-buffer
      (insert-file-contents file)
      (condition-case nil
	  (read (current-buffer))
	(error nil)))))

(defun el-tools-ref (key list)
  (cdr (assoc key list)))

(defun el-tools-emacs-version (command &optional major-only)
  (with-temp-buffer
    (call-process command nil (current-buffer) nil "-version")
    (let ((output (buffer-string)))
      (unless (string-match "\\(2[0-9]\\)\\.[0-9]+\\.[0-9]+" output)
	(error "Unable get version"))
      (if major-only
	  (string-to-number (match-string 1 output))
	(match-string 0 output)))))

(defun el-tools-library-files ()
  (let (list)
    (mapc
     (lambda (lib)
       (cond
	((file-directory-p lib)
	 (mapc
	  (lambda (el)
	    (unless (file-directory-p el)
	      (setq list (cons el list))))
	  (directory-files lib t "\\.el$")))
	((file-exists-p lib)
	 (setq list (cons lib list)))))
     load-path)
    (nreverse list)))

 

(defvar el-tools-map nil)

(unless el-tools-map
  (setq el-tools-map
	(let ((map (make-sparse-keymap)))

	  (define-key map "R" 'el-tools-refactor-rename-symbol-in-package)
	  (define-key map "r" 'el-tools-refactor-rename-symbol-in-buffer)
	  (define-key map "c" 'el-tools-refactor-change-prefix-in-buffer)
	  ;; (define-key map "B" 'el-tools-refactor-build-modules)
	  ;; (define-key map "b" 'el-tools-refactor-build-module-buffer)
	  ;; (define-key map "L" 'el-tools-refactor-load-module-alist)
	  (define-key map "h" 'el-tools-highlight-current-symbol)
	  (define-key map "d" 'el-tools-dehighlight-symbol)
	  (define-key map "l" 'el-tools-clean-lint)
	  (define-key map "L" 'el-tools-clean-lint-emacsen)

	  map)))

(provide 'el-tools)

;;; el-tools.el ends here
