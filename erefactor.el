;;; erefactor.el --- Emacs-Lisp tools

;; Author: Hayashi Masahiro <mhayashi1120@gmail.com>
;; Keywords: lisp refactor lint
;; URL: http://github.com/mhayashi1120/Emacs-gosh-mode/raw/master/erefactor.el
;; URL: http://www.emacswiki.org/emacs/download/erefactor.el
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

;; Put this file into load-path'ed directory, 
;; and byte compile its if desired. 
;; And put the following expression into your ~/.emacs.
;;
;;     (require 'erefactor)
;;     (add-hook 'emacs-lisp-mode-hook
;;        (lambda ()
;;          (define-key emacs-lisp-mode-map "\C-c\C-v" erefactor-map)))
;;
;; And set `erefactor-lint-path-alist', `erefactor-lint-by-emacsen'

;;; Usage:
;; C-c C-v l : elint current buffer
;; C-c C-v L : elint current buffer by multiple emacs binaries.
;;             See `erefactor-lint-emacsen'
;; C-c C-v r : Rename symbol in current buffer. 
;;             Resolve `let' binding as long as i can.

;;; TODO:
;; * Flymake? Server process?

;;; Code:

(eval-when-compile
  (require 'cl))

;; externals
(defvar load-path)
(defvar obarray)
(defvar shell-command-switch)
(defvar shell-file-name)
(defvar load-history)
(defvar isearch-lazy-highlight-last-string)
(defvar lazy-highlight-cleanup)

(defgroup erefactor nil
  "Emacs Lisp Refactoring utilities"
  :group 'lisp
  :prefix "erefactor-")

 
(defvar erefactor--read-symbol-history nil)
(defvar erefactor--read-prefix-history nil)

(defun erefactor--symbol-group-prefix (symbol)
  (let ((symbol-name (symbol-name symbol))
        most-prefix len p)
    (mapatoms
     (lambda (s)
       (when (and (setq p (get s 'custom-prefix))
                  (stringp p)
                  (string-match (concat "^" (regexp-quote p)) symbol-name)
                  (or (null len) (< len (match-end 0))))
         (setq len (match-end 0))
         (setq most-prefix p)))
     obarray)
    most-prefix))

(defun erefactor--guessed-using-files (symbol)
  (let (ret file)
    (let* ((prefix (erefactor--symbol-group-prefix symbol))
           (prefix-regexp (concat "^" (regexp-quote prefix))))
      (mapatoms
       (lambda (s)
         (when (and (string-match prefix-regexp (symbol-name s))
                    (setq file (symbol-file s)))
           ;; if byte compiled file
           (when (string-match "^\\(.*\\.el\\)c$" file)
             (setq file (match-string 1 file)))
           (add-to-list 'ret file)))
       obarray))
    ;;TODO refactor
    (let ((files (append (erefactor--symbol-using-sources symbol 'defun)
                         (erefactor--symbol-using-sources symbol 'defvar)
                         (erefactor--symbol-using-sources symbol 'defface))))
      (setq ret (union files ret)))
    ret))

(defun erefactor--find-local-binding (name)
  (save-excursion
    (catch 'found
      (condition-case err
          (while t
            (backward-up-list)
            (let* ((start (point-marker))
                   (form (read (current-buffer)))
                   (end (point-marker)))
              (when (or
                     (erefactor--local-binding-p name form)
                     (erefactor--macroexpand-contains-p name form))
                (throw 'found (cons start end)))))
        (scan-error nil)))))

(defun erefactor--local-binding-p (name form)
  (or
   ;; todo difference between let and let*
   (and (memq (car-safe form) '(let let*))
        (erefactor--let-binding-contains-p (cadr form) name))
   (and (memq (car-safe form) '(defun defmacro))
        (erefactor--lambda-binding-contains-p (caddr form) name))
   (and (eq (car-safe form) 'lambda)
        (erefactor--lambda-binding-contains-p (cadr form) name))
   (and (eq (car-safe form) 'catch)
        (erefactor--catch-binding-contains-p (cdr form) name))))

(defun erefactor--macroexpand-contains-p (name form)
  (when (and (not (memq (car-safe form) '(lambda)))
             (erefactor-macrop (car-safe form)))
    (let ((expand-form (macroexpand form)))
      (catch 'found
        (when (erefactor--local-binding-p name expand-form)
          (throw 'found t))
        (when (erefactor--binding-exists-p name expand-form)
          (throw 'found t))
        nil))))

(defun erefactor--binding-exists-p (name form)
  (catch 'found
    (mapc
     (lambda (f)
       (when (or (erefactor--local-binding-p name f)
                 (erefactor--macroexpand-contains-p name f))
         (throw 'found t))
       (when (and (listp f)
                  (erefactor--binding-exists-p name f))
         (throw 'found t)))
     form)
    nil))

(defun erefactor--let-binding-contains-p (let-arg name)
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

(defun erefactor--lambda-binding-contains-p (lambda-arg name)
  (catch 'found
    (mapc
     (lambda (item)
       (when (string= (symbol-name item) name)
         (throw 'found t)))
     lambda-arg)
    nil))

(defun erefactor--catch-binding-contains-p (catch-arg name)
  "Consider (catch variable ...) like form."
  (and (listp (car catch-arg))
       (eq (caar catch-arg) 'quote)
       (symbolp (cadar catch-arg))
       (string= (symbol-name (cadar catch-arg)) name)))

(defun erefactor-rename-symbol-in-package (old-name new-name)
  (interactive 
   (erefactor-rename-symbol-read-args 'erefactor--read-symbol-history))
  (let* ((symbol (intern-soft old-name))
         (guessed-files (erefactor--guessed-using-files symbol)))
    (when buffer-file-name
      (unless (member buffer-file-name guessed-files)
        (setq guessed-files (cons buffer-file-name guessed-files))))
    (mapc
     (lambda (file)
       (erefactor-with-file file
         (erefactor-rename-symbol-in-buffer old-name new-name)))
     guessed-files)))

(defun erefactor-rename-symbol-in-buffer (old-name new-name)
  "Rename symbol at point."
  (interactive 
   (erefactor-rename-symbol-read-args 'erefactor--read-symbol-history))
  (let (region after)
    (setq region (erefactor--find-local-binding old-name))
    (unless region
      (setq after 'erefactor-after-rename-symbol))
    (erefactor-rename-region old-name new-name region nil after)))

(defun erefactor-change-prefix-in-buffer (old-prefix new-prefix)
  "Rename symbol at point."
  (interactive 
   (erefactor-change-prefix-read-args 'erefactor--read-prefix-history))
  (erefactor-change-symbol-prefix old-prefix new-prefix 
                                 nil 'erefactor-after-rename-symbol))

(defun erefactor-after-rename-symbol (old-name new-name)
  (let ((fnsym (erefactor--current-fnsym))
        (old (intern old-name))
        (new (intern new-name)))
    (eval-defun nil)
    (when (eq (cadr fnsym) new)
      (case (car fnsym)
        ((defvar defcustom defconst)
         (erefactor--change-load-name old new 'defvar))
        ((defun defmacro)
         (erefactor--change-load-name old new 'defun))
        ((defface)
         (erefactor--change-load-name old new 'defface))))))

(defun erefactor--current-fnsym ()
  (save-excursion
    (let (ret)
      (condition-case nil
          (while (not (bobp))
            (let ((sym (thing-at-point 'symbol)))
              (backward-sexp)
              (when sym
                (push (intern sym) ret))
              (skip-syntax-backward " ")))
        (scan-error nil))
      ret)))

(defun erefactor--symbol-defined-alist (symbol)
  (let (funcs faces vars tmp)
    (mapc
     (lambda (def)
       (when (memq symbol (cdr def))
         (push (car def) vars))
       (when (and (setq tmp (rassq symbol (cdr def)))
                  (eq (car tmp) 'defface))
         (push (car def) faces))
       (when (and (setq tmp (rassq symbol (cdr def)))
                  (eq (car tmp) 'defun))
         (push (car def) funcs)))
     load-history)
    `((defun . ,funcs)
      (defface . ,faces)
      (defvar . ,vars))))

(defun erefactor--change-load-name (old-symbol new-symbol type)
  (let* ((defs (erefactor--symbol-defined-alist old-symbol))
         (files (cdr (assq type defs))))
    (mapc
     (lambda (file)
       (let ((def (cdr (assoc file load-history))))
         (cond
          ((memq type '(defun defface))
           (let ((tmp (rassq old-symbol def)))
             (when tmp
               (setcdr tmp new-symbol))))
          (t
           (let ((tmp (memq old-symbol def)))
             (setcar tmp new-symbol))))))
     files)))

(defun erefactor--symbol-package (symbol type)
  (let* ((defs (erefactor--symbol-defined-alist symbol))
         (files (cdr (assq type defs))))
    (catch 'found
      (mapc
       (lambda (file)
         (let ((tmp (cdr (assq 'provide (cdr (assoc file load-history))))))
           (when tmp
             (throw 'found tmp))))
       files)
      nil)))

;;TODO merge to erefactor--guessed-using-files
(defun erefactor--symbol-using-sources (symbol type)
  (let ((package (erefactor--symbol-package symbol type)))
    (loop for defs in load-history
          when (loop for def in (cdr defs)
                     when (and (listp def) 
                               (eq (car def) 'require)
                               (eq package (cdr def)))
                     collect def)
          collect (car defs))))

 

(defvar erefactor--overlay nil)
(defvar erefactor--region-start nil)
(defvar erefactor--region-end nil)

(defun erefactor-already-bounded (symbol start end)
  "SYMBOL is already bounded or not in region START END."
  (save-excursion
    (goto-char start)
    (and (re-search-forward (erefactor-create-regexp symbol) nil t)
         (< (point) end))))

(defun erefactor-create-regexp (symbol)
  "Create SYMBOL exclusive regexp."
  (format "\\_<\\(%s\\)\\_>" (regexp-quote symbol)))

(defun erefactor-create-prefixed-regexp (prefix)
  "Create matching to PREFIX exclusive regexp."
  (format "\\_<\\(\\(%s\\)\\(\\(?:\\s_\\|\\sw\\)+\\)\\)\\_>" (regexp-quote prefix)))

(defun erefactor-dehighlight ()
  "Dehighlight `erefactor-highlight'."
  (when erefactor--overlay
    (delete-overlay erefactor--overlay))
  (lazy-highlight-cleanup lazy-highlight-cleanup)
  (setq isearch-lazy-highlight-last-string nil))

(defun erefactor-highlight (string beg fin)
  "Highlight STRING between BEG and FIN."
  (setq erefactor--overlay (make-overlay beg fin))
  (overlay-put erefactor--overlay 'priority 1001) ;higher than lazy overlays
  (overlay-put erefactor--overlay 'face 'query-replace)
  (let ((isearch-string string)
        (isearch-regexp t)
        (search-whitespace-regexp nil)
        (isearch-case-fold-search nil))
    (isearch-lazy-highlight-new-loop erefactor--region-start erefactor--region-end)))

(defmacro erefactor-with-file (file &rest form)
  (declare (indent 1))
  `(let ((win (selected-window))
         buffer opened)
     (if (setq buffer (get-file-buffer file))
         (setq opened t)
       (setq buffer (find-file-noselect file)))
     (unwind-protect 
         (with-current-buffer buffer
           (save-window-excursion
             (set-window-buffer win buffer)
             (let (buffer-read-only)
               ,@form)))
       (unless opened
         (when (buffer-live-p buffer)
           (unless (buffer-modified-p buffer)
             (kill-buffer buffer)))))))

(defun erefactor--call-before (func old-name new-name)
  (save-match-data
    (if func 
        (funcall func old-name new-name)
      (y-or-n-p "Rename? "))))

(defun erefactor--call-after (func old-name new-name)
  (save-match-data
    (when func
      (funcall func old-name new-name))))

(defun erefactor-rename-region (symbol new &optional region before-func after-func)
  "Rename SYMBOL to NEW in REGION.
Optional arg BEFORE-FUNC is called with two args SYMBOL and NEW before replacing.
  This function must return non-nil value if executing replacing.
Optional arg AFTER-FUNC is called with two args SYMBOL and NEW after replaced.
"
  (let ((start (if region (car region) (point-min)))
        (end (save-excursion 
               (goto-char (if region (cdr region) (point-max)))
               (point-marker)))
        regexp)
    (when (or (not (erefactor-already-bounded new start end))
              (y-or-n-p (format "%s is already bound. Continue? " new)))
      (save-excursion
        (setq erefactor--region-start start)
        (setq erefactor--region-end end)
        (goto-char start)
        (setq regexp (erefactor-create-regexp symbol))
        ;; cannot use narrow-to-region because unnatural while interactive loop
        (while (and (re-search-forward regexp nil t)
                    (< (point) end))
          (goto-char (match-end 1))
          (erefactor-highlight regexp (match-beginning 1) (match-end 1))
          (unwind-protect
              (when (erefactor--call-before before-func symbol new)
                (replace-match new nil nil nil 1)
                (erefactor--call-after after-func symbol new))
            (erefactor-dehighlight)))))))

(defun erefactor-change-symbol-prefix (prefix new &optional before-func after-func)
  "Rename SYMBOL to NEW in REGION.
Optional arg BEFORE-FUNC is called with two args old-name and new-name before replacing.
  This function must return non-nil value if executing replacing.
Optional arg AFTER-FUNC is called with two args old-name and new-name after replaced.
"
  (let (regexp)
    (save-excursion
      (setq erefactor--region-start (point-min))
      (setq erefactor--region-end (point-max))
      (goto-char (point-min))
      (setq regexp (erefactor-create-prefixed-regexp prefix))
      ;; cannot use narrow-to-region because unnatural while interactive loop
      (while (re-search-forward regexp nil t)
        (goto-char (match-end 1))
        (erefactor-highlight regexp (match-beginning 2) (match-end 2))
        (let* ((suffix (match-string 3))
               (old-name (concat prefix suffix))
               (new-name (concat new suffix)))
          (unwind-protect
              (when (erefactor--call-before before-func old-name new-name)
                (replace-match new nil nil nil 2)
                (erefactor--call-after after-func old-name new-name)))
          (erefactor-dehighlight))))))

(defun erefactor-rename-symbol-read-args (hist-var)
  (let (current-name prompt new-name)
    (barf-if-buffer-read-only)
    (unless (setq current-name (thing-at-point 'symbol))
      (error "No symbol at point"))
    (setq prompt (format "%s -> New name: " current-name))
    (setq new-name (read-string prompt current-name hist-var))
    (when (string= current-name new-name)
      (error "No difference"))
    (list current-name new-name)))

(defun erefactor-change-prefix-read-args (hist-var)
  (let (current-prefix prompt new-prefix)
    (barf-if-buffer-read-only)
    (setq current-prefix (thing-at-point 'symbol))
    (setq current-prefix (read-string "Changing prefix: " current-prefix hist-var))
    (setq prompt (format "Changing prefix: %s -> New prefix: " current-prefix))
    (setq new-prefix (read-string prompt current-prefix hist-var))
    (when (string= current-prefix new-prefix)
      (error "No difference"))
    (list current-prefix new-prefix)))

(defun erefactor-highlight-current-symbol ()
  (interactive)
  (let ((symbol (thing-at-point 'symbol))
        region start end)
    (setq region (erefactor--find-local-binding symbol))
    (setq start (if region (car region) (point-min)))
    (setq end (if region (cdr region) (save-excursion (goto-char (point-max)) (point-marker))))
    (let ((isearch-string (erefactor-create-regexp symbol))
          (isearch-regexp t)
          (search-whitespace-regexp nil)
          (isearch-case-fold-search nil))
      (isearch-lazy-highlight-new-loop start end))))

(defun erefactor-dehighlight-symbol ()
  (interactive)
  (lazy-highlight-cleanup lazy-highlight-cleanup)
  (setq isearch-lazy-highlight-last-string nil))

 

(defcustom erefactor-lint-emacsen nil
  "*Emacs executables.

Examples:
\(\"emacs-21\" \"emacs-22.1\" \"emacs-23.2\" \"emacs-current\")
"
  :group 'erefactor
  :type '(list file))

(defcustom erefactor-lint-path-alist nil
  "*Associate list key is file name of Elisp. 
value is `load-path' that required by key file if key file require some module.

Examples:
\(
  (\"/home/bob/.emacs.d/linting-file.el\"
    \"/home/bob/.emacs.d/misc\")
)

\"/home/bob/.emacs.d/misc\" directory have some requiring module(s).
"
  :group 'erefactor
  :type '(list (list file)))

(defun erefactor-lint ()
  (interactive)
  (erefactor-lint-initialize)
  (let ((command (expand-file-name (invocation-name) (invocation-directory)))
        (file (expand-file-name (buffer-file-name))))
    (let ((proc (erefactor-lint-internal command file)))
      (set-process-sentinel proc
                            (lambda (p e)
                              (when (eq (process-status p) 'exit)
                                (erefactor-lint-exit-mode-line p)))))))

(defun erefactor-lint-by-emacsen ()
  (interactive)
  (when (erefactor-lint-running-p)
    (error "Active process is running"))
  (unless erefactor-lint-emacsen
    (error "No command found."))
  (let ((file (expand-file-name (buffer-file-name))))
    (erefactor-lint-initialize)
    (erefactor-lint-async file erefactor-lint-emacsen)))

(defun erefactor-lint-running-p ()
  (let ((buffer (erefactor-lint-get-buffer)))
    (get-buffer-process buffer)))

(defun erefactor-lint-async (file commands)
  (let ((command (car commands))
        (rest (cdr commands)))
    (let ((proc (erefactor-lint-internal command file)))
      (set-process-sentinel 
       proc
       `(lambda (p e)
          (when (eq (process-status p) 'exit)
            (with-current-buffer (process-buffer p)
              (erefactor-lint-append "\n\n")
              (erefactor-lint-exit-mode-line p))
            (when ',rest
              (erefactor-lint-async ,file ',rest))))))))

(defun erefactor-lint-exit-mode-line (process)
  (with-current-buffer (process-buffer process)
    (let* ((code (process-exit-status process))
           (msg  (format " (Exit [%d])" code)))
      (setq mode-line-process 
            (propertize msg 'face (if (= code 0) 'compilation-info 'compilation-error))))))

(defun erefactor-lint-internal (command file)
  (let* ((path (erefactor-ref file erefactor-lint-path-alist))
         (version (erefactor-emacs-version command t))
         (sexp `(progn 
                  (setq load-path (append load-path ',path)) 
                  (find-file ,file)
                  ;;FIXME probablly ok...
                  ,(when (<= version 23)
                     '(eval-buffer))
                  (elint-initialize)
                  (elint-current-buffer)
                  (with-current-buffer "*Elint*"
                    (princ (buffer-string)))))
         (eval-form (prin1-to-string sexp))
         (buffer (erefactor-lint-get-buffer))
         cmdline)
    (setq eval-form (replace-regexp-in-string "\n" " " eval-form))
    (setq eval-form (replace-regexp-in-string "\"" "\\\\\"" eval-form))
    (setq cmdline (format "%s -batch -eval \"%s\"" command eval-form))
    (display-buffer buffer)
    (with-current-buffer buffer
      (erefactor-lint-append (format "----- Linting by %s -----\n" command))
      (let ((proc (start-process "Clean Lint" (current-buffer) 
                                 shell-file-name shell-command-switch
                                 cmdline)))
        (set-process-sentinel proc (lambda (p e)))
        (setq mode-line-process (propertize " (Running)" 'face 'compilation-warning))
        proc))))

(defun erefactor-lint-initialize ()
  (with-current-buffer (erefactor-lint-get-buffer)
    (let ((inhibit-read-only t)
          buffer-read-only)
      (erase-buffer))))

(defun erefactor-lint-get-buffer ()
  (get-buffer-create "*Async Elint*"))

(defun erefactor-lint-append (&rest strings)
  (let (buffer-read-only)
    (goto-char (point-max))
    (apply 'insert strings)))

 

(defun erefactor-macrop (symbol)
  (and 
   (symbolp symbol)
   (fboundp symbol)
   (eq (car-safe (symbol-function symbol)) 'macro)))

(defun erefactor-ref (key list)
  (cdr (assoc key list)))

(defun erefactor-emacs-version (command &optional major-only)
  (with-temp-buffer
    (call-process command nil (current-buffer) nil "-version")
    (let ((output (buffer-string)))
      (unless (string-match "\\(2[0-9]\\)\\.[0-9]+\\.[0-9]+" output)
        (error "Unable get version"))
      (if major-only
          (string-to-number (match-string 1 output))
        (match-string 0 output)))))

 

(defvar erefactor-map nil)

(unless erefactor-map
  (let ((map (make-sparse-keymap)))

    (define-key map "R" 'erefactor-rename-symbol-in-package)
    (define-key map "r" 'erefactor-rename-symbol-in-buffer)
    (define-key map "c" 'erefactor-change-prefix-in-buffer)
    (define-key map "h" 'erefactor-highlight-current-symbol)
    (define-key map "d" 'erefactor-dehighlight-symbol)
    (define-key map "l" 'erefactor-lint)
    (define-key map "L" 'erefactor-lint-by-emacsen)

    (setq erefactor-map map)))

(provide 'erefactor)

;;; erefactor.el ends here
