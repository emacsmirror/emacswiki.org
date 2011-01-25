;;; erefactor.el --- Emacs-Lisp tools

;; Author: Hayashi Masahiro <mhayashi1120@gmail.com>
;; Keywords: elisp refactor lint
;; URL: http://github.com/mhayashi1120/Emacs-erefactor/raw/master/erefactor.el
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
;; And set these variables correctly.
;;  `erefactor-lint-path-alist', `erefactor-lint-by-emacsen'

;; Put the following in your .emacs, if you desire highlighting local variable.
;;
;;     (add-hook 'emacs-lisp-mode-hook 'erefactor-lazy-highlight-turn-on)
;;     (add-hook 'lisp-interaction-mode-hook 'erefactor-lazy-highlight-turn-on)

;;; Usage:

;; C-c C-v l : elint current buffer in clean environment.
;; C-c C-v L : elint current buffer by multiple emacs binaries.
;;             See `erefactor-lint-emacsen'
;; C-c C-v r : Rename symbol in current buffer. 
;;             Resolve `let' binding as long as i can.
;; C-c C-v R : Rename symbol in requiring modules and current buffer. 
;; C-c C-v h : Highlight current symbol in this buffer
;;             and suppress `erefacthr-highlight-mode'.
;; C-c C-v d : Dehighlight all by above command.
;; C-c C-v c : Switch prefix bunch of symbols. 
;;             ex: '(hoge-var hoge-func) -> '(foo-var foo-func)

;;; TODO:
;; * Flymake? Server process?
;; * Change only same case if symbol. But docstring is not.
;; * `.' is not a separator of lisp symbol.
;;   rename `region' and `REGION.' in docstring
;;   don't use re-search while idiom, 
;;   gather symbols in code, string, comment each context.

;;; Code:

(eval-when-compile
  (require 'cl))

;; externals
(defvar load-path)
(defvar obarray)
(defvar shell-command-switch)
(defvar shell-file-name)
(defvar load-history)
(defvar idle-update-delay) ;; after Emacs 22
(defvar this-command)
(defvar timer-idle-list)
(defvar current-prefix-arg)

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
  (let* ((first (point))
         (symbol (intern name))
         (history (cons first nil))
         previous)
    (save-excursion
      (catch 'found
        (condition-case err
            (while t
              (backward-up-list)
              (let* ((start (point-marker))
                     (form (read (current-buffer)))
                     (end (point-marker))
                     (special-bind (erefactor--special-binding symbol form history)))
                ;; detect looping
                ;; list start char by syntax-table
                (when (and previous (= (car previous) start))
                  (signal 'scan-error nil))
                (when special-bind
                  (throw 'found special-bind))
                (when (or
                       (erefactor--local-binding-p symbol form)
                       (erefactor--macroexpand-contains-p symbol form))
                  (throw 'found (cons start end)))
                (setq previous (cons start end))
                (setq history (cons previous history))))
          (scan-error nil))))))

(defun erefactor--special-binding (name form history)
  "NAME scope is not single sexp."
  (erefactor--local-fbinding name form history))

(defun erefactor--local-fbinding (name form history)
  (when (memq (car-safe form) '(flet macrolet labels))
    (save-excursion
      ;; ignore all error because `flet' case is special!!
      (condition-case nil
          (let ((region (cadr history))
                (first (car (last history))))
            (when (and (consp region)
                       (< (car region) first)
                       (> first (car region)))
              ;; at start definition of local function 
              ;; (flet ((func (a b) (list a b))))
              ;;        ^^
              (goto-char (car region))
              (forward-char)
              (forward-sexp) ;; end of function name
              (let ((args (read (current-buffer))))
                (when (erefactor--lambda-binding-contains-p args name)
                  region))))
        (error nil)))))

(defun erefactor--local-binding-p (name form)
  (or
   ;; todo difference between let and let*
   (and (memq (car-safe form) '(let let* lexical-let lexical-let*))
        (erefactor--let-binding-contains-p (cadr form) name))
   (and (memq (car-safe form) '(defun defmacro))
        (erefactor--lambda-binding-contains-p (caddr form) name))
   (and (eq (car-safe form) 'lambda)
        (erefactor--lambda-binding-contains-p (cadr form) name))
   (and (eq (car-safe form) 'defadvice)
        (erefactor--defadvice-binding-contains-p (caddr form) name))
   (and (eq (car-safe form) 'catch)
        (erefactor--catch-binding-contains-p (cdr form) name))
   (and (eq (car-safe form) 'condition-case)
        (erefactor--condition-case-contains-p (cdr form) name))
   (and (eq (car-safe form) 'eieio-defmethod)
        (erefactor--eieio-defmethod-contains-p (caadr (caddr form)) name))))

(defun erefactor--macroexpand-contains-p (name form)
  ;; `lambda' is macro expanded like (function (lambda () ...))
  (when (and (not (memq (car-safe form) '(lambda))) 
             (erefactor-macrop (car-safe form)))
    (condition-case nil
        (let ((expand-form (macroexpand form)))
          (catch 'found
            (when (erefactor--local-binding-p name expand-form)
              (throw 'found t))
            (when (erefactor--binding-exists-p name expand-form)
              (throw 'found t))
            nil))
      (error nil))))

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

(defun erefactor--condition-case-contains-p (form name)
  (let ((var (car-safe form)))
    ;; error binded variable and must be non-nil value
    (when (and (atom var) var)
      (eq var name))))

(defun erefactor--let-binding-contains-p (let-arg name)
  (or (memq name let-arg)
      (assq name let-arg)))

(defun erefactor--lambda-binding-contains-p (lambda-arg name)
  (and (not (memq name '(&optional &rest)))
       (memq name lambda-arg)))

(defun erefactor--eieio-defmethod-contains-p (method-arg name)
  (and (not (memq name '(&optional &rest)))
       (or (memq name method-arg)
           (assq name method-arg))))

(defun erefactor--defadvice-binding-contains-p (ad-args name)
  (let* ((rest (cddr ad-args))
         ;; consider optional position arg
         (args (if (consp (car rest)) 
                   (car rest)
                 (cadr rest))))
    (erefactor--lambda-binding-contains-p args name)))

(defun erefactor--catch-binding-contains-p (catch-arg name)
  "Consider (catch variable ...) like form."
  (and (listp (car catch-arg))
       (eq (caar catch-arg) 'quote)
       (symbolp (cadar catch-arg))
       (eq (cadar catch-arg) name)))

(defun erefactor-rename-symbol-in-package (old-name new-name)
  "Rename symbol at point with queries. This affect to current buffer and requiring modules.

Please remember, this function only works well if 
the module have observance of `require'/`provide' system.
"
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
  "Rename symbol at point resolving reference local variable as long as i can with queries.
This affect to current buffer."
  (interactive 
   (erefactor-rename-symbol-read-args 'erefactor--read-symbol-history))
  (let ((region (erefactor--find-local-binding old-name))
        after)
    (unless region
      (setq after 'erefactor-after-rename-symbol))
    (erefactor-rename-region old-name new-name region nil after)))

(defun erefactor-change-prefix-in-buffer (old-prefix new-prefix)
  "Rename symbol prefix with queries.

OLD-PREFIX: `foo-' -> NEW-PREFIX: `baz-'
`foo-function1' -> `baz-function1'
`foo-variable1' -> `baz-variable1'
"
  (interactive 
   (erefactor-change-prefix-read-args 'erefactor--read-prefix-history))
  (erefactor-change-symbol-prefix old-prefix new-prefix 
                                  nil 'erefactor-after-rename-symbol))

;; (defun erefactor-before-rename-symbol (old-name captured new-name)
;;   (cond
;;    ((erefactor-context-code-p)
;;     ;; ignore if case is different
;;     (when (string= old-name captured)
;;       (y-or-n-p "Rename? ")))
;;    (t
;;     (y-or-n-p "Replace? "))))

(defun erefactor-after-rename-symbol (old-name new-name)
  (let ((fnsym (erefactor--current-fnsym))
        (old (intern old-name))
        (new (intern new-name)))
    ;; re-define definition.
    ;; if `defvar' or `defcustom' current value will be cleared.
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

(defun erefactor-context-code-p (&optional point)
  (save-excursion
    (let ((parses (parse-partial-sexp (point-min) (or point (point)))))
      (and (not (nth 3 parses))
           (not (nth 4 parses))))))

(defun erefactor-context-string-p (&optional point)
  (save-excursion
    (let ((parses (parse-partial-sexp (point-min) (or point (point)))))
      (nth 3 parses))))

(defun erefactor-context-comment-p (&optional point)
  (save-excursion
    (let ((parses (parse-partial-sexp (point-min) (or point (point)))))
      (nth 4 parses))))

(defun erefactor-already-bounded (symbol start end)
  "SYMBOL is already bounded or not in region START END."
  (save-excursion
    (goto-char start)
    ;; search only symbol. (if possible...)
    (let (case-fold-search)
      (and (re-search-forward (erefactor-create-regexp symbol) nil t)
           (< (point) end)))))

(defun erefactor-create-regexp (symbol)
  "Create SYMBOL exclusive regexp."
  (format "\\_<\\(%s\\)\\_>" (regexp-quote symbol)))

(defun erefactor-create-prefixed-regexp (prefix)
  "Create matching to PREFIX exclusive regexp."
  (format "\\_<\\(\\(%s\\)\\(\\(?:\\s_\\|\\sw\\)+\\)\\)\\_>" (regexp-quote prefix)))

(defun erefactor-dehighlight-in-interactive ()
  "Dehighlight text by `erefactor-re-highlight-in-interactive'."
  (when erefactor--overlay
    (delete-overlay erefactor--overlay))
  (erefactor-dehighlight-all))

(defun erefactor-re-highlight-in-interactive (regexp beg fin)
  "Highlight REGEXP between BEG and FIN in region 
`erefactor--region-start' to `erefactor--region-end'."
  ;; highlight replacing text
  (if (overlayp erefactor--overlay)
      (move-overlay erefactor--overlay beg fin (current-buffer))
    (setq erefactor--overlay (make-overlay beg fin))
    (overlay-put erefactor--overlay 'priority 100) ; higher than erefactor-highlight-face
    (overlay-put erefactor--overlay 'face 'query-replace))
  ;; highlight scheduled replacing text.
  (erefactor-highlight-update-region
   erefactor--region-start erefactor--region-end
   regexp t))

(defface erefactor-highlight-face
  '((t (:inherit match)))
  "Face for highlighting of matches."
  :group 'erefactor)

(defvar erefactor-highlight-face 'erefactor-highlight-face)

(defun erefactor-highlight-update-region (start end regexp &optional ignore-case check)
  "highlight START to END word that match to REGEXP.
CHECK is function that accept no arg and return boolean."
  (save-match-data
    (save-excursion
      (goto-char start)
      (let ((case-fold-search ignore-case))
        (while (and (re-search-forward regexp nil t)
                    (< (point) end))
          (when (or (null check)
                    (save-match-data (funcall check)))
            (let ((ov (make-overlay (match-beginning 0) (match-end 0))))
              (overlay-put ov 'priority 1) ;; few value
              (overlay-put ov 'face erefactor-highlight-face)
              (overlay-put ov 'erefactor-overlay-p t))))))))

(defun erefactor-dehighlight-all ()
  (save-match-data
    (mapc
     (lambda (ov)
       (when (overlay-get ov 'erefactor-overlay-p)
         (delete-overlay ov)))
     (overlays-in (point-min) (point-max)))))

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

(defun erefactor--call-before (func old-name capture new-name)
  (save-match-data
    (if func 
        (funcall func old-name capture new-name)
      (y-or-n-p "Rename? "))))

(defun erefactor--call-after (func old-name new-name)
  (save-match-data
    (when func
      (funcall func old-name new-name))))

(defun erefactor-rename-region (symbol new-symbol &optional region before-func after-func)
  "Rename SYMBOL to NEW-SYMBOL in REGION.
Optional arg BEFORE-FUNC is not used currently (TODO). 
    But called with three args SYMBOL and NEW-SYMBOL before replacing.
    This function must return non-nil value if executing to replace.
Optional arg AFTER-FUNC is called with two args SYMBOL and NEW-SYMBOL after replaced."
  (let ((start (if region (car region) (point-min)))
        (end (save-excursion
               (goto-char (if region (cdr region) (point-max)))
               (point-marker)))
        regexp)
    (when (or (not (erefactor-already-bounded new-symbol start end))
              (y-or-n-p (format "%s is already bound. Continue? " new-symbol)))
      (save-excursion
        (setq erefactor--region-start start)
        (setq erefactor--region-end end)
        (goto-char start)
        (setq regexp (erefactor-create-regexp symbol))
        ;; cannot use narrow-to-region because is unnatural while interactive loop
        (while (and (re-search-forward regexp nil t)
                    (< (point) end))
          (let ((target (match-string 0)))
            (goto-char (match-end 1))
            (erefactor-re-highlight-in-interactive regexp (match-beginning 1) (match-end 1))
            (unwind-protect
                (when (erefactor--call-before before-func symbol target new-symbol)
                  (replace-match new-symbol nil nil nil 1)
                  (erefactor--call-after after-func symbol new-symbol))
              (erefactor-dehighlight-in-interactive))))))))

(defun erefactor-change-symbol-prefix (prefix new-prefix &optional before-func after-func)
  "Switch symbol PREFIX to NEW-PREFIX in buffer.
Optional arg BEFORE-FUNC is not used currently (TODO). 
    But called with three args SYMBOL and NEW-SYMBOL before replacing.
    This function must return non-nil value if executing to replace.
Optional arg AFTER-FUNC is called with two args old-name and new-name after replaced."
  (save-excursion
    (setq erefactor--region-start (point-min))
    (setq erefactor--region-end (point-max))
    (goto-char (point-min))
    (let ((regexp (erefactor-create-prefixed-regexp prefix)))
      ;; cannot use narrow-to-region because is unnatural while interactive loop
      (while (re-search-forward regexp nil t)
        (goto-char (match-end 1))
        (erefactor-re-highlight-in-interactive regexp (match-beginning 2) (match-end 2))
        (let* ((target (match-string 0))
               (suffix (match-string 3))
               (old-name (concat prefix suffix))
               (new-name (concat new-prefix suffix)))
          (unwind-protect
              (when (erefactor--call-before before-func old-name target new-name)
                (replace-match new-prefix nil nil nil 2)
                (erefactor--call-after after-func old-name new-name))
            (erefactor-dehighlight-in-interactive)))))))

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
  "Highlight current symbol in this buffer.
Force to dehighlight \\[erefactor-dehighlight-all-symbol]"
  (interactive)
  (let ((symbol (thing-at-point 'symbol)))
    (erefactor-dehighlight-all)
    (unless symbol
      (error "No symbol at point"))
    (erefactor-highlight-update-region 
     (point-min) (point-max) (erefactor-create-regexp symbol))
    (erefactor-lazy-highlight-suspend)))

(defun erefactor-dehighlight-all-symbol ()
  "Dehighlight the all highlighted symbols in this buffer."
  (interactive)
  (erefactor-dehighlight-all)
  (erefactor-lazy-highlight-resume))

;;
;; experimental
;;

(define-minor-mode erefactor-highlight-mode
  "Toggle highlight mode on or off.
In highlight mode, the highlight the current symbol if recognize as a local variable.
"
  :group 'erefactor
  (if erefactor-highlight-mode
      (erefactor-lazy-highlight--start)
    (erefactor-lazy-highlight--stop)
    (erefactor-lazy-highlight--dehihglight)))

(defun erefactor-lazy-highlight-turn-on ()
  (erefactor-highlight-mode 1))

(defun erefactor-lazy-highlight-suspend ()
  (setq erefactor-lazy-highlight--previous t))

(defun erefactor-lazy-highlight-resume ()
  (setq erefactor-lazy-highlight--previous nil))

(defvar erefactor-lazy-highlight--timer nil)

;;FIXME if highlight current symbol 
;; -> change symbol name
;; -> quickly back to highlighted point
(defvar erefactor-lazy-highlight--previous nil)
(make-variable-buffer-local 'erefactor-lazy-highlight--previous)

(defun erefactor-lazy-highlight--stop ()
  (when erefactor-lazy-highlight--timer
    (unless (catch 'found
              (mapc
               (lambda (buf)
                 (with-current-buffer buf
                   (when erefactor-highlight-mode
                     (throw 'found t))))
               (buffer-list))
              nil)
      (cancel-timer erefactor-lazy-highlight--timer)
      (setq erefactor-lazy-highlight--timer nil))))

(defun erefactor-lazy-highlight--start ()
  (or
   (and erefactor-lazy-highlight--timer
        (memq erefactor-lazy-highlight--timer timer-idle-list))
   (setq erefactor-lazy-highlight--timer
         (run-with-idle-timer idle-update-delay t 
                              'erefactor-lazy-highlight--highlight))))

(defun erefactor-lazy-highlight--dehihglight ()
  (erefactor-dehighlight-all)
  (setq erefactor-lazy-highlight--previous nil))

(defun erefactor-lazy-highlight--highlight ()
  (condition-case nil
      (cond
       ;; ignore when other command executing.
       ;; ex: erefactor-rename-symbol-*
       (this-command)
       ((not erefactor-highlight-mode))
       ;; t means suppress lazy highlight
       ((eq erefactor-lazy-highlight--previous t))
       ((and erefactor-lazy-highlight--previous 
             (= erefactor-lazy-highlight--previous (point))))
       (t
        (erefactor-lazy-highlight--dehihglight)
        (setq erefactor-lazy-highlight--previous (point)) ;; save point
        (let ((symbol (thing-at-point 'symbol)))
          (when symbol
            (let ((region (erefactor--find-local-binding symbol)))
              (when region
                (erefactor-highlight-update-region 
                 (car region) (cdr region)
                 (erefactor-create-regexp symbol)
                 nil 'erefactor-context-code-p)))))))
    ;; completely ignore all errors
    (error nil)))

(defcustom erefactor-lint-emacsen nil
  "*Emacs executables.

Examples:
\(setq erefactor-lint-emacsen
    '\(\"emacs-21\" \"emacs-22.1\" \"emacs-23.2\" \"emacs-current\"))
"
  :group 'erefactor
  :type '(list file))

(defcustom erefactor-lint-path-alist nil
  "*Associate list key is file name of Elisp. 
value is `load-path' that required by key file if key file require some module.

Examples:
\(setq erefactor-lint-path-alist
   '\((\"/home/bob/.emacs.d/linting-file.el\"
       \"/home/bob/.emacs.d/misc\"))


\"/home/bob/.emacs.d/misc\" directory have some requiring module(s).
"
  :group 'erefactor
  :type '(list (list file)))

(defun erefactor-lint ()
  "Execuet Elint in new Emacs process."
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
  "Execuet Elint in new Emacs processes.
See variable `erefactor-lint-emacsen'."
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

    (define-key map "L" 'erefactor-lint-by-emacsen)
    (define-key map "R" 'erefactor-rename-symbol-in-package)
    (define-key map "c" 'erefactor-change-prefix-in-buffer)
    (define-key map "d" 'erefactor-dehighlight-all-symbol)
    (define-key map "h" 'erefactor-highlight-current-symbol)
    (define-key map "l" 'erefactor-lint)
    (define-key map "r" 'erefactor-rename-symbol-in-buffer)

    (setq erefactor-map map)))

;;
;; unit test
;;

(dont-compile
  (when (fboundp 'expectations)

    (expectations 
     (expect t (erefactor--local-binding-p 'v '(defun f (v))))
     (expect t (erefactor--local-binding-p 'v '(lambda (v))))
     (expect t (erefactor--local-binding-p 'v '(let ((v v1)))))
     (expect t (erefactor--local-binding-p 'tag '(catch 'tag)))

     (expect nil (erefactor--local-binding-p 'v '(defun f (v1) v)))
     (expect nil (erefactor--local-binding-p 'v '(lambda (v1) v)))
     (expect nil (erefactor--local-binding-p 'v '(let ((v1 val)) v)))
     (expect nil (erefactor--local-binding-p 'tag '(catch 'tag1 tag)))

     (expect t (erefactor--macroexpand-contains-p 'v '(defun* f (v))))
     (expect t (erefactor--macroexpand-contains-p 'k1 '(defun* f (v1 &key k1))))
     (expect nil (erefactor--macroexpand-contains-p 'v '(defun* f (v1) v)))

     ;; check ignoring failed (defface) form expansion
     (expect nil (erefactor--macroexpand-contains-p 'v1 '(when test (case v1 (defface)))))

     ;; cannot test `erefactor--local-fbinding' because that move point.
     )))

;; (expectations-execute)

(provide 'erefactor)

;;; erefactor.el ends here
