;;; gtk-perl.el --- A minor mode to help write gtk-perl code

;; Copyright 2007 Ye Wenbin
;;
;; Author: wenbinye@163.com
;; Version: $Id: gtk-perl.el,v 0.0 2007/07/21 11:40:54 ywb Exp $
;; Keywords:
;; X-URL: not distributed yet

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
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Features:
;; 1. Partial complete package name, methods name, signals or other
;;    enum type argument.
;;    That means you can complete like this:
;;        my $button = Gtk2::Button->n_f_s<TAB>
;;    This will get a right expansion `new_from_stock'. The
;;    completion is done with some syntax analysis, so you won't get a
;;    completion like `new_from_string'. The completions table will
;;    limited to all methods of Gtk2::Button.
;;
;; 2. Eldoc support for methods of gtk-perl.
;;    When you write the `new_from_stock' function, the minibuffer
;;    will echo the arguments of the function as:
;;      (Gtk2::Button) widget = new_from_stock($stock_id)
;;
;; 3. Describe functions.
;;
;;; Comments:
;; A file contain documentation info can be download from:
;;  http://www.emacswiki.org/cgi-bin/emacs/GtkPerlData
;;
;; Rename the file downloaded with extension tar.gz if need.
;; Uncompress it and put the file gtk-doc.txt to load-path.

;;; Put all file into your load-path and the following into your ~/.emacs:
;; (autoload 'gtk-perl-mode "gtk-perl"
;;   "A minor mode to help write gtk-perl code" t)
;; (add-hook 'cperl-mode-hook
;;           (lambda () (gtk-perl-mode 1)))

;; If you don't won't turn on it every where, you can add a hook like this:
;; (add-hook 'cperl-mode-hook
;;           (lambda ()
;;             (save-excursion
;;               (goto-char (point-min))
;;               (if (re-search-forward "use Gtk2" 1000 t)
;;                   (gtk-perl-mode 1))))
;;; Code:

(provide 'gtk-perl)
(eval-when-compile
  (require 'cl))
(require 'cperl-mode)
(require 'complete)

(defvar gtk-perl-doc-file (locate-file "gtk-doc.txt" load-path)
  "*The name of file that has gtk-perl document information.")

(defvar gtk-perl-mark-enums t
  "*non-nil to show the enum type of arguments with star.")

(defvar gtk-perl-arg-handler
  '(("Gtk2::.*signal_connect" . gtk-perl-signal-completion-table)
    ("." . gtk-perl-type-completion-table))
  "Functions to generate completion table.
Each element contain a pair like (REGEXP . HANDLER). If a method
match the REGEXP, the HANDLER will be used to generate a
completion for current argument. The HANDLER can be a function
will recept two argument: the package and name of the method. It
can also be a variable whose value can be used as collection. It
also can be a list.")

(defvar gtk-perl-doc-buffer nil
  "Buffer visit `gtk-perl-doc-file'.")

(defvar gtk-perl-magic-number 211
  "The length of the `gtk-perl-obarray'.")

(defvar gtk-perl-obarray nil
  "All symbols used for `PC-complete'.
If you want do more partial completions, you can add more word to
this table use function `intern'. It won't interference
completing for gtk-perl.")

(defvar gtk-perl-index nil
  "A index to keep hierarchy information of gtk module.
Every element of the vector is a pair as (MODULE HIERARCHY) The
cdr of the list is the hierarchy list of this module, the number
indicate the index of the module in the vector.")

(defvar gtk-perl-section-index nil
  "The offset of section.")

;; Is it needed?
(defvar gtk-perl-cache nil
  "Cache the result of `gtk-perl-get-inherit'")

(defvar gtk-perl-object-package-cache nil
  "Cache the candicate package for object.
This is a list use for determing the package of object from it's
name. The structure is like ((REGEXP . PACKAGE) ...). If the
object name match the REGEXP, so it belongs to that PACKAGE.

The similar variable is `gtk-perl-method-package-cache' which
used for search the package of function. But it only affect
certain functions which maybe more precise.
")

(defvar gtk-perl-method-package-cache nil
  "Cache the candicate package for method.
This is a list use for `gtk-perl-guess-method-package', the structure is like:
  ((METHOD  . ((REGEXP . PACKAGE)
               ...))
   ...)
")

(defvar gtk-perl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\t" 'gtk-perl-complete-function)
    (define-key map "\C-c\C-hf" 'gtk-perl-describe-function)
    map)
  "Key map of `gtk-perl-mode'")

(define-minor-mode gtk-perl-mode
  "A minor mode to help write gtk-perl code.

\\{gtk-perl-mode-map}
"
  :lighter " Gtk2"
  :keymap gtk-perl-mode-map
  (when gtk-perl-mode
    (unless gtk-perl-obarray
      (gtk-perl-load-doc))
    (set (make-local-variable 'eldoc-documentation-function)
         'gtk-perl-documentation-function)
    (eldoc-mode 1)))

(defun gtk-perl-complete-function ()
  "Do partial completion.
If current word is a package name or a method that didn't appear
in `gtk-perl-file', the completion table will set to
`gtk-perl-obarray', otherwise, if the object is found as a
package of Gtk2, the methods to complete will limit to that
package."
  (interactive)
  (let* ((end (point))
         (minibuffer-completion-predicate 'identity)
         (PC-not-minibuffer t)
         beg pkg
         minibuffer-completion-table)
    (if (gtk-perl-in-arglist-p)
        (setq beg (save-excursion
                    (skip-chars-backward "A-Za-z0-9_-")
                    (point))
              minibuffer-completion-table
              (or (gtk-perl-arg-completion-table) gtk-perl-obarray))
      ;; Maybe complete modules
      (unless (and (looking-back "::[A-Za-z0-9_]*")
                   (setq beg (save-excursion
                               (backward-sexp 1)
                               (point)))
                   (setq minibuffer-completion-table
                         (all-completions (buffer-substring beg end) gtk-perl-obarray)))
        (setq beg (save-excursion
                    (skip-chars-backward "A-Za-z0-9_")
                    (point)))
        ;; not complete variable
        (if (memq (char-before beg) '(?% ?$ ?@))
            (setq minibuffer-completion-table nil)
          (setq pkg (gtk-perl-get-package))
          (if (and pkg (string-match "^\\$" pkg))
              (setq pkg (gtk-perl-guess-object-package pkg)))
          (setq minibuffer-completion-table
                (if (intern-soft pkg gtk-perl-obarray)
                    (gtk-perl-get-inherit pkg "Methods")
                  gtk-perl-obarray)))))
    (if (equal last-command 'PC-lisp-complete-symbol)
        (PC-do-completion nil beg PC-lisp-complete-end t)
      (if PC-lisp-complete-end
          (move-marker PC-lisp-complete-end end)
        (setq PC-lisp-complete-end (copy-marker end t)))
      (PC-do-completion nil beg end t))))

(defun gtk-perl-describe-function (func)
  "Show the prototype and posible values of every argument for the FUNC."
  (interactive
   (list (let ((method (gtk-perl-method-ap t))
               prototype)
           (if (and method
                    (setq prototype
                          (gtk-perl-method-prototype (car method) (cdr method))))
               (setq method (gtk-perl-method-full-name (cons (car prototype) (cdr method))))
             (setq method nil))
           (gtk-perl-read-method
            (if method
                (format "Describe function(default %s): " method)
              "Describe function: ")
            method))))
  (let (pkg method prototype)
    (if (string-match func "::")
        (setq pkg nil
              method func)
      (setq pkg (split-string func "::")
            method (car (last pkg))
            pkg (mapconcat 'identity (nbutlast pkg) "::")))
    (if (setq prototype (gtk-perl-method-prototype pkg method))
        (with-output-to-temp-buffer (help-buffer)
          (princ (format "Arguments of %s:\n" func))
          (princ (gtk-perl-format-method-arg prototype))
          (princ "\n")
          (princ "\n")
          (setq types (delq nil (mapcar (lambda (n)
                                          (and (cadr n)
                                               n))
                                        (gtk-perl-parse-arg (cdr prototype)))))
          (when types
            (princ "Posible enum types:\n")
            (dolist (type types)
              (princ (format "%s belongs to %s which can be one of these values:\n"
                             (car type) (cadr type)))
              (princ
               (with-current-buffer gtk-perl-doc-buffer
                 (goto-char (gtk-perl-get-pos (cadr type) "Enums"))
                 (buffer-substring (point)
                                   (progn (if (re-search-forward "^[#[]" nil t)
                                              (- (point) 1)
                                            (point-max))))))
              (princ "\n")))
          (with-current-buffer standard-output
            (goto-char (point-min))
            (if (re-search-forward "Arguments of \\(.*\\):$" nil t)
                (make-text-button (match-beginning 1)
                                  (match-end 1)
                                  'method (list pkg method)
                                  'action
                                  (lambda (but)
                                    (apply 'gtk-perl-find-method
                                           (button-get but 'method)))))))
      (error "No description found for %s!" func))))

(defun gtk-perl-documentation-function ()
  "Get the funciton documentation string.
If the function is a gtk-perl method, this will search in
`gtk-perl-doc-file'. Otherwise call `cperl-get-help'."
  (save-excursion
    (let ((method (gtk-perl-method-ap))
          (cperl-message-on-help-error nil)
          pkg prototype)
      (when method
        (setq pkg (car method))
        ;; It is a gtk method
        (if (and (intern-soft (cdr method) gtk-perl-obarray)
                 (setq prototype (gtk-perl-method-prototype pkg (cdr method))))
            (gtk-perl-format-method-arg prototype)
          (car (cperl-describe-perl-symbol (gtk-perl-method-full-name method))))))))

(defun gtk-perl-load-doc ()
  "Load `gtk-perl-doc-file'.
Fill `gtk-perl-obarray', read `gtk-perl-index', generate
`gtk-perl-section-index' and so on."
  (if (and gtk-perl-doc-file (file-exists-p gtk-perl-doc-file))
      (progn
        (setq gtk-perl-doc-buffer (get-buffer-create " *gtk-perl-doc*"))
        (with-current-buffer gtk-perl-doc-buffer
          ;; for reenter the function
          (erase-buffer)
          (setq gtk-perl-section-index nil)
          (insert-file-contents gtk-perl-doc-file)
          (goto-char (point-min))
          (setq gtk-perl-index (read (current-buffer))
                gtk-perl-obarray (make-vector gtk-perl-magic-number nil))
          (dotimes (i (length gtk-perl-index))
            (put (intern (car (aref gtk-perl-index i))
                         gtk-perl-obarray)
                 'package t))
          ;; intern all functions
          (save-excursion
            (re-search-forward "\\[Methods]")
            (while (and (not (eobp))
                        (not (= (char-after) ?\[)))
              (when (looking-at "\\(^[a-z_]+\\)(")
                (let ((sym (intern (match-string 1) gtk-perl-obarray)))
                  (set sym (cons (point)
                                 (if (boundp sym)
                                     (symbol-value sym)
                                   nil)))))
              (forward-line 1)))
          (while (setq index (gtk-perl-section-index t))
            (push index gtk-perl-section-index))
          (setq gtk-perl-section-index (nreverse gtk-perl-section-index))
          ;; some package can be not appear in gtk-perl-index
          (dolist (pkg (cdr (assoc "Methods" gtk-perl-section-index)))
            (put (intern (car pkg) gtk-perl-obarray) 'package t))))
    (error "Can't find document file!")))

(defun gtk-perl-section-index (&optional omit-null)
  "Return all position of package in current section."
  (let (header index end)
    (when (re-search-forward "^\\[" nil t)
      (setq header (buffer-substring (point)
                                     (progn (re-search-forward "]" (line-end-position))
                                            (1- (point)))))
      (setq end (save-excursion (if (re-search-forward "^\\[" nil t)
                                    (1- (point))
                                  (point-max))))
      (forward-line 1)
      (while (re-search-forward "^#\\s-*" end t)
        (push (cons (buffer-substring (point)
                                      (progn
                                        (forward-line 1)
                                        (1- (point))))
                    (point))
              index))
      (and (or (not omit-null) index)
           (cons header index)))))

(defun gtk-perl-arg-completion-table ()
  (let* ((method (gtk-perl-method-ap))
         (pkg (if (and (car method)
                       (string-match "^[a-zA-Z_]" (car method)))
                  (car method)
                (gtk-perl-guess-method-package (car method) (cdr method))))
         (name (cdr method))
         (handler (assoc-default (if pkg
                                     (concat pkg "::" name)
                                   name)
                                 gtk-perl-arg-handler 'string-match)))
    (and handler
         (cond ((boundp handler)
                (symbol-value handler))
               ((fboundp handler)
                (funcall handler pkg name))
               ((listp handler)
                handler)))))

(defun gtk-perl-signal-completion-table (pkg method)
  (gtk-perl-get-inherit pkg "Signals"))

(defun gtk-perl-in-arglist-p ()
  "Test whether the point is in arguments list."
  (let ((pps (parse-partial-sexp (save-excursion
                                   (if (beginning-of-defun)
                                       (point)
                                     (point-min))) (point))))
    (and (cadr pps)
         (save-excursion
           (goto-char (cadr pps))
           (gtk-perl-methodp)))))

(defun gtk-perl-parse-arg (arg)
  "Return parsed arguments list.
The ARG is from the `gtk-perl-doc-buffer', which can be get by
`gtk-perl-method-arg'. Every argument of the method will split to
a pair of variable name and the enum type(if there is any)"
  (if (and (string-match "(\\(.*\\))" arg)
           (> (length (match-string 1 arg)) 0))
      (mapcar (lambda (s)
                (if (string-match "<" s)
                    (list (substring s 0 (match-beginning 0))
                          (substring s (match-end 0) -1))
                  (list s)))
              (split-string (match-string 1 arg) ",\\s-*"))))

(defun gtk-perl-arg-index ()
  "If the point in argument list of some method, return the index of current argument."
  (save-excursion
    (let ((pps (parse-partial-sexp (save-excursion
                                     (if (beginning-of-defun)
                                         (point)
                                       (point-min))) (point)))
          (end (point))
          (index 0)
          start)
      (when (and (cadr pps)
                 (save-excursion
                   (goto-char (cadr pps))
                   (skip-chars-forward " \t\n")
                   (= (char-after) ?\()))
        (setq start (1+ (cadr pps)))
        (goto-char start)
        (while (re-search-forward "\\(,\\|=>\\)" end t)
          (setq pps (parse-partial-sexp start (point)))
          ;; not in string or in deep paren
          (if (and (= (car pps) 0)
                   (not (nth 3 pps))
                   (not (nth 4 pps)))
              (setq index (1+ index))))
        index))))

(defun gtk-perl-type-completion-table (mod method)
  (let ((arg (cdr (gtk-perl-method-prototype mod method)))
        (start 0)
        (index (gtk-perl-arg-index))
        types)
    (when arg
      (setq types (gtk-perl-parse-arg arg))
      (and types index
           (< index (length types))
           (cadr (nth index types))
           (gtk-perl-get (cadr (nth index types)) "Enums")))))

(defun gtk-perl-parse-obj (obj)
  "Search the package that the OBJ belongs to from the context.
This will search \"$obj = Package->new\" to get the package name of
the object $obj."
  (if (save-excursion
        (re-search-backward (concat (regexp-quote obj)
                                    "\\s-*=\\s-*\\(\\([a-z0-9A-Z]+::\\)*[a-z0-9A-Z]+\\)\\s-*->\\s-*new")
                            nil t))
      (match-string 1)
    obj))

(defun gtk-perl-get-package ()
  "Return the package name at point.
If the text before point is like \"$var->\", the package name
will be the package of the $var. It is done by
`gtk-perl-parse-obj'. If the text is like \"Package->\", return
\"Package\" in stead."
  (cond ((looking-back "\\([A-Z0-9a-z_]+::\\)+[A-Z0-9a-z_]*")
         (mapconcat 'identity
                    (nbutlast (split-string
                               (buffer-substring
                                (save-excursion (backward-sexp 1) (point))
                                (point)) "::")) "::"))
        ((looking-back "\\s-*->\\s-*[a-z_]*")
         (save-excursion
           (goto-char (match-beginning 0))
           (with-syntax-table cperl-mode-syntax-table
             (backward-sexp 1))
           (when (looking-at "\\(\\$\\)?\\(\\([a-z0-9A-Z]+::\\)*[a-z0-9A-Z_]+\\)")
             (if (match-string 1)
                 (gtk-perl-parse-obj (concat "$" (match-string 2)))
               (match-string 2)))))))

(defun gtk-perl-guess-object-package (obj)
  "Guess what the package of the OBJ.
First try search OBJ in `gtk-perl-object-package-cache'. If failed,
search backward for \"$obj->method\" and use
`gtk-perl-guess-method-package' to determine the package of the
object. Nothing found return OBJ.
"
  (let ((pkg (assoc-default obj gtk-perl-object-package-cache 'string-match)))
    (or pkg
        (save-excursion
          (when (and
                 (re-search-backward (regexp-quote obj) nil t)
                 (re-search-backward (concat (regexp-quote obj)
                                             "\\s-*->\\s-*\\([a-zA-Z0-9_]+\\)") nil t)
                 (setq pkg (gtk-perl-guess-method-package obj (match-string 1))))
            (add-to-list 'gtk-perl-object-package-cache (cons (regexp-quote obj) pkg))
            pkg))
        obj)))

(defun gtk-perl-guess-method-package (pkg method)
  "Guess what the package of method.
First try search in `gtk-perl-method-package-cache'. The try
guess package from obj using `gtk-perl-object-package-cache'. If
not found, if PKG is nil search method in package main. If
failed, use the first PKG that contain the method. If PKG is an
object, omit the main package, search other packages.
"
  (let ((sym (intern-soft method gtk-perl-obarray))
        (cache (assoc method gtk-perl-method-package-cache))
        pos end found)
    (or pkg (setq pkg "main"))
    ;; the method should be registered
    (when sym
      (or (and cache (assoc-default pkg (cdr cache) 'string-match))
          (when (string= pkg "main")
            (setq pos (gtk-perl-get-pos "main" "Methods"))
            (with-current-buffer gtk-perl-doc-buffer
              (goto-char pos)
              (setq end (save-excursion
                          (if (re-search-forward "^[#[]" nil t)
                              (point)
                            (point-max))))
              (if (re-search-forward (concat "^" (regexp-quote method)) end t)
                  "main")))
          (let ((res (assoc-default pkg gtk-perl-object-package-cache 'string-match)))
            (and res (assoc method (gtk-perl-get-inherit res "Methods")) res))
          (with-current-buffer gtk-perl-doc-buffer
            (setq pos (symbol-value sym))
            (while (and (not found) pos)
              (goto-char (car pos))
              (re-search-backward "^#\\s-*")
              (goto-char (match-end 0))
              (if (string= (buffer-substring (point) (line-end-position)) "main")
                  (setq pos (cdr pos))
                (setq found (buffer-substring (point) (line-end-position)))))
            found)))))

(defun gtk-perl-object-ap ()
  (let ((method (gtk-perl-method-ap))
        obj)
    (if (and method (string-match "^\\$" (car method)))
        (setq obj (car method))
      (save-excursion
        (if (= (char-after) ?\$)
            (concat "$" (progn (forward-char 1) (current-word)))
          (if (gtk-perl-in-arglist-p)
              (up-list -1))
          (if (or (= (char-after) ?\$)
                  (re-search-backward "\\$" (line-beginning-position) t))
              (concat "$" (progn (forward-char 1) (current-word)))))))))

(defun gtk-perl-set-object-package (pkg obj)
  (interactive
   (let ((obj (gtk-perl-object-ap)))
     (unless obj
       (error "No object found at point!"))
     (list (completing-read (format "Package of %s: " obj)
                            gtk-perl-obarray (lambda (s) (get s 'package)) t)
           obj)))
  (let ((old (assoc (regexp-quote obj) gtk-perl-object-package-cache)))
    (if old
        (setcdr old pkg)
      (add-to-list 'gtk-perl-object-package-cache
                   (cons (regexp-quote obj) pkg)))))

(defun gtk-perl-set-method-package (pkg obj method)
  (interactive 
   (let ((method (gtk-perl-method-ap))
         packages)
     (unless method
       (error "No method found at point!"))
     (setq packages (gtk-perl-get-method-packages (cdr method)))
     (unless packages
       (error "No package have the method \"%s\"" (cdr method)))
     (if (= (length packages) 1)
         (error "Not nessisary to set package for %s, only %s has the method."
                (cdr method) (car packages)))
     (list (completing-read (format "Package of %s: "
                                    (if (null (car method))
                                        (cdr method)
                                      (concat (cdr method) " for " (car method))))
                            packages)
           (car method)
           (cdr method))))
  (let ((cache (assoc method gtk-perl-method-package-cache))
        (regexp (concat "^" (if (null obj)
                                "main"
                              (regexp-quote obj)) "$"))
        tmp)
    (if (null cache)
        (add-to-list 'gtk-perl-method-package-cache
                     (cons method (list (cons regexp pkg))))
      (if (setq tmp (assoc regexp (cdr cache)))
          (setcdr tmp pkg)
        (setq gtk-perl-method-package-cache
              (cons (cons method (cons (cons regexp pkg)
                                       (cdr cache)))
                    (delq cache gtk-perl-method-package-cache)))))))

(defun gtk-perl-get-ancestor (pkg)
  "Search the ancestors of PKG in `gtk-perl-index'."
  (let ((i 0) info found)
    (when pkg
      (while (and (< i (length gtk-perl-index)) (not found))
        (if (string= (car (aref gtk-perl-index i)) pkg)
            (setq found t)
          (setq i (1+ i))))
      (if found (aref gtk-perl-index i)))))

(defun gtk-perl-get-pos (pkg type)
  "Get the position of method or signal in `gtk-perl-doc-buffer' of PKG.
If PKG is null, search TYPE in package main."
  (cdr (assoc (or pkg "main") (cdr (assoc type gtk-perl-section-index)))))

(defun gtk-perl-get (pkg type &optional regexp)
  "Get the methods or signals of the package PKG.
The REGEXP is used to extract the method or signal name from
`gtk-perl-doc-buffer'."
  (or regexp (setq regexp "^\\([-A-Za-z0-9_]+\\)"))
  (let (methods pos)
    (when (setq pos (gtk-perl-get-pos pkg type))
      (with-current-buffer gtk-perl-doc-buffer
        (goto-char pos)
        (while (and (not (eobp))
                    (not (looking-at "^#")))
          (if (re-search-forward regexp (line-end-position) t)
              (push (match-string 1) methods))
          (forward-line 1)))
      methods)))

(defun gtk-perl-get-inherit (pkg type &optional regexp)
  "Get all methods or signals of package PKG.
The methods inherited from the ancestors of the package will be
also included. An association list with structure like ((METHOD .
INDEX) ...) will be return. The methods in the alist is with the
same order as the ancestors in `gtk-perl-index', so use `assoc'
can get the current used method in the package rather than the
shadowed methods. The INDEX can be -1, if the pkg don't appear in
`gtk-perl-index'."
  (if (and (string= pkg (car gtk-perl-cache))
           (string= type (cadr gtk-perl-cache)))
      (nth 2 gtk-perl-cache)
    (let ((ancestors (gtk-perl-get-ancestor pkg))
          methods)
      (if ancestors
          (setq methods
                (apply 'append
                       (mapcar
                        (lambda (i)
                          (mapcar (lambda (n) (cons n i))
                                  (gtk-perl-get (car (aref gtk-perl-index i)) type regexp)))
                        (cdr ancestors))))
        (setq methods
              (mapcar (lambda (n) (cons n -1))
                      (gtk-perl-get pkg type regexp))))
      (setq gtk-perl-cache (list pkg type methods))
      methods)))

(defun gtk-perl-method-prototype (pkg method)
  "Return the prototype of PKG::METHOD."
  (let (methods anc)
    ;; if pkg is nil or $obj
    (if (or (null pkg)
            (string= pkg "main")
            (string-match "^\\$" pkg))
        (setq pkg (gtk-perl-guess-method-package pkg method)))
    (when (and pkg
               (intern-soft pkg gtk-perl-obarray)
               (get (intern-soft pkg gtk-perl-obarray) 'package))
      (setq methods (gtk-perl-get-inherit pkg "Methods"))
      (when methods
        (with-current-buffer gtk-perl-doc-buffer
          (setq index (cdr (assoc method methods)))
          (if (and index (> index -1))
              (setq anc (car (aref gtk-perl-index index)))
            (setq anc pkg))
          (goto-char (gtk-perl-get-pos anc "Methods"))
          (and (re-search-forward (concat "^" (regexp-quote method) "(")
                                  (save-excursion
                                    (if (re-search-forward "^[#[]" nil t)
                                        (point)
                                      (point-max))) t)
               (cons pkg (buffer-substring (line-beginning-position) (line-end-position)))))))))

(defun gtk-perl-format-method-arg (prototype)
  (let ((arg (cdr prototype)))
    (format "(%s) %s" (car prototype)
            (if (string-match " => " arg)
                (concat (substring  arg (match-end 0))
                        " = "
                        (replace-regexp-in-string
                         "<[^>]*>"
                         (if gtk-perl-mark-enums "*" "")
                         (substring arg 0 (match-beginning 0))))
              (replace-regexp-in-string "<[^>]*>"
                                        (if gtk-perl-mark-enums "*" "")
                                        arg)))))

;; It force all method should call like "foo(args)".
;; I don't know how to handle function call like "foo args"
(defun gtk-perl-methodp (&optional ignore-paren)
  (and (or ignore-paren (= (char-after) ?\())
       (looking-back "\\s-*\\(->\\)?\\s-*[a-zA-Z0-9_]+\\s-*")))

(defun gtk-perl-method-ap (&optional ignore-paren)
  "Return method and package at POS.
Only support for four type of function call:
 1. package::method()
 2. package->method()
 3. $obj->method()
 4. method()
The first two return (package . method), and the third one will
search $obj declaration by `gtk-perl-get-package', if failed, the
package is set to $obj. The fourth one return (nil . method).
"
  (catch 'not-found
    (save-excursion
      (and (memq (char-syntax (char-after)) '(?w ?_))
           (forward-sexp 1))
      (skip-chars-forward " \t\n")
      ;; if we at a function
      (unless (gtk-perl-methodp ignore-paren)
        (let ((pps (parse-partial-sexp (save-excursion
                                         (if (beginning-of-defun)
                                             (point)
                                           (point-min))) (point))))
          ;; if we are in parent for function arguemnt
          (cond ((cadr pps)
                 (goto-char (cadr pps)))
                ;; or just finish the arguemnt list
                ((nth 2 pps)
                 (goto-char (nth 2 pps)))
                (t (throw 'not-found nil)))
          (unless (gtk-perl-methodp)
            (throw 'not-found nil))))
      (skip-chars-backward " \t\n")
      (cons (gtk-perl-get-package)
            (buffer-substring
             (save-excursion
               (skip-chars-backward "A-Za-z0-9_")
               (point)) (point))))))

(defun gtk-perl-method-full-name (method)
  "Return full name of METHOD.
Concat package and method with \"::\" if there is a package."
  (and method
       (if (car method)
           (concat (car method) "::" (cdr method))
         (cdr method))))

(defun gtk-perl-read-method (prompt &optional default)
  "Completing read a method registered."
  (completing-read
   prompt
   (lambda (str pred flag)
     (let (pkg method)
       (if (string-match str "::")
           (setq pkg nil
                 method str)
         (setq pkg (split-string str "::")
               method (car (last pkg))
               pkg (mapconcat 'identity (nbutlast pkg) "::")))
       (cond ((eq flag 'lambda)
              (or (test-completion str gtk-perl-obarray (lambda (s) (get s 'package)))
                  (test-completion method (gtk-perl-get-inherit pkg "Methods"))))
             ((null flag) str)
             (t
              (nconc
               (mapcar
                (lambda (mod)
                  (concat mod "::"))
                (all-completions str gtk-perl-obarray (lambda (s) (get s 'package))))
               (all-completions str
                                (if (intern-soft pkg gtk-perl-obarray)
                                    (mapcar (lambda (method)
                                              (concat pkg "::" (car method)))
                                            (gtk-perl-get-inherit pkg "Methods"))
                                  (gtk-perl-get "main" "Methods"))))))))
   nil t nil nil default))

(defun gtk-perl-find-method (pkg method)
  (let ((perl "perl")
        file)
    (when (string-match "\\([a-zA-Z0-9_]+::\\)*[a-zA-Z0-9_]+" pkg)
      (find-file
       (with-temp-buffer
         (call-process "perl" nil (current-buffer) nil
                       (format "-M%s" pkg)
                       "-e" (format "print $INC{'%s'}" 
                                    (concat (replace-regexp-in-string "::" "/" pkg) ".pm")))
         (goto-char (point-min))
         (if (looking-at "Can't locate")
             (error "No such module: %s" pkg)                
           (buffer-string))))
      (goto-char (point-min))
      (or (re-search-forward (concat "^\\s *sub\\s-+.*\\b" method "\\_>") nil t)
          (message "Can't find the method %s" method)))))

(defun gtk-perl-get-method-packages (method)
  (let ((sym (intern-soft method gtk-perl-obarray)))
    (when sym
      (with-current-buffer gtk-perl-doc-buffer
        (mapcar (lambda (pos)
                  (goto-char pos)
                  (re-search-backward "^#\\s-*")
                  (goto-char (match-end 0))
                  (buffer-substring (point) (line-end-position)))
                (symbol-value sym))))))

;;; gtk-perl.el ends here

