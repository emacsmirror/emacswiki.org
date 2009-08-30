;;; emboss.el --- Commands to browse and write emboss code

;; Copyright 2007 Ye Wenbin
;;
;; Author: wenbinye@gmail.com
;; Version: 0.01
;; Keywords: c, convenience
;; Requires: tempo-x

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

;;; Commentary:
;;  For useful information, See document of `emboss-mode'.

;;; TODO:
;; 1. Imenu support for sections
;; 2. Auto generate template for data and function

;;; BUGS:
;; 1. Some symbols are defined more than once. Currently only one
;;    symbol is register to `emboss-obarray'.

;; Put this file into your load-path and the following into your
;; ~/.emacs:
;; 
;;   (setq emboss-src-diretory "/path/to/emboss")
;;   ;;; set database and deprecate file if need
;;   ;; (setq emboss-database-file "/path/to/database/file")
;;   ;; (setq emboss-deprecate-file "/path/to/deprecate/file")
;;   (add-hook 'c-mode-hook 'emboss-c-mode-hook)
;;   (autoload 'emboss-c-mode-hook "emboss")
;;   (autoload 'emboss-describe-symbol "emboss"
;;      "Describe emboss data structure or functions." t)
;; 
;; To use emboss.el with help-dwim.el, put this to .emacs:
;; 
;;   (help-dwim-register
;;    '(emboss . ["a-zA-Z0-9_." emboss-obarray nil emboss-describe-symbol])
;;    t
;;    '((require 'emboss)
;;      (emboss-init)))
;; 
;;; Code:

(eval-when-compile
  (require 'cl))
(require 'tempo-x)
(require 'cc-mode)
(require 'etags)
(require 'hippie-exp)

(defgroup emboss nil
  "Commands to browse and write emboss code"
  :group 'c)

(defcustom emboss-magic-mode-regexp
  (concat "#include \"" (regexp-opt '("emboss" "ajax") t)
          "\\.h\"")
  "A regexp to determine whether to turn on `emboss-mode'.
Search the regexp in `magic-mode-regexp-match-limit'."
  :type 'regexp
  :group 'emboss)

(defcustom emboss-src-diretory nil
  "The root directory of emboss."
  :type 'directory
  :group 'emboss)

(defcustom emboss-database-file
  (when emboss-src-diretory
    (concat (file-name-as-directory emboss-src-diretory)
            (file-name-as-directory "doc")
            "efunc.dat"))
  "The file that contain documents of all symbols used in emboss."
  :type 'file
  :group 'emboss)

(defcustom emboss-deprecate-file
  (when emboss-src-diretory
    (concat (file-name-as-directory emboss-src-diretory)
            (file-name-as-directory "doc")
            "deprecated"))
  "The file that contain all deprecated symbols."
  :type 'file
  :group 'emboss)

(defcustom emboss-index-file "~/.emacs.d/emboss.idx"
  "Index file for `emboss-database-file'"
  :type 'file
  :group 'emboss)

(defvar emboss-expand-functions
  (cons 'emboss-try-complete-symbol hippie-expand-try-functions-list)
  "Use `hippie-expand' to complete symbol in `emboss-obarray'.
Set `emboss-try-complete-symbol' in the front, so try complete
emboss-symbol first")

(defvar emboss-obarray nil
  "Database that contain position of symbol in `emboss-database-file'")
(defvar emboss-symbol-chars "a-zA-Z0-9_."
  "Characters that may occur in any symbol of emboss")

(defun emboss-symbol-ap ()
  (save-excursion
    (skip-chars-backward emboss-symbol-chars)
    (buffer-substring (point)
                      (progn
                        (skip-chars-forward emboss-symbol-chars)
                        (point)))))

(defun emboss-build-index ()
  "Build index for database.
The index is a list contain symbol name and it's start and end
position in `emboss-database-file'."
  (if (not (file-exists-p emboss-database-file))
      (error "Database file is not found!"))
  (let (case-fold-search id start alist)
    (with-temp-buffer
      (insert-file-contents emboss-database-file)
      (goto-char (point-min))
      (looking-at (concat "^ID \\([" emboss-symbol-chars "]+\\)"))
      (setq id (substring-no-properties (match-string 1))
            start (point-min))
      (forward-line 1)
      (while (not (eobp))
        (or (re-search-forward (concat "^ID \\([" emboss-symbol-chars "]+\\)") nil t)
            (goto-char (point-max)))
        (push (list id (1- start) (1- (line-beginning-position))) alist)
        (setq id (match-string 1)
              start (line-beginning-position)))
      alist)))

(defun emboss-create-index ()
  "Create and save index to `emboss-index-file'."
  (interactive)
  (with-temp-buffer
    (insert "(\n")
    (dolist (id (emboss-build-index))
      (insert (format "  %S\n" id)))
    (insert ")")
    (write-region (point-min) (point-max) emboss-index-file)))

(defun emboss-parse-deprecated ()
  "Add deprecated symbol to `emboss-obarray'.
The deprecated will have a property deprecated, which value is a
cons cell whose CAR is the symbol currently used and the CDR is
the file that defines the symbol."
  (if (not (file-exists-p emboss-deprecate-file))
      (error "Deprecated file is not found!"))
  (let (module pair new)
    (with-temp-buffer
      (insert-file-contents emboss-deprecate-file)
      (goto-char (point-min))
      (while (not (eobp))
        (if (looking-at "^#")
            (setq module (buffer-substring (1+ (point)) (line-end-position)))
          (setq pair (split-string (buffer-substring (point) (line-end-position))))
          (setq new (cadr pair))
          (unless (= (aref new 0) ?\=)
            (cond ((= (aref new 0) ?\-)
                   (setq new nil))
                  ((= (aref new 0) ?\@)
                   (setq new (split-string (substring new 1)
                                           "_or_@")))
                  (t (setq new (list new))))
            (put (intern (car pair) emboss-obarray)
                 'deprecated (cons new module))))
        (forward-line 1)))))

(defun emboss-load-index ()
  "Load index and deprecated symbol to `emboss-obarray'."
  (interactive)
  (if (not (file-exists-p emboss-index-file))
      (emboss-create-index))
  (setq emboss-obarray (make-vector 1519 0))
  (emboss-parse-deprecated)
  (with-temp-buffer
    (insert-file-contents emboss-index-file)
    (goto-char (point-min))
    (let (sym)
      (dolist (id (read (current-buffer)))
        (if (setq sym (intern-soft (car id) emboss-obarray))
            (or (interactive-p) (message "%S is register twice!" sym))
          (setq sym (intern (car id) emboss-obarray)))
        (set sym (cdr id)))))
  (message "Load emboss index successfully!"))

(defun emboss-read-symbol (prompt)
  "Read an exists symbol in `emboss-obarray'"
  (emboss-init)
  (let ((cur (emboss-symbol-ap)))
    (unless (intern-soft cur emboss-obarray)
      (setq cur nil))
    (completing-read (if (> (length cur) 0)
                         (format "%s (default %s): " prompt cur)
                       (concat prompt ": "))
                     emboss-obarray nil t nil nil cur)))

;; Cache the documents. Will it cost a lot of memory?
(defun emboss-raw-document (id)
  (let ((sym (intern-soft id emboss-obarray))
        info)
    (when sym
      (or (get sym 'documentation)
          (put sym 'documentation
               (if (setq info (get sym 'deprecated))
                   (format "%s is a deprecated function which defined in %s\n%s" id (cdr info)
                           (if (car info)
                               (concat "Use " (mapconcat 'identity (car info) " or ") " instead.\n")
                             "It is obsoleted.\n"))
                 (setq info (symbol-value sym))
                 (with-temp-buffer
                   (insert-file-contents emboss-database-file nil (car info) (cadr info))
                   (buffer-string))))))))

(defun emboss-format-insert (offset doc &optional attrs buts)
  "Insert document to `help-buffer'.
ATTRS is a list of text properties to add in text. Element in
this list looks like (START END PROPERTY VALUE) which will be applied
to `put-text-property'.

BUTS is a list of buttons to make. Element in this list looks like
 (START END PROPERTY VALUE ...) which will be applied to
`make-text-button'.

All string index should begin with 0."
  (princ doc)
  (dolist (attr attrs)
    (apply 'put-text-property
           (+ offset (car attr))
           (+ offset (cadr attr))
           (nthcdr 2 attr)))
  (dolist (but buts)
    (apply 'make-text-button
           (+ offset (car but))
           (+ offset (cadr but))
           (nthcdr 2 but)))
  (+ offset (length doc)))

;;;###autoload 
(defun emboss-describe-symbol (symbol)
  "Show the documentation for the SYMBOL like `describe-function'."
  (interactive (list (emboss-read-symbol "Describe")))
  (let ((name symbol)
        (inhibit-read-only t)
        (offset 1))
    (setq symbol (intern-soft symbol emboss-obarray))
    (if (null symbol)
        (error "No symbol named %s" name))
    (help-setup-xref (list 'emboss-describe-symbol name)
                     (interactive-p))
    (with-output-to-temp-buffer (help-buffer)
      (with-current-buffer standard-output
        (if (get symbol 'deprecated)
            (emboss-format-deprecated-symbol name)
          (let ((sec (emboss-parse (emboss-raw-document name)))
                (format-alist emboss-format-alist)
                (header-alist emboss-header-alist)
                last tmp header seen formater 
                case-fold-search)
            ;; PN is a duplicate token that have different meaning
            (if (string-match "^[A-Z]" (cdr (assoc "ID" (car sec))))
                (push '("PN" . "Output") header-alist)
              (push '("PN" . "Arguments") header-alist)
              (push '("PN" . emboss-format-arguments) format-alist))
            (setq offset (apply 'emboss-format-insert offset (emboss-format-header sec)))
            (setq offset (emboss-format-insert offset "\n\n"))
            (dolist (token (cdr sec))
              (if (string= (caar token) last)
                  (setq offset (emboss-format-insert offset "\n")))
              (setq tmp (car token)
                    last (car tmp))
              (unless (member last seen)
                (setq header (cdr (assoc last header-alist))
                      seen (cons last seen))
                (when header
                  (setq offset (emboss-format-insert offset (concat "\n" header "\n")
                                                     `((1 ,(1+ (length header)) face bold))))))
              (setq offset
                    (apply 'emboss-format-insert offset
                           (funcall
                            (if (setq formater (cdr (assoc (caar token) format-alist)))
                                formater 'emboss-format-generic)
                            token))))))
        (print-help-return-message)))))

(defun emboss-format-deprecated-symbol (name)
  (save-excursion
    (emboss-format-insert (point-min) (emboss-raw-document name))
    (set-buffer standard-output)
    (goto-char (point-min))
    (forward-line 1)
    (make-text-button (save-excursion
                        (backward-word 1)
                        (point))
                      (1- (point))
                      'id name
                      'action 'emboss-push-file-button)
    (when (looking-at "^Use ")
      (forward-char 4)
      (while (progn
               (make-text-button (point)
                                 (progn (skip-chars-forward emboss-symbol-chars)
                                        (point))
                                 'action 'emboss-push-symbol-button)
               (when (looking-at " or ")
                 (forward-char 4)
                 t))))))

(defun emboss-parse (doc)
  "Parse document from string.
The database file is mix with annotaion and source code. The annotaion
is seperated by a tag \"xX\"(the first letter should be [A-Z]). 
Return with a list contain sections separated by \"xX\". Element in
every section is a cons cell which CAD is the tag and CDR is the
infomation in the same line. The source code is contain in last
section. To make it as the same structure, a new tag \"SR\" is used.

This is an typical result:

   (((\"ID\" . \"acdKeywords\")
     (\"TY\" . \"list\")
     (\"MO\" . \"ajacd\")
     (\"LB\" . \"ajax\"))
    ((\"DE\" . \"Processing predefined ACD keywords (application, variable, section,\")
     (\"DE\" . \"endsection)\"))
    ((\"SR\" . \"AcdOKey...\")))
"
  (with-temp-buffer
    (insert doc)
    (let (sec tokens)
      (goto-char (point-min))
      (while (not (looking-at "//"))
        (if (= (char-after (1+ (point))) ?X)
            (setq sec (cons (nreverse tokens) sec)
                  tokens nil)
          (push (cons (buffer-substring (point) (+ (point) 2))
                      (replace-regexp-in-string
                       "^ " ""
                       (buffer-substring (+ (point) 2) (line-end-position))))
                tokens))
        (forward-line 1))
      (forward-line 1)
      (skip-chars-forward " \t\n")
      (push (list (cons "SR" (buffer-substring (point) (point-max)))) sec)
      (nreverse sec))))

(defvar emboss-format-alist
  '(("CA" . emboss-format-category)
    ("AN" . emboss-format-attributes)
    ("DE" . emboss-format-description)
    ("SR" . emboss-format-source))
  "Special format function list.")

(defvar emboss-header-alist
  '(("CA" . "Category")
    ("AN" . "Attributes")
    ("RT" . "Return")
    ("CN" . "Cast(s)")
    ("DN" . "Destructor")
    ("EN" . "Assignment(s)")
    ("JN" . "Input")
    ("KN" . "Iterator name(s)")
    ("LN" . "Iterator")
    ("MN" . "Modifier(s)")
    ("NN" . "Constructor")
    ("ON" . "Operator(s)")
    ("RN" . "Other")
    ("TN" . "Alias name(s)")
    ("SR" . "Code"))
  "Header for each section.")

(defun emboss-format-header (sec)
  (let ((header (car sec))
        (src (cdr (caar (last sec))))
        id type module lib doc but len
        proto attr case-fold-search)
    (dolist (sym '(id type module lib))
      (set sym (cdar header))
      (setq header (cdr header)))
    (if (member type '("public" "static"))
        (if (string-match "^[A-Z]" id)
            (setq type (concat type " data type"))
          (setq type (concat type " function")
                proto (substring src 0 (1- (string-match "{" src))))))
    (setq doc (format "%s is a %s in %s" id type module)
          len (length doc)
          but (list (- len (length module)) len
                    'id id
                    'action 'emboss-push-file-button))
    (when proto
      (setq doc (concat doc "\n" proto)))
    ;; (setq doc (concat doc "\n" "Prototype: " proto))
    ;; (setq attr (list (list (+ len 1) (+ len 10) 'face 'bold))))
    (list doc attr (list but))))

(defun emboss-format-description (desc)
  (with-temp-buffer
    (insert (mapconcat 'cdr desc "\n") "\n")
    (fill-region (point-min) (point-max))
    (list (buffer-string))))

(defun emboss-format-1 (name desc)
  (let (attrs pos)
    (with-temp-buffer
      (push (list (point) (+ (point) 4) 'face 'italic) attrs)
      (insert "name: " name "\n")
      (setq pos (point))
      (push (list pos (+ pos 4) 'face 'italic) attrs)
      (insert "desc: " desc "\n")
      (put-text-property (+ pos 4) (point) 'left-margin 6)
      (fill-region pos (point))
      (list (buffer-string) attrs))))

(defun emboss-format-attributes (attr)
  (let ((name (concat (cdr (assoc "AT" attr)) " "
                      (cdr (assoc "AN" attr)))))
    (setq attr (nthcdr 2 attr))
    (emboss-format-1 name (mapconcat 'cdr attr "\n"))))

(defun emboss-format-arguments (args)
  (let (arg)
    (setq args (cdr args)
          arg (split-string (cdar args))
          args (cdr args))
    (emboss-format-1
     (concat (mapconcat 'identity (nthcdr 2 arg) " ") " "
             (cadr arg))
     (mapconcat 'cdr args "\n"))))

(defun emboss-format-category (category)
  (let (attrs pos)
    (with-temp-buffer
      (push (list (point) (+ (point) 4) 'face 'italic) attrs)
      (insert "type: " (cdr (assoc "CA" category)) "\n")
      (push (list (point) (+ (point) 4) 'face 'italic) attrs)
      (insert "data: " (cdr (assoc "CT" category)) "\n")
      (setq pos (point)
            category (nthcdr 2 category))
      (push (list pos (+ pos 4) 'face 'italic) attrs)
      (insert "desc: " (mapconcat 'cdr category "\n") "\n")
      (put-text-property (+ pos 4) (point) 'left-margin 6)
      (fill-region pos (point))
      (list (buffer-string) attrs))))

(defun emboss-format-generic (sec)
  (emboss-format-1 (cdar sec)
                   (mapconcat 'cdr (cdr sec) "\n")))

(defun emboss-format-source (src)
  (let ((code (cdar src))
        sym buts)
    (with-temp-buffer
      (insert code)
      (goto-char (point-min))
      (while (not (eobp))
        (if (intern-soft (setq sym (emboss-symbol-ap)) emboss-obarray)
            (push (list (1- (point)) (+ (point) (length sym) -1)
                        'action 'emboss-push-symbol-button)
                  buts))
        (skip-chars-forward emboss-symbol-chars)
        (skip-chars-forward (concat "^" emboss-symbol-chars)))
      (list code nil buts))))

(defun emboss-push-symbol-button (but)
  (emboss-describe-symbol (button-label but)))

(defun emboss-push-file-button (but)
  (emboss-find-symbol (button-get but 'id) t))

(defun emboss-find-symbol (name &optional display)
  "Goto the place of NAME where it was defined.
DISPLAY non-nil means just display the file.
If DISPLAY is nil, a mark will be inserted to `find-tag-marker-ring',
you can use M-x `pop-tag-mark' to go back.
"
  (interactive (list (emboss-read-symbol "Find") current-prefix-arg))
  (let ((symbol (intern-soft name emboss-obarray))
        (marker (point-marker))
        case-fold-search files buf module lib type refine)
    (unless symbol
      (error "No symbol named %s" name))
    (if (get symbol 'deprecated)
        (let ((dirs '("ajax" "nucleus" "emboss"))
              (module (cdr (get symbol 'deprecated)))
              found)
          (while (and (not found)
                      dirs)
            (if (file-exists-p (setq file (concat
                                           (file-name-as-directory emboss-src-diretory)
                                           (file-name-as-directory (car dirs))
                                           module ".c")))
                (setq found t)
              (setq dirs (cdr dirs))))
          (if found
              (setq files (list file)
                    refine (concat "^__deprecated.*?" name "\\>"))
            (error "Can't found where %s defined!" name)))
      (with-temp-buffer
        (insert (emboss-raw-document name))
        (goto-char (point-min))
        (forward-line 1)
        (setq type (buffer-substring (+ (point) 3) (line-end-position)))
        (forward-line 1)
        (setq module (buffer-substring (+ (point) 3) (line-end-position)))
        (forward-line 1)
        (setq lib (buffer-substring (+ (point) 3) (line-end-position)))
        (setq file (concat (file-name-as-directory emboss-src-diretory)
                           (file-name-as-directory lib)
                           module))
        ;; functions are start with lower case. search .c file first.
        ;; data types search .h file first.
        (if (or (string= type "macro")
                (and (string-match "^[A-Z]" name)
                     (not (string= type "list"))))
            (setq files (list (concat file ".h")
                              (concat file ".c")))
          (setq files (list (concat file ".c")
                            (concat file ".h"))))
        (re-search-forward "//")
        (skip-chars-forward "\t \n")
        (setq refine (buffer-substring (point)
                                       (progn (forward-line 1)
                                              (point))))
        (if (> (length refine) 0)
            (setq refine (regexp-quote refine))
          (cond ((string= type "macro")
                 (setq refine (concat "^#define\\s-*" (regexp-quote name))))
                (t (setq refine (regexp-quote name)))))))
    (unless (catch 'found
              (dolist (file files)
                (with-current-buffer (find-file-noselect file)
                  (goto-char (point-min))
                  (when (re-search-forward refine nil t)
                    (forward-line 0)
                    (setq buf (current-buffer))
                    (throw 'found t)))))
      (setq buf (find-file-noselect (car files)))
      (message "Can't find the %s" name))
    (if display
        (with-selected-window (display-buffer buf)
          (goto-char (point-min))
          (when (re-search-forward refine nil t)
            (forward-line 0)))
      (ring-insert find-tag-marker-ring marker)
      (switch-to-buffer buf))))

(defun emboss-eldoc-function ()
  (when emboss-obarray
    (let ((name (emboss-symbol-ap))
          case-fold-search symbol state start)
      (unless (and (string-match "^[a-z]" name)
                   (setq symbol (intern-soft name emboss-obarray)))
        (setq state (parse-partial-sexp (save-excursion
                                          (beginning-of-defun)
                                          (point)) (point)))
        (when (cadr state)
          (save-excursion
            (goto-char (cadr state))
            (skip-syntax-backward "^w")
            (skip-chars-backward emboss-symbol-chars)
            (setq name (emboss-symbol-ap)))))
      (when (and (string-match "^[a-z]" name)
                 (setq symbol (intern-soft name emboss-obarray)))
        (with-temp-buffer
          (insert (emboss-raw-document name))
          (goto-char (point-min))
          (re-search-forward "^//")
          (skip-chars-forward " \t\n")
          (setq start (point))
          (when (re-search-forward "{" nil t)
            (goto-char (match-beginning 0))
            (replace-regexp-in-string "[ \n\t]+" " "
                                      (buffer-substring start
                                                        (match-beginning 0)))))))))

;; completion function borrow from `try-complete-lisp-symbol'
(defun emboss-try-complete-symbol (old)
  (if (not old)
      (progn
        (he-init-string (he-lisp-symbol-beg) (point))
        (if (not (he-string-member he-search-string he-tried-table))
            (setq he-tried-table (cons he-search-string he-tried-table)))
        (setq he-expand-list
              (and (not (equal he-search-string ""))
                   (sort (all-completions he-search-string
                                          emboss-obarray)
                         'string-lessp)))))
  (while (and he-expand-list
              (he-string-member (car he-expand-list) he-tried-table))
    (setq he-expand-list (cdr he-expand-list)))
  (if (null he-expand-list)
      (progn
        (if old (he-reset-string))
        nil)
    (he-substitute-string (car he-expand-list))
    (setq he-expand-list (cdr he-expand-list))
    t))

;;; Style
(defconst emboss-c-style
  '((c-tab-always-indent        . t)
    (c-comment-only-line-offset . 0)
    (c-hanging-braces-alist     . ((brace-list-open)))
    (c-hanging-colons-alist     . ((member-init-intro before)
                                   (inher-intro)
                                   (case-label after)
                                   (label after)
                                   (access-label after)))
    (c-cleanup-list             . (scope-operator
                                   empty-defun-braces
                                   defun-close-semi))
    ;; Uncomment the following line if you don't want newlines after semicolons
    ;;    (c-hanging-semi&comma-criteria)
    (c-offsets-alist            . ((arglist-close     . c-lineup-arglist)
                                   (substatement-open . 0)
                                   (case-label        . 4)
                                   (block-open        . 0)
                                   (knr-argdecl-intro . -))))
  "EMBOSS C Programming Style")

;;; Tempo abbrevs
(defvar emboss-tempo-tags nil
  "Tempo template for emboss")

(tempo-define-template
 "emboss-data"
 '("/* @data " (p "Name: " name) " "
   (make-string (- 72 (length (tempo-lookup-named 'name))
                   (length "data")) ?\*) "\n"
   "**\n"
   "** " p "\n"
   "**\n"
   (R "** @attr " (p "Attribute: " attr)
      (& attr (delete-region recursion-start (point)))
      "\n")
   "** @@\n"
   (make-string 78 ?\*) "/\n")
 "datac"
 "Insert documentation for data"
 'emboss-tempo-tags)

(tempo-define-template
 "emboss-func"
 '("/* @func " (p "Function: " name) " "
   (make-string (- 72 (length (tempo-lookup-named 'name))
                   (length "func")) ?\*) "\n"
   "**\n"
   "** " p "\n"
   "**\n"
   (R "** @param " (p "Parameter: " attr)
      (& attr (delete-region recursion-start (point)))
      "\n")
   "** @@\n"
   (make-string 78 ?\*) "/\n")
 "func"
 "Insert documentation for function"
 'emboss-tempo-tags)

(define-minor-mode emboss-mode
  "Minor mode to edit c code for emboss development or application.

Features:
If you don't have emboss source code, `emboss.el' provides:

 1. Abbrevs for insert comments. See `acd.el' for instruction to
    use tempo. `tempo-x.el' is needed. See `emboss-tempo-tags'
    for all available abbrevs.
 2. Emboss coding style.

If you have emboss source code, you can try these features:

 1. Search document of data structure or functions.
    M-x `emboss-describe-symbol' will show the documentation for
    the symbol. You can press the button to display the file where
    it was defined. Or press other button with a symbol label to
    view the documentation. Use C-c C-b to go back like
    `describe-function'.
 2. Find the definition of data structure or functions.
    M-x emboss-find-symbol switch to the file where the symbol was
    defined. Use M-* to pop back like `find-tag'.
 3. Show prototype of function in echo area when editing.
 4. Completing symbol when using `hippie-expand'.

But before you can use these features, several database files need
to generate. The database file and deprecated file can be generate
using a perl script which can be download:
http://www.emacswiki.org/cgi-bin/emacs/EmbossScript
Run the script like:
> perl gen_doc.pl -r /home/ywb/downloads/emboss/

It will create a file \"efunc.dat\" and \"deprecated\". Set
`emboss-database-file' and `emboss-deprecate-file' to the files. Or
put the two files in directory \"doc\" in `emboss-src-diretory', so
that you can set `emboss-src-diretory' only.

Key bindings:
\\{emboss-mode-map}"
  :lighter " EMB"
  :keymap '(("\en" . emboss-find-symbol))
  (when emboss-mode
    (emboss-init)
    (eldoc-mode 1)
    (c-subword-mode 1)
    (set (make-local-variable 'hippie-expand-try-functions-list)
         emboss-expand-functions)
    (set (make-local-variable 'eldoc-documentation-function)
         'emboss-eldoc-function)
    (tempo-use-tag-list 'emboss-tempo-tags)
    (c-add-style "EMBOSS" emboss-c-style t)))

;;;###autoload
(defun emboss-c-mode-hook ()
  "Automatic turn on `emboss-mode' when in `emboss-src-diretory'."
  (if (or (and buffer-file-name
               emboss-src-diretory
               (string-match (regexp-quote (expand-file-name emboss-src-diretory))
                             (expand-file-name buffer-file-name)))
          (save-excursion
            (goto-char (point-min))
            (re-search-forward emboss-magic-mode-regexp
                               magic-mode-regexp-match-limit t)))
      (emboss-mode 1)))

(defun emboss-init ()
  (unless emboss-obarray
    (and emboss-database-file
         (file-exists-p emboss-database-file)
         emboss-deprecate-file
         (file-exists-p emboss-deprecate-file)
         emboss-index-file
         (file-writable-p emboss-index-file)
         (emboss-load-index))))

(provide 'emboss)
;;; emboss.el ends here
