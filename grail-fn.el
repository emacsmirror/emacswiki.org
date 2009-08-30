;;;----------------------------------------------------------------------
;; grail-fn.el
;; Primary Author: Mike Mattie
;; Copyright (C) 2008,2009 Mike Mattie
;; License: LPGL-v3
;;----------------------------------------------------------------------

;; definitions that are essential to the Emacs boot. This was split
;; from my general utility collection so that the risk of introducing
;; bugs/complexity early in the boot process could be minimized.

;;----------------------------------------------------------------------
;; basic utilities.
;;----------------------------------------------------------------------

(defun quote-string-for-shell ( string )
  "quote-string-for-shell STRING

   quote the string with ' for the shell.
  "
  (concat "\'" string "\'"))

(defun list-filter-nil ( list )
  "Filter nil symbols from a list"
  (remq 'nil list))

(defun seq-filter-nil ( &rest list-seq )
  "Filter nil symbols from a sequence."
  (list-filter-nil list-seq))

(defun map-filter-nil ( func &rest seq )
  "map-filter-nil FUNC LIST

   Filter the nil elements of LIST from the input and output of
   function FUNC.

   FUNC is applied to the non-nil elements of SEQ ala mapcar. The
   result is either a list or nil if filtering eliminated all
   output."
  (lexical-let
    ((rvalue nil))

    (dolist (element seq)
      (when element
        (lexical-let
          ((transform (funcall func element)))
          (when transform
            (push transform rvalue)))))
    (reverse rvalue)))

(defun is-current-frame-gui ( &optional frame-arg )
  "is-current-frame-x FRAME

   Return t if FRAME or (selected-frame) is a GUI frame, nil
   otherwise.
  "
  (let*
    ((frame (or frame-arg (selected-frame)))
     (frame-type (framep frame)))

;;    (message "frame type is %s" (princ frame-type))

    (when (and frame-type
            (or
              (equal 'x frame-type)
              (equal 'w32 frame-type)
              (equal 'ns frame-type)))
      t) ))

(defun grail-print-fn-to-scratch ( fn-name description )
  "grail-print-fn-to-scratch FN-NAME DESCRIPTION

   Print FN-NAME as a function call with DESCRIPTION instructions
   in the scratch buffer. The user can evaluate the description
   and easily un-comment the function and execute it.
  "
  (with-current-buffer "*scratch*"
    (goto-char (point-max))
    (insert (format "; (%s) ; un-comment and evaluate to %s\n" fn-name description))) )

(defun grail-groups-loaded-p ()
  "return t if grail-groups.el has been loaded"
  (when grail-local-groups
    t))

;;----------------------------------------------------------------------
;; filter-ls: a general purpose tools for filtering directory listings.
;;----------------------------------------------------------------------

(defun filter-ls-predicate ( attr-name attr-match )
  "create predicate filters for path/mode values"
  (cond
    ((string-equal "type" attr-name) `(char-equal ,attr-match  (aref (cdr path-pair) 0)))
    ((string-equal "path" attr-name) `(string-match ,attr-match (car path-pair)))
    ((string-equal "name" attr-name) `(string-match ,attr-match (file-name-nondirectory (car path-pair)))) ))

(defun filter-ls-attributes ( filter-form )
  "implement the various attribute filters for the filter-ls form"
  (lexical-let
    ((attr-name (symbol-name (car filter-form)))
      (attr-match (cadr filter-form)))

    (if (char-equal ?! (aref attr-name 0))
      (list 'not (filter-ls-predicate (substring attr-name 1) attr-match))
      (filter-ls-predicate attr-name attr-match))
    ))

(defmacro filter-ls (path path-type &rest filters)
  "filter-ls PATH PATH-TYPE
  a form for flexibly filtering the result of listing a directory with attributes

   t   absolute paths
   nil relative paths"
  `(apply 'map-filter-nil
     (lambda ( path-pair )
       (if ,(cons 'and (mapcar 'filter-ls-attributes filters))
         (car path-pair)))

     ;; reduce the attributes to a pair of the path, and the mode string
     (mapcar (lambda ( attr-list )
               (cons (car attr-list) (nth 9 attr-list)))
       ;; get the list of files.
       (directory-files-and-attributes ,path ,path-type)) ))

;;----------------------------------------------------------------------
;; diagnostic support routines.
;;----------------------------------------------------------------------

;; find-library-name is not an auto-load so we need to force a load.
(require 'find-func)

(defmacro diagnostic-load-elisp ( &rest load-expr )
  "robust-load-elisp LOAD-EXPR

   evaluate LOAD-EXPR trapping any errors that occur. the value
   of LOAD-EXPR is discarded, and nil for a succesful load, or
   the trapped error is returned.
   "
  `(condition-case error-trap
     (progn
       ,@load-expr
       nil)
     (error error-trap)) )

(defmacro robust-load-elisp ( &rest load-expr )
  "robust-load-elisp LOAD-EXPR

   evaluate LOAD-EXPR trapping any errors that occur. the value
   of LOAD-EXPR is discarded, and t for successful, nil for
   errors is returned.
   "
  `(condition-case nil
     (progn
       ,@load-expr
       t)
     (error nil)) )

(defun grail-in-load-path-p (package)
  "grail-in-load-path-p elisp-name

   Return either the absolute path to the elisp-file if it is found
   in load-path, or nil otherwise.
  "
  (condition-case nil
    (find-library-name package)
    (error nil)) )

;;----------------------------------------------------------------------
;; faces
;;----------------------------------------------------------------------

;; I previously used custom-theme-set-faces for setting faces, however
;; it broke with emacs --daemon, so I have created a macro to go
;; to use set-face-attribute.

(defun grail-set-face ( face attribute value )
  "grail-set-face FACE ATTRIBUTE VALUE

   set FACE attribute ATTRIBUTE to value. Attribute is a plain
   symbol 'foo' converted to a syntatic attribute ':foo' by this
   function.
  "
  (set-face-attribute face nil
    (read (concat ":" attribute)) value))

(defun pointer-to-face-p ( symbol )
  "pointer-to-face-p SYMBOL

   determine if SYMBOL is a variable that points to
   a face (t), or a face symbol (nil).
  "
  (condition-case nil
    (progn
      (eval symbol)
      t)
    (error
      nil)))

(defmacro grail-set-faces ( &rest list )
  "grail-set-faces BODY

   Set one or more faces with a list of attributes.
  "
  (let
    ((set-face-calls nil))

    (mapc
      ;; traverse the list of faces
      (lambda ( face )
        ;; traverse the list of attributes for a face.
        (mapc
          (lambda (attr-pair)
            ;; cons each attribute as a call to grail-set-face
            ;; onto a list of calls.
            (setq set-face-calls
              (cons
                `(grail-set-face
                   ,(if (pointer-to-face-p (car face))
                      (car face)
                      `',(car face))
                   ,(symbol-name (car attr-pair)) ,(cadr attr-pair))
                set-face-calls)))
          (cdr face)))
      list)
    (cons 'progn set-face-calls)))

;;----------------------------------------------------------------------
;; ELPA
;;
;; The preferred way to install software is with ELPA which is a
;; sophisticated package management system.
;;
;; the grail install functions are overly simplistic in comparison.
;;----------------------------------------------------------------------

(defconst elpa-url
  "http://tromey.com/elpa/")

(defun load-elpa-when-installed ()
  "load-elpa-when-installed

   If the ELPA package management system http://tromey.com/elpa/ is installed,
   configure it for use, assuming a proper install by grail-install-elpa.

   t is returned if succesful, otherwise nil is returned.
  "
  (interactive)
  (if (load-elisp-if-exists (concat grail-dist-elisp "package.el"))
    (progn
      (unless (dir-path-if-accessible grail-dist-elpa)
        (make-directory grail-dist-elpa t))

      (setq-default package-user-dir (grail-sanitize-path grail-dist-elpa))
      (push grail-dist-elpa package-directory-list)

      (let
        ((elpa-errors (diagnostic-load-elisp (package-initialize))))

        (if elpa-errors
          (progn
            (grail-dup-error-to-scratch
              (format "ELPA failed to initialize with error %s" (format-signal-trap elpa-errors)))
            nil)
          t) ))
    nil))

(defun grail-install-elpa ()
  "install the ELPA package management system"
  (interactive)

  (catch 'abort
    (unless (grail-groups-loaded-p)
      (message "installing ELPA requires loading grail-groups.el for installation routines. Please consult README.grail and place grail-groups.el in USER_ELISP")
      (throw 'abort))

    (let
      ((elpa-install (grail-repair-by-installing 'package
                       (grail-define-installer "package" "file" (concat elpa-url "package.el")))))

      (unless elpa-install
        (message "ELPA installation failed %s" elpa-install)))

    (load-elpa-when-installed) ))
