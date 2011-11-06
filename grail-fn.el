;;;----------------------------------------------------------------------
;; grail-fn.el
;; Primary Author: Mike Mattie
;; Copyright (C) 2008,2009 Mike Mattie
;; License: LPGL-v3
;;----------------------------------------------------------------------

;; grail-fn is a library of functions required by grail to boot. These
;; functions are seperated from grail itself to minimize the
;; opportunities for errors to occur in the earliest stage of loading,
;; and to facilitate compilation.

(eval-when-compile
  (require 'cl))

;;----------------------------------------------------------------------
;; general lisp functions
;;----------------------------------------------------------------------

;;
;; lists
;;

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
  (let
    ((rvalue nil))

    (dolist (element seq)
      (when element
        (let
          ((transform (funcall func element)))
          (when transform
            (push transform rvalue)))))
    (reverse rvalue)))

;;
;; error handling
;;

(defun format-signal-trap (signal-trap)
  "format-signal-trap list:SIGNAL-TRAP

   format SIGNAL-TRAP for use in error messages.
  "
  (format "(%s , \"%s\")"
    (symbol-name (car signal-trap))

    (if (listp (cdr signal-trap))
      (cadr signal-trap)
      (cdr signal-trap)) ))


(defun grail-insert-error ( message )
  (let
    ((message-start (point)))

    (cond
      ((stringp message) (insert message))
      ((functionp message) (insert "(%s)" (princ message)))
      (t (insert "%s" (princ message))) )

    (fill-region message-start (point))

    (insert "\n\n")))

(defun grail-report-errors (message &rest errors )
  "grail-report-errors ERROR-MESSAGE

  duplicate the ERROR-MESSAGE to both *Messages* as a log and to the
  *scratch* buffer as a comment where it is highly visible.
  "

  (with-current-buffer "*scratch*"
    (let
      ((error-start (progn
                      (goto-char (point-max))
                      (insert "\n")
                      (point)) ))

      (grail-insert-error message)

      (when (not (null errors))
        (let
          ((cause-start (point)))

          (mapc
            (lambda (cause)
              (grail-insert-error (format "*  %s" cause)))
            errors)

          (insert "\n")
          (let
            ((fill-prefix "  "))
            (indent-region cause-start (point))) ))

      (let
        ((comment-start ";"))

        (comment-region error-start (point))) )) )

;;
;; path functions
;;

(defun dir-path-if-accessible ( path )
  "return the path if the directory is readable, otherwise nil"
  (if (and path (file-accessible-directory-p path))
    path
    nil))

(defun file-path-if-readable ( file )
  "return the path if the file is readable, otherwise nil"
  (if (file-readable-p file)
    file))

(defun grail-garuntee-dir-path ( path )
  "grail-garuntee-dir-path PATH

   If the directory PATH does not already exist then create it.
   return the path of the directory or nil.
  "
  (or (dir-path-if-accessible path)
    (progn
      (make-directory path t)
      path)) )

;;----------------------------------------------------------------------
;; interface detection
;;----------------------------------------------------------------------

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

;;----------------------------------------------------------------------
;; loading functions
;;----------------------------------------------------------------------

(defun load-elisp-trapping-errors ( path )
  "load-elisp-file-trapping-errors PATH

   load PATH throwing grail-trap with diagnostics if an error is
   reported by diagnostic-load-elisp-file.

   t is returned on success, nil on failure.
  "
  (let
    ((diagnostics (diagnostic-load-elisp-file path)))

    (if diagnostics
      (throw 'grail-trap (list
                           (format "grail: %s aborted loading" path)
                           (format-signal-trap diagnostics)))
      t) ))

(defun load-elisp-if-exists ( path )
  "load-elisp-if-exists PATH

   Try to load the elisp file PATH only if it exists and is
   readable.

   t is returned if the file was found and loaded without
   errors, nil otherwise.
  "
  (let
    ((checked-path (or (file-path-if-readable path)
                       (file-path-if-readable (concat path ".el"))
                       (file-path-if-readable (concat path ".elc")) )))

    (if checked-path
      (let
        ((trap (catch 'grail-trap (load-elisp-trapping-errors path))))

        (when (consp trap)
          (throw 'grail-trap (cons "grail: unexepected error loading an existing path; likely a syntax problem, or a missing require"
                               trap)))
        t)
      nil)))

(defun load-user-elisp ( file )
  "load-user-elisp FILE

   A fully guarded load that checks for a non-nil FILE name
   and attempts to load it relative to grail-elisp-root.

   t is returned if the file was found and loaded without
   errors, nil otherwise.
  "
  (when file
    (load-elisp-if-exists (concat grail-elisp-root file))))

;;
;; display/gui loading
;;

(defvar grail-display-configured nil
  "Boolean for if grail has configured the frame.")

(defvar grail-gui-configured nil
  "Boolean for if grail has configured the gui.")

(defun grail-load-gui-configuration-once ( frame )
  "grail-load-gui-configuration-once

   Load the GUI configuration file gui.el setting a flag to
   ensure that multiple calls only load the file once so that
   this function can be safely placed on a hook.

   It ignores an optional parameter so that it can be placed on
   after-make-frame-functions.
  "
  (when (and (not grail-gui-configured) (is-current-frame-gui frame))
    (progn

      (grail-trap
        "loading the gui file"
        (load-user-elisp "gui"))
      (setq grail-gui-configured t)) ))

(defun grail-load-display-configuration-once ()
  "grail-load-display-configuration-once

   Load the display configuration file display.el only once,
   before a frame is created ala grail-load-gui-configuration-once.
  "
  (unless grail-display-configured
    (load-user-elisp "display")
    (setq grail-display-configured t)))

(defun grail-groups-loaded-p ()
  "return t if grail-groups.el has been loaded"
  (when grail-local-groups t))

;;----------------------------------------------------------------------
;; load-path construction
;;----------------------------------------------------------------------

;;
;; filter-ls: a general purpose tools for filtering directory listings.
;;

(eval-and-compile
  (defun filter-ls-predicate ( attr-name attr-match )
    "create predicate filters for path/mode values"
    (cond
      ((string-equal "type" attr-name) `(char-equal ,attr-match  (aref (cdr path-pair) 0)))
      ((string-equal "path" attr-name) `(string-match ,attr-match (car path-pair)))
      ((string-equal "name" attr-name) `(string-match ,attr-match
                                          (file-name-nondirectory (car path-pair)))) ))

  (defun filter-ls-attributes ( filter-form )
    "implement the various attribute filters for the filter-ls form"
    (let
      ((attr-name (symbol-name (car filter-form)))
       (attr-match (cadr filter-form)))

      (if (char-equal ?! (aref attr-name 0))
        (list 'not (filter-ls-predicate (substring attr-name 1) attr-match))
        (filter-ls-predicate attr-name attr-match)) )) )

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

;;
;; load-path construction
;;

(defun grail-extend-load-path ()
  "grail-extend-load-path

   build extended-load-path in override order highest -> lowest with:

   --- override ---

   1. grail-local-emacs   - local, for preferring local modifications of mainline packages.
   2. emacs-load-path     - the emacs boot load path

   --- extend ---

   3. grail-local-elisp   - user written elisp
   4. elpa-load-path      - elpa managed third party packages
   5. grail-dist-elisp    - grail managed third party packages

   non-existent directories are filtered out.
  "

  (let*
    ((filter-dot-dirs "^\\.")
     (extended-load-path
       (condition-case signal-trap
         (apply 'append
           (seq-filter-nil

             (if (file-accessible-directory-p grail-local-emacs)
               (list grail-local-emacs))

             ;; prefer the load-path as it existed after loading
             ;; the platform files over the Emacs boot load-path.
             (or grail-platform-load-path grail-boot-load-path)

             (if (file-accessible-directory-p grail-local-elisp)
               (cons grail-local-elisp
                 (filter-ls grail-local-elisp t
                   (type ?d)
                   (!name filter-dot-dirs))))

             ;; load the version-control based code before ELPA
             ;; to always prefer local changes.

             (if (file-accessible-directory-p grail-dist-git)
               (cons grail-dist-git
                 (filter-ls grail-dist-git t
                   (type ?d)
                   (!name filter-dot-dirs))))

             (if (file-accessible-directory-p grail-dist-cvs)
               (cons grail-dist-cvs
                 (filter-ls grail-dist-cvs t
                   (type ?d)
                   (!name filter-dot-dirs))))

             (if (file-accessible-directory-p grail-dist-svn)
               (cons grail-dist-svn
                 (filter-ls grail-dist-svn t
                   (type ?d)
                   (!name filter-dot-dirs))))

             ;; ELPA loaded packages.

             grail-elpa-load-path

             ;; give the user a place to drop files and organize
             ;; as they see fit.

             (if (file-accessible-directory-p grail-dist-elisp)
               (cons grail-dist-elisp
                 (filter-ls grail-dist-elisp t
                   (type ?d)
                   (!name filter-dot-dirs))))
              ))

         ;; if there is an error, trap and re-throw the error
         (error
           (error "grail-extend-load-path magic failed: %s. grail-fn.el has likely been humbled by recursion stack growth."
                  (cdr signal-trap))) )) )

    ;; minimally check that the extended-load-path, if it's ok AFAICT
    ;; then update load-path

    (if (and extended-load-path (listp extended-load-path))
      (setq load-path extended-load-path)
      (error "new extended-load-path is not a list !?! %s" (pp-to-string extended-load-path))) ))

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

(defconst elpa-url "http://repo.or.cz/w/emacs.git/blob_plain/1a0a666f941c99882093d7bd08ced15033bc3f0c:/lisp/emacs-lisp/")

;(defconst elpa-url "http://tromey.com/elpa/")

(defun load-elpa-when-installed ()
  "load-elpa-when-installed

   If the ELPA package management system http://tromey.com/elpa/ is installed,
   configure it for use, assuming a proper install by grail-install-elpa.

   t is returned if succesful, otherwise nil is returned.
  "
  (interactive)
  (if (load-elisp-if-exists (concat grail-dist-elisp "package"))
    (progn
      (unless (dir-path-if-accessible grail-dist-elpa)
        (make-directory grail-dist-elpa t))

      (setq-default package-user-dir grail-dist-elpa)
      (push grail-dist-elpa package-directory-list)

      ;; ELPA is loaded so do the ugly parts and hook into package.el's guts
      ;; to pick up it's modifications to load-path

      (defadvice package-activate-1 (after grail-snoop/do-activate)
        (let
          ((snooped (car load-path))) ;; elpa always cons's the new path on the front.
          (when snooped
            (message "grail: snooped load-path update %s from package.el" snooped)
            (setq grail-elpa-load-path (cons snooped grail-elpa-load-path))
            (grail-extend-load-path))
          ))

      (ad-activate 'package-activate-1)

      (let
        ((elpa-errors (diagnostic-load-elisp (package-initialize))))

        (if elpa-errors
          (grail-report-errors "ELPA failed to initialize" elpa-errors)
          t)) )
    nil))

(defun grail-install-elpa ()
  "install the ELPA package management system"
  (interactive)

  (catch 'abort
    (unless grail-local-profiles
      (message "installing ELPA requires loading grail-profile.el for installation routines. Please consult README.grail and place grail-profile.el in USER_ELISP")
      (throw 'abort))

    (let
      ((elpa-install (grail-run-installer
                       (grail-define-installer "package"
                         "file"
                         (concat elpa-url "package.el"))) ))

      (unless elpa-install
        (message "ELPA installation failed %s" elpa-install)))

    (load-elpa-when-installed) ))

(provide 'grail-fn)
