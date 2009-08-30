;;;----------------------------------------------------------------------
;; grail-groups.el
;; Primary Author: Mike Mattie
;; Copyright (C) 2009 Mike Mattie
;; License: LGPL-v3
;;----------------------------------------------------------------------

(defun grail-load-group ( group-name )
  (unless (load-elisp-if-exists (concat grail-local-groups group-name ".el"))
    (grail-dup-error-to-scratch
      (format "grail: group module %s aborted loading from errors" group-name)) ))

(defvar grail-requested-groups
  nil
  "List of group modules requested by the user.")

(defun grail-load-requested-groups ()
  "grail-load-requested-groups

   load the groups in the request list grail-requested-groups
   and then set the list to null, so that it can be re-run later.
  "
    (when grail-requested-groups
      (mapc 'grail-load-group
        grail-requested-groups)
      (setq grail-requested-groups nil)))

(defun use-grail-groups ( &rest request-list )
  "use-grail-groups: LIST

   request a list of string quoted groups to be loaded after the configuration
   files have been loaded.
  "
  (setq grail-requested-groups
    (append request-list grail-requested-groups)))

;;----------------------------------------------------------------------
;; installation routines.
;;----------------------------------------------------------------------

(defvar grail-save-downloads nil
  "when t downloaded archive files will be saved to grail-dist-dir")

(defun grail-recursive-delete-directory ( path )
  "grail-recursive-delete-directory PATH

   recursively delete the directory PATH. t on success, nil on error.
  "
  (condition-case trapped-error
    (if (dir-path-if-accessible path)
      (if (equal 0 (call-process-shell-command "rm" nil nil nil "-r" path))
        t
        nil)
      (progn
        (message "grail-recursive-delete-directory path %s is not a directory or the user does not have permissions" path)
        nil))
    (error
      (message "grail-recursive-delete-directory failed %s" (format-signal-trap trapped-error))
      nil)) )

(defun grail-dist-install-directory ( &optional package )
  "grail-dist-install-directory &optional string:PACKAGE

   Ensure that the installation directory exists. The default is grail-dist-elisp,
   however for multi-file packages an optional package name can be supplied.

   The path of the installation directory is returned for the installer's use.
  "
  (grail-garuntee-dir-path (grail-sanitize-path
                             (concat
                             (if package
                               (concat grail-dist-elisp "/" package)
                               grail-dist-elisp)
                               "/"))))

(defun grail-download-dir-and-file-path ( name )
  (let
    ((dl-dir  nil))

    (when (condition-case trapped-error
          (progn
            (setq dl-dir (if grail-save-downloads
                            grail-dist-dir
                            (make-temp-file "grail" t)))
            t)
          (error
            (progn
              (message "grail: grail-download-dir-and-file-path could not create a download path for %s" name)
              nil)))
      (cons dl-dir (grail-sanitize-path (concat dl-dir "/" name))) )))

(defun grail-cleanup-download ( dl-dir-and-file &optional ignore-save )
  "grail-cleanup-download

   delete the directory and the downloaded files.

   TODO: save downloads option.
  "
  (when dl-dir-and-file
    (if grail-save-downloads
      ;; when grail-save-downloads is enabled absolutely do not recursive delete !
      (when (not ignore-save)
        (delete-file (cdr dl-dir-and-file)))
      ;; otherwise it is a temp dir so nuke it
      (grail-recursive-delete-directory (car dl-dir-and-file))) ))

(defun grail-process-async-chain ( start-process-fn doesnt-start-fn proc-fail-fn
                                   do-after-fn next-fn)
  "grail-process-async-chain START-PROCESS-FN DOESNT-START-FN PROC-FAIL-FN
                             DO-AFTER-FN NEXT-FN

   create asynchronous processes that can be changed. START-PROCESS-FN
   creates a process object. This function generates a process sentinel
   and attaches the sentinel to the process.

   a number of lambdas are supplied in the arguments to fill in the body
   of the process sentinel.

   DOESNT-START-FN: executed if the process does not start.

   PROC-FAIL-FN   : executed if the process returns an error (a non-zero exit code).
   DO-AFTER-FN    : executed when the process exits with success (0 exit code)
   NEXT-FN        : when DO-AFTER-FN returns non-nil this function is executed,
                    typically to chain another async process, but it can do
                    anything.

   With this function processes can be changed by nesting another
   grail-process-async-chain as the tail, or NEXT-FN function for
   a sequence of process execution.
  "
  (lexical-let
    ((async-proc (funcall start-process-fn))
     (no-start   doesnt-start-fn)
     (fail-fn    proc-fail-fn)
     (after-fn   do-after-fn)
     (chain-fn   next-fn))

    (if (or (not (processp async-proc))
            (not (process-status async-proc)))
      (funcall doesnt-start-fn)
      (progn
        ;; setup a lambda process sentinal that does the chaining.
        (set-process-sentinel async-proc
          ;; a sentinal that accepts status-change signals
          (lambda ( bound-proc status-change )
            (when (memq (process-status bound-proc) '(signal exit))
              ;; do something when the process exits
              (if (equal 0 (process-exit-status bound-proc))

                ;; If bound-proc process exits with success call the
                ;; do-after-exit function (do-after-fn).

                ;; If (do-after-fn) returns non-nil, and the (next-fn)
                ;; is non-nil run that function.
                (and (funcall after-fn) (and chain-fn (funcall chain-fn)))

                ;; if the process exits non-zero call (proc-fail-fn)
                (funcall fail-fn)) ))) ))))

(defun grail-wget-url-async ( url path output-buffer )
  "grail-wget-url-async URL PATH OUTPUT-BUFFER

   retrieve the URL to PATH, with OUTPUT-BUFFER as the output
   buffer. The process object created is returned, or nil if a
   process could not be created.
  "
  (condition-case trapped-error
    (start-process-shell-command "grail-wget" output-buffer
      "wget"
      "--progress=dot:binary"
      (quote-string-for-shell url) "-O" (quote-string-for-shell path))
    (error
      (progn
        (message "grail-wget-url failed %s" (format-signal-trap trapped-error))
        nil)) ))

(defun grail-untar-async ( path target-dir compression output-buffer )
  "grail-untar-async PATH DIR COMPRESSION OUTPUT-BUFFER

   untar PATH in DIR with output going to OUTPUT-BUFFER.
   return the process object or nil if there was an error.
  "
  (condition-case trapped-error
    (start-process-shell-command "grail-untar" output-buffer
      "tar"
      (concat
        "xv"
        (cond
          ((equal "gz"  compression) "z")
          ((equal "bz2" compression) "j")
          (signal error (format "grail: error! unsupported compression %s" compression)))
        "f")
      (quote-string-for-shell path)
      "-C" (quote-string-for-shell target-dir))
    (error
      (progn
        (message "grail-wget-url failed %s" (format-signal-trap trapped-error))
        nil)) ))

(defun grail-archive-installer ( name url compression )
  "grail-archive-installer NAME URL COMPRESSION

   Download a tarball and install it.
  "
  (save-excursion
    (lexical-let*
      ((dl-dir-and-file nil)
       (old-window       (selected-window))

       ;; open a new window but do not put it in recently selected
       (grail-buffer  (pop-to-buffer (generate-new-buffer "*grail-install*") nil t))
       (grail-window  (not (eq old-window (selected-window)))))

      (catch 'abort
        ;; confirm with the user that they want to install the file.
        (unless (yes-or-no-p (format "download and install %s? " name))
          (throw 'abort nil))

        ;; signal the start of the download in the grail buffer.
        (insert (format "Starting the download of %s\n" url))

        ;; create a temporary directory to download into
        (unless (setq dl-dir-and-file (grail-download-dir-and-file-path (concat name "." compression)))
          (throw 'abort "could not create a temporary directory for the download"))

        (lexical-let
          ((dl-url  url)
           (compression-type compression))

          (grail-process-async-chain
            ;; start the download with wget
            (lambda ()
              (grail-wget-url-async
                dl-url
                (cdr dl-dir-and-file)
                grail-buffer))

            ;; the downloader doesn't start cleanup function
            (lambda ()
              (insert "could not start the download! Install aborted.\n")
              (grail-cleanup-download dl-dir-and-file t))

            ;; the downloader fail cleanup function
            (lambda ()
              (grail-cleanup-download dl-dir-and-file t)
              (message "download of %s failed! Install aborted, and downloads deleted." (cdr dl-dir-and-file)))

            ;; the downloader succeeded function
            (lambda ()
              (insert "grail: download completed\n")
              t)

            ;; the chain function
            (lambda ()
              (grail-process-async-chain
                ;; start the untar
                (lambda ()
                  (message "starting the untar")
                  (grail-untar-async (cdr dl-dir-and-file) (grail-dist-install-directory) compression-type grail-buffer))

                ;; tar doesn't start cleanup function
                (lambda ()
                  (insert "could not start tar to extract the downloaded archive. Install aborted, deleting downloads.\n")
                  (grail-cleanup-download dl-dir-and-file t))

                ;; the tar fail cleanup function
                (lambda ()
                  (insert (format "could not install files in %s from downloaded archive." grail-dist-elisp))
                  (grail-cleanup-download dl-dir-and-file t))

                ;; the tar succeeded function
                (lambda ()
                  (insert "grail: Installation Completed ! Re-Generating load-path\n")
                  (grail-extend-load-path)

                  (insert "grail: cleaning up downloads\n")
                  (grail-cleanup-download dl-dir-and-file)

                  (delete-windows-on grail-buffer)
                  (kill-buffer grail-buffer)
                  t)

                ;; terminate the chain.
                nil))) )
        ;; return nil if an abort is not thrown.
        nil))
    )) ;; save excursion and the defun.

(defun grail-file-installer ( name url &optional path )
  "grail-file-installer NAME URL &optional PATH

   install from URL into PATH with name NAME.  nil is returned
   when successful, otherwise an error is thrown.
  "
  (condition-case error-trap
    (let
      ((install-path (concat (grail-dist-install-directory path) name)))

      (with-temp-buffer
        (url-insert-file-contents url)
        (write-file install-path))

      (message "grail-file-installer: installed of %s to %s completed" name install-path))
    nil
    (error
      (format "grail-file-installer for %s failed with: %s"
        name (format-signal-trap error-trap))) ))

(defun grail-decompose-installer-type ( type-spec )
  (if (string-equal "file" type-spec)
    type-spec
    (let
      ((split-index (string-match ":" type-spec)))
      (when split-index
        (cons (substring type-spec 0 split-index) (substring type-spec (match-end 0)))) )))

(defun grail-define-installer ( name type &rest urls )
  "grail-define-installer NAME TYPE &rest URLS

  "
  (let
    ((install-spec (if (and (stringp name) (> (length name) 0))
                     (list name)
                     (signal 'error "installer was given something other than a string or a zero length string") )))

    (let
      ((install-type (grail-decompose-installer-type type)))

      (unless install-type
        (signal 'error (format "grail-define-installer: installer type %s not recognized" type)))

      (setq install-spec (cons type install-spec)) )

    (unless urls
      (signal 'error (format "grail-define-installer: installer definition for %s must be given urls" name)))

    (if (> (length urls) 1)
      (mapc
        (lambda ( url-pair )
          (unless (consp url-pair)
            (signal 'error (format "grail-define-installer: installer definition for %s requires (name . url) pairs\
 for installing more than one package, not: %s"  name (princ url-pair))))

            (setq install-spec (cons url-pair install-spec)))
        urls)
      (setq install-spec (cons (cons name (car urls)) install-spec)) )

    (reverse install-spec)))

(defun grail-run-installer ( installer )
  "grail-run-installer installer

   run an installer created by grail-define-installer.
  "
  (let*
    ((name             (car  installer))

     (install-type-arg (cadr installer))
     ;; anything not file is thrown at the archive installer to decode.
     (install-method   (or (when (and (stringp install-type-arg) (equal "file" install-type-arg)) 'grail-file-installer)
                           'grail-archive-installer))

     (url-list       (cddr installer))
     (install-many   (> (length url-list) 1))

     (call-the-installer nil))

    (unless install-method
      (signal 'error (format "grail-run-installer: I don't have an installer for %s" (princ install-type-arg))))

    ;; generate a lambda instead of riddling a static sexp with if's
    (setq call-the-installer (eval
      ;; a function that sets up the arguments for the type specific installer
      `(lambda ( url-pair )
         ,(cond
            ;; -> file installer call.
            ;;
            ;; When there are more than one url assume that the url list is a list of cons cells,
            ;; and supply the name as directory argument.
            ;; For a single file to install use the name as the file to write to, and do not supply
            ;; a directory argument.
            ((equal 'grail-file-installer install-method)
              `(,install-method
                 ,(if install-many
                    `(car url-pair)
                    `(concat (car url-pair) ".el"))
                 (cdr url-pair)
                 ,(when install-many name)) )

            ;; -> archive installer call
            ;;
            ;; for a single archive to download use name to name the downloaded tarball. for many
            ;; assume cons pairs as above with the file installer.
            ((equal 'grail-archive-installer install-method)
              `(,install-method
                 ,(if install-many `(car url-pair) 'name)
                 (cdr url-pair)
                 ,install-type-arg)) ))))

    (mapc call-the-installer url-list) ))

;;----------------------------------------------------------------------
;; grail repair routines.
;;----------------------------------------------------------------------

(defun grail-repair-by-installing ( package installer )
  "grail-repair-by-installing symbol:PACKAGE list|function:INSTALLER

   Attempt to install PACKAGE and load the missing
   dependency. INSTALLER is either defined by
   grail-define-installer or a custom installer function.

   t is returned on success and nil for failure.
  "
  (let
    ((package-name    (symbol-name package)))

    (catch 'installer-abort
      (condition-case install-trap

        ;; run the installer
        (cond
          ((functionp installer) (funcall installer))
          ((listp installer) (grail-run-installer installer))
          (signal error (fomat "unhandled installer type: not a function or a list %s" (princ (type-of installer)))))

        (error
          (message "grail repair of package %s failed with %s" package-name (format-signal-trap install-trap))
          (throw 'installer-abort nil)))

      ;; if there wasn't a error update the load path.
      (grail-extend-load-path)

      ;; try to load it again.
      (condition-case load-trap
        (require package)
        (error
          (message "repair of package %s : installed ok, but loading failed anyways - %s."
            package-name (format-signal-trap load-trap))
          (throw 'installer-abort nil)) )

      (message "installation repair of dependency %s completed :)" package-name)
      t)))

(defun grail-repair-by-debugging ( package )
  "grail-repair-by-debugging symbol:PACKAGE

   Repair package loading by debugging.
  "
  (when (yes-or-no-p "repair: load the library source and enter the debugger on error ? ")
    (message
      "debug-on-error will be set. You may want to clear it after debugging with toggle-debug-on-error")

    (find-file-read-only-other-window (find-library-name (symbol-name package)))
    (setq debug-on-error t)
    (funcall 'require package) ))

(defun grail-repair-dependency-fn ( package installer )
  "grail-repair-dependency-fn PACKAGE INSTALLER

   Repair dependency loading problems by installation or by a
   entry point into a debugging work-flow.

   In essence this function selects between repairing by
   installation or debugging and generates the interactive
   function binding important variables such as the package.

   Based upon a search of load-path the error will be diagnosed
   either as a evaluation failure (if the library is found in
   load-path) or a missing installation.

   For a evaluation failure a debugging entry point is
   constructed. For a missing entry point an installer is
   generated.
  "
  (let*
    ((pkg-name (symbol-name package))
    ;; repair procedure is set to a diagnostic/function pair
    ;; format (string lambda) by cons.

     (repair-procedure
       (lexical-let
         ;; the package symbol needs to be lexically bound to the generated lambda's
         ((pkg-symbol package)
          (pkg-installer installer))

         (if (grail-in-load-path-p pkg-name)
           ;; if it is installed in load-path the library is aborting in
           ;; evaluation.

           (cons "%s is aborting during load on evaluation."
             (lambda ()
               (interactive)
               (grail-repair-by-debugging pkg-symbol)))

           (cons "%s cannot be found in the load-path"
             (lambda ()
               (interactive)
               (grail-repair-by-installing pkg-symbol pkg-installer))) )))

      (repair-fn-name (concat "repair-dependency-" pkg-name)) )

    ;; print the diagnostic to the scratch buffer
    (grail-dup-error-to-scratch (format (car repair-procedure) pkg-name))
    (grail-print-fn-to-scratch repair-fn-name (concat "install or initiate debugging of " pkg-name))

    (fset (intern repair-fn-name) (cdr repair-procedure)) ))

;;----------------------------------------------------------------------
;; grail forms
;;----------------------------------------------------------------------

(defmacro grail-activate-with-recovery ( group package installer &rest init-code )
  "grail-load-dep-with-recovery string:GROUP symbol:PACKAGE INSTALLER code:INIT-CODE

   Attempt to load PACKAGE via require with error trapping, diagnosis, and repair.

   t is returned on success, nil on failure.
  "
  (let
    ((pkg-name     (symbol-name package))
     (diagnostic   nil)
     (load-fn `(lambda ()
                 (require ',package)))

     (init-fn `(lambda ()
                 (interactive)
                 ,@init-code)) )

    (if (catch 'abort ;; non-local exit for early termination from errors.

      ;; try loading the package catching any diagnostic errors from signals
      (when (setq diagnostic (diagnostic-load-elisp (funcall load-fn)))
        (grail-dup-error-to-scratch (format
                                      "grail: group module %s is degraded from %s loading failure %s"
                                      group
                                      pkg-name
                                      (format-signal-trap diagnostic)))
        (grail-repair-dependency-fn package (eval installer))
        (throw 'abort t))

      ;; try the initialization trapping any errors.
      (when (setq diagnostic (diagnostic-load-elisp (funcall init-fn) ))
        (grail-dup-error-to-scratch (format
                                      "grail: group module %s is degraded from initialization error %s"
                                      group
                                      (format-signal-trap diagnostic)))
        (throw 'abort t))
        nil)

      (let
        ((init-fn-name (concat "initialize-" pkg-name)))

        (fset (intern init-fn-name) init-fn)

        (grail-print-fn-to-scratch init-fn-name (format "re-initialize %s after repair" pkg-name))
        nil)

      t) ))
