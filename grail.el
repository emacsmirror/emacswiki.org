;;;----------------------------------------------------------------------
;; grail.el
;; Primary Author: Mike Mattie
;; Copyright (C) 2008,2009 Mike Mattie
;; License: LGPL-v3
;;----------------------------------------------------------------------

;; Grail loads an .emacs configuration in a robust, modular, and mode
;; aware manner.

;; The user has the opportunity to split their emacs configuration
;; into seperate files making it much easier to maintain. If the elisp
;; is sorted into the files treated as special, where grail loads them
;; directly by name, then the configuration will work properly when
;; emacs in started in different modes:

;; * batch mode    - non-interactive library functions only
;; * tty           - a console frame
;; * gui           - a graphical frame
;; * deamon        - a headless emacs server

;; The error trapping in grail catches errors in both errors in grail
;; itself, and in the configuration. This allows Emacs to start
;; containing the errors and reporting them. This is vital to using
;; Emacs to fix such errors :) and avoiding the dreaded --debug-init.

;; The second level of features involve managing load-path and third
;; party code not distributed with emacs itself.

;; It is very common for load-path to evolve, or devolve :) in a
;; hackish way until it is difficult to maintain. The tree structure
;; for a configuration defined by grail is used to construct a
;; load-path that scales from a simple configuration, to ambitious
;; elisp hacking.

;; There wealth of third party emacs packages provide powerful
;; functionality, but they are a burden to maintain, and especially to
;; replicate across multiple machines.

;; The primary mechanism for managing third party elisp is installing
;; and activating ELPA, the package management system hosted at
;; http://tromey.com/elpa/.  For those packages not hosted by ELPA
;; grail can install them in a limited way.

;; To collalesce a variety of packages into a feature group Grail
;; provides the "group" configuration files. In a group file
;; third party elisp is managed with installation and configuration.

;; In a group when elisp packages are missing an install function and
;; a initialization function are generated, with calls to those
;; functions inserted into the scratch buffer. The user can uncomment
;; the functions and execute them to replace the missing peices and
;; execute the configuration bits that depend on them.

;; ---> Starting with Grail.

;; The README.grail file provides installation instructions a more
;; detailed description of the file and directory structure that is
;; significant to Grail.

(defconst grail-release-version "0.1.3"
  "the release number of grail.el")

(defconst grail-maintainer-email "codermattie@gmail.com"
  "The maintainer's e-mail address")

(defconst grail-project-url "http://www.emacswiki.org/emacs/Grail"
  "the project page for Grail")

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

(defun grail-dup-error-to-scratch (error-message)
  "grail-dup-error-to-scratch ERROR-MESSAGE

  duplicate the ERROR-MESSAGE to both *Messages* as a log and to the
  *scratch* buffer as a comment where it is highly visible.
  "
  (message "%s" error-message)
  (with-current-buffer "*scratch*"
    (goto-char (point-max))
    (insert (format "; grail error! %s\n" error-message))) )

(defun format-signal-trap (signal-trap)
  "format-signal-trap list:SIGNAL-TRAP

   format SIGNAL-TRAP for use in error messages.
  "
  (format "(%s , \"%s\")"
    (symbol-name (car signal-trap))

    (if (listp (cdr signal-trap))
      (cadr signal-trap)
      (cdr signal-trap)) ))

;;----------------------------------------------------------------------
;; define a robust methods of loading and evaluating elisp that trap
;; errors.
;;----------------------------------------------------------------------

(defun grail-sanitize-path ( path )
  "grail-sanitize-path PATH

   sanitize a load-path reducing redundant file separators to single
   separators. The sanitized PATH is returned.

   This situation: foo/bar/baz//  has bombed (load file) for me.
  "
  (replace-regexp-in-string "/+" "/" path))

(defun diagnostic-load-elisp-file ( path )
  "diagnostic-load-elisp-file PATH

   load a elisp file trapping any errors that occur. nil
   is returned for a successful load. If there are
   errors the signal is returned.
  "
  (condition-case error-trap
    (progn
      (load (grail-sanitize-path path))
      nil)
    (error error-trap)))

(defun load-elisp-file-reporting-errors ( path )
  "load-elisp-file-reporting-errors PATH

   load PATH relative to grail-elisp-root reporting any errors that occur.

   t is returned on success, nil on failure.
  "
  (let
    ((diagnostics (diagnostic-load-elisp-file path)))
      (if diagnostics
        (progn
          (grail-dup-error-to-scratch
             (format "grail: errors %s prevented %s from loading correctly"
             (format-signal-trap diagnostics)
              path))
          nil)
        t)) )

(defun load-elisp-if-exists ( path )
  "load-elisp-if-exists PATH

   Try to load the elisp file PATH only if it exists and is
   readable.

   t is returned if the file was found and loaded without
   errors, nil otherwise.
  "
  (let
    ((accessible-path  (file-path-if-readable path)))

    (when accessible-path
      (load-elisp-file-reporting-errors accessible-path)) ))

(defun load-user-elisp ( file )
  "load-user-elisp FILE

   A fully guarded load that checks for a non-nil FILE name
   and attempts to load it relative to grail-elisp-root.

   t is returned if the file was found and loaded without
   errors, nil otherwise.
  "
  (when file
    (load-elisp-if-exists (concat grail-elisp-root file))))

(defun grail-load-requested-groups ()
  ;; a dummy function replaced when grail loads the grail-groups package
  nil)

(defun grail-load-gui-configuration-once ( frame )
  "grail-load-gui-configuration-once

   Load the GUI configuration file gui.el setting a flag to
   ensure that multiple calls only load the file once so that
   this function can be safely placed on a hook.

   It ignores an optional parameter so that it can be placed on
   after-make-frame-functions.
  "
  (when (and (not grail-gui-configured) (is-current-frame-gui frame))
    (and (load-user-elisp "gui.el") (grail-load-requested-groups))
    (setq grail-gui-configured t)))

(defun grail-load-display-configuration-once ()
  "grail-load-display-configuration-once

   Load the display configuration file display.el only once,
   before a frame is created ala grail-load-gui-configuration-once.
  "
  (unless grail-display-configured
    (load-user-elisp "display.el")
    (setq grail-display-configured t)))

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

             grail-elpa-load-path

             (if (file-accessible-directory-p grail-dist-elisp)
               (cons grail-dist-elisp
                 (filter-ls grail-dist-elisp t
                   (type ?d)
                   (!name filter-dot-dirs))))

             (if (file-accessible-directory-p grail-dist-cvs)
               (cons grail-dist-cvs
                 (filter-ls grail-dist-cvs t
                   (type ?d)
                   (!name filter-dot-dirs)))) ))

         ;; if there is an error, trap and re-throw the error
         (error
           (error "grail-extend-load-path magic failed: %s. grail-fn.el has likely been humbled by recursion stack growth."
                  (cdr signal-trap))) )) )

    ;; minimally check that the extended-load-path, if it's ok AFAICT
    ;; then update load-path

    (if (and extended-load-path (listp extended-load-path))
      (setq load-path extended-load-path)
      (error "new extended-load-path is not a list !?! %s" (pp-to-string extended-load-path))) ))

(condition-case error-trap
  (progn
    ;; establish the root of the USER_ELISP configuration tree.
    (defvar grail-elisp-root
      (grail-sanitize-path
        (concat (or (dir-path-if-accessible (getenv "USER_ELISP"))
                  (dir-path-if-accessible (concat (getenv "HOME") "/system/emacs")))
          "/"))
      "The root of the user's elisp tree")

    ;; abort the rest of grail if the USER_ELISP tree cannot be found.
    (unless grail-elisp-root
      (error "%s" "cannot access USER_ELISP directory !!"))

    (message "grail is loading USER_ELISP %s" grail-elisp-root)

    ;; define the directory structure of the configuration tree
    ;; relative to USER_ELISP

    (defvar grail-local-dir
      (concat grail-elisp-root "local/")
      "The directory containing the user's local modifications to emacs
       and elisp.

       grail-local-emacs and grail-local-elisp are the preferred
       variables for accessing user specific elisp paths.")

    (defvar grail-settings-file "customize-settings.el"
      "The file where Emacs writes settings and customize data")

    (defvar grail-local-emacs
      (concat grail-local-dir "emacs/")
      "The directory containing Emacs packages that over-ride the packages
       distributed with Emacs.")

    (defvar grail-local-elisp
      (concat grail-local-dir "elisp/")
      "The directory containing Emacs libraries created and maintained by the
       user.")

    (defvar grail-dist-dir
      (concat grail-elisp-root "dist/")
      "The directory for managing distributed packages")

    (defvar grail-dist-archive
      (concat grail-dist-dir "archive/")
      "The directory for managing distributed packages")

    (defvar grail-dist-elisp
      (concat grail-dist-dir "elisp/")
      "The directory containing third-party elisp extensions of Emacs.")

    (defvar grail-dist-elpa
      (concat grail-dist-dir "elpa/")
      "ELPA managed third party elisp.")

    (defvar grail-dist-cvs
      (concat grail-dist-dir "cvs/")
      "cvs managed third party elisp")

    (defvar grail-elpa-load-path nil
      "The load-path extensions made by ELPA package activation")

    (defvar grail-boot-load-path load-path
      "The load-path as constructed by emacs before grail initialization")

    (defvar grail-platform-load-path nil
      "The load-path after the platform files have been loaded.")

    (defvar grail-state-path (concat (getenv "HOME") "/.emacs.d/")
      "The grail session state & persistent data directory which defaults to .emacs.d")
    (defvar grail-display-configured nil
      "Boolean for if grail has configured the frame.")

    (defvar grail-gui-configured nil
      "Boolean for if grail has configured the gui.")

    (require 'cl)

    ;;----------------------------------------------------------------------
    ;; load the 2cnd stage of grail with the more complex functions.
    ;;----------------------------------------------------------------------

    (let*
      ((stage-2-path (concat grail-elisp-root "grail-fn.el"))
       (stage-2-errors (diagnostic-load-elisp-file stage-2-path)))

      (when stage-2-errors
        (error "could not load %s , the second stage of grail. with %s errors. USER_ELISP does not point to a working GRAIL install" stage-2-path (format-signal-trap stage-2-errors))) )

    ;; grail-cfg.el is a file for user to change the tree structure that grail
    ;; traverses before load-path is formed.
    (load-user-elisp "grail-cfg.el")

    ;;----------------------------------------------------------------------
    ;; Host specific adaptation
    ;;
    ;; Each host system has a file that normalizes the platform
    ;; and extends the library search space for extra libraries the system
    ;; manages.
    ;;
    ;; these files and platform specific customization are loaded by
    ;; platform here.
    ;;----------------------------------------------------------------------
    (load-user-elisp
      (cond
        ((string-equal "gnu/linux" system-type)  "linux.el")
        ((string-equal "darwin"    system-type)  "darwin.el")
        ((string-equal "windows"   system-type)  "windows.el")))

    ;; save the state of load-path after the platform file if any has
    ;; been loaded.
    (setq grail-platform-load-path load-path)

    ;; integrate the user's Elisp tree into the load-path for the first time.
    (grail-extend-load-path)

    ;;----------------------------------------------------------------------
    ;; support for groups.
    ;;----------------------------------------------------------------------

    (defvar grail-local-groups
      (when (load-user-elisp "grail-groups.el") (concat grail-local-dir "groups/"))
      "The directory containing Emacs group modules.")

    ;; activate ELPA
    (load-elpa-when-installed)

    ;; elisp loads the user's general elisp library.
    (load-user-elisp "elisp.el")

    ;; make sure there is a directory for session state and persistent data
    (grail-garuntee-dir-path grail-state-path)

    ;; dummy compatability functions for recent features in emacs, or
    ;; broken ports.

    (unless (functionp 'window-system)
      ;; Annoying Emacs.app 0.9-rc2 compat.

      (message "window-system not defined. creating a dummy function")
      (defun window-system ()
        "grail.el replacement for window system function."
        window-system))

    (unless (functionp 'daemonp)
      (defun daemonp ()
        "grail.el replacement for daemonp function."
        nil))

    ;;----------------------------------------------------------------------
    ;; load the configuration based on mode.
    ;;----------------------------------------------------------------------

    (when (or (not noninteractive) (daemonp))
      ;; the user-init-file _must_ be changed otherwise emacs will
      ;; scribble all over grail which is not OK.

      ;; The customize file path also needs to be set so that
      ;; customize writes settings to a data-file rather than
      ;; appending them to code.

      (setq user-init-file
        (setq custom-file
          (concat grail-elisp-root grail-settings-file)))

      ;; Load the user's settings if any. Do it early so that this
      ;; stuff can be over-ridden in their config files.  The priority
      ;; of customize settings is a toss-up, but it only comes into
      ;; play when the user advances beyond relying on customize, and
      ;; by then the priority is sensible.
      (load-user-elisp grail-settings-file)

      ;; only loaded when there is an active terminal.
      (load-user-elisp "interface.el")

      (load-user-elisp "user.el")

      ;; load commands and keys last so they can use definitions from user.el
      ;; and friends.
      (load-user-elisp "commands.el")
      (load-user-elisp "keys.el")

      ;; In deamon mode the GUI related values are not defined by
      ;; Emacs until a GUI frame is created. In this case place the
      ;; GUI configuration on frame creation hooks.

      ;; when not using emacs --daemon load frame.el and gui.el
      ;; immediately.

      (cond
        ((daemonp) (progn
                     (add-to-list 'after-make-frame-functions 'grail-load-gui-configuration-once t)
                     (add-hook 'before-make-frame-hook 'grail-load-display-configuration-once) ))
        ((window-system) (progn
                           (load-user-elisp "display.el")
                           (load-user-elisp "gui.el") ))) )

    ;; load all the group files requested.
    (grail-load-requested-groups))
  (error
    (grail-dup-error-to-scratch
      (apply 'format "grail aborted ! %s" (cdr error-trap)))) )
