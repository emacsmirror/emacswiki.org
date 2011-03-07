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
;; load-path that scales from simple configuration to ambitious elisp
;; hacking.

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

(defconst grail-release-version "0.3.0"
  "the release number of grail.el")

(defconst grail-maintainer-email "codermattie@gmail.com"
  "The maintainer's e-mail address")

(defconst grail-project-url "http://www.emacswiki.org/emacs/Grail"
  "the project page for Grail")

;;
;; two functions needed before grail-fn is loaded.
;;

(defun diagnostic-load-elisp-file ( path )
  "diagnostic-load-elisp-file PATH

   load a elisp file trapping any errors that occur. nil
   is returned for a successful load. If there are
   errors the signal is returned.
  "
  (condition-case error-trap
    (progn
      (load (expand-file-name path))
      nil)
    (error error-trap)))

(defmacro grail-trap ( message &rest body )
  `(let
     ((trap (catch 'grail-trap ,@body)))

     (if (consp trap)
       (apply 'grail-report-errors (format "grail error: %s" ,message) trap)
       trap) ))


(defun grail-load-requested-profiles ()
  ;; a dummy function replaced when grail loads the grail-profile package.
  nil)

(defun use-grail-profiles ( order &rest request-list )
  ;; a dummy function replaced when grail loads the grail-profile package.
  nil)

(condition-case error-trap
  (progn
    ;;
    ;; establish the root of the USER_ELISP configuration tree.
    ;;

    ;; the first priority is the USER_ELISP environment variable
    ;; the second priority is
    (defvar grail-elisp-root
      (expand-file-name (concat (getenv "USER_ELISP") "/"))
      "The root of the user's elisp tree")

    ;; abort the rest of grail if the USER_ELISP tree cannot be found.
    (unless (file-accessible-directory-p grail-elisp-root)
      (error "%s" "cannot access USER_ELISP directory !!"))

    (message "grail is loading USER_ELISP %s" grail-elisp-root)

    ;;----------------------------------------------------------------------
    ;; load the 2cnd stage of grail with the more complex functions.
    ;;----------------------------------------------------------------------
    (let*
      ((stage-2-path (concat grail-elisp-root "grail-fn"))
       (stage-2-errors (diagnostic-load-elisp-file stage-2-path)))

      (when stage-2-errors
        (error "could not load %s , the second stage of grail. with %s errors. USER_ELISP does not point to a working GRAIL install" stage-2-path (format-signal-trap stage-2-errors))) )

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
      "version control managed third party elisp")

    (defvar grail-dist-git
      (concat grail-dist-dir "git/")
      "version control managed third party elisp")

    (defvar grail-dist-svn
      (concat grail-dist-dir "svn/")
      "version control managed third party elisp")

    (defvar grail-elpa-load-path nil
      "The load-path extensions made by ELPA package activation")

    (defvar grail-boot-load-path load-path
      "The load-path as constructed by emacs before grail initialization")

    (defvar grail-platform-load-path nil
      "The load-path after the platform files have been loaded.")

    (defvar grail-state-path (concat (getenv "HOME") "/.emacs.d/")
      "The grail session state & persistent data directory which defaults to .emacs.d")

    (grail-trap
      "Loading the grail-cfg file for user path changes."
      ;; grail-cfg.el is a file for user to change the tree structure that grail
      ;; traverses before load-path is formed.
      (load-user-elisp "grail-cfg"))

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

    (grail-trap
      "Loading the OS specific elisp."

      (load-user-elisp
        (cond
          ((string-equal "gnu/linux" system-type)  "linux")
          ((string-equal "darwin"    system-type)  "darwin")
          ((string-equal "windows"   system-type)  "windows"))))

    ;; save the state of load-path after the platform file if any has
    ;; been loaded.
    (setq grail-platform-load-path load-path)

    ;; integrate the user's Elisp tree into the load-path for the first time.
    (grail-extend-load-path)

    ;;----------------------------------------------------------------------
    ;; support for profiles.
    ;;----------------------------------------------------------------------

    (defvar grail-local-profiles
      (when (grail-trap
	     "loading grail profiles support"
	     (load-user-elisp "grail-profile"))
	(concat grail-local-dir "profiles/"))
      "The directory containing Grail profiles modules.")

    ;; make sure there is a directory for session state and persistent data
    (grail-garuntee-dir-path grail-state-path)

    (grail-trap
      "loading user ELISP"
      ;; elisp loads the user's general elisp library.
      (load-user-elisp "elisp"))

    ;; load ELPA when it is installed.
    (grail-trap
      "load ELPA"
      (load-elpa-when-installed))

    ;; dummy compatability functions for recent features in emacs, or
    ;; broken ports.

    (unless (functionp 'window-system)
      ;; Annoying Emacs.app 0.9-rc2 compat.
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

      (grail-trap
        "redirect user-init-file and custom-file variables to grail-settings-file"
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
      (load-user-elisp grail-settings-file))

      (grail-trap
        "load interface level elisp files"

        ;; only loaded when there is an active terminal.
        (load-user-elisp "interface")

        (load-user-elisp "user")

        ;; load commands and keys last so they can use definitions from user.el
        ;; and friends.
        (load-user-elisp "commands")
        (load-user-elisp "keys"))

      ;; In deamon mode the GUI related values are not defined by
      ;; Emacs until a GUI frame is created. In this case place the
      ;; GUI configuration on frame creation hooks.

      ;; when not using emacs --daemon load frame.el and gui.el
      ;; immediately.

      (grail-trap
        "load GUI files"
        (cond
          ((daemonp) (progn
                       (add-to-list 'after-make-frame-functions 'grail-load-gui-configuration-once t)
                       (add-hook 'before-make-frame-hook 'grail-load-display-configuration-once) ))
          ((window-system) (progn
                             (load-user-elisp "display")
                             (load-user-elisp "gui") ))) )

      ;; load all the group files requested. defer profile loading
      ;; when starting as a daemon.
      (grail-trap
        "loading profiles from .emacs startup"
        (grail-load-requested-profiles)) ))

  (error
    (message "Grail aborted from an internal error! %s" (princ error-trap))
    (message "Please report this to %s" grail-maintainer-email))
  )
