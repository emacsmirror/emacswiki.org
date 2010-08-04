;;; bzr-ui.el --- Utility functions to navigate a working copy of a bazaar repository

;; Copyright (C) 2009 Mike Mattie

;; Author: Mike Mattie
;; License: LGPL-v3

;;; Commentary:

;; Utility functions to navigate a working copy. It assumes that all of
;; the branches are relative to the repository root (DVCS)

;; Currently the functions are bzr specific but hopefully that can change
;; as vc evolves.

;;; Code:

(require 'cm-path)
(require 'vc-bzr)

(defun bzr-find-repository-top ( path )
  "bzr-find-repository-top PATH

   given path as a starting point find the top of the repository.
  "
  (when path
    (let
      ((root-dir  nil)
        (next-dir (vc-bzr-root (file-name-directory path))))

      (while next-dir
        (setq root-dir next-dir)
        (setq next-dir (vc-bzr-root (file-name-directory (delete-trailing-path-separators root-dir)))) )

      root-dir)))

(defun make-branch-path-repository-relative ( branch-or-file )
  (let
    ((repository (bzr-find-repository-top branch-or-file)))

    (make-path-relative-to repository (file-name-directory branch-or-file)) ))

;; (bzr-find-repository-top "/usr/home/mattie/system/emacs/mattie/")

(defun bzr-pick-brances-list ( path )
  (let
    ((root (bzr-find-repository-top path)))

    (when root
      (filter-ls root t
        (type ?d)
        (!name "^\\."))) ))

;; (bzr-pick-brances-list "/usr/home/mattie/system/emacs/mattie/")

(defun bzr-branch-p ( path )
  "bzr-branch-p PATH

   Return the path if it is a bzr branch, nil otherwise.
  "
  (when (and (file-accessible-directory-p path)
             (vc-bzr-root path))
    path))


(defun bzr-prompt-for-branch-dir ( prompt in-dir &optional initialize default )
  "bzr-prompt-for-branch-dir PROMPT PATH &optional initialize

   PROMPT for a branch path in PATH. The path is returned. This
   should not be called directly, rather bzr-prompt-for-branch
   should be used.
  "
    (read-file-name prompt
      in-dir              ;; complete in directory
      (or default path)   ;; default if user presses enter
      nil                 ;; don't force it to match

      initialize ;; initial input
      'bzr-branch-p) )

(defun bzr-prompt-for-branch ( prompt &optional path )
  "bzr-prompt-for-branch PROMPT &optional PATH

   Search for the repository root from PATH.  Prompt the user for
   a branch path starting from the root of the repository.
  "
  (let
    ;; by default the prompting will start with either the given path,
    ;; the buffer path, or HOME.
    ((start-path (or path buffer-file-name (getenv "HOME")))
     (default nil)
     (root nil))

    (and
      (setq default (vc-bzr-root start-path))
      (setq root    (bzr-find-repository-top default)))

    ;; If a branch can be found then use that as a starting point.
    ;; root, and start from there by default.

    (unless (and default root)
      (setq default (file-name-directory start-path))
      (setq root (getenv "HOME")))

    ;; prompt the user with reasonably sane defaults.

    ;; NOTE: when the prompt ends with a colon the read-file-name detects
    ;;       it and re-formats the prompt a bit. This magic is not documented
    ;;       currently.
    (expand-file-name
      (grail-sanitize-path (concat
                             (bzr-prompt-for-branch-dir
                               (concat prompt " w/default(" default "):")
                               root nil default) "/"))) ))

(provide 'bzr-ui)
