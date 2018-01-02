;;; vc+.el --- Extensions for `vc.el'.
;;
;; Filename: vc+.el
;; Description: Extensions for `vc.el'.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 1999-2018, Drew Adams, all rights reserved.
;; Created: Thu Mar 11 16:45:20 1999
;; Version: 20.0
;; Last-Updated: Mon Jan  1 16:16:16 2018 (-0800)
;;           By: dradams
;;     Update #: 1496
;; URL: https://www.emacswiki.org/emacs/download/vc%2b.el
;; Keywords: internal, tools, unix
;; Compatibility: GNU Emacs 20.x
;;
;; Features that might be required by this library:
;;
;;   `avoid', `ediff', `ediff-diff', `ediff-help', `ediff-init',
;;   `ediff-merg', `ediff-mult', `ediff-util', `ediff-wind',
;;   `fit-frame', `frame-cmds', `frame-fns', `misc-fns', `ring',
;;   `ring+', `strings', `thingatpt', `thingatpt+', `vc', `vc+',
;;   `vc-', `vc-hooks'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Extensions for `vc.el'.
;;
;; Note: This code is quite OLD, and is LIKELY OBSOLETE NOW.  You
;;       might find it useful in some way to mine - or not. ;-)
;;
;; -------------------------------------------------------------------
;;
;;  See also the companion file `vc-.el'.
;;        `vc-.el' should be loaded before `vc.el'.
;;        `vc+.el' should be loaded after `vc.el'.
;;
;;  All Dired buffers are now treated as if in `vc-dired-mode'.
;;  For example, you can use `C-x v v' to register marked files.
;;
;;  New functions defined here:
;;
;;    `vc-dired-relist-entry', `vc-dired-update', `vc-ediff',
;;    `vc-status-below', `vc-status-below-other-frame',
;;    `vc-status-below-other-window',`vc-status-here',
;;    `vc-status-here-other-frame', `vc-status-here-other-window'.
;;
;;  New user option defined here: `vc-log-width'.
;;
;;  Other variable defined here: `vc-last-dired-option'.
;;
;;
;;  ***** NOTE: The following functions defined in `vc.el'
;;              have been REDEFINED HERE:
;;
;;    `vc-ensure-vc-buffer' - Treat Dired buffers like `vc-dired-mode'.
;;    `vc-finish-logentry' - 1. Uses `remove-windows-on'.
;;                           2. Doc string explains more.
;;                           3. Treats Dired like `vc-dired-mode'.
;;    `vc-log-mode' - 1. Doc string reflects new bindings.
;;                    2. `vc-comment-ring-index' is not local.
;;    `vc-next-action' - 1. Treats Dired buffers as `vc-dired-mode'.
;;                       2. Treats file registering like checking in:
;;                          `vc-start-entry' vs `vc-next-action-dired'.
;;                       3. Changes to log prompt and doc string.
;;    `vc-next-action-dired' - 1. Update all dired buffers.
;;                             2. `vc-dired-update-line' only if in
;;                                vc-dired buffer.
;;                             3. Redisplay only if < 2 files marked.
;;    `vc-next-action-on-file' - Calls `vc-register' with FILE arg.
;;    `vc-previous-comment' - 1. Better msg, with help on bindings.
;;                            2. Treat null `vc-comment-ring'.
;;    `vc-register' - 1. Lets `vc-next-action' register files too.
;;                       a. Added optional FILE argument.
;;                       b. Pass COMMENT arg to `vc-admin'.
;;                    2. Usable in Dired buffer too.
;;    `vc-rename-file' - 1. Can be called from Dired buffer.
;;                       2. Error if different directory.
;;                       3. Update buffer if Dired.
;;                       4. Added confirmation message at end.
;;    `vc-revert-buffer' - 1. Prefix arg => don't need confirmation.
;;                         2. Treats Dired buffers as `vc-dired-mode'.
;;    `vc-start-entry' - Lists bindings for previous comments in msg.
;;
;;
;;  The following binding is made here for vc-dired mode:
;;
;;    `"'        `vc-ediff'
;;
;;
;;  This file should be loaded after loading the standard GNU file
;;  `vc.el'.  So, in your `~/.emacs' file, do this:
;;  (eval-after-load "vc" '(require 'vc+))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2013/12/26 dadams
;;     Do not bother to require dired+.el.
;; 2010/12/04 dadams
;;     vc-dired-mode: Removed call to make-local-hook.
;; 1999/04/09 dadams
;;     Changed binding of vc-ediff in vc-dired-mode-map bc of conflict.
;; 1999/03/31 dadams
;;     1. vc-directory*, vc-status-here*:
;;        Set vc-last-dired-option at end (in VC Dired buffer, not original).
;;        "No files...": message -> error.
;;     2. vc-dired-update: Verbose arg is now a symbol ('Checked-out etc.).
;; 1999/03/29 dadams
;;     1. Bound vc-rename-file.
;;     2. Added (replacement of): vc-rename-file.
;; 1999/03/29 dadams
;;     1. Put menu-enable for vc-ediff.
;;     2. vc-register: Allow use in Dired mode.  Make read-file-name match.
;; 1999/03/29 dadams
;;     1. vc-next-action: Treat register like checkin: do vc-start-entry.
;;     2. vc-register: Pass comment arg to vc-admin.
;;     3. Ensure loaded before compile.
;; 1999/03/26 dadams
;;     1. Replaced vc-diff binding with vc-ediff.
;;     2. Added vc-ediff to menu-bar-ediff-menu (also done in menu-bar+.el.
;;     3. Added new derivation of vc-dired-mode, with new doc string.
;;     3. Added: vc-ediff (with new definition).
;; 1999/03/23 dadams
;;     1. Added: vc-last-dired-option.  Memorize last C-u option used per buffer.
;;     2. Added: vc-dired-update (replacement): Smart update.
;; 1999/03/23 dadams
;;     1. Added: vc-ediff, vc-status-here-other-frame,
;;        vc-status-here-other-window.
;;     2. vc-directory, vc-status-here: Added arg other-window-p.
;;     3. New key bindings: ediff-revision, vc-diff,
;;        vc-dired-update, vc-status-here-other-window,
;;        vc-status-here-other-frame
;; 1999/03/19 dadams
;;     1. Added: vc-version-other-window, vc-insert-headers, vc-print-log.
;;     2. vc-directory, vc-status-here: Put back `mouse-face' highlights.
;; 1999/03/19 dadams
;;     vc-directory: Prompt indicates prefix arg behavior.
;; 1999/03/19 dadams
;;     1. Added vc-directory (replacement).
;;     2. vc-status-here: Treat negative prefix arg.
;;     3. Restore original default-directory (bug fix).
;;     4. Force trailing slash (as in vc-directory).
;;     5. Change mode line to describe files shown.
;;     6. No buffer change if no files to show.
;; 1999/03/19 dadams
;;     Treat dired buffers roughly as if in vc-dired-mode:
;;     1. vc-dired-mode -> (or vc-dired-mode (eq major-mode 'dired-mode)).
;;     2. vc-next-action-dired: a) update all dired buffers; b) only do
;;        vc-dired-update-line if vc-dired-mode (dired as if vc-dired).
;;     3. Added vc-dired-relist-entry (but can't use it bc of bug (see comment).
;;     4. Added vc-next-action (replacement).
;;     5. vc-register: Corrected typos (%s% -> %s).
;;     6. vc-status-here: convert to abs via expand-file-name.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(eval-when-compile (require 'cl)) ;; case (plus, for Emacs <20: when, unless)
(require 'vc-)
(require 'vc)
(require 'ediff) ;; ediff-load-version-control, ediff-version-control-package

(require 'frame-cmds nil t) ;; (no error if not found): remove-windows-on
(require 'fit-frame nil t) ;; (no error if not found):
                           ;; fit-frame-empty-special-display-frame-width,
                           ;; fit-frame-empty-special-display-frame-height
                           ;; fit-frame
(require 'ring+ nil t) ;; (no error if not found): ring-insert+extend

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'vc+)
(require 'vc+)                      ; Ensure loaded before compile.

;;;;;;;;;;;;;;;;;;;

;;; FREE VARIABLES HERE:
;;; To quiet the byte compiler:
(defvar vc-dired-window-configuration)
(defvar vc-log-version)
(defvar vc-log-file)
(defvar vc-dired-switches)

;;;;;;;;;;;;;;;;;;;

;; Switch standard `vc-diff' bindings.
(define-key vc-dired-mode-map [?\"] 'vc-ediff)

(defvar vc-log-width 76
  "*`window-min-width' and `fill-column' for buffer \"*VC Log*\".")


;; REPLACES ORIGINAL in `vc.el':
;; Calls `vc-register' with FILE argument.
(defun vc-next-action-on-file (file verbose &optional comment)
  ;; COMMENT is used as an admin or checkin comment.
  (let ((vc-type (vc-backend file))
        owner version buffer)
    (cond

     ;; If the file is not under version control, register it
     ((not vc-type)
      (vc-register verbose comment file) ; DDA: FILE arg added.
;;;@@@Emacs19        (if vc-initial-comment
;;;@@@Emacs19     (setq vc-log-after-operation-hook
;;;@@@Emacs19           'vc-checkout-writable-buffer-hook)
;;;@@@Emacs19   (vc-checkout-writable-buffer file)))
      )
     ;; CVS: Changes to the master file need to be
     ;; merged back into the working file.
     ((and (eq vc-type 'CVS)
           (or (eq (vc-cvs-status file) 'needs-checkout)
               (eq (vc-cvs-status file) 'needs-merge)))
      (if (or vc-dired-mode
              (eq major-mode 'dired-mode) ; DDA: added.
              (yes-or-no-p
               (format "%s is not up-to-date.  Merge in changes now? "
                       (buffer-name))))
          (progn;; DDA: allow dired-mode too.
            (if (or vc-dired-mode (eq major-mode 'dired-mode))
                (and (setq buffer (get-file-buffer file))
                     (buffer-modified-p buffer)
                     (switch-to-buffer-other-window buffer)
                     (vc-buffer-sync t))
              (setq buffer (current-buffer))
              (vc-buffer-sync t))
            (if (and buffer (buffer-modified-p buffer)
                     (not (yes-or-no-p
                           (format
                            "Buffer %s modified; merge file on disc anyhow? "
                            (buffer-name buffer)))))
                (error "Merge aborted"))
            (let ((status (vc-backend-merge-news file)))
              (and buffer
                   (vc-resynch-buffer file t
                                      (not (buffer-modified-p buffer))))
              (if (not (zerop status))
                  (if (y-or-n-p "Conflicts detected.  Resolve them now? ")
                      (vc-resolve-conflicts)))))
        (error "%s needs update" (buffer-name))))

     ;; For CVS files with implicit checkout: if unmodified, don't do anything
     ((and (eq vc-type 'CVS)
           (eq (vc-checkout-model file) 'implicit)
           (not (vc-locking-user file))
           (not verbose))
      (message "%s is up to date" (buffer-name)))

     ;; If there is no lock on the file, assert one and get it.
     ((not (setq owner (vc-locking-user file)))
      ;; With implicit checkout, make sure not to lose unsaved changes.
      (and (eq (vc-checkout-model file) 'implicit)
           (buffer-modified-p buffer)
           (vc-buffer-sync))
      (if (and vc-checkout-carefully
               (not (vc-workfile-unchanged-p file t)))
          (if (save-window-excursion
                (pop-to-buffer "*vc-diff*")
                (goto-char (point-min))
                (insert-string (format "Changes to %s since last lock:\n\n"
                                       file))
                (not (beep))
                (yes-or-no-p
                 "File has unlocked changes.  Claim lock retaining changes? "))
              (progn (vc-backend-steal file)
                     (vc-mode-line file))
            (if (not (yes-or-no-p "Revert to checked-in version, instead? "))
                (error "Checkout aborted")
              (vc-revert-buffer1 t t)
              (vc-checkout-writable-buffer file))
            )
        (if verbose
            (if (not (eq vc-type 'SCCS))
                (vc-checkout file nil
                             (read-string "Branch or version to move to: "))
              (error "Sorry, this is not implemented for SCCS"))
          (if (vc-latest-on-branch-p file)
              (vc-checkout-writable-buffer file)
            (if (yes-or-no-p
                 "This is not the latest version.  Really lock it?  ")
                (vc-checkout-writable-buffer file)
              (if (yes-or-no-p "Lock the latest version instead? ")
                  (vc-checkout-writable-buffer
                   file
                   (if (vc-trunk-p (vc-workfile-version file))
                       "";; this means check out latest on trunk
                     (vc-branch-part (vc-workfile-version file)))))))
          )))

     ;; A checked-out version exists, but the user may not own the lock.
     ((and (not (eq vc-type 'CVS))
           (not (string-equal owner (vc-user-login-name))))
      (if comment
          (error "Sorry, you can't steal the lock on %s this way" file))
      (and (eq vc-type 'RCS)
           (not (vc-backend-release-p 'RCS "5.6.2"))
           (error "File is locked by `%s'" owner))
      (vc-steal-lock
       file
       (if verbose (read-string "Version to steal: ")
         (vc-workfile-version file))
       owner))

     ;; OK, user owns the lock on the file
     (t;; DDA: allow dired-mode too.
      (if (or vc-dired-mode (eq major-mode 'dired-mode))
          (find-file-other-window file)
        (find-file file))

      ;; If the file on disk is newer, then the user just
      ;; said no to rereading it.  So the user probably wishes to
      ;; overwrite the file with the buffer's contents, and check
      ;; that in.
      (if (not (verify-visited-file-modtime (current-buffer)))
          (if (yes-or-no-p "Replace file on disk with buffer contents? ")
              (write-file (buffer-file-name))
            (error "Aborted"))
        ;; if buffer is not saved, give user a chance to do it
        (vc-buffer-sync))

      ;; Revert if file is unchanged and buffer is too.
      ;; If buffer is modified, that means the user just said no
      ;; to saving it; in that case, don't revert,
      ;; because the user might intend to save
      ;; after finishing the log entry.
      (if (and (vc-workfile-unchanged-p file)
               (not (buffer-modified-p)))
          (cond;; Revert file only upon confirmation.
           ((yes-or-no-p "No changes since last checkin.  Revert to checked-in version? ")
            (vc-backend-revert file)
            (vc-resynch-window file t t)))

        ;; user may want to set nonstandard parameters
        (if verbose
            (setq version (read-string "New version level: ")))
        ;; OK, let's do the checkin
        (vc-checkin file version comment))))))


;; REPLACES ORIGINAL in `vc.el':
;; 1. Update all dired buffers.
;; 2. `vc-dired-update-line' only if in vc-dired buffer.
;; 3. Redisplay only if at most one file is marked.
;; NOTE: `vc-dired-window-configuration' is free here.
(defun vc-next-action-dired (file rev comment)
  "Do a vc-next-action-on-file on each of the marked files,
possibly passing on the log comment we've just entered."
  (let ((dired-buffer (current-buffer))
        (dired-dir default-directory))
    (dired-map-over-marks
     (let ((file (dired-get-filename)))
       (message "Processing %s..." file)
       ;; Adjust the default directory so that checkouts
       ;; go to the right place.
       (let ((default-directory (file-name-directory file)))
         (vc-next-action-on-file file nil comment)
         (set-buffer dired-buffer))
       ;; Make sure that files don't vanish after they are checked in.

       ;; TRYING TO USE vc-dired-relist-entry here and removed
       ;; following dired-do-redisplay.
       ;; CANNOT USE `vc-dired-relist-entry' here (?).
       ;; Bug somewhere (in macro `dired-map-over-marks'?).
       (let ((vc-dired-terse-mode nil))
           (dired-relist-file file)
           )
;;;@@@Emacs19 ;;;;          (dired-fun-in-all-buffers (file-name-directory file)
;;;@@@Emacs19 ;;;;                                    nil
;;;@@@Emacs19 ;;;;                                    (function vc-dired-relist-entry)
;;;@@@Emacs19 ;;;;                                    file))
;;;        ;; Since can't use `vc-dired-relist-entry', at least
;;;        ;; update the line when called from a vc-dired buffer.
;;;        (when vc-dired-mode (vc-dired-update-line file))
       (set-window-configuration vc-dired-window-configuration)
       (message "Processing %s...done" file))
     nil
     (not (save-excursion               ; At most one marked file.
            (goto-char (point-min))
            (re-search-forward (dired-marker-regexp) nil t 2)))))
  (dired-move-to-filename))

;; Like `dired-relist-entry', but vc display in vc-dired-mode.
;; CANNOT USE this in place of `dired-relist-entry' in `vc-next-action-dired'.
;; For some reason, there is a bug somewhere (I think it's in the macro
;;`dired-map-over-marks').
;;;;;; (@@@ IS THIS STILL TRUE IN EMACS20?)
(defun vc-dired-relist-entry (file)
  "Relist the line for FILE, or just add it if it did not exist.
FILE must be an absolute file name.
Like `dired-relist-entry', but vc display in vc-dired-mode."
  (if (eq major-mode 'dired-mode)
      (dired-relist-entry file)
    (save-excursion
      (dired-goto-file file)
      (dired-do-redisplay file))))


;;; The major entry point.

;; REPLACES ORIGINAL in `vc.el':
;; 1. Treats Dired buffers as if in `vc-dired-mode'.
;; 2. Treats file registering like checking in
;;    (calls `vc-start-entry', not `vc-next-action-dired').
;; 3. Changes to log prompt and doc string.
;; NOTE: `vc-dired-window-configuration' is free here.
;;;###autoload
(defun vc-next-action (arg)
  "Do next logical version control operation: register, check in/out.
If you call this from within a Dired or VC Dired buffer:

  This operates on each marked (*) file; if no files are marked,
  then the current line's file is treated as if it were marked.

  If any of the marked files are to be registered or checked in, then
  you enter a log message in the *VC Log* buffer, which is popped up.
  (This log comment is ignored for any files being checked out.)

  Attempted lock steals raise errors.

If you call this from a buffer visiting a file:

For RCS and SCCS files:
  If the file is not yet registered for version control, this
  registers it, then checks it out for you (alone) to edit.

  If the file is not checked out by anyone, this checks it out
  for you (alone) to edit.

  If the file is checked out by you, this first checks to see if
  it has actually changed since you checked it out.
    If not, the file is reverted, upon confirmation.
    If so, then check-in begins:
           You enter a log message in the *VC Log* buffer, which is
           popped up, then finish the checkin via `\\<vc-log-entry-mode>\\[vc-finish-logentry]'.
           If `vc-keep-workfiles' is non-nil (default), a
           read-only copy of the changed file is left in place afterwards.

  For checkin, a prefix ARG lets you specify the version number to use.

  If the file is checked out by someone else, you are given the
  option of checking it out by stealing the check-out lock.

For CVS files:
   If the file is not already registered, this registers it.
   This does a \"cvs add\", but no \"cvs commit\".

   If the file is added but not committed, it is committed.

   If your working file is changed, but the repository file is
   unchanged, this pops up a buffer for entry of a log message; when
   the message has been entered, it checks in the resulting changes
   along with the logmessage as change commentary.  A writable file is
   retained.

   If the repository file is changed, you are asked if you want to
   merge the changes into your working copy."
  (interactive "P")
  (catch 'nogo
    (if (or vc-dired-mode (eq major-mode 'dired-mode))
        (let ((files (dired-get-marked-files)))
          (set (make-local-variable 'vc-dired-window-configuration)
               (current-window-configuration))
          (if (string=
               ""
               (mapconcat
                (function
                 (lambda (f)
                   (if (eq (vc-backend f) 'CVS)
                       (if (or (eq (vc-cvs-status f) 'locally-modified)
                               (eq (vc-cvs-status f) 'locally-added))
                           "@" "")
                     (if (or (vc-locking-user f) ; checked out
                             (not (vc-name f))) ; not registered
                         "@"
                       ""))))
                files
                ""))
              (vc-next-action-dired nil nil "dummy")
            (vc-start-entry nil nil nil
                            (format "Marked files in %s:" (buffer-name))
                            'vc-next-action-dired))
          (throw 'nogo nil)))
    (while vc-parent-buffer (pop-to-buffer vc-parent-buffer))
    (if buffer-file-name
        (vc-next-action-on-file buffer-file-name arg)
      (error "Buffer `%s' is not associated with a file" (buffer-name)))))


;; REPLACES ORIGINAL in `vc.el':
;; 1. Lets `vc-next-action' register files too.
;;    a. Added optional FILE argument.
;;    b. Pass COMMENT arg to `vc-admin'.
;; 2. Usable in Dired buffer too.
;;;###autoload
(defun vc-register (&optional override comment file)
  "Register FILE (default: buffer's) into your version-control system.
Non-nil prefix arg OVERRIDE means you are prompted for version number.
COMMENT is the comment for registering."
  (interactive
   (list current-prefix-arg
         nil
         (or (buffer-file-name)         ; Buffer's file.
             (and (eq 'dired-mode major-mode)
                  (dired-get-filename)) ; Dired cursor's file.
             (read-file-name "File to register: "
                             nil nil 'match-and-confirm))))
  ;; Watch out for new buffers of size 0: The corresponding file
  ;; does not exist yet, even though buffer-modified-p is nil.
  (when (and (or (not file)             ; Using buffer's file.
                 (eq t (car (file-attributes file)))) ; e.g. directory
             (or buffer-file-name (error "No visited file"))
             (not (buffer-modified-p))
             (zerop (buffer-size))
             (not (file-exists-p buffer-file-name)))
    (set-buffer-modified-p t))
  (setq file (or file buffer-file-name))


  (let ((master (vc-name file)))
    (and master
         (file-exists-p master)
         (error "File `%s' is already registered" file))
    (and master
         (not (y-or-n-p "Previous master file has vanished.  Make a new one? "))
         (error "File `%s' is already registered" file)))
  (unless file (vc-buffer-sync))
  (unless vc-make-backup-files
    ;; Inhibit backup for this buffer.
    (make-local-variable 'backup-inhibited)
    (setq backup-inhibited t))
  (vc-admin
   file
   (or (and override
            (read-string
             (format "Initial version level for %s: " file)))
       vc-default-init-version)
   comment)
  ;; Recompute backend property (it may have been set to nil before).
  (setq vc-buffer-backend (vc-backend (buffer-file-name))))


;; REPLACES ORIGINAL in `vc.el':
;; Mentions `vc-previous-comment', `vc-next-comment',
;; `vc-comment-search-reverse', and `vc-comment-search-forward' in message.
;; Uses `vc-log-width'.
;; NOTE: `vc-log-version' is free here.
(defun vc-start-entry (file rev comment msg action &optional after-hook)
  "Accept a comment for an operation on FILE version REV.
If COMMENT is nil, pop up the *VC Log* buffer, emit MSG, and set the
action on close to ACTION.  Otherwise, do ACTION immediately.
Remember the file's buffer in `vc-parent-buffer' (current one if no file).
AFTER-HOOK specifies the local value for `vc-log-operation-hook'."
  (let ((parent (if file (find-file-noselect file) (current-buffer))))
    (if vc-before-checkin-hook
        (if file
            (save-excursion
              (set-buffer parent)
              (run-hooks 'vc-before-checkin-hook))
          (run-hooks 'vc-before-checkin-hook)))
    (if comment
        (set-buffer (get-buffer-create "*VC Log*"))
      (let ((fit-frame-empty-special-display-frame-width (+ 2 vc-log-width))
            (fit-frame-empty-special-display-frame-height (+ 10 window-min-height)))
        (pop-to-buffer (get-buffer-create "*VC Log*"))))
    (set (make-local-variable 'vc-parent-buffer) parent)
    (set (make-local-variable 'vc-parent-buffer-name)
         (concat " from " (buffer-name vc-parent-buffer)))
    (vc-mode-line (or file " (no file)"))
    (vc-log-mode file)
    (make-local-variable 'vc-log-after-operation-hook)
    (when after-hook (setq vc-log-after-operation-hook after-hook))
    (setq vc-log-operation action)
;;;@@@Emacs19    (setq vc-log-file file)
    (setq vc-log-version rev)
    (if comment
        (progn (erase-buffer)
               (if (eq comment t)
                   (vc-finish-logentry t)
                 (insert comment)
                 (vc-finish-logentry nil)))
      (message (substitute-command-keys
                "%s  \\<vc-log-entry-mode>\\[vc-finish-logentry] in \
*VC Log* when done.  \\[vc-previous-comment], \\[vc-next-comment], \
\\[vc-comment-search-reverse], \\[vc-comment-search-forward]: previous \
comments.")
               msg))))


;; REPLACES ORIGINAL in `vc.el':
;; 1. Creates `vc-comment-ring' if doesn't exist.
;; 2. Doesn't add comment to ring if empty.
;; 3. Resets ring index so `vc-previous-comment' and `vc-next-comment' use latest.
;; 4. Uses `remove-windows-on'.
;; 5. Doc string explains more.
;; 6. Treats Dired buffers as `vc-dired-mode'.
;; NOTE: `vc-log-version' and `vc-log-file' are free here.
;;;###autoload
(defun vc-finish-logentry (&optional nocomment)
  "Complete the operation implied by the current log entry.
Checks in the file(s) associated with the log buffer, saving the log
message as a change (delta) commentary.  If the variable
`vc-keep-workfiles' is non-nil (default), a read-only copy of each
checked-in file is left in place afterwards.
Non-nil NOCOMMENT means do not use a comment."
  (interactive)
  (let ((buf-strg (buffer-string)))
    ;; Check and record the comment, if any.
    (unless nocomment
      (goto-char (point-max))
      (unless (bolp) (newline))
      (vc-backend-logentry-check vc-log-file) ; Comment too long?
      (unless vc-comment-ring           ; Record comment in comment ring.
        (setq vc-comment-ring (make-ring vc-maximum-comment-ring-size)))
      (unless (string= "" buf-strg)
        (if (fboundp 'ring-insert+extend)
            (ring-insert+extend vc-comment-ring buf-strg)
          (ring-insert vc-comment-ring buf-strg)))
      ;; Reset index so `vc-previous-comment' and `vc-next-comment' use latest.
      (setq vc-comment-ring-index nil))
    ;; Sync parent buffer in case user modified it while editing the comment.
    ;; But not if it is a vc-dired or dired buffer.
    (save-excursion
      (set-buffer vc-parent-buffer)
      (unless (or vc-dired-mode (eq major-mode 'dired-mode))
        (vc-buffer-sync)))
    (unless vc-log-operation (error "No log operation is pending"))
    ;; Save parameters held in buffer-local variables.
    (let ((log-operation vc-log-operation)
          (log-file vc-log-file)
          (log-version vc-log-version)
          (after-hook vc-log-after-operation-hook)
          (tmp-vc-parent-buffer vc-parent-buffer))
      ;; Return to "parent" buffer of this checkin and remove checkin window.
      (pop-to-buffer vc-parent-buffer)
      (save-excursion                   ; OK, do it to it
        (funcall log-operation
                 log-file
                 log-version
                 buf-strg))
      ;; Remove checkin window (after the checkin so that if that fails
      ;; we don't zap the *VC Log* buffer and the typing therein).
      (let ((logbuf (get-buffer "*VC Log*")))
        (when logbuf
          (save-excursion (set-buffer logbuf) (erase-buffer))
          (if (or vc-delete-logbuf-window (window-dedicated-p (get-buffer-window logbuf t)))
              (if (fboundp 'remove-windows-on)
                  (remove-windows-on logbuf)
                (delete-windows-on logbuf))
            (pop-to-buffer "*VC Log*")
            (bury-buffer))
          (pop-to-buffer tmp-vc-parent-buffer)))
      (when buffer-file-name            ; Make sure we see expanded headers.
        (vc-resynch-window buffer-file-name vc-keep-workfiles t))
      (when (or vc-dired-mode (eq major-mode 'dired-mode))
        (dired-move-to-filename))
      (run-hooks after-hook 'vc-finish-logentry-hook))))


;; REPLACES ORIGINAL in `vc.el':
;; 1. Better message, with help on bindings.
;; 2. Treat null `vc-comment-ring'.
;;;###autoload
(defun vc-previous-comment (arg)
  "Cycle backwards through comment history.
Prefix ARG determines how many comments backward to go.
If ARG is positive, go forward, not back."
  (interactive "*p")
  (let ((len (ring-length vc-comment-ring)))
    (cond ((or (null len) (<= len 0))
           (message "No previous log comments.") (ding))
          (t
           (erase-buffer)
           ;; Initialize the index on the first use of this command
           ;; so that the first M-p gets index 0, and the first M-n gets
           ;; index -1.
           (unless vc-comment-ring-index
             (setq vc-comment-ring-index (if (> arg 0)
                                             -1
                                           (if (< arg 0) 1 0))))
           (setq vc-comment-ring-index (mod (+ vc-comment-ring-index arg) len))
           (message (substitute-command-keys "%d%s of %d previous comments    \
\(\\<vc-log-entry-mode>\\[vc-previous-comment], \\[vc-next-comment], \
\\[vc-comment-search-reverse], \\[vc-comment-search-forward], \
\\[vc-finish-logentry])")
                    (1+ vc-comment-ring-index)
                    (ordinal-suffix (1+ vc-comment-ring-index))
                    (ring-length vc-comment-ring))
           (insert (ring-ref vc-comment-ring vc-comment-ring-index))))))


;; REPLACES ORIGINAL in `vc.el'.
;; Treat Dired buffers as if in `vc-dired-mode'.
(defun vc-ensure-vc-buffer ()
  "Make sure that the current buffer visits a version-controlled file."
  (if (or vc-dired-mode (eq major-mode 'dired-mode))
      (set-buffer (find-file-noselect (dired-get-filename)))
    (while vc-parent-buffer (pop-to-buffer vc-parent-buffer))
    (unless (buffer-file-name)
      (error "Buffer `%s' is not associated with a file" (buffer-name)))
    (unless (vc-backend (buffer-file-name))
      (error "File `%s' is not under version control" (buffer-file-name)))))


;; REPLACES ORIGINAL in `vc.el':
;; Different doc string.
;; NOTE: `vc-dired-switches' is free here.
(define-derived-mode vc-dired-mode dired-mode "Dired VC"
  "The major mode used in VC directory buffers.
It is derived from Dired; all Dired commands operate normally,
with the exception of `v', which is redefined as the version control
prefix.

It works like Dired, but lists only files under version control.

The currently displayed files may be one of the following (as
indicated in the buffer's mode line):
  - Checked-Out
  - Registered (i.e. checked in or out)
  - Unregistered
The current VC state of each file is indicated in place of the file's
link count, owner, group and size.

Keys bound to VC commands execute as though called on a buffer
attached to the file named in the current Dired buffer line.
You can type `vl', `v=' etc. to invoke `vc-print-log', `vc-diff', and
the like on the file named on the current Dired buffer line.  `vv'
invokes `vc-next-action' on this file, or on all files currently
marked.  The special command, `*l' marks all files currently locked."
  ;; (make-local-hook 'dired-after-readin-hook)
  (add-hook 'dired-after-readin-hook 'vc-dired-hook nil t)
  ;; The following is slightly modified from `dired.el',
  ;; because file lines look a bit different in `vc-dired-mode'.
  (set (make-local-variable 'dired-move-to-filename-regexp)
       (let*
           ((l "\\([A-Za-z]\\|[^\0-\177]\\)")
            ;; In some locales, month abbreviations are as short as 2 letters,
            ;; and they can be padded on the right with spaces.
            (month (concat l l "+ *"))
            ;; Recognize any non-ASCII character.
            ;; The purpose is to match a Kanji character.
            (k "[^\0-\177]")
            ;; (k "[^\x00-\x7f\x80-\xff]")
            (s " ")
            (yyyy "[0-9][0-9][0-9][0-9]")
            (mm "[ 0-1][0-9]")
            (dd "[ 0-3][0-9]")
            (HH:MM "[ 0-2][0-9]:[0-5][0-9]")
            (western (concat "\\(" month s dd "\\|" dd s month "\\)"
                             s "\\(" HH:MM "\\|" s yyyy"\\|" yyyy s "\\)"))
            (japanese (concat mm k s dd k s "\\(" s HH:MM "\\|" yyyy k "\\)")))
         (concat s "\\(" western "\\|" japanese "\\)" s)))
  (and (boundp 'vc-dired-switches)
       vc-dired-switches
       (set (make-local-variable 'dired-actual-switches)
            vc-dired-switches))
  (set (make-local-variable 'vc-dired-terse-mode) vc-dired-terse-display)
  (setq vc-dired-mode t))


;; REPLACES ORIGINAL in `vc.el':
;; 1. Added arg SUPPRESS-CONFIRM-P: => doesn't ask for confirmation.
;; 2. Treats Dired buffers as if in `vc-dired-mode'.
;;;###autoload
(defun vc-revert-buffer (&optional suppress-confirm-p)
  "Revert current buffer's file back to the version it was based on.
Unless prefix arg SUPPRESS-CONFIRM-P is non-nil, this asks for
confirmation if buffer contents are not identical to that version
\(NB: regardless of `vc-suppress-confirm').

Note that for RCS and CVS, this function does not
automatically pick up newer changes found in the master file;
use \\[universal-argument] \\[vc-next-action] RET to do so."
  (interactive "P")
  (vc-ensure-vc-buffer)
  (let ((file buffer-file-name)
        (vc-suppress-confirm nil)       ; Ask anyway.
        (obuf (current-buffer))
        (changed (vc-diff nil t)))
    (if changed
        (unwind-protect
            (when (or suppress-confirm-p
                      (not (yes-or-no-p "Discard changes? ")))
              (error "Revert cancelled"))
          (if (and (window-dedicated-p (selected-window))
                   (one-window-p t 'selected-frame))
              (make-frame-invisible (selected-frame))
            (delete-window))))
    (set-buffer obuf)
    (vc-backend-revert file)
    (vc-resynch-window file t t)))

(defvar vc-last-dired-option nil
  "The value of the last choice for vc-dired display:
`Checked-out', `Registered', or `Unregistered'.
Set in functions`vc-status-below...' and `vc-status-here...'.")
(make-variable-buffer-local 'vc-last-dired-option)


;;;###autoload
(defun vc-status-below-other-window (dirname verbose)
  "Show VC status of directory and its subdirs in other window.
If you don't need the subdirs, then use `\\[vc-status-here-other-window]' instead.
DIRNAME is the directory.
Prefix arg VERBOSE:
  With no prefix arg (nil), checked-out files are listed.
  With a positive prefix arg, registered files are listed.
  With a negative prefix arg, unregistered files are listed."
  (interactive
   (let ((verb (cond ((and current-prefix-arg
                           (natnump (prefix-numeric-value current-prefix-arg)))
                      'Registered)
                     (current-prefix-arg 'Unregistered)
                     ('Checked-out))))
     (list
      (read-file-name (format "%s files here and below: " verb)
                      default-directory default-directory)
      verb)))
  (vc-status-below dirname verbose t))


;;;###autoload
(defun vc-status-below-other-frame (dirname verbose)
  "Show VC status of directory and its subdirs in other frame.
If you don't need the subdirs, then use `\\[vc-status-here-other-frame]' instead.
DIRNAME is the directory.
Prefix arg VERBOSE:
  With no prefix arg (nil), checked-out files are listed.
  With a positive prefix arg, registered files are listed.
  With a negative prefix arg, unregistered files are listed."
  (interactive
   (let ((verb (cond ((and current-prefix-arg
                           (natnump (prefix-numeric-value current-prefix-arg)))
                      'Registered)
                     (current-prefix-arg 'Unregistered)
                     ('Checked-out))))
     (list
      (read-file-name (format "%s files here and below: " verb)
                      default-directory default-directory)
      verb)))
  (vc-status-below dirname verbose 5))


;;;###autoload
(defun vc-dired-update (verbose)
  "Update the current directory in `vc-dired-mode'.
The kind of files listed depends on the prefix argument VERBOSE:
  With just `\\[universal-argument]' (no explicit number):
      Checked-out files are listed.
  With an explicit positive prefix arg (e.g. `\\[universal-argument] 2'):
      Registered files are listed.
  With an explicit negative prefix arg (e.g. `\\[negative-argument]'):
      Unregistered files are listed.
  With no prefix arg (nil):
      The kind of files listed remains as before.
This calls `vc-status-below' or `vc-status-here', as appropriate:
  `vc-status-below' if any directories are currently listed;
  `vc-status-here', otherwise."
  (interactive "P")
  (if verbose                           ; change it
      (if (consp verbose)
          (setq verbose 'Checked-out)
        (if (natnump (prefix-numeric-value verbose))
            (setq verbose 'Registered)
          (setq verbose 'Unregistered)))
    (setq verbose vc-last-dired-option)) ; no change
  (if (save-excursion (goto-char (point-min))
                      (forward-line 2)  ; skip header
                      (search-forward "/" nil t))
      (vc-status-below default-directory verbose)
    (vc-status-here verbose)))


(or (fboundp 'old-vc-rename-file)
(fset 'old-vc-rename-file (symbol-function 'vc-rename-file)))

;; REPLACES ORIGINAL in `vc.el':
;; 1. New interactive spec so can be called from Dired buffer.
;; 2. Error if different directory.
;; 3. Update buffer if Dired.
;; 4. Added confirmation message at end.
;;;###autoload
(defun vc-rename-file (old new)
  "Rename file OLD to NEW, and rename its master file likewise."
  (interactive
   (let* ((old1 (or (buffer-file-name)  ; Buffer's file.
                    (and (or vc-dired-mode
                             (eq 'dired-mode major-mode))
                         (dired-get-filename)) ; Dired cursor's file.
                    (read-file-name "File to VC rename: "
                                    nil nil 'match-and-confirm)))
          (new1 (read-file-name
                 (format "Rename `%s' to: "
                         (file-name-nondirectory old1)))))
     (list old1 new1)))
  ;; There are several ways of renaming files under CVS 1.3, but they all
  ;; have serious disadvantages.  See the FAQ (available from think.com in
  ;; pub/cvs/).  I'd rather send the user an error, than do something he might
  ;; consider to be wrong.  When the famous, long-awaited rename database is
  ;; implemented things might change for the better.  This is unlikely to occur
  ;; until CVS 2.0 is released.  --ceder 1994-01-23 21:27:51
  (when (eq (vc-backend old) 'CVS)
    (error "Renaming files under CVS is dangerous and not supported in VC"))
  (unless (string=
           (expand-file-name (file-name-directory new))
           (expand-file-name (file-name-directory old)))
    (error "Cannot change directories"))
  (old-vc-rename-file old new)
  (cond (vc-dired-mode (vc-dired-update nil))
        ((eq 'dired-mode major-mode) (revert-buffer)))
  (message
   "File `%s' renamed to `%s' (VC master renamed too)."
   (file-name-nondirectory old) (file-name-nondirectory new)))


;; REPLACES ORIGINAL in `vc.el':
;; 1. Doc string reflects new bindings.
;; 2. `vc-comment-ring-index' is not local.
;; NOTE: `vc-log-version' and `vc-log-file' are free here.
;;;###autoload
(defun vc-log-mode (&optional file)
  "Minor mode for driving version-control tools.
These bindings are added to global keymap when you enter this mode:
\\[vc-next-action]\t-- Perform next logical version-control operation.
\\[vc-register]\t-- Register current file.
\\[vc-toggle-read-only]\t-- Like `\\[vc-next-action]', but won't register \
files.
\\[vc-insert-headers]\t-- Insert version-control headers in current file.
\\[vc-print-log]\t-- Display change history of current file.
\\[vc-revert-buffer]\t-- Revert buffer to latest version.
\\[vc-cancel-version]\t-- Undo latest checkin.
\\[vc-diff]\t-- Show diffs between file versions.
\\[vc-version-other-window]\t-- Visit old version in another window.
\\[vc-status-here]\t-- Show version-control files in current directory.
\\[vc-status-here-other-window]\t-- Same as above, but in another window.
\\[vc-status-here-other-frame]\t-- Same as above, but in another frame.
\\[vc-status-below]\t-- Show version-control files in or below current dir.
\\[vc-status-below-other-window]\t-- Same as above, but in another window.
\\[vc-status-below-other-frame]\t-- Same as above, but in another frame.
\\[vc-annotate]         Display the CVS annotate command.
\\[vc-update-change-log]\t-- Add change log entry from recent checkins.

While you are entering a change log message for a version, the
following additional bindings are in effect.\\<vc-log-entry-mode>

\\[vc-finish-logentry]\t-- Proceed with check in, ending log message entry.

Whenever you do a checkin, your log comment is added to a ring of
saved comments.  These can be recalled as follows:

\\[vc-next-comment]\t-- Replace region with next message in comment ring.
\\[vc-previous-comment]\t-- Replace region with previous message in comment \
ring.
\\[vc-comment-search-reverse]\t-- Search backward for regexp in the comment \
ring.
\\[vc-comment-search-forward]\t-- Search backward for regexp in the comment \
ring.

Entry to the change-log submode calls the value of `text-mode-hook',
then the value of `vc-log-mode-hook'.

Global user options:
`vc-initial-comment'\tIf non-nil, require user to enter a change
\t\t\tcomment upon first checkin of the file.

`vc-keep-workfiles'\tNon-nil value prevents workfiles from being
\t\t\tdeleted when changes are checked in

`vc-suppress-confirm'\tSuppresses some confirmation prompts,
\t\t\tnotably for buffer reversions.

`vc-header-alist'\tWhich keywords to insert when adding headers
\t\t\twith \\[vc-insert-headers].  Defaults to
\t\t\t'(\"\%\W\%\") under SCCS, '(\"\$Id\$\") under RCS and CVS.

`vc-static-header-alist'
\t\t\tBy default, version headers inserted in C
\t\t\tfiles get stuffed in a static string area so
\t\t\tthat ident(RCS/CVS) or what(SCCS) can see them in
\t\t\tthe compiled object code.  You can override
\t\t\tthis by setting this variable to nil, or
\t\t\tchange the header template by changing it.

`vc-command-messages'\tIf non-nil, display run messages from the
\t\t\tactual version-control utilities (this is
\t\t\tintended primarily for people hacking vc
\t\t\titself)."
  (interactive)
  (set-syntax-table text-mode-syntax-table)
  (use-local-map vc-log-entry-mode)
  (setq local-abbrev-table text-mode-abbrev-table)
  (setq major-mode 'vc-log-mode)
  (setq mode-name "VC-Log")
  (make-local-variable 'vc-log-file)
  (setq vc-log-file file)
  (make-local-variable 'vc-log-version)
  (make-local-variable 'vc-comment-ring-index)
  (set-buffer-modified-p nil)
  (setq buffer-file-name nil)
  (set-fill-column vc-log-width)
  (run-hooks 'text-mode-hook 'vc-log-mode-hook))


;;;###autoload
(defun vc-status-here-other-window (verbose)
  "Show VC status of current directory's files in other window.
Prefix arg VERBOSE:
  With no prefix arg (nil), checked-out files are listed.
  With a positive prefix arg, registered files are listed.
  With a negative prefix arg, unregistered files are listed."
  (interactive "P")
  (vc-status-here verbose t))


;;;###autoload
(defun vc-status-here-other-frame (verbose)
  "Show VC status of current directory's files in other frame.
Prefix arg VERBOSE:
  With no prefix arg (nil), checked-out files are listed.
  With a positive prefix arg, registered files are listed.
  With a negative prefix arg, unregistered files are listed."
  (interactive "P")
  (vc-status-here verbose 5))


;;;###autoload
(defun vc-status-here (verbose &optional other-window-p)
  "Show version-control status of files in current directory.
Prefix arg VERBOSE:
  With no prefix arg (nil), checked-out files are listed.
  With a positive prefix arg, registered files are listed.
  With a negative prefix arg, unregistered files are listed.

Optional arg OTHER-WINDOW-P:
  natnump means use another frame,
  otherwise non-nil means use another window."
  (interactive "P")
  (require 'dired)
  (setq verbose (and verbose (prefix-numeric-value verbose)))
  (cond ((natnump verbose) (setq verbose 'Registered))
        (verbose (setq verbose 'Unregistered))
        ((setq verbose 'Checked-out)))
  (message "%s files in %s..." verbose default-directory)
  (let* ((orig-dirname default-directory) ; Save to restore to vc dired buffer.
         (dirname (expand-file-name default-directory))
         (dl (length dirname))
         (filelist nil)
         (statelist nil)
         dired-buf)
    ;; Force a trailing slash.
    (if (not (eq (elt dirname (1- (length dirname))) ?/))
        (setq dirname (concat dirname "/")))
    (mapcar
     (function
      (lambda (f)
        (or (string-equal f ".")
            (string-equal f "..")
            (let ((dirf (concat dirname f)))
              (or (file-symlink-p dirf) ; Avoid possible loops.
                  (funcall
                   (function
                    (lambda (f)
                      (case verbose
                        (Registered
                         (when (vc-registered f)
                           (let ((state (vc-dired-state-info f)))
                             (setq filelist (cons (substring f dl) filelist))
                             (setq statelist (cons state statelist)))))
                        (Unregistered
                         (unless (or (file-directory-p f) ; skip directories
                                     (vc-registered f))
                           (setq filelist (cons (substring f dl) filelist))))
                        (Checked-out
                         (when (vc-registered f)
                           (let ((state (vc-dired-state-info f)))
                             (when state
                               (setq filelist (cons (substring f dl) filelist))
                               (setq statelist (cons state statelist)))))))))
                   dirf))))))
     (directory-files dirname))
    (if (eq 0 (length filelist))
        (error "No files are currently %s in %s"
               (downcase (format "%s" verbose))
               default-directory)
      (save-window-excursion
        (save-excursion
          ;; This counts on a semi-documented feature of dired; providing a SWITCH
          ;; argument forces the buffer to revert.
          (setq dired-buf
                (dired-internal-noselect (cons dirname (nreverse filelist))
                                         dired-listing-switches
                                         'vc-dired-mode))))
      (if other-window-p
          (if (natnump other-window-p)
              (switch-to-buffer-other-frame dired-buf)
            (switch-to-buffer-other-window dired-buf))
        (switch-to-buffer dired-buf))
      (setq mode-name (format "Dired VC:%s" verbose))
      (setq default-directory orig-dirname) ; Restore original (e.g. ~/t vs /.../t).
      ;; Make a few modifications to the header
      (setq buffer-read-only nil)
      (goto-char (point-min))
      (forward-line 1);; Skip header line
      (let ((start (point)));; Erase (but don't remove) the
        (end-of-line);; "wildcard" line.
        (delete-region start (point)))
      (beginning-of-line)
      (setq vc-last-dired-option verbose) ; Remember for `vc-dired-update'.
      (message "Formatting directory (%s files)..."
               (setq verbose (downcase (format "%s" verbose))))
      (mapcar;; Plug the version information into the individual lines
       (function
        (lambda (x)
          (forward-char 2);; skip dired's mark area
          (vc-dired-reformat-line x)
          (forward-line 1)));; go to next line
       (nreverse statelist))
      ;; Highlight lines of file names for mouse selection.
      (dired-insert-set-properties (point-min) (point-max))
      (when (fboundp 'fit-frame) (fit-frame))
      (setq buffer-read-only t)
      (goto-char (point-min))
      (message "Formatting directory (%s files)...done" verbose)
      (dired-next-line 2))))


;; This is like `vc-directory', except for:
;;  - its treatment of prefix arg
;;  - messages
;;  - no buffer change if no files to show
;;  - `mouse-face' highlights.
;;  - optional arg: OTHER-WINDOW-P
;;;###autoload
(defun vc-status-below (dirname verbose &optional other-window-p)
  "Show version-control status of directory and its subdirectories.
If you don't need the subdirectories, then use `\\[vc-status-here]' instead.
DIRNAME is the directory.
Prefix arg VERBOSE:
  With no prefix arg (nil), checked-out files are listed.
  With a positive prefix arg, registered files are listed.
  With a negative prefix arg, unregistered files are listed.
Optional arg OTHER-WINDOW-P:
  natnump means use another frame,
  otherwise non-nil means use another window.
Unlike `\\[dired]', shell wildcards (e.g. /xxx/*.c) are not allowed."
  (interactive
   (let ((verb (cond ((and current-prefix-arg
                           (natnump (prefix-numeric-value current-prefix-arg)))
                      'Registered)
                     (current-prefix-arg 'Unregistered)
                     ('Checked-out))))
     (list
      (read-file-name (format "%s files here and below: " verb)
                      default-directory default-directory)
      verb)))
  (require 'dired)
  ;; (require 'dired-aux) ????? NEEDED?
  (setq dirname (expand-file-name dirname))
  ;; Force a trailing slash.
  (if (not (eq (elt dirname (1- (length dirname))) ?/))
      (setq dirname (concat dirname "/")))
  (let ((dl (length dirname))
        (filelist nil) (statelist nil)
        (old-dir default-directory)
        dired-buf)
    (vc-file-tree-walk;; This fn doesn't allow for shell wildcards.
     dirname
     (function
      (lambda (f)
        (case verbose
          (Registered
           (when (vc-registered f)
             (let ((state (vc-dired-state-info f)))
               (setq filelist (cons (substring f dl) filelist))
               (setq statelist (cons state statelist)))))
          (Unregistered
           (unless (vc-registered f)
             (setq filelist (cons (substring f dl) filelist))))
          (Checked-out
           (when (vc-registered f)
             (let ((state (vc-dired-state-info f)))
               (when state
                 (setq filelist (cons (substring f dl) filelist))
                 (setq statelist (cons state statelist))))))))))
    (if (eq 0 (length filelist))
        (error "No files are currently %s under %s"
               (downcase (format "%s" verbose))
               default-directory)
      (save-window-excursion
        (save-excursion
          ;; This uses a semi-documented feature of dired; giving a switch
          ;; argument forces the buffer to refresh each time.
          (setq dired-buf
                (dired-internal-noselect
                 (cons dirname (nreverse filelist))
                 dired-listing-switches
                 'vc-dired-mode))))
      (if other-window-p
          (if (natnump other-window-p)
              (switch-to-buffer-other-frame dired-buf)
            (switch-to-buffer-other-window dired-buf))
        (switch-to-buffer dired-buf))
      (setq mode-name (format "Dired VC:%s" verbose))
      ;; Make a few modifications to the header
      (setq buffer-read-only nil)
      (goto-char (point-min))
      (forward-line 1);; Skip header line
      (let ((start (point)));; Erase (but don't remove) the
        (end-of-line);; "wildcard" line.
        (delete-region start (point)))
      (beginning-of-line)
      (setq vc-last-dired-option verbose) ; Remember for `vc-dired-update'.
      (message "Formatting (%s files)..."
               (setq verbose (downcase (format "%s" verbose))))
      (mapcar;; Plug the version information into the individual lines
       (function
        (lambda (x)
          (forward-char 2);; Skip dired's mark area
          (vc-dired-reformat-line x)
          (forward-line 1)))
       (nreverse statelist))
      ;; Highlight lines of file names for mouse selection.
      (dired-insert-set-properties (point-min) (point-max))
      (when (fboundp 'fit-frame) (fit-frame))
      (setq buffer-read-only t)
      (goto-char (point-min))
      (message "Formatting (%s files)...done" verbose)
      (dired-next-line 2))))


;;;###autoload
(defun vc-ediff (file &optional startup-hooks)
  "Compare file versions using `ediff'.
The FILE is:
  That visited by the current buffer, if any.
  That of the current line, if in a Dired buffer.
Otherwise you are prompted for the name of a registered file.
Uses `vc.el' or `rcs.el' depending on `ediff-version-control-package'.

This can be used interactively to replace `ediff-revision'.
It differs in:
  1. FILE arg is not optional and there is no STARTUP-HOOKS arg.
  2. FILE is determined interactively.
  3. FILE is displayed in another window, if called from Dired."
  (interactive
   (let ((file1 (buffer-file-name)))
     (list (or (and file1 (vc-name file1) file1) ; Buffer's file.
               (and (or vc-dired-mode (eq 'dired-mode major-mode))
                    (dired-get-filename)) ; Dired cursor's file.
               (and (setq file1 (read-file-name "Compare versions of file: "
                                                nil nil 'match-and-confirm))
                    (if (vc-name file1)
                        file1           ; Input file name.
                      (error "`%s' is not a registered file" file1)))))))
  (require 'ediff) ;; ediff-load-version-control, ediff-version-control-package
  (let ((rev1
         (read-string
          (format
           "Compare 2 versions of `%s'.  Revision 1 (default: last check-in): "
           (file-name-nondirectory file))))
        (rev2
         (read-string "Revision 2 (default: current file): ")))
    (when (and (or (not (buffer-file-name)) ; dired buffer
                   (not (string= file (buffer-file-name)))) ; diff input file
               (string= "" rev1))       ; current
      (find-file-other-window file))
    (ediff-load-version-control)        ; Defined in `ediff.el'
    (funcall (intern (format "ediff-%S-internal"
                             ediff-version-control-package)) ; `ediff-init.el'.
             rev1 rev2 startup-hooks)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; vc+.el ends here
