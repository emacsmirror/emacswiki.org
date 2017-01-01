;;; savehist-20+.el --- Save minibuffer history.
;; Author: Hrvoje Niksic <hniksic@xemacs.org>
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 2007-2017, Drew Adams
;; Copyright (C) 1997, 2005, 2006, 2007  Free Software Foundation, Inc.

;; Keywords: minibuffer history
;; Version: 24

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is essentially vanilla Emacs `savehist.el', modified to work
;; also with versions of GNU Emacs prior to Emacs 22.  (You can of
;; course use it with Emacs 22 and later also.)
;;
;; Some other changes were also made, such as removing text properties
;; from history elements.  All changes are marked with "DADAMS".
;; - Drew Adams

;; Many editors (e.g. Vim) have the feature of saving minibuffer
;; history to an external file after exit.  This package provides the
;; same feature in Emacs.  When set up, it saves recorded minibuffer
;; histories to a file (`~/.emacs-history' by default).  Additional
;; variables may be specified by customizing
;; `savehist-additional-variables'.

;; To use savehist, turn on savehist-mode by putting the following in
;; `~/.emacs':
;;
;;     (savehist-mode 1)
;;
;; or with customize: `M-x customize-option RET savehist-mode RET'.
;;
;; You can also explicitly save history with `M-x savehist-save' and
;; load it by loading the `savehist-file' with `M-x load-file'.

;; If you are using a version of Emacs that does not ship with this
;; package, be sure to have `savehist.el' in a directory that is in
;; your load-path, and to byte-compile it.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2016/12/10 dadams
;;     Added WHEN arg to make-obsolete (required, starting with Emacs 23).
;; 2014/02/21 dadams
;;     savehist-autosave-interval: Correct :type per Emacs 24 (can be nil).
;;     savehist-mode: Treat ARG like define-minor-mode would (e.g. for Lisp).
;;     savehist-save: Move prin1 inside condition-case, like vanilla Emacs 24.
;;     savehist-printable: Just use or, not cond.
;; 2013/07/28 dadams
;;     savehist-printable: Do not allow string if it has text properties.
;; 2011/01/04 dadams
;;     Added autoload cookies for defgroup, defcustom, and commands.
;; 2010/04/27 dadams
;;     savehist-save: Unpropertize history elements.
;; 2007/12/08 dadams
;;     savehist-save, savehist-printable: Updated wrt CVS of 2007-11-28.
;;     savehist-coding-system: Use emacs-mule-unix for all Emacs versions.
;; 2007/09/09 dadams
;;     savehist-file: Use user-emacs-directory (for Emacs 22+).
;;     savehist-save: Use Davis Herring's savehist-prin1-readable, after fixing.
;;     Removed: savehist-printable.
;;     Added: savehist-prin1-readable.
;; 2007/07/22 dadams
;;     savehist-save: Ensure printable, if Emacs 20 (side-steps bug).
;; 2007/07/11 dadams
;;     Updated wrt latest version, in Emacs 22.1.
;; 2005/12/01 dadams
;;     savehist-autosave: Wrapped in condition-case.
;; 2005/10/18 dadams
;;     savehist-save: no checksum support if `md5' not defined (e.g. Emacs 20).
;; 2005/10/15 dadams
;;     savehist-modes Changed value from octal #o600 to decimal 384.
;;     savehist-coding-system: Change to emacs-mule (for Emacs 20).
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'custom)

(eval-when-compile (require 'cl)) ;; loop, ignore-errors, dolist, pop, push - and XEmacs

;; User variables

;;;###autoload
(defgroup savehist nil
  "Save minibuffer history."
  :version "22.1" :group 'minibuffer)

;;;###autoload
(defcustom savehist-mode nil
  "*Mode for automatic saving of minibuffer history.
Set this by calling function `savehist-mode' or using the Customize
interface."
  :set (lambda (symbol value) (savehist-mode (or value  0)))
  :initialize 'custom-initialize-default
  :require 'savehist :type 'boolean :group 'savehist)

;;;###autoload
(defcustom savehist-save-minibuffer-history t
  "*If non-nil, save all recorded minibuffer histories.
If you want to save only specific histories, use `savehist-save-hook' to
modify the value of `savehist-minibuffer-history-variables'."
  :type 'boolean :group 'savehist)

;;;###autoload
(defcustom savehist-additional-variables ()
  "*List of additional variables to save.
Each element is a symbol whose value is persisted across Emacs
sessions that use `savehist'.  The contents of variables should be
printable with the Lisp printer.  You don't need to add minibuffer
history variables to this list, all minibuffer histories will be saved
automatically as long as `savehist-save-minibuffer-history' is
non-nil.

User options should be saved with the customize interface.  This
list is useful for saving automatically updated variables that are not
minibuffer histories, such as `compile-command' or `kill-ring'."
  :type '(repeat variable) :group 'savehist)

;;;###autoload
(defcustom savehist-ignored-variables nil ;; '(command-history)
  "*List of additional variables not to save."
  :type '(repeat variable) :group 'savehist)

;;;###autoload
(defcustom savehist-file
  (cond ((file-exists-p "~/.emacs-history") "~/.emacs-history") ; Backward compatibility
        ((and (not (featurep 'xemacs))
              (file-directory-p (if (boundp 'user-emacs-directory)
                                    user-emacs-directory
                                  "~/.emacs.d/")))
         (concat (if (boundp 'user-emacs-directory) user-emacs-directory "~/.emacs.d/")
                 "history"))
        ((and (featurep 'xemacs)  (file-directory-p "~/.xemacs/")) "~/.xemacs/history")
        ;; For users without `~/.emacs.d/' or `~/.xemacs/'.
        (t "~/.emacs-history"))
  "*File name where minibuffer history is saved to and loaded from.
The minibuffer history is a series of Lisp expressions loaded
automatically when `savehist-mode' is turned on.  See `savehist-mode'
for more details.

If you want your minibuffer history shared between Emacs and XEmacs,
customize this value and make sure that `savehist-coding-system' is
set to a coding system that exists in both emacsen."
  :type 'file :group 'savehist)

;; DADAMS, 2005-10-15: changed to decimal 384.
;;;###autoload
(defcustom savehist-file-modes 384      ; Octal: #o600
  "*Default permissions of the history file.
This is decimal, not octal.  The default is 384 (0600 in octal).
Set to nil to use the default permissions that Emacs uses, typically
mandated by umask.  The default is a bit more restrictive to protect
the user's privacy."
  :type 'integer :group 'savehist)

;;;###autoload
(defcustom savehist-autosave-interval (* 5 60)
  "*The interval between autosaves of minibuffer history.
If set to nil, disables timer-based autosaving."
  :type '(choice (const :tag "Disabled" nil)  (integer :tag "Seconds"))
  :group 'savehist)

;;;###autoload
(defcustom savehist-mode-hook nil
  "*Hook called when `savehist-mode' is enabled."
  :type 'hook :group 'savehist)

;;;###autoload
(defcustom savehist-save-hook nil
  "*Hook called by `savehist-save' before saving the variables.
You can use this hook to influence choice and content of variables to
save."
  :type 'hook :group 'savehist)

;;; DADAMS, 2007-12-08: changed to `emacs-mule-unix' for all versions.  There
;;;   are problems with `utf-8' even for Emacs 22, though probably `utf-8-unix'
;;;   would be OK. See emacs-devel@gnu.org, thread "saveplace: don't ask for
;;;   coding system", 2007-11-28 to 2007-12-05.
;;; DADAMS, 2005-10-15: changed to `emacs-mule' (for Emacs 20).
;; This should be capable of representing characters used by Emacs.
;; We prefer UTF-8 over ISO 2022 because it is well-known outside
;; Mule.  XEmacs prior to 21.5 had UTF-8 provided by an external
;; package which may not be loaded, which is why we check for version.
;;; (defvar savehist-coding-system
;;;   (if (and (featurep 'xemacs)
;;;            (<= emacs-major-version 21)
;;;            (< emacs-minor-version 5))
;;;       'iso-2022-8                       ; XEmacs
;;;     (if (coding-system-p 'utf-8)
;;;         'utf-8                          ; Emacs 22
;;;       'emacs-mule))                     ; Emacs 20
(defvar savehist-coding-system
  (if (and (featurep 'xemacs)  (<= emacs-major-version 21)  (< emacs-minor-version 5))
      'iso-2022-8                       ; XEmacs
    'emacs-mule-unix)                   ; Emacs
  "The coding system savehist uses for saving the minibuffer history.
Changing this value while Emacs is running is supported, but considered
unwise, unless you know what you are doing.")


;; Internal variables.

(defvar savehist-timer nil)

(defvar savehist-last-checksum nil)

(defvar savehist-minibuffer-history-variables nil
  "List of minibuffer histories.
The contents of this variable is built while Emacs is running, and saved
along with minibuffer history.  You can change its value off
`savehist-save-hook' to influence which variables are saved.")

(defconst savehist-no-conversion (if (featurep 'xemacs) 'binary 'no-conversion)
  "Coding system without any conversion.
This is used for calculating an internal checksum.  Should be as fast
as possible, ideally simply exposing the internal representation of
buffer text.")

(defvar savehist-loaded nil
  "Whether the history has already been loaded.
This prevents toggling `savehist-mode' from destroying existing
minibuffer history.")

(when (featurep 'xemacs)
  ;; Must declare this for XEmacs, which has no built-in minibuffer history truncation.
  (defvar history-length 100))
 
;; Functions.

;;;###autoload
(defun savehist-mode (arg)
  "Toggle saving minibuffer history (Savehist mode).
When Savehist mode is enabled, minibuffer history is saved
periodically and when exiting Emacs.  When Savehist mode enabled for
the first time in an Emacs session, it loads the previous minibuffer
history from `savehist-file'.

Other Emacs variables may also be saved by Savehist mode, depending on
the values of options `savehist-additional-variables' and
`savehist-ignored-variables'.

With a prefix argument ARG, enable Savehist mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

This mode should normally be turned on from your Emacs init file.
Calling it at any other time replaces your current minibuffer
histories, which is probably undesirable."
  (interactive "P")
  (setq savehist-mode  (or (and (not (interactive-p))  (not arg)) ; Lisp, no ARG.
                           (if (not arg)
                               (not savehist-mode)
                             (> (prefix-numeric-value arg) 0))))
  (if (not savehist-mode)
      (savehist-uninstall)
    (when (and (not savehist-loaded)  (file-exists-p savehist-file))
      (condition-case errvar
          (progn
            ;; Do not set coding-system-for-read -- we rely on the coding cookie to
            ;; convey that information.  That way, if the user changes the value of
            ;; `savehist-coding-system', we can still correctly load the old file.
            (load savehist-file nil (not (interactive-p)))
            (setq savehist-loaded  t))
        (error
         ;; Do not install the mode if reading failed.  Doing so would effectively
         ;; destroy the user's data at the next save.
         (setq savehist-mode  nil)
         (savehist-uninstall)
         (signal (car errvar) (cdr errvar)))))
    (savehist-install)
    (run-hooks 'savehist-mode-hook))
  savehist-mode)                        ; Return the new setting.

;;; DADAMS, 2007-07-11: Protect by fboundp - not defined for Emacs 20.
(when (fboundp 'add-minor-mode) (add-minor-mode 'savehist-mode ""))

(defun savehist-load ()
  "Load the variables stored in `savehist-file' and turn on `savehist-mode'.
If `savehist-file' is in the old format that does not record the value
of `savehist-minibuffer-history-variables', that value is removed
from the contents of the file.

OBSOLETE function provided for transition from old versions of Savehist.
Do NOT call this from new code.  Use (savehist-mode 1) instead."
  (savehist-mode 1)
  ;; Old versions of savehist distributed with XEmacs didn't save
  ;; `savehist-minibuffer-history-variables'.  If that variable is nil
  ;; after loading the file, try to intuit the intended value.
  (unless savehist-minibuffer-history-variables
    (setq savehist-minibuffer-history-variables
          (with-temp-buffer
            (ignore-errors (insert-file-contents savehist-file))
            (let ((vars  ())
                  form)
              (while (setq form  (condition-case nil
                                     (read (current-buffer)) (error nil)))
                ;; Each form read is of the form (setq VAR  VALUE).
                ;; Collect VAR, i.e. (nth form 1).
                (push (nth 1 form) vars))
              vars)))))

(if (< emacs-major-version 22)
    (make-obsolete 'savehist-load 'savehist-mode)
  (make-obsolete 'savehist-load 'savehist-mode "WHEN?"))

(defun savehist-install ()
  "Hook savehist into Emacs.
Normally invoked by calling `savehist-mode' to set the minor mode.
Installs `savehist-autosave' in `kill-emacs-hook' and on a timer.
To undo this, call `savehist-uninstall'."
  (add-hook 'minibuffer-setup-hook 'savehist-minibuffer-hook)
  (add-hook 'kill-emacs-hook 'savehist-autosave)
  ;; Install an invocation of `savehist-autosave' on a timer.  This should not cause
  ;; noticeable delays for users.
  (when (and savehist-autosave-interval  (null savehist-timer))
    (setq savehist-timer
          (if (featurep 'xemacs)
              (start-itimer "savehist" 'savehist-autosave savehist-autosave-interval
                            savehist-autosave-interval)
            (run-with-timer savehist-autosave-interval savehist-autosave-interval
                            'savehist-autosave)))))

(defun savehist-uninstall ()
  "Undo installing savehist.
Normally invoked by calling `savehist-mode' to unset the minor mode."
  (remove-hook 'minibuffer-setup-hook 'savehist-minibuffer-hook)
  (remove-hook 'kill-emacs-hook 'savehist-autosave)
  (when savehist-timer
    (if (featurep 'xemacs) (delete-itimer savehist-timer) (cancel-timer savehist-timer))
    (setq savehist-timer  nil)))

;; From XEmacs?
(defvar print-readably)
(defvar print-string-length)

;;;###autoload
(defun savehist-save (&optional auto-save)
  "Save the values of minibuffer history variables.
Unbound symbols referenced in `savehist-additional-variables' are ignored.
If AUTO-SAVE is non-nil, compare the saved contents to the one last saved,
 and don't save the buffer if they are the same."
  (interactive)
  (with-temp-buffer
    (insert (format ";; -*- mode: emacs-lisp; coding: %s -*-\n" savehist-coding-system)
            ";; Minibuffer history file, automatically generated by `savehist'.\n\n")
    (run-hooks 'savehist-save-hook)
    (let ((print-length         nil)
	  (print-string-length  nil)
	  (print-level          nil)
	  (print-readably       t)
	  (print-quoted         t))

;;;       ;; During 24.3 development, `read-passwd' had a bug which resulted in
;;;       ;; passwords being saved by Savehist.  Trim them, retroactively.
;;;       ;; This code not needed after (or before) the 24.3 release.
;;;       (dolist (sym  savehist-minibuffer-history-variables)
;;;         (when (and (symbolp sym)  (equal (symbol-name sym) "forget-history"))
;;;           (setq savehist-minibuffer-history-variables
;;;                 (delq sym savehist-minibuffer-history-variables))))

      ;; Save minibuffer histories, along with `savehist-minibuffer-history-variables'.
      (when savehist-save-minibuffer-history
	(prin1 `(setq savehist-minibuffer-history-variables
                 ',savehist-minibuffer-history-variables)
	       (current-buffer))
	(insert ?\n)
	(dolist (symbol  savehist-minibuffer-history-variables)
	  (when (and (boundp symbol)  (not (memq symbol savehist-ignored-variables)))
	    (let ((value  (savehist-trim-history (symbol-value symbol)))
		  excess-space)
	      (when value		; Do not save empty histories.
		(insert "(setq ")
		(prin1 symbol (current-buffer))
		(insert " '(")
		;; We will print an extra space before the first element.
		;; Record where that is.
		(setq excess-space  (point))
		;; Print elements of VALUE, one by one.
		(dolist (elt  value)
                  ;; DADAMS 2010-04-27: Unpropertize element.
                  (set-text-properties 0 (length elt) nil elt)
		  (let ((start  (point)))
		    (insert " ")
		    (condition-case nil ; Try to print and then to read an element.
			(progn (prin1 elt (current-buffer))
                               (save-excursion (goto-char start)
                                               (read (current-buffer))))
		      (error
		       ;; If writing or reading raised an error, comment out line.
		       (goto-char start)
		       (insert "\n")
		       (while (not (eobp))
			 (insert ";;; ")
			 (forward-line 1))
		       (insert "\n")))
		    (goto-char (point-max))))
		;; Delete the extra space before the first element.
		(save-excursion
		  (goto-char excess-space)
                  ;; DADAMS: Changed ?\s to ?\ .
		  (when (eq (following-char) ?\ ) (delete-region (point) (1+ (point)))))
		(insert "))\n"))))))
      ;; Save the additional variables.
      (dolist (symbol  savehist-additional-variables)
        (when (boundp symbol)
          (let ((value  (symbol-value symbol)))
	    (when (savehist-printable value)
	      (prin1 `(setq ,symbol  ',value) (current-buffer))
	      (insert ?\n))))))
    ;; DADAMS, 2005-10-18: no checksum support if `md5' undefined (Emacs 20).
    ;;
    ;; If autosaving, avoid writing if nothing has changed since the last write.
    (let ((checksum  (and (fboundp 'md5)
                          (md5 (current-buffer) nil nil savehist-no-conversion))))
      (unless (and auto-save  checksum  (equal checksum savehist-last-checksum))
        ;; Set `file-precious-flag' when saving the buffer, because we do not want a
        ;; half-finished write ruining the entire history.  This is run from a timer
        ;; and from `kill-emacs-hook', and multiple Emacs instances could write to this
        ;; file at the same time.
        (let ((file-precious-flag       t)
              (coding-system-for-write  savehist-coding-system))
          (write-region (point-min) (point-max) savehist-file nil
                        (and (not (interactive-p))  'quiet)))
        (when savehist-file-modes (set-file-modes savehist-file savehist-file-modes))
        (setq savehist-last-checksum  checksum)))))

;; DADAMS, 2005-12-01: Wrapped in `condition-case'.
(defun savehist-autosave ()
  "Save minibuffer history if it has been modified since the last save.
Do nothing if `savehist-mode' is not enabled."
  (condition-case nil (when savehist-mode (savehist-save t)) (error nil)))

(defun savehist-trim-history (value)
  "Retain only the first `history-length' items in VALUE.
Only used under XEmacs, which doesn't (yet) implement automatic
trimming of history lists to `history-length' items."
  (if (and (featurep 'xemacs)
           (natnump history-length)
           (> (length value) history-length))
      ;; Equivalent to `(subseq value 0 history-length)', but does not need `cl-extra'
      ;; at run-time.
      (loop repeat history-length collect (pop value))
    value))

(defun savehist-printable (value)
  "Return non-nil if VALUE is printable."
  (or (numberp value)
      (symbolp value)
      (and (stringp value)              ; String with no text properties.
           (if (fboundp 'equal-including-properties) ; Emacs 22+.
               (equal-including-properties value (substring-no-properties value))
             (and (null (text-properties-at 0 value))
                  (= 0 (next-property-change 0 value)))))
      (with-temp-buffer
        (condition-case nil
            (let ((print-readably  t)
                  (print-level     nil))
              (prin1 value (current-buffer)) ; Print and try to read back.
              (read (point-min-marker))
              t)
          (error nil)))))               ; Could not print and read back.

;; ;; DADAMS, 2007-07-22: Emacs 20 has a bug that puts
;; ;;   `M-x cancel-debug-on-entry RET' into `command-history' as
;; ;;   (cancel-debug-on-entry ') - note the quote mark before the paren.
;; (defun savehist-prin1-readable (value)
;;   "Print VALUE in the current buffer, but only if it's also readable.
;; Return non-nil if it was printed."
;;   (let ((opoint      (copy-marker (point)))
;;         (opoint-cpy  (copy-marker (point))))
;;     (condition-case nil
;;         (progn (prin1 value (current-buffer))
;;                (read opoint)
;;                t)
;;       (error (delete-region opoint-cpy (point))
;;              nil))))

(defun savehist-minibuffer-hook ()
  (unless (or (eq minibuffer-history-variable t)
              ;; XEmacs sets `minibuffer-history-variable' to t to mean "no history
              ;; is being recorded".
              (memq minibuffer-history-variable savehist-ignored-variables))
    (add-to-list 'savehist-minibuffer-history-variables minibuffer-history-variable)))

(provide 'savehist-20+)
 
;; arch-tag: b3ce47f4-c5ad-4ebc-ad02-73aba705cf9f
;;; savehist-20+.el ends here

