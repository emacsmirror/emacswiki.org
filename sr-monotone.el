;;;
;; File:    sr-monotone.el
;; Date:    2006-12-04
;; Author:  Sebastian Rose <sebastian_rose@gmx.de>
;; License: GPL
;;
;; Copyright (c) 2006 - 2007 Sebastian Rose <sebastian_rose gmx de>
;;
;;
;; Provides to funktions:
;;    * mtn-get-parent-directory (child)
;;      returns the parent directory or nil, if there is no parent.
;;      I wrote it, since I could not find a function like this.
;;    * mtn-add-change-log-entry ()
;;      looks for _MTN directory and changes the behaviour of
;;      add-change-log-entry to use /PATH/TO/_MTN as directory
;;      and 'log' as the default filename for the logfile.
;;
;;
;;
;; Usage:
;;
;;                    THE ONE WAY
;; In your .emacs put this line:
;; (require 'sr-monotone)
;; ... and place this file somewhere in your load-path.
;;
;;
;;                    THE OTHER WAY
;; Copy the two functions here and paste them in your .emacs.
;;
;;
;; Bind any key you like to mtn-add-change-log-entry.
;; (global-set-key [(f8)] 'mtn-add-change-log-entry)
;;
;; From now on only use mtn-add-change-log-entry. It detects automagically the
;; right way to call add-change-log-entry.
(provide 'sr-monotone)


(defun mtn-get-parent-directory (child)
  "Retruns the parent directory or nil, if there is no parent.
Parent has always a trailing slash, or what ever is your systems
file separtor.
Improvements and suggestions to sebastian_rose gmx de."
  (if (file-regular-p child)
      (file-name-as-directory (file-name-directory child))
                                        ; this is else:
    (let ((child (file-name-as-directory child)))
      (let ((dir (file-name-as-directory (expand-file-name child)))
            (parent (file-truename
                     (file-name-as-directory (expand-file-name (concat child ".."))))))
        (if (string= dir parent)
            nil
          parent)))))



(defun mtn-add-change-log-entry()
  "Searches in buffers default-directory and above for a directory
named '_MTN'. If it exists, monotone-add-change-log-entry calls
add-change-log-entry interactively with apropriate arguments.
Otherwise interactively calls add-change-log-entry the normal way.
So one can just bind this function to the keys, that call
add-change-log-entry now, and it will work just fine.

Improvements and suggestions to sebastian_rose gmx de."
  (interactive)
  (let ((filename buffer-file-name)
        (is-mtn-dir nil)
        (original-default-directory default-directory)
        (original-change-log-filename nil))
    (if (boundp 'change-log-default-name)
        (setq original-change-log-filename change-log-default-name))
    (unwind-protect
        (progn
          (if filename
              (progn
                (catch 'done
                  (while (setq filename (mtn-get-parent-directory filename))
                    (if (file-directory-p (concat filename "_MTN"))
                        (progn
                          (setq change-log-default-name "log")
                          (setq is-mtn-dir (concat filename "_MTN"))
                          (throw 'done 'is-mtn-dir)))))
                (if is-mtn-dir
                    (let ((default-directory is-mtn-dir))
                      (call-interactively 'add-change-log-entry))
                  (call-interactively 'add-change-log-entry))
                )))
      (setq default-directory original-default-directory)
      (setq change-log-default-name original-change-log-filename))))




;; This function is the one you need, to make add-change-log-entry
;; print the path of the file you're writing the change log entry for
;; relative to the root of your project directory.
;; If change-log-default-name is "log", this function assumes the
;; file is part of a monotone project and return the files path
;; relative to "/_MTN/..", just like monotone commands would do.
;; Otherwise returns the path of the file relative to the directory,
;; where the log file lives in.
;;
;; No leading slash prepended.
;;
;; To use this function type
;;            M-x customize-variable RET add-log-file-name-function RET
;; and make it point to mtn-add-log-file-name.
;; Unfortunately this function relies on log-file, which is an
;; undocumented variable in add-log.el, actually a local variable
;; in function
;;      add-log-file-name (buffer-file log-file).
;; But I couldn't find an other way, to get the logfile, the user has
;; choosen at this point.
;;
(defun mtn-add-log-file-name(original-name)
  "Return the filename printed in _MTN/log (or ChangeLog) relative to
the projects root. That is the driectory the file ChangeLog lives in,
if not a monotone project, _MTN/../ otherwise.

Improvements and suggestions to sebastian_rose gmx de."

  (let ((directory (mtn-get-parent-directory log-file))
        (file (file-truename original-name)))

    (if (string= change-log-default-name "log")
        ;; monotone
        (let ((directory  (mtn-get-parent-directory directory)))
          (file-relative-name file directory))
      ;; else no monotone:
      (file-relative-name file directory))))
