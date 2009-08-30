;;; ctypes.el --- Enhanced Font lock support for custom defined types.

;; Copyright (C) 1997, 1999 Anders Lindgren.

;; Author: Anders Lindgren <andersl@andersl.com>
;; Maintainer: Anders Lindgren <andersl@andersl.com>
;; Version: 1.3.1
;; Created: 1997-03-16
;; Date: 1999-06-23

;; CTypes is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; CTypes is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;{{{ Documentation

;; Background:
;;
;; As most Emacs users know, Emacs can fontify source code buffers
;; using the `font-lock' package.  Most of the time it does a really
;; good job.  Unfortunately, the syntax of one of the most widely
;; spread languages, C, makes it difficult to fontify variable
;; declarations.  For example, what does the following line mean:
;;
;;     hello(foo * bar);
;;
;; 1) A new function `hello' that takes one argument `bar' that is a
;;    pointer to a `foo', or;
;;
;; 2) call the function `hello' with the result of `foo' multiplied
;;    by `bar'.
;;
;; To answer the question correctly you must know whether `foo' is a
;; type or not.  Unfortunately, font-lock has no way of knowing this.

;; This package:
;;
;; This package can search through source files hunting down typedefs.
;; When found, font-lock is informed and your source code will be even
;; more beautifully colored than before.
;;
;; Each major mode has it's own set of types.  It is possible for one
;; major mode to inherit the types of another mode.
;;
;; Currently, this package can parse C and C++ files.  (However, since
;; I do not use C++, the probability is high (about 12, on a scale
;; from 1 to 12) that I've missed something).  By default C++ inherits
;; the types defined for C mode.

;; Installation:
;;
;; Place this file in any directory in the emacs load path
;; and add the following line to your init file:
;;  (require 'ctypes)
;;
;; Or, if you should prefer to load ctypes only when needed:
;;
;; (defun my-activate-ctypes () (require 'ctypes))
;; (add-hook 'c-mode-hook 'my-activate-ctypes)
;; (add-hook 'c++-mode-hook 'my-activate-ctypes)
;;
;; Of course, you must also activate font-lock.  I also recomend using
;; lazy-lock since adding types requires refontification of all
;; buffers.  (Should you use many small buffers, consider lowering
;; `lazy-lock-minimum-size' aswell.)

;; Defining types:
;;
;; The following commands are available to define and remove types:
;;
;; `ctypes-define-type'            Add a type.
;; `ctypes-define-type-in-mode'    Add a type to another major mode.
;; `ctypes-buffer'                 Scan a buffer for types.
;; `ctypes-all-buffer'             Scan all buffer for types.
;; `ctypes-tags'                   Search through all files in a TAGS table.
;; `ctypes-dir'                    Search a directory hierarchy for files.
;; `ctypes-file'                   Search in a file for types.
;; `ctypes-remove-type'            Remove one type.
;; `ctypes-remove-type-in-mode'    Remove one type in another mode.
;; `ctypes-clear-types'            Forget all types.
;; `ctypes-clear-types-all-modes'  Forget all types in all major modes.

;; Edit types:
;;
;; If you would like to view or change the types found you can use the
;; function `ctypes-edit'.  When done press C-c C-c.  Should you like
;; do discard your changes just kill the buffer with C-x k.
;;
;; To edit the types for another major mode use the command
;; `ctypes-edit-types-for-mode'.

;; Saving types:
;;
;; The commands `ctypes-write-file' and `ctypes-read-file' can be used
;; to save your hard-earned collection of types to a file and to
;; retrieve it later.
;;
;; The default file name is stored in the variable `ctypes-file-name'.
;;
;; Note that only one collection of types are managed.  Should you
;; prefer to keep one type file per project, remember to clear the set
;; of known types (using the command `ctypes-clear-types-all-modes')
;; before each new set is generated.

;; At Load:
;;
;; It is possible to automatically add new types, or read specific
;; type files, when Emacs opens a file.
;;
;; By adding a "Local Variables" section to the end of the file
;; containing the variables `ctypes-add-types-at-load' and/or
;; `ctypes-read-files-at-load' this can be accomplished.
;;
;; For example:
;;
;; /*
;;  * Local Variables:
;;  * ctypes-add-types-at-load: ("MyType" "YourType")
;;  * ctypes-read-files-at-load: (".ctypes")
;;  * End:
;;  */

;; The `Auto Parse' mode:
;;
;; This package can automatically search for new types in all visited
;; files.  Activate the minor mode `ctypes-auto-parse-mode' to enable
;; this feature.
;;
;; Add the following line to your startup file to automatically
;; scan all visited files:
;;  (ctypes-auto-parse-mode 1)

;; Example 1:
;;
;; The following setup is for the really lazy person.  The keywords
;; collected during one session will be kept for the next, and all
;; visited files will be parsed in the boldly search for new types.
;; I would recomend using this approach only when you are keeping all
;; your types in one file.
;;
;; (require 'ctypes)
;; (setq ctypes-write-types-at-exit t)
;; (ctypes-read-file nil nil t t)
;; (ctypes-auto-parse-mode 1)

;; Example 2:
;;
;; In this example, ctypes will not be not loaded until either c-mode
;; or c++-mode is activated.  When loaded, ctypes will read the type
;; file "~/.ctypes_std_c" (containing, for example, all types defined
;; in the standard C header files).
;;
;; (defun my-c-mode-hook ()
;;   (require 'ctypes)
;;   (turn-on-font-lock))
;; (add-hook 'c-mode-hook 'my-c-mode-hook)
;; (add-hook 'c++-mode-hook 'my-c-mode-hook)
;;
;; (defun my-ctypes-load-hook ()
;;   (ctypes-read-file "~/.ctypes_std_c" nil t t))
;; (add-hook 'ctypes-load-hook 'my-ctypes-load-hook)

;; Home Page:
;;
;; You can always find the latest version of this package on my Emacs
;; page:
;;
;;     http://www.andersl.com/emacs

;; Reporting bugs:
;;
;;     Out of the last ten bugs you found, how many did you report?
;;
;; When reporting a bug, please:
;;
;; * Send a mail the maintainer of the package, or to the author
;;   if no maintainer exists.
;; * Include the name of the package in the title of the mail, to
;;   simplify for the recipient.
;; * State exactly what you did, what happened, and what you expected
;;   to see when you found the bug.
;; * If possible, include an example that activates the bug.
;; * Should you speculate about the cause of the problem, please
;;   state explicitly that you are guessing.

;; CTypes, the true story:
;;
;; Well, brave reader, are you willing to learn what this package
;; really is capable of?
;;
;; Basically, it is a general purpose parsing package.  The default
;; settings just happened to specify a parser that looks for C
;; typedefs, and that the default action is to add the types found to
;; font-lock.
;;
;; Be redefining the variable `ctypes-mode-descriptor' you can change
;; the behavior totally.  For example, you can use it to search for
;; all occurrences of XX (replace XX with whatever you like) in all
;; files edited in major mode YY (ditto for YY) and to perform ZZ-top
;; whenever a new XX is found.  (However, it might be difficult for
;; Emacs to grow a beard).
;;
;; I will, however, in the document string, write "search for types"
;; when I really mean "Call the parser routine as specified by
;; `ctypes-mode-descriptor'".  Also, I write "Informing font-lock"
;; whenever I mean "Performing the default action as specified in
;; `ctypes-mode-descriptor'".

;; The future:
;;
;; Should this package be included in future versions of Emacs almost
;; all of the font-lock code could be removed.  Also there will be no
;; need to load font-lock to determine which version of
;; ctypes-mode-descriptor to use.

;;}}}

;;; Code:

;;{{{ Dependencies

;; The only reason to load font-lock is to determinate the font-lock
;; version we are using.

(require 'font-lock)

(eval-when-compile
  (require 'cl))

;;}}}
;;{{{ Variables

(defvar ctypes-file-name "~/.ctypes"
  "*Default name of file to read types from.

When `ctypes-read-file' and `ctypes-write-file' are called interactively
the directory part of the file name is ignored.")


(defvar ctypes-write-types-at-exit nil
  "*When non-nil types are saved to file when Emacs exits.

When this variable be 'ask, the user is prompted before the
types are saved.")


(defvar ctypes-mode-descriptor
  (if (boundp 'c-font-lock-extra-types)
      ;; A new version of font-lock is used.  (As of this writing,
      ;; it has not yet been released.)
      '((c-mode
         (parser ctypes-parse-buffer-c)
         (action ctypes-font-lock-set-extra-types
                 c-font-lock-extra-types))
        (c++-mode
         (inherit c-mode)
         (parser ctypes-parse-buffer-c++)
         (action ctypes-font-lock-set-extra-types
                 c++-font-lock-extra-types)))
    ;; The following can be used together with good old font-lock from
    ;; XEmacs and GNU Emacs up to, and including, 19.34.
    '((c-mode
       (parser ctypes-parse-buffer-c)
       (action ctypes-font-lock-add-keywords
               ((1 c-font-lock-keywords-2)
                (1 c-font-lock-keywords-3)
                (2 c-font-lock-keywords-3 t))))
      (c++-mode
       (inherit c-mode)
       (parser ctypes-parse-buffer-c++)
       (action ctypes-font-lock-add-keywords
               ((1 c++-font-lock-keywords-2)
                (1 c++-font-lock-keywords-3)
                (2 c++-font-lock-keywords-3 t))))))
  "*Describe parser, action, and inheritance structure of major mode.

This structure should be a list where each element should be on
the following form:
    (<major-mode>
     (inherit <other-major-mode>)
     (parser <parser function>)
     (action function [Optional extra arguments]))

The function specified in the `action' field is called with at least
one arguments, the major mode.  Should the function in the action
field be followed by anything it will be used as additional arguments
when the function is called.")


(defvar ctypes-dir-read-file nil
  "*Variable determinating which files `ctypes-dir' should read.

When search for types in a large number of files it is difficult
to determine which files to parse.  Should to few be opened, we
can miss some types.  The opposite, to open to many be opened,
the parse process could take much longer than needed.

The default behavior, when `ctypes-dir-read-file' is nil, is to look
at the extension of the files found.  Should it match a major mode in
`auto-mode-alist', and the major mode is in `ctypes-mode-descriptor'
we read the file.  Obviously, this approach is fast but it is possible
to miss files.

After the file has been read the real major mode is determined from
the content of the file.  This allows you to specify the real mode
using the -*- mode -*- construction.

Should this variable be t, all non-backup files are read.

Please see the variable `ctypes-dir-backup-files' for a description on
how backup files are treated.

To open only a few extra files, bind this variable to a regexp.

For example, when using the following setting `ctypes-dir' will
open all files ending in `.cplusplus'.

    (setq ctypes-dir-read-file \"\\\\.cplusplus\\\\'\")

However, the files would still need a -*- C++ -*- header line
to be parsed as C++ files.")


(defvar ctypes-dir-backup-files nil
  "*Non-nil means that `ctypes-dir' should parse backup files.")

(defvar ctypes-auto-parse-mode nil
  "Non-nil when the minor mode `ctypes-auto-parse-mode' is enabled.

When this mode is active the `ctypes' package will search for
types in all new buffers loaded.

To start the mode call the function `ctypes-auto-parse-mode', do not
set this variable explicitly.")


(defvar ctypes-auto-parse-mode-hook nil
  "*List of functions to run when `ctypes-auto-parse-mode' is activated.")

(defvar ctypes-load-hook nil
  "*List of functions to run when `ctypes' is loaded.")


(defvar ctypes-saved-p t
  "Nil when types not saved to file.")


(defvar ctypes-repetitive-type-regexp
  (concat "\\<\\(short\\|int\\|long\\|float\\|"
          "double\\|char\\|\\(un\\)?signed\\|const\\)\\>")
  "Regexp matching C types and modifiers that can be combined.

Example: `unsigned char'")


;; In some environments the $-sign can be part of C identifiers.
(defvar ctypes-identifier-regexp "[a-zA-Z_][a-zA-Z0-9_$]*"
  "Regexp matching C identifiers.")
;; I removed the :-sign, why was it added?  (Maybe the regexp should
;; match stuff not ending in colon.)


;; Useful during debug and development.
(defvar ctypes-parse-error nil
  "(File pos) of latest error, or nil.")

;;}}}
;;{{{ Commands

;;;###autoload
(defun ctypes-define-type (type &optional delay-action mode)
  "Add a new TYPE to current major mode and inform font-lock.

When preceded by C-u the display is not updated.

Return non-nil if the type was not known before."
  (interactive 
   (list
    (let* ((default (ctypes-get-type-under-point))
           (prompt (if default
                       (format "Type: (default %s) " default)
                     "Type: "))
           (spec (read-string prompt)))
      (if (equal spec "") default spec))
    prefix-arg))
  (if (equal type "")
      (error "Can't define \"\" as a type"))
  (or mode
      (setq mode major-mode))
  (and type 
       (> (length type) 0)
       (let ((added (ctypes-add-types mode (list type))))
         (ctypes-perform-action mode added delay-action)
         added)))


;; Designed for interactive use only.
;;;###autoload
(defun ctypes-define-type-in-mode (type &optional delay-action mode)
  "Add TYPE to major mode MODE and inform font-lock.

When preceded by C-u the display is not updated.

\(This function is designed for interactive use, please call
`ctypes-define-type' from Lisp programs.)"
  (interactive "sType: \nP\nsIn mode: ")
  (ctypes-define-type type delay-action (ctypes-string-to-mode mode)))


;;;###autoload
(defun ctypes-buffer (&optional buf delay-action mode)
  "Search for types in buffer, inform font-lock if any is found.

When preceded by C-u the action is not performed.

Return non-nil if new types are found."
  (interactive "bSearch for types in buffer: \nP")
  (save-excursion
    (if buf
        (set-buffer buf)
      (setq buf (current-buffer)))
    (or mode
        (setq mode major-mode)))
  (let ((added (ctypes-add-types mode (ctypes-parse-buffer buf mode))))
    (ctypes-perform-action mode added delay-action)
    added))


;;;###autoload
(defun ctypes-all-buffers (&optional delay-action)
  "Search for types in all buffers, inform font-lock about all discoveries.

When preceded by C-u the display is not updated.

Return non-nil if new types are found."
  (interactive "P")
  (save-excursion
    (let ((modes '())
          (added nil))
      (dolist (buf (buffer-list))
        (set-buffer buf)
        (if (assq major-mode ctypes-mode-descriptor)
            (if (not (ctypes-buffer nil t))
                ()
              (setq added t)
              (if (not (memq major-mode modes))
                  (setq modes (cons major-mode modes))))))
      (ctypes-perform-action modes added delay-action)
      added)))


;;;###autoload
(defun ctypes-tags (&optional delay-action)
  "Search for types in files in the visited TAGS table.
Should no tags table be visited, the user will be prompted for a new.

When preceded by C-u the display is not updated.

Return non-nil if new types are found."
  (interactive "P")
  (let ((modes (ctypes-tags-parse)))
    (ctypes-perform-action modes t delay-action)
    (not (null modes))))


;;;###autoload
(defun ctypes-dir (&optional dir delay-action)
  "Search for types in files in a directory hierarchy.

See variable `ctypes-dir-read-file' for a description of which files
are opened during scanning, and how you can change the behavior.

When preceded by C-u the display is not updated.

Return non-nil if new types are found."
  (interactive "DSearch in directory: \nP")
  (if (null dir)
      (setq dir default-directory))
  (let ((dirs (list dir))
        (modes '()))
    (while dirs
      (setq dir (car dirs))
      (setq dirs (cdr dirs))
      (dolist (file (directory-files dir t)) ; Files and dirs
        (cond
         ((file-accessible-directory-p file)
          (if (and (not (string= (file-name-nondirectory file) "."))
                   (not (string= (file-name-nondirectory file) "..")))
              (setq dirs (cons file dirs))))
         ((file-readable-p file)
          (if (or ctypes-dir-backup-files
                  (not (backup-file-name-p file)))
              (if (or (eq ctypes-dir-read-file t)
                      (and (stringp ctypes-dir-read-file)
                           (string-match ctypes-dir-read-file file))
                      (assq (ctypes-get-mode-from-file-name file)
                            ctypes-mode-descriptor))
                  (let ((mode (ctypes-file file t)))
                    (if mode
                        (setq modes (cons mode modes))))))))))
    (and modes
         (ctypes-perform-action modes t delay-action))))


;;;###autoload
(defun ctypes-file (file &optional delay-action)
  "Search for types in file FILE.
Should FILE not be loaded it is read into a temporary buffer.

Return mode of file, if new types was found."
  (interactive "fSearch in file: \nP")
  ;; (message "Scanning %s..." file)    ; Debug
  (let ((added nil)
        mode)
    (save-excursion
      (cond ((get-file-buffer file)
             (set-buffer (find-file-noselect file t))
             (setq mode major-mode)
             (setq added (ctypes-buffer nil t mode)))
            (t
             (set-buffer (get-buffer-create " *ctypes-file*"))
             (kill-all-local-variables)
             (erase-buffer)
             (insert-file-contents file nil)
             (let ((buffer-file-name file))
               (setq mode (ctypes-get-mode))
               (if mode
                   (setq added (ctypes-buffer nil delay-action mode))))
             (kill-buffer (current-buffer)))))
    ;; (message "Scanning %s...done" file)      ; Debug
    (if mode
        (ctypes-perform-action mode added delay-action))
    (and added mode)))


(defun ctypes-remove-type (type &optional delay-action mode)
  "Remove TYPE from the set of known types for major mode of current buffer.

When preceded by C-u the display is not updated.

Return non-nil if type is removed."
  (interactive 
   (list
    (let* ((default (ctypes-get-type-under-point))
           (prompt (if default
                       (format "Type: (default %s) " default)
                     "Type: "))
           (spec (read-string prompt)))
      (if (equal spec "") default spec))
    prefix-arg))
  (or mode
      (setq mode major-mode))
  (let ((removed (ctypes-delete-types mode (list type))))
    (ctypes-perform-action mode removed delay-action)
    removed))


;; Designed for interactive use only.
(defun ctypes-remove-type-in-mode (type &optional delay-action mode)
  "Remove TYPE from the set of known types for major mode MODE.

MODE can either be a symbol (e.g. c++-mode), or a string (e.g. \"C++\").

When preceded by C-u the display is not updated.

Return non-nil if type is removed."
  (interactive "sType: \nP\nsIn mode: ")
  (ctypes-remove-type type delay-action (ctypes-string-to-mode mode)))


(defun ctypes-clear-types (&optional delay-action)
  "Clear all known types for major mode of current buffer.

When preceded by C-u the display is not updated.

Return non-nil if any types actually were removed."
  (interactive "P")
  (setq ctypes-parse-error nil)         ; Debug
  (let ((removed (ctypes-set-types major-mode '())))
    (ctypes-perform-action major-mode removed delay-action)
    removed))


(defun ctypes-clear-types-all-modes (&optional delay-action)
  "Clear all types for all modes.

When preceded by C-u the display is not updated.

Return non-nil if any types actually were removed."
  (interactive "P")
  (setq ctypes-parse-error nil)         ; Debug
  (let ((modes '()))
    (dolist (desc ctypes-mode-descriptor)
      (if (ctypes-set-types (car desc) '())
          (setq modes (cons (car desc) modes))))
    (if modes
        (ctypes-perform-action modes t t))
    (or delay-action
        (ctypes-perform-delayed-action))
    (not (null modes))))


(defun ctypes-update ()
  "Make sure no delayed action is pending for types of major mode.

Since it can take some time to re-fontify all buffers after every
command it is possible to inhibit redisplay by preceding the command
by C-u.  This command can be used to refontify all buffers after a
number of such commands."
  (ctypes-perform-action major-mode nil t))


(defun ctypes-update-all-modes ()
  "Make sure no delayed action is pending for any major mode.

Since it can take some time to re-fontify all buffers after every
command it is possible to inhibit redisplay by preceding the command
by C-u.  This command can be used to refontify all buffers after a
number of such commands."
  (ctypes-perform-delayed-action))

;;}}}
;;{{{ Minor mode: ctypes-auto-parse-mode

;;;###autoload
(defun ctypes-auto-parse-mode (&optional arg)
  "Toggle CTypes auto parse mode; search all new buffers for types.
With arg, turn types Auto Mode on if and only if arg is positive.

This a global minor mode, it does not have a private keymap, nor does
it add itself to the mode line.

Place the following in your startup file to enable this feature in
future sessions:

    (require 'ctypes)
    (ctypes-auto-parse-mode 1)

When activated, the functions in the hook `ctypes-auto-parse-mode-hook'
is called with no args."
  (interactive "P")
  (setq ctypes-auto-parse-mode
        (if (null arg)
            (not ctypes-auto-parse-mode)
          (> (prefix-numeric-value arg) 0)))
  (if ctypes-auto-parse-mode
      (run-hooks 'ctypes-auto-parse-mode-hook)))

;;}}}
;;{{{ Find-file hook

(defvar ctypes-add-types-at-load '()
  "List of types to be added when file is opened.

This variable is designed to be used in a \"Local Variables\" section
at the end of source files.")


(defvar ctypes-read-files-at-load '()
  "CTypes files to be read when file is opened.

This variable could either be the name of a type file, or a list of
type files.

This variable is designed to be used in a \"Local Variables\" section
at the end of source files.  Should this variable not be defined in a
Local Variables section, the global value is used.  By setting this
varible to, for examle, \".ctypes\" Emacs will try to read a type file
named \".ctypes\" in every directory it opens files from.")


(defun ctypes-find-file-hook ()
  "Add types specified in file local variables.

This function is called every time a file is opened.  It looks at two
variables `ctypes-add-types-at-load' and `ctypes-read-files-at-load'.
They are designed to be added to the \"Local Variables:\" section at
the end of source files.  The idea is to automatically set or load
the types needed when a file is opened.

When `ctypes-auto-parse-mode' is active this function will parse
the content of the buffer looking for types."
  (let ((added nil))
    (if (and ctypes-add-types-at-load
             (ctypes-add-types major-mode ctypes-add-types-at-load))
        (setq added t))
    (dolist (file (or (and (stringp ctypes-read-files-at-load)
                           (list ctypes-read-files-at-load))
                      ctypes-read-files-at-load))
      (if (and (stringp file)
               (ctypes-read-file file nil t))
            (setq added t)))
    (if ctypes-auto-parse-mode
        (ctypes-buffer nil t))
    (ctypes-perform-action major-mode added nil)))

;;}}}
;;{{{ Read and write

;;;###autoload
(defun ctypes-read-file (&optional file delay-action no-error quietly)
  "Load types previously saved with `ctypes-write-file'.
The name of the file is given by the optional argument FILE.
Should no file name be given the value of the variable `ctypes-file-name'
is used.

Please note that the types read will be added to the current types.

When preceded by C-u the display is not updated.

The third argument, NO-ERROR, determines whether or not we should
raise an error if there should be any problem loading the file.

Should the fourth argument, QUIETLY, be non-nil no messages are
generated when the file is loaded.

Return non-nil if new types are found."
  (interactive
   (list
    (ctypes-interactive-read-file-name "Read types from file: ")
    current-prefix-arg))
  (setq file (ctypes-gen-file-name file))
  (let ((current-types-alist '()))
    (dolist (desc ctypes-mode-descriptor)
      (let ((mode (car desc)))
        (setq current-types-alist
              (cons (cons mode (ctypes-get-types mode))
                    current-types-alist))))
    (load file no-error quietly)
    (setq ctypes-saved-p t)
    ;; Add the original types, and update wherever needed.
    (let ((modes '()))          ; Updated modes
      (dolist (pair current-types-alist)
        (if (not (ctypes-subset
                  (ctypes-get-types (car pair))
                  (cdr pair)))
            ;; New types was defined for this mode.
            (setq modes (cons (car pair) modes)))
        (ctypes-add-types (car pair) (cdr pair)))
      (if modes
          (ctypes-perform-action modes t delay-action))
      modes)))


(defun ctypes-write-file (&optional file)
  "Write all types to a file.
The file is readable by the function `ctypes-read-file'.

Should no file name be given, the value of the variable `ctypes-file-name'
is used."
  (interactive
   (list
    (ctypes-interactive-read-file-name "Write types file: ")))
  (setq file (ctypes-gen-file-name file))
  (save-excursion
    (set-buffer (get-buffer-create " *ctypes-write-file*"))
    (erase-buffer)
    (insert ";; This file has been automatically generated by the ")
    (insert "Emacs package `ctypes'.\n")
    (insert ";; Please use the `ctypes-read-file' to load it.\n\n")
    (dolist (desc ctypes-mode-descriptor)
      (insert "(ctypes-set-types '")
      (insert (prin1-to-string (car desc)))
      (insert " '")
      (insert (prin1-to-string (ctypes-get-types (car desc))))
      (insert ")\n\n"))
    (write-region 1 (point-max) file)
    (erase-buffer))
  (setq ctypes-saved-p t))


(defun ctypes-kill-emacs-hook (&optional file)
  "Save the types to FILE, when needed.

Should the variable `ctypes-write-types-at-exit' be nil this function
does nothing.  Should it be the atom `ask' the user is prompted before
the types are saved.

When FILE is nil, the variable `ctypes-file-name' is used."
  (interactive)
  (setq file (ctypes-gen-file-name file))
  (and (not ctypes-saved-p)
       ctypes-write-types-at-exit
       (or (not (eq ctypes-write-types-at-exit 'ask))
           (y-or-n-p (format "Save types in `%s'? " file)))
       (ctypes-write-file file)))


(defun ctypes-interactive-read-file-name (prompt)
  "Command argument reader, suitable for `interactive'."
  (read-file-name prompt
                  default-directory
                  (file-name-nondirectory ctypes-file-name)))


(defun ctypes-gen-file-name (file)
  "Generate the file name to used to read and write the types.

Should FILE be nil or an empty string, the content of the
variable `ctypes-file-name' is used.  Should FILE be a directory
name, the file part of `ctypes-file-name' is added to FILE."
  (cond ((or (null file) (string= file ""))
         ctypes-file-name)
        ((file-directory-p file)
         ;; I really would like a system-independent
         ;; add-file-to-directory function...
         (let* ((base (file-name-nondirectory ctypes-file-name))
                (first-try (concat file base)))
           (if (string= base (file-name-nondirectory first-try))
               first-try
             (concat file "/" base))))
        (t file)))

;;}}}
;;{{{ Edit

(defvar ctypes-edit-map nil
  "Keymap used in ctypes-edit mode.")
(if ctypes-edit-map
    nil
  (setq ctypes-edit-map (make-sparse-keymap))
  (define-key ctypes-edit-map "\C-c\C-c" 'ctypes-edit-update-and-exit)
  (define-key ctypes-edit-map "\C-c\C-x" 'ctypes-edit-update)
  (define-key ctypes-edit-map "\C-c\C-w" 'ctypes-edit-write-file))


(defvar ctypes-edit-types-for-mode nil
  "Major mode that the edited types belong to.

This is a buffer-local variable used by `ctypes-edit-mode'.")


(defun ctypes-edit (&optional mode)
  "Create buffer for editing types in current major mode.

The buffer can be edited using normal Emacs editing commands.  When
done, press C-c C-c to use the edited version of the types.

See also the function `ctypes-edit-types-in-mode'."
  (interactive)
  (or mode
      (setq mode major-mode))
  (let ((buf (get-buffer-create "*ctypes-edit*"))
        (lst (ctypes-get-types mode)))
    (if (not (assq mode ctypes-mode-descriptor))
        (error "Can't edit types for %s %s"
               mode "(see variable `ctypes-mode-descriptor')."))
    (switch-to-buffer buf)
    (set (make-local-variable 'ctypes-edit-types-for-mode) mode)
    (erase-buffer)
    (insert (format ";; Types for %s.\n" mode))
    (insert ";;\n")
    (insert ";; Press `C-c C-c' to install types.\n")
    (insert ";;       `C-x k' to discard changes.\n\n")
    (save-excursion
      (dolist (type lst)
        (insert type)
        (insert "\n")))
    (set-buffer-modified-p nil)
    (ctypes-edit-mode)))


;; This function is designed for interactive use only.
(defun ctypes-edit-types-in-mode (mode)
  "Create buffer for editing types in major mode MODE.

The buffer can be edited using normal Emacs editing commands.  When
done, press C-c C-c to install the edited version of the types."
  (interactive "sMode: ")
  (ctypes-edit (ctypes-string-to-mode mode)))


(defun ctypes-edit-mode ()
  "Major mode for editing types.
\\{ctypes-edit-map}"
  (interactive)
  (setq major-mode 'ctypes-edit-mode)
  (setq mode-name "CTypes-Edit")
  (use-local-map ctypes-edit-map))


(defun ctypes-edit-update (&optional delay-action)
  "Install the types currently found in the *ctypes-edit* buffer.

When preceded by C-u the display is not updated.

Return non-nil if the set of types has been changed."
  (interactive "P")
  (if (not (eq major-mode 'ctypes-edit-mode))
      (error "Command only meaningful in the *ctypes-edit* buffer"))
  (let* ((lst (ctypes-edit-get-types))
         (added (ctypes-set-types ctypes-edit-types-for-mode lst)))
    (ctypes-perform-action ctypes-edit-types-for-mode added delay-action)
    added))


(defun ctypes-edit-update-and-exit (&optional inhibit-redraw)
  "Install the types and close the edit buffer.

When preceded by C-u the display is not updated.

Return non-nil if the set of types has been changed."
  (interactive "P")
  (prog1
      (ctypes-edit-update inhibit-redraw)
    (set-buffer-modified-p nil)
    (kill-buffer (current-buffer))))


(defun ctypes-edit-write-file (file &optional inhibit-redraw)
  (interactive
   (list
    (ctypes-interactive-read-file-name "Write types file: ")
    current-prefix-arg))
  (ctypes-edit-update inhibit-redraw)
  (ctypes-write-file file)
  (set-buffer-modified-p nil))


(defun ctypes-edit-get-types ()
  "Return, as a list of strings, the types in an `ctypes-edit' buffer.
The types could even be regexps."
  (save-excursion
    (let ((lst '()))
      (goto-char (point-min))
      (while (< (point) (point-max))
        (skip-chars-forward " \t")
        (if (not (eq (following-char) ?\;))
            (let ((p (point)))
              (end-of-line)
              (skip-chars-backward " \t")
              (if (not (equal p (point)))
                  (setq lst (cons (buffer-substring-no-properties
                                   p (point))
                                  lst)))))
        (forward-line))
      (reverse lst))))

;;}}}
;;{{{ Types alist primitives

;; This section contains functions that handle the actual set of types
;; found in each mode.

(defvar ctypes-types-alist '()
  "AList containing types for various modes.

The car of each element is the major mode (a symbol) and the cdr is a
list containing the types (strings).")


(defun ctypes-get-types (mode)
  "Return types for major mode MODE."
  (let ((pair (assq mode ctypes-types-alist)))
    (if pair
        (cdr pair)
      '())))


(defun ctypes-set-types (mode type-list)
  "Replace current set of types for major mode MODE.

Return non-nil if the new set of types is different from the original set."
  (let ((tmp '())
        (done nil)
        added)
    (while (not done)
      (cond ((null ctypes-types-alist)
             (setq added (not (null type-list)))
             (setq done t))
            ((eq mode (car (car ctypes-types-alist)))
             (setq added (not (ctypes-equal type-list
                                            (cdr (car ctypes-types-alist)))))
             (setq ctypes-types-alist (cdr ctypes-types-alist))
             (setq done t))
            (t
             (setq tmp (cons (car ctypes-types-alist) tmp))
             (setq ctypes-types-alist (cdr ctypes-types-alist)))))
    (if type-list
        (setq ctypes-types-alist (cons (cons mode type-list)
                                       ctypes-types-alist)))
    (setq ctypes-types-alist (append (reverse tmp) ctypes-types-alist))
    (if added
        (setq ctypes-saved-p nil))
    added))


(defun ctypes-add-types (mode type-list)
  "Add types in TYPE-LIST to major mode MODE.
Return non-nil if at least one new type was added."
  (let ((current-types (ctypes-get-types mode))
        (added nil))
    (dolist (type (reverse type-list))  ; Try to keep original order.
      (if (member type current-types)
          ()
        (setq current-types (cons type current-types))
        (setq added t)))
    (if added
        (ctypes-set-types mode current-types))
    added))


(defun ctypes-collect-types (mode)
  "Return types for MODE, including inherited types."
  (let ((modes (ctypes-collect-super-modes mode))
        (types '()))
    (while modes
      (setq types (ctypes-union-types types (ctypes-get-types (car modes))))
      (setq modes (cdr modes)))
    types))


(defun ctypes-collect-super-modes (mode)
  "Return a list of all super modes to MODE.

Note that we have not superimposed any type of structure on the
inheritance graph.  For example,  it can contain cycles!

MODE is trivially a super mode to itself."
  (let ((super-modes '())
        (must-check (list mode)))
    (while must-check
      (setq mode (car must-check))
      (setq must-check (cdr must-check))
      (let ((desc (assq mode ctypes-mode-descriptor)))
        (cond (desc
               (setq desc (cdr desc))  ;; Remove the mode name.
               (while desc
                 (if (eq (nth 0 (car desc)) 'inherit)
                     (let ((other-mode (nth 1 (car desc))))
                       (if (and (not (eq other-mode mode))
                                (not (memq other-mode super-modes))
                                (not (memq other-mode must-check)))
                           (setq must-check (cons other-mode must-check)))))
                 (setq desc (cdr desc)))))
        (setq super-modes (cons mode super-modes))))
    super-modes))


(defun ctypes-collect-sub-modes (mode)
  "Return a list of all modes that inherits MODE."
  (let ((sub-modes '())
        (alist ctypes-mode-descriptor))
    (while alist
      (if (memq mode (ctypes-collect-super-modes (car (car alist))))
          (setq sub-modes (cons (car (car alist)) sub-modes)))
      (setq alist (cdr alist)))
    sub-modes))
    

(defun ctypes-delete-types (mode type-list)
  "Removes types in TYPE-LIST.
Return non-nil if any type was removed."
  (let ((current-types (ctypes-get-types mode))
        (removed nil)
        (new-list '()))
    (while current-types
      (if (member (car current-types) type-list)
          (setq removed t)
        (setq new-list (cons (car current-types) new-list)))
      (setq current-types (cdr current-types)))
    (if removed
        (ctypes-set-types mode (reverse new-list)))
    removed))


;; Type-list primitives.

(defun ctypes-equal (type-list1 type-list2)
  "Non-nil if the lists contain the same types.
Note that the elements need not come in the same order in the two lists."
  (and (ctypes-subset type-list1 type-list2)
       (ctypes-subset type-list2 type-list1)))


(defun ctypes-subset (type-list1 type-list2)
  "Non-nil if type-list1 is included in type-list2."
  (let ((included t))
    (while (and included type-list1)
      (if (not (member (car type-list1) type-list2))
          (setq included nil))
      (setq type-list1 (cdr type-list1)))
    included))


(defun ctypes-union-types (type-list1 type-list2)
  "Return the union of the two type lists."
  (setq type-list1 (reverse type-list1)) ; Try to maintain original order
  (while type-list1
    (if (not (member (car type-list1) type-list2))
        (setq type-list2 (cons (car type-list1) type-list2)))
    (setq type-list1 (cdr type-list1)))
  type-list2)

;;}}}
;;{{{ Perform Action

(defvar ctypes-delayed-action-list '()
  "List of major modes whose action has been delayed.

Normally, this means that the user are executing a number of `ctypes'
and wants to wait to perform the display update until after the last
command.

The actions are performed the next time the function
`ctypes-perform-action' is called with nil as it's DELAY-ACTION
argument, or when `ctypes-perform-delayed-action' is called.")


(defun ctypes-perform-action (modes changed-p delay-action)
  "Perform action for all modes in MODES.

MODES can a mode or a list of modes.

The action is performed immediately for major modes in MODES, and for
major modes that inherits types from modes in MODES, when
`delay-action' is nil, and either changed-p is non-nil or the modes
previously have been marked for delayed action.

Should DELAY-ACTION be non-nil, the actions are not performed
and the modes are marked for delayed action."
  (if (not (listp modes))
      (setq modes (list modes)))
  (cond (delay-action
         ;; Mark all modes for delayed action.
         (if changed-p
             (while modes
               (if (not (memq (car modes) ctypes-delayed-action-list))
                   (setq ctypes-delayed-action-list
                         (cons (car modes) ctypes-delayed-action-list)))
               (setq modes (cdr modes)))))
        (t
         ;; Unless a mode has been changed or has been aschedules for
         ;; delayed action no action should be performed.
         (if (not changed-p)
             (let ((new-modes '()))
               (while modes
                 (if (memq (car modes) ctypes-delayed-action-list)
                     (setq new-modes (cons (car modes) new-modes)))
                 (setq modes (cdr modes)))
               (setq modes new-modes)))
         ;; Update all modes that inherits types.
         (let ((sub-modes '()))
           (while modes
             (let ((tmp (ctypes-collect-sub-modes (car modes))))
               (while tmp
                 (if (not (memq (car tmp) sub-modes))
                     (setq sub-modes (cons (car tmp) sub-modes)))
                 (setq tmp (cdr tmp))))
             (setq modes (cdr modes)))
           (setq modes sub-modes))
         ;; Remove all modes from the delayed action list:
         (let ((new-modes modes)
               (dlist (copy-sequence ctypes-delayed-action-list)))
           ;; We make a copy of `ctypes-delayed-action-list' since we
           ;; don't want to destructively a list that someone else
           ;; might be using.  A concrete example is when
           ;; `ctypes-perform-delayed-action' is used; the variable
           ;; `modes' is also bound to `ctypes-delayed-action-list'!
           (while new-modes
             (setq dlist (delq (car new-modes) dlist))
             (setq new-modes (cdr new-modes)))
           (setq ctypes-delayed-action-list dlist))
         ;; Finally, perform the action.
         (while modes
           (let ((desc (assq (car modes) ctypes-mode-descriptor)))
             (cond (desc
                    (setq desc (cdr desc))      ; Skip mode name
                    (while desc
                      (if (eq (nth 0 (car desc)) 'action)
                          (apply (nth 1 (car desc))
                                 (car modes)
                                 (nthcdr 2 (car desc))))
                      (setq desc (cdr desc))))))
           (setq modes (cdr modes))))))


(defun ctypes-perform-delayed-action ()
  "Perform the action (normally update the display)"
  (ctypes-perform-action ctypes-delayed-action-list nil nil))

;;}}}
;;{{{ The parser

(defun ctypes-parse-buffer (&optional buffer mode filename)
  "Parse BUFFER for types assuming the major mode MODE.

Note: You can not assume the the buffer actually is in mode MODE.

Note 2: The file name is only used for debugging."
  (save-excursion
    (if buffer
        (set-buffer buffer))
    (or mode
        (setq mode major-mode))
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((desc (assq mode ctypes-mode-descriptor)))
        (if desc
            (let ((parser (assq 'parser desc)))
              (if (null parser)
                  (error "No parser specified"))
              (funcall (nth 1 parser) filename)))))))


(defun ctypes-parse-buffer-c (&optional filename)
  "Return list of types found in current buffer."
  (let ((orig-syntax-table (syntax-table)))
    (require 'cc-mode)
    (set-syntax-table c-mode-syntax-table)
    (set (make-local-variable 'parse-sexp-ignore-comments) t)
    (unwind-protect
        (let ((lst '()))
          (while (re-search-forward "^typedef\\>" nil t)
            (condition-case ()
                (setq lst (append (ctypes-parse-typedef) lst))
              (error
               (setq ctypes-parse-error
                     (list (or filename (buffer-file-name)) (point))))))
            lst)
      (set-syntax-table orig-syntax-table))))


(defun ctypes-parse-buffer-c++ (&optional filename)
  "Return list of C++ types found in current buffer."
  (let ((orig-syntax-table (syntax-table)))
    (require 'cc-mode)
    (set-syntax-table c-mode-syntax-table)
    (set (make-local-variable 'parse-sexp-ignore-comments) t)
    (unwind-protect
        (let ((lst '()))
          (while (re-search-forward 
                  "^\\(\\(typedef\\)\\|class\\|struct\\|enum\\)\\>" nil t)
            (condition-case ()
                (if (match-beginning 2)
                    (setq lst (append (ctypes-parse-typedef) lst))
                  (setq lst (cons (ctypes-parse-class) lst)))
              (error
               (setq ctypes-parse-error
                     (list (or filename (buffer-file-name)) (point))))))
          lst)
      (set-syntax-table orig-syntax-table))))


;; I'm not 100% convinced that I haven't oversimplified anything.
(defun ctypes-parse-typedef ()
  "Return the newly defined type in a typedef declaration.
Assume that the point is positioned directly after the `typedef'."
  (ctypes-skip-blank)
  ;; `const' can precede everything, including a `struct'.
  (cond ((looking-at "\\<const\\>")
         (goto-char (match-end 0))
         (ctypes-skip-blank)))
  ;; Skip past the basic type.
  (cond ((looking-at ctypes-repetitive-type-regexp)
         (goto-char (match-end 0))
         (ctypes-skip-blank)
         (while (looking-at ctypes-repetitive-type-regexp)
           (goto-char (match-end 0))
           (ctypes-skip-blank)))
        ((looking-at "\\<\\(struct\\|union\\|enum\\)\\>")
         (goto-char (match-end 0))
         (ctypes-skip-blank)
         (if (looking-at ctypes-identifier-regexp)
             (goto-char (match-end 0)))
         (ctypes-skip-blank)
         (if (eq (following-char) ?{)
             (forward-sexp 1)))
        ((eq (following-char) ?\()
         ;; The basic type is complex, skip it.
         (forward-sexp 1))
        ((looking-at ctypes-identifier-regexp)
         ;; Another typedefed type?
         (goto-char (match-end 0)))
        (t
         (error "Can't parse typedef statement")))
  (ctypes-skip-blank)
  (while (memq (following-char) '(?& ?<))
    (cond ((eq (following-char) ?<)
           ;; C++ template
           (skip-chars-forward "^>")
           (forward-char))
          ((eq (following-char) ?&)
           ;; C++ Reference type
           (forward-char)))
    (ctypes-skip-blank))
  ;; Step into the type to find the name.  Save the start position
  ;; so we can pass over pairs of parentheses to find the next name.
  (let ((types '()))
    (while
        (let (start)
          (ctypes-skip-blank)
          (setq start (point))
          (while (memq (following-char) '(?* ?\())
            (forward-char 1)
            (ctypes-skip-blank))
          (cond ((looking-at ctypes-identifier-regexp)
                 (setq types (cons
                              (regexp-quote
                               (buffer-substring-no-properties
                                (match-beginning 0) (match-end 0)))
                              types)))
                (t
                 (error "Parse error")))
          (goto-char start)
          (while (looking-at
                  (concat "\\([*(&[]\\|\\(" ctypes-identifier-regexp
                          "\\)\\)"))
            (cond ((match-beginning 2)
                   (goto-char (match-end 0)))
                  ((memq (following-char) '(?\( ?\[))
                   (forward-sexp 1))
                  (t
                   (forward-char)))
            (ctypes-skip-blank))
          ;; Comment out this for a more liberal parser.
          (if (not (memq (following-char) '(?, ?\;)))
              (error "Parse Error"))
          (eq (following-char) ?,))
      ;; I know it isn't much, but this is the body of the while-expression.
      (forward-char))
    types))


;; Probably wrong, since I don't speak C++.
(defun ctypes-parse-class ()
  (ctypes-skip-blank)
  (if (looking-at ctypes-identifier-regexp)
      (regexp-quote
       (buffer-substring-no-properties (match-beginning 0) (match-end 0)))
    (error "Not a valid class (I think)")))


(defun ctypes-skip-blank (&optional lim)
  (or lim (setq lim (point-max)))
  (let ((stop nil))
    (while (and (not stop) (< (point) lim))
      (cond ((looking-at "//")
             (skip-chars-forward "^\n" lim))
            ((looking-at "/\\*")
             ;; A comment; there must be a better way to skip this.
             (if (search-forward "*/" nil t)
                 (goto-char (match-end 0))
               (setq stop t)))
            ((= (following-char) ?\n)
             (skip-chars-forward "\n" lim))
            ((looking-at "^#")
             (while (progn
                      (end-of-line)
                      (eq (preceding-char) ?\\))
               (forward-line))
             (forward-line))
            ((looking-at "\\s ")
             (if (re-search-forward "\\S " lim 'move)
                 (forward-char -1)))
            (t
             (setq stop t))))
    stop))

;;}}}
;;{{{ TAGS

(defun ctypes-tags-parse ()
  "Parse files in current TAGS table.  Does not perform redraw.

Return list of updated modes.

See the function `ctypes-tags'."
  (save-excursion
    (let ((first-time t)
          (modes '())
          new)
      (while (condition-case ()
                 (progn
                   (setq new (next-file first-time t))
                   t)
               (error nil))
        (setq first-time nil)
        (let* ((buffer-file-name new)
               (mode (or (and new (ctypes-get-mode)) major-mode)))
          (if (assq mode ctypes-mode-descriptor)
              (if (ctypes-buffer nil t mode)
                  (if (not (memq mode modes))
                      (setq modes (cons mode modes)))))))
      modes)))

;;}}}
;;{{{ Major mode functions

;; Sigh, all this code already exists in Emacs!  However, In addition
;; to finding the mode that code also _activates_ the modes.  Clearly,
;; this is not a Good Thing since we only would like to parse the
;; content and get on with it.
;;
;; Also, I had to chop up the original function `set-auto-mode' into
;; three parts since I needed to get access to the `auto-mode-alist'
;; code in isolation.

(defun ctypes-get-mode (&optional buf)
  "Return mode the buffer ought to have."
  (or buf
      (setq buf (current-buffer)))
  (save-excursion
    (set-buffer buf)
    (or (let ((modes (ctypes-get-auto-mode buf)))
          (cond ((eq modes '())
                 nil)
                ((= (length modes) 1)
                 (car modes))
                (t
                 ;; Several modes was specified using the -*- mode:
                 ;; foo; mode: bar; -*- construction.  We have no idea
                 ;; which are major and which are minor modes so we
                 ;; pick the last one that is a member of
                 ;; `ctypes-mode-descriptor'.
                 (let ((done nil))
                   (setq modes (reverse modes))
                   (while (and (not done) (> (length modes) 1))
                     (if (assq (car modes) ctypes-mode-descriptor)
                         (setq done t)
                       (setq modes (cdr modes))))
                   (car modes)))))
        (ctypes-get-mode-from-file-name)
        (ctypes-get-mode-interpreter))))


(defun ctypes-get-auto-mode (buf)
  "Return list of modes specified in a -*- ... -*- header line."
  (let (beg end modes)
    (save-excursion
      (goto-char (point-min))
      (skip-chars-forward " \t\n")
      (and enable-local-variables
           ;; Don't look for -*- if this file name matches any
           ;; of the regexps in inhibit-first-line-modes-regexps.
           (let ((temp inhibit-first-line-modes-regexps)
                 (name (if buffer-file-name
                           (file-name-sans-versions buffer-file-name)
                         (buffer-name))))
             (while (let ((sufs inhibit-first-line-modes-suffixes))
                      (while (and sufs (not (string-match (car sufs) name)))
                        (setq sufs (cdr sufs)))
                      sufs)
               (setq name (substring name 0 (match-beginning 0))))
             (while (and temp
                         (not (string-match (car temp) name)))
               (setq temp (cdr temp)))
             (not temp))
           (search-forward "-*-" (save-excursion
                                   ;; If the file begins with "#!"
                                   ;; (exec interpreter magic), look
                                   ;; for mode frobs in the first two
                                   ;; lines.  You cannot necessarily
                                   ;; put them in the first line of
                                   ;; such a file without screwing up
                                   ;; the interpreter invocation.
                                   (end-of-line (and (looking-at "^#!") 2))
                                   (point)) t)
           (progn
             (skip-chars-forward " \t")
             (setq beg (point))
             (search-forward "-*-"
                             (save-excursion (end-of-line) (point))
                             t))
           (progn
             (forward-char -3)
             (skip-chars-backward " \t")
             (setq end (point))
             (goto-char beg)
             (if (save-excursion (search-forward ":" end t))
                 ;; Find all specifications for the `mode:' variable
                 ;; and execute them left to right.
                 (while (let ((case-fold-search t))
                          (or (and (looking-at "mode:")
                                   (goto-char (match-end 0)))
                              (re-search-forward "[ \t;]mode:" end t)))
                   (skip-chars-forward " \t")
                   (setq beg (point))
                   (if (search-forward ";" end t)
                       (forward-char -1)
                     (goto-char end))
                   (skip-chars-backward " \t")
                   (setq modes 
                         (cons (intern 
                                (concat 
                                 (downcase 
                                  (buffer-substring beg (point))) "-mode"))
                               modes)))
               ;; Simple -*-MODE-*- case.
               (setq modes 
                     (cons (intern 
                            (concat (downcase (buffer-substring beg end))
                                                 "-mode"))
                                 modes))))))
    (reverse modes)))


(defun ctypes-get-mode-from-file-name (&optional name)
  "Suggest major mode for file named NAME, no nil."
  (or name
      (setq name buffer-file-name))
  ;; Code taken from `set-auto-mode'.
  (let ((keep-going t)
        (mode nil))
    (setq name (file-name-sans-versions name))
    (while keep-going
      (setq keep-going nil)
      (let ((alist auto-mode-alist))
        ;; Find first matching alist entry.
        (let ((case-fold-search (memq system-type '(vax-vms windows-nt))))
          (while (and (not mode) alist)
            (if (string-match (car (car alist)) name)
                (if (and (consp (cdr (car alist)))
                         (nth 2 (car alist)))
                    (setq mode (car (cdr (car alist)))
                          name (substring name 0 (match-beginning 0))
                          keep-going t)
                  (setq mode (cdr (car alist))
                        keep-going nil)))
            (setq alist (cdr alist))))))
    mode))


(defun ctypes-get-mode-interpreter ()
  "Get major mode based on #! sequence at head of buffer."
  (save-excursion
    (goto-char (point-min))
    (and (looking-at "#![ \t]?\\([^ \t\n]*/bin/env[ \t]\\)?\\([^ \t\n]+\\)")
         (cdr-safe (assoc (file-name-nondirectory
                           (buffer-substring (match-beginning 2)
                                             (match-end 2)))
                          interpreter-mode-alist)))))

;;}}}
;;{{{ Misc

(or (fboundp 'regexp-opt-depth)
    (defun regexp-opt-depth (keyword)
      "Return the depth of KEYWORD regexp.
This means the number of parenthesized expressions."
      (let ((count 0) start)
        (while (string-match "\\\\(" keyword start)
          (setq start (match-end 0) count (1+ count)))
        count)))


(defun ctypes-string-to-mode (mode)
  "Convert a mode name, entered by the user, to a mode symbol.

Example:
    (ctypes-string-to-mode \"C++\")  =>  c++-mode"
  (if (stringp mode)
      (if (string-match "-mode$" mode)
          (setq mode (intern mode))
        (setq mode (intern (concat mode "-mode")))))
  ;; Make sure "C++" works.
  (if (not (assq mode ctypes-mode-descriptor))
      (let ((lowercase-mode (intern (downcase (symbol-name mode)))))
        (if (assq lowercase-mode ctypes-mode-descriptor)
            (setq mode lowercase-mode))))
  mode)


(defun ctypes-get-type-under-point ()
  (save-excursion
    (if (eq (char-syntax (following-char)) ? )
        (skip-chars-backward " \t"))
    (skip-chars-backward "a-zA-Z0-9:_$")
    (and (looking-at ctypes-identifier-regexp)
         (regexp-quote
          (buffer-substring-no-properties
           (match-beginning 0)
           (match-end 0))))))

;;}}}
;;{{{ Font-lock stuff

;; This section contains some font-lock support functions.
;;
;; Even though the main purpose of the `ctypes' package is to enhance
;; the fontification of C-like languages, the design of the package
;; does not limit itself to such narrow goal.  Should you prefer to
;; use ctypes to anything else just redefine `ctypes-mode-descriptor'.


(defun ctypes-font-lock-set-extra-types (mode extra-types-var)
  "Add the new keywords to font-lock.
This function is used for font-lock versions that have native
support for extra types.  As of this writing, no official releases
with this feature has been made."
  (set extra-types-var (ctypes-get-types mode))
  (ctypes-font-lock-refontify mode))


(defun ctypes-font-lock-refontify (mode)
  "Refontify all buffers in major mode MODE."
  (save-excursion
    (let ((bufs (buffer-list)))
      (while bufs
        (set-buffer (car bufs))
        (if (and (eq major-mode mode) font-lock-mode)
            (progn
              (font-lock-mode -1)
              (font-lock-mode 1)))
        (setq bufs (cdr bufs))))))


;; Code used by font-lock versions without native type support.

(defvar ctypes-font-lock-keywords '()
  "AList of all keywords installed by ctypes in font-lock keywords.

This is needed when old keywords are replaced with newer.")


(defun ctypes-font-lock-add-keywords (mode rules)
  "Add font-lock keywords for major mode MODE."
  (let ((types (ctypes-get-types mode)))
    (if (null types)
        (ctypes-font-lock-delete-keywords mode rules)
      (ctypes-font-lock-install-keywords
       mode (mapconcat 'identity types "\\|") rules))
    (ctypes-font-lock-refontify mode)))


(defun ctypes-font-lock-install-keywords (mode regexp rules)
  "Add REGEXP as new C-style types in major mode MODE.

The rules is a list containing elements on the following form:
  (number var [append])

Where `number' can be 1 or 2 and represents a simple and one complex
keyword, respectively.  Normally, the simpler is defined at a lower
fontification but both are needed to get full fontification.

`var' is the font-lock keyword variable to use and `append' is an
optional argument, when true the new keyword is appended to the end
of the keyword list."
  (ctypes-font-lock-delete-keywords mode rules)
  (let ((keyword-1
         (cons (concat "\\<\\(" regexp "\\)\\>") 'font-lock-type-face))
        (keyword-2
         (list
          (concat "\\<\\(" regexp "\\)\\>\\([ \t*&]+\\sw+\\>\\)*")
          ;; Fontify each declaration item.
          (list 'font-lock-match-c++-style-declaration-item-and-skip-to-next
            ;; Start with point after all type specifiers.
            (list 'goto-char 
                  (list 'or (list 'match-beginning
                                  (+ 2 (regexp-opt-depth regexp)))
                        '(match-end 1)))
            ;; Finish with point after first type specifier.
            '(goto-char (match-end 1))
            ;; Fontify as a variable or function name.
            '(1 (if (match-beginning 4)
                    font-lock-function-name-face
                  font-lock-variable-name-face))))))
    (setq ctypes-font-lock-keywords
          (cons (list mode keyword-1 keyword-2)
                ctypes-font-lock-keywords))
    (while rules
      (let ((number (nth 0 (car rules)))
            (var (nth 1 (car rules)))
            (append-p (nth 2 (car rules)))
            keywords)
        (cond ((= number 1)
               (setq keywords keyword-1))
              ((= number 2)
               (setq keywords keyword-2))
              (t
               (error "Incorrect entry in rule.  Found `%s', expected 1 or 2."
                      number)))
        (if append-p
            (set var (append (symbol-value var) (list keywords)))
          (set var (cons keywords (symbol-value var)))))
      (setq rules (cdr rules)))))


(defun ctypes-font-lock-delete-keywords (mode rules)
  "Delete keywords form major mode MODE, described by RULES.

See the function `ctypes-font-lock-install-keywords' for a description
of RULES."
  (let ((keywords (assq mode ctypes-font-lock-keywords)))
    (if (null keywords)
        ()
      (setq ctypes-font-lock-keywords
            (delq keywords ctypes-font-lock-keywords))
      (setq keywords (cdr keywords))    ; Skip the mode name
      (while rules
        (let ((keywords keywords)       ; Iteration-local alias
              (var (nth 1 (car rules))))
          (while keywords
            (set var (delq (car keywords) (symbol-value var)))
            (setq keywords (cdr keywords))))
        (setq rules (cdr rules))))))

;;}}}

;;{{{ Debug

(defun ctypes-debug ()
  (interactive)
  (with-output-to-temp-buffer "*CTypes-Debug*"
    (princ "ctypes-types-alist:")
    (print ctypes-types-alist)
    (terpri)
    
    (princ "ctypes-font-lock-keywords:")
    (print ctypes-font-lock-keywords)
    (terpri)

    (princ "ctypes-delayed-action-list:")
    (print ctypes-delayed-action-list)
    (terpri)))

;;}}}

;; The End

(add-hook 'find-file-hooks 'ctypes-find-file-hook)
(add-hook 'kill-emacs-hook 'ctypes-kill-emacs-hook)

(provide 'ctypes)

(run-hooks 'ctypes-load-hook)

;; ctypes.el ends here.
