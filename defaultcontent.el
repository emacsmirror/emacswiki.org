;;; defaultcontent.el --- a templating tool. Fill new files with default content.
;;
;; Author     : Christian Queinnec (University Paris 6) (UPMC)
;; Modified by: Dino Chiesa <dpchiesa@hotmail.com>
;; Created    : September 12, 1993
;; Version    : 1.4
;; Keywords   : template
;; X-URL      : http://www.emacswiki.org/emacs/defaultcontent.el
;; Last-saved : <2012-March-24 02:34:42>

;; Copyright (C) 1993-2001 by Christian Queinnec (University Paris 6)
;;

;; This file is not part of GNU Emacs and is licensed differently.

;;; Commentary:

;;; The purpose of this package is to provide a default content for
;;; files. It extends the autoinsert package from Charlie Martin and
;;; took the idea of expanding parameters as in auto-template from
;;; Kevin Broadey (as suggested by Cedric Beust). It also allows to
;;; specify the initial position of the dot.

;;; Whenever an unexistent file is visited, its initial content can be
;;; specified by a parameterized template stored in the directory
;;; dc-auto-insert-directory or by explicit evaluation of some
;;; specified forms. A single undo removes all this initialization.

;;; To use it, add to your .emacs (taking care of load-path)
;;;          (require 'defaultcontent)
;;; and it is ready (try to open the empty foobar.el file for example).

;;; Repository:
;;; older version:
;;;  ftp://ftp.cse.ohio-state.edu/pub/emacs-lisp/old-archive/misc/defaultcontent.el.Z
;;; Newer versions will be sent to the LCD Archive but may appear earlier
;;; on http://youpou.lip6.fr/queinnec/Miscellaneous/
;;; Other Emacs packages in:
;;;    http://youpou.lip6.fr/queinnec/WWW/elisp.html

;;; Code:

;;; Indicate where you store the default content of files.

(defvar dc-auto-insert-directory "~/.el/defaultcontent"
  "*Directory from which template files are taken. On
Windows, use forward slashes like: c:/foo/bar/baz ")


;;; A list of actions. You can insert the name of a file (which will
;;; be extracted from dc-auto-insert-directory) or a list of filenames
;;; (you'll be prompted to choose one among them) or a list of forms
;;; to be evaluated that might fill the newly created file. These
;;; actions may use the variables of the mode, use the name of the
;;; buffer etc.

(defvar dc-auto-insert-alist
  '(("\\.tex$"         . "tex-insert.tex")
    ("\\.c$"           . "c-insert.c")
    ("\\.h$"           . "h-insert.h")
    ("[I]?[Mm]akefile" . "makefile-insert")
    ("\\.scm$"         (dc-fill-initial-content-of-file))
    ("\\.el$"          (dc-fill-initial-content-of-file))
    ("\\.bib$"         . "bib-insert.bib") )
  "An Alist specifying text to insert by default into a new file.
Elements look like (REGEXP . FILENAME) or (REGEXP LISP-CODE ...);
if the new file's name matches REGEXP, then the file FILENAME is
inserted into the buffer or LISP-CODE is evaluated with the same goal.
Only the first matching element is effective." )

;;; Courtesy of Frederic Lepied <Frederic.Lepied@sugix.frmug.fr.net>
;;; Fetch customization by major-mode if not yet specified in
;;; dc-auto-insert-alist.

(defvar dc-auto-insert-mode-alist
  '((sh-mode         . "sh-insert.sh") )
  "An Alist specifying text to insert by default into a new file.
Elements look like (MODE . FILENAME) or (MODE LISP-CODE ...);
if the new file's major mode MODE, then the file FILENAME is
inserted into the buffer or LISP-CODE is evaluated with the same goal.
Only the first matching element is effective. This list is tried
after `dc-auto-insert-alist'." )

;;; This function fills an empty file with:
;;;          +---------------------------------
;;;          |### $ Id $
;;;          |
;;;          |### end of <filename>
;;;          +------------------------------
;;; Where # is supposed to be the comment-start character. You can
;;; use it instead of having multiple templates.

(defun dc-fill-initial-content-of-file ()
  "Create the initial content of a RCS-kept file appropriately ended."
  (goto-char 0)
  (insert comment-start)(insert comment-start)(insert comment-start)
  (insert " \$Id\$\n\n")(insert comment-end)
  (goto-char (point-max))
  (insert comment-start)(insert comment-start)(insert comment-start)
  (insert " end of ")(insert (file-name-nondirectory buffer-file-name))
  (insert comment-end)(insert "\n")
  (goto-char 0)(goto-line 2)
  (message "Nature dislikes emptyness!") )

(defvar dc-initial-dot-position nil
  "This variable defines, if not nil, the initial position of the dot.
It can be set by the @DOT@ pseudo-variable in a template." )

;;; Two possibilities exist whether one wants to be fast or slow (but
;;; more powerful). This was suggested by Luc Moreau
;;; <moreau@montefiore.ulg.ac.be>.

(defvar dc-fast-variable-handling t
  "A boolean telling if variables are slowly recognized with a regexp
or quickly handled with a delimiting character." )

;;; File template are processed and every thing surrounded with double
;;; @ is replaced by its value.  The variables to recognize and to
;;; expand and their associated actions is kept in the following
;;; Alist. In fact, the character that delimit variables is a
;;; programmable regexp.

(defvar dc-variable-delimiter "\@[^@]*\@"
  "Regexp to recognize variables to expand." )

;;; Use a single character to delimit file templates.

(defvar dc-variable-border "@"
  "Delimiting character for variables to expand." )

;; This alist allows to replace various keywords, aka "expandos", with
;; some elaborated value. For example, the expando FILENAME will expand
;; to the full name of the file. The BASEFILENAME will expand to the file
;; name without the qualifying directory. And so on.

(defvar dc-expandos-alist
  '(( "@BASEFILENAME@"  (file-name-nondirectory buffer-file-name) )
    ( "@BASEFILENAMELESSEXTENSION@"
      (dc--filename-remove-extension
       (file-name-nondirectory buffer-file-name) ) )
    ( "@FILENAME@"      buffer-file-name )
    ( "@DATE@"          (current-time-string) )
    ( "@HOST@"          (or (getenv "HOST") (getenv "COMPUTERNAME")))
    ( "@AUTHOR@"        (capitalize (or (getenv "USER") (getenv "USERNAME"))))
    ( "@COMMENT-START@" (if comment-start comment-start "") )
    ( "@COMMENT-END@"   (if comment-end comment-end "") )
    ( "@DOT@"           (setq dc-initial-dot-position (match-beginning 0))
                        "" )


    ( "@\\(INSERT\\|INSERTFILE\\)(\\(.+\\))@"
      (let ((filename
             (buffer-substring-no-properties
              (match-beginning 2)
              (match-end 2))))
        (if (file-readable-p filename)
            (with-temp-buffer
              (insert-file-contents filename)
              (buffer-substring-no-properties (point-min) (point-max)))
          (concat "The file '" filename "' is not readable"))))

    ( "@ENV(\\(.+\\))@"    (let ((varname
                                (buffer-substring-no-properties
                                 (match-beginning 1)
                                 (match-end 1))))
                           (or (getenv varname) varname)))

    ( "@@"              "@")

    ;; This expands custom elisp code.
    ;; Courtesy of Luc Moreau:
    ( "@LISP(\\(.*\\))@"     (let (sexp value (here (point)))
                             (goto-char (match-beginning 0))
                             (setq sexp (dc--read-closest-sexp))
                             (if sexp (setq value (eval sexp)))
                             (goto-char here)
                             (if value value "") ) )
    )
  "An Alist specifying the variables to recognize and how to replace them.
Elements look like (REGEXP LISP-CODE ...). When a variable is recognized,
using dc-variable-delimiter, it is compared to the REGEXPs (if dc-fast-
-variable-handling is false) and once one is found, the associated forms
are evaluated and the result replaces the occurrence of the variable." )

;;; A small utility to remove filename extensions.

(require 'thingatpt)

(defun dc--read-closest-sexp ()
  "utility to read sexp at pt"
  (thing-at-point 'sexp))


(defun dc--filename-remove-extension (name &optional extension)
  "Return NAME less its EXTENSION. If the extension is given as second
argument then it is an error for the extension not to be present."
  (let* ((extension (if extension (regexp-quote extension) "\\.[^.]*"))
         (regexp (concat "\\(.*\\)" extension "$")) )
    ;(message regexp)(sleep-for 10)
    (if (string-match regexp name)
        (substring name (match-beginning 1) (match-end 1))
      (error "No extension" name) ) ) )

;;; For instance, here is my default template for Perl files. It is stored
;;; as a file in the dc-auto-insert-alist directory.
;;; +-------------------------
;;; |#! /usr/local/bin/perl
;;; |@DOT@
;;; |# end of @BASEFILENAME@
;;; +------------------------

(defvar dc-show-unexpanded-variables t
  "This variable shows, if true, the variables that cannot be expanded
by dc-auto-insert-file. Nevertheless, it slows down expansion but gives
you a chance to see bad variables." )

;;; The real function that does the real work ie it looks for variables
;;; and expands them. It may also notiy erroneous variables with the
;;; previous dc-show-unexpanded-variables boolean flag.

(defun dc-expand-internal-variables (start)
  "Replace @ things @ by their expansion in a freshly filled file."
  (interactive (list 0))
  (goto-char start)
  (let ((number-of-expanded-variables 0))
    (if dc-fast-variable-handling
        ;; courtesy of Luc Moreau:
        (while (search-forward dc-variable-border nil t)
          (backward-char 1)
          (let ((l dc-expandos-alist))
            (while (consp l)
              (let ((regexp (car (car l)))
                    (forms (cdr (car l))) )
                (setq l (cdr l))
                ;; Search if it is a known variable
                (if (looking-at regexp)
                    (let* ((the-first-match (match-data))
                           (new (eval (cons 'progn forms))) )
                      ;; restore the old match
                      (store-match-data the-first-match)
                      (replace-match new t t)
                      (setq number-of-expanded-variables
                            (+ 1 number-of-expanded-variables) )
                      (setq l nil) )
                  (if (null l)
                      (forward-char 1) ) ) ) )))
      ;; use regexp to recognize variables
      (while (re-search-forward dc-variable-delimiter nil t)
        (let ((the-first-match (match-data))
              (beg (match-beginning 0))
              (end (match-end 0)) )
          (goto-char beg)
          (let ((l dc-expandos-alist))
            (while (consp l)
              (let ((regexp (car (car l)))
                    (forms (cdr (car l))) )
                (setq l (cdr l))
                ;; Search if it is a known variable
                (if (looking-at regexp)
                    (let (new)
                      (goto-char end)
                      (setq new (eval (cons 'progn forms)))
                      (setq number-of-expanded-variables
                            (+ 1 number-of-expanded-variables) )
                      ;; restore the old match
                      (store-match-data the-first-match)
                      (replace-match "" t t)
                      (insert new)
                      (setq l nil) )
                  (if (and (null l) dc-show-unexpanded-variables)
                      (progn
                        (goto-char (+ beg 1))
                        (message "Cannot expand \"%s\"."
                                 (buffer-substring beg end) ) )
                    (sleep-for 1) ) ) ) ) ) ) ) )
    (message "Note: %s variable(s) expanded."
             number-of-expanded-variables ) ) )

;;; This local variable tells if a file is new. It will be used by the
;;; dc-insert-auto-insert-file function to determine if the file was
;;; new or not. It cannot be made buffer-local since the correct
;;; buffer is still not present. So use a global variable. Note that
;;; an empty file is not immediately filled, this hook only records
;;; that it is fresh. Only a fresh file is filled but not an existing
;;; and empty file!!!

(defvar dc-file-appears-to-be-new nil
  "Global variable set to T if find-file-not-found-hooks was run.
Used by dc-insert-auto-insert-file to detect if a file is new." )

;;; Just mark the file as new since this is run from find-file-not-found-hooks
;;; but return () to continue to process the remaining hooks.

(defun dc-mark-file-as-new ()
  ;;(message "Hmm... this is a new file.")(sit-for 1)
  (setq dc-file-appears-to-be-new t)
  () )

;;; This hook sets a boolean flag if the file is new.

(add-hook 'find-file-not-found-hooks 'dc-mark-file-as-new)

;;; This function is the hook that triggers the initial filling of an
;;; empty file. It scans the dc-auto-insert-alist to find an appropriate
;;; action, otherwise it does nothing.

(defun dc-insert-auto-insert-file ()
  "Matches the visited file name against the elements of `dc-auto-insert-alist'
to determine the initial content of the visited file."
  (interactive)
  (let ((mfan dc-file-appears-to-be-new))
    ;;(message "Is this file new? %s!" mfan)(sit-for 1)
    (if (and (or current-prefix-arg mfan) (= 0 (buffer-size)))
        ;; the file is new and empty, so try to fill it.
        (let ((alist dc-auto-insert-alist)
              ;; remove backup suffixes from file name
              (name (file-name-sans-versions buffer-file-name))
              (data nil) )
          ;; find first matching alist entry
          (while (and (not data) alist)
            (if (string-match (car (car alist)) name)
                (setq data (cdr (car alist)))
              (setq alist (cdr alist))))
          ;; Courtesy of Frederic Lepied <Frederic.Lepied@sugix.frmug.fr.net>
          ;; else match a major mode alist entry
          (if (not data)
              (let ((pair (assoc major-mode dc-auto-insert-mode-alist)))
                (if pair
                    (setq data (cdr pair)))))
          ;; Courtesy of Stefan Reichör <xsteve@riic.at>
          ;; select template from a list with completion
          (when (and data (listp data) (stringp (caar data)))
            (setq data (completing-read
                        "Select template to insert: "
                        (mapcar (lambda (elem) (cons elem elem)) (car data)))))
          ;; analyze data
          (cond ((not data)
                 (message "No initial content specified!") )
                ;; insert the specified file
                ((stringp data)
                 (let ((file (concat (file-name-as-directory dc-auto-insert-directory) data)))
                   (if (file-readable-p file)
                       (progn
                         (insert-file-contents file)
                         (setq dc-initial-dot-position 0)
                         (dc-expand-internal-variables 0)
                         (goto-char dc-initial-dot-position) )
                     (progn
                       (message "Auto-insert: Template file %s not found" file)
                       (sleep-for 1) ) ) ) )
                ;; evaluate the forms
                (t (eval (cons 'progn data))) )
          ;;(message "Initialization done")(sit-for 1)
          ;; reset this global variable in case the file is revisited.
          (setq dc-file-appears-to-be-new nil) ) ) ) )

;;; Run after all other hooks to benefit from them and particularly of
;;; the correct settings for comment-start and comment-end variables.

(add-hook 'find-file-hooks 'dc-insert-auto-insert-file t)

;;; So it can be required.

(provide 'defaultcontent)

;;; end of defaultcontent.el
