;;; rmailgen.el --- generates guesses for RMAIL output files and FCC files.

;; Author: Lawrence R. Dodd <dodd@roebling.poly.edu>
;; Maintainer: Lawrence R. Dodd <dodd@roebling.poly.edu>
;; Created: 13 Jul 1993
;; Version: 2.1
;; Date: 1994/12/26 17:23:49
;; Keywords: rmail extensions

;; Copyright (C) 1993, 1994, 1995 Free Software Foundation, Inc.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; LCD Archive Entry:
;;; rmailgen|Lawrence R. Dodd|dodd@roebling.poly.edu|
;;; Generates guesses for RMAIL output files and FCC files.|
;;; 1994/12/26 17:23:49|2.1|~/misc/rmailgen.el.Z|

;;; Commentary:

;;; This package generates guesses for RMAIL output files when `o'
;;; (`rmail-output-to-rmail-file') is hit in RMAIL.  When replying to mail
;;; messages (`r') it will generate archive file names (i.e., the FCC, file
;;; carbon-copy, file names that appear in *mail* buffers) if
;;; `rmailgen-archive-file-name' is non-nil (the default).  Both of
;;; these are done using the header of the current RMAIL message.  See Example
;;; Installation below.
;;;
;;; This package also allows user to set a default RMAIL mail directory, see
;;; `rmailgen-default-directory'.
;;;
;;; Specifically, this package parses the account names in the "From:" and
;;; "To:" lines of the original RMAIL message.  It does this by placing a lisp
;;; expression in `rmail-output-file-alist'.  The expression-driven form of
;;; `rmail-output-file-alist' requires GNU Emacs version 19.17 or later.
;;;
;;; Note that `C-o' (`rmail-output'), used for Un*x output format, does not
;;; use `rmail-output-file-alist' so in that case no name is automatically
;;; generated.  However, `o' (`rmail-output-to-rmail-file') is smart enough to
;;; distinguish between Un*x and BABYL format in pre-existing folders.
;;; Therefore, there is really no need to use C-o for appending to a
;;; pre-existing folder (`C-o' is still very useful for outputing a message in
;;; plain Un*x format -- in case you don't want a BABYL header stuck on it).

;;; Bugs:
;;;
;;; If you encounter a bug in this code, wish to suggest an enhancement, or
;;; want to make a smart remark, then type
;;;
;;;              M-x rmailgen-submit-report
;;;
;;; This will set up an outgoing mail buffer, with the proper address to the
;;; `rmailgen.el' maintainer automatically inserted in the `To:' field.  This
;;; command also inserts information that the maintainer can use to recreate
;;; your exact setup, making it easier to verify your bug or social
;;; maladjustment.

;;; Available:
;;;
;;; via anonymous ftp to roebling.poly.edu [128.238.5.31]
;;;
;;;        /anonymous@roebling.poly.edu:/pub/lisp/rmailgen.el.gz
;;;
;;; and at the elisp archive
;;;
;;; /archive.cis.ohio-state.edu:/pub/gnu/emacs/elisp-archive/misc/rmailgen.Z

;;; Installation:
;;;
;;; If not already installed, save this as rmailgen.el in a directory known to
;;; Emacs (check your `load-path' variable for a list of such directories).
;;; In your ~/.emacs file you will need to do the following (only step 8 is
;;; required the rest is optional)
;;;
;;;             1. Change rmailgen-default-directory   (optional)
;;;             2. Define mail-archive-file-name       (optional)
;;;             3. Set rmailgen-archive-file-name      (optional)
;;;             4. Set rmailgen-downcase               (optional)
;;;             5. Set rmailgen-gzip-file-name         (optional)
;;;             6. Set rmailgen-archive-only-if-exists (optional)
;;;             7. Set rmail-output-file-alist         (optional)
;;;             8. Load rmailgen                       [REQUIRED]

;;; There are only two interactive functions:
;;;
;;; rmailgen-rebuild
;;; rmailgen-submit-report

;;; Example Installation:

;;; You can copy this example into your ~/.emacs.  Remove the `;;; ' prefix!

;;; ;; 1. I want to define a mail directory that isn't `~/'
;;; (setq rmailgen-default-directory "~/Mail/") ; must end in slash
;;;
;;; ;; 2. I want to define my own default archive file name.
;;; ;; Regular `send-mail' will use this.
;;; (add-hook 'mail-mode-hook
;;;           (function (lambda ()
;;;                       (setq mail-archive-file-name
;;;                             "~/Mail/.CarbonCopy"))))
;;;
;;; ;; 3. By default, if mail-archive-file-name is non-nil then
;;; ;; archive file names will be generated automatically based on
;;; ;; the message to which a reply is being constructed.  If I
;;; ;; wanted to turn this off I would put in a statement like
;;; ;; (setq rmailgen-archive-file-name nil)
;;;
;;; ;; 4. By default, rmailgen.el downcases generated filenames
;;; ;; If I wanted uppercase I would put in a statement like
;;; ;; (setq rmailgen-downcase nil) here.
;;;
;;; ;; 5. By default, rmailgen.el does not append `.gz' to generated
;;; ;; filenames.  If I wanted such an extension I would put in a statement
;;; ;; like
;;; ;; (setq rmailgen-gzip-file-name t)
;;;
;;; ;; 6. By default, rmailgen.el will use generated FCC filenames even
;;; ;; if the file does not exist.  If I wanted to FCC only if the file
;;; ;; already exists I would put in a statement like
;;; ;; (setq rmailgen-archive-only-if-exists t)
;;;
;;; ;; 7. Add my own personal output list for specific friends
;;; ;; and special subjects.
;;;
;;; ;; First define rmail-output-file-alist, just in case this
;;; ;; is not already defined.  That is, may be
;;; ;; ../lisp/rmailout.el has not been loaded yet.
;;; (if (not (boundp 'rmail-output-file-alist))
;;;     (defvar rmail-output-file-alist nil))
;;;
;;; (setq rmail-output-file-alist
;;;       (append
;;;        (list
;;;
;;;         ;; For my friends (some have strange account names).
;;;         '("^From:[ \t]*.*jones.*" . "jmjones")
;;;         '("^From:[ \t]*.*Joe[ \t]*Smith.*" . "joe")
;;;
;;;         ;; Special subject lines.
;;;         '("^Subject:[ \t]*.*crypt.*" . "crypt++")
;;;         '("^Subject:[ \t]*.*rmailgen.*" . "genrmail")
;;;         '("^Subject:[ \t]*.*dired-x.*" . "dired-x")
;;;         '("^Subject:[ \t]*.*GNU Emacs 19 RMAIL Poll.*" . "rmail")
;;;
;;;         ;; Add more entries here...
;;;         )
;;;
;;;        ;; In case rmail-output-file-alist has been defined
;;;        ;; already elsewhere.
;;;        rmail-output-file-alist))
;;;
;;; ;; 8. Load package [REQUIRED].
;;; (require 'rmailgen)

;;; To do: Should modify `mail-fcc' (C-c C-f C-f in mail-mode) to use
;;; `rmailgen-get-archive-file-name'.

;;; Code:

 
;; vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
;; user defined variables

(defvar rmailgen-archive-file-name t
  "*If non-nil, generate archive filename using message being replied to.
`mail-archive-file-name' must be non-nil as well.  If t then guess archive
filename using `rmail-output-to-rmail-file' algorithm.  If not t, but still
non-nil, will use only explicitly mapped entries appearing in
`rmail-output-file-alist'.  See also `rmailgen-gzip-file-name' and
`rmailgen-downcase'.")

(defvar rmailgen-archive-only-if-exists nil
  "*If non-nil, use generated archive filenames only if file already exists.
If value is non-nil (e.g., 'ask), will query.")

(defvar rmailgen-gzip-file-name nil
  "*If non-nil, appends `.gz' suffix to generated file names.

Be aware that rmailgen is *not* executed if matches are made to the user's
explicit entries in `rmail-output-file-alist' (e.g., for specific friends and
special subjects).  That is, rmailgen's only generates guesses for names *not*
matched explicitly.  In particular this means that rmailgen cannot append a
`.gz' to those matches.  The user should add `.gz' to her/his entries in
`rmail-output-file-alist' if desired.")

(defvar rmailgen-downcase t
  "*t means generate lowercase names, nil means upcase, otherwise do nothing.")

(defvar rmailgen-default-directory nil
  "*If non-nil then directory ending in slash for rmail output default.
`rmail-file-name', `rmail-default-rmail-file', and `rmail-default-file' are
all prepended with this variable.  If nil, nothing is done to these variables.
If you change this variable type \\[rmailgen-rebuild]")

(defvar rmailgen-dont-output-to-names nil "\
*A regexp specifying names to which not to output.
A value of nil means exclude your own name only.")

;; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;; end of user defined variables

;;; It would be nice if there were an `rmail-load-hook'.  That way the
;;; installation of rmailgen.el could be done when rmail is first loaded --
;;; just like dired-x.el is loaded in dired-mode.

 
;;;; SECTION A.
;;;; Default mail directory stuff.

;;; Override all defaults defined by (or to be defined) by ../lisp/rmail.el.

(defun rmailgen-rebuild ()
  "Reinitializes RMAIL variables when `rmailgen-default-directory' is changed."
  (interactive)
  (if rmailgen-default-directory
      (setq

       ;; SYM
       rmail-file-name
       ;; VAL
       (expand-file-name
        (concat rmailgen-default-directory
                (if (and (boundp 'rmail-file-name)
                         rmail-file-name)
                    (file-name-nondirectory rmail-file-name)
                  "RMAIL")))

       ;; SYM
       rmail-default-rmail-file
       ;; VAL
       (expand-file-name
        (concat rmailgen-default-directory
                (if (and (boundp 'rmail-default-rmail-file)
                         rmail-default-rmail-file)
                    (file-name-nondirectory rmail-default-rmail-file)
                  "XMAIL")))

       ;; SYM
       rmail-default-file
       ;; VAL
       (expand-file-name
        (concat rmailgen-default-directory
                (if (and (boundp 'rmail-default-file)
                         rmail-default-file)
                    (file-name-nondirectory rmail-default-file)
                  "xmail"))))))

(rmailgen-rebuild)

 
;;;; SECTION B.
;;;; Generate a guess for `rmail-output-to-rmail-file' ("o" in RMAIL).

;;; Algorithm:
;;;
;;; 1. What ../lisp/rmailout.el of GNU Emacs 19 does:
;;;
;;;    Each element of the variable `rmail-output-file-alist' is of the form
;;;    (REGEXP . NAME-EXP).  `rmail-output-to-rmail-file' searches the current
;;;    RMAIL message for REGEXP.  If found it then evaluates the NAME-EXP
;;;    although it may just be a plain string.  If no match is found then the
;;;    name of the last output file is used.
;;;
;;; 2. What this file does:
;;;
;;;    REGEXP is defined so it will locate the login-name of the sender of the
;;;    current message by looking in the "From" field and then the "To" field.
;;;    NAME-EXP is the function `rmailgen-output-name' which extracts this
;;;    name.  The sender's login-name is returned as a lower-case string as
;;;    long as it does not match the user's own login-name, otherwise it
;;;    returns nil so `rmail-output-to-rmail-file' will continue to search the
;;;    alist.

;;; Define `rmail-output-file-alist' if not already.  That is, maybe
;;; ../lisp/rmailout.el has not been loaded yet and user has not defined any
;;; values.  The value and the doc-string will both be overwritten when
;;; necessary this just prevents an error message when we try to `append' to
;;; this variable.

(if (not (boundp 'rmail-output-file-alist))
    (defvar rmail-output-file-alist nil))

;;; Regular expressions for three types of mail addresses have been moved
;;; together into the function `rmailgen-output-name'.  This saves time since
;;; only one regexp search is used where as before up to six were used.
;;; Someone suggested something similar (thanks) but I have since lost their
;;; address.

;;; Internal variable.
(defvar rmailgen-alist
  ;; Parse `From' line of mail messages.
  (list
   '("^From:[ \t\"]*" . (rmailgen-output-name))
   ))

;;; Attach ourselves to alist variable so that rmail-output finds us.
(setq rmail-output-file-alist
      (append
       rmail-output-file-alist
       rmailgen-alist))

;;; This is the NAME-EXP of `rmail-output-file-alist'.  In this case it is an
;;; expression that returns a folder-name as a string.
(defun rmailgen-output-name (&optional second-pass)

  ;; Returns suggested output or carbon-copy folder-name as lower-case string
  ;; or nil if string the same as user login name or contains a `/'.  Will
  ;; check `To:' line if `From:' line doesn't reveal any guesses.
  ;; Argument SECOND-PASS used internally for recursive test.

  (let ((case-fold-search t) ; ignore case
        folder-name)

    ;; Test for the three types of addresses.
    (if (or

         ;; 1. Joe Blow <foo@bar.gar.edu>
         ;;    Retrieves login name `foo' in first parenthetic expression.
         (looking-at "[^\n<]+<\\([^ \t\n\f@%()<>\"]+\\)[@%]\\([^ \t\n\f<>()]+\\)>")

         ;; 2. foo@bar.gar.edu (Joe Blow)
         ;;    Retrieves username in first parenthetic expression.
         (looking-at "\\([^ \t\n\f@%()<>,\"]+\\)\\([@%]\\([^ \t\n\f<>()]+\\)\\)?")

         ;; 3. <joe>
         ;;    Retrieves login name in first parenthetic expression.
         (looking-at "<\\([^>\"]+\\)>"))

        (progn

          ;; Extract folder-name from first parenthetic expression match.
          (setq folder-name (buffer-substring (match-beginning 1)
                                              (match-end 1)))

          ;; Check if we should accept folder-name.
          (cond ((or
                  ;; Check rmailgen-dont-output-to-names first...
                  (and rmailgen-dont-output-to-names
                       (string-match rmailgen-dont-output-to-names folder-name))
                  ;; ...then check user-login-name.
                  (and (user-login-name)
                       (string-match (user-login-name) folder-name)))

                 ;; User login and folder-name match.  Message may be
                 ;; carbon-copy of outgoing one.  If `CC:' (or `BCC:') used in
                 ;; sending, user's name will appear in `From:' line of copy
                 ;; sent back to user.  In such a case we wish to parse
                 ;; contents of `To:' line.
                 (if (not second-pass) ; Return nil if second pass.
                     (save-excursion
                       (goto-char (point-min))
                       (if (re-search-forward "^To:[ \t]*" nil t)
                           ;; Call this defun one more time with non-nil arg.
                           (rmailgen-output-name t)))))

                ((string-match "/" folder-name)
                 ;; Contains a `/', return nil.  For strange usernames like
                 ;; `company/G=JOE/I=M/S=BLOW' easier to set explicit output.
                 nil)

                ;; Return correct case.
                (t
                 ;; Use generated name and append `.gz' if necessary.
                 (rmailgen-case-append folder-name)))))))

 
;;;; SECTION C.
;;;; Generate an FCC filename based on "From" and "To" fields when replying to
;;;; a message.

;;; Algorithm:
;;;
;;; Part of `mail-setup-hook' as called by `mail-setup' of ../lisp/sendmail.el.
;;;
;;; If replying to a message then `mail-reply-buffer' will be non-nil.  Go to
;;; that buffer and use `rmail-output-file-alist' to search the "From" and
;;; "To" fields for a suggestion for an archive file-name (see above).
;;;
;;; If suggestion found then search mail buffer, which has already been setup
;;; by `mail-setup', for the old FCC field which will contain the value of
;;; `mail-archive-file-name'.  Remove this and replace with the suggestion
;;; prepended with `rmailgen-default-directory'.

(defun rmailgen-get-archive-file-name ()

  ;; Inserts guess for archive file name, based on contents variable
  ;; `rmail-output-file-alist', in mail buffer if `mail-archive-file-name' and
  ;; `rmailgen-archive-file-name' are non-nil and if replying to a message.
  ;; Part of `mail-setup-hook'.
  (if (and mail-archive-file-name
           rmailgen-archive-file-name
           mail-reply-buffer)
      (let (answer
            ;; Generate guess for archive name only if
            ;; rmailgen-archive-file-name is t.
            (tail
             (if (eq rmailgen-archive-file-name t)
                 rmail-output-file-alist
               (delete rmailgen-alist rmail-output-file-alist))))
        (save-excursion
          ;; Search the rmail buffer.
          (set-buffer mail-reply-buffer)
          ;; Suggest a file based on a pattern match.
          (while (and tail (not answer))
            (save-excursion
              (goto-char (point-min))
              (if (re-search-forward (car (car tail)) nil t)
                  (setq answer (eval (cdr (car tail)))))
              (setq tail (cdr tail)))))
        ;; If we found a suggestion and we can use it, then insert in
        ;; mail buffer.
        (if (and answer
                 ;; Does archive file need to pre-exist?
                 (or
                  ;; If `rmailgen-archive-only-if-exists' is nil, then
                  ;; outer and-form is t.
                  (not rmailgen-archive-only-if-exists)
                  ;; `rmailgen-archive-only-if-exists' is *not* nil.
                  ;; Could be t or non-nil (e.g. 'ask).
                  ;; Check existence of archive file.
                  (or
                   ;; If archive file exists, then outer and-form is t.
                   (file-exists-p
                    (concat (or rmailgen-default-directory "~/") answer))
                   ;; Archive file does not exist.  Last chance.
                   ;; If `rmailgen-archive-only-if-exists' is non-nil (e.g.,
                   ;; 'ask) then ask, otherwise outer and-form is nil.
                   (and (not (eq rmailgen-archive-only-if-exists t))
                        (y-or-n-p
                         (concat "\"" answer
                                 "\" does not exist, FCC to it anyway? "))))))

            (save-excursion
              (goto-char (point-min))
              ;; Search and remove old FCC.  We know this is there because
              ;; `mail-archive-file-name' is non-nil so `mail-setup' has
              ;; inserted it already.
              (re-search-forward
               (concat "^FCC: " (regexp-quote mail-archive-file-name) "\n"))
              (delete-region (match-beginning 0) (match-end 0))
              ;; Append `.gz' if necessary.
              (and rmailgen-gzip-file-name
                   (not (string-match "\\.gz$" answer))
                   (setq answer (concat answer ".gz")))
              ;; Insert new FCC.
              (insert "FCC: " (or rmailgen-default-directory "~/") answer "\n"))))))

;;; Put on hook.
(add-hook 'mail-setup-hook 'rmailgen-get-archive-file-name)

 
;;;; SECTION D.
;;; Internal Utilities
(defun rmailgen-case-append (obj)
  ;; Downcase OBJ if rmailgen-downcase is t,
  ;; Upcase OBJ if rmailgen-downcase is nil,
  ;; otherwise do nothing.
  ;; Append `.gz' to OBJ if rmailgen-gzip-file-name is t.
  (cond ((eq rmailgen-downcase t)
         (setq obj (downcase obj)))
        ((eq rmailgen-downcase nil)
         (setq obj (upcase obj))))
  ;; Append `.gz' if necessary.
  (if (and rmailgen-gzip-file-name
           (not (string-match "\\.gz$" obj)))
      (setq obj (concat obj ".gz")))
  ;; Return it.
  obj)

 
;;;; BUG REPORTS
;;; Uses Barry A. Warsaw's reporter.el

(defconst rmailgen-version "2.1"
  "Revision number of rmailgen.el -- generates RMAIL output and FCC names.
Type \\[rmailgen-submit-report] to send a bug report.  Available via
anonymous ftp in

   /roebling.poly.edu:/pub/lisp/rmailgen.gz
   /archive.cis.ohio-state.edu:/pub/gnu/emacs/elisp-archive/misc/rmailgen.Z")

(defun rmailgen-submit-report ()
  "Submit via reporter.el a bug report on rmailgen."
  (interactive)

  ;; Since we have to be using v19 this should work.
  (require 'reporter)

  (reporter-submit-bug-report
   "dodd@roebling.poly.edu"                  ; address
   (concat "rmailgen.el " rmailgen-version)  ; pkgname
   ;; varlist
   (list 'load-path ; useful
         'rmailgen-default-directory
         'rmailgen-downcase
         'rmailgen-archive-file-name
         'rmailgen-gzip-file-name
         'rmailgen-archive-only-if-exists
         'rmail-output-file-alist
         )
   nil nil                                         ; hooks
   (concat "Yo! Larry,")))                         ; salutation

 
;;;; Provide this package.
(provide 'rmailgen)

;;; rmailgen.el ends here
