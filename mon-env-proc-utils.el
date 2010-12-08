;;; mon-env-proc-utils.el --- procedures for interacting w/ process environment
;; -*- mode: EMACS-LISP; -*-

;;; ================================================================
;; Copyright © 2010 MON KEY. All rights reserved.
;;; ================================================================

;; FILENAME: mon-env-proc-utils.el
;; AUTHOR: MON KEY
;; MAINTAINER: MON KEY
;; CREATED: 2010-11-23T19:12:48-05:00Z
;; VERSION: 1.0.0
;; COMPATIBILITY: Emacs23.*
;; KEYWORDS: external, execute, extensions, lisp, emacs,

;;; ================================================================

;;; COMMENTARY: 

;; =================================================================
;; DESCRIPTION:
;; mon-env-proc-utils provides procedures for interacting w/ process environment
;;
;; FUNCTIONS:►►►
;; `mon-get-env-vars-symbols', `mon-get-env-vars-emacs',
;; `mon-get-env-vars-strings', `mon-get-emacsd-paths', `mon-get-proc-w-name',
;; `mon-get-sys-proc-list', `mon-insert-sys-proc-list',
;; `mon-get-process', `mon-get-system-specs', 
;; `mon-escape-string-for-cmd',
;; `mon-cmd', `mon-terminal', `mon-firefox', `mon-conkeror',
;;
;; FUNCTIONS:◄◄◄
;;
;; MACROS:
;;
;; METHODS:
;;
;; CLASSES:
;;
;; CONSTANTS:
;;
;; FACES:
;;
;; VARIABLES:
;;
;; GROUPS:
;;
;; ALIASED/ADVISED/SUBST'D:
;;
;; DEPRECATED:
;;
;; RENAMED:
;;
;; MOVED:
;; `mon-get-system-specs'                            <- mon-utils.el
;; `mon-get-env-vars-symbols'                        <- mon-utils.el
;; `mon-get-env-vars-strings'                        <- mon-utils.el
;; `mon-get-env-vars-emacs'                          <- mon-utils.el
;; `mon-get-sys-proc-list'                           <- mon-utils.el
;; `mon-insert-sys-proc-list'                        <- mon-utils.el
;; `mon-get-proc-w-name'                             <- mon-utils.el
;; `mon-get-process'                                 <- mon-utils.el
;; `mon-escape-string-for-cmd'                       <- mon-utils.el
;; `mon-terminal'                                    <- mon-utils.el
;; `mon-cmd'                                         <- mon-utils.el
;; `mon-firefox'                                     <- mon-utils.el
;; `mon-conkeror'                                    <- mon-utils.el
;; TODO:
;;
;; NOTES:
;;
;; SNIPPETS:
;;
;; REQUIRES:
;;
;; THIRD-PARTY-CODE:
;;
;; URL: http://www.emacswiki.org/emacs/mon-env-proc-utils.el
;; FIRST-PUBLISHED:
;;
;; EMACSWIKI: { URL of an EmacsWiki describing mon-env-proc-utils. }
;;
;; FILE-CREATED:
;; <Timestamp: #{2010-11-23T19:12:48-05:00Z}#{10472} - by MON KEY>
;;
;; =================================================================

;;; LICENSE:

;; =================================================================
;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; =================================================================
;; Permission is granted to copy, distribute and/or modify this
;; document under the terms of the GNU Free Documentation License,
;; Version 1.3 or any later version published by the Free Software
;; Foundation; with no Invariant Sections, no Front-Cover Texts,
;; and no Back-Cover Texts. A copy of the license is included in
;; the section entitled ``GNU Free Documentation License''.
;; 
;; A copy of the license is also available from the Free Software
;; Foundation Web site at:
;; (URL `http://www.gnu.org/licenses/fdl-1.3.txt').
;;; ==============================
;; Copyright © 2010 MON KEY 
;;; ==============================

;;; CODE:

 
(eval-when-compile (require 'cl))

(unless (and (intern-soft "*IS-MON-OBARRAY*")
             (bound-and-true-p *IS-MON-OBARRAY*))
(setq *IS-MON-OBARRAY* (make-vector 17 nil)))

(declare-function w32-shell-execute "w32fns.c")

;;; ==============================
;;; :PREFIX "mgss-"
;;; :CREATED <Timestamp: #{2009-12-09T11:54:07-05:00Z}#{09503} - by MON>
(defun mon-get-system-specs (&optional insrtp intrp)
  "Return the output of shell-command 'uname -a'.\n
When called-interactively or INSRTP is non-nil insert at point.\n
Does not move point.\n
:EXAMPLE\n\n\(mon-get-system-specs\)\n
:SEE-ALSO `system-name', `mon-get-env-vars-strings', `mon-get-env-vars-symbols',
`mon-get-env-vars-emacs', `mon-get-proc-w-name', `mon-get-sys-proc-list',
`mon-insert-sys-proc-list', `read-envvar-name'.\n►►►"
  (interactive "i\np")
  (if (executable-find "uname")
      (let ((mgss-unm (shell-command-to-string "uname -a")))
        (setq mgss-unm (replace-regexp-in-string "[[:blank:]]+" " " mgss-unm))
        (if (or insrtp intrp)
            (save-excursion 
              (newline)
              (princ mgss-unm (current-buffer)))
          mgss-unm))
    (when (eq system-type 'windows-nt)
      (message "The command `uname -a' is not available"))))
;;
;;; :TEST-ME (mon-get-system-specs)
;;; :TEST-ME (mon-get-system-specs t)

;;; ==============================
;;; :PREFIX "mgevsym-"
;;; :CREATED <Timestamp: #{2010-01-18T20:20:35-05:00Z}#{10032} - by MON>
(defun mon-get-env-vars-symbols ()
  "Return a list of symbols for current-process' environmental-variables.\n
like `mon-get-env-vars-strings' but returns symbols instead of strings.\n
:EXAMPLE\n\n(mon-get-env-vars-symbols)\n
:SEE-ALSO `mon-get-env-vars-strings', `mon-get-system-specs', 
`mon-help-emacs-introspect', `process-environment', `initial-environment',
`getenv', `setenv', `read-envvar-name'.\n►►►"
  (interactive) 
  (let ((mgevsym-proc-env process-environment)
        mgevsym-gthr)
    (dolist (mgevsym-D-1 mgevsym-proc-env mgevsym-gthr)
      (when (string-match "\\(.*\\)=.*" mgevsym-D-1)
        (push 
         (car (read-from-string mgevsym-D-1 (match-beginning 0) (match-end 1)))
         mgevsym-gthr)))))

;;; ==============================
;;; :PREFIX "mgevs-"
;;; :COURTESY :FILE emacs/lisp/env.el :WAS `read-envvar-name' 
;;; :CREATED <Timestamp: #{2009-10-16T15:29:37-04:00Z}#{09425} - by MON KEY>
(defun mon-get-env-vars-strings (&optional as-strings insrtp intrp)
  "Return a list strings for the current process' enviromental variables.
When AS-STRINGS is non-nil or called with a prefix-arg return as strings.
When insrtp or called-interactively insert returned vars at point.\n
:EXAMPLE\(mon-get-env-vars-strings\)\n
:SEE-ALSO `mon-get-env-vars-symbols', `mon-get-env-vars-emacs',
`mon-get-system-specs', `mon-help-emacs-introspect', `process-environment',
`initial-environment', `setenv', `getenv', `read-envvar-name'.\n►►►"
  (interactive "P\ni\np")
  (let ((mgevs-getenvs
         (mapcar #'(lambda (mgevs-L-1)
                     (let ((mgevs-L-1-str 
                            (substring mgevs-L-1 0 (string-match-p "=" mgevs-L-1))))
                       (if (multibyte-string-p mgevs-L-1-str)
                           (decode-coding-string mgevs-L-1-str locale-coding-system t)
                         mgevs-L-1-str)))
                 ;; :NOTE Why did this use append here?
                 (append process-environment))))
    (setq mgevs-getenvs (sort mgevs-getenvs #'string<))
    (when as-strings
      (setq mgevs-getenvs (concat "\"" (mapconcat #'identity mgevs-getenvs "\"\n\"") "\"")))
    (cond ((or insrtp intrp)
           ;; (mapc (lambda (x) (prin1 x (current-buffer))) mgevs-getenvs)
           (if as-strings
               (prin1 mgevs-getenvs (current-buffer))
             (princ mgevs-getenvs (current-buffer))))
          (t (if as-strings
                 (prin1 mgevs-getenvs)
               mgevs-getenvs)))))
;;
;;; :TEST-ME (mon-get-env-vars-strings)
;;; :TEST-ME (mon-get-env-vars-strings t)
;;; :TEST-ME (mon-get-env-vars-strings nil nil)
;;; :TEST-ME (mon-get-env-vars-strings  t t)
;;; :TEST-ME (mon-get-env-vars-strings  nil t)
;;; :TEST-ME (princ (mon-get-env-vars-strings t) (current-buffer))
;;; :TEST-ME (prin1 (mon-get-env-vars-strings t) (current-buffer))

;;; ==============================
;;; :PREFIX "mgeve-"
;;; :CREATED <Timestamp: #{2010-01-22T16:29:45-05:00Z}#{10035} - by MON>
(defun mon-get-env-vars-emacs (&optional insrtp intrp)
  "Return an list of the current environmental variables of the running Emacs.\n
Alist Keys have the form:\n
 \(:ENV-VAR \"VALUE-STRING\"\)
 \(ENV_VAR \"VALUE-STRING\"\)\n
For each colon prefixed :ENV-VAR key there is an equivalent symbol key. Each has
an identical value.\n
:EXAMPLE\n\n\(mon-get-env-vars-emacs\)\n
\(mapcar 'car \(mon-get-env-vars-emacs\)\)\n
:NOTE MON stores some local variables in `*mon-misc-path-alist*'. When this
symbol is present values associated with the key ``the-emacs-vars'' will also included
when generating the return value.\n
When called-interactively pretty-print return value in buffer named
\"*MON-EMACS-ENV-VARS*\".\n
:SEE info node `(emacs)General Variables'.\n
:SEE-ALSO `mon-get-env-vars-strings', `mon-get-env-vars-symbols'
`mon-get-system-specs', `mon-insert-sys-proc-list',
`mon-get-sys-proc-list', `mon-get-proc-w-name', `mon-get-process',
`mon-help-process-functions', `mon-help-emacs-introspect', `emacs-pid',
`process-environment', `initial-environment', `getenv', `setenv'.\n►►►"
  (interactive "i\np")
  (let ((mgeve-vars (mon-intersection 
                         (mon-get-env-vars-symbols)
                         (append 
                          ;; MON-LOCAL-VARS                          
                          (when (bound-and-true-p *mon-misc-path-alist*)
                            (cadr (assq 'the-emacs-vars *mon-misc-path-alist*)))
                          (do* ((i '(;; :LENNART-EMACS-W32-VARS
                                     EMACSCLIENT_STARTING_SERVER EMACS_SERVER_FILE
                                     ;; :GNUS-MAIL
                                     ;; MH NNTPSERVER REPLYTO SAVEDIR SMTPSERVER  MAIL
                                     ;; ORGANIZATION VERSION_CONTROL HISTFILE EMAIL EMACS_UNIBYTE CDPATH
                                     ;; :STANDARD-EMACS-VARS 
                                     emacs_dir INFOPATH ESHELL  INCPATH
                                     EMACSLOADPATH EMACSDATA EMACSPATH EMACSDOC SHELL TERM)   i)
                                (j (pop i) (pop i))
                                (k))
                               ((null j) (nreverse k))
                            (when (getenv (format "%s" j))(push j k))))
                         nil t))
        gthr-env-vars)
    (dolist (mgeve-D-1 mgeve-vars (setq gthr-env-vars (nreverse gthr-env-vars)))
      (let ((mgeve-D-lcl-is-var (car (memq mgeve-D-1 mgeve-vars)))
            mgeve-D-lcl-var-prs)
        (unless (null mgeve-D-lcl-is-var)
          (let* ((mgeve-D-lcl2-frmt-var (upcase (format "%s" mgeve-D-lcl-is-var)))
                 (mgeve-D-lcl2-get-var (file-truename (getenv mgeve-D-lcl2-frmt-var))))
            (push `(,(car (read-from-string (subst-char-in-string 95 45 (concat ":" mgeve-D-lcl2-frmt-var))))
                    . ,mgeve-D-lcl2-get-var)
                  mgeve-D-lcl-var-prs)
            (push `(,(car (read-from-string mgeve-D-lcl2-frmt-var)) . ,mgeve-D-lcl2-get-var)
                  mgeve-D-lcl-var-prs)))
        (setq mgeve-D-lcl-var-prs (nreverse mgeve-D-lcl-var-prs))
        (push (car mgeve-D-lcl-var-prs) gthr-env-vars)
        (push (cadr mgeve-D-lcl-var-prs) gthr-env-vars)))
    insrtp ;; Not currently evaluating INSRTP.
    (when intrp 
      (let ((mgeve-get-bfr (get-buffer-create "*MON-EMACS-ENV-VARS*")))
        (with-current-buffer (buffer-name mgeve-get-bfr)
          (erase-buffer)
          (pp-display-expression gthr-env-vars (buffer-name mgeve-get-bfr))
          (mon-g2be -1) 
          (insert ";; :MON-EMACS-ENV-VARS output from:\n;; (mon-get-env-vars-emacs nil t)\n;;\n"))))))
;;
;;; :TEST-ME (mon-get-env-vars-emacs)
;;; :TEST-ME (mapcar 'car (mon-get-env-vars-emacs))

;;; ==============================
;;; :PREFIX "mgspl"
;;; :NOTE MON recently found the :FILE proced.el 
;;;       Some of this might be accomplished with that excellent package.
;;; :CHANGESET 1708 <Timestamp: #{2010-04-12T17:02:32-04:00Z}#{10151} - by MON KEY>
;;; :CHANGESET 1703 <Timestamp: #{2010-04-07T14:39:56-04:00Z}#{10143} - by MON KEY>
;;; :CREATED <Timestamp: #{2009-10-16T15:49:07-04:00Z}#{09425} - by MON KEY>
(defun mon-get-sys-proc-list (&optional intrp)
  "Return a full lisp list of current system-proceses.\n
When called-interactively return results in buffer \"*MON-GET-SYS-PROCESSES*\".\n
:EXAMPLE:\n\n(mon-get-sys-proc-list)\n\n\(mon-get-sys-proc-list t\)\n
:SEE-ALSO `mon-get-process',`mon-insert-sys-proc-list',
`mon-help-process-functions', `mon-get-env-vars-strings',
`mon-get-env-vars-symbols', `mon-get-env-vars-emacs', `mon-get-system-specs',
`mon-help-emacs-introspect', `emacs-pid'.\n►►►"
  (interactive "p")
  (let (mgspl)
    (dolist (sys-proc (list-system-processes) (setq mgspl (nreverse mgspl)))
      (push (process-attributes sys-proc) mgspl))
    (when intrp 
      (pp-display-expression mgspl "*MON-GET-SYS-PROCESSES*"))))
;;
;;; :TEST-ME (mon-get-sys-proc-list)
;;; :TEST-ME (mon-get-sys-proc-list t)

;;; ==============================
;;; :PREFIX "mispl-"
;;; :NOTE MON recently found the :FILE proced.el 
;;;       Some of this might be accomplished with that excellent package.
;;; CREATED: <Timestamp: #{2009-10-16T15:54:29-04:00Z}#{09425} - by MON KEY>
(defun mon-insert-sys-proc-list ()
  "Insert a full lisp list of current system-proceses at point.
Does not move point.\n
:SEE-ALSO `mon-get-process', `mon-get-sys-proc-list', 
`mon-insert-sys-proc-list', `emacs-pid',
`mon-get-env-vars-strings', `mon-get-env-vars-symbols'
`mon-get-env-vars-emacs', `mon-get-system-specs'
`mon-help-emacs-introspect'.\n►►►"
  (interactive)
  (save-excursion
    (newline)
    (mapc #'(lambda (mispl-L-1)
              (princ (concat ";;;\n" (pp mispl-L-1))(current-buffer)))
          (mon-get-sys-proc-list))))

;;; ==============================
;;; :NOTE MON recently found the :FILE proced.el 
;;;       Some of this might be accomplished with that excellent package.
;;; :CREATED <Timestamp: #{2009-10-16T16:34:48-04:00Z}#{09425} - by MON KEY>
(defun mon-get-proc-w-name (comm)
  "Return list of `process-attributes' lists for Command name COMM.
COMM (a string) is an executable name. 
On w32 it is not required give the .exe suffix.\n
:EXAMPLE\n\(mon-get-proc-w-name \"emacs\"\)\n
:SEE-ALSO `mon-get-process', `mon-get-sys-proc-list', `mon-get-sys-proc-list',
`mon-help-process-functions'.\n►►►"
  (let (fnd-proc gthr)
    (mapc #'(lambda (x)
              (let ((t-aso (assoc 'comm x)))
                (if (string-match comm (cdr t-aso)) ;"emacs.exe"
                    (setq fnd-proc (cons x fnd-proc)))))
          (mon-get-sys-proc-list))
    fnd-proc))
;;
;;; :TEST-ME (mon-get-proc-w-name "emacs")
;;; :TEST-ME (mon-get-proc-w-name "svchost")
;;; :TEST-ME (mon-get-proc-w-name "bubba")
;;; :TEST-ME (mon-get-proc-w-name (invocation-name))
                               
;;; ==============================
;;; :NOTE Built to test for "mysql" command before looking for a comint.
;;;       MON recently found the :FILE proced.el 
;;;       Some of this might be accomplished with that excellent package.
;;; :NOTE `mon-get-process' uses `flet' cl--find-if -> `find-if'
;;; :CREATED <Timestamp: #{2009-12-09T20:02:30-05:00Z}#{09503} - by MON>
(defun mon-get-process (&optional proc-comm)
  "Find the process-id for process invoked with command.
When PROC-COMM is non-nil it is a command name to find.
Default is value of \(invocation-name\).\n
:NOTE This function is GNU/Linux centric! However, unlike `mon-get-proc-w-name'
this function can match multiple processes with identical invocation commands.\n
:EXAMPLE\n\n\(if IS-W32-P 
    \(mon-get-process \(concat \(invocation-name\) \".exe\"\)\)
    \(mon-get-process \(invocation-name\)\)\)\n
:SEE-ALSO `mon-insert-sys-proc-list', `mon-get-sys-proc-list',
`mon-help-process-functions'.\n►►►"
  (interactive)
  (let* ((pmatch)
         (prc-l (nreverse (list-system-processes)))
         (map-prc #'(lambda (u) 
                      (flet ((cl--fi (cl-pred cl-list &rest cl-keys) ;; `find-if'
                               (apply 'find nil cl-list :if cl-pred cl-keys)))
                        (let ((got-it 
                               (cl--fi  #'(lambda (z) 
                                            (and (eql (car z) 'comm)
                                                 (equal (cdr z) 
                                                        (if proc-comm 
                                                            proc-comm 
                                                            (invocation-name)))))
                                        (process-attributes u))))
                          (when got-it (if (not prc-l)
                                           (push `(,u ,got-it) prc-l)
                                           (push u pmatch))))))))
    (mapc map-prc prc-l)
    (if pmatch
        (progn 
          (setq prc-l nil)
          (mapc map-prc pmatch)
          ;; :WAS (if prc-l prc-l))
          (or prc-l))
        pmatch)))
;;
;;; :TEST-ME (if IS-W32-P 
;;;              (mon-get-process (concat (invocation-name) ".exe"))
;;;              (mon-get-process (invocation-name)))

;;; ==============================
;;; :PREFIX "mesfc-"
;;; :CREATED <Timestamp: #{2009-10-22T17:58:11-04:00Z}#{09434} - by MON>
(defun mon-escape-string-for-cmd (unescape a-string &rest more-strings)
  "Return A-STRING escaped for passing to the w32 cmd.exe e.g `/' -> `\\\\'.
When MORE-STRINGS is non-nil escape these also.\n
When UNESCAPE is non-nil unescape A-STRING and/or MORE-STRINGS.\n
:SEE-ALSO `convert-standard-filename', `w32-shell-dos-semantics'.
`w32-quote-process-args', `mon-exchange-slash-and-backslash',
`mon-escape-lisp-string-region', `mon-unescape-lisp-string-region'.\n►►►"
  (let ((mesfc-mr-str (if more-strings
                          (cons a-string more-strings)
                        a-string))
        (mesfc-rplc (if unescape
                        #'(lambda (mesfc-L-1)
                            (replace-regexp-in-string  "\\\\" "/" mesfc-L-1))
                      #'(lambda (mesfc-L-2)
                          (replace-regexp-in-string "/" "\\\\" mesfc-L-2)))))
    (if (consp mesfc-mr-str)
        (mapconcat mesfc-rplc mesfc-mr-str " ")
      (funcall mesfc-rplc mesfc-mr-str))))

;;; ==============================
(defun mon-terminal ()
  "When `gnu-linuxp' launch a terminal.\n
When `win32p' invoke Cygwin Bash in cmd console.\n
:SEE-ALSO `mon-cmd' which when win32p returns the NT Command console.
`mon-shell', `mon-make-shell-buffer', `w32shell-cmd-here', `w32shell-cmd',
`w32shell-explorer', `mon-help-process-functions'.\n►►►"
  (interactive)
  (cond  ((and (intern-soft "IS-BUG-P" obarray)         ;; *IS-MON-OBARRAY*
               (bound-and-true-p IS-BUG-P))
          (message "You don't have the goods for this"))
         ((and (intern-soft "IS-MON-P-W32" obarray)     ;; *IS-MON-OBARRAY*
               (bound-and-true-p IS-MON-P-W32))
          (w32-shell-execute "open" "cmd.exe" "C:\\Cygwin.bat"))
         ((and (intern-soft "IS-MON-P-GNU" obarray)     ;; *IS-MON-OBARRAY*
               (bound-and-true-p IS-MON-P-GNU))
          (shell-command "mrxvt"))
         (t (message ":FUNCTION `mon-terminal' -- check the manual :P"))))

;;; ==============================
(defun mon-cmd ()
  "When `win32p' launch the NT Command console.\n
When `gnu-linuxp' launch a terminal \(mrxvt\).\n
:SEE `mon-terminal' which when `win32p' gives a Cygwin bash shell wrapped
in a cmd console.\n
:SEE-ALSO `mon-shell', `mon-make-shell-buffer', `w32shell-cmd-here',
`w32shell-cmd', `w32-shell-execute', `w32shell-explorer', `shell-command',
`shell', `mon-help-process-functions'.\n►►►"
  (interactive)
  (cond ((and (intern-soft "IS-W32-P" obarray)           ;; *IS-MON-OBARRAY*
              (bound-and-true-p IS-W32-P))
         (w32-shell-execute "open" "cmd"))
        ((and (intern-soft "IS-GNU-P" obarray)           ;; *IS-MON-OBARRAY*
              (bound-and-true-p IS-GNU-P))
         ;; :WAS (shell-command "terminal")
         (shell-command "mrxvt"))
        (t (message ":FUNCTION `mon-cmd' -- not sure which terminal/console to use"))))

;;; ==============================
(defun mon-firefox (url &optional intrp)
  "Jump to the running firefox and open URL in new tab.\n
:SEE-ALSO `browse-url-firefox-program',`mon-conkeror',
`browse-url-generic-program', `browse-url-browser-function',
`browse-url-generic'.\n►►►"
  (interactive "i\np")
  (when intrp
    (setq url (read-string (concat ":FUNCTION `mon-firefox' "
                                   "-- which URL: "))))
  (browse-url-firefox url))

;;; ==============================
(defun mon-conkeror (url)
  "Launch or find a running conkeror web browser with URL.\n
:NOTE To enusre Emacs gets existing conkeror process put following in
conkeror-rc file:
 url_remoting_fn = load_url_in_new_buffer;
 require\(\"clicks-in-new-buffer.js\");\n
:SEE-ALSO `mon-firefox', `browse-url-firefox-program',
`browse-url-generic-program',`browse-url-browser-function',
`browse-url-generic'.\n►►►"
  (interactive "s:FUNCTION `mon-conkeror' -- what URL: ")
  (eval-when-compile (require 'browse-url))
  (if (string-match "conkeror" browse-url-generic-program)
      (cond ;; :NOTE These conditionals are here so we can adjust as needed.
       ((and (intern-soft "IS-MON-P-W32" obarray)       ;; *IS-MON-OBARRAY*
             (bound-and-true-p IS-MON-P-W32))
        (browse-url-generic url))
       ((and (intern-soft "IS-MON-P-GNU" obarray)       ;; *IS-MON-OBARRAY*
             (bound-and-true-p IS-MON-P-GNU))
        (browse-url-generic url))
       ((and (intern-soft "IS-BUG-P" obarray)           ;; *IS-MON-OBARRAY*
             (bound-and-true-p IS-BUG-P))
        (browse-url-generic url))
       ((and (intern-soft "IS-MON-SYSTEM-P" obarray)    ;; *IS-MON-OBARRAY*
             (bound-and-true-p IS-MON-SYSTEM-P))
        (browse-url-generic url))
       ((and (intern-soft "IS-NOT-A-MON-SYSTEM" obarray)  ;; *IS-MON-OBARRAY*
             (bound-and-true-p IS-NOT-A-MON-SYSTEM))
        (browse-url-generic url))
       (t (error (concat ":FUNCTION `mon-conkeror' "
                         "-- can not grok your system and/or "
                         "conkeror not set as `browse-url-generic-program'"))))
    (error (concat ":FUNCTION `mon-conkeror' "
                   "-- conkeror not set `browse-url-generic-program'"))))

;;; ==============================
(provide 'mon-env-proc-utils)
;;; ==============================

 
;; Local Variables:
;; mode: EMACS-LISP
;; coding: utf-8
;; generated-autoload-file: "./mon-loaddefs.el"
;; End:

;;; ====================================================================
;;; mon-env-proc-utils.el ends here
;;; EOF
