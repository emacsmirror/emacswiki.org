;;; mon-site-local-defaults.el --- load/init fncns, vars, const for MON packages
;; -*- mode: EMACS-LISP; no-byte-compile: t; -*-

;;; ================================================================
;; Copyright © 2009, 2010 MON KEY. All rights reserved.
;;; ================================================================

;; FILENAME: mon-site-local-default.el
;; AUTHOR: MON KEY
;; MAINTAINER: MON KEY
;; CREATED: 2009-08-11T16:48:54-04:00Z
;; VERSION: 1.0.0
;; COMPATIBILITY: Emacs23.*
;; KEYWORDS: local, environment, installation

;;; COMMENTARY: 
;;; ================================================================
;;; DESCRIPTION: Example configuration of mon-site-local-defaults file.
;;; mon-site-local-defaults provides functions and vars that encapsulate 
;;; user data that is site local but used in public functions and across 
;;; systems.
;;;
;;; FUNCTIONS:►►►
;;; `mon-system-type-conditionals', `mon-user-name-conditionals'
;;; `mon-user-system-conditionals-TEST'
;;; `mon-build-misc-path-example', `mon-build-mon-emacsd-example'
;;; `mon-string-wonkify', `mon-build-user-name-example', 
;;; `mon-build-user-name-example-TEST'
;;; FUNCTIONS:◄◄◄
;;;
;;; CONSTANTS:
;;; `*mon-emacsd*', `*MON-NAME*', `*BUG-NAME*', `*MON-ORG-NAME*'
;;;
;;; VARIABLES:
;;; `*mon-misc-path-alist*'
;;;
;;; NOTES: 
;; :W32-RELATED-JUNK: 
;; :SEE :FILE w32-fns.el :FILE w32-vars.el
;; `w32-version'      <FUNCTION> (w32-version)
;; `w32-system-shell-p' (w32-system-shell-p "cmd.exe")
;; `w32-shell-name' (w32-shell-name)
;; `w32-init-info'    <FUNCTION> (w32-init-info)
;; `w32-list-locales' <FUNCTION> (w32-list-locales)
;; `convert-standard-filename' (convert-standard-filename (getenv "HOME"))
;; `w32-shell-dos-semantics'   (w32-shell-dos-semantics)
;; `w32-check-shell-configuration' (w32-check-shell-configuration)
;; (getenv "SHELL") (getenv "ESHELL")
;; `w32-system-shells' <VARIABLE>
;; `w32-allow-system-shell'
;; `explicit-shell-file-name' <VARIABLE>
;; `w32-quote-process-args'   <VARIABLE>
;; `process-environment' returns a list of this process' environment.
;; (getenv "SystemDrive") ;=> "C:" on w3
;; 
;; URL: http://www.emacswiki.org/emacs/mon-site-local-defaults.el
;; FIRST-PUBLISHED: <Timestamp: #{2009-09-23T14:44:53-04:00Z}#{09393} - by MON>
;;
;; FILE-CREATED:
;; <Timestamp: #{2009-08-11T16:48:54-04:00Z}#{09332} - by MON KEY>
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
;; Copyright © 2009, 2010 MON KEY 
;;; ==============================

;;; CODE:

;;; ==============================
;;; :CREATED <Timestamp: #{2010-02-10T17:17:59-05:00Z}#{10063} - by MON KEY>
;;; ==============================
;;; :CREATED <Timestamp: #{2010-02-10T17:17:59-05:00Z}#{10063} - by MON KEY>
(defun mon-build-user-name-example (name-count &optional w-this-var bind-var force-bind)
  "Return pre-formatted list of username  pairs for use with mon packages.\n
`*BUG-NAME*', `*MON-NAME*', `*MON-ORG-NAME*', etc.
When `bind-var' is non-nil if `' is unbound bind it.\n
When `force-bind' is non-nil force the binding even if `' bound.\n
Elements of the list might have a form such as this:
\(\(1 \"Short-First Last\"\)\n \(2 \"First\"\) \ \(3 \"FML\"\) 
 \(4 \"Full-First Last\"\) \n \(5 \"fl\"\) \n \(6 \"MONIKER NAME\"\) 
 \(7 \"MONIKER\"\) \n \(8 \"MONIKER_NAME\"\) \n \(9 \"moniker_name\"\)\)\n
:EXAMPLE\n\n(mon-build-user-name-example 5)\n
\(mon-build-user-name-example-TEST\)
:SEE-ALSO `mon-build-user-name-example-TEST', `mon-get-mon-emacsd-paths',
`mon-build-misc-path-example' `mon-build-mon-emacsd-example'.\n►►►"
  (let ((wonky-user (mon-string-wonkify
                     (concat (if w-this-var 
                                 (cond ((equal (symbol-name w-this-var) "*MON-NAME*") 
                                        user-login-name)
                                       ((equal (symbol-name w-this-var) "*BUG-NAME*") 
                                        (format "BUG-%s" user-login-name))
                                       (t "some-name"))
                                 user-login-name)
                             (when (> (length user-full-name) 0)
                               (concat "-" (length user-full-name)))) name-count))
        wonky
        (wonky-step 0))
    (while wonky-user
      (setq wonky-step (1+ wonky-step))
      (push `(,wonky-step ,(format "<%s-%s-%d>" 
                                   (pop wonky-user) 
                                   (car (mon-string-wonkify "NAMEFORM" 1)) 
                                   wonky-step)) 
            wonky))
    (setq wonky (nreverse wonky))
    ;; Don't allow MON to force-bind `*MON-NAME*' or `*BUG-NAME*' by accident : )
    (if (and force-bind 
             (or (eq w-this-var '*MON-NAME*) (eq w-this-var '*BUG-NAME*))
             (and (bound-and-true-p *mon-misc-path-alist*)
                  (cdr (assoc 'the-only-if-its-a-mon-system *mon-misc-path-alist*))))
        wonky ;<- ITSA MON KEY so just return 
        (cond (force-bind (set w-this-var wonky))
              (bind-var (unless (bound-and-true-p w-this-var)
                          (setq w-this-var wonky)))
              (t wonky)))))
;;
;;; :TEST-ME (mon-build-user-name-example 5)
;;; :TEST-ME (mon-build-user-name-example 5 '*BUG-NAME*)
;;; :TEST-ME (mon-build-user-name-example 5 '*BUG-NAME* t)
;;; :TEST-ME (mon-build-user-name-example 5 '*BUG-NAME* nil t)
;;; :TEST-ME (mon-build-user-name-example 5 '*SOME-RANDOM-VAR* t)

;;; ==============================
;;; :CREATED <Timestamp: #{2010-03-25T15:35:10-04:00Z}#{10124} - by MON>
(defun mon-build-user-name-example-TEST ()
"Test function for `mon-build-user-name-example'.\n
Return value displayed in buffer \"*MON-BUILD-USER-NAME-EXAMPLE-TEST*\".\n
:SEE-ALSO .\n►►►"
(let ((mk-eu #'(lambda (USER-BIND) 
                 (eval (defconst USER-BIND nil
                         "An example user for `mon-build-user-name-example'."))))
      (rmv-eu #'(lambda (USR)
                  (progn (makunbound USR) (unintern USR))))
      (shw-eu #'(lambda (test-key w-var ev-rslt)
                  (push `(,test-key :W-NAME-VAR ,(format "%s" w-var) ,ev-rslt) mbunet-gthr)))
      (l-o-a '((:W-BIND-VAR *SOME-BV-USER* nil t)
               (:W-FORCE-BIND *SOME-FB-USER* nil t)
               (:W-NO-BIND/FORCE nil nil nil)))
      mbunet-gthr)
  (dolist (ev l-o-a (setq mbunet-gthr (nreverse mbunet-gthr)))
    (let ((tk  (nth 0 ev))
          (wtv (or (nth 1 ev) '*EXAMPLE-USER*))
          (bv  (nth 2 ev))
          (fb  (nth 2 ev)))
      (progn
        (funcall mk-eu wtv)
        (funcall shw-eu tk wtv
                 (mon-build-user-name-example 5 wtv nil t))
        (funcall rmv-eu wtv))))
  (pp-display-expression mbunet-gthr "*MON-BUILD-USER-NAME-EXAMPLE-TEST*")))

;;; ==============================
;;; :MODIFICATIONS <Timestamp: #{2010-03-24T16:52:12-04:00Z}#{10123} - by MON KEY>
;;; :CREATED <Timestamp: #{2010-02-10T14:19:41-05:00Z}#{10063} - by MON KEY>
(defun mon-build-misc-path-example (&optional bind-var force-bind)
  "Return a list preformatted with keys and values suitable for use with 
`*mon-misc-path-alist*'. 
When `bind-var' is non-nil if `*mon-misc-path-alist*' is unbound bind it with
return value.\n
When `force-bind' is non-nil force the binding even if `*mon-misc-path-alist*'
is already bound.\n
:EXAMPLE\n\n\(mon-build-misc-path-example\)\n
\(assoc 'the-1-path \(mon-build-misc-path-example\)\)\n
\(assoc 'the-emacs-vars \(mon-build-misc-path-example\)\)\n
\(nth 3 \(assoc 'the-sub \(mon-build-misc-path-example)\)\)\n
:SEE-ALSO `mon-get-mon-emacsd-paths', `mon-build-mon-emacsd-example',
`mon-build-user-name-example'.\n►►►"
  (let (pth egs)
    (do ((i 1 (1+ i)))
        ((> i 10) i)
      (push `(,(car (read-from-string  (concat "the-" (number-to-string i) "-path")))
               ,(concat "<PATHSTRING-" (number-to-string i) ">")) pth))
    (setq egs (nreverse pth))
    (setq pth nil)
    (do ((su 1 (1+ su)))
        ((> su 10) su)
      (push `(,(car (read-from-string  (concat "the-sub-" (number-to-string su))))
               ,(concat "<SUB-PATHSTRING-1-" (number-to-string su) ">")
               ,(concat "<SUB-PATHSTRING-2-" (number-to-string su) ">"))
            pth))
    (setq pth `((the-sub ,@(nreverse pth))))
    (setq egs (append egs pth))
    (setq pth nil)
    ;;; ==============================
    ;; :NOTE Simple form. Enhanced version below. 
    ;; (dotimes (E 9) 
    ;;   (push (car (read-from-string (concat "EMACS_VAR" (number-to-string E)))) pth))
    ;; (setq pth `((the-emacs-vars ,(nreverse pth))))
    ;; ==============================
    ;; :CALLED-BY `mon-get-env-vars-emacs'  :SEE (info "(emacs)General Variables")
    ;; :NOTE on MON's W32 systems these are:
    ;; (the-emacs-vars (EMC_BIN EMC_CUR EMC_PTH EMC_REPO EMC_W32 EMC_GNUW32 EMACS_LAUNCH))
    ;; Remove the suffix `-N' if you actually use these. They map as follows:
    ;; EMC_BIN <- The current Emacs' `bin' directory.
    ;; EMC_CUR <- The current Emacs' directory path.
    ;; EMC_PTH <- The current Emacs' path.
    ;; EMC_REPO   <- The path for loading site-local code stored in a repository.
    ;; EMC_W32    <- The path to Lennart's pathed Emacs-W32 build. 
    ;; EMC_GNUW32 <- The path to the local GNUwin32 binaries useful when this
    ;;               differs from the one Lennart hardwires with his patched build. 
    ;;               :SEE (URL `http://gnuwin32.sourceforge.net')
    ;; EMACS_LAUNCH <- The path to a W32 .cmd script to launch the emacsclient.
    ;;; ==============================
    (let ((var-cnt 0))
          (dolist (E '(EMC_BIN EMC_CUR EMC_PTH EMC_REPO EMC_W32 EMC_GNUW32 EMACS_LAUNCH EMCS_DUMMY1 EMCS_DUMMY2)
                   (setq pth `((the-emacs-vars ,(nreverse pth)))))
            (incf var-cnt)
            (push (car (read-from-string (format "%S-%d" E var-cnt))) pth)))
    (setq egs (append egs pth))
  (cond (bind-var (unless (bound-and-true-p *mon-misc-path-alist*)
                    (setq *mon-misc-path-alist* egs)))
        (force-bind (setq *mon-misc-path-alist* egs))
        (t egs))))
;;
;;; :TEST-ME (assoc 'the-1-path (mon-build-misc-path-example))
;;; :TEST-ME (assoc 'the-emacs-vars (mon-build-misc-path-example))
;;; :TEST-ME (nth 3 (assoc 'the-sub (mon-build-misc-path-example)))

;;; ==============================
;;; :CREATED <Timestamp: #{2010-02-10T16:32:23-05:00Z}#{10063} - by MON KEY>
(defun mon-build-mon-emacsd-example (&optional bind-var force-bind)
  "Return a list pre-formatted with keys and values suitable for use with 
`*mon-emacsd*'.
When `bind-var' is non-nil if `*mon-emacsd*' is unbound bind it.\n
When `force-bind' is non-nil force the binding even if `*mon-emacsd*' bound.\n
:EXAMPLE\n\n\(mon-build-mon-emacsd-example\)\n
\(assoc 1 \(mon-build-mon-emacsd-example\)\)\n
\(nth 8 \(assoc 1 \(mon-build-mon-emacsd-example\)\)\)\n
\(assoc 5 \(mon-build-mon-emacsd-example\)\)\n
\(assoc 'IS-USER-4-P \(cadr \(assoc 5 \(mon-build-mon-emacsd-example\)\)\)\)\n
\(assoc \(cadr \(nth 3 \(cadr \(assoc 5 \(mon-build-mon-emacsd-example\)\)\)\)\)\n
   \(mon-build-mon-emacsd-example\)\)\n\n
:SEE-ALSO `mon-get-mon-emacsd-paths', `mon-build-misc-path-example'.\n►►►"
  (let (gthr-emacsd usr-map)
    (dotimes (p 5) 
      (unless (= p 0)
        (push `(,p ,@(nreverse (do* ((j 0 (1+ j))
                                      (k (format "<USER-%s-EMACSD-PATH-Nth-%d>" p j)
                                         (format "<USER-%s-EMACSD-PATH-Nth-%d>" p j))
                                      (l () (cons k l)))
                                     ((>= j 14) l))))
              gthr-emacsd)
        (push `(,(car (read-from-string (format "IS-USER-%d-P" p))) ,p) usr-map)))
    (setq gthr-emacsd (nreverse gthr-emacsd))
    (setq gthr-emacsd (append gthr-emacsd `((5 ,(nreverse usr-map)))))
  (cond (bind-var (unless (bound-and-true-p *mon-emacsd*)
                    (setq *mon-emacsd* gthr-emacsd)))
        (force-bind (setq *mon-emacsd* gthr-emacsd))
        (t gthr-emacsd))))
;; 
;;; :TEST-ME (mon-build-mon-emacsd-example)
;;; :TEST-ME (assoc 1 (mon-build-mon-emacsd-example))
;;; :TEST-ME (nth 8 (assoc 1 (mon-build-mon-emacsd-example)))
;;; :TEST-ME (assoc 5 (mon-build-mon-emacsd-example))
;;; :TEST-ME (assoc 'IS-USER-4-P (cadr (assoc 5 (mon-build-mon-emacsd-example))))
;;; :TEST-ME (assoc (cadr (nth 3 (cadr (assoc 5 (mon-build-mon-emacsd-example)))))
;;;             (mon-build-mon-emacsd-example))

;;; ==============================
;;; This function is shadowed in mon-utils.el so that symbol it can be compiled.
;;; :CREATED <Timestamp: #{2010-02-10T19:47:57-05:00Z}#{10064} - by MON KEY>
(defun mon-string-wonkify (wonk-words wonkify-n-times)
  "Wonkify the string WONK-WORDS.\n
:EXAMPLE\n\n\(mon-string-wonkify \"These are some wonky words\" 10\)\n
\(mon-string-wonkify \"These are some wonky words\" 3\)\n
:SEE-ALSO `mon-generate-prand-seed', `mon-generate-prand-id',
 `mon-generate-WPA-key', \n►►►" 
  (eval-when-compile (require 'cookie1))
  (let ((wonkify wonk-words)
        (wonk-usr #'(lambda (l eo) 
                      (let (new-round)
                        (dolist (u l)
                          (let ((U u))
                            (dotimes (i (random (length U)))
                              (let ((rnd (random (length U))))
                                (setf (nth rnd U)
                                      (if eo (upcase (nth rnd U)) (downcase (nth rnd U))))))
                            (push (apply 'string U) new-round)))
                        (setq wonkify new-round))))
        (seqify #'(lambda (q)
                    (let (reseq)
                      (mapc #'(lambda (s) (push (string-to-sequence s 'list) reseq)) 
                            (cond ((listp q) q)
                                  ((stringp q) (list q))))
                      reseq))))
    (setq wonkify (make-list wonkify-n-times (car (funcall seqify  wonkify))))
    (do ((w wonkify-n-times))
         ((< w 0)  wonkify)
      (setq w (1- w))
      (setq wonkify (apply 'vector wonkify))
      (setq wonkify (shuffle-vector wonkify))
      (setq wonkify (append wonkify nil))
      (when (stringp (car wonkify))
        (setq wonkify (funcall seqify wonkify)))
      (funcall wonk-usr wonkify (if (eq (gcd w 2) 2) t)))))
;;
;;; :TEST-ME (mon-string-wonkify "These are some wonky words" 10)
;;; :TEST-ME (mon-string-wonkify "These are some wonky words" 3)

;;; ==============================
;;; :NOTE Following doesn't test for the system-type's:
;;;  `darwin', `ms-dos', `windows-nt', `cygwin'
;;; FTMP you should be able to _substitute_ the above into the system-type
;;; equivalence conditionals below where appropriate. Thus, if you are on and MS
;;; derived system the system type check should set `IS-MON-P-W32' to t.
;;; If you are on a `darwin'ish system I suppose you should try setting `IS-MON-P-GNU'
;;; (Caveat, I have no idea if this is correct)
;;; As many of the conditionals that depend on these vars are binary/boolean
;;; you will most-likely experience difficulty if you try to add a new var such as
;;; `IS-MON-P-DARWIN'. I apologize if this causes problems. I'm simply unable to
;;; build/verify/test MON code on these other systems.

;;; :EMACS-WIKI bind `IS-MON-P-W32' `IS-MON-P-GNU' when 
;;; :FILE mon-site-local-private.el isn't in your load-path. 
(unless (featurep 'mon-default-loads)
  (cond ((eq system-type 'windows-nt)
         (setq IS-MON-P-W32 t)
         (setq IS-MON-P-GNU nil))
        ((or (eq system-type 'gnu/linux) (eq system-type 'linux))
         (setq IS-MON-P-W32 nil)
         (setq IS-MON-P-GNU t))))
;;
;;; :TEST-ME IS-MON-P-W32
;;; :TEST-ME IS-MON-P-GNU

;;; ==============================
;;; :MODIFICATIONS <Timestamp: #{2010-03-24T12:06:54-04:00Z}#{10123} - by MON>
;;; :CREATED <Timestamp: #{2009-08-11T19:47:14-04:00Z}#{09332} - by MON KEY>
;;; ==============================
(defun mon-user-name-conditionals (&optional as-symbol)
  "Return value for the current system's user.\n
For debugging startup system name conditions.\n
Return one of the strings:\n
 \"IS-MON-P-W32\"  \"IS-MON-P-GNU\"
 \"IS-BUG-P\"      \"IS-BUG-P-REMOTE\"
 \"IS-W32-P\"      \"IS-GNU-P\"\n
When optional arg AS-SYMBOL is non-nil return one of the symbols:\n
  IS-MON-P-W32  IS-MON-P-GNU
  IS-BUG-P      IS-BUG-P-REMOTE
  IS-W32-P      IS-GNU-P\n
When AS-SYMBOL is non-nil return value is as per the constants:\n
 `IS-MON-P-W32'  `IS-MON-P-GNU' 
 `IS-BUG-P'      `IS-BUG-P-remote'
 `IS-W32-P'      `IS-GNU-P'\n
:NOTE The last two return values are system-type fallbacks when
`user-real-login-name' does not match. This has certain consequences for the
boolean return values when eval'ing for `eq' on the constant. For example,
assuming the constant `IS-MON-P-W32' and `IS-W32-P' each return t the second
form below will return correct but non-intuitive results:
\(eval \(eq \(mon-user-name-conditionals t\) 'IS-MON-P-W32\)\)\n
\(eval \(eq \(mon-user-name-conditionals t\) 'IS-W32-P\)\)\n
:EXAMPLE\n\n\(mon-user-name-conditionals\)\n
\(mon-user-name-conditionals t\)\n
\(mon-user-system-conditionals-TEST)\n
:SEE-ALSO `mon-system-type-conditionals', `mon-user-system-conditionals-TEST'
`mon-get-mon-emacsd-paths', `IS-W32-P', `IS-GNU-P', `IS-MON-SYSTEM-P',
`mon-emacs-root', `mon-site-lisp-root', `mon-user-emacsd', `mon-naf-mode-notes',
`mon-naf-mode-root', `mon-ebay-tmplt-mode-root', `*smith-poster-docs*'
`*MON-NAME*', `*BUG-NAME*', `*DCP-NAME*', `*mon-CIFS-domain*',
`*mon-CIFS-mount-root*', `*dbc-auth-path*', `*dbc-ap*', `*dbc-au*'.\n►►►"
  (let ((is-w32 (eq system-type 'windows-nt)) 
        (is-gnu (or (eq system-type 'gnu) 
                    (eq system-type 'gnu/linux) 
                    (eq system-type 'gnu/kfreebsd)
                    (eq system-type 'linux) ; It could happen!
                    ;; 'aix 'berkeley-unix 'hpux 'irix 'usg-unix-v
                    )))
    (cond (is-w32 
           (cond ((equal user-real-login-name "<LOCAL-W32-SYSTEM-LOGIN-NAME>") 
                  (if as-symbol 'IS-MON-P-W32 "IS-MON-P-W32"))
                 ;; :NOTE Following branch checks for the presence of a path.
                 ;; Useful for users whom access init files on both a local
                 ;; system and over network shares, file-servers, remote
                 ;; systems, etc.
                 ((equal user-real-login-name "Bug") 
                  (if (file-directory-p "<CONDITIONAL-TRIGGERS-ON-DIR-PATH-TEST>")
                      (if as-symbol 'IS-BUG-P "IS-BUG-P")
                      (if as-symbol 'IS-BUG-P-REMOTE "IS-BUG-P-REMOTE")))
                 ;; Catch any w32 stragglers
                 (t (if as-symbol 'IS-W32-P "IS-W32-P"))))
          (is-gnu 
           (cond ((equal user-real-login-name "<LOCAL-GNU-SYSTEM-LOGIN-NAME>")
                  (if as-symbol 'IS-MON-P-GNU "IS-MON-P-GNU"))
                 ;; Catch any gnu stragglers
                 (t (if as-symbol 'IS-GNU-P "IS-GNU-P")))))))
;;
;;; :TEST-ME (mon-user-name-conditionals)
;;; :TEST-ME (mon-user-name-conditionals t)
;;; :TEST-ME (eval (eq (mon-user-name-conditionals t) 'IS-MON-P-W32))
;;; :TEST-ME (eval (eq (mon-user-name-conditionals t) 'IS-W32-P))
;;; :TEST-ME (mon-user-system-conditionals-TEST 
;;;              '(:W32-THIS-USER :W-REAL-USER windows-nt "<W32-USERNAME-HERE")
;;;              '(:W32-THIS-USER :W-REAL-USER gnu/linux "<GNU-USERNAME-HERE"))


;;; ==============================
;;; :NOTE This can't be loaded from mon-default-start-loads!
;;; :MODIFICATIONS <Timestamp: #{2010-03-24T12:21:37-04:00Z}#{10123} - by MON KEY>
;;; :CREATED <Timestamp: #{2009-08-11T19:46:53-04:00Z}#{09332} - by MON KEY>
;;; ==============================
;;; :NOTE This can't be loaded from mon-default-start-loads!
;;; :MODIFICATIONS <Timestamp: #{2010-03-24T12:21:37-04:00Z}#{10123} - by MON KEY>
;;; :CREATED <Timestamp: #{2009-08-11T19:46:53-04:00Z}#{09332} - by MON KEY>
(defun mon-system-type-conditionals (&optional as-symbol)
  "Return the local system type as a string.
Return value is either:
 \"IS-W32-P\" \"IS-GNU-P\" 
Return value is conditional on return value of `mon-user-name-conditionals'.\n
When optional arg AS-SYMBOL is non-nil return one of the symbols:\n
 IS-W32-P IS-GNU-P
:EXAMPLE\n\n\(mon-system-type-conditionals\)\n
\(mon-system-type-conditionals t\)\n
\(mon-user-system-conditionals-TEST)\n
:NOTE return value affects the constants bound in mon-default-loads.el e.g.:\n
 `IS-MON-P-W32' `IS-MON-P-GNU'\n `IS-BUG-P' `IS-BUG-P-REMOTE'\n `IS-W32-P' `IS-GNU-P'\n
:SEE-ALSO  `mon-system-type-conditionals', `mon-user-system-conditionals-TEST'
`mon-get-mon-emacsd-paths', `IS-MON-SYSTEM-P', `mon-emacs-root',
`mon-site-lisp-root', `mon-user-emacsd', `mon-naf-mode-notes',
`mon-naf-mode-root', `mon-ebay-tmplt-mode-root', `*mon-HG-root-path*',
`*artist-naf-path*', `*smith-poster-docs*', `*MON-NAME*', `*BUG-NAME*',
`*DCP-NAME*', `*mon-CIFS-domain*', `*mon-CIFS-mount-root*', `*dbc-auth-path*',
`*dbc-ap*', `*dbc-au*'.\n►►►"
  ;; :NOTE Because we also set the case values as constants in :FILE
  ;; mon-default-loads.el it can be useful to verify the case switches with the
  ;; with commented code.  The let binding of the `user-real-login-name' below gets
  ;; us a generic IS-GNU-P or IS-W32-P. 
  ;;(let ((as-symbol   t) (system-type 'gnu) (user-real-login-name "bubba"))
  (let ((get-sys (mon-user-name-conditionals t))
        rtn)
    (setq rtn
          (case get-sys                 
            ('IS-MON-P-GNU     'IS-GNU-P)      ;'is-tracer-mon-gnu) 
            ('IS-GNU-P         'IS-GNU-P)      ;'is-tracer-gnu-generic)
            ('IS-BUG-P         'IS-W32-P)      ;'is-tracer-bug-local)  
            ('IS-BUG-P-REMOTE  'IS-W32-P)      ;'is-tracer-bug-remote)
            ('IS-MON-P-W32     'IS-W32-P)      ;'is-tracer-mon-w32) 
            ('IS-W32-P         'IS-W32-P)))    ;'is-tracer-w32-generic)))
    (if as-symbol 
        rtn 
        (setq rtn (format "%s" rtn))))) ;; )) ;:CLOSE testing let form
;;
;;; :TEST-ME (mon-system-type-conditionals)
;;; :TEST-ME (mon-system-type-conditionals t)
;;; :TEST-ME (mon-user-system-conditionals-TEST)

;;; ==============================
;;; :CREATED <Timestamp: #{2010-03-24T13:38:11-04:00Z}#{10123} - by MON>
(defun mon-user-system-conditionals-TEST (&rest user-system-test-with)
  "Test function for `mon-user-name-conditionals' and `mon-system-type-conditionals'.\n
Return pp'ed results to buffer named \"*MON-USR/SYS-COND-TESTS*\".
When USER-SYSTEM-TEST-WITH args are supplied additional username system-type
tests are performed. Each USER-SYSTEM-TEST-WITH is quoted list of the form:\n
 (<LIST-KEY> <NAME-PROP> <SYSTEM-TYPE> <USER-NAME>)\n
For example:\n
 '\(:W32-THIS-USER :W-REAL-USER windows-nt \"i-am-w32-username\"\)
 '\(:GNU-THIS-USER :W-REAL-USER gnu/linux \"i-am-gnu-username\"\)\)\n
:EXAMPLE\n
\(mon-user-system-conditionals-TEST
 '\(:W32-THIS-USER :W-REAL-USER windows-nt \"i-am-w32-username\"\)
 '\(:GNU-THIS-USER :W-REAL-USER gnu/linux \"i-am-gnu-username\"\)\)\n
:SEE-ALSO `mon-build-copyright-string-TEST',
`mon-help-propertize-regexp-symbol-defs-TEST', `mon-help-propertize-tags-TEST',
`mon-help-regexp-symbol-defs-TEST', `mon-help-wget-cl-pkgs-TEST',
`mon-wget-list-to-script-TEST', `mon-line-strings-to-list-TEST',
`mon-help-keys-wikify-TEST', `mon-insert-lisp-testme-fancy',
`mon-insert-lisp-testme', `mon-insert-test-cases'.\n►►►"
(let ((gthrer #'(lambda (lst-key nm-prop sys-typ usr-rln)
                  (let ((system-type sys-typ)
                        (user-real-login-name usr-rln))
                    (push `(,lst-key ,nm-prop ,usr-rln
                            (:USER-NAME-COND
                            ,(mon-user-name-conditionals)
                            ,(mon-user-name-conditionals t))
                            (:SYSTEM-TYPE-COND ,nm-prop ,usr-rln
                            ,(mon-system-type-conditionals)
                            ,(mon-system-type-conditionals t)))
                          gthr))))
      (test-with `((:W32-NO-USER :W-ANON-USER windows-nt "bubba")
                   (:W32-THIS-USER :W-REAL-USER windows-nt ,user-real-login-name)
                   (:GNU-NO-USER :W-ANON-USER gnu/linux  "bubba")
                   (:GNU-THIS-USER :W-REAL-USER gnu/linux ,user-real-login-name)
                   ,@user-system-test-with))
      gthr)
  (dolist (tw test-with (setq gthr (nreverse gthr)))
    (funcall gthrer (nth 0 tw) (nth 1 tw) (nth 2 tw) (nth 3 tw)))
  (pp-display-expression gthr "*MON-USR/SYS-COND-TESTS*")))
;;
;;; :TEST-ME (mon-user-system-conditionals-TEST
;;;              '(:W32-THIS-USER :W-REAL-USER windows-nt "i-am-w32-username")
;;;              '(:GNU-THIS-USER :W-REAL-USER gnu/linux "i-am-gnu-username"))

;;; ==============================
;; :NOTE The function `mon-build-mon-emacsd-example'  binds temporary key
;;; value pairs for `*mon-emacsd*'.  If you find that you actually use
;;; MON packages you will want to uncomment above and populate with reasonable
;;; values according to the template generated by `mon-build-mon-emacsd-example'
;;; Also note, I have padding the length of the list to ensure that calling code
;;; doesn't ask for a value that isn't there. This list doesn't change that
;;; often and I can't guarantee to remember to increase the step value :\
;;; :MODIFICATIONS <Timestamp: #{2010-02-10T17:06:55-05:00Z}#{10063} - by MON KEY>
;;; :CREATED <Timestamp: #{2009-08-11T19:47:24-04:00Z}#{09332} - by MON KEY>
(defconst *mon-emacsd* (mon-build-mon-emacsd-example t)
  "*An alist to encapusulate common site local and default paths.
alist contains numbered keys (a key per system). 
cdr of each associated key maps to an Nary elt list where each elt points to a
site-local path, default value, string, etc.\n
EXAMPLE:\n\(assoc 1 *mon-emacsd*\)
:CALLED-BY `mon-user-emacsd'            ;<- nth 1
:CALLED-BY `custom-file'                ;<- nth 2
:CALLED-BY `mon-open-workspace'         ;<- nth 3
:CALLED-BY `mon-open-moz-down'          ;<- nth 4
:CALLED-BY `*mon-HG-root-path*'         ;<- nth 5
:CALLED-BY `set-emacs-root'             ;<- nth 6
:CALLED-BY `*artist-naf-path*'          ;<- nth 7
:CALLED-BY `common-lisp-hyperspec-root' ;<- nth 8
:CALLED-BY `browse-url-generic-program' ;<- nth 9
:CALLED-BY  `browse-url-firefox-program';<- nth 10\n
:SEE-ALSO `mon-build-mon-emacsd-example', `mon-get-mon-emacsd-paths',
`mon-system-type-conditionals', `mon-user-name-conditionals',
`*mon-misc-path-alist*', `IS-W32-P', `IS-GNU-P', `IS-MON-SYSTEM-P',
`mon-site-lisp-root', `mon-naf-mode-notes', `mon-naf-mode-root',
`*MON-NAME*', `*BUG-NAME*', `*MON-ORG-NAME*', `*mon-CIFS-domain*'.\n►►►")
;;
;;; :TEST-ME (assoc  1 *mon-emacsd*)
;;; :TEST-ME (assoc 2 *mon-emacsd*)
;;; :TEST-ME (assoc 3 *mon-emacsd*)
;;; :TEST-ME (nth 6 (assoc 3 *mon-emacsd*))
;;; :TEST-ME (nth 7 (assoc 3 *mon-emacsd*))
;;
;;;(progn (makunbound '*mon-emacsd*) (unintern '*mon-emacsd*) )

;;; ==============================
;; :NOTE The function `mon-build-misc-path-example' binds temporary key
;; value pairs for `*mon-misc-path-alist*'.  If you find that you actually use
;; MON packages you will want to uncomment above and populate with reasonable
;; values according to the template generated by `mon-build-misc-path-example'.
;;; CREATED: <Timestamp: #{2009-08-14T13:28:29-04:00Z}#{09335} - by MON>
(defvar *mon-misc-path-alist* (mon-build-misc-path-example t)
  "*An alist of keys to path values which are not available on all systems.\n
Paths values in this list don't warrant assignement to a dedicated variable.
All keys in this list should get a 'the-' prefix to help distinguish when they
will be used to assign a global var with a similar name.
:CALLED-BY `*mon-CIFS-domain*'       <- key 'the-shr-prfx
:CALLED-BY `*mon-CIFS-mount-root*'   <- key 'the-mnt-prfx
:CALLED-BY `*mon-CIFS-mount-points*' <- key 'the-mnt-maps
:CALLED-BY `mon-open-images-ed-swap' <- key 'the-mon-img-ed-swp, 'the-bug-img-ed-swp
:CALLED-BY `mon-its-all-text-purge-on-quit' <- key 'the-itsalltext-temp-dir
:CALLED-BY `mon-get-env-vars-emacs'  <- key 'the-emacs-vars\n
The 'the-emacs-vars key holds these environment variables (mostly W32 centric):
 EMC_BIN      <- The current Emacs' `bin' directory.
 EMC_CUR      <- The current Emacs' directory path.
 EMC_PTH      <- The current Emacs' path.
 EMC_REPO     <- The path for loading site-local code stored in a repository.
 EMC_W32      <- The path to Lennart's pathed Emacs-W32 build. 
 EMACS_LAUNCH <- The path to a W32 .cmd script to launch the emacsclient.
 EMC_GNUW32   <- The path to the local GNUwin32 binaries useful when this
                 differs from the one Lennart hardwires with his patched build.
                 :SEE (URL `http://gnuwin32.sourceforge.net')\n
:SEE info node `(emacs)General Variables'\n
:EXAMPLE\n\(assoc 'the-1-path *mon-misc-path-alist*\) -> \"<PATHSTRING-1>\"\n
\(assoc 'the-1-path \(mon-build-misc-path-example\)\)\n
\(assoc 'the-emacs-vars \(mon-build-misc-path-example\)\)\n
\(assoc 'the-sub \(mon-build-misc-path-example\)\)\n
\(assoc 'the-sub-1 \(assoc 'the-sub \(mon-build-misc-path-example\)\)\)
:SEE-ALSO `mon-get-mon-emacsd-paths', `mon-system-type-conditionals',
`mon-user-name-conditionals' `IS-W32-P', `IS-GNU-P', `IS-MON-SYSTEM-P',
`IS-MON-P-W32', `IS-MON-P-GNU', `IS-MON-P', `IS-BUG-P', `IS-BUG-P-REMOTE',
`mon-emacs-root', `mon-site-lisp-root', `mon-user-emacsd', `mon-naf-mode-notes',
`mon-naf-mode-root', `mon-ebay-tmplt-mode-root'.\n►►►")
;;
;;; :TEST-ME  *mon-misc-path-alist*
;;; :TEST-ME (assoc 'the-1-path *mon-misc-path-alist*)
;;
;;;(progn (makunbound '*mon-misc-path-alist*) (unintern '*mon-misc-path-alist*) )

;;; (assoc 'the-sub-1 (assoc 'the-sub (mon-build-misc-path-example)))
;;; ==============================
;;; :NOTE The function `mon-build-user-name-example' builds temporary key value
;;; pairs.  If you find you actually use MON packages you will prob. want to
;;; uncomment above and populate with reasonable values.
;;  '((1 "<NAMEFORM-1>")(2 "<NAMEFORM-2>")(3 "<NAMEFORM-3>"));; {...etc..})

;; :NOTE Following `do' loop builds temporary key value pairs Suitable for 
;;; use with
;; If you find that you actually use MON pacages you will want to uncomment 
;; above and populate with reasonable values.
;;; (let (nmf)
;;;   (do ((i 1 (1+ i)))
;;;       ((> i 10)i)
;;;     (push `(,i ,(concat "<NAMEFORM-" (number-to-string i) ">")) nmf))
;;;   (nreverse nmf))

;;; CREATED: <Timestamp: #{2009-08-13T17:41:02-04:00Z}#{09334} - by MON KEY>
(defconst *MON-NAME* (mon-build-user-name-example 5 '*MON-NAME* t t)
  "*An alist to encapsulate MON name across packages.\n
Different system's and user's keep an alist of their preferred
Nameforms stored in a constant symbol of the form `*SOME-NAME*'
Numbered keys in alist \(1 indexed\) map to Nameforms - typically a string.
:EXAMPLE\n(assoc 1 *MON-NAME*) ;-> \"<NAMEFORM-1>\"\n
\(mon-build-user-name-example 9\)\n
\(assoc 3 \(mon-build-user-name-example 3\)\)\n
:SEE-ALSO `mon-get-mon-emacsd-paths', `mon-system-type-conditionals',
`mon-build-user-name-example' `mon-user-name-conditionals',
`*mon-misc-path-alist*', `IS-W32-P', `IS-GNU-P', `IS-MON-SYSTEM-P',
`IS-MON-P-W32', `IS-MON-P-GNU', `IS-MON-P', `IS-BUG-P', `IS-BUG-P-REMOTE',
`mon-emacs-root', `mon-site-lisp-root', `mon-user-emacsd', `mon-naf-mode-notes',
`mon-naf-mode-root', `*mon-HG-root-path*', `*artist-naf-path*',
`*smith-poster-docs*' , `mon-ebay-tmplt-mode-root', `*BUG-NAME*', `*DCP-NAME*',
`*mon-CIFS-domain*', `*mon-CIFS-mount-root*'.\n►►►")
;;
;;; :TEST-ME *MON-NAME*
;;; :TEST-ME (assoc 1 *MON-NAME*)
;;; :TEST-ME (cadr (assoc 2 *MON-NAME*)) 
;;
;;;(progn (makunbound '*MON-NAME*) (unintern '*MON-NAME*))

;;; ==============================
;;; :CREATED <Timestamp: #{2009-10-24T15:49:36-04:00Z}#{09436} - by MON KEY>
(defconst *BUG-NAME* (mon-build-user-name-example 5 '*BUG-NAME*  t t)
  "*Alist to encapsulate MON name across packages.\n
Different system's and user's keep an alist of their preferred
Nameforms stored in a constant symbol of the form `*SOME-NAME*'
Numbered keys in alist \(1 indexed\) map to Nameforms - typically a string.
:EXAMPLE\n(assoc 1 *BUG-NAME*) ;-> \"<NAMEFORM-1>\"
:SEE-ALSO `mon-get-mon-emacsd-paths', `mon-system-type-conditionals',
`mon-build-user-name-example' `mon-user-name-conditionals',
`*mon-misc-path-alist*', `IS-W32-P', `IS-GNU-P', `IS-MON-SYSTEM-P',
`IS-MON-P-W32', `IS-MON-P-GNU', `IS-MON-P', `IS-BUG-P', `IS-BUG-P-REMOTE',
`mon-emacs-root', `mon-site-lisp-root', `mon-user-emacsd', `mon-naf-mode-notes',
`mon-naf-mode-root', `*mon-HG-root-path*', `*artist-naf-path*',
`*smith-poster-docs*' , `mon-ebay-tmplt-mode-root', `*BUG-NAME*', `*DCP-NAME*',
`*mon-CIFS-domain*', `*mon-CIFS-mount-root*'.\n►►►")
;;
;;; :TEST-ME *BUG-NAME*
;;; :TEST-ME (assoc 1 *BUG-NAME*)
;;; :TEST-ME (cadr (assoc 2 *BUG-NAME*)) 
;;
;;;(progn (makunbound '*BUG-NAME*) (unintern '*BUG-NAME*))

;; :REQUIRED-BY :FILE mon-insertion-utils.el
(defvar *MON-ORG-NAME*
  '((1 " - c/o YourSite.com") 
    (2 "YourSite.com")
    (3 "www.YourSite.com")
    (4 "http://www.YourSite.com/")
    (5 "https://www.YourSite.com/")
    (6 "http://www.YourSite.com/path/to/some/images")
    (7 "your-email@some-domain.com")
    (8 "your-email@some-domain.com")
    (9 "your-email@some-domain.com")
    (10 ("Your-ID" "Your-ID1" "Your-ID2"))) ;;Account-IDs
  "*An alist to encapsulate Your-Site.com URLs across packages.\n
More generally this an organization, insitution, corporation etc. with whom
local users are affilated, employed-by, etc. 
A good value might also be gleaned from: 
  (getenv \"Organization\")\n
:EXAMPLE\n(assoc 4 *MON-ORG-NAME*)
:SEE-ALSO `mon-get-mon-emacsd-paths', `mon-system-type-conditionals',
`mon-user-name-conditionals', `*mon-misc-path-alist*', `IS-W32-P', `IS-GNU-P',
`IS-MON-SYSTEM-P', `IS-MON-P-W32', `IS-MON-P-GNU', `IS-MON-P', `IS-BUG-P',
`IS-BUG-P-REMOTE', `mon-emacs-root', `mon-site-lisp-root', `mon-user-emacsd',
`mon-naf-mode-notes', `mon-naf-mode-root', `*mon-HG-root-path*',
`*artist-naf-path*', `*MON-NAME*', `*BUG-NAME*', 
`*mon-CIFS-domain*', `*mon-CIFS-mount-root*'.\n►►►")

;;; ==============================
;;; :CREATED <Timestamp: #{2009-08-26T11:12:06-04:00Z}#{09353} - by MON KEY>
;;; :TODO 
;;; For browsing local urls, Consider building some pathname translations for
;;; browsing local `cached' URLS using `browse-url-of-buffer'.
;;; This could be useful for example when editing ebay-template files. 
;;; 1) Capture the html region 
;;; 2) spit it to a temp-buffer (or just write it to a temp file!)
;;; 3) browse it in a conkeror/ffx 
;;; :SEE (URL `http://www.emacswiki.org/emacs/BrowseUrl#toc3')
;;; (add-to-list 'browse-url-filename-alist
;;;              '("/var/www/cgi/files/" . "http://my.website.com/cgi?"))

;;; ==============================
(provide 'mon-site-local-defaults)
;;; ==============================

;;; ================================================================
;;; mon-site-local-defaults.el ends here
;;; EOF
