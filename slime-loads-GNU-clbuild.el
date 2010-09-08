;;; slime-loads-GNU-clbuild.el --- conform slime/swank to current environment
;; -*- mode: EMACS-LISP; -*-

;;; ================================================================
;; Copyright © 2009, 2010 MON KEY. All rights reserved.
;;; ================================================================

;; FILENAME: slime-loads-GNU-clbuild.el
;; AUTHOR: MON KEY
;; MAINTAINER: MON KEY
;; CREATED: 2009-09-06T10:19:19-04:00Z
;; VERSION: 1.0.0
;; COMPATIBILITY: Emacs23.*
;; KEYWORDS: 

;;; ================================================================

;;; COMMENTARY: 

;; =================================================================
;; DESCRIPTION:
;; slime-loads-GNU-clbuild provides procedures to conform slime/swank to the
;; currrent Emacs environment. Includes utilities for finding the currenly
;; installed Quicklisp distribution of slime/swank. Some features provided are
;; GNU/Linux centric and may require adaptation to other OS'. 
;; 
;; slime-loads-GNU-clbuild configs stored seperately from Slime load defs called from 
;; `default-start-loads.el' to prevent the HG synch issues that arise.
;;
;; FUNCTIONS:►►►
;; `quicklisp-find-slime', `quicklisp-current-swank-loader',
;; `quicklisp-write-dot-swank-loader-if', `quicklisp-write-dot-swank-loader',
;; `quicklisp-dot-swank-current-p', 
;; `quicklisp-hash-system-completions', `quicklisp-system-complete',
;; `quicklisp-hash-system-completions-slime-loadtime',
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
;; `*quicklisp-path*', `*quicklisp-systems*', 
;;
;; ALIASED/ADVISED/SUBST'D:
;;
;; DEPRECATED:
;;
;; RENAMED:
;;
;; MOVED:
;;
;; TODO:
;;
;; NOTES:
;;
;; >> *STANDARD-OUTPUT* of functions called from the REPL is going to
;; >> *inferior-lisp* not to the REPL as it used to.  I noticed this when
;; >> calling DISASSEMBLE; the disassembly appears in the *inferior-lisp*
;; >> buffer.
;; > This is a known bug, but unfortunately it's still not fixed. As a quick
;; > workaround you can evaluate:
;;
;; (setf *standard-output*
;;       (swank::connection.user-output swank::*emacs-connection*)) 
;;
;; > at the REPL.
;;
;;(setq asdf-install:*verify-gpg-signatures* nil)
;;
;; :SEE (URL `http://article.gmane.org/gmane.lisp.slime.devel/8712')
;; :SEE (URL `http://thread.gmane.org/gmane.lisp.slime.devel/8442')
;;
;; ==============================
;; :QUICKLISP-NOTES
;; - Grab quicklisp.lisp from http://quickstart.quicklisp.org/quicklisp.lisp 
;; - wget/curl it Or, from Emacs: 
;;       (progn (require 'url)
;;              (require 'url-handlers)
;;              (with-current-buffer (get-buffer-create "*QUICKLISP-QUICKSTART*")
;;                (erase-buffer)
;;                (url-insert-file-contents 
;;                 "http://quickstart.quicklisp.org/quicklisp.lisp")
;;                (write-file "quicklisp.lisp")
;;                (display-buffer (current-buffer) t)))
;;             
;; - Start a lisp and load (load #P"/<QUICKLISP-PATH>/quicklisp.lisp") 
;;
;; - Add  ~/quicklisp/setup.lisp to your lisp implementation's init file, (e.g. ~/.sbclrc)
;;   with something like: 
;;   (load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))
;;
;; - Initialize a new lisp session.
;;
;; - To load systems, use (ql:quickload "system-name")
;;
;; - To use slime/swank with the locally installed Emacs do: (ql:quickload "swank") 
;;
;; (URL `http://quicklisp.org/')
;; (URL `http://blog.quicklisp.org/')
;; (URL `git://github.com/quicklisp/quicklisp-projects.git')
;; (URL `http://www.youtube.com/watch?v=11wYPAy9qNw&feature=PlayList&p=9A2D7E31B7D039AF&index=0&playnext=1&hd=1')
;; (URL `http://github.com/quicklisp/boston-lisp-meeting/raw/master/blm-lightning-talk.pdf')
;; (URL `http://xach.com/tmp/ql.pdf')
;; (URL `http://xach.com/tmp/fails/')
;; (URL `http://groups.google.com/group/quicklisp')
;; (URL `http://dist.quicklisp.org/quicklisp.txt')
;;
;;; ==============================
;; :SLIME-MODE-PACKAGES
;;
;; :SLIME-AUTODOC
;; Better arglist display, can be turned off by customization.
;;
;; :SLIME-C-P-C
;; Adds new commands and installs compound-prefix-completion as
;; default completion command.  Behaves similar to standard Emacs
;; completion, unless dashes are present. --mkoeppe
;;
;; :SLIME-EDITING-COMMANDS
;; Just adds commands.  (Well, shadows commands in lisp-mode-map)
;;
;; :SLIME-FANCY-INSPECTOR
;; Makes the inspector fancier.  (Once loaded, can't be turned off.)
;;
;; :SLIME-FUZZY
;; Just adds the command C-c M-i.  We do not make fuzzy completion the
;; default completion invoked by TAB. --mkoeppe
;;
;; :SLIME-HIGHLIGHT-EDITS
;; :NOTE "Do not activate slime-highlighting-edits by default, as it's easier
;;        to explictly activate it (if a user really wants it) than to explictly
;;        deactivate it once it got globally enabled. -TCR."
;;
;; :SLIME-PRESENTATIONS
;; :NOTE "Load slime-presentations even though they seem to be a
;;        controversial feature, as they can be easily turned off by
;;        customizing swank:*record-repl-results*. --mkoeppe"
;;
;; (require 'SLIME-PRESENTATION-STREAMS)
;; :NOTE "Do not load slime-presentation-streams, as this is an experimental
;;        feature that installs patches into some Lisps. --mkoeppe"
;;
;; (require 'slime-typeout-frame)
;; :NOTE "Do not load slime-typeout-frame, as simply loading causes display of a
;;       typeout frame, which cannot be turned off. --mkoeppe"
;;
;; :SLIME-REFERENCES-INIT
;; Puts clickable references to documentation into SBCL errors.
;;
;; :SLIME-MDOT-FU-INIT
;; Makes M-. work on local definitions, too.
;;
;; :SLIME-PACKAGE-FU
;; Add/Remove a symbol at point from the relevant DEFPACKAGE form
;; via C-c x.
;;
;; :SLIME-FONTIFYING-FU
;; Fontify with-foo and do-foo like standard macros.
;;
;; ==============================
;; :LISP-ENVIRONMENT
;;
;; :SBCL-RELEVANT-PATHS
;;
;; ~/.sbclrc
;; ~/.sbcl
;; ~/.sbcl/trusted-uids.lisp
;; ~/.sbcl/keyring.asc
;; ~/.sbcl/site
;; ~/.sbcl/systems
;; /etc/bash_completion.d/sbcl
;; /usr/local/lib/sbcl
;; /usr/local/bin/sbcl
;;
;; (unless (getenv "SBCL_HOME")
;;   (setenv "SBCL_HOME" "/usr/local/lib/sbcl"))
;;
;; (setq slime-lisp-implementations
;;       '((sbcl ("sbcl" "--core" "<PATH-TO>/sbcl-core-for-slime"))))
;;
;;
;; (custom-set-variables 'slime-sbcl-manual-root 
;;                       "file://usr/local/share/doc/sbcl/html/sbcl/")
;;
;; :CLISP-CLOCC
;;
;; :NOTE Following of potential of use this with Sam Steingold's CLOCC:
;; :SEE (URL `http://clocc.sourceforge.net/')
;;
;; (unless (getenv "LISPTYPE")
;;   (setenv "LISPTYPE" inferior-lisp-program))
;;
;; (executable-find "clisp")
;; (setq inferior-lisp-program ;"/usr/bin/clisp")
;;      "/usr/bin/clisp -K full")
;;
;; ==============================
;;
;; SNIPPETS:
;;
;; REQUIRES:
;;
;; THIRD-PARTY-CODE: 
;; :SEE comments above. 
;;
;; URL: http://www.emacswiki.org/emacs/slime-loads-GNU-clbuild.el
;; FIRST-PUBLISHED: <Timestamp: #{2010-09-07T11:31:31-04:00Z}#{10362} - by MON>
;;
;; EMACSWIKI: { URL of an EmacsWiki describing slime-loads-GNU-clbuild. }
;;
;; FILE-CREATED:
;; <Timestamp: #{2009-09-06T10:19:19-04:00Z}#{09367} - by MON KEY>
;;
;; =================================================================

;;; LICENSE:

;; =================================================================
;; This file is not part of GNU Emacs.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:

;;    1. Redistributions of source code must retain the above copyright
;;       notice, this list of conditions and the following disclaimer.

;;    2. Redistributions in binary form must reproduce the above
;;       copyright notice, this list of conditions and the following
;;       disclaimer in the documentation and/or other materials
;;       provided with the distribution.

;; THIS SOFTWARE IS PROVIDED BY MON KEY ``AS IS'' AND ANY
;; EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
;; PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL MON KEY OR
;; CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
;; USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
;; OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
;; OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;; SUCH DAMAGE.

;; The views and conclusions contained in the software and
;; documentation are those of the authors and should not be interpreted
;; as representing official policies, either expressed or implied, of
;; MON KEY."

;;; ==============================
;; Copyright © 2009, 2010 MON KEY 
;;; ==============================

;;; CODE:

(eval-when-compile (require 'cl))


;;; ==============================
;;; :PASTED (URL `http://paste.lisp.org/+2EGF')
;;; ==============================
;;; :CREATED <Timestamp: #{2010-06-30T16:13:46-04:00Z}#{10263} - by MON KEY>
(defvar *quicklisp-path* nil
  "Names a directory for use with quicklisp.\n
Defaults to the value of:
 \(merge-pathnames \"quicklisp/\" \(user-homedir-pathname\)\)\n
:SEE (URL `http://www.quicklisp.org').\n
:SEE-ALSO `slime-backend', `quicklisp-find-slime',
`quicklisp-current-swank-loader', `quicklisp-write-dot-swank-loader-if',
`quicklisp-write-dot-swank-loader', `quicklisp-dot-swank-current-p',
`*quicklisp-path*'.\n►►►")
;;
(unless (bound-and-true-p *quicklisp-path*)
  ;; (setq *quicklisp-path* (substitute-in-file-name "$HOME/quicklisp")
  ;; (setq *quicklisp-path* (expand-file-name "quicklisp/" (getenv "HOME")))
  (setq *quicklisp-path* (concat (getenv "HOME") "/quicklisp/")))

;;; ==============================
;;; :CREATED <Timestamp: #{2010-06-30T16:13:52-04:00Z}#{10263} - by MON KEY>
(defun quicklisp-find-slime (&optional ql-path) 
  "Find the most-recently installed slime/swank quicklisp installation.\n
QL-PATH is directory namestring of an alternate path to the quicklisp directory,
when provided it has the form:\n
 \"/some/path/to/quicklisp/\"\n
Where \"software/\" is an immediate subdir of QL-PATH.\n
When ommitted defaults to the value of `*quicklisp-path*'.\n
Signal an error if ther is not a directory name matching the regexp:
 \\\(slime-\\\(%s\\\)-cvs\\\)\n
in the software subdir of `*quicklisp-path*'.\n
:EXAMPLE\n\n\(quicklisp-find-slime\)\n
\(quicklisp \"~/quicklisp/\")\n
:SEE-ALSO `slime-backend', `quicklisp-current-swank-loader',
`quicklisp-write-dot-swank-loader-if', `quicklisp-dot-swank-current-p',
`quicklisp-write-dot-swank-loader', `*quicklisp-path*'.\n►►►"
  (let* ((ql-softs (concat (or ql-path *quicklisp-path*) 
                           ;; :WAS "software/"))
                           "dists/quicklisp/software/")) 
         (fnd-slm-rgxp "\\(slime-\\(%s\\)-cvs\\)") ;; %s -> "[0-9]+" & most-recent
         (ql-slimes 
          (directory-files ql-softs nil (format fnd-slm-rgxp "[0-9]+")))
         (ql-esignal (format (concat ":FUNCTION `quicklisp-find-slime' "
                                     "-- no slime/swank directory in `*quicklisp-path*': %s")
                             ql-softs))
         most-recent)
    (if (null ql-slimes)
        (error ql-esignal)
      (dolist (mr ql-slimes 
                  (if (null most-recent)
                      (error ql-esignal)
                    (setq most-recent 
                          (car (sort (nreverse most-recent) 
                                     #'(lambda (old new) (string< new old)))))))
                                         
        (progn (string-match (format fnd-slm-rgxp "[0-9]+") mr)
               (when (> (match-beginning 2) 0)
                 (push (substring mr (match-beginning 2) (match-end 2))
                       most-recent)))))
    (let ((mr most-recent))
      (dolist (fnd-slm ql-slimes)
        (when (string-match-p (format fnd-slm-rgxp mr) fnd-slm)
          (setq most-recent fnd-slm))))
    ;; :NOTE `most-recent' is either a datestring, or a directory-namestring:
    ;;  "*quicklisp-path*/*/slime-<DATE>-cvs"
    ;; If its a directory we win.
    (if (file-directory-p (concat ql-softs most-recent))
        (file-truename (concat ql-softs most-recent))
      (error ql-esignal))))
;;
;;; :TEST-ME (quicklisp-find-slime)
;;; :TEST-ME (quicklisp-find-slime "~/quicklisp/")

;;; ==============================
;;; :CHANGESET 1929
;;; :CREATED <Timestamp: #{2010-06-30T16:44:21-04:00Z}#{10263} - by MON KEY>
(defun quicklisp-current-swank-loader (&optional ql-path)
  "Return a list suitable for writing/verifying the existence of
  \".swank-loader.lisp\" in quicklisp directory `*quicklisp-path*'.
Optional arg QL-PATH is as per `quicklisp-find-slime'.\n
Return value is a three elt list of the form:
\( \( {t | nil} . { *quicklisp-path* | ql-path }.swank-loader.lisp \)
  { *quicklisp-path* | ql-path }software/slime-<DATE>-cvs
  { *quicklisp-path* | ql-path }software/slime-<DATE>-cvs/swank-loader.lisp \)\n
When file with name \".swank-loder.lisp\" is not present car of elt 0 is nil.\n
cdr of elt 0 is the file's truename whether it exists or not, signal an error if
file is can not be written.\n
elt1 is the `directory-file-name' of the most current slime directory having the form:
 slime-<DATE>-cvs\n
elt2 is the truename of the most current swank-loader.lisp\n
:EXAMPLE\n\n\(quicklisp-current-swank-loader\)\n
\(quicklisp-current-swank-loader \"~/quicklisp/\"\)\n
:SEE-ALSO `slime-backend', `quicklisp-find-slime',
`quicklisp-write-dot-swank-loader-if', `quicklisp-write-dot-swank-loader',
`quicklisp-dot-swank-current-p', `*quicklisp-path*'.\n►►►"
  (let* ((swk-ldr-pth (quicklisp-find-slime ql-path))
         (swk-ldr     (concat (file-name-as-directory swk-ldr-pth)
                              "swank-loader.lisp"))
         (dt-swk-ldr  (concat (file-name-directory
                               (directory-file-name 
                                (file-name-directory swk-ldr-pth)))
                              ".swank-loader.lisp")))
    (and (or ;; Unless, Slime-devs change this flname should be ok. Check anyhow.
          (file-exists-p swk-ldr) 
          (error (concat ":FUNCTION `quicklisp-current-swank-loader' "
                         "-- non-existent file: %s") swk-ldr))
         (cond ((file-exists-p dt-swk-ldr) 
                (setq dt-swk-ldr `(t . ,dt-swk-ldr)))
               ((file-writable-p dt-swk-ldr) 
                (setq dt-swk-ldr `(nil . ,dt-swk-ldr)))
               (t (error (concat ":FUNCTION `quicklisp-current-swank-loader' "
                                 "-- non-writable file: %s") swk-ldr)))
    `(,dt-swk-ldr ,swk-ldr-pth ,swk-ldr))))
;;
;;; :TEST-ME (quicklisp-current-swank-loader)
;;; :TEST-ME (quicklisp-current-swank-loader "~/quicklisp/")

;;; ==============================
;;; :CHANGESET 1929
;;; :CREATED <Timestamp: #{2010-06-30T19:09:57-04:00Z}#{10263} - by MON KEY>
(defun quicklisp-write-dot-swank-loader (&optional w-swank-loader-list
                                                   ql-path w-msg-user)
  "Write new .swank-loader.lisp file with pointer to most current swank/slime.\n
When optional arg W-SWANK-LOADER-LIST is omitted default is the value returned
by `quicklisp-current-swank-loader', when non-nil it is a list of the form
returned by that function.
When W-SWANK-LOADER-LIST is omitted and optional arg QL-PATH is non-nil it is a
directory namestring as per `quicklisp-find-slime' and .swank-loader.lisp file
is written to the QL-PATH directory. 
When W-SWANK-LOADER-LIST and QL-PATH are ommitted, default to value of
`*quicklisp-path*'.\n
When optional arg W-MSG-USER is non-nil write a message to \"*Messages*\" buffer
indicating that file has changed.\n
:SEE-ALSO `slime-backend', `quicklisp-find-slime',
`quicklisp-current-swank-loader', `quicklisp-write-dot-swank-loader-if',
`quicklisp-dot-swank-current-p', `*quicklisp-path*'.\n►►►"
  (let* ((qcsw (or w-swank-loader-list
                   (quicklisp-current-swank-loader ql-path)))
         (dot-swank  (cdar qcsw))
         (cur-swank  (cadr qcsw))
         (ldr-swank  (caddr qcsw)))
    (with-temp-file dot-swank
      (insert (format ";;; %s\n;;; %s\n" dot-swank
                      (format-time-string "%Y-%m-%dT%T%z\n"))
              (format (concat "(unless (find-package 'swank-loader)\n"
                              "  (load #P\"%s\"))") ldr-swank)))
    (if w-msg-user
        (progn
          (message (concat ":FUNCTION `quicklisp-write-dot-swank-loader' "
                         "-- wrote:\n:FILE \"%s\"\n"
                         ":FINDS-FILE \"%s\"") dot-swank ldr-swank)
          dot-swank)
      dot-swank)))
;;
;;; :TEST-ME (quicklisp-write-dot-swank-loader)
;;; :TEST-ME (quicklisp-write-dot-swank-loader "~/quicklisp/")
 
;;; ==============================
;;; :CHANGESET 1929
;;; :CREATED <Timestamp: #{2010-06-30T19:29:49-04:00Z}#{10263} - by MON KEY>
(defun quicklisp-write-dot-swank-loader-if (&optional w-swank-loader-list
                                                      ql-path w-msg-user)
  "Write a new .swank-loader.lisp if current one does not exist.\n
Optional arg W-SWANK-LOADER-LIST is as per `quicklisp-write-dot-swank-loader'.\n
Optional arg QL-PATH is directory namestring of an alternate path to the
quicklisp directory as per `quicklisp-find-slime'./n
When optional arg W-MSG-USER is non-nil write a message to \"*Messages*\" buffer
indicating that file has changed as per `quicklisp-write-dot-swank-loader'.\n
:SEE-ALSO `slime-backend',`quicklisp-find-slime',
`quicklisp-current-swank-loader', `quicklisp-write-dot-swank-loader',
`quicklisp-dot-swank-current-p', `*quicklisp-path*'.\n►►►"
  (let ((swk-ldr-lst 
         (or w-swank-loader-list
             (quicklisp-current-swank-loader ql-path))))
    (if (or (null (caar swk-ldr-lst))
            (null (car (quicklisp-dot-swank-current-p swk-ldr-lst))))
        (quicklisp-write-dot-swank-loader swk-ldr-lst nil w-msg-user)
      (cdar swk-ldr-lst))))
;;
;;; :TEST-ME (quicklisp-write-dot-swank-loader-if)
;;; :TEST-ME (quicklisp-write-dot-swank-loader-if nil nil t)
;;; :TEST-ME (quicklisp-write-dot-swank-loader-if nil "~/quicklisp/" t)

;;; ==============================
;;; :CHANGESET 1929
;;; :CREATED <Timestamp: #{2010-06-30T17:54:15-04:00Z}#{10263} - by MON KEY>
(defun quicklisp-dot-swank-current-p (&optional w-swank-loader-list ql-path)
  "Test if the swank-loader.lisp is current.\n
Search in file returned by `quicklisp-current-swank-loader' e.g.
<QUICKLISP-PATH>/.swank-loader.lisp for a swank-loader filename which
matches the most current Quicklisp dists slime/swank.\n
Optional arg W-SWANK-LOADER-LIST is as per `quicklisp-write-dot-swank-loader'.\n
Optional arg QL-PATH is directory namestring of an alternate path to the
quicklisp directory as per `quicklisp-find-slime'./n
:EXAMPLE\n\n\(quicklisp-dot-swank-current-p\)\n
\(quicklisp-dot-swank-current-p nil \"~/quicklisp/\"\)\n
\(quicklisp-dot-swank-current-p
 \(quicklisp-current-swank-loader \"~/quicklisp/\"\)\)\n
:SEE-ALSO `slime-backend', `quicklisp-find-slime',
`quicklisp-current-swank-loader', `quicklisp-write-dot-swank-loader',
`quicklisp-write-dot-swank-loader-if', `*quicklisp-path*'.\n►►►"
  ;; Assumes the swank-loader.lisp path is a namestring.
  (let ((ql-cur-slm (or w-swank-loader-list
                        (quicklisp-current-swank-loader ql-path))))
    (when (caar ql-cur-slm)
      (with-temp-buffer 
        (insert-file-contents (cdar ql-cur-slm))
        (search-forward-regexp (caddr ql-cur-slm) nil t)
        (if (> (point) (buffer-end 0))
            `(t . ,(caddr ql-cur-slm))
          (if (progn
                (goto-char (buffer-end 0))
                (search-forward-regexp "/.*/swank-loader.lisp" nil t))
              `(nil . ,(match-string-no-properties 0))))))))
;;
;;; :TEST-ME (quicklisp-dot-swank-current-p)
;;; :TEST-ME (quicklisp-dot-swank-current-p nil "~/quicklisp/")
;;; :TEST-ME (quicklisp-dot-swank-current-p
;;;            (quicklisp-current-swank-loader "~/quicklisp/")))


;;; :PASTED (URL `http://paste.lisp.org/+2EGX')
;;; quicklisp completing-read systems.txt -> hash-table
;;; ==============================
;;; :CREATED <Timestamp: #{2010-06-30T23:12:26-04:00Z}#{10263} - by MON KEY>
(defvar *quicklisp-systems* nil
  "Hash-table of Quicklisp systems.\n
:EXAMPLE\n\n\(hash-table-size *quicklisp-systems*\)\n
\(hash-table-count *quicklisp-systems*\)\n
\(hash-table-test *quicklisp-systems*\)\n
:SEE-ALSO `quicklisp-system-complete', `quicklisp-hash-system-completions',
`quicklisp-hash-system-completions-slime-loadtime', `*quicklisp-path*'.\n►►►")
;;
(unless (bound-and-true-p *quicklisp-systems*)
  (setq *quicklisp-systems* (make-hash-table :test 'equal)))

;;; ==============================
;;; :NOTE Can/should this leverage `lazy-completion-table'?
;;; :CREATED <Timestamp: #{2010-06-30T23:12:22-04:00Z}#{10263} - by MON KEY>
(defun quicklisp-hash-system-completions (&optional force-rehash)
  "Hash all system names in file \"quicklisp/index/systems.txt\".\n
Return number of system names hashed.\n
When optional arg FORCE-REHASH is non-nil clear the `*quicklisp-systems*'
hash-table, this effectivley forces a rehash of existing systems.\n
:SEE-ALSO `quicklisp-system-complete',
`quicklisp-hash-system-completions-slime-loadtime', `*quicklisp-systems*',
`*quicklisp-path*'.\n►►►"
  (when force-rehash (clrhash *quicklisp-systems*))
  (if (< (hash-table-count *quicklisp-systems*) 67) ;don't branch needlesly
      (let ((ql-sys-txt ;; :WAS pre-2010/09/01 (concat *quicklisp-path* "index/systems.txt")) 
             (expand-file-name "dists/quicklisp/systems.txt" *quicklisp-path*))
            (ql-sys-bfr (upcase (symbol-name '*quicklisp-systems*)))
            (rd-reading t)
            rd-got)
        (if (not (file-exists-p ql-sys-txt))
            (error (concat ":FUNCTION `quicklisp-hash-system-completions' "
                           "-- non-existent file: %s")
                   ql-sys-txt)
          (with-current-buffer (get-buffer-create ql-sys-bfr)
            (save-excursion (insert-file-contents ql-sys-txt))
            ;; Remove the comment at BOF Emacs lisp doesn't have `#' read syntax :(
            ;; "# project system-file system-name [dependency1..dependencyN]"
            (flush-lines "^#.*$" (buffer-end 0) (buffer-end 1)))
          (unwind-protect
              (ignore-errors
                (while rd-reading
                  (setq rd-got (read (get-buffer ql-sys-bfr)))
                  (if (null rd-got)
                      (setq rd-reading nil)
                    (unless (gethash rd-got *quicklisp-systems*)
                      (puthash (format "%s" rd-got) rd-got *quicklisp-systems*)))))
            (with-current-buffer (get-buffer ql-sys-bfr)
              (kill-buffer (current-buffer))))
          (hash-table-count *quicklisp-systems*)))
    (hash-table-count *quicklisp-systems*)))
;;
;;; :TEST-ME (quicklisp-hash-system-completions)
;;; :TEST-ME (quicklisp-hash-system-completions 'force-rehash)

;;; ==============================
;;; :CREATED <Timestamp: #{2010-06-30T23:12:53-04:00Z}#{10263} - by MON KEY>
(defun quicklisp-system-complete (&optional insrtp intrp)
  "A `completing-read' for currenlty provided quicklisp systems.\n
When called-interactively or ptional arg INSRTP is non-nil insert retrun value
in current-buffer moving point.\n
:EXAMPLE\n\n(quicklisp-system-complete\)\n
:SEE-ALSO `mon-help-CL-symbols', `mon-help-CL-pkgs',
`quicklisp-hash-system-completions', `*quicklisp-systems*'.\n►►►"
  (interactive "i\np")
  (let ((ql-sys-cmplt 
         (gethash 
          (completing-read (concat ":FUNCTION `quicklisp-system-complete' "
                                   " -- Quicklisp system name: ")
                           *quicklisp-systems*)
          *quicklisp-systems*)))
    (if (or insrtp intrp)
        (prin1 ql-sys-cmplt (current-buffer))
      ql-sys-cmplt)))
;; 
;;; :TEST-ME (quicklisp-system-complete)
;;; :TEST-ME (quicklisp-system-complete t)
;;; :TEST-ME (quicklisp-system-complete nil t)

;;; ==============================
;;; :CHANGESET 1933
;;; :CREATED <Timestamp: #{2010-07-01T13:29:02-04:00Z}#{10264} - by MON KEY>
(defun quicklisp-hash-system-completions-slime-loadtime ()
  "Initialize `quicklisp-hash-system-completions' when loading `slime'.\n
:SEE-ALSO `quicklisp-hash-system-completions', `quicklisp-system-complete',
`quicklisp-hash-system-completions-slime-loadtime'.\n►►►"
  (let* (;; Find the current slime in `load-path'.
         (get-cur-slm (locate-library "slime"))
         ;; If we found it, make sure it has an ".el" extension not ".elc"
         (cur-slm-as-el
          (when get-cur-slm 
            (let* ((prpr-ext  (file-name-extension get-cur-slm t))
                   (prpr-ext-lst 
                    (if prpr-ext
                        (car (nreverse (append prpr-ext nil)))
                      (error 
                       (concat ":FUNCTION `quicklisp-hash-system-completions-slime-loadtime' "
                               "-- no extension for slime library:\n%s")
                       get-cur-slm))))
              (file-truename
               (concat (file-name-sans-extension get-cur-slm) 
                       (case prpr-ext-lst ;; its either l or c 
                         (108 prpr-ext) 
                         (99  ".el")
                         (t (error 
                             (concat ":FUNCTION `quicklisp-hash-system-completions-slime-loadtime' "
                                     "-- unknown extension for slime library:\n%s") get-cur-slm))))))))
         ;; Find the current slime/swank dist dir in `*quicklisp-path*'
         (ql-cur-slm-dr (cadr (quicklisp-current-swank-loader)))
         ;; If found: i) make directory namestring; ii) tack on /slime.el 
         (ql-cur-slm-fnm (when ql-cur-slm-dr 
                           (concat (file-name-as-directory ql-cur-slm-dr) "slime.el")))
         ;; Make sure the slime.el file exists and return its truename.
         (ql-cur-slm-if (when (and ql-cur-slm-fnm (file-exists-p ql-cur-slm-fnm))
                          (file-truename ql-cur-slm-fnm))))
    (when (and ql-cur-slm-if cur-slm-as-el (string-equal ql-cur-slm-if cur-slm-as-el))
      (ignore-errors (quicklisp-hash-system-completions)))))
;;
(eval-after-load "slime" '(quicklisp-hash-system-completions-slime-loadtime))

;;; ==============================
;; :QUICKLISP-SLIME-PATH 
;; :NOTE To test above w/ quicklisp + sbcl
;; ,---- :UNCOMMENT-BELOW-TO-TEST
;; | (setq inferior-lisp-program (executable-find "sbcl"))  
;; | (set-language-environment "UTF-8")
;; |   ;; (setq slime-net-coding-system 'iso-latin-1-unix)
;; | (setq slime-net-coding-system 'utf-8-unix)
;; | (setq slime-backend (quicklisp-write-dot-swank-loader-if))
;; | (let ((this-swank (quicklisp-current-swank-loader)))
;; |   (add-to-list 'load-path (concat (cadr this-swank) "/contrib"))
;; |   (add-to-list 'load-path  (cadr this-swank))
;; |   (add-to-list 'load-path  (cadr this-swank))
;; |   (load (locate-library "slime")))
;; `----

;; ==============================

;;; ==============================
;;; :CHANGESET 1917
;;; :CREATED <Timestamp: #{2010-06-23T20:38:32-04:00Z}#{10253} - by MON KEY>
(defun mon-slime-setup-init ()
  "Configure some slime related setings at init.\n
Evaluates `set-language-environment' with \"UTF-8\".\n
Adds value of `quicklisp-current-swank-loader' to `load-path'\n
Sets slime-backend to value of `quicklisp-write-dot-swank-loader-if'
Sets the following variables:\n
 `inferior-lisp-program' `slime-net-coding-system'`slime-truncate-lines'
Attempts to disable `slime-use-autodoc-mode' in various ways \(mostly w/out success\).\n
Requires slime package.\n
Evaluates `slime-setup', `slime-require'.\n
:SEE-ALSO `mon-set-lisp-init', `mon-keybind-slime',
`mon-help-CL-slime-keys', `slime-cheat-sheet', `mon-slime-setup-init',
`mon-keybind-lisp-interaction-mode', `mon-keybind-emacs-lisp-mode',
`slime-setup-contribs', `slime-load-contribs', `slime-required-modules'.\n►►►"
  ;; :NOTE sb-ext:*runtime-pathname* returns the current sbcl runtime
  (set-language-environment "UTF-8")
  ;; :QUICKLISP-SLIME-PATH
  (let ((this-swank (quicklisp-current-swank-loader)))
    (add-to-list 'load-path (concat (cadr this-swank) "/contrib"))
    (add-to-list 'load-path  (cadr this-swank))
    (add-to-list 'load-path  (cadr this-swank))
    ) ;;(load (locate-library "slime")))
  ;; ==============================
  ;; :SLIME-LOADS-FOR-CLBUILD
  ;; :CLBUILD-SLIME-PATH
  ;; Add this to your ~/.emacs to use clbuild and its slime:
  ;; possibly controversial as a global default, but shipping a lisp
  ;; that dies trying to talk to slime is stupid, so:
  ;; (set-language-environment "UTF-8")
  ;; ;; (setq slime-net-coding-system 'iso-latin-1-unix)
  ;; (setq slime-net-coding-system 'utf-8-unix)
  ;; (add-to-list 'load-path "/<CL-SYSTEMS-PATH>/clbuild/source/slime/contrib" t)
  ;; (add-to-list 'load-path "/<CL-SYSTEMS-PATH>/clbuild/source/slime" t)
  ;; (setq slime-backend "/<CL-SYSTEMS-PATH>/clbuild/.swank-loader.lisp")
  ;; (load "~/HG-Repos/CL-repo-HG/CL-SYSTEMS/clbuild/source/slime/slime")
  ;; (setq inferior-lisp-program "/<CL-SYSTEMS-PATH>/clbuild/clbuild lisp")
  ;; ==============================
  (custom-set-variables
   '(inferior-lisp-program (executable-find "sbcl"))  
   ;; '(slime-net-coding-system 'iso-latin-1-unix))
   '(slime-net-coding-system 'utf-8-unix)
   '(slime-backend (quicklisp-write-dot-swank-loader-if))
   '(slime-truncate-lines nil)
   ;; :NOTE Setting `slime-use-autodoc-mode' is about the only to stop the
   ;;       insanity once it starts.
   '(slime-use-autodoc-mode nil)
   '(slime-autodoc-delay 2)
   '(slime-autodoc-accuracy-depth 4) ;; The default: 10 is prob. way to much IMHO
   )
  (require 'slime)
  ;; (require 'slime-autoloads)
  (slime-setup '(slime-fancy slime-sbcl-exts slime-asdf slime-tramp			     
                 slime-presentation-streams ;slime-highlight-edits 
		 slime-repl slime-scratch ))
  (slime-require :swank-sbcl-exts)
  (slime-require :swank-listener-hooks)
  ;; :NOTE `slime-setup-contribs' is a function and a variable
  ;; `slime-setup' <- `slime-setup-contribs' 
  ;; The slime-fancy.el package is one `define-slime-contrib' macro form that
  ;; requires, compiles, and loads certain slime/contrib packages. 
  ;; For each slime-fancy package, it also builds the functions: 
  ;;  `slime-<SOME-CONTRIB>-init' and `slime-<SOME-CONTRIB>-unload':
  ;; slime-fancy loads the following packages:
  ;; slime-repl             ;; (require 'slime-repl)
  ;; slime-autodoc          ;; (require 'slime-autodoc)
  ;; slime-c-p-c            ;; (require 'slime-c-p-c)
  ;; slime-editing-commands ;; (require 'slime-editing-commands)
  ;; slime-fancy-inspector  ;; (require 'slime-fancy-inspector)
  ;; slime-fuzzy            ;; (require 'slime-fuzzy)
  ;; slime-presentations    ;; (require 'slime-presentations)
  ;; slime-scratch          ;; (require 'slime-scratch)
  ;; slime-references       ;; (require 'slime-references)
  ;; slime-package-fu       ;; (require 'slime-package-fu)
  ;; slime-fontifying-fu    ;; (require 'slime-fontifying-fu)
  ;;
  ;; :WHEN-SBCL
  ;; (require 'slime-sbcl-exts) (slime-sbcl-exts-init)                                          
  ;; ==============================
  ;; Not loaded by `slime-fancy'
  ;; (require 'slime-highlight-edits) (slime-highlight-edits-init)
  ;; (require 'slime-presentation-streams) (slime-presentation-streams-init)
  ;; (require 'slime-tramp) (slime-tramp-init)
  ;; (require 'slime-asdf) (slime-asdf-init)
  ;; (require 'slime-mdot-fu) (slime-mdot-fu-init)
  ;;; ==============================
  ;; (slime)
  ;;
  ;; (remove-hook 'slime-mode-hook  
  (add-hook 'slime-mode-hook 
            (function (lambda ()
                        (set (make-local-variable 'slime-use-autodoc-mode) nil))))

  ;;(remove-hook 'slime-mode-hook 
  ;; (add-hook 'slime-mode-hook 
  ;;              (function (lambda () (slime-list-threads))))
  
                                        
  ;; (remove-hook 'slime-load-hook 
  ;; (add-hook 'slime-load-hook 
  ;;           (when (buffer-local-value 'slime-highlight-edits-mode (current-buffer))
  ;;             (slime-highlight-edits-mode -1)))
  

  ;; (remove-hook 'slime-mode-hook
  (add-hook 'slime-mode-hook
            (function (lambda () 
                        (set (make-local-variable 'lisp-indent-function)  
                             'common-lisp-indent-function))))
  ;;
  (add-to-list 'auto-mode-alist  '("\\.kif\\'" . lisp-interaction-mode))
  )

;; (mon-slime-setup-init)



;; :WAS `slime-fancy'
;; (define-slime-contrib mon-slime-fancy
;;   "Make SLIME fancy."
;;   (:authors "Matthias Koeppe  <mkoeppe@mail.math.uni-magdeburg.de>"
;;             "Tobias C Rittweiler <tcr@freebits.de>")
;;   (:license "GPL")
;;   (:slime-dependencies slime-repl
;;                        slime-autodoc
;;                        slime-c-p-c
;;                        slime-editing-commands
;;                        slime-fancy-inspector
;;                        slime-fuzzy
;;                        slime-presentations
;;                        slime-scratch
;;                        slime-references     ;; SBCL-ONLY `slime-sbcl-manual-root'
;;                        slime-package-fu
;;                        slime-fontifying-fu)

;;   (:on-load  (slime-repl-init)
;;              (slime-autodoc-init)
;;              (slime-c-p-c-init)
;;              (slime-editing-commands-init)
;;              (slime-fancy-inspector-init)
;;              (slime-fuzzy-init)
;;              (slime-presentations-init)
;;              (slime-scratch-init)
;;              (slime-references-init)
;;              (slime-package-fu-init)
;;              (slime-fontifying-fu-init)))

;;; ==============================
;;; :CHANGESET 1978
;;; :CREATED <Timestamp: #{2010-07-14T17:41:00-04:00Z}#{10283} - by MON KEY>
;; (defun slime-auto-doc-mode-STFU ()
;;   "Make `slime-autodoc-mode' STFU. Silence his little buddy `eldoc-mode' too.\n
;; :EXAMPLE\n\n
;; :SEE-ALSO .\n►►►"
;;   (interactive)
;;   (when (buffer-local-value 'slime-autodoc-mode (current-buffer))
;;     ;(set (make-local-variable  'slime-autodoc-mode) nil)
;;     ;(set (make-local-variable 'eldoc-mode) nil)))
    



;;; ==============================
;;; :NOTE This can't be installed until >= Guile 1.9.12 is installed.
;;
;; :FILE geiser-install.el
;; (setq geiser-scheme-dir 
;;     (expand-file-name "geiser/scheme" *mon-site-lisp-root*))
;;
;; :FILE geiser-custom.el
;; `geiser-custom--defcustom' 
;;
;; :FILE geiser-guile.el
;; `geiser-guile-init-file' "~/.guile-geiser"
;; `define-geiser-implementation' 
;; `geiser-guile-binary'

;; 
;; (add-to-list 'load-path 
;;              (expand-file-name "company" *mon-site-lisp-root*))

;; (add-to-list 'load-path 
;;              (expand-file-name "geiser/elisp" *mon-site-lisp-root*))

;; (custom-set-variables 
;;  '(geiser-scheme-dir 
;;    (expand-file-name "geiser/scheme" *mon-site-lisp-root*))

;;  '(geiser-guile-binary (expand-file-name "guile-1.9.11/bin/guile1.9.11" (getenv "LOCAL_MON")))
;;  '(geiser-active-implementations '(guile)) ;; '(guile racket))
;;  '(geiser-default-implementation 'guile))
;;
;; (require 'company)
;; (load-library "geiser.el")
;;
;; (require 'quack) ;; NO, Don't do it!

;;; ==============================
(provide 'slime-loads-GNU-clbuild)
;;; ==============================

 
;; Local Variables:
;; generated-autoload-file: "./mon-loaddefs.el"
;; End:

;;; ====================================================================
;;; slime-loads-GNU-clbuild.el ends here
;;; EOF
