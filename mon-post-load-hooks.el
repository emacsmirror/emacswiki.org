;;; mon-post-load-hooks.el --- fncns to perform after initializing MON Emacsen
;; -*- mode: EMACS-LISP; no-byte-compile: t -*-

;;; ================================================================
;; Copyright © 2010-2011 MON KEY. All rights reserved.
;;; ================================================================

;; FILENAME: mon-post-load-hooks.el
;; AUTHOR: MON KEY
;; MAINTAINER: MON KEY
;; CREATED: 2010-03-24T11:27:54-04:00Z
;; VERSION: 1.0.0
;; COMPATIBILITY: Emacs23.*
;; KEYWORDS: environment, installation, site, local,

;;; ================================================================

;;; COMMENTARY: 

;; =================================================================
;; DESCRIPTION:
;; mon-post-load-hooks provides fncns to perform after initializing MON Emacsen.
;;
;; FUNCTIONS:►►►
;; `mon-run-post-load-hooks', `mon-purge-cl-symbol-buffers-on-load',
;; `mon-purge-emacs-temp-files-on-quit', `mon-purge-doc-view-cache-directory',
;; `mon-purge-thumbs-directory', `mon-purge-slime-swank-port-file',
;; `mon-purge-temporary-file-directory', `mon-purge-tramp-persistency-file',
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
;; `*mon-post-load-hook-trigger-buffer*',
;; `*mon-purge-emacs-temp-file-dir-fncns*',
;; `*mon-post-load-hooks-xrefs*'
;;
;; GROUPS:
;; `mon-post-load-hooks'
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
;; SNIPPETS:
;;
;; REQUIRES:
;;
;; THIRD-PARTY-CODE:
;;
;; URL: http://www.emacswiki.org/emacs/mon-post-load-hooks.el
;; FIRST-PUBLISHED: <Timestamp: #{2010-03-27T22:59:00-04:00Z} - by MON>
;;
;; EMACSWIKI: { URL of an EmacsWiki describing mon-post-load-hooks. }
;;
;; FILE-CREATED:
;; <Timestamp: #{2010-03-24T11:27:54-04:00Z}#{10123} - by MON>
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
;; Copyright © 2010-2011 MON KEY 
;;; ==============================

 
;;; CODE:

 
(eval-when-compile (require 'cl))

(unless (and (intern-soft "*IS-MON-OBARRAY*")
             (bound-and-true-p *IS-MON-OBARRAY*))
(setq *IS-MON-OBARRAY* (make-vector 17 nil)))


;;; ==============================
;;; :CHANGESET 2389
;;; :CREATED <Timestamp: #{2011-01-12T19:04:38-05:00Z}#{11023} - by MON KEY>
(defgroup mon-post-load-hooks nil
  "Customization group for variables and functions of :FILE mon-post-load-hooks.el\n
:SEE-ALSO .\n►►►"
  ;; :prefix "<PREFIX>"
  :link '(url-link 
          :tag ":EMACSWIKI-FILE (URL `http://www.emacswiki.org/emacs/mon-post-load-hooks.el')" 
          "http://www.emacswiki.org/emacs/mon-post-load-hooks.el")
  :link '(emacs-library-link 
          :tag ":FILE mon-post-load-hooks.el" 
          "mon-post-load-hooks.el")
  :group 'mon-base)

;;; ==============================
;;; :CHANGESET 2389
;;; :CREATED <Timestamp: #{2011-01-12T19:04:21-05:00Z}#{11023} - by MON KEY>
(defcustom *mon-post-load-hooks-xrefs* 
  '(mon-purge-cl-symbol-buffers-on-load
    mon-run-post-load-hooks
    mon-purge-doc-view-cache-directory
    mon-purge-thumbs-directory
    mon-purge-slime-swank-port-file
    mon-purge-tramp-persistency-file
    mon-purge-temporary-file-directory
    *mon-post-load-hook-trigger-buffer*
    *mon-post-load-hooks-xrefs*)
  "Xrefing list of mon Emacs startup/quit related symbols.\n
The symbols contained of this list are defined in :FILE mon-post-load-hooks.el\n
:SEE-ALSO `*mon-default-loads-xrefs*', `*mon-default-start-loads-xrefs*',
`*mon-dir-locals-alist-xrefs*', `*mon-keybindings-xrefs*',
`*mon-testme-utils-xrefs*', `*mon-button-utils-xrefs*',
`*mon-buffer-utils-xrefs*', `*mon-line-utils-xrefs*', `*mon-plist-utils-xrefs*'
`*mon-seq-utils-xrefs*', `*mon-string-utils-xrefs*', `*mon-type-utils-xrefs*',
`*mon-window-utils-xrefs*', `*naf-mode-xref-of-xrefs*', `*mon-slime-xrefs*',
`*naf-mode-faces-xrefs*', `*naf-mode-date-xrefs*', `*mon-ulan-utils-xrefs*',
`*mon-xrefs-xrefs'.\n►►►"
  :type '(repeat symbol)
  :group 'mon-post-load-hooks
  :group 'mon-xrefs)

;;; ==============================
;;; :TODO incorporate these:
;;; `mon-check-feature-for-loadtime', `mon-after-mon-utils-loadtime',
;;; `mon-set-register-tags-loadtime', `mon-bind-iptables-vars-at-loadtime',
;;; `mon-bind-cifs-vars-at-loadtime', `mon-CL-cln-colon-swap',
;;; `mon-bind-nefs-photos-at-loadtime', `mon-help-utils-loadtime',
;;; `mon-help-utils-CL-loadtime', `mon-bind-doc-help-proprietery-vars-at-loadtime'

 
;;; ==============================
;; :LOAD-EMACS
;;; ==============================

;;; ==============================
;;; :CREATED <Timestamp: #{2010-03-23T22:24:16-04:00Z}#{10122} - by MON KEY>
(defun mon-purge-cl-symbol-buffers-on-load ()
  "Remove buffers left behind at loadtime init by the CL hspec snarfage.\n
E.g. those left behind by the slime pacakges hyperspec.el\n
Also, removes loadtime buffers generated by mon-doc-help-cl.el routines.\n
Names of buffers removed at loadtime when present:\n
 \"Map_Sym.txt\" \"Map_IssX.txt\"  
 \"Issue-Cross-Refs.text\" \"Issue-Writeups.text\"
 \"*CL-EXT-PKG-MAP*\"
 \(get '*mon-help-CL-ext-pkg-map* 'mon-help-CL-pkgs-buffer-name\)
:NOTE Evaluated post Emacs init with `mon-run-post-load-hooks'.\n
:SEE-ALSO `mon-set-common-lisp-hspec-init',
`mon-purge-emacs-temp-files-on-quit', `mon-check-feature-for-loadtime',
`mon-after-mon-utils-loadtime', `mon-set-register-tags-loadtime',
`mon-bind-iptables-vars-at-loadtime', `mon-bind-cifs-vars-at-loadtime',
`mon-CL-cln-colon-swap', `mon-bind-nefs-photos-at-loadtime',
`mon-help-utils-loadtime', `mon-help-utils-CL-loadtime',
`mon-bind-doc-help-proprietery-vars-at-loadtime',
`mon-purge-doc-view-cache-directory', `mon-purge-thumbs-directory',
`mon-purge-temporary-file-directory', `mon-htmlfontify-dir-purge-on-quit',
`mon-its-all-text-purge-on-quit', `*mon-purge-emacs-temp-file-dir-fncns*',
`*mon-purge-emacs-temp-file-dir-fncns*',
`*mon-post-load-hook-trigger-buffer*'.\n►►►"
  (when ;;(and 
      (or (and (intern-soft "IS-MON-P-GNU" obarray) ;; *IS-MON-OBARRAY*
               (bound-and-true-p IS-MON-P-GNU))
          (and (intern-soft "IS-MON-P-W32" obarray) ;; *IS-MON-OBARRAY*
               (bound-and-true-p IS-MON-P-32))
          (or (bound-and-true-p common-lisp-hyperspec-root)
              (bound-and-true-p common-lisp-hyperspec-issuex-table)
              (bound-and-true-p common-lisp-hyperspec-symbol-table)))
    (dolist (gb `("*CL-EXT-PKG-MAP*"
                  ;; "*MON-HELP-CL-EXT-PKG-MAP*"
                  ,(get '*mon-help-CL-ext-pkg-map* 'mon-help-CL-pkgs-buffer-name)
                  ;; hspec-V7
                  "Map_Sym.txt" "Map_IssX.txt" 
                  ;; hspec-V3
                  "Issue-Cross-Refs.text" "Issue-Writeups.text"))
      (when (get-buffer gb)
        (kill-buffer (get-buffer gb))))))

;;; ==============================
;;; :CREATED <Timestamp: #{2010-03-23T22:58:12-04:00Z}#{10122} - by MON KEY>
(defvar *mon-post-load-hook-trigger-buffer* nil
  "A buffer with a locally bound `kill-buffer-hook'.\n
When this buffer exists it is killed by `mon-run-post-load-hooks'.\n
:SEE-ALSO  `mon-purge-cl-symbol-buffers-on-load'
`mon-check-feature-for-loadtime', `mon-after-mon-utils-loadtime',
`mon-set-register-tags-loadtime', `mon-bind-iptables-vars-at-loadtime',
`mon-bind-cifs-vars-at-loadtime', `mon-CL-cln-colon-swap',
`mon-bind-nefs-photos-at-loadtime', `mon-help-utils-loadtime',
`mon-help-utils-CL-loadtime', `mon-bind-doc-help-proprietery-vars-at-loadtime'.\n►►►")
;;
(unless (bound-and-true-p *mon-post-load-hook-trigger-buffer*)
  (setq *mon-post-load-hook-trigger-buffer*
        (upcase (symbol-name '*mon-post-load-hook-trigger-buffer*))))
;;
;;;(progn (makunbound '*mon-post-load-hook-trigger-buffer*)
;;;       (unintern "*mon-post-load-hook-trigger-buffer*" obarray) )

;;; ==============================
;;; :CREATED <Timestamp: #{2010-03-23T22:53:41-04:00Z}#{10122} - by MON KEY>
(defun mon-run-post-load-hooks ()
  "Kill the buffer named by the variable `*mon-post-load-hook-trigger-buffer*'.\n
Killing this buffer will run that buffer's local `kill-buffer-hook'.\n
:SEE-ALSO `mon-run-post-load-hooks', `mon-purge-cl-symbol-buffers-on-load',
`mon-check-feature-for-loadtime', `mon-after-mon-utils-loadtime',
`mon-set-register-tags-loadtime', `mon-bind-iptables-vars-at-loadtime',
`mon-bind-cifs-vars-at-loadtime', `mon-CL-cln-colon-swap',
`mon-bind-nefs-photos-at-loadtime', `mon-help-utils-loadtime',
`mon-help-utils-CL-loadtime', `mon-bind-doc-help-proprietery-vars-at-loadtime'.\n►►►"
  (unwind-protect
      ;;
      (with-current-buffer (get-buffer-create *mon-post-load-hook-trigger-buffer*)
        ;;
        ;; :NOTE What instead setting a local variable with a function object as its value
        ;;       and then doing running that on the kill-buffer-hook:
        ;; (set (make-local-variable '<LOCAL-VAR>) #'(lambda () (... doing local stuff here ...))
        ;; (add-hook 'kill-buffer-hook (run-hooks '<LOCAL-VAR>) t t)
        ;; Or, (add-hook 'kill-buffer-hook #'(lambda (&rest x) (run-hook-with-args '<LOCAL-VAR> x)))
        ;;
        ;; IS-MON-SYSTEM-P        
        (when (and (intern-soft "IS-MON-SYSTEM-P" obarray) ;; *IS-MON-OBARRAY*
                   ;; (bound-and-true-p IS-MON-P-GNU))
                   (bound-and-true-p IS-MON-SYSTEM-P))
          ;; IS-MON-P-GNU
          (when (and (intern-soft "IS-MON-P-GNU" obarray) ;; *IS-MON-OBARRAY*
                     (bound-and-true-p IS-MON-P-GNU))
            (mon-help-utils-CL-loadtime t)
            (add-hook 'kill-buffer-hook 'mon-purge-cl-symbol-buffers-on-load nil t)
            ;; (remove-hook 'kill-buffer-hook 'mon-update-tags-tables-loadtime  t)
            (add-hook 'kill-buffer-hook 'mon-update-tags-tables-loadtime nil t))
          ;; IS-W32-P
          (when (and (intern-soft "IS-W32-P" obarray) ;; *IS-MON-OBARRAY*
                     (bound-and-true-p IS-W32-P)
                     (fboundp  'mon-maximize-frame-w32))
            (add-hook 'kill-buffer-hook 'mon-maximize-frame-w32 nil t))
          ;; Instantiate the tags-table w/ visit-tags-table on 
          (add-hook 'kill-buffer-hook 
                    #'(lambda ()
                        (let ((default-directory 
                                (or (caar *mon-tags-table-list*) default-directory)))
                          (when (and (visit-tags-table default-directory) 
                                     ;; (visit-tags-table (caar *mon-tags-table-list*))
                                     ;; (visit-tags-table-buffer t)
                                     tags-file-name)
                            (mon-message :msg-spec '(":FUNCTION `mon-run-post-load-hooks' "
                                                     "-- evaluated `visit-tags-table-buffer' at loadtime"))
                            (mon-message :msg-spec'(":FUNCTION `mon-run-post-load-hooks' "
                                                    "-- value of current `tags-file-name': #P%S")
                                         :msg-args tags-file-name))))
                    t t))
        ;; :DEBUGGING
        ;; (buffer-local-value 'kill-buffer-hook (current-buffer))
        ;; (prin1 (buffer-local-variables (current-buffer)) (current-buffer))
        ;;
        (when (get-buffer *mon-post-load-hook-trigger-buffer*)
          (kill-buffer (get-buffer *mon-post-load-hook-trigger-buffer*))))
    ;; PROTECTED-FORM
    (when (get-buffer *mon-post-load-hook-trigger-buffer*)
      (kill-buffer (get-buffer *mon-post-load-hook-trigger-buffer*)))
    ))

 
;;; ==============================
;; :KILL-EMACS
;;; ==============================

;;; ==============================
;; :TODO Relocate/reorganize `*emacs2html-temp*' and subdirs.
;;; Right now `mon-htmlfontify-buffer-to-firefox' writes to top-level.
;;; That dir should be partitioned as per those in other fncns below.

;;; ==============================
;;; (declare-function 'mon-remove-if "mon-utils" '(rmv-if-predicate rmv-list))
;;; :CREATED <Timestamp: #{2010-04-01T15:58:59-04:00Z}#{10134} - by MON KEY>
(defun mon-purge-doc-view-cache-directory ()
  "Remove all files in `doc-view-cache-directory'.\n
:NOTE When `IS-MON-SYSTEM-P', this function is evaluated by
`mon-purge-emacs-temp-files-on-quit' on the `kill-emacs-hook'.\n
:SEE-ALSO `doc-view-current-cache-dir', `mon-run-post-load-hooks',
`mon-purge-doc-view-cache-directory', `mon-purge-thumbs-directory',
`mon-purge-temporary-file-directory', `mon-htmlfontify-dir-purge-on-quit',
`mon-its-all-text-purge-on-quit', `mon-purge-cl-symbol-buffers-on-load',
`*mon-purge-emacs-temp-file-dir-fncns*'.\n►►►"
  (when (and (intern-soft "IS-MON-SYSTEM-P" obarray) ;; *IS-MON-OBARRAY*
             (bound-and-true-p IS-MON-SYSTEM-P)
             (file-exists-p doc-view-cache-directory)
             (file-directory-p doc-view-cache-directory))
    ;;; (delete-directory doc-view-cache-directory)))
    (dired-delete-file doc-view-cache-directory 'always)))

;;; ==============================
;; :CREATED <Timestamp: #{2010-04-01T15:59:12-04:00Z}#{10134} - by MON KEY>
(defun mon-purge-thumbs-directory ()
  "Remove all files in `thumbs-thumbsdir' with `thumbs-cleanup-thumbsdir'.\n
:NOTE When `IS-MON-SYSTEM-P', this function is evaluated by
`mon-purge-emacs-temp-files-on-quit' on the `kill-emacs-hook'.\n
:SEE-ALSO `mon-run-post-load-hooks', `mon-purge-doc-view-cache-directory',
`mon-purge-thumbs-directory', `mon-purge-temporary-file-directory',
`mon-htmlfontify-dir-purge-on-quit', `mon-its-all-text-purge-on-quit',
`*mon-purge-emacs-temp-file-dir-fncns*', `mon-purge-cl-symbol-buffers-on-load'.\n►►►"
  (when (and (intern-soft "IS-MON-SYSTEM-P" obarray) ;; *IS-MON-OBARRAY*
             (bound-and-true-p IS-MON-SYSTEM-P)
             (featurep 'thumbs))
    ;; (eval-when (compile load eval)
    ;;   (require 'thumbs))
    (let ((thumbs-thumbsdir-max-size 1))
      (thumbs-cleanup-thumbsdir))))

;;; ==============================
;;; :PREFIX "mpsspf-"
;;; :CHANGESET 1704
;;; :CREATED <Timestamp: #{2010-04-07T15:05:41-04:00Z}#{10143} - by MON KEY>
(defun mon-purge-slime-swank-port-file ()
  "Delete any `slime-swank-port-file's in `slime-temp-directory'.\n
:SEE-ALSO `slime-delete-swank-port-file', `mon-purge-temporary-file-directory',
`mon-purge-tramp-persistency-file'.\n►►►"
  (let ((mpsspf-fl (and 
                    (and (intern-soft "IS-MON-SYSTEM-P" obarray) ;; *IS-MON-OBARRAY*
                         (bound-and-true-p IS-MON-SYSTEM-P))
                    (intern-soft "slime-swank-port-file" obarray)
                    (fboundp 'slime-swank-port-file)
                    (slime-swank-port-file))))
    (when (and mpsspf-fl 
               (file-exists-p mpsspf-fl))
      (delete-file mpsspf-fl))))

;;; ==============================
;;; :CHANGESET 1721
;;; :CREATED <Timestamp: #{2010-05-07T15:05:49-04:00Z}#{10185} - by MON KEY>
(defun mon-purge-tramp-persistency-file ()
  "Delete the file with `tramp-persistency-file-name'.\n
:SEE info node `(tramp)Connection caching'.\n
:SEE-ALSO `tramp-compat-temporary-file-directory',
`mon-set-emacs-temp-file/dir-init', `mon-purge-temporary-file-directory',
`mon-purge-slime-swank-port-file', `mon-run-post-load-hooks',
`mon-purge-doc-view-cache-directory', `mon-purge-thumbs-directory',
`mon-purge-temporary-file-directory', `mon-htmlfontify-dir-purge-on-quit',
`mon-its-all-text-purge-on-quit', `mon-purge-cl-symbol-buffers-on-load',
`*mon-purge-emacs-temp-file-dir-fncns*'.\n►►►"
  (when (and (intern-soft "tramp-persistency-file-name" obarray) ;; *IS-MON-OBARRAY*
             (bound-and-true-p tramp-persistency-file-name))
    (when (file-exists-p tramp-persistency-file-name)
      (delete-file tramp-persistency-file-name))))

;;; ==============================
;;; :CREATED <Timestamp: #{2010-04-01T16:10:45-04:00Z}#{10134} - by MON KEY>
(defun mon-purge-temporary-file-directory ()
  "Remove all files in `temporary-file-directory'.\n
:NOTE When `IS-MON-SYSTEM-P', this function is evaluated by
`mon-purge-emacs-temp-files-on-quit' on the `kill-emacs-hook'.\n
:SEE-ALSO `null-device', `small-temporary-file-directory',
`monb-purge-doc-view-cache-directory', `mon-purge-thumbs-directory',
`mon-purge-temporary-file-directory', `mon-htmlfontify-dir-purge-on-quit',
`mon-its-all-text-purge-on-quit', `*mon-purge-emacs-temp-file-dir-fncns*',
`mon-run-post-load-hooks'.\n►►►"
  ;;(when IS-MON-SYSTEM-P
  (when (and (intern-soft "IS-MON-SYSTEM-P" obarray) ;; *IS-MON-OBARRAY*
             (bound-and-true-p IS-MON-SYSTEM-P))
    (let ((tfd (mon-remove-if 
                #'(lambda (f-chk) 
                    (string-match-p (concat temporary-file-directory "/\.\.?$") f-chk))
                (directory-files temporary-file-directory t))))
      (dolist (is-fl tfd tfd)
        (if (car (file-attributes is-fl))
            ;; (delete-directory is-fl t)
            (delete-directory is-fl)
            (delete-file is-fl))))))

;;; ==============================
;;; :CREATED <Timestamp: #{2010-04-01T18:17:55-04:00Z}#{10134} - by MON KEY>
(defvar *mon-purge-emacs-temp-file-dir-fncns* nil
  "A list of functions evald by `mon-purge-emacs-temp-files-on-quit'.\n
:NOTE When `IS-MON-SYSTEM-P' these are run on the `kill-emacs-hook'.\n
:SEE-ALSO `mon-purge-doc-view-cache-directory' `mon-purge-thumbs-directory',
`mon-purge-temporary-file-directory', `mon-htmlfontify-dir-purge-on-quit',
`mon-its-all-text-purge-on-quit'.\n►►►")
;;
(unless (and (intern-soft "*mon-purge-emacs-temp-file-dir-fncns*" obarray)
             (bound-and-true-p *mon-purge-emacs-temp-file-dir-fncns*))
  (setq *mon-purge-emacs-temp-file-dir-fncns*
        `(mon-purge-doc-view-cache-directory
          mon-purge-thumbs-directory
          mon-htmlfontify-dir-purge-on-quit
          ,(when (fboundp 'slime-swank-port-file)
                 'mon-purge-temporary-file-directory)
          mon-purge-tramp-persistency-file
          mon-its-all-text-purge-on-quit)))
;;
;;;(progn (makunbound '*mon-purge-emacs-temp-file-dir-fncns*)
;;;       (unintern  "*mon-purge-emacs-temp-file-dir-fncns*" obarray) )

;;; ==============================
;;; :CREATED <Timestamp: #{2010-03-29T20:46:44-04:00Z}#{10132} - by MON KEY>
(defun mon-purge-emacs-temp-files-on-quit ()
  "Purge various Emacs temporary files and directories.\n
Empty/Delete the following when the `kill-emacs-hook' is run:\n
:SEE-ALSO `mon-purge-doc-view-cache-directory', `mon-purge-thumbs-directory',
`mon-purge-temporary-file-directory', `mon-htmlfontify-dir-purge-on-quit',
`mon-its-all-text-purge-on-quit', `*mon-purge-emacs-temp-file-dir-fncns*'
`*mon-purge-emacs-temp-file-dir-fncns*', `mon-purge-slime-swank-port-file'.\n►►►"
  (when (and (intern-soft "IS-MON-SYSTEM-P" obarray) ;; *IS-MON-OBARRAY*
             (bound-and-true-p IS-MON-SYSTEM-P))
    (dolist (p-o-q *mon-purge-emacs-temp-file-dir-fncns*)
      (unless (null p-o-q)
        (funcall p-o-q)))))
;;
;; Now, add-hook to purge the directory on quit.
(when (and (intern-soft "IS-MON-SYSTEM-P" obarray) ;; *IS-MON-OBARRAY*
           (bound-and-true-p IS-MON-SYSTEM-P)
           (file-directory-p *emacs2html-temp*))
  (add-hook 'kill-emacs-hook 'mon-purge-emacs-temp-files-on-quit))
;;
;;; (remove-hook 'kill-emacs-hook 'mon-purge-emacs-temp-files-on-quit)

;;; ==============================
(eval-after-load "mon-utils"
  '(let ((message-log-max t))
     (mon-after-mon-utils-loadtime)
     (mon-run-post-load-hooks)))

;;; ==============================
(provide 'mon-post-load-hooks)
;;; ==============================

 
;; Local Variables:
;; generated-autoload-file: "./naf-mode/mon-loaddefs.el"
;; no-byte-compile: t
;; coding: utf-8
;; mode: EMACS-LISP
;; End:

;;; ====================================================================
;;; mon-post-load-hooks.el ends here
;;; EOF
