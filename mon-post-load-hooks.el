;;; mon-post-load-hooks.el --- fncns to perform after initializing MON Emacsen
;; -*- mode: EMACS-LISP; -*-

;;; ================================================================
;; Copyright © 2010 MON KEY. All rights reserved.
;;; ================================================================

;; FILENAME: mon-post-load-hooks.el
;; AUTHOR: MON KEY
;; MAINTAINER: MON KEY
;; CREATED: 
;; VERSION: 1.0.0
;; COMPATIBILITY: Emacs23.*
;; KEYWORDS: environment, installation, site 

;;; ================================================================

;;; COMMENTARY: 

;; =================================================================
;; DESCRIPTION:
;; mon-post-load-hooks provides { some description here. }
;;
;; FUNCTIONS:►►►
;; `mon-run-post-load-hooks'
;; `mon-purge-cl-symbol-buffers-on-load'
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
;; `*mon-post-load-hook-trigger-buffer*'
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
;; FIRST-PUBLISHED:
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
;; Copyright © 2010 MON KEY 
;;; ==============================

;;; CODE:

(eval-when-compile (require 'cl))

;;; ==============================
;;; :TODO incorporate these:
;;; `mon-run-post-load-hooks', `mon-purge-cl-symbol-buffers-on-load'
;;; `mon-check-feature-for-loadtime', `mon-after-mon-utils-loadtime',
;;; `mon-set-register-tags-loadtime', `mon-bind-iptables-vars-at-loadtime',
;;; `mon-bind-cifs-vars-at-loadtime', `mon-CL-cln-colon-swap',
;;; `mon-bind-nefs-photos-at-loadtime', `mon-help-utils-loadtime',
;;; `mon-help-utils-CL-loadtime', `mon-bind-doc-help-proprietery-vars-at-loadtime'

;;; ==============================
;;; :CREATED <Timestamp: #{2010-03-23T22:24:16-04:00Z}#{10122} - by MON KEY>
(defun mon-purge-cl-symbol-buffers-on-load ()
  "Invoke post Emacs init. Remove buffers left by CL Hspec snarfage routines.\n
:SEE-ALSO `mon-run-post-load-hooks', `mon-purge-cl-symbol-buffers-on-load'
`*mon-post-load-hook-trigger-buffer*', `mon-check-feature-for-loadtime',
`mon-after-mon-utils-loadtime', `mon-set-register-tags-loadtime',
`mon-bind-iptables-vars-at-loadtime', `mon-bind-cifs-vars-at-loadtime',
`mon-CL-cln-colon-swap', `mon-bind-nefs-photos-at-loadtime',
`mon-help-utils-loadtime', `mon-help-utils-CL-loadtime',
`mon-bind-doc-help-proprietery-vars-at-loadtime'.\n►►►"
  (dolist (gb '("Map_Sym.txt"
                "Map_IssX.txt"
                "*CL-EXT-PKG-MAP*"))
    (when (get-buffer gb)
      (kill-buffer (get-buffer gb)))))

;;; ==============================
;;; :CREATED <Timestamp: #{2010-03-23T22:58:12-04:00Z}#{10122} - by MON KEY>
(defvar *mon-post-load-hook-trigger-buffer* nil
  "A buffer with a local kill-buffer-hook bound to 
`mon-run-post-load-hooks'. When this buffer is killed that run the hooks.
:SEE-ALSO `mon-run-post-load-hooks', `mon-purge-cl-symbol-buffers-on-load'
`mon-check-feature-for-loadtime', `mon-after-mon-utils-loadtime',
`mon-set-register-tags-loadtime', `mon-bind-iptables-vars-at-loadtime',
`mon-bind-cifs-vars-at-loadtime', `mon-CL-cln-colon-swap',
`mon-bind-nefs-photos-at-loadtime', `mon-help-utils-loadtime',
`mon-help-utils-CL-loadtime', `mon-bind-doc-help-proprietery-vars-at-loadtime'.\n►►►")
;;
(unless (bound-and-true-p *mon-post-load-hook-trigger-buffer*)
  (setq *mon-post-load-hook-trigger-buffer*
        (upcase (symbol-name '*mon-post-load-hook-trigger-buffer*))))
;;;(progn
;;; (makunbound '*mon-post-load-hook-trigger-buffer*)
;;; (unintern '*mon-post-load-hook-trigger-buffer*))

;;; ==============================
;;; :CREATED <Timestamp: #{2010-03-23T22:53:41-04:00Z}#{10122} - by MON KEY>
(defun mon-run-post-load-hooks ()
  "Kill the buffer named by the variable `*mon-post-load-hook-trigger-buffer*'.
Killing this buffer will run that buffers local `kill-buffer-hook'.\n
:SEE-ALSO `mon-run-post-load-hooks', `mon-purge-cl-symbol-buffers-on-load'
`mon-check-feature-for-loadtime', `mon-after-mon-utils-loadtime',
`mon-set-register-tags-loadtime', `mon-bind-iptables-vars-at-loadtime',
`mon-bind-cifs-vars-at-loadtime', `mon-CL-cln-colon-swap',
`mon-bind-nefs-photos-at-loadtime', `mon-help-utils-loadtime',
`mon-help-utils-CL-loadtime', `mon-bind-doc-help-proprietery-vars-at-loadtime'.\n►►►"
(when (get-buffer *mon-post-load-hook-trigger-buffer*)
  (kill-buffer (get-buffer *mon-post-load-hook-trigger-buffer*))))
;;
(with-current-buffer 
    (get-buffer-create *mon-post-load-hook-trigger-buffer*)
  (add-hook 'kill-buffer-hook 'mon-purge-cl-symbol-buffers-on-load nil t))
;;
(eval-after-load "mon-utils"
  '(progn
    (mon-after-mon-utils-loadtime)
    (mon-run-post-load-hooks)))

;;; ==============================
(provide 'mon-post-load-hooks)
;;; ==============================

;;; ====================================================================
;;; mon-post-load-hooks.el ends here
;;; EOF
