;;; mon-get-mon-packages.el --- snarf MON libraries with wget.
;; -*- mode: EMACS-LISP; -*-
;;; ================================================================
;; DESCRIPTION:
;; mon-get-mon-packages provides var and function to snarf MON libraries with
;; wget.
;;
;; AUTHOR: MON KEY
;; MAINTAINER: MON KEY
;;
;; FUNCTIONS:►►►
;; `mon-wget-mon-pkgs',
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
;; VARIABLES:
;; `*mon-el-library*',
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
;; URL: http://www.emacswiki.org/emacs/mon-get-mon-packages.el
;; FIRST-PUBLISHED:
;;
;; EMACSWIKI: {URL of an EmacsWiki describing mon-get-mon-packages.}
;; FILE-CREATED:
;; <Timestamp: #{2010-02-17T18:55:33-05:00Z}#{10073} - by MON KEY>
;;; ================================================================
;; Copyright © 2010 MON KEY. All rights reserved.
;;; ================================================================
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

;;; ================================================================

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
;;; ================================================================
;; Copyright © 2010 MON KEY 
;;; ==============================
;;; CODE:

(eval-when-compile (require 'cl))

(require 'mon-wget-utils)

;;; ==============================
;;; :NOTE Intent in keeping this list to ease bundling of working tarballs.
;;; :CREATED <Timestamp: #{2010-02-15T16:03:40-05:00Z}#{10071} - by MON KEY>
(defvar *mon-el-library*
  '(;; mon-GNU-load.el
    ;; slime-loads-GNU.el
    ;; slime-loads.el
    ;; mon-animate-nursery-rhymes.el
    ;; SuperDuperRedundantPlus+.el  

    ;; naf-mode-abbrevs.el
    ;; naf-mode-sql-skeletons.el
    ;; naf-mode-ulan-help-docs.el

    ;; "mon-syntax-utils.el"
    ;; "mon-testme-fancy.el"
    ;; "mon-bzr-utils.el"
    ;; "mon-ps-pdf-utils.el"
    ;; "mon-eight-bit-raw-utils.el"
    ;; "mon-rename-image-utils-supplemental.el"
    ;; "google-define-redux-supplemental.el"
    ;; "mon-doc-help-utils-supplemental.el"
    ;; ==============================
    ;;
    "monDOTemacs.el"
    ;; ../*mon-HG-root-path*/*mon-emacs-root*/
    "mon-w32-load.el"
    "mon-post-load-hooks.el"
    "mon-default-loads.el"
    "mon-default-start-loads.el"
    "mon-site-local-defaults.el"
    "mon-keybindings.el"
    ;;
    ;; ../*mon-HG-root-path*/*mon-emacs-root*/*mon-naf-mode-root*/
    "mon-testme-utils.el"
    "mon-alphabet-list-utils.el"    
    "mon-text-property-utils.el"
    "mon-utils.el"
    "mon-time-utils.el"    
    "mon-empty-registers.el"
    "mon-hash-utils.el"
    "mon-insertion-utils.el"
    "mon-aliases.el"
    "mon-regexp-symbols.el"
    "mon-rename-image-utils.el"
    "mon-replacement-utils.el"
    "mon-macs"
    "mon-type-utils-vars"
    "mon-type-utils"
    "mon-error-utils"
    "mon-plist-utils"
    "mon-seq-utils"
    "mon-buffer-utils"
    "mon-window-utils"
    "mon-randomize-utils"
    "mon-event-utils"
    "mon-line-utils"
    "mon-region-utils"
    "mon-string-utils"
    "mon-env-proc-utils"
    "mon-rectangle-utils"
    "mon-word-syntax-utils"
    ;;
    "google-define-redux.el"
    "mon-button-utils.el"
    "mon-url-utils.el"
    ;;
    "mon-tramp-utils.el"
    "mon-cifs-utils.el"
    ;;
    "mon-cl-compat-regexps.el"
    "mon-cl-compat.el"
    ;;
    "mon-color-utils.el"
    "mon-css-color.el"
    "mon-css-check.el"
    "mon-css-complete.el"
    ;;
    "mon-dir-locals-alist.el"
    "mon-dir-utils-local.el"
    "mon-dir-utils.el"
    ;;
    "mon-doc-help-CL.el"
    "mon-doc-help-mail.el"
    "mon-doc-help-pacman.el"
    "mon-doc-help-proprietary.el"
    "mon-doc-help-tidy.el"
    "mon-doc-help-utils.el"
    ;;
    "mon-iptables-regexps.el"
    "mon-iptables-vars.el"    
    ;;
    "mon-drive-transfer-utils.el"
    "mon-jg-directory-creator.el"    
    ;;
    "mon-get-freenode-lisp-logs.el"
    "mon-wget-utils.el"
    "mon-get-mon-packages.el"
    ;;
    "mon-boxcutter.el"
    "mon-mysql-utils.el"
    ;;
    ;; ../*mon-HG-root-path*/*mon-emacs-root*/*mon-naf-mode-root*/*mon-ebay-tmplt-mode-root*/
    "ebay-template-mode.el"
    "ebay-template-tools.el"
    ;;
    "perlisisms.el"
    "STING-software-engineering-glossary.el"
    ;; 
    "naf-mode.el"
    "naf-mode-faces.el"
    "naf-mode-insertion-utils.el"
    "naf-mode-replacements.el"
    "mon-name-utils.el"
    "naf-mode-classes.el"
    "naf-mode-db-fields.el"
    "naf-mode-db-flags.el"
    "naf-mode-dates.el"
    "naf-mode-french-months.el"
    "naf-mode-nation-english.el"
    "naf-mode-nation-french.el"
    "naf-mode-nationality-english.el"
    "naf-mode-nationality-french.el"
    "naf-mode-intnl-city-names.el"
    "naf-mode-city-names-us.el"
    "naf-mode-state-names.el"
    "naf-mode-regions.el"
    "naf-mode-publications-periodicals-english.el"
    "naf-mode-publications-periodicals-french.el"
    "naf-mode-publications-periodicals-intnl.el"
    "naf-mode-institution.el"
    "naf-mode-students-of-julian.el"
    "naf-mode-events.el"
    "naf-mode-english-roles.el"
    "naf-mode-french-roles.el"
    "naf-mode-awards-prizes.el"
    "naf-mode-group-period-styles.el"
    "naf-mode-art-keywords.el"
    "naf-mode-benezit-flags.el"
    "naf-mode-ulan-utils.el"
    "naf-mode-xrefs.el")
  "*A list of MON files published online EmacsWiki.\n
:SEE (URL `http://www.emacswiki.org/emacs/mon_key')
:SEE-ALSO `mon-wget-mon-pkgs', `mon-file-map-elisp-fileset'.\n►►►")
;;
;;;(progn (makunbound '*mon-el-library*) (unintern '*mon-el-library*) )

;;; ==============================
;;; :CREATED <Timestamp: #{2010-02-18T17:45:15-05:00Z}#{10074} - by MON KEY>
(defun mon-wget-mon-pkgs (&optional in-dir)
  "Invoke wget to pull the elisp files in `*mon-el-library*'.\n
When non-nil IN-DIR is a directory to wget MON's elisp libraries to.
If omitted files are are downloaded to `default-directory'.\n
:SEE-ALSO `mon-wget-list-give-script-to-shell-command'.\n►►►"
    (let ((mel (if (and in-dir (stringp in-dir))
                   in-dir 
                   (concat "mon-el-libs"
                           (format-time-string "-%y-%m-%d-%S")))))
      (if in-dir
          (unless (file-exists-p mel)
            (mkdir mel))
          (mkdir mel))
      (cd mel)
      (mon-wget-list-give-script-to-shell-command 
       *mon-el-library* 
       "http://www.emacswiki.org/emacs/download/" 
       (format-time-string "mon-wget-el-%y-%m-%d-%S")
       ;; EmacsWiki appears throttled. Lower wait times may cause 503 errors.
       "--wait=8")))
;;
;;; :TEST (mon-wget-mon-pkgs)

;;; :TODO Build lowtech tarball packages from list in variable *mon-el-library*.
;;; (directory-files (or some-dir *mon-naf-mode-root*) nil ".*.el$") 
;;; :SEE `mon-get-mon-pkgs2', `mon-get-mon-pkgs-w-wget' after EOF for alternative
;;; approach which attempted to incorporate that functionality inline. w32 fail!.

;;; ==============================
;;; ==============================
;;; ==============================
;;; :NOTE Following not trustworthy on w32. 
;;; Use `mon-wget-list-give-script-to-shell-command' instead which writes then
;;; reads back in a file. Better history, tests, logging.
;;;
;;; ;;; ==============================
;;; ;;; :CREATED <Timestamp: #{2010-02-15T16:03:43-05:00Z}#{10071} - by MON KEY>y
;;; (defun mon-get-mon-pkgs2 (&optional alt-dir w-pull insrtp) ;some-dir 
;;;   "Return a strings suitable for wget snarfage of MON's .el files.\n
;;; When ALT-DIR is non-nil it is an alternate source for concat'd return value.
;;; Default is \"http://www.emacswiki.org/emacs/download/\".\n
;;; When W-PULL is non-nil prepend return value with \"wget -nd -np \\\" and suffix
;;; each line of return value with \" \\\"\n
;;; When insrtp is non-nil return value is inserted at point. Does not move point.\n
;;; :EXAMPLE\n\n\(mon-get-mon-pkgs\)\n\n\(mon-get-mon-pkgs t\)\n
;;; \(mon-get-mon-pkgs default-directory)\n
;;; \(mon-get-mon-pkgs \"http://www.YEK-NOW.com/mon-key/\" t\)\n
;;; \(with-current-buffer 
;;;     \(get-buffer-create \(upcase \(symbol-name '*mon-el-library*\)\)\)
;;;   \(erase-buffer\) \(mon-get-mon-pkgs t t\)
;;;   \(set \(make-local-variable 'buffer-read-only\) t\)
;;;   \(display-buffer \(current-buffer\) t\)\)\n
;;; :SEE-ALSO `*mon-el-library*'.\n►►►"
;;;   (interactive "i\np")
;;;   (let ((mpme (mapconcat 
;;;                #'(lambda (el-file) 
;;;                    (concat 
;;;                     (or alt-dir "http://www.emacswiki.org/emacs/download/") el-file))
;;;                *mon-el-library*
;;;                (if (and w-pull 
;;;                         ;; Not fucking with the rest. "In Texas... yer on yer own."
;;;                         (or (eq system-type 'gnu) (eq system-type 'gnu/linux)))
;;;                    " \\\n" "\n"))))
;;;     (when w-pull (setq mpme (concat 
;;;                              "wget -nd -np "
;;;                              (if (or (eq system-type 'gnu)(eq system-type 'gnu/linux))
;;;                                  "\\\n" "\n" )
;;;                              mpme)))
;;;     (if insrtp
;;;         (save-excursion (princ mpme (current-buffer)))
;;;         mpme)))
;;; ;;
;;; ;;; :TEST-ME (mon-get-mon-pkgs)
;;; ;;; :TEST-ME (mon-get-mon-pkgs t)
;;; ;;; :TEST-ME (mon-get-mon-pkgs default-directory)
;;; ;;; :TEST-ME (mon-get-mon-pkgs default-directory t)
;;; ;;; :TEST-ME (mon-get-mon-pkgs "http://www.YEK-NOW.com/mon-key/" t)

;;; ;;; ==============================
;;; ;;; :CREATED <Timestamp: #{2010-02-17T17:10:18-05:00Z}#{10073} - by MON KEY>
;;; (defun mon-get-mon-pkgs-w-wget (&optional in-dir)
;;;   "
;;; Default is to invoke wget in default-directory.
;;; When IN-DIR is non-nil download files to that directory.
;;; It is a list of strings. First string is the directory to start from
;;; rest are subdirs to create. Strings should not contain trailing \"/\".
;;; :EXAMPLE\n
;;; \(mon-get-mon-pkgs-w-wget_  ;<- underscore prevents evaluation here :)
;;;  `\(,default-directory \"tt-wget" "subs\"\)
;;; :SEE-ALSO .\n►►►"
;;;  (let* ((aysn-buff (upcase (symbol-name '*mon-el-library*)))
;;;         (sys-g (eq system-type 'gnu))
;;;         (idp (when in-dir 
;;;                (concat "--directory-prefix=" 
;;;                        (mapconcat #'(lambda (id) 
;;;                                       (replace-regexp-in-string "/$" "" id))
;;;                                   in-dir "/" ))))
;;;          (mgmp (mon-get-mon-pkgs nil t)))
;;;     (when idp 
;;;       (setq idp (concat (substring mgmp 0 13) idp (if sys-g " \\\n" "\n")))
;;;       (setq mgmp (if sys-g (substring mgmp 15)(substring mgmp 14)))
;;;       (setq mgmp (concat idp mgmp)))
;;;     (with-current-buffer 
;;;         (get-buffer-create aysn-buff)
;;;       (erase-buffer))
;;;     (shell-command (concat mgmp))))
;;; ==============================

;;; ==============================
(provide 'mon-get-mon-packages)
;;; ==============================

 
;; Local Variables:
;; mode: EMACS-LISP
;; generated-autoload-file: "./mon-loaddefs.el"
;; coding: utf-8
;; End:

;;; ================================================================
;;; mon-get-mon-packages.el ends here
;;; EOF
