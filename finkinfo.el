;;; fink-info.el --- major mode for Fink package info files

;; Copyright (C) 2006 Dean Scarff

;; Author: Dean Scarff <dos_at_ scarff _dot_ id.au>
;; Maintainer: Dean Scarff <dos _at_ scarff _dot_ id.au>
;; Created: 10 Dec 2006
;; Version: 1.0
;; Keywords: convenience

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301 USA.

;;; Commentary:

;; See: <http://fink.sourceforge.net/doc/packaging/packaging.en.html>
;; for a description of the file format.

;; Based loosely around debian-control-mode V0.9.

;;; Code:

(require 'font-lock)

;; XEmacs compatibility
(eval-and-compile
  (unless (fboundp 'line-beginning-position)
    (defun line-beginning-position ()
      (save-excursion
	(beginning-of-line)
	(point))))
  (unless (fboundp 'line-end-position)
    (defun line-end-position ()
      (save-excursion
	(end-of-line)
	(point))))
  (unless (fboundp 'match-string-no-properties)
    (defalias 'match-string-no-properties 'match-string)))

(defgroup finkinfo nil "Fink package information"
  :group 'tools)

(defcustom finkinfo-fieldname-face 'font-lock-function-name-face
  "The face to use for highlighting field names."
  :type 'face
  :group 'finkinfo)

(defcustom finkinfo-percent-expansions-face 'font-lock-builtin-face
  "The face to use for highlighting percent expansions."
  :type 'face
  :group 'finkinfo)

(defcustom finkinfo-param-face 'font-lock-variable-name-face
  "The face to use for highlighting variable components of field
names."
  :type 'face
  :group 'finkinfo)

(defvar finkinfo-syntax-table nil
  "Syntax table used in finkinfo-mode buffers.")

(if finkinfo-syntax-table
    ()
  (setq finkinfo-syntax-table (make-syntax-table))
  (modify-syntax-entry ?#  "<"  finkinfo-syntax-table)
  (modify-syntax-entry ?\n "> " finkinfo-syntax-table))

(defvar finkinfo-parasep-regexp
  "^\\(?:\\(\\sw\\|-\\)+:\\s-*<<\\s-*\\)\\|<<$"
  "Regular expression matching lines that should not be filled as
  part of paragraphs.")

(defvar finkinfo-plain-fields
  '("Package" "Version" "Revision" "Architecture" "Epoch" "Description"
    "Type" "License" "Maintainer"

    "Depends" "BuildDepends" "Provides" "Conflicts" "BuildConflicts"
    "Replaces" "Recommends" "Suggests" "Enhances" "Pre-Depends"
    "Essential" "BuildDependsOnly"

    "CustomMirror" "Source" "SourceDirectory" "NoSourceDirectory"
    "Source" "SourceRename" "Source-MD5" "TarFilesRename"

    "UpdateConfigGuess" "UpdateConfigGuessInDirs" "UpdateLibtool"
    "UpdateLibtoolInDirs" "UpdatePoMakefile" "Patch" "PatchFile"
    "PatchFile-MD5" "PatchScript"

    "ConfigureParams" "GCC" "CompileScript" "NoPerlTests"

    "InfoTest"

    "UpdatePOD" "InstallScript" "AppBundles" "JarFiles" "DocFiles"
    "Shlibs" "RuntimeVars" "SplitOff" "Files"

    "PreInstScript" "PostInstScript" "PreRmScript" "PostRmScript"
    "ConfFiles" "InfoDocs" "DaemonicFile" "DaemonicName"

    "Homepage" "DescDetail" "DescUsage" "DescPackaging" "DescPort")
  "Fields used as-is.")

(defvar finkinfo-plain-fields-regexp
  (concat
   "^\\("
   (let ((max-specpdl-size 1000))
     (regexp-opt finkinfo-plain-fields t))
   "\\):")
  "font-lock regexp matching known fields in the source section.")

(defvar finkinfo-suffix-numbered-fields-regexp
  "^\\(Info\\|S\\(?:plitOff\\|ource\\)\\)\\([[:digit:]]+\\):"
  "font-lock regexp matching enumerated fields.")

(defvar finkinfo-source-numbered-fields-regexp
  "^\\(Source\\)\\([[:digit:]]+\\)\\(-MD5\\|ExtractDir\\|Rename\\):"
  "font-lock regexp matching SourceNFoo enumerated fields.")

(defvar finkinfo-tar-numbered-fields-regexp
  "^\\(Tar\\)\\([[:digit:]]+\\)+\\(FilesRename\\):"
  "font-lock regexp matching TarNFilesRename enumerated fields.")

(defvar finkinfo-env-fields-regexp
  "^\\(\\(?:No\\)?Set\\)\\(\w+\\):"
  "font-lock regexp matching environment-variable control fields.")

(defvar finkinfo-percent-expansions-regexp
  (concat
   "\\(%\\(?:"
   "[nNevrfpPdDiIabcm]\\|"
   "type_\\(?:raw\\|pkg\\|num\\)\\[\\w+\\]\\|"
   "{[nN]i\\|default_script\\|PatchFile}\\|"
   "lib"
   "\\)\\)")
  "font-lock regexp matching percent-expansions.")

(defmacro finkinfo-flkw (subexp fl-face)
  (list 'list subexp
	(if (featurep 'xemacs)
	    (list 'quote (list 'symbol-value 'fl-face))
	  (list 'quote (list 'list (quote 'face) fl-face)))
	nil nil))

(defvar finkinfo-font-lock-keywords
  (list (list finkinfo-plain-fields-regexp
	      (finkinfo-flkw 1 finkinfo-fieldname-face))
	(list finkinfo-suffix-numbered-fields-regexp
	      (finkinfo-flkw 2 finkinfo-param-face)
	      (finkinfo-flkw 1 finkinfo-fieldname-face))
	(list finkinfo-source-numbered-fields-regexp
	      (finkinfo-flkw 2 finkinfo-param-face)
	      (finkinfo-flkw 1 finkinfo-fieldname-face)
	      (finkinfo-flkw 3 finkinfo-fieldname-face))
	(list finkinfo-tar-numbered-fields-regexp
	      (finkinfo-flkw 2 finkinfo-param-face)
	      (finkinfo-flkw 1 finkinfo-fieldname-face)
	      (finkinfo-flkw 3 finkinfo-fieldname-face))
	(list finkinfo-env-fields-regexp
	      (finkinfo-flkw 1 finkinfo-fieldname-face)
	      (finkinfo-flkw 2 finkinfo-param-face))
	(list finkinfo-percent-expansions-regexp
	      (finkinfo-flkw 1 finkinfo-percent-expansions-face))))

(defvar finkinfo-mode-menu nil)

;;;###autoload
(define-derived-mode finkinfo-mode fundamental-mode "Finkinfo"
  "A major mode for editing Fink package info files."
  (if (< emacs-major-version 21)
      (message "finkinfo-mode only supports emacsen version >= 21; disabling features")
    (progn
      (set-syntax-table finkinfo-syntax-table)
      (make-local-variable 'comment-start)
      (make-local-variable 'comment-end)
      (make-local-variable 'comment-start-skip)
      (make-local-variable 'font-lock-defaults)
      (setq
       comment-start "#"
       comment-end ""
       comment-start-skip "\\(^\\|\\s-\\);?#+ *"

       font-lock-defaults
       '(finkinfo-font-lock-keywords nil nil nil nil))
      (set (make-local-variable 'fill-paragraph-function)
	   #'finkinfo-mode-fill-paragraph)
      (if (and (featurep 'goto-addr) goto-address-highlight-p)
        (goto-address)))))

(defun finkinfo-mode-fill-paragraph (&rest args)
  (let (beg end)
    (save-excursion
      (if (save-excursion
	    (beginning-of-line)
	    (looking-at finkinfo-parasep-regexp))
	  (setq beg (match-end 0)
		end (line-end-position))
	;; Find the beginning and end of the paragraph
	(setq beg (save-excursion
		    (beginning-of-line)
		    (while (not (or (bobp)
				    (looking-at finkinfo-parasep-regexp)))
		      (forward-line -1))
		    (unless (eobp) (forward-line 1))
		    (point))
	      end (save-excursion
		    (beginning-of-line)
		    (while (not (or (eobp)
				    (looking-at finkinfo-parasep-regexp)))
		      (forward-line 1))
		    (unless (bobp) (forward-line -1) (end-of-line))
		    (point))))
      (apply #'fill-region beg end args))))

(add-to-list 'auto-mode-alist '("/finkinfo/[^.]+.info\\'" . finkinfo-mode))
;;;###autoload(add-to-list 'auto-mode-alist '("/finkinfo/[^.]+.info\\'" . finkinfo-mode))

(provide 'finkinfo)

;;; finkinfo.el ends here
