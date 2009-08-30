;;; icicles-install.el -- Download and optionally byte-compile Icicles 
;;
;; Filename: icicles-install.el
;; Description: Download and optionally byte-compile the Icicles package
;; Author: Anupam Sengupta
;; Maintainer: Anupam Sengupta
;; Copyright (C) 2007-2008 Anupam Sengupta, all rights reserved.
;; Created: Wed May 24 14:05:13 2007
;; Version: 1.0
;; Last-Updated: Fri May 22 12:57:40 2009 (-0700)
;;           By: dradams
;; URL: http://www.emacswiki.org/cgi-bin/wiki/icicles-install.el
;; Keywords: package, download
;; Compatibility: GNU Emacs 20.x, GNU Emacs 21.x, GNU Emacs 22.x
;;
;; Features that might be required by this library:
;;
;;   `url'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  Download the Icicles package (libraries) from
;;  http://www.emacswiki.org and optionally byte-compile any files in
;;  the target directory that were previously byte-compiled.
;;
;;  Usage:
;;
;;  1. Download this file to a convenient directory (e.g. ~/elisp)
;;  2. Add the following line in your .emacs file:
;;
;;      (load "~/<dirname>/icicles-install")
;;
;;     Adjust the directory name for your local download directory.
;;
;;  3. You need not restart Emacs. At end of the sexp, enter C-x C-e
;;     to load the file
;;
;;  4. Optional Step: You may want to customize the download directory
;;     (it defaults to "~/icicles") by running `customize-variable' on
;;     the `icicle-download-dir' variable. If you do this, be sure to
;;     also add the value of `icicle-download-dir' to variable
;;     `load-path'.
;;
;;  5. Run the command `icicle-download-wizard' from the mini-buffer:
;;
;;      M-x icicle-download-wizard
;;
;;  Acknowledgements:
;;
;;  Code based largely on `package.el' by Tom Tromey.
;;  See: http://tromey.com/elpa/package-install.el
;;  See: http://www.emacswiki.org/cgi-bin/wiki/TomTromey
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;; 2009/05/22 dadams
;;     Split icicles-cmd.el into icicles-cmd1.el and icicles-cmd2.el.
;; 2008/08/16 dadams
;;     icicle-download-wizard: Bind load-path to include icicle-download-dir.
;;       Thx to Kevin Rodgers.
;; 2008/03/06 dap (Damon Permezel)
;;     Mention setting url-http-version for ^M work around.
;; 2007/08/16 dadams
;;     Mention updating load-path in step 4. Thx to Lars Bjornfot.
;; 2007/06/09 dadams
;;     Corrected Tom Tromey's name and URL.
;; 2007/06/07 dadams (Drew Adams)
;;     Added icicles-doc1.el and icicles-doc2.el.
;;     Clarified that only files that have been byte-compiled are byte-compiled.
;; 2007/06/02 AnupamSengupta
;;     Created.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software software; you can redistribute it
;; and/or modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(eval-when-compile (require 'cl)) ;; dolist

(require 'url nil t) ;; (no error if not found): If not present, uses `wget' instead

;; Override HTTP version to avoid url bug handling Transfer-Encoding: chunked
;; which results in a spurious ^M at the end of each file.
;; Leave this commented out for now, mainly for documentation of this problem
;; while some more investigation is performed ... by someone as yet unknown.
;; - Damon Permezel
;;
;; (if (fboundp 'url-retrieve-synchronously) (set-variable 'url-http-version "1.0"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defcustom icicle-archive-base "http://www.emacswiki.org/cgi-bin/wiki/download/"
  "*Base URL from which the Icicles files should be downloaded.
Default value is the Emacs Wiki site."
  :type 'string :group 'Icicles-Miscellaneous)

;;;###autoload
(defcustom icicle-download-dir "~/icicles"
  "*Directory to which the Icicles files should be downloaded."
  :type 'directory :group 'Icicles-Miscellaneous)

;;;###autoload
(defcustom icicle-files-to-download-list
  (list
   "icicles.el"                 ; Main library - just loads the others
   "icicles-chg.el"             ; Change logs
   "icicles-doc1.el"            ; Doc, part 1
   "icicles-doc2.el"            ; Doc, part 2
   "icicles-cmd1.el"            ; Top-level Icicles commands, part 1
   "icicles-cmd2.el"            ; Top-level Icicles commands, part 2
   "icicles-face.el"            ; Faces
   "icicles-fn.el"              ; Non-interactive functions
   "icicles-mac.el"             ; Macros
   "icicles-mcmd.el"            ; Minibuffer commands
   "icicles-mode.el"            ; Icicle (Icy) mode
   "icicles-opt.el"             ; User options
   "icicles-var.el"             ; Internal variables
   "lacarte.el"                 ; Menu-bar access from keyboard
   "icomplete+.el"              ; Enhancements to `icomplete.el'
   "hexrgb.el"                  ; Color manipulation
   "synonyms.el")               ; Synonym lookup
  "*Icicles files to download.
Icicles is made to take advantage of some libraries that are not
strictly required.  All of these are recommended, but only some are
included in the default value of `icicle-files-to-download-list'.

The following files are included in the download list by default.  If
you like, you can remove any of them without impacting core Icicles
functionality:

  `icicles-chg.el' - Change logs for all Icicles libraries
  `icicles-doc1.el' and `icicles-doc2.el' - Icicles documentation
  `lacarte.el' (aka `icicles-menu.el')- Menu-bar access from keyboard
  `icomplete+.el' - Enhancements to `icomplete.el'
  `hexrgb.el' - Lets you use and manipulate colors with Icicles
  `synonyms.el' - Look up synonyms in a thesaurus using Icicles

The following files are also recommended but not strictly required.
You might want to add them to `icicle-files-to-download-list'.

  `apropos-fn+var.el' - Enhanced apropos commands
  `dired+.el' - Use file on current line as default in Dired
  `doremi.el' - Change *Completions* display incrementally
  `ffap-.el' - Extensions to `ffap.el'
  `fit-frame.el' - Fit frames to their (sole) buffers
  `fuzzy-match.el' - Provides Icicles with fuzzy matching
  `linkd' - Provides hypertext links for Icicles doc
  `menu-bar+.el' - Menu-bar menus (Apropos, Describe, Frames, Tags)
  `misc-cmds.el' - Clear search history.  Kill buffer deletes window.
  `palette.el' - Pick up foreground/background color
  `pp+.el' - Enhanced version of `pp-eval-expression', for `M-:'
  `thingatpt+.el' - Use names near, not just at, point as defaults
  `wid-edit+.el' - Color widget for Customize"
  :type '(repeat file) :group 'Icicles-Miscellaneous)

;;;###autoload
(defun icicle-download-file (url)
  "Download a file from the specified URL and return the download buffer.
This uses package `url' if available or `wget' otherwise."
  (if (fboundp 'url-retrieve-synchronously)
      (let ((buffer (url-retrieve-synchronously url))) ; Use URL to download
        (save-excursion
          (set-buffer buffer)
          (goto-char (point-min))
          (re-search-forward "^$" nil 'move)
          (forward-char)
          (delete-region (point-min) (point))
          buffer))
    (with-current-buffer                ; Else use `wget' to download
        (get-buffer-create (generate-new-buffer-name " *Download*"))
      (shell-command (concat "wget -q -O- " url) (current-buffer))
      (goto-char (point-min))
      (current-buffer))))

;;;###autoload
(defun icicle-download-and-save-file (file-to-download)
  "Download and save FILE-TO-DOWNLOAD."
  (let ((pkg-buffer (icicle-download-file (concat icicle-archive-base
                                                  file-to-download))))
    ;; Save the downloaded buffer contents in the file
    (save-excursion
      (set-buffer pkg-buffer)
      (setq buffer-file-name (concat (file-name-as-directory icicle-download-dir)
                                     file-to-download))
      (save-buffer)
      (kill-buffer pkg-buffer)
      (message "Downloaded `%s'" file-to-download))))

;;;###autoload
(defun icicle-download-all-files ()
  "Download and save all Icicles files.
Create the download directory if it does not exist."
  (dolist (file-to-download icicle-files-to-download-list t)
    (icicle-download-and-save-file file-to-download)
    (sleep-for 2)))            ; Sleep to prevent overloading the site

;;;###autoload
(defun icicle-byte-compile-downloaded-files ()
  "Byte-compile all previously compiled files in `icicle-download-dir'."
  (byte-recompile-directory icicle-download-dir 0))

;;;###autoload
(defun icicle-download-wizard ()
  "Run the interactive wizard for downloading Icicles libraries."
  (interactive)
  (if (not (y-or-n-p "Download the Icicle files? "))
      (message "Icicles download cancelled")
    (make-directory icicle-download-dir t) ; Create directory if not present.
    (icicle-download-all-files)
    (message "Icicles download completed")
    (if (y-or-n-p "Byte-compile files in download directory now? ")
        (let ((load-path (cons icicle-download-dir load-path)))
          (icicle-byte-compile-downloaded-files))
      (message "Byte-compiled the Icicles files"))
    (when (y-or-n-p "Show the Icicle files in Dired? ")
      (dired-other-frame icicle-download-dir))
    (message "Icicles download complete")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'icicles-install)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Icicle-install ends here.
