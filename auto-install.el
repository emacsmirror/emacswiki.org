;;; auto-install.el --- Auto install elisp file

;; Filename: auto-install.el
;; Description: Auto install elisp file
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;;         rubikitch <rubikitch@ruby-lang.org>
;; Maintainer: rubikitch <rubikitch@ruby-lang.org>
;; Copyright (C) 2008, 2009, Andy Stewart, all rights reserved.
;; Copyright (C) 2009, rubikitch, all rights reserved.
;; Created: 2008-12-11 13:56:50
;; Version: $Revision: 1.18 $
;; Last-Updated: Fri May 22 13:07:04 2009 (-0700)
;;           By: dradams
;; URL: http://www.emacswiki.org/emacs/download/auto-install.el
;; Keywords: auto-install
;; Compatibility: GNU Emacs 22 ~ 23
;;
;; Features that might be required by this library:
;;
;;   `backquote', `bytecomp', `dired', `find-func', `ietf-drums',
;;   `loadhist', `mail-parse', `mail-prsvr', `mailcap', `mm-util',
;;   `qp', `rfc2045', `rfc2047', `rfc2231', `thingatpt', `time-date',
;;   `timezone', `url', `url-cookie', `url-expand', `url-history',
;;   `url-methods', `url-parse', `url-privacy', `url-proxy',
;;   `url-util', `url-vars'.
;;

(defvar auto-install-version "$Id: auto-install.el,v 1.19 2009/05/22 13:04:56 dadams Exp $")
;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Automates the installation of Emacs Lisp files and packages.
;;
;; `auto-install' provides an automated way to:
;;
;; (1) Download Emacs Lisp files and packages from common sources
;; (2) View them (diff) and save them to your repository
;; (3) Compile and Load them
;;

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `auto-install-minor-mode'
;;    Auto Install minor mode.
;;  `auto-install-from-buffer'
;;    Install the elisp file in the current buffer.
;;  `auto-install-from-url'
;;    Install an elisp file from a given url.
;;  `auto-install-from-emacswiki'
;;    Install an elisp file from EmacsWiki.org.
;;  `auto-install-from-gist'
;;    Install an elisp file from gist.github.com.
;;  `auto-install-from-library'
;;    Update an elisp LIBRARY.
;;  `auto-install-from-directory'
;;    Update elisp files under DIRECTORY from EmacsWiki.
;;  `auto-install-from-dired'
;;    Update dired marked elisp files from EmacsWiki.org.
;;  `auto-install-update-emacswiki-package-name'
;;    Update the list of elisp package names from `EmacsWiki'.
;;  `auto-install-dired-mark-files'
;;    Mark dired files that contain at `EmacsWiki.org'.
;;  `auto-install-mode'
;;    Major mode for auto-installing elisp code.
;;  `auto-install-buffer-quit'
;;    Quit from `auto-install' temporary buffer.
;;  `auto-install-compatibility-setup'
;;    Install Compatibility commands for install-elisp.el users.
;;  `auto-install-batch'
;;    Batch install many libraries in some extension.
;;  `auto-install-buffer-diff'
;;    View different between old version.
;;  `auto-install-buffer-save'
;;    Save downloaded content to file FILENAME.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;
;;  `auto-install-directory'
;;    The directory for saving elisp files.
;;    default = "~/.emacs.d/auto-install/"
;;  `auto-install-buffer-name'
;;    The temporary buffer for storing download content.
;;    default = "auto-install"
;;  `auto-install-emacswiki-base-url'
;;    The base emacswiki.org url from which to download elisp files.
;;    default = "http://www.emacswiki.org/cgi-bin/wiki/download/"
;;  `auto-install-gist-base-url'
;;    The base gist.github.com url from which to download elisp files.
;;    default = "http://gist.github.com/"
;;  `auto-install-filter-url'
;;    Alist mapping filter url for library.
;;    default = (quote (("color-grep" "http://www.bookshelf.jp/elc/")))
;;  `auto-install-save-confirm'
;;    Whether confirmation is needed to save downloaded content.
;;    default = t
;;  `auto-install-replace-confirm'
;;    Whether confirmation is needed to replace an existing elisp file.
;;    default = nil
;;  `auto-install-install-confirm'
;;    Whether confirmation is needed to install a downloaded elisp file.
;;    default = nil
;;  `auto-install-from-dired-confirm'
;;    Whether confirmation is needed to download marked files from Dired.
;;    default = t
;;  `auto-install-batch-list'
;;    This list contain packages information for batch install.
;;    default = (quote (("icicles" 21 10 ...) ("auto-complete development version" nil nil ...) ("anything" nil nil ...) ("sdcv" nil nil ...) ("lazy-search" nil nil ...) ...))

;;; Tips:
;;
;;      Downloading is asynchronous: you can do your work and download
;;      files at the same time.  The download process won't hang
;;      Emacs.
;;
;;      `auto-install-from-url' remembers previous installations.  So if
;;      your search is the same as the previous search, you don't need
;;      to type it in, just hit RETURN.
;;
;;      `auto-install-from-emacswiki' will complete then names of
;;      packages from those in the Elisp area in `EmacsWiki'.
;;
;;      `auto-install-from-library' will prompt you library name in
;;      you load-path, then it try to download from EmacsWiki if it
;;      can't find match in `auto-install-filter-url'.
;;
;;      `auto-install-from-directory' can install elisp file
;;      under specify directory.
;;
;;      `auto-install-from-dired' can install marked files using dired.
;;      You can mark the files you want in dired and then use
;;      `auto-install-from-dired' to download those files
;;      asynchronously.
;;
;;      `auto-install-from-buffer' can save and install the contents of
;;      the current buffer as a file.  You need a valid elisp file name.
;;      The default name is the buffer name.
;;
;;      `auto-install-from-emacswiki' and `auto-install-from-library'
;;      will try to pick up file around point, you can move
;;      cursor to file name, and just hit RET for install.
;;
;;      Some extension (such as icicles) have many libraries to need install,
;;      and install one by one is painful, you can use command
;;      `auto-install-batch' install all icicles libraries.
;;      And `auto-install-batch' handle max connect limit with some website
;;      (such as EmacsWiki) to avoid download failed.
;;
;;      All of the above functions support a filename filter.  You can
;;      input any url to download an elisp file, if the file name suffix is
;;      `.el', it will download and install the file automatically.
;;      Otherwise, it won't install it unless you input a valid elisp
;;      file name.
;;
;;      By default, if a file that you download does not exist on your
;;      system the file is downloaded to `auto-install-directory'.  If
;;      you already have a file with the same name in your load
;;      directory, `auto-install' will try to replace that file.
;;
;;      You can use command `auto-install-dired-mark-files' to mark files
;;      that contain at `EmacsWiki.org' for fast update.
;;
;;      By default, command `auto-install-from-emacswiki' will initialization
;;      current symbol as default value, if default value is you want,
;;      just hit RET, so lazy!
;;

;;; Installation:
;;
;; (1) Put auto-install.el somewhere in your load-path.
;;
;;     For example, put it into  ~/elisp/.
;;     Then add the following to your ~/.emacs:
;;
;;       (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; (2) And put the following in your ~/.emacs startup file:
;;
;;       (require 'auto-install)
;;
;; (3) Add this to your ~/.emacs to optionally specify a download directory:
;;
;;       (setq auto-install-directory "~/.emacs.d/auto-install/")
;;
;;     If you don't set this, "~/.emacs.d/auto-install/" will be used as the default,
;;     and will be created as needed.
;;
;; (4) Optionally, if your computer is always connected Internet when Emacs start up,
;;     I recommend you add below to your ~/.emacs, to update package name when start up:
;;
;;       (auto-install-update-emacswiki-package-name t)
;;
;;     And above setup is not necessary, because AutoInstall will automatically update
;;     package name when you just first call `auto-install-from-emacswiki',
;;     above setup just avoid *delay* when you first call `auto-install-from-emacswiki'.
;;
;; (5) I recommend you add below to your ~/.emacs for install-elisp users:
;;
;;       (auto-install-compatibility-setup)
;;
;;     This command `defalias'es `install-elisp',
;;     `install-elisp-from-emacswiki' and `install-elisp-from-gist' to
;;     `auto-install' ones.
;;
;; (6) If you want to use proxy server, set `url-proxy-services'. For example:
;;
;;       (setq url-proxy-services '(("http" . "localhost:8339")))

;;; Customize:
;;
;; `auto-install-directory'
;; The default directory for keeping auto-downloaded elisp files.
;;
;; `auto-install-buffer-name'
;; The base buffer name for temporarily storing downloaded download content.
;;
;; `auto-install-emacswiki-base-url'
;; The base url for downloading from EmacsWiki.org.
;;
;; `auto-install-gist-base-url'
;; The base url for downloading from gist.github.com
;;
;; `auto-install-filter-url'
;; Filter url for downloading a special library.
;;
;; `auto-install-save-confirm'
;; Whether to require confirmation when saving downloaded content.
;;
;; `auto-install-replace-confirm'
;; Whether to require confirmation when replacing an already-installed
;; file.
;;
;; `auto-install-install-confirm'
;; Whether to require confirmation when installing a file.
;;
;; `auto-install-from-dired-confirm'
;; Whether to require confirmation when downloading files marked in dired.
;;
;; `auto-install-batch-list'
;; This list contain packages information for batch install.
;; Anyone can add packages information in this list for batch install.
;;
;; And all above option can customize easy through:
;;      M-x RET customize-group RET auto-install RET
;;

;;; Change log:
;;
;; $Log: auto-install.el,v $
;; Revision 1.19  2009/05/22 13:04:56  dadams
;; Split icicles-cmd.el into icicles-cmd[12].el.
;;
;; Revision 1.18  2009/05/20 15:42:54  rubikitch
;; Add php-completion / perl-completion to auto-install-batch-list
;;
;; Revision 1.17  2009/05/20 01:19:15  rubikitch
;; Add document for proxy server
;;
;; Revision 1.16  2009/05/15 20:28:18  rubikitch
;; More readable temporary buffer name.
;;
;; Revision 1.15  2009/05/15 20:12:49  rubikitch
;; Added missing require
;;
;; Revision 1.14  2009/05/15 20:11:44  rubikitch
;; How to save
;;
;; Revision 1.13  2009/05/15 20:09:07  rubikitch
;; Code cleanup
;;
;; Revision 1.12  2009/05/15 19:59:30  rubikitch
;; Fixed a bug of single file installation
;;
;; Revision 1.11  2009/05/15 19:44:32  rubikitch
;; Ordering `auto-install-batch'
;;
;; Revision 1.10  2009/05/15 17:48:09  rubikitch
;; Replace `message' with `error' for error messages.
;;
;; Revision 1.9  2009/05/15 17:40:37  rubikitch
;; refactoring
;;
;; Revision 1.8  2009/05/15 17:19:47  rubikitch
;; refactoring
;;
;; Revision 1.7  2009/05/15 17:17:03  rubikitch
;; Use `view-mode' if `view-read-only'.
;;
;; Revision 1.6  2009/05/15 17:10:22  rubikitch
;; Adjust docstrings of commands to auto-document.
;; Delete `It provides the following commands:' section because of duplication.
;;
;; Revision 1.5  2009/05/15 17:03:13  rubikitch
;; Show downloaded URL in header-line.
;;
;; Revision 1.4  2009/05/15 16:59:32  rubikitch
;; New internal variable: `auto-install-add-load-path-flag'
;;
;; Revision 1.3  2009/05/09 02:41:32  rubikitch
;; Add `auto-install-directory' automatically.
;;
;; Revision 1.2  2009/05/09 02:37:14  rubikitch
;; Changed `auto-install-get-buffer' format (including URL)
;;
;; Revision 1.1  2009/05/09 02:33:09  rubikitch
;; Initial revision
;;
;; 2009/05/01
;;  * Andy Stewart:
;;      * Take over by rubikitch.
;;
;; 2009/04/15
;;  * rubikitch:
;;      * Encoding detection support.
;;
;; 2009/04/07
;;  * Andy Stewart:
;;      * Fix bug of `auto-install-batch'.
;;      * Add more sources to `auto-install-batch-list'.
;;
;; 2009/03/30
;;  * Andy Stewart:
;;      * Add new command: `auto-install-batch'.
;;      * Add new option: `auto-install-batch-list'.
;;
;; 2009/03/29
;;  * Andy Stewart:
;;      * Add new function: `auto-install-from-url-list'.
;;
;; 2009/03/11
;;  * Andy Stewart:
;;      * Fix bug of `auto-install-download'.
;;
;; 2009/03/03
;;  * rubikitch
;;      * Add new command `auto-install-compatibility-setup'
;;        for install-elisp users.
;;  * Andy Stewart:
;;      * `auto-install-region-or-thing' return region string
;;        just when `transient-mark-mode' is on.
;;      * Fix doc.
;;
;; 2009/02/17
;;  * Andy Stewart:
;;      * Modified keybindings, make it more easy to remember.
;;      * Make `auto-install-save-confirm' default with `t'
;;        for security problem.
;;      * Pick up current symbol when use `auto-install-from-library'.
;;      * Remove unnecessary completion name from `auto-install-from-library'.
;;      * Refactory code.
;;      * Fix doc.
;;
;; 2009/02/12
;;  * Andy Stewart:
;;      * Remove option `auto-install-update-emacswiki-package-name-when-startup'.
;;      * Make current symbol as initialization of `auto-install-from-emacswiki'.
;;      * Add option `unforced' to function `auto-install-update-emacswiki-package-name'.
;;      * Fix doc.
;;      * Fix bug of `auto-install-from-library'.
;;
;; 2009/02/10
;;  * Andy Stewart:
;;      * Automatically download package name list when
;;        variable `auto-install-package-name-list' is nil.
;;      * Reverse `auto-install-package-name-list' for `anything' interface.
;;      * New command `auto-install-dired-mark-files',
;;        mark files that contain at `EmacsWiki.org'.
;;      * New command `auto-install-buffer-diff',
;;        view different between current version and old version.
;;
;; 2009/02/06
;;  * Andy Stewart:
;;      * Add new command `auto-install-from-directory'.
;;      * Remove option `auto-install-create-directory', not necessary.
;;      * Documentation improvements (thanks Scot Becker)
;;
;; 2009/02/01
;;  * Andy Stewart:
;;      * Make command `auto-install-from-emacswiki' can
;;        completing package name for input.
;;      * Add new command `auto-install-update-emacswiki-package-name'.
;;      * Add new option `auto-install-update-emacswiki-package-name-when-startup'
;;
;; 2009/01/30
;;  * Andy Stewart:
;;      * Compatibility with GNU Emacs 22.
;;
;; 2009/01/26
;;  * Andy Stewart:
;;      * Add new command `auto-install-from-gist'.
;;
;; 2009/01/21
;;  * Andy Stewart:
;;      * Add emacs-lisp syntax highlight for download buffer.
;;      * Make notify message display at mode-line instead echo-area.
;;
;; 2009/01/10
;;  * Andy Stewart:
;;      * Add new option `auto-install-filter-url' and new function
;;        `auto-install-from-library', try to use it. ;)
;;
;; 2009/01/08
;;  * Andy Stewart:
;;      * Fix coding bug.
;;
;; 2009/01/07
;;  * Andy Stewart:
;;      * Move `w3m' code to file `auto-install-extension.el' to make all
;;        user can use this package with standard emacs.
;;
;; 2009/01/06
;;  * Andy Stewart:
;;      * Clean code.
;;
;; 2009/01/02
;;  * Andy Stewart:
;;      * Add new option `auto-install-create-directory' for create install directory
;;        automatically if it doesn't exist.
;;      * Improve many document make it more clear.
;;      * Thanks document improve and create directory advice of 'Drew Adams'!
;;
;; 2008/12/24
;;  * Andy Stewart:
;;      * Remove `auto-install-window-configuration-before-download', `auto-install-init-window-layout'
;;        and `auto-install-revert-window-layout'.
;;        It's not necessary to revert window layout, `winner-mode' can revert window layout more better,
;;        just type `winner-undo'.
;;
;; 2008/12/15
;;  * Andy Stewart:
;;      * Fix a little bug of `auto-install-window-configuration-before-download'.
;;
;; 2008/12/11
;;  * Andy Stewart:
;;      * Add new function `auto-install-from-buffer', to install elisp file from current buffer.
;;        Modified `auto-install-buffer-save' to use `auto-install-from-buffer'.
;;
;;      * First released.
;;

;;; Acknowledgements:
;;
;;      rubikitch       <rubikitch@ruby-lang.org>
;;              For install-elisp.el
;;      Drew Adams      <drew.adams@oracle.com>
;;      Scot Becker     <scot.becker@gmail.com>
;;      Richard Riley   <rileyrgdev@gmail.com>
;;              For documentation improvements and advices.
;;

;;; TODO
;;
;;      Fix the problem parallel install process with recursive prompt.
;;      Redesign and give more friendly user interface.
;;      Scan RSS track package update and notify.
;;

;;; Require
(require 'url)
(require 'dired)
(require 'find-func)
(require 'bytecomp)
(require 'thingatpt)
(eval-when-compile (require 'cl))
(when (<= emacs-major-version 22)       ;Compatibility with 22.
  (autoload 'ignore-errors "cl-macs"))

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Customize ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup auto-install nil
  "Auto install elisp files."
  :group 'external)

(defcustom auto-install-directory "~/.emacs.d/auto-install/"
  "The directory for saving elisp files.
This directory is used when a downloaded
elisp file does not already exist in other directory.
Otherwise, the existing file of the same name is replaced."
  :type 'string
  :group 'auto-install)

(defcustom auto-install-buffer-name "auto-install"
  "The temporary buffer for storing download content."
  :type 'string
  :group 'auto-install)

(defcustom auto-install-emacswiki-base-url "http://www.emacswiki.org/cgi-bin/wiki/download/"
  "The base emacswiki.org url from which to download elisp files."
  :type 'string
  :group 'auto-install)

(defcustom auto-install-gist-base-url "http://gist.github.com/"
  "The base gist.github.com url from which to download elisp files."
  :type 'string
  :group 'auto-install)

(defcustom auto-install-filter-url
  '(("color-grep" "http://www.bookshelf.jp/elc/"))
  "Alist mapping filter url for library.
Default command `auto-install-from-library' will install from EmacsWiki,
if it can't find match in this alist."
  :type '(repeat (list (string :tag "Library")
                       (string :tag "Download URL")))
  :group 'auto-install)

(defcustom auto-install-save-confirm t
  "Whether confirmation is needed to save downloaded content.
Nil means no confirmation is needed.
If non-nil, the downloaded content is shown in a buffer and you are
prompted to confirm saving it to a file."
  :type 'boolean
  :group 'auto-install)

(defcustom auto-install-replace-confirm nil
  "Whether confirmation is needed to replace an existing elisp file.
Nil means no confirmation is needed."
  :type 'boolean
  :group 'auto-install)

(defcustom auto-install-install-confirm nil
  "Whether confirmation is needed to install a downloaded elisp file.
Nil means no confirmation is needed."
  :type 'boolean
  :group 'auto-install)

(defcustom auto-install-from-dired-confirm t
  "Whether confirmation is needed to download marked files from Dired.
Nil means no confirmation is needed."
  :type 'boolean
  :group 'auto-install)

(defcustom auto-install-batch-list
  '(
    ;; Icicles.
    ("icicles" 21 10
     (
      "http://www.emacswiki.org/emacs/download/icicles.el"      ; Main library
      "http://www.emacswiki.org/emacs/download/icicles-chg.el"  ; Change logs
      "http://www.emacswiki.org/emacs/download/icicles-cmd1.el" ; Top-level Icicles commands, part 1
      "http://www.emacswiki.org/emacs/download/icicles-cmd2.el" ; Top-level Icicles commands, part 2
      "http://www.emacswiki.org/emacs/download/icicles-doc1.el" ; Doc, part 1
      "http://www.emacswiki.org/emacs/download/icicles-doc2.el" ; Doc, part 2
      "http://www.emacswiki.org/emacs/download/icicles-face.el" ; Faces
      "http://www.emacswiki.org/emacs/download/icicles-fn.el"   ; Non-interactive functions
      "http://www.emacswiki.org/emacs/download/icicles-mac.el"  ; Macros
      "http://www.emacswiki.org/emacs/download/icicles-mcmd.el" ; Minibuffer commands
      "http://www.emacswiki.org/emacs/download/icicles-mode.el" ; Icicle (Icy) mode
      "http://www.emacswiki.org/emacs/download/icicles-opt.el"  ; User options
      "http://www.emacswiki.org/emacs/download/icicles-var.el"  ; Internal variables
      "http://www.emacswiki.org/emacs/download/lacarte.el"      ; Menu-bar access from keyboard
      "http://www.emacswiki.org/emacs/download/icomplete+.el"   ; Enhancements to `icomplete.el'
      "http://www.emacswiki.org/emacs/download/hexrgb.el"       ; Color manipulation
      "http://www.emacswiki.org/emacs/download/synonyms.el"     ; Look up synonyms
      ))
    ;; AutoComplete development version.
    ("auto-complete development version" nil nil
     (
      "http://www.cx4a.org/pub/auto-complete.el"            ; Main library
      "http://www.cx4a.org/pub/auto-complete-cpp.el"        ; Completion for C++
      "http://www.cx4a.org/pub/auto-complete-css.el"        ; Completion for CSS
      "http://www.cx4a.org/pub/auto-complete-emacs-lisp.el" ; Completion for elisp
      "http://www.cx4a.org/pub/auto-complete-gtags.el"      ; Completion for gtags
      "http://www.cx4a.org/pub/auto-complete-python.el"     ; Completion for python
      "http://www.cx4a.org/pub/auto-complete-ruby.el"       ; Completion for ruby
      "http://www.cx4a.org/pub/auto-complete-semantic.el"   ; Completion for semantic
      "http://www.cx4a.org/pub/auto-complete-yasnippet.el"  ; Completion for yasnippet
      ))
    ;; Anything
    ("anything" nil nil
     (
      "http://www.emacswiki.org/emacs/download/anything.el"        ; Main library
      "http://www.emacswiki.org/emacs/download/anything-config.el" ; Configuration for anything.el
      "http://www.emacswiki.org/emacs/download/anything-match-plugin.el" ; Matching algorithm humanely
      ))
    ;; SDCV (Interface for StartDict console version)
    ("sdcv" nil nil
     (
      "http://www.emacswiki.org/emacs/download/showtip.el" ; Basic tooltip show library
      "http://www.emacswiki.org/emacs/download/sdcv.el"    ; sdcv.el
      ))
    ;; Lazy search
    ("lazy-search" nil nil
     (
      "http://www.emacswiki.org/emacs/download/one-key.el"     ; Basic library for lazy-search.el
      "http://www.emacswiki.org/emacs/download/lazy-search.el" ; Main library
      ))
    ;; PHP completion
    ("php-completion" nil nil
     (
      "http://www.emacswiki.org/emacs/download/anything.el"
      "http://www.emacswiki.org/emacs/download/anything-match-plugin.el"
      "http://www.emacswiki.org/emacs/download/anything-show-completion.el"
      "http://www.emacswiki.org/emacs/download/php-completion.el"
      ))
    ;; Perl completion
    ("perl-completion" nil nil
     (
      "http://www.emacswiki.org/emacs/download/anything.el"
      "http://www.emacswiki.org/emacs/download/anything-match-plugin.el"
      "http://www.emacswiki.org/emacs/download/anything-show-completion.el"
      "http://www.emacswiki.org/emacs/download/perl-completion.el"
      ))
    )
  "This list contain packages information for batch install.

Have four arguments per list:
First argument is extension name.
Second argument is delay time for batch install.
Third argument is libraries number limit in delay time.
Fourth argument is libraries url list.

Anyone can add information in this list for batch install."
  :group 'auto-install)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Variable ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar auto-install-download-buffer nil
  "The download buffer used by `url-retrieve'.
This variable is always buffer-local.")
(make-variable-buffer-local 'auto-install-download-buffer)

(defvar auto-install-download-url nil
  "The url from which to download files.
This variable is always buffer-local.")
(make-variable-buffer-local 'auto-install-download-url)

(defvar auto-install-last-url nil
  "The last url used in `auto-install-from-url'.")

(defvar auto-install-last-gist-id nil
  "The last gist id you visit in `auto-install-from-gist'.")

(defvar auto-install-package-name-list nil
  "The package name list for completion input.")

(defvar auto-install-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-d") 'auto-install-buffer-diff) ;diff
    (define-key map (kbd "C-c C-c") 'auto-install-buffer-save) ;save
    (define-key map (kbd "C-c C-q") 'auto-install-buffer-quit) ;quit
    map)
  "Keymap used by variable `auto-install-minor-mode'.")

(defvar auto-install-add-load-path-flag t
  "If non-nil, add `auto-install-directory' to `load-path'.
This variable is intended to be used in test.")

(defvar auto-install-waiting-url-list nil
  "URLs in downloading.")
(defvar auto-install-url-queue nil
  "Installation order.")
(defvar auto-install-download-buffer-alist nil
  "Pairs of URL and downloaded buffer.")


(define-minor-mode auto-install-minor-mode
  "Auto Install minor mode."
  :init-value nil
  :lighter " Auto-Install"
  :keymap auto-install-minor-mode-map
  :group 'auto-install)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Interactive Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun auto-install-from-buffer ()
  "Install the elisp file in the current buffer."
  (interactive)
  (let (filename)
    (setq filename (read-string (format "Filename (%s): " (buffer-name)) nil nil (buffer-name)))
    (auto-install-mode)
    (auto-install-buffer-save filename)))

(defun auto-install-from-url (&optional url)
  "Install an elisp file from a given url."
  (interactive)
  (or url (setq url (read-string (format "URL (%s): " (or auto-install-last-url "")) nil nil auto-install-last-url)))
  (setq auto-install-last-url url)
  (auto-install-download url))

(defun auto-install-from-emacswiki (&optional file)
  "Install an elisp file from EmacsWiki.org."
  (interactive)
  (cond (auto-install-package-name-list
         ;; Install package if `auto-install-package-name-list' is non-nil.
         (or file (setq file (auto-install-get-candidate "Package" auto-install-package-name-list)))
         (auto-install-download (concat auto-install-emacswiki-base-url file)))
        (t
         ;; Otherwise update package name and install.
         (auto-install-download "http://www.emacswiki.org/cgi-bin/emacs?action=index;raw=1"
                                'auto-install-handle-emacswiki-package-install))))

(defun auto-install-from-gist (&optional gistid)
  "Install an elisp file from gist.github.com.
Optional argument GISTID is gist ID for download elisp file from gist.github.com."
  (interactive)
  (or gistid (setq gistid (read-string (format "Gist ID (%s): " (or auto-install-last-gist-id ""))
                                       nil nil
                                       auto-install-last-gist-id)))
  (setq auto-install-last-gist-id gistid)
  (auto-install-download (format "%s%s.txt" auto-install-gist-base-url gistid)))

(defun auto-install-from-library (&optional library)
  "Update an elisp LIBRARY.
Default this function will found 'download url' from `auto-install-filter-url',
if not found, try to download from EmacsWiki."
  (interactive
   (let* ((dirs load-path)
          (suffixes (find-library-suffixes)))
     (list (auto-install-get-candidate "Library name" (auto-install-get-library-list)))))
  (let ((filename (file-name-nondirectory (find-library-name library)))
        (base-url auto-install-emacswiki-base-url)
        (library-name (replace-regexp-in-string "\\(\\.el.*$\\)" "" library)))
    (if (assoc library-name auto-install-filter-url)
        (setq base-url (cadr (assoc library-name auto-install-filter-url))))
    (auto-install-download (concat base-url filename))))

(defun auto-install-from-directory (directory)
  "Update elisp files under DIRECTORY from EmacsWiki.
You can use this command to update elisp file under DIRECTORY."
  (interactive "DDirectory: ")
  (let (filename)
    (dolist (file (directory-files directory t))
      (if (file-directory-p file)
          ;; Don't match . or .. directory.
          (unless (string-match "^\\.\\.?$" (file-name-nondirectory file))
            ;; Find files in sub-directory.
            (auto-install-from-directory file))
        ;; Get file name.
        (setq filename (file-name-nondirectory file))
        ;; Not backup file.
        (unless (string-match "^\\.?#" filename)
          ;; Match elisp file.
          (if (string-match "^.*\\.el" filename)
              (auto-install-download (concat auto-install-emacswiki-base-url filename))))))))

(defun auto-install-from-dired ()
  "Update dired marked elisp files from EmacsWiki.org.
You can use this to download marked files in Dired asynchronously."
  (interactive)
  (if (eq major-mode 'dired-mode)
      (if (or (not auto-install-from-dired-confirm)
              (yes-or-no-p "Do you want install marked files from EmacsWiki.org?"))
          (dolist (file (dired-get-marked-files))
            (auto-install-download (concat auto-install-emacswiki-base-url (file-name-nondirectory file)))))
    (error "This command is only for `dired-mode'.")))

(defun auto-install-update-emacswiki-package-name (&optional unforced)
  "Update the list of elisp package names from `EmacsWiki'.
By default, this function will update package name forcibly.
If UNFORCED is non-nil, just update package name when `auto-install-package-name-list' is nil."
  (interactive)
  (unless (and unforced
               auto-install-package-name-list)
    (auto-install-download "http://www.emacswiki.org/cgi-bin/emacs?action=index;raw=1"
                           'auto-install-handle-emacswiki-package-name)))

(defun auto-install-dired-mark-files ()
  "Mark dired files that contain at `EmacsWiki.org'."
  (interactive)
  (if (eq major-mode 'dired-mode)
      (if auto-install-package-name-list
          ;; Mark files that exist at `EmacsWiki'.
          (auto-install-dired-mark-files-internal)
        ;; Or get package name list and match files.
        (auto-install-download "http://www.emacswiki.org/cgi-bin/emacs?action=index;raw=1"
                               'auto-install-handle-dired-mark-files))
    (error "This command just use in `dired-mode'.")))

(defun auto-install-mode ()
  "Major mode for auto-installing elisp code."
  (interactive)
  ;; Load emacs-lisp syntax highlight.
  (set-syntax-table emacs-lisp-mode-syntax-table)
  (lisp-mode-variables)
  (setq font-lock-mode t)
  (font-lock-fontify-buffer)
  ;; Read only.
  (setq buffer-read-only t)
  (and view-read-only (view-mode 1))
  ;; Load `auto-install' mode.
  (auto-install-minor-mode t)
  (setq major-mode 'auto-install-minor-mode))

(defun auto-install-buffer-quit ()
  "Quit from `auto-install' temporary buffer."
  (interactive)
  ;; Quit buffer.
  (if (eq major-mode 'auto-install-minor-mode)
      (auto-install-quit)
    (error "This command just use in `auto-install-minor-mode'.")))

(defun auto-install-compatibility-setup ()
  "Install Compatibility commands for install-elisp.el users."
  (interactive)
  (defalias 'install-elisp 'auto-install-from-url)
  (if (require 'anything-auto-install nil t)
      (defalias 'install-elisp-from-emacswiki 'anything-auto-install-from-emacswiki)
    (defalias 'install-elisp-from-emacswiki 'auto-install-from-emacswiki))
  (defalias 'install-elisp-from-gist 'auto-install-from-gist)
  (message "Install-elisp compatibility installed.
install-elisp                = %s
install-elisp-from-emacswiki = %s
install-elisp-from-gist      = %s"
           (symbol-function 'install-elisp)
           (symbol-function 'install-elisp-from-emacswiki)
           (symbol-function 'install-elisp-from-gist)))

(defun auto-install-batch (&optional extension-name)
  "Batch install many libraries in some extension.
EXTENSION-NAME is extension name for batch install."
  (interactive)
  (let (extension-info-list)
    ;; Get extension information list.
    (setq extension-info-list
          (assoc (or
                  ;; Get information list from give extension name.
                  extension-name
                  ;; Otherwise completion from user select.
                  (completing-read "Extension name: " (mapcar 'car auto-install-batch-list)))
                 auto-install-batch-list))
    (if extension-info-list
        ;; Install extension libraries.
        (let ((extension-delay-time (nth 1 extension-info-list))
              (extension-limit-number (nth 2 extension-info-list))
              (extension-library-list (car (last extension-info-list))))
          (setq auto-install-waiting-url-list extension-library-list
                auto-install-url-queue extension-library-list)
          (if (not (and
                    ;; Delay time is above 0.
                    extension-delay-time
                    (> extension-delay-time 0)
                    ;; Limit number is above 0.
                    extension-limit-number
                    (> extension-limit-number 0)))
              (auto-install-from-url-list extension-library-list)
            (let ((delay-counter 0)
                    install-list)
                (while extension-library-list
                  (if (> (length extension-library-list) extension-limit-number)
                      ;; Install apart libraries list under `extension-limit-number'
                      (progn
                        (setq install-list (nthcar extension-limit-number extension-library-list))
                        (run-with-timer
                         (* delay-counter extension-delay-time)
                         nil
                         'auto-install-from-url-list install-list)
                        (setq extension-library-list (nthcdr+ extension-limit-number extension-library-list))
                        (incf delay-counter))
                    ;; Install remain libraries list.
                    (setq install-list extension-library-list)
                    (run-with-timer
                     (* delay-counter extension-delay-time)
                     nil
                     'auto-install-from-url-list install-list)
                    (setq extension-library-list nil))))))
      ;; Notify message when haven't install information
      ;; for libraries that user given.
      (message "Haven't install information for `%s'." extension-name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Utilities Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun auto-install-download (url &optional handle-function)
  "Download elisp file from URL.
HANDLE-FUNCTION for handle download content,
default is `auto-install-handle-download-content'."
  ;; Check and create install directory.
  (unless (file-exists-p auto-install-directory)
    (make-directory auto-install-directory)
    (when auto-install-add-load-path-flag
      (add-to-list 'load-path auto-install-directory)) 
    (message "Create directory %s for install elisp file." auto-install-directory))
  ;; Download.
  (let* ((url-request-method "GET")
         (url-request-extra-headers nil)
         (url-mime-accept-string "*/*")
         (parsed-url (url-generic-parse-url url))
         (download-buffer (auto-install-get-buffer url))
         (download-buffer-name (buffer-name download-buffer)))
    (with-current-buffer download-buffer
      ;; Bind download url with local buffer.
      (setq auto-install-download-url url)
      ;; Bind download buffer with local buffer.
      ;;
      ;; Use buffer-local variable receive
      ;; data from `url-retrieve' to make asynchronously
      ;; download file with special buffer.
      ;;
      ;; Because the buffer name is unique that generate
      ;; through `current-time', so can download many elisp file
      ;; asynchronously and won't conflict each other.
      (setq auto-install-download-buffer
            (url-retrieve parsed-url
                          'auto-install-download-callback
                          (list download-buffer-name handle-function))))))

(defun auto-install-download-callback (&optional redirect download-buffer-name handle-function)
  "The callback for `auto-install-download'.
With `auto-install-download', this downloads elisp files asynchronously.
REDIRECT is the argument for check download status.
DOWNLOAD-BUFFER-NAME is the name of download buffer.
HANDLE-FUNCTION is function for handle download content."
  (if (eq (car redirect) ':error)
      ;; Notify user and kill buffer when occur error.
      (with-current-buffer (get-buffer download-buffer-name)
        (message "Download from '%s' failed." auto-install-download-url)
        (kill-buffer download-buffer-name))
    ;; Otherwise continue install process.
    (auto-install-retrieve-decode download-buffer-name) ;decode retrieve information.
    (with-current-buffer (get-buffer download-buffer-name)
      ;; Show successful message
      (message "Download from '%s' successful." auto-install-download-url)
      ;; Handle download content.
      (funcall (or handle-function 'auto-install-handle-download-content)
               (current-buffer)))))

(defun auto-install-retrieve-decode (retrieve-buffer-name)
  "Decode the RETRIEVE-BUFFER-NAME with coding detection."
  (declare (special url-http-end-of-headers))
  (with-current-buffer (get-buffer retrieve-buffer-name)
    (insert
     (with-current-buffer auto-install-download-buffer
       (set-buffer-multibyte t)
       (goto-char (1+ url-http-end-of-headers))
       (decode-coding-region
        (point) (point-max)
        (coding-system-change-eol-conversion
         ;; rubikitch: encoding detection is better because of
         ;; non-utf8 Japanese encodings.
         (detect-coding-region (point-min) (point-max) t) 'dos))
       (buffer-substring (point) (point-max))))
    (goto-char (point-min))))

(defun auto-install-handle-download-content (download-buffer)
  "Handle the content downloaded to buffer DOWNLOAD-BUFFER."
  (with-current-buffer download-buffer
    ;; Load mode.
    (auto-install-mode)
    ;; Display help information in mode-line.
    (setq mode-line-format (list "Type C-c C-c to continue; Type C-c C-d for view diff; Type C-c C-q to quit."))

    (setq header-line-format (list auto-install-download-url))
    (setq auto-install-download-buffer-alist
          (cons (cons auto-install-download-url download-buffer)
                auto-install-download-buffer-alist))
    (setq auto-install-waiting-url-list
          (remove auto-install-download-url auto-install-waiting-url-list))
    ;; When all files are downloaded
    (unless auto-install-waiting-url-list
      ;; Select first file
      (switch-to-buffer (or (assoc-default (car auto-install-url-queue)
                                           auto-install-download-buffer-alist)
                            ;; if single file
                            download-buffer))
      (unless auto-install-save-confirm
        (auto-install-buffer-save)))))

(defun auto-install-handle-emacswiki-package-name (download-buffer &optional prompt-install)
  "Handle elisp package name from `EmacsWiki'.
DOWNLOAD-BUFFER is the name of download buffer.
PROMPT-INSTALL is non-nil, will prompt package name for install."
  ;; Update package name list.
  (auto-install-update-emacswiki-package-list download-buffer)
  ;; Prompt package name for install.
  (when prompt-install
    (auto-install-download
     (concat auto-install-emacswiki-base-url
             (auto-install-get-candidate "Package" auto-install-package-name-list)))))

(defun auto-install-handle-dired-mark-files (download-buffer)
  "Handle dired mark files that exist at `EmacsWiki'.
DOWNLOAD-BUFFER is the name of download buffer."
  ;; Update package name list.
  (auto-install-update-emacswiki-package-list download-buffer)
  ;; Mark dired files.
  (auto-install-dired-mark-files-internal))

(defun auto-install-handle-emacswiki-package-install (download-buffer)
  "Handle elisp package install from `EmacsWiki'.
DOWNLOAD-BUFFER is the name of download buffer."
  (auto-install-handle-emacswiki-package-name download-buffer t))

(defun auto-install-update-emacswiki-package-list (download-buffer)
  "Filter and update package name list from `EmacsWiki'.
DOWNLOAD-BUFFER is the name of download buffer."
  (goto-char (point-min))
  (setq auto-install-package-name-list
        (loop while (re-search-forward "^.*\\.el$" nil t)
              collect (match-string 0)))
  ;; Kill buffer.
  (kill-buffer download-buffer)
  ;; Display successful message.
  (message "Update package name from `EmacsWiki' successful."))

(defun auto-install-buffer-diff ()
  "View different between old version.
This command just run when have exist old version."
  (interactive)
  (let* ((new-file (url-file-nondirectory auto-install-download-url))
         (old-file (auto-install-get-path new-file)))
    (if old-file
        ;; View different when have old version exist.
        (ediff-buffers (current-buffer) (find-file-noselect old-file))
      ;; Otherwise notify user.
      (message "Haven't old version exist."))))

(defun auto-install-buffer-save (&optional filename)
  "Save downloaded content to file FILENAME."
  (interactive)
  (if (eq major-mode 'auto-install-minor-mode)
      (let (file-path)
        ;; Get filename
        (unless filename
          (setq filename (url-file-nondirectory auto-install-download-url)))
        ;; Make sure file suffix with `.el'.
        (while (not (string-match ".*\.el$" filename))
          (setq filename (read-string "Please input file name suffix with `.el': ")))
        ;; Get file path.
        (setq file-path
              (or
               ;; Replace file if have exist.
               (auto-install-get-path filename)
               ;; Otherwise, install in directory `auto-install-directory'.
               (concat auto-install-directory filename)))
        ;; Save file.
        (if (and (file-exists-p file-path)
                 auto-install-replace-confirm
                 (not (yes-or-no-p (format "Do you want replace file: '%s' ?" file-path))))
            (auto-install-quit)
          (write-file file-path)
          (auto-install-install file-path)))
    (error "This command just use in `auto-install-minor-mode'.")))

(defun auto-install-install (file-path)
  "Install elisp file FILE-PATH."
  (if (and auto-install-install-confirm
           (not (yes-or-no-p (format "Do you want install file: '%s' ?" file-path))))
      (auto-install-quit)
    (let (byte-compile-warnings) ;; suppress compile warnings
      ;; Compile and load file.
      (setq auto-install-url-queue (cdr auto-install-url-queue))
      (unless (ignore-errors (byte-compile-file file-path t))
        ;; Show `ERROR' message if compile failed.
        (message (format "Auto-Install ERROR: Compiled file '%s' failed." file-path)))
      ;; Install next file.
      (cond ((car auto-install-url-queue)
             (switch-to-buffer (assoc-default (car auto-install-url-queue)
                                              auto-install-download-buffer-alist))
             (unless auto-install-save-confirm
               (auto-install-buffer-save)))
            (t                          ;completed
             ;; cleanup
             (setq auto-install-url-queue nil)
             (setq auto-install-download-buffer-alist nil)
             (message "Installation is completed."))))))

(defun auto-install-quit ()
  "Quit auto-install."
  ;; Kill buffer
  (kill-buffer (current-buffer))
  ;; Show quit message.
  (message "Quit auto-install process."))

(defun auto-install-get-path (library)
  "Return the absolute file path of the Lisp source of LIBRARY."
  ;; If the library is byte-compiled, try to find a source library by
  ;; the same name.
  (if (string-match "\\.el\\(c\\(\\..*\\)?\\)\\'" library)
      (setq library (replace-match "" t t library)))
  (or
   (locate-file library
                (or find-function-source-path load-path)
                (find-library-suffixes))
   (locate-file library
                (or find-function-source-path load-path)
                load-file-rep-suffixes)))

(defun auto-install-get-buffer (url)
  "Get a buffer for temporary storage of downloaded content.
Uses `current-time' to make buffer name unique."
  (get-buffer-create (format "*%s %s <%s>*"
                             auto-install-buffer-name url
                             (format-time-string "%m/%d %H:%M:%S"))))

(defun auto-install-dired-mark-files-internal ()
  "Mark files that match `auto-install-package-name-list'."
  ;; Set buffer visible in select window.
  (set-buffer (window-buffer))
  ;; Get mark files.
  (save-excursion
    (let (filename)
      ;; Unmark all markes.
      (dired-unmark-all-marks)
      ;; Try to mark files.
      (goto-char (point-min))
      (while (not (eobp))
        (setq filename (dired-get-filename nil t))
        (if (and filename
                 (not (file-directory-p filename))
                 (member (file-name-nondirectory filename) auto-install-package-name-list))
            (dired-mark 1))
        (dired-next-line 1)))))

(defun auto-install-region-or-thing (&optional thing)
  "Return region or thing around point.
If `mark-active' and variable `transient-mark-mode', return region.
If THING is non-nil, return THING around point;
otherwise return symbol around point."
  ;; Return string.
  (if (and mark-active
           transient-mark-mode)
      ;; Return region string just when
      ;; `mark-active' and `transient-mark-mode' is on.
      (buffer-substring-no-properties (region-beginning)
                                      (region-end))
    ;; Otherwise try to pick-up THING around point.
    (setq thing (or thing 'symbol))
    (ignore-errors
      (save-excursion
        (buffer-substring-no-properties (beginning-of-thing thing)
                                        (end-of-thing thing))))))

(defun auto-install-get-library-list (&optional dirs string)
  "Do completion for file names passed to `locate-file'.
DIRS is directory to search path.
STRING is string to match."
  ;; Use `load-path' as path when ignore `dirs'.
  (or dirs (setq dirs load-path))
  ;; Init with blank when ignore `string'.
  (or string (setq string ""))
  ;; Get library list.
  (let ((string-dir (file-name-directory string))
        name
        names)
    (dolist (dir dirs)
      (unless dir
        (setq dir default-directory))
      (if string-dir
          (setq dir (expand-file-name string-dir dir)))
      (when (file-directory-p dir)
        (dolist (file (file-name-all-completions
                       (file-name-nondirectory string) dir))
          ;; Suffixes match `load-file-rep-suffixes'.
          (setq name (if string-dir (concat string-dir file) file))
          (if (string-match (format "^.*\\.el%s$" (regexp-opt load-file-rep-suffixes)) name)
              (add-to-list 'names name)))))
    names))

(defun auto-install-get-candidate (prompt collection)
  "Get candidate from completing list.
PROMPT is string for prompt.
COLLECTION is list for completing candidates."
  (completing-read (format "%s (%s): " prompt (or (auto-install-region-or-thing) ""))
                   collection
                   nil nil nil nil
                   (auto-install-region-or-thing)))

(defun auto-install-from-url-list (&optional url-list)
  "Batch install many packages form URL-LIST."
  (if (listp url-list)
      (dolist (url url-list)
        (auto-install-from-url url))
    (error "Invalid url list for install.")))

(defun nthcdr+ (n list)
  "Take cdr N times on LIST, return the result.
If LIST length below N, return entire list.
If LIST is nil, return nil."
  (if (or (null list)
          (> n (length list)))
      list
    (nthcdr n list)))

(defun nthcar (n list)
  "Return first N elements of LIST.
If LIST length below N, return entire list.
If LIST is nil, return nil."
  (reverse (nthcdr (- (length list) n) (reverse list))))

(provide 'auto-install)

;; How to save (DO NOT REMOVE!!)
;; (emacswiki-post "auto-install.el")
;;; auto-install.el ends here

;;; LocalWords:  el eol dirs fontify gistid txt func bytecomp DDirectory ediff
;;; LocalWords:  noselect Unmark unmark AutoInstall keybindings defalias'es
