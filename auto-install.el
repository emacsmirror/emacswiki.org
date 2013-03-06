;;; auto-install.el --- Auto install elisp file
;; $Id: auto-install.el,v 1.54 2012/01/15 12:10:20 rubikitch Exp rubikitch $

;; Filename: auto-install.el
;; Description: Auto install elisp file
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;;         rubikitch <rubikitch@ruby-lang.org>
;; Maintainer: rubikitch <rubikitch@ruby-lang.org>
;; Copyright (C) 2008, 2009, Andy Stewart, all rights reserved.
;; Copyright (C) 2009, rubikitch, all rights reserved.
;; Created: 2008-12-11 13:56:50
;; Version: $Revision: 1.54 $
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

(defvar auto-install-version "$Id: auto-install.el,v 1.54 2012/01/15 12:10:20 rubikitch Exp rubikitch $")
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
;;    Batch install many files (libraries and non-elisp files) in some extension.
;;  `auto-install-batch-edit'
;;    Edit auto-install-batch-list.el
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
;;  `auto-install-wget-command'
;;    *Wget command. Use only if `auto-install-use-wget' is non-nil.
;;    default = "wget"
;;  `auto-install-use-wget'
;;    *Use wget instead of `url-retrieve'.
;;    default = (executable-find "wget")
;;  `auto-install-batch-list'
;;    This list contain packages information for batch install.
;;    default = nil

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


;;; Bug Report:
;;
;; If you have problem, send a bug report via M-x auto-install-send-bug-report.
;; The step is:
;;  0) Setup mail in Emacs, the easiest way is:
;;       (setq user-mail-address "your@mail.address")
;;       (setq user-full-name "Your Full Name")
;;       (setq smtpmail-smtp-server "your.smtp.server.jp")
;;       (setq mail-user-agent 'message-user-agent)
;;       (setq message-send-mail-function 'message-smtpmail-send-it)
;;  1) Be sure to use the LATEST version of auto-install.el.
;;  2) Enable debugger. M-x toggle-debug-on-error or (setq debug-on-error t)
;;  3) Use Lisp version instead of compiled one: (load "auto-install.el")
;;  4) Do it!
;;  5) If you got an error, please do not close *Backtrace* buffer.
;;  6) M-x auto-install-send-bug-report and M-x insert-buffer *Backtrace*
;;  7) Describe the bug using a precise recipe.
;;  8) Type C-c C-c to send.
;;  # If you are a Japanese, please write in Japanese:-)

;;; Change log:
;;
;; $Log: auto-install.el,v $
;; Revision 1.54  2012/01/15 12:10:20  rubikitch
;; Default value of `auto-install-use-wget' is whether wget exists or not.
;;
;; Revision 1.53  2011/04/12 06:28:20  rubikitch
;; fix for proxy
;;
;; Revision 1.52  2011/01/29 11:11:47  rubikitch
;; bugfix: auto-install-buffer-save cannot treat auto-install-directory properly if it doesn't end with `/'
;;
;; patched by MaskRay thanks!
;;
;; Revision 1.51  2010/12/10 10:30:58  rubikitch
;; Bugfix when wget is not installed
;;
;; replace auto-install-use-wget with (auto-install-use-wget-p)
;;
;; Revision 1.50  2010/11/29 15:52:57  rubikitch
;; compatibility code for emacs21.1
;;
;; Revision 1.49  2010/11/10 13:32:37  rubikitch
;; Use `wget -q -O- --no-check-certificate' if wget is available.
;; Change default value: `auto-install-use-wget' = t
;;
;; Revision 1.48  2010/05/20 23:29:10  rubikitch
;; `auto-install-update-emacswiki-package-name': Check whether network is reachable
;;
;; Revision 1.47  2010/05/14 00:09:08  rubikitch
;; Fixed a bug of `auto-install-batch' with argument.
;;
;; Pass extension-name argument to `auto-install-batch-real'.
;;
;; Revision 1.46  2010/05/04 08:46:21  rubikitch
;; Added bug report command
;;
;; Revision 1.45  2010/05/01 01:24:25  rubikitch
;; auto-install-batch: Dependency support
;;
;; Revision 1.44  2010/05/01 00:53:48  rubikitch
;; auto-install-batch-real: refactoring
;;
;; Revision 1.43  2010/04/30 23:41:48  rubikitch
;; Changed default value of `auto-install-use-wget' to nil.
;;
;; Revision 1.42  2010/04/25 02:30:27  rubikitch
;; Avoid `auto-async-byte-compile'
;;
;; Revision 1.41  2010/04/25 01:59:20  rubikitch
;; *** empty log message ***
;;
;; Revision 1.40  2010/04/25 01:58:17  rubikitch
;; Do not save/compile up-to-date files from `auto-install-batch'.
;;
;; Revision 1.39  2010/04/25 01:11:33  rubikitch
;; If downloaded file is not updated, kill download buffer.
;; (auto-install-batch support is not yet)
;;
;; Revision 1.38  2010/04/25 00:10:49  rubikitch
;; code cleanup: remove unneeded `format'
;;
;; Revision 1.37  2010/04/24 23:59:11  rubikitch
;; comment change (no code change)
;;
;; Revision 1.36  2010/04/19 07:10:36  rubikitch
;; Avoid updating time-stamp
;;
;; Revision 1.35  2010/04/07 20:16:10  rubikitch
;; Fixed a typo
;;
;; Revision 1.34  2010/04/05 21:34:07  rubikitch
;; `auto-install-from-gist' can accept gist URL.
;;
;; Revision 1.33  2010/04/05 21:27:30  rubikitch
;; `auto-install-use-wget' is enabled by default when wget command (`auto-install-wget-command') is found.
;;
;; Revision 1.32  2010/04/05 21:24:30  rubikitch
;; * `auto-install-batch' can install non-elisp files because some elisp requires external scripts.
;; * New option: `auto-install-add-exec-path-flag'
;;
;; Revision 1.31  2010/04/01 03:43:17  rubikitch
;; added RCS Id: tag
;;
;; Revision 1.30  2010/04/01 03:42:34  rubikitch
;; document typo
;;
;; Revision 1.29  2010/04/01 03:28:39  rubikitch
;; New command: `auto-install-batch-edit'
;;
;; Revision 1.28  2010/04/01 03:10:38  rubikitch
;; The real value of `auto-install-batch-list' is moved to
;; auto-install-batch-list.el to split program and data.
;;
;; Revision 1.27  2010/03/29 07:36:39  rubikitch
;; Stupid bug fix in auto-install-use-wget
;;
;; Revision 1.26  2010/03/29 02:38:46  rubikitch
;; New option: `auto-install-use-wget', `auto-install-wget-command'
;;
;; Revision 1.25  2010/03/26 00:07:27  rubikitch
;; `url-http-end-of-headers' workaround
;;
;; Revision 1.24  2010/01/05 09:40:04  rubikitch
;; fixed error of auto-complete development version in `auto-install-batch-list'
;;
;; Revision 1.23  2009/12/29 09:31:23  rubikitch
;; add Text Translator to auto-install-batch-list
;;
;; Revision 1.22  2009/12/21 12:51:56  rubikitch
;; Update auto-install-batch anything
;;
;; Revision 1.21  2009/12/21 12:26:54  rubikitch
;; New URL for auto-complete development version
;;
;; Revision 1.20  2009/05/22 20:17:24  rubikitch
;; Merged from dradams' change
;;
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
(require 'ffap)
(eval-when-compile (require 'cl))
(when (<= emacs-major-version 22)       ;Compatibility with 22.
  (autoload 'ignore-errors "cl-macs")
  (unless (fboundp 'url-file-nondirectory)
    (defun url-file-nondirectory (file)
      "Return the nondirectory part of FILE, for a URL."
      (cond
       ((null file) "")
       ((string-match (eval-when-compile (regexp-quote "?")) file)
        (file-name-nondirectory (substring file 0 (match-beginning 0))))
       (t (file-name-nondirectory file))))))

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

(defcustom auto-install-wget-command "wget"
  "*Wget command. Use only if `auto-install-use-wget' is non-nil."
  :type 'string  
  :group 'auto-install)

(defcustom auto-install-use-wget (executable-find "wget")
  "*Use wget instead of `url-retrieve'.

It is enabled by default when wget is found."
  :type 'boolean  
  :group 'auto-install)

(defcustom auto-install-batch-list
  nil
  "This list contain packages information for batch install.

Have four arguments per list:
First argument is extension name.
Second argument is delay time for batch install.
Third argument is libraries number limit in delay time.
Fourth argument is list of libraries url or extension name.

If you want to add files, please edit auto-install-batch-list.el in EmacsWiki.
Use M-x `auto-install-batch-edit'. "
  :group 'auto-install)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Variable ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar auto-install-batch-list-internal nil
  "The real value of `auto-install-batch-list'. ")

(defvar auto-install-batch-list-el-url
  "http://www.rubyist.net/~rubikitch/archive/auto-install-batch-list.el"
  "The url of auto-install-batch-list.el.
It is downloaded and evaluated just after M-x `auto-install-batch'. ")

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

(defvar auto-install-add-exec-path-flag t
  "If non-nil, add `auto-install-directory' to `exec-path'.
This variable is intended to be used in test.

It is needed because `auto-install-batch' can install non-elisp files.")

(defvar auto-install-waiting-url-list nil
  "URLs in downloading.")
(defvar auto-install-url-queue nil
  "Installation order.")
(defvar auto-install-download-buffer-alist nil
  "Pairs of URL and downloaded buffer.")
(defvar auto-install-batch-using nil)

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

(defun auto-install-from-gist (&optional gistid-or-url)
  "Install an elisp file from gist.github.com.

Optional argument GISTID-OR-URL is gist ID or URL for download
elisp file from gist.github.com."
  (interactive)
  (or gistid-or-url
      (setq gistid-or-url (read-string (format "Gist ID or URL (%s): " (or auto-install-last-gist-id ""))
                                       nil nil
                                       auto-install-last-gist-id)))
  (let ((gistid (file-name-sans-extension (file-name-nondirectory gistid-or-url))))
    (setq auto-install-last-gist-id gistid)
    (auto-install-download (format "%s%s.txt" auto-install-gist-base-url gistid))))

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

(defun auto-install-network-available-p (host)
  (if auto-install-use-wget
      (eq (call-process auto-install-wget-command nil nil nil "-q" "--spider" host) 0)
    (with-current-buffer (url-retrieve-synchronously (concat "http://" host))
      (prog1 (not (zerop (buffer-size)))
        (kill-buffer (current-buffer))))))
;; (auto-install-network-available-p "www.emacswiki.org")
(require 'timer)
(defun auto-install-update-emacswiki-package-name (&optional unforced)
  "Update the list of elisp package names from `EmacsWiki'.
By default, this function will update package name forcibly.
If UNFORCED is non-nil, just update package name when `auto-install-package-name-list' is nil."
  (interactive)
  (unless (and unforced
               auto-install-package-name-list)
    (if (and (auto-install-network-available-p "www.emacswiki.org")
             (with-timeout (5 nil)
               (progn (auto-install-download "http://www.emacswiki.org/cgi-bin/emacs?action=index;raw=1"
                                             'auto-install-handle-emacswiki-package-name))
               t))
      (message
       (concat "Network unreachable!\n"
               "Try M-x auto-install-handle-emacswiki-package-name afterward."))
      (sit-for 2))))

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
  "Batch install many files (libraries and non-elisp files) in some extension.
EXTENSION-NAME is extension name for batch install.

Note that non-elisp can be installed only via `auto-install-batch'"
  (interactive)
  (if (and auto-install-batch-list-internal extension-name)
      (auto-install-batch-real extension-name)
    (auto-install-download
     auto-install-batch-list-el-url
     (lexical-let ((extension-name extension-name))
       (lambda (buf)
         (with-current-buffer buf
           (eval-buffer)
           (run-at-time 0 nil 'auto-install-batch-real extension-name)))))))

(defun auto-install-batch-edit ()
  "Edit auto-install-batch-list.el"
  (interactive)
  (cond ((fboundp 'yaoddmuse-edit)
         (yaoddmuse-edit "EmacsWiki" "auto-install-batch-list.el"))
        ((fboundp 'oddmuse-edit)
         (oddmuse-edit "EmacsWiki" "auto-install-batch-list.el"))
        (t
         (browse-url "http://www.emacswiki.org/emacs/?action=edit;id=auto-install-batch-list.el"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Utilities Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun auto-install-batch-real (&optional extension-name)
  (setq auto-install-batch-using t)
  (when auto-install-add-exec-path-flag
      (add-to-list 'exec-path auto-install-directory))
  (destructuring-bind (name delay-time limit-number (&rest urls))
      (auto-install-batch-get-info
       (or
        ;; Get information list from give extension name.
        extension-name
        ;; Otherwise completion from user select.
        (completing-read "Extension name: " (mapcar 'car auto-install-batch-list-internal)))
       auto-install-batch-list-internal)
    (or name (error "Haven't install information for `%s'." extension-name))
    (setq auto-install-waiting-url-list urls
          auto-install-url-queue urls)
    (if (not (and
              ;; Delay time is above 0.
              delay-time
              (> delay-time 0)
              ;; Limit number is above 0.
              limit-number
              (> limit-number 0)))
        (auto-install-from-url-list urls)
      (let ((delay-counter 0)
            install-list)
        (while urls
          (if (> (length urls) limit-number)
              ;; Install apart libraries list under `limit-number'
              (progn
                (setq install-list (nthcar limit-number urls))
                (run-with-timer
                 (* delay-counter delay-time)
                 nil
                 'auto-install-from-url-list install-list)
                (setq urls (nthcdr+ limit-number urls))
                (incf delay-counter))
            ;; Install remain libraries list.
            (setq install-list urls)
            (run-with-timer
             (* delay-counter delay-time)
             nil
             'auto-install-from-url-list install-list)
            (setq urls nil)))))))

;;; borrowed from eev.el
(defun auto-install-flatten (obj &rest rest)
  (cond (rest (append (auto-install-flatten obj) (auto-install-flatten rest)))
	((null obj) nil)
	((listp obj) (append (auto-install-flatten (car obj)) (auto-install-flatten (cdr obj))))
	(t (list obj))))

(defun auto-install-batch-get-info (extension batch-list)
  (let* ((it (assoc extension batch-list))
         (urls (car (last it)))
         (urlp (lambda (url) (string-match "^https?://" url))))
    (cond ((not it)
           '(nil nil nil (nil)))
          ((loop for url in urls always (funcall urlp url))
           it)
          (t
           (append
            (butlast it)
            (list (auto-install-flatten
                   (loop for url in urls collect
                         (if (funcall urlp url)
                             url
                           (car (last (auto-install-batch-get-info url batch-list))))))))))))

;;;; unit test
;; (install-elisp "http://www.emacswiki.org/cgi-bin/wiki/download/el-expectations.el")
;; (install-elisp "http://www.emacswiki.org/cgi-bin/wiki/download/el-mock.el")
(dont-compile
  (when (fboundp 'expectations)
    (expectations
      (desc "auto-install-flatten")
      (expect '(1 2 3 4 5 6 7 8 9)
        (auto-install-flatten '((1 2 3) (4 5) (((6)) 7) nil nil 8 9)))
      (expect '(1 2 3 4 5 6 7 8 9)
        (auto-install-flatten '(1 2 3) '(4 5) '(((6)) 7) nil nil 8 9))
      (desc "auto-install-batch-get-info")
      (expect '(nil nil nil (nil))
        (auto-install-batch-get-info
         "not-found"
         '(("foo" nil nil ("https://example.com/1.el")))))
      (expect '("foo" nil nil ("http://example.com/1.el"))
        (auto-install-batch-get-info
         "foo"
         '(("foo" nil nil ("http://example.com/1.el")))))
      (expect '("withdep" nil nil ("http://example.com/1.el"
                                   "http://example.com/2.el"))
        (auto-install-batch-get-info
         "withdep"
         '(("foo" nil nil ("http://example.com/1.el"))
           ("withdep" nil nil ("foo" "http://example.com/2.el")))))
      (expect '("withdep-recursive" nil nil ("http://example.com/1.el"
                                             "http://example.com/2.el"
                                             "http://example.com/3.el"))
        (auto-install-batch-get-info
         "withdep-recursive"
         '(("foo" nil nil ("http://example.com/1.el"))
           ("withdep" nil nil ("foo" "http://example.com/2.el"))
           ("withdep-recursive" nil nil ("withdep" "http://example.com/3.el")))))
      )))


(defun auto-install-use-wget-p ()
  (and auto-install-use-wget
       (executable-find auto-install-wget-command)))
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
  (funcall
   (if (auto-install-use-wget-p)
       'auto-install-download-by-wget
     'auto-install-download-by-url-retrieve)
   url handle-function (auto-install-get-buffer url)))

(defun auto-install-download-by-wget (url handle-function download-buffer)
  (with-current-buffer download-buffer
    (setq auto-install-download-buffer (get-buffer-create (concat (buffer-name download-buffer)
                                                "-wget")))
    (setq auto-install-download-url url)
    (set-process-sentinel
     (start-process "auto-install-wget" (current-buffer)
                    auto-install-wget-command "-q" "-O-" "--no-check-certificate" url)
     (lexical-let ((handle-function handle-function))
       (lambda (proc stat)
         (auto-install-download-callback-continue (buffer-name (process-buffer proc))
                                                  handle-function))))))

(defun auto-install-download-by-url-retrieve (url handle-function download-buffer)
  (let* ((url-request-method "GET")
         (url-request-extra-headers nil)
         (url-mime-accept-string "*/*")
         (parsed-url (url-generic-parse-url url))
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
    (auto-install-download-callback-continue download-buffer-name handle-function)))

(defun auto-install-download-callback-continue (download-buffer-name handle-function)
  (auto-install-retrieve-decode download-buffer-name) ;decode retrieve information.
  (with-current-buffer (get-buffer download-buffer-name)
    ;; Show successful message
    (message "Download from '%s' successful." auto-install-download-url)
    ;; Handle download content.
    (funcall (or handle-function 'auto-install-handle-download-content)
             (current-buffer))))

(defun auto-install-retrieve-decode (retrieve-buffer-name)
  "Decode the RETRIEVE-BUFFER-NAME with coding detection."
  (declare (special url-http-end-of-headers))
  (with-current-buffer (get-buffer retrieve-buffer-name)
    (insert
     (with-current-buffer auto-install-download-buffer
       (set-buffer-multibyte t)
       ;; I do not know why the case url-http-end-of-headers is nil exists!!
       ;; I HATE url-retrieve.
       (if (and (boundp 'url-http-end-of-headers)
                (numberp url-http-end-of-headers))
           (goto-char (1+ url-http-end-of-headers))
         ;; workaround
         (if (auto-install-use-wget-p)
             (goto-char (point-min))
           (search-forward "\n\n" nil t)))
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
      (if (not (and (not auto-install-batch-using) (auto-install-check-update)))
          (unless auto-install-save-confirm
            (auto-install-buffer-save))
        (message "%s is up-to-date" (url-file-nondirectory auto-install-download-url))
        (kill-buffer))
      ;; (unless auto-install-save-confirm
      ;;   (auto-install-buffer-save))
      )))

(defun auto-install-check-update ()
  (let* ((new-file (url-file-nondirectory auto-install-download-url))
         (old-file (auto-install-get-path new-file))
         (old-content (and old-file
                           (with-temp-buffer
                             (insert-file-contents-literally old-file)
                             (buffer-string)))))
    (equal old-content (buffer-string))))

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
        (unless auto-install-batch-using
          ;; Make sure file suffix with `.el'.
          (while (not (string-match ".*\.el$" filename))
           (setq filename (read-string "Please input file name suffix with `.el': "))))
        ;; Get file path.
        (setq file-path
              (or
               ;; Replace file if have exist.
               (auto-install-get-path filename)
               ;; Otherwise, install in directory `auto-install-directory'.
               (expand-file-name filename auto-install-directory)))
        ;; Save file.
        (if (and (file-exists-p file-path)
                 (file-writable-p file-path)
                 auto-install-replace-confirm
                 (not (yes-or-no-p (format "Do you want replace file: '%s' ?" file-path))))
            (auto-install-quit)
          (if (auto-install-check-update)
              (auto-install-install-next-file)
            (let ((before-save-hook before-save-hook)
                  (auto-async-byte-compile-exclude-files-regexp
                   (regexp-quote file-path)))
              (remove-hook 'before-save-hook 'time-stamp)
              (write-file file-path)
              (auto-install-install file-path)))))
    (error "This command just use in `auto-install-minor-mode'.")))

(defun auto-install-install (file-path)
  "Install elisp file FILE-PATH."
  (if (and auto-install-install-confirm
           (not (yes-or-no-p (format "Do you want install file: '%s' ?" file-path))))
      (auto-install-quit)
    (let (byte-compile-warnings) ;; suppress compile warnings
      ;; Compile and load file.
      (when (and (string= "el" (file-name-extension file-path))
                 (not (ignore-errors (byte-compile-file file-path t))))
        ;; Show `ERROR' message if compile failed.
        (message "Auto-Install ERROR: Compiled file '%s' failed." file-path))
      (auto-install-install-next-file))))

(defun auto-install-install-next-file ()
  (setq auto-install-url-queue (cdr auto-install-url-queue))
  (let (byte-compile-warnings)
    (cond ((car auto-install-url-queue)
           (switch-to-buffer (assoc-default (car auto-install-url-queue)
                                            auto-install-download-buffer-alist))
           (unless auto-install-save-confirm
             (auto-install-buffer-save)))
          (t                        ;completed
           (auto-install-cleanup)
           (message "Installation is completed.")))))

(defun auto-install-cleanup ()
  (while auto-install-minor-mode
    (kill-buffer))
  (setq auto-install-url-queue nil)
  (setq auto-install-download-buffer-alist nil)
  (setq auto-install-batch-using nil))

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

;;;; Bug report
(defvar auto-install-maintainer-mail-address
  (concat "rubiki" "tch@ru" "by-lang.org"))
(defvar auto-install-bug-report-salutation
  "Describe bug below, using a precise recipe.

When I executed M-x ...

How to send a bug report:
  1) Be sure to use the LATEST version of auto-install.el.
  2) Enable debugger. M-x toggle-debug-on-error or (setq debug-on-error t)
  3) Use Lisp version instead of compiled one: (load \"auto-install.el\")
  4) If you got an error, please paste *Backtrace* buffer.
  5) Type C-c C-c to send.
# If you are a Japanese, please write in Japanese:-)")
(defun auto-install-send-bug-report ()
  (interactive)
  (reporter-submit-bug-report
   auto-install-maintainer-mail-address
   "auto-install.el"
   (apropos-internal "^auto-install-" 'boundp)
   nil nil
   auto-install-bug-report-salutation))


(provide 'auto-install)

;; TestCase
;; (find-file-other-window "~/memo/junk/2010-04-25-094621.auto-install-check-update.test.el")

;; How to save (DO NOT REMOVE!!)
;; (emacswiki-post "auto-install.el")
;;; auto-install.el ends here

;;; LocalWords:  el eol dirs fontify gistid txt func bytecomp DDirectory ediff
;;; LocalWords:  noselect Unmark unmark AutoInstall keybindings defalias'es

