;;; yaoddmuse-extension.el --- Some enhanced functions for yaoddmuse.el

;; Filename: yaoddmuse-extension.el
;; Description: Some enhanced functions for yaoddmuse.el
;; Author: Andy Stewart lazycat.manatee@gmail.com
;; Maintainer: Andy Stewart lazycat.manatee@gmail.com
;; Copyright (C) 2009, Andy Stewart, all rights reserved.
;; Created: 2009-01-09 22:27:36
;; Version: 0.2.3
;; Last-Updated: 2009-03-11 22:01:29
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/yaoddmuse-extension.el
;; Keywords: oddmuse, yaoddmuse
;; Compatibility: GNU Emacs 22 ~ 23
;;
;; Features that might be required by this library:
;;
;; `yaoddmuse' `w3m'
;;

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
;; Some enhanced functions for `yaoddmuse.el'.
;;
;; 1. Function `yaoddmuse-browse-page-in-w3m' is an enhanced function to
;;    replace the default browse function (`browse-url) in Lisp:yaoddmuse.el.
;;    By default, ‘browse-url’ just opens a new buffer.
;;    `yaoddmuse-browse-page-in-w3m' will search for an existing buffer
;;    containing the page, and update that instead of duplicating the page in w3m.
;;    It’s very useful when you edit a lot of pages in a single session.
;;
;;    Setup like below:
;;    (setq yaoddmuse-browse-function 'yaoddmuse-browse-page-in-w3m)
;;
;; 2. `yaoddmuse-w3m-edit-emacswiki-page' is a lazy function.
;;    You can use this function to edit the EmacsWiki page displayed
;;    in the current w3m buffer without entering the page name.
;;
;; 3. Command `yaoddmuse-yasnippet-insert-file' and `yaoddmuse-yasnippet-insert-directory'
;;    can fast transform Yasnippet template code with EmacsWiki format and insert it.
;;    So you can update your Yasnippet template fast, handy to sharing with others.  :)
;;
;; 4. Function `yaoddmuse-notify-popup-window' use program `notify-send'
;;    notify yaoddmuse message.
;;
;;    Setup like below:
;;    (setq yaoddmuse-notify-function 'yaoddmuse-notify-popup-window)
;;

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `yaoddmuse-yasnippet-insert-file'
;;    Insert `yasnippet' template FILE.
;;  `yaoddmuse-yasnippet-insert-directory'
;;    Insert `yasnippet' template file under DIR.
;;  `yaoddmuse-w3m-edit-emacswiki-page'
;;    Edit current emacswiki wiki page.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;
;;  `yaoddmuse-notify-cmd'
;;    The command that use for notify.
;;    default = "notify-send"
;;  `yaoddmuse-notify-icon'
;;    Specifies an icon filename or stock icon to display.
;;    default = "~/MyEmacs/Image/Irc.png"
;;  `yaoddmuse-notify-timeout'
;;    Specifies the timeout in milliseconds at which to expire the notification.
;;    default = 5000
;;  `yaoddmuse-notify-urgency'
;;    Specifies the urgency level (low, normal, critical).
;;    default = "low"
;;  `yaoddmuse-notify-category'
;;    Specifies the notification category.
;;    default = "im.received"

;;; Installation:
;;
;; Put yaoddmuse-extension.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'yaoddmuse-extension)
;;
;; No need more.

;;; Change log:
;; 2009/03/11
;;  * Andy Stewart:
;;      * Fix bug of `yaoddmuse-w3m-edit-emacswiki-page'.
;;      * Fix doc.
;;
;; 2009/03/11
;;   * rubikitch
;;      * Add new function `yaoddmuse-notify-by-growl'
;; 2009/02/22
;;   * Andy Stewart:
;;      * Add new function `yaoddmuse-notify-popup-window'
;;      * Fix doc.
;;
;; 2009/02/13
;;   * Andy Stewart:
;;      * Add new command `yaoddmuse-yasnippet-insert-file'
;;        and `yaoddmuse-yasnippet-insert-directory'.
;;        Fast insert yasnippet for sharing. :)
;;
;; 2009/01/09
;;      * First released.
;;

;;; Acknowledgements:
;;
;;
;;

;;; TODO
;;
;;
;;

;;; Require
(require 'yaoddmuse)
(require 'w3m)

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Customize ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom yaoddmuse-notify-cmd "notify-send"
  "The command that use for notify."
  :type 'string
  :group 'yaoddmuse)

(defcustom yaoddmuse-notify-icon "~/MyEmacs/Image/Irc.png"
  "Specifies an icon filename or stock icon to display."
  :type 'string
  :group 'yaoddmuse)

(defcustom yaoddmuse-notify-timeout 5000
  "Specifies the timeout in milliseconds at which to expire the notification."
  :type 'number
  :group 'yaoddmuse)

(defcustom yaoddmuse-notify-urgency "low"
  "Specifies the urgency level (low, normal, critical)."
  :type 'string
  :group 'yaoddmuse)

(defcustom yaoddmuse-notify-category "im.received"
  "Specifies the notification category."
  :type 'string
  :group 'yaoddmuse)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Interactive Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun yaoddmuse-yasnippet-insert-file (file)
  "Insert `yasnippet' template FILE."
  (interactive "fFile: ")
  (insert
   (with-temp-buffer
     ;; Insert template content.
     (insert-file-contents file)
     ;; Indent template content.
     (string-insert-rectangle (point-min) (point-max) "    ")
     ;; Insert }}}
     (goto-char (point-max))
     (newline)
     (insert "}}}\n\n")
     ;; Insert {{{
     (goto-char (point-min))
     (insert "{{{")
     (newline)
     ;; Insert template title.
     (goto-char (point-min))
     (open-line 1)
     (insert (format "* %s" (file-name-nondirectory file)))
     ;; Return.
     (buffer-string))))

(defun yaoddmuse-yasnippet-insert-directory (dir)
  "Insert `yasnippet' template file under DIR."
  (interactive "DDirectory: ")
  (dolist (file (directory-files dir t))
    (unless (file-directory-p file)
      (unless (string-match "^\\.?#" (file-name-nondirectory file))
        (yaoddmuse-yasnippet-insert-file file)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Utilities Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun yaoddmuse-browse-page-in-w3m (url)
  "This function is browse URL in w3m."
  (let ((current-window (selected-window)))
    ;; Switch window if major-mode
    ;; is not `w3m-mode'.
    (unless (eq major-mode 'w3m-mode)
      (other-window 1))
    (catch 'find-match
      ;; Switch w3m buffer if find
      ;; match page.
      (dolist (buffer (buffer-list))
        (set-buffer buffer)
        (when (eq major-mode 'w3m-mode)
          (if (or
               ;; `w3m-current-url' equal search url.
               (string-equal url w3m-current-url)
               ;; Or match corresponding page in `emacswiki'.
               (yaoddmuse-match-emacswiki-page w3m-current-url url))
              (with-current-buffer buffer
                (switch-to-buffer buffer)
                (w3m-reload-this-page)
                (throw 'find-match "Find same page in w3m-mode.")))))
      ;; Otherwise open new page.
      (w3m-goto-url-new-session url t))
    ;; Select original window.
    (select-window current-window)))

(defun yaoddmuse-match-emacswiki-page (current-url search-url)
  "Return t if current url match search url for EmacsWiki page.
Otherwise return nil.
CURRENT-URL is current w3m buffer url.
SEARCH-URL is url try to search."
  (and (string-match "^http://www.emacswiki.org" (or current-url ""))
       (let ((page-name (replace-regexp-in-string
                         (format "^%s/" (cadr (assoc "EmacsWiki" yaoddmuse-wikis))) "" search-url)))
         (or (string-equal (format "http://www.emacswiki.org/emacs/%s" page-name) current-url)
             (string-equal (format "http://www.emacswiki.org/emacs-en/%s" page-name) current-url)
             (string-equal (format "http://www.emacswiki.org/cgi-bin/emacs/%s" page-name) current-url)))))

(defun yaoddmuse-w3m-edit-emacswiki-page ()
  "Edit current emacswiki wiki page."
  (interactive)
  (yaoddmuse-edit "EmacsWiki" (replace-regexp-in-string
                               "\\(.*id=\\).*$" "" ;pick up page name from `id=PageName'
                               (replace-regexp-in-string
                                "http://.*/\\([^/]+\\?\\)?" "" ;pick up last path from url
                                w3m-current-url)
                               nil nil 1)))

(defun yaoddmuse-notify-popup-window (msg)
  "Use program `notify-send' notify yaoddmuse-message MSG."
  (flet ((message (&rest args)))
    (shell-command (concat yaoddmuse-notify-cmd
                           " -i " yaoddmuse-notify-icon
                           " -t " (int-to-string
                                   yaoddmuse-notify-timeout)
                           " -u " yaoddmuse-notify-urgency
                           " -c " yaoddmuse-notify-category
                           " -- "
                           " \"Yaoddmuse-Notify\""
                           " \""
                           (if (boundp 'msg)
                               msg "")
                           "\""))))

(defun yaoddmuse-notify-by-growl (msg)
  "Use program `growl' notify yaoddmuse-message MSG."
  (if (require 'growl nil t)
      (with-no-warnings
        (growl "Yaoddmuse" msg))))

(provide 'yaoddmuse-extension)

;;; yaoddmuse-extension.el ends here

;;; LocalWords:  yaoddmuse oddmuse yasnippet fFile DDirectory cmd im msg
;;; LocalWords:  PageName
