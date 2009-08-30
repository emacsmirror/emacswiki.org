;;; wikirel.el --- Visit relevant pages on the Emacs Wiki

;; Copyright (C) 2008  Aaron S. Hawley

;; Author: Aaron S. Hawley
;; Keywords: hypermedia
;; Version: %Id: 1%
;; RCS Version: $Id: wikirel.el,v 1.1 2008/12/23 18:18:28 aaronh Exp $
;; URL: http://www.emacswiki.org/elisp/wikirel.el

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.  See <http://www.gnu.org/licenses/>.

;;; Commentary:

;; The library wikirel.el opens pages on Emacs Wiki in a Web browser
;; or in Oddmuse mode based on the context the user is currently
;; working in in Emacs.

;; Opens a page in a Web browser using browse-url or Oddmuse mdoe for
;; either the current major mode or a minor mode.

;; To browse the current major mode in a Web browser:

;; M-x wikirel-browse-major-mode

;; To list the current minor modes in a separate buffer and browse
;; any one of them in a Web browser:

;; M-x wikirel-browse-minor-modes

;; To view the current major mode in Oddmuse mode:

;; M-x wikirel-oddmuse-major-mode

;; To list the current minor modes in a separate buffer and
;; view any one of them in Oddmuse mode:

;; M-x wikirel-oddmuse-minor-modes

;;; History:

;; Iniitally written on July 18, 2007 in South Burlington, VT, USA
;; based on a proposal by Bruce Ingalls.  First released as wikirel.el
;; on December 23, 2008.

;;; Code:

(require 'browse-url)
(require 'oddmuse)

(defvar wikirel-url-root "http://emacswiki.org/emacs/"
  "URL for the Emacs Wiki.")

(defvar wikirel-page-suffix ""
  "Suffix for page URLs.  For example, this could be \".html\".")

(defun wikirel-browse-major-mode ()
  "Browse the current Emacs major mode in a Web browser:

See `browse-url'."
  (interactive)
  (browse-url (wikirel-make-url major-mode)))

(defun wikirel-oddmuse-major-mode ()
  "View the current Emacs major mode in Oddmuse mode.

See `oddmuse-go'."
  (interactive)
  (oddmuse-go "EmacsWiki" (wikirel-symbol-to-page-name major-mode)))

(defun wikirel-browse-minor-modes ()
"List the current Emacs minor modes for Web browsing.

In a separate buffer, the current Emacs minor modes are listed.
Choose any of the links to browse one of them in a Web browser:

See `browse-url'."
  (interactive)
  (wikirel-minor-mode-pages 'browse-url))

(defun wikirel-oddmuse-minor-modes ()
"List the current minor modes in a separate buffer for Oddmus mode.

In a separate buffer, the current Emacs minor modes are listed.
Choose any of the links to view their source code in Oddmuse mode.

See `oddmuse-go'."
  (interactive)
  (wikirel-minor-mode-pages 'oddmuse))

(defun wikirel-minor-mode-pages (browse-or-oddmuse)
  "Make a separate buffer of minor modes for BROWSE-OR-ODDMUSE.

If BROWSE-OR-ODDMUSE.is the symbol \"oddmuse\" then the
links open in Oddmus mode.  Otherwise, if the symbol is
\"browse-url\" then the links open in a Web browser."
  (let ((minor-modes (sort
                      (mapcar 'symbol-name
                              (wikirel-get-minor-modes))
                      'string<))
        (emacswiki-buffer "*EmacsWiki*"))
    (if (< (length minor-modes) 1)
        (message "No minor modes are active")
      (with-output-to-temp-buffer emacswiki-buffer
        (with-current-buffer standard-output
          (insert "Current minor mode(s):")
          (newline 2)
          (mapcar (lambda (minor-mode)
                    (insert "* ")
                    (insert-button minor-mode 'action
                                   (if (equal 'oddmuse
					      browse-or-oddmuse)
				       'wikirel-minor-mode-oddmuse-button
				     'wikirel-minor-mode-browse-button)
                                   'wikirel-minor-mode-browse-button
                                   'follow-link t
                                   'help-echo
                                   (if (equal 'oddmuse
					      browse-or-oddmuse)
				       "mouse-2, RET: go to EmacsWiki page"
				     "mouse-2, RET: browse EmacsWiki page"))
                    (newline))
                  minor-modes)))
      (pop-to-buffer emacswiki-buffer)
      (shrink-window-if-larger-than-buffer))))

(defun wikirel-minor-mode-browse-button (at-button)
  "Action AT-BUTTON for opening minor mode links in a Web browser."
  (let ((minor-mode
         (with-current-buffer (overlay-buffer at-button)
           (buffer-substring-no-properties
            (overlay-start at-button) (overlay-end at-button)))))
    (browse-url
     (wikirel-make-url (make-symbol minor-mode)))))

(defun wikirel-minor-mode-oddmuse-button (at-button)
  "Action AT-BUTTON for opening minor mode Oddmuse mode."
  (let ((minor-mode
         (with-current-buffer (overlay-buffer at-button)
           (buffer-substring-no-properties
            (overlay-start at-button) (overlay-end at-button)))))
    (oddmuse-go "EmacsWiki"
     (wikirel-symbol-to-page-name (make-symbol minor-mode)))))

(defun wikirel-get-minor-modes ()
  "Get current minor modes.

Written based on code in `describe-mode'."
  (delq nil
        (mapcar (lambda (mode)
                  (and (boundp mode)
                       (symbol-value mode)
                       (fboundp
                        (or (get mode :minor-mode-function)
                            mode))
                       mode))
                (append
                 ;; Older packages register in minor-mode-alist but
                 ;; not in minor-mode-alist.
                 (mapcar (lambda (x)
                           (if (null (memq (car x) minor-mode-list))
                               (car x)
                             nil))
                         minor-mode-alist)
                 minor-mode-list))))

(defun wikirel-make-url (sym)
  "Convert symbol SYM to a URL on Emacs Wiki."
  (concat wikirel-url-root
          (wikirel-symbol-to-page-name sym)
          wikirel-page-suffix))

(defun wikirel-symbol-to-page-name (sym)
  "Convert symbol SYM to a PageName on Emacs Wiki."
  (wikirel-convert-wikiisms
   (wikirel-symbol-to-CamelCase sym)))

(defun wikirel-symbol-to-CamelCase (sym)
  "Convert symbol SYM to CamelCase."
  (replace-regexp-in-string "-" ""
                            (capitalize (symbol-name sym))))

(defun wikirel-convert-wikiisms (str)
  "Convert recognized characters in STR to Wiki equivalents."
  (replace-regexp-in-string (regexp-quote "+") "Plus" str))

(provide 'wikirel)
;;; wikirel.el ends here
