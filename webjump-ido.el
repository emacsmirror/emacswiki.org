;;; webjump-ido.el --- Use ido for selecting URLs for webjump

;; Filename: webjump-ido.el
;; Description: Use ido for selecting URLs for webjump
;; Author: Joe Bloggs <vapniks@yahoo.com>
;; Maintainer: Joe Bloggs <vapniks@yahoo.com>
;; Copyleft (Ↄ) 2015, Joe Bloggs, all rites reversed.
;; Created: 2015-09-01 23:59:48
;; Version: 0.1
;; Last-Updated: 2015-09-01 23:59:48
;;           By: Joe Bloggs
;; URL: https://github.com/vapniks/webjump-ido
;; Keywords: convenience
;; Compatibility: GNU Emacs 24.5.1
;; Package-Requires:  
;;
;; Features that might be required by this library:
;;
;; webjump
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
;; along with this program; see the file COPYING.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary: 
;;
;; Bitcoin donations gratefully accepted: 12k9zUo9Dgqk8Rary2cuzyvAQWD5EAuZ4q
;;
;; This package provides an ido version of the `webjump' command, with customizable
;; URL's
;; 
;;;;

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `webjump-ido' : Jumps to a Web site from a programmable hotlist.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;
;;  `webjump-ido-sites' : 
;;    *Hotlist for WebJump.
;;    default = (if (featurep (quote webjump-plus)) (append webjump-plus-sites webjump-sample-sites) webjump-sample-sites)


;;; Installation:
;;
;; Put webjump-ido.el in a directory in your load-path, e.g. ~/.emacs.d/
;; You can add a directory to your load-path with the following line in ~/.emacs
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;; where ~/elisp is the directory you want to add 
;; (you don't need to do this for ~/.emacs.d - it's added by default).
;;
;; Add the following to your ~/.emacs startup file.
;;
;; (require 'webjump-ido)

;;; Customize:
;;
;; To automatically insert descriptions of customizable variables defined in this buffer
;; place point at the beginning of the next line and do: M-x auto-document

;;
;; All of the above can customized by:
;;      M-x customize-group RET webjump-ido RET
;;

;;; Require
(require 'webjump)
(require 'webjump-plus nil t)

;;; Code:

(defcustom webjump-ido-sites (if (featurep 'webjump-plus)
                                    (append webjump-plus-sites webjump-sample-sites)
                                  webjump-sample-sites)
 "Hotlist for `webjump-ido'. This variable takes the same for as `webjump-sites' (which see),
but is specific to `webjump-ido', and is customizable. By default it should contains the same
URL's as `webjump-sample-sites' and `webjump-plus-sites' (if available)."
  :type '(alist :key-type (string :tag "Name")
                :value-type (choice (string :tag "URL")
                                    (choice :tag "Vector"
                                            (vector :tag "Name"
                                                    (const name))
                                            (vector :tag "Simple-query"
                                                    (const simple-query)
                                                    (string :tag "Static URL")
                                                    (string :tag "Prefix")
                                                    (string :tag "Suffix"))
                                            (sexp :tag "Mirrors"
                                                  :help-echo "This should be a vector whos first element is the symbol 'mirrors and subsequent elements are strings containing URLs of mirror sites."))
                                    (function :tag "Function")
                                    (sexp :tag "Sexp to eval"))))

;;;###autoload
(defun webjump-ido nil
  "Jumps to a Web site from a programmable hotlist, using ido to select the website.
See the documentation for the `webjump-ido-sites' variable for how to customize the hotlist."
  (interactive)
  (let* ((completion-ignore-case t)
         (item (assoc-string
                (ido-completing-read "WebJump to site: "
                                     (mapcar 'car webjump-ido-sites) nil t)
                webjump-ido-sites t))
         (name (car item))
         (expr (cdr item)))
    (browse-url (webjump-url-fix
                 (cond ((not expr) "")
                       ((stringp expr) expr)
                       ((vectorp expr) (webjump-builtin expr name))
                       ((listp expr) (eval expr))
                       ((symbolp expr)
                        (if (fboundp expr)
                            (funcall expr name)
                          (error "WebJump URL function \"%s\" undefined"
                                 expr)))
                       (t (error "WebJump URL expression for \"%s\" invalid"
                                 name)))))))

(provide 'webjump-ido)

;; (magit-push)
;; (yaoddmuse-post "EmacsWiki" "webjump-ido.el" (buffer-name) (buffer-string) "update")

;;; webjump-ido.el ends here
