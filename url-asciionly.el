;;;; url-asciionly.el --- ffap url handling workaround
;; $Id: url-asciionly.el,v 1.2 2010/03/25 10:10:16 rubikitch Exp $

;; Copyright (C) 2010  rubikitch

;; Author: rubikitch <rubikitch@ruby-lang.org>
;; Keywords: i18n, lisp
;; URL: http://www.emacswiki.org/cgi-bin/wiki/download/url-asciionly.el

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; 

;;; Commands:
;;
;; Below are complete command list:
;;
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;

;;; Installation:
;;
;; Put url-asciionly.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'url-asciionly)
;;
;; No need more.

;;; Customize:
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET url-asciionly RET
;;


;;; History:

;; $Log: url-asciionly.el,v $
;; Revision 1.2  2010/03/25 10:10:16  rubikitch
;; document
;;
;; Revision 1.1  2010/03/25 10:08:25  rubikitch
;; Initial revision
;;

;;; Code:

(defvar url-asciionly-version "$Id: url-asciionly.el,v 1.2 2010/03/25 10:10:16 rubikitch Exp $")
(defgroup url-asciionly nil
  "url-asciionly"
  :group 'emacs)

(defadvice ffap-string-at-point (around practical-url activate)
  "Workaround to ensure URL ascii only."
  (if (eq (ad-get-arg 0) 'url)
      (let ((result ad-do-it))
        (setq ad-return-value
              (with-temp-buffer
                (insert result)
                (and (re-search-backward "https?://" nil t)
                     (delete-region 1 (point)))
                (goto-char 1)
                (skip-chars-forward "^[:ascii:]")
                (delete-region 1 (point))
                (skip-chars-forward "[:ascii:]")
                (buffer-substring 1 (point)))))
    ad-do-it))

;;;; unit test
;; (install-elisp "http://www.emacswiki.org/cgi-bin/wiki/download/el-expectations.el")
;; (install-elisp "http://www.emacswiki.org/cgi-bin/wiki/download/el-mock.el")
(dont-compile
  (when (fboundp 'expectations)
    (expectations
      (desc "ffap url")
      (expect "http://www.rubyist.net/~rubikitch/"
        (ffap-url-test "http://www.rubyist.net/~rubikitch/"))
      (expect "http://www.rubyist.net/~rubikitch/"
        (ffap-url-test "a http://www.rubyist.net/~rubikitch/"))
      (expect "http://www.rubyist.net/~rubikitch/"
        (ffap-url-test "a http://www.rubyist.net/~rubikitch/ b"))
      (expect "http://www.rubyist.net/~rubikitch/"
        (ffap-url-test "あhttp://www.rubyist.net/~rubikitch/"))
      (expect "http://www.rubyist.net/~rubikitch/"
        (ffap-url-test "http://www.rubyist.net/~rubikitch/あ"))
      (expect "http://www.rubyist.net/~rubikitch/"
        (ffap-url-test "http://www.rubyist.net/~rubikitch/あ0"))
      (expect "http://www.rubyist.net/~rubikitch/"
        (ffap-url-test "aaa:http://www.rubyist.net/~rubikitch/"))
      )))

(provide 'url-asciionly)

;; How to save (DO NOT REMOVE!!)
;; (emacswiki-post "url-asciionly.el")
;;; url-asciionly.el ends here
