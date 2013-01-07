;;; pastebin.el --- A simple interface to the www.pastebin.com webservice

;;; Copyright (C) 2008 by Nic Ferrier <nic@ferrier.me.uk>
;;; Copyright (C) 2010 by Ivan Korotkov <twee@tweedle-dee.org>
;;; Copyright (C) 2012 by Filonenko Michael <filonenko.mikhail@gmail.com>
;;; Copyright (C) 2012 by Daniel Hilst <danielhilst@gmail.com>

;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with this program; see the file COPYING.  If not, write to the
;;; Free Software Foundation, Inc.,   51 Franklin Street, Fifth Floor,
;;; Boston, MA  02110-1301  USA

;;; Commentary:
;;;
;;; Make pastebin account:
;;;
;;;   http://pastebin.com/signup
;;; 
;;; Get Unique developer key:
;;;
;;;   http://pastebin.com/api  
;;;
;;; Setup emacs:
;;;
;;;   (require 'pastebin)
;;;   (setq pastebin-unique-developer-api-key "HEX") ;; from http://pastebin.com/api
;;;   (setq pastebin-user-name "username")
;;;   (setq pastebin-password "password")
;;;
;;;   (pastebin-login)
;;;
;;; To send the whole buffer or select region and run
;;;
;;;  M-x pastebin
;;;
;;; In either case the url that pastebin generates is left on the kill
;;; ring and the paste buffer.


;;; Code:

(defgroup pastebin nil
  "Pastebin -- pastebin.com client"
  :tag "Pastebin"
  :group 'tools)

(defcustom pastebin-unique-developer-api-key "INSERT_YOURS"
  "Everybody using our API is required to use a valid Developer
 API Key. You automaticly get a key when you become a member of
 Pastebin. http://pastebin.com/api"
  :type 'string
  :group 'pastebin)

(defcustom pastebin-user-name "guest"
  "Username of the user you want to login."
  :type 'string
  :group 'pastebin)

(defcustom pastebin-password "guest"
  "Password of the user you want to login."
  :type 'string
  :group 'pastebin)

(defcustom pastebin-post-request-login-url "http://pastebin.com/api/api_login.php"
  "Login url"
  :type 'string
  :group 'pastebin)

(defcustom pastebin-post-request-paste-url "http://pastebin.com/api/api_post.php"
  "Paste url"
  :type 'string
  :group 'pastebin)

(defcustom pastebin-user-api-key "INSERT_YOURS"
  "Key for pastebin members"
  :type 'string
  :group 'pastebin)

(defun pastebin-login ()
  (let* ((params (concat "api_dev_key=" pastebin-unique-developer-api-key
                         "&api_user_name=" (url-hexify-string pastebin-user-name)
                         "&api_user_password=" (url-hexify-string pastebin-password)))
         (url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/x-www-form-urlencoded")))
         (url-request-data params)
         (content-buf (url-retrieve
                       pastebin-post-request-login-url
                       (lambda (arg)
                         (cond
                          ((equal :error (car arg))
                           (signal 'pastebin-error (cdr arg)))
                          (t
                           (re-search-forward "\n\n")
                           (setq pastebin-user-api-key (buffer-substring (point) (point-max)))))))))))


(defcustom pastebin-type-assoc
  '((actionscript-mode . " actionscript")
    (ada-mode . "ada")
    (asm-mode . "asm")
    (autoconf-mode . "bash")
    (bibtex-mode . "bibtex")
    (cmake-mode . "cmake")
    (c-mode . "c")
    (c++-mode . "cpp")
    (cobol-mode . "cobol")
    (conf-colon-mode . "properties")
    (conf-javaprop-mode . "properties")
    (conf-mode . "ini")
    (conf-space-mode . "properties")
    (conf-unix-mode . "ini")
    (conf-windows-mode . "ini")
    (cperl-mode . "perl")
    (csharp-mode . "csharp")
    (css-mode . "css")
    (delphi-mode . "delphi")
    (diff-mode . "dff")
    (ebuild-mode . "bash")
    (eiffel-mode . "eiffel")
    (emacs-lisp-mode . "lisp")
    (erlang-mode . "erlang")
    (erlang-shell-mode . "erlang")
    (espresso-mode . "javascript")
    (fortran-mode . "fortran")
    (glsl-mode . "glsl")
    (gnuplot-mode . "gnuplot")
    (graphviz-dot-mode . "dot")
    (haskell-mode . "haskell")
    (html-mode . "html4strict")
    (idl-mode . "idl")
    (inferior-haskell-mode . "haskell")
    (inferior-octave-mode . "octave")
    (inferior-python-mode . "python")
    (inferior-ruby-mode . "ruby")
    (java-mode . "java")
    (js2-mode . "javascript")
    (jython-mode . "python")
    (latex-mode . "latex")
    (lisp-mode . "lisp")
    (lua-mode . "lua")
    (makefile-mode . "make")
    (makefile-automake-mode . "make")
    (makefile-gmake-mode . "make")
    (makefile-makepp-mode . "make")
    (makefile-bsdmake-mode . "make")
    (makefile-imake-mode . "make")
    (matlab-mode . "matlab")
    (nxml-mode . "xml")
    (oberon-mode . "oberon2")
    (objc-mode . "objc")
    (ocaml-mode . "ocaml")
    (octave-mode . "matlab")
    (pascal-mode . "pascal")
    (perl-mode . "perl")
    (php-mode . "php")
    (plsql-mode . "plsql")
    (po-mode . "gettext")
    (prolog-mode . "prolog")
    (python-2-mode . "python")
    (python-3-mode . "python")
    (python-basic-mode . "python")
    (python-mode . "python")
    (ruby-mode . "ruby")
    (scheme-mode . "lisp")
    (shell-mode . "bash")
    (sh-mode . "bash")
    (smalltalk-mode . "smalltalk")
    (sql-mode . "sql")
    (tcl-mode . "tcl")
    (visual-basic-mode . "vb")
    (xml-mode . "xml")
    (yaml-mode . "properties")
    (text-mode . "text"))
  "Alist composed of major-mode names and corresponding pastebin highlight formats."
  :type '(alist :key-type symbol :value-tupe string)
  :group 'pastebin)

(defcustom pastebin-type-assoc
  '((never . "N") ;;    N = Never
    (ten-minutes . "10M")             ;; 10M = 10 Minutes
    (one-hour . "1H")  ;; 1H = 1 Hour
    (one-day . "1D")     ;; 1D = 1 Day
    (one-month . "1M"))     ;; 1M = 1 Month 
    "We have 5 valid values available which you can use with the 'api_paste_expire_date' parameter."
    :type '(alist :key-type symbol :value-tupe string)
    :group 'pastebin)

(defvar pastebin-domain-history '())

(defun pastebin (start end &optional name)
  "Send the region to the pastebin service specified.

See pastebin.com for more information about pastebin.

Called interactively pastebin uses the current region for
preference for sending... if the mark is NOT set then the entire
buffer is sent.

Argument START is the start of region.
Argument END is the end of region.

If domain is used pastebin prompts for a domain defaulting to
'pastebin-default-domain' so you can send requests or use a
different domain.
"
  (interactive 
   (if (region-active-p)
       (list (region-beginning) (region-end) nil)
     (list (point-min) (point-max) nil)))
  ;; Main function
  (print (buffer-substring-no-properties start end))
  (let* ((data (buffer-substring-no-properties start end))
         (params (concat "api_dev_key=" pastebin-unique-developer-api-key
                         "&api_user_key=" pastebin-user-api-key
                         "&api_option=" "paste"
                         "&api_paste_code=" (url-hexify-string data)
                         "&api_paste_format=" (or
                                               (assoc-default major-mode pastebin-type-assoc nil "text")
                                               "text")
                         (if name
                             (concat "&api_paste_name=" (url-hexify-string name))
                           (concat "&api_paste_name=" (url-hexify-string (buffer-name))))
                         ))
         (url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/x-www-form-urlencoded")))
         (url-request-data
          params)
         (content-buf (url-retrieve 
                       pastebin-post-request-paste-url
                       (lambda (arg)
                         (cond
                          ((equal :error (car arg))
                           (signal 'pastebin-error (cdr arg)))
                          (t
                           (re-search-forward "\n\n")
                           (clipboard-kill-ring-save (point) (point-max))
                           (message "Pastebin URL: %s" (buffer-substring (point) (point-max)))))))))))

(provide 'pastebin)
;;; pastebin.el ends here
