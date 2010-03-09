;;; pastebin.el --- A simple interface to the www.pastebin.com webservice

;;; Copyright (C) 2008 by Tapsell-Ferrier Limited
;;; Copyright (C) 2010 by Ivan Korotkov <twee@tweedle-dee.org>

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
;;; Load this file and run:
;;;
;;;   M-x pastebin-buffer
;;;
;;; to send the whole buffer or select a region and run
;;;
;;;  M-x pastebin
;;;
;;; to send just the region.
;;;
;;; In either case the url that pastebin generates is left on the kill
;;; ring and the paste buffer.


;;; Code:

;;;###autoload
(defgroup pastebin nil
  "Pastebin -- pastebin.com client"
  :tag "Pastebin"
  :group 'tools)

(defcustom pastebin-default-subdomain ""
  "Pastebin subdomain to use by default"
  :type 'string
  :group 'pastebin)

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
    (diff-mode . "diff")
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
    (yaml-mode . "properties"))
  "Alist composed of major-mode names and corresponding pastebin highlight formats."
  :type '(alist :key-type symbol :value-tupe string)
  :group 'pastebin)

(defvar pastebin-subdomain-history '())

;;;###autoload
(defun pastebin-buffer (&optional subdomain)
  "Send the whole buffer to pastebin.com.
Optional argument subdomain will request the virtual host to use,
 eg:'emacs' for 'emacs.pastebin.com'."
  (interactive
   (let ((pastebin-subdomain
      (if current-prefix-arg
          (read-string "pastebin subdomain:" nil 'pastebin-subdomain-history) pastebin-default-subdomain)))
     (list pastebin-subdomain)))
  (pastebin (point-min) (point-max) subdomain))

;;;###autoload
(defun pastebin (start end &optional subdomain)
  "An interface to the pastebin code snippet www service.

See pastebin.com for more information about pastebin.

Called interactively pastebin uses the current region for
preference for sending... if the mark is NOT set then the entire
buffer is sent.

Argument START is the start of region.
Argument END is the end of region.

If subdomain is used pastebin prompts for a subdomain to be used as the
virtual host to use.  For example use 'emacs' for 'emacs.pastebin.com'."
  (interactive
   (let ((pastebin-subdomain
          (if current-prefix-arg
              (read-string "pastebin subdomain:" nil 'pastebin-subdomain-history) pastebin-default-subdomain)))
     (if (mark)
         (list (region-beginning) (region-end) pastebin-subdomain)
       (list (point-min) (point-max) pastebin-subdomain))))
  ;; Main function
  (let* ((data (buffer-substring-no-properties start end))
         (pastebin-url "http://pastebin.com/api_public.php")
         (url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/x-www-form-urlencoded")))
         (url-request-data

          (concat (format "submit=submit&paste_private=0&paste_expire_date=N&paste_subdomain=%s&paste_format=%s&paste_name=%s&paste_code=%s"
              subdomain
                          (or (assoc-default major-mode pastebin-type-assoc) "text")
                          (url-hexify-string (user-full-name))
                          (url-hexify-string data))))
         (content-buf (url-retrieve pastebin-url
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
