;;; pastebox.el --- Emacs lisp lib for pasting to PasteBox-powered bins.
;; Copyright (C) 2009  Anthony Garcia

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; pastebox.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with pastebox.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Support for pasting a buffer or region to PasteBox powered sites such
;; as pastebin.ch

;; Keywords: paste,pastebin,pastebox
;; URL: http://www.emacswiki.org/emacs/pastebox.el

;;; Code:

(require 'url)

(defvar pastebox-url "http://pastebin.ch/new"
  "The URL to the pastebin")

(defvar pastebox-user-name (user-login-name)
  "String for the name field, defaults to what `user-login-name' returns.")

(defvar pastebox-subject nil
  "String for the subject field, defaults to what `buffer-name' returns.")

(defvar pastebox-description ""
  "String for the description field, defaults to \"\".")

(defvar pastebox-types
  '((c++-mode . "cpp")
    (css-mode . "css")
    (js-mode . "javascript")
    (java-mode . "java")
    (html-mode . "html4strict")
    (pascal-mode . "pascal")
    (perl-mode . "perl")
    (php-mode . "php")
    (python-mode . "python")
    (xml-mode . "xml")
    (ada-mode . "ada")
    (asm-mode . "asm")
    (sh-mode . "bash")
    (c-mode . "c")
    (cs-mode . "csharp")
    (delphi-mode . "delphi")
    (diff-mode . "diff")
    (fortran-mode . "fortran")
    (idl-mode . "idl")
    (conf-windows-mode . "ini")
    (conf-unix-mode . "ini")
    (latex-mode . "latex")
    (tex-mode . "latex")
    (lisp-mode . "lisp")
    (lua-mode . "lua")
    (sql-mode . "mysql")
    (objc-mode . "objc")
    (ruby-mode . "ruby")
    (scheme-mode . "scheme")
    (tcl-mode . "tcl")
    (vhdl-mode . "vhdl")
    (emacs-lisp-mode . "lisp"))
  "Alist of syntax highlighting types associated with emacs modes")

(defun pastebox-get-captcha ()
  (let ((val nil))
    (set-buffer (url-retrieve-synchronously pastebox-url))
    (if (string-match-p "Press button 2 to submit\\."
                        (buffer-substring (point-min) (point-max)))
        (setq val "Submit 2")
      (setq val "Submit 1"))
    (kill-buffer (current-buffer))
    val))

(defvar pastebox-paste-url-buffer "*PasteBox-Paste*"
  "Name of buffer created to insert the URL to the paste.")
(defvar pastebox-be-annoying nil
  "If t, switch to a buffer with the URL to the paste in addition to saving it to the kill ring. Defaults to nil")

(defun pastebox-region (start end)
  "Paste a region"
  (interactive "r")
  (let ((the-region (if (use-region-p) (cons (region-beginning) (region-end))
                      (cons start end))))
    (let (
          (url-request-method "POST")
          (url-request-extra-headers
           '(("Content-Type" . "application/x-www-form-urlencoded")))
          (url-request-data
           (format "subject=%s&name=%s&type=%s&description=%s&content=%s&submit=%s"
                   (url-hexify-string (or pastebox-subject (buffer-name)))
                   (url-hexify-string pastebox-user-name)
                   (url-hexify-string
                    (assoc-default major-mode pastebox-types nil "plain"))
                   (url-hexify-string pastebox-description)
                   (url-hexify-string
                    (buffer-substring-no-properties (car the-region) (cdr the-region)))
                   (url-hexify-string
                    (pastebox-get-captcha)))))
      (set-buffer (url-retrieve pastebox-url
                                '(lambda (status)
                                   (kill-buffer (current-buffer))
                                   (let ((buf (get-buffer-create pastebox-paste-url-buffer)))
                                     (if pastebox-be-annoying
                                         (switch-to-buffer buf)
                                       (set-buffer buf)))
                                   (erase-buffer)
                                   (insert (cadr status))
                                   (goto-char (point-min))
                                   (copy-region-as-kill (point-min) (point-max))))))))

(defun pastebox-buffer ()
  "Paste a buffer"
  (interactive)
  (pastebox-region (point-min) (point-max)))

(provide 'pastebox)

;;; pastebox.el ends here
