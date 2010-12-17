;;; xlicense.el --- Insert a pre-defined license text

;; Copyright (C) 2010  Seong-Kook Shin

;; Author: Seong-Kook Shin <cinsky@gmail.com>
;; Keywords: abbrev, convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; $Id: xlicense.el,v 1.2 2010/12/16 12:09:41 cinsk Exp $

;;; Code:

(require 'skeleton)
;;(eval-when-compile (require 'cl))

(defvar license-directory (concat (file-name-as-directory user-emacs-directory)
                                  "license")
  "Directory for license templates")

(defvar license-types '((gpl . "GPL-2.0")
                        (gpl2 . "GPL-2.0")
                        (gpl3 . "GPL-3.0")
                        (lgpl . "LGPL")
                        (apache . "APACHE-2.0")
                        (boost . "BOOST")
                        (bsd-old . "BSD-old")
                        (bsd-new . "BSD-new")
                        (bsd . "BSD-new")
                        (freebsd . "FREEBSD")
                        (mit . "MIT")
                        ;(tmp . "TMP")
                        )
  "Alist of licenses.  CAR of each item is a symbol represents the license,
CDR of each item is a filename of the license template")

(defvar license-default-summary
  "<one line to give the program's name and a brief idea of what it does.>"
  "Short description of what it does.")

(defvar license-eol-text "!@#$EOL!@#$"
  "Text to mark blank lines -- used internally")

(defun license-file (type)
  "Return the pathname of the given license file"
  (let ((tp (assoc type license-types)))
    (if tp
        (concat (file-name-as-directory license-directory) (cdr tp))
      tp)))

(defvar license-keywords-alist '(("@author@" . user-full-name)
                                 ("@email@" . "hello")
                                 ("@year@" . (lambda ()
                                               (substring (current-time-string)
                                                          -4)))
                                 ("@organization@" . 
                                  (lambda ()
                                    (getenv "ORGANIZATION"))))
  "Keywords that need to be substituted by `license-substitute-keywords'.

The CAR of an item is a keyword and CDR is a replacement.  If the
CDR of an item is a function, the return value(string) is used as
a replacement.  If the returned value is nil, no substitution for
that keyword.")


(defun license-substitute-keywords (&optional record)
  "Substitute all occurences of keywords to their replacement and returns 
the replacement positions in markers."
  (let (markers)
    (dolist (i license-keywords-alist)
      (let ((keyword (regexp-quote (car i)))
            (what (if (functionp (cdr i)) (funcall (cdr i)) (cdr i))))
        (if what
            (progn
              (goto-char (point-min))
              (while (re-search-forward keyword nil t)
                (if record
                    (setq markers (cons (point-marker) markers)))
                (replace-match what))))))
    markers))

(defun license-fill-paragraphs (lst)
  "Fill paragraphs at markers in LST."
  (dolist (i lst)
    (goto-char i)
    (fill-paragraph)))


(defun create-license (type &optional comments summary author)
  "Create a license paragraphs according to current buffer's major mode.

IF COMMENTS is non-nil, comment the license text.
If SUMMARY is non-nil, it is inserted as a header of the comment.
If AUTHOR is non-nil, all occurrence of the author keyword are
replaced to AUTHOR.

See `license-keywords-alist' for keywords and their meaning."
  (let (;(buffer (get-buffer-create "*LICENSE*"))
        (desc (if (and summary (> (length summary) 0))
                  summary
                license-default-summary))
        (auth (or author (user-full-name)))
        (lfile (license-file type))
        (mode major-mode)
        (fill-points nil))
    ;;(save-current-buffer
    (with-temp-buffer
      ;(set-buffer buffer)
      ;(erase-buffer)
      (insert (format "%s\n" desc))
      (insert (format "Copyright (C) %d  %s" (nth 5 (decode-time)) auth))
      (if user-mail-address
          (insert (format " <%s>" user-mail-address)))
      (insert "\n")
      (insert-file-contents lfile)

      (goto-char (point-min))
      (while (re-search-forward "^$" nil t)
        (replace-match license-eol-text))

      (let ((case-fold-search t)
            (markers (license-substitute-keywords t)))
        (funcall mode)
        (if (and comments comment-start)
            (let ((comment-style 'extra-line))
              (comment-region (point-min) (point-max))))

        (goto-char (point-min))

        (let ((re-eol (concat (regexp-quote license-eol-text) "$")))
          (while (re-search-forward re-eol nil t)
            (replace-match "")))

        (license-fill-paragraphs markers)

        ;;(print markers)
        ;;(pop-marker)
        (goto-char (point-max))
        (insert "\n"))
      (buffer-substring-no-properties (point-min) (point-max)))))


(defun insert-license (&optional type)
  "Insert a license template into the current buffer." 
  (interactive)
  (let ((text (create-license
               (or type (intern (completing-read "Choose a license type: "
                                                 license-types nil t)))
               t (read-from-minibuffer "Summary (short description): "))))
    (if (called-interactively-p 'any)
        (insert text)
      text)))


(define-skeleton license-skeleton
  "Insert a license template into the current buffer."
  ""
  (insert-license)
  "\n"
  _)


(define-skeleton gpl-interactive-skeleton
  "Insert an Interactive GPL banner."
  ""
  \n > "static const char *gpl_banner[] = {" \n
  > "\"" (file-name-nondirectory (file-name-sans-extension buffer-file-name))
  > " version XXX, Copyright (C) "
  (substring (current-time-string) -4) " " (user-full-name) "\"," \n
  > "\"" (file-name-nondirectory (file-name-sans-extension buffer-file-name))
  "comes with ABSOLUTELY NO WARRANTY; for details type `show w'.\"," \n
  "\"This is free software, and you are welcome to redistribute it\"," \n
  "\"under certain conditions; type `show c' for details.\"," \n
  > > "};" \n
  > _)


(provide 'xlicense)
;;; license.el ends here
