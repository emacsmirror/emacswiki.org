;;; handbook.el --- Quick access to the FreeBSD handbook

;; Author: padik@live.com
;; Website: padik.t15.org
;; Version: 1.0.0
;; Keywords: FreeBSD Handbook
;; URL: http://www.emacswiki.org/elisp/handbook.el

;; This file is not part of GNU Emacs.

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
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Makes for quick access to the FreeBSD Handbook of The FreeBSD Documentation Project

;;; Requirements:

;; /usr/ports/www/emacs-w3m: text-based web browser for Emacs

;;; Acknowledgement:

;; This code is derived from haskell-ref.el by Mark Triggs <mst@dishevelled.net>

;; Use M-x handbook-mode RET C-ch index RET

;;; Code:

(defvar handbook-mode-map nil)

(defvar *handbook-reference-location*
        "file://usr/share/doc/en_US.ISO8859-1/books/handbook")

(defvar *handbook-reference*
        '(("legal" . "LEGALNOTICE.html")
       ("acpi" . "acpi-overview.html")
       ("swap" . "adding-swap-space.html")
       ("anoncvs" . "anoncvs.html")
       ("appendices" . "appendices.html")
       ("backup" . "backup-basics.html")
       ("basics" . "basics.html")
       ("bibliography" . "bibliography.html")
       ("binary-formats" . "binary-formats.html")
       ("preface" . "book-preface.html")
       ("book" . "book.html")
       ("boot" . "boot.html")
       ("colophon" . "colophon.html")
       ("config-network-setup" . "config-network-setup.html")
       ("config-tuning" . "config-tuning.html")
       ("consoles" . "consoles.html")
       ("creating-cds" . "creating-cds.html")
       ("crypt" . "crypt.html")
       ("ctm" . "ctm.html")
       ("current-stable" . "current-stable.html")
       ("cutting-edge" . "cutting-edge.html")
       ("cvs-tags" . "cvs-tags.html")
       ("cvsup" . "cvsup.html")
       ("desktop" . "desktop.html")
       ("device-hints" . "device-hints.html")
       ("dialout" . "dialout.html")
       ("dialup" . "dialup.html")
       ;; not finished.     
       ("index" . "index.html")
       ("install" . "install.html")
       ("x11-wm" . "x11-wm.html")
       ("x11" . "x11.html")))

(defun handbook-reference-lookup ()
        "Look up a topic in the FreeBSD Handbook"
        (interactive)
        (let* ((table (append *handbook-reference* ))
                        (symbol (completing-read "Topic: " table nil t
                                (let ((word (or (thing-at-point 'word)
                                                (thing-at-point 'sexp))))
                                                        (if (assoc word table) word nil)))))
        (funcall (if (fboundp 'w3m) 'w3m 'browse-url)
                (format "%s/%s" *handbook-reference-location*
                                (cdr (assoc symbol table))))))

(if handbook-mode-map()
        (setq handbook-mode-map (make-sparse-keymap))
                (define-key handbook-mode-map "\C-ch" 'handbook-reference-lookup))
          
(defun handbook-mode()
        (interactive)
        (use-local-map handbook-mode-map)
        (setq mode-name "The FreeBSD Documentation Project"))

(provide 'handbook)

;;; handbook.el ends here
