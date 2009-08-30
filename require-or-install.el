;;; require-or-install.el --- Download and automagically install Emacs Lisp

;; Copyright (C) 2007  Olivier Ramonat

;; Author: Olivier Ramonat <olivier@ramonat.fr>
;; Created: 8 Aug 2007

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
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

;; Use (require-or-install 'my-package) to automatically download and install
;; my-package.el if require fails.
;;
;; require-or-install takes two others optionals arguments :
;;    - PAGENAME (the EmacsWiki PAGENAME where to download my-package)
;;    - URL (the download URL)
;;
;; To install, to add to .emacs:
;;    (require 'require-or-install)

;;; Code:

(eval-when-compile (require 'install-elisp))

(defgroup require-or-install nil
  "Download and automagically install lib"
  :group 'hypermedia
  :prefix "require-or-install-"
)

(defcustom require-or-install-confirm-flag t
  "Non-nil means do download confirmation.
Note that install-elisp.el ask for install confirmation.
See"
  :type 'boolean
  :require 'install-elisp
  :group 'require-or-install)

(defcustom require-or-install-is-enabled t
  "If nil, disable installation.  Do require only."
  :type 'boolean
  :require 'install-elisp
  :group 'require-or-install)

(defvar require-or-install-map
  (let ((map (make-sparse-keymap)))
    (define-key map " " 'act)
    (define-key map "\d" 'skip)
    (define-key map [delete] 'skip)
    (define-key map [backspace] 'skip)
    (define-key map "y" 'act)
    (define-key map "n" 'skip)
    (define-key map "Y" 'act)
    (define-key map "N" 'skip)
    (define-key map "!" 'automatic)
    (define-key map "\C-h" 'help)
    (define-key map [f1] 'help)
    (define-key map [help] 'help)
    (define-key map "?" 'help)
    (define-key map "\C-g" 'quit)
    (define-key map "\C-]" 'quit)
    (define-key map "q" 'exit)
    (define-key map "\r" 'exit)
    (define-key map [return] 'exit)
    map)
  "Key map that defines the responses to questions in `require-or-install'.
The valid answers include `act', `skip', `automatic', `exit' and `help'.")

(defconst require-or-install-help
  "Type Space or `y' to install the library, Delete or `n' to skip to next,
RET or `q' to exit, ! to install all remaining libraries with no
more questions."
  "Help message while in `require-or-install'.")


(defun require-or-install-ask (library)
  "Ask for confirmation to install LIBRARY."
  (let (key def done do-install)
    (message (format "Install Emacs Lisp code %s? " library))
    (while (not done)
      (setq key (read-event))
      (setq key (vector key))
      (setq def (lookup-key require-or-install-map key))
      (cond ((eq def 'help)
             (with-output-to-temp-buffer "*Help*"
               (princ
                (concat "Require Or Install.\n\n"
                        (substitute-command-keys
                         require-or-install-help)))
               (with-current-buffer standard-output
                 (help-mode))))
            ((eq def 'exit)
             (setq done t)
             (setq require-or-install-is-enabled nil))
            ((eq def 'skip)
             (setq done t))
            ((eq def 'automatic)
             (setq do-install t)
             (setq done t)
             (setq require-or-install-confirm-flag nil))
            ((eq def 'act)
             (setq do-install t)
             (setq done t))))
    do-install))

(defun require-or-install (feature &optional pagename &optional url)
  "Try to require FEATURE.  Ask for install if `require' fails.
PAGENAME is the name of the EmacsWiki page when FEATURE can be downloaded
If PAGENAME is nil, FEATURE.el is used.
When URL is not null then install from URL."
  (if (locate-library (format "%s" feature))
      (require feature)
    (when require-or-install-is-enabled
      (setq install-elisp-simple-confirm t)
      (if url
          (when (or (not require-or-install-confirm-flag)
                    (require-or-install-ask url))
            (install-elisp url))
        (let ((emacswiki-pagename
               ((lambda (a &optional b)
                  (if b b (format "%s.el" a)))
                feature pagename)))
          (when (or (not require-or-install-confirm-flag)
                    (require-or-install-ask emacswiki-pagename))
            (install-elisp-from-emacswiki emacswiki-pagename))))
      (setq install-elisp-simple-confirm nil))))

(defun require-or-install-add-font-lock ()
  "Add keyword face for `require-or-install'."
  (font-lock-add-keywords 'emacs-lisp-mode
                          '(("require-or-install" . font-lock-keyword-face))))

(provide 'require-or-install)

;;; require-or-install.el ends here
