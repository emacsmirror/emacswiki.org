;;; backport-package.el --- lift package.el from emacs 24 for use in emacs 23

;; Copyright © 2011 Richard Kim
;; Author: Richard Y. Kim, <emacs18@gmail.com>
;; Version: 0.2
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Change Log:
;;
;; 2011/12/31 version 0.1 - initial release
;; 2011/12/31 version 0.2 - improvments from Michael Heerdegen to use
;;      locate-library function rather than exec-directory to find elisp files
;;      as well as other tweaks.

;;; Commentary:
;;
;; Forthcoming emacs 24 introduces elisp packaging system which is not
;; available in emacs 23.  This file backports package.el from emacs 24 so that
;; one can use it on emacs 23 as well.  This defines backport-package which
;; is intended to be used like this
;;
;;   emacs24 -batch -q -l ./backport-package.el -f backport-package
;;
;; where emacs24 is assumed to be the emacs 24 binary compiled from modern
;; bzr revision such as version 106726 dated Dec 23, 2011.  The net result of
;; this is to copy three emacs-lisp files out of emacs24 to current directory
;; while making tweaks to them.  The resulting files can then be used within
;; emacs23, i.e., packages now can be installed and maintained on emacs23.
;;
;; My intestest is in creating my own ELPA packages which I can then install
;; into both emacs23 and emacs24.  I have been doing this for over a year since
;; end of 2010.  This has allowed me to create and manage add-on elisp packages
;; such as cedet (latest from cedet.sf.net), bbdb, anything, emacs-w3m
;; etc. as well as my own elisp packages.  This has simplified the task of
;; managing and updating all of these packages for both emacs23 and emacs24.
;;
;; At the beginning of 2011, one needed to lift only package.el and
;; help-mode.el.  Since then package.el in emacs24 has been modified to add
;; dependence on tabulated-list.el as well as other dependencies indicated by
;; the code tweaks shown below.  If emacs23 compability is kept in mind as
;; emacs24 progresses, it would be a lot easier to use package.el in emacs23.
;; Till then I guess we need to use ad-hoc methods such as backport-package
;; below to use package.el in emacs23.
;;

(defun backport-package ()
  "Copy package.el and other related files from emacs 24 lisp directory to
current directory while making necessary changes so that the copied files can
be used in emacs 23."
  (let ((dest-directory default-directory)
        text-to-copy
        (make-backup-files nil))

    (auto-compression-mode +1)          ;sources may be .gz like on Debian

    ;; First copy three files to current directory.
    (copy-file (locate-library "package.el") dest-directory
               'ok-if-already-exists)
    (copy-file (locate-library "tabulated-list.el") dest-directory
               'ok-if-already-exists)
    (copy-file (locate-library "help-mode.el") dest-directory
               'ok-if-already-exists)

    ;; Edit package.el by copying one sexp from startup.el.
    ;; package-subdirectory-regexp is defined in startup.el, but used in
    ;; package.el, so simply lift that one sexp out of startup.el and pluck it
    ;; into package.el
    (find-file (locate-library "startup.el"))
    (re-search-forward "^(defconst package-subdirectory-regexp" nil t)
    (beginning-of-line)
    (setq text-to-copy
          (buffer-substring (point) (progn (forward-sexp) (1+ (point)))))
    (kill-buffer)
    ;; Paste the copied text into package.el
    (find-file (locate-library "package.el" nil (list dest-directory)))
    (re-search-forward "^(defgroup package" nil t)
    (beginning-of-line)
    (insert text-to-copy)
    (insert "\n")
    (save-buffer)
    (kill-buffer)

    ;; Copy one sexp from subr.el to be pasted into help-mode.el
    ;; make-composed-keymap is defined in subr.el, but used in help-mode.el, so
    ;; lift that one sexp and add it to help-mode.el.
    (find-file (locate-library "subr.el"))
    (re-search-forward "^(defun make-composed-keymap " nil t)
    (beginning-of-line)
    (setq text-to-copy
          (buffer-substring (point) (progn (forward-sexp) (1+ (point)))))
    (kill-buffer)
    ;; Paste the copied text into help-mode.el.
    (find-file (locate-library "help-mode.el" nil (list dest-directory)))
    (re-search-forward "^(defvar help-mode-map" nil t)
    (beginning-of-line)
    (insert text-to-copy)
    (insert "\n")
    (save-buffer)
    (kill-buffer)

    ;; Edit tabulated-list.el to remove dependence on glyphless-char-display.
    ;; Don't know what this is, so just set
    ;; tabulated-list-glyphless-char-display to nil.  This does not seem to
    ;; cause any problems.  Also define bidi-string-mark-left-to-right to just
    ;; return its argument.
    (find-file (locate-library "tabulated-list.el" nil (list dest-directory)))
    (re-search-forward "^(defvar " nil t)
    (beginning-of-line)
    (insert ";; bidi-string-mark-left-to-right exists only in emacs 24\n")
    (insert "(unless (fboundp 'bidi-string-mark-left-to-right)\n")
    (insert "  (defalias 'bidi-string-mark-left-to-right 'identity))\n\n")
    (re-search-forward "^(defvar tabulated-list-glyphless-char-display" nil t)
    (beginning-of-line)
    (insert ";; Set this to nil, because emacs23 does not define ")
    (insert "glyphless-char-display.\n")
    (insert "(defvar tabulated-list-glyphless-char-display nil)\n'")
    (save-buffer)
    (kill-buffer)))

;; backport-package.el ends here
