;;; novice+.el --- Extensions to `novice.el'.
;;
;; Filename: novice+.el
;; Description: Extensions to `novice.el'.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 1996-2012, Drew Adams, all rights reserved.
;; Created: Thu Jul 11 17:10:39 1996
;; Version: 21.0
;; Last-Updated: Sun Jan  1 14:05:14 2012 (-0800)
;;           By: dradams
;;     Update #: 140
;; URL: http://www.emacswiki.org/cgi-bin/wiki/novice+.el
;; Keywords: internal, help
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x
;;
;; Features that might be required by this library:
;;
;;   `advice', `advice-preload', `apropos', `apropos+',
;;   `apropos-fn+var', `avoid', `backquote', `bookmark', `bookmark+',
;;   `bookmark+-1', `bookmark+-bmu', `bookmark+-key',
;;   `bookmark+-lit', `bookmark+-mac', `bytecomp', `cl', `cus-edit',
;;   `cus-face', `cus-load', `cus-start', `custom', `dired',
;;   `dired+', `dired-aux', `dired-x', `doremi', `easymenu',
;;   `ediff-diff', `ediff-help', `ediff-init', `ediff-merg',
;;   `ediff-mult', `ediff-util', `ediff-wind', `el-swank-fuzzy',
;;   `ffap', `ffap-', `fit-frame', `frame-cmds', `frame-fns',
;;   `fuzzy', `fuzzy-match', `help+20', `hexrgb', `icicles',
;;   `icicles-cmd1', `icicles-cmd2', `icicles-face', `icicles-fn',
;;   `icicles-mcmd', `icicles-mode', `icicles-opt', `icicles-var',
;;   `image-dired', `info', `info+', `kmacro', `levenshtein',
;;   `menu-bar', `menu-bar+', `misc-cmds', `misc-fns', `mkhtml',
;;   `mkhtml-htmlize', `mouse3', `mwheel', `naked', `novice',
;;   `novice+', `pp', `pp+', `regexp-opt', `ring', `ring+',
;;   `second-sel', `strings', `thingatpt', `thingatpt+', `unaccent',
;;   `w32-browser', `w32browser-dlgopen', `wid-edit', `wid-edit+',
;;   `widget'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Extensions to `novice.el'.
;;
;;
;;  ***** NOTE: The following functions defined in `novice.el' have
;;              been REDEFINED HERE:
;;
;;  `disable-command', `enable-command' -
;;     These now use `completing-read' in the interactive spec, with,
;;     as default, `symbol-nearest-point'.
;;
;;
;;  This file should be loaded after loading the standard GNU file
;;  `novice.el'.  So, in your `~/.emacs' file, do this:
;;  (eval-after-load "novice" '(require 'novice+))
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2010/01/12 dadams
;;     (enable|disable)-command: save-excursion + set-buffer -> with-current-buffer.
;; 2005/10/31 dadams
;;     Use nil as init-value arg in calls to completing-read, everywhere.
;; 2004/09/21 dadams
;;     enable-command, disable-command: created Emacs 21 version
;; 1999/03/17 dadams
;;     Protect with fboundp.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(and (< emacs-major-version 20) (eval-when-compile (require 'cl))) ;; when, unless

;; Cannot do (require 'novice) prior to version 20, because `novice.el'
;; does no `provide'.  Don't want to do a (load-library "novice") either,
;; for prior versions, because it wouldn't allow doing
;; (eval-after-load "novice" '(progn (require 'novice+)))
(when (>= emacs-major-version 20) (require 'novice))

(require 'icicles nil t) ;; (no error if not found): completing-read
(require 'thingatpt nil t) ;; (no error if not found): symbol-at-point
(require 'thingatpt+ nil t) ;; (no error if not found): symbol-nearest-point

;;; Variable USER-INIT-FILE is free here (defined in `files.el').

;;;;;;;;;;;;;;;;;;;;;;;;



;; REPLACE ORIGINAL in `novice.el':
;; Uses `completing-read' in interactive spec, with `symbol-nearest-point'.
;; `symbol-nearest-point' is defined in `thingatpt+.el'.
;; `symbol-at-point' is defined in `thingatpt.el'.
(if (< emacs-major-version 21)
    (defun enable-command (command)
      "Allow COMMAND to be invoked without confirmation from now on.
Your `~/.emacs' file is altered so that this applies also to future
sessions.  See command `\\[disable-command]'."
      (interactive
       (let ((symb (cond ((fboundp 'symbol-nearest-point) (symbol-nearest-point))
                         ((fboundp 'symbol-at-point) (symbol-at-point))
                         (t nil)))
             (enable-recursive-minibuffers t))
         (list (intern (completing-read "Enable command: " obarray 'commandp nil
                                        nil nil (symbol-name symb) t)))))
      (put command 'disabled nil)
      (with-current-buffer (find-file-noselect (substitute-in-file-name user-init-file))
        (goto-char (point-min))
        (when (search-forward (concat "(put '" (symbol-name command) " ") nil t)
          (delete-region (progn (beginning-of-line) (point))
                         (progn (forward-line 1) (point))))
        ;; Explicitly enable, in case this command is disabled by default
        ;; or in case the code we deleted was actually a comment.
        (goto-char (point-max))
        (insert "\n(put '" (symbol-name command) " 'disabled nil)\n")
        (save-buffer)))
  (defun enable-command (command)
    "Allow COMMAND to be executed without special confirmation from now on.
The user's .emacs file is altered so that this will apply
to future sessions."
    (interactive
     (let ((symb (cond ((fboundp 'symbol-nearest-point) (symbol-nearest-point))
                       ((fboundp 'symbol-at-point) (symbol-at-point))
                       (t nil)))
           (enable-recursive-minibuffers t))
       (list (intern (completing-read "Enable command: " obarray 'commandp nil
                                      nil nil (symbol-name symb) t)))))
    (put command 'disabled nil)
    (let ((init-file user-init-file)
          (default-init-file
              (if (eq system-type 'ms-dos) "~/_emacs" "~/.emacs")))
      (when (null init-file)
        (if (or (file-exists-p default-init-file)
                (and (eq system-type 'windows-nt)
                     (file-exists-p "~/_emacs")))
            ;; Started with -q, i.e., the file containing
            ;; enabled/disabled commands hasn't been read.  Saving
            ;; settings there would overwrite other settings.
            (error "Saving settings from \"emacs -q\" would overwrite existing customizations"))
        (setq init-file default-init-file)
        (if (and (not (file-exists-p init-file))
                 (eq system-type 'windows-nt)
                 (file-exists-p "~/_emacs"))
            (setq init-file "~/_emacs")))
      (with-current-buffer (find-file-noselect (substitute-in-file-name init-file))
        (goto-char (point-min))
        (if (search-forward (concat "(put '" (symbol-name command) " ") nil t)
            (delete-region (progn (beginning-of-line) (point))
                           (progn (forward-line 1) (point))))
        ;; Explicitly enable, in case this command is disabled by default
        ;; or in case the code we deleted was actually a comment.
        (goto-char (point-max))
        (insert "\n(put '" (symbol-name command) " 'disabled nil)\n")
        (save-buffer)))))



;; REPLACE ORIGINAL in `novice.el':
;; Uses `completing-read' in interactive spec, with `symbol-nearest-point'.
;; `symbol-nearest-point' is defined in `thingatpt+.el'.
;; `symbol-at-point' is defined in `thingatpt.el'.
(if (< emacs-major-version 21)
    (defun disable-command (command)
      "Require confirmation to execute COMMAND from now on.
Your `~/.emacs' file is altered so that this applies also to future
sessions.  See command `\\[enable-command]'."
      (interactive
       (let ((symb (cond ((fboundp 'symbol-nearest-point) (symbol-nearest-point))
                         ((fboundp 'symbol-at-point) (symbol-at-point))
                         (t nil)))
             (enable-recursive-minibuffers t))
         (list (intern (completing-read "Disable command: " obarray 'commandp nil
                                        nil nil (symbol-name symb) t)))))
      (unless (commandp command) (error "Invalid command name `%s'" command))
      (put command 'disabled t)
      (with-current-buffer (find-file-noselect (substitute-in-file-name user-init-file))
        (goto-char (point-min))
        (if (search-forward (concat "(put '" (symbol-name command) " ") nil t)
            (delete-region (progn (beginning-of-line) (point))
                           (progn (forward-line 1) (point))))
        (goto-char (point-max))
        (insert "\n(put '" (symbol-name command) " 'disabled t)\n")
        (save-buffer)))
  (defun disable-command (command)
    "Require special confirmation to execute COMMAND from now on.
The user's .emacs file is altered so that this will apply
to future sessions."
    (interactive
     (let ((symb (cond ((fboundp 'symbol-nearest-point) (symbol-nearest-point))
                       ((fboundp 'symbol-at-point) (symbol-at-point))
                       (t nil)))
           (enable-recursive-minibuffers t))
       (list (intern (completing-read "Disable command: " obarray 'commandp nil
                                      nil nil (symbol-name symb) t)))))
    (if (not (commandp command))
        (error "Invalid command name `%s'" command))
    (put command 'disabled t)
    (let ((init-file user-init-file)
          (default-init-file
              (if (eq system-type 'ms-dos) "~/_emacs" "~/.emacs")))
      (when (null init-file)
        (if (or (file-exists-p default-init-file)
                (and (eq system-type 'windows-nt)
                     (file-exists-p "~/_emacs")))
            ;; Started with -q, i.e., the file containing
            ;; enabled/disabled commands hasn't been read.  Saving
            ;; settings there would overwrite other settings.
            (error "Saving settings from \"emacs -q\" would overwrite existing customizations"))
        (setq init-file default-init-file)
        (if (and (not (file-exists-p init-file))
                 (eq system-type 'windows-nt)
                 (file-exists-p "~/_emacs"))
            (setq init-file "~/_emacs")))
      (with-current-buffer (find-file-noselect (substitute-in-file-name init-file))
        (goto-char (point-min))
        (if (search-forward (concat "(put '" (symbol-name command) " ") nil t)
            (delete-region (progn (beginning-of-line) (point))
                           (progn (forward-line 1) (point))))
        (goto-char (point-max))
        (insert "\n(put '" (symbol-name command) " 'disabled t)\n")
        (save-buffer)))))

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'novice+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; novice+.el ends here
