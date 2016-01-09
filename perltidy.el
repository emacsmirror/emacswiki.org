;;; perltidy.el --- Tidy perl code

;; Copyright (C) 2007-2015 Free Software Foundation, Inc.
;;
;; Author: Ye Wenbin <wenbinye@gmail.com>
;; Maintainer: Kirill Babikhin <mrakobes86reg@yandex.ru>
;; Created: 22 Dec 2007
;; Version: 0.05
;; Keywords: tools, convenience, languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; As the PBP(Perl Best Practice) suggest, put this to your ~/.perltidyrc:
;; ## .perltidyrc --- configuration for perltidy
;; # Max line width is 78 cols
;; -l=78
;; # Indent level is 4 cols
;; -i=4
;; # Continuation indent is 4 cols
;; -ci=4
;; # Output to STDOUT
;; -st
;; # Errors to STDERR
;; -se
;; # Maximal vertical tightness
;; -vt=2
;; # No extra indentation for closing brackets
;; -cti=0
;; # Medium parenthesis tightness
;; -pt=1
;; # Medium brace tightness
;; -bt=1
;; # Medium square bracket tightness
;; -sbt=1
;; # Medium block brace tightness
;; -bbt=1
;; # No space before semicolons
;; -nsfs
;; # Don't outdent long quoted strings
;; -nolq
;; # Break before all operators
;; -wbb="% + - * / x != == >= <= =~ !~ < > | & >= < = **= += *= &= <<= &&= -= /= |= >>= ||= .= %= ^= x="

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'perltidy)

;;; Code:

(eval-when-compile
  (require 'cl))

(defgroup perltidy nil
  "Tidy perl code using perltidy"
  :group 'tools
  :group 'pde)

(defcustom perltidy-program "perltidy"
  "*Program name of perltidy"
  :type 'string
  :group 'perltidy)

(defcustom perltidy-program-params
  '(;; I/O control
    "--standard-output"
    "--standard-error-output"
    "--force-read-binary"
    "--quiet"

    ;; FORMATTING OPTIONS
    "--no-check-syntax"
    )
  "*perltidy run options"
  :type 'list
  :group 'perltidy)

(defcustom perltidy-rcregex "\\.perltidyrc"
  "perltidyrc file regex"
  :type 'string
  :group 'perltidy)

(defmacro perltidy-save-point (&rest body)
  (declare (indent 0) (debug t))
  `(let ((old-point (point)))
     ,@body
     (goto-char old-point)))

;;;###autoload
(defun perltidy-region (beg end)
  "Tidy perl code in the region."
  (interactive "r")
  (or (get 'perltidy-program 'has-perltidy)
      (if (executable-find perltidy-program)
          (put 'perltidy-program 'has-perltidy t)
        (error "Seem perltidy is not installed")))
  (perltidy-save-point

    (let ((old-perltidy-env (getenv "PERLTIDY"))
          (remote? (tramp-tramp-file-p buffer-file-name))
          (perltidyrc (perltidy-find-perltidyrc buffer-file-truename))
          (pertidyrc-remote (expand-file-name "perltidyrc-remote" temporary-file-directory))
          (perltidy-run-list perltidy-program-params)
          )

      (if (and (bound-and-true-p remote?)
               perltidyrc)
          (progn
            (require 'tramp-sh)
            (tramp-sh-handle-copy-file perltidyrc pertidyrc-remote t)
            (setq perltidyrc pertidyrc-remote)
            (setq perltidy-run-list
                  (append perltidy-run-list
                          (list (concat "-pro=" pertidyrc-remote))))))

      (apply #'call-process-region
             (append (list beg end perltidy-program
                           t
                           t
                           t
                           )
                     perltidy-run-list)))
    t))

;;;###autoload
(defun perltidy-buffer ()
  "Call perltidy for whole buffer."
  (interactive)
  (perltidy-region (point-min) (point-max)))

;;;###autoload
(defun perltidy-subroutine ()
  "Call perltidy for subroutine at point."
  (interactive)

  (save-excursion
    (let ((current-point (point))
          b e)
      (setq b (progn (beginning-of-defun) (point)))
      (when (and
             (looking-at "\\s-*sub\\s-+")
             (< b current-point)
             (> (save-excursion
                  (setq e (progn (end-of-defun) (point))))
                current-point))
        (perltidy-region b e)))))

;;;###autoload
(defun perltidy-dwim-safe (arg)
  "Perltidy Do What I Mean safe.
If region is active call perltidy on the region.
If inside subroutine, call perltidy on the subroutine,
otherwise stop."
  (interactive "P")
  (let ((buf (current-buffer))
        beg end)
    (cond ((and mark-active transient-mark-mode)
           (setq beg (region-beginning)
                 end (region-end)))
          ((save-excursion
             (let ((current-point (point))
                   b e)
               (setq b (progn (beginning-of-defun) (point)))
               (when (and
                      (looking-at "\\s-*sub\\s-+")
                      (< b current-point)
                      (> (save-excursion
                           (setq e (progn (end-of-defun) (point))))
                         current-point))
                 (setq beg b
                       end e)))))
          (t (setq beg nil
                   end nil)))
    (if (and beg
             end)
        (progn
          (perltidy-region beg end)
          (font-lock-fontify-buffer)))))

;;;###autoload
(defun perltidy-dwim (arg)
  "Perltidy Do What I Mean.
If region is active call perltidy on the region.
If inside subroutine, call perltidy on the subroutine,
otherwise call perltidy for whole buffer."
  (interactive "P")
  (let ((buf (current-buffer))
        beg end)
    (cond ((and mark-active transient-mark-mode)
           (setq beg (region-beginning)
                 end (region-end)))
          ((save-excursion
             (let ((current-point (point))
                   b e)
               (setq b (progn (beginning-of-defun) (point)))
               (when (and
                      (looking-at "\\s-*sub\\s-+")
                      (< b current-point)
                      (> (save-excursion
                           (setq e (progn (end-of-defun) (point))))
                         current-point))
                 (setq beg b
                       end e)))))
          (t (setq beg (point-min)
                   end (point-max))))
    (perltidy-region beg end)
    (font-lock-fontify-buffer)))

(defun perltidy-find-perltidyrc (&optional dir rcregex)
  (unless dir (setq dir (buffer-file-name)))
  (unless rcregex (setq rcregex perltidy-rcregex))
  (setq dir (file-name-directory dir))

  (let (rcfile)
    (catch 'my-tag
      (locate-dominating-file
       dir
       (lambda (parent)
         (let ((rc (car (ignore-errors (directory-files parent t rcregex))))
               (pparent (file-name-directory (directory-file-name parent))))
           (setq rcfile rc)
           (cond ((equal parent
                         pparent)
                  (if (= (length rc) 0)
                      (throw 'my-tag rc)
                    (throw 'my-tag nil)))

                 ((and (= (length rc) 0)
                       (file-exists-p    (expand-file-name "lib" pparent))
                       (file-directory-p (expand-file-name "lib" pparent)))
                  (setq rcfile (car (ignore-errors (directory-files pparent t rcregex))))
                  (throw 'my-tag rcfile))
                 (t rc))))))
    rcfile))

(provide 'perltidy)
;;; perltidy.el ends here
