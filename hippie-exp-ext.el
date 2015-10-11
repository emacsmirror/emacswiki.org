;;; hippie-exp-ext.el --- Extension of hippie-expand

;; Filename: hippie-exp-ext.el
;; Description: Extension of hippie-expand
;; Author: rubikitch <rubikitch@ruby-lang.org>
;; Maintainer: rubikitch <rubikitch@ruby-lang.org>
;; Copyright (C) 2012, rubikitch, all rights reserved.
;; Time-stamp: <2015-10-11 17:50:45 rubikitch>
;; Created: 2012-09-08 12:56:37
;; Version: 0.1
;;           By: rubikitch
;; URL: http://www.emacswiki.org/emacs/download/hippie-exp-ext.el
;; Keywords: abbrev, convenience, completions, hippie-expand
;; Compatibility: GNU Emacs 24.2.2
;;
;; Features that might be required by this library:
;;
;; `hippie-exp', `cl'
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Extension of hippie-expand.
;;
;; I created this program to let `hippie-expand' expand only unibyte
;; chars and expand long symbols.
;;
;; M-x try-expand-dabbrev-limited-chars
;;   - expand dabbrev substring when input begins from `-' or `_'.
;;   - otherwise expand dabbrev
;;   - symbols expanded this command only contain 0-9a-zA-Z\?!_-,
;;     which is controlled to set `he-dabbrev-chars'
;;
;; M-x hippie-expand-file-name
;;   - complete file name at point
;;
;;
;; Example:
;;
;; -ex [M-x hippie-expand-dabbrev-limited-chars] -> hippie-expand

;;; Installation:
;;
;; Put hippie-exp-ext.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'hippie-exp-ext)
;; (global-set-key (kbd "C-@") 'hippie-expand-dabbrev-limited-chars)
;; (global-set-key (kbd "M-/") 'hippie-expand-file-name)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET hippie-exp RET
;;

;;; Change log:
;;
;; 2012/09/08
;;      * First released.
;;

;;; Acknowledgements:
;;
;;
;;

;;; TODO
;;
;;
;;

;;; Require
(require 'hippie-exp)
(eval-when-compile (require 'cl-lib))

;;; Code:
(defcustom he-dabbrev-chars "0-9a-zA-Z\\?!_-"
  "*Chars to be expanded by hippie-expand/dabbrev."
  :type 'string
  :group 'hippie-exp)
(defcustom he-dabbrev-substring-start-pattern "^[_-]"
  "*Regexp to start hippie-expand/dabbrev-substring."
  :type 'string
  :group 'hippie-exp)

;; refactored and generalized
(defun try-expand-dabbrev-0 (old search-func limit-up limit-down)
  "Generalized version of `try-expand-dabbrev'."
  (let (expansion)
    (unless old
      (he-init-string (he-dabbrev-beg) (point))
      (set-marker he-search-loc he-string-beg)
      (setq he-search-bw t))

    (unless (equal he-search-string "")
      (save-excursion
        (save-restriction
          (if hippie-expand-no-restriction
              (widen))
          ;; Try looking backward unless inhibited.
          (when he-search-bw
            (goto-char he-search-loc)
            (setq expansion (funcall search-func he-search-string t limit-up))
            (set-marker he-search-loc (point))
            (unless expansion
              (set-marker he-search-loc he-string-end)
              (setq he-search-bw nil)))
          (unless expansion             ; Then look forward.
            (goto-char he-search-loc)
            (setq expansion (funcall search-func he-search-string nil limit-down))
            (set-marker he-search-loc (point))))))

    (cond ((not expansion)
           (if old (he-reset-string))
           nil)
          (t
           (he-substitute-string expansion t)
           t))))

(defun he-dabbrev-substring-search (pattern &optional reverse limit)
  (when (string-match he-dabbrev-substring-start-pattern pattern)
    (let* ((result ())
           (regpat (concat (regexp-quote pattern) "\\([" he-dabbrev-chars "]+\\)")))
      (while (and (not result)
                  (if reverse
                      (re-search-backward regpat limit t)
                    (re-search-forward regpat limit t)))
        (setq result (buffer-substring-no-properties (save-excursion
                                                       (goto-char (match-beginning 0))
                                                       (skip-chars-backward he-dabbrev-chars)
                                                       (point))
                                                     (match-end 0)))
        (if (he-string-member result he-tried-table t)
            (setq result nil))) ; ignore if bad prefix or already in table
      result)))

(defun try-expand-dabbrev-substring (old)
  (try-expand-dabbrev-0 old 'he-dabbrev-substring-search nil nil))
(defun try-expand-dabbrev-substring-visible (old)
  (cl-letf (((symbol-function 'he-dabbrev-search)
             (symbol-function 'he-dabbrev-substring-search)))
    (try-expand-dabbrev-visible old)))
(defun try-expand-dabbrev-substring-visible-in-current-buffer (old)
  (try-expand-dabbrev-0 old 'he-dabbrev-substring-search (window-start) (window-end)))

(defun he-dabbrev-search--limited-chars (pattern &optional reverse limit)
  (let ((result ())
        (case-fold-search nil)
	(regpat (concat "\\b" (regexp-quote pattern) "\\([" he-dabbrev-chars "]+\\)")))
    (while (and (not result)
		(if reverse
		     (re-search-backward regpat limit t)
		     (re-search-forward regpat limit t)))
      (setq result (buffer-substring-no-properties (match-beginning 0)
						   (match-end 0)))
      (if (he-string-member result he-tried-table t)
	  (setq result nil)))     ; ignore if bad prefix or already in table
    result))

;; (find-fline (locate-library "hippie-exp.el") "defun he-dabbrev-beg")
(defun he-dabbrev-beg--limited-chars ()
  (let ((op (point)))
    (save-excursion
      (skip-chars-backward he-dabbrev-chars)
      (point))))

(defun he-limited-chars-replace-functions (func old)
  (cl-letf (((symbol-function 'he-dabbrev-search)
          (symbol-function 'he-dabbrev-search--limited-chars))
         ((symbol-function 'he-dabbrev-beg)
          (symbol-function 'he-dabbrev-beg--limited-chars)))
    (funcall func old)))

(defun try-expand-dabbrev-limited-chars (old)
  (he-limited-chars-replace-functions 'try-expand-dabbrev old))
(defun try-expand-dabbrev-limited-chars-visible (old)
  (he-limited-chars-replace-functions 'try-expand-dabbrev-visible old))
(defun try-expand-dabbrev-limited-chars-all-buffers (old)
  (he-limited-chars-replace-functions 'try-expand-dabbrev-all-buffers old))

(defun hippie-expand-with-function-list (funcs)
  "Do `hippie-expand' with `hippie-expand-try-functions-list' = FUNC."
  (let ((hippie-expand-try-functions-list funcs))
    (call-interactively 'hippie-expand)))

(defun hippie-expand-dabbrev-limited-chars ()
  "Preconfigured `hippie-expand' for dabbrev substring/limited-chars."
  (interactive)
  (hippie-expand-with-function-list
   '(try-expand-dabbrev-substring-visible-in-current-buffer
     try-expand-dabbrev-substring
     try-expand-dabbrev-substring-visible
     try-expand-dabbrev-limited-chars
     try-expand-dabbrev-limited-chars-visible
     try-expand-dabbrev-limited-chars-all-buffers)))

(defun hippie-expand-file-name ()
  "Preconfigured `hippie-expand' for file name completion."
  (interactive)
  (hippie-expand-with-function-list
   '(try-complete-file-name-partially
     try-complete-file-name)))

(provide 'hippie-exp-ext)

;;; hippie-exp-ext.el ends here
