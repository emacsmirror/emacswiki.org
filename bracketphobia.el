;;; bracketphobia.el --- Protect the user from the Bracket Invasion

;; Copyright (C) 2006  Jorgen Schaefer

;; Version: 1.1
;; Author: Jorgen Schaefer <forcer@forcix.cx>
;; URL: http://www.emacswiki.org/cgi-bin/wiki/emacs/bracketphobia.el

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
;; 02110-1301  USA

;;; Commentary:

;; There has been a tendency among Scheme teachers to use brackets at
;; some places in the Scheme syntax to help newcomers to read the
;; language. Some experienced Scheme users claim that this makes code
;; less readable, and I agree. If you do, too, this mode will protect
;; you from the malicious bracket invasion.

;; See http://www.forcix.cx/weblog/2006-05-07.html for a longer
;; explanation.

;; To use, simply M-x bracketphobia in a file with brackets, or add
;; `bracketphobia-auto' to your `find-file-hook'.

;; There's also M-x bracketphobia-hide, but that is only usable for
;; viewing, and I haven't found out how to turn it off yet :-)

;;; Code:

(defcustom bracketphobia-modes '(scheme-mode common-lisp-mode emacs-lisp-mode)
  "The modes where brackets need to be gone.
See `bracketphobia-auto'."
  :group 'applications
  :type '(repeat function))

(defun bracketphobia ()
  "Replace brackets outside of strings and comments with parens."
  (interactive)
  ;; This can be called in a hook before font lock
  ;; mode has a chance to run, but we need its
  ;; information. So we enforce a font lock run.
  (font-lock-set-defaults)
  (font-lock-default-fontify-region (point-min)
                                    (point-max)
                                    nil)
  (save-excursion
    (goto-char (point-min))
    (while (bracketphobia-find)
      (if (string= "[" (match-string 0))
          (replace-match "(")
        (replace-match ")")))))

(defun bracketphobia-auto ()
  "Query the user whether to remove brackets from this file.
This is a suitable value for `find-file-hook', and will only
consider files in modes mentioned in `bracketphobia-modes'. Also
see `bracketphobie'."
  (when (memq major-mode bracketphobia-modes)
    (save-excursion
      (goto-char (point-min))
      (font-lock-set-defaults)
      (font-lock-default-fontify-region (point-min)
                                        (point-max)
                                        nil)
      (when (and (bracketphobia-find)
                 (save-window-excursion
                   (switch-to-buffer (current-buffer))
                   (y-or-n-p "This file contains brackets. Exterminate them? ")))
        (bracketphobia)))))

(defun bracketphobia-find ()
  "Find the next bracket.
This sets the match string 0 to the bracket."
  (catch 'return
    (while (re-search-forward "[][]" nil t)
      (let ((face (get-text-property (match-beginning 0)
                                     'face)))
        (message "Face: %S" face)
        (when (not (or (looking-back "#\\\\[][]") ; character literal
                       (if (consp face)
                           (or (memq 'font-lock-comment-face face)
                               (memq 'font-lock-string-face face))
                         (or (eq face 'font-lock-comment-face)
                             (eq face 'font-lock-string-face)))))
          (throw 'return t))))
    nil))


;; Hiding of brackets
;; This is not as stable and changeable as I would like it to be.

(defvar bracketphobia-keywords
  '(("\\["
     (0 (progn
          (compose-region (match-beginning 0) (match-end 0)
                          "(")
          nil)))
    ("\\]"
     (0 (progn
          (compose-region (match-beginning 0) (match-end 0)
                          ")")
          nil))))
  "The keywords used to hide brackets.")

;; Does anyone know how to turn this off again?
;; `font-lock-remove-keywords' doesn't seem to work right.
(defun bracketphobia-hide ()
  "Show brackets as parens."
  (interactive)
  (font-lock-add-keywords nil bracketphobia-keywords)
  (font-lock-fontify-buffer))


(provide 'bracketphobia)
;;; bracketphobia.el ends here
