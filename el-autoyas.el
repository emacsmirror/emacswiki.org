;;; el-autoyas.el --- Automatically create Emacs-Lisp Yasnippets
;; 
;; Filename: el-autoyas.el
;; Description: Automatically create Emacs-Lisp Yasnippets
;; Author: Matthew L. Fidler
;; Maintainer: Matthew L. Fidler
;; Created: Mon Nov 21 10:55:55 2011 (-0600)
;; Version: 0.20
;; Last-Updated: Tue May 29 21:43:49 2012 (-0500)
;;           By: Matthew L. Fidler
;;     Update #: 196
;; URL: https://github.com/mlf176f2/el-autoyas.el
;; Keywords: Emacs Lisp Mode Yasnippet
;; Compatibility: 
;; 
;; Features that might be required by this library:
;;
;;   `assoc', `button', `cl', `dropdown-list', `easymenu', `eldoc',
;;   `help-fns', `help-mode', `view', `yasnippet',
;;   `yasnippet-bundle'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Install:
;;
;;    (autoload 'el-autoyas-enable "el-autoyas")
;;    (add-hook 'emacs-lisp-mode-hook 'el-autoyas-enable)
;;
;;; Commentary: 
;; 
;;   
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;; 29-May-2012    Matthew L. Fidler  
;;    Last-Updated: Mon Nov 28 08:46:55 2011 (-0600) #194 (Matthew L. Fidler)
;;    Typo fixed (rdparker)
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'eldoc)
(require 'yasnippet nil t)
(require 'yasnippet-bundle nil t)

(defgroup el-autoyas nil
  "Automatic gerenation of Yasnippets based on Emacs Lisp documentation"
  :group 'yasnippet)

(defcustom el-autoyas-abbrevs
  '(
    ("ah" "add-hook")
    ("a" "and")
    ("bc" "backward-char")
    ("bol" "beginning-of-line")
    ("bounds-of-thing-at-point" "botap")
    ("bfn" "buffer-file-name")
    ("bmp" "buffer-modified-p")
    ("bsnp" "buffer-substring-no-properties")
    ("bs" "buffer-substring")
    ("cc" "condition-case")
    ("cf" "copy-file")
    ("cb" "current-buffer")
    ("ca" "custom-autoload")
    ("dk" "define-key")
    ("d" "defun")
    ("dc" "delete-char")
    ("dd" "delete-directory")
    ("df" "delete-file")
    ("dr" "delete-region")
    ("df" "directory-files")
    ("eol" "end-of-line")
    ("efn" "expand-file-name")
    ("f" "format")
    ("fnd" "file-name-directory")
    ("fne" "file-name-extension")
    ("fnn" "file-name-nondirectory")
    ("fnse" "file-name-sans-extension")
    ("frn" "file-relative-name")
    ("ff" "find-file")
    ("fc" "forward-char")
    ("bc" "backward-char")
    ("fl" "forward-line")
    ("gsk" "global-set-key")
    ("gc" "goto-char")
    ("ifc" "insert-file-contents")
    ("i" "insert")
    ("kb" "kill-buffer")
    ("l" "let")
    ("lbp" "line-beginning-position")
    ("lep" "line-end-position")
    ("la" "looking-at")
    ("md" "make-directory")
    ("mlv" "make-local-variable")
    ("mb" "match-beginning")
    ("me" "match-end")
    ("ms" "match-string")
    ("m" "message")
    ("n" "not")
    ("nts" "number-to-string")
    ("o" "or")
    ("pm" "piont-min")
    ("p" "point")
    ("rsb" "re-search-backward")
    ("rsf" "re-search-forward")
    ("rap" "region-active-p")
    ("rb" "region-beginning")
    ("re" "region-end")
    ("rf" "rename-file")
    ("rris" "replace-regexp-in-string")
    ("rr" "replace-regexp")
    ("sb" "save-buffer")
    ("se" "save-excursion")
    ("sbr" "search-backward-regexp")
    ("sb" "search-backward")
    ("sfr" "search-forward-regexp")
    ("sf" "search-forward")
    ("sb" "set-buffer")
    ("sfm" "set-file-modes")
    ("sm" "set-mark")
    ("s" "setq")
    ("scb" "skip-chars-backward")
    ("scf" "skip-chars-forward")
    ("ss" "split-string")
    ("sm" "string-match")
    ("tap" "thing-at-point")
    ("v" "vector")
    ("wg" "widget-get")
    ("wcb" "with-current-buffer")
    ("yonp" "y-or-no-p")
    )
  "A list of abbreviations that expand to a set function"
  :type '(repeat
          (list
           (string :tag "Abbreviation")
           (string :tag "Emacs Function")))
  :group 'el-autoyas)

(defvar el-autoyas-load-hook nil
  "*Hook run on package load.
Suggestion: Add `el-autoyas-install'.")

(defun el-autoyas-expand-maybe (&optional arg)
  "Expands autoyas snippets.  Falls back to
`indent-for-tab-command' if it does not expand."
  (interactive "P")
  (let (what del)
    (if (and (looking-at ")")
             (looking-back "([^ \n\t()]*" nil t))
        (progn
          (setq what (match-string 0))
          (setq del t))
      
      (if (looking-back "([^ \n\t()]*" nil t)
          (progn
            (setq what (match-string 0)))
          (if (looking-back "[^ \n\t()]*" nil t)
           (setq what (match-string 0)))))
    (if what
        (el-autoyas-snippet what del arg)
      (indent-for-tab-command arg))))

(defun el-autoyas-snippet (symbol-name &optional del-char arg)
  "Creates a snippet based on the currently defined symbol" 
  (let ((symbol symbol-name)
        function-name
        (i 1)
        (opt nil)
        (found-body nil)
        (moving-away-text "nil")
        (snippet nil))
    (if (string= "" symbol-name)
        (indent-for-tab-command arg)
      ;; Remove initial (
      (when (and symbol (string= (substring symbol 0 1) "("))
        (setq symbol (substring symbol 1)))
      (when (assoc symbol el-autoyas-abbrevs)
        (setq symbol (nth 1 (assoc symbol el-autoyas-abbrevs))))
      (setq function-name symbol)
      (setq symbol (intern-soft symbol))
      (if (not (and symbol (fboundp symbol)))
          (indent-for-tab-command arg)
        (setq snippet (eldoc-get-fnsym-args-string symbol))
        (if (not (string-match ":[ \t]*[(]\\(.*?\\)[)]" snippet))
            (indent-for-tab-command arg)
          (setq snippet (match-string 1 snippet))
          (with-temp-buffer
            (insert snippet)
            (goto-char (point-min))
            (while (re-search-forward "\\([^ \t}]+\\)" nil t)
              (when found-body
                (goto-char (match-beginning 0))
                (setq found-body nil)
                (insert "\n")
                (re-search-forward "\\([^ \t}]+\\)" nil t))
              (cond
               ((string= "&rest" (match-string 1))
                (replace-match "")
                (setq opt t)
                (setq moving-away-text "")
                (delete-char 1))
               ((save-match-data (string-match "^BODY" (match-string 1)))
                (setq found-body t)
                (replace-match "\n$0"))
               ((string= "&optional" (match-string 1))
                (replace-match (concat "${" (number-to-string i) ":"))
                (save-excursion
                  (goto-char (match-beginning 0))
                  (backward-char 1)
                  (delete-char 1))
                (setq opt t)
                (save-excursion
                  (goto-char (point-max))
                  (insert "}"))
                (setq i (+ 1 i)))
               (opt
                (replace-match (concat "${" (number-to-string i) ":\\1$(el-autoyas-text-on-moving-away \"" moving-away-text "\" \"\\1\")}") t)
                (setq i (+ 1 i)))
               (t
                (replace-match (concat "${" (number-to-string i) ":\\1}"))
                (setq i (+ 1 i)))))
            (goto-char (point-min))
            (insert "(" function-name " ")
            (goto-char (point-max))
            (when (looking-back "[$]{[^{]*[.][.][.][}]\\([}]*\\)")
              (replace-match "\\1")
              (insert "$0"))
            (insert ")")
            (setq snippet (buffer-substring-no-properties (point-min) (point-max))))
          (when (and del-char (looking-at ")"))
            (replace-match ""))
          (delete-region (- (point) (length symbol-name)) (point))
          (yas/expand-snippet snippet))))))

(defun el-autoyas-text-on-moving-away (default-text &optional orig-text)
  "* Changes text when moving away AND original text has not changed"
  (cond
   ((or (and (not yas/modified-p) yas/moving-away-p)
        (and yas/moving-away-p orig-text (string= orig-text yas/text)))
    (let (el-autoyas-not-editing)
      (if (string= "" default-text)
          (yas/skip-and-clear-or-delete-char)
        (insert default-text))
      (el-autoyas-update)))))

(defun el-autoyas-update ()
  "Update fields"
  (let ((snippet (first (yas/snippets-at-point))))
    (when snippet
      (yas/update-mirrors snippet))))

;;;###autoload
(defun el-autoyas-enable ()
  "Load and activate package."
  (require 'el-autoyas nil t)
  (when (featurep 'el-autoyas)
    (set (make-local-variable 'yas/fallback-behavior)
	 '(apply el-autoyas-expand-maybe))
    (yas/minor-mode 1)))

(provide 'el-autoyas)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; el-autoyas.el ends here
