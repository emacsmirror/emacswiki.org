;;; outline-wiki.el --- An outline-based wiki-like mode

;; Copyright (C) 2005  Jorgen Schaefer

;; Version: 1.1
;; Keywords: outlines, wiki
;; Author: Jorgen Schaefer
;; URL: http://www.emacswiki.org/cgi-bin/wiki/OutlineWikiMode

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
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Commentary:

;; outline-wiki-mode is an extended outline mode. The only addition to
;; the normal outline capabilities is a link syntax to files. Every
;; consecutive range or characters without whitespace between braces
;; is treated as a link to a file.

;; Braces where chosen because brackets are often used for array
;; indices in some languages, so this might prove annoying. Braces are
;; annoying in TeX files, though.

;; The usefulness of such file links were proven by emacs-wiki. This
;; mode tries to be as useful as emacs-wiki without most of the more
;; arcane features of that mode. To ease the creation of links --
;; emacs-wiki allowed CamelCase links --, we provide a C-c C-x
;; shortcut to turn the current word into a link.

;;; Code:

(require 'outline)

(defface outline-wiki-link-face '((t (:foreground "cyan")))
  "The face to use for file links."
  :group 'outline)

(defvar outline-wiki-link-regexp "{\\(\\S-+\\)}"
  "The regexp that matches a link.")

(defvar outline-wiki-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'outline-wiki-follow-link-at-point)
    (define-key map (kbd "C-c C-x") 'outline-wiki-make-link)
    (define-key map (kbd "TAB") 'outline-wiki-next-link)
    map)
  "The keymap used in `outline-wiki-mode'.")

(defvar outline-wiki-font-lock-keywords
  `((,outline-wiki-link-regexp 1 'outline-wiki-link-face)
    ,@outline-font-lock-keywords)
  "Expressions to highlight in Outline Wiki mode.
This includes the definitions from Outline mode, and extends them
to include the file link syntax.")

(define-derived-mode outline-wiki-mode outline-mode "OutlWiki"
  "An Emacs mode for extending outline with file links.

This mode extends `outline-mode' to include file links, i.e.
links within a buffer that lead to a different file when
\\[outline-wiki-follow-link-at-point] is used on the link.

File links are created using braces syntax. Any consecutive
sequence of non-whitespace characters between braces are
automatically converted to a link to a file with that name in the
same directory.

\\{outline-wiki-mode-map}"
  (set (make-local-variable 'font-lock-defaults)
       '(outline-wiki-font-lock-keywords t nil nil backward-paragraph)))

(defun outline-wiki-follow-link-at-point ()
  "Follow the outline-wiki link at point. This is the consecutive
sequence of non-whitespace characters that is enclosed by
braces."
  (interactive)
  (if (save-excursion
        (and (re-search-backward "\\s-\\|{\\|}" nil t)
             (looking-at outline-wiki-link-regexp)))
      (let* ((link (match-string 1))
             (subref-match (string-match "^\\(.*\\)#\\(.*\\)$" link))
             (file (if subref-match
                       (match-string 1 link)
                     link))
             (anchor (if subref-match
                         (match-string 2 link)
                       nil))
             (path (if (string-match "^[/~]" file)
                       file
                     (concat (file-name-directory (buffer-file-name))
                             file))))
        (find-file path)
        (when anchor
          (let ((oldpoint (point)))
            (goto-char (point-min))
            (when (not (or (re-search-forward
                            (concat "^\*+ " anchor "$")
                            nil t)
                           (re-search-forward
                            (concat "^\*+ " anchor)
                            nil t)))
              (goto-char oldpoint)
              (error "Anchor %s not found" anchor)))))
    (newline)))

(defun outline-wiki-make-link ()
  "Convert the word at point to a file link by surrounding it
with braces."
  (interactive)
  (save-excursion
    (when (not (looking-at "\\<"))
      (backward-word))
    (insert "{")
    (forward-word)
    (insert "}")))

(defun outline-wiki-next-link ()
  "Jump to the next file link in the current buffer."
  (interactive)
  (if (re-search-forward outline-wiki-link-regexp nil t)
      (goto-char (match-beginning 1))
    (goto-char (point-min))
    (if (re-search-forward outline-wiki-link-regexp nil t)
        (goto-char (match-beginning 1))
      (error "No file link in current buffer."))))

(provide 'outline-wiki)
;;; outline-wiki.el ends here
