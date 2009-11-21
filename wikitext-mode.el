;;; wikitext-mode.el --- Mode for editing Wikitexts
;;; (This mode is derived from wikipedia-mode ver.0.3.3)

;; Copyright (C) 2003, 2004 Chong Yidong (wikipedia-mode.el)
;; Copyright (C) 2005 Dmitri Brechalov (wikitext-mode.el)

;; Author: Chong Yidong <cyd at stupidchicken com>
;; Version: 0.2
;; Keywords: wiki

;; This file is NOT part of GNU Emacs.

;; This file is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This file is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with GNU Emacs; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:

;; This is `wikitext-mode', a major mode for editing articles written
;; in the markup language used by Wikispace. It is intended to work
;; with GNU Emacs 21.x, though it may also work with other versions of
;; (X)Emacs.

;; Installing wikitext-mode
;; =========================
;;
;; Save wikitext-mode.el in a convenient directory, preferably in
;; your `load-path'. Add the following to your `user-init-file':
;;
;;   (autoload 'wikitext-mode
;;     "wikitext-mode.el"
;;     "Major mode for editing wiki-documents." t)
;;
;; If you did not save wikitext-mode.el in your `load-path', you must
;; use the full pathname. On MS Windows, use forward slashes (/)
;; rather than back slashes (\) to indicate the directory, e.g.:
;;
;;   (autoload 'wikitext-mode
;;     "C:/Documents and Settings/USERNAME/.emacs.d/Wikitext-mode.el"
;;     "Major mode for editing wiki-documents." t)
;;
;; If you want to associate filenames ending in ".wiki" with
;; wikitext-mode, add the following to your init file:
;;
;;   (setq auto-mode-alist
;;     (cons '("\\.wiki\\'" . wikitext-mode) auto-mode-alist))

;; Installing longlines-mode
;; =========================
;;
;; Wikitext articles don't use newline characters to break paragraphs
;; into lines, so each paragraph looks like a super-long line to
;; Emacs. To let Emacs handle "soft word wrapping", you need to
;; download a third-party package, longlines-mode.
;;
;; Download longlines.el, saving into your `load-path':
;;
;;   http://www.emacswiki.org/elisp/longlines.el
;;
;; Add the following to your `user-init-file':
;;
;;   (autoload 'longlines-mode "longlines.el"
;;     "Minor mode for editing long lines." t)

;; TODO: 
;; + do not reformat headers
;; * use monospace face inside [[code]]...[[code]] (just place a space at the beginning of each line)
;; + do not touch the text inside [[code]]...[[code]]
;; - make regexps customizable
;; - make customization group
;; - clean up the code

;;; Code:

(require 'derived)
(require 'font-lock)

;; (defvar wikitext-simple-tags
;;   '("b" "big" "blockquote" "br" "caption" "code" "center" "cite" "del"
;;     "dfn" "dl" "em" "i" "ins" "kbd" "math" "nowiki" "ol" "pre" "samp"
;;     "small" "strike" "strong" "sub" "sup" "tt" "u" "ul" "var")
;;   "Tags that do not accept arguments.")

;; (defvar wikitext-complex-tags
;;   '("a" "div" "font" "table" "td" "th" "tr")
;;   "Tags that accept arguments.")

(defvar wikitext-url-protocols
  '("ftp" "gopher" "http" "https" "mailto" "news")
  "Valid protocols for URLs in Wikitext articles.")

(defface wikitext-strong-emphasis-face '((t (:inherit bold-italic)))
  "`wikitext-mode' face used to highlight text marked with strong and emhasis markup (e.g. //**FOO**// or **//FOO//**.)")
(defvar wikitext-strong-emphasis-face 'wikitext-strong-emphasis-face)

(defface wikitext-strong-face '((t (:inherit bold)))
  "`wikitext-mode' face used to highlight text marked with two asteriscs (e.g. **FOO**.)")
(defvar wikitext-strong-face 'wikitext-strong-face)

(defface wikitext-emphasis-face '((t (:inherit italic)))
  "`wikitext-mode' face used to highlight text marked with two
slashes (e.g. //FOO//.)")
(defvar wikitext-emphasis-face 'wikitext-emphasis-face)

(defface wikitext-header-face '((t (:inherit bold)))
  "`wikitext-mode' face used to highlight section and subsection
headers (e.g. == FOO ==.)")
(defvar wikitext-header-face 'wikitext-header-face)

(defvar wikitext-font-lock-keywords
      (list

  ;; Text markup
  (cons "\\(//\\*\\*\\)\\(.*?\\|\n\\)\\(\\*\\*//\\)"
        'wikitext-strong-emphasis-face)
  (cons "\\(\\*\\*//\\)\\(.*?\\|\n\\)\\(//\\*\\*\\)"
        'wikitext-strong-emphasis-face)
  (cons "\\*\\*\\([^*]\\|[^*]\\*\\)*?\\*\\*"
        'wikitext-strong-face)
  (list "\\([^:]\\)\\(//\\([^/]\\|[^/]/\\)*?//\\)"
        '(1 font-lock-doc-face)
        '(2 wikitext-emphasis-face))

  ;; Headers and dividers
  (list "^\\(=+\\) \\(.*\\) \\(\\1\\)"
        '(1 font-lock-builtin-face)
        '(2 wikitext-header-face)
        '(3 font-lock-builtin-face))
  (cons "^-----*" 'font-lock-builtin-face)

  ;; Bare URLs
  (cons (concat "\\(^\\|[ \t]\\)" (regexp-opt wikitext-url-protocols t)
                "://[-A-Za-z0-9._\/~%+&#?!=()@]+")
        'font-lock-variable-name-face)

  ;; Indentation, lists and definitions
  (cons "^\\(> +\\|[*#]+ +\\)" 'font-lock-builtin-face)
;;   (list "^\\(;\\)\\([^:\n]*\\)\\(:?\\)"
;;         '(1 font-lock-builtin-face)
;;         '(2 font-lock-doc-face)
;;         '(3 font-lock-builtin-face))
  (cons "^[ \t]+\\(.*\\)$" 'font-lock-constant-face)

  ;; Tables
  (cons "||" 'font-lock-builtin-face)

  ;; Tags and comments
;;   (list (concat "\\(</?\\)"
;;                 (regexp-opt wikitext-simple-tags t) "\\(>\\)")
;;         '(1 font-lock-builtin-face t t)
;;         '(2 font-lock-function-name-face t t)
;;         '(3 font-lock-builtin-face t t))
;;   (list (concat "\\(</?\\)"
;;                 (regexp-opt wikitext-complex-tags t)
;;    "\\(\\(?: \\(?:[^\"'/><]\\|\"[^\"]*\"\\|'[^']*'\\)*\\)?\\)\\(>\\)")
;;         '(1 font-lock-builtin-face t t)
;;         '(2 font-lock-function-name-face t t)
;;         '(3 font-lock-doc-face t t)
;;         '(4 font-lock-builtin-face t t))
;;   (cons (concat "<!-- \\([^->]\\|>\\|-\\([^-]\\|-[^>]\\)\\)*-->")
;;         '(0 font-lock-comment-face t t))

  ;; External Links
  (list (concat "\\(\\[\\)\\(\\(?:"
                (regexp-opt wikitext-url-protocols)
"\\)://[-A-Za-z0-9._\/~%-+&#?!=()@]+\\)\\(\\(?: [^]\n]*\\)?\\)\\(\\]\\)")
        '(1 font-lock-builtin-face t t)
        '(2 font-lock-variable-name-face t t)
        '(3 font-lock-doc-face t t)
        '(4 font-lock-builtin-face t t))

  ;; Wiki links & tags
  '("\\(\\[\\[\\)\\([^]\n|]*\\)\\([| ]?\\)\\([^]\n]*\\)\\(\\]\\]\\)"
    (1 font-lock-builtin-face t t)
    (2 font-lock-variable-name-face t t)
    (3 font-lock-builtin-face t t)
    (4 font-lock-doc-face t t)
    (5 font-lock-builtin-face t t))

  ;; Monospaced font
  '("\\({{\\)\\(\\(.\\|\n\\)*?\\)\\(}}\\)"
    (1 font-lock-builtin-face t t)
    (2 font-lock-constant-face t t)
    (4 font-lock-builtin-face t t))

  ;; Preformatted text
  '("\\(``\\)\\(.*?\\|\n\\)\\(``\\)"
    (1 font-lock-builtin-face t t)
    (2 font-lock-constant-face t t)
    (3 font-lock-builtin-face t t))

  ;; Character entity references
;;   (cons "&#?[a-zA-Z0-9]+;" '(0 font-lock-type-face t t))

  ;; Indented text
;;   (cons "^> .*$" '(0 font-lock-variable-face t t))

  ;; Special tags
;;   (list "\\[\\[code\\]\\]\\(\\(.\\|\n+ \\)+?\\)\\[\\[code\\]\\]"
;;         '(1 font-lock-constant-face t t))
  ))

(defvar wikitext-imenu-generic-expression
  (list '(nil "^=+ *\\(.*[^\n=]\\)=+" 1))
  "Imenu expression for wikitext-mode. See `imenu-generic-expression'.")

(defun wikitext-next-header ()
  "Move point to the end of the next section header."
  (interactive)
  (let ((oldpoint (point)))
    (end-of-line)
    (if (re-search-forward "\\(^=+\\) .*\\1" (point-max) t)
        (beginning-of-line)
      (goto-char oldpoint)
      (message "No section headers after point."))))

(defun wikitext-prev-header ()
  "Move point to the start of the previous section header."
  (interactive)
  (unless (re-search-backward "\\(^=+\\) .*\\1" (point-min) t)
    (message "No section headers before point.")))

(defun wikitext-terminate-paragraph ()
  "In a list, start a new list item. In a paragraph, start a new
paragraph; if the current paragraph is colon indented, the new
paragraph will be indented in the same way."
  (interactive)
  (let (indent-chars)
    (save-excursion
      (beginning-of-line)
      (while (cond ((looking-at "^$") nil)
                   ((looking-at "^\\([#*> ]+\\)")
                    (setq indent-chars (match-string 1)) nil)
                   ((eq (point) (point-min)) nil)
                   ((progn (forward-line -1) t)))
        t))
    (insert (concat "\n" (or indent-chars "\n")))))

(defun wikitext-link-fill-nobreak-p ()
  "When filling, don't break the line for preformatted (fixed-width)
text or inside a Wiki link. See `fill-nobreak-predicate'."
  (save-excursion
    (let ((pos (point)))
      (or (eq (char-after (line-beginning-position)) ? )
          (if (re-search-backward "\\[\\[" (line-beginning-position) t)
              ;; Break if the link is really really long.
              ;; You often get this with captioned images.
              (null (or (> (- pos (point)) fill-column)
                        (re-search-forward "\\]\\]" pos t))))))))

(defun wikitext-fill-article ()
  "Fill the entire article."
  (interactive)
  (save-excursion
    (fill-region (point-min) (point-max))))

(defun wikitext-unfill-article ()
  "Undo filling, deleting stand-alone newlines (newlines that do not
end paragraphs, list entries, etc.)"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward ".\\(\n\\)\\(\\[\\[code\\]\\]\\|[^# *|\n>]\\)" nil t)
      ;; unfill unless [[code]] at the beginning of the line
      (if (string= (match-string 2) "[[code]]") nil
        (replace-match " " nil nil nil 1))))
  (message "Stand-alone newlines deleted"))

;;;###autoload
(define-derived-mode wikitext-mode text-mode "Wikitext"
  "Major mode for editing articles written in the markup language used by some wikis.

There are several ways to use wikitext-mode. One is to copy articles
between Emacs and your web browser's text box. Another way is to use
MozEx, a Mozilla/Firefox web browser extension that allows you to
call Emacs from a text box (http://mozex.mozdev.org/), or use any
other browser plug-in or extension, which allow save and load
textareas to files.

Wikitext articles are usually unfilled: newline characters are not
used for breaking paragraphs into lines. Unfortunately, Emacs does
not handle word wrapping yet. As a workaround, wikitext-mode turns
on longlines-mode automatically. In case something goes wrong, the
following commands may come in handy:

\\[wikitext-fill-article] fills the buffer.
\\[wikitext-unfill-article] unfills the buffer.

The following commands are also defined:

\\[wikitext-terminate-paragraph]     starts a new list item or paragraph in a context-aware manner.
\\[wikitext-next-header]     moves to the next (sub)section header.
\\[wikitext-prev-header]     moves to the previous (sub)section header."

  (set (make-local-variable 'adaptive-fill-regexp) "[ ]*")
  (set (make-local-variable 'comment-start-skip) "\\(?:<!\\)?-- *")
  (set (make-local-variable 'comment-end-skip) " *--\\([ \n]*>\\)?")
  (set (make-local-variable 'comment-start) "<!-- ")
  (set (make-local-variable 'comment-end) " -->")
  (set (make-local-variable 'paragraph-start)
       "> \\|\\*\\| \\|=+ \\|#\\|;\\|:\\||\\|!\\|$")
  (set (make-local-variable 'sentence-end-double-space) nil)
  (set (make-local-variable 'font-lock-multiline) t)
  (set (make-local-variable 'font-lock-defaults)
       '(wikitext-font-lock-keywords t nil nil nil))
  (set (make-local-variable 'fill-nobreak-predicate)
       'wikitext-link-fill-nobreak-p)
  (set (make-local-variable 'auto-fill-inhibit-regexp) 
       "^\\([ *#:|;>]\\|=+ .* =+\\)")

  ;; Support for outline-minor-mode. No key conflicts, so we'll use
  ;; the normal outline-mode prefix.
  (set (make-local-variable 'outline-regexp) "=+")
  (set (make-local-variable 'outline-minor-mode-prefix) "\C-c")

  ;; Turn on the Imenu automatically.
  (when menu-bar-mode
    (set (make-local-variable 'imenu-generic-expression)
         wikitext-imenu-generic-expression)
    (imenu-add-to-menubar "Contents"))

  (modify-syntax-entry ?< "(>" wikitext-mode-syntax-table)
  (modify-syntax-entry ?> ")<" wikitext-mode-syntax-table)

  (define-key wikitext-mode-map "\M-n" 'wikitext-next-header)
  (define-key wikitext-mode-map "\M-p" 'wikitext-prev-header)
  (define-key wikitext-mode-map [M-down] 'wikitext-next-header)
  (define-key wikitext-mode-map [M-up]   'wikitext-prev-header)
  (define-key wikitext-mode-map "\C-j" 'wikitext-terminate-paragraph)
  (define-key wikitext-mode-map [(control return)]
    'wikitext-terminate-paragraph)

  (let ((map (make-sparse-keymap "Wikitext")))
    (define-key wikitext-mode-map [menu-bar wikitext]
      (cons "Wikitext" map))
    (define-key map [unfill-article]
      '("Unfill article" . wikitext-unfill-article))
    (define-key map [fill-article]
      '("Fill article" . wikitext-fill-article))
    (define-key map [separator-fill] '("--"))
    (define-key map [next-header]
      '("Next header" . wikitext-next-header))
    (define-key map [prev-header]
      '("Previous header" . wikitext-prev-header))
    (define-key map [separator-header] '("--"))
    (define-key map [outline]
      '("Toggle Outline Mode..." . outline-minor-mode)))

  (define-key wikitext-mode-map "\C-c\C-q"
    'wikitext-unfill-article)
  (define-key wikitext-mode-map "\C-c\M-q"
    'wikitext-fill-article)

  (make-local-variable 'change-major-mode-hook))

(defun wikitext-turn-on-longlines ()
  "Turn on longlines-mode if it is defined."
  (if (functionp 'longlines-mode)
      (longlines-mode 1)))
(add-hook 'wikitext-mode-hook 'wikitext-turn-on-longlines)

(provide 'wikitext-mode)
;;; wikitext-mode.el ends here.
