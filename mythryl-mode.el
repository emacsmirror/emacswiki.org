;;; mythryl-mode.el --- Major mode for editing Mythryl code

;; Copyright (C) 2009 Phil Rand <philrand@gmail.com>
;; Copyright (C) 2010 Michele Bini <rev.22@hotmail.com>
;;
;; http://www.mythryl.org
;;
;; Largly cribbed from Stefan Monnier's sml-mode. See:
;; http://www.iro.umontreal.ca/~monnier/elisp/
;;

;; Mythryl-mode is not part of emacs.

;; Mythryl-mode is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; Mythryl-mode is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with mythryl-mode; see the file COPYING. If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; A major mode for editing the Mythryl programming language.
;;
;; This version of mythryl mode is derived from Stefan Monnier's
;; sml-mode. See http://www.iro.umontreal.ca/~monnier/elisp/, but
;; as of August 2009, the instructions on that page for accessing
;; the svn repository were incorrect.
;;
;; To use this mode, install the elisp files from the sml-mode
;; suite from the above URL somewhere in your elisp load path,
;; along with this file. Insert the expression:
;; (load "mythryl-mode")
;; Somewhere in your .emacs file.

;; Other useful lines for your .emacs (to automatically activate
;; mythryl-mode)

;; ;; for .pkg and .api files
;; (setq auto-mode-alist
;;       (append '(("\\.pkg$" . mythryl-mode)
;;                 ("\\.api$" . mythryl-mode))
;;               auto-mode-alist))

;; ;; for scripts starting with #!/.../mythryl
;; (setq interpreter-mode-alist
;;       (append '(("mythryl" . mythryl-mode))
;;               interpreter-mode-alist))

;; This revision of mythryl was released on 11 Feb 2010 by Michele
;; Bini and includes simple indentation support.

;;; Code:

(defgroup mythryl () "Group for customizing mythryl-mode"
  :prefix "mythryl-" :group 'languages)

(defvar mythryl-mode-op-face 'mythryl-mode-op-face)
(defface  mythryl-mode-op-face
  '((((class color) (background light)) (:foreground "blue"))
    (((class color) (background dark))  (:foreground "yellow"))
    (((class grayscale))                (:foreground "black"))
    (t                                  ()))
  "Face used for non-alphabetic identifiers in mythryl"
  :group 'mythryl)

(defvar mythryl-mode-pkg-face 'mythryl-mode-pkg-face)
(defface  mythryl-mode-pkg-face
  '((((class color) (background light)) (:foreground "#0af"))
    (((class color) (background dark))  (:foreground "#f50"))
    (((class grayscale))                (:foreground "black"))
    (t                                  ()))
  "Face used for non-alphabetic identifiers in mythryl"
  :group 'mythryl)

(defvar mythryl-mode-structure-face 'mythryl-mode-structure-face)
(defface  mythryl-mode-structure-face
  '((((class color) (background light)) (:foreground "black"))
    (((class color) (background dark))  (:foreground "white"))
    (((class grayscale))                (:foreground "white"))
    (t                                  ()))
  "Face used for structure characters in mythryl"
  :group 'mythryl)

(defconst mythryl-comment-regexp
  "\\(#\\($\\|[ #!]\\).*\\|/[*]\\([^*]\\|\\*[^/]\\)*[*]/\\)"
  )
;; (re-search-forward mythryl-comment-regexp) /* blue */

(defconst mythryl-string-regexp "\"\\([^\"\\]\\|\n\\|\\\\.\\)*\"")
;; (re-search-forward mythryl-string-regexp) "Hel\nlo"

(defconst mythryl-comment-or-string-regexp
  (concat "\\(" mythryl-comment-regexp
          "\\|" mythryl-string-regexp
          "\\)"))

(defvar mythryl-mode-hook nil
  "*Run upon entering `mythryl-mode'.
This is a good place to put your preferred key bindings.")

;;; * Indentation support code

(defun mythryl-skip-closing ()
  (and (looking-at
        (eval-when-compile
          (concat
           "\\(\\([]}); ]\\|"
           (regexp-opt (mapcar 'symbol-name '(end fi esac)) 'words)
           "\\) *\\)+")))
       (goto-char (match-end 0))))

(defun mythryl-skip-closing-2 ()
  (and (looking-at
        (eval-when-compile
          (concat
           "\\(\\([]}); ]\\|"
           (regexp-opt (mapcar 'symbol-name '(end fi esac then elif else)) 'words)
           "\\) *\\)+")))
       (goto-char (match-end 0))))

;; See also:
;; http://mythryl.org/my-Indentation.html
;; http://mythryl.org/my-If_statements.html
(defun mythryl-indent-line ()
  (interactive)
  (save-restriction
    (widen)
    (let ((c (current-indentation))
          (b (save-excursion
               (let (p)
                 (while
                     (and
                      (backward-to-indentation 1)
                      (not (or (looking-at "\\w") (looking-at "=")))
                      (or (not p) (< (point) p))
                      )
                   (setq p (point))))
               (mythryl-skip-closing)
               (cons (current-indentation)
                     (point))))
          (mp nil))
    (save-excursion
        (let ((bl (cdr b)) (i 0) (ln 1) (fn nil))
          (backward-to-indentation 0)
          (mythryl-skip-closing-2)
          (narrow-to-region bl (point))
          (goto-char (point-min))
          (save-excursion
            (while (re-search-forward
                    (eval-when-compile
                      (concat
                       "\\([][{}()\n\"#/;]\\|"
                       "[^\\!%&$+/:<=>?@~|*^-]\\(=>?\\)[^\\!%&$+/:<=>?@~|*^-]\\|"
                       (regexp-opt
                        (mapcar
                         'symbol-name
                         '(where end case esac if fn fun fi then else elif
                                 stipulate herein except))
                        'words)
                       "\\)"))
                    nil t)
              (goto-char (match-beginning 0))
              (let ((p (char-after (point)))
                    (mae (match-end 0)))
                (setq i (+ i
                           (cond
                            ((let ((m (match-string 2)))
                               (when m
                                 (setq mae (match-end 2))
                                 (if fn (progn (setq fn nil) (if (string= m "=>") +4 0)) 0))))
                            ((eq p ?\n) (setq ln -1) 0)
                            ((or (eq p ?\")
                                 (eq p ?#)
                                 (eq p ?/))
                             (and (looking-at
                                   mythryl-comment-or-string-regexp)
                                  (setq mae (match-end 0)))
                             0)
                            ((eq p ?\;) (setq fn nil) 0)
                            ((eq p ?\{) +4)
                            ((eq p ?\}) -4)
                            ((or (eq p ?\[) (eq p ?\()) +2)
                            ((or (eq p ?\]) (eq p ?\))) -2)
                            ((eq p ?c)
                             (cond
                              ((looking-at "\\<case\\>") +5)
                              (t 0)))
                            ((eq p ?f)
                             (cond
                              ((looking-at "\\<fu?n\\>") (setq fn t) 0)
                              ((looking-at "\\<fi\\>") -5)
                              (t 0)))
                            ((eq p ?e)
                             (cond
                              ((looking-at "\\<end\\>") -4)
                              ((looking-at "\\<else\\>")
                               (let ((n ln)) (setq ln 0) (* n 5)))
                              ((looking-at "\\<elif\\>")
                               (let ((n ln)) (setq ln 0) (* n 5)))
                              ((looking-at "\\<esac\\>") -5)
                              ((looking-at "\\<except\\>")
                               (setq fn t) 0)
                              (t 0)))
                            ((eq p ?h)
                             (cond
                              ((looking-at "\\<herein\\>") +4)
                              (t 0)))
                            ((eq p ?i)
                             (cond
                              ((looking-at "\\<if\\>")
                               (setq ln 0)
                               5)
                              (t 0)))
                            ((eq p ?s)
                             (cond
                              ((looking-at "\\<stipulate\\>") +4)
                              (t 0)))
                            ((eq p ?t)
                             (cond
                              ((looking-at "\\<then\\>")
                               (let ((n ln)) (setq ln 0) (* n 5)))
                              (t 0)))
                            ((eq p ?w)
                             (cond
                              ((looking-at "\\<where\\>") +4)
                              (t 0)))
                            (t (error
                                (concat
                                 "Unexpected char while scanning: "
                                 (string p))))
                            )))
                (goto-char mae))))
          (goto-char (point-max)) (widen)
          (setq b (car b))
          (backward-to-indentation 0)
          (setq i (+ b i))
          (if fn
              (if (< c b) (setq i b)
                (if (> c (+ b 4)) (setq i (+ b 4))
                  (setq c b))))
          (unless (= c i)
            (delete-region
             (point)
             (save-excursion (beginning-of-line) (point)))
            (indent-to i)
            (setq mp (point)))))
    (if (and mp (< (point) mp)) (goto-char mp)))))
                

(define-derived-mode mythryl-mode fundamental-mode
  "Mythryl"
  "Major mode for the Mythryl programming language."
  :group 'mythryl
  (set (make-local-variable 'indent-line-function) 'mythryl-indent-line)
  (set (make-local-variable 'comment-start) "# ")
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
(list

 ;; KEYWORDS
(list
 (list
  (eval-when-compile
    (concat "\\(#[0-9]+\\|"
            (regexp-opt
             (mapcar 'symbol-name
                     '(print)) 'words)
            "\\)"))
  1 font-lock-builtin-face)
 (list
  (eval-when-compile
    (regexp-opt
     (list "abstype" "also" "and" "api" "as" "case" "class" "elif"
           "else" "end" "eqtype" "esac" "except" "exception" "fi"
           "field" "fn" "for" "fprintf" "fun" "generic" "generic_api"
           "herein" "if" "include" "infix" "infixr" "lazy" "method"
           "my" "nonfix" "op" "or" "overload" "package" "printf"
           "raise" "rec" "sharing" "sprintf" "stipulate" "then" "type"
           "val" "where" "with" "withtype") 'words))
  1 font-lock-keyword-face)
 (list "\\(\\<[a-z][a-z'_0-9]*::+\\)" 1 mythryl-mode-pkg-face)
 (list "\\<\\(#?[a-z][a-z'_0-9]*\\|([\\!%&$+/:<=>?@~|*^-]+)\\)\\>" 0
font-lock-variable-name-face)
 (list "\\<[A-Z][A-Za-z'_0-9]*[a-z][A-Za-z'_0-9]*\\>" 0 font-lock-type-face)
 (list "\\<\\(_\\|[A-Z][A-Z'_0-9]*[A-Z][A-Z'_0-9]*\\)\\>" 0
font-lock-constant-face)
 (list "[\\!%&$+/:<=>?@~|*^-]+" 0 mythryl-mode-op-face)
 (list "[][(){};,.]+" 0 mythryl-mode-structure-face)
 (list mythryl-string-regexp 0 font-lock-string-face)

 ;; not caught by the syntax-table
 ;; FIXME: should use font-lock-syntactic-keywords
 (list "#\n" 0 font-lock-comment-face)
 )
;; KEYWORDS-ONLY
nil
;; CASE-FOLD
nil
;; SYNTAX-ALIST
'(
  (?\# . "w 12")
  ;;(?\# . ". 12")
  (?\! . ". 2")
(" " . "  2")
(?\n . ">")
(?\/ . ". 14b")
(?\* . ". 23b")
(?\( . "()")
(?\{ . "(}")
(?\[ . "(]")
("$:=&@\\^-.%+?>~" . ".")
(?\' . "w")
(?_ . "w")
(?\" . "\"")
(?\\ . "\\")
)
; SYNTAX-BEGIN
nil
; OTHER-VARS
)))
  ;(make-local-variable 'font-lock-syntax-table)
  ;(setq font-lock-syntax-table mythryl-syntax-table))

(provide 'mythryl-mode)
