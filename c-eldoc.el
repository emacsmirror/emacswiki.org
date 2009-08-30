;;; c-eldoc.el --- helpful description of the arguments to C functions

;; Copyright (C) 2004 Paul Pogonyshev
;; Copyright (C) 2004, 2005 Matt Strange

;; This file is NOT a part of GNU Emacs

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
;; USA

;;; Commentary:

;; Provides helpful description of the arguments to C functions.
;; Uses child process grep and preprocessor commands for speed.
;; v0.4 01/16/05

;; Comments and suggestions are appreciated
;; mstrange at wans dot net

;;; Code:

(require 'eldoc)
;; without this, you can't compile this file and have it work properly
;; since the `c-save-buffer-state' macro needs to be known as such
(require 'cc-defs)

;; make sure that the opening parenthesis in C will work
(eldoc-add-command 'c-electric-paren)

;; NOTE: most people with normal GNU systems won't need to mess with
;; these options.

;; if you aren't using /lib/cpp as your preprocessor, set the
;; replacement here
(defvar c-eldoc-cpp-command "/lib/cpp")

;; if you've got a non-GNU preprocessor with funny options, set these
;; variables to fix it
(defvar c-eldoc-cpp-macro-arguments "-dD -w -P")
(defvar c-eldoc-cpp-normal-arguments "-w -P")

;; if you aren't using a program named grep or it needs a path to
;; execute, set that here
(defvar c-eldoc-grep-command "grep")

;; add in your commonly used packages/include directories here, for
;; example, SDL or OpenGL. this shouldn't slow down cpp, even if
;; you've got a lot of them
(defvar c-eldoc-includes "`pkg-config gtk+-2.0 --cflags` -I./ -I../ ")

;; how expansive is your code? if you've split your declarations into
;; one variable per line AND write functions w/ a ridiculously large
;; number of parameters, you might need to up this. it will make the
;; macro slightly less efficient
(defvar c-eldoc-context "10")

;; command to pass to `grep'
;; NOTE: i didn't really want -m 2, but it segfaults sometimes with
;; -m1 and the -dD option! TODO: why is this?
(defvar c-eldoc-grep (concat "grep -A " c-eldoc-context " -B "
                             c-eldoc-context " -m 2 -e "))

;; tell the macro not to check built-in commands
(defvar c-eldoc-reserved-words
  (list "if" "else" "switch" "while" "for" "sizeof"))

;; run this to begin
(defun c-turn-on-eldoc-mode ()
  (interactive)
  (set (make-local-variable 'eldoc-documentation-function)
       'c-eldoc-print-current-symbol-info)
  (turn-on-eldoc-mode))

;; call the preprocessor on the current file
;; only left interactive for debugging purposes
;;
;; run cpp the first time to get macro declarations, the second time
;; to get normal function declarations
(defun c-eldoc-get-buffer (function-name)
  (interactive)
  "Call the preprocessor on the current file"
  ;; run the first time for macros
  (let* ((this-name (concat "*" buffer-file-name "-preprocessed*"))
         (preprocessor-command (concat c-eldoc-cpp-command " "
                                       c-eldoc-cpp-macro-arguments " "
                                       c-eldoc-includes " "
                                       buffer-file-name " | "
                                       c-eldoc-grep "'"
                                       function-name "[ \\t\\n]*('" ))
         (output-buffer (generate-new-buffer this-name)))
    (set-buffer output-buffer)
    (call-process-shell-command preprocessor-command nil output-buffer nil)
    ;; run the second time for normal functions
    (setq preprocessor-command (concat c-eldoc-cpp-command " "
                                       c-eldoc-cpp-normal-arguments " "
                                       c-eldoc-includes " "
                                       buffer-file-name " | "
                                       c-eldoc-grep "'"
                                       function-name "[ \\t\\n]*('" ))
    (call-process-shell-command preprocessor-command nil output-buffer nil)
    output-buffer))

;; finds the current function and position in argument list
(defun c-eldoc-function-and-argument (&optional limit)
  (let* ((literal-limits (c-literal-limits))
         (literal-type (c-literal-type literal-limits)))
    (save-excursion
      ;; if this is a string, move out to function domain
      (when (eq literal-type 'string)
        (goto-char (car literal-limits))
        (setq literal-type nil))
      (if literal-type
          nil
        (c-save-buffer-state ((argument-index 1))
          (while (or (eq (c-forward-token-2 -1 t limit) 0)
                     (when (eq (char-before) ?\[)
                       (backward-char)
                       t))
            (when (eq (char-after) ?,)
              (setq argument-index (1+ argument-index))))
          (c-backward-syntactic-ws)
          (when (eq (char-before) ?\()
            (backward-char)
            (c-forward-token-2 -1)
            (when (looking-at "[a-zA-Z_][a-zA-Z_0-9]*")
              (cons (buffer-substring-no-properties
                     (match-beginning 0) (match-end 0))
                    argument-index))))))))

;; make this extended parameter set into a single line
(defun c-eldoc-format-arguments-string (arguments index)
  (let ((paren-pos (string-match "(" arguments))
        (pos 0))
    (when paren-pos
      (setq arguments (replace-regexp-in-string "\\\\?[[:space:]\\\n]"
                                                " "
                                                (substring arguments paren-pos))
            arguments (replace-regexp-in-string "\\s-+" " " arguments)
            arguments (replace-regexp-in-string " *, *" ", " arguments)
            arguments (replace-regexp-in-string "( +" "(" arguments)
            arguments (replace-regexp-in-string " +)" ")" arguments))
      ;; find the correct argument to highlight, taking `...'
      ;; arguments into account
      (while (and (> index 1)
                  pos
                  (not (string= (substring arguments (+ pos 2) (+ pos 6))
                                "...)")))
        (setq pos (string-match "," arguments (1+ pos))
              index (1- index)))
      ;; embolden the current argument
      (when (and pos
                 (setq pos (string-match "[^ ,()]" arguments pos)))
        (add-text-properties pos (string-match "[,)]" arguments pos)
                             '(face bold) arguments))
      arguments)))

;; master printing function
(defun c-eldoc-print-current-symbol-info ()
  (let* ((current-function-cons (c-eldoc-function-and-argument (- (point) 1000)))
         (current-function (car current-function-cons))
         (current-function-regexp (concat "[ \t\n]+[*]*" current-function "[ \t\n]*("))
         (current-buffer (current-buffer))
         (tag-buffer)
         (function-name-point)
         (arguments)
         (type-face 'font-lock-type-face))
    (when (and current-function
               (not (member current-function c-eldoc-reserved-words)))
      (when (setq tag-buffer (c-eldoc-get-buffer current-function))
        ;; setup the buffer
        (set-buffer tag-buffer)
        (goto-char (point-min))
        (prog1
            ;; protected regexp search
            (when (condition-case nil
                      (re-search-forward current-function-regexp)
                    (error (prog1 nil
                             (message "Function doesn't exist..."))))
              ;; move outside arguments list
              (search-backward "(")
              (c-skip-ws-backward)
              (setq function-name-point (point))
              (forward-sexp)
              (setq arguments (buffer-substring-no-properties
                               function-name-point (point)))
              (goto-char function-name-point)
              (backward-char (length current-function))
              (c-skip-ws-backward)
              (setq function-name-point (point))
              (search-backward-regexp "[};/#]" (point-min) t)
              ;; check for macros
              (if (= (char-after) ?#)
                  (let ((is-define (looking-at "#[[:space:]]*define"))
                        (preprocessor-point (point)))
                    (while (prog2 (end-of-line)
                               (= (char-before) ?\\)
                             (forward-char)))
                    (when (and is-define (> (point) function-name-point))
                      (goto-char preprocessor-point)
                      (setq type-face 'font-lock-preprocessor-face)))
                (forward-char)
                (when (looking-back "//")
                  (end-of-line)))
              (c-skip-ws-forward)
              ;; colorize
              (concat (propertize (buffer-substring-no-properties
                                   (point)
                                   function-name-point)
                                  'face type-face)
                      " "
                      (propertize current-function
                                  'face 'font-lock-function-name-face)
                      " "
                      (c-eldoc-format-arguments-string arguments
                                                       (cdr current-function-cons))))
          (kill-buffer tag-buffer)
          (set-buffer current-buffer))))))

(provide 'c-eldoc)
;;; c-eldoc.el ends here
