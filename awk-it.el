;;; awk-it.el --- Little utility that combines awk and yasnippet.

;; Copyright (C) 2012, by Igor Sikaček

;; Author: Igor Sikaček <isikacek@gmail.com>
;; Maintainer: Igor Sikaček <isikacek@gmail.com>
;; Created: 10 Sep 2012
;; Version: 0.5
;; Keywords: awk

;; This file is not part of Emacs

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; AWK it! sends selected region to awk and uses yasnippet as interactive UI.
;; It supports both simplified(default) and raw AWK syntax and has support for multiple lines.

;; Simplest usage is selecting mark and point and running M-x awk-it. Default
;; field separator is space. AWK it! matches every non empty row in region ($0 !~ /^$/).

;; After invoking command buffer will change to show the following interface:

;; Data: <First line with most fields>
;; AWK pattern: <AWK code; see below; may be multiple lines without extra formatting>
;; <AWK output>

;; Example:

;; From:

;; John 26 London
;; Mark 27 Seattle 50
;; Scott 26 Sydney

;; to (ignoring the extra field in example pattern):

;; Data: Mark 27 Seattle 50
;; AWK pattern: <person name="$1" age="$2">
;;     <location>$3</location>
;; </person>
;; <person name="John" age="26">
;;     <location>London</location>
;; </person>
;; <person name="Mark" age="27">
;;     <location>Seattle</location>
;; </person>
;; <person name="Scott" age="26">
;;     <location>Sydney</location>
;; </person>

;; and after expansion:

;; <person name="John" age="26">
;;     <location>London</location>
;; </person>
;; <person name="Mark" age="27">
;;     <location>Seattle</location>
;; </person>
;; <person name="Scott" age="26">
;;     <location>Sydney</location>
;; </person>

;; Simplified AWK code differs from regular in that it:
;;   - prints code(print "<code>")
;;   - excapes double and single quotes (latter for shell interaction)
;;   - excapes newline
;;   - concatenates fields with rest of the text

;; The previous example written in raw AWK code would be:

;; print "<person name=\"" $1 "\" age=\"" $2 "\">\n"\
;; "    <location>" $3 "</location>\n"\
;; "</person>"

;; AWK it can also be invoked with custom separator: awk-it-with-separator. Combining
;; everything the following functions are available:
;;   - awk-it
;;   - awk-it-raw
;;   - awk-it-with-separator
;;   - awk-it-raw-with-separator

;; Also AWK it! can be customized in External -> Awk it:
;;   - awk-it-load-hook; Hook that gets run after the AWK it! has been loaded
;;   - awk-it-default-separator; Default AWK field separator - if nil AWK default is used
;;   - awk-it-default-row-filter; Default AWK row filter - if nil '$0 !~ /^$/' is used

;;; Change Log:

;; 2012.09.10 - first version

;;; Code:


(require 'yasnippet)


(defgroup awk-it nil
  "Run awk interactively on region."
  :version "0.5"
  :group 'external)


(defcustom awk-it-load-hook nil
  "*Hook that gets run after the awk-it has been loaded."
  :type 'hook
  :group 'awk-it)


(defcustom awk-it-default-separator " "
  "*Default AWK field separator - if nil AWK default is used."
  :type 'string
  :group 'awk-it)


(defcustom awk-it-default-row-filter "$0 !~ /^$/"
  "*Default AWK row filter - if nil '$0 !~ /^$/' is used."
  :type 'string
  :group 'awk-it)


(defvar awk-it-data nil "Data for AWK to process.")

(defvar awk-it-code nil "AWK code to transform data.")

(defvar awk-it-point nil "Begining of data/head.")


(defun awk-it (beg end)
  "Run AWK for each line between point and mark."
  (interactive "r")
  (awk-it-full beg end awk-it-default-separator))


(defun awk-it-with-separator (beg end fs)
  "Run AWK for each line between point and mark, specifying custom field separator."
  (interactive "r\nsAWK it! field separator: ")
  (awk-it-full beg end fs))


(defun awk-it-raw (beg end)
  "Run AWK code(raw) for each line between point and mark."
  (interactive "r")
  (awk-it-full beg end " " t))


(defun awk-it-raw-with-separator (beg end fs code)
  "Run AWK code(raw) for each line between point and mark, specifying custom field separator."
  (interactive "r\nsAWK it! field separator: ")
  (awk-it-full beg end fs t))


(defun awk-it-full (beg end &optional fs raw)
  "AWK it! - full func.; captures data and sets up yasnippet."
  (interactive "r")
  (save-window-excursion
    (setq awk-it-data (buffer-substring beg end)
          awk-it-point beg)
    (add-hook 'yas/after-exit-snippet-hook 'awk-it-yas-completed)
    (yas/expand-snippet beg end (concat
      "Data: " (awk-it-get-line-with-max-separators beg end) "
AWK pattern: ${1:pattern}
${1:$(awk-it-process text "
      "\"" (if fs fs " ") "\""
      (if raw " t")
      ")}"))))



(defun awk-it-process (code &optional fs raw)
  "Sends AWK code and data to the shell."
  (setq awk-it-code code)
  (shell-command-to-string (concat
    "echo \"" awk-it-data
    "\" | awk -v auto_quote=\"'\" ' "
    (if fs (concat " BEGIN { FS = \"" fs "\" }") "")
    " " (if awk-it-default-row-filter awk-it-default-row-filter "$0 !~ /^$/")
    " { "
    (if raw
        (concat code " }'")
      (concat
        "print \""
        (awk-it-n-regex-replace code
          '(("\""                     "\\\\\"")
            ("\\(\\$[1234567890]+\\)" "\" \\1 \"")
            ("
" "\\\\n")
            ("'"                      "\" auto_quote \"")))
      "\" } /^$/ { print }'")))))


(defun awk-it-yas-completed ()
  "After yas completion hook - removes header from buffer and itself from hook."
  (when awk-it-point
    (goto-char awk-it-point)
    (kill-line (+ 2 (count ?
 (string-to-sequence awk-it-code 'list))))
    (setq awk-it-point nil
          awk-it-data nil
          awk-it-code nil))
  (remove-hook 'yas/after-exit-snippet-hook 'awk-it-yas-completed))


(defun awk-it-get-line-with-max-separators (beg end &optional separator)
  "Returns line with max # of separators inside region."
  (save-window-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ((count 0)
            (final-point (point)))
        (while (not (eobp))
          (let ((x (count-matches (if (not separator) " " separator) (line-beginning-position) (line-end-position))))
            (when (< count x)
              (setq count x
                    final-point (line-beginning-position))))
          (forward-line))
        (goto-char final-point)
        (buffer-substring (line-beginning-position) (line-end-position))))))


(defun awk-it-n-regex-replace (string list)
  "Makes multiple regex replacements using ((search replace) . ) syntax."
  (if list
      (destructuring-bind ((a b) . rest) list
        (awk-it-n-regex-replace (replace-regexp-in-string a b string) rest))
    string))


(provide 'awk-it)


(run-hooks 'awk-it-load-hook)


;;; awk-it.el ends here
