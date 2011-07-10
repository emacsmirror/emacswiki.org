;;; gtk2hs.el --- This is tools package for gtk2hs

;; Filename: gtk2hs.el
;; Description: This is tools package for gtk2hs
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2010, Andy Stewart, all rights reserved.
;; Created: 2010-03-21 03:13:45
;; Version: 0.2
;; Last-Updated: 2010-03-21 03:13:45
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/gtk2hs.el
;; Keywords: gtk2hs
;; Compatibility: GNU Emacs 23.0.60.1
;;
;; Features that might be required by this library:
;;
;;
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
;; This is tools package for gtk2hs
;;
;; Because gtk2hs have *huge* code/document need convert.
;; So i wrote those elisp package for convert quickly in Emacs.
;;
;; This tool must be use with w3m.
;;

;;; Installation:
;;
;; Put gtk2hs.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'gtk2hs)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET gtk2hs RET
;;

;;; Change log:
;;
;; 2010/03/21
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


;;; Code:

(defvar gtk2hs-replace-type-list '(("TRUE" . "True")
                                   ("FALSE" . "False")
                                   ("NULL" . "Nothing"))
  "C Type list for replace.")

(defvar gtk2hs-replace-symbol-list '(("●" . "*") ; ● can't convert by haddock
                                     )
  "Documentation symbol list for replace.")

(defun gtk2hs-format-docs ()
  "Format C document to Haskell style."
  (interactive)
  (let (transform-start
        transform-end
        transform-doc)
    ;; Get region string, or all buffer if region not active.
    (if (region-active-p)
        (setq transform-start (region-beginning)
              transform-end (region-end))
      (setq transform-start (point-min)
            transform-end (point-max)))
    ;; Keep current edit status and position.
    (save-excursion
      ;; Get transform documentation.
      (setq transform-doc
            (buffer-substring-no-properties transform-start
                                            transform-end))
      ;; Do transform in temp buffer.
      (message "Transforming doc...")
      (with-temp-buffer
        ;; Insert documentation that need transform.
        (insert transform-doc)
        ;; Fill buffer with special column.
        (fill-region (point-min) (point-max))
        ;; Format docs.
        (gtk2hs-format-docs-internal)
        ;; Comment docs.
        (gtk2hs-comment-docs)
        ;; Record new docs.
        (setq transform-doc (buffer-string))
        )
      ;; Copy transform documentation to yank
      (kill-new transform-doc)
      (message "Transforming functions ...done, use C-y to insert Haskell style functions.")
      )))

(defun gtk2hs-format-fun ()
  "Interactive function for pick up function name."
  (interactive)
  (let (transform-start
        transform-end
        transform-doc)
    (if (region-active-p)
        (setq transform-start (region-beginning)
              transform-end (region-end))
      (setq transform-start (line-beginning-position)
            transform-end (line-end-position)))
    ;; Get transform documentation.
    (setq transform-doc (buffer-substring-no-properties transform-start
                                                        transform-end))
    (kill-new (gtk2hs-format-fun-internal transform-doc))
    (message "Pick up function done, use C-y to insert Haskell function name.")))

(defun gtk2hs-format-args ()
  "interactive function for pick up function arguments."
  (interactive)
  (let (transform-start
        transform-end
        transform-arg
        transform-body
        transform-doc)
    ;; Get region string, or all buffer if region not active.
    (if (region-active-p)
        (setq transform-start (region-beginning)
              transform-end (region-end))
      (setq transform-start (point-min)
            transform-end (point-max)))
    ;; Keep current edit status and position.
    (save-excursion
      ;; Get transform documentation.
      (setq transform-doc
            (buffer-substring-no-properties transform-start
                                            transform-end))
      ;; Do transform in temp buffer.
      (message "Transforming arguments...")
      (with-temp-buffer
        ;; Insert documentation that need transform.
        (insert transform-doc)
        ;; Pick argument and docs body.
        (goto-char (point-min))
        (while (not (eobp))
          (if (search-forward-regexp "\\(^[a-zA-Z_]+\\) :\\(.*$\\)" nil t)
              (let (arg-name arg-body)
                (setq transform-arg (match-string 1))
                (setq transform-body (match-string 2)))
            (goto-char (point-max))))
        ;; Format args.
        (setq transform-arg (if (string-equal transform-arg "Returns")
                                "returns"
                              (concat "@" (gtk2hs-format-variable-name transform-arg) "@")))
        ;; Format body.
        (with-temp-buffer
          (insert transform-body)
          (gtk2hs-format-docs-internal)
          (setq transform-body (buffer-string)))
        ;; Record new docs.
        (setq transform-doc (concat " -- ^ " transform-arg transform-body))
        )
      ;; Copy transform documentation to yank
      (kill-new transform-doc)
      (message "Transforming arguments ...done, use C-y to insert Haskell style arguments.")
      )))

(defun gtk2hs-format-function-name (str)
  "This is format C function name with Haskell style.
And C function name match regexp : [a-zA-Z]*_[a-zA-Z_]*[ ]?() "
  (let (fun-list fun-name result) ;; Pick up function name.
    ;; From `gtk_text_buffer_new()` to ("text" "buffer" "new()")
    (setq fun-list (cdr (split-string str "_")))
    ;; Capitalize function name, get : textBufferNew()
    (setq fun-name (case (length fun-list)
                     (1 (car fun-list))
                     (2 (concat (car fun-list) (capitalize (cadr fun-list))))
                     (t (concat (car fun-list) (mapconcat 'capitalize (cdr fun-list) "")))))
    ;; Remove '()', get : textBufferNew
    (car (split-string fun-name "()"))))

(defun gtk2hs-format-variable-name (str)
  "This is format C variable name with Haskell style."
  (let (fun-list fun-name result) ;; Pick up variable name.
    ;; From group_cycling to ("group" "cycling")
    (setq fun-list (split-string str "_"))
    ;; Capitalize variable name, get : groupCycling
    (setq fun-name (case (length fun-list)
                     (1 (car fun-list))
                     (2 (concat (car fun-list) (capitalize (cadr fun-list))))
                     (t (concat (car fun-list) (mapconcat 'capitalize (cdr fun-list) "")))))))

(defun gtk2hs-format-signal-name (str)
  "This is format C signal name with Haskell style."
  (let (fun-list fun-name result) ;; Pick up signal name.
    ;; From group-cycling to ("group" "cycling")
    (setq fun-list (split-string str "-"))
    ;; Capitalize signal name, get : groupCycling
    (setq fun-name (case (length fun-list)
                     (1 (car fun-list))
                     (2 (concat (car fun-list) (capitalize (cadr fun-list))))
                     (t (concat (car fun-list) (mapconcat 'capitalize (cdr fun-list) "")))))))

(defun gtk2hs-comment-docs ()
  "Comment documentation with Haskell style."
  ;; Comment first line with `-- | `
  (goto-char (point-min))               ; go first line first
  (setq comment-start "-- | ")
  (comment-region (point-min) (line-end-position))
  ;; Comment rest doc with `-- `
  (forward-line +1)                     ; move to next line
  (unless (eobp)
    (progn
      (setq comment-empty-lines t)      ; comment empty lines.
      (setq comment-start "-- ")
      (comment-region (point) (point-max))))
  ;; Fill blank line with "--"
  (goto-char (point-min))
  (while (and (search-forward-regexp "^$" nil t)
              (not (eobp)))
    ;; Just comment when current blank line not last line.
    (insert comment-start))
  ;; Replace "-- " with "--"
  (goto-char (point-min))
  (while (not (eobp))
    (if (search-forward-regexp "^--\s+$" nil t)
        (let (temp-str version-str)
          (setq temp-str (match-string 0))
          (delete-region (point) (- (point) (length temp-str)))
          (insert "--"))
      (goto-char (point-max)))))

(defun gtk2hs-format-docs-internal ()
  "Internal functions for format document."
  ;; Don't ignore case
  (setq case-fold-search nil)
  ;; Transform function name to Haskell style.
  ;; `gtk_text_buffer_new()` to 'textBufferNew'.
  (goto-char (point-min))
  (while (not (eobp))
    (if (search-forward-regexp "[a-zA-Z]+_[a-zA-Z_]*[ ]?(.*)" nil t)
        (let (temp-str)
          (setq temp-str (match-string 0))
          (delete-region (point) (- (point) (length temp-str)))
          (insert (concat "'" (gtk2hs-format-function-name temp-str) "'")))
      (goto-char (point-max))))
  ;; Transform `gtk_bla_foo` to `blaFoo`.
  (goto-char (point-min))
  (while (not (eobp))
    (if (search-forward-regexp "\\(gtk\\|gdk\\)_[a-zA-Z_]*" nil t)
        (let (temp-str type-name)
          (setq temp-str (match-string 0))
          (delete-region (point) (- (point) (length temp-str)))
          (insert (concat "'" (gtk2hs-format-fun-internal (concat temp-str "()")) "'")))
      (goto-char (point-max))))
  ;; Transform signal name.
  ;; GtkWidget::realize to 'realize'
  (goto-char (point-min))
  (while (not (eobp))
    (if (search-forward-regexp "[a-zA-Z]*::\\([a-zA-Z-]+\\)" nil t)
        (let (temp-str signal-name)
          (setq temp-str (match-string 0))
          (setq signal-name (match-string 1))
          (delete-region (point) (- (point) (length temp-str)))
          (insert (concat "'" (gtk2hs-format-signal-name signal-name) "'")))
      (goto-char (point-max))))
  ;; Transform signal name.
  ;; From "mnemonic-activate" signal to 'mnemonicAcivate' signal.
  (goto-char (point-min))
  (while (not (eobp))
    (if (search-forward-regexp "\"\\([a-zA-Z]+-[a-zA-Z-]*[a-zA-Z]+\\)\" signal" nil t)
        (let (temp-str signal-name)
          (setq temp-str (match-string 0))
          (setq signal-name (match-string 1))
          (delete-region (point) (- (point) (length temp-str)))
          (insert (concat "'" (gtk2hs-format-signal-name signal-name) "'" " signal")))
      (goto-char (point-max))))
  ;; Transform rest "foo-bar" to 'fooBar'
  (goto-char (point-min))
  (while (not (eobp))
    (if (search-forward-regexp "\"\\([a-zA-Z]+-[a-zA-Z-]*[a-zA-Z]+\\)\"" nil t)
        (let (temp-str signal-name)
          (setq temp-str (match-string 0))
          (setq signal-name (match-string 1))
          (delete-region (point) (- (point) (length temp-str)))
          (insert (concat "'" (gtk2hs-format-signal-name signal-name) "'")))
      (goto-char (point-max))))
  ;; Transform enum name
  ;; FOO_BAR to 'FooBar'
  (goto-char (point-min))
  (while (not (eobp))
    (if (search-forward-regexp "[A-Z]+_[A-Z_]*[A-Z]+" nil t)
        (let (temp-str signal-name)
          (setq temp-str (match-string 0))
          (delete-region (point) (- (point) (length temp-str)))
          (insert (concat (mapconcat 'capitalize (split-string temp-str "_") ""))))
      (goto-char (point-max))))
  ;; Transform variable name.
  ;; From group_cycling to @groupCycling@
  (goto-char (point-min))
  (while (not (eobp))
    (if (search-forward-regexp "[a-z]+_[a-z_]*[a-z]+" nil t)
        (let (temp-str signal-name)
          (setq temp-str (match-string 0))
          (delete-region (point) (- (point) (length temp-str)))
          (insert (concat "@" (gtk2hs-format-variable-name temp-str) "@")))
      (goto-char (point-max))))
  ;; Transform `GtkFooBar` to 'FooBar'
  (goto-char (point-min))
  (while (not (eobp))
    (if (search-forward-regexp "\\(Gtk\\|Gdk\\)\\([A-Z].[a-zA-Z]*\\)" nil t)
        (let (temp-str type-name)
          (setq temp-str (match-string 0))
          (setq type-name (match-string 2))
          (delete-region (point) (- (point) (length temp-str)))
          (insert (concat "'" type-name "'")))
      (goto-char (point-max))))
  ;; Convert version string.
  (goto-char (point-min))
  (while (not (eobp))
    (if (search-forward-regexp "^Since\s\\([0-9\.]+\\)" nil t)
        (let (temp-str version-str)
          (setq temp-str (match-string 0))
          (setq version-str (match-string 1))
          (delete-region (point) (- (point) (length temp-str)))
          (insert (concat "* Available since Gtk+ version " version-str "\n")))
      (goto-char (point-max))))
  ;; Replace type.
  (dolist (type gtk2hs-replace-type-list)
    (goto-char (point-min))
    (while (not (eobp))
      (if (search-forward (car type) nil t)
          (let (temp-str signal-name)
            (setq temp-str (match-string 0))
            (delete-region (point) (- (point) (length temp-str)))
            (insert (concat "'" (cdr type) "'")))
        (goto-char (point-max)))))
  ;; Replace symbol.
  (dolist (type gtk2hs-replace-symbol-list)
    (goto-char (point-min))
    (while (not (eobp))
      (if (search-forward (car type) nil t)
          (let (temp-str signal-name)
            (setq temp-str (match-string 0))
            (delete-region (point) (- (point) (length temp-str)))
            (insert (cdr type)))
        (goto-char (point-max))))))

(defun gtk2hs-format-fun-internal (str)
  "The internal function of `gtk2hs-format-fun'."
  (with-temp-buffer
    ;; Insert docs first.
    (insert str)
    ;; Convert function name.
    (goto-char (point-min))
    ;; Don't ignore case
    (setq case-fold-search nil)
    (while (not (eobp))
      (if (search-forward-regexp "[a-zA-Z]+_[a-zA-Z_]*[ ]?(.*)" nil t)
          (let (temp-str)
            (setq temp-str (match-string 0))
            (delete-region (point) (- (point) (length temp-str)))
            (insert (gtk2hs-format-function-name temp-str)))
        (goto-char (point-max))))
    ;; Result.
    (replace-regexp-in-string "\\s-" "" (buffer-string)) ; remove blank
    ))

(provide 'gtk2hs)

;;; gtk2hs.el ends here
