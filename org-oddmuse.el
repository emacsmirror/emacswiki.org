;;; org-oddmuse.el --- Transform from Org-mode to Oddmuse Wiki format.

;; Filename: org-oddmuse.el
;; Description: Transform from Org-mode to Oddmuse Wiki format.
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2009, Andy Stewart, all rights reserved.
;; Created: 2009-03-09 16:50:50
;; Version: 0.1
;; Last-Updated: 2009-03-09 16:50:50
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/org-oddmuse.el
;; Keywords: org-mode, oddmuse
;; Compatibility: GNU Emacs 23.0.60.1
;;
;; Features that might be required by this library:
;;
;; `org'
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
;; Transform from Org-mode to Oddmuse Wiki format.
;;
;; Use commands transform Org-mode text, and yank will get transform result.
;;

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `org-oddmuse-transform-subtree'
;;    Transform Org-mode subtree to Oddmuse Wiki format.
;;  `org-oddmuse-transform-current-buffer'
;;    Transform current Org-mode buffer to Oddmuse Wiki format.
;;  `org-oddmuse-transform-buffer'
;;    Transform buffer to Oddmuse Wiki format.
;;

;;; Installation:
;;
;; Put org-oddmuse.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'org-oddmuse)
;;
;; No need more.

;;; Customize:
;;
;; `org-oddmuse-transform-level'
;; The level of transform heading.
;; If is nil, will transform all Org-mode heading with Oddmuse Wiki format.
;; If is a number, just transform heading level that use specify (index from 1).
;;
;; All of the above can customize by:
;;      M-x customize-group RET org-oddmuse RET
;;

;;; Change log:
;;
;; 2009/03/09
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
(require 'org)

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Customize ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup org-oddmuse nil
  "Transform from Org-mode to Oddmuse Wiki format."
  :group 'edit)

(defcustom org-oddmuse-transform-level nil
  "The level of transform heading.
If is nil, will transform all Org-mode heading with Oddmuse Wiki format.
If is a number, just transform heading level that use specify (index from 1)."
  :type 'number
  :group 'org-oddmuse)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Interactive Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun org-oddmuse-transform-subtree (level)
  "Transform Org-mode subtree to Oddmuse Wiki format.
And get result form yank.
If LEVEL is non-nil, just transform heading level user specify."
  (interactive "P")
  (org-copy-special)
  (kill-new (org-oddmuse-transform (current-kill 0) level)))

(defun org-oddmuse-transform-current-buffer (level)
  "Transform current Org-mode buffer to Oddmuse Wiki format.
And get result form yank.
If LEVEL is non-nil, just transform heading level user specify."
  (interactive "P")
  (kill-new (org-oddmuse-transform (buffer-string) level)))

(defun org-oddmuse-transform-buffer (level buffer)
  "Transform buffer to Oddmuse Wiki format.
And get result form yank.
If LEVEL is non-nil, just transform heading level user specify.
BUFFER is buffer for transform."
  (interactive "P\nbBuffer: ")
  (with-current-buffer buffer
    (kill-new (org-oddmuse-transform (buffer-string) level))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Utilities Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun org-oddmuse-transform (org-string &optional level)
  "Transform from Org-mode to Oddmuse Wiki format.
ORG-STRING is string with Org-mode string.
If LEVEL is non-nil, just transform heading level user specify."
  (let (heading-content
        heading-level
        heading-level-search-regexp
        content-indent-start
        content-indent-end)
    ;; Transform string to Oddmuse wiki format.
    (with-temp-buffer
      ;; Load `org-mode'.
      (org-mode)
      ;; Insert Org-mode string.
      (insert org-string)
      ;; Indent Org-mode content with Oddmuse Wiki `source' format.
      (goto-char (point-min))
      (while (re-search-forward "^\\*+\\s-" nil t)
        ;; Get heading level.
        (setq heading-level (length (replace-regexp-in-string "\\s-+" "" (match-string 0))))
        (save-excursion
          ;; Move to end of line.
          (move-end-of-line 1)
          ;; Get content under current heading.
          (save-excursion
            (setq heading-content
                  (buffer-substring-no-properties (point)
                                                  (progn
                                                    ;; Move to end of heading.
                                                    (org-end-of-heading)
                                                    (point)))))
          (if (and (equal (string-match "^\\s-*$" heading-content) 0)
                   (equal (match-string 0 heading-content) heading-content))
              ;; Move to end of heading when content is avoid.
              (org-end-of-heading)
            ;; Otherwise transform content with Oddmuse Wiki format.
            ;; Format as Oddmuse Wiki `source' format.
            (newline)
            (insert "{{{")
            (save-excursion             ;record indent start position.
              (forward-line +1)
              (setq content-indent-start (point)))
            (org-end-of-heading)        ;move end of heading
            (newline)
            (insert "}}}")
            (save-excursion             ;record indent end position.
              (forward-line -1)
              (setq content-indent-end (point)))
            ;; Indent `source' area.
            (setq content-indent-end (copy-marker content-indent-end))
            (save-excursion
              (goto-char content-indent-start)
              (while (<= (point) content-indent-end)
                ;; Indent space with per line.
                (insert (make-string (* heading-level 4) ? ))
                (forward-line +1))))))
      ;; Get search regexp of transform heading.
      (setq heading-level-search-regexp
            (cond ((and level           ;if `level' above 0
                        (> level 0))
                   (format "^\\*\\{1,%s\\}\\s-+" level))
                  ((and org-oddmuse-transform-level ;if `org-oddmuse-transform-level' above 0
                        (> org-oddmuse-transform-level 0))
                   (format "^\\*\\{1,%s\\}\\s-+" org-oddmuse-transform-level))
                  (t "^\\*+\\s-+")))
      ;; Transform Org-mode heading to Oddmuse Wiki heading.
      (goto-char (point-min))
      (while (re-search-forward heading-level-search-regexp nil t)
        ;; Get heading level.
        (setq heading-level (length (replace-regexp-in-string "\\s-+" "" (match-string 0))))
        ;; Delete "*".
        (kill-region (point) (line-beginning-position))
        ;; Replace as "=".
        (insert (make-string (1+ heading-level) ?=))
        (move-end-of-line 1)
        (insert (make-string (1+ heading-level) ?=)))
      ;; Return transformed string.
      (buffer-string))))

(unless (fboundp 'org-end-of-heading)
  (defun org-end-of-heading ()
    "Move end of current heading."
    (interactive)
    (org-back-to-heading t)
    (outline-next-heading)
    (if (bolp)
        (forward-char -1))))

(provide 'org-oddmuse)

;;; org-oddmuse.el ends here

;;; LocalWords:  subtree nbBuffer
