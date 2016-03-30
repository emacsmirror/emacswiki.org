;;; replace-string-simulatenously.el - replace several strings at the same time in the same region
;;
;; This program is free software. It comes without any warranty, to
;; the extent permitted by applicable law. You can redistribute it
;; and/or modify it under the terms of the Do What The Fuck You Want
;; To Public License, Version 2, as published by Sam Hocevar. See
;; http://sam.zoy.org/wtfpl/COPYING for more details.
;;
;; Copy of the license text:
;;
;;             DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
;;                     Version 2, December 2004
;;
;;  Copyright (C) 2004 Sam Hocevar <sam@hocevar.net>
;;
;;  Everyone is permitted to copy and distribute verbatim or modified
;;  copies of this license document, and changing it is allowed as long
;;  as the name is changed.
;;
;;             DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
;;    TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION
;;
;;   0. You just DO WHAT THE FUCK YOU WANT TO.
;;
;; Commentary:
;;
;; Changes switches for `dired-sort-toggle-or-edit' command, which is bind in dired to s by defaults.
;;
;;; Code:

(require 'cl)

(defun read-replacement-strings-till-blank-string-to-replace (&rest args)
 (let ((arg1 (read-string "String to replace (leave blank to finish): ")))
  (if (equal arg1 "")
   args
   (let ((arg2 (read-string "Replacement string: ")))
    (apply 'conditional-read arg1 arg2 args)))))

(defun step (n xs)
  (loop for x in xs by (lambda (xs) (nthcdr n xs))
   collect x))

(defun replace-string-simulatenously (&rest args)
 (interactive (read-replacement-strings-till-blank-string-to-replace))
 (let* ((text-selected (and transient-mark-mode mark-active))
        (right-bound (if text-selected (region-end) (buffer-end 1)))
        (to-replace-strings (step 2 args))
        (to-replace-regexp (mapconcat (lambda (arg) (concat "\\(" (regexp-quote arg) "\\)")) to-replace-strings "\\|")))
  (if (and text-selected (< (mark) (point))) (exchange-point-and-mark))
  (while (re-search-forward to-replace-regexp right-bound t)
   (let* ((match-string (match-string 0))
          (replacement-string (lax-plist-get args match-string)))
    (replace-match (regexp-quote replacement-string))))))

(provide 'replace-string-simulatenously)
