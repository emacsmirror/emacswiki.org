;;; buffer-time-stamp.el --- display buffer time stamps in the mode line

;;; Copyright (C) 2004, 2005 Matthew P. Hodges

;; Author: Matthew P. Hodges <MPHodges@member.fsf.org>
;; Version: $Id: buffer-time-stamp.el,v 1.20 2005/06/14 14:37:38 mphodges-guest Exp $

;; buffer-time-stamp.el is free software; you can redistribute it
;; and/or modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.

;; buffer-time-stamp.el is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied warranty
;; of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;;; Commentary:
;;
;; This package displays in the mode line a time stamp for the last
;; text change in a buffer. Use M-x buffer-time-stamp-mode RET to
;; toggle.

;;; Code:

(defconst buffer-time-stamp-version "2.2.0"
  "Version number of this package.")

(defvar buffer-time-stamp nil
  "String used by function `buffer-time-stamp-mode'.")
(make-variable-buffer-local 'buffer-time-stamp)

;; Customizable variables

(defgroup buffer-time-stamp nil
  "Display buffer time stamps in the mode line."
  :group 'tools
  :link '(url-link "http://mph-emacs-pkgs.alioth.debian.org/BufferTimeStampEl.html"))

(defcustom buffer-time-stamp-format nil
  "Time stamp format used for `buffer-time-stamp'.
If a string, the value is passed to `format-time-string'. If a
function, this is evaluated, and should return a string. If nil,
`current-time-string' is used."
  :group 'buffer-time-stamp
;;   :set (lambda (sym val)
;;          (set sym val)
;;          (and (fboundp 'buffer-time-stamp-update)
;;               (dolist (b (buffer-list))
;;                 (with-current-buffer b
;;                   (buffer-time-stamp-update)))))
  :type '(choice (const :tag "Default format (current-time-string)" nil)
                 (string :tag "Custom format")
                 (function :tag "Lisp function name (or lambda expression)")))

(define-minor-mode buffer-time-stamp-mode
  "Toggle Buffer Time-Stamp mode.
This displays in the mode line a time stamp for the last text
change in a buffer."
  nil buffer-time-stamp nil
  (if buffer-time-stamp-mode
      (progn
        ;; In XEmacs, the LOCAL arg in add-hook doesn't make the hook
        ;; variable buffer-local, so the following is needed.
        (make-local-hook 'after-change-functions)
        (add-hook 'after-change-functions 'buffer-time-stamp-update nil t)
        (setq buffer-time-stamp " BTS"))
    (remove-hook 'after-change-functions 'buffer-time-stamp-update t)
    (setq buffer-time-stamp nil)))

(defun buffer-time-stamp-update (&rest ignore)
  "Update `buffer-time-stamp' for the current buffer."
  (when buffer-time-stamp-mode
    (setq buffer-time-stamp
          (format " BTS [%s]"
                  (condition-case nil
                      (cond
                       ((null buffer-time-stamp-format)
                        (current-time-string))
                       ((stringp buffer-time-stamp-format)
                        (format-time-string buffer-time-stamp-format))
                       ((functionp buffer-time-stamp-format)
                        (let ((res (funcall buffer-time-stamp-format)))
                          (if (stringp res) res "?")))
                       (t
                        "?"))
                    (error "!"))))
          (force-mode-line-update)))

(provide 'buffer-time-stamp)

;;; buffer-time-stamp.el ends here
