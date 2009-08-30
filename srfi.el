;;; srfi.el --- View Scheme requests for implementation

;; Copyright (C) 2005  Jorgen Schaefer

;; Version: 1.0
;; Keywords: scheme, documentation, programming
;; Author: Jorgen Schaefer
;; URL: http://www.emacswiki.org/elisp/srfi.el

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

;; This file provides a functionality to view SRFIs, Scheme Requests
;; for Implementation, using a simple M-x srfi command. To update the
;; local SRFI cache, use M-x srfi-update-cache

;; The idea and some code ideas are from Neil W. Van Dykes quack.el.

;;; Code:

(require 'url)

(defcustom srfi-cache-format "~/.srfi/%s-srfis.html"
  "*Where to store cached SRFI index files.
This is given as an argument to `format', with the single other
argument being a symbol which indicates the type of the cache:
'final, 'draft or 'withdrawn."
  :type 'string
  :group 'scheme)

(defun srfi (num)
  "View SRFI number NUM using the configured browser."
  (interactive (list (string-to-number
                      (completing-read "View SRFI number: "
                                       (srfi-list)
                                       nil nil nil "-1"))))
  (when (>= num 0)
    (browse-url (srfi-num-url num))))

(defun srfi-num-url (num)
  "Return the URL for the SRFI number NUM."
  (format "http://srfi.schemers.org/srfi-%d/srfi-%d.html" num num))

(defvar srfi-cache 'invalid
  "The list of possible SRFIs, or 'invalid if none yet.")

(defun srfi-list ()
  "Return a list of possible SRFIs."
  (when (eq srfi-cache 'invalid)
    (setq srfi-cache (srfi-parse)))
  srfi-cache)

(defun srfi-parse ()
  "Return a list of possible SRFIs from the files according to
`srfi-cache-format'."
  (let ((srfis '()))
    (mapc (lambda (kind)
            (let* ((file (format srfi-cache-format kind))
                   (buf (get-file-buffer file))
                   (visitedp buf))
              (when (not buf)
                (setq buf (find-file-noselect file t t)))
              (save-excursion
                (set-buffer buf)
                (setq srfis
                      (append srfis
                              (srfi-parse-buffer kind))))
              (when (not visitedp)
                (kill-buffer buf))))
          '(final draft withdrawn))
    (mapcar (lambda (entry)
              (format "%3d  %s" (car entry) (cdr entry)))
            (sort srfis
                  (lambda (a b)
                    (< (car a)
                       (car b)))))))

(defun srfi-parse-buffer (kind)
  "Return a list of possible SRFIs from the current buffer."
  (let ((prefix (cond
                 ((eq kind 'final) "")
                 ((eq kind 'draft) "[draft] ")
                 ((eq kind 'withdrawn) "[withdrawn] ")))
        (srfis '()))
    (goto-char (point-min))
    (while (re-search-forward (rx "<LI><A HREF="
                                  (? "\"") "srfi-" (+ digit) (? "/") (? "\"")
                                  ">"
                                  "SRFI" (+ space)
                                  (submatch (+ digit))
                                  "</A>" (? ":") (* space)
                                  (submatch (+ not-newline))
                                  eol)
                              nil
                              t)
      (setq srfis
            (cons (cons (string-to-number (match-string-no-properties 1))
                        (concat prefix (match-string-no-properties 2)))
                  srfis)))
    srfis))

(defun srfi-update-cache ()
  "Update the local cached SRFI index files."
  (interactive)
  (mapc (lambda (kind)
          (srfi-url-to-file (format "http://srfi.schemers.org/%s-srfis.html"
                                    kind)
                            (format srfi-cache-format
                                    kind)))
        '(final draft withdrawn))
  (setq srfi-cache 'invalid))

(defun srfi-url-to-file (url file)
  "Retrieve URL to a local FILE."
  (let ((buf (url-retrieve-synchronously url)))
    (save-excursion
      (set-buffer buf)
      (goto-char (point-min))
      (re-search-forward "\n\n" nil t)
      (write-region (point) (point-max) file))
    (kill-buffer buf)))

(provide 'srfi)
;;; srfi.el ends here
