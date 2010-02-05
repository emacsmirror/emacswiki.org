;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*- mode: EMACS-LISP; -*-
;;; this is google-define-redux.el
;;; ================================================================
;;; DESCRIPTION:
;;; google-define-redux provides extensions for google-define.el
;;; Extends google-define with fontification of definition buffer.
;;; No longer relies on `with-output-to-temp-buffer'
;;; Adds parsing tokens for programatically extracting definitions.
;;; FUNCTIONS:►►►
;;; `google-define-get-command', `google-define-parse-buffer', `google-define',
;;; `google-define-font-lock',
;;; FUNCTIONS:◄◄◄
;;;
;;; MACROS:
;;;
;;; METHODS:
;;;
;;; CLASSES:
;;;
;;; FACES:
;;; `gg-def-base', `gg-def-delim', `gg-def-defined', `gg-def-num',
;;; `gg-def-inition',
;;;
;;; CONSTANTS:
;;;
;;; VARIABLES:
;;; `*google-define-view-map*', `*url-get-buffer*'
;;;
;;; ALIASED/ADVISED/SUBST'D:
;;;
;;; DEPRECATED:
;;;
;;; RENAMED:
;;;
;;; MOVED:
;;;
;;; TODO:
;;;
;;; NOTES:
;;;
;;; SNIPPETS:
;;;
;;; REQUIRES:
;;;
;;; THIRD-PARTY-CODE:
;;;
;;; AUTHOR: MON KEY
;;; MAINTAINER: MON KEY
;;;
;;; PUBLIC-LINK: (URL `http://www.emacswiki.org/emacs/google-define-redux.el')
;;; FIRST-PUBLISHED: <Timestamp: #{2010-02-04T21:39:15-05:00Z}#{10055} - by MON>
;;;
;;; FILE-CREATED:
;;; <Timestamp: #{2010-02-03T13:41:11-05:00Z}#{10053} - by MON>
;;; ================================================================
;;; This file is not part of GNU Emacs.
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 3, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; see the file COPYING.  If not, write to
;;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;;; Floor, Boston, MA 02110-1301, USA.
;;; ================================================================
;;; Permission is granted to copy, distribute and/or modify this
;;; document under the terms of the GNU Free Documentation License,
;;; Version 1.3 or any later version published by the Free Software
;;; Foundation; with no Invariant Sections, no Front-Cover Texts,
;;; and no Back-Cover Texts. A copy of the license is included in
;;; the section entitled ``GNU Free Documentation License''.
;;; 
;;; A copy of the license is also available from the Free Software
;;; Foundation Web site at:
;;; (URL `http://www.gnu.org/licenses/fdl-1.3.txt').
;;; ================================================================
;;; Copyright © 2010 MON KEY 
;;; ==============================
;;; CODE:

(eval-when-compile (require 'cl))
(require 'google-define)
(require 'mon-doc-help-utils)

;;; ==============================
;;; :CREATED <Timestamp: #{2010-02-04T20:22:51-05:00Z}#{10055} - by MON>
(defface gg-def-base
    '((t :inherit mon-help-KEY-tag))
  "*
:SEE-ALSO `gg-def-base', `gg-def-delim', `gg-def-defined', `gg-def-num',
`gg-def-inition'.\n►►►")
;;
(defface gg-def-delim
    '((t :inherit mon-help-META-tag))
"*
:SEE-ALSO `gg-def-base', `gg-def-delim', `gg-def-defined', `gg-def-num',
`gg-def-inition'.\n►►►")
;;
(defface gg-def-num
    '((t :inherit mon-help-PNTR-tag))
  "*
:SEE-ALSO `gg-def-base', `gg-def-delim', `gg-def-defined', `gg-def-num',
`gg-def-inition'.\n►►►")
;;
(defface gg-def-inition
    '((t ;:inherit gg-def-base
       :foreground "wheat1"
       :weight normal))
  "*
:SEE-ALSO `gg-def-base', `gg-def-delim', `gg-def-defined', `gg-def-num',
`gg-def-inition'.\n►►►")
;;
;;;(makunbound 'gg-def-inition) (unintern 'gg-def-inition)
;;
(defface gg-def-defined
    '((t :inherit mon-help-DYNATAB-tag))
  "*
:SEE-ALSO `gg-def-base', `gg-def-delim', `gg-def-defined', `gg-def-num',
`gg-def-inition'.\n►►►")

;;; ==============================
;;; :CREATED <Timestamp: #{2010-02-03T16:20:02-05:00Z}#{10053} - by MON>
(defvar *google-define-view-map* 
  (cons (string-to-char "Q") 'View-kill-and-leave)
  "Keybinding for `view-mode' in google-define help buffers.")

;;; ==============================
;;; :CREATED <Timestamp: #{2010-02-03T13:43:28-05:00Z}#{10053} - by MON>
(defvar *url-get-buffer* nil 
  "*Name of buffer `google-define-get-command' should return results in.")
;;
(unless (bound-and-true-p *url-get-buffer*)
  (setq *url-get-buffer* (symbol-name '*url-get-buffer*)))


;;; ==============================
;;; :MODIFICATIONS <Timestamp: #{2010-02-04T21:28:01-05:00Z}#{10055} - by MON>
(defun google-define-get-command (host path)
  (let* ((timeout 180)
         (port 80) ;http
         (post-cmd
          (concat "GET " path " HTTP/1.0\r\n"
                  "Host: " host "\r\n"
                  "User-Agent: Emacs\r\n"
;                 "User-Agent: Mozilla/5.0 (Windows; U; Windows NT 5.0; en-US; rv:1.8.1.1) Gecko/20061204 Firefox/2.0.0.1\r\n"
                  "Accept: text/xml,application/xml,application/xhtml+xml,text/html;q=0.9,text/plain;q=0.8,image/png,*/*;q=0.5\r\n"
                  "Accept-Language: en-us,en;q=0.5\r\n"
                  "Accept-Encoding: gzip,deflate\r\n"
                  "Accept-Charset: ISO-8859-1;q=0.7,*;q=0.7\r\n"
                  "\r\n"))
         proc buf)
    (unwind-protect
        (progn
          (when (get-buffer *url-get-buffer*)
            (kill-buffer *url-get-buffer*))
          (setq proc (open-network-stream "url-get-command"
                                          *url-get-buffer* ;; "*url-get-buffer*"
                                          host
                                          port)
                buf (process-buffer proc))
          (process-send-string proc post-cmd)
          (message "Getting information from %s: waiting for response..." host)

          (while (equal (process-status proc) 'open)
            (unless (accept-process-output proc timeout)
              (delete-process proc)
              (error "Server error: timed out while waiting!")))
          (message "Response received: processing...")))
    ;; unwind-protect
    buf))

;;; ==============================
;;; :MODIFICATIONS <Timestamp: #{2010-02-03T13:41:22-05:00Z}#{10053} - by MON>
(defun google-define-parse-buffer (search-word def-disp-buffer)
  "Pull all of the definitions out of the data returned from google, and print
in a temp-buffer.
:SEE-ALSO .\n►►►"
  (let* ((count 0)
         (accum-buffer  ;; <- *standard-output* is here
          (prog1 def-disp-buffer
            (when (buffer-live-p (get-buffer def-disp-buffer))
              (with-current-buffer (get-buffer def-disp-buffer)
                (let ((buffer-read-only nil))
                  (erase-buffer))))))
         ;; :NOTE `accum-buffer' _should_ be empty, don't bother checking.
         (standard-output (get-buffer-create accum-buffer)))
    (princ (format "Definitions for: %s\n\n" search-word))
    (set-buffer (google-define-get-command 
                 "www.google.com" (concat "/search?num=100&hl=en&q=define%3A%22"
                                          (replace-regexp-in-string " +" "+" search-word)
                                          "%22&btnG=Search")))
    (unwind-protect
      (goto-char (point-min))
      (while (search-forward-regexp "<li>\\([^<]+\\)" nil t)
        (incf count)
        (let ((definition (replace-regexp-in-string "\\(\n\\|\r\\|\t\\)" "" (match-string 1))))
          (princ         ;<- spitting to our *Definitions:<SEARCH-WORD>* buffer.
           (with-temp-buffer
             (let ((>pnt (make-marker))
                   (<pnt (make-marker))
                   (fill-column 68)
                   (fill-prefix "    | "))
               (save-excursion              
                 (insert (format "%3d ►\n" count))               
                 (set-marker >pnt (point))
                 (insert (replace-regexp-in-string "^    " "    | " 
                                                   (mon-string-justify-left definition 64 4)))
                 (set-marker <pnt (point))
                 (insert "\n    ◄\n\n"))
               (fill-region-as-paragraph >pnt <pnt)
               (google-define-replace-html)
               (google-define-replace-unicode))
             ;; mon-help-PNTR-tag
             (buffer-substring (buffer-end 0) (buffer-end 1))))))
      ;; Close current TCP process of current `google-define-get-command' buffer.
        (delete-process nil))
    (set-buffer accum-buffer)
    ;; Make _sure_ we closed the process.
    (when (get-buffer *url-get-buffer*) (kill-buffer *url-get-buffer*))
    (setq accum-buffer 
          (with-current-buffer accum-buffer 
            (google-define-font-lock search-word)
            (buffer-substring (buffer-end 0) (buffer-end 1))))
    ;; Put name of `accum-buffer' on the `count' var and kill that buffer too. 
    (setq count (current-buffer))
    (when (get-buffer count) (kill-buffer count))
    accum-buffer))

;;; ==============================
;;; :CREATED <Timestamp: #{2010-02-03T13:41:17-05:00Z}#{10053} - by MON>
(defun google-define (search-word &optional intrp)
  "Ask google for the definition of a word.
If we have a current region use it's value as the default.
:SEE-ALSO .\n►►►"
  (interactive "i\np")
  (let* ((sw (cond (search-word search-word)
                   (intrp (read-from-minibuffer "Define: "
                                                (thing-at-point 'word)))))
         (header (concat "Definitions for " sw))
         (dfb (concat "*" search-word ":definitions*"))
         (defs (google-define-parse-buffer sw dfb)))
    (prog1 (message header)
      (mon-help-temp-docstring-display defs dfb t))))


;;; ==============================
;;; :CREATED <Timestamp: #{2010-02-04T21:33:14-05:00Z}#{10055} - by MON>
(defun google-define-font-lock (search-word)
  "
:SEE-ALSO .\n►►►"
  (let* ((help-props
          `(("^Definitions for:" 0 (face gg-def-base)) ;; gg-def-hdr-def
            ("^ +\\([0-9]\\{1,2\\}\\) " 1 (face gg-def-num))
            (" \\(►\\||\\|◄\\) ?" 1 (face gg-def-delim))
            ("^    | \\(.*\\)$" 1 (face gg-def-inition))
            (,(concat " \\(" search-word "\\)[\\[:punct:]\\[:blank:]\n]") 1 (face gg-def-defined)))
             ))
      (mapc #'(lambda (x)
                (goto-char (buffer-end 0))
                (while (search-forward-regexp  (elt x 0) nil t)
                  (add-text-properties 
                   (match-beginning (elt x 1)) (match-end (elt x 1)) 
                   (elt x 2))))
            help-props)
      ;;(font-lock-fontify-buffer)
      ))

;;; ==============================
(provide 'google-define-redux)
;;; ==============================

;;; ================================================================
;;; google-define-redux.el ends here
;;; EOF
