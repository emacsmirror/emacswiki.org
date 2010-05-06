;;; lispxmp.el --- Automagic emacs lisp code annotation
;; $Id: lispxmp.el,v 1.34 2010/05/06 01:28:32 rubikitch Exp $

;; Copyright (C) 2009, 2010  rubikitch

;; Author: rubikitch <rubikitch@ruby-lang.org>
;; Keywords: lisp, convenience
;; URL: http://www.emacswiki.org/cgi-bin/wiki/download/lispxmp.el

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Automagical annotation of lisp values like Ruby's xmpfilter. For
;; example, executing M-x lispxmp on the following buffer:
;;
;; ====
;; 1 ; =>
;; (+ 3 4) ; =>
;; (dotimes (i 3)
;;   (* i 4)  ; =>
;; )
;; ====
;; 
;; produces
;; 
;; ====
;; 1 ; => 1
;; (+ 3 4) ; => 7
;; (dotimes (i 3)
;;   (* i 4) ; => 0, 4, 8
;; )
;; ====


;;; Commands:
;;
;; Below are complete command list:
;;
;;  `lispxmp'
;;    Annotate value of lines containing `; =>' .
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;
;;  `lispxmp-string-no-properties'
;;    *When non-nil, remove text priperties of strings in annotation.
;;    default = t

;;; Installation:
;;
;; paredit.el is optional. Get it from here:
;; http://mumble.net/~campbell/emacs/paredit.el
;;
;; Put lispxmp.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'lispxmp)
;;
;; No need more.

;;; Customize:
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET lispxmp RET
;;


;;; Bug Report:
;;
;; If you have problem, send a bug report via M-x lispxmp-send-bug-report.
;; The step is:
;;  0) Setup mail in Emacs, the easiest way is:
;;       (setq user-mail-address "your@mail.address")
;;       (setq user-full-name "Your Full Name")
;;       (setq smtpmail-smtp-server "your.smtp.server.jp")
;;       (setq mail-user-agent 'message-user-agent)
;;       (setq message-send-mail-function 'message-smtpmail-send-it)
;;  1) Be sure to use the LATEST version of lispxmp.el.
;;  2) Enable debugger. M-x toggle-debug-on-error or (setq debug-on-error t)
;;  3) Use Lisp version instead of compiled one: (load "lispxmp.el")
;;  4) Do it!
;;  5) If you got an error, please do not close *Backtrace* buffer.
;;  6) M-x lispxmp-send-bug-report and M-x insert-buffer *Backtrace*
;;  7) Describe the bug using a precise recipe.
;;  8) Type C-c C-c to send.
;;  # If you are a Japanese, please write in Japanese:-)

;;; History:

;; $Log: lispxmp.el,v $
;; Revision 1.34  2010/05/06 01:28:32  rubikitch
;; pass pp reexecute #2
;;
;; Revision 1.33  2010/05/06 01:25:51  rubikitch
;; fix tests
;;
;; Revision 1.32  2010/05/06 01:20:42  rubikitch
;; ensure last newline
;;
;; Revision 1.31  2010/05/06 01:16:22  rubikitch
;; pass pp test #5-6
;;
;; Revision 1.30  2010/05/06 01:04:25  rubikitch
;; pass pp reexecute
;;
;; Revision 1.29  2010/05/06 00:52:34  rubikitch
;; silence byte-compiler
;;
;; Revision 1.28  2010/05/06 00:50:06  rubikitch
;; lispxmp-create-code: disable after-change-major-mode-hook
;;
;; Revision 1.27  2010/05/06 00:40:29  rubikitch
;; refactor tests 2
;;
;; Revision 1.26  2010/05/06 00:39:09  rubikitch
;; refactor tests
;;
;; Revision 1.25  2010/05/06 00:36:13  rubikitch
;; pp test3-4 passed
;;
;; Revision 1.24  2010/05/06 00:12:23  rubikitch
;; pp test2 passed
;;
;; Revision 1.23  2010/05/06 00:03:10  rubikitch
;; pp test1 passed
;;
;; Revision 1.22  2010/05/05 23:27:32  rubikitch
;; add flag `use-pp'
;;
;; Revision 1.21  2010/05/05 23:11:13  rubikitch
;; allow more spaces between ";" and "=>"
;;
;; Revision 1.20  2010/05/04 09:01:42  rubikitch
;; Added bug report command
;;
;; Revision 1.19  2010/04/16 12:07:26  rubikitch
;; New algorithm. Fix result compound object such as rings.
;;
;; Revision 1.18  2010/04/16 12:01:02  rubikitch
;; refined test
;;
;; Revision 1.17  2010/04/01 01:21:49  rubikitch
;; `%lispxmp-prin1-to-string-no-properties': newline -> \n
;;
;; Revision 1.16  2010/04/01 01:14:37  rubikitch
;; Fix a testcase
;;
;; Revision 1.15  2010/04/01 00:58:58  rubikitch
;; Fix an bug in cons cell
;;
;; Revision 1.14  2010/03/28 04:52:56  rubikitch
;; * Fix destructive function bug
;; * New command: `lispxmp-debug-buffer'
;;
;; Revision 1.13  2010/03/25 07:10:58  rubikitch
;; Fix escape bug
;;
;; Revision 1.12  2010/03/25 06:14:57  rubikitch
;; New option: `lispxmp-string-no-properties'
;;   When non-nil, remove text priperties of strings in annotation.
;;
;; Revision 1.11  2010/03/23 03:17:57  rubikitch
;; `lispxmp-comment-advice': tiny bug fix
;;
;; Revision 1.10  2010/03/20 21:31:37  rubikitch
;; `=>' check in `comment-dwim' and `paredit-comment-dwim' advice
;;
;; Revision 1.9  2009/03/17 09:23:41  rubikitch
;; Enter debugger if `debug-on-error' is non-nil.
;;
;; Revision 1.8  2009/03/17 08:42:38  rubikitch
;; *** empty log message ***
;;
;; Revision 1.7  2009/03/16 22:02:52  rubikitch
;; Bug fix
;;
;; Revision 1.6  2009/03/16 21:50:42  rubikitch
;; Error handling
;;
;; Revision 1.5  2009/03/16 12:15:00  rubikitch
;; refactoring
;;
;; Revision 1.4  2009/03/16 11:59:37  rubikitch
;; Bug fix. (New algorithm)
;;
;; Revision 1.3  2009/03/16 11:23:05  rubikitch
;; refactoring
;;
;; Revision 1.2  2009/03/06 20:19:06  rubikitch
;; Add missing requires
;;
;; Revision 1.1  2009/03/06 19:02:14  rubikitch
;; Initial revision
;;

;;; Code:

(defvar lispxmp-version "$Id: lispxmp.el,v 1.34 2010/05/06 01:28:32 rubikitch Exp $")
(require 'cl)
(require 'newcomment)
(require 'pp)
(eval-when-compile (require 'paredit nil t))
(defgroup lispxmp nil
  "lispxmp"
  :group 'emacs)

(defcustom lispxmp-string-no-properties t
  "*When non-nil, remove text priperties of strings in annotation.

Need paredit.el.
http://mumble.net/~campbell/emacs/paredit.el"
  :type 'boolean  
  :group 'lispxmp)

(defvar lispxmp-temp-buffer " *lispxmp tmp*")
(defvar lispxmp-results nil)
(defun lispxmp ()
  "Annotate value of lines containing `; =>' ."
  (interactive)
  (let ((pt (point)) (wstart (window-start (selected-window))))
    (lispxmp-create-code (current-buffer))
    (erase-buffer)
    (insert-buffer-substring lispxmp-temp-buffer)
    (unwind-protect
        (if debug-on-error
            (eval-buffer)
          (condition-case err
              (eval-buffer)
            (error
             (ding)
             ;; next action when error
             (run-with-timer 0 nil 'message "Error in eval: %S" err))))
      (lispxmp-create-annotations (current-buffer) lispxmp-results)
      (goto-char pt)
      (set-window-start (selected-window) wstart))))

(defun lispxmp-create-code (buf)
  (setq lispxmp-results nil)
  (with-current-buffer (get-buffer-create lispxmp-temp-buffer)
    (buffer-disable-undo)
    (erase-buffer) 
    (let (emacs-lisp-mode-hook after-change-major-mode-hook) (emacs-lisp-mode))
    (insert-buffer-substring buf)
    (goto-char (point-max))
    (or (bolp) (insert "\n"))
    (goto-char 1)
    (lispxmp-adjust-pp-annotations)
    (lispxmp-add-out-markers)))

(defun lispxmp-adjust-pp-annotations ()
  (save-excursion
    (loop while (re-search-forward "^\\(;+\\)\\( +=> \\)" nil t)
          for next-line-re = (concat
                              "^"
                              (regexp-quote
                               (concat (match-string 1)
                                       (make-string (- (match-end 2) (match-beginning 2))
                                                    ?\s)))
                              ".+\n")
          do
          (forward-line 1)
          (while (looking-at next-line-re)
            (delete-region (point) (progn (forward-line 1) (point)))))))

(defun lispxmp-add-out-markers ()
  (save-excursion
    (loop while (re-search-forward "\\(;+\\) +=>" nil t)
          for use-pp = (eq (point-at-bol) (match-beginning 0))
          for semicolons = (match-string 1)
          for i from 0
          when (lispxmp-annotation-p) do
          (delete-region (match-beginning 0) (point-at-eol))
          (lispxmp-out-make-sexp use-pp (length semicolons) i)
          (insert (format "%s <<%%lispxmp-out-marker %d %d>>"
                          semicolons (length semicolons) i)))))
;; (progn (lispxmp-create-code (current-buffer))(display-buffer lispxmp-temp-buffer))
(defun lispxmp-debug-buffer ()
  (interactive)
  (display-buffer lispxmp-temp-buffer))


(defun lispxmp-annotation-p ()
  (save-match-data
    (save-excursion
      (beginning-of-line)
      (ignore-errors (comment-search-forward (point-at-eol) t))
      (looking-at "=>"))))

(defun lispxmp-out-make-sexp (use-pp semicolons-len i)
  (end-of-line)
  (let ((e (make-marker)))
    (set-marker e (point))
    (forward-sexp -1)
    (insert (format "(%%lispxmp-out %s %d %s " use-pp semicolons-len i))
    (goto-char e)
    (insert ")")))

(defun lispxmp-out-remove ()
  (goto-char (point-min))
  (while (re-search-forward "(%lispxmp-out [a-z]+ [0-9]+ [0-9]+ " nil t)
    (replace-match "")
    (save-excursion
      (forward-sexp)
      (and (search-forward ")" nil t) (replace-match "")))))

(defvar lispxmp-results nil)
(defun %lispxmp-out (use-pp semicolons-len index result)
  (push (cons index (%lispxmp-prin1-to-string use-pp semicolons-len result)) lispxmp-results)
  result)

(defun %lispxmp-prin1-to-string (use-pp semicolons-len object)
  (let ((print-func (if use-pp 'pp-to-string 'prin1-to-string)))
    (with-temp-buffer
      (insert (let (pp-escape-newlines) (funcall print-func object)))
      (goto-char 1)
      (save-excursion
        (when (and lispxmp-string-no-properties
                   (require 'paredit nil t))
          (while (search-forward "#(\"" nil t)
            (forward-char -1)
            (paredit-raise-sexp)
            (delete-backward-char 1)
            (forward-sexp 1))))
      (save-excursion
        (if (eq print-func 'prin1-to-string)
            ;; escape newlines
            (while (search-forward "\n" nil t)
              (replace-match "\\\\n"))
          ;; add paddings
          (goto-char 1)
          (forward-line 1)
          (unless (eobp)
            (string-rectangle (point) (point-max)
                             (concat (make-string semicolons-len ?\;)  "    ")))
          ;; delete last newline
          (goto-char (point-max))
          (and (bolp) (delete-backward-char 1))))
      (buffer-string))))

(defun lispxmp-create-annotations (buf results)
  (set-buffer buf)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\(;+\\) <<%lispxmp-out-marker \\([0-9]+\\) \\([0-9]+\\)>> *$" nil t)
      (let ((index (string-to-number (match-string 3)))
            (semicolons (match-string 1)))
        ;; I do not use `replace-match', because it interprets backslashes.
        ;; Insert replacement string literally.
        (delete-region (match-beginning 0) (match-end 0))
        (insert semicolons
                " => "
                ;; pair := (INDEX . VALUE)
                (mapconcat 'cdr
                           (remove-if-not (lambda (pair) (= index (car pair)))
                                          (reverse results))
                           ", ")))))
  (lispxmp-out-remove))
;; (with-new-window (find-epp lispxmp-results))

(defmacro lispxmp-comment-advice (func)
  `(defadvice ,func (around lispxmp-hack activate)
     ,(format "If `%s' is successively called, add => mark." func)
     (if (and (eq major-mode 'emacs-lisp-mode)
              (eq last-command ',func)
              (not (member "=>" (list (ignore-errors (buffer-substring (- (point) 2) (point)))
                                      (ignore-errors (buffer-substring (point) (+ (point) 2)))))))
         (insert " =>")
       ad-do-it)))
(lispxmp-comment-advice comment-dwim)
(lispxmp-comment-advice paredit-comment-dwim)

;;;; Bug report
(defvar lispxmp-maintainer-mail-address
  (concat "rubiki" "tch@ru" "by-lang.org"))
(defvar lispxmp-bug-report-salutation
  "Describe bug below, using a precise recipe.

When I executed M-x ...

How to send a bug report:
  1) Be sure to use the LATEST version of lispxmp.el.
  2) Enable debugger. M-x toggle-debug-on-error or (setq debug-on-error t)
  3) Use Lisp version instead of compiled one: (load \"lispxmp.el\")
  4) If you got an error, please paste *Backtrace* buffer.
  5) Type C-c C-c to send.
# If you are a Japanese, please write in Japanese:-)")
(defun lispxmp-send-bug-report ()
  (interactive)
  (reporter-submit-bug-report
   lispxmp-maintainer-mail-address
   "lispxmp.el"
   (apropos-internal "^lispxmp-" 'boundp)
   nil nil
   lispxmp-bug-report-salutation))

;;;; unit test
;; (install-elisp "http://www.emacswiki.org/cgi-bin/wiki/download/el-expectations.el")
;; (install-elisp "http://www.emacswiki.org/cgi-bin/wiki/download/el-mock.el")
(dont-compile
  (when (fboundp 'expectations)
    (defun lispxmp-to-string (lispxmp-string-no-properties from)
      (with-temp-buffer
        (insert from)
        (lispxmp)
        (buffer-string)))
    (expectations
      (desc "lispxmp-string-no-properties = t")
      (expect "\"a\\nb\" ; => \"a\\nb\"
"
        (lispxmp-to-string t "\"a\\nb\" ; => "))
      (expect "(propertize \"aaaa\" 'face 'match) ; => \"aaaa\"
"
        (lispxmp-to-string t "(propertize \"aaaa\" 'face 'match) ; => "))
      (expect "
 (list (propertize \"a\" 'face 'match)
 (propertize \"b\" 'face 'match)) ; => (\"a\" \"b\")
"
        (lispxmp-to-string t "
 (list (propertize \"a\" 'face 'match)
 (propertize \"b\" 'face 'match)) ; => "))
      (desc "lispxmp-string-no-properties = nil")
      (expect "\"a\\nb\" ; => \"a\\nb\"
"
        (lispxmp-to-string nil "\"a\\nb\" ; => "))
      (expect "(propertize \"aaaa\" 'face 'match) ; => #(\"aaaa\" 0 4 (face match))
"
        (lispxmp-to-string nil "(propertize \"aaaa\" 'face 'match) ; => "))
      (expect "
 (list (propertize \"a\" 'face 'match)
 (propertize \"b\" 'face 'match)) ; =>\
 (#(\"a\" 0 1 (face match)) #(\"b\" 0 1 (face match)))
"
        (lispxmp-to-string nil "
 (list (propertize \"a\" 'face 'match)
 (propertize \"b\" 'face 'match)) ; => "))
      (desc "destructive annotation test")
      (expect "
         (setq l (list 1 2)) ; => (1 2)
         (setcar l 100)      ; => 100
         l                   ; => (100 2)
"
        (lispxmp-to-string t "
         (setq l (list 1 2)) ; =>
         (setcar l 100)      ; =>
         l                   ; =>
"))
      (expect "
         (setq s (copy-sequence \"abcd\")) ; => \"abcd\"
         (aset s 0 ?A)                     ; => 65
         s                                 ; => \"Abcd\"
"
        (lispxmp-to-string t "
         (setq s (copy-sequence \"abcd\")) ; =>
         (aset s 0 ?A)                     ; =>
         s                                 ; =>
"))
      (expect "
         (setq c (cons 1 2)) ; => (1 . 2)
         (setcar c 100)      ; => 100
         c                   ; => (100 . 2)
"
        (lispxmp-to-string t "
         (setq c (cons 1 2)) ; =>
         (setcar c 100)      ; =>
         c                   ; =>
"))
      (desc "pp test")
      (expect "'((\"a\") \"b\" (\"c\"))
;;; => ((\"a\")
;;;     \"b\"
;;;     (\"c\"))
"
        (lispxmp-to-string t "'((\"a\") \"b\" (\"c\"))
;;; =>
"))
      (expect "'((\"a\") \"b\" (\"c\"))
;; => ((\"a\")
;;     \"b\"
;;     (\"c\"))
"
        (lispxmp-to-string t "'((\"a\") \"b\" (\"c\"))
;; =>
"))
      (expect "'((\"a\") \"b\" (\"c\"))
;;; => ((\"a\")
;;;     \"b\"
;;;     (\"c\"))
"
        (lispxmp-to-string nil "'((\"a\") \"b\" (\"c\"))
;;; =>
"))
      (expect "'((\"a\") \"b\" (\"c\"))
;; => ((\"a\")
;;     \"b\"
;;     (\"c\"))
"
        (lispxmp-to-string nil "'((\"a\") \"b\" (\"c\"))
;; =>
"))
      (expect "'a
;;; => a
"
        (lispxmp-to-string t "'a
;;; =>
"))
      (expect "'(\"a\")
;;; => (\"a\")
"
        (lispxmp-to-string t "'(\"a\")
;;; =>
"))
      (desc "pp reexecute")
      (expect "'((\"a\") \"b\" (\"c\"))
;;; => ((\"a\")
;;;     \"b\"
;;;     (\"c\"))
"
        (lispxmp-to-string t "'((\"a\") \"b\" (\"c\"))
;;; => ((\"a\")
;;;     \"b\"
;;;     (\"c\"))
"))
      (expect "1
;;; => 1
;;; 2
;;;    3
"
        (lispxmp-to-string t "1
;;; => 1
;;; 2
;;;    3
"))
      )))

(provide 'lispxmp)

;; How to save (DO NOT REMOVE!!)
;; (emacswiki-post "lispxmp.el")
;;; lispxmp.el ends here
