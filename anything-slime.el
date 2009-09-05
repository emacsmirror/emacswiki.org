;;; anything-slime.el --- anything-sources and some utilities for SLIME.

;; Copyright (C) 2009 Takeshi Banse <takebi@laafc.net>
;; Author: Takeshi Banse <takebi@laafc.net>
;; Keywords: convenience, anything, slime

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
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
;; Some Anything and SLIME Configurations for using SLIME within the
;; Anything interface. (The `ascsa-' prefix comes here.)

;;; Installation:
;;
;; Put the anything-slime.el, anything.el and anything-complete.el to your
;; load-path.
;; Set up the SLIME properly.
;; Call `slime-setup' and include 'anything-slime as the arguments:
;;
;;   (slime-setup '([others contribs ...] anything-slime))
;;
;; or simply require anything-slime in some appropriate manner.

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `anything-slime-complete'
;;    Select a symbol from the SLIME's completion systems.
;;  `anything-slime-list-connections'
;;    Yet another `slime-list-connections' with `anything'.
;;  `anything-slime-apropos'
;;    Yet another `slime-apropos' with `anything'.
;;  `anything-slime-repl-history'
;;    Select an input from the SLIME repl's history and insert it.
;;

;;; Code:

(require 'anything)
(require 'anything-complete)
(require 'slime)
(require 'slime-c-p-c)
(require 'slime-fuzzy)
(require 'slime-repl)

(when (require 'anything-show-completion nil t)
  (use-anything-show-completion 'anything-slime-complete
                                '(length anything-complete-target)))

(defun* ascsa-symbol-position-funcall
    (f &optional (end-pt (point)) (beg-pt (slime-symbol-start-pos)))
  (let* ((end (move-marker (make-marker) end-pt))
         (beg (move-marker (make-marker) beg-pt)))
    (unwind-protect
        (funcall f beg end)
      (set-marker end nil)
      (set-marker beg nil))))

(define-anything-type-attribute 'anything-slime-complete
  '((action
     . (("Insert" . ac-insert)
        ("Describe symbol" . slime-describe-symbol)
        ("Edit definition" . slime-edit-definition)))
    (persistent-action . slime-describe-symbol)
    (volatile)
    (candidates-in-buffer)
    (get-line . buffer-substring))
  "SLIME complete.")

(defun ascsa-asc-init-candidates-buffer-base (complete-fn insert-fn)
  (let ((put-text-property1 (lambda (s)
                              (put-text-property (point-at-bol 0)
                                                 (point-at-eol 0)
                                                 'anything-realvalue
                                                 s))))
    (let* ((completion-result (with-current-buffer anything-current-buffer
                                (funcall complete-fn)))
           (completions (first completion-result))
           (base  (second completion-result)))
      (with-current-buffer (anything-candidate-buffer 'global)
        (funcall insert-fn completions base put-text-property1)))))
(defun ascsa-asc-init-candidates-buffer-basic-insert-function (completions base put-text-property1)
  (let ((len (length base)))
    (dolist (c completions)
      (let ((start (point))
            end)
        (insert c)
        (setq end (point))
        (put-text-property start (+ start len) 'face 'bold)
        (insert "\n")
        (funcall put-text-property1 c)))))
(defun ascsa-asc-simple-init ()
  (ascsa-asc-init-candidates-buffer-base
   (slime-curry 'slime-simple-completions anything-complete-target)
   'ascsa-asc-init-candidates-buffer-basic-insert-function))
(defun ascsa-asc-compound-init ()
  (ascsa-asc-init-candidates-buffer-base
   (slime-curry 'ascsa-symbol-position-funcall 'slime-contextual-completions)
   'ascsa-asc-init-candidates-buffer-basic-insert-function))
(defun* ascsa-asc-fuzzy-init (&optional
                              (insert-choice-fn
                               'slime-fuzzy-insert-completion-choice))
  (ascsa-asc-init-candidates-buffer-base
   (slime-curry 'slime-fuzzy-completions anything-complete-target)
   (lambda (completions _ put-text-property1)
     (with-current-buffer (anything-candidate-buffer 'global)
       (let ((max-len (loop for (x _) in completions maximize (length x))))
         (dolist (c completions)
           (funcall insert-choice-fn c max-len)
           (funcall put-text-property1 (car c))))))))
;; These sources are private for the use of the `anything-slime-complete'
;; command, so I should not make `anything-c-source-*' symbols.
(defvar anything-slime-simple-complete-source
  '((name . "SLIME simple complete")
    (init . ascsa-asc-simple-init)
    (type . anything-slime-complete)))
(defvar anything-slime-compound-complete-source
  '((name . "SLIME compound complete")
    (init . ascsa-asc-compound-init)
    (type . anything-slime-complete)))
(defvar anything-slime-fuzzy-complete-source
  '((name . "SLIME fuzzy complete")
    (init . ascsa-asc-fuzzy-init)
    (type . anything-slime-complete)))
(defvar anything-slime-complete-sources
  '(anything-slime-simple-complete-source
    anything-slime-fuzzy-complete-source
    anything-slime-compound-complete-source))

;; Copied from the anything-complete.el and added an optional parameter
;; TARGET-IS-DEFAULT-INPUT-P to not defaulting the target for some kind of
;; the compound/fuzzy completes.
(defun ascsa-anything-noresume (&optional any-sources any-input any-prompt any-resume any-preselect any-buffer)
  (let (anything-last-sources anything-compiled-sources anything-last-buffer)
    (anything any-sources any-input any-prompt any-resume any-preselect any-buffer)))

(defun ascsa-anything-complete (sources target &optional limit idle-delay input-idle-delay target-is-default-input-p)
  (let ((anything-candidate-number-limit (or limit anything-candidate-number-limit))
        (anything-idle-delay (or idle-delay anything-idle-delay))
        (anything-input-idle-delay (or input-idle-delay anything-input-idle-delay))
        (anything-complete-target target)
        (anything-execute-action-at-once-if-one t)
        (enable-recursive-minibuffers t)
        anything-samewindow)
    (ascsa-anything-noresume sources (if target-is-default-input-p target nil)
                             nil nil nil "*anything complete*")))

(defalias 'ascsa-complete 'ascsa-anything-complete)

(defun anything-slime-complete ()
  "Select a symbol from the SLIME's completion systems."
  (interactive)
  (ascsa-complete anything-slime-complete-sources
                  (ascsa-symbol-position-funcall
                   #'buffer-substring-no-properties)))

(defvar anything-c-source-slime-connection
  '((name . "SLIME connections")
    (candidates
     . (lambda ()
         (let ((fstring "%s%2s  %-10s  %-17s  %-7s %-s")
               (collect (lambda (p)
                          (cons
                           (format fstring
                                   (if (eq slime-default-connection p) "*" " ")
                                   (slime-connection-number p)
                                   (slime-connection-name p)
                                   (or (process-id p) (process-contact p))
                                   (slime-pid p)
                                   (slime-lisp-implementation-type p))
                           p))))
           (mapcar collect (reverse slime-net-processes)))))
    (action
     . (("Go to repl"
         . (lambda (p)
             (let ((slime-dispatching-connection p))
               (switch-to-buffer (slime-output-buffer)))))
        ("Set default" . slime-select-connection)
        ("Restart" . slime-restart-connection-at-point)
        ("Quit" . slime-quit-connection-at-point)))))

(defun anything-slime-list-connections ()
  "Yet another `slime-list-connections' with `anything'."
  (interactive)
  (anything 'anything-c-source-slime-connection))

(defadvice anything-slime-update-connection-list (around ignore activate)
  "Don't call slime-update-connection-list if anythinging. (This is iffy.)"
  (when (not anything-source-name)
    ad-do-it))

(define-anything-type-attribute 'anything-slime-apropos
  '((action
     . (("Describe symbol" . slime-describe-symbol)
        ("Edit definition" . slime-edit-definition)))
    (persistent-action . slime-describe-symbol)
    (requires-pattern . 2))
    ;;(volatile)
  "SLIME apropos.")

(defun ascsa-apropos-source (name slime-expressions)
  `((name . ,name)
    (candidates
     . (lambda ()
         (with-current-buffer anything-current-buffer
           (loop for plist in (slime-eval ,slime-expressions)
                 collect (plist-get plist :designator)))))
    (type . anything-slime-apropos)))
(defvar anything-c-source-slime-apropos-symbol-current-package
  (ascsa-apropos-source "SLIME apropos (current package)"
                        (quote
                         `(swank:apropos-list-for-emacs
                           ,anything-pattern
                           nil
                           nil
                           ,(or slime-buffer-package
                                (slime-current-package))))))
(defvar anything-c-source-slime-apropos-symbol-current-external-package
  (ascsa-apropos-source "SLIME apropos (current external package)"
                        (quote
                         `(swank:apropos-list-for-emacs
                           ,anything-pattern
                           t
                           nil
                           ,(or slime-buffer-package
                                (slime-current-package))))))
(defvar anything-c-source-slime-apropos-symbol-all-external-package
  (ascsa-apropos-source "SLIME apropos (all external package)"
                        (quote
                         `(swank:apropos-list-for-emacs
                           ,anything-pattern
                           t
                           nil
                           nil))))
(defvar anything-c-source-slime-apropos-symbol-all-package
  (ascsa-apropos-source "SLIME apropos (all package)"
                        (quote
                         `(swank:apropos-list-for-emacs
                           ,anything-pattern
                           nil
                           nil
                           nil))))
(defvar anything-slime-apropos-sources
  '(anything-c-source-slime-apropos-symbol-current-package
    anything-c-source-slime-apropos-symbol-current-external-package
    anything-c-source-slime-apropos-symbol-all-external-package
    anything-c-source-slime-apropos-symbol-all-package))

(defun anything-slime-apropos ()
  "Yet another `slime-apropos' with `anything'."
  (interactive)
  (anything-other-buffer anything-slime-apropos-sources
                         "*anything SLIME apropos*"))

(defvar anything-c-source-slime-repl-history
  `((name . "SLIME repl history")
    (candidates
     . (lambda ()
         (with-current-buffer anything-current-buffer
           slime-repl-input-history)))
    ;;(multiline)
    (action
     . (lambda (cand)
         (slime-repl-history-replace 'backward
                                     (concat "^" (regexp-quote cand) "$"))))))
(defun anything-slime-repl-history ()
  "Select an input from the SLIME repl's history and insert it."
  (interactive)
  (ascsa-complete 'anything-c-source-slime-repl-history
                  (ascsa-symbol-position-funcall
                   #'buffer-substring-no-properties
                   (point)
                   slime-repl-input-start-mark)
                  nil nil nil t))

(defun anything-slime-init ()
  (run-hooks 'anything-slime-init-hook))

(provide 'anything-slime)
;;; anything-slime.el ends here
