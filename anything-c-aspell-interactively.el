;;; anything-c-aspell-interactively.el --- Interactive aspell with `anything'.

;; Copyright (C) 2009 Takeshi Banse <takebi@laafc.net>
;; Author: Takeshi Banse <takebi@laafc.net>
;; Keywords: convenience, wp, unix, anything

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
;; Interactive aspell with Anything interface. This is just an
;; "aspell -a"'s anything interface.

;;; Setup:
;;
;; (require 'anything-c-aspell-interactively)
;; (define-key global-map "\C-c\C-z" 'anything-c-aspell-interactively)

;;; Note:
;;
;; This package requires a working executable, aspell.

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `anything-c-aspell-interactively'
;;    Interactive aspell with `anything'.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;
;;  `anything-c-aspell-interactively-program'
;;    *An aspell program file name for the `start-process'.
;;    default = "aspell"
;;  `anything-c-aspell-interactively-lang'
;;    *A language code to be passed as the aspell program's "-l" option if any.
;;    default = nil

;;; Code:

(require 'anything)

(defcustom anything-c-aspell-interactively-program "aspell"
  "*An aspell program file name for the `start-process'."
  :type '(string :tag "Program")
  :group 'anything-config)
(defcustom anything-c-aspell-interactively-lang nil
  "*A language code to be passed as the aspell program's \"-l\" option if any."
  :type '(choice (const nil) string)
  :group 'anything-config)

(defun* anything-c-aspell-interactively-any-buffer->sources
    (&optional (any-name (anything-attr 'name))
               (any-buffer anything-buffer)
               (action (anything-attr 'action))
               (persistent-action (anything-attr 'persistent-action)))
  "Convert un-classified aspell's ANY-NAME candidates on ANY-BUFFER into classified sources for `anything'.

  ACTION and PERSISTENT-ACTION are for the classified sources' respectively."
  (labels
      ((source (name cands)
         `((name . ,(format "Aspell Classified (%s)" name))
           (candidates ,@(nreverse cands))
           (action . ,action)
           (persistent-action . ,persistent-action)))
       (rec (xs name cands acc)
         (cond ((and (null xs) name)
                (nreverse (cons (source name cands) acc)))
               ((null xs) nil)
               ((and (consp (car xs)) name)
                (rec (cdr xs) (caar xs) (list (cdar xs))
                     (cons (source name cands) acc)))
               ((consp (car xs))
                (rec (cdr xs) (caar xs) (list (cdar xs)) acc))
               (t
                (rec (cdr xs) name (cons (car xs) cands) acc)))))
    (rec (with-current-buffer any-buffer
           (save-excursion
             (let ((bounds (acai-bounds-of-any-buffer any-name any-buffer)))
               (when bounds
                 (save-restriction
                   (narrow-to-region (car bounds) (cdr bounds))
                   (loop until (eobp)
                         initially (goto-char (point-min)) (forward-line 1)
                         for line = (buffer-substring-no-properties
                                     (point-at-bol) (point-at-eol))
                         unless (equal "" line)
                         collect (case (aref line 0)
                                   ((?* ?&)
                                    (cons line
                                          (progn
                                            (goto-char (+ 2 (point)))
                                            (thing-at-point 'word))))
                                   (t line))
                         do (forward-line 1)))))))
         nil nil nil)))
(defun acai-bounds-of-any-buffer (any-name any-buffer)
  (let* ((beg (loop with pos
                    initially (goto-char (point-min))
                    while (setq pos
                                (next-single-property-change
                                 (point) 'anything-header))
                    do (goto-char pos)
                    if (equal any-name
                              (buffer-substring-no-properties
                               (point-at-bol) (point-at-eol)))
                    return pos))
         (end (or (progn
                    (forward-line 1)
                    (next-single-property-change (point)
                                                 'anything-header))
                  (and beg (point-max)))))
    (when (and beg end)
      (cons beg end))))

(defvar anything-c-source-aspell-interactively
  '((name . "Aspell interactively")
    (match identity)
    (candidates
     . (lambda ()
         (let* ((args `("-a"
                        ,@(anything-aif anything-c-aspell-interactively-lang
                                             `("-l" ,it))))
                (proc (apply 'start-process "aspell-process" nil
                             anything-c-aspell-interactively-program args)))
           (prog1 proc
             (process-send-string proc (concat anything-pattern "\n"))))))
    (candidate-transformer
     . (lambda (cands)
         (let ((collect2 (lambda (line pattern)
                           (case (aref line 0)
                             (?* (list (cons (concat "* " pattern) pattern)))
                             (?& (let ((orig&miss (split-string line ": ")))
                                   (append
                                    (list (cons (car orig&miss) pattern))
                                    (split-string (cadr orig&miss) ", "))))
                             (t nil))))
               (skipp (lambda (x)
                        (or (equal "" x) (string-match "^@(#)" x)))))
           (apply 'append
                  (mapcar* collect2
                           (remove-if skipp cands)
                           (split-string anything-pattern "\\W+"))))))
    (action-transformer
     . (lambda (actions _selection)
         (let ((ss (anything-c-aspell-interactively-any-buffer->sources)))
           (if (not (cdr ss))
               actions
             (append actions
                     (when ss
                       (list
                        (cons
                         "Classify with anything"
                         (lexical-let ((ss ss))
                           (lambda (_)
                             (anything-aif (get-buffer anything-action-buffer)
                                 (kill-buffer it))
                             (anything-other-buffer
                              ss
                              "*anything aspell classification*")))))))))))
    (action . (("Insert" . insert)
               ("Copy result to kill-ring" . kill-new)
               ("Replace"
                . (lambda (c)
                    (let ((b (bounds-of-thing-at-point 'word)))
                      (delete-region (car b) (cdr b))
                      (goto-char (car b))
                      (insert c))))))
    (persistent-action . kill-new)
    (requires-pattern . 3)))
;; (anything 'anything-c-source-aspell-interactively)

(defun anything-c-aspell-interactively ()
  "Interactive aspell with `anything'."
  (interactive)
  (anything 'anything-c-source-aspell-interactively
            (when current-prefix-arg (thing-at-point 'word))
            nil nil nil
            "*anything aspell interactively*"))

(dont-compile
  (when (fboundp 'expectations)
    (expectations
      (desc "acai-bounds-of-any-buffer")
      (expect nil
        (with-temp-buffer
          (acai-bounds-of-any-buffer "any-name" (current-buffer))))
      (expect nil
        (with-temp-buffer
          (insert (mapconcat
                   'identity
                   (list
                    (propertize "not any-name" 'anything-header t)
                    "foo")
                   "\n"))
          (acai-bounds-of-any-buffer "any-name" (current-buffer))))
      (expect (type cons)
        (with-temp-buffer
          (insert (mapconcat
                   'identity
                   (list
                    (propertize "any-name" 'anything-header t)
                    "foo")
                   "\n"))
          (acai-bounds-of-any-buffer "any-name" (current-buffer)))) 
      (desc "anything-c-aspell-interactively-any-buffer->sources")
      (expect nil
        (with-temp-buffer
          (anything-c-aspell-interactively-any-buffer->sources
           "aspell" (current-buffer) "action" "persistent-action")))
      (expect nil
        (with-temp-buffer
          (insert (mapconcat
                   'identity
                   (list
                    (propertize "not aspell" 'anything-header t)
                    "foo"
                    "bar")
                   "\n"))
          (anything-c-aspell-interactively-any-buffer->sources
           "aspell" (current-buffer) "action" "persistent-action")))
      (expect '(((name . "Aspell Classified (* foo)")
                 (candidates "foo" "bar")
                 (action . "action")
                 (persistent-action . "persistent-action")))
        (with-temp-buffer
          (insert (mapconcat
                   'identity
                   (list
                    (propertize "aspell" 'anything-header t)
                    "* foo"
                    "bar")
                   "\n"))
          (anything-c-aspell-interactively-any-buffer->sources
           "aspell" (current-buffer) "action" "persistent-action")))
      (expect '(((name . "Aspell Classified (& foo)")
                 (candidates "foo" "foo")
                 (action . "action")
                 (persistent-action . "persistent-action"))
                ((name . "Aspell Classified (* bar)")
                 (candidates "bar" "blah")
                 (action . "action")
                 (persistent-action . "persistent-action")))
        (with-temp-buffer
          (insert (mapconcat
                   'identity
                   (list
                    "not aspell"
                    "dummy"
                    ""
                    (propertize "aspell" 'anything-header t)
                    "& foo"
                    "foo"
                    "* bar"
                    "blah")
                   "\n"))
          (anything-c-aspell-interactively-any-buffer->sources
           "aspell" (current-buffer) "action" "persistent-action")))
      )))

(provide 'anything-c-aspell-interactively)
;;; anything-c-aspell-interactively.el ends here
