;;; ri.el --- Ruby Documentation Lookup

;; Copyright (C) 2008 Phil Hagelberg

;; Author: Phil Hagelberg
;; Version: 0.3
;; Keywords: tools, documentation
;; Created: 2008-09-19
;; URL: http://www.emacswiki.org/cgi-bin/wiki/RiEl
;; EmacsWiki: RiEl

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see `http://www.gnu.org/licenses/'.

;;; Commentary:

;; ri.el provides an Emacs frontend to Ruby's `ri' documentation
;; tool. It offers lookup and completion.

;; It relies on the ruby script `ri_repl' being on the path, which
;; should be found at http://p.hagelb.org/ri_repl

;;; TODO:

;; * Hypertext-ish follows
;; * Flex matching?
;; * Can we bundle the Ruby script *inside* the elisp and run it with
;;   "ruby -e"? Is that even *sane*?

;;; Code:

(require 'thingatpt)

;; Borrow this functionality from ido.
(unless (functionp 'ido-find-common-substring)
  (require 'ido))

(defvar ri-mode-hook nil
  "Hooks to run when invoking ri-mode.")

;;;###autoload
(defun ri (&optional ri-documented)
  "Look up Ruby documentation."
  (interactive)
  (setq ri-documented (or ri-documented (ri-completing-read)))
  (let ((ri-buffer (get-buffer-create (format "*ri %s*" ri-documented)))
        (ri-content (ri-query ri-documented)))
    (display-buffer ri-buffer)
    (with-current-buffer ri-buffer
      (erase-buffer)
      (insert ri-content)
      (goto-char (point-min))
      (ri-mode))))

(defun ri-mode ()
  "Mode for viewing Ruby documentation."
  (buffer-disable-undo)
  (kill-all-local-variables)
  (local-set-key (kbd "q") 'quit-window)
  (local-set-key (kbd "RET") 'ri-follow)
  (setq mode-name "ri")
  (setq major-mode 'ri-mode)
  (setq buffer-read-only t)
  (run-hooks 'ri-mode-hook))

;;; Completion

(defun ri-completing-read ()
  "Read the name of a Ruby class, module, or method to look up."
  (let (ri-completions-alist) ;; Needs to be dynamically bound.
    (completing-read "RI: " 'ri-complete nil t (ri-symbol-at-point))))

(defun ri-complete (string predicate flag)
  "Dispatch to the proper completion functions based on flag."
  ;; Populate ri-completions-alist if necessary
  (unless (assoc string ri-completions-alist)
    (add-to-list 'ri-completions-alist
                 (cons string
                       (split-string (ri-query (concat "Complete: " string))))))
  (cond ((eq t flag)
         (ri-all-completions string))
        ((eq nil flag)
         (ri-try-completion string))
        ((eq 'lambda flag)
         (ri-test-completion string))
        ((and (listp flag) (eq (car flag) 'boundaries))
         ;; Going to treat boundaries like all-completions for now.
         (ri-all-completions string))
        (t (message "Unknown flag: %s" flag))))

(defun ri-test-completion (string)
  "Return non-nil if STRING is a valid completion."
  (assoc string ri-completions-alist))

(defun ri-all-completions (string)
  "Search for partial matches to STRING in RDoc."
  (cdr (assoc string ri-completions-alist)))

(defun ri-try-completion (string)
  "Return common substring of all completions of STRING in RDoc."
  (ido-find-common-substring (ri-all-completions string) string))

(defun ri-symbol-at-point ()
  ;; TODO: make this smart about class/module at point
  (let ((ri-symbol (symbol-at-point)))
    (if ri-symbol
        (symbol-name ri-symbol)
      "")))

;;; Process Communication

(defun ri-get-process ()
  "Return the subprocess, starting it if necessary."
  (or (get-process "ri-repl")
      (start-process "ri-repl" " *ri-output*" "ri_repl")))

(defun ri-query (string)
  "Passes the `command' to the `ri' subprocess."
  (with-current-buffer (process-buffer (ri-get-process))
    (erase-buffer)
    (process-send-string (ri-get-process) (concat string "\n"))
    (accept-process-output (ri-get-process) 3 0 t)
    (buffer-string)))

(provide 'ri)
;;; ri.el ends here
