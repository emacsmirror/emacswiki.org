;;; flex-isearch.el --- Flex matching (like ido) in isearch.

;; Copyright (C) 2011 Jonathan Kotta

;; Author: Jonathan Kotta <jpkotta@gmail.com>
;; Contributors: Tomohiro Matsuyama
;; Keywords: convenience, search

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This was inspired by and based on Tomohiro Matsuyama's fuzzy.el.

;; This file defines a minor mode that allows flex matching in
;; isearch-mode.  The flex matching is very similar to the flex
;; matching in ido mode (see `ido-enable-flex-matching').  Given an
;; isearch string, it is transformed to a regexp that matches the
;; original and also similar strings (basically, strings that contain
;; the original characters in the same order, but possibly with other
;; characters in between).  You can redefine
;; `flex-isearch-compile-regexp' to change this behavior.

;; When `flex-isearch-mode' is enabled, the `flex-isearch-enable'
;; variable controls when the flex matching is activated.  See its
;; docstring for details.

;;; Code:

 

(eval-when-compile
  (require 'cl))
(require 'regexp-opt)

;;; Variables

(defvar flex-isearch-activated nil
  "True if flex isearch is activated (i.e. it is overriding
  normal isearch behavior).")

(defvar flex-isearch-failed-count 0
  "Used to decide when to activate flex searching ")

(defvar flex-isearch-enabled 'on-failed
  "When `flex-isearch-mode' is enabled, this determines when flex
  searching is activated.  If it is 'always, then flex matching
  is used for all isearches.  If it is 'on-failed (the default),
  flex matching will only be used after a standard isearch
  failed.")

(defvar flex-isearch-original-search-fun nil
  "Saves the original value of `isearch-search-fun-function' when
  `flex-isearch-mode' is enabled.")

(defvar flex-isearch-prefix "[FLEX] "
  "This string is prepended to the isearch prompt when flex
  searching is activated.")

;;; Functions

(defun flex-isearch-activate ()
  (setq flex-isearch-activated t)
  (setq flex-isearch-failed-count 0))

(defun flex-isearch-deactivate ()
  (setq flex-isearch-activated nil)
  (setq flex-isearch-failed-count 0))

(defun flex-isearch-regexp-compile (string)
  "Transform a normal isearch query string to a regular
  expression that matches the original string but also similar
  strings. "
  (concat "\\<"
          (mapconcat #'regexp-quote
                     (split-string string "")
                     "[[:graph:]]*?") ;; non-greedy * operator
          "\\>"))

(defun flex-search-forward (string &optional bound noerror count)
  "A function suitable to be returned by
  `isearch-search-fun-function' (it is called like
  `search-forward')."
  (let ((regexp (flex-isearch-regexp-compile string)))
    (re-search-forward regexp bound t)))
      
(defun flex-search-backward (string &optional bound noerror count)
  "A function suitable to be returned by
  `isearch-search-fun-function' (it is called like
  `search-forward')."
  (let ((regexp (flex-isearch-regexp-compile string))) 
    (re-search-backward regexp bound t)))

(defun flex-isearch-search-fun ()
  "Set to `isearch-search-fun-function' when `flex-isearch-mode' is
  enabled."
  (cond (isearch-word
         (if isearch-forward 'word-search-forward 'word-search-backward))
        (isearch-regexp
         (if isearch-forward 're-search-forward 're-search-backward))
        ((or flex-isearch-activated
            (eq flex-isearch-enabled 'always)
            (and (eq flex-isearch-enabled 'on-failed)
               (null isearch-success)
               isearch-wrapped
               (> (setq flex-isearch-failed-count (1+ flex-isearch-failed-count))
                  1)))
         (unless flex-isearch-activated
           ;;(goto-char isearch-opoint)
           (flex-isearch-activate))
         (if isearch-forward 'flex-search-forward 'flex-search-backward))
        (t
         (if isearch-forward 'search-forward 'search-backward))))

(defun flex-isearch-end-hook ()
  "Added to `isearch-mode-end-hook' when `flex-isearch-mode' is
  enabled."
  (flex-isearch-deactivate))

(defadvice isearch-message-prefix (after flex-isearch-message-prefix activate)
  (if flex-isearch-activated
      (setq ad-return-value (concat flex-isearch-prefix ad-return-value))
    ad-return-value))

(define-minor-mode flex-isearch-mode
  "Flex matching (similar to ido's flex matching) in incremental searches.

When enabled, it transforms a regular isearch into a much looser
regexp search that will match the original string, but also
strings that simply contain the characters of the search string
in order.  For example, a search string of \"thlongstr\" matches
\"the=long_string\".  See `flex-isearch-regexp-compile' for the actual
regexp that the search string is transformed to."
  
  :init-value nil
  (if flex-isearch-mode
      (progn
        (setq flex-isearch-original-search-fun isearch-search-fun-function)
        (setq isearch-search-fun-function 'flex-isearch-search-fun)
        (add-hook 'isearch-mode-end-hook 'flex-isearch-end-hook))
    (setq isearch-search-fun-function flex-isearch-original-search-fun)
    (remove-hook 'isearch-mode-end-hook 'flex-isearch-end-hook)))
    
(defun turn-on-flex-isearch ()
  (interactive)
  (flex-isearch-mode 1))

(defun turn-off-flex-isearch ()
  (interactive)
  (flex-isearch-mode 0))

(provide 'flex-isearch)

;;; flex-isearch.el ends here
