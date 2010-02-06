;;; webgen-mode.el --- webgen mode

;; Author:  recycelbin5385
;; Keywords: webgen, minor mode
;; Website: http://recyclebin5385.blog13.fc2.com/
;; E-mail: recyclebin5385 at yahoo dot co dot jp
;; $Date: 2010-02-06 17:04:33 +0900 (土, 06 2 2010) $
;; $Rev: 12 $

;; This is free software.

;; Copyright (c) 2010 recycelbin5385 All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
;;
;;     * Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
;;     * Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
;;     * Neither the name of 'recycelbin5385' nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Commentary:

;;; Code:

;; variables
(defvar webgen-program "webgen" "webgen program")
(defvar webgen-source-directory "src" "Name of the webgen source directory")
(defvar webgen-output-directory "out" "Name of the webgen output directory")
(defvar webgen-output-page-ext ".html" "Extension of the webgen output page file")

;; mode variable
(defvar webgen-mode nil "webgen Mode")
(make-variable-buffer-local 'webgen-mode)

;; keymap
(defvar webgen-mode-keymap (make-sparse-keymap) "webgen mode keymap")
(define-key webgen-mode-keymap "\C-c\C-b" 'webgen-build)
(define-key webgen-mode-keymap "\C-c\C-v" 'webgen-browse-html)

(or (assq 'webgen-mode minor-mode-map-alist)
    (add-to-list 'minor-mode-map-alist (cons 'webgen-mode webgen-mode-keymap)))


;; Registers webgen minor mode
(or (assq 'webgen-mode minor-mode-alist)
    (add-to-list 'minor-mode-alist '(webgen-mode " wg")))


;; mode function
(defun webgen-mode (&optional arg) "\
Webgen minor mode.
This mode makes it easy to edit your website using webgen.
Webgen is a static website generator written in Ruby.
You can get webgen fron the URL below:
http://webgen.rubyforge.org/


How to use
==========

You can toggle webgen mode interactively by the command below:

    M-x webgen-mode

Input 'C-c C-b' or call funciton 'webgen-build' to run webgen.
Input 'C-c C-v' or call function 'webgen-browse-html' to browse the generated HTML file.


Configuration
=============

Add the code below to your '.emacs' or 'init.el'.
NOTE: If you haven't installed markdown-mode.el, change 'markdown-mode' to whatever mode you like such as 'text-mode'.
    (require 'webgen-mode nil t)
    (add-to-list 'auto-mode-alist '(\"\\\\.page\\\\'\" . (lambda () (markdown-mode) (webgen-mode))))
    (add-to-list 'auto-mode-alist '(\"\\\\.template\\\\'\" . (lambda () (html-mode) (webgen-mode))))
    (add-to-list 'auto-mode-alist '(\"[Mm]etainfo\\\\'\" . (lambda () (text-mode) (webgen-mode))))


Functions and variables
=======================

Webgen mode provides the functions below:

- webgen-build
- webgen-browse-html

Use 'describe-function' to know more about these functions.


Webgen mode provides the variables below:

- webgen-program
- webgen-source-directory
- webgen-output-directory
- webgen-output-page-ext

Use 'describe-variable' to know more about these variables.
"

  (interactive)

  ;; mode variable settings
(setq webgen-mode
      (if (null arg) (not webgen-mode)
        (> (prefix-numeric-value arg) 0)))

  ;; content
  (if webgen-mode ; If webgen mode is turned on, executes the procedure below
      (progn
        (run-hooks 'webgen-mode-hook))
    nil))


;; functions

(defun webgen-search-root (&optional FILE) "\
Returns the absolute directory path of the root of the webgen folder hierarchy.
Parameters:
  FILE : webgen file path (if nil, the current buffer's file path is used instead.)"

  (let ((tmp-directory (file-name-directory (or FILE buffer-file-name)))
        (old-tmp-directory nil))
    (while (and (not (equal old-tmp-directory tmp-directory))
                tmp-directory
                (not (file-exists-p (expand-file-name webgen-source-directory tmp-directory))))
      (setq old-tmp-directory tmp-directory)
      (setq tmp-directory (file-name-directory (directory-file-name tmp-directory))))
    (cond
     ((equal old-tmp-directory tmp-directory) nil)
     (t tmp-directory))))

(defun webgen-output-file (&optional FILE) "\
Returns the absolute file path of the HTML file corresponding to the argument.
Parameters:
  FILE : webgen file path (if nil, the current buffer's file path is used instead.)"
  (let ((root-directory (webgen-search-root FILE)))
    (concat
     (file-name-sans-extension
      (expand-file-name
       (file-relative-name
        (expand-file-name (or FILE buffer-file-name))
        (expand-file-name webgen-source-directory root-directory))
       (expand-file-name webgen-output-directory root-directory)))
     webgen-output-page-ext)))

(defun webgen-build () "\
Calls webgen and build a website."

  (interactive)
  (save-current-buffer
    (let ((output-buffer (get-buffer-create "*webgen*"))
          (webgen-root-directory (webgen-search-root)))
      (save-some-buffers)
      (set-buffer output-buffer)
      (setq buffer-read-only nil)
      (goto-char (point-max))
      (cd webgen-root-directory)
      (call-process webgen-program nil output-buffer t)
      (insert "\n")
      (goto-char (point-max))
      (setq buffer-read-only t)
      (compilation-mode)
      (select-window (display-buffer output-buffer nil t))
      )))

(defun webgen-browse-html () "\
Opens the output file generated by webgen which corresponds to the current buffer."
  (interactive)
  (browse-url (webgen-output-file)))


;;auto insert template

(require 'autoinsert)
(add-to-list 'auto-insert-alist
             '(("\\.page\\'" . "webgen page")
               "webgen page"
               "---" n
               "title: " (read-input "Title: ") n
               "in_menu: true" n
               "sort_info: " (read-input "Sort Info: ") n
               "---" n
               _ ))

(add-to-list 'auto-insert-alist
             '(("[Mm]etainfo\\'" . "webgen metainfo page")
               "webgen page"
               "--- name:paths" n
               "./:" n
               "  title: " (read-input "Title: ") n
               "in_menu: true" n
               "sort_info: " (read-input "Sort Info: ") "\n"
               "--- name:alcn" n
               _ ))

(provide 'webgen-mode)

;;; webgen-mode.el ends here
