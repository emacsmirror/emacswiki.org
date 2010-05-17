;;; fcsh-mode.el --- Adobe Flex Compiler Shell helper

;; Copyright (C) 2010  Denis Martinez

;; Author: Denis Martinez <deuns.martinez@gmail.com>
;; Keywords: fcsh flex flash actionscript as3 mxml adobe

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'comint)

(defvar fcsh-mode-map
  (let ((fcsh-mode-map (make-keymap "fcsh-mode keymap")))
    (define-key fcsh-mode-map "g" 'fcsh-build)
    fcsh-mode-map)
  "fcsh-mode keymap")

(defconst fcsh-prompt "(fcsh) ")
(defconst fcsh-params-filename "fcsh-build.el")

(defgroup fcsh nil "Adobe Flex Compiler Shell integration"
  :group 'fcsh)

(defcustom fcsh-executable "fcsh"
  "Path to the fcsh executable"
  :group 'fcsh)

(defcustom fcsh-default-compiler
  "mxmlc"
  "Default fcsh compiler"
  :group 'fcsh)

(defcustom fcsh-default-flags
  "-incremental=true -optimize=true -actionscript-file-encoding=UTF-8"
  "Default fcsh flags")

(defcustom fcsh-main-script
  "Main.as"
  "Default name for the main script"
  :group 'fcsh)

(defun fcsh-get-params ()
  (let ((params nil))
    (with-temp-buffer
      (when (file-readable-p fcsh-params-filename)
          (insert-file-contents fcsh-params-filename)
          (setf params (read (current-buffer)))))
    params))

(defun fcsh-mode ()
  "Start the fcsh shell in a new buffer"
  (interactive)
  (let* ((params (fcsh-get-params))
         (compiler (or (second (assq 'compiler params)) fcsh-default-compiler))
         (flags (or (second (assq 'flags params)) fcsh-default-flags))
         (script (or (second (assq 'script params)) (read-from-minibuffer "Main script: " fcsh-main-script)))
         (command (mapconcat 'identity `(,compiler ,flags "--" ,script) " ")))

    ;; save the params to file
    (when (null params)
      (with-temp-buffer
        (princ ";; fcsh-mode compile settings" (current-buffer))
        (terpri (current-buffer))
        (princ (pp-to-string `((compiler ,compiler)
                               (flags ,flags)
                               (script ,script))) (current-buffer))
        (write-file fcsh-params-filename)))

    ;; start the shell
    (comint-run fcsh-executable)
    (setq major-mode 'fcsh-mode)
    (setq mode-name "fcsh")
    (use-local-map fcsh-mode-map)
    (set (make-local-variable 'fcsh-compile-command) command)
    (set (make-local-variable 'fcsh-compiled) nil)
    (setf buffer-read-only t)))

(defun fcsh-build ()
  (interactive)
  "Build the flex application"
  (let ((last-line (car (last (split-string (buffer-string) "\n")))))
    (when (string= last-line fcsh-prompt)
      (setf buffer-read-only nil)
      (end-of-buffer)
      (if fcsh-compiled
          (insert "compile 1")
        (progn
          (insert fcsh-compile-command)
          (setf fcsh-compiled t)))
      (comint-send-input)
      (setf buffer-read-only t))))

(defalias 'fcsh 'fcsh-mode)

(provide 'fcsh-mode)
;;; fcsh-mode.el ends here
