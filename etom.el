;;; etom.el --- Emacs to Maya communication

;; Copyright (C) 2007 Slavomir Kaslev
;; Copyright (C) 2009 Koichi Tamura
;; Copyright (C) 2017 Michael Abrahams

;; Author: Slavomir Kaslev <slavomir.kaslev@gmail.com>
;; Maintainer: Michael Abrahams <miabraha@gmail.com>
;; Created: 17 Jun 2007
;; Version: etom.el 0.1, 13/10/2017
;; Keywords: Emacs, Maya, MEL, Python

;; Modified the original script by Slavomir Kaslev to send Python scripts
;; Modified By: Koichi Tamura <hohehohe2@gmail.com>
;; Date: 28 Jun 2009

;; Modified to remove broken MEL support.
;; Michael Abrahams <miabraha@gmail.com>
;; Date: 13 Oct 2017

;; This file is NOT part of Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301, USA.

;; Commentary:

;; This package is used for communication between Emacs and Maya.
;; It supports Python only.

;; To open a command port in Maya, use this code.
;;
;; Python (userSetup.py):
;; maya.cmds.commandPort(name=":2222", sourceType="python", echoOutput=True)
;;
;; MEL (userSetup.mel):
;; commandPort -name ":2222" -sourceType "python" -echoOutput;

;; To automatically load EtoM for Python, insert in your ~/.emacs file:
;; (add-hook
;;  'python-mode-hook
;;  (lambda ()
;;    (require 'etom)
;;    (setq etom-default-host "localhost")
;;    (setq etom-default-port 2222)
;;    (local-set-key (kbd "C-c C-r") 'etom-send-region)
;;    (local-set-key (kbd "C-c C-c") 'etom-send-buffer)
;;    (local-set-key (kbd "C-c C-l") 'etom-send-buffer)
;;    (local-set-key (kbd "C-c C-z") 'etom-show-buffer)))


;; Note about MEL support.
;; A previous version of this code attempted to provide both MEL and Python
;; support by opening a MEL commandport and sending Python calls by wrapping
;; them in the function python(). Some undocumented changes in Maya's
;; commandport broke MEL support, and only Python currently works.


;;; Code:

(require 'comint)

(defcustom etom-default-host "localhost"
  "Default name of the host on which Maya is running."
  :type 'string
  :group 'etom)

(defcustom etom-default-port 2222
  "Default port number to connect to Maya."
  :type 'integer
  :group 'etom)

(defcustom etom-always-show t
  "Non-nil means display etom-buffer after sending a command."
  :type 'boolean
  :group 'etom)

(defcustom etom-prompt-regexp "^\0$"
  "Regexp which matches the Maya's prompt."
  :type 'regexp
  :group 'etom)

(defvar etom-buffer nil
  "Buffer used for communication with Maya.")

(defvar etom-cmd-template
  (concat
   "import __main__\n"
   "f = open(\"%s\")\n"
   "try:\n"
   "  c = f.read()\n"
   "  exec(c,  __main__.__dict__, __main__.__dict__)\n"
   "finally:\n"
   "  f.close()"
   )
  "Python command template to execute a script file.")


(defun etom-show-buffer ()
  "Make sure `etom-buffer' is being displayed."
  (interactive)
  (if (not (etom-connected))
      (etom-connect))
  (display-buffer etom-buffer))

(defun etom-hide-buffer ()
  "Delete all windows that display `etom-buffer'."
  (interactive)
  (delete-windows-on etom-buffer))

(defun etom-connect ()
  "Connect to Maya."
  (interactive)
  (setq comint-prompt-regexp etom-prompt-regexp)
  (setq etom-buffer (make-comint "Maya" (cons etom-default-host etom-default-port)))
  (set-process-query-on-exit-flag (get-buffer-process etom-buffer) nil)
  (if etom-always-show
      (etom-show-buffer)))

(defun etom-disconnect ()
  "Disconnect from Maya and kill etom-buffer."
  (interactive)
  (if etom-buffer
      (kill-buffer etom-buffer)))

(defun etom-connected ()
  "Return non-nil if there is connection to Maya."
  (interactive)
  (comint-check-proc etom-buffer))

(defun etom-prompt-line ()
  (save-excursion
    (forward-line 0)
    (looking-at comint-prompt-regexp)))

(defun etom-wait-for-prompt (last-prompt)
  (let ((prompt-found nil))
    (while (not prompt-found)
      (accept-process-output (get-buffer-process (current-buffer)))
      (goto-char (point-max))
      (setq prompt-found (and (etom-prompt-line) (not (= (count-lines (point-min) (point-max)) last-prompt)))))))

(defun etom-send-current-line ()
  "Send current line to Maya as Python."
  (interactive)
  (let ((start (save-excursion (beginning-of-line) (point)))
        (end (save-excursion (end-of-line) (point))))
    (etom-send-region start end)))

(defun etom-send-region (start end)
  "Send region to Maya as Python."
  (interactive "r")
  (if (not (etom-connected))
      (etom-connect))
  (if etom-always-show
      (etom-show-buffer))
  (let* ((tempfile (make-temp-file "etom-"))
         (tempfile-slashes (replace-regexp-in-string "\\\\" "\\\\\\\\" tempfile ))
         (pycmd (format etom-cmd-template tempfile-slashes tempfile-slashes)))
    (write-region start end tempfile)
    ;; Send temporary file
    (with-current-buffer etom-buffer
      (let ((last-prompt (count-lines (point-min) (point-max))))
        (goto-char (point-max))
        (comint-simple-send (get-buffer-process (current-buffer)) pycmd)
        (etom-wait-for-prompt last-prompt)
        (delete-file tempfile)))))

(defun etom-send-region-2 (start end)
  "Send region to Maya as Python."
  (interactive "r")
  (if (not (etom-connected))
      (etom-connect))
  (if etom-always-show
      (etom-show-buffer))
  (comint-simple-send (get-buffer-process etom-buffer)
                      (buffer-substring start end)))

(defun etom-send-buffer ()
  "Send whole buffer to Maya as Python."
  (interactive)
  (etom-send-region (point-min) (point-max)))

(provide 'etom)

;;; etom.el ends here
