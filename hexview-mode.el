;;; hexview-mode.el --- A simple & fast hexadecimal file viewer


;; Copyright (C) 2010 Joyer Huang

;; Author: Joyer Huang <collger@eyou.com>
;; Version: 0.0.3
;; Keywords: hex, view, fast, user interface
;; URL: http://slimeweb.com


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


;;; Commentary:
;;
;; This package provides a user-interface for hex viewing large file!!
;; Hexview mode can open a 5GB file within a second. If you often need
;; view large file for data debugging of probing, Hexview is for you!!
;; For viewing binary content, Hexview mode is much better than the
;; offical hexl-mode(using hexl program piping result in a lot memory
;; waste). The core part  of Hexview mode is just a simple builtin
;; function: `insert-file-contents-literally'. So Hexview doesn't need
;; large memory to view large file. Due to the limitation of emacs's
;; Integer representation , on 32-bit version, you can only view file
;; content up to index `268435455', but in 64-bit version `emacs' the
;; valid range of Integer is much larger. Practically, Hexview mode is
;; enough for 80% of you dailly usage. So enjoy.

;;; Install:
;;
;; Put this file with name `hexview-mode.el' into you emacs `load-path'.
;; Then run or add (require 'hexview-mode) to your .emacs file
;; Use `hexview-find-file' to find/open a file, you will see the magic~


;;; Change Log:
;;
;; Version 0.0.3
;; * fix for emacs version lower than 23
;; 
;; Version 0.0.2
;; * improve UI
;; * add file size range check
;; * edit Change Log
;; * clean prototype code
;; * wrote some help info
;; * add a advice for `find-file-noselect'
;;
;; Version 0.0.1
;; * initial release


;;; Code:

(provide 'hexview-mode)

(require 'cl)
(load-library "files")


(defconst hexview-mode-version "0.0.3")
(defconst hexview-bug-e-mail "collger@eyou.com")
(defconst hexview-web-url "http://slimeweb.com/")

;;how many chars in a Hexview line
(defconst hexview-line-width 16)
;;how many lines in a Hexview buffer
(defconst hexview-line-height 32)
;;should we plot some usage information?
(defconst hexview-usage-info t)

(defvar hexview-mode-hook nil
  "Hook to run after installing hexview mode")
(defvar hexview-mode-map
  (let ((map (make-keymap)))
    map))
(defun hexview:filelen (f)
  (elt (file-attributes f) 7))
(defun hexview:textp (c)
  (and (> c 31)
       (< c 127)))
(defun hexview:clamp-index ()
  (let ((flen (hexview:filelen hexview-view-file)))
    (setq hexview-start-index (cond ((< hexview-start-index 0) 0)
                                    ((>= hexview-start-index flen) (1- flen))
                                    (t hexview-start-index)))))
(defun hexview:usage-info ()
  (if hexview-usage-info
      (progn
        (insert "\n"
                "n: next-line    p: prev-line   q: kill-buffer          M-g | g: goto HEX index\n"
                "M-n | PgDn: next-page          M-p | PgUp: prev-page   M-j | j: goto DEC index\n"
        ))
    t))

(defun hexview:next-page ()
  "View the next page of the Hexview buffer."
  (interactive)
  (setf hexview-start-index (+ hexview-start-index (* hexview-line-height hexview-line-width)))
  (hexview:clamp-index)
  (hexview:update))
(defun hexview:next-line ()
  "View the next line of the Hexview buffer."
  (interactive)
  (setf hexview-start-index (+ hexview-start-index hexview-line-width))
  (hexview:clamp-index)
  (hexview:update))
(defun hexview:prev-page ()
  "View the previous page of the Hexview buffer."
  (interactive)
  (setf hexview-start-index (- hexview-start-index (* hexview-line-height hexview-line-width)))
  (hexview:clamp-index)
  (hexview:update))
(defun hexview:prev-line ()
  "View the previous line of the Hexview buffer."
  (interactive)
  (setf hexview-start-index (- hexview-start-index hexview-line-width))
  (hexview:clamp-index)
  (hexview:update))
(defun hexview:goto-index-hex ()
  "Prompt for a hexadecimal index of the Hexviewing file, and jump to it."
  (interactive)
  (let ((target (read-string "GoTo Hex:")))
    (setq hexview-start-index (string-to-int target 16)))
  (hexview:clamp-index)
  (hexview:update))
(defun hexview:goto-index-dec ()
  "Prompt for a decimal index of the Hexviewing file, and jump to it."
  (interactive)
  (let ((target (read-string "GoTo Dec:")))
    (setq hexview-start-index (string-to-int target 10)))
  (hexview:clamp-index)
  (hexview:update))
;;large-file-warning-threshold
(defun hexview:large-file-hook ()
  "Use hexview-find-file if the file is too large.(by asking users)"
  (message "Try & failed")
  )

(define-key hexview-mode-map "\M-n" 'hexview:next-page)
(define-key hexview-mode-map [next] 'hexview:next-page)
(define-key hexview-mode-map "\M-p" 'hexview:prev-page)
(define-key hexview-mode-map [prior] 'hexview:prev-page)
(define-key hexview-mode-map "n" 'hexview:next-line)
(define-key hexview-mode-map "p" 'hexview:prev-line)
(define-key hexview-mode-map "\M-g" 'hexview:goto-index-hex)
(define-key hexview-mode-map "g" 'hexview:goto-index-hex)
(define-key hexview-mode-map "\M-j" 'hexview:goto-index-dec)
(define-key hexview-mode-map "j" 'hexview:goto-index-dec)
(define-key hexview-mode-map "q" 'kill-buffer)

(defvar hexview:string-to-byte nil
  "Get the byte of a string")

(if (< emacs-major-version 23)
    (setq hexview:string-to-byte (lambda (s idx)
                                    (elt s idx)))
  (setq hexview:string-to-byte (lambda (s idx)
                                  (get-byte idx s))))

(defun hexview:read-file-part (filename beg cnt)
  "Read part of file into a byte sequence"
  (let ((seg (with-temp-buffer 
             (insert-file-contents-literally filename nil beg (+ beg cnt))
             (buffer-string))
           ))
  (mapcar #'(lambda (x) (funcall hexview:string-to-byte (char-to-string x) 0)) seg)))

(defun hexview:set-file (filename)
  "Set the viewing file name of a Hexview buffer"
  (interactive "f")
  (setq hexview-view-file filename))

(defun hexview:update ()
  "Use the `hexview-start-index' to update the whole buffer"
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (format "Hexviewing file:\t %s (%d of %8.0f)\n" hexview-view-file (truncate hexview-start-index) (hexview:filelen hexview-view-file)))
    (dotimes (line hexview-line-height)
      (let* ((line-index (+ (truncate hexview-start-index) (* line hexview-line-width)))
             (line-chars (hexview:read-file-part hexview-view-file line-index hexview-line-width))
             (line-len (length line-chars)))
      (insert (format "%08X: " line-index))
      (mapcar #'(lambda (x) (insert (format "%02X " x))) line-chars)
      (dotimes (v (max (- hexview-line-width line-len) 0))
        (insert "   "))
      (mapcar #'(lambda (x) (insert (if (hexview:textp x) x "."))) line-chars)
      (insert "\n")))
    (hexview:usage-info)
    (goto-char 0)))

(defun hexview-mode ()
  "Major mode for viewing file in hexical mode.
thus \\{hexview-mode}. It's just a weekend project
from Joyer Huang, but more feature will be added as
the time going.
use (Meta N) to page down
use (Meta P) to page up
use (Control Up) to line up
use (Control Down) to line down
use (Meta G) to jump with Hex Index
use (Meta J) to jump with Dec Index
When started, run `hexview-mode-hook'.
\\{hexview-mode-map}"
  (interactive)
  ;; set up local variables
  (kill-all-local-variables)
  (make-local-variable 'hexview-start-index)
  (make-local-variable 'hexview-cursor-index)
  (make-local-variable 'hexview-view-file)
  ;;
  (setq major-mode                    'hexview-mode
	mode-name                     "Hexview"
    hexview-start-index            0
    hexview-cursor-index           0
	)
  (toggle-read-only 1)
  (use-local-map hexview-mode-map)
  (if hexview-mode-hook
      (run-hooks 'hexview-mode-hook)))

(defun hexview-find-file (f)
  "Find a file with `hexview-mode'"
  (interactive "f")
  (let ((hb (get-buffer-create f))
        )
    (switch-to-buffer hb)
    (hexview-mode)
    (hexview:set-file f)
    (hexview:update)))
;(add-hook 'find-file-hook 'hexview:large-file-hook)
(defadvice find-file-noselect (around find-file-noselect-with-hexview last (filename &optional nowarn rawfile wildcards) activate)
  "Use hexview-find-file if the file is too large.(by asking users)"
  (cond ((not (file-exists-p filename)) ad-do-it)
         ((< (hexview:filelen filename) large-file-warning-threshold) ad-do-it)
         (t (if (yes-or-no-p "Try open file with Hexview mode?")
                (hexview-find-file filename)
              ad-do-it))))
