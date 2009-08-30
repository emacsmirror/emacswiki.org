;;; intel-hex-mode.el --- Mode for Intel Hex files.

;; Copyright (C) 2008 Rubens Ramos

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;; Authors: Rubens Ramos <rubensr AT users.sourceforge.net>
;; Maintainer: Rubens Fernandes <rubensr AT users.sourceforge.net>
;; Homepage: None
;; Created: 08 Oct 2008
;; Last modified: 08 Oct 2008
;; Version: 0.1.1
;; Keywords: mode intel hex

;;; Commentary:
;; Use this mode for editing files in the intel hex format 
;; (http://en.wikipedia.org/wiki/.hex).
;;
;; To use intel-hex-mode, add 
;; (load-file "PATH_TO_FILE/intel-hex-mode.el") 
;; to your ~/.emacs(.el) or ~/.xemacs/init.el
;;
;; The intel-hex-mode will do font locking, and calculate checksums. 
;; This was developed on XEmacs, but should work on Emacs as well.
;;
;; Font locking is automatic.

;;; Todo:

;;; History:

;; Version 0.1.1 First Version
;; 08/10/2008: * First version

;;; Code:

(defconst intel-hex-mode-version "0.1.1"
  "Version of `intel-hex-mode.el'.")

(defgroup intel-hex nil
  "Major mode for editing Intel Hex files"
  :group 'tools)

(defun intel-hex-customize ()
  "Run \\[customize-group] for the `intel-hex' group."
  (interactive)
  (customize-group 'intel-hex))

(defvar intel-hex-mode-abbrev-table nil
  "Abbrev table in use in Intel Hex mode buffers.")
(define-abbrev-table 'intel-hex-mode-abbrev-table ())

;;(defcustom intel-hex-program "dot"
;;  "*Some option."
;;  :type 'string
;;  :group 'intel-hex)

;;; Font lock
(defvar intel-hex-font-lock-keywords
  '(
    ("^\\:" . font-lock-comment-face)
    ("^\\:\\([0-9A-Fa-f]\\{2\\}\\)" 1 font-lock-variable-name-face)
    ("^\\:[0-9A-Fa-f]\\{2\\}\\([0-9A-Fa-f]\\{4\\}\\)" 1 font-lock-reference-face)
    ("^\\:[0-9A-Fa-f]\\{6\\}\\([0-9A-Fa-f]\\{2\\}\\)" 1 font-lock-string-face)
    ("\\([0-9A-Fa-f]\\{2\\}\\)$" 1 font-lock-keyword-face)
    )
  "Highlighting patterns for Intel Hex mode")

;;; Key map
(defvar intel-hex-mode-map ()
  "Keymap used in Intel Hex mode.")

(if intel-hex-mode-map
    ()
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'intel-hex-update-line-checksum)
    (define-key map "\C-c\C-b" 'intel-hex-update-buffer-checksum)
    (setq intel-hex-mode-map map)
    ))

;;;###autoload
(defun intel-hex-mode ()
  "Major mode for the Intel Hex files. \\<intel-hex-mode-map> 

\\[intel-hex-update-line-checksum]\t- Updates the line checksum.
\\[intel-hex-update-buffer-checksum]\t- Updates the checksum for all lines in
the current buffer.

Variables specific to this mode:

  intel-hex-some-variable            (default `value')
       Some variable.

This mode can be customized by running \\[intel-hex-customize].

Turning on Intel Hex mode calls the value of the variable 
`intel-hex-mode-hook' with no args, if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (use-local-map intel-hex-mode-map)
  (setq major-mode 'intel-hex-mode)
  (setq mode-name "intel-hex")
  (setq local-abbrev-table intel-hex-mode-abbrev-table)
;;  (set-syntax-table intel-hex-mode-syntax-table)
  (set (make-local-variable 'font-lock-defaults)
       '(intel-hex-font-lock-keywords))
  (if intel-hex-menu
      (easy-menu-add intel-hex-menu))
  (run-hooks 'intel-hex-mode-hook)
  )

;;;; Menu definitions

(defvar intel-hex-menu nil
  "Menu for Intel Hex Mode.
This menu will get created automatically if you have the `easymenu'
package. Note that the latest X/Emacs releases contain this package.")

(and (condition-case nil
         (require 'easymenu)
       (error nil))
     (easy-menu-define
      intel-hex-menu intel-hex-mode-map "Intel Hex menu"
      '("Intel Hex"
        ["Update Line Checksum"       intel-hex-update-line-checksum     t]
        ["Update File Checksums"      intel-hex-update-buffer-checksum   t]
        "-"
        ["Customize..."               intel-hex-customize                t]
        )))


(defun intel-hex-calculate-line-checksum ()
  "Returns the calculated checksum for a line"
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward ":")
    (let ((byte-count (string-to-number 
		       (buffer-substring (point) (+ (point) 2)) 16))
	  (record-type (buffer-substring (+ (point) 6) (+ (point) 8)))
	  (checksum 0)
	  (count 0))
      (while (< count (+ byte-count 4))
	(setq checksum (+ checksum 
			  (string-to-number 
			   (buffer-substring (point) (+ (point) 2)) 16)))
	(forward-char 2)
	(setq count (1+ count)))
      (logand 255 (- 256 (logand 255 checksum))))))

(defun intel-hex-update-buffer-checksum ()
  "Updates the checksums in the whole file"
  (interactive)
  (save-excursion
    (goto-char 0)
    (while (not (eobp))
      (intel-hex-update-line-checksum)
      (forward-line)))
  (message "Operation completed!"))

(defun intel-hex-update-line-checksum ()
  "Updates the checksum of the current line. If it does not have a checksum
yet, one is appended, otherwise the current one is replaced if necessary."
  (interactive)
  (save-excursion
    (end-of-line)
    (let ((decoded (intel-hex-decode-line)))
      (if (intel-hex-is-valid-line decoded)
	  (let ((new-checksum (intel-hex-calculate-line-checksum))
		(old-checksum (if (nth 5 decoded)
				  (string-to-number (nth 5 decoded) 16)
				-1)))
	    (if (/= new-checksum old-checksum)
		(progn
		  (if (nth 5 decoded)
		      (delete-backward-char 2))
		  (insert (format "%02X" new-checksum))
		  (message 
		   (format "Line checksum updated from %02X to %02X." 
			   old-checksum new-checksum)))))))))
    
(defun intel-hex-is-valid-line (decoded)
  "Returns t if the decoded parameter from intel-hex-decode-line is fine"
  (and (nth 0 decoded) (nth 1 decoded) (nth 2 decoded)
       (nth 3 decoded) (nth 4 decoded)))

(defun intel-hex-decode-line ()
  "Returns a list in the form (t/nil string string string t/nil string)
which represents (in order): if the line has a start code, the byte
count, the address, the record type, if the line has the correct number
of bytes, and the checksum. Where items are not present or incorrect, 
nil is used"
  (save-excursion
    (beginning-of-line)
    (let ((line-length (- (line-end-position) (point)))
	  (has-start-code (looking-at ":"))
	  (byte-count nil)
	  (address nil)
	  (record-type nil)
	  (data nil)
	  (checksum nil)
	  (exp-line-len 11))
      (if (and has-start-code (> line-length 2))
	  (progn
	    (forward-char 1)
	    (setq byte-count (buffer-substring (point) (+ 2 (point))))
	    (setq exp-line-len (+ (* 2 (string-to-number byte-count 16)) 11))
	    (forward-char 2)))
      (if (and byte-count (> line-length 6))
	  (progn
	    (setq address (buffer-substring (point) (+ 4 (point))))
	    (forward-char 4)))
      (if (and address (> line-length 8))
	  (progn
	    (setq record-type (buffer-substring (point) (+ 2 (point))))
	    (forward-char 2)))
      (if (and record-type 
	       (or (= 2 (- exp-line-len line-length))
		   (= 0 (- exp-line-len line-length))))
	  (progn
	    (setq data t)
	    (forward-char (* 2 (string-to-number byte-count 16)))))
      (if (and data (= 0 (- exp-line-len line-length)))
	    (setq checksum (buffer-substring (point) (+ 2 (point)))))
      (list has-start-code byte-count address record-type data checksum))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.hex\\'" . intel-hex-mode))
(add-to-list 'auto-mode-alist '("\\.a90\\'" . intel-hex-mode))

(provide 'intel-hex-mode)
;;; intel-hex-mode.el ends here
