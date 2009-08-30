This is an (extremely) minor mode for magically creating and saving aliases for emacs lisp functions, which are then reloaded when emacs restarts.

The standard use case would be something link this:
{{{
M-x save-abbreviation ...
eval-defun ENTER
ed ENTER
}}}
this lets one use "M-x ed" to execute eval-defun, and allows you to use this binding in any future emacs session so long as 
save-abbreviation-mode is enabled.

Caveats:
* At the moment there is no means to delete aliases other than removing them by hand.
* Existing aliases still exist even after save-abbrevition-mode has been disabled - these will only be lost once emacs is restarted.

If these limitations get in the way at all, ask and I'll probably be able to remove them quite quickly.

Notes:
* The default file for saved abbreviations is ~/.abbrev

{{{
;;; save-abbreviation-mode.el - magically save and load aliases for
;;;emacs lisp functions

;; Copyright (c) 2008 Tom Wright

;; Author: Tom Wright
;; Maintainer: Tom Wright
;; Keywords: convenience

;; This file is distributed under the MIT license:
;; http://www.opensource.org/licenses/mit-license.php

; This probably doesn't deserve to be called a mode. But having 
; something that you can treat as
; a black box is probably rather useful.

;;; Installation:
; Place save-abbreviation-mode.el so that it is in your emacs path
; (see load-path variable "C-h v load-path")
; Add
;  (require 'save-abbreviation-mode)
;  (save-abbreviation-mode 1)
; to your emacs file.
;
;;; Caveats:
; At the moment save-abbreviation-mode does not unload abbreviations
; if the mode is disabled.
; If you feel the need to this ask and I will probably add it in a
; couple of days or so.

;;; Code:

(defgroup save-abbreviation nil "Magically saving aliases to emacs
lisps functions (abbreviations) to a file that is reloaded on startup.
This is not to be confused with the abbrev package. There are other
modes to provide automatic abbreviations for all emacs lisp
functions.")

(defcustom save-abbreviation-file "~/.abbrev" "File where
abbreviations should be saved." :group 'save-abbreviaion)

;;;###autoload
(defun save-abbreviation (command-string abbrev)
 "Add an abbreviation (an alias for a function) to the abbreviation list."

 (message "abbrev is %s" abbrev)
 (interactive (list
	        (let ((default-command (car (car command-history))))
		  (read-command 
		   (format "Command to make an abbreviation for: (default %s)" (symbol-name default-command)) default-command))
		(read-string "Abbreviation:")))
 (let ((abbrev-open
       (member (file-truename save-abbreviation-file) (mapcar
'buffer-file-name (buffer-list))))
      (abbrev-buffer (find-file-noselect save-abbreviation-file)))
  (assert (not (fboundp (intern abbrev))) "This abbreviation is already bound")
  (save-excursion
    (set-buffer abbrev-buffer)
    (end-of-buffer)
    (insert (format "(defalias '%s '%s)\n" abbrev command-string))
    (eval-buffer)
    (sort-lines nil (buffer-end -1) (buffer-end 1))
    (basic-save-buffer))
  (if (not abbrev-open)
      (kill-buffer abbrev-buffer))))

;;;###autoload
(define-minor-mode save-abbreviation-mode
 "Toggle save-abbreviation mode.
    With no argument, this command toggles the mode.
    Non-null prefix argument turns on the mode.
    Null prefix argument turns off the mode.

 Save-abbreviation-mode lets one interactively create alias
 for emacs lisp functions - these are saved to disk and
 restored the next time save-abbreviation mode is enabled.
 However these aliases will not be undefined when save
 abbreviation mode is disabled - this will only happen if
 they are removed by hand or emacs is restarted"
 nil
 ;; The indicator for the mode line.
 nil
 ;; The minor mode bindings.
 nil
 :group 'save-abbreviation)


(defun save-abbreviation-load-abbrevs ()
 (message (format "Loading abbreviations file:%s" save-abbreviation-file))
 (if (file-exists-p save-abbreviation-file)
     (load-file save-abbreviation-file)))

(add-hook 'save-abbreviation-mode-hook  'save-abbreviation-load-abbrevs)


(save-abbreviation-load-abbrevs)

(provide 'save-abbreviation-mode)
;; save-abbreviation-mode.el ends here
}}}
