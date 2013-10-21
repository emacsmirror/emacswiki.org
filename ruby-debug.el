;;; ruby-debug.el --- 

;; Copyright 2013 Giménez, Christian
;;
;; Author: Giménez, Christian
;; Version: $Id: ruby-debug.el,v 0.0 2013/10/20 22:29:58 christian Exp $
;; Keywords: 
;; X-URL: not distributed yet

;; This program is free software: you can redistribute it and/or modify
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

;; 

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'ruby-debug)

;;; Code:

(require 'inf-ruby)

(defvar rubyd-code-buffer nil
  "This points to the ruby code buffer."
  )

(defvar rubyd-inf-buffer nil
  "This points to the inferior ruby buffer."
  )

(defun rubyd-debug ()
  "Start the debugger."
  (interactive)
  
  )

(defconst rubyd-debug-regexp "  \\([^:]+\\):\\([[:digit:]]+\\):"
  "Regexp used by inf-ruby for helping this script. ")

(defun rubyd-show-line-buffer ()
  "Open the buffer (or the file if needed) of the last line displayed by the debugger.

This works finding the clues at the inf-ruby buffer."
  (interactive)
  (save-excursion
    (with-current-buffer inf-ruby-buffer
      (goto-char (point-max))
      (when (search-backward-regexp rubyd-debug-regexp nil t)
	(rubyd-show-line (match-string 0))
	)
      )
    )
  )


(defun rubyd-preoutput-fnc (proc-ans)
  "This is a function used for inserting to the hook variable `comint-preoutput-filter-functions'.

This shows the results and the line of code recently executed by `rubyd-show-line'."
  ;; (with-current-buffer inf-ruby-buffer
  ;;   (goto-char (point-max))
  ;;   (insert proc-ans)
  ;;   )
  (rubyd-show-line proc-ans)
  proc-ans
  )

(defun rubyd-show-line (proc-ans)
  "Open the buffer (or the file if needed) of the last line displayed by the debugger.

This can be used for adding to the `comint-preoutput-filter-functions' hook variable."
  (if (string-match rubyd-debug-regexp proc-ans)	
      (let* ((filename (match-string 1 proc-ans))
	     (linenum (string-to-number (match-string 2 proc-ans)))
	     (buff (get-buffer (file-name-nondirectory filename)))
	     )      
	
	(unless buff ;; The file haven't been opened...
	  (setq buff (find-file filename))
	  )
	
	;; Showing the buffer and going to the line...
	(switch-to-buffer-other-window buff)
	(with-current-buffer buff
	  (goto-line linenum)	  
	  )
	
	;; Returning to the Inferior Ruby
	(switch-to-buffer-other-window inf-ruby-buffer)	  
	)
    )
  )

					; ____________________
					;
					; Commands

(defun rubyd-next (&optional lines)
  "Next step in debugger.

Send the \"next\" command to the IRB debugger. 

This command accept an optional parameter described as follow at the help command:

>   n[ext][ nnn]               go over one line or till line nnn

"
  (interactive "pLine parameter(see 'help' command)?")
  (rubyd-command 
   (if lines
       (concat "next " (number-to-string lines))
     "next"
     )
   )
  )

(defun rubyd-step (&optional lines)
  "Next step *into* in debugger.

Send the \"next\" command to the IRB debugger. 

This command accept an optional parameter described as follow at the help command:

>  s[tep][ nnn]               step (into methods) one line or till line nnn

"
  (interactive "pLine parameter(see 'help' command)?")
  (rubyd-command 
   (if lines
       (concat "step " (number-to-string lines))
     "step"
     )
   )
  )



(defun rubyd-display (text)
  "Execute the command display."
  (interactive "MRuby expression to display?")
  (rubyd-command (concat "display " text))
  )

(defun rubyd-undisplay (num)
  "Execute the command undisplay."
  (interactive "nNumber of expression to undisplay?")
  (rubyd-command (concat "undisplay " (number-to-string num)))
  )

(defun rubyd-reset ()
  (interactive)
  (when (y-or-n-p "Are you sure you want to reset the debugger?")
      (rubyd-command "reset")
    )
  )
					; ____________________

(defun rubyd-command (command)
  "Send the COMMAND to the ruby inferior buffer."
  (with-current-buffer inf-ruby-buffer
    (goto-char (point-max))
    (insert command)
    ;;(comint-send-string (inf-ruby-proc) command)
    (comint-send-input)
    )
  )

(defvar rubyd-activated nil
  "rubyd-debug has been activated?

Activate with `rubyd-debug-activate' and deactivate with `rubyd-debug-deactivate'.")

(defun rubyd-debug-activate ()
  "Activate keybindings for using ruby-debug with this inf-ruby buffer."
  (interactive)
  (if rubyd-activated
      (message "ruby-debug: keybindings has been activated already")
    (progn
      (define-key inf-ruby-mode-map "s" 'rubyd-show-line-buffer)
      (define-key inf-ruby-mode-map "n" 'rubyd-next)
      (define-key inf-ruby-mode-map "i" 'rubyd-step)
      (define-key inf-ruby-mode-map "d" 'rubyd-display)
      (define-key inf-ruby-mode-map "D" 'rubyd-undisplay)
      (define-key inf-ruby-mode-map "r" 'rubyd-reset)
      (add-hook 'comint-preoutput-filter-functions 'rubyd-preoutput-fnc nil t)
      (message "ruby-debug: Activated keybindings. Press C-c C-n for deactivating it and write freely")
      (set (make-local-variable 'rubyd-activated) t)
      )
    )
  )

(defun rubyd-debug-deactivate ()
  "Deactivate commands, now you can use this inf-ruby freely."
  (interactive)
  (define-key inf-ruby-mode-map "s" nil)
  (define-key inf-ruby-mode-map "n" nil)
  (define-key inf-ruby-mode-map "i" nil)
  (define-key inf-ruby-mode-map "d" nil)
  (define-key inf-ruby-mode-map "D" 'nil)
  (define-key inf-ruby-mode-map "r" 'nil)
  (remove-hook 'comint-preoutput-filter-functions 'rubyd-preoutput-fnc t)
  (message "ruby-debug: Keybindings deactivated. Press C-c C-s for restarting.")
  (set (make-local-variable 'rubyd-activated) nil)	
  )

(define-key inf-ruby-mode-map (kbd "C-c C-s") 'rubyd-debug-activate)
(define-key inf-ruby-mode-map (kbd "C-c C-n") 'rubyd-debug-deactivate)
 




(provide 'ruby-debug)

;;; ruby-debug.el ends here
