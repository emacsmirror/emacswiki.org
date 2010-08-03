;;; dove-ext.el --- Emacs minor extensions written by David young

;; Copyright (C) 2009  David Young <dove.young@gmail.com>

;; Author: David Young
;; Maintainer: David Young <dove.young@gmail.com>
;; Keywords: shell, copy, help, tools, convenience
;; URL: http://www.emacswiki.org/cgi-bin/wiki/download/dove-ext.el
;; Site: http://www.emacswiki.org/cgi-bin/emacs/DavidYoung

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the

;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;; miscellaneous


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                 ;;
;;             shell mode related                  ;;
;;                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun come-here (&optional arg)
  "Bring content from specific buffer to current buffer"
  (interactive (list (let (( arg  (read-buffer "Input buffer Name: " "*Shell Command Output*") ))
            (insert-buffer-substring (get-buffer arg))  ))
  )
)

(defun ccc (&optional some)
  "try interactive"
  (interactive (list (let ((some (read-string "Input a string: " nil) ))
             (message "%S" "erwerw") )
        ) )
)


(defun jump (&optional arg)
  "Put current line to another window."
  (interactive "P")
  (let ((command (comint-get-old-input-default))
	 (num (or arg 1)))
       (progn (other-window num) (insert command))
       )
)


;(defun beforeLast (&optional arg)
;  (interactive "P")
;  (let ((num (or arg 1)))
;    (comint-previous-prompt num)
;    )
;  (let ((input (funcall comint-get-old-input))
;	      (goto-char (process-mark process))
;
;	      (insert input))))
;
;

(defun some (&optional arg)
  "Delete Shell command output, to which C-c C-o cannot do for you."
  (interactive "P")
  (let ((end
	 (progn (comint-previous-prompt (or arg 1))
;		(previous-line)   -- this command is for interactive only
		(forward-line -1)
;		(move-beginning-of-line 1)
		(move-end-of-line 1)
		(point)))
	(start
	 (progn (comint-previous-prompt 1)
;		(next-line)
		(move-beginning-of-line 2)
		(point)))
	)
    (delete-region start end)
    )

)

 (defun mywrite-region (&optional string)
   "Append current region to specified file. Leverage write-region to implement this function"
   (interactive "BInput file name: ")
   (write-region (region-beginning) (region-end) string "append")
 )



(defun matrixSum (start end)
  "Calculate matrix statistics by row & column. Then insert result into current buffer."
  (interactive "r")
  (progn (shell-command-on-region start end "matrixSum")
	 (let ((buf "*Shell Command Output*"))
	   (progn (come-here buf) (kill-buffer buf) (delete-other-windows))
	   )
	   )
)


(defun exitshell(&optional arg)
  " Exit from login shell, with prefix to exit many levels "
  (interactive "P")
  (let (( nlist (make-list (or arg 1) 1) ))
    (while nlist
      (comint-next-prompt 1)
      (insert "exit ")
      (comint-send-input)
      (sit-for 1)
      (pop nlist)
      )
    )
)

(defun dove-backward-kill-word (&optional arg)
  "Backward kill word, but do not insert it into kill-wring"
  (interactive "P")
  (let (( end (point) )
	( beg (progn (backward-word arg ) (point)))
	      )
    (delete-region beg end)
    )
)

(defun dove-forward-kill-word (&optional arg)
  "Backward kill word, but do not insert it into kill-wring"
  (interactive "P")
  (let (( beg (point) )
	( end (progn (forward-word arg ) (point)))
	      )
    (delete-region beg end)
    )
)

(defun rename-buffer-in-ssh-login (cmd)
  "Rename buffer to the destination hostname in ssh login"
  (if (string-match "ssh [a-z0-9A-Z]+@[a-z0-9A-Z]+" cmd)
      (let (( host (car (last (split-string cmd "[ @\n]" t) )))
	    )
      (message "%s" host)
      (rename-buffer (concat "*" host))
      )
    )
)

(add-hook 'comint-input-filter-functions 'rename-buffer-in-ssh-login)

(defun kill-shell-buffer(process event)
  "kill the shell buffer "
  (kill-buffer (process-buffer process))
)

(defun kill-shell-buffer-after-exit()
  "kill shell buffer when exit."
  (set-process-sentinel (get-buffer-process (current-buffer))
		      #'kill-shell-buffer)
)

(add-hook 'shell-mode-hook 'kill-shell-buffer-after-exit t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                 ;;
;;             copy & paste related                ;;
;;                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun insert-buffer-name (&optional arg)
 "Insert current buffer name at point"
  (interactive (list (let (( arg  (read-buffer "Input buffer Name: " (buffer-name (current-buffer))) ))
            (insert (buffer-name (get-buffer arg)))  ))
  )
)


(defun copy-line (&optional arg)
 "Save current line into Kill-Ring without mark the line "
  (interactive "P")
;  (copy-region-as-kill (line-beginning-position) (line-end-position arg))
  (let ((beg (line-beginning-position))
	(end (line-end-position arg)))
    (if (= (or arg 0) 1)    ;; arg=1, mark line only
	(progn (set-mark beg) (goto-char end))
    (copy-region-as-kill beg end))
    )
)

 (defun to-the-line-end (&optional arg)
   "Copy/Erase to current line line, like kill-line"
   (interactive "P")
   (if (= (or arg 0) 1)    ;; arg=1, erease line only
     (copy-region-as-kill (point) (line-end-position arg))
     (delete-region (point) (line-end-position arg))
     )
)

; (defun erease-to-line-end (&optional arg)
;   "Erease contains to the end of line"
;   (interactive "p")
;   (delete-region (point) (line-end-position arg))
;)


(defun copy-word (&optional arg)
 "Copy words at point into kill-ring"
  (interactive "P")
  (let ((beg (progn (if (looking-back "[a-zA-Z0-9]" 1) (backward-word 1)) (point)))
	(end (progn (forward-word arg) (point))))
    (if (= (or arg 0) 1)    ;; arg=1, mark word only
	(set-mark beg)
    (copy-region-as-kill beg end))
    )
)

(defun copy-paragraph (&optional arg)
 "Copy paragraphes at point"
  (interactive "P")
  (let ((beg (progn (backward-paragraph 1) (point)))
	(end (progn (forward-paragraph arg) (point))))
    (if (= (or arg 0) 1)    ;; arg=1, mark paragraph only
      (set-mark beg)
      (copy-region-as-kill beg end))
    )
)


;; (defun copy-string-to-mark (&optional arg)
;;  "Copy a sequence of string into kill-ring and then paste to the mark point"
;;   (interactive "P")
;;   (let (
;;	 (onPoint (point))
;;	 )
;;     (let (
;;
;;	 ( beg 	(progn (re-search-backward "[\t ]" (line-beginning-position) 3 1)
;;			  (if (looking-at "[\t ]") (+ (point) 1) (point) ) )
;;		)
;;          ( end  (progn  (goto-char onPoint) (re-search-forward "[\t ]" (line-end-position) 3 1)
;;			  (if (looking-back "[\t ]") (- (point) 1) (point) ) )
;;		 )
;;	  )
;;       (copy-region-as-kill beg end)
;;       (message "%s" major-mode)
;;       (if (string= "shell-mode" major-mode)
;;	   (progn (comint-next-prompt 25535) (yank))
;;	 ;; unless arg == 0, paste content to mark, else hold in kill ring
;;	   (unless (> (or arg 0) 0) (progn (goto-char (mark)) (yank) )))
;;
;;       )
;;     )
;; )


(defun beginning-of-string()
  "  "
;  (interactive "p")
;  (re-search-backward "[ \t]" (line-beginning-position) 3 1)
  (re-search-backward "[ \t]" (line-beginning-position) 3 1)
	     (if (looking-at "[\t ]")  (goto-char (+ (point) 1)) )
)
(defun end-of-string()
  " "
;  (interactive "p")
;  (re-search-forward "[ \t]")
  (re-search-forward "[ \t]" (line-end-position) 3 1)
	     (if (looking-back "[\t ]") (goto-char (- (point) 1)) )
)

;(put 'string 'forward-op 'forward-string)
(put 'string 'beginning-op 'beginning-of-string)
(put 'string 'end-op 'end-of-string)


(defun thing-copy-string ()
  "Try to get string"
  (interactive)
  (thing-edit 'string)
)

(defun thing-copy-string-to-mark(&optional arg)
  " Try to copy a string and paste it to the mark
When used in shell-mode, it will paste string on shell prompt by default "
  (interactive "P")
  (thing-copy-string)
   (if (not arg)
       (if (string= "shell-mode" major-mode)
	   (progn (comint-next-prompt 25535) (yank))
	   (progn (goto-char (mark)) (yank) )))
)

(defun beginning-of-parenthesis()
  "  "
;  (interactive "p")
;  (re-search-backward "[ \t]" (line-beginning-position) 3 1)
  (re-search-backward "[[<(]" (line-beginning-position) 3 1)
	     (if (looking-at "[[<(]")  (goto-char (+ (point) 1)) )
)
(defun end-of-parenthesis()
  " "
;  (interactive "p")
;  (re-search-forward "[ \t]")
  (re-search-forward "[]>)]" (line-end-position) 3 1)
	     (if (looking-back "[]>)]") (goto-char (- (point) 1)) )
)


;(put 'parenthesis 'forward-op 'forward-parenthesis)
(put 'parenthesis 'beginning-op 'beginning-of-parenthesis)
(put 'parenthesis 'end-op 'end-of-parenthesis)

(defun thing-copy-parenthesis ()
  "Try to get parenthesis"
  (interactive)
  (thing-edit 'parenthesis)
)

(defun thing-copy-parenthesis-to-mark(&optional arg)
  " Try to copy a parenthesis and paste it to the mark
When used in shell-mode, it will paste parenthesis on shell prompt by default "
  (interactive "P")
  (thing-copy-parenthesis)
   (if (not arg)
       (if (string= "shell-mode" major-mode)
	   (progn (comint-next-prompt 25535) (yank))
	   (progn (goto-char (mark)) (yank) )))
)



(defun backward-symbol (&optional arg)
  (interactive "P")
  "Go backward a symbol, just like forward-symbol, by provide a -1 arg to it"
   (if arg  (forward-symbol arg) (forward-symbol -1))
   (message "%s" arg)
)

(defun move-forward-paren (&optional arg)

  (interactive "sInput a Parenthesis:")
  (message "%s" arg)
  (re-search-forward arg (point-max) 3 1)
)

(defun move-backward-paren (&optional arg)

  (interactive "sInput a Parenthesis:")
  (message "%s" arg)
  (re-search-backward arg (point-max) 3 1)
)

(defun move-to-the-word (&optional arg)
 "Moving to next occurrance of current word"
 (interactive "P")
 (let (( cur-word (current-word 1 1) ))
		(re-search-forward cur-word))
)

(defun back-to-the-word (&optional arg)
 "Moving to next occurrance of current word"
 (interactive "P")
 (let (( cur-word (current-word 1 1) ))
		(re-search-backward cur-word))
)


(defun convert-Table(start end)
"Convert Emacs table into HTML table"
  (interactive "r")
    (shell-command-on-region start end "sed -e 's%^.%<tr><td>&%' -e 's%    *%</td><td>%g' -e 's%<td>$%</tr>%'")
	 (set-buffer "*Shell Command Output*")
;	 (beginning-of-buffer)
	 (goto-char (point-min))
	 (insert "<table border=\"1\" cellspacing=\"0\" cellpadding=\"0\">\n")
;	 (end-of-buffer)
	 (goto-char (point-max))
	 (insert "</table>\n")
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                 ;;
;;             window layout related               ;;
;;                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun split-window-4()
 "Splite window into 4 sub-window"
 (interactive)
 (progn (split-window-vertically)
        (split-window-horizontally)
        (other-window 2)
        (split-window-horizontally)
 )
)

(defun split-v ()
  (interactive)
    (let (( thisBuf (window-buffer))
	  ( nextBuf (progn (other-window 1) (buffer-name))))
	  (progn   (delete-other-windows)
		   (split-window-horizontally)
		   (set-window-buffer nil thisBuf)
		   (set-window-buffer (next-window) nextBuf)
		   ))
)

(defun split-h ()
  (interactive)
    (let (( thisBuf (window-buffer))
	  ( nextBuf (progn (other-window 1) (buffer-name))))
	  (progn   (delete-other-windows)
		   (split-window-vertically)
		   (set-window-buffer nil thisBuf)
		   (set-window-buffer (next-window) nextBuf)
		   ))
)


;
;(defun dove-hide-shell-output()
;  "Hide Shell Output"
;  (interactive)
;;  (let ((beg (progn (move-end-of-line 0) (point))))
;;        (end (progn (comint-previous-prompt 0) (move-beginning-of-line 2) (point)
;;
;;   (set-mark-command (progn (move-end-of-line 0) (point)))
;  (let ((start (point)))
;    (progn
;      (comint-previous-prompt 1)
;      (move-beginning-of-line 2)
;      (set-mark (point))
;      (comint-next-prompt 1)
;      (move-end-of-line -1)
;      (hide-region-hide)
;      (set-mark start))
;    )
;)

;; edit related

;(defun try-expand-dabbrev-path (old)
;  (setq hippie-expand-dabbrev-as-symbol nil)
;  (try-expand-dabbrev old)
;  (setq hippie-expand-dabbrev-as-symbol t)
;)

;(message "%s" hippie-expand-try-functions-list)

(setq hippie-expand-try-functions-list
      (append (list (car  hippie-expand-try-functions-list)
	       'try-expand-dabbrev-path)
	      (cdr  hippie-expand-try-functions-list)
	      )
)


    (defun find-next-occurance-of-region (start end)
      "Jump to the next occurance of region, and sets it as the current region"
      (interactive "r")
      (let ((region-size (- end start)) (region-text (buffer-substring start end)))
        (unless
	    (when (search-forward region-text nil t 1)
	      (setq mark-active nil)
	      (set-mark (- (point) region-size))
	      (setq mark-active t))
          (message "No more occurances of \"%s\" in buffer!" region-text))))


(provide 'dove-ext)
