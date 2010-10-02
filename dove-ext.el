;;; dove-ext.el --- Emacs minor extensions written by David young

;; Copyright (C) 2009  David Young <dove.young@gmail.com>

;; Author: David Young
;; Maintainer: David Young <dove.young@gmail.com>
;; Keywords: shell,shell-mode, copy, help, tools, convenience
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

;; 2010-08-04 improved rename-buffer-in-ssh-login function
;; 2010-08-08 rewrote function rename-buffer-in-ssh-login
;; 2010-08-08 added function rename-buffer-in-ssh-exit
;; 2010-08-21 added my-overwrite, updated jump function
;; 2010-09-18 added very fancy function split-v-3 and split-h-3
;; 2010-09-27 added roll-v-3
;; 2010-10-01 Rewrote some functions in more Lisp like style

(defun get-point (symbol num)
 "get the point"
 (funcall symbol num)
 (point)
)

(defun test-get-point (&optional arg)
  "test-get-point"
  (interactive "P")
  (message "%s" (get-point 'forward-word 3))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                 ;;
;;             shell mode related                  ;;
;;                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun come-here (&optional arg)
  "Bring content from specific buffer to current buffer"
  (interactive (list (let (( arg (read-buffer "Input buffer Name: " "*Shell Command Output*") ))
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
       (progn (other-window num)
	      ;; if in shell-mode, goto shell input prompt, else just insert at point
	      (if (string= "shell-mode" major-mode)
		  (progn
		    (goto-char (point-max))
		    ;; First delete any old unsent input at the end
		    (delete-region
		     (or (marker-position comint-accum-marker)
			 (process-mark (get-buffer-process (current-buffer))))
		     (point))
		    )
		)
	      (insert command)
	      )
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
	 (get-point  '(lambda (&optional arg) "" (interactive "P")
		(comint-previous-prompt (or arg 1))
;		(previous-line)   -- this command is for interactive only
		(forward-line -1)
;		(move-beginning-of-line 1)
		(move-end-of-line 1)
		)  arg))
	(start
	 (get-point '(lambda (&optional arg) "" (interactive "P")
		 (comint-previous-prompt 1)
;		(next-line)
		(move-beginning-of-line 2)
		)  arg))
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


; The function will ignore command like this
; ssh msg@tivx24.cn.ibm.com ls bin
; It could response only to command like this
; ssh msg@tivx24.cn.ibm.com 

(add-hook 'shell-mode-hook 
	  (lambda()
	    (setq shell-buffer-name-list (list "*shell*") )
	    (message "%s" shell-buffer-name-list)
	    )
	  t
)

(eval-after-load 'shell
  '(progn

      ; rewrote this function to co-work with rename-buffer-in-ssh-exit

      (defun rename-buffer-in-ssh-login (cmd)
        "Rename buffer to the destination hostname in ssh login"
        (if (string-match "ssh [-_a-z0-9A-Z]+@[-_a-z0-9A-Z.]+[ ]*[^-_a-z0-9-A-Z]*$" cmd)
            (let (( host (nth 2 (split-string cmd "[ @\n]" t) ))
      	    )
;            (message "%s" (split-string cmd "[ @\n]" t) )
            (rename-buffer (concat "*" host))
            (add-to-list 'shell-buffer-name-list (concat "*" host))
            (message "%s" shell-buffer-name-list)
            )
          )
      ;  (if (string-match "^bash$") 
      ;      (add-to-list 'shell-buffer-name-list nil)
      ;    )
      )
      
      (add-hook 'comint-input-filter-functions 'rename-buffer-in-ssh-login)
      
      ;; This function works only in a single shell session. 
      ;; Not sure how to make it work and safe in multi-session.
      ;; how to handle commands like 'bash' and then 'exit' is also a problem

;;      (defun rename-buffer-in-ssh-exit (cmd)
;;        "Rename buffer to its previous name when user exit from a ssh login"
;;      ;  (message "%s" cmd)
;;        (message "%s" shell-buffer-name-list)
;;        (if (string-match "^exit$" cmd)
;;            (if (> (length shell-buffer-name-list) 1)
;;      	  (progn (pop shell-buffer-name-list)
;;      		 (rename-buffer  (car shell-buffer-name-list)))
;;          )
;;        )
;;        (message "%s" shell-buffer-name-list)
;;      )
      	     
;;      (add-hook 'comint-input-filter-functions 'rename-buffer-in-ssh-exit t)
     
      
      (defun kill-shell-buffer(process event)
        "The one actually kill shell buffer when exit. "
        (kill-buffer (process-buffer process))
      )
      
      (defun kill-shell-buffer-after-exit()
        "kill shell buffer when exit."
        (set-process-sentinel (get-buffer-process (current-buffer))
      		      #'kill-shell-buffer)
      )
      
      (add-hook 'shell-mode-hook 'kill-shell-buffer-after-exit t)

))

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

;(defun copy-word (&optional arg)
; "Copy words at point into kill-ring"
;  (interactive "P")
;  (if arg
;      (if (< arg 0)
;	  (let ((end (end-of-thing 'word))
;	    (beg (progn (forward-word arg) (point))))
;	    (copy-region-as-kill beg end))
;	  (let ((beg (beginning-of-thing 'word))     
;	    (end (progn (forward-word arg) (point))))
;	    (copy-region-as-kill beg end))
;      )
;  	  (let ((beg (beginning-of-thing 'word)) 
;	    (end (progn (forward-word arg) (point))))
;	    (copy-region-as-kill beg end))
;  )
;)


(defun copy-word (&optional arg)
 "Copy words at point into kill-ring"
  (interactive "P")
  (if arg
      (if (< arg 0)
	  (let ((end (end-of-thing 'word))
		(beg (get-point 'forward-word arg)))
	    (copy-region-as-kill beg end))
	  (let ((beg (beginning-of-thing 'word)) 
		(end (get-point 'forward-word arg)))
	    (copy-region-as-kill beg end))
      )
  	  (let ((beg (beginning-of-thing 'word))
		(end (get-point 'forward-word arg)))
	    (copy-region-as-kill beg end))
  )
)

(defun copy-paragraph (&optional arg)
 "Copy paragraphes at point"
  (interactive "P")
  (let ((beg (get-point 'backward-paragraph 1))
	(end (get-point 'forward-paragraph arg)))
    (if (= (or arg 0) 1)    ;; arg=1, mark paragraph only
      (set-mark beg)
      (copy-region-as-kill beg end))
    )
)

;(defun dove-backward-kill-word (&optional arg)
;  "Backward kill word, but do not insert it into kill-wring"
;  (interactive "P")
;  (let (( end (point) )
;	( beg (progn (backward-word arg ) (point)))
;	      )
;    (delete-region beg end)
;    )
;)

(defun dove-forward-kill-word (&optional arg)
  "Backward kill word, but do not insert it into kill-wring"
  (interactive "P")
  (let (( beg (point) )
	( end (get-point 'forward-word arg)))
    (delete-region beg end)
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
  (re-search-backward "[[<(?\"]" (line-beginning-position) 3 1)
	     (if (looking-at "[[<(?\"]")  (goto-char (+ (point) 1)) )
)
(defun end-of-parenthesis()
  " "
;  (interactive "p")
  (re-search-forward "[]>)?\"]" (line-end-position) 3 1)
	     (if (looking-back "[]>)?\"]") (goto-char (- (point) 1)) )
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

;  +-----------------------+----------------------+
;  |                       |                      |
;  |                       |                      |
;  |                       |                      |
;  +-----------------------+----------------------+
;  |                       |                      |
;  |                       |                      |
;  |                       |                      |
;  +-----------------------+----------------------+

(defun split-window-4()
 "Splite window into 4 sub-window"
 (interactive)
 (if (= 1 (length (window-list)))
     (progn (split-window-vertically)
	    (split-window-horizontally)
	    (other-window 2)
	    (split-window-horizontally)
	    )
   )
)

;  +----------------------+                 +----------- +-----------+ 
;  |                      |           \     |            |           | 
;  |                      |   +-------+\    |            |           | 
;  +----------------------+   +-------+/    |            |           |
;  |                      |           /     |            |           | 
;  |                      |                 |            |           | 
;  +----------------------+                 +----------- +-----------+ 

(defun split-v ()
  (interactive)
  (if (= 2 (length (window-list)))
    (let (( thisBuf (window-buffer))
	  ( nextBuf (progn (other-window 1) (buffer-name))))
	  (progn   (delete-other-windows)
		   (split-window-horizontally)
		   (set-window-buffer nil thisBuf)
		   (set-window-buffer (next-window) nextBuf)
		   ))
    )
)


;  +----------- +-----------+                  +----------------------+ 
;  |            |           |            \     |                      | 
;  |            |           |    +-------+\    |                      | 
;  |            |           |    +-------+/    +----------------------+ 
;  |            |           |            /     |                      | 
;  |            |           |                  |                      | 
;  +----------- +-----------+                  +----------------------+ 

(defun split-h ()
  (interactive)
  (if (= 2 (length (window-list)))
    (let (( thisBuf (window-buffer))
	  ( nextBuf (progn (other-window 1) (buffer-name))))
	  (progn   (delete-other-windows)
		   (split-window-vertically)
		   (set-window-buffer nil thisBuf)
		   (set-window-buffer (next-window) nextBuf)
		   ))
    )
)

;  +----------------------+                 +----------- +-----------+ 
;  |                      |           \     |            |           | 
;  |                      |   +-------+\    |            |           | 
;  +----------------------+   +-------+/    |            |-----------|
;  |         |            |           /     |            |           | 
;  |         |            |                 |            |           | 
;  +----------------------+                 +----------- +-----------+ 


(defun split-v-3 ()
  "Change 3 window style from horizontal to vertical"
  (interactive)
  (select-window (get-largest-window))
  (if (= 3 (length (window-list)))
      (let ((winList (window-list)))
	    (let ((1stBuf (window-buffer (car winList)))
		  (2ndBuf (window-buffer (car (cdr winList))))
		  (3rdBuf (window-buffer (car (cdr (cdr winList))))))
	      (message "%s %s %s" 1stBuf 2ndBuf 3rdBuf)
	      (delete-other-windows)
	      (split-window-horizontally)
	      (set-window-buffer nil 1stBuf)
	      (other-window 1)
	      (set-window-buffer nil 2ndBuf)
	      (split-window-vertically)
	      (set-window-buffer (next-window) 3rdBuf)
	      (select-window (get-largest-window))
	    )
	  )
    )
)

;  +----------- +-----------+                  +----------------------+ 
;  |            |           |            \     |                      | 
;  |            |           |    +-------+\    |                      | 
;  |            |-----------|    +-------+/    +----------------------+ 
;  |            |           |            /     |          |           | 
;  |            |           |                  |          |           | 
;  +----------- +-----------+                  +----------------------+ 


(defun split-h-3 ()
  "Change 3 window style from vertical to horizontal"
  (interactive)
  (select-window (get-largest-window))
  (if (= 3 (length (window-list)))
      (let ((winList (window-list)))
	    (let ((1stBuf (window-buffer (car winList)))
		  (2ndBuf (window-buffer (car (cdr winList))))
		  (3rdBuf (window-buffer (car (cdr (cdr winList))))))
		(message "%s %s %s" 1stBuf 2ndBuf 3rdBuf)
		(delete-other-windows)
		(split-window-vertically)
		(set-window-buffer nil 1stBuf)
		(other-window 1)
		(set-window-buffer nil 2ndBuf)
		(split-window-horizontally)
		(set-window-buffer (next-window) 3rdBuf)
		(select-window (get-largest-window))
	      )
	    )
    )
)

;  +----------- +-----------+                    +----------- +-----------+ 
;  |            |     C     |            \       |            |     A     | 
;  |            |           |    +-------+\      |            |           | 
;  |     A      |-----------|    +-------+/      |     B      |-----------| 
;  |            |     B     |            /       |            |     C     | 
;  |            |           |                    |            |           | 
;  +----------- +-----------+                    +----------- +-----------+ 
;
;  +------------------------+                     +------------------------+ 
;  |           A            |           \         |           B            | 
;  |                        |   +-------+\        |                        | 
;  +------------------------+   +-------+/        +------------------------+ 
;  |     B     |     C      |           /         |     C     |     A      | 
;  |           |            |                     |           |            | 
;  +------------------------+                     +------------------------+ 


(defun roll-v-3 ()
  "Rolling 3 window buffers clockwise"
  (interactive)
  (select-window (get-largest-window))
  (if (= 3 (length (window-list)))
      (let ((winList (window-list)))
	    (let ((1stWin (car winList))
		  (2ndWin (car (cdr winList)))
		  (3rdWin (car (cdr (cdr winList)))))
	      (let ((1stBuf (window-buffer 1stWin))
		    (2ndBuf (window-buffer 2ndWin))
		    (3rdBuf (window-buffer 3rdWin))
		    )
		    (set-window-buffer 1stWin 3rdBuf)
		    (set-window-buffer 2ndWin 1stBuf)
		    (set-window-buffer 3rdWin 2ndBuf)
		    )
	      )
	    )
    )
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

(defun my-overwrite (&optional arg)
  "Encapsulate overwrite-mode function, to enable red alert in mode-line "
  (interactive "P")
  (if (not overwrite-mode)
      (progn (add-to-list 'mode-line-format (propertize overwrite-mode-textual 'face '(:foreground "white" :background "red") ))
	     (setq my-overwrite-mode-line (car mode-line-format))
	     (overwrite-mode 1)
	     )
    (progn (overwrite-mode 0)
	   (if (memq my-overwrite-mode-line mode-line-format) (setq mode-line-format (delq my-overwrite-mode-line mode-line-format)))
	   )
    )
)




(provide 'dove-ext)
