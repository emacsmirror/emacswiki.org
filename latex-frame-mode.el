;;; latex-frame-mode.el --- minor mode for latex beamer geeks equipped with folding power

;; Copyright (C) 2011  Thomas Alexander Gerds

;; Author: Thomas Alexander Gerds <tagteam@sund.ku.dk>
;; Keywords: convenience, tex
;; Version: 0.0.1 (12 oct 2011)

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

;;;  Installation
;;;
;;; save the file somewhere in the load-path of your (X)emacs and add the
;;; line
;;;    (require 'latex-frame-mode)
;;; to your ~/.xemacs/init.el or ~/.emacs file
;;; if you do not know what the load-path is you may say
;;;    (load-file "/path/to/latex-frame-mode.el")
;;; in your ~/.xemacs/init.el or ~/.emacs file
;;;
;;;  Usage
;;;
;;; In your latex buffer say first
;;;   M-x latex-frame-mode
;;; and somewhere between \begin{document} and \end{document}
;;;   hit `C-c SPACE'
;;;
;;; Code:

(defvar latex-frame-mode nil)
(make-variable-buffer-local 'latex-frame-mode)

(defvar latex-frame-map (make-sparse-keymap)
  "Keymap used for `latex-frame-mode' commands.")

(define-key latex-frame-map "\C-c " 'latex-frame-new-frame)
(define-key latex-frame-map "\C-cn" 'latex-frame-forward-frame)
(define-key latex-frame-map "\C-cp" 'latex-frame-backward-frame)
(define-key latex-frame-map "\C-cb" 'latex-frame-backward-frame)
(define-key latex-frame-map "\C-c1" 'latex-frame-goto-first-frame)
(define-key latex-frame-map "\C-ch" 'latex-frame-mark-this-frame)
(define-key latex-frame-map "\C-cm" 'latex-frame-mark-this-frame)
(define-key latex-frame-map "\C-j" 'newline)
(define-key latex-frame-map "\C-cc" 'latex-frame-refresh)

(or (assq 'latex-frame-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
	  (append minor-mode-map-alist
		  (list (cons 'latex-frame-mode latex-frame-map)))))

(defun latex-frame-mode (&optional arg)
  "A minor mode for latex beamer documents
Activate:        M-x latex-frame-mode
Insert a frame:  \C-c SPACE
Mark a frame:  \C-c m
Goto first frame:  \C-c 1
Goto next frame:  \C-c n
Goto previous frame:  \C-c b
"
  (interactive "P")
  (setq latex-frame-mode
	(not (or (and (null arg) latex-frame-mode)
		 (<= (prefix-numeric-value arg) 0)))))


(or (assq 'latex-frame-mode minor-mode-alist)
              (setq minor-mode-alist
                    (cons '(latex-frame-mode " Beamer") minor-mode-alist)))

(defun latex-frame-goto-first-frame ()
  (interactive)
  (goto-char (point-min))
  (latex-frame-forward-frame))

(defun latex-frame-forward-frame ()
  (interactive)
  (when (re-search-forward "\\\\end{frame}" nil t)
    (re-search-forward "\\\\begin{frame}" nil t)
    ))

(defun latex-frame-beginning () 
  (re-search-backward "\\\\begin{frame}" nil t)
  (beginning-of-line -1))

(defun latex-frame-end () 
  (re-search-forward "\\\\end{frame}" nil t)
  (beginning-of-line 2))

(defun latex-frame-delete-frame ()
  (interactive)
  (latex-frame-mark-this-frame)
  (pending-delete-active-region)
  (latex-frame-refresh-labels))

(defun latex-frame-backward-frame ()
  (interactive)
  (latex-frame-beginning)
  (latex-frame-beginning)
  (previous-line 1))

(defun latex-frame-mark-this-frame ()
  (interactive)
  (latex-frame-beginning)
  (push-mark
   (save-excursion
     (latex-frame-end)
     (point))
   nil t))

(defun latex-frame-folding-top-mark ()
  (or
   (car (cdr (assoc major-mode folding-mode-marks-alist)))
   "% "))

(defun latex-frame-folding-bottom-mark ()
  (or
   (caddr (assoc major-mode folding-mode-marks-alist))
   "% "))

(defun latex-frame-new-frame ()
  (interactive)
  (LaTeX-insert-environment "frame" "\\frametitle{ }")
  (save-excursion
    (beginning-of-line 0)
    (newline)
    (beginning-of-line 0)
    (insert "\n" (latex-frame-folding-top-mark) " Frame 17 \n%%" 
	    (make-string (- fill-column 2) (string-to-char "-")))
    (re-search-forward "\\\\end{frame}" nil nil)
    (insert "\n" (latex-frame-folding-bottom-mark))
    (latex-frame-refresh))
    (when (folding-mode)
      (fold-dwim-show)))

(defun latex-frame-refresh ()
  (interactive)
  (save-excursion
    (latex-frame-refresh-labels)
    (latex-frame-refresh-titles)))

(defun latex-frame-refresh-titles ()
  (interactive)
  (let ((fmode folding-mode))
    (when fmode (folding-mode))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward (concat "\\(^" (latex-frame-folding-top-mark) "[ \t]*Frame[ \t]*[0-9]+\\)\\(.*\\)$") nil t)
	(replace-match (match-string 1))
	(insert " "
		(save-excursion
		  (let* ((fb (re-search-forward "\\\\begin{frame}" nil t))
			 (fe (re-search-forward "\\\\end{frame}" nil t)))
		    (latex-frame-get-title fb fe))))))
    (when fmode (folding-mode))))

       
(defun latex-frame-refresh-labels ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((counter 1))
      (while (re-search-forward (concat "^" (latex-frame-folding-top-mark) "[ \t]*Frame[ \t]*\\([0-9]+\\)") nil t)
	(replace-match (concat (latex-frame-folding-top-mark) " Frame " (int-to-string counter)))
	(setq counter (1+ counter))))
    (save-buffer)))

(defun latex-frame-insert-graph (&optional)
  (interactive)
  (LaTeX-insert-environment "center" (concat "\n\\includegraphics[width=8cm,angle=-90]{" (expand-file-name (default-directory)) "figure.ps}"))
  )

(defun latex-frame-fix-old-style (&optional ignore-header)
  (interactive)
  (goto-char (point-min))
  (unless ignore-header
    (insert (latex-frame-folding-top-mark) "Header\n")
    (re-search-forward "begin{document}")
    (beginning-of-line)
    (insert (latex-frame-folding-top-mark) "\n"))
  (while (re-search-forward "%% Frame" nil t)
    (replace-match "Frame")
    (beginning-of-line)
    (insert  (latex-frame-folding-bottom-mark) " ")
    (let ((ppp (progn (end-of-line) (point)))
	  (title (if (re-search-forward "frametitle{\\(.*\\)}" nil t)
		     (match-string 1)
		   "")))
      (goto-char ppp)
      (insert " " title))
    (latex-frame-end)
    (insert (latex-frame-folding-bottom-mark))))

(defun latex-frame-get-title (&optional frame-beg frame-end)
  (save-excursion
    (goto-char frame-beg)
    (cond
     ((re-search-forward "\\\\frametitle{\\(.*\\)" frame-end t)
      (replace-in-string (match-string 1) "}$" ""))
     ((progn
	(goto-char frame-beg)
	(re-search-forward "\\\\titlepage" frame-end t))
      "Titlepage")
     ((progn
	(goto-char frame-beg)
	(re-search-forward "\\\\includegraphics.*{\\(.*\\.ps\\)}" frame-end t)
	(match-string 1)))
     (t "Untitled"))))
     
(defun latex-frame-make-folds (&optional ignore-header)
  (interactive)
  (let* ((marks (cdr (assoc major-mode folding-mode-marks-alist)))
	 (folding-start (car marks))
	 (folding-end (cadr marks)))
    (unless ignore-header
      (goto-char (point-min))
      (if (re-search-forward (concat "^" folding-start ".*Header.*$") nil t)
	  (re-search-forward (concat "^" folding-end))
	(insert folding-start " Header\n")
	(re-search-forward "begin{document}")
	(beginning-of-line)
	(insert (concat folding-end "\n"))))
    (while (re-search-forward "\\\\begin{frame}" nil t)
      (let ((fb (re-search-backward "\\\\begin{frame}" nil t))
	    (fe (re-search-forward "\\\\end{frame}" nil t)))
	(goto-char fb)
	(insert "\n" (latex-frame-folding-top-mark) " Frame 1 " (latex-frame-get-title fb fe) "\n") 
	(goto-char (re-search-forward "\\\\end{frame}" nil t))
	(insert "\n" (latex-frame-folding-bottom-mark) " ")))
    (latex-frame-refresh-labels)))

(defun latex-frame-fix-folds (&optional ignore-header)
  (interactive)
  (let* ((marks (cdr (assoc major-mode folding-mode-marks-alist)))
	 (folding-start (car marks))
	 (folding-end (cadr marks)))
    (unless ignore-header
      (goto-char (point-min))
      (if (re-search-forward (concat "^" folding-start ".*Header.*$") nil t)
	  (re-search-forward (concat "^" folding-end))
	(insert folding-start " Header\n")
	(re-search-forward "begin{document}")
	(beginning-of-line)
	(insert (concat folding-end "\n"))))
    (while (re-search-forward "\\\\begin{frame}" nil t)
      (let ((fb (re-search-backward "\\\\begin{frame}" nil t))
	    (fe (re-search-forward "\\\\end{frame}" nil t)))
	(goto-char fb)
	(insert "\n" (latex-frame-folding-top-mark) " Frame 1 " (latex-frame-get-title fb fe) "\n") 
	(goto-char (re-search-forward "\\\\end{frame}" nil t))
	(insert "\n" (latex-frame-folding-bottom-mark) " ")))
    (latex-frame-refresh-labels)))

(defun latex-frame-remove-whitelines ()
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward "^[ \t]*$" nil t)
    (kill-entire-line)))


(defun latex-frame-fix-folds-this-frame ()
  (interactive)
  (narrow-to-region
   (progn (latex-frame-beginning) (point))
   (progn (latex-frame-end) (point)))
  (latex-frame-make-folds t)
  (widen))

(defun replace-in-string (string regexp newtext)
  "In STRING, replace all matches for REGEXP with NEWTEXT.
Hack to get a common function for all Emacsen.  Note that Oort Gnus
has
`gnus-replace-in-string', but we don't want to load Gnus."
  (cond
   ;; Emacs 21 and later
   ((fboundp 'replace-regexp-in-string)
    (replace-regexp-in-string regexp newtext string))
   ;; Emacs < 21; XEmacs
   (t
    ;; Code duplicated from `subr.el' revision 1.423 of Emacs. Neither
    ;; `replace-in-string' from XEmacs 21.4.15 nor the Gnus replacement works
    ;; correctly when an empty string is matched.
    (let ((rep newtext)
	  (l (length string))
	  (start 0) ;; (or start 0) in `subr.el'
	  fixedcase literal subexp
	  matches str mb me)
      (save-match-data
	(while (and (< start l) (string-match regexp string start))
	  (setq mb (match-beginning 0)
		me (match-end 0))
	  ;; If we matched the empty string, make sure we advance by
	  one char
	  (when (= me mb) (setq me (min l (1+ mb))))
	  ;; Generate a replacement for the matched substring.
	  ;; Operate only on the substring to minimize string consing.
	  ;; Set up match data for the substring for replacement;
	  ;; presumably this is likely to be faster than munging the
	  ;; match data directly in Lisp.
	  (string-match regexp (setq str (substring string mb me)))
	  (setq matches
		(cons (replace-match (if (stringp rep)
					 rep
				       (funcall rep (match-string 0
								  str)))
				     fixedcase literal str subexp)
		      (cons (substring string start mb) ; unmatched
			    prefix
			    matches)))
	  (setq start me))
	;; Reconstruct a string from the pieces.
	(setq matches (cons (substring string start l) matches)) ;
	leftover
	(apply #'concat (nreverse matches)))))))
  

(provide 'latex-frame-mode)
;;; latex-frame-mode.el ends here
