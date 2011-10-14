;;; muse-ess.el --- emacs muse speaks statistics

;; Copyright (C) 2009-2011, Thomas A. Gerds <tag@biostat.ku.dk>

;; Author: Thomas Alexander Gerds <tagteam@sund.ku.dk>
;; Keywords: extensions

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
;; Provides three tags R, Rplot, Rtext for EmacsMuse
;; 
;; Usage:
;;
;; <R tag="muse"> Rcode </R>
;; <R show="before" tag="muse"> Rcode </R>
;; <Rplot file="filename-sans-extension">plot(1:10)</Rplot>
;; <Rtext>some text then $$Rcode$$ then text then code etc</Rtext>
;;
;; Examples:
;;
;; 
;;; Code:


(setq muse-ess-wait-ms 0)

(add-to-list 'muse-publish-markup-tags
	     '("R" t t nil muse-publish-R-tag))

(add-to-list 'muse-publish-markup-tags
	     '("Rtext" t t nil muse-publish-Rtext-tag))

(add-to-list 'muse-publish-markup-tags
	     '("Rplot" t t nil muse-publish-Rplot-tag))

(setq muse-ess-plot-file-name-sep "_")

(defun muse-publish-Rplot-tag (beg end attrs)
  (muse-publish-ensure-block beg end)
  (muse-publish-markup-attribute
      beg end attrs 'verse
    (save-excursion
      (save-restriction
	(let* ((show (cdr (assoc "show" attrs)))
	       (tag (or (cdr (assoc "tag" attrs)) "muse"))
	       (debug (cdr (assoc "debug" attrs)))
	       (proc (cdr (assoc "proc" attrs)))
	       (type (or (cdr (assoc "type" attrs)) "png"))
	       (width (or (cdr (assoc "width" attrs)) nil))
	       (height (or (cdr (assoc "height" attrs)) nil))
	       (res (or (cdr (assoc "res" attrs)) nil))
	       (title (or (cdr (assoc "title" attrs)) "R code:"))
	       (dpi (or (cdr (assoc "dpi" attrs)) "NA"))
	       (dir (file-name-directory muse-publishing-current-output-path))
	       (muse-file (file-name-sans-extension (file-name-nondirectory (muse-current-file))))
	       (count 0)
	       (file (cond ((cdr (assoc "file" attrs))
			    (concat dir muse-file
				    muse-ess-plot-file-name-sep
				    (cdr (assoc "file" attrs))))
			   (t (while (file-exists-p (concat dir muse-file muse-ess-plot-file-name-sep "plot-" count "." type))
				(setq count (+ count 1)))
			      (file-exists-p (concat dir muse-file muse-ess-plot-file-name-sep "plot-" count "." type)))))
	       file-1
	       (cs (cdr (assoc "coding-system" attrs)))
	       str
	       showcode)
	  (unless (string= (file-name-extension file) type)
	    (setq file (concat file "." type)))
	  (setq file-1 (file-name-nondirectory file))
	  (if show (setq showcode (buffer-substring-no-properties (point-min) (point-max))))
	  (goto-char (point-min))
	  (insert
	   (concat type "(file=\""
		   file
		   "\""
		   (if res (concat ",res=" res))
		   (if width (concat ",width=" width))
		   (if height (concat ",height=" height))
		   ")\n"))
	  (goto-char (point-max))
	  (insert ".off=dev.off()\n")
	  (insert "cat(\"\n[[\",\"" file-1 "\",\"]]\n\",sep=\"\")\n")
	  (message "Processing R code ...")
	  (setq str (muse-eval-ess
		     (prog1
			 (buffer-substring-no-properties
			  (point-min)
			  (point-max))
		       (delete-region (point-min) (point-max))
		       (widen))
		     show showcode title tag debug proc cs t))
	  (set-text-properties 0 (length str) nil str)
	  (insert str))))))

(defun muse-publish-R-tag (beg end attrs)
  (muse-publish-ensure-block beg end)
  (muse-publish-markup-attribute
      beg end attrs 'verse
    (save-excursion
      (save-restriction
	(let* ((show (cdr (assoc "show" attrs)))
	       (title (or (cdr (assoc "title" attrs)) "R code:"))
	       (tag (cdr (assoc "tag" attrs)))
	       (debug (cdr (assoc "debug" attrs)))
	       (proc (cdr (assoc "proc" attrs)))
	       (cs (cdr (assoc "coding-system" attrs)))
	       showcode
	       (str (muse-eval-ess 
		     (prog1
			 (buffer-substring-no-properties
			  (point-min)
			  (point-max))
		       (delete-region (point-min) (point-max))
		       (widen))
		     show showcode title tag debug proc cs t)))
	  (set-text-properties 0 (length str) nil str)
	  (insert str))))))

(defun muse-publish-Rtext-tag (beg end attrs)
  (muse-publish-ensure-block beg end)
  (muse-publish-markup-attribute
      beg end attrs 'verse
    (save-excursion
      (save-restriction
	(let* ((show (cdr (assoc "show" attrs)))
	       (title (or (cdr (assoc "title" attrs)) "R code:"))
	       (tag (cdr (assoc "tag" attrs)))
	       (debug (cdr (assoc "debug" attrs)))
	       (proc (cdr (assoc "proc" attrs)))
	       (cs (cdr (assoc "coding-system" attrs)))
	       showcode
	       (str (muse-ess-eval-text 
		     (prog1
			 (buffer-substring-no-properties
			  (point-min)
			  (point-max))
		       (delete-region (point-min) (point-max))
		       (widen)))))
	  (set-text-properties 0 (length str) nil str)
	  (insert str))))))



(defun muse-eval-ess (code &optional show showcode title tag debug proc coding-system newline)
  "This function is used for muse insinuate ess.
If SHOW is non-nil then the R code is shown in a muse-example tag.
If DEBUG is non-nil then the R error and other messages are shown in a
muse-example tag.
Desirable option: If PROC is non-nil the ESS process proc is used."
  (let* ((Rproc (condition-case nil
		    (get-ess-process (or proc "R") nil)
		  (error nil)))
	 (buf (get-buffer-create "*ess-mus-temp-buffer*"))
	 (buf-1 (when debug (get-buffer-create "*ess-mus-mess-buffer*")))
	 (tag (or tag "src"))
	 (coding-system-for-read  (or coding-system 'iso-8859-1))
	 mess
	 res)
    (unless Rproc
      (save-excursion
	(split-window-vertically)
	(R)				; start R if needed
	(delete-window)))
    (ess-eval-linewise
     "sink(\"/tmp/tag-muse-ess-output.txt\")"
     t nil t 'wait nil muse-ess-wait-ms)
    (when debug
      (ess-eval-linewise "museEssMess <- file(\"/tmp/tag-muse-ess-mess.txt\",open=\"wt\")" t nil t 'wait muse-ess-wait-ms)
      (ess-eval-linewise  "sink(museEssMess,type=\"message\")" t nil t 'wait muse-ess-wait-ms))
    ;; evaluation of R code
    (ess-eval-linewise code 'invisibly nil 'even-empty 'wait muse-ess-wait-ms)
    (when debug
      (ess-eval-linewise  "sink(type=\"message\")" t nil t 'wait muse-ess-wait-ms))
    (ess-eval-linewise "sink(NULL)" t nil t 'wait muse-ess-wait-ms)
    (save-excursion
      (set-buffer buf)
      (insert-file-contents
       "/tmp/tag-muse-ess-output.txt"
       nil nil nil 'replace)
      (cond ((string= tag "nil")
	     (setq res ""))
	    ((string= tag "muse")
	     (setq res (concat (when newline "\n\n") (buffer-string) (when newline "\n\n"))))
	    (t (setq res (concat "\n\n<" tag ">\n\n" (buffer-string) "\n\n</" tag ">\n\n")))))
    (when debug
      (save-excursion
	(set-buffer buf-1)
	(insert-file-contents "/tmp/tag-muse-ess-mess.txt" nil nil nil 'replace)
	(setq mess (buffer-string))))
    (unless showcode (setq showcode code))
    (cond ((and show (string-match show "beforefirstupBefore"))
	   (setq res (concat "\n\n<example>\n" title "\n" showcode "\n</example>\n\n" res)))
	  (show (setq res (concat res "\n\n<example>\n" title "\n" showcode "\n</example>\n\n"))))
    (when debug
      (setq res (concat res "\n\n<example>\n **Debugging messages from the ESS process "
			ess-current-process-name
			"**\n" mess "\n\n</example>\n\n")))
    res))


(defun muse-ess-eval-text (string)
  "Evaluate the R code between a pairs of $$ signs and replace the result."
  (let* ((Rproc
	  (condition-case nil
	      (get-ess-process (or proc "R") nil)
	    (error nil)))
	 (coding-system-for-read 'iso-8859-1)
	 (tbuffer (get-buffer-create " *ess-code*"))
	 (ibuffer (get-buffer-create " *ess-res*")))
    (unless Rproc
      (save-excursion
	(split-window-vertically)
	(R)				; start R if needed
	(delete-window)))
    (save-excursion
      (save-restriction
	(set-buffer tbuffer)
	(erase-buffer)
	(insert string)
	(goto-char (point-min))
	(while (re-search-forward "\\$\\$" nil t)
	  (let ((beg (point))
		(end (progn (re-search-forward "\\$\\$" nil t)
			    (re-search-backward "\\$\\$" nil t))))
	    (ess-command
	     (concat
	      (buffer-substring-no-properties beg end)
	      "\n") ibuffer)
	    (delete-region (- beg 2) (+ 2 end))
	    (insert
	     (replace-in-string
	      (replace-in-string
	       	(save-excursion
		  (set-buffer ibuffer)
		  (buffer-string))
		"^\\[[0-9]\\] \\|^\\[+[0-9]+\\]+\n\\|^\n" "")
	      "\n$" ""))))
	(save-excursion
	  (set-buffer tbuffer)
	  (buffer-string))
	))))




(provide 'muse-ess)
;;; muse-ess.el ends here
