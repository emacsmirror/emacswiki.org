;;; project-buffer-occur.el --- Occur functionality for Project Mode
;;
;; Author:      Cedric Lallain <kandjar76@hotmail.com>
;; Version:     1.1
;; Keywords:    occur project buffer makefile filesystem management
;; Description: Occur Functionality for Project-Buffer-Mode
;; Tested with: GNU Emacs 22.x and GNU Emacs 23.x
;;
;; This file is *NOT* part of GNU Emacs.
;;
;;    This program is free software; you can redistribute it and/or modify
;;    it under the terms of the GNU General Public License as published by
;;    the Free Software Foundation; either version 2 of the License, or
;;    (at your option) any later version.
;;
;;    This program is distributed in the hope that it will be useful,
;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;    GNU General Public License for more details.
;;
;;    You should have received a copy of the GNU General Public License
;;    along with this program; if not, write to the Free Software
;;    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;

;;; Summary:
;;

;; This is an extension for project-buffer-mode.
;;
;; Provide a 'occur' like functionality for project-buffer-mode.
;;


;;; Commentary:
;;

;; HOW TO USE IT:
;;
;; Call the command `project-buffer-occur' in a project-buffer-mode buffer.
;;
;; The research will occur in all marked files; or in all files
;; belonging to the current project if there are no files.  Using the
;; prefix argument, the research will be done in all files.
;;
;;
;; TO INSTALL IT:
;;
;; Put the following lines in your .emacs:
;;
;; (eval-after-load "project-buffer-mode"
;;  '(progn
;;    (require 'project-buffer-occur)
;;    (define-key project-buffer-mode-map [(control ?f)] 'project-buffer-occur)))
;;
;;
;; KEY BINDINGS:
;;
;; <RET> - goto-occurence
;;  o    - goto-occurence other window
;;  v    - display occurrence
;;  n    - next occurence / prev search occurrence
;;  p    - prev occurence / next search occurrence
;;  M-n  - go to next file
;;  M-p  - go to prev file
;;  C-n  - go to next occurrence and display it
;;  C-p  - go to the previous occurrence and display it
;;  r    - rename buffer
;;  g    - refresh the research
;;  d    - delete the current line
;;  q    - quit-window
;;  ?    - show brief help


;;; History:
;;
;; v1.0: First official release.
;; v1.1: Bugs fixed:
;;       - (goto-char (point-min)) was not working.
;;       - file-name weren't attached properly to the occurrences
;;       - non-existing files were stopping the research / they are now skipped.
;;


(require 'project-buffer-mode)



;;; Code:


(defgroup project-buffer-occur nil
  "An occur mode for project-buffer.")



;;
;;  Global configuration variable:
;;


(defvar project-buffer-occur-context-size 32
  "Size of the context stored for each occurrence; to help retrieving the data after modification.")


(defface project-buffer-occur-file-line
  '((((class color) (background light)) (:foreground "blue"))
    (((class color) (background dark)) (:foreground "yellow")))
  "Project buffer occur face used to highlight file line."
  :group 'project-buffer-occur)


(defface project-buffer-occur-line-number
  '((((class color) (background light)) (:foreground "red"))
    (((class color) (background dark)) (:foreground "cyan")))
  "Project buffer occur face used to highlight line number."
  :group 'project-buffer-occur)


(defface project-buffer-occur-odd-matching-line
  '((((class color) (background light)) (:foreground "black"))
    (((class color) (background dark)) (:foreground "white")))
  "Project buffer occur face used to highlight odd matching line."
  :group 'project-buffer-occur)


(defface project-buffer-occur-even-matching-line
  '((((class color) (background light)) (:foreground "gray60"))
    (((class color) (background dark)) (:foreground "gray")))
  "Project buffer occur face used to highlight even matching line."
  :group 'project-buffer-occur)


(defface project-buffer-occur-highlight-matching-string
  '((((class color) (background light)) (:background "yellow"))
    (((class color) (background dark)) (:background "yellow")))
  "Project buffer occur face used to highlight the matching string."
  :group 'project-buffer-occur)


(defcustom project-buffer-occur-mode-hook nil
  "Post `project-buffer-occur-mode' initialization hook."
  :type 'hook
  :group 'project-buffer-occur)



;;
;;  Local variables:
;;


(defvar project-buffer-occur-saved-project-buffer nil)
(defvar project-buffer-occur-saved-regexp nil)



;;
;;  Key Bindings:
;;


;; Define the key mapping for the spu mode:
(defvar project-buffer-occur-map
  (let ((project-buffer-occur-map (make-keymap)))
    (define-key project-buffer-occur-map [return] 'project-buffer-occur-goto-occurrence)
    (define-key project-buffer-occur-map [?o] 'project-buffer-occur-goto-occurrence-other-window)
    (define-key project-buffer-occur-map [?v] 'project-buffer-occur-view-occurrence)
    (define-key project-buffer-occur-map [?n] 'project-buffer-occur-next-occurrence)
    (define-key project-buffer-occur-map [?p] 'project-buffer-occur-previous-occurrence)
    (define-key project-buffer-occur-map [(meta ?n)] 'project-buffer-occur-next-file)
    (define-key project-buffer-occur-map [(meta ?p)] 'project-buffer-occur-previous-file)
    (define-key project-buffer-occur-map [(control ?n)] 'project-buffer-occur-view-next-occurrence)
    (define-key project-buffer-occur-map [(control ?p)] 'project-buffer-occur-view-previous-occurrence)
    (define-key project-buffer-occur-map [?d] 'project-buffer-occur-delete-line)
    (define-key project-buffer-occur-map [?q] 'quit-window)
    (define-key project-buffer-occur-map [?r] 'project-buffer-occur-rename-buffer)
    (define-key project-buffer-occur-map [?g] 'project-buffer-occur-refresh)
    (define-key project-buffer-occur-map [??] 'project-buffer-occur-help)
    (define-key project-buffer-occur-map [mouse-2] 'project-buffer-occur-mouse-find-file)
    project-buffer-occur-map))


;;
;;  Functions:
;;


(defun project-buffer-occur-clear-overlays()
  "Clear the project-buffer-occur overlays from the current buffer."
  (let ((ovl-lists (overlay-lists)))
    (mapcar (lambda (overlay)
	      (when (overlay-get overlay 'project-buffer-occur-tag)
		(delete-overlay overlay)))
	    (and ovl-lists
		 (append (car ovl-lists) (cdr ovl-lists))))))


(defun project-buffer-occur-get-and-clear-occur-buffer()
  "Retrieve the occur buffer and returns it.
If the buffer exists; the buffer is cleared.  If the buffer
doesn't exist, a new buffer is created and initialized with
project-buffer-occur-major-mode."
  (let ((buffer (get-buffer-create "*Project-Buffer-Occur*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
	(project-buffer-occur-clear-overlays)
	(erase-buffer))
      (project-buffer-occur-mode))
    buffer))


(defun project-buffer-occur-add-occurrence(file occurrence occurrence-num regexp)
  "Add an OCCURRENCE from FILE in the buffer.

FILE should be the file in which the occurrence has been found,
OCCURRENCE is a list of (#line matching-line before-string after-string).
OCCURRENCE-NUM represents the OCCURENCE-NUM'th occurrence found in FILE"

  (let ((occ-line-num   (car occurrence))
	(occ-line-str   (nth 1 occurrence))
	(occ-before-str (nth 2 occurrence))
	(occ-after-str  (nth 3 occurrence))
	(start-pos      (point))
	(cur-line       (line-number-at-pos (point))))
    (insert (propertize (format "%8i:" occ-line-num)
			'follow-link t
			'mouse-face 'highlight
			'face 'project-buffer-occur-line-number))
    (insert " ")
    (insert (propertize occ-line-str
			'follow-link t
			'mouse-face 'highlight
			'face (if (oddp occurrence-num)
				  'project-buffer-occur-odd-matching-line
				  'project-buffer-occur-even-matching-line)))
    (when (not (= (point) (point-at-bol)))
      (insert "\n"))

    ;; Highlight matching string:
    (goto-char start-pos)
    (forward-char 10) ; skip the line number
    (while (re-search-forward regexp nil t)
      (let ((overlay (make-overlay (match-beginning 0) (match-end 0))))
	(overlay-put overlay 'face 'project-buffer-occur-highlight-matching-string)
	(overlay-put overlay 'project-buffer-occur-tag t)
	))

    ;; Fix the indentation:
    (goto-char (point-max))
    (forward-line -1)
    (while (not (= cur-line (line-number-at-pos (point))))
      (goto-char (point-at-bol))
      (insert (propertize (make-string 10 32)
			  'follow-link t
			  'mouse-face 'highlight))
      (forward-line -1))

    ;; Data overlay:
    (let ((overlay (make-overlay start-pos (point-max))))
      (overlay-put overlay 'project-buffer-occur-tag t)
      (overlay-put overlay 'project-buffer-occur-context (list file occurrence regexp)))
    (goto-char (point-max))))


(defun project-buffer-occur-collect-occurrences(regexp)
  "Create a list of occurrences by searching REGEXP in the current buffer.

The return value is a list of ( line# matching-line before-string
after-string ).  This function doesn't save the position and
assume the position will be saved and restored by the caller if
required."
  (goto-char (point-min))
  (save-match-data
    (let (occurrences
	  next-start
	  occ-beg
	  occ-end
	  occ-bol
	  occ-eol
	  occ-line-num
	  occ-line-str
	  occ-before-str
	  occ-after-str)
      (setq next-start (re-search-forward regexp nil t))
      (while next-start
	;; Collect the data for this occcurrence:
	;;  consider using: jit-lock-fontify-now! To get colors on the line...
	(setq occ-beg (match-beginning 0))
	(setq occ-end (match-end 0))
	(goto-char occ-beg)
	(setq occ-bol (point-at-bol))
	(setq occ-line-num (line-number-at-pos))
	(goto-char occ-end)
	(setq occ-eol (point-at-eol))
	(setq occ-line-str (buffer-substring-no-properties occ-bol occ-eol))
	(setq occ-after-str (and (>= (- (point-max) occ-eol) project-buffer-occur-context-size)
				 (buffer-substring-no-properties occ-eol (+ occ-eol project-buffer-occur-context-size))))
	(setq occ-before-str (and (>= (- occ-bol (point-min)) project-buffer-occur-context-size)
				  (buffer-substring-no-properties (- occ-bol project-buffer-occur-context-size) occ-bol)))
	;; Add the occurrence to the list unless it occurs on the same line.
	(unless (eq occ-line-num (car (car occurrences)))
	  (setq occurrences (cons (list occ-line-num occ-line-str occ-before-str occ-after-str)
				  occurrences)))
	;; Carry on:
	(goto-char next-start)
	(setq next-start (re-search-forward regexp nil t)))
      (reverse occurrences))))


(defun project-buffer-occur-research(project-file-name file-path project-name regexp occur-buffer)
  "Research REGEXP in FILE-PATH and fill OCCUR-BUFFER with the
different occurences found.
PROJECT-FILE-NAME and PROJECT-NAME are ignored."
  (let (occurrences)
    (message "Project '%s' -- Searching in '%s'" project-name file-path)
    ;; Collect all occurrences in this file:
    (let ((file-buf (get-file-buffer file-path)))
      (if file-buf
	  (with-current-buffer file-buf
	    (save-excursion
	      (setq occurrences (project-buffer-occur-collect-occurrences regexp))))
	  (when (file-exists-p file-path)
	    (with-temp-buffer
	      (insert-file-contents file-path)
	      (setq occurrences (project-buffer-occur-collect-occurrences regexp))))))

    ;; Then populate the occurr buffer with it:
    (when occurrences
      (with-current-buffer occur-buffer
	(let ((inhibit-read-only t))
	  (goto-char (point-max))
	  (let ((start-pos (point)))
		(insert (propertize (format "%i occurrence%s found in %s/%s"
					    (length occurrences)
					    (if (= 1 (length occurrences)) "" "s")
					    project-name
					    project-file-name)
				    'follow-link t
				    'mouse-face 'highlight
				    'face 'project-buffer-occur-file-line))
		(let ((overlay (make-overlay start-pos (point))))
		  (overlay-put overlay 'project-buffer-occur-tag t)
		  (overlay-put overlay 'project-buffer-occur-context (list file-path nil regexp))))
	  (insert "\n")
	  (let ((occ-count 1))
	    (while occurrences
	      (let ((occurrence (pop occurrences)))
		(project-buffer-occur-add-occurrence file-path occurrence occ-count regexp)
		(setq occ-count (1+ occ-count))))))))))


(defun project-buffer-occur-mode()
  "Major mode for output from `project-buffer-occur'

Commands:
\\{project-buffer-mode-map}"
  (kill-all-local-variables)
  (use-local-map project-buffer-occur-map)
  ;;
  (setq major-mode 'project-buffer-occur-mode)
  (setq mode-name "pbm-occur")
  ;;
  (make-local-variable 'project-buffer-occur-saved-project-buffer)
  (make-local-variable 'project-buffer-occur-saved-regexp)
  ;;
  (setq buffer-read-only t)
  (setq buffer-undo-list t) ; disable undo recording
  (run-mode-hooks 'project-buffer-occur-mode-hook))


(defun project-buffer-occur-goto-file(file &optional other-window)
  "Go to the selected files."
  (if other-window
      (find-file-other-window file)
      (find-file file)))


(defun project-buffer-occur-highlight-current(regexp start end)
  "Highlight all occurrences of REGEXP between START adn END points"
  (let (ovl-list)
    (unwind-protect
	(save-match-data
	  (save-excursion
	    (goto-char start)
	    (while (re-search-forward regexp end t)
	      (let ((overlay (make-overlay (match-beginning 0) (match-end 0))))
		(overlay-put overlay 'face 'project-buffer-occur-highlight-matching-string)
		(overlay-put overlay 'project-buffer-occur-tag t)
		(setq ovl-list (cons overlay ovl-list)))))
	  (sit-for 10))
      (mapcar (lambda (overlay)
		(delete-overlay overlay))
	      ovl-list))))


(defun project-buffer-occur-goto-matching-string(file line matching-line before-string after-string regexp &optional other-window)
  "Go to an occurrence."
  (let* ((buffer (find-file-noselect file))
	(window (get-buffer-window buffer)))
    (if window
	(progn (select-window window)
	       (set-buffer buffer))
	(if other-window
	    (switch-to-buffer-other-window buffer)
	    (switch-to-buffer buffer)))
    (save-restriction
      (widen)
      (goto-char (point-min))
      (forward-line (1- line)))

    ;;
    (let ((cur-pt (point))
	  (end-pt (+ (point) (length matching-line) 1))
	  aft-pt
	  bef-pt
	  found)
      (when (and after-string (search-forward after-string nil t))
	(setq aft-pt (match-beginning 0))
	(goto-char aft-pt))
      (when (and before-string (search-backward before-string nil t))
	(setq bef-pt (match-end 0))
	(goto-char bef-pt))

      (cond
       ((and aft-pt bef-pt)
	(if (search-forward matching-line aft-pt t)
	    (progn (goto-char (match-beginning 0))
		   (goto-char (point-at-bol)))
	    (if (re-search-forward regexp aft-pt t)
		(progn (goto-char (match-beginning 0))
		       (goto-char (point-at-bol)))
		(goto-char cur-pt))))
       (aft-pt
	(goto-char cur-pt)
	(if (search-forward matching-line aft-pt t)
	    (progn (goto-char (match-beginning 0))
		   (goto-char (point-at-bol)))
	    (if (re-search-forward regexp aft-pt t)
		(progn (goto-char (match-beginning 0))
		       (goto-char (point-at-bol)))
		(goto-char cur-pt))))
       (bef-pt
	(if (search-forward matching-line end-pt t)
	    (progn (goto-char (match-beginning 0))
		   (goto-char (point-at-bol)))
	    (if (re-search-forward regexp end-pt t)
		(progn (goto-char (match-beginning 0))
		       (goto-char (point-at-bol)))
		(goto-char cur-pt))))
       (t
	(goto-char cur-pt)
	(if (search-forward matching-line end-pt t)
	    (progn (goto-char (match-beginning 0))
		   (goto-char (point-at-bol)))
	    (if (re-search-forward regexp end-pt t)
		(progn (goto-char (match-beginning 0))
		       (goto-char (point-at-bol)))
		(if (search-forward matching-line nil t)
		    (progn (goto-char (match-beginning 0))
			   (goto-char (point-at-bol)))
		    (if (search-backward matching-line nil t)
			(progn (goto-char (match-beginning 0))
			       (goto-char (point-at-bol)))
			(goto-char cur-pt))))))))))


(defun project-buffer-occur-goto-occurrence-at-pos(pos other-window)
  "Go to the occurence found at POS."
  (let (context)
    ;; Check if there is a context at that line:
    (mapcar (lambda (overlay) (when (overlay-get overlay 'project-buffer-occur-context)
				(setq context (overlay-get overlay 'project-buffer-occur-context))))
	    (overlays-at pos))
    (unless context
      (error "No occurrence on this line"))
    (let ((file-name (car context))
	  (occurrence (nth 1 context))
	  (regexp (nth 2 context)))
      (if occurrence
	  (let ((occ-line-num   (car occurrence))
		(occ-line-str   (nth 1 occurrence))
		(occ-before-str (nth 2 occurrence))
		(occ-after-str  (nth 3 occurrence)))
	    (project-buffer-occur-goto-matching-string file-name occ-line-num occ-line-str occ-before-str occ-after-str regexp other-window)
	    (project-buffer-occur-highlight-current regexp (point) (+ (point) (length occ-line-str))))
	  (project-buffer-occur-goto-file file-name other-window)))))


(defun project-buffer-occur-delete-line()
  "Delete the current occurrence line from this buffer."
  (interactive)
  (goto-char (point-at-bol))
  (let ((start (point))
	end
	(inhibit-read-only t))
    (if (looking-at "^[0-9]+ occurrence")
	(progn (project-buffer-occur-next-file)
	       (setq end (if (eq start (point)) (point-max) (point)))
	       (delete-region start end))
	(progn (forward-line 1)
	       (setq end (point))
	       (save-excursion
		 (save-match-data
		   (project-buffer-occur-previous-file)
		   (looking-at "^[0-9]+")
		   (let ((num (string-to-int (buffer-substring-no-properties (match-beginning 0) (match-end 0)))))
		     (if (= num 1)
			 (setq start (point))
			 (progn (delete-region (match-beginning 0) (match-end 0))
				(insert (propertize (format "%i" (1- num))
						    'follow-link t
						    'mouse-face 'highlight
						    'face 'project-buffer-occur-file-line)))))))
	       (delete-region start end)))))


;;
;;  Interactive commands:
;;


(defun project-buffer-occur-mouse-find-file(event)
  "Goto the selected occurrence."
  (interactive "e")
  (set-buffer (window-buffer (posn-window (event-end event))))
  (project-buffer-occur-goto-occurrence-at-pos (posn-point (event-end event)) t))


(defun project-buffer-occur-goto-occurrence()
  "Goto the selected occurrence."
  (interactive)
  (project-buffer-occur-goto-occurrence-at-pos (point) nil))


(defun project-buffer-occur-goto-occurrence-other-window()
  "Goto the selected occurrence in another window."
  (interactive)
  (project-buffer-occur-goto-occurrence-at-pos (point) t))


(defun project-buffer-occur-view-occurrence()
  "View the selected occurrence without leaving the project-buffer."
  (interactive)
  (let ((buffer (current-buffer)))
    (project-buffer-occur-goto-occurrence-at-pos (point) t)
    (let ((window (get-buffer-window buffer)))
      (when window
	(select-window window)))))


(defun project-buffer-occur-next-occurrence()
  "Go to the next occurrence."
  (interactive)
  (forward-line 1)
  (goto-char (point-at-bol))
  (while (and (not (eobp))
	      (looking-at "^[0-9]+ occurrence"))
    (forward-line 1)))


(defun project-buffer-occur-previous-occurrence()
  "Go to the next occurrence."
  (interactive)
  (forward-line -1)
  (goto-char (point-at-bol))
  (while (and (not (bobp))
	      (looking-at "^[0-9]+ occurrence"))
    (forward-line -1))
  (if (and (bobp)
	   (looking-at "^[0-9]+ occurrence"))
      (project-buffer-occur-next)))


(defun project-buffer-occur-next-file()
  "Go to the next file."
  (interactive)
  (let ((current (point)))
    (forward-line 1)
    (goto-char (point-at-bol))
    (while (and (not (eobp))
		(not (looking-at "^[0-9]+ occurrence")))
      (forward-line 1))
    (unless (looking-at "^[0-9]+ occurrence")
      (goto-char current))))


(defun project-buffer-occur-previous-file()
  "Go to the next file."
  (interactive)
  (let ((current (point)))
    (forward-line -1)
    (goto-char (point-at-bol))
    (while (and (not (eobp))
		(not (looking-at "^[0-9]+ occurrence")))
      (forward-line -1))
    (unless (looking-at "^[0-9]+ occurrence")
      (goto-char current))))


(defun project-buffer-occur-view-next-occurrence()
  "Go to the next occurrence and view it."
  (interactive)
  (project-buffer-occur-next-occurrence)
  (project-buffer-occur-view-occurrence))


(defun project-buffer-occur-view-previous-occurrence()
  "Go to the next occurrence."
  (interactive)
  (project-buffer-occur-previous-occurrence)
  (project-buffer-occur-view-occurrence))


(defun project-buffer-occur-help ()
  "Display help for `project-buffer-occur' mode."
  (interactive)
  (describe-function 'project-buffer-occur-mode))


(defun project-buffer-occur-rename-buffer()
  "Rename the buffer; make its name uniq."
  (interactive)
  (let ((new-name (format "*Project-Buffer-Occur:%s*" project-buffer-occur-saved-project-buffer)))
    (rename-buffer new-name t)))


(defun project-buffer-occur-refresh()
  "Refresh the buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (project-buffer-occur-clear-overlays)
    (erase-buffer)
    (let ((regexp       (nth 0 project-buffer-occur-saved-regexp))
	  (all-files    (nth 1 project-buffer-occur-saved-regexp))
	  (project      (nth 2 project-buffer-occur-saved-regexp))
	  (occur-buffer (current-buffer)))
      ;; Fill the occur buffer with all occurrences:
      (save-excursion
	(set-buffer project-buffer-occur-saved-project-buffer)
	(if all-files
	    (project-buffer-apply-to-each-file 'project-buffer-occur-research regexp occur-buffer)
	    (unless (project-buffer-apply-to-marked-files 'project-buffer-occur-research regexp occur-buffer)
	      (project-buffer-apply-to-project-files project 'project-buffer-occur-research regexp occur-buffer)))))))



;;
;;  Entry command:
;;


(defun project-buffer-occur(regexp all-files)
  "Search REGEXP in the project files; if ALL-FILES is t the
research will occur in all project's files; if ALL-FILES is
false, the research will occur in all marked files unless there
are none in which case it will occur in all files of the current
project (current project is determined by the cursor position)."
  (interactive
   (list (project-buffer-read-regexp (format "List lines matching regexp%s: " (if current-prefix-arg " [all files]" "")))
	 current-prefix-arg))
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (unless (and regexp (not (string-equal regexp "")))
    (error "Invalid regexp"))
  ;; Generate an occur buffer:
  (let ((pb-buffer (current-buffer)))
    (let ((occur-buffer (project-buffer-occur-get-and-clear-occur-buffer))
	  (project-name (project-buffer-get-current-project-name))
	  (project-directory default-directory))
      ;; Set the local variable:
      (with-current-buffer occur-buffer
	(cd project-directory)
	(setq project-buffer-occur-saved-project-buffer pb-buffer)
	(setq project-buffer-occur-saved-regexp (list regexp all-files project-name)))
      ;; Fill the occur buffer with all occurrences:
      (if all-files
	  (project-buffer-apply-to-each-file 'project-buffer-occur-research regexp occur-buffer)
	  (unless (project-buffer-apply-to-marked-files 'project-buffer-occur-research regexp occur-buffer)
	    (project-buffer-apply-to-project-files project-name
						   'project-buffer-occur-research regexp occur-buffer)))
      (with-current-buffer occur-buffer
	(goto-char (point-min)))
      (display-buffer occur-buffer)
      (message "Done.")
)))


;;

(provide 'project-buffer-occur)

;;; project-buffer-occur.el ends here
