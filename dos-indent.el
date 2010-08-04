;;; dos-indent.el --- Adds indentation to Dos-mode

;; Copyright (C) 2009  Matthew Fidler

;;; Commentary:

;; Adds indentation to Dos-mode
;; Copied and modified from Batch-mode
;; Install by adding to load-path, and then:
;;   (require 'dos-indent)

;;; Code:

(defcustom dos-indent-level 4
  "Amount by which batch subexpressions are indented."
  :type 'integer
  :group 'dos)

(defun dos-indent-line ()
  "Indent current line as batch script"
  (message "Indent line")
  (let ((indent (dos-calculate-indent))
	beg shift-amt 
	(old-pos (- (point-max) (point))))
    (beginning-of-line)
    (setq beg (point))
    (skip-chars-forward " \t")
    (if (looking-at ")")
	(setq indent (max (- indent dos-indent-level))))
    (message "prev indent: %d" indent)
    (setq shift-amt (- indent (current-column)))
    (if (not (zerop shift-amt))
	(progn
	  (delete-region beg (point))
	  ; ArcView replaces tabs with single spaces, so we only insert
	  ; spaces to make indentation correct in ArcView.
	  (insert-char ?  indent)
	  (if (> (- (point-max) old-pos) (point))
	      (goto-char (- (point-max) old-pos)))))
    shift-amt))
    
(defun dos-calculate-indent ()
  "Return appropriate indentation for the current line as batch code."
  ;; Lifted from batch-mode and changed for parenthesis indentation.
  (save-excursion
    (beginning-of-line)
    (current-indentation)
    (if (bobp)
	0
      (if (re-search-backward "^[ \t]*[^ \t\n\r]" nil t)
	  (if (looking-at "[ \t]*\\()[ \t]*else\\|if\\|for.*\\<do\\|for\\|)[ \t]*do\\)\\>[^(\n]*([^)\n]*")
	      (+ (current-indentation) dos-indent-level)
	    (if (looking-at "[ \t]*[^(\n]*)[ \t]*")
		(- (current-indentation) dos-indent-level)
	      (current-indentation))
	    )
	0))))

(defun dos-indent ()
  (interactive)
  (set (make-local-variable 'indent-line-function) 'dos-indent-line)
  )
(add-hook 'dos-mode-hook 'dos-indent)
(provide 'dos-indent)
 
