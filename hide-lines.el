;;; hide-lines.el --- Commands for hiding lines based on a regexp
;; 
;;; Author: Mark Hulme-Jones <ture at plig cucumber dot net>
;; 
;;; History
;; 
;; 24/03/2004 - Incorporate fix for infinite loop bug from David
;; Hansen
;; 
;;; Commentary
;; 
;; The simplest way to make hide-lines work is to add the following
;; lines to your .emacs file:
;; 
;; (autoload 'hide-lines "hide-lines" "Hide lines based on a regexp" t)
;; (global-set-key "\C-ch" 'hide-lines)
;; 
;; Now, when you type C-c h, you will be prompted for a regexp
;; (regular expression).  All lines matching this regexp will be
;; hidden in the buffer.
;; 
;; Alternatively, you can type C-u C-c h (ie. provide a prefix
;; argument to the hide-lines command) to hide all lines that *do not*
;; match the specified regexp.
;; 
;; If you want to make all of the hidden areas re-appear again, type:
;; M-x show-all-invisible
;; Or you can bind show-all-invisible to a key and use that to show
;; all the hidden areas again.

(defvar invisible-areas-list ()
 "List of invisible overlays used by hidelines")

(add-to-invisibility-spec 'hl)

(defun hide-lines (&optional arg)
  "Hide lines matching the specified regexp.
With prefix arg: Hide lines that do not match the specified regexp"
  (interactive "p")
  (if (> arg 1)
      (call-interactively 'hide-non-matching-lines)
      (call-interactively 'hide-matching-lines)))

(defun add-invisible-overlay (start end)
  "Add an overlay from `start' to `end' in the current buffer.  Push the
overlay onto the invisible-areas-list list"
  (let ((overlay (make-overlay start end)))
    (setq invisible-areas-list (cons overlay invisible-areas-list))
    (overlay-put overlay 'invisible 'hl)))

(defun hide-non-matching-lines (search-text)
  "Hide lines that don't match the specified regexp."
  (interactive "MHide lines not matched by regexp: ")
  (make-variable-buffer-local 'line-move-ignore-invisible)
  (setq line-move-ignore-invisible t)
  (save-excursion 
    (goto-char (point-min))
    (let ((start-position (point-min))
          (pos (re-search-forward search-text nil t)))
      (while pos
        (beginning-of-line)
        (add-invisible-overlay start-position (point))
        (forward-line 1)
        (setq start-position (point))
        (if (eq (point) (point-max))
            (setq pos nil)
          (setq pos (re-search-forward search-text nil t))))
      (add-invisible-overlay start-position (point-max)))))

(defun hide-matching-lines  (search-text)
  "Hide lines matching the specified regexp."
  (interactive "MHide lines matching regexp: ")
  (make-variable-buffer-local 'line-move-ignore-invisible)
  (setq line-move-ignore-invisible t)
  (save-excursion
    (goto-char (point-min))
    (let ((pos (re-search-forward search-text nil t))
          start-position)
      (while pos
        (beginning-of-line)
        (setq start-position (point))
        (end-of-line)
        (add-invisible-overlay start-position (+ 1 (point)))
        (forward-line 1)
        (if (eq (point) (point-max))
            (setq pos nil)
          (setq pos (re-search-forward search-text nil t)))))))

(defun show-all-invisible ()
  "Show all areas hidden by the filter-buffer command"
  (interactive)
  (mapcar (lambda (overlay) (delete-overlay overlay)) 
          invisible-areas-list)
  (setq invisible-areas-list ()))

(provide 'hide-lines)
