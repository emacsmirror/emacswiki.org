;;; org-table-comment.el ---  Org table comment modes.
;; 
;; Filename: org-table-comment.el
;; Description: Provides Org-table functionality within single line comments 
;; Author: Matthew L. Fidler <matthew dot fidler at gmail . com>
;; Maintainer: Matthew L. Fidler
;; Created: Wed Jan 12 14:51:05 2011 (-0600)
;; Version: 0.2
;; Last-Updated: Thu Apr  7 14:35:07 2011 (-0500)
;;           By: US041375
;;     Update #: 443
;; URL: http://www.emacswiki.org/emacs/download/org-table-comment.el
;; Keywords: org-mode orgtbl
;; Compatibility: Tested with Org 7.4 on Windows Emacs 23.2 
;; 
;; Features that might be required by this library:
;;
;;   `org', `org-table'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; This allows you to create org-tables in languages that do not have
;; block comments.  For example in elisp:
;;
;; |------+----------------+----+-------|
;; |      |                |    | <5>   |
;; | This | is             | a  | table |
;; | that | may be created | in | orgtbl-comment-mode |
;; |------+----------------+----+-------|
;;
;; or
;;
;; |------+----------------+----+---------------------|
;; | This | is             | a  | table               |
;; | that | may be created | in | orgtbl-comment-mode |
;; |------+----------------+----+---------------------|
;;
;; It also supports single-comment radio tables, for example in LaTeX
;; the following now works if you are using the overlays driver for
;; org-table-comment:
;;
;; % BEGIN RECEIVE ORGTBL salesfigures
;; % END RECEIVE ORGTBL salesfigures
;;
;; % #+ORGTBL: SEND salesfigures orgtbl-to-latex
;; % |-------+------+---------+---------|
;; % | Month | Days | Nr sold | per day |
;; % |-------+------+---------+---------|
;; % | Jan   |   23 |      55 |     2.4 |
;; % | Feb   |   21 |      16 |     0.8 |
;; % | March |   22 |     278 |    12.6 |
;; % |-------+------+---------+---------|
;; % #+TBLFM: $4=$3/$2;.1f
;;
;; When editing the table, pressing C-c C-c produces the LaTeX table, as follows:
;;
;; % BEGIN RECEIVE ORGTBL salesfigures
;; \begin{tabular}{lrrr}
;; \hline
;; Month & Days & Nr sold & per day \\
;; \hline
;; Jan & 23 & 55 & 2.4 \\
;; Feb & 21 & 16 & 0.8 \\
;; March & 22 & 278 & 12.6 \\
;; \hline
;; \end{tabular}
;; % END RECEIVE ORGTBL salesfigures
;;
;; % #+ORGTBL: SEND salesfigures orgtbl-to-latex
;; % |-------+------+---------+---------|
;; % | Month | Days | Nr sold | per day |
;; % |-------+------+---------+---------|
;; % | Jan   |   23 |      55 |     2.4 |
;; % | Feb   |   21 |      16 |     0.8 |
;; % | March |   22 |     278 |    12.6 |
;; % |-------+------+---------+---------|
;; % #+TBLFM: $4=$3/$2;.1f
;;
;; NOTE: This requires `comment-region' and `uncomment-region' to work
;; properly in the mode you are using.  Also filling/wrapping in the
;; mode needs to not wrap the orgbls.
;;
;; TODO: Make killing and cutting cut the appropriate lines when
;; inside org-comment-table.
;;
;; Eventually this could allow for R radio tables..?
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;; 07-Apr-2011      
;;    Last-Updated: Thu Apr  7 14:32:50 2011 (-0500) #439 (US041375)
;;    Minor changes.  Beter ELPA support.
;; 18-Jan-2011    Matthew L. Fidler  
;;    Last-Updated: Tue Jan 18 15:23:04 2011 (-0600) #437 (Matthew L. Fidler)
;;    Made permissive text only include a certain number (specified)
;; 13-Jan-2011    Matthew L. Fidler  
;;    Last-Updated: Thu Jan 13 12:44:44 2011 (-0600) #396 (Matthew L. Fidler)
;;    Initial version
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

(require 'org-table)


(defun org-table-comment-p (&optional only-comment allow-blanks)
  "Determines if you are in an either a org-table inside single comments or an org-table.
if ONLY-COMMENT is true only consider org-table inside single comments
IF ALLOW-BLANKS, a blank line is considered an org-table-comment"
  (save-excursion
    (goto-char (point-at-bol))
    (let (ret)
      (if (not (and (< 0 org-table-comment-permissive)
		    (not org-table-comment-permissive-text)))
          (setq ret 
                (looking-at (concat (if allow-blanks "\\(?:^[ \t]*$\\|" "")
                                    (if only-comment "" "\\(?:")
                                    "[ \t]*" comment-start "+"
                                    (if only-comment "" "\\)?")
                                    (if org-table-comment-permissive-text
                                        (regexp-quote org-table-comment-permissive-text)
                                      "")
                                    orgtbl-line-start-regexp
                                    (if allow-blanks "\\)" ""))))
        (setq ret (looking-at (concat (if allow-blanks "\\(?:^[ \t]*$\\|" "")
                                      (if only-comment "" "\\(?:")
                                      "[ \t]*" comment-start "+"
                                      (if only-comment "" "\\)?")
                                      (if org-table-comment-permissive "\\(.*?[ \t]*\\)" "")
                                      orgtbl-line-start-regexp
                                      (if allow-blanks "\\)" ""))))
        (when ret
          (setq org-table-comment-permissive-text (match-string-no-properties 1))
	  (if (< org-table-comment-permissive (length org-table-comment-permissive-text))
	      (progn
		(setq ret nil)
		(setq org-table-comment-permissive-text nil)
		)
          (if org-table-comment-permissive-text
              (when (string-match "^[ \t]*$" org-table-comment-permissive-text)
                (setq org-table-comment-permissive-text ""))
            (setq org-table-comment-permissive-text "")))))
      (symbol-value 'ret))))

(defun org-table-comment-region ()
  "Gets the region for the comment orgtbl"
  (when (org-table-comment-p)
    (save-excursion
      (let (start stop (pt (point)))
        (forward-line -1)
        (while (and (org-table-comment-p t t)
                    (not (= (point-min) (point-at-bol))))
          (forward-line -1))
        
        (unless (and (org-table-comment-p t) (= (point-min) (point-at-bol)))
          (while (not (org-table-comment-p t))
            (forward-line 1)))
        (setq start (point-at-bol))
        (goto-char pt)
        (forward-line 1)
        (while (and (org-table-comment-p t t)
                    (not (= (point-max) (point-at-eol))))
          (forward-line 1))
        (unless (and (org-table-comment-p t)
                     (= (point-max) (point-at-eol)))
          (while (not (org-table-comment-p t))
            (forward-line -1)))
        (setq stop (+ 1 (point-at-eol)))
        (list start stop)))))

(defun org-table-comment-narrow ()
  "Narrow region to org-table comment"
  (interactive)
  (when (org-table-comment-p)
    (let ((region (org-table-comment-region)))
      (narrow-to-region (nth 0 region) (nth 1 region)))))

(defun org-table-comment-overlay ()
  "Create overlay for org-table-comment"
  (interactive)
  (if (not (org-table-comment-p))
      nil
    (let ((region (org-table-comment-region))
          over)
      (setq over (make-overlay (nth 0 region) (nth 1 region) nil nil t))
      (overlay-put over 'category 'org-table-comment-category)
      (overlay-put over 'modification-hooks 'org-table-comment-mode)
      over)))

(defun org-table-comment-overlay-rm (beg end)
  "Remove org table comment overlays.
BEG is the beginning of the region to check
END is the end of the region to check"
  (interactive (list (point-min) (point-max)))
  (remove-overlays beg end 'org-comment-table t)
  (setq org-table-comment-editing nil)
  (setq org-table-comment-permissive-text nil))

(defun org-table-comment-overlay-get (pt)
  "Gets the comment table overlay at point (if exists)."
  (interactive (list (point)))
  (let ((overlays (overlays-at pt))
        found)
    (while overlays
      (let ((overlay (car overlays)))
        (if (overlay-get overlay 'org-comment-table)
            (progn
              (setq found (nth 0 (cons overlay found)))
              (setq overlays nil))))
      (setq overlays (cdr overlays)))
    found))

(defvar org-table-comment-before-edit-hook nil
  "Hook for `org-table-comment-before-edit-hook'")

(defvar org-table-comment-after-edit-hook nil
  "Hook for `org-table-comment-before-edit-hook'")


(defvar org-table-comment-category nil)

(unless org-table-comment-category
  (put 'org-table-comment-category 'face 'highlight)
  (put 'org-table-comment-category 'org-comment-table t))
(defun org-table-comment-edit (&optional pt)
  "Starts editing an org-table in a comment"
  (interactive)
  (run-hooks 'org-table-comment-before-edit-hook)
  (let (perm
        (debug-on-error t) (debug-on-quit t))
    (if (not org-table-comment-use-overlay)
        (if (not (org-table-comment-p))
            nil
          (org-table-comment-narrow)
          (uncomment-region (point-min) (point-max))
          (setq perm org-table-comment-permissive-text)
          (when (string= "" perm)
            (setq perm nil))
          (when perm
            (when (string-match "^[ \t]*" perm)
              (setq perm (replace-match "" nil nil perm)))
            (save-excursion
              (goto-char (point-min))
              (while (re-search-forward (format "^[ \t]*%s" (regexp-quote perm)) nil t)
                (replace-match ""))))
          (setq org-table-comment-editing (point)))
      (when (org-table-comment-p t)
        (let ((over (org-table-comment-overlay-get (or pt (point)))))
          (unless over
            (setq over (org-table-comment-overlay)))
          (when over
            (uncomment-region (overlay-start over) (overlay-end over))
            (setq perm org-table-comment-permissive-text)
            (when (string= "" perm)
              (setq perm nil))
            (when perm
              (when (string-match "^[ \t]*" perm)
                (setq perm (replace-match "" nil nil perm)))
              (save-excursion
                (goto-char (overlay-start over))
                (while (re-search-forward (format "^[ \t]*%s" (regexp-quote perm))
                                          (overlay-end over) t)
                  (replace-match ""))))
            (setq org-table-comment-editing (point))
            over))))))

(defun org-table-comment-return (&optional passed-overlay)
  "Stops editing an org-table in a comment"
  (interactive)
  (when org-table-comment-editing
    (if (not org-table-comment-use-overlay)
        (let ((perm org-table-comment-permissive-text))
          (condition-case error
              (comment-region (point-min) (point-max))
            (error nil))
          (when (string= "" perm)
            (setq perm nil))
          (when perm
            (save-excursion
              (goto-char (point-min))
              (while (re-search-forward (format "^[ \t]*%s+" comment-start) nil t)
                (when (looking-at "[ \t]*")
                  (replace-match perm)))))
          (widen)
          (run-hooks 'org-table-comment-after-edit-hook)
          (setq org-table-comment-permissive-text nil)
          (setq org-table-comment-editing nil))
      ;; Return from comment
      (let ((over (or passed-overlay (org-table-comment-overlay-get org-table-comment-editing)))
            (perm org-table-comment-permissive-text))
        (when over
          (condition-case error
              (comment-region (overlay-start over) (overlay-end over))
            (error nil))
          (when (string= "" perm)
            (setq perm nil))
          (when perm
            (save-excursion
              (goto-char (overlay-start over))
              (while (re-search-forward (format "^[ \t]*%s+" comment-start) (overlay-end over) t)
                (when (looking-at "[ \t]*")
                  (replace-match org-table-comment-permissive-text)))))
                
          (delete-overlay over)
          (setq org-table-comment-permissive-text nil)
          (setq org-table-comment-editing nil)
          (run-hooks 'org-table-comment-after-edit-hook))))))

(defgroup org-table-comment nil
  "Options for using org-table when in single comment areas."
  :group 'org-table)

(defcustom org-table-comment-use-overlay t
  "User overlays instead of narrowing to a region when editing region."
  :group 'org-table-comment
  :type 'boolean)

(defvar org-table-comment-permissive-text nil
  "Variable storing what the permissive text is when `org-table-comment-permissive' is true.")
(defcustom org-table-comment-permissive 4
  "The org-mode comment is permissive.  Therefore as long as it
starts with `comment-start' and some combination of characters at
a maximum length of `org-table-comment-permissive', followed by
an org-table, it will be considered an org-comment-table.  For
example:

;; % #+ORGTBL: SEND salesfigures orgtbl-to-latex
;; % |-------+------+---------+---------|
;; % | Month | Days | Nr sold | per day |
;; % |-------+------+---------+---------|
;; % | Jan   |   23 |      55 |     2.4 |
;; % | Feb   |   21 |      16 |     0.8 |
;; % | March |   22 |     278 |    12.6 |
;; % |-------+------+---------+---------|
;; % #+TBLFM: $4=$3/$2;.1f

can be edited in elisp mode.  In this example ' %' is stored as the `org-table-comment-permissive-text'

Zero or below is considered a non-permissive comment
"
  :type 'integer
  :group 'org-table-comment)

(defvar org-table-comment-editing nil
  "Variable to describe if we are editing a commented org-table")


(defvar org-table-comment-at-beginning nil)

(defvar org-table-comment-at-end nil)

(defun org-table-comment-pre-command ()
  "Pre-command called for `orgtbl-comment-mode'"
  (condition-case error
      (progn
        (unless (memq 'org-table-comment-post-command post-command-hook)
          (add-hook 'post-command-hook 'org-table-comment-post-command))
        (when (and orgtbl-comment-mode (not (minibufferp)))
          (setq org-table-comment-at-beginning (= (point-at-bol) (point-min)))
          (setq org-table-comment-at-end (= (point-at-eol) (point-max)))))
    (error
     (message "[org-table-comment] Error running pre-command-hook: %s" (error-message-string error)))))

(defun org-table-comment-post-command ()
  "Post-command called for `org-table-comment'"
  (condition-case error
      (progn
        (unless (memq 'org-table-comment-pre-command pre-command-hook)
          (add-hook 'pre-command-hook 'org-table-comment-pre-command))
        (when (and orgtbl-comment-mode orgtbl-mode (not (minibufferp)))
          (let ((over (if org-table-comment-use-overlay (org-table-comment-overlay-get (point)) nil)))
          (cond
           ((and org-table-comment-use-overlay (not org-table-comment-editing)
                 (org-table-comment-p t))
            (org-table-comment-edit))
           ((and over (string-match "\\`[ \t\n]*\\'" (buffer-substring-no-properties (overlay-start over) (overlay-end over))))
            (setq org-table-comment-editing nil)
            (setq org-table-comment-permissive-text nil)
            (delete-overlay over))
           ((and org-table-comment-use-overlay over org-table-comment-editing)
            ;; Update point.  Occasionally when updating radio table this point gets disrupted.
            (setq org-table-comment-editing (point)))
           ((and org-table-comment-use-overlay org-table-comment-editing (not over))
            (org-table-comment-return))
           ((and (not org-table-comment-use-overlay) org-table-comment-editing
                 org-table-comment-at-end
                 (memq this-command '(next-line)))
            (org-table-comment-return)
            (next-line))
           ((and (not org-table-comment-use-overlay) org-table-comment-editing
                 org-table-comment-at-beginning
                 (memq this-command '(previous-line)))
            (org-table-comment-return)
            (previous-line))
           ((and (not org-table-comment-use-overlay)
                 org-table-comment-editing
                 (memq this-command '(cua-scroll-down cua-scroll-up scroll-up scroll-down)))
            (org-table-comment-return))
           ((and (not org-table-comment-use-overlay)
                 (not org-table-comment-editing) (org-table-comment-p))
            (org-table-comment-edit))))))
    (error
     (message "[org-table-comment] Error running post-command-hook: %s" (error-message-string error)))))

(defalias 'org-table-comment-mode 'orgtbl-comment-mode)
;;;###autoload
(define-minor-mode orgtbl-comment-mode
  "Orgtbl comment mode.  Changes how orgtbl works for modes that don't support block comment regions (like emacs-lisp).

Currently supports radio tables through overlay interface.
"
  ;; The initial value.
  nil
  ;; The indicator for the mode line.  Nothing.
  ""
  :group 'org-table-comment
  (let (
        (orgtbl (assoc 'orgtbl-mode minor-mode-alist)))
    (when orgtbl
      (setq orgtbl (cdr orgtbl))
      (if orgtbl-comment-mode
          (setcar orgtbl (replace-regexp-in-string "[*]*$" "*" (car orgtbl)))
        (setcar orgtbl (replace-regexp-in-string "[*]+$" "" (car orgtbl))))))
  (cond (orgtbl-comment-mode
         ;; Setup
         (orgtbl-mode 1)
         (set (make-local-variable 'org-table-comment-editing) nil)
         (set (make-local-variable 'org-table-comment-permissive-text) nil)
         (set (make-local-variable 'org-table-comment-at-beginning) nil)
         (set (make-local-variable 'org-table-comment-at-end) nil)
         (set (make-local-variable 'org-table-comment-pre-point) nil)
         (add-hook 'write-contents-hooks 'org-table-comment-return nil t)
         (add-hook 'post-command-hook 'org-table-comment-pre-command nil t)
         (add-hook 'pre-command-hook 'org-table-comment-post-command nil t))
        (t
         ;; Kill
         (orgtbl-mode -1)
         (set (make-local-variable 'org-table-comment-editing) nil)
         (set (make-local-variable 'org-table-comment-at-beginning) nil)
         (set (make-local-variable 'org-table-comment-at-end) nil)
         (set (make-local-variable 'org-table-comment-pre-point) nil)
         (remove-hook 'post-command-hook 'org-table-comment-pre-command t)
         (remove-hook 'pre-command-hook 'org-table-comment-post-command t)
         (remove-hook 'write-contents-hook 'org-table-comment-return t))))
(provide 'org-table-comment)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org-table-comment.el ends here
