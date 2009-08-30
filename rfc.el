;;; rfc.el --- view RFC

;; Maintainer: Katsuya Iida (katsuya_iida@hotmail.com)
;; Keywords: rfc view

;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with This software; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; With this package, you can view the RFC articles.  To use this
;;; execute function `rfc-index' or
;;; `rfc-goto-number'. The variable `rfc-article-alist' specifies the
;;; directories or URLs which has rfc-index.txt and rfc????.txt's under them,
;;; or a zip file which contains them.
;;; rfc- is used as prefix for commands that is for this package.
;;; rfc-index- is used for commands that is for `rfc-index-mode'.
;;; rfc-article- is used for commands that is for `rfc-article-mode'.
;;;
;;; Example
;;;
;;; (setq rfc-url-save-directory "~/rfc")
;;; (setq rfc-index-url "http://www.ietf.org/iesg/1rfc_index.txt")
;;; (setq rfc-archive-alist (list (concat rfc-url-save-directory "/rfc.zip")
;;;                               rfc-url-save-directory
;;;                               "http://www.ietf.org/rfc/"))
;;; (setq rfc-insert-content-url-hook '(rfc-url-save))

;;; Code:

(defface rfc-node
  '((t (:bold t :foreground "blue")))
  "Face for RFC index node."
  :group 'rfc)

(defface rfc-xref
  '((t (:bold t)))
  "Face for RFC index node."
  :group 'rfc)

(defface rfc-header
  '((t (:bold t :italic t)))
  "Face for RFC header"
  :group 'rfc)

(defface rfc-subject
  '((t (:bold t)))
  "Face for RFC subject"
  :group 'rfc)

(defcustom rfc-index-mode-hook nil
  "Normal hook run when starting RFC index mode."
  :type 'hook
  :group 'rfc)

(defcustom rfc-insert-content-url-hook nil
  ""
  :type 'hook
  :group 'rfc)

(defcustom rfc-article-mode-hook nil
  "Normal hook run when starting RFC index mode."
  :type 'hook
  :group 'rfc)

(defcustom rfc-archive-alist nil
  "Alist of places from which RFC files are retrieved."
  :type '(repeat string)
  :group 'rfc)

(defcustom rfc-fontify t
  "*Non-nil enables highlighting and fonts in rfc mode."
  :type 'boolean
  :group 'rfc)

(defcustom rfc-url-save-directory nil
  "Directory which files retrieved from URL is saved"
  :type 'directory
  :group 'rfc)

(defcustom rfc-unzip-command "unzip"
  "UnZip command filename"
  :type 'file
  :group 'rfc)

(defcustom rfc-index-url nil
  "URL for index file"
  :type 'string
  :group 'rfc)

(defun rfc-four-char-number (number)
  "Return four columns of string representing NUMBER."
  (let ((cat-string (concat "000" (number-to-string number))))
    (substring cat-string (- (length cat-string) 4))))

(defun rfc-nearest-rfc-number ()
  "Returns the number of RFC which appears nearest in the buffer.
If there is no such a node, it returns nil."
  (save-excursion
    (backward-word 2)
    (and (re-search-forward "RFC[- ]?\\([0-9][0-9][0-9][0-9]\\)"
		       (min (point-max) (+ (point) 15)) t)
	 (string-to-int (buffer-substring (match-beginning 1) (match-end 1))))))

;; RFC index mode

(defvar rfc-index-mode-map nil
  "Keymap for RFC index mode")

(if rfc-index-mode-map
    nil
  (setq rfc-index-mode-map (make-sparse-keymap))
  (suppress-keymap rfc-index-mode-map)
  (let ((map rfc-index-mode-map))
    (define-key map "\C-m" 'rfc-index-goto-nearest)
    (define-key map "g" 'rfc-goto-number)
    (define-key map "\C-j" 'rfc-index-follow-nearest)
    (define-key map "f" 'rfc-index-follow-number)
    (define-key map "o" 'rfc-index-follow-obsoleted)
    (define-key map "O" 'rfc-index-follow-obsoletes)
    (define-key map "u" 'rfc-index-follow-updates)
    (define-key map "U" 'rfc-index-follow-updated)
    (define-key map [mouse-2] 'rfc-index-mouse-2)
    (define-key map "n" 'scroll-up)
    (define-key map "p" 'scroll-down)
    (define-key map " " 'scroll-up)
    (define-key map "\C-?" 'scroll-down)
    (define-key map "s" 'isearch-forward)
    (define-key map "r" 'isearch-backward)
    (define-key map "q" 'rfc-index-kill-buffer)
    ))

(defun rfc-index-mode ()
  (setq major-mode 'rfc-index-mode)
  (setq mode-name "RFC Index")
  (setq buffer-read-only t)
  (use-local-map rfc-index-mode-map)
  (run-hooks 'rfc-index-mode-hook)
  )

;;;###autoload
(defun rfc-index ()
  "Show the index of RFC."
  (interactive)
  (switch-to-buffer "*RFC index*")
  (if (< (buffer-size) 10) ;; Buffer is empty
      (progn
       (rfc-insert-contents "rfc-index.txt")
       (if rfc-fontify
	   (rfc-index-fontify-buffer))
       (set-buffer-modified-p nil)
       (goto-char (point-min))))
  (rfc-index-mode))

(defun rfc-index-follow-number (number)
  (interactive "nFollow RFC number: ")
  (switch-to-buffer "*RFC index*")
  (goto-char (point-min))
  (re-search-forward (concat "^" (rfc-four-char-number number)))
  (beginning-of-line))
  
(defun rfc-index-fontify-buffer ()
  "Fontify the current buffer in article mode."
  (save-excursion
    (let ((buffer-read-only nil))
      (goto-char (point-min))
      (while (re-search-forward "^\\([0-9][0-9][0-9][0-9]\\) " nil t)
	(put-text-property (match-beginning 1) (match-end 1) 'face 'rfc-node)
	(put-text-property (match-beginning 1) (match-end 1) 'mouse-face 'highlight))
      (goto-char (point-min))
      (while (re-search-forward "\\(RFC[0-9][0-9][0-9][0-9]\\)" nil t)
	(put-text-property (match-beginning 1) (match-end 1) 'face 'rfc-xref)
	(put-text-property (match-beginning 1) (match-end 1) 'mouse-face 'highlight))
	)))

(defconst rfc-start-tag-index
      "^[ \t]*\n\\([0-9][0-9][0-9][0-9]\\) ")

(defun rfc-index-current-number ()
  (save-excursion
    (end-of-line)
    (re-search-backward rfc-start-tag-index nil t)
    (string-to-int (buffer-substring (match-beginning 1) (match-end 1)))))

(defun rfc-index-start-of-current ()
  (save-excursion
    (end-of-line)
    (re-search-backward rfc-start-tag-index nil t)))

(defun rfc-index-end-of-current ()
  (save-excursion
    (end-of-line)
    (or (re-search-forward rfc-start-tag-index nil t) (point-max))))

(defun rfc-index-ralated-number-of-nearest-node (tag)
  (save-excursion
    (re-search-backward "^[ \t]*$" nil t)
    (forward-line)
    (and
     (re-search-forward
      (concat tag "[ \t]+by[ \t]+RFC\\([0-9][0-9][0-9][0-9]\\)")
      (rfc-index-end-of-current) t)
     (string-to-int (buffer-substring (match-beginning 1) (match-end 1))))))

;; The user commands of RFC index mode

(defun rfc-index-mouse-2 (click)
  (interactive "e")
  (if (featurep 'xemacs)
      nil ; (event-closest-point click)
    (goto-char (car (cdr (event-start click)))))
  (rfc-index-goto-nearest))

(defun rfc-index-goto-nearest ()
  "Go to the article referenced most closely."
  (interactive)
  (let ((number (rfc-nearest-rfc-number)))
   (if number
       (rfc-goto-number number)
     (rfc-goto-number (rfc-index-current-number)))))

(defun rfc-index-follow-updates () ; might not be used.
  "Go to the index of the node updated by the current node."
  (interactive)
  (let ((number (rfc-index-ralated-number-of-nearest-node "Updates")))
    (or number (error "This RFC updates nothing"))
    (rfc-index-follow-number number)))

(defun rfc-index-follow-updated () ; might not be used.
  "Go to the index of the node updating the current node."
  (interactive)
  (let ((number (rfc-index-ralated-number-of-nearest-node "Updates")))
    (or number (error "This RFC isn't updated"))
    (rfc-index-follow-number number)))

(defun rfc-index-follow-obsoletes ()
  "Go to the index of the node obsoleted by the current node."
  (interactive)
  (let ((number (rfc-index-ralated-number-of-nearest-node "Obsoletes")))
    (or number (error "This RFC obsoletes nothing"))
    (rfc-index-follow-number number)))

(defun rfc-index-follow-obsoleted ()
  "Go to the index of the node obsoleting the current node."
  (interactive)
  (let ((number (rfc-index-ralated-number-of-nearest-node "Obsoleted")))
    (or number (error "This RFC isn't obsoleted"))
    (rfc-index-follow-number number)))

(defun rfc-index-kill-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))

;; RFC Article mode

(defvar rfc-article-mode-map nil
  "Keymap for RFC Article mode")

(defvar rfc-article-number 0
  "Number of RFC article currently visited")

(if rfc-article-mode-map
    nil
  (setq rfc-article-mode-map (make-sparse-keymap))
  (make-local-variable 'rfc-article-number)
  (suppress-keymap rfc-article-mode-map)
  (let ((map rfc-article-mode-map))
    (define-key map "\C-m" 'rfc-article-goto-nearest)
    (define-key map "d" 'rfc-index)
    (define-key map "g" 'rfc-goto-number)
    (define-key map "P" 'rfc-article-goto-page)
    (define-key map [mouse-2] 'rfc-article-mouse-2)
    (define-key map "n" 'rfc-article-next-page)
    (define-key map "p" 'rfc-article-previous-page)
    (define-key map " " 'rfc-article-next-page)
    (define-key map "\C-?" 'rfc-article-previous-page)
    (define-key map "s" 'isearch-forward)
    (define-key map "r" 'isearch-backward)
    (define-key map "q" 'rfc-article-kill-buffer)
    ))

(defun rfc-article-mode ()
  (setq major-mode 'rfc-index-mode)
  (setq mode-name "RFC Article")
  (setq buffer-read-only t)
  (use-local-map rfc-article-mode-map)
  (run-hooks 'rfc-article-mode-hook))

;;;###autoload
(defun rfc-goto-number (number)
  "Show an RFC article which number is NUMBER."
  (interactive "nGo to RFC number: ")
  (switch-to-buffer (concat "*RFC" (number-to-string number) "*"))
  (rfc-insert-contents (concat "rfc" (number-to-string number) ".txt"))
  (rename-buffer (concat "*RFC" (number-to-string number) "*"))
  (set-buffer-modified-p nil)
  (rfc-article-mode)
  (if rfc-fontify
      (rfc-article-fontify-buffer))
  (set-buffer-modified-p nil)
  (setq rfc-article-number number)
  (rfc-article-beginning-of-article))

(defun rfc-article-fontify-buffer ()
  "Fontify the current buffer in article mode."
  (save-excursion
    (let ((buffer-read-only nil))
      (goto-char (point-min))
      (while (re-search-forward ".\\(RFC[- ]?[0-9][0-9][0-9][0-9]\\)" nil t)
	(put-text-property (match-beginning 1) (match-end 1) 'face 'rfc-node)
	(put-text-property (match-beginning 1) (match-end 1) 'mouse-face 'highlight))
;      (goto-char (point-min))
;      (while (re-search-forward "^\\([^ \t].*$\\)" nil t)
;	(put-text-property (match-beginning 1) (match-end 1) 'face 'rfc-subject))
      (goto-char (point-min))
      (while (re-search-forward "^\\(RFC[- ][0-9][0-9][0-9][0-9].*$\\)" nil t)
	(put-text-property (match-beginning 1) (match-end 1) 'face 'rfc-header))
      (goto-char (point-min))
      (while (re-search-forward "^\\([^ \t].*\\[Page [0-9]+\\]$\\)" nil t)
	(put-text-property (match-beginning 1) (match-end 1) 'face 'rfc-header)
	))))

(defun rfc-article-mouse-2 (click)
  (interactive "e")
  (if (featurep 'xemacs)
      nil
    (goto-char (car (cdr (event-start click)))))
  (rfc-article-goto-nearest))

    
(defun rfc-article-goto-nearest ()
  "Go to the article referenced most closely."
  (interactive)
  (rfc-index-goto-nearest))

(defun rfc-article-beginning-of-article ()
  (interactive)
  (goto-char (point-min))
  (forward-line 3)
  (recenter 0))

(defun rfc-article-goto-page (number)
  "Go to the page of the current RFC document."
  (interactive "nGo to page number: ")
  (cond
   ((= number 1)
    (rfc-article-beginning-of-article))
   ((> number 1)
    (goto-char (point-min))
    (search-forward (concat "[Page " (number-to-string (1- number)) "]"))
    (forward-line 2)
    (beginning-of-line)
    (recenter 0)
    )
   (t
    (error "Page number should be positive"))))

(defun rfc-article-next-page ()
  (interactive)
  (if (search-forward "\014" nil t)
      (progn
       (if (> (forward-line 1) 0)
	   (beep))
       (beginning-of-line)
       (recenter 0))
    (beep)))

(defun rfc-article-previous-page ()
  (interactive)
  (if (search-backward "\014" nil t)
      nil
    (beep)
    (rfc-article-beginning-of-article))
  (if (search-backward "\014" nil t)
      (progn
       (forward-line 1)
       (beginning-of-line)
       (recenter 0))
    (rfc-article-beginning-of-article)))

(defun rfc-article-kill-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))

;; Functions to fetch RFC files

(defun rfc-insert-contents (filename &optional place)
  "Retrieve text file named FILENAME in RFC archive
and insert into the current buffer."
  (if (not rfc-archive-alist)
      (error "rfc-archive-alist undefined"))
  (erase-buffer)
  (let ((buffer-read-only nil)
	(archive-alist rfc-archive-alist)
	(to-continue t))
    (while (and archive-alist to-continue)
      (let ((archive (car archive-alist)))
	(if (cond
	     ((and (string-match "\\.zip$" archive)
		   (not (eq place 'network)))
	      (rfc-insert-contents-zip archive filename))
	     ((and (string-match "^\\(ftp\\|http\\)://" archive)
		   (not (eq place 'local)))
	      (rfc-insert-contents-url archive filename))
	     ((not (eq place 'network))
	      (rfc-insert-contents-file archive filename)))
	    (setq to-continue nil)
	  (setq archive-alist (cdr archive-alist)))))
    (if to-continue
	(progn
	  (set-buffer-modified-p nil)
	  (kill-buffer (current-buffer))
	  (error "not found"))
      t)))

(defun rfc-insert-contents-zip (archive filename)
    (shell-command (concat rfc-unzip-command
			   " -p \"" archive "\" " filename)
		   (current-buffer))
    (goto-char (point-min))
    (> (buffer-size) 100))

(defun rfc-insert-contents-file (archive filename)
  (condition-case nil
      (insert-file-contents (concat archive "/" filename))
    (error nil)))

(defun rfc-insert-contents-url (archive filename)
  (require 'w3)
  (let ((url
	 (if (and (equal filename "rfc-index.txt")
		  rfc-index-url)
	     rfc-index-url
	   (concat archive filename))))
    (url-insert-file-contents url)
    (if (string-match "<\\(title\\|TITLE\\)>"
		      (buffer-substring (point-min)
					(min 500 (point-max))))
	(progn
	  (erase-buffer) nil)
      (progn
	(run-hook-with-args 'rfc-insert-content-url-hook filename)
	t))))

(defun rfc-url-save (filename)
  (write-file (concat rfc-url-save-directory "/" filename))
  (set-visited-file-name nil))

(defun rfc-url-index-update ()
  (interactive)
  (condition-case nil
      (delete-file (concat rfc-url-save-directory "/rfc-index.txt"))
    (error nil))
  (kill-buffer "*RFC index*")
  (rfc-index))

(provide 'rfc)
;;; rfc.el ends here
