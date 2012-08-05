;;; org-readme.el --- Integrates Readme.org and Commentary/Change-logs.
;; 
;; Filename: org-readme.el
;; Description: Integrate Readme.org and Commentary/Change Logs.
;; Author: Matthew L. Fidler
;; Maintainer: Matthew L. Fidler
;; Created: Fri Aug  3 22:33:41 2012 (-0500)
;; Version: 0.01
;; Last-Updated: Sun Aug  5 12:30:49 2012 (-0500)
;;           By: Matthew L. Fidler
;;     Update #: 252
;; URL: https://github.com/mlf176f2/org-readme
;; Keywords: Header2, Readme.org, Emacswiki
;; Compatibility: Tested with Emacs 24.1 on Windows.
;;
;; Features that might be required by this library:
;;
;;   None
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;; 05-Aug-2012    Matthew L. Fidler  
;;    Last-Updated: Sun Aug  5 12:30:26 2012 (-0500) #250 (Matthew L. Fidler)
;;    Added git pushing to org-readme
;; 05-Aug-2012    Matthew L. Fidler  
;;    Last-Updated: Sun Aug  5 12:21:53 2012 (-0500) #237 (Matthew L. Fidler)
;;    Added git support as well as a comment mode.  The only thing that
;;    should need to be called is `org-readme-sync'
;; 04-Aug-2012    Matthew L. Fidler  
;;    Last-Updated: Sat Aug  4 21:40:14 2012 (-0500) #122 (Matthew L. Fidler)
;;    Added syncing with emacswiki. 
;; 04-Aug-2012    Matthew L. Fidler  
;;    Last-Updated: Sat Aug  4 00:02:49 2012 (-0500) #20 (Matthew L. Fidler)
;;    Initial Release
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

(require 'yaoddmuse)

(defgroup org-readme nil
  "Org-readme is a way to create Readme.org files based on an elisp file.")

(defcustom org-readme-remove-sections '("History" "Possible Dependencies" "Library Information")
  "List of sections to remove when changing the Readme.org to Change-log."
  :group 'org-readme
  :type '(repeat (string :tag "Section")))

(defvar org-readme-edit-mode-map nil
  "Keymap for editing change-logs.")

(unless org-readme-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'org-readme-edit-commit)
    (define-key map (kbd "C-x C-s") 'org-readme-edit-commit)
    (define-key map (kbd "C-c C-k") 'org-readme-edit-cancel)
    (setq org-readme-edit-mode-map map)))

(defun org-readme-edit-commit ()
  "Changelog for editing."
  (interactive)
  (let ((comment (buffer-substring (point-min) (point-max)))
        mr)
    (kill-buffer (get-buffer "*Change Comment*"))
    (when org-readme-edit-last-window-configuration
      (set-window-configuration org-readme-edit-last-window-configuration))
    (with-temp-buffer
      (insert comment)
      (goto-char (point-min))
      (end-of-line)
      (while (re-search-forward "^" nil t)
        (insert ";;    "))
      (setq mr (buffer-substring (point-min) (point-max))))
    (make-revision)
    (insert mr)
    (save-buffer)
    (with-temp-file (org-readme-get-change)
      (insert comment))
    (org-readme-sync t)))

(defun org-readme-edit-cancel ()
  "Cancel the edit log."
  (interactive)
  (kill-buffer (get-buffer "*Change Comment*"))
  (when org-readme-edit-last-window-configuration
    (set-window-configuration org-readme-edit-last-window-configuration)))

(defvar org-readme-edit-last-window-configuration nil)

(defun org-readme-edit ()
  "Edit change comment for commit."
  (interactive)
  (setq org-readme-edit-last-window-configuration (current-window-configuration))
  (switch-to-buffer-other-window (get-buffer-create "*Change Comment*"))
  (org-readme-edit-mode))

(define-derived-mode org-readme-edit-mode text-mode "Org-readme Log edit.")

(defun org-readme-convert-to-emacswiki ()
  "Converts Readme.org to oddmuse markup and uploads to emacswiki."
  (interactive)
  (let ((readme (org-readme-find-readme))
        (what (file-name-nondirectory (buffer-file-name)))
        (wiki (org-readme-get-emacswiki-name))
        tmp tmp2)                       
    (with-temp-buffer
      (insert-file-contents readme)
      
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*[A-Z]+:[ \t]*\\[[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}.*" nil t)
        (replace-match ""))
      (goto-char (point-min))
      (while (re-search-forward "\\[\\[file:\\(.*?\\)\\]\\[\\1\\]\\]" nil t)
        (replace-match "[[\\1]]"))
      
      (goto-char (point-min))
      (while (re-search-forward "=\\<\\(.*?\\)\\>=" nil t)
        (replace-match (format "<tt>%s</tt>" (match-string 1))))
      
      (goto-char (point-min))
      (while (re-search-forward "^\\([*]+\\) *\\(.*\\)" nil t)
        (setq tmp (make-string (min 4 (+ 1 (length (match-string 1)))) ?=))
        (replace-match (format "%s %s %s" tmp (match-string 2) tmp) t t))
      
      (goto-char (point-min))
      (while (re-search-forward "^: " nil t)
        (replace-match "<pre>\n::::")
        (while (progn
                 (end-of-line)
                 (re-search-forward "\\=\n: " nil t))
          (replace-match "\n:::: "))
        (end-of-line)
        (insert "\n</pre>"))
      
      (goto-char (point-min))
      (while (re-search-forward "^ *#[+]BEGIN_SRC emacs-lisp *.*" nil t)
        (replace-match "{{{")
        (setq tmp (point))
        (when (re-search-forward "^ *#[+]END_SRC" nil t)
          (replace-match "}}}")
          (beginning-of-line)
          (setq tmp2 (point))
          (goto-char tmp)
          (while (and (> tmp2 (point))
                      (re-search-forward "^" tmp2 t))
            (replace-match "::::"))))
      
      (goto-char (point-min))
      (while (re-search-forward "^ *#[+]BEGIN_SRC.*" nil t)
        (replace-match "<pre>")
        (setq tmp (point))
        (when (re-search-forward "^ *#[+]END_SRC" nil t)
          (replace-match "</pre>")
          (beginning-of-line)
          (setq tmp2 (point))
          (goto-char tmp)
          (while (and (> tmp2 (point))
                      (re-search-forward "^" tmp2 t))
            (replace-match "::::"))))
      
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*[+-] +\\(.*?\\)::" nil t)
        (replace-match (format "* <b>%s</b> -- " (match-string 1))))
      
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*[+-] +" nil t)
        (replace-match "* "))
      
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*#.*" nil t)
        (replace-match ""))
      
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*[0-9]+[.)] +" nil t)
        (replace-match "# "))
      
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]+" nil t)
        (replace-match ""))
      
      (goto-char (point-min))
      (while (re-search-forward "^::::" nil t)
        (replace-match ""))
      (goto-char (point-min))
      (insert "This page describes [[")
      (insert what)
      (insert "]].\n\n")
      (setq readme (buffer-substring (point-min) (point-max))))
    (with-temp-file wiki
      (insert readme))
    (save-excursion
      (set-buffer (find-file-noselect wiki))
      (emacswiki-post nil "")
      (kill-buffer (current-buffer)))
    (delete-file wiki)))

(defun org-readme-git ()
  "Add The files to git."
  (interactive)
  (let* ((df (file-name-directory (buffer-file-name)))
         (default-directory df))
    (message "Git Adding Readme")
    (shell-command
     (format "git add %s"
             (file-name-nondirectory (org-readme-find-readme))))
    (message "Git Adding %s" (file-name-nondirectory (buffer-file-name)))
    (shell-command
     (format "git add %s"
             (file-name-nondirectory (buffer-file-name))))
    (when (file-exists-p (org-readme-get-change))
      (message "Git Committing")
      (shell-command
       (format "git commit -F %s"
               (file-name-nondirectory
                (org-readme-get-change))))
      (delete-file (org-readme-get-change))
      (message "Git push")
      (shell-command "git push"))))


;;;###autoload
(defun org-readme-sync (&optional comment-added)
  "Syncs Readme.org with current buffer.
When COMMENT-ADDED is non-nil, the comment has been added and the syncing should begin.
"
  (interactive)
  (if (not comment-added)
      (org-readme-edit)
    (message "Adding Readme to Header Commentary")
    (org-readme-to-commentary)
    (save-buffer)
    (message "Updating Changelog in current file.")
    (org-readme-changelog-to-readme)
    (org-readme-top-header-to-readme)
    (message "Posting lisp file to emacswiki")
    (emacswiki-post nil "")
    (message "Posting Description to emacswiki")
    (org-readme-convert-to-emacswiki)
    (org-readme-git)))

;;;###autoload
(defun org-readme-to-commentary ()
  "Change Readme.org to a Commentary section."
  (interactive)
  (let ((readme (org-readme-find-readme)) p1)
    (with-temp-buffer
      (insert-file-contents readme)
      (mapc
       (lambda(section)
         (org-readme-remove-section section))
       org-readme-remove-sections)
      
      (goto-char (point-min))
      (while (re-search-forward "=\\<\\(.*?\\)\\>=" nil t)
        (replace-match (format "<tt>%s</tt>" (match-string 1))))
      
      (goto-char (point-min))
      (while (re-search-forward "#.*" nil t)
        (replace-match ""))
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*[A-Z]+:[ \t]*\\[[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}.*" nil t)
        (replace-match ""))
      (goto-char (point-min))
      (while (re-search-forward "^:" nil t)
        (replace-match ""))
      (goto-char (point-min))
      (while (org-readme-remove-section
              (regexp-opt
               (mapcar
                (lambda(x)
                  (nth 0 x))
                org-todo-keyword-faces)) nil t))
      (goto-char (point-min))
      (skip-chars-forward " \t\n")
      (delete-region (point-min) (point))
      (insert "\n")
      (goto-char (point-max))
      (skip-chars-backward " \t\n")
      (delete-region (point) (point-max))
      (insert "\n")
      (goto-char (point-min))
      (while (re-search-forward "^" nil t)
        (insert ";; "))
      (setq readme (buffer-substring (point-min) (point-max))))
    (goto-char (point-min))
    (when (re-search-forward "^;;;[ \t]*Commentary:[ \t]*$" nil t)
      (skip-chars-forward "\n")
      (setq pt (point))
      (when (re-search-forward "^;;;;+[ \t]*$" nil t)
        (goto-char (match-beginning 0))
        (skip-chars-backward "\n")
        (delete-region pt (point))
        (insert readme)))))

(defun org-readme-get-emacswiki-name ()
  "Gets emacswiki-style name based on buffer."
  (let ((dir (file-name-directory (buffer-file-name)))
        (name (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))))
    (with-temp-buffer
      (insert (downcase name))
      (goto-char (point-min))
      (when (looking-at ".")
        (replace-match (upcase (match-string 0))))
      (while (re-search-forward "-\\(.\\)" nil t)
        (replace-match  (upcase (match-string 1))) t t)
      (setq name (concat dir (buffer-substring (point-min) (point-max)))))
    (symbol-value 'name)))

(defun org-readme-get-change ()
  "Get file for changelog commits."
  (expand-file-name "Changelog" (file-name-directory (buffer-file-name))))

(defun org-readme-find-readme ()
  "Find the Readme.org."
  (let* ((dir (file-name-directory (buffer-file-name)))
         (df (directory-files dir t "^[Rr][Ee][Aa][Dd][Mm][Ee][.][Oo][Rr][Gg]$")))
    (if (= 1 (length df))
        (setq df (nth 0 df))
      (setq df (expand-file-name "Readme.org" dir))
      (symbol-value 'df))))

(defun org-readme-remove-section (section &optional txt any-level at-beginning)
  "Remove `org-mode' SECTION. Optionally insert TXT.
When ANY-LEVEL is non-nil, any level may be specified.
When AT-BEGINNING is non-nil, if the section is not found, insert it at the beginning."
  (let ((case-fold-search t)
        (mtch ""))
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward
           (format "^\\([*]%s\\) +%s"
                   (if any-level
                       "+" "")
                   section) nil t)
          (progn
            (setq mtch (match-string 1))
            (delete-region
             (match-beginning 0)
             (if (re-search-forward (format "^%s +" (regexp-quote mtch)) nil t)
                 (progn
                   (- (match-beginning 0) 1))
               (point-max)))
            (when txt
              (beginning-of-line)
              (skip-chars-backward " \t\n")
              (skip-chars-forward "\n")
              (insert txt))
            t)
        (when txt
          (goto-char (if at-beginning
                         (point-min)
                       (point-max)))
          ;; Skip comments 
          (if at-beginning
              (while (re-search-forward "\\=[ \t]*#.*\n" nil t))
            (while (re-search-backward "\n[ \t]*#.*\\=" nil t)))
          (beginning-of-line)
          (insert txt))
        nil))))

;;;###autoload
(defun org-readme-top-header-to-readme ()
  "This puts the top header into the Readme.org file as Library Information"
  (interactive)
  (let ((top-header "")
        (readme (org-readme-find-readme)))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^;;;;+[ \t]*$" nil t)
        (beginning-of-line)
        (setq top-header (buffer-substring (point-min) (point)))))
    (with-temp-buffer
      (insert top-header)
      (goto-char (point-min))
      (when (looking-at ";;; *\\(.*?\\) *--+ *\\(.*\\)")
        (replace-match " *\\1* --- \\2"))
      
      (goto-char (point-min))
      (while (re-search-forward "^ *;; ?" nil t)
        (replace-match ""))
      
      (goto-char (point-min))
      (when (re-search-forward "[Ff]ile[Nn]ame: *\\(.*\\) *$" nil t)
        (replace-match "Filename: [[file:\\1][\\1]]"))
      
      (goto-char (point-min))
      (while (re-search-forward "^\\(.*?\\):\\(.*?[A-Za-z0-9.].*\\)$" nil t)
        (replace-match " - \\1 ::\\2"))
      (goto-char (point-min))
      (insert "* Library Information\n")
      
      (goto-char (point-min))
      (when (re-search-forward "^[ \t]*Features that might be required by this library:[ \t]*$" nil t)
        (replace-match "* Possible Dependencies"))
      (setq top-header (buffer-substring (point-min) (point-max))))
    (with-temp-buffer
      (insert-file-contents readme)
      (org-readme-remove-section "Possible Dependencies")
      (org-readme-remove-section "Library Information" top-header nil t)
      (write-file readme))))

;;;###autoload
(defun org-readme-changelog-to-readme ()
  "This puts the emacs lisp change-log into the Readme.org file."
  (interactive)
  (when (buffer-file-name)
    (let ((readme (org-readme-find-readme))
          pt1 pt2 txt)
      (save-excursion
        (goto-char (point-min))         
        (when (re-search-forward "^[ \t]*;;; Change Log:[ \t]*$" nil t)
          (setq pt1 (point))
          (when (re-search-forward "^[ \t]*;;;;+[ \t]*$" nil t)
            (setq pt2 (match-beginning 0))
            (setq txt (buffer-substring pt1 pt2))
            (with-temp-buffer
              (insert txt)
              (goto-char (point-min))
              ;; Take out comments
              (while (re-search-forward "^[ \t]*;+ ?" nil t)
                (replace-match ""))
              (goto-char (point-min))
              (while (re-search-forward "^[ \t]*\\([0-9][0-9]-[A-Za-z][A-Za-z][A-Za-z]-[0-9][0-9][0-9][0-9]\\)[ \t]+\\(.*\\)\n.*\n\\(\\(?:\n\\|.\\)*?\\)\n[ \t]*\\([0-9][0-9]\\)" nil t)
                (replace-match
                 (format " - %s :: %s (%s)\n %s"
                         (match-string 1)
                         (save-match-data
                           (replace-regexp-in-string
                            "~~~~" "\n    + "
                            (replace-regexp-in-string
                             "  +" " "
                             (replace-regexp-in-string
                              "\n" " "
                              (replace-regexp-in-string
                               "\n[ \t]*[*-+] +" "~~~~" (match-string 3))))))
                         (save-match-data
                           (replace-regexp-in-string
                            "[ \t]*$" ""
                            (match-string 2))) (match-string 4)) t t)
                (beginning-of-line))
              (when (re-search-forward "\\([0-9][0-9]-[A-Za-z][A-Za-z][A-Za-z]-[0-9][0-9][0-9][0-9]\\)[ \t]+\\(.*\\)\n.*\n\\(\\(?:\n\\|.\\)*\\)" nil t)
                (replace-match
                 (format " - %s :: %s (%s)" 
                         (match-string 1)
                         (save-match-data
                           (replace-regexp-in-string
                            "~~~~" "\n    + "
                            (replace-regexp-in-string
                             "  +" " "
                             (replace-regexp-in-string
                              "\n" " "
                              (replace-regexp-in-string
                               "\n[ \t]*[*-+] +" "~~~~" (match-string 3))))))
                         (save-match-data
                           (replace-regexp-in-string
                            "[ \t]*$" ""
                              (match-string 2)))) t t))
              (goto-char (point-min))
              (while (re-search-forward "`\\(.*?\\)'" nil t)
                (replace-match "=\1="))
              (goto-char (point-min))
              (insert "* History\n")
              (setq txt (buffer-substring (point-min) (point-max))))
            (with-temp-buffer
              (insert-file-contents readme)
              (org-readme-remove-section "History" txt)
              (write-file readme))))))))

(provide 'org-readme)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org-readme.el ends here
