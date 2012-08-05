;;; org-readme.el --- Integrates Readme.org and Commentary/Change-logs.
;; 
;; Filename: org-readme.el
;; Description: Integrate Readme.org and Commentary/Change Logs.
;; Author: Matthew L. Fidler
;; Maintainer: 
;; Created: Fri Aug  3 22:33:41 2012 (-0500)
;; Version: 
;; Last-Updated: Sat Aug  4 21:45:57 2012 (-0500)
;;           By: Matthew L. Fidler
;;     Update #: 125
;; URL: https://github.com/mlf176f2/org-readme
;; Keywords: 
;; Compatibility: 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; * Org-readme
;; This file is to help generate <tt>Readme.org</tt> files based on emacs lisp
;; files.
;; * Changelog
;; Currently this library allows changelog to become a history section in
;; Readme.org
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
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

(defcustom org-readme-remove-sections '("History")
  "List of sections to remove when changing the Readme.org to Change-log."
  :group 'org-readme
  :type '(repeat (string :tag "Section")))

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


;;;###autoload
(defun org-readme-sync ()
  "Syncs Readme.org with current buffer."
  (interactive)
  (message "Adding Readme to Header Commentary")
  (org-readme-to-commentary)
  (save-buffer)
  (message "Updating Changelog in current file.")
  (org-readme-changelog-to-readme)
  (message "Posting lisp file to emacswiki")
  (emacswiki-post nil "")
  (message "Posting Description to emacswiki")
  (org-readme-convert-to-emacswiki))

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

(defun org-readme-find-readme ()
  "Find the Readme.org."
  (let* ((dir (file-name-directory (buffer-file-name)))
         (df (directory-files dir t "^[Rr][Ee][Aa][Dd][Mm][Ee][.][Oo][Rr][Gg]$")))
    (if (= 1 (length df))
        (setq df (nth 0 df))
      (setq df (expand-file-name "Readme.org" dir))
      (symbol-value 'df))))

(defun org-readme-remove-section (section &optional txt any-level)
  "Remove `org-mode' SECTION. Optionally insert TXT.
When ANY-LEVEL is non-nil, any level may be specified."
  (let ((case-fold-search t))
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward
           (format "^[*]%s %s"
                   (if any-level
                       "+" "")
                   section) nil t)
          (progn
            (delete-region
             (match-beginning 0)
             (or
              (if (re-search-forward  "^[*] " nil t)
                  (progn
                    (- (match-beginning 0) 1))
                nil)
              (point-max)))
            (when txt
              (insert txt))
            t)
        (goto-char (point-max))
        (when txt
          (insert txt))
        nil))))

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
              (while (re-search-forward "\\([0-9][0-9]-[A-Za-z][A-Za-z][A-Za-z]-[0-9][0-9][0-9][0-9]\\)[ \t]+\\(.*\\)\n.*\n\\(\\(?:\n\\|.\\)*?\\)\n[ \t]*\\([0-9][0-9]\\)" nil t)
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
