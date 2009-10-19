;;; btjunkie-search.el --- brutal query engine against btjunkie.org
;;
;; Copyright (C) 2009 Marco <Bj> Bardelli
;;
;; Author: Marco <Bj> Bardelli <bardelli.marco@gmail.com>
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; For who, like me, don't want to open a browser to find torrents.
;;
;; Search atoms rss.xml in btjunkie.org and present a
;; comfortable buffer to download torrent files.
;; Brutally use `wget' command.

;;; Code:
; TODO: a lot of clean, using `start-process' and sentinel, really async.

(defgroup btjunkie-search nil
  "btjunkie-search group."
  :group 'my
  :prefix "btjunkie-search-")

(defcustom btjunkie-wget-command "wget"
  "`wget' command to use."
  :group 'btjunkie-search
  :type 'string)

(defcustom btjunkie-search-top-buffer-name "*BTJunkie Searches*"
  "Buffer name fo cached query results."
  :group 'btjunkie-search
  :type 'string)

(defcustom btjunkie-wget-command-std-args " -O - 2>/dev/null"
  "Put dowloaded data to stdout and discard errors.
Useful to insert wget output in buffer."
  :group 'btjunkie-search
  :type 'string)

(defcustom btjunkie-query-url-wo-search "http://btjunkie.org/rss.xml?query="
  "Prefix url to query btjunkie.org"
  :group 'btjunkie-search
  :type 'string)

(defcustom btjunkie-query-url-options "&o=52"
  "This is the last part of url to download.
In btjunkie 'o=52' sort descending for seeders ;)."
  :group 'btjunkie-search
  :type 'string)

;; deployment variables.
(defcustom btjunkie-query-save-dir "/tmp/btjunkie-search"
  "Where to save chache xml query results."
  :group 'btjunkie-search
  :type 'string)

;; if u use a sort of bittorrent daemon that scan a dir to start download,
;; set this to that dir.
(defcustom btjunkie-torrents-dowload-dir btjunkie-query-save-dir
  "Where BTJunkie Search save dowloaded torrents."
  :group 'btjunkie-search
  :type 'string)

(defsubst btjunkie-search-convert-link-to-get-torrent (url)
  "Replace 'http://' with 'http://dl.' and append '/download.torrent'.
This work with btjunkie.org."
  (concat "http://dl." (substring url 7) "/download.torrent"))

(defun btjunkie-search-get-xml-for (query &optional sync)
  (save-excursion
    (unless (file-directory-p btjunkie-query-save-dir)
      (make-directory btjunkie-query-save-dir))
    (set-buffer
     (find-file-noselect
      (expand-file-name
       (concat query ".xml") btjunkie-query-save-dir)))
    (erase-buffer)
    ;; FIX TODO use start-process
    (insert
     (shell-command-to-string
      (concat
       btjunkie-wget-command " \"" ; quote url
       btjunkie-query-url-wo-search query
       btjunkie-query-url-options "\"" ; end quoted url
       btjunkie-wget-command-std-args (if sync "" " &"))))
    ;; set beauty buffer
    (xml-mode) (mark-whole-buffer)
    (indent-region (region-beginning)(region-end) nil)
    (deactivate-mark)(goto-char (point-min))
    (save-buffer)(kill-buffer)))

(defun btjunkie-make-buffer-for-xml (button)
  (let ((inhibit-read-only t)(xml-file (button-get button 'xml))
	(btlabel (button-label button)) items)
    (set-buffer
     (get-buffer-create (concat "*BTJunkie - " btlabel " *")))
    (erase-buffer)
    (with-temp-buffer
      (insert-file-contents-literally xml-file)
      ;; per item
      (while (search-forward "<item>" (point-max) t)
	(let* ((beg (point))
	       (end (save-excursion (search-forward "</item>")))
	       (item-string (buffer-substring-no-properties beg end))
	       title link desc date)
	  (when (string-match "<title>\\(.*\\)</title>" item-string)
	    (setq title (match-string 1 item-string)))
	  (when (string-match "<link>\\(.*\\)</link>" item-string)
	    (setq link (match-string 1 item-string)))
	  (when (string-match "<description>\\(.*\\)</description>" item-string)
	    (setq desc (match-string 1 item-string)))
	  (when (string-match "<pubDate>\\(.*\\)</pubDate>" item-string)
	    (setq date (match-string 1 item-string)))
	  (when (and title link desc date)
	    (setq items (cons (list title link desc date) items)))
	  (goto-char end))))
    (insert
     (concat "\tBTJunke Search Results for " btlabel " :\n\n"))
    (dolist (IT (nreverse items))
      (insert-text-button
       (nth 0 IT) 'action 'btjunkie-get-torrent
       'url (btjunkie-search-convert-link-to-get-torrent (nth 1 IT)))
      (insert (concat "\n" (nth 2 IT) "\n"))
      (insert (concat (nth 3 IT) "\n\n")))
    (btjunkie-search-mode)
    (goto-char (point-min))
    (display-buffer (current-buffer))))

(defun btjunkie-get-torrent (but)
  (let ((torrent-file-name
	 (concat (button-label but) ".torrent"))
	(obuf (concat "*BTJunkie Log " (button-label but) " *"))
	cmd (pos 0))
    (while (string-match "/" torrent-file-name pos)
      (setq torrent-file-name (replace-match "_" t t torrent-file-name))
      (setq pos (match-beginning 0)))
    (setq torrent-file-name
	  (expand-file-name torrent-file-name btjunkie-torrents-dowload-dir))
    (setq cmd
	  (concat
	   btjunkie-wget-command " -q \"" (button-get but 'url) "\""
	   " -O '" torrent-file-name "' &"))
    ;; FIX TODO use start-process
    (shell-command cmd obuf)))

(defun btjunkie-searches ()(interactive)
  (let ((inhibit-read-only t))
    (set-buffer (get-buffer-create btjunkie-search-top-buffer-name))
    (erase-buffer)
    (insert "BTJunkie.org Searches\n\n")
    (insert-text-button
     "Clean xml(s) search results cache"
     'action '(lambda (unused)
		(progn
		  (shell-command (concat "rm -f " btjunkie-query-save-dir "/*.xml"))
		  (btjunkie-searches))))
    (insert "\n\n")
    (dolist (XML (directory-files btjunkie-query-save-dir t "\\.xml$"))
      ;(insert "\t")
      (insert-text-button
       (file-name-sans-extension (file-name-nondirectory XML))
       'action 'btjunkie-make-buffer-for-xml 'xml XML)
      (insert "\n"))
    (goto-char (point-min))
    (btjunkie-search-mode)
    (display-buffer (current-buffer))))

(defun btjunkie-search-mode ()
  "Set up utils keymap for BTJunkie Search."
  (let ((km (make-sparse-keymap)))
    (define-key km "q" 'bury-buffer)
    (define-key km [tab]
      '(lambda ()(interactive)
	 (condition-case nil
	     (forward-button 1 t t)
	   (error nil))))
    (define-key km [backtab]
      '(lambda ()(interactive)
	 (condition-case nil
	     (forward-button -1 t t)
	   (error nil))))
    
    (kill-all-local-variables)
    (use-local-map km))
  (setq buffer-read-only t))

(defun btjunkie-search (&optional string)
  (interactive "sQuery for: ")
  (btjunkie-search-get-xml-for string)
  (btjunkie-searches))

(provide 'btjunkie-search)

;;; btjunkie-search.el ends here
