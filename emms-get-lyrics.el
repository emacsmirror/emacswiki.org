;;; emms-get-lyrics.el --- Get the lyrics of the song emms is currently playing

;;; Copyright (C) 2013, 2014 andres.ramirez
;;; Copyright (C) 2007 Jay Belanger

;; emms-get-lyrics.el is free software; you can redistribute it and/or 
;; modify it under the terms of the GNU General Public License as 
;; published by the Free Software Foundation; either version 2, or 
;; (at your option) any later version.

;; emms-get-lyrics.el is distributed in the hope that it will be 
;; useful, but WITHOUT ANY WARRANTY; without even the implied warranty 
;; of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;;; Commentary:

;; The function 'emms-get-lyrics-current-song' tries to 
;; get the lyrics to the song that emms is currently playing.
;; It currently requires w3m to get the lyrics.
;; It copies the lyrics to a file ending in .lyrics; if the variable
;; `emms-get-lyrics-use-file' is nil, it will just display the lyrics 
;; in a buffer without saving them to a file.
;; If the variable `emms-get-lyrics-dir' is non-nil, then the lyrics will
;; be put in this directory with the file ARTIST-TITLE.lyrics;
;; otherwise it will be put in the same directory as the song file, in
;; a file with the same name as the song file except the extension will
;; by ".lyrics".

;;; Code:

(defvar emms-get-lyrics-use-files t)
(defvar emms-get-lyrics-dir nil)

(defvar emms-get-lyrics-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'bury-buffer)
    (define-key map "\\" 'isearch-backward)
    (define-key map "/" 'isearch-forward)
    (define-key map "r" 'isearch-backward)
    (define-key map "s" 'isearch-forward)
    (define-key map [delete]  'scroll-down)
    (define-key map " " 'scroll-up)
    (define-key map ">" 'end-of-buffer)
    (define-key map "<" 'beginning-of-buffer)
    map))

(defun emms-get-lyrics-mode ()
  "Major mode for displaying lyrics."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'emms-get-lyrics-mode)
  (setq mode-name "Lyrics")
  (use-local-map emms-get-lyrics-mode-map)
  (setq buffer-read-only t)
  (run-hooks 'emms-get-lyrics-mode-hook))

(defun emms-get-lyrics-url (artist title)
  (concat
   "https://duckduckgo.com/?q=!ducky+"  (base64-decode-string "c2l0ZTplbHlyaWNzLm5ldCs=")
   (replace-regexp-in-string 
    " " "+"
    (concat
     artist
     " "
     title ""))
   ""))

(defun emms-get-lyrics-w3m (url buffer)
  (call-process "w3m" nil buffer nil "-dump" url))

(defun emms-get-lyrics (artist title fn &optional file)
  "chg 4 using duckduckgo"
  (let ((bname (concat "Lyrics: " title " by " artist)))
    (cond ((get-buffer bname)
           (switch-to-buffer bname))
          ((and file (file-exists-p file))
           (find-file file)
           (emms-get-lyrics-mode)
           (rename-buffer bname))
          (t
           (let ((buffer (if file
                             (find-file-noselect file)
                           (get-buffer-create bname))))
             (set-buffer buffer)
             (funcall fn (emms-get-lyrics-url artist title) buffer)
             (goto-char (point-min))
             (if (and
                  (search-forward "Genre" nil t)
                  (not (search-forward "No results." nil t)))
                 (let ((frominsert ""))
                   (forward-line 10)
                   (delete-region (point-min) (1+ (line-end-position)))
                   (insert title " by " artist "\n\n")
                   (goto-char (point-max))
                   (if (search-backward "Correct these lyrics" nil t)
                       (progn (beginning-of-line)(forward-line -1)(delete-region (point) (point-max))))
                   (when file 
                     (rename-buffer bname)
                     (save-buffer)))
               (delete-region (point-min) (point-max))
               (insert "Unable to find lyrics for " title " by " artist)
               (if file (set-buffer-modified-p nil)))

             (goto-char (point-min))
             (emms-get-lyrics-mode)
             (switch-to-buffer buffer)
             (goto-char (point-min)))))))

(defun emms-get-lyrics-current-song ()
  (interactive)
  (let* ((track (emms-playlist-current-selected-track))
         (artist (cdr (assoc 'info-artist track)))
         (title (cdr (assoc 'info-title track))))
    (if (and emms-player-playing-p artist title)
        (emms-get-lyrics artist title 'emms-get-lyrics-w3m
                         (if emms-get-lyrics-use-files
                             (if emms-get-lyrics-dir
                                 (concat
                                  emms-get-lyrics-dir
                                  "/"
                                  (replace-regexp-in-string
                                   " " "_"
                                   (concat
                                    artist
                                    "-"
                                    title
                                    ".lyrics")))
                               (concat
                                (file-name-sans-extension (cdr (assoc 'name track)))
                                ".lyrics"))))
      (message "Nothing playing right now, or no artist, title"))))

(provide 'emms-get-lyrics)
;;; emms-get-lyrics.el ends here
