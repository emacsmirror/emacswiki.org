;;; syslog-mode.el --- Major-mode for viewing log files

;; Filename: syslog-mode.el
;; Description: Major-mode for viewing log files
;; Author: Harley Gorrell <harley@mahalito.net>
;; Maintainer: Joe Bloggs <vapniks@yahoo.com>
;; Created: 2003-03-17 18:50:12 Harley Gorrell
;; Version: 2.2
;; Last-Updated: 2013-10-21 19:04:00
;;           By: Joe Bloggs
;; URL: https://github.com/vapniks/syslog-mode
;; Keywords: unix
;; Compatibility: GNU Emacs 24.3.1
;; Package-Requires:  ((hide-lines "20130623"))
;;
;; Features that might be required by this library:
;;
;; hide-lines cl ido
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; version 2

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;;; Commentary:
;; * Handy functions for looking at system logs.
;; * Fontifys the date and su messages.

;;; Keybindings
;; "C-down" : syslog-boot-start
;; "R"      : revert-buffer
;; "/"      : syslog-filter-lines
;; "g"      : hide-lines-show-all
;; "h r"    : highlight-regexp
;; "h p"    : highlight-phrase
;; "h l"    : highlight-lines-matching-regexp
;; "h u"    : unhighlight-regexp
;; "C-/"    : syslog-filter-dates
;; "D"      : open dired buffer in log directory (`syslog-log-file-directory')
;; "j"      : ffap
;; "<"      : syslog-previous-file
;; ">"      : syslog-next-file
;; "o"      : syslog-open-files
;; "q"      : quit-window

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `syslog-open-files'
;;    Open consecutive log files in same buffer.
;;  `syslog-previous-file'
;;    Open the previous logfile backup, or the next one if a prefix arg is used.
;;  `syslog-next-file'
;;    Open the next logfile.
;;  `syslog-filter-lines'
;;    Restrict buffer to lines matching regexp.
;;  `syslog-filter-dates'
;;    Restrict buffer to lines between dates.
;;  `syslog-mode'
;;    Major mode for working with system logs.
;;  `syslog-boot-start'
;;    Jump forward in the log to when the system booted.
;;


;;; Customizable Options:
;;
;; Below are customizable option list:
;;
;;  `syslog-datetime-regexp'
;;    A regular expression matching the date-time at the beginning of each line in the log file.
;;    default = "^[a-z]\\{3\\} [0-9]\\{1,2\\} \\([0-9]\\{2\\}:\\)\\{2\\}[0-9]\\{2\\} "
;;  `syslog-log-file-directory'
;;    The directory in which log files are stored.
;;    default = "/var/log/"
;;  `syslog-ip'
;;    Face for IPs
;;    default = (quote ((t :underline t :slant italic ...)))
;;  `syslog-hour'
;;    Face for IPs
;;    default = (quote ((t :weight bold :inherit font-lock-type-face)))
;;  `syslog-error'
;;    Face for IPs
;;    default = (quote ((t :weight bold :foreground "red")))
;;  `syslog-warn'
;;    Face for IPs
;;    default = (quote ((t :weight bold :foreground "goldenrod")))
;;  `syslog-info'
;;    Face for IPs
;;    default = (quote ((t :weight bold :foreground "deep sky blue")))
;;  `syslog-debug'
;;    Face for IPs
;;    default = (quote ((t :weight bold :foreground "medium spring green")))
;;  `syslog-su'
;;    Face for IPs
;;    default = (quote ((t :weight bold :foreground "firebrick")))
;;
;; All of the above can customized by:
;;      M-x customize-group RET syslog-mode RET
;;

;;; Installation:
;;
;; Put syslog-mode.el in a directory in your load-path, e.g. ~/.emacs.d/
;; You can add a directory to your load-path with the following line in ~/.emacs
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;; where ~/elisp is the directory you want to add
;; (you don't need to do this for ~/.emacs.d - it's added by default).
;;
;; Add the following to your ~/.emacs startup file.
;;
;; (require 'syslog-mode)



;;; Change log:
;;
;; 21-03-2013    Joe Bloggs
;;    Added functions and keybindings for filtering
;;    lines by regexps or dates, and for highlighting,
;;    and quick key for find-file-at-point
;;
;; 20-03-2013    Christian Giménez
;;    Added more keywords for font-lock.
;;
;; 16-03-2003 : Updated URL and contact info.

;;; Acknowledgements:
;;
;;  Harley Gorrell  (Author)
;;  Christian Giménez
;;

;; If anyone wants to make changes please fork the following github repo: https://github.com/vapniks/syslog-mode

;;; TODO: statistical reporting - have a regular expression to match item type, then report counts of each item type.
;;        also statistics on number of items per hour/day/week/etc.


;;; Require
(require 'hide-lines)
(eval-when-compile (require 'cl))
(require 'ido)

;;; Code:

;; Setup
(defgroup syslog nil
  "syslog-mode - a major mode for viewing log files"
  :link '(url-link "https://github.com/vapniks/syslog-mode"))

(defvar syslog-mode-hook nil
  "*Hook to setup `syslog-mode'.")

(defvar syslog-mode-load-hook nil
  "*Hook to run when `syslog-mode' is loaded.")

;;;###autoload
(defvar syslog-setup-on-load nil
  "*If not nil setup syslog mode on load by running syslog-add-hooks.")

;; I also use "Alt" as C-c is too much to type for cursor motions.
(defvar syslog-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Ctrl bindings
    (define-key map [C-down] 'syslog-boot-start)
    (define-key map "R" 'revert-buffer)
    (define-key map "/" 'syslog-filter-lines)
    (define-key map "g" 'hide-lines-show-all)
    (define-prefix-command 'syslog-highlight-map)
    (define-key map "h" 'syslog-highlight-map)
    (define-key map (kbd "h r") 'highlight-regexp)
    (define-key map (kbd "h p") 'highlight-phrase)
    (define-key map (kbd "h l") 'highlight-lines-matching-regexp)
    (define-key map (kbd "h u") 'unhighlight-regexp)
    (define-key map (kbd "C-/") 'syslog-filter-dates)
    (define-key map "D" (lambda nil (interactive) (dired syslog-log-file-directory)))
    (define-key map "j" 'ffap)
    (define-key map "<" 'syslog-previous-file)
    (define-key map ">" 'syslog-next-file)
    (define-key map "o" 'syslog-open-files)
    (define-key map "q" 'quit-window)
    ;; XEmacs does not like the Alt bindings
    (if (string-match "XEmacs" (emacs-version))
	t)
    map)
  "The local keymap for `syslog-mode'.")

(defvar syslog-number-suffix-start 1
  "The first number used as rotation suffix.")

(defun syslog-get-basename-and-number (filename)
  "Return the basename and number suffix of a log file in FILEPATH.
Return results in a cons cell '(basename . number) where basename is a string,
and number is a number."
  (let* ((res (string-match "\\(.*?\\)\\.\\([0-9]+\\)\\(\\.t?gz\\)?" filename))
         (basename (if res (match-string 1 filename) filename))
         (str (and res (match-string 2 filename)))
         (num (or (and str (string-to-number str)) (1- syslog-number-suffix-start))))
    (cons basename num)))

(defun syslog-open-files (filename num)
  "Open consecutive log files in same buffer.
When called interactively the user is prompted for the initial file FILENAME,
and the number NUM of consecutive backup files to include."
  (interactive (list (ido-read-file-name "Log file: " syslog-log-file-directory "syslog" t)
                     (read-number "Number of consecutive backup files to include" 0)))
  (let* ((pair (syslog-get-basename-and-number filename))
         (basename (car pair))
         (curver (cdr pair))
         (buf (get-buffer-create
               (concat (file-name-nondirectory basename)
                       "[" (number-to-string curver) "-"
                       (number-to-string (+ curver num)) "]"))))
    (with-current-buffer buf
      (erase-buffer)
      (goto-char (point-min))
      (insert-file-contents filename)
      (loop for n from (1+ curver) to (+ curver num)
            for numsuffix = (concat "." (number-to-string n))
            for nextfile = (loop for suffix in '(nil ".gz" ".tgz")
                                 if (file-readable-p (concat basename numsuffix suffix))
                                 return (concat basename numsuffix suffix))
            if nextfile do
            (goto-char (point-min))
            (insert-file-contents nextfile))
      (goto-char (point-min))
      (syslog-mode))
    (switch-to-buffer buf)))

(defun syslog-previous-file (&optional arg)
  "Open the previous logfile backup, or the next one if a prefix arg is used.
Unix systems keep backups of log files with numbered suffixes, e.g. syslog.1 syslog.2.gz, etc.
where higher numbers indicate older log files.
This function will load the previous log file to the current one (if it exists), or the next
one if ARG is non-nil."
  (interactive "P")
  (let* ((pair (syslog-get-basename-and-number buffer-file-name))
         (basename (car pair))
         (curver (cdr pair))
         (nextver (if arg (1- curver) (1+ curver)))
         (nextfile (if (> nextver (1- syslog-number-suffix-start))
                       (concat basename "." (number-to-string nextver))
                     basename)))
    (cond ((file-readable-p nextfile)
           (find-file nextfile))
          ((file-readable-p (concat nextfile ".bz2"))
           (find-file (concat nextfile ".bz2")))
          ((file-readable-p (concat nextfile ".gz"))
           (find-file (concat nextfile ".gz")))
          ((file-readable-p (concat nextfile ".tgz"))
           (find-file (concat nextfile ".tgz"))))))

(defun syslog-next-file nil
  "Open the next logfile.
This just calls `syslog-previous-file' with non-nil argument, so we can bind it to a key."
  (interactive)
  (syslog-previous-file t))

;;;###autoload
(defun syslog-filter-lines (&optional arg)
  "Restrict buffer to lines matching regexp.
With prefix arg: remove lines matching regexp."
  (interactive "p")
  (if (> arg 1)
      (let ((regex (read-regexp "Regexp matching lines to remove")))
        (unless (string= regex "")
          (hide-lines-matching regex)))
    (let ((regex (read-regexp "Regexp matching lines to keep")))
        (unless (string= regex "")
          (hide-lines-not-matching regex)))))

;;;###autoload
(defcustom syslog-datetime-regexp "^[a-z]\\{3\\} [0-9]\\{1,2\\} \\([0-9]\\{2\\}:\\)\\{2\\}[0-9]\\{2\\} "
  "A regular expression matching the date-time at the beginning of each line in the log file."
  :group 'syslog
  :type 'regexp)

(defcustom syslog-log-file-directory "/var/log/"
  "The directory in which log files are stored."
  :group 'syslog
  :type 'directory)

;;;###autoload
(defun* syslog-date-to-time (date &optional safe)
  "Convert DATE string to time.
If no year is present in the date then the current year is used.
If DATE can't be parsed then if SAFE is non-nil return nil otherwise throw an error."
  (if safe
      (let ((time (safe-date-to-time (concat date " " (substring (current-time-string) -4)))))
        (if (and (= (car time) 0) (= (cdr time) 0))
            nil
          time))
    (date-to-time (concat date " " (substring (current-time-string) -4)))))

;;;###autoload
(defun syslog-filter-dates (start end &optional arg)
  "Restrict buffer to lines between dates.
With prefix arg: remove lines between dates."
  (interactive (let (firstdate lastdate)
                 (save-excursion
                   (goto-char (point-min))
                   (beginning-of-line)
                   (re-search-forward syslog-datetime-regexp nil t)
                   (setq firstdate (match-string 0))
                   (goto-char (point-max))
                   (beginning-of-line)
                   (re-search-backward syslog-datetime-regexp nil t)
                   (setq lastdate (match-string 0)))
                 (list (syslog-date-to-time (read-string "Start date and time: "
                                                         firstdate nil firstdate))
                       (syslog-date-to-time (read-string "End date and time: "
                                                         lastdate nil lastdate))
                     current-prefix-arg)))
  (set (make-local-variable 'line-move-ignore-invisible) t)
  (goto-char (point-min))
  (let* ((start-position (point-min))
         (pos (re-search-forward syslog-datetime-regexp nil t))
         (intime-p (if arg (lambda (time)
                             (and time (not (and (time-less-p time end)
                                                 (not (time-less-p time start))))))
                     (lambda (time)
                       (and time (and (time-less-p time end)
                                      (not (time-less-p time start)))))))
         (keeptime (funcall intime-p (syslog-date-to-time (match-string 0) t)))
         (dodelete t))
    (while pos
      (cond ((and keeptime dodelete)
             (add-invisible-overlay start-position (point-at-bol))
             (setq dodelete nil))
            ((not (or keeptime dodelete))
             (setq dodelete t start-position (point-at-bol))))
      (setq pos (re-search-forward syslog-datetime-regexp nil t)
            keeptime (funcall intime-p (syslog-date-to-time (match-string 0) t))))
    (if dodelete (add-invisible-overlay start-position (point-max)))))

;;;###autoload
(defun syslog-mode ()
  "Major mode for working with system logs.

\\{syslog-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq mode-name "syslog")
  (setq major-mode 'syslog-mode)
  (use-local-map syslog-mode-map)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(syslog-font-lock-keywords))
  (toggle-read-only 1)
  (run-hooks 'syslog-mode-hook))

(defvar syslog-boot-start-regexp "unix: SunOS"
  "Regexp to match the first line of boot sequence.")

(defun syslog-boot-start ()
  "Jump forward in the log to when the system booted."
  (interactive)
  (search-forward-regexp syslog-boot-start-regexp (point-max) t)
  (beginning-of-line))

(defface syslog-ip
  '((t :underline t :slant italic :weight bold))
  "Face for IPs"
  :group 'syslog)

(defface syslog-hour
  '((t :weight bold  :inherit font-lock-type-face))
  "Face for IPs"
  :group 'syslog)

(defface syslog-error
  '((t  :weight bold :foreground "red"))
  "Face for IPs"
  :group 'syslog)

(defface syslog-warn
  '((t  :weight bold :foreground "goldenrod"))
  "Face for IPs"
  :group 'syslog)

(defface syslog-info
  '((t  :weight bold :foreground "deep sky blue"))
  "Face for IPs"
  :group 'syslog)

(defface syslog-debug
  '((t  :weight bold :foreground "medium spring green"))
  "Face for IPs"
  :group 'syslog)

(defface syslog-su
  '((t  :weight bold :foreground "firebrick"))
  "Face for IPs"
  :group 'syslog)

;; Keywords
;; Todo: Seperate the keywords into a list for each format, rather
;; than one for all.
(defvar syslog-font-lock-keywords
  '(
    ;; Hours: 17:36:00
    ("\\(?:^\\|[[:space:]]\\)\\([[:digit:]]\\{1,2\\}:[[:digit:]]\\{1,2\\}\\(:[[:digit:]]\\{1,2\\}\\)?\\)\\(?:$\\|[[:space:]]\\)" 1 'syslog-hour append)
    ;; Date
    ("\\(?:^\\|[[:space:]]\\)\\([[:digit:]]\\{1,2\\}/[[:digit:]]\\{1,2\\}/[[:digit:]]\\{2,4\\}\\)\\(?:$\\|[[:space:]]\\)" 1 'syslog-hour append)
    ;; Dates: May  9 15:52:34
    ("^\\(\\(?:[[:alpha:]]\\{3\\}\\)?[[:space:]]*[[:alpha:]]\\{3\\}\\s-+[0-9]+\\s-+[0-9:]+\\)" 1 'font-lock-type-face t)
    ;; Su events
    ("\\(su:.*$\\)" 1 'syslog-su t)
    ("\\(sudo:.*$\\)" 1 'syslog-su t)
    ("\\[[^]]*\\]" . 'font-lock-comment-face)
    ;; IPs
    ("[[:digit:]]\\{1,3\\}\\.[[:digit:]]\\{1,3\\}\\.[[:digit:]]\\{1,3\\}\\.[[:digit:]]\\{1,3\\}" 0 'syslog-ip append)
    ("[Ee][Rr][Rr]\\(?:[Oo][Rr]\\)?" 0 'syslog-error append)
    ("[Ii][Nn][Ff][Oo]" 0 'syslog-info append)
    ("STARTUP" 0 'syslog-info append)
    ("CMD" 0 'syslog-info append)
    ("[Ww][Aa][Rr][Nn]\\(?:[Ii][Nn][Gg]\\)?" 0 'syslog-warn append)
    ("[Dd][Ee][Bb][Uu][Gg]" 0 'syslog-debug append)
    ("(EE)" 0 'syslog-error append)
    ("(WW)" 0 'syslog-warn append)
    ("(II)" 0 'syslog-info append)
    ("(NI)" 0 'syslog-warn append)
    ("(!!)" 0 'syslog-debug append)
    ("(--)" 0 'syslog-debug append)
    ("(\\*\\*)" 0 'syslog-debug append)
    ("(==)" 0 'syslog-debug append)
    ("(\\+\\+)" 0 'syslog-debug append))
  "Expressions to hilight in `syslog-mode'.")

;;; Setup functions
(defun syslog-find-file-func ()
  "Invoke `syslog-mode' if the buffer appears to be a system logfile.
and another mode is not active.
This function is added to `find-file-hooks'."
  (if (and (eq major-mode 'fundamental-mode)
	   (looking-at syslog-sequence-start-regexp))
      (syslog-mode)))

(defun syslog-add-hooks ()
  "Add a default set of syslog-hooks.
These hooks will activate `syslog-mode' when visiting a file
which has a syslog-like name (.fasta or .gb) or whose contents
looks like syslog.  It will also turn enable fontification for `syslog-mode'."
  ;; (add-hook 'find-file-hooks 'syslog-find-file-func)
  (add-to-list
   'auto-mode-alist
   '("\\(messages\\(\\.[0-9]\\)?\\|SYSLOG\\)\\'" . syslog-mode)))

;; Setup hooks on request when this mode is loaded.
(if syslog-setup-on-load
    (syslog-add-hooks))

;; done loading
(run-hooks 'syslog-mode-load-hook)

(provide 'syslog-mode)

;;; syslog-mode.el ends here

;;; (magit-push)
;;; (yaoddmuse-post "EmacsWiki" "syslog-mode.el" (buffer-name) (buffer-string) "update")
