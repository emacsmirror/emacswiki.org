;;; di.el --- Simple directory editor for GNU Emacs

;; Copyright (C) 1998 Joe Keane <jgk@jgk.org>

;; This file is public domain.

;; Suggested usage:
;; (global-set-key "\C-xd" 'di-edit-directory)
;; (global-set-key "\M-`" 'di-edit-current-directory)
;; (autoload 'dired-noselect "di" nil t)
;; (autoload 'di-edit-directory "di" nil t)
;; (autoload 'di-edit-current-directory "di" nil t)

;; entry points

(defun di-edit-directory (dirfile)
  "Fire up Di on some directory."
  (interactive "DEdit directory: ")
  (find-file dirfile))

(defun di-edit-current-directory nil
  "Fire up Di on the current directory."
  (interactive)
  (di-edit-directory default-directory))

(defun di-edit-next-file ()
  "Edit the file after the current one."
  (interactive)
  (di-edit-current-directory)
  (di-next-line)
  (di-edit-this))

(defun di-edit-previous-file ()
  "Edit the file before the current one."
  (interactive)
  (di-edit-current-directory)
  (di-previous-line)
  (di-edit-this))

;; hook into find-file

(defun dired-noselect (dirfile)
  "Find a Di buffer for DIRFILE, or make one if none exists.  Follow any links."
  (setq dirfile (canonicalize-file-name dirfile))
  (let*
      ((bufname (di-buffer-name dirfile))
       (buf (get-buffer bufname)))
    (if buf
	(progn
	  (or (file-exists-p dirfile)
	      (error "Directory %s no longer exists!" dirfile))
	  (save-excursion
	    (set-buffer buf)
	    (or (eq major-mode 'di-mode)
		(error "Buffer isn't in Di mode!"))))
      (setq buf (get-buffer-create bufname))
      (save-excursion
	(set-buffer buf)
	(setq default-directory (file-name-as-directory dirfile))
	(di-mode)
	(di-redo-buffer)))
    buf))

;; the major mode

(defun di-mode ()
  "Major mode for editing directories, similar to dired, but better.
The current file or `this' file is the one whose name point is sitting on.
Some useful commands are:
e edit this file
d delete this file
r rename this file
c copy this file
m mark this file
u unmark this file
D delete marked files
R rename marked files
C copy marked files"
  (kill-all-local-variables)
  (setq buffer-read-only t)
  (setq selective-display t)
  (setq major-mode 'di-mode)
  (setq mode-name "Di")
  (setq mode-line-buffer-identification '("Di: %17b"))
  (use-local-map di-mode-map)
  (run-hooks 'di-mode-hook))
(put 'di-mode 'mode-class 'special)

(defvar di-quick nil "If non-nil, Di operates in `quick' mode; that is, it doesn't stat files.")
(defvar di-uid-alist nil "Assoc list to translate uids to login names.  What a lose.")
(defvar di-mode-map nil "Local keymap for Di buffers.")
(setq di-uid-alist
  (cons
   (cons
    (user-uid)
    (format "%-8s" (user-login-name)))
   '((0 . "root    "))))
(setq di-mode-map
 '(keymap
   [nil nil nil (keymap (?\C-m . di-mark-regexp) (?c . di-copy-regexp) (?m . di-mark-regexp) (?r . di-rename-regexp) (?t . di-toggle-regexp) (?u . di-unmark-regexp) (?\C-? . di-unmark-regexp)) nil nil nil nil di-previous-line nil nil nil nil di-mark-this di-next-line nil
    di-previous-line nil nil nil nil nil di-scroll-up nil nil nil nil (keymap (?\C-m . di-mark-all) (?< . di-beginning-of-buffer) (?> . di-end-of-buffer) (?m . di-mark-all) (?u . di-unmark-all) (?v . di-scroll-down) (?\C-? . di-unmark-all)) nil nil nil nil
    di-next-line di-background undefined di-mark-temporaries undefined di-replace-string di-make-directory undefined undefined undefined di-mark-executables di-mark-this di-mark-character-devices negative-argument di-mark-dot-files di-mark-directories
    digit-argument digit-argument digit-argument digit-argument digit-argument digit-argument digit-argument digit-argument digit-argument digit-argument undefined di-mark-block-devices di-unmark-previous di-mark-sockets di-mark-this di-summary
    di-mark-links undefined undefined di-copy-marked di-delete-marked undefined undefined di-redo-buffer di-hide-marked undefined undefined undefined undefined di-mark-all di-end-of-buffer undefined
    di-beginning-of-buffer undefined di-rename-marked di-show-all di-toggle-all di-unmark-all di-scroll-down undefined undefined undefined undefined undefined di-edit-parent undefined di-edit-parent undefined
    di-edit-parent undefined undefined di-copy-this di-delete-this di-edit-this di-edit-this di-update-buffer di-hide-this undefined di-jump di-edit-this-kill-buffer di-list-this di-mark-this di-next-line di-edit-this-other-window
    di-previous-line di-toggle-quickness di-rename-this di-show-this di-toggle-this di-unmark-this di-scroll-up undefined di-mark-by-extension undefined undefined undefined undefined undefined di-mark-backups di-unmark-previous
    ]
   ))

;; various bound functions

(defun di-scroll-up ()
  "Scroll the window up, ending up on a filename."
  (interactive)
  (scroll-up)
  (move-to-column 4))

(defun di-scroll-down ()
  "Scroll the window down, ending up on a filename."
  (interactive)
  (scroll-down)
  (move-to-column 4))

(defun di-beginning-of-buffer ()
  "Move to the first filename in the buffer."
  (interactive)
  (goto-char (point-min))
  (move-to-column 4))

(defun di-end-of-buffer ()
  "Move to the last filename in the buffer."
  (interactive)
  (goto-char (1- (point-max)))
  (move-to-column 4))

(defun di-background ()
  "Start up a background command, with the list of marked files as a default string."
  (interactive)
  (let (files)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^>" nil t)
	(setq files
	  (concat files " "
	    (buffer-substring (+ (point) 3) (progn (end-of-line) (point)))))))
    (background (read-string "% " files))))

(defun di-summary ()
  "Tell the user there's no documentation."
  (interactive)
  (message "I don't know what you're doing, do you?"))

(defun di-copy-marked (directory)
  "Copy the marked files to DIRECTORY."
  (interactive "DCopy marked files to directory: ")
  (setq directory (file-name-as-directory directory))
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^>" nil t)
      (let*
	  ((old (di-this-name))
	   (new (concat directory old)))
	(copy-file old new 0)
	(di-create-name new)))))

(defun di-delete-marked ()
  "Delete the marked files, asking for confirmation."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (and
     (save-window-excursion
       (with-output-to-temp-buffer "*deletions*"
	 (while (re-search-forward "^>" nil t)
	   (princ (di-this-name))
	   (princ "  ")))
       (y-or-n-p "Delete these files? "))
     (progn
       (goto-char (point-min))
       (let (buffer-read-only)
	 (while (re-search-forward "^>" nil t)
	   (di-delete-file (di-this-name)))))))
  (backward-char 1)
  (move-to-column 4))

(defun di-redo-buffer ()
  "Completely redo the buffer so it matches the contents of the directory.
Point is put after the dot files.  All state in the buffer is lost, such as
current, marked, and hidden files."
  (interactive)
  (let
      (buffer-read-only
       (files (directory-files default-directory)))
    (or files (error "Can't list directory"))
    (erase-buffer)
    (mapcar 'di-insert-name files))
  (and (re-search-backward "^... \\." nil t)
       (progn
	 (forward-line 2)
	 (backward-char 1)
	 (move-to-column 4))))

(defun di-hide-marked ()
  "Hide the marked files."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let (buffer-read-only)
      (and (looking-at ">") (replace-match "\r"))
      (while (search-forward "\n>" nil t)
	(replace-match "\r"))))
  (move-to-column 4))

(defun di-rename-marked (directory)
  "Rename the marked files to DIRECTORY."
  (interactive "DRename marked files to directory: ")
  (setq directory (file-name-as-directory directory))
  (save-excursion
    (goto-char (point-min))
    (let (buffer-read-only)
      (while (re-search-forward "^>" nil t)
	(let*
	    ((old (di-this-name))
	     (new (concat directory old)))
	  (rename-file old new 0)
	  (di-delete-name)
	  (di-create-name new)))))
  (backward-char 1)
  (move-to-column 4))

(defun di-show-all ()
  "Show all the hidden files."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let (buffer-read-only)
      (and (looking-at "\r") (replace-match " "))
      (while (search-forward "\r" nil t)
	(replace-match "\n "))))
  (move-to-column 4))

(defun di-edit-parent ()
  "Edit the parent of the current directory."
  (interactive)
  (find-file ".."))

(defun di-copy-this (destination)
  "Copy the current file to DESTINATION."
  (interactive "FCopy this to: ")
  (let*
      ((old (di-this-name))
       (new
	(if (not (file-directory-p destination))
	    destination
	  (concat (file-name-as-directory destination) old))))
    (and (file-directory-p old) (error "Can't copy directory"))
    (copy-file old new 0)
    (let (buffer-read-only)
      (di-create-name new)))
  (move-to-column 4))

(defun di-delete-this ()
  "Delete the current file, asking for confirmation."
  (interactive)
  (let ((name (di-this-name)))
    (and
     (y-or-n-p (format "Delete `%s'? " name))
     (let (buffer-read-only)
       (di-delete-file name))))
  (and (eobp)
       (backward-char 1))
  (move-to-column 4))

(defun di-delete-file (file)
  "Delete FILE, and the current name in the buffer, which we assume is FILE's.
If FILE is a directory, the user is asked for confirmation and the directory is
removed by forking rmdir."
  (if (not (eq (car (file-attributes file)) 't))
      (delete-file file)
    (and (cdr (cdr (directory-files file)))
	 (error "Directory `%s' not empty" file))
    (call-process "rmdir" nil nil nil (expand-file-name file))
    (and (file-exists-p file)
	 (error "Directory `%s' not removed" file)))
  (di-delete-name))

(defun di-edit-this ()
  "Edit the current file, switching to its buffer."
  (interactive)
  (find-file (di-this-name)))

(defun di-update-buffer ()
  "Incrementally update the buffer to match the current contents of the directory.
Current and marked file information is retained, but all files are unhidden."
  (interactive)
  (di-show-all)
  (let (buffer-read-only)
    (or di-quick (di-delete-name))
    (save-excursion
      (goto-char (point-min))
      (let ((files (directory-files default-directory)))
	(let
	    ((file (car files))
	     (name (di-this-name)))
	  (while (and files (not (eobp)))
	    (cond
	     ((string-equal file name)
	      (forward-line)
	      (setq file (car (setq files (cdr files))))
	      (setq name (di-this-name)))
	     ((string-lessp file name)
	      (di-insert-name file)
	      (setq file (car (setq files (cdr files)))))
	     (t
	      (di-delete-name)
	      (setq name (di-this-name))))))
	(if files
	    (mapcar 'di-insert-name files)
	  (or (eobp) (delete-region (1- (point)) (point-max)))))))
  (and (eobp)
       (backward-char 1))
  (move-to-column 4))

(defun di-hide-this ()
  "Hide the current file."
  (interactive)
  (beginning-of-line)
  (let (buffer-read-only)
    (delete-char 1)
    (or (bobp) (delete-backward-char 1))
    (insert ?\r))
  (move-to-column 4))

(defun di-edit-this-kill-buffer ()
  (interactive)
  (find-alternate-file (di-this-name)))

(defun di-show-this ()
  "Show the files hidden on the current line."
  (interactive)
  (beginning-of-line)
  (let
      (buffer-read-only
       (bol (point)))
    (and (looking-at "\r") (replace-match " "))
    (end-of-line)
    (while (search-backward "\r" bol t)
      (replace-match "\n ")))
  (move-to-column 4))

(defun di-edit-this-other-window ()
  "Edit the current file, switching to its buffer in another window."
  (interactive)
  (find-file-other-window (expand-file-name (di-this-name))))

;;; Modification/Simplification/Bug Insertion Comments:
;;; To whack off 90% of the time to lookup a file's owner uid in /etc/passwd,
;;; the regexp for finding the line has been simplified to finding the first
;;; match of ":uid:".  This is a problem if there exists a groupid = the pid.
;;; I suggest creating an assoc list with these (and other frequently
;;; used uids) to avoid conflict.
;;;
;;; Original:
;;;		 (re-search-forward (format "^[^:]*:[^:]*:%d:" uid) nil t)
;;; Modification:
;;;		 (search-forward (format ":%d:" uid) nil t)
;;;

(defun di-list-this ()
  "List the current file, a la `ls -l'."
  (interactive)
  (let*
      ((name (di-this-name))
       (attr (file-attributes name)))
    (if (not attr)
	(message "Can't stat this.")
      (message
	"%s%3d %8s%9d %s %s%s"
	(nth 8 attr)
	(car (cdr attr))
	(let ((uid (nth 2 attr)))
	  (or (cdr (assoc uid di-uid-alist))
	      (save-window-excursion
		(message "Looking up uid %d..." uid)
		(find-file "/etc/passwd")
		(bury-buffer (current-buffer))
		(goto-char (point-min))
		(let
		    ((username
		      (format
		       "%-8s"
		       (if (not (search-forward (format ":%d:" uid) nil t))
			   (int-to-string uid)
			 (progn
			   (beginning-of-line)
			   (buffer-substring
			    (point)
			    (progn (search-forward ":") (1- (point)))))))))
		  (nconc di-uid-alist (list (cons uid username)))
		  username))))
	(nth 7 attr)
	(let ((date (nth 5 attr)))
	  (if (>= (string-to-int (substring emacs-version 0 2)) 19)
	      (substring (current-time-string date) 4 16)
	    (format " [%04x%04x] " (car date) (cdr date))))
	name
	(let ((link (car attr)))
	  (if (stringp link) (concat " -> " link) ""))))))

(defun di-next-line (&optional prefix)
  "Move to the next line."
  (interactive "p")
  (forward-line prefix)
  (and (eobp)
       (forward-line -1))
  (move-to-column 4))

(defun di-jump ()
  (interactive)
  (message "J\"\"")
  (let
      ((str "")
       char
       failed)
    (while
	(progn
	  (setq char (read-char))
	  (cond
	   ((= char ?\C-?)
	    (if (string-equal str "")
		(message "Empty string")
	      (setq str (substring str 0 -1)))
	    (goto-char (point-min))
	    t)
	   ((or (< char 32) (>= char 128)) nil)
	   (t
	    (and (string-equal str "") (goto-char (point-min)))
	    (or failed (setq str (concat str (char-to-string char))))
	    t)))
      (beginning-of-line)
      (setq
       failed
       (not (re-search-forward (concat "^...." (regexp-quote str)) nil t)))
      (while (and (string-lessp (di-this-name) str) (not (eobp)))
	(forward-line 1))
      (message "%sJ\"%s\"" (if failed "F" "") str)
      ))
  (and (eobp) (backward-char 1))
  (move-to-column 4))

(defun di-previous-line (&optional prefix)
  "Move to the previous line."
  (interactive "p")
  (forward-line (- (or prefix 1)))
  (and (eobp)
       (forward-line -1))
  (move-to-column 4))

(defun di-toggle-quickness ()
  "Toggle Di's global quickness (whether it stats files), and tell what it is."
  (interactive)
  (setq di-quick (not di-quick))
  (message "Di is now %s." (if di-quick "QUICK" "SLOW")))

(defun di-rename-this (destination)
  "Rename the current file to DESTINATION."
  (interactive "FRename this to: ")
  (let*
      ((old (di-this-name))
       (new
	(if (not (file-directory-p destination))
	    destination
	  (concat (file-name-as-directory destination) old))))
    (rename-file old new 0)
    (save-excursion
      (let ((buffer-read-only))
	(di-delete-name)
	(di-create-name new))))
  (and (eobp)
       (forward-line -1))
  (move-to-column 4))

;; un/marking functions

(defun di-mark-regexp (regexp)
  "Mark (unmark with prefix) all files completely matching REGEXP."
  (interactive "sMark files completely matching regexp: ")
  (di-mark-main (format ".. %s$" regexp)))

(defun di-unmark-regexp (regexp)
  "Unmark (mark with prefix) all files completely matching REGEXP."
  (interactive "sUnmark files completely matching regexp: ")
  (setq current-prefix-arg (not current-prefix-arg))
  (di-mark-main (format ".. %s$" regexp)))

(defun di-mark-all ()
  "Mark (unmark with prefix) all files except `.' and `..'."
  (interactive)
  (di-mark-main ".. \\([^.]\\|[.][^.\n]\\)"))

(defun di-toggle-all ()
  "Exchange the sets of marked and unmarked files."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (and (looking-at "\r") (forward-line))
    (let (buffer-read-only)
      (while (not (eobp))
	(insert (if (prog1 (= (following-char) ? ) (delete-char 1)) ?> ? ))
	(forward-line)))))

(defun di-unmark-all ()
  "Unmark (mark with prefix) all files."
  (interactive)
  (let ((current-prefix-arg (not current-prefix-arg)))
    (di-mark-main "")))

(defun di-mark-temporaries ()
  "Mark (unmark with prefix) all temporary files (those beginning with `#' or
ending in `.CKP')."
  (interactive)
  (di-mark-main ".. \\(#\\|.*\\.CKP$\\)"))

(defun di-make-directory (directory)
  "Make DIRECTORY by forking mkdir."
  (interactive "FMake directory: ")
  (and (file-exists-p directory)
       (error "Directory `%s' already exists" directory))
  (call-process "mkdir" nil nil nil (expand-file-name directory))
  (or (file-exists-p directory)
      (error "Directory `%s' not made" directory))
  (di-create-name directory)
  (move-to-column 4))

(defun di-mark-executables ()
  "Mark (unmark with prefix) all executable files."
  (interactive)
  (di-mark-main "\\*"))

(defun di-mark-character-devices ()
  "Mark (unmark with prefix) all character devices."
  (interactive)
  (di-mark-main ","))

(defun di-mark-dot-files ()
  "Mark (unmark with prefix) all files beginning with `.'."
  (interactive)
  (di-mark-main ".. \\."))

(defun di-mark-directories ()
  "Mark (unmark with prefix) all directories."
  (interactive)
  (di-mark-main "/"))

(defun di-mark-block-devices ()
  "Mark (unmark with prefix) all block devices."
  (interactive)
  (di-mark-main ";"))

(defun di-unmark-previous (arg)
  "Unmark the previous ARG unhidden lines and end up on the first; with negative
ARG, unmark the next -ARG unhidden lines and end up after the last."
  (interactive "p")
  (di-unmark-this (- (or arg 1))))

(defun di-mark-sockets ()
  "Mark (unmark with prefix) all sockets.  This is of questionable value since
you don't find many sockets lying around."
  (interactive)
  (di-mark-main "="))

(defun di-mark-this (arg)
  "Mark the next ARG unhidden lines and end up after the last; with negative ARG,
unmark the -ARG unhidden lines before the current one and end up on the
first."
  (interactive "p")
  (or arg (setq arg 1))
  (beginning-of-line)
  (let (buffer-read-only)
    (if (< arg 0)
	(while (and (not (bobp)) (< arg 0))
	  (forward-line -1)
	  (and
	   (looking-at " ")
	   (progn (replace-match ">") (setq arg (1+ arg)))))
      (while (and (not (eobp)) (> arg 0))
	(and
	 (looking-at " ")
	 (progn (replace-match ">") (setq arg (1- arg))))
	(forward-line))))
  (forward-line 1)
  (backward-char 1)
  (move-to-column 4))

(defun di-mark-links ()
  "Mark (unmark with prefix) all symbolic links."
  (interactive)
  (di-mark-main ".@"))

(defun di-mark-by-extension (extension)
  "Mark (unmark with prefix) all files having EXTENSION."
  (interactive "sMark files with extension: .")
  (di-mark-main (format ".*\\.%s$" extension)))

(defun di-toggle-this ()
  "If the current file is marked, unmark it, or vice versa."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let (buffer-read-only)
      (cond
       ((looking-at " ")
	(replace-match ">"))
       ((looking-at "\r")
	(error "This line contains hidden files"))
       ((looking-at ">")
	(replace-match " "))))))

(defun di-unmark-this (arg)
  "Unmark the next ARG unhidden lines and end up after the last, or with negative
ARG, unmark the -ARG unhidden lines before the current one and end up on the
first."
  (interactive "p")
  (or arg (setq arg 1))
  (beginning-of-line)
  (let (buffer-read-only)
    (if (< arg 0)
	(progn
	  (while 
	      (progn
		(and
		 (looking-at ">")
		 (progn (replace-match " ") (setq arg (1+ arg))))
		(and (not (bobp)) (< arg 0)))
	    (forward-line -1))
	  (end-of-line))
      (while (and (not (eobp)) (> arg 0))
	(and (looking-at ">")
	  (progn (replace-match " ") (setq arg (1- arg))))
	(forward-line))))
  (backward-char 1)
  (move-to-column 4))

(defun di-mark-backups ()
  "Mark (unmark with prefix) all backup files, those ending with `~' or `.BAK'."
  (interactive)
  (di-mark-main ".*\\(~\\|\\.BAK\\)$"))

(defun di-mark-main (regexp)
  "Mark (unmark with prefix) all files matching REGEXP starting at their file type
 (second column).  This function is called by all the marking commands."
  (save-excursion
    (goto-char (point-min))
    (setq regexp (concat (if current-prefix-arg "^>" "^ ") regexp))
    (let
	(buffer-read-only
	 (char (if current-prefix-arg ?  ?>)))
      (while (re-search-forward regexp nil t)
	(beginning-of-line)
	(delete-char 1)
	(insert char)))))

(defun di-replace-string (old-string new-string)
  "Replace OLD-STRING with NEW-STRING in filenames."
  (interactive "sOld string: \nsNew string: ")
  (and (or (string-match "\n" old-string) (string-match "\n" new-string))
       (error "String contains newline"))
  (save-excursion
    (goto-char (point-min))
    (let
	((regexp (concat "^... .*\\(" (regexp-quote old-string) "\\).*$"))
	 bol
	 (files nil)
	 buffer-read-only)
      (while
	  (re-search-forward regexp nil t)
	(setq bol (match-beginning 0))
	(rename-file
	 (buffer-substring (+ bol 4) (point))
	 (car
	  (setq files
	   (cons
	    (concat
	     (buffer-substring (+ bol 4) (match-beginning 1))
	     new-string
	     (buffer-substring (match-end 1) (point)))
	    files))))
	 (if (not (eobp))
	     (delete-region bol (1+ (point)))
	   (delete-region (1- bol) (point))))
    (mapcar 'di-insert-name-search files)))
  (move-to-column 4))

;; low-level subroutines

(defun di-this-name ()
  "Return the name of the current file."
  (save-excursion
    (move-to-column 4)
    (buffer-substring (point) (progn (end-of-line) (point)))))

(defun di-delete-name ()
  "Delete the current line from a Di buffer."
  (beginning-of-line)
  (delete-region (point) (progn (forward-line 1) (point))))

(defun di-insert-name (name)
  "Insert a line for NAME before the current line."
  (if (string-match "\n" name)
      (message "Filename contains newline")
    (insert
     (if di-quick
	 "    "
       (let ((attr (file-attributes name)))
	 (if (not attr)
	     " X  "
	   (let ((link (car attr)))
	     (cond
	      ((eq link 't) " /  ")
	      ((stringp link)
	       (if (not (setq attr (file-attributes link)))
		   " X@ "
		 (cond
		  ((eq (setq link (car attr)) 't) " /@ ")
		  ((stringp link) " @@ ")
		  ((= (aref (nth 8 attr) 3) ?-) "  @ ")
		  (t " *@ "))))
	      (t
	       (let*
		   ((mode (nth 8 attr))
		    (type (string-to-char mode)))
		 (cond
		  ((= type ?-)
		   (let ((exec (aref mode 3)))
		     (cond
		      ((= exec ?-) (if (zerop (nth 7 attr)) "  0 " "    "))
		      ((= exec ?x) (if (zerop (nth 7 attr)) " *0 " " *  "))
		      ((= exec ?s) " ** ")
		      (t " *? "))))
		  ((= type ?c) " ,  ")
		  ((= type ?b) " ;  ")
		  ((= type ?p) " |  ")
		  ((= type ?s) " =  ")
		  (t " ?? ")))))))))
     name
     "\n")))

(defun di-create-name (file)
  "Create a listing for FILE in the right place in the right Di buffer.  If this
buffer doesn't exist, forget it."
  (interactive "FCreate name for file: ")
  (let*
      ((file (expand-file-name file))
       (buf
	(get-buffer
	 (di-buffer-name (directory-file-name (file-name-directory file))))))
    (and buf
	 (save-window-excursion
	   (set-buffer buf)
	   (let (buffer-read-only)
	     (di-insert-name-search (file-name-nondirectory file)))))))

(defun di-insert-name-search (name)
  (goto-char (point-min))
  (while (and (not (eobp)) (string-lessp (di-this-name) name))
    (forward-line))
  (or (string-equal (di-this-name) name)
      (let (buffer-read-only) (di-insert-name name))))

(defun di-buffer-name (dir)
  "Return the name of the buffer corresponding to the given directory."
  (concat (file-name-nondirectory dir) " " (file-name-directory dir)))

;; utility function

(defun canonicalize-file-name (name)
  (setq name (expand-file-name (directory-file-name name)))
  (or (string-equal name "/")
      (progn
	(setq name
	 (concat
	  (file-name-as-directory
	   (canonicalize-file-name
	    (directory-file-name (file-name-directory name))))
	  (file-name-nondirectory name)))
	(let (link)
	  (while (stringp (setq link (car (file-attributes name))))
	    (progn
	      (setq name (expand-file-name link (file-name-directory name)))
	      (and (string-match "^//" name) ; bug in expand-file-name
		   (setq name (substring name 1))))))))
  name)

(provide 'di)
;;; di.el ends here
