;;; itunes.el -- utility functions to interact with iTunes

;; Author: Sean O'Rourke <sorourke@cs.ucsd.edu>
;; Keywords: multimedia

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the Artistic License; see
;; http://www.perl.com/language/misc/Artistic.html

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

;;; Commentary:

;; Allows OS X Emacs users to control iTunes without having to leave
;; the safe confines of Emacs.  Simple stop/start/skip is supported,
;; plus some simple playlist management (with extra software).

;; The main entry point is `itunes-manip'.

;;; Code:
(require 'cl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;_ - User-level commands:
;;;###autoload
(defun itunes-select-playlist (name)
  "Select the playlist NAME from itunes' list of playlists."
  (interactive
   (let ((completion-ignore-case t))
     (list (completing-read "Playlist: "
			    (itunes-get-playlists)))))
  (do-applescript (concat
		   "tell application \"iTunes\" to play user playlist \""
		   name "\"")))

;;;###autoload
(defun itunes-toggle-repeat ()
  "Toggle between repeat-one, repeat-all, and no repeat."
  (interactive)
  (let ((res (applescript-list (do-applescript "\
tell application \"iTunes\"\r\
    tell current playlist\r\
        if song repeat is off then\r\
            set song repeat to all\r return \"all\"
        else if song repeat is all then\r\
            set song repeat to one\r return \"one\" 
        else\r\
            set song repeat to off\r return \"off\"
        end if\r\
    end tell\r\
end"))))
    (if (interactive-p) (message res))
    res
    ))

;;;###autoload
(defun itunes-select-itunes (str)
  "Tell iTunes to select and play a set of tunes.  Syntax is
applescript with some abbreviations:

    'a        artist
    't        name
    'c        composer
    'y        year
     ~        contains
    'op'      (name op or ... or composer op)

e.g.
    'a is \"Juno Reactor\" or ~ \"foo\"'
"
  (interactive "MQuery: " )
  (let ((pl "\"From Emacs\""))
    (dolist (kv '(("'t\\>" "name")
		  ("'a\\>" "artist")
		  ("'c\\>" "composer")
		  ("'y\\>" "year")
		  ("~" "contains")))
      (setq str (replace-regexp-in-string (car kv) (cadr kv) str)))
    (setq str (replace-regexp-in-string
	       "'\\([^']+\\)'"
	       "(name \\1 or artist \\1 or album \\1 or composer \\1)"
	       str))
    (message "playing %s" str)
    (do-applescript (concat
                     "tell application \"iTunes\"\r\
    set fixed indexing to true\r"
                     (itunes-reset-pl pl)
                     "    duplicate (every track of library playlist 1 whose ("
                     str ")) to playlist " pl "\r\
    play user playlist " pl "\r\
end tell"))))

;;;###autoload
(defun itunes-next-track (&optional n)
  "Go to the next track in the current playlist (or Nth next, if N given)"
  (interactive "p")
  (do-applescript
   (format
    "tell application \"iTunes\"\r\
repeat %d times\r\
next track\r\
end repeat\r\
end tell" (or n 1))))

;;;###autoload
(defun itunes-previous-track (&optional n)
  "Go to the previous track in the current playlist (or Nth
previous, if N given)"
  (interactive "p")
  (do-applescript
   (format
    "tell application \"iTunes\"\r\
repeat %d times\r\
back track\r\
end repeat\r\
end tell" (or n 1))))

;;;###autoload
(defun itunes-volume-up (&optional n)
  "Increase the volume by N (or 5%)"
  (interactive "P")
  (itunes-changev (or n 5)))

;;;###autoload
(defun itunes-volume-down (&optional n)
  "Increase the volume by N (or 5%)"
  (interactive "P")
  (itunes-changev (- (or n 5))))

;;;###autoload
(defun itunes-playpause (&optional name)
  (interactive)
  (do-applescript
   (if name
       (concat "tell application \"iTunes\" to play user playlist \""
	       name "\"")
     "tell application \"iTunes\" to playpause")))

;;;###autoload
(defun itunes-get-current ()
  (interactive)
  (destructuring-bind (name artist album)
      (applescript-list (do-applescript
                         "tell application \"iTunes\" to get {name,artist,album} of current track"))
    (if (interactive-p)
	(message "%s, %s (%s)" artist name album)
      (list name artist album))))

(defun itunes-manip ()
  "A pseudo-\"iTunes control mode\", providing single-key control
from the minibuffer:

    i,k                up, down volume
    j,l                next, previous track
    s,/                select new set of songs
    p                  choose named playlist
    <spc>              play/pause
    h                  help
"
  (interactive)
  (catch 'itunes-done
    (while t
      (case (read-char (apply #'format "%s, %s (%s) ['h' for help]"
			      (itunes-get-current)))
	(?l (itunes-next-track))
	(?j (itunes-previous-track))
	(?i (itunes-volume-up))
	(?k (itunes-volume-down))
	(?p (call-interactively 'itunes-select-playlist))
	((?s ?/) (call-interactively #'itunes-select))
	(?h (save-excursion
	      (switch-to-buffer-other-window
	       (get-buffer-create "*itunes-help*"))
	      (delete-region (point-min) (point-max))
	      (insert (documentation 'itunes-manip))))
	(?\  (itunes-playpause))
	(t (throw 'itunes-done nil))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;_ - General support code

(defvar itunes-playlists nil)

(defun itunes-get-playlists ()
  (or itunes-playlists
      (setq itunes-playlists
	    (applescript-list
	     (do-applescript "tell application \"iTunes\" to return name of every playlist")))))

(defun itunes-update-playlist (pl ids)
  (message "Playing %s" ids)
  (do-applescript (concat "\
tell application \"iTunes\"\r\
    set fixed indexing to true\r"
			  (itunes-reset-pl pl) "\
	set pl to user playlist \"" pl "\"\r\
	repeat with i in " (applescript-write ids) "\r\
		duplicate (first track of library playlist 1 whose database ID is i) to pl\r\
	end repeat\r\
end tell")))

(defun itunes-reset-pl (pl)
  (concat
   "    if ({\"" pl "\"} is in name of (user playlists)) then\r\
        delete user playlist \"" pl "\"\r\
    end if\r\
    make new user playlist with properties { name: \"" pl "\" }\r"))

(defun itunes-track (&optional name attr)
  (applescript-list
   (do-applescript (concat "tell application \"iTunes\" to get "
                           (or attr "name") " of "
                           (or name "current track")))))

(defun itunes-playlist (&optional name attr)
  (applescript-list
   (do-applescript (concat "tell application \"iTunes\" to get "
                           (or attr "name") " of every track of "
                           (or name "current playlist")))))

(defun itunes-changev (n)
  (do-applescript (format
                   "tell application \"iTunes\" to set sound volume to (sound volume) + %d"
                   n)))

(defsubst assocd (k l &optional def)
  (let ((res (assoc k l)))
    (if res (cdr res) def)))

(defun applescript-list (str)
  "Read in an applescript expression, translating into a lisp
sexp."
  (split-string (substring str 0 -1) ", " t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;_ - features that require XML parsing (which is painfully slow!).

(when (and (boundp 'itunes-external-playlist)
	   (eq itunes-external-playlist 'xml))
  (require 'xml)
  (require 'cl)

  (defun my-parse-xml (beg end)
    (interactive "r")
    (when (< end beg)
      (let ((tmp beg))
        (setq beg end)
        (setq end tmp)))
    (labels ((cleanup (node)
                      (cond
                       ((not node) nil)
                       ((stringp node)
                        (if (string-match "^[ \t\r\n\f]+$" node)
                            nil
                          (substring-no-properties node)))
                       ((listp node)
                        (destructuring-bind (name attrs &rest rest) node
                          (cons name (mapcan (lambda (x)
                                               (let ((xx (cleanup x)))
                                                 (if xx (list xx) nil)))
                                             rest))))
                       (t (list 'unknown (type-of node) node)))))
      (let ((res (cleanup (car (xml-parse-region beg end)))))
        res)))

  (defun parse-plist (beg end)
    (labels ((parse-thing (thing)
                          (ecase (car thing)
                            (dict (parse-kvs (cdr thing)))
                            ((key string) (cadr thing))
                            (integer (car (read-from-string (cadr thing))))
                            (plist (parse-thing (cadr thing)))
                            (t (cons 'unknown thing))))
             (parse-kvs (kvs)
                        (let ((res nil))
                          (while kvs
                            (let ((k (car kvs))
                                  (v (cadr kvs)))
                              (setq kvs (cddr kvs))
                              (push (cons (parse-thing k) (parse-thing v))
                                    res)))
                          (nreverse res))))
      (parse-thing (my-parse-xml beg end))))

  (defvar itunes-library-file "~/Music/iTunes/iTunes Music Library.xml")
  (defvar itunes-library nil)
  (defun itunes-cleanup ()
    (dolist (x (itunes-song-list))
      (setf (car x) (string-to-number (car x)))
      (dolist (y (cdr x))
        (setf (car y)
              (assocd (car y) '(("Track ID" . id)
                                ("Name" . title)
                                ("Artist" . artist)
                                ("Album" . album)
                                ("Track Number" . track)
                                ("Location" . file))
                      (car y))))))

  (defun itunes-load-library ()
    (interactive)
    (with-temp-buffer
      (insert-file-contents itunes-library-file)
      (setq itunes-library (parse-plist (point-min) (point-max)))
      (itunes-cleanup)))

  (defun itunes-song-list ()
    (cdr (nth 4 itunes-library)))

  (defun itunes-library-playlist ()
    (cdr (assoc "Library" (nth 5 itunes-library))))

  (defun itunes-search (v)
    (let ((lib (itunes-song-list))
          (ids nil)
          (nsongs 0))
      (dolist (x lib)
        (let ((id (car x))
              (kvs (cdr x)))
          (incf nsongs)
          (if (rassoc v kvs)
              (push id ids))))
      (message "(%s) %d songs." ids nsongs)
      ids))

  ;; (defun itunes-select (v)
  ;;   (interactive "MMatch: ")
  ;;   (itunes-update-playlist "From Emacs 2" (itunes-search v)))

  (defun itunes-track-number (n)
    (let ((lib (itunes-song-list)))
      (cdr (assoc (format "%d" n) lib))))
  ) ;; End XML-based code

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;_ - MySQL-based alternative to itunes' playlist selection
(when (and (boundp 'itunes-external-playlist)
	   (eq itunes-external-playlist 'mysql))

  (defvar itunes-sql-db "music.music")
  (defvar itunes-sql-playlist-file "~/.emacs.d/playlist.el")
  (defvar itunes-db-proc nil)
  (defvar itunes-sql-user "root")
  (defvar itunes-sql-passwd "foo9bar")
  (defvar itunes-query-history nil)

  (defun itunes-db-send (query)
    (unless (and itunes-db-proc
                 (processp itunes-db-proc)
                 (eq (process-status itunes-db-proc) 'run))
      (let ((process-connection-type t))
        (setq itunes-db-proc (start-process "iTunes DB" nil
                                            "/usr/local/mysql/bin/mysql"
                                            "-u" itunes-sql-user
                                            (concat "-p" itunes-sql-passwd)
                                            "-h" "localhost"
                                            "-B")))
      (set-process-filter
       itunes-db-proc
       (lambda (proc str)
         (if (string-match "^select.*;\r?\n?$" str)
             (message "ignoring cursed echo")
	   (itunes-update-playlist "Emacs SQL" (cdr (split-string str)))))))
    (message "itunes query: %s" query)
    (process-send-string itunes-db-proc query))

  (defun itunes-expand-query (str)
    (dolist (kv '(("'[tn]\\>" "name")
                  ("'a\\>" "artist")
                  ("'[Al]\\>" "album")
                  ("'[Tr]\\>" "track")
                  ("'c\\>" "composer")
                  ("'y\\>" "year")
                  ("~" "rlike")))
      (setq str (replace-regexp-in-string (car kv) (cadr kv) str)))
    str)

;;;###autoload
  (defun itunes-select (q)
    "Select tracks using a raw SQL query."
    (interactive
     (list (read-from-minibuffer "Query: " nil nil nil itunes-query-history)))
    (itunes-db-send (concat "select id from " itunes-sql-db " where "
                            (itunes-expand-query q)
                            ";\n")))

;;;###autoload
  (defun itunes-name-last-query (name)
    "Create a name for the last SQL query you executed."
    (interactive "sName: ")
    (unless itunes-query-history
      (error "No last query."))
    (let ((res (assoc name itunes-presets))
          (q (car itunes-query-history)))
      (if res
          (setf (cdr res) q)
	(progn
	  (push (cons name q) itunes-presets)
	  (itunes-update-playlists)))))

;;;###autoload
  (defun itunes-update-db ()
    (interactive)
    (shell-command
     (concat (expand-file-name "~/src/perl/itunes-export.pl") " &")))

  (defun itunes-update-playlists ()
    (with-temp-buffer
      (prin1 itunes-presets (current-buffer))
      (write-region (point-min) (point-max) itunes-sql-playlist-file)))

  (defvar itunes-presets
    (with-temp-buffer
      (insert-file-contents itunes-sql-playlist-file)
      (goto-char (point-min))
      (read (current-buffer)))
    "* Some predefined queries.")

  (defun itunes-playlist-preset (s)
    "Choose a playlist based on a previously-named query."
    (interactive (list (completing-read "Playlist: " itunes-presets)))
    (itunes-playlist-sql s)
    (itunes-playpause "Emacs SQL"))
  ) ;;; end SQL-based playlists

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; iTunes "tell me what's playing" action

(defvar itunes-tracker nil)
(defvar itunes-current-track nil)

(defun itunes-toggle-tracker ()
  (interactive)
  (cond
   (itunes-tracker
    (message "Stopping tracker")
    (cancel-timer itunes-tracker)
    (setq itunes-tracker nil))
   (t
    (message "Starting tracker")
    (setq itunes-tracker
          (run-with-timer 20 20
                          (lambda () 
                            (let ((res (itunes-get-current)))
                              (unless (equal res itunes-current-track)
                                (setq itunes-current-track res)
                                (message "%s, %s (%s)" (second res)
                                         (first res) (third res))))))))))

(provide 'itunes)
;;; itunes.el ends here
