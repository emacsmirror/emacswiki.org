;;; cricket.el --- Get live cricket scores and news updates from inside emacs.
;;
;; Copyright (C) 2002 by Free Software Foundation, Inc.
;;
;; Author:  Anand B Pillai <anand_pillai@delmia.com>
;;
;; Keywords: tools, sports, cricket, live, news
;; Time-stamp: <Mon Nov 18 19:08:09 2002>
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; Commentary 
;; This file consists of functions for the cricket enthusiast using
;; emacs. There is a function which reports live cricket scores and
;; there is another one which retrieves cricket news headlines from
;; around the world using www servers. 

;; The latest version of this file should be available as
;;
;; <URL: http://members.fortunecity.com/anandpillai/emacs/cricket.el.html>
;;
;; Functions
;; M-x cricket-score - latest live cricket score 
;; M-x cricket-news   - cricket news updates
;; This needs the latest version of emacs w3.
;; Modified Mon Nov 25 14:34:08 2002 - Fixed problem with string
;;                                     and periodic updation.
;; Wed Dec 04 14:45:52 2002 - Added w3 to url require list
;; Mon Mar 03 16:08:54 2003 - Added new team abbrevs.


(require 'cl)
(require 'custom)
(require 'url)
(require 'w3)


(defgroup cricket nil
  "Real-time cricket score reporting and news updates"
  :group 'applications
  :prefix "cricket-")

(defconst cricket-news-servers
  '(("Cricket.org" . ("http://www-usa.cricket.org/index.html" "Automatically generated" "<table" "</table>"))
    ("CricLive" . ("http://www.criclive.com" "Cricket Headlines" "<!--Start News--" "<!--End News-->"))
	("Cricket365" . ("http://www.cricket365.com" "BREAKING NEWS" "<table" "</table>"))))

(defcustom cricket-country-abbrevs '(("Aus" . "Australia")
                                     ("SA"  . "South Africa")
                                     ("Ind" . "India")
                                     ("SL"  . "Sri Lanka")
                                     ("Pak" . "Pakistan")
                                     ("NZ"  . "New Zealand")
                                     ("Eng" . "England")
                                     ("Zim" . "Zimbabwe")
                                     ("WI"  . "West Indies")
						   ("Ban" . "Bangladesh")
						   ("Can" . "Canada")
						   ("Ken" . "Kenya")
						   ("Nam" . "Namibia")
						   ("Holl" .  "Netherlands")
						   ("Neth" . "Netherlands"))
  "Abbreviations for various countries. This is an association list,
so every element should be a pair of the form (Abbrev . Name), where Abbrev
is a string which should be replaced by the Name"
  :type 'sexp
  :group 'cricket)

(defcustom cricket-live-server
  "http://livechat.rediff.com/sports/score/score.txt"
  "The url used to obtain live cricket scores"
  :type 'string)

(defcustom cricket-update-scores 300
  "Time interval to update the cricket function"
  :type 'integer
  :group 'cricket)

(defconst cricket-frame-name "Match")

;; strip html tags from a buffer
;; Courtesy, small-functions.el, Steve Kemp <skx@tardis.ed.ac.uk>

(defun chk-insert-string( arg )
  (if (and (not (eq  arg nil)) (not (equal arg "")))
	  (insert-string arg)))

(defun cricket-news()
  "Get cricket news from around cricket servers and display it in netscape"
  (interactive)
  (setq url-automatic-caching nil)
  (let* ((url-show-status)
         (news-file (concat (getenv "TMPDIR") "/" "cricket.html"))
         (buffer1 "*criclines*")
         (buffer2 "*Cricket News*")
         (server (completing-read "Server: " cricket-news-servers nil t "CricLive"))
         (cric-server-info (cdr (assoc server cricket-news-servers)))
         (cric-url (car cric-server-info))
         (cric-string1 (cadr cric-server-info))
         (cric-string2 (caddr cric-server-info))
         (cric-string3 (cadddr cric-server-info)))

    (switch-to-buffer (get-buffer-create buffer1))
    (url-insert-file-contents cric-url)
    (set-buffer-modified-p nil)
    (goto-char (point-min))
    (if (not (equal cric-string1 "")) (re-search-forward cric-string1 nil t))
    (setq news (buffer-substring
                (re-search-forward cric-string2 nil t) 
                (re-search-forward cric-string3 nil t)))
    (switch-to-buffer (get-buffer-create buffer2))
    (erase-buffer)
    (kill-buffer buffer1)
	(chk-insert-string "<html>\n")
	(chk-insert-string "<head>\n")
	(chk-insert-string "<title>Emacs Cricket News</title>\n")
	(chk-insert-string "<H2>\n")
    (chk-insert-string " **** Cricket Headlines ****\n")
	(chk-insert-string "</H2>\n")
	(chk-insert-string "</head>\n")
	(chk-insert-string "<body bgcolor=\"lightblue\">\n")
	(chk-insert-string "<BR>\n")
	(chk-insert-string "<BR>\n")
	(chk-insert-string "<BR>\n")
	(chk-insert-string "<strong>\n")
	(chk-insert-string "<font size=3 face=\"arial,verdana\" >\n")
    (chk-insert-string "<p")
    (chk-insert-string news)
    (if (equal server "CricLive")
        (progn
          (goto-char (point-min))
          (replace-string "
" "\n")
          (goto-char (point-min))
          (replace-string "
" "\n")
          (goto-char (point-min))))
	(if (equal server "Cricket.org")
        (progn
          (goto-char (point-min))
		  (replace-string "/link_to_database" "http://www-usa.cricket.org/link_to_database")))
	(chk-insert-string "</font>\n")
	(chk-insert-string "</p>\n")
	(chk-insert-string "</strong>\n")
	(chk-insert-string "</body>\n")
	(chk-insert-string "</html>\n")

    ;;(trim-trailing-blanks)
    (goto-char (point-min))
    (indent-region (point-min) (point-max) nil)
    (goto-char (point-min))
  (set-buffer-file-coding-system 'utf-8)
  (write-file news-file)
  (kill-buffer (current-buffer))
  (require 'browse-url)
  (browse-url-netscape (concat "file://" news-file))))

(defun cricket-score ()
  (interactive)
  (rediff-cricket-score))

(defun rediff-cricket-score()
  (let ((bname "*Cricket*")
        (frame-name (frame-parameter (selected-frame) 'name))
        (match-beginning)
        (match-end)
        (match-data)
        (cricket-match-vars (make-hash-table :test 'equal))
        (mom-abbrev "MOM")
        (mos-abbrev "MOS")
        (mom-string "Man of the Match")
        (mos-string "Man of the Series")
        (kelvinator-string "'The Coolest One'"))
    (if (string-match cricket-frame-name frame-name)
        (switch-to-buffer (get-buffer-create bname))
      (switch-to-buffer-other-frame (get-buffer-create bname)))
    (erase-buffer)
    ;; the cricket scores should not be old!
    (setq url-automatic-caching nil)
    ;; perform task in the background
    (setq url-be-asynchronous t)
    (url-insert-file-contents cricket-live-server)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (looking-at "\\(\\w+\\)=\\(.*\\)")
          (setf (gethash (buffer-substring-no-properties 
                          (match-beginning 1) (match-end 1)) 
                         cricket-match-vars)
                (buffer-substring-no-properties 
                 (match-beginning 2) (match-end 2))))
        (forward-line 1)))
    (erase-buffer)
    (setq match-info (gethash "l1" cricket-match-vars))
    (setq score-line (gethash "l2" cricket-match-vars))
    (setq score-line (replace-regexp-in-string ")" " overs)" score-line))
    (setq report (gethash "message" cricket-match-vars))
    (setq team-info match-info)
    (setq report
          (replace-regexp-in-string "\\*\\*" "\n"
                                    (replace-regexp-in-string mos-abbrev mos-string
                                                              (replace-regexp-in-string "\\." ".\n"
                                                                                        (replace-regexp-in-string mom-abbrev mom-string
                                                                                                                  (replace-regexp-in-string kelvinator-string " " report))))))
    (setq report-list (split-string report "/ "))
    (setq mom-report (car report-list))
    (setq mos-report (cadr report-list))
    (chk-insert-string "-----------------------------------------------------------------------\n")
	(chk-insert-string match-info)
	(chk-insert-string "\n")
    (chk-insert-string score-line)
	(chk-insert-string "\n")
    (chk-insert-string "-----------------------------------------------------------------------\n")
    (chk-insert-string "\nReport\n\n")
    (chk-insert-string mom-report)
	(chk-insert-string "\n")
    (chk-insert-string mos-report)
	(chk-insert-string "\n")
    (chk-insert-string "-----------------------------------------------------------------------\n")
    (goto-char (point-min))
    (loop for country in cricket-country-abbrevs do 
          (save-excursion 
            (replace-string (car country) (cdr country) t)))
    (loop for country in cricket-country-abbrevs do
          (progn
            (setq match-info (replace-regexp-in-string (car country) (cdr country) match-info))
            (setq score-line (replace-regexp-in-string (car country) (cdr country) score-line))
            (setq team-info (replace-regexp-in-string (car country) (cdr country) team-info))
            ))
    (message score-line)
    (set-frame-name (concat cricket-frame-name "---> " team-info ", Score---> " score-line))))

;; Periodic updation
(run-at-time t cricket-update-scores 'cricket-score))

(provide 'cricket)

;;; cricket.el ends here
