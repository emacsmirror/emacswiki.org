;;; GLIMPSE.EL --- Interface to glimpse 

;; Copyright (C) 1997 Alan Shutko <ats@acm.org> 

;; Author: Alan Shutko <ats@acm.org> 
;; Maintainer: Alan Shutko <ats@acm.org> 
;; Created: Mon Jul  7 1997 
;; Version: $Revision: 1.2 $ 
;; Keywords: 

;; This program is free software; you can redistribute it and/or 
;; modify it under the terms of the GNU General Public License as 
;; published by the Free Software Foundation; either version 1, or (at 
;; your option) any later version.  ;; This program is distributed in 
;; the hope that it will be useful, but WITHOUT ANY WARRANTY; without 
;; even the implied warranty of MERCHANTABILITY or FITNESS FOR A 
;; PARTICULAR PURPOSE.  See the GNU General Public License for more 
;; details.  ;; A copy of the GNU General Public License can be 
;; obtained from this program's author (send electronic mail to Alan 
;; Shutko <ats@acm.org>) or from the Free Software Foundation, Inc., 
;; 675 Mass Ave, Cambridge, MA 02139, USA.  ;; 

;;; LCD Archive Entry: 
;;; glimpse|Alan Shutko|ats@acm.org 
;;; |Interface to glimpse 
;;; |$Date: 1997/07/07 21:21:38 $|$Revision: 1.2 $| 

;;; Commentary: 

;; These are some helper functions to use glimpse from within Emacs. 
;; They contain: 

;; * glimpse-dired: Conduct a glimpse search and display the files 
;; found in a dired buffer. 

;; * glimpse-find-file: Conduct a glimpse search and open one of the 
;; files found 

;; * glimpse and glimpse-in-files:  Run glimpse like grep. 

;; `glimpse' and `glimpse-in-files' are from code posted by Peter 
;; Breton <pbreton@i-kinetics.com> and are included here for 
;; completeness. 

;; Several variables are of potential interest.  `glimpse-switches' 
;; are the default switches on the command line for `glimpse' and 
;; `glimpse-in-files'.  `glimpse-dired-switches' are the switches used 
;; for `glimpse-find-file' and `glimpse-dired'.  The switches given 
;; are what I use... the code is probably not robust enough to handle 
;; other switches.  Let me know what you like to use and I'll make it 
;; work. 

;; This code is very beta.  The code works fine for me, but it doesn't 
;; handle all cases people might use.  Please let me know if you'd 
;; like it to do something else and I'll try to fix it.  Patches are 
;; also welcome. 

;;; Code: 

(require 'compile) 
(require 'dired) 

(defconst glimpse-version (substring "$Revision: 1.2 $" 11 -2) 
  "$Id: glimpse.el,v 1.2 1997/07/07 21:21:38 ats Exp $ 

Report bugs to: Alan Shutko <ats@acm.org>") 

(defvar glimpse-command "glimpse" 
  "*The glimpse executable program") 

(defvar glimpse-switches "-n -i -y" 
  "*Options to the glimpse command for M-x glimpse") 

(defvar glimpse-dired-switches "-lWyz" 
  "*Switches passed to glimpse by `glimpse-dired' 
Must tell glimpse to output a list of matching files.") 

(defun glimpse-list-hits (search) 
  "Returns a list of files found by glimpse." 
  (save-excursion 
    (let ((buffer (get-buffer-create " *glimpse work*")) 
          (list)) 
      (set-buffer buffer) 
      (erase-buffer) 
      (call-process "glimpse" nil buffer nil 
                    glimpse-dired-switches search) 
       (goto-char (point-min)) 
;      (next-line 1 ) 
      (while (re-search-forward "^.+$" nil t) 
        (setq list (append (list (match-string 0)) list))) 
      list))) 

(defun glimpse-dired (search) 
  "Search glimpse and display results in a dired buffer. 
Does a search using `glimpse-dired-switches'." 
  (interactive "sSearch for: ") 
  (dired (cons (concat "glimpse " glimpse-dired-switches " " search) 
               (glimpse-list-hits search)))) 

(defun glimpse-find-file (search) 
  "Search glimpse and open one of the found files. 
Useful when you know keywords in a file, but not the location." 
  (interactive "sSearch for: ") 
  (find-file (completing-read "Select file: " (glimpse-list-to-alist (glimpse-list-hits search))))) 

(defun glimpse-list-to-alist (list) 
  "Returns a dummy alist created by consing t and the members of the input list" 
  (if (not list) 
      nil 
    (let (alist) 
      (cons (cons (car list) t) 
            (glimpse-list-to-alist (cdr list)))))) 

(defun glimpse-cmdline() 
  "The command line used by the glimpse function" 
  (concat glimpse-command " " glimpse-switches " " )) 

(defvar glimpse-history-list nil 
  "The history list used by the glimpse command") 

;; Copped from the grep command 
;; Note that emacs 19.28 (?) and earlier hard-coded the use of /dev/null in 
;; the grep command 
(defun glimpse (command-args) 
  "Run glimpse, with user-specified args, and collect output in a buffer. 
While glimpse runs asynchronously, you can use the \\[next-error] command 
to find the text that glimpse hits refer to. 

This command uses a special history list for its arguments, so you can 
easily repeat a glimpse command." 
  (interactive 
   (list (read-from-minibuffer "Run glimpse (like this): " 
           (glimpse-cmdline) 
              nil nil 'glimpse-history-list))) 
  (compile-internal (concat command-args) 
                    "No more glimpse hits" "glimpse" 
                    ;; Give it a simpler regexp to match. 
                    nil grep-regexp-alist)) 

;; Shell Quotes on both file pattern and search pattern 
(defun glimpse-in-files (file-pattern needle) 
  "Run glimpse only on the files which match pattern" 
  (interactive "sFile Pattern: \nsGlimpse: ") 
  (let ((glimpse-cmdline 
        (concat glimpse-command 
                " -F '" file-pattern "' " 
                glimpse-switches " '" needle "' "))) 
     (glimpse glimpse-cmdline))) 

(provide 'glimpse) 
;;; GLIMPSE.EL ends here
