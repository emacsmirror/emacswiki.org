;;; rmail-extras.el --- remote inboxes for RMAIL, extend "time" with multiple mail flags

;; This file is *NOT* part of GNU emacs
;;
;; Copyright (C) 1996 Trey Jackson
;; Author:       Trey Jackson <trey@cs.berkeley.edu>
;; Maintainer:   Trey Jackson <trey@cs.berkeley.edu>
;; Created:      Aug 13 1996
;; Version:      1.1 1996/08/13
;; Keywords:     mail, time

;; COPYRIGHT NOTICE
;;
;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2 of the License, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with GNU Emacs.  If you did not, write to the Free Software Foundation,
;; Inc., 675 Mass Ave., Cambridge, MA 02139, USA.


;; Installation:

;; Put this file on your emacs-lisp load path, byte-compile it for efficiency.
;;
;; For a truly minimal installation,
;; put the following in your ~/.emacs startup file:
;;
;; (require 'rmail-extras)

;;{{{ Usage documentation

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Usage documentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General description of what rmail-extras does:
;;
;; rmail-extras.el modifies the behavior of RMAIL to allow reading mail from
;; remote inboxes (using ange-ftp).  It was already possible to read from
;; multiple inboxes (most people I talk to didn't know that).
;;
;; rmail-extras.el also modifies the "time" package to enable raising
;; flags for each individual inbox that contain mail.  Previously it
;; was only possible to display a flag indicating mail had arrived in
;; just one given inbox.
;;
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Changes to behavior of RMAIL:
;;
;; Let us illustrate the above by an example:
;; 
;; Imagine you are reading mail on some account with the user name `trey'.
;; You also have an account with the user name `sam', and you'd like to read
;; mail from both accounts while only logged into the `trey' account.  Sure,
;; you could take the easy way out and use a .forward file, but it's not as
;; hip.
;;
;; You acquire this package, and put the line
;;
;;   (require 'rmail-extras)
;;
;; in your .emacs.  Now what?
;; All you do is change the variable 'rmail-inbox-list to contain the file
;; names of your two inboxes: 
;;
;;   (setq rmail-inbox-list '("/usr/spool/mail/trey" 
;;                            "/sam@comp1.berkeley.edu:/usr/spool/mail/sam"))
;;
;; And you're up and running.
;; One caveat, the above line must be evaluated *before* running RMAIL.  If
;; for some reason you want/need to run RMAIL before the above line, use
;; this solution instead: 
;;
;;   (rmail-update-inbox-list '("/usr/spool/mail/trey" 
;;                              "/sam@comp1.berkeley.edu:/usr/spool/mail/sam"))
;;
;; Now when you run rmail emacs will prompt you for a password for the `sam'
;; account (ange-ftp needs it to log in and get access to the file).
;; Luckily, ange-ftp is smart enough to keep track of the password so you
;; only have to enter it once per emacs session.
;; 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Changes made to "time" package:
;;
;; What is the "time" package?
;; To quote the documentation:
;;
;;   Facilities to display current time/date and a new-mail indicator
;;   in the Emacs mode line.  The single entry point is `display-time'.
;;
;; 
;; How does rmail-extras.el modify the "time" package?
;; 
;; The changes are redefinitions of:
;;   o display-time-filter           (function)
;;   o display-time-string-forms     (variable)
;;   o display-time-file-nonempty-p  (function)
;;
;; To use this package, simply run the function 'display-time:
;;
;;   M-x display-time     ; by hand
;;         or
;;   (display-time)       ; in .emacs
;;
;; This results in changing the mode-line to display the current time and a
;; flag indicating new mail has arrived.  You can modify this display one of
;; two ways:
;;
;;   1) set options
;;   2) redefine the variable 'display-time-string-forms
;;
;; The first way is easier to understand, the second allows you real
;; control over the format.
;; 
;; The options you have available to change are:
;; 
;; o display-time-display-time         - non-nil <==> time is displayed
;; o display-time-24hr-format          - non-nil <==> time displayed in 24hr format
;; o display-time-am/pm                - non-nil <==> "am" or "pm" in 12hr mode
;; o display-time-day-and-date         - non-nil <==> day and date are displayed
;; o display-time-display-mail-flag    - non-nil <==> new-mail flag(s) displayed
;; o display-time-use-frame-title-bar  - non-nil <==> display on frame title bar
;; o show-inbox-specific-strings       - non-nil <==> new-mail flag for each inbox
;; o beep-for-mail                     - non-nil <==> beep when new mail arrives
;; o display-time-interval             - seconds between updates of time in mode line
;;
;; If you wish to redefine the variable 'display-time-string-forms you should
;; read the documentation on the variable (C-h v display-time-string-forms).
;; rmail-extras.el doesn't change how it functions, but it adds a few new
;; keywords involving new mail:
;;
;;   o mail                     
;;        this is `t' when you have mail in at least one inbox
;;
;;   o mail1, mail2, ..., mailn
;;        each one is set to `t' when the corresponding inbox has mail
;;        for example, if 'rmail-inbox-list is set to
;;            ("/usr/spool/mail/trey" "/usr/spool/mail/sam")
;;        and you have mail in `trey', but not in `sam'
;;        then `mail1' will be `t' and `mail2' will be `nil'
;;        order is what counts here.
;;
;;   o mail-flag-list
;;        this is just a list of all the mail1, mail2, .., mailn values
;;        in the above example, `mail-flag-list' would be
;;            (t nil)
;;        again, order is what counts.
;;
;; In the example above, by default, the mode line would look like:
;; 
;;     8:05 MAIL in (trey)
;;
;; By setting 'show-inbox-specific-strings to nil it would look like:
;; 
;;     8:05 MAIL
;;
;; By setting 'yes-mail-string-list to ("YERT" "--Sam--") you get:
;; 
;;     8:05 MAIL in (YERT)
;;
;; Having mail in both inboxes results in:
;; 
;;     8:05 MAIL in (YERT, --Sam--)
;;
;; You get the idea.
;;
;; All changes to the variables are incorporated dynamically.
;; Use the function 'display-time-update to change the display.
;;
;;
;; By default, the strings used in 'yes-mail-string-list are created from the
;; list of inboxes.  An local inbox results in a flag that is the name of the
;; file without the directory.  A remote inbox results in a flag that is the
;; name of the machine (the reasoning is that many people have the same user
;; name on different accounts).  For example, if 'rmail-inbox-list is:
;;
;;   ("/usr/spool/mail/trey" "/sam@comp1.berkeley:/usr/mail/sam" "/dev/null")
;;
;; then 'yes-mail-string-list will default to:
;;
;;   ("trey" "comp1.berkeley" "null")
;;
;; The 'no-mail-string-list defaults to a list of null strings ("").
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Quirks/Problems:
;; 
;; o it doesn't beep each time new mail arrives, only when you get the first
;;   piece of unread mail
;;
;; o should work with Emacs up to 19.33 (starting at ???)
;;
;; NOTE: This obsoletes frame-status-line.el  (if you have it)
;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; To do (maybe):
;;
;; o add option to use filename on remote inboxes
;; o get RMAIL to send mail from different inboxes to different RMAIL files
;; o once the above is working, get mail flags for the different RMAIL files
;;

;;}}}

;;{{{ Code 

;;{{{ requirements

;; what do we need?
(require 'ange-ftp)
(require 'cl)

;;}}}

;;{{{ changes relevant to RMAIL

;; redefinition
;;{{{ rmail-insert-inbox-text

;;; oops, code is actually from 19.32, make it work with earlier versions
(setq rmail-pop-password nil)


;;; this is required to obtain mail from the multiple,
;;; possibly remote, inboxes properly
(require 'rmail)
(defun rmail-insert-inbox-text (files renamep)
  (let (file tofile delete-files movemail popmail)
    (while files
      (setq file (file-truename
                  (expand-file-name (substitute-in-file-name (car files))))
            tofile (expand-file-name
                    ;; Generate name to move to from inbox name,
                    ;; in case of multiple inboxes that need moving.
                    (concat ".newmail-" (file-name-nondirectory file))
                    ;; Use the directory of this rmail file
                    ;; because it's a nuisance to use the homedir
                    ;; if that is on a full disk and this rmail
                    ;; file isn't.
                    (file-name-directory
                     (expand-file-name buffer-file-name))))

      ;; inboxes w/the same name mess things up
      (while (member tofile delete-files)
        (setq tofile (concat tofile "-")))

      ;; Always use movemail to rename the file,
      ;; since there can be mailboxes in various directories.
      (setq movemail t)

      (setq popmail (string-match "^po:" (file-name-nondirectory file)))
      (if popmail (setq file (file-name-nondirectory file)
                        renamep t))
      (if movemail
          (progn
            ;; On some systems, /usr/spool/mail/foo is a directory
            ;; and the actual inbox is /usr/spool/mail/foo/foo.
            (if (file-directory-p file)
                (setq file (expand-file-name (user-login-name)
                                             file)))))
      (cond (popmail
             (if (and rmail-pop-password-required (not rmail-pop-password))
                 (setq rmail-pop-password
                       (rmail-read-passwd
                        (format "Password for %s: "
                                (substring file (+ popmail 3))))))
             (if (eq system-type 'windows-nt)
                 ;; cannot have "po:" in file name
                 (setq tofile
                       (expand-file-name
                        (concat ".newmail-pop-" (substring file (+ popmail 3)))
                        (file-name-directory
                         (expand-file-name buffer-file-name)))))
             (message "Getting mail from post office ..."))
            ((and (file-exists-p tofile)
                  (/= 0 (nth 7 (file-attributes tofile))))
             (message "Getting mail from %s..." tofile))
            ((and (file-exists-p file)
                  (/= 0 (nth 7 (file-attributes file))))
             (message "Getting mail from %s..." file)))
      ;; Set TOFILE if have not already done so, and
      ;; rename or copy the file FILE to TOFILE if and as appropriate.
      (cond ((not renamep)
             (setq tofile file))
            ((or (file-exists-p tofile) (and (not popmail)
                                             (not (file-exists-p file))))
             nil)
            ((or (and (not movemail) (not popmail))
                 (ftp-file-p file))
             ;; Try copying.  If that fails (perhaps no space),
             ;; rename instead.
             (condition-case nil
                 (copy-file file tofile nil)
               (error
                ;; Third arg is t so we can replace existing file TOFILE.
                (rename-file file tofile t)))
             ;; Make the real inbox file empty.
             ;; Leaving it deleted could cause lossage
             ;; because mailers often won't create the file.
             (condition-case ()
                 (write-region (point) (point) file)
               (file-error nil)))
            (t
             (let ((errors nil))
               (unwind-protect
                   (save-excursion
                     (setq errors (generate-new-buffer " *rmail loss*"))
                     (buffer-disable-undo errors)
                     (if rmail-pop-password
                         (call-process
                          (or rmail-movemail-program
                              (expand-file-name "movemail" exec-directory))
                          nil errors nil file tofile rmail-pop-password)
                       (call-process
                        (or rmail-movemail-program
                            (expand-file-name "movemail" exec-directory))
                        nil errors nil file tofile))
                     (if (not (buffer-modified-p errors))
                         ;; No output => movemail won
                         nil
                       (set-buffer errors)
                       (subst-char-in-region (point-min) (point-max)
                                             ?\n ?\  )
                       (goto-char (point-max))
                       (skip-chars-backward " \t")
                       (delete-region (point) (point-max))
                       (goto-char (point-min))
                       (if (looking-at "movemail: ")
                           (delete-region (point-min) (match-end 0)))
                       (beep t)
                       (message "movemail: %s"
                                (buffer-substring (point-min)
                                                  (point-max)))
;;                       (sit-for 3)
                       nil))
                 (if errors (kill-buffer errors))))))
      ;; At this point, TOFILE contains the name to read:
      ;; Either the alternate name (if we renamed)
      ;; or the actual inbox (if not renaming).
      (if (file-exists-p tofile)
          (let (size)
            (goto-char (point-max))
            (setq size (nth 1 (insert-file-contents tofile)))
            (goto-char (point-max))
            (or (= (preceding-char) ?\n)
                (zerop size)
                (insert ?\n))

            ;; if reading mail remotely
            ;; need to save RMAIL file securely,
            ;; and if it works, then delete the tofile
            (if (ftp-file-p file)
                ;; if ange-ftp
                (if (condition-case ()
                        (save-buffer 1)
                      (file-error t))
                    ;; hmmm... error saving
                    (error (concat "can't save " (buffer-name)))
                  (delete-file tofile))
              
              ;;else normal file
              (setq delete-files (cons tofile delete-files)))))

      (message "")
      (setq files (cdr files)))
    delete-files))

;;}}}

;; new function
;;{{{ rmail-update-inbox-list

;;###autoload
(defun rmail-update-inbox-list (&optional ilist)
  (interactive)
  (setq ilist (or ilist
                  rmail-inbox-list))
  (let ((rmail-buff (get-file-buffer rmail-file-name)))
    (if (not rmail-buff)
        (setq rmail-primary-inbox-list
              (setq rmail-inbox-list ilist))
      (set-buffer rmail-buff)
      (setq rmail-inbox-list
            (setq rmail-primary-inbox-list
                  (setq rmail-inbox-list
                        (or ilist
                            (list (or (getenv "MAIL")
                                      (concat rmail-spool-directory
                                              (user-login-name)))))))))))

;;}}}

;; not defvar'ed before
(defvar rmail-inbox-list nil
  "List of inboxes to check for mail.")

;; call it to make sure the variable is set in RMAIL buffer
(rmail-update-inbox-list)      

;;}}}

;;{{{ changes relevant to time.el

;;{{{ variables

(defvar rmail-extras-modify-time t
  "Non-nil value indicates that `rmail-extras' will modify the
behavior of the `time' package.  Be sure to set this to nil *before*
loading `rmail-extras' if you wish to keep `time' pure.")

(defvar yes-mail-string-list nil)
(defvar no-mail-string-list nil)

(defvar display-time-use-frame-title-bar nil
  "Non-nil value indicates to put time and mail status on frame title bars.")

(defvar display-time-display-time t
  "Non-nil value indicates to display time.")

(defvar display-time-display-mail-flag t
  "Non-nil value indicates to display a new-mail flag.")

(defvar display-time-am/pm nil
  "Non-nil value indicates to display `am' or `pm' when not in 24 hour format.")

(defvar show-inbox-specific-strings t)

(defvar display-time-frame-names nil)

(defvar beep-for-mail nil
  "*If non-nil, beep when mail arrives")
(defvar beeped-already nil)

;;}}}

;;!!;;!!;;!!;;!!;;!!;;!!;;!!;;!!;;!!;;!!;;!!;;!!;;!!;;!!;;!!;;!!;;!!;;!!;;!!;
;; make sure that the functions are redefined after the package is loaded. ;;
;;!!;;!!;;!!;;!!;;!!;;!!;;!!;;!!;;!!;;!!;;!!;;!!;;!!;;!!;;!!;;!!;;!!;;!!;;!!;

(eval-after-load
 "time"
 '(if rmail-extras-modify-time
    (progn
                 
;;{{{        display-time-filter

;; this is to allow use of the display-time-string-forms
;; now inside of the form
;; if rmail-inbox-list is a list (and not a single string)
;; then mail is bound to a list of boolean values specifying
;; which of the inboxes have mail

;; ignore arguments, but keep them there to work with 19.30 and before
(defun display-time-filter (&optional ignore1 ignore2)
  "no real docs on this yet"
  (let* ((now (current-time))
         (time (current-time-string now))
         (load (condition-case ()
                   (if (zerop (car (load-average))) ""
                     (let ((str (format " %03d" (car (load-average)))))
                       (concat (substring str 0 -2) "." (substring str -2))))
                 (error "")))
         (yes-mail-string-list yes-mail-string-list) ; don't clobber original
         (no-mail-string-list no-mail-string-list) ; don't clobber original

         ;;new   make mail-spool-file-list a list
         (mail-spool-file (or rmail-inbox-list
                              (getenv "MAIL")
                              (concat rmail-spool-directory
                                      (user-login-name))))
         (mail-spool-file-list (if (listp mail-spool-file);; it's a list for sure now
                                   mail-spool-file
                                 (list mail-spool-file)))
         ;;orig         (mail-spool-file (or rmail-inbox-list
         ;;                              (getenv "MAIL")
         ;;                              (concat rmail-spool-directory
         ;;                                      (user-login-name))))
         (mail-flag-list  (if (and (listp mail-spool-file-list)
                                   (< 0 (length mail-spool-file-list)))
                              (progn
                                ;; check list of (yes/no)-mail-strings
                                (if (/= (length mail-spool-file-list) (length yes-mail-string-list))
                                    (setq yes-mail-string-list (make-default-yes-mail-strings
                                                                mail-spool-file-list)))
                                (if (/= (length mail-spool-file-list) (length no-mail-string-list))
                                    (setq no-mail-string-list (make-default-no-mail-strings
                                                               mail-spool-file-list)))
                     
                                (let (mail-flag-list
                                      server-down-time-list
                                      mfile
                                      dtime)
                                  (while mail-spool-file-list
                                    (setq mfile (car mail-spool-file-list))
                                    (setq dtime (car display-time-server-down-time))
                                    (setq mail-flag-list
                                          (cons 
                                           (and (stringp mfile)
                                                (or (null dtime)
                                                    ;; If have been down for 20 min, try again.
                                                    (> (- (nth 1 (current-time))
                                                          dtime)
                                                       1200))
                                                (let ((start-time (current-time)))
                                                  (prog1
                                                      (display-time-file-nonempty-p mfile)
                                                    (if (> (- (nth 1 (current-time)) (nth 1 start-time))
                                                           20)
                                                        ;; Record that mail file is not accessible.
                                                        (setq server-down-time-list
                                                              (cons (nth 1 (current-time))
                                                                    server-down-time-list))
                                                      ;; Record that mail file is accessible.
                                                      (setq server-down-time-list
                                                            (cons nil server-down-time-list))))))
                                           mail-flag-list))
                                    (setq mail-spool-file-list (cdr mail-spool-file-list))
                                    (setq display-time-server-down-time (cdr display-time-server-down-time)))
                                  (setq display-time-server-down-time (reverse server-down-time-list))
                                  (reverse mail-flag-list)))))

         (mail (eval (cons 'or mail-flag-list)))

         (24-hours (substring time 11 13))
         (hour (string-to-int 24-hours))
         (12-hours (int-to-string (1+ (% (+ hour 11) 12))))
         (am-pm (if (>= hour 12) "pm" "am"))
         (minutes (substring time 14 16))
         (seconds (substring time 17 19))
         (time-zone (car (cdr (current-time-zone now))))
         (day (substring time 8 10))
         (year (substring time 20 24))
         (monthname (substring time 4 7))
         (month
          (cdr
           (assoc
            monthname
            '(("Jan" . "1") ("Feb" . "2") ("Mar" . "3") ("Apr" . "4")
              ("May" . "5") ("Jun" . "6") ("Jul" . "7") ("Aug" . "8")
              ("Sep" . "9") ("Oct" . "10") ("Nov" . "11") ("Dec" . "12")))))
         (dayname (substring time 0 3)))

    ;; beep for mail
    (if (and beep-for-mail mail (not beeped-already))
        (progn
          (ding)
          (setq beeped-already t)))
    (and (not mail) (setq beeped-already nil))


    ;; use the display-time-string-forms to set display-time-string
    ;; wrap a let statement around it so that mail1, mail2, ..., mailn
    ;; variables are defined
    (eval
     `(let ,(display-time-form-let-vars mail-flag-list)
        (setq display-time-string
              (mapconcat 'eval display-time-string-forms ""))))

    ;; choice b/w frame and mode-line
    (if display-time-use-frame-title-bar
        (progn
          (display-time-update-frame-name display-time-string)
          (setq display-time-string nil))
      (display-time-update-frame-name ""))

    (sit-for 0)
    
    ;; This is inside the let binding, but we are not going to document
    ;; what variables are available.
    (run-hooks 'display-time-hook))
  
  )

;;}}}

;;{{{        display-time-string-forms

;; need to defvar and document this
(setq display-time-string-forms
      '((and display-time-display-time
             (concat "  "
                     (if display-time-day-and-date
                         (format "%s %s %s " dayname monthname day)
                       "")
                     (format "%s:%s%s"
                             (if display-time-24hr-format 24-hours 12-hours)
                             minutes
                             (if display-time-24hr-format "" (if 
                                                              display-time-am/pm 
                                                              am-pm "")))))
        (and display-time-display-mail-flag (if mail " MAIL " ""))
        ;; if more than one inbox, report which ones have mail...
        (if (and show-inbox-specific-strings (< 1 (length mail-flag-list)))
            (concat "in ("
                    (mapconcat 'identity
                               (delete ""
                                       (mapcar*
                                        (function (lambda (flag yup nope)
                                                    (if flag
                                                        yup
                                                      nope)))
                                        mail-flag-list
                                        yes-mail-string-list
                                        no-mail-string-list))
                               ", ")
                    ")"))))


;;}}}

;;{{{        helpers for making the form

(defun display-time-form-let-vars (val-list &optional count)
  (setq count (or count 1))
  (and val-list
       `((,(intern (concat "mail" (number-to-string count))) ,(car val-list))
         ,@(display-time-form-let-vars (cdr val-list) (+ 1 count)))))

;;}}}

;;{{{        display-time-file-nonempty-p

;; this was redefined because ange-ftp always returns a size -1 for remote files
(defun display-time-file-nonempty-p (file)
  (and (file-exists-p file)
       (if (ftp-file-p file)
           (let ((buf (generate-new-buffer "**"))
                 res)
             (set-buffer buf)
             (setq res
                   (/= 0 (nth 1 (insert-file-contents file nil 0 1))))
             (kill-buffer buf)
             res)
         (/= 0 (nth 7 (file-attributes (file-chase-links file)))))))

;; orig
;;(defun display-time-file-nonempty-p (file)
;;  (and (file-exists-p file)
;;       (< 0 (nth 7 (file-attributes (file-chase-links file))))))

;;}}}

;;{{{        display-time-update

(defun display-time-update ()
  (interactive)
  (display-time-filter)
  (sit-for 0))

;;}}}

;;{{{        display-time-update-frame-name

(defun display-time-update-frame-name (mail-flag)
  "Updates all the status line of all frames."
  (mapcar '(lambda (frame)
             (let ((orig-string (cdr (assq frame display-time-frame-names))))
               (if (not orig-string)
                   (progn
                     (setq orig-string (cdr (assq 'name (frame-parameters frame))))
                     (setq display-time-frame-names (cons
                                                     (cons frame orig-string)
                                                     display-time-frame-names))))
               (modify-frame-parameters
                frame
                (list (cons 'name
                            (concat
                             orig-string " "
                             mail-flag))))))
          (frame-list)))


;;}}}

)))  ; eval-after-load "time"



;;{{{ helper functions

(defun make-default-yes-mail-strings (inlist)
  "Generate default mail flags for a list of inboxes.
If the inbox is remote, the flag is the host machine.
If the inbox is local, the flag is the name of the inbox."
  (mapcar (function
           (lambda (inbox)
             (if (ftp-file-p inbox)
                 (car (ange-ftp-ftp-name inbox))
               (file-name-nondirectory inbox))))
          inlist))

(defun make-default-no-mail-strings (inlist)
  "Generate default nomail flags for a list of inboxes. (default \"\")"
  (mapcar '(lambda (inbox)
             "")
          inlist))

;;}}}

;;}}}

;;{{{ general helper functions

;; helper function
(defun ftp-file-p (filename)
  "Teturn true iff FILENAME specifies a remote file."
  (let ((f-list (ange-ftp-ftp-name filename)))
    (and f-list
         (< 0 (length (car f-list))))))

;;}}}

;;}}}

(provide 'rmail-extras)

;;; rmail-extras.el ends here
