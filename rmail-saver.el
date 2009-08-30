;;; rmail-saver.el - An easy way of saving mail messages to files
;;;
;;; Copyright (C) 1993 David L. Sifry  sifry@sun57.pic.melco.co.jp
;;; Version 1.0
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 1, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; A copy of the GNU General Public License can be obtained from this
;;; program's author (send electronic mail to <sifry@sun57.pic.melco.co.jp>)
;;; or from the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA
;;; 02139, USA.
;;;
;;; This package does 2 things. It:
;;; 
;;; 1) parses your current message for a From: or Sender: line and
;;; intelligently creates a file name according to your specifications,
;;; i.e. it will save mail from andy@some.regular.path to a file
;;; called andy or a file called andy@some or a file called
;;; andy@some.regular or a file called andy@some.regular.path in a
;;; directory  that you specify.
;;;
;;; It will save in either rmail or regular mail (inbox) format.
;;;
;;; The address parser also understands uucp style bang paths and
;;; converts them to internet style paths when creating the file name.
;;; 
;;; 2) Includes functions to do this for all the undeleted messages in your
;;; RMAIL file and then will mark them for deletion.
;;; 
;;; To save the current message in a file, use
;;; M-x daves-rmail-save-this-message-babyl
;;; or
;;; M-x daves-rmail-save-this-message-inbox
;;; 
;;; which parses the message, then saves into either babyl (RMAIL) or
;;; inbox (regular mail) files in the directory saved-rmail-file-directory.
;;; 
;;; To save all files that are currently not marked for deletion, use
;;; M-x daves-rmail-save-messages-babyl
;;; or
;;; M-x daves-rmail-save-messages-inbox
;;;
;;; The files are saved under the directory saved-rmail-file-directory. 
;;; the default for this is ~/RMail/
;;;
;;; Now some more details to allow you to screw around with the
;;; filename creation scheme. There are 3 user-defined variables that
;;; control the 3 levels of filename length and content checking. They are:
;;; user-machine-domain-and-company-length (default value 15)
;;; user-machine-and-domain-length (default value 12) 
;;; user-and-machine-type-address-length (default value 7) 
;;;
;;; First of all, by making any of these variables nil, only the user
;;; name (the first part of the address) is returned as the file name.   
;;;
;;; Each is the maximum length of an incoming address part - These
;;; variables define where to truncate the mail address when making a
;;; file name. For addresses of the type "user@machine.domain.company"
;;; or longer, only the first 3 names are looked at (up to the company
;;; field). I'm no RFC 822 pro so I just called them user, machine,
;;; domain, and company. If someone's got good info on this, I'd be
;;; glad to change the names to the correct ones.
;;;
;;; user-machine-domain-and-company-length deals with addresses of the
;;; type "user@machine.domain.company". If the address is shorter than
;;; this length, then that address is returned as the file name.
;;; Otherwise, the other two levels of checking are done. To
;;; disable this level of checking but keep the other two, set
;;; user-machine-domain-and-company-length to 0.
;;;
;;; user-machine-and-domain-length deals with addresses of the
;;; type "user@machine.domain". If the address is shorter than this
;;; length, then the address is returned as the file name. Otherwise
;;; the last level of checking is done. To disable this level of
;;; checking but to keep the other two, set
;;; user-machine-and-domain-length to 0.
;;;
;;; user-and-machine-type-address-length deals with addresses of the
;;; type "user@machine". If the address is shorter than this length,
;;; then the address is returned as the file name. If the length of
;;; the address is greater than user-and-machine-type-address-length
;;; then the default file name, the user name is returned.
;;;
;;; If the mail is local (of the type "user") then it is made the file
;;; name. 
;;;
;;; If the variable daves-rmail-prompt-for-filename is non-nil, the
;;; functions in rmail-saver will give you a prompt allowing you to
;;; enter your own filename when saving, and the parsed filename
;;; becomes the default. If nil, rmail-saver works quietly, using only
;;; the parsed filenames when saving. 
;;;
;;;
;;; There are 2 hooks: daves-rmail-before-save-message-hook and
;;; daves-rmail-after-save-message-hook which are called before and
;;; after messages are saved, respectively. These hooks are only run
;;; for daves-rmail-save-messages-babyl and
;;; daves-rmail-save-messages-inbox calls. 
;;;
;;; Since this package makes use of mail-extract-address-components from
;;; mail-extr.el, that package is needed. It is packaged as part of
;;; the 19.19 distribution.
;;;
;;; I'm always looking for improvements, suggestions, bug reports,
;;; criticisms, etc of the code. If you have a patch, please send it
;;; to me and I'll incorporate it into the next version. This is my
;;; first major piece of lisp code that I'm putting out to the net, so
;;; be warned, and be gentle. I consider this beta code - It works on
;;; my setup of emacs, but there may be problems on other setups.
;;; 
;;;
;;; To Do: Set it up to save only messages marked with a certain tag
;;;        or via a regexp. Clean up the code, especially the parser.
;;;        Set up the Classify menu so that the original menu options
;;;        are at the top, and the new options are on the bottom.
;;;        Maybe someone with lots of keymap experience can fix this?
;;;
;;; Bugs: It has some problems with mail headers using % - my bug fix
;;; just changes all of the % in the headers to . after the parsing is
;;; done. This does not affect the parsing routine. Anyone know of a
;;; better way to do this to maintain the % ? Emacs thinks that it is
;;; part of a format command.
;;;
;;;
;;; To use:
;;;
;;; You'll need to either create a directory called RMail in your home
;;; directory (note the capitalizations) or change the variable
;;; saved-rmail-file-directory to the directory you want messages
;;; saved to.
;;; 
;;; Add this to your .emacs file:
;;; (autoload 'daves-rmail-save-messages-babyl "rmail-saver" nil t)
;;; (autoload 'daves-rmail-save-messages-inbox "rmail-saver" nil t)
;;; (autoload 'daves-rmail-save-this-message-babyl "rmail-saver" nil t)
;;; (autoload 'daves-rmail-save-this-message-inbox "rmail-saver" nil t)
;;;
;;; If you want rmail-saver to prompt you each time it saves a message, put
;;; this in your .emacs: 
;;; (setq daves-rmail-prompt-for-filename t)
;;;
;;; If you want to bind these functions to key mappings and menu bar items in 
;;; RMAIL and RMAIL-summary modes, I use:
;;;
;;; (setq rmail-mode-hook
;;;       '(lambda ()
;;;   (define-key rmail-mode-map "\C-co" 
;;;     'daves-rmail-save-this-message-babyl)
;;;   (define-key rmail-mode-map "\C-c\C-o" 
;;;     'daves-rmail-save-this-message-inbox)
;;;   (define-key rmail-mode-map "\C-c\eo" 
;;;     'daves-rmail-save-messages-babyl)
;;;   (define-key rmail-mode-map "\C-c\e\C-o" 
;;;     'daves-rmail-save-messages-inbox)
;;;   (define-key rmail-mode-map [menu-bar classify dave-clear-to-inbox]
;;;     '("Clear box (inbox)" . daves-rmail-save-messages-inbox))
;;;   (define-key rmail-mode-map [menu-bar classify dave-clear-to-rmail]
;;;     '("Clear box (Rmail)" . daves-rmail-save-messages-babyl))
;;;   (define-key rmail-mode-map [menu-bar classify dave-output]
;;;     '("Save msg (inbox)" . daves-rmail-save-this-message-inbox))
;;;   (define-key rmail-mode-map [menu-bar classify dave-output-inbox]
;;;     '("Save msg (Rmail)" . daves-rmail-save-this-message-babyl))
;;;   ))
;;; (setq rmail-summary-mode-hook
;;;       '(lambda ()
;;;   (define-key rmail-summary-mode-map "\C-co" 
;;;     'daves-rmail-save-this-message-babyl)
;;;   (define-key rmail-summary-mode-map "\C-c\C-o" 
;;;     'daves-rmail-save-this-message-inbox)
;;;   (define-key rmail-summary-mode-map "\C-c\eo" 
;;;     'daves-rmail-save-messages-babyl)
;;;   (define-key rmail-summary-mode-map "\C-c\e\C-o" 
;;;     'daves-rmail-save-messages-inbox)
;;;   (define-key rmail-summary-mode-map 
;;;     [menu-bar classify dave-clear-to-inbox]
;;;     '("Clear box (inbox)" . daves-rmail-save-messages-inbox))
;;;   (define-key rmail-summary-mode-map 
;;;     [menu-bar classify dave-clear-to-rmail]
;;;     '("Clear box (Rmail)" . daves-rmail-save-messages-babyl))
;;;   (define-key rmail-summary-mode-map 
;;;     [menu-bar classify dave-output]
;;;     '("Save msg (inbox)" . daves-rmail-save-this-message-inbox))
;;;   (define-key rmail-summary-mode-map 
;;;     [menu-bar classify dave-output-inbox]
;;;     '("Save msg (Rmail)" . daves-rmail-save-this-message-babyl))
;;;      ))
;;; which binds C-c o to daves-rmail-save-this-message-babyl
;;;             C-c C-o to daves-rmail-save-this-message-inbox
;;;             C-c M-o to daves-rmail-save-messages-babyl
;;;             C-c M-C-o to daves-rmail-save-messages-inbox
;;;
;;; in the RMAIL and RMAIL-summary buffers
;;; 
;;; and which also sets up 4 new menu options in the Classify menu of
;;; rmail and rmail-summary modes. Anybody know of a good way to have
;;; these options put at the end of the menu?


;; We need the mail-extr package. Should be in the distribution. If not, e-mail
;; me for a copy.
(autoload 'mail-extract-address-components "mail-extr")

(defconst rmail-saver-version (substring "$Revision 1.0$" 11 -2)
  "This set of extensions will allow for automatically saving mail messages to individual rmail files in another directory, then tag them for immediate deletion.

Report bugs to: David L. Sifry <sifry@sun57.pic.melco.co.jp>")

(defvar current-sender-name nil)
(defvar saved-rmail-file-directory "~/RMail/"
  "*This is the directory where saved mail files are placed")

(defvar user-and-machine-type-address-length 7 
  "*For addresses of the type aaaaa@bbbbb, This variable describes the maximum 
length of the string bbbbb so that messages from people having addresses of 
this type will be saved under the filename aaaaa@bbbbb. If the string bbbbb 
is greater than this length, the messages will be saved under the file name 
aaaaa.  

To disable this check, set this variable to 0. To use only the user name of
the address as the file name, set this variable to nil.")

(defvar user-machine-and-domain-length 12 
  "*For addresses of the type aaaaa@bbbbb.ccccc, This variable describes the
maximum length of the string bbbbb.ccccc so that messages from people having
addresses of this type will be saved under the filename aaaaa@bbbbb.ccccc. If
the string bbbbb.ccccc is greater than this length, the messages will be
checked against user-and-machine-type-adress-length to be saved under the
file name aaaaa@bbbbb.

To disable this check, set this variable to 0. To use only the user name of
the address as the file name, set this variable to nil.")

(defvar user-machine-domain-and-company-length 15 
  "*For addresses of the type aaaaa@bbbbb.ccccc.ddddd, This variable describes
the maximum length of the string bbbbb.ccccc.ddddd so that messages from
people having addresses of this type will be saved under the filename
aaaaa@bbbbb.ccccc.ddddd. If the string bbbbb.ccccc.ddddd is greater than this
length, the messages will be checked against user-machine-and-domain-length
to be saved under fhe file name aaaaa@bbbbb.ccccc, and if that fails, then
checked against user-and-machine-type-adress-length to be saved under the
file name aaaaa@bbbbb or aaaaa.

To disable this check, set this variable to 0. To use only the user name of
the address as the file name, set this variable to nil.")

(defvar daves-rmail-before-save-message-hook nil
  "*This is a hook called before messages are saved")

(defvar daves-rmail-after-save-message-hook nil
  "*This is a hook called after messages are saved")

(defvar daves-rmail-prompt-for-filename nil
  "*If the variable daves-rmail-prompt-for-filename is non-nil, the
functions in rmail-saver will give you a prompt allowing you to
enter your own filename when saving, and the parsed filename
becomes the default. If nil, rmail-saver works quietly, using only
the parsed filenames when saving. ")

(defun daves-rmail-save-messages-babyl ()
  (interactive)
  (if (or noninteractive
   (y-or-n-p "Save all current undeleted messages to files? "))
      (progn
 (message "") ; Erase Yes or No question
 (run-hooks 'daves-rmail-before-save-message-hook)
 (daves-rmail-save-messages-for-real t)
 (run-hooks 'daves-rmail-after-save-message-hook))
    (message "")))

(defun daves-rmail-save-this-message-babyl ()
  (interactive)
  (save-excursion
    (if (not (get-buffer (file-name-nondirectory rmail-file-name)))
 (rmail)
      (set-buffer (file-name-nondirectory rmail-file-name)))
    (beginning-of-buffer)
 (daves-get-sender) ;; Get the file name to save in
 (rmail-output 
  (if daves-rmail-prompt-for-filename
      (read-file-name
       (concat "Output message to Rmail file: (default "
        current-sender-name ") ")
       (file-name-directory saved-rmail-file-directory)
       (expand-file-name current-sender-name
    saved-rmail-file-directory))
    (expand-file-name current-sender-name
        saved-rmail-file-directory)))))

(defun daves-rmail-save-messages-inbox ()
  (interactive)
  (if (or noninteractive
   (y-or-n-p "Save all current undeleted messages to files? "))
      (progn
 (message "") ; Erase Yes or No question
 (run-hooks 'daves-rmail-before-save-message-hook)
 (daves-rmail-save-messages-for-real nil)
 (run-hooks 'daves-rmail-after-save-message-hook))
    (message "")))

(defun daves-rmail-save-this-message-inbox ()
  (interactive)
  (save-excursion
    (if (not (get-buffer (file-name-nondirectory rmail-file-name)))
 (rmail)
      (set-buffer (file-name-nondirectory rmail-file-name)))
    (beginning-of-buffer)
 (daves-get-sender) ;; Get the file name to save in
 (rmail-output 
  (if daves-rmail-prompt-for-filename
      (read-file-name
       (concat "Output message to Unix mail file: (default "
        current-sender-name ") ")
       (file-name-directory saved-rmail-file-directory)
       (expand-file-name current-sender-name
    saved-rmail-file-directory))
    (expand-file-name current-sender-name
        saved-rmail-file-directory)))))


(defun daves-rmail-save-messages-for-real (babyl-format)
  (save-excursion
    (if (not (get-buffer (file-name-nondirectory rmail-file-name)))
 (rmail)
      (set-buffer (file-name-nondirectory rmail-file-name)))
    (let ((rmailbuf (current-buffer))
   (done nil)
   (message-number nil))
      (rmail-first-message)
      (if (rmail-message-deleted-p (rmail-what-message))
   (rmail-next-undeleted-message 1))
      (beginning-of-buffer)
      (while (not done)
 (setq message-number (rmail-what-message))
 (daves-get-sender) ;; Get the file name to save in
 (if babyl-format
     (rmail-output-to-rmail-file
  (if daves-rmail-prompt-for-filename
      (read-file-name
       (concat "Output message to Rmail file: (default "
        current-sender-name ") ")
       (file-name-directory saved-rmail-file-directory)
       (expand-file-name current-sender-name
    saved-rmail-file-directory))
    (expand-file-name current-sender-name
        saved-rmail-file-directory)))
   (rmail-output
  (if daves-rmail-prompt-for-filename
      (read-file-name
       (concat "Output message to Unix mail file: (default "
        current-sender-name ") ")
       (file-name-directory saved-rmail-file-directory)
       (expand-file-name current-sender-name
    saved-rmail-file-directory))
    (expand-file-name current-sender-name
        saved-rmail-file-directory))))
 (rmail-delete-forward) ;; Mark the message for deletion
 (if (equal (rmail-what-message) message-number)
     (setq done t))))))

(defun daves-get-sender (&optional from)
  (interactive "sAddress: ")
  (save-excursion
    (if (not from)
 (progn
   (beginning-of-buffer)
   (if (re-search-forward "^[Ff]rom:[ \t]*" nil t);; Search for From:
       (setq from (buffer-substring (point) 
        (progn 
          (end-of-line) (point))))
     (if (re-search-forward "^[Ss]ender:[ \t]*" nil t);;Search for Sender:
  (setq from (buffer-substring (point) 
          (progn 
            (end-of-line) (point))))))))
    (let* ((mail-extr-mangle-uucp t)
    (data (mail-extract-address-components from))
    (name (car data))
    (net (car (cdr data))))
      (if (equal name "") (setq name net))
      (if (equal net "") 
   (error "mail-extr returned \"\" as the address"))
      (setq current-sender-name (truncate-address net))
      (if (not noninteractive)
   (message current-sender-name)))
    current-sender-name))

(defun truncate-address (net)
  (let* ((temp-buffer (get-buffer-create "*rmail-get-sender-address*"))
  (file-name nil)
  (at-sign-position nil)
  (first-dot-position nil)
  (second-dot-position nil)
  (eobuf nil))
    (set-buffer temp-buffer)
    (erase-buffer)
    (insert net)
    (beginning-of-buffer)
    (setq 
     file-name 
     (buffer-substring
      1
      (progn
 (if (re-search-forward "@" nil t)
     (if (and user-and-machine-type-address-length
       user-machine-and-domain-length
       user-machine-domain-and-company-length)
  ;; If the address is a net address (non-local)
  (progn
    (setq at-sign-position (point))
    (setq first-dot-position (search-forward "." nil t))
    (setq second-dot-position (search-forward "." nil t))
    (setq eobf (point-max))
   (if (not second-dot-position)
       ;; if there are not less than 2 dots 
       ;; (aaaa@bbbbb.ccccc) or (aaaa@bbbbb)
       (if (not first-dot-position)
    ;; if there are no dots (aaaa@bbbbb)
    (if (> (- eobf at-sign-position) 
    user-and-machine-type-address-length)
        ;; if the length of the stuff
        ;; after the @ is greater than the variable
        ;; user-and-machine-type-address-length
        ;; use only the username (aaaa)
        (- at-sign-position 1)
      ;; otherwise return the end of the
      ;; buffer (aaaa@bbbbb)
      eobf)
         (if (> (- eobf at-sign-position) 
         user-machine-and-domain-length)
      ;; if there is one dot (aaaa@bbbbb.ccccc) then
      ;; if the string from the @ to end of the address
      ;; is greater than the variable
      ;; user-machine-and-domain-length then see if we 
      ;; can get the username and the machine name 
      ;; (aaaa@bbbbb)
      (if (> (- first-dot-position at-sign-position) 
      user-and-machine-type-address-length)
          ;; if the length of the stuff
          ;; after the @ is greater than the variable
          ;; user-and-machine-type-address-length
          ;; use only the username (aaaa)
          (- at-sign-position 1)
        ;; otherwise return up to the first dot
        ;; (aaaa@bbbbb)
        (- first-dot-position 1))
    ;; otherwise go to the end of the buffer 
    ;;(aaaa@bbbb.ccccc)
    eobf))
     (if (> (- eobf at-sign-position) 
     user-machine-domain-and-company-length)
         ;; if there are 2 dots (aaaa@bbbbb.ccccc.dddddd) 
         ;; if the string from the @ to the end of the buffer
         ;; is greater than 
         ;; user-machine-domain-and-company-length
         (if (> (- second-dot-position at-sign-position) 
         user-machine-and-domain-length)
      ;; if there is one dot (aaaa@bbbbb.ccccc) then
      ;; if the string from the @ to end of the address
      ;; is greater than the variable
      ;; user-machine-and-domain-length then see if we 
      ;; can get the username and the machine name 
      ;; (aaaa@bbbbb)
      (if (> (- first-dot-position at-sign-position) 
      user-and-machine-type-address-length)
          ;; if the length of the stuff
          ;; after the @ is greater than the variable
          ;; user-and-machine-type-address-length
          ;; use only the username (aaaa)
          (- at-sign-position 1)
        ;; otherwise return up to the first dot
        ;; (aaaa@bbbbb)
        (- first-dot-position 1))
    ;; otherwise return up to the second dot 
    ;;(aaaa@bbbb.ccccc)
    (- second-dot-position 1))
       ;; otherwise go to the end of the buffer 
       ;;(aaaa@bbbb.ccccc.dddddd)
       (- second-dot-position 1))))
   ;; otherwise it's either a local address (aaaaa) or
   ;; we're going to save as if it were a local address
       (- (point) 1))
   (point-max)))))
    (erase-buffer)
    (insert file-name)
    (beginning-of-buffer)
    (while (search-forward "%" nil t)
      (replace-match "." nil t))
    (setq file-name (buffer-string))
    (kill-buffer temp-buffer)
    file-name))

(provide 'rmail-saver)
;;; rmail-saver.el ends here
