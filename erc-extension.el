;;; erc-extension.el --- Some extension for erc

;; Filename: erc-extension.el
;; Description: Some extension for erc
;; Author: Andy Stewart lazycat.manatee@gmail.com
;; Maintainer: Andy Stewart lazycat.manatee@gmail.com
;; Copyright (C) 2008, 2009, Andy Stewart, all rights reserved.
;; Created: 2008-12-04 11:28:06
;; Version: 0.1
;; Last-Updated: 2008-12-04 11:28:11
;;           By: Andy Stewart
;; URL:
;; Keywords: erc
;; Compatibility: GNU Emacs 23.0.60.1
;;
;; Features that might be required by this library:
;;
;;
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Some extension for erc
;;

;;; Installation:
;;
;; Put erc-extension.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'erc-extension)
;;
;; No need more.

;;; Change log:
;;
;; 2008/12/04
;;      First released.
;;

;;; Acknowledgements:
;;
;;
;;

;;; TODO
;;
;;
;;

;;; Require


;;; Code:

(defun erc-autologin (nick)
  "Auto longing erc."
  (interactive "sNick: \n")
  (let ((passwd (read-passwd "Password: ")))
    (erc
     :server erc-server
     :port erc-port
     :full-name erc-user-full-name
     :nick nick
     :password passwd
     ))
  )

(defun switch-to-erc ()
  "Switch to an erc buffer, or run `erc-select'.
When called repeatedly, cycle through the buffers."
  (interactive)
  (let ((buffers (and (fboundp 'erc-buffer-list)
                      (erc-buffer-list))))
    (when (eq (current-buffer) (car buffers))
      (bury-buffer)
      (setq buffers (cdr buffers)))
    (if buffers
        (switch-to-buffer (car buffers))
      (call-interactively 'erc-autologin))))

(defun erc-cmd-HOWMANY (&rest ignore)
  "Display how many users (and ops) the current channel has."
  (interactive)
  (erc-display-message nil 'notice (current-buffer)
                       (let ((hash-table (with-current-buffer
                                             (erc-server-buffer)
                                           erc-server-users))
                             (users 0)
                             (ops 0))
                         (maphash (lambda (k v)
                                    (when (member (current-buffer)
                                                  (erc-server-user-buffers v))
                                      (incf users))
                                    (when (erc-channel-user-op-p k)
                                      (incf ops)))
                                  hash-table)
                         (format
                          "There are %s users (%s ops) on the current channel"
                          users ops))))

(defun erc-cmd-UPTIME (&rest ignore)
  "Display the up time of the system, as well as some load-related
stuff, to the current ERC buffer."
  (let ((uname-output
         (replace-regexp-in-string
          ", load average: " "] {Load average} ["
          ;; Collapse spaces, remove
          (replace-regexp-in-string
           " +" " "
           ;; Remove beginning and trailing whitespace
           (replace-regexp-in-string
            "^ +\\|[ \n]+$" ""
            (shell-command-to-string "uptime"))))))
    (erc-send-message
     (concat "{Uptime} [" uname-output "]"))))

(defun erc-cmd-YOW (&rest ignore)
  "Display some pinhead wisdom into the current ERC buffer.  I'd
rather not see it messaged to me, just sent out."
  (let ((yow-msg (replace-regexp-in-string "\n" "" (yow nil nil))))
    (erc-send-message
     (concat "{Pinhead wisdom} "
             yow-msg))))

(defun erc-cmd-UNAME (&rest ignore)
  "Display the result of running `uname -a' to the current ERC
buffer."
  (let ((uname-output
         (replace-regexp-in-string
          "[ \n]+$" "" (shell-command-to-string "uname -a"))))
    (erc-send-message
     (concat "{uname -a} [" uname-output "]"))))

(defun erc-cmd-TEMPLATE (command &rest words)
  (fset (intern (concat "erc-cmd-" (upcase command)))
        `(lambda ()
           (run-with-idle-timer 0 nil
                                (lambda (&rest ignore)
                                  (insert ,(mapconcat 'identity words " ")))))))

(autoload 'doctor-doc "doctor")
(autoload 'make-doctor-variables "doctor")

(defvar erc-doctor-id "{Emacs doctor} ")

(defun erc-cmd-DOCTOR (&optional last-sender &rest ignore)
  "Get the last message in the channel and doctor it."
  (let ((limit (- (point) 1000))
        (pos (point))
        doctor-buffer
        last-message
        text)
    ;; Make sure limit is not negative
    (when (< limit 0) (setq limit 0))
    ;; Search backwards for text from someone
    (while (and pos (not (let ((data (get-text-property pos 'erc-parsed)))
                           (and data
                                (string= (aref data 3) "PRIVMSG")
                                (or (not last-sender)
                                    (string= (car (split-string (aref data 2) "!"))
                                             last-sender))))))
      (setq pos (previous-single-property-change
                 pos 'erc-parsed nil limit))
      (when (= pos limit)
        (error "No appropriate previous message to doctor")))
    (when pos
      (setq last-sender (car (split-string
                              (aref (get-text-property
                                     pos 'erc-parsed) 2) "!"))
            doctor-buffer (concat "*ERC Doctor: " last-sender "*")
            last-message (split-string
                          ;; Remove punctuation from end of sentence
                          (replace-regexp-in-string
                           "[ .?!;,/]+$" ""
                           (aref (get-text-property pos
                                                    'erc-parsed) 5)))
            text (mapcar (lambda (s)
                           (intern (downcase s)))
                         ;; Remove salutation if it exists
                         (if (string-match
                              (concat "^" erc-valid-nick-regexp
                                      "[:,]*$\\|[:,]+$")
                              (car last-message))
                             (cdr last-message)
                           last-message))))
    (erc-send-message
     (concat erc-doctor-id
             ;; Only display sender if not in a query buffer
             (if (not (erc-query-buffer-p))
                 (concat last-sender ": "))
             (save-excursion
               (if (get-buffer doctor-buffer)
                   (set-buffer doctor-buffer)
                 (set-buffer (get-buffer-create doctor-buffer))
                 (make-doctor-variables))
               (erase-buffer)
               (doctor-doc text)
               (buffer-string))))))

(defun erc-cmd-HOP (&rest rest)
  "Part channel and immediately rejoin."
  (let ((channel (erc-default-target)))
    (erc-part-from-channel "hop")
    (erc-join-channel channel)))

(defun erc-cmd-SHOW (&rest form)
  "Eval FORM and send the result and the original form as:
 FORM => (eval FORM)."
  (let* ((form-string (mapconcat 'identity form " "))
         (result
          (condition-case err
              (eval (read-from-whole-string form-string))
            (error
             (format "Error: %s" err)))))
    (erc-send-message (format "%s => %S" form-string result))))

(defun erc-cmd-SHOWOFF (&rest ignore)
  "Show off implementation"
  (let* ((chnl (erc-buffer-list))
         (srvl (erc-buffer-list 'erc-server-buffer-p))
         (memb (apply '+ (mapcar (lambda (chn)
                                   (with-current-buffer chn
                                     (1- (length (erc-get-channel-user-list)))))
                                 chnl)))
         (show (format "is connected to %i networks and talks in %i chans to %i ppl overall :>"
                       (length srvl)
                       (- (length chnl) (length srvl))
                       memb)))
    (erc-send-action (erc-default-target) show)))

(defun erc-cmd-OPME ()
  "request a chanop op to me"
  (erc-message "PRIVMSG"
               (format "chanserv op %s %s"
                       (erc-default-target)
                       (erc-current-nick)) nil))

(defun erc-cmd-EMMS (&rest ignore)
  "Display the current EMMS track to the current ERC buffer."
  (let ((music-string (if emms-player-playing-p
                          (let ((track (emms-playlist-current-selected-track)))
                            (format "%s <<%s>> -- %s"
                                    (or (emms-track-get track 'info-artist) "")
                                    (or (emms-track-get track 'info-album) "")
                                    (or (emms-track-get track 'info-title) "")))
                        "Nothing playing right now")))
    (when music-string
      (erc-send-action (erc-default-target) music-string))))

(provide 'erc-extension)

;;; erc-extension.el ends here

;;; LocalWords:  erc autologin irc cmd HOWMANY UPTIME uname uptime msg pos
;;; LocalWords:  PRIVMSG
