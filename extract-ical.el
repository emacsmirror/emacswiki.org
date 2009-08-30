;;; extract-ical --- Import iCalendar events from buffers to OS X iCal

;; Copyright (C) 2007 Seth Falcon

;; Author: Seth Falcon <seth@userprimary.net>
;; Keywords: icalendar, ical, vcal, gnus
;; URL: http://userprimary.net/user/software

;;; Commentary:

;; extract-ical provides send-invite-to-ical to extract an
;; icalendar event from a buffer, write it to a temporary
;; file, and open iCal such that the extracted event is imported.

;; send-invite-to-ical-from-gnus allows you to run this function from
;; gnus on articles containing icalendar event invitations.  You can
;; cal extract-ical-gnus-insinuate in your init file to set 'C-c i' as
;; the binding for importing icalendar invites from an article into
;; iCal.

;; You can tell extract-ical where to write its temporary file by
;; setting the extract-ical-invite-file variable below.

;;; Code:

(require 'gnus)

(defvar extract-ical-invite-file "/tmp/current-ical-invite.ics"
  "The file to use for writing vcal events (will be overwritten)")

(defun send-invite-to-ical ()
  "Write vcal event in current buffer to extract-ical-invite-file"
  (interactive)
  (save-excursion
    (let (invite i-beg i-end
                 (content-buf (current-buffer))
                 (invite-file extract-ical-invite-file))
      (goto-char (point-min))
      (if (not (re-search-forward "^BEGIN:VCALENDAR" (point-max) t))
          (message "Didn't find BEGIN:VCALENDAR, stopping")
        (setq i-beg (match-beginning 0))
        (message "found match at %d" i-beg))
      (setq i-end (re-search-forward "^END:VCALENDAR\n?" (point-max) t))
      (if (not i-end)
          (message "Didn't find END:VCALENDAR, stopping"))
      (write-region i-beg i-end invite-file nil nil nil nil)
      (shell-command (format "open %s" invite-file)))))

(defun send-invite-to-ical-from-gnus ()
  (interactive)
  (save-excursion
    (when (equal major-mode 'gnus-article-mode)
      (gnus-article-show-summary))
    (when (equal major-mode 'gnus-summary-mode)
      (gnus-summary-show-article t)
      (gnus-summary-select-article-buffer)
      (send-invite-to-ical)
      (gnus-summary-show-article))))

(defun extract-ical-gnus-insinuate ()
  "Call this in your init file and you will be able to use 'C-c i'
to import an icalendar event into iCal"
  (eval-after-load 'gnus-sum
    `(define-key gnus-summary-mode-map ,(kbd "C-c i")
       'send-invite-to-ical-from-gnus))
  (eval-after-load 'gnus
    `(define-key gnus-article-mode-map ,(kbd "C-c i")
       'send-invite-to-ical-from-gnus)))

(provide 'extract-ical)
