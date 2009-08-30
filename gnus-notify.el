;;; gnus-notify.el --- Show groups with new mail on the modeline

;; Author: Mark Triggs <mst@dishevelled.net>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(require 'gnus)

(gnus-define-group-parameter
 modeline-notify
 :type bool
 :parameter-type '(const :tag "Notify of new messages for this group." t)
 :parameter-document "\

If this is set, the name of this group will be placed on the modeline when it
contains new messages")

(defvar gnus-mst-display-new-messages "")
(defvar gnus-mst-notify-groups '())

(unless (member 'gnus-mst-display-new-messages global-mode-string)
  (setq global-mode-string (append global-mode-string
                                   (list 'gnus-mst-display-new-messages))))

(defun gnus-mst-notify-shorten-group-name (group)
  "shorten the group name to make it better fit on the modeline"
  (let ((name (if (string-match ":" group)
                  (cadr (split-string group "[:]"))
                group)))
    (mapconcat 'identity
               (mapcar
                #'(lambda (segment)
                    (string (elt segment 0)))
                (split-string name "[\\./]"))
               ".")))

(defun gnus-mst-notify-update-modeline ()
  "Update the modeline to show groups containing new messages"
  (if gnus-mst-notify-groups
      (setq gnus-mst-display-new-messages
            (format " [m: %s]"
                    (mapconcat 'gnus-mst-notify-shorten-group-name
                               gnus-mst-notify-groups
                               ", ")))
    (setq gnus-mst-display-new-messages "")))

(defun gnus-mst-notify-group (group)
  "Add notification for this group"
  (unless (member group gnus-mst-notify-groups)
    (setq gnus-mst-notify-groups (cons group gnus-mst-notify-groups))
    (gnus-mst-notify-update-modeline)))

(defun gnus-mst-show-groups-with-new-messages ()
  (interactive)
  (setq gnus-mst-notify-groups '())
  (gnus-mst-notify-update-modeline)
  (mapcar '(lambda (g)
             (let* ((group (car g))
                    (unread (gnus-group-unread group)))
               (when (and (cdr (assoc 'modeline-notify
                                      (gnus-group-find-parameter group)))
                          (and (numberp unread) (> unread 0)))
                 (gnus-mst-notify-group group))))
          gnus-newsrc-alist))

;; this hook has to be _appended_ otherwise the nnimap message count
;;  will be corrupted (dmr@gmx.it)

(add-hook 'gnus-after-getting-new-news-hook
          'gnus-mst-show-groups-with-new-messages t)

(provide 'gnus-notify)
;;; gnus-notify.el ends here
