;;; gnus-notify.el --- use the modeline to indicate groups with new messages

;; Author: Mark Triggs <mark@dishevelled.net>
;;
;; Contributions from: Frederic Couchet <fcouchet AT april.org>

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

;;; Commentary:

;; This code provides modeline notification of when certain groups contain
;; unread messages. Groups for whom unread messages should be indicated are
;; chosen by setting a group parameter.

;; Clicking on a group in the modeline will enter that group and view the new
;; message.

;; Code:

(defvar gnus-notify-show-unread-counts t
  "If true, show the number of unread messages in the modeline in addition to shortened group names.")


(when (fboundp 'gnus-define-group-parameter)
  (gnus-define-group-parameter
   modeline-notify
   :type bool
   :parameter-type '(const :tag "Notify of new messages for this group." t)
   :parameter-document "\

If this is set, the name of this group will be placed on the modeline when it
contains new messages"))

(defvar gnus-mst-display-new-messages "")
(defvar gnus-mst-notify-groups '())
(defvar gnus-notify-jump-to-group-hook '()
  "This hook is invoked before jumping to a gnus group with unread messages.
  Each hook should take a single argument - the GROUP to be selected")

(defvar gnus-mst-notify-face 'gnus-group-mail-3-empty
  "Face used to gnus notification message in modeline")

(add-hook 'gnus-exit-gnus-hook
          (lambda ()
            (setq gnus-mst-display-new-messages "")))


(defun gnus-mst-notify-modeline-form ()
  gnus-mst-display-new-messages)


(if (featurep 'xemacs)
    (unless (member 'gnus-mst-display-new-messages global-mode-string)
      (if (null global-mode-string)
          (setq global-mode-string '("" gnus-mst-display-new-messages))
        (setq global-mode-string
              (append global-mode-string
                      '(gnus-mst-display-new-messages)))))
  (unless (member '(:eval (gnus-mst-notify-modeline-form)) global-mode-string)
    (setq global-mode-string
          (append global-mode-string
                  (list '(:eval (gnus-mst-notify-modeline-form)))))))


(defun gnus-mst-notify-shorten-group-name (group)
  "shorten the group name to make it better fit on the modeline"
  (let ((name (if (string-match ":" group)
                  (cadr (split-string group "[:]"))
                group)))
    (mapconcat 'identity
               (mapcar
                (lambda (segment)
                  (string (elt segment 0)))
                (split-string name "[\\./]"))
               ".")))


(defun gnus-mst-notify-update-modeline ()
  "Update the modeline to show groups containing new messages"
  (if gnus-mst-notify-groups
      (setq gnus-mst-display-new-messages
            (append (list " [m:")
                    (maplist
                     #'(lambda (sublist)
                         (let ((group (car sublist))
                               (map (make-sparse-keymap)))
                           (define-key map [mode-line mouse-1]
                             `(lambda ()
                                (interactive)
                                (run-hook-with-args
                                 'gnus-notify-jump-to-group-hook ,group)
                                (gnus-group-read-group nil nil ,group)))
                           (list*
                            (list ':propertize
                                  (if gnus-notify-show-unread-counts
                                      (format "[%s%s]"
                                              (gnus-mst-notify-shorten-group-name
                                               (car sublist))
                                              (gnus-group-unread (car sublist)))
                                    (format "%s"
                                            (gnus-mst-notify-shorten-group-name
                                             (car sublist))))
                                  'face gnus-mst-notify-face
                                  'keymap map
                                  'help-echo "Visit this group")
                            (if (cdr sublist)
                                (list ", ")
                              nil))))
                     gnus-mst-notify-groups)
                    (list "] ")))
    (setq gnus-mst-display-new-messages "")))


(defun gnus-mst-notify-group (group)
  "Add notification for this group"
  (unless (member group gnus-mst-notify-groups)
    (add-to-list 'gnus-mst-notify-groups group)
    (gnus-mst-notify-update-modeline)))


(defun gnus-mst-show-groups-with-new-messages (&rest ignored)
  (interactive)
  (setq gnus-mst-notify-groups '())
  (gnus-mst-notify-update-modeline)
  (mapc '(lambda (g)
           (let* ((group (car g))
                  (unread (gnus-group-unread group)))
             (when (and (cdr (assoc 'modeline-notify
                                    (gnus-group-find-parameter group)))
                        (and (numberp unread) (> unread 0)))
               (gnus-mst-notify-group group))))
        gnus-newsrc-alist))


(add-hook 'gnus-after-getting-new-news-hook
          'gnus-mst-show-groups-with-new-messages)


(add-hook 'gnus-summary-exit-hook
          'gnus-mst-show-groups-with-new-messages)


(provide 'gnus-notify)
;;; gnus-notify.el ends here
