;;; pbotutil.el --- interface to pbotutil.pl

;; Copyright (C) 2007 Ævar Arnfjörð Bjarmason

;; Author: Ævar Arnfjörð Bjarmason <avar@cpan.org>
;; Created: 2007-11-22
;; Keywords: comm

;; This file is not a part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Interfaces with paste bots via the `pbotutil.pl' utility found at
;; http://sial.org/code/perl/scripts/pbotutil.pl

;; Example usage with a custom path and nick:

;; (require 'pbotutil)
;; (setq pbotutil-command "perl ~/pbotutil.pl")
;; (setq pbotutil-nick "avar")

;;; Code:

(defvar pbotutil-command "pbotutil.pl"
  "The pbotutil.pl command name, this could be changed to \"perl
  ~/pbotutil.pl\" if pbotutil.pl were not in $PATH")
(defvar pbotutil-nick ""
  "The nickname `pbotutil-region' will use by default")
(defvar pbotutil-channel "#perl"
  "The channel `pbotutil-region' will use by default")

(defvar pbotutil-prev-summary ""
  "The last summary provided. For internal use")
(defvar pbotutil-prev-channel nil
  "The last channel provided or `nil' if none. For internal use")

(defun pbotutil-region (start end &optional channel nick summary)
  ""
  (interactive "r")
  (let ((channel (or channel (read-from-minibuffer "Channel: " (or pbotutil-prev-channel pbotutil-channel))))
        (nick (or nick (read-from-minibuffer "Nick: " pbotutil-nick)))
        (summary (read-from-minibuffer "Summary: " pbotutil-prev-summary)))
    (setq pbotutil-prev-summary summary)
    (setq pbotutil-prev-channel channel)
    (let* ((out (make-temp-file "pbotutil"))
           (command (concat
                     pbotutil-command
                     " -c '" channel
                     "' -u '" nick
                     "' -m '" summary
                     "' put '" out "'")))
      (unwind-protect
           (progn
             (write-region start end out)
             (let* ((str (shell-command-to-string command))
                    (str-len (length str))
                    (url (substring str 0 (- str-len 1))))
               (kill-new url))
             (delete-file out))))))

(defun pbotutil-get (url)
  "Uses \"pbotutil.pl get\" to get the contents of a given paste
  url"
  (let ((command (concat pbotutil-command " get '" url "'")))
    (shell-command-to-string command)))

(provide 'pbotutil)

;;; pbotutil.el ends here
