;;; sequential-command-config.el --- Examples of sequential-command.el 
;; $Id: sequential-command-config.el,v 1.3 2009/03/22 09:09:58 rubikitch Exp $

;; Copyright (C) 2009  rubikitch

;; Author: rubikitch <rubikitch@ruby-lang.org>
;; Keywords: extensions, convenience
;; URL: http://www.emacswiki.org/cgi-bin/wiki/download/sequential-command-config.el

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
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Examples of sequential-command.el .

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `sequential-command-setup-keys'
;;    Rebind C-a, C-e, M-u, M-c, and M-l to seq-* commands.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;

;;; History:

;; $Log: sequential-command-config.el,v $
;; Revision 1.3  2009/03/22 09:09:58  rubikitch
;; New command: `sequential-command-setup-keys'
;;
;; Revision 1.2  2009/02/17 12:56:26  rubikitch
;; fixed typo
;;
;; Revision 1.1  2009/02/17 03:13:47  rubikitch
;; Initial revision
;;

;;; Code:

(defvar sequential-command-config-version "$Id: sequential-command-config.el,v 1.3 2009/03/22 09:09:58 rubikitch Exp $")
(require 'sequential-command)

(define-sequential-command seq-home
  beginning-of-line beginning-of-buffer seq-return)
(define-sequential-command seq-end
  end-of-line end-of-buffer seq-return)

(defun seq-upcase-backward-word ()
  (interactive)
  (upcase-word (- (1+ (seq-count)))))
(defun seq-capitalize-backward-word ()
  (interactive)
  (capitalize-word (- (1+ (seq-count)))))
(defun seq-downcase-backward-word ()
  (interactive)
  (downcase-word (- (1+ (seq-count)))))

(when (require 'org nil t)
  (define-sequential-command org-seq-home
    org-beginning-of-line beginning-of-buffer seq-return)
  (define-sequential-command org-seq-end
    org-end-of-line end-of-buffer seq-return))

(defun sequential-command-setup-keys ()
  "Rebind C-a, C-e, M-u, M-c, and M-l to seq-* commands.
If you use `org-mode', rebind C-a and C-e."
  (interactive)
  (global-set-key "\C-a" 'seq-home)
  (global-set-key "\C-e" 'seq-end)
  (global-set-key "\M-u" 'seq-upcase-backward-word)
  (global-set-key "\M-c" 'seq-capitalize-backward-word)
  (global-set-key "\M-l" 'seq-downcase-backward-word)
  (when (require 'org nil t)
    (define-key org-mode-map "\C-a" 'org-seq-home)
    (define-key org-mode-map "\C-e" 'org-seq-end)))

(provide 'sequential-command-config)

;; How to save (DO NOT REMOVE!!)
;; (emacswiki-post "sequential-command-config.el")
;;; sequential-command-config.el ends here
