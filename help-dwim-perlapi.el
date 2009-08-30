;;; help-dwim-perlapi.el --- Show help of perlapi

;; Copyright 2007 Ye Wenbin
;;
;; Author: wenbinye@gmail.com
;; Version: $Id: help-dwim-perlapi.el,v 0.0 2007/08/23 11:34:04 ywb Exp $
;; Keywords: 
;; X-URL: not distributed yet

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; 

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'help-dwim-perlapi)

;;; Code:

(eval-when-compile
  (require 'cl))

(defvar help-dwim-perlapi-file
  (substring (shell-command-to-string "perldoc -l perlapi") 0 -1)
  "Pod file of perlapi")

(defvar help-dwim-perlapi-obarray
  (let ((db (make-vector 293 0))
        beg name)
    (with-temp-buffer
      (insert-file-contents help-dwim-perlapi-file)
      (goto-char (point-min))
      (while (re-search-forward "^=item " nil t)
        (setq name (buffer-substring (match-end 0) (line-end-position))
              beg (1- (match-beginning 0)))
        (re-search-forward "^=for hackers")
        (forward-line 2)
        (set (intern name db)
             (cons beg (point)))))
    db)
  "Database for perlapi")

(defvar help-dwim-perlapi-src-directory nil
  "Source directory of perl")

(defun help-dwim-perlapi (symbol)
  (interactive
   (list (intern (completing-read "Describe: " help-dwim-perlapi-obarray nil t)
                 help-dwim-perlapi-obarray)))
  (let ((bufname (format "*Woman Perlapi %S*" symbol))
        (name (symbol-name symbol)))
    (if (buffer-live-p (get-buffer bufname))
        (display-buffer bufname)
      (with-current-buffer (get-buffer-create bufname)
        (setq bound (symbol-value symbol))
        (insert-file-contents help-dwim-perlapi-file nil
                              (car bound) (cdr bound))
        (goto-char (point-min))
        (insert (format "=head1 %s\n\n=over 8\n\n" name))
        (if (re-search-forward "=for hackers" nil t)
            (replace-match "=back\n\n=head2 for hackers\n"))
        (goto-char (point-max))
        (insert "\n=cut\n")
        (call-process-region (point-min) (point-max)
                             "pod2man" t t nil "-n" name)
        (woman-process-buffer)
        (goto-char (point-max))
        (if (re-search-backward "Found in file " nil t)
            (let ((inhibit-read-only t))
              (make-text-button (match-end 0) (line-end-position)
                                'id name
                                'action 'help-dwim-perlapi-push-button)))
        (display-buffer (current-buffer))))))

(defun help-dwim-perlapi-push-button (but)
  (let ((label (button-label but))
        (id (button-get but 'id)))
    (with-selected-window
        (display-buffer (find-file-noselect (concat (file-name-as-directory help-dwim-perlapi-src-directory) label)))
      (goto-char (point-min))
      (re-search-forward (concat "^=for apidoc .*" (regexp-quote id) "\\>")))))

(help-dwim-register
 '(perlapi . [ "a-zA-Z0-9_" help-dwim-perlapi-obarray nil help-dwim-perlapi ])
 nil)
(help-dwim-load-extra)

(provide 'help-dwim-perlapi)
;;; help-dwim-perlapi.el ends here
