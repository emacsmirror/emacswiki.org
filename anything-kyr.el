;;; anything-kyr.el --- Show context-aware commands
;; $Id: anything-kyr.el,v 1.13 2014/10/14 18:29:34 rubikitch Exp $

;; Copyright (C) 2009  rubikitch

;; Author: rubikitch <rubikitch@ruby-lang.org>
;; Keywords: anything, convenience
;; URL: http://www.emacswiki.org/cgi-bin/wiki/download/anything-kyr.el

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

;; Show context-aware commands by `anything'. It avoids forgetting
;; existence of appropriate commands. KYR is an abbreviation of "Kuuki
;; wo YomeRu", which means that person can read the atmosphere in
;; Japanese. Anything-kyr shows appropriate commands for the
;; situation.

;; Note that anything-kyr.el provides only the framework. See
;; anything-kyr-config.el for practical, polished, easy to use
;; configurations which can be used to assemble a custom personalized
;; configuration.
;;
;; http://www.emacswiki.org/cgi-bin/wiki/download/anything-kyr-config.el


;;; Installation:
;;
;; Put anything-kyr.el and anything-kyr-config.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'anything-kyr-config)
;;
;; Then put `anything-c-source-kyr' to `anything-sources'.
;; M-x `anything-kyr' for demo.

;;; History:

;; $Log: anything-kyr.el,v $
;; Revision 1.13  2014/10/14 18:29:34  rubikitch
;; Notify strings
;;
;; Revision 1.12  2014/10/14 18:05:00  rubikitch
;; indentation (no code change)
;;
;; Revision 1.11  2014/10/14 18:03:33  rubikitch
;; better display string
;;
;; Revision 1.10  2014/10/14 17:55:54  rubikitch
;; anything-kyr-candidates: return (display . real)
;;
;; Revision 1.9  2014/10/14 17:44:35  rubikitch
;; org-link support
;;
;; Revision 1.8  2014/10/14 12:05:29  rubikitch
;; new function: anything-kyr-should-notify-p
;;
;; Revision 1.7  2009/03/04 00:02:14  rubikitch
;; Command description support (thanks tk159)
;;
;; Revision 1.6  2009/02/22 05:04:33  rubikitch
;; ignore erros when calculating conditions
;;
;; Revision 1.5  2009/02/19 16:25:02  rubikitch
;; * update doc.
;; * New command: `anything-kyr'
;;
;; Revision 1.4  2009/02/19 16:17:44  rubikitch
;; remove unneeded defvars
;;
;; Revision 1.3  2009/02/19 16:12:31  rubikitch
;; update doc
;;
;; Revision 1.2  2009/02/18 10:19:57  rubikitch
;; Commands are now symbols.
;;
;; Revision 1.1  2009/02/18 09:59:05  rubikitch
;; Initial revision
;;

;;; Code:

(defvar anything-kyr-version "$Id: anything-kyr.el,v 1.13 2014/10/14 18:29:34 rubikitch Exp $")
(require 'anything)
(require 'org)
(require 'org-element)

(defvar anything-c-source-kyr
  '((name . "Context-aware Commands")
    (candidates . anything-kyr-candidates)
    (action)
    (action-transformer . anything-kyr-action-transformer)
    (persistent-action . describe-function)))

(defvar anything-kyr-commands-by-major-mode nil
  "Show commands by major-mode.
It is a list of elements of (MAJOR-MODE COMMAND1 ...) or ((MAJOR-MODE1 MAJOR-MODE2) COMMAND1 ...).")
(defvar anything-kyr-commands-by-file-name nil
  "Show commands by file name.
It is a list of elements of (REGEXP COMMAND1 ...).")
(defvar anything-kyr-commands-by-condition nil
  "Show commands by condition.
It is a list of elements of (CONDITION COMMAND1 ...).")
(defvar anything-kyr-kinds
  '((anything-kyr-commands-by-condition . eval)
    (anything-kyr-commands-by-file-name
     . (lambda (re) (string-match re (or buffer-file-name ""))))
    (anything-kyr-commands-by-major-mode
     . (lambda (mode) (if (listp mode)
                          (memq major-mode mode)
                        (eq major-mode mode))))))
(defvar anything-kyr-default-notify-string
  (propertize "◎" 'face '((:foreground "red")))
  "Default notify string in mode-line")

;; (anything 'anything-c-source-kyr)

(defun anything-kyr-candidates ()
  (with-current-buffer anything-current-buffer
    (delq nil
          (loop for (varname . condition-func) in anything-kyr-kinds append
                (loop for (condition . cmds) in (symbol-value varname)
                      when (save-excursion
                             (ignore-errors (funcall condition-func condition)))
                      append (mapcar
                              (lambda (command)
                                (cond ((stringp command)
                                       (let ((plink (anything-kyr-parse-org-link command)))
                                         (cons (apply 'anything-kyr-format-string plink) (car plink))))
                                      ((commandp command)
                                       (cons (anything-kyr-format-string command) command))
                                      ((and (consp command) (commandp (car command)))
                                       (cons (anything-kyr-format-string (car command) (cdr command))
                                             (car command)))))
                              cmds))))))

(defun anything-kyr-format-string (command-or-link &optional description)
  (if description
      (format "%s (%s)" description command-or-link)
      (format "%s" command-or-link)))

(defun anything-kyr-action-transformer (action command)
  (if (stringp command)
      '(("Open link" . org-open-link-from-string))
    '(("Execute" . call-interactively)
      ("Describe Function" . describe-function))))

(defun anything-kyr-parse-org-link (link)
  (with-temp-buffer
    (insert link)
    (goto-char (point-min))
    (let ((plink (org-element-link-parser)))
      (list (org-element-property :raw-link plink)
            (ignore-errors
              (buffer-substring (org-element-property :contents-begin plink)
                                (org-element-property :contents-end plink)))))))

(defun anything-kyr-commands-by-major-mode ()
  (assoc-default major-mode anything-kyr-commands-by-major-mode))

(defun anything-kyr ()
  (interactive)
  (anything 'anything-c-source-kyr))

(defun anything-kyr-should-notify-p ()
  (setq anything-kyr-should-notify-p
        (loop for (condition . cmds) in anything-kyr-commands-by-condition
              for result = (save-excursion (ignore-errors (eval condition)))
              when (stringp result)
              collect result
              else if result
              return anything-kyr-default-notify-string)))

(provide 'anything-kyr)

;; How to save (DO NOT REMOVE!!)
;; (emacswiki-post "anything-kyr.el")
;;; anything-kyr.el ends here
