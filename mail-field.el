;;; mail-field.el --- Emulate Sun mailtool field movement
 
;; Copyright (C) 1995 Kevin Davidson
 
;; Author: Kevin Davidson tkld@quadstone.com
;; Maintainer: Kevin Davidson tkld@quadstone.com
;; Created: 10 Mar 1995
;; Version: $Revision: 1.3 $
;; Keywords: mail message fields
 
  
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
 
;; A copy of the GNU General Public License can be obtained from this
;; program's author (send electronic mail to tkld@quadstone.com) or
;; from the Free Software Foundation, Inc., 675 Mass Ave, Cambridge,
;; MA 02139, USA.
 
;; LCD Archive Entry:
;; mail-field|Kevin Davidson|tkld@quadstone.com
;; |Emulate Sun mailtool field movement
;; |$Date: 1997/11/11 11:14:22 $|$Revision: 1.3 $|~/packages/mail-field.el
 
;;; Commentary:
;; Usage:
;; (require 'mail-field)
;; (add-hook 'mail-mode-hook '(lambda ()
;;      (define-key mail-mode-map [C-tab] 'mail-next-field)
;;      (define-key mail-mode-map [S-C-tab] 'mail-previous-field)))
;; (add-hook 'message-mode-hook '(lambda ()
;;      (define-key message-mode-map [C-tab] 'mail-next-field)
;;      (define-key message-mode-map [S-C-tab] 'mail-previous-field)))
 
 
;;; Change log:
;; $Log: mail-field.el,v $
;; Revision 1.3  1997/11/11 11:14:22  tkld
;; Added message-mode support in comments. Updated email address.
;;
;; Revision 1.2  1995/03/10  14:25:27  tkld
;; Added provide line!
;;
;; Revision 1.1  1995/03/10  14:24:17  tkld
;; Initial revision
;;
 
;;; Code:
 
(defconst mail-field-version (substring "$Revision: 1.3 $" 11 -2)
  "$Id: mail-field.el,v 1.3 1997/11/11 11:14:22 tkld Exp $
 
Report bugs to: Kevin Davidson tkld@quadstone.com")
 
(defconst mail-field-regexp "|>.*<|"
  "Regexp matching an editable text field.")
 
(defun mail-next-field (&optional N)
  "Move to next field in mail message. With arg move to Nth next field."
  (interactive "p")
  (forward-char)
  (search-forward-regexp mail-field-regexp nil "stay.at.end" N)
  (goto-char (match-beginning 0))
  (push-mark (match-end 0) nil t))
 
(defun mail-previous-field (&optional N)
  "Move to next field in mail message. With arg move to Nth next field."
  (interactive "p")
  (search-backward-regexp mail-field-regexp nil "stay.at.end" N)
  (goto-char (match-beginning 0))
  (push-mark (match-end 0) nil t))
 
(provide 'mail-field)
 
;;; mail-field.el ends here
