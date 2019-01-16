;;; erc-list-cache.el --- Cache IRC server channel lists

;; Filename: erc-list-cache.el
;; Description: Cache IRC server channel lists
;; Author: Joe Bloggs <vapniks@yahoo.com>
;; Maintainer: Joe Bloggs <vapniks@yahoo.com>
;; Copyleft (Ↄ) 2019, Joe Bloggs, all rites reversed.
;; Created: 2019-01-14 02:29:19
;; Version: 20190116.1719
;; Last-Updated: Wed Jan 16 17:19:00 2019
;;           By: Joe Bloggs
;;     Update #: 12
;; URL: https://github.com/vapniks/erc-list-cache
;; Keywords: comm
;; Compatibility: GNU Emacs 25.2.2
;; Package-Requires: ()
;;
;; Features that might be required by this library:
;;
;; erc
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
;; along with this program; see the file COPYING.
;; If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Commentary: 
;; 
;; Bitcoin donations gratefully accepted: 1ArFina3Mi8UDghjarGqATeBgXRDWrsmzo
;; 
;;; Installation
;; 
;; To make sure you have the most up-to-date version of this library it is best to install 
;; using the emacs package system, with the appropriate repository added (e.g https://melpa.org/)
;; 
;; To install without using a package manager:
;; 
;;  - Put the library in a directory in the emacs load path, like ~/.emacs.d/
;;  - Add (require 'erc-list-cache) in your ~/.emacs file
;;;;;;;;;;;

;;; Commands:
;;
;; Below is a complete list of commands:
;;
;;  `erc-switch-to-channels-list'
;;    Switch to the channels list buffer for the current IRC server.
;;    Keybinding: M-x erc-switch-to-channels-list
;;
;;; Customizable Options:
;;
;; Below is a list of customizable options:
;;
;;  `erc-list-cache-dir'
;;    Directory containing cached channel lists.
;;    default = (concat erc-user-emacs-directory "erc_channel_lists")
;;  `erc-list-cache-max-age'
;;    Maximum number of days before prompting for update of cached channel list.
;;    default = 30

;;; Installation:
;;
;; Put erc-list-cache.el in a directory in your load-path, e.g. ~/.emacs.d/
;; You can add a directory to your load-path with the following line in ~/.emacs
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;; where ~/elisp is the directory you want to add 
;; (you don't need to do this for ~/.emacs.d - it's added by default).
;;
;; Add the following to your ~/.emacs startup file.
;;
;; (require 'erc-list-cache)

;;; History:

;;; Require
(require 'erc)

;;; Code:

(defcustom erc-list-cache-dir (concat erc-user-emacs-directory "erc_channel_lists")
  "Directory containing cached channel lists."
  :group 'erc-list
  :type 'directory)

(defcustom erc-list-cache-max-age 30
  "Maximum number of days before prompting for update of cached channel list."
  :group 'erc-list
  :type 'integer)

;;;###autoload
(defun erc-switch-to-channels-list (&optional arg)
  "Switch to the channels list buffer for the current IRC server.
If the channels list buffer doesn't currently exist create one from either cached information
or a call `erc-cmd-LIST' (if cache doesn't exist or prefix ARG is used)."
  (interactive "P")
  (if (eq major-mode 'erc-mode)
      (let* ((network (symbol-name (erc-network)))
	     (serverbuf (process-buffer erc-server-process))
	     (cachefilename (concat erc-list-cache-dir "/" network))
	     (cachefileage (- (time-to-days (current-time))
			      (time-to-days (nth 5 (file-attributes cachefilename))))))
	(if (buffer-live-p erc-list-buffer)
	    (switch-to-buffer erc-list-buffer)
	  (if (or arg
		  (not (file-readable-p cachefilename))
		  (and (> cachefileage erc-list-cache-max-age)
		       (y-or-n-p (format "Cache file is more than %s days old, reload from server? "
					 erc-list-cache-max-age))))
	      (progn (erc-cmd-LIST)
		     (sit-for 1 t) ;need to wait for `erc-list-buffer' to be defined
		     (with-current-buffer erc-list-buffer
		       (add-hook 'kill-buffer-hook
				 `(lambda nil (unless (or (with-current-buffer erc-list-server-buffer
							    (memq 'erc-list-handle-322 erc-server-322-functions))
							  (not (y-or-n-p "Cache this channel list? ")))
						(if (not (file-directory-p erc-list-cache-dir))
						    (make-directory erc-list-cache-dir t))
						(write-file ,cachefilename)))
				 nil t)))
	    (find-file cachefilename)
	    (goto-char (point-min))
	    ;; Align columns
	    (while (re-search-forward "^#+[^ ]+" nil t)
	      (put-text-property
	       (point) (1+ (point))
	       'display '(space :align-to erc-list-nusers-column)))
	    (goto-char (point-min))
	    (while (re-search-forward "^#+[^ ]+ [0-9]+" nil t)
	      (put-text-property
	       (point) (1+ (point))
	       'display '(space :align-to erc-list-topic-column)))
	    (erc-list-menu-mode)
	    (setq-local erc-list-server-buffer serverbuf)
	    (let ((buf (current-buffer)))
	      (rename-buffer
	       (with-current-buffer serverbuf
		 (setq erc-list-buffer buf)
		 (concat "*Channels of " erc-server-announced-name " (cached)*"))))
	    (set-buffer-modified-p nil)
	    (goto-char (point-min)))))
    (error "This is not an ERC buffer")))


(provide 'erc-list-cache)

;; (org-readme-sync)
;; (magit-push)

;;; erc-list-cache.el ends here
