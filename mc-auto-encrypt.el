;;; mc-auto-encrypt.el --- automatically encrypt and decrypt at file open and save

;; Copyright (c) 2003 Charles Sebold, Doug Alcorn
;; Author: Charles Sebold
;; Contributors: Doug Alcorn <doug@lathi.net>
;;               Xavier Maillard <xma@gnu.org>
;; Version: 1.4a

;; This file is not yet part of GNU Emacs.

;; mc-auto-encrypt.el free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; mc-auto-encrypt.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; mc-auto-encrypt.el is free software

;;; Commentary:

;; I grabbed this code from Charles on the
;; http://www.emacswiki.org/wiki.pl?AutoEncrypt page.  It takes
;; advantage of the Local Variables auto evaluation feature in files.
;; By evaluating mc-auto-encrypt when a file is opened you can use
;; mailcrypt to encrypt a region of the file.

;; To start encrypting any plain text file add this to the bottom of
;; the file:

;; Local Variables:
;; eval: (mc-auto-encrypt)
;; End:

;; Note that you can tell Emacs to automatically evaluate code instead
;; of asking you. To do so, just add this to your .emacs (Note that you 
;; shouldn't do it for security reasons):

;; (setq enable-local-eval t) ;; VERY VERY DANGEROUS !!!

;; Take a look at the emacs info page on "File Variables" for details
;; on how to specify file local variables and what types of comments
;; you can place them in.

;; The mc-auto-encrypt function then gets evaluated when the file is
;; opened.  It sets up a before-save-hook and a buffer local
;; after-save-hook to handle the (d)encryption.

;; Known problems:

;; When you previously edited a file and adding the Local Variables 
;; without encrypting the file, this results in an error when opening
;; it with Emacs. The trick would be to check if the file has been
;; encrypted before trying to do so.

(require 'mailcrypt)

;; create a before-save-hook for auto encryption functions
(defvar before-save-hook nil
  "Hook run via defadvice before a buffer is saved.  This is a buffer
local hook.")
(make-local-hook 'before-save-hook)

(defadvice save-buffer (before crs-before-save-run-hooks)
  "Run before-save-hook before saving."
  (run-hooks 'before-save-hook))

(ad-activate 'save-buffer)

; this is the auto-encryption function called at the bottom of important files
(defun mc-auto-encrypt ()
  "Function called from the bottom of important files to handle gpg (d)encryption"
  (add-hook 'before-save-hook
            (lambda ()
              (mc-encrypt-region
               0 0 (save-excursion
                     (end-of-buffer)
                     (re-search-backward "Local Variables:" nil t)
                     (beginning-of-line)
                     (point)))) nil t)
  (add-hook 'after-save-hook
	    (lambda ()
	      (mc-decrypt)
	      (set-buffer-modified-p nil)) nil t)
  (mc-decrypt)
  (auto-save-mode nil)
  (set-buffer-modified-p nil))

(provide 'mc-auto-encrypt)

;;; Changes

;; 1.4 - corrected the function name, few other typos
;; 1.3 - released as a separate .el file
;; 1.2 - made the after-save-hook buffer local
;; 1.1 - improved handling of buffer modification with an after-save-hook
;; 1.0 - original version by Charles

;;; mc-auto-encrypt.el ends here
