;;; mc-gpg-file-mode.el - automatically gpg decrypt / encrypt file
;;; when opening / saving files ending with '.gpg'

;; Copyright (c) 2004 Josef Bauer
;;
;; Author: Josef Bauer
;;
;; Contributors: this was partly inspired by mc-auto-encrypt.el as
;;               found on http://www.emacswiki.org
;;               
;; Version: 0.1 of 2004-08-04
;;
;; There are plans to provide this file on  http://www.emacswiki.org

;; This file is not part of GNU Emacs.

;; mc-gpg-file-mode.el is free software; you can redistribute it
;; and/or modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.

;; mc-gpg-file-mode.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; mc-gpg-file-mode.el is free software


;; Abstract:
;; 
;; This mode helps you to handle gpg encrypted files in emacs
;; comfortably. This can be useful to store secrets (passwords etc.)
;; When you open a file with ending 'gpg' it gets decrypted and
;; displayed in emacs and it gets encrypted just before you save
;; it. Similar to auto-compression-mode for '.gz' files just that you
;; need your pass-phrase for decryption.

;; How to use this:
;; 
;; * make sure that mailcrypt is installed and functional
;;
;; * Put this file in a place where emacs can find it. e.g. put this
;;   in your .emacs file:
;;
;;   (setq load-path (cons (expand-file-name "~/sw/share/emacs/site-lisp") load-path)) 
;;
;;   and put this file in the directory ~/sw/share/emacs/site-lisp
;;
;; * Put this in your .emacs file:
;;
;;   (require 'mailcrypt)
;;   (mc-setversion "gpg")
;;   (require 'mc-gpg-file-mode)
;;
;; * Optionally put something like this in your .emacs if your gpg
;;   user id does not match your account name or such
;;
;;   (setq mc-gpg-user-id "josef.bauer.nospam@web.de")
;;
;; * Just open a gpg encrypted file in emacs and 'hope' that it gets
;;   decrypted after loading

;; Dependencies:
;;
;; mailcrypt as found at http://mailcrypt.sourceforge.net. This
;; version of mc-gpg-file-mode should work with mailcrypt version
;; 3.5.8.

;; Compatability:
;;
;; This version of mc-gpg-file-mode was tested with (GNU) emacs
;; 21.3.1, gpg 1.2.4 and mailcrypt 3.5.8 on linux 2.6 (SuSE 9.1).

;; History:
;;
;; For a long time I used an encrypted file for passwords and such
;; that I manually en- and decrypted using mailcrypt in emacs. Then I
;; started to write a generic mode that decrypted the buffer after
;; loading. When I found mc-auto-encrypt.el I extended my mode to
;; automatically encrypt the file before writing.

(require 'mailcrypt)

(define-generic-mode 'mc-gpg-file-mode
  (list ?#) 
  nil nil
  '(".gpg\\'" ".gpg-encrypted\\'")
  (list (lambda () 
	    (add-hook 'local-write-file-hooks
		          (lambda () 
			    (setq mc-pgp-always-sign 'never)
			    (mc-gpg-encrypt-region 
			     (list mc-gpg-user-id) 
			     (point-min) (point-max) 0)
			    nil nil) nil t)
	    (add-hook 'after-save-hook 
		      (lambda () (mc-decrypt)
			(set-buffer-modified-p nil)
			(auto-save-mode nil))
		      nil t)
	    (mc-decrypt)
	    (auto-save-mode nil)
	    (set-buffer-modified-p nil))
	)
  "Mode for gpg encrypted files"
  )

(provide 'mc-gpg-file-mode)

;;; mc-gpg-file-mode.el ends here
