;;; openpaste.el -- Emacs interface for OpenPastebin

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

;;;; Installation:
;;
;; Put this code in your .emacs
;;  (autoload 'openpaste-region "openpaste")
;;  (define-key global-map (kbd "C-x p") 'openpaste-region)

;;;; Depends on:
;;   -  xml-rpc  (http://www.emacswiki.org/cgi-bin/wiki/XmlRpc)


(require 'xml-rpc)


(defvar openpaste-url-server "http://openpaste.org/api/xml-rpc/"
  "XML-RPC URL Service")

(defvar openpaste-author     (user-login-name)
  "Author of post to commit")

(defvar openpaste-secret-key  nil
  "Private password is neccesary to you can erase your posts
  later. It should be nil or a string.")

(defvar openpaste-public-key  nil
  "Any user who want to read your posts should know this
  password. It could be a string itself, t in order to emacs
  ask you a password or nil if you don't want one.")


(defmacro openpaste-syntax-to-alist (syntax-table)
  `(mapcar (lambda (e) (cons (cdr (nth 1 e))
			     (cdr (nth 0 e))))
	   ,syntax-table))



(defun openpaste-region (start end)
  "Post a post on openpaste-url-server"
  (interactive "r")

  (let* ((syntax-list (openpaste-syntax-to-alist
		       (xml-rpc-method-call openpaste-url-server
					    'Openpaste.getSupportedSyntax)))
	 (syntax (cdr (assoc (completing-read "Syntax: " syntax-list) syntax-list)))
	 (desc   (read-string "Description: "))
	 (secret-key openpaste-secret-key)
	 (public-key (cond
		      ((stringp openpaste-public-key) openpaste-public-key)
		      ((not (null openpaste-public-key)) (read-passwd "Password: "))
		      (t nil))))

    (if (null syntax)
	(error "Syntax not supported"))

    (kill-new
     (cdr (assoc "URL"
		 (xml-rpc-method-call openpaste-url-server
				      'Openpaste.addPost 
				      (buffer-substring-no-properties start end)
				      syntax
				      openpaste-author
				      desc
				      (if openpaste-public-key t nil)
				      (or public-key "")
				      (or openpaste-secret-key "")))))

    (message "Post commited. You can yank the url now")))


(provide 'openpaste)

;; openpaste.el ends here
