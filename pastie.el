;;; pastie.el --- Emacs integration for pastie.org

;; Copyright (C) 2006  Christian Neukirchen <purl.org/net/chneukirchen>
;; Authors: Christian Neukirchen
;; URL: http://www.emacswiki.org/emacs/pastie.el
;; Created: 2007
;; Version: 20091230
;; Keywords: pastie webservices
;;

;;; Commentary:
;;
;; This file is NOT part of GNU Emacs.
;; Licensed under the GPL.

;;; Documentation:
;;
;; M-x pastie-buffer - Paste the entire buffer to pastie.org. Pastie.el will
;;                     attempt to autodetect the language by the current
;;                     active major-mode. Place the URL on the kill ring.
;; M-x pastie-region - Same as above but paste the current selected region only.
;; M-x pastie-browse - Open the last pastie in a browser using (browse-url)

;;; Change Log:
;;
;; 2007-12-27 Updated to work with more recent changes to the pastie API. 
;;            (Rob Christie)
;; 2007-12-30 Added more major mode sniffs (Ryan McGeary)
;; 2007-12-31 Added some minor mode sniffs that are Rails specific. 
;;            (Rob Christie)
;; 2008-01-07 Added pastie-browse (Dan McKinley)
;; 2008-06-01 Added support for private pastes, js2-mode, and a timeout. 
;;            (Dan McKinley)
;; 2008-08-04 Fixed xml escaping. Fixed naming conflict with pastie-buffer.
;;            Updated for the new pastie.org domain. (Dan McKinley)
;; 2008-12-09 Allow pastie.el to compile without warnings and add
;;            prefix argument toggling of restricted posting. (Peter Jones)
;; 2009-12-30 packaged up for elpa, documentation (Tim Harper)

;;; Code:
(eval-when-compile
  (require 'cl))

(defgroup pastie nil
  "Interface to pastie.org"
  :tag "Pastie" :group 'applications)

(defcustom *pastie-restricted* t
  "When non-nil, creates private pastes."
  :type 'boolean :group 'pastie)

(defcustom *pastie-timeout* 10
  "The time, in seconds, to wait for a pastie request."
  :type 'integer :group 'pastie)

(defvar *pastie-last-url* ""
  "The last url pasted.")

(defvar *pastie-buffer* nil
  "The buffer used to show fetched pastes.")

(defun pastie-language ()
  "Sniffs for the language of the region that is being pasted"
  (or (when (boundp 'rails-view-minor-mode) 
	(if rails-view-minor-mode "html_rails"))
      (when (boundp 'rails-minor-mode) (if rails-minor-mode "ruby_on_rails"))
      (cdr (assoc major-mode '((c-mode . "c++")
                               (c++-mode . "c++")
                               (css-mode . "css")
                               (diff-mode . "diff")
                               (html-mode . "html")
                               (java-mode . "java")
			       (python-mode . "python")
                               (javascript-mode . "javascript")
			       (js2-mode . "javascript")
                               (jde-mode . "java")
                               (php-mode . "php")
                               (ruby-mode . "ruby")
                               (text-mode . "plain_text")
                               (sql-mode . "sql")
                               (sh-mode . "shell-unix-generic"))))
      "plain_text"))

(defun pastie-url-format ()
  (if *pastie-restricted* "http://pastie.org/private/%s"
    "http://pastie.org/paste/%s"))

;;;###autoload
(defun pastie-region (begin end &optional toggle-restricted)
  "Post the current region as a new paste at pastie.org.
Copies the URL into the kill ring.

With a prefix argument, toggle the current value of
`*pastie-restricted*'."
  (interactive "r\nP")
  (let* ((body-raw (buffer-substring-no-properties begin end))
         (body (replace-regexp-in-string
                "[<>&]"
                (lambda (match)
                  (case (string-to-char match)
                    (?< "<")
                    (?> ">")
                    (?& "&")))
                body-raw))
         (mode (pastie-language))
         (url-request-method "POST")
         (url-mime-accept-string "application/xml")
         (url-request-extra-headers '(("Content-Type" . "application/xml")))
         (url (url-generic-parse-url "http://pastie.org/pastes"))
         (*pastie-restricted* (if toggle-restricted (not *pastie-restricted*)
                                *pastie-restricted*))
         (url-request-data
          (concat "<paste>"
                  "<parser>" mode "</parser>"
                  "<authorization>burger</authorization>"
                  (when *pastie-restricted* "<restricted>1</restricted>")
                  "<body>" body "</body>"
                  "</paste>")))
    (with-timeout (*pastie-timeout* (error "Pastie timed out."))
      (pastie-retrieve url))))

(defun pastie-retrieve (url)
  "Submits the request and validates the response."
  (let ((*pastie-buffer* (url-retrieve-synchronously url)))
    (with-current-buffer *pastie-buffer*
      (goto-char (point-min))
      (search-forward-regexp "^Status: \\([0-9]+.*\\)")
      (let ((status (match-string 1)))
	(if (string-match "^20[01]" status)
	    (progn
	      (goto-char (point-max))
	      (beginning-of-line)
	      (let ((id (buffer-substring (point) (point-max))))
		(let ((url (format (pastie-url-format) id)))
		  (message "Paste created: %s" url)
		  (setq *pastie-last-url* url)
		  (kill-new url))))
	  (message "Error occured: %s" status))))
    (kill-buffer *pastie-buffer*)))


;;;###autoload
(defun pastie-buffer (&optional toggle-restricted)
  "Post the current buffer as a new paste at pastie.org.
Copies the URL into the kill ring.

With a prefix argument, toggle the current value of
`*pastie-restricted*'."
  (interactive "P")
  (pastie-region (point-min) (point-max) toggle-restricted))

;;;###autoload
(defun pastie-get (id)
  "Fetch the contents of the paste from pastie.org into a new buffer."
  (interactive "nPastie #: ")

  (let ((url-request-method "GET")
        (url-request-extra-headers nil)
        (url-mime-accept-string "*/*")
        (url (url-generic-parse-url
              (format "http://pastie.org/pastes/%s/download" id))))
    (setq *pastie-buffer* (url-retrieve-synchronously url))

    (with-current-buffer *pastie-buffer*
      (goto-char (point-min))
      (search-forward-regexp "^Status: \\([0-9]+.*\\)")
      (let ((status (match-string 1)))
        (if (string-match "^200" status)
            (progn
              (search-forward-regexp
               "^Content-Disposition: attachment; filename=\"\\(.*\\)\"")
              (set-visited-file-name (match-string 1))
              (search-forward-regexp "\n\n")
              (delete-region (point-min) (point))
              (normal-mode)
              (set-buffer-modified-p nil)
              (switch-to-buffer *pastie-buffer*))
          (message "Error occured: %s" status)
          (kill-buffer *pastie-buffer*))))))

;;;###autoload
(defun pastie-browse ()
  (interactive)
  (browse-url *pastie-last-url*))

(provide 'pastie)

;;; pastie.el ends here
