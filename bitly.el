;;;  bitly.el --- Shorten URLs with bit.ly from emacs
;;   Also available at https://gist.github.com/716717.
;; Copyright (C) 2010 Vivek Haldar
;;
;;  This program is free software; you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation; either version 2 of the License, or
;;  (at your option) any later version.
;;
;;  This program is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.
;;
;;  You should have received a copy of the GNU General Public License
;;  along with this program; if not, write to the Free Software
;;  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
;;
;; Author: Vivek Haldar <vh@vivekhaldar.com>
;; Created: 27 November 2010
;;
;;; Commentary: 
;; bitly.el allows shortening URLs through the api the bit.ly service
;; provides. See http://code.google.com/p/bitly-api/ for info about
;; the tumblr.com api service.
;;
;; Installation:
;; You will need to register with bit.ly and get an API key. Customize
;; the variables bitly-username and bitly-api-key.
;;
;; Description:
;; Interactive functions:
;; bitly-shorten
;;     Shorten a URL (prompted for in the minibuffer) and insert at
;;     point.

(require 'url)

(defcustom bitly-username nil
  "Bitly username."
  :type '(choice
          (const :tag "none" nil)
          (string :tag "bitly_username"))
  :group 'bitly)

(defcustom bitly-api-key nil
  "Bitly API key."
  :type '(choice
          (const :tag "none" nil)
          (string :tag "bitly_api_key"))
  :group 'bitly)

(defvar bitly-api-url "http://api.bit.ly/v3/")

(defun bitly-shorten ()
  "Shorten a full URL using Bitly, and insert at point."
  (interactive)
  (setq longurl (read-from-minibuffer "URL:"))
  (let* ((apicall 
	  (concat bitly-api-url
		  (format "shorten?login=%s&apiKey=%s&longUrl=%s&format=txt"
			  bitly-username bitly-api-key longurl)))
	 (resultbuffer (url-retrieve-synchronously apicall)))
    (message "bitly url = %s" apicall)
    (bitly-strip-http-headers resultbuffer)
    (insert-buffer-substring resultbuffer)
    ))


(defun bitly-strip-http-headers (httpbuffer)
  "Strip headers from HTTP reply."
  (save-excursion 
    (set-buffer httpbuffer)
    (goto-char (point-min))
    (let ((endpt (search-forward "

")))
      (delete-region (point-min) endpt)
      ; we're still left with an extra newline
      (goto-char (point-max))
      (delete-backward-char 1)
      (insert-string " "))))

(provide 'bitly)
