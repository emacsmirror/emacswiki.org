;;; http-post-simple.el --- HTTP POST requests using the url library

;; Author: Tom Schutzer-Weissmann
;; Keywords: comm, data, processes, hypermedia

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as1
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:

;; Provides ways to use the url library to perform HTTP POST requests.
;; See the documentation to `http-post-simple' for more information.
;;
;; The `url-http' library does not handle 1xx response codes.

;; However, as RFC 2616 puts it:
;;     a server MAY send a 100 (Continue)
;;     status in response to an HTTP/1.1 PUT or POST request that does
;;     not include an Expect request-header field with the "100-continue"
;;     expectation.
;;
;; -- and some servers do, giving you annoying errors. To avoid these errors,
;; you can either set `url-http-version' to "1.0", in which case any compliant
;; server will not send the 100 (Continue) code, or call
;; `http-post-finesse-code-100'. Note that the latter advises
;; 'url-http-parse-response'
;;
;;; Change Log:

;; 11/06/2008 Set `url-http-version' to "1.0" when posting.
;; 19/07/2008 Don't set special variables like `url-http-version' and
;;	      `url-http-attempt-keepalives'.
;; 03/11/2008 Tell the server what charset we're using & accepting.

;;; Code:
(require 'url)
(require 'url-http)
(require 'cl)

(defun http-post-simple (url fields &optional charset)
  "Send FIELDS to URL as an HTTP POST request, returning the response
and response headers.
FIELDS is an alist, eg ((field-name . \"value\")); all values
need to be strings, and they are encoded using CHARSET,
which defaults to 'utf-8"
  (http-post-simple-internal
   url
   (http-post-encode-fields fields charset)
   charset
   `(("Content-Type"
      .
      ,(http-post-content-type
        "application/x-www-form-urlencoded"
        charset)))))


(defun http-post-simple-multipart (url fields files &optional charset)
  "Send FIELDS and FILES to URL as a multipart HTTP POST, returning the
response and response headers.
FIELDS is an alist, as for `http-post-simple', FILES is an a list of
\(fieldname \"filename\" \"file MIME type\" \"file data\")*"
(let ((boundary (http-post-multipart-boundary)))
  (http-post-simple-internal
   url
   (http-post-encode-multipart-data fields files charset)
   charset
   `(("Content-Type"
      .
      ,(http-post-content-type
        (format "multipart/form-data; boundary=%S" boundary)
        charset))))))


(defun http-post-content-type (content-type &optional charset)
  (if charset
      (format "%s; charset=%s" content-type (http-post-charset-name charset))
      content-type))


(defun http-post-charset-name (charset)
  (symbol-name charset))


;; based on `http-url-encode' from the from http-get package
;; http://savannah.nongnu.org/projects/http-emacs
(defun http-post-encode-string (str content-type)
  "URL encode STR using CONTENT-TYPE as the coding system."
  (apply 'concat
	 (mapcar (lambda (c)
		   (if (or (and (>= c ?a) (<= c ?z))
			   (and (>= c ?A) (<= c ?Z))
			   (and (>= c ?0) (<= c ?9)))
		       (string c)
		       (format "%%%02x" c)))
		 (encode-coding-string str content-type))))


(defun http-post-encode-fields (fields &optional charset)
  "Encode FIELDS using `http-post-encode-string', where
FIELDS is an alist of \(
	\(field-name-as-symbol . \"field value as string\"\) |
	\(field-name \"value1\" \"value2\" ...\)
	\)*

CHARSET defaults to 'utf-8"
  (let ((charset (or charset 'utf-8)))
    (mapconcat #'identity
	       (mapcar '(lambda (field)
			 (concat (symbol-name (car field))
			  "="
			  (http-post-encode-string (cdr field) charset)))
		       (mapcan '(lambda (field)
				 (if (atom (cdr field)) (list field)
				     ;; unpack the list
				     (mapcar '(lambda (value)
					       `(,(car field) . ,value))
					     (cdr field))))
			       fields))
	       "&")))


(defun http-post-simple-internal (url data charset extra-headers)
  (let ((url-request-method        "POST")
	(url-request-data          data)
	(url-request-extra-headers extra-headers)
        (url-mime-charset-string   (http-post-charset-name charset)))
    (let (header
	  data
	  status)
      (with-current-buffer
	  (url-retrieve-synchronously url)
	;; status
	(setq status url-http-response-status)
	;; return the header and the data separately
	(goto-char (point-min))
	(if (search-forward-regexp "^$" nil t)
	    (setq header (buffer-substring (point-min) (point))
		  data   (buffer-substring (1+ (point)) (point-max)))
	    ;; unexpected situation, return the whole buffer
	    (setq data (buffer-string))))
      (values data header status))))


(defun http-post-multipart-boundary ()
  "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=")


(defun http-post-bound-field (&rest parts)
  (let ((boundary (format "--%s" (http-post-multipart-boundary))))
    (http-post-join-lines  boundary parts)))


(defun http-post-encode-multipart-data (fields files charset)
  "Return FIELDS and FILES encoded for use as the data for a multipart HTTP POST request"
  (http-post-join-lines
   (mapcar '(lambda (field)
	     (http-post-bound-field
	      (format "Content-Disposition: form-data; name=%S" (symbol-name (car field)))
	      ""
	      (cdr field)))
	   fields)
   (mapcan '(lambda (file)
	     (destructuring-bind (fieldname filename mime-type data) file
	       (http-post-bound-field
		(format "Content-Disposition: form-data; name=%S; filename=%S" fieldname filename)
		(format "Content-type: %s" (http-post-content-type mime-type charset))
		""
		data)))
	   files)
   (format "--%s--" (http-post-multipart-boundary))))


(defun http-post-join-lines (&rest bits)
  (let ((sep "\r\n"))
    (mapconcat (lambda (bit)
		 (if (listp bit)
		     (apply 'http-post-join-lines bit)
		     bit))
	       bits sep)))


(defun http-post-finesse-code-100 ()
  "Transforms response code 100 into 200, to avoid errors when the
server sends code 100 in response to a POST request."
  (defadvice url-http-parse-response (after url-http-parse-response-100 activate)
    "Turns any HTTP 100 response code to 200, to avoid getting an error."
    (declare (special url-http-response-status
                      url-request-method))
    (when (and (= 100               url-http-response-status)
               (string-equal "POST" url-request-method)
               (string-equal "1.1"  url-http-version))
      (setf url-http-response-status 200))))


(provide 'http-post-simple)
;;; http-post-simple.el ends here
