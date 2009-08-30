;;; ipp.el --- implementation of the Internet Printing Protocol
;;;
;;; Author: Eric Marsden <emarsden@laas.fr>
;;; Version: 0.5
;;; Keywords: printing
;;; Copyright: (C) 2001  Eric Marsden
;;
;;     This program is free software; you can redistribute it and/or
;;     modify it under the terms of the GNU General Public License as
;;     published by the Free Software Foundation; either version 2 of
;;     the License, or (at your option) any later version.
;;
;;     This program is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;     GNU General Public License for more details.
;;
;;     You should have received a copy of the GNU General Public
;;     License along with this program; if not, write to the Free
;;     Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;;     MA 02111-1307, USA.
;;
;; The latest version of this package should be available from
;;
;;     <URL:http://purl.org/net/emarsden/home/downloads/>

;;; Commentary:

;; The Internet Printing Protocol is intended to replace the LPD
;; protocol for interacting with network printers. It specifies
;; mechanisms for querying the capabilities of a printer, submitting
;; and cancelling jobs, and queue monitoring.
;;
;; You can find out whether a device is IPP-capable by trying to
;; telnet to port 631. If it accepts the connection it probably
;; understands IPP. You then need to discover the path component of
;; the URI, for example by reading the documentation or from a driver
;; program. Tested or reported to work on the following devices:
;;
;;   * Tektronix Phaser 750, with an URI of the form ipp://host:631/
;;     (empty path component)
;;
;;   * HP Laserjet 4000, with a path component of /ipp/port1.
;;
;;   * Xerox Document Centre 460 ST, with empty path component.
;;
;;   * CUPS printer spooler (see <URL:http://www.cups.org/>).
;;
;;
;;
;; Usage: load this package by putting in your ~/.emacs.el
;;
;;    (require 'ipp)
;;
;; then try printing a file using 'M-x ipp-print'. This will prompt
;; you for a file name (which should be in a format understood by the
;; printer, such as Postscript), and the URI of the printer. There are
;; also two functions for querying the capability of the device
;; `ipp-get-attributes' and examining its queue `ipp-get-jobs'. Until
;; I write display code for these functions you will have to call them
;; from the *scratch* buffer with C-j to examine their return value.
;;
;;
;; The IPP network protocol is based on HTTP/1.1 POST requests, using
;; a special "application/ipp" MIME Content-Type. The data is encoded
;; using simple marshalling rules.
;;
;; The Internet Printing Protocol is described in a number of RFCs:
;;   <URL:http://www.faqs.org/rfcs/rfc2565.html>
;;   <URL:http://www.faqs.org/rfcs/rfc2566.html>
;;   <URL:http://www.faqs.org/rfcs/rfc2568.html>
;;
;; and the Printer Working Group maintain a page at
;;
;;   <URL:http://www.pwg.org/ipp/>
;;
;;
;; Eventually it would be nice to modify the Emacs printing API to
;; support this type of direct printing, so that a user could set
;; `ps-printer-name' to "ipp://modern-printer:631/" or
;; "lpd://ancient-printer/queue" (it would be easy to write a package
;; similar to this one implementing the LPD protocol at the network
;; level; the LDP protocol is very simple).
;;
;; See also printing.el at <URL:http://www.cpqd.br/~vinicius/emacs/>.
;;
;;
;; Thanks to Vinicius Jose Latorre for patches and Colin Marquardt and
;; Andrew Cosgriff for help in debugging.

;;; Code:

(require 'cl)


(defstruct ipp-reply
  status
  request-id
  attributes)

(unless (fboundp 'char-int)
  (fset 'char-int 'identity))

(defun ipp-value-tag-p (tag)
  (and (<= ?\x10 tag)
       (<= tag ?\xFF)))

;; should really make the port optional defaulting to 631
(defun ipp-parse-uri (uri)
  "Parse an URI of the form ipp://host:631/ipp/port1
into values host, port, path."
  (unless (string-match "^ipp://\\([^:]+\\):\\([0-9]+\\)/\\(.*\\)$" uri)
    (error "Invalid URI %s" uri))
  (values (match-string 1 uri)
          (string-to-number (match-string 2 uri))
          (concat "/" (match-string 3 uri))))

;; attribute = value-tag name-length name value-length value
(defun ipp-demarshal-name-value (reply)
  (let (value-tag name-length name value-length value)
    (setq value-tag (char-int (char-after (point))))
    (when (ipp-value-tag-p value-tag)
      (forward-char)
      (setq name-length (ipp-demarshal-int 2)
            name (ipp-demarshal-string name-length)
            value-length (ipp-demarshal-int 2)
            value (ipp-demarshal-string value-length))
      (push (list name value value-tag) (ipp-reply-attributes reply)))))

;; see rfc2565 section 3.2
(defun ipp-demarshal-attribute (reply)
  (let ((tag (char-int (char-after (point)))))
    (forward-char)
    (cond ((= tag 3)                    ; end-of-attributes-tag
           nil)
          ;; xxx-attributes-tag *(attribute *additional-values)
          ((member tag '(1 2 4 5))
           (ipp-demarshal-name-value reply)
           t)
          ;; we're still in *(attribute *additional-values)
          ((ipp-value-tag-p tag)
           (backward-char)
           (ipp-demarshal-name-value reply))
          (t (error "unknown IPP attribute tag %s" tag)))))

(defun ipp-demarshal-attributes (reply)
  (loop while (ipp-demarshal-attribute reply)))

(defun ipp-demarshal-string (octets)
  (forward-char octets)
  (buffer-substring (- (point) octets) (point)))

(defun ipp-demarshal-int (octets)
  (do ((i octets (- i 1))
       (accum 0))
      ((zerop i) accum)
    (setq accum (+ (* 256 accum) (char-after (point))))
    (forward-char)))

(defun ipp-make-http-header (uri octets)
  (multiple-value-bind (host port path)
      (ipp-parse-uri uri)
    (concat "POST " path " HTTP/1.1\r\n"
          (format "Host: %s:%s\r\n" host port)
          "Content-Type: application/ipp\r\n"
          (format "Content-Length: %d\r\n" octets))))

;; a length is two octets
(defun ipp-length (str)
  (let ((octets (length str)))
    (string (/ octets 256) (mod octets 256))))

(defun ipp-attribute (type name value)
  (concat (string type)
          (ipp-length name)
          name
          (ipp-length value)
          value))

(defun ipp-marshal-printer-attributes-request (printer-uri)
  (concat (string 1 0)                  ; version as major/minor
          (string 0 ?\xB)               ; operation-id
          (string 0 0 0 ?e)             ; request-id as 4 octets
          (string 1)                    ; operation-attributes-tag
          (ipp-attribute ?\x47 "attributes-charset" "utf-8")
          (ipp-attribute ?\x48 "attributes-natural-language" "C")
          (ipp-attribute ?\x45 "printer-uri" printer-uri)
          (ipp-attribute ?\x42 "requesting-user-name" (user-login-name))
          (string 3)))                  ; end-of-attributes-tag

(defun ipp-marshal-get-jobs-request (printer-uri)
  (concat (string 1 0)                  ; version as major/minor
          (string 0 ?\xA)               ; operation-id
          (string 0 0 0 ?e)             ; request-id as 4 octets
          (string 1)                    ; operation-attributes-tag
          (ipp-attribute ?\x47 "attributes-charset" "utf-8")
          (ipp-attribute ?\x48 "attributes-natural-language" "C")
          (ipp-attribute ?\x45 "printer-uri" printer-uri)
          (ipp-attribute ?\x42 "requesting-user-name" (user-login-name))
          (string 3)))                  ; end-of-attributes-tag

(defun ipp-marshal-print-job-header (printer-uri)
  (concat (string 1 0)                  ; version as major/minor
          (string 0 2)                  ; operation-id: 2 == print-job
          (string 0 0 0 ?e)             ; request-id as 4 octets
          (string 1)                    ; operation-attributes-tag
          (ipp-attribute ?\x47 "attributes-charset" "utf-8")
          (ipp-attribute ?\x48 "attributes-natural-language" "C")
          (ipp-attribute ?\x45 "printer-uri" printer-uri)
          (ipp-attribute ?\x42 "job-name" "My Fine Job")
          (ipp-attribute ?\x42 "requesting-user-name" (user-login-name))
          (string 3)))                  ; end-of-attributes-tag

(defmacro ipp-marshal-print-job-request (printer &rest body)
  `(let ((buf (get-buffer-create " *ipp-print-job*")))
     (save-excursion
       (set-buffer buf)
       (erase-buffer)
       (insert (ipp-marshal-print-job-header ,printer))
       ,@body
       (buffer-string))))

(defun ipp-marshal-print-job-request-file (printer filename)
  (ipp-marshal-print-job-request
   printer
   (insert-file-contents-literally filename)))

(defun ipp-marshal-print-job-request-region (printer buffer
						     &optional start end)
  (ipp-marshal-print-job-request
   printer
   (insert-buffer-substring buffer start end)))

(defun ipp-open (printer-uri)
  (multiple-value-bind (host port)
      (ipp-parse-uri printer-uri)
    (let* ((buf (generate-new-buffer " *ipp connection*"))
           (proc (open-network-stream "ipp" buf host port)))
    (buffer-disable-undo buf)
    (when (fboundp 'set-buffer-process-coding-system)
      (save-excursion
        (set-buffer buf)
        (set-buffer-process-coding-system 'binary 'binary)
        (set-buffer-multibyte nil)))
    proc)))

(defun ipp-close (connection)
  ;; FIXME add delete-process
  )

(defun ipp-send (proc &rest args)
  (dolist (arg args)
    (process-send-string proc arg)
    (accept-process-output)))

(defun ipp-decode-reply (conn)
  (let ((buf (if (bufferp conn) conn (process-buffer conn)))
        (reply (make-ipp-reply)))
    ;; wait for the connection to close
    (when (processp conn)
      (loop until (eq (process-status conn) 'closed)
            do (accept-process-output conn 5)))
    (save-excursion
      (set-buffer buf)
      (goto-char (point-min))
      (when (re-search-forward "^HTTP/1.[01] 501" nil t)
        (error "Unimplemented IPP request"))
      (goto-char (point-min))
      (when (re-search-forward "^HTTP/1.[01] 403" nil t)
        (error "Access forbidden"))
      (goto-char (point-min))
      (if (search-forward "HTTP/1.1 100" nil t)
          (end-of-line 2)
        (goto-char (point-min)))
      (unless (search-forward (string 13 10 13 10) nil t)
        (error "Malformed IPP reply"))
      (unless (looking-at (string 1 0))
        (error "Unknown IPP protocol version"))
      (forward-char 2)
      (setf (ipp-reply-status reply) (ipp-demarshal-int 2))
      (setf (ipp-reply-request-id reply) (ipp-demarshal-int 4))
      (ipp-demarshal-attributes reply))
    reply))

(defun ipp-get (printer request)
  "Get REQUEST from IPP-capable network device PRINTER.
The printer name should be of the form ipp://host:631/ipp/port1."
  (let ((connection (ipp-open printer)))
    (ipp-send connection
	      (ipp-make-http-header printer (length request))
	      "\r\n"
	      request)
    (ipp-decode-reply connection)
    (ipp-close connection)))

;; these are not autoloaded, since they need some sort of widget-based
;; interface to present the information.
(defun ipp-get-attributes (printer)
  "Get attributes from IPP-capable network device PRINTER.
The printer name should be of the form ipp://host:631/ipp/port1."
  (ipp-get printer
	   (ipp-marshal-printer-attributes-request printer)))

(defun ipp-get-jobs (printer)
  "Get running jobs at IPP-capable network device PRINTER.
The printer name should be of the form ipp://host:631/ipp/port1."
  (ipp-get printer
	   (ipp-marshal-get-jobs-request printer)))

(defun ipp-print (printer content)
  "Print CONTENT to IPP-capable network device PRINTER.
CONTENT must be in a format understood by your printer, so probably Postscript
or PCL.
The printer name should be of the form ipp://host:631/ipp/port1."
  (let ((connection (ipp-open printer)))
    (ipp-send connection
	      (ipp-make-http-header printer (length content))
	      "\r\n"
	      content)
    (ipp-close connection)))

;;;###autoload
(defun ipp-print-file (filename printer)
  "Print FILENAME to the IPP-capable network device PRINTER.
FILENAME must be in a format understood by your printer, so probably Postscript
or PCL.
The printer name should be of the form ipp://host:631/ipp/port1."
  (interactive "fIPP print file: \nsPrinter URI: ")
  (ipp-print printer
	     (ipp-marshal-print-job-request-file printer filename)))

;;;###autoload
(defun ipp-print-region (buffer printer &optional start end)
  "Print BUFFER region from START to END to IPP-capable network device PRINTER.
BUFFER must be in a format understood by your printer, so probably Postscript
or PCL.
If START is nil, it defaults to beginning of BUFFER.
If END is nil, it defaults to end of BUFFER.
The printer name should be of the form ipp://host:631/ipp/port1."
  (interactive "bIPP print buffer (region): \nsPrinter URI: ")
  (ipp-print printer
	     (ipp-marshal-print-job-request-region printer buffer start end)))

;;;###autoload
(defun ipp-print-buffer (buffer printer)
  "Print BUFFER to IPP-capable network device PRINTER.
BUFFER must be in a format understood by your printer, so probably Postscript
or PCL.
The printer name should be of the form ipp://host:631/ipp/port1."
  (interactive "bIPP print buffer: \nsPrinter URI: ")
  (ipp-print-region buffer printer))


(provide 'ipp)

;;; ipp.el ends here
