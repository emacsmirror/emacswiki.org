;;; moikrug.el --- (S)XEmacs interface to MoiKrug.

;;{{{ `-- Top Comments

;; Copyright (C) 2006-2007 by Zajcev Evgeny

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Keywords: bbdb
;; Created: Thu Jan 12 15:03:17 MSK 2006
;; Version: 1.2

;; This file is NOT part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; XEmacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Synched up with: Not in FSF

;;; Commentary:

;; This is the Emacs interface to MoiKrug (http://moikrug.ru) service.
;; It has only two commands:

;;   moikrug-bbdb-invite   - Invite person from bbdb to your MoiKrug.
;;   moikrug-bbdb-merge-in - Merge MoiKrug entries into bbdb.

;; moikrug.el depends on w3, net-utils, gnus and bbdb packages, make
;; sure they are installed.

;; Sample configuration might look like:

;;   (autoload 'moikrug-bbdb-invite "moikrug" nil t)
;;   (autoload 'moikrug-bbdb-merge-in "moikrug" nil t)

;;   (setq moikrug-login user-mail-address)

;;   (add-hook 'bbdb-mode-hook
;;             (lambda ()
;;               (define-key bbdb-mode-map [?u] 'moikrug-bbdb-merge-in)
;;               (define-key bbdb-mode-map [?i] 'moikrug-bbdb-invite)))

;;; Note:

;; File layout controlled by Emacs folding.el available at:
;; http://www.csd.uu.se/~andersl/emacs.shtml - Latest included in
;; XEmacs

;;; Thanks:

;;  - MoiKrug project for providing API.
;;  - Edward O'Connor for his soap.el and google.el code.

;;; Bugs:

;;  - Built-in UTF8<-->KOI8-R convertor.

;;}}}

;;; Code:

;;{{{ `-- Requires

(require 'cl)
(require 'parse-time)
(require 'time-date)
(require 'bbdb-com)
(require 'url)
(require 'xml)

;;}}}
;;{{{ `-- Customization

(defgroup moikrug nil
  "Group to customize moikrug."
  :prefix "moikrug-"
  :group 'tools)

(defcustom moikrug-login 'ask-once
  "Login for your account at MoiKrug."
  :type '(choice (const :tag "Ask everytime" ask)
                 (const :tag "Ask once" ask-once)
                 (string :tag "Set password"))
  :group 'moikrug)

(defcustom moikrug-password 'ask-once
  "Password for your account at MoiKrug."
  :type '(choice (const :tag "Ask everytime" ask)
                 (const :tag "Ask once" ask-once)
                 (string :tag "Set password"))
  :group 'moikrug)

(defcustom moikrug-sort-type 'modified
  "Default sorting type when merging in moikrug."
  :type '(choice (const :tag "By name" name)
                 (const :tag "Modification date" modified)
                 (const :tag "Number of contacts" contacts))
  :group 'moikrug)

(defcustom moikrug-invite-subject "ðÒÉÇÌÁÛÁÀ × íÏÊ ëÒÕÇ"
  "Subject for invite mail."
  :type 'string
  :group 'moikrug)

(defcustom moikrug-invite-message
  "ñ ×ÏÓÓÔÁÎÁ×ÌÉ×ÁÀ ÉÎÆÏÒÍÁÃÉÀ Ï ÚÎÁËÏÍÙÈ, ÞÔÏÂÙ × ÂÕÄÕÝÅÍ ÎÅ ÔÅÒÑÔØ Ó
ÎÉÍÉ Ó×ÑÚØ.

ðÒÉÇÌÁÛÁÀ ÐÒÉÓÏÅÄÉÎÉÔØÓÑ Ë íÏÅÍÕ ëÒÕÇÕ, ÞÔÏÂÙ ÍÙ ÉÍÅÌÉ ÄÏÓÔÕÐ Ë
ÐÒÏÆÅÓÓÉÏÎÁÌØÎÏÊ ËÏÎÔÁËÎÏÊ ÉÎÆÏÒÍÁÃÉÉ ÄÒÕÇ ÄÒÕÇÁ.

MoiKrug.ru ÐÏÚ×ÏÌÑÅÔ:

 - îÁÊÔÉ ÞÅÒÅÚ ÍÅÎÑ ÐÏÌÅÚÎÙÅ ÐÒÏÆÅÓÓÉÏÎÁÌØÎÙÅ ËÏÎÔÁËÔÙ É ÎÁÛÉÈ ÏÂÝÉÈ
   ÚÎÁËÏÍÙÈ

 - äÏÂÁ×ÉÔØ Ó×ÏÅ ÒÅÚÀÍÅ É ÚÎÁËÏÍÙÈ × ËÏÎÆÉÄÅÎÃÉÁÌØÎÕÀ ÚÁÐÉÓÎÕÀ ËÎÉÖËÕ
   online, ËÏÔÏÒÁÑ ×ÓÅÇÄÁ ÂÕÄÅÔ ÓÏÄÅÒÖÁÔØ ÁËÔÕÁÌØÎÕÀ ÉÎÆÏÒÍÁÃÉÀ

÷ÏÔ ÞÔÏ Ï ÎÉÈ ÐÉÛÅÔ ÐÒÅÓÓÁ: http://moikrug.ru/press/"
  "Body for invite mail."
  :type 'string
  :group 'moikrug)

;;}}}


;;{{{ `-- SOAP

(defun soap-process-response (response-buffer)
  "Process the SOAP response in RESPONSE-BUFFER."
  (let ((retval nil))
    (with-current-buffer response-buffer
      (goto-char (point-min))
      (when (re-search-forward "^HTTP/1.* 200 OK\015?$" nil t)
        (re-search-forward "^\015?$" nil t 1)
        (setq retval (buffer-substring-no-properties (point) (point-max)))))
    (with-temp-buffer
      (insert "\n" retval "\n")
      (goto-char (point-min))
      (while (re-search-forward "\r" nil t)
        (replace-match ""))
      (xml-parse-region (point-min) (point-max)))))

(defun soap-request (url data)
  "Send and process SOAP request to URL with DATA."
  (let* ((url-request-extra-headers
          `(("Content-type" . "text/xml; charset=\"utf-8\"")
            ("SOAPAction" . ,(format "%S" url))))
         (url-request-method "POST")
         (url-request-data
          (concat "<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n"
                  data)))
    (let* ((url-inhibit-mime-parsing t)
           (buf (cdr (url-retrieve url))))
      (with-current-buffer buf
        (save-excursion
          (goto-char (point-min))
          (while (search-forward "\015" nil t)
            (replace-match ""))))
      (soap-process-response buf))))

;;}}}
;;{{{ `-- XML-SEXP

(defun moikrug-xml-sexp-attr-to-xml (attr-cons)
  "Convert S-exp ATTR-CONS to xml form."
  (let ((attr-name (car attr-cons))
        (attr-val (cdr attr-cons)))
    (unless (stringp attr-val)
      (setq attr-val (format "%s" attr-val)))
    (concat (format " %s=" attr-name)
            (if (string-match "[\"]" attr-val)
                (format "'%s'" attr-val)
              (format "\"%s\"" attr-val)))))

(defun moikrug-xml-sexp-to-xml (xml-sexp)
  "Return a string containing an XML representation of XML-SEXP."
  (cond ((null xml-sexp) "")
        ((stringp xml-sexp) xml-sexp)
        ((listp xml-sexp)
         (let ((tag (xml-node-name xml-sexp))
               (attrs (xml-node-attributes xml-sexp))
               (children (xml-node-children xml-sexp)))
           (concat (format "<%s" tag)
                   (if attrs
                       (mapconcat #'moikrug-xml-sexp-attr-to-xml attrs "")
                     "")
                   (if children
                       (concat
                        ">" (mapconcat #'moikrug-xml-sexp-to-xml children "")
                        (format "</%s>" tag))
                     "/>"))))
        (t (moikrug-xml-sexp-to-xml (format "%s" xml-sexp)))))

;;}}}

;;{{{ `-- Request

(defvar moikrug-gate "https://moikrug.ru/extras/alpha.ws"
  "Set it to \"http://moikrug.ru/extras/alpha.ws\" for plain/text request.")

(defconst moikrug-code "4276cd85815f6e639ce0037b3d025034"
  "Code for moikrug.el application, given by MoiKrug.")
  
(defun moikrug-send-request (xml-sexp)
  "Send XML-SEXP to MoiKrug as a request."
  (soap-request moikrug-gate (moikrug-xml-sexp-to-xml xml-sexp)))

(defun moikrug-request (msg-type &rest params)
  "Requst MoiKrug API.
MSG-TYPE is one of:
  'list - PARAMS are :login and :password
  'invite - PARAMS are :email, :firstname, :lastname, :message"
  (let ((login (if (stringp moikrug-login) moikrug-login
                 (read-string "MoiKrug login: " user-mail-address)))
        (passwd (if (stringp moikrug-password) moikrug-password
                  (read-passwd "MoiKrug password: "))))
    ;; Save login/password in case of ask-once
    (when (eq moikrug-login 'ask-once)
      (setq moikrug-login login))
    (when (eq moikrug-password 'ask-once)
      (setq moikrug-password passwd))

    (moikrug-send-request
     `(SOAP-ENV:Envelope
       ((xmlns:SOAP-ENV . "http://schemas.xmlsoap.org/soap/envelope/")
        (xmlns:xsi . "http://www.w3.org/1999/XMLSchema-instance")
        (xmlns:xsd . "http://www.w3.org/1999/XMLSchema"))
       (SOAP-ENV:Body
        ()
        ,(nconc (list (ecase msg-type
                        (list 'ns1:getFirstCircle)
                        (invite 'ns1:invitePerson))
                      (list (cons 'xmlns:ns1 "urn:MoikrugContacts")
                            (cons 'SOAP-ENV:encodingStyle
                                  "http://schemas.xmlsoap.org/soap/encoding/"))
                      (list 'code '((xsi:type . "xsd:string")) moikrug-code)
                      (list 'login '((xsi:type . "xsd:string")) login)
                      (list 'password '((xsi:type . "xsd:string")) passwd))
                (when (eq msg-type 'invite)
                  (list (list 'targetEmail '((xsi:type . "xsd:string"))
                              (plist-get params :email))
                        (list 'targetFirstname '((xsi:type . "xsd:string"))
                              (plist-get params :firstname))
                        (list 'targetLastname '((xsi:type . "xsd:string"))
                              (plist-get params :lastname))
                        (list 'message '((xsi:type . "xsd:string"))
                              (plist-get params :message)))))
        )))))

(put 'moikrug-request 'lisp-indent-function 'defun)

;;}}}

;;{{{ `-- UTF8 <--> KOI8-R convertor

;; GNU Emacs compatibility
(unless (fboundp 'char-to-int)
  (defalias 'char-to-int 'identity))
(unless (fboundp 'int-to-char)
  (defalias 'int-to-char 'identity))

(defvar moikrug-utf8-koi8-table
  (list [#x10 ?á] [#x11 ?â] [#x12 ?÷] [#x13 ?ç] [#x14 ?ä] [#x15 ?å]
        [#x16 ?ö] [#x17 ?ú] [#x18 ?é] [#x19 ?ê] [#x1A ?ë] [#x1B ?ì]
        [#x1C ?í] [#x1D ?î] [#x1E ?ï] [#x1F ?ð]

        [#x20 ?ò] [#x21 ?ó] [#x22 ?ô] [#x23 ?õ] [#x24 ?æ] [#x25 ?è]
        [#x26 ?ã] [#x27 ?þ] [#x28 ?û] [#x29 ?ý] [#x2A ??] [#x2B ?ù]
        [#x2C ?ø] [#x2D ?ü] [#x2E ?à] [#x2F ?ñ]

        [#x30 ?Á] [#x31 ?Â] [#x32 ?×] [#x33 ?Ç] [#x34 ?Ä] [#x35 ?Å]
        [#x36 ?Ö] [#x37 ?Ú] [#x38 ?É] [#x39 ?Ê] [#x3A ?Ë] [#x3B ?Ì]
        [#x3C ?Í] [#x3D ?Î] [#x3E ?Ï] [#x3F ?Ð]

        [#x40 ?Ò] [#x41 ?Ó] [#x42 ?Ô] [#x43 ?Õ] [#x44 ?Æ] [#x45 ?È]
        [#x46 ?Ã] [#x47 ?Þ] [#x48 ?Û] [#x49 ?Ý] [#x4A ??] [#x4B ?Ù]
        [#x4C ?Ø] [#x4D ?Ü] [#x4E ?À] [#x4F ?Ñ] [#x51 ?£]))

(defun moikrug-utf8-to-koi8-char (utf8-char)
  "Convert UTF8-CHAR to KOI8-R character."
  (let  ((ukl moikrug-utf8-koi8-table))
    (while (and ukl
                (not (= (aref (car ukl) 0) utf8-char)))
      (setq ukl (cdr ukl)))
    (or (and ukl
             (aref (car ukl) 1))
        ??)))

(defun moikrug-utf8-to-koi8 (utf8-string)
  "Convert UTF8-STRING to KOI8 string.

Here is how UTF-8 looks like

1:  0xxxxxxx
2:  110xxxxx 10xxxxxx
3:  1110xxxx 10xxxxxx 10xxxxxx
4:  11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
5:  111110xx 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx
6:  1111110x 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx

Supported types:

1:  0xxxxxxx
2:  110000xx 10xxxxxx

\\0400 - cyrillic"
  (if (featurep 'mule)
      (decode-coding-string utf8-string 'utf-8)
    (let ((rstr "") fc ulen)
      (while (not (string= utf8-string ""))
        (setq fc (aref utf8-string 0))
        (when (= (logand #xa0 fc) #xa0)
          (error "Unsupported utf8 character type %d" fc))
        (setq ulen (if (zerop (logand #x80 fc)) 1 2))

        (if (= ulen 1)
            (setq rstr (concat rstr (char-to-string fc))
                  utf8-string (substring utf8-string 1))
          (setq rstr (concat rstr
                             (char-to-string
                              (moikrug-utf8-to-koi8-char
                               (logior (lsh (logand #xf fc) 6)
                                       (logand #x3f (aref utf8-string 1))))))
                utf8-string (substring utf8-string 2))))
      rstr)))

(defun moikrug-koi8-to-utf8-char (koi8-char)
  "Convert KOI8-CHAR to UTF8 character."
  (let  ((ukl moikrug-utf8-koi8-table))
    (while (and ukl
                (not (= (aref (car ukl) 1) koi8-char)))
      (setq ukl (cdr ukl)))
    (or (and ukl
             (aref (car ukl) 0))
        ??)))

(defun moikrug-koi8-to-utf8 (koi8-string)
  "Convert KOI8-STRING to UTF8 string."
  (if (featurep 'mule)
      (encode-coding-string koi8-string 'utf-8)
    (mapconcat #'(lambda (k8c)
                   (if (zerop (logand (char-to-int k8c) #x80))
                       (char-to-string k8c)
                     (let ((cc (moikrug-koi8-to-utf8-char k8c)))
                       (concat (char-to-string
                                (int-to-char (logior 208 (lsh cc -6))))
                               (char-to-string
                                (int-to-char (logior 128 (logand 63 cc))))))))
               (string-to-list koi8-string) "")))

;;}}}
;;{{{ `-- Processing response

(defun moikrug-process-list (response &optional sort-type)
  "Process getFirstCircleResponse RESPONSE.
If SORT-TYPE is specified, then sort entries.  SORT-TYPE can be one of:
  'name     - Sort by name (default)
  'modified - Sort by recency of entry modification
  'contacts - Sort by number of contacts"
  (let* ((body (car (xml-get-children (car response) 'SOAP-ENV:Body)))
         (gfcr (car (xml-get-children body 'ns1:getFirstCircleResponse)))
         (return (car (xml-get-children gfcr 'return)))
         (items (xml-get-children return 'item))
         (retval '()))
    (while items
      (let (tits contact)
        (setq tits (xml-get-children (car items) 'item))
        (while tits
          (push (cons (intern (nth 2 (car (xml-get-children (car tits) 'key))))
                      (moikrug-utf8-to-koi8
                       (nth 2 (car (xml-get-children (car tits) 'value)))))
                contact)
          (setq tits (cdr tits)))
        (push (nreverse contact) retval))
      (setq items (cdr items)))

    ;; Sort the result according to
    (sort (nreverse retval)
          #'(lambda (c1 c2)
              (ecase sort-type
                ((name nil) nil)
                (modified
                 (let ((m1 (cdr (assq 'modified c1)))
                       (m2 (cdr (assq 'modified c2))))
                   (cond ((and m1 m2)
                          (> (time-to-seconds
                              (apply #'encode-time (parse-time-string m1)))
                             (time-to-seconds
                              (apply #'encode-time (parse-time-string m2)))))
                         ((and m1 (null m2)) t)
                         ((and (null m1) m2) nil))))
                (contacts (let ((cc1 (string-to-int
                                      (cdr (assq 'contacts_count c1))))
                                (cc2 (string-to-int
                                      (cdr (assq 'contacts_count c2)))))
                            (> cc1 cc2))))))))

(defun moikrug-process-invite (response)
  "Process invitePersonResponse RESPONSE."
  (let* ((body (car (xml-get-children (car response) 'SOAP-ENV:Body)))
         (gfcr (car (xml-get-children body 'ns1:invitePersonResponse)))
         (retval (car (xml-get-children gfcr 'return))))
    (nth 2 retval)))

(defun moikrug-process (msg-type response)
  "Process MSG-TYPE RESPONSE from MoiKrug."
  (ecase msg-type
    (list (moikrug-process-list response))
    (invite (moikrug-process-invite response))))

;;}}}

;;{{{ `-- BBDB Commands

;;;###autoload
(defun moikrug-bbdb-invite (record)
  "Invite person stored in RECORD."
  (interactive (list (bbdb-current-record)))
  (let* ((buf (get-buffer-create " *moi-krug-invite*"))
         (message (save-excursion
                    (set-window-buffer (selected-window) buf)
                    (erase-buffer)
                    (insert moikrug-invite-message)
                    (text-mode)
                    (local-set-key (kbd "C-c C-c") 'exit-recursive-edit)
                    (message "Press `C-c C-c' when done")
                    (recursive-edit)
                    (prog1
                        (buffer-substring (point-min) (point-max))
                      (kill-buffer (current-buffer)))))
         (ret (moikrug-process-invite
               (moikrug-request 'invite
                 :firstname (moikrug-koi8-to-utf8
                             (bbdb-record-firstname record))
                 :lastname (moikrug-koi8-to-utf8
                            (bbdb-record-lastname record))
                 :email (car (bbdb-record-net record))
                 :message (moikrug-koi8-to-utf8 message)))))
    (unless (string= ret "true")        ; XXX
      (error (format "Error inviting %s %s: %s" (bbdb-record-firstname record)
                     (bbdb-record-lastname record) ret)))
    (bbdb-record-putprop record 'moikrug-invite-date (current-time-string))
    (bbdb-display-records (list record))))

;;;###autoload
(defun moikrug-bbdb-merge-in (sort-type)
  "Merge MoiKrug records into BBDB.
If prefix argument is specified it will ask for SORT-TYPE,
otherwise `moikrug-sort-type' will be used."
  (interactive
   (list (if (null current-prefix-arg)
             moikrug-sort-type
           (intern (completing-read
                    (format "MoiKrug Sort [%S]: " moikrug-sort-type)
                    '(("name") ("modified") ("contacts")) nil t nil nil
                    (symbol-name moikrug-sort-type))))))
  (bbdb-display-records
   (mapcar #'(lambda (mkr)
               ;; Try to find appropriate BBDB record, create new BBDB
               ;; record if not found
               (let* ((email (cdr (assq 'email_primary mkr)))
                      (firstname (cdr (assq 'firstname mkr)))
                      (lastname (cdr (assq 'lastname mkr)))
                      (record (car (bbdb-search (bbdb-records) nil nil email))))
                 (unless record
                   ;; Create new BBDB record
                   (setq record
                         (bbdb-create-internal
                          (concat firstname " " lastname) nil email nil
                          nil nil)))
                 ;; Update record
                 (bbdb-record-set-firstname record firstname)
                 (bbdb-record-set-lastname record lastname)
                 (bbdb-record-set-net
                  record (union (bbdb-record-net record)
                                (list email) :test #'string=))
                 (when (cdr (assq 'modified mkr))
                   (bbdb-record-putprop
                    record 'moikrug-modified
                    (format-time-string
                     bbdb-time-display-format
                     (apply #'encode-time
                            (parse-time-string (cdr (assq 'modified mkr)))))))
                 (bbdb-record-putprop
                  record 'moikrug-merge
                  (format-time-string bbdb-time-display-format))
                 (bbdb-record-putprop
                  record 'moikrug-contacts (cdr (assq 'contacts_count mkr)))
                 (bbdb-change-record record t)
                 record))
           (moikrug-process-list (moikrug-request 'list) sort-type))))

;;}}}

(provide 'moikrug)

;;; moikrug.el ends here
