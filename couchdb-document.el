;;; couchdb-document.el --- couchdb document handler

;; Copyright (C) 2010 Changyuan Yu

;; Author: Changyuan Yu <rei.vzy@gmail.com>
;; Created: 2010-10-27
;; Version: 0.1
;; Keywords: file, couchdb, json, org

;; This file is *NOT* part of GNU Emacs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; A copy of the GNU General Public License can be obtained from this
;; program's author (send electronic mail to andyetitmoves@gmail.com)
;; or from the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;;; Commentary:

;; Usage:
;; (require 'couchdb-document)
;; (find-file "/couchdb:/test_db1/doc_XXXX")

;; ChangeLog:

;; TODO:
;; 1. revision(append '@' as revision in filename) support
;; 2. delete-file, delete-directory(delete db), make-directory(create db)
;;    using variable enable and disable delete-directory
;; 3. support dired.

(require 'json)
(require 'url)
(require 'org-json)

;;; Code:

(defvar couchdb-document-host "127.0.0.1")
(defvar couchdb-document-port 5984)

(defvar couchdb-document-debug nil)

(defvar couchdb-document-cache
  (make-hash-table :test 'equal))

(defun couchdb-document-json-decode (ht)
  (json-encode-alist (gethash 'doc ht)))

(defun couchdb-document-org-decode (ht)
  "Using org file to represente json object.
`HT' is the hashtable store information of couchdb document."
  (concat "# -*- mode: org -*-\n"
          "#+startup: contents\n"
          "\n"
   (org-json-decode (gethash 'doc ht))
   "\n"))

(defun couchdb-document-json-encode (ht)
  (save-excursion
    (goto-char (point-min))
    (json-read-r)))

(defun couchdb-document-org-encode (ht)
  "Encode org file to a json object."
  (org-json-encode))

(defun couchdb-document-decode (ht)
  "Decode text representation from couchdb document.
This function is called when read document from couchdb server.

Argument `HT' is a hashtable including necessary information of
a couchdb document:
'_rev => Revision of a document, final text should not depend on this,
'_id => ID of a document, final text should not depend on this,
'doc => Lisp representation of document json object(parsed by `json-read').

Return should be a string, that will inserted into file buffer."
  (funcall (or (gethash :decode ht)
               'couchdb-document-org-decode
               ;;'couchdb-document-json-decode
               )
           ht))

(defun couchdb-document-encode (ht)
  "Encode a json object(lisp representation) from text of `current-buffer' and
the information in hashtable `ht', this function is called when write document
back to couchdb server.

Values of '_id and '_rev in `ht' should not be modified, any key-value pair which
key start with '_' will insert into final return json object, then using `json-encode'
and \"PUT\" to couchdb server."
  (funcall (or (gethash :encode ht)
               'couchdb-document-org-encode
               ;;'couchdb-document-json-encode
               )
           ht))

(defun couchdb-document-handler (operation &rest args)
  ;; First check for the specific operations
  ;; that we have special handling for.
  (let ((op (get operation 'couchdb-document)))
    (if op
        (apply op args)
      ;; Handle any operation we don't know about.
      (with-current-buffer "*Messages*"
        (if couchdb-document-debug
            (insert (format "call op %S with %S" operation args))))
      (apply 'couchdb-document-fallback-handler
             (append (list operation) args)))))

(defun couchdb-document-fallback-handler (operation &rest args)
  (let ((inhibit-file-name-handlers
         (cons 'couchdb-document-handler
               (and (eq inhibit-file-name-operation operation)
                    inhibit-file-name-handlers)))
        (inhibit-file-name-operation operation))
    (apply operation args)))


(defun couchdb-document-name-extract (name)
  "Extract information from a couchdb document path.

Example:
\"/couchdb:/\" => (nil \"127.0.0.1\" 5984 \"\" \"\" \"/\"),
\"/couchdb:/a\" => (nil \"127.0.0.1\" 5984 \"a\" \"\" \"/a\"),
\"/couchdb:/a/b\" => (nil \"127.0.0.1\" 5984 \"a\" \"b\" \"/a/b\"),
\"/couchdb:host:/a/b\" => (nil \"host\" 5984 \"a\" \"b\" \"/a/b\",
\"/couchdb:host#p:/a/b\" => (nil \"host\" p \"a\" \"b\" \"/a/b\"."
  (when (and (stringp name)
             (string-match
              "^/couchdb\\(:\\(\\([^@]+\\)@\\)?\\(.+\\)\\(#\\([0-9]+\\)\\)?\\)?:\\(.*\\)$"
              name))
    (let (host port user path db id)
      (setq user (match-string 3 name))
      (setq host (or (match-string 4 name)
                     couchdb-document-host))
      (setq port (or (match-string 6 name)
                     couchdb-document-port))
      (when (stringp port)
        (setq port (string-to-number port)))
      (setq path (match-string 7 name))
      (setq path
            (concat "/"
                    (replace-regexp-in-string "^/*\\|/*$" "" path)))
      (string-match "^/\\(.*?\\)\\(\\(/.*\\)?\\)$" path)
      (setq db (match-string 1 path))
      (setq id (replace-regexp-in-string "^/" "" (match-string 2 path)))
      (list user host port db id path))))

(defun couchdb-document-name-check (name)
  (let ((a (couchdb-document-name-extract name)))
    (unless a
      (if couchdb-document-debug
          (backtrace)
        (error "Couchdb document name error, %S" name)))
    a))

(defun couchdb-document-name (host port db id)
  "Construct couchdb document name."
  (format "/couchdb:%s%s:/%s"
          host
          (if (equal port couchdb-document-port) ""
            (format "#%d" port))
          (if (equal "" db) ""
            (if (equal "" id) db
              (concat db "/" id)))))

(defun couchdb-document-url (filename)
  "Generate http url from `FILENAME'."
  (let* ((a (couchdb-document-name-check filename)))
    (format "http://%s:%d%s" (nth 1 a) (nth 2 a) (nth 5 a))))

(defun couchdb-document-get (filename &optional nocache)
  (let ((url (couchdb-document-url filename)) json doc metadata)
    (let ((url-package-name "couchdb-document.el")
          (url-request-method "GET")
          (url-request-extra-headers '(("Accept" . "application/json"))))
      (with-current-buffer (url-retrieve-synchronously url)
        (goto-char (point-min))
        (re-search-forward "\r?\n\r?\n")
        (setq json (json-read-r))
        (kill-buffer)))

    ;; json maybe is a simple vector, returned by "_all_dbs"
    (mapc (lambda (kv)
            (if (listp kv)
                (let ((k (car kv))
                      (v (cdr kv)))
                  (if (string-match "^_" (symbol-name k))
                      (setq metadata (nconc metadata (list kv)))
                    (setq doc (nconc doc (list kv)))))
              (setq doc (nconc doc (list kv)))))
          json)

    (unless nocache
      (couchdb-document-cache-set filename 'doc doc)
      (dolist (kv metadata)
        (couchdb-document-cache-set filename (car kv) (cdr kv)))
      (couchdb-document-cache-set filename 'filename filename))
    doc))

(defun couchdb-document-put (filename)
  (let ((url (couchdb-document-url filename)) json ht doc ret)
    (setq ht (couchdb-document-cache-get filename))
    (setq doc (couchdb-document-cache-get filename 'doc))
    ;; copy doc to json, avoid change values in hashtable
    (setq json (copy-list doc))
    ;; merge metadata
    (maphash
     (lambda (k v)
       (when (and (symbolp k) (string-match "^_" (symbol-name k)))
         (setq json (nconc json `((,k . ,v))))))
     ht)
    (let ((url-package-name "couchdb-document.el")
          (url-request-method "PUT")
          (url-request-extra-headers '(("Content-type" . "application/json")))
          (url-request-data (json-encode-alist json)))
      (with-current-buffer (url-retrieve-synchronously url)
        (goto-char (point-min))
        (re-search-forward "\r?\n\r?\n")
        (setq ret (json-read-r))
        (kill-buffer)))
    ret))

;; TODO, return a alist instead of raw string.
(defun couchdb-document-head (filename)
  (let ((url (couchdb-document-url filename)) head)
    (let ((url-package-name "couchdb-document.el")
          (url-request-method "HEAD"))
      (with-current-buffer (url-retrieve-synchronously url)
        (setq head (buffer-string))
        (kill-buffer)))
    head))

(defun couchdb-document-cache-get (filename &optional key)
  "Get hashtable associated to `FILENAME' from `couchdb-document-cache'.
if `KEY' is not nil, return value of `key' instead of hashtable."
  (let ((url (couchdb-document-url filename)) ht)
    (setq ht (gethash url couchdb-document-cache))
    (unless ht
      (couchdb-document-get filename)
      (setq ht (gethash url couchdb-document-cache)))
    (if key (gethash key ht) ht)))

(defun couchdb-document-cache-set (filename key val)
  (let ((url (couchdb-document-url filename)) ht)
    (setq ht (gethash url couchdb-document-cache))
    (unless ht
      (setq ht (make-hash-table))
      (puthash url ht couchdb-document-cache))
    (puthash key val ht)))

(defun couchdb-document-cache-clear (&optional filename)
  "Clear hashtable associated to `FILENAME', called when buffer is killed."
  (unless filename
    (setq filename (buffer-file-name)))
  (when (couchdb-document-name-extract filename)
    (let ((url (couchdb-document-url filename)) ht)
      (setq ht (gethash url couchdb-document-cache))
      (when ht
        (remhash url couchdb-document-cache)
        (when (hash-table-p ht)
          (clrhash ht))))))

(defmacro couchdb-document-define-handler (func args &rest body)
  `(progn
     (defun ,(intern (concat "couchdb-document-"
                             (symbol-name func))) ,args
                             ,(concat "`" (symbol-name func) "'"
                               " for couchdb document defined with macro "
                               "`couchdb-document-define-handler'.")
                             ,@body)
     (put ',func
          'couchdb-document
          ',(intern (concat "couchdb-document-"
                            (symbol-name func))))))


(couchdb-document-define-handler
 file-directory-p (filename)
 (equal "" (nth 4 (couchdb-document-name-check filename))))

(couchdb-document-define-handler
 file-name-directory (filename)
 (let (a id db path)
   (setq a (couchdb-document-name-check filename))
   (setq db (nth 3 a))
   (setq id (nth 4 a))
   (if (not (equal "" id))
       (setq id "")
     (setq db ""))
   (couchdb-document-name
    (nth 1 a) ; host
    (nth 2 a) ; port
    db id)))

(couchdb-document-define-handler
 file-name-nondirectory (filename)
 (let (a id db)
   (setq a (couchdb-document-name-check filename))
   (setq db (nth 3 a))
   (setq id (nth 4 a))
   (cond ((not (equal "" id)) id)
         ((not (equal "" db)) db)
         (t ""))))

(couchdb-document-define-handler
 file-truename (filename)
 ;; TODO, do couchdb query here
 filename)

(couchdb-document-define-handler
 file-attributes
  (filename &optional id-format)
  (let (dir head size)
    (setq dir (file-directory-p filename))
    (setq head (couchdb-document-head filename))
    (setq size (or (and
                    (string-match "Content-Length: \\([0-9]+\\)$" head)
                    (string-to-number (match-string 1 head)))
                   0))
    (list dir ; dir/sym/file
          1 ; links
          (if id-format "-1" -1) ; uid
          (if id-format "-1" -1) ; gid
          '(-1 0) ; last access time
          '(-1 0) ; last modification time
          '(-1 0) ; last status change time
          size ; file size
          (if dir "drwxrwxrwx" "-rw-rw-rw-") ; rw
          nil ;  t if file's gid would change if file were deleted and recreated.
          1 ; inode number
          -1))) ; file system device number


(couchdb-document-define-handler
 expand-file-name (name &optional default-directory)
 (if default-directory
     (if (file-name-absolute-p name)
         ;; drop default-directory
         (if (couchdb-document-name-extract name)
             ;; NOTE: here expan-file-name will **NOT** check file-name-handler-alist
             ;; again, but directly call couchdb-document-handler
             (expand-file-name name)
           (funcall 'couchdb-document-fallback-handler
                    'expand-file-name name))
       (concat default-directory name))
   (let (a user host port db id)
     (setq a (couchdb-document-name-check name))
     (setq user (nth 0 a))
     (setq host (nth 1 a))
     (setq port (nth 2 a))
     (setq db   (nth 3 a))
     (setq id   (nth 4 a))
     (when user (error "Couchdb document does not support user yet"))
     (couchdb-document-name host port db id))))

(defun couchdb-document-file-local-copy-name (filename)
  "Get local copy name of `FILENAME'."
  (let (a)
    (setq a (couchdb-document-name-check filename))
    (format "%s%s%s_%d_%s" temporary-file-directory
            "couchdb-doucment."
            (nth 1 a)
            (nth 2 a)
            (replace-regexp-in-string "/" "_"
                                      (nth 5 a)))))

(defun couchdb-document-delete-file-local-copy (&optional filename)
  "Delete local copy of couchdb document, this is called when buffer is killed."
  (unless filename
    (setq filename (buffer-file-name)))
  (when filename
    (let ((fn (couchdb-document-file-local-copy-name filename)))
      (when (file-exists-p fn)
        (delete-file fn)))))

(couchdb-document-define-handler
 file-local-copy (filename)
  (let (local-file ht)
    (setq local-file
          (couchdb-document-file-local-copy-name filename))
    (setq ht (couchdb-document-cache-get filename))
    (write-region (couchdb-document-decode ht) nil local-file)
    local-file))

(couchdb-document-define-handler
 insert-file-contents (filename &optional visit beg end replace)
  (barf-if-buffer-read-only)
  (if (and visit (or beg end))
      (error "Attempt to visit less than an entire file"))
  (couchdb-document-get filename) ;; TODO, check reversion
  (let ((local-file (file-local-copy filename)))
    (insert-file-contents local-file nil beg end replace)
    (when visit
      (setq buffer-file-name filename)
      (set-visited-file-modtime)
      ;; add kill-buffer-hook
      (add-hook (make-local-variable 'kill-buffer-hook)
                'couchdb-document-cache-clear)
      (add-hook (make-local-variable 'kill-buffer-hook)
                'couchdb-document-delete-file-local-copy))))


(couchdb-document-define-handler
 write-region (start end filename &optional append visit
                     lockname mustbenew)
 (when append (error "Can't append to couchdb document"))
 (unless (equal (buffer-file-name) filename)
   (error "Couchdb document not support save to other name: '%s'" filename))
 (when (stringp start)
   (error "Couchdb document not support write string to file"))
 ;; whole buffer
 (when (not start)
   (setq start (point-min))
   (setq end (point-max)))
 (when (or (not (eq start (point-min))) ; this may support in future
           (not (eq end (point-max))))
   (error "Couchdb document not support write part of file"))

 (let (doc ht ret rev)
   (setq ht (couchdb-document-cache-get filename))
   (setq doc (couchdb-document-encode ht))
   (couchdb-document-cache-set filename 'doc doc)
   (setq ret (couchdb-document-put filename))
   ;; get rev
   (when (assoc 'error ret)
     (error "Couchdb document save error: %s"
            (cdr (assoc 'error ret))))
   (setq rev (cdr (assoc 'rev ret)))
   (couchdb-document-cache-set filename '_rev rev)
   (message "new rev %s" rev)))



(couchdb-document-define-handler
   file-writable-p (filename)
  ;; TODO, always writable
  (not (file-directory-p filename)))

;; no backup
(couchdb-document-define-handler
 find-backup-file-name (filename)
 nil)

;; if filename valid, it is remote
(couchdb-document-define-handler
 file-remote-p (filename &optional id connected)
  (let ((a (couchdb-document-name-check filename)))
    (if connected nil ;; http always not connected
      (cond ((eq id 'method) "couchdb")
            ((eq id 'user) (or (nth 0 a) ""))
            ((eq id 'host) (nth 1 a))
            ((eq id 'localname) (nth 5 a))
            (t (format "/couchdb:%s#%d:"
                       (nth 1 a) ;; host
                       (nth 2 a))))))) ;; port

;; make auto save name local
(couchdb-document-define-handler
 make-auto-save-file-name ()
  (let* ((local-file (file-local-copy (buffer-file-name)))
         (dir (file-name-directory local-file))
         (name (file-name-nondirectory local-file)))
    (concat dir "#" name "#")))


(couchdb-document-define-handler
 file-exists-p (filename)
  (let (head)
    (setq head (couchdb-document-head filename))
    (if (string-match "HTTP.* 200 OK$" head)
        t
      nil)))

(couchdb-document-define-handler
 file-accessible-directory-p (filename)
  (and (file-directory-p filename)
       (file-exists-p filename)))


(couchdb-document-define-handler
 vc-registered (filename)
  ;; TODO return t for couchdb rev control
  nil)

(couchdb-document-define-handler
 insert-directory (file switch &optional w full-dir-p)
 (error "insert-directory not implement yet."))

(couchdb-document-define-handler
 file-readable-p (filename)
 (file-exists-p filename))

(couchdb-document-define-handler
 file-name-all-completions (file dir)
 (directory-files dir nil file nil))

;; TODO, sort not implement
(couchdb-document-define-handler
 directory-files (dir &optional full match nosort)
   (when (file-directory-p dir)
     (let (doc a ret host port db get-db name)
       (setq a (couchdb-document-name-check dir))
       (setq host (nth 1 a))
       (setq port (nth 2 a))
       (setq db   (nth 3 a))
       (setq get-db (equal "" db))
       (setq name (if get-db (couchdb-document-name host port "_all_dbs" "")
                    (couchdb-document-name host port db "_all_docs")))
       (setq doc (couchdb-document-get name t))
       (setq ret (mapcar
                  (lambda (x)
                    (let (id)
                      (setq id (if get-db x (cdr (assoc 'id x))))
                      (when full (setq id (concat dir "/" id)))
                      (list id)))
                  (if get-db doc (cdr (assoc 'rows doc)))))
       (apply 'append ret))))

;; verify-visited-file-modtime, using revison
(couchdb-document-define-handler
 verify-visited-file-modtime (buf)
 (with-current-buffer buf
   (let ((f (buffer-file-name)) rev0 rev1 header)
     (setq rev0 (couchdb-document-cache-get f '_rev))
     (setq header (couchdb-document-head f))
     (setq rev1
           (when (string-match "^Etag: *\"\\(.*\\)\" *$" header)
             (match-string 1 header)))
     (equal rev0 rev1))))

;; add file handler
(add-to-list 'file-name-handler-alist
             '("\\`/couchdb:" . couchdb-document-handler))

(provide 'couchdb-document)

;;; couchdb-document.el ends here
