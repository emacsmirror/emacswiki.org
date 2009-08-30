;;; encrypt.el --- encrypt and decrypt using coding system

;; Copyright 2006 Ye Wenbin
;;
;; Author: wenbinye@163.com
;; Version: $Id: encrypt.el,v 0.0 2006/11/08 08:44:25 Administrator Exp $
;; Keywords: 
;; X-URL: not distributed yet

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Installation:
;; Put this file into your load-path and the following into your ~/.emacs:
;;   (autoload 'encrypt-decrypt "encrypt"
;;     "Decrypt a crypted file use encrypt coding system" t)
;;   
;;; Quick Start
;; This elisp provide a method to use a coding system to encrypt files
;; in emacs. 
;; o For Emacs 23
;;   If you use emacs 23, that is very lucky that emacs did almost
;;   everything for decode and encode work. So it is very easy and
;;   fast to create new map for a new coding system. At the first time
;;   you have to generate a encrypt map file under `charset-map-path':
;;   (setq encrypt-map-file "encrypt") ; the map file name M-x
;;   encrypt-generate-map-file Enter the password, and Press C-x C-s
;;   to save the file. If you don't have the permission to save the
;;   file the default `charset-map-path', you can add a new path to
;;   `charset-map-path' and save the buffer to a file with the same
;;   name to `encrypt-map-file' and with extension ".map". Now use
;;   command `encrypt-generate-coding-system' to add new coding system
;;   'encrypt' to `coding-system-list'. Now you can use coding system
;;   encrypt to save or read your file. It is not easy to read the
;;   file without the map file. Next time you can use command
;;   `encrypt-decrypt' to decrypt a encrypt file.
;;   
;;   If you don't want save the map file to disk, It is also possible
;;   to generate the coding system for this session: (setq
;;   encrypt-map-file nil).  At the first time, use command
;;   `encrypt-generate-coding-system' to create a coding system. Now
;;   you can save and write the file with the coding system. Next time
;;   you can also use command `encrypt-decrypt' to decrypt the file,
;;   but this time you will be asked the password.
;;
;; o For other emacs version
;;   A coding system is also need to created. This may take a bit time
;;   to work out it. You can also use same map file that used by emacs
;;   23, but this time you should set the variable to the absolute
;;   path of the file name. Also it can work without the map file. use M-x
;;   encrypt-generate-coding-system to create the encrypt coding
;;   system. This may take a long time to setup translation table for
;;   unicode. So it will save a little time if you save the hash table
;;   to a file use M-x encrypt-dump-translation-hash and set variable
;;   `encrypt-map-load-file' to the file name.
;;   
;;   You can also not save the map file or load file to disk. Just set
;;   variable `encrypt-map-file' and `encrypt-map-load-file' both to
;;   nil.

;;; FIX ME:
;;   It is seem not work with mule-gbk, only can decode the file,
;;   can't encode the buffer.
;;
;;; TO DO:
;;  * Use different passwords for different files

;;; Code:

(eval-when-compile
  (require 'cl))

;;{{{ generate map
(defun encrypt-sxhash (key)
  "An algorithm proposed by Donald E. Knuth in The Art Of
Computer Programming Volume 3, under the topic of sorting and
search chapter 6.4.

Redefine this function because `sxhash' function is different
between emacs versions"
  (let ((hash (length key)))
    (dolist (i (append key nil))
      (setq hash (logxor (lsh hash 5)
                         (lsh hash -27)
                         i)))
    (logand #x7ffffff hash)))

(defun encrypt-shuffle-vector (vector)
  "Randomly permute the elements of VECTOR (all permutations equally likely)."
  (let ((i 0)
	j	temp
	(len (length vector)))
    (while (< i len)
      (setq j (+ i (prand-rand (- len i))))
      (setq temp (aref vector i))
      (aset vector i (aref vector j))
      (aset vector j temp)
      (setq i (1+ i))))
  vector)

(defvar encrypt-map-group-size 255)
(defvar encrypt-map-max-char #xffff)
(defun encrypt-generate-map (passwd)
  (require 'prand)
  (let ((i 0) (start 0) step groups pair newgroups end)
    (setq prand-seed (if (string< "" passwd)
                              (encrypt-sxhash passwd)
                            (random t)))
    ;; put ascii random
    (while (< i 256)
      (setq groups (cons (cons i i) groups)
            i (1+ i)))
    ;; random group other chars
    (while (<= i encrypt-map-max-char)
      (setq step (prand-rand encrypt-map-group-size)
            groups (cons (cons i (min (+ i step) encrypt-map-max-char))
                         groups)
            i (+ i step 1)))
    ;; shuffle groups
    (setq groups (append (encrypt-shuffle-vector (apply 'vector groups)) nil))
    ;; merge groups
    (setq pair (car groups)
          groups (cdr groups))
    (while groups
      (if (= (1+ (cdr pair)) (caar groups))
          (progn
            (setcdr pair (cdar groups))
            (setq groups (cdr groups)))
        (setq newgroups (cons pair newgroups)
              pair (car groups)
              groups (cdr groups))))
    ;; insert index for pairs
    (setq groups nil)
    (dolist (p (cons pair newgroups))
      (if (= (cdr p) (car p))
          (setq groups (cons (cons (cons start nil) (car p)) groups)
                start (1+ start))
        (setq end (+ (- (cdr p) (car p)) start)
              groups
              (cons
               (cons
                (cons start end)
                (car p))
               groups)
              start (1+ end))))
    (nreverse groups)))

(defun encrypt-dump-map (map)
  (with-current-buffer (get-buffer-create "*map*")
    (erase-buffer)
    (dolist (pair map)
      (if (cdar pair)
          (insert (format "0x%X-0x%X 0x%04X\n"
                          (caar pair) (cdar pair) (cdr pair)))
        (insert (format "0x%X 0x%04X\n" (caar pair) (cdr pair)))))))

;;}}}

;;{{{ decrypt command
(defvar encrypt-map-file nil
  "If nil, ask for a password, and generate a map for this session.
If non-nil, in emacs 23, set this variable to the map file name
under directory /etc/charsets. Other version of emacs, set to a
map file name that can be in any place.
")
(defvar encrypt-coding-system-name 'encrypt)

(defun encrypt-decrypt ()
  (interactive)
  (unless (member 'encrypt (coding-system-list))
    (encrypt-generate-coding-system))
  (revert-buffer-with-coding-system 'encrypt))

;;;###autoload
(defun encrypt-read-map-file (file)
  (let (map from to groups)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (while (not (eobp))
        (if (looking-at "^#")
            (forward-line 1)
          (setq map (split-string
                     (buffer-substring (point)
                                       (progn (forward-line 1)
                                              (point))))
                to (string-to-number
                    (substring (cadr map) 2) 16)
                groups
                (cons
                 (cons (if (string-match "-" (car map))
                           (cons
                            (string-to-number
                             (substring (car map)  2 (match-beginning 0)) 16)
                            (string-to-number
                             (substring (car map) (+ (match-end 0) 2)) 16))
                         (cons (string-to-number
                                (substring (car map) 2) 16) nil))
                       to)
                 groups)))))
    (nreverse groups)))
;;}}}

;;{{{ generate coding system for unicode
(when (= emacs-major-version 23)
  (defsubst encrypt-map-file ()
    (when encrypt-map-file
      (require 'ffap)
      (ffap-locate-file encrypt-map-file '(".map") charset-map-path)))
  (defun encrypt-install-coding-system (map)
    (define-charset encrypt-coding-system-name
      "my encrypt coding system charset."
      :short-name "ENCRYPT"
      :code-space [0 #xFF 0 #xFF]
      :map map)
    (define-coding-system encrypt-coding-system-name
      "my encrypt coding system."
      :coding-type 'charset
      :mnemonic ?e
      :charset-list (list encrypt-coding-system-name)))
  
  (defun encrypt-generate-coding-system ()
    (interactive)
    (if encrypt-map-file
        (if (file-exists-p (encrypt-map-file))
            (encrypt-install-coding-system encrypt-map-file)
          (call-interactively 'encrypt-generate-map-file))
      (let ((passwd (read-passwd
                     "Enter your password: "))
            (mapfile (make-temp-file "map" nil ".map")))
        (with-current-buffer (get-buffer-create "*map*")
          (encrypt-dump-map
           (encrypt-generate-map passwd))
          (write-file mapfile)
          (kill-buffer (current-buffer)))
        (encrypt-install-coding-system
         (file-name-nondirectory
          (file-name-sans-extension mapfile)))
        ;; (delete-file mapfile)
        )))
  
  (defun encrypt-generate-map-file (passwd)
    (interactive "sEnter your password to generate map: ")
    (with-current-buffer (get-buffer-create "*map*")
      (encrypt-dump-map
       (encrypt-generate-map passwd))
      (when encrypt-map-file
        (setq buffer-file-name (or (encrypt-map-file)
                                   (concat (car charset-map-path) "/"
                                           encrypt-map-file ".map")))
        (message "Press C-x C-s to save to file %s!" buffer-file-name))
      (switch-to-buffer (current-buffer))
      (setq default-directory (concat (car charset-map-path) "/"))
      (goto-char (point-min))))
  (unless encrypt-map-file
      (add-to-list 'charset-map-path temporary-file-directory t))
  )
;;}}}

;;{{{ generate coding system for not unicode branch
(when (/= emacs-major-version 23)
  (defvar encrypt-translation-decode-hash-table
    (make-hash-table :test 'eq :size 21500 :rehash-size 1000))

  (define-translation-hash-table
    'encrypt-translation-table-for-decode
    encrypt-translation-decode-hash-table)

  (defvar encrypt-translation-encode-hash-table
    (make-hash-table :test 'eq :size 21500 :rehash-size 1000))

  (define-translation-hash-table
    'encrypt-translation-table-for-encode
    encrypt-translation-encode-hash-table)

  (define-ccl-program ccl-decode-encrypt
    `(2
      ((loop
        (read r0 r1)
        (r0 = (r0 <8 r1))
        (lookup-integer encrypt-translation-table-for-decode r0 r1)
        (write-multibyte-character r0 r1)
        (repeat)))))

  (define-ccl-program ccl-encode-encrypt
    `(2
      ((loop
        (read-multibyte-character r0 r1)
        (lookup-character encrypt-translation-table-for-encode r0 r1)
        (write (r0 >> 8))
        (write (r0 & 255))
        (repeat)))))

  (defvar encrypt-cache-password nil)
  (defvar encrypt-map-load-file nil "If non-nil, load map from this file")

  (defun encrypt-generate-coding-system ()
    (interactive)
    (let (map char code)
      (cond ((and encrypt-map-load-file
                  (file-exists-p encrypt-map-load-file))
             (load encrypt-map-load-file))
            ((and encrypt-map-file
                  (file-exists-p encrypt-map-file))
             (setq map (encrypt-read-map-file encrypt-map-file)))
            (t
             (setq map (encrypt-generate-map
                        (read-from-minibuffer "Enter your password: ")))))
      (when map
        (clrhash encrypt-translation-decode-hash-table)
        (clrhash encrypt-translation-encode-hash-table)
        (dolist (pair map)
          (if (cdar pair)
              (dotimes (i (- (cdar pair) (caar pair)))
                (setq char (decode-char 'ucs (+ (cdr pair) i)))
                (when (char-valid-p char)
                  (setq code (+ (caar pair) i))
                  (puthash code char encrypt-translation-decode-hash-table)
                  (puthash char code encrypt-translation-encode-hash-table)))
            (setq char (decode-char 'ucs (cdr pair)))
            (when (char-valid-p char)
              (puthash (caar pair) char encrypt-translation-decode-hash-table)
              (puthash char (caar pair) encrypt-translation-encode-hash-table)))))
      (make-coding-system
       encrypt-coding-system-name 4 ?e
       "Encrypt coding system"
       '(ccl-decode-encrypt . ccl-encode-encrypt)
       `((safe-charsets
          ascii
          eight-bit-control
          eight-bit-graphic
          latin-iso8859-1
          mule-unicode-0100-24ff
          mule-unicode-2500-33ff
          mule-unicode-e000-ffff
          ,@(if utf-translate-cjk-mode
                utf-translate-cjk-charsets))
         (valid-codes (0 . 255))
         (mime-text-unsuitable . t)
         (pre-write-conversion . utf-8-pre-write-conversion)
         (dependency unify-8859-on-encoding-mode
                     unify-8859-on-decoding-mode
                     utf-fragment-on-decoding
                     utf-translate-cjk-mode)
         (ascii-incompatible . t)
         (mime-charset . utf-16)))))

  (defun encrypt-generate-map-file ()
    (interactive)
    (encrypt-dump-map
     (encrypt-generate-map
      (read-from-minibuffer "Enter your password: ")))
    (switch-to-buffer "*map*")
    (goto-char (point-min)))

;;;###autoload
  (defun encrypt-dump-translation-table ()
    (interactive)
    (with-current-buffer (get-buffer-create "*map*")
      (erase-buffer)
      (insert ";;; --- encrypt-map.el -*- coding: utf-8 -*-
;;; Code:\n(mapc
 (lambda (pair)
   (let ((code (car pair))
	 (char (cdr pair)))
	 (puthash code char encrypt-translation-decode-hash-table)
     (puthash char code encrypt-translation-encode-hash-table)))
'(")
      (dotimes (i #xffff)
        (setq char (gethash i encrypt-translation-decode-hash-table))
        (if char
            (insert (format "(#x%X . #x%X)\n  " i char))))
      ;;    (cond ((eq char ?\")
      ;;           (insert (format "(#x%X . ?\\\")\n  " i)))
      ;;          ((eq char ?\\)
      ;;           (insert (format "(#x%X . ?\\\\)\n  " i)))
      ;;          (t
      ;;           (insert (format "(#x%X . ?%c)\n  " i char))))))
      (insert "))\n;;; encrypt-map.el ends here\n")
      (display-buffer (current-buffer))))
  )
;;}}}

(provide 'encrypt)
;;; encrypt.el ends here
