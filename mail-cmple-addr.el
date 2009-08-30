;;; mail-cmple-addr.el v.1.00
;; complete mail address or expand mail alias interactively
;; on the header field at mail mode
;;
;; Copyright (C) Takashi Mitsuishi, 1995
;; Author: Takashi MITSUISHI <takashi@riec.tohoku.ac.jp>
;; Keyword: mail, alias, address, completion
 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
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
 
;; Try completion of mail address interactively or expand alias on the header.
;; You type some string on the header and do this command,
;; you can try completion the string as a mail address.
;;
;; It perform with mail aliases, original addresses of mail aliases,
;; specified domain names and specified addresses.
;; If `mail-expand-alias' is non-nil and the string is match for alias,
;; it expand original mail address.
;; If `mail-complete-alias` is non-nil, it try completion with mail aliases.
;; If `mail-complete-original-address' is non-nil, it try completion with
;; original mail addresses of mail aliases.
;;
;; You can specify complete address with `mail-address-table.'
;; And you can specify complete domain with `mail-domain-table'.
;;
;; If `mail-noh-self-insert' is non-nil,
;; you can self insert key sequence for this command not on the header."
;;
 
(defvar mail-expand-alias t
  "*If non-nil, expand alias to original address.")
 
(defvar mail-complete-alias t
  "*If non-nil, try completion with mail aliases.")
 
(defvar mail-complete-original-address t
  "*If non-nil, try completion with original addresses of aliases.")
 
(defvar mail-noh-self-insert t
  "*If non-nil and not on the enabled header, self insert key sequence.
For example, you define TAB key for mail-complete-address,
you can insert TAB not on the header.")
 
(defvar mail-address-table nil
  "*Specifies completion address list like \
(\"user1@foo.woo\" \"user2@boo.woo\" ...)")
 
(defvar mail-domain-table nil
  "*Specifies completion domain list like (\"foo.woo\" \"boo.woo\" ...)
and you can complete domain name after typing \"user@f\".")
 
(defvar mail-enabled-header '("To:" "TO:" "Cc:" "CC:" "Bcc:" "BCC:")
  "Specifies enabled headers to type mail addresses.")
 
(defvar mail-address-separator "[@%][^@%]*$"
  "Specifies separator of domain name from user ID or other domain name.")
 
(setq mail-window-configuration nil)
 
(defun convert-list-to-alist (list)
  (and list
       (cons (cons (car list) nil)
      (convert-list-to-alist (cdr list)))))
 
(defun mail-get-original-address (mail-alias-alist)
  (and mail-alias-alist
       (cons (cons (cdr (car mail-alias-alist)) nil)
      (mail-get-original-address (cdr mail-alias-alist)))))
 
(defun mail-concat-domain (string domain-list)
  (and domain-list
       (cons (list (concat string (car domain-list)))
      (mail-concat-domain string (cdr domain-list)))))
 
(defun mail-unique-completions (address-alist)
  (and address-alist
       (let ((cdr-alist (mail-unique-completions (cdr address-alist))))
  (cond ((equal (car address-alist) (car cdr-alist))
  cdr-alist)
        (t (cons (car address-alist) cdr-alist))))))
 
(defun mail-complete-address ()
  "Try completion of mail address interactively or expand alias on the header.
You type some string on the header and do this command,
you can try completion the string as a mail address.
 
It perform with mail aliases, original addresses of mail aliases,
specified domain names and specified addresses.
If `mail-expand-alias' is non-nil and the string is match for alias,
it expand original mail address.
If `mail-complete-alias` is non-nil, it try completion with mail aliases.
If `mail-complete-original-address' is non-nil, it try completion with
original mail addresses of mail aliases.
 
You can specify complete address with `mail-address-table.'
And you can specify complete domain with `mail-domain-table'.
 
If `mail-noh-self-insert' is non-nil,
you can self insert key sequence for this command not on the header."
 
  (interactive)
 
  (let ((line-end (save-excursion (end-of-line) (point)))
 (header-begin nil)
 (header-end nil)
 (header-string nil)
 (address-begin nil)
 (address-end (point))
 (address-string nil)
 (address-completion nil)
 (address-alist nil)
 (mail-alias-entry nil)
 (with-left-angle nil)
 (with-right-angle nil)
 (current-point (point)))
 
    ;; get header
    (save-excursion
      (re-search-backward "^[a-zA-Z-]*:" nil t)
      (setq header-begin (point))
      (search-forward ":" nil t)
      (setq header-end (point)
     header-string (buffer-substring header-begin header-end)))
 
    ;; check header
    (and (or (save-excursion
        (and (null (= (progn (beginning-of-line) (point))
        current-point))
      (goto-char current-point)
      (null (string-match
      ":" (buffer-substring current-point line-end)))
     ; on the header ?
      (search-forward mail-header-separator nil t)
     ; before text ?
      (assoc header-string
      (convert-list-to-alist mail-enabled-header))))
 
      ;; if not on the header for address, self insert key sequence
      (let ((key-sequence (this-command-keys)))
        (if (and (equal (key-binding key-sequence)
          'mail-complete-address)
   mail-noh-self-insert)
     (progn (goto-char current-point)
     (insert key-sequence)))))
 
  ;; get address string
  (save-excursion
    (if (string-equal
  (buffer-substring (1- current-point) current-point) ">")
        (backward-char))
    (re-search-forward "[ \t\n,>]" nil t)
    (backward-char)
    (setq address-end (point)
   with-right-angle
   (string-equal (buffer-substring address-end
       (1+ address-end))
          ">"))
    (re-search-backward "[:, \t\n<]" nil t)
    (re-search-forward "[^ \t\n:,<]" nil t)
    (backward-char)
    (if (> (point) current-point) (goto-char current-point))
    (setq address-begin (point)
   with-left-angle
   (string-equal (buffer-substring (1- address-begin)
       address-begin)
          "<")
   address-string
   (buffer-substring address-begin address-end)))
 
  ;; expand mail alias
  (if (and mail-expand-alias
    (setq mail-alias-entry (assoc address-string mail-aliases)))
      (progn (delete-region address-begin address-end)
      (goto-char address-begin)
      (insert (cdr mail-alias-entry))
      (if with-left-angle
   (if with-right-angle (forward-char)
     (insert ">"))))
 
    ;; make completion alist
    (setq address-alist
   (mail-unique-completions
    (sort
     (append (convert-list-to-alist mail-address-table)
      (and mail-complete-original-address
    (mail-get-original-address mail-aliases))
      (and mail-complete-alias mail-aliases)
      (and mail-domain-table
    (string-match mail-address-separator
           address-string)
    (mail-concat-domain
     (substring
      address-string 0
      (+ (string-match mail-address-separator
         address-string) 1))
     mail-domain-table)))
     '(lambda (a b) (string< (car a) (car b))))))
 
    ;; try completion
    (if (setq address-completion
       (try-completion address-string address-alist))
        (cond ((eq address-completion t) ;; sole completion
        (goto-char current-point)
        (message "Sole completion")
        (cond (with-right-angle (goto-char (1+ address-end)))
       (with-left-angle (goto-char address-end)
          (insert ">"))
       (t (goto-char address-end))))
 
       ;; no more match
       ((string= address-string address-completion)
        (if (null mail-window-configuration)
     (setq mail-window-configuration
    (current-window-configuration)))
        (with-output-to-temp-buffer "*Completions*"
   (display-completion-list
    (all-completions address-string address-alist)))
        (goto-char address-end)
        (if (and with-left-angle (null with-right-angle))
     (progn (insert ">")
     (backward-char))))
 
       ;; macth more
       (t
        (if mail-window-configuration
     (set-window-configuration mail-window-configuration))
        (setq mail-window-configuration nil)
        (delete-region address-begin address-end)
        (goto-char address-begin)
        (insert address-completion)
        (if (and with-left-angle (null with-right-angle))
     (progn (insert ">") (backward-char)))
        (if (and (assoc address-completion address-alist)
          with-left-angle)
     (forward-char))))
 
      ;; no match
      (progn (goto-char current-point)
      (beep)
      (message (concat "No match: " address-string))))))))
 
(provide 'mail-cmple-addr)
;;; mail-cmple-addr.el ends here
