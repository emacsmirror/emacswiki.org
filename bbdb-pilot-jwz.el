;;; bbdb-pilot-jwz.el --- bbdb to palmos address book conduit

;; Copyright (C) 1999 Jamie Zawinski <jwz@jwz.org>, all rights reserved.

;; Maintainer: Noah Friedman <friedman@splode.com>
;; Created: 2000-01-21

;; Noah Friedman's version string.
;; !Id: bbdb-pilot-jwz.el,v 1.7 2001/05/15 10:38:15 friedman Exp !
;; Mine:
;; $Id: bbdb-pilot-jwz.el,v 1.9 2001/05/16 08:50:41 martins Exp $

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
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Commentary:

;; pilot-addresses expects a file with the following 19 fields:
;;
;;    Last Name
;;    First Name
;;    Title
;;    Company
;;    Named Field 1 (default: Work)
;;    Named Field 2 (default: Home)
;;    Named Field 3 (default: Fax)
;;    Named Field 4 (default: Other)
;;    Named Field 5 (default: E-mail)
;;    Address
;;    City
;;    State
;;    Zip
;;    Country
;;    Custom 1
;;    Custom 2
;;    Custom 3
;;    Custom 4
;;    Note
;;
;; The "named fields" are the ones that have a field title that can be set
;; with a popup menu.  The available titles are:
;;
;;    Work
;;    Home
;;    Fax
;;    Other
;;    E-mail
;;    Main
;;    Pager
;;    Mobile
;;
;; A record in the file consists of 19 fields followed by a newline.
;; Field values are enclosed in double-quotes and are separated by commas.
;; The "named" fields may also be preceeded by the field name and a
;; semicolon, e.g.:
;;    "Home";"(415) 555-1212",
;;
;; Strings may contain newlines, and are read with backslash-decoding
;; (for \n, \t and so on.)
;;
;; Embedded quotes are double-quoted in csv output, e.g. " -> ""


;;; Code:

(require 'bbdb)
(require 'cl)

(defconst bbdb-pilot-field-names
  '["Work" "Home" "Fax" "Other" "E-mail" "Main" "Pager" "Mobile"])

;; `title' is in this list since, if present, it is handled specially and
;; we do not want to duplicate it in the notes section of each entry.
;; But it's still a user-defined "notes" field as far as bbdb is concerned.
(defconst bbdb-pilot-ignored-notes
  '(mail-name mail-alias face mark-char title creation-date timestamp))


(bbdb-defstruct bbdb-pilot-
  lastname		; 1
  firstname		; 2
  title			; 3
  company		; 4
  name-1 value-1	; 5
  name-2 value-2	; 6
  name-3 value-3	; 7
  name-4 value-4	; 8
  name-5 value-5	; 9
  address		; 10
  city			; 11
  state			; 12
  zip			; 13
  country		; 14
  custom-1		; 15
  custom-2		; 16
  custom-3		; 17
  custom-4		; 18
  note			; 19
  )


(defun bbdb-pilot-format (pilot)
  "Inserts a `pilot-addresses'-compatible description of the `pilot' struct
into the current buffer."
  (let ((print-escape-newlines nil)
        (print-escape-nonascii nil)
        (standard-output (current-buffer)))
    (save-restriction
      (narrow-to-region (point) (point))
      (prin1 (or (bbdb-pilot-lastname  pilot) ""))      (insert ",")
      (prin1 (or (bbdb-pilot-firstname pilot) ""))      (insert ",")
      (prin1 (or (bbdb-pilot-title     pilot) ""))      (insert ",")
      (prin1 (or (bbdb-pilot-company   pilot) ""))      (insert ",")

      (prin1 (or (bbdb-pilot-name-1    pilot) "Other")) (insert ";")
      (prin1 (or (bbdb-pilot-value-1   pilot) ""))      (insert ",")

      (prin1 (or (bbdb-pilot-name-2    pilot) "Other")) (insert ";")
      (prin1 (or (bbdb-pilot-value-2   pilot) ""))      (insert ",")

      (prin1 (or (bbdb-pilot-name-3    pilot) "Other")) (insert ";")
      (prin1 (or (bbdb-pilot-value-3   pilot) ""))      (insert ",")

      (prin1 (or (bbdb-pilot-name-4    pilot) "Other")) (insert ";")
      (prin1 (or (bbdb-pilot-value-4   pilot) ""))      (insert ",")

      (prin1 (or (bbdb-pilot-name-5    pilot) "Other")) (insert ";")
      (prin1 (or (bbdb-pilot-value-5   pilot) ""))      (insert ",")

      (prin1 (or (bbdb-pilot-address   pilot) ""))      (insert ",")
      (prin1 (or (bbdb-pilot-city      pilot) ""))      (insert ",")
      (prin1 (or (bbdb-pilot-state     pilot) ""))      (insert ",")
      (prin1 (or (bbdb-pilot-zip       pilot) ""))      (insert ",")
      (prin1 (or (bbdb-pilot-country   pilot) ""))      (insert ",")
      (prin1 (or (bbdb-pilot-custom-1  pilot) ""))      (insert ",")
      (prin1 (or (bbdb-pilot-custom-2  pilot) ""))      (insert ",")
      (prin1 (or (bbdb-pilot-custom-3  pilot) ""))      (insert ",")
      (prin1 (or (bbdb-pilot-custom-4  pilot) ""))      (insert ",")
      (prin1 (or (bbdb-pilot-note      pilot) ""))      (insert "\n")

      ;; Replace escaped double quotes (\") with "".
      (goto-char (point-min))
      (while (re-search-forward "\\\\\"" nil t)
        (replace-match "\"\"" nil t))
      (goto-char (point-max))))
  nil)


(defun bbdb-pilot-pretty-print (pilot)
  ;; for debugging
  (let ((i 0)
        (names '["lastname" "firstname" "title" "company"
                 "name-1" "value-1" "name-2" "value-2" "name-3" "value-3"
                 "name-4" "value-4" "name-5" "value-5" "address" "city"
                 "state" "zip" "country" "custom-1" "custom-2" "custom-3"
                 "custom-4" "note"]))
    (while (< i bbdb-pilot-length)
      (insert (format "%12s: " (aref names i)))
      (let ((s (aref pilot i))
            (print-escape-newlines t))
        (if (null s) (setq s ""))
        (insert (format "%S\n" s)))
      (setq i (1+ i))))
  nil)


(defun bbdb-record-to-pilot-record (record)
  "Converts a BBDB record to a Pilot record."
  (let ((pilot (make-vector bbdb-pilot-length nil))
        (phones (bbdb-record-phones record))
        (notes (bbdb-record-raw-notes record)))

    (if (stringp notes)
        (setq notes (list (cons 'notes notes)))
      ;; may be destructively modified later
      (setq notes (copy-alist notes)))

    (if (bbdb-record-aka record)
        (setq notes
              (append notes
                      (list (cons 'AKA
                                  (mapconcat 'identity
                                             (bbdb-record-aka record)
                                             ",\n"))))))

    ;; These fields are easy...
    ;;
    (bbdb-pilot-set-lastname  pilot (bbdb-record-lastname record))
    (bbdb-pilot-set-firstname pilot (bbdb-record-firstname record))
    (bbdb-pilot-set-title     pilot (bbdb-record-getprop record 'title))
    (bbdb-pilot-set-company   pilot (bbdb-record-company record))

    ;; Now do the phone numbers...
    ;;
    (let ((pilot-phones '()))
      (while phones
        (let ((loc (bbdb-phone-location (car phones)))
              (num (bbdb-phone-string (car phones)))
              field)
          (cond ((string-match "\\b\\(work\\|office\\)" loc)
                 (setq field "Work"))
                ((string-match "\\b\\(home\\)" loc)
                 (setq field "Home"))
                ((string-match "\\b\\(fax\\|facs?imile\\)" loc)
                 (setq field "Fax"))
                ((string-match "\\b\\(pager?\\|beeper\\)" loc)
                 (setq field "Pager"))
                ((string-match "\\b\\(cell\\|mobile\\)" loc)
                 (setq field "Mobile"))
                ((string-match "\\b\\(voice\\|main\\|phone\\)" loc)
                 (setq field "Main"))
                (t
                 ;; If we don't recognise the phone label, then call it
                 ;; "Other" but preserve the original label in the field
                 ;; itself.
                 (setq field "Other"
                       num (concat loc ": " num))))

          ;; If this phone number is the same type as one previously seen
          ;; (e.g. there are two pager numbers), append with a newline to
          ;; the existing entry.  This makes it possible to group multiple
          ;; numbers in the same pilot field and make room for more numbers
          ;; of different loc types.
          (let ((seen (assoc field pilot-phones)))
            (if seen
                (setcdr seen (concat (cdr seen) "\n" num))
              (setq pilot-phones (cons (cons field num) pilot-phones))))
          (setq phones (cdr phones))))
      (setq pilot-phones (nreverse pilot-phones))

      ;; The email field goes last in the list of phone fields
      (if (bbdb-record-net record)
          (let ((c (cons "E-mail" (car (bbdb-record-net record)))))
            (setq pilot-phones (nconc pilot-phones (list c)))))

      (if (cdr (bbdb-record-net record))
          (setq notes
                (cons (cons 'other-email
                            (mapconcat 'identity
                                       (cdr (bbdb-record-net record))
                                       ",\n"))
                      notes)))

      (let (pp)
        (setq pp (pop pilot-phones))
        (bbdb-pilot-set-name-1  pilot (car pp))
        (bbdb-pilot-set-value-1 pilot (cdr pp))

        (setq pp (pop pilot-phones))
        (bbdb-pilot-set-name-2  pilot (car pp))
        (bbdb-pilot-set-value-2 pilot (cdr pp))

        (setq pp (pop pilot-phones))
        (bbdb-pilot-set-name-3  pilot (car pp))
        (bbdb-pilot-set-value-3 pilot (cdr pp))

        ;; We've filled in three phone-number fields.
        ;; If there are more than 2 phone numbers left (not counting the
        ;; email field), put remaining numbers in 4th field (with their
        ;; headings) and put the email address in the 5th field.
        (cond ((< (length pilot-phones) 3)
               (setq pp (pop pilot-phones))
               (bbdb-pilot-set-name-4  pilot (car pp))
               (bbdb-pilot-set-value-4 pilot (cdr pp))

               (setq pp (pop pilot-phones))
               (bbdb-pilot-set-name-5  pilot (car pp))
               (bbdb-pilot-set-value-5 pilot (cdr pp)))
              (t
               (let* ((email (assoc "E-mail" pilot-phones))
                      (val
                       (mapconcat
                        #'(lambda (pp)
                            (let ((p 0) s)
                              ;; If there are newlines in the data, make sure
                              ;; each new line begins with the field name
                              ;; since this record is heterogenous.
                              (while (string-match "\n" (cdr pp) p)
                                (setq s (concat "\n" (car pp) ": "))
                                (setq p (+ (match-end 0) (length s)))
                                (setcdr pp (replace-match s t t (cdr pp)))))
                            (concat (car pp) ": " (cdr pp)))
                        (delq email pilot-phones) "\n")))
                 (bbdb-pilot-set-name-4  pilot "Other")
                 (bbdb-pilot-set-value-4 pilot val)

                 (bbdb-pilot-set-name-5  pilot (car email))
                 (bbdb-pilot-set-value-5 pilot (cdr email)))))))

    ;; Now do the addresses...
    ;; Put the first address in the address field, and the others
    ;; in the "custom" fields.
    ;;
    (let* ((addrs (bbdb-record-addresses record))
           (addr1 (pop addrs)))
      (cond
       (addr1
 	(let (st)
 	  (cond ((>= bbdb-file-format 6)
                 (setq st (bbdb-join (bbdb-address-streets addr1) "\n")))
                (t
                 (setq st (bbdb-address-street1 addr1))
                 (if (> (length (bbdb-address-street2 addr1)) 0)
                     (setq st (concat st "\n" (bbdb-address-street2 addr1))))
                 (if (> (length (bbdb-address-street3 addr1)) 0)
                     (setq st (concat st "\n" (bbdb-address-street3 addr1))))))

 	  (setq st (concat (bbdb-address-location addr1) ":\n" st))

 	  (bbdb-pilot-set-address pilot st)
 	  (bbdb-pilot-set-city    pilot (bbdb-address-city  addr1))
 	  (bbdb-pilot-set-state   pilot (bbdb-address-state addr1))
 	  (bbdb-pilot-set-zip     pilot (bbdb-address-zip-string addr1))
 	  (bbdb-pilot-set-country pilot nil))))

      (cond
       (addrs
        (let ((indent-tabs-mode nil)
              (formatted '())
              addr c s)
          (while addrs
            (setq addr (car addrs))
            (save-excursion
              (set-buffer (get-buffer-create "*bbdb-tmp*"))
              (erase-buffer)
              (insert (bbdb-address-location addr) ":\n")
 	      (cond
 	       ((>= bbdb-file-format 6)
 		(let ((sts (bbdb-address-streets addr)))
 		  (while sts
 		    (indent-to 8)
 		    (insert (car sts) "\n")
 		    (setq sts (cdr sts)))))
 	       (t
 		(if (= 0 (length (setq s (bbdb-address-street1 addr)))) nil
 		  (indent-to 8) (insert s "\n"))
 		(if (= 0 (length (setq s (bbdb-address-street2 addr)))) nil
 		  (indent-to 8) (insert s "\n"))
 		(if (= 0 (length (setq s (bbdb-address-street3 addr)))) nil
 		  (indent-to 8) (insert s "\n"))))
              (indent-to 8)
              (insert (setq c (bbdb-address-city addr)))
              (setq s (bbdb-address-state addr))
              (if (and (> (length c) 0) (> (length s) 0)) (insert ", "))
              (insert s "  ")
              (insert (bbdb-address-zip-string addr))

              (setq formatted (cons (buffer-string) formatted))
              (setq addrs (cdr addrs))))
          (setq formatted (nreverse formatted))

          (bbdb-pilot-set-custom-1 pilot (pop formatted))
          (bbdb-pilot-set-custom-2 pilot (pop formatted))
          (bbdb-pilot-set-custom-3 pilot (pop formatted))
          (if (null (cdr formatted))
              (bbdb-pilot-set-custom-4 pilot (pop formatted))
            (bbdb-pilot-set-custom-4 pilot
                                     (mapconcat 'identity formatted "\n"))))))
      )

    ;; Now handle the notes...
    ;;
    (let ((losers bbdb-pilot-ignored-notes))
      (while losers
        (let ((c (assq (car losers) notes)))
          (if c (setq notes (delete c notes))))
        (setq losers (cdr losers))))

    (bbdb-pilot-set-note pilot
     (mapconcat
      #'(lambda (cons)
          (save-excursion
            (set-buffer (get-buffer-create "*bbdb-tmp*"))
            (erase-buffer)
            (insert (format "%s:\n%s" (car cons) (cdr cons)))
            (goto-char (point-min))
            (while (search-forward "\n" nil t)
              (replace-match "\n       " t t))
            (goto-char (point-max))
            (skip-chars-backward "\n\t ")
            (buffer-substring (point-min) (point))))
      notes
      "\n\n"))

    pilot))


(defun bbdb-pilot-make-phone (location phone-string)
  (let* ((num (make-vector
               (if bbdb-north-american-phone-numbers-p
                   bbdb-phone-length
                 2)
               nil))
         (p (bbdb-parse-phone-number phone-string)))
    (bbdb-phone-set-location num location)
    (bbdb-phone-set-area num (nth 0 p)) ; euronumbers too.
    (if (= (length num) 2)
	nil
      (bbdb-phone-set-exchange  num (nth 1 p))
      (bbdb-phone-set-suffix    num (nth 2 p))
      (bbdb-phone-set-extension num (or (nth 3 p) 0)))
    num))


(defun pilot-record-to-bbdb-record (pilot)
  "Converts a Pilot record to a BBDB record."
  (let ((firstname (bbdb-pilot-firstname pilot))
        (lastname  (bbdb-pilot-lastname pilot))
        (company   (bbdb-pilot-company pilot))
        (title     (bbdb-pilot-title pilot)) ; ####
        ;; #### AKA
        (net nil)
        (addrs '())
        (phones '())
        (pphones '())
        (notes '())
        )
      (if (equal company "") (setq company nil))
      (if (equal title   "") (setq title nil))
      (if (equal notes   "") (setq notes nil))

      ;; Process the phone numbers and primary net address...
      ;;
      (setq pphones (list (cons (bbdb-pilot-name-1  pilot)
                                (bbdb-pilot-value-1 pilot))
                          (cons (bbdb-pilot-name-2  pilot)
                                (bbdb-pilot-value-2 pilot))
                          (cons (bbdb-pilot-name-3  pilot)
                                (bbdb-pilot-value-3 pilot))
                          (cons (bbdb-pilot-name-4  pilot)
                                (bbdb-pilot-value-4 pilot))
                          (cons (bbdb-pilot-name-5  pilot)
                                (bbdb-pilot-value-5 pilot))))
      (while pphones
        (cond ((equal (car (car pphones)) "E-mail")
               (setq net (list (cdr (car pphones)))))
              ((and (equal (car (car pphones)) "Other")
                    (string-match "^\\([^ \t\n:]+\\):[ \t]*"
                                  (cdr (car pphones))))
               (let ((a (substring (cdr (car pphones))
                                   (match-beginning 1) (match-end 1)))
                     (b (substring (cdr (car pphones)) (match-end 0))))
                 (setq phones (cons (bbdb-pilot-make-phone a b)
                                    phones))))
              ((> (length (cdr (car pphones))) 0)
               (setq phones (cons (bbdb-pilot-make-phone (car (car pphones))
                                                         (cdr (car pphones)))
                                  phones))))
        (setq pphones (cdr pphones)))
      (setq phones (nreverse phones))

      ;; Now parse the primary address...
      ;;
      (cond ((> (length (bbdb-pilot-address pilot)) 0)
             (let ((addr (make-vector bbdb-address-length nil))
                   loc sts st1 st2 st3
                   (street (bbdb-pilot-address pilot))
                   (cty (bbdb-pilot-city pilot))
                   (ste (bbdb-pilot-state pilot))
                   (zip (bbdb-pilot-zip pilot))
                   )
               (if (equal cty "") (setq cty nil))
               (if (equal ste "") (setq ste nil))
               (if (equal zip "") (setq zip nil))
               (if zip (setq zip (bbdb-parse-zip-string zip)))

               (if (string-match "^\\([^ \t\n:]+\\):[ \t\n]*" street)
                   (setq loc (substring street 0 (match-end 1))
                         street (substring street (match-end 0))))

               (bbdb-address-set-location addr loc)

 	       (cond
 		((>= bbdb-file-format 6)
 		 (while (string-match "^\\([^\n]+\\)\\(\n\\|$\\)" street)
                   (setq sts (append
 			      sts
 			      (list (substring street 0 (match-end 1))))
                         street (substring street (match-end 0))))
 		 (bbdb-address-set-streets  addr sts))
 		(t
 		 (if (string-match "^\\([^\n]+\\)\\(\n\\|$\\)" street)
 		     (setq st1 (substring street 0 (match-end 1))
 			   street (substring street (match-end 0))))
 		 (if (string-match "^\\([^\n]+\\)\\(\n\\|$\\)" street)
 		     (setq st2 (substring street 0 (match-end 1))
 			   street (substring street (match-end 0))))
 		 (if (string-match "^\\([^\n]+\\)\\(\n\\|$\\)" street)
 		     (setq st3 (substring street 0 (match-end 1))
 			   street (substring street (match-end 0))))
 		 (bbdb-address-set-street1  addr (or st1 ""))
 		 (bbdb-address-set-street2  addr (or st2 ""))
 		 (bbdb-address-set-street3  addr (or st3 ""))))

               (bbdb-address-set-city     addr (or cty ""))
               (bbdb-address-set-state    addr (or ste ""))
               (bbdb-address-set-zip      addr zip)
               (setq addrs (list addr))
               )))

      ;; Now parse the secondary addresses...
      ;;
      (let ((paddrs (list (bbdb-pilot-custom-1 pilot)
                          (bbdb-pilot-custom-2 pilot)
                          (bbdb-pilot-custom-3 pilot)
                          (bbdb-pilot-custom-4 pilot))))
        (while paddrs
          (cond
           ((car paddrs)
            ;; #### parse text to address.  fmh.
            ))
          (setq paddrs (cdr paddrs))))

      ;; Now parse the notes field.
      ;;
      ;; ####

      (let ((record
	     (vector firstname lastname nil company phones addrs net notes
		     (make-vector bbdb-cache-length nil))))
	record)))


;;;###autoload
(defun bbdb-to-pilot-file (filename &optional records)
  (interactive "FWrite pilot-addresses file: ")
  (or records (setq records (bbdb-records)))
  (save-excursion
    (set-buffer (find-file-noselect filename))
    (erase-buffer)
    (let ((len (length records))
          (i 0))
      (while records
        (message "%d%%..." (/ (* 100 i) len))
        (bbdb-pilot-format (bbdb-record-to-pilot-record (car records)))
        (setq records (cdr records)
              i (1+ i))))
    (save-buffer)
    (kill-buffer (current-buffer)))
  filename)


(defun bbdb-to-pilot ()
  "Push the current contents of BBDB out to the Pilot."
  (interactive)
  (bbdb-records)  ; load bbdb
  (message "Selecting records...")
  (let ((records
         (remove-if-not
          #'(lambda (record)
              (and (or (bbdb-record-name record)
                       (bbdb-record-company record))
                   (let ((phones-p nil)
                         (phones (bbdb-record-phones record)))
                     (while phones
                       (let ((loc (bbdb-phone-location (car phones))))
                         (if (and (not (string-match "cid" loc))
                                  (not (string-match "[?]" loc)))
                             (setq phones-p t)))
                       (setq phones (cdr phones)))
                     phones-p)))
          (bbdb-records)))
        (file "/tmp/pilot-bbdb"))

    (bbdb-to-pilot-file file records)
    (shell-command (concat "pilot-addresses -p /dev/pilot "
                           "-d BBDB -c BBDB -r " file
                           ;;"; rm " file
                           " &"))
    ))

(provide 'bbdb-pilot-jwz)

;;; bbdb-pilot-jwz.el ends here
