;;; mon-hash-utils.el ---  procedures to extend Emacs lisp hashtables
;; -*- mode: EMACS-LISP; -*-

;;; ================================================================
;; Copyright © 2009, 2010 MON KEY. All rights reserved.
;;; ================================================================

;; FILENAME: mon-hash-utils.el
;; AUTHOR: MON KEY
;; MAINTAINER: MON KEY
;; CREATED: 2009-05-15T14:18:14-04:00Z
;; VERSION: 1.0.0
;; COMPATIBILITY: Emacs23.*
;; KEYWORDS: extensions, lisp, tools, alloc, data, development,

;;; ================================================================

;;; COMMENTARY: 

;; =================================================================
;; DESCRIPTION:
;; mon-hash-utils provides a collection of procedures to extend Emacs lisp hash
;; table functionality.
;;
;; The majority of the procedures included of this file were gleaned from
;; various authors/sources. Specific authorship of these routines is identified
;; in the header sections below and inline. Thus, where a function is indicated
;; as having been sourced from a third party the MON KEY does not claim
;; authorship of the individual components included of this file. In general,
;; the only act of authorship on MON's part are minor symbol changes additions
;; of gensyms and the assembly of these routines in the aggregate.
;;
;; FUNCTIONS:►►►
;; `mon-hash-all-values',`mon-hash-all-keys',`mon-hash-to-list',
;; `mon-hash-key-onto-list',`mon-hash-describe',`mon-hash-describe-descend',
;; `mon-hash-readlines-file', `mon-hash-readlines-buffer',
;; `mon-hash-make-size', `mon-hash<-vector', `mon-hash-add-uniquify',
;; `mon-hash-table-complete', `mon-hash-to-alist', `mon-hash-from-alist',
;; FUNCTIONS:◄◄◄
;;
;; MACROS:
;; `mon-hash-get-items',`mon-hash-get-values', `mon-hash-get-symbol-keys',
;; `mon-hash-get-keys', `mon-hash-has-key', `mon-hash-get-string-keys',
;; `cl-put-hash', `mon-hash-get-symbol-name-if',
;;
;; METHODS:
;;
;; CLASSES:
;;
;; CONSTANTS:
;;
;; FACES:
;;
;; VARIABLES:
;;
;; ALIASED/ADVISED/SUBST'D:
;;
;; DEPRECATED:
;;
;; RENAMED:
;;
;; MOVED:
;;
;; TODO:
;; Import relevant portionns Beebe's hash routines.
;;
;; NOTES:
;; :SEE (elisp hash functions)
;; :SEE (info "(elisp)Hash Tables") 
;; :SEE-ALSO `make-hash-table', `gethash', `puthash', `remhash', `clrhash',
;; `maphash', `define-hash-table-test', `sxhash', `hash-table-p',
;; `copy-hash-table', `hash-table-count', `hash-table-test',
;; `hash-table-weakness', `hash-table-rehash-size',
;; `hash-table-rehash-threshold', `hash-table-size',
;;
;; NOTES:
;;
;; SNIPPETS:
;;
;; REQUIRES:
;;
;; THIRD-PARTY-CODE:
;;
;; :COURTESY Xah Lee
;; :SEE (URL `http://xahlee.org/emacs/elisp_hash_table.html')
;; Following renamed: 
;; :FROM             -> :TO
;; `hash-all-values' -> `mon-hash-all-values' 
;; `hash-all-keys'   -> `mon-hash-all-keys'
;; `hash-to-list'    -> `mon-hash-to-list'
;;
;; :COURTESY Thierry Volpiatto's - Copyright (C) 2008 - "macros-func-thierry.el"
;; :SEE (URL `http://www.emacswiki.org/emacs/HashMap')
;; 
;; :COURTESY Thierry Volpiatto HIS: traverselisp.el 
;; :SEE (URL `http://freehg.org/u/thiedlecques/traverselisp/')
;; :FROM                                 -> :TO 
;; `traverse-hash-readlines-from-buffer' -> `mon-hash-readlines-buffer'
;; `traverse-hash-readlines'             -> `mon-hash-readlines-file'
;;
;; :COURTESY :FILE gnus-util.el
;; :FROM                     -> :TO
;; `gnus-make-hashtable'     -> `mon-hash<-vector'
;; `gnus-create-hash-size'   -> `mon-hash-make-size'
;; `gnus-hashtable-to-alist' -> `mon-hash-to-alist'
;; `gnus-alist-hashtable'    -> `mon-hash-from-alist'
;;
;; :COURTESY Stefan Reichör :HIS xsteve-functions.el 
;; :FROM                      -> :TO 
;; `xsteve-add-hash-uniquify' -> `mon-hash-add-uniquify' 
;;  
;; :COURTESY Sam Steingold :HIS clhs.el GPL'd with GNU CLISP
;; :FROM                 -> :TO
;; `hash-table-complete' -> `mon-hash-table-complete'
;;
;; URL: http://www.emacswiki.org/emacs/mon-hash-utils.el
;; FIRST-PUBLISHED: <Timestamp: #{2009-09-22} - by MON KEY>
;;
;; EMACSWIKI: { URL of an EmacsWiki describing mon-hash-utils. }
;;
;; FILE-CREATED:
;; <Timestamp: #{2009-05-15T14:18:14-04:00Z}#{} - by MON KEY>
;; =================================================================

;;; LICENSE:

;; =================================================================
;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; =================================================================
;; Permission is granted to copy, distribute and/or modify this
;; document under the terms of the GNU Free Documentation License,
;; Version 1.3 or any later version published by the Free Software
;; Foundation; with no Invariant Sections, no Front-Cover Texts,
;; and no Back-Cover Texts. A copy of the license is included in
;; the section entitled ``GNU Free Documentation License''.
;; 
;; A copy of the license is also available from the Free Software
;; Foundation Web site at:
;; (URL `http://www.gnu.org/licenses/fdl-1.3.txt').
;;; ==============================
;; Copyright © 2009, 2010 MON KEY 
;;; ==============================

;;; CODE:

 
(eval-when-compile (require 'cl))

(unless (and (intern-soft "*IS-MON-OBARRAY*")
             (bound-and-true-p *IS-MON-OBARRAY*))
(setq *IS-MON-OBARRAY* (make-vector 17 nil)))


;;; ==============================
;;; :CHANGESET 2331
;;; :CREATED <Timestamp: #{2010-11-30T20:53:45-05:00Z}#{10482} - by MON KEY>
(defun mon-hash-get-symbol-name-if (hash-string-or-symbol-name &optional as-cons)
  "Like `hash-table-p' but accepts quoted symbols and strings.
HASH-STRING-OR-SYMBOL-NAME is a quoted symbol or string satisfying 
either `symbolp' or `mon-string-not-null-nor-zerop'.\n
When optional arg AS-CONS is ommitted return value is one of the following:\n
 <SYMBOL> hash-table nil\n
Return <SYMBOL> if symbol-value of HASH-STRING-OR-SYMBOL-NAME is `hash-table-p'.
Return the symbol `hash-table` when argument is a literal with hash-table
notation e.g. #s(hash-table { ... } )
Else, return value is nil.\n
When optional arg AS-CONS is non-nil return value is a consd pair with one of
the following formats:\n
- Arg neither `stringp' nor `symbolp', value was `hash-table-p'
 \(hash-table hash-table <HASHTBL>\)\n
- Arg satisfied `symbolp', its value was `hash-table-p'
 \(<SYMBOL> symbol <HASHTBL>\)\n
- Arg satisfied `mon-string-not-null-nor-zerop' its value was `hash-table-p'
 \(<SYMBOL> string <HASHTBL>\)\n
- Arg was `symbolp' or `stringp', not `hash-table-p', its value was
  `mon-sequence-mappable-p'
 \(nil string  \(t . <TYPE-MAPPAPLE>\)\)
 \(nil symbol  \(t . <TYPE-MAPPAPLE>\)\)\n
- Arg was `symbolp' or `stringp', not `hash-table-p', its value was not
  `mon-sequence-mappable-p' 
 \(nil symbol  \(nil . <TYPE-NON-MAPPAPLE>\)\)
 \(nil string  \(nil . <TYPE-NON-MAPPAPLE>\)\)\n
- Arg was `symbolp' or `stringp', not `hash-table-p' its value was not
  consdiered `mon-sequence-mappable-p' b/c it was either `t' or `nil'
 \(nil string  \(nil boolean \(t t\)\)\)
 \(nil string  \(nil boolean \(nil t\)\)\)
 \(nil symbol  \(nil boolean \(t t\)\)\)
 \(nil symbol  \(nil boolean \(nil t\)\)\)\n
- Arg `symbolp' or `stringp' its value was void
 \(nil symbol  void\)
 \(nil string  void\)\n
:NOTE When AS-CONS is ommitted signals an error if value of
HASH-STRING-OR-SYMBOL-NAME is void.\n
:EXAMPLE\n\n\(setq tt--qtd-htable \(make-hash-table\)\)\n
\(hash-table-p tt--qtd-htable\)
\(hash-table-p 'tt--qtd-htable\)
\(hash-table-p \"tt--qtd-htable\"\)

\(mon-hash-get-symbol-name-if 'tt--qtd-htable\)
\(mon-hash-get-symbol-name-if \"tt--qtd-htable\"\)
\(mon-hash-get-symbol-name-if tt--qtd-htable\)

\(mon-hash-get-symbol-name-if 'tt--qtd-htable t\)
\(mon-hash-get-symbol-name-if \"tt--qtd-htable\" t\)
\(mon-hash-get-symbol-name-if tt--qtd-htable t\)

\(setq tt--qtd-htable '\(a b c d\)\)

\(mon-hash-get-symbol-name-if 'tt--qtd-htable\)
\(mon-hash-get-symbol-name-if \"tt--qtd-htable\"\)
\(mon-hash-get-symbol-name-if tt--qtd-htable\)

\(mon-hash-get-symbol-name-if 'tt--qtd-htable t\)
\(mon-hash-get-symbol-name-if \"tt--qtd-htable\" t\)
\(mon-hash-get-symbol-name-if tt--qtd-htable t\)

\(setq tt--qtd-htable \(current-buffer\)\)

\(mon-hash-get-symbol-name-if 'tt--qtd-htable t\)
\(mon-hash-get-symbol-name-if \"tt--qtd-htable\" t\)
\(mon-hash-get-symbol-name-if tt--qtd-htable t\)

\(setq tt--qtd-htable [a b c]\)

\(mon-hash-get-symbol-name-if 'tt--qtd-htable t\)
\(mon-hash-get-symbol-name-if \"tt--qtd-htable\" t\)
\(mon-hash-get-symbol-name-if tt--qtd-htable t\)

\(unintern \"tt--qtd-htable\" obarray\)\n
:SEE-ALSO `mon-hash-or-mappable-p', `mon-sequence-mappable-p',
`mon-string-not-null-nor-zerop', `mon-sequence-mappable-p',
`mon-booleanp'.\n►►►"
  (setq as-cons
        `(,as-cons
          ,(or 
            (and (eq (type-of hash-string-or-symbol-name) 'hash-table) 'hash-table)
            (and (mon-string-not-null-nor-zerop hash-string-or-symbol-name)
                 (funcall #'(lambda (mhgsni-L-1)
                              (setq mhgsni-L-1 (intern-soft mhgsni-L-1 obarray))
                              (and mhgsni-L-1
                                   (boundp mhgsni-L-1)
                                   (hash-table-p (symbol-value mhgsni-L-1))
                                   mhgsni-L-1))
                          hash-string-or-symbol-name))
            (and (not (or (stringp hash-string-or-symbol-name)
                          (null hash-string-or-symbol-name)
                          (not (symbolp hash-string-or-symbol-name))
                          (not (boundp hash-string-or-symbol-name))))
                 (hash-table-p (symbol-value hash-string-or-symbol-name))
                 (intern-soft (symbol-name hash-string-or-symbol-name) obarray)))))
  (if (not (car as-cons))
      (cadr as-cons)
    (or (and (cadr as-cons)
             (or (and (eq (cadr as-cons) 'hash-table)
                      (list (cadr as-cons) 
                            (cadr as-cons) hash-string-or-symbol-name))
                 (list (cadr as-cons) 
                       (type-of hash-string-or-symbol-name)
                       (symbol-value (cadr as-cons)))))
        (and (not (cadr as-cons))
             (setq hash-string-or-symbol-name
                   `(nil 
                     ,@(case (type-of hash-string-or-symbol-name)
                         (symbol 
                          `(symbol 
                            ,(setq as-cons 
                                   (or 
                                    (and (boundp hash-string-or-symbol-name)
                                         (mon-sequence-mappable-p 
                                          (symbol-value hash-string-or-symbol-name) t t))
                                    'void))))
                         (string 
                          `(string 
                            ,(and 
                              (setq as-cons (intern-soft hash-string-or-symbol-name obarray))
                              (setq as-cons 
                                    (or 
                                     (and (boundp as-cons)
                                          (mon-sequence-mappable-p (symbol-value as-cons) t t))
                                     'void)))))
                         (t `(,(type-of hash-string-or-symbol-name)
                              ,(mon-sequence-mappable-p hash-string-or-symbol-name t t))))))))))

 
;;; ==============================
;;; :COURTESY :FILE gnus-util.el
;;; :FROM                     -> :TO
;;; `gnus-make-hashtable'     -> `mon-hash<-vector'
;;; `gnus-create-hash-size'   -> `mon-hash-make-size'
;;; The following two functions are used in gnus-registry.
;;; They were contributed to GNUS by Andreas Fuchs <asf@void.at>.
;;; `gnus-hashtable-to-alist' -> `mon-hash-to-alist'
;;; `gnus-alist-hashtable'    -> `mon-hash-from-alist'
;;; ==============================

;;; ==============================
;;; :CREATED <Timestamp: #{2010-01-15T11:55:46-05:00Z}#{10025} - by MON>
(defun mon-hash<-vector (&optional hashsize)
  "Make a vector based hash table (default and minimum size is 256).\n
Optional argument HASHSIZE specifies a target table size.\n
This value will be optimized with `mon-hash-make-size'.\n
:SEE `mon-help-hash-functions'.\n
:SEE-ALSO `mon-hash-make-size', `mon-hash-add-uniquify',
`mon-hash-readlines-buffer', `mon-hash-readlines-file', `mon-hash-all-values',
`mon-hash-all-keys', `mon-hash-to-list', `mon-hash-get-items',
`mon-hash-get-values', `mon-hash-has-key', `mon-hash-get-symbol-keys',
`mon-hash-get-string-keys', `mon-hash-put-CL', `mon-hash-describe',
`mon-hash-describe-descend'.\n►►►"
  (make-vector (if hashsize (max (mon-hash-make-size hashsize) 256) 256) 0))
;;
;;; ==============================
;;; :CREATED <Timestamp: #{2010-01-15T11:55:46-05:00Z}#{10025} - by MON>
(defun mon-hash-make-size (min-size)
  "Return a number suitable for use when instantiating a new hash-table.\n
Return value is bigger than MIN-SIZE and equal to some 2^x.\n
Like `sxhash' but implemented in Emacs-lisp.\n
:NOTE Many machines (such as sparcs) do not have a hardware modulo operation, so
they implement it in software.  On many sparcs over 50% of the time to intern is
spent in the modulo.  Yes, it's slower than actually computing the hash from the
string!  So we use powers of 2 so people can optimize the modulo to a mask.
:EXAMPLE\n\(let \(k\)
  \(dolist \(q '\(0 3 7 15 31 63 127 255\) \(nreverse k\)\)
    \(push \(mon-hash-make-size q\) k\)\)\)\n
:CALLED-BY `mon-hash<-vector'\n
:SEE `mon-help-hash-functions'.\n
:SEE-ALSO `mon-hash<-vector', `mon-hash-add-uniquify', `mon-hash-to-alist',
`mon-hash-from-alist', `mon-hash-readlines-buffer',
`mon-hash-readlines-file', `mon-hash-all-values', `mon-hash-all-keys',
`mon-hash-to-list', `mon-hash-get-items', `mon-hash-get-values',
`mon-hash-has-key', `mon-hash-get-symbol-keys', `mon-hash-get-string-keys',
`mon-hash-put-CL', `mon-hash-describe', `mon-hash-describe-descend'.\n►►►"
  (let ((mhmi 1))
    (while (< mhmi min-size)
      (setq mhmi (* 2 mhmi)))
    (mon-next-almost-prime mhmi)))

;;; ==============================
;;; :PREFIX "mhfa-"
;;; :CHANGESET 2109
;;; :CREATED <Timestamp: #{2010-09-04T15:30:26-04:00Z}#{10356} - by MON KEY>
(defun mon-hash-from-alist (alist)
  "Build a hashtable from the values in ALIST.\n
:SEE `mon-help-hash-functions'.\n
:SEE-ALSO `mon-hash-to-alist', `mon-hash<-vector', `mon-hash-add-uniquify',
`mon-hash-readlines-buffer', `mon-hash-readlines-file', `mon-hash-all-values',
`mon-hash-all-keys', `mon-hash-to-list', `mon-hash-get-items',
`mon-hash-get-values', `mon-hash-has-key', `mon-hash-get-symbol-keys',
`mon-hash-get-string-keys', `mon-hash-put-CL', `mon-hash-describe',
`mon-hash-describe-descend'.\n►►►"
  (let ((mhfa-lst (make-hash-table :size (mon-hash-make-size (length alist)) ;4096 
                                   :test 'equal)))
    (mapc #'(lambda (mhfa-L-1)
              (puthash (car mhfa-L-1) (cdr mhfa-L-1) mhfa-lst))
          alist)
    mhfa-lst))

;;; ==============================
;;; :PREFIX "mhta-"
;;; :CHANGESET 2109
;;; :CREATED <Timestamp: #{2010-09-04T15:28:34-04:00Z}#{10356} - by MON KEY>
(defun mon-hash-to-alist (hash)
  "Build an alist from the values in HASH.\n
:SEE `mon-help-hash-functions'.\n
:SEE-ALSO `mon-hash-from-alist', `mon-hash<-vector', `mon-hash-add-uniquify',
`mon-hash-readlines-buffer', `mon-hash-readlines-file', `mon-hash-all-values',
`mon-hash-all-keys', `mon-hash-to-list', `mon-hash-get-items',
`mon-hash-get-values', `mon-hash-has-key', `mon-hash-get-symbol-keys',
`mon-hash-get-string-keys', `mon-hash-put-CL', `mon-hash-describe',
`mon-hash-describe-descend'.\n►►►"
  (let ((mhta-lst nil))
    (maphash #'(lambda (mhta-L-kk mhta-L-vv)
                 (setq mhta-lst (cons (cons mhta-L-kk mhta-L-vv) mhta-lst)))
             hash)
    mhta-lst))

;;; ==============================
;;; :COURTESY Stefan Reichör :HIS xsteve-functions.el 
;;; :WAS `xsteve-add-hash-uniquify'
;;; :CREATED <Timestamp: #{2010-01-16T14:45:52-05:00Z}#{10026} - by MON KEY>
(defun mon-hash-add-uniquify (key value table)
  "Add KEY with VALUE to the hash-table TABLE ensure KEY is unique.\n
When KEY is already present in TABLE generate a new KEY such that:\n
 'KEY-N' is 1+ KEY|KEY-V.\n
:SEE `mon-help-hash-functions'.\n
:SEE-ALSO `mon-hash<-vector', `mon-hash-make-size', `mon-hash-readlines-buffer',
`mon-hash-to-alist', `mon-hash-from-alist', `mon-hash-readlines-file',
`mon-hash-all-values', `mon-hash-all-keys', `mon-hash-to-list',
`mon-hash-get-items', `mon-hash-get-values', `mon-hash-has-key',
`mon-hash-get-symbol-keys', `mon-hash-get-string-keys', `mon-hash-put-CL',
`mon-hash-describe', `mon-hash-describe-descend'.\n►►►"
  (when (gethash key table)
    (setq key
          (loop for i = 1 then (1+ i)
                for name = (format "%s<%d>" key i)
                while (gethash name table)
                finally return name)))
  (puthash key value table))

;;; ==============================
;;; :COURTESY Thierry Volpiatto HIS: traverselisp.el 
;;; :SEE (URL `http://freehg.org/u/thiedlecques/traverselisp/')
;;;  :FROM                                -> :TO 
;;; `traverse-hash-readlines-from-buffer' -> `mon-hash-readlines-buffer'
;;; `traverse-hash-readlines'             -> `mon-hash-readlines-file'
;;; ==============================
;;
;;; :PREFIX "mhrf-"
(defun mon-hash-readlines-file (hash-file file-table)
  "Load lines in HASH-FILE into hash-table FILE-TABLE line-numbers as keys.\n
:SEE `mon-help-hash-functions'.\n
:SEE-ALSO `mon-hash<-vector', `mon-hash-make-size', `mon-hash-add-uniquify',
`mon-hash-to-alist', `mon-hash-from-alist', `mon-hash-readlines-buffer',
`mon-hash-all-values', `mon-hash-all-keys', `mon-hash-to-list',
`mon-hash-get-items', `mon-hash-get-values', `mon-hash-has-key',
`mon-hash-get-symbol-keys', `mon-hash-get-string-keys', `mon-hash-put-CL',
`mon-hash-describe', `mon-hash-describe-descend'.\n►►►"
  (save-match-data
    (let* ((mhrf-str (with-temp-buffer
                       (insert-file-contents hash-file)
                       (buffer-string)))
           (mhrf-rd-lst (split-string mhrf-str "\n"))
           (mhrf-cnt 0))
      (dolist (mhrf-D-1 mhrf-rd-lst)
        (puthash mhrf-cnt mhrf-D-1 file-table)
        (incf mhrf-cnt)))))
;;
;;; :PREFIX "mhrb-"
(defun mon-hash-readlines-buffer (buffer buffer-table)
  "Load BUFFER lines into hash-table BUFFER-TABLE with line-numbers as keys.\n
:SEE `mon-help-hash-functions'.\n
:SEE-ALSO `mon-hash<-vector', `mon-hash-make-size', `mon-hash-add-uniquify',
`mon-hash-to-alist', `mon-hash-from-alist', `mon-hash-readlines-file',
`mon-hash-all-values', `mon-hash-all-keys', `mon-hash-to-list',
`mon-hash-get-items', `mon-hash-get-values', `mon-hash-has-key',
`mon-hash-get-symbol-keys', `mon-hash-get-string-keys', `mon-hash-put-CL',
`mon-hash-describe', `mon-hash-describe-descend'.\n►►►"
  (save-match-data
    (let* ((mhrb-str (with-temp-buffer
                       (insert-buffer-substring-no-properties buffer)
                       (buffer-string)))
           (mhrb-rd-lst (split-string mhrb-str "\n"))
           (mhrb-cnt 0))
      (dolist (mhrb-D-1 mhrb-rd-lst)
        (puthash mhrb-cnt mhrb-D-1 buffer-table)
        (incf mhrb-cnt)))))

;;; ==============================
;;; COURTESY: Xah Lee
;;; :SEE (URL `http://xahlee.org/emacs/elisp_hash_table.html')
;;; :FROM             -> :TO
;;; `hash-all-values' -> `mon-hash-all-values' 
;;; `hash-all-keys'   -> `mon-hash-all-keys'
;;; `hash-to-list'    -> `mon-hash-to-list'
;;; ==============================
;;; :MODIFICATIONS <Timestamp: #{2010-02-08T20:32:53-05:00Z}#{10062} - by MON KEY>
;;; Xah functions `mon-hash-to-list', `mon-hash-all-keys', `mon-hash-all-values'
;;; Now use: (push `(, . ,) sym) idiom instead of:
;;; (setq sym cons (list k v) sym)
;;; ==============================
;;
(defun mon-hash-all-values (hashtable)
  "Return all values in HASHTABLE.\n
:SEE `mon-help-hash-functions'.\n
:SEE-ALSO `mon-hash<-vector', `mon-hash-make-size', `mon-hash-add-uniquify',
`mon-hash-to-alist', `mon-hash-from-alist', `mon-hash-readlines-buffer',
`mon-hash-readlines-file', `mon-hash-all-keys', `mon-hash-to-list',
`mon-hash-get-items', `mon-hash-get-values', `mon-hash-has-key',
`mon-hash-get-symbol-keys', `mon-hash-get-string-keys', `mon-hash-put-CL',
`mon-hash-describe', `mon-hash-describe-descend'.\n►►►"
  (let (mhav-lst)
    (maphash #'(lambda (mhav-kk mhav-vv) 
                 (push mhav-vv mhav-lst))
             hashtable)
    mhav-lst))
;;
(defun mon-hash-all-keys (hashtable)
  "Return all keys in HASHTABLE.\n
:SEE `mon-help-hash-functions'.\n
:SEE-ALSO `mon-hash<-vector', `mon-hash-make-size', `mon-hash-add-uniquify',
`mon-hash-to-alist', `mon-hash-from-alist', `mon-hash-readlines-buffer',
`mon-hash-readlines-file', `mon-hash-all-values', `mon-hash-to-list',
`mon-hash-get-items', `mon-hash-get-values', `mon-hash-has-key',
`mon-hash-get-symbol-keys', `mon-hash-get-string-keys', `mon-hash-put-CL',
`mon-hash-describe', `mon-hash-describe-descend'.\n►►►"
  (let (mhak-gthr)
    (maphash #'(lambda (mhak-kk mhak-vv)
                 (push mhak-kk mhak-gthr))
             hashtable)
    mhak-gthr))
;;
(defun mon-hash-to-list (hashtable)
  "Return a list representing key/value pairs in HASHTABLE.\n
:SEE `mon-help-hash-functions'.\n
:SEE-ALSO `mon-hash<-vector', `mon-hash-make-size', `mon-hash-add-uniquify',
`mon-hash-to-alist', `mon-hash-from-alist', `mon-hash-readlines-buffer',
`mon-hash-readlines-file', `mon-hash-all-values', `mon-hash-all-keys',
`mon-hash-get-items', `mon-hash-get-values', `mon-hash-has-key',
`mon-hash-get-symbol-keys', `mon-hash-get-string-keys', `mon-hash-put-CL',
`mon-hash-describe', `mon-hash-describe-descend'.\n►►► "
  (let (mhtl-lst)
    (maphash #'(lambda (mhtl-kk mhtl-vv) 
                 (push `(,mhtl-kk . ,mhtl-vv) mhtl-lst))
             hashtable)
    mhtl-lst))
;;
;;; ==============================
;;; End Xah-lee Section
;;; ==============================

;;; ==============================
;;; :COURTESY MON KEY :HIS mon-hash-utils.el
;;; :CREATED <Timestamp: #{2010-02-16T16:11:53-05:00Z}#{10072} - by MON KEY>
(defmacro mon-hash-get-keys (hashtable)
  "Return all keys in HASHTABLE.\n
Macromatic version of `mon-hash-all-keys'. Unlike `mon-hash-get-string-keys' and
`mon-hash-get-symbol-keys' doesn't try to distinguish between string or symbols.
:SEE `mon-help-hash-functions'.\n
:SEE-ALSO `mon-hash<-vector', `mon-hash-make-size', `mon-hash-add-uniquify',
`mon-hash-to-alist', `mon-hash-from-alist', `mon-hash-readlines-buffer',
`mon-hash-readlines-file', `mon-hash-all-values', `mon-hash-to-list',
`mon-hash-get-items', `mon-hash-get-values', `mon-hash-has-key',
`mon-hash-get-symbol-keys', `mon-hash-get-string-keys', `mon-hash-put-CL',
`mon-hash-describe', `mon-hash-describe-descend'.\n►►►"
  (let ((mhgk-keys (make-symbol "mhgk-keys")))
    `(let (,mhgk-keys)
       (maphash #'(lambda (mhgk-L-1-kk mhgk-L-1-vv)
                    (push mhgk-L-1-kk ,mhgk-keys))
                ,hashtable)
       (setq ,mhgk-keys (nreverse ,mhgk-keys)))))
;;
;; (put 'mon-hash-get-get-keys 'lisp-indent-function <INT>) 

;;; ==============================
;;; COURTESY: Thierry Volpiatto HIS: macros-func-thierry.el
;;; Hash-table procedures to emulate here methods of python dictionaries.
;;; Following were renamed :FROM -> :TO 
;;; `hash-get-items'       -> `mon-hash-get-items'
;;; `hash-get-values'      -> `mon-hash-get-values' 
;;; `hash-has-key',        -> `mon-hash-has-key'   
;;; `hash-get-symbol-keys' -> `mon-hash-get-symbol-keys'
;;; `hash-get-string-keys' -> `mon-hash-get-string-keys'
;;; `cl-put-hash'          -> `mon-hash-put-CL'
;;; :MODIFICATIONS 
;;; `mon-hash-get-items' Added gensym mhgi-items
;;; `mon-hash-get-values' Added gensyms mhgv-val mhgv-all
;;; `mon-hash-get-symbol-keys' Added gensyms mhgsk-keys mhgsk-all. 
;;;  Added optional arg COERCE-STRINGS which reads a string -> symbol if present.
;;; `mon-hash-get-string-keys' Added gensyms mhgSk-keys, mhgSk-all,  mhgSk-str
;;;  Added conditional to tests if key is already a string. 
;;; `mon-hash-has-key' Added gensym mhhk-keys-l
;;; ==============================

;;; ==============================
;;; :MODIFICATIONS <Timestamp: #{2010-02-16T16:30:02-05:00Z}#{10072} - by MON KEY>
(defmacro mon-hash-get-items (hashtable)
  "Return a list of all keys/value pairs in HASHTABLE.\n
:NOTE Each key's value returned as a strings.\n
:SEE `mon-help-hash-functions'.\n
:SEE-ALSO `mon-hash<-vector', `mon-hash-make-size', `mon-hash-add-uniquify',
`mon-hash-to-alist', `mon-hash-from-alist', `mon-hash-readlines-buffer',
`mon-hash-readlines-file', `mon-hash-all-values',  `mon-hash-all-keys',
`mon-hash-to-list', `mon-hash-get-values',  `mon-hash-has-key', 
`mon-hash-get-symbol-keys', `mon-hash-get-string-keys', `mon-hash-put-CL',
`mon-hash-describe', `mon-hash-describe-descend'.\n►►►"
  (let ((mhgi-items (make-symbol "mhgi-items")))
    `(let ((,mhgi-items nil)) 
       (maphash #'(lambda (mhgi-L-1 mhgi-L-2) (push (list mhgi-L-1 mhgi-L-2) ,mhgi-items))
                ,hashtable)
       ,mhgi-items)))
;;
;; (put 'mon-hash-get-items 'lisp-indent-function <INT>) 
;;
;;; ==============================
;;; :MODIFICATIONS <Timestamp: #{2010-02-16T16:28:11-05:00Z}#{10072} - by MON KEY>
(defmacro mon-hash-get-values (hashtable)
  "Return a list of all HASHTABLE values as strings.\n
:SEE `mon-help-hash-functions'.\n
:SEE-ALSO `mon-hash<-vector', `mon-hash-make-size', `mon-hash-add-uniquify',
`mon-hash-readlines-buffer', `mon-hash-readlines-file', `mon-hash-all-values', 
`mon-hash-all-keys', `mon-hash-to-list', `mon-hash-get-items',
`mon-hash-has-key',  `mon-hash-get-symbol-keys', `mon-hash-get-string-keys',
`mon-hash-put-CL', `mon-hash-describe', `mon-hash-describe-descend'.\n►►►"
  (let ((mhgv-val (make-symbol "mhgv-val"))
        (mhgv-all (make-symbol "mhgv-all")))
    `(let ((,mhgv-val nil)
           (,mhgv-all (mon-hash-get-items ,hashtable)))
       (setq ,mhgv-val (mapcar #'cadr ,mhgv-all))
       ,mhgv-val)))
;;
;; (put 'mon-hash-get-values 'lisp-indent-function <INT>) 

;;; ==============================
;;; :MODIFICATIONS <Timestamp: #{2010-02-16T16:18:46-05:00Z}#{10072} - by MON KEY>
;;; :CHANGESET 2316 <Timestamp: #{2010-11-12T23:40:37-05:00Z}#{10455} - by MON KEY>
(defmacro mon-hash-get-symbol-keys (hashtable &optional coerce-strings)
  "Reuturn a list of all keys in HASHTABLE.\n
Like `mon-hash-get-string-keys' but return keys that are symbols.\n
When optional arg COERCE-STRINGS is non-nil when a hashtable key is a string
read that string -> symbol but don't intern it.\n
:EXAMPLE\n
\(let \(\(wonky-hash \(make-hash-table\)\)
      \(wonky-tst \(mon-string-wonkify 
                   \(concat \(mon-nshuffle-vector 
                             \(vconcat \(number-sequence 97 122\)\)\)\) 16\)\)
      gthr\)
  \(mapc #'\(lambda \(ph\) \(puthash ph \(sxhash ph\) wonky-hash\)\) wonky-tst\)
  \(setq gthr \(nreverse `\(,\(mon-hash-get-symbol-keys wonky-hash t\)
                          ,\(mapcar #'\(lambda \(not-in-ob\)
                                       \(intern-soft not-in-ob\)\) wonky-tst\)\)\)\)\)
\(mon-hash-get-symbol-keys-TEST\)\n
:SEE `mon-help-hash-functions'.\n
:SEE-ALSO `mon-hash<-vector', `mon-hash-make-size', `mon-hash-add-uniquify',
`mon-hash-to-alist', `mon-hash-from-alist', `mon-hash-readlines-buffer',
`mon-hash-readlines-file', `mon-hash-all-values',  `mon-hash-all-keys',
`mon-hash-to-list', `mon-hash-get-items', `mon-hash-get-values', 
`mon-hash-has-key',  `mon-hash-get-string-keys', `mon-hash-put-CL',
`mon-hash-describe', `mon-hash-describe-descend'.\n►►►"
  (let ((mhgsk-keys (make-symbol "mhgsk-keys"))
        (mhgsk-all  (make-symbol "mhgsk-all"))
        (mhgsk-ob   (make-symbol "mhgsk-ob")))
    `(let ((,mhgsk-keys nil)
           (,mhgsk-all (mon-hash-get-items ,hashtable))
           (,mhgsk-ob  (and ,coerce-strings (make-vector 8 nil))))
       (setq ,mhgsk-keys (mapcar #'(lambda (mhgsk-L-1)
                                     (if ,coerce-strings
                                         (if (stringp (car mhgsk-L-1))
                                             ;; :WAS (car (read-from-string (car mhgsk)))
                                             (intern (car mhgsk-L-1) ,mhgsk-ob)
                                           (car mhgsk-L-1))
                                       (car mhgsk-L-1)))
                                 ,mhgsk-all))
       ,mhgsk-keys)))
;;
;; (put 'mon-hash-get-symbol-keys 'lisp-indent-function <INT>) 

;;; ==============================
;;; :MODIFICATIONS <Timestamp: #{2010-02-16T16:10:26-05:00Z}#{10072} - by MON KEY>
(defmacro mon-hash-get-string-keys (hashtable)
  "Return a list of all the keys in HASHTABLE.\n
Like `mon-hash-get-symbol-keys' but return keys as strings.\n
:SEE `mon-help-hash-functions'.\n
:SEE-ALSO `mon-hash<-vector', `mon-hash-make-size', `mon-hash-add-uniquify',
`mon-hash-to-alist', `mon-hash-from-alist', `mon-hash-readlines-buffer',
`mon-hash-readlines-file', `mon-hash-all-values',  `mon-hash-all-keys',
`mon-hash-to-list', `mon-hash-get-items', `mon-hash-get-values', 
`mon-hash-has-key',  `mon-hash-get-symbol-keys', `mon-hash-put-CL',
`mon-hash-describe', `mon-hash-describe-descend'.\n►►►"
  (let ((mhgSk-keys (make-symbol "mhgSk-keys"))
        (mhgSk-all  (make-symbol "mhgSk-all"))
        (mhgSk-str  (make-symbol "mhgSk-str")))
  `(let ((,mhgSk-keys nil)
         (,mhgSk-all (mon-hash-get-items ,hashtable))
         (,mhgSk-str nil))
     (setq ,mhgSk-keys (mapcar #'car ,mhgSk-all))
     (dolist (i ,mhgSk-keys)
       (if (stringp i)
           (push i ,mhgSk-str)
           (push (symbol-name i) ,mhgSk-str)))
     ,mhgSk-str)))

;; (put 'mon-hash-get-string-keys 'lisp-indent-function <INT>) 
;;
;;; (defalias 'hash-get-string-keys 'mon-hash-get-string-keys)
;;;(progn (fmakunbound 'hash-get-string-keys) (unintern 'hash-get-string-keys) )
;;
;;; ==============================

;;; ==============================
;;; :MODIFICATIONS <Timestamp: #{2010-02-16T16:23:21-05:00Z}#{10072} - by MON KEY>
(defmacro mon-hash-has-key (key hashtable)
  "Return non-nil if HASHTABLE contains KEY.\n
KEY must be a symbol \(not a string\) as test uses `memq'/`eq'.\n
:NOTE CL uses `member'/`equal' for same.\n
:SEE `mon-help-hash-functions'.\n
:SEE-ALSO `mon-hash<-vector', `mon-hash-make-size', `mon-hash-add-uniquify',
`mon-hash-to-alist', `mon-hash-from-alist', `mon-hash-readlines-buffer',
`mon-hash-readlines-file', `mon-hash-all-values',  `mon-hash-all-keys',
`mon-hash-to-list', `mon-hash-get-items', `mon-hash-get-values', 
`mon-hash-get-symbol-keys', `mon-hash-get-string-keys', `mon-hash-put-CL',
`mon-hash-describe', `mon-hash-describe-descend'.\n►►►"
  (let ((mhhk-keys-l (make-symbol "mhhk-keys-l")))
    `(let ((,mhhk-keys-l (mon-hash-get-symbol-keys ,hashtable)))
       (if (memq ,key ,mhhk-keys-l)
           t
           nil))))
;;
;; (put 'mon-hash-has-key 'lisp-indent-function <INT>) 

;;; ==============================
(defmacro mon-hash-put-CL (key table value)
  "A `puthash' in the style of Common Lisp using elisp cl.el package's `setf'.\n
Associate KEY with VALUE in hashtable TABLE.\n
If key is already present in TABLE, replace its current value with VALUE.\n
:NOTE The argument order of elisp `puthash' is:\n
 \(puthash key value table\)\n
The order of arguments in mon-hash-put-CL is:\n
 \(cl-puthash key table value\)\n
:NOTE :FILE cl-extra.el defines `cl-puthash' by aliasing Emacs-lisp's `puthash'.\n
:SEE `mon-help-hash-functions'.\n
:SEE-ALSO `mon-hash<-vector', `mon-hash-make-size', `mon-hash-add-uniquify',
`mon-hash-to-alist', `mon-hash-from-alist', `mon-hash-readlines-buffer',
`mon-hash-readlines-file', `mon-hash-all-values', `mon-hash-all-keys',
`mon-hash-to-list', `mon-hash-get-items', `mon-hash-get-values',
`mon-hash-has-key', `mon-hash-get-symbol-keys', `mon-hash-get-string-keys',
`mon-hash-describe', `mon-hash-describe-descend'.\n ►►►"
  `(setf (gethash ,key ,table) ,value))

;; (put 'mon-hash-put-CL 'lisp-indent-function <INT>) 

;;; ==============================
;;; End Thierry Macros
;;; ==============================

;;; ==============================
;;; From emacswiki.org:
;;; "I use describe-hash all the time. 
;;;  Its great unless except for when you have nested hash-tables. 
;;;  For that I use a slightly modified version:
;;;      (with-output-to-temp-buffer (help-buffer)
;;;        (mon-describe-hash-descend (symbol-value variable)))"
;;; :SEE (URL `http://www.emacswiki.org/emacs/HashMap') 
;;; Following renamed:
;;; :FROM                   -> :TO
;;; `describe-hash'         -> `mon-hash-describe'
;;; `describe-hash-descend' -> `mon-hash-describe-descend'
;;; ==============================
;;
;;; :CREATED <Timestamp: Wednesday May 13, 2009 @ 07:40.02 PM - by MON KEY>
(defun mon-hash-describe (variable &optional buffer)
  "Display the full documentation of VARIABLE (a symbol).\n
Returns the documentation as a string, also.\n
If VARIABLE has a buffer-local value in BUFFER (default to the current buffer),
it is displayed along with the global value.\n
:SEE `mon-help-hash-functions'.\n
:SEE-ALSO `mon-hash<-vector', `mon-hash-make-size', `mon-hash-add-uniquify',
`mon-hash-to-alist', `mon-hash-from-alist', `mon-hash-readlines-buffer',
`mon-hash-readlines-file', `mon-hash-all-values',  `mon-hash-all-keys',
`mon-hash-to-list', `mon-hash-get-items', `mon-hash-get-values', 
`mon-hash-has-key', `mon-hash-get-symbol-keys', `mon-hash-get-string-keys',
`mon-hash-put-CL', `mon-hash-describe-descend'.\n►►►"
  (interactive
   (let ((v (variable-at-point))
	 (enable-recursive-minibuffers t)
	 val)
     (setq val (completing-read
		(if (and (symbolp v)
			 (hash-table-p (symbol-value v)))
		    (format (concat ":FUNCTION `mon-hash-describe' "
                                    "-- describe hash-map (default %s): ") v)
                  (concat ":FUNCTION `mon-hash-describe' "
                          "-- describe hash-map: "))
		obarray
		(lambda (atom) (and (boundp atom)
				    (hash-table-p (symbol-value atom))))
		t nil nil
		(if (hash-table-p v) (symbol-name v))))
     (list (if (equal val "")
	       v (intern val)))))
  (with-output-to-temp-buffer (help-buffer)
    (maphash (lambda (key value)
	       (pp key)
	       (princ " => ")
	       (pp value)
	       (terpri))
	     (symbol-value variable))))
;;
;;; :COURTESY Peter Sanford 
;;; :SEE (URL `http://github.com/psanford/emacs-oauth.git')
;;; :CREATED <Timestamp: Wednesday May 13, 2009 @ 07:40.02 PM - by MON KEY>
(defun mon-hash-describe-descend (hash)
  "Recursive describe hash function for nested hash-tables.\n
:SEE `mon-help-hash-functions'.\n
:SEE-ALSO `mon-hash<-vector', `mon-hash-make-size', `mon-hash-add-uniquify',
`mon-hash-to-alist', `mon-hash-from-alist', `mon-hash-readlines-buffer',
`mon-hash-readlines-file', `mon-hash-all-values', `mon-hash-all-keys',
`mon-hash-to-list', `mon-hash-get-items', `mon-hash-get-values',
`mon-hash-has-key', `mon-hash-get-symbol-keys', `mon-hash-get-string-keys',
`mon-hash-put-CL', `mon-hash-describe'.\n►►►"
  (maphash (lambda (key value)
	     (pp key)
	     (princ " => ")
	     (if (hash-table-p value)
		 (progn
		   (princ " { ")
		   (terpri)
		   (mon-hash-describe-descend value)
		   (princ " } "))
	       (pp value))
	     (terpri))
	   hash))

;;; ==============================
;;; :COURTESY Sam Steingold :HIS clhs.el :WAS `hash-table-complete'
(defun mon-hash-table-complete (string table how)
  "This makes it possible to use hash-tables with `completing-read'.\n
:NOTE `completing-read' in Emacs 22 accepts hash-tables natively.\n
:SEE `mon-help-hash-functions'.\n
:SEE-ALSO `mon-hash<-vector', `mon-hash-make-size', `mon-hash-add-uniquify',
`mon-hash-to-alist', `mon-hash-from-alist', `mon-hash-readlines-buffer',
`mon-hash-readlines-file', `mon-hash-all-values', `mon-hash-all-keys',
`mon-hash-to-list', `mon-hash-get-items', `mon-hash-get-values',
`mon-hash-has-key', `mon-hash-get-symbol-keys', `mon-hash-get-string-keys',
`mon-hash-put-CL', `mon-hash-describe'.\n►►►"
  (let ((res nil) 
        (st (upcase string)) 
        (len (length string)))
    (maphash #'(lambda (key val)
                 (when (and (<= len (length key))
                            (string= st (substring key 0 len)))
                 (push key res)))
             table)
    (if how
        res                       ; `all-completions'
        (if (cdr res)
            (try-completion st (mapcar #'list res))
            (if (string= st (car res))
                t
                (car res))))))

;;; ==============================
(provide 'mon-hash-utils)
;;; ==============================

 
;; Local Variables:
;; generated-autoload-file: "./mon-loaddefs.el"
;; coding: utf-8
;; End:

;;; ================================================================
;;; mon-hash-utils.el ends here
;;; EOF
