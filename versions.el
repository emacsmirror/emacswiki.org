;;; versions.el --- version string comparison

;; Copyright (C) 2005, 2006, 2007, 2008, 2009, 2010 Vinicius Jose Latorre

;; Author: Vinicius Jose Latorre <viniciusjl@ig.com.br>
;; Maintainer: Vinicius Jose Latorre <viniciusjl@ig.com.br>
;; Keywords: help, internal, maintenance, debug
;; Version: 1.5
;; X-URL: http://www.emacswiki.org/cgi-bin/wiki/ViniciusJoseLatorre

;; This file is part of GNU Emacs (inside `emacs/lisp/subr.el').

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Introduction
;; ------------
;;
;; This package provides routines to compare string version and to convert
;; string version into an integer list.
;;
;; versions was tested with GNU Emacs 22.0.50.1, 23 and 24.
;;
;; I don't know if it is still compatible with XEmacs.
;;
;; It provides the following functions:
;;
;; `version-to-list'		convert a version string into an integer list.
;;
;; `version-list-<'		return t if integer list L1 is lesser than L2.
;;
;; `version-list-='		return t if integer list L1 is equal to L2.
;;
;; `version-list-<='		return t if integer list L1 is lesser than or
;;				equal to L2.
;;
;; `version='			return t if version V1 is equal to V2.
;;
;; `version<'			return t if version V1 is lesser than V2.
;;
;; `version<='			return t if version V1 is lesser than or equal
;;				to V2.
;;
;;
;; Usage
;; -----
;;
;; To use versions, insert in your Emacs Lisp code:
;;
;;    (require 'versions)
;;
;; So, you can compare versions in Emacs Lisp code like:
;;
;;    (and (version< other-version "6.6pre4")
;;         (error "`my-pack' requires `other' package v6.6pre4 or later"))
;;
;;
;; Acknowledgments
;; ---------------
;;
;; Thanks to Chong Yidong <cyd@stupidchicken.com> for extending
;; `version-to-list' to handle versions like "10.3d".
;;
;; Thanks to Eli Zaretskii <eliz@gnu.org> for extending valid syntax for
;; version string in `version-regexp-alist'.
;;
;; Thanks to Kim F. Storm <storm@cua.dk> for:
;;   * fixing `version-list-not-zero'.
;;   * allowing space as separator before non-numeric part, e.g. "1.0 alpha" in
;;     `version-regexp-alist'.
;;   * interpreting ".X.Y" version as "0.X.Y" version in `version-to-list'.
;;
;; Thanks to Sébastien Kirche <sebastien.kirche@free.fr> for fixing typos and
;; testing.
;;
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; code:


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User variables


(defgroup version nil
  "Version group"
  :link '(emacs-library-link :tag "Source Lisp File" "versions.el")
  :prefix "version-"
  :group 'internal
  :group 'maintenance
  :group 'debug)


(defcustom version-separator "."
  "Specify the string used to separate the version elements.

Usually the separator is \".\", but it can be any other string."
  :type '(string :tag "Version Separator")
  :group 'version)


(defcustom version-regexp-alist
  '(("^[-_+ ]?alpha$"           . -3)
    ("^[-_+]$"                  . -3) ; treat "1.2.3-20050920" and "1.2-3" as alpha releases
    ("^[-_+ ]cvs$"              . -3) ; treat "1.2.3-CVS" as alpha release
    ("^[-_+ ]?beta$"            . -2)
    ("^[-_+ ]?\\(pre\\|rcc\\)$" . -1))
  "Specify association between non-numeric version and its priority.

This association is used to handle version string like \"1.0pre2\",
\"0.9alpha1\", etc.  It's used by `version-to-list' (which see) to convert the
non-numeric part of a version string to an integer.  For example:

   String Version    Integer List Version
   \"1.0pre2\"         (1  0 -1 2)
   \"1.0PRE2\"         (1  0 -1 2)
   \"22.8beta3\"       (22 8 -2 3)
   \"22.8 Beta3\"      (22 8 -2 3)
   \"0.9alpha1\"       (0  9 -3 1)
   \"0.9AlphA1\"       (0  9 -3 1)
   \"0.9 alpha\"       (0  9 -3)

Each element has the following form:

   (REGEXP . PRIORITY)

Where:

REGEXP		regexp used to match non-numeric part of a version string.
		It should begin with the `^' anchor and end with a `$' to
		prevent false hits.  Letter-case is ignored while matching
		REGEXP.

PRIORITY	a negative integer specifying non-numeric priority of REGEXP."
  :type '(repeat :tag "Version Regexp Alist"
		 (cons :tag ""
		       (string :tag "Version Regexp")
		       (integer :tag "Version Priority")))
  :group 'version)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions


(defun version-to-list (ver)
  "Convert version string VER into a list of integers.

The version syntax is given by the following EBNF:

   VERSION ::= NUMBER ( SEPARATOR NUMBER )*.

   NUMBER ::= (0|1|2|3|4|5|6|7|8|9)+.

   SEPARATOR ::= `version-separator' (which see)
	       | `version-regexp-alist' (which see).

The NUMBER part is optional if SEPARATOR is a match for an element
in `version-regexp-alist'.

Examples of valid version syntax:

   1.0pre2   1.0.7.5   22.8beta3   0.9alpha1   6.9.30Beta

Examples of invalid version syntax:

   1.0prepre2   1.0..7.5   22.8X3   alpha3.2   .5

Examples of version conversion:

   Version String    Version as a List of Integers
   \"1.0.7.5\"         (1  0  7 5)
   \"1.0pre2\"         (1  0 -1 2)
   \"1.0PRE2\"         (1  0 -1 2)
   \"22.8beta3\"       (22 8 -2 3)
   \"22.8Beta3\"       (22 8 -2 3)
   \"0.9alpha1\"       (0  9 -3 1)
   \"0.9AlphA1\"       (0  9 -3 1)
   \"0.9alpha\"        (0  9 -3)

See documentation for `version-separator' and `version-regexp-alist'."
  (or (and (stringp ver) (> (length ver) 0))
      (error "Invalid version string: '%s'" ver))
  ;; Change .x.y to 0.x.y
  (if (and (>= (length ver) (length version-separator))
	   (string-equal (substring ver 0 (length version-separator))
			 version-separator))
      (setq ver (concat "0" ver)))
  (save-match-data
    (let ((i 0)
	  (case-fold-search t)		; ignore case in matching
	  lst s al)
      (while (and (setq s (string-match "[0-9]+" ver i))
		  (= s i))
	;; handle numeric part
	(setq lst (cons (string-to-number (substring ver i (match-end 0)))
			lst)
	      i   (match-end 0))
	;; handle non-numeric part
	(when (and (setq s (string-match "[^0-9]+" ver i))
		   (= s i))
	  (setq s (substring ver i (match-end 0))
		i (match-end 0))
	  ;; handle alpha, beta, pre, etc. separator
	  (unless (string= s version-separator)
	    (setq al version-regexp-alist)
	    (while (and al (not (string-match (caar al) s)))
	      (setq al (cdr al)))
	    (cond (al
		   (push (cdar al) lst))
		  ;; Convert 22.3a to 22.3.1, 22.3b to 22.3.2, etc.
		  ((string-match "^[-_+ ]?\\([a-zA-Z]\\)$" s)
		   (push (- (aref (downcase (match-string 1 s)) 0) ?a -1)
			 lst))
		  (t (error "Invalid version syntax: '%s'" ver))))))
      (if (null lst)
	  (error "Invalid version syntax: '%s'" ver)
	(nreverse lst)))))


(defun version-list-< (l1 l2)
  "Return t if L1, a list specification of a version, is lower than L2.

Note that a version specified by the list (1) is equal to (1 0),
\(1 0 0), (1 0 0 0), etc.  That is, the trailing zeros are insignificant.
Also, a version given by the list (1) is higher than (1 -1), which in
turn is higher than (1 -2), which is higher than (1 -3)."
  (while (and l1 l2 (= (car l1) (car l2)))
    (setq l1 (cdr l1)
	  l2 (cdr l2)))
  (cond
   ;; l1 not null and l2 not null
   ((and l1 l2) (< (car l1) (car l2)))
   ;; l1 null and l2 null         ==> l1 length = l2 length
   ((and (null l1) (null l2)) nil)
   ;; l1 not null and l2 null     ==> l1 length > l2 length
   (l1 (< (version-list-not-zero l1) 0))
   ;; l1 null and l2 not null     ==> l2 length > l1 length
   (t  (< 0 (version-list-not-zero l2)))))


(defun version-list-= (l1 l2)
  "Return t if L1, a list specification of a version, is equal to L2.

Note that a version specified by the list (1) is equal to (1 0),
\(1 0 0), (1 0 0 0), etc.  That is, the trailing zeros are insignificant.
Also, a version given by the list (1) is higher than (1 -1), which in
turn is higher than (1 -2), which is higher than (1 -3)."
  (while (and l1 l2 (= (car l1) (car l2)))
    (setq l1 (cdr l1)
	  l2 (cdr l2)))
  (cond
   ;; l1 not null and l2 not null
   ((and l1 l2) nil)
   ;; l1 null and l2 null     ==> l1 length = l2 length
   ((and (null l1) (null l2)))
   ;; l1 not null and l2 null ==> l1 length > l2 length
   (l1 (zerop (version-list-not-zero l1)))
   ;; l1 null and l2 not null ==> l2 length > l1 length
   (t  (zerop (version-list-not-zero l2)))))


(defun version-list-<= (l1 l2)
  "Return t if L1, a list specification of a version, is lower or equal to L2.

Note that integer list (1) is equal to (1 0), (1 0 0), (1 0 0 0),
etc.  That is, the trailing zeroes are insignificant.  Also, integer
list (1) is greater than (1 -1) which is greater than (1 -2)
which is greater than (1 -3)."
  (while (and l1 l2 (= (car l1) (car l2)))
    (setq l1 (cdr l1)
	  l2 (cdr l2)))
  (cond
   ;; l1 not null and l2 not null
   ((and l1 l2) (< (car l1) (car l2)))
   ;; l1 null and l2 null     ==> l1 length = l2 length
   ((and (null l1) (null l2)))
   ;; l1 not null and l2 null ==> l1 length > l2 length
   (l1 (<= (version-list-not-zero l1) 0))
   ;; l1 null and l2 not null ==> l2 length > l1 length
   (t  (<= 0 (version-list-not-zero l2)))))


(defun version= (v1 v2)
  "Return t if version V1 is equal to V2.

Note that version string \"1\" is equal to \"1.0\", \"1.0.0\", \"1.0.0.0\",
etc.  That is, the trailing \".0\"s are insignificant.  Also, version
string \"1\" is higher (newer) than \"1pre\", which is higher than \"1beta\",
which is higher than \"1alpha\"."
  (version-list-= (version-to-list v1) (version-to-list v2)))


(defun version< (v1 v2)
  "Return t if version V1 is lower (older) than V2.

Note that version string \"1\" is equal to \"1.0\", \"1.0.0\", \"1.0.0.0\",
etc.  That is, the trailing \".0\"s are insignificant.  Also, version
string \"1\" is higher (newer) than \"1pre\", which is higher than \"1beta\",
which is higher than \"1alpha\"."
  (version-list-< (version-to-list v1) (version-to-list v2)))


(defun version<= (v1 v2)
  "Return t if version V1 is lower (older) than or equal to V2.

Note that version string \"1\" is equal to \"1.0\", \"1.0.0\", \"1.0.0.0\",
etc.  That is, the trailing \".0\"s are insignificant.  Also, version
string \"1\" is higher (newer) than \"1pre\", which is higher than \"1beta\",
which is higher than \"1alpha\"."
  (version-list-<= (version-to-list v1) (version-to-list v2)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal functions


(defun version-list-not-zero (lst)
  "Return the first non-zero element of LST, which is a list of integers.

If all LST elements are zeros or LST is nil, return zero."
  (while (and lst (zerop (car lst)))
    (setq lst (cdr lst)))
  (if lst
      (car lst)
    ;; there is no element different of zero
    0))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'versions)


;;; versions.el ends here
