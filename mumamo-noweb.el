;;; mumamo-noweb.el --- MuMaMo noweb goo

;; Copyright (C) 2008  Regents of the University of California

;; Author: Jason Riedy <jason@acm.org>
;; Keywords: MuMaMo noweb

;; Copyright (C) 2008 Regents of the University of California.
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;; - Redistributions of source code must retain the above copyright
;;   notice, this list of conditions and the following disclaimer.
;;
;; - Redistributions in binary form must reproduce the above copyright
;;   notice, this list of conditions and the following disclaimer listed
;;   in this license in the documentation and/or other materials
;;   provided with the distribution.
;;
;; - Neither the name of the copyright holders nor the names of its
;;   contributors may be used to endorse or promote products derived from
;;   this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Commentary:

;; I honestly do not understand the complications in the MuMaMo helper
;; routines, so I don't use them.  Intervals appear to be left-closed
;; and right-open, so start <= pos < end for all chunks.

;;; Code:

(require 'mumamo)

;; Really should be a derived mode that understands the noweb
;; <<framing>>, provides folding/hiding for code portions
;; of code chunks, and tries to guess the file type.  Probably
;; would need serious indirect buffer magic, essentially implementing
;; noweb in elisp.
(defvar mumamo-noweb-code-mode 'fundamental-mode
  "Major mode MuMaMo uses in noweb code sections.")
(defvar mumamo-noweb-code-start-re "^<<.+>>="
  "Regular expression to find the start of a code chunk.")
;; Use a regexp rather than "\n@ " to match beginning-of-buffer.
(defvar mumamo-noweb-code-end-re "^@ "
  "Regular expression to find the end of a code chunk.")

(defun mumamo-noweb-code-chunk (pos posmin posmax)
  "Return a pair of POSITIONs around POS but between POSMIN and
POSMAX, inclusive, that bracket a code chunk, or nil if POS is in
a doc chunk.  Places point after the chunk, or at POSMIN if no
code chunk is found."
  (goto-char pos)
  (end-of-line)
  (let* ((code-start (re-search-backward mumamo-noweb-code-start-re
					 posmin 1))
	 (code-end (re-search-forward mumamo-noweb-code-end-re posmax 1)))
    (when (and code-start code-end (< pos code-end))
      (cons code-start code-end))))

(defun mumamo-noweb-doc-chunk (pos posmin posmax)
  "Return a pair of POSITIONs around POS but between POSMIN and
POSMAX, inclusive, that bracket a doc chunk.  Only valid of POS is not
in a code chunk."
  (goto-char pos)
  (end-of-line)
  (let* ((code-end (if (re-search-backward mumamo-noweb-code-end-re
					   posmin 1)
		       (match-end 0)
		     posmin))
	 (code-start (if (re-search-forward mumamo-noweb-code-start-re
					    posmax 1)
			 (match-beginning 0)
		       posmax)))
    (if (and (<= code-end pos) (< pos code-start))
	(cons code-end code-start))))

(defun mumamo-chunk-noweb (pos posmin posmax)
  "This should work similar to `mumamo-find-possible-chunk'."
  (mumamo-condition-case err
      (save-excursion
	(let ((code-chunk (mumamo-noweb-code-chunk pos posmin posmax)))
	  (if code-chunk
	      (list (car code-chunk) (cdr code-chunk)
		    mumamo-noweb-code-mode nil nil pos)
	    (let ((doc-chunk (mumamo-noweb-doc-chunk pos posmin posmax)))
	      (list (car doc-chunk) (cdr doc-chunk) nil nil nil pos)))))
    (error (mumamo-display-error 'mumamo-chunk-noweb "%s"
				 (error-message-string err)))))

(define-mumamo-turn-on noweb-mumamo-turn-on
  "Turn on multiple major modes for noweb."
  ("noweb literate program" latex-mode (mumamo-chunk-noweb)))

(provide 'mumamo-noweb)
;;; mumamo-noweb.el ends here
