;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*- mode: EMACS-LISP; -*-
;;; this is mon-iptables-regexps.el
;;; ================================================================
;;; DESCRIPTION:
;;; mon-iptables-regexps provides giant nested list wich attempts to
;;; document the symbols, flags, etc. for `iptables' i.e. `netfilter'
;;;
;;; FUNCTIONS:►►►
;;; `mon-iptables-pp-as-sym', `mon-iptables-pp-key',
;;; `mon-cln-iptables-short-form', `mon-iptables-make-regexps',
;;; `mon-cln-iptables-long-form', `mon-iptables-make-regexps-long'
;;; FUNCTIONS:◄◄◄
;;;
;;; MACROS:
;;;
;;; METHODS:
;;;
;;; CLASSES:
;;;
;;; CONSTANTS:
;;; `*mon-iptables-alist-as-sym*'
;;;
;;; VARIABLES:
;;; `*regexp-clean-iptables*'
;;;
;;; ALIASED/ADVISED/SUBST'D:
;;;
;;; DEPRECATED:
;;;
;;; RENAMED:
;;;
;;; MOVED:
;;;
;;; TODO:
;;;
;;; NOTES:
;;;
;;; SNIPPETS:
;;;
;;; REQUIRES:
;;; `mon-iptables-vars.el' <- `*mon-iptables-alst*'
;;;
;;; THIRD-PARTY-CODE:
;;; List `*mon-iptables-alst*' produced from :SOURCE iptables man page.
;;; :COURTESY Herve Eychenne <rv@wallfire.org>
;;; :COURTESY Marc Boucher, Martin Josefsson, Yasuyuki Kozakai, Jozsef
;;; Kadlecsik, Patrick McHardy, James Morris, Pablo Neira Ayuso, Harald Welte
;;; and Rusty Russell <- :NETFILTER-CORE-TEAM
;;; :SEE (URL `http://www.netfilter.org/')
;;;
;;; Strip or reformat with regexps on these commonly employed "TAGS":
;;;
;;; TAGS-APPEARING-IN-COMMENTS:
;;; :CLEANUP :CLOSE :COURTESY :CREATED :DATE :EMACS-WIKI :EVAL-BELOW-TO-TEST
;;; :FIXES :FIXME :HIS :IF-NOT-FEATURE-P :KEYWORD-REGEXPS-IN
;;; :LOAD-SPECIFIC-PROCEDURES :MODIFICATIONS :RENAMED :SEE-BELOW :SUBJECT :TODO
;;; :TEST-ME :UNCOMMENT-BELOW-TO-TEST :VERSION :WAS
;;;
;;; TAGS-APPEARING-IN-DOCSTRINGS:
;;; :ALIASED-BY :CALLED-BY :EXAMPLE :FACE-DEFINED-IN :FACE-DOCUMENTED-IN
;;; :FILE :IDIOM :NOTE :SEE :SEE-ALSO :SOURCE :USED-BY
;;;
;;; AUTHOR: MON KEY
;;; MAINTAINER: MON KEY
;;;
;;; PUBLIC-LINK: (URL `http://www.emacswiki.org/emacs/mon-iptables-regexps.el)
;;; FIRST-PUBLISHED: <Timestamp: #{2009-11-27T16:43:19-05:00Z}#{09485} - by MON>
;;;
;;; FILE-CREATED: <Timestamp: #{2009-11-25T14:34:37-05:00Z}#{09483} - by MON>
;;; ================================================================
;;; This file is not part of GNU Emacs.
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 3, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; see the file COPYING.  If not, write to
;;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;;; Floor, Boston, MA 02110-1301, USA.
;;; ================================================================
;;; Permission is granted to copy, distribute and/or modify this
;;; document under the terms of the GNU Free Documentation License,
;;; Version 1.3 or any later version published by the Free Software
;;; Foundation; with no Invariant Sections, no Front-Cover Texts,
;;; and no Back-Cover Texts. A copy of the license is included in
;;; the section entitled "GNU Free Documentation License".
;;; A copy of the license is also available from the Free Software
;;; Foundation Web site at:
;;; (URL `http://www.gnu.org/licenses/fdl-1.3.txt').
;;; ================================================================
;;; Copyright © 2009 MON KEY
;;; ==============================
;;; CODE:

;;
(require 'mon-iptables-vars)

;;; ==============================
;;; :CREATED <Timestamp: #{2009-11-27T16:29:49-05:00Z}#{09485} - by MON KEY>
(defvar *mon-iptables-alist-as-sym* nil
  "*Lisp alist of symbols, flags, etc. for GNU/Linux `iptables' i.e. `netfilter'.
List keys associate elements of list which are all symbols.\n
List keys include:\n\n:IPTABLES-TARGETS\n:IPTABLES-TABLES\n:IPTABLES-COMMANDS
:IPTABLES-PARAMETERS\n:IPTABLES-OPTIONS\n:IPTABLES-MATCH-EXTENSIONS
:IPTABLES-TARGET-EXTENSIONS\n
For equivalent list with all sublist elts as strings 
:SEE `*mon-iptables-alst*'.\n
List produced from :SOURCE iptables man page.\n
:SEE-ALSO `*regexp-clean-iptables*',
`mon-iptables-pp-key', `mon-iptables-pp-as-sym',
`mon-iptables-make-regexps', `mon-iptables-make-regexps-long'
`mon-cln-iptables-short-form', `mon-cln-iptables-long-form',\n►►►")
;;
;;
;;; :TEST-ME (mapcar 'cdr *mon-iptables-alist-as-sym*)
;;
;;; (progn (makunbound '*mon-iptables-alist-as-sym*) (unintern '*mon-iptables-alist-as-sym*))

;;; ==============================
;;; :CREATED <Timestamp: #{2009-11-27T12:57:26-05:00Z}#{09485} - by MON KEY>
(defvar *regexp-clean-iptables* nil
  "*Regexp alist for converting iptables short flags to 'long-option' flags.\n
alist key :IPTABLES-REGEXPS-SHORT->LONG  <- `mon-iptables-make-regexps'.
alist key :IPTABLES-REGEXPS-LONG->SHORT  <- `mon-iptables-make-regexps-long'.\n
:CALLED-BY `mon-cln-iptables-short-form', `mon-iptables-make-regexps-long'\n
:SEE-ALSO `*regexp-clean-iptables*', `*mon-iptables-alst*',
`*mon-iptables-alist-as-sym*',`mon-iptables-pp-key', `mon-iptables-pp-as-sym'.
►►►")
;; 
;;; :TEST-ME (cdr (assoc :IPTABLES-REGEXPS-SHORT->LONG *regexp-clean-iptables*))
;;; :TEST-ME (cdr (assoc :IPTABLES-REGEXPS-LONG->SHORT *regexp-clean-iptables*))
;;
;;; (progn (makunbound '*regexp-clean-iptables*) (unintern '*regexp-clean-iptables*))

;;; ==============================
;;; :CREATED <Timestamp: #{2009-11-25T22:38:13-05:00Z}#{09484} - by MON KEY>
(defun mon-iptables-make-regexps ()
  "Return regexp list for converting iptables short flags to 'long-option' flags.
Regexps are returned for `*mon-iptables-alst*' keywords:
:IPTABLES-COMMANDS e.g. \(assoc :IPTABLES-COMMANDS *regexp-clean-iptables*\)
:IPTABLES-PARAMETERS e.g. \(assoc :IPTABLES-PARAMETERS *regexp-clean-iptables*\)
Each return elemented is a list of the form:\n
 \(\"\\\\\( <-SHRT> \\\\\)\" \" --<LONG> \"\)\n
Where the car is the short form to match and the cadr is the longform to replace
the match.\n
:EXAMPLE\n\(mon-iptables-make-regexps\)\n
:CALLED-BY `*regexp-clean-iptables*'.\n
:SEE-ALSO `mon-iptables-make-regexps-long', `mon-cln-iptables-short-form', 
`mon-cln-iptables-long-form',`*mon-iptables-alist-as-sym*'.\n►►►"
  (let ((mk-shrt-lng
         (append
          (cdr (assoc :IPTABLES-COMMANDS *mon-iptables-alst*))
          (cdr (assoc :IPTABLES-PARAMETERS *mon-iptables-alst*))))
        (mk-shrt-lng-rgxps))
    (mapc (lambda (x)
            (push `(,(concat "\\( " (cdr x) " \\)") ,(concat " " (car x) " ")) mk-shrt-lng-rgxps))
          mk-shrt-lng)
    mk-shrt-lng-rgxps))
;;
;;; :TEST-ME (mon-iptables-make-regexps)

;;; ==============================
;;; :CREATED <Timestamp: #{2009-11-27T17:26:56-05:00Z}#{09485} - by MON KEY>
(defun mon-iptables-make-regexps-long ()
  "Return regexp list for converting iptables short flags to 'long-option' flags.
Regexps are returned for `*mon-iptables-alst*' keywords:
:IPTABLES-COMMANDS e.g. \(assoc :IPTABLES-COMMANDS *regexp-clean-iptables*\)
:IPTABLES-PARAMETERS e.g. \(assoc :IPTABLES-PARAMETERS *regexp-clean-iptables*\)
Each return elemented is a list of the form:\n
 \(\"\\\\\( --<LONG> \\\\\)\" \" -<SHORT> \"\)\n
Where the car is the short form to match and the cadr is the longform to replace
the match.\n
:EXAMPLE\n\(mon-iptables-make-regexps\)\n
:CALLED-BY `*regexp-clean-iptables*'.\n
:SEE-ALSO `mon-cln-iptables-short-form', `*mon-iptables-alist-as-sym*'.\n►►►"
  (let ((mk-shrt-lng
         (append
          (cdr (assoc :IPTABLES-COMMANDS *mon-iptables-alst*))
          (cdr (assoc :IPTABLES-PARAMETERS *mon-iptables-alst*))))
        (mk-shrt-lng-rgxps))
    (mapc (lambda (x)
            (push `(,(concat "\\( " (car x) " \\)") ,(concat " " (cdr x) " ")) mk-shrt-lng-rgxps))
          mk-shrt-lng)
    mk-shrt-lng-rgxps))
;;
;;; :TEST-ME (mon-iptables-make-regexps-long)
;;
;;; ==============================
;; Now we can bind `*regexp-clean-iptables*'.
(unless (bound-and-true-p *regexp-clean-iptables*)
  (setq *regexp-clean-iptables*
	`((:IPTABLES-REGEXPS-SHORT->LONG .
					 ,(mon-iptables-make-regexps))
	  (:IPTABLES-REGEXPS-LONG->SHORT .
					 ,(mon-iptables-make-regexps-long)))))

;;; ==============================
;;; :CREATED <Timestamp: #{2009-11-27T12:57:04-05:00Z}#{09485} - by MON KEY>
(defun mon-cln-iptables-short-form (&optional start end intrp)
  "Replace short format iptables argument flags with long format equivalent.\n
When optional args START END are non-nil replace short with long in region
otherwise replace all in buffer.\n
When called-interactively, if region is active replace short with long in region
otherwise replace all in buffer.\n
:SEE-ALSO `mon-cln-iptables-long-form',
`mon-iptables-make-regexps', `mon-iptables-make-regexps-long'
`*regexp-clean-iptables*',`*mon-iptables-regexps*', 
`*mon-iptables-alist-as-sym*',`*mon-iptables-alst*'.\n►►►"
  (interactive "i\n\i\np")
  (let* ((reg-or-buffer (cond ((and intrp (use-region-p)
                                    `(,(region-beginning) . ,(region-end))))
                              ((and start end) (cons start end))
                              (t nil)))
         (rep (if (null reg-or-buffer)
                  (buffer-substring-no-properties (buffer-end 0) (buffer-end 1))
                  (buffer-substring-no-properties (car reg-or-buffer) (cdr reg-or-buffer))))
         (rep-s-l (cdr (assoc :IPTABLES-REGEXPS-SHORT->LONG *regexp-clean-iptables*)))
         (mks)
         (mke))
    (when reg-or-buffer 
      (setq mks (make-marker)
            mke (make-marker))
      (set-marker mks (car reg-or-buffer))
      (set-marker mke (cdr reg-or-buffer)))
    (setq rep
          (with-temp-buffer
            (insert rep)
            (goto-char (buffer-end 0))
            (mapc (lambda (x)
                    (let ((mtch (car x))
                          (repl (cadr x))
                          (case-fold-search nil))
                      (goto-char (buffer-end 0))
                      (while (search-forward-regexp mtch nil t)
                        (replace-match repl))))
                  rep-s-l)
            (buffer-substring-no-properties (buffer-end 0) (buffer-end 1))))
    (if reg-or-buffer
        (progn
          (delete-region mks mke)
          (goto-char mks)
          (insert rep))
        (progn
          (erase-buffer)
          (insert rep)))))
;;; 
;;; :TEST-ME (let ((st-nd (save-excursion `(,(search-forward-regexp "►") . 
;;;                           ,(search-forward-regexp "◄")))))
;;;            (mon-cln-iptables-short-form (car st-nd) (cdr st-nd)))
;;
;;,---- :UNCOMMENT-TO-TEST
;;| ►
;;| $TBL -A STLTHSCAN -p tcp --tcp-flags ALL NONE -j DROP
;;| $TBL -A STLTHSCAN -p tcp --tcp-flags SYN,FIN SYN,FIN -j DROP
;;| $TBL -A STLTHSCAN -p tcp --tcp-flags SYN,RST SYN,RST -j DROP
;;| $TBL -A STLTHSCAN -p tcp --tcp-flags FIN,RST FIN,RST -j DROP
;;| $TBL -A STLTHSCAN -p tcp --tcp-flags ACK,FIN FIN -j DROP
;;| $TBL -A STLTHSCAN -p tcp --tcp-flags ACK,PSH PSH -j DROP
;;| $TBL -A STLTHSCAN -p tcp --tcp-flags ACK,URG URG -j DROP
;;| ◄
;;`----

;;; ==============================
;;; :CREATED <Timestamp: #{2009-11-27T12:57:04-05:00Z}#{09485} - by MON KEY>
(defun mon-cln-iptables-long-form (&optional start end intrp)
  "Replace long format iptables argument flags with short format equivalent.
When optional args START END are non-nil replace short with long in region
otherwise replace all in buffer.\
When called-interactively, if region is active replace long with short in region
otherwise replace all in buffer.\n
:SEE-ALSO `mon-cln-iptables-short-form', `mon-iptables-make-regexps', 
`*regexp-clean-iptables*', `*mon-iptables-regexps*',
`*mon-iptables-alist-as-sym*',`*mon-iptables-alst*'.\n►►►"
  (interactive "i\n\i\np")
  (let* ((reg-or-buffer (cond ((and intrp (use-region-p)
                                    `(,(region-beginning) . ,(region-end))))
                              ((and start end) (cons start end))
                              (t nil)))
         (rep (if (null reg-or-buffer)
                  (buffer-substring-no-properties (buffer-end 0) (buffer-end 1))
                  (buffer-substring-no-properties (car reg-or-buffer) (cdr reg-or-buffer))))
         (rep-s-l (cdr (assoc :IPTABLES-REGEXPS-LONG->SHORT *regexp-clean-iptables*)))
         (mks)
         (mke))
    (when reg-or-buffer 
      (setq mks (make-marker)
            mke (make-marker))
      (set-marker mks (car reg-or-buffer))
      (set-marker mke (cdr reg-or-buffer)))
    (setq rep
          (with-temp-buffer
            (insert rep)
            (goto-char (buffer-end 0))
            (mapc (lambda (x)
                    (let ((mtch (car x))
                          (repl (cadr x))
                          (case-fold-search nil))
                      (goto-char (buffer-end 0))
                      (while (search-forward-regexp mtch nil t)
                        (replace-match repl))))
                  rep-s-l)
            (buffer-substring-no-properties (buffer-end 0) (buffer-end 1))))
    (if reg-or-buffer
        (progn
          (delete-region mks mke)
          (goto-char mks)
          (insert rep))
        (progn
          (erase-buffer)
          (insert rep)))))
;;
;;; :TEST-ME (let ((st-nd (save-excursion `(,(search-forward-regexp "►") . 
;;;                           ,(search-forward-regexp "◄")))))
;;;            (mon-cln-iptables-long-form (car st-nd) (cdr st-nd)))
;;
;;,---- :UNCOMMENT-TO-TEST
;;| ►
;;| $TBL --append STLTHSCAN --protocol tcp --tcp-flags ALL NONE --jump DROP
;;| $TBL --append STLTHSCAN --protocol tcp --tcp-flags SYN,FIN SYN,FIN --jump DROP
;;| $TBL --append STLTHSCAN --protocol tcp --tcp-flags SYN,RST SYN,RST --jump DROP
;;| $TBL --append STLTHSCAN --protocol tcp --tcp-flags FIN,RST FIN,RST --jump DROP
;;| $TBL --append STLTHSCAN --protocol tcp --tcp-flags ACK,FIN FIN --jump DROP
;;| $TBL --append STLTHSCAN --protocol tcp --tcp-flags ACK,PSH PSH --jump DROP
;;| $TBL --append STLTHSCAN --protocol tcp --tcp-flags ACK,URG URG --jump DROP
;;| ◄
;;`----

;;; ==============================
;;; :CREATED <Timestamp: #{2009-11-25T22:08:09-05:00Z}#{09483} - by MON>
(defun mon-iptables-pp-key (iptbl-key &optional insrtp)
  "Pretty-print the iptable sublist of `*mon-iptables-alst*' with IPTBL-KEY.
IPTBL-KEY is one of:\n
:IPTABLES-TARGETS\n:IPTABLES-TABLES\n:IPTABLES-COMMANDS\n:IPTABLES-PARAMETERS
:IPTABLES-OPTIONS\n:IPTABLES-MATCH-EXTENSIONS\:IPTABLES-TARGET-EXTENSIONS\n
:EXAMPLE\n(mon-iptables-pp-key :IPTABLES-COMMANDS)\n
:SEE-ALSO `mon-iptables-pp-as-sym', `mon-iptables-make-regexps',
`*regexp-clean-iptables*', `*mon-iptables-alist-as-sym*'.\n►►►"
  (let ((map-ipt-k (car (member iptbl-key (mapcar 'car *mon-iptables-alst*))))
        (ipt-k #'(lambda (x) ;(cdr
                   (assoc x *mon-iptables-alst*)))
        (ipt-sub))
    (if map-ipt-k
        (setq ipt-sub (with-temp-buffer
                      (princ (funcall ipt-k map-ipt-k) (current-buffer))
                      (pp-buffer)
                      (buffer-substring-no-properties (buffer-end 0) (buffer-end 1)))))
    (if insrtp
        (save-excursion
          (newline)
          (princ ipt-sub (current-buffer)))
        (princ ipt-sub))))
;;
;;; :TEST-ME (mon-iptables-pp-key :IPTABLES-COMMANDS)
;;; :TEST-ME (mon-iptables-pp-key :IPTABLES-COMMANDS t)

;;; ==============================
;;; :CREATED <Timestamp: #{2009-11-25T22:14:24-05:00Z}#{09483} - by MON>
(defun mon-iptables-pp-as-sym (&optional insrtp)
  "Pretty-print the iptable list `*mon-iptables-alst*' as symbol values.\n
:SEE-ALSO `mon-iptables-pp-key', `*mon-iptables-alist-as-sym*'.\n►►►"
  (let (ipt-v)
    (setq ipt-v 
          (with-temp-buffer
            (princ *mon-iptables-alst* (current-buffer))
            (pp-buffer)
            (buffer-substring-no-properties (buffer-end 0) (buffer-end 1))))
  (if insrtp
      (save-excursion
        (newline)
        (princ ipt-v (current-buffer)))
    (princ ipt-v))))
;;
;;; :TEST-ME (mon-iptables-pp-as-sym )
;;; :TEST-ME (mon-iptables-pp-as-sym t)
;;
;;; ==============================
;; Now we can bind `*mon-iptables-alist-as-sym*'.
(unless (bound-and-true-p *mon-iptables-alist-as-sym*)
  (setq *mon-iptables-alist-as-sym* (read (mon-iptables-pp-as-sym))))


;;; ==============================
(provide 'mon-iptables-regexps)
;;; ==============================

;;; ================================================================
;;; mon-iptables-regexps.el ends here
;;; EOF

