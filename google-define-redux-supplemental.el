;;; google-define-redux-supplemental.el --- supplemental functions for google-define-redux
;; -*- mode: EMACS-LISP; -*-

;;; ================================================================
;; Copyright © 2010 MON KEY. All rights reserved.
;;; ================================================================

;; FILENAME: google-define-redux-supplemental.el
;; AUTHOR: MON KEY
;; MAINTAINER: MON KEY
;; CREATED: 2010-03-20T13:39:03-04:00Z
;; VERSION: 1.0.0
;; COMPATIBILITY: Emacs23.*
;; KEYWORDS: applications, comm, external, processes, hypermedia

;;; ================================================================

;;; COMMENTARY: 

;; =================================================================
;; DESCRIPTION:
;; google-define-redux-supplemental provides supplemental functions required by
;; google-define-redux.el 
;; :SEE (URL `http://www.emacswiki.org/emacs/google-define-redux.el')
;; 
;; :NOTE Everything provided here is avaiable elsewhere in the other mon-*.el 
;; This packaged is provided in an attempt to allow use of google-define-redux.el
;; without extensive dependencies. None the less, this package may not always be
;; current and it is preferred/recommended to load the required packages
;; instead should you encounter any errors.
;;
;; FUNCTIONS:►►►
;; `mon-buffer-exists-p'
;; `mon-g2be'
;; `mon-buffer-exists-so-kill'
;; `mon-string-justify-left'
;; `mon-help-temp-docstring-display'
;;
;; FUNCTIONS:◄◄◄
;;
;; MACROS:
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
;;
;; NOTES:
;; `google-define-kill-def-buffers'
;; `mon-buffer-exists-p'             <- :FILE mon-utils.el  <MACRO>
;; `mon-g2be'                        <- :FILE mon-utils.el
;; `mon-buffer-exists-so-kill'       <- :FILE mon-utils.el
;; `mon-string-justify-left'         <- :FILE mon-utils.el
;; `mon-help-temp-docstring-display' <- :FILE mon-doc-help-utils.el
;;
;; SNIPPETS:
;;
;; REQUIRES:
;;
;; THIRD-PARTY-CODE:
;; 
;; URL: http://www.emacswiki.org/emacs/google-define-redux-supplemental.el
;; FIRST-PUBLISHED: <Timestamp: #{2010-03-30T15:41:05-04:00Z}#{10132} - by MON>
;;
;; EMACSWIKI: { URL of an EmacsWiki describing google-define-redux-supplemental. }
;;
;; FILE-CREATED:
;; <Timestamp: #{2010-03-20T13:39:03-04:00Z}#{10116} - by MON KEY>
;;
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
;; Copyright © 2010 MON KEY 
;;; ==============================

;;; CODE:

(eval-when-compile (require 'cl))


(unless (fboundp 'mon-g2be)
;;; ==============================
;;; :MODIFICATIONS <Timestamp: #{2010-03-20T12:59:35-04:00Z}#{10116} - by MON KEY>
;;; :CREATED <Timestamp: #{2010-03-10T20:04:58-05:00Z}#{10104} - by MON KEY>
(defun mon-g2be (&optional min/max)
  "Move point as with `goto-char' to `point-max' or `point-min'.\n
If optional arg MIN/MAX is non-nil and greater than 0 go to `point-max' else
`point-min'.\n
:EXAMPLE\n\n(mon-g2be)\n\n(mon-g2be 0)\n\n(mon-g2be 1)\n
:SEE-ALSO `point-min', `point-max', `buffer-end'.\n►►►"
  (goto-char
   ;; :WAS (if (> min/max 0) (point-max) (point-min))))
   (if (and min/max (> min/max 0))
       (point-max) (point-min))))
) ;; :CLOSE unless
;;; ==============================


(unless (fboundp 'mon-buffer-exists-p)
;;; ==============================
;;; :COURTESY :FILE gnus-util.el :WAS `gnus-buffer-exists-p'
;;; :ADDED let wrapper gensym for local var BUFF-P
;;; :CREATED <Timestamp: #{2010-02-04T14:17:59-05:00Z}#{10054} - by MON KEY>
(defmacro mon-buffer-exists-p (buffer-to-check)
  "Return buffer-name of BUFFER-TO-CHECK if exists.\n
:EXAMPLE\n\n\(mon-buffer-exists-p \(current-buffer\)\)\n
\(prog2 \(get-buffer-create \"*BAD-IF-NOT-KILLED*\"\)
    \(mon-buffer-exists-p \"*BAD-IF-NOT-KILLED*\"\)
  \(kill-buffer \(mon-buffer-exists-p \"*BAD-IF-NOT-KILLED*\"\)\)\)\n
:SEE-ALSO `mon-buffer-exists-so-kill', `mon-with-file-buffer',
`mon-buffer-written-p', `mon-buffer-name->kill-ring', 
`mon-get-buffers-parent-dir', `mon-get-proc-buffers-directories',
`mon-get-buffers-directories', `mon-split-string-buffer-name',
`mon-split-string-buffer-parent-dir', `with-current-buffer', `with-temp-file',
`with-temp-buffer'.\n►►►"
  (let ((buff-p (make-symbol "buff-p")))
    `(let ((,buff-p ,buffer-to-check))
       (when ,buff-p
         (funcall (if (stringp ,buffer-to-check) 'get-buffer 'buffer-name)
                  ,buff-p)))))
) ;; :CLOSE unless
;;; ==============================

;;; ==============================
;; `mon-buffer-exists-so-kill' <- :FILE mon-utils.el
(unless (fboundp 'mon-buffer-exists-so-kill)
;;; ==============================
;;; :CREATED <Timestamp: #{2010-02-05T14:21:16-05:00Z}#{10055} - by MON KEY>
(defun mon-buffer-exists-so-kill (buffer-to-kill)
  "If BUFFER-TO-KILL exists kill it.\n
Return `#<killed buffer>' if buffered killed, else nil.\n
:EXAMPLE\n\n\(let \(\(not-much-longer \(get-buffer-create \"not-much-longer\"\)\)\)
  \(mon-buffer-exists-so-kill \(buffer-name not-much-longer\)\)\)\n
:SEE-ALSO `mon-buffer-exists-p', `mon-with-file-buffer',
`mon-buffer-written-p', `mon-buffer-name->kill-ring'
`mon-get-buffers-parent-dir', `mon-get-proc-buffers-directories',
`mon-get-buffers-directories', `mon-split-string-buffer-name',
`mon-split-string-buffer-parent-dir', `with-current-buffer', `with-temp-file',
`with-temp-buffer'.\n►►►"
  (let ((mbep (mon-buffer-exists-p buffer-to-kill)))
    (if (when mbep (kill-buffer mbep))        
        (get-buffer mbep))))
) ;; :CLOSE unless
;;; ==============================

;;; ==============================
;;; `mon-string-justify-left' <- :FILE mon-utils.el
(unless (fboundp 'mon-string-justify-left)
;;; ==============================
;;; :COURTESY Pascal Bourguignon :HIS pjb-strings.el :WAS `string-justify-left'
;;; :MODIFICATIONS <Timestamp: #{2010-02-03T18:08:59-05:00Z}#{10053} - by MON KEY>
;;; :ADDED `save-match-data' for `split-string'
;;; :RENAMED LEFT-MARGIN arg -> lft-margin. `left-margin' is a global var.
(defun mon-string-justify-left (string &optional width lft-margin)
    "Return a left-justified string built from STRING.\n
:NOTE The default WIDTH is `current-column'.
      The default LFT-MARGIN is `left-margin'.
      The WIDTH is counted from column 0.
      The word separators are those of `split-string':
      [ \\f\\t\\n\\r\\v]+
      Which means that STRING is justified as one paragraph.\n
:EXAMPLE\n\(mon-string-justify-left 
 \(let \(jnk\)
   \(dotimes \(i 8 jnk\) 
     \(dolist \(i '\(64 94\)\)
       \(setq jnk 
             \(concat 
              \" \" 
              \(make-string \(elt \(shuffle-vector [7 5 3 9]\) 3\) i\) jnk\)\)\)\)\) 68 4\)\n
:SEE-ALSO `mon-string-fill-to-col'.\n►►►"
    (save-match-data
      (if ;; (or (null width) (< width lft-margin))
       (null width) (setq width (or fill-column 72)))
      (if (null lft-margin) (setq lft-margin (or left-margin 0)))
      (if (not (stringp string)) 
          (error "string-justify-left: The first argument must be a string"))
      (if (not (and (integerp width) (integerp lft-margin)))
          (error "string-justify-left: The optional arguments must be integers"))
      (let* ((margin (make-string lft-margin 32))
             (splited (split-string string))
             (col lft-margin)
             (justified (substring margin 0 col))
             (word)
             (word-length 0)
             (separator ""))
        (while splited
          (setq word (car splited))
          (setq splited (cdr splited))
          (setq word-length (length word))
          (if (> word-length 0)
              (if (>= (+ col (length word)) width)
                  (progn
                    (setq justified (concat justified "\n" margin word))
                    (setq col (+ left-margin word-length)))
                  (progn
                    (setq justified (concat justified separator word))
                    (setq col (+ col 1 word-length)))))
          (setq separator " "))
        (if (< col width)
            (setq justified (concat justified (make-string (- width col) 32))))
        justified)))
;;;
;;; :TEST-ME 
;;; (mon-string-justify-left 
;;;  (let (jnk)
;;;    (dotimes (i 8 jnk) 
;;;      (dolist (i '(64 94))
;;;        (setq jnk (concat " " (make-string (elt (shuffle-vector [7 5 3 9]) 3) i) jnk))))) 68)
) ;; :CLOSE unless
;;; ==============================

;;; ==============================
;;; `mon-help-temp-docstring-display' <- :FILE mon-doc-help-utils.el
(unless (fboundp 'mon-help-temp-docstring-display)
;;; ==============================
;;; :NOTE See the hidden fnctn: `make-help-screen' .
;;; :MODIFICATIONS <Timestamp: #{2010-02-03T15:16:13-05:00Z}#{10053} - by MON>
;;; :CREATED <Timestamp: #{2009-12-20T17:50:49-05:00Z}#{09517} - by MON>
(defun mon-help-temp-docstring-display (the-help-doc &optional some-other-buffer
                                        kill-em-before-they-grow)
  "Display THE-HELP-DOC string formatted as per help-mode.
Leave existing *Help* buffer untouched.\n
Docstring is displayed in the buffer named by the the value of 
:VARIABLE `*mon-help-docstring-help-bffr*'.\n
When optional arg SOME-OTHER-BUFFER is non-nil display THE-HELP-DOC in that
buffer instead.\n
When optional arg KILL-EM-BEFORE-THEY-GROW is non-nil kill any existing
SOME-OTHER-BUFFER with name before displaying contents there.\n
:EXAMPLE\n\(mon-help-temp-docstring-display
 \(documentation 'mon-help-temp-docstring-display\)\)\n 
:CALLED-BY `google-define' \(MON's VERSION\)\n
:SEE-ALSO `mon-help-view-file'.\n►►►"
  (let ((help-window-select 'always)
        (dhb (buffer-name 
              (if some-other-buffer
                 ;; :NOTE Going into view-mode don't know whats there or how it got there. 
                 ;;;      Don't try to erase the contents - Kill them. 
                  (if (and kill-em-before-they-grow
                           (buffer-live-p (get-buffer some-other-buffer)))
                      (progn 
                        (kill-buffer (get-buffer some-other-buffer))
                        (get-buffer-create some-other-buffer))
                        (get-buffer-create some-other-buffer))
                 ;; *mon-help-docstring-help-bffr* -> "*MON-HELP*"
                  (get-buffer-create*mon-help-docstring-help-bffr*)))))
    (with-help-window dhb 
      (with-current-buffer dhb
        (insert the-help-doc)
        (help-window-setup-finish (selected-window))))))
;;
;;; :TEST-ME (mon-help-temp-docstring-display
;;;           (documentation 'mon-help-temp-docstring-display))
) ;; :CLOSE unless
;;; ==============================

;;; ==============================
;;; (provide 'google-define-redux-supplemental)
;;; ==============================

;;; ====================================================================
;;; google-define-redux-supplemental.el ends here
;;; EOF
