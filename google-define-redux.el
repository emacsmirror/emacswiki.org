;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; this is google-define-redux.el
;;; ================================================================
;;; DESCRIPTION:
;;; google-define-redux provides extensions for google-define.el
;;; Extends google-define with fontification of definition buffer.
;;; No longer relies on `with-output-to-temp-buffer'
;;; Adds parsing tokens for programatically extracting definitions.
;;; FUNCTIONS:►►►
;;; `google-define-get-command', `google-define-parse-buffer', `google-define',
;;; `google-define-font-lock', `google-define-kill-def-buffers'
;;; FUNCTIONS:◄◄◄
;;;
;;; MACROS:
;;;
;;; METHODS:
;;;
;;; CLASSES:
;;;
;;; FACES:
;;; `gg-def-base', `gg-def-delim', `gg-def-defined', `gg-def-num',
;;; `gg-def-inition',
;;;
;;; CONSTANTS:
;;;
;;; VARIABLES:
;;; `*google-define-view-map*', `*url-get-buffer*', 
;;; `*google-define-buffer-suffix*'
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
;;; `mon-string-justify-left'          <- :FILE mon-utils.el
;;; `mon-buffer-exists-p'              <- :FILE mon-utils.el
;;; `mon-buffer-exists-so-kill'        <- :FILE mon-utils.el
;;; `mon-help-temp-docstring-display'  <- :FILE mon-doc-help-utils.el
;;; `mon-help-KEY-tag'                 <- :FILE mon-doc-help-utils.el
;;;
;;;
;;; THIRD-PARTY-CODE:
;;;
;;; AUTHOR: MON KEY
;;; MAINTAINER: MON KEY
;;;
;;; PUBLIC-LINK: (URL `http://www.emacswiki.org/emacs/google-define-redux.el')
;;; FIRST-PUBLISHED: <Timestamp: #{2010-02-04T21:39:15-05:00Z}#{10055} - by MON>
;;; 
;;; FILE-CREATED:
;;; <Timestamp: #{2010-02-03T13:41:11-05:00Z}#{10053} - by MON>
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
;;; the section entitled ``GNU Free Documentation License''.
;;; 
;;; A copy of the license is also available from the Free Software
;;; Foundation Web site at:
;;; (URL `http://www.gnu.org/licenses/fdl-1.3.txt').
;;; ================================================================
;;; Copyright © 2010 MON KEY 
;;; ==============================
;;; CODE:

(eval-when-compile (require 'cl))
(require 'google-define)

;;; ==============================
;;; :CREATED <Timestamp: #{2010-02-03T16:20:02-05:00Z}#{10053} - by MON>
(defvar *google-define-view-map* (cons (string-to-char "Q") 'View-kill-and-leave)
  "Keybinding for `view-mode' in google-define help buffers.
:SEE-ALSO `*google-define-get-command'\n.►►►")
;;
;;; :TEST-ME *google-define-view-map* 
;;;(progn (makunbound '*google-define-view-map*) (unintern '*google-define-view-map*))

;;; ==============================
;;; :CREATED <Timestamp: #{2010-02-03T13:43:28-05:00Z}#{10053} - by MON>
(defvar *google-define-get-buffer* nil 
  "*Name of buffer `google-define-get-command' should return results in.
:SEE-ALSO \n.►►►")
;;
(unless (bound-and-true-p *google-define-get-buffer*)
  (setq *google-define-get-buffer* (symbol-name '*google-define-get-buffer*)))
;;
;;; :TEST-ME *google-define-get-buffer*
;;;(progn (makunbound '*google-define-get-buffer*) (unintern '*google-define-get-buffer*) )

;;; ==============================
;;; :CREATED <Timestamp: #{2010-02-05T14:55:41-05:00Z}#{10055} - by MON>
(defvar *google-define-buffer-suffix* '("*" . ":gg-definition*")
  "*Prefix and suffix to attach to defition buffers created with `google-define'.\n
:CALLED-BY `google-define-kill-def-buffers' when cleaning up definition buffers.
:SEE-ALSO .\n►►►")
;;
;;; :TEST-ME *google-define-buffer-suffix*
;;;(progn (makunbound '*google-define-buffer-suffix*) (unintern '*google-define-buffer-suffix*) )

;;; ==============================
;;; `mon-help-KEY-tag' <- :FILE mon-doc-help-utils.el
;;; :CREATED <Timestamp: #{2010-02-04T20:22:51-05:00Z}#{10055} - by MON>
(defface gg-def-base
    (if (featurep 'mon-doc-help-utils)
        '((t :inherit mon-help-KEY-tag))
        '(( ((class color) (min-colors 88)) 
           (:foreground "light steel blue" :weight extrabold)) ))
  "*
:SEE-ALSO `gg-def-base', `gg-def-delim', `gg-def-defined', `gg-def-num',
`gg-def-inition'.\n►►►")
;;
;;; :TEST-ME (describe-face 'gg-def-base)
;;;(progn (makunbound 'gg-def-base) (unintern 'gg-def-base) )

;;; ==============================
;;; :CREATED <Timestamp: #{2010-02-05T16:06:13-05:00Z}#{10055} - by MON KEY>
(defface gg-def-delim '((t :inherit gg-def-base :foreground "sky blue"))
  "*Provides fontlocking of space seperated delmiter charcaters.\n
Delimiters characters include: ►, |, ◄ preceded and followed by whitespace e.g.:
 \" ► \" \" | \" \" ◄ \"\n
:SEE-ALSO `gg-def-base', `gg-def-delim', `gg-def-defined', `gg-def-num',
`gg-def-inition'.\n►►►")
;;
;;; :TEST-ME (describe-face 'gg-def-delim)
;;;(progn (makunbound 'gg-def-delim) (unintern 'gg-def-delim) )

;;; ==============================
;;; :CREATED <Timestamp: #{2010-02-05T16:06:16-05:00Z}#{10055} - by MON KEY>
(defface gg-def-num '((t :inherit gg-def-base  :foreground "powder blue"))
  "*Provides fontlocking of enumerated numbers preceding a definition.\n
:SEE-ALSO `gg-def-base', `gg-def-delim', `gg-def-defined', `gg-def-num',
`gg-def-inition'.\n►►►")
;;
;;; :TEST-ME (describe-face 'gg-def-num)
;;;(progn (makunbound 'gg-def-num) (unintern 'gg-def-num) )

;;; ==============================
;;; :CREATED <Timestamp: #{2010-02-05T16:06:20-05:00Z}#{10055} - by MON KEY>
(defface gg-def-inition
    '((t ;:inherit gg-def-base
       :foreground "wheat1" :weight normal))
  "*Provides fontlocking of definition content e.g. anything after \"    | \".\n
:SEE-ALSO `gg-def-base', `gg-def-delim', `gg-def-defined', `gg-def-num',
`gg-def-inition'.\n►►►")
;;
;;; :TEST-ME (describe-face 'gg-def-inition)
;;;(progn (makunbound 'gg-def-inition) (unintern 'gg-def-inition) )

;;; ==============================
;;; :TODO See notes below about matching alternate reasonable suffixes.
;;; :CREATED <Timestamp: #{2010-02-05T16:05:47-05:00Z}#{10055} - by MON KEY>
(defface gg-def-defined '((t :inherit gg-def-base :foreground "cadet blue"))
  "*Provides fontlocking of word defined by definition.
:SEE-ALSO `gg-def-base', `gg-def-delim', `gg-def-defined', `gg-def-num',
`gg-def-inition'.\n►►►")
;;
;;; :TEST-ME (describe-face 'gg-def-defined)
;;;(progn (makunbound 'gg-def-defined) (unintern 'gg-def-defined) )

;;; ==============================
;;; :MODIFICATIONS <Timestamp: #{2010-02-04T21:28:01-05:00Z}#{10055} - by MON>
(defun google-define-get-command (host path)
  "Retrieve google defnitions to process-buffer `*google-define-get-buffer*'.\n
:SEE-ALSO `google-define', `google-define-parse-buffer', `google-define-font-lock',
`google-define-kill-def-buffers'.\n►►►"
  (let* ((timeout 180)
         (port 80) ;; http
         (post-cmd
          (concat "GET " path " HTTP/1.0\r\n"
                  "Host: " host "\r\n"
                  "User-Agent: Emacs\r\n"
                  ;; (concat "User-Agent: Mozilla/5.0 "
                  ;;         "(Windows; U; Windows NT 5.0; en-US; rv:1.8.1.1) "
                  ;;         "Gecko/20061204 Firefox/2.0.0.1\r\n")
                  (concat "Accept: text/xml,application/xml,application/"
                          "xhtml+xml,text/html;q=0.9,text/plain;q=0.8,"
                          "image/png,*/*;q=0.5\r\n")
                  "Accept-Language: en-us,en;q=0.5\r\n"
                  "Accept-Encoding: gzip,deflate\r\n"
                  "Accept-Charset: ISO-8859-1;q=0.7,*;q=0.7\r\n"
                  "\r\n"))
         proc buf)
    (unwind-protect
         (progn
           (when (get-buffer *google-define-get-buffer*)
             (kill-buffer *google-define-get-buffer*))
           (setq proc (open-network-stream "url-get-command"
                                           *google-define-get-buffer*
                                           host
                                           port)
                 buf (process-buffer proc))
           (process-send-string proc post-cmd)
           (message "Getting information from %s: waiting for response..." host)
           (while (equal (process-status proc) 'open)
             (unless (accept-process-output proc timeout)
               (delete-process proc)
               (error "Server error: timed out while waiting!")))
           (message "Response received: processing...")))
    ;; unwind-protect
    buf))
;; (google-define-get-command)

;;; ==============================
;;; :MODIFICATIONS <Timestamp: #{2010-02-03T13:41:22-05:00Z}#{10053} - by MON>
(defun google-define-parse-buffer (search-word def-disp-buffer)
  "Pull all of the definitions out of the data returned from google.
Print in a temp-buffer, parse, and convert for presentation.\n
:SEE-ALSO `google-define', `google-define-get-command',
`google-define-parse-buffer', `google-define-font-lock',
`google-define-kill-def-buffers', `*google-define-get-buffer*',
`mon-string-justify-left'.\n►►►"
  (let* ((count 0)
         (accum-buffer  ;; <- *standard-output* is here
          (prog1 def-disp-buffer
            (when (buffer-live-p (get-buffer def-disp-buffer))
              (with-current-buffer (get-buffer def-disp-buffer)
                (let ((buffer-read-only nil))
                  (erase-buffer))))))
         ;; :NOTE `accum-buffer' _should_ be empty, don't bother checking.
         (standard-output (get-buffer-create accum-buffer)))
    (princ (format "Definitions for: %s\n\n" search-word))
    (set-buffer (google-define-get-command 
                 "www.google.com" 
                 (concat "/search?num=100&hl=en&q=define%3A%22"
                         (replace-regexp-in-string " +" "+" search-word)
                         "%22&btnG=Search")))
    (unwind-protect
      (goto-char (point-min))
      (while (search-forward-regexp "<li>\\([^<]+\\)" nil t)
        (incf count)
        (let ((definition 
               (replace-regexp-in-string "\\(\n\\|\r\\|\t\\)" "" (match-string 1))))
          (princ  ;; <- spitting to our *Definitions:<SEARCH-WORD>* buffer.
           (with-temp-buffer
             (let ((>pnt (make-marker))
                   (<pnt (make-marker))
                   (fill-column 68)
                   (fill-prefix "    | "))
               (save-excursion              
                 (insert (format "%3d ►\n" count))               
                 (set-marker >pnt (point))
                 (insert ;; :NOTE `mon-string-justify-left' <- :FILE mon-utils.el
                  (replace-regexp-in-string  
                   "^    " "    | " (mon-string-justify-left definition 64 4)))
                 (set-marker <pnt (point))
                 (insert "\n    ◄\n\n"))
               (fill-region-as-paragraph >pnt <pnt)
               (google-define-replace-html)
               (google-define-replace-unicode))
             ;; mon-help-PNTR-tag
             (buffer-substring (buffer-end 0) (buffer-end 1))))))
      ;; Close current TCP process of current `google-define-get-command' buffer.
        (delete-process nil))
    (set-buffer accum-buffer)
    ;; Make _sure_ we closed the process.
    (when (get-buffer *google-define-get-buffer*) 
      (kill-buffer *google-define-get-buffer*))
    (setq accum-buffer 
          (with-current-buffer accum-buffer 
            (google-define-font-lock search-word)
            (buffer-substring (buffer-end 0) (buffer-end 1))))
    ;; Put name of `accum-buffer' on the `count' var and kill that buffer too. 
    (setq count (current-buffer))
    (when (get-buffer count) (kill-buffer count))
    accum-buffer))

;;; ==============================
;;; :TODO The word definition fontlocking should also find the prefix of word 
;;; e.g. affixed -> "ed\\_w" "ing\\_w" "ing\\_w" ".tion\\_w" "edness\\_w"
;;; :CREATED <Timestamp: #{2010-02-04T21:33:14-05:00Z}#{10055} - by MON>
(defun google-define-font-lock (search-word)
  "Add fontification text-properties to the definitions.
Fontify with the following faces:
 `gg-def-base', `gg-def-num', `gg-def-delim',
 `gg-def-inition', `gg-def-defined'\n
:SEE-ALSO `google-define', `google-define-get-command',
`google-define-parse-buffer', `google-define-font-lock',
`google-define-kill-def-buffers'.\n►►►"
  (let* ((help-props
          `(("^Definitions for:" 0 (face gg-def-base)) 
            ("^ +\\([0-9]\\{1,2\\}\\) " 1 (face gg-def-num))
            (" \\(►\\||\\|◄\\) ?" 1 (face gg-def-delim))
            ("^    | \\(.*\\)$" 1 (face gg-def-inition))
            (,(concat " \\(" search-word "\\)[\\[:punct:]\\[:blank:]\n]") 1 (face gg-def-defined)))
             ))
      (mapc #'(lambda (x)
                (goto-char (buffer-end 0))
                (while (search-forward-regexp  (elt x 0) nil t)
                  (add-text-properties 
                   (match-beginning (elt x 1)) (match-end (elt x 1)) 
                   (elt x 2))))
            help-props))) ;; (font-lock-fontify-buffer)))

;;; ==============================
;;; :REQUIRES `mon-help-temp-docstring-display' <- :FILE mon-doc-help-utils.el
;;; :CREATED <Timestamp: #{2010-02-03T13:41:17-05:00Z}#{10053} - by MON>
(defun google-define (search-word &optional intrp)
  "Ask Google for the definition of a word.\n
Return and display definition in new buffer other-window.\n
Buffer name constructed by prepending car of `*google-define-buffer-suffix*'
to SEARCH-WORD and appending cdr of `*google-define-buffer-suffix*' e.g.:
\n *damned:gg-definitions*\n
When called-interactively prompt for SEARCH-WORD with \"Define: \"
If there is a word at point us it as default value for prompt.\n
:EXAMPLE\n\n\(google-define \"define\"\)\n
\(google-define nil t\)define\n
:SEE-ALSO `google-define-font-lock', `google-define-parse-buffer',
`google-define-kill-def-buffers', `google-define-get-command',
`mon-help-temp-docstring-display',`*google-define-view-map*' `gg-def-base',
`gg-def-num', `gg-def-delim', `gg-def-inition', `gg-def-defined'.\n►►►"
  (interactive "i\np")
  (let* ((sw (cond (intrp (read-from-minibuffer "Define: "
                                                (thing-at-point 'word)))
                   (search-word search-word)))
         (header (concat "Definitions for " sw))
         (dfb (concat (car *google-define-buffer-suffix*)
                      sw
                      (cdr *google-define-buffer-suffix*)))
         (defs (google-define-parse-buffer sw dfb)))
    (prog1 (message header)
      (mon-help-temp-docstring-display defs dfb t))))
;;
;;; :TEST-ME (google-define "define")
;;; :TEST-ME (google-define nil t)define

;;; ==============================
;;; :NOTE `mon-buffer-exists-p' is macro in :FILE mon-utils.el 
;;; If that feature isn't preste we should provide it here now.
;;; :BEFORE `google-define-kill-def-buffers'
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
;;; :REQUIRES `mon-buffer-exists-p'
;;; :REQUIRES `mon-buffer-exists-so-kill'
;;; :CREATED <Timestamp: #{2010-02-05T14:45:57-05:00Z}#{10055} - by MON>
(defun google-define-kill-def-buffers (&optional intrp)
  "Kill all google define buffers with `*google-define-buffer-suffix*'.\n
Return list of buffer names killed.\n
When called-interactively message with the buffers killed.\n
:EXAMPLE\n\n\(save-window-excursion
  \(dolist \(i '\(\"damned\" \"data\" \"delta\"\) 
           \(google-define-kill-def-buffers t\)\)
    \(google-define i\)\)\)\n
:SEE-ALSO `google-define', `google-define-get-command',
`google-define-parse-buffer', `google-define-font-lock',
`google-define-kill-def-buffers', `mon-buffer-exists-p',
`mon-buffer-exists-so-kill'.\n►►►"
  (let ((buffs (mapcar #'(lambda (b) (buffer-name (get-buffer b))) (buffer-list)))
        (def-buffs))
    (mapc #'(lambda (mb) 
              (when ;; (string-match ".*:gg-definition\\*" mb)
                  (string-match (concat ".*" (cdr *google-define-buffer-suffix*)))
                (push mb def-buffs)))
          buffs)
    (setq buffs nil)  
    (mapc #'(lambda (kb) 
              (when (mon-buffer-exists-so-kill kb)
                (push kb buffs)))
          def-buffs)
    (if intrp 
        (message (mapconcat #'(lambda (ded) (format "Killed buffer %s" ded))
                            buffs "\n"))
        buffs)))
;;
;;; :TEST-ME (google-define-kill-def-buffers t) 
;;; :TEST-ME (save-window-excursion
;;;            (dolist (i '("damned" "data" "delta") 
;;;                     (google-define-kill-def-buffers t))
;;;              (google-define i)))


;;; ==============================
;; `mon-buffer-exists-so-kill' <- :FILE mon-untils.el
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
When optional arg SOME-OTHER-BUFFER is non-nil display THE-HELP-DOC in that buffer instead.
When optional arg KILL-EM-BEFORE-THEY-GROW is non-nil kill any existing
SOME-OTHER-BUFFER with name before displaying contentsthere.
Useful when temporarily editing docstrings.\n
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
                 *mon-help-docstring-help-bffr*))))
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
(provide 'google-define-redux)
;;; ==============================

;;; ================================================================
;;; google-define-redux.el ends here
;;; EOF
