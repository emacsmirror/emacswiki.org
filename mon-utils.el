;;; This is mon-utils.el
;;; ================================================================
;;; DESCRIPTION:
;;; Provides common utilities and BIG require for other of MON's packages.
;;;
;;; FUNCTIONS:###
;;; FUNCTIONS:►►►
;;; `scratch', `switch-to-messages',
;;; `scroll-down-in-place', `scroll-up-in-place', `mon-kill-appending',
;;; `mon-kill-completions', `mon-flip-windows', `mon-twin-horizontal',
;;; `mon-twin-vertical', `mon-what-face', `mon-toggle-menu-bar',
;;; `mon-append-to-register', `mon-append-to-buffer', `mon-region-position',
;;; `mon-region-length', `mon-region-unfill', `mon-region-capitalize',
;;; `mon-region-reverse', `mon-trunc', `mon-inhibit-read-only',
;;; `mon-inhibit-modification-hooks', `mon-inhibit-point-motion-hooks',
;;; `mon-toggle-read-only-point-motion', `mon-wrap-selection',
;;; `mon-wrap-text', `mon-wrap-with', `mon-choose-from-menu',
;;; `mon-match-at-point', `mon-spacep', `mon-spacep-not-bol',
;;; `mon-spacep-is-bol', `mon-spacep-is-after-eol',
;;; `mon-spacep-is-after-eol-then-graphic', `mon-spacep-at-eol',
;;; `mon-spacep-first', `mon-line-bol-is-eol',
;;; `mon-line-previous-bol-is-eol', `mon-line-next-bol-is-eol',
;;; `mon-line-eol-is-eob', `mon-line-end-or-code-end', `mon-line-get-next',
;;; `mon-line-count-region', `mon-line-count-matchp', `mon-line-length-max',
;;; `mon-is-digit', `mon-is-letter', `mon-is-alphanum',
;;; `mon-is-digit-simp', `mon-is-letter-simp', `mon-is-alphanum-simp'
;;; `mon-string-justify-left', `mon-string-to-sequence',
;;; `mon-string-from-sequence', `mon-string-alpha-list', `mon-string-index',
;;; `mon-string-position', `mon-string-has-suffix', `mon-string-ify-list',
;;; `mon-string-split-on-regexp', `mon-string-sub-old->new',
;;; `mon-string-split-line', `mon-string-ify-current-line',
;;; `mon-get-word-list-buffer', `mon-word-get-next',
;;; `mon-word-reverse-region', `mon-word-iterate-over',
;;; `mon-word-count-analysis', `mon-word-count-occurrences',
;;; `mon-word-count-region', `mon-word-count-chars-region',
;;; `mon-rectangle-columns', `mon-rectangle-sum-column',
;;; `mon-rectangle-operate-on', `mon-rectangle-apply-on-region-points',
;;; `mon-rectangle-downcase', `mon-rectangle-upcase',
;;; `mon-rectangle-capitalize', `mon-line-test-content', `mon-test-props',
;;; `mon-view-help-source', `mon-index-elisp-symbol',
;;; `mon-plist-keys', `mon-list-all-properties-in-buffer',
;;; `mon-nuke-text-properties-buffer', `mon-remove-text-property',
;;; `mon-remove-single-text-property', `mon-nuke-text-properties-region',
;;; `mon-elt->', `mon-elt-<', `mon-elt->elt', `mon-elt-<elt',
;;; `mon-flatten', `mon-combine', `mon-recursive-apply',
;;; `mon-escape-lisp-string-region', `mon-unescape-lisp-string-region',
;;; `mon-princ-cb', `mon-eval-sexp-at-point', `mon-eval-print-last-sexp',
;;; `mon-extend-selection', `mon-semnav-up', `mon-eval-expression',
;;; `mon-nuke-and-eval', `mon-unbind-defun', `mon-unbind-symbol',
;;; `mon-unbind-function', `mon-unbind-command', `mon-unbind-variable',
;;; `mon-byte-compile-and-load', `mon-compile-when-needed',
;;; `mon-load-or-alert', `mon-cmd', `mon-terminal', `mon-string-to-symbol'
;;; `mon-line-find-duplicates', `mon-test-keypresses'
;;; `mon-line-strings-to-list', `mon-line-strings-to-list-*test*'
;;; `mon-line-string-rotate-name', `mon-line-string-rotate-namestrings'
;;; `mon-line-string-unrotate-namestrings', `mon-symbol-to-string'
;;; `mon-line-string-rotate-namestrings-combine'
;;; `mon-sublist', `mon-sublist-gutted', `mon-map-append'
;;; `mon-string-chop-spaces', `mon-maptree', `mon-transpose', `plist-remove'
;;; `mon-dump-object-to-file'`mon-string-upto-index',
;;; `mon-string-after-index', `mon-line-strings-bq-qt-sym-bol'
;;; FUNCTIONS:◄◄◄
;;; FUNCTIONS:###
;;; 
;;; MACROS:
;;;
;;; METHODS:
;;;
;;; CLASSES:
;;;
;;; CONSTANTS:
;;;
;;; VARIABLES:
;;;
;;; ALIASED:
;;; `mon-string-combine-and-quote' -> `combine-and-quote-strings'
;;; `mon-string-split-and-unquote' -> `split-string-and-unquote'
;;; `mon-string->symbol'           -> `mon-string-to-symbol'
;;; `mon-symbol->string'           -> `mon-symbol-to-string'
;;; `mon-string-from-symbol'       -> `mon-symbol-to-string'
;;; `mon-string<-symbol'           -> `mon-symbol-to-string'
;;;
;;; MACROS:
;;; `mon-foreach', `mon-for', `mon-loop', `mon-moveq'
;;;
;;; DEPRECATED:
;;; `mon-string-from-sequence2' ;removed
;;;
;;; RENAMED:
;;; `mon-stringify-list' -> `mon-string-ify-list'
;;; `mon-split-string-line' -> `mon-string-split-line'
;;;
;;; MOVED:
;;; `mon-coerce->char'                  -> mon-empty-registers.el
;;; `mon-decode-meta-key-event'         -> mon-empty-registers.el
;;; `mon-catch-meta-key'                -> mon-empty-registers.el
;;; `mon-cmd'                           <- default-start-loads.el
;;; `mon-terminal'                      <- default-start-loads.el
;;; `mon-index-elisp-symbol'            -> mon-doc-help-utils.el
;;;
;;;
;;; REQUIRES:
;;; `mon-word-iterate-over' `mon-loop' -> CL
;;;
;;; TODO:
;;;
;;; NOTES:
;;;
;;; SNIPPETS:
;;;
;;; THIRD PARTY CODE:
;;;
;;; AUTHOR: MON KEY
;;; MAINTAINER: MON KEY
;;;
;;; PUBLIC-LINK: (URL `http://www.emacswiki.org/emacs/mon-utils.el')
;;; FIRST-PUBLISHED:
;;;
;;; FILE-CREATED:
;;; <Timestamp: Autumn 2008 - by MON KEY>
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

;;; ==============================
;; `mon-word-iterate-over', `mon-loop', etc. 
(eval-when-compile (require 'cl)) 

;;; ==============================
(require 'mon-regexp-symbols)
(require 'mon-time-utils)
(require 'naf-mode-replacements) ;;BEFORE mon-dir-utils, BEFORE: naf-mode-insertion-utils
(require 'mon-dir-locals-alist)
(require 'mon-dir-utils)
(require 'mon-insertion-utils)
(require 'naf-mode-insertion-utils)
(require 'mon-url-utils)
(require 'mon-hash-utils)
(require 'mon-doc-help-utils)
(require 'mon-doc-help-CL)
(require 'naf-skeletons)
(require 'naf-mode)
(require 'ebay-template-mode)
(require 'mon-empty-registers)

;;; ==============================
;;; EMACSWIKI: Enable 'em if you got 'em.
;;; ==============================
;;; (require 'mon-regexp-symbols)
;;; (require 'mon-time-utils)
;;; (require 'naf-mode-replacements) ;before dir-utils
;;; (require 'mon-dir-locals-alist)
;;; (require 'mon-dir-utils)
;;; (require 'mon-insertion-utils)
;;; (require 'naf-mode-insertion-utils)
;;; (require 'mon-hash-utils)
;;; (require 'mon-doc-help-utils)
;;; (require 'mon-doc-help-CL)
;;; (require 'naf-skeletons)
;;; (require 'naf-mode)
;;; ==============================

;;; ==============================
;;; :CREATED <Timestamp: #{2009-10-06T16:04:09-04:00Z}#{09412} - by MON KEY>
(defun mon-load-cedet ()
  "Load CEDET if it isn't already.
Alias 'slot-makunbound -> `slot-makeunbound'
This function will be :DEPRECATED once EMACS <-> CEDET merge is complete."
  (interactive)
(progn
  (if (and IS-MON-P (not (featurep 'cedet)))
      (load-file  (concat mon-site-lisp-root "/cedet-cvs/common/cedet.el")))
  (message "CEDET already loaded or your of a MONish way.")
  ;; :REMOVE-ME once slot-makeunbound is removed/renamed/aliased in cedet/eieio.el
  (unless (fboundp 'slot-makunbound)
    (defalias 'slot-makunbound 'slot-makeunbound))))
  
;;; ==============================
(defun mon-terminal ()
  "When `gnu-linuxp' launch a terminal. 
When `win32p' launch Cygwin Bash in cmd console.\n
:SEE-ALSO `mon-cmd' which when win32p returns the NT Command console.\n►►►"
  (interactive)
  (cond 
   (IS-BUG-P (message "you don't have the goods for this"))
   (IS-MON-P-W32 (w32-shell-execute "open" "C:\\Cygwin.bat"))
   (IS-MON-P-GNU (shell-command "terminal"))))

;;; ==============================
(defun mon-cmd ()
  "When `win32p' launch the NT Command console. 
When `gnu-linuxp' return a terminal.\n
:SEE-ALSO `mon-terminal' which when `win32p' gives a cygwin bash shell wrapped
in a cmd console.\n►►►"
  (interactive)
  (cond (win32p (w32-shell-execute "open" "cmd"))
        (gnu-linuxp (shell-command "terminal"))))

;;; ==============================
(defun mon-firefox (url)
  "Jump to the running firefox and open URL in new tab.\n
:SEE-ALSO `browse-url-firefox-program',`mon-conkeror',
`browse-url-generic-program', `browse-url-browser-function',
`browse-url-generic'.\n►►►"
  (interactive "sGIMME A URL:")
  (browse-url-firefox url))

;;; ==============================
(defun mon-conkeror (url)
  "Launch the conkeror web browser with URL.
To enusre Emacs gets existing conkeror process 
put following in conkerorrc file:
 url_remoting_fn = load_url_in_new_buffer;
 require\(\"clicks-in-new-buffer.js\");\n
:SEE-ALSO `mon-firefox', `browse-url-firefox-program',
`browse-url-generic-program',`browse-url-browser-function',
`browse-url-generic'.\n►►►"
  (interactive "sWhat Url:")
  (if (string-match "conkeror" browse-url-generic-program)
      (cond 
       (IS-MON-P-W32 (browse-url-generic url))
       (IS-MON-P-GNU (browse-url-generic url))
       (IS-BUG-P "Do you have Conkeror installed?"))
    (error "This function requires conkeror be set as browse-url-generic-program")))

;;; ==============================
(defun scratch ()
  "Switch to *scratch* buffer.
Get \(or create\) a *scratch* buffer now!\n►►►"
  (interactive)
  (switch-to-buffer "*scratch*")
  ;;(lisp-interaction-mode)
  (if current-prefix-arg
      (delete-region (point-min) (point-max))
    (goto-char (point-max))))

;;; ==============================
(defun switch-to-messages ()
  "Select buffer *message* in the current window.\n►►►"
  (interactive)
  (switch-to-buffer "*Messages*"))

;;; ==============================
(defun scroll-down-in-place (n)
  "Scroll with the cursor in place, moving the UP page instead.\n►►►"
  (interactive "p")
  (previous-line n)
  (scroll-down n))

;;; ==============================
(defun scroll-up-in-place (n)
  "Scroll with the cursor in place, moving the DOWN page instead.\n►►►"
  (interactive "p")
  (next-line n)
  (scroll-up n))

;;; ==============================
;;; :CREATED <Timestamp: Friday March 20, 2009 @ 09:17.35 PM - by MON KEY>
(defun mon-kill-appending (beg end)
  "Append the region current to the kill ring without killing it. 
Like `append-next-kill' but skips the C M-w M-w finger-chord hoop jump.\n►►►"
  (interactive "r")
  (progn 
    (append-next-kill)
    (kill-ring-save beg end)))

;;; ==============================
;;; :CREATED <Timestamp: Thursday March 05, 2009 @ 04:49.29 PM - by MON KEY>
(defun mon-kill-completions ()
  "Kill *Completions* buffer without leaving point.\n►►►"
  (interactive)
  (save-excursion
    (when (get-buffer-window "*Completions*")
      (progn
	(switch-to-completions)
	(delete-completion-window)))))

;;; =======================
(defun mon-flip-windows ()
  "Swap current buffer display with buffer in other window.
:SEE-ALSO `mon-twin-vertical', `mon-twin-horizontal'.\n►►►"
  (interactive)
  (let ((cur-buffer (current-buffer))
        (top-buffer)
        (bottom-buffer))
    (pop-to-buffer (window-buffer (frame-first-window)))
    (setq top-buffer (current-buffer))
    (other-window 1)
    (setq bottom-buffer (current-buffer))
    (switch-to-buffer top-buffer)
    (other-window -1)
    (switch-to-buffer bottom-buffer)
    (pop-to-buffer cur-buffer)))

;;; ==============================
;;; :COURTESY Francois Fleuret <fleuret@idiap.ch> :HIS fleuret.emacs.el
;;; (URL `http://www.idiap.ch/~fleuret/files/fleuret.emacs.el')
;;; :WAS `ff/twin-horizontal-current-buffer' -> `mon-twin-horizontal'
(defun mon-twin-horizontal () 
  "Split current-buffer horizontally.
:SEE-ALSO `mon-twin-vertical', `mon-flip-windows'.\n►►►"
  (interactive)
  (delete-other-windows)
  (split-window-horizontally)
  (balance-windows))
;;
;;; :WAS `ff/twin-vertical-current-buffer' -> `mon-twin-vertical'
(defun mon-twin-vertical () 
  "Split current-buffer vertically.
:SEE-ALSO `mon-twin-horizontal', `mon-flip-windows'.\n►►►"
  (interactive)
  (delete-other-windows)
  (split-window-vertically)
  (balance-windows))

;;; ==============================
;;; :COURTESY Miles Bader <miles@lsi.nec.co.jp>, (gnus.emacs.help)
(defun mon-what-face (pos)
  "Return the font-lock face information at the current point.\n►►►"
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
		  (get-char-property (point) 'face))))
    (if face
	(message "Face: %s" face)
      (message "No face at %d" pos))))

;;; ==============================
(defun mon-toggle-menu-bar ()
  "Toggle the top menu bar.\nGets the max editor screen for your money!\n►►►"
  (interactive)
  (let ((height (frame-height)))
    (menu-bar-mode nil)
    (set-frame-height 
     (selected-frame)
     (if menu-bar-mode
	 (1- height)
       (1+ height)))
    (force-mode-line-update t)))

;;; ==============================
;;; :COURTESY Thierry Volpiatto :HIS tv-utils.el :WAS `tv-append-to-register'
;;; :CREATED <Timestamp: Tuesday June 16, 2009 @ 07:09.33 PM - by MON KEY>
(defun mon-append-to-register (register start end &optional delete-region-p)
  "Append region to text in register REGISTER.
When non-nil prefix arg DELETE-REGION-P will delete region as well.
Called programaticaly, takes four args: REGISTER, START, END and DELETE-REGION-P.
START and END are buffer positions indicating what to append.
Redefines `append-to-register' with a \"\n\".
:SEE-ALSO `mon-append-to-buffer'.\n►►►"
  (interactive "cAppend to register: \nr\nP")
  (let ((reg (get-register register))
        (text (filter-buffer-substring start end)))
    (set-register
     register (cond ((not reg) text)
                    ((stringp reg) (concat reg "\n" text))
                    (t (error "Register does not contain text")))))
  (if delete-region-p (delete-region start end)))

;;; ==============================
;;; :CREATED <Timestamp: Thursday June 18, 2009 @ 11:26.02 AM - by MON KEY>
(defun mon-append-to-buffer (buffer start end)
  "Append to specified buffer the text of the region.
It is inserted into that buffer before its point.\n
When calling from a program, give three arguments:
BUFFER (or buffer name), START and END.
START and END specify the portion of the current buffer to be copied.
Redefines `append-to-buffer' with a \"\n\".
:SEE-ALSO `mon-append-to-register'.\n►►►"
  (interactive
   (list (read-buffer "Append to buffer: " (other-buffer (current-buffer) t))
	 (region-beginning) (region-end)))
  (let ((oldbuf (current-buffer)))
    (save-excursion
      (let* ((append-to (get-buffer-create buffer))
	     (windows (get-buffer-window-list append-to t t))
	     point)
	(set-buffer append-to)
	(setq point (point))
	(barf-if-buffer-read-only)
	(newline)  ;added newline else identical to append-to-buffer
	(insert-buffer-substring oldbuf start end)
	(dolist (window windows)
	  (when (= (window-point window) point)
	    (set-window-point window (point))))))))

;;; ==============================
(defun mon-region-position ()
  "Return the postion of current region. 
A stupid and mostly useless function.\n►►►"
  (interactive)
  (message "current reg-beg is: %s reg-end is: %s" (region-beginning) (region-end)))

;;; ==============================
(defun mon-region-length ()
  "Return the regions length.\n►►►"
  (interactive)
  (message "Region length=%s" (- (region-end) (region-beginning))))

;;; ==============================
(defun mon-region-unfill (start end)
  "Do the opposite of fill-region.
Stuff all paragraphs paragraphs in the current region into long lines.\n►►►"
  (interactive "r")
  (let ((fill-column 9000))
    (fill-region start end)))

;;; ==============================
(defun mon-region-capitalize (start end)
  "Capitalize the region. 
\"mon-\" funcution name wrapper for consistency, and to aid completion
because we also have `mon-rectangle-capitalize'.
This function is a 1:1 duplicate of `capitalize-region'.\n►►►"
  (interactive "r")
  (capitalize-region start end))

;;; ==============================
(defun mon-region-reverse (beg end)
  "Reverses the characters in the region. 
:SEE-ALSO `mon-word-reverse-region'.\n►►►"
  (interactive "r")
  (insert (apply 'concat (reverse (split-string (delete-and-extract-region beg end) "")))))

;;; ==============================
(defun mon-trunc ()
  "Toggle the truncate-line variable and redraw the display.\n►►►"
  (interactive)
  (toggle-truncate-lines nil)
  (message
   (if truncate-lines
       "truncating lines (... $)"
     "wrapping lines (...\\)")
   (redraw-display)))

;;; ==============================
;;; NOTES: consider macrology?
;;; Working-but BUGGGY as of:
;;; :CREATED <Timestamp: #{2009-09-09T12:29:52-04:00Z}#{09373} - by MON>
(defun mon-test-keypresses (&optional first second third)
  "Used to test if additioanl optional Prefix args have been passed to interactive.\n
:EXAMPLE\nM-34 M-x mon-test-keypresses\n
=> \(\(meta . 51\) \(meta . 52\) \(meta . 120\) mon-test-keypresses\)\n►►►"
  (interactive "P\nP\np")
  (let ((accum-string '())
	(accum-event '())
	(self 'mon-test-keypresses))
    (mapc (lambda (x) 
            (cond ((= x 13) nil)
                  ((or (eql (car (event-modifiers x)) 'meta)
                       (eql (car (event-modifiers x)) 'control))
                   (setq accum-event (cons (cons (car (event-modifiers x)) (event-basic-type x)) accum-event)))
                  (t (setq accum-string (cons (char-to-string (event-basic-type x)) accum-string)))))
          (this-command-keys-vector))
    (setq accum-event (reverse accum-event))
    (setq accum-string (reverse accum-string))
    (setq accum-string (apply 'concat accum-string))
    (setq accum-string `(,@accum-event ,(if (string= accum-string self) self accum-string)))
        (prin1 accum-string)))

;;; :TEST-ME (mon-test-keypresses 1 2 3) ;->("cj")("cj")
;;; :TEST-ME (call-interactively 'mon-test-keypresses);-> ("cj")("cj")

;;; ==============================
(defun mon-inhibit-read-only (func-arg)
  "Evaluate FUNC-ARG at point with `inhibit-read-only' t.
Evaluation occurs inside an unwind protect so 'safe-enough' 
for invoking 'one off functions' such-as `kill-line' without
the tedium of building the entire scaffolding.
:SEE-ALSO `mon-inhibit-modification-hooks', `mon-inhibit-point-motion-hooks',
`mon-toggle-read-only-point-motion'.\n►►►"
(let ((re-inhibit (if (not inhibit-read-only) t nil)))
  (unwind-protect
      (progn 
	(setq inhibit-read-only t)
	(eval `(,func-arg)))
    (when re-inhibit (setq inhibit-read-only nil)))))

;;;(let (tt) (setq tt (propertize "I'm read only!" 'read-only t)) (newline)(insert tt))
;;; :TEST-ME (progn (line-move -1) (beginning-of-line) (mon-inhibit-read-only 'kill-line))

;;; ==============================
(defun mon-inhibit-modification-hooks (func-arg)
  "Evaluate FUNC-ARG at point with `inhibit-modification-hooks' t.
Evaluation occurs inside an unwind protect so 'safe-enough' 
for invoking 'one off functions' such-as `kill-line' without
the tedium of building the entire scaffolding.
:SEE-ALSO `mon-inhibit-read-only', `mon-inhibit-point-motion-hooks',
`mon-toggle-read-only-point-motion'.\n►►►"
(let ((re-inhibit (if (not inhibit-modification-hooks) t nil)))
  (unwind-protect
      (progn 
	(setq inhibit-modification-hooks t)
	(eval `(,func-arg)))
    (when re-inhibit (setq inhibit-modification-hooks nil)))))

;;; ==============================
(defun mon-inhibit-point-motion-hooks (func-arg)
  "Evaluate FUNC-ARG at point with `inhibit-point-motion-hooks' t.
Evaluation occurs inside an unwind protect so 'safe-enough' 
for invoking 'one off functions' such-as `kill-line' without
the tedium of building the entire scaffolding.
:SEE-ALSO `mon-inhibit-read-only', `mon-inhibit-modification-hooks',
`mon-toggle-read-only-point-motion'.\n►►►"
  (let ((re-inhibit (if (not inhibit-point-motion-hooks) t nil)))
    (unwind-protect
	(progn 
	  (setq inhibit-point-motion-hooks t)
	  (eval `(,func-arg)))
      (when re-inhibit (setq inhibit-point-motion-hooks nil)))))

;;; ==============================
;;; :CREATED <Timestamp: Monday June 15, 2009 @ 05:36.12 PM - by MON KEY>
(defun mon-toggle-read-only-point-motion ()
"Toggle `inhibit-read-only' and `inhibit-point-motion-hooks'.
:SEE-ALSO `mon-inhibit-read-only', `mon-inhibit-point-motion-hooks',
`mon-inhibit-modification-hooks'.\n►►►"
  (interactive)
  (if (or
       (bound-and-true-p inhibit-read-only)
       (bound-and-true-p inhibit-read-only))
      (progn
	(setq inhibit-read-only nil)
	(setq inhibit-point-motion-hooks nil))
    (progn
      (setq inhibit-read-only t)
      (setq inhibit-point-motion-hooks t))))

;;; ==============================
;;; :COURTESY Stefan Reichor <stefan@xsteve.at> :HIS xsteve-functions.el
(defun mon-wrap-selection (&optional front-arg rear-arg)
  "Wraps contents region with a front and rear delimeter.\n\n
 Prompts for: 
            Front Delimiter:  <--- Delmiter for beginning of region
            Rear Delimiter:  <--- Delmiter for end of region\n\n
 Example:
          Point/Mark of region contain: My cats breath smells like cat food
          Front's prompt is provided: |[
          Rear's prompt is provided:  ]|
          Return: |[My cats breath smells like catfood]|
:SEE-ALSO `mon-wrap-url', `mon-wrap-span', `mon-wrap-text', `mon-wrap-with'.\n►►►"
  (interactive)
  (let* ((front (or front-arg (read-string "Front Delimiter: ")))
         (rear (or rear-arg (read-string "Rear Delimiter: "))))
    (if mark-active
        (progn
          (save-excursion
            (goto-char (region-beginning))
            (insert front))
          (save-excursion
            (goto-char (region-end))
            (insert rear)))
      (insert front)
      (save-excursion
        (insert rear)))))

;;; ==============================
(defun mon-wrap-text (aa bb)
  "Wrap string args AA and BB around current word or region.\n
:SEE-ALSO `mon-wrap-selection', `mon-wrap-url', `mon-wrap-span' 
`mon-wrap-with'.\n►►►"
  (save-excursion
    (let (p1 p2 word-to-wrap)
      (if (and transient-mark-mode mark-active)
          (progn (setq p1 (region-beginning)) (setq p2 (region-end)))
        (progn
          (skip-chars-backward "-A-Za-z")
          (setq p1 (point))
          (skip-chars-forward "-A-Za-z")
          (setq p2 (point))))
      (setq word-to-wrap (buffer-substring-no-properties p1 p2))
      (goto-char p2) (insert bb)
      (goto-char p1) (insert aa))))

;;; stub to remind us where we're going 
;; (defun mon-wrap-artist-name ()
;; (interactive)
;; (mon-wrap-text "\\@:artist[" "]"))

;;; ==============================
(defun mon-wrap-with (front-wrap back-wrap)
  "Wrap the current word or region with supplied args FRONT-WRAP and BACK-WRAP.\n
:SEE-ALSO `mon-wrap-selection', `mon-wrap-url', `mon-wrap-span',
`mon-wrap-text', `mon-wrap-with'.\n►►►"
  (interactive "sEnter String for front-wrap:\nsEnter String for back-wrap:")
  (mon-wrap-text front-wrap back-wrap))

;;; ==============================
;;; :COURTESY Sandip Chitale <sandipchitale@attbi.com>
(defun mon-choose-from-menu (menu-title menu-items)
  "Choose from a list of choices from a popup menu.\n►►►"
  (let ((item)
        (item-list))
    (while menu-items
      (setq item (car menu-items))
      (if (consp item)
          (setq item-list (cons (cons (car item) (cdr item) ) item-list))
        (setq item-list (cons (cons item item) item-list)))
      (setq menu-items (cdr menu-items)))
    (x-popup-menu t (list menu-title (cons menu-title (nreverse item-list))))))

;;; ==============================
;;; :COURTESY Andy Stewart <lazycat.manatee@gmail.com> :WAS `match-at-point'
;;; (URL `http://www.emacswiki.org/emacs/lazycat-toolkit.el')
;;; :CREATED <Timestamp: Wednesday June 03, 2009 @ 06:18.14 PM - by MON KEY>
(defun mon-match-at-point (regexp)
  "Return the buffer substring around point matching REGEXP.
Look for a match starting at or before point.  Move back a character
at a time while still looking at a match ending at the same point.  If
no match is found at or before point, return the first match after
point, or nil if there is no match in the buffer.\n►►►"
  (let ((backup nil) (start nil) (end nil))
    (save-excursion
      (setq backup
            (or (looking-at regexp)
                (and (re-search-forward regexp nil 'limit)
                     (setq end t)
                     (goto-char (match-beginning 0))
                     nil)
                ;; failed search doesn't change match-data
                (re-search-backward regexp nil t)))
      (if (or backup end) (setq start (match-beginning 0)
                                end (match-end 0)))
      (if backup
          (while (and (not (bobp))
                      (progn (backward-char) t)
                      (looking-at regexp)
                      (= (match-end 0) end))
            (setq start (point)))
        (or (bobp) (re-search-forward regexp nil t))))
    (and start
         (progn (goto-char end) t)
         (buffer-substring start end))))

;;; ===============================================
;;; Whitespace EOL, BOL, EOB, BOB, LEP, LBP, etc.
;;; ===============================================

;;; ==============================
;;; :COURTESY Pascal J. Bourguignon :HIS pjb-emacs.el :WAS `space-p'
;;; :MODIFICATIONS <Timestamp: Tuesday February 10, 2009 @ 04:11.49 PM - by MON KEY>
(defun mon-spacep (&optional pt after)
  "Return t when char before point is a 'space' character.
If non-nil, PT (a char position) returns t for a'space' before/after PT.
If AFTER is non-nil return t when char after point is a 'space'.\n
:SEE-ALSO `mon-spacep-is-bol', `mon-spacep-not-bol',
`mon-spacep', `mon-line-bol-is-eol', `mon-line-next-bol-is-eol',
`mon-line-previous-bol-is-eol', `mon-spacep-is-after-eol',
`mon-cln-spc-tab-eol'.\n►►►"
  (let* ((look-pt (and pt pt))
	 (pc (cond
	      ((and pt after) 
	       (char-after look-pt))
	      ((and after)
	       (char-after))
	      ((and pt)
	       (char-before look-pt))
	      ((and (not pt) (not after))
	       (char-before))))
	 (space-char '(9 10 11 12 13 32))
	 (test-char (member pc space-char)))
    (when test-char t)))

;;; ==============================
;;; :CREATED <Timestamp: Thursday May 07, 2009 @ 03:11.19 PM - by MON KEY>
(defun mon-spacep-not-bol (&optional intrp)
  "t if character after point at BOL is not a space.\n
:SEE-ALSO `mon-spacep-is-bol', `mon-spacep', `mon-line-bol-is-eol',
`mon-spacep-is-after-eol', `mon-spacep-is-after-eol-then-graphic',
`mon-spacep-at-eol', `mon-cln-spc-tab-eol'.\n►►►"
(interactive "p")
  (let* ((char-bol (char-after (point-at-bol)))
	 (space-char '(9 10 11 12 13 32))
	 (not-space (not (member char-bol space-char))))
      (cond (intrp
	 (if not-space
	     (message "Char after point at Beginning of Line _NOT_ whitespace.")
	   (message "Char after point at Beginning of Line IS whitespace."))))
    not-space))
 
;;; :TEST-ME  (format "%s" (not-spacep-bol))

;;; ==============================
;;; :CREATED <Timestamp: Thursday May 07, 2009 @ 03:11.19 PM - by MON KEY>
(defun mon-spacep-is-bol (&optional intrp)
  "t if character after point at BOL _is_ a space.\n
:SEE-ALSO `mon-spacep-not-bol', `mon-spacep', `mon-line-bol-is-eol', 
`mon-line-next-bol-is-eol', `mon-line-previous-bol-is-eol',
`mon-spacep-is-after-eol', `mon-spacep-is-after-eol-then-graphic',
`mon-spacep-at-eol', `mon-cln-spc-tab-eol'.\n►►►"
(interactive "p")
  (let* ((char-bol (char-after (point-at-bol)))
	 (space-char '(9 10 11 12 13 32))
	 (is-space (numberp (car (member char-bol space-char)))))
      (cond (intrp
	 (if is-space
	     (message "Char after point at Beginning of Line IS whitespace.")
	   (message "Char after point at Beginning of Line _NOT_ whitespace."))))
    is-space))
 
;;; :TEST-ME  (format "%s" (mon-spacep-is-bol))

;;; ==============================
;;; :CREATED <Timestamp: Thursday May 07, 2009 @ 05:39.17 PM - by MON KEY>
(defun mon-spacep-is-after-eol (&optional intrp)
  "t if character after eol _is_ a space.\n
:SEE-ALSO `mon-spacep-is-bol', `mon-spacep-not-bol',
`mon-spacep', `mon-line-bol-is-eol', `mon-line-next-bol-is-eol',
`mon-line-previous-bol-is-eol', `mon-spacep-is-after-eol-then-graphic',
`mon-spacep-at-eol', `mon-cln-spc-tab-eol'.\n►►►"
(interactive "p")
  (let* ((after-eol (char-after (1+ (line-end-position))))
	 (space-char '(9 10 11 12 13 32))
	 (is-space (numberp (car (member after-eol space-char))))
	 (rtrn is-space))
      (cond (intrp
	 (if rtrn
	     (message "Whitespace IS after End of Line.")
	   (message "NO Whitespace after End of Line."))))
       rtrn)) 

;;; ==============================
;;; :CREATED <Timestamp: Thursday May 07, 2009 @ 05:54.27 PM - by MON KEY>
(defun mon-spacep-is-after-eol-then-graphic (&optional intrp)
  "Return t if character after eol _is_ a space and next char is not.\n
:SEE-ALSO `mon-spacep-is-bol', `mon-spacep-not-bol',
`mon-spacep', `mon-line-bol-is-eol', `mon-line-next-bol-is-eol',
`mon-line-previous-bol-is-eol', `mon-line-previous-bol-is-eol'
`mon-spacep-is-after-eol-then-graphic', `mon-spacep-at-eol',
`mon-cln-spc-tab-eol'.\n►►►"
(interactive "p")
  (let* ((after-eol (char-after (1+ (line-end-position))))
	 (after-eol-then (char-after (+ (line-end-position) 2)))	 
	 (space-char '(9 10 11 12 13 32))
	 (is-space (numberp (car (member after-eol space-char))))
	 (not-space (not (member after-eol-then space-char)))
	(rtrn (and is-space not-space)))
      (cond (intrp
	 (if rtrn
	     (message "Space or Tab IS after End of Line and Next Line is Graphic.")
	   (message "NO Space or Tab at End of Line or Next Line isn't Graphic."))))
       rtrn)) 

;;; ==============================
;;; :CREATED <Timestamp: Friday May 08, 2009 @ 05:58.38 PM - by MON KEY>
(defun mon-spacep-at-eol (&optional intrp)
  "Return t if character at eol is either TAB (char 9) or SPC (char 32).
:SEE-ALSO `mon-spacep-is-bol', `mon-spacep-not-bol',
`mon-spacep', `mon-line-bol-is-eol', `mon-line-next-bol-is-eol',
`mon-line-previous-bol-is-eol', `mon-line-previous-bol-is-eol'
`mon-spacep-at-eol', `mon-spacep-is-after-eol',
`mon-spacep-is-after-eol-then-graphic'.\n►►►"
  (interactive "p")
  (let ((rtrn 
	 (or (= (char-before (point-at-eol)) 9)(= (char-before (point-at-eol)) 32))))
    (cond (intrp
	   (if rtrn
	       (message "Space or Tab IS at End of Line.")
	     (message "NO Space or Tab at End of Line."))))
    rtrn)) 

;;; ==============================
;;; :COURTESY Andy Stewart <lazycat.manatee@gmail.com> :WAS `colp'
;;; (URL `http://www.emacswiki.org/emacs/lazycat-toolkit.el')
(defun mon-spacep-first ()
  "Return t if point is first non-whitespace character of line.\n►►►"
  (let (current-point)
    (setq current-point (point))
    (save-excursion
      (back-to-indentation)
      (equal current-point (point)))))

;;; ==============================
;;; :CREATED <Timestamp: Thursday May 07, 2009 @ 03:17.51 PM - by MON KEY>
(defun mon-line-bol-is-eol (&optional intrp)
  "Return t if postion at beginning of line is eq end of line.\n
:SEE-ALSO `mon-spacep-is-bol', `mon-spacep-not-bol',
`mon-spacep', `mon-line-bol-is-eol', `mon-line-next-bol-is-eol',
`mon-line-previous-bol-is-eol', `mon-spacep-is-after-eol',
`mon-spacep-is-after-eol-then-graphic', `mon-spacep-at-eol',
`mon-cln-spc-tab-eol'.\n►►►"
(interactive "p")
  (let ((bol-eol(= (line-end-position) (line-beginning-position))))
     (cond (intrp
	 (if bol-eol
	     (message "Beginning of Line _IS_  End of Line.")
	   (message "Beginning of Line _NOT_ End of Line."))))
    bol-eol))
;;
;;; :TEST-ME (save-excursion (previous-line) (beginning-of-line) (mon-line-bol-is-eol))

;;; ==============================
;;; :CREATED <Timestamp: Thursday May 07, 2009 @ 03:38.46 PM - by MON KEY>
(defun mon-line-previous-bol-is-eol (&optional intrp move-times)
  "Return t if position at beginning of previous line is eq end of line.
called non-interactively MOVE-TIMES arg examines Nth previos line.\n
:SEE-ALSO `mon-spacep-is-bol', `mon-spacep-not-bol',
`mon-spacep', `mon-line-bol-is-eol', `mon-line-next-bol-is-eol',
`mon-spacep-is-after-eol', `mon-spacep-is-after-eol-then-graphic',
`mon-spacep-at-eol',`mon-cln-spc-tab-eol'.\n►►►"
  (interactive "p")
  (let ((p-bol-eol (save-excursion 
		     (previous-line move-times) 
		     (beginning-of-line) 
		     (mon-line-bol-is-eol))))
      (cond (intrp
	 (if p-bol-eol
	     (message "Previous line _IS_ Beginning of Line and End of Line.")
	   (message "Previous line _NOT_ Beginning of Line and End of Line."))))
    p-bol-eol))
;;
;;; :TEST-ME  (mon-line-previous-bol-is-eol)
;;; :TEST-ME  (mon-line-previous-bol-is-eol 4)

;;; ==============================
;;; :CREATED <Timestamp: Thursday May 07, 2009 @ 03:38.46 PM - by MON KEY>
(defun mon-line-next-bol-is-eol (&optional intrp move-times)
  "Return t if position at beginning of previous line is eq end of line.
Called non-interactively MOVE-TIMES arg examines Nth previos line.\n
:SEE-ALSO `mon-spacep-is-bol', `mon-spacep-not-bol',
`mon-spacep', `mon-line-bol-is-eol', `mon-line-previous-bol-is-eol',
`mon-spacep-is-after-eol', `mon-spacep-is-after-eol-then-graphic',
`mon-spacep-at-eol',`mon-cln-spc-tab-eol'.\n►►►"
(interactive "p")
  (let ((n-bol-eol (save-excursion 
		     (next-line move-times) 
		     (beginning-of-line) 
		     (mon-line-bol-is-eol))))
      (cond (intrp
	 (if n-bol-eol
	     (message "Next line's Beginning of Line _IS_ End of Line.")
	   (message "Next line's Beginning of Line _NOT_ End of Line."))))
    n-bol-eol))
;;
;;; :TEST-ME (mon-line-next-bol-is-eol)

;;; ==============================
;;; :CREATED <Timestamp: Friday May 08, 2009 @ 05:58.27 PM - by MON KEY>
(defun mon-line-eol-is-eob (&optional intrp)
  "Return t if point EOL is also EOB \(point-max\).
NOTE: Does not test for narrowing!\n
:SEE-ALSO `mon-spacep-is-bol', `mon-spacep-not-bol',
`mon-spacep', `mon-line-bol-is-eol', `mon-line-next-bol-is-eol',
`mon-line-previous-bol-is-eol', `mon-line-eol-is-eob'
`mon-spacep-at-eol', `mon-spacep-is-after-eol',
`mon-spacep-is-after-eol-then-graphic'.\n►►►"
  (interactive "p")
(let ((rtrn (= (point-at-eol) (point-max))))
   (cond (intrp
	  (if rtrn
	      (message "End of Line is End of Buffer.")  ;%S"); rtrn)
	    (message "End of Line isn't End of Buffer.")))) ;%S); rtrn)
       rtrn))

;;; ==============================
;;; :SOURCE (URL `http://www.emacswiki.org/emacs/BackToIndentationOrBeginning')
;;; "To get the same same type of functionality at the end of the line, try this
;;; function. I bind it to my <end> key just like the <home> key above. It jumps
;;; between the actual end-of-line and the end of the code line which is different
;;; if the line has comments on the end."
;;; :CREATED <Timestamp: Tuesday June 02, 2009 @ 05:36.44 PM - by MON KEY>
;;; ==============================
(defun mon-line-end-or-code-end () 
  "Move point to EOL. If point is already there, to EOL sans comments.
That is, the end of the code, ignoring any trailing comment
or whitespace.  
:NOTE this does not handle 2 character  comment starters like // or /*.
Instances of such chars are be skipped.\n►►►"
  (interactive)
  (if (not (eolp))
      (end-of-line)
    (skip-chars-backward " \t")
    (let ((pt (point))
	  (lbp (line-beginning-position))
	  (lim))
      (when (re-search-backward "\\s<" lbp t)
	(setq lim (point))
	(if (re-search-forward "\\s>" (1- pt) t)
	    (goto-char pt)
	  (goto-char lim)               ; test here ->
          (while (looking-back "\\s<" (1- (point)))
            (backward-char))
          (skip-chars-backward " \t"))))))

;;; ==============================
;;; TODO: Wrap in a function and install under bol/eol funcs in mon-utils.el
;;; :COURTESY thing-at-point.el
;;; :CREATED <Timestamp: #{2009-09-14T15:15:57-04:00Z}#{09381} - by MON KEY>
;;; (funcall (lambda () (if (bolp) (forward-line -1) (beginning-of-line))))

;;; ============================== *
;;; :COURTESY Nelson H. F. Beebe :HIS clsc.el :VERSION 1.53 of May 27, 2001
;;; :WAS `get-next-line' -> `mon-line-get-next'
(defun mon-line-get-next ()
  "Return the next line in the buffer, leaving point following it.
Nil is returned at `end-of-buffer'.\n
:SEE-ALSO `mon-string-ify-current-line'.\n►►►"
  (let (start)
    (beginning-of-line)
    (setq start (point))
    (forward-line 1)
    (if (equal start (point))
	nil
      (buffer-substring-no-properties start (point)))))

;;; ==============================
;;; :CREATED <Timestamp: #{2009-09-08T15:18:51-04:00Z}#{09372} - by MON>
(defun mon-line-find-duplicates (&optional insertp interp)
  "Locate adjacent duplicate lines in buffer.
Functions which find duplicate lines don't always sort lines.
Where  lines of a file are presorted, use to locate duplicates before removing.
i.e. situations of type: `uniquify-maybe'. Extend `find-duplicate-lines' by
comparing its result list with one or more of the list comparison procedures
`set-difference', `union', `intersection', etc.\n
:SEE-ALSO `mon-line-get-next', `mon-cln-blank-lines', `mon-cln-uniq-lines',
`uniq', `uniq-region'.\n►►►"
  (let ((max-pon (line-number-at-pos (point-max)))
	(gather-dups))
    (while (< (line-number-at-pos) max-pon) (= (forward-line) 0)
	   (let ((this-line (buffer-substring-no-properties (line-beginning-position 1) (line-end-position 1)))
		 (next-line (buffer-substring-no-properties (line-beginning-position 2) (line-end-position 2))))
	     (when  (equal this-line next-line)  (setq gather-dups (cons this-line gather-dups)))))
    (if (or insertp interp)
	(save-excursion (newline) (princ gather-dups (current-buffer)))
      gather-dups)))

;;; ==============================
(defun mon-line-count-region (start end)
  "Return a mini-buffer message with regions' number of lines and characters.\n
:SEE-ALSO `mon-word-count-chars-region', `mon-word-count-region',
`mon-word-count-analysis', `mon-word-count-occurrences'.\n►►►"
  (interactive "r")
  (count-lines-region start end))

;;; ==============================
;;; :CREATED <Timestamp: Thursday April 30, 2009 @ 04:42.13 PM - by MON KEY>
(defun mon-line-count-matchp (test-from line-count &optional bol-char-test)
  "Return t when number of lines in region is eq LINE-COUNT.
Arg TEST-FROM is a buffer pos to start counting from.\n
:SEE-ALSO `mon-word-count-chars-region', `mon-word-count-region',
`mon-word-count-analysis', `mon-word-count-occurrences'.\n►►►"
  (save-excursion
    (let ((rg-start (line-number-at-pos test-from))
	  (rg-end)
	  (rg-diff)
	  (bct (if (and bol-char-test)
		   bol-char-test
		 35)))
      (progn
	(goto-char test-from)
	(line-move line-count)
	(cond ((eq (char-after (point)) bct)
	       (move-to-column 7))
	      ((eolp) 
	  (setq rg-end (line-number-at-pos (point))))))
      (setq rg-diff (- rg-end rg-start))
      (message "line-count %s" rg-diff)
      (eq rg-diff line-count))))

;;; ==============================
(defun mon-line-length-max (&optional intrp)
  "Return the maximum line length of the current buffer.
Called interactively returns a message in mini-buffer:
\"The longest line in buffer `mon-utils.el' ends at column 115.\"\n►►►"
(interactive "p")
  (let ((max-len 0))
    (save-excursion
      (goto-char (point-min))
      (while (eq (forward-line) 0)
        (end-of-line)
        (when (> (current-column) max-len)
          (setq max-len (current-column)))))
    (if intrp  
        (message "The longest line in buffer `%s' ends at column %d." 
                 (current-buffer) max-len)
      max-len)))
;;
;;; :TEST-ME (mon-line-length-max)

;;; ==============================
;;; Word, Line, String Related utils
;;; ==============================

;;; ==============================
;;; :COURTESY Pascal J. Bourguignon :HIS pjb-strings.el :WAS `is-digit'
(defun mon-is-digit (x)
"Reutrn t when X is a digit character.\n
:SEE-ALSO `mon-is-letter', `mon-is-alphanum' `mon-string-index',
`mon-string-position'.\n►►►"
  (cond ((stringp x) (mon-is-digit (string-to-char x)))
        ((integerp x) (and (<= ?0 x) (<= x ?9)))
        (t nil)))
;;
;;; :TEST-ME (mon-is-digit (char-after (point)))8
;;; :TEST-ME (mon-is-digit (char-after (point)))x

;;; ==============================
;;; :COURTESY Pascal J. Bourguignon :HIS pjb-strings.el :WAS `is-letter'
(defun mon-is-letter (x)
"Return t when X is an alpha character.\n
:EXAMPLE\n\(mon-is-alphanum \(char-after \(point\)\)\)\\?56
\(mon-is-alphanum \(char-after \(point\)\)\)8\n
:SEE-ALSO `mon-is-digit', `mon-is-alphanum', `mon-string-index',
`mon-string-position'.\n►►►"
  (cond ((stringp x) (mon-is-letter (string-to-char x)))
        ((integerp x) (not (equal (downcase x) (upcase x))))
        (t nil)))
;;
;;; :TEST-ME (mon-is-letter (char-after (point)))x
;;; :TEST-ME (mon-is-letter (char-after (point)))8
;;; :TEST-ME (mon-is-letter ?x)

;;; ==============================
(defun mon-is-alphanum (x)
  "Return t when X is either an alpha character or integer.\n
:EXAMPLE\n\(mon-is-alphanum \(char-after \(point\)\)\)\C-h 
\(mon-is-alphanum \(char-after \(point\)\)\)8\n
:SEE-ALSO `mon-is-digit', `mon-is-digit-2', `mon-string-index', 
`mon-string-position'.\n►►►"
  (or (mon-is-letter x)
      (mon-is-digit x)))
;;
;;; :TEST-ME (mon-is-alphanum "8")
;;; :TEST-ME (mon-is-alphanum "A")
;;; :TEST-ME (mon-is-alphanum "a")
;;; :TEST-ME (mon-is-alphanum "?")
;;; :TEST-ME (mon-is-alphanum (char-to-string 88)) ;X
;;; :TEST-ME (mon-is-alphanum (char-to-string 10)) ;C-j LF newline
;;; :TEST-ME (mon-is-alphanum (char-to-string 32)) ;SPC
;;; :TEST-ME (mon-is-alphanum ?\C-m)
;;; :TEST-ME (mon-is-alphanum ?\13)
;;; :TEST-ME (mon-is-alphanum 13)
;;; :TEST-ME (mon-is-alphanum "13")
;;; :TEST-ME (mon-is-alphanum (let ((what ?\b)) (format "%s" what)))
;;; :TEST-ME (progn (insert ?\8) (mon-is-alphanum (char-before (point))))
;;; :TEST-ME (progn (insert 8) (mon-is-alphanum (char-before (point))))

;;; ==============================
;;; :COURTESY Nelson H. F. Beebe :HIS bibtools.el :WAS `bibtex-isdigit'
;;; :CREATED <Timestamp: 2009-08-03-W32-1T10:27:00-0400Z - by MON KEY>
(defun mon-is-digit-simp (c)
 "Return t if C is a digit character, and otherwise, nil.\n
Unlike `mon-is-digit' fails when other than \\? prefixed digit.
Wants char literals.\n:EXAMPLE\n\(mon-is-digit-simp ?0\)
\(mon-is-digit-simp \"0\"\)\n\(mon-is-digit \"0\"\)\n
:SEE-ALSO `mon-is-letter-simp', `mon-is-alphanum-simp',
`mon-string-index', `mon-string-position'.\n►►►"
 (and (>= c ?0) (<= c ?9)))
;;
;;; :TEST-ME (mon-is-digit-simp ?0)

;;; ==============================
;;; :COURTESY Nelson H. F. Beebe :HIS bibtools.el :WAS `bibtex-isalpha'
;;; :CREATED <Timestamp: 2009-08-03-W32-1T10:26:57-0400Z - by MON KEY>
(defun mon-is-letter-simp (c) ;
 "Return t if C is an alphabetic character, and otherwise, nil.
Unlike `mon-is-letter' fails when other than \\? prefixed chars.
Wants char literals.\n
:EXAMPLE\n\(mon-is-letter-simp ?x\)\n
\(mon-is-letter-simp \"x\"\)
\(mon-is-letter \"x\"\)\n
:SEE-ALSO `mon-is-digit-simp',`mon-is-alphanum-simp'.
`mon-string-index', `mon-string-position'.\n►►►"
 (or  (and (>= c ?a) (<= c ?z))
      (and (>= c ?A) (<= c ?Z))))
;;
;;; :TEST-ME (mon-is-letter-simp ?x)

;;; ==============================
;;; :CREATED <Timestamp: 2009-08-03-W32-1T15:18:01-0400Z - by MON KEY>
(defun mon-is-alphanum-simp (x)
  "Return t when X is either an alpha character or integer.\n
Unlike `mon-is-alphanum' fails when other than \\? prefixed chars or digits.
Wants char literals.\n
\(mon-is-alphanum-simp ?8\)             
\(mon-is-alphanum-simp ?A\)             
\(mon-is-alphanum-simp \"8\"\)            
\(mon-is-alphanum-simp \"A\"\)            
\(mon-is-alphanum-simp \(prin1-char 88\)\)
\(mon-is-alphanum \(char-to-string 88\)\)\n
:SEE-ALSO `mon-is-digit-simp' `mon-is-letter-simp',
`mon-string-index', `mon-string-position'.\n►►►"
(or (mon-is-letter-simp x)
    (mon-is-digit-simp x)))
;;
;;; :TEST-ME (mon-is-alphanum-simp ?8)
;;; :TEST-ME (mon-is-alphanum-simp ?A)
;;; :TEST-ME (mon-is-alphanum-simp "8") ;should fail
;;; :TEST-ME (mon-is-alphanum-simp "A");should fail
;;; :TEST-ME (mon-is-alphanum-simp (prin1-char 88)) ;should fail

;;; ==============================
;;; :COURTESY Pascal Bourguignon :HIS pjb-strings.el :WAS `string-justify-left'
(defun mon-string-justify-left (string &optional width left-margin)
  "RETURN: a left-justified string built from string. 
NOTE:   The default width is 72 characters, the default left-margin is 0.  
        The width is counted from column 0.
        The word separators are those of split-string: [ \\f\\t\\n\\r\\v]+, which
        means that the string is justified as one paragraph.\n►►►"
  (if (null width) (setq width 72))
  (if (null left-margin) (setq left-margin 0))
  (if (not (stringp string)) 
      (error "string-justify-left: The first argument must be a string."))
  (if (not (and (integerp width) (integerp left-margin)))
      (error "string-justify-left: The optional arguments must be integers."))
  (let* ((margin (make-string left-margin 32))
         (splited (split-string string))
         (col left-margin)
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
    justified))

;;; ==============================
;;; :NOTE Alias these and don't forget to use them!
;;; :CREATED <Timestamp: Wednesday July 01, 2009 @ 06:32.08 PM - by MON KEY>
(defalias 'mon-string-combine-and-quote 'combine-and-quote-strings)
;;
(defalias 'mon-string-split-and-unquote 'split-string-and-unquote)
;;
(defalias 'mon-replace-char-in-region 'subst-char-in-region)

;;; ==============================
;;; :CREATED <Timestamp: #{2009-09-12T14:07:56-04:00Z}#{09376} - by MON KEY>
(defun mon-string-read-match-string (&optional match-subexp)
  "Make `match-string' return more than just the last string matched.
Strip the # char from the side-effect value returned by match-string.
When MATCH-SUBEXP is non-nil return match-string of nth subexp.
The function match-string carries more data than just the string it returns.
These datum include:
from-idx to-idx of subexp match location;
if match string is fontified and the face used @ from-sub-idx to-sub-idx;
if match string carries text properties and if so the stickiness
of these props @ from-sub-idx to-sub-idx;
However, this data is not accessible to read because match-string returns as an
unreadable object with the '#' prefix so we strip it.\n
:EXAMPLE 
> \(search-forward-regexp \"\\\\\(\\\\\(\\=[\\\\\)\\\\\([0-9\\=]\\\\\\={8,10\\\\}\\\\\)\\\\\(]\\\\\)\\\\\)\" nil t\)
\"[500006383]\"
> (match-string 0)\n; => #\(\"[500006383]\" 0 11 \(fontified t face font-lock-string-face\)\)
> \(search-forward-regexp \"\\\\\(\\\\\(\\=[\\\\\)\\\\\([0-9\\=]\\\\\\={8,10\\\\}\\\\\)\\\\\(]\\\\\)\\\\\)\" nil t\)
\"[500006383]\"
> (mon-string-read-match-string)
; => \(\"[500006383]\" 0 11 \(fontified t face font-lock-string-face\)\)\n►►►"
  (let ((matched (if (and (= (match-beginning 0) 1)(> (point) (point-min))) 
		     nil ;last search didn't move point was a dud don't proceed
		   (car (read-from-string 
			 (substring (format "%S" (match-string (if match-subexp match-subexp 0))) 1))))))
    matched))
;;
;;; :TEST-ME (search-forward-regexp "\\(\\(\\[\\)\\([0-9]\\{8,10\\}\\)\\(]\\)\\)" nil t)
;;;        [500006383]
;;; :TEST-ME (mon-string-read-match-string)
;;; :TEST-ME (mon-string-read-match-string 4)

;;; ==============================
;;; I hope this isn't reinventing the wheel here... 
;;; If not, WTF? why isn't this in Emacs already?
;;; :CREATED <Timestamp: #{2009-08-26T17:08:02-04:00Z}#{09353} - by MON KEY>
(defun mon-string-to-symbol (str)
  "Return string STR as a symbol.\n
:EXAMPLE\n\(mon-string-to-symbol \"Bubba\")\n
\(mon-string-to-symbol \(mon-symbol->string 'bubba\)\)
:SEE-ALSO `mon-symbol-to-string', `mon-string-to-sequence',
`mon-string-from-sequence', `mon-string-alpha-list',
`mon-string-index', `mon-string-has-suffix'.\n►►►"
  (car (read-from-string str)))
;;
(defalias 'mon-string->symbol 'mon-string-to-symbol)
;;
;;; :TEST-ME (mon-string-to-symbol "bubba")
;;; :TEST-ME (mon-string-to-symbol (mon-symbol->string 'bubba))

;;; ==============================
;;; Periodically MON is completely at a loss for how to accomplish this.
;;; Lets make _damn_ sure it never happens again!!
;;; :CREATED <Timestamp: #{2009-09-29T21:00:43-04:00Z}#{09403} - by MON KEY>
(defun mon-symbol-to-string (symbol) 
  "Return SYMBOL as a string.\n
:EXAMPLE\n(mon-symbol-to-string 'bubba)\n
\(mon-symbol-to-string \(mon-string-to-symbol \"bubba\"\)\)\n
:SEE-ALSO `mon-string-to-symbol', `mon-string-to-sequence',
`mon-string-from-sequence'.\n►►►"
  ;;Which is more correct? (format "%s" symbol)) ; OR:
  (format "%S" symbol)) 
;;
(defalias 'mon-symbol->string    'mon-symbol-to-string)
(defalias 'mon-string-from-symbol 'mon-symbol-to-string)
(defalias 'mon-string<-symbol    'mon-symbol-to-string)
;;
;;; :TEST-ME (mon-symbol->string 'bubba)
;;; :TEST-ME (mon-symbol->string (mon-string-to-symbol "bubba"))

;;; ==============================
;;; :CREATED <Timestamp: Wednesday June 24, 2009 @ 11:50.11 AM - by MON KEY>
(defun mon-string-to-sequence (str)
  "Return string STR as a list of chars.\n
:EXAMPLE\n(mon-string-to-sequence \" string\"\)\n
:SEE-ALSO `mon-string-from-sequence', `mon-string-index',
`mon-string-position', `mon-string-alpha-list',
`mon-is-alphanum', `mon-is-digit', `mon-is-letter'.\n►►►"
  ;; :WAS (mapcar (lambda (l) l) str))
  (let (to-seq)
    (mapc (lambda (l) (push l to-seq)) str)
    (nreverse to-seq)))
;;
;;; :TEST-ME (mon-string-to-sequence " string")
;;; :TEST-ME (apply 'string (mon-string-to-sequence " string"))

;;; ==============================

;;; ==============================
;;; :MODIFICATIONS <Timestamp: #{2009-10-09T16:07:57-04:00Z}#{09415} - by MON>
;;; :CREATED <Timestamp: #{2009-09-30T13:31:42-04:00Z}#{09403} - by MON KEY>
(defun mon-string-from-sequence (seq &rest seqs)
  "Return SEQ - a sequence of character integers - as a string.
When SEQS is one or more quoted lists concatenate these to return string.\n
:EXAMPLE(S)\n\(mon-string-from-sequence '(115 116 114 105 110 103))\n
\(mon-string-from-sequence\n '(115 116 114 105 110 103 48)
 '\(115 116 114 105 110 103 115 49\)\n '\(115 116 114 50\)\)\n
\(mon-string-from-sequence \(number-sequence 0 127\)\)\n
:SEE-ALSO `mon-string-index',`mon-string-position', `mon-string-alpha-list',
`mon-is-alphanum',`mon-is-digit',`mon-is-letter'.\n►►►"
  (let ((g-str (lambda (x) (apply 'string x)))
        (seq-seqs))
    (while seqs (push (funcall g-str (pop seqs)) seq-seqs))
    (setq seq-seqs (nreverse seq-seqs))
    (push (funcall g-str seq) seq-seqs)
    (apply 'concat (car seq-seqs) (cdr seq-seqs))))
;;
;;; :TEST-ME (mon-string-from-sequence '(98 117 98 98 97))
;;; :TEST-ME (mon-string-from-sequence (string-to-list "bubba"))
;;; :TEST-ME (mon-string-from-sequence '(98 117 98 98 97 115 97) (string-to-list "bubba"))
;;; :TEST-ME (mon-string-from-sequence '(98 117 98 98 97) (arrayp [98 117 98 98 97 115 97])

;;; ==============================
;;; :CREATED <Timestamp: Thursday June 25, 2009 @ 11:17.43 AM - by MON KEY>
(defun mon-string-alpha-list (from-letter to-letter &optional as-symb)
  "Return alphabetized list of ASCII character strings FROM-LETTER TO-LETTER.
If either FROM-LETTER or TO-LETTER is upper-cased list returned 
will be in upper cased. When TO-LETTER comes before FROM-LETTER in a 
lexicographic sort the two args are swapped; this check is exclusive of case
check.\n\n:EXAMPLE\n
\(mon-string-alpha-list \"a\" \"f\"\)\n\(mon-string-alpha-list \"A\" \"F\"\)
\(mon-string-alpha-list \"l\" \"G\"\)\n\(mon-string-alpha-list \"g\" \"l\"\)
Use this to get a list of symbols instead:\n
\(princ \(mon-string-alpha-list \"m\" \"r\"\) \(current-buffer\)\);=>\(m n o p q r\)\n
:SEE-ALSO `mon-string-to-sequence', `mon-string-to-sequence',
`number-sequence', `mon-is-alphanum', `mon-is-digit', `mon-is-letter'.\n►►►"
  (let ((frm (string-to-char from-letter))
        (to (string-to-char to-letter))
        (swap)
        (rtn))
    (cond ((and (and (>= frm 65) (<= frm 90))
                (and (>= to 97) (<= to 127)))
           (setq to (- to 32)))
          ((and (and (>= to 65) (<= to 90))
                (and (>= frm 97) (<= frm 127)))
           (setq frm (- frm 32))))
    (when (< to frm)
      (setq swap frm)
      (setq frm to)
      (setq to swap))
    (split-string (mon-string-from-sequence (number-sequence frm to)) "" t)))
;;
;;; :TEST-ME (mon-string-alpha-list "a" "z")
;;; :TEST-ME (mon-string-alpha-list "A" "Z")
;;; :TEST-ME (mon-string-alpha-list "Z" "A")
;;; :TEST-ME (mon-string-alpha-list "z" "a")
;;; :TEST-ME (mon-string-alpha-list "Z" "a")
;;; :TEST-ME (mon-string-alpha-list "a" "Z")
;;; :TEST-ME (mon-string-alpha-list "z" "Z")
;;; :TEST-ME (mon-string-alpha-list "A" "a")

;;; ==============================
;;; :COURTESY Pascal J. Bourguignon :HIS pjb-strings.el :WAS `string-index'
(defun mon-string-index (string-to-idx needle &optional frompos)
  "Return the position in STRING of the beginning of first occurence of NEEDLE.
Return nil if needle is not found. NEEDLE is a char, number, or string.
When FROMPOS is non-nil begin search for needle from position. 
Default is to search from start of string.\n
:EXAMPLE\n\(mon-string-index \"string before ### string after\" \"###\"\)
:SEE-ALSO `mon-string-upto-index', `mon-string-after-index',
`mon-string-position', `mon-string-has-suffix', `mon-string-chop-spaces'.\n►►►"
  (string-match 
   (regexp-quote 
    (cond ((or (characterp needle) (numberp needle)) (format "%c" needle))
          ((stringp needle) needle)
          (t (error "string-index expects a needle, number or string as 2nd argument."))))
   string-to-idx frompos))
;; 
;;; :TEST-ME (mon-string-index "string before ### string after" "###")

;;; ==============================
;;; :CREATED <Timestamp: #{2009-10-01T15:16:26-04:00Z}#{09404} - by MON KEY>
(defun mon-string-upto-index (in-string upto-string)
  "Return substring of IN-STRING UPTO-STRING.
UPTO-STRING is a simple string. No regexps, chars, numbers, lists, etc.\n
:EXAMPLE\n\(mon-string-upto-index \"string before ### string after\" \"###\"\)\n
:SEE-ALSO `mon-string-index', `mon-string-after-index'
`mon-string-position', `mon-string-has-suffix', `mon-string-chop-spaces'.\n►►►"
  (substring in-string 0
             (mon-string-index in-string upto-string)))
;;
;;; :TEST-ME (mon-string-upto-index "string before ### string after" "###")

;;; ==============================
;;; :CREATED <Timestamp: #{2009-10-01T15:16:29-04:00Z}#{09404} - by MON KEY>
(defun mon-string-after-index (in-string after-string)
  "Return substring of IN-STRING UPTO-STRING.
UPTO-STRING is a simple string. No regexps, chars, numbers, lists, etc.\n
:EXAMPLE\n\(mon-string-after-index \"string before ### string after\" \"###\"\)\n
:SEE-ALSO `mon-string-index', `mon-string-upto-index',
 `mon-string-position', `mon-string-has-suffix', `mon-string-chop-spaces'.\n►►►"
(substring in-string 
           (+ (mon-string-index in-string after-string) (length after-string))))
;;
;;; :TEST-ME (mon-string-after-index "string before ### string after" "###")

;;; ==============================
;;; :COURTESY Pascal J. Bourguignon :HIS pjb-strings.el :WAS `string-position'
(defun mon-string-position (string substr &optional frompos)
  "Return the position in STRING of the first occurence of the SUBSTR
searching FROMPOS, or from the start if FROMPOS is absent or nil. 
If the SUBSTR is not found, then return nil.\n
:EXAMPLE\n\(mon-string-position \"dogmeat\" \"meat\"\)\n
:SEE-ALSO `mon-string-index', `mon-string-upto-index', `mon-string-after-index'
`mon-string-to-sequence', `mon-string-from-sequence'.\n►►►"
  (string-match (regexp-quote substr) string frompos))
;;
;;; :TEST-ME (mon-string-position "dogmeat" "meat")

;;; ==============================
;;; :COURTESY Pascal J. Bourguignon :HIS pjb-strings.el :WAS `string-has-suffix'
(defun mon-string-has-suffix (string suffix)
"Return t when STRING has SUFFIX as a component.\n\n:EXAMPLE\n
\(mon-string-has-suffix \"dogmeat\" \"meat\"\).\n
:SEE-ALSO `mon-string-position', `mon-string-index'
`mon-string-upto-index', `mon-string-after-index'.\n►►►"
  (cond ((or
          (not (stringp string))
          (not (stringp suffix)))
         (error "The parameters STRING and SUFFIX must be strings."))
        ((< (length string) (length suffix)) nil)
        (t (string-equal (substring string (- (length string) (length suffix))) suffix))))
;;
;;; :TEST-ME (mon-string-has-suffix "dogmeat" "meat")

;;; ==============================
;;; :COURTESY Pascal J. Bourguignon :HIS pjb-advices.el :WAS `pjb-chop-spaces'
;;; :CREATED <Timestamp: #{2009-09-28T16:39:34-04:00Z}#{09401} - by MON>
(defun mon-string-chop-spaces (string)
  "Return a substring of `string' with spaces removed left and right.\n
:SEE-ALSO `mon-string-split-on-regexp', `mon-string-sub-old->new', 
`mon-string-chop-spaces', `mon-string-position', `mon-string-index',
`mon-string-upto-index', `mon-string-after-index'\n►►►"
  (let ((i 0)
        (l (1- (length string)))
        (space 32))
    (while (and (< 0 l) (eq (aref string l) space))
      (setq l (1- l)))
    (setq l (1+ l))
    (while (and (< i l) (eq (aref string i) space))
      (setq i (1+ i)))
    (substring string i l)))
;;
;;; :TEST-ME (mon-string-chop-spaces " some string no spaces ")

;;; ==============================
;;; RENAMED: `mon-stringify-list' -> `mon-string-ify-list'
(defun mon-string-ify-list (string-given)
  "Return a list of strings obtained by breaking the string the user entered at the
space boundaries.\n\n:EXAMPLE
\(mon-string-ify-list \"Make this sentence a list of strings\").\n
:SEE-ALSO `mon-stringify-list' ,`mon-insert-string-ify', 
`mon-string-ify-current-line', `mon-line-get-next', 
`mon-get-word-list-buffer'.\n►►►"
  (let ((string string-given) list)
    (store-match-data nil)
    (while (string-match " *\\([^ ]+\\) *" string (match-end 0))
      (setq list
	    (cons (substring string (match-beginning 1) (match-end 1)) list)))
     (nreverse list)))
;;
;;; :TEST-ME (mon-stringify-list \"Make this sentence a list of strings\")

;;; ==============================
;;; :COURTESY Nelson H. F. Beebe :HIS bibtools.el :WAS `bibtex-split'
(defun mon-string-split-on-regexp (str regexp)
 "Return a list of strings from splitting STR at the regular expression
REGEXP. This function is patterned after the awk split() function.\n
:SEE-ALSO `mon-string-chop-spaces', `mon-string-sub-old->new',
`mon-string-position', `mon-string-index'.\n►►►"
 (if (or (null str) (null regexp)) ;then return nil if either s or regexp is nil
     nil
   (let ((p nil) 
         (k 0)) ;else split the string and return a list of strings
     (while (and (< k (length str)) (string-match regexp str k))
       (setq p (nconc p (list (substring str k (match-beginning 0)))))
       (setq k (match-end 0)))
     (setq p (nconc p (list (substring str k))))
     p)))
;;
;;; :TEST-ME (mon-string-split-on-regexp "split-on-split" "-split")

;;; ==============================
;;; :CREATED <Timestamp: 2009-08-03-W32-1T10:26:52-0400Z - by MON KEY>
;;; :COURTESY Nelson H. F. Beebe :HIS bibtools.el :WAS `melvyl-sub'
(defun mon-string-sub-old->new (old new str)
 "Substitute the first occurrence of OLD by NEW in a copy of the
string STR and return it.
:SEE-ALSO `mon-string-split-on-regexp', `mon-string-chop-spaces',
`mon-string-position', `mon-string-index'.\n►►►"
 (let ((k 0))
   ;; (debug)
   (while (and (< k (1+ (- (length str) (length old))))
               (not (string-equal old (substring str k (+ k (length old))))))
     (setq k (1+ k)))
   (if (and (< k (1+ (- (length str) (length old))))
            (string-equal old (substring str k (+ k (length old)))))
       (concat (substring str 0 k) new
               (substring str (+ k (length old)) (length str)))
     str)))

;;; :TEST-ME (mon-string-sub-old->new"old" "new" "old old new")

;;; ==============================
;;; :COURTESY Jared D. :WAS `string-repeat'
;;; (URL `http://curiousprogrammer.wordpress.com/2009/07/26/emacs-utility-functions/')
;;; :MODIFICATIONS <Timestamp: #{2009-08-19T20:13:32-04:00Z}#{09344} - by MON KEY>
(defun mon-string-repeat (str n &optional insertp w/spc intrp)
  "Return the string STR N times.
When optional INSERTP is non-nil or called-interactively insert STR at point.
Does not move point. 
When W/SPC is non-nil return string with whitespace interspersed.\n
:EXAMPLE\n\(mon-string-repeat \"bubba\" 3 nil t\)\n
:SEE-ALSO `mon-insert-string-ify', `mon-insert-string-incr', 
`mon-insert-string-n-fancy-times', `mon-insert-string-n-times'.\n►►►"
  (interactive 
   (list 
    (replace-regexp-in-string "[[:space:]]+$" "" (read-string "String to repeat :"))
    (read-number "Times to repeat :")
    nil
    (yes-or-no-p "With whitespace")))
  (let ((retval ""))
    (dotimes (i n)
      (if w/spc 
          (setq retval (concat retval str " "))
        (setq retval (concat retval str))))
    (if (or insertp intrp)
        (save-excursion (insert retval)))
    retval))
;;
;;; :TEST-ME (mon-string-repeat "bubba" 3)
;;; :TEST-ME (mon-string-repeat "bubba" 3 t)
;;; :TEST-ME (mon-string-repeat "bubba" 3 t t)
;;; :TEST-ME (mon-string-repeat "bubba" 3 nil t)
;;; :TEST-ME (call-interactively 'mon-string-repeat) 

;;; ==============================
;;; :COURTESY Drew Adams :HIS strings.el
;;; RENAMED: `mon-split-string-line' -> `mon-string-split-line'
;;; :MODIFICATIONS <Timestamp: #{2009-09-23T18:49:22-04:00Z}#{09393} - by MON KEY>
(defun mon-string-split-line (&optional buffer insrtp intrp)
  "Return current line of text in BUFFER as a string.
When INSRTP is non-nil or called interactively insert return string at point. 
Does not move-point.\n
:SEE-ALSO `mon-line-strings-to-list', `mon-stringify-list',
`mon-insert-string-ify', `mon-line-drop-in-words', `mon-string-ify-current-line',
`mon-get-word-list-buffer'.\n►►►"
(interactive "i\ni\np")
(let ((splt-str-s)
      (splt-str-e)
      (splt-str))  
  (setq buffer (or buffer (current-buffer)))
  (save-excursion
    (set-buffer buffer)
    (setq splt-str
          (buffer-substring-no-properties (progn (end-of-line 1) (setq splt-str-e (point)))
                                          (progn (beginning-of-line 1) (setq splt-str-s (point))))))
  (if (or insrtp intrp)
      (save-excursion (prin1 splt-str (current-buffer)))
    splt-str)))

;;; ==============================
;;; :CREATED <Timestamp: Sunday May 31, 2009 @ 03:08.46 PM - by MON KEY>
(defun mon-string-ify-current-line (&optional intrp split-on delim)
  "Return line at point as a list of strings.
When non-nil split-on is a string which should be split on.
When non-nil delim is a delimter to be concatenated to _front_ of each string. 
Called interacively kills current line replacing with string per-word
unless in an unreadable buffer where just retruns.
Neither SPLIT-ON nor DELIM have an effect when Invoked interactively.\n:EXAMPLE
\(mon-string-ify-current-line\) split me to a list of strings
\(mon-string-ify-current-line nil \"s\" \"S\"\) split me to a list of strings
\(mon-string-ify-current-line nil nil \"|\"\) split me to a list of strings\n\n
:SEE-ALSO `mon-line-strings-to-list', `mon-string-ify-list' ,`mon-insert-string-ify',
`mon-string-split-line', `mon-line-drop-in-words', `mon-get-word-list-buffer'.\n►►►"
  (interactive "p")
  (let* ((sp (if split-on " "))
	 (dlm (cond (delim delim)
		    ((not delim)
		     (if intrp  "\""  ""))))
	 (ss (split-string (mon-string-split-line) split-on t)))
    (cond ((and intrp (not buffer-read-only))
	   (save-excursion
	     (progn 
	       (kill-line)
	       (mapcar '(lambda (x) (princ (format "%s%s%s " dlm x dlm) (current-buffer))) ss)
	       (delete-char -1)))ss)
	  ((and intrp buffer-read-only)
	   (progn
	     (kill-new (format "%S" ss))
	     (message "Buffer is read only is in kill ring\n %S"  ss)))
	  ((and (not intrp) dlm)
	   (let (ss2)
	   (setq ss2 nil)
	     (mapcar '(lambda (x) (setq ss2 (cons (format "%s%s" dlm x) ss2)))ss)
	     ss2))
	  (t ss))))
;;
;;; :TEST-ME (mon-string-ify-current-line\) ;split me to a list of strings
;;; :TEST-ME (mon-string-ify-current-line nil \"s\" \"S\"\) split me to a list of strings
;;; :TEST-ME (mon-string-ify-current-line nil nil \"|\"\) split me to a list of strings

;;; ==============================
;;; :CREATED <Timestamp: #{2009-10-06T14:45:00-04:00Z}#{09412} - by MON KEY>
(defun mon-line-strings-bq-qt-sym-bol (start end &optional insertp intrp)
"Return symbols at BOL in region wrapped in backquote and quote.
When INSERTP is non-nil or called-interactively replace active region and
move point to region-beginning.
When following characters are at BOL no replacement is peformed on symbol:
^ ; , . ( ) < > ` ' # ► \| Likewise, do not replace if ' follows symbol.\n
:EXAMPLE\n(mon-line-strings-bq-qt-sym-bol
 \(1+ \(search-forward-regexp \"►\"\)\) \(- \(search-forward-regexp \"►\"\) 2\)\)
►\ncall-next-method &rest replacement-args
call-next-method &rest replacement-args
`call-next-method &rest replacement-args
call-next-method' &rest replacement-args\n►\n
:SEE-ALSO `mon-line-strings-to-list', `mon-string-ify-list',
`mon-string-ify-current-line', `mon-string-split-line',
`mon-line-drop-in-words'.\n►►►"
  (interactive "r\ni\np")
  (let (rtn-v)
    (setq rtn-v (buffer-substring-no-properties start end))
    (setq rtn-v
          (with-temp-buffer 
            (insert rtn-v)
            (goto-char (buffer-end 0))
            (while (search-forward-regexp                                 ;,-+ Is this correct? 
                    "^\\([^;,.()<>`'#►\|\\[:blank:]][\\[:graph:]]+[^'\\[:blank:]]+\\)\\( \\)\\(.*\\)$" nil t)
              (replace-match "`\\1'\\2\\3"))
            (buffer-substring-no-properties (buffer-end 0) (buffer-end 1))))
    (if (or insertp intrp)
        (save-excursion (delete-region start end) (insert rtn-v))
        rtn-v)))
;;
;;; :TEST-ME
;;; (mon-line-strings-bq-qt-sym-bol 
;;;   (1+ (search-forward-regexp "►")) (- (search-forward-regexp "►") 2))
;;,---- :UNCOMMENT-TO-TEST
;;|►
;;|call-next-method  &rest replacement-args
;;|call-next-method  &rest replacement-args
;;|`call-next-method  &rest replacement-args
;;|call-next-method' &rest replacement-args
;;|►
;;`----

;;; ==============================
;;; :CREATED <Timestamp: #{2009-09-13T09:30:42-04:00Z}#{09377} - by MON>
(defun mon-line-strings-to-list (start end &optional w-cdr w-wrap insertp intrp)
  "Return region's lines as list, each list elt contains string content of line.
Region between START END should be passed as a line per string/symbol.
Strips trailing whitespace. Does not preseve tabs converts them to spaces.
When W-CDR is non-nil or called-interactively with prefix-arg return each
element of list with an empty string as cdr.\n\n:EXAMPLE\n
Mon Key\nMON\nMon\nMON KEY\n\n;; When W-CDR nil:
=>\((\"Mon Key\"\)\n   \(\"MON\"\)\n   \(\"Mon\"\)\n   \(\"MON KEY\"\)\)\n
;; When W-CDR non-nil:\n=>\(\(\"Mon Key\" \"\"\)\n   \(\"MON\" \"\"\)
   (\"Mon\" \"\"\)\n   \(\"MON KEY\" \"\"\)\)\n
\(mon-line-strings-to-list-*test* t nil\)\n
\(mon-line-strings-to-list-*test*\)\n
:SEE-ALSO `mon-line-string-rotate-name', `mon-line-strings-to-list-*test*',
`mon-line-string-rotate-namestrings', `mon-line-string-unrotate-namestrings'
`mon-line-string-rotate-namestrings-combine', `mon-make-lastname-firstname', 
`naf-make-name-for-lisp', `mon-make-names-list',`mon-string-ify-current-line', 
`mon-string-ify-list', `mon-string-split-line', `mon-line-drop-in-words'.\n►►►"
  (interactive "r\ni\nP\ni\np") ;; (interactive "r\nP\ni\ni\np") make w-cdr the pref arg
  (let ((start-reg start)
        (end-reg end)
        (rgn-l))
    (setq rgn-l (buffer-substring-no-properties start end))
    (save-excursion
      (setq rgn-l (with-temp-buffer
                    (insert rgn-l) 
                    (untabify (point-min) (point-max))
                    (mon-cln-trail-whitespace) ;; (point-min) (point-max))
                    (goto-char (point-min))
                    (while (search-forward-regexp "^\\(.*\\)$" nil t)
                      (if w-cdr 
                          (replace-match "(\"\\1\" \"\")")
                        (replace-match "(\"\\1\")")))
		    (goto-char (point-max)) 
		    (if w-wrap (insert "))") (insert ")"))
		    (goto-char (point-min))
		    (if w-wrap
			(save-excursion 
			  (insert "(;; defvar defconst let let* setq\n'("))
		      (indent-pp-sexp 1)
		      (insert "("))
			(buffer-substring-no-properties (point-min) (point-max)))))
      (if (or insertp intrp)
	  (save-excursion (delete-region start-reg end-reg)(insert rgn-l))
	rgn-l)))

;;; ==============================
;;; :CREATED <Timestamp: #{2009-09-13T09:28:46-04:00Z}#{09377} - by MON>
(defun mon-line-strings-to-list-*test* (&optional with-cdr with-wrap insertp)
  "Test function for `mon-line-strings-to-list'.\n►►►"
  (let ((st01 (make-marker))
        (en01 (make-marker))
        (t-str (concat "hendr erit\norci\nultrices\naugue\nAliquam\n"
                       "odio\nNam\ne ros\nurna\naliquam\nvitae\nlacinia")))
    (cond ((not insertp)
           (with-temp-buffer
             (insert t-str)
             (mon-line-strings-to-list (point-min) (point-max) with-cdr with-wrap)))
          (insertp 
           (set-marker st01 (point))
           (insert t-str)
           (set-marker en01 (point))
           (goto-char st01)
           (mon-line-strings-to-list st01 en01 with-cdr with-wrap t)))))
;;
;;; :TEST-ME (mon-line-strings-to-list-*test*)
;;; :TEST-ME (mon-line-strings-to-list-*test* t nil)
;;; :TEST-ME (mon-line-strings-to-list-*test* t t)
;;; :TEST-ME (mon-line-strings-to-list-*test* t nil t)
;;; :TEST-ME (mon-line-strings-to-list-*test* t t t)
;;
;;;(progn (newline) (mon-line-strings-to-list-*test* t t))
;;;(progn (newline) (mon-line-strings-to-list-*test* nil t))

;;; ==============================
;;; :CREATED <Timestamp: #{2009-09-19T13:53:29-04:00Z}#{09386} - by MON>
(defun mon-line-string-rotate-name (name-str-or-elt &optional as-list)
  "Rotate the namestring NAME-STR-OR-ELT. 
Return the last whitespace delimited name in string at head top of string.
Remaining names in string returned inside a parenthetical group.
NAME-STR-OR-ELT is a string containing one nameform or one elt listsame 
holing a string containing one nameform.\n
:EXAMPLE\n\(mon-line-string-rotate-name \"István Tisza\")
\(mon-line-string-rotate-name '\(\"Stanisław Marcin Ulam\")\)
\(mon-line-string-rotate-name '\(\"Dmitri Pavlovich Romanov\")\)\n
:SEE-ALSO `mon-line-strings-to-list', `mon-line-string-rotate-namestrings'
`mon-line-string-unrotate-namestrings', `mon-line-string-rotate-namestrings-combine'
`mon-make-lastname-firstname', `naf-make-name-for-lisp', `mon-make-names-list'.\n►►►"
  (let* ((nm-or-elt (if (atom name-str-or-elt)
			name-str-or-elt
		      (let ((get-head name-str-or-elt))
			(while (consp get-head)
			  (setq get-head (car get-head)))
			get-head)))
	 (the-split (split-string nm-or-elt))
	 (split-len (length the-split))
	 (last-in (cond ((= split-len 1) (format "%s" (car the-split)))
			((> split-len 1) 
			 (let ((rot-split (append (subseq the-split -1)
						  (subseq the-split 0 (1- split-len)))))
			   (format "%s %s" (car rot-split) (cdr rot-split))))
			((= split-len 0) nil))))
    (if as-list (list last-in) last-in)))
;;
;;; :TEST-ME (mon-line-string-rotate-name "Elvis")
;;; :TEST-ME (mon-line-string-rotate-name "István Tisza")
;;; :TEST-ME (mon-line-string-rotate-name "Thomas Pollock Anshutz")
;;; :TEST-ME (mon-line-string-rotate-name "Thomas Pollock Anshutz" t)
;;; :TEST-ME (mon-line-string-rotate-name '("Thomas Pollock Anshutz") t)
;;; :TEST-ME (mapc (lambda (x) (princ (concat "\n" (mon-line-string-rotate-name x)) (current-buffer)))
;;;        '(("George Charles Aid")("Thomas Pollock Anshutz")("Cecilia Beaux")("Frank Weston Benson")
;;;          ("Thomas Hart Benton")("Saul Bernstein")("George Biddle")("Gutzon Borglum")))

;;; ==============================
;;; :CREATED <Timestamp: #{2009-09-22T16:39:59-04:00Z}#{09392} - by MON KEY>
(defun mon-line-string-rotate-namestrings (start end &optional as-strings insrtp intrp)
  "Rotate namestrings in region. Namestring are formatted one name per line
Firstname Middlenames Lastname. Return Lastname (Firstname Middlename).
When AS-STRINGS is non-nil retrun namestrings as strings as with prin1.
When INSRTP is non-nil or called-interactively insert rotated names at point.
Does not move point.\n
:SEE-ALSO `mon-line-string-unrotate-namestrings', `mon-line-string-rotate-name', 
`mon-line-string-rotate-namestrings-combine', `mon-line-strings-to-list',
`mon-make-lastname-firstname', `mon-make-names-list', `naf-make-name-for-lisp'.\n►►►"
  (interactive "r\nP\ni\np")
  (let ((r-nms-strt start)
	(r-nms-end  end)
	(get-namestrings))
    (setq get-namestrings 
	  (mapconcat (lambda (x) (mon-line-string-rotate-name (car x))) 
     		     (read (mon-line-strings-to-list r-nms-strt r-nms-end)) "\n"))
    (if (or insrtp intrp)
        (progn
          (save-excursion
	  (delete-region r-nms-strt r-nms-end)
	  (if as-strings
              (mapc (lambda (x) (newline) (prin1 x (current-buffer)))
                    (split-string get-namestrings "\n"))
              (insert get-namestrings)))
          (when as-strings (delete-char 1)))
      (if as-strings 
          (split-string get-namestrings "\n") 
        get-namestrings))))
;;
;;; :TEST-ME 
;;; (mon-line-string-rotate-namestrings 
;;;    (1+ (search-forward-regexp "►")) (- (search-forward-regexp "►") 2))
;;; (mon-line-string-rotate-namestrings 
;;;    (1+ (search-forward-regexp "►")) (- (search-forward-regexp "►") 2) t)
;;
;;,---- :UNCOMMENT-TO-TEST
;;|►
;;|George Charles Aid
;;|Thomas Pollock Anshutz
;;|Cecilia Beaux
;;|Frank Weston Benson
;;|Thomas Hart Benton
;;|Saul Bernstein
;;|George Biddle
;;|Gutzon Borglum
;;|►
;;`----

;; ==============================
;;; :CREATED <Timestamp: #{2009-09-23T20:12:26-04:00Z}#{09394} - by MON KEY>
(defun mon-line-string-unrotate-namestrings (start end &optional as-strings insrtp intrp)
  "Unrotate namestrings in region. 
Namestrings are formatted name per line e.g. `Lastname (Firstname Middlenames)'
Return `Firstname Middlename Lastname'
When INSRTP is non-nil or Called-interactively insert rotated names at point.
Does not move point. When AS-STRINGS is non-nil return rotated names as strings.\n
:EXAMPLE\n\(mon-line-string-unrotate-namestrings 
   (1+ \(search-forward-regexp \"►\"\)) \(- \(search-forward-regexp \"►\"\) 2\)\)\n
►\nKennan (George Frost)\nAlbert (Lukács János)\nAchesonn (Dean Gooderham)
Harriman (William Averell)\nMcCloy (John Jay)\nBohlen (Charles Eustis)
Lovett (Robert Abercrombie)\n►\n
:SEE-ALSO `mon-line-string-rotate-name', `mon-line-string-rotate-namestrings'
`mon-line-string-rotate-namestrings-combine' `mon-line-strings-to-list',
`mon-make-lastname-firstname', `naf-make-name-for-lisp', `mon-make-names-list'.\n►►►"
  (interactive "r\nP\ni\np")
  (let ((s-r start)
        (e-r end)
        (go-temp))
    (setq go-temp (buffer-substring-no-properties s-r e-r))
    (save-excursion
      (setq go-temp
            (with-temp-buffer
              (insert go-temp)
              (whitespace-cleanup)
              (goto-char (buffer-end 0))
              (while (search-forward-regexp  
                      "^\\([A-z-]+\\) \\((\\)\\(.*\\)\\()\\)$" (buffer-end 1) t)
                ;;..1.............2.......3......4....
                (replace-match  "\\3 \\1"))
              (if as-strings
                  (mon-line-strings-to-list (buffer-end 0) (buffer-end 1))
                (buffer-substring-no-properties (buffer-end 0) (buffer-end 1))))))
    (if (or insrtp intrp)
        (progn
          (save-excursion 
            (delete-region s-r e-r)
            (if as-strings
                (let ((as-str (read go-temp)))
                  (mapc (lambda (x) (newline)(prin1 (car x) (current-buffer))) as-str))
              (insert go-temp)))
          (when as-strings (delete-char 1)))
      ;; elseif
      (if as-strings
          (let ((as-str (read go-temp))
                (rtn-str))
            (setq rtn-str (mapcar (lambda (x) (car x)) as-str))
            rtn-str)
        go-temp))))
;;
;;; :TEST-ME 
;;; (mon-line-string-unrotate-namestrings 
;;;    (1+ (search-forward-regexp "►")) (- (search-forward-regexp "►") 2))
;;
;;,---- :UNCOMMENT-TO-TEST:
;;|►
;;|George Frost Kennan
;;|Dean Gooderham Acheson
;;|William Averell Harriman
;;|Lukács János Albert 
;;|John Jay McCloy
;;|Charles Eustis Bohlen 
;;|Robert Abercrombie Lovett
;;|►
;;`----

;;; ==============================
;;; :CREATED <Timestamp: #{2009-09-24T14:18:44-04:00Z}#{09394} - by MON>
(defun mon-line-string-rotate-namestrings-combine (start end &optional insertp intrp)
  "Return lists of namestrings from START to END both rotated and normalalized.
Elements of list returned have the form:
\(\"Fname Lname\" \"Lname \(Fname\)\"\)\n
:EXAMPLE\n\(mon-line-string-rotate-namestrings-combine
   (1+ \(search-forward-regexp \"►\"\)) \(- \(search-forward-regexp \"►\"\) 2\)\)\n
►\nEmil Max Hödel\nJohn Wilkes Booth\nLeon Frank Czolgosz\nLee Harvey Oswald
Dmitry Grigoriyevich Bogrov\nPaul Gorguloff\nJohn Bellingham
Charles Julius Guiteau\n►\n:SEE-ALSO\n
`mon-line-string-rotate-namestrings' `mon-line-string-unrotate-namestrings',
`mon-line-string-rotate-name', `mon-line-strings-to-list'.
`mon-make-lastname-firstname', `naf-make-name-for-lisp', `mon-make-names-list'.
►►►"
  (interactive "r\ni\np")
  (let ((rotd-nms (mon-line-string-rotate-namestrings start end t))
        (unrotd-nms)
        (combined))
    (with-temp-buffer
      (progn
        (save-excursion
          (mapc (lambda (x) (newline) (princ x (current-buffer))) rotd-nms))
        (delete-char 1))
      (setq unrotd-nms
            (mon-line-string-unrotate-namestrings (point-min) (point-max) t)))
    (mapc (lambda (x)
            (let ((orig (pop rotd-nms)))
              (setq combined (cons `(,x ,orig) combined))))
          unrotd-nms)
    (if (or insertp intrp)
        (prin1 combined (current-buffer))
      combined)))
;;
;;; :TEST-ME 
;;; (mon-line-string-rotate-namestrings-combine
;;;    (1+ (search-forward-regexp "►")) (- (search-forward-regexp "►") 2))
;;
;;,---- :UNCOMMENT-TO-TEST:
;;|►
;;|Emil Max Hödel
;;|John Wilkes Booth
;;|Leon Frank Czolgosz
;;|Lee Harvey Oswald
;;|Dmitry Grigoriyevich Bogrov
;;|Paul Gorguloff
;;|John Bellingham
;;|Charles Julius Guiteau
;;|►
;;`----

;;; ==============================
;;; :COURTESY Nelson H. F. Beebe :HIS clsc.el :VERSION 1.53 of 2001-05-27
(defun mon-get-word-list-buffer ()
  "Convert the entire buffer to a list of `newline' separated ``words''
in a new buffer *Word List*, where a word is defined by `forward-word'
according to the syntax-table settings.  You can apply `sort-lines' and
unique-lines to this to obtain a list of all the unique words in a
document.\n\n:SEE-ALSO `mon-line-strings-to-list', `mon-string-ify-current-line',
`mon-stringify-list', `mon-dropin-line-word', `mon-insert-string-ify',
`mon-word-count-analysis' `mon-word-count-occurrences', 
`mon-word-count-region', `mon-word-count-chars-region'.\n►►►"
  (interactive)
  (let (word)
    (with-output-to-temp-buffer "*Word List*"
      (save-excursion
	(goto-char (point-min))
	(while (setq word (mon-word-get-next))
	  (princ (format "%s\n" word)))))))

;;; ==============================
;;; :COURTESY Nelson H. F. Beebe :HIS clsc.el :VERSION 1.53 of 2001-05-27
;;; :WAS `get-next-word' -> `mon-word-get-next'
(defun mon-word-get-next ()
  "Return the next 'word' in the buffer, where a word is defined by
`forward-word' according to the syntax-table settings.  Point is left
following the word.  At `end-of-buffer', nil is returned and point is
unchanged.\n\n:SEE-ALSO `mon-line-get-next', `mon-get-word-list-buffer'.\n►►►"
  (let (start end)
    (if (eobp)
	nil
      (progn
	(setq start (point))
	(forward-word 1)
	(setq end (point))
	(forward-word -1)
	(if (< (point) start)           ;then already past last word
	    (progn
	      (goto-char (point-max))
              nil)
	  (setq start (point))
	  (goto-char end)
	  (buffer-substring-no-properties start end))))))

;;; ==============================
(defun mon-word-reverse-region (beg end)
  "Reverse the order of words in region.\n:SEE-ALSO `mon-region-reverse'.\n►►►"
  (interactive "r")
  (apply
   'insert
   (reverse
    (split-string
     (delete-and-extract-region beg end) "\\b"))))

;;; ==============================
;;; :COURTESY Jonathan Rockway :VERSION 2009-01-18
;;; (URL `http://blog.jrock.us/articles/Iterators%20in%20elisp.pod')
;;; REQUIRES:: (require 'cl)
(defun mon-word-iterate-over (buffer)
  "Return an iterator that gets the next word in buffer.
Uses lexical-let for a lambda closure over buf and pos.
Extract one word at a time by calling (funcall next-word).\n
:EXAMPLE For BUFFER test-buffer containing \"This is text.\"
\(setq next-word (mon-word-iterate-over-in (get-buffer \"test buffer\")))
The first time calling next-word, returns \"This\".
The next time, retruns get \" is\". Then, \" text.\". 
Finally, you returns nil forever.\n►►►"
  (lexical-let ((buf buffer)(pos 1))
    (lambda ()
      (save-excursion
        (let ((cur (current-buffer)) result)
          (switch-to-buffer buf)
          (goto-char pos)
          (forward-word)
          (let ((pt (point)))
            (if (not (eq pos pt))
                (progn 
                  (setq result (buffer-substring-no-properties pos pt))
                  (setq pos pt))))
          (switch-to-buffer cur) result)))))

;;; =======================
(defun mon-word-count-analysis (start end)
  "Count how many times each word is used in the region. Ignores punctuation.\n
:SEE-ALSO `mon-line-count-region', `mon-word-count-chars-region',
`mon-word-count-occurrences', `mon-word-count-region', 
`mon-get-word-list-buffer'.\n►►►"
  (interactive "r")
  (let (words)
	(save-excursion
	  (goto-char start)
	  (while (re-search-forward "\\w+" end t)
	    (let* ((word (intern (match-string 0)))
		   (cell (assq word words)))
	      (if cell
		  (setcdr cell (1+ (cdr cell)))
		(setq words (cons (cons word 1) words))))))
	(when (interactive-p)
	  (message "%S" words))
	words))

;;; ==============================
;;; :COURTESY Francois Fleuret <fleuret@idiap.ch> :HIS fleuret.emacs.el :WAS `ff/word-occurrences'
;;; (URL `http://www.idiap.ch/~fleuret/files/fleuret.emacs.el')
(defun mon-word-count-occurrences ()
  "Display in a new buffer the list of words sorted by number of occurrences.\n
:SEE-ALSO `mon-line-count-region', `mon-word-count-region',`mon-word-count-analysis',
`mon-word-count-chars-region', `mon-get-word-list-buffer'.\n►►►"
  (interactive)
  (let ((buf (get-buffer-create "*word counting*"))
        (map (make-sparse-keymap))
        (nb (make-hash-table))
        (st (make-hash-table))
        (result nil))
    ;; Collects all words in a hash table
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\([\\-a-zA-Z\\\\]+\\)" nil t)
        (let* ((s (downcase (match-string-no-properties 1)))
               (k (sxhash s)))
          (puthash k s st)
          (puthash k (1+ (gethash k nb 0)) nb))))
    ;; Creates the result buffer
    (define-key map "q" 'kill-this-buffer)
    (display-buffer buf)
    (set-buffer buf)
    (setq show-trailing-whitespace nil)
    (erase-buffer)
    ;; Builds a list from the hash table
    (maphash
     (lambda (key value)
       (setq result (cons (cons value (gethash key st)) result)))
     nb)
    ;; Sort and display it
    (mapc (lambda (x)
            (if (and (> (car x) 3)
                     ;; No leading backslash and at least four characters
                     (string-match "^[^\\]\\{4,\\}" (cdr x)))
                (insert (number-to-string (car x)) " " (cdr x) "\n")))
          (sort result (lambda (a b) (> (car a) (car b)))))
    ;; Adjust the window size and stuff
    (fit-window-to-buffer (get-buffer-window buf))
    (use-local-map map)
    (set-buffer-modified-p nil)))

;;; =======================
(defun mon-word-count-region (start end)
  "Return the number of words in the region.\n
:SEE-ALSO `mon-line-count-region', `mon-word-count-chars-region',
`mon-word-count-analysis', `mon-word-count-occurrences',
`mon-get-word-list-buffer'.\n►►►"
  (interactive "r")
    (save-excursion
      (save-restriction
        (narrow-to-region start end)
	(goto-char (point-min))
	(let ((matches (count-matches "\\sw+")))
	 (message "There are %s words in the Region." matches)))))

;;; ==============================
;;; :COURTESY Xah Lee (URL `http://xahlee.org/emacs/xah_emacs_generic.el')
(defun mon-word-count-chars-region (beginning end)
  "Return message indicating the number of words and chars that are in a region.\n
:SEE-ALSO `mon-line-count-region', `mon-word-count-region',
`mon-word-count-analysis', `mon-word-count-occurrences', 
`mon-get-word-list-buffer', `mon-string-from-sequence'.\n►►►"
  (interactive "r")
  (message "Counting ...")
  (save-excursion
    (let (wcount charCount)
      (setq wcount 0)
      (setq charCount (- end beginning))
      (goto-char beginning)
      (while (and (< (point) end)
                  (re-search-forward "\\w+\\W*" end t))
        (setq wcount (1+ wcount)))
      (message "Words: %d. Chars: %d." wcount charCount))))

;;; ==============================
;;; :COURTESY Henrik Enberg but prob. pulled out of:
;;; :COURTESY Stefan Reichor, stefan@xsteve.at :HIS xsteve-functions.el  
;;; NOT-WORKING-AS-OF:
;;; :CREATED <Timestamp: Tuesday February 17, 2009 @ 04:53.44 PM - by MON KEY>
;;; ==============================
;; (defun mon-query-remove-doubled-words (&optional force)
;;   "Find all doubled words and ask to remove them.
;; With optional arg FORCE remove them without asking."
;;   (interactive "P")
;;   (let ((case-fold-search t)
;; 	(del-counter 0))
;;     (while (re-search-forward
;; 	    "\\(\\<\\w\\{3,\\}\\>\\)[ \t\n]*\\(\\1\\)" nil t)
;;       (replace-highlight (match-beginning 2) (match-end 2))
;;       (unwind-protect
;; 	  (when (or force (y-or-n-p "Remove this doubled word? "))
;; 	    (delete-region (match-beginning 2) (match-end 2))
;; 	    (canonically-space-region (match-beginning 0) (match-end 0))
;; 	    (setq del-counter (1+ del-counter)))
;; 	(replace-dehighlight)))
;;     (if (> del-counter 0)
;; 	(message "Removed %d doubled %s." del-counter
;; 		 (if (< del-counter 1) "words" "word"))
;;       (message "No doubled words found or removed."))))
;;; =======================

;;; ==============================
;;; Rectangle Related Functions
;;; ==============================

;;; ==============================
;;; :CREATED <Timestamp: Friday June 05, 2009 @ 07:03.00 PM - by MON KEY>
(defun mon-rectangle-columns (start end)
  "Return column positions at START and END.
Mostly useful as a code template for rectangle related functions.\n►►►"
  (interactive "r")
  (let ((col-s (make-marker))
	(col-e (make-marker))
	(cols))
    (save-excursion
      (goto-char start)
      (set-marker col-s (point))
      (goto-char end)
      (set-marker col-e (point)))
    (setq cols    `(,(car (nth 6 (posn-at-point (marker-position col-s))))
		    ,(car (nth 6 (posn-at-point (marker-position col-e))))))
    cols))

;;; ==============================
;;; :COURTESY Alex Schroeder
;;; :MODIFICATIONS Charlie Hethcoat - Improved number regex.
(defun mon-rectangle-sum-column (start end)
  "Add all integer, decimal, and floating-point numbers in selected rectangle.\n
Numbers which can be read include (nonexhaustive):
2 +2 -2 2. +2. -2. 2.0 +2.0 -2.0 2e0 +2e0 -2e0 2E0 2e+0 2e-0, 2.e0, 2.0e0, etc.\n
:SEE-ALSO `mon-insert-numbers-padded', `mon-number-lines-region',
`mon-insert-string-incr', `mon-re-number-region'.\n►►►"
  (interactive "r")
  (save-excursion
    (kill-rectangle start end)
    (exchange-point-and-mark)
    (yank-rectangle)
    (set-buffer (get-buffer-create "*calc-sum*"))
    (erase-buffer)
    (yank-rectangle)
    (exchange-point-and-mark)
    (let ((sum 0))
      (while (re-search-forward
              "[-+]?\\([0-9]+\\(\\.[0-9]*\\)?\\|\\.[0-9]+\\)\\([eE][-+]?[0-9]+\\)?"
              nil t)
        (setq sum (+ sum (string-to-number (match-string 0)))))
      (message "Sum: %f" sum))))

;;; ==============================
;;; :COURTESY Noah Friedman <friedman@splode.com> :HIS buffer-fns.el 
;;; NOTE: Functions for modifying buffer contents or display.
;;; Brings in `operation-on-rectangle' for the old-school holmessss.
;;; :WAS `operate-on-rectangle' -> `apply-on-rectangle' -> `mon-rectangle-operate-on'
;;; ==============================
(defun mon-rectangle-operate-on (function start end &rest args)
  "Call FUNCTION for each line of rectangle with corners at START, END.
FUNCTION is called with two arguments: the start and end columns of the
rectangle, plus ARGS extra arguments.  Point is at the beginning of line when
the function is called.\n
:SEE-ALSO `mon-rectangle-operate-on', `mon-rectangle-apply-on-region-points',
`mon-rectangle-downcase', `mon-rectangle-upcase', `mon-rectangle-capitalize',
and `apply-on-rectangle' in `rect.el'.\n►►►"
  (let (startcol startpt endcol endpt)
    (save-excursion
      (goto-char start)
      (setq startcol (current-column))
      (beginning-of-line)
      (setq startpt (point))
      (goto-char end)
      (setq endcol (current-column))
      (forward-line 1)
      (setq endpt (point-marker))
      ;; ensure the start column is the left one.
      (if (< endcol startcol)
	  (let ((col startcol))
	    (setq startcol endcol endcol col)))
      ;; start looping over lines
      (goto-char startpt)
      (while (< (point) endpt)
	(apply function startcol endcol args)
	(forward-line 1)))))

;;; ==============================
;;; :COURTESY Noah Friedman <friedman@splode.com> :HIS buffer-fns.el
(defun mon-rectangle-apply-on-region-points (fun start end &rest args)
  "Like `apply-on-rectangle', but pass points in the buffer instead of columns.\n
:SEE-ALSO`mon-rectangle-operate-on', `mon-rectangle-apply-on-region-points',
`mon-rectangle-downcase', `mon-rectangle-upcase', `mon-rectangle-capitalize'.
►►►"
  (mon-rectangle-operate-on
   (lambda (bcol ecol)
     (apply fun
            (progn
              (move-to-column bcol 'coerce)
              (point))
            (progn
              (move-to-column ecol 'coerce)
              (prog1
                  (point)
                (beginning-of-line)))
            args))
   start end))

;;; ==============================
;;; :COURTESY Noah Friedman <friedman@splode.com> :HIS buffer-fns.el
(defun mon-rectangle-downcase (beg end)
  "Convert the marked rectangle to lower case.\n
:SEE-ALSO `mon-rectangle-upcase', `mon-rectangle-capitalize',
`mon-rectangle-operate-on', `mon-rectangle-apply-on-region-points'.\n►►►"
  (interactive "r")
  (mon-rectangle-apply-on-region-points 'downcase-region beg end))

;;; ==============================
;;; :COURTESY Noah Friedman <friedman@splode.com> :HIS buffer-fns.el
(defun mon-rectangle-upcase (beg end)
  "Convert the marked rectangle to upper case.
:SEE-ALSO `mon-rectangle-downcase' ,`mon-rectangle-operate-on',
`mon-rectangle-apply-on-region-points', `rect.el'."
  (interactive "r")
  (mon-rectangle-apply-on-region-points 'upcase-region beg end))

;;; ==============================
(defun mon-rectangle-capitalize (beg end)
  "Convert the marked rectangle to Title case.\n
:SEE-ALSO `mon-rectangle-downcase', `mon-rectangle-upcase', `mon-rectangle-capitalize'
`mon-rectangle-operate-on', `mon-rectangle-apply-on-region-points'.\n►►►"
  (interactive "r")
  (mon-rectangle-apply-on-region-points 'mon-region-capitalize beg end))

;;; ==============================
;;; TEXT-PROPERTIES:
;;; ==============================

;;; ==============================
;;; :CREATED <Timestamp: Monday May 11, 2009 @ 05:07.49 PM - by MON KEY>
(defun mon-line-test-content (syn-sym &optional rtrn-as-list)
  "Examine Syntax Location of SYN-SYM from point.
When syntax SYN-SYM is t advances point to end of syntax.
Return a formatted string describing syntax locations.
SYN-SYM arg is a symbol of type: 'word 'whitespace or 'punctuation.
When RTRN-AS-LIST is non-nil returns as list.\n\n:EXAMPLE
\(mon-line-test-content 'word)word =>
\"[line:413 word:word word-start:20267 word-end:20271]\"\n
\(mon-line-test-content 'word t\)word => 
\(413 word \"word\" 20269 20273\)
\(car cadr caddr cadddr cddddr\)
 line type found satart end\n
\(if \(> \(skip-syntax-forward \"^-\"\) 0\)
     \(mon-line-test-content 'whitespace t\)\)word more-word\n
:NOTE Function relies on current buffers local syntax table.\n
:SEE-ALSO `mon-test-props', `mon-view-help-source'.\n►►►"
  (let* ((syntax-type (cond 
		       ((eq syn-sym 'word) 'word)
		       ((eq syn-sym 'whitespace) 'whitespace)
		       ((eq syn-sym 'punctuation) 'punctuation)))
	 (syntax (cond 
		  ((eq syntax-type 'word)  '(syntax-type "\w" "word-start:" "word-end:"))
		  ((eq syntax-type 'whitespace) '(syntax-type "-" "spc-start:" "spc-end:"))
		  ((eq syntax-type 'punctuation) '(syntax-type "." "punct-start:" "punct-end:"))))
	 (start-of (caddr syntax))
	 (end-of (cadddr syntax))
	 (starting) (ending) (tlc-marker) (next))
    (setq tlc-marker (point-marker))
    (setq next (skip-syntax-forward (cadr syntax)))
    (let* ((lnap (line-number-at-pos))
	   (range-start (marker-position tlc-marker))
	   (range-end (point))
	   (at-here (buffer-substring-no-properties range-start range-end))
	   (syn-match at-here)
	   (syn-is (cond	   
		    ((and (eq syntax-type 'whitespace) (eq (string-match " " syn-match) 0)) t)
		    ((and (eq syntax-type 'whitespace) (not (eq (string-match " " syn-match) 0))) nil)
	   	    ;; not testing for numbers add another case if thats whats wanted
	   	    ((and (eq syntax-type 'word) (eq (string-match "[[:alpha:]]" syn-match) 0)) t)
	   	    ((and (eq syntax-type 'word) (not (eq (string-match "[[:alpha:]]" syn-match) 0))) nil)
	   	    ((and (eq syntax-type 'punctuation) (eq (string-match "[[:punct:]]" syn-match) 0)) t)
	   	    ((and (eq syntax-type 'punctuation) (not (eq (string-match "[[:punct:]]" syn-match) 0))) nil)))
	   (result-loc (cond 
			((and syn-is (eq syntax-type 'word)) ;test word
			 (format "[line:%d %s:%s %s%d %s%d]" 
				 lnap syntax-type syn-match start-of range-start end-of range-end))
			((and (not syn-is) (eq syntax-type 'word))
			 (format "[line:%d %s:_no_ %s%d %s%d]"
				 lnap syntax-type  start-of  range-start  end-of range-end))
			((and syn-is (eq syntax-type 'whitespace)) ;test whitespace
			 (format "[line:%d %s:_yes_ %s%d %s%d]" 
				 lnap syntax-type start-of range-start end-of range-end))
			((and (not syn-is) (eq syntax-type 'whitespace))
			 (format "[line:%d %s:_no_ %s%d %s%d]"
				 lnap syntax-type start-of  range-start  end-of range-end))
			((and syn-is (eq syntax-type 'punctuation)) ;test punctuation
			 (format "[line:%d %s:%s %s%d %s%d]" 
				 lnap syntax-type syn-match start-of range-start end-of range-end))
			((and (not syn-is) (eq syntax-type 'punctuation))
			 (format "[line:%d %s:_no_ %s%d %s%d]"
				 lnap syntax-type start-of  range-start  end-of range-end))))
	   (result-location (cond 
			     ((and syn-is (eq syntax-type 'word)) ;test word
			      `(,lnap ,syntax-type ,syn-match ,range-start ,range-end))
			     ((and (not syn-is) (eq syntax-type 'word))
			      `(,lnap ,syntax-type nil ,range-start ,range-end))
			     ((and syn-is (eq syntax-type 'whitespace))	;test whitespace
			      `(,lnap ,syntax-type ,syn-match ,range-start ,range-end))
			     ((and (not syn-is) (eq syntax-type 'whitespace))
			      `(,lnap ,syntax-type nil ,range-start ,range-end))
			     ((and syn-is (eq syntax-type 'punctuation)) ;test punctuation
			      `(,lnap ,syntax-type ,syn-match ,range-start ,range-end))
			     ((and (not syn-is) (eq syntax-type 'punctuation))
			      `(,lnap ,syntax-type nil ,range-start ,range-end))
			     )))
      (if rtrn-as-list
	  result-location
	result-loc))))
;;
;;; :TEST-ME (mon-line-test-content 'word t)this-word
;;; :TEST-ME (mon-line-test-content 'word)this-word
;;; :TEST-ME (mon-line-test-content 'word) this-word
;;; :TEST-ME (mon-line-test-content 'word t) this-word
;;; :TEST-ME (mon-line-test-content 'whitespace) this-word
;;; :TEST-ME (mon-line-test-content 'whitespace t) this-word
;;; :TEST-ME (mon-line-test-content 'whitespace)this-word
;;; :TEST-ME (mon-line-test-content 'whitespace t)this-word
;;; :TEST-ME (mon-line-test-content 'punctuation t),this-word
;;; :TEST-ME (mon-line-test-content 'punctuation),this-word
;;; :TEST-ME (mon-line-test-content 'punctuation t)this-word
;;; :TEST-ME (mon-line-test-content 'punctuation),his-word
;;; :TEST-ME (car (mon-line-test-content 'word t))word
;;; :TEST-ME (car (nthcdr 1 (mon-line-test-content 'word t)))word
;;; :TEST-ME (car (nthcdr 2 (mon-line-test-content 'word t)))word
;;; :TEST-ME (car (nthcdr 3 (mon-line-test-content 'word t)))word
;;; :TEST-ME (car (nthcdr 4 (mon-line-test-content 'word t)))word
;;; :TEST-ME (mon-line-test-content 'word)word => "[line:413 word:word word-start:20267 word-end:20271]"
;;; :TEST-ME (mon-line-test-content 'word t)word => (413 word "word" 20269 20273)
;;; :TEST-ME (car cadr caddr cadddr cddddr)
;;; :TEST-ME (if (> (skip-syntax-forward "^-") 0) (mon-line-test-content 'whitespace t))word more-word

;;; ==============================
(defun mon-test-props ()
  "Test for a category text-property. 
Helper function for `mon-view-help-source'\n
:SEE-ALSO `mon-test-props', `mon-view-help-source',`mon-line-test-content'.\n►►►"
  (let* ((to-view ((lambda () (text-properties-at (point)))))
	 (my-props `(,@to-view))
	 (prop-value (plist-get my-props 'category)))
    prop-value))
;;
(defun mon-view-help-source ()
  ":SEE-ALSO `mon-test-props', `mon-line-test-content'."
  (interactive)
  (unwind-protect			;body
       (let ((gb))
         (if (or (equal (buffer-name)(help-buffer))
                 (string= "*Help*" (buffer-name)))
             (save-window-excursion
               (goto-char (point-min))
               (while (not (eobp))      ;
                 (let ((this-change)
                       (next-change
                        (or (next-property-change (point) (current-buffer))
                            (point-max))))
                   (progn
                     (goto-char next-change)
                     (setq this-change (mon-test-props))
                     (when (and
                            (and (string= this-change 'help-function-def-button))
                            (and (ffap-file-at-point)))
                       (let* ((f-to-get-fl (ffap-file-at-point)))
                         (view-file-other-window (ffap-file-at-point))
                         (setq gb (find-buffer-visiting f-to-get-fl)))
                       gb)))))))))

;;; ==============================
;;; :COURTESY Pascal J. Bourguignon :HIS list.lisp :WAS PLIST-REMOVE
;;; :CREATED <Timestamp: #{2009-09-28T17:32:55-04:00Z}#{09401} - by MON>
(defun plist-remove (plist prop)
  "DO:      (remf plist prop)
RETURN:  The modified PLIST.\n
:SEE-ALSO `mon-plist-keys'\n►►►"
  (remf plist prop)
  plist)

;;; ==============================
;;; :NOTE Keep with `mon-list-all-properties-in-buffer', `mon-plist-keys',
;;; :COURTESY Pascal J. Bourguignon :HIS pjb-emacs.el
;;; `mon-nuke-text-properties-buffer'
(defun mon-plist-keys (plist)
  (if (null plist)
      plist
      (cons (car plist) (mon-plist-keys (cddr plist)))))
;;
;;; ==============================
;;; :COURTESY Pascal J. Bourguignon :HIS pjb-emacs.el
;;; :NOTE Keep with `mon-nuke-text-properties-buffer', `mon-plist-keys'
(defun mon-list-all-properties-in-buffer (buffer)
  (save-excursion
    (set-buffer buffer)
    (delete-duplicates
     (loop
        for i from (point-min) to (point-max)
        nconc  (delete-duplicates (mon-plist-keys (text-properties-at i nil)))))))
;;
;;; ==============================
;;; :COURTESY Pascal J. Bourguignon :HIS pjb-emacs.el
;;; NOTE: Keep with `mon-list-all-properties-in-buffer', `mon-plist-keys'
(defun mon-nuke-text-properties-buffer ()
  (interactive)
  (remove-list-of-text-properties
   (point-min)
   (point-max)
   (mon-list-all-properties-in-buffer (current-buffer))))

;;; ==============================
;;; :COURTESY  ../emacs/lisp/font-lock.el 
;;; NOTE: For completeness: this is to `remove-text-properties' as
;;; `put-text-property' ; is to `add-text-properties', etc. Included therein but
;;; commented out by SM as 'Additional text property functions' these may
;;; eventually become C builtins.
;;; For consistency: maybe this should be called `remove-single-property' like
;;; `next-single-property-change' (not `next-single-text-property-change'), etc.
;;; :WAS `remove-text-property'        -> ../emacs/lisp/font-lock.el
(defun mon-remove-text-property (start end property &optional object)
  "Remove a property from text from START to END.
Argument PROPERTY is the property to remove.
Optional argument OBJECT is the string or buffer containing the text.
Return t if the property was actually removed, nil otherwise.\n
:SEE-ALSO `mon-remove-single-text-property', `remove-text-property',
`mon-nuke-text-properties-region', `next-single-property-change',
`add-text-properties', `put-text-property'.\n►►►"
  (remove-text-properties start end (list property) object))
;;
;;; :WAS `remove-single-text-property' -> ../emacs/lisp/font-lock.el
(defun mon-remove-single-text-property (start end prop value &optional object)
 "Remove a specific property value from text from START to END.
Arguments PROP and VALUE specify the property and value to remove.  The
resulting property values are not equal to VALUE nor lists containing VALUE.
Optional argument OBJECT is the string or buffer containing the text.
:SEE-ALSO `mon-remove-text-property', , `remove-text-property',
`mon-nuke-text-properties-region', `next-single-property-change',
`add-text-properties', `put-text-property'.\n►►►"
 (let ((start (text-property-not-all start end prop nil object)) next prev)
   (while start
     (setq next (next-single-property-change start prop object end)
	    prev (get-text-property start prop object))
     (cond ((and (symbolp prev) (eq value prev))
	     (mon-remove-text-property start next prop object))
	    ((and (listp prev) (memq value prev))
	     (let ((new (delq value prev)))
	       (cond ((null new)
		      (mon-remove-text-property start next prop object))
		     ((= (length new) 1)
		      (put-text-property start next prop (car new) object))
		     (t
		      (put-text-property start next prop new object))))))
     (setq start (text-property-not-all next end prop nil object)))))

;;; ==============================
;;; :COURTESY Noah Friedman :HIS buffer-fns.el
(defun mon-nuke-text-properties-region (beg end)
  "Eliminate all text properties in marked region of current buffer.
This only removes text properties, not overlays.\n
:SEE-ALSO `mon-remove-single-text-property', `mon-remove-text-property'
`mon-nuke-text-properties-region', `remove-text-property',
`next-single-property-change', `add-text-properties', `put-text-property'.\n►►►"
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (not (eobp))
        (let ((inhibit-read-only t)
              (plist (text-properties-at (point)))
              (next-change (or (next-property-change (point) (current-buffer))
                               (point-max))))
          (remove-text-properties (point) next-change plist (current-buffer))
          (goto-char next-change))))))

;;; ==============================
;;; Elisp Related
;;; ==============================

;;; ==============================
;;; :COURTESY Andy Stewart :HIS lazycat-toolkit.el
;;; :WAS insert-after -> `mon-elt->' 
;;; :WAS insert-before -> `mon-elt-<'
;;; :WAS list-set-elt -> `mon-elt->elt'
;;; :WAS list-exchange-els -> `mon-elt-<elt'
;;; :CREATED <Timestamp: Wednesday June 03, 2009 @ 06:31.33 PM - by MON KEY>
;;; ==============================
(defun mon-elt-> (list aft-el el)
  "Insert EL after AFT-EL in LIST.\n
:SEE-ALSO `mon-elt->', `mon-elt->elt', `mon-elt-<', `mon-elt-<elt'.\n►►►"
  (push el (cdr (member aft-el list)))
  list)
;;
(defun mon-elt-< (list bef-el el)
  "Insert EL before BEF-EL in LIST.\n
:SEE-ALSO `mon-elt->', `mon-elt->elt', `mon-elt-<', `mon-elt-<elt'.\n►►►"
  (nreverse (mon-elt-> (nreverse list) bef-el el)))
;;
(defun mon-elt->elt (list old-el new-el)
  "Set OLD-EL to NEW-EL in LIST.\n
:SEE-ALSO `mon-elt->', `mon-elt->elt', `mon-elt-<', `mon-elt-<elt'.\n►►►"
  (setcar (member old-el list) new-el)
  list)
;;
(defun mon-elt-<elt (list el1 el2)
  "Exchange places of EL1 and EL2 in LIST.\n
:SEE-ALSO `mon-elt->', `mon-elt->elt', `mon-elt-<', `mon-elt-<elt'.\n►►►"
  (when (or (null (member el1 list))
            (null (member el2 list)))
    (error "el1 or el2 is not in list.")))
;;; ==============================

;;; ==============================
;;; :COURTESY Jean-Marie Chauvet :HIS ncloseemacs-ml-dataset.el :WAS `sublist'
;;; :CREATED <Timestamp: #{2009-09-19T19:10:14-04:00Z}#{09386} - by MON>
(defun mon-sublist (skip-n return-n in-list)
  "RETURN-N elements IN-LIST skipping the first SKIP-N.\n
:EXAMPLE
\(let \(\(piece-work
       '\(A B \(C D\) E \(F G\) \(Q \(H I\)\) K\)\)\)
        ;0 1   2   3  4     5        6
  \(mon-sublist 4 2 piece-work\)\)
;=> \((F G) (Q (H I)))
     ;4     5\n
:SEE-ALSO `mon-sublist-gutted' `mon-remove-dups', `mon-assoc-replace',
`mon-moveq', `mon-flatten', `mon-transpose', `mon-maptree', 
`mon-recursive-apply' `mon-map-append', `mon-combine'\n►►►"
  (let* ((sub (nthcdr skip-n in-list)) 
	 (q (length sub)))
    (reverse (nthcdr (- q return-n) (reverse sub)))))
;;
;;; :TEST-ME (mon-sublist 0 1 '(A B (C D) E (F G) (Q (H I)) K))
;;; :TEST-ME (mon-sublist 3 3 '(A B (C D) E (F G) (Q (H I)) K))
;;; :TEST-ME (mon-sublist 5 2 '(A B (C D) E (F G) (Q (H I)) K))
;;; :TEST-ME (mon-sublist 1 2 '(A B (C D) E (F G) (Q (H I)) K))  
;;; :TEST-ME (mon-sublist 6 1 '(A B (C D) E (F G) (Q (H I)) K))  

;;; ==============================
;;; :COURTESY Jean-Marie Chauvet :HIS ncloseemacs-ml-dataset.el :WAS `sublist-rest'
;;; :CREATED <Timestamp: #{2009-09-19T18:55:37-04:00Z}#{09386} - by MON>
(defun mon-sublist-gutted (gut-from-n to-n-ards gut-list)
  "Return GUT-LIST with GUTS-FROM-N TO-N-ARDS extracted.\n
:EXAMPLE\n\(let \(\(eviscerate-me 
       '\(A B \(C D\) E \(F G\) \(Q \(H I\)\) K\)\)\)
        ;0 1   2   3  4     5        6
  \(mon-sublist-gutted 4 2 eviscerate-me\)\)\n;=> \(A B \(C D\) E K\)\n
     ;0 1   2   3 6\n
:SEE-ALSO `mon-sublist'. `mon-flatten', `mon-transpose', `mon-remove-dups',
`mon-assoc-replace', `mon-moveq' `mon-maptree', `mon-recursive-apply',
`mon-map-append', `mon-combine'.\n►►►"
  (let* ((pre-guts 
          (nthcdr (length (nthcdr gut-from-n gut-list)) (reverse gut-list))) ;; pre-guts reversed
	 (post-guts 
          (nthcdr (+ to-n-ards (length pre-guts)) gut-list)))
    (append (reverse pre-guts) post-guts))) ;;:WAS (append prefix postfix)
;;
;;; :TEST-ME (mon-sublist-gutted 3 1 '(A B (C D) E (F G) (Q (H I)) K))
;;; :TEST-ME (mon-sublist-gutted 5 2 '(A B (C D) E (F G) (Q (H I)) K))
;;; :TEST-ME (mon-sublist-gutted 5 1 '(A B (C D) E (F G) (Q (H I)) K))
;;; :TEST-ME (mon-sublist-gutted 0 6 '(A B (C D) E (F G) (Q (H I)) K))

;;; ==============================
;;; :COURTESY Jean-Marie Chauvet nclose-eieio.el :WAS `map-append'
;;; :CREATED <Timestamp: #{2009-09-21T15:26:14-04:00Z}#{09391} - by MON KEY>
(defun mon-map-append (mapping-l)
  "Append all sublists in list.\n
:SEE-ALSO `mon-maptree', `mon-recursive-apply', `mon-combine',
`mon-flatten', `mon-transpose', `mon-sublist', `mon-sublist-gutted', 
`mon-remove-dups', `mon-assoc-replace', `mon-moveq'\n►►►"
  (cond ((null mapping-l) nil)
	(t (append (car mapping-l) (mon-map-append (cdr mapping-l))))))

;;; ==============================
;;; :COURTESY Jared D. :WAS `assoc-replace'
;;; (URL `http://curiousprogrammer.wordpress.com/2009/07/26/emacs-utility-functions/')
;;; :CREATED <Timestamp: #{2009-08-19T20:00:51-04:00Z}#{09344} - by MON KEY>
(defun mon-assoc-replace (seq values)
  "Replace an element within an association list where the cars match.\n
:SEE-ALSO `mon-moveq',`mon-remove-dups', `mon-maptree', `mon-recursive-apply',
`mon-map-append', `mon-combine',`mon-flatten', `mon-transpose', `mon-sublist',
`mon-sublist-gutted'.\n►►►"
  (mapcar (lambda (elem)
            (let* ((key (car elem))
                   (val (assoc key values)))
              (if val (cadr val) elem))) seq))

;;; ==============================
;;; :COURTESY Jared D. :WAS `remove-dupes'
;;; (URL `http://curiousprogrammer.wordpress.com/2009/07/26/emacs-utility-functions/')
;;; :CREATED <Timestamp: #{2009-08-19T20:10:43-04:00Z}#{09344} - by MON KEY>
(defun mon-remove-dups (list)
"Remove duplicate adjoining elts in LIST.\n
:SEE-ALSO `mon-assoc-replace', `mon-moveq' `mon-maptree', `mon-recursive-apply',
`mon-map-append', `mon-combine', `mon-flatten', `mon-transpose', `mon-sublist',
`mon-sublist-gutted'.\n►►►"
  (let (tmp-list head)
    (while list
      (setq head (pop list))
      (unless (equal head (car list))
        (push head tmp-list)))
    (reverse tmp-list)))

;; ==============================
;;; :COURTESY Henry Kautz :HIS refer-to-bibtex.el :WAS `moveq'
;;; :CREATED <Timestamp: 2009-08-04-W32-2T18:57:30-0400Z - by MON KEY>
(defmacro mon-moveq (new old)
  "Set NEW to OLD and set OLD to nil.\n
:SEE-ALSO `mon-maptree', `mon-recursive-apply' `mon-map-append', `mon-combine',
`mon-flatten', `mon-transpose', `mon-sublist', `mon-sublist-gutted', 
`mon-remove-dups', `mon-assoc-replace'.►►►"
  (list 'progn (list 'setq new old) (list 'setq old 'nil)))

;;; ==============================
;;; :COURTESY Pascal J. Bourguignon :HIS pjb-list.el :WAS `flatten'
(defun mon-flatten (tree)
  "Return a tree containing all the elements of the `tree'.\n
:SEE-ALSO `mon-maptree', `mon-recursive-apply' `mon-map-append', `mon-combine',
`mon-transpose', `mon-sublist', `mon-sublist-gutted', `mon-remove-dups',
`mon-assoc-replace', `mon-moveq'►►►"
  (do ((result nil)
       (stack  nil))
      ((not (or tree stack)) (nreverse result))
    (cond ((null tree)
	   (setq tree (pop stack)))
	  ((atom tree)
	   (push tree result)
	   (setq tree (pop stack)))
	  ((listp (car tree))
	   (push (cdr tree) stack)
	   (setq tree (car tree)))
	  (t (push (car tree) result)
	     (setq tree (cdr tree))))))

;;; ==============================
;;; :COURTESY Pascal J. Bourguignon :HIS list.lisp :WAS TRANSPOSE
;;; :CREATED <Timestamp: #{2009-09-28T17:40:47-04:00Z}#{09401} - by MON>
(defun mon-transpose (tree)
  "Return a tree where all the CAR and CDR are exchanged.\n
:SEE-ALSO `mon-maptree', `mon-recursive-apply' `mon-map-append', `mon-combine',
`mon-flatten',`mon-sublist', `mon-sublist-gutted',  `mon-remove-dups',
`mon-assoc-replace', `mon-moveq'►►►"
  (if (atom tree)
      tree
      (cons (mon-transpose (cdr tree)) (mon-transpose (car tree)))))
;;
;;; :TEST-ME (mon-transpose '(a (bb cc) dd)))
;;; :TEST-ME (mon-flatten (mon-transpose '(a (bb cc) dd)))

;;; ==============================
;;; :COURTESY Pascal J. Bourguignon :HIS ???
(defun mon-combine (&rest args)
  "Return the set of tuples built taking one item in order from each list
in ARGS.\n:EXAMPLE\n\(mon-combine '\(www ftp\) '\(exa\) '\(com org\)\)\)\n
;=> \(\(www exa com\) \(www exa org\) \(ftp exa com\) \(ftp exa org\)\)\n
:SEE-ALSO `mon-maptree', `mon-recursive-apply' `mon-map-append',
`mon-flatten', `mon-transpose', `mon-sublist', `mon-sublist-gutted', 
`mon-remove-dups', `mon-assoc-replace', `mon-moveq'.►►►"
  (cond ((null args) '(nil))
        ((consp (car args))
         (mapcan (lambda (item) (apply (function combine) item (cdr args)))
                 (car args)))
        (t (mapcan (lambda (rest) (list (cons (car args) rest)))
                   (apply (function combine) (cdr args))))))

;;; ==============================
;;; :COURTESY Pascal J. Bourguignon :HIS list.lisp :WAS MAPTREE
;;; :CREATED <Timestamp: #{2009-09-28T17:40:37-04:00Z}#{09401} - by MON>
(defun mon-maptree (fun &rest trees)
"Map function FUN over trees or TREES.\n
:EXAMPLE\n\(mon-maptree
 #'\(lambda \(x\) \(when \(stringp x\) \(prin1 x\)\)\) '\(a \(\"b\" b cc\) dd\)\)\n
:SEE-ALSO `mon-recursive-apply' `mon-map-append', `mon-combine',
`mon-flatten', `mon-transpose', `mon-sublist', `mon-sublist-gutted', 
`mon-remove-dups', `mon-assoc-replace', `mon-moveq'\n►►►"
  (cond ((null trees) nil)
        ((every (function null)  trees) nil)
        ((every (function atom)  trees) (apply fun trees))
        ((every (function consp) trees)
         (cons (apply (function mon-maptree) fun (mapcar (function car) trees))
               (apply (function mon-maptree) fun (mapcar (function cdr) trees))))
        (t nil)))
;;
;;; :TEST-ME (mon-maptree #'(lambda (x) (when (stringp x) (prin1 x)))  '(a ("b" b cc) dd))
;;;=> "b" (nil ("b" nil nil) nil)

;;; ==============================
;;; :COURTESY Pascal J. Bourguignon :HIS pjb-utilities.el :WAS recursive-apply?
(defun mon-recursive-apply (atom-func list-a list-b)
  "Apply recursively the function atom-func on each and every pairs
that can be found recursively in the two parallel structures a-list
and b-list. Only the elements from a-list must be an atom to be passed
to atom-func.\n
:EXAMPLE(1)\n\n\(mon-recursive-apply '+ '\(\(1 2\) \(3 4\)\) '\(\(1 0\) \(0 1\)\)\)\n
:EXAMPLE(2)\n\n(mon-recursive-apply\n #'\(lambda \(atom other\) \(cons atom other\)\)
   '\(apple orange peach\) '\(\(red yellow green\) \(orange\) \(yellow white\)\)\)\n
:SEE-ALSO `mon-maptree', `mon-map-append', `mon-combine', `mon-flatten',
`mon-transpose', `mon-sublist', `mon-sublist-gutted', `mon-remove-dups',
`mon-assoc-replace', `mon-moveq'.\n►►►"
  (cond ((null list-a) nil)
        ((atom list-a) (apply atom-func (list list-a list-b)))
        (t (cons (mon-recursive-apply atom-func (car list-a) (car list-b)) 
                 (mon-recursive-apply atom-func (cdr list-a) (cdr list-b))))))

;;; ==============================
;;; :COURTESY Pascal J. Bourguignon :HIS pjb-utilities.el :WAS foreach
(defmacro mon-foreach (var list &rest body)
  "A foreach style macro idiom for looping over vars in a list with body.\n
  (foreach i ;;<-var			;
	   '(1 2 3 4) ;;<-list		;
	   (+ i i)) ;;<-body		;
  (foreach i '(1 2 3 4) (+ i i)) 
  => (2 4 6 8)\n
  :SEE-ALSO `mon-for', `mon-loop'.\n►►►"
  `(mapcar (lambda (,var) ,@body) ,list))

;;; ==============================
;;; :COURTESY Pascal J. Bourguignon :HIS pjb-utilities.el :WAS `for'
(defmacro mon-for (var  init  final  &rest body)
  "Execute a simple for loop.\n
:EXAMPLE\n\(for i  1  10  \(print i\)\)\n
:SEE-ALSO `mon-foreach', `mon-loop'.\n►►►"
  (let ((tempvar (make-symbol "max")))
    `(let ((,var ,init)
           (,tempvar ,final))
       (if (< ,var ,tempvar)
           (while (<= ,var ,tempvar)
             ,@body
             (setq ,var (+ ,var 1)))
           (while (>= ,var ,tempvar)
             ,@body
             (setq ,var (- ,var 1)))))))

;;; ==============================
;;; :COURTESY Pascal J. Bourguignon :HIS pjb-emacs.el :WAS `rloop'
;;; :MODIFICATIONS <Timestamp: #{2009-09-29T13:35:36-04:00Z}#{09402} - by MON KEY>
(defmacro mon-loop (clauses &rest body)
  "Macro to execute a loop over clauses.
:SEE-ALSO `mon-foreach', `mon-for'.\n►►►"
  (if (null clauses)
      `(progn ,@body)
    `(loop ,@(car clauses) do (mon-loop ,(cdr clauses) ,@body))))

;;; ==============================
;;; :MODIFICATIONS <Timestamp: Saturday May 30, 2009 @ 06:26.12 PM - by MON KEY>
(defun mon-escape-lisp-string-region (start end)
  "Escape special characters in the region as if a lisp string.
Insert backslashes in front of special characters (namely  `\' backslash,
`\"' double quote, `(' `)' parens in the region, according to the docstring escape 
requirements.\n\nNOTE:\n Don't run this on docstrings with regexps.\n
Region should only contain the characters actually comprising the string
supplied without the surrounding quotes.\n
See also`mon-unescape-lisp-string-region'.\n►►►"
  (interactive "*r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char start)
      (while (search-forward "\\" nil t)
	(replace-match "\\\\" nil t))
      (goto-char start)
      (while (search-forward "\"" nil t)
        (replace-match "\\\"" nil t))
      ;; MON ADDITIONS
      (goto-char start)
      (while (search-forward "(" nil t)
	(replace-match "\\\(" nil t))
      (goto-char start)
      (while (search-forward ")" nil t)
	(replace-match "\\\)" nil t)))))

;;; ==============================
(defun mon-unescape-lisp-string-region (start end)
  "Unescape special characters from the CL string specified by the region.
This amounts to removing preceeding backslashes from characters they escape.\n
:NOTE region should only contain the characters actually comprising the string
without the surrounding quotes.\n:SEE-ALSO `mon-escape-lisp-string-region'.\n►►►"
  (interactive "*r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char start)
      (while (search-forward "\\" nil t)
	(replace-match "" nil t)
	(forward-char)))))

;;; ==============================
;;; :CREATED <Timestamp: Wednesday May 20, 2009 @ 02:13.22 PM - by MON KEY>
(defun mon-princ-cb ()
  "Wrap region in a princ->current-buffer to eval and print newline\\result after point.
:SEE-ALSO `mon-eval-sexp-at-point'\n►►►"
  (interactive)
  (save-excursion
    ;; (let (sexp-pnt
  (mon-wrap-text "(progn(newline)(princ\n" "\n(current-buffer)))")))

;;; ==============================
;;; :CREATED <Timestamp: Wednesday May 20, 2009 @ 03:14.44 PM - by MON KEY>
(defun mon-eval-sexp-at-point ()
  "Evaluate S-expression at point print commented result on nl.
Return point after commented result. Best on trivial expressions.\n
:EXAMPLE\n\(+ 1 3)\n;;;=>4\n^point^\n\n:SEE-ALSO `mon-princ-cb'\n►►►"
  (interactive)
  (let* ((wrap (sexp-at-point))
	 (val (eval wrap))      
	 (comnt "\n;;;=>")
	 (comn-sexp (format "%S%s%S"  wrap comnt val))
	 (bnds))
    (save-excursion
      (if (not (eobp))
	  (forward-line)
	(newline))
      (insert comn-sexp))
    (setq bnds (bounds-of-thing-at-point 'sexp))
    (delete-region (car bnds) (cdr bnds))
    (when (mon-line-bol-is-eol)
      (delete-char 1))
    (search-forward-regexp "^;;;=>.*$" nil t)))
;;
(defun mon-eval-print-last-sexp ()
  "Like `eval-print-last-sexp' but does not move point.\n►►►"
  (interactive)
  (save-excursion
    (eval-print-last-sexp)))
;;
;;; :TEST-ME (+ 1 3) (mon-eval-print-last-sexp)

;;; ==============================
;;; :CREATED <Timestamp: Thursday June 25, 2009 @ 12:59.22 PM - by MON KEY>
(defun mon-eval-expression (eval-expression-arg &optional eval-expression-insert-value)
  "This is `eval-expression' with the EVAL-EXPRESSION-INSERT-VALUE defaulted to t.
Gets us eval-expression automatically inserted into current-buffer.\n►►►"
  (interactive
   (list (let ((minibuffer-completing-symbol t))
	   (read-from-minibuffer "Eval: "
				 nil read-expression-map t
				 'read-expression-history))
         ;; the only point of this function is to 
         ;; set current-prefix-arg to default to t
         ;; CHANGED:
         ;; current-prefix-arg)) 
	 t))
  (if (null eval-expression-debug-on-error)
      (setq values (cons (eval eval-expression-arg) values))
    (let ((old-value (make-symbol "t")) new-value)
      (let ((debug-on-error old-value))
	(setq values (cons (eval eval-expression-arg) values))
	(setq new-value debug-on-error))
      (unless (eq old-value new-value)
	(setq debug-on-error new-value))))
  (let ((print-length eval-expression-print-length)
	(print-level eval-expression-print-level))
    (if eval-expression-insert-value
	(with-no-warnings
          (let ((standard-output (current-buffer)))
            (prin1 (car values))))
      (prog1
          (prin1 (car values) t)
        (let ((str (eval-expression-print-format (car values))))
          (if str (princ str t)))))))

;;; ==============================
;;; :TODO `mon-semnav-up' and `mon-extend-selection' need default key bindings.
;;; :COURTESY Nikolaj Schumacher :VERSION 2008-10-20
(defun mon-extend-selection (arg &optional incremental)
  "Mark symbol surrounding point.
Subsequent calls mark higher levels of sexps.\n►►►"
  (interactive
   (list (prefix-numeric-value current-prefix-arg)
	 (or (and transient-mark-mode mark-active)
	     (eq last-command this-command))))
  (if incremental
      (progn
        (mon-semnav-up (- arg))
        (forward-sexp)
        (mark-sexp -1))
    (if (> arg 1)
        (mon-extend-selection (1- arg) t)
      (if (looking-at "\\=\\(\\s_\\|\\sw\\)*\\_>")
          (goto-char (match-end 0))
        (unless (memq (char-before) '(?\) ?\"))
          (forward-sexp)))
      (mark-sexp -1))))

;;; ==============================
;;; :COURTESY Nikolaj Schumacher  :VERSION 2008-10-20
;;; (URL `http://xahlee.org/emacs/syntax_tree_walk.html')
;;; :CREATED <Timestamp: Sunday January 18, 2009 - by MON KEY>
(defun mon-semnav-up (arg)
  (interactive "p")
  (when (nth 3 (syntax-ppss))
    (if (> arg 0)
        (progn
          (skip-syntax-forward "^\"")
          (goto-char (1+ (point)))
          (decf arg))
      (skip-syntax-backward "^\"")
      (goto-char (1- (point)))
      (incf arg)))
  (up-list arg))

;;; ==============================
;;; :COURTESY Nelson H. F. Beebe :HIS clsc.el :WAS `show-columns'
;;; :CREATED <Timestamp: 2009-08-04-W32-2T19:07:47-0400Z - by MON KEY>
(defun mon-show-columns ()
 "Show a numbered column display above the current line.
With ARG, column display begins at current column, instead of at left margin.
►►►"
 (interactive)
 (let* ((leading-blanks
         (if (null current-prefix-arg) 0 (current-column)))
        (column-display (concat (make-string leading-blanks ?\ )
                                "123456789.123456789.123456789."
                                "123456789.123456789.123456789."
                                "123456789.123456789.123456789."
                                "123456789.123456789.123456789."
                                "123456789.123456789.123456789."
                                "123456789.123456789.123456789."
                                "123456789.123456789.123456789."
                                "123456789.123456789.123456789.")))
   (save-excursion
     (forward-line -1)
     (momentary-string-display
      (substring column-display
                 0 (min (1- (window-width)) (length column-display)))
      (point)))))
;;
;;; :TEST-ME (call-interactively 'mon-show-columns)

;;; ==============================
;;; :COURTESY Dave Pearson <davep@davep.org> :HIS nukneval.el :WAS `nuke-and-eval'
;;; :VERSION 1.1 Copyright 2002 GPL V2 (URL `http://www.davep.org/emacs/nukneval.el')
;;; :NOTE Nukes and reevaluates an elisp buffer. 
;;; ==============================
(defun mon-nuke-and-eval ()
  "Attempt to cleanly re-evaluate a buffer of elisp code.\n►►►"
  (interactive)
  (save-excursion
    (setf (point) (point-min))
    (loop for form = (condition-case nil
                         (read (current-buffer))
                       (error nil))
          while form
          do (let ((type (car form))
                   (name (cadr form)))
               (cond
                 ((memq type '(defun defun* defsubst defalias defmacro))
                  (fmakunbound name))
                 ((memq type '(defvar defparameter defconst defcustom))
                  (makunbound name))))))
  (eval-buffer))

;;; ==============================
;;; :COURTESY Dave Pearson <davep@davep.org> :HIS unbind.el
;;; :VERSION 1.3 - Copyright 2002 - GPL v2 (URL `http://www.davep.org/emacs/unbind.el')
;;; :WAS `unbind-command'    -> `mon-unbind-command'
;;; :WAS `unbind-variable'   -> `mon-unbind-variable'
;;; :WAS `unbind-defun'      -> `mon-unbind-defun'
;;; :WAS `unbind-symbol'     -> `mon-unbind-symbol'
;;; :WAS `unbind-function'   -> `mon-unbind-function'
;;; :NOTE Commands for unbinding things.
;;; ==============================
(defun mon-unbind-defun ()
  "Unbind the `defun' near `point' in `current-buffer'.\n►►►"
  (interactive)
  (save-excursion
    (if (and (beginning-of-defun) (looking-at "(defun"))
        (fmakunbound (cadr (read (current-buffer))))
      (error "No defun found near point"))))
;;keep-with-above
(defun mon-unbind-symbol (symbol)
  "Totally unbind SYMBOL. Includes unbinding function binding, variable binding,
and property list.\n►►►"
  (interactive "SSymbol: ")
  (fmakunbound symbol)
  (makunbound symbol)
  (setf (symbol-plist symbol) nil))
;; keep-with-above.
(defun mon-unbind-function (symbol)
  "Remove the function binding of SYMBOL.\n►►►"
  (interactive "aFunction: ")
  (fmakunbound symbol))
;; keep-with-above.
(defun mon-unbind-command (symbol)
  "Remove the command binding of SYMBOL.\n►►►"
  (interactive "CCommand: ")
  (fmakunbound symbol))
;; keep-with-above.
(defun mon-unbind-variable (symbol)
  "Remove the variable binding of SYMBOL.\n►►►"
  (interactive (list 
                (completing-read "Variable: "
                                 (loop for s being the symbols
                                       when (boundp s) collect (list (symbol-name s))))))
  (makunbound (if (stringp symbol) (intern symbol) symbol)))
;;
;;; ==============================
;;; :END Pearson's commands for unbinding things.
;;; ==============================

;;; ==============================
;;; :COURTESY Thierry Volpiatto :WAS `tv-dump-object-to-file'
;;; (URL `http://lists.gnu.org/archive/html/emacs-devel/2009-09/msg00846.html')
;;; :CREATED <Timestamp: #{2009-10-01T12:31:29-04:00Z}#{09404} - by MON>
(defun mon-dump-object-to-file (obj file)
  "Save symbol object OBJ to the byte compiled version of FILE.
OBJ can be any lisp object, list, hash-table, etc.
FILE is an elisp file with ext *.el.
Loading the *.elc file will re-institute object.\n
:NOTE This function utilizes an documented feature of `eval-when-compile'. It
can be interesting way to save a persistent elisp object. Using `setf' combined
with `eval-when-compile' is a convenient way to save lisp objects like
hash-table."
  (require 'cl)          ;; Be sure we use the CL version of `eval-when-compile'.
  (if (file-exists-p file)
      (error "File already exists.")
      (with-temp-file file
        (erase-buffer)
        (let* ((str-obj (symbol-name obj))
               (fmt-obj (format "(setq %s (eval-when-compile %s))" str-obj str-obj)))
          (insert fmt-obj)))
      (byte-compile-file file)
      (delete-file file)
      (message "`%s' dumped to %sc" obj file)))


;;; ==============================
(defun mon-byte-compile-and-load ()
  "Byte compile and load the current .el file.
This was only easily accesible from the menu.\n►►►"
  (interactive)
  (byte-compile-file buffer-file-name t))

;;; ==============================
;;; :COURTESY Francois Fleuret <fleuret@idiap.ch> :HIS fleuret.emacs.el
;;; (URL `http://www.idiap.ch/~fleuret/files/fleuret.emacs.el')
;;; :WAS `ff/compile-when-needed' -> `mon-compile-when-needed'
;;; ==============================
(defun mon-compile-when-needed (name)
  "Compile the given file only if needed. 
Add .el if required, and use `load-path' to find it.\n►►►"
  (if (not (string-match "\.el$" name))
      (mon-compile-when-needed (concat name ".el"))
    (mapc (lambda (dir)
            (let* ((src (concat dir "/" name)))
              (when (file-newer-than-file-p src (concat src "c"))
                (if (let ((byte-compile-verbose nil))
                      (condition-case nil
                          (byte-compile-file src)
                        (error nil)))
                    (message (format "Compiled %s" src ))
                  (message (format "Failed compilation of %s" src))))))
          load-path)))

;;; ==============================
;;; :COURTESY Francois Fleuret <fleuret@idiap.ch> :WAS `ff/load-or-alert'
;;; This is useful when using the same .emacs in many places.
;;; ==============================
(defun mon-load-or-alert (name &optional compile-when-needed)
  "Try to load the specified file and insert a warning message in a
load-warning buffer in case of failure.\n►►►"
  (when compile-when-needed (mon-compile-when-needed name))
  (if (load name t nil) t
    (let ((buf (get-buffer-create "*loading warnings*")))
      (display-buffer buf)
      (set-buffer buf)
      (insert (propertize "Warning:" 'face 'font-lock-warning-face) " could not load '" name "'\n")
      (fit-window-to-buffer (get-buffer-window buf))
      (set-buffer-modified-p nil))
    nil))

;;; ==============================
(provide 'mon-utils)
;;; ==============================

;;; ==============================
;;; mon-utils.el ends here
;;; EOF
