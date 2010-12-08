;;; mon-event-utils.el --- event related procedures for mon-*utils features
;; -*- mode: EMACS-LISP; -*-

;;; ================================================================
;; Copyright © 2010 MON KEY. All rights reserved.
;;; ================================================================

;; FILENAME: mon-event-utils.el
;; AUTHOR: MON KEY
;; MAINTAINER: MON KEY
;; CREATED: 2010-11-22T16:28:13-05:00Z
;; VERSION: 1.0.0
;; COMPATIBILITY: Emacs23.*
;; KEYWORDS: lisp, extensions, emacs,

;;; ================================================================

;;; COMMENTARY: 

;; =================================================================
;; DESCRIPTION:
;; mon-event-utils provides event related procedures for mon-*utils features
;;
;; FUNCTIONS:►►►
;; `mon-inhibit-modification-hooks', `mon-inhibit-read-only',
;; `mon-inhibit-point-motion-hooks', `mon-toggle-read-only-point-motion',
;; `mon-scroll-down-in-place', `mon-scroll-up-in-place', `mon-line-move-n',
;; `mon-line-move-next', `mon-line-move-prev', `mon-read-keys-as-string',
;; `mon-read-multiple', `mon-postion-for-x-popup-menu', `mon-choose-from-menu',
;; `mon-test-keypresses', `mon-abort-recursive-edit',
;; `mon-abort-autosave-when-fucked', `mon-rotate-ascii-cursor',
;; `mon-toggle-truncate-line', `mon-kill-appending', `mon-append-to-register',
;; `mon-catch-meta-key', `mon-decode-meta-key-event',
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
;; `*mon-popup-pos-x-offset*', `*mon-ascii-cursor-state*',
;;
;; GROUPS:
;;
;; ALIASED/ADVISED/SUBST'D:
;; <UNQUALIFIED-ALIAS>                 <CORE-SYMBOL>
;; <UNQUALIFIED-ALIAS>                 <PREFIX>-<NON-CORE-SYMBOL>
;; <PREFIX>-<QUALIFIED>                <CORE-SYMBOL>
;; `mon-prin1-char->?char'             -> `prin1-char'
;;
;; <PREFIX>-<QUALIFIED>                <PREFIX>-<NON-CORE-SYMBOL>
;; `mon-region-append-to-register'   -> `mon-append-to-register'
;; `mon-append-next-kill'            -> `mon-kill-appending' 
;; `mon-read-keys-last-event'        -> `mon-test-keypresses'
;; `scroll-up-in-place'              -> `mon-scroll-up-in-place'
;; `scroll-down-in-place'            -> `mon-scroll-down-in-place'
;; `mon-string-from-keybard-input'   -> `mon-read-keys-as-string'
;;
;; DEPRECATED:
;;
;; RENAMED:
;; `mon-trunc'                       -> `mon-toggle-truncate-line'
;;
;; MOVED:
;; `mon-catch-meta-key'              <- mon-empty-registers.el
;; `mon-decode-meta-key-event'       <- mon-empty-registers.el
;; `mon-postion-for-x-popup-menu'    <- mon-utils.el
;; `mon-choose-from-menu'            <- mon-utils.el 
;; `mon-scroll-up-in-place'          <- mon-utils.el
;; `mon-scroll-down-in-place'        <- mon-utils.el
;; `mon-line-move-n'                 <- mon-utils.el
;; `mon-line-move-prev'              <- mon-utils.el
;; `mon-line-move-next'              <- mon-utils.el
;; `mon-rotate-ascii-cursor'         <- mon-utils.el
;; `mon-test-keypresses'             <- mon-utils.el
;; `mon-read-keys-as-string'         <- mon-utils.el
;; `mon-read-multiple'               <- mon-utils.el
;; `mon-abort-recursive-edit'        <- mon-utils.el
;; `mon-abort-autosave-when-fucked'  <- mon-utils.el
;; `mon-append-to-register'          <- mon-utils.el
;; `mon-append-to-buffer'            <- mon-utils.el
;; `mon-kill-appending'              <- mon-utils.el
;; `*mon-popup-pos-x-offset*'        <- mon-utils.el
;; `*mon-ascii-cursor-state*'        <- mon-utils.el
;;
;; TODO:
;;
;; NOTES:
;;
;; SNIPPETS:
;;
;; REQUIRES:
;;
;; THIRD-PARTY-CODE:
;;
;; URL: http://www.emacswiki.org/emacs/mon-event-utils.el
;; FIRST-PUBLISHED:
;;
;; EMACSWIKI: { URL of an EmacsWiki describing mon-event-utils. }
;;
;; FILE-CREATED:
;; <Timestamp: #{2010-11-22T16:28:13-05:00Z}#{10471} - by MON KEY>
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

(unless (and (intern-soft "*IS-MON-OBARRAY*")
             (bound-and-true-p *IS-MON-OBARRAY*))
(setq *IS-MON-OBARRAY* (make-vector 17 nil)))

;;; ==============================
;;; :CHANGESET 2291
;;; :CREATED <Timestamp: #{2010-11-09T22:26:44-05:00Z}#{10452} - by MON KEY>
(defcustom *mon-popup-pos-x-offset* 
  (or (and (eq window-system 'x) 40)
      (and (eq window-system 'w32) 110) 
      0 ;; terminal
      )
  "Position for top place an `x-popup-menu'.\n
:EXAMPLE\n\n
:SEE-ALSO `*css-complete-popup-pos-x-offset*'.\n►►►"
  :type  'integer
  :group 'mon-base)

;;; ==============================
(defun mon-inhibit-read-only (func-arg)
  "Evaluate FUNC-ARG at point with `inhibit-read-only' t.\n
Evaluation occurs inside an unwind protect so 'safe-enough' 
for invoking 'one off functions' such-as `kill-line' without
the tedium of building the entire scaffolding.\n
:EXAMPLE\n\n\(let \(\(tt \(propertize \"I'm read only!\" 'read-only t\)\)
      \(buffer-read-only nil\)\)
  \(line-move -5\) 
  \(insert tt\)\(sit-for 2\)\(beginning-of-line\)
  \(mon-inhibit-read-only 'kill-line\)\)\n
:SEE-ALSO `mon-with-inhibit-buffer-read-only',
`mon-with-inhibit-buffer-read-only-TEST', `mon-inhibit-modification-hooks',
`mon-inhibit-point-motion-hooks', `mon-toggle-read-only-point-motion',
`view-read-only', `view-mode-disable'.\n►►►"
  (let ((re-inhibit (if (not inhibit-read-only) t nil)))
    (unwind-protect
        (progn 
          ;; :NOTE What about `buffer-read-only'?
          ;; Why setq?
          (setq inhibit-read-only t)
          (eval `(,func-arg)))
      (when re-inhibit (setq inhibit-read-only nil)))))
;;
;;; :TEST-ME (let ((tt (propertize "I'm read only!" 'read-only t))
;;;                   (buffer-read-only nil))
;;;               (line-move -5) 
;;;               (insert tt)(sit-for 2)(beginning-of-line)
;;;               (mon-inhibit-read-only 'kill-line))

;;; ==============================
(defun mon-inhibit-modification-hooks (func-arg)
  "Evaluate FUNC-ARG at point with `inhibit-modification-hooks' t.\n
FUNC-ARG is a function which evaluates without parameters.
Evaluation occurs inside an unwind protect so 'safe-enough' 
for invoking 'one off functions' such-as `kill-line' without
the tedium of building the entire scaffolding.\n
:SEE-ALSO `mon-inhibit-read-only', `mon-inhibit-point-motion-hooks',
`mon-toggle-read-only-point-motion'.\n►►►"
  (let ((re-inhibit (if (not inhibit-modification-hooks) t nil)))
    (unwind-protect
         (progn 
           ;; Why `setq'?
           (setq inhibit-modification-hooks t)
           (eval `(,func-arg)))
      (when re-inhibit (setq inhibit-modification-hooks nil)))))

;;; ==============================
(defun mon-inhibit-point-motion-hooks (func-arg)
  "Evaluate FUNC-ARG at point with `inhibit-point-motion-hooks' t.\n
Evaluation occurs inside an unwind protect so 'safe-enough' 
for invoking 'one off functions' such-as `kill-line' without
the tedium of building the entire scaffolding.\n
:SEE-ALSO `mon-inhibit-read-only', `mon-inhibit-modification-hooks',
`mon-toggle-read-only-point-motion'.\n►►►"
  (let ((re-inhibit (if (not inhibit-point-motion-hooks) t nil)))
    (unwind-protect
	(progn 
          ;; why setq?
	  (setq inhibit-point-motion-hooks t)
	  (eval `(,func-arg)))
      (when re-inhibit (setq inhibit-point-motion-hooks nil)))))

;;; ==============================
;;; :TODO I'm no longer sure if this is TRT should we be checking for
;;; `buffer-local-variables'/`buffer-local-value' of these too?
;;; :CREATED <Timestamp: Monday June 15, 2009 @ 05:36.12 PM - by MON KEY>
(defun mon-toggle-read-only-point-motion ()
  "Toggle `inhibit-read-only' and `inhibit-point-motion-hooks'.\n
:SEE-ALSO `mon-inhibit-read-only', `mon-inhibit-point-motion-hooks',
`mon-inhibit-modification-hooks', `mon-naf-mode-toggle-restore-llm'.\n►►►"
  (interactive)
  ;; :NOTE What about `buffer-read-only'?
  (if (or (bound-and-true-p inhibit-read-only)
          (bound-and-true-p inhibit-point-motion-hooks)) ;;inhibit-read-only))
      (progn
	(setq inhibit-read-only nil)
	(setq inhibit-point-motion-hooks nil))
    (progn
      (setq inhibit-read-only t)
      (setq inhibit-point-motion-hooks t))))

;;; ==============================
;;; :RENAMED `mon-trunc' -> `mon-toggle-truncate-line'
;;; :MODIFICATIONS <Timestamp: #{2009-10-26T15:45:18-04:00Z}#{09441} - by MON KEY>
(defun mon-toggle-truncate-line (&optional intrp)
  "Toggle the truncate-line variable and redraw the display.\n
When optional arg intrp is non-nil or called-interactively message user that
change has occured.\n
:EXAMPLE\n\n\(mon-toggle-truncate-line\)\n
\(mon-toggle-truncate-line t\)\n
:SEE-ALSO `mon-toggle-eval-length', `print-length',
`mon-toggle-dired-dwim-target', `mon-toggle-menu-bar'
`mon-toggle-show-point-mode', `mon-naf-mode-toggle-restore-llm', 
`mon-toggle-read-only-point-motion', `mon-inhibit-modification-hooks',
`mon-inhibit-point-motion-hooks', `mon-inhibit-read-only'.\n►►►"
  (interactive "p")
  (toggle-truncate-lines nil)
  ;; (buffer-local-value 'truncate-lines (current-buffer))
  (if intrp 
      (message
       (concat ":FUNCTION `mon-toggle-truncate-line' " 
               (if truncate-lines
                   "-- truncating lines (... $)"
                 "-- wrapping lines (...\\)"))))
  (redraw-display))

;;; ==============================
;;; :RENAMED `scroll-down-in-place' -> `mon-scroll-down-in-place'
(defun mon-scroll-down-in-place (&optional down-by)
  "Scroll with the cursor in place, moving screen DOWN-BY instead.\n
:SEE-ALSO `mon-scroll-up-in-place', `scroll-up', `scroll-down',
`mon-line-move-n', `mon-line-move-next'.\n►►►"
  (interactive "p")
  (let* ((inhibit-redisplay t)
        (down-by (abs (or down-by 1)))
        (next-screen-context-lines down-by))
    (if (forward-line (- down-by))
        (ignore-errors (scroll-down down-by)))))

;;; ==============================
;;; :RENAMED `scroll-up-in-place' -> `mon-scroll-up-in-place'
(defun mon-scroll-up-in-place (&optional up-by)
  "Scroll with the cursor in place, moving the screen UP-BY lines instead.\n
:SEE-ALSO `mon-scroll-down-in-place', `scroll-up', `scroll-down'
`mon-line-move-n', `mon-line-move-prev'.\n►►►"
  (interactive "P")
  (let* ((inhibit-redisplay t)
         (up-by (abs (or up-by 1)))
         (next-screen-context-lines up-by))
    (when (forward-line up-by)
      (ignore-errors (scroll-up up-by)))))

;;; ==============================
;;; :COURTESY :FILE thing-at-point.el
;;; :TODO Wrap in a function and install under bol/eol funcs in mon-utils.el
;;; :CREATED <Timestamp: #{2009-09-14T15:15:57-04:00Z}#{09381} - by MON KEY>
;;; (funcall (lambda () (if (bolp) (forward-line -1) (beginning-of-line))))

;;; ==============================
;;; :CHANGESET 1885
;;; :CREATED <Timestamp: #{2010-06-16T11:43:14-04:00Z}#{10243} - by MON KEY>
(defun mon-line-move-n (&optional move-cnt) 
  "Move cursor as if by `line-move' with its NOERROR arg non-nil.\n
When optional arg MOVE-CNT non-nil move cursor n lines.\n
Default is to move 0 lines.
:EXAMPLE\n\n\(mon-line-move-next\)
\(mon-line-move-next 3\)\n
\(mon-line-move-next -3\)\n
:NOTE Function is intended for use with `mon-keybind-*' functions which would
otherwise duplicate anonymous forms with identical behavior.\n
:SEE-ALSO `mon-line-move-prev', `mon-line-move-next',
`mon-scroll-down-in-place', `mon-scroll-up-in-place'.\n►►►"
  (interactive "P")
  (line-move (or (and (integerp move-cnt) move-cnt) 0) t))

;;; ==============================
;;; :CHANGESET 1885
;;; :CREATED <Timestamp: #{2010-06-16T12:09:24-04:00Z}#{10243} - by MON KEY>
(defun mon-line-move-next (&optional move-next-cnt)
  "Move cursor vertically forward as if by `mon-line-move-n'.\n
When optional arg MOVE-NEXT-CNT non-nil move cursor n lines. Default is 1.\n
:EXAMPLE\n\n(mon-line-move-next)\n\n\(mon-line-move-next 3\)\n
:NOTE Function is intended for invocation from `mon-keybind-*' functions which would
otherwise duplicate anonymous forms with identical behavior.\n
:SEE-ALSO `mon-line-move-prev', `mon-scroll-down-in-place',
`mon-scroll-up-in-place'.\n►►►"
  (interactive "P")
  (mon-line-move-n 
   (or (and (integerp move-next-cnt) (abs move-next-cnt)) 1)))

;;; ==============================
;;; :CHANGESET 1885
;;; :CREATED <Timestamp: #{2010-06-16T12:09:54-04:00Z}#{10243} - by MON KEY>
(defun mon-line-move-prev (&optional move-prev-cnt)
  "Move cursor vertically forward as if by `mon-line-move-n'.\n
When optional arg MOVE-PREV-CNT non-nil move cursor n lines. Default is -1.\n
:EXAMPLE\n\n\(mon-line-move-prev\)\n\n\(mon-line-move-prev 3)\n
:NOTE Function is intended for invocation from `mon-keybind-*' functions which would
otherwise duplicate anonymous forms with identical behavior.\n
:SEE-ALSO `mon-line-move-next', `mon-scroll-down-in-place',
`mon-scroll-up-in-place'.\n►►►"
  (interactive "P")
  (mon-line-move-n 
   (or (and (integerp move-prev-cnt) (- (abs move-prev-cnt))) -1)))


;;; ==============================
;;; :COURTESY :FILE bookmark.el :WAS `bookmark-read-search-input'
;;; :CREATED <Timestamp: #{2010-09-13T15:16:54-04:00Z}#{10371} - by MON>
(defun mon-read-keys-as-string (&optional w-kbd-quit)
  "Read keyboard input, return a list of what was read.
When W-KBD-QUIT is non-nil a when C-g is caught tail of list is non-nil.
:EXAMPLE\n\n\(mon-read-keys-as-string\)\n
\(mon-read-keys-as-string t\)\n
:ALIASED-BY `mon-string-from-keyboard-input'\n
:SEE-ALSO `mon-test-keypresses', `mon-help-key-functions', `read-key',
`unread-command-events', `this-single-command-raw-keys'.\n►►►"
  ;; (interactive "P")
  (let ((prompt       (propertize (concat ":FUNCTION `mon-read-keys-as-string' "
                                          " -- keys (RET/ESC to exit): ")
                                  'face 'minibuffer-prompt))
        ;; (inhibit-quit t) ; inhibit-quit is evil.  Use it with extreme care!
        (tmp-list     ())
        mrkas-read
        caught-cg-flag)
    (while
        (let ((char (read-key (concat prompt mrkas-read))))
          (case char
            ((?\e ?\r) nil) ;; RET or ESC break the search loop.
            ;; :NOTE Uncomment below to re-enable C-g `keyboard-quit'
            ;; (?\C-g (setq caught-cg-flag t) nil)
            (?\C-g (when w-kbd-quit (setq caught-cg-flag t)) (push char tmp-list) t); nil)
            (?\d (pop tmp-list) t)      ; Delete last char of pattern with DEL
            (t (if (characterp char)
                   (push char tmp-list)
                 (setq unread-command-events
                       (nconc (mapcar 'identity
                                      (this-single-command-raw-keys))
                              unread-command-events))
                 nil))))
      (setq mrkas-read      
            (apply 'string (reverse tmp-list))))
    `(,mrkas-read ,caught-cg-flag)))
;;
;;; :TEST-ME (mon-read-keys-as-string)
;;; :TEST-ME (mon-read-keys-as-string t)

;;; ==============================
;;; :PREFIX "mmrn-"
;;; :NOTE Inspired by Thierry Volpiatto :HIS tv-utils.el :WAS `multi-read-name'
;;; :MODIFICATIONS <Timestamp: #{2010-03-30T14:02:31-04:00Z}#{10132} - by MON KEY>
;;; :CHANGESET 2291 <Timestamp: #{2010-11-11T20:52:23-05:00Z}#{10454} - by MON KEY>
(defun mon-read-multiple (&optional multi-fun &rest multi-fun-args)
  "Prompt indefinely while a comma \",\" \(char 44\) is  suffixed to read value.\n
Return a list of containing each input read.\n
When MULTI-FUN is non-nil, it is a symbol naming an input function which returns
a string. If MULTI-FUN returns some other object type default to reading only
one value. Default is `read-string'.\n
MULTI-FUN-ARGS are additional arguments to pass to MULTI-FUN.
When non-nil PROMPT args should be ommitted.\n
:EXAMPLE\n\n\(mon-read-multiple\)\n
\(mon-read-multiple 'read-face-name\)\n
\(mon-read-multiple 'read-variable\)\n
\(mon-read-multiple 'read-char\)\n
\(mon-read-multiple 'read-command '\(\"doctor\" \"5x5\"\)\)\n
:SEE-ALSO `read-string', `read-directory-name', `completing-read-multiple'.\n►►►"
  (catch 'unsupported
    (let* ((mmrn-var (make-symbol "mmrn-var"))
           (mmrn-bail '((read-variable . "variable")
                        (read-command   . "command")
                        (read-coding-system . "coding system")
                        (read-color     . "color")
                        (read-event     . "event")
                        (read-key          . "key")
                        (read-key-sequence . "key sequence")
                        (read-key-sequence-vector . "keys")
                        (read-number    . "number")
                        (read-charset   . "character set")
                        (read-char      . "char")
                        (read-char-by-name   . "char (by name)")
                        (read-char-exclusive . "char (exclusive)")
                        (read-file-modes . "File modes (octal or symbolic)")
                        ))
           (mmrn-fun-prompt 
            (or (or (and (memq multi-fun 
                               (mapcar #'car 
                                       (setq mmrn-bail 
                                             (mapcar #'(lambda (mmrn-L-1)
                                                         (cons (car mmrn-L-1) 
                                                               (format ":FUNCTION `%s' -- %s: " 
                                                                       (car mmrn-L-1) (cdr mmrn-L-1))))
                                                     mmrn-bail))))
                         (throw 'unsupported
                                (apply multi-fun (cdr (assq multi-fun mmrn-bail)) multi-fun-args)))
                    (and (eq multi-fun 'read-face-name)
                         (throw 'unsupported 
                                (read-face-name ":FUNCTION `read-face-name' -- (add \",\" to repeat)" nil t))))
                (and (null multi-fun) 
                     (setq multi-fun 'read-string)
                     (cons (format ":FUNCTION `%s' " multi-fun)
                           (case multi-fun 
                             (read-string "string") 
                             (read-directory-name "directory")
                             (read-file-name "file")
                             (read-buffer "buffer")
                             (read-buffer-to-switch "buffer")
                             (read-passwd "password")
                             (read-envvar-name "enviroment variable")
                             (t "thing"))))))
           ;; The duplicate local var prevents byte-compiler whining
           mmrn-multiread
           (mmrn-multiread #'(lambda (&optional mread-stack)
                               (let ((mmrn-str 
                                      (apply multi-fun 
                                             (concat (car mmrn-fun-prompt)
                                                     (format
                                                      (or (and mread-stack 
                                                               "-- %ss currently read:\n %S\n-- (add \",\" to repeat): ")
                                                          "-- which %s (add \",\" to repeat): " )
                                                      (cdr mmrn-fun-prompt) mread-stack))
                                             multi-fun-args)))
                                 (and (or (and (car (setq mmrn-str (cons (string-match-p "," mmrn-str) mmrn-str)))
                                               (setcdr mmrn-str 
                                                       (mon-string-not-null-nor-zerop 
                                                        (substring (cdr mmrn-str) 0 (car mmrn-str))))
                                               (push (cdr mmrn-str) mmrn-var))
                                          t)
                                      (or 
                                       (and (car mmrn-str)
                                            (funcall mmrn-multiread mmrn-var))
                                       (and 
                                        (or (and (mon-string-not-null-nor-zerop (cdr mmrn-str))
                                                 (not (string-match-p "^[[:blank:]]+$" (cdr mmrn-str)))
                                                 (push (cdr mmrn-str) mmrn-var))
                                            t)
                                        (nreverse mmrn-var))))))))
      (save-window-excursion
        (let (mmrn-var)
          (funcall mmrn-multiread))))))

;;; ==============================
;;; :NOTE Duplicates functionality of `css-pos-for-x-popup-menu' in 
;;;  (lookup-key global-map [menu-bar]) `mouse-menu-bar-map'
;;; `mouse-selection-click-count-buffer'
;;; (posn-x-y (event-start (read-event nil nil 0.0))
;;; :FILE mon-css-complete.el
;;; :CHANGESET 2291 
;;; :CREATED <Timestamp: #{2010-11-09T22:29:47-05:00Z}#{10452} - by MON KEY>
(defun mon-postion-for-x-popup-menu ()                              
  "Return a position for displaying an `x-popup-menu'.\n
Return value has the form:\n
 \(\(<POSN-X> <POSN-Y>\) <POSN-WINDOW>\)\n
Returned value is informed by variable `*mon-popup-pos-x-offset*'.\n
:EXAMPLE\n\n\(mon-postion-for-x-popup-menu\)\n
\(popup-menu naf-mode-menu \(mon-postion-for-x-popup-menu\)\)\n
:SEE-ALSO `posn-at-point', `posn-x-y', `posn-window', `mouse-pixel-position',
`css-pos-for-x-popup-menu'.\n►►►"
  (let* ((cpfxpm     (posn-at-point))
         (cpfxpm-x-y (posn-x-y cpfxpm)) ;; (posn-x-y cpfxpm))
         (cpfxpm-win (posn-window cpfxpm)))
    `((,(+ *mon-popup-pos-x-offset* (car cpfxpm-x-y)) ,(cdr cpfxpm-x-y))
      ,cpfxpm-win)))

;;; ==============================
;;; :COURTESY Sandip Chitale <sandipchitale@attbi.com>
(defun mon-choose-from-menu (menu-title menu-items &optional menu-posn)
  "Choose from a list of choices from a popup menu.\n
:EXAMPLE\n\n\(mon-choose-from-menu \"Bubbas-Choice\"
 '\(\"one-bubba\" \"two-bubba\" \"three-bubba\"\)\)\n
\(mon-choose-from-menu \"Bubbas-Choice\"
 '\(\"one-bubba\" \"two-bubba\" \"three-bubba\"\) 
   \(mon-postion-for-x-popup-menu\)\)\n
:SEE-ALSO `choose-completion', `x-popup-menu',
`mon-postion-for-x-popup-menu', `popup-menu', `mouse-pixel-position'
`mouse-menu-bar-map'.\n►►►"
  (let (mcfm-item mcfm-item-list)
    (while menu-items
      (setq mcfm-item (car menu-items))
      (if (consp mcfm-item)
          (setq mcfm-item-list 
                (cons (cons (car mcfm-item) (cdr mcfm-item) ) mcfm-item-list))
        (setq mcfm-item-list 
              (cons (cons mcfm-item mcfm-item) mcfm-item-list)))
      (setq menu-items (cdr menu-items)))
    (x-popup-menu 
     (or menu-posn t)
     (list menu-title (cons menu-title (nreverse mcfm-item-list))))))
;;
;;; :TEST-ME (mon-choose-from-menu "Bubbas-Choice" '("one-bubba" "two-bubba" "three-bubba"))

;;; ==============================
;;; :NOTE consider macrology? BUGGY but :WORKING-AS-OF
;;; :CREATED <Timestamp: #{2009-09-09T12:29:52-04:00Z}#{09373} - by MON>
(defun mon-test-keypresses (&optional first second third)
  "Use to test if additioanl optional prefix args have been passed to interactive.\n
:EXAMPLE\nM-34 M-x mon-test-keypresses\n
=> \(\(meta . 51\) \(meta . 52\) \(meta . 120\) mon-test-keypresses\)
:ALIASED-BY `mon-read-keys-last-event'\n
:SEE-ALSO `mon-read-keys-as-string', `event-basic-type',
`this-command-keys-vector', `event-modifiers', `current-prefix-arg',
`mon-help-key-functions'.\n►►►"
  (interactive "P\nP\np")
  (let ((accum-string '())
	(accum-event '())
	(self 'mon-test-keypresses))
    (mapc #'(lambda (x) 
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
;;
;;; :TEST-ME (mon-test-keypresses 1 2 3) ;->("cj")("cj")
;;; :TEST-ME (call-interactively 'mon-test-keypresses);-> ("cj")("cj")

;;; ==============================
;;; :CREATED <Timestamp: 2009-08-04-W32-2T17:43:27-0400Z - by MON KEY>
(defun mon-decode-meta-key-event (event)
  "The key <meta> lives on the 2**27 bit:\n
\(expt 2 27) ;=> 134217728\n
<Meta>-some-ASCII-key in range 1-127 is 2**27 + char\n
So, to decode M-3 i.e. '<meta>-3' do this:\n
\(- \(+ ?3 \(expt 2 27\)\) \(expt 2 27\)\) ;=> 51 
e.g. \(- 134217779  134217728\) ;=> 51\n
:EXAMPLE\n\(mon-decode-meta-key-event 134217771\)\n
:SEE-ALSO `mon-catch-meta-key' `mon-coerce->char', `mon-string-to-symbol'.\n►►►"
  (let ((M-key (expt 2 27)))
    ;; (event-key event))
    (if (> event M-key)
        (list (car (event-modifiers event))
              ;; (- event-key M-key))
              (mon-string-to-symbol (char-to-string (event-basic-type event)))))))
;;              
;;; :TEST-ME (mon-decode-meta-key-event 134217771)

;;; ==============================
;;; :CREATED <Timestamp: 2009-08-04-W32-2T19:26:05-0400Z - by MON KEY>
(defun mon-catch-meta-key (&optional event->string) ;(event-vect)
  "Return the first <meta>-? key prefix call to wrapper function.\n
When optional arg EVENT->STRING is non-nil return a meta event as a string.
When a meta-key event is not present return the first event modifer passed.
Can be alled programatically within a wrapper functions.\n
:EXAMPLE\n\(mon-catch-meta-key\)   ;<- M-3 C-x C-e ;=> (meta 51)
\(mon-catch-meta-key t\) ;<- M-3 C-x C-e ;=>\"meta-3\"\n
:SEE-ALSO `mon-decode-meta-key-event', `mon-catch-meta-key' `mon-coerce->char',
`mon-string-to-symbol'.\n►►►"
  (let ((key-seq (listify-key-sequence (this-command-keys-vector)));event-vect))
        map-events
        psn-if)
    (setq map-events (mapcar #'(lambda (x) 
                                 (car (event-modifiers x)))
                             key-seq))
    (let ((psn-idx 0)
          (evnts map-events)
          fnd)
      (while (and (not fnd) (not (null evnts)))
        (if (not (eq (pop evnts) 'meta))
            (setq psn-idx (1+ psn-idx))
          (setq fnd psn-idx)))
      (when fnd (setq psn-if fnd)))
    ;; :WAS (if (equal (cl::position 'meta map-events) 0)
    ;; (eq (cl::position 'meta map-events) 0)
    (if (and psn-if (eq psn-if 0))
        (cond (event->string
               (format "%s-%s" 
                       (car (event-modifiers (car key-seq)))
                       (char-to-string (event-basic-type (car key-seq)))))
               ;; (if (and (>= (event-basic-type (car key-seq)) 48)
               ;;          (<= (event-basic-type (car key-seq)) 57))
               ;;   (mon-string-to-symbol 
               ;;  (event-basic-type (car key-seq))))
              (t (list  (car (event-modifiers (car key-seq)))
                        (event-basic-type (car key-seq)))))
      (car (event-modifiers (car key-seq))))))
;;
;;; :TEST-ME (mon-catch-meta-key) ;<- M-3 C-x C-e ;=> (meta 51)
;;; :TEST-ME (mon-catch-meta-key t) ;<- M-3 C-x C-e ;=>"meta-3"
;;; :TEST-ME (let ((event (mon-catch-meta-key)))
;;;               (when (listp event)(cadr event)))

;;; ==============================
;;; :CHANGESET 1898
;;; :CREATED <Timestamp: #{2010-06-18T15:18:46-04:00Z}#{10245} - by MON KEY>
(defun mon-abort-recursive-edit ()
  "Try to exit gracefully from hung/corrupted `recursive-edit' minibuffer.\n
Repeatedly invoke `exit-recursive-edit' and `abort-recursive-edit' when
`recursion-depth' is greater than 0.\n
:SEE-ALSO `mon-abort-autosave-when-fucked', `exit-recursive-edit',
`abort-recursive-edit', `command-error-function', `throw-on-input',
`top-level', `kill-this-buffer', `mon-help-key-functions'.\n►►►"
  (interactive)
  (while (> (recursion-depth) 0)
    (progn (abort-recursive-edit)
           (exit-recursive-edit)
           (top-level))))

;;; ==============================
;;; :PREFIX "maawf-"
;;; :CHANGESET 2087
;;; :CREATED <Timestamp: #{2010-08-25T18:12:21-04:00Z}#{10343} - by MON KEY>
(defun mon-abort-autosave-when-fucked (&optional fucked-minibuffer-count)
  "HELP! Getme the fuck out of autosave hell.\n
Optional arg FUCKED-MINIBUFFER-COUNT is a positive integer specifying the
maximum number of screwed up minibuffers e.g. those named \" *Minibuf-<N>*\"
which need to be destroyed indiscriminately. Default is 300.\n
This happens when visiting a file encoded with raw-bytes and `auto-save-default'
is non-nil; sometimes when we accidently C-g to escape the autosave prompt: the
entire window/buffer/minibuffer stack gets corrupted such that every subsequent
C-g generates a new minibuffer which prompts: 
 \"Select coding system (default raw-text):\"
and with each one looking for some non-existent temporary buffer to do their
work in, e.g. the \" *Format Temp %<N>*\" buffer created by
`format-annotate-function' which can sometimes cause something else to trigger a
message about a missing \"*Warnings*\" buffer most likely as per the internal
function `coding-system-require-warning' and her compatriots
`select-safe-coding-system-function', `select-safe-coding-system',
`coding-system-for-write', and `universal-coding-system-argument'.\n
Let binds the following auto-save-* variables to their non-middling states:\n
 `auto-save-interval' 0
 `auto-save-timeout'  0 
 `auto-save-default' nil\n
Return value is a `yes-or-no-p' prompt which reinstates a clean minibuffer.\n
:NOTE Emcas-devels when you allow creation of non completion accessible
whitespace prefixed mini-buffers you should make _DAMN_ sure that calling
functions don't implode! I loathe this practice of hiding buffers from the user.
My Emacs, my buffers!
:SEE-ALSO `mon-abort-recursive-edit', `exit-recursive-edit',
`abort-recursive-edit', `kill-this-buffer', `command-error-function',
`throw-on-input', `top-level', `mon-help-key-functions'.\n►►►"
  (interactive)
  (let ((auto-save-interval 0)
        (auto-save-timeout  0)
        (auto-save-default nil))
    (dolist (maawf-D-1 (number-sequence 0 (or fucked-minibuffer-count 300)) (top-level))
      (let ((maawf-kill-MB (get-buffer (format " *Minibuf-%d*" maawf-D-1))))
        (when maawf-kill-MB
          (with-current-buffer maawf-kill-MB
            (kill-this-buffer)))))
    (yes-or-no-p 
     (concat ":FUNCTION `mon-abort-autosave-when-fucked' "
             "-- my work is done here, are you glad to have your minibuffer back: "))))

;;; ==============================
;;; :CREATED <Timestamp: #{2010-06-22T16:22:02-04:00Z}#{10252} - by MON>
(defvar *mon-ascii-cursor-state* nil
  "Variable to hold state for `mon-rotate-ascii-cursor'.\n
Its value is set during execution by that functions ROTATE-PRED arg.
:SEE-ALSO .\n►►►")

;;; ==============================
;;; :TODO This needs to be re-written as a macro in the style of `with-temp-message'
;;; :CREATED <Timestamp: #{2010-06-18T13:17:42-04:00Z}#{10245} - by MON>
(defun mon-rotate-ascii-cursor (rotate-pred &optional rotate-message)
  "Spin an ASCII cursor while ROTATE-PRED evaluates t.
ROTATE-PRED is a function which modifies the value of the global variable
`*mon-ascii-cursor-state*' after each full rotation of the cursor.  On entry to this
function the the value of `*mon-ascii-cursor-state*' is nil. On exit it is set to
nil. This function will signal an error if `*mon-ascii-cursor-state*' is void.\n
Optional arg is a string to display before while cursor while it is spinning.
Default is \"processing .... \"
:EXAMPLE\n\n\(progn
  \(setq *mon-ascii-cursor-state* 8\)
  \(mon-rotate-ascii-cursor 
   #'\(lambda \(\) 
       \(if \(= *mon-ascii-cursor-state* 0\) 
           \(setq *mon-ascii-cursor-state* nil\)
         \(setq *mon-ascii-cursor-state* \(1- *mon-ascii-cursor-state*\)\)\)\)
   \"running `mon-rotate-ascii-cursor' example ... \"\)
  \(message \(format \":VARIABLE `*mon-ascii-cursor-state*' is: %S\"
                   \(symbol-value *mon-ascii-cursor-state*\)\)\)\)\n
:SEE-ALSO `dotimes-with-progress-reporter', `progress-reporter-done',
`progress-reporter-update', `make-progress-reporter',
`progress-reporter-force-update', `progress-reporter-do-update'
`url-display-percentage', `url-show-status'.\n►►►"
  (when (null *mon-ascii-cursor-state*)
    (error (concat ":FUNCTION `mon-rotate-ascii-cursor' "
                   "-- global variabe `*mon-ascii-cursor-state*' null")))
  (unwind-protect 
      (let ((mrac-msg (or rotate-message "processing .... ")))
        (while *mon-ascii-cursor-state*
          (dolist (rot '(92 45 124 47 45))
            (message (concat mrac-msg (char-to-string rot)))
            (sit-for 0.1))
          (funcall rotate-pred)))
    (setq *mon-ascii-cursor-state* nil)))
;;
;;,---- :UNCOMMENT-TO-TEST
;;| (progn
;;|   (setq *mon-ascii-cursor-state* 8)
;;|   (mon-rotate-ascii-cursor 
;;|    #'(lambda () 
;;|        (if (= *mon-ascii-cursor-state* 0) 
;;|            (setq *mon-ascii-cursor-state* nil)
;;|          (setq *mon-ascii-cursor-state* (1- *mon-ascii-cursor-state*))))
;;|    "running `mon-rotate-ascii-cursor' example ... ")
;;|   (message (format ":VARIABLE `*mon-ascii-cursor-state*' is: %S"
;;|                    (symbol-value *mon-ascii-cursor-state*))))
;;`----

;;; ==============================
;;; :CREATED <Timestamp: Friday March 20, 2009 @ 09:17.35 PM - by MON KEY>
(defun mon-kill-appending (beg end)
  "Append the region current to the kill ring without killing it.\n
Like `append-next-kill' but skips the C M-w M-w finger-chord hoop jump.
:ALIASED-BY `mon-append-next-kill'\n
:SEE-ALSO `mon-append-to-buffer', `mon-append-to-register'.\n►►►"
  (interactive "r")
  (progn 
    (append-next-kill)
    (kill-ring-save beg end)))

;;; ==============================
;;; :PREFIX "matr-"
;;; :COURTESY Thierry Volpiatto :HIS tv-utils.el :WAS `tv-append-to-register'
;;; :CREATED <Timestamp: Tuesday June 16, 2009 @ 07:09.33 PM - by MON KEY>
(defun mon-append-to-register (register start end &optional w-region-deleted)
  "Append region to text in register REGISTER.\n
When non-nil prefix arg W-REGION-DELETED will delete region as well.
Called programaticaly, takes four args: REGISTER, START, END and W-REGION-DELETED.
START and END are buffer positions indicating what to append.\n
Redefines `append-to-register' with a \"\n\".\n
:ALIASED-BY `mon-region-append-to-register'
:SEE-ALSO `mon-append-to-buffer', `mon-kill-appending', `mon-append-to-register'.\n►►►"
  (interactive "cAppend to register: \nr\nP")
  (let ((matr-reg (get-register register))
        (matr-text (filter-buffer-substring start end)))
    (set-register
     register (cond ((not matr-reg) matr-text)
                    ((stringp matr-reg) (concat matr-reg "\n" matr-text))
                    ;;(t (error "Register does not contain text")))))
                    (t (error (concat ":FUNCTION `mon-append-to-register' "
                                      "-- REGISTER does not contain text"))))))
  (if w-region-deleted (delete-region start end)))

;;; ==============================
(provide 'mon-event-utils)
;;; ==============================

 
;; Local Variables:
;; mode: EMACS-LISP
;; coding: utf-8
;; generated-autoload-file: "./mon-loaddefs.el"
;; End:

;;; ====================================================================
;;; mon-event-utils.el ends here
;;; EOF
