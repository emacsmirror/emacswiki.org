;;; apu.el --- Apropos Unicode characters.
;;
;; Filename: apu.el
;; Description: Apropos Unicode characters.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2015, Drew Adams, all rights reserved.
;; Created: Thu May  7 14:08:38 2015 (-0700)
;; Version: 0
;; Package-Requires: ()
;; Last-Updated: Sun May 10 14:53:36 2015 (-0700)
;;           By: dradams
;;     Update #: 288
;; URL: http://www.emacswiki.org/apu.el
;; Doc URL: http://www.emacswiki.org/AproposUnicode
;; Other URL: http://en.wikipedia.org/wiki/The_World_of_Apu ;-)
;; Keywords: unicode, characters, encoding, commands, ucs-names
;; Compatibility: GNU Emacs: 23.x, 24.x, 25.x
;;
;; Features that might be required by this library:
;;
;;   `button', `cl', `cl-lib', `descr-text', `descr-text+', `gv',
;;   `help-fns', `help-fns+', `help-mode', `info', `macroexp',
;;   `naked', `wid-edit', `wid-edit+'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Apropos Unicode characters.
;;
;;  Command `apropos-unicode' shows you the Unicode characters that
;;  match an apropos pattern you specify: a regexp or a
;;  space-separated list of words.  The characters whose names match
;;  are shown in a help buffer, along with the names and code points
;;  (decimal and hex).
;;
;;  In the help buffer, you can use these keys to act on the character
;;  described on the current line:
;;
;;   * `RET' or `mouse-2' - see detailed information about it.
;;   * `i' - google for more information about it.
;;   * `^' - insert it in the buffer where you invoked
;;           `apropos-unicode'.
;;   * `c' - define a command to insert the character, having the same
;;           name.  (You need library `ucs-cmds.el' for this.)
;;   * `k' - globally bind a key to insert it.
;;   * `l' - locally bind a key to insert it.
;;   * `M-w' - copy it to the `kill-ring'.
;;   * `M-y' - copy it to the secondary selection.
;;
;;
;;  Commands defined here:
;;
;;    `apropos-unicode', `apu-char-codepoint-at-point',
;;    `apu-char-name-at-point', `apu-chars',
;;    `apu-copy-char-at-point-as-kill', `apu-copy-char-here-as-kill',
;;    `apu-copy-char-at-point-to-second-sel',
;;    `apu-copy-char-here-to-second-sel', `apu-define-insert-command',
;;    `apu-global-set-insertion-key', `apu-google-char',
;;    `apu-local-set-insertion-key', `apu-mode',
;;    `apu-show-char-details', `apu-zoom-char-here',
;;    `apu-zoom-char-at-point'.
;;
;;  User options defined here:
;;
;;    `apu-match-word-pairs-only-flag'.
;;
;;  Non-interactive functions defined here:
;;
;;    `apu-char-at-point', `apu-char-here', `apu-char-name-here',
;;    `apu-char-string-here', `apu-copy-char-to-second-sel',
;;    `apu-match' `apu-remove-if-not'.
;;
;;  Internal variables defined here:
;;
;;    `apu-insertion-buffer', `apu-orig-buffer'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2015/05/10 dadams
;;     Added: apu-zoom-char-here, apu-zoom-char-at-point.  Bound apu-zoom-char-here to z.
;; 2015/05/09 dadams
;;     Added: apu-match, apu-match-word-pairs-only-flag, defgroup, and apu-delete-if-not.
;;     apu-chars: Respect apu-match-word-pairs-only-flag: Match all words by default.
;; 2015/05/08 dadams
;;     Created.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'descr-text+ nil t) ; Soft-requires `help-fns+.el'. Help on `keymap' prop for `C-u C-x ='.

;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup apu nil
  "Apropos Unicode characters."
  :prefix "apu-"
  :group 'i18n :group 'help :group 'matching :group 'editing :group 'convenience
  :link `(url-link :tag "Send Bug Report"
                   ,(concat "mailto:" "drew.adams" "@" "oracle" ".com?subject=\
apu.el bug: \
&body=Describe bug here, starting with `emacs -Q'.  \
Don't forget to mention your Emacs and library versions."))
  :link '(url-link :tag "Download" "http://www.emacswiki.org/apu.el")
  :link '(url-link :tag "Description" "http://www.emacswiki.org/AproposUnicode")
  :link '(emacs-commentary-link :tag "Commentary" "apu"))

(defcustom apu-match-word-pairs-only-flag nil
  "Non-nil means match a word-list pattern like ordinary `apropos' commands.
A nil value requires each word in the list to be matched.  A non-nil
value does not require each word to be matched.  It requires only each
pair of words in the list to be matched.

For example, if non-nil then instead of matching each of the words
`greek', `small', and `letter' in any order, it actually matches each
pair of these words (in both pair orders), so you get some results
that match only two of the three words.  This is probably not what you
want in most cases, so nil is the default value."
  :type 'boolean :group 'apu)

(defvar apu-orig-buffer nil
  "Buffer current when `apu-chars' was last invoked.")

(defvar apu-insertion-buffer nil
  "Buffer current when `apu-chars' was invoked to produce this output.")
(make-variable-buffer-local 'apu-insertion-buffer)

;; Same as `icicle-remove-if-not' etc.
(defun apu-remove-if-not (pred xs)
  "A copy of list XS with only elements that satisfy predicate PRED."
  (let ((result  ())) (dolist (x xs) (when (funcall pred x) (push x result))) (nreverse result)))

(defun apu-delete-if-not (predicate xs)
  "Remove all elements of list XS that satisfy PREDICATE.
This operation is destructive, reusing conses of XS whenever possible."
  (while (and xs  (not (funcall predicate (car xs))))
    (setq xs  (cdr xs)))
  (let ((cl-p  xs))
    (while (cdr cl-p)
      (if (not (funcall predicate (cadr cl-p))) (setcdr cl-p (cddr cl-p)) (setq cl-p  (cdr cl-p)))))
  xs)

(defun apu-match (pattern char+code)
  "Return non-nil if regexp PATTERN matches the car of cons CHAR+CODE."
  (let ((case-fold-search  t))
    (ignore-errors (string-match-p pattern (car char+code)))))

(defun apu-char-here ()
  "Return the Unicode character described on this line."
  (string-to-char (apu-char-string-here)))

;;;###autoload
(defun apu-char-name-at-point (&optional position msgp) ; Not bound.
  "Return the name of the Unicode character at point, or nil if none.
Non-nil POSITION means use the character at POSITION."
  (interactive "d\np")
  (apu-char-at-point 'name position msgp))

;;;###autoload
(defun apu-char-codepoint-at-point (&optional position msgp) ; Not bound.
  "Return the codepoint of the Unicode char at point, or nil if none.
Non-nil POSITION means use the character at POSITION."
  (interactive "d\np")
  (apu-char-at-point 'code position msgp))
  
(defun apu-char-at-point (return-type position msgp)
  "Return the name or codepoint of the Unicode char at point."
  (let* ((name+code  (rassq (char-after position) (ucs-names)))
         (name       (car name+code))
         (code       (cdr name+code)))
    (unless name (error "No Unicode char here"))
    (prog1 (if (eq return-type 'name) name code)
      (when msgp (message "Char: `%s', Codepoint: `%d' (`%#x')" name code code)))))

(defun apu-char-name-here ()
  "Return the name of the Unicode char described on this line, as a string."
  (car (rassq (apu-char-here) (ucs-names))))

(defun apu-char-string-here ()
  "Return the Unicode character described on this line, as a string."
  (buffer-substring (line-beginning-position) (1+ (line-beginning-position))))

;;;###autoload
(defun apu-copy-char-at-point-as-kill (&optional msgp) ; Not bound.
  "Copy the character at point to the `kill-ring'."
  (interactive "p")
  (let ((strg  (string (char-after))))
    (kill-new strg)
    (when msgp (message "Copied char `%s' to kill ring" strg))))

(defun apu-copy-char-here-as-kill (&optional msgp)
  "Copy the Unicode character described on this line to the `kill-ring'."
  (interactive "p")
  (let ((strg  (string (apu-char-here))))
    (kill-new strg)
    (when msgp (message "Copied char `%s' to kill ring" strg))))

;;;###autoload
(defun apu-copy-char-at-point-to-second-sel (&optional msgp) ; Not bound.
  "Copy the character at point to the secondary selection.
If you have library `second-sel.el' then also copy it to the
`secondary-selection-ring'."
  (interactive "p")
  (apu-copy-char-to-second-sel (point) msgp))

(defun apu-copy-char-here-to-second-sel (&optional msgp)
  "Copy Unicode char described on this line to the secondary selection.
If you have library `second-sel.el' then also copy it to the
`secondary-selection-ring'."
  (interactive "p")
  (apu-copy-char-to-second-sel (line-beginning-position) msgp))

(defun apu-copy-char-to-second-sel (position msgp)
  "Copy char at POSITION in current buffer to secondary selection.
If you have library `second-sel.el' then this also copies it to the
`secondary-selection-ring'."
  (let* ((char  (char-after position))
         (strg  (string char)))
    (x-set-selection 'SECONDARY strg)
    (if mouse-secondary-overlay
        (move-overlay mouse-secondary-overlay position (1+ position) (current-buffer))
      (setq mouse-secondary-overlay  (make-overlay position (1+ position) (current-buffer)))
      (overlay-put mouse-secondary-overlay 'face 'secondary-selection))
    (when (require 'second-sel nil t) (add-secondary-to-ring strg))
    (when msgp (message "Copied char `%s' to secondary selection ring" strg))))

(defun apu-define-insert-command ()
  "Define a command that inserts the character described on this line.
The command name is the lowercase Unicode character name, with spaces
 replaced by hyphens.
This command requires library `ucs-cmds.el'."
  (interactive)
  (unless (require 'ucs-cmds nil t) (error "This command requires library `ucs-cmds.el'"))
  (ucsc-define-char-insert-cmd (apu-char-here) 'MSG))

(defun apu-global-set-insertion-key (key &optional msgp)
  "Globally bind a key to insert the character described on this line."
  (interactive "KKey to bind globally: \np")
  (let ((char  (apu-char-string-here)))
    (global-set-key key char)
    (when msgp (message "`%s' will now insert `%s' globally" (key-description key) char))))

(defun apu-google-char (&optional msgp)
  "Google the Unicode character described on this line."
  (interactive "p")
  (browse-url (format "https://www.google.com/search?ion=1&q=%s"
                      (url-hexify-string (concat "UNICODE " (apu-char-name-here))))))

(defun apu-insert-char (buffer &optional msgp)
  "Insert the Unicode character described on this line at point in BUFFER.
By default, BUFFER is the buffer that was current when
`apropos-unicode' was invoked.  With a prefix arg you are prompted for
the buffer to use instead."
  (interactive (list (if current-prefix-arg
                         (read-buffer "Insert in buffer: " apu-insertion-buffer 'REQUIRE-MATCH)
                       apu-insertion-buffer)
                     t))
  (let ((char  (apu-char-string-here)))
    (with-current-buffer buffer (insert char))
    (when msgp (message "Inserted `%s' in buffer `%s'" char (buffer-name buffer)))))

(defun apu-local-set-insertion-key (key &optional msgp)
  "Locally bind a key to insert the character described on this line."
  (interactive "KKey to bind locally: \np")
  (let ((char  (apu-char-string-here)))
    (local-set-key key char)
    (when msgp (message "`%s' will now insert `%s' locally" (key-description key) char))))

(defun apu-show-char-details (&optional event)
  "Show details about Unicode character of current line."
  (interactive (list last-nonmenu-event))
  (run-hooks 'mouse-leave-buffer-hook)
  (with-current-buffer (window-buffer (posn-window (event-start event)))
    (goto-char (posn-point (event-start event)))
    (save-excursion (goto-char (line-beginning-position)) (what-cursor-position t))))

(defun apu-zoom-char-here (&optional height)
  "Show the char described on the current line in a zoomed tooltip.
With a numerical prefix arg, show it that many times larger."
  (interactive (list (and current-prefix-arg  (prefix-numeric-value current-prefix-arg))))
  (unless height (setq height  1))
  (x-show-tip (propertize (apu-char-string-here)
                          'face `(:foreground "red" :height ,(* 200 height)))))

;;;###autoload
(defun apu-zoom-char-at-point (&optional height position)
  "Show the Unicode char at point in a zoomed tooltip.
With a numerical prefix arg, show it that many times larger.
Non-nil POSITION means use the character at POSITION."
  (interactive (list (and current-prefix-arg  (prefix-numeric-value current-prefix-arg))))
  (unless height (setq height  1))
  (x-show-tip (propertize (char-to-string (char-after position))
                          'face `(:foreground "red" :height ,(* 200 height)))))

;;;###autoload
(defalias 'apropos-unicode 'apu-chars)
;;;###autoload
(defun apu-chars (pattern)
  "Show all Unicode chars whose names match PATTERN.
PATTERN is as for command `apropos': a word, a list of words
 \(separated by spaces), or a regexp (using some regexp special
 characters).  If it is a word, search for matches for that word as a
 substring.  If it is a list of words, search for matches for any two
 \(or more) of those words.

The output is in `apu-mode'.

Option `apu-match-word-pairs-only-flag' determines whether each word
in a word-list pattern is required to be matched or only each possible
pairs of words are required to be matched.  A nil value means the
former and is the default.  A non-nil value gives the same behavior as
the ordinary `apropos' commands.

For example, if non-nil then instead of matching each of the words
`greek', `small', and `letter' in any order, it actually matches each
pair of these words (in both pair orders), so you get some results
that match only two of the three words.  This is probably not what you
want in most cases.

Simple tips for matching some common Unicode character names:
* You can match chars that have a given base char, such as `e', by
  using a regexp ` \(BASE-CHAR \|$\)'.  That matches BASE-CHAR after a
  `SPC' char and before a `SPC' char or at the end of the line.
* You can use `small letter' to match lowercase letters, and `capital
  letter' to match capital letters.  Just `small' matches lots of
  chars that are not letters.  Just `capital' matches chars that
  include capital letters that serve as math symbols and such."
  (interactive
   (list (let ((pat  (read-string "Search for Unicode char (word list or regexp): ")))
           (if (string-equal (regexp-quote pat) pat)
               (or (split-string pat "[ \t]+" t)  (user-error "No word list given"))
             pat))))
  (message "Matching `%s'..." pattern)
  (ucs-names)
  (setq apu-orig-buffer  (current-buffer))
  (let* ((case-fold-search  t)
         ;; Use `apropos-parse-pattern' if a regexp or `apu-match-word-pairs-only-flag' is non-nil.
         (chars+codes       (cond ((or (atom pattern)  apu-match-word-pairs-only-flag)
                                   (require 'apropos) ; `apropos-parse-pattern', `apropos-regexp'.
                                   (apropos-parse-pattern pattern)
                                   (apu-remove-if-not (apply-partially #'apu-match apropos-regexp)
                                                      ucs-names))
                                  (t
                                   (let ((chs+cds  ucs-names))
                                     (dolist (word  pattern)
                                       (setq chs+cds
                                             (apu-delete-if-not (apply-partially #'apu-match word)
                                                                chs+cds)))
                                     chs+cds))))
         (max-char          0)
         (bufname           (format "*`%s' Matching Unicode Chars*" pattern)))
    (unless chars+codes (error "No matching characters"))
    (message "Matching `%s'...done" pattern)
    (dolist (char+code  chars+codes) (setq max-char  (max max-char (string-width (car char+code)))))
    (with-help-window bufname
      (with-current-buffer bufname
        (goto-char (point-min))
        (dolist (char+code  chars+codes)
          (insert (format "%c %s" (cdr char+code) (car char+code)))
          (insert (propertize "\t" 'display `(space :align-to ,(+ 3 max-char))) " ")
          (insert (format "%6d\t%#8x\n" (cdr char+code) (cdr char+code)))
          (add-text-properties (line-beginning-position -1) (line-end-position -1)
                               '(mouse-face underline keymap apu-mode-map)))))
    (apu-mode)))

(define-derived-mode apu-mode special-mode "Apropos Unicode"
  "Major mode for `apropos-unicode' output.
\\{apu-mode-map}"
  (setq case-fold-search      t
        apu-insertion-buffer  apu-orig-buffer))

(when (featurep 'ucs-cmds)
  (define-key apu-mode-map "c"         'apu-define-insert-command))
(define-key apu-mode-map   "i"         'apu-google-char)
(define-key apu-mode-map   "k"         'apu-global-set-insertion-key)
(define-key apu-mode-map   "l"         'apu-local-set-insertion-key)
(define-key apu-mode-map   "z"         'apu-zoom-char-here)
(define-key apu-mode-map   "^"         'apu-insert-char)
(define-key apu-mode-map (kbd "RET")   'apu-show-char-details)
(define-key apu-mode-map [mouse-2]     'apu-show-char-details)
(define-key apu-mode-map (kbd "M-w")   'apu-copy-char-here-as-kill)
(when (featurep 'second-sel)
  (define-key apu-mode-map (kbd "M-y") 'apu-copy-char-here-to-second-sel))

;;;;;;;;;;;;;;;;;;;;;;

(provide 'apu)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; apu.el ends here
