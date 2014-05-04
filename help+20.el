;;; help+20.el --- Extensions to `help.el' for Emacs 20.
;;
;; Filename: help+20.el
;; Description: Extensions to `help.el' for Emacs 20.
;; Author: Drew Adams
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 1999-2014, Drew Adams, all rights reserved.
;; Created: Tue Mar 16 14:18:11 1999
;; Version: 0
;; Package-Requires: ()
;; Last-Updated: Sun May  4 10:27:08 2014 (-0700)
;;           By: dradams
;;     Update #: 2208
;; URL: http://www.emacswiki.org/help+20.el
;; Doc URL: http://emacswiki.org/HelpPlus
;; Keywords: help
;; Compatibility: GNU Emacs 20.x
;;
;; Features that might be required by this library:
;;
;;   `avoid', `fit-frame', `frame-fns', `info', `info+', `misc-fns',
;;   `naked', `strings', `thingatpt', `thingatpt+', `wid-edit',
;;   `wid-edit+', `widget'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Extensions to `help.el' for Emacs 20.
;;
;;  Commands defined here:
;;
;;    `describe-command', `describe-file', `describe-keymap',
;;    `describe-option', `describe-option-of-type',
;;    `help-on-click/key', `mouse-help-on-click',
;;    `mouse-help-on-mode-line-click', `pop-to-help-toggle',
;;    `view-emacs-lisp-news', `save-*Help*-buffer'.
;;
;;  Non-interactive functions defined here:
;;
;;    `help-custom-type', `help-on-click/key-lookup',
;;    `help-remove-duplicates', `help-value-satisfies-type-p',
;;    `help-var-inherits-type-p', `help-var-is-of-type-p',
;;    `help-var-matches-type-p', `help-var-val-satisfies-type-p',
;;    `remove-help-window'.
;;
;;  Internal variables defined here:
;;
;;    `help-origin-buffer'.
;;
;;
;;  ***** NOTE: The following functions defined in `help.el' have
;;              been REDEFINED HERE:
;;
;;  `describe-function', `describe-key', `describe-mode',
;;  `describe-project', `describe-variable', `help-mode',
;;  `help-with-tutorial', `locate-library', `view-emacs-FAQ',
;;  `view-emacs-news', `where-is'.
;;
;;
;;  ***** NOTE: The doc string for `help-for-help' has been
;;              REDEFINED HERE
;;              (see `make-help-screen help-for-help')
;;
;;  The following bindings are made here:
;;
;;    `q'          `View-quit' (in `help-mode')
;;    `C-h c'      `describe-command' (replaces `describe-key-briefly')
;;    `C-h o'      `describe-option'
;;    `C-h u'      `manual-entry'
;;    `C-h C-a'    `apropos'
;;    `C-h C-c'    `describe-key-briefly' (replaces `describe-copying')
;;    `C-h C-l'    `locate-library'
;;    `C-h C-n'    `view-emacs-lisp-news'
;;    `C-h C-o'    `describe-option-of-type'
;;    `C-h C-s'    `save-*Help*-buffer'
;;    `C-h RET'    `help-on-click/key'
;;    `C-h M-a'    `apropos-documentation'
;;    `C-h M-c'    `describe-copying' (replaces `C-h C-c')
;;    `C-h M-f'    `describe-file'
;;    `C-h M-k'    `describe-keymap'
;;    `C-h M-o'    `pop-to-help-toggle'
;;    `C-h M-C-a'  `tags-apropos'
;;    [mouse-1]    `mouse-help-on-click' (non-mode-line)
;;    [mouse-1]    `mouse-help-on-mode-line-click' (mode-line)
;;
;;  Suggested additional binding:
;;
;;   (global-set-key [f1] 'help-on-click/key)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2014/05/04 dadams
;;     Soft-require info+20.el (new) instead of info+.el.
;; 2012/09/24 dadams
;;     describe-file: Added optional arg NO-ERROR-P.
;; 2012/08/21 dadams
;;     Call tap-put-thing-at-point-props after load thingatpt+.el.
;; 2012/08/18 dadams
;;     Invoke tap-define-aliases-wo-prefix if thingatpt+.el is loaded.
;;     help-on-click/key: Use tap-symbol-at-point, not symbol-at-point, if defined.
;; 2012/01/11 dadams
;;     describe-variable: Remove * from beginning of doc string.
;; 2011/12/19 dadams
;;     help-with-tutorial, describe-variable: Use line-end-position, not end-of-line + point.
;;     describe-variable: if -> when.
;; 2011/10/07 dadams
;;     Added soft require of naked.el.
;;     describe-key, where-is, help-on-click/key-lookup: Use naked-key-description if available.
;; 2011/04/25 dadams
;;     describe-file: Incorporate autofile bookmark description.  Added optional arg.
;; 2011/03/31 dadams
;;     help-var-(matches|inherits)-type-p: Wrap string-match with save-match-data.
;; 2011/01/04 dadams
;;     Removed autoload cookies from non-interactive function and define-key.
;; 2009/08/30 dadams
;;     describe-keymap: Don't print nil if the map has no doc.
;; 2008/08/19 dadams
;;     describe-keymap: Use insert instead of princ for map part.  Thx to Chong Yidong.
;; 2008/05/20 dadams
;;     describe-function: Different prompt if prefix arg.
;; 2008/03/02 dadams
;;     Moved describe-file here from misc-cmds.el.  Bound to C-h M-f.
;; 2008/01/17 dadams
;;     Removed soft require of icicles.el
;; 2007/12/25 dadams
;;     help-var-inherits-type-p:
;;       Recheck var-type match after set var-type to its car.
;;       Handle string (regexp) TYPES elements.
;;     help-value-satisfies-type-p: Skip type check for string type (regexp).
;;     help-var-is-of-type-p: Doc string.  Use help-var-matches-type-p.
;;     Added: help-var-matches-type-p.
;; 2007/12/24 dadams
;;     help-var-inherits-type-p: Recheck type match after set var-type to its car.
;;     Added: help-custom-type.
;; 2007/12/23 dadams
;;     help-var-is-of-type-p:
;;       Added MODE arg.  Use help-var-inherits-type-p, help-var-val-satisfies-type-p.
;;       Redefined as MODE choice, not just a simple or.  Treat more cases.
;;     Added: help-var-inherits-type-p, help-var-val-satisfies-type-p,
;;            help-value-satisfies-type-p.
;;     describe-option-of-type: Prefix arg means use mode inherit-or-value.
;; 2007/12/22 dadams
;;     help-var-is-of-type-p:
;;       Check supertypes also.  Use both :validate and :match.
;;       Wrap type check in condition-case. Use widget-put instead of plist-put.
;;     Added soft require of wid-edit+.el.
;; 2007/12/21 dadams
;;     help-var-is-of-type-p: Use :validate, not :match, for the test.
;; 2007/12/20 dadams
;;     Swapped C-o and M-o bindings.
;; 2007/12/15 dadams
;;     Bound C-h c to describe-command and C-h C-c to describe-key-briefly.
;; 2007/12/14 dadams
;;     Renamed library from help+.el to help+20.el.  New help+.el is for Emacs 22+.
;;     Removed commented Emacs 19 stuff.
;;     view-emacs-lisp-news, describe-(key|function|variable):
;;       Removed emacs-major-version test.
;;     Added: Redefinition of help-mode.
;;     Bound q in help-mode to View-quit.
;;     Don't require cl.el if before Emacs 20.
;; 2007/12/13 dadams
;;     help-on-click/key: Removed extra arg in call to message.
;;     help-for-help: Reordered help string.
;; 2007/12/09 dadams
;;     (make-help-screen help-for-help...): Rewrote help text.
;; 2007/12/07 dadams
;;     describe-variable: if OPTIONP, then allow custom-variable-p as well as user-variable-p.
;;     describe-option-of-type: Use "nil" as default value.
;; 2007/12/06 dadams
;;     describe-option-of-type:
;;       If nil type, all defcustom vars are candidates.  Use custom-variable-p, if available.
;;       Specific error if no such custom type.
;;     describe-option: Use both custom-variable-p and user-variable-p.
;; 2007/12/04 dadams
;;     Added: describe-option-of-type, help-remove-duplicates, help-var-is-of-type-p.
;;     Bound o to describe-option (no longer edit-options), M-o to describe-option-of-type,
;;       C-c to describe-command, M-c to describe-copying.
;; 2007/11/28 dadams
;;     Renamed describe-bindings-in-map to describe-keymap.  Added keymap's doc string.
;; 2007/11/27 dadams
;;     locate-library: Use icicle-read-string-completing, if available.
;;     Soft require Icicles.
;; 2007/11/22 dadams
;;     Added: describe-bindings-in-map.  Bound to C-h M-k.
;; 2007/10/17 dadams
;;     describe-(function|variable): Prefix arg means describe only commands or user options.
;;     Added: describe-(command|option).
;; 2007/09/04 dadams
;;     remove-windows-on -> delete-windows-on.  Removed require of frame-cmds.el.
;; 2006/12/08 dadams
;;     describe-variable: Fixed interactive case when symbol-nearest-point is not defined.
;; 2006/07/11 dadams
;;     Added: help-origin-buffer, pop-to-help-toggle.  Bound latter to C-h C-o globally
;; 2005/10/31 dadams
;;     Use nil as init-value arg in calls to completing-read, everywhere.
;; 2001/01/10 dadams
;;     Protected remove-windows-on via fboundp.
;; 1999/08/12 dadams
;;     Added doc strings for help-on-click/key(-lookup) and updated header.
;; 1999/04/09 dadams
;;     help-on-click/key: Treat mouse click on symbol via apropos.
;; 1999/04/08 dadams
;;     help-on-click/key: 1. Treat mouse menus.
;;                        2. Corrected: flush extra mode-line mouse events.
;; 1999/04/08 dadams
;;     help-on-click/key: Bound temp-buffer-show-function so use other win.
;; 1999/04/08 dadams
;;     1. Added binding: help-on-click/key.
;;     2. Added: make-help-screen help-for-help.
;;     3. help-on-click/key-lookup: show-*Help*-buffer.
;;     4. help-on-click/key: Prompt.  Corrected: event->key, show-*Help*-buffer.
;; 1999/04/07 dadams
;;     1. Added: (replacement) describe-key.
;;     2. Added: help-on-click/key-lookup, help-on-click/key.
;; 1999/04/06 dadams
;;     Added binding for save-*Help*-buffer.
;; 1999/04/06 dadams
;;     1. Added some key bindings: o, u, C-l, C-a, M-a, C-M-a.
;;     2. Added: save-*Help*-buffer.
;; 1999/03/31 dadams
;;     Protected symbol-nearest-point with fboundp.
;; 1999/03/17 dadams
;;     1. Added: remove-help-window, help-with-tutorial, describe-project,
;;        view-emacs-FAQ, view-emacs-news, describe-function, describe-variable,
;;        where-is, locate-library, view-emacs-lisp-news.
;;     2. help-iso-prefix: Treat unbound iso-transl-char-map error.  Removed
;;        highlighting.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'info nil t) ;; (no error if not found):
                      ;; Info-exit, Info-goto-node, Info-goto-emacs-key-command-node
(require 'info+20 nil t) ;; (no error if not found):
                         ;; Info-goto-emacs-key-command-node (returns found-p)
(require 'thingatpt nil t) ;; (no error if not found): symbol-at-point

(when (and (require 'thingatpt+ nil t);; (no error if not found)
           (fboundp 'tap-put-thing-at-point-props)) ; >= 2012-08-21
  (tap-define-aliases-wo-prefix)
  (tap-put-thing-at-point-props))
  ;;  symbol-nearest-point, tap-symbol-at-point

(require 'frame-fns nil t) ;; (no error if not found): 1-window-frames-on
(require 'wid-edit+ nil t) ;; (no error if not found):
                           ;; redefined color widget (for help-var-is-of-type-p)
(require 'naked nil t) ;; (no error if not found): naked-key-description

;; Get macro `make-help-screen' when this is compiled,
;; or run interpreted, but not when the compiled code is loaded.
(eval-when-compile
  (require 'help-macro nil t) ;; (no error if not found) make-help-screen
  (require 'help-macro+ nil t)) ;; (no error if not found): make-help-screen
;; (require 'icicles nil t) ;; (no error if not found): icicle-read-string-completing


;; Quiet the byte-compiler.
(defvar view-no-disable-on-exit)
(defvar view-exit-action)

;;;;;;;;;;;;;;;;;;;;

(defvar help-origin-buffer nil "Buffer that we left, to go to *Help*.")

(define-key help-map "c" 'describe-command)
(define-key help-map "o" 'describe-option)
(define-key help-map "u" 'manual-entry) ; in `man.el'
(define-key help-map "\C-a" 'apropos)
(define-key help-map "\C-c" 'describe-key-briefly)
(define-key help-map "\C-l" 'locate-library)
(define-key help-map [?\C-m] 'help-on-click/key) ; RET
(define-key help-map [?\C-n] 'view-emacs-lisp-news)
(define-key help-map "\C-o" 'describe-option-of-type)
(define-key help-map "\C-s" 'save-*Help*-buffer)
(define-key help-map "\M-a" 'apropos-documentation)
(define-key help-map "\M-c" 'describe-copying)
(define-key help-map "\M-f" 'describe-file)
(define-key help-map "\M-k" 'describe-keymap)
(define-key help-map "\M-o" 'pop-to-help-toggle)
(define-key help-map "\M-\C-a" 'tags-apropos)
(define-key help-map [down-mouse-1] 'mouse-help-on-click)
(define-key help-map [mode-line down-mouse-1] 'mouse-help-on-mode-line-click)

;; `help-mode' too needs a quit key.
(define-key help-mode-map "q" 'View-quit)


(defsubst remove-help-window ()
  "If called from `help-for-help', remove display of help window."
  (when (eq 'help-for-help this-command) (delete-windows-on "*Help*")))


;; REPLACES ORIGINAL in `help.el':
;; Deletes *Help* frame when done, if `one-window-p'.
;;
;;;###autoload
(defun help-mode ()
  "Major mode for viewing help text and navigating references in it.
Entry to this mode runs the normal hook `help-mode-hook'.
Commands:
\\{help-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map help-mode-map)
  (setq mode-name   "Help"
        major-mode  'help-mode)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults  nil)        ; font-lock would defeat xref
  (view-mode)
  (make-local-variable 'view-no-disable-on-exit)
  (setq view-no-disable-on-exit  t
        view-exit-action         (lambda (buffer)
                                   (or (window-minibuffer-p (selected-window))
                                       (when (eq (window-buffer) (get-buffer "*Help*"))
                                         (if (one-window-p t)
                                             (delete-frame)
                                           (delete-window))))))
  ;; `help-make-xrefs' would be run here if not invoked from
  ;; `help-mode-maybe'.
  (run-hooks 'help-mode-hook))


;; REPLACES ORIGINAL in `help.el':
;; 1. Uses other window.
;; 2. Calls `remove-help-window'.
;; 3. Messages added.
;;;###autoload
(defun help-with-tutorial (&optional arg)
  "Select the Emacs learn-by-doing tutorial.
If there is a tutorial version written in the language
of the selected language environment, that version is used.
If there's no tutorial in that language, `TUTORIAL' is selected.
With prefix ARG, you are asked to choose which language."
  (interactive "P")
  (message "Looking for Emacs Tutorial file...")
  (let ((lang  (if arg
                   (read-language-name 'tutorial "Language: " "English")
                 (if (get-language-info current-language-environment 'tutorial)
                     current-language-environment
                   "English")))
        file filename)
    (setq filename  (get-language-info lang 'tutorial)
          file      (expand-file-name (concat "~/" filename)))
    (delete-other-windows)
    (if (get-file-buffer file)
        (switch-to-buffer-other-window (get-file-buffer file))
      (switch-to-buffer-other-window (create-file-buffer file))
      (setq buffer-file-name            file
            default-directory           (expand-file-name "~/")
            buffer-auto-save-file-name  nil)
      (insert-file-contents (expand-file-name filename data-directory))
      (goto-char (point-min))
      (search-forward "\n<<")
      (beginning-of-line)
      (delete-region (point) (line-end-position))
      (let ((n  (- (window-height (selected-window))
                   (count-lines (point-min) (point))
                   6)))
        (if (< n 12)
            (newline n)
          ;; Some people get confused by the large gap.
          (newline (/ n 2))
          (insert "[Middle of page left blank for didactic purposes.  "
                  "Text continues below]")
          (newline (- n (/ n 2)))))
      (goto-char (point-min))
      (set-buffer-modified-p nil)))
  (remove-help-window)
  (message "Looking for Emacs Tutorial file...done"))


;; REPLACES ORIGINAL in `help.el':
;; Return nil if KEY is undefined; else return t.
;;
;;;###autoload
(defun describe-key (key)
  "Describe the command that a keyboard/menu/mouse sequence invokes.
Argument KEY is a string.
Return nil if KEY is undefined; else return t."
  (interactive "kDescribe command bound to keyboard/menu/mouse sequence: ")
  (save-excursion
    (let ((modifiers  (event-modifiers (aref key 0)))
          window position)
      ;; For a mouse button event, go to the button it applies to
      ;; to get the right key bindings.  And go to the right place
      ;; in case the keymap depends on where you clicked.
      (if (or (memq 'click modifiers) (memq 'down modifiers)
              (memq 'drag modifiers))
          (setq window    (posn-window (event-start (aref key 0)))
                position  (posn-point (event-start (aref key 0)))))
      (if (windowp window)
          (progn
            (set-buffer (window-buffer window))
            (goto-char position)))
      (let ((defn  (key-binding key)))
        (cond ((or (null defn) (integerp defn))
               (message "`%s' is undefined." (if (fboundp 'naked-key-description)
                                                 (naked-key-description key)
                                               (key-description key)))
               nil)                     ; Return nil: undefined.
              (t
               (with-output-to-temp-buffer "*Help*"
                 (princ (if (fboundp 'naked-key-description)
                            (naked-key-description key)
                          (key-description key)))
                 (if (windowp window)
                     (princ " at that spot"))
                 (princ " runs the command ")
                 (prin1 defn)
                 (princ "\n   which is ")
                 (describe-function-1 defn nil (interactive-p))
                 (print-help-return-message))
               t))))))                  ; Return t: defined.


;; REPLACES ORIGINAL in `help.el':
;; 1. Uses other window.
;; 2. Calls `remove-help-window'.
;; 3. Messages.
;;
;;;###autoload
(defun describe-project ()
  "Display information on the GNU project."
  (interactive)
  (message "Looking for file describing GNU project...")
  (find-file-read-only-other-window (expand-file-name "GNU" data-directory))
  (remove-help-window)
  (message "Looking for file describing GNU project...done"))


;; REPLACES ORIGINAL in `help.el':
;; 1. Uses other window.
;; 2. Calls `remove-help-window'.
;; 3. Messages added.
;; 4. Turns off `auto-save-mode'.
;;
;;;###autoload
(defun view-emacs-news (&optional arg)
  "Display information on recent changes to Emacs.
With numeric prefix ARG, display correspondingly older changes."
  (interactive "P")
  (message "Looking for Emacs Changes file...")
  (let ((arg  (if arg (prefix-numeric-value arg) 0)))
    (find-file-read-only-other-window
     (expand-file-name (concat (make-string arg ?O) "NEWS")
                       data-directory)))
  (auto-save-mode nil)                  ; Turn it off.
  (remove-help-window)
  (message "Looking for Emacs Changes file...done"))


;; REPLACES ORIGINAL in `help.el':
;; 1. Uses other window.
;; 2. Calls `remove-help-window'.
;; 3. Turns off `auto-save-mode'.
;;
;;;###autoload
(defun view-emacs-FAQ ()
  "Display Frequently Asked Questions about Emacs (with answers)."
  (interactive)
  (message "Looking for Emacs FAQ file...")
  (find-file-read-only-other-window (expand-file-name "FAQ" data-directory))
  (auto-save-mode nil)                  ; Turn it off.
  (remove-help-window)
  (message "Looking for Emacs FAQ file...done"))


;; REPLACES ORIGINAL in `help.el':
;; Updated key bindings.
;;
(make-help-screen help-for-help
  "RET [abcCfFhiIklLmnopqstuvw] C-[\acdfiklnopsw] M-[acko] C-M-a (? for more help):"
  "This is the Emacs `help-command', accessible via `%THIS-KEY%'.
Type a Help option (below) now, for help on a particular topic.
Use \\<help-map>`\\[scroll-up]' or `\\[scroll-down]' to scroll this text.  \
Type `\\[help-quit]' to exit Help.
\(A \"command\" is any function that you can execute via `M-x'.)

LEARNING EMACS
--------------
\\[help-with-tutorial]:   Starts a tutorial for learning Emacs.
\\[view-emacs-FAQ]:   Explains frequently asked Emacs questions.

COMMONLY USED
-------------
\\[help-on-click/key]: Help about a key sequence or something you click with the mouse.
\\[apropos-command]:   Shows commands that match a regular expression (regexp).
\\[describe-bindings]:   Shows current key bindings: keyboard, menu bar, and mouse.
\\[describe-command]:   Shows the doc for an Emacs command.
\\[describe-function]:   Shows the doc for an Emacs function.
\\[info]:   Enters `Info', to browse manuals, including Emacs and Emacs Lisp.
\\[describe-key]:   Describes the command bound to keyboard/menu/mouse sequence.
\\[describe-mode]:   Describes the current major and minor modes.
\\[describe-option]:   Shows an Emacs user option's value and documentation.
\\[describe-variable]:   Shows an Emacs variable's value and documentation.
\\[Info-goto-emacs-command-node]: Opens the Emacs manual for an Emacs command.
\\[Info-goto-emacs-key-command-node]: Opens the Emacs manual for a keyboard/menu/mouse \
sequence.

MORE ADVANCED HELP
------------------
\\[view-lossage]:   Shows what you just typed (last 100 keystrokes & mouse actions).
\\[view-emacs-news]:   Describes what's new in this Emacs release.
\\[finder-by-keyword]:   Finds Emacs-Lisp libraries that match a topic.
\\[describe-syntax]:   Describes the current syntax table.
\\[manual-entry]:   Finds a topic in the Unix manual.
\\[where-is]:   Identifies a keyboard/menu/mouse sequence that invokes a command.
\\[apropos]: Shows Emacs functions and variables that match a regexp.
\\[describe-key-briefly]: Identifies the command bound to a keyboard/menu/mouse sequence.
\\[describe-distribution]: Shows Emacs ordering information.
\\[locate-library]: Shows the path name to an Emacs library.
\\[view-emacs-lisp-news]: Describes latest Emacs Lisp changes.
\\[describe-project]: Shows information about the GNU project.
\\[save-*Help*-buffer]: Renames buffer *Help* as buffer *Help*<N>.
\\[describe-no-warranty]: Shows information about the absence of a warranty.
\\[apropos-documentation]: Shows Emacs functions and variables whose doc matches a regexp.
\\[describe-copying]: Shows the GNU Emacs General Public License.
\\[pop-to-help-toggle]: Pops to Help buffer or back to the buffer that sent you to Help.
\\[tags-apropos]: Shows the tags matched by a given string.

INTERNATIONAL
-------------
\\[describe-coding-system]:   Describes a coding system.
h    Displays the HELLO file, which illustrates scripts and languages.
\\[describe-input-method]:   Describes an input method.
\\[describe-language-environment]:   Describes a language environment.
\\[info-lookup-symbol]: Finds a symbol in the manual for the current buffer's language.
"
  help-map)


(or (fboundp 'old-describe-mode)
    (fset 'old-describe-mode (symbol-function 'describe-mode)))


;; REPLACES ORIGINAL in `help.el':
;; 1. Provides message telling how to change pages in *Help* buffer.
;; 2. Doc string also explains this.
;;    Really, the text at the beginning of *Help* should explain this - TO BE DONE.
;;
;;;###autoload
(defun describe-mode ()
  "Display documentation of current major mode and minor modes.
Each mode (minor or major) is displayed on a different \"page\" in the
*Help* buffer (the pages are separated by `^L' characters).
You can change pages with `\\[forward-page]' and `\\[backward-page]'.

Note: For a minor mode to be described correctly here, the mode's
indicator variable (listed in `minor-mode-alist') must also be a
function whose documentation describes the minor mode."
  (interactive)
  (let ((font-lock-verbose  nil))       ; This should inhibit msgs, but doesn't!
    (old-describe-mode)
    (message (substitute-command-keys
              "You can use `\\[forward-page]' and `\\[backward-page]' \
in *Help* buffer to change pages."))))


;; REPLACES ORIGINAL in `help.el':
;; Preferred candidate is `symbol-nearest-point'.
;; With a prefix argument, candidates are commands only.
;;
;;;###autoload
(defun describe-function (function &optional commandp)
  "Display the full documentation of FUNCTION (a symbol).
FUNCTION names an Emacs Lisp function, possibly a user command.
With a prefix argument, candidates are commands (interactive) only.
Default candidate is: preferably the `symbol-nearest-point', or else
the innermost function call surrounding point
\(`function-called-at-point').
Return the description that was displayed, as a string."
  (interactive
   (let ((fn                            (or (and (fboundp 'symbol-nearest-point)
                                                 (symbol-nearest-point))
                                            (function-called-at-point)))
         (enable-recursive-minibuffers  t))
     (list (intern (completing-read
                    (if current-prefix-arg "Describe command: " "Describe function: ")
                    obarray (if current-prefix-arg 'commandp 'fboundp) t nil nil
                    (and fn (symbol-name fn)) t))
           current-prefix-arg)))
  (unless (or (not commandp) (commandp function))
    (error "Not a defined Emacs command (interactive function): `%s'" function))
  (unless (fboundp function)
    (error "Not a defined Emacs function: `%s'" function))
  (with-output-to-temp-buffer "*Help*"
    (prin1 function)
    ;; Use " is " instead of a colon so that
    ;; it is easier to get out the function name using forward-sexp.
    (princ " is ")
    (describe-function-1 function nil (interactive-p))
    (print-help-return-message)
    (save-excursion
      (set-buffer standard-output)
      ;; Return the text we displayed.
      (buffer-string))))

;;;###autoload
(defun describe-command (function)
  "Describe an Emacs command (interactive function).
Same as using a prefix arg with `describe-function'."
  (interactive
   (let ((fn                            (or (and (fboundp 'symbol-nearest-point)
                                                 (symbol-nearest-point))
                                            (function-called-at-point)))
         (enable-recursive-minibuffers  t))
     (list (intern (completing-read "Describe command: " obarray 'commandp
                                    t nil nil (and fn (symbol-name fn)) t)))))
  (describe-function function t))


;; REPLACES ORIGINAL in `help.el':
;;
;; 1. Preferred candidate is `symbol-nearest-point'.
;; 2. Use `substitute-command-keys' on doc string.
;; 3. Remove initial `*' from doc string (indicates it is a user variable).
;; 4. With a prefix argument, candidates are user variables (options) only.
;;
;;;###autoload
(defun describe-variable (variable &optional optionp)
  "Display the full documentation of VARIABLE (a symbol).
VARIABLE names an Emacs Lisp variable, possibly a user option.
With a prefix argument, candidates are user variables (options) only.
Default candidate is the `symbol-nearest-point'.
Return the documentation, as a string."
  (interactive
   (let ((symb                          (or (and (fboundp 'symbol-nearest-point)
                                                 (symbol-nearest-point))
                                            (and (symbolp (variable-at-point))
                                                 (variable-at-point))))
         (enable-recursive-minibuffers  t))
     (list (intern (completing-read "Describe variable: " obarray
                                    (if current-prefix-arg 'user-variable-p 'boundp)
                                    t nil nil (and symb (symbol-name symb)) t))
           current-prefix-arg)))
  (unless (or (not optionp) (or (user-variable-p variable) (custom-variable-p variable)))
    (error "Not a defined Emacs user option: `%s'" variable))
  (unless (boundp variable) (error "Not a defined Emacs variable: `%s'" variable))
  (let (valvoid)
    (with-output-to-temp-buffer "*Help*"
      (prin1 variable)
      (if (not (boundp variable))       ; This will never happen.
          (progn (princ " is void") (terpri) (setq valvoid  t))
        (princ "'s value is ")
        (terpri)
        (pp (symbol-value variable))
        (terpri))
      (when (local-variable-p variable)
        (princ (format "Local in buffer %s; " (buffer-name)))
        (if (not (default-boundp variable))
            (princ "globally void")
          (princ "global value is ")
          (terpri)
          (pp (default-value variable)))
        (terpri))
      (terpri)
      (save-current-buffer
        (set-buffer standard-output)
        (when (> (count-lines (point-min) (point-max)) 10)
          (goto-char (point-min))
          (if valvoid
              (forward-line 1)
            (forward-sexp 1)
            (delete-region (point) (line-end-position))
            (insert "'s value is shown below.\n\n")
            (save-excursion (insert "\n\nValue:")))))
      (princ "Documentation:")
      (terpri)
      (let ((doc  (documentation-property variable 'variable-documentation)))
        (if (or (null doc)  (string= "" doc))
            (princ "Not documented as a variable.")
          (when (and (> (length doc) 1)  (eq ?* (elt doc 0)))
            (setq doc  (substring doc 1))) ; Remove any user-variable prefix `*'.
          (princ (substitute-command-keys doc))))
      (help-setup-xref (list #'describe-variable variable) (interactive-p))
      ;; Make a link to customize if this variable can be customized.
      ;; Note, it is not reliable to test only for a custom-type property
      ;; because those are only present after the var's definition
      ;; has been loaded.
      (when (or (get variable 'custom-type) ; after defcustom
                (get variable 'custom-loads) ; from loaddefs.el
                (get variable 'standard-value)) ; from cus-start.el
        (let ((customize-label  "customize"))
          (terpri)
          (terpri)
          (princ (concat "You can " customize-label " this variable."))
          (with-current-buffer "*Help*"
            (save-excursion  (re-search-backward
                              (concat "\\(" customize-label "\\)") nil t)
                             (help-xref-button 1 #'(lambda (v)
                                                     (customize-variable v)) variable)))))
      ;; Make a hyperlink to the library if appropriate.  (Don't
      ;; change the format of the buffer's initial line in case
      ;; anything expects the current format.)
      (when (string< "20.5" emacs-version)
        (let ((file-name  (symbol-file variable)))
          (when file-name
            (princ "\n\nDefined in `")
            (princ file-name)
            (princ "'.")
            (with-current-buffer "*Help*"
              (save-excursion
                (re-search-backward "`\\([^`']+\\)'" nil t)
                (help-xref-button 1 (lambda (arg)
                                      (let ((location  (find-variable-noselect arg)))
                                        (pop-to-buffer (car location))
                                        (goto-char (cdr location))))
                                  variable))))))

      (print-help-return-message)
      (save-excursion (set-buffer standard-output)
                      (buffer-string))))) ; Return the text we displayed.

;;;###autoload
(defun describe-option (variable)
  "Describe an Emacs user variable (option).
Same as using a prefix arg with `describe-variable'."
  (interactive
   (let ((symb                          (or (and (fboundp 'symbol-nearest-point)
                                                 (symbol-nearest-point))
                                            (and (symbolp (variable-at-point))
                                                 (variable-at-point))))
         (enable-recursive-minibuffers  t))
     (list (intern (completing-read "Describe user option: " obarray
                                    ;; Emacs 20 `user-variable-p' does not include defcustoms.
                                    (lambda (c) (or (custom-variable-p c) (user-variable-p c)))
                                    t nil nil (and symb (symbol-name symb)) t)))))
  (describe-variable variable t))

;;;###autoload
(defun describe-option-of-type (type option)
  "Describe an Emacs user OPTION (variable) of a given `defcustom' TYPE.
A prefix argument determines the type-checking behavior:
 - None:         OPTION is defined with TYPE or a subtype of TYPE.
 - Plain `C-u':  OPTION is defined with TYPE or a subtype of TYPE,
                 or its current value is compatible with TYPE.
 - Negative:     OPTION is defined with TYPE (exact match).
 - Non-negative: OPTION is defined with TYPE (exact match),
                 or its current value is compatible with TYPE.

If TYPE is nil (default value) then *all* `defcustom' variables are
potential candidates.  That is different from using `describe-option',
because `describe-option' includes user-variable candidates not
defined with `defcustom' (with `*'-prefixed doc strings)."
  (interactive
   (let* ((symb      (or (and (fboundp 'symbol-nearest-point) (symbol-nearest-point))
                         (and (symbolp (variable-at-point)) (variable-at-point))))
          (typ       (car (condition-case err
                              (read-from-string
                               (let ((types  ()))
                                 (mapatoms
                                  (lambda (cand)
                                    (when (if (fboundp 'custom-variable-p) ; Not in Emacs 20.
                                              (custom-variable-p cand)
                                            (user-variable-p cand))
                                      (push (list
                                             (format "%s" (format "%S" (get cand 'custom-type))))
                                            types))))
                                 (completing-read "Describe option of type: "
                                                  (help-remove-duplicates types)
                                                  nil nil nil nil "nil")))
                            (end-of-file (error "No such custom type")))))
          (pref-arg  current-prefix-arg))
     (list typ (intern (completing-read
                        "Option: " obarray
                        (lambda (v)
                          (and (if (fboundp 'custom-variable-p) ; Not in vanilla Emacs 20.
                                   (custom-variable-p v)
                                 (user-variable-p v))
                               (or (not typ) ; Allow all vars if requested type = nil.
                                   (help-var-is-of-type-p
                                    v (list typ)
                                    (cond ((not pref-arg) 'inherit)
                                          ((consp pref-arg) 'inherit-or-value)
                                          ((wholenump (prefix-numeric-value pref-arg))
                                           'direct-or-value)
                                          (t 'direct))))))
                        t nil nil (and symb (symbol-name symb)) t)))))
  (describe-variable option t))

(defun help-var-is-of-type-p (variable types &optional mode)
  "Return non-nil if VARIABLE satisfies one of the custom types in TYPES.
TYPES is a list of `defcustom' type sexps or a list of regexp strings.
TYPES are matched, in order, against VARIABLE's type definition or
VARIABLE's current value, until one is satisfied or all are tried.

If TYPES is a list of regexps, then each is regexp-matched against
VARIABLE's custom type.

Otherwise, TYPES is a list of type sexps, each of which is a
definition acceptable for `defcustom' :type or the first symbol of
such a definition (e.g. `choice').  In this case, two kinds of type
comparison are possible:

1. VARIABLE's custom type, or its first symbol, is matched using
  `equal' against each type in TYPES.

2. VARIABLE's current value is checked against each type in TYPES to
   see if it satisfies one of them.  In this case, VARIABLE's own type
   is not used; VARIABLE might not even be typed - it could be a
   variable not defined using `defcustom'.

For any of the comparisons against VARIABLE's type, either that type
can be checked directly or its supertypes (inherited types) can also
be checked.

These different type-checking possibilities depend on the value of
argument MODE, as follows, and they determine the meaning of the
returned value:

`direct':   VARIABLE's type matches a member of list TYPES
`inherit':  VARIABLE's type matches or is a subtype of a TYPES member
`value':    VARIABLE is bound, and its value satisfies a type in TYPES
`inherit-or-value': `inherit' or `value', tested in that order
`direct-or-value':  `direct' or `value', tested in that order
anything else (default): `inherit'

VARIABLE's current value cannot satisfy a regexp type: it is
impossible to know which concrete types a value must match."
  (case mode
    ((nil inherit)     (help-var-inherits-type-p variable types))
    (inherit-or-value  (or (help-var-inherits-type-p variable types)
                           (help-var-val-satisfies-type-p variable types)))
    (value             (help-var-val-satisfies-type-p variable types))
    (direct            (help-var-matches-type-p variable types))
    (direct-or-value   (or (member (get variable 'custom-type) types)
                           (help-var-val-satisfies-type-p variable types)))
    (otherwise         (help-var-inherits-type-p variable types))))

(defun help-var-matches-type-p (variable types)
  "VARIABLE's type matches a member of TYPES."
  (catch 'help-type-matches
    (let ((var-type  (get variable 'custom-type)))
      (dolist (type  types)
        (when (if (stringp type)
                  (save-match-data (string-match type (format "%s" (format "%S" var-type))))
                (equal var-type type))
          (throw 'help-type-matches t))))
    nil))

(defun help-var-inherits-type-p (variable types)
  "VARIABLE's type matches or is a subtype of a member of list TYPES."
  (catch 'help-type-inherits
    (let ((var-type  (get variable 'custom-type)))
      (dolist (type  types)
        (while var-type
          (when (or (and (stringp type)
                         (save-match-data
                           (string-match type (format "%s" (format "%S" var-type)))))
                    (equal type var-type))
            (throw 'help-type-inherits t))
          (when (consp var-type) (setq var-type  (car var-type)))
          (when (or (and (stringp type)
                         (save-match-data
                           (string-match type (format "%s" (format "%S" var-type)))))
                    (equal type var-type))
            (throw 'help-type-inherits t))
          (setq var-type  (car (get var-type 'widget-type))))
        (setq var-type  (get variable 'custom-type))))
    nil))

(defun help-var-val-satisfies-type-p (variable types)
  "VARIABLE is bound, and its value satisfies a type in the list TYPES."
  (and (boundp variable)
       (let ((val  (symbol-value variable)))
         (and (widget-convert (get variable 'custom-type))
              (help-value-satisfies-type-p val types)))))

(defun help-value-satisfies-type-p (value types)
  "Return non-nil if VALUE satisfies a type in the list TYPES."
  (catch 'help-type-value-satisfies
    (dolist (type  types)
      (unless (stringp type)            ; Skip, for regexp type.
        (setq type  (widget-convert type))
        ;; Satisfies if either :match or :validate.
        (when (condition-case nil
                  (progn (when (and (widget-get type :match)
                                    (widget-apply type :match value))
                           (throw 'help-type-value-satisfies t))
                         (when (and (widget-get type :validate)
                                    (progn (widget-put type :value value)
                                           (not (widget-apply type :validate))))
                           (throw 'help-type-value-satisfies t)))
                (error nil))
          (throw 'help-type-value-satisfies t))))
    nil))

(defun help-custom-type (variable)
  "Returns the `defcustom' type of VARIABLE.
Returns nil if VARIABLE is not a user option.

Note: If the library that defines VARIABLE has not yet been loaded,
then `help-custom-type' loads it.  Be sure you want to do that
before you call this function."
  (and (custom-variable-p variable)
       (or (get variable 'custom-type) (progn (custom-load-symbol variable)
                                              (get variable 'custom-type)))))

;; Borrowed from `ps-print.el'
(defun help-remove-duplicates (list)
  "Copy of LIST with duplicate elements removed.  Tested with `equal'."
  (let ((tail  list)
        new)
    (while tail
      (unless (member (car tail) new) (push (car tail) new))
      (pop tail))
    (nreverse new)))

;;;###autoload
(defun describe-file (filename &optional internal-form-p no-error-p)
  "Describe the file named FILENAME.
If FILENAME is nil, describe current directory (`default-directory').
If FILENAME is the name of an autofile bookmark and you use library
`Bookmark+', then show also the bookmark information (tags etc.).  In
this case, a prefix arg shows the internal form of the bookmark.

In Lisp code:

Non-nil optional arg INTERNAL-FORM-P shows the internal form.
Non-nil optional arg NO-ERROR-P prints an error message but does not
 raise an error."
  (interactive "FDescribe file: \nP")
  (unless filename (setq filename default-directory))
  (help-setup-xref `(describe-file ,filename ,internal-form-p ,no-error-p) (interactive-p))
  (let ((attrs  (file-attributes filename))
        ;; Functions `bmkp-*' are defined in `bookmark+.el'.
        (bmk   (and (fboundp 'bmkp-get-autofile-bookmark)  (bmkp-get-autofile-bookmark filename))))
    (if (not attrs)
        (if no-error-p
            (message "Cannot open file `%s'" filename)
          (error "Cannot open file `%s'" filename))
      (let* ((type             (nth 0 attrs))
             (numlinks         (nth 1 attrs))
             (uid              (nth 2 attrs))
             (gid              (nth 3 attrs))
             (last-access      (nth 4 attrs))
             (last-mod         (nth 5 attrs))
             (last-status-chg  (nth 6 attrs))
             (size             (nth 7 attrs))
             (permissions      (nth 8 attrs))
             ;; Skip 9: t iff file's gid would change if file were deleted and recreated.
             (inode            (nth 10 attrs))
             (device           (nth 11 attrs))
             (help-text
              (concat
               (format "`%s'\n%s\n\n" filename (make-string (+ 2 (length filename)) ?-))
               (format "File Type:                       %s\n"
                       (cond ((eq t type) "Directory")
                             ((stringp type) (format "Symbolic link to `%s'" type))
                             (t "Normal file")))
               (format "Permissions:                %s\n" permissions)
               (and (not (eq t type)) (format "Size in bytes:              %g\n" size))
               (format-time-string
                "Time of last access:        %a %b %e %T %Y (%Z)\n" last-access)
               (format-time-string
                "Time of last modification:  %a %b %e %T %Y (%Z)\n" last-mod)
               (format-time-string
                "Time of last status change: %a %b %e %T %Y (%Z)\n" last-status-chg)
               (format "Number of links:            %d\n" numlinks)
               (format "User ID (UID):              %s\n" uid)
               (format "Group ID (GID):             %s\n" gid)
               (format "Inode:                      %S\n" inode)
               (format "Device number:              %s\n" device))))
        (with-output-to-temp-buffer "*Help*"
          (when bmk
            (if internal-form-p
                (let* ((bname     (bookmark-name-from-full-record bmk))
                       (bmk-defn  (format "Bookmark `%s'\n%s\n\n%s"
                                          bname   (make-string (+ 11 (length bname)) ?-)
                                          (pp-to-string bmk))))
                  (princ bmk-defn) (terpri) (terpri))
              (princ (bmkp-bookmark-description bmk 'NO-IMAGE)) (terpri) (terpri)))
          (princ help-text))
        help-text))))                   ; Return displayed text.

;;;###autoload
(defun describe-keymap (keymap)
  "Describe bindings in KEYMAP, a variable whose value is a keymap.
Completion is available for the keymap name."
  (interactive
   (list (intern
          (completing-read
           "Keymap: " obarray
           (lambda (m) (and (boundp m) (keymapp (symbol-value m))))
           t nil 'variable-name-history))))
  (unless (and (symbolp keymap) (boundp keymap) (keymapp (symbol-value keymap)))
    (error "`%S' is not a keymapp" keymap))
  (let ((name  (symbol-name keymap))
        (doc   (documentation-property keymap 'variable-documentation)))
    (help-setup-xref (list #'describe-keymap keymap) (interactive-p))
    (with-output-to-temp-buffer "*Help*"
      (princ name) (terpri)
      (princ (make-string (length name) ?-)) (terpri) (terpri)
      (when doc (princ doc) (terpri) (terpri))
      ;; Use `insert' instead of `princ', so control chars (e.g. \377) insert correctly.
      (with-current-buffer "*Help*"
        (insert (substitute-command-keys (concat "\\{" name "}")))))))


;; REPLACES ORIGINAL in `help.el':
;; 1. Preferred candidate is `symbol-nearest-point'.
;; 2. Must be a command, not just a function.
;; 3. Calls `remove-help-window'.
;;
;;;###autoload
(defun where-is (definition &optional insert)
  "Give keyboard/menu/mouse sequences that invoke specified command.
Argument DEFINITION is a command definition, usually a symbol with a
function definition.  Default candidate is: preferably the
`symbol-nearest-point', or else the innermost function call
surrounding point (`function-called-at-point').
Non-nil prefix arg INSERT means insert the message in the buffer."
  (interactive
   (let ((fn                            (or (and (fboundp 'symbol-nearest-point)
                                                 (symbol-nearest-point))
                                            (function-called-at-point)))
         (enable-recursive-minibuffers  t))
     (list (intern (completing-read "Where is command: " obarray 'commandp t
                                    nil nil (and fn (symbol-name fn)) t)))))
  (remove-help-window)
  (let* ((keys             (where-is-internal definition overriding-local-map nil nil))
         (keys1            (mapconcat (if (fboundp 'naked-key-description)
                                          #'naked-key-description
                                        #'key-description)
                                      keys ", "))
         (standard-output  (if insert (current-buffer) t)))
    (if insert
        (if (> (length keys1) 0)
            (princ (format "%s (%s)" keys1 definition))
          (princ (format "M-x %s RET" definition)))
      (if (> (length keys1) 0)
          (princ (format "`%s' is on `%s'" definition keys1))
        (princ (format "`%s' is not on any key" definition)))))
  nil)


;; REPLACES ORIGINAL in `help.el':
;; Lets you complete the library name against string variables.
;; Calls `remove-help-window'.
;;
;;;###autoload
(defun locate-library (library &optional nosuffix path interactive-call)
  "Show the full path name of Emacs library LIBRARY.
This command searches the directories in your `load-path' like
`M-x load-library' to find the file that would be loaded by
`M-x load-library RET LIBRARY RET'.

Optional second arg NOSUFFIX non-nil means don't add suffixes `.elc'
or `.el' to the specified name LIBRARY (like calling `load' instead of
`load-library').

If the optional third arg PATH is specified, that list of directories
is used instead of `load-path'.

When called from a program, the file name is normally returned as a
string.  When run interactively, the argument INTERACTIVE-CALL is t,
and the file name is displayed in the echo area."
  (interactive (list (if (fboundp 'icicle-read-string-completing)
                         (icicle-read-string-completing "Locate library: ")
                       (read-string "Locate library: "))
                     nil nil t))
  (let (result)
    (catch 'answer
      (mapcar
       (lambda (dir)
         (mapcar (lambda (suf)
                   (let ((try  (expand-file-name (concat library suf) dir)))
                     (when (and (file-readable-p try)
                                (null (file-directory-p try)))
                       (setq result  try)
                       (message "Library is file `%s'." try)
                       (throw 'answer try))))

                 (if nosuffix
                     '("")
                   '(".elc" ".el" "")
;;; load doesn't handle this yet.
;;;         (let ((basic  '(".elc" ".el" ""))
;;;               (compressed '(".Z" ".gz" "")))
;;;           ;; If autocompression mode is on,
;;;           ;; consider all combinations of library suffixes
;;;           ;; and compression suffixes.
;;;           (if (rassq 'jka-compr-handler file-name-handler-alist)
;;;               (apply 'nconc
;;;                      (mapcar (lambda (compelt)
;;;                                (mapcar (lambda (baselt)
;;;                                          (concat baselt compelt))
;;;                                        basic))
;;;                              compressed))
;;;             basic))
                   )))
       (or path load-path)))
    (and interactive-call
         (if result
             (message "Library is file `%s'" result)
           (message "No library `%s' in search path." library)))
    (remove-help-window)
    result))

;;;###autoload
(defun view-emacs-lisp-news ()
  "Display information on recent changes to Emacs Lisp."
  (interactive)
  (message "Looking for Emacs Lisp Changes file...")
  (find-file-read-only-other-window (expand-file-name "LNEWS" data-directory))
  (auto-save-mode nil)                  ; Turn it off.
  (remove-help-window)
  (message "Looking for Emacs Lisp Changes file...done"))

;;;###autoload
(defun save-*Help*-buffer ()
  "Rename *Help* buffer as new buffer *Help*<N>, N=2,3...."
  (interactive)
  (let ((notifying-user-of-mode  nil)    ; No msg on mode (in `misc-fns.el').
        (saved-help              (buffer-name (generate-new-buffer "*Help*"))))
    (save-excursion
      (set-buffer "*Help*")
      (copy-to-buffer saved-help (point-min) (point-max))
      (when (interactive-p)
        (message "Saved contents of *Help* buffer to buffer %s."
                 saved-help)))))

(defun help-on-click/key-lookup (key &optional pp-key where)
  "Look up information on KEY via `describe-key' and `info'.
Optional args PP-KEY and WHERE are strings naming KEY and its type.
Their defaults are KEY's `key-description' and \"Key sequence\".
Function `Info-goto-emacs-key-command-node' is used to look up KEY."
  (sit-for 0 200);; HACK to fix bug if click on scroll bar in `help-on-click/key'.
  (setq where   (or where "Key sequence ")
        pp-key  (or pp-key (if (fboundp 'naked-key-description)
                               (naked-key-description key)
                             (key-description key))))
  (let* ((described-p  (if (fboundp 'naked-key-description)
                           (naked-key-description key)
                         (key-description key)))
         ;; The version of `Info-goto-emacs-key-command-node' defined in `info+20.el' returns
         ;; non-nil if Info doc is found.  The standard version defined `info.el' will not.
         (documented-p (Info-goto-emacs-key-command-node key))) ; nil if have only std version
    (when (and (not documented-p)(get-buffer-window "*info*" 'visible)) (Info-exit))
    (cond ((and described-p documented-p)
           (when (fboundp 'show-*Help*-buffer) (show-*Help*-buffer))
           (message "%s`%s': summary in *Help* buffer; doc in *info* buffer."
                    where pp-key))
          (described-p
           (when (fboundp 'show-*Help*-buffer) (show-*Help*-buffer))
           (message "%s`%s': summary in *Help* buffer." where pp-key))
          (documented-p
           (message "%s`%s': doc in *info* buffer." where pp-key))
          (t
           (message "%s`%s' is undefined." where pp-key)))))

;;;###autoload
(defun help-on-click/key (key)
  "Give help on a key/menu sequence or object clicked with the mouse.
The object can be any part of an Emacs window or a name appearing in a
buffer.  You can do any of the following:

    type a key sequence (e.g. `C-M-s')
    choose a menu item (e.g. [menu-bar files open-file])
    click on a scroll bar
    click on the mode line
    click in the minibuffer
    click on an Emacs-related name in a buffer: apropos is called
    click anywhere else in a buffer: its modes are described

Help is generally provided using `describe-key' and the Emacs online
manual (via `Info-goto-emacs-key-command-node').  If no entry is found
in the index of the Emacs manual, then the manual is searched from the
beginning for literal occurrences of KEY.

For example, the KEY `C-g' is not in the index (for some reason), so
the manual is searched.  (Once an occurrence is found, you can
repeatedly type `s' in *Info* to search for additional occurrences.)

If you click on a name in a buffer, then `apropos-documentation' and
`apropos' are used to find information on the name.  These functions
are not used when you do something besides click on a name.

If you click elsewhere in a buffer other than the minibuffer, then
`describe-mode' is used to describe the buffer's current mode(s)."
  (interactive "kClick mouse on something or type a key sequence.")
  (let ((temp-buffer-show-function  'switch-to-buffer-other-window)
        (font-lock-verbose          nil)
        (global-font-lock-mode      nil))
    ;; DEBUG (message "KEY: `%s'" key)(sit-for 4) ; DEBUG
    (cond ((stringp key)
           (help-on-click/key-lookup key))
          (t                            ; Vector.
           (let ((type  (aref key 0)))
             (cond ((or (symbolp type)(integerp type))
                    (cond ((eq 'mode-line type) ; Click on the mode line.
                           (Info-goto-node "(emacs)Mode Line")
                           (message "Mode line: decribed in *info* buffer."))
                          (t            ; Normal key sequence.
                           (help-on-click/key-lookup key))))
                   ((eq 'menu-bar (car type))

                    (help-on-click/key-lookup key (aref key (1- (length key))) "Menu item "))
                   ((not (eq 'down (car (event-modifiers (car type))))) ; e.g. mouse menus
                    (help-on-click/key-lookup key))
                   (t                   ; Mouse click.
                    (setq key  type)
                    (cond ((window-minibuffer-p ; Click in minibuffer.
                            (posn-window (event-start key)))
                           (Info-goto-node "(emacs)Minibuffer")
                           (message "Minibuffer: decribed in *info* buffer."))
                          (t
                           (let ((symb            (save-excursion
                                                    (mouse-set-point key)
                                                    (if (fboundp 'tap-symbol-at-point)
                                                        (tap-symbol-at-point)
                                                      (symbol-at-point))))
                                 (apropos-do-all  t)
                                 (found-doc       nil)
                                 (found           nil)
                                 (symb-regexp     nil))
                             (cond (symb
                                    (message "Looking for info apropos `%s'..." symb)
                                    (when (get-buffer "*Apropos Doc*")
                                      (kill-buffer (get-buffer "*Apropos Doc*")))
                                    (setq found-doc  (apropos-documentation
                                                      (setq symb-regexp
                                                            (regexp-quote
                                                             (setq symb  (format "%s" symb))))))
                                    (when found-doc
                                      (save-excursion
                                        (set-buffer (get-buffer "*Apropos*"))
                                        (rename-buffer "*Apropos Doc*"))
                                      (when (fboundp '1-window-frames-on) ; In `frame-fns.el'.
                                        (let ((frames  (1-window-frames-on "*Apropos Doc*")))
                                          (while frames
                                            (save-window-excursion
                                              (select-frame (car frames))
                                              (rename-frame nil "*Apropos Doc*")
                                              (pop frames))))))
                                    (setq found  (apropos symb-regexp))
                                    ;; Remove empty stuff.
                                    (setq found  (and (consp found) (or (cdr found) (cadr found))))
                                    ;; Remove *Apropos* window that was displayed needlessly.
                                    (unless found (delete-windows-on "*Apropos*"))
                                    (cond
                                      ((and found-doc found)
                                       (message
                                        "See *Apropos* and *Apropos Doc* buffers."))
                                      (found
                                       (message
                                        "See information on `%s' in the *Apropos* buffer."
                                        symb))
                                      (found-doc
                                       (message
                                        "See information on `%s' in the *Apropos Doc* buffer."
                                        symb))
                                      (t
                                       (message
                                        "No information found regarding `%s'."
                                        symb))))
                                   (t   ; User clicked in buffer, but not on a symbol.
                                    (let ((bufname  (buffer-name (current-buffer))))
                                      (describe-mode)
                                      (when (fboundp 'show-*Help*-buffer) (show-*Help*-buffer))
                                      (message
                                       "Mode(s) of buffer `%s' are described in *Help* buffer."
                                       bufname))))))))))))))

;;;###autoload
(defun mouse-help-on-click (event)
  "Give help on an object clicked with the mouse."
  (interactive "e")
  (help-on-click/key (vector event)))

;;;###autoload
(defun mouse-help-on-mode-line-click (event)
  "Give help on the mode line."
  (interactive "e")
  (help-on-click/key (vector 'mode-line event)))

;;;###autoload
(defun pop-to-help-toggle ()
  "Pop to buffer *Help* or back to the buffer that sent you to *Help*."
  (interactive)
  (let ((orig-buf                   (and (buffer-live-p help-origin-buffer)
                                         (get-buffer help-origin-buffer)))
        (w32-grab-focus-on-raise    t)
        (win32-grab-focus-on-raise  t)) ; Older name.
    (if (string-match "*Help*" (buffer-name))
        (cond ((not orig-buf)
               (error "No buffer to return to"))
              ((string-match "Minibuf" (buffer-name orig-buf)) ; No `minibufferp' in Emacs 20.
               (select-frame-set-input-focus
                (window-frame (select-window (minibuffer-window)))))
              (t
               (pop-to-buffer orig-buf)))
      (setq help-origin-buffer  (current-buffer))
      (pop-to-buffer "*Help*"))))

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'help+20)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; help+20.el ends here
