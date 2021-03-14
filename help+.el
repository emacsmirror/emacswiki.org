;;; help+.el --- Extensions to `help.el'.
;;
;; Filename: help+.el
;; Description: Extensions to `help.el'.
;; Author: Drew Adams
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 1999-2021, Drew Adams, all rights reserved.
;; Created: Tue Mar 16 14:18:11 1999
;; Version: 0
;; Package-Requires: ()
;; Last-Updated: Sun Mar 14 14:47:44 2021 (-0700)
;;           By: dradams
;;     Update #: 2236
;; URL: https://www.emacswiki.org/emacs/download/help%2b.el
;; Doc URL: https://emacswiki.org/emacs/HelpPlus
;; Keywords: help
;; Compatibility: GNU Emacs: 22.x, 23.x, 24.x, 25.x, 26.x
;;
;; Features that might be required by this library:
;;
;;   `apropos', `apropos+', `avoid', `backquote', `bookmark',
;;   `bookmark+', `bookmark+-1', `bookmark+-bmu', `bookmark+-key',
;;   `bookmark+-lit', `button', `bytecomp', `cconv', `cl', `cl-lib',
;;   `cmds-menu', `col-highlight', `crosshairs', `fit-frame',
;;   `font-lock', `font-lock+', `frame-fns', `gv', `help+',
;;   `help-fns', `help-fns+', `help-macro', `help-macro+',
;;   `help-mode', `hl-line', `hl-line+', `info', `info+', `kmacro',
;;   `macroexp', `menu-bar', `menu-bar+', `misc-cmds', `misc-fns',
;;   `naked', `pp', `pp+', `radix-tree', `replace', `second-sel',
;;   `strings', `syntax', `text-mode', `thingatpt', `thingatpt+',
;;   `vline', `w32browser-dlgopen', `wid-edit', `wid-edit+'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Extensions to `help.el' for Emacs 22 and later.  For similar
;;    extensions to `help.el' for Emacs 20, see `help+20.el'.
;;
;;    Note: As of Emacs 24.4, byte-compiling this file in one Emacs
;;    version and using the compiled file in another Emacs version
;;    does not work.
;;
;;
;;  Commands defined here:
;;
;;    `help-on-click/key', `mouse-help-on-click',
;;    `mouse-help-on-mode-line-click', `pop-to-help-toggle'.
;;
;;  Non-interactive functions defined here:
;;
;;    `help-on-click/key-lookup'.
;;
;;  Internal variables defined here:
;;
;;    `help-origin-buffer'.
;;
;;
;;  ***** NOTE: The following functions defined in `help.el' have
;;              been REDEFINED HERE:
;;
;;  `describe-key' (Emacs 22-25), `where-is'.
;;
;;
;;  ***** NOTE: The doc string for `help-for-help' has been
;;              REDEFINED HERE (see `make-help-screen help-for-help')
;;
;;  The following bindings are made here:
;;
;;    `C-h u'      `man'
;;    `C-h C-a'    `apropos'
;;    `C-h C-l'    `locate-library'
;;    `C-h RET'    `help-on-click/key'
;;    `C-h M-a'    `apropos-documentation'
;;    `C-h M-o'    `pop-to-help-toggle'
;;    `C-h C-M-a'  `tags-apropos'
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
;; 2021/03/14 dadams
;;     Soft-require help-fns+.el at both compile time and runtime.
;; 2020/09/26 dadams
;;     help-on-click/key-lookup:
;;       Fix change from 2017-10-21 - describe-key if analyzed returns non-nil.
;;       Wrap Info-goto-emacs-key-command-node in condition-case, to give nil if error.
;; 2020/08/14 dadams
;;     describe-key Use help-print-return-message, not print-help-return-message.
;; 2017/10/21 dadams
;;     describe-key: Do not redefine for Emacs 26+ (not worth it).
;;     help-on-click/key-lookup: Use help--analyze-key, if defined, instead of describe-key.
;; 2015/07/02 dadams
;;     help-on-click/key: Removed part of doc string that said that C-g is not in manual index.
;; 2014/05/04 dadams
;;     Emacs 20-22: soft-require info+20.el (new) instead of info+.el.
;; 2014/04/22 dadams
;;     Updated for Emacs 24.4: with-help-window, not with-output-to-temp-buffer - bug #17109.
;;     help-on-click/key: save-excursion.set-buffer -> with-current-buffer.
;; 2012/08/21 dadams
;;     Call tap-put-thing-at-point-props after load thingatpt+.el.
;; 2012/08/18 dadams
;;     Invoke tap-define-aliases-wo-prefix if thingatpt+.el is loaded.
;;     help-on-click/key: Use tap-symbol-at-point, not symbol-at-point, if defined.
;; 2012/04/01 dadams
;;     where-is: Wrap individual key sequences in `', not just all of them together.
;; 2011/10/07 dadams
;;     Added soft require of naked.el.
;;     where-is, help-on-click/key-lookup: Use naked-key-description if available.
;; 2011/01/04 dadams
;;     Removed autoload cookies from non-interactive function and define-key.
;; 2008-01-03 dadams
;;     describe-key: Replaced newline with ", " before "which is".
;; 2007/12/20 dadams
;;     pop-to-help-toggle = C-h M-o, so C-h C-o can be describe-option.
;; 2007/12/13 dadams
;;     Created.
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

(require 'info nil t)  ;; (no error if not found):
                       ;; Info-exit, Info-goto-node, Info-goto-emacs-key-command-node
(if (> emacs-major-version 22);; (no error if not found):
    ;; Info-goto-emacs-key-command-node (returns found-p)
    (require 'info+ nil t)
  (require 'info+20 nil t))
(require 'thingatpt nil t)  ;; (no error if not found): symbol-at-point

(when (and (require 'thingatpt+ nil t) ;; (no error if not found)
           (fboundp 'tap-put-thing-at-point-props)) ; >= 2012-08-21
  (tap-define-aliases-wo-prefix)
  (tap-put-thing-at-point-props))
 ;; symbol-nearest-point, tap-symbol-at-point

(require 'frame-fns nil t)  ;; (no error if not found): 1-window-frames-on
(require 'naked nil t) ;; (no error if not found): naked-key-description

;; Get macro `make-help-screen' when this is compiled or run interpreted,
;; but not when the compiled code is loaded.
;;
;; Get function `help-substitute-command-keys', to put links on key descriptions.
(eval-when-compile
  (require 'help-macro nil t) ;; (no error if not found) make-help-screen
  (require 'help-macro+ nil t)) ;; (no error if not found): make-help-screen
(eval-and-compile
  (require 'help-fns+ nil t)) ;; (no error if not found): help-substitute-command-keys

;;;;;;;;;;;;;;;;;;;;

(defvar help-origin-buffer nil "Buffer that we left, to go to *Help*.")

(define-key help-map [?\C-m] 'help-on-click/key) ; RET
(define-key help-map "u" 'man) ; in `man.el'
(define-key help-map "\C-a" 'apropos)
(define-key help-map "\M-o" 'pop-to-help-toggle)
(define-key help-map "\C-l" 'locate-library)
(define-key help-map "\M-a" 'apropos-documentation)
(define-key help-map "\C-\M-a" 'tags-apropos)
(define-key help-map [down-mouse-1] 'mouse-help-on-click)
(define-key help-map [mode-line down-mouse-1] 'mouse-help-on-mode-line-click)


;; REPLACES ORIGINAL in `help.el':
;;
;; Return nil if KEY is undefined; else return t.
;;
(unless (fboundp 'help--analyze-key)    ; Emacs 26+
  (defun describe-key (&optional key untranslated up-event)
    "Describe the command that a keyboard/menu/mouse sequence invokes.
KEY can be any kind of a key sequence; it can include keyboard events,
mouse events, and/or menu events.  When calling from a program,
pass KEY as a string or a vector.

If non-nil, UNTRANSLATED is a vector of the corresponding untranslated events.
It can also be a number, in which case the untranslated events from
the last key sequence entered are used.
UP-EVENT is the up-event that was discarded by reading KEY, or nil.

If KEY is a menu item or a tool-bar button that is disabled, this command
temporarily enables it to allow getting help on disabled items and buttons.
Return nil if KEY is undefined; else return t."
    (interactive
     (let ((enable-disabled-menus-and-buttons  t)
           (cursor-in-echo-area                t)
           saved-yank-menu)
       (unwind-protect
            (let (key)
              ;; If yank-menu is empty, populate it temporarily, so that
              ;; "Select and Paste" menu can generate a complete event.
              (when (null (cdr yank-menu))
                (setq saved-yank-menu  (copy-sequence yank-menu))
                (menu-bar-update-yank-menu "(any string)" nil))
              (setq key  (read-key-sequence "Describe key (or click or menu item): "))
              (list key
                    (prefix-numeric-value current-prefix-arg)
                    ;; If KEY is a down-event, read and include the
                    ;; corresponding up-event.  Note that there are also
                    ;; down-events on scroll bars and mode lines: the actual
                    ;; event then is in the second element of the vector.
                    (and (vectorp key)
                         (let ((last-idx  (1- (length key))))
                           (and (eventp (aref key last-idx))
                                (memq 'down (event-modifiers (aref key last-idx)))))
                         (or (and (eventp (aref key 0))
                                  (memq 'down (event-modifiers (aref key 0)))
                                  ;; However, for the C-down-mouse-2 popup
                                  ;; menu, there is no subsequent up-event.  In
                                  ;; this case, the up-event is the next
                                  ;; element in the supplied vector.
                                  (= (length key) 1))
                             (and (> (length key) 1)
                                  (eventp (aref key 1))
                                  (memq 'down (event-modifiers (aref key 1)))))
                         (read-event))))
         ;; Put yank-menu back as it was, if we changed it.
         (when saved-yank-menu
           (setq yank-menu  (copy-sequence saved-yank-menu))
           (fset 'yank-menu (cons 'keymap yank-menu))))))
    (when (numberp untranslated) (setq untranslated  (this-single-command-raw-keys)))
    (let* ((event      (aref key (if (and (symbolp (aref key 0))
                                          (> (length key) 1)
                                          (consp (aref key 1)))
                                     1
                                   0)))
           (modifiers  (event-modifiers event))
           (mouse-msg  (if (or (memq 'click modifiers)
                               (memq 'down modifiers)
                               (memq 'drag modifiers))
                           " at that spot"
                         ""))
           (defn       (key-binding key t))
           defn-up defn-up-tricky ev-type mouse-1-remapped mouse-1-tricky)

      ;; Handle the case where we faked an entry in "Select and Paste" menu.
      (when (and (eq defn nil)
                 (stringp (aref key (1- (length key))))
                 (eq (key-binding (substring key 0 -1)) 'yank-menu))
        (setq defn  'menu-bar-select-yank))
      (cond ((or (null defn)  (integerp defn)  (equal defn 'undefined))
             (message "%s%s is undefined" (help-key-description key untranslated) mouse-msg)
             nil)                       ; Return nil: undefined.
            (t
             (help-setup-xref (list #'describe-function defn) (interactive-p))
             ;; Don't bother user with strings from (e.g.) the select-paste menu.
             (when (stringp (aref key (1- (length key))))
               (aset key (1- (length key)) "(any string)"))
             (when (and untranslated  (stringp (aref untranslated (1- (length untranslated)))))
               (aset untranslated (1- (length untranslated))
                     "(any string)"))
             ;; Need to do this before erasing *Help* buffer in case event
             ;; is a mouse click in an existing *Help* buffer.
             (when up-event
               (setq ev-type  (event-basic-type up-event))
               (let ((sequence  (vector up-event)))
                 (when (and (eq ev-type 'mouse-1)
                            mouse-1-click-follows-link
                            (not (eq mouse-1-click-follows-link 'double))
                            (setq mouse-1-remapped  (mouse-on-link-p (event-start up-event))))
                   (setq mouse-1-tricky  (and (integerp mouse-1-click-follows-link)
                                              (> mouse-1-click-follows-link 0)))
                   (cond ((stringp mouse-1-remapped) (setq sequence  mouse-1-remapped))
                         ((vectorp mouse-1-remapped) (setcar up-event (elt mouse-1-remapped 0)))
                         (t (setcar up-event 'mouse-2))))
                 (setq defn-up  (key-binding sequence nil nil (event-start up-event)))
                 (when mouse-1-tricky
                   (setq sequence  (vector up-event))
                   (aset sequence 0 'mouse-1)
                   (setq defn-up-tricky  (key-binding sequence nil nil
                                                      (event-start up-event))))))
             (if (fboundp 'with-help-window)
                 (with-help-window (help-buffer)
                   (princ (help-key-description key untranslated))
                   (princ (format "\
%s runs the command %S, which is "
                                  mouse-msg defn))
                   (describe-function-1 defn)
                   (when up-event
                     (unless (or (null defn-up)  (integerp defn-up)  (equal defn-up 'undefined))
                       (princ (format "

----------------- up-event %s----------------

<%S>%s%s runs the command %S, which is "
                                      (if mouse-1-tricky "(short click) " "")
                                      ev-type mouse-msg
                                      (if mouse-1-remapped " is remapped to <mouse-2>, which" "")
                                      defn-up))
                       (describe-function-1 defn-up))
                     (unless (or (null defn-up-tricky)
                                 (integerp defn-up-tricky)
                                 (eq defn-up-tricky 'undefined))
                       (princ (format "

----------------- up-event (long click) ----------------

Pressing <%S>%s for longer than %d milli-seconds
runs the command %S, which is "
                                      ev-type mouse-msg
                                      mouse-1-click-follows-link
                                      defn-up-tricky))
                       (describe-function-1 defn-up-tricky)))
                   (if (fboundp 'help-print-return-message)
                       (help-print-return-message)
                     (print-help-return-message)))
               (with-output-to-temp-buffer (help-buffer) ; Emacs 22-24.3.
                 (princ (help-key-description key untranslated))
                 (princ (format "\
%s runs the command %S, which is "
                                mouse-msg defn))
                 (describe-function-1 defn)
                 (when up-event
                   (unless (or (null defn-up)  (integerp defn-up)  (equal defn-up 'undefined))
                     (princ (format "

----------------- up-event %s----------------

<%S>%s%s runs the command %S, which is "
                                    (if mouse-1-tricky "(short click) " "")
                                    ev-type mouse-msg
                                    (if mouse-1-remapped " is remapped to <mouse-2>, which" "")
                                    defn-up))
                     (describe-function-1 defn-up))
                   (unless (or (null defn-up-tricky)
                               (integerp defn-up-tricky)
                               (eq defn-up-tricky 'undefined))
                     (princ (format "

----------------- up-event (long click) ----------------

Pressing <%S>%s for longer than %d milli-seconds
runs the command %S, which is "
                                    ev-type mouse-msg
                                    mouse-1-click-follows-link
                                    defn-up-tricky))
                     (describe-function-1 defn-up-tricky)))
                 (if (fboundp 'help-print-return-message)
                     (help-print-return-message)
                   (print-help-return-message))))))))) ; Return t: defined.


;; REPLACES ORIGINAL in `help.el':
;;
;; 1. If `help-fns+.el' is loaded then put links on key descriptions.
;; 2. Updated key bindings.
;;
(make-help-screen help-for-help-internal
                  "RET [abcCefFhiIkKlLmnopqrsStuvw] C-[\acdeflmoptw] M-[acko] C-M-a \
\(? for more help):"
                  (let ((raw  "This is the Emacs `help-command', accessible via `%THIS-KEY%'.
Type a help option (below) now, for help on a particular topic.
Use \\<help-mode-map>`\\[scroll-up-command]' or `\\[scroll-down-command]' to scroll this text.\
  Type \\<help-map>`\\[help-quit]' to exit Help.
\(A \"command\" is any function that you can execute via `M-x'.)

LEARNING EMACS
--------------
\\[help-with-tutorial]:   Starts a tutorial for learning Emacs.
\\[view-emacs-FAQ]: Explains frequently asked Emacs questions.

COMMONLY USED
-------------
\\[help-on-click/key]: Help about a key sequence or something you click with the mouse.
\\[apropos-command]:   Shows commands that match a regular expression (regexp).
\\[describe-bindings]:   Shows current key bindings: keyboard, menu bar, and mouse.
\\[describe-command]:   Shows the doc for an Emacs command.
\\[describe-function]:   Shows the doc for an Emacs function.
\\[Info-goto-emacs-command-node]:   Opens the Emacs manual for an Emacs command.
\\[info]:   Enters `Info', to browse manuals, including Emacs and Emacs Lisp.
\\[describe-key]:   Describes the command bound to keyboard/menu/mouse sequence.
\\[Info-goto-emacs-key-command-node]:   Opens the Emacs manual for a keyboard/menu/mouse \
sequence.
\\[describe-mode]:   Describes the current major and minor modes.
\\[describe-option]:   Shows an Emacs user option's value and documentation.
\\[describe-variable]:   Shows an Emacs variable's value and documentation.

MORE ADVANCED HELP
------------------
\\[apropos-documentation]:   Shows Emacs functions and variables whose doc matches a regexp.
\\[view-lossage]:   Shows what you just typed (last 100 keystrokes & mouse actions).
\\[view-emacs-news]:   Describes what's new in this Emacs release.
\\[finder-by-keyword]:   Finds Emacs-Lisp libraries that match a topic.
\\[describe-syntax]:   Describes the current syntax table.
\\[info-lookup-symbol]:   Finds a symbol in the manual for the current buffer's language.
\\[man]:   Finds a topic in the Unix manual.
\\[where-is]:   Identifies a keyboard/menu/mouse sequence that invokes a command.
\\[apropos]: Shows Emacs functions and variables that match a regexp.
\\[describe-key-briefly]: Identifies the command bound to a keyboard/menu/mouse sequence.
\\[locate-library]: Shows the path name to an Emacs library.
\\[describe-option-of-type]: Shows value and doc for an Emacs user option of a given type.
\\[describe-gnu-project]: Shows information about the GNU project.
\\[describe-no-warranty]: Shows information about the absence of a warranty.
\\[describe-copying]: Shows the GNU Emacs General Public License.
\\[pop-to-help-toggle]: Pops to Help buffer or back to the buffer that sent you to Help.
\\[tags-apropos]: Shows the tags matched by a given string.
\\[describe-distribution]: Shows Emacs ordering information.

INTERNATIONAL
-------------
\\[describe-coding-system]:   Describes a coding system.
\\[view-hello-file]    Displays the HELLO file, which illustrates scripts and languages.
\\[describe-input-method]:   Describes an input method.
\\[describe-language-environment]:   Describes a language environment.
"))
                    (if (fboundp 'help-substitute-command-keys) ; In `help-fns+.el'.
                        (help-substitute-command-keys raw 'ADD-HELP-BUTTONS)
                      raw))
                  help-map)


;; REPLACES ORIGINAL in `help.el':
;; Preferred candidate is `symbol-nearest-point'.
;;
;;;###autoload
(defun where-is (definition &optional insert)
  "Show keyboard/menu/mouse sequences that invoke specified command.
Argument is a command definition, usually a symbol with a function
definition.  Default candidate is: preferably the
`symbol-nearest-point', or else the innermost function call
surrounding point (`function-called-at-point').

With no prefix argument, only commands actually bound to keys are
completion candidates.  With a prefix argument, all commands are
candidates.

With a plain (non-numeric) prefix argument, `C-u', insert the message
in the current buffer."
  (interactive
   (let ((fn                            (or (and (fboundp 'symbol-nearest-point)
                                                 (symbol-nearest-point))
                                            (function-called-at-point)))
         (enable-recursive-minibuffers  t)
         (orig-buf                      (current-buffer))
         val)
     (setq val  (completing-read (if fn
                                     (format "Where is command (default %s): " fn)
                                   "Where is command: ")
                                 obarray
                                 (if current-prefix-arg
                                     'commandp
                                   (lambda (c)
                                     (with-current-buffer orig-buf
                                       (and (commandp c)
                                            (where-is-internal c overriding-local-map
                                                               'non-ascii)))))
                                 t))
     (list (if (equal val "") fn (intern val))
           (consp current-prefix-arg))))
  (unless definition (error "No command"))
  (let ((func             (indirect-function definition))
        (defs             ())
        (standard-output  (if insert (current-buffer) t)))
    ;; In DEFS, find all symbols that are aliases for DEFINITION.
    (mapatoms (lambda (symbol)
                (and (fboundp symbol)
                     (not (eq symbol definition))
                     (eq func (condition-case () (indirect-function symbol) (error symbol)))
                     (push symbol defs))))
    ;; Look at all the symbols--first DEFINITION, then its aliases.
    (dolist (symbol  (cons definition defs))
      (let* ((remapped  (command-remapping symbol))
             (keys      (where-is-internal symbol overriding-local-map nil nil remapped))
             (keys      (mapconcat (if (fboundp 'naked-key-description)
                                       #'naked-key-description
                                     #'key-description)
                                   keys "', `"))
             string)
        (setq string  (if insert
                          (if (> (length keys) 0)
                              (if remapped
                                  (format "%s (%s) (remapped from %s)" keys remapped symbol)
                                (format "%s (%s)" keys symbol))
                            (format "M-x %s RET" symbol))
                        (if (> (length keys) 0)
                            (if remapped
                                (format "%s is remapped to %s which is on `%s'"
                                        symbol remapped keys)
                              (format "%s is on `%s'" symbol keys))
                          ;; If this is the command the user asked about, and it is not on any
                          ;; key, say so.  For other symbols, its aliases, say nothing about
                          ;; them unless they are on keys.
                          (and (eq symbol definition)
                               (format "%s is not on any key" symbol)))))
        (when string
          (unless (eq symbol definition) (princ ";\n its alias "))
          (princ string)))))
  nil)

(defun help-on-click/key-lookup (key &optional pp-key where)
  "Look up information on KEY via `describe-key' and `info'.
Optional args PP-KEY and WHERE are strings naming KEY and its type.
Their defaults are KEY's `key-description' and \"Key sequence\".
Function `Info-goto-emacs-key-command-node' is used to look up KEY."
  (sit-for 0 200) ;; HACK to fix bug if click on scroll bar in `help-on-click/key'.
  (setq where   (or where  "Key sequence ")
        pp-key  (or pp-key  (if (fboundp 'naked-key-description)
                                (naked-key-description key)
                              (key-description key))))
  (let ((described-p   (if (fboundp 'help--analyze-key) ; Emacs 26+
                           (and (cadr (help--analyze-key key nil))  (describe-key key))
                         (describe-key key)))
        ;; The version of `Info-goto-emacs-key-command-node' defined in `info+.el' returns
        ;; non-nil if Info doc is found.  The standard version defined `info.el' raises
        ;; an error if not found.
        (documented-p  (condition-case nil
                           (Info-goto-emacs-key-command-node key)
                         (error nil))))
    (when (and (not documented-p)  (get-buffer-window "*info*" 'visible)) (Info-exit))
    (cond ((and described-p  documented-p)
           (when (fboundp 'show-*Help*-buffer) (show-*Help*-buffer))
           (message "%s`%s': summary in *Help* buffer; doc in buffer `*info*'."
                    where pp-key))
          (described-p
           (when (fboundp 'show-*Help*-buffer) (show-*Help*-buffer))
           (message "%s`%s': summary in buffer `*Help*'." where pp-key))
          (documented-p
           (message "%s`%s': doc in buffer `*info*'." where pp-key))
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

If you click on a name in a buffer, then `apropos-documentation' and
`apropos' are used to find information on the name.  These functions
are not used when you do something besides click on a name.

If you click elsewhere in a buffer other than the minibuffer, then
`describe-mode' is used to describe the buffer's current mode(s)."
  (interactive "kClick mouse on something or type a key sequence")
  (let ((temp-buffer-show-function  'switch-to-buffer-other-window)
        (font-lock-verbose          nil)
        (global-font-lock-mode      nil))
    ;; DEBUG (message "KEY: `%s'" key)(sit-for 4) ; DEBUG
    (cond ((stringp key)
           (help-on-click/key-lookup key))
          (t                            ; Vector.
           (let ((type  (aref key 0)))
             (cond ((or (symbolp type)  (integerp type))
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
                           (message "Minibuffer: decribed in buffer `*info*'."))
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
                                    (setq found-doc
                                          (apropos-documentation
                                           (setq symb-regexp
                                                 (regexp-quote
                                                  (setq symb  (format "%s" symb))))))
                                    (when found-doc
                                      (with-current-buffer (get-buffer "*Apropos*")
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
                                    (setq found  (and (consp found)
                                                      (or (cdr found)  (cadr found))))
                                    ;; Remove *Apropos* window that was displayed needlessly.
                                    (unless found (delete-windows-on "*Apropos*"))
                                    (cond
                                      ((and found-doc  found)
                                       (message
                                        "See buffers `*Apropos*' and `*Apropos Doc*'."))
                                      (found
                                       (message
                                        "See information on `%s' in buffer `*Apropos*'."
                                        symb))
                                      (found-doc
                                       (message
                                        "See information on `%s' in buffer `*Apropos Doc*'."
                                        symb))
                                      (t
                                       (message
                                        "No information found regarding `%s'."
                                        symb))))
                                   (t ; User clicked in buffer, but not on a symbol.
                                    (let ((bufname  (buffer-name (current-buffer))))
                                      (describe-mode)
                                      (when
                                          (fboundp 'show-*Help*-buffer) (show-*Help*-buffer))
                                      (message
                                       "Mode(s) of buffer `%s' are described in buffer \
`*Help*'."
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
        (cond ((not orig-buf) (error "No buffer to return to"))
              ((string-match "Minibuf" (buffer-name orig-buf)) ; No `minibufferp' in Emacs 20.
               (select-frame-set-input-focus
                (window-frame (select-window (minibuffer-window)))))
              (t (pop-to-buffer orig-buf)))
      (setq help-origin-buffer  (current-buffer))
      (pop-to-buffer "*Help*"))))

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'help+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; help+.el ends here
