;;; one-key.el --- One key

;; Filename: one-key.el
;; Description: One key
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;;         rubikitch <rubikitch@ruby-lang.org>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2008, 2009, Andy Stewart, all rights reserved.
;; Copyright (C) 2009, rubikitch, all rights reserved.
;; Created: 2008-12-22 21:54:30
;; Version: 0.7.0
;; Last-Updated: 2009-05-23 00:52:06
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/one-key.el
;; Keywords: one-key
;; Compatibility: GNU Emacs 22 ~ 23
;;
;; Features that might be required by this library:
;;
;; `cl'
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Along with many extensions into Emacs, have many keystroke is
;; used, and always forget the keystroke when have too many.
;;
;; This package is for fix above problems.
;;
;; One Key provide a handle with TOP keystroke, and then when you
;; type TOP keystroke, you will get a keystroke menu with pop-up
;; window, and will show a group keystroke in pop-up window.
;;
;; Then you just type keystroke in window show, you can execute
;; command corresponding.
;;
;; So you need just remember the TOP keystroke with group command.
;; Others keystroke notify will display in pop-up window.
;;
;; * Quick use:
;;
;; Setup below variable and function in your ~/.emacs
;;
;; (defvar one-key-menu-emms-alist nil
;;   "`One-Key' menu list for EMMS.")
;;
;; (setq one-key-menu-emms-alist
;;       '(
;;         (("g" . "Playlist Go") . emms-playlist-mode-go)
;;         (("d" . "Play Directory Tree") . emms-play-directory-tree)
;;         (("f" . "Play File") . emms-play-file)
;;         (("i" . "Play Playlist") . emms-play-playlist)
;;         (("t" . "Add Directory Tree") . emms-add-directory-tree)
;;         (("c" . "Toggle Repeat Track") . emms-toggle-repeat-track)
;;         (("w" . "Toggle Repeat Playlist") . emms-toggle-repeat-playlist)
;;         (("u" . "Play Now") . emms-play-now)
;;         (("z" . "Show") . emms-show)
;;         (("s" . "Emms Streams") . emms-streams)
;;         (("b" . "Emms Browser") . emms-browser)))
;;
;; (defun one-key-menu-emms ()
;;   "`One-Key' menu for EMMS."
;;   (interactive)
;;   (one-key-menu "EMMS" one-key-menu-emms-alist t))
;;
;; Bind any you like keystroke with function `one-key-menu-emms'.
;; Like me,
;;
;;      (global-set-key (kbd "C-c p") 'one-key-menu-emms)
;;
;; When type "C-c p" will popup window and list keystroke menu.
;; Then you just type special keystroke that list in menu,
;; you will execute corresponding command.
;;
;; That's all.
;;
;; And now you don't need remember so many keystrokes, just remember
;; TOP keystroke is enough.
;;
;; * Advanced use:
;;
;; ** The format of menu list:
;;
;; (("KEYSTROKE" . "DESCRIBE") . COMMAND)
;;
;; Example:
;;
;; (defvar example-menu-alist
;;      '(
;;        (("Keystroke-A" . "Describe-A") . Command-A)
;;        (("Keystroke-B" . "Describe-B") . Command-B)
;;        (("Keystroke-C" . "Describe-C") . Command-C)
;;        ))
;;
;; Make sure COMMAND is `interactive', otherwise will
;; throw error.
;;
;; ** The format of menu function:
;;
;; (one-key-menu "MENU-NAME" MENU-ALIST)
;;
;; Example:
;;
;; (defun example-menu ()
;;   (interactive)
;;   (one-key-menu "example" example-menu-alist)
;;
;; ** The argument of function `one-key-menu':
;;
;; `title' is the title of menu, any string you like.
;; `info-alist' is a special list that contain KEY, DESCRIBE
;;      and COMMAND.  see above describe about `example-menu-alist'.
;; `miss-match-exit-p' is mean popup window will exit when you
;;      type a KEY that can't match in menu.
;; `recursion-p' is whether recursion execute `one-key-menu' self
;;      when no KEY match in menu.
;; `protect-function' is a protect function call last in `one-key-menu',
;;      make sure this function is a `interactive' function.
;; `alternate-function' is alternate function execute at last.
;; `execute-last-command-when-miss-match' whether execute last input command
;; when keystroke is miss match.
;;
;; Tips:
;;
;; You can use `one-key-insert-template' insert template code for special keymap.
;; Example, after you run `one-key-insert-template', you will got Keymap prompt:
;; "Keymap to One-Key: ", you type "C-x r", you will got Title prompt:
;; "Title: ", input any name you like.
;; Then will generate template code like below:
;;
;; (defvar one-key-menu-bookmark-alist nil
;;   "The `one-key' menu list for BOOKMARK.")
;;
;; (setq one-key-menu-bookmark-alist
;;    '(
;;      (("C-@" . "point-to-register") . point-to-register)
;;      (("SPC" . "point-to-register") . point-to-register)
;;      (("+" . "increment-register") . increment-register)
;;      (("b" . "bookmark-jump") . bookmark-jump)
;;      (("c" . "clear-rectangle") . clear-rectangle)
;;      (("d" . "delete-rectangle") . delete-rectangle)
;;      (("f" . "frame-configuration-to-register") . frame-configuration-to-register)
;;      (("g" . "insert-register") . insert-register)
;;      (("i" . "insert-register") . insert-register)
;;      (("j" . "jump-to-register") . jump-to-register)
;;      (("k" . "kill-rectangle") . kill-rectangle)
;;      (("l" . "bookmark-bmenu-list") . bookmark-bmenu-list)
;;      (("m" . "bookmark-set") . bookmark-set)
;;      (("n" . "number-to-register") . number-to-register)
;;      (("o" . "open-rectangle") . open-rectangle)
;;      (("r" . "copy-rectangle-to-register") . copy-rectangle-to-register)
;;      (("s" . "copy-to-register") . copy-to-register)
;;      (("t" . "string-rectangle") . string-rectangle)
;;      (("w" . "window-configuration-to-register") . window-configuration-to-register)
;;      (("x" . "copy-to-register") . copy-to-register)
;;      (("y" . "yank-rectangle") . yank-rectangle)
;;      (("C-SPC" . "point-to-register") . point-to-register)
;;      ))
;;
;; (defun one-key-menu-bookmark ()
;;   (interactive)
;;   (one-key-menu "BOOKMARK" one-key-menu-bookmark-alist))
;;
;; Alike you can use command `one-key-show-template', it similar with
;; `one-key-insert-template', it show template code in buffer
;; "One-Key-Template" instead insert.
;;

;;; Installation:
;;
;; Put one-key.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'one-key)
;;
;; Because this library use special implement method,
;; can occur `max-lisp-eval-depth' or `max-specpdl-size' error.
;;
;; So i think setup above two variables larger
;; will minish probability that error occur.
;;
;; Example I set below:
;;
;; (setq max-lisp-eval-depth 10000)
;; (setq max-specpdl-size 10000)
;;

;;; Customize:
;;
;; `one-key-popup-window' Whether popup window when first time run,
;; default is `t'.
;; `one-key-buffer-name' The buffer name of popup menu.
;; `one-key-template-buffer-name' The buffer  name of template code.
;; `one-key-items-per-line' Number of items in one line,
;; if this option is `nil', will calculated by `window-width'.
;; `one-key-help-window-max-height' The maximal height use in popup window.
;;
;; All above can customize easy through:
;;      M-x customize-group RET one-key RET
;;

;;; Change log:
;;
;; 2009/05/23
;;   * Andy Stewart:
;;      * Fix bug of option `one-key-popup-window'.
;;
;; 2009/03/09
;;   * Andy Stewart:
;;      * Add `char-valid-p' for compatibility Emacs 22.
;;
;; 2009/02/25
;;   * Andy Stewart:
;;      * Fix a bug of `one-key-menu'.
;;
;; 2009/02/19
;;   * Andy Stewart:
;;      * Just show help message when first call function `one-key-menu',
;;        don't overwritten message from command.
;;      * Remove function `one-key-menu-quit' and
;;        option `one-key-show-quit-message', unnecessary now.
;;
;; 2009/02/10
;;   * rubikitch
;;      * Fix bug.
;;      * PageUp and PageDown are scroll page keys now.
;;      * Add new option `one-key-show-quit-message'.
;;
;; 2009/01/28
;;   * Andy Stewart:
;;      * Capitalize describe in variable `one-key-menu-*-alist'.
;;
;; 2009/01/27
;;   * rubikitch
;;      * Fix doc.
;;
;; 2009/01/26
;;   * rubikitch
;;      * Improve code.
;;
;; 2009/01/25
;;   * Andy Stewart:
;;      * Applied rubikitch's patch for generate
;;        template code automatically, very nice!
;;
;; 2009/01/22
;;   * rubikitch:
;;      * Add new option `one-key-items-per-line'.
;;      * Refactory code make it more clear.
;;      * Fix bug.
;;   * Andy Stewart:
;;      * Applied rubikitch's patch. Thanks!
;;      * Modified code make build-in keystroke
;;        can be overridden.
;;      * Fix doc.
;;
;; 2009/01/20
;;   * Andy Stewart:
;;      * Add new option `execute-last-command-when-miss-match'
;;        to function `one-key-menu', make user can execute
;;        last input command when miss match key alist.
;;
;; 2009/01/15
;;   * rubikitch:
;;      * Fix bug of `one-key-menu'.
;;      * Add recursion execute support for `one-key-menu'.*
;;        Thanks rubikitch patched for this! ;)
;;
;; 2009/01/04
;;   * Andy Stewart:
;;      * Add `alternate-function' argument with function `one-key-menu'.
;;
;; 2008/12/22
;;   * Andy Stewart:
;;      * First released.
;;

;;; Acknowledgements:
;;
;;      rubikitch <rubikitch@ruby-lang.org>
;;              For send many patches.
;;

;;; TODO
;;
;;
;;

;;; Require
(eval-when-compile (require 'cl))

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Customize ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup one-key nil
  "One key."
  :group 'editing)

(defcustom one-key-popup-window t
  "Whether popup window when first time run `one-key-menu'."
  :type 'boolean
  :group 'one-key)

(defcustom one-key-buffer-name "*One-Key*"
  "The buffer name of popup help window."
  :type 'string
  :group 'one-key)

(defcustom one-key-template-buffer-name "*One-Key-Template*"
  "The name of template buffer."
  :type 'string
  :group 'one-key)

(defcustom one-key-items-per-line nil
  "Number of items in one line.
If nil, it is calculated by `window-width'."
  :type 'int
  :group 'one-key)

(defcustom one-key-help-window-max-height nil
  "The max height of popup help window."
  :type 'int
  :set (lambda (symbol value)
         (set symbol value)
         ;; Default is half height of frame.
         (unless value
           (set symbol (/ (frame-height) 2))))
  :group 'one-key)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Faces ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defface one-key-title
  '((t (:foreground "Gold")))
  "Face for highlighting title."
  :group 'one-key)

(defface one-key-keystroke
  '((t (:foreground "DarkRed")))
  "Face for highlighting keystroke."
  :group 'one-key)

(defface one-key-prompt
  '((t (:foreground "khaki3")))
  "Face for highlighting prompt."
  :group 'one-key)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Variable ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar one-key-help-window-configuration nil
  "The window configuration that record current window configuration before popup help window.")

(defvar one-key-menu-call-first-time t
  "The first time call function `one-key-menu'.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Interactive Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun one-key-show-template (keystroke title)
  "Show template code in buffer `one-key-template-buffer-name'.
KEYSTROKE is bind keymap that you want generate.
TITLE is title name that any string you like."
  (interactive "sKeymap to One-Key (keystroke or keymap name): \nsTitle: ")
  (let ((keymap (one-key-read-keymap keystroke)))
    (with-current-buffer (get-buffer-create one-key-template-buffer-name)
      ;; Insert template.
      (erase-buffer)
      (insert (one-key-make-template keymap title))
      ;; Load `emacs-lisp' syntax highlight.
      (set-syntax-table emacs-lisp-mode-syntax-table)
      (lisp-mode-variables)
      (setq font-lock-mode t)
      (font-lock-fontify-buffer)
      ;; Pop to buffer.
      (switch-to-buffer (current-buffer))
      ;; Move to last argument position of function define.
      (backward-char 3))))

(defun one-key-insert-template (keystroke title)
  "Insert template code.
KEYSTROKE is bind keymap that you want generate.
TITLE is title name that any string you like."
  (interactive "sKeymap to One-Key (keystroke or keymap name): \nsTitle: ")
  (let ((keymap (one-key-read-keymap keystroke)))
    ;; Insert.
    (insert (one-key-make-template keymap title))
    ;; Move to last argument position of function define.
    (backward-char 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Utilities Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun one-key-highlight (msg msg-regexp msg-face)
  "Highlight special `MSG' with regular expression `MSG-REGEXP'.
Will highlight this `MSG' with face `MSG-FACE'."
  (with-temp-buffer
    (insert msg)
    (goto-char (point-min))
    (while (re-search-forward msg-regexp nil t)
      (add-text-properties (match-beginning 0)
                           (match-end 0)
                           msg-face))
    (buffer-string)))

(defun one-key-highlight-prompt (prompt)
  "Highlight PROMPT information."
  (let ((msg (format "The keystroke menu of <%s> type '?' for help." prompt)))
    (message (one-key-highlight msg
                                " \\(<[^<>]*>\\|'[^']*'\\) "
                                '(face one-key-prompt)))))

(defun one-key-highlight-help (title keystroke)
  "Highlight TITLE help information with KEYSTROKE."
  (setq title (one-key-highlight (format "Here is a list of <%s> keystrokes. Type '?' for hide. Type 'q' for exit.\n\n" title)
                                 "\\(<[^<>]*>\\|'[^']*'\\)"
                                 '(face one-key-title)))
  (setq keystroke (one-key-highlight keystroke
                                     "\\[\\([^\\[\\]\\)*\\]"
                                     '(face one-key-keystroke)))
  (concat title keystroke))

(defun one-key-menu (title
                     info-alist
                     &optional
                     miss-match-exit-p
                     recursion-p
                     protect-function
                     alternate-function
                     execute-last-command-when-miss-match)
  "One key menu.

`TITLE' is the title of men, any string can use.
`INFO-ALIST' is a special alist
that contain KEY, DESCRIBE and COMMAND.
`MISS-MATCH-EXIT-P' whether hide popup help window
when keystroke can't match in menu.
`RECURSION-P' whether recursion execute self
when keystroke can't match in menu.
`PROTECT-FUNCTION' the protect function
that call in `unwind-protect'.
`ALTERNATE-FUNCTION' the alternate function execute at last.
`EXECUTE-LAST-COMMAND-WHEN-MISS-MATCH' whether execute
last command when it miss match in key alist."
  (let ((self (function
               (lambda ()
                 (one-key-menu
                  title info-alist miss-match-exit-p
                  recursion-p
                  protect-function
                  alternate-function
                  execute-last-command-when-miss-match))))
        last-key)
    ;; Popup help window when first time call
    ;; and option `one-key-popup-window' is `non-nil'.
    (when (and one-key-menu-call-first-time
               one-key-popup-window)
      (one-key-help-window-toggle title info-alist))
    ;; Execute.
    (unwind-protect
        (let* ((event (read-event
                       ;; Just show help message when first call,
                       ;; don't overwritten message from command.
                       (if one-key-menu-call-first-time
                           (progn
                             (one-key-highlight-prompt title)
                             (setq one-key-menu-call-first-time nil))
                         "")))
               (key (if (if (<= emacs-major-version 22)
                            (with-no-warnings
                              (char-valid-p event)) ;for compatibility Emacs 22
                          (characterp event))
                        ;; Transform to string when event is char.
                        (char-to-string event)
                      ;; Otherwise return vector.
                      (make-vector 1 event)))
               match-key)
          (cond
           ;; Match user keystrokes.
           ((catch 'match
              (loop for ((k . desc) . command) in info-alist do
                    ;; Get match key.
                    (setq match-key k)
                    ;; Call function when match keystroke.
                    (when (one-key-match-keystroke key match-key)
                      ;; Close help window first.
                      (one-key-help-window-close)
                      ;; Set `one-key-menu-call-first-time' with "t" for recursion execute.
                      (setq one-key-menu-call-first-time t)
                      ;; Execute.
                      (call-interactively command)
                      ;; Set `one-key-menu-call-first-time' with "nil".
                      (setq one-key-menu-call-first-time nil)
                      (throw 'match t)))
              nil)
            ;; Handle last.
            (one-key-handle-last alternate-function self recursion-p))
           ;; Match build-in keystroke.
           ((one-key-match-keystroke key "q")
            ;; quit
            (keyboard-quit))
           ((one-key-match-keystroke key "?")
            ;; toggle help window
            (one-key-help-window-toggle title info-alist)
            (funcall self))
           ((one-key-match-keystroke key "C-n")
            ;; scroll up one line
            (one-key-help-window-scroll-up-line)
            (funcall self))
           ((one-key-match-keystroke key "C-p")
            ;; scroll down one line
            (one-key-help-window-scroll-down-line)
            (funcall self))
           ((or (one-key-match-keystroke key "C-j")
                (one-key-match-keystroke key [next]))
            ;; scroll up one screen
            (one-key-help-window-scroll-up)
            (funcall self))
           ((or (one-key-match-keystroke key "C-k")
                (one-key-match-keystroke key [prior]))
            ;; scroll down one screen
            (one-key-help-window-scroll-down)
            (funcall self))
           ;; Not match any keystrokes.
           (t
            ;; Close help window first.
            (one-key-help-window-close)
            ;; Quit when keystroke not match
            ;; and argument `miss-match-exit-p' is `non-nil'.
            (when miss-match-exit-p
              ;; Record last key.
              (setq last-key key)
              ;; Abort.
              (keyboard-quit))
            ;; Handle last.
            (one-key-handle-last alternate-function self recursion-p))))
      ;; Restore value of `one-key-call-first-time'.
      (setq one-key-menu-call-first-time t)
      ;; Close help window.
      (one-key-help-window-close)
      ;; Run protect function
      ;; when `protect-function' is valid function.
      (if (and protect-function
               (functionp protect-function))
          (call-interactively protect-function))
      ;; Execute last command when miss match
      ;; user key alist.
      (when (and execute-last-command-when-miss-match
                 last-key)
        ;; Execute command corresponding last input key.
        (one-key-execute-binding-command last-key)))))

(defun one-key-execute-binding-command (key)
  "Execute the command binding KEY."
  (let ( ;; Try to get function corresponding `KEY'.
        (function (key-binding key)))
    ;; Execute corresponding command, except `keyboard-quit'.
    (when (and (not (eq function 'keyboard-quit))
               (functionp function))
      ;; Make sure `last-command-event' equal `last-input-event'.
      (setq last-command-event last-input-event)
      ;; Run function.
      (call-interactively function))))

(defun one-key-match-keystroke (key match-key)
  "Return `non-nil' if `KEY' match `MATCH-KEY'.
Otherwise, return nil."
  (cond ((stringp match-key) (setq match-key (read-kbd-macro match-key)))
        ((vectorp match-key) nil)
        (t (signal 'wrong-type-argument (list 'array match-key))))
  (equal key match-key))

(defun one-key-read-keymap (keystroke)
  "Read keymap.
If KEYSTROKE is a name of keymap, use the keymap.
Otherwise it is interpreted as a key stroke."
  (let ((v (intern-soft keystroke)))
    (if (and (boundp v) (keymapp (symbol-value v)))
        (symbol-value v)
      (key-binding (read-kbd-macro keystroke)))))

(defun one-key-handle-last (alternate-function recursion-function recursion-p)
  "The last process when match user keystroke or not match.
ALTERNATE-FUNCTION is the alternate function to be execute.
RECURSION-FUNCTION is the recursion function to be execute
when option RECURSION-P is non-nil."
  ;; Execute alternate function.
  (when (and alternate-function
             (functionp alternate-function))
    (call-interactively alternate-function))
  ;; Recursion execute when argument
  ;; `recursion-p' is `non-nil'.
  (if recursion-p
      (funcall recursion-function)))

(defun one-key-help-window-exist-p ()
  "Return `non-nil' if `one-key' help window exist.
Otherwise, return nil."
  (and (get-buffer one-key-buffer-name)
       (window-live-p (get-buffer-window (get-buffer one-key-buffer-name)))))

(defun one-key-help-window-toggle (title info-alist)
  "Toggle the help window.
Argument TITLE is title name for help information.
Argument INFO-ALIST is help information as format ((key . describe) . command)."
  (if (one-key-help-window-exist-p)
      ;; Close help window.
      (one-key-help-window-close)
    ;; Open help window.
    (one-key-help-window-open title info-alist)))

(defun one-key-help-window-open (title info-alist)
  "Open the help window.
Argument TITLE is title name for help information.
Argument INFO-ALIST is help information as format ((key . describe) . command)."
  ;; Save current window configuration.
  (or one-key-help-window-configuration
      (setq one-key-help-window-configuration (current-window-configuration)))
  ;; Generate buffer information.
  (unless (get-buffer one-key-buffer-name)
    (with-current-buffer (get-buffer-create one-key-buffer-name)
      (goto-char (point-min))
      (save-excursion
        (insert (one-key-highlight-help
                 title
                 (one-key-help-format info-alist))))))
  ;; Pop `one-key' buffer.
  (pop-to-buffer one-key-buffer-name)
  (set-buffer one-key-buffer-name)
  ;; Adjust height of help window
  ;; to display buffer's contents exactly.
  (fit-window-to-buffer nil one-key-help-window-max-height))

(defun one-key-help-window-close ()
  "Close the help window."
  ;; Kill help buffer.
  (when (bufferp (get-buffer one-key-buffer-name))
    (kill-buffer one-key-buffer-name))
  ;; Restore window layout if
  ;; `one-key-help-window-configuration' is valid value.
  (when (and one-key-help-window-configuration
             (boundp 'one-key-help-window-configuration))
    (set-window-configuration one-key-help-window-configuration)
    (setq one-key-help-window-configuration nil)))

(defun one-key-help-window-scroll-up ()
  "Scroll up one screen `one-key' help window."
  (if (one-key-help-window-exist-p)
      (ignore-errors
        (with-current-buffer one-key-buffer-name
          (scroll-up)))))

(defun one-key-help-window-scroll-down ()
  "Scroll down one screen `one-key' help window."
  (if (one-key-help-window-exist-p)
      (ignore-errors
        (with-current-buffer one-key-buffer-name
          (scroll-down)))))

(defun one-key-help-window-scroll-up-line ()
  "Scroll up one line `one-key' help window."
  (if (one-key-help-window-exist-p)
      (ignore-errors
        (with-current-buffer one-key-buffer-name
          (scroll-up 1)))))

(defun one-key-help-window-scroll-down-line ()
  "Scroll down one line `one-key' help window."
  (if (one-key-help-window-exist-p)
      (ignore-errors
        (with-current-buffer one-key-buffer-name
          (scroll-down 1)))))

(defun one-key-help-format (info-alist)
  "Format `one-key' help information.
Argument INFO-ALIST is help information as format ((key . describe) . command)."
  (let* ((max-length (loop for ((key . desc) . command) in info-alist
                           maximize (+ (string-width key) (string-width desc))))
         (current-length 0)
         (items-per-line (or one-key-items-per-line
                             (floor (/ (- (window-width) 3)
                                       (+ max-length 4)))))
         keystroke-msg)
    (loop for ((key . desc) . command) in info-alist
          for counter from 1  do
          (push (format "[%s] %s " key desc) keystroke-msg)
          (setq current-length (+ (string-width key) (string-width desc)))
          (push (if (zerop (% counter items-per-line))
                    "\n"
                  (make-string (- max-length current-length) ? ))
                keystroke-msg))
    (mapconcat 'identity (nreverse keystroke-msg) "")))

(defun add-to-alist (alist-var elt-cons &optional no-replace)
  "Add to the value of ALIST-VAR an element ELT-CONS if it isn't there yet.
If an element with the same car as the car of ELT-CONS is already present,
replace it with ELT-CONS unless NO-REPLACE is non-nil; if a matching
element is not already present, add ELT-CONS to the front of the alist.
The test for presence of the car of ELT-CONS is done with `equal'."
  (let ((existing-element (assoc (car elt-cons) (symbol-value alist-var))))
    (if existing-element
        (or no-replace
            (rplacd existing-element (cdr elt-cons)))
      (set alist-var (cons elt-cons (symbol-value alist-var)))))
  (symbol-value alist-var))

(defun one-key-make-template (keymap title)
  "Generate template code.
KEYMAP is keymap you want generate.
TITLE is title name that any string you like."
  (with-temp-buffer
    (let ((indent-tabs-mode t)
          (funcname (replace-regexp-in-string " " "-" title)))
      (insert (substitute-command-keys "\\<keymap>\\{keymap}"))
      ;; Remove header/footer
      (goto-char (point-min))
      (forward-line 3)
      (delete-region 1 (point))
      (goto-char (point-max))
      (backward-delete-char 1)
      ;; Insert.
      (goto-char (point-min))
      ;; Insert alist variable.
      (insert (format "(defvar one-key-menu-%s-alist nil\n\"The `one-key' menu alist for %s.\")\n\n"
                      funcname title)
              (format "(setq one-key-menu-%s-alist\n'(\n" funcname))
      ;; Insert (("key" . "desc") . command).
      (while (not (eobp))
        (unless (eq (point-at-bol) (point-at-eol))
          (destructuring-bind (key cmd)
              (split-string (buffer-substring (point-at-bol) (point-at-eol)) "\t+")
            (delete-region (point-at-bol) (point-at-eol))
            (insert (format "((\"%s\" . \"%s\") . %s)"
                            (replace-regexp-in-string
                             "\\\"" "\\\\\""
                             (replace-regexp-in-string "\\\\" "\\\\\\\\" key))
                            (capitalize (replace-regexp-in-string "-" " " cmd))
                            cmd))
            (when (and cmd
                       (string-match " " (concat key cmd)))
              (forward-sexp -1)
              (insert ";; "))))
        (forward-line 1))
      (goto-char (point-max))
      (insert "))\n\n")
      ;; Insert function.
      (insert (format "(defun one-key-menu-%s ()\n\"The `one-key' menu for %s\"\n(interactive)\n(one-key-menu \"%s\" one-key-menu-%s-alist))\n"
                      funcname title title funcname))
      ;; Indent.
      (emacs-lisp-mode)
      (indent-region (point-min) (point-max))
      ;; Result.
      (buffer-string)
      )))

(provide 'one-key)

;;; one-key.el ends here

;;; LocalWords:  emms specpdl minish DarkRed msg FUNCITN num str decf elt args
;;; LocalWords:  rubikitch's desc SPC bmenu sKeymap nsTitle fontify funcname
;;; LocalWords:  bol eol destructuring cmd PageUp PageDown
