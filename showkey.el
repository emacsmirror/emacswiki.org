;;; showkey.el --- Show keys as you use them.
;; 
;; Filename: showkey.el
;; Description: Show keys as you use them.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2014-2018, Drew Adams, all rights reserved.
;; Created: Sun Mar 22 16:24:39 2015 (-0700)
;; Version: 0
;; Package-Requires: ()
;; Last-Updated: Sat Feb 10 08:39:55 2018 (-0800)
;;           By: dradams
;;     Update #: 157
;; URL: https://www.emacswiki.org/emacs/download/showkey.el
;; Doc URL: https://www.emacswiki.org/emacs/ShowKey
;; Keywords: help keys mouse
;; Compatibility: GNU Emacs: 23.x, 24.x, 25.x, 26.x
;; 
;; Features that might be required by this library:
;;
;;   `fit-frame'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;;    Show key and mouse events and other events as you use them.
;;
;;  There are two ways to show them:
;;
;;  * Show the last key used, in a tooltip.  This is refreshed with
;;    each such event.  For this you use global minor mode
;;    `showkey-tooltip-mode'.
;;
;;  * Show a log of such events, in a separate frame.  It is refreshed
;;    with each event, and it is kept on top of other frames without
;;    stealing the input focus.  For this you use global minor mode
;;    `showkey-log-mode'.
;;
;;  Events that raise an error are not shown.
;;
;;  Several user options control the behavior:
;;
;;  * `showkey-log-frame-alist' is an alist of frame parameters for
;;    the logging frame.  (It is not used for `showkey-tooltip-mode'.)
;;
;;  * `showkey-log-erase-keys' is a list of keys that will each
;;    restart logging, that is, erase the log and start it over.  (It
;;    is not used for `showkey-tooltip-mode'.)
;;
;;  * `showkey-tooltip-height' is the height of the tooltip text, in
;;    units of 1/10 point.  The default value is 100, meaning 10pts.
;;
;;  * `showkey-tooltip-ignored-events' and
;;    `showkey-log-ignored-events' are each a list of regexps to match
;;    against events that you do not want to show, for
;;    `showkey-tooltip-mode' and `showkey-log-mode', respectively.
;;
;;  * `showkey-tooltip-key-only-flag' non-nil means show only the key
;;    used, not also its description.  The default value is nil.
;;
;;  * `showkey-tooltip-sleep-time' is the number of seconds to pause
;;    while showing the tooltip.  This is zero by default, but you
;;    might want to use a positive integer when playing back a
;;    recorded keyboard macro.
;;
;;  * `showkey-tooltip-timeout' is the number of seconds to show the
;;    tooltip, before hiding it.  It is also hidden upon any user
;;    event, such as hitting another key, but it is always shown for
;;    at least `showkey-tooltip-sleep-time' seconds.
;;
;;
;;
;;  Commands defined here:
;;
;;    `showkey-log-mode', `showkey-tooltip-mode'.
;;
;;  User options defined here:
;;
;;    `showkey-log-erase-keys', `showkey-log-frame-alist',
;;    `showkey-tooltip-height', `showkey-log-ignored-events',
;;    `showkey-tooltip-ignored-events',
;;    `showkey-tooltip-key-only-flag', `showkey-tooltip-timeout'.
;;
;;  Faces defined here:
;;
;;    `showkey-log-latest'.
;;
;;  Non-interactive functions defined here:
;;
;;    `showkey-log', `showkey-show-tooltip', `showkey-some'.
;;
;;  Internal variables defined here:
;;
;;    `showkey-insert-cmds', `showkey-last-key-desc',
;;    `showkey-log-frame', `showkey-log-overlay',
;;    `showkey-nb-consecutives'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;;
;; 2018/02/10 dadams
;;     showkey-show-tooltip:
;;       Use selected-frame, not nil, as second arg to x-show-tip.  See Emacs bug #30399.
;; 2017/12/27 dadams
;;     Added: showkey-tooltip-sleep-time.
;;     showkey-show-tooltip: Use showkey-tooltip-sleep-time.
;; 2016/10/27 dadams
;;     Added: showkey-tooltip-timeout.
;;     showkey-show-tooltip: Use showkey-tooltip-timeout.
;; 2016/08/16 dadams
;;     showkey-log-frame-alist: Commented out font spec.
;; 2016/07/04 dadams
;;     showkey-log: Respect showkey-tooltip-key-only-flag too.
;; 2015/05/27 dadams
;;     Added: showkey-tooltip-height, showkey-tooltip-key-only-flag.
;;     showkey-show-tooltip: Respect those new options.
;; 2015/03/22 dadams
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
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

(require 'fit-frame nil t) ; fit-frame

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defgroup Show-Key nil
  "Show keys as you use them."
  :prefix "showkey-" :group 'help
  :link `(url-link :tag "Send Bug Report"
                   ,(concat "mailto:" "drew.adams" "@" "oracle" ".com?subject=\
showkey.el bug: \
&body=Describe bug here, starting with `emacs -q'.  \
Don't forget to mention your Emacs and library versions."))
  :link '(url-link :tag "Other Libraries by Drew"
          "https://www.emacswiki.org/emacs/DrewsElispLibraries")
  :link '(url-link :tag "Download" "https://www.emacswiki.org/emacs/download/showkey.el")
  :link '(url-link :tag "Description" "https://www.emacswiki.org/emacs/ShowKey")
  :link '(emacs-commentary-link :tag "Commentary" "showkey"))

(defface showkey-log-latest '((t (:foreground "Red")))
  "*Face for latest event logged in buffer `*KEYS*'."
  :group 'Show-Key)

(defcustom showkey-log-erase-keys ()  ;; E.g., '(?\r)
  "List of keys that erase buffer `*KEYS*', so key logging starts over.
For example, if the value is `(?\r)' then `RET' restarts logging.
This is used by `showkey-log-mode'."
  :type '(repeat character) :group 'Show-Key)

(defcustom showkey-log-frame-alist
  `((top  . 50)
    (left . -1)
    (width . 65)
    (menu-bar-lines . 0)
    (tool-bar-lines . 0)
    (minibuffer)
    (left-fringe . 0)
    (right-fringe . 0)
    (name . "*KEYS*")
    (unsplittable . t)
    (user-position . t)
    (horizontal-scroll-bars)
    (vertical-scroll-bars . right)
    (background-color . "LightSteelBlue")
    ;; Choose whatever font you like.  This is what I use:
    ;; (font . "-*-Lucida Console-normal-r-*-*-12-*-*-*-c-*-iso8859-1")
    )
  "Alist of frame parameters for the `*KEYS*' frame of `showkey-log-mode'."
  :type 'alist :group 'Show-Key)

(defcustom showkey-log-ignored-events '("^<mouse-movement>")
  "List of strings naming events to ignore for `showkey-log-mode'.
These events are not logged in buffer `*KEYS*'.

Each string is used as a regexp to match the user-friendly description
of an event.  It should be `^' followed by the event name enclosed in
angle brackets.  Example: \"^<mouse-movement>\"."
  :type '(repeat string) :group 'Show-Key)

(defcustom showkey-tooltip-height 100
  "The height of the tooltip text, in units of 1/10 point."
  :type '(restricted-sexp
          :match-alternatives ((lambda (x) (and (integerp x)  (> x 0))))
          :value 100)
  :group 'Show-Key)

(defcustom showkey-tooltip-ignored-events '("^<mouse-movement>"
                                            "^<vertical-scroll-bar>"
                                            "^<horizontal-scroll-bar>")
  "List of strings naming events to ignore for `showkey-tooltip-mode'.
These events are not logged in buffer `*KEYS*'.

Each string is used as a regexp to match the user-friendly description
of an event.  It should be `^' followed by the event name enclosed in
angle brackets.  Example: \"^<mouse-movement>\"."
  :type '(repeat string) :group 'Show-Key)

(defcustom showkey-tooltip-key-only-flag nil
  "Non-nil means show only the key used, not also its description."
  :type 'boolean :group 'Show-Key)

(defcustom showkey-tooltip-sleep-time 0
  "Minimum number of seconds to pause while showing the tooltip."
  :type 'integer :group 'Show-Key)

(defcustom showkey-tooltip-timeout 5
  "Hide tooltip after this many seconds.
It is also hidden upon any user event, such as hitting another key,
but it is always shown for at least `showkey-tooltip-sleep-time'
seconds."
  :type 'integer :group 'Show-Key)

(defvar showkey-nb-consecutives 1
  "Counter of how many times the current key has been pressed.")

(defvar showkey-insert-cmds '(self-insert-command icicle-self-insert)
  "List of commands to treat as self-inserting.")

(defvar showkey-last-key-desc "" "`key-description' of last key used.")

(defvar showkey-log-frame nil
  "Frame used for logging keys.")

(defvar showkey-log-overlay nil
  "Overlay for latest event logged in buffer `*KEYS*'.")


;;;###autoload
(define-minor-mode showkey-tooltip-mode
    "Global minor mode that logs the keys you use.
See option `showkey-tooltip-ignored-events' for customization.

Note that keys such as `C-g' that quit, and keys that raise an error,
are not logged."
  nil nil nil :global t
  (if showkey-tooltip-mode
      (add-hook 'pre-command-hook 'showkey-show-tooltip 'APPEND)
    (x-hide-tip)
    (remove-hook 'pre-command-hook 'showkey-show-tooltip)))

(defun showkey-show-tooltip ()
  "Global minor mode that shows the keys you use in a tooltip.
See options `showkey-tooltip-height',
`showkey-tooltip-ignored-events', `showkey-tooltip-key-only-flag',
`showkey-tooltip-sleep-time', and `showkey-tooltip-timeout'.

Note that keys such as `C-g' that quit, and keys that raise an error,
are not indicated."
  (let* ((key       (this-single-command-keys))
         (key-desc  (key-description key)))
    (unless (or (equal [] key)
                (showkey-some showkey-tooltip-ignored-events key-desc #'string-match-p))
      (let* ((event      (if (and (symbolp (aref key 0))
                                  (> (length key) 1)
                                  (consp (aref key 1)))
                             (aref key 1)
                           (aref key 0)))
             (modifiers  (event-modifiers event))
             (mouse-msg  (if (or (memq 'click modifiers)
                                 (memq 'down modifiers)
                                 (memq 'drag modifiers))
                             " at that spot"
                           ""))
             (cmd-desc   (format "%s" key-desc)))
        (unless showkey-tooltip-key-only-flag
          (setq cmd-desc  (format "%s%s runs the command `%S'"
                                  cmd-desc mouse-msg (key-binding key t))))
        ;; Accumulate self-inserting chars, with no command descriptions.
        ;; Otherwise, show command description.
        (cond ((member this-command showkey-insert-cmds)
               (when (equal key-desc "SPC") (setq key-desc  " "))
               (unless (member last-command showkey-insert-cmds)
                 (setq showkey-last-key-desc  ""))
               (setq cmd-desc               (concat showkey-last-key-desc key-desc)
                     showkey-last-key-desc  (concat showkey-last-key-desc key-desc)))
              ((eq this-command last-command)
               (setq cmd-desc                 (format "[%d]%s"
                                                      showkey-nb-consecutives cmd-desc)
                     showkey-nb-consecutives  (1+ showkey-nb-consecutives)
                     showkey-last-key-desc    key-desc))
              (t
               (setq showkey-nb-consecutives  1
                     showkey-last-key-desc    key-desc)))
        (let* ((x.y  (posn-actual-col-row (posn-at-point (point))))
               (x    (car x.y))
               (y    (cdr x.y)))
          (when (string= "" mouse-msg)
            (set-mouse-position (selected-frame) (+ 3 x) (+ 2 y))))
        (x-show-tip (propertize cmd-desc
                                'face `(:foreground "red" :height ,showkey-tooltip-height))
                    (selected-frame)    ; Emacs bug #30399 says nil is broken for Emacs < 27
                    nil
                    showkey-tooltip-timeout)
        (sleep-for showkey-tooltip-sleep-time)))))

;;;###autoload
(define-minor-mode showkey-log-mode
    "Global minor mode that logs the keys you use.
See options `showkey-log-erase-keys', `showkey-log-ignored-events',
and `showkey-log-frame-alist' for customization.

Note that keys such as `C-g' that quit, and keys that raise an error,
are not logged."
  nil nil nil :global t
  (cond (showkey-log-mode
         (unless (get-buffer "*KEYS*")
           (let ((pop-up-frames                t)
                 (oframe                       (selected-frame))
                 (default-frame-alist          showkey-log-frame-alist)
                 (special-display-frame-alist  showkey-log-frame-alist)
                 (w32-grab-focus-on-raise      nil))
             (save-selected-window
               (with-help-window "*KEYS*"
                 (with-current-buffer "*KEYS*"
                   (insert "************ KEYS ************\n\n")))
               (setq showkey-log-frame  (selected-frame)))
             (select-frame-set-input-focus oframe)
             (raise-frame showkey-log-frame))
           (add-hook 'pre-command-hook 'showkey-log 'APPEND)))
        (t
         (when (get-buffer "*KEYS*")
           (remove-hook 'pre-command-hook 'showkey-log)
           (kill-buffer "*KEYS*")))))

(defun showkey-log ()
  "Log the current key in buffer `*KEYS*'."
  (let* ((key       (this-single-command-keys))
         (key-desc  (key-description key)))
    (unless (or (equal [] key)
                (showkey-some showkey-log-ignored-events key-desc #'string-match-p)
                (not (get-buffer-window "*KEYS*" 0)))
      (let* ((event                    (if (and (symbolp (aref key 0))
                                                (> (length key) 1)
                                                (consp (aref key 1)))
                                           (aref key 1)
                                         (aref key 0)))
             (modifiers                (event-modifiers event))
             (mouse-msg                (if (or (memq 'click modifiers)
                                               (memq 'down modifiers)
                                               (memq 'drag modifiers))
                                           " at that spot"
                                         ""))
             (cmd-desc                 (if showkey-tooltip-key-only-flag
                                           (format "%s" key-desc)
                                         (format "%s%s runs the command `%S'"
                                                 key-desc mouse-msg (key-binding key t))))
             (w32-grab-focus-on-raise  nil))
        (save-selected-window
          (select-window (get-buffer-window "*KEYS*" 0))
          (setq showkey-log-frame  (selected-frame))
          (raise-frame showkey-log-frame)
          (with-current-buffer "*KEYS*"
            (let ((inhibit-read-only  t))
              (goto-char (point-max))
              (if (memq (aref key 0) showkey-log-erase-keys)
                  ;; Key in `showkey-log-erase-keys', so erase buffer.
                  (progn (insert "\n") (insert key-desc) (sit-for 1) (erase-buffer)
                         (insert "************ KEYS ************\n\n"))
                ;; Keep self-inserting chars on same line.  Otherwise, start a new line.
                (cond ((member this-command showkey-insert-cmds)
                       (unless (member last-command showkey-insert-cmds) (insert "\n"))
                       (insert (if (equal key-desc "SPC") " " key-desc)))
                      (t
                       (insert "\n")
                       (cond ((eq this-command last-command)
                              (save-excursion
                                (forward-line -1)
                                (setq showkey-nb-consecutives  (1+ showkey-nb-consecutives))
                                (if (looking-at "^\[[0-9]+\]")
                                    (replace-match (format "[%d]" showkey-nb-consecutives))
                                  (insert (format "[%d]" showkey-nb-consecutives))))
                              (backward-delete-char 1)) ; the \n
                             (t
                              (setq showkey-nb-consecutives  1)
                              (insert cmd-desc)))))
                (let ((bol  (line-beginning-position))
                      (eol  (line-end-position)))
                  (if showkey-log-overlay ; Don't re-create it.
                      (move-overlay showkey-log-overlay bol eol (current-buffer))
                    (setq showkey-log-overlay  (make-overlay bol eol))
                    (overlay-put showkey-log-overlay 'face 'showkey-log-latest)))
                (when (fboundp 'fit-frame)
                  (with-selected-window (get-buffer-window "*KEYS*" 0) (fit-frame)))))))))))

;; Same as `icicle-some'.
(defun showkey-some (list arg2 predicate)
  "Apply binary PREDICATE successively to ARG1 and an item of LIST.
Return the first non-nil value returned by PREDICATE, or nil if none.
PREDICATE must be a function with two required arguments."
  (let ((result  nil))
    (catch 'icicle-some
      (dolist (arg1  list)
        (when (setq result  (funcall predicate arg1 arg2))  (throw 'icicle-some result))))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'showkey)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; showkey.el ends here
