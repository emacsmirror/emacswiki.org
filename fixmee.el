;;; fixmee.el --- Quickly navigate to FIXME notices in code
;;
;; Copyright (c) 2012 Roland Walker
;;
;; Author: Roland Walker <walker@pobox.com>
;; Homepage: http://github.com/rolandwalker/fixmee
;; URL: http://raw.github.com/rolandwalker/fixmee/master/fixmee.el
;; Version: 0.7.0
;; Last-Updated: 30 Aug 2012
;; EmacsWiki: FixmeeMode
;; Keywords: navigation, convenience
;; Package-Requires: ((button-lock 0.9.8) (nav-flash "1.0.0") (back-button "0.6.0") (smartrep "0.0.3"))
;;
;; Simplified BSD License
;;
;;; Commentary:
;;
;; Fixmee-mode tracks "fixme" notices in code comments, highlights
;; them, ranks them by urgency, and lets you navigate to them quickly.
;;
;; Urgency of "fixme" notices is indicated by repeating the final
;; character.  For example, one might write "FIXMEEEEEEEEE" for an
;; important issue.
;;
;; Quickstart:
;;
;; Add the following to your ~/.emacs
;;
;;     (require 'fixmee)
;;     (global-fixmee-mode 1)
;;
;; Then, open some buffers and right-click on the word "fixme" in a
;; comment
;;
;; or press
;;
;;     C-c f
;;
;; or
;;
;;     M-x fixmee RET
;;
;; Key Bindings:
;;
;; The default key bindings are
;;
;;     C-c f `fixmee-goto-nextmost-urgent'
;;     C-c F `fixmee-goto-prevmost-urgent'
;;     M-n   `fixmee-goto-next-by-position'      ; only when the point is
;;     M-p   `fixmee-goto-previous-by-position'  ; inside a fixme notice
;;
;; When the smartrep package is installed, the "C-c" prefix need not
;; be used for consecutive fixmee-mode keyboard commands.  Instead,
;; just keep pressing "f" (or whichever key you set in customize).
;;
;; There is also a context menu and mouse-wheel bindings on the
;; minor-mode lighter in the modeline:
;;
;;             mouse-1   context menu
;;       wheel-down/up   next/prev by urgency
;;     M-wheel-down/up   next/prev by position
;;
;; Patterns:
;;
;; The following fixme patterns are supported by default:
;;
;;     @@@
;;     XXX             ; only this one is case-sensitive
;;     todo
;;     fixme
;;
;; See Also
;;
;;     M-x customize-group RET fixmee RET
;;     M-x customize-group RET nav-flash RET
;;
;; Notes
;;
;;     Currently, only open buffers are searched, not files or
;;     projects.
;;
;; Compatibility and Requirements
;;
;;     Tested only on GNU Emacs version 24.1
;;
;;     Requires button-lock.el
;;
;;     Uses if present: smartrep.el, nav-flash.el, back-button.el
;;
;; Bugs
;;
;;    When comment-start is defined, only the first notice on a line
;;    is lit up by button-lock, though fixme-mode is aware of multiple
;;    notices on a line.  This is worked around for the moment by
;;    stripping these cases from fixmee-notice-list.  Better would be
;;    to add comment-sensitivity logic to button-lock, and remove the
;;    comment-matching section of the regexp passed to button-lock.
;;
;;    Fixmee-maybe-turn-on gets called multiple times when a file
;;    is loaded.
;;
;;    Fixmee-buffer-include-functions may not contain the function
;;    'frame-bufs-associated-p, because a new buffer is not yet
;;    associated with the frame at the time the global mode check
;;    calls fixmee-maybe-turn-on.
;;
;; TODO
;;
;;    Better feedback messages for end-of-list and start-of-list.
;;
;;    Integrate with next-error (make a separate buffer showing hits) -
;;    for first pass at this just send regexp to occur, second pass
;;    build a buffer from the contents of fixmee-notice-list.
;;
;;    Consider changing prefix arg to mean "constrain this command to
;;    the current file".
;;
;;    Bookmark integration? (implicit bookmarking on notices).
;;
;;    Wrap/cycle options on navigation-by-position.
;;
;;    How to get last-command when user does M-x? (smex is not helping
;;    here).  (nth 0 command-history) ?
;;
;;    Navigation can land on line near vertical edge of window -
;;    should respect user settings and scroll in as needed for
;;    context.
;;
;;    Project support.
;;
;;    Some kind of extra comment indicating a notice is to be ignored?
;;    Lead with a backwhack?
;;
;;; License
;;
;;    Simplified BSD License
;;
;;    Copyright (c) 2012, Roland Walker
;;    All rights reserved.
;;
;;    Redistribution and use in source and binary forms, with or
;;    without modification, are permitted provided that the following
;;    conditions are met:
;;
;;       1. Redistributions of source code must retain the above
;;          copyright notice, this list of conditions and the following
;;          disclaimer.
;;
;;       2. Redistributions in binary form must reproduce the above
;;          copyright notice, this list of conditions and the following
;;          disclaimer in the documentation and/or other materials
;;          provided with the distribution.
;;
;;    This software is provided by Roland Walker "AS IS" and any express
;;    or implied warranties, including, but not limited to, the implied
;;    warranties of merchantability and fitness for a particular
;;    purpose are disclaimed.  In no event shall Roland Walker or
;;    contributors be liable for any direct, indirect, incidental,
;;    special, exemplary, or consequential damages (including, but not
;;    limited to, procurement of substitute goods or services; loss of
;;    use, data, or profits; or business interruption) however caused
;;    and on any theory of liability, whether in contract, strict
;;    liability, or tort (including negligence or otherwise) arising in
;;    any way out of the use of this software, even if advised of the
;;    possibility of such damage.
;;
;;    The views and conclusions contained in the software and
;;    documentation are those of the authors and should not be
;;    interpreted as representing official policies, either expressed
;;    or implied, of Roland Walker.
;;
;;; Code:
;;

;;; requires

;; for caddr, cadddr, incf, decf, callf, callf2, remove-if-not, position
(eval-when-compile
  (defvar button-lock-mode)
  (require 'cl))

(require 'nav-flash   nil t)
(require 'back-button nil t)
(require 'smartrep    nil t)

(autoload 'button-lock-mode       "button-lock"  "Toggle button-lock-mode, a minor mode for making text clickable.")
(autoload 'button-lock-set-button "button-lock"  "Attach mouse actions to text via `font-lock-mode'.")

(declare-function smartrep-define-key                     "smartrep.el")
(declare-function back-button-push-mark                   "back-button.el")
(declare-function back-button-push-mark-local-and-global  "back-button.el")
(declare-function remove-if-not                           "cl-seq.el")
(declare-function position                                "cl-seq.el")
(declare-function button-lock-called-interactively-p      "button-lock.el")
(declare-function button-lock-unset-button                "button-lock.el")
(declare-function button-lock-extend-binding              "button-lock.el")

;;; customizable variables

;;;###autoload
(defgroup fixmee nil
  "Navigate to \"fixme\" notices in code."
  :version "0.7.0"
  :link '(emacs-commentary-link "fixmee")
  :prefix "fixmee-"
  :group 'navigation
  :group 'extensions)

(defcustom fixmee-notice-regexp "\\(@@@+\\|\\_<\\(?:[Tt][Oo][Dd][Oo]+\\|[Ff][Ii][Xx][Mm][Ee]+\\|XXX+\\)\\_>\\)"
  "Pattern for matching \"fixme\" notices.

There must be one parenthesized grouping which captures the
\"fixme\" text exactly.  The captured text must be at minimum
three characters long.

The pattern will only be applied within comments for any mode
that defines comments in its syntax table."
  :type 'regexp
  :group 'fixmee)

(defcustom fixmee-mode-lighter " fixm"
  "This string appears in the mode-line when `fixmee-mode' is active.

Set to nil or the empty string to disable the mode-line
lighter for `fixmee-mode'."
  :type 'string
  :risky t
  :group 'fixmee)

(defcustom fixmee-less-feedback nil
  "Give less echo area feedback."
  :type 'boolean
  :group 'fixmee)

(defcustom fixmee-cache-refresh-interval 60
  "Force clearing of cached data after this many minutes.

To disable cache refresh, set this value to 0 or nil."
  :type 'integer
  :group 'fixmee)

(defcustom fixmee-push-mark t
  "Whether to set the global mark before a series of `fixmee-mode' navigation commands.

When this option is set, `pop-global-mark' (typically bound to
C-x C-SPC) will return the cursor to the starting point after
a series of `fixmee-mode' navigation commands."
  :type 'boolean
  :group 'fixmee)

(defface fixmee-notice-face
   '((t (:inherit font-lock-warning-face)))
  "Face to show fixmee notices"
  :group 'fixmee)

;;;###autoload
(defgroup fixmee-global nil
  "Settings for `global-fixmee-mode'."
  :group 'fixmee)

(defcustom fixmee-exclude-modes '(
                                  fundamental-mode
                                  Buffer-menu-mode
                                  bm-show-mode
                                  dired-mode
                                  eshell-mode
                                  gnus-article-mode
                                  mime/viewer-mode
                                  rmail-mode
                                  term-mode
                                  )
  "Fixmee will not scan a buffer if its major mode is included in this list."
  :type '(repeat symbol)
  :group 'fixmee-global)

(defcustom fixmee-buffer-name-exclude-pattern "\\`[* ]"
  "Fixmee will not scan a buffer if its name matches this regular expression.

The default pattern is designed to match buffers which are
programatically generated or internal to Emacs."
  :type 'regexp
  :group 'fixmee-global)

(defcustom fixmee-buffer-maximum-size 256000
  "Fixmee will not scan a buffer if it is larger than this size.

The size is measured in characters.

Set the value to 0 to disable size limits."
  :type 'integer
  :group 'fixmee-global)

(defcustom fixmee-buffer-include-functions '(buffer-file-name)
  "Fixmee will only scan buffers for which all functions evaluate to non-nil.

Each function should take a single argument (a buffer).  The
default filter causes fixmee mode to consider only buffers which
are associated with a file.

Set this value to nil to disable."
  :type '(repeat function)
  :group 'fixmee-global)

(defcustom fixmee-buffer-exclude-functions '()
  "Fixmee will not scan buffers for which any functions evaluate to non-nil.

Each function should take a single argument (a buffer).

Set this value to nil to disable."
  :type '(repeat function)
  :group 'fixmee-global)

;;;###autoload
(defgroup fixmee-keys nil
  "Key bindings for `fixmee-mode'."
  :group 'fixmee)

(defcustom fixmee-smartrep-prefix "C-c"
  "Prefix key for smartrep.el bindings.

Smartrep bindings will be installed for all `fixmee-mode' key
bindings which match this prefix.

The format for key sequences is as defined by `kbd'.

Set to nil or the empty string to disable smartrep for
`fixmee-mode'."
  :type 'string
  :group 'fixmee-keys)

(defcustom fixmee-goto-nextmost-urgent-keystrokes '("C-c f")
  "Key sequences to search for a \"fixme\" notice.

These keys are in effect whenever `fixmee-mode' is active in a
buffer.

These keys will navigate to the most urgent notice as
defined by length.  If pressed multiple times in succession,
navigate to successively less-urgent notices.

Once any other command is used, `fixmee-mode' will forget your
place the list of notices and begin again at the most-urgent
notice.

Note that \"next\" here means next-by-urgency and not by
position.

The format for key sequences is as defined by `kbd'."
  :type '(repeat string)
  :group 'fixmee-keys)

(defcustom fixmee-goto-prevmost-urgent-keystrokes '("C-c F")
  "Key sequences to search \"upward\" in the urgency ranking.

\"Upward\" means up in rankings, toward a more-urgent notice.

These keys are in effect whenever `fixmee-mode' is active in a
buffer.

These keys can only be used to back up after descending a series
of \"fixme\" notices via `fixmee-goto-nextmost-urgent-keystrokes'.

Once any other command is used, `fixmee-mode' will forget your
place the list of notices and begin again at the least-urgent
notice.

Note that \"previous\" here means previous-by-urgency and not
by position.

The format for key sequences is as defined by `kbd'."
   :type '(repeat string)
   :group 'fixmee-keys)

(defcustom fixmee-goto-next-by-position-keystrokes nil
  "Key sequences to search forward in the same buffer for \"fixme\" notices.

These keys are in effect whenever `fixmee-mode' is active in a
buffer.

The format for key sequences is as defined by `kbd'."
   :type '(repeat string)
   :group 'fixmee-keys)

(defcustom fixmee-goto-previous-by-position-keystrokes nil
  "Key sequences to search backward in the same buffer for \"fixme\" notices.

These keys are in effect whenever `fixmee-mode' is active in a
buffer.

The format for key sequences is as defined by `kbd'."
   :type '(repeat string)
   :group 'fixmee-keys)

(defcustom fixmee-within-notice-mouse-1-command nil
  "Command bound to mouse-1 when clicking on \"fixme\" notices.

The format for key sequences is as defined by `kbd'."
   :type 'symbol
   :group 'fixmee-keys)

(defcustom fixmee-within-notice-mouse-2-command 'ignore
  "Command bound to mouse-2 when clicking on \"fixme\" notices.

The format for key sequences is as defined by `kbd'."
   :type 'symbol
   :group 'fixmee-keys)

(defcustom fixmee-within-notice-down-mouse-3-command 'fixmee-notice-popup
  "Command bound to down-mouse-3 when clicking on \"fixme\" notices.

The format for key sequences is as defined by `kbd'."
   :type 'symbol
   :group 'fixmee-keys)

(defcustom fixmee-within-notice-goto-next-by-position-keystrokes '("M-n")
  "Key sequences to search forward in the same buffer for \"fixme\" notices.

These keys are only in effect when the point is inside a
\"fixme\" notice.

The format for key sequences is as defined by `kbd'."
   :type '(repeat string)
   :group 'fixmee-keys)

(defcustom fixmee-within-notice-goto-previous-by-position-keystrokes '("M-p")
  "Key sequences to search backward in the same buffer for \"fixme\" notices.

These keys are only in effect when the point is inside a
\"fixme\" notice.

The format for key sequences is as defined by `kbd'."
   :type '(repeat string)
   :group 'fixmee-keys)

;;; variables

(defvar fixmee-pristine-buffer-list nil
  "List of buffers unmodified since the last execution of `fixmee-locate-all-notices'.")

(defvar fixmee-last-locate-state nil
  "Status of all buffers at the last execution of `fixmee-locate-all-notices'.")

(defvar fixmee-last-good-hit nil
  "The last successful `fixmee-mode' navigation.

Expressed as an element of `fixmee-notice-list'.")

(defvar fixmee-cache-refresh-timer nil
  "A timer object for periodic cache invalidation.")

(defvar fixmee-notice-list nil
  "Global list of \"fixme\" notices.  Each element is a list (URGENCY BUFFER LOCATION).")

(defvar fixmee-navigation-commands  '(
                                      fixmee                          ; alias for fixmee-goto-nextmost-urgent
                                      fixmee-goto-prevmost-urgent
                                      fixmee-goto-nextmost-urgent
                                      fixmee-goto-previous-by-position
                                      fixmee-goto-next-by-position
                                      )
  "List of interactive navigation commands.")

(defvar fixmee-button nil
  "Buffer-local variable holding the `button-lock' button for \"fixme\" notices.")
(make-variable-buffer-local 'fixmee-button)

(defvar fixmee-lighter-context-menu-keystrokes "<mode-line> <down-mouse-1>"
  "Key sequence to invoke the modeline context menu.")

;;; keymaps

(defvar fixmee-mode-map (make-sparse-keymap) "Keymap for `fixmee-mode' minor-mode.")

(let ((fixmee-navigation-commands (remq 'fixmee fixmee-navigation-commands)))
  (if (and (stringp fixmee-smartrep-prefix)
           (length fixmee-smartrep-prefix))
      (let ((keys nil))
        (dolist (cmd fixmee-navigation-commands)
          (dolist (k (remove-if-not #'(lambda (x)
                                        (string-match-p (concat "\\`" fixmee-smartrep-prefix "\\>") x))
                                    (symbol-value (intern (concat (symbol-name cmd) "-keystrokes")))))
            (push (cons (replace-regexp-in-string (concat "\\`" fixmee-smartrep-prefix "\\>[ \t]*") "" k) cmd) keys)))
        (smartrep-define-key fixmee-mode-map fixmee-smartrep-prefix keys))
    ;; else
    (dolist (cmd fixmee-navigation-commands)
      (dolist (k (symbol-value (intern (concat (symbol-name cmd) "-keystrokes"))))
        (define-key fixmee-mode-map (read-kbd-macro k) cmd)))))

;;; lighter

(defvar fixmee-lighter-map  (let ((map (make-sparse-keymap))
                                  (menu-map (make-sparse-keymap "Fixmee Mode")))
                              (define-key menu-map [customize]                         '(menu-item "Customize"      (lambda (e) (interactive "e") (customize-group 'fixmee))))
                              (define-key menu-map [turn-off-fixmee-mode]              '(menu-item "Turn Off Fixmee Mode"  fixmee-mode))
                              (define-key menu-map [separator-1]                       '(menu-item "--"))
                              (define-key menu-map [fixmee-goto-previous-by-position]  '(menu-item "Previous Fixme By Position" fixmee-goto-previous-by-position))
                              (define-key menu-map [fixmee-goto-next-by-position]      '(menu-item "Next Fixme By Position" fixmee-goto-next-by-position))
                              (define-key menu-map [fixmee-goto-prevmost-urgent]       '(menu-item "Previous Fixme By Urgency"  fixmee-goto-prevmost-urgent))
                              (define-key menu-map [fixmee-goto-nextmost-urgent]       '(menu-item "Next Fixme By Urgency"  fixmee-goto-nextmost-urgent))
                              (define-key map (kbd "<mode-line> <wheel-up>"     )      'fixmee-goto-prevmost-urgent)
                              (define-key map (kbd "<mode-line> <wheel-down>"   )      'fixmee-goto-nextmost-urgent)
                              (define-key map (kbd "<mode-line> <M-wheel-up>"   )      'fixmee-goto-previous-by-position)
                              (define-key map (kbd "<mode-line> <M-wheel-down>" )      'fixmee-goto-next-by-position)
                              (define-key map (read-kbd-macro fixmee-lighter-context-menu-keystrokes) menu-map)
                              map) "Keymap for the `fixmee-mode' lighter.")

(when (stringp fixmee-mode-lighter)
      (callf propertize fixmee-mode-lighter 'local-map fixmee-lighter-map
                                            'help-echo "fixmee-mode: mouse-3 menu\nwheel down/up by urgency\nwheel M-down/M-up by position."))

;;; aliases and fsets

(defalias 'fixmee 'fixmee-goto-nextmost-urgent)

;;; compatibility functions

(unless (fboundp 'back-button-push-mark-local-and-global)
  (fset 'back-button-push-mark (symbol-function 'push-mark))
  (defun back-button-push-mark-local-and-global (&optional location nomsg activate consecutives)
  "Push mark at LOCATION, and unconditionally add to `global-mark-ring'.

This function differs from `push-mark' in that `global-mark-ring'
is always updated.

LOCATION is optional, and defaults to the current point.

NOMSG and ACTIVATE are as documented at `push-mark'.

When CONSECUTIVES is set to 'limit and the new mark is in the same
buffer as the first entry in `global-mark-ring', the first entry
in `global-mark-ring' will be replaced.  Otherwise, a new entry
is pushed onto `global-mark-ring'.

When CONSECUTIVES is set to 'allow-dupes, it is possible to push
an exact duplicate of the current topmost mark onto `global-mark-ring'."
  (callf or location (point))
  (back-button-push-mark location nomsg activate)
  (when (or (eq consecutives 'allow-dupes)
            (not (equal (mark-marker)
                        (car global-mark-ring))))
    (when (and (eq consecutives 'limit)
               (eq (marker-buffer (car global-mark-ring)) (current-buffer)))
      (move-marker (car global-mark-ring) nil)
      (pop global-mark-ring))
    (push (copy-marker (mark-marker)) global-mark-ring)
    (when (> (length global-mark-ring) global-mark-ring-max)
      (move-marker (car (nthcdr global-mark-ring-max global-mark-ring)) nil)
      (setcdr (nthcdr (1- global-mark-ring-max) global-mark-ring) nil)))))

;;; utility functions

;; general functions
(defun fixmee-refresh-timer-setup ()
  "Set up a timer to invalidate caches.

This should help cover up various minor bugs, such as not
invalidating the cache when the regexp is changed."
  (when (timerp fixmee-cache-refresh-timer)
    (cancel-timer fixmee-cache-refresh-timer)
    (setq fixmee-cache-refresh-timer nil))
  (when fixmee-cache-refresh-interval
    (let ((secs (truncate (* 60 fixmee-cache-refresh-interval))))
      (when (> secs 0)
        (setq fixmee-cache-refresh-timer (run-with-timer secs secs 'fixmee-cache-invalidate))))))

(defun fixmee-cache-invalidate ()
  "Delete all cached data."
  (setq fixmee-last-locate-state nil)
  (setq fixmee-notice-list nil)
  (setq fixmee-pristine-buffer-list nil))

;; buffer functions
(defun fixmee-get-buffer-state () ;; optimization
  "Return a data structure we can use to test if buffers have changed."
  (list (sort
         (remove-if-not #'(lambda (buf)
                            (buffer-local-value 'fixmee-mode buf)) (buffer-list))
         #'(lambda (a b) (string< (buffer-name a) (buffer-name b))))
        fixmee-pristine-buffer-list))

(defun fixmee-this-buffer-not-pristine-hook (&optional beg end len)
  "Add BUFFER to the list modified since the last search for \"fixme\" notices.

Arguments BEG, END, and LEN are as passed to an `after-change-functions'
hook, and are ignored."
  (remove-hook 'after-change-functions 'fixmee-this-buffer-not-pristine-hook t)
  (setq fixmee-last-locate-state nil)
  (callf2 delq (current-buffer) fixmee-pristine-buffer-list))

(defun fixmee-buffer-included-p (buf)
  "Return BUF if BUF should be scanned for \"fixme\" notices."
  (when (and (not noninteractive)
             (bufferp buf)
             (buffer-name buf))
    (with-current-buffer buf
      (when (and (not (minibufferp buf))
                 (not (eq (aref (buffer-name) 0) ?\s))           ; overlaps with exclude-pattern
                 (not (memq major-mode fixmee-exclude-modes))
                 (not (string-match-p fixmee-buffer-name-exclude-pattern (buffer-name buf)))
                 (or (not (numberp fixmee-buffer-maximum-size))
                     (= 0 fixmee-buffer-maximum-size)
                     (<= (point-max) fixmee-buffer-maximum-size))
                 (catch 'success
                   (dolist (filt fixmee-buffer-exclude-functions)
                     (when (funcall filt buf)
                       (throw 'success nil)))
                   t)
                 (catch 'failure
                   (dolist (filt fixmee-buffer-include-functions)
                     (unless (funcall filt buf)
                       (throw 'failure nil)))
                   t))
        buf))))

(defun fixmee-maybe-turn-on (&optional arg)
  "Called by `global-fixmee-mode' to activate fixmee-mode in a buffer.

`fixmee-mode' will be activated in every buffer, except

   minibuffers
   buffers with names that begin with space
   buffers excluded by `fixmee-exclude-modes'
   buffers excluded by `button-lock-exclude-modes'
   buffers excluded by `fixmee-buffer-name-exclude-pattern'
   buffers excluded by `button-lock-buffer-name-exclude-pattern'

If called with a negative ARG, deactivate fixmee-mode in the buffer."
  (callf or arg 1)
  (when (or (< arg 0)
            (fixmee-buffer-included-p (current-buffer)))
    (fixmee-mode arg)))

;; functions that operate on or produce notices
(defun fixmee-inside-notice-p (&optional pos)
   "Whether POS is inside a \"fixme\" notice.

POS defaults to the current point."
   (callf or pos (point))
   (fixmee-locate-all-notices)
   (catch 'found
     (dolist (hit fixmee-notice-list)
       (when (and (eq (current-buffer) (cadr hit))
                  (<= (caddr  hit) pos)
                  (>  (cadddr hit) pos))
         (throw 'found hit)))))

(defun fixmee-locate-all-notices ()
  "Search all open buffers for \"fixme\" notices, and store the results in `fixmee-notice-list'."
  (let ((state (fixmee-get-buffer-state)))
    (if (and state (equal state fixmee-last-locate-state))
        fixmee-notice-list
      (setq fixmee-last-locate-state state)
      (setq fixmee-notice-list (fixmee-notices-from-pristine-buffers))
      (condition-case nil
          (save-match-data
            (dolist (buf (buffer-list))
              (when (buffer-local-value 'fixmee-mode buf)
                (with-current-buffer buf
                  (add-hook 'after-change-functions 'fixmee-this-buffer-not-pristine-hook nil t)
                  (add-hook 'kill-buffer-hook 'fixmee-this-buffer-not-pristine-hook nil t)
                  (unless (memq buf fixmee-pristine-buffer-list)
                    (save-excursion
                      (goto-char (point-min))
                      (while (re-search-forward fixmee-notice-regexp nil t)
                        (save-excursion                                                ; don't know why this was needed - syntax-ppss?
                          (let ((raw-match (match-string-no-properties 1)))
                            (when (and (>= (length raw-match) 3)
                                       (or (not comment-start)                         ; mode has no comment syntax
                                           (save-match-data (nth 4 (syntax-ppss)))))   ; point is within a comment
                              (push (list
                                     (fixmee-measure-urgency raw-match)
                                     (current-buffer)
                                     (match-beginning 1)
                                     (match-end 1))
                                    fixmee-notice-list))))
                        ;; todo removeme forward-line is just a
                        ;; workaround for a current limitation of the
                        ;; regexp passed to button-lock: no multiple
                        ;; hits on the same line.
                        (forward-line 1))))))
              (add-to-list 'fixmee-pristine-buffer-list buf)))
        (quit (fixmee-cache-invalidate))
        (error (fixmee-cache-invalidate)))
      (fixmee-sort-notice-list))))

(defun fixmee-measure-urgency (str-val)
  "Counts how many times the trailing character is repeated on STR-VAL.

Returns an integer, minimum of 1.  Case-insensitive.  The first two
characters of STR-VAL are always ignored."
  (let ((tailchar (downcase (aref str-val (1- (length str-val)))))
        (counter 1))
    (while (and (< counter (- (length str-val) 2))
                (eq tailchar (downcase (aref str-val (- (length str-val) (1+ counter))))))
      (incf counter))
  counter))

(defun fixmee-notices-from-pristine-buffers () ;; optimization
  "Return the subset of `fixmee-notice-list' elements found in pristine buffers."
  (remove-if-not #'(lambda (hit)
                     (and (buffer-name (cadr hit))
                          (memq (cadr hit) fixmee-pristine-buffer-list)))
                 fixmee-notice-list))

(defun fixmee-notices-from-current-buffer ()
  "Return the subset of `fixmee-notice-list' elements found in the current buffer."
  (remove-if-not #'(lambda (hit)
                     (eq (cadr hit) (current-buffer)))
                 fixmee-notice-list))

(defun fixmee-sort-notice-list ()
  "Sort `fixmee-notice-list' by urgency."
  (callf sort fixmee-notice-list #'(lambda (a b)
                                     (cond
                                       ((not (= (car a) (car b)))
                                        (> (car a) (car b)))
                                       ((not (eq (nth 1 a) (nth 1 b)))
                                        (string< (buffer-name (nth 1 a)) (buffer-name (nth 1 b))))
                                       (t
                                        (< (nth 2 a) (nth 2 b)))))))

;; internal navigation driver functions
(defun fixmee-leave-current-notice (&optional reverse)
  "Move the point outside the current notice, if it is within one.

If REVERSE is set, move backwards."
  (let ((hit (fixmee-inside-notice-p)))     ; updates list of notices
    (when hit
      (if reverse
          (goto-char (1- (caddr hit)))
        (goto-char (cadddr hit))))))

(defun fixmee-navigate-to-hit (hit)
  "Navigate to HIT, an element of `fixmee-notice-list'."
  (if (null hit)
      (progn
        (ding t)
        (message "no more \"fixme\" notices"))
    (setq fixmee-last-good-hit hit)
    (switch-to-buffer (nth 1 hit))
    (goto-char (nth 2 hit))
    (unless fixmee-less-feedback
      (message "\"fixme\" notice urgency: %s" (nth 0 hit)))
    (when (fboundp 'nav-flash-show)
      (nav-flash-show))
    t))

(defun fixmee-find-nextmost-urgent (hit)
  "Return the next-most urgent \"fixme\" notice, ranking below HIT in urgency.

The return value is an element of `fixmee-notice-list', not a position.

Returns nil on failure."
  (let ((pos (position hit fixmee-notice-list)))
    (if (not pos)
        nil
      ;; else
      (incf pos)
      (nth pos fixmee-notice-list))))

(defun fixmee-find-prevmost-urgent (hit)
  "Return the previous-most urgent \"fixme\" notice ranking above HIT.

The returned value should rank above HIT in urgency.

The return value is an element of `fixmee-notice-list', not a position.

Returns nil on failure."
  (let ((pos (position hit fixmee-notice-list)))
    (if (not pos)
        nil
      ;; else
      (decf pos)
      (if (< pos 0)
          nil
        ;; else
        (nth pos fixmee-notice-list)))))

(defun fixmee-find-next-by-position (&optional pos)
   "The closest \"fixme\" notice after POS in the current buffer.

POS defaults to the current point.

The return value is an element of `fixmee-notice-list', not a position."
   (save-excursion
     (let ((best nil))
       (goto-char (or pos (point)))
       (fixmee-leave-current-notice) ; updates list of notices
       (setq pos (point))
       (dolist (hit fixmee-notice-list)
         (when (and (eq (current-buffer) (cadr hit))
                    (> (caddr hit) pos)
                    (or (eq (point) pos)
                        (< (caddr hit) (point))))
           (setq best hit)
           (goto-char (caddr hit))))
       best)))

(defun fixmee-find-previous-by-position (&optional pos)
   "The closest \"fixme\" notice before POS in the current buffer.

POS defaults to the current point.

The return value is an element of `fixmee-notice-list', not a position."
   (save-excursion
     (let ((best nil))
       (goto-char (or pos (point)))
       (fixmee-leave-current-notice 'reverse) ; updates list of notices
       (setq pos (point))
       (dolist (hit fixmee-notice-list)
         (when (and (eq (current-buffer) (cadr hit))
                    (< (caddr hit) pos)
                    (or (eq (point) pos)
                        (> (caddr hit) (point))))
           (setq best hit)
           (goto-char (caddr hit))))
       best)))

(defun fixmee-notice-popup (ev)
  "Pop up a context menu on a \"fixme\" notice.

EV is a mouse event which is passed to `popup-menu'."
  (interactive "e")
  (let ((popup-menu-map (make-sparse-keymap "Fixmee Notice")))
    (define-key popup-menu-map [fixmee-goto-previous-by-position]  '(menu-item "Previous Fixme By Position" fixmee-mouse-goto-previous-by-position))
    (define-key popup-menu-map [fixmee-goto-next-by-position]      '(menu-item "Next Fixme By Position"     fixmee-mouse-goto-next-by-position))
    (define-key popup-menu-map [fixmee-goto-prevmost-urgent]       '(menu-item "Previous Fixme By Urgency"  fixmee-mouse-goto-prevmost-urgent))
    (define-key popup-menu-map [fixmee-goto-nextmost-urgent]       '(menu-item "Next Fixme By Urgency"      fixmee-mouse-goto-nextmost-urgent))
    (popup-menu popup-menu-map ev)))

(defun fixmee-button-setup (&optional arg)
  "Use `button-lock-mode' to set up fixmee buttons in a buffer.

If called with negative ARG, remove the buttons."
  (callf or arg 1)
  (when (and (>= arg 0)
             (or (not (boundp 'button-lock-mode))
                 (not button-lock-mode)))
    (button-lock-mode 1))
  (if (< arg 0)
      (button-lock-unset-button fixmee-button)
    (let ((help-content nil))
      (when (eq fixmee-within-notice-down-mouse-3-command 'fixmee-notice-popup)
        (setq help-content "<mouse-3> : context-menu")
        (when fixmee-within-notice-goto-next-by-position-keystrokes
          (callf concat help-content (format "\n%s       : next by position"
                                             (car fixmee-within-notice-goto-next-by-position-keystrokes))))
        (when fixmee-within-notice-goto-previous-by-position-keystrokes
          (callf concat help-content (format "\n%s       : previous by position"
                                             (car fixmee-within-notice-goto-previous-by-position-keystrokes)))))
      (setq fixmee-button (button-lock-set-button (concat (if comment-start "\\s<\\S>*?" "") fixmee-notice-regexp)
                                                   fixmee-within-notice-mouse-1-command
                                                   :mouse-2         fixmee-within-notice-mouse-2-command
                                                   :down-mouse-3    fixmee-within-notice-down-mouse-3-command
                                                   :help-echo       help-content
                                                   :keyboard-action 'fixmee-goto-nextmost-urgent
                                                   :face            'fixmee-notice-face :face-policy 'prepend
                                                   :mouse-face      'fixmee-notice-face
                                                   :grouping        1))
      (dolist (key fixmee-within-notice-goto-next-by-position-keystrokes)
        (button-lock-extend-binding fixmee-button 'fixmee-goto-next-by-position nil key))
      (dolist (key fixmee-within-notice-goto-previous-by-position-keystrokes)
        (button-lock-extend-binding fixmee-button 'fixmee-goto-previous-by-position nil key)))))

;;; minor mode definition

(define-minor-mode fixmee-mode
  "Turn on `fixmee-mode'.

When called interactively with no prefix argument this command
toggles the mode.  With a prefix argument, it enables the mode
if the argument is positive and otherwise disables the mode.

When called from Lisp, this command enables the mode if the
argument is omitted or nil, and toggles the mode if the argument
is 'toggle."
  :lighter fixmee-mode-lighter
  :keymap fixmee-mode-map
  :group 'fixmee
  (fixmee-cache-invalidate)
  (cond
    ((and fixmee-mode
          (or noninteractive                    ; never turn on fixmee-mode where
              (eq (aref (buffer-name) 0) ?\s))) ; there can be no font-lock
     (setq fixmee-mode nil))
   (fixmee-mode
    (fixmee-button-setup 1)
    (fixmee-refresh-timer-setup)
    (when (and (button-lock-called-interactively-p 'interactive)
               (not fixmee-less-feedback))
      (message "fixmee mode enabled")))
   (t
    (fixmee-button-setup -1)
    (when (and (button-lock-called-interactively-p 'interactive)
               (not fixmee-less-feedback))
      (message "fixmee mode disabled")))))

;;; global minor-mode setup

(define-globalized-minor-mode global-fixmee-mode fixmee-mode fixmee-maybe-turn-on
  :group 'fixmee)

;;; interactive commands

(defun fixmee-reload ()
  "Toss out cached data and recalculate locations of \"fixme\" notices."
  (interactive)
  (fixmee-cache-invalidate)
  (fixmee-locate-all-notices))

(defun fixmee-goto-nextmost-urgent (&optional arg)
  "From within a \"fixme\" notice, navigate to the next-most urgent one.

If executed while the point is not within a \"fixme\" notice,
navigate to the most urgent notice in all buffers.

With universal prefix ARG, always navigate to the most urgent
notice in the current buffer.

With two universal prefix ARGs, always navigate to the most urgent
notice in all buffers.

Only buffers in which `fixmee-mode' is active will be searched."
  (interactive "P")
  (fixmee-locate-all-notices)
  (when (and (not (memq last-command fixmee-navigation-commands))
             fixmee-push-mark)
    (back-button-push-mark-local-and-global nil t))
  (cond
    ((and (not (consp arg))
          (fixmee-inside-notice-p))
     (fixmee-navigate-to-hit (fixmee-find-nextmost-urgent (fixmee-inside-notice-p))))
    ((equal arg '(4))
     (fixmee-navigate-to-hit (car (fixmee-notices-from-current-buffer))))
    (t
     (fixmee-navigate-to-hit (car fixmee-notice-list)))))

(defun fixmee-goto-prevmost-urgent (&optional arg)
  "From within a \"fixme\" notice, navigate to the previous-most urgent one.

If executed while the point is not within a \"fixme\" notice,
navigate to the least urgent notice in all buffers.

With universal prefix ARG, always navigate to the least urgent
notice in the current buffer.

With two universal prefix ARGs, always navigate to the most urgent
notice in all buffers.

Only buffers in which `fixmee-mode' is active will be searched."
  (interactive "P")
  (fixmee-locate-all-notices)
  (when (and (not (memq last-command fixmee-navigation-commands))
             fixmee-push-mark)
    (back-button-push-mark-local-and-global nil t))
  (cond
    ((and (not (consp arg))
          (fixmee-inside-notice-p))
     (fixmee-navigate-to-hit (fixmee-find-prevmost-urgent (fixmee-inside-notice-p))))
    ((equal arg '(4))
     (fixmee-navigate-to-hit (car (last (fixmee-notices-from-current-buffer)))))
    (t
     (fixmee-navigate-to-hit (car (last fixmee-notice-list))))))

(defun fixmee-goto-next-by-position (&optional arg)
  "Navigate to the next \"fixme\" notice in positional order in the buffer.

With universal prefix ARG, navigate to the first notice in the
buffer by positional order.

The urgency level of notices is ignored."
  (interactive "P")
  (when (and (not (memq last-command fixmee-navigation-commands))
             fixmee-push-mark)
    (back-button-push-mark-local-and-global nil t))
  (fixmee-navigate-to-hit (fixmee-find-next-by-position (when (consp arg) (point-min)))))

(defun fixmee-goto-previous-by-position (&optional arg)
  "Navigate to the next \"fixme\" notice in positional order in the buffer.

With universal prefix ARG, navigate to the last notice in the
buffer by positional order.

The urgency level of notices is ignored."
  (interactive "P")
  (when (and (not (memq last-command fixmee-navigation-commands))
             fixmee-push-mark)
    (back-button-push-mark-local-and-global nil t))
  (fixmee-navigate-to-hit (fixmee-find-previous-by-position (when (consp arg) (point-max)))))

;; mouse commands
(defun fixmee-mouse-goto-nextmost-urgent (event)
  "Just like `fixmee-goto-nextmost-urgent', but invoked via mouse.

EVENT should be the mouse event which invoked the command."
  (interactive "e")
  (deactivate-mark)
  (goto-char (posn-point (event-start event)))
  (fixmee-goto-nextmost-urgent))

(defun fixmee-mouse-goto-prevmost-urgent (event)
  "Just like `fixmee-goto-prevmost-urgent', but invoked via mouse.

EVENT should be the mouse event which invoked the command."
  (interactive "e")
  (deactivate-mark)
  (goto-char (posn-point (event-start event)))
  (fixmee-goto-prevmost-urgent))

(defun fixmee-mouse-goto-next-by-position (event)
  "Just like `fixmee-goto-next-by-position', but invoked via mouse.

EVENT should be the mouse event which invoked the command."
  (interactive "e")
  (deactivate-mark)
  (goto-char (posn-point (event-start event)))
  (fixmee-goto-next-by-position))

(defun fixmee-mouse-goto-previous-by-position (event)
  "Just like `fixmee-goto-previous-by-position', but invoked via mouse.

EVENT should be the mouse event which invoked the command."
  (interactive "e")
  (deactivate-mark)
  (goto-char (posn-point (event-start event)))
  (fixmee-goto-previous-by-position))

(provide 'fixmee)

;; Emacs
;;
;; Local Variables:
;; indent-tabs-mode: nil
;; mangle-whitespace: t
;; require-final-newline: t
;; coding: utf-8
;; byte-compile-warnings: (not cl-functions)
;; End:
;;
;; LocalWords: fixme fixmee ARGs FixmeeMode smartrep fsets Smartrep
;; LocalWords: incf callf CONSECUTIVES NOMSG caddr cadddr ppss Fixmee
;; LocalWords: nextmost Prevmost Fixme prev smex fixm
;;

;;; fixmee.el ends here
