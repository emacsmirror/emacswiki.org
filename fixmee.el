;;; fixmee.el --- Quickly navigate to FIXME notices in code
;;
;; Copyright (c) 2012 Roland Walker
;;
;; Author: Roland Walker <walker@pobox.com>
;; Homepage: http://github.com/rolandwalker/fixmee
;; URL: http://raw.github.com/rolandwalker/fixmee/master/fixmee.el
;; Version: 0.8.2
;; Last-Updated: 16 Nov 2012
;; EmacsWiki: FixmeeMode
;; Keywords: navigation, convenience
;; Package-Requires: ((button-lock "0.9.8") (nav-flash "1.0.0") (back-button "0.6.0") (smartrep "0.0.3") (string-utils "0.0.6") (tabulated-list "0"))
;;
;; Simplified BSD License
;;
;;; Commentary:
;;
;; Quickstart
;;
;;     (require 'fixmee)
;;
;;     (global-fixmee-mode 1)
;;
;;     right-click on the word "fixme" in a comment
;;
;;     ;; for `next-error' support
;;     M-x fixmee-view-listing RET
;;
;; Explanation
;;
;; Fixmee-mode tracks "fixme" notices in code comments, highlights
;; them, ranks them by urgency, and lets you navigate to them quickly.
;;
;; A distinguishing feature of this library is that it tracks the
;; urgency of each notice, allowing the user to jump directly to
;; the most important problems.
;;
;; Urgency of "fixme" notices is indicated by repetitions of the final
;; character.  For example, one might write "FIXMEEEEEEEEE" for an
;; important issue.  The `fixmee-goto-nextmost-urgent' command will
;; navigate to the longest notice first.
;;
;; To use fixmee-mode, add the following to your ~/.emacs
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
;; or
;;
;;    roll the mouse wheel when hovering over the text "fixm"
;;    in the modeline.
;;
;; or
;;
;;    execute `fixmee-view-listing' to navigate using
;;    `next-error' conventions.
;;
;; Key Bindings
;;
;; The default key bindings are
;;
;;     C-c f   `fixmee-goto-nextmost-urgent'
;;     C-c F   `fixmee-goto-prevmost-urgent'
;;     C-c v   `fixmee-view-listing'
;;     M-n     `fixmee-goto-next-by-position'      ; only when the point is
;;     M-p     `fixmee-goto-previous-by-position'  ; inside a "fixme" notice
;;
;; To constrain the nextmost/prevmost-urgent commands to the current
;; buffer only, use a universal prefix argument, eg
;;
;;     C-u C-c f
;;
;; When the smartrep package is installed, the "C-c" prefix need not
;; be used for consecutive fixmee-mode keyboard commands.  Instead,
;; just keep pressing "f" (or whichever key you set in customize).
;;
;; There is also a context menu and mouse-wheel bindings on the
;; minor-mode lighter in the modeline:
;;
;;             mouse-1  context menu
;;       wheel-down/up  next/prev by urgency
;;     M-wheel-down/up  next/prev by position
;;
;; Patterns
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
;;     GNU Emacs version 24.3-devel     : yes, at the time of writing
;;     GNU Emacs version 24.1 & 24.2    : yes
;;     GNU Emacs version 23.3           : yes
;;     GNU Emacs version 22.3 and lower : no
;;
;;     Requires: button-lock.el
;;               tabulated-list.el (included with Emacs 24.x)
;;
;;     Uses if present: smartrep.el, nav-flash.el, back-button.el,
;;                      string-utils.el
;;
;; Bugs
;;
;;     fixmee--listview-mode-map (the major-mode menu) does not work
;;     well as a context menu.
;;         - menu from major mode of selected window appears even when
;;           right-clicking in this window
;;         - mouse-event wrappers keep keyboard shortcuts from appearing
;;           in menu-bar
;;         - better to have a separate context menu attached to the
;;           entries, using a keymap text property
;;
;;     When comment-start is defined, only the first notice on a line
;;     is lit up by button-lock, though fixmee-mode is aware of multiple
;;     notices on a line.  This is worked around for the moment by
;;     stripping these cases from fixmee-notice-list.  Better would be
;;     to add comment-sensitivity logic to button-lock, and remove the
;;     comment-matching section of the regexp passed to button-lock.
;;
;;     Fixmee-maybe-turn-on gets called multiple times when a file
;;     is loaded.
;;
;;     Fixmee-buffer-include-functions may not contain the function
;;     'frame-bufs-associated-p, because a new buffer is not yet
;;     associated with the frame at the time the global mode check
;;     calls fixmee-maybe-turn-on.
;;
;;     Bug in tabulated-list: position of point is not maintained
;;     when sort headers are clicked while a different window is
;;     selected.
;;
;; TODO
;;
;;     Push mark for navigation which happens from the listview
;;
;;     There is no need for fixmee--listview-mode to be an interactive
;;     command.
;;
;;     Consider allowing all navigation commands to update the listview
;;     buffer (currently only next-error commands do so)
;;
;;     Display fully fontified context lines in listview buffer - some
;;     lines have fontification, seemingly at random.  Disabling
;;     whitespace trimming and excluded properties had no effect.
;;
;;     Multi-line context in listview buffer - tabulated-list accepts
;;     newlines, but data then runs out of the column on the next
;;     line.  Would need to pad to match column position.
;;
;;     Better feedback messages for end-of-list and start-of-list.
;;
;;     Bookmark integration? (implicit bookmarking on notices).
;;
;;     Wrap/cycle options on navigation-by-position.
;;
;;     How to get last-command when user does M-x? (smex is not helping
;;     here).  (nth 0 command-history) ?
;;
;;     Navigation can land on line near vertical edge of window -
;;     should respect user settings and scroll in as needed for
;;     context.
;;
;;     Project support.
;;
;;     Some kind of extra comment indicating a notice is to be ignored?
;;     Lead with a backwhack?
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

;;; requirements

;; for caddr, cadddr, incf, decf, callf, callf2, remove-if-not, position
(require 'cl)

(require 'nav-flash      nil t)
(require 'back-button    nil t)
(require 'smartrep       nil t)
(require 'string-utils   nil t)

(autoload 'button-lock-mode       "button-lock"    "Toggle button-lock-mode, a minor mode for making text clickable.")
(autoload 'button-lock-set-button "button-lock"    "Attach mouse actions to text via `font-lock-mode'.")
(autoload 'tabulated-list-mode    "tabulated-list" "Generic major mode for browsing a list of items.")

;;; declarations

(declare-function smartrep-define-key                     "smartrep.el")
(declare-function back-button-push-mark                   "back-button.el")
(declare-function back-button-push-mark-local-and-global  "back-button.el")
(declare-function button-lock-unset-button                "button-lock.el")
(declare-function button-lock-extend-binding              "button-lock.el")
(declare-function string-utils-squeeze-filename           "string-utils.el")
(declare-function string-utils-trim-whitespace            "string-utils.el")
(declare-function tabulated-list-get-id                   "tabulated-list.el")
(declare-function tabulated-list-init-header              "tabulated-list.el")
(declare-function tabulated-list-print                    "tabulated-list.el")
(declare-function tabulated-list-revert                   "tabulated-list.el")
(declare-function tabulated-list-print-entry              "tabulated-list.el")

(eval-when-compile
  (defvar tabulated-list-sort-key)
  (defvar tabulated-list-format)
  (defvar tabulated-list-padding)
  (defvar tabulated-list-entries)
  (defvar tabulated-list-printer)
  (defvar button-lock-mode))

;;; customizable variables

;;;###autoload
(defgroup fixmee nil
  "Navigate to \"fixme\" notices in code."
  :version "0.8.2"
  :link '(emacs-commentary-link :tag "Commentary" "fixmee")
  :link '(url-link :tag "Github" "http://github.com/rolandwalker/fixmee")
  :link '(url-link :tag "EmacsWiki" "http://emacswiki.org/emacs/FixmeeMode")
  :prefix "fixmee-"
  :group 'navigation
  :group 'convenience)

(defcustom fixmee-notice-regexp "\\(@@@+\\|\\_<\\(?:[Tt][Oo][Dd][Oo]+\\|[Ff][Ii][Xx][Mm][Ee]+\\|XXX+\\)\\)\\(?:[/:?!. \t\r\n\f\v]+\\|-+\\(?:\\s-\\|[\r\n\f\v]\\)\\|\\_>\\)"
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
  "Face to show \"fixme\" notices"
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
                                  fixmee--listview-mode
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

(defcustom fixmee-view-listing-keystrokes '("C-c v")
  "Key sequences to run `fixmee-view-listing'.

The listview buffer displays all current \"fixme\" notices in a
sortable tabulated list, and provides `next-error' support.

These keys are in effect whenever `fixmee-global-mode' is active,
or when `fixmee-mode' is active in a buffer.

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
   :type 'function
   :group 'fixmee-keys)

(defcustom fixmee-within-notice-mouse-2-command 'ignore
  "Command bound to mouse-2 when clicking on \"fixme\" notices.

The format for key sequences is as defined by `kbd'."
   :type 'function
   :group 'fixmee-keys)

(defcustom fixmee-within-notice-down-mouse-3-command 'fixmee-notice-popup
  "Command bound to down-mouse-3 when clicking on \"fixme\" notices.

The format for key sequences is as defined by `kbd'."
   :type 'function
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

(defvar fixmee-mode nil "Mode variable for `fixmee-mode'.")
(make-variable-buffer-local 'fixmee-mode)

(defvar global-fixmee-mode nil
  "Mode variable for `global-fixmee-mode'.")

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
  "Global list of \"fixme\" notices.  Each element is a list (URGENCY BUFFER LOCATION-START LOCATION-END).")

(defvar fixmee-keyboard-navigation-commands  '(
                                               fixmee-goto-prevmost-urgent
                                               fixmee-goto-nextmost-urgent
                                               fixmee-goto-previous-by-position
                                               fixmee-goto-next-by-position
                                               )
  "List of interactive keyboard navigation commands.")

(defvar fixmee-mouse-navigation-commands  '(
                                            fixmee-mouse-goto-nextmost-urgent
                                            fixmee-mouse-goto-prevmost-urgent
                                            fixmee-mouse-goto-next-by-position
                                            fixmee-mouse-goto-previous-by-position
                                            )
  "List of interactive mouse navigation commands.")

(defvar fixmee-all-navigation-commands (append '(fixmee)   ; alias for fixmee-goto-nextmost-urgent
                                               fixmee-keyboard-navigation-commands
                                               fixmee-mouse-navigation-commands)
  "List of interactive navigation commands.")

(defvar fixmee-global-commands '(fixmee-view-listing)
  "List of globally available commands.")

(defvar fixmee-button nil
  "Buffer-local variable holding the `button-lock' button for \"fixme\" notices.")
(make-variable-buffer-local 'fixmee-button)

(defvar fixmee-lighter-menu-mouse-button 1
  "Which mouse button invokes the modeline context menu.")

(defvar fixmee-lighter-keymap-property 'keymap
  "Which property sets the lighter keymap.")

;; fixmee--listview variables

(defvar fixmee--listview-mode nil "Mode variable for `fixmee--listview-mode'.")
(make-variable-buffer-local 'fixmee--listview-mode)

(defvar fixmee--listview-buffer-name "*fixmee notices*"
  "The name of the buffer used to show a listview of all \"fixme\" notices.")

(defvar fixmee--listview-arrow-id nil
  "The `tabulated-list-mode' id associated with the overlay arrow.")
(make-variable-buffer-local 'fixmee--listview-arrow-id)

(defvar fixmee--listview-find-notice-hook nil
  "Hook to run when viewing a notice from a `fixmee--listview-mode' buffer.")

(defvar fixmee--listview-line-format '[("Buffer"  35 fixmee--listview-name-sorter)
                                       ("Urgency" 10 fixmee--listview-urgency-sorter)
                                       ("Context" 80 fixmee--listview-context-sorter)]
  "Tabulated list format for `fixmee--listview-mode' buffers.")

(defvar fixmee--listview-excluded-properties '(
                                               category
                                               field
                                               follow-link
                                               fontified
                                               help-echo
                                               intangible
                                               invisible
                                               keymap
                                               local-map
                                               mouse-face
                                               read-only
                                               yank-handler
                                               )
  "Properties removed from text before display in `fixmee--listview-mode' buffers.")

(defvar fixmee--listview-local-only nil
  "If non-nil, `fixmee--listview-mode' shows notices from the current buffer only.")
(make-variable-buffer-local 'fixmee--listview-local-only)

;;; keymaps

(defvar fixmee-mode-map (make-sparse-keymap) "Keymap for `fixmee-mode' minor-mode.")

(defvar fixmee-mode-global-map (make-sparse-keymap) "Keymap for `global-fixmee-mode' global minor-mode.")

(let ((smart-keys nil))
  (dolist (cmd fixmee-keyboard-navigation-commands)
    (dolist (k (symbol-value (intern (concat (symbol-name cmd) "-keystrokes"))))
      (when (and (not (string-match-p "mouse\\|wheel\\|button" k))
                 (not (get cmd :advertised-binding)))
        (put cmd :advertised-binding (read-kbd-macro k)))
      (if (and (featurep 'smartrep)
               (stringp fixmee-smartrep-prefix)
               (> (length fixmee-smartrep-prefix) 0)
               (string-match-p (concat "\\`" fixmee-smartrep-prefix "\\>") k))
          (push (cons (replace-regexp-in-string
                       (concat "\\`" fixmee-smartrep-prefix "\\>[ \t]*")
                       ""
                       k)
                      cmd)
                smart-keys)
        ;; else
        (define-key fixmee-mode-map (read-kbd-macro k) cmd))))
  (when smart-keys
    (smartrep-define-key fixmee-mode-map fixmee-smartrep-prefix smart-keys)))

(dolist (cmd fixmee-global-commands)
  (dolist (k (symbol-value (intern (concat (symbol-name cmd) "-keystrokes"))))
      (when (and (not (string-match-p "mouse\\|wheel\\|button" k))
                 (not (get cmd :advertised-binding)))
        (put cmd :advertised-binding (read-kbd-macro k)))
    (define-key fixmee-mode-map        (read-kbd-macro k) cmd)
    (define-key fixmee-mode-global-map (read-kbd-macro k) cmd)))

(defvar fixmee--listview-mode-map
  (let ((map (make-sparse-keymap))
        (menu-map (make-sparse-keymap "Fixmee Listview")))
    (define-key map (kbd "<mouse-2>") 'fixmee-listview-mouse-goto-notice)
    (define-key map (kbd "C-c C-c"  ) 'fixmee-listview-goto-notice)
    (define-key map (kbd "<return>" ) 'fixmee-listview-goto-notice)
    (define-key map (kbd "<SPC>"    ) 'fixmee-listview-view-notice)
    (define-key map (kbd "v"        ) 'fixmee-listview-view-notice)
    (define-key map (kbd "l"        ) 'fixmee-listview-toggle-local-only)
    (define-key map (kbd "n"        ) 'next-error-no-select)
    (define-key map (kbd "p"        ) 'previous-error-no-select)
    (define-key map (kbd "<tab>"    ) 'next-error-no-select)
    (define-key map (kbd "<backtab>") 'previous-error-no-select)
    (define-key map (kbd "M-n"      ) 'next-error-no-select)
    (define-key map (kbd "M-p"      ) 'previous-error-no-select)
    (define-key map (kbd "C-c C-f"  ) 'next-error-follow-minor-mode)
    (define-key map (kbd "q"        ) 'quit-window)
    (define-key map (kbd "Q"        ) 'fixmee-listview-quit)
    (define-key map (kbd "{"        ) 'fixmee-listview-previous-buffer)
    (define-key map (kbd "}"        ) 'fixmee-listview-next-buffer)
    (define-key map (kbd "M-{"      ) 'fixmee-listview-previous-buffer)
    (define-key map (kbd "M-}"      ) 'fixmee-listview-next-buffer)
    (define-key menu-map [customize]          '(menu-item "Customize"      (lambda (e) (interactive "e") (customize-group 'fixmee))))
    (define-key menu-map [quit-listview]      '(menu-item "Quit Listview"  fixmee-listview-quit))
    (define-key menu-map [separator-1]        '(menu-item "--"))
    (define-key menu-map [follow-mode]        '(menu-item "Follow Mode"        next-error-follow-minor-mode
                                                          :button (:toggle .   next-error-follow-minor-mode)))
    (define-key menu-map [toggle-local-only]  '(menu-item "Local Notices Only" fixmee-listview-toggle-local-only
                                                          :button (:toggle .   fixmee--listview-local-only)))
    (define-key menu-map [separator-2]        '(menu-item "--"))
    (define-key menu-map [previous-buffer]    '(menu-item "Previous Buffer"  (lambda (e)
                                                                               (interactive "e")
                                                                               (ignore-errors (posn-set-point (event-end last-nonmenu-event)))
                                                                               (fixmee-listview-previous-buffer))))
    (define-key menu-map [next-buffer]        '(menu-item "Next Buffer"      (lambda (e)
                                                                               (interactive "e")
                                                                               (ignore-errors (posn-set-point (event-end last-nonmenu-event)))
                                                                               (fixmee-listview-next-buffer))))
    (define-key menu-map [previous-notice]    '(menu-item "Previous Notice"  (lambda (e)
                                                                               (interactive "e")
                                                                               (ignore-errors (posn-set-point (event-end last-nonmenu-event)))
                                                                               (previous-error-no-select))))
    (define-key menu-map [next-notice]        '(menu-item "Next Notice"      (lambda (e)
                                                                               (interactive "e")
                                                                               (ignore-errors (posn-set-point (event-end last-nonmenu-event)))
                                                                               (next-error-no-select))))
    (define-key menu-map [separator-3]        '(menu-item "--"))
    (define-key menu-map [goto-notice]        '(menu-item "Goto Notice"      (lambda (e)
                                                                               (interactive "e")
                                                                               (ignore-errors (posn-set-point (event-end last-nonmenu-event)))
                                                                               (fixmee-listview-goto-notice))))
    (define-key menu-map [view-notice]        '(menu-item "View Notice"      (lambda (e)
                                                                               (interactive "e")
                                                                               (ignore-errors (posn-set-point (event-end last-nonmenu-event)))
                                                                               (fixmee-listview-view-notice))))
    (define-key map [menu-bar fixmee-listview] (cons "Fixmee Listview" menu-map))
    map)
  "Keymap for `fixmee--listview-mode' buffers.")
(put 'fixmee-listview-goto-notice :advertised-binding (kbd "<return>"))
(put 'fixmee-listview-view-notice :advertised-binding (kbd "<SPC>"))

;;; lighter

(defvar fixmee-lighter-map  (let ((map (make-sparse-keymap))
                                  (menu-map (make-sparse-keymap "Fixmee Mode")))
                              (define-key menu-map [customize]                         '(menu-item "Customize"      (lambda (e) (interactive "e") (customize-group 'fixmee))))
                              (define-key menu-map [turn-off-fixmee-mode]              '(menu-item "Turn Off Fixmee Mode"  fixmee-mode))
                              (define-key menu-map [separator-2]                       '(menu-item "--"))
                              (define-key menu-map [fixmee-view-listing]               '(menu-item "View Listing of Notices" fixmee-view-listing))
                              (define-key menu-map [separator-1]                       '(menu-item "--"))
                              (define-key menu-map [fixmee-goto-previous-by-position]  (append '(menu-item "Previous Fixme By Position" fixmee-goto-previous-by-position)
                                                                                               ;; force :keys because of smartrep
                                                                                               (when (get 'fixmee-goto-previous-by-position :advertised-binding)
                                                                                                 (list :keys
                                                                                                       (format-kbd-macro
                                                                                                        (get 'fixmee-goto-previous-by-position :advertised-binding))))))
                              (define-key menu-map [fixmee-goto-next-by-position]      (append '(menu-item "Next Fixme By Position" fixmee-goto-next-by-position)
                                                                                               (when (get 'fixmee-goto-next-by-position :advertised-binding)
                                                                                                 (list :keys
                                                                                                        (format-kbd-macro
                                                                                                         (get 'fixmee-goto-next-by-position :advertised-binding))))))
                              (define-key menu-map [fixmee-goto-prevmost-urgent]       (append '(menu-item "Previous Fixme By Urgency"  fixmee-goto-prevmost-urgent)
                                                                                               (when (get 'fixmee-goto-prevmost-urgent :advertised-binding)
                                                                                                 (list :keys
                                                                                                       (format-kbd-macro
                                                                                                        (get 'fixmee-goto-prevmost-urgent :advertised-binding))))))
                              (define-key menu-map [fixmee-goto-nextmost-urgent]       (append '(menu-item "Next Fixme By Urgency" fixmee-goto-nextmost-urgent)
                                                                                               (when (get 'fixmee-goto-nextmost-urgent :advertised-binding)
                                                                                                 (list :keys
                                                                                                       (format-kbd-macro
                                                                                                        (get 'fixmee-goto-nextmost-urgent :advertised-binding))))))
                              (define-key map (kbd "<mode-line> <wheel-up>"     )      'fixmee-goto-prevmost-urgent)
                              (define-key map (kbd "<mode-line> <wheel-down>"   )      'fixmee-goto-nextmost-urgent)
                              (define-key map (kbd "<mode-line> <M-wheel-up>"   )      'fixmee-goto-previous-by-position)
                              (define-key map (kbd "<mode-line> <M-wheel-down>" )      'fixmee-goto-next-by-position)
                              (define-key map (kbd "<mode-line> <mouse-4>"      )      'fixmee-goto-prevmost-urgent)
                              (define-key map (kbd "<mode-line> <mouse-5>"      )      'fixmee-goto-nextmost-urgent)
                              (define-key map (kbd "<mode-line> <M-mouse-4>"    )      'fixmee-goto-previous-by-position)
                              (define-key map (kbd "<mode-line> <M-mouse-5>"    )      'fixmee-goto-next-by-position)
                              (define-key map (read-kbd-macro (format "<mode-line> <down-mouse-%s>" fixmee-lighter-menu-mouse-button)) menu-map)
                              map) "Keymap for the `fixmee-mode' lighter.")

(when (and (stringp fixmee-mode-lighter)
           (> (length fixmee-mode-lighter) 0))
  (callf propertize fixmee-mode-lighter
                    fixmee-lighter-keymap-property fixmee-lighter-map
                    'help-echo (format "fixmee-mode: mouse-%s menu\nwheel down/up by urgency\nwheel M-down/M-up by position." fixmee-lighter-menu-mouse-button)))

;;; aliases and fsets

;;;###autoload
(defalias 'fixmee 'fixmee-goto-nextmost-urgent)

;;; macros

(defmacro fixmee-called-interactively-p (&optional kind)
  "A backward-compatible version of `called-interactively-p'.

Optional KIND is as documented at `called-interactively-p'
in GNU Emacs 24.1 or higher."
  (cond
    ((not (fboundp 'called-interactively-p))
     '(interactive-p))
    ((condition-case nil
         (progn (called-interactively-p 'any) t)
       (error nil))
     `(called-interactively-p ,kind))
    (t
     '(called-interactively-p))))

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

(unless (fboundp 'string-utils-trim-whitespace)
  (defun string-utils-trim-whitespace (str-val &optional whitespace-type multi-line)
  "Return STR-VAL with leading and trailing whitespace removed.

WHITESPACE-TYPE is ignored.

If optional MULTI-LINE is set, trim spaces at starts and
ends of all lines throughout STR-VAL."
  (let* ((string-utils-whitespace " \t\n\r\f")
         (whitespace-regexp (concat "[" string-utils-whitespace "]"))
         (start-pat (if multi-line "^" "\\`"))
         (end-pat   (if multi-line "$" "\\'")))
    (save-match-data
      (replace-regexp-in-string (concat start-pat whitespace-regexp "+") ""
         (replace-regexp-in-string (concat whitespace-regexp "+" end-pat) ""
            str-val))))))

;;; utility functions

;; general functions
(defun fixmee-refresh-timer-setup (&optional arg)
  "Set up a timer to invalidate caches.

When optional ARG is less than 0, turn off timer.

This should help cover up various minor bugs, such as not
invalidating the cache when the regexp is changed."
  (when (timerp fixmee-cache-refresh-timer)
    (cancel-timer fixmee-cache-refresh-timer)
    (setq fixmee-cache-refresh-timer nil))
  (unless (and (numberp arg)
               (< arg 0))
    (when fixmee-cache-refresh-interval
      (let ((secs (truncate (* 60 fixmee-cache-refresh-interval))))
        (when (> secs 0)
          (setq fixmee-cache-refresh-timer (run-with-timer secs secs 'fixmee-cache-invalidate)))))))

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

(defun fixmee-this-buffer-not-pristine-hook (&rest _ignored)
  "Add BUFFER to the list modified since the last search for \"fixme\" notices."
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

;; functions that operate on or produce notices
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
                                           (save-match-data
                                             (nth 4 (syntax-ppss (match-beginning 1)))))) ; point is within a comment
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

;; fixmee--listview-mode utility functions
(defun fixmee--listview-name-sorter (a b)
  "Sort `fixmee--listview-mode' buffer lines by name."
  (save-match-data
    (let* ((string-a (ignore-errors (replace-regexp-in-string ":\\([0-9]+\\)\\'" "" (car (aref (cadr a) 0)))))
           (string-b (ignore-errors (replace-regexp-in-string ":\\([0-9]+\\)\\'" "" (car (aref (cadr b) 0)))))
           (line-a   (ignore-errors (string-to-number (replace-regexp-in-string "\\`.*:\\([0-9]+\\)\\'" "\\1" (car (aref (cadr a) 0))))))
           (line-b   (ignore-errors (string-to-number (replace-regexp-in-string "\\`.*:\\([0-9]+\\)\\'" "\\1" (car (aref (cadr b) 0)))))))
      (cond
        ((string< string-a string-b)
         t)
        ((and (string= string-a string-b)
              line-a
              line-b
              (< line-a line-b))
         t)
        (t
         nil)))))

(defun fixmee--listview-urgency-sorter (a b)
  "Sort `fixmee--listview-mode' buffer lines by urgency."
  (cond
    ((< (string-to-number (aref (cadr a) 1))
        (string-to-number (aref (cadr b) 1)))
     t)
    ((= (string-to-number (aref (cadr a) 1))
        (string-to-number (aref (cadr b) 1)))
     (if (cdr tabulated-list-sort-key)
         (fixmee--listview-name-sorter b a)
       (fixmee--listview-name-sorter a b)))
    (t
     nil)))

(defun fixmee--listview-context-sorter (a b)
  "Sort `fixmee--listview-mode' buffer lines by context."
  (string< (aref (cadr a) 2)
           (aref (cadr b) 2)))

(defun fixmee--listview-max-line-number-width (notice-list)
  "Return the maximum width needed to print line numbers of buffers in NOTICE-LIST."
  (let ((maxbuf (nth 1 (car notice-list))))
    (dolist (buf (delete-dups (mapcar #'(lambda (x) (nth 1 x)) notice-list)))
      (when (> (buffer-size buf) (buffer-size maxbuf))
        (setq maxbuf buf)))
    (with-current-buffer maxbuf
      (length (number-to-string (1+ (count-lines 1 (point-max))))))))

(defun fixmee--listview-buffer-plus-line-formatted (buf pos len)
  "Return a formatted string containing BUF and line number for POS.

BUF is intelligently truncated if its length is longer than LEN."
  (save-match-data
    (let ((line (with-current-buffer buf (count-lines 1 pos)))
          (ellipsis "..."))
      (when (bufferp buf)
        (setq buf (buffer-name buf)))
      (when (> (length buf) len)
        (if (fboundp 'string-utils-squeeze-filename)
            (callf string-utils-squeeze-filename buf len)
          (callf substring buf (- len (length ellipsis)))))
      (concat buf (format ":%s" line)))))

(defun fixmee--listview-render-entries (notice-list)
  "Convert NOTICE-LIST to the format expected by `tabulated-list'.

NOTICE-LIST is a list in the format of `fixmee-notice-list'."
  (when notice-list
    (let* ((line-num-width (fixmee--listview-max-line-number-width notice-list))
           (buffer-col-width (nth 1 (aref fixmee--listview-line-format 0)))
           (tablist nil))
      (dolist (hit notice-list)
        (destructuring-bind (urgency buf start end) hit
          (push (list
                 ;; id
                 (list buf start)
                 ;; columns
                 (vector (list (fixmee--listview-buffer-plus-line-formatted buf start (- buffer-col-width line-num-width 2))
                               'face 'link
                               'follow-link t
                               'action 'fixmee-listview-mouse-goto-notice)
                         (number-to-string urgency)
                         (with-current-buffer buf
                           (save-excursion
                             (goto-char start)
                             (let ((str-val (string-utils-trim-whitespace
                                             (buffer-substring (line-beginning-position) (line-end-position)))))
                               (remove-list-of-text-properties 0 (length str-val) fixmee--listview-excluded-properties str-val)
                               str-val)))))
                tablist)))
      tablist)))

(defun fixmee--listview-find-current-notice ()
  "Get the id for the \"fixme\" notice at the current line in `fixmee--listview-mode'."
  (assert (eq major-mode 'fixmee--listview-mode)
          nil "Not in a fixmee--listview-mode buffer")
  (let ((id (tabulated-list-get-id)))
    (assert id nil "No \"fixme\" notice on this line")
    (assert (buffer-live-p (car id)) nil "Buffer for this \"fixme\" notice no longer active")
    id))

(defun fixmee--listview-print-entry-function (id cols)
  "Call `tabulated-list-print-entry' and restore the arrow overlay if needed.

ID and COLS are as documented at `tabulated-list-print-entry'."
  (tabulated-list-print-entry id cols)
  (when fixmee--listview-arrow-id
    (save-excursion
      (forward-line -1)
      (when (equal fixmee--listview-arrow-id (tabulated-list-get-id))
        (setq overlay-arrow-position (copy-marker (line-beginning-position)))))))

(defun fixmee--listview-revert-function (&rest _ignored)
  "Handle revert in `fixmee--listview-mode' buffers."
  (when (eq major-mode 'fixmee--listview-mode)
    (fixmee-locate-all-notices)
    (set (make-local-variable 'tabulated-list-entries)
         (fixmee--listview-render-entries fixmee-notice-list))
    (setq mode-line-process (if fixmee--listview-local-only "[local]" nil))
    (when fixmee--listview-local-only
      ;; todo this is a bit wrong in that the user could be looking
      ;;      at another buffer which is not in fixmee-mode.  But
      ;;      `other-buffer' doesn't deliver anything useful.
      (let ((other-buf (car
                        (remove-if-not #'(lambda (buf)
                                           (buffer-local-value 'fixmee-mode buf))
                                       (delq (current-buffer) (buffer-list))))))
        (callf2 remove-if-not #'(lambda (entry)
                                  (eq (caar entry) other-buf))
                tabulated-list-entries)))
    (unless tabulated-list-entries
      (setq tabulated-list-entries '(((nil nil) ["<no matches found>" "" ""]))))
    (tabulated-list-revert)))

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
    (fixmee-refresh-timer-setup 1)
    (when (and (fixmee-called-interactively-p 'interactive)
               (not fixmee-less-feedback))
      (message "fixmee mode enabled")))
   (t
    (fixmee-button-setup -1)
    (fixmee-refresh-timer-setup -1)
    (when (eq (current-buffer)
              (nth 1 fixmee-last-good-hit))
      (setq fixmee-last-good-hit nil))
    (when (and (fixmee-called-interactively-p 'interactive)
               (not fixmee-less-feedback))
      (message "fixmee mode disabled")))))

;;; global minor-mode definition

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

(defun fixmee-mode-maybe-global-teardown ()
  "Wipe global values when `global-fixmee-mode' is turned off."
  (unless global-fixmee-mode
    (setq fixmee-last-good-hit nil)
    (fixmee-cache-invalidate)))

;;;###autoload
(define-globalized-minor-mode global-fixmee-mode fixmee-mode fixmee-maybe-turn-on
  :keymap fixmee-mode-global-map
  :group 'fixmee)

(add-hook 'global-fixmee-mode-hook 'fixmee-mode-maybe-global-teardown)

;;; listview buffer major-mode definition

(put 'fixmee--listview-mode 'mode-class 'special)
(define-derived-mode fixmee--listview-mode tabulated-list-mode "Fixmee Listview"
  "Major mode for browsing fixmee notices.
\\<fixmee--listview-mode-map>Move point to any notice in this buffer, then use
\\[fixmee-listview-goto-notice] to view that notice.
Alternatively, click \\[fixmee-listview-mouse-goto-notice].

\\{fixmee--listview-mode-map}"
  :group 'fixmee
  (set (make-local-variable 'revert-buffer-function) 'fixmee--listview-revert-function)
  (set (make-local-variable 'next-error-function) 'fixmee--listview-next-error-function)
  (set (make-local-variable 'tabulated-list-printer) 'fixmee--listview-print-entry-function)
  (setq next-error-last-buffer (current-buffer))
  (make-local-variable 'overlay-arrow-position)
  (set (make-local-variable 'overlay-arrow-string) "")
  (setq next-error-overlay-arrow-position nil)
  (add-hook 'kill-buffer-hook
            (lambda () (setq next-error-overlay-arrow-position nil)) nil t)
  (setq tabulated-list-format fixmee--listview-line-format)
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Urgency" t))
  (tabulated-list-init-header))

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

With universal prefix ARG, constrain navigation to notices within
the current buffer.

With two universal prefix ARGs, always navigate to the most
urgent notice in all buffers (ignoring whether the point is
currently inside a notice).

Only buffers in which `fixmee-mode' is active will be searched."
  (interactive "P")
  (fixmee-locate-all-notices)
  (when (and (not (memq last-command fixmee-all-navigation-commands))
             (fixmee-called-interactively-p 'interactive)
             fixmee-push-mark)
    (back-button-push-mark-local-and-global nil t))
  (cond
    ((and (not (consp arg))
          (fixmee-inside-notice-p))
     (fixmee-navigate-to-hit (fixmee-find-nextmost-urgent (fixmee-inside-notice-p))))
    ((and (equal arg '(4))
          (fixmee-inside-notice-p))
     (let ((fixmee-notice-list (fixmee-notices-from-current-buffer)))
       (fixmee-navigate-to-hit (fixmee-find-nextmost-urgent (fixmee-inside-notice-p)))))
    ((equal arg '(4))
     (fixmee-navigate-to-hit (car (fixmee-notices-from-current-buffer))))
    (t
     (fixmee-navigate-to-hit (car fixmee-notice-list)))))

(defun fixmee-goto-prevmost-urgent (&optional arg)
  "From within a \"fixme\" notice, navigate to the previous-most urgent one.

If executed while the point is not within a \"fixme\" notice,
navigate to the least urgent notice in all buffers.

With universal prefix ARG, constrain navigation to notices within
the current buffer.

With two universal prefix ARGs, always navigate to the least
urgent notice in all buffers (ignoring whether the point is
currently inside a notice).

Only buffers in which `fixmee-mode' is active will be searched."
  (interactive "P")
  (fixmee-locate-all-notices)
  (when (and (not (memq last-command fixmee-all-navigation-commands))
             (fixmee-called-interactively-p 'interactive)
             fixmee-push-mark)
    (back-button-push-mark-local-and-global nil t))
  (cond
    ((and (not (consp arg))
          (fixmee-inside-notice-p))
     (fixmee-navigate-to-hit (fixmee-find-prevmost-urgent (fixmee-inside-notice-p))))
    ((and (equal arg '(4))
          (fixmee-inside-notice-p))
     (let ((fixmee-notice-list (fixmee-notices-from-current-buffer)))
       (fixmee-navigate-to-hit (fixmee-find-prevmost-urgent (fixmee-inside-notice-p)))))
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
  (when (and (not (memq last-command fixmee-all-navigation-commands))
             (fixmee-called-interactively-p 'interactive)
             fixmee-push-mark)
    (back-button-push-mark-local-and-global nil t))
  (fixmee-navigate-to-hit (fixmee-find-next-by-position (when (consp arg) (point-min)))))

(defun fixmee-goto-previous-by-position (&optional arg)
  "Navigate to the next \"fixme\" notice in positional order in the buffer.

With universal prefix ARG, navigate to the last notice in the
buffer by positional order.

The urgency level of notices is ignored."
  (interactive "P")
  (when (and (not (memq last-command fixmee-all-navigation-commands))
             (fixmee-called-interactively-p 'interactive)
             fixmee-push-mark)
    (back-button-push-mark-local-and-global nil t))
  (fixmee-navigate-to-hit (fixmee-find-previous-by-position (when (consp arg) (point-max)))))

;; mouse commands
(defun fixmee-mouse-goto-nextmost-urgent (event)
  "Just like `fixmee-goto-nextmost-urgent', but invoked via mouse.

EVENT should be the mouse event which invoked the command."
  (interactive "e")
  (deactivate-mark)
  (when (and (not (memq last-command fixmee-all-navigation-commands))
             (fixmee-called-interactively-p 'interactive)
             fixmee-push-mark)
    (back-button-push-mark-local-and-global nil t))
  (goto-char (posn-point (event-start event)))
  (fixmee-goto-nextmost-urgent))

(defun fixmee-mouse-goto-prevmost-urgent (event)
  "Just like `fixmee-goto-prevmost-urgent', but invoked via mouse.

EVENT should be the mouse event which invoked the command."
  (interactive "e")
  (deactivate-mark)
  (when (and (not (memq last-command fixmee-all-navigation-commands))
             (fixmee-called-interactively-p 'interactive)
             fixmee-push-mark)
    (back-button-push-mark-local-and-global nil t))
  (goto-char (posn-point (event-start event)))
  (fixmee-goto-prevmost-urgent))

(defun fixmee-mouse-goto-next-by-position (event)
  "Just like `fixmee-goto-next-by-position', but invoked via mouse.

EVENT should be the mouse event which invoked the command."
  (interactive "e")
  (deactivate-mark)
  (when (and (not (memq last-command fixmee-all-navigation-commands))
             (fixmee-called-interactively-p 'interactive)
             fixmee-push-mark)
    (back-button-push-mark-local-and-global nil t))
  (goto-char (posn-point (event-start event)))
  (fixmee-goto-next-by-position))

(defun fixmee-mouse-goto-previous-by-position (event)
  "Just like `fixmee-goto-previous-by-position', but invoked via mouse.

EVENT should be the mouse event which invoked the command."
  (interactive "e")
  (deactivate-mark)
  (when (and (not (memq last-command fixmee-all-navigation-commands))
             (fixmee-called-interactively-p 'interactive)
             fixmee-push-mark)
    (back-button-push-mark-local-and-global nil t))
  (goto-char (posn-point (event-start event)))
  (fixmee-goto-previous-by-position))

;; listview next-error support

(defun fixmee-listview-goto-notice ()
  "Go to the \"fixme\" notice on the current line."
  (interactive)
  (let ((buf (get-buffer fixmee--listview-buffer-name)))
    (assert buf nil "No current fixmee--listview-mode buffer.")
    (with-current-buffer buf
      (let ((id (fixmee--listview-find-current-notice))
            (from-fixmee-listview-window (eq buf (window-buffer (selected-window))))
            (fixmee-listview-window (get-buffer-window (current-buffer) t)))
        (when id
          (unless fixmee-listview-window
            (with-selected-window (selected-window)
              (pop-to-buffer buf)
              (setq fixmee-listview-window (get-buffer-window (current-buffer) t))))
          (when fixmee-listview-window
            (set-window-point fixmee-listview-window (point)))
          (setq overlay-arrow-position (copy-marker (line-beginning-position)))
          (setq fixmee--listview-arrow-id (tabulated-list-get-id))
          (if from-fixmee-listview-window
              (switch-to-buffer-other-window (car id))
            (switch-to-buffer (car id)))
          (goto-char (cadr id))
          (run-hooks 'fixmee--listview-find-notice-hook))))))

;; don't know why this should be interactive, but compilation-mode has it that way
(defun fixmee--listview-next-error-function (&optional arg reset)
  "Move to the ARGth next match in a `fixmee--listview-mode' buffer.

ARG defaults to 1.

When RESET is non-nil, move to the first match in the error
buffer."
  (interactive "p")
  (callf or arg 1)
  (with-current-buffer
      (if (next-error-buffer-p (current-buffer))
          (current-buffer)
        (next-error-find-buffer nil nil
                                #'(lambda ()
                                    (eq major-mode 'fixmee--listview-mode))))
      (let ((orig-pos (point)))
        (goto-char (cond
                     (reset
                      (point-min))
                     ((< arg 0)
                      (line-beginning-position))
                     ((> arg 0)
                      (line-end-position))
                     (t
                      (point))))
        (unless reset
          (when (or (> (abs (forward-line arg)) 0)
                    (not (tabulated-list-get-id)))
            (goto-char orig-pos)
            (error "No more matches")))
        (fixmee-listview-goto-notice))))

;; listview interactive commands

(defun fixmee-listview-view-notice ()
  "View the \"fixme\" notice on the current line.

This command does not change the current window."
  (interactive)
  (next-error-no-select 0))

;;;###autoload
(defun fixmee-view-listing (&optional arg)
  "View \"fixme\" notices in a `fixmee--listview-mode' buffer.

If the listview buffer currently exists, pop to it; otherwise
create it.

The listview buffer represents a snapshot of the current state
and does not update dynamically as you edit other
buffers.  (Other fixmee navigation commands such as
`fixmee-goto-nextmost-urgent' update and act according to
your latest edits.)

To regenerate the listview, issue this command with universal
prefix ARG."
  (interactive "P")
  (let ((buf (get-buffer fixmee--listview-buffer-name)))
    (when (and (buffer-live-p buf)
               (consp arg))
      (kill-buffer buf)
      (setq buf nil))
    (unless (buffer-live-p buf)
      (setq buf (get-buffer-create fixmee--listview-buffer-name))
      (with-current-buffer buf
        (setq fixmee--listview-local-only nil)
        (fixmee--listview-mode)))
    (pop-to-buffer buf)
    (fixmee--listview-revert-function)))

(defun fixmee-listview-toggle-local-only ()
  "Toggle whether to view \"fixme\" notices only from a single buffer."
  (interactive)
  (let ((buf (get-buffer fixmee--listview-buffer-name)))
    (assert buf nil "No current fixmee--listview-mode buffer.")
    (with-current-buffer buf
      (callf not fixmee--listview-local-only)
      (fixmee--listview-revert-function))))

(defun fixmee-listview-previous-error (&optional arg)
  "Move to the ARGth previous match in a `fixmee--listview-mode' buffer.

ARG defaults to 1."
  (interactive "p")
  (callf or arg 1)
  (fixmee--listview-next-error-function (- arg)))

(defun fixmee-listview-next-buffer (&optional arg)
  "Skip to the ARGth next buffer listed in a `fixmee--listview-mode' buffer.

ARG defaults to 1.

If the listview is not sorted per-buffer, considers each block of
contiguous identical buffers separately."
  (interactive "p")
  (callf or arg 1)
  (with-current-buffer
      (if (next-error-buffer-p (current-buffer))
          (current-buffer)
        (next-error-find-buffer nil nil
                                #'(lambda ()
                                    (eq major-mode 'fixmee--listview-mode))))
      (let* ((orig-pos (point))
             (orig-id (tabulated-list-get-id))
             (orig-buf (car orig-id))
             (id nil)
             (buf nil))
        (unless orig-id
          (while (and (not (if (< arg 0) (bobp) (eobp)))
                      (not (tabulated-list-get-id)))
            (forward-line (signum arg)))
          (unless (tabulated-list-get-id)
            (goto-char orig-pos)
            (error "No more buffers"))
          (setq arg (* (signum arg) (1- (abs arg)))))
        (dotimes (i (abs arg))
          (setq id (tabulated-list-get-id))
          (setq buf (car id))
          (while (and buf
                      (not (if (< arg 0) (bobp) (eobp)))
                      (tabulated-list-get-id)
                      (eq buf (car (tabulated-list-get-id))))
            (forward-line (signum arg)))
          (when (or (not (tabulated-list-get-id))
                    (eq buf (car (tabulated-list-get-id))))
            (goto-char orig-pos)
            (error "No more buffers")))
        (when (or (not (tabulated-list-get-id))
                  (eq orig-buf (car (tabulated-list-get-id))))
          (goto-char orig-pos)
          (error "No more buffers")))))

(defun fixmee-listview-previous-buffer (&optional arg)
  "Skip to the ARGth previous buffer listed in a `fixmee--listview-mode' buffer.

ARG defaults to 1.

If the listview is not sorted per-buffer, considers each block of
contiguous identical buffers separately."
  (interactive "p")
  (callf or arg 1)
  (fixmee-listview-next-buffer (- arg)))

(defun fixmee-listview-mouse-goto-notice (event)
  "Go to the \"fixme\" notice on the current line using the mouse.

EVENT is the current mouse event."
  (interactive "e")
  (posn-set-point (event-end last-nonmenu-event))
  (fixmee-listview-goto-notice))

(defun fixmee-listview-quit (&optional buf)
  "Quit the buffer created by `fixmee-view-listing'.

Delete window associated with buffer BUF and kill BUF."
  (interactive)
  (callf or buf (get-buffer fixmee--listview-buffer-name))
  (when (buffer-live-p buf)
    (when (get-buffer-window buf)
      (ignore-errors (delete-window (get-buffer-window buf))))
    (kill-buffer buf)))

(provide 'fixmee)

;; Emacs
;;
;; Local Variables:
;; indent-tabs-mode: nil
;; mangle-whitespace: t
;; require-final-newline: t
;; coding: utf-8
;; byte-compile-warnings: (not cl-functions redefine)
;; End:
;;
;; LocalWords: fixme fixmee ARGs FixmeeMode smartrep fsets Smartrep
;; LocalWords: incf callf CONSECUTIVES NOMSG caddr cadddr ppss Fixmee
;; LocalWords: nextmost Prevmost Fixme prev smex fixm listview utils
;; LocalWords: devel ARGth MULTI prevmost Multi Listview
;;

;;; fixmee.el ends here
