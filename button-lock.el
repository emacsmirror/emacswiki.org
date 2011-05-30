;;; button-lock.el --- clickable text defined by regular expression, controlled by font-lock
;;
;; Copyright (c) 2011 D Roland Walker
;;
;; Author: D Roland Walker <walker@pobox.com>
;; URL: https://github.com/rolandwalker/button-lock/raw/master/button-lock.el
;; Version: 0.81
;; Last-Updated: 29 May 2011
;; EmacsWiki: ButtonLockMode
;; Keywords: mouse, button, hyperlink
;;
;; Simplified BSD License
;;
;;; Commentary:
;;
;; Button-lock is a minor mode which provides simple facilities to
;; define clickable text based regular expressions.  Button-lock.el
;; piggybacks on font-lock.el, and is efficient.  Overlays are not
;; used.
;;
;; Button-lock buttons (links) can execute any function.
;;
;; See wiki-nav.el for a user-friendly library built on top of
;; button-lock.el.  Wiki-nav.el is available here:
;;
;;    https://github.com/rolandwalker/button-lock
;;
;; Example usage
;;
;;    (require 'button-lock)
;;    (global-button-lock-mode 1)
;;
;;    ; add a button to the current buffer
;;    (button-lock-set-button "\\<print\\>" 'beginning-of-line)
;;
;;    ; to remove that button later, pass all the same arguments
;;    ; to button-lock-unset-button
;;    (button-lock-unset-button "\\<print\\>" 'beginning-of-line)
;;
;;    ; or to remove the most recently added button
;;    (button-lock-pop-button)
;;
;;    ; create a fancy raised button
;;    (button-lock-set-button "print" #'(lambda ()
;;                                        (interactive)
;;                                        (save-match-data
;;                                          (if (re-search-forward "print" nil t)
;;                                              (goto-char (match-beginning 0))
;;                                            (goto-char (point-min))
;;                                            (if (re-search-forward "print" nil t)
;;                                                (goto-char (match-beginning 0))))))
;;                            :face 'custom-button-face :mouse-face 'custom-button-mouse)
;;
;;    ; activate hyperlinks
;;    (button-lock-set-button "\\<http://[^[:space:]\n]+"
;;                            'browse-url-at-mouse
;;                            :face 'link :policy 'prepend)
;;
;;    ; activate hyperlinks only in lines that begin with a comment character
;;    (button-lock-set-button "^\\s-*\\s<.*?\\<\\(http://[^[:space:]\n]+\\)"
;;                            'browse-url-at-mouse
;;                            :face 'link :policy 'prepend :grouping 1)
;;
;;    ; turn folding-mode delimiters into mouseable buttons
;;    (add-hook 'folding-mode-hook  #'(lambda ()
;;                                      (button-lock-mode 1)
;;                                      (button-lock-set-button
;;                                       (concat "^" (regexp-quote (car (folding-get-mode-marks))))
;;                                       'folding-toggle-show-hide)
;;                                      (button-lock-set-button
;;                                       (concat "^" (regexp-quote (cadr (folding-get-mode-marks))))
;;                                       'folding-toggle-show-hide)))
;;
;;    ; create a button that responds to the keyboard, but not the mouse
;;    (button-lock-set-button "\\<http://[^[:space:]\n]+"
;;                            'browse-url-at-point
;;                            :mouse-binding     nil
;;                            :mouse-face        nil
;;                            :face             'link
;;                            :face-policy      'prepend
;;                            :keyboard-binding "RET")
;;
;;    ; define a global button, to be set whenever the minor mode is activated
;;    (button-lock-set-global-button '("\\<print\\>" 'beginning-of-line))
;;
;; See Also
;;
;;    M-x customize-group RET button-lock RET
;;
;; Prior Art
;;
;;    hi-lock.el
;;    David M.  Koppelman <koppel@ece.lsu.edu>
;;
;;    buttons.el
;;    Miles Bader <miles@gnu.org>
;;
;; Notes
;;
;;    By default, button-lock uses newfangled left-clicks rather than
;;    Emacs-traditional middle clicks.
;;
;;    Font lock is very efficient, but it is still possible to bog
;;    things down if you feed it expensive regular expressions.  Use
;;    anchored expressions, and be careful about backtracking.  See
;;    `regexp-opt'.
;;
;;    Some differences between button-lock.el and hi-lock.el:
;;
;;       * The purpose of hi-lock.el is to change the _appearance_
;;         of keywords.  The purpose of button-lock is to change the
;;         _bindings_ on keywords.
;;
;;       * Hi-lock also supports embedding new keywords in files,
;;         which is too risky of an approach for button-lock.
;;
;;       * Hi-lock supports overlays and can work without font-lock.
;;
;;    Some differences between button-lock.el and buttons.el
;;
;;       * Buttons.el is for inserting individually defined
;;         buttons.  Button-lock.el is for changing all matching text
;;         into a button.
;;
;; Compatibility
;;
;;     Tested only on GNU Emacs version 23.x
;;
;; Bugs
;;
;;     Case-sensitivity of matches depends on how font-lock-defaults
;;     was called for the current mode (setting
;;     font-lock-keywords-case-fold-search).  So, it is safest to
;;     assume that button-lock pattern matches are case-sensistive --
;;     though they might not be.
;;
;;     Return value for button-lock-set-global-button is inconsistent
;;     with button-lock-set-button.  The global function does not
;;     return a button which could be later passed to
;;     button-lock-extend-binding.  The other global functions are
;;     similarly inconsistent; they can only be depended on to return
;;     nil on failure.
;;
;;     button-lock-mode gets activated twice for each buffer when
;;     global-button-lock-mode is on.
;;
;; TODO
;;
;;     Consider defining mode-wide button locks (pass the mode as the
;;     first argument of font-lock-add-keywords).  Could use functions
;;     named eg button-lock-set-mode-button.
;;
;;     language-specific navigation library (header files in C, etc)
;;
;;     example of exchanging text values on wheel event
;;
;;     right-click menus
;;
;;     button-down visual effects as with Emacs widgets
;;
;; License
;;
;;    Simplified BSD License
;;
;;    Copyright (c) 2011, D Roland Walker
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
;;    This software is provided by D Roland Walker "AS IS" and any express
;;    or implied warranties, including, but not limited to, the implied
;;    warranties of merchantability and fitness for a particular
;;    purpose are disclaimed.  In no event shall D Roland Walker or
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
;;    or implied, of D Roland Walker.
;;
;;; Code:
;;

(eval-when-compile
  (require 'font-lock))

(require 'cl)
(autoload 'font-lock-remove-keywords "font-lock" nil nil)

;;;###autoload
(defgroup button-lock nil "Clickable text defined by regexp")

(defcustom button-lock-modestring " bl"
  "This string appears in the modeline when button-lock mode is active."
  :group 'button-lock
  :type 'string)

(defcustom button-lock-exclude-modes
  '(rmail-mode mime/viewer-mode gnus-article-mode term-mode eshell-mode dired-mode bc-menu-mode)
  "List of major modes in which global button-lock will not be activated.

Modes may be excluded for reasons of security (since buttons can
execute arbitrary functions), efficiency, or to avoid conflicts
with modes that provide similar functionality."
  :type '(repeat symbol)
  :group 'button-lock)

(defcustom button-lock-exclude-pattern "\\(^\\*.*\\*$\\)\\|^ "
  "Global button-lock will not be activated in buffers whose names match this regular expression.

Buffers may be excluded for reasons of security (since buttons
can execute arbitrary functions), efficiency, or to avoid
conflicts with modes that provide similar functionality.

The default pattern is designed to match buffers which are
programatically generated or internal to Emacs."
  :type 'string
  :group 'button-lock)

(defface button-lock-button-face
    '((t nil))
    "Face used to show active button-lock buttons.

The default is for buttons to inherit whatever properties are
already provided by font-lock."
    :group 'button-lock)

(defface button-lock-mouse-face
   '((t (:inherit highlight)))
   "Face used to highlight button-lock buttons when the mouse hovers over."
   :group 'button-lock)

(defvar button-lock-global-button-list nil
  "A list of global button definitions to be applied each time the button-lock minor-mode is activated.

The form is a list of lists, each member being a set of arguments
to `button-lock-set-button'.

This variable should be set by calling `button-lock-set-global-button' and friends.")

(defvar button-lock-button-list nil
  "An internal variable used to keep track of button-lock buttons.")

(make-variable-buffer-local 'button-lock-button-list)
(put 'button-lock-button-list 'permanent-local t)

(define-minor-mode button-lock-mode
  "Toggle button-lock-mode, a minor mode for making text clickable.

Button-lock uses `font-lock-mode' to create and maintain its text
properties.  Therefore this mode can only be used where
`font-lock-mode' is active.

When button-lock mode is active, `button-lock-set-button' may be
called to create a new button.  When button-lock mode is
disabled, all button definition are cleared.

With no argument, this command toggles the mode. Non-null prefix
argument turns on the mode.  Null prefix argument turns off the
mode."
  nil button-lock-modestring nil
  (when (or noninteractive (eq (aref (buffer-name) 0) ?\s))  ; don't set up button-lock on hidden or noninteractive
    (setq button-lock-mode nil))                             ; buffers, b/c there will be no font-lock
   (if button-lock-mode
       (progn
         (font-lock-mode 1)
         (button-lock-maybe-fontify-buffer)
         (button-lock-maybe-activate-global-buttons)
         (when (called-interactively-p)
           (message "button-lock mode enabled")))
     (button-lock-unset-all-buttons t)
     (button-lock-maybe-fontify-buffer)
     (when (called-interactively-p)
       (message "button-lock mode disabled"))))

; The define-globalized-minor-mode macro adds some complexity and causes some bugs.
; Specifically, it will cause multiple cycles of on/off toggling at each open, particularly
; when used by both wiki-nav.el and button-lock.el.
;
; This setup does not eliminate multiple invocations of the minor mode.  However it seems
; that the second invocation is the needed one, so no questions asked.
(define-minor-mode global-button-lock-mode
  "Toggle global `button-lock-mode'.

The global mode will cause button-lock to be activated in every buffer,
unless specifically excluded by `button-lock-exclude-modes' or
`button-lock-exclude-pattern',

With no argument, this command toggles the mode. Non-null prefix
argument turns on the mode.  Null prefix argument turns off the
mode."
  nil nil nil
  :global t
  :group 'button-lock
  (if global-button-lock-mode
      (progn
         (dolist (buf (buffer-list))
           (with-current-buffer buf
             (maybe-local-button-lock)))
         ; seems to work fine without find-file hooks
        (add-hook 'after-change-major-mode-hook 'maybe-local-button-lock))
    (remove-hook 'after-change-major-mode-hook  'maybe-local-button-lock)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (maybe-local-button-lock -1)))))

(defun maybe-local-button-lock (&optional arg)
  "Called by global-button-lock-mode to activate button-lock mode in a buffer if appropriate.

If called with a negative ARG, deactivate button-lock mode in the
buffer."
  (setq arg (or arg 1))
  (unless (or button-lock-mode
              (or noninteractive (eq (aref (buffer-name) 0) ?\s))
              (memq major-mode button-lock-exclude-modes)
              (string-match-p button-lock-exclude-pattern (buffer-name (current-buffer))))
    (button-lock-mode arg)))

(defun* button-lock-set-button (pattern action &key

                                 (face 'button-lock-face)
                                 (mouse-face 'button-lock-mouse-face)
                                 (face-policy 'append)
                                 help-text

                                 (grouping 0)

                                 (mouse-binding 'mouse-1)
                                 keyboard-binding
                                 keyboard-action
                                 additional-property
                                 remove

                                 mouse-2
                                 mouse-3
                                 mouse-4
                                 mouse-5
                                 wheel-down
                                 wheel-up

                                 double-mouse-1
                                 double-mouse-2
                                 double-mouse-3
                                 double-mouse-4
                                 double-mouse-5

                                 A-mouse-1
                                 A-mouse-2
                                 A-mouse-3
                                 A-mouse-4
                                 A-mouse-5
                                 A-wheel-down
                                 A-wheel-up

                                 C-mouse-1
                                 C-mouse-2
                                 C-mouse-3
                                 C-mouse-4
                                 C-mouse-5
                                 C-wheel-down
                                 C-wheel-up

                                 M-mouse-1
                                 M-mouse-2
                                 M-mouse-3
                                 M-mouse-4
                                 M-mouse-5
                                 M-wheel-down
                                 M-wheel-up

                                 S-mouse-1
                                 S-mouse-2
                                 S-mouse-3
                                 S-mouse-4
                                 S-mouse-5
                                 S-wheel-down
                                 S-wheel-up

                                 s-mouse-1
                                 s-mouse-2
                                 s-mouse-3
                                 s-mouse-4
                                 s-mouse-5
                                 s-wheel-down
                                 s-wheel-up
                                 )

"Attach mouse actions to text via `font-lock-mode'.

Required argument PATTERN is a regular expression to match.

Required argument ACTION is a function to call when the matching
text is clicked.  A quoted function name or a lambda expression
may be given.  The function called by ACTION must be interactive.
If ACTION is not valid the user may experience a silent failure.

If the function called by ACTION uses (interactive \"e\") it may
receive the relevant mouse event.  Note that you may wish to use
the mouse event to reposition the point.

Following PATTERN and ACTION is a Common Lisp-style series of
keyword/value arguments:

:FACE is a font face to set on matching text, like hi-lock mode.
By default, :FACE has no properties, and :FACE-POLICY is :APPEND.
This means that other, existing text properties will take
priority, and that clickable text will not be distinguished
without a mouseover.  To change this, try adding the arguments
\":face 'link :policy 'prepend.\" Alternatively, `button-lock-face'
may be customized.

:MOUSE-FACE is the font face to set on mouseovers.  It defaults
to `button-lock-mouse-face'.

:FACE-POLICY sets the override policy for button faces.  Useful
values are nil, 'keep, 'prepend, and 'append (the default).  See
the documentation for OVERRIDE in `font-lock-keywords'.

:HELP-TEXT is applied to the help-echo text property, and may
become visible in a tooltip depending on your Emacs setup.

:GROUPING designates a subgroup in the pattern match to receive
the new text properties.  Subgroups, delimited by parentheses,
are numbered from 1.  The default :GROUPING is 0, indicating the
entire match.

:MOUSE-BINDING sets the mouse event which will invoke ACTION.
The default is 'mouse-1.

:KEYBOARD-BINDING sets a keyboard event which will invoke ACTION.
The format is as accepted by `kbd'.  The default is nil, meaning
no keyboard binding is in effect.  If this is set, it might also
be wise to alert the user by setting :FACE.  Note, the only
difference between :MOUSE-BINDING and :KEYBOARD-BINDING is
that :KEYBOARD-BINDING is interpreted by `kbd'.  It is possible
to pass keyboard events into :MOUSE-BINDING and vice versa.

:KEYBOARD-ACTION is an alternate event to be run by
:KEYBOARD-BINDING.  The default is nil, meaning that
:KEYBOARD-BINDING will invoke ACTION.  This is intended for cases
where ACTION is dependent on the position of the mouse. See also
`button-lock-extend-binding' for a general method of adding
alternate bindings.

:ADDITIONAL-PROPERTY defines an arbitrary text property which
will be set to t in for text which matches PATTERN, as optionally
modified by :GROUPING. The property 'button-lock will always be
set.

As a convenience, :MOUSE-2 through :MOUSE-5 can be used to attach
an alternate ACTION, as can :M-MOUSE-1 ..., :A-MOUSE-1 ...,
:DOUBLE-MOUSE-1 ..., :WHEEL-UP..., and :WHEEL-DOWN... The list is
not exhaustive.  For a general method of adding alternate
bindings, see `button-lock-extend-binding'.

If :REMOVE is non-nil, any button matching the exact properties
given will be removed and forgotten by font-lock.

If successful, this function returns the button which was added
or removed from `font-lock-keywords'. Otherwise it returns nil.
The button value can be passed to `button-lock-extend-binding'."

  (if (not button-lock-mode)
      (progn
        (message "button-lock mode is not active")
        nil)

    ; else (not button-lock-mode)
    (let ((map (make-sparse-keymap))
          (properties nil)
          (success nil)
          (fl-keyword nil))

      (define-key map `[,mouse-binding] action)

      (dolist (var '(
                     mouse-2
                     mouse-3
                     mouse-4
                     mouse-5
                     wheel-down
                     wheel-up

                     double-mouse-1
                     double-mouse-2
                     double-mouse-3
                     double-mouse-4
                     double-mouse-5

                     A-mouse-1
                     A-mouse-2
                     A-mouse-3
                     A-mouse-4
                     A-mouse-5
                     A-wheel-down
                     A-wheel-up

                     C-mouse-1
                     C-mouse-2
                     C-mouse-3
                     C-mouse-4
                     C-mouse-5
                     C-wheel-down
                     C-wheel-up

                     M-mouse-1
                     M-mouse-2
                     M-mouse-3
                     M-mouse-4
                     M-mouse-5
                     M-wheel-down
                     M-wheel-up

                     S-mouse-1
                     S-mouse-2
                     S-mouse-3
                     S-mouse-4
                     S-mouse-5
                     S-wheel-down
                     S-wheel-up

                     s-mouse-1
                     s-mouse-2
                     s-mouse-3
                     s-mouse-4
                     s-mouse-5
                     s-wheel-down
                     s-wheel-up
                     ))
        (when (symbol-value var)
          (define-key map `[,var] (symbol-value var))))

      (when keyboard-binding
        (define-key map (eval `(kbd ,keyboard-binding)) (or keyboard-action action)))

      (setq props `(face ,face keymap ,map button-lock t))
      (add-to-list 'font-lock-extra-managed-props 'keymap)
      (add-to-list 'font-lock-extra-managed-props 'button-lock)

      (when additional-property
        (setq props (append props `(,additional-property t)))
        (add-to-list 'font-lock-extra-managed-props additional-property))

      (when mouse-face
        (setq props (append props `(mouse-face ,mouse-face)))
        (add-to-list 'font-lock-extra-managed-props 'mouse-face))

      (when help-text
        (setq props (append props `(help-echo ,help-text)))
        (add-to-list 'font-lock-extra-managed-props 'help-echo))

      (setq fl-keyword `((,pattern (,grouping ',props ,face-policy))))

      (if remove
          (progn
            (condition-case nil
                (setq success (font-lock-remove-keywords nil fl-keyword))
              (error nil))
            (when success
              (setq button-lock-button-list (delete fl-keyword button-lock-button-list)))
            (button-lock-maybe-unbuttonify-buffer))   ; cperl-mode workaround
        ; else
        (condition-case nil
            (setq success (font-lock-add-keywords nil fl-keyword))
          (error nil))
        (when success
          (add-to-list 'button-lock-button-list fl-keyword)))
      (button-lock-maybe-fontify-buffer)
      (if success
          fl-keyword
        nil))))

(defmacro button-lock-unset-button (&rest args)
  "This is equivalent to running `button-lock-set-button' with :REMOVE set to true.

The syntax is otherwise identical to `button-lock-set-button',
which see."
  `(button-lock-set-button ,@args :remove t))

(defun button-lock-extend-binding (existing-button action mouse-binding &optional keyboard-binding)
  "Add a binding to an existing button.

The principal button creation function `button-lock-set-button'
accepts only a limited subset of mouse bindings when binding
multiple actions.  This function supports arbitrary key bindings
for binding additional actions on a button.

EXISTING-BUTTON is a button value as returned by
`button-lock-set-button'.

ACTION, MOUSE-BINDING and KEYBOARD-BINDING are as documented in
`button-lock-set-button'.  It is possible to pass a nil
MOUSE-BINDING in order to set only a KEYBOARD-BINDING."
  (if (and (not (member existing-button button-lock-button-list))
           (not (member (car existing-button) (cdr (cdr font-lock-keywords)))))
      (progn
        (message "no such button")
        nil)
    ; else
    (let ((success nil)
          (map nil))
      (condition-case nil
          (setq success (font-lock-remove-keywords nil existing-button))
        (error nil))
      (if (not success)
          nil
        ; else
        (setq map (nth 3 (nth 1 (nth 1 (nth 1 (car (car (member existing-button button-lock-button-list))))))))
        (when mouse-binding
          (define-key map `[,mouse-binding] action))
        (when keyboard-binding
          (define-key map (eval `(kbd ,keyboard-binding)) action))
        (setf (nth 3 (nth 1 (nth 1 (nth 1 (car (car (member existing-button button-lock-button-list))))))) map)
        (setf (nth 3 (nth 1 (nth 1 (nth 1 (car existing-button))))) map)
        (condition-case nil
            (setq success (font-lock-add-keywords nil existing-button))
          (error nil))
        success))))

(defun button-lock-pop-button (&optional first force)
"Unset the most recently added button-lock button.

If FIRST is non-nil, instead unset the earliest-added button.

If FORCE is non-nil, try to remove buttons even when the minor
mode is not active.

When successful, returns the number of button patterns removed,
which should always be 1."
  (interactive)
   (if (and (not force)
            (not button-lock-mode))
       (progn
         (message "button-lock mode is not active")
         nil)
     ; else
     (if (not button-lock-button-list)
         nil
       ; else
       (let ((fl-keyword nil)
             (num (length button-lock-button-list)))
         (when first
           (setq button-lock-button-list (nreverse button-lock-button-list)))
         (setq fl-keyword (pop button-lock-button-list))
         (when first
           (setq button-lock-button-list (nreverse button-lock-button-list)))
         (font-lock-remove-keywords nil fl-keyword)
         (button-lock-maybe-unbuttonify-buffer) ; cperl-mode workaround
         (button-lock-maybe-fontify-buffer)
         (setq num (- num (length button-lock-button-list)))
         (when (called-interactively-p)
           (message "removed %d button patterns" num))
         num))))

(defun button-lock-unset-all-buttons (&optional force)
  "Unset all active button-lock buttons.

If FORCE is non-nil, try to remove buttons even when the minor
mode is not active."
  (interactive)
  (if (and (not force)
           (not button-lock-mode))
      (progn
        (message "button-lock mode is not active")
        nil)
    ; else
    (let ((fl-keyword nil)
          (num (length button-lock-button-list)))
      (while (setq fl-keyword (pop button-lock-button-list))
        (font-lock-remove-keywords nil fl-keyword))
      (button-lock-maybe-unbuttonify-buffer)   ; cperl-mode workaround
      (button-lock-maybe-fontify-buffer)
      (when (and
             (called-interactively-p)
             num)
        (message "removed %d button patterns" num))
      num)))

(defun button-lock-set-global-button (args)
  "Add a global button-lock button definition, to be applied each time the button-lock minor mode is activated.

ARGS is a list of arguments, following the form of
`button-lock-set-button'.

To see an effect, button-lock mode must be deactivated and
reactivated."
  (add-to-list 'button-lock-global-button-list args))

(defun button-lock-unset-global-button (args)
  "Remove a global button-lock button definition.

ARGS is a list of arguments, following the form of
`button-lock-set-button'.

To see an effect, button-lock mode must be deactivated and
reactivated."
  (if (member args button-lock-global-button-list)
      (setq button-lock-global-button-list (delete args button-lock-global-button-list))
    nil))

(defun button-lock-pop-global-button (&optional first)
"Unset the most recently added global button-lock button definition.

If FIRST is non-nil, instead unset the earliest-added button
definition.

To see an effect, button-lock mode must be deactivated and
reactivated.

When successful, this function returns the number of button
definitions removed, which should always be 1."
  (interactive)
  (if (not button-lock-global-button-list)
      nil
    ; else
    (let ((num (length button-lock-global-button-list)))
      (when first
        (setq button-lock-global-button-list (nreverse button-lock-global-button-list)))
      (pop button-lock-global-button-list)
      (when first
        (setq button-lock-global-button-list (nreverse button-lock-global-button-list)))
      (setq num (- num (length button-lock-global-button-list)))
      (when (called-interactively-p)
        (message "removed %d button patterns" num))
      num)))

(defun button-lock-unset-all-global-buttons ()
  "Unset all global button-lock buttons definitions.

To see an effect, button-lock mode must be deactivated and
reactivated."
  (interactive)
  (setq button-lock-global-button-list nil)
  t)

(defun button-lock-maybe-activate-global-buttons ()
  "If there are any predefined global buttons, activate them."
  (dolist (button button-lock-global-button-list)
    (eval `(button-lock-set-button ,@button))))

(defun button-lock-find-extent (&optional pos property)
  "Find the extent of a button-lock property around some point.

POS defaults to the current point.  PROPERTY defaults to
'button-lock.

Returns list containing the start and end, or nil if there is no
such property around the point."
  (setq pos (or pos (point)))
  (let ((start nil)
        (end nil))
    (setq property (or property 'button-lock))
    (if (not (get-text-property pos property))
        nil
      ; else
      (setq start pos)
      (setq end pos)
      (while (and (> start (point-min))
                  (get-text-property (- start 1) property))
        (setq start (- start 1)))
      (while (and (< end (point-max))
                  (get-text-property (+ end 1) property))
        (setq end (+ end 1)))
      (setq end (min (point-max) (+ end 1)))
      (list start end))))

(defun button-lock-maybe-unbuttonify-buffer ()
  "This is a workaround for cperl mode, which clobbers `font-lock-unfontify-region-function'."
  (when (and (boundp 'font-lock-fontified)
             font-lock-fontified
             (not (eq font-lock-unfontify-region-function 'font-lock-default-unfontify-region)))
    (font-lock-default-unfontify-region (point-min) (point-max))))

(defun button-lock-maybe-fontify-buffer ()
  "This is to avoid turning on font-lock if we are currently in the process of disabling button-lock."
  (when (and (boundp 'font-lock-fontified)
             font-lock-fontified)
    (font-lock-fontify-buffer)))

(provide 'button-lock)
;;; button-lock.el ends here
