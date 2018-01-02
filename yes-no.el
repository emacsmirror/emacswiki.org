;;; yes-no.el --- Specify use of `y-or-n-p' or `yes-or-no-p' on a case-by-case basis
;; 
;; Filename: yes-no.el
;; Description: Specify use of `y-or-n-p' or `yes-or-no-p' on a case-by-case basis
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2017-2018, Drew Adams, all rights reserved.
;; Created: Thu Aug 17 11:12:21 2017 (-0700)
;; Version: 0
;; Package-Requires: ()
;; Last-Updated: Mon Jan  1 16:26:36 2018 (-0800)
;;           By: dradams
;;     Update #: 507
;; URL: https://www.emacswiki.org/emacs/download/yes-no.el
;; Doc URL: 
;; Keywords: help
;; Compatibility: GNU Emacs: 22.x, 23.x, 24.x, 25.x, 26.x
;; 
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary:
;;
;;    Let a user decide, for any given yes/no confirmation prompt,
;;    whether it should use `y-or-n-p' or `yes-or-no-p' from then on.
;;
;;  Emacs has two simple functions to prompt a user for yes/no
;;  confirmation: `y-or-n-p' and `yes-or-no-p'.  It is a judgment call
;;  which is more appropriate to use in any given context.
;;
;;  `y-or-n-p' acts as soon as the user hits key `y' or key `n'.
;;  `yes-or-no-p' requires the user to type `yes' or `no' and then hit
;;  `RET'.  `y-or-n-p' is generally used for quick interaction where
;;  the consequences of mistaken input are not too important.
;;  `yes-or-no-p' is generally used for interaction where the user is
;;  expected to reply carefully, especially if the consequences of a
;;  mistaken reply could be important.
;;
;;  In order to allow faster interaction, some users have customized
;;  Emacs to simply replace the `yes-or-no-p' behavior by that of
;;  `y-or-n-p'.  That might be appropriate for some users, but it is
;;  probably not a great idea in general.
;;
;;  Which of the two functions a Lisp programmer chooses for a given
;;  context might not be what any given user prefers for that context.
;;  A missing feature is to let users in on the design of a given
;;  interaction.
;;
;;  This library lets a user decide, for any given function that
;;  prompts with one of these confirmation functions, which of the
;;  behaviors to use.  It does this by allowing, besides the y/n and
;;  yes/no responses, a response that says "Switch to using the other
;;  prompting method from now on".  It thus lets users change the
;;  prompting function interactively, for the given calling function.
;;
;;  To provide this feature, the code that prompts needs to know which
;;  function is currently calling the yes/no prompting function.  For
;;  that, `y-or-n-p' and `yes-or-no-p' are redefined so that they
;;  include an optional argument CALLER, which is the symbol that
;;  names the function that is calling.  As long as the CALLER
;;  argument is supplied, users can choose, on demand, which function
;;  to be prompted with.
;;
;;  When argument CALLER is provided:
;;
;;  * If `y-or-n-p' is called, a user response of character `e',
;;    instead of `y' or `n', switches to `yes-or-no-p', and from then
;;    on `yes-or-no-p' is used for `y-or-n-p' calls from CALLER.
;;
;;  * If `yes-or-no-p' is called, a user response of `use-y-or-n',
;;    instead of `yes' or `no', switches to `y-or-n-p', and from then
;;    on `y-or-n-p' is used for `yes-or-no-p' calls from CALLER.
;;
;;  * In addition, if a particular prompting behavior has been decided
;;    for a given CALLER, a user can choose to return that CALLER to
;;    the default behavior (no redirection).  If currently prompted by
;;    `yes-or-no-p' then a user does this with a response of
;;    `reset-to-default'.  If prompted by `y-or-n-p' then a user does
;;    it using character `^'.
;;
;;  When `yes-or-no-p' prompts and either `use-y-or-n' or
;;  `reset-to-default' is available as a response, completion is
;;  available for these inputs.  In addition, if option
;;  `yn-allow-completion-for-yes/no-flag' is non-nil then
;;  `yes-or-no-p' provides completion for `yes' and `no' also.
;;
;;  Examples of use: the first sexp here does not change the behavior
;;  - there is no CALLER arg.  The others let the user switch between
;;  the two functions if either is called from
;;  `help-mode-revert-buffer'.
;;
;;  (yes-or-no-p "Agree? ") ; No change in behavior.
;;  (yes-or-no-p "Agree? " 'help-mode-revert-buffer) ; Let user switch
;;  (y-or-n-p    "Agree? " 'help-mode-revert-buffer) ; Let user switch
;;
;;  To take advantage of this library, consider adding CALLER to your
;;  calls of `yes-or-no-p' and `y-or-n-p'.
;;
;;  As a user, you can set your prompting preference for any callers
;;  you like, by customizing option `yn-prompting-alist'.  The entries
;;  in this alist associate a caller with a prompting function
;;  (`yes-or-no-p' or `y-or-n-p').
;;
;;  For example, to prefer `y-or-n-p' for the prompting done by
;;  `help-mode-revert-buffer' you would customize the option so that
;;  it has this entry: (help-mode-revert-buffer . y-or-n-p).
;;
;;  User options defined here:
;;
;;    `yn-allow-caller-specific-yes/no-flag',
;;    `yn-allow-completion-for-yes/no-flag', `yn-prompting-alist'.
;;
;;
;;  ***** NOTE: The following predefined functions have been REDEFINED
;;              HERE:
;;
;;    `yes-or-no-p', `y-or-n-p'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;; 
;;(@* "Change log")
;;
;; 2017/10/08 dadams
;;     Added option yn-prompting-alist.  Change from using a symbol property to using an alist.
;; 2017/10/01 dadams
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup yes-no nil
  "Specify yes/no behavior for a function prompting for confirmation.
This affects the use of `y-or-n-p' and `yes-or-no-p' when they are
called with an optional function name, by specifying which behavior is
used when that function calls one of these confirmation functions."
  :prefix "yn-"
  :group 'editing :group 'help :group 'convenience :group 'development :group 'environment
  :link `(url-link :tag "Send Bug Report"
                   ,(concat "mailto:" "drew.adams" "@" "oracle" ".com?subject=yes-no.el bug: \
&body=Describe bug here, starting with `emacs -q'.  \
Don't forget to mention your Emacs and library versions."))
  :link '(url-link :tag "Other Libraries by Drew"
          "https://www.emacswiki.org/emacs/DrewsElispLibraries")
  :link '(url-link :tag "Download"
          "https://www.emacswiki.org/emacs/download/yes-no.el"))

(defcustom yn-allow-caller-specific-yes/no-flag t
  "*Non-nil means let users specify yes/no prompting for a given caller.
This means that an invocation of `yes-or-no-p' or `y-or-n-p' that
passes a second, function-symbol CALLER argument uses whichever of
those prompting functions is the value of symbol CALLER's entry in
option `yn-prompting-alist'.  If there is no CALLER entry in that
option value then there no change to the yes/no prompting in CALLER.

A nil value means use the default yes/no prompting behavior for all
callers; ignore any optional CALLER function for `yes-or-no-p' and
`y-or-n-p'.

See also option `yn-allow-completion-for-yes/no-flag'."
  :type 'boolean :group 'yes-no)

(defcustom yn-allow-completion-for-yes/no-flag nil
  "*Non-nil allows completion for `yes' and `no' answers to `yes-or-no-p'.
Note that if `yn-allow-caller-specific-yes/no-flag' is non-nil then
completion is allowed for answers other than `yes' and `no'."
  :type 'boolean :group 'yes-no)

(defcustom yn-prompting-alist ()
  "Alist defining prompting for specific caller functions.
Entries have the form (CALLER . PROMPT-FUNCTION), where CALLER is a
function defined to call prompting function `yes-or-no-p' or
`y-or-n-p' and PROMPT-FUNCTION is one of those prompting functions.

The effect of entry (CALLER . PROMPT-FUNCTION) is to use
PROMPT-FUNCTION in CALLER, in place of whatever yes/no prompt function
was defined for it.

PROMPT-FUNCTION can also be nil, which acts the same as if
\(CALLER . nil) is absent."
  :group 'yes-no
  :type '(alist
          :key-type   (symbol :tag "Caller function")
          :value-type (choice
                       (const :tag "Prompt using `yes-or-no-p'"             yes-or-no-p)
                       (const :tag "Prompt using `y-or-n-p'"                y-or-n-p)
                       (const :tag "Default - Prompt as originally defined" nil))))

(defun yes-or-no-p (prompt &optional caller)
  "Ask user a yes-or-no question.
Return t if the answer is `yes', and nil if the answer is `no'.

PROMPT is the string to display to ask the question.  It should end in
a space; `yes-or-no-p' adds \"(yes or no) \" to it.

The user must confirm the answer with `RET', and can edit the
input until `RET' is used.

Under a windowing system, a dialog box is used if `last-nonmenu-event'
is nil and `use-dialog-box' is non-nil.

If CALLER is non-nil then it is the symbol of a function that calls
`yes-or-no-p'.  In this case:

* If the value of CALLER in option `yn-prompting-alist' is `y-or-n-p'
  then just call `y-or-n-p'.  This means that CALLER should interpret
  `yes-or-no-p' as `y-or-n-p'.

* If the value of CALLER in option `yn-prompting-alist' is
  `yes-or-no-p' then the user can also answer `reset-to-default',
  meaning remove any CALLER entry from `yn-prompting-alist', restoring
  CALLER to the default prompting behavior.

* Otherwise, the user can also answer `use-y-or-n', meaning update
  option `yn-prompting-alist' to give CALLER the value `y-or-n-p'
  there, and call `y-or-n-p' immediately.  That is, answer
  `use-y-or-n' means that function `yes-or-no-p' uses `y-or-n-p' for
  CALLER from now on.

Whenever the value of option `yn-prompting-alist' is changed it is
also saved.  That is, the effect is persistent between Emacs
sessions."
  (let* ((allow-y-n  (and caller  yn-allow-caller-specific-yes/no-flag))
         (allow-def  (and allow-y-n  (eq 'yes-or-no-p (cdr (assq caller yn-prompting-alist))))))
    (if (and allow-y-n  (eq 'y-or-n-p (cdr (assq caller yn-prompting-alist))))
        (y-or-n-p prompt caller)
      (let ((msg-will-use-def  (and allow-def
                                    (format "Function `%s' will use DEFAULT yes/no prompting from now on" caller)))
            (msg-will-use-y    (and allow-y-n
                                    (format "Function `%s' will use `Y-OR-N-P' prompting from now on"     caller))))
        (if (and (fboundp 'display-popup-menus-p)
                 (display-popup-menus-p)
                 last-input-event       ; Not during startup
                 (listp last-nonmenu-event)
                 use-dialog-box)
            (let ((answer  (x-popup-dialog
                            t
                            `(,prompt ("Yes" . act)
                                      ("No"  . skip)
                                      ,(and allow-y-n  '("Use `y-or-n-p' from now on"               . edit-replacement))
                                      ,(and allow-def  '("Use DEFAULT yes/no prompting from now on" . backup))))))
              (cond ((and (eq answer 'backup)  msg-will-use-def)
                     (message msg-will-use-def)
                     (sit-for 3.0)
                     (customize-save-variable 'yn-prompting-alist
                                              (delete (assq caller yn-prompting-alist) yn-prompting-alist))
                     (yes-or-no-p prompt))
                    ((and (eq answer 'edit-replacement)  msg-will-use-y)
                     (setq-default yn-prompting-alist
                                   (delete (assq caller yn-prompting-alist) yn-prompting-alist))
                     (customize-save-variable 'yn-prompting-alist
                                              (push (cons caller 'y-or-n-p) yn-prompting-alist))
                     (message msg-will-use-y)
                     (sit-for 3.0)
                     (y-or-n-p prompt caller))
                    ((eq answer 'act))))
          (let* ((prmpt   (concat prompt "(yes or no) "))
                 (input   (intern (if yn-allow-completion-for-yes/no-flag
                                      (completing-read prmpt `("yes"
                                                               "no"
                                                               ,@(and allow-y-n  '("use-y-or-n"))
                                                               ,@(and allow-def  '("reset-to-default"))))
                                    (if allow-y-n
                                        (completing-read prmpt `("use-y-or-n" ,@(and allow-def  '("reset-to-default"))))
                                      (read-from-minibuffer prmpt)))))
                 (nogood  (not (memq input `(yes
                                             no
                                             ,@(and allow-y-n  '(use-y-or-n))
                                             ,@(and allow-def  '(reset-to-default)))))))
            (cond ((eq input 'yes)
                   t)
                  (nogood
                   (message "Please answer %s"
                            (cond ((and allow-y-n  allow-def) "`yes', `no', `use-y-or-n', or `reset-to-default'")
                                  (allow-y-n                  "`yes', `no', or `use-y-or-n'")
                                  (allow-def                  "`yes', `no', or `reset-to-default'")
                                  (t                          "`yes' or `no'")))
                   (sit-for 3.0)
                   (yes-or-no-p prompt caller))
                  ((and msg-will-use-def  (eq input 'reset-to-default))
                   (message msg-will-use-def)
                   (sit-for 3.0)
                   (customize-save-variable 'yn-prompting-alist
                                            (delete (assq caller yn-prompting-alist) yn-prompting-alist))
                   (yes-or-no-p prompt))
                  ((and msg-will-use-y  (eq input 'use-y-or-n))
                   (message msg-will-use-y)
                   (sit-for 3.0)
                   (setq-default yn-prompting-alist
                                 (delete (assq caller yn-prompting-alist) yn-prompting-alist))
                   (customize-save-variable 'yn-prompting-alist
                                            (push (cons caller 'y-or-n-p) yn-prompting-alist))
                   (y-or-n-p prompt caller)))))))))


(unless (fboundp 'ignore-errors)        ; < Emacs 23
  (defmacro ignore-errors (&rest body)
    "Execute BODY; if an error occurs, return nil.
Otherwise, return result of last form in BODY.
See also `with-demoted-errors' that does something similar
without silencing all errors."
    (declare (debug t) (indent 0)) `(condition-case nil (progn ,@body) (error nil))))



;; REPLACE ORIGINAL in `subr.el'.
;;
;; 1. Added optional arg CALLER.
;; 2. Non-nil CALLER with input `e' means CALLER calls `yes-or-no-p' in place of `y-or-n-p' from now on.
;; 3. Non-nil CALLER with input `^' means CALLER is reset to use default behavior from now on.
;; 4. Adapt to be usable with older Emacs versions.
;;
(defun y-or-n-p (prompt &optional caller)
  "Ask user a \"y or n\" question.
Return t if answer is \"y\" and nil if it is \"n\".
PROMPT is the string to display to ask the question.  It should
end in a space; `y-or-n-p' adds \"(y or n) \" to it.

No confirmation of the answer is requested; a single character is
enough.  SPC also means yes, and DEL means no.

To be precise, this function translates user input into responses by
consulting the bindings in `query-replace-map'; see the documentation
of that variable for more information.  In this case, the useful
bindings are `act', `skip', `recenter', `scroll-up', `scroll-down',
`quit', and `edit-replacement' (bound to \"e\").

An `act' response means yes, and a `skip' response means no.  A `quit'
response means to invoke `keyboard-quit'.  If the user enters a
`recenter', `scroll-up', or `scroll-down' response then perform the
requested window recentering or scrolling and ask again.

Under a windowing system, a dialog box is used if `last-nonmenu-event'
is nil and `use-dialog-box' is non-nil.

If CALLER is non-nil then it is the symbol of a function that calls
`y-or-n-p'.  In this case:

* If the value of CALLER in option `yn-prompting-alist' is
  `yes-or-no-p' then just call `yes-or-no-p'.  This means that CALLER
  should interpret `y-or-n-p' as `yes-or-no-p'.

* If the value of CALLER in option `yn-prompting-alist' is `y-or-n-p'
  then the user can also answer \"^\", meaning remove any CALLER entry
  from `yn-prompting-alist', restoring CALLER to the default behavior.

* Otherwise, the user can also answer \"e\", meaning update option
  `yn-prompting-alist' to give CALLER the value `yes-or-no-p' there,
  and call `yes-or-no-p' immediately.  That is, answer \"e\" means
  that function `y-or-n-p' uses `yes-or-no-p' for CALLER from now on.

Whenever the value of option `yn-prompting-alist' is changed it is
also saved.  That is, the effect is persistent between Emacs
sessions."
  (if (and caller
           (eq 'yes-or-no-p (cdr (assq caller yn-prompting-alist)))
           yn-allow-caller-specific-yes/no-flag)
      (yes-or-no-p prompt caller)
    (let* ((allow-y-n  (and caller  yn-allow-caller-specific-yes/no-flag))
           (allow-def  (and allow-y-n  (eq 'y-or-n-p (cdr (assq caller yn-prompting-alist)))))
           (answer     'recenter)
           (oprompt    prompt)
           (padded     (lambda (prompt &optional dialog)
                         (let ((len  (length prompt)))
                           (concat prompt
                                   (if (or (zerop len)  (eq ?\s- (aref prompt (1- len)))) "" " ")
                                   (if dialog "" "(y or n) "))))))
      (cond (noninteractive
             (setq prompt  (funcall padded prompt))
             (let ((temp-prompt  prompt)
                   (reprompt-p   t))
               (while reprompt-p
                 (let ((str  (read-string temp-prompt)))
                   (cond ((member str '("y" "Y")) (setq answer      'act
                                                        reprompt-p  nil))
                         ((member str '("n" "N")) (setq answer      'skip
                                                        reprompt-p  nil))
                         ((and allow-def  (string= str "^"))
                          (message "Function `%s' will use DEFAULT yes/no prompting from now on" caller)
                          (sit-for 3.0)
                          (customize-save-variable 'yn-prompting-alist
                                                   (delete (assq caller yn-prompting-alist) yn-prompting-alist)))
                         ((and allow-y-n  (member str '("e" "E")))
                          (setq-default yn-prompting-alist
                                        (delete (assq caller yn-prompting-alist) yn-prompting-alist))
                          (customize-save-variable 'yn-prompting-alist
                                                   (push (cons caller 'yes-or-no-p) yn-prompting-alist))
                          (message "Function `%s' will use `YES-OR-NO-P' prompting from now on" caller)
                          (sit-for 3.0)
                          (setq answer      'exit
                                reprompt-p  nil)
                          (yes-or-no-p oprompt caller))
                         ((and allow-def  allow-y-n)
                          (setq temp-prompt  (concat "Please answer y or n (or e or ^).  " prompt)))
                         (allow-y-n (setq temp-prompt  (concat "Please answer y or n (or e).  " prompt)))
                         (allow-def (setq temp-prompt  (concat "Please answer y or n (or ^).  " prompt)))
                         (t (setq temp-prompt  (concat "Please answer y or n.  " prompt))))))))
            ((and (fboundp 'display-popup-menus-p)
                  (display-popup-menus-p)
                  last-input-event      ; Not during startup
                  (listp last-nonmenu-event)
                  use-dialog-box)
             (setq prompt  (funcall padded prompt t)
                   answer  (x-popup-dialog
                            t
                            `(,prompt ("Yes" . act)
                                      ("No" . skip)
                                      ,(and allow-y-n  '("Use `yes-or-no-p' from now on"            . edit-replacement))
                                      ,(and allow-def  '("Use DEFAULT yes/no prompting from now on" . backup)))))
             (cond ((and allow-def  (eq answer 'backup))
                    (message "Function `%s' will use DEFAULT yes/no prompting from now on" caller)
                    (sit-for 3.0)
                    (customize-save-variable 'yn-prompting-alist
                                             (delete (assq caller yn-prompting-alist) yn-prompting-alist))
                    (y-or-n-p oprompt))
                   ((and allow-y-n  (eq answer 'edit-replacement))
                    (setq-default yn-prompting-alist
                                  (delete (assq caller yn-prompting-alist) yn-prompting-alist))
                    (customize-save-variable 'yn-prompting-alist
                                             (push (cons caller 'yes-or-no-p) yn-prompting-alist))
                    (message "Function `%s' will use `YES-OR-NO-P' prompting from now on" caller)
                    (sit-for 3.0)
                    (setq answer  'exit)
                    (yes-or-no-p oprompt caller))))
            (t
             (setq prompt  (funcall padded prompt))
             (while (let* ((scroll-actions  '(recenter scroll-up scroll-down scroll-other-window scroll-other-window-down))
                           (temp-prompt     (cond ((and allow-def  allow-y-n)
                                                   (concat "Please answer y or n (or e or ^).  " prompt))
                                                  (allow-y-n
                                                   (concat "Please answer y or n (or e).  " prompt))
                                                  (allow-def
                                                   (concat "Please answer y or n (or ^).  " prompt))
                                                  (t (concat "Please answer y or n.  " prompt))))
                           (key             (let ((cursor-in-echo-area  t))
                                              (when minibuffer-auto-raise (raise-frame (window-frame (minibuffer-window))))
                                              (if (fboundp 'read-key)
                                                  (read-key (propertize
                                                             (if (memq answer scroll-actions) prompt temp-prompt)
                                                             'face 'minibuffer-prompt))
                                                (read-char-exclusive
                                                 (if (memq answer scroll-actions) prompt temp-prompt))))))
                      (setq answer  (if (memq key '(?e ?E))
                                        'edit-replacement
                                      (lookup-key query-replace-map (vector key) t)))
                      (cond ((and allow-def  (eq answer 'backup))
                             (message "Function `%s' will use DEFAULT yes/no prompting from now on" caller)
                             (sit-for 3.0)
                             (customize-save-variable 'yn-prompting-alist
                                                      (delete (assq caller yn-prompting-alist) yn-prompting-alist))
                             (y-or-n-p oprompt))
                            ((and allow-y-n  (eq answer 'edit-replacement))
                             (setq-default yn-prompting-alist
                                           (delete (assq caller yn-prompting-alist) yn-prompting-alist))
                             (customize-save-variable 'yn-prompting-alist
                                                      (push (cons caller 'yes-or-no-p) yn-prompting-alist))
                             (message "Function `%s' will use `YES-OR-NO-P' prompting from now on" caller)
                             (sit-for 3.0)
                             (setq answer  'exit)
                             (yes-or-no-p oprompt caller))
                            ((memq answer '(skip act))
                             nil)
                            ((eq answer 'recenter)
                             (recenter)
                             t)
                            ((eq answer 'scroll-up)
                             (if (fboundp 'scroll-up-command)
                                 (ignore-errors (scroll-up-command))
                               (ignore-errors (scroll-up)))
                             t)
                            ((eq answer 'scroll-down)
                             (if (fboundp 'scroll-down-command)
                                 (ignore-errors (scroll-down-command))
                               (ignore-errors (scroll-down)))
                             t)
                            ((eq answer 'scroll-other-window)
                             (ignore-errors (scroll-other-window))
                             t)
                            ((eq answer 'scroll-other-window-down)
                             (ignore-errors (scroll-other-window '-))
                             t)
                            ((or (memq answer '(exit-prefix quit))  (eq key ?\e))
                             (signal 'quit nil) t)
                            (t t)))
               (ding)
               (discard-input))))
      (let ((ret  (eq answer 'act)))
        (unless noninteractive (message "%s%c" prompt (if ret ?y ?n)))
        ret))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'yes-no)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; yes-no.el ends here
