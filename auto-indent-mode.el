;;;; auto-indent-mode.el --- Auto indent Minor mode
;;
;; Filename: auto-indent-mode.el
;; Description: Auto Indent text on Yank/Paste
;; Author: Matthew L. Fidler, Le Wang & Others
;; Maintainer: Matthew L. Fidler
;; Created: Sat Nov  6 11:02:07 2010 (-0500)
;; Version: 0.3
;; Last-Updated: Tue Feb  1 23:01:01 2011 (-0600)
;;           By: Matthew L. Fidler
;;     Update #: 672
;; URL: http://www.emacswiki.org/emacs/auto-indent-mode.el
;; Keywords: Auto Indentation
;; Compatibility: Tested with Emacs 23.x
;;
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  Provides auto-indentation minor mode.  This allows the following:
;;
;;  (1) Return automatically indents the code appropriately (if enabled)
;;
;;  (2) Pasting/Yanking indents the appropriately
;;
;;  (3) Killing line will take off unneeded spaces (if enabled)
;;
;;  (4) On visit file, indent appropriately, but DONT SAVE. (Pretend like
;;  nothing happened, if enabled)
;;
;;  (5) On save, optionally unttabify, remove trailing white-spaces, and
;;  definitely indent the file (if enabled).
;;
;;  (6) TextMate behavior of keys if desired (see below)
;;
;;  All of these options can be customized. (customize auto-indent)
;;
;;  To use put this in your load path and then put the following in your emacs
;;  file:
;;
;;  (setq auto-indent-on-visit-file t) ;; If you want auto-indent on for files
;;  (require 'auto-indent-mode)
;;
;;  If you (almost) always want this on, add the following to ~/.emacs:
;;
;;  (auto-indent-global-mode)
;;
;;  Excluded modes are defined in `auto-indent-disabled-modes-list'
;;
;;  If you only want this on for a single mode, you would add the following to
;;  ~/.emacs
;;
;;  (add-hook 'emacs-lisp-mode-hook 'auto-indent-minor-mode)
;;
;;  You could always turn on the minor mode with the command
;;  `auto-indent-minor-mode'
;;
;;  If you would like TextMate behavior of Meta-RETURN going to the
;;  end of the line and then inserting a newline, as well as
;;  Meta-shift return going to the end of the line, inserting a
;;  semi-colon then inserting a newline, use the following:
;;
;;
;;  (setq auto-indent-key-for-end-of-line-then-newline "<M-return>")
;;  (setq auto-indent-key-for-end-of-line-insert-char-then-newline "<M-S-return>")
;;  (require 'auto-indent-mode)
;;  (auto-indent-global-mode)
;;
;;  This may or may not work on your system.  Many times emacs cannot
;;  distinguish between M-RET and M-S-RET, so if you don't mind a
;;  slight redefinition use:
;;
;;  (setq auto-indent-key-for-end-of-line-then-newline "<M-return>")
;;  (setq auto-indent-key-for-end-of-line-insert-char-then-newline "<C-M-return>")
;;  (require 'auto-indent-mode)
;;  (auto-indent-global-mode)
;;
;;
;;  If you want to insert something other than a semi-colon (like a
;;  colon) in a specific mode, say colon-mode, do the following:
;;
;;  (add-hook 'colon-mode-hook (lambda () (setq auto-indent-eol-char ":")))
;;
;;
;;  If you wish to use this with autopairs and yasnippet, please load
;;  this library first.
;;
;;  Also if you wish to just use specific functions from this library
;;  that is possible as well.
;;
;;  To have the auto-indentation-paste use:
;;
;;  (autoload 'auto-indent-yank "auto-indent-mode" "" t)
;;  (autoload 'auto-indent-yank-pop "auto-indent-mode" "" t)
;;
;;  (define-key global-map [remap yank] 'auto-indent-yank)
;;  (define-key global-map [remap yank-pop] 'auto-indent-yank-pop)
;;
;;  (autoload 'auto-indent-delete-char "auto-indent-mode" "" t)
;;  (define-key global-map [remap delete-char] 'auto-indent-delete-char)
;;
;;  (autoload 'auto-indent-kill-line "auto-indent-mode" "" t)
;;  (define-key global-map [remap kill-line] 'auto-indent-kill-line)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;; 01-Feb-2011    Matthew L. Fidler  
;;    Last-Updated: Tue Feb  1 22:58:45 2011 (-0600) #667 (Matthew L. Fidler)
;;    Took out the interactive requirement agin.  Causes bugs like
;;    org-delete-char below.
;; 01-Feb-2011    Matthew L. Fidler  
;;    Last-Updated: Tue Feb  1 22:43:11 2011 (-0600) #641 (Matthew L. Fidler)
;;    Bug fix for org-delete-char (and possibly others).  Allow
;;    delete-char to have auto-indent changed behavior when the
;;    command lookup is the same as the delete command (as well as if
;;    it is called interactively)
;; 01-Feb-2011    Matthew L. Fidler  
;;    Last-Updated: Tue Feb  1 20:41:49 2011 (-0600) #570 (Matthew L. Fidler)
;;    Added bugfix to kill-line advice and function (from Le Wang)
;; 01-Feb-2011    Matthew L. Fidler  
;;    Last-Updated: Tue Feb  1 15:45:08 2011 (-0600) #563 (Matthew L. Fidler)
;;    Added cua-paste and cua-paste-pop
;; 01-Feb-2011    Matthew L. Fidler  
;;    Last-Updated: Tue Feb  1 13:54:36 2011 (-0600) #556 (Matthew L. Fidler)
;;    Added auto-indent on move up and down with the arrow keys.
;; 01-Feb-2011    Matthew L. Fidler  
;;    Last-Updated: Tue Feb  1 13:20:49 2011 (-0600) #546 (Matthew L. Fidler)
;;    Added a keyboard engine that indents instead of using hooks and advices.
;; 01-Feb-2011    Matthew L. Fidler  
;;    Last-Updated: Tue Feb  1 09:40:50 2011 (-0600) #466 (Matthew L. Fidler)
;;    Removed the interactivity in the hooks.  They are definitely not interactive.
;; 01-Feb-2011    Matthew L. Fidler  
;;    Last-Updated: Tue Feb  1 09:28:11 2011 (-0600) #459 (Matthew L. Fidler)
;;    Added Le Wang's fixes:
;;
;;    * Many functions are checked for interactivity
;;    * Kill-line prefix argument is fixed
;;    * Kill region when region is active is controled by auto-indent-kill-line-kill-region-when-active
;;    * Kill-line when at eol has more options
;;    * Change auto-indent-indentation-function to auto-indent-newline-function
;; 31-Jan-2011    Matthew L. Fidler
;;    Last-Updated: Mon Jan 31 22:05:59 2011 (-0600) #440 (Matthew L. Fidler)
;;    Removed indirect reference to `shrink-whitespaces'.  Thanks Le Wang
;; 31-Jan-2011    Matthew L. Fidler
;;    Last-Updated: Mon Jan 31 21:27:39 2011 (-0600) #435 (Matthew L. Fidler)
;;    Added explicit requirement for functions
;; 18-Jan-2011    Matthew L. Fidler
;;    Last-Updated: Tue Jan 18 10:23:43 2011 (-0600) #428 (Matthew L. Fidler)
;;    Added support to turn on `org-indent-mode' when inside an org-file.
;; 12-Jan-2011    Matthew L. Fidler
;;    Last-Updated: Wed Jan 12 16:27:21 2011 (-0600) #420 (Matthew L. Fidler)
;;    Added fix for ortbl-minor-mode.  Now it will work when orgtbl-minor mode is enabled.
;; 09-Dec-2010    Matthew L. Fidler
;;    Last-Updated: Thu Dec  9 09:17:45 2010 (-0600) #414 (Matthew L. Fidler)
;;    Bugfix.  Now instead of indenting the region pasted, indent the region-pasted + beginning of line at region begin and end of line at region end.
;; 02-Dec-2010    Matthew L. Fidler
;;    Last-Updated: Thu Dec  2 13:02:02 2010 (-0600) #411 (Matthew L. Fidler)
;;    Made ignoring of modes with indent-relative and indent-relative-maybe apply to indenting returns as well.
;; 02-Dec-2010    Matthew L. Fidler
;;    Last-Updated: Thu Dec  2 11:38:37 2010 (-0600) #402 (Matthew L. Fidler)
;;    Removed auto-indent on paste/yank for modes with indent-relative and indent-relative-maybe.  This has annoyed me forever.
;; 02-Dec-2010    Matthew L. Fidler
;;    Last-Updated: Thu Dec  2 10:40:05 2010 (-0600) #397 (Matthew L. Fidler)
;;    Added an advice to delete-char.  When deleting a new-line character, shrink white-spaces afterward.
;; 02-Dec-2010    Matthew L. Fidler
;;    Last-Updated: Thu Dec  2 08:59:49 2010 (-0600) #386 (Matthew L. Fidler)
;;    Speed enhancement by checking for yasnippets only on indentation.
;; 29-Nov-2010    Matthew L. Fidler
;;    Last-Updated: Mon Nov 29 13:19:38 2010 (-0600) #377 (Matthew L. Fidler)
;;    Bug fix to allow authotkey files to save.
;; 29-Nov-2010    Matthew L. Fidler
;;    Last-Updated: Mon Nov 29 12:10:09 2010 (-0600) #367 (Matthew L. Fidler)
;;    Change auto-indent-on-save to be disabled by default.
;; 22-Nov-2010    Matthew L. Fidler
;;    Last-Updated: Mon Nov 22 14:36:10 2010 (-0600) #365 (Matthew L. Fidler)
;;    Yasnippet bug-fix.
;; 22-Nov-2010    Matthew L. Fidler
;;    Last-Updated: Mon Nov 22 12:00:07 2010 (-0600) #363 (Matthew L. Fidler)
;;    auto-indent bug fix for save on save buffer hooks.
;; 16-Nov-2010    Matthew L. Fidler
;;    Last-Updated: Tue Nov 16 13:16:05 2010 (-0600) #361 (Matthew L. Fidler)
;;    Added conf-windows-mode to ignored modes.
;; 15-Nov-2010    Matthew L. Fidler
;;    Last-Updated: Mon Nov 15 17:23:03 2010 (-0600) #354 (Matthew L. Fidler)
;;    Bugfix for deletion of whitespace
;; 15-Nov-2010    Matthew L. Fidler
;;    Last-Updated: Mon Nov 15 14:27:50 2010 (-0600) #351 (Matthew L. Fidler)
;;    Bugfix for post-command-hook.
;; 15-Nov-2010    Matthew L. Fidler
;;    Last-Updated: Mon Nov 15 08:53:03 2010 (-0600) #338 (Matthew L. Fidler)
;;    Added diff-mode to excluded modes for auto-indentaion.
;; 15-Nov-2010    Matthew L. Fidler
;;    Last-Updated: Mon Nov 15 00:22:30 2010 (-0600) #336 (Matthew L. Fidler)
;;    Added fundamental mode to excluded modes for auto-indentation.
;; 13-Nov-2010    Matthew L. Fidler
;;    Last-Updated: Sat Nov 13 20:03:10 2010 (-0600) #334 (Matthew L. Fidler)
;;    Bug fix try #3
;; 13-Nov-2010    Matthew L. Fidler
;;    Last-Updated: Sat Nov 13 19:55:29 2010 (-0600) #329 (Matthew L. Fidler)
;;    Anothe bug-fix for yasnippet.
;; 13-Nov-2010    Matthew L. Fidler
;;    Last-Updated: Sat Nov 13 19:49:47 2010 (-0600) #325 (Matthew L. Fidler)
;;
;;    Bug fix for auto-indent-mode.  Now it checks to make sure that
;;    `last-command-event' is non-nil.
;;
;; 11-Nov-2010    Matthew L. Fidler
;;    Last-Updated: Thu Nov 11 13:56:15 2010 (-0600) #308 (Matthew L. Fidler)
;;    Put back processes in.  Made the return key handled by pre and post-command-hooks.
;; 11-Nov-2010    Matthew L. Fidler
;;    Last-Updated: Thu Nov 11 11:28:42 2010 (-0600) #257 (Matthew L. Fidler)
;;    Took out processes such as *R* or *eshell*
;; 09-Nov-2010    Matthew L. Fidler
;;    Last-Updated: Tue Nov  9 22:03:34 2010 (-0600) #255 (Matthew L. Fidler)
;;
;;    Bug fix when interacting with the SVN version of yasnippet.  It
;;    will not perform the line indentation when Yasnippet is running.
;;
;; 09-Nov-2010    Matthew L. Fidler
;;    Last-Updated: Tue Nov  9 13:47:18 2010 (-0600) #253 (Matthew L. Fidler)
;;    Made sure that the auto-paste indentation doesn't work in minibuffer.
;; 09-Nov-2010    Matthew L. Fidler
;;    Last-Updated: Tue Nov  9 11:51:07 2010 (-0600) #246 (Matthew L. Fidler)
;;    When `auto-indent-pre-command-hook' is inactivated by some means, add it back.
;; 09-Nov-2010
;;    Last-Updated: Tue Nov  9 11:13:09 2010 (-0600) #238 (Matthew L. Fidler)
;;    Added snippet-mode to excluded modes.  Also turned off the kill-line by default.
;; 07-Nov-2010    Matthew L. Fidler
;;    Last-Updated: Sun Nov  7 18:24:05 2010 (-0600) #233 (Matthew L. Fidler)
;;    Added the possibility of TextMate type returns.
;; 07-Nov-2010    Matthew L. Fidler
;;    Last-Updated: Sun Nov  7 00:54:07 2010 (-0500) #180 (Matthew L. Fidler)
;;    Bug fix where backspace on indented region stopped working.Added TextMate
;; 07-Nov-2010    Matthew L. Fidler
;;    Last-Updated: Sun Nov  7 00:30:54 2010 (-0500) #167 (Matthew L. Fidler)
;;    Another small bug fix.
;; 07-Nov-2010    Matthew L. Fidler
;;    Last-Updated: Sun Nov  7 00:21:38 2010 (-0500) #154 (Matthew L. Fidler)
;;
;;    Added bugfix and also allow movement on blank lines to be
;;    automatically indented to the correct position.
;;
;; 06-Nov-2010    Matthew L. Fidler
;;    Last-Updated: Sat Nov  6 17:39:59 2010 (-0500) #113 (Matthew L. Fidler)
;;    Initial release.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(eval-when-compile
  (require 'cl))

(defgroup auto-indent nil
  "* Auto Indent Mode Customizations"
  :group 'editing)

(defcustom auto-indent-on-yank-or-paste 't
  "* Indent pasted or yanked region."
  :type 'boolean
  :group 'auto-indent)

(defcustom auto-indent-mode-untabify-on-yank-or-paste 't
  "* Untabify pasted or yanked region."
  :type 'boolean
  :group 'auto-indent)

(defcustom auto-indent-on-visit-file nil
  "* Auto Indent file upon visit."
  :type 'boolean
  :group 'auto-indent)

(defcustom auto-indent-on-save-file nil
  "* Auto Indent on visit file."
  :type 'boolean
  :group 'auto-indent)

(defcustom auto-indent-untabify-on-visit-file nil
  "* Automatically convert tabs into spaces when visiting a file."
  :type 'boolean
  :group 'auto-indent)

(defcustom auto-indent-delete-trailing-whitespace-on-visit-file nil
  "* Automatically remove trailing whitespace when visiting  file"
  :type 'boolean
  :group 'auto-indent)

(defcustom auto-indent-untabify-on-save-file t
  "* Change tabs to spaces on file-save."
  :type 'boolean
  :group 'auto-indent)

(defcustom auto-indent-delete-trailing-whitespace-on-save-file nil
  "* When saving file delete trailing whitespace. "
  :type 'boolean
  :group 'auto-indent)

(defcustom auto-indent-on-visit-pretend-nothing-changed t
  "* When modifying the file on visit, pretend nothing changed."
  :type 'boolean
  :group 'auto-indent)

(defcustom auto-indent-delete-line-char-remove-extra-spaces t
  "* When deleting a return, delete any extra spaces between the newly joined lines"
  :type 'boolean
  :group 'auto-indent)

(defcustom auto-indent-kill-line-remove-extra-spaces t
  "* When killing lines, remove extra spaces before killing the line."
  :type 'boolean
  :group 'auto-indent)

(defcustom auto-indent-kill-line-at-eol nil
  "* When killing lines, if at the end of a line,

nil - join next line to the current line. Deletes whitespace at
      join.
      See also `auto-indent-kill-line-remove-extra-spaces'
whole-lines - kill next lines
blanks - kill all empty lines after the current line, and then
         any lines specified.

You should also set `kill-whole-line' to do what you want.

"
  :type '(choice (const :tag "Default" nil)
                 (const :tag "Next whole line" whole-line)
                 (const :tag "Next whole line after any blank lines" blanks))
  :group 'auto-indent)

(defcustom auto-indent-kill-line-kill-region-when-active t
  "* When killing lines, if region is active, kill region instead."
  :type 'boolean
  :group 'auto-indent)

(defcustom auto-indent-use-text-boundaries nil
  "* When killing lines, if point is before any text, act as if
  point is at BOL.  And if point is after text, act as if point
  is at EOL"
  :type 'boolean
  :group 'auto-indent)

(defcustom auto-indent-minor-mode-symbol t
  "* When true, Auto Indent puts AI on the mode line."
  :type 'boolean
  :group 'auto-indent)

(defcustom auto-indent-disabled-modes-on-save '(ahk-mode)
  "* List of modes where indent-region of the whole file is ignored"
  :type '(repeat (sexp :tag "Major mode"))
  :tag " Major modes where linum is disabled: "
  :group 'auto-indent)

(defcustom auto-indent-disabled-modes-list '(eshell-mode wl-summary-mode compilation-mode org-mode text-mode dired-mode snippet-mode fundamental-mode diff-mode texinfo-mode conf-windows-mode)
  "* List of modes disabled when global auto-indent-mode is on."
  :type '(repeat (sexp :tag "Major mode"))
  :tag " Major modes where linum is disabled: "
  :group 'auto-indent)

(defcustom auto-indent-disabled-indent-functions '(indent-relative indent-relative-maybe)
  "* List of functions that auto-indent ignores the indent-region
  on paste and automated indent by pressing return.  The default is indent-relative and
  indent-relative-maybe.  If these are used the indentation is may not specified for the current mode."
  :type '(repeat (symbol :tag "Ignored indent-function"))
  :group 'auto-indent)

(defcustom auto-indent-newline-function 'reindent-then-newline-and-indent
  "* Auto indentation function for the return key."
  :type '(choice
          (const :tag "Reindent the current line, insert the newline then indent the current line."
                 reindent-then-newline-and-indent)
          (const :tag "Insert newline then indent current line" 'newline-and-indent))
  :tag "Indentation type for AutoComplete mode.  While `reindent-then-newline-and-indent' is a likely candidate `newline-and-indent' also works.  "
  :group 'auto-indent)

(defcustom auto-indent-blank-lines-on-move t
  "*Auto indentation on moving cursor to blank lines."
  :type 'boolean
  :group 'auto-indent)

(defcustom auto-indent-key-for-end-of-line-then-newline ""
  "* Key for the following action: end-of-line then newline. TextMate uses meta return,
I believe (M-RET).  If blank, no key is defined. The key should be in a
format used for saving keyboard macros (see `edmacro-mode'). This is useful when used
in conjunction with something that pairs delimiters like `autopair-mode'."
  :type 'string
  :group 'auto-indent)

(defcustom auto-indent-key-for-end-of-line-insert-char-then-newline ""
  "* Key for the following action: end-of-line, insert character
`auto-indent-eol-char' then newline.  By default the
`auto-indent-eol-char' is the semicolon. TextMate uses shift-meta
return, I believe (S-M-RET). If blank, no key is defined.  The
key should be in a format used for having keyboard macros (see
`edmacro-mode'). This is useful when used
in conjunction with something that pairs delimiters like `autopair-mode'.
"
  :type 'string
  :group 'auto-indent)

(defcustom auto-indent-eol-char ";"
  "* Character inserted when
`auto-indent-key-for-end-of-line-inser-char-then-newline' is
defined.  This is a buffer local variable, therefore if you have
a mode that instead of using a semi-colon for an end of
statement, you use a colon, this can be added to the mode as
follows:

  (add-hook 'strange-mode-hook (lambda() (setq auto-indent-eol-char \":\")))

This is similar to Textmate's behavior.  This is useful when used
in conjunction with something that pairs delimiters like `autopair-mode'.
"
  :type 'string
  :group 'auto-indent)

(defcustom auto-indent-start-org-indent t
  "Starts `org-indent-mode' when in org-mode"
  :type 'boolean
  :group 'auto-indent)

(defcustom auto-indent-force-interactive-advices nil
  "Forces interactive advices.  This makes sure that this is
called when this is an interactive call directly to the function.
However, if someone defines something such as `org-delete-char'
to delete a character, when `org-delete-char' is called
interactively and then calls `delete-char' the advice is never
activated (when it should be).  This specific example has a
work-around, but something like `org-kill-line' does not have a
specific work-around.  Therefore, to be more inclusive, advices
are not required to be interactive."
  :type 'boolean
  :group 'auto-indent)

(defcustom auto-indent-engine nil
  "Type of engine to use.  The possibilities are:

default: Use hooks and advices to implement auto-indent-mode

keymap: Use key remappings to implement auto-indent-mode.  This may
work in some modes but may cause things such as `company-mode' or
`auto-complete-mode' to function improperly"
  :type '(choice
	  (const :tag "default" nil)
	  (const :tag "Keymaps" keys))
  :group 'auto-indent)

(make-variable-buffer-local 'auto-indent-eol-char)

(defvar auto-indent-eol-ret-save ""
  "Saved variable for keyboard state")

(defvar auto-indent-eol-ret-semi-save ""
  "Saved variable for keyboard state")


(defvar auto-indent-minor-mode-map nil
  "* Auto Indent mode map.")
;; Keymap functions for auto-indent-mode.  Replace return with the appropriate command.

(defun auto-indent-setup-map ()
  "* Sets up minor mode map."
  (setq auto-indent-minor-mode-map (make-sparse-keymap))
  (unless (string-match "^[ \t]*$" auto-indent-key-for-end-of-line-then-newline)
    (define-key auto-indent-minor-mode-map (read-kbd-macro auto-indent-key-for-end-of-line-then-newline) 'auto-indent-eol-newline))
  (unless (string-match "^[ \t]*$" auto-indent-key-for-end-of-line-insert-char-then-newline)
    (define-key auto-indent-minor-mode-map (read-kbd-macro auto-indent-key-for-end-of-line-insert-char-then-newline) 'auto-indent-eol-char-newline))
  (setq  auto-indent-eol-ret-save auto-indent-key-for-end-of-line-then-newline)
  (setq auto-indent-eol-ret-semi-save auto-indent-key-for-end-of-line-insert-char-then-newline))

(auto-indent-setup-map)

(defun auto-indent-original-binding (key)
  (or (key-binding key)
      (key-binding (this-single-command-keys))))

;;;###autoload
(defun auto-indent-eol-newline ()
  "*Auto-indent function for end-of-line and then newline."
  (interactive)
  (end-of-line)
  (call-interactively (auto-indent-original-binding (kbd "RET"))))

;;;###autoload
(defun auto-indent-eol-char-newline ()
  "* Auto-indent function for end-of-line, insert `auto-indent-eol-char', and then newline"
  (interactive)
  (end-of-line)
  (unless (looking-back "; *")
    (insert (format "%s" auto-indent-eol-char)))
  (call-interactively (auto-indent-original-binding (kbd "RET"))))
(defalias 'auto-indent-mode 'auto-indent-minor-mode)
(define-minor-mode auto-indent-minor-mode
  "Auto Indent minor mode.

With no argument, this command toggles the mode.
positive prefix argument turns on the mode.
Negative prefix argument turns off the mode.

When auto-indent-minor-mode minor mode is enabled, yanking or pasting automatically indents

Fall back to default, non-indented yanking by preceding the yanking commands with C-u.

Based on auto-indentation posts, slightly redefined toallow it to be a minor mode

http://www.emacswiki.org/emacs/AutoIndentation

"
  ;; The initial value.
  nil
  ;; The indicator for the mode line.  Nothing.
  (if auto-indent-minor-mode-symbol
      " AI"
    "")
  :group 'auto-indent
  (auto-indent-setup-map)
  (cond (auto-indent-minor-mode
         ;; Setup
	 (cond
	  ((eq auto-indent-engine 'keys) ;; Auto-indent engine
	   (local-set-key [remap yank] 'auto-indent-yank)
	   (local-set-key [remap yank-pop] 'auto-indent-yank-pop)
	   (local-set-key [remap delete-char] 'auto-indent-delete-char)
	   (local-set-key (kbd "RET") auto-indent-newline-function)
	   (local-set-key [remap kill-line] 'auto-indent-kill-line))
	  (t ;; Default auto-indent-engine setup.
	   (when (or auto-indent-on-visit-file auto-indent-untabify-on-visit-file
                     auto-indent-delete-trailing-whitespace-on-visit-file)
             (make-local-variable 'find-file-hook)
             (add-hook 'find-file-hook 'auto-indent-file-when-visit nil 't))
           (when (or auto-indent-on-save-file
                     auto-indent-untabify-on-save-file
                     auto-indent-delete-trailing-whitespace-on-save-file)
             (add-hook 'write-contents-hooks 'auto-indent-file-when-save))
           (add-hook 'after-save-hook 'auto-indent-mode-post-command-hook nil 't)
           (add-hook 'post-command-hook 'auto-indent-mode-post-command-hook nil 't)
           (add-hook 'pre-command-hook 'auto-indent-mode-pre-command-hook nil 't)
	   (mapc
	    (lambda(ad)
	      (when (fboundp ad)
		(condition-case error
		    (progn
		      (ad-enable-advice ad 'after 'auto-indent-minor-mode-advice)
		      (ad-activate ad))
		  (error
		   (message "Error enabling after-advices for `auto-indent-mode': %s" (error-message-string error))))))
	    '(yank yank-pop cua-paste cua-paste-pop))
	   (mapc
	    (lambda(ad)
	      (ad-enable-advice ad 'around 'auto-indent-minor-mode-advice)
	      (ad-activate ad))
	    '(delete-char kill-line)))))
        (t
         ;; Kill
	 (cond
          ((eq auto-indent-engine 'keys) ;; Auto-indent engine
           )
          (t ;; Default auto-indent-engine setup.
	   (mapc
            (lambda(ad)
              (condition-case error
		  (progn
		    (ad-disable-advice ad 'after 'auto-indent-minor-mode-advice)
		    (ad-activate ad)
		    )
		(error
		 (message "[auto-indent-mode] Error disabling advices: %s" (error-message-string error)))))
            '(yank yank-pop cua-paste cua-paste-pop))
           (mapc
            (lambda(ad)
	      (when (fboundp ad)
		(ad-disable-advice ad 'around 'auto-indent-minor-mode-advice)
		(ad-activate ad)))
            '(delete-char kill-line))
	   (remove-hook 'write-contents-hook 'auto-indent-file-when-save)
	   (remove-hook 'find-file-hook 'auto-indent-file-when-visit 't)
	   (remove-hook 'after-save-hook 'auto-indent-mode-post-command-hook 't)
	   (remove-hook 'post-command-hook 'auto-indent-mode-post-command-hook 't)
	   (remove-hook 'pre-command-hook 'auto-indent-mode-pre-command-hook 't))))))

;;;###autoload
(defun auto-indent-minor-mode-on ()
  "* Turn on auto-indent minor mode."
  (unless (or (minibufferp)
              (memq major-mode auto-indent-disabled-modes-list))
    (auto-indent-minor-mode 1))
  (when (and
         auto-indent-start-org-indent
         (not (minibufferp))
         (fboundp 'org-indent-mode)
         (eq major-mode 'org-mode))
    (org-indent-mode 1)))

;;;###autoload
(define-globalized-minor-mode auto-indent-global-mode auto-indent-minor-mode auto-indent-minor-mode-on
  :group 'auto-indent
  :require 'auto-indent-mode)

(defun auto-indent-yank-engine ()
  "Engine for the auto-indent yank functions/advices"
  (when (not (minibufferp))
    (let ((mark-even-if-inactive transient-mark-mode))
      (unless (memq indent-line-function auto-indent-disabled-indent-functions)
	(if auto-indent-on-yank-or-paste
	    (save-excursion 
	      (indent-region (progn (goto-char (region-beginning)) (point-at-bol))
			     (progn (goto-char (region-end)) (point-at-eol)) nil)))
	(if auto-indent-mode-untabify-on-yank-or-paste
	    (untabify (region-beginning) (region-end)))))))

(defmacro auto-indent-advice-command (command)
  "Define advices and functions for yank and yank-pop."
  `(progn
     (defadvice ,command (after auto-indent-minor-mode-advice)
       (when (and (or (not auto-indent-force-interactive-advices)
		      (called-interactively-p 'any))
		  (not current-prefix-arg)
		  auto-indent-minor-mode)
	 (auto-indent-yank-engine)))
     (defun ,(intern (concat "auto-indent-" (symbol-name command))) (&optional prefix)
       ,(concat "Auto-indent-mode `" (symbol-name command) "' function replacement.  Instead of using advices you can change keybindings to this function.")
       (interactive ,(if (eq command 'yank) "*P" "*p"))
       (,command prefix)
       (when (not prefix)
	 (auto-indent-yank-engine)))))

(auto-indent-advice-command yank)
(auto-indent-advice-command yank-pop)

(defun auto-indent-whole-buffer (&optional save)
  "Auto-indent whole buffer and untabify it"
  (interactive)
  (unless (or (minibufferp)
              (memq major-mode auto-indent-disabled-modes-list)
              (and save (memq major-mode auto-indent-disabled-modes-on-save)))
    (when (or
           (and save auto-indent-delete-trailing-whitespace-on-save-file)
           (and (not save) auto-indent-delete-trailing-whitespace-on-visit-file)
           )
      (delete-trailing-whitespace))
    (when (or
           (and save auto-indent-on-save-file)
           (and (not save) auto-indent-on-visit-file))
      (indent-region (point-min) (point-max) nil))
    (when (or
           (and (not save) auto-indent-untabify-on-visit-file)
           (and save auto-indent-untabify-on-save-file))
      (untabify (point-min) (point-max)))))

(defun auto-indent-file-when-save ()
  "* Auto-indent file when save."
  (if (not (minibufferp))
      (if (and auto-indent-minor-mode (buffer-file-name) auto-indent-on-save-file)
          (auto-indent-whole-buffer 't))))

(defun auto-indent-file-when-visit ()
  "* auto-indent file when visit."
  (when (buffer-file-name)
    (auto-indent-whole-buffer)
    (when auto-indent-on-visit-pretend-nothing-changed
      (set-buffer-modified-p nil) ; Make the buffer appear "not modified"
      )))

(defmacro auto-indent-def-del-char (&optional function)
  "Defines advices and commands for `delete-char'"
  `(,(if function 'defun 'defadvice)
    ,(if function 'auto-indent-delete-char 'delete-char)
    ,(if function '(n &optional killflag) '(around auto-indent-minor-mode-advice))
    "If at the end of the line, take out whitespace after deleting character"
    ,(if function '(interactive "p") nil)
    (if ,(if function t '(or (not auto-indent-force-interactive-advices)
			     (called-interactively-p 'any)
			     (eq (key-binding (kbd "<delete>")) this-command)))
	(let ((del-eol (eolp)))
	  ,(if function
	       '(if (called-interactively-p 'any)
		    (delete-char n (if n t nil))
		  (delete-char n killflag))
	     'ad-do-it)
	  (when (and del-eol
		     auto-indent-minor-mode (not (minibufferp))
		     auto-indent-delete-line-char-remove-extra-spaces)
	    (save-excursion
	      (skip-chars-backward " \t")
	      (when (looking-at "[ \t]+")
		(replace-match " ")))
	    (when (and (eolp) (looking-back "[ \t]+" nil t))
	      (replace-match ""))))
      ,(if function
	   '(if (called-interactively-p 'any)
		(delete-char n (if n t nil))
	      (delete-char n killflag))
	 'ad-do-it))))
  
(auto-indent-def-del-char)
(auto-indent-def-del-char t)

(defmacro auto-indent-def-kill-line (&optional function)
  "Defines advices and functions for `kill-line'"
  `(,(if function 'defun 'defadvice)
    ,(if function 'auto-indent-kill-line 'kill-line)
    ,(if function '(&optional arg) '(around auto-indent-minor-mode-advice))
    "Obey `auto-indent-use-text-boundaries'.

If at end of line, obey `auto-indent-kill-line-at-eol'
"
    ,(if function '(interactive "P") nil)
    (if ,(if function t '(or (not auto-indent-force-interactive-advices)
			     (called-interactively-p 'any)))
	(progn
	  (if (and auto-indent-minor-mode
		   (not (minibufferp)))
	      (if (and auto-indent-kill-line-kill-region-when-active
		       (use-region-p))
		  (progn
		    (kill-region (region-beginning) (region-end)))
		(if (auto-indent-bolp)
		    (progn
		      (move-beginning-of-line 1)
		      ,(if function
			   '(kill-line arg)
			 'ad-do-it))
		  (if (auto-indent-eolp)
		      (cond ((eq auto-indent-kill-line-at-eol nil)
			     (if (> (prefix-numeric-value current-prefix-arg) 0)
				 (delete-indentation 't)
			       ,(if function
				    '(kill-line arg)
				  'ad-do-it)))
			    ((memq auto-indent-kill-line-at-eol '(whole-line blanks))
			     (if (> (prefix-numeric-value current-prefix-arg) 0)
				 (save-excursion
				   (delete-region (point) (point-at-eol))
				   (unless (eobp)
				     (forward-line 1)
				     (when (eq auto-indent-kill-line-at-eol 'blanks)
				       (kill-region (point) (+ (point)
							       (skip-chars-forward " \t\n")
							       (skip-chars-backward " \t"))))
				     ,(if function
					  '(kill-line arg)
					'ad-do-it)))
			       ,(if function
				    '(kill-line arg)
				  'ad-do-it)))
			    (t
			     (error "invalid `auto-indent-kill-line-at-eol' setting %s"				   
                                    auto-indent-kill-line-at-eol)))
                    ,(if function
                         '(kill-line arg)
                       'ad-do-it)))))
	  (indent-according-to-mode))
      ,(if function
	   nil
	 'ad-do-it))))

(auto-indent-def-kill-line t)
(auto-indent-def-kill-line)

(defvar auto-indent-mode-pre-command-hook-line nil)
(make-variable-buffer-local 'auto-indent-mode-pre-command-hook-line)
(defvar auto-indent-last-pre-command-hook-point nil)
(make-variable-buffer-local 'auto-indent-mode-pre-command-hook-point)
(defvar auto-indent-last-pre-command-hook-minibufferp nil)

(defun auto-indent-eolp ()
  "returns t if point is at eol respecting `auto-indent-use-text-boundaries'"
  (if auto-indent-use-text-boundaries
      (looking-at-p "[ \t]*$")
    (eolp)))

(defun auto-indent-bolp ()
  "returns t if point is at bol respecting `auto-indent-use-text-boundaries'"
  (if auto-indent-use-text-boundaries
      (looking-back "^[ \t]*")
    (bolp)))

(defun auto-indent-mode-pre-command-hook()
  "Hook for auto-indent-mode to tell if the point has been moved"
  (condition-case error
      (progn
	(setq auto-indent-last-pre-command-hook-minibufferp (minibufferp))
	(when (and (not (minibufferp)))
	  (setq auto-indent-mode-pre-command-hook-line (line-number-at-pos))
	  (setq auto-indent-last-pre-command-hook-point (point))))
    (error
     (message "[Auto-Indent Mode] Ignoring Error in `auto-indent-mode-pre-command-hook': %s" (error-message-string error)))))

(defun auto-indent-mode-post-command-hook ()
  "Hook for auto-indent-mode to go to the right place when moving around and the whitespace was deleted from the line."
  (condition-case err
      (when (and (not auto-indent-last-pre-command-hook-minibufferp) (not (minibufferp))
		 (not (memq indent-line-function auto-indent-disabled-indent-functions)))
	(unless (memq 'auto-indent-mode-pre-command-hook pre-command-hook)
	  (setq auto-indent-mode-pre-command-hook-line -1)
	  (add-hook 'pre-command-hook 'auto-indent-mode-pre-command-hook))
	(when auto-indent-minor-mode
	  (cond
	   ((and last-command-event (memq last-command-event '(10 13 return)))
	    (when (or (not (fboundp 'yas/snippets-at-point))
		      (and yas/minor-mode
			   (let ((yap (yas/snippets-at-point 'all-snippets)))
			     (or (not yap) (and yap (= 0 (length yap)))))))
	      (save-excursion
		(when (and auto-indent-last-pre-command-hook-point
			   (eq auto-indent-newline-function 'reindent-then-newline-and-indent))
		  (goto-char auto-indent-last-pre-command-hook-point)
		  (indent-according-to-mode))
		(when auto-indent-last-pre-command-hook-point
		  (goto-char auto-indent-last-pre-command-hook-point)
		  ;; Remove the trailing white-space after indentation because
		  ;; indentation may introduce the whitespace.
		  (save-restriction
		    (narrow-to-region (point-at-bol) (point-at-eol))
		    (delete-trailing-whitespace))))
	      (indent-according-to-mode)))
	   ((and auto-indent-blank-lines-on-move
		 auto-indent-mode-pre-command-hook-line
		 (not (= (line-number-at-pos)
			 auto-indent-mode-pre-command-hook-line)))
	    (when (and (looking-back "^[ \t]*") (looking-at "[ \t]*$"))
	      (indent-according-to-mode))))))
    (error (message "[Auto-Indent-Mode]: Ignored indentation error in `auto-indent-post-command-hook' %s" (error-message-string err)))))
(provide 'auto-indent-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; auto-indent-mode.el ends here
