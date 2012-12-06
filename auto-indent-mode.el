;;; auto-indent-mode.el --- Auto indent Minor mode
;;
;; Filename: auto-indent-mode.el
;; Description: Auto Indent text on Yank/Paste
;; Author: Matthew L. Fidler, Le Wang & Others
;; Maintainer: Matthew L. Fidler
;; Created: Sat Nov  6 11:02:07 2010 (-0500)
;; Version: 0.88
;; Last-Updated: Tue Aug 21 13:08:42 2012 (-0500)
;;           By: Matthew L. Fidler
;;     Update #: 1467
;; URL: https://github.com/mlf176f2/auto-indent-mode.el/
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
;; * About auto-indent-mode
;; Provides auto-indentation minor mode for Emacs.  This allows the
;; following: 
;; 
;;   - Return automatically indents the code appropriately (if enabled)
;; 
;;   - Pasting/Yanking indents the appropriately
;; 
;;   - Killing line will take off unneeded spaces (if enabled)
;; 
;;   - On visit file, indent appropriately, but DONT SAVE. (Pretend like
;;     nothing happened, if enabled)
;; 
;;   - On save, optionally unttabify, remove trailing white-spaces, and
;;     definitely indent the file (if enabled).
;; 
;;   - TextMate behavior of keys if desired (see below)
;; 
;;   - Deleting the end of a line will shrink the whitespace to just one
;;     (if desired and enabled)
;; 
;;   - Automatically indent balanced parenthetical expression, or sexp, if desired
;;     `auto-indent-current-pairs' or `auto-indent-next-pair' is set
;;     to be true (disabled by default).  This is not immediate but occurs
;;     after a bit to allow better responsiveness in emacs.
;; 
;;   - Attempts to set the indentation level (number of spaces for an
;;     indent) for a major-mode.
;; 
;; All of these options can be customized. (customize auto-indent)
;; * Installing auto-indent-mode
;; 
;; To use put this in your load path and then put the following in your emacs
;; file:
;; 
;;   (setq auto-indent-on-visit-file t) ;; If you want auto-indent on for files
;;   (require 'auto-indent-mode)
;; 
;; 
;; If you (almost) always want this on, add the following to ~/.emacs:
;; 
;; 
;;    (auto-indent-global-mode)
;; 
;; 
;; 
;; Excluded modes are defined in `auto-indent-disabled-modes-list'
;; 
;; If you only want this on for a single mode, you would add the following to
;; ~/.emacs
;; 
;; 
;;   (add-hook 'emacs-lisp-mode-hook 'auto-indent-minor-mode)
;; 
;; 
;; 
;; You could always turn on the minor mode with the command
;; `auto-indent-minor-mode'
;; * Setting the number of spaces for indenting major modes
;; While this is controlled by the major mode, as a convenience,
;; auto-indent-mode attempts to set the default number of spaces for an
;; indentation for specific major mode.  
;; 
;; This is done by:
;; 1. Making local variables of all the variables specified in
;;    `auto-indent-known-indent-level-variables' and setting them to
;;    auto-indent's `auto-indent-assign-indent-level'
;; 2. Looking to see if major mode variables
;;    `major-mode-indent-level' and `major-mode-basic-offset' variables
;;    are present.  If either of these variables are present,
;;    `auto-indent-mode' sets these variables to the default
;;    `auto-indent-assign-indent-level'.   
;; 
;; * TextMate Meta-Return behavior
;; If you would like TextMate behavior of Meta-RETURN going to the
;; end of the line and then inserting a newline, as well as
;; Meta-shift return going to the end of the line, inserting a
;; semi-colon then inserting a newline, use the following:
;; 
;; 
;;   (setq auto-indent-key-for-end-of-line-then-newline "<M-return>")
;;   (setq auto-indent-key-for-end-of-line-insert-char-then-newline "<M-S-return>")
;;   (require 'auto-indent-mode)
;;   (auto-indent-global-mode)
;; 
;; 
;; This may or may not work on your system.  Many times emacs cannot
;; distinguish between M-RET and M-S-RET, so if you don't mind a
;; slight redefinition use:
;; 
;; 
;;   (setq auto-indent-key-for-end-of-line-then-newline "<M-return>")
;;   (setq auto-indent-key-for-end-of-line-insert-char-then-newline "<C-M-return>")
;;   (require 'auto-indent-mode)
;;   (auto-indent-global-mode)
;; 
;; 
;; If you want to insert something other than a semi-colon (like a
;; colon) in a specific mode, say colon-mode, do the following:
;; 
;; 
;;   (add-hook 'colon-mode-hook (lambda () (setq auto-indent-eol-char ":")))
;; 
;; * Notes about autopair-mode and yasnippet compatibility
;; If you wish to use this with autopairs and yasnippet, please load
;; this library first.
;; * Using specific functions from auto-indent-mode
;; 
;; Also if you wish to just use specific functions from this library
;; that is possible as well.
;; 
;; To have the auto-indentation-paste use:
;; 
;; 
;;   (autoload 'auto-indent-yank "auto-indent-mode" "" t)
;;   (autoload 'auto-indent-yank-pop "auto-indent-mode" "" t)
;;   
;;   (define-key global-map [remap yank] 'auto-indent-yank)
;;   (define-key global-map [remap yank-pop] 'auto-indent-yank-pop)
;;   
;;   (autoload 'auto-indent-delete-char "auto-indent-mode" "" t)
;;   (define-key global-map [remap delete-char] 'auto-indent-delete-char)
;;   
;;   (autoload 'auto-indent-kill-line "auto-indent-mode" "" t)
;;   (define-key global-map [remap kill-line] 'auto-indent-kill-line)
;;   
;; 
;; 
;; 
;; However, this does not honor the excluded modes in
;; `auto-indent-disabled-modes-list'
;; 
;; 
;; * Making certain modes perform tasks on paste/yank.
;; Sometimes, like in R, it is convenient to paste c:\ and change it to
;; c:/.  This can be accomplished by modifying the
;; `auto-indent-after-yank-hook'.
;; 
;; The code for changing the paths is as follows:
;; 
;; 
;;   (defun kicker-ess-fix-path (beg end)
;;       "Fixes ess path"
;;       (save-restriction
;;         (save-excursion
;;           (narrow-to-region beg end)
;;           (goto-char (point-min))
;;           (when (looking-at "[A-Z]:\\\\")
;;             (while (search-forward "\\" nil t)
;;               (replace-match "/"))))))
;;     
;;     (defun kicker-ess-turn-on-fix-path ()
;;       (interactive)
;;       (when (string= "S" ess-language)
;;         (add-hook 'auto-indent-after-yank-hook 'kicker-ess-fix-path t t)))
;;     (add-hook 'ess-mode-hook 'kicker-ess-turn-on-fix-path)
;; 
;; 
;; Another R-hack is to take of the ">" and "+" of a command line
;; copy. For example copying:
;; 
;;  > 
;;  > availDists <- c(Normal="rnorm", Exponential="rexp")
;;  > availKernels <- c("gaussian", "epanechnikov", "rectangular",
;;  + "triangular", "biweight", "cosine", "optcosine")
;; 
;; 
;; Should give the following code on paste:
;; 
;;  
;;  availDists <- c(Normal="rnorm", Exponential="rexp")
;;  availKernels <- c("gaussian", "epanechnikov", "rectangular",
;;  "triangular", "biweight", "cosine", "optcosine")
;; 
;; 
;; This is setup by the following code snippet:
;; 
;; 
;;   (defun kicker-ess-fix-code (beg end)
;;     "Fixes ess path"
;;     (save-restriction
;;       (save-excursion
;;         (save-match-data
;;           (narrow-to-region beg end)
;;           (goto-char (point-min))
;;           (while (re-search-forward "^[ \t]*[>][ \t]+" nil t)
;;             (replace-match "")
;;             (goto-char (point-at-eol))
;;             (while (looking-at "[ \t\n]*[+][ \t]+")
;;               (replace-match "\n")
;;               (goto-char (point-at-eol))))))))
;;   
;;   (defun kicker-ess-turn-on-fix-code ()
;;     (interactive)
;;     (when (string= "S" ess-language)
;;       (add-hook 'auto-indent-after-yank-hook 'kicker-ess-fix-code t t)))
;;   (add-hook 'ess-mode-hook 'kicker-ess-turn-on-fix-code)
;;   
;; 
;; 
;; * Auto-indent and org-mode
;; Auto-indent does not technically turn on for org-mode.  Instead the
;; following can be added/changed:
;; 
;; 1. `org-indent-mode' is turned on when `auto-indent-start-org-indent' 
;;    is true.
;; 2. The return behavior is changed to newline and indent in code blocks
;;    when `auto-indent-fix-org-return' is true.
;; 3. The backspace behavior is changed to auto-indent's backspace when
;;    `auto-indent-delete-backward-char' is true.  This only works in
;;    code blocks. 
;; 4. The home beginning of line behavior is changed to auto-indent's
;;    when `auto-indent-fix-org-move-beginning-of-line' is true.
;; 5. The yank/paste behavior is changed to auto-indent in a code block
;;    when `auto-indent-fix-org-yank' is true.
;; 6. The auto-filling activity in source-code blocks can break your code
;;    depending on the language.  When `auto-indent-fix-org-auto-fill' is
;;    true, auto-filling is turned off in`org-mode' source blocks.
;; * FAQ
;; ** Why isn't my mode indenting?
;; Some modes are excluded for compatability reasons, such as
;; text-modes.  This is controlled by the variable
;; `auto-indent-disabled-modes-list'
;; ** Why isn't my specific mode have the right number of spaces?
;; Actually, the number of spaces for indentation is controlled by the
;; major mode. If there is a major-mode specific variable that controls
;; this offset, you can add this variable to
;; `auto-indent-known-indent-level-variables' to change the indentation
;; for this mode when auto-indent-mode starts.
;; 
;; See:
;; 
;; - [[http://www.pement.org/emacs_tabs.htm][Understanding GNU Emacs and tabs]]
;; - [[http://kb.iu.edu/data/abde.html][In Emacs how can I change tab sizes?]]
;; *Auto indentation on moving cursor to blank lines.
;; 
;; *** auto-indent-current-pairs
;;  - Automatically indent the current parenthetical statement.
;; 
;; *** auto-indent-delete-line-char-add-extra-spaces
;;  - When deleting a return, add a space (when appropriate)
;; between the newly joined lines.
;; 
;; This takes care of the condition when deleting text
;; 
;; Lorem ipsum dolor sit|
;; amet, consectetur adipiscing elit.  Morbi id
;; 
;; Lorem ipsum dolor sit|amet, consectetur adipiscing elit.  Morbi id
;; 
;; Which ideally should be deleted to:
;; 
;; Lorem ipsum dolor sit| amet, consectetur adipiscing elit.  Morbi id
;; 
;; This is controlled by the regular expressions in
;; `auto-indent-delete-line-char-add-extra-spaces-prog-mode-regs'
;; and
;; `auto-indent-delete-line-char-add-extra-spaces-text-mode-regs'
;; 
;; *** auto-indent-delete-line-char-add-extra-spaces-prog-mode-regs
;;  - Regular expressions for use with `auto-indent-delete-line-char-add-extra-spaces'.  This is used for programming modes as determined by `auto-indent-is-prog-mode-p'.
;; 
;; *** auto-indent-delete-line-char-add-extra-spaces-text-mode-regs
;;  - Regular expressions for use with `auto-indent-delete-line-char-add-extra-spaces'.  This is used for programming modes as determined by `auto-indent-is-prog-mode-p'.
;; 
;; *** auto-indent-delete-line-char-remove-extra-spaces
;; When deleting a return, delete any extra spaces between the newly joined lines.
;; 
;; *** auto-indent-delete-line-char-remove-last-space
;; Remove last space when deleting a line.
;; 
;; When `auto-indent-delete-line-char-remove-extra-spaces' is enabled,
;; expressions like lists can be removed in a less than optimal
;; manner.  For example, assuming =`|=' is the cursor:
;; 
;; c("Vehicle QD TO",|
;;      "1 ug IVT","3 ug IVT",...
;; 
;; would be deleted to the following
;; 
;; c("Vehicle QD TO",| "1 ug IVT","3 ug IVT",...
;; 
;; In this case it would be preferable to delete to:
;; 
;; c("Vehicle QD TO",|"1 ug IVT","3 ug IVT",...
;; 
;; However cases like sentences:
;; 
;; Lorem ipsum dolor sit amet,|
;;      consectetur adipiscing elit. Morbi id
;; 
;; Deletes to
;; Lorem ipsum dolor sit amet,| consectetur adipiscing elit. Morbi id
;; 
;; Which is a desired behavior.
;; 
;; When this is enabled, auto-indent attempts to be smarter by
;; deleting the extra space when characters before and after match
;; expressions defined in
;; `auto-indent-delete-line-char-remove-last-space-prog-mode-regs' and
;; `auto-indent-delete-line-char-remove-last-space-text-mode-regs'.
;; 
;; *** auto-indent-delete-line-char-remove-last-space-prog-mode-regs
;;  - Regular expressions for use with `auto-indent-delete-line-char-remove-last-space'.  This is used for programming modes as determined by `auto-indent-is-prog-mode-p'.
;; 
;; *** auto-indent-delete-line-char-remove-last-space-text-mode-regs
;; Regular expressions for use with `auto-indent-delete-line-char-remove-last-space'.  This is used for modes other than programming modes.  This is determined by `auto-indent-is-prog-mode-p'.
;; 
;; *** auto-indent-delete-trailing-whitespace-on-save-file
;;  - When saving file delete trailing whitespace.
;; 
;; *** auto-indent-delete-trailing-whitespace-on-visit-file
;;  - Automatically remove trailing whitespace when visiting  file.
;; 
;; *** auto-indent-disabled-indent-functions
;; List of disabled indent functions.
;; 
;; List of functions that auto-indent ignores the `indent-region' on
;; paste and automated indent by pressing return.  The default is
;; `indent-relative' and `indent-relative-maybe'.  If these are used the
;; indentation is may not specified for the current mode.
;; 
;; *** auto-indent-disabled-modes-list
;; List of modes disabled when global `auto-indent-mode' is on.
;; 
;; *** auto-indent-disabled-modes-on-save
;;  - List of modes where `indent-region' of the whole file is ignored.
;; 
;; *** auto-indent-engine
;; Type of engine to use.  The possibilities are:
;; 
;; default: Use hooks and advices to implement auto-indent-mode
;; 
;; keymap: Use key remappings to implement auto-indent-mode.  This may
;; work in some modes but may cause things such as `company-mode' or
;; `auto-complete-mode' to function improperly
;; 
;; *** auto-indent-eol-char
;; End of line/statement character, like C or matlab's semi-colon.
;; 
;; Character inserted when
;; `auto-indent-key-for-end-of-line-inser-char-then-newline' is
;; defined.  This is a buffer local variable, therefore if you have
;; a mode that instead of using a semi-colon for an end of
;; statement, you use a colon, this can be added to the mode as
;; follows:
;; 
;;      (add-hook 'strange-mode-hook (lambda() (setq auto-indent-eol-char ":")))
;; 
;; autoThis is similar to Textmate's behavior.  This is useful when used
;; in conjunction with something that pairs delimiters like `autopair-mode'.
;; 
;; *** auto-indent-fix-org-auto-fill
;; Fixes org-based
;;   auto-fill-function (i.e. `org-auto-fill-function') to only
;;   auto-fill for things outside of a source block.
;; 
;; *** auto-indent-fix-org-backspace
;; Fixes `org-backspace' to use `auto-indent-backward-delete-char-behavior' for `org-mode' buffers.
;; 
;; *** auto-indent-fix-org-move-beginning-of-line
;; Fixes `move-beginning-of-line' in `org-mode' when in source blocks to follow `auto-indent-mode'.
;; 
;; *** auto-indent-fix-org-return
;; Allows newline and indent behavior in source code blocks in org-mode.
;; 
;; *** auto-indent-fix-org-yank
;; Allows org-mode yanks to be indented in source code blocks of org-mode.
;; 
;; *** auto-indent-force-interactive-advices
;; Forces interactive advices.
;; 
;; This makes sure that this is called when this is an interactive
;; call directly to the function.  However, if someone defines
;; something such as `org-delete-char' to delete a character, when
;; `org-delete-char' is called interactively and then calls
;; `delete-char' the advice is never activated (when it should be).
;; If this is activated, `auto-indent-mode' tries to do the right
;; thing by guessing what key should have been pressed to get this
;; event.  If it is the key that was pressed enable the advice.
;; 
;; *** auto-indent-home-is-beginning-of-indent
;; The Home key, or rather the `move-beginning-of-line' function, will move to the beginning of the indentation when called interactively.  If it is already at the beginning of the indent, move to the beginning of the line.
;; 
;; *** auto-indent-home-is-beginning-of-indent-when-spaces-follow
;; This is a customization for the home key.
;; 
;; If `auto-indent-home-is-beginning-of-indent' is enabled, the Home
;; key, or rather the `move-beginning-of-line' function, will move
;; to the beginning of the indentation when called interactively.
;; 
;; If it is already at the beginning of the indent,and move to the
;; beginning of the line.  When
;; `auto-indent-home-is-beginning-of-indent-when-spaces-follow' is
;; enabled, a home key press from
;; 
;;     (defadvice move-beginning-of-line (around auto-indent-minor-mode-advice)
;;     | (let (at-beginning)
;; 
;; will change to
;; 
;;     (defadvice move-beginning-of-line (around auto-indent-minor-mode-advice)
;;       |(let (at-beginning)
;; 
;; Another home-key will chang to cursor
;; 
;;     (defadvice move-beginning-of-line (around auto-indent-minor-mode-advice)
;; |   (let (at-beginning)
;; 
;; *** auto-indent-key-for-end-of-line-insert-char-then-newline
;; Key for end of line, `auto-indent-eol-char', then newline.
;; 
;; By default the `auto-indent-eol-char' is the semicolon. TextMate
;; uses shift-meta return, I believe (S-M-RET). If blank, no key is
;; defined.  The key should be in a format used for having keyboard
;; macros (see `edmacro-mode'). This is useful when used in
;; conjunction with something that pairs delimiters like
;; `autopair-mode'.
;; 
;; *** auto-indent-key-for-end-of-line-then-newline
;; Key for end of line, then newline.
;; 
;; TextMate uses meta return, I believe (M-RET).  If blank, no key
;; is defined. The key should be in a format used for saving
;; keyboard macros (see `edmacro-mode'). This is useful when used in
;; conjunction with something that pairs delimiters like `autopair-mode'.
;; 
;; *** auto-indent-kill-line-at-eol
;; Determines how a kill at the end of line behaves.
;; 
;; When killing lines, if at the end of a line,
;; 
;; nil - join next line to the current line.  Deletes white-space at
;;          join.  [this essentially duplicated delete-char]
;; 
;;          See also `auto-indent-kill-remove-extra-spaces'
;; 
;; whole-line - kill next lines
;; 
;; subsequent-whole-lines - merge lines on first call, subsequent kill whole lines
;; 
;; blanks - kill all empty lines after the current line, and then
;;             any lines specified.
;; 
;; You should also set the function `kill-whole-line' to do what you
;; want.
;; 
;; *** auto-indent-kill-line-kill-region-when-active
;;  - When killing lines, if region is active, kill region instead.
;; 
;; *** auto-indent-kill-remove-extra-spaces
;;  - Remove indentation before killing the line or region.
;; 
;; *** auto-indent-known-indent-level-variables
;; Known indent-level-variables for major modes.  Set locally when auto-indent-mode initializes.
;; 
;; *** auto-indent-known-text-modes
;;  - List of auto-indent's known text-modes.
;; 
;; *** auto-indent-minor-mode-symbol
;;  - When true, Auto Indent puts AI on the mode line.
;; 
;; *** auto-indent-mode-untabify-on-yank-or-paste
;;  - Untabify pasted or yanked region.
;; 
;; *** auto-indent-newline-function
;;  - Auto indentation function for the return key.
;; 
;; *** auto-indent-next-pair
;; Automatically indent the next parenthetical statement.  For example in R:
;; 
;; d| <- read.csv("dat.csv",
;;                   na.strings=c(".","NA"))
;; 
;; When typing .old, the indentation will be updated as follows:
;; 
;; d.old <- read.csv("dat.csv",
;;                      na.strings=c(".","NA"))
;; 
;; This will slow down your computation, so if you use it make sure
;; that the `auto-indent-next-pair-timer-interval' is appropriate
;; for your needs.
;; 
;; It is useful when using this option to have some sort of autopairing on.
;; 
;; *** auto-indent-next-pair-timer-geo-mean
;; Number of seconds before the observed parenthetical statement is indented.
;; The faster the value, the slower Emacs responsiveness but the
;; faster Emacs indents the region.  The slower the value, the
;; faster Emacs responds.  This should be changed dynamically by
;; to the geometric mean of rate to indent a single line.
;; 
;; *** auto-indent-on-save-file
;;  - Auto Indent on visit file.
;; 
;; *** auto-indent-on-visit-file
;;  - Auto Indent file upon visit.
;; 
;; *** auto-indent-on-visit-pretend-nothing-changed
;;  - When modifying the file on visit, pretend nothing changed.
;; 
;; *** auto-indent-on-yank-or-paste
;;  - Indent pasted or yanked region.
;; 
;; *** auto-indent-start-org-indent
;; Starts `org-indent-mode' when in org-mode.
;; 
;; *** auto-indent-untabify-on-save-file
;;  - Change tabs to spaces on file-save.
;; 
;; *** auto-indent-untabify-on-visit-file
;;  - Automatically convert tabs into spaces when visiting a file.
;; 
;; *** auto-indent-use-text-boundaries
;; Use text boundaries when killing lines.
;; 
;; When killing lines, if point is before any text, act as if
;; point is at BOL.  And if point is after text, act as if point
;;      is at EOL
;; 
;; ** Internal Variables
;; 
;; *** auto-indent-eol-ret-save
;; Saved variable for keyboard state.
;; 
;; *** auto-indent-eol-ret-semi-save
;; Saved variable for keyboard state.
;; 
;; *** auto-indent-minor-mode-map
;;  - Auto Indent mode map.
;; 
;; *** auto-indent-pairs-begin
;; Defines where the pair region begins.
;; 
;; *** auto-indent-pairs-end
;; Defines where the pair region ends.
;; *Auto indentation on moving cursor to blank lines.
;; 
;; *** auto-indent-current-pairs
;;  - Automatically indent the current parenthetical statement.
;; 
;; *** auto-indent-delete-line-char-add-extra-spaces
;;  - When deleting a return, add a space (when appropriate)
;; between the newly joined lines.
;; 
;; This takes care of the condition when deleting text
;; 
;; Lorem ipsum dolor sit|
;; amet, consectetur adipiscing elit.  Morbi id
;; 
;; Lorem ipsum dolor sit|amet, consectetur adipiscing elit.  Morbi id
;; 
;; Which ideally should be deleted to:
;; 
;; Lorem ipsum dolor sit| amet, consectetur adipiscing elit.  Morbi id
;; 
;; This is controlled by the regular expressions in
;; `auto-indent-delete-line-char-add-extra-spaces-prog-mode-regs'
;; and
;; `auto-indent-delete-line-char-add-extra-spaces-text-mode-regs'
;; 
;; *** auto-indent-delete-line-char-add-extra-spaces-prog-mode-regs
;;  - Regular expressions for use with `auto-indent-delete-line-char-add-extra-spaces'.  This is used for programming modes as determined by `auto-indent-is-prog-mode-p'.
;; 
;; *** auto-indent-delete-line-char-add-extra-spaces-text-mode-regs
;;  - Regular expressions for use with `auto-indent-delete-line-char-add-extra-spaces'.  This is used for programming modes as determined by `auto-indent-is-prog-mode-p'.
;; 
;; *** auto-indent-delete-line-char-remove-extra-spaces
;; When deleting a return, delete any extra spaces between the newly joined lines.
;; 
;; *** auto-indent-delete-line-char-remove-last-space
;; Remove last space when deleting a line.
;; 
;; When `auto-indent-delete-line-char-remove-extra-spaces' is enabled,
;; expressions like lists can be removed in a less than optimal
;; manner.  For example, assuming =`|=' is the cursor:
;; 
;; c("Vehicle QD TO",|
;;      "1 ug IVT","3 ug IVT",...
;; 
;; would be deleted to the following
;; 
;; c("Vehicle QD TO",| "1 ug IVT","3 ug IVT",...
;; 
;; In this case it would be preferable to delete to:
;; 
;; c("Vehicle QD TO",|"1 ug IVT","3 ug IVT",...
;; 
;; However cases like sentences:
;; 
;; Lorem ipsum dolor sit amet,|
;;      consectetur adipiscing elit. Morbi id
;; 
;; Deletes to
;; Lorem ipsum dolor sit amet,| consectetur adipiscing elit. Morbi id
;; 
;; Which is a desired behavior.
;; 
;; When this is enabled, auto-indent attempts to be smarter by
;; deleting the extra space when characters before and after match
;; expressions defined in
;; `auto-indent-delete-line-char-remove-last-space-prog-mode-regs' and
;; `auto-indent-delete-line-char-remove-last-space-text-mode-regs'.
;; 
;; *** auto-indent-delete-line-char-remove-last-space-prog-mode-regs
;;  - Regular expressions for use with `auto-indent-delete-line-char-remove-last-space'.  This is used for programming modes as determined by `auto-indent-is-prog-mode-p'.
;; 
;; *** auto-indent-delete-line-char-remove-last-space-text-mode-regs
;; Regular expressions for use with `auto-indent-delete-line-char-remove-last-space'.  This is used for modes other than programming modes.  This is determined by `auto-indent-is-prog-mode-p'.
;; 
;; *** auto-indent-delete-trailing-whitespace-on-save-file
;;  - When saving file delete trailing whitespace.
;; 
;; *** auto-indent-delete-trailing-whitespace-on-visit-file
;;  - Automatically remove trailing whitespace when visiting  file.
;; 
;; *** auto-indent-disabled-indent-functions
;; List of disabled indent functions.
;; 
;; List of functions that auto-indent ignores the `indent-region' on
;; paste and automated indent by pressing return.  The default is
;; `indent-relative' and `indent-relative-maybe'.  If these are used the
;; indentation is may not specified for the current mode.
;; 
;; *** auto-indent-disabled-modes-list
;; List of modes disabled when global `auto-indent-mode' is on.
;; 
;; *** auto-indent-disabled-modes-on-save
;;  - List of modes where `indent-region' of the whole file is ignored.
;; 
;; *** auto-indent-engine
;; Type of engine to use.  The possibilities are:
;; 
;; default: Use hooks and advices to implement auto-indent-mode
;; 
;; keymap: Use key remappings to implement auto-indent-mode.  This may
;; work in some modes but may cause things such as `company-mode' or
;; `auto-complete-mode' to function improperly
;; 
;; *** auto-indent-eol-char
;; End of line/statement character, like C or matlab's semi-colon.
;; 
;; Character inserted when
;; `auto-indent-key-for-end-of-line-inser-char-then-newline' is
;; defined.  This is a buffer local variable, therefore if you have
;; a mode that instead of using a semi-colon for an end of
;; statement, you use a colon, this can be added to the mode as
;; follows:
;; 
;;      (add-hook 'strange-mode-hook (lambda() (setq auto-indent-eol-char ":")))
;; 
;; autoThis is similar to Textmate's behavior.  This is useful when used
;; in conjunction with something that pairs delimiters like `autopair-mode'.
;; 
;; *** auto-indent-fix-org-auto-fill
;; Fixes org-based
;;   auto-fill-function (i.e. `org-auto-fill-function') to only
;;   auto-fill for things outside of a source block.
;; 
;; *** auto-indent-fix-org-backspace
;; Fixes `org-backspace' to use `auto-indent-backward-delete-char-behavior' for `org-mode' buffers.
;; 
;; *** auto-indent-fix-org-move-beginning-of-line
;; Fixes `move-beginning-of-line' in `org-mode' when in source blocks to follow `auto-indent-mode'.
;; 
;; *** auto-indent-fix-org-return
;; Allows newline and indent behavior in source code blocks in org-mode.
;; 
;; *** auto-indent-fix-org-yank
;; Allows org-mode yanks to be indented in source code blocks of org-mode.
;; 
;; *** auto-indent-force-interactive-advices
;; Forces interactive advices.
;; 
;; This makes sure that this is called when this is an interactive
;; call directly to the function.  However, if someone defines
;; something such as `org-delete-char' to delete a character, when
;; `org-delete-char' is called interactively and then calls
;; `delete-char' the advice is never activated (when it should be).
;; If this is activated, `auto-indent-mode' tries to do the right
;; thing by guessing what key should have been pressed to get this
;; event.  If it is the key that was pressed enable the advice.
;; 
;; *** auto-indent-home-is-beginning-of-indent
;; The Home key, or rather the `move-beginning-of-line' function, will move to the beginning of the indentation when called interactively.  If it is already at the beginning of the indent, move to the beginning of the line.
;; 
;; *** auto-indent-home-is-beginning-of-indent-when-spaces-follow
;; This is a customization for the home key.
;; 
;; If `auto-indent-home-is-beginning-of-indent' is enabled, the Home
;; key, or rather the `move-beginning-of-line' function, will move
;; to the beginning of the indentation when called interactively.
;; 
;; If it is already at the beginning of the indent,and move to the
;; beginning of the line.  When
;; `auto-indent-home-is-beginning-of-indent-when-spaces-follow' is
;; enabled, a home key press from
;; 
;;     (defadvice move-beginning-of-line (around auto-indent-minor-mode-advice)
;;     | (let (at-beginning)
;; 
;; will change to
;; 
;;     (defadvice move-beginning-of-line (around auto-indent-minor-mode-advice)
;;       |(let (at-beginning)
;; 
;; Another home-key will chang to cursor
;; 
;;     (defadvice move-beginning-of-line (around auto-indent-minor-mode-advice)
;; |   (let (at-beginning)
;; 
;; *** auto-indent-key-for-end-of-line-insert-char-then-newline
;; Key for end of line, `auto-indent-eol-char', then newline.
;; 
;; By default the `auto-indent-eol-char' is the semicolon. TextMate
;; uses shift-meta return, I believe (S-M-RET). If blank, no key is
;; defined.  The key should be in a format used for having keyboard
;; macros (see `edmacro-mode'). This is useful when used in
;; conjunction with something that pairs delimiters like
;; `autopair-mode'.
;; 
;; *** auto-indent-key-for-end-of-line-then-newline
;; Key for end of line, then newline.
;; 
;; TextMate uses meta return, I believe (M-RET).  If blank, no key
;; is defined. The key should be in a format used for saving
;; keyboard macros (see `edmacro-mode'). This is useful when used in
;; conjunction with something that pairs delimiters like `autopair-mode'.
;; 
;; *** auto-indent-kill-line-at-eol
;; Determines how a kill at the end of line behaves.
;; 
;; When killing lines, if at the end of a line,
;; 
;; nil - join next line to the current line.  Deletes white-space at
;;          join.  [this essentially duplicated delete-char]
;; 
;;          See also `auto-indent-kill-remove-extra-spaces'
;; 
;; whole-line - kill next lines
;; 
;; subsequent-whole-lines - merge lines on first call, subsequent kill whole lines
;; 
;; blanks - kill all empty lines after the current line, and then
;;             any lines specified.
;; 
;; You should also set the function `kill-whole-line' to do what you
;; want.
;; 
;; *** auto-indent-kill-line-kill-region-when-active
;;  - When killing lines, if region is active, kill region instead.
;; 
;; *** auto-indent-kill-remove-extra-spaces
;;  - Remove indentation before killing the line or region.
;; 
;; *** auto-indent-known-indent-level-variables
;; Known indent-level-variables for major modes.  Set locally when auto-indent-mode initializes.
;; 
;; *** auto-indent-known-text-modes
;;  - List of auto-indent's known text-modes.
;; 
;; *** auto-indent-minor-mode-symbol
;;  - When true, Auto Indent puts AI on the mode line.
;; 
;; *** auto-indent-mode-untabify-on-yank-or-paste
;;  - Untabify pasted or yanked region.
;; 
;; *** auto-indent-newline-function
;;  - Auto indentation function for the return key.
;; 
;; *** auto-indent-next-pair
;; Automatically indent the next parenthetical statement.  For example in R:
;; 
;; d| <- read.csv("dat.csv",
;;                   na.strings=c(".","NA"))
;; 
;; When typing .old, the indentation will be updated as follows:
;; 
;; d.old <- read.csv("dat.csv",
;;                      na.strings=c(".","NA"))
;; 
;; This will slow down your computation, so if you use it make sure
;; that the `auto-indent-next-pair-timer-interval' is appropriate
;; for your needs.
;; 
;; It is useful when using this option to have some sort of autopairing on.
;; 
;; *** auto-indent-next-pair-timer-geo-mean
;; Number of seconds before the observed parenthetical statement is indented.
;; The faster the value, the slower Emacs responsiveness but the
;; faster Emacs indents the region.  The slower the value, the
;; faster Emacs responds.  This should be changed dynamically by
;; to the geometric mean of rate to indent a single line.
;; 
;; *** auto-indent-on-save-file
;;  - Auto Indent on visit file.
;; 
;; *** auto-indent-on-visit-file
;;  - Auto Indent file upon visit.
;; 
;; *** auto-indent-on-visit-pretend-nothing-changed
;;  - When modifying the file on visit, pretend nothing changed.
;; 
;; *** auto-indent-on-yank-or-paste
;;  - Indent pasted or yanked region.
;; 
;; *** auto-indent-start-org-indent
;; Starts `org-indent-mode' when in org-mode.
;; 
;; *** auto-indent-untabify-on-save-file
;;  - Change tabs to spaces on file-save.
;; 
;; *** auto-indent-untabify-on-visit-file
;;  - Automatically convert tabs into spaces when visiting a file.
;; 
;; *** auto-indent-use-text-boundaries
;; Use text boundaries when killing lines.
;; 
;; When killing lines, if point is before any text, act as if
;; point is at BOL.  And if point is after text, act as if point
;;      is at EOL
;; 
;; ** Internal Variables
;; 
;; *** auto-indent-eol-ret-save
;; Saved variable for keyboard state.
;; 
;; *** auto-indent-eol-ret-semi-save
;; Saved variable for keyboard state.
;; 
;; *** auto-indent-minor-mode-map
;;  - Auto Indent mode map.
;; 
;; *** auto-indent-pairs-begin
;; Defines where the pair region begins.
;; 
;; *** auto-indent-pairs-end
;; Defines where the pair region ends.
;; *Auto indentation on moving cursor to blank lines.
;; 
;; *** auto-indent-current-pairs
;;  - Automatically indent the current parenthetical statement.
;; 
;; *** auto-indent-delete-line-char-add-extra-spaces
;;  - When deleting a return, add a space (when appropriate)
;; between the newly joined lines.
;; 
;; This takes care of the condition when deleting text
;; 
;; Lorem ipsum dolor sit|
;; amet, consectetur adipiscing elit.  Morbi id
;; 
;; Lorem ipsum dolor sit|amet, consectetur adipiscing elit.  Morbi id
;; 
;; Which ideally should be deleted to:
;; 
;; Lorem ipsum dolor sit| amet, consectetur adipiscing elit.  Morbi id
;; 
;; This is controlled by the regular expressions in
;; `auto-indent-delete-line-char-add-extra-spaces-prog-mode-regs'
;; and
;; `auto-indent-delete-line-char-add-extra-spaces-text-mode-regs'
;; 
;; *** auto-indent-delete-line-char-add-extra-spaces-prog-mode-regs
;;  - Regular expressions for use with `auto-indent-delete-line-char-add-extra-spaces'.  This is used for programming modes as determined by `auto-indent-is-prog-mode-p'.
;; 
;; *** auto-indent-delete-line-char-add-extra-spaces-text-mode-regs
;;  - Regular expressions for use with `auto-indent-delete-line-char-add-extra-spaces'.  This is used for programming modes as determined by `auto-indent-is-prog-mode-p'.
;; 
;; *** auto-indent-delete-line-char-remove-extra-spaces
;; When deleting a return, delete any extra spaces between the newly joined lines.
;; 
;; *** auto-indent-delete-line-char-remove-last-space
;; Remove last space when deleting a line.
;; 
;; When `auto-indent-delete-line-char-remove-extra-spaces' is enabled,
;; expressions like lists can be removed in a less than optimal
;; manner.  For example, assuming =`|=' is the cursor:
;; 
;; c("Vehicle QD TO",|
;;      "1 ug IVT","3 ug IVT",...
;; 
;; would be deleted to the following
;; 
;; c("Vehicle QD TO",| "1 ug IVT","3 ug IVT",...
;; 
;; In this case it would be preferable to delete to:
;; 
;; c("Vehicle QD TO",|"1 ug IVT","3 ug IVT",...
;; 
;; However cases like sentences:
;; 
;; Lorem ipsum dolor sit amet,|
;;      consectetur adipiscing elit. Morbi id
;; 
;; Deletes to
;; Lorem ipsum dolor sit amet,| consectetur adipiscing elit. Morbi id
;; 
;; Which is a desired behavior.
;; 
;; When this is enabled, auto-indent attempts to be smarter by
;; deleting the extra space when characters before and after match
;; expressions defined in
;; `auto-indent-delete-line-char-remove-last-space-prog-mode-regs' and
;; `auto-indent-delete-line-char-remove-last-space-text-mode-regs'.
;; 
;; *** auto-indent-delete-line-char-remove-last-space-prog-mode-regs
;;  - Regular expressions for use with `auto-indent-delete-line-char-remove-last-space'.  This is used for programming modes as determined by `auto-indent-is-prog-mode-p'.
;; 
;; *** auto-indent-delete-line-char-remove-last-space-text-mode-regs
;; Regular expressions for use with `auto-indent-delete-line-char-remove-last-space'.  This is used for modes other than programming modes.  This is determined by `auto-indent-is-prog-mode-p'.
;; 
;; *** auto-indent-delete-trailing-whitespace-on-save-file
;;  - When saving file delete trailing whitespace.
;; 
;; *** auto-indent-delete-trailing-whitespace-on-visit-file
;;  - Automatically remove trailing whitespace when visiting  file.
;; 
;; *** auto-indent-disabled-indent-functions
;; List of disabled indent functions.
;; 
;; List of functions that auto-indent ignores the `indent-region' on
;; paste and automated indent by pressing return.  The default is
;; `indent-relative' and `indent-relative-maybe'.  If these are used the
;; indentation is may not specified for the current mode.
;; 
;; *** auto-indent-disabled-modes-list
;; List of modes disabled when global `auto-indent-mode' is on.
;; 
;; *** auto-indent-disabled-modes-on-save
;;  - List of modes where `indent-region' of the whole file is ignored.
;; 
;; *** auto-indent-engine
;; Type of engine to use.  The possibilities are:
;; 
;; default: Use hooks and advices to implement auto-indent-mode
;; 
;; keymap: Use key remappings to implement auto-indent-mode.  This may
;; work in some modes but may cause things such as `company-mode' or
;; `auto-complete-mode' to function improperly
;; 
;; *** auto-indent-eol-char
;; End of line/statement character, like C or matlab's semi-colon.
;; 
;; Character inserted when
;; `auto-indent-key-for-end-of-line-inser-char-then-newline' is
;; defined.  This is a buffer local variable, therefore if you have
;; a mode that instead of using a semi-colon for an end of
;; statement, you use a colon, this can be added to the mode as
;; follows:
;; 
;;      (add-hook 'strange-mode-hook (lambda() (setq auto-indent-eol-char ":")))
;; 
;; autoThis is similar to Textmate's behavior.  This is useful when used
;; in conjunction with something that pairs delimiters like `autopair-mode'.
;; 
;; *** auto-indent-fix-org-auto-fill
;; Fixes org-based
;;   auto-fill-function (i.e. `org-auto-fill-function') to only
;;   auto-fill for things outside of a source block.
;; 
;; *** auto-indent-fix-org-backspace
;; Fixes `org-backspace' to use `auto-indent-backward-delete-char-behavior' for `org-mode' buffers.
;; 
;; *** auto-indent-fix-org-move-beginning-of-line
;; Fixes `move-beginning-of-line' in `org-mode' when in source blocks to follow `auto-indent-mode'.
;; 
;; *** auto-indent-fix-org-return
;; Allows newline and indent behavior in source code blocks in org-mode.
;; 
;; *** auto-indent-fix-org-yank
;; Allows org-mode yanks to be indented in source code blocks of org-mode.
;; 
;; *** auto-indent-force-interactive-advices
;; Forces interactive advices.
;; 
;; This makes sure that this is called when this is an interactive
;; call directly to the function.  However, if someone defines
;; something such as `org-delete-char' to delete a character, when
;; `org-delete-char' is called interactively and then calls
;; `delete-char' the advice is never activated (when it should be).
;; If this is activated, `auto-indent-mode' tries to do the right
;; thing by guessing what key should have been pressed to get this
;; event.  If it is the key that was pressed enable the advice.
;; 
;; *** auto-indent-home-is-beginning-of-indent
;; The Home key, or rather the `move-beginning-of-line' function, will move to the beginning of the indentation when called interactively.  If it is already at the beginning of the indent, move to the beginning of the line.
;; 
;; *** auto-indent-home-is-beginning-of-indent-when-spaces-follow
;; This is a customization for the home key.
;; 
;; If `auto-indent-home-is-beginning-of-indent' is enabled, the Home
;; key, or rather the `move-beginning-of-line' function, will move
;; to the beginning of the indentation when called interactively.
;; 
;; If it is already at the beginning of the indent,and move to the
;; beginning of the line.  When
;; `auto-indent-home-is-beginning-of-indent-when-spaces-follow' is
;; enabled, a home key press from
;; 
;;     (defadvice move-beginning-of-line (around auto-indent-minor-mode-advice)
;;     | (let (at-beginning)
;; 
;; will change to
;; 
;;     (defadvice move-beginning-of-line (around auto-indent-minor-mode-advice)
;;       |(let (at-beginning)
;; 
;; Another home-key will chang to cursor
;; 
;;     (defadvice move-beginning-of-line (around auto-indent-minor-mode-advice)
;; |   (let (at-beginning)
;; 
;; *** auto-indent-key-for-end-of-line-insert-char-then-newline
;; Key for end of line, `auto-indent-eol-char', then newline.
;; 
;; By default the `auto-indent-eol-char' is the semicolon. TextMate
;; uses shift-meta return, I believe (S-M-RET). If blank, no key is
;; defined.  The key should be in a format used for having keyboard
;; macros (see `edmacro-mode'). This is useful when used in
;; conjunction with something that pairs delimiters like
;; `autopair-mode'.
;; 
;; *** auto-indent-key-for-end-of-line-then-newline
;; Key for end of line, then newline.
;; 
;; TextMate uses meta return, I believe (M-RET).  If blank, no key
;; is defined. The key should be in a format used for saving
;; keyboard macros (see `edmacro-mode'). This is useful when used in
;; conjunction with something that pairs delimiters like `autopair-mode'.
;; 
;; *** auto-indent-kill-line-at-eol
;; Determines how a kill at the end of line behaves.
;; 
;; When killing lines, if at the end of a line,
;; 
;; nil - join next line to the current line.  Deletes white-space at
;;          join.  [this essentially duplicated delete-char]
;; 
;;          See also `auto-indent-kill-remove-extra-spaces'
;; 
;; whole-line - kill next lines
;; 
;; subsequent-whole-lines - merge lines on first call, subsequent kill whole lines
;; 
;; blanks - kill all empty lines after the current line, and then
;;             any lines specified.
;; 
;; You should also set the function `kill-whole-line' to do what you
;; want.
;; 
;; *** auto-indent-kill-line-kill-region-when-active
;;  - When killing lines, if region is active, kill region instead.
;; 
;; *** auto-indent-kill-remove-extra-spaces
;;  - Remove indentation before killing the line or region.
;; 
;; *** auto-indent-known-indent-level-variables
;; Known indent-level-variables for major modes.  Set locally when auto-indent-mode initializes.
;; 
;; *** auto-indent-known-text-modes
;;  - List of auto-indent's known text-modes.
;; 
;; *** auto-indent-minor-mode-symbol
;;  - When true, Auto Indent puts AI on the mode line.
;; 
;; *** auto-indent-mode-untabify-on-yank-or-paste
;;  - Untabify pasted or yanked region.
;; 
;; *** auto-indent-newline-function
;;  - Auto indentation function for the return key.
;; 
;; *** auto-indent-next-pair
;; Automatically indent the next parenthetical statement.  For example in R:
;; 
;; d| <- read.csv("dat.csv",
;;                   na.strings=c(".","NA"))
;; 
;; When typing .old, the indentation will be updated as follows:
;; 
;; d.old <- read.csv("dat.csv",
;;                      na.strings=c(".","NA"))
;; 
;; This will slow down your computation, so if you use it make sure
;; that the `auto-indent-next-pair-timer-interval' is appropriate
;; for your needs.
;; 
;; It is useful when using this option to have some sort of autopairing on.
;; 
;; *** auto-indent-next-pair-timer-geo-mean
;; Number of seconds before the observed parenthetical statement is indented.
;; The faster the value, the slower Emacs responsiveness but the
;; faster Emacs indents the region.  The slower the value, the
;; faster Emacs responds.  This should be changed dynamically by
;; to the geometric mean of rate to indent a single line.
;; 
;; *** auto-indent-on-save-file
;;  - Auto Indent on visit file.
;; 
;; *** auto-indent-on-visit-file
;;  - Auto Indent file upon visit.
;; 
;; *** auto-indent-on-visit-pretend-nothing-changed
;;  - When modifying the file on visit, pretend nothing changed.
;; 
;; *** auto-indent-on-yank-or-paste
;;  - Indent pasted or yanked region.
;; 
;; *** auto-indent-start-org-indent
;; Starts `org-indent-mode' when in org-mode.
;; 
;; *** auto-indent-untabify-on-save-file
;;  - Change tabs to spaces on file-save.
;; 
;; *** auto-indent-untabify-on-visit-file
;;  - Automatically convert tabs into spaces when visiting a file.
;; 
;; *** auto-indent-use-text-boundaries
;; Use text boundaries when killing lines.
;; 
;; When killing lines, if point is before any text, act as if
;; point is at BOL.  And if point is after text, act as if point
;;      is at EOL
;; 
;; ** Internal Variables
;; 
;; *** auto-indent-eol-ret-save
;; Saved variable for keyboard state.
;; 
;; *** auto-indent-eol-ret-semi-save
;; Saved variable for keyboard state.
;; 
;; *** auto-indent-minor-mode-map
;;  - Auto Indent mode map.
;; 
;; *** auto-indent-pairs-begin
;; Defines where the pair region begins.
;; 
;; *** auto-indent-pairs-end
;; Defines where the pair region ends.
;; *Auto indentation on moving cursor to blank lines.
;; 
;; *** auto-indent-current-pairs
;;  - Automatically indent the current parenthetical statement.
;; 
;; *** auto-indent-delete-line-char-add-extra-spaces
;;  - When deleting a return, add a space (when appropriate)
;; between the newly joined lines.
;; 
;; This takes care of the condition when deleting text
;; 
;; Lorem ipsum dolor sit|
;; amet, consectetur adipiscing elit.  Morbi id
;; 
;; Lorem ipsum dolor sit|amet, consectetur adipiscing elit.  Morbi id
;; 
;; Which ideally should be deleted to:
;; 
;; Lorem ipsum dolor sit| amet, consectetur adipiscing elit.  Morbi id
;; 
;; This is controlled by the regular expressions in
;; `auto-indent-delete-line-char-add-extra-spaces-prog-mode-regs'
;; and
;; `auto-indent-delete-line-char-add-extra-spaces-text-mode-regs'
;; 
;; *** auto-indent-delete-line-char-add-extra-spaces-prog-mode-regs
;;  - Regular expressions for use with `auto-indent-delete-line-char-add-extra-spaces'.  This is used for programming modes as determined by `auto-indent-is-prog-mode-p'.
;; 
;; *** auto-indent-delete-line-char-add-extra-spaces-text-mode-regs
;;  - Regular expressions for use with `auto-indent-delete-line-char-add-extra-spaces'.  This is used for programming modes as determined by `auto-indent-is-prog-mode-p'.
;; 
;; *** auto-indent-delete-line-char-remove-extra-spaces
;; When deleting a return, delete any extra spaces between the newly joined lines.
;; 
;; *** auto-indent-delete-line-char-remove-last-space
;; Remove last space when deleting a line.
;; 
;; When `auto-indent-delete-line-char-remove-extra-spaces' is enabled,
;; expressions like lists can be removed in a less than optimal
;; manner.  For example, assuming =`|=' is the cursor:
;; 
;; c("Vehicle QD TO",|
;;      "1 ug IVT","3 ug IVT",...
;; 
;; would be deleted to the following
;; 
;; c("Vehicle QD TO",| "1 ug IVT","3 ug IVT",...
;; 
;; In this case it would be preferable to delete to:
;; 
;; c("Vehicle QD TO",|"1 ug IVT","3 ug IVT",...
;; 
;; However cases like sentences:
;; 
;; Lorem ipsum dolor sit amet,|
;;      consectetur adipiscing elit. Morbi id
;; 
;; Deletes to
;; Lorem ipsum dolor sit amet,| consectetur adipiscing elit. Morbi id
;; 
;; Which is a desired behavior.
;; 
;; When this is enabled, auto-indent attempts to be smarter by
;; deleting the extra space when characters before and after match
;; expressions defined in
;; `auto-indent-delete-line-char-remove-last-space-prog-mode-regs' and
;; `auto-indent-delete-line-char-remove-last-space-text-mode-regs'.
;; 
;; *** auto-indent-delete-line-char-remove-last-space-prog-mode-regs
;;  - Regular expressions for use with `auto-indent-delete-line-char-remove-last-space'.  This is used for programming modes as determined by `auto-indent-is-prog-mode-p'.
;; 
;; *** auto-indent-delete-line-char-remove-last-space-text-mode-regs
;; Regular expressions for use with `auto-indent-delete-line-char-remove-last-space'.  This is used for modes other than programming modes.  This is determined by `auto-indent-is-prog-mode-p'.
;; 
;; *** auto-indent-delete-trailing-whitespace-on-save-file
;;  - When saving file delete trailing whitespace.
;; 
;; *** auto-indent-delete-trailing-whitespace-on-visit-file
;;  - Automatically remove trailing whitespace when visiting  file.
;; 
;; *** auto-indent-disabled-indent-functions
;; List of disabled indent functions.
;; 
;; List of functions that auto-indent ignores the `indent-region' on
;; paste and automated indent by pressing return.  The default is
;; `indent-relative' and `indent-relative-maybe'.  If these are used the
;; indentation is may not specified for the current mode.
;; 
;; *** auto-indent-disabled-modes-list
;; List of modes disabled when global `auto-indent-mode' is on.
;; 
;; *** auto-indent-disabled-modes-on-save
;;  - List of modes where `indent-region' of the whole file is ignored.
;; 
;; *** auto-indent-engine
;; Type of engine to use.  The possibilities are:
;; 
;; default: Use hooks and advices to implement auto-indent-mode
;; 
;; keymap: Use key remappings to implement auto-indent-mode.  This may
;; work in some modes but may cause things such as `company-mode' or
;; `auto-complete-mode' to function improperly
;; 
;; *** auto-indent-eol-char
;; End of line/statement character, like C or matlab's semi-colon.
;; 
;; Character inserted when
;; `auto-indent-key-for-end-of-line-inser-char-then-newline' is
;; defined.  This is a buffer local variable, therefore if you have
;; a mode that instead of using a semi-colon for an end of
;; statement, you use a colon, this can be added to the mode as
;; follows:
;; 
;;      (add-hook 'strange-mode-hook (lambda() (setq auto-indent-eol-char ":")))
;; 
;; autoThis is similar to Textmate's behavior.  This is useful when used
;; in conjunction with something that pairs delimiters like `autopair-mode'.
;; 
;; *** auto-indent-fix-org-auto-fill
;; Fixes org-based
;;   auto-fill-function (i.e. `org-auto-fill-function') to only
;;   auto-fill for things outside of a source block.
;; 
;; *** auto-indent-fix-org-backspace
;; Fixes `org-backspace' to use `auto-indent-backward-delete-char-behavior' for `org-mode' buffers.
;; 
;; *** auto-indent-fix-org-move-beginning-of-line
;; Fixes `move-beginning-of-line' in `org-mode' when in source blocks to follow `auto-indent-mode'.
;; 
;; *** auto-indent-fix-org-return
;; Allows newline and indent behavior in source code blocks in org-mode.
;; 
;; *** auto-indent-fix-org-yank
;; Allows org-mode yanks to be indented in source code blocks of org-mode.
;; 
;; *** auto-indent-force-interactive-advices
;; Forces interactive advices.
;; 
;; This makes sure that this is called when this is an interactive
;; call directly to the function.  However, if someone defines
;; something such as `org-delete-char' to delete a character, when
;; `org-delete-char' is called interactively and then calls
;; `delete-char' the advice is never activated (when it should be).
;; If this is activated, `auto-indent-mode' tries to do the right
;; thing by guessing what key should have been pressed to get this
;; event.  If it is the key that was pressed enable the advice.
;; 
;; *** auto-indent-home-is-beginning-of-indent
;; The Home key, or rather the `move-beginning-of-line' function, will move to the beginning of the indentation when called interactively.  If it is already at the beginning of the indent, move to the beginning of the line.
;; 
;; *** auto-indent-home-is-beginning-of-indent-when-spaces-follow
;; This is a customization for the home key.
;; 
;; If `auto-indent-home-is-beginning-of-indent' is enabled, the Home
;; key, or rather the `move-beginning-of-line' function, will move
;; to the beginning of the indentation when called interactively.
;; 
;; If it is already at the beginning of the indent,and move to the
;; beginning of the line.  When
;; `auto-indent-home-is-beginning-of-indent-when-spaces-follow' is
;; enabled, a home key press from
;; 
;;     (defadvice move-beginning-of-line (around auto-indent-minor-mode-advice)
;;     | (let (at-beginning)
;; 
;; will change to
;; 
;;     (defadvice move-beginning-of-line (around auto-indent-minor-mode-advice)
;;       |(let (at-beginning)
;; 
;; Another home-key will chang to cursor
;; 
;;     (defadvice move-beginning-of-line (around auto-indent-minor-mode-advice)
;; |   (let (at-beginning)
;; 
;; *** auto-indent-key-for-end-of-line-insert-char-then-newline
;; Key for end of line, `auto-indent-eol-char', then newline.
;; 
;; By default the `auto-indent-eol-char' is the semicolon. TextMate
;; uses shift-meta return, I believe (S-M-RET). If blank, no key is
;; defined.  The key should be in a format used for having keyboard
;; macros (see `edmacro-mode'). This is useful when used in
;; conjunction with something that pairs delimiters like
;; `autopair-mode'.
;; 
;; *** auto-indent-key-for-end-of-line-then-newline
;; Key for end of line, then newline.
;; 
;; TextMate uses meta return, I believe (M-RET).  If blank, no key
;; is defined. The key should be in a format used for saving
;; keyboard macros (see `edmacro-mode'). This is useful when used in
;; conjunction with something that pairs delimiters like `autopair-mode'.
;; 
;; *** auto-indent-kill-line-at-eol
;; Determines how a kill at the end of line behaves.
;; 
;; When killing lines, if at the end of a line,
;; 
;; nil - join next line to the current line.  Deletes white-space at
;;          join.  [this essentially duplicated delete-char]
;; 
;;          See also `auto-indent-kill-remove-extra-spaces'
;; 
;; whole-line - kill next lines
;; 
;; subsequent-whole-lines - merge lines on first call, subsequent kill whole lines
;; 
;; blanks - kill all empty lines after the current line, and then
;;             any lines specified.
;; 
;; You should also set the function `kill-whole-line' to do what you
;; want.
;; 
;; *** auto-indent-kill-line-kill-region-when-active
;;  - When killing lines, if region is active, kill region instead.
;; 
;; *** auto-indent-kill-remove-extra-spaces
;;  - Remove indentation before killing the line or region.
;; 
;; *** auto-indent-known-indent-level-variables
;; Known indent-level-variables for major modes.  Set locally when auto-indent-mode initializes.
;; 
;; *** auto-indent-known-text-modes
;;  - List of auto-indent's known text-modes.
;; 
;; *** auto-indent-minor-mode-symbol
;;  - When true, Auto Indent puts AI on the mode line.
;; 
;; *** auto-indent-mode-untabify-on-yank-or-paste
;;  - Untabify pasted or yanked region.
;; 
;; *** auto-indent-newline-function
;;  - Auto indentation function for the return key.
;; 
;; *** auto-indent-next-pair
;; Automatically indent the next parenthetical statement.  For example in R:
;; 
;; d| <- read.csv("dat.csv",
;;                   na.strings=c(".","NA"))
;; 
;; When typing .old, the indentation will be updated as follows:
;; 
;; d.old <- read.csv("dat.csv",
;;                      na.strings=c(".","NA"))
;; 
;; This will slow down your computation, so if you use it make sure
;; that the `auto-indent-next-pair-timer-interval' is appropriate
;; for your needs.
;; 
;; It is useful when using this option to have some sort of autopairing on.
;; 
;; *** auto-indent-next-pair-timer-geo-mean
;; Number of seconds before the observed parenthetical statement is indented.
;; The faster the value, the slower Emacs responsiveness but the
;; faster Emacs indents the region.  The slower the value, the
;; faster Emacs responds.  This should be changed dynamically by
;; to the geometric mean of rate to indent a single line.
;; 
;; *** auto-indent-on-save-file
;;  - Auto Indent on visit file.
;; 
;; *** auto-indent-on-visit-file
;;  - Auto Indent file upon visit.
;; 
;; *** auto-indent-on-visit-pretend-nothing-changed
;;  - When modifying the file on visit, pretend nothing changed.
;; 
;; *** auto-indent-on-yank-or-paste
;;  - Indent pasted or yanked region.
;; 
;; *** auto-indent-start-org-indent
;; Starts `org-indent-mode' when in org-mode.
;; 
;; *** auto-indent-untabify-on-save-file
;;  - Change tabs to spaces on file-save.
;; 
;; *** auto-indent-untabify-on-visit-file
;;  - Automatically convert tabs into spaces when visiting a file.
;; 
;; *** auto-indent-use-text-boundaries
;; Use text boundaries when killing lines.
;; 
;; When killing lines, if point is before any text, act as if
;; point is at BOL.  And if point is after text, act as if point
;;      is at EOL
;; 
;; ** Internal Variables
;; 
;; *** auto-indent-eol-ret-save
;; Saved variable for keyboard state.
;; 
;; *** auto-indent-eol-ret-semi-save
;; Saved variable for keyboard state.
;; 
;; *** auto-indent-minor-mode-map
;;  - Auto Indent mode map.
;; 
;; *** auto-indent-pairs-begin
;; Defines where the pair region begins.
;; 
;; *** auto-indent-pairs-end
;; Defines where the pair region ends.
;; *Auto indentation on moving cursor to blank lines.
;; 
;; *** auto-indent-current-pairs
;;  - Automatically indent the current parenthetical statement.
;; 
;; *** auto-indent-delete-line-char-add-extra-spaces
;;  - When deleting a return, add a space (when appropriate)
;; between the newly joined lines.
;; 
;; This takes care of the condition when deleting text
;; 
;; Lorem ipsum dolor sit|
;; amet, consectetur adipiscing elit.  Morbi id
;; 
;; Lorem ipsum dolor sit|amet, consectetur adipiscing elit.  Morbi id
;; 
;; Which ideally should be deleted to:
;; 
;; Lorem ipsum dolor sit| amet, consectetur adipiscing elit.  Morbi id
;; 
;; This is controlled by the regular expressions in
;; `auto-indent-delete-line-char-add-extra-spaces-prog-mode-regs'
;; and
;; `auto-indent-delete-line-char-add-extra-spaces-text-mode-regs'
;; 
;; *** auto-indent-delete-line-char-add-extra-spaces-prog-mode-regs
;;  - Regular expressions for use with `auto-indent-delete-line-char-add-extra-spaces'.  This is used for programming modes as determined by `auto-indent-is-prog-mode-p'.
;; 
;; *** auto-indent-delete-line-char-add-extra-spaces-text-mode-regs
;;  - Regular expressions for use with `auto-indent-delete-line-char-add-extra-spaces'.  This is used for programming modes as determined by `auto-indent-is-prog-mode-p'.
;; 
;; *** auto-indent-delete-line-char-remove-extra-spaces
;; When deleting a return, delete any extra spaces between the newly joined lines.
;; 
;; *** auto-indent-delete-line-char-remove-last-space
;; Remove last space when deleting a line.
;; 
;; When `auto-indent-delete-line-char-remove-extra-spaces' is enabled,
;; expressions like lists can be removed in a less than optimal
;; manner.  For example, assuming =`|=' is the cursor:
;; 
;; c("Vehicle QD TO",|
;;      "1 ug IVT","3 ug IVT",...
;; 
;; would be deleted to the following
;; 
;; c("Vehicle QD TO",| "1 ug IVT","3 ug IVT",...
;; 
;; In this case it would be preferable to delete to:
;; 
;; c("Vehicle QD TO",|"1 ug IVT","3 ug IVT",...
;; 
;; However cases like sentences:
;; 
;; Lorem ipsum dolor sit amet,|
;;      consectetur adipiscing elit. Morbi id
;; 
;; Deletes to
;; Lorem ipsum dolor sit amet,| consectetur adipiscing elit. Morbi id
;; 
;; Which is a desired behavior.
;; 
;; When this is enabled, auto-indent attempts to be smarter by
;; deleting the extra space when characters before and after match
;; expressions defined in
;; `auto-indent-delete-line-char-remove-last-space-prog-mode-regs' and
;; `auto-indent-delete-line-char-remove-last-space-text-mode-regs'.
;; 
;; *** auto-indent-delete-line-char-remove-last-space-prog-mode-regs
;;  - Regular expressions for use with `auto-indent-delete-line-char-remove-last-space'.  This is used for programming modes as determined by `auto-indent-is-prog-mode-p'.
;; 
;; *** auto-indent-delete-line-char-remove-last-space-text-mode-regs
;; Regular expressions for use with `auto-indent-delete-line-char-remove-last-space'.  This is used for modes other than programming modes.  This is determined by `auto-indent-is-prog-mode-p'.
;; 
;; *** auto-indent-delete-trailing-whitespace-on-save-file
;;  - When saving file delete trailing whitespace.
;; 
;; *** auto-indent-delete-trailing-whitespace-on-visit-file
;;  - Automatically remove trailing whitespace when visiting  file.
;; 
;; *** auto-indent-disabled-indent-functions
;; List of disabled indent functions.
;; 
;; List of functions that auto-indent ignores the `indent-region' on
;; paste and automated indent by pressing return.  The default is
;; `indent-relative' and `indent-relative-maybe'.  If these are used the
;; indentation is may not specified for the current mode.
;; 
;; *** auto-indent-disabled-modes-list
;; List of modes disabled when global `auto-indent-mode' is on.
;; 
;; *** auto-indent-disabled-modes-on-save
;;  - List of modes where `indent-region' of the whole file is ignored.
;; 
;; *** auto-indent-engine
;; Type of engine to use.  The possibilities are:
;; 
;; default: Use hooks and advices to implement auto-indent-mode
;; 
;; keymap: Use key remappings to implement auto-indent-mode.  This may
;; work in some modes but may cause things such as `company-mode' or
;; `auto-complete-mode' to function improperly
;; 
;; *** auto-indent-eol-char
;; End of line/statement character, like C or matlab's semi-colon.
;; 
;; Character inserted when
;; `auto-indent-key-for-end-of-line-inser-char-then-newline' is
;; defined.  This is a buffer local variable, therefore if you have
;; a mode that instead of using a semi-colon for an end of
;; statement, you use a colon, this can be added to the mode as
;; follows:
;; 
;;      (add-hook 'strange-mode-hook (lambda() (setq auto-indent-eol-char ":")))
;; 
;; autoThis is similar to Textmate's behavior.  This is useful when used
;; in conjunction with something that pairs delimiters like `autopair-mode'.
;; 
;; *** auto-indent-fix-org-auto-fill
;; Fixes org-based
;;   auto-fill-function (i.e. `org-auto-fill-function') to only
;;   auto-fill for things outside of a source block.
;; 
;; *** auto-indent-fix-org-backspace
;; Fixes `org-backspace' to use `auto-indent-backward-delete-char-behavior' for `org-mode' buffers.
;; 
;; *** auto-indent-fix-org-move-beginning-of-line
;; Fixes `move-beginning-of-line' in `org-mode' when in source blocks to follow `auto-indent-mode'.
;; 
;; *** auto-indent-fix-org-return
;; Allows newline and indent behavior in source code blocks in org-mode.
;; 
;; *** auto-indent-fix-org-yank
;; Allows org-mode yanks to be indented in source code blocks of org-mode.
;; 
;; *** auto-indent-force-interactive-advices
;; Forces interactive advices.
;; 
;; This makes sure that this is called when this is an interactive
;; call directly to the function.  However, if someone defines
;; something such as `org-delete-char' to delete a character, when
;; `org-delete-char' is called interactively and then calls
;; `delete-char' the advice is never activated (when it should be).
;; If this is activated, `auto-indent-mode' tries to do the right
;; thing by guessing what key should have been pressed to get this
;; event.  If it is the key that was pressed enable the advice.
;; 
;; *** auto-indent-home-is-beginning-of-indent
;; The Home key, or rather the `move-beginning-of-line' function, will move to the beginning of the indentation when called interactively.  If it is already at the beginning of the indent, move to the beginning of the line.
;; 
;; *** auto-indent-home-is-beginning-of-indent-when-spaces-follow
;; This is a customization for the home key.
;; 
;; If `auto-indent-home-is-beginning-of-indent' is enabled, the Home
;; key, or rather the `move-beginning-of-line' function, will move
;; to the beginning of the indentation when called interactively.
;; 
;; If it is already at the beginning of the indent,and move to the
;; beginning of the line.  When
;; `auto-indent-home-is-beginning-of-indent-when-spaces-follow' is
;; enabled, a home key press from
;; 
;;     (defadvice move-beginning-of-line (around auto-indent-minor-mode-advice)
;;     | (let (at-beginning)
;; 
;; will change to
;; 
;;     (defadvice move-beginning-of-line (around auto-indent-minor-mode-advice)
;;       |(let (at-beginning)
;; 
;; Another home-key will chang to cursor
;; 
;;     (defadvice move-beginning-of-line (around auto-indent-minor-mode-advice)
;; |   (let (at-beginning)
;; 
;; *** auto-indent-key-for-end-of-line-insert-char-then-newline
;; Key for end of line, `auto-indent-eol-char', then newline.
;; 
;; By default the `auto-indent-eol-char' is the semicolon. TextMate
;; uses shift-meta return, I believe (S-M-RET). If blank, no key is
;; defined.  The key should be in a format used for having keyboard
;; macros (see `edmacro-mode'). This is useful when used in
;; conjunction with something that pairs delimiters like
;; `autopair-mode'.
;; 
;; *** auto-indent-key-for-end-of-line-then-newline
;; Key for end of line, then newline.
;; 
;; TextMate uses meta return, I believe (M-RET).  If blank, no key
;; is defined. The key should be in a format used for saving
;; keyboard macros (see `edmacro-mode'). This is useful when used in
;; conjunction with something that pairs delimiters like `autopair-mode'.
;; 
;; *** auto-indent-kill-line-at-eol
;; Determines how a kill at the end of line behaves.
;; 
;; When killing lines, if at the end of a line,
;; 
;; nil - join next line to the current line.  Deletes white-space at
;;          join.  [this essentially duplicated delete-char]
;; 
;;          See also `auto-indent-kill-remove-extra-spaces'
;; 
;; whole-line - kill next lines
;; 
;; subsequent-whole-lines - merge lines on first call, subsequent kill whole lines
;; 
;; blanks - kill all empty lines after the current line, and then
;;             any lines specified.
;; 
;; You should also set the function `kill-whole-line' to do what you
;; want.
;; 
;; *** auto-indent-kill-line-kill-region-when-active
;;  - When killing lines, if region is active, kill region instead.
;; 
;; *** auto-indent-kill-remove-extra-spaces
;;  - Remove indentation before killing the line or region.
;; 
;; *** auto-indent-known-indent-level-variables
;; Known indent-level-variables for major modes.  Set locally when auto-indent-mode initializes.
;; 
;; *** auto-indent-known-text-modes
;;  - List of auto-indent's known text-modes.
;; 
;; *** auto-indent-minor-mode-symbol
;;  - When true, Auto Indent puts AI on the mode line.
;; 
;; *** auto-indent-mode-untabify-on-yank-or-paste
;;  - Untabify pasted or yanked region.
;; 
;; *** auto-indent-newline-function
;;  - Auto indentation function for the return key.
;; 
;; *** auto-indent-next-pair
;; Automatically indent the next parenthetical statement.  For example in R:
;; 
;; d| <- read.csv("dat.csv",
;;                   na.strings=c(".","NA"))
;; 
;; When typing .old, the indentation will be updated as follows:
;; 
;; d.old <- read.csv("dat.csv",
;;                      na.strings=c(".","NA"))
;; 
;; This will slow down your computation, so if you use it make sure
;; that the `auto-indent-next-pair-timer-interval' is appropriate
;; for your needs.
;; 
;; It is useful when using this option to have some sort of autopairing on.
;; 
;; *** auto-indent-next-pair-timer-geo-mean
;; Number of seconds before the observed parenthetical statement is indented.
;; The faster the value, the slower Emacs responsiveness but the
;; faster Emacs indents the region.  The slower the value, the
;; faster Emacs responds.  This should be changed dynamically by
;; to the geometric mean of rate to indent a single line.
;; 
;; *** auto-indent-on-save-file
;;  - Auto Indent on visit file.
;; 
;; *** auto-indent-on-visit-file
;;  - Auto Indent file upon visit.
;; 
;; *** auto-indent-on-visit-pretend-nothing-changed
;;  - When modifying the file on visit, pretend nothing changed.
;; 
;; *** auto-indent-on-yank-or-paste
;;  - Indent pasted or yanked region.
;; 
;; *** auto-indent-start-org-indent
;; Starts `org-indent-mode' when in org-mode.
;; 
;; *** auto-indent-untabify-on-save-file
;;  - Change tabs to spaces on file-save.
;; 
;; *** auto-indent-untabify-on-visit-file
;;  - Automatically convert tabs into spaces when visiting a file.
;; 
;; *** auto-indent-use-text-boundaries
;; Use text boundaries when killing lines.
;; 
;; When killing lines, if point is before any text, act as if
;; point is at BOL.  And if point is after text, act as if point
;;      is at EOL
;; 
;; ** Internal Variables
;; 
;; *** auto-indent-eol-ret-save
;; Saved variable for keyboard state.
;; 
;; *** auto-indent-eol-ret-semi-save
;; Saved variable for keyboard state.
;; 
;; *** auto-indent-minor-mode-map
;;  - Auto Indent mode map.
;; 
;; *** auto-indent-pairs-begin
;; Defines where the pair region begins.
;; 
;; *** auto-indent-pairs-end
;; Defines where the pair region ends.
;; *Auto indentation on moving cursor to blank lines.
;; 
;; *** auto-indent-current-pairs
;;  - Automatically indent the current parenthetical statement.
;; 
;; *** auto-indent-delete-line-char-add-extra-spaces
;;  - When deleting a return, add a space (when appropriate)
;; between the newly joined lines.
;; 
;; This takes care of the condition when deleting text
;; 
;; Lorem ipsum dolor sit|
;; amet, consectetur adipiscing elit.  Morbi id
;; 
;; Lorem ipsum dolor sit|amet, consectetur adipiscing elit.  Morbi id
;; 
;; Which ideally should be deleted to:
;; 
;; Lorem ipsum dolor sit| amet, consectetur adipiscing elit.  Morbi id
;; 
;; This is controlled by the regular expressions in
;; `auto-indent-delete-line-char-add-extra-spaces-prog-mode-regs'
;; and
;; `auto-indent-delete-line-char-add-extra-spaces-text-mode-regs'
;; 
;; *** auto-indent-delete-line-char-add-extra-spaces-prog-mode-regs
;;  - Regular expressions for use with `auto-indent-delete-line-char-add-extra-spaces'.  This is used for programming modes as determined by `auto-indent-is-prog-mode-p'.
;; 
;; *** auto-indent-delete-line-char-add-extra-spaces-text-mode-regs
;;  - Regular expressions for use with `auto-indent-delete-line-char-add-extra-spaces'.  This is used for programming modes as determined by `auto-indent-is-prog-mode-p'.
;; 
;; *** auto-indent-delete-line-char-remove-extra-spaces
;; When deleting a return, delete any extra spaces between the newly joined lines.
;; 
;; *** auto-indent-delete-line-char-remove-last-space
;; Remove last space when deleting a line.
;; 
;; When `auto-indent-delete-line-char-remove-extra-spaces' is enabled,
;; expressions like lists can be removed in a less than optimal
;; manner.  For example, assuming =`|=' is the cursor:
;; 
;; c("Vehicle QD TO",|
;;      "1 ug IVT","3 ug IVT",...
;; 
;; would be deleted to the following
;; 
;; c("Vehicle QD TO",| "1 ug IVT","3 ug IVT",...
;; 
;; In this case it would be preferable to delete to:
;; 
;; c("Vehicle QD TO",|"1 ug IVT","3 ug IVT",...
;; 
;; However cases like sentences:
;; 
;; Lorem ipsum dolor sit amet,|
;;      consectetur adipiscing elit. Morbi id
;; 
;; Deletes to
;; Lorem ipsum dolor sit amet,| consectetur adipiscing elit. Morbi id
;; 
;; Which is a desired behavior.
;; 
;; When this is enabled, auto-indent attempts to be smarter by
;; deleting the extra space when characters before and after match
;; expressions defined in
;; `auto-indent-delete-line-char-remove-last-space-prog-mode-regs' and
;; `auto-indent-delete-line-char-remove-last-space-text-mode-regs'.
;; 
;; *** auto-indent-delete-line-char-remove-last-space-prog-mode-regs
;;  - Regular expressions for use with `auto-indent-delete-line-char-remove-last-space'.  This is used for programming modes as determined by `auto-indent-is-prog-mode-p'.
;; 
;; *** auto-indent-delete-line-char-remove-last-space-text-mode-regs
;; Regular expressions for use with `auto-indent-delete-line-char-remove-last-space'.  This is used for modes other than programming modes.  This is determined by `auto-indent-is-prog-mode-p'.
;; 
;; *** auto-indent-delete-trailing-whitespace-on-save-file
;;  - When saving file delete trailing whitespace.
;; 
;; *** auto-indent-delete-trailing-whitespace-on-visit-file
;;  - Automatically remove trailing whitespace when visiting  file.
;; 
;; *** auto-indent-disabled-indent-functions
;; List of disabled indent functions.
;; 
;; List of functions that auto-indent ignores the `indent-region' on
;; paste and automated indent by pressing return.  The default is
;; `indent-relative' and `indent-relative-maybe'.  If these are used the
;; indentation is may not specified for the current mode.
;; 
;; *** auto-indent-disabled-modes-list
;; List of modes disabled when global `auto-indent-mode' is on.
;; 
;; *** auto-indent-disabled-modes-on-save
;;  - List of modes where `indent-region' of the whole file is ignored.
;; 
;; *** auto-indent-engine
;; Type of engine to use.  The possibilities are:
;; 
;; default: Use hooks and advices to implement auto-indent-mode
;; 
;; keymap: Use key remappings to implement auto-indent-mode.  This may
;; work in some modes but may cause things such as `company-mode' or
;; `auto-complete-mode' to function improperly
;; 
;; *** auto-indent-eol-char
;; End of line/statement character, like C or matlab's semi-colon.
;; 
;; Character inserted when
;; `auto-indent-key-for-end-of-line-inser-char-then-newline' is
;; defined.  This is a buffer local variable, therefore if you have
;; a mode that instead of using a semi-colon for an end of
;; statement, you use a colon, this can be added to the mode as
;; follows:
;; 
;;      (add-hook 'strange-mode-hook (lambda() (setq auto-indent-eol-char ":")))
;; 
;; autoThis is similar to Textmate's behavior.  This is useful when used
;; in conjunction with something that pairs delimiters like `autopair-mode'.
;; 
;; *** auto-indent-fix-org-auto-fill
;; Fixes org-based
;;   auto-fill-function (i.e. `org-auto-fill-function') to only
;;   auto-fill for things outside of a source block.
;; 
;; *** auto-indent-fix-org-backspace
;; Fixes `org-backspace' to use `auto-indent-backward-delete-char-behavior' for `org-mode' buffers.
;; 
;; *** auto-indent-fix-org-move-beginning-of-line
;; Fixes `move-beginning-of-line' in `org-mode' when in source blocks to follow `auto-indent-mode'.
;; 
;; *** auto-indent-fix-org-return
;; Allows newline and indent behavior in source code blocks in org-mode.
;; 
;; *** auto-indent-fix-org-yank
;; Allows org-mode yanks to be indented in source code blocks of org-mode.
;; 
;; *** auto-indent-force-interactive-advices
;; Forces interactive advices.
;; 
;; This makes sure that this is called when this is an interactive
;; call directly to the function.  However, if someone defines
;; something such as `org-delete-char' to delete a character, when
;; `org-delete-char' is called interactively and then calls
;; `delete-char' the advice is never activated (when it should be).
;; If this is activated, `auto-indent-mode' tries to do the right
;; thing by guessing what key should have been pressed to get this
;; event.  If it is the key that was pressed enable the advice.
;; 
;; *** auto-indent-home-is-beginning-of-indent
;; The Home key, or rather the `move-beginning-of-line' function, will move to the beginning of the indentation when called interactively.  If it is already at the beginning of the indent, move to the beginning of the line.
;; 
;; *** auto-indent-home-is-beginning-of-indent-when-spaces-follow
;; This is a customization for the home key.
;; 
;; If `auto-indent-home-is-beginning-of-indent' is enabled, the Home
;; key, or rather the `move-beginning-of-line' function, will move
;; to the beginning of the indentation when called interactively.
;; 
;; If it is already at the beginning of the indent,and move to the
;; beginning of the line.  When
;; `auto-indent-home-is-beginning-of-indent-when-spaces-follow' is
;; enabled, a home key press from
;; 
;;     (defadvice move-beginning-of-line (around auto-indent-minor-mode-advice)
;;     | (let (at-beginning)
;; 
;; will change to
;; 
;;     (defadvice move-beginning-of-line (around auto-indent-minor-mode-advice)
;;       |(let (at-beginning)
;; 
;; Another home-key will chang to cursor
;; 
;;     (defadvice move-beginning-of-line (around auto-indent-minor-mode-advice)
;; |   (let (at-beginning)
;; 
;; *** auto-indent-key-for-end-of-line-insert-char-then-newline
;; Key for end of line, `auto-indent-eol-char', then newline.
;; 
;; By default the `auto-indent-eol-char' is the semicolon. TextMate
;; uses shift-meta return, I believe (S-M-RET). If blank, no key is
;; defined.  The key should be in a format used for having keyboard
;; macros (see `edmacro-mode'). This is useful when used in
;; conjunction with something that pairs delimiters like
;; `autopair-mode'.
;; 
;; *** auto-indent-key-for-end-of-line-then-newline
;; Key for end of line, then newline.
;; 
;; TextMate uses meta return, I believe (M-RET).  If blank, no key
;; is defined. The key should be in a format used for saving
;; keyboard macros (see `edmacro-mode'). This is useful when used in
;; conjunction with something that pairs delimiters like `autopair-mode'.
;; 
;; *** auto-indent-kill-line-at-eol
;; Determines how a kill at the end of line behaves.
;; 
;; When killing lines, if at the end of a line,
;; 
;; nil - join next line to the current line.  Deletes white-space at
;;          join.  [this essentially duplicated delete-char]
;; 
;;          See also `auto-indent-kill-remove-extra-spaces'
;; 
;; whole-line - kill next lines
;; 
;; subsequent-whole-lines - merge lines on first call, subsequent kill whole lines
;; 
;; blanks - kill all empty lines after the current line, and then
;;             any lines specified.
;; 
;; You should also set the function `kill-whole-line' to do what you
;; want.
;; 
;; *** auto-indent-kill-line-kill-region-when-active
;;  - When killing lines, if region is active, kill region instead.
;; 
;; *** auto-indent-kill-remove-extra-spaces
;;  - Remove indentation before killing the line or region.
;; 
;; *** auto-indent-known-indent-level-variables
;; Known indent-level-variables for major modes.  Set locally when auto-indent-mode initializes.
;; 
;; *** auto-indent-known-text-modes
;;  - List of auto-indent's known text-modes.
;; 
;; *** auto-indent-minor-mode-symbol
;;  - When true, Auto Indent puts AI on the mode line.
;; 
;; *** auto-indent-mode-untabify-on-yank-or-paste
;;  - Untabify pasted or yanked region.
;; 
;; *** auto-indent-newline-function
;;  - Auto indentation function for the return key.
;; 
;; *** auto-indent-next-pair
;; Automatically indent the next parenthetical statement.  For example in R:
;; 
;; d| <- read.csv("dat.csv",
;;                   na.strings=c(".","NA"))
;; 
;; When typing .old, the indentation will be updated as follows:
;; 
;; d.old <- read.csv("dat.csv",
;;                      na.strings=c(".","NA"))
;; 
;; This will slow down your computation, so if you use it make sure
;; that the `auto-indent-next-pair-timer-interval' is appropriate
;; for your needs.
;; 
;; It is useful when using this option to have some sort of autopairing on.
;; 
;; *** auto-indent-next-pair-timer-geo-mean
;; Number of seconds before the observed parenthetical statement is indented.
;; The faster the value, the slower Emacs responsiveness but the
;; faster Emacs indents the region.  The slower the value, the
;; faster Emacs responds.  This should be changed dynamically by
;; to the geometric mean of rate to indent a single line.
;; 
;; *** auto-indent-on-save-file
;;  - Auto Indent on visit file.
;; 
;; *** auto-indent-on-visit-file
;;  - Auto Indent file upon visit.
;; 
;; *** auto-indent-on-visit-pretend-nothing-changed
;;  - When modifying the file on visit, pretend nothing changed.
;; 
;; *** auto-indent-on-yank-or-paste
;;  - Indent pasted or yanked region.
;; 
;; *** auto-indent-start-org-indent
;; Starts `org-indent-mode' when in org-mode.
;; 
;; *** auto-indent-untabify-on-save-file
;;  - Change tabs to spaces on file-save.
;; 
;; *** auto-indent-untabify-on-visit-file
;;  - Automatically convert tabs into spaces when visiting a file.
;; 
;; *** auto-indent-use-text-boundaries
;; Use text boundaries when killing lines.
;; 
;; When killing lines, if point is before any text, act as if
;; point is at BOL.  And if point is after text, act as if point
;;      is at EOL
;; 
;; ** Internal Variables
;; 
;; *** auto-indent-eol-ret-save
;; Saved variable for keyboard state.
;; 
;; *** auto-indent-eol-ret-semi-save
;; Saved variable for keyboard state.
;; 
;; *** auto-indent-minor-mode-map
;;  - Auto Indent mode map.
;; 
;; *** auto-indent-pairs-begin
;; Defines where the pair region begins.
;; 
;; *** auto-indent-pairs-end
;; Defines where the pair region ends.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;; 05-Dec-2012    Matthew L. Fidler  
;;    Last-Updated: Tue Aug 21 13:08:42 2012 (-0500) #1467 (Matthew L. Fidler)
;;    Added support for new ergoemacs-mode.  Also provided updated bug-fix for indent-region
;; 19-Nov-2012      
;;    Last-Updated: Tue Aug 21 13:08:42 2012 (-0500) #1467 (Matthew L. Fidler)
;;    Bug fix for aligning parenthetical region when a yasnippet is active
;;    (It messes up yasnippet expansions.)
;; 12-Nov-2012    Matthew L. Fidler  
;;    Last-Updated: Tue Aug 21 13:08:42 2012 (-0500) #1467 (Matthew L. Fidler)
;;    Bug fix for overflows and NaNs
;; 17-Oct-2012    Matthew L. Fidler  
;;    Last-Updated: Tue Aug 21 13:08:42 2012 (-0500) #1467 (Matthew L. Fidler)
;;    Bug fix for yanking in org-mode.
;;    
;; 17-Oct-2012    Matthew L. Fidler  
;;    Last-Updated: Tue Aug 21 13:08:42 2012 (-0500) #1467 (Matthew L. Fidler)
;;    Now auto-indent-mode can suppress auto-fill in source code
;;    blocks. Small bug fix for yanking.
;; 12-Oct-2012    Matthew L. Fidler
;;    Last-Updated: Tue Aug 21 13:08:42 2012 (-0500) #1467 (Matthew L. Fidler)
;;    Add auto-indent on yank support for org-mode code buffers
;; 12-Oct-2012    Matthew L. Fidler  
;;    Last-Updated: Tue Aug 21 13:08:42 2012 (-0500) #1467 (Matthew L. Fidler)
;;    Removed History section from texinfo file.
;; 12-Oct-2012    Matthew L. Fidler  
;;    Last-Updated: Tue Aug 21 13:08:42 2012 (-0500) #1467 (Matthew L. Fidler)
;;    Fix header readme by using the latest version of org-readme.
;; 12-Oct-2012    Matthew L. Fidler  
;;    Last-Updated: Tue Aug 21 13:08:42 2012 (-0500) #1467 (Matthew L. Fidler)
;;    Took out documentation that started with a star since it messes up org-readme.
;; 12-Oct-2012    Matthew L. Fidler  
;;    Last-Updated: Tue Aug 21 13:08:42 2012 (-0500) #1467 (Matthew L. Fidler)
;;    Trying to fix header
;; 12-Oct-2012    Matthew L. Fidler  
;;    Last-Updated: Tue Aug 21 13:08:42 2012 (-0500) #1467 (Matthew L. Fidler)
;;    Added better org-mode support for code-blocks.
;; 12-Sep-2012      
;;    Last-Updated: Tue Aug 21 13:08:42 2012 (-0500) #1467 (Matthew L. Fidler)
;;    Fixed commentary section.
;; 12-Sep-2012      
;;    Last-Updated: Tue Aug 21 13:08:42 2012 (-0500) #1467 (Matthew L. Fidler)
;;    Changed yasnippet checking to be compatible with yasnippet 0.8's
;;    function renaming.
;; 21-Aug-2012    Matthew L. Fidler  
;;    Last-Updated: Tue Aug 21 12:50:21 2012 (-0500) #1465 (Matthew L. Fidler)
;;    Attempt to fix documentation with updated org-readme.
;; 21-Aug-2012    Matthew L. Fidler  
;;    Last-Updated: Tue Aug 21 12:48:19 2012 (-0500) #1463 (Matthew L. Fidler)
;;    Added `auto-indent-next-pair-timer-interval-max' and a bug fix to the
;;    interval-growth algorithm.
;; 21-Aug-2012    Matthew L. Fidler  
;;    Last-Updated: Tue Aug 21 12:05:05 2012 (-0500) #1458 (Matthew L. Fidler)
;;    Attempt to change documentation.
;; 21-Aug-2012    Matthew L. Fidler  
;;    Last-Updated: Tue Aug 21 12:02:56 2012 (-0500) #1456 (Matthew L. Fidler)
;;    Changed the default
;;    `auto-indent-next-pairt-timer-interval-do-not-grow' to nil. 
;; 20-Aug-2012    Matthew L. Fidler  
;;    Last-Updated: Mon Aug 20 23:01:35 2012 (-0500) #1448 (Matthew L. Fidler)
;;    Drop Readme.md
;; 20-Aug-2012    Matthew L. Fidler  
;;    Last-Updated: Mon Aug 20 13:18:48 2012 (-0500) #1444 (Matthew L. Fidler)
;;    Another documentation revision.
;; 20-Aug-2012    Matthew L. Fidler  
;;    Last-Updated: Mon Aug 20 12:47:45 2012 (-0500) #1442 (Matthew L. Fidler)
;;    Documentation update.
;; 20-Aug-2012    Matthew L. Fidler  
;;    Last-Updated: Mon Aug 20 12:46:02 2012 (-0500) #1440 (Matthew L. Fidler)
;;    Added a generic function to change the number of spaces for an
;;    indentation.  Should fix issue #4.
;; 20-Aug-2012    Matthew L. Fidler  
;;    Last-Updated: Mon Aug 20 10:15:12 2012 (-0500) #1417 (Matthew L. Fidler)
;;    Clarified documentation
;; 20-Aug-2012    Matthew L. Fidler  
;;    Last-Updated: Wed Aug  8 23:02:18 2012 (-0500) #1415 (Matthew L. Fidler)
;;    Added some documentation about major mode indentation issues.
;; 7-Aug-2012    Matthew L. Fidler  
;;    Last-Updated: Sun Aug  5 12:36:11 2012 (-0500) #1411 (Matthew L. Fidler)
;;    Changed a mistake in the documentation; Autoindenting of balanced
;;    sexps are not supported by default but need to be enabled.
;; 04-Aug-2012    Matthew L. Fidler  
;;    Last-Updated: Sat Aug  4 21:30:02 2012 (-0500) #1403 (Matthew L. Fidler)
;;    Added ability to turn off dynamic growth of timers per mode.
;;    The algorithm to change has not been perfected yet.
;; 04-Aug-2012    Matthew L. Fidler
;;    Last-Updated: Sat Aug  4 01:25:56 2012 (-0500) #1390 (Matthew L. Idler)
;;    Fixed a bug introduced by cleaning typos.
;;    Changing again.
;; 03-Aug-2012    Matthew L. Fidler  
;;    Last-Updated: Fri Aug  3 22:47:15 2012 (-0500) #1381 (Matthew L. Fidler)
;;    Save indentation settings on exit emacs.
;; 03-Aug-2012    Matthew L. Fidler  
;;    Last-Updated: Fri Aug  3 22:23:48 2012 (-0500) #1375 (Matthew L. Fidler)
;;    Fixed Documentation, and a few minor bugs caught by linting.
;; 30-Jul-2012    Matthew L. Fidler
;;    Last-Updated: Mon Jul 30 19:18:11 2012 (-0500) #1361 (Matthew L. Fidler)
;;    Made the Fix for issue #3 more specific to org tables.
;; 30-Jul-2012    Matthew L. Fidler
;;    Last-Updated: Mon Jul 30 19:07:02 2012 (-0500) #1357 (Matthew L. Fidler)
;;    Actual Fix for Issue #3.  Now the delete character may not work
;;    in org-mode.
;; 23-Jul-2012    Matthew L. Fidler
;;    Last-Updated: Mon Jul 23 20:54:00 2012 (-0500) #1353 (Matthew L. Fidler)
;;    Fix Issue #3.  Thanks harrylove for pointing it out.
;; 02-Jul-2012    Matthew L. Fidler
;;    Last-Updated: Mon Jul  2 16:12:20 2012 (-0500) #1341 (Matthew L. Fidler)
;;    Have an mode-based timer normalized to the number of lines used
;;    for next parenthetical indentation.
;; 26-Jun-2012    Matthew L. Fidler
;;    Last-Updated: Tue Jun 26 09:14:02 2012 (-0500) #1320 (Matthew L. Fidler)
;;    Bug fix for point-shift involved in `auto-indent-after-yank-hook'
;; 13-Jun-2012    Matthew L. Fidler
;;    Last-Updated: Wed Jun 13 10:34:07 2012 (-0500) #1307 (Matthew L. Fidler)
;;    Added `auto-indent-after-yank-hook'
;; 18-May-2012    Matthew L. Fidler
;;    Last-Updated: Fri May 18 14:53:11 2012 (-0500) #1304 (Matthew L. Fidler)
;;    Changed `auto-indent-next-pair' to be off by default.
;; 13-Mar-2012    Matthew L. Fidler
;;    Last-Updated: Tue Mar 13 09:38:39 2012 (-0500) #1302 (Matthew L. Fidler)
;;    Made timer for parenthetical statements customizable.
;; 06-Mar-2012    Matthew L. Fidler
;;    Last-Updated: Tue Mar  6 22:35:39 2012 (-0600) #1299 (Matthew L. Fidler)
;;    Speed enhancements for parenthetical statements.
;; 05-Mar-2012    Matthew L. Fidler
;;    Last-Updated: Mon Mar  5 23:06:45 2012 (-0600) #1292 (Matthew L. Fidler)
;;    Bug fix for autopair-backspace.
;; 05-Mar-2012    Matthew L. Fidler
;;    Last-Updated: Wed Feb 29 22:24:14 2012 (-0600) #1282 (Matthew L. Fidler)
;;    Have backspace cancel parenthetical alignment timer canceling
;; 29-Feb-2012    Matthew L. Fidler
;;    Last-Updated: Wed Feb 29 15:39:01 2012 (-0600) #1278 (Matthew L. Fidler)
;;    Bug fix for paren handling.
;; 29-Feb-2012    Matthew L. Fidler
;;    Last-Updated: Wed Feb 29 13:52:33 2012 (-0600) #1276 (Matthew L. Fidler)
;;    Made the handling of pairs a timer-based function so it doesn't
;;    interfere with work flow.
;; 29-Feb-2012    Matthew L. Fidler
;;    Last-Updated: Wed Feb 29 13:20:17 2012 (-0600) #1262 (Matthew L. Fidler)
;;    Better handling of pairs.
;; 28-Feb-2012    Matthew L. Fidler
;;    Last-Updated: Tue Feb 28 14:36:30 2012 (-0600) #1236 (Matthew L. Fidler)
;;    Added subsequent-whole-line from Le Wang's fork.
;; 14-Feb-2012    Matthew L. Fidler
;;    Last-Updated: Tue Feb 14 19:16:10 2012 (-0600) #1230 (Matthew L. Fidler)
;;    Fixing issue #2
;; 01-Feb-2012    Matthew L. Fidler
;;    Last-Updated: Wed Feb  1 21:50:32 2012 (-0600) #1215 (Matthew L. Fidler)
;;    Added makefile-gmake-mode to the excluded auto-indent modes.
;; 22-Dec-2011    Matthew L. Fidler
;;    Last-Updated: Thu Dec 22 13:47:07 2011 (-0600) #1211 (Matthew L. Fidler)
;;    Added bug fix for home-key
;; 21-Dec-2011    Matthew L. Fidler
;;    Last-Updated: Wed Dec 21 11:17:02 2011 (-0600) #1209 (Matthew L. Fidler)
;;    Added another smart delete case.
;; 14-Dec-2011    Matthew L. Fidler
;;    Last-Updated: Wed Dec 14 15:32:30 2011 (-0600) #1206 (Matthew L. Fidler)
;;    Went back to last known working
;;    `auto-indent-def-del-forward-char' and deleted message.
;; 14-Dec-2011    Matthew L. Fidler
;;    Last-Updated: Wed Dec 14 15:28:12 2011 (-0600) #1205 (Matthew L. Fidler)
;;    Another Paren
;; 14-Dec-2011    Matthew L. Fidler
;;    Last-Updated: Wed Dec 14 14:06:47 2011 (-0600) #1203 (Matthew L. Fidler)
;;    Paren Bug Fix.
;; 14-Dec-2011    Matthew L. Fidler
;;    Last-Updated: Tue Dec 13 13:43:46 2011 (-0600) #1199 (us041375)
;;    Changed the `auto-indent-kill-remove-extra-spaces' default to
;;    nil so that you copy-paste what you expect.
;; 10-Dec-2011    Matthew L. Fidler
;;    Last-Updated: Sat Dec 10 20:53:28 2011 (-0600) #1192 (Matthew L. Fidler)
;;    Bug fix for annoying old debugging macros.
;; 08-Dec-2011    Matthew L. Fidler
;;    Last-Updated: Thu Dec  8 15:07:44 2011 (-0600) #1190 (Matthew L. Fidler)
;;    Added autoload cookie.
;; 08-Dec-2011    Matthew L. Fidler
;;    Last-Updated: Thu Dec  8 12:19:30 2011 (-0600) #1186 (Matthew L. Fidler)
;;    Bug fix for duplicate macros
;; 08-Dec-2011    Matthew L. Fidler
;;    Last-Updated: Thu Dec  8 11:04:52 2011 (-0600) #1164 (Matthew L. Fidler)
;;    Added (( and )) to the automatically delete extra whitespace at
;;    the end of a function list.
;; 08-Dec-2011    Matthew L. Fidler
;;    Last-Updated: Thu Dec  8 10:19:54 2011 (-0600) #1161 (Matthew L. Fidler)
;;    Added
;;    `auto-indent-alternate-return-function-for-end-of-line-then-newline'
;;    option
;; 08-Dec-2011    Matthew L. Fidler
;;    Last-Updated: Thu Dec  8 09:57:19 2011 (-0600) #1157 (Matthew L. Fidler)
;;    Added a possibility of adding a space if necessary.
;; 08-Dec-2011    Matthew L. Fidler
;;    Last-Updated: Thu Dec  8 08:51:13 2011 (-0600) #1119 (Matthew L. Fidler)
;;    Smarter delete end of line character enhancements.
;; 08-Dec-2011    Matthew L. Fidler
;;    Last-Updated: Thu Dec  8 08:16:14 2011 (-0600) #1110 (Matthew L. Fidler)
;;    Changed default options.
;; 29-Nov-2011    Matthew L. Fidler
;;    Last-Updated: Tue Nov 29 14:05:04 2011 (-0600) #1093 (Matthew L. Fidler)
;;    Bug Fix in `auto-indent-mode-pre-command-hook'
;; 28-Nov-2011    Matthew L. Fidler
;;    Last-Updated: Mon Nov 28 12:52:30 2011 (-0600) #1089 (Matthew L. Fidler)
;;    Bugfix for auto-indent-mode
;; 21-Nov-2011    Matthew L. Fidler
;;    Last-Updated: Mon Nov 21 10:22:28 2011 (-0600) #1085 (Matthew L. Fidler)
;;    Changed `auto-indent-after-begin-or-finish-sexp' to be called
;;    after every other hook has been run.  That way autopair-mode
;;    should be indented correctly.
;; 18-Nov-2011    Matthew L. Fidler
;;    Last-Updated: Fri Nov 18 15:28:10 2011 (-0600) #1063 (Matthew L. Fidler)
;;    Added `auto-indent-after-begin-or-finish-sexp'
;; 08-Apr-2011    Matthew L. Fidler
;;    Last-Updated: Fri Apr  8 23:08:08 2011 (-0500) #1014 (MatthewL. Fidler)
;;    Bug fix for when Yasnippet is disabled. Now will work with it
;;    disabled or enabled.
;; 08-Mar-2011    Matthew L. Fidler
;;    Last-Updated: Mon Feb  7 12:50:38 2011 (-0600) #1005 (Matthew L. Fidler)
;;    Changed `auto-indent-delete-line-char-remove-extra-spaces' to nil by default.
;; 16-Feb-2011    Matthew L. Fidler
;;    Last-Updated: Mon Feb  7 12:50:38 2011 (-0600) #1005 (Matthew L. Fidler)
;;    Added a just one space function for pasting
;; 15-Feb-2011    Matthew L. Fidler
;;    Last-Updated: Mon Feb  7 12:50:38 2011 (-0600) #1005 (Matthew L. Fidler)
;;    Removed the deactivation of advices when this mode is turned off.  I think it was causing some issues.
;; 10-Feb-2011    Matthew L. Fidler
;;    Last-Updated: Mon Feb  7 12:50:38 2011 (-0600) #1005 (Matthew L. Fidler)
;;    Added check to make sure not trying to paste on indent for
;;    `auto-indent-disabled-modes-list'

;; 03-Feb-2011    Matthew L. Fidler
;;    Last-Updated: Thu Feb  3 17:06:22 2011 (-0600) #996 (Matthew L. Fidler)

;;    Swap `backward-delete-char' with
;;    `backward-delete-char-untabify'.  Also use
;;    `auto-indent-backward-delete-char-behavior' when
;;    auto-indent-mode is active.

;; 03-Feb-2011    Matthew L. Fidler
;;    Last-Updated: Thu Feb  3 16:11:50 2011 (-0600) #943 (Matthew L. Fidler)

;;    Added definition of `cua-copy-region' to advised functions (I
;;    thought it would have been taken care of with `kill-ring-save')

;; 03-Feb-2011    Matthew L. Fidler
;;    Last-Updated: Thu Feb  3 15:45:54 2011 (-0600) #918 (Matthew L. Fidler)

;;    Added option to delete indentation when copying or cutting
;;    regions using `kill-region' and `kill-ring-save'.  Also changed
;;    `auto-indent-kill-line-remove-extra-spaces' to
;;    `auto-indent-kill-remove-extra-spaces'

;; 03-Feb-2011    Matthew L. Fidler
;;    Last-Updated: Thu Feb  3 10:50:24 2011 (-0600) #870 (Matthew L. Fidler)
;;    Made sure that auto-indent-kill-line doesn't use the kill-line advice.
;; 03-Feb-2011    Matthew L. Fidler
;;    Last-Updated: Thu Feb  3 09:26:22 2011 (-0600) #837 (Matthew L. Fidler)
;;    
;; 03-Feb-2011    Matthew L. Fidler
;;    Last-Updated: Thu Feb  3 09:26:08 2011 (-0600) #836 (Matthew L. Fidler)
;;    Another kill-line bug-fix.
;; 03-Feb-2011    Matthew L. Fidler
;;    Last-Updated: Thu Feb  3 09:04:14 2011 (-0600) #821 (Matthew L. Fidler)
;;    Fixed the kill-line bug
;; 03-Feb-2011    Matthew L. Fidler
;;    Last-Updated: Thu Feb  3 08:35:06 2011 (-0600) #815 (Matthew L. Fidler)
;;    yank engine bug fix.
;; 03-Feb-2011    Matthew L. Fidler
;;    Last-Updated: Thu Feb  3 08:09:04 2011 (-0600) #782 (Matthew L. Fidler)
;;    Bug fix for determining if the function is a yank
;; 02-Feb-2011    Matthew L. Fidler
;;    Last-Updated: Wed Feb  2 13:22:13 2011 (-0600) #756 (Matthew L. Fidler)
;;    Added kill-line bug-fix from Le Wang.
;; 
;;    Also there is a the bug of when called as a function, you need
;;    to check for disabled modes every time.
;;
;; 02-Feb-2011    Matthew L. Fidler
;;    Last-Updated: Wed Feb  2 11:38:44 2011 (-0600) #736 (Matthew L. Fidler)

;;    Added interactive requriment again.  This time tried to
;;    back-guess if the key has been hijacked.  If so assume it was
;;    called interactively.

;; 01-Feb-2011    Matthew L. Fidler
;;    Last-Updated: Tue Feb  1 22:58:45 2011 (-0600) #667 (Matthew L. Fidler)
;;    Took out the interactive requirement again.  Causes bugs like
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
;;    * Kill region when region is active is controled by
;;      auto-indent-kill-line-kill-region-when-active
;;    * Kill-line when at eol has more options
;;    * Change auto-indent-indentation-function to auto-indent-newline-function
;;
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
;;    Added fix for ortbl-minor-mode.  Now it will work when
;;    orgtbl-minor mode is enabled.
;; 09-Dec-2010    Matthew L. Fidler
;;    Last-Updated: Thu Dec  9 09:17:45 2010 (-0600) #414 (Matthew L. Fidler)
;;    Bugfix.  Now instead of indenting the region pasted, indent the
;;    region-pasted + beginning of line at region begin and end of
;;    line at region end.
;; 02-Dec-2010    Matthew L. Fidler

;;    Last-Updated: Thu Dec  2 13:02:02 2010 (-0600) #411 (Matthew L. Fidler)
;;    Made ignoring of modes with indent-relative and
;;    indent-relative-maybe apply to indenting returns as well.
;; 02-Dec-2010    Matthew L. Fidler
;;    Last-Updated: Thu Dec  2 11:38:37 2010 (-0600) #402 (Matthew L. Fidler)
;;    Removed auto-indent on paste/yank for modes with indent-relative
;;    and indent-relative-maybe.  This has annoyed me forever.
;; 02-Dec-2010    Matthew L. Fidler
;;    Last-Updated: Thu Dec  2 10:40:05 2010 (-0600) #397 (Matthew L. Fidler)
;;    Added an advice to delete-char.  When deleting a new-line
;;    character, shrink white-spaces afterward.
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
;; 09-Nov-2010   Matthew L. Fidler
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


(defvar auto-indent-mode nil)

(defgroup auto-indent nil
  "* Auto Indent Mode Customizations"
  :group 'editing)

(defcustom auto-indent-home-is-beginning-of-indent t
  "The Home key, or rather the `move-beginning-of-line' function, will move to the beginning of the indentation when called interactively.  If it is already at the beginning of the indent, move to the beginning of the line."
  :type 'boolean
  :group 'auto-indent)

(defcustom auto-indent-home-is-beginning-of-indent-when-spaces-follow t
  "This is a customization for the home key.

If `auto-indent-home-is-beginning-of-indent' is enabled, the Home
key, or rather the `move-beginning-of-line' function, will move
to the beginning of the indentation when called interactively.

If it is already at the beginning of the indent,and move to the
beginning of the line.  When
`auto-indent-home-is-beginning-of-indent-when-spaces-follow' is
enabled, a home key press from

    (defadvice move-beginning-of-line (around auto-indent-minor-mode-advice)
    | (let (at-beginning)

will change to

    (defadvice move-beginning-of-line (around auto-indent-minor-mode-advice)
      |(let (at-beginning)

Another home-key will chang to cursor

    (defadvice move-beginning-of-line (around auto-indent-minor-mode-advice)
|   (let (at-beginning)"
  :type 'boolean
  :group 'auto-indent)

(defcustom auto-indent-current-pairs t
  "* Automatically indent the current parenthetical statement."
  :type 'boolean
  :group 'auto-indent)

(defalias 'auto-indent-after-begin-or-finish-sexp 'auto-indent-current-pairs)

(defcustom auto-indent-next-pair nil
  "Automatically indent the next parenthetical statement.  For example in R:

d| <- read.csv(\"dat.csv\",
                  na.strings=c(\".\",\"NA\"))

When typing .old, the indentation will be updated as follows:

d.old <- read.csv(\"dat.csv\",
                     na.strings=c(\".\",\"NA\"))

This will slow down your computation, so if you use it make sure
that the `auto-indent-next-pair-timer-interval' is appropriate
for your needs.

It is useful when using this option to have some sort of autopairing on."
  :type 'boolean
  :group 'auto-indent)

(defcustom auto-indent-next-pair-timer-geo-mean '((default 0.0005 0))
  "Number of seconds before the observed parenthetical statement is indented.
The faster the value, the slower Emacs responsiveness but the
faster Emacs indents the region.  The slower the value, the
faster Emacs responds.  This should be changed dynamically by
to the geometric mean of rate to indent a single line."
  :type '(repeat (list (symbol :tag "Major Mode Symbol or default")
                       (number :tag "Geometric Mean Interval")
                       (number :tag "Number of observations")))
  :group 'auto-indent)

(defun auto-indent-add-to-alist (alist-var elt-cons &optional no-replace)
  "Add to the value of ALIST-VAR an element ELT-CONS if it isn't there yet.
If an element with the same car as the car of ELT-CONS is already present,
replace it with ELT-CONS unless NO-REPLACE is non-nil; if a matching
element is not already present, add ELT-CONS to the front of the alist.
The test for presence of the car of ELT-CONS is done with `equal'."
  (let (
        (case-fold-search 't)
        (existing-element (assoc (car elt-cons) (symbol-value alist-var))))
    (if existing-element
        (or no-replace
            (rplacd existing-element (cdr elt-cons)))
      (set alist-var (cons elt-cons (symbol-value alist-var))))))

(defun auto-indent-save-par-region-interval ()
  "Saves `auto-indent-next-pair-timer-geo-mean'."
  (customize-save-variable 'auto-indent-next-pair-timer-geo-mean auto-indent-next-pair-timer-geo-mean))

(add-hook 'kill-emacs-hook 'auto-indent-save-par-region-interval)

(defun auto-indent-par-region-interval-update (interval)
  "Updates `auto-indent-next-pair-timer-geo-mean'"
  (let ((nlines (- (line-number-at-pos auto-indent-pairs-end)
                   (line-number-at-pos auto-indent-pairs-begin)))
        n i oi (iv interval))
    (setq i (assoc major-mode auto-indent-next-pair-timer-geo-mean))
    (unless i
      (setq i (assoc 'default auto-indent-next-pair-timer-geo-mean)))
    (setq n (nth 2 i))
    (setq i (nth 1 i))
    (if (isnan i)
	(progn
	  (setq i 0.00005)
	  (setq n 0)))
    (condition-case err
	(setq i (max (nth 1 i) 0.00005))
      (error (setq i 0.00005)))
    (condition-case err
	(setq iv (max (/ interval nlines) 0.00005))
      (error (setq iv 0.00005)))
    (if (= n 0)
	(progn
	  (setq i iv)
	  (setq n 1))
      (setq oi i)
      ;; Calculate Geometric Mean
      (if (= iv 0.00005)
	  (setq n (- n 1))
	(condition-case err
	    (progn
	      (setq i (exp (/ (+ (* (log i) n) (log iv)) (+ n 1))))
	      (when (save-match-data (string-match "\\(INF\\|NaN\\)$" (format "%s" i)))
		(setq i oi)
		(setq n (- n 1))
		(when (save-match-data (string-match "\\(INF\\|NaN\\)$" (format "%s" i)))
		  (setq i 0.00005)
		  (setq n -1))))
	  (error
	   (setq i oi)
	   (setq n (- n 1)))))
      (setq n (+ n 1)))
    
    (auto-indent-add-to-alist 'auto-indent-next-pair-timer-geo-mean `(,major-mode ,i ,n))))

(defun auto-indent-par-region-interval (&optional interval div)
  "Gets the interval based on `auto-indent-next-pair-timer-geo-mean'."
  (let ((nlines (- (line-number-at-pos auto-indent-pairs-end) 
                   (line-number-at-pos auto-indent-pairs-begin)))
        n i)
    (setq i (assoc major-mode auto-indent-next-pair-timer-geo-mean))
    (unless i
      (setq i (assoc 'default auto-indent-next-pair-timer-geo-mean)))
    (setq i (nth 1 i))
    (when (save-match-data (string-match "\\(INF\\|NaN\\)" (format "%s" i)))
      (setq i 0.0005))
    (if (isnan i)
	(setq i 0.00005)
      (condition-case err
          (setq i (* i nlines))
        (error
         (setq i 0.0005))))
    (symbol-value 'i)))

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
  "* Automatically remove trailing whitespace when visiting  file."
  :type 'boolean
  :group 'auto-indent)

(defcustom auto-indent-untabify-on-save-file t
  "* Change tabs to spaces on file-save."
  :type 'boolean
  :group 'auto-indent)

(defcustom auto-indent-delete-trailing-whitespace-on-save-file nil
  "* When saving file delete trailing whitespace."
  :type 'boolean
  :group 'auto-indent)

(defcustom auto-indent-on-visit-pretend-nothing-changed t
  "* When modifying the file on visit, pretend nothing changed."
  :type 'boolean
  :group 'auto-indent)


(defcustom auto-indent-delete-line-char-add-extra-spaces t
  "* When deleting a return, add a space (when appropriate)
between the newly joined lines.

This takes care of the condition when deleting text

Lorem ipsum dolor sit|
amet, consectetur adipiscing elit.  Morbi id

Lorem ipsum dolor sit|amet, consectetur adipiscing elit.  Morbi id

Which ideally should be deleted to:

Lorem ipsum dolor sit| amet, consectetur adipiscing elit.  Morbi id

This is controlled by the regular expressions in
`auto-indent-delete-line-char-add-extra-spaces-prog-mode-regs'
and
`auto-indent-delete-line-char-add-extra-spaces-text-mode-regs'"
  :type 'boolean
  :group 'auto-indent)

(defcustom auto-indent-delete-line-char-add-extra-spaces-prog-mode-regs
  '(("\\(\\s.\\|\\sw\\)" "\\(\\sw\\|\\s.\\)"))
  "* Regular expressions for use with `auto-indent-delete-line-char-add-extra-spaces'.  This is used for programming modes as determined by `auto-indent-is-prog-mode-p'."
  :type '(repeat
          (list (regexp :tag "Characters Before Match")
                (regexp :tag "Characters After Match")))
  :group 'auto-indent)

(defcustom auto-indent-delete-line-char-add-extra-spaces-text-mode-regs
  '(("\\(\\s.\\|\\sw\\)" "\\(\\sw\\|\\s.\\)"))
  "* Regular expressions for use with `auto-indent-delete-line-char-add-extra-spaces'.  This is used for programming modes as determined by `auto-indent-is-prog-mode-p'."
  :type '(repeat
          (list (regexp :tag "Characters Before Match")
                (regexp :tag "Characters After Match")))
  :group 'auto-indent)

(defcustom auto-indent-delete-line-char-remove-extra-spaces t
  "When deleting a return, delete any extra spaces between the newly joined lines."
  :type 'boolean
  :group 'auto-indent)

(defcustom auto-indent-delete-line-char-remove-last-space t
  "Remove last space when deleting a line.

When `auto-indent-delete-line-char-remove-extra-spaces' is enabled,
expressions like lists can be removed in a less than optimal
manner.  For example, assuming ``|'' is the cursor:

c(\"Vehicle QD TO\",|
     \"1 ug IVT\",\"3 ug IVT\",...

would be deleted to the following

c(\"Vehicle QD TO\",| \"1 ug IVT\",\"3 ug IVT\",...

In this case it would be preferable to delete to:

c(\"Vehicle QD TO\",|\"1 ug IVT\",\"3 ug IVT\",...

However cases like sentences:

Lorem ipsum dolor sit amet,|
     consectetur adipiscing elit. Morbi id

Deletes to
Lorem ipsum dolor sit amet,| consectetur adipiscing elit. Morbi id

Which is a desired behavior.

When this is enabled, auto-indent attempts to be smarter by
deleting the extra space when characters before and after match
expressions defined in
`auto-indent-delete-line-char-remove-last-space-prog-mode-regs' and
`auto-indent-delete-line-char-remove-last-space-text-mode-regs'."
  :type 'boolean
  :group 'auto-indent)

(defcustom auto-indent-delete-line-char-remove-last-space-prog-mode-regs
  '(("\\(\\s.\\|\\s-\\)" "\\(\\s\"\\|\\sw\\)")
    ("\\s(" "\\(\\s(\\|\\s_\\|\\sw\\)")
    ("\\s)" "\\s)"))
  "* Regular expressions for use with `auto-indent-delete-line-char-remove-last-space'.  This is used for programming modes as determined by `auto-indent-is-prog-mode-p'."
  :type '(repeat
          (list (regexp :tag "Characters Before Match")
                (regexp :tag "Characters After Match")))
  :group 'auto-indent)

(defcustom auto-indent-delete-line-char-remove-last-space-text-mode-regs nil
  "Regular expressions for use with `auto-indent-delete-line-char-remove-last-space'.  This is used for modes other than programming modes.  This is determined by `auto-indent-is-prog-mode-p'."
  :type '(repeat
          (list (regexp :tag "Characters Before Match")
                (regexp :tag "Characters After Match")))
  :group 'auto-indent)


(defcustom auto-indent-kill-remove-extra-spaces nil
  "* Remove indentation before killing the line or region."
  :type 'boolean
  :group 'auto-indent)

(defcustom auto-indent-kill-line-at-eol 'subsequent-whole-lines
  "Determines how a kill at the end of line behaves.

When killing lines, if at the end of a line,

nil - join next line to the current line.  Deletes white-space at
         join.  [this essentially duplicated delete-char]

         See also `auto-indent-kill-remove-extra-spaces'

whole-line - kill next lines

subsequent-whole-lines - merge lines on first call, subsequent kill whole lines

blanks - kill all empty lines after the current line, and then
            any lines specified.

You should also set the function `kill-whole-line' to do what you
want."
  :type '(choice (const :tag "Default" nil)
                 (const :tag "Next whole line" whole-line)
                 (const :tag "merge lines on first call, subsequent kill whole lines" subsequent-whole-line)
                 (const :tag "Next whole line after any blank lines" blanks))
  :group 'auto-indent)

(defcustom auto-indent-kill-line-kill-region-when-active t
  "* When killing lines, if region is active, kill region instead."
  :type 'boolean
  :group 'auto-indent)

(defcustom auto-indent-use-text-boundaries t
  "Use text boundaries when killing lines.

When killing lines, if point is before any text, act as if
point is at BOL.  And if point is after text, act as if point
     is at EOL"
  :type 'boolean
  :group 'auto-indent)

(defcustom auto-indent-minor-mode-symbol t
  "* When true, Auto Indent puts AI on the mode line."
  :type 'boolean
  :group 'auto-indent)

(defcustom auto-indent-disabled-modes-on-save '(ahk-mode)
  "* List of modes where `indent-region' of the whole file is ignored."
  :type '(repeat (sexp :tag "Major mode"))
  :tag " Major modes where linum is disabled: "
  :group 'auto-indent)

(defcustom auto-indent-disabled-modes-list
  `(compilation-mode
    conf-windows-mode
    diff-mode
    inferior-ess-mode
    dired-mode
    eshell-mode
    fundamental-mode
    log-edit-mode
    makefile-gmake-mode
    org-mode
    snippet-mode
    texinfo-mode
    text-mode
    wl-summary-mode
    yaml-mode
    ,(if (boundp 'mmm-mode) 'mmm-mode))
  "List of modes disabled when global `auto-indent-mode' is on."
  :type '(repeat (sexp :tag "Major mode"))
  :tag " Major modes where auto-indent is disabled: "
  :group 'auto-indent)

(defcustom auto-indent-disabled-indent-functions
  '(indent-relative indent-relative-maybe)
  "List of disabled indent functions.

List of functions that auto-indent ignores the `indent-region' on
paste and automated indent by pressing return.  The default is
`indent-relative' and `indent-relative-maybe'.  If these are used the
indentation is may not specified for the current mode."
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

(defcustom auto-indent-backward-delete-char-behavior 'all
  "Backspace behavior when `auto-indent-mode' is enabled.
Based on `backward-delete-char-untabify-method'

Currently, this can be:

- `untabify' -- turn a tab to many spaces, then delete one space;
- `hungry' -- delete all whitespace, both tabs and spaces;
- `all' -- delete all whitespace, including tabs, spaces and newlines;
- nil -- just delete one character."
  :type '(choice (const untabify) (const hungry) (const all) (const nil))
  :group 'auto-indent)

(defcustom auto-indent-key-for-end-of-line-then-newline ""
  "Key for end of line, then newline.

TextMate uses meta return, I believe (M-RET).  If blank, no key
is defined. The key should be in a format used for saving
keyboard macros (see `edmacro-mode'). This is useful when used in
conjunction with something that pairs delimiters like `autopair-mode'."
  :type 'string
  :group 'auto-indent)

(defcustom auto-indent-key-for-end-of-line-insert-char-then-newline ""
  "Key for end of line, `auto-indent-eol-char', then newline.

By default the `auto-indent-eol-char' is the semicolon. TextMate
uses shift-meta return, I believe (S-M-RET). If blank, no key is
defined.  The key should be in a format used for having keyboard
macros (see `edmacro-mode'). This is useful when used in
conjunction with something that pairs delimiters like
`autopair-mode'."
  :type 'string
  :group 'auto-indent)

(defcustom auto-indent-alternate-return-function-for-end-of-line-then-newline nil
  "Defines an alternate return function smart returns.
This allows a different function to take over for the
end-of-line-then newline.  This is useful in `R-mode', where you can
make this send the current line to the R buffer, if you wish."
  :type 'sexp
  :group 'auto-indent)


(defcustom auto-indent-eol-char ";"
  "End of line/statement character, like C or matlab's semi-colon.

Character inserted when
`auto-indent-key-for-end-of-line-inser-char-then-newline' is
defined.  This is a buffer local variable, therefore if you have
a mode that instead of using a semi-colon for an end of
statement, you use a colon, this can be added to the mode as
follows:

     (add-hook 'strange-mode-hook (lambda() (setq auto-indent-eol-char \":\")))

autoThis is similar to Textmate's behavior.  This is useful when used
in conjunction with something that pairs delimiters like `autopair-mode'."
  :type 'string
  :group 'auto-indent)

(defcustom auto-indent-start-org-indent t
  "Starts `org-indent-mode' when in org-mode."
  :type 'boolean
  :group 'auto-indent)

(defcustom auto-indent-fix-org-return t
  "Allows newline and indent behavior in source code blocks in org-mode."
  :type 'boolean
  :group 'auto-indent)

(defcustom auto-indent-fix-org-yank t
  "Allows org-mode yanks to be indented in source code blocks of org-mode."
  :type 'boolean
  :group 'auto-indent)

(defcustom auto-indent-fix-org-auto-fill t
  "Fixes org-based
  auto-fill-function (i.e. `org-auto-fill-function') to only
  auto-fill for things outside of a source block."
  :type 'boolean
  :group 'auto-indent)

(defcustom auto-indent-fix-org-backspace t
  "Fixes `org-backspace' to use `auto-indent-backward-delete-char-behavior' for `org-mode' buffers."
  :type 'boolean
  :group 'auto-indent)

(defcustom auto-indent-fix-org-move-beginning-of-line t
  "Fixes `move-beginning-of-line' in `org-mode' when in source blocks to follow `auto-indent-mode'."
  :type 'boolean
  :group 'auto-indent)

(defcustom auto-indent-force-interactive-advices t
  "Forces interactive advices.

This makes sure that this is called when this is an interactive
call directly to the function.  However, if someone defines
something such as `org-delete-char' to delete a character, when
`org-delete-char' is called interactively and then calls
`delete-char' the advice is never activated (when it should be).
If this is activated, `auto-indent-mode' tries to do the right
thing by guessing what key should have been pressed to get this
event.  If it is the key that was pressed enable the advice."
  
  :type 'boolean :group 'auto-indent)

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

(defcustom auto-indent-known-text-modes
  '(text-mode message-mode fundamental-mode texinfo-mode conf-windows-mode
              LaTeX-mode latex-mode TeX-mode tex-mode outline-mode
              nroww-mode)
  "* List of auto-indent's known text-modes."
  :type '(repeat (sexp :tag "Major mode"))
  :tag "Auto-indent known text modes"
  :group 'auto-indent)

(defcustom auto-indent-assign-indent-level 2
  "Indent level assigned when an indent-level variable is found."
  :type 'integer
  :group 'auto-indent)

(defcustom auto-indent-assign-indent-level-variables t
  "Attempt to assign `auto-indent-known-indent-level-variables' as local variables.
If the major mode has `major-mode-indent-level', `major-indent-level', `major-mode-basic-offset', or
`major-basic-offset' then attempt to set that variable as well."
  :type 'boolean
  :group 'auto-indent)

(defcustom auto-indent-known-indent-level-variables
  '( c-basic-offset lisp-body-indent
                    sgml-basic-offset
                    python-indent)
  "Known indent-level-variables for major modes.  Set locally when auto-indent-mode initializes."
  :type '(repeat (symbol :tag "Variable"))
  :group 'auto-indent)

(make-variable-buffer-local 'auto-indent-eol-char)

(defvar auto-indent-eol-ret-save ""
  "Saved variable for keyboard state.")

(defvar auto-indent-eol-ret-semi-save ""
  "Saved variable for keyboard state.")


(defvar auto-indent-minor-mode-map nil
  "* Auto Indent mode map.")

(defun auto-indent-is-prog-mode-p ()
  "Determines if this mode is a programming mode."
  (let (ret)
    ;; Check to see if flyspell-prog-mode is on.  Then it is a
    ;; programming mode.
    (when (and flyspell-mode
               (boundp 'flyspell-generic-check-word-predicate)
               (eq flyspell-generic-check-word-predicate 'flyspell-generic-progmode-verify))
      (setq ret t))
    (when (and (not ret)
               (not (memq major-mode auto-indent-known-text-modes)))
      (setq ret t))
    (symbol-value 'ret)))

;; Keymap functions for auto-indent-mode.  Replace return with the
;; appropriate command.

(defun auto-indent-setup-map ()
  "* Set up minor mode map."
  (setq auto-indent-minor-mode-map (make-sparse-keymap))
  (unless (string-match "^[ \t]*$" auto-indent-key-for-end-of-line-then-newline)
    (define-key auto-indent-minor-mode-map (read-kbd-macro auto-indent-key-for-end-of-line-then-newline) 'auto-indent-eol-newline))
  (unless (string-match "^[ \t]*$" auto-indent-key-for-end-of-line-insert-char-then-newline)
    (define-key auto-indent-minor-mode-map (read-kbd-macro auto-indent-key-for-end-of-line-insert-char-then-newline) 'auto-indent-eol-char-newline))
  (setq  auto-indent-eol-ret-save auto-indent-key-for-end-of-line-then-newline)
  (setq auto-indent-eol-ret-semi-save auto-indent-key-for-end-of-line-insert-char-then-newline))

(auto-indent-setup-map)

(defun auto-indent-original-binding (key)
  "Gets the original key binding for a specified KEY."
  (or (key-binding key)
      (key-binding (this-single-command-keys))))

;;;###autoload
(defun auto-indent-eol-newline ()
  "Auto-indent function for `end-of-line' and then newline."
  (interactive)
  (end-of-line)
  (if auto-indent-alternate-return-function-for-end-of-line-then-newline
      (call-interactively auto-indent-alternate-return-function-for-end-of-line-then-newline)
    (call-interactively (auto-indent-original-binding (kbd "RET")))))

;;;###autoload
(defun auto-indent-eol-char-newline ()
  "Auto-indent function for `end-of-line', insert `auto-indent-eol-char', and then newline."
  (interactive)
  (end-of-line)
  (unless (looking-back "; *")
    (insert (format "%s" auto-indent-eol-char)))
  (if auto-indent-alternate-return-function-for-end-of-line-then-newline
      (call-interactively auto-indent-alternate-return-function-for-end-of-line-then-newline)
    (call-interactively (auto-indent-original-binding (kbd "RET")))))

;;;###autoload
(defalias 'auto-indent-mode 'auto-indent-minor-mode)

;;;###autoload
(define-minor-mode auto-indent-minor-mode
  "Auto Indent minor mode.

With no argument, this command toggles the mode.
positive prefix argument turns on the mode.
Negative prefix argument turns off the mode.

When auto-indent-minor-mode minor mode is enabled, yanking or pasting automatically indents

Fall back to default, non-indented yanking by preceding the yanking commands with C-u.

Based on auto-indentation posts, slightly redefined to allow it to be a minor mode

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
         ;;
         (when auto-indent-assign-indent-level-variables
           (let* ((mm (symbol-name major-mode))
                  (mm2 mm))
             (when (string-match "-mode" mm2)
               (setq mm2 (replace-match "" nil nil mm2)))
             (mapc
              (lambda(var)
                (set (make-local-variable var)
                     auto-indent-assign-indent-level))
              auto-indent-known-indent-level-variables)
             (cond
              ((intern (format "%s-indent-level" mm))
               (set (make-local-variable (intern (format "%s-indent-level" mm)))
                    auto-indent-assign-indent-level))
              ((intern (format "%s-indent-level" mm2))
               (set (make-local-variable (intern (format "%s-indent-level" mm2)))
                    auto-indent-assign-indent-level))
              ((intern (format "%s-basic-offset" mm2))
               (set (make-local-variable (intern (format "%s-basic-offset" mm2)))
                    auto-indent-assign-indent-level))
              ((intern (format "%s-basic-offset" mm))
               (set (make-local-variable (intern (format "%s-basic-offset" mm)))
                    auto-indent-assign-indent-level)))))
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
           (add-hook 'post-command-hook 'auto-indent-mode-post-command-hook-last t t)
           
           (add-hook 'pre-command-hook 'auto-indent-mode-pre-command-hook nil 't)
           (mapc
            (lambda(ad)
              (when (fboundp ad)
                (condition-case error
                    (progn
                      (ad-enable-advice ad 'around 'auto-indent-minor-mode-advice)
                      (ad-activate ad))
                  (error
                   (message "[auto-indent-mode]: Error enabling after-advices for `auto-indent-mode': %s" (error-message-string error))))))
            '(yank yank-pop))
           (mapc
            (lambda(ad)
              (ad-enable-advice ad 'around 'auto-indent-minor-mode-advice)
              (ad-activate ad))
            '(delete-char kill-line kill-region kill-ring-save cua-copy-region
                          backward-delete-char-untabify backward-delete-char
                          delete-backward-char move-beginning-of-line)))))
        (t
         ;; Kill
         (cond
          ((eq auto-indent-engine 'keys) ;; Auto-indent engine
           )
          (t ;; Default auto-indent-engine setup.
           (remove-hook 'write-contents-hook 'auto-indent-file-when-save)
           (remove-hook 'find-file-hook 'auto-indent-file-when-visit 't)
           (remove-hook 'after-save-hook 'auto-indent-mode-post-command-hook 't)
           (remove-hook 'post-command-hook 'auto-indent-mode-post-command-hook 't)
           (remove-hook 'pre-command-hook 'auto-indent-mode-pre-command-hook 't))))))

(defun auto-indent-deactivate-advices ()
  "Deactivate Advices for `auto-indent-mode'."
  (interactive)
  (mapc
   (lambda(ad)
     (condition-case error
         (progn
           (ad-disable-advice ad 'around 'auto-indent-minor-mode-advice)
           (ad-activate ad))
       (error
        (message "[auto-indent-mode] Error disabling advices: %s" (error-message-string error)))))
   '(yank yank-pop))
  (mapc
   (lambda(ad)
     (when (fboundp ad)
       (ad-disable-advice ad 'around 'auto-indent-minor-mode-advice)
       (ad-activate ad)))
   '(delete-char kill-line kill-region kill-ring-save cua-copy-region
                 backward-delete-char-untabify backward-delete-char
                 delete-backward-char move-beginning-of-line)))


(defun auto-indent-turn-on-org-indent ()
  "Turn on org-indent."
  (when auto-indent-start-org-indent
    (org-indent-mode 1))
  (ad-activate 'yank)
  (ad-activate 'yank-pop))

(defadvice org-return (around auto-indent-mode activate)
  "Fixes org-return to press tab after a newline when `auto-indent-fix-org-return' is true"
  ad-do-it
  (let ((org-src-tab-acts-natively t))
    (when auto-indent-fix-org-return
      (org-src-native-tab-command-maybe))))

(defadvice org-delete-backward-char (around auto-indent-mode activate)
  "Fixes org-backspace to use `auto-indent-delete-backward-char'"
  ad-do-it
  (when (and auto-indent-fix-org-backspace
             (org-babel-where-is-src-block-head))
    (insert " ")
    (auto-indent-delete-backward-char 1)))

(defadvice org-auto-fill-function (around auto-indent-mode activate)
  "Fixes auto-fill for org-mode in source code blocks."
  (let ((do-it t))
    (when (and auto-indent-fix-org-auto-fill
               (eq major-mode 'org-mode)
               (org-babel-where-is-src-block-head))
      (setq do-it nil))
    (when do-it
      ad-do-it)))

(add-hook 'org-mode-hook 'auto-indent-turn-on-org-indent)
;;;###autoload
(defun auto-indent-minor-mode-on ()
  "Turn on auto-indent minor mode."
  (interactive)
  (unless (or (minibufferp)
              (memq major-mode auto-indent-disabled-modes-list))
    (auto-indent-minor-mode 1)))

;;;###autoload
(define-globalized-minor-mode auto-indent-global-mode auto-indent-minor-mode auto-indent-minor-mode-on
  :group 'auto-indent
  :require 'auto-indent-mode)

(defun auto-indent-remove-advice-p (&optional command) 
  "Should the advice be removed?

This is based on either the current command (`this-command') or
the provided COMMAND.  Removes advice if the function called is
actually an auto-indent function OR it should be disabled in this
mode."
  (or (and (memq major-mode auto-indent-disabled-modes-list)) (minibufferp)
      (string-match "^auto-indent" (symbol-name (or command this-command)))))

(defun auto-indent-is-yank-p (&optional command)
  "Test if the `this-command' or COMMAND was a yank."
  (let ((ret
         (or
          (and (boundp 'cua-mode) cua-mode
               (memq (or command this-command)
                     (list
                      'yank
                      (key-binding (kbd "C-c"))
                      (key-binding (kbd "C-v")))))
          (and (boundp 'ergoemacs-mode) ergoemacs-mode
               (memq (or command this-command)
                     (list
                      'yank
                      (key-binding (kbd "M-c"))
                      (key-binding (kbd "M-v")))))
          (memq (or command this-command)
                (list
                 'yank
                 (key-binding (kbd "C-y"))
                 (key-binding (kbd "M-y")))))))
    (symbol-value 'ret)))

(defcustom auto-indent-after-yank-hook nil
  "Hooks to run after auto-indent's yank.  The arguments sent to the function should be the two points in the yank."
  :type 'hook
  :group 'auto-indent)

(defun auto-indent-yank-engine ()
  "Engine for the auto-indent yank functions/advices."
  (when (not (minibufferp))
    (let ((pt (point)))
      (unless (memq indent-line-function auto-indent-disabled-indent-functions)
        (save-excursion
          (skip-chars-backward " \t")
          (when (looking-at "[ \t]+")
            (replace-match " ")))
        (let (p1 p2)
          (save-excursion
            (when (mark t)
              (save-restriction
                (narrow-to-region (progn (goto-char (mark t)) (point-at-bol))
                                  (progn (goto-char pt) (point-at-eol)))
                (condition-case err
                    (run-hook-with-args 'auto-indent-after-yank-hook (point-min) (point-max))
                  (error
                   (message "[Auto-Indent Mode] Ignoring error when running hook `auto-indent-after-yank-hook': %s" (error-message-string err))))
                (setq p1 (point-min))
                (setq p2 (point-max))))
            (if auto-indent-on-yank-or-paste
                (indent-region p1 p2))
            (save-restriction
              (narrow-to-region p1 p2)
              (if auto-indent-mode-untabify-on-yank-or-paste
                  (untabify (point-min) (point-max))))))))))

(defadvice move-beginning-of-line (around auto-indent-minor-mode-advice)
  "`auto-indent-mode' advice for moving to the beginning of the line."
  (let (at-beginning)
    (setq at-beginning (looking-back "^[ \t]*"))
    (when (and at-beginning
               auto-indent-home-is-beginning-of-indent-when-spaces-follow
               (not (looking-at "[ \t]*$"))
               (looking-at "[ \t]"))
      (setq at-beginning nil))
    ad-do-it
    (if (and auto-indent-fix-org-move-beginning-of-line
             (not at-beginning)
             (eq major-mode 'org-mode)
             (org-babel-where-is-src-block-head))
        (progn
          (org-babel-do-in-edit-buffer
           (indent-according-to-mode))
          (when (not (looking-back "^[ \t]*"))
            (beginning-of-line)
            (skip-chars-forward " \t"))))
    (when (and auto-indent-home-is-beginning-of-indent
               auto-indent-minor-mode
               (not at-beginning)
               (not (memq indent-line-function auto-indent-disabled-indent-functions))
               (or (not auto-indent-force-interactive-advices)
                   (called-interactively-p 'any))
               (not (auto-indent-remove-advice-p))
               (not current-prefix-arg))
      (indent-according-to-mode)
      (when (not (looking-back "^[ \t]*"))
        (beginning-of-line)
        (skip-chars-forward " \t")))))

(defmacro auto-indent-advice-command (command)
  "Define advices and functions for yank and `yank-pop'.

yank or `yank-pop' is defined in the COMMAND argument."
  `(progn
     (defadvice ,command (around auto-indent-minor-mode-advice)
       (let ((do-it t))
         (when (and auto-indent-fix-org-yank
                    (eq major-mode 'org-mode)
                    (org-babel-where-is-src-block-head))
           (setq do-it nil)
           (let ((org-src-strip-leading-and-trailing-blank-lines nil))
             (org-babel-do-in-edit-buffer
              (,command (ad-get-arg 0)))))
         (when do-it
           ad-do-it
           (when (and (or (not auto-indent-force-interactive-advices)
                          (called-interactively-p 'any)
                          (auto-indent-is-yank-p))
                      (not (auto-indent-remove-advice-p))
                      (not current-prefix-arg)
                      (or auto-indent-minor-mode
                          (and auto-indent-fix-org-yank)))
             
             (auto-indent-yank-engine)))))
     (defun ,(intern (concat "auto-indent-" (symbol-name command))) (&optional prefix)
       ,(concat "Auto-indent-mode `" (symbol-name command) "' function replacement.  Instead of using advices you can change keybindings to this function.")
       (interactive ,(if (eq command 'yank) "*P" "*p"))
       (,command prefix)
       (when (not prefix)
         (auto-indent-yank-engine)))))

(auto-indent-advice-command yank)
(auto-indent-advice-command yank-pop)

(defun auto-indent-whole-buffer (&optional save)
  "Auto-indent whole buffer and untabify it.

If SAVE is specified, save the buffer after indenting the entire
buffer."
  (interactive)
  (unless (or (minibufferp)
              (memq major-mode auto-indent-disabled-modes-list)
              (and save (memq major-mode auto-indent-disabled-modes-on-save)))
    (when (or
           (and save auto-indent-delete-trailing-whitespace-on-save-file)
           (and (not save) auto-indent-delete-trailing-whitespace-on-visit-file))
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
  "Auto-indent file when save."
  (if (not (minibufferp))
      (if (and auto-indent-minor-mode (buffer-file-name) auto-indent-on-save-file)
          (auto-indent-whole-buffer 't))))

(defun auto-indent-file-when-visit ()
  "auto-indent file when visit."
  (save-excursion
    (when (buffer-file-name)
      (auto-indent-whole-buffer)
      (when auto-indent-on-visit-pretend-nothing-changed
        (set-buffer-modified-p nil) ; Make the buffer appear "not modified"
        ))))

(defun auto-indent-is-bs-key-p (&optional command)
  "Determines if the backspace key was `this-command' or COMMAND."
  (or
   (and (boundp 'viper-mode) viper-mode (eq viper-current-state 'vi-state) ;; Viper VI state
        nil)
   (and (boundp 'ergoemacs-mode) ergoemacs-mode ;; Ergoemacs
        (memq (or command this-command)
              (list
               'autopair-backspace
               'auto-indent-delete-backward-char
               'delete-backward-char
               'backward-delete-char
               (key-binding (kbd "DEL"))
               (and (boundp 'ergoemacs-delete-backward-char-key)
                    ergoemacs-delete-backward-char-key)
               (key-binding (kbd "<backspace>")))))
   (memq (or command this-command)
         (list
          'autopair-backspace
          'auto-indent-delete-backward-char
          'delete-backward-char
          'backward-delete-char
          (key-binding (kbd "DEL"))
          (key-binding (kbd "<backspace>"))))))



(defmacro auto-indent-def-del-char (command &optional function)
  "Defines advices and commands for `delete-char'.

COMMAND defines which command is being advised or an alternate
function is being constructed.

When FUNCTION is non-nil, define an alternate function instead of an advice."
  (let ((do-it (if function
                   `(if (called-interactively-p 'any)
                        (,command  n (if n t nil))
                      (,command n killflag))
                 'ad-do-it)))
    `(,(if function 'defun 'defadvice)
      ,(if function (intern (concat "auto-indent-" (symbol-name command))) command)
      ,(if function '(n &optional killflag) '(around auto-indent-minor-mode-advice))
      "If at the end of the line, take out whitespace after deleting character"
      ,(if function '(interactive "p") nil)
      (if (not ,(if function t '(and
                                 (not (auto-indent-remove-advice-p))
                                 (or (not auto-indent-force-interactive-advices)
                                     (called-interactively-p 'any)
                                     (auto-indent-is-bs-key-p))))) ,do-it
        (let ((backward-delete-char-untabify-method auto-indent-backward-delete-char-behavior))
          (when auto-indent-par-region-timer
            (cancel-timer auto-indent-par-region-timer))
          (setq this-command 'auto-indent-delete-backward-char) ;; No recursive calls, please.
          ,(if (eq command 'backward-delete-char-untabify)
               do-it
             `(backward-delete-char-untabify
               ,@(if function '(n (if (called-interactively-p 'any) t killflag))
                   '((ad-get-arg 0) t)))))))))

(auto-indent-def-del-char backward-delete-char-untabify)
(auto-indent-def-del-char backward-delete-char-untabify t)

(auto-indent-def-del-char backward-delete-char)
(auto-indent-def-del-char backward-delete-char t)

(auto-indent-def-del-char delete-backward-char)
(auto-indent-def-del-char delete-backward-char t)

(defun auto-indent-is-del-key-p (&optional command)
  "Determines if the delete key was `this-command' or COMMAND.

This is based on standards for Viper, ErgoEmacs and standard Emacs"
  (or
   (and (boundp 'viper-mode) viper-mode (eq viper-current-state 'vi-state) ;; Viper VI state
        (memq (or command this-command)
              (list
               'delete-char
               (key-binding (kbd "d")))))
   (and (boundp 'ergoemacs-mode) ergoemacs-mode ;; Ergoemacs
        (memq (or command this-command)
              (list
               'delete-char
               (key-binding (kbd "<delete>"))
               (and (boundp 'ergoemacs-delete-char-key) ergoemacs-delete-char-key)
               (key-binding (kbd "<deletechar>")))))
   (memq (or command this-command)
         (list
          'delete-char
          (key-binding (kbd "<deletechar>"))
          (key-binding (kbd "C-d"))))))

(defun auto-indent-handle-end-of-line (lst &optional add)
  "Handle end of line operations.

LST is the list of regular expressions to consider.

ADD lets `auto-indent-mode' know that it should add a space instead."
  (save-match-data
    (if (or (not add)
            (and add (looking-back "\\S-")
                 (looking-at "\\S-")))
        (let (done)
          (unless add
            (save-excursion
              (skip-chars-backward "\\s-")
              (when (looking-at "\\s-+")
                (replace-match " "))))
          (when auto-indent-delete-line-char-remove-last-space
            (when lst
              (setq done nil)
              (mapc (lambda(i)
                      (when (and (not done) (looking-back (nth 0 i))
                                 (looking-at (concat (if add "" " ") (nth 1 i))))
                        (if add
                            (save-excursion
                              (insert " "))
                          (delete-char 1))
                        (setq done t)))
                    lst))))
      (unless add
        (when (and (eolp) (looking-back "[ \t]+" nil t))
          (replace-match ""))))))


(defmacro auto-indent-def-del-forward-char (&optional function)
  "Defines advices and function for `delete-char'.

When FUNCTION is non-nil, this defines `auto-indent-delete-char'"
  (let ((do-it (if function
                   '(if (called-interactively-p 'any)
                        (delete-char n (if n t nil))
                      (delete-char n killflag))
                 'ad-do-it)))
    `(,(if function 'defun 'defadvice)
      ,(if function 'auto-indent-delete-char 'delete-char)
      ,(if function '(n &optional killflag) '(around auto-indent-minor-mode-advice))
      "If at the end of the line, take out whitespace after deleting character"
      ,(if function '(interactive "p") nil)
      (save-match-data
        (if ,(if function t '(and
                              (not (auto-indent-remove-advice-p))
                              (or (not auto-indent-force-interactive-advices)
                                  (called-interactively-p 'any)
                                  (auto-indent-is-del-key-p))))
            (let ((del-eol (eolp))
                  (prog-mode (auto-indent-is-prog-mode-p)))
              ,do-it
              (when (and del-eol
                         auto-indent-minor-mode (not (minibufferp))
                         auto-indent-delete-line-char-remove-extra-spaces)
                (auto-indent-handle-end-of-line
                 (if prog-mode
                     auto-indent-delete-line-char-remove-last-space-prog-mode-regs
                   auto-indent-delete-line-char-remove-last-space-text-mode-regs)))
              (when (and del-eol
                         auto-indent-minor-mode (not (minibufferp))
                         auto-indent-delete-line-char-add-extra-spaces)
                (auto-indent-handle-end-of-line
                 (if prog-mode
                     auto-indent-delete-line-char-add-extra-spaces-prog-mode-regs
                   auto-indent-delete-line-char-add-extra-spaces-text-mode-regs)  t)))
          ,do-it)))))



(auto-indent-def-del-forward-char)
(auto-indent-def-del-forward-char t)


(defun auto-indent-is-kill-line-p (&optional command)
  "Determines if the `kill-line' was either `this-command' or COMMAND.

This is based on standards for Viper, ErgoEmacs and standard
Emacs"
  (or
   (and (boundp 'viper-mode) viper-mode (eq viper-current-state 'vi-state) nil)
   (and (boundp 'ergoemacs-mode) ergoemacs-mode
        (memq (or command this-command)
              (list
               'kill-line
               (key-binding (kbd "<deleteline>"))
               (key-binding (kbd "M-g")))))
   (memq (or command this-command)
         (list
          'kill-line
          (key-binding (kbd "C-k"))
          (key-binding (kbd "<deleteline>"))))))

(defmacro auto-indent-def-kill-line (&optional function)
  "Defines advices and functions for `kill-line'.

When FUNCTION is true, define `auto-indent-kill-line'."
  (let ((do-it (if function
                   '(kill-line arg)
                 'ad-do-it)))
    `(,(if function 'defun 'defadvice)
      ,(if function 'auto-indent-kill-line 'kill-line)
      ,(if function '(&optional arg) '(around auto-indent-minor-mode-advice))
      "Obey `auto-indent-use-text-boundaries'.

If at end of line, obey `auto-indent-kill-line-at-eol'
"
      ,(if function '(interactive "P") nil)
      (if ,(if function t '(and
                            (not (auto-indent-remove-advice-p))
                            (or (not auto-indent-force-interactive-advices)
                                (called-interactively-p 'any)
                                (auto-indent-is-kill-line-p))))
          (let ((can-do-it t)
                (prog-mode (auto-indent-is-prog-mode-p))
                (eolp (auto-indent-eolp))
                (bolp (auto-indent-bolp)))
            (if (and auto-indent-minor-mode
                     (not (minibufferp)))
                (when auto-indent-par-region-timer
                  (cancel-timer auto-indent-par-region-timer))
              (if (and auto-indent-kill-line-kill-region-when-active
                       (use-region-p))
                  (progn
                    (auto-indent-kill-region (region-beginning) (region-end))
                    (setq can-do-it nil))
                (when bolp
                  (move-beginning-of-line 1))
                (when eolp
                  (cond
                   ((eq auto-indent-kill-line-at-eol nil)
                    (when (> (prefix-numeric-value current-prefix-arg) 0)
                      (if (not kill-whole-line)
                          (save-excursion
                            (beginning-of-line)
                            (insert " ")))))
                   ((eq auto-indent-kill-line-at-eol 'subsequent-whole-line)
                    (let (auto-indent-kill-line-at-eol)
                      (if (memq last-command (list 'kill-region this-command))
                          (progn
                            (setq auto-indent-kill-line-at-eol 'whole-line)
                            (kill-line (ad-get-arg 0)))
                        (setq auto-indent-kill-line-at-eol nil)
                        (kill-line (ad-get-arg 0)))))
                   ((memq auto-indent-kill-line-at-eol '(whole-line blanks))
                    (if (> (prefix-numeric-value current-prefix-arg) 0)
                        (save-excursion
                          (delete-region (point) (point-at-eol))
                          (unless (eobp)
                            (forward-line 1)
                            (when (eq auto-indent-kill-line-at-eol 'blanks)
                              (auto-indent-kill-region (point) (+ (point)
                                                                  (skip-chars-forward " \t\n")
                                                                  (skip-chars-backward " \t"))))))))
                   (t
                    (error "Invalid `auto-indent-kill-line-at-eol' setting %s"
                           auto-indent-kill-line-at-eol))))))
            (when can-do-it
              ,do-it)
            (indent-according-to-mode )
            (when (and eolp (eq auto-indent-kill-line-at-eol nil))
              (when (and eolp
                         auto-indent-minor-mode (not (minibufferp))
                         auto-indent-delete-line-char-remove-extra-spaces)
                (auto-indent-handle-end-of-line
                 (if prog-mode
                     auto-indent-delete-line-char-remove-last-space-prog-mode-regs
                   auto-indent-delete-line-char-remove-last-space-text-mode-regs)))
              (when (and eolp
                         auto-indent-minor-mode (not (minibufferp))
                         auto-indent-delete-line-char-add-extra-spaces)
                (auto-indent-handle-end-of-line
                 (if prog-mode
                     auto-indent-delete-line-char-add-extra-spaces-prog-mode-regs
                   auto-indent-delete-line-char-add-extra-spaces-text-mode-regs)  t))))
        ,do-it))))

(auto-indent-def-kill-line t)
(auto-indent-def-kill-line)

(defun auto-indent-is-kill-region-p (&optional command)
  "Determines if the kill region/cut was `this-command' or COMMAND.

This is based on standards for viper, ergoemacs and standard Emacs."
  (or
   (and (boundp 'viper-mode) viper-mode (eq viper-current-state 'vi-state) nil)
   (and (boundp 'ergoemacs-mode) ergoemacs-mode
        (memq (or command this-command)
              (list
               'kill-region
               (key-binding (kbd "M-x")))))
   (and (boundp 'cua-mode) cua-mode
        (memq (or command this-command)
              (list 'kill-region
                    (key-binding (kbd "C-x")))))
   (memq (or command this-command)
         (list
          'kill-region
          (key-binding "C-w")))))

(defun auto-indent-deindent-last-kill ()
  "Strips out indentation in the last kill."
  (save-match-data
    (let ((ck (current-kill 0 t))
          (start 0))
      (while (string-match "^[ \t]+" ck start)
        (setq ck (replace-match "" t t ck))
        (setq start (match-beginning 0)))
      ;; Put the last entry back.
      (kill-new ck t))))

(defmacro auto-indent-def-kill-region (&optional function)
  "Defines advices and functions for `kill-region'.

When FUNCTION is non-nil, defines `auto-indent-kill-region'"
  (let ((do-it (if function
                   '(if (called-interactively-p 'any)
                        (kill-region beg end)
                      (kill-region beg end yank-handler))
                 'ad-do-it)))
    `(,(if function 'defun 'defadvice)
      ,(if function 'auto-indent-kill-region 'kill-region)
      ,(if function '(&optional beg end &optional yank-handler) '(around auto-indent-minor-mode-advice))
      "Kill region advice and function.  Allows the region to delete the beginning white-space if desired."
      ,(if function '(interactive (list (point) (mark))) nil)
      (if (not ,(if function t
                  '(and
                    auto-indent-mode
                    auto-indent-kill-remove-extra-spaces
                    (not (auto-indent-remove-advice-p))
                    (or (not auto-indent-force-interactive-advices)
                        (called-interactively-p 'any)
                        (auto-indent-is-kill-region-p))))) ,do-it
        ,do-it
        ;; Now modify last entry in kill-ring
        (auto-indent-deindent-last-kill)))))

(auto-indent-def-kill-region)
(auto-indent-def-kill-region t)

(defun auto-indent-is-kill-ring-save-p (&optional command)
  "Determines if `kill-ring-save' was called in `this-command' or COMMAND.

This is based on standards for viper, ergoemacs and standard Emacs."
  (or
   (and (boundp 'viper-mode) viper-mode (eq viper-current-state 'vi-state) nil)
   (and (boundp 'ergoemacs-mode) ergoemacs-mode
        (memq (or command this-command)
              (list
               'kill-region
               (key-binding (kbd "M-c")))))
   (and (boundp 'cua-mode) cua-mode
        (memq (or command this-command)
              (list 'kill-region
                    (key-binding (kbd "C-c")))))
   (memq (or command this-command)
         (list
          'kill-region
          (key-binding "M-w")))))

(defmacro auto-indent-def-kill-ring-save (&optional function cua)
  "Defines advices and functions for `kill-region'.

When FUNCTION is non-nil, defines a function

When CUA is non-nil, work with `cua-mode' copy functions."
  (let ((do-it
         (if function
             (if cua
                 '(if (called-interactively-p 'any)
                      (cua-copy-region arg)
                    (cua-copy-region))
               '(if (called-interactively-p 'any)
                    (kill-region beg end)
                  (kill-region beg end yank-handler)))
           'ad-do-it)))
    `(,(if function 'defun 'defadvice)
      ,(if function (if cua 'auto-indent-cua-copy-region 'auto-indent-kill-ring-save)
         (if cua 'cua-copy-region 'kill-ring-save))
      ,(if function (if cua '(&optional arg) '(&optional beg end))
         '(around auto-indent-minor-mode-advice))
      "Kill ring save/cua-copy-region advice and function.
Allows the kill ring save to delete the beginning white-space if desired."
      ,(if function (if cua '(interactive "P") '(interactive (list (point) (mark)))) nil)
      (if (not ,(if function (if cua '(not arg) t)
                  `(and
                    auto-indent-kill-remove-extra-spaces
                    ,(if cua '(not current-prefix-arg) t)
                    (not (auto-indent-remove-advice-p))
                    (or (not auto-indent-force-interactive-advices)
                        (called-interactively-p 'any)
                        (auto-indent-is-kill-ring-save-p))))) ,do-it
        ,do-it
        ;; Now modify last entry in kill-ring
        (auto-indent-deindent-last-kill)))))

(auto-indent-def-kill-ring-save)
(auto-indent-def-kill-ring-save t)
(auto-indent-def-kill-ring-save nil t)
(auto-indent-def-kill-ring-save t t)

(defvar auto-indent-mode-pre-command-hook-line nil)
(make-variable-buffer-local 'auto-indent-mode-pre-command-hook-line)

(defvar auto-indent-last-pre-command-hook-point nil)
(make-variable-buffer-local 'auto-indent-mode-pre-command-hook-point)

(defvar auto-indent-last-pre-command-hook-minibufferp nil)

(defun auto-indent-eolp ()
  "Return t if point is at eol respecting `auto-indent-use-text-boundaries'."
  (if auto-indent-use-text-boundaries
      (looking-at-p "[ \t]*$")
    (eolp)))

(defun auto-indent-bolp ()
  "Return t if point is at bol respecting `auto-indent-use-text-boundaries'."
  (if auto-indent-use-text-boundaries
      (looking-back "^[ \t]*")
    (bolp)))

(defvar auto-indent-pairs-begin nil
  "Defines where the pair region begins.")

(defvar auto-indent-pairs-end nil
  "Defines where the pair region ends.")

(defun auto-indent-mode-pre-command-hook ()
  "Hook for `auto-indent-mode' to tell if the point has been moved."
  (condition-case error
      (progn
        (setq auto-indent-last-pre-command-hook-minibufferp (minibufferp))
        (unless (eq (nth 0 (reverse post-command-hook)) 'auto-indent-mode-post-command-hook-last)
          (when (memq 'auto-indent-mode-post-command-hook-last post-command-hook)
            (remove-hook 'post-command-hook 'auto-indent-mode-post-command-hook-last t))
          (add-hook 'post-command-hook 'auto-indent-mode-post-command-hook-last t t))
        (unless (eq (nth 0 post-command-hook) 'auto-indent-mode-post-command-hook)
          (when (memq 'auto-indent-mode-post-command-hook post-command-hook)
            (remove-hook 'post-command-hook 'auto-indent-mode-post-command-hook t))
          (add-hook 'post-command-hook 'auto-indent-mode-post-command-hook nil t))
        (when (and (not (minibufferp)))
          (setq auto-indent-mode-pre-command-hook-line (line-number-at-pos))
          (setq auto-indent-last-pre-command-hook-point (point))
          (let ((mark-active mark-active))
            (when (and auto-indent-current-pairs
                       auto-indent-pairs-begin
                       auto-indent-pairs-end)
              (setq auto-indent-pairs-begin (min auto-indent-pairs-begin
                                                 (point)))
              (setq auto-indent-pairs-end (max auto-indent-pairs-end
                                               (point))))
            (when (and auto-indent-current-pairs
                       (not auto-indent-pairs-begin)
                       (auto-indent-point-inside-pairs-p))
              (setq auto-indent-pairs-begin (point))
              (setq auto-indent-pairs-end (point))
              (set (make-local-variable 'auto-indent-pairs-begin)
                   (min auto-indent-pairs-begin
                        (save-excursion
                          (goto-char (nth 1 (syntax-ppss)))
                          (point))))
              (set (make-local-variable 'auto-indent-pairs-end)
                   (max auto-indent-pairs-end
                        (save-excursion
                          (goto-char auto-indent-pairs-begin)
                          (condition-case err
                              (forward-list)
                            (error nil))
                          (point))))))))
    (error
     (message "[Auto-Indent Mode] Ignoring Error in `auto-indent-mode-pre-command-hook': %s" (error-message-string error)))))

(defun auto-indent-point-inside-pairs-p ()
  "Is point inside any pairs?"
  (condition-case err
      (> (car (syntax-ppss)) 0)
    (error nil)))

(defvar auto-indent-par-region-timer nil)

(defun auto-indent-par-region ()
  "Indent a parenthetical region (based on a timer)."
  (when (or (not (or (fboundp 'yas--snippets-at-point)
                     (fboundp 'yas/snippets-at-point)))
            (or (and (boundp 'yas/minor-mode) (not yas/minor-mode))
                (and (boundp 'yas-minor-mode) (not yas-minor-mode)))
            (and (or yas/minor-mode yas-minor-mode)
                 (let ((yap (if (fboundp 'yas/snippets-at-point)
                                (yas/snippets-at-point 'all-snippets)
                              (yas--snippets-at-point 'all-snippets))))
                   (or (not yap) (and yap (= 0 (length yap)))))))
    (when auto-indent-next-pair
      (let ((mark-active mark-active))
        (when (and (not (minibufferp))
                   (not (looking-at "[^ \t]"))
                   (not (looking-back "^[ \t]+")))
          (let ((start-time (float-time)))
            (indent-region auto-indent-pairs-begin auto-indent-pairs-end)
            (auto-indent-par-region-interval-update (- (float-time) start-time)))
          (when (or (> (point) auto-indent-pairs-end)
                    (< (point) auto-indent-pairs-begin))
            (set (make-local-variable 'auto-indent-pairs-begin) nil)
            (set (make-local-variable 'auto-indent-pairs-end) nil)))))))

(defun auto-indent-mode-post-command-hook-last ()
  "Last `post-command-hook' run.

Last hook run to take care of auto-indenting that needs to be
performed after all other post-command hooks have run (like sexp
auto-indenting)"
  (condition-case err
      (when (and (not auto-indent-last-pre-command-hook-minibufferp)
                 (not (minibufferp))
                 (not (memq indent-line-function auto-indent-disabled-indent-functions)))
        
        (unless (memq 'auto-indent-mode-pre-command-hook pre-command-hook)
          (setq auto-indent-mode-pre-command-hook-line -1)
          (add-hook 'pre-command-hook 'auto-indent-mode-pre-command-hook nil t))
        (when auto-indent-minor-mode
          (when auto-indent-next-pair
            (if auto-indent-pairs-begin
                (progn
                  (setq auto-indent-pairs-begin (min (point)
                                                     auto-indent-pairs-begin))
                  (setq auto-indent-pairs-end (max (point)
                                                   auto-indent-pairs-end)))
              (setq auto-indent-pairs-begin (point))
              (setq auto-indent-pairs-end (point))
              (set (make-local-variable 'auto-indent-pairs-begin)
                   (min auto-indent-pairs-begin (point))) 
              (set (make-local-variable 'auto-indent-pairs-end)
                   (max auto-indent-pairs-end (point))))
            (if auto-indent-par-region-timer
                (cancel-timer auto-indent-par-region-timer))
            (set (make-local-variable 'auto-indent-par-region-timer)
                 (run-with-timer (auto-indent-par-region-interval) nil
                                 'auto-indent-par-region)))
          (when (and auto-indent-current-pairs
                     auto-indent-pairs-begin)
            (setq auto-indent-pairs-begin (min (point)
                                               auto-indent-pairs-begin))
            (setq auto-indent-pairs-end (max (point) 
                                             auto-indent-pairs-end))
            (if auto-indent-par-region-timer
                (cancel-timer auto-indent-par-region-timer))
            (set (make-local-variable 'auto-indent-par-region-timer)
                 (run-with-timer (auto-indent-par-region-interval) nil
                                 'auto-indent-par-region)))
          (when (and auto-indent-current-pairs
                     (not auto-indent-pairs-begin)
                     (auto-indent-point-inside-pairs-p))
            (let* ((mark-active mark-active)
                   (p1 (save-excursion
                         (nth 1 (syntax-ppss))
                         (point)))
                   (p2 (save-excursion
                         (goto-char p1)
                         (condition-case err
                             (forward-list)
                           (error nil))
                         (point))))
              (set (make-local-variable 'auto-indent-pairs-begin)
                   (min p1 (or auto-indent-pairs-begin (point))))
              (set (make-local-variable 'auto-indent-pairs-end)
                   (min p2 (or auto-indent-pairs-end (point))))
              (if auto-indent-par-region-timer
                  (cancel-timer auto-indent-par-region-timer))
              (set (make-local-variable 'auto-indent-par-region-timer)
                   (run-with-timer (auto-indent-par-region-interval) nil
                                   'auto-indent-par-region))))))
    (error (message "[Auto-Indent-Mode]: Ignored indentation error in `auto-indent-mode-post-command-hook-last' %s" (error-message-string err)))))

(defun auto-indent-mode-post-command-hook ()
  "Post-command hook for `auto-indent-mode'.

Allows auto-indent-mode to go to the right place when moving
around and the whitespace was deleted from the line."
  (condition-case err
      (when (and (not auto-indent-last-pre-command-hook-minibufferp) (not (minibufferp))
                 (not (memq indent-line-function auto-indent-disabled-indent-functions)))
        
        (unless (memq 'auto-indent-mode-pre-command-hook pre-command-hook)
          (setq auto-indent-mode-pre-command-hook-line -1)
          (add-hook 'pre-command-hook 'auto-indent-mode-pre-command-hook nil t))
        (when auto-indent-minor-mode
          (cond
           ((and last-command-event (memq last-command-event '(10 13 return)))
            (when (or (not (or (fboundp 'yas--snippets-at-point)
                               (fboundp 'yas/snippets-at-point)))
                      (or (and (boundp 'yas/minor-mode) (not yas/minor-mode))
                          (and (boundp 'yas-minor-mode) (not yas-minor-mode)))
                      (and (or yas/minor-mode yas-minor-mode)
                           (let ((yap (if (fboundp 'yas/snippets-at-point)
                                          (yas/snippets-at-point 'all-snippets)
                                        (yas--snippets-at-point 'all-snippets))))
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
    (error (message "[Auto-Indent-Mode]: Ignored indentation error in `auto-indent-mode-post-command-hook' %s" (error-message-string err)))))

(defvar auto-indent-was-on nil)

(provide 'auto-indent-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; auto-indent-mode.el ends here
