;;; so-long.el --- Say farewell to performance problems with minified code.
;;
;; Author: Phil S.
;; URL: https://www.emacswiki.org/emacs/SoLong
;; Keywords: convenience
;; Created: 23 Dec 2015
;; Package-Requires: ((emacs "24.3"))
;; Version: 0.7.6

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; When the lines in a buffer are so long that performance could suffer to an
;; unacceptable degree, we say "so long" to the buffer's major mode, and invoke
;; something much more basic in its place.
;;
;; Many Emacs modes struggle with buffers which contain excessively long lines.
;; This is commonly on account of 'minified' code (i.e. code that has been
;; compacted into the smallest file size possible, which often entails removing
;; newlines should they not be strictly necessary).  Most programming modes
;; simply aren't optimised (remotely) for this scenario, and so performance can
;; suffer significantly.
;;
;; When such files are detected, we invoke `so-long-mode' in place of the mode
;; that Emacs selected.  This is almost identical to `fundamental-mode', and
;; so provides optimal performance in the buffer.  In addition, we explicitly
;; disable a (configurable) list of minor modes with performance implications.
;;
;; These kinds of minified files are typically not intended to be edited; so
;; not providing the usual editing mode in such cases will rarely be an issue.
;; However, should the user wish to do so, the original mode may be reinstated
;; easily in any given buffer using `so-long-mode-revert' (the key binding for
;; which is advertised when the mode is entered).

;; Installation
;; ------------
;; (when (require 'so-long nil :noerror)
;;   (so-long-enable))

;; Configuration
;; -------------
;; Use M-x customize-group RET so-long RET
;;
;; The variables `so-long-target-modes', `so-long-threshold',
;; `so-long-max-lines', and `so-long-mode-enabled' determine whether this mode
;; will be invoked for a given buffer.  The tests are made after `set-auto-mode'
;; has set the normal major mode.

;; Inhibiting and disabling minor modes
;; ------------------------------------
;; The simple way to disable most buffer-local minor modes is to add the mode
;; symbol to the `so-long-minor-modes' list.  Several modes are targeted by
;; default, and it is a good idea to customize this variable to add any
;; additional buffer-local minor modes that you use which you know to have
;; performance implications. For example:
;;
;; (when (require 'so-long nil :noerror)
;;   (mapc (apply-partially 'add-to-list 'so-long-minor-modes)
;;         '(hl-sexp-mode diff-hl-mode diff-hl-amend-mode diff-hl-flydiff-mode
;;                        idle-highlight-mode rainbow-delimiters-mode))
;;   (so-long-enable))
;;
;; In the case of globalized minor modes, be sure to specify the buffer-local
;; minor mode, and not the global mode which controls it.
;;
;; Note that `so-long-minor-modes' is not useful for other global minor modes
;; (as distinguished from globalized minor modes), but in some cases it will be
;; possible to inhibit or otherwise counter-act the behaviour of a global mode
;; using the following hooks.  You would need to inspect the code for a given
;; global mode (on a case by case basis) to determine whether it's possible to
;; inhibit it for a single buffer, and if so how best to do that, as not all
;; modes are alike.

;; Hooks
;; -----
;; Two custom hooks are available for custom behaviours.
;;
;; `so-long-mode-hook' is the standard major mode hook, which runs between
;; `change-major-mode-after-body-hook' and `after-change-major-mode-hook'.
;;
;; `so-long-hook' runs during `after-change-major-mode-hook', but after
;; globalized minor modes have acted (because we call `add-hook' with the
;; APPEND argument, and globalized modes do not).  This hook runs immediately
;; after `so-long-minor-modes' has been processed.

;; Implementation notes
;; --------------------
;; This library advises `hack-local-variables' (in order that we may inhibit our
;; functionality when a file-local mode is set), and `set-auto-mode' (in order
;; to react after Emacs has chosen the major mode for a buffer).

;;; Change Log:
;;
;; 0.7.6 - Bug fix for `so-long-mode-hook' losing its default value.
;; 0.7.5 - Documentation.
;;       - Added sgml-mode and nxml-mode to `so-long-target-modes'.
;; 0.7.4 - Refactored the handling of `whitespace-mode'.
;; 0.7.3 - Added customize group `so-long' with user options.
;;       - Added `so-long-original-values' to generalise the storage and
;;         restoration of values from the original mode upon `so-long-revert'.
;;       - Added `so-long-revert-hook'.
;; 0.7.2 - Remember the original major mode even with M-x `so-long-mode'.
;; 0.7.1 - Clarified interaction with globalized minor modes.
;; 0.7   - Handle header 'mode' declarations.
;;       - Hack local variables after reverting to the original major mode.
;;       - Reverted `so-long-max-lines' to a default value of 5.
;; 0.6.5 - Inhibit globalized `hl-line-mode' and `whitespace-mode'.
;;       - Set `buffer-read-only' by default.
;; 0.6   - Added `so-long-minor-modes' and `so-long-hook'.
;; 0.5   - Renamed library to "so-long.el".
;;       - Added explicit `so-long-enable' command to activate our advice.
;; 0.4   - Amended/documented behaviour with file-local 'mode' variables.
;; 0.3   - Defer to a file-local 'mode' variable.
;; 0.2   - Initial release to EmacsWiki.
;; 0.1   - Experimental.

;;; Code:

(defgroup so-long nil
  "Prevent unacceptable performance degradation with very long lines."
  :prefix "so-long"
  :group 'convenience)

(defcustom so-long-threshold 250
  "Maximum line length permitted before invoking `so-long-mode'.

See `so-long-line-detected-p' for details."
  :type 'integer
  :group 'so-long)

(defcustom so-long-max-lines 5
  "Number of non-blank, non-comment lines to test for excessive length.

See `so-long-line-detected-p' for details."
  :type 'integer
  :group 'so-long)

(defcustom so-long-target-modes
  '(prog-mode css-mode sgml-mode nxml-mode)
  "`so-long-mode' affects only these modes and their derivatives.

Our primary use-case is minified programming code, so `prog-mode' covers
most cases, but there are some exceptions to this."
  :type '(repeat symbol) ;; not function, as may be unknown => mismatch.
  :group 'so-long)

(defcustom so-long-minor-modes
  '(font-lock-mode
    highlight-changes-mode hi-lock-mode hl-line-mode linum-mode nlinum-mode
    prettify-symbols-mode visual-line-mode whitespace-mode)
  ;; It's not clear to me whether all of these would be problematic, but they
  ;; seemed like reasonable targets.  Some are certainly excessive in smaller
  ;; buffers of minified code, but we should be aiming to maximise performance
  ;; by default, so that Emacs is as responsive as we can manage in even very
  ;; large buffers of minified code.
  "List of buffer-local minor modes to explicitly disable in `so-long-mode'.

The modes are disabled by calling them with a single numeric argument of zero.

This happens during `after-change-major-mode-hook', and after any globalized
minor modes have acted, so that buffer-local modes controlled by globalized
modes can also be targeted.

`so-long-hook' can be used where more custom behaviour is desired.

See also `so-long-mode-hook'."
  :type '(repeat symbol) ;; not function, as may be unknown => mismatch.
  :group 'so-long)

(defcustom so-long-hook '(so-long-make-buffer-read-only) ;; n.b. do this last.
  "List of functions to call after `so-long-mode'.

This hook runs during `after-change-major-mode-hook', and after any globalized
minor modes have acted.

See also `so-long-mode-hook' and `so-long-minor-modes'."
  :type '(repeat function)
  :group 'so-long)

(defcustom so-long-revert-hook '(so-long-revert-buffer-read-only)
  "List of functions to call after `so-long-mode-revert'."
  :type '(repeat function)
  :group 'so-long)

(defvar so-long-mode-enabled t
  "Set to nil to prevent `so-long-mode' from being triggered.")

(defvar so-long-mode--inhibited nil) ; internal use
(make-variable-buffer-local 'so-long-mode--inhibited)
(put 'so-long-mode--inhibited 'permanent-local t)

(defvar-local so-long-original-values nil
  "Alist holding the buffer's original `major-mode' value, and other data.

Any values to be restored by `so-long-revert' can be stored here during
`change-major-mode-hook' and reinstated during `so-long-revert-hook'.

See also `so-long-remember' and `so-long-original'.")
(put 'so-long-original-values 'permanent-local t)

(defun so-long-original (key &optional exists)
  "Return the current value for KEY in `so-long-original-values'.

If you need to differentiate between a stored value of nil and no stored value
at all, make EXISTS non-nil. This then returns the result of `assq' directly:
nil if no value was set, and a cons cell otherwise."
  (if exists
      (assq key so-long-original-values)
    (cdr (assq key so-long-original-values))))

(defun so-long-remember (variable)
  "Push the `symbol-value' for VARIABLE to `so-long-original-values'."
  (push (cons variable (symbol-value variable))
        so-long-original-values))

(add-hook 'change-major-mode-hook 'so-long-change-major-mode)

(defun so-long-change-major-mode ()
  "Ensures that `so-long-mode' knows the original `major-mode'
even when invoked interactively.

Called by default during `change-major-mode-hook'."
  (unless (eq major-mode 'so-long-mode)
    (so-long-remember 'major-mode)
    (so-long-remember 'buffer-read-only)))

;; When the line's long
;; When the mode's slow
;; When Emacs is sad
;; We change automatically to faster code
;; And then I won't feel so mad

(defun so-long-line-detected-p ()
  "Following any initial comments and blank lines, the next N lines of the
buffer will be tested for excessive length (where \"excessive\" means above
`so-long-threshold', and N is `so-long-max-lines').

Returns non-nil if any such excessive-length line is detected."
  (let ((count 0))
    (save-excursion
      (goto-char (point-min))
      (while (comment-forward)) ;; clears whitespace at minimum
      (catch 'excessive
        (while (< count so-long-max-lines)
          (if (> (- (line-end-position 1) (point))
                 so-long-threshold)
              (throw 'excessive t)
            (forward-line)
            (setq count (1+ count))))))))

(defcustom so-long-mode-hook '(so-long-inhibit-global-hl-line-mode)
  ;; This user option must be defined prior to `so-long-mode' to
  ;; prevent `define-derived-mode' setting its value to nil; however
  ;; the mode definition will clobber our docstring, so we will set
  ;; that after the mode has been defined.
  ""
  :type '(repeat function)
  :group 'so-long)

(define-derived-mode so-long-mode nil "So long"
  "This mode is used if line lengths exceed `so-long-threshold'.

Many Emacs modes struggle with buffers which contain excessively long lines,
and may consequently cause unacceptable performance issues.

This is commonly on account of 'minified' code (i.e. code has been compacted
into the smallest file size possible, which often entails removing newlines
should they not be strictly necessary). These kinds of files are typically
not intended to be edited, so not providing the usual editing mode in these
cases will rarely be an issue.

When such files are detected, we invoke this mode. This happens after
`set-auto-mode' has set the major mode, should the selected major mode be
a member (or derivative of a member) of `so-long-target-modes'.

After changing modes, any active minor modes listed in `so-long-minor-modes'
are disabled for the current buffer, and finally `so-long-hook' is run.
These two steps occur as part of `after-change-major-mode-hook', so that
modes controlled by globalized minor modes are also visible.

Some globalized minor modes may be inhibited by acting in `so-long-mode-hook'.

By default this mode is essentially equivalent to `fundamental-mode', and
exists mainly to provide information to the user as to why the expected mode
was not used, and to facilitate hooks for other so-long functionality.

To revert to the original mode despite any potential performance issues,
type \\[so-long-mode-revert], or else re-invoke it manually."
  ;; Disable font-lock (circumventing `global-font-lock-mode'),
  ;; and other undesirable functionality.
  (add-hook 'after-change-major-mode-hook
            'so-long-after-change-major-mode :append :local)
  ;; Inform the user about our major mode hijacking.
  (message "Changed to %s (from %s) on account of line length. %s to revert."
           major-mode
           (or (so-long-original 'major-mode) "<unknown>")
           (substitute-command-keys "\\[so-long-mode-revert]")))

;; In order to provide a custom docstring for `so-long-mode-hook', we
;; must set it after `so-long-mode' is defined, as `define-derived-mode'
;; clobbers any existing docstring (see the user option definition for
;; why we define it first).
(put 'so-long-mode-hook 'variable-documentation
     "List of functions to call when `so-long-mode' is invoked.

This is the standard mode hook for `so-long-mode' which runs between
`change-major-mode-after-body-hook' and `after-change-major-mode-hook'.

Note that globalized minor modes have not yet acted.

See also `so-long-hook' and `so-long-minor-modes'.")

(defun so-long-after-change-major-mode ()
  "Disable modes in `so-long-minor-modes' and run `so-long-hook' functions.

This happens during `after-change-major-mode-hook'."
  (mapc (lambda (mode)
          (when (and (boundp mode) mode)
            (funcall mode 0)))
        so-long-minor-modes)
  (run-hooks 'so-long-hook))

(defun so-long-mode-revert ()
  "Call the `major-mode' which was selected before `so-long-mode' replaced it,
and re-process the local variables.  Lastly run `so-long-revert-hook'."
  (interactive)
  (let ((so-long-original-mode (so-long-original 'major-mode)))
    (unless so-long-original-mode
      (error "Original mode unknown."))
    (funcall so-long-original-mode)
    (hack-local-variables)
    (run-hooks 'so-long-revert-hook)))

(define-key so-long-mode-map (kbd "C-c C-c") 'so-long-mode-revert)

(defun so-long-make-buffer-read-only ()
  "Make a so-long buffer read-only.

Called by default as the final action of `so-long-hook', as some earlier
actions may generate warnings when performed in a read-only buffer (e.g.
disabling `highlight-changes-mode').

As such, making the buffer read-only should be the final action taken,
to avoid any potential errors."
  (setq buffer-read-only t))

(defun so-long-revert-buffer-read-only ()
  "Restore `buffer-read-only' to its original value.

Called by default in `so-long-revert-hook'."
  (let ((readonly (so-long-original 'buffer-read-only :exists)))
    (when readonly
      (setq buffer-read-only (cdr readonly)))))

(defun so-long-inhibit-global-hl-line-mode ()
  "Prevent `global-hl-line-mode' from activating.

Called by default during `so-long-mode-hook'."
  (setq-local global-hl-line-mode nil))

(defun so-long-check-header-modes ()
  "Handles the header-comments processing in `set-auto-mode'.

`set-auto-mode' has some special-case code to handle the 'mode' pseudo-variable
when set in the header comment.  This runs outside of `hack-local-variables'
and cannot be conveniently intercepted, so we are forced to replicate it here.

This special-case code will ultimately be removed from Emacs, as it exists to
deal with a deprecated feature; but until then we need to replicate it in order
to inhibit our own behaviour in the presence of a header comment 'mode'
declaration."
  ;; The following code for processing MODE declarations in the header
  ;; comments is copied verbatim from `set-auto-mode', because we have
  ;; no way of intercepting it.
  ;;
  (let ((try-locals (not (inhibit-local-variables-p)))
        end done mode modes)
    ;; Once we drop the deprecated feature where mode: is also allowed to
    ;; specify minor-modes (ie, there can be more than one "mode:"), we can
    ;; remove this section and just let (hack-local-variables t) handle it.
    ;; Find a -*- mode tag.
    (save-excursion
      (goto-char (point-min))
      (skip-chars-forward " \t\n")
      ;; Note by design local-enable-local-variables does not matter here.
      (and enable-local-variables
           try-locals
           (setq end (set-auto-mode-1))
           (if (save-excursion (search-forward ":" end t))
               ;; Find all specifications for the `mode:' variable
               ;; and execute them left to right.
               (while (let ((case-fold-search t))
                        (or (and (looking-at "mode:")
                                 (goto-char (match-end 0)))
                            (re-search-forward "[ \t;]mode:" end t)))
                 (skip-chars-forward " \t")
                 (let ((beg (point)))
                   (if (search-forward ";" end t)
                       (forward-char -1)
                     (goto-char end))
                   (skip-chars-backward " \t")
                   (push (intern (concat (downcase (buffer-substring beg (point))) "-mode"))
                         modes)))
             ;; Simple -*-MODE-*- case.
             (push (intern (concat (downcase (buffer-substring (point) end))
                                   "-mode"))
                   modes))))

    ;; `so-long' now processes the resulting mode list.  If any modes were
    ;; listed, we assume that one of them is a major mode.  It's possible that
    ;; this isn't true, but the buffer would remain in fundamental-mode if that
    ;; were the case, so it is very unlikely.
    (setq so-long-mode--inhibited modes)))

;; How do you solve a problem like a long line?
;; How do you stop a mode from slowing down?
;; How do you cope with processing a long line?
;; A bit of advice! A mode! A workaround!

(defadvice hack-local-variables (after so-long--file-local-mode disable)
  "Ensure that `so-long-mode' defers to file-local mode declarations.

This advice acts after any initial MODE-ONLY call to `hack-local-variables',
and ensures that we do not change to `so-long-mode' in that scenario.

File-local header comments are currently an exception (see the commentary
for details). The file-local mode will ultimately still be used, however
`so-long-mode' still runs first, thus displaying a misleading message.
This issue will eventually be resolved in Emacs."
  (when (ad-get-arg 0) ; MODE-ONLY argument to `hack-local-variables'
    ;; Inhibit `so-long-mode' if a MODE is specified.
    (setq so-long-mode--inhibited ad-return-value)))

(defadvice set-auto-mode (around so-long--set-auto-mode disable)
  "Maybe change to `so-long-mode' for files with very long lines.

This advice acts after `set-auto-mode' has set the buffer's major mode.

We can't act before this point, because some major modes must be exempt
from `so-long-mode' (binary file modes, for example).  Instead, we act
only when the selected major mode is a member (or derivative of a member)
of `so-long-target-modes'.

`so-long-line-detected-p' then determines whether the mode change is needed.

Local variables are not processed after changing to `so-long-mode', as
they might negatively affect performance.  (Local variables are processed
again if `so-long-mode-revert' is called, however.)"
  (setq so-long-mode--inhibited nil) ; is permanent-local
  (when so-long-mode-enabled
    (so-long-check-header-modes)) ; may set `so-long-mode--inhibited'
  ad-do-it ; `set-auto-mode'      ; may set `so-long-mode--inhibited'
  (when so-long-mode-enabled
    (unless so-long-mode--inhibited
      (when (and (apply 'derived-mode-p so-long-target-modes)
                 (so-long-line-detected-p))
        (so-long-mode)))))

;;;###autoload
(defun so-long-enable ()
  "Enable the so-long library's functionality."
  (interactive)
  (ad-enable-advice 'hack-local-variables 'after 'so-long--file-local-mode)
  (ad-enable-advice 'set-auto-mode 'around 'so-long--set-auto-mode)
  (ad-activate 'hack-local-variables)
  (ad-activate 'set-auto-mode)
  (setq so-long-mode-enabled t))

(defun so-long-disable ()
  "Disable the so-long library's functionality."
  (interactive)
  (ad-disable-advice 'hack-local-variables 'after 'so-long--file-local-mode)
  (ad-disable-advice 'set-auto-mode 'around 'so-long--set-auto-mode)
  (ad-activate 'hack-local-variables)
  (ad-activate 'set-auto-mode)
  (setq so-long-mode-enabled nil))

(defun so-long-unload-function ()
  (so-long-disable)
  nil)

(provide 'so-long)

;; So long, farewell, auf wiedersehen, goodbye
;; You have to go, this code is minified
;; Goodbye!

;;; so-long.el ends here
