;;; browse-kill-ring+.el --- Extensions to `browse-kill-ring.el'.
;;
;; Filename: browse-kill-ring+.el
;; Description: Extensions to `browse-kill-ring.el'.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2006-2010, Drew Adams, all rights reserved.
;; Created: Tue May 25 16:35:05 2004
;; Version: 21.0
;; Last-Updated: Fri Jan 15 10:13:41 2010 (-0800)
;;           By: dradams
;;     Update #: 325
;; URL: http://www.emacswiki.org/cgi-bin/wiki/browse-kill-ring+.el
;; Keywords: convenience
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x
;;
;; Features that might be required by this library:
;;
;;   `browse-kill-ring'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Extensions to `browse-kill-ring.el'.
;;
;;  Put this in your init file (~/.emacs):
;;
;;    (require 'browse-kill-ring+)
;;
;;  If you also use library `second-sel.el' (recommended), then
;;  require that library before this one.  If both libraries are used,
;;  then `browse-kill-ring-alternative-ring' is the secondary
;;  selection ring, and `browse-kill-ring' lets you use either that
;;  ring or the `kill-ring' as the selection ring to browse or pop.
;;
;;  You can customize the set of commands to be recognized as yank
;;  commands and alternative yank commands - see options
;;  `browse-kill-ring-yank-commands' and
;;  `browse-kill-ring-alternative-yank-commands'.  The alternative
;;  yank commands are commands that yank using a different selection
;;  ring, for example, the secondary yank commands defined by library
;;  `second-sel.el'.
;;
;;  Following a yank command or alternative yank command, `M-y' pops
;;  the appropriate type of selection.  A prefix arg N chooses the Nth
;;  previous selection in the ring.
;;
;;  Otherwise (not following a yank or alternative yank), `M-y'
;;  browses the current selection ring.  A prefix arg switches to the
;;  other selection ring.  If you are in a `browse-kill-ring' buffer,
;;  then `M-y' switches to the other ring even without a prefix arg.
;;
;;  If you do not use library `second-sel.el', then there is no other
;;  selection ring.  `M-y' either pops (following a yank) or browses
;;  (not following a yank) the `kill-ring'.
;;
;;  FWIW, I customize `browse-kill-ring-quit-action' to be
;;  `browse-kill-ring-quit-deletes-window/frame'.  This is similar to
;;  the default behavior, except that if the window is dedicated, then
;;  the frame is deleted.
;;
;;
;;  Commands defined here:
;;
;;    `toggle-browse-kill-ring-display-style'.
;;
;;  User options defined here:
;;
;;    `browse-kill-ring-alternative-ring',
;;    `browse-kill-ring-alternative-yank-commands',
;;    `browse-kill-ring-yank-commands'.
;;
;;  Non-interactive functions defined here:
;;
;;    `browse-kill-ring-quit-deletes-window/frame',
;;    `browse-kill-ring-remove-dups'.
;;
;;  Internal variables defined here:
;;
;;    `browse-kill-ring-current-ring'.
;;
;;
;;  ***** NOTE: The following functions defined in `browse-kill-ring.el'
;;              have been REDEFINED HERE:
;;
;;    `browse-kill-ring', `browse-kill-ring-default-keybindings',
;;    `browse-kill-ring-delete', `browse-kill-ring-edit',
;;    `browse-kill-ring-edit-finish', `browse-kill-ring-setup'.
;;
;;
;;  ***** NOTE: The following standard functions defined in `simple.el'
;;              have been ADVISED HERE:
;;
;;    `kill-new'.
;;
;;  Key bindings defined here for `browse-kill-ring-mode-map':
;;
;;     `t'     - `toggle-browse-kill-ring-display-style'
;;     `TAB'   - `browse-kill-ring-forward'
;;     `S-TAB' - `browse-kill-ring-previous'
;;
;;  NOTE: This library automatically calls
;;  `browse-kill-ring-default-keybindings'.  If you do not want that,
;;  then comment out this call (at the end of the file).
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;; 2009/06/25 dadams
;;     Added: browse-kill-ring-quit-deletes-window/frame.
;;     Added key bindings for TAB, S-TAB.
;;     browse-kill-ring:
;;       select-frame-set-input-focus, for MS Windows workaround.
;;       Don't show an empty buffer (for empty ring).
;; 2009/06/24 dadams
;;     Added: browse-kill-ring(-alternative)-yank-commands, browse-kill-ring-remove-dups,
;;            browse-kill-ring-default-keybindings (redefinition),
;;            browse-kill-ring (redefinition), browse-kill-ring-alternative-ring.
;;     browse-kill-ring-setup:
;;       Don't set browse-kill-ring-current-ring here.  Do it in browse-kill-ring.
;;       Use browse-kill-ring-remove-dups instead of requiring cl.el's delete-duplicates.
;; 2008/??/?? dadams
;;     Added: browse-kill-ring-current-ring, browse-kill-ring-delete,
;;            browse-kill-ring-edit(-finish), browse-kill-ring-setup.
;; 2008/??/?? dadams
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

(eval-when-compile (require 'cl)) ;; case
(require 'browse-kill-ring)

;;;;;;;;;;;;;;;;;;;;;

(defcustom browse-kill-ring-yank-commands (if (boundp 'secondary-selection-yank-commands)
                                              secondary-selection-yank-commands
                                            '(yank icicle-yank-maybe-completing))
  "*Commands that yank.
Used by `yank-pop' to tell whether the previous command was a yank command.
Used only if `browse-kill-ring-default-keybindings' has been called,
so `yank-pop' is advised."
  :type '(repeat (restricted-sexp :tag "Command that yanks text"
                  :match-alternatives (symbolp commandp) :value ignore))
  :group 'browse-kill-ring :group 'killing)

(defcustom browse-kill-ring-alternative-yank-commands
  (and (boundp 'secondary-selection-yank-secondary-commands)
       secondary-selection-yank-secondary-commands)
  "*Commands that yank using the alternative selection ring.
Used by `browse-kill-ring-setup' to tell whether the previous command
yanked from `browse-kill-ring-alternative-ring'."
  :type '(repeat (restricted-sexp :match-alternatives (symbolp commandp) :value ignore))
  :group 'browse-kill-ring :group 'killing)

(defcustom browse-kill-ring-alternative-ring
  (and (boundp 'secondary-selection-ring) ; Defined in `second-sel.el'.
       'secondary-selection-ring)
  "*Selection ring to use as an alternative to `kill-ring'.
A value of nil means `kill-ring' is always used; that is,
`browse-kill-ring-alternative-ring' is never used."
  :type '(restricted-sexp :tag "Selection ring variable"
          :match-alternatives (symbolp boundp) :value ignore)
  :group 'browse-kill-ring :group 'killing)

(defvar browse-kill-ring-current-ring 'kill-ring
  "Symbol whose value is the current selection ring for `browse-kill-ring'.")

;; Your typical standard definition - same as `icicle-remove-duplicates'.
(defun browse-kill-ring-remove-dups (list)
  "Copy of LIST with duplicate elements removed.  Tested with `equal'."
  (let ((tail  list)
        new)
    (while tail
      (unless (member (car tail) new) (push (car tail) new))
      (pop tail))
    (nreverse new)))


;; ADVISES ORIGINAL in `simple.el'.
;;
(defadvice kill-new (around browse-kill-ring-no-kill-new-duplicates)
  "Advice for not adding duplicate elements to the current selection ring.
Even after being \"activated\", this advice will only modify the
behavior of `kill-new' if `browse-kill-ring-no-duplicates' is
non-nil."
  (when browse-kill-ring-no-duplicates
    (set browse-kill-ring-current-ring
         (delete (ad-get-arg 0) (symbol-value browse-kill-ring-current-ring))))
  ad-do-it)


;; REPLACES ORIGINAL in `browse-kill-ring.el'.
;;
;; Uses `browse-kill-ring-yank-commands', not necessarily `yank'.
;;
(defun browse-kill-ring-default-keybindings ()
  "Set up `M-y' so that it can invoke `browse-kill-ring'.
Normally, if `M-y' was not preceeded by a yank command, then it has no
useful behavior.  This command sets things up so that `M-y' invokes
`browse-kill-ring'.
A yank command is a command in `browse-kill-ring-yank-commands'."
  (interactive)
  (defadvice yank-pop (around kill-ring-browse-maybe (arg))
    "If last action was not a yank, run `browse-kill-ring' instead."
    ;; yank-pop has an (interactive "*p") form which does not allow
    ;; it to run in a read-only buffer.  We want browse-kill-ring to
    ;; be allowed to run in a read only buffer, so we change the
    ;; interactive form here.  In that case, we need to
    ;; barf-if-buffer-read-only if we're going to call yank-pop with
    ;; ad-do-it
    (interactive "p")
    (if (not (memq last-command browse-kill-ring-yank-commands))
	(call-interactively #'browse-kill-ring)
      (barf-if-buffer-read-only)
      ad-do-it))
  (ad-activate 'yank-pop))


;; REPLACES ORIGINAL in `browse-kill-ring.el'.
;;
;; Uses the value of  `browse-kill-ring-current-ring', not `kill-ring'.
;;
(defun browse-kill-ring-delete ()
  "Remove the item at point from the current selection ring."
  (interactive)
  (let ((over  (car (overlays-at (point)))))
    (unless (overlayp over) (error "No kill ring item here"))
    (unwind-protect
         (progn
           (setq buffer-read-only nil)
           (let ((target  (overlay-get over 'browse-kill-ring-target)))
             (delete-region (overlay-start over) (1+ (overlay-end over)))
             (set browse-kill-ring-current-ring
                  (delete target (symbol-value browse-kill-ring-current-ring))))
           (when (get-text-property (point) 'browse-kill-ring-extra)
             (let ((prev (previous-single-property-change (point) 'browse-kill-ring-extra))
                   (next (next-single-property-change (point) 'browse-kill-ring-extra)))
               (when prev(incf prev))   ; This is some voodoo.
               (when next(incf next))
               (delete-region (or prev (point-min)) (or next (point-max))))))
      (setq buffer-read-only t)))
  (browse-kill-ring-resize-window)
  (browse-kill-ring-forward 0))


;; REPLACES ORIGINAL in `browse-kill-ring.el'.
;;
;; Uses the value of  `browse-kill-ring-current-ring', not `kill-ring'.
;;
(defun browse-kill-ring-edit ()
  "Edit the current selection ring entry at point."
  (interactive)
  (let ((overs  (overlays-at (point))))
    (unless overs (error "No kill ring entry here"))
    (let* ((target       (overlay-get (car overs) 'browse-kill-ring-target))
	   (target-cell  (member target (symbol-value browse-kill-ring-current-ring))))
      (unless target-cell(error "Item deleted from the current selection ring"))
      (switch-to-buffer (get-buffer-create "*Kill Ring Edit*"))
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert target)
      (goto-char (point-min))
      (browse-kill-ring-resize-window)
      (browse-kill-ring-edit-mode)
      (message "%s" (substitute-command-keys
                     "Use \\[browse-kill-ring-edit-finish] to finish editing."))
      (setq browse-kill-ring-edit-target target-cell))))


;; REPLACES ORIGINAL in `browse-kill-ring.el'.
;;
;; Uses the value of  `browse-kill-ring-current-ring', not `kill-ring'.
;;
(defun browse-kill-ring-edit-finish ()
  "Commit the changes to the current selection ring."
  (interactive)
  (if browse-kill-ring-edit-target
      (setcar browse-kill-ring-edit-target (buffer-string))
    (when (y-or-n-p "The item has been deleted; add to front? ")
      (push (buffer-string) (symbol-value browse-kill-ring-current-ring))))
  (bury-buffer)
  (when (eq major-mode 'browse-kill-ring-mode) ; The user might have rearranged the windows
    (browse-kill-ring-setup (current-buffer) browse-kill-ring-original-window nil
			    browse-kill-ring-original-window-config)
    (browse-kill-ring-resize-window)))


;; REPLACES ORIGINAL in `browse-kill-ring.el'.
;;
;; 1. Added prefix arg.
;; 2. Added current ring name to name of display buffer: *Kill Ring: `<NAME>'*.
;; 3. Don't show buffer if ring is empty and buffer is not already shown.  Just message.
;; 4. Updated doc string to mention `browse-kill-ring-current-ring'.
;;
(defun browse-kill-ring (&optional other-ring-p)
  "Display the items in `browse-kill-ring-current-ring'.
With a prefix arg, switch to the other selection ring.

Also, if the current buffer is in `browse-kill-ring-mode', then switch
to the other selection ring.  When in a `browse-kill-ring' window, you
can thus use this toggle between the two rings.

\(If `browse-kill-ring-alternative-ring' is nil, then switching to the
other ring has no effect.)"
  (interactive "P")
  (when (eq major-mode 'browse-kill-ring-mode)
    (setq other-ring-p  t)
    (delete-window))
  (when (and other-ring-p browse-kill-ring-alternative-ring)
    (setq browse-kill-ring-current-ring  (if (eq browse-kill-ring-current-ring 'kill-ring)
                                             browse-kill-ring-alternative-ring
                                           'kill-ring)))
  (when buffer-read-only (message "Buffer is read-only %s" (current-buffer)))
  (let ((orig-buf  (current-buffer))
        (buf       (get-buffer-create (format "*Kill Ring: `%s'*"
                                              browse-kill-ring-current-ring))))
    (unless (symbol-value browse-kill-ring-current-ring)
      (message "Selection ring is empty"))
     ;; If empty ring and BUF is not already displayed, then don't display it.
    (when (or (symbol-value browse-kill-ring-current-ring)
              (get-buffer-window buf 'visible))
      (browse-kill-ring-setup buf (selected-window))
      (pop-to-buffer buf)
      (select-frame-set-input-focus (window-frame (selected-window))) ; Windows workaround
      (browse-kill-ring-resize-window))
    nil))


;; REPLACES ORIGINAL in `browse-kill-ring.el'.
;;
;; 1. Use `browse-kill-ring-current-ring', not necessarily `kill-ring'.
;; 2. Don't require `cl.el' - use local function `browse-kill-ring-remove-dups'.
;;
(defun browse-kill-ring-setup (buf window &optional regexp window-config)
  (with-current-buffer buf
    (unwind-protect
         (progn
           (browse-kill-ring-mode)
           (setq buffer-read-only nil)
           (when (eq browse-kill-ring-display-style 'one-line) (setq truncate-lines t))
           (let ((inhibit-read-only  t))  (erase-buffer))
           (setq browse-kill-ring-original-window   window
                 browse-kill-ring-original-window-config
                 (or window-config (current-window-configuration)))
           (let ((browse-kill-ring-maximum-display-length
                  (if (and browse-kill-ring-maximum-display-length
                           (<= browse-kill-ring-maximum-display-length 3))
                      4
                    browse-kill-ring-maximum-display-length))
                 (items  (mapcar (if browse-kill-ring-depropertize
                                     #'browse-kill-ring-depropertize-string
                                   #'copy-sequence)
                                 (symbol-value browse-kill-ring-current-ring))))
             (unless browse-kill-ring-display-duplicates
               (setq items (browse-kill-ring-remove-dups items)))
             (when (stringp regexp)
               (setq items (delq nil (mapcar #'(lambda (item)
                                                 (when (string-match regexp item) item))
                                             items))))
             (funcall (or (cdr (assq browse-kill-ring-display-style
                                     browse-kill-ring-display-styles))
                          (error "Invalid `browse-kill-ring-display-style': %s"
                                 browse-kill-ring-display-style))
                      items)
             (message
              (let* ((len    (length (symbol-value browse-kill-ring-current-ring)))
                     (entry  (if (= 1 len) "entry" "entries")))
                (concat
                 (if (and (not regexp) browse-kill-ring-display-duplicates)
                     (format "%s %s in the ring." len entry)
                   (format "%s (of %s) %s in the ring shown." (length items) len entry))
                 (substitute-command-keys
                  "    Type \\[browse-kill-ring-quit] to quit.  \
\\[describe-mode] for help."))))
             ;; End code from Michael Slass <mikesl@wrq.com>
             (set-buffer-modified-p nil)
             (goto-char (point-min))
             (browse-kill-ring-forward 0)
             (when regexp (setq mode-name (concat "Kill Ring [" regexp "]")))
             (run-hooks 'browse-kill-ring-hook)
             ;; I will be very glad when I can get rid of this gross
             ;; hack, which solely exists for XEmacs users.
             (when (and (featurep 'xemacs) font-lock-mode)
               (browse-kill-ring-fontify-region (point-min) (point-max)))))
      (setq buffer-read-only t))))

(defun browse-kill-ring-quit-deletes-window/frame ()
  "Bury buffer.  Delete window or, if dedicated, frame.
Useful as customized value of `browse-kill-ring-quit-action'."
  (let ((buf            (current-buffer))
        (sole-window-p  (eq (selected-window) (frame-root-window))))
    (if (and sole-window-p (window-dedicated-p (selected-window)))
        (delete-frame)
      (unless sole-window-p (delete-window)))
    ;; Avoid giving BUF as explicit arg to `bury-buffer', since we want to undisplay it.
    (with-current-buffer buf (bury-buffer))))

(defun toggle-browse-kill-ring-display-style ()
  "Toggle browse-kill-ring-display-style between `separated' and `one-line'."
  (interactive)
  (setq browse-kill-ring-display-style (case browse-kill-ring-display-style
                                         (separated 'one-line)
                                         (otherwise 'separated)))
  (browse-kill-ring-update)
  (message "browse-kill-ring-display-style is now %s" browse-kill-ring-display-style))


;;; Key Bindings

(browse-kill-ring-default-keybindings)

(define-key browse-kill-ring-mode-map (kbd "t")     'toggle-browse-kill-ring-display-style)
(define-key browse-kill-ring-mode-map (kbd "<tab>")   'browse-kill-ring-forward)
(define-key browse-kill-ring-mode-map [(control ?i)]  'browse-kill-ring-forward)
(define-key browse-kill-ring-mode-map (kbd "<S-tab>") 'browse-kill-ring-previous)

;;;;;;;;;;;;;;;;;;;;;

(provide 'browse-kill-ring+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; browse-kill-ring+.el ends here
