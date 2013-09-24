;;; frame-bufs.el --- a minor mode for frame-relative buffer lists

;; Copyright (c) 2011-2013 Alp Aker

;; Author: Alp Aker <alp.tekin.aker@gmail.com>
;; Version: 2.04
;; Keywords: convenience, buffers

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; A copy of the GNU General Public License can be obtained from the
;; Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:

;; Frame-bufs extends Emacs's buffer menu so that it understands a
;; distinction between those buffers that "belong" to a frame and those that
;; do not.  The buffer menu can be toggled between listing all buffers and
;; listing only those buffers associated with the selected frame.  Buffers can be
;; added to and removed from the list of buffers.  The criteria governing
;; which buffers are automatically associated with a frame can be customized.

;; The package interacts properly with `other-buffer' and respects changes in
;; buffer ordering made by `bury-buffer'.  It does not alter the
;; `buffer-list' or `buried-buffer-list' frame parameters.  It is not
;; compatible with non-nil values of `pop-up-frames'.

;; Installation
;; ============

;; Put this file in your load path and put:
;;
;;   (require 'frame-bufs)
;;
;; in your .emacs.  To toggle frame-bufs mode on and off, use the command
;;`frame-bufs-mode'.  To turn it on automatically when starting Emacs, put:
;;
;;  (frame-bufs-mode t)
;;
;; in your .emacs.

;; Usage
;; =====

;; When frame-bufs-mode is enabled, the buffer menu has two modes: In global
;; mode, it lists all buffers; in local mode it lists only those buffers that
;; are associated with the selected frame.  One can toggle between the modes
;; by typing "F".

;; In global mode, there is a new fourth column after the initial CRM
;; columns--the `F' column.  Buffers associated with the selected frame are
;; indicated with an `o' in this column.  In local mode, the fourth `F'
;; column is suppressed.  (Global/local status is also indicated in the mode
;; line.)

;; The typical way a buffer becomes associated with a frame is by being
;; selected in a window on the frame.  One can manually associate buffers
;; with a frame, and disassociate them as well, using two other commands in
;; the buffer menu.  By typing `A' a buffer can be marked as to be added to
;; the buffers associated with the selected frame.  By typing `N' a buffer
;; can be marked as to have its association with the selected frame
;; severed.  As with other actions in the buffer menu, these changes take
;; effect when `Buffer-menu-execute' is called.

;; When first called, the buffer menu opens in global mode.  In subsequent
;; calls it opens in whatever mode it was last in.

;; Criteria That Control Buffer-Frame Association
;; ==============================================

;; The association between buffers and frames is dynamic:  If a buffer is
;; selected on a frame, then it becomes associated with that frame.  Note,
;; then, that a buffer can be associated with more than one frame.

;; In addition, several other variables control which buffers automatically
;; become associated with a frame:

;; o If `frame-bufs-include-displayed-buffers' is non-nil, then buffers that
;;   are merely displayed on a frame become associated with the frame, even
;;   if they have not been selected.

;; o If a buffer's name is a member of `frame-bufs-always-include-names' then
;;   that buffer is automatically associated with every frame.  The default
;;   value is ("*scratch*" "*notes*").

;; o Three variables control which buffers are associated with a newly created
;;   frame:
;;
;;   - `frame-bufs-new-frames-inherit': If non-nil, then the buffers
;;      associated with a new frame include (at least) the buffers that were
;;      associated with the new frame's "parent," i.e., the frame that was
;;      selected when the new frame was created.
;;   - `frame-bufs-include-new-buffers': If non-nil, and the command that
;;      creates a new frame also creates new buffers, the new buffers are
;;      associated with the new frame.  (This applies only to buffers that
;;      are created *after* the new frame is created.)
;;   - `frame-bufs-include-init-buffer':  If non-nil, then the buffer that is
;;      current when a new frame is created will be associated with the new
;;      frame.  If nil, it will not.  (Note that
;;      frame-bufs-new-frames-inherit takes precedence over this
;;      variable.  Also note:  If the buffer in question is displayed on the
;;      new frame when the frame-creating command terminates, it will still
;;      be associated with the new frame.)

;; Other Commands and Features
;; ===========================

;; o If `frame-bufs-use-buffer-predicate' is non-nil, each frame's buffer
;;   predicate is set so that `other-buffer' will prefer buffers associated
;;   with the selected frame.  Thus, when a buffer is removed from a window
;;   and automatically replaced with another (as happens, say, when one kills
;;   a buffer), the newly displayed buffer will, if possible, be another
;;   frame-associated buffer.  The default value is t.

;; Frame-bufs provides three other commands that are available everywhere,
;; not just in the buffer menu:

;; o `frame-bufs-dismiss-buffer' is somewhat analogous to `bury-buffer'.  It
;;   severs the association of a buffer with a frame, and if that buffer is
;;   displayed in any windows on the selected frame, it is replaced by
;;   another buffer.  When called with no arguments, it acts on the current
;;   buffer.

;; o `frame-bufs-reset-frame' resets a frame's associated-buffer list;
;;   specifically, it sets the list of associated buffers to the list of
;;   buffers that have been selected on the frame.  When called with no
;;   argument, it acts on the current frame.

;; o `frame-bufs-reset-all-frames' resets the associated buffers of all
;;   frames.

;; None of these commands is given a defualt key binding.

;; Other Customization Options
;; ===========================

;; o To rebind the new buffer menu commands, alter their bindings in the
;;   keymap `frame-bufs-mode-map'.

;; o The indicator bit used for frame-associated buffers (default `o') can be
;;   set via the variable `frame-bufs-associated-buffer-bit'.

;; o The strings used to indicate local/global state in the buffer menu's
;;   mode line can be changed by means of the variables
;;   `frame-bufs-mode-line-local-string' and
;;   `frame-bufs-mode-line-global-string'.  The mode-line indication can be
;;   turned off by setting `frame-bufs-mode-line-indication' to nil.  (This
;;   latter variable can be set to any valid mode-line construct; users
;;   setting this variable to a custom mode-line construct will probably want
;;   to make use of the variable `frame-bufs--global-list'.)

;; Using Frame-Bufs in Programs
;; ============================

;; o To use a frame's associated-buffer list from within a Lisp progam, it is
;;   recommended that you work with the list returned by the function
;;   `frame-bufs-buffer-list'; don't use the value of the
;;   frame-bufs-buffer-list frame parameter.  The latter can contain internal
;;   buffers (buffers whose names starts with a space) and dead buffers; it
;;   is not guaranteed to respect `frame-bufs-always-include-names'; and its
;;   order is meaningless.  The list returned by `frame-bufs-buffer-list'
;;   will contain only live, non-internal buffers; be updated to reflect the
;;   current value of frame-bufs-always-include-names; and be sorted
;;   stably by selection order on the current frame.

;; Acknowledgements
;; ============================

;; Thanks to Greg Bognar for alpha testing and to Drew Adams for suggesting
;; many improvements.

;;; Code:

(when (< emacs-major-version 24)
  (error "Frame-Bufs requires version 22 or later"))

;;; ---------------------------------------------------------------------
;;; User Options
;;; ---------------------------------------------------------------------

(defgroup frame-bufs nil
  "Extend buffer-menu to allow listing of buffers associated with particular frame."
  :group 'convenience)

(defcustom frame-bufs-mode-hook nil
  "Hook run when frame-bufs mode is enabled or disabled."
  :group 'frame-bufs
  :type 'hook)

(defcustom frame-bufs-mode-on-hook nil
  "Hook run when frame-bufs mode is enabled."
  :group 'frame-bufs
  :type 'hook)

(defcustom frame-bufs-mode-off-hook nil
  "Hook run when frame-bufs mode is disabled."
  :group 'frame-bufs
  :type 'hook)

(defcustom frame-bufs-use-buffer-predicate t
  "Make `other-buffer' prefer associated buffers.
If non-nil, frame-bufs sets the buffer predicate of each frame
so that `other-buffer' will prefer buffers associated with that
frame.  If nil, `other-buffer' does not prefer frame-associated
buffers.

Changes to this variable do not take effect until the
mode-function `frame-bufs-mode' is run."
  :group 'frame-bufs
  :type 'boolean)

(defcustom frame-bufs-always-include-names '("*scratch*" "*notes*")
  "Buffers whose names are in this list are associated with every frame."
  :group 'frame-bufs
  :type '(repeat string))

(defcustom frame-bufs-include-displayed-buffers nil
  "If non-nil, buffers displayed on a frame becomes associated with it.
If nil, buffers becomes associated with a frame only if they are
selected on that frame, not merely displayed."
  :group 'frame-bufs
  :type 'boolean)

(defcustom frame-bufs-include-new-buffers nil
  "Include new buffers in a new frame's associated-buffer list.
If non-nil, and the command that creates a new frame also creates
new buffers, those buffers will be associated with the new frame,
even if they have not been selected.  (Buffers created before the
new frame is created are not thus captured.)"
  :group 'frame-bufs
  :type 'boolean)

(defcustom frame-bufs-new-frames-inherit nil
  "Whether a new frame inherits the associations  of its \"parent\".
If non-nil, the associated buffers of a newly created frame
include (at least) those buffers that were associated with the
frame that was selected when the frame-creating command was
called."
  :group 'frame-bufs
  :type 'boolean)

(defcustom frame-bufs-include-init-buffer nil
  "Whether a new frame's associated buffers include the last buffer before creation.
If non-nil, then the buffer that is current when a frame-creating
command is called--the \"init buffer\"--is associated with the
new frame.  If nil, it is not.

Note:  If the init buffer is displayed on the new frame after the
frame-creating command terminates, then it will be associated
with the new frame, even if this variable is nil.  Also note:
`frame-bufs-new-frames-inherit' takes precedence over this
variable."
  :group 'frame-bufs
  :type 'boolean)

; Experimental.
(defcustom frame-bufs-assoc-rules nil
  ""
  :group 'frame-bufs
  :type 'list)

(defcustom frame-bufs-mode-line-local-string "[Local]"
  "Mode-line indication that the buffer menu is in local mode."
  :group 'frame-bufs
  :type 'string)

(defcustom frame-bufs-mode-line-global-string "[Global]"
  "Mode-line indication that the buffer menu is in global mode."
  :group 'frame-bufs
  :type 'string)

(defcustom frame-bufs-mode-line-identification
  '(frame-bufs--global-list
    (:eval (propertize frame-bufs-mode-line-global-string
                        'local-map frame-bufs-mode-line-keymap
                        'help-echo  (concat "List of all buffers\n"
                                            "mouse-1 for local list")))
     (:eval (propertize frame-bufs-mode-line-local-string
                        'local-map frame-bufs-mode-line-keymap
                        'help-echo (concat "Buffer list for frame \""
                                           (frame-parameter nil 'name)
                                           "\"\n"
                                           "mouse-1 for global list"))))
  "Mode-line indication of the buffer menu's state.
When frame-bufs is enabled, this variable is inserted into the
value of `mode-line-format' in the buffer menu, after
`mode-line-buffer-identification'.  If this variable is set to
nil, no special information appears in the mode-line.  The value
should be a valid mode-line construct.

When customizing this variable, users will probably want to make
use of the variable `frame-bufs--global-list'."
  :group 'frame-bufs
  :type 'sexp)

(defcustom frame-bufs-associated-buffer-bit ?o
  "Character used to indicate frame-associated buffers in the buffer menu."
  :group 'frame-bufs
  :type 'character)

;;; ---------------------------------------------------------------------
;;; Internal Variables
;;; ---------------------------------------------------------------------

(defvar frame-bufs--global-list t
  "Records whether the buffer menu is in global or local mode.")

;; The following are used in initializing the associated-buffer list of a
;; newly created frame.

;; Records which buffer is current when a new frame is created.  Used when
;; `frame-bufs-include-new-buffers' is non-nil.
(defvar frame-bufs--init-buffer nil)

;; Records the associated buffers of the selected frame before a new frame is
;; created.  Used when `per-frame-new-frames-inherit' is non-nil.
(defvar frame-bufs--parent-buffer-list nil)

;; Records which buffers are already in existence when a new frame is
;; created.  Used when `frame-bufs-include-new-buffers' is non-nil.
(defvar frame-bufs--prev-buffers nil)

;; When a new frame is created, records the identity of that frame.  Used by
;; `frame-bufs--initialize-new-frame' in conjunction with the previous
;; variables to initialized the associated-buffer list.
(defvar frame-bufs--new-frame nil)

(defconst frame-bufs--size-column 4)

(defconst frame-bufs--advised-fns
  '(electric-buffer-list select-window))

(defconst frame-bufs--hook-assignments
  '((Buffer-menu-mode-hook . frame-bufs--set-up-buff-menu)
    (window-configuration-change-hook . frame-bufs--window-change)
    (before-make-frame-hook . frame-bufs--before-make-frame)
    (after-make-frame-functions . frame-bufs--after-make-frame)))

;;; ---------------------------------------------------------------------
;;; Mode Definition and Keymaps
;;; ---------------------------------------------------------------------

(defvar frame-bufs-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "F" 'frame-bufs-toggle-global-list)
    (define-key map "A" 'frame-bufs-make-associated)
    (define-key map "N" 'frame-bufs-make-non-associated)
    map)
    "Keymap for `frame-bufs-mode'.
See the documentation of that command for details.")

(set-keymap-parent frame-bufs-mode-map Buffer-menu-mode-map)

(defvar frame-bufs-mode-line-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] 'frame-bufs-mode-line-toggle-global-list)
    map)
"Keymap for `frame-bufs-mode-line-identification'.")

(defvar frame-bufs-mode nil
  "Non-nil if frame-bufs mode is enabled.

Do not set this variable directly.  Use the command
`frame-bufs-mode' instead.")

(add-to-list 'minor-mode-list 'frame-bufs-mode)

(defun frame-bufs-mode (&optional arg)
  "Toggle frame-bufs-mode.

Frame-bufs-mode tracks which buffers are associated with a given
frame and extends the buffer menu to take advantage of this
information.  The buffer menu can be toggled between listing all
buffers and listing only frame-associated buffers.

When listing all buffers, there is a fourth column in the buffer
menu after the CRM columns: the F column.  Buffers associated
with the current frame are indicated with an `o' in this column
.  When listing only frame-associated buffers, this fourth column
is suppressed.

The list of buffers associated with a frame can be manually
edited from within the buffer menu.

The following new commands are available in the buffer
menu:

\\<frame-bufs-mode-map>\\[frame-bufs-toggle-global-list] -- Toggle between listing frame-associated buffers and all buffers.
\\[frame-bufs-make-associated] -- Mark a buffer to be added to the associated buffer list.
\\[frame-bufs-make-non-associated] -- Mark a buffer to be removed from the associated buffer list.

Requested changes in frame-buffer associations are effected by
calling `Buffer-menu-execute'.

Buffers automatically become associated with a frame if they are
selected in one of the frame's windows.  Further control over
which buffers are automatically associated with a frame is
provided by the variables `frame-bufs-include-displayed-buffers',
`frame-bufs-always-include-names',
`frame-bufs-include-new-buffers',
`frame-bufs-new-frames-inherit', and
`frame-bufs-include-init-buffer'.

For further customization options, see the documentation of the
variables `frame-bufs-associated-buffer-bit', `frame-bufs-use-buffer-predicate',
`frame-bufs-mode-line-local-string',
`frame-bufs-mode-line-global-string', and
`frame-bufs-mode-line-identification'."
  (interactive "P")
  (setq frame-bufs-mode (if (not arg)
                         (not frame-bufs-mode)
                       (> (prefix-numeric-value arg) 0)))
  (if frame-bufs-mode
      ;; Enabling.
      (progn
        (dolist (frame (frame-list))
          (frame-bufs--set-buffer-predicate frame frame-bufs-use-buffer-predicate)
          (frame-bufs--initialize-existing-frame frame))
        (setq Buffer-menu-buffer-column 5
              frame-bufs--size-column 5)
        (ad-enable-regexp "frame-bufs")
        (dolist (fn frame-bufs--advised-fns)
          (ad-activate fn))
        (dolist (hook frame-bufs--hook-assignments)
          (add-hook (car hook) (cdr hook)))
        ;; In case we toggle the mode while the buffer menu exists.
        (let ((buf (get-buffer "*Buffer List*")))
          (when buf
            (with-current-buffer buf
              (revert-buffer)
              (frame-bufs--set-up-buff-menu))))
        (run-hooks 'frame-bufs-mode-on-hook)
        (message "Per-frame buffer menus are enabled"))
    ;; Disabling.
    (dolist (frame (frame-list))
      (frame-bufs--set-buffer-predicate frame nil))
    (setq Buffer-menu-buffer-column 4
          frame-bufs--size-column 4)
    (ad-disable-regexp "frame-bufs")
    (dolist (fn frame-bufs--advised-fns)
      (ad-activate fn))
    (dolist (hook frame-bufs--hook-assignments)
      (remove-hook (car hook) (cdr hook)))
    ;; Again, in case we toggle the mode while the buffer menu exists.
    (let ((buf (get-buffer "*Buffer List*")))
      (when buf
        (with-current-buffer buf
          (revert-buffer)
          (frame-bufs--unload-from-buff-menu))))
    (run-hooks 'frame-bufs-mode-off-hook)
    (message "Per-frame buffer menus are disabled"))
  (run-mode-hooks 'frame-bufs-mode-hook))

;;; ---------------------------------------------------------------------
;;; Frame Initialization and Clean Up
;;; ---------------------------------------------------------------------

;; Set the associated-buffer list for frames already in existence when frame-bufs
;; is enabled.
(defun frame-bufs--initialize-existing-frame (frame)
  (frame-bufs--add-buffers (append (frame-parameter frame 'buffer-list)
                                  (frame-parameter frame 'buried-buffer-list)
                                  (if frame-bufs-include-displayed-buffers
                                      (mapcar #'(lambda (x) (window-buffer x))
                                              (window-list frame 'no-minibuf))))
                          frame))

;; The next four functions handle initialization of the associated-buffer
;; list for newly created frames.  We defer some of the initialization until
;; after the command creating a new frame terminates, for the sake of the
;; option `frame-bufs-include-new-buffers'.
(defun frame-bufs--before-make-frame ()
    (setq frame-bufs--init-buffer (current-buffer)
          frame-bufs--prev-buffers (buffer-list)
          frame-bufs--parent-buffer-list (copy-sequence
                                         (frame-parameter (selected-frame)
                                                          'frame-bufs-buffer-list))))

(defun frame-bufs--after-make-frame (frame)
  (frame-bufs--set-buffer-predicate frame frame-bufs-use-buffer-predicate)
  (add-hook 'post-command-hook 'frame-bufs--initialize-new-frame)
  (setq frame-bufs--new-frame frame))

(defun frame-bufs--initialize-new-frame ()
  (remove-hook 'post-command-hook 'frame-bufs-initialize-new-frame)
  (unwind-protect
      (when (frame-live-p frame-bufs--new-frame)
        (when frame-bufs-include-new-buffers
          (frame-bufs--add-buffers (frame-bufs--set-diff (buffer-list)
                                                         frame-bufs--prev-buffers)
                                  frame-bufs--new-frame))
        (unless (or frame-bufs-include-init-buffer
                    (memq frame-bufs--init-buffer
                          (mapcar #'(lambda (x) (window-buffer x))
                                  (window-list frame-bufs--new-frame 'no-minibuf))))
          (frame-bufs--remove-buffer frame-bufs--init-buffer frame-bufs--new-frame))
        (when frame-bufs-new-frames-inherit
          (frame-bufs--add-buffers frame-bufs--parent-buffer-list frame-bufs--new-frame))
        ;; Enforce custom buffer-frame associations.
        (frame-bufs--enforce-rules frame-bufs--new-frame))
    (setq frame-bufs--new-frame nil
          frame-bufs--parent-buffer-list nil
          frame-bufs--init-buffer nil
          frame-bufs--prev-buffers nil)))

(defun frame-bufs--set-buffer-predicate (frame on)
  (let ((buffer-pred  (frame-parameter frame 'buffer-predicate)))
    (if on
        (unless (eq buffer-pred 'frame-bufs--ok-to-display-p)
          (set-frame-parameter frame
                               'frame-bufs-saved-buffer-pred
                               buffer-pred)
          (set-frame-parameter frame
                               'buffer-predicate
                               'frame-bufs--ok-to-display-p))
      (when (eq buffer-pred 'frame-bufs--ok-to-display-p)
        (set-frame-parameter frame
                             'buffer-predicate
                             (frame-parameter frame 'frame-bufs-saved-buffer-predicate))
        (set-frame-parameter frame
                             'frame-bufs-saved-buffer-predicate
                             nil)))))

;;; ---------------------------------------------------------------------
;;; Per-Frame Buffer List Maintenance and Manipulation
;;; ---------------------------------------------------------------------

(defadvice select-window (after frame-bufs)
  (frame-bufs--add-buffer (window-buffer) (window-frame)))

;; Called by window-configuration-change-hook to update the associated-buffer
;; list.
(defun frame-bufs--window-change ()
  (let ((frame (selected-frame)))
    (dolist (win (window-list frame 'no-minibuf))
      (let ((buf (window-buffer win)))
        ;; If merely displayed buffers are ok add buf.  If not, add buf if
        ;; it's been selected on the frame.
        (when (or frame-bufs-include-displayed-buffers
                  (memq buf (frame-parameter frame 'buffer-list))
                  (memq buf (frame-parameter frame 'buried-buffer-list)))
          (frame-bufs--add-buffer buf frame))))))

(defun frame-bufs--remove-buffer (buf frame)
  "Remove BUF from FRAME's associated-buffer list."
  (set-frame-parameter frame
                       'frame-bufs-buffer-list
                       (delq buf (frame-parameter frame 'frame-bufs-buffer-list))))

(defun frame-bufs--add-buffer (buf frame)
  "Add BUF to FRAME's associated-buffer list if not already present."
  (unless (bufferp buf)
    (signal 'wrong-type-argument (list 'bufferp buf)))
  (let ((associated-bufs (frame-parameter frame 'frame-bufs-buffer-list)))
    (unless (memq buf associated-bufs)
      (set-frame-parameter frame 'frame-bufs-buffer-list (cons buf associated-bufs)))))

(defun frame-bufs--add-buffers (bufs frame)
  "Add each member of BUFS to FRAME's associated-buffer list if
not already present."
  (dolist (buf bufs)
    (frame-bufs--add-buffer buf frame)))

(defun frame-bufs-buffer-list (frame &optional global)
  "When called with argument GLOBAL non-nil, return the same
result as (buffer-list FRAME).  With GLOBAL nil, update FRAME's
associated-buffer list and return it, sorted by selection order
on FRAME.  The return value is a copy of the list, not the list
itself."
  (frame-bufs--filter-buffers
   (if global
       (buffer-list frame)
     ;; First, remove dead buffers.  (Should be able to do this as the
     ;; buffers are killed, via kill-buffer-hook, but there are a few corner
     ;; cases that let dead buffers slip through that way.)
     (set-frame-parameter frame
                          'frame-bufs-buffer-list
                          (delq nil
                                (mapcar #'(lambda (x) (if (buffer-live-p x) x))
                                        (frame-parameter frame
                                                         'frame-bufs-buffer-list))))
     ;; Include members of frame-bufs-always-include-names
     (dolist (bufname frame-bufs-always-include-names)
       (when (get-buffer bufname)
         (frame-bufs--add-buffer (get-buffer bufname) frame)))
     ;; Enforce custom buffer-frame associations.
     (frame-bufs--enforce-rules frame)
     (frame-bufs--sort-buffers frame (frame-parameter frame 'frame-bufs-buffer-list)))))

(defun frame-bufs--enforce-rules (&optional frame)
  (or frame (setq frame (selected-frame)))
  (dolist (rule frame-bufs-assoc-rules)
    (dolist (buffer (buffer-list))
      (if (funcall rule frame buffer)
          (frame-bufs--add-buffer buffer frame)
        (frame-bufs--remove-buffer buffer frame)))))

;;; ---------------------------------------------------------------------
;;; Utilities and Predicates
;;; ---------------------------------------------------------------------

;; Return a list in which BUFS are sorted according to selection order on
;; FRAME.
(defun frame-bufs--sort-buffers (frame bufs)
  (let ((l (buffer-list frame)))
    (sort (copy-sequence bufs) #'(lambda (x y) (> (length (memq x l))
                                                  (length (memq y l)))))))

;; Remove internal buffers from BUFS.
(defun frame-bufs--filter-buffers (bufs)
  (delq nil
        (mapcar #'(lambda (x) (if (not (string-match "^ " (buffer-name x))) x))
                bufs)))

(defun frame-bufs--set-diff (minuend subtrahend)
  (let ((res '()))
    (dolist (e minuend)
      (unless (memq e subtrahend)
        (push e res)))
    (nreverse res)))

(defun frame-bufs--ok-to-display-p (buf)
  (let ((other-pred (frame-parameter nil 'frame-bufs-saved-buffer-pred)))
    (and (frame-bufs--associated-p buf)
         (or (not (functionp other-pred)
                  (funcall other-pred buf))))))

(defun frame-bufs--associated-p (buf &optional frame)
  (memq buf (frame-parameter frame 'frame-bufs-buffer-list)))

;; Return bit info for BUF appropriate for the 4th column in the buffer-menu.
(defun frame-bufs--bit-info (buf)
  (if (and frame-bufs--global-list
           (frame-bufs--associated-p buf))
      (char-to-string frame-bufs-associated-buffer-bit)
    " "))

(defun frame-bufs--set-up-buff-menu ()
  (use-local-map frame-bufs-mode-map)
  (let ((before (reverse (memq 'mode-line-buffer-identification
                                (reverse mode-line-format))))
        (after (cdr (memq 'mode-line-buffer-identification mode-line-format))))
  (setq mode-line-format (append before
                                 (list frame-bufs-mode-line-identification)
                                 after))))

(defun frame-bufs--unload-from-buff-menu ()
  (use-local-map Buffer-menu-mode-map)
  (setq mode-line-format (delq frame-bufs-mode-line-identification mode-line-format)))

;;; ---------------------------------------------------------------------
;;; Global Commands
;;; ---------------------------------------------------------------------

(defun frame-bufs-dismiss-buffer (&optional buf frame)
  "Remove assocation between BUF and FRAME.
If any windows on FRAME are currently displaying BUF, replace BUF
in those windows with some other buffer.  Arguments default to
the current buffer and the selected frame."
  (interactive)
  (unless buf
    (setq buf (current-buffer)))
  (unless frame
    (setq frame (selected-frame)))
  ;; We loop over the windows ourselves because replace-buffer-in-windows
  ;; acts on all frames; we only want to act on the selected frame.
  (dolist (win (get-buffer-window-list buf 'no-minibuf frame))
    (set-window-buffer win (other-buffer buf)))
  (frame-bufs--remove-buffer buf frame))

(defun frame-bufs-reset-frame (&optional frame)
  "Reset FRAME's associated-buffer list.
Set list of buffers associated with FRAME to the list of all
buffers that have been selected on FRAME, and no others.  FRAME
defualts to the selected frame."
  (interactive)
  (unless frame
    (setq frame (selected-frame)))
  (set-frame-parameter frame
                       'frame-bufs-buffer-list
                       ;; Make sure we get copies, not the lists themselves.
                       (append
                        (frame-parameter frame 'buffer-list)
                        (frame-parameter frame 'buried-buffer-list)
                        '())))

(defun frame-bufs-reset-all-frames ()
  "Call `frame-bufs-reset-frame' on all live frames."
  (interactive)
  (dolist (frame (frame-list))
    (frame-bufs-reset-frame frame)))

;;; ---------------------------------------------------------------------
;;; Buffer Menu Commands (New)
;;; ---------------------------------------------------------------------

(defun frame-bufs-toggle-global-list (&optional arg)
  "Toggle whether the buffer-menu displays only buffers associated with this frame.
With a positive or true ARG display only frame-associated buffers.  With
zero, negative, or nil ARG, display all buffers."
  (interactive "P")
  (setq frame-bufs--global-list
        (cond ((not arg) (not frame-bufs--global-list))
              ((<= (prefix-numeric-value arg) 0) t)))
  (revert-buffer))

(defun frame-bufs-mode-line-toggle-global-list (e)
  "Toggle whether the buffer-menu displays only buffers associated with this frame."
  (interactive "e")
  (save-selected-window
    (select-window (posn-window (event-start e)))
    (frame-bufs-toggle-global-list)))

(defun frame-bufs-make-associated (&optional arg)
  "Mark buffer on this line to be associated with this frame by \\<Buffer-memu-mode-map>\\[Buffer-menu-execute].
Prefix arg is how many buffers to associate.  Negative arg means
work backwards."
  (interactive "p")
  (when (or (null arg) (= arg 0))
    (setq arg 1))
  (if (not frame-bufs--global-list)
      (forward-line arg)
    (while (< 0 arg)
      (when (Buffer-menu-buffer)
        (tabulated-list-set-col 3 "A" t)
        (forward-line 1)
        (setq arg (1- arg))))
    (while (< arg 0)
    (when (Buffer-menu-buffer)
      (tabulated-list-set-col 3 "A" t)
      (forward-line -1)
      (setq arg (1+ arg))))))

(defun frame-bufs-make-non-associated (&optional arg)
  "Mark buffer on this line to be associated with this frame by \\<Buffer-memu-mode-map>\\[Buffer-menu-execute].
Prefix arg is how many buffers to associate.  Negative arg means
work backwards."
  (interactive "p")
  (when (or (null arg) (= arg 0))
    (setq arg 1))
  (while (< 0 arg)
    (when (Buffer-menu-buffer)
      (tabulated-list-set-col 3 "N" t)
      (forward-line 1)
      (setq arg (1- arg))))
  (while (< arg 0)
    (when (Buffer-menu-buffer)
      (tabulated-list-set-col 3 "N" t)
      (forward-line -1)
      (setq arg (1+ arg)))))

;;; ---------------------------------------------------------------------
;;;  Buffer Menu Commands (Redefined)
;;; ---------------------------------------------------------------------

(defun Buffer-menu-unmark (&optional backup)
  "Cancel all requested operations on buffer on this line and move down.
Optional ARG means move up."
  (interactive "P")
  (Buffer-menu--unmark)
  (forward-line (if backup -1 1)))

(defun Buffer-menu--unmark ()
  (tabulated-list-set-col 0 " " t)
  (let ((buf (Buffer-menu-buffer)))
    (when buf
      (if (buffer-modified-p buf)
          (tabulated-list-set-col 2 "*" t)
        (tabulated-list-set-col 2 " " t))
      (when frame-bufs-mode
        (tabulated-list-set-col 3 (frame-bufs--bit-info buf) t)))))

(defun Buffer-menu-backup-unmark ()
  "Move up and cancel all requested operations on buffer on line above."
  (interactive)
  (forward-line -1)
  (Buffer-menu--unmark))

(defun Buffer-menu-execute ()
  "Save and/or delete buffers marked with \\<Buffer-menu-mode-map>\\[Buffer-menu-save] or \\<Buffer-menu-mode-map>\\[Buffer-menu-delete]."
  (interactive)
  (save-excursion
    (Buffer-menu-beginning)
    (while (not (eobp))
      (let ((buffer (tabulated-list-get-id))
            (entry  (tabulated-list-get-entry)))
        (cond ((null entry)
               (forward-line 1))
              ((not (buffer-live-p buffer))
               (tabulated-list-delete-entry))
              (t
               (let ((delete (eq (char-after) ?D)))
                 (when (and frame-bufs-mode (not delete))
                   (cond
                    ((equal (aref entry 3) "A")
                     (frame-bufs--add-buffer buffer (selected-frame))
                     (tabulated-list-set-col 3 (char-to-string frame-bufs-associated-buffer-bit) t))
                    ((equal (aref entry 3) "N")
                     (frame-bufs-dismiss-buffer buffer (selected-frame))
                     (if frame-bufs--global-list
                         (tabulated-list-set-col 3 " " t)
                       (tabulated-list-delete-entry)))))
                 (when (equal (aref entry 2) "S")
                   (condition-case nil
                       (progn
                         (with-current-buffer buffer
                           (save-buffer))
                         (tabulated-list-set-col 2 " " t))
                     (error (warn "Error saving %s" buffer))))
                 (if delete
                     (unless (eq buffer (current-buffer))
                       (kill-buffer buffer)
                       (tabulated-list-delete-entry))
                   (forward-line 1)))))))))

;;; ---------------------------------------------------------------------
;;;  Buffer Menu Functions
;;; ---------------------------------------------------------------------

(defun list-buffers--refresh (&optional buffer-list old-buffer)
  ;; Set up `tabulated-list-format'.
  (let ((name-width Buffer-menu-name-width)
        (size-width Buffer-menu-size-width))
    ;; Handle obsolete variable:
    (if Buffer-menu-buffer+size-width
        (setq name-width (- Buffer-menu-buffer+size-width size-width)))
    (setq tabulated-list-format
          (let* ((bits `(("C" 1 t :pad-right 0)
                         ("R" 1 t :pad-right 0)
                         ,(if frame-bufs-mode
                              '("M" 1 t :pad-right 0)
                            '("M" 1 t))))
                 (fb-bit (if frame-bufs-mode
                             (if frame-bufs--global-list
                                 '(("F"  1 t))
                               '(("-"  1 t)))))
                 (tail `(("Buffer" ,name-width t)
                         ("Size" ,size-width tabulated-list-entry-size->
                          :right-align t)
                         ("Mode" ,Buffer-menu-mode-width t)
                         ("File" 1 t))))
            (apply 'vector (append bits fb-bit tail)))))
  (setq tabulated-list-use-header-line Buffer-menu-use-header-line)
  ;; Collect info for each buffer we're interested in.
  (let ((buffer-menu-buffer (current-buffer))
        (show-non-file (not Buffer-menu-files-only))
        entries)
    (dolist (buffer (or buffer-list
                        (and frame-bufs-mode
                             (frame-bufs-buffer-list (selected-frame) frame-bufs--global-list))
                        (buffer-list (if Buffer-menu-use-frame-buffer-list
                                         (selected-frame)))))
      (with-current-buffer buffer
        (let* ((name (buffer-name))
               (file buffer-file-name))
          (when (and (buffer-live-p buffer)
                     (or buffer-list
                         (and (not (string= (substring name 0 1) " "))
                              (not (eq buffer buffer-menu-buffer))
                              (or file show-non-file))))
            (push (list buffer
                        (let* ((bits (list (if (eq buffer old-buffer) "." " ")
                                           (if buffer-read-only "%" " ")
                                           (if (buffer-modified-p) "*" " ")))
                               (fb-bit (if frame-bufs-mode
                                           (list (frame-bufs--bit-info buffer))))
                               (tail (list (Buffer-menu--pretty-name name)
                                           (number-to-string (buffer-size))
                                           (concat (format-mode-line mode-name nil nil buffer)
                                                   (if mode-line-process
                                                       (format-mode-line mode-line-process
                                                                         nil nil buffer)))
                                           (Buffer-menu--pretty-file-name file))))
                          (apply 'vector (append bits fb-bit tail))))
                  entries)))))
    (setq tabulated-list-entries (nreverse entries)))
  (tabulated-list-init-header))

(defun tabulated-list-entry-size-> (entry1 entry2)
  (> (string-to-number (aref (cadr entry1) frame-bufs--size-column))
     (string-to-number (aref (cadr entry2) frame-bufs--size-column))))

;;; ---------------------------------------------------------------------
;;;  Electric Buffer List Accomodation
;;; ---------------------------------------------------------------------

;; Make sure we don't interfere with electric-buffer-list.  Dynamic scoping
;; to the rescue.
(defadvice electric-buffer-list (around frame-bufs)
  (let ((frame-bufs-mode nil)
        (Buffer-menu-buffer-column 4)
        (frame-bufs--size-column 4))
    ad-do-it))

(provide 'frame-bufs)

;;; frame-bufs.el ends here
